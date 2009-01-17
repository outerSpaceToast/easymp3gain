unit UnitVorbisGain;

{
     Copyright (C) 2007-2009 by Thomas Dieffenbach
     giantics@gmx.de

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2 of the License, or
     (at your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the
     Free Software Foundation, Inc.,
     59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UnitMediaGain, UnitMain, VorbisComment;
  

procedure CreateCommand(MediaGain: TMediaGain; var cmd: String);
procedure ProcessProgress(MediaGain: TMediaGain; strData: String; var FCurrentSongItem: Integer;
                          var FProgress: Byte);
procedure ProcessResult(MediaGain: TMediaGain; strData: String; FHeaderList, FDataList: TStringList;
                        var SongItem: TSongItem);
procedure ReadVorbisComments(MediaGain: TMediaGain);

implementation


// ------------------------------------------------------------------------------------------------
// ExtractProgressValue: Extracts the progress-value (0%-100%) out of the strings of the backend
// ------------------------------------------------------------------------------------------------
function ExtractProgressValue(MediaGain: TMediaGain; S: String; var CurrentSongItem: Integer): SmallInt;
var
  a, i: Integer;
begin
  if Pos('%',S)<>0 then
  begin
    a := Pos('%',S);
    if a<1 then
    begin
      Result := -1;
      exit;
    end;
    Result := StrToInt(Trim(Copy(S,a-2,2)));
    for i:= 0 to MediaGain.SongItems.Count-1 do
    begin
      if MediaGain.SongItems[i].FileName = Trim(Copy(S,a+3,Length(S))) then
        CurrentSongItem := i;
    end;
  end;
end;

// ---------------------------------------------------------------------------------------
// ProcessProgress: Handles the progress-value (0%-100%), current file, etc. of the backend
// ---------------------------------------------------------------------------------------
procedure ProcessProgress(MediaGain: TMediaGain; strData: String; var FCurrentSongItem: Integer;
                          var FProgress: Byte);
var
  SL: TStringList;
  i, NewSongItem: Integer;
  b: SmallInt;
begin
  NewSongItem := 0;
  SL := TStringList.Create;
  try
    SL.Text := strData;
    for i:= SL.Count-1 downto 0 do
    begin
      if (SL[i]='') then SL.Delete(i);
    end;
    if SL.Count=0 then exit;
    {$IFDEF DEBUG_VERSION}
      SL.SaveToFile(strHomeDir+'prog.txt');
    {$ENDIF}
    for i:= SL.Count-1 downto 0 do
    begin
      b := ExtractProgressValue(MediaGain, SL[i], NewSongItem);
      if NewSongItem >FCurrentSongItem then
      begin
        FCurrentSongItem := NewSongItem;
        FProgress := 0;
        MediaGain.MediaGainSync(setSongItemHasFinished);
        MediaGain.MediaGainSync(setSongItemHasStarted);
      end;
      if b>FProgress then
      begin
        FProgress := Byte(b);
      end;
    end;
  finally
    SL.Free;
    MediaGain.MediaGainSync(setProgress);
  end;
end;

// ------------------------------------------------------------------------------------------------
// CreateCommand: Creates the correct command for calling the backend
// ------------------------------------------------------------------------------------------------
procedure CreateCommand(MediaGain: TMediaGain; var cmd: String);
var
  i: Integer;
begin
  cmd := MediaGainOptions.strVorbisGainBackend + ' ';
  with MediaGain do
  begin
    for i:=0 to SongItems.Count-1 do
    begin
      SongItems[i].Volume_Old := SongItems[i].Volume_Track;
    end;
    case MediaGainAction of
      mgaTrackAnalyze:
      begin
        StatusText := strStatus_Analyzing;
      end;
      mgaAlbumAnalyze:
      begin
        cmd := cmd + '-a ';
        StatusText := strStatus_Analyzing;
      end;
      mgaCheckTagInfo:
      begin
        cmd := '';
        StatusText := strStatus_CheckingTagInfo;
      end;
      mgaDeleteTagInfo:
      begin
        cmd := cmd + '-c ';
        StatusText := strStatus_DeletingTagInfo;
      end;
      mgaAlbumGain:
      begin
        for i:=0 to SongItems.Count-1 do
        begin
          SongItems[i].Volume_Difference:=RoundGainValue(TargetVolume-SongItems[i].Volume_Album);
        end;
        cmd := cmd + '-a -f';
        StatusText := strStatus_Gaining;
      end;
      mgaTrackGain:
      begin
        cmd := cmd + '-f ';
        StatusText := strStatus_Gaining
      end;
      mgaConstantGain:
      begin
        cmd := '';
        StatusText := strStatus_Gaining //??
      end;
      mgaUndoChanges:
      begin
        cmd := cmd + '-c ';
        StatusText := strStatus_UndoingChanges
      end;
    end;
    cmd := cmd + ' -s';                                                   // silent
  end; // with FMediaGain
end;


// ------------------------------------------------------------------------------------------------
// ExtractNumber: Extracts the real-value out of a string
// ------------------------------------------------------------------------------------------------
function ExtractNumber(S: String): Real;
var
  i,a,b: Integer;
  strResult: String;
  NumChars : set of #1..#254;
begin
  a := 0;
  b := 0;
  S := S + ' ';     // add one character for algorithm
  NumChars := ['0'..'9','-','.'];
  for i:=1 to Length(S) do
  begin
    if (S[i] in NumChars) then
    begin
      a := i;
      b := a;
      repeat inc(b) until not (S[b] in NumChars);
      break;
    end;
  end;
  if (a>0) and (b>0) then
  begin
    strResult := Copy(S,a,b-a);
    (*i := Pos('.',strResult);                            // Lazarus only sees '.' as decimalsep.
    if (i>0) then strResult[i] := decimalseparator; *)
    Result := StrToFloat(strResult);
  end
  else
   Result := 0;
end;

// ---------------------------------------------------
// TrimList: Trims every item of a TStringList
// ---------------------------------------------------
procedure TrimList(AList: TStringList);
var
  i: Integer;
begin
  for i:=AList.Count -1 downto 0 do
    AList[i] := Trim(AList[i]);
end;

// ------------------------------------------------------------------------------------------------
// ProcessResult: Used to parse the output of the backend, not needed anymore (see ReadVorbisComments)
// ------------------------------------------------------------------------------------------------
procedure ProcessResult(MediaGain: TMediaGain; strData: String; FHeaderList, FDataList: TStringList;
                        var SongItem: TSongItem);
begin
  // not used
end;

// ------------------------------------------------------------------------------------------------
// ReadVorbisComments: Reads the VorbisGain-Tags out of a Vorbis-file
// ------------------------------------------------------------------------------------------------
procedure ReadVorbisComments(MediaGain: TMediaGain);
const
  strTrackPeak = 'REPLAYGAIN_TRACK_PEAK';
  strTrackGain = 'REPLAYGAIN_TRACK_GAIN';
  strAlbumPeak = 'REPLAYGAIN_ALBUM_PEAK';
  strAlbumGain = 'REPLAYGAIN_ALBUM_GAIN';
var
  VorbisComment: TVorbisComment;
  Comments: TComments;
  bSuccess: Boolean;
  i: Integer;
begin
  VorbisComment := TVorbisComment.Create;
  try
    MediaGain.MediaGainSync(setSongItemHasStarted);
    Writeln('Reading Vorbis-Tags from: ' + MediaGain.SongItems[0].FileName);
    Comments := VorbisComment.ReadComments(MediaGain.SongItems[0].FileName, bSuccess);
    if not bSuccess then exit;
    for i:=0 to Length(Comments)-1do
    begin
      if (Comments[i].Name = strTrackGain) then
      begin
        MediaGain.Result:= ExtractNumber(Comments[i].Value);
        MediaGain.MediaGainSync(setTrackGain);
      end;
      if (Comments[i].Name = strAlbumGain) then
      begin
        MediaGain.Result:= ExtractNumber(Comments[i].Value);
        MediaGain.MediaGainSync(setAlbumGain);
      end;
    end;
  finally
    VorbisComment.Free;
    MediaGain.MediaGainSync(setSongItemHasFinished);
  end;
end;

end.

