unit UnitVorbisGain;

{
     Copyright (C) 2007-2008 by Thomas Dieffenbach
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
  
const
{$IFDEF LINUX}
  VORBIS_GAIN_CMD = 'vorbisgain';
  VORBIS_INFO_CMD = 'ogginfo';
{$ENDIF}
{$IFDEF WIN32}
  VORBIS_GAIN_CMD = 'VorbisGain.exe';
{$ENDIF}
  
procedure CreateCommand(MediaGain: TMediaGain; var cmd: String);
procedure ProcessProgress(MediaGain: TMediaGain; strData: String; var FCurrentSongItem: Integer;
                          var FProgress: Byte);
procedure ProcessResult(MediaGain: TMediaGain; strData: String; FHeaderList, FDataList: TStringList;
                        var SongItem: TSongItem);
procedure ReadVorbisComments(MediaGain: TMediaGain);

implementation

function ExtractProgressValue(S: String; var CurrentSongItem: Integer): SmallInt;
var
  a, b: Integer;
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

    (*if (a>0) and (b>0) then
    begin
      CurrentSongItem := StrToInt(Trim(Copy(S,a+1,b-a-1))) -1;  // starts with 0 not 1
    end;*)
  end;
end;

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
      b := ExtractProgressValue(SL[i], NewSongItem);
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

procedure CreateCommand(MediaGain: TMediaGain; var cmd: String);
var
  i: Integer;
begin
  cmd := VORBIS_GAIN_CMD + ' ';
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
        cmd := VORBIS_INFO_CMD + ' '; //cmd + '-d ';    // display
        StatusText := strStatus_CheckingTagInfo;
      end;
      mgaDeleteTagInfo:
      begin
        cmd := cmd + '-c ';    // clean
        StatusText := strStatus_DeletingTagInfo;
      end;
      mgaAlbumGain:
      begin
        for i:=0 to SongItems.Count-1 do
        begin
          SongItems[i].Volume_Difference:=RoundGainValue(TargetVolume-SongItems[i].Volume_Album);
        end;
        cmd := cmd + '-a '; //'-g ' + Format('%3.1f',[TargetVolume-REF_VOLUME]);
        StatusText := strStatus_Gaining;
      end;
      mgaTrackGain:
      begin
        StatusText := strStatus_Gaining
      end;
      mgaConstantGain:
      begin
        //cmd := cmd + '-g ' + Format('%3.1f',[SongItems[0]. VolumeGain]);   // -c = ignore clipping
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

function ExtractNumber(S: String): Real;
var
  i,k,a,b: Integer;
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

procedure TrimList(AList: TStringList);
var
  i: Integer;
begin
  for i:=AList.Count -1 downto 0 do
    AList[i] := Trim(AList[i]);
end;

procedure ProcessResult(MediaGain: TMediaGain; strData: String; FHeaderList, FDataList: TStringList;
                        var SongItem: TSongItem);
begin
  // not used
end;

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
        MediaGain.MediaGainSync(setWholeAlbumGain);
      end;
    end;
  finally
    VorbisComment.Free;
    MediaGain.MediaGainSync(setSongItemHasFinished);
  end;
end;

end.

