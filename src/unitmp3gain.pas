unit UnitMP3Gain;

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

{_$DEFINE DEBUG_VERSION}

interface

uses
  Classes, SysUtils, UnitMediaGain;
  
procedure CreateCommand(MediaGain: TMediaGain; var cmd: String);
procedure ProcessResult(MediaGain: TMediaGain; strData: String; FHeaderList, FDataList: TStringList;
                        var SongItem: TSongItem);
procedure ProcessProgress(MediaGain: TMediaGain; strData: String; var FCurrentSongItem: Integer;
                          var FProgress: Byte);

implementation

function ExtractProgressValue(S: String; var CurrentSongItem: Integer): SmallInt;
var
  a, b: Integer;
begin
  if Pos('bytes analyzed',S)<>0 then
  begin
    a := Pos('%',S);
    if a<1 then
    begin
      Result := -1;
      exit;
    end;
    Result := StrToInt(Trim(Copy(S,a-2,2)));
    a := Pos('[', S);
    b := Pos('/', S);
    if (a>0) and (b>0) then
    begin
      CurrentSongItem := StrToInt(Trim(Copy(S,a+1,b-a-1))) -1;  // starts with 0 not 1
    end;
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

procedure ProcessResult(MediaGain: TMediaGain; strData: String; FHeaderList, FDataList: TStringList;
                        var SongItem: TSongItem);

  function GetSongItem(const AFileName: String): TSongItem;
  var
    i: Integer;
  begin
    Result := nil;
    for i:=MediaGain.SongItems.Count-1 downto 0 do
    begin
      if (MediaGain.SongItems[i].FileName = AFileName) then
        Result := MediaGain.SongItems[i];
    end;
  end;

const
  strResult_Album = '"Album"';
  strResult_Album_Unquoted = 'Album';
  strResult_File = 'File';
  strResult_TrackGain = 'dB gain'; //'"Track" dB change: ';
  strResult_AlbumGain = 'Album dB gain'; //'"Album" dB change: ';
  strResult_MaxAmplitudeTrack = 'Max Amplitude'; //'Max PCM sample at current gain: ';
var
  SL: TStringList;
  i,p,e: Integer;
  r: Double;
  s: String;
  Album_Result_Event: Boolean;
begin
  with MediaGain do
  begin
    Album_Result_Event := false;
    SL := TStringList.Create;
    try
      SL.Text := strData;
    {$IFDEF DEBUG_VERSION}
      SL.SaveToFile(strHomeDir+'out'+InttoStr(QWord(now)) +'.txt');
    {$ENDIF}
      for i:= SL.Count-1 downto 0 do
      begin
        if (SL[i]='') then SL.Delete(i);
      end;
      if FHeaderList.Count<1 then
      begin
        FHeaderList.DelimitedText := '"'+StringReplace(SL[0],#9,'"'#9'"',[rfReplaceAll]) + '"';
        SL.Delete(0); // Delete Header-Item in Stringlist
      end;
      if SL.Count<1 then exit;
      for i:=0 to SL.Count-1 do
      begin
        S := StringReplace(SL[i], strResult_Album, strResult_Album_Unquoted, [rfReplaceAll]);
        FDataList.DelimitedText := '"'+StringReplace(S,#9,'"'#9'"',[rfReplaceAll]) + '"';

        p := FHeaderList.IndexOf(strResult_File);
        if (p>-1) then
        begin
            {$IFDEF DEBUG_VERSION}
              FDataList.SaveToFile(strHomeDir+'out_data.txt');
            {$ENDIF}
          if (FDataList[p] = strResult_Album_Unquoted) then
          begin
            Album_Result_Event := true;
          end else
          begin
            Album_Result_Event := false;
            SongItem := GetSongItem(FDataList[p]);
            Writeln('SongItem: ', LongInt(SongItem));
            if SongItem=nil then continue;
          end;
        end;

        try
          p := FHeaderList.IndexOf(strResult_TrackGain);
          if (p>-1) then
          begin
            Val(FDataList[p],r,e);
            if not (e>0) then
            begin
              Result := r;
              if Album_Result_Event then
                MediaGainSync(setWholeAlbumGain)
              else
                MediaGainSync(setTrackGain);
            end;
          end;

          p := FHeaderList.IndexOf(strResult_AlbumGain);
          if (p>-1) then
          begin
            Val(FDataList[p],r,e);
            if not (e>0) then
            begin
              Result := r;
              //if Album_Result_Event then
              //  MediaGainSync(setWholeAlbumGain);
              //else
              if not Album_Result_Event then
                MediaGainSync(setAlbumGain)
            end;
          end;

          p := FHeaderList.IndexOf(strResult_MaxAmplitudeTrack);
          if (p>-1) then
          begin
            Val(FDataList[p],r,e);
            if not (e>0) then
            begin
              Result := r;
              if not Album_Result_Event then
                MediaGainSync(setMaxAmplitude_Track);
            end;
          end;
        except
          //on E:Error do ;// An error occured
        end;
      end;
    finally
      SL.Free;
      FDataList.Clear;
    end;
  end
end;

procedure CreateCommand(MediaGain: TMediaGain; var cmd: String);
var
  i: Integer;
begin
  with MediaGain do
  begin
    if SongItems.Count < 1 then exit;
    if SongItems[0].MediaType=mtMP3 then
      cmd := MediaGainOptions.strMP3GainBackend + ' '
    else if SongItems[0].MediaType=mtAAC then
      cmd := MediaGainOptions.strAACGainBackend + ' ';
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
        StatusText := strStatus_Analyzing;
      end;
      mgaCheckTagInfo:
      begin
        cmd := cmd + '-s c ';
        StatusText := strStatus_CheckingTagInfo;
      end;
      mgaDeleteTagInfo:
      begin
        cmd := cmd + '-s d ';
        StatusText := strStatus_DeletingTagInfo;
      end;
      mgaAlbumGain:
      begin
        for i:=0 to SongItems.Count-1 do
        begin
          SongItems[i].Volume_Difference:=RoundGainValue(TargetVolume-SongItems[i].Volume_Album);
        end;
        cmd := cmd + '-a -d ' + Format('%3.1f',[TargetVolume-REF_VOLUME]) + ' -c ';
        StatusText := strStatus_Gaining;
      end;
      mgaTrackGain:
      begin
        for i:=0 to SongItems.Count-1 do
        begin
          SongItems[i].Volume_Difference:=RoundGainValue(TargetVolume-SongItems[i].Volume_Track);
        end;
        cmd := cmd + '-r -d ' + Format('%3.1f',[TargetVolume-REF_VOLUME]) + ' -c ';     // -c = ignore clipping
        StatusText := strStatus_Gaining
      end;
      mgaConstantGain:
      begin
        cmd := cmd + '-g ' + Format('%3.1f',[VolumeGain/1.5]) + ' -c ';   // -c = ignore clipping
        StatusText := strStatus_Gaining
      end;
      mgaUndoChanges:
      begin
        cmd := cmd + '-u ';
        StatusText := strStatus_UndoingChanges
      end;
    end;
    if MediaGainOptions.UseTempFiles then
      cmd := cmd + '-t '
    else
      cmd := cmd + '-T ';
    if MediaGainOptions.PreserveOriginalTimestamp then cmd := cmd + '-p ';
    if MediaGainOptions.IgnoreTags then cmd := cmd + '-s s ';
    cmd := cmd + ' -o';                                                   // -o Tab delimited output
  end; // with FMediaGain
end;

end.

