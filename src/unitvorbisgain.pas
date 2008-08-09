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
  Classes, SysUtils, UnitMediaGain, UnitMain;
  
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
        cmd := cmd + '-a -g ' + Format('%3.1f',[TargetVolume-REF_VOLUME]);
        StatusText := strStatus_Gaining;
      end;
      mgaTrackGain:
      begin
        for i:=0 to SongItems.Count-1 do
        begin
          SongItems[i].Volume_Difference:=RoundGainValue(TargetVolume-SongItems[i].Volume_Track);
        end;
        cmd := cmd + '-g ' + Format('%3.1f',[TargetVolume-REF_VOLUME]);
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
    (*if MediaGainOptions.UseTempFiles then
      cmd := cmd + '-t '
    else
      cmd := cmd + '-T ';*)
    //if MediaGainOptions.PreserveOriginalTimestamp then cmd := cmd + '-p ';
    //if MediaGainOptions.IgnoreTags then cmd := cmd + '-s s ';
    cmd := cmd + ' -f -s';                                                   // fast & silent
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
  strResult_Gain = 'Gain';
  strResult_Peak = 'Peak';
  strResult_Scale = 'Scale';
  strResult_NewPeak = 'New Peak';
  strResult_File = 'Track';
  strResult_AlbumGain = 'Recommended Album Gain:';
//   Gain   |  Peak  | Scale | New Peak | Track
//----------+--------+-------+----------+------
// -8.59 dB |  41100 |  0.37 |    15288 | Nightwish - 02 - Bye Bye Beautiful.ogg
//-10.32 dB |  42759 |  0.30 |    13033 | Nightwish - 03 - Amaranth.ogg
//
//Recommended Album Gain: -9.92 dB
//Writing tags to 'Nightwish - 02 - Bye Bye Beautiful.ogg'
//Writing tags to 'Nightwish - 03 - Amaranth.ogg'


var
  SL: TStringList;
  i,p,e: Integer;
  r: Double;
  s: String;
  Album_Result_Event: Boolean;
begin
  FHeaderList.Delimiter := '|';
  FDataList.Delimiter := '|';
  with MediaGain do
  begin
    Album_Result_Event := false;
    SL := TStringList.Create;
    try
      SL.Text := strData;
    {_$IFDEF DEBUG_VERSION}
      SL.SaveToFile(strHomeDir+'out'+InttoStr(QWord(now)) +'.txt');
    {_$ENDIF}
      for i:= SL.Count-1 downto 0 do
      begin
        if (SL[i]='') then SL.Delete(i);
      end;
      while (FHeaderList.Count<1) and (SL.Count>0) do
      begin
        S:=SL[0];
        S:=IntToStr(Pos(S,strResult_Gain));
        Writeln(S);
        if (Pos(strResult_Gain,SL[0])>0) and
           (Pos(strResult_Peak,SL[0])>0) and
           (Pos(strResult_File,SL[0])>0) then
        begin
          FHeaderList.DelimitedText := SL[0];
          TrimList(FHeaderList);
          SL.Delete(0); // Delete Header-Item in Stringlist
          break;
        end;
        SL.Delete(0);   // Delete Pre-Header-Item in Stringlist
      end;
      if SL.Count<1 then exit;
      
      for i:=0 to SL.Count-1 do
      begin
        S := SL[i];
        FDataList.DelimitedText := S;
        if FDataList.Count < FHeaderList.Count then
        begin
          if Pos(S,strResult_AlbumGain)>0 then
          begin
            Result := ExtractNumber(S);
            if not Album_Result_Event then
              MediaGainSync(setAlbumGain)
          end;
          continue; // no Track-Line
        end;
        TrimList(FDataList);

        p := FHeaderList.IndexOf(strResult_File);
        if (p>-1) then
        begin
            {$IFDEF DEBUG_VERSION}
              FDataList.SaveToFile(strHomeDir+'out_data.txt');
            {$ENDIF}
         (* if (FDataList[p] = strResult_AlbumGain) then
          begin
            Album_Result_Event := true;
          end else
          begin
            Album_Result_Event := false;
            SongItem := GetSongItem(FDataList[p]);
            Writeln('SongItem: ', LongInt(SongItem));
            if SongItem=nil then continue;
          end; *)
          SongItem := GetSongItem(FDataList[p]);
        end;

        try
          p := FHeaderList.IndexOf(strResult_Gain);
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

          p := FHeaderList.IndexOf(strResult_Peak);
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

end.

