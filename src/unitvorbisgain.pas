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
  Classes, SysUtils, UnitMediaGain;
  
const
{$IFDEF LINUX}
  VORBIS_GAIN_CMD = 'vorbisgain';
  VORBIS_INFO_CMD = 'ogginfo';
{$ENDIF}
{$IFDEF WIN32}
  VORBIS_GAIN_CMD = 'VorbisGain.exe';
{$ENDIF}
  
procedure CreateCommand(MediaGain: TMediaGain; var cmd: String);

implementation

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
        cmd := cmd + '-d ';    // display
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
    (*if MediaGainOptions.UseTempFiles then
      cmd := cmd + '-t '
    else
      cmd := cmd + '-T ';*)
    //if MediaGainOptions.PreserveOriginalTimestamp then cmd := cmd + '-p ';
    //if MediaGainOptions.IgnoreTags then cmd := cmd + '-s s ';
    cmd := cmd + ' -f';                                                   // fast
  end; // with FMediaGain
end;

end.

