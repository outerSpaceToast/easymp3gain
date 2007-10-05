
unit UnitMP3Gain;

{
     Copyright (C) 2007 by Thomas Dieffenbach
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
  Classes, SysUtils,ComCtrls,Forms, process;

type

  TSyncEventType = (setProgress, setStatusText, setStatusCode, setTrackGain,
    setAlbumGain, setMaxAmplitude_Track, setMaxAmplitude_Album);

  TMP3GainAction = (mgaTrackAnalyze, mgaAlbumAnalyze, mgaCheckTagInfo,
    mgaDeleteTagInfo, mgaAlbumGain, mgaTrackGain, mgaConstantGain,
    mgaUndoChanges);

  TSongItemInfo = record
    ListViewItem: TListItem;
    MaxAmplitude_Track: Double;
    MaxAmplitude_Album: Double;
    Gain_Track: Double;
    Gain_Album: Double;
    Volume_Track: Double;
    Volume_Album: Double;
    Clipping: Boolean;
    Clip_Track: Boolean;
    Clip_Album: Boolean;
    HasAlbumData: Boolean;
    HasData: Boolean;
  end;

  PSongItemInfo = ^TSongItemInfo;

  TMP3GainTask = record
    FileName: String[255]; //TFileName;  // Is a String being created with GetMem(PMP3GainTask)?
    MP3GainAction: TMP3GainAction;
    SongItem: PSongItemInfo;
    Volume: Real;
  end;

  PMP3GainTask = ^TMP3GainTask;

  TMP3GainTaskList = class(TList)
  private
    function GetItem(AIndex:integer): PMP3GainTask;
    procedure SetItem(AIndex:integer; AItem: PMP3GainTask);
  protected
  public
    function Add(Item: PMP3GainTask): Integer;
    procedure AddTask(ASongItem: PSongItemInfo; AMP3GainAction: TMP3GainAction; AVolume: Real);
    procedure DeleteTask(AIndex: Integer);
    property Items[AIndex:integer]: PMP3GainTask read GetItem write SetItem;default;
  end;

  
  TSynEvt = TThreadMethod;

  TMP3GainProcess = class(TThread)
  private
    FProcessOutput: String;
    FProcessCommand: String;
    FProgressEvent: TSynEvt;
    FResultEvent: TSynEvt;
    FFinishedEvent: TSynEvt;
    FStatusCodeEvent: TSynEvt;
    FExitStatus: Integer;
  protected
    procedure Execute; override;
  public
    Cancel: Boolean;
    Constructor Create(CreateSuspended : boolean);
    destructor Destroy;
  published
    property OnProgressEvent: TSynEvt read FProgressEvent write FProgressEvent;
    property OnResultEvent: TSynEvt read FResultEvent write FResultEvent;
    property OnFinished: TSynEvt read FFinishedEvent write FFinishedEvent;
    property OnStatusCodeEvent: TSynEvt read FStatusCodeEvent write FStatusCodeEvent;
    property ProcessOutput: String read FProcessOutput;
    property ProcessCommand: String read FProcessCommand write FProcessCommand;
    property ExitStatus: Integer read FExitStatus;
  end;

  { TMP3Gain }

  TMP3Gain = class
  private
    FMP3GainProcess:TMP3GainProcess;
    FReady: Boolean;
    FProgress: Byte;
    FStatusText: String;
    FExitCodeProcess: Integer;
    FFileName: String;
    FMP3GainAction: TMP3GainAction;
    FTargetVolume: Real;
    FVolumeGain: Real;
    FResult: Real;
    FBoolResult: Boolean;
    function GetIsReady: Boolean;
    procedure MP3GainSync(value: TSyncEventType);
    procedure ProcessProgress;
    procedure ProcessResult;
    procedure ProcessStatusCode;
    FOnRunFinished: TNotifyEvent;
    procedure RunFinished;
    procedure CreateProcess;
    procedure FreeProcess;
  public
    SongItem: PSongItemInfo;
    procedure Run;
    constructor Create;
    destructor Destroy;
  published
    property Progress: Byte read FProgress;
    property StatusText: String read FStatusText;
    property FileName: String read FFileName write FFileName;
    property MP3GainAction: TMP3GainAction read FMP3GainAction write FMP3GainAction;
    property TargetVolume: Real read FTargetVolume write FTargetVolume;
    property VolumeGain: Real read FVolumeGain write FVolumeGain;
    property Result: Real read FResult;
    property IsReady: Boolean read GetIsReady;
    property ExitCodeProcess: Integer read FExitCodeProcess write FExitCodeProcess;
    property OnRunFinished: TNotifyEvent read FOnRunFinished write FOnRunFinished;
end;

function RoundGainValue(Value: Real): Real;
  
const

{$IFDEF LINUX}
  MP3_GAIN_CMD = 'mp3gain';
{$ENDIF}
{$IFDEF WIN32}
  MP3_GAIN_CMD = 'mp3gain.exe';
{$ENDIF}
  
  SI_VOLUME = 0;
  SI_CLIPPING = 1;
  SI_TRACKGAIN = 2;
  SI_CLIPTRACK = 3;
  SI_ALBUMVOLUME = 4;
  SI_ALBUMGAIN = 5;
  SI_CLIPALBUM = 6;
  
  SI_COUNT = 7;
  
  REF_VOLUME = 89;
  
var
  TaskList: TMP3GainTaskList;
  strStatus_Analyzing: String = 'Analyzing...';
  strStatus_Gaining: String = 'Gaining...';
  strStatus_Finished: String = 'Finished.';
  strStatus_CheckingTagInfo: String = 'Checking Tag Info...';
  strStatus_DeletingTagInfo: String = 'Deleting Tag Info...';
  strStatus_UndoingChanges: String = 'Undoing Changes...';
  strStatus_ExitCode127: String = 'Could not start mp3gain. Is it installed?';

  boolStr: array[Boolean] of String = ('no','yes');

implementation

uses UnitMain;

function RoundGainValue(Value: Double): Double;
var
  t: Double;
begin
  t := Round(Value/1.5);
  Result := t*1.5;
end;

// ----------------------------------- TMP3GainTaskList -----------------------

function TMP3GainTaskList.Add(Item: PMP3GainTask): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TMP3GainTaskList.AddTask(ASongItem: PSongItemInfo; AMP3GainAction: TMP3GainAction; AVolume: Double);
var
  Item: PMP3GainTask;
begin
  GetMem(Item, SizeOf(TMP3GainTask));
  Item^.FileName := ASongItem^.ListViewItem.Caption;
  Item^.MP3GainAction := AMP3GainAction;
  Item^.SongItem := ASongItem;
  Item^.Volume := AVolume;
  inherited Add(Item);
end;

procedure TMP3GainTaskList.DeleteTask(AIndex: Integer);
begin
  FreeMem(Items[AIndex]);
  inherited Delete(AIndex);
end;

function TMP3GainTaskList.GetItem(AIndex:integer): PMP3GainTask;
begin
  Result := inherited Items[AIndex];
end;

procedure TMP3GainTaskList.SetItem(AIndex:integer;AItem:PMP3GainTask);
begin
  inherited Items[AIndex] := AItem;
end;

// ----------------------------------- TMP3GAIN -------------------------------

function TMP3Gain.GetIsReady: Boolean;
begin
  Result := FReady;
end;


procedure TMP3Gain.ProcessResult;
const
  strResult_TrackGain = 'dB gain'; //'"Track" dB change: ';
  strResult_AlbumGain = 'Album dB gain'; //'"Album" dB change: ';
  strResult_MaxAmplitudeTrack = 'Max Amplitude'; //'Max PCM sample at current gain: ';
var
  SL,header,data: TStringList;
  i,p,e: Integer;
  r: Double;
begin
  if FMP3GainProcess.ExitStatus=0 then
  begin
    FStatusText := strStatus_Finished;
    MP3GainSync(setStatusText);
    FProgress := 100;
    MP3GainSync(setProgress);

    SL := TStringList.Create;
    header := TStringList.Create;
    data := TStringList.Create;
    try
      SL.Text := FMP3GainProcess.ProcessOutput;
    {$IFDEF DEBUG_VERSION}
      SL.SaveToFile('/home/thomas/out.txt');
    {$ENDIF}
      for i:= SL.Count-1 downto 0 do
      begin
        if (SL[i]='') then SL.Delete(i);
      end;
      if SL.Count<2 then exit;
      header.Delimiter := chr(9);
      data.Delimiter := chr(9);
      header.DelimitedText := '"'+StringReplace(SL[0],#9,'"'#9'"',[rfReplaceAll]) + '"';
      data.DelimitedText := '"'+StringReplace(SL[1],#9,'"'#9'"',[rfReplaceAll]) + '"';

      p := header.IndexOf(strResult_TrackGain);
      if (p>-1) then
      begin
        Val(data[p],r,e);
        if not (e>0) then
        begin
          FResult := r;
          MP3GainSync(setTrackGain);
        end;
      end;

      p := header.IndexOf(strResult_AlbumGain);
      if (p>-1) then
      begin
        Val(data[p],r,e);
        if not (e>0) then
        begin
          FResult := r;
          MP3GainSync(setAlbumGain);
        end;
      end;

      p := header.IndexOf(strResult_MaxAmplitudeTrack);
      if (p>-1) then
      begin
        Val(data[p],r,e);
        if not (e>0) then
        begin
          FResult := r;
          FBoolResult := FResult > 32768;
          MP3GainSync(setMaxAmplitude_Track);
        end;
      end;

      (*if SL.Count=0 then exit;
      for i:= SL.Count-1 downto 0 do
      begin

        p := Pos(strResult_TrackGain, SL[i]);
        if (p<>0) then
        begin
          a := Length(strResult_TrackGain);
          FResult := StrToFloat(Copy(SL[i],p+a,Length(SL[i])-(p+a)));
          MP3GainSync(setTrackGain);
        end;

        p := Pos(strResult_AlbumGain, SL[i]);
        if (p<>0) then
        begin
          a := Length(strResult_AlbumGain);
          FResult := StrToFloat(Copy(SL[i],p+a,Length(SL[i])-(p+a)));
          MP3GainSync(setAlbumGain);
        end;

        p := Pos(strResultMaxPCMsample ,SL[i]);
        if (p<>0) then
        begin
          a := Length(strResultMaxPCMsample);
          FResult := StrToFloat(Copy(SL[i],p+a,Length(SL[i])-(p+a)));
          FBoolResult := FResult > 32768;
          MP3GainSync(setClipping);
        end;

      end; *)
    finally
      SL.Free;
      header.free;
      data.free;
    end;
  end
  else  // Exit Code <> 0
  begin
    if FMP3GainProcess.ExitStatus=127 then
      FStatusText := strStatus_ExitCode127
    else
      FStatusText := 'Error running mp3gain: ' + IntToStr(FMP3GainProcess.ExitStatus);
    MP3GainSync(setStatusText);
    FProgress := 100;
    MP3GainSync(setProgress);
  end;
end;


procedure TMP3Gain.ProcessProgress;
var
  SL: TStringList;
  i: Integer;
  b: SmallInt;
begin
  SL := TStringList.Create;
  try
    SL.Text := FMP3GainProcess.ProcessOutput;
    for i:= SL.Count-1 downto 0 do
    begin
      if (SL[i]='') then SL.Delete(i);
    end;
    if SL.Count=0 then exit;
    for i:= SL.Count-1 downto 0 do
    begin
      if Pos('bytes analyzed',SL[i])<>0 then
      begin
        b := StrToInt(Copy(SL[i],2,2));
        if b>FProgress then FProgress := b;
      end;
    end;
  finally
    SL.Free;
    MP3GainSync(setProgress);
  end;
end;

procedure TMP3Gain.ProcessStatusCode;
begin
  FExitCodeProcess := FMP3GainProcess.ExitStatus;
  MP3GainSync(setStatusCode);
end;

procedure TMP3Gain.RunFinished;
begin
  FReady := true;
  OnRunFinished(Self);
end;

procedure TMP3Gain.MP3GainSync(value: TSyncEventType);
begin
  if SongItem= nil then
  begin
    case value of
      setProgress:
        frmMP3GainGUIMain.ProgressBar.Position := FProgress;
      setStatusText:
        frmMP3GainGUIMain.StatusBar.Panels[0].Text := FStatusText;
      setTrackGain:
        frmMP3GainGUIMain.Memo1.Lines.Add('Track Gain: ' + FloatToStr(FResult));
      setAlbumGain:
        frmMP3GainGUIMain.Memo1.Lines.Add('Album Gain: ' + FloatToStr(FResult));
    end;
  end else
  begin
    case value of
      setProgress:
        frmMP3GainGUIMain.ProgressBar.Position := FProgress;
      setStatusText:
        frmMP3GainGUIMain.StatusBar.Panels[0].Text := FStatusText;
      setStatusCode:
        ;//frmMP3GainGUIMain.StatusBar.Panels[1].Text := IntToStr(FExitCodeProcess);
      setTrackGain:
      begin
        //SongItem^.ListViewItem.SubItems[SI_TRACKGAIN] := Format('%.2f',[RoundGainValue(FResult+FTargetVolume-REF_VOLUME)]);
        //SongItem^.ListViewItem.SubItems[SI_VOLUME] := Format('%.1f',[REF_VOLUME-FResult]);
        SongItem^.HasData := true; // TagInfo existing
        SongItem^.Gain_Track := FResult+FTargetVolume-REF_VOLUME;
        SongItem^.Volume_Track := REF_VOLUME-FResult;
      end;
      setAlbumGain:
      begin
        //SongItem^.ListViewItem.SubItems[SI_ALBUMGAIN] := Format('%3.2f',[FResult+FTargetVolume-REF_VOLUME]);
        //SongItem^.ListViewItem.SubItems[SI_ALBUMVOLUME] := Format('%3.1f',[REF_VOLUME-FResult]);
        SongItem^.HasAlbumData := true;
        SongItem^.Gain_Album := FResult+FTargetVolume-REF_VOLUME;
        SongItem^.Volume_Album := REF_VOLUME-FResult;
      end;
      setMaxAmplitude_Track:
      begin
        //SongItem^.ListViewItem.SubItems[SI_CLIPPING] := boolStr[FBoolResult];
        //SongItem^.MaxAmplitude_Track := Trunc(FResult);      //Fehler?????
        SongItem^.Clipping := FBoolResult;
        SongItem^.MaxAmplitude_Track := FResult;
      end;
      setMaxAmplitude_Album:
      begin
        //SongItem^.ListViewItem.SubItems[SI_CLIPPING] := boolStr[FBoolResult];
        //SongItem^.MaxAmplitude_Album := FResult;
        SongItem^.MaxAmplitude_Album := FResult;
        
      end;
    end;
  end;
end;

procedure TMP3Gain.CreateProcess;
begin
  FMP3GainProcess := TMP3GainProcess.Create(true);
  FMP3GainProcess.FProgressEvent := @ProcessProgress;
  FMP3GainProcess.FResultEvent := @ProcessResult;
  FMP3GainProcess.FFinishedEvent := @RunFinished;
  FMP3GainProcess.FStatusCodeEvent := @ProcessStatusCode;
end;

procedure TMP3Gain.FreeProcess;
begin
  FMP3GainProcess.Free;
end;

procedure TMP3Gain.Run;
var
  cmd: String;
begin
  FReady := false;
  FProgress := 0;
  MP3GainSync(setProgress);
  CreateProcess;
  cmd := MP3_GAIN_CMD + ' ';
  case FMP3GainAction of
    mgaTrackAnalyze:
    begin
      FStatusText := strStatus_Analyzing;
    end;
    mgaAlbumAnalyze:
    begin
      FStatusText := strStatus_Analyzing;
    end;
    mgaCheckTagInfo:
    begin
      cmd := cmd + '-s c ';
      FStatusText := strStatus_CheckingTagInfo;
    end;
    mgaDeleteTagInfo:
    begin
      cmd := cmd + '-s d ';
      FStatusText := strStatus_DeletingTagInfo;
    end;
    mgaAlbumGain:
    begin
      cmd := cmd + '-a -d ' + Format('%3.1f',[FTargetVolume-REF_VOLUME]) + ' -c ';
      FStatusText := strStatus_Gaining;
    end;
    mgaTrackGain:
    begin
      cmd := cmd + '-r -d ' + Format('%3.1f',[FTargetVolume-REF_VOLUME]) + ' -c ';     // -c = ignore clipping
      FStatusText := strStatus_Gaining
    end;
    mgaConstantGain:
    begin
      cmd := cmd + '-g ' + Format('%3.1f',[FVolumeGain/1.5]) + ' -c ';     // -c = ignore clipping
      FStatusText := strStatus_Gaining
    end;
    mgaUndoChanges:
    begin
      cmd := cmd + '-u ';
      FStatusText := strStatus_UndoingChanges
    end;
  end;
  MP3GainSync(setStatusText);
  
  FMP3GainProcess.ProcessCommand := cmd + ' -o "' + FFileName + '"';    // -o Tab delimited output
  FMP3GainProcess.Resume;
end;

constructor TMP3Gain.Create;
begin
  inherited Create;
  FReady := true;
  SongItem := nil;
end;

destructor TMP3Gain.Destroy;
begin
  FMP3GainProcess.Free;
  inherited Destroy;
end;

// ----------------------------------- TMP3GainProcess ------------------------

constructor TMP3GainProcess.Create(CreateSuspended : boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

destructor TMP3GainProcess.Destroy;
begin
  inherited Destroy;
end;


procedure TMP3GainProcess.Execute;
const
  READ_BYTES = 2048;
var
  P: TProcess;
  n: LongInt;
{$IFDEF DEBUG_VERSION}
  X: TStringList;
{$ENDIF}
  pc: PChar;
  a:array[0..2047] of char;
  str_echo: String;
  e: Integer;
begin
  str_echo := '';
  pc := @(A);
  P := TProcess.Create(nil);
  try
    P.CommandLine := FProcessCommand;
    P.Options := [poUsePipes];
    P.Execute;
    while P.Running do
    begin
      n := P.Stderr.Read(pc[0], READ_BYTES);
      if n>0 then
      begin
        FProcessOutput := pc;
        Synchronize(OnProgressEvent);
      end;
      Sleep(100);
    end;
    repeat
      n := P.Output.Read(pc[0],READ_BYTES);
      pc[n] := Char(0);
    if n>0 then
    begin
      str_echo := str_echo + pc;
    end;
    until n <= 0;
  finally
    e := P.ExitStatus;
    P.Free;
  end;
{$IFDEF DEBUG_VERSION}
  X := TStringList.Create;
  try
    X.Text := str_echo;
    X.SaveToFile('/home/thomas/op.txt');
  finally
    X.Free;
  end;
{$ENDIF}
  FExitStatus := e;
  Synchronize(OnStatusCodeEvent);
  FProcessOutput := str_echo;
  Synchronize(OnResultEvent);
  Self.FreeOnTerminate := true;
  Synchronize(OnFinished);
  //OnFinished();
  Self.Terminate;
end;



end.

