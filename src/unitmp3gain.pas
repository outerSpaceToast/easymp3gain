
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

{$DEFINE DEBUG_VERSION}

interface

uses
  Classes, SysUtils,ComCtrls,Forms, process;

type

  TSyncEventType = (setProgress, setStatusText, setStatusCode, setTrackGain,
    setAlbumGain, setMaxAmplitude_Track, setMaxAmplitude_Album);

  TMP3GainAction = (mgaTrackAnalyze, mgaAlbumAnalyze, mgaCheckTagInfo,
    mgaDeleteTagInfo, mgaAlbumGain, mgaTrackGain, mgaConstantGain,
    mgaUndoChanges);
    
  TMP3GainTask = class;

  TSongItem = class
  public
    FileName: String;
    ExtractedFileName: String;
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
  
  TMP3GainTaskList = class(TList)
  private
    function GetItem(AIndex:integer): TMP3GainTask;
    procedure SetItem(AIndex:integer; AItem: TMP3GainTask);
  protected
  public
    function Add(Item: TMP3GainTask): Integer;
    function AddTask(ASongItem: TSongItem; AMP3GainAction: TMP3GainAction; AVolume: Double): Integer; overload;
    function AddTask(ASongItem: Pointer; AMP3GainAction: TMP3GainAction; AVolume: Double): Integer; overload;
    procedure DeleteTask(AIndex: Integer);
    property Items[AIndex:integer]: TMP3GainTask read GetItem write SetItem;default;
  end;

  TSongItemList = class(TList)
  private
    function GetItem(AIndex:integer): TSongItem;
    procedure SetItem(AIndex:integer; AItem: TSongItem);
  protected
  public
    function Add(Item: TSongItem): Integer; overload;
    function Add(Item: Pointer): Integer; overload;
    property Items[AIndex:integer]: TSongItem read GetItem write SetItem;default;
  end;

  TMP3GainTask = class
  private
    FSongItemList: TSongItemList;
  public
    //FileName: String;
    MP3GainAction: TMP3GainAction;
    //SongItem: TSongItem;
    Volume: Real;
    constructor Create;
    destructor Destroy;
  published
    property SongItems: TSongItemList read FSongItemList;
  end;
  
a// Song Item rausnehmen, da SongItemListe!
 // CurrentSongItem beim Synchronisieren benutzen!
  
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
    FCurrentSongItem: Integer;
    function ReadProcessOutput(P: TProcess; Buffer: PChar): LongInt;
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
    //FFileName: String;
    FMP3GainAction: TMP3GainAction;
    FTargetVolume: Real;
    FVolumeGain: Real;
    FResult: Real;
    FBoolResult: Boolean;
    FSongItemList: TSongItemList;
    FErrorHasOccured: Boolean;
    function ExtractProgressValue(S: String; var CurrentSongItem: Integer): SmallInt;
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
    SongItem: TSongItem;
    procedure Run;
    constructor Create;
    destructor Destroy;
  published
    property Progress: Byte read FProgress;
    property StatusText: String read FStatusText;
    //property FileName: String read FFileName write FFileName;
    property MP3GainAction: TMP3GainAction read FMP3GainAction write FMP3GainAction;
    property TargetVolume: Real read FTargetVolume write FTargetVolume;
    property VolumeGain: Real read FVolumeGain write FVolumeGain;
    property Result: Real read FResult;
    property IsReady: Boolean read GetIsReady;
    property ExitCodeProcess: Integer read FExitCodeProcess write FExitCodeProcess;
    property OnRunFinished: TNotifyEvent read FOnRunFinished write FOnRunFinished;
    property SongItems: TSongItemList read FSongItemList;
    property ErrorHasOccured: Boolean read FErrorHasOccured default false;
end;

function RoundGainValue(Value: Double): Double;
  
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

function TMP3GainTaskList.Add(Item: TMP3GainTask): Integer;
begin
  Result := inherited Add(Item);
end;

function TMP3GainTaskList.AddTask(ASongItem: TSongItem; AMP3GainAction: TMP3GainAction; AVolume: Double): Integer;
var
  Item: TMP3GainTask;
begin
  Item := TMP3GainTask.Create;
  Item.MP3GainAction := AMP3GainAction;
  if not (ASongItem=nil) then Item.SongItems.Add(ASongItem);
  Item.Volume := AVolume;
  Result := inherited Add(Item);
end;

function TMP3GainTaskList.AddTask(ASongItem: Pointer; AMP3GainAction: TMP3GainAction; AVolume: Double): Integer;
begin
  AddTask(TSongItem(ASongItem), AMP3GainAction, AVolume);
end;

procedure TMP3GainTaskList.DeleteTask(AIndex: Integer);
begin
  TMP3GainTask(Items[AIndex]).Free;
  inherited Delete(AIndex);
end;

function TMP3GainTaskList.GetItem(AIndex:integer): TMP3GainTask;
begin
  Result := TMP3GainTask(inherited Items[AIndex]);
end;

procedure TMP3GainTaskList.SetItem(AIndex:integer;AItem:TMP3GainTask);
begin
  inherited Items[AIndex] := AItem;
end;

// ----------------------------------- TSongItemList --------------------------

function TSongItemList.Add(Item: TSongItem): Integer;
begin
  Result := inherited Add(Item);
end;

function TSongItemList.Add(Item: Pointer): Integer;
begin
  Result := inherited Add(Item);
end;

function TSongItemList.GetItem(AIndex:integer): TSongItem;
begin
  Result := TSongItem(inherited Items[AIndex]);
end;

procedure TSongItemList.SetItem(AIndex:integer; AItem:TSongItem);
begin
  inherited Items[AIndex] := AItem;
end;

// ----------------------------------- TMP3GainTask ---------------------------

constructor TMP3GainTask.Create;
begin
  inherited;
  FSongItemList := TSongItemList.Create;
  FSongItemList.Clear;
end;

destructor TMP3GainTask.Destroy;
begin
  FSongItemList.Free;
  inherited;
end;

// ----------------------------------- TMP3Gain -------------------------------

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
      for i:=1 to SL.Count-1 do
      begin
        data.DelimitedText := '"'+StringReplace(SL[i],#9,'"'#9'"',[rfReplaceAll]) + '"';
        if (SongItems.Count<i) then continue;
        SongItem := SongItems[i-1];

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
      end;
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
    FErrorHasOccured := True;
  end;
end;

function TMP3Gain.ExtractProgressValue(S: String; var CurrentSongItem: Integer): SmallInt;
var
  a, b: SmallInt;
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
      //FMP3GainProcess.FCurrentSongItem := ;
    end;
  end;
end;

procedure TMP3Gain.ProcessProgress;
var
  SL: TStringList;
  i, CurrentSongItem: Integer;
  b: SmallInt;
begin
  CurrentSongItem := 0;
  SL := TStringList.Create;
  try
    SL.Text := FMP3GainProcess.ProcessOutput;
    for i:= SL.Count-1 downto 0 do
    begin
      if (SL[i]='') then SL.Delete(i);
    end;
    if SL.Count=0 then exit;
    {$IFDEF DEBUG_VERSION}
      SL.SaveToFile('/home/thomas/prog.txt');
    {$ENDIF}
    for i:= SL.Count-1 downto 0 do
    begin
      b := ExtractProgressValue(SL[i], CurrentSongItem);
      if CurrentSongItem > FMP3GainProcess.FCurrentSongItem then
      begin
        FMP3GainProcess.FCurrentSongItem := CurrentSongItem;
        FProgress := 0;
      end;
      if b>FProgress then
      begin
        FProgress := b;
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
        SongItem.HasData := true; // TagInfo existing
        SongItem.Gain_Track := FResult+FTargetVolume-REF_VOLUME;
        SongItem.Volume_Track := REF_VOLUME-FResult;
      end;
      setAlbumGain:
      begin
        SongItem.HasAlbumData := true;
        SongItem.Gain_Album := FResult+FTargetVolume-REF_VOLUME;
        SongItem.Volume_Album := REF_VOLUME-FResult;
      end;
      setMaxAmplitude_Track:
      begin
        SongItem.Clipping := FBoolResult;
        SongItem.MaxAmplitude_Track := FResult;
      end;
      setMaxAmplitude_Album:
      begin
        SongItem.MaxAmplitude_Album := FResult;
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
  cmd, Filenames: String;
  i: Integer;
begin
  FReady := false;
  FProgress := 0;
  Filenames := '';
  FErrorHasOccured := false;
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
  for i:=0 to SongItems.Count-1 do
    Filenames := Filenames + ' "' + SongItems[i].FileName + '"';
  FMP3GainProcess.ProcessCommand := cmd + ' -o' + Filenames;    // -o Tab delimited output
  FMP3GainProcess.Resume;
end;

constructor TMP3Gain.Create;
begin
  inherited Create;
  FReady := true;
  SongItem := nil;
  FSongItemList := TSongItemList.Create;
end;

destructor TMP3Gain.Destroy;
begin
  FMP3GainProcess.Free;
  FSongItemList.Free;
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

function TMP3GainProcess.ReadProcessOutput(P: TProcess; Buffer: PChar): LongInt;
const
  READ_BYTES = 2048;
var
  n: LongInt;
  S: String;
begin
  //n := P.Output.Read(Buffer[0],P.Output. READ_BYTES);
  //Buffer[n] := Char(0);
  n := P.Output.NumBytesAvailable;
  P.Output.ReadBuffer(Buffer[0], n) ;
  Result := n;
  //StrCopy(Buffer,PChar(S));
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
  Buffer: array[0..READ_BYTES-1] of char;
  str_echo: String;
  e, a: Integer;
begin
  str_echo := '';
  FCurrentSongItem := 0;
  P := TProcess.Create(nil);
  try
    P.CommandLine := FProcessCommand;
    P.Options := [poUsePipes,poNoConsole];
    P.Execute;
    while P.Running do
    begin
      FillChar(Buffer,READ_BYTES,#0);
      n := P.Stderr.Read(Buffer, READ_BYTES);
      if n>0 then
      begin
        FProcessOutput := Buffer;
        Synchronize(OnProgressEvent);
      end;
      Sleep(100);
      repeat
        FillChar(Buffer,READ_BYTES,#0);
        n := ReadProcessOutput(P, @Buffer);
        if n>0 then
        begin
          str_echo := str_echo + Buffer;
        end;
      until n <= 0;
      FProcessOutput := str_echo;
      Synchronize(OnResultEvent);
      Self.
    end;
  finally
    e := P.ExitStatus;
    P.Free;
  end;
{$IFDEF DEBUG_VERSION}
  X := TStringList.Create;
  try
    X.Text := str_echo;
    X.Add(IntToStr(n));
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

