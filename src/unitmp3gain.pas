
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
  Classes, SysUtils,ComCtrls,Forms, process, pipes;

type

  TSyncEventType = (setProgress, setStatusText, setStatusCode, setTrackGain,
    setAlbumGain, setMaxAmplitude_Track, setMaxAmplitude_Album,
    setSongItemHasFinished, setSongItemHasStarted, setWholeAlbumGain);

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
    Volume_Difference: Double;
    Volume_Old: Double;
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
    MP3GainAction: TMP3GainAction;
    Volume: Real;
    constructor Create;
    destructor Destroy; override;
  published
    property SongItems: TSongItemList read FSongItemList;
  end;
  

  TSynEvt = TThreadMethod;

  TMP3GainProcess = class(TThread)
  private
    FProcessCommand: String;
    FProgressEvent: TSynEvt;
    FResultEvent: TSynEvt;
    FFinishedEvent: TSynEvt;
    FStatusCodeEvent: TSynEvt;
    FExitStatus: Integer;
    FCurrentSongItem: Integer;
    FASongItemHasFinished: Boolean;
    //function ReadProcessOutput(Pipe: TInputPipeStream; Buffer: PChar): LongInt;
  protected
    procedure Execute; override;
  public
    Cancel: Boolean;
    ProcessOutput: String;
    Constructor Create(CreateSuspended : boolean);
    destructor Destroy; override;
  published
    property OnProgressEvent: TSynEvt read FProgressEvent write FProgressEvent;
    property OnResultEvent: TSynEvt read FResultEvent write FResultEvent;
    property OnFinished: TSynEvt read FFinishedEvent write FFinishedEvent;
    property OnStatusCodeEvent: TSynEvt read FStatusCodeEvent write FStatusCodeEvent;
    //property ProcessOutput: String read FProcessOutput;
    property ProcessCommand: String read FProcessCommand write FProcessCommand;
    property ExitStatus: Integer read FExitStatus;
    property ASongItemHasFinished: Boolean read FASongItemHasFinished write FASongItemHasFinished;
  end;

  { TMP3Gain }

  TMP3Gain = class
  private
    FMP3GainProcess:TMP3GainProcess;
    FReady: Boolean;
    FProgress: Byte;
    FStatusText: String;
    FExitCodeProcess: Integer;
    FMP3GainAction: TMP3GainAction;
    FTargetVolume: Real;
    FVolumeGain: Real;
    FResult: Real;
    FBoolResult: Boolean;
    FSongItemList: TSongItemList;
    FErrorHasOccured: Boolean;
    FHeaderList: TStringList;
    FDataList: TStringList;
    SongItem: TSongItem;
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
    procedure Run;
    constructor Create;
    destructor Destroy; override; // reintroduce
  published
    property Progress: Byte read FProgress;
    property StatusText: String read FStatusText;
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

  TMP3GainOptions = record
    IgnoreTags, AutoReadAtStartup, UseTempFiles, PreserveOriginalTimestamp:Boolean;
    
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
  
  SB_STATUS = 0;
  SB_FILECOUNT = 1;
  SB_FILENAME = 2;
  
  REF_VOLUME = 89;
  
  strConfigFileName: String = '.easymp3gain';
  
var
  TaskList: TMP3GainTaskList;
  strStatus_Analyzing: String = 'Analyzing...';
  strStatus_Gaining: String = 'Gaining...';
  strStatus_Finished: String = 'Finished.';
  strStatus_CheckingTagInfo: String = 'Checking Tag Info...';
  strStatus_DeletingTagInfo: String = 'Deleting Tag Info...';
  strStatus_UndoingChanges: String = 'Undoing Changes...';
  strStatus_ExitCode127: String = 'Could not start mp3gain. Is it installed?';
  strAbout: String = 'About';
  strFiles: String = 'File(s)';

  boolStr: array[Boolean] of String = ('no','yes');

  strHomeDir: String = '';
  
  MP3GainOptions: TMP3GainOptions;
  

implementation

uses UnitMain;

procedure Debugln(p1: String; p2: Integer; p3: String); overload;
begin
  if Print_Debug_Info then
    Writeln(p1, p2, p3);
end;

procedure Debugln(p1: String; p2: Integer); overload;
begin
  if Print_Debug_Info then
    Writeln(p1, p2);
end;

procedure Debugln(p1: String); overload;
begin
  if Print_Debug_Info then
    Writeln(p1);
end;

function RoundGainValue(Value: Double): Double;
var
  t: Double;
begin
  t := Round(Value/1.5);
  Result := Double(t*1.5);
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
  Result := AddTask(TSongItem(ASongItem), AMP3GainAction, AVolume);
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
  inherited Destroy;
end;

// ----------------------------------- TMP3Gain -------------------------------

function TMP3Gain.GetIsReady: Boolean;
begin
  Result := FReady;
end;

procedure TMP3Gain.ProcessResult;

  function GetSongItem(const AFileName: String): TSongItem;
  var
    i: Integer;
  begin
    Result := nil;
    for i:=SongItems.Count-1 downto 0 do
    begin
      if (SongItems[i].FileName = AFileName) then
        Result := SongItems[i];
    end;
    Debugln(AFileName+' ',i );
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
  if FMP3GainProcess.ExitStatus=0 then
  begin
    Album_Result_Event := false;
    SL := TStringList.Create;
    try
      SL.Text := FMP3GainProcess.ProcessOutput;
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
            Debugln('trackgain: ',p);
            Debugln('count: ', FDataList.Count);
            Val(FDataList[p],r,e);
            if not (e>0) then
            begin
              FResult := r;
              if Album_Result_Event then
                MP3GainSync(setWholeAlbumGain)
              else
                MP3GainSync(setTrackGain);
            end;
          end;

          p := FHeaderList.IndexOf(strResult_AlbumGain);
          if (p>-1) then
          begin
            Debugln('albumgain: ',p);
            Debugln('count: ', FDataList.Count);
            Val(FDataList[p],r,e);
            if not (e>0) then
            begin
              FResult := r;
              //if Album_Result_Event then
              //  MP3GainSync(setWholeAlbumGain);
              //else
              if not Album_Result_Event then
                MP3GainSync(setAlbumGain)
            end;
          end;

          p := FHeaderList.IndexOf(strResult_MaxAmplitudeTrack);
          if (p>-1) then
          begin
            Debugln('maxamp: ',p);
            Debugln('count: ', FDataList.Count);
            Val(FDataList[p],r,e);
            if not (e>0) then
            begin
              FResult := r;
              if not Album_Result_Event then
                MP3GainSync(setMaxAmplitude_Track);
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
      Debugln('Current Song Item: ', CurrentSongItem);
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
      SL.SaveToFile(strHomeDir+'prog.txt');
    {$ENDIF}
    for i:= SL.Count-1 downto 0 do
    begin
      b := ExtractProgressValue(SL[i], CurrentSongItem);
      if CurrentSongItem > FMP3GainProcess.FCurrentSongItem then
      begin
        FMP3GainProcess.FCurrentSongItem := CurrentSongItem;
        FProgress := 0;
        FMP3GainProcess.ASongItemHasFinished := true;
        MP3GainSync(setSongItemHasFinished);
        MP3GainSync(setSongItemHasStarted);
      end;
      if b>FProgress then
      begin
        FProgress := Byte(b);
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
  FStatusText := strStatus_Finished;
  MP3GainSync(setStatusText);
  FProgress:=100;
  MP3GainSync(setProgress);
  MP3GainSync(setSongItemHasFinished);
  OnRunFinished(Self);
end;

procedure TMP3Gain.MP3GainSync(value: TSyncEventType);
var
  i: Integer;
begin
  case value of
    setProgress:
      frmMP3GainMain.ProgressBar.Position := FProgress;
    setStatusText:
      frmMP3GainMain.StatusBar.Panels[0].Text := FStatusText;
    setStatusCode:
      if (FExitCodeProcess<>0) then
        frmMP3GainMain.StatusBar.Panels[1].Text := 'An error occured: ' + IntToStr(FExitCodeProcess);
    setSongItemHasFinished:
    begin
      Inc(FilesProcessedCount);
      frmMP3GainMain.ProgressBarGeneral.Max := FilesToProcessCount;
      frmMP3GainMain.ProgressBarGeneral.Position := FilesProcessedCount;
      frmMP3GainMain.StatusBar.Panels[SB_FILENAME].Text := '';
    end;
    setSongItemHasStarted:
    begin
      SongItem := SongItems[FMP3GainProcess.FCurrentSongItem]; // Careful!
      frmMP3GainMain.StatusBar.Panels[SB_FILENAME].Text := SongItem.FileName;
    end;
  end;
  if not (SongItem=nil) then
  begin
    Debugln('Synchronization of GUI and algorithm: ',Integer(value), SongItem.FileName);
    case value of
      setTrackGain:
      begin
        if FMP3GainAction = mgaTrackGain then exit; //FResult := REF_VOLUME - (SongItem.Volume_Old + SongItem.Volume_Difference); //REF_VOLUME - (SongItem.Volume_Track + FResult);
        if FMP3GainAction = mgaAlbumGain then exit;//FResult := REF_VOLUME - (SongItem.Volume_Old + SongItem.Volume_Difference);//REF_VOLUME - (FTargetVolume - FResult);
        SongItem.HasData := true; // TagInfo existing
        SongItem.Volume_Track := REF_VOLUME-FResult;  //FResult + SongItem.Gain_Track;
        SongItem.Gain_Track := FResult+FTargetVolume-REF_VOLUME; //FTargetVolume - SongItem.Volume_Track
      end;
      setAlbumGain:
      begin
        SongItem.HasAlbumData := true;
        SongItem.Gain_Album := FResult+FTargetVolume-REF_VOLUME;
        SongItem.Volume_Album := REF_VOLUME-FResult;
      end;
      setWholeAlbumGain:  // For the whole SongItem-List
      begin
        if FMP3GainAction = mgaAlbumGain then FResult := REF_VOLUME - (SongItem.Volume_Album + FResult);
        for i:=SongItems.Count-1 downto 0 do
        begin
          SongItems[i].HasAlbumData := true;
          SongItems[i].Gain_Album := FResult+FTargetVolume-REF_VOLUME;
          SongItems[i].Volume_Album := REF_VOLUME-FResult;
          frmMP3GainMain.UpdateView(SongItems[i]);
        end;
      end;
      setMaxAmplitude_Track:
      begin
        if FMP3GainAction = mgaTrackGain then FResult := SongItem.MaxAmplitude_Track + FResult;
        FBoolResult := FResult > 32768;
        SongItem.Clipping := FBoolResult;
        SongItem.MaxAmplitude_Track := FResult;
      end;
      setMaxAmplitude_Album:
      begin
        SongItem.MaxAmplitude_Album := FResult;
      end;
    end;
    frmMP3GainMain.UpdateView(SongItem);
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
  if SongItems.Count<1 then exit;
  //while (not FMP3GainProcess.Suspended) do ;//FMP3GainProcess.WaitFor;  // Thread-Bug in FPC2.2 True     1
  FReady := false;
  FProgress := 0;
  Filenames := '';
  SongItem := SongItems[0]; // Set Pointer to first Item
  FErrorHasOccured := false;
  MP3GainSync(setProgress);
  CreateProcess;    // Thread-Bug in FPC2.2 True     1
  FHeaderList.Clear;
  cmd := MP3_GAIN_CMD + ' ';
  for i:=0 to SongItems.Count-1 do
  begin
    SongItems[i].Volume_Old := SongItems[i].Volume_Track;
  end;
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
      for i:=0 to SongItems.Count-1 do
      begin
        SongItems[i].Volume_Difference:=RoundGainValue(FTargetVolume-SongItems[i].Volume_Album);
      end;
      cmd := cmd + '-a -d ' + Format('%3.1f',[FTargetVolume-REF_VOLUME]) + ' -c ';
      FStatusText := strStatus_Gaining;
    end;
    mgaTrackGain:
    begin
      for i:=0 to SongItems.Count-1 do
      begin
        SongItems[i].Volume_Difference:=RoundGainValue(FTargetVolume-SongItems[i].Volume_Track);
      end;
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
  FMP3GainProcess.ASongItemHasFinished := false;
  MP3GainSync(setSongItemHasStarted);
  FMP3GainProcess.Resume;
end;

constructor TMP3Gain.Create;
begin
  inherited Create;
  FReady := true;
  SongItem := nil;
  FSongItemList := TSongItemList.Create;
  FHeaderList := TStringList.Create;
  FDataList := TStringList.Create;
  FHeaderList.Delimiter := chr(9);
  FDataList.Delimiter := chr(9);
  //CreateProcess;  // Thread-Bug in FPC2.2 True    1
end;

destructor TMP3Gain.Destroy;
begin
  //FMP3GainProcess.Free;
  FHeaderList.Free;
  FDataList.Free;
  FSongItemList.Free;
  inherited Destroy;
end;

// ----------------------------------- TMP3GainProcess ------------------------

constructor TMP3GainProcess.Create(CreateSuspended : boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;   // Thread-Bug in FPC2.2 True    1
end;

destructor TMP3GainProcess.Destroy;
begin
  inherited Destroy;
end;

(*function TMP3GainProcess.ReadProcessOutput(Pipe: TInputPipeStream; Buffer: PChar): LongInt;
var
  n: LongInt;
begin
  n := Pipe.NumBytesAvailable;
  if n>0 then
    n := Pipe.Read(Buffer[0], n);
  Buffer[n] := #0; // add terminating 0
  Result := n;
end;*)


procedure TMP3GainProcess.Execute;

  function ReadFromPipeStream(AStream: TInputPipeStream; var AString: String): Integer;
  var
    M: TMemoryStream;
    BytesRead: Integer;
    n: Integer;
  begin
    M := TMemoryStream.Create;
    BytesRead := 0;
    try
      repeat
        M.SetSize(BytesRead + AStream.NumBytesAvailable);
        n := AStream.Read((M.Memory + BytesRead)^, AStream.NumBytesAvailable);
        Inc(BytesRead, n);
      until (n=0);
      if BytesRead>0 then
      begin
        Debugln(AString);
        SetLength(AString,BytesRead);
        M.Read(AString[1],BytesRead);
      end;
    finally
      M.Free;
      Result := BytesRead;
    end;
  end;
  
var
  P: TProcess;
{$IFDEF DEBUG_VERSION}
  X: TStringList;
{$ENDIF}
  e: Integer;
begin
  FCurrentSongItem := 0;
{$IFDEF DEBUG_VERSION}
  X := TStringList.Create;
{$ENDIF}
  P := TProcess.Create(nil);
  try
    P.CommandLine := FProcessCommand;
    P.Options := [poUsePipes,poNoConsole];
    P.Execute;
    Debugln('mp3gain started.');
    while P.Running do
    begin
      Debugln('Trying to read progress-output...');
      BytesRead := ReadFromPipeStream(P.Stderr, ProcessOutput);
      Debugln('Read ', BytesRead, ' Bytes');
      if BytesRead>0 then
      begin
        Debugln('Trying to synchronize progress...');
        Synchronize(OnProgressEvent);
        Debugln('Synchronized.');
      end;
      Sleep(100);
      if FASongItemHasFinished then   // A SongItem Finished, read the output
      begin
        Debugln('SongItem has finished: ', FCurrentSongItem);
        FASongItemHasFinished := false;
        BytesRead := ReadFromPipeStream(P.Output, ProcessOutput);
        if (BytesRead>0) then
        begin
          Debugln('Processing output from mp3gain: ');
          Debugln(' # ' + ProcessOutput);
          Synchronize(OnResultEvent);
        end;
       (* n := ReadProcessOutput(P.Output, @Buffer); //P.Output.Read(Buffer, READ_BYTES);
        if n>0 then
        begin
          str_echo := str_echo + Buffer;
          ProcessOutput := str_echo; //pchar
  {$IFDEF DEBUG_VERSION}
      X.Add(ProcessOutput);
      X.Add('## Count:'+IntToStr(n)+'##');
      X.SaveToFile(strHomeDir+'op.txt');
  {$ENDIF}
        Debugln('Processing output from mp3gain.');
        Synchronize(OnResultEvent);
        end; *)
      end;
    end;
    Debugln('Trying to read output...');
    BytesRead := ReadFromPipeStream(P.Output, ProcessOutput);
    Debugln('Read ', BytesRead, ' Bytes.');
    Debugln(ProcessOutput);
    Synchronize(OnResultEvent);
    (*
    n := ReadProcessOutput(P.Output, @Buffer);  // read the final output
    if n>0 then
    begin
      str_echo := str_echo + Buffer;
      ProcessOutput := str_echo; //pchar
    end;*)
  finally
    e := P.ExitStatus;
    P.Free;
  end;
{$IFDEF DEBUG_VERSION}
  X.Free;
{$ENDIF}
  M.Free;
  FExitStatus := e;
  Synchronize(OnStatusCodeEvent);
  Self.FreeOnTerminate := true;   // Thread-Bug in FPC2.2 True
  Synchronize(OnFinished);
  Self.Terminate;   // Thread-Bug in FPC2.2 True
end;



end.

