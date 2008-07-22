
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
  Classes, SysUtils,ComCtrls,Forms, callbackprocess;

type

  TSyncEventType = (setProgress, setStatusText, setStatusCode, setTrackGain,
    setAlbumGain, setMaxAmplitude_Track, setMaxAmplitude_Album,
    setSongItemHasFinished, setSongItemHasStarted, setWholeAlbumGain);

  TMediaGainAction = (mgaTrackAnalyze, mgaAlbumAnalyze, mgaCheckTagInfo,
    mgaDeleteTagInfo, mgaAlbumGain, mgaTrackGain, mgaConstantGain,
    mgaUndoChanges);
    
  TMediaGainTask = class;
  
  TMediaType = (mtMP3, mtVorbis, mtUnknown);

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
    MediaType: TMediaType;
  end;
  
  TMediaGainTaskList = class(TList)
  private
    function GetItem(AIndex:integer): TMediaGainTask;
    procedure SetItem(AIndex:integer; AItem: TMediaGainTask);
  protected
  public
    function Add(Item: TMediaGainTask): Integer;
    function AddTask(ASongItem: TSongItem; AMediaGainAction: TMediaGainAction; AVolume: Double): Integer; overload;
    function AddTask(ASongItem: Pointer; AMediaGainAction: TMediaGainAction; AVolume: Double): Integer; overload;
    procedure DeleteTask(AIndex: Integer);
    property Items[AIndex:integer]: TMediaGainTask read GetItem write SetItem;default;
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

  TMediaGainTask = class
  private
    FSongItemList: TSongItemList;
  public
    MediaGainAction: TMediaGainAction;
    Volume: Real;
    constructor Create;
    destructor Destroy; override;
  published
    property SongItems: TSongItemList read FSongItemList;
  end;
  

  TSynEvt = TThreadMethod;

  { TMediaGain }

  TMediaGain = class
  private
    FGainProcess:TCallbackProcess;
    FReady: Boolean;
    FProgress: Byte;
    FStatusText: String;
    FExitCodeProcess: Integer;
    FMediaGainAction: TMediaGainAction;
    FTargetVolume: Real;
    FVolumeGain: Real;
    FResult: Real;
    FBoolResult: Boolean;
    FSongItemList: TSongItemList;
    FErrorHasOccured: Boolean;
    FHeaderList: TStringList;
    FDataList: TStringList;
    SongItem: TSongItem;
    FConsoleOutput: TStrings;
    FCurrentSongItem: Integer;
    FCancel: Boolean;
    function ExtractProgressValue(S: String; var CurrentSongItem: Integer): SmallInt;
    function GetIsReady: Boolean;
    procedure MediaGainSync(value: TSyncEventType);
    procedure ProcessProgress(strData: String);
    procedure ProcessResult(strData: String);
    procedure ProcessStatusCode;
    procedure RunFinished;
    procedure CreateProcess;
    procedure FreeProcess;
    procedure OnGainProcessEvent(pcChannel: TProcessChannel; strData: String);
    procedure DoCancel(value: Boolean);
  public
    procedure Run;
    constructor Create;
    destructor Destroy; override; // reintroduce
  published
    property Cancel: Boolean read FCancel write DoCancel default false;
    property ConsoleOutput: TStrings read FConsoleOutput write FConsoleOutput;
    property Progress: Byte read FProgress;
    property StatusText: String read FStatusText;
    property MediaGainAction: TMediaGainAction read FMediaGainAction write FMediaGainAction;
    property TargetVolume: Real read FTargetVolume write FTargetVolume;
    property VolumeGain: Real read FVolumeGain write FVolumeGain;
    property Result: Real read FResult;
    property IsReady: Boolean read GetIsReady;
    property ExitCodeProcess: Integer read FExitCodeProcess write FExitCodeProcess;
    //property OnRunFinished: TNotifyEvent read FOnRunFinished write FOnRunFinished;
    property SongItems: TSongItemList read FSongItemList;
    property ErrorHasOccured: Boolean read FErrorHasOccured default false;
end;

  TMediaGainOptions = record
    IgnoreTags, AutoReadAtFileAdd, UseTempFiles, PreserveOriginalTimestamp:Boolean;
    ToolBarImageListIndex: Integer;
    
  end;

function RoundGainValue(Value: Double): Double;
  
const

{$IFDEF LINUX}
  MP3_GAIN_CMD = 'mp3gain';
{$ENDIF}
{$IFDEF WIN32}
  MP3_GAIN_CMD = 'MP3Gain.exe';
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
  SB_ERROR = 1;
  
  REF_VOLUME = 89;
  
  strConfigFileName: String = '.easyMediaGain';
  
  CONSOLE_OUTPUT_MAX_LINES = 400;
  
resourcestring
  strStatus_Analyzing = 'Analyzing...';
  strStatus_Gaining = 'Gaining...';
  strStatus_Finished = 'Finished.';
  strStatus_CheckingTagInfo = 'Checking Tag Info...';
  strStatus_DeletingTagInfo = 'Deleting Tag Info...';
  strStatus_UndoingChanges = 'Undoing Changes...';
  strStatus_ExitCode127 = 'Could not start mp3Gain. Is it installed?';
  strStatus_Aborted = 'Aborted.';
  strAbout = 'About';
  strFiles = 'File(s)';
  strYes = 'yes';
  strNo = 'no';

var
  TaskList: TMediaGainTaskList;

  boolStr: array[Boolean] of String = (strNo,strYes);

  strHomeDir: String = '';
  
  MediaGainOptions: TMediaGainOptions;
  

implementation

uses UnitMain;


function RoundGainValue(Value: Double): Double;
var
  t: Double;
begin
  t := Round(Value/1.5);
  Result := Double(t*1.5);
end;

// ----------------------------------- TMediaGainTaskList -----------------------

function TMediaGainTaskList.Add(Item: TMediaGainTask): Integer;
begin
  Result := inherited Add(Item);
end;

function TMediaGainTaskList.AddTask(ASongItem: TSongItem; AMediaGainAction: TMediaGainAction; AVolume: Double): Integer;
var
  Item: TMediaGainTask;
begin
  Item := TMediaGainTask.Create;
  Item.MediaGainAction := AMediaGainAction;
  if not (ASongItem=nil) then Item.SongItems.Add(ASongItem);
  Item.Volume := AVolume;
  Result := inherited Add(Item);
end;

function TMediaGainTaskList.AddTask(ASongItem: Pointer; AMediaGainAction: TMediaGainAction; AVolume: Double): Integer;
begin
  Result := AddTask(TSongItem(ASongItem), AMediaGainAction, AVolume);
end;

procedure TMediaGainTaskList.DeleteTask(AIndex: Integer);
begin
  TMediaGainTask(Items[AIndex]).Free;
  inherited Delete(AIndex);
end;

function TMediaGainTaskList.GetItem(AIndex:integer): TMediaGainTask;
begin
  Result := TMediaGainTask(inherited Items[AIndex]);
end;

procedure TMediaGainTaskList.SetItem(AIndex:integer;AItem:TMediaGainTask);
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

// ----------------------------------- TMediaGainTask ---------------------------

constructor TMediaGainTask.Create;
begin
  inherited;
  FSongItemList := TSongItemList.Create;
  FSongItemList.Clear;
end;

destructor TMediaGainTask.Destroy;
begin
  FSongItemList.Free;
  inherited Destroy;
end;

// ----------------------------------- TMediaGain -------------------------------

procedure TMediaGain.DoCancel(value: Boolean);
begin
  if (value and Assigned(FGainProcess)) then
    ;//FGainProcess.Free;
  FCancel := value;
end;

function TMediaGain.GetIsReady: Boolean;
begin
  Result := FReady;
end;

procedure TMediaGain.ProcessResult(strData: String);

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
  //if FMediaGainProcess.ExitStatus=0 then
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
              FResult := r;
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
              FResult := r;
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
              FResult := r;
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
  (*else  // Exit Code <> 0
  begin
    if FMediaGainProcess.ExitStatus=127 then
      FStatusText := strStatus_ExitCode127
    else
      FStatusText := 'Error running MediaGain: ' + IntToStr(FMediaGainProcess.ExitStatus);
    MediaGainSync(setStatusText);
    FProgress := 100;
    MediaGainSync(setProgress);
    FErrorHasOccured := True;
  end;*)
end;

function TMediaGain.ExtractProgressValue(S: String; var CurrentSongItem: Integer): SmallInt;
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

procedure TMediaGain.ProcessProgress(strData: String);
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
        MediaGainSync(setSongItemHasFinished);
        MediaGainSync(setSongItemHasStarted);
      end;
      if b>FProgress then
      begin
        FProgress := Byte(b);
      end;
    end;
  finally
    SL.Free;
    MediaGainSync(setProgress);
  end;
end;

procedure TMediaGain.ProcessStatusCode;
begin
  //FExitCodeProcess := FGainProcess.ExitStatus;
  MediaGainSync(setStatusCode);
end;

procedure TMediaGain.RunFinished;
begin
  FReady := true;
  FStatusText := strStatus_Finished;
  FProgress:=100;
  if not (FMediaGainAction = mgaCheckTagInfo) then
  begin
    MediaGainSync(setStatusText);   // only synchronize when not checking TagInfo, because
    MediaGainSync(setProgress);     // its much faster when loading long file lists
  end;
  MediaGainSync(setSongItemHasFinished);
end;

procedure TMediaGain.MediaGainSync(value: TSyncEventType);
var
  i: Integer;
begin
  case value of
    setProgress:
      frmMP3GainMain.ProgressBar.Position := FProgress;
    setStatusText:
      frmMP3GainMain.StatusBar.Panels[SB_STATUS].Text := FStatusText;
    setStatusCode:
      if (FExitCodeProcess<>0) then
      begin
        if FExitCodeProcess=127 then
          frmMP3GainMain.StatusBar.Panels[SB_ERROR].Text := 'Error: Could not run mp3gain'
        else
          frmMP3GainMain.StatusBar.Panels[SB_ERROR].Text := 'An error occured: ' + IntToStr(FExitCodeProcess);
      end
      else
      begin
         frmMP3GainMain.StatusBar.Panels[SB_STATUS].Text := '';
      end;
    setSongItemHasFinished:
    begin
      Inc(FilesProcessedCount);
      frmMP3GainMain.ProgressBarGeneral.Max := FilesToProcessCount;
      frmMP3GainMain.ProgressBarGeneral.Position := FilesProcessedCount;
      //frmMP3GainMain.StatusBar.Panels[SB_FILENAME].Text := '';
    end;
    setSongItemHasStarted:
    begin
      //SongItem := SongItems[FGainProcess.FCurrentSongItem]; // Careful!
      frmMP3GainMain.StatusBar.Panels[SB_FILENAME].Text := SongItem.FileName;
    end;
  end;
  if not (SongItem=nil) then
  begin
    case value of
      setTrackGain:
      begin
        if FMediaGainAction = mgaTrackGain then exit; //FResult := REF_VOLUME - (SongItem.Volume_Old + SongItem.Volume_Difference); //REF_VOLUME - (SongItem.Volume_Track + FResult);
        if FMediaGainAction = mgaAlbumGain then exit;//FResult := REF_VOLUME - (SongItem.Volume_Old + SongItem.Volume_Difference);//REF_VOLUME - (FTargetVolume - FResult);
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
        if FMediaGainAction = mgaAlbumGain then FResult := REF_VOLUME - (SongItem.Volume_Album + FResult);
        for i:=SongItems.Count-1 downto 0 do
        begin
          SongItems[i].HasAlbumData := true;
          SongItems[i].Gain_Album := FResult+FTargetVolume-REF_VOLUME;
          SongItems[i].Volume_Album := REF_VOLUME-FResult;
//qwert          frmMP3GainMain.UpdateView(SongItems[i]);
        end;
      end;
      setMaxAmplitude_Track:
      begin
        if FMediaGainAction = mgaTrackGain then FResult := SongItem.MaxAmplitude_Track + FResult;
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
  Application.ProcessMessages;
end;

procedure TMediaGain.CreateProcess;
begin
  FGainProcess := TCallBackProcess.Create(Application);
  FGainProcess.CallBackEvent := @OnGainProcessEvent;
end;

procedure TMediaGain.FreeProcess;
begin
  FGainProcess.Free;
end;

procedure TMediaGain.OnGainProcessEvent(pcChannel: TProcessChannel; strData: String);
begin
  if (Assigned(FConsoleOutput)) then
  begin
    FConsoleOutput.Add(strData);
    while (FConsoleOutput.Count > CONSOLE_OUTPUT_MAX_LINES) do
      FConsoleOutput.Delete(0);
  end;
    
  if pcChannel=pcStdError then
    ProcessProgress(strData);
  if pcChannel=pcStdOut then
    ProcessResult(strData);
  if pcChannel=pcFinished then
    RunFinished;
  if pcChannel=pcError then
  begin
    FExitCodeProcess := 127;
    MediaGainSync(setStatusCode);
  end;
end;

procedure TMediaGain.Run;
var
  cmd, Filenames: String;
  i: Integer;
begin
  if SongItems.Count<1 then exit;
  //while (not FMediaGainProcess.Suspended) do ;//FMediaGainProcess.WaitFor;  // Thread-Bug in FPC2.2 True     1
  FReady := false;
  FProgress := 0;
  Filenames := '';
  SongItem := SongItems[0]; // Set Pointer to first Item
  FCurrentSongItem := 0;
  FErrorHasOccured := false;
  MediaGainSync(setProgress);
  CreateProcess;    // Thread-Bug in FPC2.2 True     1
  FHeaderList.Clear;
  cmd := MP3_GAIN_CMD + ' ';
  for i:=0 to SongItems.Count-1 do
  begin
    SongItems[i].Volume_Old := SongItems[i].Volume_Track;
  end;
  case FMediaGainAction of
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
  if MediaGainOptions.UseTempFiles then
    cmd := cmd + '-t '
  else
    cmd := cmd + '-T ';
  if MediaGainOptions.PreserveOriginalTimestamp then cmd := cmd + '-p ';
  if MediaGainOptions.IgnoreTags then cmd := cmd + '-s s ';
  MediaGainSync(setStatusText);
  for i:=0 to SongItems.Count-1 do
    Filenames := Filenames + ' "' + SongItems[i].FileName + '"';
  FGainProcess.CommandLine := cmd + ' -o' + Filenames;    // -o Tab delimited output
  MediaGainSync(setSongItemHasStarted);
  FGainProcess.Execute;
  while not (FReady) do
    Application.ProcessMessages;
end;

constructor TMediaGain.Create;
begin
  inherited Create;
  FReady := true;
  SongItem := nil;
  FSongItemList := TSongItemList.Create;
  FHeaderList := TStringList.Create;
  FDataList := TStringList.Create;
  FConsoleOutput := nil;
  FHeaderList.Delimiter := chr(9);
  FDataList.Delimiter := chr(9);
end;

destructor TMediaGain.Destroy;
begin
  FHeaderList.Free;
  FDataList.Free;
  FSongItemList.Free;
  //FConsoleOutput.Free;
  inherited Destroy;
end;

end.

