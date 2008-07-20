unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  callbackprocess, unitmp3gain, ComCtrls, Menus, gettext, translations;

type

  { TfrmMP3GainMain }

  TfrmMP3GainMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    lvFiles: TListView;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    mnuFile: TMenuItem;
    ProgressBar: TProgressBar;
    ProgressBarGeneral: TProgressBar;
    StatusBar: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ProcessCallbackEvent(pcChannel: TProcessChannel; strData: String);
    procedure OnMP3GainReady(Sender: TObject);
    procedure AddSongItem(AName: String);
    procedure UpdateFileCount;
    { private declarations }
  public
    procedure UpdateView(AItem: TSongItem);
    { public declarations }
  end; 

var
  frmMP3GainMain: TfrmMP3GainMain;
  BytesRead: Integer;
  ProcessOutput: String;
  FASongItemHasFinished:Boolean;
  FCurrentSongItem:Integer;
  FExitStatus: Integer;
  ACallBackProcess: TCallbackProcess;
  FilesProcessedCount, FilesToProcessCount: Integer;
  strHomeDir: String;
  TaskList: TMP3GainTaskList;
  MP3Gain: TMP3Gain;
  
implementation

resourcestring
   Caption1 = 'Some text';
   HelloWorld1 = 'Hello World';

{ TfrmMP3GainMain }

procedure TfrmMP3GainMain.Button1Click(Sender: TObject);
begin
  ACallBackProcess := TCallBackProcess.Create(Self);
  try
    ACallBackProcess.CallBackEvent := @ProcessCallbackEvent;
    ACallBackProcess.CommandLine := Edit1.Text;
    ACallBackProcess.Execute;
  finally
    ACallBackProcess.Free;
  end;
end;

procedure TfrmMP3GainMain.Button2Click(Sender: TObject);
var
  a: Integer;
begin
  ProgressBar.Position:= 0;
  ProgressBarGeneral.Position:=0;
  AddSongItem('/home/thomas/test.mp3'); //home/thomas/mp32/m1000.mp3
  a := TaskList.AddTask(lvFiles.Items[0].Data, mgaDeleteTagInfo, 89);
  a := TaskList.AddTask(lvFiles.Items[0].Data, mgaTrackAnalyze, 89);
  a := TaskList.AddTask(lvFiles.Items[0].Data, mgaCheckTagInfo, 89);
  FilesToProcessCount:=1;
  //QueueFiles(mgaCheckTagInfo, MP3Gain.TargetVolume, false);
  OnMP3GainReady(Self);
end;

procedure TfrmMP3GainMain.Button3Click(Sender: TObject);
begin
  Application.ProcessMEssages
end;

procedure TfrmMp3GainMain.AddSongItem(AName: String);
var
  SongItem: TSongItem;
  ListViewItem: TListItem;
  k: SmallInt;
begin
  ListViewItem := lvFiles.Items.Add;
  with ListViewItem do
  begin
    Caption := AName;
    SongItem := TSongItem.Create; //GetMem(SongItem, SizeOf(TSongItem));
    Data := SongItem;
    SongItem.ListViewItem := ListViewItem;
    SongItem.HasAlbumData := false;
    SongItem.HasData := false;
    SongItem.FileName := AName;
    SongItem.ExtractedFileName := ExtractFileName(AName);
    for k := 0 to SI_COUNT-1 do
    begin
      SongItem.ListViewItem.SubItems.Add('');
    end;
  end;
  if MP3GainOptions.AutoReadAtFileAdd then
  begin
    TaskList.AddTask(SongItem, mgaCheckTagInfo, MP3Gain.TargetVolume);
    if MP3Gain.IsReady then
      OnMP3GainReady(Self);
  end;
  UpdateFileCount;
end;

procedure TfrmMp3GainMain.UpdateFileCount;
begin
  StatusBar.Panels[SB_FILECOUNT].Text := IntToStr(lvFiles.Items.Count) + ' '+ strFiles;
end;

procedure TfrmMp3GainMain.UpdateView(AItem: TSongItem);
begin
  if not AItem.HasData then exit;
  with AItem.ListViewItem do
  begin
    SubItems[SI_TRACKGAIN] := Format('%.1f',[RoundGainValue(AItem.Gain_Track)]);
    SubItems[SI_VOLUME] := Format('%.1f',[AItem.Volume_Track]);
    SubItems[SI_CLIPPING] := boolStr[AItem.Clipping];
    if AItem.HasAlbumData then
    begin
      SubItems[SI_ALBUMGAIN] := Format('%.1f',[RoundGainValue(AItem.Gain_Album)]);
      SubItems[SI_ALBUMVOLUME] := Format('%.1f',[AItem.Volume_Album]);
    end;
  end;
  Application.ProcessMessages;
end;


procedure TfrmMP3GainMain.FormCreate(Sender: TObject);
    var
      PODirectory, Lang, FallbackLang: String;
begin
      PODirectory := './languages/';
      GetLanguageIDs(Lang, FallbackLang); // in unit gettext
      TranslateUnitResourceStrings('Unit1', PODirectory + 'process.%s.po', Lang, FallbackLang);
      Caption := HelloWorld1;
    
  MP3Gain := TMP3Gain.Create;
  MP3Gain.OnRunFinished := @OnMP3GainReady;
  MP3Gain.TargetVolume := REF_VOLUME;
  strHomeDir := IncludeTrailingPathDelimiter(getenvironmentvariable('HOME'));
  MP3GainOptions.UseTempFiles:=True;       // Pre-setting
  MP3GainOptions.AutoReadAtFileAdd:=False;  // Pre-setting
  MP3GainOptions.ToolBarImageListIndex:=1; // Pre-setting
  TaskList := TMP3GainTaskList.Create;
end;

procedure TfrmMP3GainMain.ProcessCallbackEvent(pcChannel: TProcessChannel; strData: String);
begin
  if pcChannel=pcStdError then
    Memo1.Lines.Add('stdError: '+strData);
  if pcChannel=pcStdOut then
    Memo1.Lines.Add('stdOut:   '+strData);
  if pcChannel=pcFinished then
    Memo1.Lines.Add('Finished.');
  if pcChannel=pcError then
    Memo1.Lines.Add('Error  '+ strData);
end;

procedure TfrmMp3GainMain.OnMP3GainReady(Sender: TObject);
var
  i: Integer;
  n,k: Integer;
begin
  //if (not (MP3Gain.SongItem=nil)) then UpdateView(MP3Gain.SongItem);
  //if TaskList.Count >0 then
  while (TaskList.Count >0) do
  begin
    n := 0;
    for k:=0 to TaskList[n].SongItems.Count-1 do
    begin
      with TaskList[n].SongItems[k].ListViewItem do
      begin
        for i:=0 to SubItems.Count-1 do
          SubItems[i] := '';
      end;
    end;
    MP3Gain.SongItems.Assign(TaskList[n].SongItems);
    //MP3Gain.FileName := TaskList[n].SongItem.FileName;
    MP3Gain.MP3GainAction := TaskList[n].MP3GainAction;
    if MP3Gain.MP3GainAction=mgaConstantGain then
      MP3Gain.VolumeGain := TaskList[n].Volume
    else
      MP3Gain.TargetVolume := TaskList[n].Volume;
    MP3Gain.Run;
    TaskList.DeleteTask(n);
  end;
  //else
  begin
    FilesToProcessCount := 0;
    FilesProcessedCount := 0;
  end;
end;


initialization
  {$I unit1.lrs}

end.

