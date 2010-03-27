unit UnitMain;

{
     Copyright (C) 2007-2010 by Thomas Dieffenbach
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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, StdCtrls, Process, UnitMediaGain, UnitGainConstant, Math, ExtCtrls,
  Buttons, LazHelpHTML, HelpIntfs;

type

  { TfrmMp3GainMain }

  TfrmMp3GainMain = class(TForm)
    bvlTargetVolume: TBevel;
    edtVolume: TEdit;
    HTMLBrowserHelpViewer: THTMLBrowserHelpViewer;
    HTMLHelpDatabase: THTMLHelpDatabase;
    ImageList1: TImageList;
    ImageList2: TImageList;
    ImageList_Oxygen: TImageList;
    lblTargetVolume: TLabel;
    lblTargetVolumeUnit: TLabel;
    lvFiles: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    mnuHelpHelp: TMenuItem;
    mnuOptionsShowConsoleOutput: TMenuItem;
    mnuSelectionInvert: TMenuItem;
    mnuFileSelectNone: TMenuItem;
    mnuFileSelectAll: TMenuItem;
    mnuOptionsAdvanced: TMenuItem;
    mnuFileAddFolderRecursive: TMenuItem;
    mnuOptionsOnlySelectedItems: TMenuItem;
    pnlVolume: TPanel;
    pmnGainAlbum: TMenuItem;
    pmnGainTrack: TMenuItem;
    pmnAnalysisTrack: TMenuItem;
    pmnAnalysisAlbum: TMenuItem;
    mnuOptionsReadTagInfo: TMenuItem;
    mnuOptionsDeleteTagInfos: TMenuItem;
    mnuFileClearAllFiles: TMenuItem;
    mnuFileClearSelected: TMenuItem;
    mnuHelpInfo: TMenuItem;
    mnuHelp: TMenuItem;
    mnuOptions: TMenuItem;
    mnuModifyGainUndo: TMenuItem;
    mnuModifyGainApplyTrack: TMenuItem;
    mnuModifyGainApplyConstant: TMenuItem;
    MenuItem4: TMenuItem;
    mnuModifyGainApplyAlbum: TMenuItem;
    mnuModifyGain: TMenuItem;
    mnuAnalysisTrack: TMenuItem;
    mnuAnalysisClear: TMenuItem;
    MenuItem5: TMenuItem;
    mnuAnalysisAlbum: TMenuItem;
    mnuAnalysis: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuFileAddFiles: TMenuItem;
    mnuFileAddFolder: TMenuItem;
    mnuFile: TMenuItem;
    OpenDialog: TOpenDialog;
    pmnAnalysis: TPopupMenu;
    pmnGain: TPopupMenu;
    ProgressBar: TProgressBar;
    ProgressBarGeneral: TProgressBar;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    btnAddFiles: TToolButton;
    btnAddFolder: TToolButton;
    btnAnalysis: TToolButton;
    btnGain: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    btnClearFiles: TToolButton;
    btnClearAll: TToolButton;
    btnOnlySelectedItems: TToolButton;
    ToolButton4: TToolButton;
    btnCancel: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure ListView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnAddFilesClick(Sender: TObject);
    procedure btnAddFolderClick(Sender: TObject);
    procedure btnAnalysisClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
    procedure btnClearFilesClick(Sender: TObject);
    procedure btnGainClick(Sender: TObject);
    procedure btnOnlySelectedItemsClick(Sender: TObject);
    procedure edtVolumeChange(Sender: TObject);
    procedure lvFilesColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure lvFilesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mnuAnalysisAlbumClick(Sender: TObject);
    procedure mnuAnalysisClearClick(Sender: TObject);
    procedure mnuAnalysisTrackClick(Sender: TObject);
    procedure mnuFileAddFilesClick(Sender: TObject);
    procedure mnuFileAddFolderClick(Sender: TObject);
    procedure mnuFileAddFolderRecursiveClick(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure mnuFileClearAllFilesClick(Sender: TObject);
    procedure mnuFileClearSelectedClick(Sender: TObject);
    procedure mnuFileSelectAllClick(Sender: TObject);
    procedure mnuHelpHelpClick(Sender: TObject);
    procedure mnuHelpInfoClick(Sender: TObject);
    procedure mnuModifyGainApplyAlbumClick(Sender: TObject);
    procedure mnuModifyGainApplyConstantClick(Sender: TObject);
    procedure mnuModifyGainApplyTrackClick(Sender: TObject);
    procedure mnuModifyGainUndoClick(Sender: TObject);
    procedure mnuOptionsAdvancedClick(Sender: TObject);
    procedure mnuOptionsDeleteTagInfosClick(Sender: TObject);
    procedure mnuOptionsOnlySelectedItemsClick(Sender: TObject);
    procedure mnuOptionsReadTagInfoClick(Sender: TObject);
    procedure mnuFileSelectNoneClick(Sender: TObject);
    procedure mnuOptionsShowConsoleOutputClick(Sender: TObject);
    procedure mnuSelectionInvertClick(Sender: TObject);
    procedure StatusBarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    MediaGain: TMediaGain;
    procedure QueueFiles(AAction: TMediaGainAction; AVolume: Double; CheckTagInfoAfterwards: Boolean);
    procedure ProcessQueue(Sender: TObject);
    function AddSongItem(AName: String): TSongItem;
    procedure DelSongItem(AItemIndex: Integer);
    procedure LockControls(lock: Boolean; freeze_ListView: Boolean);
    //procedure LoadLanguageFile(AFile: String);
    procedure AddFiles(SL: TStrings);
    procedure AddFolder(S: String; sublevels: Integer);
    procedure AddFileAndDirectoryList(SL: TStringList; sublevels: Integer);
    procedure UpdateFileCount;
    procedure SortListView(Lv:TListView; Index:integer; Reverse: Boolean);
    function AddTask(ASongItem: TSongItem; AMediaGainAction: TMediaGainAction; AVolume: Double): Integer;
    function FitsTaskType(ATask: TMediaGainTask; AMediaGainAction: TMediaGainAction; ASongItem: TSongItem): Boolean;
    { private declarations }
  public
    procedure Init;
    procedure UpdateView(AItem: TSongItem);
    { public declarations }
  end; 
  
  TStringArray = array of String;
  
  const
   READ_BYTES = 2048;
   
   APPLICATION_NAME = 'easyMP3Gain';
   APPLICATION_VERSION = '0.5.1 SVN-0124';
   APPLICATION_URL = 'http://easymp3gain.sourceforge.net';
   HELP_DIR = 'help';
   EASYMP3GAIN_DIR = '/usr/share/easymp3gain/';

  SI_PATH = 1;
  SI_FILE = 0;
  SI_VOLUME = 0+2;
  SI_CLIPPING = 1+2;
  SI_TRACKGAIN = 2+2;
  SI_CLIPTRACK = 3+2;
  SI_ALBUMVOLUME = 4+2;
  SI_ALBUMGAIN = 5+2;
  SI_CLIPALBUM = 6+2;

  SI_COUNT = 9;

  LVFILES_SUBITEMS_MIN = 2;
  LVFILES_SUBITEMS_MAX = 8;

  resourcestring
   APPLICATION_DESCRIPTION = 'graphical user interface for mp3gain, aacgain and vorbisgain';
   COLUMN_PATHANDFILE = 'Path/File';
   COLUMN_FILE = 'File';
   COLUMN_PATH = 'Path';
   COLUMN_VOLUME = 'Volume';
   COLUMN_CLIPPING = 'clipping';
   COLUMN_TRACKGAIN = 'Track Gain';
   COLUMN_CLIPTRACK = 'clip';
   COLUMN_ALBUMVOLUME = 'Album Vol.';
   COLUMN_ALBUMGAIN = 'Album Gain';
   COLUMN_CLIPALBUM = 'clip (Album)';
   TARGET_VOLUME_HINT = 'Only valid for MP3 and AAC files. Ogg-Vorbis files'' target volume is fixed to 89 dB.';

 var
   S: TStringList;
   M: TMemoryStream;
   P: TProcess;
   n: LongInt;
   BytesRead: LongInt;
   FilesToProcessCount: Integer=0;
   FilesProcessedCount: Integer=0;
   Print_Debug_Info: Boolean = false;
   strWidgetSet: String;
   strHomeDir: String = '';
   strBinDir: String = '';
   strLang: String = '';

var
  frmMp3GainMain: TfrmMp3GainMain;

implementation

uses unitInfo, unitConsoleOutput, unitTranslate, unitOptions
  {$IFDEF UNIX}, BaseUnix{$ENDIF};

{ TfrmMp3GainMain }

Procedure ListFiles(const FilePath: String; Extension: array of String;
                    ListBox:TStringList; SubLevelMax: Byte);
                    
  function IsRealDirectory(const strPath, strFile: string):Boolean;
  var
    info: stat;
  begin
    fplstat(strPath + strFile, @info);
    Result := not ((strFile='.') or (strFile='..') or fpS_ISLNK(info.st_mode));
  end;

var
  SR: TSearchRec;
  i: SmallInt;
begin
  if FindFirst(FilePath+'*',faAnyFile,SR)=0 then
  repeat
    for i:=Low(Extension) to High(Extension) do            // Files
    begin
      if (LowerCase(ExtractFileExt(SR.Name))='.'+Extension[i]) and
        not ((faDirectory and SR.Attr)=faDirectory) then
        ListBox.Add(FilePath + SR.Name);
    end;
    if (SubLevelMax>0) and (((faDirectory or faSymLink) and SR.Attr)=faDirectory) then  //Directories
    begin
      if IsRealDirectory(FilePath, SR.Name) then
        ListFiles(IncludeTrailingPathDelimiter(FilePath + SR.Name), Extension, ListBox, SubLevelMax-1);
    end;
  until FindNext(SR)<>0;
  FindClose(SR);

end;

function URLDecode(a: String): String;
var
  i: Integer;
begin
  i:=1;
  Result := '';
  if Copy(a,1,7)='file://' then                   // removes "files://" (Qt)
    a := Copy(a,8,Length(a)-1)
  else
  begin
    Result := a;
    exit;
  end;
  while i<=Length(a) do
  begin
    if not (a[i]='%') then
    begin
      Result := Result + a[i];
      inc(i);
    end
    else
    begin
      Result := Result + Chr(StrToInt('$'+a[i+1]+a[i+2]));
      inc(i,3);
    end;
  end;
end;

procedure TfrmMp3GainMain.SortListView(Lv:TListView; Index:integer; Reverse: Boolean);
 var StrLi: TStringList;
   i: integer;
 begin
   StrLi := TStringList.Create;
   if Index = 0 then
     for i := 0 to Lv.Items.Count - 1 do StrLi.AddObject(Lv.Items[i].Caption, Lv.Items[i])
   else
     for i := 0 to Lv.Items.Count - 1 do StrLi.AddObject(Lv.Items[i].SubItems[Index - 1], Lv.Items[i]);
   StrLi.Sort;
   if not Reverse then
     for i := 0 to StrLi.count - 1 do Lv.Items[i] := TListItem(StrLi.Objects[i])
   else
     for i := 0 to StrLi.count - 1 do Lv.Items[i] := TListItem(StrLi.Objects[StrLi.count-1-i]);
   StrLi.free;
 end;

procedure TfrmMp3GainMain.UpdateFileCount;
begin
  StatusBar.Panels[SB_FILECOUNT].Text := IntToStr(lvFiles.Items.Count) + ' '+ strFiles;
end;

procedure TfrmMp3GainMain.Init;
var
  SL:TStringList;
  i: SmallInt;
  aSubLevelCount: Byte;
  strArch :String;
begin
  Self.Caption := APPLICATION_NAME + ' ' + APPLICATION_VERSION;
  strWidgetset := 'not specified';
  strArch := 'unknown';
  {$IFDEF LCLwin32}strWidgetset := 'Win32';{$ENDIF}
  {$IFDEF LCLgtk}strWidgetset := 'GTK';{$ENDIF}
  {$IFDEF LCLgtk2}strWidgetset := 'GTK2';{$ENDIF}
  {$IFDEF LCLqt}strWidgetset := 'Qt4';{$ENDIF}
  {$IFDEF LCLcarbon}strWidgetset := 'Carbon';{$ENDIF}
  {$IFDEF CPU32} strArch := '32bit'; {$ENDIF}
  {$IFDEF CPU64} strArch := '64bit'; {$ENDIF}

  MediaGain := TMediaGain.Create;
  MediaGain.TargetVolume := REF_VOLUME;
  MediaGain.ConsoleOutput := frmMP3GainConsoleOutput.memoData.Lines;
  strHomeDir := IncludeTrailingPathDelimiter(getenvironmentvariable('HOME'));
  HTMLHelpDataBase.BaseURL := 'file://' + HELP_DIR;

  bvlTargetVolume.Hint := TARGET_VOLUME_HINT;
  lblTargetVolume.Hint := TARGET_VOLUME_HINT;
  edtVolume.Hint := TARGET_VOLUME_HINT;

  // directory where langauge files, help files, etc be found.
  if DirectoryExists(EASYMP3GAIN_DIR) then
    strBinDir := EASYMP3GAIN_DIR
  else
    strBinDir := IncludeTrailingPathDelimiter(Application.Location);
  //ShowMessage(strBinDir);
  TranslateAll;

  MediaGainOptions.TargetVolume := @(MediaGain.TargetVolume);
  if not frmMP3GainOptions.LoadSettings then         // Load settings from config-file
  begin
    MediaGainOptions.UseTempFiles:=True;               // Pre-setting
    MediaGainOptions.AutoReadAtFileAdd:=True;          // Pre-setting
    MediaGainOptions.ToolBarImageListIndex:=1;         // Pre-setting
    MediaGainOptions.AnalysisTypeAlbum := False;       // Pre-setting
    MediaGainOptions.GainTypeAlbum := False;           // Pre-setting
    MediaGainOptions.SubLevelCount := 8;               // Pre-setting
    MediaGainOptions.FileNameDisplay_FileAndPath:=True;// Pre-setting
    MediaGainOptions.strMP3GainBackend := 'mp3gain';   // Pre-setting
    MediaGainOptions.strAACGainBackend := 'aacgain';   // Pre-setting
    MediaGainOptions.strVorbisGainBackend := 'vorbisgain';   // Pre-setting
    ToolBar1.Images := frmMp3GainMain.ImageList1;
  end;
  if MediaGainOptions.strMP3GainBackend ='' then
    MediaGainOptions.strMP3GainBackend := 'mp3gain';   // Pre-setting
  if MediaGainOptions.strAACGainBackend ='' then
    MediaGainOptions.strAACGainBackend := 'aacgain';   // Pre-setting
  if MediaGainOptions.strVorbisGainBackend ='' then
    MediaGainOptions.strVorbisGainBackend := 'vorbisgain';   // Pre-setting
  frmMP3GainOptions.SettingsToControls;
  frmMP3GainOptions.SettingsToMainForm;
  {$IFDEF LCLqt}
  ToolBar1.Images := frmMP3GainMain.ImageList_Oxygen;
  {$ENDIF}
    
  TaskList := TMediaGainTaskList.Create;
  frmMp3GainMain.ImageList1.GetBitmap(8,frmMP3GainGUIInfo.Image1.Picture.Bitmap);

  
  frmMP3GainGUIInfo.lblDescription.Caption := APPLICATION_NAME + ', ' +
     APPLICATION_DESCRIPTION +#10 +'Toolkit: '+strWidgetset +
      #10 + 'CPU: ' + strArch +
      #10#10 + '(c) 2007-2010, Thomas Dieffenbach';
  frmMP3GainGUIInfo.lblProgramName.Caption := APPLICATION_NAME+' '+APPLICATION_VERSION;
  frmMP3GainGUIInfo.lblURL.Caption := APPLICATION_URL;
  frmMP3GainGUIInfo.Caption := strAbout + ' ' + APPLICATION_NAME;

  lvFiles.Column[0].Caption                := COLUMN_PATHANDFILE;
  lvFiles.Column[SI_FILE+1].Caption        := COLUMN_FILE;
  lvFiles.Column[SI_PATH+1].Caption        := COLUMN_PATH;
  lvFiles.Column[SI_VOLUME+1].Caption      := COLUMN_VOLUME;
  lvFiles.Column[SI_CLIPPING+1].Caption    := COLUMN_CLIPPING;
  lvFiles.Column[SI_TRACKGAIN+1].Caption   := COLUMN_TRACKGAIN;
  lvFiles.Column[SI_CLIPTRACK+1].Caption   := COLUMN_CLIPTRACK;
  lvFiles.Column[SI_ALBUMVOLUME+1].Caption := COLUMN_ALBUMVOLUME;
  lvFiles.Column[SI_ALBUMGAIN+1].Caption   := COLUMN_ALBUMGAIN;
  lvFiles.Column[SI_CLIPALBUM+1].Caption   := COLUMN_CLIPALBUM;
  
  lblTargetVolume.Width := lblTargetVolume.Canvas.TextWidth(lblTargetVolume.Caption);
  lblTargetVolumeUnit.Width := lblTargetVolume.Canvas.TextWidth(lblTargetVolumeUnit.Caption);
  edtVolume.Left := lblTargetVolume.Width + 10;
  lblTargetVolumeUnit.Left := edtVolume.Left + 50;
  pnlVolume.Width := lblTargetVolumeUnit.Left + lblTargetVolumeUnit.Width + 20;
  
  if ParamStr(1)='-r' then
    aSubLevelCount := MediaGainOptions.SubLevelCount
  else
    aSubLevelCount := 0;
  SL := TStringList.Create;
  try
    for i:= 1 to ParamCount do
      SL.Add(Paramstr(i));
    AddFileAndDirectoryList(SL,aSubLevelCount);
  finally
    SL.Free;
  end;
end;

procedure TfrmMp3GainMain.LockControls(lock: Boolean; freeze_ListView: Boolean);
var
  i: Integer;
begin
  edtVolume.Enabled := not lock;
  {$IFDEF LCLgtk2}                    // GTK2's TListView is some kind of slow!
  if freeze_ListView then
  begin
    if lock then
      lvFiles.BeginUpdate
    else
      lvFiles.EndUpdate;
  end;
  {$ENDIF}
  for i:=0 to ToolBar1.ButtonCount-1 do
  begin
    ToolBar1.Buttons[i].Enabled := not lock;
  end;
  for i:=0 to MainMenu1.Items.Count-1 do
  begin
    MainMenu1.Items[i].Enabled := not lock;
  end;
  btnCancel.Enabled := lock;
end;

function TfrmMp3GainMain.FitsTaskType(ATask: TMediaGainTask; AMediaGainAction: TMediaGainAction; ASongItem: TSongItem): Boolean;
begin
  Result := False;
  if ATask.SongItems.Count = 0 then
    exit;
  if (ATask.SongItems[0].MediaType = ASongItem.MediaType) and
     (ATask.SongItems[0].FilePath = ASongItem.FilePath) and
     (ATask.MediaGainAction = AMediaGainAction) then
     Result := True;

end;

function TfrmMp3GainMain.AddTask(ASongItem: TSongItem; AMediaGainAction: TMediaGainAction; AVolume: Double): Integer; overload;
var
  iListIdx: Integer;
  bListIdxFound: Boolean;
begin
  Result := -1;
  bListIdxFound := False;
  if (AMediaGainAction=mgaAlbumAnalyze) or
     ((AMediaGainAction=mgaAlbumGain) and (ASongItem.MediaType=mtVorbis)) then
  begin
    for iListIdx:=0 to TaskList.Count-1 do
    begin
      if FitsTaskType(TaskList[iListIdx], AMediaGainAction, ASongItem) then
      begin
        bListIdxFound := True;
        break;
      end;
    end;
  end;
  
  if (bListIdxFound) then
  begin
    TaskList[iListIdx].SongItems.Add(ASongItem);
    Result := iListIdx;
  end
  else
  begin
    TaskList.AddTask(ASongItem, AMediaGainAction, AVolume);
    Result := TaskList.Count-1;
  end;
  Inc(FilesToProcessCount);
end;

procedure TfrmMp3GainMain.QueueFiles(AAction: TMediaGainAction; AVolume: Double; CheckTagInfoAfterwards: Boolean);
var
  i: Integer;
begin
  for i:=0 to lvFiles.Items.Count-1 do
  begin
    if not (mnuOptionsOnlySelectedItems.Checked and (not lvFiles.Items[i].Selected)) then
    begin
      TSongItem(lvFiles.Items[i].Data).HasData := false;
      TSongItem(lvFiles.Items[i].Data).HasAlbumData := false;
      AddTask(TSongItem(lvFiles.Items[i].Data), AAction, AVolume);
      if (CheckTagInfoAfterwards) or (TSongItem(lvFiles.Items[i].Data).MediaType=mtVorbis)
         and (not (AAction=mgaCheckTagInfo)) then
        AddTask(TSongItem(lvFiles.Items[i].Data), mgaCheckTagInfo, AVolume);
    end;
  end;

  if MediaGain.IsReady then
  begin
    ProgressBarGeneral.Position := 0;
    ProgressBar.Position := 0;
    ProcessQueue(Self);
  end;
end;

procedure TfrmMp3GainMain.UpdateView(AItem: TSongItem);
begin
  if not AItem.HasData then exit;
  with AItem.ListViewItem do
  begin
    if (AItem.MediaType=mtMP3) or (AItem.MediaType=mtAAC) then
    begin
      SubItems[SI_TRACKGAIN] := Format('%.1f',[RoundGainValue(AItem.Gain_Track)]);
      SubItems[SI_VOLUME] := Format('%.1f',[AItem.Volume_Track]);
      SubItems[SI_CLIPPING] := boolStr[AItem.Clipping];
    end
    else
    begin
      SubItems[SI_TRACKGAIN] := Format('(%.2f)',[AItem.Gain_Track]);
      SubItems[SI_VOLUME] := Format('%.2f',[AItem.Volume_Track]);
      SubItems[SI_CLIPPING] := '';
    end;
    if AItem.HasAlbumData then
    begin
      if (AItem.MediaType=mtMP3) or (AItem.MediaType=mtAAC) then
      begin
        SubItems[SI_ALBUMGAIN] := Format('%.1f',[RoundGainValue(AItem.Gain_Album)]);
        SubItems[SI_ALBUMVOLUME] := Format('%.1f',[AItem.Volume_Album]);
      end else
      begin
        SubItems[SI_ALBUMGAIN] := Format('(%.2f)',[AItem.Gain_Album]);
        SubItems[SI_ALBUMVOLUME] := '?';
      end;
    end;
  end;
end;

procedure TfrmMp3GainMain.ProcessQueue(Sender: TObject);
var
  i: Integer;
  n,k: Integer;
  freeze_ListView: Boolean=false;
begin
  if TaskList.Count>0 then
    freeze_ListView := TaskList[0].MediaGainAction=mgaCheckTagInfo;  // freezing ListView means speed improvements
  StatusBar.Panels[SB_ERROR].Text:='';
  LockControls(True, freeze_ListView);
  MediaGain.Cancel := False;
  while ((TaskList.Count >0) {and (not MediaGain.Cancel)}) do
  begin
    if MediaGain.Cancel then break;
    n := 0;
    for k:=0 to TaskList[n].SongItems.Count-1 do
    begin
      with TaskList[n].SongItems[k].ListViewItem do
      begin
        for i:=LVFILES_SUBITEMS_MIN to LVFILES_SUBITEMS_MAX do //0 to SubItems.Count-1 do
          SubItems[i] := '';
      end;
    end;
    MediaGain.SongItems.Assign(TaskList[n].SongItems);
    MediaGain.MediaGainAction := TaskList[n].MediaGainAction;
    if MediaGain.MediaGainAction=mgaConstantGain then
      MediaGain.VolumeGain := TaskList[n].Volume
    else
      MediaGain.TargetVolume := TaskList[n].Volume;
    MediaGain.Run;
    TaskList.DeleteTask(n);
  end;
  TaskList.Clear;
  
  if MediaGain.Cancel then
    StatusBar.Panels[SB_STATUS].Text := strStatus_Aborted
  else
    StatusBar.Panels[SB_STATUS].Text := strStatus_Finished;
  FilesToProcessCount := 0;
  FilesProcessedCount := 0;
  ProgressBarGeneral.Position := ProgressBarGeneral.Max;
  ProgressBar.Position := 0;
  LockControls(False, freeze_ListView);
end;

procedure TfrmMp3GainMain.AddFiles(SL: TStrings);
var
  i: Integer;
  SongItem: TSongItem;
begin
  for i:=0 to SL.Count-1 do
  begin
    SongItem := AddSongItem(SL[i]);
    if (SongItem = nil) then continue;
    if (MediaGainOptions.AutoReadAtFileAdd) then
    begin
      AddTask(SongItem, mgaCheckTagInfo, MediaGain.TargetVolume);
    end;
  end;
  UpdateFileCount;
  if MediaGainOptions.AutoReadAtFileAdd then
  begin
    if MediaGain.IsReady then
      ProcessQueue(Self);                    // start to read tags of new files
  end;
end;

procedure TfrmMp3GainMain.AddFolder(S: String; sublevels: Integer);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(S);
    AddFileAndDirectoryList(SL, sublevels);
  finally
    SL.Free;
  end;
end;

procedure TfrmMp3GainMain.AddFileAndDirectoryList(SL: TStringList; sublevels: Integer);
var
  i: Integer;
  aFileList: TStringList;
begin
  aFileList := TStringList.Create;
  try
    for i:=0 to SL.Count-1 do
    begin
      if DirectoryExists(SL[i]) then
        ListFiles(IncludeTrailingPathDelimiter(SL[i]),['mp3','ogg','mp4','m4a'], aFileList, sublevels) // array of Endungen
      else
        aFileList.Add(SL[i]);
    end;
    AddFiles(aFileList);
  finally
    aFileList.Free;
  end;
end;

procedure TfrmMp3GainMain.mnuFileAddFilesClick(Sender: TObject);
begin
  {$IFDEF LCLqt}OpenDialog.Files.Clear;{$ENDIF}
  if not OpenDialog.Execute then exit;
  AddFiles(OpenDialog.Files)
end;

procedure TfrmMp3GainMain.mnuFileAddFolderClick(Sender: TObject);
var
  sublevels: Byte;
begin
  if not SelectDirectoryDialog.Execute then exit;
  Application.ProcessMessages;
  if Sender=mnuFileAddFolderRecursive then
    sublevels := MediaGainOptions.SubLevelCount
  else
    sublevels := 0;
  AddFolder(SelectDirectoryDialog.FileName, sublevels);
end;

procedure TfrmMp3GainMain.mnuFileAddFolderRecursiveClick(Sender: TObject);
begin

end;

procedure TfrmMp3GainMain.mnuFileExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMp3GainMain.mnuFileClearAllFilesClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    DelSongItem(i);
  end;
end;

procedure TfrmMp3GainMain.mnuFileClearSelectedClick(Sender: TObject);
var
  i: Integer;
  iDel: array of Boolean;
begin
  SetLength(iDel, lvFiles.Items.Count);
  for i:= High(iDel) downto Low(iDel) do
    iDel[i] := lvFiles.Items[i].Selected;
  for i:= High(iDel) downto Low(iDel) do
     if iDel[i] then  lvFiles.Items.Delete(i);
  (*for i:=lvFiles.Items.Count-1 downto 0 do      // works not with Qt
  begin
    if (lvFiles.Items[i].Selected) then
    begin
      DelSongItem(i);
    end;
  end;*)
end;

procedure TfrmMp3GainMain.mnuFileSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    lvFiles.Items[i].Selected:=true;
  end;
end;

procedure TfrmMp3GainMain.mnuHelpHelpClick(Sender: TObject);
var
  strKeyWord: String;
  strTempDir: String;
begin
  // function OpenURL( AURL: String):Boolean;  // since 0.9.29
  strKeyWord := 'index.'+ strLang + '.html';
  if not FileExists(strBinDir+HELP_DIR+PathDelim+strKeyWord) then
    strKeyWord := 'index.html';
  strTempDir := GetCurrentDir;
  SetCurrentDir(strBinDir);
  ShowHelpOrErrorForKeyword('','HTML/'+strKeyWord);
  SetCurrentDir(strTempDir);
end;

procedure TfrmMp3GainMain.mnuFileSelectNoneClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    lvFiles.Items[i].Selected:=false;
  end;
end;

procedure TfrmMp3GainMain.mnuOptionsShowConsoleOutputClick(Sender: TObject);
begin
  frmMP3GainConsoleOutput.Show;
  MediaGain.ConsoleOutput := frmMP3GainConsoleOutput.memoData.Lines;
end;

procedure TfrmMp3GainMain.mnuSelectionInvertClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    lvFiles.Items[i].Selected:= not lvFiles.Items[i].Selected;
  end;
end;

procedure TfrmMp3GainMain.StatusBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  StatusBar.Hint:=StatusBar.Panels[StatusBar.GetPanelIndexAt(X,Y)].Text;
end;

procedure TfrmMp3GainMain.mnuHelpInfoClick(Sender: TObject);
begin
  frmMP3GainGUIInfo.ShowModal;
end;

procedure TfrmMp3GainMain.mnuModifyGainApplyAlbumClick(Sender: TObject);
begin
  QueueFiles(mgaAlbumGain, MediaGain.TargetVolume, true);
end;

procedure TfrmMp3GainMain.mnuModifyGainApplyConstantClick(Sender: TObject);
var
  r: Integer;
begin
  r := frmMP3GainConstant.ShowModal;
  if r=-1 then exit;
  QueueFiles(mgaConstantGain, Double(r)/10, true);
  edtVolumeChange(frmMP3GainConstant);                    // set volume back
end;

procedure TfrmMp3GainMain.mnuModifyGainApplyTrackClick(Sender: TObject);
begin
  QueueFiles(mgaTrackGain, MediaGain.TargetVolume, true);
end;

procedure TfrmMp3GainMain.mnuModifyGainUndoClick(Sender: TObject);
begin
  QueueFiles(mgaUndoChanges, MediaGain.TargetVolume, true);
end;

procedure TfrmMp3GainMain.mnuOptionsAdvancedClick(Sender: TObject);
begin
  frmMP3GainOptions.Show;
end;

procedure TfrmMp3GainMain.mnuOptionsDeleteTagInfosClick(Sender: TObject);
begin
  QueueFiles(mgaDeleteTagInfo, MediaGain.TargetVolume, false);
end;

procedure TfrmMp3GainMain.mnuOptionsOnlySelectedItemsClick(Sender: TObject);
begin
  btnOnlySelectedItems.Down := mnuOptionsOnlySelectedItems.Checked;
end;

procedure TfrmMp3GainMain.mnuOptionsReadTagInfoClick(Sender: TObject);
begin
  QueueFiles(mgaCheckTagInfo, MediaGain.TargetVolume, false);
end;

procedure TfrmMp3GainMain.mnuAnalysisTrackClick(Sender: TObject);
begin
  QueueFiles(mgaTrackAnalyze, MediaGain.TargetVolume, false);
end;

procedure TfrmMp3GainMain.mnuAnalysisAlbumClick(Sender: TObject);
begin
  QueueFiles(mgaAlbumAnalyze, MediaGain.TargetVolume, false);
end;

function TfrmMp3GainMain.AddSongItem(AName: String): TSongItem;
var
  SongItem: TSongItem;
  ListViewItem: TListItem;
  k: SmallInt;
  strExt: String;
begin
  Result := nil;
  if (not FileExists(AName)) then exit;
  ListViewItem := lvFiles.Items.Add;
  with ListViewItem do
  begin
    Caption := AName;
    SongItem := TSongItem.Create;
    Data := SongItem;
    SongItem.ListViewItem := ListViewItem;
    SongItem.HasAlbumData := false;
    SongItem.HasData := false;
    SongItem.FileName := AName;
    SongItem.ExtractedFileName := ExtractFileName(AName);
    SongItem.FilePath := ExtractFilePath(AName);
    SongItem.MediaType := mtUnknown;
    strExt := LowerCase(ExtractFileExt(AName));
    if strExt = '.mp3' then
      SongItem.MediaType := mtMP3
    else if (strExt = '.ogg') or (strExt = '.oga') then
      SongItem.MediaType := mtVorbis
    else if (strExt = '.mp4') or (strExt = '.m4a') then
      SongItem.MediaType := mtAAC;
    for k := 0 to SI_COUNT-1 do
    begin
      SongItem.ListViewItem.SubItems.Add('');
    end;
    SubItems[SI_FILE] := SongItem.ExtractedFileName;
    SubItems[SI_PATH] := SongItem.FilePath;
  end;
  Result := SongItem;
  if Print_Debug_Info then
    Writeln('added: ' + SongItem.FileName);
end;

procedure TfrmMp3GainMain.DelSongItem(AItemIndex: Integer);
begin
  TSongItem(lvFiles.Items[AItemIndex].Data).Free;
  lvFiles.Items.Delete(AItemIndex);
  UpdateFileCount;
end;

procedure TfrmMp3GainMain.btnAddFilesClick(Sender: TObject);
begin
  mnuFileAddFilesClick(Sender);
end;

procedure TfrmMp3GainMain.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  mnuFileClearAllFilesClick(Sender);
  btnCancelClick(Sender);
  frmMP3GainOptions.btnOKClick(Sender);
  frmMP3GainOptions.SaveSettings;
  MediaGain.Free;
  TaskList.Free;
end;

procedure TfrmMp3GainMain.Button1Click(Sender: TObject);
begin
end;

procedure TfrmMp3GainMain.FormCreate(Sender: TObject);
begin
  Print_Debug_Info := Paramstr(1)='-debug';
end;

procedure TfrmMp3GainMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i:Integer;
  S: String;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    for i:=0 to Length(FileNames)-1 do
    begin
      SL.Add(URLDecode(FileNames[i]));
    end;
    AddFileAndDirectoryList(SL, MediaGainOptions.SubLevelCount);
  finally
    SL.Free;
  end;
end;

procedure TfrmMp3GainMain.ListView1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TfrmMp3GainMain.btnAddFolderClick(Sender: TObject);
begin
  mnuFileAddFolderClick(mnuFileAddFolderRecursive);
end;

procedure TfrmMp3GainMain.btnAnalysisClick(Sender: TObject);
begin
  if pmnAnalysisTrack.Checked then
    mnuAnalysisTrackClick(Sender)
  else
    mnuAnalysisAlbumClick(Sender);
end;

procedure TfrmMp3GainMain.btnCancelClick(Sender: TObject);
var
  i: Integer;
begin
  MediaGain.Cancel := True;
  btnCancel.Enabled := False;
end;

procedure TfrmMp3GainMain.btnClearAllClick(Sender: TObject);
begin
  mnuFileClearAllFilesClick(Sender);
end;

procedure TfrmMp3GainMain.btnClearFilesClick(Sender: TObject);
begin
  mnuFileClearSelectedClick(Sender);
end;

procedure TfrmMp3GainMain.btnGainClick(Sender: TObject);
begin
  if pmnGainTrack.Checked then
    mnuModifyGainApplyTrackClick(Sender)
  else
    mnuModifyGainApplyAlbumClick(Sender);
end;

procedure TfrmMp3GainMain.btnOnlySelectedItemsClick(Sender: TObject);
begin
  mnuOptionsOnlySelectedItems.Checked := btnOnlySelectedItems.Down;
end;

procedure TfrmMp3GainMain.edtVolumeChange(Sender: TObject);
var
  value, r: Double;
  i, e: Integer;
begin
  Val(edtVolume.Text, value, e);
  if (e>0) or (value<1) then exit;
  if value>255 then value:=255;
  if value<5 then value:=5;
  MediaGain.TargetVolume := value;
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    with lvFiles.Items[i] do
    begin
      if (TSongItem(Data).HasData) and
         ((TSongItem(Data).MediaType=mtMP3) or (TSongItem(Data).MediaType=mtAAC)) then
      begin
        r := value-TSongItem(Data).Volume_Track;
        TSongItem(Data).Gain_Track := RoundGainValue(r);
        SubItems[SI_TRACKGAIN] := Format('%.1f',[RoundGainValue(r)]);
        // 3dB more means multiply MaxAmpitude with sqrt(2)
        if (TSongItem(Data).MaxAmplitude_Track*Power(2,(TSongItem(Data).Gain_Track)/6)>32768) then
          SubItems[SI_CLIPTRACK] := boolStr[true]
        else
          SubItems[SI_CLIPTRACK] := boolStr[false];
        if TSongItem(Data).HasAlbumData then
        begin
          r := value-TSongItem(Data).Volume_Album;
          TSongItem(Data).Gain_Album := RoundGainValue(r);
          SubItems[SI_ALBUMGAIN] := Format('%.1f',[RoundGainValue(r)]);
        end;
      end;
    end;
  end;
end;

procedure TfrmMp3GainMain.lvFilesColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  SortListView(lvFiles, Column.Index, Boolean(Column.Tag));
  Column.Tag := not Column.Tag;
end;

procedure TfrmMp3GainMain.lvFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=46) then mnuFileClearSelectedClick(Sender);
end;

procedure TfrmMp3GainMain.lvFilesMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
  a: Integer;
begin
  Item := lvFiles.GetItemAt(X,Y);
  if (Item=nil) then
  begin
    lvFiles.Hint := '';
    exit;
  end;
  a := Item.Index;
  if (a<0) or (a>lvFiles.Items.Count-1) then
  begin
    lvFiles.Hint := '';
    exit;
  end;
  Item := lvFiles.Items[a];
  lvFiles.Hint := TSongItem(Item.Data).FileName;
end;

procedure TfrmMp3GainMain.mnuAnalysisClearClick(Sender: TObject);
var
  i,k: Integer;
begin
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    with lvFiles.Items[i] do
    begin
      for k:=LVFILES_SUBITEMS_MIN to LVFILES_SUBITEMS_MAX do
        SubItems[k] := '';
    end;
  end;
end;

initialization
  {$I unitmain.lrs}

end.

