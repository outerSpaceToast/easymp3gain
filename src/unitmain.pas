unit UnitMain;

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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, StdCtrls, Process, UnitMP3Gain, UnitGainConstant, Math, ExtCtrls,
  Buttons;

type

  { TfrmMp3GainMain }

  TfrmMp3GainMain = class(TForm)
    Bevel2: TBevel;
    edtVolume: TEdit;
    ImageList1: TImageList;
    ImageList2: TImageList;
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
    procedure ToolBar1Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
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
  private
    MP3Gain: TMP3Gain;
    procedure WaitForMP3GainReady;
    procedure QueueFiles(AAction: TMP3GainAction; AVolume: Double; CheckTagInfoAfterwards: Boolean);
    procedure ProcessQueue(Sender: TObject);
    procedure AddSongItem(AName: String);
    procedure DelSongItem(AItemIndex: Integer);
    procedure LockControls(lock: Boolean);
    procedure LoadLanguageFile(AFile: String);
    procedure AddFiles(SL: TStringList);
    procedure AddFolder(S: String; sublevels: Integer);
    procedure UpdateFileCount;
    procedure SortListView(Lv:TListView; Index:integer; Reverse: Boolean);
    { private declarations }
  public
    procedure Init;
    procedure UpdateView(AItem: TSongItem);
    { public declarations }
  end; 
  
  const
   READ_BYTES = 2048;
   
   APPLICATION_NAME = 'easyMP3Gain';
   APPLICATION_VERSION = '0.3.1 beta SVN-0074';
   APPLICATION_DESCRIPTION = 'graphical user interface for mp3gain';

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

var
  frmMp3GainMain: TfrmMp3GainMain;

implementation

uses unitInfo, unitConsoleOutput,  unitOptions {$IFDEF UNIX}, BaseUnix{$ENDIF};

{ TfrmMp3GainMain }

Procedure ListFiles(const FilePath: String; const Extension: String;
                    ListBox:TStringList; SubLevelMax: Byte);

  function IsRealDirectory(FilePath, Value: String): Boolean;
  var
    info: stat;
  begin
    Result := false;
    if not ((Value='.') or (Value='..') or (Value='')) then
    begin
      fplstat(FilePath+Value,@info);
      if info.nlink<>1 then Result := true;
    end;
  end;

var
  SR: TSearchRec;
  {$IFDEF DEBUG_VERSION}
  X: TStringList;
  {$ENDIF}
begin
  if FindFirst(FilePath+'*.'+Extension,faAnyFile,SR)=0 then    // Files
  repeat
    if not ((faDirectory and SR.Attr)=faDirectory) then
      ListBox.Add(FilePath + SR.Name);
  until FindNext(SR)<>0;
  FindClose(SR);
  if (SubLevelMax>0) and (FindFirst(FilePath+'*',faDirectory,SR)=0) then     // SubFolders
  repeat
    if ((faDirectory or faSymLink) and SR.Attr)=faDirectory then
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
  bmp: TBitmap;
begin
  Self.Caption := APPLICATION_NAME + ' ' + APPLICATION_VERSION;
  frmMP3GainGUIInfo.Caption := strAbout + ' ' + APPLICATION_NAME;
  strWidgetset := 'not specified';
  {$IFDEF LCLwin32}strWidgetset := 'Win32';{$ENDIF}
  {$IFDEF LCLgtk}strWidgetset := 'GTK';{$ENDIF}
  {$IFDEF LCLgtk2}strWidgetset := 'GTK2';{$ENDIF}
  {$IFDEF LCLqt}strWidgetset := 'QT';{$ENDIF}
  {$IFDEF LCLcarbon}strWidgetset := 'Carbon';{$ENDIF}
  frmMP3GainGUIInfo.lblDescription.Caption := APPLICATION_NAME + ', ' +
     APPLICATION_DESCRIPTION +#10 +'Widgetset: '+strWidgetset +
      #10#10 + '(c) 2007-2008, Thomas Dieffenbach';
  MP3Gain := TMP3Gain.Create;
  MP3Gain.TargetVolume := REF_VOLUME;
  MP3Gain.ConsoleOutput := frmMP3GainConsoleOutput.memoData.Lines;
  strHomeDir := IncludeTrailingPathDelimiter(getenvironmentvariable('HOME'));
  MP3GainOptions.UseTempFiles:=True;       // Pre-setting
  MP3GainOptions.AutoReadAtFileAdd:=True;  // Pre-setting
  MP3GainOptions.ToolBarImageListIndex:=1; // Pre-setting
  frmMP3GainOptions.LoadSettings;          // Load settings from config-file
  if MP3GainOptions.ToolBarImageListIndex=1 then
    ToolBar1.Images := ImageList1;
  if MP3GainOptions.ToolBarImageListIndex=2 then
    ToolBar1.Images := ImageList2;
  TaskList := TMP3GainTaskList.Create;
  frmMP3GainGUIInfo.lblProgramName.Caption := APPLICATION_NAME+' '+APPLICATION_VERSION;
  frmMp3GainMain.ImageList1.GetBitmap(8,frmMP3GainGUIInfo.Image1.Picture.Bitmap);
  SL := TStringList.Create;
  try
    ListFiles(Application.Location,'lng',SL,0);
    Writeln('Searching for language-file in '+Application.Location+'  found: ',SL.Count);
    if SL.Count>0 then
      LoadLanguageFile(SL[0]);
  finally
    SL.Free;
  end;
end;

procedure TfrmMp3GainMain.LoadLanguageFile(AFile: String);
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFile);
    if not (Copy(SL.Values['version'],1,5)='0.3.1') then
    begin
      MessageDlg('Wrong language-pack version.',mtError,[mbOK],0);
      Exit;
    end;
    strStatus_Analyzing := SL.Values['Status_Analyzing'];
    strStatus_Gaining := SL.Values['Status_Gaining'];
    strStatus_Finished := SL.Values['Status_Finished'];
    strStatus_CheckingTagInfo := SL.Values['Status_CheckingTagInfo'];
    strStatus_DeletingTagInfo := SL.Values['Status_DeletingTagInfo'];
    strStatus_UndoingChanges := SL.Values['Status_UndoingChanges'];
    strStatus_ExitCode127 := SL.Values['Status_ExitCode127'];
    strAbout := SL.Values['About'];
    strFiles := SL.Values['Files'];
    mnuFile.Caption := SL.Values['mnuFile'];
    mnuFileAddFolder.Caption := SL.Values['mnuFileAddFolder'];
    mnuFileAddFolderRecursive.Caption := SL.Values['mnuFileAddFolderRecursively'];
    mnuFileAddFiles.Caption := SL.Values['mnuFileAddFiles'];
    mnuFileClearSelected.Caption := SL.Values['mnuFileClearSelected'];
    mnuFileClearAllFiles.Caption := SL.Values['mnuFileClearAllFiles'];
    mnuFileExit.Caption := SL.Values['mnuFileExit'];
    mnuAnalysis.Caption := SL.Values['mnuAnalysis'];
    mnuAnalysisTrack.Caption := SL.Values['mnuAnalysisTrack'];
    mnuAnalysisAlbum.Caption := SL.Values['mnuAnalysisAlbum'];
    mnuAnalysisClear.Caption := SL.Values['mnuAnalysisClear'];
    mnuModifyGain.Caption := SL.Values['mnuModifyGain'];
    mnuModifyGainApplyTrack.Caption := SL.Values['mnuModifyGainApplyTrack'];
    mnuModifyGainApplyAlbum.Caption := SL.Values['mnuModifyGainApplyAlbum'];
    mnuModifyGainApplyConstant.Caption := SL.Values['mnuModifyGainApplyConstant'];
    mnuModifyGainUndo.Caption := SL.Values['mnuModifyGainUndo'];
    mnuOptions.Caption := SL.Values['mnuOptions'];
    mnuOptionsReadTagInfo.Caption := SL.Values['mnuOptionsReadTagInfo'];
    mnuOptionsDeleteTagInfos.Caption := SL.Values['mnuOptionsDeleteTagInfos'];
    mnuOptionsOnlySelectedItems.Caption := SL.Values['mnuOptionsOnlySelectedItems'];
    mnuOptionsAdvanced.Caption := SL.Values['mnuOptionsAdvanced'];
    mnuHelp.Caption := SL.Values['mnuHelp'];
    mnuHelpInfo.Caption := SL.Values['mnuHelpInfo'];
    
    btnAddFiles.Hint := mnuFileAddFiles.Caption;
    btnAddFolder.Hint := mnuFileAddFolder.Caption;
    btnAnalysis.Hint := mnuAnalysis.Caption;
    btnGain.Hint := mnuModifyGain.Caption;
    btnClearFiles.Hint := mnuFileClearSelected.Caption;
    btnClearAll.Hint := mnuFileClearAllFiles.Caption;
    btnOnlySelectedItems.Hint := mnuOptionsOnlySelectedItems.Caption;
    btnCancel.Hint := SL.Values['CancelHint'];
    pmnAnalysisTrack.Caption := mnuAnalysisTrack.Caption;
    pmnAnalysisAlbum.Caption := mnuAnalysisAlbum.Caption;
    pmnGainTrack.Caption := mnuModifyGainApplyTrack.Caption;
    pmnGainAlbum.Caption := mnuModifyGainApplyAlbum.Caption;

    for i:= 0 to lvFiles.Columns.Count-1 do
      lvFiles.Columns[i].Caption := SL.Values['FileBoxColumn'+IntToStr(i)];
      
    lblTargetVolume.Caption := SL.Values['TargetVolume'];
    lblTargetVolumeUnit.Caption := SL.Values['TargetVolumeUnit'];
    lblTargetVolume.Width := lblTargetVolume.Canvas.TextWidth(lblTargetVolume.Caption);
    lblTargetVolumeUnit.Width := lblTargetVolume.Canvas.TextWidth(lblTargetVolumeUnit.Caption);
    edtVolume.Left := lblTargetVolume.Width + 10;
    lblTargetVolumeUnit.Left := edtVolume.Left + 50;
    pnlVolume.Width := lblTargetVolumeUnit.Left + lblTargetVolumeUnit.Width + 20;

    frmMP3GainGUIInfo.btnClose.Caption := SL.Values['Close'];
    frmMP3GainGUIInfo.tbsAbout.Caption := SL.Values['InfoAbout'];
    frmMP3GainGUIInfo.tbsAuthors.Caption := SL.Values['InfoAuthors'];
    frmMP3GainGUIInfo.tbsTranslation.Caption := SL.Values['InfoTranslation'];
    frmMP3GainGUIInfo.tbsLicense.Caption := SL.Values['InfoLicence'];
    frmMP3GainGUIInfo.tbsThanksTo.Caption := SL.Values['InfoThanksTo'];
    frmMP3GainGUIInfo.lblDescription.Caption := APPLICATION_NAME + ', ' +
      SL.Values['InfoDescription'] +#10 +'Widgetset: '+strWidgetset +
      #10#10 + '(c) 2007, Thomas Dieffenbach';
    frmMP3GainGUIInfo.Caption := SL.Values['InfoAbout'] + ' ' + APPLICATION_NAME;

    frmMP3GainConstant.btnCancel.Caption := SL.Values['Cancel'];
    frmMP3GainConstant.btnOK.Caption := SL.Values['OK'];
    
    frmMP3GainOptions.Caption := SL.Values['Options_Caption'];
    frmMP3GainOptions.btnOK.Caption := SL.Values['OK'];
    frmMP3GainOptions.btnCancel.Caption := SL.Values['Cancel'];
    frmMP3GainOptions.chkAutoReadAtFileAdd.Caption := SL.Values['Options_chkAutoReadAtFileAdd'];;
    frmMP3GainOptions.chkIgnoreTags.Caption := SL.Values['Options_chkIgnoreTags'];;
    frmMP3GainOptions.chkPreserveOriginalTimestamp.Caption := SL.Values['Options_chkPreserveOriginalTimestamp'];;
    frmMP3GainOptions.chkUseTempFiles.Caption := SL.Values['Options_chkUseTempFiles'];;
    
    boolStr[FALSE] := SL.Values['clipping_no'];
    boolStr[TRUE] := SL.Values['clipping_yes'];
  finally
    SL.Free;
  end;
end;

procedure TfrmMp3GainMain.LockControls(lock: Boolean);
var
  i: Integer;
begin
  edtVolume.Enabled := not lock;
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

procedure TfrmMp3GainMain.QueueFiles(AAction: TMP3GainAction; AVolume: Double; CheckTagInfoAfterwards: Boolean);
var
  i: Integer;
  a: Integer;
begin
  //LockControls(true);
  if AAction=mgaAlbumAnalyze then
  begin
    a := TaskList.AddTask(nil, AAction, AVolume);
    for i:=0 to lvFiles.Items.Count-1 do
    begin
      if not (mnuOptionsOnlySelectedItems.Checked and (not lvFiles.Items[i].Selected)) then
      begin
        Inc(FilesToProcessCount);
        TaskList[a].SongItems.Add(lvFiles.Items[i].Data);
      end;
    end;
  end
  else // no AlbumAnalyze-Task
  begin
    for i:=0 to lvFiles.Items.Count-1 do
    begin
      if not (mnuOptionsOnlySelectedItems.Checked and (not lvFiles.Items[i].Selected)) then
      begin
        Inc(FilesToProcessCount);
        TaskList.AddTask(lvFiles.Items[i].Data, AAction, AVolume);
        if CheckTagInfoAfterwards then
        begin
          Inc(FilesToProcessCount);
          TaskList.AddTask(lvFiles.Items[i].Data, mgaCheckTagInfo, AVolume);
        end;
      end;
    end;
  end;
  for i:=0 to lvFiles.Items.Count-1 do
  begin
    if (mnuOptionsOnlySelectedItems.Checked and (not lvFiles.Items[i].Selected)) then continue;
    TSongItem(lvFiles.Items[i].Data).HasData := false;
    //TSongItem(lvFiles.Items[i].Data).HasAlbumData := false;
  end;
  if MP3Gain.IsReady then
  begin
    ProgressBarGeneral.Position := 0;
    ProgressBar.Position := 0;
    ProcessQueue(Self);
  end;
end;

procedure TfrmMp3GainMain.WaitForMP3GainReady;
begin                             // Synchronize notwendig?
 while (not MP3Gain.IsReady) do ;
 Sleep(100);
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
end;

procedure TfrmMp3GainMain.ProcessQueue(Sender: TObject);
var
  i: Integer;
  n,k:
  Integer;
begin
  LockControls(True);
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
  FilesToProcessCount := 0;
  FilesProcessedCount := 0;
  LockControls(False);
  
 (* //if (not (MP3Gain.SongItem=nil)) then UpdateView(MP3Gain.SongItem);
  if TaskList.Count >0 then
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
  end
  else
  begin
    LockControls(false);
    FilesToProcessCount := 0;
    FilesProcessedCount := 0;
  end;  *)
end;

procedure TfrmMp3GainMain.AddFiles(SL: TStringList);
var
  i: Integer;
begin
  for i:=0 to SL.Count-1 do
  begin
    AddSongItem(SL[i]);
  end;
end;

procedure TfrmMp3GainMain.AddFolder(S: String; sublevels: Integer);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    ListFiles(IncludeTrailingPathDelimiter(S),'mp3', SL, sublevels);
    AddFiles(SL);
  finally
    SL.Free;
  end;
end;

procedure TfrmMp3GainMain.mnuFileAddFilesClick(Sender: TObject);
var
  i: Integer;
begin
  if not OpenDialog.Execute then exit;
  for i:=0 to OpenDialog.Files.Count-1 do
    AddSongItem(OpenDialog.Files[i]);
  //AddFiles(OpenDialog.Files);
end;

procedure TfrmMp3GainMain.mnuFileAddFolderClick(Sender: TObject);
var
  sublevels: Byte;
begin
  if not SelectDirectoryDialog.Execute then exit;
  Application.ProcessMessages;
  if Sender=mnuFileAddFolderRecursive then sublevels := 6 else sublevels := 0;
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
begin
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    if (lvFiles.Items[i].Selected) then
      DelSongItem(i);
  end;
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
  MP3Gain.ConsoleOutput := frmMP3GainConsoleOutput.memoData.Lines;
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

procedure TfrmMp3GainMain.mnuHelpInfoClick(Sender: TObject);
begin
  frmMP3GainGUIInfo.ShowModal;
end;

procedure TfrmMp3GainMain.mnuModifyGainApplyAlbumClick(Sender: TObject);
begin
  QueueFiles(mgaAlbumGain, MP3Gain.TargetVolume, true);
end;

procedure TfrmMp3GainMain.mnuModifyGainApplyConstantClick(Sender: TObject);
var
  r: Integer;
begin
  r := frmMP3GainConstant.ShowModal;
  if r=-1 then exit;
  QueueFiles(mgaConstantGain, Double(r)/10, true);
end;

procedure TfrmMp3GainMain.mnuModifyGainApplyTrackClick(Sender: TObject);
begin
  QueueFiles(mgaTrackGain, MP3Gain.TargetVolume, true);
end;

procedure TfrmMp3GainMain.mnuModifyGainUndoClick(Sender: TObject);
begin
  QueueFiles(mgaUndoChanges, MP3Gain.TargetVolume, true);
end;

procedure TfrmMp3GainMain.mnuOptionsAdvancedClick(Sender: TObject);
begin
  frmMP3GainOptions.Show;
end;

procedure TfrmMp3GainMain.mnuOptionsDeleteTagInfosClick(Sender: TObject);
begin
  QueueFiles(mgaDeleteTagInfo, MP3Gain.TargetVolume, false);
end;

procedure TfrmMp3GainMain.mnuOptionsOnlySelectedItemsClick(Sender: TObject);
begin
  btnOnlySelectedItems.Down := mnuOptionsOnlySelectedItems.Checked;
end;

procedure TfrmMp3GainMain.mnuOptionsReadTagInfoClick(Sender: TObject);
begin
  QueueFiles(mgaCheckTagInfo, MP3Gain.TargetVolume, false);
end;

procedure TfrmMp3GainMain.mnuAnalysisTrackClick(Sender: TObject);
begin
  QueueFiles(mgaTrackAnalyze, MP3Gain.TargetVolume, false);
end;

procedure TfrmMp3GainMain.mnuAnalysisAlbumClick(Sender: TObject);
begin
  QueueFiles(mgaAlbumAnalyze, MP3Gain.TargetVolume, false);
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
      ProcessQueue(Self);
  end;
  UpdateFileCount;
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
  MP3Gain.Free;
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
begin
   for i:=0 to Length(FileNames)-1 do
   begin
     S := URLDecode(FileNames[i]);
     if DirectoryExists(S) then
       AddFolder(S,0)
     else
       AddSongItem(S);
   end;
end;

procedure TfrmMp3GainMain.ListView1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TfrmMp3GainMain.ToolBar1Click(Sender: TObject);
begin

end;

procedure TfrmMp3GainMain.ToolButton5Click(Sender: TObject);
var
  X: TStringList;
begin
  {$IFDEF DEBUG_VERSION}
  X := TStringList.Create;
  try
    ListFiles(strHomeDir + '.wine/','*',X,10);
  finally
    X.Free;
  end;
  {$ENDIF}
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
  for i:=TaskList.Count-1 downto 0 do
    TaskList.DeleteTask(i);
  if (MP3Gain.MP3GainAction in [mgaTrackAnalyze, mgaAlbumAnalyze]) then
    MP3Gain.Cancel;
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
  MP3Gain.TargetVolume := value;
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    with lvFiles.Items[i] do
    begin
      if TSongItem(Data).HasData then
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
  a := Item.Index-1;
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
      for k:=0 to SubItems.Count-1 do
        SubItems[k] := '';
    end;
  end;
end;

initialization
  {$I unitmain.lrs}

end.

