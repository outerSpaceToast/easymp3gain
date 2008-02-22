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

  { TfrmMp3GainGUIMain }

  TfrmMp3GainGUIMain = class(TForm)
    Bevel2: TBevel;
    edtVolume: TEdit;
    ImageList1: TImageList;
    lblTargetVolume: TLabel;
    lblTargetVolumeUnit: TLabel;
    lvFiles: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
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
    procedure CheckBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
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
    procedure mnuHelpInfoClick(Sender: TObject);
    procedure mnuModifyGainApplyAlbumClick(Sender: TObject);
    procedure mnuModifyGainApplyConstantClick(Sender: TObject);
    procedure mnuModifyGainApplyTrackClick(Sender: TObject);
    procedure mnuModifyGainUndoClick(Sender: TObject);
    procedure mnuOptionsDeleteTagInfosClick(Sender: TObject);
    procedure mnuOptionsOnlySelectedItemsClick(Sender: TObject);
    procedure mnuOptionsReadTagInfoClick(Sender: TObject);
  private
    MP3Gain: TMP3Gain;
    procedure WaitForMP3GainReady;
    procedure QueueFiles(AAction: TMP3GainAction; AVolume: Double; CheckTagInfoAfterwards: Boolean);
    procedure OnMP3GainReady(Sender: TObject);
    procedure AddSongItem(AName: String);
    procedure DelSongItem(AItemIndex: Integer);
    procedure LockControls(lock: Boolean);
    procedure LoadLanguageFile(AFile: String);
    procedure AddFiles(SL: TStringList);
    procedure UpdateFileCount;
    { private declarations }
  public
    procedure Init;
    procedure UpdateView(AItem: TSongItem);
    { public declarations }
  end; 
  
  const
   READ_BYTES = 2048;
   
   APPLICATION_NAME = 'easyMP3Gain';
   APPLICATION_VERSION = '0.2.0 alpha';
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
  frmMp3GainGUIMain: TfrmMp3GainGUIMain;

implementation

uses unitinfo, BaseUnix;

{ TfrmMp3GainGUIMain }

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

procedure TfrmMP3GainGUIMain.UpdateFileCount;
begin
  StatusBar.Panels[SB_FILECOUNT].Text := IntToStr(lvFiles.Items.Count) + ' '+ strFiles;
end;

procedure TfrmMP3GainGUIMain.Init;
var
  SL:TStringList;
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
      #10#10 + '(c) 2007, Thomas Dieffenbach';
  MP3Gain := TMP3Gain.Create;
  MP3Gain.OnRunFinished := @OnMP3GainReady;
  MP3Gain.TargetVolume := REF_VOLUME;
  //MP3Gain.SongItem := nil;
  TaskList := TMP3GainTaskList.Create;
  frmMP3GainGUIInfo.lblProgramName.Caption := APPLICATION_NAME+' '+APPLICATION_VERSION;
  frmMP3GainGUIMain.ImageList1.GetBitmap(3,frmMP3GainGUIInfo.Image1.Picture.Bitmap);
  SL := TStringList.Create;
  try
    ListFiles(Application.Location,'lng',SL,0);
    if SL.Count>0 then
      LoadLanguageFile(SL[0]);
  finally
    SL.Free;
  end;
end;

procedure TfrmMP3GainGUIMain.LoadLanguageFile(AFile: String);
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFile);
    if not (Copy(SL.Values['version'],1,5)='0.1.2') then
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
    mnuHelp.Caption := SL.Values['mnuHelp'];
    mnuHelpInfo.Caption := SL.Values['mnuHelpInfo'];
    
    btnAddFiles.Hint := mnuFileAddFiles.Caption;
    btnAddFolder.Hint := mnuFileAddFolder.Caption;
    btnAnalysis.Hint := mnuAnalysis.Caption;
    btnGain.Hint := mnuModifyGain.Caption;
    btnClearFiles.Hint := mnuFileClearSelected.Caption;
    btnClearAll.Hint := mnuFileClearAllFiles.Caption;
    btnOnlySelectedItems.Hint := mnuOptionsOnlySelectedItems.Caption;
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
    
    boolStr[FALSE] := SL.Values['clipping_no'];
    boolStr[TRUE] := SL.Values['clipping_yes'];
  finally
    SL.Free;
  end;
end;

procedure TfrmMP3GainGUIMain.LockControls(lock: Boolean);
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

procedure TfrmMp3GainGUIMain.QueueFiles(AAction: TMP3GainAction; AVolume: Double; CheckTagInfoAfterwards: Boolean);
var
  i: Integer;
  a: Integer;
begin
  LockControls(true);
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
    OnMP3GainReady(Self);
  end;
end;

procedure TfrmMp3GainGUIMain.WaitForMP3GainReady;
begin                             // Synchronize notwendig?
 while (not MP3Gain.IsReady) do ;
 Sleep(100);
end;

procedure TfrmMp3GainGUIMain.UpdateView(AItem: TSongItem);
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
  //MP3Gain.SongItem := nil;
end;

procedure TfrmMp3GainGUIMain.OnMP3GainReady(Sender: TObject);
var
  i: Integer;
  n,k: Integer;
begin
  //if (not (MP3Gain.SongItem=nil)) then UpdateView(MP3Gain.SongItem);
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
  end;
end;

procedure TfrmMp3GainGUIMain.AddFiles(SL: TStringList);
var
  i: Integer;
begin
  for i:=0 to SL.Count-1 do
  begin
    AddSongItem(SL[i]);
  end;
end;

procedure TfrmMp3GainGUIMain.mnuFileAddFilesClick(Sender: TObject);
var
  i: Integer;
begin
  if not OpenDialog.Execute then exit;
  for i:=0 to OpenDialog.Files.Count-1 do
    AddSongItem(OpenDialog.Files[i]);
  //AddFiles(OpenDialog.Files);
end;

procedure TfrmMp3GainGUIMain.mnuFileAddFolderClick(Sender: TObject);
var
  SL: TStringList;
  sublevels: Byte;
begin
  if not SelectDirectoryDialog.Execute then exit;
  Application.ProcessMessages;
  if Sender=mnuFileAddFolderRecursive then sublevels := 6 else sublevels := 0;
  SL := TStringList.Create;
  try
    ListFiles(IncludeTrailingPathDelimiter(SelectDirectoryDialog.FileName),'mp3', SL, sublevels);
    AddFiles(SL);
  finally
    SL.Free;
  end;
  //mnuOptionsReadTagInfoClick(Sender);
end;

procedure TfrmMp3GainGUIMain.mnuFileAddFolderRecursiveClick(Sender: TObject);
begin

end;

procedure TfrmMp3GainGUIMain.mnuFileExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMp3GainGUIMain.mnuFileClearAllFilesClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    DelSongItem(i);
  end;
end;

procedure TfrmMp3GainGUIMain.mnuFileClearSelectedClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    if (lvFiles.Items[i].Selected) then
      DelSongItem(i);
  end;
end;

procedure TfrmMp3GainGUIMain.mnuHelpInfoClick(Sender: TObject);
begin
  frmMP3GainGUIInfo.ShowModal;
end;

procedure TfrmMp3GainGUIMain.mnuModifyGainApplyAlbumClick(Sender: TObject);
begin
  QueueFiles(mgaAlbumGain, MP3Gain.TargetVolume, true);
end;

procedure TfrmMp3GainGUIMain.mnuModifyGainApplyConstantClick(Sender: TObject);
var
  r: Integer;
begin
  r := frmMP3GainConstant.ShowModal;
  if r=-1 then exit;
  QueueFiles(mgaConstantGain, Double(r)/10, true);
end;

procedure TfrmMp3GainGUIMain.mnuModifyGainApplyTrackClick(Sender: TObject);
begin
  QueueFiles(mgaTrackGain, MP3Gain.TargetVolume, true);
end;

procedure TfrmMp3GainGUIMain.mnuModifyGainUndoClick(Sender: TObject);
begin
  QueueFiles(mgaUndoChanges, MP3Gain.TargetVolume, true);
end;

procedure TfrmMp3GainGUIMain.mnuOptionsDeleteTagInfosClick(Sender: TObject);
begin
  QueueFiles(mgaDeleteTagInfo, MP3Gain.TargetVolume, false);
end;

procedure TfrmMp3GainGUIMain.mnuOptionsOnlySelectedItemsClick(Sender: TObject);
begin
  btnOnlySelectedItems.Down := mnuOptionsOnlySelectedItems.Checked;
end;

procedure TfrmMp3GainGUIMain.mnuOptionsReadTagInfoClick(Sender: TObject);
begin
  QueueFiles(mgaCheckTagInfo, MP3Gain.TargetVolume, false);
end;

procedure TfrmMp3GainGUIMain.mnuAnalysisTrackClick(Sender: TObject);
begin
  QueueFiles(mgaTrackAnalyze, MP3Gain.TargetVolume, false);
end;

procedure TfrmMp3GainGUIMain.mnuAnalysisAlbumClick(Sender: TObject);
begin
  QueueFiles(mgaAlbumAnalyze, MP3Gain.TargetVolume, false);
end;

procedure TfrmMp3GainGUIMain.AddSongItem(AName: String);
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
  LockControls(true);
  TaskList.AddTask(SongItem, mgaCheckTagInfo, MP3Gain.TargetVolume);
  if MP3Gain.IsReady then
    OnMP3GainReady(Self);
  UpdateFileCount;
end;

procedure TfrmMp3GainGUIMain.DelSongItem(AItemIndex: Integer);
begin
  TSongItem(lvFiles.Items[AItemIndex].Data).Free;
  lvFiles.Items.Delete(AItemIndex);
  UpdateFileCount;
end;

procedure TfrmMp3GainGUIMain.btnAddFilesClick(Sender: TObject);
begin
  mnuFileAddFilesClick(Sender);
end;

procedure TfrmMp3GainGUIMain.CheckBox1Change(Sender: TObject);
begin

end;

procedure TfrmMp3GainGUIMain.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  mnuFileClearAllFilesClick(Sender);
  //MP3Gain.FMP3GainProcess.Free;
  MP3Gain.Free;
end;

procedure TfrmMp3GainGUIMain.FormCreate(Sender: TObject);
begin
  Print_Debug_Info := Paramstr(1)='-debug';
end;

procedure TfrmMp3GainGUIMain.ListView1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TfrmMp3GainGUIMain.ToolBar1Click(Sender: TObject);
begin

end;

procedure TfrmMp3GainGUIMain.ToolButton5Click(Sender: TObject);
var
  X: TStringList;
begin
  {$IFDEF DEBUG_VERSION}
  X := TStringList.Create;
  try
    ListFiles('/home/thomas/.wine/','*',X,10);
  finally
    X.Free;
  end;
  {$ENDIF}
end;

procedure TfrmMp3GainGUIMain.btnAddFolderClick(Sender: TObject);
begin
  mnuFileAddFolderClick(mnuFileAddFolderRecursive);
end;

procedure TfrmMp3GainGUIMain.btnAnalysisClick(Sender: TObject);
begin
  if pmnAnalysisTrack.Checked then
    mnuAnalysisTrackClick(Sender)
  else
    mnuAnalysisAlbumClick(Sender);
end;

procedure TfrmMp3GainGUIMain.btnCancelClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=TaskList.Count-1 downto 0 do
    TaskList.DeleteTask(i);
  btnCancel.Enabled := False;
end;

procedure TfrmMp3GainGUIMain.btnClearAllClick(Sender: TObject);
begin
  mnuFileClearAllFilesClick(Sender);
end;

procedure TfrmMp3GainGUIMain.btnClearFilesClick(Sender: TObject);
begin
  mnuFileClearSelectedClick(Sender);
end;

procedure TfrmMp3GainGUIMain.btnGainClick(Sender: TObject);
begin
  if pmnGainTrack.Checked then
    mnuModifyGainApplyTrackClick(Sender)
  else
    mnuModifyGainApplyAlbumClick(Sender);
end;

procedure TfrmMp3GainGUIMain.btnOnlySelectedItemsClick(Sender: TObject);
begin
  mnuOptionsOnlySelectedItems.Checked := btnOnlySelectedItems.Down;
end;

procedure TfrmMp3GainGUIMain.edtVolumeChange(Sender: TObject);
var
  value, r: Double;
  i, e: Integer;
begin
  Val(edtVolume.Text, value, e);
  if (e>0) or (value<1) then exit;
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

procedure TfrmMp3GainGUIMain.lvFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=46) then mnuFileClearSelectedClick(Sender);
end;

procedure TfrmMp3GainGUIMain.lvFilesMouseMove(Sender: TObject;
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
  lvFiles.Hint := Item.Caption;
end;

procedure TfrmMp3GainGUIMain.mnuAnalysisClearClick(Sender: TObject);
var
  i,k: Integer;
begin
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    with lvFiles.Items[i] do
    begin
      //TSongItem(Data)^.HasData := false;
      //TSongItem(Data)^.HasAlbumData := false;
      for k:=0 to SubItems.Count-1 do
        SubItems[k] := '';
    end;
  end;
end;

initialization
  {$I unitmain.lrs}

end.

