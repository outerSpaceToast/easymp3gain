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

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, StdCtrls, Buttons, Process, UnitMP3Gain, UnitGainConstant, Math;

type

  { TfrmMp3GainGUIMain }

  TfrmMp3GainGUIMain = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    edtVolume: TEdit;
    grbVolume: TGroupBox;
    ImageList1: TImageList;
    lblTargetVolume: TLabel;
    lblTargetVolumeUnit: TLabel;
    lvFiles: TListView;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    pmnGainAlbum: TMenuItem;
    pmnGainTrack: TMenuItem;
    pmnAnalysisTrack: TMenuItem;
    pmnAnalysisAlbumAnalysis: TMenuItem;
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
    SelectDirectoryDialog: TSelectDirectoryDialog;
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    btnAddFiles: TToolButton;
    btnAddFolder: TToolButton;
    btnAnalysis: TToolButton;
    btnGain: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    btnClearFiles: TToolButton;
    btnClearAll: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ToolBar1Click(Sender: TObject);
    procedure btnAddFilesClick(Sender: TObject);
    procedure btnAddFolderClick(Sender: TObject);
    procedure btnAnalysisClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
    procedure btnClearFilesClick(Sender: TObject);
    procedure btnGainClick(Sender: TObject);
    procedure edtVolumeChange(Sender: TObject);
    procedure lvFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure lvFilesKeyPress(Sender: TObject; var Key: char);
    procedure lvFilesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mnuAnalysisAlbumClick(Sender: TObject);
    procedure mnuAnalysisClearClick(Sender: TObject);
    procedure mnuAnalysisTrackClick(Sender: TObject);
    procedure mnuFileAddFilesClick(Sender: TObject);
    procedure mnuFileAddFolderClick(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure mnuFileClearAllFilesClick(Sender: TObject);
    procedure mnuFileClearSelectedClick(Sender: TObject);
    procedure mnuHelpInfoClick(Sender: TObject);
    procedure mnuModifyGainApplyAlbumClick(Sender: TObject);
    procedure mnuModifyGainApplyConstantClick(Sender: TObject);
    procedure mnuModifyGainApplyTrackClick(Sender: TObject);
    procedure mnuModifyGainUndoClick(Sender: TObject);
    procedure mnuOptionsDeleteTagInfosClick(Sender: TObject);
    procedure mnuOptionsReadTagInfoClick(Sender: TObject);
  private
    MP3Gain: TMP3Gain;
    procedure WaitForMP3GainReady;
    procedure QueueFiles(AAction: TMP3GainAction; AVolume: Real);
    procedure OnMP3GainReady(Sender: TObject);
    procedure AddSongItem(AName: String);
    procedure DelSongItem(AItemIndex: Integer);
    procedure LockControls(lock: Boolean);
    procedure UpdateView(AItem: PSongItemInfo);
    procedure LoadLanguageFile(AFile: String);
    { private declarations }
  public
    procedure Init;
    { public declarations }
  end; 
  
  const
   READ_BYTES = 2048;
   
   APPLICATION_NAME = 'easyMP3Gain';
   APPLICATION_VERSION = '0.1 alpha';

 var
   S: TStringList;
   M: TMemoryStream;
   P: TProcess;
   n: LongInt;
   BytesRead: LongInt;

var
  frmMp3GainGUIMain: TfrmMp3GainGUIMain;

implementation

uses unitinfo;

{ TfrmMp3GainGUIMain }

Procedure ListFiles(const FilePath: String;ListBox:TStringList);
var
  SR: TSearchRec;
begin
  ListBox.Clear;
  if FindFirst(FilePath,faAnyFile,SR)=0 then
  begin
    if not (faDirectory and SR.Attr>0) then ListBox.Add(SR.Name);
    while FindNext(SR)=0 do
    begin
      if not (faDirectory and SR.Attr>0) then ListBox.Add(SR.Name);    // FindData.cFileName
    end;
  end;
  FindClose(SR);
end;

procedure TfrmMP3GainGUIMain.Init;
var
  SL:TStringList;
begin
  Self.Caption := APPLICATION_NAME;
  MP3Gain := TMP3Gain.Create;
  MP3Gain.OnRunFinished := @OnMP3GainReady;
  MP3Gain.TargetVolume := REF_VOLUME;
  MP3Gain.SongItem := nil;
  TaskList := TMP3GainTaskList.Create;
  frmMP3GainGUIInfo.lblProgramName.Text := APPLICATION_NAME+' '+APPLICATION_VERSION;
  frmMP3GainGUIMain.ImageList1.GetBitmap(3,frmMP3GainGUIInfo.Image1.Picture.Bitmap);
  SL := TStringList.Create;
  try
    ListFiles(Application.Location+'*.lng',SL);
    if SL.Count>0 then
      LoadLanguageFile(Application.Location+SL[0]);
  finally
    SL.Free;
  end;
end;

procedure TfrmMP3GainGUIMain.LoadLanguageFile(AFile: String);
var
  SL: TStringList;
  i: SmallInt;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFile);
    strStatus_Analyzing := SL.Values['Status_Analyzing'];
    strStatus_Gaining := SL.Values['Status_Gaining'];
    strStatus_Finished := SL.Values['Status_Finished'];
    strStatus_CheckingTagInfo := SL.Values['Status_CheckingTagInfo'];
    strStatus_DeletingTagInfo := SL.Values['Status_DeletingTagInfo'];
    strStatus_UndoingChanges := SL.Values['Status_UndoingChanges'];
    strStatus_ExitCode127 := SL.Values['Status_ExitCode127'];
    
    mnuFile.Caption := SL.Values['mnuFile'];
    mnuFileAddFolder.Caption := SL.Values['mnuFileAddFolder'];
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
    mnuHelp.Caption := SL.Values['mnuHelp'];
    mnuHelpInfo.Caption := SL.Values['mnuHelpInfo'];

    for i:= 0 to lvFiles.Columns.Count-1 do
      lvFiles.Columns[i].Caption := SL.Values['FileBoxColumn'+IntToStr(i)];
      
    lblTargetVolume.Caption := SL.Values['TargetVolume'];
    lblTargetVolumeUnit.Caption := SL.Values['TargetVolumeUnit'];
    lblTargetVolume.Width := lblTargetVolume.Canvas.TextWidth(lblTargetVolume.Caption);
    lblTargetVolumeUnit.Width := lblTargetVolume.Canvas.TextWidth(lblTargetVolumeUnit.Caption);
    edtVolume.Left := lblTargetVolume.Width + 10;
    lblTargetVolumeUnit.Left := edtVolume.Left + 50;
    grbVolume.Width := lblTargetVolumeUnit.Left + lblTargetVolumeUnit.Width + 20;

    frmMP3GainGUIInfo.btnClose.Caption := SL.Values['Close'];
    frmMP3GainGUIInfo.TabSheet1.Caption := SL.Values['InfoAbout'];
    frmMP3GainGUIInfo.TabSheet2.Caption := SL.Values['InfoAuthors'];
    frmMP3GainGUIInfo.TabSheet3.Caption := SL.Values['InfoTranslation'];
    frmMP3GainGUIInfo.TabSheet4.Caption := SL.Values['InfoLicence'];
    frmMP3GainGUIInfo.lblDescription.Caption := APPLICATION_NAME + ', ' +
      SL.Values['InfoDescription'] + #13#13 + '(c) 2007, Thomas Dieffenbach';
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
  i: SmallInt;
begin
  edtVolume.Enabled := not lock;
  ToolBar1.Enabled := not lock;
  for i:=0 to MainMenu1.Items.Count-1 do
  begin
    MainMenu1.Items[i].Enabled := not lock;
  end;
end;

procedure TfrmMp3GainGUIMain.QueueFiles(AAction: TMP3GainAction; AVolume: Double);
var
  i: SmallInt;
begin
  LockControls(true);
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    //if AAction=mgaConstantGain then AVolume := PSongItemInfo(lvFiles.Items[i].Data)^.Volume_Track + AVolume;
    //if (AAction=mgaConstantGain) and not PSongItemInfo(lvFiles.Items[i].Data)^.HasData then continue;
    TaskList.AddTask(lvFiles.Items[i].Data, AAction, AVolume);
    PSongItemInfo(lvFiles.Items[i].Data)^.HasData := false;
    PSongItemInfo(lvFiles.Items[i].Data)^.HasAlbumData := false;
  end;
  if MP3Gain.IsReady then
    OnMP3GainReady(Self);
end;

procedure TfrmMp3GainGUIMain.WaitForMP3GainReady;
begin                             // Synchronize notwendig?
 while (not MP3Gain.IsReady) do ;
 Sleep(100);
end;

procedure TfrmMp3GainGUIMain.UpdateView(AItem: PSongItemInfo);
begin
  if not AItem^.HasData then exit;
  with AItem^.ListViewItem do
  begin
    SubItems[SI_TRACKGAIN] := Format('%.1f',[RoundGainValue(AItem^.Gain_Track)]);
    SubItems[SI_VOLUME] := Format('%.1f',[AItem^.Volume_Track]);
    SubItems[SI_CLIPPING] := boolStr[AItem^.Clipping];
    if AItem^.HasAlbumData then
    begin
      SubItems[SI_ALBUMGAIN] := Format('%.1f',[RoundGainValue(AItem^.Gain_Album)]);
      SubItems[SI_ALBUMVOLUME] := Format('%.1f',[AItem^.Volume_Album]);
    end;
  end;
  MP3Gain.SongItem := nil;
end;

procedure TfrmMp3GainGUIMain.OnMP3GainReady(Sender: TObject);
var
  i: SmallInt;
  n: Integer;
begin
  if (not (MP3Gain.SongItem=nil)) then UpdateView(MP3Gain.SongItem);
  if TaskList.Count >0 then
  begin
    n := 0; //TaskList.Count-1;
    with TaskList[n]^.SongItem^.ListViewItem do
    begin
      for i:=0 to SubItems.Count-1 do
        SubItems[i] := '';
    end;
    MP3Gain.SongItem := TaskList[n]^.SongItem;
    MP3Gain.FileName := TaskList[n]^.FileName;
    MP3Gain.MP3GainAction := TaskList[n]^.MP3GainAction;
    if MP3Gain.MP3GainAction=mgaConstantGain then
      MP3Gain.VolumeGain := TaskList[n]^.Volume
    else
      MP3Gain.TargetVolume := TaskList[n]^.Volume;
    MP3Gain.Run;
    TaskList.DeleteTask(n);
  end
  else
  begin
    //edtVolumeChange(Sender);
    LockControls(false);
  end;
end;

procedure TfrmMp3GainGUIMain.mnuFileExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMp3GainGUIMain.mnuFileClearAllFilesClick(Sender: TObject);
var
  i: SmallInt;
begin
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    DelSongItem(i);
  end;
end;

procedure TfrmMp3GainGUIMain.mnuFileClearSelectedClick(Sender: TObject);
var
  i: SmallInt;
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
  QueueFiles(mgaAlbumGain, MP3Gain.TargetVolume);
  mnuOptionsReadTagInfoClick(Sender);
end;

procedure TfrmMp3GainGUIMain.mnuModifyGainApplyConstantClick(Sender: TObject);
var
  r: Integer;
begin
  r := frmMP3GainConstant.ShowModal;
  if r=-1 then exit;
  QueueFiles(mgaConstantGain, r/10);
  mnuOptionsReadTagInfoClick(Sender);
end;

procedure TfrmMp3GainGUIMain.mnuModifyGainApplyTrackClick(Sender: TObject);
begin
  QueueFiles(mgaTrackGain, MP3Gain.TargetVolume);
  mnuOptionsReadTagInfoClick(Sender);
end;

procedure TfrmMp3GainGUIMain.mnuModifyGainUndoClick(Sender: TObject);
begin
  QueueFiles(mgaUndoChanges, MP3Gain.TargetVolume);
end;

procedure TfrmMp3GainGUIMain.mnuOptionsDeleteTagInfosClick(Sender: TObject);
begin
  QueueFiles(mgaDeleteTagInfo, MP3Gain.TargetVolume);
end;

procedure TfrmMp3GainGUIMain.mnuOptionsReadTagInfoClick(Sender: TObject);
begin
  QueueFiles(mgaCheckTagInfo, MP3Gain.TargetVolume);
end;


procedure TfrmMp3GainGUIMain.AddSongItem(AName: String);
var
  SongItemInfo: PSongItemInfo;
  ListViewItem: TListItem;
  k: SmallInt;
begin
  ListViewItem := lvFiles.Items.Add;
  with ListViewItem do
  begin
    Caption := AName;
    GetMem(SongItemInfo, SizeOf(TSongItemInfo));
    Data := SongItemInfo;
    SongItemInfo^.ListViewItem := ListViewItem;
    SongItemInfo^.HasAlbumData := false;
    SongItemInfo^.HasData := false;
    for k := 0 to SI_COUNT do
    begin
      SongItemInfo^.ListViewItem.SubItems.Add('');
    end;
  end;
end;

procedure TfrmMp3GainGUIMain.DelSongItem(AItemIndex: Integer);
var
  SongItemInfo: PSongItemInfo;
  ListViewItem: TListItem;
begin
  FreeMem(PSongItemInfo(lvFiles.Items[AItemIndex].Data));
  lvFiles.Items.Delete(AItemIndex);
end;

procedure TfrmMp3GainGUIMain.mnuFileAddFilesClick(Sender: TObject);
var
  i: SmallInt;
begin
  if not OpenDialog.Execute then exit;
  for i:=0 to OpenDialog.Files.Count-1 do
    AddSongItem(OpenDialog.Files[i]);
  mnuOptionsReadTagInfoClick(Sender);
end;

procedure TfrmMp3GainGUIMain.mnuFileAddFolderClick(Sender: TObject);
var
  SL: TStringList;
  i: SmallInt;
begin
  if not SelectDirectoryDialog.Execute then exit;
  SL := TStringList.Create;
  try
    ListFiles(IncludeTrailingPathDelimiter(SelectDirectoryDialog.FileName)+'*.mp3', SL);
    for i:=0 to SL.Count-1 do
      AddSongItem(SL[i]);
  finally
    SL.Free;
  end;
  mnuOptionsReadTagInfoClick(Sender);
end;

procedure TfrmMp3GainGUIMain.btnAddFilesClick(Sender: TObject);
begin
  mnuFileAddFilesClick(Sender);
end;

procedure TfrmMp3GainGUIMain.Button1Click(Sender: TObject);
begin
  MP3Gain.FileName := Edit1.Text;
  MP3Gain.MP3GainAction := mgaDeleteTagInfo;
  MP3Gain.SongItem := nil;
  MP3Gain.Run;
end;


procedure TfrmMp3GainGUIMain.Button2Click(Sender: TObject);
begin
  MP3Gain.FileName := Edit1.Text;
  MP3Gain.MP3GainAction := mgaTrackAnalyze;
  MP3Gain.SongItem := nil;
  MP3Gain.Run;
end;

procedure TfrmMp3GainGUIMain.Button3Click(Sender: TObject);
begin
  MP3Gain.FileName := Edit1.Text;
  MP3Gain.MP3GainAction := mgaTrackGain;
  MP3Gain.TargetVolume := StrToFloat(edtVolume.Text);
  MP3Gain.SongItem := nil;
  MP3Gain.Run;
end;

procedure TfrmMp3GainGUIMain.Button4Click(Sender: TObject);
begin
  MP3Gain.FileName := Edit1.Text;
  MP3Gain.MP3GainAction := mgaCheckTagInfo;
  MP3Gain.SongItem := nil;
  MP3Gain.Run;
end;

procedure TfrmMp3GainGUIMain.CheckBox1Change(Sender: TObject);
begin
  Button1.Visible := CheckBox1.Checked;
  Button2.Visible := CheckBox1.Checked;
  Button3.Visible := CheckBox1.Checked;
  Button4.Visible := CheckBox1.Checked;
  Memo1.Visible := CheckBox1.Checked;
  Edit1.Visible := CheckBox1.Checked;
end;

procedure TfrmMp3GainGUIMain.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  mnuFileClearAllFilesClick(Sender);
  MP3Gain.Free;
end;

procedure TfrmMp3GainGUIMain.FormCreate(Sender: TObject);
begin

end;

procedure TfrmMp3GainGUIMain.ListView1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TfrmMp3GainGUIMain.ToolBar1Click(Sender: TObject);
begin

end;

procedure TfrmMp3GainGUIMain.btnAddFolderClick(Sender: TObject);
begin
  mnuFileAddFolderClick(Sender);
end;

procedure TfrmMp3GainGUIMain.btnAnalysisClick(Sender: TObject);
begin
  if pmnAnalysisTrack.Checked then
    mnuAnalysisTrackClick(Sender)
  else
    mnuAnalysisAlbumClick(Sender);
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

procedure TfrmMp3GainGUIMain.edtVolumeChange(Sender: TObject);
var
  value, r, d: Double;
  e: Integer;
  i: SmallInt;
  s: String;
begin
  Val(edtVolume.Text, value, e);
  if (e>0) or (value<1) then exit;
  MP3Gain.TargetVolume := value;
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    with lvFiles.Items[i] do
    begin
      if PSongItemInfo(Data)^.HasData then
      begin
        r := value-PSongItemInfo(Data)^.Volume_Track;
        PSongItemInfo(Data)^.Gain_Track := RoundGainValue(r);
        SubItems[SI_TRACKGAIN] := Format('%.1f',[RoundGainValue(r)]);
        if (PSongItemInfo(Data)^.MaxAmplitude_Track*Power(2,(PSongItemInfo(Data)^.Gain_Track)/6)>32768) then //(r+d)/4)
          SubItems[SI_CLIPTRACK] := boolStr[true]
        else
          SubItems[SI_CLIPTRACK] := boolStr[false];
        if PSongItemInfo(Data)^.HasAlbumData then
        begin
          r := value-PSongItemInfo(Data)^.Volume_Album;
          PSongItemInfo(Data)^.Gain_Album := RoundGainValue(r);
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

procedure TfrmMp3GainGUIMain.lvFilesKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TfrmMp3GainGUIMain.lvFilesMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
begin
  Item := lvFiles.GetItemAt(X,Y);
  if Item=nil then exit;
  lvFiles.Hint := Item.Caption;
end;

procedure TfrmMp3GainGUIMain.mnuAnalysisAlbumClick(Sender: TObject);
begin
  QueueFiles(mgaAlbumAnalyze, MP3Gain.TargetVolume);
end;

procedure TfrmMp3GainGUIMain.mnuAnalysisClearClick(Sender: TObject);
var
  i,k: SmallInt;
begin
  for i:=lvFiles.Items.Count-1 downto 0 do
  begin
    with lvFiles.Items[i] do
    begin
      //PSongItemInfo(Data)^.HasData := false;
      //PSongItemInfo(Data)^.HasAlbumData := false;
      for k:=0 to SubItems.Count-1 do
        SubItems[k] := '';
    end;
  end;
end;

procedure TfrmMp3GainGUIMain.mnuAnalysisTrackClick(Sender: TObject);
begin
  QueueFiles(mgaTrackAnalyze, MP3Gain.TargetVolume);
end;

initialization
  {$I unitmain.lrs}

end.

