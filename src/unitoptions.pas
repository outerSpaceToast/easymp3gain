unit UnitOptions;

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

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmMp3GainOptions }

  TfrmMp3GainOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    chkUseTempFiles: TCheckBox;
    chkPreserveOriginalTimestamp: TCheckBox;
    chkIgnoreTags: TCheckBox;
    chkNoClipping: TCheckBox;
    chkAutoReadAtFileAdd: TCheckBox;
    edtVorbisGainBackend: TEdit;
    edtSublevelCount: TEdit;
    edtMP3GainBackend: TEdit;
    edtAACGainBackend: TEdit;
    grbBackends: TGroupBox;
    grbMP3Gain: TGroupBox;
    grbMiscellaneous: TGroupBox;
    lblFilenameDisplay: TLabel;
    lblSublevels: TLabel;
    lblVorbisGainBackend: TLabel;
    lblAACGainBackend: TLabel;
    lblMP3GainBackend: TLabel;
    rbFileAndPath: TRadioButton;
    rbFileWithPath: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure chkAutoReadAtFileAddChange(Sender: TObject);
    procedure chkIgnoreTagsChange(Sender: TObject);
  private
    { private declarations }
  public
    procedure SaveSettings;
    function LoadSettings: Boolean;
    procedure SettingsToControls;
    procedure SettingsFromControls;
    procedure SettingsToMainForm;
    { public declarations }
  end; 

var
  frmMp3GainOptions: TfrmMp3GainOptions;

implementation

uses UnitMediaGain, UnitMain;

{ TfrmMp3GainOptions }

procedure TfrmMp3GainOptions.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMp3GainOptions.btnOKClick(Sender: TObject);
begin
  SettingsFromControls;
  SettingsToMainForm;
  Close;
end;

procedure TfrmMp3GainOptions.chkAutoReadAtFileAddChange(Sender: TObject);
begin
  if chkAutoReadAtFileAdd.Checked then chkIgnoreTags.Checked := False;
end;

procedure TfrmMp3GainOptions.chkIgnoreTagsChange(Sender: TObject);
begin
  if chkIgnoreTags.Checked then chkAutoReadAtFileAdd.Checked := False;
end;

procedure TfrmMp3GainOptions.FormShow(Sender: TObject);
begin
  SettingsToControls;
end;

procedure TfrmMp3GainOptions.SettingsFromControls;
begin
  MediaGainOptions.IgnoreTags := chkIgnoreTags.Checked;
  MediaGainOptions.NoClipping := chkNoClipping.Checked;
  MediaGainOptions.AutoReadAtFileAdd := chkAutoReadAtFileAdd.Checked;
  MediaGainOptions.UseTempFiles := chkUseTempFiles.Checked;
  MediaGainOptions.PreserveOriginalTimestamp := chkPreserveOriginalTimestamp.Checked;
  MediaGainOptions.strMP3GainBackend := edtMP3GainBackend.Text;
  MediaGainOptions.strAACGainBackend := edtAACGainBackend.Text;
  MediaGainOptions.strVorbisGainBackend := edtVorbisGainBackend.Text;
  MediaGainOptions.AnalysisTypeAlbum := frmMp3GainMain.pmnAnalysisAlbum.Checked;
  MediaGainOptions.GainTypeAlbum := frmMp3GainMain.pmnGainAlbum.Checked;
  MediaGainOptions.SubLevelCount := StrToInt(edtSublevelCount.Text);
  MediaGainOptions.FileNameDisplay_FileAndPath := rbFileAndPath.Checked;
end;

procedure TfrmMp3GainOptions.SettingsToControls;
begin
  chkIgnoreTags.Checked := MediaGainOptions.IgnoreTags;
  chkNoClipping.Checked := MediaGainOptions.NoClipping;
  chkAutoReadAtFileAdd.Checked := MediaGainOptions.AutoReadAtFileAdd;
  chkPreserveOriginalTimestamp.Checked := MediaGainOptions.PreserveOriginalTimestamp;
  chkUseTempFiles.Checked := MediaGainOptions.UseTempFiles;
  edtMP3GainBackend.Text := MediaGainOptions.strMP3GainBackend;
  edtAACGainBackend.Text := MediaGainOptions.strAACGainBackend;
  edtVorbisGainBackend.Text := MediaGainOptions.strVorbisGainBackend;
  edtSubLevelCount.Text := IntToStr(MediaGainOptions.SubLevelCount);
  if MediaGainOptions.FileNameDisplay_FileAndPath then
  begin
    rbFileAndPath.Checked := True
  end
  else
  begin
    rbFileWithPath.Checked := True;
  end;
end;

procedure TfrmMp3GainOptions.SettingsToMainForm;
begin
  if MediaGainOptions.FileNameDisplay_FileAndPath then
  begin
    frmMp3GainMain.lvFiles.Columns[0].Visible := False;
    frmMp3GainMain.lvFiles.Columns[SI_FILE+1].Visible := True;
    frmMp3GainMain.lvFiles.Columns[SI_PATH+1].Visible := True;
  end
  else
  begin
    frmMp3GainMain.lvFiles.Columns[0].Visible := True;
    frmMp3GainMain.lvFiles.Columns[SI_FILE+1].Visible := False;
    frmMp3GainMain.lvFiles.Columns[SI_PATH+1].Visible := False;
  end;
end;

procedure TfrmMp3GainOptions.SaveSettings;
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  StringList.Clear;
  try
    StringList.Values['Version'] := APPLICATION_VERSION;
    StringList.Values['IgnoreTags']:=BoolToStr(MediaGainOptions.IgnoreTags);
    StringList.Values['NoClipping']:=BoolToStr(MediaGainOptions.NoClipping);
    StringList.Values['AutoReadAtFileAdd']:=BoolToStr(MediaGainOptions.AutoReadAtFileAdd);
    StringList.Values['UseTempFiles']:=BoolToStr(MediaGainOptions.UseTempFiles);
    StringList.Values['PreserveOriginalTimestamp']:=BoolToStr(MediaGainOptions.PreserveOriginalTimestamp);
    StringList.Values['MP3GainBackend']:=MediaGainOptions.strMP3GainBackend;
    StringList.Values['AACGainBackend']:=MediaGainOptions.strAACGainBackend;
    StringList.Values['VorbisGainBackend']:=MediaGainOptions.strVorbisGainBackend;
    StringList.Values['AnalysisTypeAlbum'] := BoolToStr(MediaGainOptions.AnalysisTypeAlbum);
    StringList.Values['GainTypeAlbum'] := BoolToStr(MediaGainOptions.GainTypeAlbum);
    StringList.Values['SubLevelCount'] := IntToStr(MediaGainOptions.SubLevelCount);
    StringList.Values['ToolBarImageListIndex']:=IntToStr(MediaGainOptions.ToolBarImageListIndex);
    StringList.Values['TargetVolume'] := FloatToStr(MediaGainOptions.TargetVolume^);
    StringList.Values['WindowHeight'] := IntToStr(frmMp3GainMain.Height);
    StringList.Values['WindowWidth'] := IntToStr(frmMp3GainMain.Width);
    StringList.Values['FilenameDisplay_FileAndPath'] := BoolToStr(MediaGainOptions.FileNameDisplay_FileAndPath);
    StringList.SaveToFile(strHomeDir+strConfigFileName);
  finally
    StringList.Free;
  end;
end;

function TfrmMp3GainOptions.LoadSettings: Boolean;
var
 StringList: TStringList;
 iWidth, iHeight: Integer;
begin
  try
    StringList := TStringList.Create;
    Result:=False;
    try
      StringList.LoadFromFile(strHomeDir+strConfigFileName);
      MediaGainOptions.IgnoreTags := StrToBool(StringList.Values['IgnoreTags']);
      MediaGainOptions.NoClipping := StrToBool(StringList.Values['NoClipping']);
      MediaGainOptions.AutoReadAtFileAdd := StrToBool(StringList.Values['AutoReadAtFileAdd']);
      MediaGainOptions.PreserveOriginalTimestamp := StrToBool(StringList.Values['PreserveOriginalTimestamp']);
      MediaGainOptions.UseTempFiles := StrToBool(StringList.Values['UseTempFiles']);
      MediaGainOptions.ToolBarImageListIndex := StrToInt(StringList.Values['ToolBarImageListIndex']);
      MediaGainOptions.strMP3GainBackend := StringList.Values['MP3GainBackend'];
      MediaGainOptions.strAACGainBackend := StringList.Values['AACGainBackend'];
      MediaGainOptions.strVorbisGainBackend := StringList.Values['VorbisGainBackend'];
      MediaGainOptions.TargetVolume^ := StrToFloat(StringList.Values['TargetVolume']);
      MediaGainOptions.AnalysisTypeAlbum := StrToBool(StringList.Values['AnalysisTypeAlbum']);
      MediaGainOptions.GainTypeAlbum := StrToBool(StringList.Values['GainTypeAlbum']);
      MediaGainOptions.SubLevelCount := StrToInt(StringList.Values['SubLevelCount']);
      MediaGainOptions.FileNameDisplay_FileAndPath := StrToBool(StringList.Values['FilenameDisplay_FileAndPath']);
      iHeight := StrToInt(StringList.Values['WindowHeight']);
      iWidth := StrToInt(StringList.Values['WindowWidth']);
      if (iHeight>20) then
        frmMp3GainMain.Height := iHeight;
      if (iWidth>20) then
        frmMp3GainMain.Width := iWidth;
      Result := StringList.Values['Version'] = APPLICATION_VERSION;
    finally
      StringList.Free;
      if MediaGainOptions.ToolBarImageListIndex=1 then
        frmMp3GainMain.ToolBar1.Images := frmMp3GainMain.ImageList1;
      if MediaGainOptions.ToolBarImageListIndex=2 then
        frmMp3GainMain.ToolBar1.Images := frmMp3GainMain.ImageList2;
      frmMp3GainMain.pmnAnalysisAlbum.Checked := MediaGainOptions.AnalysisTypeAlbum;
      frmMp3GainMain.pmnGainAlbum.Checked := MediaGainOptions.GainTypeAlbum;
      frmMp3GainMain.edtVolume.Text := FloatToStr(MediaGainOptions.TargetVolume^);
      if MediaGainOptions.SubLevelCount<0 then MediaGainOptions.SubLevelCount := 0;
    end;
  except
  end;
end;

initialization
  {$I unitoptions.lrs}

end.

