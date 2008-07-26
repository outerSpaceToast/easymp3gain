unit UnitOptions;

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
    chkAutoReadAtFileAdd: TCheckBox;
    edtMP3GainBackend: TEdit;
    edtAACGainBackend: TEdit;
    lblAACGainBackend: TLabel;
    lblMP3GainBackend: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure chkAutoReadAtFileAddChange(Sender: TObject);
    procedure chkIgnoreTagsChange(Sender: TObject);
  private
    { private declarations }
  public
    procedure LoadSettings;
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
var
 StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    MediaGainOptions.IgnoreTags := chkIgnoreTags.Checked;
    StringList.Values['IgnoreTags']:=BoolToStr(MediaGainOptions.IgnoreTags);
    MediaGainOptions.AutoReadAtFileAdd := chkAutoReadAtFileAdd.Checked;
    StringList.Values['AutoReadAtFileAdd']:=BoolToStr(MediaGainOptions.AutoReadAtFileAdd);
    MediaGainOptions.UseTempFiles := chkUseTempFiles.Checked;
    StringList.Values['UseTempFiles']:=BoolToStr(MediaGainOptions.UseTempFiles);
    MediaGainOptions.PreserveOriginalTimestamp := chkPreserveOriginalTimestamp.Checked;
    StringList.Values['PreserveOriginalTimestamp']:=BoolToStr(MediaGainOptions.PreserveOriginalTimestamp);
    StringList.Values['ToolBarImageListIndex']:=IntToStr(MediaGainOptions.ToolBarImageListIndex);
    MediaGainOptions.strMP3GainBackend := edtMP3GainBackend.Text;
    StringList.Values['MP3GainBackend']:=MediaGainOptions.strMP3GainBackend;
    MediaGainOptions.strAACGainBackend := edtAACGainBackend.Text;
    StringList.Values['AACGainBackend']:=MediaGainOptions.strAACGainBackend;
    StringList.SaveToFile(strHomeDir+strConfigFileName);
  finally
    StringList.Free;
  end;
  
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
  chkIgnoreTags.Checked := MediaGainOptions.IgnoreTags;
  chkAutoReadAtFileAdd.Checked := MediaGainOptions.AutoReadAtFileAdd;
  chkPreserveOriginalTimestamp.Checked := MediaGainOptions.PreserveOriginalTimestamp;
  chkUseTempFiles.Checked := MediaGainOptions.UseTempFiles;
  edtMP3GainBackend.Text := MediaGainOptions.strMP3GainBackend;
  edtAACGainBackend.Text := MediaGainOptions.strAACGainBackend;
end;

procedure TfrmMp3GainOptions.LoadSettings;
var
 StringList: TStringList;
begin
  try
    StringList := TStringList.Create;
    try
      StringList.LoadFromFile(strHomeDir+strConfigFileName);
      MediaGainOptions.IgnoreTags := StrToBool(StringList.Values['IgnoreTags']);
      MediaGainOptions.AutoReadAtFileAdd := StrToBool(StringList.Values['AutoReadAtFileAdd']);
      MediaGainOptions.PreserveOriginalTimestamp := StrToBool(StringList.Values['PreserveOriginalTimestamp']);
      MediaGainOptions.UseTempFiles := StrToBool(StringList.Values['UseTempFiles']);
      MediaGainOptions.ToolBarImageListIndex := StrToInt(StringList.Values['ToolBarImageListIndex']);
      MediaGainOptions.strMP3GainBackend := StringList.Values['MP3GainBackend'];
      MediaGainOptions.strAACGainBackend := StringList.Values['AACGainBackend'];
    finally
      StringList.Free;
    end;
  except
  end;
end;

initialization
  {$I unitoptions.lrs}

end.

