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

uses UnitMP3Gain;

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
    MP3GainOptions.IgnoreTags := chkIgnoreTags.Checked;
    StringList.Values['IgnoreTags']:=BoolToStr(MP3GainOptions.IgnoreTags);
    MP3GainOptions.AutoReadAtFileAdd := chkAutoReadAtFileAdd.Checked;
    StringList.Values['AutoReadAtFileAdd']:=BoolToStr(MP3GainOptions.AutoReadAtFileAdd);
    MP3GainOptions.UseTempFiles := chkUseTempFiles.Checked;
    StringList.Values['UseTempFiles']:=BoolToStr(MP3GainOptions.UseTempFiles);
    MP3GainOptions.PreserveOriginalTimestamp := chkPreserveOriginalTimestamp.Checked;
    StringList.Values['PreserveOriginalTimestamp']:=BoolToStr(MP3GainOptions.PreserveOriginalTimestamp);
    StringList.Values['ToolBarImageListIndex']:=IntToStr(MP3GainOptions.ToolBarImageListIndex);
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
  chkIgnoreTags.Checked := MP3GainOptions.IgnoreTags;
  chkAutoReadAtFileAdd.Checked := MP3GainOptions.AutoReadAtFileAdd;
  chkPreserveOriginalTimestamp.Checked := MP3GainOptions.PreserveOriginalTimestamp;
  chkUseTempFiles.Checked := MP3GainOptions.UseTempFiles;
end;

procedure TfrmMp3GainOptions.LoadSettings;
var
 StringList: TStringList;
begin
  try
    StringList := TStringList.Create;
    try
      StringList.LoadFromFile(strHomeDir+strConfigFileName);
      MP3GainOptions.IgnoreTags := StrToBool(StringList.Values['IgnoreTags']);
      MP3GainOptions.AutoReadAtFileAdd := StrToBool(StringList.Values['AutoReadAtFileAdd']);
      MP3GainOptions.PreserveOriginalTimestamp := StrToBool(StringList.Values['PreserveOriginalTimestamp']);
      MP3GainOptions.UseTempFiles := StrToBool(StringList.Values['UseTempFiles']);
      MP3GainOptions.ToolBarImageListIndex := StrToInt(StringList.Values['ToolBarImageListIndex']);
    finally
      StringList.Free;
    end;
  except
  end;
end;

initialization
  {$I unitoptions.lrs}

end.

