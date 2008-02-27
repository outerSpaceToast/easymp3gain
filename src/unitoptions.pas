unit UnitOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmMp3GainOptions }

  TfrmMp3GainOptions = class(TForm)
    btnOK: TButton;
    btnClose: TButton;
    chkUseTempFiles: TCheckBox;
    chkPreserveOriginalTimestamp: TCheckBox;
    chkIgnoreTags: TCheckBox;
    chkAutoReadAtStartup: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
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

procedure TfrmMp3GainOptions.btnCloseClick(Sender: TObject);
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
    MP3GainOptions.AutoReadAtStartup := chkAutoReadAtStartup.Checked;
    StringList.Values['AutoReadAtStartup']:=BoolToStr(MP3GainOptions.AutoReadAtStartup);
    MP3GainOptions.UseTempFiles := chkUseTempFiles.Checked;
    StringList.Values['UseTempFiles']:=BoolToStr(MP3GainOptions.UseTempFiles);
    MP3GainOptions.PreserveOriginalTimestamp := chkPreserveOriginalTimestamp.Checked;
    StringList.Values['PreserveOriginalTimestamp']:=BoolToStr(MP3GainOptions.PreserveOriginalTimestamp);
    StringList.SaveToFile(strHomeDir+strConfigFileName);
  finally
    StringList.Free;
  end;
  
  Close;
end;

procedure TfrmMp3GainOptions.FormShow(Sender: TObject);
begin
  chkIgnoreTags.Checked := MP3GainOptions.IgnoreTags;
  chkAutoReadAtStartup.Checked := MP3GainOptions.AutoReadAtStartup;
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
      MP3GainOptions.AutoReadAtStartup := StrToBool(StringList.Values['AutoReadAtStartup']);
      MP3GainOptions.PreserveOriginalTimestamp := StrToBool(StringList.Values['PreserveOriginalTimestamp']);
      MP3GainOptions.UseTempFiles := StrToBool(StringList.Values['UseTempFiles']);
    finally
      StringList.Free;
    end;
  except
  end;
end;

initialization
  {$I unitoptions.lrs}

end.

