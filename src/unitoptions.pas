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
    chkRewriteTags: TCheckBox;
    chkAutoReadAtStartup: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { private declarations }
  public
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
    MP3GainOptions.RewriteTags := chkRewriteTags.Checked;
    StringList.Values['RewriteTags']:=BoolToStr(MP3GainOptions.RewriteTags);
    MP3GainOptions.AutoReadAtStartup := chkAutoReadAtStartup.Checked;
    StringList.Values['AutoReadAtStartup']:=BoolToStr(MP3GainOptions.AutoReadAtStartup);
    StringList.SaveToFile(strHomeDir+strConfigFileName);
  finally
    StringList.Free;
  end;
  
  Close;
end;

procedure TfrmMp3GainOptions.FormShow(Sender: TObject);
var
 StringList: TStringList;
begin
  try
    StringList := TStringList.Create;
    try
      StringList.LoadFromFile(strHomeDir+strConfigFileName);
      MP3GainOptions.RewriteTags := StrToBool(StringList.Values['RewriteTags']);
      chkRewriteTags.Checked := MP3GainOptions.RewriteTags;
      MP3GainOptions.AutoReadAtStartup := StrToBool(StringList.Values['AutoReadAtStartup']);
      chkAutoReadAtStartup.Checked := MP3GainOptions.AutoReadAtStartup;
    finally
      StringList.Free;
    end;
  except
  end;
end;

initialization
  {$I unitoptions.lrs}

end.

