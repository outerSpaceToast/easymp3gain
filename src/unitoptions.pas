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
 strHomeDir: String;
begin
  strHomeDir := IncludeTrailingPathDelimiter(getenvironmentvariable('HOME'));
  StringList := TStringList.Create;
  try
    StringList.Values['RewriteTags']:=BoolToInt(chkRewriteTags.Checked);
    StringList.Values['AutoReadAtStartup']:=BoolToStr(chkAutoReadAtStartup.Checked);
    StringList.SaveToFile(strHomeDir+strConfigFileName);
  finally
    StringList.Free;
  end;
  
  Close;
end;

procedure TfrmMp3GainOptions.FormShow(Sender: TObject);
begin
  // Load from Config-File
end;

initialization
  {$I unitoptions.lrs}

end.

