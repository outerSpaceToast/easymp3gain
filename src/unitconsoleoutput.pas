unit unitConsoleOutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmMP3GainConsoleOutput }

  TfrmMP3GainConsoleOutput = class(TForm)
    btnClose: TButton;
    memoData: TMemo;
    Panel1: TPanel;
    procedure FormResize(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMP3GainConsoleOutput: TfrmMP3GainConsoleOutput;

implementation

{ TfrmMP3GainConsoleOutput }

procedure TfrmMP3GainConsoleOutput.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMP3GainConsoleOutput.FormResize(Sender: TObject);
begin
  btnClose.Left := (ClientWidth-btnClose.Width) div 2;
end;

initialization
  {$I unitconsoleoutput.lrs}

end.

