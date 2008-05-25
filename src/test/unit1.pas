unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  callbackprocess;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ProcessCallbackEvent(pcChannel: TProcessChannel; strData: String);
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 
  BytesRead: Integer;
  ProcessOutput: String;
  FASongItemHasFinished:Boolean;
  FCurrentSongItem:Integer;
  FExitStatus: Integer;
  ACallBackProcess: TCallbackProcess;
  
implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ACallBackProcess := TCallBackProcess.Create(Self);
  try
    ACallBackProcess.CallBackEvent := @ProcessCallbackEvent;
    ACallBackProcess.CommandLine := Edit1.Text;
    ACallBackProcess.Execute;
  finally
    ACallBackProcess.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.ProcessCallbackEvent(pcChannel: TProcessChannel; strData: String);
begin
  if pcChannel=pcStdError then
    Memo1.Lines.Add('stdError: '+strData);
  if pcChannel=pcStdOut then
    Memo1.Lines.Add('stdOut:   '+strData);
  if pcChannel=pcFinished then
    Memo1.Lines.Add('Finished.');
  if pcChannel=pcError then
    Memo1.Lines.Add('Error  '+ strData);
end;


initialization
  {$I unit1.lrs}

end.

