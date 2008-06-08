program process;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, Unit1, callbackprocess;

begin
  Application.Initialize;
  Application.CreateForm(TfrmMP3GainMain, frmMP3GainMain);
  Application.Run;
end.

