unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  process, pipes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    procedure Execute;
    procedure Synchronize(value: Integer);
    { public declarations }
  end; 

var
  Form1: TForm1; 
  BytesRead: Integer;
  ProcessOutput: String;
  FASongItemHasFinished:Boolean;
  FCurrentSongItem:Integer;
  FExitStatus: Integer;
  
const
  OnResultEvent=0;
  OnProgressEvent=1;
  OnStatusCodeEvent=2;
  OnFinished=3;

implementation

{ TForm1 }

procedure TForm1.Synchronize(value: Integer);
begin
  Memo1.Lines.Text:=Memo1.Lines.Text + ProcessOutput;
end;

procedure TForm1.Execute;

  function ReadFromPipeStream(AStream: TInputPipeStream; var AString: String): Integer;
  var
    M: TMemoryStream;
    BytesRead: Int64;
    n: Integer;
  begin
    M := TMemoryStream.Create;
    BytesRead := 0;
    try
      repeat
        M.SetSize(BytesRead + AStream.NumBytesAvailable);
        n := AStream.Read((M.Memory + BytesRead)^, AStream.NumBytesAvailable);
        Inc(BytesRead, n);
      until (n=0);
      if BytesRead>0 then
      begin
        Writeln(AString);
        SetLength(AString,BytesRead);
        M.Read(AString[1],BytesRead);
      end;
    finally
      M.Free;
      Result := BytesRead;
    end;
  end;

var
  P: TProcess;
{$IFDEF DEBUG_VERSION}
  X: TStringList;
{$ENDIF}
  e: Integer;
begin
//  FCurrentSongItem := 0;
{$IFDEF DEBUG_VERSION}
  X := TStringList.Create;
{$ENDIF}
  P := TProcess.Create(nil);
  try
    try
      P.CommandLine := 'mp3gain /home/thomas/mp32/m1000.mp3';
      P.Options := [poUsePipes,poNoConsole];
      P.Execute;
      Writeln('mp3gain started.');
      while (P.Running) do
      begin
        (*if Self.Cancel then
        begin
          Writeln('terminating mp3gain');
          P.Terminate(0);  // terminate mp3gain-process
        end;*)
        Writeln('Trying to read progress-output...');
        BytesRead := ReadFromPipeStream(P.Stderr, ProcessOutput);
        Writeln('Read ', BytesRead, ' Bytes');
        if BytesRead>0 then
        begin
          Writeln('Trying to synchronize progress...');
          Synchronize(OnProgressEvent);
          Writeln('Synchronized.');
        end;
        Sleep(100);
        if FASongItemHasFinished then   // A SongItem Finished, read the output
        begin
          Writeln('SongItem has finished: ', FCurrentSongItem);
          FASongItemHasFinished := false;
          BytesRead := ReadFromPipeStream(P.Output, ProcessOutput);
          if (BytesRead>0) then
          begin
            Writeln('Processing output from mp3gain: ');
            Writeln(' # ' + ProcessOutput);
            Synchronize(OnResultEvent);
          end;
        end;
      end;
      Writeln('mp3gain terminated...');
      Writeln('Trying to read output...');
      BytesRead := ReadFromPipeStream(P.Output, ProcessOutput);
      Writeln('Read ', BytesRead, ' Bytes.');
      Writeln(ProcessOutput);
      Synchronize(OnResultEvent);
    finally
      e := P.ExitStatus;
      FExitStatus := e;
      P.Free;
    end;
  except
    on e1:EProcess do
      FExitStatus := 127;
  end;
{$IFDEF DEBUG_VERSION}
  X.Free;
{$ENDIF}
  Synchronize(OnStatusCodeEvent);
//  Self.FreeOnTerminate := true;   // Thread-Bug in FPC2.2 True
  Synchronize(OnFinished);
//  Self.Terminate;   // Thread-Bug in FPC2.2 True
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  BytesRead:=0;
  ProcessOutput:='';
  FASongItemHasFinished := false;
  FCurrentSongItem:=0;
  Execute;
end;



initialization
  {$I unit1.lrs}

end.

