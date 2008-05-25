unit callbackprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, pipes;

type

  TProcessChannel=(pcStdOut, pcStdError, pcFinished, pcError);

  TCallBackEvent=procedure(pcChannel: TProcessChannel; strData: String) of object;

  TCallbackProcess=class(TComponent)
  private
    FProcess: TProcess;
    FCallBackEvent: TCallBackEvent;
    FCommandLine: String;
    procedure CreateProcess;
    function ReadFromPipeStream(AStream: TInputPipeStream; var AString: String): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    property CallBackEvent: TCallBackEvent write FCallBackEvent;
  published
    property CommandLine: String read FCommandLine write FCommandLine;
  end;

implementation

constructor TCallbackProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCallbackProcess.Destroy;
begin
  inherited Destroy;
end;

function TCallbackProcess.ReadFromPipeStream(AStream: TInputPipeStream; var AString: String): Integer;
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
      SetLength(AString,BytesRead);
      M.Read(AString[1],BytesRead);
    end;
  finally
    M.Free;
    Result := BytesRead;
  end;
end;

procedure TCallbackProcess.Execute;
var
  strTemp: String;
begin
  try
    CreateProcess;
    FProcess.CommandLine := FCommandLine;
    FProcess.Execute;
    while (FProcess.Running) do
    begin
      Sleep(10);
      if ReadFromPipeStream(FProcess.Stderr,strTemp)>0 then
        FCallBackEvent(pcStdError, strTemp);
    end;
    if ReadFromPipeStream(FProcess.Stderr,strTemp)>0 then
      FCallBackEvent(pcStdError, strTemp);
    if ReadFromPipeStream(FProcess.Output,strTemp)>0 then
      FCallBackEvent(pcStdOut, strTemp);
    //FCallBackEvent(pcFinished, '');
  except
    on E:EProcess do
      FCallBackEvent(pcError, 'Process-Error');
    else
      FCallBackEvent(pcError, '');
  end;
  FCallBackEvent(pcFinished, '');
  FProcess.Free;
end;

procedure TCallbackProcess.CreateProcess;
begin
  FProcess := TProcess.Create(nil);
  FProcess.Options :=  [poUsePipes,poNoConsole];
end;

end.

