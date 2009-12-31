unit VorbisComment;

interface

uses Sysutils;//Windows;

type
  TComment = record
    Name: String;
    Value: String;
  end;
  TComments = array of TComment;
  TVorbisComment = class
  public
    function ReadComments(const aFileName: string;
      var Success: Boolean): TComments;
  end;

implementation

function TVorbisComment.ReadComments(const aFileName: string;
  var Success: Boolean): TComments;
//const
//  INVALID_FILE_ATTRIBUTES = DWORD(-1);
var
  F: File;
  i, j, k: Cardinal;
  l: Integer;
  Buf: array[0..249] of Byte;
  S: String;
begin
  //if GetFileAttributes(PChar(aFileName)) <> INVALID_FILE_ATTRIBUTES then
  j:=0;
  k:=0;
  begin
    AssignFile(F, aFileName);
    Reset(F, 1);

    BlockRead(F, Buf, 250);

    i := 0;
    while (i < 249 - 6) do
    begin
      if (Buf[i] = 3) and (Buf[Succ(i)] = $76) and (Buf[i+2] = $6F)
       and (Buf[i+3] = $72) and (Buf[i+4] = $62) and (Buf[i+5] = $69)
       and (Buf[i+6] = $73) then
      begin
        Inc(i, 7);
        Break;
      end;
      Inc(i);
    end;

    Seek(F, i);
    BlockRead(F, j, 4);
    Seek(F, i + j + 4);
    BlockRead(F, i, 4);
    if i > 1000 then
    begin
      Success := False;
      Exit;
    end;
    for j := 1 to i do
    begin
      BlockRead(F, k, 4);
      SetLength(S, k);
      BlockRead(F, S[1], k);
      SetLength(Result, Succ(Length(Result)));
      l := Pos('=', S);
      Result[Pred(Length(Result))].Name  := Copy(S, 1, Pred(l));
      Result[Pred(Length(Result))].Value := Copy(S, Succ(l), Length(S) - l);
    end;
    BlockRead(F, Success, 1);

    CloseFile(F);
  end; //else
    //Success := False;
end;

end.
