unit unitConsoleOutput;

{
     Copyright (C) 2007-2010 by Thomas Dieffenbach
     giantics@gmx.de

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2 of the License, or
     (at your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the
     Free Software Foundation, Inc.,
     59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

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

