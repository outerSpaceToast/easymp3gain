unit UnitInfo;

{
     Copyright (C) 2007 by Thomas Dieffenbach
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
  ComCtrls, Buttons, ExtCtrls, process;

type

  { TfrmMP3GainGUIInfo }

  TfrmMP3GainGUIInfo = class(TForm)
    btnClose: TButton;
    Image1: TImage;
    Label1: TLabel;
    lblProgramName: TLabel;
    lblDescription: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMP3GainGUIInfo: TfrmMP3GainGUIInfo;

implementation

uses Unitmain;

{ TfrmMP3GainGUIInfo }

procedure TfrmMP3GainGUIInfo.FormCreate(Sender: TObject);
begin

end;

procedure TfrmMP3GainGUIInfo.Label1Click(Sender: TObject);
begin
  // open browser
end;

procedure TfrmMP3GainGUIInfo.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
end;

procedure TfrmMP3GainGUIInfo.btnCloseClick(Sender: TObject);
begin
  Close;
end;

initialization
  {$I unitinfo.lrs}

end.

