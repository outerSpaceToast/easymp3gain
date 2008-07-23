unit UnitGainConstant;

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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ComCtrls, StdCtrls, UnitMediaGain;

type

  { TfrmMP3GainConstant }

  TfrmMP3GainConstant = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lblGain: TLabel;
    tbGain: TTrackBar;
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure tbGainChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMP3GainConstant: TfrmMP3GainConstant;

implementation

{ TfrmMP3GainConstant }

procedure TfrmMP3GainConstant.btnCancelClick(Sender: TObject);
begin
  Self.ModalResult := -1;
end;

procedure TfrmMP3GainConstant.btnOKClick(Sender: TObject);
begin
  Self.ModalResult := Integer(Round(Double(tbGain.Position)/15)*15);
end;

procedure TfrmMP3GainConstant.FormShow(Sender: TObject);
begin
  tbGainChange(Self);
end;

procedure TfrmMP3GainConstant.tbGainChange(Sender: TObject);
begin
  lblGain.Caption := 'Gain: ' + FormatFloat('0.0',RoundGainValue(Double(tbGain.Position)/10)) + ' dB';
end;

initialization
  {$I unitgainconstant.lrs}

end.

