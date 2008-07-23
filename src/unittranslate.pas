unit unitTranslate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, menus, gettext, translations;
  
procedure TranslateAll;

implementation

uses unitMain, unitConsoleOutput, unitGainConstant, unitInfo;

procedure PassComponents(po: TPOFile; aComponent: TComponent);
var
  i,k: Integer;
  comp: TComponent;
begin
  for i:=0 to aComponent.ComponentCount-1 do
  begin
    comp := aComponent.Components[i];
    if comp is TControl then
    begin
      TControl(comp).Caption := po.Translate(TControl(comp).Caption, TControl(comp).Caption);
      TControl(comp).Hint    := po.Translate(TControl(comp).Hint   , TControl(comp).Hint);
    end;
    if comp is TMenuItem then
      TMenuItem(comp).Caption := po.Translate(TMenuItem(comp).Caption, TMenuItem(comp).Caption);
    (*if comp is TListView then
      for k:=0 to TListView(comp).Columns.Count-1 do
        TListView(comp).Columns[k].Caption :=
          po.Translate(TListView(comp).Columns[k].Caption, TListView(comp).Columns[k].Caption);*)
    PassComponents(po, aComponent.Components[i]);
  end;
end;

procedure TranslateAll;
var
  PODirectory, Lang, FallbackLang: String;
  po : TPOFile;
begin
  PODirectory := '../languages/';
  GetLanguageIDs(Lang, FallbackLang); // in unit gettext
  TranslateUnitResourceStrings('UnitMain', PODirectory + 'easymp3gain.%s.po', Lang, FallbackLang);
  TranslateUnitResourceStrings('UnitMediaGain', PODirectory + 'easymp3gain.%s.po', Lang, FallbackLang);
  po := TPOFile.Create(PODirectory+'easymp3gain.'+FallbackLang+'.po');
  try
    PassComponents(po,frmMP3GainMain);
    PassComponents(po,frmMP3GainConstant);
    PassComponents(po,frmMP3GainConsoleOutput);
    PassComponents(po,frmMP3GainGUIInfo);
  finally
    po.Free;
  end;
end;

end.

