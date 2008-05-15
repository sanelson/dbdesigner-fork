unit EditorString;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of fabFORCE DBDesigner4.
// Copyright (C) 2002 Michael G. Zinner, www.fabFORCE.net
//
// DBDesigner4 is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// DBDesigner4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------
//
// Unit EditorString.pas
// ---------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Asks the user to type in or modify a string
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons, QExtCtrls, Qt;

type
  TEditorStringForm = class(TForm)
    PromtLbl: TLabel;
    InputPnl: TPanel;
    ValueEd: TEdit;
    OKBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure SetParams(ATitle, APromt, Value: string; SelectionStart: integer = 0; LimitChars: integer = 0);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ValueEdChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    LimitCharsMode: integer;
  public
    { Public-Deklarationen }
  end;

var
  EditorStringForm: TEditorStringForm;

implementation

uses MainDM;

{$R *.xfm}


procedure TEditorStringForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);
end;

procedure TEditorStringForm.OKBtnClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TEditorStringForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TEditorStringForm.SetParams(ATitle, APromt, Value: string; SelectionStart: integer = 0; LimitChars: integer = 0);
begin
  Caption:=ATitle;
  PromtLbl.Caption:=APromt;
  ValueEd.Text:=Value;
  ValueEd.SelStart:=SelectionStart;
  ValueEd.SelLength:=Length(value)-SelectionStart;

  LimitCharsMode:=LimitChars;

  InputPnl.Left:=PromtLbl.Left+PromtLbl.Width+2;

  Width:=InputPnl.Left+InputPnl.Width+PromtLbl.Left;
end;

procedure TEditorStringForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_Return)then
    OKBtnClick(self);
end;

procedure TEditorStringForm.ValueEdChange(Sender: TObject);
var i: integer;
begin
  if(LimitCharsMode=0)then
    Exit;

  i:=1;
  while(i<=Length(ValueEd.Text))do
  begin
    if(LimitCharsMode=1)then
    begin
      if(Not(
        ((ValueEd.Text[i]>='A')and(ValueEd.Text[i]<='Z'))or
        ((ValueEd.Text[i]>='a')and(ValueEd.Text[i]<='z'))or
        ((ValueEd.Text[i]>='0')and(ValueEd.Text[i]<='9'))or
        (ValueEd.Text[i]=' ')or(ValueEd.Text[i]='_')or
        (ValueEd.Text[i]='.')
        ))then
       ValueEd.Text:=Copy(ValueEd.Text, 1, i-1)+
        Copy(ValueEd.Text, i+1, Length(ValueEd.Text));
    end;

    if(LimitCharsMode=2)then
    begin
      if(Not(
        ((ValueEd.Text[i]>='A')and(ValueEd.Text[i]<='Z'))or
        ((ValueEd.Text[i]>='a')and(ValueEd.Text[i]<='z'))or
        ((ValueEd.Text[i]>='0')and(ValueEd.Text[i]<='9'))
        ))then
       ValueEd.Text:=Copy(ValueEd.Text, 1, i-1)+
        Copy(ValueEd.Text, i+1, Length(ValueEd.Text));
    end;

    inc(i);
  end;
end;

end.
