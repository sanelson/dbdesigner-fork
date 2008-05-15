//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the SimpleWebFront-DBDesigner4-Plugin.
// Copyright (C) 2003 Bayer Ulrich
//
// The SimpleWebFront-DBDesigner4-Plugin is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// SimpleWebFront-DBDesigner4-Plugin is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------

unit DialogPopupSettings;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QButtons, QExtCtrls, Weboutput;

type
  TDialogPopupSettingsForm = class(TForm)
    OKBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    GroupBox1: TGroupBox;
    Label21: TLabel;
    WidthEd: TEdit;
    HeightEd: TEdit;
    Label22: TLabel;
    YEd: TEdit;
    XEd: TEdit;
    Label19: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    view : TView;
  public
    procedure SetView(view :TView);
  end;

var
  DialogPopupSettingsForm: TDialogPopupSettingsForm;

implementation

uses MainDM;

{$R *.xfm}

procedure TDialogPopupSettingsForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self); //set the correct font
end;

procedure TDialogPopupSettingsForm.OKBtnClick(Sender: TObject);
begin
  //update the view first
  view.GridPopupWidth := StrToInt(WidthEd.Text);
  view.GridPopupHeight := StrToInt(HeightEd.Text);
  view.GridPopupX := StrToInt(XEd.Text);
  view.GridPopupY := StrToInt(YEd.Text);

  ModalResult := mrOK;
end;

procedure TDialogPopupSettingsForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TDialogPopupSettingsForm.SetView(view :TView);
begin
  self.view := view;
  WidthEd.Text := IntToStr(view.GridPopupWidth);
  HeightEd.Text := IntToStr(view.GridPopupHeight);
  XEd.Text := IntToStr(view.GridPopupX);
  YEd.Text := IntToStr(view.GridPopupY);
end;

end.
