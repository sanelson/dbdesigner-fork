unit ZoomSel;

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
// Unit ZoomSel.pas
// ----------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Contains the ZoomSel from, a small form which is used by the MainForm
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QT;

type
  TZoomSelForm = class(TForm)
    ZoomPnl: TPanel;
    ZoomLBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FocusZoom(theZoomFac: double);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ZoomLBoxDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ZoomFac: double;
  end;

var
  ZoomSelForm: TZoomSelForm;

implementation

uses MainDM;

{$R *.xfm}

procedure TZoomSelForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);
  
  Height:=ZoomPnl.Height;
  Width:=ZoomPnl.Width;
end;

procedure TZoomSelForm.FocusZoom(theZoomFac: double);
var i: integer;
begin
  ZoomFac:=theZoomFac;

  for i:=0 to ZoomLBox.Items.Count-1 do
    if(ZoomLBox.Items[i]=FloatToStr(theZoomFac)+' %')then
      ZoomLBox.ItemIndex:=i;
end;

procedure TZoomSelForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_Escape)then
    ModalResult:=mrAbort;
end;

procedure TZoomSelForm.ZoomLBoxDblClick(Sender: TObject);
var oldDecSep: char;
begin
  oldDecSep:=DecimalSeparator;
  DecimalSeparator:=',';
  try
    ZoomFac:=StrToFloat(Copy(ZoomLBox.Items[ZoomLBox.ItemIndex], 1,
      Length(ZoomLBox.Items[ZoomLBox.ItemIndex])-2));
  finally
    DecimalSeparator:=oldDecSep;
  end;

  ModalResult:=mrOk;
end;

end.
