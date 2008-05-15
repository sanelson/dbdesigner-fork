unit EditorTableFieldParam;

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
// Unit EditorTableField.pas
// -------------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Is used by the table editor to edit table datatypes
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, EERModel, QMask, QExtCtrls, Qt, EERDM;

type
  TEditorTableFieldParamForm = class(TForm)
    MainPnl: TPanel;
    Panel2: TPanel;
    ParamMaskEdit: TMaskEdit;
    Label1: TLabel;
    DatatypeLbl: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SetData(theDatatype: TEERDatatype;
      DatatypeParams: string; winleft: integer = -1; wintop: integer = -1);
    function GetParams: string;
    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    winleftpos, wintoppos: integer;

    WindowTitleHeight,
    WindowXBorder: integer;
  public
    { Public declarations }
    Datatype: TEERDatatype;
    TableColumn: TEERColumn;
    ParentForm: TForm;
  end;

var
  EditorTableFieldParamForm: TEditorTableFieldParamForm;

implementation

uses MainDM, EditorTable;

{$R *.xfm}

procedure TEditorTableFieldParamForm.FormCreate(Sender: TObject);
begin
  WindowTitleHeight:=-22;
  WindowXBorder:=-4;

  {$IFDEF LINUX}
  BorderStyle:=fbsSingle;
  {$ENDIF}
end;

procedure TEditorTableFieldParamForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TEditorTableFieldParamForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //Action:=caFree;
end;

procedure TEditorTableFieldParamForm.SetData(theDatatype: TEERDatatype;
  DatatypeParams: string; winleft: integer = -1; wintop: integer = -1);
var params: string;
  i: integer;
begin
  Datatype:=theDatatype;

  winleftpos:=winleft;
  wintoppos:=wintop;

  DatatypeLbl.Caption:=Datatype.TypeName+'(';

  MainPnl.Width:=DatatypeLbl.Width+78;

  Width:=MainPnl.Width;
  Height:=MainPnl.Height;

  if(Not(theDatatype.EditParamsAsString))then
  begin
    //Build as many parameter as required
    params:='';
    for i:=0 to theDatatype.ParamCount-1 do
    begin
      {if(not(theDatatype.ParamRequired))then
        params:=params+'999'
      else
        params:=params+'099';}

      if(i<theDatatype.ParamCount-1)then
        params:=params+',';
    end;
    params:=params;

    ParamMaskEdit.EditMask:=params+';1;_';
  end
  else
    ParamMaskEdit.EditMask:='';


  //Fill the edit with the Parameters
  ParamMaskEdit.Text:=DMMain.ReplaceString(
    DMMain.ReplaceString(
      DatatypeParams, '(', ''),
    ')', '');

end;

procedure TEditorTableFieldParamForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if(Key=Key_Return)or(Key=Key_Enter)then
    ModalResult:=mrOK;

  if(Key=Key_Escape)then
    ModalResult:=mrAbort;
end;

function TEditorTableFieldParamForm.GetParams: string;
var Value: string;
begin
  Value:=DMMain.ReplaceString(ParamMaskEdit.Text, ' ', '');

  if(Copy(Value, Length(Value), 1)=',')then
    Value:=Copy(Value, 1, Length(Value)-1);

  Value:='('+Value+')';

  if(Value='()')then
    Value:='';

  GetParams:=Value;
end;

procedure TEditorTableFieldParamForm.FormShow(Sender: TObject);
begin
  if(winleftpos>-1)then
  begin
    Left:=winleftpos;
    Top:=wintoppos;
  end
  else
  begin
    Left:=(Screen.Width - Width) div 2;
    Top:=(Screen.Height - Height) div 2;
  end;
end;

procedure TEditorTableFieldParamForm.FormDeactivate(Sender: TObject);
begin
  if(Not(DMMain.IsFormStayingOnTop(self)))then
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));
end;

end.
