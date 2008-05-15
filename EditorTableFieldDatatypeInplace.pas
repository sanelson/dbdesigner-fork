unit EditorTableFieldDatatypeInplace;

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
// Unit EditorTableFieldParamInplace.pas
// -------------------------------------
// Version 1.0, 13.01.2003, Mike
// Description
//   Is used by the table editor to edit table fields
//
// Changes:
//   Version 1.0, 13.01.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, EERModel, QMask, QExtCtrls, Qt;

type
  TEditorTableFieldDatatypeInplaceEditor = class(TPanel)
    DatatypeCBox: TComboBox;

    constructor Create(Sender: TComponent); override;
    procedure DoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SetData(theEERModel: TEERModel; theTableEditor: TForm; theTableColumn: TEERColumn; theDatatype: TEERDatatype;
      DatatypeParams: string; winleft: integer = -1; wintop: integer = -1);
    function GetDatatype: TEERDatatype;
    function GetParams: string;
    procedure ApplyChanges(mode: integer = -1);
    procedure HideEdit;
    procedure DoTheExit(Sender: TObject);
    procedure DoTheCloseUp(Sender: TObject);
  private
    TableEditor: TForm;
    EERModel: TEERModel;
  public
    { Public declarations }
    Datatype: TEERDatatype;
    TableColumn: TEERColumn;
    ParentForm: TForm;
  end;

  const
    goUp=1;
    goDown=2;
    goLeft=3;
    goRight=4;
    goNowhereDontClose=5;

implementation

uses MainDM, EditorTable;

constructor TEditorTableFieldDatatypeInplaceEditor.Create(Sender: TComponent);
begin
  inherited Create(Sender);
  Visible:=False;

  Color:=$00E6E7FF;
  BorderStyle:=bsNone;
  BevelInner:=bvNone;
  BevelOuter:=bvNone;


  Width:=118;
  Height:=18;


  DatatypeCBox:=TComboBox.Create(Sender);
  DatatypeCBox.Parent:=self;
  DatatypeCBox.Left:=0;
  DatatypeCBox.Top:=-1;
  DatatypeCBox.Width:=118;
  DatatypeCBox.Height:=20;
  DatatypeCBox.Style:=csDropDown;
  DatatypeCBox.AutoComplete:=True;

  DatatypeCBox.OnKeyDown:=DoKeyDown;
  DatatypeCBox.OnExit:=DoTheExit;
{$IFDEF LINUX}
  DatatypeCBox.OnCloseUp:=DoTheCloseUp;
{$ENDIF}

  Parent:=TWidgetControl(Sender);
end;

procedure TEditorTableFieldDatatypeInplaceEditor.SetData(theEERModel: TEERModel; theTableEditor: TForm; theTableColumn: TEERColumn; theDatatype: TEERDatatype;
  DatatypeParams: string; winleft: integer = -1; wintop: integer = -1);
var
  i: integer;
begin
  EERModel:=theEERModel;
  Datatype:=theDatatype;
  TableEditor:=theTableEditor;
  TableColumn:=theTableColumn;

  Left:=winleft;
  Top:=wintop;

  //Add datatypes
  DatatypeCBox.Items.Clear;
  for i:=0 to EERModel.Datatypes.Count-1 do
    DatatypeCBox.Items.Add(TEERDatatype(EERModel.Datatypes[i]).TypeName);
  DatatypeCBox.Sort;

  DatatypeCBox.Text:=theDatatype.TypeName+DatatypeParams;

  if(theDatatype.ParamRequired)and(Pos('(', DatatypeCBox.Text)=0)then
    DatatypeCBox.Text:=DatatypeCBox.Text+'()';


  Show;
  BringToFront;
  try
    if(DatatypeCBox.CanFocus)then
      DatatypeCBox.SetFocus;
  except
  end;

  if(Pos('(', DatatypeCBox.Text)>0)then
  begin
    DatatypeCBox.SelStart:=Pos('(', DatatypeCBox.Text);
    DatatypeCBox.SelLength:=Pos(')', DatatypeCBox.Text)-Pos('(', DatatypeCBox.Text)-1;
  end;
end;

procedure TEditorTableFieldDatatypeInplaceEditor.DoKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if(Key=Key_Return)or(Key=Key_Enter)then
    ApplyChanges;

  if(Key=Key_Up)then
    ApplyChanges(goUp);

  if(Key=Key_Down)then
    ApplyChanges(goDown);

  if(Key=Key_Escape)then
    HideEdit;

  //If the user presses ( he want's to accept the autocompletion
  if(Key=Ord('('))then
  begin
    DatatypeCBox.Text:=DatatypeCBox.Text+'()';
    DatatypeCBox.SelStart:=Length(DatatypeCBox.Text)-1;
    DatatypeCBox.SelLength:=0;

    Key:=0;
  end;

  //When user presses a Alpha Key instead of a num option
  //he wants to enter a new datatype
  if(Key>=Ord('A'))and(Key<=Ord('Z'))and(DatatypeCBox.SelStart>1)then
  begin
    if(Pos('(', Copy(DatatypeCBox.Text, 1, DatatypeCBox.SelStart))>0)then
    begin
      if(DMMain.GetSubStringCountInString(Copy(DatatypeCBox.Text, 1, DatatypeCBox.SelStart), '''') mod 2=0)then
      begin
        DatatypeCBox.Text:='';
      end;
    end;
  end;

  if(DatatypeCBox.SelLength=Length(DatatypeCBox.Text))then
    DatatypeCBox.Text:='';
end;

function TEditorTableFieldDatatypeInplaceEditor.GetDatatype: TEERDatatype;
begin
  if(Pos('(', DatatypeCBox.Text)>0)then
    GetDatatype:=EERModel.GetDataTypeByName(Copy(DatatypeCBox.Text, 1, Pos('(', DatatypeCBox.Text)-1))
  else
    GetDatatype:=EERModel.GetDataTypeByName(DatatypeCBox.Text);
end;

function TEditorTableFieldDatatypeInplaceEditor.GetParams: string;
var Value: string;
begin
  if(Pos('(', DatatypeCBox.Text)>0)then
    Value:=Copy(DatatypeCBox.Text, Pos('(', DatatypeCBox.Text), Length(DatatypeCBox.Text))
  else
    Value:='';

  GetParams:=Value;
end;

procedure TEditorTableFieldDatatypeInplaceEditor.ApplyChanges(mode: integer = -1);
var theDatatype: TEERDatatype;
  i: Integer;
begin
  theDatatype:=GetDatatype;

  if(theDatatype<>nil)and
    (theDatatype<>Datatype)then
  begin
    TableColumn.idDatatype:=theDatatype.id;
    for i:=0 to 5 do
      TableColumn.OptionSelected[i]:=theDatatype.OptionDefaults[i];
  end;

  if(theDatatype<>nil)then
    TableColumn.DatatypeParams:=GetParams;

  HideEdit;

  //If this is the last datatype, create new row
  if(TEditorTableForm(TableEditor).ColumnGrid.Row=
    TEditorTableForm(TableEditor).ColumnGrid.RowCount-2)then
  begin
    TEditorTableForm(TableEditor).ColumnGrid.Row:=
      TEditorTableForm(TableEditor).ColumnGrid.RowCount-1;

    TEditorTableForm(TableEditor).ColumnGrid.Col:=1;

    TEditorTableForm(TableEditor).EditCellStr;
  end;

end;

procedure TEditorTableFieldDatatypeInplaceEditor.HideEdit;
begin
  TEditorTableForm(TableEditor).DoCellEdit:=False;
  TEditorTableForm(TableEditor).ColumnGrid.SetFocus;
  Hide;
end;

procedure TEditorTableFieldDatatypeInplaceEditor.DoTheExit(Sender: TObject);
begin
  ApplyChanges;
end;

procedure TEditorTableFieldDatatypeInplaceEditor.DoTheCloseUp(Sender: TObject);
begin
  Show;
  BringToFront;

  try
    if(DatatypeCBox.CanFocus)then
      DatatypeCBox.SetFocus;
  except
  end;
end;

end.
