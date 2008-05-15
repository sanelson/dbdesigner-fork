unit EditorTableField;

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
// Version 1.0, 13.01.2003, Mike
// Description
//   Is used by the table editor to edit table fields
//
// Changes:
//   Version 1.0, 13.01.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, EERModel, QMask, QExtCtrls, Qt, QTypes, StrUtils, EditorTable;

type
  TEditorTableFieldEdit = class(TEdit)
    constructor Create(Sender: TComponent); override;
    procedure DoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SetData(TableEd: TEditorTableForm; theCol, theRow: integer; NewName: string = '');
    procedure ApplyChanges(mode: integer = -1);
    procedure DoTheExit(Sender: TObject);
    procedure HideEdit;
  private
    { Private declarations }
    //winleftpos, wintoppos: integer;
    TableEditor: TEditorTableForm;
    //IsClosing: Boolean;

    {WindowTitleHeight,
    WindowXBorder: integer;}
  public
    { Public declarations }
    Col, Row: integer;
  end;

  const
    goUp=1;
    goDown=2;
    goLeft=3;
    goRight=4;
    goNowhereDontClose=5;


implementation

uses MainDM;


constructor TEditorTableFieldEdit.Create(Sender: TComponent);
begin
  inherited Create(Sender);

  BorderStyle:=bsNone;
  Color:=$00E6E7FF;
  Visible:=False;

  OnKeyDown:=DoKeyDown;
  OnExit:=DoTheExit;

  Parent:=TWidgetControl(Sender);
end;


procedure TEditorTableFieldEdit.DoKeyDown(Sender: TObject;
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
end;

procedure TEditorTableFieldEdit.SetData(TableEd: TEditorTableForm; theCol, theRow: integer; NewName: string = '');
var theStr: string;
  i,  xpos: integer;
begin
  TableEditor:=TableEd;

  Col:=theCol;
  Row:=theRow;

  Text:='';

  xpos:=0;
  for i:=0 to Col-1 do
    xpos:=xpos+TableEditor.ColumnGrid.ColWidths[i];

  Left:=TableEditor.MidLeftPnl.Width+xpos+3;
  Top:=TableEditor.MidPnl.Top+
    19*(TableEditor.ColumnGrid.Row-TableEditor.ColumnGrid.TopRow+1)+4;
  Width:=TableEditor.ColumnGrid.ColWidths[Col]-2;
  Height:=16;

  if(NewName<>'')then
    Text:=NewName
  else
  begin
    case TableEditor.ColumnGrid.Col of
      1:
      begin
        //If user edits existing row
        if(TableEditor.ColumnGrid.Row<=TableEditor.EERTable.Columns.Count)then
          theStr:=TEERColumn(TableEditor.EERTable.Columns[TableEditor.ColumnGrid.Row-1]).ColName
        else
          theStr:='';

        //if this is the first column of the table, suggest IDtablename
        if(TableEditor.EERTable.Columns.Count=0)and(TableEditor.ColumnGrid.Row=1)and(theStr='')then
          theStr:='id'+TableEditor.TableNameEd.Text;
      end;
      7:
        theStr:=TEERColumn(TableEditor.EERTable.Columns[TableEditor.ColumnGrid.Row-1]).DefaultValue;
      8:
        theStr:=TEERColumn(TableEditor.EERTable.Columns[TableEditor.ColumnGrid.Row-1]).Comments;
    end;

    Text:=theStr;
  end;
end;

procedure TEditorTableFieldEdit.ApplyChanges(mode: integer = -1);
var theStr: string;
  theColumn: TEERColumn;
  i, j, moveBy: integer;
  theValue: string;
  NewRowWasAdded: Boolean;
begin
  NewRowWasAdded:=False;

  try
    if(CanFocus)then
      SetFocus
    else
      Exit;
  except
    Exit;
  end;

  theStr:=Text;

  if(Col=1)and(theStr<>'')then
  begin
    //Check Reserved Words
    if(TEERModel(TableEditor.EERModel).CheckReservedWord(theStr))then
      theValue:=theStr+'_2'
    else
      theValue:=theStr;

    //Check other Columns for same name
    j:=0;
    while(j=0)do
    begin
      j:=1;
      for i:=0 to TableEditor.EERTable.Columns.Count-1 do
        if(TEERColumn(TableEditor.EERTable.Columns[i]).ColName=theValue)and(i<>TableEditor.ColumnGrid.Row-1)then
        begin
          try
            theValue:=LeftStr(theValue, Length(theValue)-1)+IntToStr(StrToInt(RightStr(theValue, 1))+1)
          except
            theValue:=theValue+'_2';
          end;

          j:=0;
          break;
        end;
    end;

    //New Column
    if(TableEditor.ColumnGrid.Row>TableEditor.EERTable.Columns.Count)then
    begin
      //New(theColumn);
      theColumn:=TEERColumn.Create(TableEditor.EERTable);
      try
        theColumn.ColName:=theValue;
        theColumn.PrevColName:='';
        theColumn.Obj_id:=DMMain.GetNextGlobalID;
        theColumn.Pos:=TableEditor.EERTable.Columns.Count;
        theColumn.idDatatype:=TEERModel(TableEditor.EERModel).DefaultDataType;
        theColumn.DatatypeParams:='';
        theColumn.Width:=-1;
        theColumn.Prec:=-1;
        theColumn.PrimaryKey:=(Row=0);
        theColumn.NotNull:=(Row=0);
        theColumn.AutoInc:=(Row=0);
        theColumn.IsForeignKey:=False;

        if(TEERModel(TableEditor.EERModel).GetDataType(theColumn.idDatatype)=nil)then
          raise EInOutError.Create('The Datatype of the column cannot be found. '#13#10+
            'idDatatype: '+IntToStr(theColumn.idDatatype));

        //Get Option Defaults
        for i:=0 to TEERDatatype(TEERModel(TableEditor.EERModel).GetDataType(theColumn.idDatatype)).OptionCount-1 do
          theColumn.OptionSelected[i]:=
            TEERDatatype(TEERModel(TableEditor.EERModel).GetDataType(theColumn.idDatatype)).OptionDefaults[i];

        theColumn.DefaultValue:='';

        TableEditor.EERTable.Columns.Add(theColumn);

        //Add an empty row an go down there
        TableEditor.ColumnGrid.RowCount:=TableEditor.ColumnGrid.RowCount+1;
        //TableEditor.ColumnGrid.Row:=TableEditor.ColumnGrid.RowCount-1;
        NewRowWasAdded:=True;
      except
        theColumn.Free;
      end;
    end
    //Update existing Column
    else
    begin
      TEERColumn(TableEditor.EERTable.Columns[TableEditor.ColumnGrid.Row-1]).ColName:=theValue;
    end;
  end;

  //Default value
  if(Col=7)and(TableEditor.ColumnGrid.Row<=TableEditor.EERTable.Columns.Count)then
    TEERColumn(TableEditor.EERTable.Columns[TableEditor.ColumnGrid.Row-1]).DefaultValue:=theStr;

  //Comment
  if(Col=8)and(TableEditor.ColumnGrid.Row<=TableEditor.EERTable.Columns.Count)then
      TEERColumn(TableEditor.EERTable.Columns[TableEditor.ColumnGrid.Row-1]).Comments:=theStr;

  //Refresh Index
  TableEditor.CheckPrimaryIndex;
  TableEditor.RefreshCurrentIndex;

  if(mode=-1)then
  begin
    //Edit the new created row
    {if(Col=1)and(TableEditor.ColumnGrid.Row=TableEditor.ColumnGrid.RowCount-1)then
      //SetData(TableEditor, Col, TableEditor.ColumnGrid.Row)
    }
    if(NewRowWasAdded)then
    begin
      HideEdit;
      TableEditor.EditDatatype;
    end
    else
    begin
      HideEdit;
    end;
  end
  else if(mode=goUp)then
  begin
    if(Row>0)then
    begin
      moveBy:=1;
      //Catch OutOfIndex Error, when no Columns have been create so far
      if(Row-moveBy<TableEditor.EERTable.Columns.Count)then
        while(TEERColumn(TableEditor.EERTable.Columns[Row-moveBy]).IsForeignKey)and
          (Col=1)do
        begin
          inc(moveBy);

          if(Row-moveBy<0)then
            break;
        end;

      if(Row-moveBy<0)then
        moveBy:=0;

      TableEditor.ColumnGrid.Row:=Row-moveBy+1;
      SetData(TableEditor, Col, TableEditor.ColumnGrid.Row-1);
    end;
  end
  else if(mode=goDown)then
  begin
    if(Row<TableEditor.ColumnGrid.RowCount-2)then
    begin
      moveBy:=1;
      if(Row<TableEditor.ColumnGrid.RowCount-3)then
      begin
        while(TEERColumn(TableEditor.EERTable.Columns[Row+moveBy]).IsForeignKey)and
        (Col=1)do
        begin
          inc(moveBy);

          if(Row+moveBy>=TableEditor.ColumnGrid.RowCount-2)then
            break;
        end;
      end;

      TableEditor.ColumnGrid.Row:=Row+moveBy+1;
      SetData(TableEditor, Col, TableEditor.ColumnGrid.Row-1);
      {TableEditor.ColumnGrid.Row:=TableEditor.ColumnGrid.Row+1;
      SetData(TableEditor, Col, TableEditor.ColumnGrid.Row)}
    end;
  end;

  {if(theStr='')then
    HideEdit;}
end;

procedure TEditorTableFieldEdit.DoTheExit(Sender: TObject);
begin
  if(TableEditor.DoCellEdit)then
    ApplyChanges;
end;

procedure TEditorTableFieldEdit.HideEdit;
begin
  //Some workaround to make it work after the unoff. CLX fixes installed
  TableEditor.DoCellEdit:=False;
  TableEditor.ColumnGrid.SetFocus;
  Hide;


  TableEditor.SetFocusedControl(TableEditor.ColumnGrid);
  TableEditor.ColumnGrid.Invalidate;
  Application.ProcessMessages;

  {TableEditor.ColumnGrid.Invalidate;
  Application.ProcessMessages;
  TableEditor.PageControlTreeView.SetFocus;}

end;

end.
