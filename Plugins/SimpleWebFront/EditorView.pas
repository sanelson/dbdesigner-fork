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
//
// Unit EditorView.pas
// ---------------
//
//  Contains the Dialog that is used to edit or create a view.
//
//
//----------------------------------------------------------------------------------------------------------------------

unit EditorView;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QButtons, QCheckLst, QGrids, QDBGrids, QExtCtrls,
  QComCtrls, Layer, Contnrs, QFileCtrls, Weboutput;

type

  TEditorViewForm = class(TForm)
    ViewLbl: TLabel;
    OKBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    WhereClauselLbl: TLabel;
    NameEd: TEdit;
    Label3: TLabel;
    TableInfo: TGroupBox;
    WhereClauseGroupBox: TGroupBox;
    TablesComboBox: TComboBox;
    Label2: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    JoinCheckListBox: TCheckListBox;
    NMTablesCheckListBox: TCheckListBox;
    ColListBox: TListBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ImgPnl: TPanel;
    IconImage: TImage;
    WhereClauseMemo: TMemo;
    Label8: TLabel;
    GroupBox1: TGroupBox;
    OrderColumnsComboBox: TComboBox;
    DirectionComboBox: TComboBox;
    Label9: TLabel;

  private
    Table : SW_Table;
    JoinTables, NMTables : TObjectList;
    Fview: TView;
    IconFilename : String;
    newView : Boolean;
    ProvideViewWasCalled :Boolean;
    CheckErrorString: String;
  published
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TablesComboBoxChange(Sender: TObject);
    procedure NMTablesCheckListBoxClickCheck(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JoinCheckListBoxClickCheck(Sender: TObject);
    procedure IconImageDblClick(Sender: TObject);

  private
    function CheckUserInput() : Boolean;    //returns true if the userinput is ok
    procedure ShowColsInListBox();
    procedure SetOrderByClause(orderBy :String);
    function GetOrderByClause() : String;

  public
    constructor Create(Owner :TComponent); override;
    procedure provideView(view :TView);
    property view :TView read FView;

    function ShowModal: Integer; override;
  end;

function tablenameExists(table :SW_Table; tableList : TObjectList) : Boolean;

var
  EditorViewForm: TEditorViewForm;

implementation

uses Main, StrUtils,Qt, SplitFns, DialogImageSelection, MainDM, StringConstants;

{$R *.xfm}


constructor TEditorViewForm.Create(Owner :TComponent);
var theTableNames: TStringList;
    thePic: TPicture;
    iconPath,exeFilepath :String;
begin
  inherited Create(Owner);

  //display all tables in the drop-down list
  theTableNames:= TStringList.Create;
  Model.GetAllTables(theTableNames);
  TablesComboBox.Items.Assign(theTableNames);
  theTableNames.Free;

  ProvideViewWasCalled := false;
  newView:= false;
  FView := nil;
  Table:= nil;
  JoinTables:= TObjectList.Create(false); //assume no ownership because we will use the objects of the model
  NMTables := TObjectList.Create(false);

  //load/show a default-picture
  thePic:=TPicture.Create;
  exeFilepath :=  IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  iconPath := exeFilepath+ DataDir+'Standard'+PathDelim+'icons'+PathDelim+'icons_01.png';
  thePic.LoadFromFile(iconPath);
  IconImage.Picture.Assign(thePic);

  self.IconFilename := 'icons_01.png';

end;

//has to be called before ShowModal
//if view is nil we will create a new View
procedure TEditorViewForm.ProvideView(view: TView);
var i , ind: Integer;
    tmpTable: SW_Table;
begin
  ProvideViewWasCalled := true;
  if (view = nil) then
  begin
    newView := true;
    exit;
  end;

  FView := view;

  NameEd.Text := view.Name;
  WhereClauseMemo.Text := view.WhereClause;
  Iconfilename := view.IconFilename;

  if (view.table <> nil) then
    TablesComboBox.ItemIndex:= TablesComboBox.Items.IndexOf(view.table.name);
  if (view.icon <> nil) then
    IconImage.Picture.Assign(view.Icon);

  //this will find all join and nm-tables for us and write them into
  //their respective listboxes
  TablesComboBoxChange(self);

  //no jointable or nmtable could be selected now
  assert(JoinTables.Count = 0);
  assert(NMTables.Count = 0);

  //For all JoinTables we have to make sure that the editor knows they are selected
  for i:=0  to View.JoinTables.Count-1 do
  begin
    tmpTable := SW_Table(View.JoinTables[i]);

    ind := JoinCheckListBox.Items.IndexOf( tmpTable.name );
    assert (ind <> -1);
    JoinCheckListBox.Checked[ind] := true;

    //we don't add the table itself, since the JoinTables-List is cleared and
    //rewritten on every tablechange-event
    JoinTables.Add(Model.FindTable(tmpTable.name));
  end;

  //select all NMTables of our view
  for i:=0  to View.NMTables.Count-1 do
  begin
    tmpTable := SW_Table(View.NMTables[i]);

    ind := NMTablesCheckListBox.Items.IndexOf( tmpTable.name );
    assert (ind <> -1);
    NMTablesCheckListBox.Checked[ind] := true;

    //we don't add the table itself, since the NMTables-List is cleared and
    //rewritten on every tablechange-event
    NMTables.Add(Model.FindTable(tmpTable.name));
  end;

  ShowColsInListbox;

  if (View.OrderBy <> '') then SetOrderByClause(view.OrderBy)
  else OrderColumnsComboBox.ItemIndex := 0;
end;

procedure TEditorViewForm.FormDestroy(Sender: TObject);
begin
  JoinTables.Free;
  NMTables.Free;
end;

procedure TEditorViewForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self); //set the correct font
end;

function TEditorViewForm.ShowModal: Integer;
begin
  assert(ProvideViewWasCalled);
  ShowModal := inherited ShowModal;
end;



function TEditorViewForm.GetOrderByClause() : String;
var orderBy,direction : String;
    orderByCol : SW_Column;
begin
  GetOrderByClause := '';
  
  assert(DirectionComboBox.ItemIndex <> -1);
  direction := DirectionComboBox.Items[DirectionComboBox.ItemIndex];
  if (direction = 'Ascending') then orderBy := 'a_'
  else if (direction = 'Descending') then orderBy := 'd_'
  else assert(false);

  assert(self.OrderColumnsComboBox.ItemIndex <> -1);
  if (OrderColumnsComboBox.Items[OrderColumnsComboBox.ItemIndex] <> '') then
  begin
    orderByCol := SW_Column(OrderColumnsComboBox.Items.Objects[OrderColumnsComboBox.ItemIndex]);
    GetOrderByClause := orderBy + orderByCol.Table.name +'.'+ orderByCol.name;
  end;
end;

procedure TEditorViewForm.SetOrderByClause(orderBy :String);
var direction,tablename,colname,signature : String;
    point_Pos,i : Integer;
    //found_entry : Boolean;
begin
  //first set the sorting direction
  direction := AnsiMidStr(orderBy,1,2);
  if direction='a_' then DirectionComboBox.ItemIndex := 0 //1 is ascending
  else if direction='d_' then DirectionComboBox.ItemIndex := 1 //1 is ascending
  else assert(false);
  Delete(orderBy,1,2);

  //now set the selected column
  point_pos := AnsiPos('.',orderBy);
  tablename := AnsiMidStr(orderBy , 1 ,point_pos-1);
  colname := AnsiMidStr(orderBy,point_pos+1,length(orderBy) );
  signature := colname +' ('+tablename+')';

  //found_entry := false;
  for i:=0 to OrderColumnsComboBox.Items.Count-1 do
  begin
    if (OrderColumnsComboBox.Items[i] = signature) then
    begin
      //found_entry := true;
      OrderColumnsComboBox.ItemIndex := i;
      break;
    end;
  end;
  //assert (found_entry = true);

end;

procedure TEditorViewForm.OKBtnClick(Sender: TObject);
var i:Integer;
begin
  if (NOT CheckUserInput()) then
  begin
    MessageDlg(CheckErrorString, mtInformation,[mbOk], 0, mbOk);
    exit;
  end;

  if (newView) then
  begin

    FView := TView.Create(NameEd.Text,WhereClauseMemo.Text, Table, JoinTables, NmTables,IconImage.Picture,IconFilename,GetOrderByClause());
  end
  else
  begin
    Fview.Name := NameEd.Text;
    Fview.WhereClause := WhereClauseMemo.Text;
    Fview.OrderBy := GetOrderByClause();
    if (FView.Table.GetEERId <> Table.GetEERId) then
    begin
      FView.Table.Free;
      FView.Table := Table.DeepCopy();
    end;
    FView.Icon.Assign(IconImage.Picture);
    FView.IconFilename := IconFilename;

    for i:=0 to FView.JoinTables.Count-1 do
    begin
      if (NOT tablenameExists(SW_Table(FView.JoinTables[i]), JoinTables)) then FView.JoinTables.Delete(i);
    end;

    for i:=0 to JoinTables.Count-1 do
    begin
      if (NOT tablenameExists(SW_Table(JoinTables[i]), FView.JoinTables)) then
        FView.JoinTables.Add(SW_Table(JoinTables[i]).DeepCopy() );
    end;

    for i:=0 to FView.NMTables.Count-1 do
    begin
      if (NOT tablenameExists(SW_Table(FView.NMTables[i]), NMTables)) then FView.NMTables.Delete(i);
    end;

    for i:=0 to NMTables.Count-1 do
    begin
      if (NOT tablenameExists(SW_Table(NMTables[i]), FView.NMTables)) then
        FView.NMTables.Add(SW_Table(NMTables[i]).DeepCopy() );
    end;

    FView.rebuildSortColumns();

  end;

  ModalResult := mrOK;

end;

procedure TEditorViewForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;




function TEditorViewForm.CheckUserInput() : Boolean;
var i: Integer;
begin
  CheckUserInput := true;

  //check the Viewname first
  if (NameEd.Text='') then
  begin
    CheckUserInput := false;
    CheckErrorString := CheckErrorString_NoViewname;
    Exit;
  end;

  //a viewname must not contain any chars of the following list: \ / : * ? „ < > |
  //since these are not allowed in filenames
  for i:=1 to length(NameEd.Text) do
  begin
    if ((NameEd.Text[i]= '\') or
       (NameEd.Text[i]= '/') or
       (NameEd.Text[i]= ':') or
       (NameEd.Text[i]= '*') or
       (NameEd.Text[i]= '?') or
       (NameEd.Text[i]= '"') or
       (NameEd.Text[i]= '<') or
       (NameEd.Text[i]= '>') or
       (NameEd.Text[i]= '''') or
       (NameEd.Text[i]= '|') ) then
    begin
      CheckUserInput := false;
      CheckErrorString := CheckErrorString_CharNotAllowed;
      Exit;
    end;
  end;


  //a base table has to get selected
  if (TablesComboBox.ItemIndex=-1) then
  begin
    CheckUserInput:= false;
    CheckErrorString := CheckErrorString_NoTable;
    Exit;
  end;

  //check the Where-clause
  //the first word should be where
  {if (NOT (WhereClauseMemo.Text='')) then
  begin
    str:= trim(self.WhereClauseMemo.Text);
    firstPart:= AnsiLeftStr(str,length('where'));
    if ( NOT (CompareText(firstPart, 'where') = 0) ) then
    begin
      //add a 'where' at the beginning
      WhereClauseMemo.Text := 'WHERE '+WhereClauseMemo.Text;
    end
    else
      WhereClauseMemo.Text := WhereClauseMemo.Text;
  end; }
  //mmh
  //TODO: we need a decent check here, the user should not be able to
  //use column names that don't even exist
end;

//checks if the table exists in the tableList;
//the table is said to exist if a table-object with the same name
//is a member of the list
function tablenameExists(table :SW_Table; tableList : TObjectList) : Boolean;
var i : Integer;
    found : Boolean;
    name : string;
begin
  name := table.name;
  found := false;

  for i:= 0 to tableList.Count-1 do
  begin
    if (name = SW_Table(tableList[i]).name) then
    begin
      found := true;
      break;
    end;
  end;

  tablenameExists := found;
end;






procedure TEditorViewForm.TablesComboBoxChange(Sender: TObject);
var Columns :TObjectList;
    col: SW_Column;
    i: Integer;
    stringList : TStringList;
begin
  assert(TablesComboBox.ItemIndex <> -1);
  Table := SW_Table(TablesComboBox.Items.Objects[TablesComboBox.ItemIndex]);
  Columns := Table.Columns;

  //clear the temporary data holders
  JoinTables.Clear();
  NMTables.Clear();

  //update the ColumnslistBox
  ColListBox.Items.Clear;
  OrderColumnsComboBox.Items.Clear;

  //no Order
  OrderColumnsComboBox.Items.AddObject('', nil);

  //show all columns in the listbox
  for i:=0 to Columns.Count-1 do
  begin
    col := SW_Column(Columns[i]);
    ColListBox.Items.AddObject(col.name+' ('+col.table.name+')', col);
    OrderColumnsComboBox.Items.AddObject(col.name+' ('+col.table.name+')', col);
  end;

  //update the join-dropDown Box
  JoinCheckListBox.Items.Assign(Table.relatedTables);

  //update the NMTables-combo Box
  stringList := TStringList.Create;
  Model.GetAllNMRelations(table.name, stringList);
  NMTablesCheckListBox.Items.Assign(stringList);
  stringList.Free;
end;


procedure TEditorViewForm.NMTablesCheckListBoxClickCheck(Sender: TObject);
var nmtableName :String;
begin
  nmtableName := NMTablesCheckListBox.Items[NMTablesCheckListBox.ItemIndex];

  if (NMTablesCheckListBox.Checked[NMTablesCheckListBox.ItemIndex]) then
    NMTables.Add( Model.FindTable(nmTableName) )
  else
    NMTables.Remove(Model.FindTable(nmTableName));

  ShowColsInListBox();
end;



procedure TEditorViewForm.JoinCheckListBoxClickCheck(Sender: TObject);
var joinTableName: String;
begin
  joinTableName:= JoinCheckListBox.Items[JoinCheckListBox.ItemIndex];

  if (JoinCheckListBox.Checked[JoinCheckListBox.ItemIndex]) then
    joinTables.Add(Model.FindTable(joinTableName) )
  else
    joinTables.Remove(Model.FindTable(joinTableName) );

  ShowColsInListBox();
end;



procedure TEditorViewForm.ShowColsInListBox();
var i,j: Integer;
  columnList : TObjectList;
  col: SW_Column;
begin
  //clear it and draw it again
  ColListBox.Items.Clear;
  OrderColumnsComboBox.Items.Clear;

  //no Order
  OrderColumnsComboBox.Items.AddObject('', nil);

  //show all columns of the table in the listbox and the combobox
  columnList := Table.Columns;
  for i:=0 to ColumnList.Count-1 do
  begin
    col := SW_Column(ColumnList[i]);
    ColListBox.Items.AddObject(col.name+' ('+col.table.name+')', col);
    OrderColumnsComboBox.Items.AddObject(col.name+' ('+col.table.name+')', col);
  end;

  //show all columns of the join-tables in the listbox and the combobox
  for i:=0 to JoinTables.Count-1 do
  begin
    ColumnList := SW_Table(JoinTables[i]).Columns;
    for j:=0 to ColumnList.Count-1 do
    begin
      col := SW_Column(ColumnList[j]);
      ColListBox.Items.AddObject(col.name+' ('+col.table.name+')', col);
      OrderColumnsComboBox.Items.AddObject(col.name+' ('+col.table.name+')', col);
    end;
  end;

  //show all columns of the nmtable in the listbox
  for i:=0 to NMTables.Count-1 do
  begin
    ColumnList := SW_Table(NMTables[i]).Columns;
    for j:=0 to ColumnList.Count-1 do
    begin
      col := SW_Column(ColumnList[j]);
      ColListBox.Items.AddObject(col.name+' ('+col.table.name+')', col);
    end;
  end;

end;



procedure TEditorViewForm.IconImageDblClick(Sender: TObject);
var theImageSelectionForm: TImageSelectionForm;
    dir : String;
begin
  theImageSelectionForm:=TImageSelectionForm.Create(self);
  try
    dir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
    theImageSelectionForm.SetData(dir+DataDir+'Standard'+PathDelim+'icons');
    if(theImageSelectionForm.ShowModal=mrOK)then
    begin
      IconImage.Picture.Assign(theImageSelectionForm.selectedPicture);
      iconFilename:= ExtractFileName(theImageSelectionForm.selectedFileName);
    end;
  finally
    theImageSelectionForm.Free;
  end;
end;

//****************************************************
//***Helpers for Forming an SQL-whereStatement********
//****************************************************

{
function TEditorViewForm.JoinStatement(table1 : Sw_Table; table2 : SW_Table) : String;
var i: Integer;
    whereClause, fk: String;
    fkList : TStringList;
begin
  assert(table1 <> nil);
  assert(table2 <> nil);

  i:=table1.relatedTables.IndexOf(table2.name);
  fk := table1.foreignKeys[i];
  fkList := TStringList.Create;
  Split(fk, ',', fkList);
  i:=0;
  while (i < fkList.Count-1) do
  begin
    whereClause:=whereClause+table1.name+'.'+fkList[i]+'='+table2.name+'.'+fkList[i+1]+' AND ';
    i := i+2;
  end;
  fkList.Free;
  Delete(whereClause,length(whereClause)-4,5); //delete the last ' and ', whereClause begins at 1!

  JoinStatement := whereClause;
end;

function TEditorViewForm.GetWhereClause(): String;
var connTable : SW_Table;
    whereClause, joinTablePart, nmTablePart: String;
    addToWhere: String;
    i : Integer;
begin
  GetWhereClause:= '';
  joinTablePart := '';
  nmTablePart := '';

  if (WhereClauseMemo.Text = '') then //if the user enters no whereClause
    whereClause := 'WHERE '
  else
    whereClause := WhereClauseMemo.Text + ' AND ';

  addToWhere:='';
  for i:= 0 to JoinTables.Count-1 do
  begin
    joinTablePart := JoinStatement(Table, SW_Table(JoinTables[i]));
    addToWhere := addToWhere + joinTablePart + ' AND ';
  end;

  //Check if there is something to add to the where clause
  if(addToWhere<>'')then
  begin
    Delete(addToWhere, Length(addToWhere)-4, 5); //delete the trailing ' AND '
    if(Pos(addToWhere, whereClause)=0)then
      whereClause:=whereClause+addToWhere;
  end;

  addToWhere:='';
  for i:= 0 to NMTables.Count-1 do
  begin
    connTable := Model.GetConnectionTable(table, SW_Table(NMTables[i]) );
    nmTablePart := JoinStatement(connTable, table);
    nmTablePart := nmTablePart + ' AND ' + JoinStatement(connTable, SW_Table(nmTables[i]) );

    addToWhere := addToWhere + nmTablePart + ' AND ';
  end;

  //Check if there is something to add to the where clause
  if(addToWhere<>'')then
  begin
    Delete(addToWhere, Length(addToWhere)-4, 5); //delete the trailing ' AND '
    if(Pos(addToWhere, whereClause)=0)then
      whereClause:=whereClause+addToWhere;
  end;


  if (whereClause = 'WHERE ') then
    whereClause := ''
  else if(Copy(whereClause, Length(whereClause)-4, 5)=' AND ')then
    //delete the trailing ' AND '
    Delete(whereClause, Length(whereClause)-4, 5);
  
  GetWhereClause := whereClause;
end;
}

end.

