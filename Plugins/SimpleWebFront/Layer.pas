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

unit Layer;

interface
uses EERModel, Classes, Contnrs, SWF_XML_Binding,  SysUtils;

type
  ETableDoesntExistException = class(Exception);
  EColumnDoesntExistException = class(Exception);
  
  //forward declaration
  SW_Table = class;                //SW = SimpleWebFront
  SW_Region = class;
  
  //more or less a singleton
  SW_Model = class(TObject)
  private
    allTables: TObjectList; // a list of SW_Table-Objects; contains all tables of the model
    origModel : TEERModel;
  public
    constructor Create(model : TEERModel);
    destructor Destroy; override;

    procedure GetAllNMRelations(tablename: string; stringList: TStringList);
    function FindTable(tablename: String) : SW_Table;  overload;
    function FindTable(EERId :Integer) :SW_Table; overload;

    //name with object
    procedure GetTablesWithFKOf(tablename : String; stringList:TStringList);

    //gets the names of all tables
    procedure GetAllTableNames(stringList :TStringList);
    //writes the name of all tables paired with the object into the argument
    procedure GetAllTables(stringList :TStringList);

    //gets the names of all regions
    procedure GetAllRegionNames(stringList: TStringList);
    //the names paired with the object reference to a SW_Region
    procedure GetAllRegions(stringList : TStringList);

    procedure GetAllTablesInRegion(region: SW_Region; stringList :TStringList);

    function GetConnectionTable(table, nmtable : SW_Table) : SW_Table;

    function GetDataTypeName(id :Integer) : String;
  end;


  SW_Region = class(TObject)
  private
    origRegion: TEERRegion;
  public
    constructor Create(Region: TEERRegion);
  end;

  SW_Table = class(TObject)
  private
    origTable: TEERTable;
    model : SW_Model; //father; needed when the model creates its tables
    Fname: String;
    Fcolumns :TObjectList; // a list of SW_Column objects
    FrelatedTables : TStringList; //lists the names of tables that have their key referenced in this table
    FprimaryKey : TStringList;
    Fjoin_columnName : string; //these three values
    Fjoin_width: Integer;      //are stored here because for all columns
    Fnm_width : Integer;       //they share the same value; (if used at all)
    function IsStillConsistent() :Boolean;

    //these 2 methods are used to initialize some of the private data
    procedure InitRelationships();
    procedure InitPrimaryKey();
  public
    property name: String read Fname;
    property Columns: TObjectList read FColumns;
    //related table lists all tables that are referenced by some attribute of this table
    //it stores only strings (their names), no objects
    property relatedTables: TStringList read FrelatedTables write FrelatedTAbles;

    //actually these are column properties but we are saving them here, since
    //they are the same for all columns of a table
    property Join_ColumnName: String read Fjoin_ColumnName write Fjoin_ColumnName;
    property Join_Width: Integer read Fjoin_width write Fjoin_width;
    property NM_Width: Integer read Fnm_width  write Fnm_width;

    property primaryKey: TStringList read FprimaryKey;

    constructor Create(origTable : TEERTable; model : SW_model = nil);
    destructor Destroy;  override;

    function HasRelationTo(tableName: String) : Boolean;
    function DeepCopy() : SW_Table;

    function GetEERId() : Integer;
    procedure GetFkColumns(list: TObjectList);

    //function SaveToString() :String;
    //class function LoadFromString(s: String) :SW_Table;

    procedure SaveToXMLNode(node :IXMLSWF_TableType);
    class function LoadFromXMLNode(node :IXMLSWF_TableType) :SW_Table;
  end;



  SW_Column = class(TObject)
  private
    origCol : TEERColumn;

    FName : string; //this is the name that the column has in the db   (=origCol.ColName)
    FGridName: string; //this is the name that should be shown in the output-files
    FDataTypeName, FDataTypeParams : string;
    Ftable : SW_Table;
    FselectedForGrid : Boolean; //should this col be displayed in the gridview
    FselectedForForm : Boolean; //form = update/insert popup-window
    FFixedWidth : Integer; //this value is used as a html/css width-attribute in the <td>-element
    FTruncateChars : Integer; //truncate values in this column to [truncateChars] chars
    FIsForeignKey : Boolean;  //does this column refer to the key of another table?
    FfkTablename : String; //is only defined when FIsForeignKey = true
    FfkColname: String;    //is only defined when FIsForeignKey = true
    FFormName : String;     //the name that should be displayed on the form page
    FFormWidth : Integer;   //the width of the input field for this column on the form page
    function isStillConsistent() :Boolean;
    function isPrimaryKey() :Boolean;
    function getDefaultValue() : STring;
    function getNotNull() : Boolean;
    function getObj_Id : Integer;
  public
    property name: String read FName;
    property selectedForGrid :Boolean read FselectedForGrid write FselectedForGrid;
    property selectedForForm :Boolean read FselectedForForm write FselectedForForm;
    property GridName: String read FGridName write FGridName;
    property FormName: String read FFormName write FFormName;
    property FormWidth: Integer read FFormWidth write FFormWidth;
    property FixedWidth: Integer read FFixedWidth write FFixedWidth;
    property TruncateChars: Integer read FTruncateChars write FTruncateChars;
    property Table :SW_Table read Ftable;
    property DataTypeName : String read FDataTypeName;
    property DataTypeParams: String read FDataTypeParams;
    property IsForeignKey :Boolean read FIsForeignKey;
    property fkTablename: String read FfkTablename;
    property fkColname: String read FfkColname;
    property IsKey : Boolean read IsPrimaryKey;
    property DefaultValue : String read getDefaultValue;
    property NotNull: Boolean read getNotNull;
    property Obj_id : Integer read getObj_Id;

    constructor Create(origCol :TEERColumn; table: SW_Table);
    destructor Destroy(); override;
    
    procedure ToggleGridSelected();
    procedure ToggleFormSelected();

    //every column is part of a table
    //this table has to be supplied by the caller since normally
    //the copied column should have a different (father)table than the source column
    function DeepCopy(father: SW_Table) :SW_Column;

    //function SaveToString() :String;
    //class function LoadFromString(s: String; father:SW_Table) :SW_Column;

    procedure SaveToXMLNode(node :IXMLSWF_ColumnType);
    class function LoadFromXMLNode(node :IXMLSWF_ColumnType; father:SW_Table) :SW_Column;
  end;

  procedure SetFKRelations(table: SW_Table);

var
  Model   : SW_Model;


implementation
uses SplitFns;

//does NOT assume ownership of model
constructor SW_Model.Create(model :TEERModel);
var i: Integer;
    tmp: SW_Table;
    allTables_EER : TList;
begin

  self.origModel := model;
  
  //now get all tables and store them in the allTables-member
  allTables:= TObjectList.Create;

  allTables_EER := TList.Create;
  origModel.GetEERObjectList([EERTable],allTables_EER);

  for i:=0 to allTables_EER.Count-1 do
  begin
    tmp := SW_Table.Create(allTables_EER[i], self);
    allTables.Add(tmp);
  end;
  allTables_EER.Free;
end;

destructor SW_Model.Destroy;
begin
  allTables.Free();
end;

procedure SW_Model.GetAllNMRelations(tablename: string; stringList :TStringList);
var i,j: Integer;
    table: SW_Table;
    intermediateTables : TStringList;
    fkList : TObjectList;
    primKey_is_made_of_fkeys_only :  Boolean;
begin
  intermediateTables := TStringList.Create;
  fkList := TObjectList.Create(false);

  try
  GetTablesWithFKOf(tablename, intermediateTables);
  for i:=0 to intermediateTables.Count-1 do
  begin
    table := SW_Table(intermediateTables.Objects[i]);

    //we expect a connection-table to connect exactly 2 tables
    if (table.relatedTables.Count <> 2) then continue;

    //moreover the 2 fks must be primary keys
    primKey_is_made_of_fkeys_only := true;
    fkList.Clear;
    table.GetFkColumns(fkList);

    if (table.primaryKey.Count <> fkList.Count) then continue;
    for j:= 0 to fkList.Count-1 do
    begin
      if (table.primaryKey.IndexOf( SW_Column(fklist[j]).name ) = -1) then
      begin
        primKey_is_made_of_fkeys_only := false;
        break;
      end;
    end;

    if (NOT primKey_is_made_of_fkeys_only) then continue;

    for j:=0 to table.relatedTables.Count-1 do
    begin
      //filter out our src-table
      if (table.relatedTables[j] = tablename) then continue;

      stringList.Add(table.relatedTables[j]);
    end;

  end;

  finally
    fkList.Free;
    intermediateTables.Free;
  end;
end;

function SW_Model.GetConnectionTable(table, nmtable : SW_Table) : SW_Table;
var i,j: Integer;
    tmpTable : SW_Table;
    intermediateTables : TStringList;
begin
  GetConnectionTable := nil;
  intermediateTables := TStringList.Create;
  GetTablesWithFKOf(table.name, intermediateTables);
  
  for i:=0 to intermediateTables.Count-1 do
  begin
    tmpTable := SW_Table(intermediateTables.Objects[i]);
    if (tmpTable.relatedTables.Count <> 2) then continue;

    for j:=0 to tmpTable.relatedTables.Count-1 do
    begin
      if (tmpTable.relatedTables[j] = table.name) then continue;
      if (tmpTable.relatedTables[j] = nmTable.name) then
        GetConnectionTable := tmpTable;
    end;

  end;

  intermediateTables.Free;
end;

procedure SW_Model.GetTablesWithFKOf(tablename : String; stringList: TStringList);
var i : Integer;
begin

  for i:=0 to allTables.Count-1 do
  begin
    if (SW_Table(allTables[i]).HasRelationTo(tablename)) then
      stringList.AddObject(SW_Table(allTables[i]).origTable.ObjName, allTables[i]);
  end;

end;

function SW_Model.FindTable(tablename: String) : SW_Table;
var i: Integer;
begin
  FindTable := nil;
  
  for i:=0 to allTables.Count-1 do
  begin
    if (SW_Table(allTables[i]).origTable.ObjName = tablename) then
    begin
      FindTable:= SW_Table(allTables[i]);
      break;
    end;
  end;

end;

function SW_Model.FindTable(EERId :Integer) : SW_Table;
var i: Integer;
begin
  FindTable := nil;

  for i:=0 to allTables.Count-1 do
  begin
    if (SW_Table(allTables[i]).GetEERId() = EERId) then
    begin
      FindTable:= SW_Table(allTables[i]);
      break;
    end;
  end;

end;


procedure SW_Model.GetAllTableNames(stringList :TStringList);
var i: Integer;
begin
  assert(stringList <>nil);

  for i:= 0 to self.allTables.Count-1 do
  begin
    stringList.Add(Sw_Table(allTables[i]).name);
  end;

end;


procedure SW_Model.GetAllTables(stringList :TStringList);
var i: Integer;
begin
  assert(stringList <>nil);

  for i:= 0 to self.allTables.Count-1 do
  begin
    stringList.AddObject(Sw_Table(allTables[i]).name, allTables[i]);
  end;

end;

procedure SW_Model.GetAllRegionNames(stringList: TStringList);
begin
  assert(stringList <>nil);
  origModel.GetEERObjectNameList([EERRegion], stringList);
end;

procedure SW_Model.GetAllRegions(stringList : TStringList);
var objList : TList;
    i: Integer;
    region : TEERRegion;
begin
  assert(stringList <>nil);

  objList := TList.Create;
  origModel.GetEERObjectList([EERRegion], objList);

  for i:=0 to objList.Count-1 do
  begin
    region := TEERRegion(objList[i]);
    stringList.AddObject(region.ObjName,SW_Region.Create(region));
  end;

  objList.Free;
end;


function SW_Model.GetDataTypeName(id :Integer) : String;
begin
  GetDataTypeName := origModel.GetDataTypeName(id);
end;

procedure SW_Model.GetAllTablesInRegion(region: SW_Region; stringList :TStringList);
var objList: TList;
    i : Integer;
    table: TEERTable;
begin
  assert(stringList <> nil);

  objList := TList.Create;
  region.origRegion.GetEERObjsInRegion([EERTable],objList);
  for i:=0 to objList.Count-1 do
  begin
    table := TEERTable(objList[i]);
    stringList.AddObject(table.ObjName, SW_Table.Create(table));
  end;

  objList.Free;
end;





constructor SW_Region.Create(region : TEERRegion);
begin
  self.origRegion := region;
end;







procedure SW_Table.SaveToXMLNode(node :IXMLSWF_TableType);
var i: Integer;
    xmlCol : IXMLSWF_ColumnType;
    col : SW_Column;
begin
  node.OrigTable := self.origTable.Obj_id;
  node.Join_ColumnName := Join_ColumnName;
  node.Join_Width := Join_Width;
  node.NM_Width := NM_Width;

  for i:= 0 to self.Columns.Count-1 do
  begin
    xmlCol := node.SWF_Columns.Add;
    col := SW_Column(Columns[i]);
    col.SaveToXMLNode(xmlCol);
  end;
end;

class function SW_Table.LoadFromXMLNode(node :IXMLSWF_TableType) :SW_Table;
var t :SW_Table;
    origTable :TEERTable;
    cols :IXMLSWF_ColumnsType;
    col: IXMLSWF_ColumnType;
    realCol : SW_Column;
    i,j: Integer;
begin
  t := Layer.Model.FindTable(node.OrigTable);
  if (t = nil) then
    raise ETableDoesntExistException.Create('This table is not in the model any longer.');

  origTable := t.origTable;
  t := SW_Table.Create(origTable);
  t.Join_ColumnName := node.Join_ColumnName;
  t.Join_Width := node.Join_Width;
  t.FNM_Width := node.NM_Width;


  cols := node.SWF_Columns;
  for i:= 0 to cols.Count-1 do
  begin
    try
    col := cols[i];
    realCol := SW_Column.LoadFromXMLNode(col, t);

    for j:=0 to t.columns.Count-1 do
    begin
      if (SW_Column(t.columns[j]).origCol.Obj_id = realCol.origCol.Obj_id) then
      begin
        t.Columns.Delete(j);
        break;
      end;
    end;
    t.Columns.Add(realCol);
    except
    on EColumnDoesntExistException do ;
    end;
  end;

  t.relatedTables.Clear;
  t.primaryKey.Clear;
  t.InitPrimaryKey();
  t.InitRelationships();

  LoadFromXMLNode := t;
end;
  {
function SW_Table.SaveToString() :String;
var str,tmpString :String;
    i : Integer;
begin
  assert(IsStillConsistent());
  
  str:= '(';
  str := str + 'EERId=' + IntToStr(GetEERId()) + delimiter;
  str := str + 'Join_ColumnName=' + Join_ColumnName + delimiter;
  str := str + 'Join_Width=' + IntToStr(Join_Width) + delimiter;
  str := str + 'NM_Width=' + IntToStr(NM_Width) + delimiter;

  tmpString := 'Columns=(';
  for i:=0 to Columns.Count-1 do
  begin
    tmpString := tmpString + SW_Column(Columns[i]).SaveToString + delimiter;
  end;
  if (Columns.Count > 0) THEN Delete(tmpString,Length(tmpString),1);  //delete the last delimiter
  tmpString := tmpString + ')';

  str := str + tmpString;

  SaveToString :=  str + ')';
end;

class function SW_Table.LoadFromString(s: String) :SW_Table;
var tmpTable: SW_Table;
    stringList :TStringList;
    cols_string, join_columnName :String;
    Join_width, nm_width: Integer;
    Columns : TObjectList;
    i: Integer;
begin
  stringList := TSTringList.Create;
  Delete(s,1,1);
  Delete(s,Length(s),1);

  IntelligentSplit(s, delimiter,stringList);

  tmpTable := Layer.Model.FindTable(StrToInt(stringList.Values['EERId']));
  Join_ColumnName := stringList.Values['Join_ColumnName'];
  Join_Width := -1;
  NM_Width := -1;
  if (stringList.Values['Join_Width'] <> '') then
  begin
    Join_Width := StrToInt(stringList.Values['Join_Width']);
    NM_Width := StrToInt(stringList.Values['NM_Width']);
  end;

  tmpTable := tmpTable.DeepCopy();
  tmpTable.Fjoin_columnName := Join_ColumnName;
  tmpTable.Fjoin_width := Join_Width;
  tmpTable.Fnm_width := NM_Width;

  cols_string := stringList.Values['Columns'];
  stringList.Clear();

  //remove the enclosing bracket-pair
  Delete(cols_string,1,1);
  Delete(cols_string,Length(cols_string),1);

  IntelligentSplit(cols_string, delimiter, stringList);

  Columns:= TObjectList.Create();
  for i:= 0 to stringList.Count-1 do
  begin
    Columns.Add(SW_Column.LoadFromString(stringList[i],tmpTable));
  end;

  tmpTable.Fcolumns.Free;
  tmpTable.Fcolumns := Columns;

  //SetFKRelations(tmpTable);
  LoadFromString := tmpTable;
end;

   }

procedure SW_Table.GetFkColumns(list: TObjectList);
var i : Integer;
    tmpCol : SW_Column;
begin
  assert(list <> nil);
  assert(list.OwnsObjects = false);
  
  for i:= 0 to columns.Count-1 do
  begin
    tmpCol := SW_Column(columns[i]);
    if (tmpCol.IsForeignKey) then list.Add(tmpCol);
  end;

end;

procedure SetFKRelations(table: SW_Table);
var i,j,k:Integer;
    rel: TEERRel;
    fieldName, origKeyname : String;
    tmpCol : SW_Column;
begin
  //for all relations that end here
  for i:=0 to table.origTable.RelEnd.Count-1 do
  begin
    rel:= table.origTable.RelEnd[i];
    table.relatedTables.Add( TEERTable(rel.SrcTbl).ObjName );

    for j:=0 to rel.FKFields.Count-1 do
    begin
      fieldName := rel.FKFields.ValueFromIndex[j];
      origKeyname := rel.FKFields.Names[j];

      for k:= 0 to table.Columns.Count-1 do
      begin
        tmpCol := SW_Column(table.columns[k]);
        if (tmpCol.name =  fieldName) then
        begin
          tmpCol.FIsForeignKey := true;
          tmpCol.FfkTablename := TEERTable(rel.SrcTbl).ObjName;
          tmpCol.FfkColname := origKeyname;
          break;
        end;
      end;
    end;
  end;
end;

procedure SW_Table.InitPrimaryKey();
var i : Integer;
    tmpCol : SW_Column;
begin
  assert(columns <> nil);
  assert(primaryKey <> nil);
  assert(primaryKey.Count = 0);

  for i:=0 to origTable.Columns.Count-1 do
  begin
    tmpCol := SW_Column.Create(TEERColumn(origTable.Columns[i]), self);

    if (TEERColumn(origTable.Columns[i]).PrimaryKey) then
      primaryKey.AddObject(tmpCol.name, tmpCol);
  end;

end;

//fills the relatedTables-stringlist
//set the FK-related attributes of the columns
procedure SW_Table.InitRelationships();
var i,j,k: Integer;
    rel : TEERRel;
    tmpCol : SW_Column;
    fieldName, origKeyname : String;
begin
  assert(RelatedTables <> nil);
  assert(RelatedTables.Count  = 0);
  assert(columns <> nil);

  RelatedTables.Sorted := true;
  RelatedTables.Duplicates:= dupIgnore;

  //for all relations that end here
  for i:=0 to origTable.RelEnd.Count-1 do
  begin
    rel:= origTable.RelEnd[i];
    relatedTables.Add( TEERTable(rel.SrcTbl).ObjName );

    for j:=0 to rel.FKFields.Count-1 do
    begin
      fieldName := rel.FKFields.ValueFromIndex[j];
      origKeyname := rel.FKFields.Names[j];

      for k:= 0 to self.Columns.Count-1 do
      begin
        tmpCol := SW_Column(columns[k]);
        if (tmpCol.name =  fieldName) then
        begin
          tmpCol.FIsForeignKey := true;
          tmpCol.FfkTablename := TEERTable(rel.SrcTbl).ObjName;
          tmpCol.FfkColname := origKeyname;
          break;
        end;
      end;
    end;
  end;

end;

constructor SW_Table.Create(origTable : TEERTable; model : SW_model);
var i:Integer;
    tmpCol : SW_Column;
begin
  self.origTable := origTable;
  Fname := origTable.ObjName;
  self.model := model;

  self.Fjoin_width := -1;
  self.Fnm_width := -1;

  //set the other member variables
  FprimaryKey := TStringList.Create;
  Fcolumns := TObjectList.Create;

  for i:=0 to origTable.Columns.Count-1 do
  begin
    tmpCol := SW_Column.Create(TEERColumn(origTable.Columns[i]), self);
    columns.Add(tmpCol);
  end;

  InitPrimaryKey();

  //Set a default value for Fjoin_columName
  //the default for the columnname is the name of all key attributes separeted by a comma
  for i:= 0 to FprimaryKey.Count-1 do
    self.Fjoin_columnName := Fjoin_columnName + ','+primaryKey[i];
  Delete(Fjoin_columnName,1,1); //remove the first comma

  FrelatedTables := TStringList.Create;

  InitRelationships();

end;

destructor SW_Table.Destroy;
begin
  relatedTables.Free;
  columns.Free;
  primaryKey.Free;
end;

function SW_Table.HasRelationTo(tableName: String) : Boolean;
var i: Integer;
begin
  HasRelationTo := false;

  for i:=0 to self.relatedTables.Count-1 do
  begin
    if (relatedTables[i] = tableName) then
      HasRelationTo := True;
  end;
end;

function SW_Table.DeepCopy() :SW_Table;
var tmpTable :SW_Table;
    i : Integer;
begin
  assert(IsStillConsistent());

  tmpTable := SW_Table.Create(self.origTable);
  tmpTable.Columns.Clear;
  for i:= 0 to Columns.Count-1 do
    tmpTable.columns.Add(SW_Column(Columns[i]).DeepCopy(tmpTable));

  tmpTable.origTable := origTable;
  tmpTable.Fname := Fname;
  tmpTable.relatedTables.Assign(relatedTables);
  tmpTable.FprimaryKey.Assign(FprimaryKey);
  tmpTable.Fjoin_columnName:= Fjoin_columnName;
  tmpTable.Fjoin_width := Fjoin_width;
  tmpTable.Fnm_width := Fnm_width;

  DeepCopy:= tmpTable;
end;

function SW_Table.GetEERId() : Integer;
begin
  GetEERId := origTable.Obj_id;
end;

function SW_Table.isStillConsistent() : Boolean;
var expectedSize : LongInt;
    realSize : LongInt;
begin
    expectedSize := 40; //found out by debugging

    realSize := self.InstanceSize;
    isStillConsistent := (expectedSize = realSize);
end;





 {
function SW_Column.SaveToString() :String;
var str :String;
begin
  assert(IsStillConsistent());
  
  str:= '(';
  str := str + 'ColId=' + IntToStr(origCol.Obj_id) + delimiter;
  str := str + 'TableId=' + IntToStr(table.GetEERId()) + delimiter;
  str := str + 'FixedWidth=' + IntToStr(FixedWidth) + delimiter;
  str := str + 'TruncateChars=' + IntToStr(truncateChars) + delimiter;
  str := str + 'SelectedForGrid=' + BoolToStr(selectedForGrid) + delimiter;
  str := str + 'SelectedForForm=' + BoolToStr(selectedForForm) + delimiter;
  str := str + 'ShowName=' + GridName + delimiter;
  str := str + 'IsForeignKey=' + BoolToStr(IsForeignKey) + delimiter;
  str := str + 'fkTablename=' + fkTablename + delimiter;
  str := str + 'fkColname=' + fkColname + delimiter;
  str := str + 'FormName=' + FormName + delimiter;
  str := str + 'FormWidth=' + IntToStr(FormWidth) + delimiter;
  str := str + ')';

  SaveToString :=  str;
end;   }

procedure SW_Column.SaveToXMLNode(node :IXMLSWF_ColumnType);
begin
  node.OrigCol := self.origCol.Obj_id;
  node.GridName := GridName;
  node.FormName := FormName;
  node.SelectedForGrid := SelectedForGrid;
  node.SelectedForForm := SelectedForForm;
  node.FixedWidth := FixedWidth;
  node.TruncateChars := TruncateChars;
  node.IsForeignKey := IsForeignKey;
  node.FkTablename := FkTablename;
  node.FkColname := FkColname;
  node.FormWidth := FormWidth;
end;

class function SW_Column.LoadFromXMLNode(node :IXMLSWF_ColumnType; father:SW_Table) :SW_Column;
var origCol : TEERColumn;
    c : SW_Column;
begin
  origCol := father.origTable.GetColumnByID(node.OrigCol);
  if (origCol = nil) then
    raise EColumnDoesntExistException.Create('This column is not in the model any longer.');

  c := SW_Column.Create(origCol, father);

  c.FGridName := node.GridName;
  c.FFormName := node.FormName;
  c.FselectedForGrid := node.SelectedForGrid;
  c.FselectedForForm := node.SelectedForForm;
  c.FFixedWidth := node.FixedWidth;
  c.FTruncateChars := node.TruncateChars;
  c.FFormWidth := node.FormWidth;

  //we don't have to do this since these values will be overwritten
  //in InitRelationships anyways
  c.FIsForeignKey := node.IsForeignKey;
  c.FfkTablename := node.FkTablename;
  c.FfkColname := node.FkColname;


  LoadFromXMLNode := c;
end;
  {
class function SW_Column.LoadFromString(s: String; father:SW_Table) :SW_Column;
var table: SW_Table;
    origCol : TEERColumn;
    stringList :TStringList;
    tmpCol: Sw_Column;
begin
  stringList := TSTringList.Create;
  Delete(s,1,1);
  Delete(s,Length(s),1);

  IntelligentSplit(s, delimiter,stringList);

  table := father;
  origCol := table.origTable.GetColumnByID(StrToInt(stringList.Values['ColId']));

  tmpCol := SW_Column.Create(origCol,table);

  tmpCol.GridName := stringList.Values['ShowName'];
  tmpCol.fixedWidth := StrToInt(stringList.Values['FixedWidth']);
  tmpCol.truncateChars := StrToInt(stringList.Values['TruncateChars']);
  tmpCol.selectedForGrid := StrToBool(stringList.Values['SelectedForGrid']);
  tmpCol.SelectedForForm := StrToBool(stringList.Values['SelectedForForm']);
  tmpCol.FIsForeignKey := StrToBool(stringList.Values['IsForeignKey']);
  tmpCol.FfkTablename := stringList.Values['fkTablename'];
  tmpCol.FfkColname := stringList.Values['fkColname'];

  if (stringList.Values['FormName'] <> '') then
    tmpCol.FFormname := stringList.Values['FormName']
  else
    tmpCol.FFormname := tmpCol.FName;
    
  tmpCol.FFormWidth := -1;
  if (stringList.Values['FormWidth'] <> '') then
    tmpCol.FFormWidth := StrToInt(stringList.Values['FormWidth']);

  LoadFromString := tmpCol;
end;
         }

constructor SW_Column.Create(origCol :TEERColumn; table : SW_Table);
begin
  self.origCol := origCol;
  Ftable := table;
  Fname := origCol.ColName;
  selectedForGrid := true;
  selectedForForm := true;
  FIsForeignKey := false;

  self.FDataTypeParams := origCol.DatatypeParams;
  self.fgridName := name;
  self.fixedWidth := -1;
  self.truncateChars := -1;

  //when the program starts, one model object of type Sw_Model is created
  //this single instance has a list of all tables and columns available in the model
  //anyway the global variable model is still nil when the model is being created
  if (table.model <> nil) then
    self.FDataTypeName := table.model.GetDataTypeName(origCol.idDataType)
  else
    self.FDataTypeName := model.GetDataTypeName(origCol.idDataType);
  FFormName := origCol.ColName;
  FFormWidth := -1;
end;

destructor SW_Column.Destroy;
begin
  inherited Destroy;
end;

procedure SW_Column.ToggleGridSelected;
begin
  selectedForGrid := NOT selectedForGrid;
end;

procedure SW_Column.ToggleFormSelected;
begin
  selectedForForm := NOT selectedForForm;
end;



function SW_Column.DeepCopy(father: SW_Table) :SW_Column;
var tmpCol :SW_Column;
begin
  //assert(isStillConsistent());
  assert(father <> nil); //every column must be part of a table;

  tmpCol := SW_Column.Create(origCol,self.Ftable);

  tmpCol.origCol := self.origCol;
  tmpCol.FName := self.FName;
  tmpCol.FGridName := self.FGridName;
  tmpCol.FDataTypeName := self.FDataTypeName;
  tmpCol.Ftable := father;
  tmpCol.FselectedForGrid := self.FselectedForGrid;
  tmpCol.FselectedForForm := self.FselectedForForm;
  tmpCol.FfixedWidth := self.FfixedWidth;
  tmpCol.TruncateChars := self.TruncateChars;
  tmpCol.FIsForeignKey := self.FIsForeignKey;
  tmpCol.FfkTablename := self.FfkTablename;
  tmpCol.FfkColname := self.FfkColname;
  tmpCol.FFormName := self.FFormName;
  tmpCol.FFormWidth := self.FFormWidth;

  DeepCopy := tmpCol;
end;

function SW_Column.isStillConsistent() : Boolean;
var expectedSize : LongInt;
    realSize : LongInt;
begin
    expectedSize := 56; //found out by debugging
    {expectedSize := expectedSize + sizeof(origCol);          //object
    expectedSize := expectedSize + sizeof(@FName);           //string
    expectedSize := expectedSize + sizeof(@FshowName);       //string
    expectedSize := expectedSize + sizeof(@FDataTypeName);   //string
    expectedSize := expectedSize + sizeof(Ftable);           //object
    expectedSize := expectedSize + sizeof(FselectedForGrid); //boolean
    expectedSize := expectedSize + sizeof(FselectedForForm); //boolean
    expectedSize := expectedSize + sizeof(FFixedWidth);      //integer
    expectedSize := expectedSize + sizeof(FTruncateChars);   //integer
    expectedSize := expectedSize + sizeof(FIsForeignKey);    //boolean
    expectedSize := expectedSize + sizeof(@FfkTablename);    //string
    expectedSize := expectedSize + sizeof(@FfkColname);      //string
    expectedSize := expectedSize + sizeof(@FFormName);       //string
    expectedSize := expectedSize + sizeof(FFormWidth);       //integer
    }
    realSize := self.InstanceSize;
    isStillConsistent := (expectedSize = realSize);
end;

function SW_Column.IsPrimaryKey() : Boolean;
begin
  IsPrimaryKey := origCol.PrimaryKey;
end;

function SW_Column.getDefaultValue() : String;
begin
  GetDefaultValue := origCol.defaultValue;
end;

function SW_Column.getNotNull() : Boolean;
begin
  GetNotNull := origCol.NotNull;
end;

function SW_Column.getObj_Id() : Integer;
begin
  getObj_Id := origCol.Obj_id;
end;

end.


