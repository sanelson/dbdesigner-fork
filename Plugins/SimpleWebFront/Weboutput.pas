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

unit Weboutput;

interface
uses Classes,Contnrs, Layer, QGraphics, SWF_XML_Binding, SysUtils;

type

  ETemplateFileException = class(Exception);

  //a class symbolyzing a Group
  //*************Class TGroup**************************
  //***************************************************
  TGroup = class(TObject)
  private
    FName: string; //the name of the group
    FshowOnLine, FshowInColumn : Integer; //the position on the index-webpage
    Views: TObjectList; //the views associated with this group
    FViewsAsPopup : Boolean; //show views in a browser popup-window or in the same window?

  public
    property Name: String read FName write FName;
    property ShowOnLine :Integer read FshowOnLine write FshowOnLine;
    property ShowInColumn :Integer read FshowInColumn write FshowInColumn;
    property ViewsAsPopup :Boolean read FViewsAsPopup write FViewsAsPopup;

    constructor Create(Groupname: String; lineNumber,columnNumber: Integer; viewsAsPopup: Boolean); overload;
    destructor Destroy; override;

    //function SaveToString() :String;
   // class function LoadFromString(s :String) :TGroup;

    procedure SaveToXMLNode(node :IXMLSWF_GroupType);
    class function LoadFromXMLNode(node :IXMLSWF_GroupType) :TGroup;
  end;


  //TView represents an sql view
  //*************Class TView***************************
  //***************************************************
  TView = class(TObject)
  private
    FFormHeight, FFormWidth : Integer;
    FFormX,FFormY : Integer;
    FGridPopupHeight, FGridPopupWidth: Integer;
    FGridPopupX, FGridPopupY : Integer;
    FName: String;
    FWhereClause: String;
    FTable : SW_Table;
    FRowsPerPage: Integer;
    FUseCompoundColNames : Boolean;
    FIcon : TPicture;
    FIconFilename: String;
    FOrderBy : String;
    FJoinTables, FNMTables : TObjectList; //a list of SW_Table objects
    GridSortedColumnList,FormSortedColumnList: TStringList;
    
    //returns the Columns from all tables in a stringlist consisting of (name,SW_Column) pairs
    procedure GetColumns(stringList: TStringList); overload;
    class procedure GetColumns(stringList: TStringList; view:TView);overload;

    //This constructor is meant for internal use only; used by LoadFromString
    constructor Create(); overload;
    function RelationsValid() : Boolean;
    class function FindColumnInView(obj_id: Integer; view: TView) : SW_Column;
  public
    property FormHeight : Integer read FFormHeight write FFormHeight;
    property FormWidth : Integer read FFormWidth write FFormWidth;
    property FormX : Integer read FFormX write FFormX;
    property FormY : Integer read FFormY write FFormY;
    property GridPopupHeight : Integer read FGridPopupHeight write FGridPopupHeight;
    property GridPopupWidth: Integer read FGridPopupWidth write FGridPopupWidth;
    property GridPopupX : Integer read FGridPopupX write FGridPopupX;
    property GridPopupY : Integer read FGridPopupY write FGridPopupY;
    property Name : String read FName write FName;
    property WhereClause :String read FWhereClause write FWhereClause;
    property Table: SW_Table read FTable write FTable;
    property RowsPerPage: Integer read FRowsPerPage write FRowsPerPage;
    property UseCompoundColNames: Boolean read FUseCompoundColNames write FUseCompoundColNames;
    property Icon: TPicture read FIcon;
    property JoinTables: TObjectList read FJoinTables;
    property NMTables :TObjectList read FNMTables;
    property IconFilename: STring read FIconFilename write FIconFilename;
    property OrderBy : String read FOrderBy write FOrderBy;

    //methods
    constructor Create(Viewname, WhereClause :String; Table : SW_Table;
                       JoinTables, NMTables: TObjectList; IconImage:TPicture;
                       IconFilename,OrderBy:String;RowsPerPage: Integer = 20); overload;

    destructor Destroy; override;

    //returns the Columns from all tables in a stringlist consisting of (name,SW_Column) pairs
    //moreover the stringlist is ordered after the way the columns shall appear on the GridList
    procedure GetGridSortedColumns(stringList: TStringList);

    //returns the Columns from all tables in a stringlist consisting of (name,SW_Column) pairs
    //moreover the stringlist is ordered after the way the columns shall appear on the Form
    procedure GetFormSortedColumns(stringList: TStringList);

    procedure ExchangeGridSortedColumns(ind1, ind2 :Integer);
    procedure ExchangeFormSortedColumns(ind1, ind2 :Integer);

    procedure RebuildSortColumns();

    function OneGridColVisible() : Boolean;
    //function GetSelectStatement() : String;


    procedure SaveToXMLNode(node :IXMLSWF_ViewType);
    class function LoadFromXMLNode(node :IXMLSWF_ViewType) :TView;
  end;

  //TWeboutput is the base class for all output classes
  //*************Class TWEBOUTPUT**********************
  //***************************************************
  TWebOutput = class(TObject)
  private
    //fields
    GroupList : TObjectList; //a list of TGroup-objects
    UnassignedViewsList : TObjectList; //contains all views that are not assigned to a view yet

    Fhostname,Fusername,Fpassword,Fdatabasename : String;  //db-connection settings
    Fheading, FsaveDir,Flayout : String;
    fdirDoesntExist :Boolean;
    procedure SetSaveDir(s: String);
  public
    property dirDoesntExist: Boolean read FDirdoesntExist;
    property Hostname: String read FHostname write FHostname;
    property Username: STring read FUsername write FUsername;
    property Password: STring read FPassword write FPassword;
    property Databasename: String read FDatabasename write FDatabasename;
    property Heading: String read FHeading write FHeading;
    property SaveDir: String read FSaveDir write SetSaveDir;
    property Layout: String read FLayout write FLayout;

    //methods
    constructor Create;
    destructor Destroy; override;

    //group/view management functions
    //procedure AddGroup(Groupname: String; lineNumber, columnNumber: Integer);
    procedure AddGroup(group : TGroup);
    procedure DeleteGroup(group: TGroup); //removes the specified object from the list and frees it
    function GroupPositionFree(lineNo, ColNo : Integer) : Boolean;
    function GetGroupAtPosition(lineNo, ColNo : Integer) : TGroup;


    //adds the view to the unassignedviewsList
    procedure AddView(view: TView); overload;
    procedure DeleteView(view: TView);
    procedure AssignViewToGroup(group :TGroup; view :TView);
    procedure UnassignViewFromGroup(group :TGroup; view :TView);

    //returns all groups in a StringList that contains strings paired with objects
    procedure GetGroupNames(stringList : TStringList);

    procedure GetViewNamesFromGroup(group :TGroup ; stringList :TStringList);

    procedure GetViewNames(stringList :TStringList);

    procedure GetUnassignedViewNames(stringList : TStringList);


    //returns true if all necessary settings for creating the pages have been made
    function InputComplete() : Boolean;

    //procedure SaveToStringList(stringList :TStringList);
    //procedure LoadFromStringList(stringList :TStringList);

    procedure LoadFromXMLNode(node :IXMLSWF_DataType);
    procedure SaveToXMLNode(node :IXMLSWF_DataType);

    //makes the output file(s)
    procedure Run(); virtual; abstract;

  end;

  //this class implements the output for php-files
  //*************Class TPHPOUTPUT**********************
  //***************************************************
  TPHPOutput = class (TWeboutput)

  private
    procedure CreateIncludeFile();
    procedure CreateIndexFile(IndexTemplateFilename: string);
    procedure CreateFrameFiles(FrameTemplateFilename: String);
    procedure CreateGridFiles(GridTemplateFilename :String);
    procedure CreateSQLfiles(SQLTemplateFilename :string);
    procedure CreateUpdateFiles(UpdateTemplateFilename :string);
    procedure CreateInsertFiles(InsertTemplateFilename :string);
    procedure CreateSearchFiles(SearchTemplateFilename: string);
    function MakeMainIndex() : String;    //index
    function MakeKey(table : SW_Table) : String; overload;
    function MakeKey(tables : TObjectList) : String; overload;
    function MakeDataTypes(table: SW_Table) : String; overload;
    function MakeDataTypes(tables: TObjectList) : String; overload;
    function MakeDataTypeParams(table: SW_Table) : String; overload;
    function MakeDataTypeParams(tables: TObjectList) : String; overload;
    function MakeDispCols(table: SW_Table; option : Integer) : String;  overload;
    function MakeDispCols(tables :TObjectList; option :Integer) : String;  overload;
    function MakeTableNames(tables: TObjectList) : String;
    function MakeConnectionTables(mainTable: SW_Table; nmTables: TObjectList) : String;
    procedure SaveIcons();
    function MakeColumnCaptions(view: TView) : String;
    function MakeFormColumnCaptions(view: TView) : String;
    function MakeColumnWidths(view :TView) : String;
    function MakeFormColumnWidths(view :TView) : String;
    function MakeColumnTruncChars(view :TView) : String;
    function MakeKeyMapping(view: TView) : String;
    function MakeImageNames() :String;
    function MakeDefaultValues(table: SW_Table) : String;
    function MakeNotNull(table: SW_Table) : String;
    function MakeSortedSelectClause(view: TView) :String;
    function MakeColumnOrder(view: TView) :String;
    function MakeGridColumnOrder(view: TView) :String;

  public
    procedure Run(); override;
  end;





implementation
uses QForms,Main, MainDM, StrUtils, QDialogs, IniFiles, SplitFns, StringConstants, QControls;

//The tokens denote character sequences that will be expanded in the php-code.
const SELECT_TOKEN = '/*[[SELECT]]*/';
      WHERE_CONSTRAINT_TOKEN = '/*[[WHERE_CONSTRAINT]]*/';
      INCLUDE_TOKEN = '/*[[INCLUDE]]*/';
      VIEWNAME_TOKEN = '/*[[VIEWNAME]]*/';
      HEADING_TOKEN = '/*[[HEADING]]*/';
      INDEX_TOKEN = '/*[[INDEX]]*/';

      FILENAME_TOKEN = '/*[[FILENAME]]*/';
      GRID_FILENAME_TOKEN = '/*[[FILENAME_GRID]]*/';
      FRAME_FILENAME_TOKEN = '/*[[FILENAME_FRAME]]*/';

      IMAGES_TOKEN = '/*[[IMAGE_NAMES]]*/';
      SORTED_SELECT_CLAUSE_TOKEN = '/*[[SORTED_SELECT_CLAUSE]]*/';
      COLUMNS_ORDER_TOKEN = '/*[[COLUMNS_ORDER]]*/';

      TABLENAME_TOKEN = '/*[[TABLENAME]]*/';
      DATATYPES_TOKEN = '/*[[DATATYPES]]*/';
      DATATYPE_PARAMS_TOKEN = '/*[[DATATYPE_PARAMS]]*/';
      TABLEKEY_TOKEN = '/*[[TABLEKEY]]*/';
      TABLEDISPCOLS_TOKEN = '/*[[TABLEDISPCOLS]]*/';
      MAIN_DEFAULT_VALUE_TOKEN = '/*[[MAIN_DEFAULT_VALUE]]*/';
      MAIN_NOT_NULL_TOKEN = '/*[[MAIN_NOTNULL]]*/';

      JOINTABLENAME_TOKEN = '/*[[JOINTABLENAMES]]*/';
      JOINTABLEKEY_TOKEN = '/*[[JOINTABLES_KEY]]*/';
      JOINTABLEDISPCOLS_TOKEN = '/*[[JOINTABLES_DISPCOLS]]*/';

      NMTABLENAME_TOKEN = '/*[[NMTABLENAMES]]*/';
      NMTABLEKEY_TOKEN = '/*[[NMTABLES_KEY]]*/';
      NMTABLEDATATYPES_TOKEN = '/*[[NMTABLES_DATATYPES]]*/';
      NMTABLEDATATYPE_PARAMS_TOKEN = '/*[[NMTABLES_DATATYPE_PARAMS]]*/';
      NMTABLEDISPCOLS_TOKEN = '/*[[NMTABLES_DISPCOLS]]*/';
      CONNECTIONTABLENAME_TOKEN = '/*[[CONNECTIONTABLENAMES]]*/';

      KEY_MAPPING_TOKEN = '/*[[KEY_MAPPING]]*/';

      //these tokens are recognized in the grid-file only
      ROWSPP_TOKEN = '/*[[ROWSPERPAGE]]*/';
      GRID_AS_POPUP_TOKEN = '/*[[GRID_AS_POPUP]]*/';

      FORMHEIGHT_TOKEN = '/*[[FORMHEIGHT]]*/';
      FORMWIDTH_TOKEN =  '/*[[FORMWIDTH]]*/';
      FORMX_TOKEN = '/*[[FORMX]]*/';
      FORMY_TOKEN = '/*[[FORMY]]*/';
      COLUMN_CAPTIONS_TOKEN = '/*[[COLUMN_CAPTIONS]]*/';
      COLUMN_WIDTHS_TOKEN = '/*[[COLUMN_WIDTHS]]*/';
      COLUMN_TRUNC_TOKEN = '/*[[COLUMN_TRUNCATE_CHARS]]*/';



      {$IFDEF MSWINDOWS}
        EOL = #13#10; //Carriage Return + Newline
      {$ELSEIF LINUX}
        EOL = #10;  //only newline
      {$IFEND}
      


//*************************************************************
//**Implementations for TGroup*********************************
//*************************************************************
constructor TGroup.Create(Groupname: String; lineNumber, columnNumber: Integer; viewsAsPopup: Boolean);
begin
  inherited Create();

  Name:= Groupname;
  Views := TObjectList.Create();
  showOnLine := lineNumber;
  showInColumn := columnNumber;
  FViewsAsPopup := viewsAsPopup;
end;

destructor TGroup.Destroy;
begin
  Views.Free();
end;


procedure TGroup.SaveToXMLNode(node :IXMLSWF_GroupType);
var i : Integer;
    xmlView :IXMLSWF_ViewType;
    view: TView;
begin
  node.Name := self.FName;
  node.ShowOnLine := ShowOnLine;
  node.ShowInColumn := ShowInColumn;
  node.ViewsAsPopup := self.FViewsAsPopup;

  for i:= 0 to Views.Count-1 do
  begin
    xmlView := node.SWF_Views.Add;
    view := TView(Views[i]);
    view.SaveToXMLNode(xmlView);
  end;
end;

class function TGroup.LoadFromXMLNode(node :IXMLSWF_GroupType) :TGroup;
var views : IXMLSWF_ViewsType;
    view: IXMLSWF_ViewType;
    i : Integer;
    grpName : string;
    lineNo,colNo : Integer;
    ViewsAsPopup : Boolean;
    tmpGrp :TGroup;
    viewList :TObjectList;
    realView :TView;
begin
  grpName := node.Name;
  lineNo := node.ShowOnLine;
  colNo:= node.ShowInColumn;
  ViewsAsPopup := node.ViewsAsPopup;

  views := node.SWF_Views;
  viewList := TObjectList.Create;

  for I:=0 to views.Count-1 do
  begin
    view:= views[i];
    realView := TView.LoadFromXMLNode(view);
    if (realView <> nil) then //if one of the tables of the view doesn't exist
      viewList.Add(realView);
  end;

  TmpGrp := Create(grpName,lineNo,colNo, ViewsAspopup);
  TmpGrp.Views := ViewList;
  LoadFromXMLNode :=  TmpGrp;
end;



//*************************************************************
//**Implementations for TView**********************************
//*************************************************************

procedure TView.SaveToXMLNode(node :IXMLSWF_ViewType);
var i :Integer;
    tmpTable :SW_Table;
    xmlTable : IXMLSWF_TableType;
    stream : TMemoryStream;
    gs, fs : STring;
begin
  node.FormHeight := FormHeight;
  node.FormWidth := FormWidth;
  node.FormX := FormX;
  node.FormY := FormY;
  node.GridPopupHeight := GridPopupHeight;
  node.GridPopupWidth := GridPopupWidth;
  node.GridPopupX := GridPopupX;
  node.GridPopupY := GridPopupY;
  node.Name := Name;
  node.WhereClause := WhereClause;
  node.RowsPerPage := RowsPerPage;
  node.UseCompoundColNames := UseCompoundColNames;
  node.IconFilename := FIconFilename;
  node.OrderBy := self.OrderBy;
  
  //converting the sorted column lists
  gs := '';
  for i:=0 to self.GridSortedColumnList.Count-1 do
  begin
    gs := gs + IntToStr(SW_Column(GridSortedColumnList.Objects[i]).Obj_id) +',';
  end;
  Delete(gs,length(gs),1);
  node.GridSortedColumns := gs;

  fs := '';
  for i:=0 to self.FormSortedColumnList.Count-1 do
  begin
    fs := fs + IntToStr(SW_Column(FormSortedColumnList.Objects[i]).Obj_id) +',';
  end;
  Delete(fs,length(fs),1);
  node.FormSortedColumns := fs;

  //Converting the picture
  stream:=TMemoryStream.Create;
  try
    Icon.Bitmap.SaveToStream(stream);
    node.Icon := DMMain.EncodeStreamForXML(stream);
  finally
    FreeAndNil(stream);
  end;


  self.Table.SaveToXMLNode(node.SWF_Table);

  for i:= 0 to JoinTables.Count-1 do
  begin
    xmlTable := node.JoinTables.Add;
    tmpTable := SW_Table(JoinTables[i]);
    tmpTable.SaveToXMLNode(xmlTable);
  end;

  for i:= 0 to NMTables.Count-1 do
  begin
    xmlTable := node.NMTables.Add;
    tmpTable := SW_Table(NMTables[i]);
    tmpTable.SaveToXMLNode(xmlTable);
  end;
end;

class function TView.LoadFromXMLNode(node :IXMLSWF_ViewType) :TView;
var v :TView;
    joinTables :IXMLJoinTablesType;
    table: IXMLSWF_TableType;
    realTable : SW_Table;
    i :Integer;
    nmTables :IXMLNMTablesType;
    theImgFile: TMemoryStream;
    gs,fs :string;
    helpList : TStringList;
    col : SW_Column;
begin
  try
  v := TView.Create;
  v.FFormHeight:= node.FormHeight;
  v.FFormWidth := node.FormWidth;
  v.FFormX := node.FormX;
  v.FFormY := node.FormY;
  v.FGridPopupHeight := node.GridPopupHeight;
  v.FGridPopupWidth := node.GridPopupWidth;
  v.FGridPopupX := node.GridPopupX;
  v.FGridPopupY := node.GridPopupY;
  v.FName := node.Name;
  v.FWhereClause := node.WhereClause;
  v.FRowsPerPage := node.RowsPerPage;
  v.FUseCompoundColNames := node.UseCompoundColNames;
  v.FTable := SW_Table.LoadFromXMLNode(node.SWF_Table);
  v.FIconFilename := node.IconFilename;
  v.OrderBy := node.OrderBy;
  v.FIcon := TPicture.Create();
  theImgFile:=TMemoryStream.Create;
  try
    DMMain.DecodeStreamFromXML(node.Icon, theImgFile);
    v.Icon.Bitmap.LoadFromStream(theImgFile);
  finally
    FreeAndNil(theImgFile);
  end;



  joinTables:= node.JoinTables;
  v.FJoinTables := TObjectList.Create;

  for i:= 0 to joinTables.Count-1 do
  begin
    table := joinTables[i];
    realTable := SW_Table.LoadFromXMLNode(table);
    v.FJoinTables.Add(realTable);
  end;

  nmTables:= node.NMTables;
  v.FNMTables := TObjectList.Create;

  for i:= 0 to nmTables.Count-1 do
  begin
    table := NMTables[i];
    realTable := SW_Table.LoadFromXMLNode(table);
    v.FNMTables.Add(realTable);
  end;

  if (NOT v.RelationsValid()) then
  begin
    v.JoinTables.Clear;
    v.NMTables.Clear;
  end;

  //load in the GridSortedColumns property
  gs := node.GridSortedColumns;
  helpList := TStringList.Create;
  Split (gs, ',', helpList);
  v.GridSortedColumnList := TStringList.Create;
  for i:=0 to helpList.Count-1 do
  begin
    col := FindColumnInView(StrToInt(helpList[i]),v );
    if (col <> nil) then v.GridSortedColumnList.AddObject(col.Table.name+'.'+col.name, col);
  end;
  helpList.Clear;
  GetColumns(helpList,v);
  if (helpList.Count <> v.GridSortedColumnList.Count) then
    v.GridSortedColumnList.Assign(helpList);

  //load in the FormSortedColumns property
  fs := node.FormSortedColumns;
  helpList.Clear;
  Split (fs, ',', helpList);
  v.FormSortedColumnList := TStringList.Create;
  for i:=0 to helpList.Count-1 do
  begin
    col := FindColumnInView(StrToInt(helpList[i]),v );
    if (col <> nil) then v.FormSortedColumnList.AddObject(col.Table.name+'.'+col.name, col);
  end;
  helpList.Clear;
  GetColumns(helpList,v);
  if (helpList.Count <> v.FormSortedColumnList.Count) then
    v.FormSortedColumnList.Assign(helpList);

  helpList.Free;

  LoadFromXMLNode := v;
  except
  on ETableDoesntExistException do LoadFromXMLNOde := nil;
  end;
end;

class function TView.FindColumnInView(obj_id: Integer; view: TView) : SW_Column;
var i, j: Integer;
    tmpTable: SW_Table;
begin
  FindColumnInView := nil;

  for i:=0 to view.Table.columns.Count-1 do
  begin
    if (SW_Column(view.Table.columns[i]).Obj_id = obj_id) then
    begin
      FindColumnInView :=SW_Column(view.Table.columns[i]);
      exit;
    end;
  end;

  for i:= 0 to view.JoinTables.Count-1 do
  begin
    tmpTable := SW_Table(view.JoinTables[i]);
    for j:= 0 to tmpTable.columns.Count-1 do
    begin
      if (SW_Column(tmpTable.columns[j]).Obj_id = obj_id) then
      begin
        FindColumnInView :=SW_Column(tmpTable.columns[j]);
        exit;
      end;
    end;
  end;

  for i:= 0 to view.NMTables.Count-1 do
  begin
    tmpTable := SW_Table(view.NMTables[i]);
    for j:= 0 to tmpTable.columns.Count-1 do
    begin
      if (SW_Column(tmpTable.columns[j]).Obj_id = obj_id) then
      begin
        FindColumnInView :=SW_Column(tmpTable.columns[j]);
        exit;
      end;
    end;
  end;
end;

function GetTablename(qual_col: String) :String;
var pos: Integer;
begin
  pos := AnsiPos('.', qual_col);
  assert (pos <> 0);

  GetTablename :=MidStr(qual_col,1, pos-1);
end;

function tablenameExists(name: String; tableList : TObjectList) : Boolean;
var i : Integer;
    found : Boolean;
begin
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

function tablenameExistsInColList(tableName :String; sortedColList :TStringList) : Boolean;
var i :integer;
begin
  tablenameExistsinColList := false;

  for i:=0 to sortedcolList.Count-1 do
  begin
    if (tableName = GetTAblename(sortedColList[i]) ) then
    begin
      tablenameExistsinColList := true;
      exit;
    end;
  end;

end;

procedure TView.RebuildSortColumns();
var i,j: Integer;
    tablename : String;
    mainTableDidntChange :Boolean;
    col : SW_Column;
    tmpTable : SW_Table;
begin
  //--handle the GridSortedColumnList first--//
  mainTableDidntChange := false;
  //first delete all columns in gridSortedColumnList that don't exist any longer
  i:= 0;
  while (i < self.GridSortedColumnList.Count) do
  begin
    tablename := GetTablename(GridSortedColumnList[i]);
    inc(i);
    if (tablename = Table.name) then
    begin
      mainTableDidntChange := true;
      continue;
    end
    else if (tablenameExists(tablename,self.JoinTables)) then continue
    else if (tablenameExists(tablename,self.NMTables)) then continue
    else
    begin
      dec(i);
      self.GridSortedColumnList.Delete(i);
    end;
  end;

  //now add all columns that are still missing in GridSortedColumnList
  if (MainTabledidntChange = false) then
  begin
    for i:=0 to self.Table.Columns.Count-1 do
    begin
      col := SW_Column(table.Columns[i]);
      GridSortedColumnList.AddObject(col.Table.name+'.'+col.name, table.Columns[i]);
    end;
  end;
  for i:=0 to self.JoinTables.Count-1 do
  begin
    tmpTable := SW_Table(JoinTables[i]);
    if (tablenameExistsInColList(tmpTable.name,self.GridSortedColumnList) ) then continue;
    for j:=0 to tmpTable.Columns.Count-1 do
    begin
      col := SW_Column(tmpTable.Columns[j]);
      GridSortedColumnList.AddObject(col.Table.name+'.'+col.name, tmpTable.Columns[j]);
    end;
  end;
  for i:=0 to self.NMTables.Count-1 do
  begin
    tmpTable := SW_Table(NMTables[i]);
    if (tablenameExistsInColList(tmpTable.name,self.GridSortedColumnList) ) then continue;
    for j:=0 to tmpTable.Columns.Count-1 do
    begin
      col := SW_Column(tmpTable.Columns[j]);
      GridSortedColumnList.AddObject(col.Table.name+'.'+col.name, tmpTable.Columns[j]);
    end;
  end;


  //--now handle the FormSortedColumnList--//
  mainTableDidntChange := false;
  //first delete all columns in formSortedColumnList that don't exist any longer
  i:=0;
  while (i < self.FormSortedColumnList.Count) do
  begin
    tablename := GetTablename(FormSortedColumnList[i]);
    inc(i);
    if (tablename = Table.name) then
    begin
      mainTableDidntChange := true;
      continue;
    end
    else if (tablenameExists(tablename,self.JoinTables)) then continue
    else if (tablenameExists(tablename,self.NMTables)) then continue
    else
    begin
      dec(i);
      self.FormSortedColumnList.Delete(i);
    end;
  end;

  //now add all columns that are still missing in FormSortedColumnList
  if (MainTabledidntChange = false) then
  begin
    for i:=0 to self.Table.Columns.Count-1 do
    begin
      col := SW_Column(table.Columns[i]);
      FormSortedColumnList.AddObject(col.Table.name+'.'+col.name, table.Columns[i]);
    end;
  end;
  for i:=0 to self.JoinTables.Count-1 do
  begin
    tmpTable := SW_Table(JoinTables[i]);
    if (tablenameExistsInColList(tmpTable.name,self.FormSortedColumnList) ) then continue;
    for j:=0 to tmpTable.Columns.Count-1 do
    begin
      col := SW_Column(tmpTable.Columns[j]);
      FormSortedColumnList.AddObject(col.Table.name+'.'+col.name, tmpTable.Columns[j]);
    end;
  end;
  for i:=0 to self.NMTables.Count-1 do
  begin
    tmpTable := SW_Table(NMTables[i]);
    if (tablenameExistsInColList(tmpTable.name,self.FormSortedColumnList) ) then continue;
    for j:=0 to tmpTable.Columns.Count-1 do
    begin
      col := SW_Column(tmpTable.Columns[j]);
      FormSortedColumnList.AddObject(col.Table.name+'.'+col.name, tmpTable.Columns[j]);
    end;
  end;

end;

//checks if the tables in Jointables and NMtables
//are real JoinTables/NMTables (they may not be if
//the user changes the model in DBD4 after saving
//the swf-configs)
function TView.RelationsValid() : Boolean;
var i: Integer;
    realNMTables: TStringList;
begin
  RelationsValid := true;

  for i:=0 to self.JoinTables.Count-1 do
  begin
    if (table.RelatedTables.IndexOf( SW_Table(JoinTables[i]).name ) = -1) then
    begin
      RelationsValid := false;
      exit;
    end;
  end;

  realNmTables:= TStringList.Create;
  try
  Model.GetAllNMRelations(table.name, realNMTables);
  for i:=0 to self.NMTables.Count-1 do
  begin
    if (realNMTables.IndexOf( SW_Table(NMTables[i]).Name) = -1) then
    begin
      RelationsValid := false;
      exit;
    end;
  end;
  finally
    realnmTables.Free;
  end;
end;

//returns the Columns from all tables in a stringlist consisting of (name,SW_Column) pairs
//moreover the stringlist is ordered after the way the columns shall appear on the GridList
procedure TView.GetGridSortedColumns(stringList: TStringList);
begin
  stringList.Assign(self.GridSortedColumnList);
end;

//returns the Columns from all tables in a stringlist consisting of (name,SW_Column) pairs
//moreover the stringlist is ordered after the way the columns shall appear on the Form
procedure TView.GetFormSortedColumns(stringList: TStringList);
begin
  stringList.Assign(self.FormSortedColumnList);
end;

procedure TView.ExchangeGridSortedColumns(ind1, ind2 :Integer);
begin
  GridSortedColumnList.Exchange(ind1,ind2);
end;
procedure TView.ExchangeFormSortedColumns(ind1, ind2 :Integer);
begin
  FormSortedColumnList.Exchange(ind1,ind2);
end;

constructor TView.Create(Viewname, WhereClause :String; Table : SW_Table;
                        JoinTables, NMTables: TObjectList;IconImage: TPicture;
                        IconFilename, OrderBy: String; RowsPerPage:Integer);
var i: Integer;
begin
  inherited Create();

  Name:= Viewname;
  self.WhereClause:=WhereClause;
  self.RowsPerPage:= RowsPerPage; //default value
  FormHeight := 400;
  FormWidth := 600;
  FormX := 50;
  FormY := 50;
  GridPopupWidth := 800;
  GridPopupHeight := 600;
  GridPopupX := 200;
  GridPopupY := 100;
  self.OrderBy := OrderBy;
  
  FIcon := TPicture.Create;
  FIcon.Assign(IconImage);
  FIconFilename := IconFilename;
  self.Table := Table.DeepCopy;
  FJoinTables := TObjectList.Create();
  FNMTables := TObjectList.Create();

  if (JoinTables <> nil) then
  begin
    for i:=0 to JoinTables.Count-1 do
    begin
      self.JoinTables.Add(SW_Table(JoinTables[i]).DeepCopy() );
    end;
  end;

  if (NMTables <> nil) then
  begin
    for i:=0 to NMTables.Count-1 do
    begin
      self.NMTables.Add(SW_Table(NMTables[i]).DeepCopy() );
    end;
  end;

  GridSortedColumnList:= TStringList.Create;
  GetColumns(GridSortedColumnList);
  FormSortedColumnList:= TStringList.Create;
  GetColumns(FormSortedColumnList);
end;


//not for public use
constructor TView.Create();
begin
  inherited Create();
end;

destructor TView.Destroy;
begin
  JoinTables.Free;
  NMTables.Free;
  Icon.Free;

  GridSortedColumnList.Free;
  FormSortedColumnList.Free;
end;




function TView.OneGridColVisible() : Boolean;
var stringList: TStringList;
    i: Integer;
    col : SW_Column;
begin
  OneGridColVisible:= false;
  stringList := TStringList.Create;

  self.GetColumns(stringList);  //get all columns
  for i:= 0 to stringList.Count-1 do
  begin
    col := SW_Column(stringList.Objects[i]);
    if (col.selectedForGrid) then
    begin
      OneGridColVisible:= True;
      break;
    end;
  end;

  stringList.Free;
end;



procedure TView.GetColumns(stringList: TStringList);
var i, j: Integer;
    name : String;
    tmpTable: SW_Table;
begin
  for i:=0 to Table.columns.Count-1 do
  begin
    name := SW_Column(Table.columns[i]).table.name+'.'+ SW_Column(Table.columns[i]).name;
    stringList.AddObject( name , Table.columns[i] );
  end;

  for i:= 0 to JoinTables.Count-1 do
  begin
    tmpTable := SW_Table(JoinTables[i]);
    for j:= 0 to tmpTable.columns.Count-1 do
    begin
      name := SW_Column(tmpTable.columns[j]).table.name+'.'+ SW_Column(tmpTable.columns[j]).name;
      stringList.AddObject( name , tmpTable.columns[j] );
    end;
  end;

  for i:= 0 to NMTables.Count-1 do
  begin
    tmpTable := SW_Table(NMTables[i]);
    for j:= 0 to tmpTable.columns.Count-1 do
    begin
      name := SW_Column(tmpTable.columns[j]).table.name+'.'+ SW_Column(tmpTable.columns[j]).name;
      stringList.AddObject( name , tmpTable.columns[j] );
    end;
  end;

end;

class procedure TView.GetColumns(stringList: TStringList; view:TView);
var i, j: Integer;
    name : String;
    tmpTable: SW_Table;
begin
  for i:=0 to view.Table.columns.Count-1 do
  begin
    name := SW_Column(view.Table.columns[i]).table.name+'.'+ SW_Column(view.Table.columns[i]).name;
    stringList.AddObject( name , view.Table.columns[i] );
  end;

  for i:= 0 to view.JoinTables.Count-1 do
  begin
    tmpTable := SW_Table(view.JoinTables[i]);
    for j:= 0 to tmpTable.columns.Count-1 do
    begin
      name := SW_Column(tmpTable.columns[j]).table.name+'.'+ SW_Column(tmpTable.columns[j]).name;
      stringList.AddObject( name , tmpTable.columns[j] );
    end;
  end;

  for i:= 0 to view.NMTables.Count-1 do
  begin
    tmpTable := SW_Table(view.NMTables[i]);
    for j:= 0 to tmpTable.columns.Count-1 do
    begin
      name := SW_Column(tmpTable.columns[j]).table.name+'.'+ SW_Column(tmpTable.columns[j]).name;
      stringList.AddObject( name , tmpTable.columns[j] );
    end;
  end;

end;





//*************************************************************
//**Implementations for TWeboutput*****************************
//*************************************************************

constructor TWeboutput.Create;
begin
  inherited Create;
  GroupList := TObjectList.Create;
  UnassignedViewsList := TObjectList.Create;
end;


destructor TWeboutput.Destroy;
begin
  GroupList.Free; //frees the list including all objects it contains
  UnassignedViewsList.Free;
  inherited Destroy;
end;

procedure TWeboutput.LoadFromXMLNode(node :IXMLSWF_DataType);
var groups :IXMLSWF_GroupsType;
    group : IXMLSWF_GroupType;
    realGroup : TGroup;
    views :IXMLUnassignedViewsType;
    view: Ixmlswf_ViewType;
    realView :TView;
    i: Integer;
begin
  assert(node.Hostname <> '');

  hostname := node.Hostname;
  username := node.Username;
  databasename := node.databaseName;
  heading := node.Heading;
  layout := node.Layout;
  {$IFDEF MSWINDOWS}
    saveDir := node.WinSaveDir;
  {$ELSE}
    saveDir := node.LinuxSaveDir;
  {$ENDIF}

  groups := node.SWF_Groups;
  GroupList := TObjectList.Create;

  for i:= 0 to groups.Count-1 do
  begin
    group := groups[i];
    realGroup := TGroup.LoadFromXMLNode(group);
    GroupList.Add(realGroup);
  end;

  views := node.UnassignedViews;
  self.UnassignedViewsList := TObjectList.Create;

  for i:= 0 to views.Count-1 do
  begin
    view := views[i];
    realView := TView.LoadFromXMLNode(view);
    if (realView <> nil) then
      UnassignedViewsList.Add(realView);
  end;
end;

procedure TWeboutput.SaveToXmlNode(node :IXMLSWF_DataType);
var i: Integer;
    xmlGroup : IXMLSWF_GroupType;
    group :TGroup;
    xmlView :IXMLSWF_ViewType;
    view :TView;
begin
  node.hostname := hostname;
  node.Username := username;
  node.databasename := databasename;
  node.heading := heading;
  node.layout := layout;
  {$IFDEF MSWINDOWS}
    node.WinSavedir := SaveDir;
  {$ELSE}
    node.LinuxSaveDir := SaveDir;
  {$ENDIF}

  for i:=0 to self.GroupList.Count-1 do
  begin
    xmlGroup := node.SWF_Groups.Add;
    group := TGroup(GroupList[i]);
    group.SaveToXMLNode(xmlGroup);
  end;

  for i:= 0 to self.UnassignedViewsList.Count-1  do
  begin
    xmlView := node.UnassignedViews.Add;
    view := TView(UnassignedViewsList[i]);
    view.SaveToXMLNode(xmlView);
  end;
end;

procedure TWeboutput.SetSaveDir(s: String);
begin
  FsaveDir := IncludeTrailingPathDelimiter(s);
  if (NOT DirectoryExists(FSaveDir)) THEN
    FdirDoesntExist := True
  else
    FdirDoesntExist := False;
end;


//**********************************
//Group/View -  Mangement functions*
//**********************************

procedure TWeboutput.AddGroup(group: TGroup);
begin
  GroupList.Add(group);
end;

procedure TWeboutput.DeleteGroup(group: TGroup);
begin
  GroupList.Remove(group);
end;

function TWeboutput.GroupPositionFree(lineNo, colNo : Integer) : Boolean;
var i : Integer;
    tmpGrp : TGroup;
begin

  GroupPositionFree := true;

  for i:=0 to GroupList.Count-1 do
  begin
    tmpGrp := TGroup(GroupList[i]);
    if ( (tmpGrp.showOnLine=lineNo) and (tmpGrp.showInColumn=colNo) ) then
    begin
      GroupPositionFree := false;
      break;
    end;
  end;

end;

function TWeboutput.GetGroupAtPosition(lineNo, ColNo : Integer) : TGroup;
var i : Integer;
    tmpGrp : TGroup;
begin
  GetGroupAtPosition := nil;

  for i:=0 to GroupList.Count-1 do
  begin
    tmpGrp := TGroup(GroupList[i]);
    if ( (tmpGrp.showOnLine=lineNo) and (tmpGrp.showInColumn=colNo)) then
    begin
      GetGroupAtPosition := tmpGrp;
      exit;
    end;
  end;

end;

//adds a view to the list of unassigned views
procedure TWeboutput.AddView(view: TView);
begin
  UnassignedViewsList.Add(view);
end;


procedure TWeboutput.DeleteView(view: TView);
var i,j: Integer;
    views: TList;
begin
  for i:=0 to UnassignedViewsList.Count-1 do
  begin
    if (UnassignedViewsList[i] = view) then
    begin
      UnassignedViewsList.Delete(i);
      Exit;
    end;
  end;
  for i:=0 to GroupList.Count-1 do
  begin
    views := TGroup(GroupList[i]).Views;
    for j:=0 to views.Count-1 do
    begin
      if (views[j] = view) then
      begin
        views.Delete(j);
        Exit;
      end;
    end;
  end;
end;

procedure TWeboutput.AssignViewToGroup(group :TGroup; view: TView);
begin
  self.UnassignedViewsList.Extract(view);
  group.Views.Add(view);
end;

procedure TWeboutput.UnassignViewFromGroup(group :TGroup; view :TView);
begin
  group.Views.Extract(view);
  UnassignedViewsList.Add(view);
end;



//returns a stringlist with associated objects
procedure TWeboutput.GetGroupNames(stringList : TStringList);
var i: Integer;
    group: TGroup;
begin

  for i:=0 to GroupList.Count-1 do
  begin
    group := TGroup(GroupList.Items[i]);
    stringList.AddObject(group.Name, group);
  end;

end;


//paired stringList
procedure TWeboutput.GetViewNamesFromGroup(group: TGroup; stringList :TStringList);
var j : Integer;
    viewList: TObjectList;
begin

  viewList:= group.Views;
  for j:= 0 to viewList.Count-1 do
    stringList.AddObject(TView(viewList[j]).name, viewList[j]);

end;


procedure TWeboutput.GetUnassignedViewNames(stringList : TStringList);
var i: Integer;
begin

  for i:= 0 to self.UnassignedViewsList.Count-1 do
  begin
    stringList.AddObject(TView(UnassignedViewsList[i]).Name, UnassignedViewsList[i]);
  end;

end;


procedure TWeboutput.GetViewNames(stringList: TStringList);
var i,j: Integer;
begin
  GetUnassignedViewNames(stringList);

  for i:=0 to Grouplist.Count-1 do
  begin
    for j:= 0 to TGroup(Grouplist[i]).Views.Count-1 do
    begin
      stringList.AddObject(TView(TGroup(Grouplist[i]).Views[j]).Name, TGroup(Grouplist[i]).Views[j]);
    end;
  end;

end;







function TWeboutput.InputComplete() : Boolean;
var i,j:Integer;
    tmpGroup: TGroup;
    tmpView: TView;
begin

  if (databasename = '') then
  begin
    InputComplete:=false;
    Exit;
  end;
  if (heading = '') then
  begin
    InputComplete:=false;
    Exit;
  end;
  if (hostname = '') then
  begin
    InputComplete:=false;
    Exit;
  end;
  if (layout = '') then
  begin
    InputComplete:=false;
    Exit;
  end;
  if (password = '') then
  begin
    InputComplete:=false;
    Exit;
  end;
  if (saveDir = '') then
  begin
    InputComplete:=false;
    Exit;
  end;
  if (username = '') then
  begin
    InputComplete:=false;
    Exit;
  end;

  InputComplete:= false;
  //at least 1 assigned view has to exist   (so 1 group has to exist too)
  //and moreover this view has to have at least 1 grid-column visible
  //*WARNING: THIS HAS TO BE THE LAST CHECK*
  for i:=0 to Grouplist.Count-1 do
  begin
    tmpGroup:= TGroup(Grouplist[i]);

    for j:= 0 to tmpGroup.Views.Count-1 do
    begin
      tmpView := TView(tmpGroup.Views[j]);
      if (tmpView.OneGridColVisible()) then
      begin
        InputComplete := true;
        Exit;
      end;
    end;
    
  end;

end;



//*************************************************************
//**Implementations for TPhpoutput*****************************
//*************************************************************

procedure TPhpoutput.CreateIncludeFile();
var F: TextFile;
begin
  try
  AssignFile(F,self.SaveDir+'db_open.php');
  Rewrite(F);
  Writeln(F,'<?php');
  Writeln(F,'$host = "'+hostname+'";');
  Writeln(F,'$user = "'+username+'";');
  Writeln(F,'$password = "'+password+'";');
  Writeln(F,'$database = "'+databasename+'";');
  Writeln(F,'/* Connect to the db and select a database*/');
  Writeln(F,'$dbLink  = mysql_connect($host,$user, $password) or die("Couldn''t connect to the database!");');
  Writeln(F,'$success = mysql_select_db($database) or die("Error selecting the database! <br> ".mysql_error());');
  Writeln(F,'?>');
  finally
    Close(F);
  end;
end;


procedure TPhpoutput.Run();
var a:Integer;
    LayoutFilepath,GridTemplateFilename: string;
    NeededFilesString, NeededDirsString : String;
    tmp,IndexTemplateFilename,SQLTemplateFilename, UpdateTemplateFilename: string;
    InsertTemplateFilename,FrameTemplateFilename,SearchTemplateFilename :string;
    layoutSettings : TIniFile;
    neededDirs, neededFiles : TStringList;
begin
  //find out more about the layout we are using

  LayoutFilepath:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + DataDir+layout+PathDelim;
  LayoutSettings:= TIniFile.Create(LayoutFilepath+layout+'.ini');

  IndexTemplateFilename:= layoutSettings.ReadString('General','IndexTemplateFilename','');
  FrameTemplateFilename:= layoutSettings.ReadString('General','FrameTemplateFilename','');
  GridTemplateFilename:= layoutSettings.ReadString('General','GridTemplateFilename','');
  UpdateTemplateFilename:= layoutSettings.ReadString('General', 'UpdateTemplateFilename','');
  InsertTemplateFilename:= layoutSettings.ReadString('General', 'InsertTemplateFilename','');
  SearchTemplateFilename:= layoutSettings.ReadString('General', 'SearchTemplateFilename','');
  SQLTemplateFilename:= layoutSettings.ReadString('General', 'SQLTemplateFilename','');
  NeededFilesString:= layoutSettings.ReadString('General','NeededFiles','');
  NeededDirsString:= layoutSettings.ReadString('General','NeededDirs','');

  LayoutSettings.Free();

  neededFiles := TStringList.Create;
  neededDirs := TStringList.Create;
  Split(NeededFilesString , ',', neededFiles);
  Split(NeededDirsString  , ',', neededDirs);

  //copy all files
  for a:=0 to neededFiles.Count-1 do
  begin
    DMMain.CopyDiskFile(LayoutFilepath+neededFiles[a],saveDir+neededFiles[a],false);
  end;

  //copy all directories including their files
  for a:=0 to neededDirs.Count-1 do
  begin
    tmp:= neededDirs[a];
    //copy directory including all files includefilepathdelim
    //CopyDir(LayoutFilePath+tmp+PathDelim,tmp,saveDir+tmp);
    DMMain.CopyDirRecursive(LayoutFilePath+tmp, saveDir+tmp, false);
  end;

  neededFiles.Free;
  neededDirs.Free;

  //first we create the include file
  CreateIncludeFile();

  //let's create the index.php file now
  CreateIndexFile(LayoutFilePath+IndexTemplateFilename);

  CreateFrameFiles(LayoutFilePath+FrameTemplateFilename);

  CreateSearchFiles(LayoutFilePath+SearchTemplateFilename);

  //create the grid-files (= tableview for each view)
  CreateGridFiles(LayoutFilepath+GridTemplateFilename);

  if (SQLTemplateFilename <> '') THEN
    CreateSQLFiles(LayoutFilepath+SQLTemplateFilename);

  //create the update-files
  if (UpdateTemplateFilename <> '') then
    CreateUpdateFiles(LayoutFilepath+UpdateTemplateFilename);

  if (InsertTemplateFilename <> '') then
    CreateInsertFiles(LayoutFilepath+InsertTemplateFilename);
  
  SaveIcons();
end;

procedure TPHPOutput.SaveIcons();
var i, j: Integer;
    TmpGroup : TGroup;
    TmpView: TView;
    saveFilename : String;
begin
  for i:=0 to GroupList.Count-1 do
  begin
    TmpGroup:=TGroup(GroupList[i]);

    ForceDirectories(IncludeTrailingPathDelimiter(saveDir)+'images'+PathDelim+'icons');
    //for all views of the group do
    for j:=0 to TmpGroup.Views.Count-1 do
    begin
      TmpView:=TView(TmpGroup.Views[j]);

      saveFilename := IncludeTrailingPathDelimiter(saveDir)+'images'+PathDelim+'icons'+PathDelim+TmpView.IconFilename;
      DMMain.SaveBitmap(TmpView.Icon.Bitmap.Handle, saveFilename, ExtractFileExt(saveFilename));

    end;

  end;
end;

procedure TPHPOutput.CreateFrameFiles(FrameTemplateFilename: string);
var i, j, u,fIndex: Integer;
    TmpGroup : TGroup;
    TmpView: TView;
    saveFilename,str,tmp,gridFilename : String;
    FrameTemplate : TStringList;
    TemplateF, frameFile :   TextFile;
begin
  fIndex:= 0;

  //read in the frame-template file
  FrameTemplate := TStringList.Create;

  if (NOT (FileExists(FrameTemplateFilename)) ) then
  begin
    raise ETemplateFileException.Create(FrameTemplateFilename+','+TemplateFileError);
  end;

  try
    AssignFile(TemplateF,FrameTemplateFilename);
    Reset(TemplateF);
    while not eof(TemplateF) do
    begin
        ReadLn(TemplateF, str);

        //add this line to our stringlist
        i := FrameTemplate.Add(str);
        if (AnsiContainsText(str,GRID_FILENAME_TOKEN) ) then fIndex:= i;
    end;
  finally
    Close(TemplateF);
  end;

  for i:=0 to GroupList.Count-1 do
  begin
    TmpGroup:=TGroup(GroupList[i]);

    //for all views of the group do
    for j:=0 to TmpGroup.Views.Count-1 do
    begin
      TmpView:=TView(TmpGroup.Views[j]);

      gridFilename := TmpGroup.Name+'_'+TmpView.Name+'.php';
      saveFilename := IncludeTrailingPathDelimiter(saveDir)+TmpGroup.Name+'_'+TmpView.Name+'_frame.php';

      //create file for grid-view
      AssignFile(frameFile,saveFilename);
      Rewrite(frameFile);

      Writeln(frameFile, GeneratedFileHeader);

      for u:=0 to FrameTemplate.Count-1 do
      begin
        tmp := FrameTemplate[u];

        if (u=fIndex) then
          tmp := DMMain.ReplaceText(tmp,GRID_FILENAME_TOKEN,'"'+gridFilename+'"');

        Writeln(frameFile, tmp);
      end;

      Close(frameFile);
    end;
  end;

  FrameTemplate.Free;
end;


procedure TPHPOutput.CreateSearchFiles(SearchTemplateFilename: string);
var i, j, u: Integer;
    TmpGroup : TGroup;
    TmpView: TView;
    saveFilename,str,tmp,gridFilename,frameFilename : String;
    SearchTemplate : TStringList;
    TemplateF, pFile :   TextFile;
    gridFilenameIndex,frameFilenameIndex,columnCaptionsIndex,columnsOrderIndex : Integer;
    viewNameIndex :Integer;
begin
  gridFilenameIndex := 0; viewnameIndex:=0;
  frameFilenameIndex := 0;
  columnCaptionsIndex := 0;
  columnsOrderIndex := 0;

  //read in the frame-template file
  SearchTemplate := TStringList.Create;

  if (NOT (FileExists(SearchTemplateFilename)) ) then
  begin
    raise ETemplateFileException.Create(SearchTemplateFilename+','+TemplateFileError);
  end;

  try
    AssignFile(TemplateF,SearchTemplateFilename);
    Reset(TemplateF);
    while not eof(TemplateF) do
    begin
        ReadLn(TemplateF, str);

        //add this line to our stringlist
        i := SearchTemplate.Add(str);
        if (AnsiContainsText(str,VIEWNAME_TOKEN) ) then viewnameIndex := i;
        if (AnsiContainsText(str,GRID_FILENAME_TOKEN) ) then gridFilenameIndex:= i;
        if (AnsiContainsText(str,FRAME_FILENAME_TOKEN) ) then frameFilenameIndex:= i;
        if (AnsiContainsText(str,COLUMN_CAPTIONS_TOKEN) ) then columnCaptionsIndex:= i;
        if (AnsiContainsText(str,COLUMNS_ORDER_TOKEN) ) then columnsOrderIndex:= i;

    end;
  finally
    Close(TemplateF);
  end;

  for i:=0 to GroupList.Count-1 do
  begin
    TmpGroup:=TGroup(GroupList[i]);

    //for all views of the group do
    for j:=0 to TmpGroup.Views.Count-1 do
    begin
      TmpView:=TView(TmpGroup.Views[j]);

      gridFilename := TmpGroup.Name+'_'+TmpView.Name+'.php';
      frameFilename := TmpGroup.Name+'_'+TmpView.name+'_frame.php';
      saveFilename := IncludeTrailingPathDelimiter(saveDir)+TmpGroup.Name+'_'+TmpView.Name+'_search.php';

      AssignFile(pFile,saveFilename);
      Rewrite(pFile);

      Writeln(pFile, GeneratedFileHeader);

      for u:=0 to SearchTemplate.Count-1 do
      begin
        tmp := SearchTemplate[u];

        if (u=gridFilenameIndex) then
          tmp := DMMain.ReplaceText(tmp,GRID_FILENAME_TOKEN,'"'+gridFilename+'"');
        if (u=frameFilenameIndex) then
          tmp := DMMain.ReplaceText(tmp,FRAME_FILENAME_TOKEN,'"'+frameFilename+'"');
        if (u=columnCaptionsIndex) then
          tmp := DMMain.ReplaceText(tmp,COLUMN_CAPTIONS_TOKEN,MakeColumnCaptions(TmpView));
        if (u=columnsOrderIndex) then
          tmp := DMMain.ReplaceText(tmp,COLUMNS_ORDER_TOKEN,MakeGridColumnOrder(TmpView));
        if (u=viewnameIndex) then
          tmp := DMMain.ReplaceText(tmp,VIEWNAME_TOKEN,'"'+TmpView.Name+'"');
        
        Writeln(pFile, tmp);
      end;

      Close(pFile);
    end;
  end;

  SearchTemplate.Free;
end;

function Groupcompare_line(Item1, Item2: Pointer): Integer;
var group1, group2 : TGroup;
begin
  group1 := TGroup(Item1);
  group2 := TGroup(Item2);

  if (group1.showOnLine < group2.showOnLine) then Groupcompare_line:= -1
  else if (group1.showOnLine > group2.showOnLine) then Groupcompare_line:=  +1
  else if (group1.showInColumn < group2.showInColumn) then Groupcompare_line:=  -1
  else if (group1.showInColumn > group2.showInColumn) then Groupcompare_line:=  +1
  else Groupcompare_line:=  0;
end;

function TPHPOutput.MakeMainIndex() :String;
var viewList: String; //a string in the form of a php array : array (viewname1 => linkaddress,...)
    filename, tmp: String;
    currentCol,currentLine,j,maxLine,grpCount: Integer;
    TmpGroup:TGroup;
    TmpView: TView;
    oneGroupAdded: Boolean;
    order: String;
begin
  MakeMainIndex := '';

  tmp:= '';
  GroupList.Sort(GroupCompare_line);
  maxLine := TGroup(GroupList[GroupList.Count-1]).showOnLine;

  grpCount := 0;
  currentLine := 1;
  while (currentLine <= maxLine) do
  begin
    tmp := tmp + '<table border="0"><tr>';
    oneGroupAdded := false;
    currentCol := 1;


    while ((grpCount < GroupList.Count) and (TGroup(GroupList.Items[grpCount]).showOnLine = currentLine)) do       //draw this group
    begin
      TmpGroup := TGroup(GroupList.Items[grpCount]);
      //are we in the right column?
      if (TmpGroup.showInColumn <> currentCol) then
      begin
        tmp := tmp+'<td style="width:100px"></td>';
        currentCol:= currentCol + 1;
        continue;
      end;

      //show the group
      oneGroupAdded := True;
      tmp := tmp + '<td>';

      viewList:= 'array( ';
      //for all views of the group do
      for j:=0 to TmpGroup.Views.Count-1 do
      begin
        TmpView:=TView(TmpGroup.Views[j]);
        order := '';
        if (TmpView.OrderBy <> '') then order:= '&OrderBy=' + TmpView.OrderBy;

        filename:=TmpGroup.Name+'_'+TmpView.Name+'_frame.php';

        if (TmpGroup.FViewsAsPopup) then
          viewList:= viewList + '"'+TmpView.Name+'"' +'=>' + '"javascript: void window.open(' + #39 +
          filename+'?page=0&mode=normal'+order+ ''',' +
            ' '''+DMMain.ReplaceText(TmpView.Name, ' ', '')+''','+
            '''width='+IntToStr(TmpView.FGridPopupWidth)+', '+
            'height='+IntToStr(TmpView.FGridPopupHeight)+', '+
            'left='+IntToStr(TmpView.FGridPopupX)+', '+
            'top='+IntToStr(TmpView.FGridPopupY)+', '+
            'scrollbars=yes, status=no, toolbar=no, resizable=yes, dependent=yes'')",'

        else
          viewList:= viewList + '"'+TmpView.Name+'"' +'=>'+'"'+filename+'?page=0&mode=normal'+order+'",';
      end;
      //delete the last char
      Delete(viewList,Length(viewList),1);

      tmp:= tmp + '<?php displayGroup("'+TmpGroup.Name+'",'+viewList+') ); ?>';
      tmp := tmp + '</td>';

      grpCount := grpCount +1;
      currentCol := currentCol +1;
    end;

    if (NOT oneGroupAdded) then            //just make an empty table, we have to account for the row
    begin
      tmp := tmp + '<td style="height:120px"></td>';
    end;

    tmp := tmp + '</tr></table>';
    currentLine := currentLine+1;
  end;



  MakeMainIndex:= tmp;
end;

procedure TPhpOutput.CreateIndexFile(IndexTemplateFilename: string);
var IndexTemplate: TStringList;
    i: Integer;
    tmp :String;
    F: TextFile;
begin
  IndexTemplate:= TStringList.Create;
  try  
  //read in the index-template file
  if (NOT (FileExists(IndexTemplateFilename)) ) then
  begin
    raise ETemplateFileException.Create(IndexTemplateFilename+','+TemplateFileError);
  end;
  IndexTemplate.LoadFromFile(IndexTemplateFilename);

  AssignFile(F,saveDir+'index.php');
  Rewrite(F);

  Writeln(F, GeneratedFileHeader);

  for i:=0 to IndexTemplate.Count-1 do
  begin
    tmp := IndexTemplate[i];

    if (AnsiStartsStr('//|', tmp)) then continue; //ignore lines beginning with '//|'
    if AnsiContainsText(tmp,INDEX_TOKEN) then
      tmp:= DMMain.ReplaceText(tmp, INDEX_TOKEN, MakeMainIndex);
    if AnsiContainsText(tmp,HEADING_TOKEN) then
      tmp:= DMMain.ReplaceText(tmp, HEADING_TOKEN, '"'+heading+'"');
    if AnsiContainsText(tmp,IMAGES_TOKEN) then
      tmp:= DMMain.ReplaceText(tmp, IMAGES_TOKEN, MakeImageNames());

    Writeln(F,tmp);
  end;
  finally
    Close(F);
    IndexTemplate.Free;
  end;
end;



procedure TPhpOutput.CreateUpdateFiles(UpdateTemplateFilename :string);
var templateFile, updateFile: TextFile;
    str,tmp, filename: String;
    TemplateLines: TStringList;
    i,j,u : Integer;
    tablenameIndex,datatypesIndex, tablekeyIndex, tabledispcolsIndex :Integer;
    jointablenameIndex, jointablekeyIndex, jointabledispcolsIndex :Integer;
    nmtablenameIndex, nmtableKeyIndex, nmtabledispcolsIndex,connectiontablenameIndex : Integer;
    nmtabledatatypesIndex, keyMappingIndex,nmtabledatatypeparamsIndex: Integer;
    colWidthIndex, colCaptIndex, datatypeparamsIndex, MainNotnullIndex : Integer;
    gridFilenameIndex,columnsOrderIndex : Integer;
    TmpGroup: TGroup;
    TmpView: TView;
begin
  tablenameIndex:= 0; datatypesIndex := 0;tablekeyIndex:=0; tabledispcolsIndex:= 0;
  jointablenameIndex:=0; jointablekeyIndex:=0; jointabledispcolsIndex:= 0;
  nmtablenameIndex:= 0; nmtableKeyIndex:=0; nmtabledispcolsIndex:=0;connectiontablenameIndex:=0;
  nmtabledatatypesIndex := 0;keyMappingIndex := 0; datatypeparamsIndex :=0;
  colWidthIndex := 0; colCaptIndex := 0; nmtabledatatypeparamsIndex := 0;
  MainNotNullIndex := 0;  gridFilenameIndex :=0;   columnsOrderIndex:=0;

  TemplateLines:= TStringList.Create;

  try
  //read in the template
  if (NOT (FileExists(UpdateTemplateFilename)) ) then
  begin
    raise ETemplateFileException.Create(UpdateTemplateFilename+','+TemplateFileError);
  end;
  AssignFile(templateFile,UpdateTemplateFilename);
  Reset(templateFile);
  while not eof(templateFile) do
  begin
    ReadLn(templateFile, str);
    if (AnsiContainsText(str,INCLUDE_TOKEN) )then
      str:= DMMain.ReplaceText(str, INCLUDE_TOKEN, 'include "db_open.php"');

    if (AnsiStartsStr('//|', str)) then continue; //ignore lines beginning with '//|'
    i := templateLines.Add(str);

    if (AnsiContainsText(str,COLUMNS_ORDER_TOKEN) ) then columnsOrderIndex:= i;
    if (AnsiContainsText(str,GRID_FILENAME_TOKEN) ) then gridFilenameIndex := i;
    if (AnsiContainsText(str,TABLENAME_TOKEN) ) then tablenameIndex := i;
    if (AnsiContainsText(str,DATATYPES_TOKEN) ) then datatypesIndex := i;
    if (AnsiContainsText(str,DATATYPE_PARAMS_TOKEN) ) then datatypeparamsIndex := i;
    if (AnsiContainsText(str,TABLEKEY_TOKEN) ) then tablekeyIndex := i;
    if (AnsiContainsText(str,TABLEDISPCOLS_TOKEN) ) then tabledispcolsIndex := i;
    if (AnsiContainsText(str,MAIN_NOT_NULL_TOKEN) ) then MainNotNullIndex := i;

    if (AnsiContainsText(str,JOINTABLEDISPCOLS_TOKEN) ) then jointabledispcolsIndex := i;
    if (AnsiContainsText(str,JOINTABLENAME_TOKEN) ) then jointablenameIndex := i;
    if (AnsiContainsText(str,JOINTABLEKEY_TOKEN) ) then jointablekeyIndex := i;

    if (AnsiContainsText(str,NMTABLEDISPCOLS_TOKEN) ) then nmtabledispcolsIndex := i;
    if (AnsiContainsText(str,NMTABLENAME_TOKEN) ) then nmtablenameIndex := i;
    if (AnsiContainsText(str,NMTABLEDATATYPES_TOKEN) ) then nmtabledatatypesIndex := i;
    if (AnsiContainsText(str,NMTABLEDATATYPE_PARAMS_TOKEN) ) then nmtabledatatypeparamsIndex := i;
    if (AnsiContainsText(str,NMTABLEKEY_TOKEN) ) then nmtablekeyIndex := i;
    if (AnsiContainsText(str,CONNECTIONTABLENAME_TOKEN) ) then connectiontablenameIndex := i;

    if (AnsiContainsText(str,KEY_MAPPING_TOKEN) ) then keyMappingIndex := i;
    if (AnsiContainsText(str,COLUMN_WIDTHS_TOKEN) ) then colWidthIndex := i;
    if (AnsiContainsText(str,COLUMN_CAPTIONS_TOKEN) ) then colCaptIndex := i;
  end;
  finally
    Close(templateFile);
  end;

  //for all groups
  for i:=0 to GroupList.Count-1 do
  begin
    TmpGroup:=TGroup(GroupList[i]);

    //for all views of the group do
    for j:=0 to TmpGroup.Views.Count-1 do
    begin
      TmpView:=TView(TmpGroup.Views[j]);
      filename:=TmpGroup.Name+'_'+TmpView.Name+'_update.php';

      try
        //create the update-file for each view
        AssignFile(updateFile,saveDir+filename);
        Rewrite(updateFile);

        Writeln(updateFile, GeneratedFileHeader);

        for u:=0 to TemplateLines.Count-1 do
        begin
          tmp := TemplateLines[u];

          if (u= columnsOrderIndex) then
            tmp := DMMain.ReplaceText(tmp, COLUMNS_ORDER_TOKEN, MakeColumnOrder(TmpView));
          if (u= gridFilenameIndex) then
            tmp := DMMain.ReplaceText(tmp, GRID_FILENAME_TOKEN, '"'+TmpGroup.Name+'_'+TmpView.Name+'.php"');
          if (u=tablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,TABLENAME_TOKEN,'"'+TmpView.Table.Name+'"');
          if (u=datatypesIndex) then
            tmp := DMMain.ReplaceText(tmp,DATATYPES_TOKEN, MakeDataTypes(TmpView.Table));
          if (u=datatypeparamsIndex) then
            tmp := DMMain.ReplaceText(tmp,DATATYPE_PARAMS_TOKEN, MakeDataTypeParams(TmpView.Table));
          if (u=tablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,TABLEKEY_TOKEN,MakeKey(TmpView.Table));
          if (u=tabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,TABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.Table, 1));
          if (u=mainNotNullIndex) then
            tmp := DMMain.ReplaceText(tmp,MAIN_NOT_NULL_TOKEN, MakeNotNull(TmpView.Table));

          if (u=keyMappingIndex) then
            tmp:= DMMain.ReplaceText(tmp, KEY_MAPPING_TOKEN, MakeKeyMapping(TmpView));
          if (u=colCaptIndex) then
            tmp:= DMMain.ReplaceText(tmp, COLUMN_CAPTIONS_TOKEN, MakeFormColumnCaptions(TmpView));
          if (u=colWidthIndex) then
            tmp:= DMMain.ReplaceText(tmp, COLUMN_WIDTHS_TOKEN, MakeFormColumnWidths(TmpView));

          if (Tmpview.JoinTables.Count <> 0) then
          begin
            if (u=jointablenameIndex) then
              tmp := DMMain.ReplaceText(tmp,JOINTABLENAME_TOKEN, MakeTableNames(TmpView.JoinTables) );
            if (u=jointablekeyIndex) then
              tmp := DMMain.ReplaceText(tmp,JOINTABLEKEY_TOKEN,MakeKey(TmpView.JoinTables));
            if (u=jointabledispcolsIndex) then
              tmp := DMMain.ReplaceText(tmp,JOINTABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.JoinTables, 1) );
          end
          else
          begin
            if (u=jointablenameIndex) then
              tmp := DMMain.ReplaceText(tmp,JOINTABLENAME_TOKEN, 'array ()');
            if (u=jointablekeyIndex) then
              tmp := DMMain.ReplaceText(tmp,JOINTABLEKEY_TOKEN,'array ()');
            if (u=jointabledispcolsIndex) then
              tmp := DMMain.ReplaceText(tmp,JOINTABLEDISPCOLS_TOKEN, 'array ()');
          end;

          if (TmpView.NMTables.Count <> 0) then
          begin
            if (u=nmtablenameIndex) then
              tmp := DMMain.ReplaceText(tmp,NMTABLENAME_TOKEN, MakeTableNames(TmpView.NMTables) );
            if (u=nmtablekeyIndex) then
              tmp := DMMain.ReplaceText(tmp,NMTABLEKEY_TOKEN,MakeKey(TmpView.NMTables));
            if (u=nmtabledatatypesIndex) then
              tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPES_TOKEN, MakeDataTypes(TmpView.NMTables) );
            if (u=nmtabledatatypeparamsIndex) then
              tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPE_PARAMS_TOKEN, MakeDataTypeParams(TmpView.NMTables) );
            if (u=nmtabledispcolsIndex) then
              tmp := DMMain.ReplaceText(tmp,NMTABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.NMTables, 1) );
            if (u=connectiontablenameIndex) then
              tmp := DMMain.ReplaceText(tmp,CONNECTIONTABLENAME_TOKEN, MakeConnectionTables(TmpView.Table,TmpView.NMTables) );
          end
          else
          begin
            if (u=nmtablenameIndex) then
              tmp := DMMain.ReplaceText(tmp,NMTABLENAME_TOKEN, 'array ()');
            if (u=nmtablekeyIndex) then
              tmp := DMMain.ReplaceText(tmp,NMTABLEKEY_TOKEN,'array ()');
            if (u=nmtabledatatypesIndex) then
              tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPES_TOKEN, 'array ()');
            if (u=nmtabledatatypeparamsIndex) then
              tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPE_PARAMS_TOKEN, 'array ()');
            if (u=nmtabledispcolsIndex) then
              tmp := DMMain.ReplaceText(tmp,NMTABLEDISPCOLS_TOKEN, 'array ()');
            if (u=connectiontablenameIndex) then
              tmp := DMMain.ReplaceText(tmp,CONNECTIONTABLENAME_TOKEN, 'array ()');
          end;

          Writeln(updateFile,tmp);
        end;
      finally
        Close(updateFile);
      end;
    end;
  end;
  
  TemplateLines.Free;
end;



procedure TPhpOutput.CreateInsertFiles(InsertTemplateFilename :string);
var templateFile, insertFile: TextFile;
    str,tmp, filename: String;
    TemplateLines: TStringList;
    i,j,u : Integer;
    tablenameIndex,datatypesIndex, tablekeyIndex, tabledispcolsIndex :Integer;
    jointablenameIndex, jointablekeyIndex, jointabledispcolsIndex :Integer;
    nmtablenameIndex, nmtableKeyIndex, nmtabledispcolsIndex,connectiontablenameIndex : Integer;
    nmtabledatatypesIndex, keyMappingIndex,nmtabledatatypeparamsIndex : Integer;
    colWidthIndex, colCaptIndex,datatypeparamsIndex,columnsOrderIndex : Integer;
    mainDefaultValueIndex, mainNotNullIndex,gridFilenameIndex : Integer;
    TmpGroup: TGroup;
    TmpView: TView;
begin
  tablenameIndex:= 0; datatypesIndex := 0;tablekeyIndex:=0; tabledispcolsIndex:= 0;
  jointablenameIndex:=0; jointablekeyIndex:=0; jointabledispcolsIndex:= 0;
  nmtablenameIndex:= 0; nmtableKeyIndex:=0; nmtabledispcolsIndex:=0;connectiontablenameIndex:=0;
  nmtabledatatypesIndex := 0;keyMappingIndex := 0;  datatypeparamsIndex := 0;
  colWidthIndex := 0; colCaptIndex := 0;   nmtabledatatypeparamsIndex := 0;
  mainDefaultValueIndex := 0; mainNotNullIndex := 0;  gridFilenameIndex :=0;
  columnsOrderIndex:=0;

  TemplateLines:= TStringList.Create;

  //read in the template
  if (NOT (FileExists(InsertTemplateFilename)) ) then
  begin
    raise ETemplateFileException.Create(InsertTemplateFilename+','+TemplateFileError);
  end;
  AssignFile(templateFile,InsertTemplateFilename);
  Reset(templateFile);
  while not eof(templateFile) do
  begin
    ReadLn(templateFile, str);
    if (AnsiContainsText(str,INCLUDE_TOKEN) )then
      str:= DMMain.ReplaceText(str, INCLUDE_TOKEN, 'include "db_open.php"');

    if (AnsiStartsStr('//|', str)) then continue; //ignore lines beginning with '//|'
    i := templateLines.Add(str);

    if (AnsiContainsText(str,COLUMNS_ORDER_TOKEN) ) then columnsOrderIndex:= i;
    if (AnsiContainsText(str,GRID_FILENAME_TOKEN) ) then gridFilenameIndex:= i;
    if (AnsiContainsText(str,TABLENAME_TOKEN) ) then tablenameIndex := i;
    if (AnsiContainsText(str,DATATYPES_TOKEN) ) then datatypesIndex := i;
    if (AnsiContainsText(str,DATATYPE_PARAMS_TOKEN) ) then datatypeparamsIndex := i;
    if (AnsiContainsText(str,TABLEKEY_TOKEN) ) then tablekeyIndex := i;
    if (AnsiContainsText(str,TABLEDISPCOLS_TOKEN) ) then tabledispcolsIndex := i;
    if (AnsiContainsText(str,MAIN_DEFAULT_VALUE_TOKEN) ) then mainDefaultValueIndex := i;
    if (AnsiContainsText(str,MAIN_NOT_NULL_TOKEN) ) then MainNotNullIndex := i;

    if (AnsiContainsText(str,JOINTABLEDISPCOLS_TOKEN) ) then jointabledispcolsIndex := i;
    if (AnsiContainsText(str,JOINTABLENAME_TOKEN) ) then jointablenameIndex := i;
    if (AnsiContainsText(str,JOINTABLEKEY_TOKEN) ) then jointablekeyIndex := i;

    if (AnsiContainsText(str,NMTABLEDISPCOLS_TOKEN) ) then nmtabledispcolsIndex := i;
    if (AnsiContainsText(str,NMTABLENAME_TOKEN) ) then nmtablenameIndex := i;
    if (AnsiContainsText(str,NMTABLEDATATYPES_TOKEN) ) then nmtabledatatypesIndex := i;
    if (AnsiContainsText(str,NMTABLEDATATYPE_PARAMS_TOKEN) ) then nmtabledatatypeparamsIndex := i;
    if (AnsiContainsText(str,NMTABLEKEY_TOKEN) ) then nmtablekeyIndex := i;
    if (AnsiContainsText(str,CONNECTIONTABLENAME_TOKEN) ) then connectiontablenameIndex := i;

    if (AnsiContainsText(str,KEY_MAPPING_TOKEN) ) then keyMappingIndex := i;
    if (AnsiContainsText(str,COLUMN_WIDTHS_TOKEN) ) then colWidthIndex := i;
    if (AnsiContainsText(str,COLUMN_CAPTIONS_TOKEN) ) then colCaptIndex := i;
  end;
  Close(templateFile);

  //for all groups
  for i:=0 to GroupList.Count-1 do
  begin
    TmpGroup:=TGroup(GroupList[i]);

    //for all views of the group do
    for j:=0 to TmpGroup.Views.Count-1 do
    begin
      TmpView:=TView(TmpGroup.Views[j]);
      filename:= TmpGroup.Name+'_'+TmpView.Name+'_insert.php';

      //create the insert-file for each view
      AssignFile(insertFile,saveDir+filename);
      Rewrite(insertFile);

      Writeln(insertFile, GeneratedFileHeader);

      for u:=0 to TemplateLines.Count-1 do
      begin
        tmp := TemplateLines[u];

        if (u= columnsOrderIndex) then
          tmp := DMMain.ReplaceText(tmp, COLUMNS_ORDER_TOKEN, MakeColumnOrder(TmpView));
        if (u= gridFilenameIndex) then
          tmp := DMMain.ReplaceText(tmp, GRID_FILENAME_TOKEN, '"'+TmpGroup.Name+'_'+TmpView.Name+'.php"');
        if (u=tablenameIndex) then
          tmp := DMMain.ReplaceText(tmp,TABLENAME_TOKEN,'"'+TmpView.Table.Name+'"');
        if (u=datatypesIndex) then
          tmp := DMMain.ReplaceText(tmp,DATATYPES_TOKEN, MakeDataTypes(TmpView.Table));
        if (u=datatypeParamsIndex) then
          tmp := DMMain.ReplaceText(tmp,DATATYPE_PARAMS_TOKEN, MakeDataTypeParams(TmpView.Table));
        if (u=tablekeyIndex) then
          tmp := DMMain.ReplaceText(tmp,TABLEKEY_TOKEN,MakeKey(TmpView.Table));
        if (u=tabledispcolsIndex) then
          tmp := DMMain.ReplaceText(tmp,TABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.Table, 1));
        if (u=mainDefaultValueIndex) then
          tmp := DMMain.ReplaceText(tmp,MAIN_DEFAULT_VALUE_TOKEN, MakeDefaultValues(TmpView.Table));
        if (u=mainNotNullIndex) then
          tmp := DMMain.ReplaceText(tmp,MAIN_NOT_NULL_TOKEN, MakeNotNull(TmpView.Table));

        if (u=keyMappingIndex) then
          tmp:= DMMain.ReplaceText(tmp, KEY_MAPPING_TOKEN, MakeKeyMapping(TmpView));
        if (u=colCaptIndex) then
          tmp:= DMMain.ReplaceText(tmp, COLUMN_CAPTIONS_TOKEN, MakeFormColumnCaptions(TmpView));
        if (u=colWidthIndex) then
          tmp:= DMMain.ReplaceText(tmp, COLUMN_WIDTHS_TOKEN, MakeFormColumnWidths(TmpView));

        if (Tmpview.JoinTables.Count <> 0) then
        begin
          if (u=jointablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLENAME_TOKEN, MakeTableNames(TmpView.JoinTables) );
          if (u=jointablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEKEY_TOKEN,MakeKey(TmpView.JoinTables));
          if (u=jointabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.JoinTables, 1) );
        end
        else
        begin
          if (u=jointablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLENAME_TOKEN, 'array ()');
          if (u=jointablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEKEY_TOKEN,'array ()');
          if (u=jointabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEDISPCOLS_TOKEN, 'array ()');
        end;

        if (TmpView.NMTables.Count <> 0) then
        begin
          if (u=nmtablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLENAME_TOKEN, MakeTableNames(TmpView.NMTables) );
          if (u=nmtablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEKEY_TOKEN,MakeKey(TmpView.NMTables));
          if (u=nmtabledatatypesIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPES_TOKEN, MakeDataTypes(TmpView.NMTables) );
          if (u=nmtabledatatypeparamsIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPE_PARAMS_TOKEN, MakeDataTypeParams(TmpView.NMTables) );
          if (u=nmtabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.NMTables, 1) );
          if (u=connectiontablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,CONNECTIONTABLENAME_TOKEN, MakeConnectionTables(TmpView.Table,TmpView.NMTables) );
        end
        else
        begin
          if (u=nmtablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLENAME_TOKEN, 'array ()');
          if (u=nmtablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEKEY_TOKEN,'array ()');
          if (u=nmtabledatatypesIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPES_TOKEN, 'array ()');
          if (u=nmtabledatatypeparamsIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPE_PARAMS_TOKEN, 'array ()');
          if (u=nmtabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDISPCOLS_TOKEN, 'array ()');
          if (u=connectiontablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,CONNECTIONTABLENAME_TOKEN, 'array ()');
        end;

        Writeln(insertFile,tmp);
      end;
      Close(insertFile);
    end;
  end;
  
  TemplateLines.Free;
end;


procedure TPhpOutput.CreateGridfiles(GridTemplateFilename :string);
var TemplateF, gFile: TextFile;
    str,filename,tmp: String;
    TemplateLines: TStringList;
    i,j,u,viewnameIndex,titleIndex :Integer;
    rowsppIndex, filenameIndex, tablenameIndex :Integer;
    tablekeyIndex, datatypesIndex, joinTablekeyIndex, nmtableKeyIndex : Integer;
    whereConstraintIndex, jointablenameIndex,nmtablenameIndex: Integer;
    tabledispcolsIndex,jointabledispcolsIndex, nmtabledispcolsIndex : Integer;
    connectiontablenameIndex,nmtabledatatypesIndex, colCaptIndex : Integer;
    fwIndex,fhIndex,fxIndex,fyIndex,sortedSelectClauseIndex :Integer;
    colTruncIndex, colWidthsIndex,gridAsPopupIndex :Integer;
    TmpGroup: TGroup;
    TmpView: TView;
    keyMappingIndex,columnsOrderIndex: Integer;
begin
  TemplateLines:= TStringList.Create;
  viewnameIndex:=0;  titleIndex:=0;  rowsppIndex:= 0;
  filenameIndex := 0; tablenameIndex := 0;  tablekeyIndex := 0;
  datatypesIndex := 0;  jointablekeyIndex := 0;   nmtablekeyIndex := 0;
  whereConstraintIndex := 0;jointablenameIndex :=0; nmtablenameIndex := 0;
  jointabledispcolsIndex := 0; nmtabledispcolsIndex := 0; tabledispcolsIndex := 0;
  connectiontablenameIndex := 0; nmtabledatatypesIndex := 0; colCaptIndex := 0;
  fwIndex := 0;fHIndex := 0;fXIndex := 0;fYIndex := 0;
  colTruncIndex := 0;colWidthsIndex := 0;sortedSelectClauseIndex:=0;
  keyMappingIndex:=0; gridAsPopupIndex := 0; columnsOrderIndex := 0;

  //read in the template
  if (NOT (FileExists(GridTemplateFilename)) ) then
  begin
    raise ETemplateFileException.Create(GridTemplateFilename+','+TemplateFileError);
  end;
  AssignFile(TemplateF,GridTemplateFilename);
  Reset(TemplateF);
  while not eof(TemplateF) do
  begin
      ReadLn(TemplateF, str);

      //things we do here are the same for all files we would want to generate
      if (AnsiContainsText(str,INCLUDE_TOKEN) )then
        str:= DMMain.ReplaceText(str, INCLUDE_TOKEN, 'include "db_open.php"');
      if (AnsiStartsStr('//|', str)) then continue; //ignore lines beginning with '//|'

      //add this line to our stringlist
      i := TemplateLines.Add(str);

      //we store the line number for the last occurence
      //we actually demand that no token occurs more than once
      if (AnsiContainsText(str,COLUMNS_ORDER_TOKEN) ) then columnsOrderIndex:= i;
      if (AnsiContainsText(str,SORTED_SELECT_CLAUSE_TOKEN) ) then sortedSelectClauseIndex:= i;
      if (AnsiContainsText(str,WHERE_CONSTRAINT_TOKEN) ) then whereConstraintIndex:= i;
      if (AnsiContainsText(str,VIEWNAME_TOKEN) ) then viewnameIndex := i;
      if (AnsiContainsText(str,ROWSPP_TOKEN) ) then rowsppIndex := i;
      if (AnsiContainsText(str,FILENAME_TOKEN) ) then filenameIndex := i;
      if (AnsiContainsText(str,COLUMN_CAPTIONS_TOKEN) ) then colCaptIndex := i;
      if (AnsiContainsText(str,FORMWIDTH_TOKEN) ) then fwIndex := i;
      if (AnsiContainsText(str,FORMHEIGHT_TOKEN) ) then fhIndex := i;
      if (AnsiContainsText(str,FORMX_TOKEN) ) then fXIndex := i;
      if (AnsiContainsText(str,FORMY_TOKEN) ) then fyIndex := i;
      if (AnsiContainsText(str,COLUMN_WIDTHS_TOKEN) ) then colWidthsIndex := i;
      if (AnsiContainsText(str,COLUMN_TRUNC_TOKEN) ) then colTruncIndex := i;

      if (AnsiContainsText(str,TABLENAME_TOKEN) ) then tablenameIndex := i;
      if (AnsiContainsText(str,DATATYPES_TOKEN) ) then datatypesIndex := i;
      if (AnsiContainsText(str,TABLEKEY_TOKEN) ) then tablekeyIndex := i;
      if (AnsiContainsText(str,TABLEDISPCOLS_TOKEN) ) then tabledispcolsIndex := i;

      if (AnsiContainsText(str,JOINTABLEDISPCOLS_TOKEN) ) then jointabledispcolsIndex := i;
      if (AnsiContainsText(str,JOINTABLENAME_TOKEN) ) then jointablenameIndex := i;
      if (AnsiContainsText(str,JOINTABLEKEY_TOKEN) ) then jointablekeyIndex := i;

      if (AnsiContainsText(str,NMTABLEDISPCOLS_TOKEN) ) then nmtabledispcolsIndex := i;
      if (AnsiContainsText(str,NMTABLENAME_TOKEN) ) then nmtablenameIndex := i;
      if (AnsiContainsText(str,NMTABLEDATATYPES_TOKEN) ) then nmtabledatatypesIndex := i;
      if (AnsiContainsText(str,NMTABLEKEY_TOKEN) ) then nmtablekeyIndex := i;
      if (AnsiContainsText(str,CONNECTIONTABLENAME_TOKEN) ) then connectiontablenameIndex := i;

      if (AnsiContainsText(str,KEY_MAPPING_TOKEN) ) then keyMappingIndex := i;
      if (AnsiContainsText(str,GRID_AS_POPUP_TOKEN) ) then gridAsPopupIndex := i;
  end;
  Close(TemplateF);

  //for all groups
  for i:=0 to GroupList.Count-1 do
  begin
    TmpGroup:=TGroup(GroupList[i]);

    //for all views of the group do
    //for each view 1 grid-file is generated
    for j:=0 to TmpGroup.Views.Count-1 do
    begin
      TmpView:=TView(TmpGroup.Views[j]);
      filename:=TmpGroup.Name+'_'+TmpView.Name+'.php';

      //create file for grid-view
      AssignFile(gFile,saveDir+filename);
      Rewrite(gFile);

      Writeln(gFile, GeneratedFileHeader);

      for u:=0 to TemplateLines.Count-1 do
      begin
        tmp := TemplateLines[u];

        if (u = sortedSelectClauseIndex) then
          tmp := DMMain.ReplaceText(tmp,SORTED_SELECT_CLAUSE_TOKEN,MakeSortedSelectClause(TmpView) );
        if (u=whereConstraintIndex) then
          tmp := DMMain.ReplaceText(tmp,WHERE_CONSTRAINT_TOKEN,'"'+TmpView.WhereClause+'"');
        if (u=viewnameIndex) then
          tmp := DMMain.ReplaceText(tmp,VIEWNAME_TOKEN,'"'+TmpView.Name+'"');
        if (u=rowsppIndex) then
          tmp := DMMain.ReplaceText(tmp,ROWSPP_TOKEN,'"'+IntToStr(TmpView.RowsPerPage)+'"');
        if (u=filenameIndex) then
          tmp := DMMain.ReplaceText(tmp,FILENAME_TOKEN,'"'+filename+'"');
        if (u=fWIndex) then
          tmp := DMMain.ReplaceText(tmp,FORMWIDTH_TOKEN, IntToStr(TmpView.FFormWidth));
        if (u=fHIndex) then
          tmp := DMMain.ReplaceText(tmp,FORMHEIGHT_TOKEN, IntToStr(TmpView.FFormHeight));
        if (u=fXIndex) then
          tmp := DMMain.ReplaceText(tmp,FORMX_TOKEN, IntToStr(TmpView.FFormX));
        if (u=fYIndex) then
          tmp := DMMain.ReplaceText(tmp,FORMY_TOKEN, IntToStr(TmpView.FFormY));
        if (u=colCaptIndex) then
          tmp := DMMain.ReplaceText(tmp,COLUMN_CAPTIONS_TOKEN, MakeColumnCaptions(TmpView));
        if (u=colWidthsIndex) then
          tmp := DMMain.ReplaceText(tmp,COLUMN_WIDTHS_TOKEN, MakeColumnWidths(TmpView));
        if (u=colTruncIndex) then
          tmp := DMMain.ReplaceText(tmp,COLUMN_TRUNC_TOKEN, MakeColumnTruncChars(TmpView));
        if (u=columnsOrderIndex) then
          tmp := DMMain.ReplaceText(tmp,COLUMNS_ORDER_TOKEN,MakeGridColumnOrder(TmpView));
          
        if (u=tablenameIndex) then
          tmp := DMMain.ReplaceText(tmp,TABLENAME_TOKEN,'"'+TmpView.Table.Name+'"');
        if (u=tablekeyIndex) then
          tmp := DMMain.ReplaceText(tmp,TABLEKEY_TOKEN,MakeKey(TmpView.Table));
        if (u=datatypesIndex) then
          tmp := DMMain.ReplaceText(tmp,DATATYPES_TOKEN, MakeDataTypes(TmpView.Table));
        if (u=tabledispcolsIndex) then
          tmp := DMMain.ReplaceText(tmp,TABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.Table, 0));

        if (u=keyMappingIndex) then
          tmp:= DMMain.ReplaceText(tmp, KEY_MAPPING_TOKEN, MakeKeyMapping(TmpView));
        if (u=gridAsPopupIndex) then
          tmp:= DMMain.ReplaceText(tmp, GRID_AS_POPUP_TOKEN, BoolToStr(TmpGroup.FViewsAsPopup));

        if (Tmpview.JoinTables.Count <> 0) then
        begin
          if (u=jointablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLENAME_TOKEN, MakeTableNames(TmpView.JoinTables) );
          if (u=jointablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEKEY_TOKEN,MakeKey(TmpView.JoinTables));
          if (u=jointabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.JoinTables, 0));
        end
        else
        begin
          if (u=jointablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLENAME_TOKEN, 'array ()');
          if (u=jointablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEKEY_TOKEN,'array ()');
          if (u=jointabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEDISPCOLS_TOKEN, 'array ()');
        end;

        if (TmpView.NMTables.Count <> 0) then
        begin
          if (u=nmtablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLENAME_TOKEN, MakeTableNames(TmpView.NMTables) );
          if (u=nmtablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEKEY_TOKEN,MakeKey(TmpView.NMTables));
          if (u=nmtabledatatypesIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPES_TOKEN, MakeDataTypes(TmpView.NMTables) );
          if (u=nmtabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.NMTables, 0));
          if (u=connectiontablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,CONNECTIONTABLENAME_TOKEN, MakeConnectionTables(TmpView.Table,TmpView.NMTables) );
        end
        else
        begin
          if (u=nmtablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLENAME_TOKEN, ' array ()');
          if (u=nmtablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEKEY_TOKEN,'array ()');
          if (u=nmtabledatatypesIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPES_TOKEN, 'array ()' );
          if (u=nmtabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDISPCOLS_TOKEN, 'array ()');
          if (u=connectiontablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,CONNECTIONTABLENAME_TOKEN, 'array ()');
        end;
        
        Writeln(gFile,tmp);
      end;
      Close(gFile);
    end;
  end;

  TemplateLines.Free;
end;




procedure TPhpOutput.CreateSQLfiles(SQLTemplateFilename :string);
var TemplateF, gFile: TextFile;
    str,filename,tmp: String;
    TemplateLines: TStringList;
    i,j,u,viewnameIndex,titleIndex :Integer;
    rowsppIndex, filenameIndex, tablenameIndex :Integer;
    tablekeyIndex, datatypesIndex, joinTablekeyIndex, nmtableKeyIndex : Integer;
    whereConstraintIndex, jointablenameIndex,nmtablenameIndex: Integer;
    tabledispcolsIndex,jointabledispcolsIndex, nmtabledispcolsIndex : Integer;
    connectiontablenameIndex,nmtabledatatypesIndex, colCaptIndex : Integer;
    fwIndex,fhIndex,fxIndex,fyIndex,sortedSelectClauseIndex :Integer;
    colTruncIndex, colWidthsIndex,gridAsPopupIndex :Integer;
    TmpGroup: TGroup;
    TmpView: TView;
    keyMappingIndex,columnsOrderIndex: Integer;
begin
  TemplateLines:= TStringList.Create;
  viewnameIndex:=0;  titleIndex:=0;  rowsppIndex:= 0;
  filenameIndex := 0; tablenameIndex := 0;  tablekeyIndex := 0;
  datatypesIndex := 0;  jointablekeyIndex := 0;   nmtablekeyIndex := 0;
  whereConstraintIndex := 0;jointablenameIndex :=0; nmtablenameIndex := 0;
  jointabledispcolsIndex := 0; nmtabledispcolsIndex := 0; tabledispcolsIndex := 0;
  connectiontablenameIndex := 0; nmtabledatatypesIndex := 0; colCaptIndex := 0;
  fwIndex := 0;fHIndex := 0;fXIndex := 0;fYIndex := 0;
  colTruncIndex := 0;colWidthsIndex := 0;sortedSelectClauseIndex:=0;
  keyMappingIndex:=0; gridAsPopupIndex := 0; columnsOrderIndex := 0;

  //read in the template
  if (NOT (FileExists(SQLTemplateFilename)) ) then
  begin
    raise ETemplateFileException.Create(SQLTemplateFilename+','+TemplateFileError);
  end;
  AssignFile(TemplateF,SQLTemplateFilename);
  Reset(TemplateF);
  while not eof(TemplateF) do
  begin
      ReadLn(TemplateF, str);

      //things we do here are the same for all files we would want to generate
      if (AnsiContainsText(str,INCLUDE_TOKEN) )then
        str:= DMMain.ReplaceText(str, INCLUDE_TOKEN, 'include "db_open.php"');
      if (AnsiStartsStr('//|', str)) then continue; //ignore lines beginning with '//|'

      //add this line to our stringlist
      i := TemplateLines.Add(str);

      //we store the line number for the last occurence
      //we actually demand that no token occurs more than once
      if (AnsiContainsText(str,COLUMNS_ORDER_TOKEN) ) then columnsOrderIndex:= i;
      if (AnsiContainsText(str,SORTED_SELECT_CLAUSE_TOKEN) ) then sortedSelectClauseIndex:= i;
      if (AnsiContainsText(str,WHERE_CONSTRAINT_TOKEN) ) then whereConstraintIndex:= i;
      if (AnsiContainsText(str,VIEWNAME_TOKEN) ) then viewnameIndex := i;
      if (AnsiContainsText(str,ROWSPP_TOKEN) ) then rowsppIndex := i;
      if (AnsiContainsText(str,FILENAME_TOKEN) ) then filenameIndex := i;
      if (AnsiContainsText(str,COLUMN_CAPTIONS_TOKEN) ) then colCaptIndex := i;
      if (AnsiContainsText(str,FORMWIDTH_TOKEN) ) then fwIndex := i;
      if (AnsiContainsText(str,FORMHEIGHT_TOKEN) ) then fhIndex := i;
      if (AnsiContainsText(str,FORMX_TOKEN) ) then fXIndex := i;
      if (AnsiContainsText(str,FORMY_TOKEN) ) then fyIndex := i;
      if (AnsiContainsText(str,COLUMN_WIDTHS_TOKEN) ) then colWidthsIndex := i;
      if (AnsiContainsText(str,COLUMN_TRUNC_TOKEN) ) then colTruncIndex := i;

      if (AnsiContainsText(str,TABLENAME_TOKEN) ) then tablenameIndex := i;
      if (AnsiContainsText(str,DATATYPES_TOKEN) ) then datatypesIndex := i;
      if (AnsiContainsText(str,TABLEKEY_TOKEN) ) then tablekeyIndex := i;
      if (AnsiContainsText(str,TABLEDISPCOLS_TOKEN) ) then tabledispcolsIndex := i;

      if (AnsiContainsText(str,JOINTABLEDISPCOLS_TOKEN) ) then jointabledispcolsIndex := i;
      if (AnsiContainsText(str,JOINTABLENAME_TOKEN) ) then jointablenameIndex := i;
      if (AnsiContainsText(str,JOINTABLEKEY_TOKEN) ) then jointablekeyIndex := i;

      if (AnsiContainsText(str,NMTABLEDISPCOLS_TOKEN) ) then nmtabledispcolsIndex := i;
      if (AnsiContainsText(str,NMTABLENAME_TOKEN) ) then nmtablenameIndex := i;
      if (AnsiContainsText(str,NMTABLEDATATYPES_TOKEN) ) then nmtabledatatypesIndex := i;
      if (AnsiContainsText(str,NMTABLEKEY_TOKEN) ) then nmtablekeyIndex := i;
      if (AnsiContainsText(str,CONNECTIONTABLENAME_TOKEN) ) then connectiontablenameIndex := i;

      if (AnsiContainsText(str,KEY_MAPPING_TOKEN) ) then keyMappingIndex := i;
      if (AnsiContainsText(str,GRID_AS_POPUP_TOKEN) ) then gridAsPopupIndex := i;
  end;
  Close(TemplateF);

  //for all groups
  for i:=0 to GroupList.Count-1 do
  begin
    TmpGroup:=TGroup(GroupList[i]);

    //for all views of the group do
    //for each view 1 grid-file is generated
    for j:=0 to TmpGroup.Views.Count-1 do
    begin
      TmpView:=TView(TmpGroup.Views[j]);
      filename:=TmpGroup.Name+'_'+TmpView.Name+'_sql.php';

      //create file for grid-view
      AssignFile(gFile,saveDir+filename);
      Rewrite(gFile);

      Writeln(gFile, GeneratedFileHeader);

      for u:=0 to TemplateLines.Count-1 do
      begin
        tmp := TemplateLines[u];

        if (u = sortedSelectClauseIndex) then
          tmp := DMMain.ReplaceText(tmp,SORTED_SELECT_CLAUSE_TOKEN,MakeSortedSelectClause(TmpView) );
        if (u=whereConstraintIndex) then
          tmp := DMMain.ReplaceText(tmp,WHERE_CONSTRAINT_TOKEN,'"'+TmpView.WhereClause+'"');
        if (u=viewnameIndex) then
          tmp := DMMain.ReplaceText(tmp,VIEWNAME_TOKEN,'"'+TmpView.Name+'"');
        if (u=rowsppIndex) then
          tmp := DMMain.ReplaceText(tmp,ROWSPP_TOKEN,'"'+IntToStr(TmpView.RowsPerPage)+'"');
        if (u=filenameIndex) then
          tmp := DMMain.ReplaceText(tmp,FILENAME_TOKEN,'"'+filename+'"');
        if (u=fWIndex) then
          tmp := DMMain.ReplaceText(tmp,FORMWIDTH_TOKEN, IntToStr(TmpView.FFormWidth));
        if (u=fHIndex) then
          tmp := DMMain.ReplaceText(tmp,FORMHEIGHT_TOKEN, IntToStr(TmpView.FFormHeight));
        if (u=fXIndex) then
          tmp := DMMain.ReplaceText(tmp,FORMX_TOKEN, IntToStr(TmpView.FFormX));
        if (u=fYIndex) then
          tmp := DMMain.ReplaceText(tmp,FORMY_TOKEN, IntToStr(TmpView.FFormY));
        if (u=colCaptIndex) then
          tmp := DMMain.ReplaceText(tmp,COLUMN_CAPTIONS_TOKEN, MakeColumnCaptions(TmpView));
        if (u=colWidthsIndex) then
          tmp := DMMain.ReplaceText(tmp,COLUMN_WIDTHS_TOKEN, MakeColumnWidths(TmpView));
        if (u=colTruncIndex) then
          tmp := DMMain.ReplaceText(tmp,COLUMN_TRUNC_TOKEN, MakeColumnTruncChars(TmpView));
        if (u=columnsOrderIndex) then
          tmp := DMMain.ReplaceText(tmp,COLUMNS_ORDER_TOKEN,MakeGridColumnOrder(TmpView));
          
        if (u=tablenameIndex) then
          tmp := DMMain.ReplaceText(tmp,TABLENAME_TOKEN,'"'+TmpView.Table.Name+'"');
        if (u=tablekeyIndex) then
          tmp := DMMain.ReplaceText(tmp,TABLEKEY_TOKEN,MakeKey(TmpView.Table));
        if (u=datatypesIndex) then
          tmp := DMMain.ReplaceText(tmp,DATATYPES_TOKEN, MakeDataTypes(TmpView.Table));
        if (u=tabledispcolsIndex) then
          tmp := DMMain.ReplaceText(tmp,TABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.Table, 0));

        if (u=keyMappingIndex) then
          tmp:= DMMain.ReplaceText(tmp, KEY_MAPPING_TOKEN, MakeKeyMapping(TmpView));
        if (u=gridAsPopupIndex) then
          tmp:= DMMain.ReplaceText(tmp, GRID_AS_POPUP_TOKEN, BoolToStr(TmpGroup.FViewsAsPopup));

        if (Tmpview.JoinTables.Count <> 0) then
        begin
          if (u=jointablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLENAME_TOKEN, MakeTableNames(TmpView.JoinTables) );
          if (u=jointablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEKEY_TOKEN,MakeKey(TmpView.JoinTables));
          if (u=jointabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.JoinTables, 0));
        end
        else
        begin
          if (u=jointablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLENAME_TOKEN, 'array ()');
          if (u=jointablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEKEY_TOKEN,'array ()');
          if (u=jointabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,JOINTABLEDISPCOLS_TOKEN, 'array ()');
        end;

        if (TmpView.NMTables.Count <> 0) then
        begin
          if (u=nmtablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLENAME_TOKEN, MakeTableNames(TmpView.NMTables) );
          if (u=nmtablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEKEY_TOKEN,MakeKey(TmpView.NMTables));
          if (u=nmtabledatatypesIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPES_TOKEN, MakeDataTypes(TmpView.NMTables) );
          if (u=nmtabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDISPCOLS_TOKEN, MakeDispCols(TmpView.NMTables, 0));
          if (u=connectiontablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,CONNECTIONTABLENAME_TOKEN, MakeConnectionTables(TmpView.Table,TmpView.NMTables) );
        end
        else
        begin
          if (u=nmtablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLENAME_TOKEN, ' array ()');
          if (u=nmtablekeyIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEKEY_TOKEN,'array ()');
          if (u=nmtabledatatypesIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDATATYPES_TOKEN, 'array ()' );
          if (u=nmtabledispcolsIndex) then
            tmp := DMMain.ReplaceText(tmp,NMTABLEDISPCOLS_TOKEN, 'array ()');
          if (u=connectiontablenameIndex) then
            tmp := DMMain.ReplaceText(tmp,CONNECTIONTABLENAME_TOKEN, 'array ()');
        end;
        
        Writeln(gFile,tmp);
      end;
      Close(gFile);
    end;
  end;

  TemplateLines.Free;
end;





function TPHPOutput.MakeSortedSelectClause(view: TView) :String;
var str: String;
    i: Integer;
begin
  MakeSortedSelectClause := '';
  str := '"';

  for i:=0 to view.GridSortedColumnList.Count-1 do
  begin
    if NOT(SW_Column(view.GridSortedColumnList.Objects[i]).selectedForGrid)
      AND NOT(SW_Column(view.GridSortedColumnList.Objects[i]).IsKey)
     then continue;
    
    str:= str + view.GridSortedColumnList[i] + ' AS'+' '''+view.GridSortedColumnList[i]+''',';
  end;

  str[Length(str)] := '"'; //replace the last comma

  MakeSortedSelectClause := str;
end;

function TPHPOutput.MakeColumnOrder(view: TView) :String;
var str, tableType: String;
    i: Integer;
    colTable :SW_Table;
    joinTables :TObjectList;
begin
  MakeColumnOrder := '';
  str := 'array( ';

  joinTables := TObjectList.Create(false);

  for i:=0 to view.FormSortedColumnList.Count-1 do
  begin
    if NOT(SW_Column(view.FormSortedColumnList.Objects[i]).selectedForForm) then continue;

    colTable := SW_Column(view.FormSortedColumnList.Objects[i]).Table;
    if (joinTables.IndexOf(colTable) <> -1) then continue;  //only one column per jointable is accepted

    if (colTable = view.Table) then
      tabletype := 'main'
    else if (view.JoinTables.IndexOf(colTable) <> -1) then
    begin
      tabletype := 'join';
      joinTables.Add(colTable);
    end
    else if (view.NMTables.IndexOf(colTable) <> -1) then
      tabletype:= 'nm'
    else assert(false);

    str:= str + '"'+view.FormSortedColumnList[i] + '" =>"' +tabletype+'",';
  end;

  joinTables.Free;
  str[Length(str)] := ')'; //replace the last comma with a ')'

  MakeColumnOrder := str;
end;

function TPHPOutput.MakeGridColumnOrder(view: TView) :String;
var str, tableType: String;
    i: Integer;
    colTable :SW_Table;
begin
  MakeGridColumnOrder := '';
  str := 'array( ';

  for i:=0 to view.GridSortedColumnList.Count-1 do
  begin
    if NOT(SW_Column(view.GridSortedColumnList.Objects[i]).selectedForGrid) then continue;

    colTable := SW_Column(view.FormSortedColumnList.Objects[i]).Table;

    if (colTable = view.Table) then tabletype := 'main'
    else if (view.JoinTables.IndexOf(colTable) <> -1) then tabletype := 'join'
    else if (view.NMTables.IndexOf(colTable) <> -1) then tabletype:= 'nm'
    else assert(false);

    str:= str + '"'+view.GridSortedColumnList[i] + '" =>"' +tabletype+'",';
  end;

  str[Length(str)] := ')'; //replace the last comma with a ')'

  MakeGridColumnOrder := str;
end;

function TPHPOutput.MakeKey(table : SW_Table) : String;
var str: String;
    i: Integer;
begin
  MakeKey := '';
  str := 'array (';

  for i:=0 to Table.PrimaryKey.Count-1 do
  begin
    str:= str + '"'+Table.primaryKey[i] +'",';
  end;

  str[Length(str)] := ')'; //replace the last comma with a ')'

  MakeKey := str;
end;

function TPHPOutput.MakeKey(tables: TObjectList) : String;
var str: String;
    i: Integer;
begin
  MakeKey := '';
  str := 'array (';

  for i:=0 to tables.Count-1 do
  begin
    str:= str + '"'+SW_Table(tables[i]).name + '" => ';
    str:= str + MakeKey(SW_Table(tables[i])) + ',';
  end;

  str[Length(str)] := ')'; //replace the last comma with a ')'
  MakeKey := str;
end;



function TPHPOutput.MakeDataTypes(table: SW_Table) : String;
var str,dataType : String;
    i : Integer;
begin
  MakeDataTypes := '';
  str:= 'array (';

  for i:=0 to Table.Columns.Count-1 do
  begin
    str:= str + '"'+SW_Column(Table.Columns[i]).Name +'"=> ';
    dataType := SW_Column(Table.Columns[i]).DataTypeName;
    if (i = Table.Columns.Count-1) then
      str:= str + '"'+dataType +'")'
    else
      str:= str + '"'+dataType +'",';
  end;
  if (Table.Columns.Count = 0) then str := str + ')';

  MakeDataTypes := str;
end;

function TPHPOutput.MakeDataTypes(tables: TObjectList) : String;
var str : String;
    i : Integer;
begin
  MakeDataTypes := '';
  str:= 'array (';

  for i:=0 to tables.Count-1 do
  begin
    str:= str + '"'+SW_Table(tables[i]).name + '" => ';
    str:= str + MakeDataTypes(SW_Table(tables[i])) + ',';
  end;

  str[Length(str)] := ')'; //replace the last comma with a ')'
  MakeDataTypes := str;
end;

function TPHPOutput.MakeDataTypeParams(table: SW_Table) : String;
var str,dataTypeParams,d : String;
    i,j : Integer;
    ParamList : TStringList;
begin
  MakeDataTypeParams := '';
  str:= 'array (';

  for i:=0 to Table.Columns.Count-1 do
  begin
    str:= str + '"'+SW_Column(Table.Columns[i]).Name +'"=> ';
    dataTypeParams := SW_Column(Table.Columns[i]).DataTypeParams;
    //remove the surrounding parentheses
    Delete(dataTypeParams,1,1);
    Delete(dataTypeParams,Length(dataTypeParams),1);

    ParamList := TStringLIst.Create;
    Split(dataTypeParams, ',', paramList);
    d:= 'array ( ';
    for j:=0 to paramList.Count-1 do
    begin
      d := d +paramList[j] + ',';
    end;
    d[Length(d)] := ')';

    if (i = Table.Columns.Count-1) then
      str:= str + d +')'
    else
      str:= str + d +',';
  end;
  if (Table.Columns.Count = 0) then str := str + ')';

  MakeDataTypeParams := str;
end;

function TPHPOutput.MakeDataTypeParams(tables: TObjectList) : String;
var str : String;
    i : Integer;
begin
  MakeDataTypeParams := '';
  str:= 'array (';

  for i:=0 to tables.Count-1 do
  begin
    str:= str + '"'+SW_Table(tables[i]).name + '" => ';
    str:= str + MakeDataTypeParams(SW_Table(tables[i])) + ',';
  end;

  str[Length(str)] := ')'; //replace the last comma with a ')'
  MakeDataTypeParams := str;
end;

function TPhpOutput.MakeDispCols(table: SW_Table; option: Integer) : String;
var i: Integer;
    resultString : String;
begin
  MakeDispCols := '';
  resultString := 'array( ';

  for i:=0 to table.Columns.Count-1 do
  begin
    if (option = 0) then    //Grid
    begin
      if (SW_Column(table.columns[i]).selectedForGrid) then
        resultString := resultString+'"'+SW_Column(table.columns[i]).name + '",';
    end
    else if (option =1) then //Form
    begin
      if (SW_Column(table.columns[i]).selectedForForm) then
        resultString := resultString+'"'+SW_Column(table.columns[i]).name + '",';
    end
    else assert(false);
  end;

  resultString[Length(resultString)] := ')'; //replace the last comma with a ')'

  MakeDispCols := resultString;
end;

function TPhpOutput.MakeDispCols(tables: TObjectList; option: Integer) : String;
var i: Integer;
    resultString : String;
begin
  MakeDispCols := '';
  resultString := 'array(';

  for i:=0 to tables.Count-1 do
  begin
    resultString:= resultString + '"'+SW_Table(tables[i]).name + '" => ';
    resultString:= resultString + MakeDispCols(SW_Table(tables[i]), option) + ',';
  end;

  resultString[Length(resultString)] := ')'; //replace the last comma with a ')'

  MakeDispCols := resultString;
end;

function TPhpOutput.MakeTableNames(tables: TObjectList) : String;
var i: Integer;
    str : String;
begin
  MakeTableNames := '';
  str := 'array(';

  for i:= 0 to tables.Count-1 do
  begin
    str := str + '"' + SW_Table(tables[i]).name + '",';
  end;

  str[Length(str)] := ')';

  MakeTableNames := str;
end;

function TPhpOutput.MakeConnectionTables(mainTable: SW_Table; nmTables: TObjectList) : String;
var i: Integer;
    str: String;
begin
  MakeConnectionTables := '';
  str:= 'array (';

  for i:=0 to nmTables.Count-1 do
  begin
    str:= str + '"' + SW_Table(nmTables[i]).name + '" => ';
    str:= str + '"'+ Model.GetConnectionTable(mainTable, SW_Table(nmTables[i]) ).name+'",';
  end;

  str[Length(str)] := ')';
  MakeConnectionTables := str;
end;

//type TemplateType = (grid, update, insert);
function TPhpOutput.MakeColumnCaptions(view : Tview) : String;
var k :Integer;
    tmpCol : SW_Column;
    cols :TStringList;
    str : String;
begin
  cols := TStringList.Create;
  str := 'array ( ';      //the trailing space would be overwritten by ')' in case of an empty arrary

  view.GetColumns(cols);

  for k:= 0 to cols.Count-1 do
  begin
    tmpCol := SW_Column(cols.Objects[k]);
    if (tmpCol.selectedForGrid) then
      str := str +'"'+tmpCol.Table.Name+'.'+tmpCol.name +'"'+ '=>' + '"'+tmpCol.GridName +'"'+ ',';
  end;

  cols.Free;
  str[Length(str)] := ')';
  assert(Length(str) <> 8); //the array must be longer than 'array ( '
  MakeColumnCaptions := str;
end;

function TPhpOutput.MakeFormColumnCaptions(view :TView) : String;
var k :Integer;
    tmpCol : SW_Column;
    cols :TStringList;
    str : String;
begin
  cols := TStringList.Create;
  str := 'array ( ';      //the trailing space would be overwritten by ')' in case of an empty arrary

  view.GetColumns(cols);

  for k:= 0 to cols.Count-1 do
  begin
    tmpCol := SW_Column(cols.Objects[k]);

    //We have to differentiate if it is the colum of a mainTable, joinTable, or nmTable
    if (view.Table = tmpCol.Table) then
    begin
      if (tmpCol.selectedForForm) then
        str := str +'"'+tmpCol.Table.Name+'.'+tmpCol.name +'"'+ '=>' + '"'+tmpCol.FormName +'"'+ ',';
    end
    else if (view.JoinTables.IndexOf(tmpCol.Table) <> -1) then    //if it is the column of a join-table
    begin
      if (tmpCol.isKey) then //we only need it once
        str := str +'"'+tmpCol.Table.Name+'.'+tmpCol.name +'"'+ '=>' + '"'+tmpCol.Table.Join_ColumnName +'"'+ ',';
    end
    else if (view.NMTables.IndexOf(tmpCol.Table) <> -1) then    //if it is the column of an NM-table
      if (tmpCol.selectedForForm) then
        str := str +'"'+tmpCol.Table.Name+'.'+tmpCol.name +'"'+ '=>' + '"'+tmpCol.FormName +'"'+ ','
  end;

  cols.Free;
  str[Length(str)] := ')';
  assert(Length(str) > 8); //the array must not be empty
  MakeFormColumnCaptions := str;
end;

function TPhpOutput.MakeColumnWidths(view :TView) : String;
var k :Integer;
    tmpCol : SW_Column;
    cols :TStringList;
    str : String;
begin
  cols := TStringList.Create;
  str := 'array ( ';

  view.GetColumns(cols);

  for k:= 0 to cols.Count-1 do
  begin
    tmpCol := SW_Column(cols.Objects[k]);
    if ((tmpCol.FixedWidth <> -1) and (tmpCol.selectedForGrid)) then
      str := str +'"'+tmpCol.Table.Name+'.'+tmpCol.name +'"'+ '=>' + '"'+IntToStr(tmpCol.FixedWidth) +'"'+ ',';
  end;


  cols.Free;
  str[Length(str)] := ')';
  MakeColumnWidths := str;
end;


function TPhpOutput.MakeFormColumnWidths(view :TView) : String;
var k :Integer;
    tmpCol : SW_Column;
    cols :TStringList;
    str : String;
begin
  cols := TStringList.Create;
  str := 'array ( ';

  view.GetColumns(cols);

  for k:= 0 to cols.Count-1 do
  begin
    tmpCol := SW_Column(cols.Objects[k]);

    //We have to differentiate if it is the colum of a mainTable, joinTable, or nmTable
    if (view.Table = tmpCol.Table) then
    begin
      if ((tmpCol.FormWidth <> -1) and (tmpCol.selectedForForm)) then
            str := str +'"'+tmpCol.Table.Name+'.'+tmpCol.name +'"'+ '=>' + '"'+IntToStr(tmpCol.FormWidth) +'"'+ ',';
    end
    else if (view.JoinTables.IndexOf(tmpCol.Table) <> -1) then    //if it is the column of a join-table
    begin
      if ((tmpCol.Table.Join_Width <> -1) and (tmpCol.selectedForForm)) then
        str := str +'"'+tmpCol.Table.Name+'.'+tmpCol.name +'"'+ '=>' + '"'+IntToStr(tmpCol.Table.Join_Width) +'"'+ ',';
    end
    else if (view.NMTables.IndexOf(tmpCol.Table) <> -1) then    //if it is the column of an NM-table
      if ((tmpCol.Table.NM_Width <> -1) and (tmpCol.selectedForForm)) then
        str := str +'"'+tmpCol.Table.Name+'.'+tmpCol.name +'"'+ '=>' + '"'+IntToStr(tmpCol.Table.NM_Width) +'"'+ ',';
  end;

  cols.Free;
  str[Length(str)] := ')';
  MakeFormColumnWidths := str;
end;

function TPhpOutput.MakeColumnTruncChars(view :TView) : String;
var k :Integer;
    tmpCol : SW_Column;
    cols :TStringList;
    str : String;
begin
  cols := TStringList.Create;
  str := 'array ( ';

  view.GetColumns(cols);

  for k:= 0 to cols.Count-1 do
  begin
    tmpCol := SW_Column(cols.Objects[k]);
    if ((tmpCol.TruncateChars <> -1) and (tmpCol.selectedForGrid)) then
      str := str +'"'+tmpCol.Table.Name+'.'+tmpCol.name +'"'+ '=>' + '"'+IntToStr(tmpCol.TruncateChars) +'"'+ ',';
  end;

  cols.Free;
  str[Length(str)] := ')';
  MakeColumnTruncChars := str;
end;

function TPhpOutput.MakeKeyMapping(view : TView) : String;
var k, l :Integer;
    table, nmTable, ConnTable : SW_Table;
    colList :TObjectList;
    col : SW_Column;
    str, nameThere : String;
begin
  str := 'array ( ';

  table:=view.Table;

  colList := TObjectList.Create(false);
  try
  table.getFKColumns(colList);
  for k:=0 to colList.Count-1 do
  begin
    col := SW_Column(colList[k]);
    assert(col.IsForeignKey);

    nameThere := col.fkTablename + '.' + col.fkColname;
    str := str+'"'+table.name+'.'+col.name+'"'+ '=>' +'"'+ nameThere+'"'+',';
  end;
  colList.Clear;

  for k:= 0 to view.NMTables.Count-1 do
  begin
    nmTable := SW_Table(view.NMTables[k]);
    connTable := model.GetConnectionTable(table, nmtable);

    connTable.getFkColumns(colList);

    for l:=0 to colList.Count-1 do
    begin
      col := SW_Column(colList[l]);
      assert(col.IsForeignKey);

      nameThere := col.fkTablename + '.' + col.fkColname;
      str := str+'"'+connTable.name+'.'+col.name+'"'+ '=>' +'"'+ nameThere+'"'+',';
    end;
    colList.Clear;
  end;

  finally
    ColList.Free;
  end;
  str[Length(str)] := ')';
  MakeKeyMapping := str;
end;

function TPhpOutput.MakeImageNames() :String;
var i,j :Integer;
    tmpGroup: TGroup;
    tmpView: TView;
    str : String;
begin
  str := 'array ( ';

  for i:=0 to GroupList.Count-1 do
  begin
    tmpGroup := TGroup(GroupList[i]);
    for j:=0 to tmpGroup.Views.Count-1 do
    begin
      tmpView:= TView(tmpGroup.Views[j]);
      str := str+ '"'+tmpGroup.Name + '.'+ tmpView.name+'"=>"'+tmpView.IconFilename+'",';
    end;
  end;

  str[Length(str)] := ')';
  MakeImageNames := str;
end;

function TPHPOutput.MakeDefaultValues(table: SW_Table) : String;
var str,dataType : String;
    i : Integer;
begin
  MakeDefaultValues := '';
  str:= 'array ( ';

  for i:=0 to Table.Columns.Count-1 do
  begin
    str:= str + '"'+SW_Column(Table.Columns[i]).Name +'"=> ';
    dataType := SW_Column(Table.Columns[i]).DefaultValue;
    str := str + '"'+dataType +'",';
  end;
  str[Length(str)] := ')';

  MakeDefaultValues := str;
end;

function TPHPOutput.MakeNotNull(table: SW_Table) : String;
var str,dataType : String;
    i : Integer;
begin
  MakeNotNull := '';
  str:= 'array ( ';

  for i:=0 to Table.Columns.Count-1 do
  begin
    if (NOT SW_Column(Table.Columns[i]).selectedForForm) then continue;

    str:= str + '"'+SW_Column(Table.Columns[i]).Name +'"=> ';
    dataType := BoolToStr(SW_Column(Table.Columns[i]).NotNull,true);
    str := str + dataType +',';
  end;
  str[Length(str)] := ')';

  MakeNotNull := str;
end;

end.
