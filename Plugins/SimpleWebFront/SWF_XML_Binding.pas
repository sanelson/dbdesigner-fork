
{***************************************************************************}
{                                                                           }
{                             XML Data Binding                              }
{                                                                           }
{         Generated on: 06.08.2003 21:22:56                                 }
{       Generated from: D:\Projects\DBDesigner4\Stuff\SWF_XML_Binding.xdb   }
{   Settings stored in: D:\Projects\DBDesigner4\Stuff\SWF_XML_Binding.xdb   }
{                                                                           }
{***************************************************************************}

unit SWF_XML_Binding;

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLSWF_DataType = interface;
  IXMLSWF_GroupsType = interface;
  IXMLSWF_GroupType = interface;
  IXMLSWF_ViewsType = interface;
  IXMLSWF_ViewType = interface;
  IXMLSWF_TableType = interface;
  IXMLSWF_ColumnsType = interface;
  IXMLSWF_ColumnType = interface;
  IXMLJoinTablesType = interface;
  IXMLNMTablesType = interface;
  IXMLUnassignedViewsType = interface;

{ IXMLSWF_DataType }

  IXMLSWF_DataType = interface(IXMLNode)
    ['{BFE7CE6E-C8A2-4095-B53F-A48D90CDAB1B}']
    { Property Accessors }
    function Get_Username: WideString;
    function Get_Hostname: WideString;
    function Get_Databasename: WideString;
    function Get_Heading: WideString;
    function Get_Layout: WideString;
    function Get_WinSaveDir: WideString;
    function Get_LinuxSaveDir: WideString;
    function Get_SWF_Groups: IXMLSWF_GroupsType;
    function Get_UnassignedViews: IXMLUnassignedViewsType;
    procedure Set_Username(Value: WideString);
    procedure Set_Hostname(Value: WideString);
    procedure Set_Databasename(Value: WideString);
    procedure Set_Heading(Value: WideString);
    procedure Set_Layout(Value: WideString);
    procedure Set_WinSaveDir(Value: WideString);
    procedure Set_LinuxSaveDir(Value: WideString);
    { Methods & Properties }
    property Username: WideString read Get_Username write Set_Username;
    property Hostname: WideString read Get_Hostname write Set_Hostname;
    property Databasename: WideString read Get_Databasename write Set_Databasename;
    property Heading: WideString read Get_Heading write Set_Heading;
    property Layout: WideString read Get_Layout write Set_Layout;
    property WinSaveDir: WideString read Get_WinSaveDir write Set_WinSaveDir;
    property LinuxSaveDir: WideString read Get_LinuxSaveDir write Set_LinuxSaveDir;
    property SWF_Groups: IXMLSWF_GroupsType read Get_SWF_Groups;
    property UnassignedViews: IXMLUnassignedViewsType read Get_UnassignedViews;
  end;

{ IXMLSWF_GroupsType }

  IXMLSWF_GroupsType = interface(IXMLNodeCollection)
    ['{52F9652F-4FA4-4D94-8BEB-84ACF4099E2B}']
    { Property Accessors }
    function Get_SWF_Group(Index: Integer): IXMLSWF_GroupType;
    { Methods & Properties }
    function Add: IXMLSWF_GroupType;
    function Insert(const Index: Integer): IXMLSWF_GroupType;
    property SWF_Group[Index: Integer]: IXMLSWF_GroupType read Get_SWF_Group; default;
  end;

{ IXMLSWF_GroupType }

  IXMLSWF_GroupType = interface(IXMLNode)
    ['{EA59D275-C9F2-41FB-9757-0096AFF53963}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_ShowOnLine: Integer;
    function Get_ShowInColumn: Integer;
    function Get_ViewsAsPopup: Boolean;
    function Get_SWF_Views: IXMLSWF_ViewsType;
    procedure Set_Name(Value: WideString);
    procedure Set_ShowOnLine(Value: Integer);
    procedure Set_ShowInColumn(Value: Integer);
    procedure Set_ViewsAsPopup(Value: Boolean);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property ShowOnLine: Integer read Get_ShowOnLine write Set_ShowOnLine;
    property ShowInColumn: Integer read Get_ShowInColumn write Set_ShowInColumn;
    property ViewsAsPopup: Boolean read Get_ViewsAsPopup write Set_ViewsAsPopup;
    property SWF_Views: IXMLSWF_ViewsType read Get_SWF_Views;
  end;

{ IXMLSWF_ViewsType }

  IXMLSWF_ViewsType = interface(IXMLNodeCollection)
    ['{AC4E7BD9-3470-41A8-9DF8-1AB3420A7822}']
    { Property Accessors }
    function Get_SWF_View(Index: Integer): IXMLSWF_ViewType;
    { Methods & Properties }
    function Add: IXMLSWF_ViewType;
    function Insert(const Index: Integer): IXMLSWF_ViewType;
    property SWF_View[Index: Integer]: IXMLSWF_ViewType read Get_SWF_View; default;
  end;

{ IXMLSWF_ViewType }

  IXMLSWF_ViewType = interface(IXMLNode)
    ['{50B7054B-0EA4-4A62-8960-A1186B95DB25}']
    { Property Accessors }
    function Get_FormHeight: Integer;
    function Get_FormWidth: Integer;
    function Get_FormX: Integer;
    function Get_FormY: Integer;
    function Get_GridPopupHeight: Integer;
    function Get_GridPopupWidth: Integer;
    function Get_GridPopupX: Integer;
    function Get_GridPopupY: Integer;
    function Get_Name: WideString;
    function Get_WhereClause: WideString;
    function Get_SWF_Table: IXMLSWF_TableType;
    function Get_RowsPerPage: Integer;
    function Get_UseCompoundColNames: Boolean;
    function Get_Icon: WideString;
    function Get_IconFilename: WideString;
    function Get_JoinTables: IXMLJoinTablesType;
    function Get_NMTables: IXMLNMTablesType;
    function Get_GridSortedColumns: WideString;
    function Get_FormSortedColumns: WideString;
    function Get_OrderBy: WideString;
    procedure Set_FormHeight(Value: Integer);
    procedure Set_FormWidth(Value: Integer);
    procedure Set_FormX(Value: Integer);
    procedure Set_FormY(Value: Integer);
    procedure Set_GridPopupHeight(Value: Integer);
    procedure Set_GridPopupWidth(Value: Integer);
    procedure Set_GridPopupX(Value: Integer);
    procedure Set_GridPopupY(Value: Integer);
    procedure Set_Name(Value: WideString);
    procedure Set_WhereClause(Value: WideString);
    procedure Set_RowsPerPage(Value: Integer);
    procedure Set_UseCompoundColNames(Value: Boolean);
    procedure Set_Icon(Value: WideString);
    procedure Set_IconFilename(Value: WideString);
    procedure Set_GridSortedColumns(Value: WideString);
    procedure Set_FormSortedColumns(Value: WideString);
    procedure Set_OrderBy(Value: WideString);
    { Methods & Properties }
    property FormHeight: Integer read Get_FormHeight write Set_FormHeight;
    property FormWidth: Integer read Get_FormWidth write Set_FormWidth;
    property FormX: Integer read Get_FormX write Set_FormX;
    property FormY: Integer read Get_FormY write Set_FormY;
    property GridPopupHeight: Integer read Get_GridPopupHeight write Set_GridPopupHeight;
    property GridPopupWidth: Integer read Get_GridPopupWidth write Set_GridPopupWidth;
    property GridPopupX: Integer read Get_GridPopupX write Set_GridPopupX;
    property GridPopupY: Integer read Get_GridPopupY write Set_GridPopupY;
    property Name: WideString read Get_Name write Set_Name;
    property WhereClause: WideString read Get_WhereClause write Set_WhereClause;
    property SWF_Table: IXMLSWF_TableType read Get_SWF_Table;
    property RowsPerPage: Integer read Get_RowsPerPage write Set_RowsPerPage;
    property UseCompoundColNames: Boolean read Get_UseCompoundColNames write Set_UseCompoundColNames;
    property Icon: WideString read Get_Icon write Set_Icon;
    property IconFilename: WideString read Get_IconFilename write Set_IconFilename;
    property JoinTables: IXMLJoinTablesType read Get_JoinTables;
    property NMTables: IXMLNMTablesType read Get_NMTables;
    property GridSortedColumns: WideString read Get_GridSortedColumns write Set_GridSortedColumns;
    property FormSortedColumns: WideString read Get_FormSortedColumns write Set_FormSortedColumns;
    property OrderBy: WideString read Get_OrderBy write Set_OrderBy;
  end;

{ IXMLSWF_TableType }

  IXMLSWF_TableType = interface(IXMLNode)
    ['{B00B8FDB-9C53-46AC-9ADD-20D47CBEE481}']
    { Property Accessors }
    function Get_OrigTable: Integer;
    function Get_Join_ColumnName: WideString;
    function Get_Join_Width: Integer;
    function Get_NM_Width: Integer;
    function Get_SWF_Columns: IXMLSWF_ColumnsType;
    procedure Set_OrigTable(Value: Integer);
    procedure Set_Join_ColumnName(Value: WideString);
    procedure Set_Join_Width(Value: Integer);
    procedure Set_NM_Width(Value: Integer);
    { Methods & Properties }
    property OrigTable: Integer read Get_OrigTable write Set_OrigTable;
    property Join_ColumnName: WideString read Get_Join_ColumnName write Set_Join_ColumnName;
    property Join_Width: Integer read Get_Join_Width write Set_Join_Width;
    property NM_Width: Integer read Get_NM_Width write Set_NM_Width;
    property SWF_Columns: IXMLSWF_ColumnsType read Get_SWF_Columns;
  end;

{ IXMLSWF_ColumnsType }

  IXMLSWF_ColumnsType = interface(IXMLNodeCollection)
    ['{E599C6A1-A674-4CED-A44E-83D962A9949E}']
    { Property Accessors }
    function Get_SWF_Column(Index: Integer): IXMLSWF_ColumnType;
    { Methods & Properties }
    function Add: IXMLSWF_ColumnType;
    function Insert(const Index: Integer): IXMLSWF_ColumnType;
    property SWF_Column[Index: Integer]: IXMLSWF_ColumnType read Get_SWF_Column; default;
  end;

{ IXMLSWF_ColumnType }

  IXMLSWF_ColumnType = interface(IXMLNode)
    ['{0E0D142D-3B40-40C0-851B-6F7201295632}']
    { Property Accessors }
    function Get_OrigCol: Integer;
    function Get_GridName: WideString;
    function Get_FormName: WideString;
    function Get_SelectedForGrid: Boolean;
    function Get_SelectedForForm: Boolean;
    function Get_FixedWidth: Integer;
    function Get_TruncateChars: Integer;
    function Get_IsForeignKey: Boolean;
    function Get_FkTablename: WideString;
    function Get_FkColname: WideString;
    function Get_FormWidth: Integer;
    procedure Set_OrigCol(Value: Integer);
    procedure Set_GridName(Value: WideString);
    procedure Set_FormName(Value: WideString);
    procedure Set_SelectedForGrid(Value: Boolean);
    procedure Set_SelectedForForm(Value: Boolean);
    procedure Set_FixedWidth(Value: Integer);
    procedure Set_TruncateChars(Value: Integer);
    procedure Set_IsForeignKey(Value: Boolean);
    procedure Set_FkTablename(Value: WideString);
    procedure Set_FkColname(Value: WideString);
    procedure Set_FormWidth(Value: Integer);
    { Methods & Properties }
    property OrigCol: Integer read Get_OrigCol write Set_OrigCol;
    property GridName: WideString read Get_GridName write Set_GridName;
    property FormName: WideString read Get_FormName write Set_FormName;
    property SelectedForGrid: Boolean read Get_SelectedForGrid write Set_SelectedForGrid;
    property SelectedForForm: Boolean read Get_SelectedForForm write Set_SelectedForForm;
    property FixedWidth: Integer read Get_FixedWidth write Set_FixedWidth;
    property TruncateChars: Integer read Get_TruncateChars write Set_TruncateChars;
    property IsForeignKey: Boolean read Get_IsForeignKey write Set_IsForeignKey;
    property FkTablename: WideString read Get_FkTablename write Set_FkTablename;
    property FkColname: WideString read Get_FkColname write Set_FkColname;
    property FormWidth: Integer read Get_FormWidth write Set_FormWidth;
  end;

{ IXMLJoinTablesType }

  IXMLJoinTablesType = interface(IXMLNodeCollection)
    ['{138498A7-68EB-4A9A-A94C-9FA641A359BF}']
    { Property Accessors }
    function Get_SWF_Table(Index: Integer): IXMLSWF_TableType;
    { Methods & Properties }
    function Add: IXMLSWF_TableType;
    function Insert(const Index: Integer): IXMLSWF_TableType;
    property SWF_Table[Index: Integer]: IXMLSWF_TableType read Get_SWF_Table; default;
  end;

{ IXMLNMTablesType }

  IXMLNMTablesType = interface(IXMLNodeCollection)
    ['{C1AD2CB0-4E04-4EF0-A573-27AB67AA065F}']
    { Property Accessors }
    function Get_SWF_Table(Index: Integer): IXMLSWF_TableType;
    { Methods & Properties }
    function Add: IXMLSWF_TableType;
    function Insert(const Index: Integer): IXMLSWF_TableType;
    property SWF_Table[Index: Integer]: IXMLSWF_TableType read Get_SWF_Table; default;
  end;

{ IXMLUnassignedViewsType }

  IXMLUnassignedViewsType = interface(IXMLNodeCollection)
    ['{E0E2D81E-FEF8-4992-9047-A1F6556CA88E}']
    { Property Accessors }
    function Get_SWF_View(Index: Integer): IXMLSWF_ViewType;
    { Methods & Properties }
    function Add: IXMLSWF_ViewType;
    function Insert(const Index: Integer): IXMLSWF_ViewType;
    property SWF_View[Index: Integer]: IXMLSWF_ViewType read Get_SWF_View; default;
  end;

{ Forward Decls }

  TXMLSWF_DataType = class;
  TXMLSWF_GroupsType = class;
  TXMLSWF_GroupType = class;
  TXMLSWF_ViewsType = class;
  TXMLSWF_ViewType = class;
  TXMLSWF_TableType = class;
  TXMLSWF_ColumnsType = class;
  TXMLSWF_ColumnType = class;
  TXMLJoinTablesType = class;
  TXMLNMTablesType = class;
  TXMLUnassignedViewsType = class;

{ TXMLSWF_DataType }

  TXMLSWF_DataType = class(TXMLNode, IXMLSWF_DataType)
  protected
    { IXMLSWF_DataType }
    function Get_Username: WideString;
    function Get_Hostname: WideString;
    function Get_Databasename: WideString;
    function Get_Heading: WideString;
    function Get_Layout: WideString;
    function Get_WinSaveDir: WideString;
    function Get_LinuxSaveDir: WideString;
    function Get_SWF_Groups: IXMLSWF_GroupsType;
    function Get_UnassignedViews: IXMLUnassignedViewsType;
    procedure Set_Username(Value: WideString);
    procedure Set_Hostname(Value: WideString);
    procedure Set_Databasename(Value: WideString);
    procedure Set_Heading(Value: WideString);
    procedure Set_Layout(Value: WideString);
    procedure Set_WinSaveDir(Value: WideString);
    procedure Set_LinuxSaveDir(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSWF_GroupsType }

  TXMLSWF_GroupsType = class(TXMLNodeCollection, IXMLSWF_GroupsType)
  protected
    { IXMLSWF_GroupsType }
    function Get_SWF_Group(Index: Integer): IXMLSWF_GroupType;
    function Add: IXMLSWF_GroupType;
    function Insert(const Index: Integer): IXMLSWF_GroupType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSWF_GroupType }

  TXMLSWF_GroupType = class(TXMLNode, IXMLSWF_GroupType)
  protected
    { IXMLSWF_GroupType }
    function Get_Name: WideString;
    function Get_ShowOnLine: Integer;
    function Get_ShowInColumn: Integer;
    function Get_ViewsAsPopup: Boolean;
    function Get_SWF_Views: IXMLSWF_ViewsType;
    procedure Set_Name(Value: WideString);
    procedure Set_ShowOnLine(Value: Integer);
    procedure Set_ShowInColumn(Value: Integer);
    procedure Set_ViewsAsPopup(Value: Boolean);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSWF_ViewsType }

  TXMLSWF_ViewsType = class(TXMLNodeCollection, IXMLSWF_ViewsType)
  protected
    { IXMLSWF_ViewsType }
    function Get_SWF_View(Index: Integer): IXMLSWF_ViewType;
    function Add: IXMLSWF_ViewType;
    function Insert(const Index: Integer): IXMLSWF_ViewType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSWF_ViewType }

  TXMLSWF_ViewType = class(TXMLNode, IXMLSWF_ViewType)
  protected
    { IXMLSWF_ViewType }
    function Get_FormHeight: Integer;
    function Get_FormWidth: Integer;
    function Get_FormX: Integer;
    function Get_FormY: Integer;
    function Get_GridPopupHeight: Integer;
    function Get_GridPopupWidth: Integer;
    function Get_GridPopupX: Integer;
    function Get_GridPopupY: Integer;
    function Get_Name: WideString;
    function Get_WhereClause: WideString;
    function Get_SWF_Table: IXMLSWF_TableType;
    function Get_RowsPerPage: Integer;
    function Get_UseCompoundColNames: Boolean;
    function Get_Icon: WideString;
    function Get_IconFilename: WideString;
    function Get_JoinTables: IXMLJoinTablesType;
    function Get_NMTables: IXMLNMTablesType;
    function Get_GridSortedColumns: WideString;
    function Get_FormSortedColumns: WideString;
    function Get_OrderBy: WideString;
    procedure Set_FormHeight(Value: Integer);
    procedure Set_FormWidth(Value: Integer);
    procedure Set_FormX(Value: Integer);
    procedure Set_FormY(Value: Integer);
    procedure Set_GridPopupHeight(Value: Integer);
    procedure Set_GridPopupWidth(Value: Integer);
    procedure Set_GridPopupX(Value: Integer);
    procedure Set_GridPopupY(Value: Integer);
    procedure Set_Name(Value: WideString);
    procedure Set_WhereClause(Value: WideString);
    procedure Set_RowsPerPage(Value: Integer);
    procedure Set_UseCompoundColNames(Value: Boolean);
    procedure Set_Icon(Value: WideString);
    procedure Set_IconFilename(Value: WideString);
    procedure Set_GridSortedColumns(Value: WideString);
    procedure Set_FormSortedColumns(Value: WideString);
    procedure Set_OrderBy(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSWF_TableType }

  TXMLSWF_TableType = class(TXMLNode, IXMLSWF_TableType)
  protected
    { IXMLSWF_TableType }
    function Get_OrigTable: Integer;
    function Get_Join_ColumnName: WideString;
    function Get_Join_Width: Integer;
    function Get_NM_Width: Integer;
    function Get_SWF_Columns: IXMLSWF_ColumnsType;
    procedure Set_OrigTable(Value: Integer);
    procedure Set_Join_ColumnName(Value: WideString);
    procedure Set_Join_Width(Value: Integer);
    procedure Set_NM_Width(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSWF_ColumnsType }

  TXMLSWF_ColumnsType = class(TXMLNodeCollection, IXMLSWF_ColumnsType)
  protected
    { IXMLSWF_ColumnsType }
    function Get_SWF_Column(Index: Integer): IXMLSWF_ColumnType;
    function Add: IXMLSWF_ColumnType;
    function Insert(const Index: Integer): IXMLSWF_ColumnType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSWF_ColumnType }

  TXMLSWF_ColumnType = class(TXMLNode, IXMLSWF_ColumnType)
  protected
    { IXMLSWF_ColumnType }
    function Get_OrigCol: Integer;
    function Get_GridName: WideString;
    function Get_FormName: WideString;
    function Get_SelectedForGrid: Boolean;
    function Get_SelectedForForm: Boolean;
    function Get_FixedWidth: Integer;
    function Get_TruncateChars: Integer;
    function Get_IsForeignKey: Boolean;
    function Get_FkTablename: WideString;
    function Get_FkColname: WideString;
    function Get_FormWidth: Integer;
    procedure Set_OrigCol(Value: Integer);
    procedure Set_GridName(Value: WideString);
    procedure Set_FormName(Value: WideString);
    procedure Set_SelectedForGrid(Value: Boolean);
    procedure Set_SelectedForForm(Value: Boolean);
    procedure Set_FixedWidth(Value: Integer);
    procedure Set_TruncateChars(Value: Integer);
    procedure Set_IsForeignKey(Value: Boolean);
    procedure Set_FkTablename(Value: WideString);
    procedure Set_FkColname(Value: WideString);
    procedure Set_FormWidth(Value: Integer);
  end;

{ TXMLJoinTablesType }

  TXMLJoinTablesType = class(TXMLNodeCollection, IXMLJoinTablesType)
  protected
    { IXMLJoinTablesType }
    function Get_SWF_Table(Index: Integer): IXMLSWF_TableType;
    function Add: IXMLSWF_TableType;
    function Insert(const Index: Integer): IXMLSWF_TableType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLNMTablesType }

  TXMLNMTablesType = class(TXMLNodeCollection, IXMLNMTablesType)
  protected
    { IXMLNMTablesType }
    function Get_SWF_Table(Index: Integer): IXMLSWF_TableType;
    function Add: IXMLSWF_TableType;
    function Insert(const Index: Integer): IXMLSWF_TableType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLUnassignedViewsType }

  TXMLUnassignedViewsType = class(TXMLNodeCollection, IXMLUnassignedViewsType)
  protected
    { IXMLUnassignedViewsType }
    function Get_SWF_View(Index: Integer): IXMLSWF_ViewType;
    function Add: IXMLSWF_ViewType;
    function Insert(const Index: Integer): IXMLSWF_ViewType;
  public
    procedure AfterConstruction; override;
  end;

{ Global Functions }

function GetSWF_Data(Doc: IXMLDocument): IXMLSWF_DataType;
function LoadSWF_Data(const FileName: WideString): IXMLSWF_DataType;
function NewSWF_Data: IXMLSWF_DataType;
function LoadSWF_DataFromString(xmlData :string): IXMLSWF_DataType;

const
  TargetNamespace = '';

implementation

{ Global Functions }

function GetSWF_Data(Doc: IXMLDocument): IXMLSWF_DataType;
begin
  Result := Doc.GetDocBinding('SWF_Data', TXMLSWF_DataType, TargetNamespace) as IXMLSWF_DataType;
end;

function LoadSWF_Data(const FileName: WideString): IXMLSWF_DataType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('SWF_Data', TXMLSWF_DataType, TargetNamespace) as IXMLSWF_DataType;
end;

function NewSWF_Data: IXMLSWF_DataType;
begin
  Result := NewXMLDocument.GetDocBinding('SWF_Data', TXMLSWF_DataType, TargetNamespace) as IXMLSWF_DataType;
end;

function LoadSWF_DataFromString(xmlData :string): IXMLSWF_DataType;
begin
  Result := LoadXMLData(xmlData).GetDocBinding('SWF_Data', TXMLSWF_DataType, TargetNamespace) as IXMLSWF_DataType;
end;

{ TXMLSWF_DataType }

procedure TXMLSWF_DataType.AfterConstruction;
begin
  RegisterChildNode('SWF_Groups', TXMLSWF_GroupsType);
  RegisterChildNode('UnassignedViews', TXMLUnassignedViewsType);
  inherited;
end;

function TXMLSWF_DataType.Get_Username: WideString;
begin
  Result := ChildNodes['Username'].Text;
end;

procedure TXMLSWF_DataType.Set_Username(Value: WideString);
begin
  ChildNodes['Username'].NodeValue := Value;
end;

function TXMLSWF_DataType.Get_Hostname: WideString;
begin
  Result := ChildNodes['Hostname'].Text;
end;

procedure TXMLSWF_DataType.Set_Hostname(Value: WideString);
begin
  ChildNodes['Hostname'].NodeValue := Value;
end;

function TXMLSWF_DataType.Get_Databasename: WideString;
begin
  Result := ChildNodes['Databasename'].Text;
end;

procedure TXMLSWF_DataType.Set_Databasename(Value: WideString);
begin
  ChildNodes['Databasename'].NodeValue := Value;
end;

function TXMLSWF_DataType.Get_Heading: WideString;
begin
  Result := ChildNodes['Heading'].Text;
end;

procedure TXMLSWF_DataType.Set_Heading(Value: WideString);
begin
  ChildNodes['Heading'].NodeValue := Value;
end;

function TXMLSWF_DataType.Get_Layout: WideString;
begin
  Result := ChildNodes['Layout'].Text;
end;

procedure TXMLSWF_DataType.Set_Layout(Value: WideString);
begin
  ChildNodes['Layout'].NodeValue := Value;
end;

function TXMLSWF_DataType.Get_WinSaveDir: WideString;
begin
  Result := ChildNodes['WinSaveDir'].Text;
end;

procedure TXMLSWF_DataType.Set_WinSaveDir(Value: WideString);
begin
  ChildNodes['WinSaveDir'].NodeValue := Value;
end;

function TXMLSWF_DataType.Get_LinuxSaveDir: WideString;
begin
  Result := ChildNodes['LinuxSaveDir'].Text;
end;

procedure TXMLSWF_DataType.Set_LinuxSaveDir(Value: WideString);
begin
  ChildNodes['LinuxSaveDir'].NodeValue := Value;
end;

function TXMLSWF_DataType.Get_SWF_Groups: IXMLSWF_GroupsType;
begin
  Result := ChildNodes['SWF_Groups'] as IXMLSWF_GroupsType;
end;

function TXMLSWF_DataType.Get_UnassignedViews: IXMLUnassignedViewsType;
begin
  Result := ChildNodes['UnassignedViews'] as IXMLUnassignedViewsType;
end;

{ TXMLSWF_GroupsType }

procedure TXMLSWF_GroupsType.AfterConstruction;
begin
  RegisterChildNode('SWF_Group', TXMLSWF_GroupType);
  ItemTag := 'SWF_Group';
  ItemInterface := IXMLSWF_GroupType;
  inherited;
end;

function TXMLSWF_GroupsType.Get_SWF_Group(Index: Integer): IXMLSWF_GroupType;
begin
  Result := List[Index] as IXMLSWF_GroupType;
end;

function TXMLSWF_GroupsType.Add: IXMLSWF_GroupType;
begin
  Result := AddItem(-1) as IXMLSWF_GroupType;
end;

function TXMLSWF_GroupsType.Insert(const Index: Integer): IXMLSWF_GroupType;
begin
  Result := AddItem(Index) as IXMLSWF_GroupType;
end;

{ TXMLSWF_GroupType }

procedure TXMLSWF_GroupType.AfterConstruction;
begin
  RegisterChildNode('SWF_Views', TXMLSWF_ViewsType);
  inherited;
end;

function TXMLSWF_GroupType.Get_Name: WideString;
begin
  Result := ChildNodes['Name'].Text;
end;

procedure TXMLSWF_GroupType.Set_Name(Value: WideString);
begin
  ChildNodes['Name'].NodeValue := Value;
end;

function TXMLSWF_GroupType.Get_ShowOnLine: Integer;
begin
  Result := ChildNodes['ShowOnLine'].NodeValue;
end;

procedure TXMLSWF_GroupType.Set_ShowOnLine(Value: Integer);
begin
  ChildNodes['ShowOnLine'].NodeValue := Value;
end;

function TXMLSWF_GroupType.Get_ShowInColumn: Integer;
begin
  Result := ChildNodes['ShowInColumn'].NodeValue;
end;

procedure TXMLSWF_GroupType.Set_ShowInColumn(Value: Integer);
begin
  ChildNodes['ShowInColumn'].NodeValue := Value;
end;

function TXMLSWF_GroupType.Get_ViewsAsPopup: Boolean;
begin
  Result := ChildNodes['ViewsAsPopup'].NodeValue;
end;

procedure TXMLSWF_GroupType.Set_ViewsAsPopup(Value: Boolean);
begin
  ChildNodes['ViewsAsPopup'].NodeValue := Value;
end;

function TXMLSWF_GroupType.Get_SWF_Views: IXMLSWF_ViewsType;
begin
  Result := ChildNodes['SWF_Views'] as IXMLSWF_ViewsType;
end;

{ TXMLSWF_ViewsType }

procedure TXMLSWF_ViewsType.AfterConstruction;
begin
  RegisterChildNode('SWF_View', TXMLSWF_ViewType);
  ItemTag := 'SWF_View';
  ItemInterface := IXMLSWF_ViewType;
  inherited;
end;

function TXMLSWF_ViewsType.Get_SWF_View(Index: Integer): IXMLSWF_ViewType;
begin
  Result := List[Index] as IXMLSWF_ViewType;
end;

function TXMLSWF_ViewsType.Add: IXMLSWF_ViewType;
begin
  Result := AddItem(-1) as IXMLSWF_ViewType;
end;

function TXMLSWF_ViewsType.Insert(const Index: Integer): IXMLSWF_ViewType;
begin
  Result := AddItem(Index) as IXMLSWF_ViewType;
end;

{ TXMLSWF_ViewType }

procedure TXMLSWF_ViewType.AfterConstruction;
begin
  RegisterChildNode('SWF_Table', TXMLSWF_TableType);
  RegisterChildNode('JoinTables', TXMLJoinTablesType);
  RegisterChildNode('NMTables', TXMLNMTablesType);
  inherited;
end;

function TXMLSWF_ViewType.Get_FormHeight: Integer;
begin
  Result := ChildNodes['FormHeight'].NodeValue;
end;

procedure TXMLSWF_ViewType.Set_FormHeight(Value: Integer);
begin
  ChildNodes['FormHeight'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_FormWidth: Integer;
begin
  Result := ChildNodes['FormWidth'].NodeValue;
end;

procedure TXMLSWF_ViewType.Set_FormWidth(Value: Integer);
begin
  ChildNodes['FormWidth'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_FormX: Integer;
begin
  Result := ChildNodes['FormX'].NodeValue;
end;

procedure TXMLSWF_ViewType.Set_FormX(Value: Integer);
begin
  ChildNodes['FormX'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_FormY: Integer;
begin
  Result := ChildNodes['FormY'].NodeValue;
end;

procedure TXMLSWF_ViewType.Set_FormY(Value: Integer);
begin
  ChildNodes['FormY'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_GridPopupHeight: Integer;
begin
  Result := ChildNodes['GridPopupHeight'].NodeValue;
end;

procedure TXMLSWF_ViewType.Set_GridPopupHeight(Value: Integer);
begin
  ChildNodes['GridPopupHeight'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_GridPopupWidth: Integer;
begin
  Result := ChildNodes['GridPopupWidth'].NodeValue;
end;

procedure TXMLSWF_ViewType.Set_GridPopupWidth(Value: Integer);
begin
  ChildNodes['GridPopupWidth'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_GridPopupX: Integer;
begin
  Result := ChildNodes['GridPopupX'].NodeValue;
end;

procedure TXMLSWF_ViewType.Set_GridPopupX(Value: Integer);
begin
  ChildNodes['GridPopupX'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_GridPopupY: Integer;
begin
  Result := ChildNodes['GridPopupY'].NodeValue;
end;

procedure TXMLSWF_ViewType.Set_GridPopupY(Value: Integer);
begin
  ChildNodes['GridPopupY'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_Name: WideString;
begin
  Result := ChildNodes['Name'].Text;
end;

procedure TXMLSWF_ViewType.Set_Name(Value: WideString);
begin
  ChildNodes['Name'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_WhereClause: WideString;
begin
  Result := ChildNodes['WhereClause'].Text;
end;

procedure TXMLSWF_ViewType.Set_WhereClause(Value: WideString);
begin
  ChildNodes['WhereClause'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_SWF_Table: IXMLSWF_TableType;
begin
  Result := ChildNodes['SWF_Table'] as IXMLSWF_TableType;
end;

function TXMLSWF_ViewType.Get_RowsPerPage: Integer;
begin
  Result := ChildNodes['RowsPerPage'].NodeValue;
end;

procedure TXMLSWF_ViewType.Set_RowsPerPage(Value: Integer);
begin
  ChildNodes['RowsPerPage'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_UseCompoundColNames: Boolean;
begin
  Result := ChildNodes['UseCompoundColNames'].NodeValue;
end;

procedure TXMLSWF_ViewType.Set_UseCompoundColNames(Value: Boolean);
begin
  ChildNodes['UseCompoundColNames'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_Icon: WideString;
begin
  Result := ChildNodes['Icon'].Text;
end;

procedure TXMLSWF_ViewType.Set_Icon(Value: WideString);
begin
  ChildNodes['Icon'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_IconFilename: WideString;
begin
  Result := ChildNodes['IconFilename'].Text;
end;

procedure TXMLSWF_ViewType.Set_IconFilename(Value: WideString);
begin
  ChildNodes['IconFilename'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_JoinTables: IXMLJoinTablesType;
begin
  Result := ChildNodes['JoinTables'] as IXMLJoinTablesType;
end;

function TXMLSWF_ViewType.Get_NMTables: IXMLNMTablesType;
begin
  Result := ChildNodes['NMTables'] as IXMLNMTablesType;
end;

function TXMLSWF_ViewType.Get_GridSortedColumns: WideString;
begin
  Result := ChildNodes['GridSortedColumns'].Text;
end;

procedure TXMLSWF_ViewType.Set_GridSortedColumns(Value: WideString);
begin
  ChildNodes['GridSortedColumns'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_FormSortedColumns: WideString;
begin
  Result := ChildNodes['FormSortedColumns'].Text;
end;

procedure TXMLSWF_ViewType.Set_FormSortedColumns(Value: WideString);
begin
  ChildNodes['FormSortedColumns'].NodeValue := Value;
end;

function TXMLSWF_ViewType.Get_OrderBy: WideString;
begin
  Result := ChildNodes['OrderBy'].Text;
end;

procedure TXMLSWF_ViewType.Set_OrderBy(Value: WideString);
begin
  ChildNodes['OrderBy'].NodeValue := Value;
end;

{ TXMLSWF_TableType }

procedure TXMLSWF_TableType.AfterConstruction;
begin
  RegisterChildNode('SWF_Columns', TXMLSWF_ColumnsType);
  inherited;
end;

function TXMLSWF_TableType.Get_OrigTable: Integer;
begin
  Result := ChildNodes['OrigTable'].NodeValue;
end;

procedure TXMLSWF_TableType.Set_OrigTable(Value: Integer);
begin
  ChildNodes['OrigTable'].NodeValue := Value;
end;

function TXMLSWF_TableType.Get_Join_ColumnName: WideString;
begin
  Result := ChildNodes['Join_ColumnName'].Text;
end;

procedure TXMLSWF_TableType.Set_Join_ColumnName(Value: WideString);
begin
  ChildNodes['Join_ColumnName'].NodeValue := Value;
end;

function TXMLSWF_TableType.Get_Join_Width: Integer;
begin
  Result := ChildNodes['Join_Width'].NodeValue;
end;

procedure TXMLSWF_TableType.Set_Join_Width(Value: Integer);
begin
  ChildNodes['Join_Width'].NodeValue := Value;
end;

function TXMLSWF_TableType.Get_NM_Width: Integer;
begin
  Result := ChildNodes['NM_Width'].NodeValue;
end;

procedure TXMLSWF_TableType.Set_NM_Width(Value: Integer);
begin
  ChildNodes['NM_Width'].NodeValue := Value;
end;

function TXMLSWF_TableType.Get_SWF_Columns: IXMLSWF_ColumnsType;
begin
  Result := ChildNodes['SWF_Columns'] as IXMLSWF_ColumnsType;
end;

{ TXMLSWF_ColumnsType }

procedure TXMLSWF_ColumnsType.AfterConstruction;
begin
  RegisterChildNode('SWF_Column', TXMLSWF_ColumnType);
  ItemTag := 'SWF_Column';
  ItemInterface := IXMLSWF_ColumnType;
  inherited;
end;

function TXMLSWF_ColumnsType.Get_SWF_Column(Index: Integer): IXMLSWF_ColumnType;
begin
  Result := List[Index] as IXMLSWF_ColumnType;
end;

function TXMLSWF_ColumnsType.Add: IXMLSWF_ColumnType;
begin
  Result := AddItem(-1) as IXMLSWF_ColumnType;
end;

function TXMLSWF_ColumnsType.Insert(const Index: Integer): IXMLSWF_ColumnType;
begin
  Result := AddItem(Index) as IXMLSWF_ColumnType;
end;

{ TXMLSWF_ColumnType }

function TXMLSWF_ColumnType.Get_OrigCol: Integer;
begin
  Result := ChildNodes['OrigCol'].NodeValue;
end;

procedure TXMLSWF_ColumnType.Set_OrigCol(Value: Integer);
begin
  ChildNodes['OrigCol'].NodeValue := Value;
end;

function TXMLSWF_ColumnType.Get_GridName: WideString;
begin
  Result := ChildNodes['GridName'].Text;
end;

procedure TXMLSWF_ColumnType.Set_GridName(Value: WideString);
begin
  ChildNodes['GridName'].NodeValue := Value;
end;

function TXMLSWF_ColumnType.Get_FormName: WideString;
begin
  Result := ChildNodes['FormName'].Text;
end;

procedure TXMLSWF_ColumnType.Set_FormName(Value: WideString);
begin
  ChildNodes['FormName'].NodeValue := Value;
end;

function TXMLSWF_ColumnType.Get_SelectedForGrid: Boolean;
begin
  Result := ChildNodes['SelectedForGrid'].NodeValue;
end;

procedure TXMLSWF_ColumnType.Set_SelectedForGrid(Value: Boolean);
begin
  ChildNodes['SelectedForGrid'].NodeValue := Value;
end;

function TXMLSWF_ColumnType.Get_SelectedForForm: Boolean;
begin
  Result := ChildNodes['SelectedForForm'].NodeValue;
end;

procedure TXMLSWF_ColumnType.Set_SelectedForForm(Value: Boolean);
begin
  ChildNodes['SelectedForForm'].NodeValue := Value;
end;

function TXMLSWF_ColumnType.Get_FixedWidth: Integer;
begin
  Result := ChildNodes['FixedWidth'].NodeValue;
end;

procedure TXMLSWF_ColumnType.Set_FixedWidth(Value: Integer);
begin
  ChildNodes['FixedWidth'].NodeValue := Value;
end;

function TXMLSWF_ColumnType.Get_TruncateChars: Integer;
begin
  Result := ChildNodes['TruncateChars'].NodeValue;
end;

procedure TXMLSWF_ColumnType.Set_TruncateChars(Value: Integer);
begin
  ChildNodes['TruncateChars'].NodeValue := Value;
end;

function TXMLSWF_ColumnType.Get_IsForeignKey: Boolean;
begin
  Result := ChildNodes['IsForeignKey'].NodeValue;
end;

procedure TXMLSWF_ColumnType.Set_IsForeignKey(Value: Boolean);
begin
  ChildNodes['IsForeignKey'].NodeValue := Value;
end;

function TXMLSWF_ColumnType.Get_FkTablename: WideString;
begin
  Result := ChildNodes['FkTablename'].Text;
end;

procedure TXMLSWF_ColumnType.Set_FkTablename(Value: WideString);
begin
  ChildNodes['FkTablename'].NodeValue := Value;
end;

function TXMLSWF_ColumnType.Get_FkColname: WideString;
begin
  Result := ChildNodes['FkColname'].Text;
end;

procedure TXMLSWF_ColumnType.Set_FkColname(Value: WideString);
begin
  ChildNodes['FkColname'].NodeValue := Value;
end;

function TXMLSWF_ColumnType.Get_FormWidth: Integer;
begin
  Result := ChildNodes['FormWidth'].NodeValue;
end;

procedure TXMLSWF_ColumnType.Set_FormWidth(Value: Integer);
begin
  ChildNodes['FormWidth'].NodeValue := Value;
end;

{ TXMLJoinTablesType }

procedure TXMLJoinTablesType.AfterConstruction;
begin
  RegisterChildNode('SWF_Table', TXMLSWF_TableType);
  ItemTag := 'SWF_Table';
  ItemInterface := IXMLSWF_TableType;
  inherited;
end;

function TXMLJoinTablesType.Get_SWF_Table(Index: Integer): IXMLSWF_TableType;
begin
  Result := List[Index] as IXMLSWF_TableType;
end;

function TXMLJoinTablesType.Add: IXMLSWF_TableType;
begin
  Result := AddItem(-1) as IXMLSWF_TableType;
end;

function TXMLJoinTablesType.Insert(const Index: Integer): IXMLSWF_TableType;
begin
  Result := AddItem(Index) as IXMLSWF_TableType;
end;

{ TXMLNMTablesType }

procedure TXMLNMTablesType.AfterConstruction;
begin
  RegisterChildNode('SWF_Table', TXMLSWF_TableType);
  ItemTag := 'SWF_Table';
  ItemInterface := IXMLSWF_TableType;
  inherited;
end;

function TXMLNMTablesType.Get_SWF_Table(Index: Integer): IXMLSWF_TableType;
begin
  Result := List[Index] as IXMLSWF_TableType;
end;

function TXMLNMTablesType.Add: IXMLSWF_TableType;
begin
  Result := AddItem(-1) as IXMLSWF_TableType;
end;

function TXMLNMTablesType.Insert(const Index: Integer): IXMLSWF_TableType;
begin
  Result := AddItem(Index) as IXMLSWF_TableType;
end;

{ TXMLUnassignedViewsType }

procedure TXMLUnassignedViewsType.AfterConstruction;
begin
  RegisterChildNode('SWF_View', TXMLSWF_ViewType);
  ItemTag := 'SWF_View';
  ItemInterface := IXMLSWF_ViewType;
  inherited;
end;

function TXMLUnassignedViewsType.Get_SWF_View(Index: Integer): IXMLSWF_ViewType;
begin
  Result := List[Index] as IXMLSWF_ViewType;
end;

function TXMLUnassignedViewsType.Add: IXMLSWF_ViewType;
begin
  Result := AddItem(-1) as IXMLSWF_ViewType;
end;

function TXMLUnassignedViewsType.Insert(const Index: Integer): IXMLSWF_ViewType;
begin
  Result := AddItem(Index) as IXMLSWF_ViewType;
end;

end. 