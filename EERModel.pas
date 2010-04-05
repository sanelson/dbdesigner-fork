unit EERModel;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of DBDesigner Fork which is forked from DBDesigner 4.
//
// DBDesigner Fork is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// DBDesigner Fork is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with DBDesigner Fork; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------
//
// Unit EERModel.pas
// -----------------
// Version Fork 1.0, 18.09.2006, JP
// Version 2.2, 01.08.2003, Mike
// Description
//   Contains all classes for the EERModel
//
// Changes:
//   Version Fork 1.5, 31.03.2010, JP.
//     added Outputs commets for MYSQL.
//     added support for PostgreSQL "SERIAL" type.
//   Version Fork 1.0, 22.09.2006, Carlos
//     added option to create auto increment by trigger to Oracle/FireBird
//     added option to create a special column to save date of record created/changed
//   Version Fork 1.0, 18.09.2006, JP
//     added export SQL options for Oracle, SQL Server and FireBird.
//   Version 2.2, 01.08.2003, Mike
//     New XML Parser
//   Version 2.1, 03.05.2003, Mike
//     Constraint object mode when user holds shift
//     Apply datatype and Options to FK columns when Source Key was changed
//   Version 2.0, 18.04.2003, Mike
//     Changed all Records to TObjects.
//   Version 1.9, 15.04.2003, Mike
//     Included automatic fix of ENUM/SET datatype defs when loaded from model / inifile
//   Version 1.8, 11.04.2003, Mike
//     1.6 was buggy again, now FK References fixed for MySQL4.0 INNODB,
//     added FKPrefix, FKPostfix
//     added TEER_Datatype.EditParamsAsString to support ENUM and SET datatypes
//   Version 1.7, 08.04.2003, Mike
//     Changed initial RefDef to NoAction for OnDelete/OnUpdate
//   Version 1.6, 08.04.2003, Mike
//     Fixed bug in TEERTable.GetSQLCreateCode, FK References were created
//     using a wrong syntax
//   Version 1.5, 04.04.2003, Mike / Shannon Weyrick
//     Added TableNameInRefs param and functionality
//     Get default settings for PositionGrid and TableNameInRefs from DMEER
//   Version 1.4, 28.03.2003, Mike
//     Added PositionGrid Parameters to model and implemtented functionality
//     Store and Load PositionGrid Parameters
//   Version 1.3, 25.03.2003, Mike
//     Changes TEERImage.SetXML and TEERImage.GetXML to support new
//     Encoding/Decoding functions in DMMain using a memory stream
//   Version 1.2, 21.03.2003, Mike
//     EERModel.LoadFromFile: When getting MaxID of the model,
//       ColumnIDs and IndexIDs were ignored - fixed.
//   Version 1.1, 20.03.2003, Mike
//     added PrevTableName for DBSync (rename table cmds)
//     Indices now store the IndexColumn parmeter length (for blob fields)
//     Added AddToLog parameter for DeleteSelectedObjs and change call in
//       EERModel LoadFromFile
//   Version 1.0, 13.03.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

{$I DBDesigner4.inc}

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QImgList, QMenus, QTypes, IniFiles, Math, StrUtils,
  QPrinters, QClipbrd, QComCtrls, Qt,
  {$IFDEF USE_IXMLDBMODELType}
  XMLDoc,
  EERModel_XML,
  {$ENDIF}
  Contnrs,
  LibXmlParser;

type
  TEERObject = (EERAllObjects, EERNote, EERRegion, EERRelation, EERTable, EERImage, EERStoredProc);
  TEERObjectSet = set of TEERObject;

  TEERObjectAlign = (EERObjAlignLeft, EERObjAlignRight, EERObjAlignTop, EERObjAlignBottom, EERObjAlignCenterH, EERObjAlignCenterV, EERObjAlignDistributeH, EERObjAlignDistributeV);

  //-----------------------------------------------
  TEERPluginData  = class(TPersistent)
    constructor Create(AOwner: TComponent); overload;
    destructor Destroy; override;
  public
    Owner: TComponent;

    PluginName: string;       // Name of the Plugin which stored the Data
    Obj_id: integer;          // ID of the Object the Data is related to, <0 if non
    Params: TStringList;      // Stringlist which stores the values
    Data: Pointer;            // Additional binary data (NOTE: the data is
                              // not stored, for temporary use only)
  end;

  TEERLinkedModel  = class(TPersistent)
    constructor Create(AOwner: TComponent); overload;
  public
    Owner: TComponent;

    IDLinkedModel: integer;

    ModelName: string;
    IDModel: integer;

    IsStoredInDB: Boolean;
    ModelFilename: string;
    DriverName, DBConnName, HostCaption,
    HostName, Database, User: string;
  end;

  // -----------------------------------------------
  // Declaration of the MAIN-Class

  TEERModel = class(TPanel)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //Load initial Datatypes from ini-file
    procedure LoadDataTypesFromIniFile;

    //Paint Model function
    procedure PaintModel(theCanvas: TCanvas; theZoomfac: double = -1;
      x: integer = 0; y: integer = 0; w: integer = 0; h: integer = 0;
      Objs2Paint: TEERObjectSet = [EERAllObjects];
      theDPI: integer = 72; doDrawText: Boolean = True);
    procedure PaintModelToImage(ModelBmp: TBitmap; PaintSelectedOnly: Boolean = False);
    procedure DrawRelIcon(theCanvas: TCanvas; theRect: TRect; theBmpNr: integer);

    //Place new Objects
    function NewTable(x, y: integer; LogTheAction: Boolean): Pointer;
    function NewRelation(RelationKind: integer;
      theSrcTable, theDestTable: Pointer; LogTheAction: Boolean; FKColumnName: string = ''): Pointer;
    function NewNote(x, y: integer; LogTheAction: Boolean): Pointer;
    function NewRegion(x, y, w, h: integer; LogTheAction: Boolean): Pointer;
    procedure SendRegionsToBack;
    function NewImage(x, y, w, h: integer; LogTheAction: Boolean): Pointer;

    //Set, Get ModelName
    procedure SetModelName(name: string);
    function GetModelName: string;


    //Set Model changed
    procedure ModelHasChanged;

    //Zoom Functions
    procedure SetZoomFac(NewZoomFac: double;  X: integer = -1; Y: integer = -1);
    function GetZoomFac: double;
    procedure ZoomIn(x, y: integer);
    procedure ZoomOut(x, y: integer);
    //Calculate a value by the Zoom Factor
    function EvalZoomFac(thevalue: integer): integer;
    function ReEvalZoomFac(thevalue: integer): integer;
    //PositionMarker functions
    procedure SetPositionMarker(nr: integer);
    procedure GotoPositionMarker(nr: integer);

    //Font functions
    function GetFontHeight: integer;
    function GetTextExtent(s: string): TSize;

    //Load the reserved word list
    procedure LoadReservedWordsFromIniFile;
    //Check Functions
    function CheckReservedWord(s: string): Boolean;
    procedure CheckAllRelations;

    //Load and store functions
    procedure SaveToFile(fname: string; WriteSettings: Boolean = True;
      OnlySelected: Boolean = False; UpdateModelFileName: Boolean = True);
    procedure LoadFromFile(fname: string; ReadSettings: Boolean = True; SelectObjs: Boolean = False; MoveToSavedPosition: Boolean = True; AddDataToExistingModel: Boolean = True);
    procedure LoadFromFile2(fname: string; ReadSettings: Boolean = True; SelectObjs: Boolean = False; MoveToSavedPosition: Boolean = True; AddDataToExistingModel: Boolean = True);

    //Selection Functions
    function GetSelectedObjsCount: integer;
    function GetFirstSelectedObj: TObject;
    procedure SelectAllObjs;
    procedure DeSelectAllObjs(ExcludeObj: TObject);
    procedure DeleteSelectedObjs(AddToLog: Boolean = True);
    //Aligns Selected Objs
    procedure AlignSelectedObjs(alignPos: TEERObjectAlign);

    //Refresh Functions
    procedure Refresh;
    procedure RefreshFont;
    procedure RefreshTblImgs;

    //Action functions
    procedure LogAction(ActionType, Obj_id: integer; Params: String);
    procedure StartSubActionLog(ActionType: integer);
    procedure LogSubAction(ActionSubType, Obj_id: integer; Params: String);
    procedure EndSubAction;
    procedure DeleteAction(theActionPointer: Pointer);
    procedure DeleteOpenAction;
    procedure UndoActions(TillAction: integer);
    procedure RedoActions(TillAction: integer);
    function GetActionName(ActionType: integer): string;
    function GetSubActionOfObj(ActionIndex, Obj_id: integer): Pointer;

    //Datatype functions
    function GetDataTypeName(id: integer): string;
    function GetDataTypeGroup(id: integer): integer;
    function GetDataType(id: integer): Pointer;
    function GetDataTypeByName(name: string): Pointer;
    function GetDataTypeByNameSubst(DatatypeName: string; DatatypeSubstList: TStringList): Pointer;

    //MouseOverObj functions
    procedure SetMouseOverObj(Obj: TObject; SubObj: Pointer);
    procedure ClearMouseOverObj;
    function GetMouseOverObj: TObject;
    function GetMouseOverSubObj: Pointer;

    //Plugin functions
    function GetPluginDataCount(PluginName: string): integer;
    function GetPluginDataByIndex(PluginName: string; Index: integer): TEERPluginData;
    function GetPluginDataByID(PluginName: string; ID: integer): TEERPluginData;
    function AddPluginData(PluginName: string; ID: integer): TEERPluginData;

    //EERObj List functions
    function GetEERObjectCount(ObjType: TEERObjectSet): integer;
    procedure GetEERObjectNameList(ObjType: TEERObjectSet; ObjectnamesList: TStringList; OnlySelected: Boolean=False);
    procedure GetEERObjectList(ObjType: TEERObjectSet; ObjectList: TList; OnlySelected: Boolean=False);
    procedure SortEERObjectListByObjName(ObjectList: TList);
    procedure SortEERObjectListByOrderPos(ObjectList: TList);

    //EERObj functions
    function GetEERObjectByID(id: integer): Pointer;
    function GetEERObjectByLinkedID(id: integer): Pointer;
    function GetEERObjectByIndex(ObjType: TEERObject; Index: integer): Pointer;
    function GetEERObjectByName(ObjType: TEERObject; Name: string): Pointer;
    function GetEERObjectClassName(ObjType: TEERObject): string;

    //EERTable functions
    function GetEERTableByColumnID(id: integer): Pointer;
    function GetEERIndexByID(id: integer): Pointer;
    //sorts all tables in the given list by the FK Reference
    //so the tables can be created / dropped in the correct order
    procedure SortEERTableListByForeignKeyReferences(EERTableList: TList);

    //StoredSQLCmd functions
    function GetStoredSQLCmdIndex(SQLCmdType: integer; StoredPosition: string): integer;

    procedure MoveSelectedEERObjects(x, y: integer);

    function GetNextIDPlacedModel: integer;
    function GetPlacedModelByID(id: integer): TEERLinkedModel;
  protected
    //Handle to Mouse Events
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;

    //Load needed images
    procedure AddImgToPopupImgList(fname: string);
    procedure LoadModelBitmaps;

    //CheckHelp Function
    procedure CheckRelations(Tbl2Refresh: TList);

    //Selection Rect functions
    procedure DoSelectionRectPaint(Sender: TObject);
    procedure SetSelectionRectPos(l, t, w, h: integer);
    procedure GetSelectionRectPos(var selRect: TRect);
    function GetSelectionRectVisible: Boolean;
    procedure HideSelectionRect;
    procedure SelectObjsInSelectionRect;

    //Paint grid Box --> needs to be replaced
    procedure DoGridPaintBoxPaint(Sender: TObject);

    //Decode Datatypes ini-file data
    procedure DecodeDataTypes(s: string; p: Pointer);

    //Help function to move all selected objects
    procedure InitialMove4AllSelectedObjs(Obj: TObject);

    //Popup Menu functions
    procedure CreatePopupMenus(AOwner: TComponent);
    function GetGeneralObjPopupMenu(AOwner: TComponent; nr: integer): TPopupMenu;
    function GetThePopupComponent(Sender: TObject): TComponent;

    //Popup events
    procedure PopupMenuSelectObj(Sender: TObject);
    procedure PopupMenuEditObj(Sender: TObject);
    procedure PopupMenuRefreshObj(Sender: TObject);
    procedure PopupMenuDeleteObj(Sender: TObject);
    procedure PopupMenuAlignObjs(Sender: TObject);
    procedure PopupMenuCopyTableName(Sender: TObject);
    procedure PopupMenuCopyTableFieldName(Sender: TObject);
    procedure PopupMenuCopyTableFieldNameShow(Sender: TObject);
    procedure PopupMenuCopyTableFields(Sender: TObject);
    procedure PopupMenuCopyTableSQLCreate(Sender: TObject);
    procedure PopupMenuCopyTableSQLDrop(Sender: TObject);
    procedure PopupMenuCopyTableSQLInsert(Sender: TObject);
    procedure PopupMenuSelectRegion(Sender: TObject);
    procedure PopupMenuEditTable(Sender: TObject);
  private
    { Private declarations }
    ModelName: string;

    //The current Zoom factor of the EER Model (start @ 100%)
    ZoomFac: double;

    //Vars for MouseAction
    mouse_posx, mouse_posy, mouse_absx, mouse_absy: integer;
    MouseIsDown: Boolean;

    //Counter for new Tables (Table01)
    NewTableCounter, NewRelCounter,
    NewNoteCounter, NewRegionCounter,
    NewImageCounter,
    LastRegionColor: integer;

    TblHeaderBmp, TblHeaderRightBmp,
    TblHeaderLinkedBmp, TblHeaderRightLinkedBmp,
    FieldBmp, FieldKeyBmp,
    IndexBmp,
    Field_FKBmp,
    Index_FKBmp: TBitmap;

    //Rel_Bmp: Array[0..11] of TBitmap;
    PopUpMenuImgs: TImageList;

    RelIconSize, RelIconDSize: integer;

    //Selection PaintBox
    SelectionRect: TPaintBox;

    MouseOverObj: TObject;
    MouseOverSubObj: Pointer;

    DisableModelRefresh: Boolean;

    //Reserved Words
    ReservedWords: TStringList;
  public
    { Public declarations }

    //Name and comments
    ModelComments: string;

    //Model File Name
    ModelFilename: string;

    //Database type
    DatabaseType: string;

    //Model
    EERModel_Width: integer;
    EERModel_Height: integer;

    //Table Prefixes
    DefaultTablePrefix: integer;
    TablePrefix: TStringList;

    //Datatypes
    DatatypeGroups: TObjectList;
    Datatypes: TObjectList;
    DefaultDataType: integer;
    CommonDataType: TStringList;

    //Default Database-Connections for the diff. functions
    DefSyncDBConn,
    DefSaveDBConn,
    DefQueryDBConn: string;

    //Flags
    UseVersionHistroy,
    AutoIncVersion,
    IsChanged,
    Need2RefreshNavImg,
    LogActions: Boolean;

    //Model Version Control
    IDModel,
    IDVersion: integer;
    VersionStr: string;

    //Temporary store the two tables for a relation
    Rel_SrcTable, Rel_DestTable: TObject;

    //Grid PaintBox
    GridPaintBox: TPaintBox;

    //Size of a Page which will be printed on the printer's canvas
    ModelPrinter: string;
    PageSize: TSize;
    PageAspectRatio,
    HPageCount,
    VPageCount: double;
    PageOrientation: TPrinterOrientation;
    PageFormat: string;
    SelectedPages: Array[0..200] of Boolean;
    //DPI of the canvas to draw on
    DPI: integer;

    //Default Model font
    DefModelFont: string;

    //Indicates wether the Model is drawn to a special canvas
    PaintingToSpecialCanvas: Boolean;

    //Actions
    CurrentAction: Integer;
    ActionLog: TObjectList;

    //The PopupMenus
    PopupMenuEERTable,
    PopupMenuEERRelation,
    PopupMenuEERNote,
    PopupMenuEERImage,
    PopupMenuEERRegion: TPopupMenu;

    //Store the last selected Editor Pages
    LastTableEditorPage,
    LastRelEditorPage: integer;

    //Region Colors
    RegionColors: TStringList;

    //Plugin Data
    PluginData: TObjectList;

    //Stored SQLCmds
    StoredSQLCmds: TObjectList;

    //PositionMarkers
    PosMarkers: TObjectList;

    UsePositionGrid: Boolean;
    PositionGrid: TPoint;

    TableNameInRefs: Boolean;
    DefaultTableType: integer;
    ActivateRefDefForNewRelations: Boolean;

    FKPrefix, FKPostfix: string;

    CreateFKRefDefIndex: Boolean;

    //This flag tells the Objects that they don't have
    //to update other objects because all objects are being cleared
    ModelIsBeingCleared: Boolean;

    DBQuoteCharacter: string;

    ReadOnly: Boolean;

    LinkedModels: TObjectList;
    CreateSQLforLinkedObjects: Boolean;
  end;

  // -----------------------------------------------
  // Declaration of the EERModel - Objects (EER_Table, EER_Rel, EER_Note, ...)

  TEERObj = class(TPaintBox)
    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;
    function ObjIsEqualTo(Source: TObject): Boolean;

    //Displays the Object's Editor
    procedure ShowEditor(Sender: TObject); virtual; abstract;

    //Returns the Obj as XML Model code (for copy/paste and actions)
    function GetObjAsXMLModel: string;

    //Get the Region the Obj is in
    function GetRegion: Pointer;

    //Paint the Object
    procedure PaintObj(theCanvas: TCanvas; theZoomfac: double = -1;
      x: integer = 0; y: integer = 0; w: integer = 0; h: integer = 0);
    procedure PaintMouseOver; virtual;
    procedure PaintMouseOverClear; virtual;

    //Event, is defined as abstract, so it has to be overwriten
    procedure DoPaint(Sender: TObject); virtual; abstract;

    //Refresh Obj (with new Zoom factor) but does no paint
    procedure RefreshObj(Sender: TObject = nil); virtual; abstract;

    //Get and set Selection State
    function Selected: Boolean;
    procedure SetSelected(select: Boolean); virtual;

    //Selects the Obj
    procedure SelectObj(Sender: TObject);
    //Deletes the Object
    procedure DeleteObj; virtual; abstract;
  protected
    //Call Editor
    procedure DoDblClick(Sender: TObject);

    //Define Mouse Actions procedures as abstract, so they have to be overwritten
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    procedure DoMouseEnter(Sender: TObject);
    procedure DoMouseLeave(Sender: TObject);

    //Drag'n'Drop
    procedure DoOnStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure DoOnEndDrag(Sender, Target: TObject; X,
      Y: Integer);

    //Paints the Obj, is defined as abstract, so it has to be overwriten
    procedure PaintObj2Canvas(theCanvas: TCanvas; xo, yo: integer); virtual; abstract;

    //Calculates ScreenCoordinates to EER-Coordinates
    function EvalZoomFac(thevalue: integer): integer;
    //Calculates EER-Coordinates to ScreenCoordinates
    function ReEvalZoomFac(thevalue: integer): integer;

    //Get Obj as XML
    function GetXML: string; virtual; abstract;
  private
    { Private declarations }
    IsSelected: Boolean;

  public
    { Public declarations }
    ParentEERModel: TEERModel;

    Obj_id,
    Obj_X, Obj_Y, Obj_W, Obj_H: integer;

    ObjName: string;

    Comments: String;

    //Vars for MouseAction
    mouse_posx, mouse_posy, mouse_absx, mouse_absy: integer;
    MouseIsDown,
    EditorIsCalled: Boolean;

    ObjChanged: Boolean;

    IsLinkedObject: Boolean;
    IDLinkedModel,
    Obj_id_Linked: integer;

    OrderPos: integer;
  end;


  // -----------------------------------------------
  // Declaration of the EER Table Object

  TEERTable = class(TEERObj)
    constructor Create(AOwner: TComponent; TheName: string; FontName: string; theTableType: integer; theTablePrefix: integer; thePopupMenu: TPopupMenu); reintroduce; overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function ObjIsEqualTo(Source: TObject): Boolean; overload;

    //Displays the Obj's Editor
    procedure ShowEditor(Sender: TObject); override;

    //Event
    procedure DoPaint(Sender: TObject); override;

    //Draw mouse over Object
    procedure PaintMouseOver; override;
    procedure PaintMouseOverClear; override;

    //Paint the Object
    procedure PaintObj2Canvas(theCanvas: TCanvas; xo, yo: integer); override;

    //Refresh the table
    procedure RefreshObj(Sender: TObject = nil); override;
    //Delete the Obj
    procedure DeleteObj; override;

    //Refresh all Relations of this table
    procedure RefreshRelations(preventRecursion: Boolean = False);

    //Get the offset value of the specified Relation
    //(when there are more than one relation on one side of the table)
    function GetRelationOffset(theRel: Pointer): integer;

    //Checks the primary index and returns the obj_id or -1
    function CheckPrimaryIndex: integer;
    procedure PrimaryColumnsFirst;

    //Returns the table's prefix
    function GetTablePrefix: String;

    //Get SQL Codes
    function GetSQLTableName(DoNotAddPrefix: Boolean = False): string;

    function GetSQLCreateCode(DefinePK: Boolean = True;
      CreateIndices: Boolean = True; DefineFK: Boolean = False;
      TblOptions: Boolean = True; StdInserts: Boolean = False;
      OutputComments: Boolean = False;
      HideNullField:boolean = false; // indicate that things like "col1 char(1) null," will not apear. What will apear is "col1 char(1),"
      PortableIndices : Boolean = false; // separated create indices;
      HideOnDeleteUpdateNoAction : boolean = false; // hide ON (DELETE / UPDATE) NO ACTION
      GOStatement : boolean = false; //usefull for SQL Server
      CommitStatement : boolean = false; //needed for ORACLE Inserts
      FKIndex : boolean = false; // indicates when FKs indexes should be made (Oracle needs it)
      DefaultBeforeNotNull : boolean = false; // Defalut tag should appear before NOT NULL
      DatabaseType: string = 'My SQL'; //informs for waht database is beeing exported
      SeqName: String = 'GlobalSequence'; //informs triggers name for each table has auto increment
      PrefixName: String = 'AINC_'; //Informs the prefix name of the triggers names
      CreateAutoInc: boolean = false; //if true, sequences and triggers are created to Oracle or FireBird
      CreateLastChage: boolean = false;  //if true, the columns to save record changes are created
      LastChangeDateCol: string = 'LAST_CHANGE_DATE'; //informs column name to save the date of data alteration
      LastChangeUserCol: string = 'USERID'; //informs the column name to save user ID
      LastChangePrefix: string = 'UPDT_'; //informs the prefix to triggers name
      CreateLastExclusion: boolean = false;
      LastExclusionTbName: string = 'DT_EXCLUSION';
      LastExclusionColName: string = 'EX_DATE';
      lastExclusionTriggerPrefix: string = 'EXCDT_'
      ): string;

    // get SQL table comment
    function getSqlTableComment(TableName, Comment, DatabaseType:string): string;

    // get SQL column comment
    function getSqlColumnComment(TableName, ColumnName, Comment, DatabaseType:string): string;

    //get SQL triggers to sequences/generators
    function getTriggerForSequences(SeqName, DatabaseType, PrefixName, Field:string): string;

    function GetTriggersForLastChangeDate(ColumnName, PrefixName, DBType: string;
      pkFields: TStringList): string;

    function GetTriggerForLastDeleteDate(TbName, ColName, PrefixName, DbType: string): string;

    //Create sql tiggers definitions
    function GetTriggerSql(DbType, TriggerBody, Table, TriggerName,
      TriggerEvent: String): String;

    function GetPkJoin(Tab1, Tab2: String; PKs: TStringList): String;

    function GetSQLColumnCreateDefCode(i: integer;
                                       var TableFieldGen: string;
                                       HideNullField:boolean = false; // indicate that things like "col1 char(1) null," will not apear. What will apear is "col1 char(1),"
                                       DefaultBeforeNotNull : boolean = false; // Defalut tag should appear before NOT NULL
                                       DatabaseType: string = 'My SQL'; //database type
                                       OutputComments: boolean = false
                                       ): string;
    function GetSQLDropCode(IfExists:boolean = false): string;
    function GetSQLInsertCode: string;

    //Get an set the XML code of the Table
    function GetXML: string; override;
{$IFDEF USE_IXMLDBMODELType}
    procedure SetXML(theXMLTable: IXMLTABLEType);
{$ENDIF}
    procedure SetXML2(theXMLParser: TXmlParser);

    //Copy to clipboard functions
    procedure CopyTableName(Sender: TObject);
    procedure CopyTableFields(Sender: TObject);
    procedure CopyTableSQLInsert(Sender: TObject);
    procedure CopyTableSQLCreate(Sender: TObject);
    procedure CopyTableSQLDrop(Sender: TObject);

    //Column functions
    function GetColumnCount: integer;
    function GetColumnByIndex(index: integer): Pointer;
    function GetColumnByID(idColumn: integer): Pointer;
    function GetColumnByName(cname: string): Pointer;
    procedure DeleteColumn(index: integer);

    //nm-Table functions
    function GetnmTableStatus: Boolean;
    procedure SetnmTableStatus(isnmTable: Boolean);

  protected
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoClick(Sender: TObject);

    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);

    procedure PaintCachedImg(theCanvas: TCanvas; xo: integer = 0; yo: integer = 0);
  private
    { Private declarations }

    nmTable: Boolean;

  public
    { Public declarations }
    TableType,
    TablePrefix: integer;
    Temporary: Boolean;

    PrevTableName: string;

    TableOptions: TStringList;

    StandardInserts: TStringList;
    UseStandardInserts: Boolean;


    Columns: TObjectList;
    Indices: TObjectList;

    //List of ALL Relations Starting and Ending at this table
    RelStart, RelEnd: TList;

    // Lists of Relations on each side
    Rel: Array[1..4] of TList;

    RefreshStrechedImg: Boolean;
    StrechedImg: TBitmap;
    CachedTblXML: string;

    Collapsed: Boolean;

    tmp: string;
  end;

  // -----------------------------------------------
  // Declaration of the EER Rel Object

  TEERRel = class(TEERObj)
    constructor Create(AOwner: TComponent; TheName: string); reintroduce; overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure ShowEditor(Sender: TObject); override;

    procedure DoPaint(Sender: TObject); override;

    //Paint the Object
    procedure PaintObj2Canvas(theCanvas: TCanvas; xo, yo: integer); override;

    procedure PaintObj2(theCanvas: TCanvas; theZoomfac: double = -1;
      x: integer = 0; y: integer = 0; w: integer = 0; h: integer = 0; SubObj: integer = 1);

    // Caution: if Sender<>nil the RefreshObj WILL NOT call itself over the
    // RefreshRelations function preventing infinite recursive loop
    procedure RefreshObj(Sender: TObject = nil); override;

    //Delete the Obj
    procedure DeleteObj; override;

    function GetRelDirection: integer;

    function GetIntervalTxt(StartInterval: Boolean): string;

    function GetXML: string; override;
{$IFDEF USE_IXMLDBMODELType}
    procedure SetXML(theXMLRelation: IXMLRELATIONType);
{$ENDIF}
    procedure SetXML2(theXMLParser: TXmlParser);

  protected
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure PaintObj2Canvas_RelStart(theCanvas: TCanvas; xo, yo: integer);
    procedure PaintObj2Canvas_RelMiddle(theCanvas: TCanvas; xo, yo: integer);
    procedure PaintObj2Canvas_RelEnd(theCanvas: TCanvas; xo, yo: integer);
    procedure PaintObj2Canvas_RelCaption(theCanvas: TCanvas; xo, yo: integer);
    procedure PaintObj2Canvas_RelStartInterval(theCanvas: TCanvas; xo, yo: integer);
    procedure PaintObj2Canvas_RelEndInterval(theCanvas: TCanvas; xo, yo: integer);

    procedure DoPaint_RelMiddle(Sender: TObject);
    procedure DoPaint_RelEnd(Sender: TObject);
    procedure DoPaint_Caption(Sender: TObject);
    procedure DoPaint_StartInterval(Sender: TObject);
    procedure DoPaint_EndInterval(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    RelKind: integer;

    SrcTbl, DestTbl: TEERTable;
    FKFields, //Stores the names of Source and Dest. Fields
    FKFieldsComments: TStringList; //Stores Comments

    CreateRefDef: Boolean;
    RefDef: TStringList;
    FKRefDefIndex_Obj_id: integer;

    Invisible: Boolean;
    Splitted: Boolean;

    RelMiddle, RelEnd,
    RelCaption: TPaintBox;
    RelStartInterval,
    RelEndInterval: TPaintBox;

    OptionalStart, OptionalEnd: Boolean;

    relDirection, relMidDirection: integer;

    MidOffset, tempMidOffset: integer;
    CaptionOffset, tempOffset: TPoint;
    StartIntervalOffset,
    EndIntervalOffset: TPoint;

    tempW, tempH: integer;
  end;

  // -----------------------------------------------
  // Declaration of the EER Note Object

  TEERNote = class(TEERObj)
    constructor Create(AOwner: TComponent; TheName: string); reintroduce; overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure ShowEditor(Sender: TObject); override;

    procedure DoPaint(Sender: TObject); override;

    //Paint the Object
    procedure PaintObj2Canvas(theCanvas: TCanvas; xo, yo: integer); override;

    procedure RefreshObj(Sender: TObject = nil); override;

    //Delete the Obj
    procedure DeleteObj; override;

    function GetXML: string; override;
{$IFDEF USE_IXMLDBMODELType}
    procedure SetXML(theXMLNote: IXMLNOTEType);
{$ENDIF}
    procedure SetXML2(theXMLParser: TXmlParser);

    function GetNoteText: string;
    procedure SetNoteText(txt: string);
  private
    { Private declarations }
    NoteText: TStringList;
  public
    { Public declarations }
  end;


  // -----------------------------------------------
  // Declaration of the EER Image Object

  TEERImage = class(TEERObj)
    constructor Create(AOwner: TComponent; TheName: string); reintroduce; overload;
    destructor Destroy; override;

    procedure ShowEditor(Sender: TObject); override;

    procedure DoPaint(Sender: TObject); override;

    //Paint the Object
    procedure PaintObj2Canvas(theCanvas: TCanvas; xo, yo: integer); override;

    procedure RefreshObj(Sender: TObject = nil); override;

    //Delete the Obj
    procedure DeleteObj; override;

    procedure LoadImageFromFile;

    function GetXML: string; override;
{$IFDEF USE_IXMLDBMODELType}
    procedure SetXML(theXMLImage: IXMLIMAGEType);
{$ENDIF}
    procedure SetXML2(theXMLParser: TXmlParser);

    function GetStrechImg: Boolean;
    procedure SetStrechImg(StrechImg: Boolean);

    procedure ClearImg;
    function GetImgSize: TSize;
  protected
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  private
    { Private declarations }
    Img, StrechedImg: TBitmap;

    StrechImg: Boolean;
  public
    { Public declarations }
  end;

  // -----------------------------------------------
  // Declaration of the EER Region Object

  TEERRegion = class(TEERObj)
    constructor Create(AOwner: TComponent; TheName: string); reintroduce; overload;
    destructor Destroy; override;

    function ObjIsEqualTo(Source: TObject): Boolean; overload;

    procedure ShowEditor(Sender: TObject); override;

    procedure DoPaint(Sender: TObject); override;

    //Paint the Object
    procedure PaintObj2Canvas(theCanvas: TCanvas; xo, yo: integer); override;

    procedure RefreshObj(Sender: TObject = nil); override;

    //Delete the Obj
    procedure DeleteObj; override;

    function GetXML: string; override;
{$IFDEF USE_IXMLDBMODELType}
    procedure SetXML(theXMLRegion: IXMLREGIONType);
{$ENDIF}
    procedure SetXML2(theXMLParser: TXmlParser);

    procedure SelectAllObjsInRegion;
    procedure GetEERObjsInRegion(ObjType: TEERObjectSet; ObjectList: TList; OnlySelected: Boolean=False);

    procedure SetSelected(select: Boolean); override;
  protected
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  private
    { Private declarations }
  public
    { Public declarations }
    ObjectsInRegion: TList;

    RegionColor: integer;

    TablePrefix,
    TableType: Integer;

    OverwriteTablePrefix,
    OverwriteTableType: Boolean;

  end;

  // -----------------------------------------------
  // Declaration of the EER Stored Proc Object

  TEERStoredProc = class(TEERObj)
    constructor Create(AOwner: TComponent; TheName: string); reintroduce; overload;
    destructor Destroy; override;
  protected
  private
    { Private declarations }
    Code: TStringList;
  public
  end;

  TEERDatatypeGroup = class(TPersistent)
    constructor Create(AOwner: TComponent); overload;
    destructor Destroy; override;
  public
    Owner: TComponent;

    GroupName: string;
    IconNr: integer;
  end;

  TEERDatatype = class(TPersistent)
    constructor Create(AOwner: TComponent); overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  protected

  public
    Owner: TComponent;

    id,                     //id of the type
    group: integer;         //number of group
    TypeName,               //Name of the Datatype (INTEGER)
    description: string;    //the description
    Param,                  //Params [(length,decimals)]
    Options: Array[0..5] of string; //several options [UNSIGNED] [ZEROFILL]
    ParamCount,             //Count of the Params
    OptionCount: integer;   //Count of the Options
    ParamRequired: Boolean; //stores whether the Params are required or not
    OptionDefaults: Array[0..5] of Boolean; //stores default selection
    EditParamsAsString: Boolean;  // for ENUM and SET Types
    SynonymGroup: integer;
    PhysicalMapping: Boolean;
    PhysicalTypeName: string;

    function GetPhysicalTypeName: string;
  end;

  //-----------------------------------------------
  TEERColumn = class(TPersistent)
    constructor Create(AOwner: TComponent); overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function ObjIsEqualTo(Source: TObject): Boolean;
  public
    Owner: TComponent;

    ColName,                //Name of the Column
    PrevColName: string;    //Name of the Column before change
    Obj_id,                 //ID of the column
    Pos: integer;           //Position
    idDatatype: integer;    //ID of the Datatype
    DatatypeParams: String; //Params of the Datatype as String
    Width,                  //overall width for Numeric Types
    Prec: integer;          //Precision for Numeric Types
    PrimaryKey,             //Primary Key Flag
    NotNull,                //Not Null Flag
    AutoInc,                //Auto Increment Flag
    IsForeignKey: Boolean; //Foreign Key Flag
    FK_checked: Boolean;    //Flag to detect recursion
    OptionSelected: Array[0..5] of Boolean; //stores selected options
    DefaultValue: string;   //Default Value of the Column
    Comments: string;       //Comments

    tmp: string;
  end;

  //-----------------------------------------------
  TEERIndex = class(TPersistent)
    constructor Create(AOwner: TComponent); overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function ObjIsEqualTo(Source: TObject): Boolean;
  public
    Owner: TComponent;

    IndexName: string;
    Obj_id,                    //ID of the index
    Pos: integer;              //Position
    IndexKind: integer;
    Columns,                   //Stores the idColumns
    ColumnParams: TStringList; //Stores optional column params (length for blob)
    FKRefDef_Obj_id: integer;  //Stores the Obj_id of the parent Relation or -1
  end;

  //-----------------------------------------------
  TEERActionLog = class(TPersistent)
    constructor Create(AOwner: TComponent); overload;
    destructor Destroy; override;
  public
    Owner: TComponent;

    ActionType: integer;
    ActionDate: TDateTime;
    SubActions: TObjectList;
    Closed: Boolean;
  end;

  //-----------------------------------------------
  TEERActionSubLog = class(TPersistent)
    constructor Create(AOwner: TComponent); overload;
    destructor Destroy; override;
  public
    Owner: TComponent;

    SubActionType: integer;
    Obj_id: integer;
    Params: TStringList;
  end;

  //-----------------------------------------------
  TStoredSQLCmd = class(TPersistent)
    constructor Create(SQLCmdType: integer; StoredPosition: string; SQLText: string); overload;
  public
    SQLCmdType: integer;
    StoredPosition: string;
    SQLText: string;
  end;

  //-----------------------------------------------
  TPosMarker = class(TPersistent)
  public
    ZoomFac: double;
    X: integer;
    Y: integer;
  end;

  //-----------------------------------------------

  const
    re_Top=1;
    re_Right=2;
    re_Bottom=3;
    re_Left=4;

    rk_11=0;
    rk_1n=1;
    rk_1nNonId=2;
    rk_nm=3;
    rk_11Sub=4;
    rk_11NonId=5;

    ik_PRIMARY=0;
    ik_INDEX=1;
    ik_UNIQUE_INDEX=2;
    ik_FULLTEXT_INDEX=3;

    at_MoveObj=10;
    sa_MoveFrom=11;
    sa_MoveTo=12;

    at_NewObj=20;
    at_DeleteObj=30;

    at_RenameObj=40;

    at_ScaleObj=50;
    sa_ScaleFrom=51;
    sa_ScaleTo=52;

    at_EditObj=60;

    //SQLCmdType
    ct_SQLCmd=1;
    ct_SQLScript=2;
    ct_SQLTableSelect=3;
    ct_SQLHistory=4;
    ct_SQLDragDropStores=5;


  //-----------------------------------------------
  // General Functions

  function ReverseRelDirection(theRelDir: integer): integer;
  function SortRelations(Item1, Item2: Pointer): Integer;

implementation

uses MainDM, EERDM;

// -----------------------------------------------
// Implementation of the MAIN-Class


constructor TEERModel.Create(AOwner: TComponent);
var i: integer;
  thePosMarker: TPosMarker;
  {thePluginData: TEERPluginData; //Dummy for Plugindata}
begin
  inherited Create(AOwner);
  Parent:=TForm(AOwner);

  Color:=clWhite;
  BevelOuter:=bvNone;

  ModelName:='Noname1';
  ModelFilename:='Noname1.xml';
  ModelComments:='';

  //Load a Background Image
  //Bitmap.LoadFromFile('Gfx'+PathDelim+'background.png');

  EERModel_Width:=4096;
  EERModel_Height:=2842;

  Top:=0;
  Left:=0;
  Width:=EERModel_Width;
  Height:=EERModel_Height;

  LogActions:=False;

  //DatabaseType:=DMDB.DefaultDatabaseType;
  DatabaseType:='MySQL';
  TablePrefix:=TStringList.Create;
  TablePrefix.Add(DMMain.GetTranslatedMessage('Default (no prefix)', 208));
  DefaultTablePrefix:=0;
  DBQuoteCharacter:='`';

  DefSyncDBConn:='';
  DefSaveDBConn:='';
  DefQueryDBConn:='';

  //Create Datatypes
  DatatypeGroups:=TObjectList.Create;
  Datatypes:=TObjectList.Create;
  CommonDataType:=TStringList.Create;
  LoadDataTypesFromIniFile;


  //Reserved Words
  ReservedWords:=TStringList.Create;
  LoadReservedWordsFromIniFile;

  DefaultDataType:=5;

  //Set Font
  {$IFDEF MSWINDOWS}
  DefModelFont:='Tahoma';
  {$ELSE}
  DefModelFont:='Nimbus Sans L';
  {$ENDIF}
  
  ParentFont:=False;


  ZoomFac:=100;
  NewTableCounter:=0;
  NewRelCounter:=0;
  NewNoteCounter:=0;
  NewRegionCounter:=0;
  NewImageCounter:=0;
  LastRegionColor:=0;

  Rel_SrcTable:=nil;
  Rel_DestTable:=nil;

  LoadModelBitmaps;


  if(DMEER.Notation=noErwin)then
  begin
    RelIconSize:=9;
  end
  else
  begin
    RelIconSize:=19;
  end;

  RelIconDSize:=Trunc(RelIconSize / 2);

  //Normal DPI settings for font
  DPI:=72;

  // Create Selection PaintBox
  SelectionRect:=TPaintBox.Create(self);
  SelectionRect.Parent:=self;
  SelectionRect.Visible:=False;
  SelectionRect.OnPaint:=DoSelectionRectPaint;
  SelectionRect.Font.Name:=DefModelFont;
  SelectionRect.Canvas.Font.Name:=DefModelFont;

  // Create GridPaintBox PaintBox
  GridPaintBox:=TPaintBox.Create(self);
  GridPaintBox.Parent:=self;
  GridPaintBox.Name:='GridPaintBox';
  GridPaintBox.Visible:=DMEER.DisplayPaperGrid;
  GridPaintBox.OnPaint:=DoGridPaintBoxPaint;
  GridPaintBox.Width:=EERModel_Width;
  GridPaintBox.Height:=EERModel_Height;
  GridPaintBox.OnMouseDown:=DoMouseDown;
  GridPaintBox.OnMouseMove:=DoMouseMove;
  GridPaintBox.OnMouseUp:=DoMouseUp;

  //Capture MouseAction
  MouseIsDown:=False;
  OnMouseDown:=DoMouseDown;
  OnMouseMove:=DoMouseMove;
  OnMouseUp:=DoMouseUp;

  ModelPrinter:='';
  HPageCount:=4;
  PageAspectRatio:=1.4408925123364084960308946577988;
  PageOrientation:=poLandscape;
  PageFormat:='A4 (210x297 mm, 8.26x11.7 inches)';

  PageSize.cx:=Round(EERModel_Width/HPageCount);
  if(PageOrientation=poPortrait)then
    PageSize.cy:=Round(PageSize.cx*PageAspectRatio)
  else
    PageSize.cy:=Round(PageSize.cx/PageAspectRatio);

  VPageCount:=EERModel_Height/PageSize.cy;

  for i:=0 to 200 do
    SelectedPages[i]:=False;

  IsChanged:=False;
  Need2RefreshNavImg:=False;

  PaintingToSpecialCanvas:=False;

  //Create List for Actionlog

  ActionLog:=TObjectList.Create;
  CurrentAction:=-1;

  //Create Popupmenus
  CreatePopupMenus(AOwner);

  LastTableEditorPage:=0;
  LastRelEditorPage:=0;

  UseVersionHistroy:=True;
  AutoIncVersion:=True;
  IDModel:=0;
  IDVersion:=0;
  VersionStr:='1.0.0.0';
  DefSaveDBConn:='';

  MouseOverObj:=nil;
  MouseOverSubObj:=nil;

  RegionColors:=TStringList.Create;
  //Initialise with default colors
  RegionColors.Text:=DMEER.DefaultRegionColors;

  PluginData:=TObjectList.Create;

  StoredSQLCmds:=TObjectList.Create;

  PosMarkers:=TObjectList.Create;
  for i:=0 to 10 do
  begin
    //new(thePosMarker);
    thePosMarker:=TPosMarker.Create;
    thePosMarker.ZoomFac:=-1;
    thePosMarker.X:=0;
    thePosMarker.Y:=0;
    PosMarkers.Add(thePosMarker);
  end;

  DisableModelRefresh:=False;


  UsePositionGrid:=DMEER.UsePositionGrid;
  PositionGrid.X:=DMEER.PositionGrid.X;
  PositionGrid.Y:=DMEER.PositionGrid.Y;

  TableNameInRefs:=DMEER.TableNameInRefs;
  DefaultTableType:=DMEER.DefaultTableType;
  ActivateRefDefForNewRelations:=DMEER.ActivateRefDefForNewRelations;

  FKPrefix:=DMEER.FKPrefix;
  FKPostfix:=DMEER.FKPostfix;

  CreateFKRefDefIndex:=DMEER.CreateFKRefDefIndex;

  ModelIsBeingCleared:=False;

  ReadOnly:=False;

  LinkedModels:=TObjectList.Create;
  CreateSQLforLinkedObjects:=False;
end;

destructor TEERModel.Destroy;
var i: integer;
begin
  ModelIsBeingCleared:=True;

  TablePrefix.Free;

  //Free Datatypes
  DatatypeGroups.Free;
  CommonDataType.Free;

  Datatypes.Free;

  //Free Reserved Words
  ReservedWords.Free;

  //Free Bitmaps
  TblHeaderBmp.Free;
  TblHeaderRightBmp.Free;
  TblHeaderLinkedBmp.Free;
  TblHeaderRightLinkedBmp.Free;

  FieldBmp.Free;
  FieldKeyBmp.Free;
  IndexBmp.Free;

  PopUpMenuImgs.Free;

  Index_FKBmp.Free;
  Field_FKBmp.Free;

  SelectionRect.Free;
  GridPaintBox.Free;

  //Delete all Actions in List
  for i:=0 to ActionLog.Count-1 do
    DeleteAction(ActionLog[0]);

  ActionLog.Free;

  RegionColors.Free;

  PluginData.Free;
  StoredSQLCmds.Free;
  PosMarkers.Free;

  LinkedModels.Free;

  inherited Destroy;
end;

// ---------------------------------------------------
// Mouse action

procedure TEERModel.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(Button=mbLeft)then
  begin
    //Make shure that no TextImput has the focus
    if(Application.MainForm.ActiveControl<>nil)then
      Application.MainForm.ActiveControl:=nil;

    if(DMEER.CurrentWorkTool=wtPointer)or
      (DMEER.CurrentWorkTool=wtRegion)or
      (DMEER.CurrentWorkTool=wtImage)then
    begin
      DeSelectAllObjs(nil);

      mouse_absx:=Mouse.CursorPos.X;
      mouse_absy:=Mouse.CursorPos.Y;

      mouse_posx:=X;
      mouse_posy:=Y;

      SetSelectionRectPos(X, Y,
        1, 1);

      MouseIsDown:=True;
    end;

    if(DMEER.CurrentWorkTool=wtTable)then
    begin
      NewTable(ReEvalZoomFac(X), ReEvalZoomFac(Y), True);
      DMEER.SetWorkTool(wtPointer);
    end;

    if(DMEER.CurrentWorkTool=wtNote)then
    begin
      NewNote(ReEvalZoomFac(X), ReEvalZoomFac(Y), True);
      DMEER.SetWorkTool(wtPointer);
    end;

    if(DMEER.CurrentWorkTool=wtHand)then
    begin
      mouse_absx:=Mouse.CursorPos.X;
      mouse_absy:=Mouse.CursorPos.Y;
      if(Name='GridPaintBox')then
      begin
        mouse_posx:=TForm(TPanel(parent).parent).HorzScrollBar.Position;
        mouse_posy:=TForm(TPanel(parent).parent).VertScrollBar.Position;
      end
      else
      begin
        mouse_posx:=TForm(parent).HorzScrollBar.Position;
        mouse_posy:=TForm(parent).VertScrollBar.Position;
      end;

      MouseIsDown:=True;
    end;

    if(DMEER.CurrentWorkTool=wtZoomIn)then
      ZoomIn(X, Y);

    if(DMEER.CurrentWorkTool=wtZoomOut)then
      ZoomOut(X, Y);
  end;
end;

procedure TEERModel.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var SelRectWidth, SelRectHeight: integer;
begin
  ClearMouseOverObj;

  if(Shift=[ssLeft])and(MouseIsDown)then
  begin
    if(DMEER.CurrentWorkTool=wtPointer)or
      (DMEER.CurrentWorkTool=wtRegion)or
      (DMEER.CurrentWorkTool=wtImage)then
    begin
      //Resize SelectionRect
      SelRectWidth:=Mouse.CursorPos.X-mouse_absx;
      SelRectHeight:=Mouse.CursorPos.Y-mouse_absy;

      //update left-top corner if needed
      if(SelRectWidth<0)then
        SelectionRect.Left:=mouse_posx+SelRectWidth;
      if(SelRectHeight<0)then
        SelectionRect.Top:=mouse_posy+SelRectHeight;

      SetSelectionRectPos(SelectionRect.Left, SelectionRect.Top,
        abs(SelRectWidth), abs(SelRectHeight));

      //Shop SelectionRect
      SelectionRect.Visible:=True;
    end;

    if(DMEER.CurrentWorkTool=wtHand)then
    begin

      if(Name='GridPaintBox')then
      begin
        TForm(TPanel(parent).parent).HorzScrollBar.Position:=
          mouse_posx+(Mouse.CursorPos.X-mouse_absx)*-1;
        TForm(TPanel(parent).parent).VertScrollBar.Position:=
          mouse_posy+(Mouse.CursorPos.Y-mouse_absy)*-1;
      end
      else
      begin
        if(TForm(parent).HorzScrollBar<>nil)then
        begin
          TForm(parent).HorzScrollBar.Position:=
            mouse_posx+(Mouse.CursorPos.X-mouse_absx)*-1;
{$IFDEF LINUX}
          if(TForm(parent).HorzScrollBar.Position<0)then
            TForm(parent).HorzScrollBar.Position:=0;
          if(TForm(parent).HorzScrollBar.Position>TForm(parent).HorzScrollBar.Range-TForm(parent).ClientWidth)then
            TForm(parent).HorzScrollBar.Position:=TForm(parent).HorzScrollBar.Range-TForm(parent).ClientWidth;
{$ENDIF}
        end;

        if(TForm(parent).HorzScrollBar<>nil)then
        begin
          TForm(parent).VertScrollBar.Position:=
            mouse_posy+(Mouse.CursorPos.Y-mouse_absy)*-1;
{$IFDEF LINUX}
          if(TForm(parent).VertScrollBar.Position<0)then
            TForm(parent).VertScrollBar.Position:=0;
          if(TForm(parent).VertScrollBar.Position>TForm(parent).VertScrollBar.Range-TForm(parent).ClientHeight)then
            TForm(parent).VertScrollBar.Position:=TForm(parent).VertScrollBar.Range-TForm(parent).ClientHeight;
{$ENDIF}
        end;
      end;
    end;
  end;
end;

procedure TEERModel.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var theImage: TEERImage;
  thePoint: TPoint;
begin
  if(Button=mbLeft)then
  begin
    MouseIsDown:=False;

    if(DMEER.CurrentWorkTool=wtPointer)and
      (SelectionRect.Visible)then
    begin
      SelectionRect.Visible:=False;

      SelectObjsInSelectionRect;
    end;

    if(DMEER.CurrentWorkTool=wtRegion)and
      (SelectionRect.Visible)then
    begin
      SelectionRect.Visible:=False;

      NewRegion(ReEvalZoomFac(SelectionRect.Left),
        ReEvalZoomFac(SelectionRect.Top),
        ReEvalZoomFac(SelectionRect.Width),
        ReEvalZoomFac(SelectionRect.Height), True);

      DMEER.SetWorkTool(wtPointer);
    end;

    if(DMEER.CurrentWorkTool=wtImage)and
      (SelectionRect.Visible)then
    begin
      SelectionRect.Visible:=False;

      theImage:=NewImage(ReEvalZoomFac(SelectionRect.Left),
        ReEvalZoomFac(SelectionRect.Top),
        ReEvalZoomFac(SelectionRect.Width),
        ReEvalZoomFac(SelectionRect.Height), True);

      theImage.LoadImageFromFile;

      DMEER.SetWorkTool(wtPointer);
    end;

    if(Not(DisableModelRefresh))then
      DMEER.RefreshInfoPalette;

    if(DMEER.CurrentWorkTool=wtPlacementFromFile)or
      (DMEER.CurrentWorkTool=wtPlacementFromDB)or
      (DMEER.CurrentWorkTool=wtPlacementFromLibrary)then
    begin
      thePoint.X:=ReEvalZoomFac(X);
      thePoint.Y:=ReEvalZoomFac(Y);

      if(DMEER.CurrentWorkTool=wtPlacementFromFile)then
      begin
        DMEER.SetWorkTool(wtPointer);
        sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_PlaceModelFromFile, @thePoint));
      end
      else if(DMEER.CurrentWorkTool=wtPlacementFromDB)then
      begin
        DMEER.SetWorkTool(wtPointer);
        sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_PlaceModelFromDB, @thePoint));
      end
      else
      begin
        DMEER.SetWorkTool(wtPointer);
        sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_PlaceModelFromLibrary, @thePoint));
      end;
    end;
  end;
end;

procedure TEERModel.MouseEnter(AControl: TControl);
begin
  // set Screen.Cursor
  //DMEER.SetWorkToolCurser(DMEER.CurrentWorkTool);
end;

procedure TEERModel.MouseLeave(AControl: TControl);
begin
  // restore the cursor
  //Screen.Cursor:=crDefault;
end;

// ---------------------------------------------------
// Zoom Fac - functions

function TEERModel.EvalZoomFac(thevalue: integer): integer;
begin
  EvalZoomFac:=round(thevalue*(ZoomFac/100));
end;

function TEERModel.ReEvalZoomFac(thevalue: integer): integer;
begin
  ReEvalZoomFac:=round(thevalue/(ZoomFac/100));
end;

procedure TEERModel.SetZoomFac(NewZoomFac: double; X: integer = -1; Y: integer = -1);
var hsc, vsc, hscW, hscH,
  x2scroll, y2scroll: integer;
begin
  DMEER.DisablePaint:=True;
  try
    ZoomFac:=NewZoomFac;

    hsc:=TForm(parent).HorzScrollBar.Position;
    vsc:=TForm(parent).VertScrollBar.Position;

    if(X>-1)and(Y>-1)then
    begin
      x2scroll:=((X-TForm(parent).HorzScrollBar.Position)-(TForm(parent).Width div 2));
      y2scroll:=((Y-TForm(parent).VertScrollBar.Position)-(TForm(parent).Height div 2))
    end
    else
    begin
      x2scroll:=0;
      y2scroll:=0;
    end;

    hscW:=TForm(parent).HorzScrollBar.Range;
    hscH:=TForm(parent).VertScrollBar.Range;

    //if the model becomes bigger than the form, reset position
    if(Width<TForm(parent).Width)and
      (EvalZoomFac(EERModel_Width)>TForm(parent).Width)then
      Left:=0;
    if(Height<TForm(parent).Height)and
      (EvalZoomFac(EERModel_Height)>TForm(parent).Height)then
      Top:=0;

    //If a scrollbar disapears, reset precached Values
    if(Width>TForm(parent).Width)and
      (EvalZoomFac(EERModel_Width)<TForm(parent).Width)then
    begin
      Width:=EvalZoomFac(EERModel_Width);

      vsc:=TForm(parent).VertScrollBar.Position;
      hscW:=TForm(parent).HorzScrollBar.Range;
    end
    else
      Width:=EvalZoomFac(EERModel_Width);

    //If a scrollbar disapears, reset precached Values
    if(Height>TForm(parent).Height)and
      (EvalZoomFac(EERModel_Height)<TForm(parent).Height)then
    begin
      Height:=EvalZoomFac(EERModel_Height);

      hsc:=TForm(parent).HorzScrollBar.Position;
      hscW:=TForm(parent).HorzScrollBar.Range;
    end
    else
      Height:=EvalZoomFac(EERModel_Height);




    if(Width>=TForm(parent).Width)then
    begin
      if(hscW>0)then
        TForm(parent).HorzScrollBar.Position:=
          Round(hsc*(TForm(parent).HorzScrollBar.Range/hscW))-
          Round((TForm(parent).Width/2 - ((TForm(parent).Width /2)*(TForm(parent).HorzScrollBar.Range/hscW))))+
          x2scroll;
    end
    else
    begin
      Left:=(TForm(parent).Width-Width) div 2;
      TForm(parent).Invalidate;
    end;

    if(Height>=TForm(parent).Height)then
    begin
      if(hscH>0)then
        TForm(parent).VertScrollBar.Position:=
          Round(vsc*(TForm(parent).VertScrollBar.Range/hscH))-
          Round((TForm(parent).Height/2 - ((TForm(parent).Height /2)*(TForm(parent).VertScrollBar.Range/hscH))))+
          y2scroll;
    end
    else
    begin
      Top:=(TForm(parent).Height-Height) div 2;
      TForm(parent).Invalidate;
    end;


    //Relation Icon Size
    if(DMEER.Notation=noErwin)then
      RelIconSize:=EvalZoomFac(9)
    else
      RelIconSize:=EvalZoomFac(18);

    RelIconDSize:=Trunc(RelIconSize/2);
  finally
    DMEER.DisablePaint:=False;
  end;

  //Resize GridPaintBox
  GridPaintBox.Width:=Width;
  GridPaintBox.Height:=Height;

  if(Not(DisableModelRefresh))then
  begin
    Refresh;

    DMEER.UpdateStatusBar;

    DMEER.RefreshNavPalette;
  end;
end;

procedure TEERModel.Refresh;
var i: integer;
begin
  if(Not(DisableModelRefresh))then
  begin
    //Rescale all EER-Objects
    for i:=ComponentCount-1 downto 0 do
      if(Components[I].Classparent=TEERObj)then
        TEERObj(Components[I]).RefreshObj;

    Invalidate;
  end;
end;

procedure TEERModel.RefreshFont;
var i: integer;
begin
  //Set Font for SelectionRect
  SelectionRect.Canvas.Font.Name:=DefModelFont;
  SelectionRect.ParentFont:=False;

  //Set Font for all EER-Objects
  for i:=ComponentCount-1 downto 0 do
    if(Components[I].Classparent=TEERObj)then
    begin
      if(Not(Components[I].ClassnameIs('TEERRel')))then
      begin
        TEERObj(Components[I]).Font.Name:=DefModelFont;
        TEERObj(Components[I]).ParentFont:=False;
      end
      else
      begin
        TEERRel(Components[I]).ParentFont:=False;
        TEERRel(Components[I]).Font.Name:=DefModelFont;
        TEERRel(Components[I]).RelCaption.Font.Name:=DefModelFont;
        TEERRel(Components[I]).RelCaption.Canvas.Font.Name:=DefModelFont;
        TEERRel(Components[I]).RelEndInterval.Font.Name:=DefModelFont;
        TEERRel(Components[I]).RelEndInterval.Canvas.Font.Name:=DefModelFont;
        TEERRel(Components[I]).RelStartInterval.Font.Name:=DefModelFont;
        TEERRel(Components[I]).RelStartInterval.Canvas.Font.Name:=DefModelFont;
      end;
    end;
end;

procedure TEERModel.RefreshTblImgs;
var thePath: string;
begin
  thePath:=ExtractFilePath(Application.ExeName)+
    'Gfx'+PathDelim+'Table'+PathDelim;

  TblHeaderBmp.LoadFromFile(thePath+'Header_'+DMEER.TblHeaderBGImgs+'.bmp');
  TblHeaderRightBmp.LoadFromFile(thePath+'HeaderRight_'+DMEER.TblHeaderBGImgs+'.bmp');

  //Linked Header
  if(Not(FileExists(thePath+'Header_'+DMEER.TblHeaderBGImgs+'_Linked.bmp')))then
    TblHeaderLinkedBmp.LoadFromFile(thePath+'Header_'+DMEER.TblHeaderBGImgs+'.bmp')
  else
    TblHeaderLinkedBmp.LoadFromFile(thePath+'Header_'+DMEER.TblHeaderBGImgs+'_Linked.bmp');

  if(Not(FileExists(thePath+'HeaderRight_'+DMEER.TblHeaderBGImgs+'_Linked.bmp')))then
    TblHeaderRightLinkedBmp.LoadFromFile(thePath+'HeaderRight_'+DMEER.TblHeaderBGImgs+'.bmp')
  else
    TblHeaderRightLinkedBmp.LoadFromFile(thePath+'HeaderRight_'+DMEER.TblHeaderBGImgs+'_Linked.bmp');


  Refresh;
end;

function TEERModel.GetZoomFac: double;
begin
  GetZoomFac:=ZoomFac;
end;

procedure TEERModel.ZoomIn(x, y: integer);
begin
  SetZoomFac(ZoomFac*1.3333333333, x, y);
end;

procedure TEERModel.ZoomOut(x, y: integer);
begin
  SetZoomFac(ZoomFac/1.3333333333, x, y);
end;

procedure TEERModel.SetPositionMarker(nr: integer);
begin
  if(nr>=0)and(nr<=9)then
  begin
    TPosMarker(PosMarkers.Items[nr]).ZoomFac:=GetZoomFac;
    TPosMarker(PosMarkers.Items[nr]).X:=TForm(parent).HorzScrollBar.Position;
    TPosMarker(PosMarkers.Items[nr]).Y:=TForm(parent).VertScrollBar.Position;
  end;
end;

procedure TEERModel.GotoPositionMarker(nr: integer);
begin
  if(nr>=0)and(nr<=9)then
    if(TPosMarker(PosMarkers.Items[nr]).ZoomFac<>-1)then
    begin
      SetZoomFac(TPosMarker(PosMarkers.Items[nr]).ZoomFac,
        TPosMarker(PosMarkers.Items[nr]).X,
        TPosMarker(PosMarkers.Items[nr]).Y);

      TForm(parent).HorzScrollBar.Position:=TPosMarker(PosMarkers.Items[nr]).X;
      TForm(parent).VertScrollBar.Position:=TPosMarker(PosMarkers.Items[nr]).Y;
    end;
end;

function TEERModel.GetFontHeight: integer;
var theFontHeight: integer;
begin
  theFontHeight:=Round(EvalZoomFac(12)*(72/DPI));

  if(theFontHeight<1)then
    theFontHeight:=1;

  GetFontHeight:=theFontHeight;
end;

function TEERModel.GetTextExtent(s: string): TSize;
var theSize: TSize;
  theFontHeight: integer;
  reCalc: double;
begin
  //Get Font Size
  theFontHeight:=Round(EvalZoomFac(12));

  //if it would be smaller then 6px, calculate from fixed 6px Font
  reCalc:=1;
  if(theFontHeight<8)then
  begin
    reCalc:=12*(ZoomFac/100)/8;
    theFontHeight:=8;
  end;

  SelectionRect.Canvas.Font.Height:=theFontHeight;
  theSize:=SelectionRect.Canvas.TextExtent(s);

  if(reCalc<>1)then
  begin
    theSize.cx:=Round(theSize.cx*reCalc);
    theSize.cy:=Round(theSize.cy*reCalc);
  end;

  GetTextExtent:=theSize;
end;

// ---------------------------------------------------
// Functions

procedure TEERModel.AddImgToPopupImgList(fname: string);
var theImg: TBitmap;
begin
  theImg:=TBitmap.Create;
  theImg.LoadFromFile(fname);
  PopUpMenuImgs.Add(theImg, nil);
end;

procedure TEERModel.LoadModelBitmaps;
var
  thePath: string;
begin
  //Load Table Header Bitmaps
  TblHeaderBmp:=TBitmap.Create;
  TblHeaderRightBmp:=TBitmap.Create;
  TblHeaderLinkedBmp:=TBitmap.Create;
  TblHeaderRightLinkedBmp:=TBitmap.Create;

  FieldBmp:=TBitmap.Create;
  FieldKeyBmp:=TBitmap.Create;
  IndexBmp:=TBitmap.Create;

  Field_FKBmp:=TBitmap.Create;
  Index_FKBmp:=TBitmap.Create;

  PopUpMenuImgs:=TImageList.Create(self);
  PopUpMenuImgs.Masked:=True;
  PopUpMenuImgs.Width:=16;
  PopUpMenuImgs.Height:=15;

  try
    thePath:=ExtractFilePath(Application.ExeName)+
      'Gfx'+PathDelim+'Table'+PathDelim;

    if(Not(FileExists(thePath+'Header_'+DMEER.TblHeaderBGImgs+'.bmp')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Header_'+DMEER.TblHeaderBGImgs+'.bmp'));
    TblHeaderBmp.LoadFromFile(thePath+'Header_'+DMEER.TblHeaderBGImgs+'.bmp');

    if(Not(FileExists(thePath+'HeaderRight_'+DMEER.TblHeaderBGImgs+'.bmp')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'HeaderRight_'+DMEER.TblHeaderBGImgs+'.bmp'));
    TblHeaderRightBmp.LoadFromFile(thePath+'HeaderRight_'+DMEER.TblHeaderBGImgs+'.bmp');

    //Linked Header
    if(Not(FileExists(thePath+'Header_'+DMEER.TblHeaderBGImgs+'_Linked.bmp')))then
      TblHeaderLinkedBmp.LoadFromFile(thePath+'Header_'+DMEER.TblHeaderBGImgs+'.bmp')
    else
      TblHeaderLinkedBmp.LoadFromFile(thePath+'Header_'+DMEER.TblHeaderBGImgs+'_Linked.bmp');

    if(Not(FileExists(thePath+'HeaderRight_'+DMEER.TblHeaderBGImgs+'_Linked.bmp')))then
      TblHeaderRightLinkedBmp.LoadFromFile(thePath+'HeaderRight_'+DMEER.TblHeaderBGImgs+'.bmp')
    else
      TblHeaderRightLinkedBmp.LoadFromFile(thePath+'HeaderRight_'+DMEER.TblHeaderBGImgs+'_Linked.bmp');



    if(Not(FileExists(thePath+'Field.bmp')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Field.bmp'));
    FieldBmp.LoadFromFile(thePath+'Field.bmp');

    if(Not(FileExists(thePath+'FieldKey.bmp')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'FieldKey.bmp'));
    FieldKeyBmp.LoadFromFile(thePath+'FieldKey.bmp');

    if(Not(FileExists(thePath+'Index.bmp')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Index.bmp'));
    IndexBmp.LoadFromFile(thePath+'Index.bmp');

    if(Not(FileExists(thePath+'Field_FK.bmp')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Field_FK.bmp'));
    Field_FKBmp.LoadFromFile(thePath+'Field_FK.bmp');

    if(Not(FileExists(thePath+'Index_FK.bmp')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Index_FK.bmp'));
    Index_FKBmp.LoadFromFile(thePath+'Index_FK.bmp');

    //Load PopupMenu Icons
    thePath:=ExtractFilePath(Application.ExeName)+
      'Gfx'+PathDelim+'Popup'+PathDelim;

    if(Not(FileExists(thePath+'Select.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Select.png'));
    AddImgToPopupImgList(thePath+'Select.png');

    if(Not(FileExists(thePath+'Edit.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Edit.png'));
    AddImgToPopupImgList(thePath+'Edit.png');

    if(Not(FileExists(thePath+'Refresh.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Refresh.png'));
    AddImgToPopupImgList(thePath+'Refresh.png');

    if(Not(FileExists(thePath+'Delete.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Delete.png'));
    AddImgToPopupImgList(thePath+'Delete.png');

    if(Not(FileExists(thePath+'Copy.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Copy.png'));
    AddImgToPopupImgList(thePath+'Copy.png');

    if(Not(FileExists(thePath+'Sql.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Sql.png'));
    AddImgToPopupImgList(thePath+'Sql.png');

    //Load Align Icons
    thePath:=ExtractFilePath(Application.ExeName)+
      'Gfx'+PathDelim+'Popup'+PathDelim+'Align'+PathDelim;

    if(Not(FileExists(thePath+'Top.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Top.png'));
    AddImgToPopupImgList(thePath+'Top.png');

    if(Not(FileExists(thePath+'Right.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Right.png'));
    AddImgToPopupImgList(thePath+'Right.png');

    if(Not(FileExists(thePath+'Bottom.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Bottom.png'));
    AddImgToPopupImgList(thePath+'Bottom.png');

    if(Not(FileExists(thePath+'Left.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'Left.png'));
    AddImgToPopupImgList(thePath+'Left.png');

    if(Not(FileExists(thePath+'CenterH.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'CenterH.png'));
    AddImgToPopupImgList(thePath+'CenterH.png');

    if(Not(FileExists(thePath+'CenterV.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'CenterV.png'));
    AddImgToPopupImgList(thePath+'CenterV.png');

    if(Not(FileExists(thePath+'DistributeH.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'DistributeH.png'));
    AddImgToPopupImgList(thePath+'DistributeH.png');

    if(Not(FileExists(thePath+'DistributeV.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'DistributeV.png'));
    AddImgToPopupImgList(thePath+'DistributeV.png');

    //Load additional icons
    thePath:=ExtractFilePath(Application.ExeName)+
      'Gfx'+PathDelim+'Popup'+PathDelim;

    if(Not(FileExists(thePath+'EditTableData.png')))then
      raise EInOutError.Create(DMMain.GetTranslatedMessage('File %s not found.', 209,
        thePath+'EditTableData.png'));
    AddImgToPopupImgList(thePath+'EditTableData.png');
  except
    on x: Exception do
      MessageDlg(DMMain.GetTranslatedMessage('ERROR: One of the Table Images could not be loaded. '#13#10+
        'Please check the Gfx/Table directory.'#13#10'%s', 30, x.Message),
        mtError, [mbOK], 0);
  end;
end;

procedure TEERModel.LoadDataTypesFromIniFile;
var theIni: TMemIniFile;
  i, j: integer;
  theStringList, theDatatypeGroupsStringList: TStringList;
  theDataTypeGroup: TEERDatatypeGroup;
  theDataType: TEERDatatype;
  s: string;
begin
  //Clear Datatypes
  Datatypes.Clear;
  CommonDataType.Clear;


  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBDesignerFork_DatabaseInfo.ini');
  try
    s:=Copy(theIni.ReadString(DatabaseType+'_QuoteCharacter', 'QuoteCharacter', '`'), 1, 1);
    DBQuoteCharacter:=s[1];

    theDatatypeGroupsStringList:=TStringList.Create;
    theStringList:=TStringList.Create;
    try
      //Read all Datatype-Groups
      theIni.ReadSectionValues(DatabaseType+'_Datatypes', theDatatypeGroupsStringList);

      for i:=0 to theDatatypeGroupsStringList.Count-1 do
      begin
        theDatatypeGroupsStringList[i]:=Trim(Copy(theDatatypeGroupsStringList[i],
          Pos('=', theDatatypeGroupsStringList[i])+1, Length(theDatatypeGroupsStringList[i])));

        //Read all Datatypes in this group
        theIni.ReadSectionValues(theDatatypeGroupsStringList[i], theStringList);

        theDataTypeGroup:=TEERDatatypeGroup.Create(self);
        theDataTypeGroup.GroupName:=theStringList.Values['Name'];
        try
          theDataTypeGroup.IconNr:=StrToInt(theStringList.Values['Icon']);
        except
          theDataTypeGroup.IconNr:=1;
        end;

        DatatypeGroups.Add(theDataTypeGroup);



        //Create all Datatypes
        for j:=0 to theStringList.Count-1 do
        begin
          //new(theDataType);
          theDataType:=TEERDatatype.Create(self);
          try
            if(theStringList.Names[j]='Name')then
              continue;
            if(theStringList.Names[j]='Icon')then
              continue;

            //Get the ID (FieldXXX)
            theDataType.id:=StrToInt(Copy(theStringList.Names[j], 6, 3));
            theDataType.group:=i;

            //theDataType.CommonDataType:=-1;

            theStringList[j]:=Trim(Copy(theStringList[j],
              Pos('=', theStringList[j])+1, Length(theStringList[j])));

            DecodeDataTypes(theStringList[j], theDataType);

            Datatypes.Add(theDataType);

            //Fix wrong ENUM/Set Params up to 4.0.2.94
            if(CompareText(theDatatype.TypeName, 'ENUM')=0)or(CompareText(theDatatype.TypeName, 'SET')=0)then
              if(theDatatype.ParamCount=3)then
                if(theDatatype.Param[0]='''value1''')then
                begin
                  theDatatype.ParamCount:=1;
                  theDatatype.Param[0]:='values';
                  theDatatype.EditParamsAsString:=True;
                end;
          except
            on x: Exception do
            begin
              theDataType.Free;
              ShowMessage(DMMain.GetTranslatedMessage('Error while loading datatypes from ini file.'+#13#10+#13#10+'%s',
                210, x.Message));
            end;
          end;
        end;
      end;

      //Set Common Datatypes
      theIni.ReadSectionValues(DatabaseType+'_CommonDatatypes', CommonDataType);
      for i:=0 to CommonDataType.Count-1 do
        CommonDataType[i]:=Copy(CommonDataType[i], Pos('=', CommonDataType[i])+1, length(CommonDataType[i]));

    finally
      theStringList.Free;
      theDatatypeGroupsStringList.Free;
    end;
  finally
    theIni.Free;
  end;
end;

procedure TEERModel.LoadReservedWordsFromIniFile;
var theIni: TMemIniFile;
begin
  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBDesignerFork_DatabaseInfo.ini');
  try
    //Read all Datatype-Groups
    theIni.ReadSectionValues(DatabaseType+'_ReservedWords', ReservedWords);
  finally
    theIni.Free;
  end;
end;

function TEERModel.CheckReservedWord(s: string): Boolean;
begin
  CheckReservedWord:=False;

  //Check if option is enabled
  if(DMEER.RenameReservedWords)and
    (ReservedWords.Values[s]='reserved')then
    CheckReservedWord:=True;
end;

procedure TEERModel.DecodeDataTypes(s: string; p: Pointer);
var i, p0, p1, p2, p3: integer;
  theOption: string;
begin
  //Get Type name (it has to be followed by ' ', '[' or '('
  p1:=Pos(' ', s); if(p1=0)then p1:=9999;
  p2:=Pos('[', s); if(p2=0)then p2:=9999;
  p3:=Pos('(', s); if(p3=0)then p3:=9999;
  p0:=min(p1, p2);
  p0:=min(p0, p3);

  //Assign Type Name
  TEERDatatype(p).TypeName:=Copy(s, 1, p0-1);

  //Replace _, <, >
  TEERDatatype(p).TypeName:=
    DMMain.ReplaceText(TEERDatatype(p).TypeName, '_', ' ');
  TEERDatatype(p).TypeName:=
    DMMain.ReplaceText(TEERDatatype(p).TypeName, '<', '(');
  TEERDatatype(p).TypeName:=
    DMMain.ReplaceText(TEERDatatype(p).TypeName, '>', ')');


  s:=Copy(s, p0, length(s));

  TEERDatatype(p).ParamCount:=0;
  TEERDatatype(p).EditParamsAsString:=False;
  TEERDatatype(p).ParamRequired:=False;

  //Check Params
  if(LeftStr(s, 1)<>' ')and(Length(s)>0)then
  begin
    //if Params start with a '[', they are optional
    if(LeftStr(s, 1)='[')then
    begin
      TEERDatatype(p).ParamRequired:=False;
      s:=Copy(s, 3, length(s));
    end
    else
    begin
      TEERDatatype(p).ParamRequired:=True;
      s:=Copy(s, 2, length(s));
    end;

    i:=0;
    while(LeftStr(s, 1)<>')')do
    begin
      p1:=Pos(',', s); if(p1=0)then p1:=9999;
      p2:=Pos(')', s); if(p2=0)then p2:=9999;
      p0:=min(p1, p2);

      TEERDatatype(p).Param[i]:=LeftStr(s, p0-1);
      //Check trailing $
      if(Copy(TEERDatatype(p).Param[i], Length(TEERDatatype(p).Param[i]), 1)='$')then
      begin
        TEERDatatype(p).EditParamsAsString:=True;
        TEERDatatype(p).Param[i]:=Copy(TEERDatatype(p).Param[i], 1, Length(TEERDatatype(p).Param[i])-1);
      end;

      s:=Copy(s, p0, Length(s));

      //Remove ,
      if(p0=p1)then
        s:=Copy(s, 2, Length(s));

      inc(i);
    end;
    TEERDatatype(p).ParamCount:=i;

    //Delete closing ')'
    if(TEERDatatype(p).ParamRequired)then
      s:=Copy(s, 2, Length(s))
    //if not ParamRequired, there is a closing ']'
    else
      s:=Copy(s, 3, Length(s));
  end;

  //Delete Space
  s:=Copy(s, 2, Length(s));

  //Check Options
  i:=0;
  TEERDatatype(p).SynonymGroup:=0;
  while(LeftStr(s, 1)='[')do
  begin
    theOption:=Copy(s, 2, Pos(']', s)-2);

    //Get Synonymgroup
    if(Pos('=', theOption)>0)then
    begin
      TEERDatatype(p).synonymgroup:=StrToInt(Copy(theOption, Pos('=', theOption)+1, 5));
    end
    else
    begin
      TEERDatatype(p).Options[i]:=theOption;

      //if the Option is followed by '!', it's a default option
      if(RightStr(TEERDatatype(p).Options[i], 1)='!')then
      begin
        TEERDatatype(p).Options[i]:=
          LeftStr(TEERDatatype(p).Options[i], Length(TEERDatatype(p).Options[i])-1);

        TEERDatatype(p).OptionDefaults[i]:=True;
      end
      else
        TEERDatatype(p).OptionDefaults[i]:=False;

      inc(i);
    end;

    s:=Copy(s, Pos(']', s)+2, Length(s));
  end;

  TEERDatatype(p).OptionCount:=i;

  //Get Description
  if(LeftStr(s, 1)='/')then
    TEERDatatype(p).Description:=Trim(Copy(s, 3, Length(s)));
end;

function TEERModel.GetDataTypeName(id: integer): string;
var i: integer;
begin
  GetDataTypeName:='';
  for i:=0 to Datatypes.Count-1 do
    if(TEERDatatype(Datatypes[i]).id=id)then
    begin
      GetDataTypeName:=TEERDatatype(Datatypes[i]).TypeName;
      break;
    end;
end;

function TEERModel.GetDataTypeGroup(id: integer): integer;
var i: integer;
begin
  GetDataTypeGroup:=-1;

  for i:=0 to Datatypes.Count-1 do
    if(TEERDatatype(Datatypes[i]).id=id)then
    begin
      GetDataTypeGroup:=TEERDatatype(Datatypes[i]).group;
      break;
    end;
end;

function TEERModel.GetDataType(id: integer): Pointer;
var i: integer;
begin
  GetDataType:=nil;

  for i:=0 to Datatypes.Count-1 do
    if(TEERDatatype(Datatypes[i]).id=id)then
    begin
      GetDataType:=Datatypes[i];
      break;
    end;
end;

function TEERModel.GetDataTypeByName(name: string): Pointer;
var i: integer;
begin
  GetDataTypeByName:=nil;

  for i:=0 to Datatypes.Count-1 do
    if(CompareText(TEERDatatype(Datatypes[i]).TypeName, name)=0)then
    begin
      GetDataTypeByName:=Datatypes[i];
      break;
    end;
end;

function TEERModel.GetDataTypeByNameSubst(DatatypeName: string; DatatypeSubstList: TStringList): Pointer;
var s: string;
  theDatatype: TEERDatatype;
begin
  s:=DatatypeName;

  //Make Substitution when nesessary
  if(Assigned(DatatypeSubstList))then
    if(DatatypeSubstList.Values[DatatypeName]<>'')then
      s:=DatatypeSubstList.Values[DatatypeName];

  theDatatype:=GetDataTypeByName(s);
  if(theDatatype<>nil)then
    GetDataTypeByNameSubst:=theDatatype
  else
    GetDataTypeByNameSubst:=GetDataType(DefaultDataType);
end;

procedure TEERModel.DoSelectionRectPaint(Sender: TObject);
begin
  with SelectionRect do
    with SelectionRect.Canvas do
    begin
      Pen.Color:=clWhite;
      MoveTo(0, 0);
      LineTo(0, height-1);
      LineTo(width-1, height-1);
      LineTo(width-1, 0);
      LineTo(0, 0);

      Pen.Color:=clBlack;
      Pen.Style:=psDot;
      MoveTo(0, 0);
      LineTo(0, height-1);
      LineTo(width-1, height-1);
      LineTo(width-1, 0);
      LineTo(0, 0);

      Pen.Style:=psSolid;
    end;
end;


procedure TEERModel.DoGridPaintBoxPaint(Sender: TObject);
var i: integer;
begin
  with GridPaintBox do
    with GridPaintBox.Canvas do
    begin
      Pen.Style:=psDot;
      Pen.Color:=clSilver;
      for i:=0 to width div 21 do
      begin
        MoveTo(EvalZoomFac(Round(EERModel_Width/HPageCount)*(i+1)), 0);
        LineTo(EvalZoomFac(Round(EERModel_Width/HPageCount)*(i+1)), height-1);
      end;

      for i:=0 to height div 16 do
      begin
        MoveTo(0, EvalZoomFac(Round(EERModel_Height/VPageCount)*(i+1)));
        LineTo(width-1, EvalZoomFac(Round(EERModel_Height/VPageCount)*(i+1)));
      end;
      Pen.Style:=psSolid;
    end;
end;

procedure TEERModel.SetSelectionRectPos(l, t, w, h: integer);
begin
  SelectionRect.Left:=l;
  SelectionRect.Top:=t;

  SelectionRect.Width:=w;
  SelectionRect.Height:=h;

  SelectionRect.Visible:=True;
end;

procedure TEERModel.GetSelectionRectPos(var selRect: TRect);
begin
  selRect.Left:=SelectionRect.Left;
  selRect.Top:=SelectionRect.Top;
  selRect.Right:=SelectionRect.Width;
  selRect.Bottom:=SelectionRect.Height;
end;

function TEERModel.GetSelectionRectVisible: Boolean;
begin
  if(SelectionRect.Visible)and
    (SelectionRect.Width>1)and(SelectionRect.Height>1)then
    GetSelectionRectVisible:=True
  else
    GetSelectionRectVisible:=False;
end;

procedure TEERModel.HideSelectionRect;
begin
  SelectionRect.Visible:=False;
end;

procedure TEERModel.SelectObjsInSelectionRect;
var i: integer;
begin
  for i:=ComponentCount-1 downto 0 do
  begin
    if(Components[I].Classparent=TEERObj)then
    begin
      if(ReEvalZoomFac(SelectionRect.Left)<=
        TEERObj(Components[I]).Obj_X)and
        (ReEvalZoomFac(SelectionRect.Top)<=
        TEERObj(Components[I]).Obj_Y)and
        (ReEvalZoomFac(SelectionRect.Left+SelectionRect.Width)>=
        TEERObj(Components[I]).Obj_X+TEERObj(Components[I]).Obj_W)and
        (ReEvalZoomFac(SelectionRect.Top+SelectionRect.Height)>=
        TEERObj(Components[I]).Obj_Y+TEERObj(Components[I]).Obj_H)then
      begin
        TEERObj(Components[I]).SetSelected(True);
        TEERObj(Components[I]).DoPaint(self);
      end;
    end;
  end;
end;

procedure TEERModel.SelectAllObjs;
var i: integer;
begin
  for i:=ComponentCount-1 downto 0 do
  begin
    if(Components[I].Classparent=TEERObj)then
    begin
      if(Not(TEERObj(Components[I]).Selected))then
      begin
        TEERObj(Components[I]).SetSelected(True);
        TEERObj(Components[I]).DoPaint(self);
      end;
    end;
  end;
end;

procedure TEERModel.DeSelectAllObjs(ExcludeObj: TObject);
var i: integer;
begin
  for i:=ComponentCount-1 downto 0 do
  begin
    if(Components[I].Classparent=TEERObj)and
      (Components[I]<>ExcludeObj)then
    begin
      if(TEERObj(Components[I]).Selected)then
      begin
        TEERObj(Components[I]).SetSelected(False);
        TEERObj(Components[I]).DoPaint(self);
      end;
    end;
  end;
end;

function TEERModel.NewTable(x, y: integer; LogTheAction: Boolean): Pointer;
var theTbl: TEERTable;
begin
  inc(NewTableCounter);

  theTbl:=TEERTable.Create(self, 'Table_'+FormatFloat('#00', NewTableCounter),
    DefModelFont, DefaultTableType, DefaultTablePrefix, PopupMenuEERTable);

  theTbl.Obj_X:=x;
  theTbl.Obj_Y:=y;

  //Use PositionGrid if selected
  if(UsePositionGrid)then
  begin
    theTbl.Obj_X:=(theTbl.Obj_X div PositionGrid.X) * PositionGrid.X;
    theTbl.Obj_Y:=(theTbl.Obj_Y div PositionGrid.Y) * PositionGrid.Y;
  end;


  theTbl.Obj_W:=100;
  theTbl.Obj_H:=100;

  //Display at the right pos and size
  theTbl.RefreshObj;

  SendRegionsToBack;

  theTbl.Show;
  theTbl.BringToFront;

  NewTable:=theTbl;

  if(LogTheAction)then
    LogAction(at_NewObj, theTbl.Obj_id,
      'ObjType=EERTable'+#13#10+
      'ObjName='+theTbl.ObjName+#13#10+
      'Obj_X='+IntToStr(theTbl.Obj_X)+#13#10+
      'Obj_Y='+IntToStr(theTbl.Obj_Y));

  if(Not(DisableModelRefresh))then
    DMEER.RefreshPalettes;
end;


function TEERModel.NewRelation(RelationKind: integer;
  theSrcTable, theDestTable: Pointer; LogTheAction: Boolean; FKColumnName: string = ''): Pointer;
var theRel: TEERRel;
  i, FKCount: integer;
  FKOrigName: String;
begin
  {//Check, if there already is a relation between this two tables
  for i:=0 to TEERTable(theSrcTable).RelStart.Count-1 do
    if(TEERRel(TEERTable(theSrcTable).RelStart[i]).DestTbl=theDestTable)then
      raise EInOutError.Create('There already is a relation between this two tables');

  for i:=0 to TEERTable(theSrcTable).RelEnd.Count-1 do
    if(TEERRel(TEERTable(theSrcTable).RelEnd[i]).SrcTbl=theDestTable)then
      raise EInOutError.Create('There already is a relation between this two tables');}

  inc(NewRelCounter);

  theRel:=TEERRel.Create(self, 'Rel_'+FormatFloat('#00', NewRelCounter));

  //Set Kind of Relation
  theRel.RelKind:=RelationKind;

  //Assign Tables to the relation
  theRel.SrcTbl:=TEERTable(theSrcTable);
  theRel.DestTbl:=TEERTable(theDestTable);

  //Add relation to Tables
  theRel.SrcTbl.RelStart.Add(theRel);
  theRel.DestTbl.RelEnd.Add(theRel);

  //FKColumnName mapping
  if(FKColumnName<>'')then
  begin
    FKCount:=0;
    for i:=0 to theRel.SrcTbl.Columns.Count-1 do
      if(TEERColumn(theRel.SrcTbl.Columns[i]).PrimaryKey)then
      begin
        FKOrigName:=TEERColumn(theRel.SrcTbl.Columns[i]).ColName;
        inc(FKCount);
      end;

    //Only works with ONE PK-Col , add mapping
    if(FKCount=1)then
      theRel.FKFields.Add(FKOrigName+'='+FKColumnName);
  end;

  //Display at the right pos and size
  theRel.SrcTbl.RefreshRelations;
  theRel.DestTbl.RefreshRelations;

  SendRegionsToBack;

  theRel.Show;
  theRel.BringToFront;
  theRel.relEnd.Show;
  theRel.relEnd.BringToFront;
  theRel.relMiddle.Show;
  theRel.relMiddle.BringToFront;

  if(DMEER.DisplayRelationNames)then
  begin
    theRel.RelCaption.Show;
    theRel.RelCaption.BringToFront;
  end;

  theRel.DestTbl.RefreshObj;

  theRel.DestTbl.DoPaint(self);

  NewRelation:=theRel;

  if(LogTheAction)then
    LogAction(at_NewObj, theRel.Obj_id,
      'ObjType=EERRelation'+#13#10+
      'ObjName='+theRel.ObjName+#13#10+
      'RelationKind='+IntToStr(theRel.RelKind)+#13#10+
      'SrcTable='+IntToStr(theRel.SrcTbl.Obj_id)+#13#10+
      'DestTable='+IntToStr(theRel.DestTbl.Obj_id));
end;

function TEERModel.NewNote(x, y: integer; LogTheAction: Boolean): Pointer;
var theNote: TEERNote;
begin
  inc(NewNoteCounter);

  theNote:=TEERNote.Create(self, 'Note_'+FormatFloat('#00', NewNoteCounter));

  theNote.Obj_X:=x;
  theNote.Obj_Y:=y;

  //Use PositionGrid if selected
  if(UsePositionGrid)then
  begin
    theNote.Obj_X:=(theNote.Obj_X div PositionGrid.X) * PositionGrid.X;
    theNote.Obj_Y:=(theNote.Obj_Y div PositionGrid.Y) * PositionGrid.Y;
  end;

  theNote.Obj_W:=100;
  theNote.Obj_H:=100;

  //Display at the right pos and size
  theNote.RefreshObj;

  SendRegionsToBack;

  theNote.Show;
  theNote.BringToFront;

  NewNote:=theNote;

  if(LogTheAction)then
    LogAction(at_NewObj, theNote.Obj_id,
      'ObjType=EERNote'+#13#10+
      'ObjName='+theNote.ObjName+#13#10+
      'Obj_X='+IntToStr(theNote.Obj_X)+#13#10+
      'Obj_Y='+IntToStr(theNote.Obj_Y));

end;

function TEERModel.NewRegion(x, y, w, h: integer; LogTheAction: Boolean): Pointer;
var theRegion: TEERRegion;
begin
  NewRegion:=nil;
  
  if(w<20)or(h<20)then
    Exit;

  inc(NewRegionCounter);

  theRegion:=TEERRegion.Create(self, 'Region_'+FormatFloat('#00', NewRegionCounter));

  theRegion.Obj_X:=x;
  theRegion.Obj_Y:=y;
  theRegion.Obj_W:=w;
  theRegion.Obj_H:=h;

  //Use PositionGrid if selected
  if(UsePositionGrid)then
  begin
    theRegion.Obj_X:=(theRegion.Obj_X div PositionGrid.X) * PositionGrid.X;
    theRegion.Obj_Y:=(theRegion.Obj_Y div PositionGrid.Y) * PositionGrid.Y;
    theRegion.Obj_W:=(theRegion.Obj_W div PositionGrid.X) * PositionGrid.X;
    theRegion.Obj_H:=(theRegion.Obj_H div PositionGrid.Y) * PositionGrid.Y;
  end;


  theRegion.RegionColor:=LastRegionColor mod (RegionColors.Count-1);
  inc(LastRegionColor);

  //Display at the right pos and size
  theRegion.RefreshObj;

  theRegion.Show;
  theRegion.SendToBack;
  GridPaintBox.SendToBack;

  NewRegion:=theRegion;

  if(LogTheAction)then
    LogAction(at_NewObj, theRegion.Obj_id,
      'ObjType=EERRegion'+#13#10+
      'ObjName='+theRegion.ObjName+#13#10+
      'Obj_X='+IntToStr(theRegion.Obj_X)+#13#10+
      'Obj_Y='+IntToStr(theRegion.Obj_Y)+#13#10+
      'Obj_W='+IntToStr(theRegion.Obj_W)+#13#10+
      'Obj_H='+IntToStr(theRegion.Obj_H));

end;

function TEERModel.NewImage(x, y, w, h: integer; LogTheAction: Boolean): Pointer;
var theImage: TEERImage;
begin
  {NewImage:=nil;

  if(w<20)or(h<20)then
    Exit;}

  inc(NewImageCounter);

  theImage:=TEERImage.Create(self, 'Image_'+FormatFloat('#00', NewImageCounter));

  theImage.Obj_X:=x;
  theImage.Obj_Y:=y;
  theImage.Obj_W:=w;
  theImage.Obj_H:=h;

  //Use PositionGrid if selected
  if(UsePositionGrid)then
  begin
    theImage.Obj_X:=(theImage.Obj_X div PositionGrid.X) * PositionGrid.X;
    theImage.Obj_Y:=(theImage.Obj_Y div PositionGrid.Y) * PositionGrid.Y;
  end;

  //Display at the right pos and size
  theImage.RefreshObj;

  theImage.Show;
  theImage.BringToFront;

  NewImage:=theImage;

  if(LogTheAction)then
    LogAction(at_NewObj, theImage.Obj_id,
      'ObjType=EERImage'+#13#10+
      'ObjName='+theImage.ObjName+#13#10+
      'Obj_X='+IntToStr(theImage.Obj_X)+#13#10+
      'Obj_Y='+IntToStr(theImage.Obj_Y)+#13#10+
      'Obj_W='+IntToStr(theImage.Obj_W)+#13#10+
      'Obj_H='+IntToStr(theImage.Obj_H));

end;

procedure TEERModel.SendRegionsToBack;
var i: integer;
begin
  //Set TEERRegion Component-Indices to max
  for i:=ComponentCount-2 downto 0 do
    if(Components[I].Classname='TEERRegion')then
      Components[I].ComponentIndex:=
        ComponentCount-2;

  //send GridPaintBox to background
  GridPaintBox.ComponentIndex:=ComponentCount-1;
end;


procedure TEERModel.CheckAllRelations;
var i, TblCount: integer;
  Tbl2Refresh: TList;
begin
  TblCount:=0;
  for i:=0 to ComponentCount-1 do
    if(Components[i].Classname='TEERTable')then
      inc(TblCount);

  Tbl2Refresh:=TList.Create;
  try
    //Check all relations
    for i:=0 to TblCount-1 do
      CheckRelations(Tbl2Refresh);

    //Refresh all Tables in ObjList
    for i:=0 to Tbl2Refresh.Count-1 do
      TEERTable(Tbl2Refresh[i]).RefreshObj;
  finally
    Tbl2Refresh.Free;
  end;

  //Check if there is a primary key and refresh it
  for i:=0 to ComponentCount-1 do
    if(Components[i].Classname='TEERTable')then
      TEERTable(Components[i]).CheckPrimaryIndex;
end;

procedure TEERModel.CheckRelations(Tbl2Refresh: TList);
var i, j, k, l, keypos, pkcount: integer;
  theColumn: TEERColumn;
  theIndex: TEERIndex;
begin
  //Clear FK_checked Flag
  for l:=0 to ComponentCount-1 do
    if(Components[l].Classname='TEERTable')then
      for k:=0 to TEERTable(Components[l]).Columns.Count-1 do
        TEERColumn(TEERTable(Components[l]).Columns[k]).FK_checked:=False;

  //Do for all tables
  for l:=0 to ComponentCount-1 do
  begin
    if(Components[l].Classname='TEERTable')then
    begin
      //Check tables where relations end
      for k:=0 to TEERTable(Components[l]).RelEnd.Count-1 do
      begin
        with TEERRel(TEERTable(Components[l]).RelEnd[k]) do
        begin
          if(RelKind=rk_1n)or(RelKind=rk_1nNonId)or(RelKind=rk_11)or(RelKind=rk_11Sub)or(RelKind=rk_11NonId)then
          begin
            //Get number of primary keys in destination table
            pkcount:=0;
            for i:=0 to TEERTable(DestTbl).Columns.Count-1 do
              if(TEERColumn(TEERTable(DestTbl).Columns[i]).PrimaryKey)then
                inc(pkcount);

            for i:=0 to TEERTable(SrcTbl).Columns.Count-1 do
            begin
              //For all primary keys
              if(TEERColumn(TEERTable(SrcTbl).Columns[i]).PrimaryKey)then
              begin
                keypos:=-1;
                for j:=0 to TEERTable(DestTbl).Columns.Count-1 do
                begin
                  if(CompareText(TEERColumn(TEERTable(DestTbl).Columns[j]).ColName,
                    FKFields.Values[TEERColumn(TEERTable(SrcTbl).Columns[i]).ColName])=0)and
                      ((RelKind=rk_1nNonId)or
                      (RelKind=rk_1n)or
                      (RelKind=rk_11)or
                      (RelKind=rk_11NonId)or
                      (RelKind=rk_11Sub))then
                    begin
                      keypos:=j;

                      //if 1n or 1:1 or 1:1sub and the col is NOT a primary key, set make it primary
                      if((RelKind=rk_1n)or(RelKind=rk_11)or(RelKind=rk_11Sub))and
                        (not(TEERColumn(TEERTable(DestTbl).Columns[j]).PrimaryKey))then
                        TEERColumn(TEERTable(DestTbl).Columns[j]).PrimaryKey:=True;

                      TEERColumn(TEERTable(DestTbl).Columns[j]).IsForeignKey:=True;

                      TEERColumn(TEERTable(DestTbl).Columns[j]).FK_checked:=True;

                      //copy datatype
                      if(DMEER.SyncDatatypesOfForeignKeys)then
                      begin
                        TEERColumn(TEERTable(DestTbl).Columns[j]).idDatatype:=
                          TEERColumn(TEERTable(SrcTbl).Columns[i]).idDatatype;
                        TEERColumn(TEERTable(DestTbl).Columns[j]).DatatypeParams:=
                          TEERColumn(TEERTable(SrcTbl).Columns[i]).DatatypeParams;
                        TEERColumn(TEERTable(DestTbl).Columns[j]).OptionSelected:=
                          TEERColumn(TEERTable(SrcTbl).Columns[i]).OptionSelected;
                      end;

                      //Add to refreshlist if not already added
                      if(Tbl2Refresh.IndexOf(TEERTable(DestTbl))=-1)then
                        Tbl2Refresh.Add(TEERTable(DestTbl));
                    end;
                end;

                //if key not found
                if(keypos=-1)and
                  (FKFields.Values[TEERColumn(TEERTable(SrcTbl).Columns[i]).ColName]<>'')then
                begin
                  theColumn:=TEERColumn.Create(DestTbl);

                  try
                    theColumn.ColName:=FKFields.Values[TEERColumn(TEERTable(SrcTbl).Columns[i]).ColName];

                    theColumn.Obj_id:=DMMain.GetNextGlobalID;
                    theColumn.Pos:=1;
                    theColumn.idDatatype:=TEERColumn(TEERTable(SrcTbl).Columns[i]).idDatatype;
                    theColumn.DatatypeParams:=TEERColumn(TEERTable(SrcTbl).Columns[i]).DatatypeParams;
                    theColumn.Width:=TEERColumn(TEERTable(SrcTbl).Columns[i]).Width;
                    theColumn.Prec:=TEERColumn(TEERTable(SrcTbl).Columns[i]).Prec;
                    theColumn.PrimaryKey:=((RelKind=rk_1n)or(RelKind=rk_11)or(RelKind=rk_11Sub));
                    theColumn.NotNull:=TEERColumn(TEERTable(SrcTbl).Columns[i]).NotNull;
                    theColumn.AutoInc:=False;
                    theColumn.IsForeignKey:=True;
                    theColumn.FK_checked:=True;

                    //Get Option Defaults
                    for j:=0 to TEERDatatype(TEERModel(TEERTable(SrcTbl).Parent).GetDataType(theColumn.idDatatype)).OptionCount-1 do
                      theColumn.OptionSelected[j]:=
                        TEERDatatype(TEERModel(TEERTable(SrcTbl).Parent).GetDataType(theColumn.idDatatype)).OptionDefaults[j];

                    theColumn.DefaultValue:='';

                    TEERTable(DestTbl).Columns.Insert(pkcount, theColumn);
                  except
                    theColumn.Free;
                  end;

                  //Add to refreshlist if not already added
                  if(Tbl2Refresh.IndexOf(TEERTable(DestTbl))=-1)then
                    Tbl2Refresh.Add(TEERTable(DestTbl));
                end;
              end;
            end;
          end;

          //Create Index for FK Rev Def if there is non at the moment and
          //this relation has RefDef activated and
          //the model's CreateFKRefDefIndex is set
          if(FKRefDefIndex_Obj_id=-1)and(CreateRefDef)and
            (CreateFKRefDefIndex)then
          begin
            theIndex:=TEERIndex.Create(TEERTable(DestTbl));
            try
              //Count FK Indices already in the table (for table name)
              j:=0;
              for i:=0 to TEERTable(DestTbl).Indices.Count-1 do
                if(TEERIndex(TEERTable(DestTbl).Indices[i]).FKRefDef_Obj_id>-1)then
                  inc(j);

              //Set Index Values
              theIndex.Obj_id:=DMMain.GetNextGlobalID;
              theIndex.IndexName:=TEERTable(DestTbl).ObjName+'_'+
                'FKIndex'+IntToStr(j+1);
              theIndex.Pos:=TEERTable(DestTbl).Indices.Count;
              theIndex.FKRefDef_Obj_id:=Obj_id;

              //Add index to Table's Index list
              try
                TEERTable(DestTbl).Indices.Add(theIndex);

                //Add columns to Index
                for i:=0 to FKFields.Count-1 do
                  if(TEERTable(DestTbl).GetColumnByName(FKFields.ValueFromIndex[i])<>nil)then
                    theIndex.Columns.Add(IntToStr(
                      TEERColumn(TEERTable(DestTbl).GetColumnByName(FKFields.ValueFromIndex[i])).Obj_id));

                FKRefDefIndex_Obj_id:=theIndex.Obj_id;

                //Add to refreshlist if not already added
                if(Tbl2Refresh.IndexOf(TEERTable(DestTbl))=-1)then
                  Tbl2Refresh.Add(TEERTable(DestTbl));
              except
                //If the index was already added, remove it
                TEERTable(DestTbl).Indices.Remove(theIndex);
              end;
            except
              theIndex.Free;
            end;
          end
          //If the relation's RefDef was turned off, remove index
          else if(FKRefDefIndex_Obj_id>-1)and(Not(CreateRefDef))then
          begin
            for i:=0 to TEERTable(DestTbl).Indices.Count-1 do
              if(TEERIndex(TEERTable(DestTbl).Indices[i]).Obj_id=FKRefDefIndex_Obj_id)then
              begin
                TEERTable(DestTbl).Indices.Delete(i);
                break;
              end;
            FKRefDefIndex_Obj_id:=-1;

            //Add to refreshlist if not already added
            if(Tbl2Refresh.IndexOf(TEERTable(DestTbl))=-1)then
              Tbl2Refresh.Add(TEERTable(DestTbl));
          end
          //If the relation's RefDef is turned on, check the columns of the index
          else if(FKRefDefIndex_Obj_id>-1)and(CreateRefDef)then
          begin
            //To simplify this, clear old columns and add new
            i:=0;
            while(i<TEERTable(DestTbl).Indices.Count)do
            begin
              if(TEERIndex(TEERTable(DestTbl).Indices[i]).Obj_id=FKRefDefIndex_Obj_id)then
              begin
                theIndex:=TEERIndex(TEERTable(DestTbl).Indices[i]);
                theIndex.Columns.Clear;

                //Add columns to Index
                for j:=0 to FKFields.Count-1 do
                  if(TEERTable(DestTbl).GetColumnByName(FKFields.ValueFromIndex[j])<>nil)then
                    theIndex.Columns.Add(IntToStr(
                      TEERColumn(TEERTable(DestTbl).GetColumnByName(FKFields.ValueFromIndex[j])).Obj_id));

                if(theIndex.Columns.Count=0)then
                begin
                  TEERTable(DestTbl).Indices.Delete(i);
                end;

                //Add to refreshlist if not already added
                if(Tbl2Refresh.IndexOf(TEERTable(DestTbl))=-1)then
                  Tbl2Refresh.Add(TEERTable(DestTbl));


                break;
              end;

              inc(i);
            end;
          end;
        end;
      end;
    end;
  end;

  //Clear obsolete FKs
  for l:=0 to ComponentCount-1 do
  begin
    if(Components[l].Classname='TEERTable')then
    begin
      k:=0;
      while(k<=TEERTable(Components[l]).Columns.Count-1)do
      begin
        if(TEERColumn(TEERTable(Components[l]).Columns[k]).IsForeignKey)and
          (TEERColumn(TEERTable(Components[l]).Columns[k]).FK_checked=False)then
        begin
          TEERTable(Components[l]).DeleteColumn(k);

          //Add to refreshlist if not already added
          if(Tbl2Refresh.IndexOf(TEERTable(Components[l]))=-1)then
             Tbl2Refresh.Add(TEERTable(Components[l]));
        end
        else
          inc(k);
      end;
    end;
  end;
end;


procedure TEERModel.SaveToFile(fname: string; WriteSettings: Boolean = True;
  OnlySelected: Boolean = False; UpdateModelFileName: Boolean = True);
var theFile: Textfile;
  i, j: integer;
  DT: TEERDatatype;
  xpos, ypos: integer;
  s: string;
  DecSep: char;
  origModelFilename: string;
begin
  if(fname='')then
    raise EInOutError.Create(DMMain.GetTranslatedMessage('This model has no filename. Please use Save As... to save the model.', 31));

  origModelFilename:=ModelFilename;
  try
    ModelFilename:=fname;

    AssignFile(theFile, fname);
    ReWrite(theFile);
    try

      WriteLn(theFile, '<?xml version="1.0" standalone="yes" ?>');
      WriteLn(theFile, '<DBMODEL Version="4.0">');

      //--------------------------------------------------------
      //ModelSettings

      if(WriteSettings)then
      begin
        WriteLn(theFile, '<SETTINGS>');

        if(Assigned(TForm(parent).HorzScrollBar))then
          xpos:=TForm(parent).HorzScrollBar.Position
        else
          xpos:=0;

        if(Assigned(TForm(parent).VertScrollBar))then
          ypos:=TForm(parent).VertScrollBar.Position
        else
          ypos:=0;

        //Get selected Pages
        for i:=0 to 200 do
          if(SelectedPages[i]=True)then
            s:=s+IntToStr(i)+';';
        s:=Copy(s, 1, Length(s)-1);

        if(ModelName='')then
          ModelName:=Copy(ExtractFileName(ModelFilename),
            1, Length(ExtractFileName(ModelFilename))-Length(ExtractFileExt(ModelFilename)));

        DecSep:=DecimalSeparator;
        try
          DecimalSeparator:='.';

          WriteLn(theFile, '<GLOBALSETTINGS '+
            'ModelName="'+DMMain.EncodeText4XML(ModelName)+'" '+
            'IDModel="'+IntToStr(IDModel)+'" '+
            'IDVersion="'+IntToStr(IDVersion)+'" '+
            'VersionStr="'+VersionStr+'" '+
            'Comments="'+DMMain.EncodeText4XML(ModelComments)+'" '+
            'UseVersionHistroy="'+IntToStr(Ord(UseVersionHistroy))+'" '+
            'AutoIncVersion="'+IntToStr(Ord(AutoIncVersion))+'" '+
            'DatabaseType="'+DatabaseType+'" '+
            'ZoomFac="'+FormatFloat('####0.00', ZoomFac)+'" '+
            'XPos="'+IntToStr(xpos)+'" '+
            'YPos="'+IntToStr(ypos)+'" '+
            'DefaultDataType="'+IntToStr(DefaultDataType)+'" '+
            'DefaultTablePrefix="'+IntToStr(DefaultTablePrefix)+'" '+
            'DefSaveDBConn="'+DefSaveDBConn+'" '+
            'DefSyncDBConn="'+DefSyncDBConn+'" '+
            'DefQueryDBConn="'+DefQueryDBConn+'" '+
            'Printer="'+ModelPrinter+'" '+
            'HPageCount="'+FormatFloat('0.0##############', HPageCount)+'" '+
            'PageAspectRatio="'+FormatFloat('0.0##############', PageAspectRatio)+'" '+
            'PageOrientation="'+IntToStr(Ord(PageOrientation))+'" '+
            'PageFormat="'+PageFormat+'" '+
            'SelectedPages="'+s+'" '+
            'UsePositionGrid="'+IntToStr(Ord(UsePositionGrid))+'" '+
            'PositionGridX="'+IntToStr(PositionGrid.X)+'" '+
            'PositionGridY="'+IntToStr(PositionGrid.Y)+'" '+
            'TableNameInRefs="'+IntToStr(Ord(TableNameInRefs))+'" '+
            'DefaultTableType="'+IntToStr(DefaultTableType)+'" '+
            'ActivateRefDefForNewRelations="'+IntToStr(Ord(ActivateRefDefForNewRelations))+'" '+
            'FKPrefix="'+DMMain.EncodeText4XML(FKPrefix)+'" '+
            'FKPostfix="'+DMMain.EncodeText4XML(FKPostfix)+'" '+
            'CreateFKRefDefIndex="'+IntToStr(Ord(CreateFKRefDefIndex))+'" '+
            'DBQuoteCharacter="'+DMMain.EncodeText4XML(DBQuoteCharacter)+'" '+
            'CreateSQLforLinkedObjects="'+IntToStr(Ord(CreateSQLforLinkedObjects))+'" '+
            'DefModelFont="'+DMMain.EncodeText4XML(DefModelFont)+'" '+
            'CanvasWidth="'+IntToStr(EERModel_Width)+'" '+
            'CanvasHeight="'+IntToStr(EERModel_Height)+'" '+
            '/>');
        finally
          DecimalSeparator:=DecSep;
        end;



        //DatatypesGroups
        WriteLn(theFile, '<DATATYPEGROUPS>');
        for i:=0 to DatatypeGroups.Count-1 do
          WriteLn(theFile, '<DATATYPEGROUP Name="'+TEERDatatypeGroup(DatatypeGroups[i]).Groupname+'" '+
            'Icon="'+IntToStr(TEERDatatypeGroup(DatatypeGroups[i]).IconNr)+'" />');
        WriteLn(theFile, '</DATATYPEGROUPS>');

        //Datatypes
        WriteLn(theFile, '<DATATYPES>');
        for i:=0 to Datatypes.Count-1 do
        begin
          DT:=TEERDatatype(Datatypes[i]);

          WriteLn(theFile, '<DATATYPE '+
            'ID="'+IntToStr(DT.id)+'" '+
            'IDGroup="'+IntToStr(DT.group)+'" '+
            'TypeName="'+DMMain.EncodeText4XML(DT.TypeName)+'" '+
            'Description="'+DMMain.EncodeText4XML(DT.description)+'" '+
            'ParamCount="'+IntToStr(DT.ParamCount)+'" '+
            'OptionCount="'+IntToStr(DT.OptionCount)+'" '+
            'ParamRequired="'+IntToStr(Ord(DT.ParamRequired))+'" '+
            'EditParamsAsString="'+IntToStr(Ord(DT.EditParamsAsString))+'" '+
            'SynonymGroup="'+IntToStr(Ord(DT.SynonymGroup))+'" '+
            'PhysicalMapping="'+IntToStr(Ord(DT.PhysicalMapping))+'" '+
            'PhysicalTypeName="'+DMMain.EncodeText4XML(DT.PhysicalTypeName)+'" '+
            '>');

          if(DT.ParamCount>0)then
          begin
            WriteLn(theFile, '<PARAMS>');
            for j:=0 to DT.ParamCount-1 do
              WriteLn(theFile, '<PARAM Name="'+DMMain.EncodeText4XML(DT.Param[j])+'" '+
              '/>');
            WriteLn(theFile, '</PARAMS>');
          end;

          if(DT.OptionCount>0)then
          begin
            WriteLn(theFile, '<OPTIONS>');
            for j:=0 to DT.OptionCount-1 do
              WriteLn(theFile, '<OPTION Name="'+DMMain.EncodeText4XML(DT.Options[j])+'" '+
                'Default="'+IntToStr(Ord(DT.OptionDefaults[j]))+'" '+
                '/>');
            WriteLn(theFile, '</OPTIONS>');
          end;

          WriteLn(theFile, '</DATATYPE>');
        end;
        WriteLn(theFile, '</DATATYPES>');

        //Common DataTypes
        WriteLn(theFile, '<COMMON_DATATYPES>');
        for i:=0 to CommonDataType.Count-1 do
          WriteLn(theFile, '<COMMON_DATATYPE ID="'+CommonDataType[i]+'" />');
        WriteLn(theFile, '</COMMON_DATATYPES>');

        //TablePrefix
        WriteLn(theFile, '<TABLEPREFIXES>');
        for i:=0 to TablePrefix.Count-1 do
          WriteLn(theFile, '<TABLEPREFIX Name="'+DMMain.EncodeText4XML(TablePrefix[i])+'" />');
        WriteLn(theFile, '</TABLEPREFIXES>');

        //Region Colors
        WriteLn(theFile, '<REGIONCOLORS>');
        for i:=0 to RegionColors.Count-1 do
          WriteLn(theFile, '<REGIONCOLOR Color="'+RegionColors[i]+'" />');
        WriteLn(theFile, '</REGIONCOLORS>');

        //Reserved Words
        DecSep:=DecimalSeparator;
        try
          DecimalSeparator:='.';

          WriteLn(theFile, '<POSITIONMARKERS>');
          for i:=0 to PosMarkers.Count-1 do
            WriteLn(theFile, '<POSITIONMARKER ZoomFac="'+FormatFloat('##0.0#####', TPosMarker(PosMarkers.Items[i]).ZoomFac)+'" '+
              'X="'+IntToStr(TPosMarker(PosMarkers.Items[i]).X)+'" Y="'+IntToStr(TPosMarker(PosMarkers.Items[i]).Y)+'" />');
          WriteLn(theFile, '</POSITIONMARKERS>');
        finally
          DecimalSeparator:=DecSep;
        end;


        WriteLn(theFile, '</SETTINGS>');
      end;

      //-----------------------------------------------------------
      //Metadata
      WriteLn(theFile, '<METADATA>');

      //Regions
      WriteLn(theFile, '<REGIONS>');
      for i:=0 to ComponentCount-1 do
        if(Components[i].Classname='TEERRegion')and
          ((OnlySelected=False)or(TEERObj(Components[i]).Selected))then
          Write(theFile, TEERObj(Components[i]).GetXML);

      WriteLn(theFile, '</REGIONS>');


      //Tables
      WriteLn(theFile, '<TABLES>');
      for i:=0 to ComponentCount-1 do
        if(Components[i].Classname='TEERTable')and
          ((OnlySelected=False)or(TEERObj(Components[i]).Selected))then
          Write(theFile, TEERObj(Components[i]).GetXML);

      WriteLn(theFile, '</TABLES>');


      //Relations
      WriteLn(theFile, '<RELATIONS>');
      for i:=0 to ComponentCount-1 do
        if(Components[i].Classname='TEERRel')and
          ((OnlySelected=False)or(TEERObj(Components[i]).Selected))then
          Write(theFile, TEERObj(Components[i]).GetXML);

      WriteLn(theFile, '</RELATIONS>');

      //Notes
      WriteLn(theFile, '<NOTES>');
      for i:=0 to ComponentCount-1 do
        if(Components[i].Classname='TEERNote')and
          ((OnlySelected=False)or(TEERObj(Components[i]).Selected))then
          Write(theFile, TEERObj(Components[i]).GetXML);

      WriteLn(theFile, '</NOTES>');

      //Notes
      WriteLn(theFile, '<IMAGES>');
      for i:=0 to ComponentCount-1 do
        if(Components[i].Classname='TEERImage')and
          ((OnlySelected=False)or(TEERObj(Components[i]).Selected))then
          Write(theFile, TEERObj(Components[i]).GetXML);

      WriteLn(theFile, '</IMAGES>');

      WriteLn(theFile, '</METADATA>');


    
      //-----------------------------------------------------------
      //PluginData
      WriteLn(theFile, '<PLUGINDATA>');

      WriteLn(theFile, '<PLUGINDATARECORDS>');
      for i:=0 to PluginData.Count-1 do
      begin
        //PluginData
        WriteLn(theFile, '<PLUGINDATARECORD PluginName="'+DMMain.EncodeText4XML(TEERPluginData(PluginData[i]).PluginName)+'" '+
          'Obj_id="'+IntToStr(TEERPluginData(PluginData[i]).Obj_id)+'" '+
          'DataValue="nil">');
        WriteLn(theFile, '<PLUGINDATAPARAMS>');
        for j:=0 to TEERPluginData(PluginData[i]).Params.Count-1 do
          WriteLn(theFile, '<PLUGINDATAPARAM Value="'+DMMain.EncodeText4XML(TEERPluginData(PluginData[i]).Params[j])+'" />');
        WriteLn(theFile, '</PLUGINDATAPARAMS>');
        WriteLn(theFile, '</PLUGINDATARECORD>');
      end;
      WriteLn(theFile, '</PLUGINDATARECORDS>');

      WriteLn(theFile, '</PLUGINDATA>');



      //-----------------------------------------------------------
      //QueryData
      WriteLn(theFile, '<QUERYDATA>');

      WriteLn(theFile, '<QUERYRECORDS>');
      for i:=0 to StoredSQLCmds.Count-1 do
      begin
        WriteLn(theFile, '<QUERYRECORD SQLCmdType="'+IntToStr(TStoredSQLCmd(StoredSQLCmds[i]).SQLCmdType)+'" '+
          'StoredPosition="'+DMMain.EncodeText4XML(TStoredSQLCmd(StoredSQLCmds[i]).StoredPosition)+'" '+
          'SQLText="'+DMMain.EncodeText4XML(TStoredSQLCmd(StoredSQLCmds[i]).SQLText)+'" />');
      end;
      WriteLn(theFile, '</QUERYRECORDS>');

      WriteLn(theFile, '</QUERYDATA>');


      //-----------------------------------------------------------
      //Placed Models
      WriteLn(theFile, '<LINKEDMODELS>');

      for i:=0 to LinkedModels.Count-1 do
      begin
        WriteLn(theFile, '<LINKEDMODEL IDLinkedModel="'+IntToStr(TEERLinkedModel(LinkedModels[i]).IDLinkedModel)+'" '+
          'ModelName="'+DMMain.EncodeText4XML(TEERLinkedModel(LinkedModels[i]).ModelName)+'" '+
          'IDModel="'+IntToStr(TEERLinkedModel(LinkedModels[i]).IDModel)+'" '+
          'IsStoredInDB="'+IntToStr(Ord(TEERLinkedModel(LinkedModels[i]).IsStoredInDB))+'" '+
          'ModelFilename="'+DMMain.EncodeText4XML(TEERLinkedModel(LinkedModels[i]).ModelFilename)+'" '+
          'DriverName="'+DMMain.EncodeText4XML(TEERLinkedModel(LinkedModels[i]).DriverName)+'" '+
          'DBConnName="'+DMMain.EncodeText4XML(TEERLinkedModel(LinkedModels[i]).DBConnName)+'" '+
          'HostCaption="'+DMMain.EncodeText4XML(TEERLinkedModel(LinkedModels[i]).HostCaption)+'" '+
          'HostName="'+DMMain.EncodeText4XML(TEERLinkedModel(LinkedModels[i]).HostName)+'" '+
          'Database="'+DMMain.EncodeText4XML(TEERLinkedModel(LinkedModels[i]).Database)+'" '+
          'User="'+DMMain.EncodeText4XML(TEERLinkedModel(LinkedModels[i]).User)+'" />');
      end;

      WriteLn(theFile, '</LINKEDMODELS>');


      WriteLn(theFile, '</DBMODEL>');
    finally
      CloseFile(theFile);
    end;
  finally
    if(Not(UpdateModelFileName))then
      ModelFilename:=origModelFilename;
  end;
end;

procedure TEERModel.LoadFromFile2(fname: string; ReadSettings: Boolean = True; SelectObjs: Boolean = False; MoveToSavedPosition: Boolean = True; AddDataToExistingModel: Boolean = True);
var Parser: TXmlParser;
  i, j, maxid: integer;
  s: string;
  DecSep: char;
  theTbl: TEERTable;

  procedure GetDATATYPEGROUPS;
  var i: integer;
    theDatatypeGroup: TEERDatatypeGroup;
  begin
    i:=0;
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='DATATYPEGROUP')then
        begin
          theDatatypeGroup:=TEERDatatypeGroup.Create(self);
          theDatatypeGroup.GroupName:=Parser.CurAttr.Value('Name');
          try
            theDatatypeGroup.IconNr:=StrToInt(Parser.CurAttr.Value('Icon'));
          except
            theDatatypeGroup.IconNr:=i+1;
          end;

          DatatypeGroups.Add(theDatatypeGroup);
          inc(i);
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetDATATYPESparams(theDatatype: TEERDatatype);
  var i: integer;
  begin
    if(theDatatype=nil)then
      Exit;

    i:=0;
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='PARAM')then
        begin
          theDatatype.Param[i]:=DMMain.DecodeXMLText(Parser.CurAttr.Value('Name'));

          inc(i);
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetDATATYPESoptions(theDatatype: TEERDatatype);
  var i: integer;
  begin
    if(theDatatype=nil)then
      Exit;

    i:=0;
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='OPTION')then
        begin
          theDatatype.Options[i]:=DMMain.DecodeXMLText(Parser.CurAttr.Value('Name'));
          theDatatype.OptionDefaults[i]:=(Parser.CurAttr.Value('Default')='1');

          inc(i);
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetDATATYPES;
  var theDatatype: TEERDatatype;
  begin
    theDatatype:=nil;

    while(Parser.Scan)do
      if(Parser.CurPartType=ptStartTag)then
      begin
        if(Parser.CurName='DATATYPE')then
        begin
          theDatatype:=TEERDatatype.Create(self);
          try
            theDatatype.id:=StrToInt(Parser.CurAttr.Value('ID'));
            theDatatype.group:=StrToInt(Parser.CurAttr.Value('IDGroup'));
            theDatatype.TypeName:=DMMain.DecodeXMLText(Parser.CurAttr.Value('TypeName'));
            theDatatype.description:=DMMain.DecodeXMLText(Parser.CurAttr.Value('Description'));
            theDatatype.ParamCount:=StrToInt(Parser.CurAttr.Value('ParamCount'));
            theDatatype.OptionCount:=StrToInt(Parser.CurAttr.Value('OptionCount'));
            theDatatype.ParamRequired:=(Parser.CurAttr.Value('ParamRequired')='1');
            theDatatype.SynonymGroup:=StrToInt(Parser.CurAttr.Value('SynonymGroup'));
            try
              theDatatype.EditParamsAsString:=(Parser.CurAttr.Value('EditParamsAsString')='1');
            except
              theDatatype.EditParamsAsString:=False;
            end;

            try
              theDatatype.PhysicalMapping:=(Parser.CurAttr.Value('PhysicalMapping')='1');
              theDatatype.PhysicalTypeName:=DMMain.DecodeXMLText(Parser.CurAttr.Value('PhysicalTypeName'));
            except
              theDatatype.PhysicalMapping:=False;
              theDatatype.PhysicalTypeName:='';
            end;

            Datatypes.Add(theDatatype);
          except
            theDatatype.Free;
          end;
        end
        else if(Parser.CurName='PARAMS')then
          GetDATATYPESparams(theDatatype)
        else if(Parser.CurName='OPTIONS')then
          GetDATATYPESoptions(theDatatype)
        else
         break;
      end
      else if(Parser.CurPartType=ptEndTag)then
      begin
        if(Parser.CurName<>'DATATYPE')then
          break;
      end
      else
        break;
  end;

  procedure GetCOMMONDATATYPES;
  begin
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='COMMON_DATATYPE')then
        begin
          CommonDataType.Add(Parser.CurAttr.Value('ID'));
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetTABLEPREFIXES;
  begin
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='TABLEPREFIX')then
        begin
          TablePrefix.Add(Parser.CurAttr.Value('Name'));
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetREGIONCOLORS;
  begin
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='REGIONCOLOR')then
        begin
          RegionColors.Add(Parser.CurAttr.Value('Color'));
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetPOSITIONMARKERS;
  var i: integer;
  begin
    i:=0;
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='POSITIONMARKER')then
        begin
          if(i<PosMarkers.Count)then
          begin
            TPosMarker(PosMarkers[i]).ZoomFac:=StrToFloat(
              Parser.CurAttr.Value('ZoomFac'));
            TPosMarker(PosMarkers[i]).X:=
              StrToInt(Parser.CurAttr.Value('X'));
            TPosMarker(PosMarkers[i]).Y:=
              StrToInt(Parser.CurAttr.Value('Y'));

            inc(i);
          end;
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetREGIONS(SelectObjs: Boolean);
  var theRegion: TEERRegion;
  begin
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='REGION')then
        begin
          //Create Region
          theRegion:=NewRegion(StrToInt(Parser.CurAttr.Value('XPos')),
            StrToInt(Parser.CurAttr.Value('YPos')),
            StrToInt(Parser.CurAttr.Value('Width')),
            StrToInt(Parser.CurAttr.Value('Height')), False);

          //Read XML Data into Obj
          theRegion.SetXML2(Parser);

          theRegion.SetSelected(SelectObjs);
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetTABLES(SelectObjs: Boolean);
  var theTbl: TEERTable;
  begin
    while(Parser.Scan)do
      if(Parser.CurPartType=ptStartTag)then
      begin
        if(Parser.CurName='TABLE')then
        begin
          //Create Table
          theTbl:=NewTable(StrToInt(Parser.CurAttr.Value('XPos')),
            StrToInt(Parser.CurAttr.Value('YPos')), False);

          //Read XML Data into Obj
          theTbl.SetXML2(Parser);

          theTbl.SetSelected(SelectObjs);

          theTbl.RefreshStrechedImg:=True;
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetRELATIONS(SelectObjs: Boolean);
  var theRel: TEERRel;
    SourceTbl, DestTbl: TEERTable;
  begin
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='RELATION')then
        begin
          //Create Relation

          //Get ID for debugging
          SourceTbl:=GetEERObjectByID(StrToInt(Parser.CurAttr.Value('SrcTable')));
          DestTbl:=GetEERObjectByID(StrToInt(Parser.CurAttr.Value('DestTable')));

          //Check if both tables exist
          if(SourceTbl<>nil)and(DestTbl<>nil)then
          begin
            theRel:=NewRelation(StrToInt(Parser.CurAttr.Value('Kind')),
              SourceTbl,
              DestTbl, False);

            //Read XML Data into Obj
            theRel.SetXML2(Parser);

            theRel.SetSelected(SelectObjs);
          end;
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetNOTES(SelectObjs: Boolean);
  var theNote: TEERNote;
  begin
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='NOTE')then
        begin
          //Create Note
          theNote:=NewNote(StrToInt(Parser.CurAttr.Value('XPos')),
            StrToInt(Parser.CurAttr.Value('YPos')), False);

          theNote.SetSelected(SelectObjs);

          //Read XML Data into Obj
          theNote.SetXML2(Parser);
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetIMAGES(SelectObjs: Boolean);
  var theImage: TEERImage;
  begin
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='IMAGE')then
        begin
          //Create Image
          theImage:=NewImage(StrToInt(Parser.CurAttr.Value('XPos')),
            StrToInt(Parser.CurAttr.Value('YPos')),
            StrToInt(Parser.CurAttr.Value('Width')),
            StrToInt(Parser.CurAttr.Value('Height')), False);

          theImage.SetSelected(SelectObjs);

          //Read XML Data into Obj
          theImage.SetXML2(Parser);
        end
        else
          break;
      end
      else
        break;
  end;

  procedure GetPLUGINDATARECORDS;
  var thePluginData: TEERPluginData;
  begin
    thePluginData:=nil;
    while(Parser.Scan)do
      if(Parser.CurPartType=ptStartTag)then
      begin
        if(Parser.CurName='PLUGINDATARECORD')then
        begin
          //Create Region
          thePluginData:=AddPluginData(DMMain.DecodeXMLText(Parser.CurAttr.Value('PluginName')),
            StrToInt(Parser.CurAttr.Value('Obj_id')));
        end
        else if(Parser.CurName='PLUGINDATAPARAMS')then
        else
          break;
      end
      else if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='PLUGINDATAPARAM')then
        begin
          thePluginData.Params.Add(DMMain.DecodeXMLText(Parser.CurAttr.Value('Value')));
        end
        else
          break;
      end
      else if(Parser.CurPartType=ptEndTag)then
      begin
        if(Parser.CurName='PLUGINDATAPARAMS')or
          (Parser.CurName='PLUGINDATARECORD')or
          (Parser.CurName='PLUGINDATARECORDS')then
        else
          break;
      end
      else
        break;
  end;

  procedure GetQUERYRECORDS;
  var theSQLCmd: TStoredSQLCmd;
  begin
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='QUERYRECORD')then
        begin
          theSQLCmd:=TStoredSQLCmd.Create;
          theSQLCmd.SQLCmdType:=StrToInt(Parser.CurAttr.Value('SQLCmdType'));
          theSQLCmd.StoredPosition:=DMMain.DecodeXMLText(Parser.CurAttr.Value('StoredPosition'));
          theSQLCmd.SQLText:=DMMain.DecodeXMLText(Parser.CurAttr.Value('SQLText'));
          StoredSQLCmds.Add(theSQLCmd);
        end
        else
          break;
      end
      else if(Parser.CurPartType=ptEndTag)then
      begin
        if(Parser.CurName='QUERYRECORDS')then
        else
          break;
      end
      else
        break;
  end;

  procedure GetLINKEDMODELS;
  var theEERLinkedModel: TEERLinkedModel;
  begin
    while(Parser.Scan)do
      if(Parser.CurPartType=ptEmptyTag)then
      begin
        if(Parser.CurName='LINKEDMODEL')then
        begin
          theEERLinkedModel:=TEERLinkedModel.Create(self);
          theEERLinkedModel.IDLinkedModel:=StrToInt(Parser.CurAttr.Value('IDLinkedModel'));
          theEERLinkedModel.ModelName:=DMMain.DecodeXMLText(Parser.CurAttr.Value('ModelName'));
          theEERLinkedModel.IDModel:=StrToInt(Parser.CurAttr.Value('IDModel'));
          theEERLinkedModel.IsStoredInDB:=(Parser.CurAttr.Value('IsStoredInDB')='1');
          theEERLinkedModel.ModelFilename:=DMMain.DecodeXMLText(Parser.CurAttr.Value('ModelFilename'));
          theEERLinkedModel.DriverName:=DMMain.DecodeXMLText(Parser.CurAttr.Value('DriverName'));
          theEERLinkedModel.DBConnName:=DMMain.DecodeXMLText(Parser.CurAttr.Value('DBConnName'));
          theEERLinkedModel.HostCaption:=DMMain.DecodeXMLText(Parser.CurAttr.Value('HostCaption'));
          theEERLinkedModel.HostName:=DMMain.DecodeXMLText(Parser.CurAttr.Value('HostName'));
          theEERLinkedModel.Database:=DMMain.DecodeXMLText(Parser.CurAttr.Value('Database'));
          theEERLinkedModel.User:=DMMain.DecodeXMLText(Parser.CurAttr.Value('User'));
          LinkedModels.Add(theEERLinkedModel);
        end
        else
          break;
      end
      else if(Parser.CurPartType=ptEndTag)then
      begin
        if(Parser.CurName='LINKEDMODELS')then
        else
          break;
      end
      else
        break;
  end;

begin
  DecSep:=DecimalSeparator;
  try
    DecimalSeparator:='.';

    DisableModelRefresh:=True;
    try

      //if loaded data is not added, delete existing data
      if(Not(AddDataToExistingModel))then
      begin
        ModelIsBeingCleared:=True;
        try
          for i:=ComponentCount-1 downto 0 do
            if(Components[I].Classparent=TEERObj)then
              TEERObj(Components[I]).DeleteObj;
        finally
          ModelIsBeingCleared:=False;
        end;
      end;


      // Initialise Parser
      Parser:=TXmlParser.Create;
      try
        Parser.Normalize := False;

        Parser.LoadFromFile(fname);

        Parser.StartScan;

        //Scan through the model
        while(Parser.Scan)do
        begin
          case Parser.CurPartType of
            ptStartTag,
            ptEmptyTag: // Process Parser.CurName and Parser.CurAttr (see below) fields here
            begin
              if(Parser.CurName='DBMODEL')then
              begin
                //Check Version
                s:='unknown';
                if(Parser.CurAttr.Count=1)then
                  s:=Parser.CurAttr.Value(0);

                if(s<>'4.0')then
                  if(MessageDlg(DMMain.GetTranslatedMessage('The selected DB-Model has the Version Number:'#13#10+'%s'+#13#10+
                    'Only Version 4.0 DB-Models should be opened with this Version of DBDesigner.'+#13#10#13#10+
                    'Do you want to try to open the file anyway?', 32,
                    ExtractFileName(fname)+', Version '+s),
                    mtInformation, [mbYes, mbNo], 0)=mrNo)then
                    Exit;
              end

              // -------------------------------------------------------
              // Settings

              else if(Parser.CurName='GLOBALSETTINGS')and
                (ReadSettings)then
              begin
                ModelFilename:=fname;

                try
                  s:=DMMain.DecodeXMLText(Parser.CurAttr.Value('ModelName'));
                  if(s='')then
                    s:=Copy(ExtractFileName(ModelFilename),
                      1, Length(ExtractFileName(ModelFilename))-Length(ExtractFileExt(ModelFilename)));
                  SetModelName(s);

                  ModelComments:=DMMain.DecodeXMLText(Parser.CurAttr.Value('Comments'));

                  IDModel:=StrToInt(Parser.CurAttr.Value('IDModel'));
                  IDVersion:=StrToInt(Parser.CurAttr.Value('IDVersion'));
                  VersionStr:=Parser.CurAttr.Value('VersionStr');
                  UseVersionHistroy:=(Parser.CurAttr.Value('UseVersionHistroy')='1');
                  AutoIncVersion:=(Parser.CurAttr.Value('AutoIncVersion')='1');
                except
                end;

                //Set parent's form caption
                if(TForm(Parent).ClassNameIs('TEERForm'))then
                  TForm(Parent).Caption:=Copy(TForm(Parent).Caption, 1,
                    Pos('|', TForm(Parent).Caption))+
                    ' '+ModelName;

                try
                  DatabaseType:=Parser.CurAttr.Value('DatabaseType');
                  SetZoomFac(StrToFloat(Parser.CurAttr.Value('ZoomFac')));
                  if(MoveToSavedPosition)then
                  begin
                    TForm(parent).HorzScrollBar.Position:=StrToInt(Parser.CurAttr.Value('XPos'));
                    TForm(parent).VertScrollBar.Position:=StrToInt(Parser.CurAttr.Value('YPos'));
                  end;
                  DefaultDataType:=StrToInt(Parser.CurAttr.Value('DefaultDataType'));
                  DefaultTablePrefix:=StrToInt(Parser.CurAttr.Value('DefaultTablePrefix'));
                  DefSaveDBConn:=Parser.CurAttr.Value('DefSaveDBConn');
                  DefSyncDBConn:=Parser.CurAttr.Value('DefSyncDBConn');
                  DefQueryDBConn:=Parser.CurAttr.Value('DefQueryDBConn');
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
                      DMMain.GetTranslatedMessage('Values: DatabaseType, ZoomFac, MoveToSavedPosition, XPos, YPos, '+
                      'DefaultDataType, DefaultTablePrefix, DefSaveDBConn, DefSyncDBConn, DefQueryDBConn'+#13#10#13#10+
                      'Error: %s', 34, x.Message));
                  end;
                end;

                try
                  if(Parser.CurAttr.Value('Printer')<>'')then
                  begin
                    ModelPrinter:=Parser.CurAttr.Value('Printer');
                    HPageCount:=StrToFloat(Parser.CurAttr.Value('HPageCount'));
                    PageAspectRatio:=StrToFloat(Parser.CurAttr.Value('PageAspectRatio'));

                    if(Parser.CurAttr.Value('PageOrientation')='0')then
                      PageOrientation:=poPortrait
                    else
                      PageOrientation:=poLandscape;

                    PageFormat:=Parser.CurAttr.Value('PageFormat');

                    //Reselect selected pages
                    s:=Parser.CurAttr.Value('SelectedPages');
                    while(pos(';', s)>0)do
                    begin
                      SelectedPages[StrToInt(Copy(s, 1, pos(';', s)-1))]:=True;
                      s:=Copy(s, pos(';', s)+1, Length(s));
                    end;
                    if(s<>'')then
                      SelectedPages[StrToInt(s)]:=True;
                  end;
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
                      DMMain.GetTranslatedMessage('Values: Printer, HPageCount, PageAspectRatio, PageOrientation, '+
                      'PageFormat, SelectedPages'+#13#10#13#10+
                      'Error: %s', 35, x.Message));
                  end;
                end;


                try
                  UsePositionGrid:=(Parser.CurAttr.Value('UsePositionGrid')='1');
                  PositionGrid.X:=StrToInt(Parser.CurAttr.Value('PositionGridX'));
                  PositionGrid.Y:=StrToInt(Parser.CurAttr.Value('PositionGridY'));
                except
                  UsePositionGrid:=DMEER.UsePositionGrid;
                  PositionGrid.X:=DMEER.PositionGrid.X;
                  PositionGrid.Y:=DMEER.PositionGrid.Y;
                end;

                try
                  TableNameInRefs:=(Parser.CurAttr.Value('TableNameInRefs')='1');
                except
                  TableNameInRefs:=DMEER.TableNameInRefs;
                end;

                try
                  DefaultTableType:=StrToInt(Parser.CurAttr.Value('DefaultTableType'));
                  ActivateRefDefForNewRelations:=(Parser.CurAttr.Value('ActivateRefDefForNewRelations')='1');
                except
                  DefaultTableType:=DMEER.DefaultTableType;
                  ActivateRefDefForNewRelations:=DMEER.ActivateRefDefForNewRelations;
                end;

                try
                  FKPrefix:=DMMain.DecodeXMLText(Parser.CurAttr.Value('FKPrefix'));
                  FKPostfix:=DMMain.DecodeXMLText(Parser.CurAttr.Value('FKPostfix'));
                except
                  FKPrefix:='';
                  FKPostfix:='';
                end;

                try
                  CreateFKRefDefIndex:=(Parser.CurAttr.Value('CreateFKRefDefIndex')='1');
                except
                  CreateFKRefDefIndex:=False;
                end;

                try
                  DBQuoteCharacter:=DMMain.DecodeXMLText(Parser.CurAttr.Value('DBQuoteCharacter'));
                except
                  DBQuoteCharacter:='`';
                end;

                try
                  CreateSQLforLinkedObjects:=(Parser.CurAttr.Value('CreateSQLforLinkedObjects')='1');
                except
                  CreateSQLforLinkedObjects:=False;
                end;

                try
                  DefModelFont:=DMMain.DecodeXMLText(Parser.CurAttr.Value('DefModelFont'));
                  if(DefModelFont='')then
                    raise EInOutError.Create('');
                except
                  {$IFDEF MSWINDOWS}
                  DefModelFont:='Tahoma';
                  {$ELSE}
                  DefModelFont:='Nimbus Sans L';
                  {$ENDIF}
                end;

                try
                  EERModel_Width:=StrToInt(Parser.CurAttr.Value('CanvasWidth'));
                  EERModel_Height:=StrToInt(Parser.CurAttr.Value('CanvasHeight'));
                except
                  EERModel_Width:=4096;
                  EERModel_Height:=2842;
                end;
              end
              else if(Parser.CurName='DATATYPEGROUPS')and
                (ReadSettings)then
              begin
                try
                  DatatypeGroups.Clear;

                  GetDATATYPEGROUPS;
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
                      DMMain.GetTranslatedMessage('Values: DatatypeGroups '+#13#10#13#10+
                      'Error: %s', 36, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='DATATYPES')and
                (ReadSettings)then
              begin
                try
                  Datatypes.Clear;

                  GetDATATYPES;
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
                      DMMain.GetTranslatedMessage('Values: Datatypes'+#13#10#13#10+
                      'Error: %s', 37, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='COMMON_DATATYPES')and
                (ReadSettings)then
              begin
                try
                  CommonDataType.Clear;

                  GetCOMMONDATATYPES;
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
                      DMMain.GetTranslatedMessage('Values: CommonDataType'+#13#10#13#10+
                      'Error: %s', 38, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='TABLEPREFIXES')and
                (ReadSettings)then
              begin
                try
                  TablePrefix.Clear;

                  GetTABLEPREFIXES;
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
                      DMMain.GetTranslatedMessage('Values: TablePrefix'+#13#10#13#10+
                      'Error: %s', 39, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='REGIONCOLORS')and
                (ReadSettings)then
              begin
                try
                  RegionColors.Clear;

                  GetREGIONCOLORS;
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
                      DMMain.GetTranslatedMessage('Values: RegionColors'+#13#10#13#10+
                      'Error: %s', 40, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='POSITIONMARKERS')and
                (ReadSettings)then
              begin
                try
                  GetPOSITIONMARKERS;
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
                      DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10+
                      'Values: Position Markers'+#13#10#13#10+
                      'Error: %s', 41, x.Message));
                  end;
                end;
              end

              // -------------------------------------------------------
              // MetaData

              else if(Parser.CurName='REGIONS')then
              begin
                try
                  GetREGIONS(SelectObjs);
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Regions from XML File:'+#13#10#13#10+
                      'Error: %s', 42, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='TABLES')then
              begin
                try
                  GetTABLES(SelectObjs);
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Tables from XML File:'+#13#10#13#10+
                      'Error: %s', 43, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='RELATIONS')then
              begin
                try
                  GetRELATIONS(SelectObjs);
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Relations from XML File:'+#13#10#13#10+
                      'Error: %s', 44, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='NOTES')then
              begin
                try
                  GetNOTES(SelectObjs);
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Notes from XML File:'+#13#10#13#10+
                      'Error: ', 45, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='IMAGES')then
              begin
                try
                  GetIMAGES(SelectObjs);
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Images from XML File:'+#13#10#13#10+
                      'Error: %s', 46, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='PLUGINDATARECORDS')and
                (ReadSettings)then
              begin
                try
                  PluginData.Clear;

                  GetPLUGINDATARECORDS;
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Plugindata from XML File:'+#13#10#13#10+
                      'Error: %s', 47, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='QUERYRECORDS')and
                (ReadSettings)then
              begin
                try
                  StoredSQLCmds.Clear;

                  GetQUERYRECORDS;
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Querys from XML File:'+#13#10#13#10+
                      'Error: %s', 48, x.Message));
                  end;
                end;
              end
              else if(Parser.CurName='LINKEDMODELS')then
              begin
                try
                  LinkedModels.Clear;

                  GetLINKEDMODELS;
                except
                  on x: Exception do
                  begin
                    ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading the Linked Models from XML File:'+#13#10#13#10+
                      'Error: %s', 245, x.Message));
                  end;
                end;
              end;

            end;
            ptContent,
            ptCData: // Process Parser.CurContent field here
            begin
            end;
            ptEndTag: // Process End-Tag here (Parser.CurName)
            begin
            end;
            ptPI: // Process PI here (Parser.CurName is the target, Parser.CurContent)
            begin
            end;
          end;
        end;
      finally
        Parser.Free;
      end;
    finally
      DecimalSeparator:=DecSep;
    end;
    

    //----------------------------------------------------
    //Refresh tables and notes
    for i:=0 to ComponentCount-1 do
      if(Components[i].Classname='TEERTable')or
        (Components[i].Classname='TEERNote')then
      begin
        TEERObj(Components[i]).RefreshObj;
        TEERObj(Components[i]).DoPaint(self);
      end;

    //----------------------------------------------------
    //get max ID from model
    maxid:=0;

    //Check Objects
    for i:=0 to ComponentCount-1 do
      if(Components[i].ClassParent=TEERObj)then
      begin
        if(TEERObj(Components[i]).Obj_id>=maxid)then
          maxid:=TEERObj(Components[i]).Obj_id+1;

        if(Components[i].Classname='TEERTable')then
        begin
          //Check Columns
          theTbl:=TEERTable(Components[i]);
          for j:=0 to theTbl.Columns.count-1 do
            if(TEERColumn(theTbl.Columns[j]).Obj_id>=maxid)then
              maxid:=TEERColumn(theTbl.Columns[j]).Obj_id+1;

          //Check indices
          for j:=0 to theTbl.Indices.count-1 do
            if(TEERIndex(theTbl.Indices[j]).Obj_id>=maxid)then
              maxid:=TEERIndex(theTbl.Indices[j]).Obj_id+1;

          TEERTable(Components[i]).BringToFront;
        end;
      end;
    //Check PluginData
    for i:=0 to PluginData.Count-1 do
      if(TEERPluginData(PluginData[i]).Obj_id>=maxid)then
        maxid:=TEERPluginData(PluginData[i]).Obj_id+1;


    //Set GlobalID
    DMMain.SetGlobalID(maxid);

  finally
    DisableModelRefresh:=False;
  end;

  DMEER.RefreshPalettes;
  DMEER.UpdateStatusBar;

  if(Assigned(Application.MainForm))then
  begin
    if(AddDataToExistingModel)then
      ModelHasChanged;

    if(Application.MainForm.Enabled)then
      Application.MainForm.SetFocus;
  end;
end;

procedure TEERModel.LoadFromFile(fname: string; ReadSettings: Boolean = True; SelectObjs: Boolean = False; MoveToSavedPosition: Boolean = True; AddDataToExistingModel: Boolean = True);
{$IFDEF USE_IXMLDBMODELType}
var i, j, maxid: integer;
  theDoc: IXMLDBMODELType;
  theTbl, SourceTbl, DestTbl: TEERTable;
  theRel: TEERRel;
  theNote: TEERNote;
  theRegion: TEERRegion;
  theImage: TEERImage;
  theDatatypeGroup: TEERDatatypeGroup;
  theDatatype: TEERDatatype;
  thePluginData: TEERPluginData;
  theSQLCmd: TStoredSQLCmd;
  DecSep: char;
  s: string;
  theEERLinkedModel: TEERLinkedModel;
{$ENDIF}
begin
{$IFNDEF USE_IXMLDBMODELType}

  LoadFromFile2(fname, ReadSettings, SelectObjs, MoveToSavedPosition, AddDataToExistingModel);

{$ELSE}

  if(DMEER.UseNewXMLParser)then
  begin
    LoadFromFile2(fname, ReadSettings, SelectObjs, MoveToSavedPosition, AddDataToExistingModel);
    Exit;
  end;

  if(fname='')then
    Exit;

  DisableModelRefresh:=True;
  try
    theDoc:=LoadDBMODEL(fname);

    if(theDoc.Version<>'4.0')then
      if(MessageDlg(DMMain.GetTranslatedMessage('The selected DB-Model has the Version Number:'#13#10+'%s'+#13#10+
        'Only Version 4.0 DB-Models should be opened with this Version of DBDesigner.'+#13#10#13#10+
        'Do you want to try to open the file anyway?', 32,
        ExtractFileName(fname)+', Version '+theDoc.Version),
        mtInformation, [mbYes, mbNo], 0)=mrNo)then
        Exit;


    if(ReadSettings)then
    begin
      ModelFilename:=fname;

      DecSep:=DecimalSeparator;
      try
        DecimalSeparator:='.';

        //----------------------------------------------------
        //Read Settings

        try
          s:=DMMain.DecodeXMLText(theDoc.SETTINGS.GLOBALSETTINGS.ModelName);
          if(s='')then
            s:=Copy(ExtractFileName(ModelFilename),
              1, Length(ExtractFileName(ModelFilename))-Length(ExtractFileExt(ModelFilename)));
          SetModelName(s);

          ModelComments:=DMMain.DecodeXMLText(theDoc.SETTINGS.GLOBALSETTINGS.Comments);

          IDModel:=theDoc.SETTINGS.GLOBALSETTINGS.IDModel;
          IDVersion:=theDoc.SETTINGS.GLOBALSETTINGS.IDVersion;
          VersionStr:=theDoc.SETTINGS.GLOBALSETTINGS.VersionStr;
          UseVersionHistroy:=(theDoc.SETTINGS.GLOBALSETTINGS.UseVersionHistroy=1);
          AutoIncVersion:=(theDoc.SETTINGS.GLOBALSETTINGS.AutoIncVersion=1);
        except
        end;

        //Set parent's form caption
        if(TForm(Parent).ClassNameIs('TEERForm'))then
          TForm(Parent).Caption:=Copy(TForm(Parent).Caption, 1,
            Pos('|', TForm(Parent).Caption))+
            ' '+ModelName;

        try
          DatabaseType:=theDoc.SETTINGS.GLOBALSETTINGS.DatabaseType;
          SetZoomFac(StrToFloat(theDoc.SETTINGS.GLOBALSETTINGS.ZoomFac));
          if(MoveToSavedPosition)then
          begin
            TForm(parent).HorzScrollBar.Position:=theDoc.SETTINGS.GLOBALSETTINGS.XPos;
            TForm(parent).VertScrollBar.Position:=theDoc.SETTINGS.GLOBALSETTINGS.YPos;
          end;
          DefaultDataType:=theDoc.SETTINGS.GLOBALSETTINGS.DefaultDataType;
          DefaultTablePrefix:=theDoc.SETTINGS.GLOBALSETTINGS.DefaultTablePrefix;
          DefSaveDBConn:=theDoc.SETTINGS.GLOBALSETTINGS.DefSaveDBConn;
          DefSyncDBConn:=theDoc.SETTINGS.GLOBALSETTINGS.DefSyncDBConn;
          DefQueryDBConn:=theDoc.SETTINGS.GLOBALSETTINGS.DefQueryDBConn;
        except
          on x: Exception do
          begin
            ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
              DMMain.GetTranslatedMessage('Values: DatabaseType, ZoomFac, MoveToSavedPosition, XPos, YPos, '+
              'DefaultDataType, DefaultTablePrefix, DefSaveDBConn, DefSyncDBConn, DefQueryDBConn'+#13#10#13#10+
              'Error: %s', 34, x.Message));
          end;
        end;

        try
          if(theDoc.SETTINGS.GLOBALSETTINGS.Printer<>'')then
          begin
            ModelPrinter:=theDoc.SETTINGS.GLOBALSETTINGS.Printer;
            HPageCount:=StrToFloat(theDoc.SETTINGS.GLOBALSETTINGS.HPageCount);
            PageAspectRatio:=StrToFloat(theDoc.SETTINGS.GLOBALSETTINGS.PageAspectRatio);

            if(theDoc.SETTINGS.GLOBALSETTINGS.PageOrientation=0)then
              PageOrientation:=poPortrait
            else
              PageOrientation:=poLandscape;

            PageFormat:=theDoc.SETTINGS.GLOBALSETTINGS.PageFormat;

            //Reselect selected pages
            s:=theDoc.SETTINGS.GLOBALSETTINGS.SelectedPages;
            while(pos(';', s)>0)do
            begin
              SelectedPages[StrToInt(Copy(s, 1, pos(';', s)-1))]:=True;
              s:=Copy(s, pos(';', s)+1, Length(s));
            end;
            if(s<>'')then
              SelectedPages[StrToInt(s)]:=True;
          end;
        except
          on x: Exception do
          begin
            ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
              DMMain.GetTranslatedMessage('Values: Printer, HPageCount, PageAspectRatio, PageOrientation, '+
              'PageFormat, SelectedPages'+#13#10#13#10+
              'Error: %s', 35, x.Message));
          end;
        end;


        try
          UsePositionGrid:=(theDoc.SETTINGS.GLOBALSETTINGS.UsePositionGrid=1);
          PositionGrid.X:=theDoc.SETTINGS.GLOBALSETTINGS.PositionGridX;
          PositionGrid.Y:=theDoc.SETTINGS.GLOBALSETTINGS.PositionGridY;
        except
          UsePositionGrid:=DMEER.UsePositionGrid;
          PositionGrid.X:=DMEER.PositionGrid.X;
          PositionGrid.Y:=DMEER.PositionGrid.Y;
        end;

        try
          TableNameInRefs:=(theDoc.SETTINGS.GLOBALSETTINGS.TableNameInRefs=1);
        except
          TableNameInRefs:=DMEER.TableNameInRefs;
        end;

        try
          DefaultTableType:=theDoc.SETTINGS.GLOBALSETTINGS.DefaultTableType;
          ActivateRefDefForNewRelations:=(theDoc.SETTINGS.GLOBALSETTINGS.ActivateRefDefForNewRelations=1);
        except
          DefaultTableType:=DMEER.DefaultTableType;
          ActivateRefDefForNewRelations:=DMEER.ActivateRefDefForNewRelations;
        end;

        try
          FKPrefix:=DMMain.DecodeXMLText(theDoc.SETTINGS.GLOBALSETTINGS.FKPrefix);
          FKPostfix:=DMMain.DecodeXMLText(theDoc.SETTINGS.GLOBALSETTINGS.FKPostfix);
        except
          FKPrefix:='';
          FKPostfix:='';
        end;

        try
          CreateFKRefDefIndex:=(theDoc.SETTINGS.GLOBALSETTINGS.CreateFKRefDefIndex=1);
        except
          CreateFKRefDefIndex:=False;
        end;

        try
          DBQuoteCharacter:=DMMain.DecodeXMLText(theDoc.SETTINGS.GLOBALSETTINGS.DBQuoteCharacter);
        except
          DBQuoteCharacter:='`';
        end;

        try
          CreateSQLforLinkedObjects:=(theDoc.SETTINGS.GLOBALSETTINGS.CreateSQLforLinkedObjects=1);
        except
          CreateSQLforLinkedObjects:=False;
        end;

        try
          DefModelFont:=DMMain.DecodeXMLText(theDoc.SETTINGS.GLOBALSETTINGS.DefModelFont);
        except
          {$IFDEF MSWINDOWS}
          DefModelFont:='Tahoma';
          {$ELSE}
          DefModelFont:='Nimbus Sans L';
          {$ENDIF}
        end;

        try
          EERModel_Width:=theDoc.SETTINGS.GLOBALSETTINGS.CanvasWidth;
          EERModel_Height:=theDoc.SETTINGS.GLOBALSETTINGS.CanvasHeight;
        except
          EERModel_Width:=4096;
          EERModel_Height:=2842;
        end;
      finally
        DecimalSeparator:=DecSep;
      end;

      //----------------------------------------------------
      //Read Datatypes

      //Read DatatypeGroups
      try
        DatatypeGroups.Clear;
        for i:=0 to theDoc.SETTINGS.DATATYPEGROUPS.Count-1 do
        begin
          theDatatypeGroup:=TEERDatatypeGroup.Create(self);
          theDatatypeGroup.GroupName:=theDoc.SETTINGS.DATATYPEGROUPS.DATATYPEGROUP[i].Name;
          try
            theDatatypeGroup.IconNr:=theDoc.SETTINGS.DATATYPEGROUPS.DATATYPEGROUP[i].Icon;
          except
            theDatatypeGroup.IconNr:=i+1;
          end;

          DatatypeGroups.Add(theDatatypeGroup);
        end;
      except
        on x: Exception do
        begin
          ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
            DMMain.GetTranslatedMessage('Values: DatatypeGroups '+#13#10#13#10+
            'Error: %s', 36, x.Message));
        end;
      end;

      //Read Datatypes
      try
        Datatypes.Clear;
        for i:=0 to theDoc.SETTINGS.DATATYPES.Count-1 do
        begin
          //new(theDatatype);
          theDatatype:=TEERDatatype.Create(self);
          try
            with theDoc.SETTINGS.DATATYPES.DATATYPE[i] do
            begin
              theDatatype.id:=ID;
              theDatatype.group:=IDGroup;
              theDatatype.TypeName:=DMMain.DecodeXMLText(TypeName);
              theDatatype.description:=DMMain.DecodeXMLText(Description);
              theDatatype.ParamCount:=ParamCount;
              theDatatype.OptionCount:=OptionCount;
              theDatatype.ParamRequired:=(ParamRequired=1);
              theDatatype.SynonymGroup:=SynonymGroup;
              try
                theDatatype.EditParamsAsString:=(EditParamsAsString=1);
              except
                theDatatype.EditParamsAsString:=False;
              end;

              try
                theDatatype.PhysicalMapping:=(PhysicalMapping=1);
                theDatatype.PhysicalTypeName:=DMMain.DecodeXMLText(PhysicalTypeName);
              except
                theDatatype.PhysicalMapping:=False;
                theDatatype.PhysicalTypeName:='';
              end;

              for j:=0 to theDoc.SETTINGS.DATATYPES.DATATYPE[i].PARAMS.Count-1 do
                theDatatype.Param[j]:=DMMain.DecodeXMLText(theDoc.SETTINGS.DATATYPES.DATATYPE[i].PARAMS[j].Name);

              for j:=0 to theDoc.SETTINGS.DATATYPES.DATATYPE[i].OPTIONS.Count-1 do
              begin
                theDatatype.Options[j]:=DMMain.DecodeXMLText(theDoc.SETTINGS.DATATYPES.DATATYPE[i].OPTIONS[j].Name);
                theDatatype.OptionDefaults[j]:=(theDoc.SETTINGS.DATATYPES.DATATYPE[i].OPTIONS[j].Default=1);
              end;
            end;

            //Fix wrong ENUM/Set Params up to 4.0.2.94
            if(CompareText(theDatatype.TypeName, 'ENUM')=0)or(CompareText(theDatatype.TypeName, 'SET')=0)then
              if(theDatatype.ParamCount=3)then
                if(theDatatype.Param[0]='''value1''')then
                begin
                  theDatatype.ParamCount:=1;
                  theDatatype.Param[0]:='values';
                  theDatatype.EditParamsAsString:=True;
                end;

            Datatypes.Add(theDatatype);
          except
            theDataType.Free;
          end;
        end;
      except
        on x: Exception do
        begin
          ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
            DMMain.GetTranslatedMessage('Values: Datatypes'+#13#10#13#10+
            'Error: %s', 37, x.Message));
        end;
      end;

      //Read Common Datatypes
      try
        CommonDataType.Clear;
        for i:=0 to theDoc.SETTINGS.COMMON_DATATYPES.Count-1 do
        begin
          CommonDataType.Add(IntToStr(theDoc.SETTINGS.COMMON_DATATYPES.COMMON_DATATYPE[i].ID));
        end;
      except
        on x: Exception do
        begin
          ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
            DMMain.GetTranslatedMessage('Values: CommonDataType'+#13#10#13#10+
            'Error: %s', 38, x.Message));
        end;
      end;

      //Read Table Prefixes
      try
        TablePrefix.Clear;
        for i:=0 to theDoc.SETTINGS.TABLEPREFIXES.Count-1 do
        begin
          TablePrefix.Add(DMMain.DecodeXMLText(theDoc.SETTINGS.TABLEPREFIXES.TABLEPREFIX[i].Name));
        end;
      except
        on x: Exception do
        begin
          ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
            DMMain.GetTranslatedMessage('Values: TablePrefix'+#13#10#13#10+
            'Error: %s', 39, x.Message));
        end;
      end;

      //Read RegionColors
      try
        RegionColors.Clear;
        for i:=0 to theDoc.SETTINGS.REGIONCOLORS.Count-1 do
        begin
          RegionColors.Add(theDoc.SETTINGS.REGIONCOLORS.REGIONCOLOR[i].Color);
        end;
      except
        on x: Exception do
        begin
          ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
            DMMain.GetTranslatedMessage('Values: RegionColors'+#13#10#13#10+
            'Error: %s', 40, x.Message));
        end;
      end;

      //Read Position Markers
      try
        DecSep:=DecimalSeparator;
        try
          DecimalSeparator:='.';

          try
            for i:=0 to theDoc.SETTINGS.POSITIONMARKERS.Count-1 do
            begin
              if(i>PosMarkers.Count)then
                break;

              TPosMarker(PosMarkers[i]).ZoomFac:=StrToFloat(
                theDoc.SETTINGS.POSITIONMARKERS.POSITIONMARKER[i].ZoomFac);
              TPosMarker(PosMarkers[i]).X:=
                theDoc.SETTINGS.POSITIONMARKERS.POSITIONMARKER[i].X;
              TPosMarker(PosMarkers[i]).Y:=
                theDoc.SETTINGS.POSITIONMARKERS.POSITIONMARKER[i].Y;
            end;
          except
          end;
        finally
          DecimalSeparator:=DecSep;
        end;
      except
        on x: Exception do
        begin
          ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10, 33)+
            DMMain.GetTranslatedMessage('An Error occurred while reading Model Settings from XML File:'+#13#10+
            'Values: Position Markers'+#13#10#13#10+
            'Error: %s', 41, x.Message));
        end;
      end;
    end;

    //----------------------------------------------------
    //Read Metadata

    //if loaded data is not added, delete existing data
    if(Not(AddDataToExistingModel))then
    begin
      ModelIsBeingCleared:=True;
      try
        for i:=ComponentCount-1 downto 0 do
          if(Components[I].Classparent=TEERObj)then
            TEERObj(Components[I]).DeleteObj;
      finally
        ModelIsBeingCleared:=False;
      end;
    end;

    //Read Regions
    try
      for i:=0 to theDoc.METADATA.REGIONS.Count-1 do
      begin
        //Create Region
        theRegion:=NewRegion(theDoc.METADATA.REGIONS.REGION[i].XPos,
          theDoc.METADATA.REGIONS.REGION[i].YPos,
          theDoc.METADATA.REGIONS.REGION[i].Width,
          theDoc.METADATA.REGIONS.REGION[i].Height, False);

        //Read XML Data into Obj
        theRegion.SetXML(theDoc.METADATA.REGIONS.REGION[i]);

        theRegion.SetSelected(SelectObjs);
      end;
    except
      on x: Exception do
      begin
        ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Regions from XML File:'+#13#10#13#10+
          'Error: %s', 42, x.Message));
      end;
    end;


    //Read Tables
    try
      for i:=0 to theDoc.METADATA.TABLES.Count-1 do
      begin
        theTbl:=NewTable(theDoc.METADATA.TABLES.TABLE[i].XPos,
          theDoc.METADATA.TABLES.TABLE[i].YPos, False);

        //Read XML Data into Obj
        theTbl.SetXML(theDoc.METADATA.TABLES.TABLE[i]);

        theTbl.SetSelected(SelectObjs);

        theTbl.RefreshStrechedImg:=True;
      end;
    except
      on x: Exception do
      begin
        ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Tables from XML File:'+#13#10#13#10+
          'Error: %s', 43, x.Message));
      end;
    end;


    //Read Relations
    try
      for i:=0 to theDoc.METADATA.RELATIONS.Count-1 do
      begin
        //Get ID for debugging
        s:=IntToStr(theDoc.METADATA.RELATIONS.RELATION[i].ID);

        SourceTbl:=GetEERObjectByID(theDoc.METADATA.RELATIONS.RELATION[i].SrcTable);
        DestTbl:=GetEERObjectByID(theDoc.METADATA.RELATIONS.RELATION[i].DestTable);

        //Check if both tables exist
        if(SourceTbl<>nil)and(DestTbl<>nil)then
        begin
          theRel:=NewRelation(theDoc.METADATA.RELATIONS.RELATION[i].Kind,
            SourceTbl,
            DestTbl, False);

          //Read XML Data into Obj
          theRel.SetXML(theDoc.METADATA.RELATIONS.RELATION[i]);

          theRel.SetSelected(SelectObjs);
        end;
      end;

      CheckAllRelations;
    except
      on x: Exception do
      begin
        ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Relations from XML File:'+#13#10#13#10+
          'Error: %s', 44, x.Message+', ID: '+s));
      end;
    end;


    //Read Notes
    try
      for i:=0 to theDoc.METADATA.NOTES.Count-1 do
      begin
        theNote:=NewNote(theDoc.METADATA.NOTES.NOTE[i].XPos,
          theDoc.METADATA.NOTES.NOTE[i].YPos, False);

        theNote.SetSelected(SelectObjs);

        //Read XML Data into Obj
        theNote.SetXML(theDoc.METADATA.NOTES.NOTE[i]);
      end;
    except
      on x: Exception do
      begin
        ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Notes from XML File:'+#13#10#13#10+
          'Error: ', 45, x.Message));
      end;
    end;


    //Read Images
    try
      for i:=0 to theDoc.METADATA.IMAGES.Count-1 do
      begin
        theImage:=NewImage(theDoc.METADATA.IMAGES.IMAGE[i].XPos,
          theDoc.METADATA.IMAGES.IMAGE[i].YPos,
          theDoc.METADATA.IMAGES.IMAGE[i].Width,
          theDoc.METADATA.IMAGES.IMAGE[i].Height, False);

        theImage.SetSelected(SelectObjs);

        //Read XML Data into Obj
        theImage.SetXML(theDoc.METADATA.IMAGES.IMAGE[i]);
      end;
    except
      on x: Exception do
      begin
        ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Images from XML File:'+#13#10#13#10+
          'Error: %s', 46, x.Message));
      end;
    end;


    //----------------------------------------------------
    //Read Plugindata

    try
      //Delete old data if loaded data will not be appended
      if(Not(AddDataToExistingModel))then
      begin
        PluginData.Clear;

        //Load plugin data
        for i:=0 to theDoc.PLUGINDATA.PLUGINDATARECORDS.Count-1 do
        begin
          thePluginData:=AddPluginData(DMMain.DecodeXMLText(theDoc.PLUGINDATA.PLUGINDATARECORDS.PLUGINDATARECORD[i].PluginName),
            StrToInt(theDoc.PLUGINDATA.PLUGINDATARECORDS.PLUGINDATARECORD[i].Obj_id));

          for j:=0 to theDoc.PLUGINDATA.PLUGINDATARECORDS.PLUGINDATARECORD[i].PLUGINDATAPARAMS.Count-1 do
            thePluginData.Params.Add(DMMain.DecodeXMLText(theDoc.PLUGINDATA.PLUGINDATARECORDS.PLUGINDATARECORD[i].PLUGINDATAPARAMS.PLUGINDATAPARAM[j].Value));

          //DataValue ...
        end;
      end;
    except
      on x: Exception do
      begin
        ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Plugindata from XML File:'+#13#10#13#10+
          'Error: %s', 47, x.Message));
      end;
    end;

    //----------------------------------------------------
    //Read Querys

    try
      //Delete old data if loaded data will not be appended
      if(Not(AddDataToExistingModel))then
      begin
        StoredSQLCmds.Clear;

        //Load Querys
        for i:=0 to theDoc.QUERYDATA.QUERYRECORDS.Count-1 do
        begin
          //new(theSQLCmd);
          theSQLCmd:=TStoredSQLCmd.Create;
          theSQLCmd.SQLCmdType:=theDoc.QUERYDATA.QUERYRECORDS.QUERYRECORD[i].SQLCmdType;
          theSQLCmd.StoredPosition:=DMMain.DecodeXMLText(theDoc.QUERYDATA.QUERYRECORDS.QUERYRECORD[i].StoredPosition);
          theSQLCmd.SQLText:=DMMain.DecodeXMLText(theDoc.QUERYDATA.QUERYRECORDS.QUERYRECORD[i].SQLText);
          StoredSQLCmds.Add(theSQLCmd);
        end;
      end;
    except
      on x: Exception do
      begin
        ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Querys from XML File:'+#13#10#13#10+
          'Error: %s', 48, x.Message));
      end;
    end;

    //----------------------------------------------------
    //Read LinkedModels

    try
      LinkedModels.Clear;

      //Load LinkedModels
      for i:=0 to theDoc.LINKEDMODELS.Count-1 do
      begin
        theEERLinkedModel:=TEERLinkedModel.Create(self);
        theEERLinkedModel.IDLinkedModel:=theDoc.LINKEDMODELS.LINKEDMODEL[i].IDLinkedModel;
        theEERLinkedModel.ModelName:=DMMain.DecodeXMLText(theDoc.LINKEDMODELS.LINKEDMODEL[i].ModelName);
        theEERLinkedModel.IDModel:=theDoc.LINKEDMODELS.LINKEDMODEL[i].IDModel;
        theEERLinkedModel.IsStoredInDB:=(theDoc.LINKEDMODELS.LINKEDMODEL[i].IsStoredInDB=1);
        theEERLinkedModel.ModelFilename:=DMMain.DecodeXMLText(theDoc.LINKEDMODELS.LINKEDMODEL[i].ModelFilename);
        theEERLinkedModel.DriverName:=DMMain.DecodeXMLText(theDoc.LINKEDMODELS.LINKEDMODEL[i].DriverName);
        theEERLinkedModel.DBConnName:=DMMain.DecodeXMLText(theDoc.LINKEDMODELS.LINKEDMODEL[i].DBConnName);
        theEERLinkedModel.HostCaption:=DMMain.DecodeXMLText(theDoc.LINKEDMODELS.LINKEDMODEL[i].HostCaption);
        theEERLinkedModel.HostName:=DMMain.DecodeXMLText(theDoc.LINKEDMODELS.LINKEDMODEL[i].HostName);
        theEERLinkedModel.Database:=DMMain.DecodeXMLText(theDoc.LINKEDMODELS.LINKEDMODEL[i].Database);
        theEERLinkedModel.User:=DMMain.DecodeXMLText(theDoc.LINKEDMODELS.LINKEDMODEL[i].User);
        LinkedModels.Add(theEERLinkedModel);
      end;
    except
      on x: Exception do
      begin
        ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading the Linked Models from XML File:'+#13#10#13#10+
          'Error: %s', 245, x.Message));
      end;
    end;

    //----------------------------------------------------
    //Refresh tables and notes
    for i:=0 to ComponentCount-1 do
      if(Components[i].Classname='TEERTable')or
        (Components[i].Classname='TEERNote')then
      begin
        TEERObj(Components[i]).RefreshObj;
        TEERObj(Components[i]).DoPaint(self);
      end;

    //----------------------------------------------------
    //get max ID from model
    maxid:=0;

    //Check Objects
    for i:=0 to ComponentCount-1 do
      if(Components[i].ClassParent=TEERObj)then
      begin
        if(TEERObj(Components[i]).Obj_id>=maxid)then
          maxid:=TEERObj(Components[i]).Obj_id+1;

        if(Components[i].Classname='TEERTable')then
        begin
          //Check Columns
          theTbl:=TEERTable(Components[i]);
          for j:=0 to theTbl.Columns.count-1 do
            if(TEERColumn(theTbl.Columns[j]).Obj_id>=maxid)then
              maxid:=TEERColumn(theTbl.Columns[j]).Obj_id+1;

          //Check indices
          for j:=0 to theTbl.Indices.count-1 do
            if(TEERIndex(theTbl.Indices[j]).Obj_id>=maxid)then
              maxid:=TEERIndex(theTbl.Indices[j]).Obj_id+1;

          TEERTable(Components[i]).BringToFront;
        end;
      end;
    //Check PluginData
    for i:=0 to PluginData.Count-1 do
      if(TEERPluginData(PluginData[i]).Obj_id>=maxid)then
        maxid:=TEERPluginData(PluginData[i]).Obj_id+1;


    //Set GlobalID
    DMMain.SetGlobalID(maxid);

  finally
    DisableModelRefresh:=False;
  end;

  DMEER.RefreshPalettes;
  DMEER.UpdateStatusBar;

  if(Assigned(Application.MainForm))then
  begin
    if(AddDataToExistingModel)then
      ModelHasChanged;

    if(Application.MainForm.Enabled)then
      Application.MainForm.SetFocus;
  end;
{$ENDIF}
end;


procedure TEERModel.DeleteSelectedObjs(AddToLog: Boolean);
var i: integer;
begin
  if(AddToLog)then
  begin
    StartSubActionLog(at_DeleteObj);
    LogActions:=True;
  end;

  try
    //Delete all EER-Objects
    for i:=ComponentCount-1 downto 0 do
    begin
      if(Components[I].Classparent=TEERObj)then
        if(TEERObj(Components[I]).Selected)then
        begin
          TEERObj(Components[I]).DeleteObj;
        end;
    end;
  finally
    if(AddToLog)then
    begin
      LogActions:=False;
      EndSubAction;
    end;
  end;

  if(Not(DisableModelRefresh))then
  begin
    Refresh;

    DMEER.RefreshPalettes;
  end;
end;

function TEERModel.GetSelectedObjsCount: integer;
var i, SelCount: integer;
begin
  SelCount:=0;

  for i:=ComponentCount-1 downto 0 do
    if(Components[I].Classparent=TEERObj)then
      if(TEERObj(Components[I]).Selected)then
        inc(SelCount);

  GetSelectedObjsCount:=SelCount;
end;

function TEERModel.GetFirstSelectedObj: TObject;
var i: integer;
begin
  GetFirstSelectedObj:=nil;
  
  for i:=ComponentCount-1 downto 0 do
    if(Components[I].Classparent=TEERObj)then
      if(TEERObj(Components[I]).Selected)then
      begin
        GetFirstSelectedObj:=Components[I];
        break;
      end;
end;

procedure TEERModel.PaintModel(theCanvas: TCanvas; theZoomfac: double = -1;
  x: integer = 0; y: integer = 0; w: integer = 0; h: integer = 0;
  Objs2Paint: TEERObjectSet = [EERAllObjects];
  theDPI: integer = 72; doDrawText: Boolean = True);
var i, oldDPI: integer;
begin
  DMEER.DisablePaint:=True;
  if(Not(doDrawText))then
    DMEER.DisableTextOutput:=True;

  PaintingToSpecialCanvas:=True;

  oldDPI:=DPI;
  try
    DPI:=theDPI;

    theCanvas.Font.Name:=DefModelFont;

    //Paint Regions before all other objects
    for i:=ComponentCount-1 downto 0 do
    begin
      if((EERRegion in Objs2Paint)and(Components[I].ClassNameIs('TEERRegion')))or
        ((EERAllObjects in Objs2Paint)and(
          (Components[i] is TEERRegion)
        ))then
        TEERObj(Components[I]).PaintObj(theCanvas, theZoomfac,
          x, y, w, h);
    end;

    for i:=ComponentCount-1 downto 0 do
    begin
      //Draw all Objects except Relations
      if((EERNote in Objs2Paint)and(Components[I].ClassNameIs('TEERNote')))or
        ((EERTable in Objs2Paint)and(Components[I].ClassNameIs('TEERTable')))or
        ((EERImage in Objs2Paint)and(Components[I].ClassNameIs('TEERImage')))or
        ((EERAllObjects in Objs2Paint)and(
          (Components[i] is TEERTable)or
          (Components[i] is TEERImage)or
          (Components[i] is TEERNote)
        ))then
        TEERObj(Components[I]).PaintObj(theCanvas, theZoomfac,
          x, y, w, h);

      //Paint Relation and all Components of the Relation
      if((EERRelation in Objs2Paint)and(Components[I].ClassNameIs('TEERRel')))or
        ((EERAllObjects in Objs2Paint)and(Components[I].ClassNameIs('TEERRel')))then
      begin
        //RelStart
        TEERRel(Components[I]).PaintObj2(theCanvas, theZoomfac,
          x, y, w, h, 0);
        //RelEnd
        TEERRel(Components[I]).PaintObj2(theCanvas, theZoomfac,
          x, y, w, h, 2);
        //RelMiddle
        TEERRel(Components[I]).PaintObj2(theCanvas, theZoomfac,
          x, y, w, h, 1);

        //RelCaption
        if(DMEER.DisplayRelationNames)then
          TEERRel(Components[I]).PaintObj2(theCanvas, theZoomfac,
            x, y, w, h, 3);

        //RelStartInterval
        TEERRel(Components[I]).PaintObj2(theCanvas, theZoomfac,
          x, y, w, h, 4);
        //RelEndInterval
        TEERRel(Components[I]).PaintObj2(theCanvas, theZoomfac,
          x, y, w, h, 5);
      end;

    end;
  finally
    DMEER.DisablePaint:=False;
    DMEER.DisableTextOutput:=False;
    PaintingToSpecialCanvas:=False;
    DPI:=oldDPI;
  end;
end;

procedure TEERModel.PaintModelToImage(ModelBmp: TBitmap; PaintSelectedOnly: Boolean = False);
var i, x, y, w, h: integer;
  SelObjList: TList;
begin
  x:=Width;
  y:=Height;
  w:=0;
  h:=0;

  SelObjList:=TList.Create;
  try
    //Only Export model area
    for i:=0 to ComponentCount-1 do
      if(Components[i].ClassParent=TEERObj)then
        if(TEERObj(Components[i]).Selected)or
          (Not(PaintSelectedOnly))then
        begin
          if(TEERObj(Components[i]).Obj_X<x)then
            x:=TEERObj(Components[i]).Obj_X;
          if(TEERObj(Components[i]).Obj_Y<y)then
            y:=TEERObj(Components[i]).Obj_Y;
          if(TEERObj(Components[i]).Obj_X+TEERObj(Components[i]).Obj_W>w)then
            w:=TEERObj(Components[i]).Obj_X+TEERObj(Components[i]).Obj_W;
          if(TEERObj(Components[i]).Obj_Y+TEERObj(Components[i]).Obj_H>h)then
            h:=TEERObj(Components[i]).Obj_Y+TEERObj(Components[i]).Obj_H;

          //If the user exports the selected objects
          //deselect them, so no dottet line is drawn
          if(PaintSelectedOnly)then
          begin
            TEERObj(Components[i]).SetSelected(False);
            SelObjList.Add(Components[i]);
          end;
        end;

//    ModelBmp.Width:=10000;     LOOK HERE!
//    ModelBmp.Height:=10000;
//    ModelBmp.Canvas.Rectangle(Rect(0, 0, 1, 1));
//    ModelBmp.Canvas.Refresh;

    ModelBmp.Width:=w;
    ModelBmp.Height:=h;

    ModelBmp.Canvas.Pen.Color:=clWhite;
    ModelBmp.Canvas.Brush.Color:=clWhite;
    ModelBmp.Canvas.Rectangle(Rect(0, 0, ModelBmp.Width-1, ModelBmp.Height-1));

    if Assigned(ModelBmp.Canvas) then
    begin

      PaintModel(ModelBmp.Canvas,
        100,
        x, y, w, h, [EERTable, EERRegion, EERNote, EERRelation, EERImage]);

    end;

    //Select the objects again (if they have been
    //deselected)
    for i:=0 to SelObjList.Count-1 do
      TEERObj(SelObjList[i]).SetSelected(True);
  finally
    SelObjList.Free;
  end;
end;


procedure TEERModel.ModelHasChanged;
begin
  IsChanged:=True;
  Need2RefreshNavImg:=True;

  DMEER.RefreshSavedImg;
end;


procedure TEERModel.LogAction(ActionType, Obj_id: integer; Params: String);
begin
  StartSubActionLog(ActionType);
  LogSubAction(ActionType, Obj_id, Params);
  EndSubAction;
end;

procedure TEERModel.StartSubActionLog(ActionType: integer);
var theAction: TEERActionLog;
  i: integer;
begin
  //When the CurrentAction is not the last Action
  //delete all post actions
  if(CurrentAction<ActionLog.Count-1)then
  begin
    for i:=CurrentAction+1 to ActionLog.Count-1 do
      DeleteAction(ActionLog[ActionLog.Count-1]);
  end;

  //LimitUndoActions
  if(DMEER.LimitUndoActions)and(ActionLog.Count>=DMEER.UndoActionLimit)then
    ActionLog.Delete(0);


  //new(theAction);
  theAction:=TEERActionLog.Create(self);
  theAction.ActionType:=ActionType;
  theAction.ActionDate:=Now;
  theAction.Closed:=False;

  ActionLog.Add(theAction);
  CurrentAction:=ActionLog.Count-1;
end;

procedure TEERModel.LogSubAction(ActionSubType, Obj_id: integer; Params: String);
var theSubAction: TEERActionSubLog;
begin
  theSubAction:=TEERActionSubLog.Create(self);
  theSubAction.SubActionType:=ActionSubType;
  theSubAction.Obj_id:=Obj_id;
  theSubAction.Params.Text:=Params;

  TEERActionLog(ActionLog[CurrentAction]).SubActions.Add(theSubAction);
end;

function TEERModel.GetSubActionOfObj(ActionIndex, Obj_id: integer): Pointer;
var i: integer;
begin
  GetSubActionOfObj:=nil;

  for i:=0 to TEERActionLog(ActionLog[ActionIndex]).SubActions.Count-1 do
    if(TEERActionSubLog(TEERActionLog(ActionLog[ActionIndex]).SubActions[i]).Obj_id=
      Obj_id)then
    begin
      GetSubActionOfObj:=TEERActionLog(ActionLog[ActionIndex]).SubActions[i];
      break;
    end;
end;

procedure TEERModel.EndSubAction;
begin
  if(CurrentAction>=0)then
    TEERActionLog(ActionLog[CurrentAction]).Closed:=True;

  //The Model has been changed
  ModelHasChanged;
end;

procedure TEERModel.DeleteAction(theActionPointer: Pointer);
var theAction: TEERActionLog;
begin
  theAction:=theActionPointer;

  //Delete Action from List
  ActionLog.Delete(ActionLog.IndexOf(theAction));

  CurrentAction:=ActionLog.Count-1;
end;

procedure TEERModel.DeleteOpenAction;
begin
  //Delete Action if it is not closed
  if(CurrentAction>=0)then
    if(TEERActionLog(ActionLog[CurrentAction]).Closed=False)then
      DeleteAction(ActionLog[CurrentAction]);
end;

procedure TEERModel.UndoActions(TillAction: integer);
var i, j: integer;
  theAction: TEERActionLog;
  theSubAction: TEERActionSubLog;
  theObj: TEERObj;
  f: Textfile;
  s: string;
{$IFDEF USE_IXMLDBMODELType}
  theDoc: IXMLDBMODELType;
{$ENDIF}
  theXMLParser: TXmlParser;
begin
  //Rollback all actions including 'TillAction'
  for i:=CurrentAction downto TillAction do
  begin
    if(i<0)then
      continue;

    theAction:=TEERActionLog(ActionLog[i]);
    //Do for all subactions of the Action
    for j:=0 to theAction.SubActions.Count-1 do
    begin
      theSubAction:=TEERActionSubLog(theAction.SubActions[j]);
      //theObj:=nil;

      case theSubAction.SubActionType of
        sa_MoveFrom:
        begin
          theObj:=GetEERObjectByID(theSubAction.Obj_id);
          theObj.Obj_X:=StrToInt(theSubAction.Params.Values['Obj_X']);
          theObj.Obj_Y:=StrToInt(theSubAction.Params.Values['Obj_Y']);
          theObj.RefreshObj;
        end;

        at_NewObj:
        begin
          theObj:=GetEERObjectByID(theSubAction.Obj_id);

          if(theObj.ClassNameIs('TEERTable'))then
            dec(NewTableCounter);
          if(theObj.ClassNameIs('TEERRel'))then
            dec(NewRelCounter);
          if(theObj.ClassNameIs('TEERNote'))then
            dec(NewNoteCounter);
          if(theObj.ClassNameIs('TEERRegion'))then
            dec(NewRegionCounter);
          if(theObj.ClassNameIs('TEERImage'))then
            dec(NewImageCounter);

          theObj.DeleteObj;
          //theObj:=nil;
        end;

        at_DeleteObj:
        begin
          AssignFile(f, DMMain.SettingsPath+'undo.xml');
          try
            Rewrite(f);

            Write(f, theSubAction.Params.Text);

          finally
            CloseFile(f);
          end;

          //Load Clipboard, ignore Settings, Select
          LoadFromFile(DMMain.SettingsPath+'undo.xml',
            False, True);

          DeleteFile(DMMain.SettingsPath+'undo.xml');
        end;

        at_RenameObj:
        begin
          theObj:=GetEERObjectByID(theSubAction.Obj_id);
          theObj.ObjName:=theSubAction.Params.Values['OldObjName'];
          theObj.DoPaint(self);
        end;

        sa_ScaleFrom:
        begin
          theObj:=GetEERObjectByID(theSubAction.Obj_id);
          theObj.Obj_W:=StrToInt(theSubAction.Params.Values['Obj_W']);
          theObj.Obj_H:=StrToInt(theSubAction.Params.Values['Obj_H']);
          theObj.RefreshObj;
        end;

        at_EditObj:
        begin
          theObj:=GetEERObjectByID(theSubAction.Obj_id);

          AssignFile(f, DMMain.SettingsPath+'undo.xml');
          try
            Rewrite(f);

            s:=Copy(theSubAction.Params.Text, Length('BeforeEdit=')+1,
              Pos('AfterEdit=', theSubAction.Params.Text)-(Length('BeforeEdit=')+1));

            Write(f, s);

          finally
            CloseFile(f);
          end;

{$IFDEF USE_IXMLDBMODELType}
          if(Not(DMEER.UseNewXMLParser))then
          begin
            //Load XML
            theDoc:=LoadDBMODEL(DMMain.SettingsPath+'undo.xml');

            //Read Obj's Data from first (and only) obj
            if(theObj.ClassNameIs('TEERTable'))then
              TEERTable(theObj).SetXML(theDoc.METADATA.TABLES.TABLE[0]);
            if(theObj.ClassNameIs('TEERRegion'))then
              TEERRegion(theObj).SetXML(theDoc.METADATA.REGIONS.REGION[0]);
            if(theObj.ClassNameIs('TEERRel'))then
              TEERRel(theObj).SetXML(theDoc.METADATA.RELATIONS.RELATION[0]);
            if(theObj.ClassNameIs('TEERNote'))then
              TEERNote(theObj).SetXML(theDoc.METADATA.NOTES.NOTE[0]);
            if(theObj.ClassNameIs('TEERImage'))then
              TEERImage(theObj).SetXML(theDoc.METADATA.IMAGES.IMAGE[0]);
          end
          else
{$ENDIF}
          begin
            theXMLParser:=TXMLParser.Create;
            try
              theXMLParser.Normalize := False;

              theXMLParser.LoadFromFile(DMMain.SettingsPath+'undo.xml');

              theXMLParser.StartScan;
              while(theXMLParser.Scan)do
              begin
                if(theXMLParser.CurPartType=ptEmptyTag)then
                begin
                  if(theXMLParser.CurName='REGION')then
                  begin
                    if(theObj.ClassNameIs('TEERRegion'))then
                      TEERRegion(theObj).SetXML2(theXMLParser);
                  end
                  else if(theXMLParser.CurName='RELATION')then
                  begin
                    if(theObj.ClassNameIs('TEERRel'))then
                      TEERRel(theObj).SetXML2(theXMLParser);
                  end
                  else if(theXMLParser.CurName='NOTE')then
                  begin
                    if(theObj.ClassNameIs('TEERNote'))then
                      TEERNote(theObj).SetXML2(theXMLParser);
                  end
                  else if(theXMLParser.CurName='IMAGE')then
                  begin
                    if(theObj.ClassNameIs('TEERImage'))then
                      TEERImage(theObj).SetXML2(theXMLParser);
                  end
                end
                else if(theXMLParser.CurPartType=ptStartTag)then
                begin
                  if(theXMLParser.CurName='TABLE')then
                    if(theObj.ClassNameIs('TEERTable'))then
                      TEERTable(theObj).SetXML2(theXMLParser);
                end;
              end;

            finally
              theXMLParser.Free;
            end;
          end;

          DeleteFile(DMMain.SettingsPath+'undo.xml');

          Refresh;
        end;
      end;
    end;

    //After undo, check FKs
    {if(theObj<>nil)then
      if(theObj.ClassNameIs('TEERRel'))then}
    CheckAllRelations;

    if(Not(DisableModelRefresh))then
    begin
      DMEER.RefreshInfoPalette;
      DMEER.RefreshNavImg;
    end;
  end;

  CurrentAction:=TillAction-1;
end;

procedure TEERModel.RedoActions(TillAction: integer);
var i, j: integer;
  theAction: TEERActionLog;
  theSubAction: TEERActionSubLog;
  theObj: TEERObj;
  f: TextFile;
  s: string;
{$IFDEF USE_IXMLDBMODELType}
  theDoc: IXMLDBMODELType;
{$ENDIF}
  theXMLParser: TXmlParser;
begin
  //Rollback all actions including 'TillAction'
  for i:=CurrentAction+1 to TillAction do
  begin

    theAction:=TEERActionLog(ActionLog[i]);
    //Do for all subactions of the Action
    for j:=theAction.SubActions.Count-1 downto 0 do
    begin
      theSubAction:=TEERActionSubLog(theAction.SubActions[j]);

      case theSubAction.SubActionType of
        sa_MoveTo:
        begin
          theObj:=GetEERObjectByID(theSubAction.Obj_id);
          if(theObj<>nil)then
          begin
            theObj.Obj_X:=StrToInt(theSubAction.Params.Values['Obj_X']);
            theObj.Obj_Y:=StrToInt(theSubAction.Params.Values['Obj_Y']);
            theObj.RefreshObj;
          end;
        end;

        at_NewObj:
        begin
          if(theSubAction.Params.Values['ObjType']='EERTable')then
          begin
            theObj:=NewTable(
              StrToInt(theSubAction.Params.Values['Obj_X']),
              StrToInt(theSubAction.Params.Values['Obj_Y']),
              False);

            theObj.Obj_id:=theSubAction.Obj_id;
          end;

          if(theSubAction.Params.Values['ObjType']='EERRelation')then
          begin
            //do only if the tables are still there (only when a bug occured)
            if(GetEERObjectByID(StrToInt(theSubAction.Params.Values['SrcTable']))<>nil)and
              (GetEERObjectByID(StrToInt(theSubAction.Params.Values['DestTable']))<>nil)then
            begin
              theObj:=NewRelation(
                StrToInt(theSubAction.Params.Values['RelationKind']),
                GetEERObjectByID(StrToInt(theSubAction.Params.Values['SrcTable'])),
                GetEERObjectByID(StrToInt(theSubAction.Params.Values['DestTable'])),
                False);

              theObj.Obj_id:=theSubAction.Obj_id;
            end;
          end;

          if(theSubAction.Params.Values['ObjType']='EERNote')then
          begin
            theObj:=NewNote(
              StrToInt(theSubAction.Params.Values['Obj_X']),
              StrToInt(theSubAction.Params.Values['Obj_Y']),
              False);

            theObj.Obj_id:=theSubAction.Obj_id;
          end;

          if(theSubAction.Params.Values['ObjType']='EERRegion')then
          begin
            theObj:=NewRegion(
              StrToInt(theSubAction.Params.Values['Obj_X']),
              StrToInt(theSubAction.Params.Values['Obj_Y']),
              StrToInt(theSubAction.Params.Values['Obj_W']),
              StrToInt(theSubAction.Params.Values['Obj_H']),
              False);

            theObj.Obj_id:=theSubAction.Obj_id;
          end;

          if(theSubAction.Params.Values['ObjType']='EERImage')then
          begin
            theObj:=NewImage(
              StrToInt(theSubAction.Params.Values['Obj_X']),
              StrToInt(theSubAction.Params.Values['Obj_Y']),
              StrToInt(theSubAction.Params.Values['Obj_W']),
              StrToInt(theSubAction.Params.Values['Obj_H']),
              False);

            theObj.Obj_id:=theSubAction.Obj_id;
          end;
        end;

        at_DeleteObj:
        begin
          theObj:=GetEERObjectByID(theSubAction.Obj_id);
          if(theObj<>nil)then
            theObj.DeleteObj;
        end;

        at_RenameObj:
        begin
          theObj:=GetEERObjectByID(theSubAction.Obj_id);
          theObj.ObjName:=theSubAction.Params.Values['NewObjName'];
          theObj.DoPaint(self);
        end;

        sa_ScaleTo:
        begin
          theObj:=GetEERObjectByID(theSubAction.Obj_id);
          theObj.Obj_W:=StrToInt(theSubAction.Params.Values['Obj_W']);
          theObj.Obj_H:=StrToInt(theSubAction.Params.Values['Obj_H']);
          theObj.RefreshObj;
        end;

        at_EditObj:
        begin
          theObj:=GetEERObjectByID(theSubAction.Obj_id);

          AssignFile(f, DMMain.SettingsPath+'undo.xml');
          try
            Rewrite(f);

            s:=Copy(theSubAction.Params.Text,
              Pos('AfterEdit=', theSubAction.Params.Text)+
                Length('AfterEdit='),
              Length(theSubAction.Params.Text));

            Write(f, s);

          finally
            CloseFile(f);
          end;

{$IFDEF USE_IXMLDBMODELType}
          if(Not(DMEER.UseNewXMLParser))then
          begin
            //Load XML
            theDoc:=LoadDBMODEL(DMMain.SettingsPath+'undo.xml');

            //Read Obj's Data from first (and only) obj
            if(theObj.ClassNameIs('TEERTable'))then
              TEERTable(theObj).SetXML(theDoc.METADATA.TABLES.TABLE[0]);
            if(theObj.ClassNameIs('TEERRegion'))then
              TEERRegion(theObj).SetXML(theDoc.METADATA.REGIONS.REGION[0]);
            if(theObj.ClassNameIs('TEERRel'))then
              TEERRel(theObj).SetXML(theDoc.METADATA.RELATIONS.RELATION[0]);
            if(theObj.ClassNameIs('TEERNote'))then
              TEERNote(theObj).SetXML(theDoc.METADATA.NOTES.NOTE[0]);
            if(theObj.ClassNameIs('TEERImage'))then
              TEERImage(theObj).SetXML(theDoc.METADATA.IMAGES.IMAGE[0]);
          end
          else
{$ENDIF}
          begin
            theXMLParser:=TXMLParser.Create;
            try
              theXMLParser.Normalize := False;

              theXMLParser.LoadFromFile(DMMain.SettingsPath+'undo.xml');

              theXMLParser.StartScan;
              while(theXMLParser.Scan)do
              begin
                if(theXMLParser.CurPartType=ptEmptyTag)then
                begin
                  if(theXMLParser.CurName='REGION')then
                  begin
                    if(theObj.ClassNameIs('TEERRegion'))then
                      TEERRegion(theObj).SetXML2(theXMLParser);
                  end
                  else if(theXMLParser.CurName='RELATION')then
                  begin
                    if(theObj.ClassNameIs('TEERRel'))then
                      TEERRel(theObj).SetXML2(theXMLParser);
                  end
                  else if(theXMLParser.CurName='NOTE')then
                  begin
                    if(theObj.ClassNameIs('TEERNote'))then
                      TEERNote(theObj).SetXML2(theXMLParser);
                  end
                  else if(theXMLParser.CurName='IMAGE')then
                  begin
                    if(theObj.ClassNameIs('TEERImage'))then
                      TEERImage(theObj).SetXML2(theXMLParser);
                  end
                end
                else if(theXMLParser.CurPartType=ptStartTag)then
                begin
                  if(theXMLParser.CurName='TABLE')then
                    if(theObj.ClassNameIs('TEERTable'))then
                      TEERTable(theObj).SetXML2(theXMLParser);
                end;
              end;

            finally
              theXMLParser.Free;
            end;
          end;

          DeleteFile(DMMain.SettingsPath+'undo.xml');

          Refresh;
        end;

      end;
    end;

    CheckAllRelations;

    if(Not(DisableModelRefresh))then
    begin
      DMEER.RefreshInfoPalette;
      DMEER.RefreshNavImg;
    end;
  end;

  CurrentAction:=TillAction;
end;

function TEERModel.GetActionName(ActionType: integer): string;
begin
  case ActionType of
    at_MoveObj:
      GetActionName:=DMMain.GetTranslatedMessage('Move Object(s)', 49);
    at_NewObj:
      GetActionName:=DMMain.GetTranslatedMessage('New Object', 50);
    at_DeleteObj:
      GetActionName:=DMMain.GetTranslatedMessage('Delete Object(s)', 51);
    at_RenameObj:
      GetActionName:=DMMain.GetTranslatedMessage('Rename Object', 52);
    at_ScaleObj:
      GetActionName:=DMMain.GetTranslatedMessage('Scale Object(s)', 53);
    at_EditObj:
      GetActionName:=DMMain.GetTranslatedMessage('Edit Object', 54);
  end;
end;

procedure TEERModel.InitialMove4AllSelectedObjs(Obj: TObject);
var i: integer;
begin
  for i:=ComponentCount-1 downto 0 do
    if(Components[I].Classparent=TEERObj)then
      if(TEERObj(Components[I]).Selected)and(Components[I]<>Obj)then
      begin
        TEERObj(Components[I]).mouse_posx:=TEERObj(Components[I]).Left;
        TEERObj(Components[I]).mouse_posy:=TEERObj(Components[I]).Top;

        LogSubAction(sa_MoveFrom, TEERObj(Components[I]).Obj_id,
          'Obj_X='+IntToStr(TEERObj(Components[I]).Obj_X)+#13#10+'Obj_Y='+IntToStr(TEERObj(Components[I]).Obj_Y));
      end;
end;

procedure TEERModel.DrawRelIcon(theCanvas: TCanvas; theRect: TRect; theBmpNr: integer);
var x2, y2, p2: integer;
  //Border: double;
begin
  with theCanvas do
  begin
    x2:=(theRect.Right-theRect.Left) div 2;
    y2:=(theRect.Bottom-theRect.Top) div 2;

    if(theBmpNr<=5)then
    begin
      Pen.Color:=clBlack;
      Brush.Color:=clBlack;

      Polygon([Point(theRect.Left+x2, theRect.Top),
        Point(theRect.Left+x2*2, theRect.Top+y2),
        Point(theRect.Left+x2, theRect.Top+y2*2),
        Point(theRect.Left, theRect.Top+y2)]);
    end;

    if(theBmpNr<5)then
    begin
      p2:=EvalZoomFac(2);
      Brush.Color:=clWhite;
      Pen.Color:=clWhite;

      if(theBmpNr=0)then
        Polygon([Point(theRect.Left+x2, theRect.Top+p2),
          Point(theRect.Left+x2*2-p2, theRect.Top+y2),
          Point(theRect.Left+x2, theRect.Top+y2*2-p2),
          Point(theRect.Left+p2, theRect.Top+y2)]);

      if(theBmpNr=1)then
        Polygon([Point(theRect.Left+x2, theRect.Top+p2),
          Point(theRect.Left+x2, theRect.Top+y2*2-p2),
          Point(theRect.Left+p2, theRect.Top+y2)]);

      //Draw Middle Line Gray
      {begin
        Polygon([Point(theRect.Left+x2-1, theRect.Top+p2+1),
          Point(theRect.Left+x2-1, theRect.Top+y2*2-p2-1),
          Point(theRect.Left+p2, theRect.Top+y2)]);

        Pen.Color:=clGray;
        MoveTo(theRect.Left+x2, theRect.Top+p2);
        LineTo(theRect.Left+x2, theRect.Top+y2*2-p2);
        Pen.Color:=clWhite;
      end;}

      if(theBmpNr=2)then
        Polygon([Point(theRect.Left+x2, theRect.Top+p2),
          Point(theRect.Left+x2*2-p2, theRect.Top+y2),
          Point(theRect.Left+p2, theRect.Top+y2)]);

      if(theBmpNr=3)then
        Polygon([Point(theRect.Left+x2, theRect.Top+p2),
          Point(theRect.Left+x2*2-p2, theRect.Top+y2),
          Point(theRect.Left+x2, theRect.Top+y2*2-p2)]);

      if(theBmpNr=4)then
        Polygon([Point(theRect.Left+x2*2-p2, theRect.Top+y2),
          Point(theRect.Left+x2, theRect.Top+y2*2-p2),
          Point(theRect.Left+p2, theRect.Top+y2)]);

      Pen.Color:=clBlack;
    end;

    if(theBmpNr=6)then
    begin
      Pen.Color:=clBlack;
      Brush.Color:=clBlack;

      Ellipse(theRect);
    end;

    if(theBmpNr=7)then
    begin
      Pen.Color:=clBlack;
      Brush.Color:=clWhite;

      Ellipse(theRect.Left+x2 div 2+1, theRect.Top+y2 div 2+1,
        theRect.Right-x2 div 2, theRect.Bottom-y2 div 2);
    end;


    if(theBmpNr>=8)and(theBmpNr<12)then
    begin
      p2:=EvalZoomFac(3);
      Pen.Width:=p2;

      //Arrow left
      if(theBmpNr=8)then
      begin
        MoveTo(theRect.Left+x2, theRect.Top+p2);
        LineTo(theRect.Left+p2, theRect.Top+y2);
        LineTo(theRect.Left+x2, theRect.Top+y2*2-p2);
      end;

      //Arrow up
      if(theBmpNr=9)then
      begin
        MoveTo(theRect.Left+p2, theRect.Top+y2);
        LineTo(theRect.Left+x2, theRect.Top+p2);
        LineTo(theRect.Left+x2*2-p2, theRect.Top+y2);
      end;

      //Arrow right
      if(theBmpNr=10)then
      begin
        MoveTo(theRect.Left+x2, theRect.Top+p2);
        LineTo(theRect.Left+x2*2-p2, theRect.Top+y2);
        LineTo(theRect.Left+x2, theRect.Top+y2*2-p2);
      end;

      //Arrow down
      if(theBmpNr=11)then
      begin
        MoveTo(theRect.Left+p2, theRect.Top+y2);
        LineTo(theRect.Left+x2, theRect.Top+y2*2-p2);
        LineTo(theRect.Left+x2*2-p2, theRect.Top+y2);
      end;
    end
    else if(theBmpNr>=12)and(theBmpNr<20)then
    begin
      p2:=EvalZoomFac(2);
      Pen.Width:=p2;

      //Arrow left <
      if(theBmpNr=12)or(theBmpNr=16)then
      begin
        //Rectangle(theRect);
        MoveTo(theRect.Right, theRect.Top);
        LineTo(theRect.Right-x2, theRect.Top+y2);
        LineTo(theRect.Right, theRect.Top+y2);
        MoveTo(theRect.Right-x2, theRect.Top+y2-1);
        LineTo(theRect.Right, theRect.Bottom-1);

        if(theBmpNr=12)then
        begin
          MoveTo(theRect.Left+x2 div 2, theRect.Top+y2 div 2-1);
          LineTo(theRect.Left+x2 div 2, theRect.Bottom-y2 div 2);
        end
        else //Optional
        begin
          Pen.Width:=EvalZoomFac(1);
          Ellipse(theRect.Left, theRect.Top+y2 div 2+1,
            theRect.Left+x2, theRect.Bottom-y2 div 2);
          Pen.Width:=p2;
        end;
      end;

      //Arrow up ^
      if(theBmpNr=13)or(theBmpNr=17)then
      begin
        MoveTo(theRect.Left, theRect.Bottom);
        LineTo(theRect.Left+x2, theRect.Bottom-y2);
        LineTo(theRect.Left+x2, theRect.Bottom);
        MoveTo(theRect.Left+x2-1, theRect.Bottom-y2);
        LineTo(theRect.Left+x2*2-1, theRect.Bottom);

        if(theBmpNr=13)then
        begin
          MoveTo(theRect.Left+x2 div 2-1, theRect.Top+y2 div 2);
          LineTo(theRect.Right-x2 div 2, theRect.Top+y2 div 2);
        end
        else //Optional
        begin
          Pen.Width:=EvalZoomFac(1);
          Ellipse(theRect.Left+x2 div 2+1, theRect.Top,
            theRect.Right-x2 div 2, theRect.Top+y2);
          Pen.Width:=p2;
        end;
      end;

      //Arrow right >
      if(theBmpNr=14)or(theBmpNr=18)then
      begin
        MoveTo(theRect.Left, theRect.Top);
        LineTo(theRect.Left+x2, theRect.Top+y2);
        LineTo(theRect.Left, theRect.Top+y2);
        MoveTo(theRect.Left+x2, theRect.Top+y2-1);
        LineTo(theRect.Left, theRect.Top+y2*2-1);

        if(theBmpNr=14)then
        begin
          MoveTo(theRect.Right-x2 div 2, theRect.Top+y2 div 2-1);
          LineTo(theRect.Right-x2 div 2, theRect.Bottom-y2 div 2);
        end
        else //Optional
        begin
          Pen.Width:=EvalZoomFac(1);
          Ellipse(theRect.Right-x2, theRect.Top+y2 div 2+1,
            theRect.Right, theRect.Bottom-y2 div 2);
          Pen.Width:=p2;
        end;
      end;

      //Arrow down v
      if(theBmpNr=15)or(theBmpNr=19)then
      begin
        MoveTo(theRect.Left, theRect.Top);
        LineTo(theRect.Left+x2, theRect.Top+y2);
        LineTo(theRect.Left+x2, theRect.Top);
        MoveTo(theRect.Left+x2-1, theRect.Top+y2);
        LineTo(theRect.Left+x2*2-1, theRect.Top);

        if(theBmpNr=15)then
        begin
          MoveTo(theRect.Left+x2 div 2-1, theRect.Bottom-y2 div 2);
          LineTo(theRect.Right-x2 div 2, theRect.Bottom-y2 div 2);
        end
        else //Optional
        begin
          Pen.Width:=EvalZoomFac(1);
          Ellipse(theRect.Left+x2 div 2+1, theRect.Bottom-y2,
            theRect.Right-x2 div 2, theRect.Bottom);
          Pen.Width:=p2;
        end;
      end;
    end
    else if(theBmpNr>=20)and(theBmpNr<28)then
    begin
      p2:=EvalZoomFac(2);
      Pen.Width:=p2;

      //Line left |
      if(theBmpNr=22)or(theBmpNr=26)then
      begin
        //Rectangle(theRect);
        MoveTo(theRect.Left+x2+1, theRect.Top+y2 div 2-1);
        LineTo(theRect.Left+x2+1, theRect.Bottom-y2 div 2);

        if(theBmpNr=22)then
        begin
          MoveTo(theRect.Left+x2 div 2, theRect.Top+y2 div 2-1);
          LineTo(theRect.Left+x2 div 2, theRect.Bottom-y2 div 2);
        end
        else //Optional
        begin
          Pen.Width:=EvalZoomFac(1);
          Ellipse(theRect.Left, theRect.Top+y2 div 2+1,
            theRect.Left+x2, theRect.Bottom-y2 div 2);
          Pen.Width:=p2;
        end;
      end;

      //Line top -
      if(theBmpNr=23)or(theBmpNr=27)then
      begin
        MoveTo(theRect.Left+x2 div 2-1, theRect.Top+y2+1);
        LineTo(theRect.Right-x2 div 2, theRect.Top+y2+1);

        if(theBmpNr=23)then
        begin
          MoveTo(theRect.Left+x2 div 2-1, theRect.Top+y2 div 2);
          LineTo(theRect.Right-x2 div 2, theRect.Top+y2 div 2);
        end
        else //Optional
        begin
          Pen.Width:=EvalZoomFac(1);
          Ellipse(theRect.Left+x2 div 2+1, theRect.Top,
            theRect.Right-x2 div 2, theRect.Top+y2);
          Pen.Width:=p2;
        end;
      end;

      //Line right |
      if(theBmpNr=20)or(theBmpNr=24)then
      begin
        MoveTo(theRect.Right-x2-1, theRect.Top+y2 div 2-1);
        LineTo(theRect.Right-x2-1, theRect.Bottom-y2 div 2);

        if(theBmpNr=20)then
        begin
          MoveTo(theRect.Right-x2 div 2, theRect.Top+y2 div 2-1);
          LineTo(theRect.Right-x2 div 2, theRect.Bottom-y2 div 2);
        end
        else //Optional
        begin
          Pen.Width:=EvalZoomFac(1);
          Ellipse(theRect.Right-x2, theRect.Top+y2 div 2+1,
            theRect.Right, theRect.Bottom-y2 div 2);
          Pen.Width:=p2;
        end;
      end;

      //Line bottom _
      if(theBmpNr=21)or(theBmpNr=25)then
      begin
       // Rectangle(theRect);
        MoveTo(theRect.Left+x2 div 2-1, theRect.Bottom-y2-1);
        LineTo(theRect.Right-x2 div 2, theRect.Bottom-y2-1);

        if(theBmpNr=21)then
        begin
          MoveTo(theRect.Left+x2 div 2-1, theRect.Bottom-y2 div 2);
          LineTo(theRect.Right-x2 div 2, theRect.Bottom-y2 div 2);
        end
        else //Optional
        begin
          Pen.Width:=EvalZoomFac(1);
          Ellipse(theRect.Left+x2 div 2+1, theRect.Bottom-y2,
            theRect.Right-x2 div 2, theRect.Bottom);
          Pen.Width:=p2;
        end;
      end;


    end;

    Pen.Width:=1;
  end;
end;

procedure TEERModel.CreatePopupMenus(AOwner: TComponent);
var newItem: TMenuItem;
begin
  //Generate General Popup Menus
  PopupMenuEERTable:=GetGeneralObjPopupMenu(AOwner, 1);
  PopupMenuEERRelation:=GetGeneralObjPopupMenu(AOwner, 2);
  PopupMenuEERNote:=GetGeneralObjPopupMenu(AOwner, 3);
  PopupMenuEERImage:=GetGeneralObjPopupMenu(AOwner, 4);
  PopupMenuEERRegion:=GetGeneralObjPopupMenu(AOwner, 5);

  //Specify Menus

  //-------------------------------------------------
  //EERTable

  //MenuItem: Copy Table Name
  newItem:=TMenuItem.Create(PopupMenuEERTable);
  newItem.Name:='EditTableDataMI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Edit Table Data', 55);
  newItem.OnClick:=PopupMenuEditTable;
  newItem.ImageIndex:=14;
  PopupMenuEERTable.Items.Insert(2, newItem);

  {//MenuItem: Sep
  newItem:=TMenuItem.Create(PopupMenuEERTable);
  newItem.Name:='SepTable1MI';
  newItem.Caption:='-';
  PopupMenuEERTable.Items.Add(newItem);}

  //MenuItem: Copy Table Name
  newItem:=TMenuItem.Create(PopupMenuEERTable);
  newItem.Name:='CopyTableNameMI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Copy Table Name', 56);
  newItem.OnClick:=PopupMenuCopyTableName;
  newItem.ImageIndex:=4;
  PopupMenuEERTable.Items.Add(newItem);

  //MenuItem: Copy All Fieldnames
  newItem:=TMenuItem.Create(PopupMenuEERTable);
  newItem.Name:='CopyTableFieldsMI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Copy All Fieldnames', 57);
  newItem.OnClick:=PopupMenuCopyTableFields;
  newItem.ImageIndex:=4;
  PopupMenuEERTable.Items.Add(newItem);

  //MenuItem: Copy Selected Fieldname
  newItem:=TMenuItem.Create(PopupMenuEERTable);
  newItem.Name:='CopyTableFieldNameMI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Copy Selected Fieldname', 58);
  newItem.OnClick:=PopupMenuCopyTableFieldName;
  newItem.OnShow:=PopupMenuCopyTableFieldNameShow;
  newItem.ImageIndex:=4;
  PopupMenuEERTable.Items.Add(newItem);

  //MenuItem: Sep
  newItem:=TMenuItem.Create(PopupMenuEERTable);
  newItem.Name:='SepTable2MI';
  newItem.Caption:='-';
  PopupMenuEERTable.Items.Add(newItem);

  //MenuItem: Copy Table SQL Create
  newItem:=TMenuItem.Create(PopupMenuEERTable);
  newItem.Name:='CopyTableSQLCreateMI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Copy Table SQL Create', 59);
  newItem.OnClick:=PopupMenuCopyTableSQLCreate;
  newItem.ImageIndex:=5;
  PopupMenuEERTable.Items.Add(newItem);

  //MenuItem: Copy Table SQL Drop
  newItem:=TMenuItem.Create(PopupMenuEERTable);
  newItem.Name:='CopyTableSQLDropMI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Copy Table SQL Drop', 60);
  newItem.OnClick:=PopupMenuCopyTableSQLDrop;
  newItem.ImageIndex:=5;
  PopupMenuEERTable.Items.Add(newItem);

  //MenuItem: Copy Table SQL Insert
  newItem:=TMenuItem.Create(PopupMenuEERTable);
  newItem.Name:='CopyTableSQLInsertMI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Copy Table SQL Insert', 61);
  newItem.OnClick:=PopupMenuCopyTableSQLInsert;
  newItem.ImageIndex:=5;
  PopupMenuEERTable.Items.Add(newItem);

  //-------------------------------------------------
  //EERRegion

  PopupMenuEERRegion.Items.Delete(5);
  PopupMenuEERRegion.Items[0].OnClick:=PopupMenuSelectRegion;
end;

function TEERModel.GetGeneralObjPopupMenu(AOwner: TComponent; nr: integer): TPopupMenu;
var newItem: TMenuItem;
  alignItem: TMenuItem;
  ObjPopupMenu: TPopupMenu;
begin
  //Create PopupMenu, Owner will free it...
  ObjPopupMenu:=TPopupMenu.Create(AOwner);
  ObjPopupMenu.Images:=PopUpMenuImgs;

  //MenuItem: Select Obj
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='SelectObject'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Select Object', 62);
  newItem.OnClick:=PopupMenuSelectObj;
  newItem.ImageIndex:=0;
  ObjPopupMenu.Items.Add(newItem);

  //MenuItem: Edit Obj
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='EditObject'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Edit Object', 63);
  newItem.OnClick:=PopupMenuEditObj;
  newItem.ImageIndex:=1;
  ObjPopupMenu.Items.Add(newItem);

  //MenuItem: Refresh Obj
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='RefreshObject'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Refresh Object', 64);
  newItem.OnClick:=PopupMenuRefreshObj;
  newItem.ImageIndex:=2;
  ObjPopupMenu.Items.Add(newItem);

  //MenuItem: Delete Obj
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='DeleteObject'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Delete Object', 65);
  newItem.OnClick:=PopupMenuDeleteObj;
  newItem.ImageIndex:=3;
  ObjPopupMenu.Items.Add(newItem);

  //MenuItem: -
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='Sep'+IntToStr(nr)+'MI';
  newItem.Caption:='-';
  ObjPopupMenu.Items.Add(newItem);


  //MenuItem: Align
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='Align'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Align', 66);
  ObjPopupMenu.Items.Add(newItem);
  alignItem:=newItem;


  //MenuItem: Align Top
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='AlignTop'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Align Top', 67);
  newItem.Tag:=Ord(EERObjAlignTop);
  newItem.OnClick:=PopupMenuAlignObjs;
  newItem.ImageIndex:=6;
  alignItem.Add(newItem);

  //MenuItem: Align Right
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='AlignRight'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Align Right', 68);
  newItem.Tag:=Ord(EERObjAlignRight);
  newItem.OnClick:=PopupMenuAlignObjs;
  newItem.ImageIndex:=7;
  alignItem.Add(newItem);

  //MenuItem: Align Bottom
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='AlignBottom'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Align Bottom', 69);
  newItem.Tag:=Ord(EERObjAlignBottom);
  newItem.OnClick:=PopupMenuAlignObjs;
  newItem.ImageIndex:=8;
  alignItem.Add(newItem);

  //MenuItem: Align Left
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='AlignLeft'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Align Left', 70);
  newItem.Tag:=Ord(EERObjAlignLeft);
  newItem.OnClick:=PopupMenuAlignObjs;
  newItem.ImageIndex:=9;
  alignItem.Add(newItem);

  //MenuItem: -
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='SepA1'+IntToStr(nr)+'MI';
  newItem.Caption:='-';
  alignItem.Add(newItem);

  //MenuItem: Center Horizontal
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='CenterHorizontal'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Center Horizontal', 71);
  newItem.Tag:=Ord(EERObjAlignCenterH);
  newItem.OnClick:=PopupMenuAlignObjs;
  newItem.ImageIndex:=10;
  alignItem.Add(newItem);

  //MenuItem: Center Vertical
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='CenterVertical'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Center Vertical', 72);
  newItem.Tag:=Ord(EERObjAlignCenterV);
  newItem.OnClick:=PopupMenuAlignObjs;
  newItem.ImageIndex:=11;
  alignItem.Add(newItem);

  //MenuItem: -
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='SepA2'+IntToStr(nr)+'MI';
  newItem.Caption:='-';
  alignItem.Add(newItem);

  //MenuItem: Distribute Horizontal
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='DistributeHorizontal'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Distribute Horizontal', 73);
  newItem.Tag:=Ord(EERObjAlignDistributeH);
  newItem.OnClick:=PopupMenuAlignObjs;
  newItem.ImageIndex:=12;
  newItem.Enabled:=False;
  alignItem.Add(newItem);

  //MenuItem: Distribute Vertical
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='DistributeVertical'+IntToStr(nr)+'MI';
  newItem.Caption:=DMMain.GetTranslatedMessage('Distribute Vertical', 74);
  newItem.Tag:=Ord(EERObjAlignDistributeV);
  newItem.OnClick:=PopupMenuAlignObjs;
  newItem.ImageIndex:=13;
  newItem.Enabled:=False;
  alignItem.Add(newItem);

  //MenuItem: -
  newItem:=TMenuItem.Create(ObjPopupMenu);
  newItem.Name:='Sep2'+IntToStr(nr)+'MI';
  newItem.Caption:='-';
  ObjPopupMenu.Items.Add(newItem);

  GetGeneralObjPopupMenu:=ObjPopupMenu;
end;

function TEERModel.GetThePopupComponent(Sender: TObject): TComponent;
var s, s2: string;
  p: integer;
begin
  if(TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent.ClassParent=TEERObj)then
    GetThePopupComponent:=TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent
  else
  begin
    s:=TPopupMenu(TMenuItem(Sender).GetParentMenu).PopupComponent.Name;

    //get 'Rel_01' from 'Rel_01_Middle'
    p:=2;
    s2:=s;
    while(Pos('_', s2)>0)do
    begin
      p:=Pos('_', s2);
      s2[p]:='x';
    end;

    GetThePopupComponent:=FindComponent(Copy(s, 1, p-1));
  end;
end;

procedure TEERModel.PopupMenuSelectObj(Sender: TObject);
begin
  TEERObj(GetThePopupComponent(Sender)).SelectObj(Sender);
end;

procedure TEERModel.PopupMenuEditObj(Sender: TObject);
var oldMode: integer;
begin
  //If the obj is a Table, always open Table Editor
  if(GetThePopupComponent(Sender).ClassNameIs('TEERTable'))and
    (DMEER.WorkMode<>wmDesign)then
  begin
    oldMode:=DMEER.WorkMode;
    DMEER.WorkMode:=wmDesign;
    try
      TEERTable(GetThePopupComponent(Sender)).ShowEditor(Sender);
    finally
      DMEER.WorkMode:=oldMode;
    end;
  end
  else
    TEERObj(GetThePopupComponent(Sender)).ShowEditor(Sender);
end;

procedure TEERModel.PopupMenuRefreshObj(Sender: TObject);
begin
  TEERObj(GetThePopupComponent(Sender)).RefreshObj(Sender);
end;

procedure TEERModel.PopupMenuDeleteObj(Sender: TObject);
begin
  if(MessageDlg(DMMain.GetTranslatedMessage('Do you want to delete this Object?', 75),
    mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
  begin
    StartSubActionLog(at_DeleteObj);
    LogActions:=True;

    try
      TEERObj(GetThePopupComponent(Sender)).DeleteObj;
    finally
      LogActions:=False;
      EndSubAction;
    end;
  end;
end;

procedure TEERModel.PopupMenuAlignObjs(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    Ord(EERObjAlignLeft): AlignSelectedObjs(EERObjAlignLeft);
    Ord(EERObjAlignRight): AlignSelectedObjs(EERObjAlignRight);
    Ord(EERObjAlignTop): AlignSelectedObjs(EERObjAlignTop);
    Ord(EERObjAlignBottom): AlignSelectedObjs(EERObjAlignBottom);
    Ord(EERObjAlignCenterV): AlignSelectedObjs(EERObjAlignCenterV);
    Ord(EERObjAlignCenterH): AlignSelectedObjs(EERObjAlignCenterH);
    Ord(EERObjAlignDistributeV): AlignSelectedObjs(EERObjAlignDistributeV);
    Ord(EERObjAlignDistributeH): AlignSelectedObjs(EERObjAlignDistributeH);
  end;
end;

//-------------------------------------------------------------


procedure TEERModel.PopupMenuEditTable(Sender: TObject);
var oldMode: integer;
begin
  oldMode:=DMEER.WorkMode;
  DMEER.WorkMode:=wmQuery;
  try
    TEERTable(GetThePopupComponent(Sender)).ShowEditor(Sender);
  finally
    DMEER.WorkMode:=oldMode;
  end;
end;

procedure TEERModel.PopupMenuCopyTableName(Sender: TObject);
begin
  TEERTable(GetThePopupComponent(Sender)).CopyTableName(Sender);
end;

procedure TEERModel.PopupMenuCopyTableFieldName(Sender: TObject);
begin
  if(MouseOverObj<>nil)then
    if(MouseOverObj.ClassNameIs('TEERTable'))then
      if(MouseOverSubObj<>nil)then
        Clipboard.AsText:=TEERColumn(MouseOverSubObj).ColName;
end;

procedure TEERModel.PopupMenuCopyTableFieldNameShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=False;

  if(MouseOverObj<>nil)then
    if(MouseOverObj.ClassNameIs('TEERTable'))then
      if(MouseOverSubObj<>nil)and(MouseOverSubObj<>nil)then
        TMenuItem(Sender).Enabled:=True;
end;

procedure TEERModel.PopupMenuCopyTableFields(Sender: TObject);
begin
  TEERTable(GetThePopupComponent(Sender)).CopyTableFields(Sender);
end;

procedure TEERModel.PopupMenuCopyTableSQLCreate(Sender: TObject);
begin
  TEERTable(GetThePopupComponent(Sender)).CopyTableSQLCreate(Sender);
end;

procedure TEERModel.PopupMenuCopyTableSQLDrop(Sender: TObject);
begin
  TEERTable(GetThePopupComponent(Sender)).CopyTableSQLDrop(Sender);
end;

procedure TEERModel.PopupMenuCopyTableSQLInsert(Sender: TObject);
begin
  TEERTable(GetThePopupComponent(Sender)).CopyTableSQLInsert(Sender);
end;


procedure TEERModel.PopupMenuSelectRegion(Sender: TObject);
begin
  SendRegionsToBack;
  
  TEERRegion(GetThePopupComponent(Sender)).SelectAllObjsInRegion;
end;

procedure TEERModel.SetMouseOverObj(Obj: TObject; SubObj: Pointer);
begin
  if(DMEER.IsDragging)then
    Exit;

  if(MouseOverObj=Obj)then
    if(MouseOverSubObj=SubObj)then
      Exit;

  if(MouseOverObj<>nil)and(Not(PaintingToSpecialCanvas))then
    ClearMouseOverObj;

  MouseOverObj:=Obj;
  MouseOverSubObj:=SubObj;

  TEERObj(Obj).PaintMouseOver;
end;

procedure TEERModel.ClearMouseOverObj;
begin
  if(DMEER.IsDragging)then
    Exit;

  if(MouseOverObj<>nil)and(Not(PaintingToSpecialCanvas))then
  begin
    TEERObj(MouseOverObj).PaintMouseOverClear;

    {if(MouseOverObj.ClassNameIs('TEERTable'))then
      with TEERTable(MouseOverObj) do
      begin
        PaintObj2Canvas(Canvas, 0, 0);
      end;}

    MouseOverObj:=nil;
    MouseOverSubObj:=nil;
  end;
end;

function TEERModel.GetMouseOverObj: TObject;
begin
  GetMouseOverObj:=MouseOverObj;
end;

function TEERModel.GetMouseOverSubObj: Pointer;
begin
  GetMouseOverSubObj:=MouseOverSubObj;
end;

function SortTablesByRelCount(List: TStringList; Index1, Index2: Integer): Integer;
var anz1, anz2: integer;
begin
  anz1:=DMMain.GetSubStringCountInString(List.ValueFromIndex[Index1], ',');
  anz2:=DMMain.GetSubStringCountInString(List.ValueFromIndex[Index2], ',');
  if(anz1<anz2)then
    SortTablesByRelCount:=-1
  else if(anz1>anz2)then
    SortTablesByRelCount:=1
  else
    SortTablesByRelCount:=0;
end;

procedure TEERModel.SortEERTableListByForeignKeyReferences(EERTableList: TList);
var i, j, k: integer;
  s: string;
  TableOrder, ValitatedTables: TStringList;
  TableValid: Boolean;
begin
  if(Assigned(EERTableList))then
  begin
    TableOrder:=TStringList.Create;
    ValitatedTables:=TStringList.Create;
    try
      //Store the tables and relation in the stringlist
      // idtable=idrel, idrel, idrel, ...
      for i:=0 to EERTableList.Count-1 do
      begin
        s:=IntToStr(TEERTable(EERTableList[i]).Obj_id)+'=';

        for j:=0 to TEERTable(EERTableList[i]).RelEnd.Count-1 do
          if(TEERRel(TEERTable(EERTableList[i]).RelEnd[j]).SrcTbl<>
            TEERRel(TEERTable(EERTableList[i]).RelEnd[j]).DestTbl)then
            s:=s+IntToStr(TEERTable(TEERRel(TEERTable(EERTableList[i]).RelEnd[j]).SrcTbl).Obj_id)+', ';

        TableOrder.Add(s);
      end;

      //Sort the stringlist
      //put the tables with less relations first
      TableOrder.CustomSort(@SortTablesByRelCount);

      //First, add tables with no dest-relations
      i:=0;
      while(i<=TableOrder.Count-1)do
      begin
        if(DMMain.GetSubStringCountInString(TableOrder.ValueFromIndex[i], ',')=0)then
        begin
          ValitatedTables.Add(TableOrder.Names[i]);
          TableOrder.Delete(i);
        end
        else
          break;
      end;

      //if there is not at least one relation, raise error
      if(ValitatedTables.Count=0)then
        raise EInOutError.Create(DMMain.GetTranslatedMessage('There are circular relations.', 76));

      k:=0;
      while(TableOrder.Count>0)and
        (k<TableOrder.Count*EERTableList.Count*2)do
      begin
        i:=0;
        while(i<=TableOrder.Count-1)do
        begin
          TableValid:=True;

          //Check if all tables related to this table have already been created
          for j:=0 to DMMain.GetSubStringCountInString(TableOrder.ValueFromIndex[i], ',')-1 do
            if(ValitatedTables.IndexOf(DMMain.GetColumnFromSepString(TableOrder.ValueFromIndex[i], j, ',', ''))=-1)then
            begin
              TableValid:=False;
              break;
            end;

          if(TableValid)then
          begin
            ValitatedTables.Add(TableOrder.Names[i]);
            TableOrder.Delete(i);
          end
          else
            inc(i);
        end;

        inc(k);
      end;

      if(EERTableList.Count<>ValitatedTables.Count)then
      begin
        s:='';
        for i:=0 to TableOrder.Count-1 do
        begin
          s:=s+TEERTable(GetEERObjectByID(StrToInt(TableOrder.Names[i]))).ObjName+' -> ';

          for j:=0 to DMMain.GetSubStringCountInString(TableOrder.ValueFromIndex[i], ',')-1 do
            s:=s+TEERTable(GetEERObjectByID(StrToInt(DMMain.GetColumnFromSepString(TableOrder.ValueFromIndex[i], j, ',', '')))).ObjName+', ';

          s:=Copy(s, 1, Length(s)-2)+#13#10;
        end;

        raise EInOutError.Create(DMMain.GetTranslatedMessage('There are circular relations.'+#13#10#13#10+
          'Please check the following tables:'+#13#10+'%s', 77, s));
      end;

      //Reorder EERTableList
      EERTableList.Clear;
      for i:=0 to ValitatedTables.Count-1 do
        EERTableList.Add(GetEERObjectByID(StrToInt(ValitatedTables[i])));


    finally
      TableOrder.Free;
      ValitatedTables.Free;
    end;
  end;
end;


//PluginData functions

//Return the number of PluginData of the given Pluginname
//if no Pluginname is specified, return total count
function TEERModel.GetPluginDataCount(PluginName: string): integer;
var i, theCount: integer;
begin
  if(PluginName='')then
    GetPluginDataCount:=PluginData.Count
  else
  begin
    theCount:=0;
    for i:=0 to PluginData.Count-1 do
      if(CompareText(PluginName, TEERPluginData(PluginData[i]).PluginName)=0)then
        inc(theCount);

    GetPluginDataCount:=theCount;
  end;
end;

//Returns the PluginData at the specified index
//If no Pluginname is specified, the PluginData at the global index is returned
function TEERModel.GetPluginDataByIndex(PluginName: string; Index: integer): TEERPluginData;
var i, theCount: integer;
begin
  GetPluginDataByIndex:=nil;

  if(PluginName='')then
    if(Index<PluginData.Count)then
      GetPluginDataByIndex:=TEERPluginData(PluginData[Index])
  else
  begin
    theCount:=0;
    for i:=0 to PluginData.Count-1 do
      if(CompareText(PluginName, TEERPluginData(PluginData[i]).PluginName)=0)then
      begin
        if(Index=theCount)then
        begin
          GetPluginDataByIndex:=TEERPluginData(PluginData[i]);
          break;
        end;

        inc(theCount);
      end;
  end;
end;

function TEERModel.GetPluginDataByID(PluginName: string; ID: integer): TEERPluginData;
var i: integer;
begin
  GetPluginDataByID:=nil;

  for i:=0 to PluginData.Count-1 do
    if(CompareText(PluginName, TEERPluginData(PluginData[i]).PluginName)=0)or
      (PluginName='')then
    begin
      if(ID=TEERPluginData(PluginData[i]).Obj_id)then
      begin
        GetPluginDataByID:=TEERPluginData(PluginData[i]);
        break;
      end;
    end;
end;

function TEERModel.AddPluginData(PluginName: string; ID: integer): TEERPluginData;
var thePluginData: TEERPluginData;
begin
  thePluginData:=TEERPluginData.Create(self);
  thePluginData.PluginName:=PluginName;
  thePluginData.Obj_id:=ID;
  thePluginData.Data:=nil;

  PluginData.Add(thePluginData);

  AddPluginData:=thePluginData;
end;

function SortEERObjByNameFunc(Item1, Item2: Pointer): Integer;
begin
  SortEERObjByNameFunc:=CompareText(TEERObj(Item1).ObjName, TEERObj(Item2).ObjName);
end;

procedure TEERModel.SortEERObjectListByObjName(ObjectList: TList);
begin
  if(Assigned(ObjectList))then
    ObjectList.Sort(SortEERObjByNameFunc);
end;

function SortEERObjByOrderPosFunc(Item1, Item2: Pointer): Integer;
begin
  SortEERObjByOrderPosFunc:=TEERObj(Item1).OrderPos-TEERObj(Item2).OrderPos;
end;

procedure TEERModel.SortEERObjectListByOrderPos(ObjectList: TList);
begin
  if(Assigned(ObjectList))then
    ObjectList.Sort(SortEERObjByOrderPosFunc);
end;

function TEERModel.GetEERObjectClassName(ObjType: TEERObject): string;
begin
  case ObjType of
    EERNote:
      GetEERObjectClassName:='TEERNote';
    EERRegion:
      GetEERObjectClassName:='TEERRegion';
    EERRelation:
      GetEERObjectClassName:='TEERRel';
    EERTable:
      GetEERObjectClassName:='TEERTable';
    EERImage:
      GetEERObjectClassName:='TEERImage';
  else
    GetEERObjectClassName:='TNoEERObject';
  end;
end;

function TEERModel.GetEERObjectCount(ObjType: TEERObjectSet): integer;
var objanz, i: integer;
begin
  objanz:=0;
  for i:=0 to ComponentCount-1 do
    //if(Components[i].ClassNameIs(GetEERObjectClassName(ObjType)))then
    if((Components[i] is TEERTable)and(EERTable in ObjType))or
      ((Components[i] is TEERRel)and(EERRelation in ObjType))or
      ((Components[i] is TEERRegion)and(EERRegion in ObjType))or
      ((Components[i] is TEERImage)and(EERImage in ObjType))or
      ((Components[i] is TEERNote)and(EERNote in ObjType))or
      ((EERAllObjects in ObjType)and(
        (Components[i] is TEERTable)or
        (Components[i] is TEERRel)or
        (Components[i] is TEERRegion)or
        (Components[i] is TEERImage)or
        (Components[i] is TEERNote)
      ))then
      inc(objanz);

  GetEERObjectCount:=objanz;
end;

procedure TEERModel.GetEERObjectNameList(ObjType: TEERObjectSet; ObjectnamesList: TStringList; OnlySelected: Boolean=False);
var i: integer;
begin
  if(not(Assigned(ObjectnamesList)))then
    Exit;

  ObjectnamesList.Clear;

  for i:=0 to ComponentCount-1 do
    //if(Components[i].ClassNameIs(GetEERObjectClassName(ObjType)))then
    if(((Components[i] is TEERTable)and(EERTable in ObjType))or
      ((Components[i] is TEERRel)and(EERRelation in ObjType))or
      ((Components[i] is TEERRegion)and(EERRegion in ObjType))or
      ((Components[i] is TEERImage)and(EERImage in ObjType))or
      ((Components[i] is TEERNote)and(EERNote in ObjType))or
      ((EERAllObjects in ObjType)and(
        (Components[i] is TEERTable)or
        (Components[i] is TEERRel)or
        (Components[i] is TEERRegion)or
        (Components[i] is TEERImage)or
        (Components[i] is TEERNote)
      )))and
      ((Not(OnlySelected))or((OnlySelected)and(TEERObj(Components[i]).Selected)))then
        ObjectnamesList.Add(TEERObj(Components[i]).ObjName);

  ObjectnamesList.Sort;
end;

procedure TEERModel.GetEERObjectList(ObjType: TEERObjectSet; ObjectList: TList; OnlySelected: Boolean=False);
var i: integer;
begin
  if(not(Assigned(ObjectList)))then
    Exit;

  ObjectList.Clear;

  for i:=0 to ComponentCount-1 do
    //if(Components[i].ClassNameIs(GetEERObjectClassName(ObjType)))then
    if(((Components[i] is TEERTable)and(EERTable in ObjType))or
      ((Components[i] is TEERRel)and(EERRelation in ObjType))or
      ((Components[i] is TEERRegion)and(EERRegion in ObjType))or
      ((Components[i] is TEERImage)and(EERImage in ObjType))or
      ((Components[i] is TEERNote)and(EERNote in ObjType))or
      ((EERAllObjects in ObjType)and(
        (Components[i] is TEERTable)or
        (Components[i] is TEERRel)or
        (Components[i] is TEERRegion)or
        (Components[i] is TEERImage)or
        (Components[i] is TEERNote)
      )))and
      ((Not(OnlySelected))or((OnlySelected)and(TEERObj(Components[i]).Selected)))then
        ObjectList.Add(Components[i]);
end;


function TEERModel.GetEERObjectByID(id: integer): Pointer;
var i: integer;
begin
  GetEERObjectByID:=nil;

  //Rescale all EER-Objects
  for i:=ComponentCount-1 downto 0 do
  begin
    if(Components[i].ClassParent=TEERObj)then
      if(TEERObj(Components[i]).Obj_id=id)then
      begin
        GetEERObjectByID:=Components[i];
        Break;
      end;
  end;
end;

function TEERModel.GetEERObjectByLinkedID(id: integer): Pointer;
var i: integer;
begin
  GetEERObjectByLinkedID:=nil;

  //Rescale all EER-Objects
  for i:=ComponentCount-1 downto 0 do
  begin
    if(Components[i].ClassParent=TEERObj)then
      if(TEERObj(Components[i]).Obj_id_Linked=id)then
      begin
        GetEERObjectByLinkedID:=Components[i];
        Break;
      end;
  end;
end;

function TEERModel.GetEERObjectByIndex(ObjType: TEERObject; Index: integer): Pointer;
var i, theCount: integer;
begin
  GetEERObjectByIndex:=nil;

  theCount:=0;
  for i:=0 to ComponentCount-1 do
    if(Components[i].ClassNameIs(GetEERObjectClassName(ObjType)))then
    begin
      if(Index=theCount)then
      begin
        GetEERObjectByIndex:=Components[i];
        break;
      end;

      inc(theCount);
    end;
end;

function TEERModel.GetEERObjectByName(ObjType: TEERObject; Name: string): Pointer;
var i: integer;
begin
  GetEERObjectByName:=nil;

  for i:=0 to ComponentCount-1 do
    if(Components[i].ClassNameIs(GetEERObjectClassName(ObjType)))then
    begin
      if(CompareText(TEERObj(Components[i]).ObjName, Name)=0)then
      begin
        GetEERObjectByName:=Components[i];
        break;
      end;
    end;
end;

function TEERModel.GetEERTableByColumnID(id: integer): Pointer;
var i, j: integer;
  ResPointer: Pointer;
begin
  ResPointer:=nil;

  for i:=0 to ComponentCount-1 do
    if(Components[i].ClassNameIs('TEERTable'))then
    begin
      for j:=0 to TEERTable(Components[i]).Columns.Count-1 do
      begin
        if(id=TEERColumn(TEERTable(Components[i]).Columns[j]).Obj_id)then
        begin
          ResPointer:=Components[i];
          break;
        end;
      end;

      if(ResPointer<>nil)then
        break;
    end;

  GetEERTableByColumnID:=ResPointer;
end;

function TEERModel.GetEERIndexByID(id: integer): Pointer;
var i, j: integer;
  ResPointer: Pointer;
begin
  ResPointer:=nil;

  for i:=0 to ComponentCount-1 do
    if(Components[i].ClassNameIs('TEERTable'))then
    begin
      for j:=0 to TEERTable(Components[i]).Indices.Count-1 do
      begin
        if(id=TEERIndex(TEERTable(Components[i]).Indices[j]).Obj_id)then
        begin
          ResPointer:=TEERIndex(TEERTable(Components[i]).Indices[j]);
          break;
        end;
      end;

      if(ResPointer<>nil)then
        break;
    end;

  GetEERIndexByID:=ResPointer;
end;

procedure TEERModel.AlignSelectedObjs(alignPos: TEERObjectAlign);
var i, newpos, objcount: integer;
begin
  if(alignPos=EERObjAlignCenterH)or(alignPos=EERObjAlignCenterV)then
    newpos:=0
  else
    newpos:=-1;

  objcount:=0;


  StartSubActionLog(at_MoveObj);

  //Get most left obj
  for i:=ComponentCount-1 downto 0 do
  begin
    if(Components[I].Classparent=TEERObj)then
    begin
      if(TEERObj(Components[I]).Selected)then
      begin
        LogSubAction(sa_MoveFrom, TEERObj(Components[I]).Obj_id,
          'Obj_X='+IntToStr(TEERObj(Components[I]).Obj_X)+#13#10+'Obj_Y='+IntToStr(TEERObj(Components[I]).Obj_Y));

        //Top
        if(alignPos=EERObjAlignTop)then
          if(newpos=-1)or(TEERObj(Components[I]).Obj_Y<newpos)then
            newpos:=TEERObj(Components[I]).Obj_Y;

        //Right
        if(alignPos=EERObjAlignRight)then
          if(newpos=-1)or(TEERObj(Components[I]).Obj_X+TEERObj(Components[I]).Obj_W>newpos)then
            newpos:=TEERObj(Components[I]).Obj_X+TEERObj(Components[I]).Obj_W;

        //Bottom
        if(alignPos=EERObjAlignBottom)then
          if(newpos=-1)or(TEERObj(Components[I]).Obj_Y+TEERObj(Components[I]).Obj_H>newpos)then
            newpos:=TEERObj(Components[I]).Obj_Y+TEERObj(Components[I]).Obj_H;

        //Left
        if(alignPos=EERObjAlignLeft)then
          if(newpos=-1)or(TEERObj(Components[I]).Obj_X<newpos)then
            newpos:=TEERObj(Components[I]).Obj_X;

        //Center H
        if(alignPos=EERObjAlignCenterH)then
        begin
          newpos:=newpos+TEERObj(Components[I]).Obj_X+TEERObj(Components[I]).Obj_W;
          inc(objcount);
        end;

        //Center V
        if(alignPos=EERObjAlignCenterV)then
        begin
          newpos:=newpos+TEERObj(Components[I]).Obj_Y+TEERObj(Components[I]).Obj_H;
          inc(objcount);
        end;
      end;
    end;
  end;

  if(alignPos=EERObjAlignCenterH)or(alignPos=EERObjAlignCenterV)then
    newpos:=newpos div objcount;

  //Align objs
  for i:=ComponentCount-1 downto 0 do
  begin
    if(Components[I].Classparent=TEERObj)then
    begin
      if(TEERObj(Components[I]).Selected)then
      begin
        //Top
        if(alignPos=EERObjAlignTop)then
          if(TEERObj(Components[I]).Obj_Y<>newpos)then
          begin
            TEERObj(Components[I]).Obj_Y:=newpos;
            TEERObj(Components[I]).RefreshObj;
          end;

        //Bottom
        if(alignPos=EERObjAlignBottom)then
          if(TEERObj(Components[I]).Obj_Y+TEERObj(Components[I]).Obj_H<>newpos)then
          begin
            TEERObj(Components[I]).Obj_Y:=newpos-TEERObj(Components[I]).Obj_H;
            TEERObj(Components[I]).RefreshObj;
          end;

        //Left
        if(alignPos=EERObjAlignLeft)then
          if(TEERObj(Components[I]).Obj_X<>newpos)then
          begin
            TEERObj(Components[I]).Obj_X:=newpos;
            TEERObj(Components[I]).RefreshObj;
          end;

        //Right
        if(alignPos=EERObjAlignRight)then
          if(TEERObj(Components[I]).Obj_X+TEERObj(Components[I]).Obj_W<>newpos)then
          begin
            TEERObj(Components[I]).Obj_X:=newpos-TEERObj(Components[I]).Obj_W;
            TEERObj(Components[I]).RefreshObj;
          end;

        //Center H
        if(alignPos=EERObjAlignCenterH)then
        begin
          TEERObj(Components[I]).Obj_X:=newpos-TEERObj(Components[I]).Obj_W div 2;
          //Use PositionGrid if selected
          if(UsePositionGrid)then
            TEERObj(Components[I]).Obj_X:=(TEERObj(Components[I]).Obj_X div PositionGrid.X) * PositionGrid.X;

          TEERObj(Components[I]).RefreshObj;
        end;

        //Center V
        if(alignPos=EERObjAlignCenterV)then
        begin
          TEERObj(Components[I]).Obj_Y:=newpos-TEERObj(Components[I]).Obj_H div 2;
          //Use PositionGrid if selected
          if(UsePositionGrid)then
            TEERObj(Components[I]).Obj_Y:=(TEERObj(Components[I]).Obj_Y div PositionGrid.Y) * PositionGrid.Y;

          TEERObj(Components[I]).RefreshObj;
        end;

        LogSubAction(sa_MoveTo, TEERObj(Components[I]).Obj_id,
          'Obj_X='+IntToStr(TEERObj(Components[I]).Obj_X)+#13#10+'Obj_Y='+IntToStr(TEERObj(Components[I]).Obj_Y));
      end;
    end;
  end;

  EndSubAction;
end;


procedure TEERModel.SetModelName(name: string);
begin
  ModelName:=name;

  //Post QEventType_ModelNameChanged Event
  if(Assigned(Application.MainForm))then
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_ModelNameChanged, self));
end;

function TEERModel.GetModelName: string;
begin
  GetModelName:=ModelName;
end;


function TEERModel.GetStoredSQLCmdIndex(SQLCmdType: integer; StoredPosition: string): integer;
var i: integer;
begin
  GetStoredSQLCmdIndex:=-1;

  i:=0;
  while(i<StoredSQLCmds.Count)do
  begin
    if(TStoredSQLCmd(StoredSQLCmds[i]).SQLCmdType=SQLCmdType)and
      (CompareText(TStoredSQLCmd(StoredSQLCmds[i]).StoredPosition, StoredPosition)=0)then
    begin
      GetStoredSQLCmdIndex:=i;

      exit;
    end;

    inc(i);
  end;
end;

procedure TEERModel.MoveSelectedEERObjects(x, y: integer);
var i: integer;
begin
  for i:=0 to ComponentCount-1 do
    if(Components[i].ClassParent=TEERObj)then
      if(TEERObj(Components[i]).selected)then
      begin
        TEERObj(Components[i]).Obj_X:=
          TEERObj(Components[i]).Obj_X+x;
        TEERObj(Components[i]).Obj_Y:=
          TEERObj(Components[i]).Obj_Y+y;

        TEERObj(Components[i]).RefreshObj;
      end;


end;

function TEERModel.GetNextIDPlacedModel: integer;
var i: integer;
  id: integer;
begin
  id:=1;
  for i:=0 to LinkedModels.Count-1 do
    if(TEERLinkedModel(LinkedModels[i]).IDLinkedModel>=id)then
      id:=TEERLinkedModel(LinkedModels[i]).IDLinkedModel+1;

  GetNextIDPlacedModel:=id;
end;

function TEERModel.GetPlacedModelByID(id: integer): TEERLinkedModel;
var i: integer;
begin
  GetPlacedModelByID:=nil;

  for i:=0 to LinkedModels.Count-1 do
    if(TEERLinkedModel(LinkedModels[i]).IDLinkedModel=id)then
    begin
      GetPlacedModelByID:=TEERLinkedModel(LinkedModels[i]);
      break;
    end;
end;


// -----------------------------------------------
// Implementation of the EERModel - Objects
constructor TEERObj.Create(AOwner: TComponent);
begin
  inherited;

  //Assign ParentEERModel
  if(AOwner.ClassNameIs('TEERModel'))then
    ParentEERModel:=TEERModel(AOwner)
  else
    ParentEERModel:=nil;

  //Get ID from the Global ID Sequence
  Obj_id:=DMMain.GetNextGlobalID;

  // Assign MouseAction procedures
  OnMouseDown:=DoMouseDown;
  OnMouseMove:=DoMouseMove;
  OnMouseUp:=DoMouseUp;

  OnMouseEnter:=DoMouseEnter;
  OnMouseLeave:=DoMouseLeave;

  OnDblClick:=DoDblClick;
  // Assign Paint procedure
  OnPaint:=DoPaint;
  OnStartDrag:=DoOnStartDrag;
  OnEndDrag:=DoOnEndDrag;

  // Set default values
  IsSelected:=False;
  MouseIsDown:=False;
  EditorIsCalled:=False;

  ParentFont:=False;

  IsLinkedObject:=False;
  IDLinkedModel:=-1;
  Obj_id_Linked:=-1;

  if(ParentEERModel<>nil)then
    OrderPos:=ParentEERModel.GetEERObjectCount([EERAllObjects])+1
  else
    OrderPos:=-1;
end;

procedure TEERObj.DoDblClick(Sender: TObject);
begin
  MouseIsDown:=False;
  EditorIsCalled:=True;

  ShowEditor(Sender);
end;

procedure TEERObj.SelectObj(Sender: TObject);
begin
  SetSelected(True);

  DoPaint(self);
end;

function TEERObj.EvalZoomFac(thevalue: integer): integer;
begin
  EvalZoomFac:=ParentEERModel.EvalZoomFac(thevalue);
end;

function TEERObj.ReEvalZoomFac(thevalue: integer): integer;
begin
  ReEvalZoomFac:=ParentEERModel.ReEvalZoomFac(thevalue);
end;

procedure TEERObj.PaintObj(theCanvas: TCanvas; theZoomfac: double = -1;
  x: integer = 0; y: integer = 0; w: integer = 0; h: integer = 0);
var
  xo, yo: integer;
  prevZoomfac: double;
  prevRelIconSize: integer;
begin
  if((DMEER.DisablePaint)and(thezoomfac=-1))or
    (ParentEERModel.DisableModelRefresh)then
    Exit;

  xo:=0;
  yo:=0;

  //Set zoom factor to specified value
  prevZoomfac:=ParentEERModel.Zoomfac;
  prevRelIconSize:=ParentEERModel.RelIconSize;

  if(thezoomfac>-1)then
  begin
    ParentEERModel.Zoomfac:=thezoomfac;

    //Relation Icon Size
    if(DMEER.Notation=noErwin)then
      ParentEERModel.RelIconSize:=EvalZoomFac(9)
    else
      ParentEERModel.RelIconSize:=EvalZoomFac(18);
    ParentEERModel.RelIconDSize:=Trunc(ParentEERModel.RelIconSize/2);

    RefreshObj;
  end;

  try
    if(theCanvas<>Canvas)then
    begin
      {xo:=Left-x;
      yo:=Top-y;}
      
      xo:=EvalZoomFac(Obj_X)-x;
      yo:=EvalZoomFac(Obj_Y)-y;

      //Check if obj in Draw-Area
      if(w>0)and((xo+w<0)or(xo>w)or(yo+h<0)or(yo>h))then
        Exit;
    end;

    //Do the real drawing
    PaintObj2Canvas(theCanvas, xo, yo);

  finally
    if(thezoomfac>-1)then
    begin
      ParentEERModel.Zoomfac:=prevZoomfac;
      ParentEERModel.RelIconSize:=prevRelIconSize;
      ParentEERModel.RelIconDSize:=Trunc(ParentEERModel.RelIconSize/2);

      RefreshObj;
    end;
  end;
end;

procedure TEERObj.PaintMouseOver;
begin
  //This is overwritten if necessary
end;

procedure TEERObj.PaintMouseOverClear;
begin
  //This is overwritten if necessary
end;

procedure TEERObj.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(Button=mbLeft)then
  begin
    //Make shure that no TextImput has the focus
    if(Application.MainForm.ActiveControl<>nil)then
      Application.MainForm.ActiveControl:=nil;
  
    if(DMEER.CurrentWorkTool=wtPointer)then
      MouseIsDown:=True;

    mouse_absx:=Mouse.CursorPos.X;
    mouse_absy:=Mouse.CursorPos.Y;

    // If Worktool wtMove is the current tool, store obj
    if(DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign))then
    begin
      mouse_posx:=Left;
      mouse_posy:=Top;

      ParentEERModel.StartSubActionLog(at_MoveObj);
      ParentEERModel.LogSubAction(sa_MoveFrom, Obj_id,
        'Obj_X='+IntToStr(Obj_X)+#13#10+'Obj_Y='+IntToStr(Obj_Y));

      //Do for all selected Objects if this obj is selected
      if(Selected)then
        ParentEERModel.InitialMove4AllSelectedObjs(self);

      ObjChanged:=False;

      MouseIsDown:=True;
    end;

    // If Worktool Hand is the current tool, store scrollbar pos
    if(DMEER.CurrentWorkTool=wtHand)then
    begin
      if(Assigned(TForm(parent.parent).HorzScrollBar))then
        mouse_posx:=TForm(parent.parent).HorzScrollBar.Position
      else
        mouse_posx:=0;

      if(Assigned(TForm(parent.parent).VertScrollBar))then
        mouse_posy:=TForm(parent.parent).VertScrollBar.Position
      else
        mouse_posy:=0;

      MouseIsDown:=True;
    end;

    if(DMEER.CurrentWorkTool=wtZoomIn)then
      ParentEERModel.ZoomIn(Left+X, Top+Y);

    if(DMEER.CurrentWorkTool=wtZoomOut)then
      ParentEERModel.ZoomOut(Left+X, Top+Y);
  end;
end;

procedure TEERObj.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i: integer;
  theSubAction: TEERActionSubLog;
begin
  // If Worktool wtMove is the current tool, move obj
  if(ssLeft in Shift)and
    (MouseIsDown)and(Not(EditorIsCalled))and
    ((DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign)))and
    (Not(TEERModel(Parent).ReadOnly))then
  begin
    //When shift is not pressed, do normal Move
    if(Not(ssShift in Shift))then
    begin
      Obj_X:=ReEvalZoomFac(mouse_posx+Mouse.CursorPos.X-mouse_absx);
      Obj_Y:=ReEvalZoomFac(mouse_posy+Mouse.CursorPos.Y-mouse_absy);
    end
    else
    begin
      //Only move in on direction when user presses shift
      theSubAction:=TEERActionSubLog(ParentEERModel.GetSubActionOfObj(ParentEERModel.CurrentAction, Obj_id));
      if(theSubAction<>nil)then
      begin
        if(abs(Mouse.CursorPos.X-mouse_absx)>abs(Mouse.CursorPos.Y-mouse_absy))then
        begin
          Obj_X:=ReEvalZoomFac(mouse_posx+Mouse.CursorPos.X-mouse_absx);
          Obj_Y:=StrToInt(theSubAction.Params.Values['Obj_Y']);
        end
        else
        begin
          Obj_X:=StrToInt(theSubAction.Params.Values['Obj_X']);
          Obj_Y:=ReEvalZoomFac(mouse_posy+Mouse.CursorPos.Y-mouse_absy);
        end;
      end;
    end;

    //Use PositionGrid if selected
    if(ParentEERModel.UsePositionGrid)then
    begin
      Obj_X:=(Obj_X div ParentEERModel.PositionGrid.X) * ParentEERModel.PositionGrid.X;
      Obj_Y:=(Obj_Y div ParentEERModel.PositionGrid.Y) * ParentEERModel.PositionGrid.Y;
    end;

    RefreshObj;

    //Do for all selected Objects if this obj is selected
    if(Selected)then
      with ParentEERModel do
      begin
        for i:=ComponentCount-1 downto 0 do
          if(Components[I].Classparent=TEERObj)then
            if(TEERObj(Components[I]).Selected)and(Components[I]<>self)then
            begin
              //When shift is not pressed, do normal Move
              if(Not(ssShift in Shift))then
              begin
                TEERObj(Components[I]).Obj_X:=ReEvalZoomFac(TEERObj(Components[I]).mouse_posx+Mouse.CursorPos.X-self.mouse_absx);
                TEERObj(Components[I]).Obj_Y:=ReEvalZoomFac(TEERObj(Components[I]).mouse_posy+Mouse.CursorPos.Y-self.mouse_absy);
              end
              else
              begin
                //Only move in on direction when user presses shift
                theSubAction:=TEERActionSubLog(GetSubActionOfObj(CurrentAction, TEERObj(Components[I]).Obj_id));

                if(theSubAction<>nil)then
                begin
                  if(abs(Mouse.CursorPos.X-self.mouse_absx)>abs(Mouse.CursorPos.Y-self.mouse_absy))then
                  begin
                    TEERObj(Components[I]).Obj_X:=ReEvalZoomFac(TEERObj(Components[I]).mouse_posx+Mouse.CursorPos.X-self.mouse_absx);
                    TEERObj(Components[I]).Obj_Y:=StrToInt(theSubAction.Params.Values['Obj_Y']);
                  end
                  else
                  begin
                    TEERObj(Components[I]).Obj_X:=StrToInt(theSubAction.Params.Values['Obj_X']);
                    TEERObj(Components[I]).Obj_Y:=ReEvalZoomFac(TEERObj(Components[I]).mouse_posy+Mouse.CursorPos.Y-self.mouse_absy);
                  end;
                end;
              end;


              //Use PositionGrid if selected
              if(UsePositionGrid)then
              begin
                TEERObj(Components[I]).Obj_X:=(TEERObj(Components[I]).Obj_X div PositionGrid.X) * PositionGrid.X;
                TEERObj(Components[I]).Obj_Y:=(TEERObj(Components[I]).Obj_Y div PositionGrid.Y) * PositionGrid.Y;
              end;

              TEERObj(Components[I]).RefreshObj;
            end;
      end;

    ObjChanged:=True;
  end
  // If Worktool Hand is the current tool, scroll the area
  else if(Shift=[ssLeft])and(MouseIsDown)and(DMEER.CurrentWorkTool=wtHand)then
  begin
    if(TForm(parent.parent).HorzScrollBar<>nil)then
    begin
      TForm(parent.parent).HorzScrollBar.Position:=
        mouse_posx+(Mouse.CursorPos.X-mouse_absx)*-1;
{$IFDEF LINUX}
      if(TForm(parent.parent).HorzScrollBar.Position<0)then
        TForm(parent.parent).HorzScrollBar.Position:=0;
      if(TForm(parent.parent).HorzScrollBar.Position>TForm(parent.parent).HorzScrollBar.Range-TForm(parent.parent).ClientWidth)then
        TForm(parent.parent).HorzScrollBar.Position:=TForm(parent.parent).HorzScrollBar.Range-TForm(parent.parent).ClientWidth;
{$ENDIF}
    end;

    if(TForm(parent.parent).VertScrollBar<>nil)then
    begin
      TForm(parent.parent).VertScrollBar.Position:=
        mouse_posy+(Mouse.CursorPos.Y-mouse_absy)*-1;
{$IFDEF LINUX}
      if(TForm(parent.parent).VertScrollBar.Position<0)then
        TForm(parent.parent).VertScrollBar.Position:=0;
      if(TForm(parent.parent).VertScrollBar.Position>TForm(parent.parent).VertScrollBar.Range-TForm(parent.parent).ClientHeight)then
        TForm(parent.parent).VertScrollBar.Position:=TForm(parent.parent).VertScrollBar.Range-TForm(parent.parent).ClientHeight;
{$ENDIF}
    end;
  end;
end;

procedure TEERObj.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  if(Button=mbLeft)then
  begin
    MouseIsDown:=False;

    // If Worktool wtPointer is the current tool,
    // select or deselect obj
    if(DMEER.CurrentWorkTool=wtPointer)then
    begin
      if(not(Selected))then
      begin
        //if Ctrl or Shift is not pressed, only select this obj
        if(not(ssCtrl in Shift)and(not(ssShift in Shift)))then
          ParentEERModel.DeSelectAllObjs(nil);

        SetSelected(True);
      end
      else
        //if obj is selected, only deselect when Ctrl or Shift is pressed
        if((ssCtrl in Shift)or(ssShift in Shift))then
          SetSelected(False);

      DoPaint(self);
      
      if(ParentEERModel.GetMouseOverObj=self)then
        PaintMouseOver;
    end;

    if(DMEER.CurrentWorkTool=wtDelete)then
    begin
      QApplication_postEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_DeleteObject, self));
      Exit;
    end;

    if(ObjChanged=True)and
      ((DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign)))then
    begin
      //Log Move
      ParentEERModel.LogSubAction(sa_MoveTo, Obj_id,
        'Obj_X='+IntToStr(Obj_X)+#13#10+'Obj_Y='+IntToStr(Obj_Y));

      //Do for all selected Objects if this obj is selected
      if(Selected)then
        with ParentEERModel do
        begin
          for i:=ComponentCount-1 downto 0 do
            if(Components[I].Classparent=TEERObj)then
              if(TEERObj(Components[I]).Selected)and(Components[I]<>self)then
              begin
                LogSubAction(sa_MoveTo, TEERObj(Components[I]).Obj_id,
                  'Obj_X='+IntToStr(TEERObj(Components[I]).Obj_X)+#13#10+'Obj_Y='+IntToStr(TEERObj(Components[I]).Obj_Y));
              end;
        end;

      //Close Log
      ParentEERModel.EndSubAction;

      ObjChanged:=False;
    end
    //if the obj has not been moved, clear move action
    else if(DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign))then
      ParentEERModel.DeleteOpenAction;

    if(Not(ParentEERModel.DisableModelRefresh))then
      DMEER.RefreshInfoPalette;

  end;
end;

procedure TEERObj.DoMouseEnter(Sender: TObject);
begin
  // set Screen.Cursor
  //DMEER.SetWorkToolCurser(DMEER.CurrentWorkTool);
end;

procedure TEERObj.DoMouseLeave(Sender: TObject);
begin
  // restore the cursor
  //Screen.Cursor:=crDefault;
end;


function TEERObj.GetObjAsXMLModel: string;
var s: string;
begin
  s:='<?xml version="1.0" standalone="yes" ?>'+#13#10+
    '<DBMODEL Version="4.0">'+#13+#10+
    '<METADATA>'+#13+#10;

  if(Classname='TEERTable')then
    s:=s+'<TABLES>'
  else if(Classname='TEERRel')then
    s:=s+'<RELATIONS>'
  else if(Classname='TEERNote')then
    s:=s+'<NOTES>'
  else if(Classname='TEERImage')then
    s:=s+'<IMAGES>'
  else if(Classname='TEERRegion')then
    s:=s+'<REGIONS>';

  s:=s+GetXML;

  if(Classname='TEERTable')then
    s:=s+'</TABLES>'
  else if(Classname='TEERRel')then
    s:=s+'</RELATIONS>'
  else if(Classname='TEERNote')then
    s:=s+'</NOTES>'
  else if(Classname='TEERImage')then
    s:=s+'</IMAGES>'
  else if(Classname='TEERRegion')then
    s:=s+'</REGIONS>';

  s:=s+'</METADATA>'+#13#10+
    '</DBMODEL>';

  GetObjAsXMLModel:=s;
end;

procedure TEERObj.DoOnStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_StartEERObjectDrag, self));
end;

procedure TEERObj.DoOnEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_EndEERObjectDrag, self));

  MouseIsDown:=False;
end;

function TEERObj.GetRegion: Pointer;
var i: integer;
begin
  GetRegion:=nil;

  with ParentEERModel do
  begin
    for i:=ComponentCount-1 downto 0 do
    begin
      if(Components[I].ClassnameIs('TEERRegion'))then
      begin
        if(Obj_X>=TEERObj(Components[I]).Obj_X)and
          (Obj_Y>=TEERObj(Components[I]).Obj_Y)and
          (Obj_X+Obj_W<=TEERObj(Components[I]).Obj_X+TEERObj(Components[I]).Obj_W)and
          (Obj_Y+Obj_H<=TEERObj(Components[I]).Obj_Y+TEERObj(Components[I]).Obj_H)then
        begin
          GetRegion:=Components[I];
          break;
        end;
      end;
    end;
  end;
end;

procedure TEERObj.Assign(Source: TPersistent);
begin
  if Source is TEERObj then
  begin
    ParentEERModel:=TEERObj(Source).ParentEERModel;
    Obj_id:=TEERObj(Source).Obj_id;
    ObjName:=TEERObj(Source).ObjName;
    Obj_X:=TEERObj(Source).Obj_X;
    Obj_Y:=TEERObj(Source).Obj_Y;
    Obj_W:=TEERObj(Source).Obj_W;
    Obj_H:=TEERObj(Source).Obj_H;
    Comments:=TEERObj(Source).Comments;
    ObjChanged:=TEERObj(Source).ObjChanged;
    IsLinkedObject:=TEERObj(Source).IsLinkedObject;
    IDLinkedModel:=TEERObj(Source).IDLinkedModel;
  end
  else
    inherited Assign(Source);
end;

function TEERObj.ObjIsEqualTo(Source: TObject): Boolean;
begin
  ObjIsEqualTo:=False;

  if(Source is TEERObj)or
    (Source.ClassParent=TEERObj)then
  begin
    if(Obj_id=TEERObj(Source).Obj_id)and
      (ObjName=TEERObj(Source).ObjName)and
      (Obj_X=TEERObj(Source).Obj_X)and
      (Obj_Y=TEERObj(Source).Obj_Y)and
      (Obj_W=TEERObj(Source).Obj_W)and
      (Obj_H=TEERObj(Source).Obj_H)and
      (Comments=TEERObj(Source).Comments)and
      (ObjChanged=TEERObj(Source).ObjChanged)then
      ObjIsEqualTo:=True;
  end;

end;


function TEERObj.Selected: Boolean;
begin
  Selected:=IsSelected;
end;

procedure TEERObj.SetSelected(select: Boolean);
begin
  IsSelected:=select;
end;


// -----------------------------------------------
// Implementation of the EER-Table

constructor TEERTable.Create(AOwner: TComponent; TheName: string; FontName: string; theTableType: integer; theTablePrefix: integer; thePopupMenu: TPopupMenu);
var i: integer;
begin
  inherited Create(AOwner);


  Visible:=False;
  Parent:=TWidgetControl(AOwner);
  Name:=DMMain.GetValidObjectName(TheName);

  ObjName:=TheName;
  PrevTableName:='';

  Font.Name:=FontName;
  Canvas.Font.Name:=FontName;

  TableType:=theTableType;
  TablePrefix:=theTablePrefix;

  //Set nm Flag
  nmTable:=False;

  //Set initial Temporary Flag
  Temporary:=False;

  //Lists for Relations
  RelStart:=TList.Create;
  RelEnd:=TList.Create;

  //Create Relation list for each side
  for i:=1 to 4 do
    Rel[i]:=TList.Create;

  //List for columns
  Columns:=TObjectList.Create;

  //List for indices
  Indices:=TObjectList.Create;

  //Stringlist for standard Inserts
  StandardInserts:=TStringList.Create;

  //The table options
  TableOptions:=TStringList.Create;

  //Stores the TableImg
  RefreshStrechedImg:=False;
  StrechedImg:=TBitmap.Create;

  if(thePopupMenu<>nil)then
    PopupMenu:=thePopupMenu;

  OnDragOver:=DoDragOver;
  OnDragDrop:=DoDragDrop;

  OnClick:=DoClick;

  Collapsed:=False;
end;

destructor TEERTable.Destroy;
var i: integer;
begin
  RelStart.Free;
  RelEnd.Free;

  for i:=1 to 4 do
    Rel[i].Free;


  //Free Columns
  Columns.Clear;

  //Free Column List
  Columns.Free;

  //Free Index
  Indices.Clear;

  //Free Index List
  Indices.Free;

  StandardInserts.Free;

  TableOptions.Free;

  StrechedImg.Free;

  inherited;
end;

procedure TEERTable.DeleteObj;
var i: integer;
begin
  if(ParentEERModel.MouseOverObj=self)then
  begin
    ParentEERModel.MouseOverObj:=nil;
    ParentEERModel.MouseOverSubObj:=nil;
  end;

  //Log the Delete-Action if the Flag is set
  if(ParentEERModel.LogActions)then
    ParentEERModel.LogSubAction(at_DeleteObj, Obj_id, GetObjAsXMLModel);

  //Delete all relations to or from this table
  for i:=0 to RelStart.Count-1 do
    TEERRel(RelStart[0]).DeleteObj;

  for i:=0 to RelEnd.Count-1 do
    TEERRel(RelEnd[0]).DeleteObj;

  Free;
end;

procedure TEERTable.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var curCol: integer;
begin
  inherited DoMouseMove(Sender, Shift, X, Y);

  if(not(MouseIsDown))then
  begin
    if(y>=EvalZoomFac(20))then
      curCol:=Trunc(((y-20)/17+((100-ParentEERModel.ZoomFac)/70))/(ParentEERModel.ZoomFac/100))
    else
      curCol:=-1;

    //If mouse is over column, set SubObj
    if(curCol>=0)and(curCol<Columns.Count)then
    begin
      ParentEERModel.SetMouseOverObj(self, Columns[curCol]);
    end
    else
      //Set Table as MouseOverObj
      ParentEERModel.SetMouseOverObj(self, nil);
      //ParentEERModel.ClearMouseOverObj;
  end;

  if(MouseIsDown)then
  begin
    if(DMEER.WorkMode=wmQuery)and
      ((DMEER.CurrentWorkTool=wtPointer)or
      (DMEER.CurrentWorkTool=wtSQLSelect)or
      (DMEER.CurrentWorkTool=wtSQLFrom)or
      (DMEER.CurrentWorkTool=wtSQLWhere)or
      (DMEER.CurrentWorkTool=wtSQLGroup)or
      (DMEER.CurrentWorkTool=wtSQLHaving)or
      (DMEER.CurrentWorkTool=wtSQLOrder)or
      (DMEER.CurrentWorkTool=wtSQLSet))then
      BeginDrag(False, 3);
  end;
end;

procedure TEERTable.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var newNMtable: TEERTable;
  FKColumn: string;
begin
  inherited DoMouseDown(Sender, Button, Shift, X, Y);

  if(Button=mbLeft)or(Button=mbRight)then
  begin
    if(DMEER.CurrentWorkTool=wtRel1n)or
      (DMEER.CurrentWorkTool=wtRel1nSub)or
      (DMEER.CurrentWorkTool=wtRel11)or
      (DMEER.CurrentWorkTool=wtRelnm)or
      (DMEER.CurrentWorkTool=wtRel11Sub)or
      (DMEER.CurrentWorkTool=wtRel11NonId)then
    begin
      if(ParentEERModel.Rel_SrcTable=nil)then
      begin;
        ParentEERModel.Rel_SrcTable:=self;
        SetSelected(True);

        ParentEERModel.DeSelectAllObjs(self);
        DoPaint(self);
      end
      else if(ParentEERModel.Rel_SrcTable<>nil){and
        (ParentEERModel.Rel_SrcTable<>self)}then
      begin
        ParentEERModel.Rel_DestTable:=self;
        ParentEERModel.DeSelectAllObjs(nil);

        FKColumn:='';
        if(Button=mbRight)then
          if(ParentEERModel.GetMouseOverSubObj<>nil)then
            if(TObject(ParentEERModel.GetMouseOverSubObj) is TEERColumn)then
              FKColumn:=TEERColumn(ParentEERModel.GetMouseOverSubObj).ColName;


        case DMEER.CurrentWorkTool of
          wtRel1n:
            if(ParentEERModel.Rel_SrcTable<>ParentEERModel.Rel_DestTable)then
              ParentEERModel.NewRelation(rk_1n, ParentEERModel.Rel_SrcTable, ParentEERModel.Rel_DestTable, True, FKColumn)
            else
              MessageDlg(DMMain.GetTranslatedMessage('You cannot connect a table to itself using a identifying Relation.'+#13#10+
                'Use a non-identifying Relation instead.', 241), mtError, [mbOK], 0);
          wtRel1nSub:
            ParentEERModel.NewRelation(rk_1nNonId, ParentEERModel.Rel_SrcTable, ParentEERModel.Rel_DestTable, True, FKColumn);
          wtRel11:
            if(ParentEERModel.Rel_SrcTable<>ParentEERModel.Rel_DestTable)then
              ParentEERModel.NewRelation(rk_11, ParentEERModel.Rel_SrcTable, ParentEERModel.Rel_DestTable, True, FKColumn)
            else
              MessageDlg(DMMain.GetTranslatedMessage('You cannot connect a table to itself using a identifying Relation.'+#13#10+
                'Use a non-identifying Relation instead.', 241), mtError, [mbOK], 0);
          wtRel11NonId:
            ParentEERModel.NewRelation(rk_11NonId, ParentEERModel.Rel_SrcTable, ParentEERModel.Rel_DestTable, True, FKColumn);
          wtRelnm:
          begin
            //Create new n:m table
            newNMtable:=ParentEERModel.NewTable(
              (TEERTable(ParentEERModel.Rel_SrcTable).Obj_X+
              TEERTable(ParentEERModel.Rel_SrcTable).Obj_W div 2+
              TEERTable(ParentEERModel.Rel_DestTable).Obj_X+
              TEERTable(ParentEERModel.Rel_DestTable).Obj_W div 2) div 2,
              (TEERTable(ParentEERModel.Rel_SrcTable).Obj_Y+
              TEERTable(ParentEERModel.Rel_SrcTable).Obj_H div 2+
              TEERTable(ParentEERModel.Rel_DestTable).Obj_Y+
              TEERTable(ParentEERModel.Rel_DestTable).Obj_H div 2) div 2,
              True);
            newNMtable.ObjName:=
              TEERTable(ParentEERModel.Rel_SrcTable).ObjName+'_has_'+
              TEERTable(ParentEERModel.Rel_DestTable).ObjName;
            newNMtable.SetnmTableStatus(True);

            //Add two 1:n relations
            ParentEERModel.NewRelation(rk_1n, ParentEERModel.Rel_SrcTable, newNMtable, True);
            ParentEERModel.NewRelation(rk_1n, ParentEERModel.Rel_DestTable, newNMtable, True);

            newNMtable.RefreshObj;
            newNMtable.Obj_X:=newNMtable.Obj_X-newNMtable.Obj_W div 2;
            newNMtable.Obj_Y:=newNMtable.Obj_Y-newNMtable.Obj_H div 2;

            //Use PositionGrid if selected
            if(ParentEERModel.UsePositionGrid)then
            begin
              newNMtable.Obj_X:=(newNMtable.Obj_X div ParentEERModel.PositionGrid.X) * ParentEERModel.PositionGrid.X;
              newNMtable.Obj_Y:=(newNMtable.Obj_Y div ParentEERModel.PositionGrid.Y) * ParentEERModel.PositionGrid.Y;
            end;

            newNMtable.RefreshObj;
          end;
          wtRel11Sub:
            ParentEERModel.NewRelation(rk_11Sub, ParentEERModel.Rel_SrcTable, ParentEERModel.Rel_DestTable, True);
        end;

        //Reset temporary Tables
        ParentEERModel.Rel_SrcTable:=nil;
        ParentEERModel.Rel_DestTable:=nil;

        //Check ALL Foreign Keys / Indices
        ParentEERModel.CheckAllRelations;

        DMEER.SetWorkTool(wtPointer);
      end;
    end;

    if(DMEER.CurrentWorkTool=wtPointer)or
      (DMEER.CurrentWorkTool=wtSQLSelect)or
      (DMEER.CurrentWorkTool=wtSQLFrom)or
      (DMEER.CurrentWorkTool=wtSQLWhere)or
      (DMEER.CurrentWorkTool=wtSQLGroup)or
      (DMEER.CurrentWorkTool=wtSQLHaving)or
      (DMEER.CurrentWorkTool=wtSQLOrder)or
      (DMEER.CurrentWorkTool=wtSQLSet)then
        MouseIsDown:=True;
  end;
end;

procedure TEERTable.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if(X>width-3-EvalZoomFac(ParentEERModel.TblHeaderRightBmp.Width))and
    (Y<EvalZoomFac(18))then
  begin
    Collapsed:=Not(Collapsed);

    BringToFront;

    RefreshObj;
  end;

  //Region of table could have been changed
  sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RedrawTableList, nil));


  inherited DoMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TEERTable.DoClick(Sender: TObject);
begin
  if(ParentEERModel.MouseOverSubObj<>nil)then
    if(DMEER.CurrentWorkTool=wtSQLSelect)or
      (DMEER.CurrentWorkTool=wtSQLFrom)or
      (DMEER.CurrentWorkTool=wtSQLOn)or
      (DMEER.CurrentWorkTool=wtSQLWhere)or
      (DMEER.CurrentWorkTool=wtSQLGroup)or
      (DMEER.CurrentWorkTool=wtSQLHaving)or
      (DMEER.CurrentWorkTool=wtSQLOrder)or
      (DMEER.CurrentWorkTool=wtSQLSet)then
    begin
      sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_SelectSQLColumnFromTable, @DMEER.CurrentWorkTool));
    end;
end;

procedure TEERTable.ShowEditor(Sender: TObject);
begin
  //Show Table Editor
  if(DMEER.CurrentWorkTool=wtPointer)or
    (DMEER.CurrentWorkTool=wtMove)then
  begin
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_EditTable, self));

    //Process Mouse Messages before setting EditorIsCalled to false
    Application.ProcessMessages;

    EditorIsCalled:=False;
  end;
end;

procedure TEERTable.DoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept:=False;

  DoMouseMove(Sender, [], X, Y);

  if(ParentEERModel.GetMouseOverObj<>nil)then
  begin
    if(Source<>nil)then
    begin
      if(Source.Classname='TListView')then
      begin
        if(TListView(Source).Name='CommonDataTypesListView')then
          Accept:=True;
      end;
      if(Source.Classname='TTreeView')then
      begin
        if(TTreeView(Source).Name='AllDataTypesTV')then
          Accept:=True;
      end;
    end;
  end;
end;

procedure TEERTable.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
var i: integer;
  DropedDataType: TEERDatatype;
  theCol: TEERColumn;
begin
  DropedDataType:=nil;

  if(ParentEERModel.MouseOverObj<>nil)and
    (ParentEERModel.MouseOverSubObj<>nil)then
  begin
    theCol:=ParentEERModel.MouseOverSubObj;

    if(Source.Classname='TListView')then
    begin
      if(TListView(Source).Name='CommonDataTypesListView')then
      begin
        if(TListView(Source).Selected<>nil)then
          if(TListView(Source).Selected.Data<>nil)then
            DropedDataType:=TEERDatatype(TListView(Source).Selected.Data);
      end
    end;

    if(Source.Classname='TTreeView')then
    begin
      if(TTreeView(Source).Name='AllDataTypesTV')then
      begin
        if(TTreeView(Source).Selected<>nil)then
          if(TTreeView(Source).Selected.Data<>nil)then
            DropedDataType:=TEERDatatype(TTreeView(Source).Selected.Data);
      end;
    end;

    if(Assigned(DropedDataType))then
    begin
      //Assign Datatype to the Column
      theCol.idDatatype:=
        DropedDataType.id;

      //Clear DatatypeParams
      theCol.DatatypeParams:='';

      //Get Option Defaults
      for i:=0 to DropedDataType.OptionCount-1 do
        theCol.OptionSelected[i]:=
          DropedDataType.OptionDefaults[i];

      RefreshObj;

      DoPaint(self);

      {if(DropedDataType.ParamRequired)then
        EditDatatype;}
    end;
  end;
end;

procedure TEERTable.PaintCachedImg(theCanvas: TCanvas; xo: integer = 0; yo: integer = 0);
var PK: Boolean;
  i, j, ypos: integer;
  txt: string;
  width, height: integer;
  theCol: TEERColumn;
  SepLineDrawed: Boolean;
begin
  SepLineDrawed:=False;

  width:=EvalZoomFac(Obj_W);
  height:=EvalZoomFac(Obj_H);

  {StrechedImg.Width:=width;
  StrechedImg.Height:=height;

  StrechedImg.Canvas.Font:=Font;}

  with theCanvas do
  begin
{$IFDEF LINUX}
    //Brush.Color:=clWhite;
    //FillRect(Rect(xo+1, yo+1, xo+width-1, yo+height-1));
{$ENDIF}

    if(Not(DMEER.DisableTextOutput))then
    begin
      if(Not(IsLinkedObject))then
      begin
        //Draw Header
        StretchDraw(Rect(xo+1, yo+0, xo+width-3, yo+EvalZoomFac(18)),
          ParentEERModel.TblHeaderBmp);

        //Draw Header Right Part
        StretchDraw(
          Rect(xo+width-3-EvalZoomFac(ParentEERModel.TblHeaderRightBmp.Width),
            yo+0,
            xo+width-3,
            yo+EvalZoomFac(18)),
            ParentEERModel.TblHeaderRightBmp);
      end
      else
      begin
        //Draw Linked Header
        StretchDraw(Rect(xo+1, yo+0, xo+width-3, yo+EvalZoomFac(18)),
          ParentEERModel.TblHeaderLinkedBmp);

        //Draw Linked Header Right Part
        StretchDraw(
          Rect(xo+width-3-EvalZoomFac(ParentEERModel.TblHeaderRightBmp.Width),
            yo+0,
            xo+width-3,
            yo+EvalZoomFac(18)),
            ParentEERModel.TblHeaderRightLinkedBmp);
      end;

      //Collaps-Icon
      Brush.Color:=clBlack;
      Pen.Style:=psClear;

      if(Collapsed)then
        Polygon([
          Point(xo+width-3-EvalZoomFac(8), yo+EvalZoomFac(5)),
          Point(xo+width-3-EvalZoomFac(4), yo+EvalZoomFac(9)),
          Point(xo+width-3-EvalZoomFac(8), yo+EvalZoomFac(13))])
      else
        Polygon([
          Point(xo+width-3-EvalZoomFac(10), yo+EvalZoomFac(8)),
          Point(xo+width-3-EvalZoomFac(7), yo+EvalZoomFac(12)),
          Point(xo+width-3-EvalZoomFac(3), yo+EvalZoomFac(8))]);

      Pen.Style:=psSolid;
    end;

    //Paint Tbl
    Pen.Color:=clDark;
    Brush.Color:=clWhite;

    //FillBG
    FillRect(Rect(xo+1, yo+EvalZoomFac(18)+1, xo+width-3, yo+height-3));

    //Draw Table Name
    if(Not(DMEER.DisableTextOutput))then
    begin
      Font.Height:=ParentEERModel.GetFontHeight;
      TextOut(xo+EvalZoomFac(4), yo+EvalZoomFac(3), ObjName);
    end;

    // ---------------------------------------
    //Draw Columns

    PK:=True;
    Font.Color:=clBlack;

    ypos:=0;
    if(DMEER.DisplayMode=dmPrimaryKeyLevel)or
      (DMEER.DisplayMode=dmAttributeLevel)then
    begin
      for i:=0 to Columns.Count-1 do
      begin
        if(DMEER.DisplayMode=dmPrimaryKeyLevel)and
          (Not(TEERColumn(Columns[i]).PrimaryKey))then
          Continue;

        if(Not(DMEER.ShowForeignKeys))and
          (TEERColumn(Columns[i]).IsForeignKey)then
          continue;

        if(Collapsed)and(
          Not((TEERColumn(Columns[i]).PrimaryKey)or
          TEERColumn(Columns[i]).IsForeignKey))then
          continue;

        //Draw Field Icon (not when Textout is disabled)
        if(Not(DMEER.DisableTextOutput))then
        begin
          if(TEERColumn(Columns[i]).PrimaryKey)then
            StretchDraw(Rect(xo+1, yo+EvalZoomFac(20+17*ypos-1+1),
              xo+1+EvalZoomFac(16), yo+EvalZoomFac(36+17*ypos-1+1)),
              ParentEERModel.FieldKeyBmp)
          else if(TEERColumn(Columns[i]).IsForeignKey)then
            StretchDraw(Rect(xo+1, yo+EvalZoomFac(20+17*ypos-1+1),
              xo+1+EvalZoomFac(16), yo+EvalZoomFac(36+17*ypos-1+1)),
              ParentEERModel.Field_FKBmp)
          else
            StretchDraw(Rect(xo+1, yo+EvalZoomFac(20+17*ypos-1+1),
              xo+1+EvalZoomFac(16), yo+EvalZoomFac(36+17*ypos-1+1)),
              ParentEERModel.FieldBmp);
        end;

        //Draw Line after a Primary Key
        if(Not(DMEER.DisableTextOutput))then
          if(i>0)and(PK<>TEERColumn(Columns[i]).PrimaryKey)and
            (DMEER.DisplayMode=dmAttributeLevel)then
          begin
            if(IsLinkedObject)then
              Pen.Color:=$00C66931
            else
              Pen.Color:=clDark;
            MoveTo(xo+1, yo+EvalZoomFac(20+17*ypos-1));
            LineTo(xo+width-4, yo+EvalZoomFac(20+17*ypos-1));

            Pen.Color:=clDark;
          end;

        txt:=TEERColumn(Columns[i]).ColName;

        if(DMEER.DisplayPhysicalSchema)then
          txt:=txt+': '+ParentEERModel.GetDataTypeName(TEERColumn(Columns[i]).idDatatype)+
            TEERColumn(Columns[i]).DatatypeParams;

        if(Length(txt)>45)then
          txt:=Copy(txt, 1, 43)+'...';

        if(TEERColumn(Columns[i]).IsForeignKey)then
          txt:=txt+' (FK)';

        //Write Field Name
        if(Not(DMEER.DisableTextOutput))then
          TextOut(xo+EvalZoomFac(4+16), yo+EvalZoomFac(20+17*ypos), txt);

        //Store PK info for comparison with next column
        PK:=TEERColumn(Columns[i]).PrimaryKey;

        inc(ypos);
      end;
    end;

    if(DMEER.DisplayTableIndices)and(Not(Collapsed))then
    begin
      for i:=0 to Indices.Count-1 do
        if(CompareText(TEERIndex(Indices[i]).IndexName, 'PRIMARY')<>0)then
        begin
          //Sep Line
          if(Not(SepLineDrawed))then
          begin
            if(IsLinkedObject)then
              Pen.Color:=$00C66931
            else
              Pen.Color:=clDark;
            MoveTo(xo+1, yo+EvalZoomFac(20+17*ypos-1));
            LineTo(xo+width-4, yo+EvalZoomFac(20+17*ypos-1));

            Pen.Color:=clDark;

            SepLineDrawed:=True;
          end;

          // (not when Textout is disabled)
          if(Not(DMEER.DisableTextOutput))then
          begin
            if(TEERIndex(Indices[i]).FKRefDef_Obj_id>-1)then
              StretchDraw(Rect(xo+1, yo+EvalZoomFac(20+17*ypos),
                xo+1+EvalZoomFac(16), yo+EvalZoomFac(36+17*ypos)),
                ParentEERModel.Index_FKBmp)
            else
              StretchDraw(Rect(xo+1, yo+EvalZoomFac(20+17*ypos),
                xo+1+EvalZoomFac(16), yo+EvalZoomFac(36+17*ypos)),
                ParentEERModel.IndexBmp);
          end;

          //IndexName
          Font.Style:=[fsItalic];
          Font.Color:=$00222222;
          if(Not(DMEER.DisableTextOutput))then
            TextOut(xo+EvalZoomFac(4+16), yo+EvalZoomFac(20+17*ypos), TEERIndex(Indices[i]).IndexName);
          Font.Style:=[];
          inc(ypos);

          //All Columns of the Index
          Font.Color:=$00555555;
          for j:=0 to TEERIndex(Indices[i]).Columns.Count-1 do
          begin
            theCol:=TEERColumn(GetColumnByID(StrToInt(TEERIndex(Indices[i]).Columns[j])));

            //Draw Field Icon
            if(theCol<>nil)and
              (Not(DMEER.DisableTextOutput))then
            begin
              if(theCol.PrimaryKey)then
                StretchDraw(Rect(xo+1+EvalZoomFac(16), yo+EvalZoomFac(20+17*ypos),
                  xo+1+EvalZoomFac(16+16), yo+EvalZoomFac(36+17*ypos)),
                  ParentEERModel.FieldKeyBmp)
              else if(theCol.IsForeignKey)then
                StretchDraw(Rect(xo+1+EvalZoomFac(16), yo+EvalZoomFac(20+17*ypos),
                  xo+1+EvalZoomFac(16+16), yo+EvalZoomFac(36+17*ypos)),
                  ParentEERModel.Field_FKBmp)
              else
                StretchDraw(Rect(xo+1+EvalZoomFac(16), yo+EvalZoomFac(20+17*ypos),
                  xo+1+EvalZoomFac(16+16), yo+EvalZoomFac(36+17*ypos)),
                  ParentEERModel.FieldBmp);

              if(Not(DMEER.DisableTextOutput))then
                TextOut(xo+EvalZoomFac(4+16+16), yo+EvalZoomFac(20+17*ypos), theCol.ColName);
            end;

            inc(ypos);
          end;
          Font.Color:=clBlack;
        end;
    end;

    //Table Box
    if(nmTable)then
    begin
      Pen.Color:=clBlack;
    end
    else
      Pen.Color:=clDark;

    if(IsLinkedObject)then
      Pen.Color:=$00C66931;

    MoveTo(xo+0, yo+0);
    LineTo(xo+0, yo+height-3);
    LineTo(xo+width-3, yo+height-3);
    LineTo(xo+width-3, yo+0);
    LineTo(xo+0, yo+0);
    Pen.Style:=psSolid;

    //HeaderLine
    MoveTo(xo+0, yo+EvalZoomFac(18));
    LineTo(xo+width-3, yo+EvalZoomFac(18));

    //Draw Shadow
    if(Not(DMEER.DisableTextOutput))then
    begin
      Pen.Style:=psSolid;
      if(IsLinkedObject)then
        Pen.Color:=$00363636
      else
        Pen.Color:=$00707070;
      MoveTo(xo+width-2, yo);
      LineTo(xo+width-2, yo+height-2);
      LineTo(xo, yo+height-2);

      if(IsLinkedObject)then
        Pen.Color:=$00B3B3B3
      else
        Pen.Color:=$00D0D0D0;
      MoveTo(xo+width-1, yo);
      LineTo(xo+width-1, yo+height-1);
      LineTo(xo, yo+height-1);
    end;
  end;
end;

procedure TEERTable.PaintObj2Canvas(theCanvas: TCanvas; xo, yo: integer);
var width, height: integer;
begin
  if(ParentEERModel.DisableModelRefresh)then
    Exit;

  //ParentEERModel.ClearMouseOverObj;
  width:=EvalZoomFac(Obj_W);
  height:=EvalZoomFac(Obj_H);

  //If the Table is NOT drawn to a special Canvas, use cached image
  if(Not(ParentEERModel.PaintingToSpecialCanvas))then
  begin
    //if the cached Image is out of date
    if(RefreshStrechedImg)then
    begin
      RefreshStrechedImg:=False;

      StrechedImg.Width:=width;
      StrechedImg.Height:=height;

      StrechedImg.Canvas.Font:=Font;

      //Repaint cached image
      PaintCachedImg(StrechedImg.Canvas);
    end;

    //Draw Cached Image
    theCanvas.Draw(xo, yo, StrechedImg);
  end
  else
  begin
    //Draw directly onto the given Canvas
    PaintCachedImg(theCanvas, xo, yo);
  end;

  // ---------------------------------------
  with theCanvas do
  begin
    // Paint selection
    if(selected)then
    begin
      Pen.Color:=clWhite;
      MoveTo(xo+0, yo+0);
      LineTo(xo+0, yo+height-3);
      LineTo(xo+width-3, yo+height-3);
      LineTo(xo+width-3, yo+0);
      LineTo(xo+0, yo+0);

      Pen.Color:=clBlack;
      Pen.Style:=psDot;
      MoveTo(xo+0, yo+0);
      LineTo(xo+0, yo+height-3);
      LineTo(xo+width-3, yo+height-3);
      LineTo(xo+width-3, yo+0);
      LineTo(xo+0, yo+0);

      Pen.Style:=psSolid;
    end;
  end;
end;

procedure TEERTable.DoPaint(Sender: TObject);
begin
  PaintObj(Canvas);
end;

procedure TEERTable.PaintMouseOver;
var i, y, h, colorFadeSpeed: integer;
begin
  try
    if(ParentEERModel.MouseOverSubObj=nil)then
    begin
      Canvas.Pen.Color:=$00666666;
      Canvas.MoveTo(0, 0);
      Canvas.LineTo(0, Height-2);
      Canvas.LineTo(Width-2, Height-2);
      Canvas.LineTo(Width-2, 0);
      Canvas.LineTo(0, 0);

      y:=0;
      Canvas.Pen.Mode:=pmMask;//pmNotMask;
    end
    else
    begin
      y:=EvalZoomFac(20+Columns.IndexOf(ParentEERModel.MouseOverSubObj)*17)-1;
      Canvas.Pen.Mode:=pmMask;//pmNotXor;
    end;

    h:=EvalZoomFac(17);

    //Top & Bottom line
    Canvas.Pen.Color:=$00888888;
    Canvas.MoveTo(1, y);
    Canvas.LineTo(Width-4, y);
    Canvas.MoveTo(1, y+h);
    Canvas.LineTo(Width-4, y+h);

    //Fill in Between
    Canvas.Pen.Color:=$00DDDDDD;
    colorFadeSpeed:=($20 div h)*$10000+($20 div h)*$100+($20 div h);
    //colorFadeSpeed:=($0F div h)*$10000+($0F div h)*100+($0F div h);
    for i:=1 to h-1 do
    begin
      Canvas.MoveTo(1, y+i);
      Canvas.LineTo(Width-4, y+i);
      Canvas.Pen.Color:=Canvas.Pen.Color-colorFadeSpeed;
    end;
  finally
    Canvas.Pen.Mode:=pmCopy;
  end;
end;

procedure TEERTable.PaintMouseOverClear;
begin
  PaintObj2Canvas(Canvas, 0, 0);
end;

procedure TEERTable.RefreshObj;
var i, maxWidth, ColCount, IndexColCount: integer;
  theSize: TSize;
  txt: string;
begin
  with Canvas do
  begin
    theSize:=ParentEERModel.GetTextExtent(ObjName);
    maxWidth:=ReEvalZoomFac(theSize.cx)+16+14;

    {txt:=GetXML;
    if(txt<>CachedTblXML)and(RefreshStrechedImg=False)then
    begin
      CachedTblXML:=txt;
      RefreshStrechedImg:=True;
    end;}
    RefreshStrechedImg:=True;

    //Get Max ColWidth
    ColCount:=0;
    for i:=0 to Columns.Count-1 do
    begin
      //if FK needs not to be displayed
      //and the column is NOT at PK
      if(Not(DMEER.ShowForeignKeys))and
        (TEERColumn(Columns[i]).IsForeignKey)and
        (Not(TEERColumn(Columns[i]).PrimaryKey))then
        continue;

      //Continue if not
      //displaymode is PKLevel and column is PK
      //or displaymode is AttrLevel and the table is not collapsed
      if((DMEER.DisplayMode=dmPrimaryKeyLevel)and
          (TEERColumn(Columns[i]).PrimaryKey))or
        ((DMEER.DisplayMode=dmAttributeLevel)and
          (Not(Collapsed)))then
        inc(ColCount);

      txt:=TEERColumn(Columns[i]).ColName;

      if(DMEER.DisplayPhysicalSchema)then
        txt:=txt+': '+ParentEERModel.GetDataTypeName(TEERColumn(Columns[i]).idDatatype)+
          TEERColumn(Columns[i]).DatatypeParams;

      if(Length(txt)>45)then
        txt:=Copy(txt, 1, 43)+'...';


      if(TEERColumn(Columns[i]).IsForeignKey)then
        txt:=txt+' (FK)';

      theSize:=ParentEERModel.GetTextExtent(txt);
      if(ReEvalZoomFac(theSize.cx)+16+14>maxWidth)then
        maxWidth:=ReEvalZoomFac(theSize.cx)+16+14;
    end;

    if(DMEER.DisplayTableIndices)and(Not(Collapsed))then
    begin
      for i:=0 to Indices.Count-1 do
        if(CompareText(TEERIndex(Indices[i]).IndexName, 'PRIMARY')<>0)then
        begin
          theSize:=ParentEERModel.GetTextExtent(TEERIndex(Indices[i]).IndexName);
          if(ReEvalZoomFac(theSize.cx)+16+14>maxWidth)then
            maxWidth:=ReEvalZoomFac(theSize.cx)+16+14;
        end;
    end;

    if(maxWidth=0)then
      maxWidth:=100;

    Obj_W:=maxWidth;

    if(ColCount<2)or(DMEER.DisplayMode=dmEntityLevel)then
      ColCount:=2;

    Obj_H:=23+ColCount*17;

    if(DMEER.DisplayTableIndices)and(Not(Collapsed))then
    begin
      IndexColCount:=0;

      for i:=0 to Indices.Count-1 do
        if(CompareText(TEERIndex(Indices[i]).IndexName, 'PRIMARY')<>0)then
          IndexColCount:=IndexColCount+TEERIndex(Indices[i]).Columns.Count+1;

      Obj_H:=Obj_H+IndexColCount*17;
    end;
  end;

  //Only reposition Obj when the model is not drawn to another canvas
  if(Not(ParentEERModel.PaintingToSpecialCanvas))then
  begin
    Left:=EvalZoomFac(Obj_X);
    Top:=EvalZoomFac(Obj_Y);
    Width:=EvalZoomFac(Obj_W);
    Height:=EvalZoomFac(Obj_H);
  end;

  //Refresh all Relations
  RefreshRelations;
end;

procedure TEERTable.RefreshRelations(preventRecursion: Boolean = False);
var i: integer;
begin
  if(not(ParentEERModel.PaintingToSpecialCanvas))then
  begin
    for i:=0 to RelStart.Count-1 do
      if(preventRecursion)then
        TEERRel(RelStart[i]).RefreshObj(self)
      else
        TEERRel(RelStart[i]).RefreshObj;

    //If this is a recursive relation, it already has been refreshed
    if(RelStart<>RelEnd)then
    begin
      for i:=0 to RelEnd.Count-1 do
        if(preventRecursion)then
          TEERRel(RelEnd[i]).RefreshObj(self)
        else
          TEERRel(RelEnd[i]).RefreshObj;
    end;
  end;
end;

function TEERTable.GetRelationOffset(theRel: Pointer): integer;
var theDir: integer;
  maxVal: integer;
begin
  if(RelStart.IndexOf(theRel)>-1)then
    theDir:=TEERRel(theRel).relDirection
  else
    theDir:=ReverseRelDirection(TEERRel(theRel).relDirection);

  if(theDir=re_right)or(theDir=re_left)then
    maxVal:=Obj_H
  else
    maxVal:=Obj_W;

  GetRelationOffset:=round(
      (maxVal/
        (Rel[theDir].Count+1)
      )*
      (Rel[theDir].IndexOf(theRel)+1)-(maxVal div 2)
    );

end;

function TEERTable.GetColumnCount: integer;
begin
  GetColumnCount:=Columns.Count;
end;

function TEERTable.GetColumnByIndex(index: integer): Pointer;
begin
  GetColumnByIndex:=Columns[index];
end;

function TEERTable.GetColumnByID(idColumn: integer): Pointer;
var i: integer;
begin
  GetColumnByID:=nil;

  for i:=0 to Columns.Count-1 do
    if(TEERColumn(Columns[i]).Obj_id=idColumn)then
    begin
      GetColumnByID:=Columns[i];
      break;
    end;
end;

function TEERTable.GetColumnByName(cname: string): Pointer;
var i: integer;
begin
  GetColumnByName:=nil;

  for i:=0 to Columns.Count-1 do
    if(TEERColumn(Columns[i]).ColName=cname)then
    begin
      GetColumnByName:=Columns[i];
      break;
    end;
end;

procedure TEERTable.DeleteColumn(index: integer);
var theCol: TEERColumn;
  j, k: integer;
begin
  theCol:=GetColumnByIndex(index);
  if(theCol=nil)then
    Exit;

  //Delete Column from Indices
  j:=0;
  while(j<Indices.Count)do
  begin
    k:=0;
    while(k<=TEERIndex(Indices[j]).Columns.Count-1)do
    begin
      if(StrToInt(TEERIndex(Indices[j]).Columns[k])=theCol.Obj_id)then
        TEERIndex(Indices[j]).Columns.Delete(k)
      else
        inc(k);
    end;

    //if the index contains no more fields, delete it
    if(TEERIndex(Indices[j]).Columns.Count=0)then
    begin
      Indices.Delete(j);
    end
    else
      inc(j);
  end;

  Columns.Delete(index);
end;

function TEERTable.CheckPrimaryIndex: integer;
var i, colCount: integer;
  theIndex: TEERIndex;
  HasAutoInc: Boolean;
begin
  if(Indices.Count>0)then
  begin
    if(CompareText(TEERIndex(Indices[0]).IndexName, 'PRIMARY')=0)then
      theIndex:=TEERIndex(Indices[0])
    else
      theIndex:=nil;
  end
  else
    theIndex:=nil;

  //There can only be one autoinc column
  HasAutoInc:=False;
  for i:=0 to Columns.Count-1 do
  begin
    if(HasAutoInc)and(TEERColumn(Columns[i]).AutoInc)then
      TEERColumn(Columns[i]).AutoInc:=False
    else if(Not(HasAutoInc))and(TEERColumn(Columns[i]).AutoInc)then
      HasAutoInc:=True;
  end;

  //Get Count of columns in Index and set columns no not null
  colCount:=0;
  for i:=0 to Columns.Count-1 do
    if(TEERColumn(Columns[i]).PrimaryKey)then
    begin
      //All columns must not be null
      TEERColumn(Columns[i]).NotNull:=True;

      inc(colCount);
    end;

  //if there was an index and no columns are defined as primary, delete the index
  if(theIndex<>nil)and(colCount=0)then
  begin
    Indices.Delete(0);
  end;

  //if there is no primary index yet and there is at least one col primary
  //create new index and insert it at first pos
  if(theIndex=nil)and(colCount>0)then
  begin
    //new(theIndex);
    theIndex:=TEERIndex.Create(self);
    theIndex.Obj_id:=DMMain.GetNextGlobalID;
    theIndex.IndexName:='PRIMARY';
    theIndex.Pos:=0;
    theIndex.IndexKind:=ik_PRIMARY;
    Indices.Insert(0, theIndex);
  end;

  if(colCount>0)then
  begin
    theIndex.Columns.Clear;

    for i:=0 to Columns.Count-1 do
      if(TEERColumn(Columns[i]).PrimaryKey)then
        theIndex.Columns.Add(IntToStr(TEERColumn(Columns[i]).Obj_id));

    CheckPrimaryIndex:=theIndex.Obj_id;
  end
  else
    CheckPrimaryIndex:=-1;
end;

function TEERTable.GetTablePrefix: String;
begin
  GetTablePrefix:=ParentEERModel.TablePrefix[TablePrefix];
end;

function TEERTable.GetSQLTableName(DoNotAddPrefix: Boolean): String;
var DBQuote, s: string;
  theTablePrefix: integer;
  theRegion: TEERRegion;
begin
  if(DMEER.EncloseNames)then
    DBQuote:=ParentEERModel.DBQuoteCharacter
  else
    DBQuote:='';

  s:='';

  //Check if region overwrites table prefix
  theTablePrefix:=TablePrefix;
  theRegion:=TEERRegion(GetRegion);
  if(theRegion<>nil)then
  begin
    if(theRegion.OverwriteTablePrefix)then
      theTablePrefix:=theRegion.TablePrefix;
  end;

  //Tablename, ignore DEFAULT
  if((theTablePrefix<ParentEERModel.TablePrefix.Count)and
    (theTablePrefix>0))and(Not(DoNotAddPrefix))then
    s:=s+DBQuote+ParentEERModel.TablePrefix[theTablePrefix]+DBQuote+'.';

  s:=s+DBQuote+ObjName+DBQuote;

  GetSQLTableName:=s;
end;

procedure RemoveCRFromString(var s:string);
begin
  s := StringReplace (s,#13,' ',[rfReplaceAll]);
  s := StringReplace (s,#10,' ',[rfReplaceAll]);
end;

function TEERTable.GetSQLCreateCode(DefinePK: Boolean = True;
  CreateIndices: Boolean = True; DefineFK: Boolean = False;
  TblOptions: Boolean = True; StdInserts: Boolean = False;
  OutputComments: Boolean = False;
  HideNullField : Boolean = false;
  PortableIndices : Boolean = false;
  HideOnDeleteUpdateNoAction : boolean = false;
  GOStatement : boolean = false; //usefull for SQL Server
  CommitStatement : boolean = false; //needed for ORACLE Inserts
  FKIndex : boolean = false;
  DefaultBeforeNotNull : boolean = false;
  DatabaseType: string = 'My SQL';
  SeqName: String = 'GlobalSequence';
  PrefixName: String = 'AINC_';
  CreateAutoInc: boolean = false;
  CreateLastChage: boolean = false;
  LastChangeDateCol: string = 'LAST_CHANGE_DATE';
  LastChangeUserCol: string = 'USERID';
  LastChangePrefix: string = 'UPDT_';
  CreateLastExclusion: boolean = false;
  LastExclusionTbName: string = 'DT_EXCLUSION';
  LastExclusionColName: string = 'EX_DATE';
  lastExclusionTriggerPrefix: string = 'EXCDT_'
  ): string;
var s, s1: string;
  sIndex:string; // string for create index
  i, j: integer;
  theRegion: TEERRegion;
  theRel: TEERRel;
  theTableType: integer;
  isTemporary: Boolean;
  relCounter, relSum: integer;
  DBQuote: string;
  theStrList: TStringList;
  indexPortable: boolean;
  indexOnTable:string;
  FinallyPortableIndexes:string;
  FieldOnGeneratorOrSequence:string;
  FKIndexes:string; // indexes for FKs (Oracle needs it)
  FKIndexName:string;// FK index name
  PkColumns: TStringList;
  GoStatementstr : string;
  CommitStatementstr : string;
  TableName : string;
  LocalComment : string;
begin
  FKIndexes := '';
  TableName := GetSQLTableName;

  GoStatementstr := IfThen(GoStatement,'GO'+#13#10,'');
  CommitStatementstr := IfThen(CommitStatement,'commit;'+#13#10,'');

  PkColumns := TStringList.Create;

  //Exit if there are no columns
  if(Columns.Count=0)then
    Exit;

  //Get relSum
  relSum:=0;
  for i:=0 to RelEnd.Count-1 do
    if(TEERRel(RelEnd[i]).CreateRefDef)then
      inc(relSum);

  theTableType:=TableType;
  isTemporary:=Temporary;

  //If table is in a region, check Overwrite-Flags
  theRegion:=TEERRegion(GetRegion);
  if(theRegion<>nil)then
  begin
    if(theRegion.OverwriteTableType)then
      theTableType:=theRegion.TableType;
  end;


  if(DMEER.EncloseNames)then
    DBQuote:=ParentEERModel.DBQuoteCharacter
  else
    DBQuote:='';

  s:='';

  // Show comments if wanted
  if(OutputComments)and
    (Trim(Comments)<>'')then
  begin
    theStrList:=TStringList.Create;
    try
      theStrList.Text:=Comments;

      s:='-- '+StringOfChar('-', 60)+#13#10;

      for i:=0 to theStrList.Count-1 do
        s:=s+'-- '+theStrList[i]+#13#10;

      s:=s+'-- '+StringOfChar('-', 60)+#13#10#13#10;
    finally
      theStrList.Free;
    end;
  end;


  s:=s+'CREATE ';

  //Make table temporary
  if(isTemporary)then
    s:=s+'TEMPORARY ';

  s:=s+'TABLE '+TableName+' ';

  s:=s+'('+#13+#10;

  //Columns
  for i:=0 to Columns.Count-1 do
  begin
    //colname
    if i>0 then
    begin
      s:=s+',';
      s:=s+#13#10;
    end;

    s:=s+'  '+GetSQLColumnCreateDefCode(i,FieldOnGeneratorOrSequence,HideNullField, DefaultBeforeNotNull, DatabaseType, OutputComments);

  end;

  //create column to store record changes
  if CreateLastChage then
  begin
    s := s + ', '+ sLineBreak;
    s := s + '  '+LastChangeDateCol + ' VARCHAR(15), ' + sLineBreak; //confirmar se é varchar(12)
    s := s + '  '+LastChangeUserCol + ' INTEGER ';
  end;

  //Indexes
  FinallyPortableIndexes := '';
  for i:=0 to Indices.Count-1 do
  begin
    if(Not(DefinePK))and(TEERIndex(Indices[i]).IndexName='PRIMARY')then
      continue;

    if(Not(CreateIndices))and(TEERIndex(Indices[i]).IndexName<>'PRIMARY')then
      continue;

    indexPortable := PortableIndices and (TEERIndex(Indices[i]).IndexKind <> ik_PRIMARY);
    sIndex := '';

    if indexPortable then
    begin
      sIndex:= sIndex + 'CREATE ';
      indexOnTable := ' ON '+ GetSQLTableName+' '
    end
    else
    begin
      indexOnTable := '';
    end;

    s:=s+'  ';
    case TEERIndex(Indices[i]).IndexKind of
      ik_PRIMARY:
        sIndex:=sIndex+'PRIMARY KEY(';
      ik_INDEX:
        sIndex:=sIndex+'INDEX '+DBQuote+TEERIndex(Indices[i]).IndexName+DBQuote+indexOnTable+'(';
      ik_UNIQUE_INDEX:
        sIndex:=sIndex+'UNIQUE INDEX '+DBQuote+TEERIndex(Indices[i]).IndexName+DBQuote+indexOnTable+'(';
      ik_FULLTEXT_INDEX:
        sIndex:=sIndex+'FULLTEXT INDEX '+DBQuote+TEERIndex(Indices[i]).IndexName+DBQuote+indexOnTable+'(';
    end;

    for j:=0 to TEERIndex(Indices[i]).Columns.Count-1 do
    begin
      sIndex:=sIndex+DBQuote+TEERColumn(GetColumnByID(StrToInt(TEERIndex(Indices[i]).Columns[j]))).ColName+DBQuote;

      if(TEERIndex(Indices[i]).ColumnParams.Values[TEERIndex(Indices[i]).Columns[j]]<>'')then
        sIndex:=sIndex+'('+TEERIndex(Indices[i]).ColumnParams.Values[TEERIndex(Indices[i]).Columns[j]]+')';

      if(j<TEERIndex(Indices[i]).Columns.Count-1)then
        sIndex:=sIndex+', ';

      //Hold PKs
      if TEERIndex(Indices[i]).IndexKind = ik_PRIMARY then
      begin
        PkColumns.Add(TEERColumn(GetColumnByID(StrToInt(TEERIndex(Indices[i]).Columns[j]))).ColName);
      end;
    end;

    sIndex:=sIndex+')';

    if indexPortable then
    begin
      FinallyPortableIndexes := FinallyPortableIndexes + sIndex + ';'+#13#10+GoStatementstr;
    end else
    begin
      s:=s+',';
      s:=s+#13#10;
      s:=s + sIndex;
    end;
  end;

  //Foreign Keys
  if(DefineFK)then
  begin
    relCounter:=0;
    for i:=0 to RelEnd.Count-1 do
    begin
      theRel:=RelEnd[i];

      if(theRel.CreateRefDef)then
      begin
        s:=s+',';
        s:=s+#13#10;
        //get the FK field list from destination table
        s1:='';
        for j:=0 to theRel.FKFields.Count-1 do
        begin
          s1:=s1+DBQuote+theRel.FKFields.ValueFromIndex[j]+DBQuote;
          if(j<theRel.FKFields.Count-1)then
            s1:=s1+', ';
        end;

        //The Index for INNODB is now created like any other index
        //s:=s+'  INDEX '+theRel.ObjName+'('+s1+'),'+#13#10; //Add this for INNODB
        if(DMEER.DoNotUseRelNameInRefDef)then
          s:=s+'  FOREIGN KEY('+s1+')'+#13#10
        else
          s:=s+'  FOREIGN KEY '+DBQuote+theRel.ObjName+DBQuote+'('+s1+')'+#13#10;

        FKIndexName := copy('IFK_'+DBQuote+theRel.ObjName+DBQuote,1,30);
        FKIndexes :=
          FKIndexes +
          'CREATE INDEX '+FKIndexName+' ON '+GetSQLTableName+' ('+s1+');'+
          #13#10+GoStatementstr;

        s:=s+'    REFERENCES '+DBQuote+TEERTable(theRel.SrcTbl).ObjName+DBQuote+'(';
        //get the FK field list from source table
        for j:=0 to theRel.FKFields.Count-1 do
        begin
          s:=s+DBQuote+theRel.FKFields.Names[j]+DBQuote;
          if(j<theRel.FKFields.Count-1)then
            s:=s+', ';
        end;
        s:=s+')';
        {if(theRel.RefDef.Values['Matching']<>'')then
          case StrToInt(theRel.RefDef.Values['Matching']) of
            0: s:=s+#13#10+'      MATCH FULL';
            1: s:=s+#13#10+'      MATCH PARTIAL';
          end;}
        if(theRel.RefDef.Values['OnDelete']<>'')then
          case StrToInt(theRel.RefDef.Values['OnDelete']) of
            0: s:=s+#13#10+'      ON DELETE RESTRICT';
            1: s:=s+#13#10+'      ON DELETE CASCADE';
            2: s:=s+#13#10+'      ON DELETE SET NULL';
            3: s:=s+IfThen(not HideOnDeleteUpdateNoAction, #13#10+'      ON DELETE NO ACTION','');
            4: s:=s+#13#10+'      ON DELETE SET DEFAULT';
          end;
        if(theRel.RefDef.Values['OnUpdate']<>'')then
          case StrToInt(theRel.RefDef.Values['OnUpdate']) of
            0: s:=s+#13#10+'      ON UPDATE RESTRICT';
            1: s:=s+#13#10+'      ON UPDATE CASCADE';
            2: s:=s+#13#10+'      ON UPDATE SET NULL';
            3: s:=s+IfThen(not HideOnDeleteUpdateNoAction,#13#10+'      ON UPDATE NO ACTION','');
            4: s:=s+#13#10+'      ON UPDATE SET DEFAULT';
          end;

        inc(relCounter);
      end;
    end;
  end;

  s:=s+')';

  //TableType (MYISAM is standard)
  if(theTableType>0) and (DatabaseType = 'My SQL')then
  begin
    s:=s+#13#10+'TYPE=';

    case theTableType of
      1: s:=s+'InnoDB';
      2: s:=s+'HEAP';
      3: s:=s+'BDB';
      4: s:=s+'ISAM';
      5: s:=s+'MERGE';
    end;
  end;

  LocalComment := trim(Comments);
  RemoveCRFromString(LocalComment);

  if (DatabaseType = 'My SQL') and OutputComments  and (length(LocalComment)>0)then
  begin
    LocalComment := StringReplace (LocalComment, '''', '''''', [rfReplaceAll]);
    s:=s+#13#10+'COMMENT = '''+LocalComment+''' ';
  end;

  //Options
  if(TblOptions) and (DatabaseType = 'My SQL') then
  begin
    if(TableOptions.Values['NextAutoIncVal']<>'')then
      s:=s+#13#10+'AUTO_INCREMENT = '+TableOptions.Values['NextAutoIncVal'];
    if(TableOptions.Values['AverageRowLength']<>'')then
      s:=s+#13#10+'AVG_ROW_LENGTH = '+TableOptions.Values['AverageRowLength'];
    if(TableOptions.Values['RowChecksum']<>'0')and
      (TableOptions.Values['RowChecksum']<>'')then
      s:=s+#13#10+'CHECKSUM = '+TableOptions.Values['RowChecksum'];
    if(TableOptions.Values['MaxRowNumber']<>'')then
      s:=s+#13#10+'MAX_ROWS = '+TableOptions.Values['MaxRowNumber'];
    if(TableOptions.Values['MinRowNumber']<>'')then
      s:=s+#13#10+'MIN_ROWS = '+TableOptions.Values['MinRowNumber'];
    if(TableOptions.Values['PackKeys']<>'0')and
      (TableOptions.Values['PackKeys']<>'')then
      s:=s+#13#10+'PACK_KEYS = '+TableOptions.Values['PackKeys'];
    if(TableOptions.Values['TblPassword']<>'')then
      s:=s+#13#10+'PASSWORD = "'+TableOptions.Values['TblPassword']+'"';
    if(TableOptions.Values['DelayKeyTblUpdates']<>'0')and
      (TableOptions.Values['DelayKeyTblUpdates']<>'')then
      s:=s+#13#10+'DELAY_KEY_WRITE = '+TableOptions.Values['DelayKeyTblUpdates'];
    if(TableOptions.Values['RowFormat']<>'0')then
    begin
      if(TableOptions.Values['RowFormat']='1')then
        s:=s+#13#10+'ROW_FORMAT = dynamic'
      else if(TableOptions.Values['RowFormat']='2')then
        s:=s+#13#10+'ROW_FORMAT = fixed'
      else if(TableOptions.Values['RowFormat']='3')then
        s:=s+#13#10+'ROW_FORMAT = compressed';
    end;

    if(TableOptions.Values['UseRaid']='1')then
    begin
      if(TableOptions.Values['RaidType']='0')then
        s:=s+#13#10+'RAID_TYPE = STRIPED ';

      s:=s+'RAID_CHUNKS = '+TableOptions.Values['Chunks']+
        ' RAID_CHUNKSIZE = '+TableOptions.Values['ChunkSize'];
    end;

    if(TableOptions.Values['TblDataDir']<>'')then
      s:=s+#13#10+'DATA DIRECTORY = "'+TableOptions.Values['TblDataDir']+'"';
    if(TableOptions.Values['TblIndexDir']<>'')then
      s:=s+#13#10+'INDEX DIRECTORY = "'+TableOptions.Values['TblIndexDir']+'"';
  end;

  s:=s+';'+#13#10+GoStatementstr;

  if length(FinallyPortableIndexes)>0 then
  begin
    s:=s+#13#10#13#10+FinallyPortableIndexes;
  end;

  //Standard Inserts
  if(StdInserts)and(trim(StandardInserts.Text)<>'')then
  begin
    s:=s+#13#10#13#10+StandardInserts.Text+#13#10+GoStatementstr+CommitStatementstr;
  end;

  //create triggers to implements auto_increment function to oracle/firebird
  if (FieldOnGeneratorOrSequence <> '') and CreateAutoInc then
  begin
    s:=s + sLineBreak +  sLineBreak +
      getTriggerForSequences(SeqName, DatabaseType, PrefixName, FieldOnGeneratorOrSequence);
  end;

  //create triggers to implements record last change date
  if CreateLastChage then
  begin
    s := s + sLineBreak + sLineBreak +
      GetTriggersForLastChangeDate(LastChangeDateCol, LastChangePrefix, DataBaseType, PkColumns);
  end;

  if CreateLastExclusion then
  begin
    s := s + sLineBreak + sLineBreak +
      GetTriggerForLastDeleteDate(LastExclusionTbName, LastExclusionColName,
                                  lastExclusionTriggerPrefix, DatabaseType);
  end;

  // Should output comments?
  if OutputComments then
  begin
    s:=s + #13#10 +
       getSqlTableComment(GetSQLTableName, Comments, DatabaseType);
  end;

  // should create indexes for FKs?
  if FKIndex then
  begin
    s:=s + #13#10 + FKIndexes;
  end;

  PkColumns.Free;

  GetSQLCreateCode:=s;
end;

function TEERTable.GetSQLColumnCreateDefCode(i: integer;
                                             var TableFieldGen: string;
                                             HideNullField:boolean = false;
                                             DefaultBeforeNotNull : boolean = false; // Defalut tag should appear before NOT NULL
                                             DatabaseType: string = 'My SQL';
                                             OutputComments: boolean = false
                                             ): string;
var s: string;
  j: integer;
  theDatatype: TEERDatatype;
  defaultTag:string;
  NullTag:string;
  ColComment:string;
begin
  defaultTag:='';
  NullTag:='';

  if(DMEER.EncloseNames)then
    s:=ParentEERModel.DBQuoteCharacter+TEERColumn(Columns[i]).ColName+
      ParentEERModel.DBQuoteCharacter+' '
  else
    s:=TEERColumn(Columns[i]).ColName+' ';

  if
    (TEERColumn(Columns[i]).AutoInc)
    and (DatabaseType = 'PostgreSQL') then
  begin
    s := s + 'SERIAL';
  end else
  begin
    //Datatype
    theDatatype:=ParentEERModel.GetDataType(TEERColumn(Columns[i]).idDatatype);
    //Datatype name (INTEGER)
    s:=s+theDatatype.GetPhysicalTypeName;

    //Datatype parameters (10, 2)
    if(TEERColumn(Columns[i]).DatatypeParams<>'')then
      s:=s+TEERColumn(Columns[i]).DatatypeParams;
    s:=s+' ';
  end;

  //Datatype options ([UNSIGNED] [ZEROFILL])
  if DatabaseType = 'My SQL' then
  begin
    for j:=0 to theDatatype.OptionCount-1 do
    begin
      if(TEERColumn(Columns[i]).OptionSelected[j])then
        s:=s+theDatatype.Options[j]+' ';
    end;
  end;

  //Set not null
  if(TEERColumn(Columns[i]).NotNull) then
  begin
    NullTag:='NOT NULL';
  end
  else
  begin
    if not HideNullField then
    begin
      NullTag:='NULL';
    end; // if not HideNullField
  end;

  // default value
  if(TEERColumn(Columns[i]).DefaultValue<>'')then
    if(Not(DMEER.AddQuotesToDefVals))then
      defaultTag:='DEFAULT '+TEERColumn(Columns[i]).DefaultValue
    else
      defaultTag:='DEFAULT '''+TEERColumn(Columns[i]).DefaultValue+'''';

  if (DefaultBeforeNotNull) then
  begin
    s := s+' '+defaultTag+' '+NullTag+' ';
  end else
  begin
    s := s+' '+NullTag+' '+defaultTag+' ';
  end;

  // auto increment
  if(TEERColumn(Columns[i]).AutoInc)then
  begin
    if DatabaseType = 'My SQL' then
    begin
      s:=s+' AUTO_INCREMENT';
      TableFieldGen := '';
    end else
    if DatabaseType = 'SQL Server' then
    begin
      s:=s+' IDENTITY ';
      TableFieldGen := '';
    end else
    begin
      TableFieldGen := TEERColumn(Columns[i]).ColName;
    end;
  end;

  {if(TEERColumn(Columns[i]).PrimaryKey)then
    s:=s+' PRIMARY KEY';}
  ColComment := trim(TEERColumn(Columns[i]).Comments);
  ColComment := StringReplace (ColComment, '''', '''''', [rfReplaceAll]);
  if OutputComments and (Length(ColComment)>0) then
  begin

    if DatabaseType = 'My SQL' then
    begin
      s := s+' COMMENT '''+ColComment+''' ';
    end;
  end;

  GetSQLColumnCreateDefCode:=s;
end;

function TEERTable.GetSQLDropCode(IfExists:boolean = false): string;
var DBQuote: string;
begin
  if(DMEER.EncloseNames)then
    DBQuote:=ParentEERModel.DBQuoteCharacter
  else
    DBQuote:='';

  if(TablePrefix=0)then
    GetSQLDropCode:=
      'DROP TABLE '+
      IfThen(IfExists,'IF EXISTS ','')+
      DBQuote+ObjName+DBQuote+';'
  else
    GetSQLDropCode:='DROP TABLE '+
      IfThen(IfExists,'IF EXISTS ','')+
      DBQuote+ParentEERModel.TablePrefix[TablePrefix]+DBQuote+'.'+DBQuote+ObjName+DBQuote+';';
end;

function TEERTable.GetSQLInsertCode: string;
var s: string;
  i: integer;
  DBQuote: string;
begin
  if(DMEER.EncloseNames)then
    DBQuote:=ParentEERModel.DBQuoteCharacter
  else
    DBQuote:='';

  s:='INSERT INTO '+GetSQLTableName+' (';

  for i:=0 to Columns.Count-1 do
  begin
    s:=s+DBQuote+TEERColumn(Columns[i]).ColName+DBQuote;

    if(i<Columns.Count-1)then
      s:=s+', ';
  end;

  s:=s+') VALUES(';

  {for i:=0 to Columns.Count-1 do
  begin
    if(i<Columns.Count-1)then
      s:=s+', ';
  end;

  s:=s+')';}

  GetSQLInsertCode:=s;
end;

procedure TEERTable.CopyTableName(Sender: TObject);
begin
  Clipboard.AsText:=GetSQLTableName;
end;

procedure TEERTable.CopyTableFields(Sender: TObject);
var s: string;
  i: integer;
begin
  s:='';

  for i:=0 to Columns.Count-1 do
  begin
    if(DMEER.EncloseNames)then
      s:=s+ParentEERModel.DBQuoteCharacter+TEERColumn(Columns[i]).ColName+
        ParentEERModel.DBQuoteCharacter
    else
      s:=s+TEERColumn(Columns[i]).ColName;

    if(i<Columns.Count-1)then
      s:=s+', ';
  end;

  Clipboard.AsText:=s;
end;

procedure TEERTable.CopyTableSQLCreate(Sender: TObject);
begin
  Clipboard.AsText:=GetSQLCreateCode;
end;

procedure TEERTable.CopyTableSQLDrop(Sender: TObject);
begin
  Clipboard.AsText:=GetSQLDropCode;
end;

procedure TEERTable.CopyTableSQLInsert(Sender: TObject);
begin
  Clipboard.AsText:=GetSQLInsertCode;
end;


procedure TEERTable.PrimaryColumnsFirst;
var i, lastKeyPos: integer;
begin
  lastKeyPos:=0;
  for i:=0 to Columns.Count-1 do
  begin
    if(TEERColumn(Columns[i]).PrimaryKey)then
    begin
      if(i<>lastKeyPos)then
      begin
        Columns.Move(i, lastKeyPos);
      end;

      inc(lastKeyPos);
    end;
  end;
end;

function TEERTable.GetXML: string;
var s: string;
  j, k: integer;
  CO: TEERColumn;
  DT: TEERDatatype;
begin
  s:='<TABLE '+
    'ID="'+IntToStr(Obj_id)+'" '+
    'Tablename="'+DMMain.EncodeText4XML(ObjName)+'" '+
    'PrevTableName="'+DMMain.EncodeText4XML(PrevTableName)+'" '+
    'XPos="'+IntToStr(Obj_X)+'" '+
    'YPos="'+IntToStr(Obj_Y)+'" '+
    'TableType="'+IntToStr(TableType)+'" '+
    'TablePrefix="'+IntToStr(TablePrefix)+'" '+
    'nmTable="'+IntToStr(Ord(nmTable))+'" '+
    'Temporary="'+IntToStr(Ord(Temporary))+'" '+
    'UseStandardInserts="'+IntToStr(Ord(UseStandardInserts))+'" '+
    'StandardInserts="'+DMMain.EncodeText4XML(StandardInserts.Text)+'" '+
    'TableOptions="'+DMMain.EncodeText4XML(TableOptions.Text)+'" '+
    'Comments="'+DMMain.EncodeText4XML(Comments)+'" '+
    'Collapsed="'+IntToStr(Ord(Collapsed))+'" '+
    'IsLinkedObject="'+IntToStr(Ord(IsLinkedObject))+'" '+
    'IDLinkedModel="'+IntToStr(IDLinkedModel)+'" '+
    'Obj_id_Linked="'+IntToStr(Obj_id_Linked)+'" '+
    'OrderPos="'+IntToStr(OrderPos)+'" '+
    '>'+#13#10;

  //Columns
  s:=s+'<COLUMNS>'+#13#10;
  for j:=0 to Columns.Count-1 do
  begin
    CO:=TEERColumn(Columns[j]);
    s:=s+'<COLUMN '+
      'ID="'+IntToStr(CO.Obj_id)+'" '+
      'ColName="'+DMMain.EncodeText4XML(CO.ColName)+'" '+
      'PrevColName="'+DMMain.EncodeText4XML(CO.PrevColName)+'" '+
      'Pos="'+IntToStr(CO.Pos)+'" '+
      'idDatatype="'+IntToStr(CO.idDatatype)+'" '+
      'DatatypeParams="'+DMMain.EncodeText4XML(CO.DatatypeParams)+'" '+
      'Width="'+IntToStr(CO.Width)+'" '+
      'Prec="'+IntToStr(CO.Prec)+'" '+
      'PrimaryKey="'+IntToStr(Ord(CO.PrimaryKey))+'" '+
      'NotNull="'+IntToStr(Ord(CO.NotNull))+'" '+
      'AutoInc="'+IntToStr(Ord(CO.AutoInc))+'" '+
      'IsForeignKey="'+IntToStr(Ord(CO.IsForeignKey))+'" '+
      'DefaultValue="'+DMMain.EncodeText4XML(CO.DefaultValue)+'" '+
      'Comments="'+DMMain.EncodeText4XML(CO.Comments)+'"'+
      '>'+#13#10;

    //Selected Options
    s:=s+'<OPTIONSELECTED>'+#13#10;
    DT:=TEERDatatype(ParentEERModel.GetDataType(CO.idDatatype));

    for k:=0 to DT.OptionCount-1 do
      s:=s+'<OPTIONSELECT Value="'+IntToStr(Ord(CO.OptionSelected[k]))+'" />'+#13#10;

    s:=s+'</OPTIONSELECTED>'+#13#10;

    s:=s+'</COLUMN>'+#13#10;
  end;
  s:=s+'</COLUMNS>'+#13#10;

  //Relations
  if(RelStart.Count>0)then
  begin
    s:=s+'<RELATIONS_START>'+#13#10;
    for j:=0 to RelStart.Count-1 do
      s:=s+'<RELATION_START ID="'+IntToStr(TEERRel(RelStart[j]).Obj_id)+'" />'+#13#10;
    s:=s+'</RELATIONS_START>'+#13#10;
  end;

  if(RelEnd.Count>0)then
  begin
    s:=s+'<RELATIONS_END>'+#13#10;
    for j:=0 to RelEnd.Count-1 do
      s:=s+'<RELATION_END ID="'+IntToStr(TEERRel(RelEnd[j]).Obj_id)+'" />'+#13#10;
    s:=s+'</RELATIONS_END>'+#13#10;
  end;

  //Write INDICES
  if(Indices.Count>0)then
  begin
    s:=s+'<INDICES>'+#13#10;
    for j:=0 to Indices.Count-1 do
    begin
      s:=s+'<INDEX '+
        'ID="'+IntToStr(TEERIndex(Indices[j]).Obj_id)+'" '+
        'IndexName="'+DMMain.EncodeText4XML(TEERIndex(Indices[j]).IndexName)+'" '+
        'IndexKind="'+IntToStr(TEERIndex(Indices[j]).IndexKind)+'" '+
        'FKRefDef_Obj_id="'+IntToStr(Ord(TEERIndex(Indices[j]).FKRefDef_Obj_id))+'"'+
        '>'+#13#10;

      //Write Index Columns
      s:=s+'<INDEXCOLUMNS>'+#13#10;
      for k:=0 to TEERIndex(Indices[j]).Columns.Count-1 do
      begin
        s:=s+'<INDEXCOLUMN idColumn="'+TEERIndex(Indices[j]).Columns[k]+'" ';
        if(TEERIndex(Indices[j]).ColumnParams.Values[TEERIndex(Indices[j]).Columns[k]]<>'')then
          s:=s+'LengthParam="'+TEERIndex(Indices[j]).ColumnParams.Values[TEERIndex(Indices[j]).Columns[k]]+'" />'+#13#10
        else
          s:=s+'LengthParam="0" />'+#13#10;
      end;
      s:=s+'</INDEXCOLUMNS>'+#13#10;

      s:=s+'</INDEX>'+#13#10;
    end;
    s:=s+'</INDICES>'+#13#10;
  end;

  s:=s+'</TABLE>'+#13#10;

  GetXML:=s;
end;

{$IFDEF USE_IXMLDBMODELType}
procedure TEERTable.SetXML(theXMLTable: IXMLTABLEType);
var thecolumn: TEERColumn;
  theIndex: TEERIndex;
  j, k: integer;
  s: string;
begin
  try
    Obj_id:=theXMLTable.ID;
    ObjName:=DMMain.DecodeXMLText(theXMLTable.Tablename);
    Obj_X:=theXMLTable.XPos;
    Obj_Y:=theXMLTable.YPos;

    try
      IsLinkedObject:=(theXMLTable.IsLinkedObject=1);
      IDLinkedModel:=StrToInt(theXMLTable.IDLinkedModel);
      Obj_id_Linked:=StrToInt(theXMLTable.Obj_id_Linked);
    except
      IsLinkedObject:=False;
      IDLinkedModel:=-1;
      Obj_id_Linked:=-1;
    end;

    PrevTableName:=DMMain.DecodeXMLText(theXMLTable.PrevTableName);

    TableType:=theXMLTable.TableType;
    TablePrefix:=theXMLTable.TablePrefix;

    //nmTable was introduced in 4.0.2.74, so it may not be present in file
    try
      nmTable:=(theXMLTable.nmTable=1);
    except
      nmTable:=False;
    end;

    Temporary:=(theXMLTable.Temporary=1);
    UseStandardInserts:=(theXMLTable.UseStandardInserts=1);
    StandardInserts.Text:=DMMain.DecodeXMLText(theXMLTable.StandardInserts);
    TableOptions.Text:=DMMain.DecodeXMLText(theXMLTable.TableOptions);

    //nmTable was introduced in 4.0.3.22, so it may not be present in file
    try
      Collapsed:=(theXMLTable.Collapsed=1);
    except
      Collapsed:=False;
    end;

    //Clear Columns
    Columns.Clear;

    for j:=0 to theXMLTable.COLUMNS.Count-1 do
    begin
      //New(theColumn);
      theColumn:=TEERColumn.Create(self);

      theColumn.ColName:=DMMain.DecodeXMLText(theXMLTable.COLUMNS.COLUMN[j].ColName);
      theColumn.PrevColName:=DMMain.DecodeXMLText(theXMLTable.COLUMNS.COLUMN[j].PrevColName);
      theColumn.Obj_id:=theXMLTable.COLUMNS.COLUMN[j].ID;
      theColumn.Pos:=theXMLTable.COLUMNS.COLUMN[j].Pos;
      theColumn.idDatatype:=theXMLTable.COLUMNS.COLUMN[j].IdDatatype;
      theColumn.DatatypeParams:=DMMain.DecodeXMLText(theXMLTable.COLUMNS.COLUMN[j].DatatypeParams);
      theColumn.Width:=StrToInt(theXMLTable.COLUMNS.COLUMN[j].Width);
      theColumn.Prec:=StrToInt(theXMLTable.COLUMNS.COLUMN[j].Prec);
      theColumn.PrimaryKey:=(theXMLTable.COLUMNS.COLUMN[j].PrimaryKey=1);
      theColumn.NotNull:=(theXMLTable.COLUMNS.COLUMN[j].NotNull=1);
      theColumn.AutoInc:=(theXMLTable.COLUMNS.COLUMN[j].AutoInc=1);
      //IsForeignKey was renamed in 4.0.2.74, so it may not be present in file
      try
        theColumn.IsForeignKey:=(theXMLTable.COLUMNS.COLUMN[j].IsForeignKey=1);
      except
        theColumn.IsForeignKey:=False;
      end;

      //Get Option Defaults
      for k:=0 to theXMLTable.COLUMNS.COLUMN[j].OPTIONSELECTED.Count-1 do
        theColumn.OptionSelected[k]:=
          (theXMLTable.COLUMNS.COLUMN[j].OPTIONSELECTED[k].Value=1);

      theColumn.DefaultValue:=DMMain.DecodeXMLText(DMMain.DecodeXMLText(theXMLTable.COLUMNS.COLUMN[j].DefaultValue));

      theColumn.Comments:=DMMain.DecodeXMLText(theXMLTable.COLUMNS.COLUMN[j].Comments);

      Columns.Add(theColumn);
    end;

    //Clear Index List
    Indices.Clear;

    for j:=0 to theXMLTable.INDICES.Count-1 do
    begin
      //new(theIndex);
      theIndex:=TEERIndex.Create(self);

      theIndex.Obj_id:=theXMLTable.INDICES.INDEX[j].ID;
      theIndex.IndexName:=theXMLTable.INDICES.INDEX[j].IndexName;
      theIndex.IndexKind:=theXMLTable.INDICES.INDEX[j].IndexKind;
      Indices.Add(theIndex);
      theIndex.Pos:=Indices.Count-1;
      try
        theIndex.FKRefDef_Obj_id:=StrToInt(theXMLTable.INDICES.INDEX[j].FKRefDef_Obj_id);
      except
        theIndex.FKRefDef_Obj_id:=-1;
      end;

      //Index Columns
      for k:=0 to theXMLTable.INDICES.INDEX[j].INDEXCOLUMNS.Count-1 do
      begin
        theIndex.Columns.Add(IntToStr(theXMLTable.INDICES.INDEX[j].INDEXCOLUMNS.INDEXCOLUMN[k].idColumn));

        //Try to read Index Column Lengths
        try
          if(theXMLTable.INDICES.INDEX[j].INDEXCOLUMNS.INDEXCOLUMN[k].LengthParam<>0)then
            theIndex.ColumnParams.Add(IntToStr(theXMLTable.INDICES.INDEX[j].INDEXCOLUMNS.INDEXCOLUMN[k].idColumn)+'='+
              IntToStr(theXMLTable.INDICES.INDEX[j].INDEXCOLUMNS.INDEXCOLUMN[k].LengthParam));
        except
        end;
      end;
    end;

    Comments:=DMMain.DecodeXMLText(theXMLTable.Comments);
  except
    on x: Exception do
    begin
      s:=ObjName+#13#10+
        'Error: '+x.Message;

      ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Table from XML File:'+#13#10+
        'Tablename: %s', 78));
    end;
  end;
end;
{$ENDIF}

procedure TEERTable.SetXML2(theXMLParser: TXmlParser);
var s: string;

  procedure getTABLECOLUMNSoptions(thecolumn: TEERColumn);
  var i: integer;
  begin
    if(thecolumn=nil)then
      Exit;

    i:=0;
    while(theXMLParser.Scan)do
      if(theXMLParser.CurPartType=ptEmptyTag)then
      begin
        if(theXMLParser.CurName='OPTIONSELECT')then
        begin
          theColumn.OptionSelected[i]:=
            (theXMLParser.CurAttr.Value('Value')='1');

          inc(i);
        end
        else
          break;
      end
      else if(theXMLParser.CurPartType=ptEndTag)then
      begin
        if(theXMLParser.CurName='OPTIONSELECTED')then
        else
          break;
      end
      else
        break;
  end;

  procedure getTABLECOLUMNS;
  var thecolumn: TEERColumn;
  begin
    thecolumn:=nil;

    while(theXMLParser.Scan)do
      if(theXMLParser.CurPartType=ptStartTag)then
      begin
        if(theXMLParser.CurName='COLUMN')then
        begin
          //Create Column
          theColumn:=TEERColumn.Create(self);

          theColumn.ColName:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('ColName'));
          theColumn.PrevColName:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('PrevColName'));
          theColumn.Obj_id:=StrToInt(theXMLParser.CurAttr.Value('ID'));
          theColumn.Pos:=StrToInt(theXMLParser.CurAttr.Value('Pos'));
          theColumn.idDatatype:=StrToInt(theXMLParser.CurAttr.Value('idDatatype'));
          theColumn.DatatypeParams:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('DatatypeParams'));
          theColumn.Width:=StrToInt(theXMLParser.CurAttr.Value('Width'));
          theColumn.Prec:=StrToInt(theXMLParser.CurAttr.Value('Prec'));
          theColumn.PrimaryKey:=(theXMLParser.CurAttr.Value('PrimaryKey')='1');
          theColumn.NotNull:=(theXMLParser.CurAttr.Value('NotNull')='1');
          theColumn.AutoInc:=(theXMLParser.CurAttr.Value('AutoInc')='1');

          //IsForeignKey was renamed in 4.0.2.74, so it may not be present in file
          try
            theColumn.IsForeignKey:=(theXMLParser.CurAttr.Value('IsForeignKey')='1');
          except
            theColumn.IsForeignKey:=False;
          end;

          theColumn.DefaultValue:=DMMain.DecodeXMLText(DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('DefaultValue')));

          theColumn.Comments:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('Comments'));

          Columns.Add(theColumn);
        end
        else if(theXMLParser.CurName='OPTIONSELECTED')then
          getTABLECOLUMNSoptions(thecolumn)
        else
          break;
      end
      else if(theXMLParser.CurPartType=ptEndTag)then
      begin
        if(theXMLParser.CurName='COLUMN')or
          (theXMLParser.CurName='COLUMNS')then
        else
          break;
      end
      else
        break;
  end;

  procedure getTABLERELATIONSstart;
  begin
    while(theXMLParser.Scan)do
      if(theXMLParser.CurPartType=ptEmptyTag)then
      begin
        if(theXMLParser.CurName='RELATION_START')then
        begin
        end
        else
          break;
      end
      else if(theXMLParser.CurPartType=ptEndTag)then
      begin
        if(theXMLParser.CurName='RELATIONS_START')then
        else
          break;
      end
      else
        break;
  end;

  procedure getTABLERELATIONSend;
  begin
    while(theXMLParser.Scan)do
      if(theXMLParser.CurPartType=ptEmptyTag)then
      begin
        if(theXMLParser.CurName='RELATION_END')then
        begin
        end
        else
          break;
      end
      else if(theXMLParser.CurPartType=ptEndTag)then
      begin
        if(theXMLParser.CurName='RELATIONS_END')then
        else
          break;
      end
      else
        break;
  end;

  procedure getTABLEINDEXCOLUMNS(theIndex: TEERIndex);
  begin
    while(theXMLParser.Scan)do
      if(theXMLParser.CurPartType=ptEmptyTag)then
      begin
        if(theXMLParser.CurName='INDEXCOLUMN')then
        begin
          theIndex.Columns.Add(theXMLParser.CurAttr.Value('idColumn'));

          //Try to read Index Column Lengths
          try
            if(theXMLParser.CurAttr.Value('LengthParam')<>'0')then
              theIndex.ColumnParams.Add(theXMLParser.CurAttr.Value('idColumn')+'='+
                theXMLParser.CurAttr.Value('LengthParam'));
          except
          end;
        end
        else
          break;
      end
      else if(theXMLParser.CurPartType=ptEndTag)then
      begin
        if(theXMLParser.CurName='INDEXCOLUMNS')then
        else
          break;
      end
      else
        break;
  end;

  procedure getTABLEINDICES;
  var theIndex: TEERIndex;
  begin
    theIndex:=nil;

    while(theXMLParser.Scan)do
      if(theXMLParser.CurPartType=ptStartTag)then
      begin
        if(theXMLParser.CurName='INDEX')then
        begin
          //Create Index
          theIndex:=TEERIndex.Create(self);

          theIndex.Obj_id:=StrToInt(theXMLParser.CurAttr.Value('ID'));
          theIndex.IndexName:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('IndexName'));
          theIndex.IndexKind:=StrToInt(theXMLParser.CurAttr.Value('IndexKind'));

          Indices.Add(theIndex);

          theIndex.Pos:=Indices.Count-1;
          try
            theIndex.FKRefDef_Obj_id:=StrToInt(theXMLParser.CurAttr.Value('FKRefDef_Obj_id'));
          except
            theIndex.FKRefDef_Obj_id:=-1;
          end;
        end
        else if(theXMLParser.CurName='INDEXCOLUMNS')then
          getTABLEINDEXCOLUMNS(theIndex)
        else
          break;
      end
      else if(theXMLParser.CurPartType=ptEndTag)then
      begin
        if(theXMLParser.CurName='INDEXCOLUMNS')or
          (theXMLParser.CurName='INDEX')or
          (theXMLParser.CurName='INDICES')then
        else
          break;
      end
      else
        break;
  end;

begin
  try
    obj_id:=StrToInt(theXMLParser.CurAttr.Value('ID'));
    ObjName:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('Tablename'));
    Obj_X:=StrToInt(theXMLParser.CurAttr.Value('XPos'));
    Obj_Y:=StrToInt(theXMLParser.CurAttr.Value('YPos'));

    try
      IsLinkedObject:=(theXMLParser.CurAttr.Value('IsLinkedObject')='1');
      IDLinkedModel:=StrToInt(theXMLParser.CurAttr.Value('IDLinkedModel'));
      Obj_id_Linked:=StrToInt(theXMLParser.CurAttr.Value('Obj_id_Linked'));
    except
      IsLinkedObject:=False;
      IDLinkedModel:=-1;
      Obj_id_Linked:=-1;
    end;

    PrevTableName:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('PrevTableName'));

    TableType:=StrToInt(theXMLParser.CurAttr.Value('TableType'));
    TablePrefix:=StrToInt(theXMLParser.CurAttr.Value('TablePrefix'));

    //nmTable was introduced in 4.0.2.74, so it may not be present in file
    try
      nmTable:=(theXMLParser.CurAttr.Value('nmTable')='1');
    except
      nmTable:=False;
    end;

    Temporary:=(theXMLParser.CurAttr.Value('Temporary')='1');
    UseStandardInserts:=(theXMLParser.CurAttr.Value('UseStandardInserts')='1');
    StandardInserts.Text:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('StandardInserts'));
    TableOptions.Text:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('TableOptions'));

    //nmTable was introduced in 4.0.3.22, so it may not be present in file
    try
      Collapsed:=(theXMLParser.CurAttr.Value('Collapsed')='1');
    except
      Collapsed:=False;
    end;

    Comments:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('Comments'));

    try
      OrderPos:=StrToInt(theXMLParser.CurAttr.Value('OrderPos'));
    except
    end;    

    //Clear Columns
    Columns.Clear;
    //Clear Index List
    Indices.Clear;

    //Get Columns
    theXMLParser.Scan;
    if(theXMLParser.CurName='COLUMNS')then
      getTABLECOLUMNS;


    //Get Relstart
    if(theXMLParser.CurName='RELATIONS_START')then
      getTABLERELATIONSstart;

    //Get Relend
    if(theXMLParser.CurName='RELATIONS_END')then
      getTABLERELATIONSend;


    //Get Indices
    if(theXMLParser.CurName='INDICES')then
      getTABLEINDICES;
  except
    on x: Exception do
    begin
      s:=ObjName+#13#10+
        'Error: '+x.Message;

      ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Table from XML File:'+#13#10+
        'Tablename: %s', 78));
    end;
  end;
end;

function TEERTable.GetnmTableStatus: Boolean;
begin
  GetnmTableStatus:=nmTable;
end;

procedure TEERTable.SetnmTableStatus(isnmTable: Boolean);
begin
  nmTable:=isnmTable;
end;

procedure TEERTable.Assign(Source: TPersistent);
var i: integer;
  theSourceTbl: TEERTable;
  theColumn: TEERColumn;
  theIndex: TEERIndex;
begin
  inherited Assign(Source);

  if Source is TEERTable then
  begin
    theSourceTbl:=TEERTable(Source);

    //Parent:=theSourceTbl.Parent;

    nmTable:=theSourceTbl.GetnmTableStatus;

    TableType:=theSourceTbl.TableType;
    TablePrefix:=theSourceTbl.TablePrefix;
    Temporary:=theSourceTbl.Temporary;

    PrevTableName:=theSourceTbl.PrevTableName;

    TableOptions.Text:=theSourceTbl.TableOptions.Text;

    StandardInserts.Text:=theSourceTbl.StandardInserts.Text;
    UseStandardInserts:=theSourceTbl.UseStandardInserts;

    //Columns.Assign(theSourceTbl.Columns);
    Columns.Clear;
    for i:=0 to theSourceTbl.Columns.Count-1 do
    begin
      theColumn:=TEERColumn.Create(self);
      theColumn.Assign(TEERColumn(theSourceTbl.Columns[i]));
      Columns.Add(theColumn);
    end;

    //Indices.Assign(theSourceTbl.Indices);
    Indices.Clear;
    for i:=0 to theSourceTbl.Indices.Count-1 do
    begin
      theIndex:=TEERIndex.Create(self);
      theIndex.Assign(TEERIndex(theSourceTbl.Indices[i]));
      Indices.Add(theIndex);
    end;

    RelStart.Assign(theSourceTbl.RelStart);
    RelEnd.Assign(theSourceTbl.RelEnd);

    // Lists of Relations on each side
    for i:=1 to 4 do
      Rel[i].Assign(theSourceTbl.Rel[i]);

    RefreshStrechedImg:=theSourceTbl.RefreshStrechedImg;
    StrechedImg.Assign(theSourceTbl.StrechedImg);
    CachedTblXML:=theSourceTbl.CachedTblXML;
  end;
end;

function TEERTable.ObjIsEqualTo(Source: TObject): Boolean;
var i, j: integer;
begin
  ObjIsEqualTo:=False;
  if(Not(Source is TEERTable))then
    Exit;

  if(inherited ObjIsEqualTo(Source))then
    if(TableType=TEERTable(Source).TableType)and
      (TablePrefix=TEERTable(Source).TablePrefix)and
      (Temporary=TEERTable(Source).Temporary)and
      (PrevTableName=TEERTable(Source).PrevTableName)and
      (TableOptions.Text=TEERTable(Source).TableOptions.Text)and
      (StandardInserts.Text=TEERTable(Source).StandardInserts.Text)and
      (UseStandardInserts=TEERTable(Source).UseStandardInserts)and
      (nmTable=TEERTable(Source).GetnmTableStatus)then
    begin
      ObjIsEqualTo:=True;

      //Check all columns
      if(Columns.Count=TEERTable(Source).Columns.Count)then
      begin
        for i:=0 to Columns.Count-1 do
          if(Not(TEERColumn(Columns[i]).ObjIsEqualTo(TEERColumn(TEERTable(Source).Columns[i]))))then
          begin
            ObjIsEqualTo:=False;
            exit;
          end;
      end
      else
        ObjIsEqualTo:=False;

      //Check all Indices
      if(Indices.Count=TEERTable(Source).Indices.Count)then
      begin
        for i:=0 to Indices.Count-1 do
          if(Not(TEERIndex(Indices[i]).ObjIsEqualTo(TEERIndex(TEERTable(Source).Indices[i]))))then
          begin
            ObjIsEqualTo:=False;
            exit;
          end;
      end
      else
        ObjIsEqualTo:=False;

      //Check Rel Start
      if(RelStart.Count=TEERTable(Source).RelStart.Count)then
      begin
        for i:=0 to RelStart.Count-1 do
          if(Not(RelStart[i]=TEERTable(Source).RelStart[i]))then
          begin
            ObjIsEqualTo:=False;
            exit;
          end;
      end
      else
        ObjIsEqualTo:=False;

      //Check Rel End
      if(RelEnd.Count=TEERTable(Source).RelEnd.Count)then
      begin
        for i:=0 to RelEnd.Count-1 do
          if(Not(RelEnd[i]=TEERTable(Source).RelEnd[i]))then
          begin
            ObjIsEqualTo:=False;
            exit;
          end;
      end
      else
        ObjIsEqualTo:=False;

      //Check Rel on each side
      for j:=1 to 4 do
        begin
        if(Rel[j].Count=TEERTable(Source).Rel[j].Count)then
        begin
          for i:=0 to Rel[j].Count-1 do
            if(Not(Rel[j][i]=TEERTable(Source).Rel[j][i]))then
            begin
              ObjIsEqualTo:=False;
              exit;
            end;
        end
        else
          ObjIsEqualTo:=False;
      end;


    end;

end;

// -----------------------------------------------
// Implementation of the EER-Rel

constructor TEERRel.Create(AOwner: TComponent; TheName: string);
begin
  inherited Create(AOwner);

  Parent:=TWidgetControl(AOwner);
  Name:=DMMain.GetValidObjectName(TheName);
  Visible:=False;

  Font.Name:=ParentEERModel.DefModelFont;


  ObjName:=TheName;
  relDirection:=0;
  MidOffset:=0;
  CaptionOffset.X:=0;
  CaptionOffset.Y:=0;

  Invisible:=False;
  Splitted:=False;

  OptionalStart:=False;
  OptionalEnd:=False;

  CreateRefDef:=ParentEERModel.ActivateRefDefForNewRelations;
  RefDef:=TStringList.Create;
  RefDef.Add('Matching=0'); //Full Matching
  RefDef.Add('OnDelete=3'); //No Action
  RefDef.Add('OnUpdate=3'); //No Action

  FKRefDefIndex_Obj_id:=-1;

  FKFields:=TStringList.Create;
  FKFieldsComments:=TStringList.Create;

  PopupMenu:=ParentEERModel.PopupMenuEERRelation;


  //-----------------------------------------------
  // Create End Paint Box

  RelEnd:=TPaintBox.Create(AOwner);
  RelEnd.Parent:=TWidgetControl(AOwner);
  RelEnd.Name:=TheName+'_RelEnd';
  RelEnd.Visible:=False;

  // Assign MouseAction procedures
  RelEnd.OnMouseDown:=DoMouseDown;
  RelEnd.OnMouseMove:=DoMouseMove;
  RelEnd.OnMouseUp:=DoMouseUp;
  RelEnd.OnDblClick:=DoDblClick;
  // Assign Paint procedure
  RelEnd.OnPaint:=DoPaint_RelEnd;

  RelEnd.PopupMenu:=ParentEERModel.PopupMenuEERRelation;


  //-----------------------------------------------
  // Create Middle Paint Box

  RelMiddle:=TPaintBox.Create(AOwner);
  RelMiddle.Parent:=TWidgetControl(AOwner);
  RelMiddle.Name:=TheName+'_RelMiddle';
  RelMiddle.Visible:=False;

  // Assign MouseAction procedures
  RelMiddle.OnMouseDown:=DoMouseDown;
  RelMiddle.OnMouseMove:=DoMouseMove;
  RelMiddle.OnMouseUp:=DoMouseUp;
  RelMiddle.OnDblClick:=DoDblClick;
  // Assign Paint procedure
  RelMiddle.OnPaint:=DoPaint_RelMiddle;

  RelMiddle.PopupMenu:=ParentEERModel.PopupMenuEERRelation;


  //-----------------------------------------------
  // Create Caption Paint Box

  //Create Relation-Caption
  RelCaption:=TPaintBox.Create(AOwner);
  RelCaption.Parent:=TWidgetControl(AOwner);
  RelCaption.Name:=TheName+'_RelCaption';
  RelCaption.Visible:=False;
  RelCaption.Font.Name:=ParentEERModel.DefModelFont;
  RelCaption.Canvas.Font.Name:=ParentEERModel.DefModelFont;

  // Assign MouseAction procedures
  RelCaption.OnMouseDown:=DoMouseDown;
  RelCaption.OnMouseMove:=DoMouseMove;
  RelCaption.OnMouseUp:=DoMouseUp;
  RelCaption.OnDblClick:=DoDblClick;
  // Assign Paint procedure
  RelCaption.OnPaint:=DoPaint_Caption;

  RelCaption.PopupMenu:=ParentEERModel.PopupMenuEERRelation;

  //-----------------------------------------------
  // Create StartInterval and EndInterval Paint Box

  RelStartInterval:=TPaintBox.Create(AOwner);
  RelStartInterval.Parent:=TWidgetControl(AOwner);
  RelStartInterval.Name:=TheName+'_RelStartInterval';
  RelStartInterval.Visible:=False;
  RelStartInterval.Font.Name:=ParentEERModel.DefModelFont;
  RelStartInterval.Canvas.Font.Name:=ParentEERModel.DefModelFont;

  // Assign MouseAction procedures
  RelStartInterval.OnMouseDown:=DoMouseDown;
  RelStartInterval.OnMouseMove:=DoMouseMove;
  RelStartInterval.OnMouseUp:=DoMouseUp;
  RelStartInterval.OnDblClick:=DoDblClick;
  // Assign Paint procedure
  RelStartInterval.OnPaint:=DoPaint_StartInterval;

  RelStartInterval.PopupMenu:=ParentEERModel.PopupMenuEERRelation;


  RelEndInterval:=TPaintBox.Create(AOwner);
  RelEndInterval.Parent:=TWidgetControl(AOwner);
  RelEndInterval.Name:=TheName+'_RelEndInterval';
  RelEndInterval.Visible:=False;
  RelEndInterval.Font.Name:=ParentEERModel.DefModelFont;
  RelEndInterval.Canvas.Font.Name:=ParentEERModel.DefModelFont;

  // Assign MouseAction procedures
  RelEndInterval.OnMouseDown:=DoMouseDown;
  RelEndInterval.OnMouseMove:=DoMouseMove;
  RelEndInterval.OnMouseUp:=DoMouseUp;
  RelEndInterval.OnDblClick:=DoDblClick;
  // Assign Paint procedure
  RelEndInterval.OnPaint:=DoPaint_EndInterval;

  RelEndInterval.PopupMenu:=ParentEERModel.PopupMenuEERRelation;
end;

destructor TEERRel.Destroy;
begin
  RefDef.Free;

  FKFields.Free;
  FKFieldsComments.Free;

  inherited;
end;

procedure TEERRel.Assign(Source: TPersistent);
var theSourceRel: TEERRel;
begin
  inherited Assign(Source);

  if Source is TEERRel then
  begin
    theSourceRel:=TEERRel(Source);

    RelKind:=theSourceRel.RelKind;

    SrcTbl:=theSourceRel.SrcTbl;
    DestTbl:=theSourceRel.DestTbl;

    FKFields.Text:=theSourceRel.FKFields.Text;
    FKFieldsComments.Text:=theSourceRel.FKFieldsComments.Text;

    CreateRefDef:=theSourceRel.CreateRefDef;
    RefDef.Text:=theSourceRel.RefDef.Text;
    FKRefDefIndex_Obj_id:=theSourceRel.FKRefDefIndex_Obj_id;

    Invisible:=theSourceRel.Invisible;
    Splitted:=theSourceRel.Splitted;

    OptionalStart:=theSourceRel.OptionalStart;
    OptionalEnd:=theSourceRel.OptionalEnd;

    relDirection:=theSourceRel.relDirection;
    relMidDirection:=theSourceRel.relMidDirection;

    MidOffset:=theSourceRel.MidOffset;
    tempMidOffset:=theSourceRel.tempMidOffset;
    CaptionOffset:=theSourceRel.CaptionOffset;
    tempOffset:=theSourceRel.tempOffset;
    StartIntervalOffset:=theSourceRel.StartIntervalOffset;
    EndIntervalOffset:=theSourceRel.EndIntervalOffset;
  end;
end;

procedure TEERRel.DeleteObj;
var i: integer;
begin
  //Log the Delete-Action if the Flag is set
  if(ParentEERModel.LogActions)then
    ParentEERModel.LogSubAction(at_DeleteObj, Obj_id, GetObjAsXMLModel);

  RelMiddle.Free;
  RelEnd.Free;
  RelCaption.Free;
  RelStartInterval.Free;
  RelEndInterval.Free;

  if(Assigned(SrcTbl))then
  begin
    SrcTbl.RelStart.Delete(SrcTbl.RelStart.IndexOf(self));

    SrcTbl.Rel[relDirection].Delete(
      SrcTbl.Rel[relDirection].IndexOf(self));
  end;

  if(Assigned(DestTbl))then
  begin
    if(SrcTbl<>DestTbl)then
      DestTbl.Rel[ReverseRelDirection(relDirection)].Delete(
        DestTbl.Rel[ReverseRelDirection(relDirection)].IndexOf(self))
    else
      DestTbl.Rel[relDirection].Delete(
        DestTbl.Rel[relDirection].IndexOf(self));

    DestTbl.RelEnd.Delete(DestTbl.RelEnd.IndexOf(self));
    DestTbl.RefreshRelations;

    //If there is an FK Index in the Dest Table, delete it,
    //but be aware of deleted parent and ModelIsBeingCleared flag
    if(FKRefDefIndex_Obj_id>-1)then
    begin
      try
        if(Assigned(ParentEERModel))then
          if(Not(ParentEERModel.ModelIsBeingCleared))then
          begin
            for i:=0 to DestTbl.Indices.Count-1 do
              if(TEERIndex(DestTbl.Indices[i]).Obj_id=FKRefDefIndex_Obj_id)then
              begin
                DestTbl.Indices.Delete(i);
                break;
              end;
            FKRefDefIndex_Obj_id:=-1;
          end;
      except
      end;
    end;


    DestTbl.RefreshObj;
    DestTbl.DoPaint(self);
  end;

  if(Assigned(SrcTbl))then
    if(SrcTbl<>DestTbl)then
    begin
      SrcTbl.RefreshRelations;
      SrcTbl.RefreshObj;
      SrcTbl.DoPaint(self);
    end;


  //Check ALL Foreign Keys / Indices for FKs
  ParentEERModel.CheckAllRelations;

  Free;
end;

procedure TEERRel.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(Button=mbLeft)then
  begin
    //Make shure that no TextImput has the focus
    if(Application.MainForm.ActiveControl<>nil)then
      Application.MainForm.ActiveControl:=nil;
  
    mouse_absx:=Mouse.CursorPos.X;
    mouse_absy:=Mouse.CursorPos.Y;

    // If Worktool wtPointer is the current tool,
    // select or deselect tbl
    if(DMEER.CurrentWorkTool=wtPointer)then
    begin
      if(not(ssCtrl in Shift)and(not(ssShift in Shift)))then
        ParentEERModel.DeSelectAllObjs(nil);

      SetSelected(Not(Selected));

      DoPaint(self);
    end;


    if(DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign))and
      (Copy(TPaintBox(Sender).Name,
        Length(TPaintBox(Sender).Name)-8, 9)='RelMiddle')then
    begin
      mouse_posx:=Left;
      mouse_posy:=Top;

      tempMidOffset:=MidOffset;

      MouseIsDown:=True;
    end;

    if((DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign)))and
      (Copy(TPaintBox(Sender).Name,
        Length(TPaintBox(Sender).Name)-9, 10)='RelCaption')then
    begin
      mouse_posx:=Left;
      mouse_posy:=Top;

      tempOffset.X:=CaptionOffset.X;
      tempOffset.Y:=CaptionOffset.Y;

      MouseIsDown:=True;
    end;

    if((DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign)))and
      (Copy(TPaintBox(Sender).Name,
        Length(TPaintBox(Sender).Name)-15, 16)='RelStartInterval')then
    begin
      mouse_posx:=Left;
      mouse_posy:=Top;

      tempOffset.X:=StartIntervalOffset.X;
      tempOffset.Y:=StartIntervalOffset.Y;

      MouseIsDown:=True;
    end;

    if((DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign)))and
      (Copy(TPaintBox(Sender).Name,
        Length(TPaintBox(Sender).Name)-13, 14)='RelEndInterval')then
    begin
      mouse_posx:=Left;
      mouse_posy:=Top;

      tempOffset.X:=EndIntervalOffset.X;
      tempOffset.Y:=EndIntervalOffset.Y;

      MouseIsDown:=True;
    end;



    // If Worktool Hand is the current tool, store scrollbar pos
    if(DMEER.CurrentWorkTool=wtHand)then
    begin
      if(Assigned(TForm(parent.parent).HorzScrollBar))then
        mouse_posx:=TForm(parent.parent).HorzScrollBar.Position
      else
        mouse_posx:=0;

      if(Assigned(TForm(parent.parent).VertScrollBar))then
        mouse_posy:=TForm(parent.parent).VertScrollBar.Position
      else
        mouse_posy:=0;

      MouseIsDown:=True;
    end;

    if(DMEER.CurrentWorkTool=wtZoomIn)then
      ParentEERModel.ZoomIn(Left+X, Top+Y);

    if(DMEER.CurrentWorkTool=wtZoomOut)then
      ParentEERModel.ZoomOut(Left+X, Top+Y);
  end;
end;

procedure TEERRel.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  ParentEERModel.ClearMouseOverObj;
  
  // If Worktool wtMove is the current tool, move obj
  if(Shift=[ssLeft])and(MouseIsDown)and((DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign)))then
  begin
    if(Copy(TPaintBox(Sender).Name,
      Length(TPaintBox(Sender).Name)-8, 9)='RelMiddle')then
    begin
      if(RelDirection=re_left)or(RelDirection=re_right)then
        MidOffset:=tempMidOffset+ReEvalZoomFac(Mouse.CursorPos.X-mouse_absx)
      else
        MidOffset:=tempMidOffset+ReEvalZoomFac(Mouse.CursorPos.Y-mouse_absy);
    end;

    if(Copy(TPaintBox(Sender).Name,
      Length(TPaintBox(Sender).Name)-9, 10)='RelCaption')then
    begin
      CaptionOffset.X:=tempOffset.X-ReEvalZoomFac(Mouse.CursorPos.X-mouse_absx);
      CaptionOffset.Y:=tempOffset.Y-ReEvalZoomFac(Mouse.CursorPos.Y-mouse_absy);
    end;

    if(Copy(TPaintBox(Sender).Name,
      Length(TPaintBox(Sender).Name)-15, 16)='RelStartInterval')then
    begin
      StartIntervalOffset.X:=tempOffset.X-ReEvalZoomFac(Mouse.CursorPos.X-mouse_absx);
      StartIntervalOffset.Y:=tempOffset.Y-ReEvalZoomFac(Mouse.CursorPos.Y-mouse_absy);
    end;

    if(Copy(TPaintBox(Sender).Name,
      Length(TPaintBox(Sender).Name)-13, 14)='RelEndInterval')then
    begin
      EndIntervalOffset.X:=tempOffset.X-ReEvalZoomFac(Mouse.CursorPos.X-mouse_absx);
      EndIntervalOffset.Y:=tempOffset.Y-ReEvalZoomFac(Mouse.CursorPos.Y-mouse_absy);
    end;

    RefreshObj;

    ParentEERModel.ModelHasChanged;
  end
  // If Worktool Hand is the current tool, scroll the area
  else if(Shift=[ssLeft])and(MouseIsDown)and(DMEER.CurrentWorkTool=wtHand)then
  begin
    if(TForm(parent.parent).HorzScrollBar<>nil)then
    begin
      TForm(parent.parent).HorzScrollBar.Position:=
        mouse_posx+(Mouse.CursorPos.X-mouse_absx)*-1;
{$IFDEF LINUX}
      if(TForm(parent.parent).HorzScrollBar.Position<0)then
        TForm(parent.parent).HorzScrollBar.Position:=0;
      if(TForm(parent.parent).HorzScrollBar.Position>TForm(parent.parent).HorzScrollBar.Range-TForm(parent.parent).ClientWidth)then
        TForm(parent.parent).HorzScrollBar.Position:=TForm(parent.parent).HorzScrollBar.Range-TForm(parent.parent).ClientWidth;
{$ENDIF}
    end;

    if(TForm(parent.parent).VertScrollBar<>nil)then
    begin
      TForm(parent.parent).VertScrollBar.Position:=
        mouse_posy+(Mouse.CursorPos.Y-mouse_absy)*-1;
{$IFDEF LINUX}
      if(TForm(parent.parent).VertScrollBar.Position<0)then
        TForm(parent.parent).VertScrollBar.Position:=0;
      if(TForm(parent.parent).VertScrollBar.Position>TForm(parent.parent).VertScrollBar.Range-TForm(parent.parent).ClientHeight)then
        TForm(parent.parent).VertScrollBar.Position:=TForm(parent.parent).VertScrollBar.Range-TForm(parent.parent).ClientHeight;
{$ENDIF}
    end;
  end;

end;

procedure TEERRel.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if(Button=mbLeft)then
  begin
    MouseIsDown:=False;

    if(DMEER.CurrentWorkTool=wtDelete)then
    begin
      QApplication_postEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_DeleteObject, self));
      Exit;
    end;

    if(Not(ParentEERModel.DisableModelRefresh))then
      DMEER.RefreshInfoPalette;
  end;
end;

procedure TEERRel.ShowEditor(Sender: TObject);
begin
  //Show Relation Editor
  if(DMEER.CurrentWorkTool=wtPointer)or
    (DMEER.CurrentWorkTool=wtMove)then
  begin
    MouseIsDown:=False;

    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_EditRel, self));

    //Process Mouse Messages before setting EditorIsCalled to false
    Application.ProcessMessages;

    EditorIsCalled:=False;
  end;
end;

procedure TEERRel.PaintObj2(theCanvas: TCanvas; theZoomfac: double = -1;
  x: integer = 0; y: integer = 0; w: integer = 0; h: integer = 0; SubObj: integer = 1);
var
  xo, yo: integer;
  prevZoomfac: double;
  prevRelIconSize: integer;
begin
  if(DMEER.DisablePaint)and(thezoomfac=-1)then
    Exit;

  if(Invisible)then
    Exit;

  xo:=0;
  yo:=0;

  //Set zoom factor to specified value
  prevZoomfac:=ParentEERModel.Zoomfac;
  prevRelIconSize:=ParentEERModel.RelIconSize;

  if(thezoomfac>-1)then
  begin
    ParentEERModel.Zoomfac:=thezoomfac;

    //Relation Icon Size
    if(DMEER.Notation=noErwin)then
      ParentEERModel.RelIconSize:=EvalZoomFac(9)
    else
      ParentEERModel.RelIconSize:=EvalZoomFac(18);
    ParentEERModel.RelIconDSize:=Trunc(ParentEERModel.RelIconSize/2);

    RefreshObj;
  end;

  try
    if(theCanvas<>Canvas)then
    begin
      case SubObj of
        0:
          begin
            xo:=Left-x;
            yo:=Top-y;
          end;
        1:
          begin
            xo:=RelMiddle.Left-x;
            yo:=RelMiddle.Top-y;
          end;
        2:
          begin
            xo:=RelEnd.Left-x;
            yo:=RelEnd.Top-y;
          end;
        3:
          begin
            xo:=RelCaption.Left-x;
            yo:=RelCaption.Top-y;
          end;
        4:
          begin
            xo:=RelStartInterval.Left-x;
            yo:=RelStartInterval.Top-y;
          end;
        5:
          begin
            xo:=RelEndInterval.Left-x;
            yo:=RelEndInterval.Top-y;
          end;
      end;

      {xo:=EvalZoomFac(Obj_X)-x;
      yo:=EvalZoomFac(Obj_Y)-y;}

      //Check if obj in Draw-Area
      if(w>0)and((xo+w<0)or(xo>w)or(yo+h<0)or(yo>h))then
        Exit;
    end;

    //Do the real drawing
    case SubObj of
      0:
        PaintObj2Canvas_RelStart(theCanvas, xo, yo);
      1:
        PaintObj2Canvas_RelMiddle(theCanvas, xo, yo);
      2:
        PaintObj2Canvas_RelEnd(theCanvas, xo, yo);
      3:
        PaintObj2Canvas_RelCaption(theCanvas, xo, yo);
      4:
        PaintObj2Canvas_RelStartInterval(theCanvas, xo, yo);
      5:
        PaintObj2Canvas_RelEndInterval(theCanvas, xo, yo);
    end;

  finally
    if(thezoomfac>-1)then
    begin
      ParentEERModel.Zoomfac:=prevZoomfac;
      ParentEERModel.RelIconSize:=prevRelIconSize;
      ParentEERModel.RelIconDSize:=Trunc(ParentEERModel.RelIconSize/2);

      RefreshObj;
    end;
  end;
end;


procedure TEERRel.DoPaint(Sender: TObject);
begin
  PaintObj2Canvas_RelStart(Canvas, 0, 0);

  PaintObj2Canvas_RelEnd(RelEnd.Canvas, 0, 0);
  PaintObj2Canvas_RelMiddle(RelMiddle.Canvas, 0, 0);

  if(DMEER.DisplayRelationNames)and(Not(Splitted))then
    PaintObj2Canvas_RelCaption(RelCaption.Canvas, 0, 0);

  if(DMEER.Notation=noStandard2)or(Splitted)then
  begin
    PaintObj2Canvas_RelStartInterval(RelStartInterval.Canvas, 0, 0);
    PaintObj2Canvas_RelEndInterval(RelEndInterval.Canvas, 0, 0);
  end;
end;

procedure TEERRel.DoPaint_RelMiddle(Sender: TObject);
begin
  //Painting is done by DoPaint cause it is called for refreshes
  //PaintObj2Canvas_RelMiddle(RelMiddle.Canvas, 0, 0);
end;

procedure TEERRel.DoPaint_RelEnd(Sender: TObject);
begin
  //Painting is done by DoPaint cause it is called for refreshes
  //PaintObj2Canvas_RelEnd(RelEnd.Canvas, 0, 0);
end;

procedure TEERRel.DoPaint_Caption(Sender: TObject);
begin
  //Painting is done by DoPaint cause it is called for refreshes
  {if(DMMain.DisplayRelationNames)then
    PaintObj2Canvas_RelCaption(RelCaption.Canvas, 0, 0);}
end;

procedure TEERRel.DoPaint_StartInterval(Sender: TObject);
begin
  //Painting is done by DoPaint cause it is called for refreshes
end;

procedure TEERRel.DoPaint_EndInterval(Sender: TObject);
begin
  //Painting is done by DoPaint cause it is called for refreshes
end;

procedure TEERRel.PaintObj2Canvas(theCanvas: TCanvas; xo, yo: integer);
begin
  //
end;

procedure TEERRel.PaintObj2Canvas_RelStart(theCanvas: TCanvas; xo, yo: integer);
var i, j, theBmpNr: integer;
  IconXY: TPoint;
  w, h: integer;
begin
  if(ParentEERModel.DisableModelRefresh)then
    Exit;
    
  if(Invisible)then
    Exit;

  with theCanvas do
  begin
    //Box for debugging
    //Rectangle(xo+0, yo+0, xo+width-1, yo+height-1);

    if(Not(Splitted))then
    begin
      w:=width-1;
      h:=height-1;
    end
    else
    begin
      w:=TEERModel(Parent).EvalZoomFac(25);
      if(w>width-1)then
        w:=width-1;
      h:=TEERModel(Parent).EvalZoomFac(23);
      if(h>height-1)then
        h:=height-1;
    end;


    if(selected)or(Splitted)then
      j:=1
    else
      j:=0;

    for i:=0 to j do
    begin
      //When painting a dashed/dotted line (select, splitt)
      //clear the background first
      if(i=0)and(j=1)then
      begin
        Pen.Color:=clWhite;
        Pen.Style:=psSolid;
      end
      else
      begin
        if(RelKind=rk_1nNonId)or
          (RelKind=rk_11NonId)then
        begin
          if(IsLinkedObject)then
            Pen.Color:=$00C66931
          else
            Pen.Color:=clMid;
        end
        else
        begin
          if(IsLinkedObject)then
            Pen.Color:=$006E3A1B
          else
            Pen.Color:=clBlack;
        end;

        if(Splitted)and(Not(selected))then
          Pen.Style:=psDashDotDot
        else if(selected)then
          Pen.Style:=psDot
        else
          Pen.Style:=psSolid;
      end;

      if(relDirection=re_right)then
      begin
        MoveTo(xo+0, yo+ParentEERModel.RelIconDSize);
        LineTo(xo+w, yo+ParentEERModel.RelIconDSize);

        IconXY:=Point(xo+0, yo+0);
      end
      else if(relDirection=re_left)then
      begin
        MoveTo(xo+width-1, yo+ParentEERModel.RelIconDSize);
        LineTo(xo+width-1-w, yo+ParentEERModel.RelIconDSize);

        IconXY:=Point(xo+width-ParentEERModel.RelIconSize, yo+0);
      end
      else if(relDirection=re_top)then
      begin
        MoveTo(xo+ParentEERModel.RelIconDSize, yo+height-1);
        LineTo(xo+ParentEERModel.RelIconDSize, yo+height-1-h);

        IconXY:=Point(xo+0, yo+height-ParentEERModel.RelIconSize);
      end
      else if(relDirection=re_bottom)then
      begin
        MoveTo(xo+ParentEERModel.RelIconDSize, yo+0);
        LineTo(xo+ParentEERModel.RelIconDSize, yo+h);

        IconXY:=Point(xo+0, yo+0);
      end;
    end;

    Pen.Color:=clBlack;
    Pen.Style:=psSolid;

    //Draw Icon
    if(DMEER.Notation=noErwin)and
      (RelKind=rk_1nNonId)then
      ParentEERModel.DrawRelIcon(theCanvas,
          Rect(IconXY.X, IconXY.Y,
          IconXY.X+ParentEERModel.RelIconSize,
          IconXY.Y+ParentEERModel.RelIconSize),
          0);

    if(DMEER.Notation=noErwin)and
      (RelKind=rk_nm)then
      ParentEERModel.DrawRelIcon(theCanvas,
          Rect(IconXY.X, IconXY.Y,
          IconXY.X+ParentEERModel.RelIconSize,
          IconXY.Y+ParentEERModel.RelIconSize),
          6);

    if((DMEER.Notation=noStandard)or(DMEER.Notation=noStandard2))and
      (RelKind=rk_11Sub)then
    begin
      //Icons 8..11
      theBmpNr:=(relDirection+2) mod 4+8;

      ParentEERModel.DrawRelIcon(theCanvas,
          Rect(IconXY.X, IconXY.Y,
          IconXY.X+ParentEERModel.RelIconSize,
          IconXY.Y+ParentEERModel.RelIconSize),
          theBmpNr);
    end;

    //OptionalIcon
    if(DMEER.Notation=noStandard)and
      (OptionalStart)then
      ParentEERModel.DrawRelIcon(theCanvas,
          Rect(IconXY.X, IconXY.Y,
          IconXY.X+ParentEERModel.RelIconSize,
          IconXY.Y+ParentEERModel.RelIconSize),
          7);

    if(DMEER.Notation=noCrowsFoot)then
      if(width>TEERModel(Parent).EvalZoomFac(10))and
        (height>TEERModel(Parent).EvalZoomFac(10))then
      begin
        //Icons 20..23, 24..27 (OptionalStart)
        theBmpNr:=(relDirection+2) mod 4+20+4*Ord(OptionalStart);

        ParentEERModel.DrawRelIcon(theCanvas,
            Rect(IconXY.X, IconXY.Y,
            IconXY.X+ParentEERModel.RelIconSize,
            IconXY.Y+ParentEERModel.RelIconSize),
            theBmpNr);
      end;
  end;
end;

procedure TEERRel.PaintObj2Canvas_RelMiddle(theCanvas: TCanvas; xo, yo: integer);
var i, theBmpNr: integer;
  IconXY: TPoint;
begin
  if(ParentEERModel.DisableModelRefresh)then
    Exit;

  if(Invisible)or(Splitted)then
    Exit;
    
  with RelMiddle do
  begin
    with theCanvas do
    begin
      //Box for debugging
      //Rectangle(xo+0, yo+0, xo+width-1, yo+height-1);

      if(RelKind=rk_1nNonId)then
        Pen.Color:=clMid;

      for i:=0 to ord(selected) do
      begin
        if(i=0)and(selected)then
        begin
          Pen.Color:=clWhite;
          Pen.Style:=psSolid;
        end
        else
        begin
          if(RelKind=rk_1nNonId)or
            (RelKind=rk_11NonId)then
          begin
            if(IsLinkedObject)then
              Pen.Color:=$00C66931
            else
              Pen.Color:=clMid;
          end
          else
          begin
            if(IsLinkedObject)then
              Pen.Color:=$006E3A1B
            else
              Pen.Color:=clBlack;
          end;

          if(selected)then
            Pen.Style:=psDot
          else
            Pen.Style:=psSolid;
        end;

        if(relDirection=re_right)or(relDirection=re_left)then
        begin
          if(height>1)then
          begin
            MoveTo(xo+ParentEERModel.RelIconDSize, yo+height-1);
            LineTo(xo+ParentEERModel.RelIconDSize, yo+0);
          end;

          IconXY:=Point(xo+0, yo+height div 2-ParentEERModel.RelIconDSize);
        end
        else if(relDirection=re_top)or(relDirection=re_bottom)then
        begin
          if(width>1)then
          begin
            MoveTo(xo+0, yo+ParentEERModel.RelIconDSize);
            LineTo(xo+width-1, yo+ParentEERModel.RelIconDSize);
          end;

          IconXY:=Point(xo+width div 2-ParentEERModel.RelIconDSize, yo+0);
        end
      end;

      Pen.Color:=clBlack;
      Pen.Style:=psSolid;

      //Draw Icon
      if(DMEER.Notation=noStandard)and(RelKind<>rk_11Sub)then
      begin
        theBmpNr:=0;

        if(RelKind=rk_1n)or(RelKind=rk_1nNonId)then
          theBmpNr:=(relMidDirection+2) mod 4+1;

        if(RelKind=rk_nm)then
          theBmpNr:=5;

        ParentEERModel.DrawRelIcon(theCanvas,
          Rect(IconXY.X, IconXY.Y,
          IconXY.X+ParentEERModel.RelIconSize,
          IconXY.Y+ParentEERModel.RelIconSize),
          theBmpNr);
      end;

      if(DMEER.Notation=noStandard2)and(RelKind<>rk_11Sub)then
        ParentEERModel.DrawRelIcon(theCanvas,
          Rect(IconXY.X, IconXY.Y,
          IconXY.X+ParentEERModel.RelIconSize,
          IconXY.Y+ParentEERModel.RelIconSize),
          0);
    end;
  end;
end;

procedure TEERRel.PaintObj2Canvas_RelEnd(theCanvas: TCanvas; xo, yo: integer);
var i, j: integer;
  IconXY: TPoint;
  w, h: integer;
  theBmpNr: integer;
begin
  if(ParentEERModel.DisableModelRefresh)then
    Exit;
    
  if(Invisible)then
    Exit;
    
  with RelEnd do
  begin
    with theCanvas do
    begin
      if(Not(Splitted))then
      begin
        w:=width-1;
        h:=height-1;
      end
      else
      begin
        w:=TEERModel(Parent).EvalZoomFac(25);
        if(w>width-1)then
          w:=width-1;
        h:=TEERModel(Parent).EvalZoomFac(23);
        if(h>height-1)then
          h:=height-1;
      end;

      if(selected)or(Splitted)then
        j:=1
      else
        j:=0;

      if(RelKind=rk_1nNonId)then
        Pen.Color:=clMid;

      //Draw 2nd time for selection
      for i:=0 to j do
      begin
        //When painting a dashed/dotted line (select, splitt)
        //clear the background first
        if(i=0)and(j=1)then
        begin
          Pen.Color:=clWhite;
          Pen.Style:=psSolid;
        end
        else
        begin
          if(RelKind=rk_1nNonId)or
            (RelKind=rk_11NonId)then
          begin
            if(IsLinkedObject)then
              Pen.Color:=$00C66931
            else
              Pen.Color:=clMid;
          end
          else
          begin
            if(IsLinkedObject)then
              Pen.Color:=$006E3A1B
            else
              Pen.Color:=clBlack;
          end;

          if(Splitted)and(Not(selected))then
            Pen.Style:=psDashDotDot
          else if(selected)then
            Pen.Style:=psDot
          else
            Pen.Style:=psSolid;
        end;

        if(relDirection=re_right)then
        begin
          MoveTo(xo+width-1, yo+ParentEERModel.RelIconDSize);
          LineTo(xo+width-1-w, yo+ParentEERModel.RelIconDSize);

          IconXY:=Point(xo+width-ParentEERModel.RelIconSize, yo+0);
        end
        else if(relDirection=re_left)then
        begin
          MoveTo(xo+0, yo+ParentEERModel.RelIconDSize);
          LineTo(xo+w, yo+ParentEERModel.RelIconDSize);

          IconXY:=Point(xo+0, yo+0);
        end
        else if(relDirection=re_top)then
        begin
          MoveTo(xo+ParentEERModel.RelIconDSize, yo+h);
          LineTo(xo+ParentEERModel.RelIconDSize, yo+0);

          IconXY:=Point(xo+0, yo+0);
        end
        else if(relDirection=re_bottom)then
        begin
          MoveTo(xo+ParentEERModel.RelIconDSize, yo+height-1-h);
          LineTo(xo+ParentEERModel.RelIconDSize, yo+height-1);

          IconXY:=Point(xo+0, yo+height-ParentEERModel.RelIconSize);
        end;
      end;

      Pen.Color:=clBlack;
      Pen.Style:=psSolid;

      //Draw Icon
      if(DMEER.Notation=noErwin)and
        ((RelKind=rk_1n)or(RelKind=rk_1nNonId)or(RelKind=rk_nm))then
        ParentEERModel.DrawRelIcon(theCanvas,
          Rect(IconXY.X, IconXY.Y,
          IconXY.X+ParentEERModel.RelIconSize,
          IconXY.Y+ParentEERModel.RelIconSize),
          6);

      //OptionalIcon
      if(DMEER.Notation=noStandard)and
        (OptionalEnd)then
        ParentEERModel.DrawRelIcon(theCanvas,
          Rect(IconXY.X, IconXY.Y,
          IconXY.X+ParentEERModel.RelIconSize,
          IconXY.Y+ParentEERModel.RelIconSize),
          7);

      if(DMEER.Notation=noCrowsFoot)then
        if(width>TEERModel(Parent).EvalZoomFac(10))and
          (height>TEERModel(Parent).EvalZoomFac(10))then
        begin
          //Icons 12..15, 16..19 (OptionalEnd)
          theBmpNr:=(relDirection+2) mod 4+12+4*Ord(OptionalEnd);

          //Fix Crowsfoot bug - by gony, 20031117           
          if(RelKind=rk_11)then
            theBmpNr:=theBmpNr+8;

          ParentEERModel.DrawRelIcon(theCanvas,
              Rect(IconXY.X, IconXY.Y,
              IconXY.X+ParentEERModel.RelIconSize,
              IconXY.Y+ParentEERModel.RelIconSize),
              theBmpNr);
        end;
    end;
  end;
end;

procedure TEERRel.PaintObj2Canvas_RelCaption(theCanvas: TCanvas; xo, yo: integer);
begin
  if(ParentEERModel.DisableModelRefresh)then
    Exit;
    
  if(Invisible)or(Splitted)then
    Exit;
    
  if(RelKind=rk_11Sub)then
    Exit;

  if(Not(DMEER.DisplayRelationNames))then
    Exit;

  with RelCaption do
  begin
    with theCanvas do
    begin
      if(IsLinkedObject)then
        Pen.Color:=$00C66931
      else
        Pen.Color:=clSilver;
      Brush.Color:=clWhite;
      Rectangle(Rect(xo+0, yo+0, xo+width, yo+height));
      Pen.Color:=clSilver;

      if(Not(DMEER.DisableTextOutput))then
      begin
        Font.Height:=ParentEERModel.GetFontHeight;
        TextRect(Rect(xo+1, yo+1, xo+width-2, yo+height-2),
          xo+1+EvalZoomFac(3), yo+1, ObjName);
      end;

      // Paint selection
      if(selected)then
      begin
        Pen.Color:=clWhite;
        MoveTo(xo+0, yo+0);
        LineTo(xo+0, yo+height-1);
        LineTo(xo+width-1, yo+height-1);
        LineTo(xo+width-1, yo+0);
        LineTo(xo+0, yo+0);

        Pen.Color:=clBlack;
        Pen.Style:=psDot;
        MoveTo(xo+0, yo+0);
        LineTo(xo+0, yo+height-1);
        LineTo(xo+width-1, yo+height-1);
        LineTo(xo+width-1, yo+0);
        LineTo(xo+0, yo+0);

        Pen.Style:=psSolid;
      end;
    end;
  end;
end;

function TEERRel.GetIntervalTxt(StartInterval: Boolean): string;
var i1, i2: string;
begin
  if(Not(Splitted))then
  begin
    if(StartInterval)then
    begin
      if(OptionalEnd)then
        i1:='0'
      else
        i1:='1';

      if(relKind=rk_11)or(relKind=rk_11Sub)or(relKind=rk_11NonId)then
        i2:='1'
      else
        i2:='*';
    end
    else
    begin
      if(OptionalStart)then
        i1:='0'
      else
        i1:='1';

      if(relKind=rk_11)or(relKind=rk_11Sub)or(relKind=rk_11NonId)or(relKind=rk_1n)or(relKind=rk_1nNonId)then
        i2:='1'
      else
        i2:='*';
    end;

    GetIntervalTxt:='['+i1+', '+i2+']';
  end
  else
    GetIntervalTxt:=ObjName;
end;

procedure TEERRel.PaintObj2Canvas_RelStartInterval(theCanvas: TCanvas; xo, yo: integer);
begin
  if(ParentEERModel.DisableModelRefresh)then
    Exit;
    
  if(Invisible)then
    Exit;
    
  if((Not(DMEER.Notation=noStandard2))or(RelKind=rk_11Sub))and
    (Not(Splitted))then
    Exit;

  with RelStartInterval do
  begin
    with theCanvas do
    begin
      if(IsLinkedObject)then
        Pen.Color:=$00C66931
      else
        Pen.Color:=clSilver;
      Brush.Color:=clWhite;
      Rectangle(Rect(xo+0, yo+0, xo+width, yo+height));

      if(Not(DMEER.DisableTextOutput))then
      begin
        Font.Height:=ParentEERModel.GetFontHeight;
        TextRect(Rect(xo+1, yo+1, xo+width-2, yo+height-2),
          xo+1+EvalZoomFac(3), yo+1, GetIntervalTxt(True));
      end;

      // Paint selection
      if(selected)then
      begin
        Pen.Color:=clWhite;
        MoveTo(xo+0, yo+0);
        LineTo(xo+0, yo+height-1);
        LineTo(xo+width-1, yo+height-1);
        LineTo(xo+width-1, yo+0);
        LineTo(xo+0, yo+0);

        Pen.Color:=clBlack;
        Pen.Style:=psDot;
        MoveTo(xo+0, yo+0);
        LineTo(xo+0, yo+height-1);
        LineTo(xo+width-1, yo+height-1);
        LineTo(xo+width-1, yo+0);
        LineTo(xo+0, yo+0);

        Pen.Style:=psSolid;
      end;
    end;
  end;
end;

procedure TEERRel.PaintObj2Canvas_RelEndInterval(theCanvas: TCanvas; xo, yo: integer);
begin
  if(ParentEERModel.DisableModelRefresh)then
    Exit;
    
  if(Invisible)then
    Exit;
    
  if((Not(DMEER.Notation=noStandard2))or(RelKind=rk_11Sub))and
    (Not(Splitted))then
    Exit;

  with RelEndInterval do
  begin
    with theCanvas do
    begin
      if(IsLinkedObject)then
        Pen.Color:=$00C66931
      else
        Pen.Color:=clSilver;
      Brush.Color:=clWhite;
      Rectangle(Rect(xo+0, yo+0, xo+width, yo+height));

      if(Not(DMEER.DisableTextOutput))then
      begin
        Font.Height:=ParentEERModel.GetFontHeight;
        TextRect(Rect(xo+1, yo+1, xo+width-2, yo+height-2),
          xo+1+EvalZoomFac(3), yo+1, GetIntervalTxt(False));
      end;

      // Paint selection
      if(selected)then
      begin
        Pen.Color:=clWhite;
        MoveTo(xo+0, yo+0);
        LineTo(xo+0, yo+height-1);
        LineTo(xo+width-1, yo+height-1);
        LineTo(xo+width-1, yo+0);
        LineTo(xo+0, yo+0);

        Pen.Color:=clBlack;
        Pen.Style:=psDot;
        MoveTo(xo+0, yo+0);
        LineTo(xo+0, yo+height-1);
        LineTo(xo+width-1, yo+height-1);
        LineTo(xo+width-1, yo+0);
        LineTo(xo+0, yo+0);

        Pen.Style:=psSolid;
      end;
    end;
  end;
end;

function SortRelations(Item1, Item2: Pointer): Integer;
var tbl1, tbl2: TEERTable;
  res: integer;
begin
  if(Assigned(Item1))and(Assigned(Item2))then
  begin
    if(TEERRel(Item1).SrcTbl=DMEER.CompareTable)then
      tbl1:=TEERRel(Item1).DestTbl
    else
      tbl1:=TEERRel(Item1).SrcTbl;

    if(TEERRel(Item2).SrcTbl=DMEER.CompareTable)then
      tbl2:=TEERRel(Item2).DestTbl
    else
      tbl2:=TEERRel(Item2).SrcTbl;

    if(TEERRel(Item1).relDirection=re_Left)or(TEERRel(Item1).relDirection=re_Right)then
      res:=tbl1.Obj_Y-tbl2.Obj_Y
    else
      res:=tbl1.Obj_X-tbl2.Obj_X;

    if(res=0)then
      res:=CompareText(TEERRel(Item1).ObjName, TEERRel(Item2).ObjName);

    SortRelations:=res;
  end
  else
    SortRelations:=0;
end;

// Caution: if Sender<>nil the RefreshObj WILL NOT call itself over the
// RefreshRelations function preventing infinite recursive loop
procedure TEERRel.RefreshObj(Sender: TObject = nil);
var theRelDir: integer;
  dx, dy: double;
  soffset, doffset: integer;
  theSize: TSize;
  oldpos: integer;
  i, j: integer;
  PKfound: Boolean;
  mappedColName: string;
begin
  theRelDir:=GetRelDirection;

  //Check if all PKs of the source Table are still in the SrcField List
  for i:=0 to SrcTbl.Columns.Count-1 do
    if(TEERColumn(SrcTbl.Columns[i]).PrimaryKey)then
      if(FKFields.IndexOfName(TEERColumn(SrcTbl.Columns[i]).ColName)=-1)and
        (TEERColumn(SrcTbl.Columns[i]).ColName<>'')then
      begin
        //Build column name mapping
        if(ParentEERModel.TableNameInRefs)then
        begin
          //Prevent recursive mappings like tablename_tablename_tablename_id
          if(Copy(TEERColumn(SrcTbl.Columns[i]).ColName, 1, Length(SrcTbl.ObjName+'_'))=
            SrcTbl.ObjName+'_')then
            mappedColName:=TEERColumn(SrcTbl.Columns[i]).ColName
          else
            mappedColName:=SrcTbl.ObjName+'_'+
              ParentEERModel.FKPrefix+TEERColumn(SrcTbl.Columns[i]).ColName+ParentEERModel.FKPostfix;
        end
        else
          mappedColName:=ParentEERModel.FKPrefix+TEERColumn(SrcTbl.Columns[i]).ColName+ParentEERModel.FKPostfix;

        //check if source table equals destination table
        //and prevent same name
        if(SrcTbl=DestTbl)and
          (TEERColumn(SrcTbl.Columns[i]).ColName=mappedColName)then
          mappedColName:=mappedColName+'_2';

        FKFields.Add(TEERColumn(SrcTbl.Columns[i]).ColName+'='+mappedColName);

        FKFieldsComments.Add('');
      end;

  //Check if all SrcFields are still PKs
  j:=0;
  while(j<FKFields.Count)do
  begin
    PKfound:=False;
    for i:=0 to SrcTbl.Columns.Count-1 do
    begin
      if(TEERColumn(SrcTbl.Columns[i]).PrimaryKey)and
        (CompareText(TEERColumn(SrcTbl.Columns[i]).ColName, FKFields.Names[j])=0)then
      begin
        PKfound:=True;
        break;
      end;
    end;

    if(Not(PKfound))then
    begin
      FKFields.Delete(j);
      FKFieldsComments.Delete(j);
    end
    else
      inc(j);
  end;

  //Check if the Relation is stil on same side
  if(theRelDir<>relDirection)then
  begin
    if(relDirection>0)then
    begin
      //if not, remove from this side
      if(TEERTable(SrcTbl).Rel[relDirection].IndexOf(self)>-1)then
      begin
        TEERTable(SrcTbl).Rel[relDirection].Delete(
          TEERTable(SrcTbl).Rel[relDirection].IndexOf(self));
      end;

      if(SrcTbl<>DestTbl)then
      begin
        if(TEERTable(DestTbl).Rel[ReverseRelDirection(relDirection)].IndexOf(self)>-1)then
          TEERTable(DestTbl).Rel[ReverseRelDirection(relDirection)].Delete(
            TEERTable(DestTbl).Rel[ReverseRelDirection(relDirection)].IndexOf(self))
      end
      else
      begin
        if(TEERTable(DestTbl).Rel[relDirection].IndexOf(self)>-1)then
          TEERTable(DestTbl).Rel[relDirection].Delete(
            TEERTable(DestTbl).Rel[relDirection].IndexOf(self));
      end;

    end;

    relDirection:=theRelDir;
    //MidOffset:=0;

    //Add to new side
    TEERTable(SrcTbl).Rel[relDirection].Add(self);

    if(SrcTbl<>DestTbl)then
      TEERTable(DestTbl).Rel[ReverseRelDirection(relDirection)].Add(self)
    else
      TEERTable(DestTbl).Rel[relDirection].Add(self);


    TEERTable(SrcTbl).RefreshRelations;
    TEERTable(DestTbl).RefreshRelations;
  end;

  //Set Visibility RelStart
  if(Visible<>Not(Invisible))then
    Visible:=Not(Invisible);
  //Set Visibility RelEnd
  if(RelEnd.Visible<>Not(Invisible))then
    RelEnd.Visible:=Not(Invisible);
  //Set Visibility RelMiddle
  if(RelMiddle.Visible<>Not(Invisible)and
    Not(Splitted))then
    RelMiddle.Visible:=Not(Invisible)and
      Not(Splitted);
  //Set Visibility RelCaption
  if(RelCaption.Visible<>Not(Invisible)and
    Not(Splitted))then
  begin
    RelCaption.Visible:=Not(Invisible)and
      Not(Splitted);
    RelCaption.BringToFront;
  end;
  //Set Visibility RelStartInterval
  if(RelStartInterval.Visible<>Not(Invisible)and
      (((DMEER.Notation=noStandard2)and
      (Not(RelStartInterval.Visible))and
      (Not(RelKind=rk_11Sub)))or
      (Splitted)))then
  begin
    RelStartInterval.Visible:=Not(Invisible)and
      (((DMEER.Notation=noStandard2)and
      (Not(RelStartInterval.Visible))and
      (Not(RelKind=rk_11Sub)))or
      (Splitted));
    RelStartInterval.BringToFront;
  end;
  //Set Visibility RelEndInterval
  if(RelEndInterval.Visible<>Not(Invisible)and
    (((DMEER.Notation=noStandard2)and
    (Not(RelKind=rk_11Sub)))or
    (Splitted)))then
  begin
    RelEndInterval.Visible:=Not(Invisible)and
      (((DMEER.Notation=noStandard2)and
      (Not(RelKind=rk_11Sub)))or
      (Splitted));
    RelEndInterval.BringToFront;
  end;


  //Reorder Relations on SourceTable side
  oldpos:=SrcTbl.Rel[relDirection].IndexOf(self);
  DMEER.CompareTable:=SrcTbl;
  SrcTbl.Rel[relDirection].Sort(@SortRelations);
  if(oldpos<>SrcTbl.Rel[relDirection].IndexOf(self))and
    (Sender=nil)then
    SrcTbl.RefreshRelations(True);

  //Reorder Relations on DestTable side
  oldpos:=DestTbl.Rel[ReverseRelDirection(relDirection)].IndexOf(self);
  DMEER.CompareTable:=DestTbl;
  DestTbl.Rel[ReverseRelDirection(relDirection)].Sort(@SortRelations);
  if(oldpos<>DestTbl.Rel[ReverseRelDirection(relDirection)].IndexOf(self))and
    (Sender=nil)then
    DestTbl.RefreshRelations(True);

  // --------------------------------------------------

  //if there is more than one relation on a side, get the offset for
  //the current relation
  soffset:=TEERTable(SrcTbl).GetRelationOffset(self);
  if(SrcTbl<>DestTbl)then
    doffset:=TEERTable(DestTbl).GetRelationOffset(self)
  else
    doffset:=round(
        (Obj_H/
          (TEERTable(DestTbl).Rel[relDirection].Count+1)
        )*
        (TEERTable(DestTbl).Rel[relDirection].IndexOf(self)+2)-(Obj_H div 2)
      );

  dx:=(TEERTable(SrcTbl).Obj_X+TEERTable(SrcTbl).Obj_W/2+soffset)-
    (TEERTable(DestTbl).Obj_X+TEERTable(DestTbl).Obj_W/2+doffset);
  dy:=(TEERTable(SrcTbl).Obj_Y+TEERTable(SrcTbl).Obj_H/2+soffset)-
    (TEERTable(DestTbl).Obj_Y+TEERTable(DestTbl).Obj_H/2+doffset);

  //Right
  if(relDirection=re_right)then
  begin
    Obj_X:=TEERTable(SrcTbl).Obj_X+TEERTable(SrcTbl).Obj_W;
    if(SrcTbl<>DestTbl)then
      Obj_W:=TEERTable(DestTbl).Obj_X-Obj_X
    else
      Obj_W:=50;

    //Top
    if(dy>0)then
    begin
      relMidDirection:=re_top;
      Obj_Y:=TEERTable(DestTbl).Obj_Y+TEERTable(DestTbl).Obj_H div 2+doffset;
      Obj_H:=TEERTable(SrcTbl).Obj_Y+TEERTable(SrcTbl).Obj_H div 2-Obj_Y+soffset;
    end
    //Bottom
    else
    begin
      relMidDirection:=re_bottom;
      Obj_Y:=TEERTable(SrcTbl).Obj_Y+TEERTable(SrcTbl).Obj_H div 2+soffset;
      Obj_H:=TEERTable(DestTbl).Obj_Y+TEERTable(DestTbl).Obj_H div 2-Obj_Y+doffset;
    end;

    //for standard Notation, if there start and end are at approximatly same pos
    if(abs(dy)<ParentEERModel.RelIconSize)then
      relMidDirection:=re_right;

    Obj_Y:=Obj_Y-ReEvalZoomFac(ParentEERModel.RelIconDSize);
    Obj_H:=Obj_H+ReEvalZoomFac(ParentEERModel.RelIconSize);
  end
  //Left
  else if(relDirection=re_left)then
  begin
    Obj_X:=TEERTable(DestTbl).Obj_X+TEERTable(DestTbl).Obj_W;
    Obj_W:=TEERTable(SrcTbl).Obj_X-Obj_X;

    //Top
    if(dy>0)then
    begin
      relMidDirection:=re_top;
      Obj_Y:=TEERTable(DestTbl).Obj_Y+TEERTable(DestTbl).Obj_H div 2+doffset;
      Obj_H:=TEERTable(SrcTbl).Obj_Y+TEERTable(SrcTbl).Obj_H div 2-Obj_Y+soffset;
    end
    //Bottom
    else
    begin
      relMidDirection:=re_bottom;
      Obj_Y:=TEERTable(SrcTbl).Obj_Y+TEERTable(SrcTbl).Obj_H div 2+soffset;
      Obj_H:=TEERTable(DestTbl).Obj_Y+TEERTable(DestTbl).Obj_H div 2-Obj_Y+doffset;
    end;

    //for standard Notation, if there start and end are at approximatly same pos
    if(abs(dy)<ParentEERModel.RelIconSize)then
      relMidDirection:=re_left;

    Obj_Y:=Obj_Y-ReEvalZoomFac(ParentEERModel.RelIconDSize);
    Obj_H:=Obj_H+ReEvalZoomFac(ParentEERModel.RelIconSize);
  end
  //Top
  else if(relDirection=re_top)then
  begin
    if(dx>0)then
    begin
      relMidDirection:=re_left;
      Obj_X:=TEERTable(DestTbl).Obj_X+TEERTable(DestTbl).Obj_W div 2+doffset;
      Obj_W:=TEERTable(SrcTbl).Obj_X+TEERTable(SrcTbl).Obj_W div 2-Obj_X+soffset;
    end
    else
    begin
      relMidDirection:=re_right;
      Obj_X:=TEERTable(SrcTbl).Obj_X+TEERTable(SrcTbl).Obj_W div 2+soffset;
      Obj_W:=TEERTable(DestTbl).Obj_X+TEERTable(DestTbl).Obj_W div 2-Obj_X+doffset;
    end;

    //for standard Notation, if there start and end are at approximatly same pos
    if(abs(dx)<ParentEERModel.RelIconSize)then
      relMidDirection:=re_top;

    Obj_Y:=TEERTable(DestTbl).Obj_Y+TEERTable(DestTbl).Obj_H;
    Obj_H:=TEERTable(SrcTbl).Obj_Y-Obj_Y;

    Obj_X:=Obj_X-ReEvalZoomFac(ParentEERModel.RelIconDSize);
    Obj_W:=Obj_W+ReEvalZoomFac(ParentEERModel.RelIconSize);
  end
  //Bottom
  else
  begin
    if(dx>0)then
    begin
      relMidDirection:=re_left;
      Obj_X:=TEERTable(DestTbl).Obj_X+TEERTable(DestTbl).Obj_W div 2+doffset;
      Obj_W:=TEERTable(SrcTbl).Obj_X+TEERTable(SrcTbl).Obj_W div 2-Obj_X+soffset;
    end
    else
    begin
      relMidDirection:=re_right;
      Obj_X:=TEERTable(SrcTbl).Obj_X+TEERTable(SrcTbl).Obj_W div 2+soffset;
      Obj_W:=TEERTable(DestTbl).Obj_X+TEERTable(DestTbl).Obj_W div 2-Obj_X+doffset;
    end;

    //for standard Notation, if there start and end are at approximatly same pos
    if(abs(dx)<ParentEERModel.RelIconSize)then
      relMidDirection:=re_bottom;

    Obj_Y:=TEERTable(SrcTbl).Obj_Y+TEERTable(SrcTbl).Obj_H;
    Obj_H:=TEERTable(DestTbl).Obj_Y-Obj_Y;

    Obj_X:=Obj_X-ReEvalZoomFac(ParentEERModel.RelIconDSize);
    Obj_W:=Obj_W+ReEvalZoomFac(ParentEERModel.RelIconSize);
  end;

  //Check MidOffset
  if(relDirection=re_right)or(relDirection=re_left)and(MidOffset<>0)then
  begin
    if(Abs(MidOffset)>Obj_W div 2-2)and(MidOffset<>0)then
      MidOffset:=(Obj_W div 2-2)*(Abs(MidOffset) div MidOffset);
  end;
  if(relDirection=re_top)or(relDirection=re_bottom)and(MidOffset<>0)then
  begin
    if(Abs(MidOffset)>Obj_H div 2-2)and(MidOffset<>0)then
      MidOffset:=(Obj_H div 2-2)*(Abs(MidOffset) div MidOffset);
  end;

  //Right
  if(relDirection=re_right)then
  begin
    //Set PaintBox Positions
    Left:=EvalZoomFac(Obj_X);
    if(dy>0)then Top:=EvalZoomFac(Obj_Y+Obj_H)-ParentEERModel.RelIconSize else Top:=EvalZoomFac(Obj_Y);
    Width:=EvalZoomFac(Obj_W div 2+MidOffset);
    Height:=ParentEERModel.RelIconSize;

    //Set End PaintBox Positions
    if(SrcTbl<>DestTbl)then
    begin
      RelEnd.Left:=EvalZoomFac(Obj_X+Obj_W div 2+MidOffset);
      if(dy>0)then RelEnd.Top:=EvalZoomFac(Obj_Y) else RelEnd.Top:=EvalZoomFac(Obj_Y+Obj_H)-ParentEERModel.RelIconSize;
      RelEnd.Width:=EvalZoomFac(Obj_W div 2-MidOffset);
      RelEnd.Height:=ParentEERModel.RelIconSize;
    end
    else
    begin
      RelEnd.Left:=Left;
      if(dy>0)then RelEnd.Top:=EvalZoomFac(Obj_Y) else RelEnd.Top:=EvalZoomFac(Obj_Y+Obj_H)-ParentEERModel.RelIconSize;
      RelEnd.Width:=Width;
      RelEnd.Height:=ParentEERModel.RelIconSize;
    end;
  end
  //Left
  else if(relDirection=re_left)then
  begin
    //Set PaintBox Positions
    Left:=EvalZoomFac(Obj_X+Obj_W div 2+MidOffset);
    if(dy>0)then Top:=EvalZoomFac(Obj_Y+Obj_H)-ParentEERModel.RelIconSize else Top:=EvalZoomFac(Obj_Y);
    Width:=EvalZoomFac(Obj_W div 2-MidOffset);
    Height:=ParentEERModel.RelIconSize;

    //Set End PaintBox Positions
    RelEnd.Left:=EvalZoomFac(Obj_X);
    if(dy>0)then RelEnd.Top:=EvalZoomFac(Obj_Y) else RelEnd.Top:=EvalZoomFac(Obj_Y+Obj_H)-ParentEERModel.RelIconSize;
    RelEnd.Width:=EvalZoomFac(Obj_W div 2+MidOffset);
    RelEnd.Height:=ParentEERModel.RelIconSize;
  end
  //Top
  else if(relDirection=re_top)then
  begin
    //Set PaintBox Positions
    if(dx>0)then Left:=EvalZoomFac(Obj_X+Obj_W)-ParentEERModel.RelIconSize else Left:=EvalZoomFac(Obj_X);
    Top:=EvalZoomFac(Obj_Y+Obj_H div 2+MidOffset);
    Width:=ParentEERModel.RelIconSize;
    Height:=EvalZoomFac(Obj_H div 2-MidOffset);

    //Set End PaintBox Positions
    if(dx>0)then RelEnd.Left:=EvalZoomFac(Obj_X) else RelEnd.Left:=EvalZoomFac(Obj_X+Obj_W)-ParentEERModel.RelIconSize;
    RelEnd.Top:=EvalZoomFac(Obj_Y);
    RelEnd.Width:=ParentEERModel.RelIconSize;
    RelEnd.Height:=EvalZoomFac(Obj_H div 2+MidOffset);
  end
  //Bottom
  else
  begin
    //Set PaintBox Positions
    if(dx>0)then Left:=EvalZoomFac(Obj_X+Obj_W)-ParentEERModel.RelIconSize else Left:=EvalZoomFac(Obj_X);
    Top:=EvalZoomFac(Obj_Y);
    Width:=ParentEERModel.RelIconSize;
    Height:=EvalZoomFac(Obj_H div 2+MidOffset);

    //Set End PaintBox Positions
    if(dx>0)then RelEnd.Left:=EvalZoomFac(Obj_X) else RelEnd.Left:=EvalZoomFac(Obj_X+Obj_W)-ParentEERModel.RelIconSize;
    RelEnd.Top:=EvalZoomFac(Obj_Y+Obj_H div 2+MidOffset);
    RelEnd.Width:=ParentEERModel.RelIconSize;
    RelEnd.Height:=EvalZoomFac(Obj_H div 2-MidOffset);
  end;

  //Calc Lable Size
  theSize:=ParentEERModel.GetTextExtent(ObjName);

  RelCaption.Width:=EvalZoomFac(ReEvalZoomFac(theSize.cx)+6);
  RelCaption.Height:=EvalZoomFac(ReEvalZoomFac(theSize.cy)+4);

  //Set Middle PaintBox Positions
  if(relDirection=re_right)or(relDirection=re_left)then
  begin
    //Set Middle PaintBox Positions
    RelMiddle.Left:=EvalZoomFac(Obj_X+Obj_W div 2+MidOffset)-ParentEERModel.RelIconDSize;

    if((DMEER.Notation=noStandard)or(DMEER.Notation=noStandard2))and
      (RelKind<>rk_11Sub)and
      (EvalZoomFac(Obj_H)-ParentEERModel.RelIconSize<ParentEERModel.RelIconSize)then
      RelMiddle.Top:=EvalZoomFac(Obj_Y)+ParentEERModel.RelIconDSize+
        (EvalZoomFac(Obj_H)-ParentEERModel.RelIconSize*2) div 2
    else
      RelMiddle.Top:=EvalZoomFac(Obj_Y)+ParentEERModel.RelIconDSize;

    RelMiddle.Width:=ParentEERModel.RelIconSize+1;
    if((DMEER.Notation=noStandard)or(DMEER.Notation=noStandard2))and
      (RelKind<>rk_11Sub)and
      (EvalZoomFac(Obj_H)-ParentEERModel.RelIconSize<ParentEERModel.RelIconSize)then
      RelMiddle.Height:=ParentEERModel.RelIconSize+1
    else
      RelMiddle.Height:=EvalZoomFac(Obj_H)-ParentEERModel.RelIconSize;

    RelCaption.Left:=RelMiddle.Left-((RelCaption.Width-RelMiddle.Width) div 2);
    RelCaption.Top:=RelMiddle.Top+((RelMiddle.Height-RelCaption.Height) div 2)-
      ParentEERModel.RelIconSize-EvalZoomFac(5);
  end
  else
  begin
    //Set Middle PaintBox Positions
    if((DMEER.Notation=noStandard)or(DMEER.Notation=noStandard2))and
      (RelKind<>rk_11Sub)and
      (EvalZoomFac(Obj_W)-ParentEERModel.RelIconSize<ParentEERModel.RelIconSize)then
      RelMiddle.Left:=EvalZoomFac(Obj_X)+ParentEERModel.RelIconDSize+
        (EvalZoomFac(Obj_W)-ParentEERModel.RelIconSize*2) div 2
    else
      RelMiddle.Left:=EvalZoomFac(Obj_X)+ParentEERModel.RelIconDSize;
    RelMiddle.Top:=EvalZoomFac(Obj_Y+Obj_H div 2+MidOffset)-ParentEERModel.RelIconDSize;

    if((DMEER.Notation=noStandard)or(DMEER.Notation=noStandard2))and
      (RelKind<>rk_11Sub)and
      (EvalZoomFac(Obj_W)-ParentEERModel.RelIconSize<ParentEERModel.RelIconSize)then
      RelMiddle.Width:=ParentEERModel.RelIconSize+1
    else
      RelMiddle.Width:=EvalZoomFac(Obj_W)-ParentEERModel.RelIconSize;
    RelMiddle.Height:=ParentEERModel.RelIconSize+1;

    RelCaption.Left:=RelMiddle.Left+((RelMiddle.Width-RelCaption.Width) div 2);
    RelCaption.Top:=RelMiddle.Top-ParentEERModel.RelIconSize-EvalZoomFac(5);
  end;

  //--------------------------------------
  //Caption

  RelCaption.Left:=RelCaption.Left-EvalZoomFac(CaptionOffset.X);
  RelCaption.Top:=RelCaption.Top-EvalZoomFac(CaptionOffset.Y);

  if(DMEER.DisplayRelationNames)and
    (Not(RelCaption.Visible))and
    (Not(RelKind=rk_11Sub))then
  begin
    RelCaption.Show;
    RelCaption.BringToFront;
  end;

  if((Not(DMEER.DisplayRelationNames))or(RelKind=rk_11Sub))and
    (RelCaption.Visible)then
    RelCaption.Hide;

  //--------------------------------------
  //Interval Notation and Splitted

  theSize:=ParentEERModel.GetTextExtent(GetIntervalTxt(True));
  RelStartInterval.Width:=EvalZoomFac(ReEvalZoomFac(theSize.cx)+8);
  RelStartInterval.Height:=EvalZoomFac(ReEvalZoomFac(theSize.cy)+4);

  theSize:=ParentEERModel.GetTextExtent(GetIntervalTxt(False));
  RelEndInterval.Width:=EvalZoomFac(ReEvalZoomFac(theSize.cx)+8);
  RelEndInterval.Height:=EvalZoomFac(ReEvalZoomFac(theSize.cy)+4);

  if(relDirection=re_right)then
  begin
    RelStartInterval.Left:=Left+EvalZoomFac(10);
    RelStartInterval.Top:=Top-ParentEERModel.RelIconDSize-
      EvalZoomFac(5);

    RelEndInterval.Left:=RelEnd.Left+RelEnd.Width-
      RelEndInterval.Width-EvalZoomFac(10);
    RelEndInterval.Top:=RelEnd.Top-ParentEERModel.RelIconDSize-
      EvalZoomFac(5);
  end
  else if(relDirection=re_left)then
  begin
    RelStartInterval.Left:=Left+Width-
      RelStartInterval.Width-EvalZoomFac(10);
    RelStartInterval.Top:=Top-ParentEERModel.RelIconDSize-
      EvalZoomFac(5);

    RelEndInterval.Left:=RelEnd.Left+EvalZoomFac(10);
    RelEndInterval.Top:=RelEnd.Top-ParentEERModel.RelIconDSize-
      EvalZoomFac(5);
  end
  else if(relDirection=re_top)then
  begin
    RelStartInterval.Left:=Left+ParentEERModel.RelIconDSize+
      EvalZoomFac(10);
    RelStartInterval.Top:=Top+Height-RelStartInterval.Height-
      EvalZoomFac(5);

    RelEndInterval.Left:=RelEnd.Left+ParentEERModel.RelIconDSize+
      EvalZoomFac(10);
    RelEndInterval.Top:=RelEnd.Top+EvalZoomFac(5);
  end
  else if(relDirection=re_bottom)then
  begin
    RelStartInterval.Left:=Left+ParentEERModel.RelIconDSize+
      EvalZoomFac(10);
    RelStartInterval.Top:=Top+EvalZoomFac(5);

    RelEndInterval.Left:=RelEnd.Left+ParentEERModel.RelIconDSize+
      EvalZoomFac(10);
    RelEndInterval.Top:=RelEnd.Top+RelEnd.Height-
      RelEndInterval.Height-EvalZoomFac(5);
  end;


  RelStartInterval.Left:=RelStartInterval.Left-EvalZoomFac(StartIntervalOffset.X);
  RelStartInterval.Top:=RelStartInterval.Top-EvalZoomFac(StartIntervalOffset.Y);

  RelEndInterval.Left:=RelEndInterval.Left-EvalZoomFac(EndIntervalOffset.X);
  RelEndInterval.Top:=RelEndInterval.Top-EvalZoomFac(EndIntervalOffset.Y);

  {if((DMEER.Notation=noStandard2)and
    (Not(RelStartInterval.Visible))and
    (Not(RelKind=rk_11Sub)))or
    (Splitted)then
  begin
    RelStartInterval.Show;
    RelStartInterval.BringToFront;

    RelEndInterval.Show;
    RelEndInterval.BringToFront;
  end;

  if((Not(DMEER.Notation=noStandard2))or(RelKind=rk_11Sub))and
    (Not(Splitted))and
    (RelStartInterval.Visible)then
  begin
    RelStartInterval.Hide;

    RelEndInterval.Hide;
  end;}
end;

function TEERRel.GetRelDirection: integer;
var hv_fac: double;
  theDir: integer;
  x1, y1, x2, y2, dx, dy: double;
begin
  if(SrcTbl=DestTbl)then
  begin
    GetRelDirection:=re_Right;
    Exit;
  end;

  hv_fac:=1.3333333;
  theDir:=0;


  //Get centers of both tables
  x1:=TEERTable(SrcTbl).Obj_X+TEERTable(SrcTbl).Obj_W/2;
  y1:=TEERTable(SrcTbl).Obj_Y+TEERTable(SrcTbl).Obj_H/2;

  x2:=TEERTable(DestTbl).Obj_X+TEERTable(DestTbl).Obj_W/2;
  y2:=TEERTable(DestTbl).Obj_Y+TEERTable(DestTbl).Obj_H/2;

  dx:=x1-x2;
  dy:=y1-y2;

  //Check direct directions

  //top right corner
  if(dx<=0)and(dy>=0)then
  begin
    if(abs(dx*hv_fac)>abs(dy))then
      theDir:=re_Right
    else
      theDir:=re_Top;
  end
  //bottom right corner
  else if(dx<=0)and(dy<0)then
  begin
    if(abs(dx*hv_fac)>abs(dy))then
      theDir:=re_Right
    else
      theDir:=re_Bottom;
  end
  //top left corner
  else if(dx>0)and(dy>=0)then
  begin
    if(abs(dx*hv_fac)>abs(dy))then
      theDir:=re_Left
    else
      theDir:=re_Top;
  end
  //bottom left corner
  else if(dx>0)and(dy<0)then
  begin
    if(abs(dx*hv_fac)>abs(dy))then
      theDir:=re_Left
    else
      theDir:=re_Bottom;
  end;

  GetRelDirection:=theDir;
end;

function ReverseRelDirection(theRelDir: integer): integer;
begin
  // Reverse RelStartPos
  if(theRelDir=re_Left)then
    ReverseRelDirection:=re_Right
  else if(theRelDir=re_Right)then
    ReverseRelDirection:=re_Left
  else if(theRelDir=re_Top)then
    ReverseRelDirection:=re_Bottom
  else
    ReverseRelDirection:=re_Top;
end;

function TEERRel.GetXML: string;
var s: string;
begin
  s:=s+'<RELATION '+
    'ID="'+IntToStr(Obj_id)+'" '+
    'RelationName="'+DMMain.EncodeText4XML(ObjName)+'" '+
    'Kind="'+IntToStr(RelKind)+'" '+
    'SrcTable="'+IntToStr(SrcTbl.Obj_id)+'" '+
    'DestTable="'+IntToStr(DestTbl.Obj_id)+'" '+
    'FKFields="'+DMMain.EncodeText4XML(FKFields.Text)+'" '+
    'FKFieldsComments="'+DMMain.EncodeText4XML(FKFieldsComments.Text)+'" '+
    'relDirection="'+IntToStr(relDirection)+'" '+
    'MidOffset="'+IntToStr(MidOffset)+'" '+
    'OptionalStart="'+IntToStr(Ord(OptionalStart))+'" '+
    'OptionalEnd="'+IntToStr(Ord(OptionalEnd))+'" '+
    'CaptionOffsetX="'+IntToStr(CaptionOffset.X)+'" '+
    'CaptionOffsetY="'+IntToStr(CaptionOffset.Y)+'" '+
    'StartIntervalOffsetX="'+IntToStr(StartIntervalOffset.X)+'" '+
    'StartIntervalOffsetY="'+IntToStr(StartIntervalOffset.Y)+'" '+
    'EndIntervalOffsetX="'+IntToStr(EndIntervalOffset.X)+'" '+
    'EndIntervalOffsetY="'+IntToStr(EndIntervalOffset.Y)+'" '+
    'CreateRefDef="'+IntToStr(Ord(CreateRefDef))+'" '+
    'Invisible="'+IntToStr(Ord(Invisible))+'" '+
    'RefDef="'+DMMain.EncodeText4XML(RefDef.Text)+'" '+
    'Comments="'+DMMain.EncodeText4XML(Comments)+'" '+
    'FKRefDefIndex_Obj_id="'+IntToStr(FKRefDefIndex_Obj_id)+'" '+
    'Splitted="'+IntToStr(Ord(Splitted))+'" '+
    'IsLinkedObject="'+IntToStr(Ord(IsLinkedObject))+'" '+
    'IDLinkedModel="'+IntToStr(IDLinkedModel)+'" '+
    'Obj_id_Linked="'+IntToStr(Obj_id_Linked)+'" '+
    'OrderPos="'+IntToStr(OrderPos)+'" '+   
    '/>'+#13#10;

  GetXML:=s;
end;

{$IFDEF USE_IXMLDBMODELType}
procedure TEERRel.SetXML(theXMLRelation: IXMLRELATIONType);
var s: string;
begin
  try
    Obj_id:=theXMLRelation.ID;
    ObjName:=DMMain.DecodeXMLText(theXMLRelation.RelationName);
    try
      MidOffset:=StrToInt(theXMLRelation.MidOffset);
    except
      MidOffset:=0;
    end;

    try
      IsLinkedObject:=(theXMLRelation.IsLinkedObject=1);
      IDLinkedModel:=StrToInt(theXMLRelation.IDLinkedModel);
      Obj_id_Linked:=StrToInt(theXMLRelation.Obj_id_Linked);
    except
      IsLinkedObject:=False;
      IDLinkedModel:=-1;
      Obj_id_Linked:=-1;
    end;

    try
      RelKind:=theXMLRelation.Kind;

      FKFields.Text:=DMMain.DecodeXMLText(theXMLRelation.FKFields);
      FKFieldsComments.Text:=DMMain.DecodeXMLText(theXMLRelation.FKFieldsComments);

      OptionalStart:=(theXMLRelation.OptionalStart=1);
      OptionalEnd:=(theXMLRelation.OptionalEnd=1);
      CaptionOffset.X:=theXMLRelation.CaptionOffsetX;
      CaptionOffset.Y:=theXMLRelation.CaptionOffsetY;

      StartIntervalOffset.X:=theXMLRelation.StartIntervalOffsetX;
      StartIntervalOffset.Y:=theXMLRelation.StartIntervalOffsetY;
      EndIntervalOffset.X:=theXMLRelation.EndIntervalOffsetX;
      EndIntervalOffset.Y:=theXMLRelation.EndIntervalOffsetY;

      CreateRefDef:=(theXMLRelation.CreateRefDef=1);
      RefDef.Text:=DMMain.DecodeXMLText(theXMLRelation.RefDef);

      Comments:=DMMain.DecodeXMLText(theXMLRelation.Comments);

      Invisible:=(theXMLRelation.Invisible=1);
    except
    end;

    try
      SrcTbl:=ParentEERModel.GetEERObjectByID(theXMLRelation.SrcTable);
      DestTbl:=ParentEERModel.GetEERObjectByID(theXMLRelation.DestTable);
    except
    end;

    try
      FKRefDefIndex_Obj_id:=StrToInt(theXMLRelation.FKRefDefIndex_Obj_id);
      if(ParentEERModel.GetEERIndexByID(FKRefDefIndex_Obj_id)=nil)then
        FKRefDefIndex_Obj_id:=-1;
    except
      FKRefDefIndex_Obj_id:=-1;
    end;

    try
      Splitted:=(theXMLRelation.Splitted=1);
    except
      Splitted:=False;
    end;
  except
    on x: Exception do
    begin
      s:=ObjName+#13#10+
        'Error: '+x.Message;

      ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Relation from XML File:'+#13#10+
        'Relationname: %s', 79, s));
    end;
  end;
end;
{$ENDIF}

procedure TEERRel.SetXML2(theXMLParser: TXmlParser);
var s: string;
begin
  try
    Obj_id:=StrToInt(theXMLParser.CurAttr.Value('ID'));
    ObjName:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('RelationName'));
    try
      MidOffset:=StrToInt(theXMLParser.CurAttr.Value('MidOffset'));
    except
      MidOffset:=0;
    end;

    try
      IsLinkedObject:=(theXMLParser.CurAttr.Value('IsLinkedObject')='1');
      IDLinkedModel:=StrToInt(theXMLParser.CurAttr.Value('IDLinkedModel'));
      Obj_id_Linked:=StrToInt(theXMLParser.CurAttr.Value('Obj_id_Linked'));
    except
      IsLinkedObject:=False;
      IDLinkedModel:=-1;
      Obj_id_Linked:=-1;
    end;

    try
      RelKind:=StrToInt(theXMLParser.CurAttr.Value('Kind'));

      FKFields.Text:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('FKFields'));
      FKFieldsComments.Text:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('FKFieldsComments'));

      OptionalStart:=(theXMLParser.CurAttr.Value('OptionalStart')='1');
      OptionalEnd:=(theXMLParser.CurAttr.Value('OptionalEnd')='1');
      CaptionOffset.X:=StrToInt(theXMLParser.CurAttr.Value('CaptionOffsetX'));
      CaptionOffset.Y:=StrToInt(theXMLParser.CurAttr.Value('CaptionOffsetY'));

      StartIntervalOffset.X:=StrToInt(theXMLParser.CurAttr.Value('StartIntervalOffsetX'));
      StartIntervalOffset.Y:=StrToInt(theXMLParser.CurAttr.Value('StartIntervalOffsetY'));
      EndIntervalOffset.X:=StrToInt(theXMLParser.CurAttr.Value('EndIntervalOffsetX'));
      EndIntervalOffset.Y:=StrToInt(theXMLParser.CurAttr.Value('EndIntervalOffsetY'));

      CreateRefDef:=(theXMLParser.CurAttr.Value('CreateRefDef')='1');
      RefDef.Text:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('RefDef'));

      Comments:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('Comments'));

      Invisible:=(theXMLParser.CurAttr.Value('Invisible')='1');
    except
    end;

    try
      SrcTbl:=ParentEERModel.GetEERObjectByID(StrToInt(theXMLParser.CurAttr.Value('SrcTable')));
      DestTbl:=ParentEERModel.GetEERObjectByID(StrToInt(theXMLParser.CurAttr.Value('DestTable')));
    except
    end;

    try
      FKRefDefIndex_Obj_id:=StrToInt(theXMLParser.CurAttr.Value('FKRefDefIndex_Obj_id'));
      if(ParentEERModel.GetEERIndexByID(FKRefDefIndex_Obj_id)=nil)then
        FKRefDefIndex_Obj_id:=-1;
    except
      FKRefDefIndex_Obj_id:=-1;
    end;

    try
      Splitted:=(theXMLParser.CurAttr.Value('Splitted')='1');
    except
      Splitted:=False;
    end;

    try
      OrderPos:=StrToInt(theXMLParser.CurAttr.Value('OrderPos'));
    except
    end;    
  except
    on x: Exception do
    begin
      s:=ObjName+#13#10+
        'Error: '+x.Message;

      ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Relation from XML File:'+#13#10+
        'Relationname: %s', 79, s));
    end;
  end;
end;



// -----------------------------------------------
// Implementation of the EER-Note

constructor TEERNote.Create(AOwner: TComponent; TheName: string);
begin
  inherited Create(AOwner);

  Parent:=TWidgetControl(AOwner);
  Name:=DMMain.GetValidObjectName(TheName);

  Font.Name:=ParentEERModel.DefModelFont;

  Visible:=False;

  ObjName:=Name;

  NoteText:=TStringList.Create;

  NoteText.Text:=DMMain.GetTranslatedMessage('Doubleclick on the Note'+#13#10+'to edit the Text.', 21);

  PopupMenu:=ParentEERModel.PopupMenuEERNote;
end;

destructor TEERNote.Destroy;
begin
  NoteText.Free;

  inherited;
end;

procedure TEERNote.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TEERNote then
  begin
    NoteText.Text:=TEERNote(Source).NoteText.Text;
  end;
end;

procedure TEERNote.DeleteObj;
begin
  //Log the Delete-Action if the Flag is set
  if(ParentEERModel.LogActions)then
    ParentEERModel.LogSubAction(at_DeleteObj, Obj_id, GetObjAsXMLModel);

  Free;
end;

procedure TEERNote.ShowEditor(Sender: TObject);
begin
  //Show Note Editor
  if(DMEER.CurrentWorkTool=wtPointer)or
    (DMEER.CurrentWorkTool=wtMove)then
  begin
    MouseIsDown:=False;

    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_EditNote, self));

    //Process Mouse Messages before setting EditorIsCalled to false
    Application.ProcessMessages;

    EditorIsCalled:=False;
  end;
end;

procedure TEERNote.PaintObj2Canvas(theCanvas: TCanvas; xo, yo: integer);
var width, height: integer;
begin
  if(ParentEERModel.DisableModelRefresh)then
    Exit;
    
  width:=EvalZoomFac(Obj_W);
  height:=EvalZoomFac(Obj_H);

  with theCanvas do
  begin
    Pen.Style:=psSolid;
    if(IsLinkedObject)then
      Pen.Color:=$00C66931
    else
      Pen.Color:=clSilver;
    Brush.Color:=clWhite;
    Rectangle(Rect(xo+0, yo+0, xo+width, yo+height));

    if(Not(DMEER.DisableTextOutput))then
    begin
      Font.Height:=ParentEERModel.GetFontHeight;
      TextRect(Rect(xo+1, yo+1, xo+width-2, yo+height-2),
        xo+1+EvalZoomFac(3), yo+1, NoteText.Text);
    end;

    // Paint selection
    if(selected)then
    begin
      Pen.Color:=clWhite;
      MoveTo(xo+0, yo+0);
      LineTo(xo+0, yo+height-1);
      LineTo(xo+width-1, yo+height-1);
      LineTo(xo+width-1, yo+0);
      LineTo(xo+0, yo+0);

      Pen.Color:=clBlack;
      Pen.Style:=psDot;
      MoveTo(xo+0, yo+0);
      LineTo(xo+0, yo+height-1);
      LineTo(xo+width-1, yo+height-1);
      LineTo(xo+width-1, yo+0);
      LineTo(xo+0, yo+0);

      Pen.Style:=psSolid;
    end;
  end;
end;

procedure TEERNote.DoPaint(Sender: TObject);
begin
  PaintObj(Canvas);
end;

procedure TEERNote.RefreshObj;
var theSize: TSize;
begin
  theSize:=ParentEERModel.GetTextExtent(NoteText.Text);
  Obj_W:=ReEvalZoomFac(theSize.cx)+6;
  Obj_H:=ReEvalZoomFac(theSize.cy)+4-14;

  //Only reposition Obj when the model is not drawn to another canvas
  if(Not(ParentEERModel.PaintingToSpecialCanvas))then
  begin
    Left:=EvalZoomFac(Obj_X);
    Top:=EvalZoomFac(Obj_Y);
    Width:=EvalZoomFac(Obj_W);
    Height:=EvalZoomFac(Obj_H);
  end;
end;

function TEERNote.GetXML: string;
var s: string;
begin
  s:=s+'<NOTE '+
    'ID="'+IntToStr(Obj_id)+'" '+
    'NoteName="'+DMMain.EncodeText4XML(ObjName)+'" '+
    'XPos="'+IntToStr(Obj_X)+'" '+
    'YPos="'+IntToStr(Obj_Y)+'" '+
    'NoteText="'+DMMain.EncodeText4XML(NoteText.Text)+'" '+
    'IsLinkedObject="'+IntToStr(Ord(IsLinkedObject))+'" '+
    'IDLinkedModel="'+IntToStr(IDLinkedModel)+'" '+
    'Obj_id_Linked="'+IntToStr(Obj_id_Linked)+'" '+
    'OrderPos="'+IntToStr(OrderPos)+'" '+      
    '/>'+#13#10;

  GetXML:=s;
end;

{$IFDEF USE_IXMLDBMODELType}
procedure TEERNote.SetXML(theXMLNote: IXMLNOTEType);
var s: string;
begin
  try
    Obj_id:=theXMLNote.ID;
    ObjName:=DMMain.DecodeXMLText(theXMLNote.NoteName);
    Obj_X:=theXMLNote.XPos;
    Obj_Y:=theXMLNote.YPos;

    try
      IsLinkedObject:=(theXMLNote.IsLinkedObject=1);
      IDLinkedModel:=StrToInt(theXMLNote.IDLinkedModel);
      Obj_id_Linked:=StrToInt(theXMLNote.Obj_id_Linked);
    except
      IsLinkedObject:=False;
      IDLinkedModel:=-1;
      Obj_id_Linked:=-1;
    end;

    NoteText.Text:=DMMain.DecodeXMLText(theXMLNote.NoteText);
  except
    on x: Exception do
    begin
      s:=ObjName+#13#10+
        'Error: '+x.Message;
      ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Note from XML File:'+#13#10+
        'Notename: %s', 80, s));
    end;
  end;
end;
{$ENDIF}

procedure TEERNote.SetXML2(theXMLParser: TXmlParser);
var s: string;
begin
  try
    Obj_id:=StrToInt(theXMLParser.CurAttr.Value('ID'));
    ObjName:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('NoteName'));
    Obj_X:=StrToInt(theXMLParser.CurAttr.Value('XPos'));
    Obj_Y:=StrToInt(theXMLParser.CurAttr.Value('YPos'));

    try
      IsLinkedObject:=(theXMLParser.CurAttr.Value('IsLinkedObject')='1');
      IDLinkedModel:=StrToInt(theXMLParser.CurAttr.Value('IDLinkedModel'));
      Obj_id_Linked:=StrToInt(theXMLParser.CurAttr.Value('Obj_id_Linked'));
    except
      IsLinkedObject:=False;
      IDLinkedModel:=-1;
      Obj_id_Linked:=-1;
    end;

    try
      OrderPos:=StrToInt(theXMLParser.CurAttr.Value('OrderPos'));
    except
    end;    

    NoteText.Text:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('NoteText'));
  except
    on x: Exception do
    begin
      s:=ObjName+#13#10+
        'Error: '+x.Message;
      ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Note from XML File:'+#13#10+
        'Notename: %s', 80, s));
    end;
  end;
end;

function TEERNote.GetNoteText: string;
begin
  GetNoteText:=NoteText.Text;
end;

procedure TEERNote.SetNoteText(txt: string);
begin
  NoteText.Text:=txt;
end;

// -----------------------------------------------
// Implementation of the EER-Region

constructor TEERRegion.Create(AOwner: TComponent; TheName: string);
begin
  inherited Create(AOwner);

  Parent:=TWidgetControl(AOwner);
  Name:=DMMain.GetValidObjectName(TheName);

  Font.Name:=ParentEERModel.DefModelFont;

  Visible:=False;

  ObjName:=Name;

  ObjectsInRegion:=TList.Create;

  TablePrefix:=0;
  TableType:=0;

  OverwriteTablePrefix:=False;
  OverwriteTableType:=False;

  PopupMenu:=ParentEERModel.PopupMenuEERRegion;
end;

destructor TEERRegion.Destroy;
begin
  ObjectsInRegion.Free;
  
  inherited;
end;

function TEERRegion.ObjIsEqualTo(Source: TObject): Boolean;
begin
  ObjIsEqualTo:=False;

  if(inherited ObjIsEqualTo(Source))then
    if(Source is TEERRegion)then
    begin
      if(RegionColor=TEERRegion(Source).RegionColor)or
        (TablePrefix=TEERRegion(Source).TablePrefix)or
        (TableType=TEERRegion(Source).TableType)or
        (OverwriteTablePrefix=TEERRegion(Source).OverwriteTablePrefix)or
        (OverwriteTableType=TEERRegion(Source).OverwriteTableType)then
        ObjIsEqualTo:=True;
    end;
end;

procedure TEERRegion.DeleteObj;
begin
  //Log the Delete-Action if the Flag is set
  if(ParentEERModel.LogActions)then
    ParentEERModel.LogSubAction(at_DeleteObj, Obj_id, GetObjAsXMLModel);

  Free;
end;


procedure TEERRegion.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(Button=mbLeft)then
  begin
    //Make shure that no TextImput has the focus
    if(Application.MainForm.ActiveControl<>nil)then
      Application.MainForm.ActiveControl:=nil;
  
    mouse_absx:=Mouse.CursorPos.X;
    mouse_absy:=Mouse.CursorPos.Y;

    // If Worktool wtPointer is the current tool,
    // select or deselect tbl
    if((DMEER.CurrentWorkTool=wtPointer)or
      (DMEER.CurrentWorkTool=wtImage))and(not(selected))then
    begin
      if(not(ssShift in Shift))then
        ParentEERModel.DeSelectAllObjs(nil);

      mouse_absx:=Mouse.CursorPos.X;
      mouse_absy:=Mouse.CursorPos.Y;

      mouse_posx:=X;
      mouse_posy:=Y;

      ParentEERModel.SetSelectionRectPos(Left+X, Top+Y,
        1, 1);

      MouseIsDown:=True;
    end;

    if((DMEER.CurrentWorkTool=wtPointer)and(ssCtrl in Shift))then
      ParentEERModel.DeSelectAllObjs(nil);

    // If Worktool wtMove is the current tool, store obj x/y
    if(DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign)and(selected))then
    begin
      mouse_posx:=Left;
      mouse_posy:=Top;

      MouseIsDown:=True;

      if(not(ssShift in Shift))then
        ParentEERModel.DeSelectAllObjs(nil);
      SelectAllObjsInRegion;

      ParentEERModel.StartSubActionLog(at_MoveObj);
      ParentEERModel.LogSubAction(sa_MoveFrom, Obj_id,
        'Obj_X='+IntToStr(Obj_X)+#13#10+'Obj_Y='+IntToStr(Obj_Y));

      //Do for all selected Objects if this obj is selected
      if(Selected)then
        ParentEERModel.InitialMove4AllSelectedObjs(self);

      ObjChanged:=False;
    end;

    //If Worktool wtSize is the current tool, store obj w/h
    if(DMEER.CurrentWorkTool=wtSize)then
    begin
      mouse_posx:=Width;
      mouse_posy:=Height;

      MouseIsDown:=True;

      ParentEERModel.StartSubActionLog(at_ScaleObj);
      ParentEERModel.LogSubAction(sa_ScaleFrom, Obj_id,
        'Obj_W='+IntToStr(Obj_W)+#13#10+'Obj_H='+IntToStr(Obj_H));

      ObjChanged:=False;
    end;

    if(DMEER.CurrentWorkTool=wtTable)then
    begin
      ParentEERModel.NewTable(
        ParentEERModel.ReEvalZoomFac(Left+X),
        ParentEERModel.ReEvalZoomFac(Top+Y), True);
      DMEER.SetWorkTool(wtPointer);
    end;

    if(DMEER.CurrentWorkTool=wtNote)then
    begin
      ParentEERModel.NewNote(ReEvalZoomFac(Left+X),
        ReEvalZoomFac(Top+Y), True);
      DMEER.SetWorkTool(wtPointer);
    end;

    // If Worktool Hand is the current tool, store scrollbar pos
    if(DMEER.CurrentWorkTool=wtHand)then
    begin
      if(Assigned(TForm(parent.parent).HorzScrollBar))then
        mouse_posx:=TForm(parent.parent).HorzScrollBar.Position
      else
        mouse_posx:=0;

      if(Assigned(TForm(parent.parent).VertScrollBar))then
        mouse_posy:=TForm(parent.parent).VertScrollBar.Position
      else
        mouse_posy:=0;

      MouseIsDown:=True;
    end;

    if(DMEER.CurrentWorkTool=wtZoomIn)then
      ParentEERModel.ZoomIn(Left+X, Top+Y);

    if(DMEER.CurrentWorkTool=wtZoomOut)then
      ParentEERModel.ZoomOut(Left+X, Top+Y);
  end;
end;

procedure TEERRegion.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var SelRect: TRect;
  i: integer;
  theSubAction: TEERActionSubLog;
begin
  ParentEERModel.ClearMouseOverObj;

  if(ssLeft in Shift)and(MouseIsDown)then
  begin
    if((DMEER.CurrentWorkTool=wtPointer)or
      (DMEER.CurrentWorkTool=wtImage))and(not(selected))then
    begin
      ParentEERModel.GetSelectionRectPos(SelRect);

      //Resize SelectionRect
      SelRect.Right:=Mouse.CursorPos.X-mouse_absx;
      SelRect.Bottom:=Mouse.CursorPos.Y-mouse_absy;

      //update left-top corner if needed
      if(SelRect.Right<0)then
        SelRect.Left:=Left+mouse_posx+SelRect.Right;
      if(SelRect.Bottom<0)then
        SelRect.Top:=Top+mouse_posy+SelRect.Bottom;

      ParentEERModel.SetSelectionRectPos(SelRect.Left, SelRect.Top,
        abs(SelRect.Right), abs(SelRect.Bottom));
    end;

    // If Worktool wtMove is the current tool, move obj
    if(DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and
      (DMEER.WorkMode=wmDesign)and(selected))and
      (Not(TEERModel(Parent).ReadOnly))then
    begin
      //When shift is not pressed, do normal Move
      if(Not(ssShift in Shift))then
      begin
        Obj_X:=ReEvalZoomFac(mouse_posx+Mouse.CursorPos.X-mouse_absx);
        Obj_Y:=ReEvalZoomFac(mouse_posy+Mouse.CursorPos.Y-mouse_absy);
      end
      else
      begin
        //Only move in on direction when user presses shift
        theSubAction:=TEERActionSubLog(ParentEERModel.GetSubActionOfObj(ParentEERModel.CurrentAction, Obj_id));
        if(theSubAction<>nil)then
        begin
          if(abs(Mouse.CursorPos.X-mouse_absx)>abs(Mouse.CursorPos.Y-mouse_absy))then
          begin
            Obj_X:=ReEvalZoomFac(mouse_posx+Mouse.CursorPos.X-mouse_absx);
            Obj_Y:=StrToInt(theSubAction.Params.Values['Obj_Y']);
          end
          else
          begin
            Obj_X:=StrToInt(theSubAction.Params.Values['Obj_X']);
            Obj_Y:=ReEvalZoomFac(mouse_posy+Mouse.CursorPos.Y-mouse_absy);
          end;
        end;
      end;

      //Use PositionGrid if selected
      if(ParentEERModel.UsePositionGrid)then
      begin
        Obj_X:=(Obj_X div ParentEERModel.PositionGrid.X) * ParentEERModel.PositionGrid.X;
        Obj_Y:=(Obj_Y div ParentEERModel.PositionGrid.Y) * ParentEERModel.PositionGrid.Y;
      end;

      RefreshObj;

      //Do for all selected Objects if this obj is selected
      if(Selected)then
        with ParentEERModel do
        begin
          for i:=ComponentCount-1 downto 0 do
            if(Components[I].Classparent=TEERObj)then
              if(TEERObj(Components[I]).Selected)then
              begin
                //When shift is not pressed, do normal Move
                if(Not(ssShift in Shift))then
                begin
                  TEERObj(Components[I]).Obj_X:=ReEvalZoomFac(TEERObj(Components[I]).mouse_posx+Mouse.CursorPos.X-self.mouse_absx);
                  TEERObj(Components[I]).Obj_Y:=ReEvalZoomFac(TEERObj(Components[I]).mouse_posy+Mouse.CursorPos.Y-self.mouse_absy);
                end
                else
                begin
                  //Only move in on direction when user presses shift
                  theSubAction:=TEERActionSubLog(GetSubActionOfObj(CurrentAction, TEERObj(Components[I]).Obj_id));

                  if(theSubAction<>nil)then
                  begin
                    if(abs(Mouse.CursorPos.X-self.mouse_absx)>abs(Mouse.CursorPos.Y-self.mouse_absy))then
                    begin
                      TEERObj(Components[I]).Obj_X:=ReEvalZoomFac(TEERObj(Components[I]).mouse_posx+Mouse.CursorPos.X-self.mouse_absx);
                      TEERObj(Components[I]).Obj_Y:=StrToInt(theSubAction.Params.Values['Obj_Y']);
                    end
                    else
                    begin
                      TEERObj(Components[I]).Obj_X:=StrToInt(theSubAction.Params.Values['Obj_X']);
                      TEERObj(Components[I]).Obj_Y:=ReEvalZoomFac(TEERObj(Components[I]).mouse_posy+Mouse.CursorPos.Y-self.mouse_absy);
                    end;
                  end;
                end;

                //Use PositionGrid if selected
                if(UsePositionGrid)then
                begin
                  TEERObj(Components[I]).Obj_X:=(TEERObj(Components[I]).Obj_X div PositionGrid.X) * PositionGrid.X;
                  TEERObj(Components[I]).Obj_Y:=(TEERObj(Components[I]).Obj_Y div PositionGrid.Y) * PositionGrid.Y;
                end;

                TEERObj(Components[I]).RefreshObj;
              end;
        end;

      ObjChanged:=True;
    end;

    if(DMEER.CurrentWorkTool=wtSize)and
      (Not(TEERModel(Parent).ReadOnly))then
    begin
      Obj_W:=ReEvalZoomFac(mouse_posx+Mouse.CursorPos.X-mouse_absx);
      Obj_H:=ReEvalZoomFac(mouse_posy+Mouse.CursorPos.Y-mouse_absy);

      //Use PositionGrid if selected
      if(ParentEERModel.UsePositionGrid)then
      begin
        Obj_W:=(Obj_W div ParentEERModel.PositionGrid.X) * ParentEERModel.PositionGrid.X;
        Obj_H:=(Obj_H div ParentEERModel.PositionGrid.Y) * ParentEERModel.PositionGrid.Y;
      end;

      RefreshObj;

      ObjChanged:=True;
    end;

    // If Worktool Hand is the current tool, scroll the area
    if(DMEER.CurrentWorkTool=wtHand)then
    begin
      if(TForm(parent.parent).HorzScrollBar<>nil)then
      begin
        TForm(parent.parent).HorzScrollBar.Position:=
          mouse_posx+(Mouse.CursorPos.X-mouse_absx)*-1;
  {$IFDEF LINUX}
        if(TForm(parent.parent).HorzScrollBar.Position<0)then
          TForm(parent.parent).HorzScrollBar.Position:=0;
        if(TForm(parent.parent).HorzScrollBar.Position>TForm(parent.parent).HorzScrollBar.Range-TForm(parent.parent).ClientWidth)then
          TForm(parent.parent).HorzScrollBar.Position:=TForm(parent.parent).HorzScrollBar.Range-TForm(parent.parent).ClientWidth;
  {$ENDIF}
      end;

      if(TForm(parent.parent).VertScrollBar<>nil)then
      begin
        TForm(parent.parent).VertScrollBar.Position:=
          mouse_posy+(Mouse.CursorPos.Y-mouse_absy)*-1;
  {$IFDEF LINUX}
        if(TForm(parent.parent).VertScrollBar.Position<0)then
          TForm(parent.parent).VertScrollBar.Position:=0;
        if(TForm(parent.parent).VertScrollBar.Position>TForm(parent.parent).VertScrollBar.Range-TForm(parent.parent).ClientHeight)then
          TForm(parent.parent).VertScrollBar.Position:=TForm(parent.parent).VertScrollBar.Range-TForm(parent.parent).ClientHeight;
  {$ENDIF}
      end;
    end;
  end;
end;

procedure TEERRegion.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var theImage: TEERImage;
  SelRect: TRect;
  i: integer;
  thePoint: TPoint;
begin
  if(Button=mbLeft)then
  begin
    MouseIsDown:=False;

    if(DMEER.CurrentWorkTool=wtPointer)and
      (ParentEERModel.GetSelectionRectVisible)then
    begin
      ParentEERModel.GetSelectionRectPos(SelRect);

      {if(abs(SelRect.Bottom)<=1)and(abs(SelRect.Right)<=1)then
      begin
        if(not(ssShift in Shift))then
          ParentEERModel.ClearAllSelectedObj(nil);
        SelectAllObjInRegion;
      end
      else}
        ParentEERModel.SelectObjsInSelectionRect;
    end
    else if(DMEER.CurrentWorkTool=wtPointer)and
      (Y<TEERModel(Parent).EvalZoomFac(17))and
      (X<TEERModel(Parent).GetTextExtent(ObjName).cx+TEERModel(Parent).EvalZoomFac(5))then
    begin
      SetSelected(True);
    end;

    ParentEERModel.HideSelectionRect;


    if(DMEER.CurrentWorkTool=wtDelete)then
    begin
      QApplication_postEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_DeleteObject, self));
      Exit;
    end;

    if(DMEER.CurrentWorkTool=wtImage)and
      (ParentEERModel.SelectionRect.Visible)then
    begin
      ParentEERModel.HideSelectionRect;

      theImage:=ParentEERModel.NewImage(
        ReEvalZoomFac(ParentEERModel.SelectionRect.Left),
        ReEvalZoomFac(ParentEERModel.SelectionRect.Top),
        ReEvalZoomFac(ParentEERModel.SelectionRect.Width),
        ReEvalZoomFac(ParentEERModel.SelectionRect.Height), True);

      theImage.LoadImageFromFile;

      DMEER.SetWorkTool(wtPointer);
    end;

    if(ObjChanged=True)and
      ((DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign)))then
    begin
      ParentEERModel.LogSubAction(sa_MoveTo, Obj_id,
        'Obj_X='+IntToStr(Obj_X)+#13#10+'Obj_Y='+IntToStr(Obj_Y));

      //Do for all selected Objects if this obj is selected
      if(Selected)then
        with ParentEERModel do
        begin
          for i:=ComponentCount-1 downto 0 do
            if(Components[I].Classparent=TEERObj)then
              if(TEERObj(Components[I]).Selected)and(Components[I]<>self)then
              begin
                LogSubAction(sa_MoveTo, TEERObj(Components[I]).Obj_id,
                  'Obj_X='+IntToStr(TEERObj(Components[I]).Obj_X)+#13#10+'Obj_Y='+IntToStr(TEERObj(Components[I]).Obj_Y));
              end;
        end;

      //Close Log
      ParentEERModel.EndSubAction;

      ObjChanged:=False;
    end
    //if the obj has not been moved, clear move action
    else if(DMEER.CurrentWorkTool=wtMove)or
      ((DMEER.CurrentWorkTool=wtPointer)and(DMEER.WorkMode=wmDesign))then
      ParentEERModel.DeleteOpenAction;

    if(ObjChanged=True)and
      (DMEER.CurrentWorkTool=wtSize)then
    begin
      {RefreshObj;
      DoPaint(self);}

      //Log Move
      ParentEERModel.LogSubAction(sa_ScaleTo, Obj_id,
        'Obj_W='+IntToStr(Obj_W)+#13#10+'Obj_H='+IntToStr(Obj_H));

      //Close Log
      ParentEERModel.EndSubAction;

      ObjChanged:=False;
    end
    //if the obj has not been moved, clear move action
    else if(DMEER.CurrentWorkTool=wtSize)then
      ParentEERModel.DeleteOpenAction;

    if(Not(ParentEERModel.DisableModelRefresh))then
      DMEER.RefreshInfoPalette;

    if(DMEER.CurrentWorkTool=wtPlacementFromFile)or
      (DMEER.CurrentWorkTool=wtPlacementFromDB)or
      (DMEER.CurrentWorkTool=wtPlacementFromLibrary)then
    begin
      thePoint.X:=Obj_X+ReEvalZoomFac(X);
      thePoint.Y:=Obj_Y+ReEvalZoomFac(Y);

      if(DMEER.CurrentWorkTool=wtPlacementFromFile)then
      begin
        DMEER.SetWorkTool(wtPointer);
        sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_PlaceModelFromFile, @thePoint));
      end
      else if(DMEER.CurrentWorkTool=wtPlacementFromDB)then
      begin
        DMEER.SetWorkTool(wtPointer);
        sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_PlaceModelFromDB, @thePoint));
      end
      else
      begin
        DMEER.SetWorkTool(wtPointer);
        sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_PlaceModelFromLibrary, @thePoint));
      end;
    end;
  end;
end;

procedure TEERRegion.ShowEditor(Sender: TObject);
begin
  //Show Region Editor
  if(DMEER.CurrentWorkTool=wtPointer)or
    (DMEER.CurrentWorkTool=wtMove)then
  begin
    MouseIsDown:=False;

    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_EditRegion, self));

    //Process Mouse Messages before setting EditorIsCalled to false
    Application.ProcessMessages;

    EditorIsCalled:=False;
  end;
end;

procedure TEERRegion.PaintObj2Canvas(theCanvas: TCanvas; xo, yo: integer);
var width, height: integer;
  s: string;
begin
  if(ParentEERModel.DisableModelRefresh)then
    Exit;

  width:=EvalZoomFac(Obj_W);
  height:=EvalZoomFac(Obj_H);

  with theCanvas do
  begin
    if(IsLinkedObject)then
      Pen.Color:=$00C66931
    else
      Pen.Color:=clSilver;

    s:=ParentEERModel.RegionColors.ValueFromIndex[RegionColor];
    try
      Brush.Color:=DMMain.RGB(DMMain.HexStringToInt(Copy(s, 2, 2)),
        DMMain.HexStringToInt(Copy(s, 4, 2)),
        DMMain.HexStringToInt(Copy(s, 6, 2)));
    except
      Brush.Color:=$00DDE1FF;
    end;

    Rectangle(Rect(xo+0, yo+0, xo+width, yo+height));

    //Draw Region Name
    if(Not(DMEER.DisableTextOutput))then
    begin
      Font.Height:=ParentEERModel.GetFontHeight;
      Font.Color:=clGray;
      TextOut(xo+EvalZoomFac(4), yo+EvalZoomFac(3), ObjName);
    end;

    // Paint selection
    if(selected)then
    begin
      Pen.Color:=clWhite;
      MoveTo(xo+0, yo+0);
      LineTo(xo+0, yo+height-1);
      LineTo(xo+width-1, yo+height-1);
      LineTo(xo+width-1, yo+0);
      LineTo(xo+0, yo+0);

      Pen.Color:=clBlack;
      Pen.Style:=psDot;
      MoveTo(xo+0, yo+0);
      LineTo(xo+0, yo+height-1);
      LineTo(xo+width-1, yo+height-1);
      LineTo(xo+width-1, yo+0);
      LineTo(xo+0, yo+0);

      Pen.Style:=psSolid;
    end;
  end;
end;

procedure TEERRegion.DoPaint(Sender: TObject);
begin
  PaintObj(Canvas);
end;

procedure TEERRegion.RefreshObj;
var i: integer;
begin
  //Check Tables in Region
  ObjectsInRegion.Clear;

  with ParentEERModel do
  begin
    for i:=ComponentCount-1 downto 0 do
    begin
      if(Components[I].Classparent=TEERObj)then
      begin
        if(ReEvalZoomFac(SelectionRect.Left)<=
          TEERObj(Components[I]).Obj_X)and
          (ReEvalZoomFac(SelectionRect.Top)<=
          TEERObj(Components[I]).Obj_Y)and
          (ReEvalZoomFac(SelectionRect.Left+SelectionRect.Width)>=
          TEERObj(Components[I]).Obj_X+TEERObj(Components[I]).Obj_W)and
          (ReEvalZoomFac(SelectionRect.Top+SelectionRect.Height)>=
          TEERObj(Components[I]).Obj_Y+TEERObj(Components[I]).Obj_H)then
        begin
          ObjectsInRegion.Add(Components[i]);

          //TEERObj(Components[I]).DoPaint(self);
        end;
      end;
    end;
  end;

  //Only reposition Obj when the model is not drawn to another canvas
  if(Not(ParentEERModel.PaintingToSpecialCanvas))then
  begin
    Left:=EvalZoomFac(Obj_X);
    Top:=EvalZoomFac(Obj_Y);
    Width:=EvalZoomFac(Obj_W);
    Height:=EvalZoomFac(Obj_H);
  end;
end;

function TEERRegion.GetXML: string;
var s: string;
begin
  s:='<REGION '+
    'ID="'+IntToStr(Obj_id)+'" '+
    'RegionName="'+DMMain.EncodeText4XML(ObjName)+'" '+
    'XPos="'+IntToStr(Obj_X)+'" '+
    'YPos="'+IntToStr(Obj_Y)+'" '+
    'Width="'+IntToStr(Obj_W)+'" '+
    'Height="'+IntToStr(Obj_H)+'" '+
    'RegionColor="'+IntToStr(RegionColor)+'" '+
    'TablePrefix="'+IntToStr(TablePrefix)+'" '+
    'TableType="'+IntToStr(TableType)+'" '+
    'OverwriteTablePrefix="'+IntToStr(Ord(OverwriteTablePrefix))+'" '+
    'OverwriteTableType="'+IntToStr(Ord(OverwriteTableType))+'" '+
    'Comments="'+DMMain.EncodeText4XML(Comments)+'" '+
    'IsLinkedObject="'+IntToStr(Ord(IsLinkedObject))+'" '+
    'IDLinkedModel="'+IntToStr(IDLinkedModel)+'" '+
    'Obj_id_Linked="'+IntToStr(Obj_id_Linked)+'" '+
    'OrderPos="'+IntToStr(OrderPos)+'" '+ 
    '/>'+#13#10;

  GetXML:=s;
end;

{$IFDEF USE_IXMLDBMODELType}
procedure TEERRegion.SetXML(theXMLRegion: IXMLREGIONType);
var s: string;
begin
  try
    obj_id:=theXMLRegion.ID;
    ObjName:=DMMain.DecodeXMLText(theXMLRegion.RegionName);
    Obj_X:=theXMLRegion.XPos;
    Obj_Y:=theXMLRegion.YPos;
    Obj_W:=theXMLRegion.Width;
    Obj_H:=theXMLRegion.Height;

    try
      IsLinkedObject:=(theXMLRegion.IsLinkedObject=1);
      IDLinkedModel:=StrToInt(theXMLRegion.IDLinkedModel);
      Obj_id_Linked:=StrToInt(theXMLRegion.Obj_id_Linked);
    except
      IsLinkedObject:=False;
      IDLinkedModel:=-1;
      Obj_id_Linked:=-1;
    end;

    RegionColor:=theXMLRegion.RegionColor;

    TablePrefix:=theXMLRegion.TablePrefix;
    TableType:=theXMLRegion.TableType;

    OverwriteTablePrefix:=(theXMLRegion.OverwriteTablePrefix=1);
    OverwriteTableType:=(theXMLRegion.OverwriteTableType=1);

    Comments:=DMMain.DecodeXMLText(theXMLRegion.Comments);
  except
    on x: Exception do
    begin
      s:=ObjName+#13#10+
        'Error: '+x.Message;

      ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Region from XML File:'+#13#10+
        'Regionname: %s', 81, s));
    end;
  end;
end;
{$ENDIF}

procedure TEERRegion.SetXML2(theXMLParser: TXmlParser);
var s: string;
begin
  try
    obj_id:=StrToInt(theXMLParser.CurAttr.Value('ID'));
    ObjName:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('RegionName'));
    Obj_X:=StrToInt(theXMLParser.CurAttr.Value('XPos'));
    Obj_Y:=StrToInt(theXMLParser.CurAttr.Value('YPos'));
    Obj_W:=StrToInt(theXMLParser.CurAttr.Value('Width'));
    Obj_H:=StrToInt(theXMLParser.CurAttr.Value('Height'));

    try
      IsLinkedObject:=(theXMLParser.CurAttr.Value('IsLinkedObject')='1');
      IDLinkedModel:=StrToInt(theXMLParser.CurAttr.Value('IDLinkedModel'));
      Obj_id_Linked:=StrToInt(theXMLParser.CurAttr.Value('Obj_id_Linked'));
    except
      IsLinkedObject:=False;
      IDLinkedModel:=-1;
      Obj_id_Linked:=-1;
    end;

    RegionColor:=StrToInt(theXMLParser.CurAttr.Value('RegionColor'));

    TablePrefix:=StrToInt(theXMLParser.CurAttr.Value('TablePrefix'));
    TableType:=StrToInt(theXMLParser.CurAttr.Value('TableType'));

    OverwriteTablePrefix:=(theXMLParser.CurAttr.Value('OverwriteTablePrefix')='1');
    OverwriteTableType:=(theXMLParser.CurAttr.Value('OverwriteTableType')='1');

    Comments:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('Comments'));

    try
      OrderPos:=StrToInt(theXMLParser.CurAttr.Value('OrderPos'));
    except
    end;    
  except
    on x: Exception do
    begin
      s:=ObjName+#13#10+
        'Error: '+x.Message;

      ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Region from XML File:'+#13#10+
        'Regionname: %s', 81, s));
    end;
  end;
end;

procedure TEERRegion.SelectAllObjsInRegion;
var i: integer;
begin
  with ParentEERModel do
  begin
    for i:=ComponentCount-1 downto 0 do
    begin
      if(Components[I].Classparent=TEERObj)then
      begin
        if(Obj_X<=TEERObj(Components[I]).Obj_X)and
          (Obj_Y<=TEERObj(Components[I]).Obj_Y)and
          (Obj_X+Obj_W>=TEERObj(Components[I]).Obj_X+TEERObj(Components[I]).Obj_W)and
          (Obj_Y+Obj_H>=TEERObj(Components[I]).Obj_Y+TEERObj(Components[I]).Obj_H)then
        begin
          TEERObj(Components[I]).SetSelected(True);
          TEERObj(Components[I]).DoPaint(self);
        end;
      end;
    end;
  end;
end;

procedure TEERRegion.GetEERObjsInRegion(ObjType: TEERObjectSet; ObjectList: TList; OnlySelected: Boolean=False);
var i: integer;
begin
  with ParentEERModel do
  begin
    for i:=ComponentCount-1 downto 0 do
    begin
      if((Components[I] is TEERTable)and(EERTable in ObjType))or
        ((Components[I] is TEERRel)and(EERRelation in ObjType))or
        ((Components[I] is TEERRegion)and(EERRegion in ObjType))or
        ((Components[I] is TEERImage)and(EERImage in ObjType))or
        ((Components[I] is TEERNote)and(EERNote in ObjType))or
        ((EERAllObjects in ObjType))then
      begin
        if(Obj_X<=TEERObj(Components[I]).Obj_X)and
          (Obj_Y<=TEERObj(Components[I]).Obj_Y)and
          (Obj_X+Obj_W>=TEERObj(Components[I]).Obj_X+TEERObj(Components[I]).Obj_W)and
          (Obj_Y+Obj_H>=TEERObj(Components[I]).Obj_Y+TEERObj(Components[I]).Obj_H)and
          (((OnlySelected)and(TEERObj(Components[I]).Selected))or
            (Not(OnlySelected)))then
        begin
          ObjectList.Add(Components[I]);
        end;
      end;
    end;
  end;
end;

procedure TEERRegion.SetSelected(select: Boolean);
begin
  inherited SetSelected(select);

  if(Visible)then
    TEERModel(Parent).Refresh;
end;

// -----------------------------------------------
// Implementation of the EER-Image

constructor TEERImage.Create(AOwner: TComponent; TheName: string);
begin
  inherited Create(AOwner);

  Parent:=TWidgetControl(AOwner);
  Name:=DMMain.GetValidObjectName(TheName);

  Visible:=False;

  ObjName:=Name;

  Img:=TBitmap.Create;
  Img.Width:=0;
  StrechedImg:=TBitmap.Create;

  StrechImg:=True;

  PopupMenu:=ParentEERModel.PopupMenuEERImage;
end;

destructor TEERImage.Destroy;
begin
  Img.Free;
  StrechedImg.Free;

  inherited;
end;

procedure TEERImage.DeleteObj;
begin
  //Log the Delete-Action if the Flag is set
  if(ParentEERModel.LogActions)then
    ParentEERModel.LogSubAction(at_DeleteObj, Obj_id, GetObjAsXMLModel);

  Free;
end;


procedure TEERImage.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseDown(Sender, Button, Shift, X, Y);

  if(Button=mbLeft)then
  begin
    //If Worktool wtSize is the current tool, store obj w/h
    if(DMEER.CurrentWorkTool=wtSize)then
    begin
      mouse_posx:=Width;
      mouse_posy:=Height;

      MouseIsDown:=True;

      ParentEERModel.StartSubActionLog(at_ScaleObj);
      ParentEERModel.LogSubAction(sa_ScaleFrom, Obj_id,
        'Obj_W='+IntToStr(Obj_W)+#13#10+'Obj_H='+IntToStr(Obj_H));

      ObjChanged:=False;
    end;
  end;
end;

procedure TEERImage.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseMove(Sender, Shift, X, Y);

  // If Worktool wtMove is the current tool, move obj
  if(Shift=[ssLeft])and(MouseIsDown)then
  begin
    if(DMEER.CurrentWorkTool=wtSize)and
      (Not(TEERModel(Parent).ReadOnly))then
    begin
      Obj_W:=ReEvalZoomFac(mouse_posx+Mouse.CursorPos.X-mouse_absx);
      Obj_H:=ReEvalZoomFac(mouse_posy+Mouse.CursorPos.Y-mouse_absy);

      if(Obj_W<5)then
        Obj_W:=5;

      if(Obj_H<5)then
        Obj_H:=5;

      //Use PositionGrid if selected
      if(ParentEERModel.UsePositionGrid)then
      begin
        Obj_W:=(Obj_W div ParentEERModel.PositionGrid.X) * ParentEERModel.PositionGrid.X;
        Obj_H:=(Obj_H div ParentEERModel.PositionGrid.Y) * ParentEERModel.PositionGrid.Y;
      end;        

      RefreshObj;
      
      ObjChanged:=True;
    end;
  end;

end;

procedure TEERImage.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseUp(Sender, Button, Shift, X, Y);

  if(Button=mbLeft)then
  begin
    if(ObjChanged=True)and
      (DMEER.CurrentWorkTool=wtSize)and(StrechImg)then
    begin
      RefreshObj;
      DoPaint(self);

      //Log Move
      ParentEERModel.LogSubAction(sa_ScaleTo, Obj_id,
        'Obj_W='+IntToStr(Obj_W)+#13#10+'Obj_H='+IntToStr(Obj_H));

      //Close Log
      ParentEERModel.EndSubAction;

      if(Not(ParentEERModel.DisableModelRefresh))then
        DMEER.RefreshInfoPalette;

      ObjChanged:=False;
    end
    //if the obj has not been moved, clear move action
    else if(DMEER.CurrentWorkTool=wtSize)then
      ParentEERModel.DeleteOpenAction;
  end;
end;

procedure TEERImage.ShowEditor(Sender: TObject);
begin
  //Show Image Editor
  if(DMEER.CurrentWorkTool=wtPointer)or
    (DMEER.CurrentWorkTool=wtMove)then
  begin
    MouseIsDown:=False;

    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_EditImage, self));

    //Process Mouse Messages before setting EditorIsCalled to false
    Application.ProcessMessages;

    EditorIsCalled:=False;
  end;
end;

procedure TEERImage.PaintObj2Canvas(theCanvas: TCanvas; xo, yo: integer);
var width, height: integer;
begin
  if(ParentEERModel.DisableModelRefresh)then
    Exit;

  width:=EvalZoomFac(Obj_W);
  height:=EvalZoomFac(Obj_H);

  with theCanvas do
  begin
    if(Img.Width>0)and(not((MouseIsDown=True)and(DMEER.CurrentWorkTool=wtSize)and(StrechImg)))then
    begin
      Pen.Style:=psClear;
      Brush.Color:=clWhite;
      Rectangle(Rect(xo+0, yo+0, xo+width-1, yo+height-1));

      if(StrechImg)then
        Draw(xo+0, yo+0, StrechedImg)
      else
        Draw(xo+0, yo+0, Img);
    end
    else
    begin
      Pen.Style:=psSolid;
      Pen.Color:=clGray;
      Brush.Color:=clWhite;
      Rectangle(Rect(xo+0, yo+0, xo+width-1, yo+height-1));

      MoveTo(xo+0, yo+0);
      LineTo(xo+width-1, yo+height-1);
      MoveTo(xo+width-1, yo+0);
      LineTo(xo+0, yo+height-1);
    end;

    // Paint selection
    if(selected)then
    begin
      Pen.Color:=clWhite;
      MoveTo(xo+0, yo+0);
      LineTo(xo+0, yo+height-1);
      LineTo(xo+width-1, yo+height-1);
      LineTo(xo+width-1, yo+0);
      LineTo(xo+0, yo+0);

      Pen.Color:=clBlack;
      Pen.Style:=psDot;
      MoveTo(xo+0, yo+0);
      LineTo(xo+0, yo+height-1);
      LineTo(xo+width-1, yo+height-1);
      LineTo(xo+width-1, yo+0);
      LineTo(xo+0, yo+0);

      Pen.Style:=psSolid;
    end;
  end;
end;

procedure TEERImage.DoPaint(Sender: TObject);
begin
  PaintObj(Canvas);
end;

procedure TEERImage.RefreshObj;
begin
  //Only reposition Obj when the model is not drawn to another canvas
  if(Not(ParentEERModel.PaintingToSpecialCanvas))then
  begin
    Left:=EvalZoomFac(Obj_X);
    Top:=EvalZoomFac(Obj_Y);
    Width:=EvalZoomFac(Obj_W);
    Height:=EvalZoomFac(Obj_H);
  end;

  if(StrechImg)and(Img.Width>0)and
    ((StrechedImg.Width<>EvalZoomFac(Obj_W))or
    (StrechedImg.Height<>EvalZoomFac(Obj_H)))and
    (not((MouseIsDown=True)and(DMEER.CurrentWorkTool=wtSize)))then
  begin
    StrechedImg.Width:=EvalZoomFac(Obj_W);
    StrechedImg.Height:=EvalZoomFac(Obj_H);

    StrechedImg.Canvas.Pen.Style:=psClear;
    StrechedImg.Canvas.Brush.Color:=clWhite;
    StrechedImg.Canvas.Rectangle(0, 0, Width+1, Height+1);
    StrechedImg.Canvas.Pen.Style:=psSolid;

    StrechedImg.Canvas.StretchDraw(Rect(0, 0,
      EvalZoomFac(Obj_W)-1, EvalZoomFac(Obj_H)-1), Img);
  end;
end;


procedure TEERImage.LoadImageFromFile;
var theOpenDialog: TOpenDialog;
  RecentOpenImageDir: string;
begin
  theOpenDialog:=TOpenDialog.Create(nil);
  try
{$IFDEF MSWINDOWS}
    //On Windows use native Win32 Open Dlg
    theOpenDialog.UseNativeDialog:=True;
    theOpenDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
{$ENDIF}

    //Get last dir
    RecentOpenImageDir:=DMMain.LoadValueFromSettingsIniFile('RecentDirectories', 'RecentOpenImageDir', '');

    if(RecentOpenImageDir='')or(Not(DirectoryExists(RecentOpenImageDir)))then
      RecentOpenImageDir:=ExtractFilePath(Application.ExeName)+
        'Models'+PathDelim;

    theOpenDialog.Title:=DMMain.GetTranslatedMessage('Open an Image ...', 82);
    theOpenDialog.Width:=600;
    theOpenDialog.Height:=450;
    theOpenDialog.DefaultExt:='png';
    theOpenDialog.InitialDir:=RecentOpenImageDir;


    {theOpenDialog.Position:=Point((Screen.Width-theOpenDialog.Width) div 2,
      (Screen.Height-theOpenDialog.Height) div 2);}

    theOpenDialog.Filter:='PNG files (*.png)|*.png|Bitmap files (*.bmp)|*.bmp|All files (*.*)|*.*';

    if(theOpenDialog.Execute)then
    begin
      RecentOpenImageDir:=ExtractFilePath(theOpenDialog.Filename);

      Img.LoadFromFile(theOpenDialog.Filename);

      if(Obj_W<2)then
      begin
        Obj_W:=Img.Width;
        Obj_H:=Img.Height;
      end;

      //Repaint the Image
      RefreshObj;
      StrechedImg.Width:=Width;
      StrechedImg.Height:=Height;
      if(StrechImg)then
        StrechedImg.Canvas.StretchDraw(Rect(0, 0,
          Width-2, Height-2), Img);
      PaintObj(Canvas);
      //Picture repainted

      DMMain.SaveValueInSettingsIniFile('RecentDirectories', 'RecentOpenImageDir', RecentOpenImageDir);
    end;

  finally
    theOpenDialog.Free;
  end;
end;

function TEERImage.GetXML: string;
var s: string;
  theImgFile: TMemoryStream;
begin
  s:=s+'<IMAGE '+
    'ID="'+IntToStr(Obj_id)+'" '+
    'ImageName="'+DMMain.EncodeText4XML(ObjName)+'" '+
    'XPos="'+IntToStr(Obj_X)+'" '+
    'YPos="'+IntToStr(Obj_Y)+'" '+
    'Width="'+IntToStr(Obj_W)+'" '+
    'Height="'+IntToStr(Obj_H)+'" '+
    'StrechImg="'+IntToStr(Ord(StrechImg))+'" '+
    'ImgWidth="'+IntToStr(Img.Width)+'" '+
    'ImgHeight="'+IntToStr(Img.Height)+'" '+
    'ImgFormat="'+Img.Format+'" '+
    'ImgData="';

  //Create a memory stream to convert image data
  theImgFile:=TMemoryStream.Create;
  try
    Img.SaveToStream(theImgFile);

    s:=s+DMMain.EncodeStreamForXML(theImgFile);
  finally
    FreeAndNil(theImgFile);
  end;

  s:=s+'" '+
    'IsLinkedObject="'+IntToStr(Ord(IsLinkedObject))+'" '+
    'IDLinkedModel="'+IntToStr(IDLinkedModel)+'" '+
    'Obj_id_Linked="'+IntToStr(Obj_id_Linked)+'" '+
    'OrderPos="'+IntToStr(OrderPos)+'" '+
    '/>'+#13#10;

  GetXML:=s;
end;

{$IFDEF USE_IXMLDBMODELType}
procedure TEERImage.SetXML(theXMLImage: IXMLIMAGEType);
var imgdata: string;
  theImgFile: TMemoryStream;
begin
  try
    Obj_id:=theXMLImage.ID;
    ObjName:=DMMain.DecodeXMLText(theXMLImage.ImageName);
    Obj_X:=theXMLImage.XPos;
    Obj_Y:=theXMLImage.YPos;
    Obj_W:=theXMLImage.Width;
    Obj_H:=theXMLImage.Height;

    try
      IsLinkedObject:=(theXMLImage.IsLinkedObject=1);
      IDLinkedModel:=StrToInt(theXMLImage.IDLinkedModel);
      Obj_id_Linked:=StrToInt(theXMLImage.Obj_id_Linked);
    except
      IsLinkedObject:=False;
      IDLinkedModel:=-1;
      Obj_id_Linked:=-1;
    end;

    StrechImg:=(theXMLImage.StrechImg=1);
    Img.Width:=theXMLImage.ImgWidth;
    Img.Height:=theXMLImage.ImgHeight;
    Img.Format:=theXMLImage.ImgFormat;

    imgdata:=theXMLImage.ImgData;

    theImgFile:=TMemoryStream.Create;
    try
      DMMain.DecodeStreamFromXML(imgdata, theImgFile);
      Img.LoadFromStream(theImgFile);
    finally
      FreeAndNil(theImgFile);
    end;

    StrechedImg.Width:=1;
    StrechedImg.Height:=1;

    RefreshObj;

  except
    on x: Exception do
    begin
      ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Image from XML File:'+#13#10+
        'Imagename: %s'+#13#10+
        'Error: %s', 211, ObjName, x.Message));
    end;
  end;
end;
{$ENDIF}

procedure TEERImage.SetXML2(theXMLParser: TXmlParser);
var imgdata: string;
  theImgFile: TMemoryStream;
begin
  try
    Obj_id:=StrToInt(theXMLParser.CurAttr.Value('ID'));
    ObjName:=DMMain.DecodeXMLText(theXMLParser.CurAttr.Value('ImageName'));
    Obj_X:=StrToInt(theXMLParser.CurAttr.Value('XPos'));
    Obj_Y:=StrToInt(theXMLParser.CurAttr.Value('YPos'));
    Obj_W:=StrToInt(theXMLParser.CurAttr.Value('Width'));
    Obj_H:=StrToInt(theXMLParser.CurAttr.Value('Height'));

    try
      IsLinkedObject:=(theXMLParser.CurAttr.Value('IsLinkedObject')='1');
      IDLinkedModel:=StrToInt(theXMLParser.CurAttr.Value('IDLinkedModel'));
      Obj_id_Linked:=StrToInt(theXMLParser.CurAttr.Value('Obj_id_Linked'));
    except
      IsLinkedObject:=False;
      IDLinkedModel:=-1;
      Obj_id_Linked:=-1;
    end;

    StrechImg:=(theXMLParser.CurAttr.Value('StrechImg')='1');
    Img.Width:=StrToInt(theXMLParser.CurAttr.Value('ImgWidth'));
    Img.Height:=StrToInt(theXMLParser.CurAttr.Value('ImgHeight'));
    Img.Format:=theXMLParser.CurAttr.Value('ImgFormat');

    imgdata:=theXMLParser.CurAttr.Value('ImgData');

    theImgFile:=TMemoryStream.Create;
    try
      DMMain.DecodeStreamFromXML(imgdata, theImgFile);
      Img.LoadFromStream(theImgFile);
    finally
      FreeAndNil(theImgFile);
    end;

    StrechedImg.Width:=1;
    StrechedImg.Height:=1;

    try
      OrderPos:=StrToInt(theXMLParser.CurAttr.Value('OrderPos'));
    except
    end;

    RefreshObj;

  except
    on x: Exception do
    begin
      ShowMessage(DMMain.GetTranslatedMessage('An Error occurred while reading Image from XML File:'+#13#10+
        'Imagename: %s'+#13#10+
        'Error: %s', 211, ObjName, x.Message));
    end;
  end;
end;

function TEERImage.GetStrechImg: Boolean;
begin
  GetStrechImg:=StrechImg;
end;

procedure TEERImage.SetStrechImg(StrechImg: Boolean);
begin
  self.StrechImg:=StrechImg;
end;

procedure TEERImage.ClearImg;
begin
  StrechedImg.FreeImage;
  Img.FreeImage;
  Img.Width:=0;
  Img.Height:=0;
end;

function TEERImage.GetImgSize: TSize;
begin
  GetImgSize.cx:=Img.Width;
  GetImgSize.cy:=Img.Height;
end;

// -----------------------------------------------

constructor TEERStoredProc.Create(AOwner: TComponent; TheName: string);
begin
  inherited Create(AOwner);

  Parent:=TWidgetControl(AOwner);
  Name:=DMMain.GetValidObjectName(TheName);

  Font.Name:=ParentEERModel.DefModelFont;

  Visible:=False;

  ObjName:=Name;

  Code:=TStringList.Create;
end;

destructor TEERStoredProc.Destroy;
begin
  Code.Free;

  inherited;
end;

// -----------------------------------------------

constructor TEERColumn.Create(AOwner: TComponent);
begin
  Owner:=AOwner;

  inherited Create;
end;

destructor TEERColumn.Destroy;
begin
  inherited;
end;

procedure TEERColumn.Assign(Source: TPersistent);
var theSource: TEERColumn;
begin
  if(Source is TEERColumn)then
  begin
    theSource:=TEERColumn(Source);

    if(theSource.Owner<>nil)then
      Owner:=theSource.Owner;

    ColName:=theSource.Colname;
    PrevColName:=theSource.PrevColName;
    Obj_id:=theSource.Obj_id;
    Pos:=theSource.Pos;
    idDatatype:=theSource.idDatatype;
    DatatypeParams:=theSource.DatatypeParams;
    Width:=theSource.Width;
    Prec:=theSource.Prec;
    PrimaryKey:=theSource.PrimaryKey;
    NotNull:=theSource.NotNull;
    AutoInc:=theSource.AutoInc;
    IsForeignKey:=theSource.IsForeignKey;
    FK_checked:=theSource.FK_checked;
    OptionSelected:=theSource.OptionSelected;
    DefaultValue:=theSource.DefaultValue;
    Comments:=theSource.Comments;
  end
  else
    inherited Assign(Source);
end;

function TEERColumn.ObjIsEqualTo(Source: TObject): Boolean;
var i: integer;
begin
  ObjIsEqualTo:=False;

  if(Source is TEERColumn)then
  begin
    if(ColName=TEERColumn(Source).Colname)and
      (PrevColName=TEERColumn(Source).PrevColName)and
      (Obj_id=TEERColumn(Source).Obj_id)and
      (Pos=TEERColumn(Source).Pos)and
      (idDatatype=TEERColumn(Source).idDatatype)and
      (DatatypeParams=TEERColumn(Source).DatatypeParams)and
      (Width=TEERColumn(Source).Width)and
      (Prec=TEERColumn(Source).Prec)and
      (PrimaryKey=TEERColumn(Source).PrimaryKey)and
      (NotNull=TEERColumn(Source).NotNull)and
      (AutoInc=TEERColumn(Source).AutoInc)and
      (IsForeignKey=TEERColumn(Source).IsForeignKey)and
      (FK_checked=TEERColumn(Source).FK_checked)and
      (DefaultValue=TEERColumn(Source).DefaultValue)and
      (Comments=TEERColumn(Source).Comments)then
    begin
      ObjIsEqualTo:=True;

      for i:=0 to 5 do
        if(OptionSelected[i]<>TEERColumn(Source).OptionSelected[i])then
        begin
          ObjIsEqualTo:=False;
          break;
        end;
    end;
  end;
end;

// -----------------------------------------------

constructor TEERIndex.Create(AOwner: TComponent);
begin
  Owner:=AOwner;

  Columns:=TStringList.Create;
  ColumnParams:=TStringList.Create;

  IndexKind:=ik_INDEX;
  FKRefDef_Obj_id:=-1;

  inherited Create;
end;

destructor TEERIndex.Destroy;
begin
  Columns.Free;
  ColumnParams.Free;

  inherited;
end;

procedure TEERIndex.Assign(Source: TPersistent);
begin
  if(Source is TEERIndex)then
  begin
    Owner:=TEERIndex(Source).Owner;

    IndexName:=TEERIndex(Source).IndexName;
    Obj_id:=TEERIndex(Source).Obj_id;
    Pos:=TEERIndex(Source).Pos;
    IndexKind:=TEERIndex(Source).IndexKind;

    Columns.Assign(TEERIndex(Source).Columns);
    ColumnParams.Assign(TEERIndex(Source).ColumnParams);

    FKRefDef_Obj_id:=TEERIndex(Source).FKRefDef_Obj_id;
  end
  else
    inherited Assign(Source);
end;

function TEERIndex.ObjIsEqualTo(Source: TObject): Boolean;
begin
  ObjIsEqualTo:=False;

  if(Source is TEERIndex)then
  begin
    if(IndexName=TEERIndex(Source).IndexName)and
      (Obj_id=TEERIndex(Source).Obj_id)and
      (Pos=TEERIndex(Source).Pos)and
      (IndexKind=TEERIndex(Source).IndexKind)and
      (Columns.Text=TEERIndex(Source).Columns.Text)and
      (ColumnParams.Text=TEERIndex(Source).ColumnParams.Text)and
      (FKRefDef_Obj_id=TEERIndex(Source).FKRefDef_Obj_id)then
      ObjIsEqualTo:=True;
  end;
end;

// -----------------------------------------------

constructor TEERDatatypeGroup.Create(AOwner: TComponent);
begin
  Owner:=AOwner;

  inherited Create;
end;

destructor TEERDatatypeGroup.Destroy;
begin
  inherited;
end;



// -----------------------------------------------

constructor TEERDatatype.Create(AOwner: TComponent);
begin
  Owner:=AOwner;

  PhysicalMapping:=False;
  PhysicalTypeName:='';

  inherited Create;
end;

destructor TEERDatatype.Destroy;
begin
  inherited;
end;

procedure TEERDatatype.Assign(Source: TPersistent);
var theSource: TEERDatatype;
begin
  if(Source is TEERDatatype)then
  begin
    theSource:=TEERDatatype(Source);

    Owner:=theSource.Owner;

    id:=theSource.id;
    group:=theSource.group;
    TypeName:=theSource.TypeName;
    description:=theSource.description;
    Param:=theSource.Param;
    Options:=theSource.Options;
    ParamCount:=theSource.ParamCount;
    OptionCount:=theSource.OptionCount;
    ParamRequired:=theSource.ParamRequired;
    OptionDefaults:=theSource.OptionDefaults;
    EditParamsAsString:=theSource.EditParamsAsString;
    SynonymGroup:=theSource.SynonymGroup;
  end
  else
    inherited Assign(Source);
end;

function TEERDatatype.GetPhysicalTypeName: string;
begin
  if(PhysicalMapping)and(PhysicalTypeName<>'')then
    GetPhysicalTypeName:=PhysicalTypeName
  else
    GetPhysicalTypeName:=TypeName;
end;

// -----------------------------------------------

constructor TEERPluginData.Create(AOwner: TComponent);
begin
  Owner:=AOwner;

  Params:=TStringList.Create;

  inherited Create;
end;

destructor TEERPluginData.Destroy;
begin
  Params.Free;

  inherited;
end;

// -----------------------------------------------

constructor TEERActionLog.Create(AOwner: TComponent);
begin
  Owner:=AOwner;

  SubActions:=TObjectList.Create;

  inherited Create;
end;

destructor TEERActionLog.Destroy;
begin
  SubActions.Free;

  inherited;
end;

// -----------------------------------------------

constructor TEERActionSubLog.Create(AOwner: TComponent);
begin
  Owner:=AOwner;

  Params:=TStringList.Create;

  inherited Create;
end;

destructor TEERActionSubLog.Destroy;
begin
  Params.Free;

  inherited;
end;

// -----------------------------------------------

constructor TStoredSQLCmd.Create(SQLCmdType: integer; StoredPosition: string; SQLText: string);
begin
  self.SQLCmdType:=SQLCmdType;
  self.StoredPosition:=StoredPosition;
  self.SQLText:=SQLText;
end;

// -----------------------------------------------

constructor TEERLinkedModel.Create(AOwner: TComponent);
begin
  self.Owner:=AOwner;

  IDLinkedModel:=TEERModel(AOwner).GetNextIDPlacedModel;

  inherited Create;
end;

function TEERTable.getSqlTableComment(TableName, Comment,
  DatabaseType: string): string;
var
  ColName : string;    //Column Name
  ColComment : string; //Column Comment
  i : integer;
begin
  RemoveCRFromString(Comment);

  if (DatabaseType = 'Oracle') and (length(trim(Comment))>0) then
  begin
    result := 'COMMENT ON TABLE '+TableName+' IS '''+
     StringReplace (Comment, '''', '''''', [rfReplaceAll]) + // converts ' into ''
     ''';'+#13#10;
  end;

  for i:=0 to Columns.Count-1 do
  begin

    //Column Name
    ColName := TEERColumn(Columns[i]).ColName;

    //Column Comment
    ColComment := TEERColumn(Columns[i]).Comments;

    if length(trim(ColComment))>0 then
    begin

      result :=
        result +
        getSqlColumnComment(TableName, ColName, ColComment, DatabaseType) +
        #13#10;

    end; // of if

  end;// of for;

end; // of function

function TEERTable.getSqlColumnComment(TableName, ColumnName, Comment,
  DatabaseType: string): string;
begin
  result := '';
  RemoveCRFromString(Comment);

  if DatabaseType = 'Oracle' then
  begin
    result := 'COMMENT ON COLUMN '+TableName+'.'+ColumnName+' IS '''+
      StringReplace (Comment, '''', '''''', [rfReplaceAll]) + // converts ' into ''
      ''';';
  end;
end;


//Create triggers to insert next sequence val into auto increment fields
function TEERTable.getTriggerForSequences(SeqName, DatabaseType, PrefixName,
  Field: string): string;
var
  AuxTriggerBody: TStringList;
begin
  AuxTriggerBody := TStringList.Create;
  
  if DatabaseType = 'FireBird' then
  begin
    AuxTriggerBody.Add('BEGIN ');
    AuxTriggerBody.Add('  IF (NEW.' + Field + ' IS NULL) THEN ');
    AuxTriggerBody.Add('    NEW.' + Field + ' = GEN_ID('+SeqName+', 1); ');
    AuxTriggerBody.Add('END! ');

    getTriggerForSequences := GetTriggerSql(DatabaseType,
                                AuxTriggerBody.Text,
                                GetSQLTableName,
                                Copy(PrefixName + GetSQLTableName, 1, 30),
                                'BEFORE INSERT  '
                              ); 
  end else
  if DatabaseType = 'Oracle' then
  begin
    AuxTriggerBody.Add('FOR EACH ROW ');
    AuxTriggerBody.Add('BEGIN ');
    AuxTriggerBody.Add('  IF (:NEW.' + Field + ' IS NULL) THEN ');
    AuxTriggerBody.Add('    SELECT '+SeqName+'.NEXTVAL INTO :NEW.' + Field + ' FROM DUAL; ');
    AuxTriggerBody.Add('  END IF; ');
    AuxTriggerBody.Add('END; ');

    getTriggerForSequences := GetTriggerSql(DatabaseType,
                                AuxTriggerBody.Text,
                                GetSQLTableName,
                                Copy(PrefixName + GetSQLTableName, 1, 30),
                                'BEFORE INSERT '
                              );
  end;

  AuxTriggerBody.Free;
end;


//Create triggers to insert the record time changed in the specified created column
function TEERTable.GetTriggersForLastChangeDate(ColumnName, PrefixName,
  DBType: string; pkFields: TStringList): string;
var
  AuxTriggerBody: TStringList;
begin
  AuxTriggerBody := TStringList.Create;

  if DBType = 'Oracle' then   // ORACLE TRIGGER
  begin
     AuxTriggerBody.Add('FOR EACH ROW ');
     AuxTriggerBody.Add('BEGIN');
     AuxTriggerBody.Add('  :NEW.'+ColumnName+
                        ' :=  TO_CHAR(SYSTIMESTAMP, ''YYMMDDHH24MISSFF3'');');
     AuxTriggerBody.Add('END;');

     GetTriggersForLastChangeDate := GetTriggerSql(DBType,
                                       AuxTriggerBody.Text,
                                       GetSQLTableName,
                                       Copy(PrefixName + GetSQLTableName, 1, 30),
                                       'BEFORE INSERT OR UPDATE '
                                     );
  end else
  if DBType = 'FireBird' then    //FIREBIRD
  begin
    AuxTriggerBody.Add('BEGIN ');
    AuxTriggerBody.Add('New.' + ColumnName + ' = ');
    AuxTriggerBody.Add('  substr(CURRENT_TIMESTAMP, 3,   4) || ');
    AuxTriggerBody.Add('  substr(CURRENT_TIMESTAMP, 6,   7) || ');
    AuxTriggerBody.Add('  substr(CURRENT_TIMESTAMP, 9,  10) || ');
    AuxTriggerBody.Add('  substr(CURRENT_TIMESTAMP, 12, 13) || ');
    AuxTriggerBody.Add('  substr(CURRENT_TIMESTAMP, 15, 16) || ');
    AuxTriggerBody.Add('  substr(CURRENT_TIMESTAMP, 18, 19) || ');
    AuxTriggerBody.Add('  substr(CURRENT_TIMESTAMP, 21, 23);   ');
    AuxTriggerBody.Add('END! ');

    GetTriggersForLastChangeDate := GetTriggerSql(DBType,
                                       AuxTriggerBody.Text,
                                       GetSQLTableName,
                                       Copy(PrefixName + GetSQLTableName, 1, 30),
                                       'BEFORE INSERT OR UPDATE '
                                     ); 
  end else
  if DBType = 'SQL Server' then  //SQL SERVER
  begin
    AuxTriggerBody.Add('BEGIN ');
    AuxTriggerBody.Add('    declare @dt varchar(15) ');
    AuxTriggerBody.Add('    set @dt = (select replace(CONVERT(VARCHAR(6),GETDATE(),12)+CONVERT(VARCHAR,GETDATE(),14), '':'', '''')) ');
    AuxTriggerBody.Add('    UPDATE '+ GetSQLTableName +' SET '+ColumnName+' = @dt ');
    AuxTriggerBody.Add('    FROM '+ GetSQLTableName +' TAB INNER JOIN inserted I ON ('+GetPkJoin('TAB', 'I', pkFields)+') ');
    AuxTriggerBody.Add('    WHERE TAB.'+ColumnName+' < @dt OR TAB.'+ColumnName+' IS NULL ');
    AuxTriggerBody.Add('END;');

    GetTriggersForLastChangeDate := GetTriggerSql(DBType,
                                       AuxTriggerBody.Text,
                                       GetSQLTableName,
                                       Copy(PrefixName + GetSQLTableName, 1, 30),
                                       ' AFTER INSERT, UPDATE '
                                     );
  end;

  AuxTriggerBody.Free;
end;

//Build a sql join wit composed pk
function TEERTable.GetPkJoin(Tab1, Tab2: String; PKs: TStringList): String;
var
  PkIndex: integer;
begin
  Result := '';
  
  for PkIndex := 0 to PKs.Count - 1 do
  begin
    if PkIndex > 0 then
      Result := Result + ' AND ';

    Result := Result + Tab1+'.'+PKs[PkIndex]+ ' = ' + Tab2+'.'+PKs[PkIndex];
  end
end;

function TEERTable.GetTriggerSql(DbType, TriggerBody, Table, TriggerName,
  TriggerEvent: String): String;
var
  Str : TStringList;
begin
  Str := TStringList.Create; 
  try
    if DbType = 'Oracle' then //Generate Oracle trigger definition
    begin
      Str.Add('CREATE OR REPLACE TRIGGER ' + TriggerName);
      Str.Add(TriggerEvent + ' ON ' + Table);
      Str.Add(TriggerBody);
      Str.Add('/');
    end else
    if DbType = 'SQL Server' then //Generate SQL Server trigger definition
    begin
      Str.Add('CREATE TRIGGER ' + TriggerName);
      Str.Add('ON ' + Table + ' ' + TriggerEvent);
      Str.Add('AS ');
      Str.Add(TriggerBody);
      Str.Add('GO ');
    end else
    if DbType = 'FireBird' then //Generate Firebird trigger definition
    begin
      Str.Add('SET TERM !; ');
      Str.Add('CREATE TRIGGER ' + TriggerName);
      Str.Add('FOR ' + Table + ' ' + TriggerEvent);
      Str.Add('AS ');
      Str.Add(TriggerBody);
      Str.Add('SET TERM ;! ');
    end;
    GetTriggerSql := Str.Text;
  finally
    Str.Free;
  end;
end;

//Triggers to last delete datetime
function TEERTable.GetTriggerForLastDeleteDate(TbName, ColName, PrefixName,
  DbType: string): string;
var
  AuxTriggerBody: TStringList;
begin
  AuxTriggerBody := TStringList.Create;

  if DBType = 'Oracle' then   // ORACLE TRIGGER
  begin
    AuxTriggerBody.Add('DECLARE ');
    AuxTriggerBody.Add('  cnt INTEGER; ');
    AuxTriggerBody.Add('BEGIN ');
    AuxTriggerBody.Add('  SELECT COUNT(*) INTO cnt FROM ' + TbName +
                       '  WHERE TABLE_NAME = ''' + GetSQLTableName +  '''; ');

    AuxTriggerBody.Add('  IF cnt = 1 then ');
    AuxTriggerBody.Add('    UPDATE ' + tbNAme + ' SET ' + ColName + ' = ' +
                            'TO_CHAR(SYSTIMESTAMP, ''YYMMDDHH24MISSFF3'') ' +
                            'WHERE TABLE_NAME = ''' + GetSQLTableName + '''; ');
    AuxTriggerBody.Add('  ELSE ');
    AuxTriggerBody.Add('    INSERT INTO ' + TbName + ' (TABLE_NAME, '+ ColName +') '+
                       '    VALUES ('''+ GetSQLTableName +''', '+
                            'TO_CHAR(SYSTIMESTAMP, ''YYMMDDHH24MISSFF3''));');
    AuxTriggerBody.Add('  END IF;');
    AuxTriggerBody.Add('END; ');


     GetTriggerForLastDeleteDate := GetTriggerSql(DBType,
                                       AuxTriggerBody.Text,
                                       GetSQLTableName,
                                       Copy(PrefixName + GetSQLTableName, 1, 30),
                                       'AFTER DELETE '
                                     );
  end else
  if DBType = 'FireBird' then    //FIREBIRD
  begin
    AuxTriggerBody.Add('Declare variable dt  varchar(15);');
    AuxTriggerBody.Add('Declare variable cnt integer;');
    AuxTriggerBody.Add('BEGIN');
    AuxTriggerBody.Add('  dt = ');
    AuxTriggerBody.Add('       substr(CURRENT_TIMESTAMP, 3,   4) || ');
    AuxTriggerBody.Add('       substr(CURRENT_TIMESTAMP, 6,   7) || ');
    AuxTriggerBody.Add('       substr(CURRENT_TIMESTAMP, 9,  10) || ');
    AuxTriggerBody.Add('       substr(CURRENT_TIMESTAMP, 12, 13) || ');
    AuxTriggerBody.Add('       substr(CURRENT_TIMESTAMP, 15, 16) || ');
    AuxTriggerBody.Add('       substr(CURRENT_TIMESTAMP, 18, 19) || ');
    AuxTriggerBody.Add('       substr(CURRENT_TIMESTAMP, 21, 23);   ');

    AuxTriggerBody.Add('  select count(*) from '+TbName+' where table_name = '''+GetSQLTableName+''' into :cnt;');

    AuxTriggerBody.Add('  if (cnt = 1) then ');
    AuxTriggerBody.Add('    update '+TbName+' set '+ColName+' = :dt '+
                       '      where table_name = '''+GetSQLTableName+''';');
    AuxTriggerBody.Add('  else ');
    AuxTriggerBody.Add('    insert into '+TbName+' (table_name, '+ColName+') '+
                       '    values ('''+GetSQLTableName+''', :dt);');

    AuxTriggerBody.Add('END!');

    GetTriggerForLastDeleteDate := GetTriggerSql(DBType,
                                       AuxTriggerBody.Text,
                                       GetSQLTableName,
                                       Copy(PrefixName + GetSQLTableName, 1, 30),
                                       'AFTER DELETE '
                                     );

  end else
  if DBType = 'SQL Server' then  //SQL SERVER
  begin
    AuxTriggerBody.Add('BEGIN ');
    AuxTriggerBody.Add('  DECLARE @dt VARCHAR(15); ');
    AuxTriggerBody.Add('  set @dt = (select replace(CONVERT(VARCHAR(6),GETDATE(),12)+CONVERT(VARCHAR,GETDATE(),14), '':'', '''')) ');

    AuxTriggerBody.Add('  IF EXISTS (SELECT 1 FROM '+ TbName +' WHERE '+
                       '    TABLE_NAME = '''+ GetSQLTableName +''') ');

    AuxTriggerBody.Add('    UPDATE '+ TbName + ' SET '+ ColName + ' = @dt ' +
                       '    WHERE TABLE_NAME = ''' + GetSQLTableName + ''' ');

    AuxTriggerBody.Add('  ELSE ');
    AuxTriggerBody.Add('    INSERT INTO ' + TbName + '(TABLE_NAME, '+ColName+') '+
                       '    VALUES ('''+GetSQLTableName+''', @dt)');

    AuxTriggerBody.Add('END;');

    GetTriggerForLastDeleteDate := GetTriggerSql(DBType,
                                       AuxTriggerBody.Text,
                                       GetSQLTableName,
                                       Copy(PrefixName + GetSQLTableName, 1, 30),
                                       'AFTER DELETE '
                                     );
  end;

  AuxTriggerBody.Free;
end;

end.

