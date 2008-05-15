unit EditorQuery;

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
// Unit EditorTableData.pas
// ------------------------
// Version 1.1, 31.03.2003, Mike
// Description
//   Editor for table data
//
// Changes:
//   Version 1.1, 31.03.2003, Mike
//     Added 'Load SQL Script from File' Menu to the SQLMemo Popup
//   Version 1.0, 13.03.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

{$I DBDesigner4.inc}

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, FMTBcd, DB, DBClient, Provider, SqlExpr, QButtons,
  QExtCtrls, QDBCtrls, QGrids, QDBGrids, QComCtrls, EERModel, IniFiles,
  QMenus, QClipbrd, {$IFDEF USE_SYNEDIT}QSynEdit, QSynHighlighterSQL, {$ENDIF}
  {$IFDEF MSWINDOWS}Windows, ShellAPI, {$ENDIF}
  Qt, QImgList, EmbeddedPdfDB;

type
  TEditorQueryForm = class(TForm)
    TopPnl: TPanel;
    BottomPnl: TPanel;
    TableLbl: TLabel;
    OutputQry: TSQLQuery;
    OutputDataSetProvider: TDataSetProvider;
    OutputClientDataSet: TClientDataSet;
    OutputDataSrc: TDataSource;
    Label1: TLabel;
    DBConnEd: TEdit;
    GetDBConnSBtn: TSpeedButton;
    TableCBox: TComboBox;
    CommitPnl: TPanel;
    QueryDockPnl: TPanel;
    LeftPnl: TPanel;
    RightPnl: TPanel;
    QueryMainPnl: TPanel;
    BlobSplitter: TSplitter;
    SQLSplitter: TSplitter;
    BlobPnl: TPanel;
    BlobPageControl: TPageControl;
    ClearSheet: TTabSheet;
    TextSheet: TTabSheet;
    DBMemo: TDBMemo;
    ImgSheet: TTabSheet;
    DBImage: TDBImage;
    BlobSheet: TTabSheet;
    Splitter1: TSplitter;
    BlobMemo: TMemo;
    BlobHexMemo: TMemo;
    BlobFuncPnl: TPanel;
    BlobBtnPnl: TPanel;
    BlobClearBtn: TSpeedButton;
    BlobOpenBtn: TSpeedButton;
    StoreBlobBtn: TSpeedButton;
    SQLPnl: TPanel;
    SQLFuncPnl: TPanel;
    SQLMemo: TMemo;
    GridPnl: TPanel;
    DBGrid: TDBGrid;
    GridTopPnl: TPanel;
    GridFuncPnl: TPanel;
    SubmitBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    DBNav: TDBNavigator;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    DBNavigator3: TDBNavigator;
    SaveSQLBtn: TSpeedButton;
    ExecSQLBtn: TSpeedButton;
    SQLHeaderPnl: TPanel;
    SQLMemoPopupMenu: TPopupMenu;
    UndoMI: TMenuItem;
    RedoMI: TMenuItem;
    N1: TMenuItem;
    CopyMI: TMenuItem;
    CutMI: TMenuItem;
    PasteMI: TMenuItem;
    ClearMI: TMenuItem;
    N2: TMenuItem;
    SelectAllMI: TMenuItem;
    StoredSQLPnl: TPanel;
    StoredSQLSplitter: TSplitter;
    StoredSQLTreeView: TTreeView;
    StoredSQLHeaderPnl: TPanel;
    Splitter2: TSplitter;
    StoredSQLImageList: TImageList;
    StoredSQLMemoPnl: TPanel;
    StoredSQLFuncPnl: TPanel;
    StoredSQLEditBtn: TSpeedButton;
    StoredSQLExecuteBtn: TSpeedButton;
    Bevel6: TBevel;
    StoredSQLMemo: TMemo;
    StoredSQLBtn: TSpeedButton;
    StoredSQLPopupMenu: TPopupMenu;
    DeleteSQLCommandMI: TMenuItem;
    ExecuteSQLCommandMI: TMenuItem;
    EditSQLCommandsMI: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    RefreshStoredSQLTreeMI: TMenuItem;
    Bevel7: TBevel;
    DBGridPopupMenu: TPopupMenu;
    ExportallrecordsMI: TMenuItem;
    Panel1: TPanel;
    HideBLOBImg: TImage;
    ShowBLOBImg: TImage;
    CopyforASPMI: TMenuItem;
    CopyforKylixDelphiMI: TMenuItem;
    CopyforPHPMI: TMenuItem;
    N5: TMenuItem;
    PasteremovingQuotesMI: TMenuItem;
    ShowSQLImg: TImage;
    HideSQLImg: TImage;
    Bevel8: TBevel;
    CopytoClipboardMI: TMenuItem;
    CopyallRecordsMI: TMenuItem;
    N6: TMenuItem;
    CopyFieldNames1: TMenuItem;
    CopyallRecordsasINSERTsMI: TMenuItem;
    N7: TMenuItem;
    LoadSQLScriptfromFileMI: TMenuItem;
    PrevCmd: TSpeedButton;
    NextCmd: TSpeedButton;
    SwitchQueryLayoutBtn: TSpeedButton;
    Bevel4: TBevel;
    TempSQLStorePnl: TPanel;
    Bevel5: TBevel;
    TempSQLStore1SBtn: TSpeedButton;
    TempSQLStore2SBtn: TSpeedButton;
    TempSQLStore3SBtn: TSpeedButton;
    TempSQLStore4SBtn: TSpeedButton;
    Bevel9: TBevel;
    TempSQLStore5SBtn: TSpeedButton;
    TempSQLStore6SBtn: TSpeedButton;
    TempSQLStore7SBtn: TSpeedButton;
    TempSQLStore8SBtn: TSpeedButton;
    PrintRecordstoPDFMI: TMenuItem;
    N8: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    function SetTable(theTable: TEERTable): Boolean;
    procedure ApplyChanges;
    procedure GetDBConnSBtnClick(Sender: TObject);
    procedure TableCBoxChange(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DBGridColEnter(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BlobClearBtnClick(Sender: TObject);
    procedure BlobOpenBtnClick(Sender: TObject);
    procedure StoreBlobBtnClick(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure SaveSQLBtnClick(Sender: TObject);
    procedure ExecSQLBtnClick(Sender: TObject);

    procedure SizeGridCols;

    procedure ProcessKey(Key: Word; col: TEERColumn);

    procedure AddTableToSQLCommand(theTable: TEERTable; SQLCmdType, SQLCmdSelectJoinType: integer);
    procedure AddColumnToSQLCommand(columnPos: integer; col: TEERColumn);

    procedure UndoMIShow(Sender: TObject);
    procedure RedoMIShow(Sender: TObject);
    procedure UndoMIClick(Sender: TObject);
    procedure RedoMIClick(Sender: TObject);
    procedure CopyMIClick(Sender: TObject);
    procedure CutMIClick(Sender: TObject);
    procedure PasteMIClick(Sender: TObject);
    procedure CopyMIShow(Sender: TObject);
    procedure PasteMIShow(Sender: TObject);
    procedure ClearMIClick(Sender: TObject);
    procedure SelectAllMIClick(Sender: TObject);
    procedure SelectAllMIShow(Sender: TObject);

    procedure GetClauses(s: string; Clauses: TStringList);
    function GetClause(s, clause: string; Clauses: TStringList): string;
    procedure DBGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure StoredSQLBtnClick(Sender: TObject);

    procedure RefreshStoredSQLTreeView(theModel: TEERModel; ExpandedNodesList: TStringList = nil);
    procedure StoredSQLTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure StoredSQLTreeViewCustomDrawItem(Sender: TCustomViewControl;
      Item: TCustomViewItem; Canvas: TCanvas; const Rect: TRect;
      State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure StoredSQLEditBtnClick(Sender: TObject);
    procedure StoredSQLExecuteBtnClick(Sender: TObject);
    procedure StoredSQLTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: WideString);
    procedure StoredSQLTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure DeleteSQLCommandMIShow(Sender: TObject);
    procedure DeleteSQLCommandMIClick(Sender: TObject);
    procedure RefreshStoredSQLTreeMIClick(Sender: TObject);
    procedure DBGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure OutputDataSetProviderUpdateError(Sender: TObject;
      DataSet: TCustomClientDataSet; E: EUpdateError;
      UpdateKind: TUpdateKind; var Response: TResolverResponse);
    procedure ExportAllRecords(fname: string);
    procedure ExportallrecordsMIClick(Sender: TObject);
    procedure ShowSQLImgClick(Sender: TObject);
    procedure HideSQLImgClick(Sender: TObject);
    procedure ShowBLOBImgClick(Sender: TObject);
    procedure HideBLOBImgClick(Sender: TObject);
    procedure SQLMemoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure StoredSQLTreeViewDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure StoredSQLTreeViewDragDrop(Sender, Source: TObject; X,
      Y: Integer);

    procedure CopySQLToClipboard(Mode: Word);
    procedure CopyforASPMIClick(Sender: TObject);
    procedure CopyallRecordsMIClick(Sender: TObject);
    procedure CopytoClipboardMIShow(Sender: TObject);
    procedure CopyFieldNames1Click(Sender: TObject);
    procedure CopyallRecordsasINSERTsMIClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure PasteremovingQuotesMIClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LoadSQLScriptfromFileMIClick(Sender: TObject);
    procedure PrevCmdClick(Sender: TObject);
    procedure SwitchQueryLayoutBtnClick(Sender: TObject);
    procedure TempSQLStore1SBtnDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TempSQLStore1SBtnDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure TempSQLStore1SBtnClick(Sender: TObject);
    procedure RefreshTempSQLStoreBtns(theModel: TEERModel);
    procedure TempSQLStore1SBtnMouseEnter(Sender: TObject);
    procedure StoredSQLTreeViewItemEnter(Sender: TObject; Node: TTreeNode);

    procedure SetLayout(Layout: integer; StoreCurrentLayoutSettings: Boolean = True);
    procedure StoreLayout(Layout: integer);
    procedure StoredSQLTreeViewMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure SQLMemoDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SQLMemoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure StoredSQLTreeViewItemExitViewportEnter(Sender: TObject);
    procedure DoHintPauseTmr(Sender: TObject);
    procedure TempSQLStore1SBtnMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure StoredSQLPopupMenuPopup(Sender: TObject);
    procedure StoredSQLSplitterMoved(Sender: TObject);
    procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure OutputClientDataSetAfterOpen(DataSet: TDataSet);
    procedure DoFieldGetText(Sender: TField; var Text: String; DisplayText: Boolean);

    procedure SetSQLMemoText(Text: string);
    function GetSQLMemoText: string;
    procedure PrintRecordstoPDFMIClick(Sender: TObject);
  private
    { Private declarations }
    theEERTable: TEERTable;
    theEERModel: TEERModel;

    //Used to track Items.Clear in onChange (might be a Delphi bug)
    ClearingStoredSQLTreeView: Boolean;

    currentSQLText: string;
    currentHistoryPos: integer;

    theHintWindow: THintWindow;
    theHintPauseTmr: TTimer;
  public
    { Public declarations }
{$IFDEF USE_SYNEDIT}
    SQLSynEdit: TSynEdit;
    SQLSynEditHighlighter: TSynSQLSyn;
{$ENDIF}
  end;

  const
    //Column Positions
    cpSelectClause=0;
    cpFromClause=1;
    cpWhereClause=2;
    cpGroupClause=3;
    cpHavingClause=4;
    cpOrderClause=5;

    cpSetClause=1;
    cpDelWhereClause=1;

    //SQL Command Types
    SQLctSELECT=0;
    SQLctUPDATE=1;
    SQLctINSERT=2;
    SQLctDELETE=3;

    //SQL Command Select Join Types
    SQLjtNONE=-1;
    SQLjtINNER=0;
    SQLjtLEFTOUTER=1;
    SQLjtRIGHTOUTER=2;

var
  EditorQueryForm: TEditorQueryForm;

implementation

uses DBDM, MainDM, GUIDM, EERDM, EER;

{$R *.xfm}

procedure TEditorQueryForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self, true);

  theEERModel:=nil;
  theEERTable:=nil;

  OutputQry.SQLConnection:=DMDB.SQLConn;
  BlobPageControl.Style:=tsNoTabs;

  BlobHexMemo.Text:='';

  SQLMemo.Lines.Clear;
  SQLMemo.PopupMenu:=SQLMemoPopupMenu;

  SQLMemo.Font.Name:=DMGUI.SQLTextFont;
  SQLMemo.Font.Size:=DMGUI.SQLTextFontSize;

  ClearingStoredSQLTreeView:=False;

  //Create the manual Hint Window
  theHintWindow:=THintWindow.Create(nil);
  theHintWindow.Color:=Application.HintColor;
  theHintPauseTmr:=TTimer.Create(nil);
  theHintPauseTmr.Interval:=300;
  theHintPauseTmr.Enabled:=False;
  theHintPauseTmr.OnTimer:=DoHintPauseTmr;

  StoredSQLTreeView.Columns[0].Caption:=
    DMMain.GetTranslatedMessage('Stored SQL Commands', 84);

{$IFDEF LINUX}
  DBGrid.Options:=DBGrid.Options + [dgAlwaysShowEditor];
{$ENDIF}

{$IFDEF USE_SYNEDIT}
  SQLSynEditHighlighter:=TSynSQLSyn.Create(self);
  SQLSynEditHighlighter.SQLDialect:=sqlMySQL;
  SQLSynEditHighlighter.KeyAttri.Foreground:=clBlue;

  SQLSynEdit:=TSynEdit.Create(self);
  SQLSynEdit.Parent:=SQLPnl;
  SQLSynEdit.Name:='SQLSynEdit';

  SQLSynEdit.Highlighter:=SQLSynEditHighlighter;
  SQLSynEdit.ScrollBars:=ssAutoBoth;
  SQLSynEdit.Gutter.Visible:=False;
{$IFDEF LINUX}
  SQLSynEdit.Font.Size:=12;
{$ELSE}
  SQLSynEdit.Font.Size:=9;
{$ENDIF}
  SQLSynEdit.Options:=[eoAutoIndent,
    eoEnhanceHomeKey, eoGroupUndo,
    eoShowScrollHint, eoScrollHintFollows,
    eoSmartTabs, eoTabsToSpaces,
    eoSmartTabDelete, eoHideShowScrollbars,
    eoTabsToSpaces{, eoHighlightCurrentLine}];

  SQLSynEdit.OnMouseMove:=SQLMemoMouseMove;
  SQLSynEdit.OnDragDrop:=SQLMemoDragDrop;
  SQLSynEdit.OnDragOver:=SQLMemoDragOver;
  SQLSynEdit.PopupMenu:=SQLMemoPopupMenu;

  SQLSynEdit.Align:=alClient;

  //Only use SynEdit when requested, otherwise set nil
  if(DMGUI.UseSQLSyntaxHighlighting)then
  begin
    SQLMemo.Hide;
    SQLSynEdit.Show;
  end
  else
  begin
    SQLMemo.Show;
    SQLSynEdit.Hide;
  end;

{$ENDIF}
end;

procedure TEditorQueryForm.FormDestroy(Sender: TObject);
begin
  if(Visible)then
    DMMain.SaveWinPos(self, True);

  //Free the Hint Window
  theHintWindow.ReleaseHandle;
end;

procedure TEditorQueryForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //Apply last changes
  if(OutputClientDataSet.Active)then
    if(OutputClientDataSet.ChangeCount>0)then
      OutputClientDataSet.ApplyUpdates(-1);

  Action:=caFree;
end;

procedure TEditorQueryForm.FormResize(Sender: TObject);
begin
  DBConnEd.Width:=TopPnl.Width-502;
  GetDBConnSBtn.Left:=DBConnEd.Left+DBConnEd.Width+5;
end;

procedure TEditorQueryForm.FormShow(Sender: TObject);
begin
  DMMain.RestoreWinPos(self, True);

  FormResize(self);
end;

function TEditorQueryForm.SetTable(theTable: TEERTable): Boolean;
var theTables: TStringList;
  i, j: integer;
  s: string;
begin
  SetTable:=False;

  DBGrid.Columns.Clear;
  DBMemo.DataField:='';
  DBImage.DataField:='';

  if(theTable=nil)then
    Exit;

  theEERTable:=theTable;
  theEERModel:=TEERModel(theEERTable.Parent);

  theTables:=TStringList.Create;
  try
    TableCBox.Items.Clear;
    theEERModel.GetEERObjectNameList([EERTable], theTables);

    TableCBox.Items.Assign(theTables);
    TableCBox.ItemIndex:=TableCBox.Items.IndexOf(theEERTable.ObjName);
  finally
    theTables.Free;
  end;


  //when not connected to DB, connect now
  if(DMDB.CurrentDBConn=nil)then
  begin
    DMDB.GetDBConnButtonClick(self, theEERModel.DefQueryDBConn);
    if(DMDB.CurrentDBConn<>nil)then
    begin
      theEERModel.DefQueryDBConn:=DMDB.CurrentDBConn.Name;
      SetTable:=True;
    end
    else
      Exit;
  end
  else
  begin
    SetTable:=True;

    //Apply changes before closing
    if(OutputClientDataSet.Active)then
      if(OutputClientDataSet.ChangeCount>0)then
        OutputClientDataSet.ApplyUpdates(-1);

    OutputQry.Close;
    OutputClientDataSet.Close;
  end;

  DBConnEd.Text:=TDBConn(DMDB.CurrentDBConn).Name;

  s:='SELECT * '+#13#10+
    'FROM '+theEERTable.ObjName;
  for i:=0 to theEERTable.Indices.Count-1 do
  begin
    if(CompareText(TEERIndex(theEERTable.Indices[i]).IndexName,
      'PRIMARY')=0)then
    begin
      if(TEERIndex(theEERTable.Indices[i]).Columns.Count>0)then
        s:=s+#13#10+
          'ORDER BY ';

      for j:=0 to TEERIndex(theEERTable.Indices[i]).Columns.Count-1 do
      begin
        s:=s+TEERColumn(theEERTable.GetColumnByID(StrToInt(TEERIndex(theEERTable.Indices[i]).Columns[j]))).ColName;

        if(j<TEERIndex(theEERTable.Indices[i]).Columns.Count-1)then
          s:=s+', ';
      end;

      break;
    end;
  end;

  SetSQLMemoText(s);

  ExecSQLBtnClick(self);
end;

procedure TEditorQueryForm.SizeGridCols;
var i: integer;
  theColumn: TColumn;
  theSize: TSize;
begin
  DBGrid.Columns.Clear;

  for i:=0 to OutputClientDataSet.Fields.Count-1 do
  begin
    theColumn:=DBGrid.Columns.Add;
    theColumn.Field:=OutputClientDataSet.Fields[i];
    theSize:=DBGrid.Canvas.TextExtent(theColumn.Field.DisplayName);

    if(theColumn.Field.DataType=ftSmallint)or
      (theColumn.Field.DataType=ftInteger)or
      (theColumn.Field.DataType=ftWord)or
      (theColumn.Field.DataType=ftLargeint)or
      (theColumn.Field.DataType=ftAutoInc)then
      theColumn.Width:=60
    else if(theColumn.Field.DataType=ftString)or
      (theColumn.Field.DataType=ftFixedChar)or
      (theColumn.Field.DataType=ftWideString)then
      theColumn.Width:=120
    else if(theColumn.Field.DataType=ftDateTime)or
      (theColumn.Field.DataType=ftTimeStamp)then
      theColumn.Width:=110
    else if(theColumn.Field.DataType=ftDate)or
      (theColumn.Field.DataType=ftTime)then
      theColumn.Width:=80
    else
      theColumn.Width:=50;

    if(theColumn.Width<theSize.Cx+5)then
      theColumn.Width:=theSize.Cx+5;
  end;

  DBGridColEnter(self);
end;

procedure TEditorQueryForm.ApplyChanges;
begin
  //
end;

procedure TEditorQueryForm.GetDBConnSBtnClick(Sender: TObject);
begin
  SetTable(theEERTable);
end;

procedure TEditorQueryForm.TableCBoxChange(Sender: TObject);
var theTable: TEERTable;
begin
  theTable:=TEERModel(theEERTable.Parent).GetEERObjectByName(EERTable, TableCBox.Items[TableCBox.ItemIndex]);

  if(theTable<>nil)then
    SetTable(theTable);
end;

procedure TEditorQueryForm.SubmitBtnClick(Sender: TObject);
begin
  if(OutputClientDataSet.Active)then
    if(OutputClientDataSet.ChangeCount>0)then
      OutputClientDataSet.ApplyUpdates(-1);
end;

procedure TEditorQueryForm.CancelBtnClick(Sender: TObject);
begin
  if(OutputClientDataSet.Active)then
  begin
    OutputClientDataSet.Close;
    OutputClientDataSet.Open;
  end;
end;

procedure TEditorQueryForm.DBGridColEnter(Sender: TObject);
var theStream: TMemoryStream;
  thePicture: TPicture;
  //i, toRead: integer;
  theBuffer: Array [0..1024] of Char;
  theText: Array [0..2048] of Char;
begin
  DBMemo.DataField:='';
  DBImage.DataField:='';

  if(DBGrid.SelectedField<>nil)then
  begin
    //String / Text
    if(DBGrid.SelectedField.ClassNameIs('TMemoField'))or
      (DBGrid.SelectedField.ClassNameIs('TStringField'))or
      (DBGrid.SelectedField.ClassNameIs('TWideStringField'))or
      (DBGrid.SelectedField.ClassNameIs('TAutoIncField'))then
    begin
      BlobPageControl.Activepage:=TextSheet;
      DBMemo.DataField:=DBGrid.Columns[DBGrid.SelectedIndex].FieldName;
    end
    //Numeric
    else if(DBGrid.SelectedField.ClassNameIs('TAutoIncField'))or
      (DBGrid.SelectedField.ClassNameIs('TBooleanField'))or
      (DBGrid.SelectedField.ClassNameIs('TFloatField'))or
      (DBGrid.SelectedField.ClassNameIs('TGuidField'))or
      (DBGrid.SelectedField.ClassNameIs('TIntegerField'))or
      (DBGrid.SelectedField.ClassNameIs('TLargeintField'))or
      (DBGrid.SelectedField.ClassNameIs('TSmallIntField'))then
    begin
      BlobPageControl.Activepage:=TextSheet;
      DBMemo.DataField:=DBGrid.Columns[DBGrid.SelectedIndex].FieldName;
    end
    else if(DBGrid.SelectedField.ClassNameIs('TGraphicField'))or
      (DBGrid.SelectedField.ClassNameIs('TBlobField'))then
    begin
      thePicture:=TPicture.Create;
      try
        theStream:=TMemoryStream.Create;
        try
          try
            DeleteFile('blob_tmp.png');
            //TBlobField(DBGrid.SelectedField).SaveToStream(theStream);
            TBlobField(DBGrid.SelectedField).SaveToFile('blob_tmp.png');

            //Try to display as Image
            thePicture.LoadFromFile('blob_tmp.png');

            DBImage.DataField:=DBGrid.Columns[DBGrid.SelectedIndex].FieldName;
            BlobPageControl.Activepage:=ImgSheet;
          except
            //if it is in another format, display as blob
            BlobMemo.Lines.LoadFromFile('blob_tmp.png');

            //Display Hex Data
            BlobHexMemo.Text:='';
            TBlobField(DBGrid.SelectedField).SaveToStream(theStream);

            {toRead:=theStream.Size;
            if(toRead>1024)then
              toRead:=1024;
            theStream.Read(theBuffer, 1024);}

            BinToHex(theBuffer, theText, 1024);
            BlobHexMemo.Text:=String(theText);

            BlobPageControl.Activepage:=BlobSheet;
          end;
        finally
          theStream.Free;
          DeleteFile('blob_tmp.png');
        end;
      finally
        thePicture.Free;
      end;
    end
    else
    begin
      BlobPageControl.Activepage:=ClearSheet;
    end;
  end;
end;

procedure TEditorQueryForm.BlobClearBtnClick(Sender: TObject);
begin
  if(DBGrid.SelectedField=nil)then
    Exit;

  if(Not(OutputClientDataSet.State=dsEdit))or
    (Not(OutputClientDataSet.State=dsInsert))then
    OutputClientDataSet.Edit;

  DBGrid.SelectedField.Clear;
end;

procedure TEditorQueryForm.BlobOpenBtnClick(Sender: TObject);
var theOpenDialog: TOpenDialog;
  RecentOpenBlobFieldDir: string;
  theIni: TMemIniFile;
  thePicture: TPicture;
begin
  if(DBGrid.SelectedField=nil)then
    Exit;

  if(Not(OutputClientDataSet.Active))then
    Exit;

  if(Not(OutputClientDataSet.State=dsEdit))or
    (Not(OutputClientDataSet.State=dsInsert))then
    OutputClientDataSet.Edit;

  //Open IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_Settings.ini');
  try
    theOpenDialog:=TOpenDialog.Create(nil);
    try
  {$IFDEF MSWINDOWS}
      //On Windows use native Win32 Open Dlg
      theOpenDialog.UseNativeDialog:=True;
      theOpenDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
  {$ENDIF}

      theOpenDialog.Title:=DMMain.GetTranslatedMessage('Open a File ...', 89);
      theOpenDialog.DefaultExt:='';
      theOpenDialog.Filter:=DMMain.GetTranslatedMessage('All files', 90)+' (*.*)|*.*';
      theOpenDialog.Width:=600;
      theOpenDialog.Height:=450;

      //Get last dir
      RecentOpenBlobFieldDir:=theIni.ReadString('RecentDirectories', 'RecentOpenBlobFieldDir', '');

      if(DirectoryExists(RecentOpenBlobFieldDir))then
        theOpenDialog.InitialDir:=RecentOpenBlobFieldDir
      else
        theOpenDialog.InitialDir:='';


      if(theOpenDialog.Execute)then
      begin
        RecentOpenBlobFieldDir:=ExtractFilePath(theOpenDialog.Filename);

        if(DBGrid.SelectedField.ClassNameIs('TBlobField'))or
          (DBGrid.SelectedField.ClassParent.ClassNameIs('TBlobField'))then
        begin
          DBImage.DataField:='';
          DBMemo.DataField:='';

          //Memo Field
          if(DBGrid.SelectedField.ClassNameIs('TMemoField'))then
          begin
            TMemoField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename);
            DBMemo.DataField:=DBGrid.Columns[DBGrid.SelectedIndex].FieldName;
          end
          else
          //Blob or Image Field
          begin
            TBlobField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename);

            thePicture:=TPicture.Create;
            try
              try
                //try to open as Image
                thePicture.LoadFromFile(theOpenDialog.Filename);

                DBImage.DataField:=DBGrid.Columns[DBGrid.SelectedIndex].FieldName;
                BlobPageControl.ActivePage:=ImgSheet;
              except
                BlobPageControl.ActivePage:=BlobSheet;
                TBlobField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename);

                DBGridColEnter(self);
              end;
            finally
              thePicture.Free;
            end;
          end;
        end;

        {if(DBGrid.SelectedField.ClassNameIs('TMemoField'))then
          TMemoField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename)
        else if(DBGrid.SelectedField.ClassNameIs('TGraphicField'))then
          TGraphicField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename)
        else if(DBGrid.SelectedField.ClassNameIs('TBlobField'))then
          TBlobField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename);}

        theIni.WriteString('RecentDirectories', 'RecentOpenBlobFieldDir', RecentOpenBlobFieldDir);
      end;

    finally
      theOpenDialog.Free;
    end;

    theIni.UpdateFile;
  finally
    theIni.Free;
  end;
end;

procedure TEditorQueryForm.StoreBlobBtnClick(Sender: TObject);
var theSaveDialog: TSaveDialog;
  RecentSaveBlobFieldDir, theFileName: string;
  theIni: TMemIniFile;
  theStringList: TStringList;
begin
  if(DBGrid.SelectedField=nil)then
    Exit;

  if(Not(OutputClientDataSet.Active))then
    Exit;

  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_Settings.ini');
  try

    theSaveDialog:=TSaveDialog.Create(nil);
    try
  {$IFDEF MSWINDOWS}
      //On Windows use native Win32 Open Dlg
      theSaveDialog.UseNativeDialog:=True;
      theSaveDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
  {$ENDIF}

      theSaveDialog.Title:=DMMain.GetTranslatedMessage('Save Field As ...', 91);
      theSaveDialog.Width:=600;
      theSaveDialog.Height:=450;
      theSaveDialog.DefaultExt:='';

      RecentSaveBlobFieldDir:=theIni.ReadString('RecentDirectories', 'RecentSaveBlobFieldDir', '');
      if(Not(DirectoryExists(RecentSaveBlobFieldDir)))then
        RecentSaveBlobFieldDir:=ExtractFilePath(Application.ExeName)+
          'Models'+PathDelim;

      theSaveDialog.InitialDir:=RecentSaveBlobFieldDir;


      {theSaveDialog.Position:=Point((Screen.Width-theSaveDialog.Width) div 2,
        (Screen.Height-theSaveDialog.Height) div 2);}

      theSaveDialog.Filter:=DMMain.GetTranslatedMessage('All Files', 90)+' (*.*)|*.*';

      if(theSaveDialog.Execute)then
      begin
        theFileName:=theSaveDialog.Filename;
        if(FileExists(theFileName))then
          if(MessageDlg(DMMain.GetTranslatedMessage('The file [%s] already exists. '#13#10+
            'Do you want to overwrite this file?', 92, ExtractFileName(theFileName)), mtInformation,
            [mbYes, mbNo], 0)=mrNo)then
            Exit;

        RecentSaveBlobFieldDir:=ExtractFilePath(theSaveDialog.FileName);

        if(DBGrid.SelectedField.ClassNameIs('TMemoField'))then
          TMemoField(DBGrid.SelectedField).SaveToFile(theFileName)
        else if(DBGrid.SelectedField.ClassNameIs('TGraphicField'))then
          TGraphicField(DBGrid.SelectedField).SaveToFile(theFileName)
        else if(DBGrid.SelectedField.ClassNameIs('TBlobField'))then
          TBlobField(DBGrid.SelectedField).SaveToFile(theFileName)
        else if(DBGrid.SelectedField.ClassNameIs('TStringField'))then
        begin
          theStringList:=TStringList.Create;
          try
            theStringList.Text:=TStringField(DBGrid.SelectedField).AsString;
            theStringList.SaveToFile(theFileName);
          finally
            theStringList.Free;
          end;
        end;


        theIni.WriteString('RecentDirectories', 'RecentSaveBlobFieldDir', RecentSaveBlobFieldDir);
      end;
    finally
      theSaveDialog.Free;
    end;

    theIni.UpdateFile;
  finally
    theIni.Free;
  end;
end;

procedure TEditorQueryForm.DBGridDblClick(Sender: TObject);
begin
  if(BlobPnl.Visible=False)then
    if(DBGrid.SelectedField<>nil)then
      if(DBGrid.SelectedField.ClassNameIs('TMemoField'))or
        (DBGrid.SelectedField.ClassNameIs('TGraphicField'))or
        (DBGrid.SelectedField.ClassNameIs('TBlobField'))then
        ShowBLOBImgClick(Self);
end;



procedure TEditorQueryForm.SaveSQLBtnClick(Sender: TObject);
var name, initalFolderName: string;
  theSQLCmd: TStoredSQLCmd;
  i: integer;
begin
  if(TForm(Application.MainForm).ActiveMDIChild=nil)then
    Exit;

  {initalFolderName:='';
  if(StoredSQLTreeView.Items.Count>0)then
    if(StoredSQLTreeView.Items[0].Data=nil)then
      initalFolderName:=StoredSQLTreeView.Items[0].Text+'/';

  name:=initalFolderName+name;}
  initalFolderName:='';
  name:='';

  if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Store SQL Command ... (use slashes / to create directories)', 93),
    DMMain.GetTranslatedMessage('Name:', 94),
    name, Length(initalFolderName)))then
  begin
    theSQLCmd:=nil;
    for i:=0 to theEERModel.StoredSQLCmds.Count-1 do
      if(CompareText(TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).StoredPosition,
        name)=0)then
      begin
        theSQLCmd:=TStoredSQLCmd(theEERModel.StoredSQLCmds[i]);
        theSQLCmd.SQLText:=GetSQLMemoText;

        theEERModel.ModelHasChanged;
      end;

    if(theSQLCmd=nil)then
    begin
      //new(theSQLCmd);
      theSQLCmd:=TStoredSQLCmd.Create;
      theSQLCmd.SQLCmdType:=ct_SQLCmd;
      theSQLCmd.StoredPosition:=name;
      theSQLCmd.SQLText:=GetSQLMemoText;
      theEERModel.StoredSQLCmds.Add(theSQLCmd);

      theEERModel.ModelHasChanged;
    end;

    if(Not(StoredSQLBtn.Down))then
      StoredSQLBtn.Click;

    RefreshStoredSQLTreeView(theEERModel);
  end;
end;

procedure TEditorQueryForm.ExecSQLBtnClick(Sender: TObject);
var i, Output, HistoryCount, totalRowsAffected: integer;
  theSQLCmd: TStoredSQLCmd;
  cmds, s: string;
  StartExecTime, EndExecTime: TDateTime;
begin
  //when not connected to DB, connect now
  if(DMDB.CurrentDBConn=nil)then
  begin
    if(TForm(Application.MainForm).ActiveMDIChild<>nil)then
    begin
      if(TForm(Application.MainForm).ActiveMDIChild.Classname='TEERForm')then
      begin
        DMDB.GetDBConnButtonClick(Sender, TEERForm(TForm(Application.MainForm).ActiveMDIChild).EERModel.DefQueryDBConn);
        if(DMDB.CurrentDBConn<>nil)then
          TEERForm(TForm(Application.MainForm).ActiveMDIChild).EERModel.DefQueryDBConn:=DMDB.CurrentDBConn.Name;
      end;
    end
    else
      DMDB.GetDBConnButtonClick(Sender);

    if(DMDB.CurrentDBConn=nil)then
      Exit;
  end
  else
  begin
    DBGrid.Columns.Clear;
    DBMemo.DataField:='';
    DBImage.DataField:='';


    //Apply changes before closing
    if(OutputClientDataSet.Active)then
      if(OutputClientDataSet.ChangeCount>0)then
        OutputClientDataSet.ApplyUpdates(-1);

    OutputQry.Close;
    OutputClientDataSet.Close;

    DBGrid.DataSource:=OutputDataSrc;
  end;

  //if there is no sql statement, exit
	if(GetSQLMemoText='')then
    Exit;

  //For undo/Redo-btns
  currentSQLText:=GetSQLMemoText;
  currentHistoryPos:=0;

  //Keep only 20 SQL Cmds in History
  if(TForm(Application.MainForm).ActiveMDIChild<>nil)then
  begin
    HistoryCount:=0;
    i:=0;
    while(i<theEERModel.StoredSQLCmds.Count)do
    begin
      if(TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).SQLCmdType=ct_SQLHistory)then
      begin
        inc(HistoryCount);
        if(HistoryCount>=20)or
          (TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).SQLText=GetSQLMemoText)then
        begin
          theEERModel.StoredSQLCmds.Delete(i);

          dec(HistoryCount);
        end
        else
          inc(i);
      end
      else
        inc(i);
    end;

    //Make Entry in History
    theSQLCmd:=TStoredSQLCmd.Create;
    theSQLCmd.SQLCmdType:=ct_SQLHistory;
    theSQLCmd.StoredPosition:=FormatDateTime('DD.MM HH:NN:SS', Now);
    theSQLCmd.SQLText:=GetSQLMemoText;
    theEERModel.StoredSQLCmds.Insert(0, theSQLCmd);

    if(Not(DMGUI.IgnoreSQLHistoryChange))then
      theEERModel.ModelHasChanged;

    RefreshStoredSQLTreeView(theEERModel);
  end;

  Output:=1;
  //No command is longer than 8 chars
  s:=Copy(GetSQLMemoText, 1, 8);

  if((CompareText(Copy(s, 1, 6), 'update')=0)
  	or(CompareText(Copy(s, 1, 6), 'insert')=0)
    or(CompareText(Copy(s, 1, 6), 'delete')=0)
    or(CompareText(Copy(s, 1, 4), 'drop')=0)
    or(CompareText(Copy(s, 1, 5), 'alter')=0)
    or(CompareText(Copy(s, 1, 6), 'create')=0)
    or(CompareText(Copy(s, 1, 3), 'set')=0)
    or(CompareText(Copy(s, 1, 4), 'save')=0)
    or(CompareText(Copy(s, 1, 4), 'load')=0)
    or(CompareText(Copy(s, 1, 4), 'move')=0)
    or(CompareText(Copy(s, 1, 5), 'flush')=0)
    or(CompareText(Copy(s, 1, 6), 'commit')=0)
    or(CompareText(Copy(s, 1, 8), 'rollback')=0)
    or(CompareText(Copy(s, 1, 1), '<')=0)
    or(CompareText(Copy(s, 1, 1), '/')=0))then
  	Output:=0;

  if(Output=0)then
  begin
    StartExecTime:=Now;
    totalRowsAffected:=DMDB.ExecuteSQLCmdScript(GetSQLMemoText);
    EndExecTime:=Now;

    DMGUI.SetStatusCaption('Query(s) executed. '+FormatFloat('##,###,##0', totalRowsAffected)+' Rows affected. Time: '+
      FormatDateTime('nn:ss:zzz', StartExecTime-EndExecTime));
  end
  else if(Output=1)then
  begin
    //Only take first select
    cmds:=GetSQLMemoText;
    cmds:=DMDB.GetFirstSQLCmdFromScript(cmds);

    OutputQry.SQL.Text:=cmds;

    StartExecTime:=Now;
    try

      //OutputQry.Open;
      OutputClientDataSet.Open;

      EndExecTime:=Now;
    except
      on x: Exception do
      begin
        //After timeout, try again
        if(Pos('Lost connection to MySQL', x.Message)>0)or
          (Pos('Cannot perform this operation on a closed dataset', x.Message)>0)then
        begin
          OutputClientDataSet.Open;
          
          EndExecTime:=now;
        end
        else
        begin
          MessageDlg('ERROR while executing Query: '+#13#10#13#10+
            Trim(OutputQry.SQL.Text)+#13#10#13#10+
            'ERROR Message: '+#13#10+
            x.Message, mtError, [mbOk], 0);

          Exit;
        end;
      end;
    end;

    //OutputClientDataSet.Open;

    SizeGridCols;

    DMGUI.SetStatusCaption('Query opened. '+FormatFloat('##,###,##0', OutputClientDataSet.RecordCount)+' Record(s) fetched. Time: '+
      FormatDateTime('nn:ss:zzz', StartExecTime-EndExecTime));
  end;
end;



procedure TEditorQueryForm.GetClauses(s: string; Clauses: TStringList);
var i: integer;
  clause: string;
begin
  s:=DMMain.ReplaceText(s, #13#10, ' ');

  for i:=0 to Clauses.Count-1 do
  begin
    clause:=GetClause(s, Clauses.Names[i], Clauses);

    if(clause<>'')then
      Clauses.ValueFromIndex[i]:=clause;
  end;
end;

function TEditorQueryForm.GetClause(s, clause: string; Clauses: TStringList): string;
var i, p1, p2: integer;
  theClause: string;
begin
  theClause:='';

  //Get Start Pos of the clause
  p1:=Pos(clause, UpperCase(s));
  if(p1>0)then
  begin
    p1:=p1+Length(clause)+1;

    //check all possible following clauses for the end if the clause
    for i:=Clauses.IndexOfName(clause)+1 to Clauses.Count-1 do
    begin
      p2:=Pos(Clauses.Names[i], UpperCase(Copy(s, p1, Length(s))));
      if(p2>0)then
      begin
        theClause:=Trim(Copy(s, p1, p2-1));
        break;
      end;
    end;

    //if at end of line
    if(theClause='')then
      theClause:=Copy(s, p1, Length(s));
  end;

  GetClause:=theClause;
end;

procedure TEditorQueryForm.ProcessKey(Key: Word; col: TEERColumn);
var columnPos: integer;
  s: string;
begin
  columnPos:=-1;

  s:=Copy(GetSQLMemoText, 1, 8);

  //Select Command
  if(Copy(s, 1, 6)='SELECT')then
  begin
    if(Key=wtSQLSelect)then
      columnPos:=cpSelectClause
    else if(Key=wtSQLWhere)then
      columnPos:=cpWhereClause
    else if(Key=wtSQLGroup)then
      columnPos:=cpGroupClause
    else if(Key=wtSQLHaving)then
      columnPos:=cpHavingClause
    else if(Key=wtSQLOrder)then
      columnPos:=cpOrderClause;
  end
  //UPDATE Command
  else if(Copy(s, 1, 6)='UPDATE')then
  begin
    if(Key=wtSQLSet)then
      columnPos:=cpSetClause
    else if(Key=wtSQLWhere)then
      columnPos:=cpWhereClause;
  end
  //DELETE Command
  else if(Copy(s, 1, 6)='DELETE')then
  begin
    if(Key=wtSQLWhere)then
      columnPos:=cpDelWhereClause;
  end;

  if(columnPos>=0)then
    AddColumnToSQLCommand(columnPos, col);
end;


procedure TEditorQueryForm.UndoMIShow(Sender: TObject);
begin
{$IFDEF USE_SYNEDIT}
  if(DMGUI.UseSQLSyntaxHighlighting)then
    TMenuItem(Sender).Enabled:=SQLSynEdit.CanUndo
  else
{$ENDIF}
    TMenuItem(Sender).Enabled:=SQLMemo.CanUndo;
end;

procedure TEditorQueryForm.RedoMIShow(Sender: TObject);
begin
{$IFDEF USE_SYNEDIT}
  if(DMGUI.UseSQLSyntaxHighlighting)then
    TMenuItem(Sender).Enabled:=SQLSynEdit.CanRedo
  else
{$ENDIF}
    TMenuItem(Sender).Enabled:=SQLMemo.CanRedo;
end;

procedure TEditorQueryForm.UndoMIClick(Sender: TObject);
begin
{$IFDEF USE_SYNEDIT}
  if(DMGUI.UseSQLSyntaxHighlighting)then
    SQLSynEdit.Undo
  else
{$ENDIF}
    SQLMemo.Undo;
end;

procedure TEditorQueryForm.RedoMIClick(Sender: TObject);
begin
{$IFDEF USE_SYNEDIT}
  if(DMGUI.UseSQLSyntaxHighlighting)then
    SQLSynEdit.Redo
  else
{$ENDIF}
    SQLMemo.Redo;
end;

procedure TEditorQueryForm.CopyMIClick(Sender: TObject);
begin
  if(GetSQLMemoText<>'')then
    Clipboard.AsText:=GetSQLMemoText;
end;

procedure TEditorQueryForm.CutMIClick(Sender: TObject);
begin
  if(GetSQLMemoText<>'')then
  begin
    Clipboard.AsText:=GetSQLMemoText;
    SetSQLMemoText('');
  end;
end;

procedure TEditorQueryForm.PasteMIClick(Sender: TObject);
begin
  if(Clipboard.AsText<>'')then
{$IFDEF USE_SYNEDIT}
    if(DMGUI.UseSQLSyntaxHighlighting)then
      SQLSynEdit.SelText:=Clipboard.AsText
    else
{$ENDIF}
      SQLMemo.SelText:=Clipboard.AsText;
end;

procedure TEditorQueryForm.CopyMIShow(Sender: TObject);
begin
{$IFDEF USE_SYNEDIT}
  if(DMGUI.UseSQLSyntaxHighlighting)then
    TMenuItem(Sender).Enabled:=(SQLSynEdit.SelText<>'')
  else
{$ENDIF}
    TMenuItem(Sender).Enabled:=(SQLMemo.SelText<>'');
end;

procedure TEditorQueryForm.PasteMIShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=(Clipboard.AsText<>'');
end;

procedure TEditorQueryForm.ClearMIClick(Sender: TObject);
begin
  SetSQLMemoText('');
end;

procedure TEditorQueryForm.SelectAllMIClick(Sender: TObject);
begin
{$IFDEF USE_SYNEDIT}
  if(DMGUI.UseSQLSyntaxHighlighting)then
    SQLSynEdit.SelectAll
  else
{$ENDIF}
    SQLMemo.SelectAll;
end;

procedure TEditorQueryForm.SelectAllMIShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=(GetSQLMemoText<>'');
end;

procedure TEditorQueryForm.DBGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=False;
  if(Source<>nil)then
    if(Source.ClassnameIs('TEERTable'))then
      Accept:=True;
end;

procedure TEditorQueryForm.StoredSQLBtnClick(Sender: TObject);
begin
  if(Not(StoredSQLBtn.Down))then
  begin
    StoredSQLPnl.Visible:=True;
    StoredSQLSplitter.Visible:=True;
  end
  else
  begin
    StoredSQLSplitter.Visible:=False;
    StoredSQLPnl.Visible:=False;
  end;
end;

procedure TEditorQueryForm.RefreshStoredSQLTreeView(theModel: TEERModel; ExpandedNodesList: TStringList = nil);
var //theSQLCmd: PStoredSQLCmd;
  parentFolderNode, theTreeNode: TTreeNode;
  i, j, k, folderanz: integer;
  foldername, itemname: string;
  ExpandedNodes: TStringList;
  StartFolder: integer;
  SQLCommandsNode,
  ScriptsNode,
  TableSelectsNode,
  HistroyNode: TTreeNode;
begin
  ExpandedNodes:=TStringList.Create;
  try
    if(theEERModel=theModel)then
    begin
      if(ExpandedNodesList<>nil)then
        ExpandedNodes.Text:=ExpandedNodesList.Text
      else
      begin
        //Store expanded nodes
        for i:=0 to StoredSQLTreeView.Items.Count-1 do
        begin
          if(StoredSQLTreeView.Items[i].Expanded)then
            ExpandedNodes.Add(StoredSQLTreeView.Items[i].Text);
        end;
      end;
    end
    else
      theEERModel:=theModel;

    ClearingStoredSQLTreeView:=True;
    StoredSQLTreeView.Items.Clear;
    ClearingStoredSQLTreeView:=False;

    if(theEERModel<>nil)then
    begin
      //Add Initial Nodes
      SQLCommandsNode:=StoredSQLTreeView.Items.Add(nil, DMMain.GetTranslatedMessage('SQL Commands', 85));
      SQLCommandsNode.ImageIndex:=1;
      SQLCommandsNode.Data:=nil;
      ScriptsNode:=StoredSQLTreeView.Items.Add(nil, DMMain.GetTranslatedMessage('Scripts', 86));
      ScriptsNode.ImageIndex:=2;
      ScriptsNode.Data:=nil;
      TableSelectsNode:=StoredSQLTreeView.Items.Add(nil, DMMain.GetTranslatedMessage('Table Selects', 87));
      TableSelectsNode.ImageIndex:=3;
      TableSelectsNode.Data:=nil;
      HistroyNode:=StoredSQLTreeView.Items.Add(nil, DMMain.GetTranslatedMessage('History', 88));
      HistroyNode.ImageIndex:=4;
      HistroyNode.Data:=nil;

      for i:=0 to theEERModel.StoredSQLCmds.Count-1 do
      begin
        if(TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).SQLCmdType=ct_SQLDragDropStores)then
          continue;

        folderanz:=DMMain.GetColumnCountFromSepString(TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).StoredPosition, '/', '');
        case TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).SQLCmdType of
          ct_SQLCmd:
            parentFolderNode:=SQLCommandsNode;
          ct_SQLScript:
            parentFolderNode:=ScriptsNode;
          ct_SQLTableSelect:
            parentFolderNode:=TableSelectsNode;
          ct_SQLHistory:
            parentFolderNode:=HistroyNode;
        else
          parentFolderNode:=nil;
        end;

        //for compatibility with version before 4.0.3.16
        foldername:=DMMain.GetColumnFromSepString(TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).StoredPosition, 0, '/', '');
        if(foldername='SQL Commands')or
          (foldername='Scripts')or
          (foldername='Table Selects')or
          (foldername='History')then
          StartFolder:=1
        else
          StartFolder:=0;


        for j:=StartFolder to folderanz-2 do
        begin
          foldername:=DMMain.GetColumnFromSepString(TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).StoredPosition, j, '/', '');
          theTreeNode:=nil;

          for k:=0 to StoredSQLTreeView.Items.Count-1 do
            if(StoredSQLTreeView.Items[k].Text=foldername)then
              theTreeNode:=StoredSQLTreeView.Items[k];

          if(theTreeNode=nil)then
          begin
            theTreeNode:=StoredSQLTreeView.Items.AddChild(parentFolderNode, foldername);
            theTreeNode.ImageIndex:=0;
            theTreeNode.Data:=nil;
          end;

          parentFolderNode:=theTreeNode;
        end;

        if(parentFolderNode<>nil)then
        begin
          itemname:=DMMain.GetColumnFromSepString(TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).StoredPosition, folderanz-1, '/', '');
          theTreeNode:=StoredSQLTreeView.Items.AddChild(parentFolderNode, itemname);
          theTreeNode.ImageIndex:=TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).SQLCmdType;
          theTreeNode.Data:=theEERModel.StoredSQLCmds[i];
        end;
      end;
    end;

    //Re-expand tree
    if(ExpandedNodes.Count>0)then
      for i:=0 to ExpandedNodes.Count-1 do
      begin
        for j:=0 to StoredSQLTreeView.Items.Count-1 do
          if(ExpandedNodes[i]=StoredSQLTreeView.Items[j].Text)and
            (Not(StoredSQLTreeView.Items[j].Expanded))then
            begin
              StoredSQLTreeView.Items[j].Expanded:=True;
              break;
            end;
      end;
  finally
    ExpandedNodes.Free;
  end;
end;

procedure TEditorQueryForm.StoredSQLTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  {if(ClearingStoredSQLTreeView)then
    Exit;

  if(Node<>nil)then
  begin
    if(Node.Data<>nil)then
      StoredSQLMemo.Text:=TStoredSQLCmd(Node.Data).SQLText
    else
      StoredSQLMemo.Text:='';
  end
  else
    StoredSQLMemo.Text:='';}
end;

procedure TEditorQueryForm.StoredSQLTreeViewCustomDrawItem(
  Sender: TCustomViewControl; Item: TCustomViewItem; Canvas: TCanvas;
  const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  {with Canvas do
  begin
    Pen.Color:=clDark;
    MoveTo(0, Rect.Bottom-1);
    LineTo(StoredSQLTreeView.Width+30, Rect.Bottom-1);

    if(TTreeNode(Item).ImageIndex>-1)then
      StoredSQLImageList.Draw(Canvas, TTreeNode(Item).ImageIndex, Rect.Left, Rect.Top);
    TextOut(Rect.Left+18, Rect.Top+3, TTreeNode(Item).Text);
  end;

  DefaultDraw := False; //item already complete}
end;

procedure TEditorQueryForm.StoredSQLEditBtnClick(Sender: TObject);
begin
  if(StoredSQLTreeView.Selected<>nil)then
    if(StoredSQLTreeView.Selected.Data<>nil)then
      if(TStoredSQLCmd(StoredSQLTreeView.Selected.Data).SQLText<>'')then
        SetSQLMemoText(TStoredSQLCmd(StoredSQLTreeView.Selected.Data).SQLText);
end;

procedure TEditorQueryForm.StoredSQLExecuteBtnClick(Sender: TObject);
begin
  StoredSQLEditBtnClick(Sender);

  if(GetSQLMemoText<>'')then
    ExecSQLBtnClick(Sender);
end;

procedure TEditorQueryForm.StoredSQLTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: WideString);
var itemname: string;
  folderanz: integer;
  i: integer;
begin
  try
    try
      if(s<>'')then
        if(Node<>nil)then
        begin
          if(Node.Data<>nil)then
          begin
            s:=DMMain.ReplaceText(S, '/', ' ');
            folderanz:=DMMain.GetColumnCountFromSepString(TStoredSQLCmd(Node.Data).StoredPosition, '/', '');
            itemname:=DMMain.GetColumnFromSepString(TStoredSQLCmd(Node.Data).StoredPosition, folderanz-1, '/', '');
            TStoredSQLCmd(Node.Data).StoredPosition:=Copy(
              TStoredSQLCmd(Node.Data).StoredPosition, 1,
                Length(TStoredSQLCmd(Node.Data).StoredPosition)-Length(itemname))+
                  S;
          end
          else
          begin
            //A Folder is renamed
            s:=DMMain.ReplaceText(S, '/', ' ');

            //TODO: Replace only the selected folder, not all folders
            //with the same name

            //Replace the Folder's name in all StoredSQLCmds
            for i:=0 to theEERModel.StoredSQLCmds.Count-1 do
              TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).StoredPosition:=DMMain.ReplaceText(TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).StoredPosition, '/'+Node.Text+'/', '/'+s+'/');
          end;
        end;
    except
    end;
  finally
    RefreshStoredSQLTreeView(theEERModel);
  end;
end;

procedure TEditorQueryForm.StoredSQLTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
var s: string;
  s1: Widestring;
begin
  AllowEdit:=False;

  if(Node<>nil)then
    if(Node.Level>0)then
    begin
      s:=Node.Text;
      if(DMMain.ShowStringEditor('Connecion Name', 'Name:', s))then
        if(s<>'')then
        begin
          s1:=s;
          //Node.Text:=s;
          StoredSQLTreeViewEdited(Sender, Node, s1);
        end;

      //AllowEdit:=True;
    end;
end;

procedure TEditorQueryForm.DeleteSQLCommandMIShow(Sender: TObject);
begin
  DeleteSQLCommandMI.Enabled:=False;

  if(StoredSQLTreeView.Selected<>nil)then
    if(StoredSQLTreeView.Selected.Data<>nil)then
      DeleteSQLCommandMI.Enabled:=True;
end;

procedure TEditorQueryForm.DeleteSQLCommandMIClick(Sender: TObject);
var theSQLCmd: TStoredSQLCmd;
  //ExpandedNodes: TStringList;
  i: integer;
begin
  if(TForm(Application.MainForm).ActiveMDIChild=nil)then
    Exit;

  if(StoredSQLTreeView.EditingItem=nil)then
  begin
    i:=0;
    while(i<StoredSQLTreeView.Items.Count)do
    begin
      if(StoredSQLTreeView.Items[i].Selected)and
        (StoredSQLTreeView.Items[i].Data<>nil)then
      begin
        theSQLCmd:=StoredSQLTreeView.Items[i].Data;

        theEERModel.StoredSQLCmds.Delete(theEERModel.StoredSQLCmds.IndexOf(theSQLCmd));
        theEERModel.ModelHasChanged;
      end;

      inc(i);
    end;

    RefreshStoredSQLTreeView(theEERModel);
  end;
end;

procedure TEditorQueryForm.RefreshStoredSQLTreeMIClick(Sender: TObject);
begin
  RefreshStoredSQLTreeView(theEERModel);
end;

procedure TEditorQueryForm.DBGridDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if(Source<>nil)then
    if(Source.ClassnameIs('TEERTable'))then
    begin
      //When the table is dragged onto the Gird directly
      if(Sender.ClassNameIs('TDBGrid'))then
        SetSQLMemoText('');

      AddTableToSQLCommand(TEERTable(Source), SQLctSELECT, SQLjtINNER);

      if(Sender.ClassNameIs('TDBGrid'))then
      begin
        ExecSQLBtnClick(self);
        if(DBGrid.Visible)then
          DBGrid.SetFocus;
      end
    end;
end;

procedure TEditorQueryForm.AddTableToSQLCommand(theTable: TEERTable; SQLCmdType, SQLCmdSelectJoinType: integer);
var i, j, k, destTblNr: integer;
  theTableList: TList;
  theInnerJoinTables,
  theTableAliases: TStringList;
  TablesInFromClauseCount: integer;
  s, SQLStr: string;
  theClauses: TStringList;
  FromClause,
  SetStart: integer;
  AliasNotAssigned: Boolean;
begin
  theEERModel:=TEERModel(theTable.Parent);

  FromClause:=0;

  theClauses:=TStringList.Create;
  theTableAliases:=TStringList.Create;
  theInnerJoinTables:=TStringList.Create;
  theTableList:=TList.Create;
  try
    //Select Command
    if(SQLCmdType=SQLctSELECT)then
    begin
      //Build the Clauses
      theClauses.Add('SELECT=');
      theClauses.Add('FROM=');
      theClauses.Add('WHERE=');
      theClauses.Add('GROUP BY=');
      theClauses.Add('HAVING=');
      theClauses.Add('ORDER BY=');
      theClauses.Add('LIMIT=');

      GetClauses(GetSQLMemoText, theClauses);

      FromClause:=1;

      //Analyze and build SQL command

      //Split into lines
      s:=theClauses.ValueFromIndex[FromClause];
      s:=DMMain.ReplaceText(s, ',', #13#10);
      s:=DMMain.ReplaceText(s, 'CROSS JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'INNER JOIN', #13#10);
      theInnerJoinTables.Text:=s;
      for i:=0 to theInnerJoinTables.Count-1 do
        theInnerJoinTables[i]:=Trim(theInnerJoinTables[i]);

      s:=DMMain.ReplaceText(s, 'STRAIGHT_JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'LEFT JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'LEFT OUTER JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'NATURAL JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'NATURAL LEFT JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'NATURAL LEFT OUTER JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'RIGHT JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'RIGHT OUTER JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'NATURAL JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'NATURAL RIGHT JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'NATURAL RIGHT OUTER JOIN', #13#10);
      s:=DMMain.ReplaceText(s, 'JOIN', #13#10);
      theTableAliases.Text:=s;
      //Store Count of tables existing in FROM Clause
      TablesInFromClauseCount:=theTableAliases.Count;


      for i:=0 to theTableAliases.Count-1 do
      begin
        //Set Tablename=Alias
        theTableAliases[i]:=DMMain.ReplaceText(Trim(theTableAliases[i]), ' AS ', '=');
        theTableAliases[i]:=DMMain.ReplaceText(theTableAliases[i], ' ', '=');

        //Add Table to TableList
        theTableList.Add(theEERModel.GetEERObjectByName(EERTable, theTableAliases.Names[i]));
      end;

      //---------------------------------------------------
      //Get all selected tables
      if(theTable.Selected)then
        for i:=0 to theEERModel.ComponentCount-1 do
          if(theEERModel.Components[i].ClassName='TEERTable')then
            if(TEERTable(theEERModel.Components[i]).Selected)then
            begin
              theTableAliases.Add(TEERTable(theEERModel.Components[i]).ObjName+'=');

              theTableList.Add(theEERModel.Components[i]);
            end;

      //If there is no selected Table, add dragged table
      if(theTableList.Count=TablesInFromClauseCount)then
      begin
        theTableAliases.Add(theTable.ObjName+'=');

        theTableList.Add(theTable);
      end;

      //Get Table Aliases for new tables
      for i:=TablesInFromClauseCount to theTableAliases.Count-1 do
      begin
        //if the table is already in FROM clause, take same Alias+Number
        if(theTableAliases.IndexOfName(TEERTable(theTableList[i]).ObjName)>-1)and
          (theTableAliases.IndexOfName(TEERTable(theTableList[i]).ObjName)<TablesInFromClauseCount)then
        begin
          j:=2;
          while(theTableAliases.IndexOf(TEERTable(theTableList[i]).ObjName+'='+
            theTableAliases.Values[TEERTable(theTableList[i]).ObjName]+IntToStr(j))>-1)do
            inc(j);

          theTableAliases[i]:=TEERTable(theTableList[i]).ObjName+'='+theTableAliases.Values[TEERTable(theTableList[i]).ObjName]+IntToStr(j);
        end
        else
        begin
          j:=0;
          AliasNotAssigned:=False;
          while(Not(AliasNotAssigned))do
          begin
            inc(j);

            AliasNotAssigned:=True;
            for k:=0 to i-1 do
              if(Copy(theTableAliases[k], Pos('=', theTableAliases[k])+1, Length(theTableAliases[k]))=
                Copy(TEERTable(theTableList[i]).ObjName, 1, j))then
              begin
                AliasNotAssigned:=False;
                break;
              end;

            if(AliasNotAssigned)then
//              theTableAliases[i]:=TEERTable(theTableList[i]).ObjName+'='+Copy(TEERTable(theTableList[i]).ObjName, 1, j);
              theTableAliases[i]:=TEERTable(theTableList[i]).ObjName+'='+TEERTable(theTableList[i]).ObjName;
          end;
        end;
      end;

      //---------------------------------------------------
      //Modify Clauses

      //Add * to SELECT clause if it's empty
      if(theClauses.ValueFromIndex[0]='')then
        theClauses.ValueFromIndex[0]:='*';

      //Add new tables to FROM Clause
      for i:=TablesInFromClauseCount to theTableAliases.Count-1 do
      begin
        if(theClauses.ValueFromIndex[FromClause]='')then
          theClauses.ValueFromIndex[FromClause]:=DMMain.ReplaceText(theTableAliases[i], '=', ' ')
        else
          theClauses.ValueFromIndex[FromClause]:=Trim(theClauses.ValueFromIndex[FromClause])+', '+
            DMMain.ReplaceText(theTableAliases[i], '=', ' ');
      end;

      //make joins
      if(SQLCmdSelectJoinType<>SQLjtNONE)then
      begin
        for i:=TablesInFromClauseCount to theTableList.Count-1 do
        begin
          //Check RelStart
          for j:=0 to TEERTable(theTableList[i]).RelStart.Count-1 do
          begin
            destTblNr:=theTableList.IndexOf(TEERRel(TEERTable(theTableList[i]).RelStart[j]).DestTbl);
            if(destTblNr>-1)and(destTblNr<TablesInFromClauseCount)then
            begin
              for k:=0 to TEERRel(TEERTable(theTableList[i]).RelStart[j]).FKFields.Count-1 do
              begin
                s:='';
                if(theClauses.ValueFromIndex[2]<>'')then
                  s:=s+' AND ';
                s:=s+theTableAliases.ValueFromIndex[destTblNr]+'.'+
                  TEERRel(TEERTable(theTableList[i]).RelStart[j]).FKFields.ValueFromIndex[k]+'='+
                  theTableAliases.ValueFromIndex[i]+'.'+
                  TEERRel(TEERTable(theTableList[i]).RelStart[j]).FKFields.Names[k];

                theClauses.ValueFromIndex[2]:=theClauses.ValueFromIndex[2]+s;
              end;
            end;
          end;

          //Check RelEnd
          for j:=0 to TEERTable(theTableList[i]).RelEnd.Count-1 do
          begin
            destTblNr:=theTableList.IndexOf(TEERRel(TEERTable(theTableList[i]).RelEnd[j]).SrcTbl);
            if(destTblNr>-1)and(destTblNr<TablesInFromClauseCount)then
            begin
              for k:=0 to TEERRel(TEERTable(theTableList[i]).RelEnd[j]).FKFields.Count-1 do
              begin
                s:='';
                if(theClauses.ValueFromIndex[2]<>'')then
                  s:=s+' AND ';
                s:=s+theTableAliases.ValueFromIndex[destTblNr]+'.'+
                  TEERRel(TEERTable(theTableList[i]).RelEnd[j]).FKFields.ValueFromIndex[k]+'='+
                  theTableAliases.ValueFromIndex[i]+'.'+
                  TEERRel(TEERTable(theTableList[i]).RelEnd[j]).FKFields.Names[k];

                theClauses.ValueFromIndex[2]:=theClauses.ValueFromIndex[2]+s;
              end;
            end;
          end;
        end;
      end;
    end
    //UPDATE Command
    else if(SQLCmdType=SQLctUPDATE)then
    begin
      //Build the Clauses
      theClauses.Add('UPDATE=');
      theClauses.Add('SET=');
      theClauses.Add('WHERE=');
      theClauses.Add('LIMIT=');

      GetClauses(GetSQLMemoText, theClauses);

      FromClause:=0;

      //Analyze and build SQL command
      theClauses.ValueFromIndex[0]:=theTable.ObjName;
    end
    //UPDATE Command
    else if(SQLCmdType=SQLctDELETE)then
    begin
      //Build the Clauses
      theClauses.Add('DELETE FROM=');
      theClauses.Add('WHERE=');
      theClauses.Add('LIMIT=');

      GetClauses(GetSQLMemoText, theClauses);

      FromClause:=0;

      //Analyze and build SQL command
      theClauses.ValueFromIndex[0]:=theTable.ObjName;
    end
    //INSERT Command
    else if(SQLCmdType=SQLctINSERT)then
    begin
      //Build the Clauses
      theClauses.Add('INSERT INTO=');
      theClauses.Add('VALUES=');
      theClauses.Add('SELECT=');

      GetClauses(GetSQLMemoText, theClauses);

      //Set Cursor to Values...
      FromClause:=-1;

      //Analyze and build SQL command
      theClauses.ValueFromIndex[0]:=theTable.ObjName+'(';

      for i:=0 to theTable.Columns.Count-1 do
      begin
        theClauses.ValueFromIndex[0]:=
          theClauses.ValueFromIndex[0]+TEERColumn(theTable.Columns[i]).ColName;

        if(i<theTable.Columns.Count-1)then
          theClauses.ValueFromIndex[0]:=
            theClauses.ValueFromIndex[0]+', '
        else
          theClauses.ValueFromIndex[0]:=
            theClauses.ValueFromIndex[0]+')';
      end;

      theClauses.ValueFromIndex[1]:='(';

    end;

    //Build SQLStr from Clauses
    SQLStr:='';
    SetStart:=0;
    for i:=0 to theClauses.Count-1 do
      if(theClauses.ValueFromIndex[i]<>'')then
      begin
        SQLStr:=SQLStr+theClauses.Names[i]+' '+
          Trim(theClauses.ValueFromIndex[i])+#13#10;
        if(FromClause=i)then
          SetStart:=Length(SQLStr)-2;
      end;

    SetSQLMemoText(SQLStr);
  finally
    theClauses.Free;
    theTableAliases.Free;
    theInnerJoinTables.Free;
    theTableList.Free;
  end;

{$IFDEF USE_SYNEDIT}
  if(DMGUI.UseSQLSyntaxHighlighting)then
  begin
    if(SQLSynEdit.Visible)then
      SQLSynEdit.SetFocus;
    SQLSynEdit.SelLength:=0;
    if(SetStart>0)then
      SQLSynEdit.SelStart:=SetStart
    else
      SQLSynEdit.SelStart:=Length(SQLSynEdit.Text);
  end
  else
  begin
{$ENDIF}
    if(SQLPnl.Visible)then
      SQLMemo.SetFocus;
    SQLMemo.SelLength:=0;
    if(SetStart>0)then
      SQLMemo.SelStart:=SetStart
    else
      SQLMemo.SelStart:=Length(SQLMemo.Text);
{$IFDEF USE_SYNEDIT}
  end;
{$ENDIF}
end;

procedure TEditorQueryForm.AddColumnToSQLCommand(columnPos: integer; col: TEERColumn);
var i, SetStart: integer;
  SQLStr, s, TableAlias: string;
  theClauses: TStringList;
begin
  if(Not(Assigned(theEERModel)))then
    Exit;

  theClauses:=TStringList.Create;
  try
    //Select Command
    if(Copy(GetSQLMemoText, 1, 6)='SELECT')then
    begin
      //Build the Clauses
      theClauses.Add('SELECT=');
      theClauses.Add('FROM=');
      theClauses.Add('WHERE=');
      theClauses.Add('GROUP BY=');
      theClauses.Add('HAVING=');
      theClauses.Add('ORDER BY=');
      theClauses.Add('LIMIT=');

      GetClauses(GetSQLMemoText, theClauses);

      //Get columns Alias
      TableAlias:='';

      //Check if column's table is in where clause
      theEERTable:=theEERModel.GetEERTableByColumnID(col.Obj_id);
      if(theEERTable<>nil)then
      begin
        i:=Pos(theEERTable.ObjName+' ', theClauses.ValueFromIndex[1]);
        if(i>0)then
        begin
          s:=Copy(theClauses.ValueFromIndex[1], i+Length(theEERTable.ObjName)+1, Length(theClauses.ValueFromIndex[1]));

          if(Pos(',', s)=0)then
            TableAlias:=Trim(s)+'.'
          else
            TableAlias:=Trim(Copy(s, 1, Pos(',', s)-1))+'.';
        end
        else
        begin
          //Add table to FROM clause
          AddTableToSQLCommand(theEERTable, SQLctSELECT, SQLjtINNER);

          AddColumnToSQLCommand(columnPos, col);

          Exit;
        end;
      end;

      case columnPos of
        cpSelectClause, cpGroupClause, cpOrderClause:
        begin
            //Check if there is only a SELECT *, and replace the *
            //With the current column
            if(theClauses.ValueFromIndex[columnPos]='*')or
              (theClauses.ValueFromIndex[columnPos]='')then
              theClauses.ValueFromIndex[columnPos]:=TableAlias+col.ColName
            //If there is already a , after the last column
            else if(Copy(Trim(theClauses.ValueFromIndex[columnPos]),
              Length(Trim(theClauses.ValueFromIndex[columnPos])), 1)=',')then
              theClauses.ValueFromIndex[columnPos]:=
                Trim(theClauses.ValueFromIndex[columnPos])+' '+TableAlias+col.ColName
            //Add column to col-list
            else
              theClauses.ValueFromIndex[columnPos]:=
                Trim(theClauses.ValueFromIndex[columnPos])+', '+TableAlias+col.ColName;
        end;
        cpWhereClause, cpHavingClause:
        begin
          //if the last char is =, then simply add the col name
          if(Copy(Trim(theClauses.ValueFromIndex[columnPos]),
            Length(Trim(theClauses.ValueFromIndex[columnPos])), 1)='=')then
            theClauses.ValueFromIndex[columnPos]:=
              Trim(theClauses.ValueFromIndex[columnPos])+TableAlias+col.ColName
          //if there already is a WHERE condition, add AND
          else if(Trim(theClauses.ValueFromIndex[columnPos])<>'')then
            theClauses.ValueFromIndex[columnPos]:=
              Trim(theClauses.ValueFromIndex[columnPos])+' AND '+TableAlias+col.ColName+'='
          else
            theClauses.ValueFromIndex[columnPos]:=TableAlias+col.ColName+'=';
        end;
      end;
    end
    //UPDATE Command
    else if(Copy(GetSQLMemoText, 1, 6)='UPDATE')then
    begin
      //Build the Clauses
      theClauses.Add('UPDATE=');
      theClauses.Add('SET=');
      theClauses.Add('WHERE=');
      theClauses.Add('LIMIT=');

      GetClauses(GetSQLMemoText, theClauses);

      case columnPos of
        cpSetClause, cpWhereClause:
        begin
          //if the last char is =, then simply add the col name
          if(Copy(Trim(theClauses.ValueFromIndex[columnPos]),
            Length(Trim(theClauses.ValueFromIndex[columnPos])), 1)='=')then
            theClauses.ValueFromIndex[columnPos]:=
              Trim(theClauses.ValueFromIndex[columnPos])+TableAlias+col.ColName
          //if there already is a WHERE condition, add AND (or ,)
          else if(Trim(theClauses.ValueFromIndex[columnPos])<>'')then
            if(columnPos=cpWhereClause)then
              theClauses.ValueFromIndex[columnPos]:=
                Trim(theClauses.ValueFromIndex[columnPos])+' AND '+TableAlias+col.ColName+'='
            else
              theClauses.ValueFromIndex[columnPos]:=
                Trim(theClauses.ValueFromIndex[columnPos])+', '+TableAlias+col.ColName+'='
          else
            theClauses.ValueFromIndex[columnPos]:=TableAlias+col.ColName+'=';
        end;
      end;
    end
    //DELETE Command
    else if(Copy(GetSQLMemoText, 1, 6)='DELETE')then
    begin
      //Build the Clauses
      theClauses.Add('DELETE FROM=');
      theClauses.Add('WHERE=');
      theClauses.Add('LIMIT=');

      GetClauses(GetSQLMemoText, theClauses);

      case columnPos of
        cpDelWhereClause:
        begin
          //if the last char is =, then simply add the col name
          if(Copy(Trim(theClauses.ValueFromIndex[columnPos]),
            Length(Trim(theClauses.ValueFromIndex[columnPos])), 1)='=')then
            theClauses.ValueFromIndex[columnPos]:=
              Trim(theClauses.ValueFromIndex[columnPos])+TableAlias+col.ColName
          //if there already is a WHERE condition, add AND (or ,)
          else if(Trim(theClauses.ValueFromIndex[columnPos])<>'')then
            theClauses.ValueFromIndex[columnPos]:=
              Trim(theClauses.ValueFromIndex[columnPos])+' AND '+TableAlias+col.ColName+'='
          else
            theClauses.ValueFromIndex[columnPos]:=TableAlias+col.ColName+'=';
        end;
      end;
    end
    else
    begin
      columnPos:=0;
    end;

    //Build SQLStr from Clauses
    SQLStr:='';
    SetStart:=0;
    for i:=0 to theClauses.Count-1 do
      if(theClauses.ValueFromIndex[i]<>'')then
      begin
        SQLStr:=SQLStr+theClauses.Names[i]+' '+
          Trim(theClauses.ValueFromIndex[i])+#13#10;
        if(columnPos=i)then
          SetStart:=Length(SQLStr)-2;
      end;

    SetSQLMemoText(SQLStr);

{$IFDEF USE_SYNEDIT}
  if(DMGUI.UseSQLSyntaxHighlighting)then
  begin
    if(SQLSynEdit.Visible)then
      SQLSynEdit.SetFocus;

    if(SetStart=0)then
      SetStart:=Length(SQLStr);
    SQLSynEdit.SelLength:=0;
    SQLSynEdit.SelStart:=SetStart;
  end
  else
  begin
{$ENDIF}
    if(SQLPnl.Visible)then
      SQLMemo.SetFocus;

    if(SetStart=0)then
      SetStart:=Length(SQLStr);
    SQLMemo.SelLength:=0;
    SQLMemo.SelStart:=SetStart;
{$IFDEF USE_SYNEDIT}
  end;
{$ENDIF}

  finally
    theClauses.Free;
  end;
end;

procedure TEditorQueryForm.OutputDataSetProviderUpdateError(
  Sender: TObject; DataSet: TCustomClientDataSet; E: EUpdateError;
  UpdateKind: TUpdateKind; var Response: TResolverResponse);
begin
  raise E;
end;

procedure TEditorQueryForm.ExportallrecordsMIClick(Sender: TObject);
var theSaveDialog: TSaveDialog;
  RecentSaveFileAsDir: string;
begin
  if(OutputClientDataSet.Active)then
  begin
    theSaveDialog:=TSaveDialog.Create(nil);
    try
{$IFDEF MSWINDOWS}
      //On Windows use native Win32 Open Dlg
      theSaveDialog.UseNativeDialog:=True;
      theSaveDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
{$ENDIF}

      theSaveDialog.Title:='Export Records ...';
      theSaveDialog.Width:=600;
      theSaveDialog.Height:=450;
      theSaveDialog.DefaultExt:='csv';

      RecentSaveFileAsDir:=DMMain.LoadValueFromSettingsIniFile('RecentDirectories', 'RecentExportRecordsDir', '');
      if(Not(DirectoryExists(RecentSaveFileAsDir)))then
        RecentSaveFileAsDir:='';

      theSaveDialog.InitialDir:=RecentSaveFileAsDir;

      theSaveDialog.Filter:='CSV (*.csv)|*.csv|Text (*.txt)|*.txt';

      if(theSaveDialog.Execute)then
      begin
        Exportallrecords(theSaveDialog.FileName);

        RecentSaveFileAsDir:=ExtractFilePath(theSaveDialog.FileName);
        DMMain.SaveValueInSettingsIniFile('RecentDirectories', 'RecentExportRecordsDir', RecentSaveFileAsDir);
      end;
    finally
      theSaveDialog.Free;
    end;
  end;
end;

procedure TEditorQueryForm.ShowSQLImgClick(Sender: TObject);
begin
  //Show SQL Pnl
  ShowSQLImg.Hide;
  if(HideSQLImg.Top<>ShowSQLImg.Top)then
    HideSQLImg.Top:=ShowSQLImg.Top;
  HideSQLImg.Show;

  SQLPnl.Visible:=True;
  SQLSplitter.Visible:=True;

  if(Not(StoredSQLPnl.Visible))and(StoredSQLBtn.Down)then
    StoredSQLPnl.Visible:=True;
end;

procedure TEditorQueryForm.HideSQLImgClick(Sender: TObject);
begin
  //Hide SQL Pnl
  ShowSQLImg.Show;
  HideSQLImg.Hide;

  if(StoredSQLPnl.Visible)then
    StoredSQLPnl.Visible:=False;

  SQLSplitter.Visible:=False;
  SQLPnl.Visible:=False;
end;

procedure TEditorQueryForm.ShowBLOBImgClick(Sender: TObject);
begin
  //Hide Blob Pnl
  ShowBLOBImg.Hide;
  if(HideBLOBImg.Top<>ShowBLOBImg.Top)then
    HideBLOBImg.Top:=ShowBLOBImg.Top;
  HideBLOBImg.Show;

  BlobPnl.Visible:=True;
  BlobSplitter.Visible:=True;

  RightPnl.Left:=Width-10;
end;

procedure TEditorQueryForm.HideBLOBImgClick(Sender: TObject);
begin
  //Show Blob Pnl
  ShowBLOBImg.Show;
  HideBLOBImg.Hide;

  BlobSplitter.Visible:=False;
  BlobPnl.Visible:=False;
end;

procedure TEditorQueryForm.SQLMemoMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if(ssRight in Shift)then
{$IFDEF USE_SYNEDIT}
    if(DMGUI.UseSQLSyntaxHighlighting)then
      SQLSynEdit.BeginDrag(False, 3)
    else
{$ENDIF}
      SQLMemo.BeginDrag(False, 3);
end;

procedure TEditorQueryForm.StoredSQLTreeViewDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if(Source<>nil)and
    (TForm(Application.MainForm).ActiveMDIChild=nil)then
  begin
    if(Source.ClassNameIs('TMemo'))then
    begin
      if(TMemo(Source).Name='SQLMemo')then
        Accept:=True;
    end
    else if(Source.ClassNameIs('TSynEdit'))then
    begin
{$IFDEF USE_SYNEDIT}
      if(TSynEdit(Source).Name='SQLSynEdit')then
        Accept:=True;
{$ENDIF}
    end;
  end;
end;

procedure TEditorQueryForm.StoredSQLTreeViewDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  if(Source<>nil)then
    if(Source.ClassNameIs('TMemo'))then
    begin
      SaveSQLBtnClick(self);
      TMemo(Source).EndDrag(False);
    end
    else if(Source.ClassNameIs('TSynEdit'))then
    begin
{$IFDEF USE_SYNEDIT}
      SaveSQLBtnClick(self);
      //TSynEdit(Source).EndDrag(False);
{$ENDIF}
    end;
end;

procedure TEditorQueryForm.CopySQLToClipboard(Mode: Word);
var s: string;
  i: integer;
  theStringList: TStringList;
begin
  if(GetSQLMemoText<>'')then
  begin
    theStringList:=TStringList.Create;
    try
      theStringList.Text:=GetSQLMemoText;

      if(Mode=1)then
      begin
        s:='';
        for i:=0 to theStringList.Count-1 do
          if(Trim(theStringList[i])<>'')then
            s:=s+Trim(theStringList[i])+#13#10;

        s:='"'+DMMain.ReplaceText(Copy(s, 1, Length(s)-2), #13#10, ' " & _'+#13#10+'"')+'"';
      end
      else if(Mode=2)then
      begin
        s:='';
        for i:=0 to theStringList.Count-1 do
          if(Trim(theStringList[i])<>'')then
            s:=s+Trim(theStringList[i])+#13#10;

        s:=''''+DMMain.ReplaceText(Copy(s, 1, Length(s)-2), #13#10, ' ''+'+#13#10+'''')+'''';
      end
      else
        s:=GetSQLMemoText;

      Clipboard.AsText:=s;
      //WindowState:=wsMinimized;
    finally
      theStringList.Free;
    end;
  end;
end;


procedure TEditorQueryForm.CopyforASPMIClick(Sender: TObject);
begin
  CopySQLToClipboard(TMenuItem(Sender).Tag);
end;

procedure TEditorQueryForm.CopytoClipboardMIShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=OutputClientDataSet.Active;
end;

procedure TEditorQueryForm.ExportAllRecords(fname: string);
var s: string;
  ExportLines: TStringList;
  i: integer;
begin
  ExportLines:=TStringList.Create;
  try
    //Build Fieldnames
    s:='';
    for i:=0 to OutputClientDataSet.Fields.Count-1 do
      s:=s+'"'+OutputClientDataSet.Fields[i].DisplayName+'";';

    //Remove last ;
    s:=Copy(s, 1, Length(s)-1);

    ExportLines.Add(s);

    OutputClientDataSet.DisableControls;
    try
      OutputClientDataSet.First;
      while(not(OutputClientDataSet.Eof))do
      begin
        s:='';
        for i:=0 to OutputClientDataSet.Fields.Count-1 do
        begin
          if(OutputClientDataSet.Fields[i].DataType=ftSmallint)or
            (OutputClientDataSet.Fields[i].DataType=ftInteger)or
            (OutputClientDataSet.Fields[i].DataType=ftWord)or
            (OutputClientDataSet.Fields[i].DataType=ftLargeint)or
            (OutputClientDataSet.Fields[i].DataType=ftAutoInc)then
          begin
            if(OutputClientDataSet.Fields[i].AsString<>'')then
              s:=s+OutputClientDataSet.Fields[i].AsString
            else
              s:=s+'0';
          end
          else if(OutputClientDataSet.Fields[i].DataType=ftString)or
            (OutputClientDataSet.Fields[i].DataType=ftFixedChar)or
            (OutputClientDataSet.Fields[i].DataType=ftWideString)then
            s:=s+'"'+DMMain.ReplaceText(OutputClientDataSet.Fields[i].AsString, '"', '""')+'"'
          else if(OutputClientDataSet.Fields[i].DataType=ftDateTime)or
            (OutputClientDataSet.Fields[i].DataType=ftTimeStamp)then
            s:=s+'"'+OutputClientDataSet.Fields[i].AsString+'"'
          else if(OutputClientDataSet.Fields[i].DataType=ftDate)or
            (OutputClientDataSet.Fields[i].DataType=ftTime)then
            s:=s+'"'+OutputClientDataSet.Fields[i].AsString+'"'
          else
            s:=s+'"'+DMMain.ReplaceText(OutputClientDataSet.Fields[i].AsString, '"', '""')+'"';

          if(i<OutputClientDataSet.Fields.Count-1)then
            s:=s+';';
        end;

        ExportLines.Add(s);

        OutputClientDataSet.Next;
      end;

      OutputClientDataSet.First;
    finally
      OutputClientDataSet.EnableControls;
    end;

    if(fname<>'')then
      ExportLines.SaveToFile(fname)
    else
      Clipboard.AsText:=ExportLines.Text;
  finally
    ExportLines.Free;
  end;
end;

procedure TEditorQueryForm.CopyallRecordsMIClick(
  Sender: TObject);
begin
  ExportAllRecords('');
end;

procedure TEditorQueryForm.CopyFieldNames1Click(Sender: TObject);
var s: string;
  i: integer;
begin
  //Build Fieldnames
  s:='';
  for i:=0 to OutputClientDataSet.Fields.Count-1 do
    s:=s+OutputClientDataSet.Fields[i].DisplayName+', ';

  //Remove last ,
  s:=Copy(s, 1, Length(s)-1);

  Clipboard.AsText:=s;
end;

procedure TEditorQueryForm.CopyallRecordsasINSERTsMIClick(Sender: TObject);
begin
  Clipboard.AsText:=DMDB.GetRecordsAsInserts(GetSQLMemoText, OutputClientDataSet);
end;

procedure TEditorQueryForm.FormDeactivate(Sender: TObject);
begin
  if(Visible)then
    if(Not(DMMain.IsFormStayingOnTop(self)))then
      sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));
end;

procedure TEditorQueryForm.PasteremovingQuotesMIClick(Sender: TObject);
var SQLLines: TStringList;
  s: string;
  i: integer;
begin
  SQLLines:=TStringList.Create;
  try
    SQLLines.Text:=Clipboard.AsText;

    for i:=0 to SQLLines.Count-1 do
    begin
      s:=trim(SQLLines[i]);

      //Remove SQLStr.SQL.Text:=
      if(Copy(s, 1, 1)<>'''')and(Pos(':=', s)>0)then
        s:=Copy(s, Pos(':=', s)+2, Length(s));

      //Remove leading ' and trailing '+
      if(Copy(s, 1, 1)='''')and(Copy(s, Length(s)-1, 2)='''+')then
        s:=Copy(s, 2, Length(s)-3);

      //Remove leading ' and trailing '
      if(Copy(s, 1, 1)='''')and(Copy(s, Length(s), 1)='''')then
        s:=Copy(s, 2, Length(s)-2);

      //Remove leading " and trailing " & _
      if(Copy(s, 1, 1)='"')and(Copy(s, Length(s)-4, 5)='" & _')then
        s:=Copy(s, 2, Length(s)-6);
      if(Copy(s, 1, 1)='"')and(Copy(s, Length(s)-3, 4)='"& _')then
        s:=Copy(s, 2, Length(s)-5);
      if(Copy(s, 1, 1)='"')and(Copy(s, Length(s)-3, 4)='" &_')then
        s:=Copy(s, 2, Length(s)-5);
      if(Copy(s, 1, 1)='"')and(Copy(s, Length(s)-2, 3)='"&_')then
        s:=Copy(s, 2, Length(s)-4);
      if(Copy(s, 1, 1)='"')and(Copy(s, Length(s), 1)='"')then
        s:=Copy(s, 2, Length(s)-2);

      //Remove leading ' and trailing ';
      if(Copy(s, 1, 1)='''')and(Copy(s, Length(s)-1, 2)=''';')then
        s:=Copy(s, 2, Length(s)-3);

      SQLLines[i]:=s;
    end;

    SetSQLMemoText(SQLLines.Text);
  finally
    SQLLines.Free;
  end;

end;

procedure TEditorQueryForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_F1)then
    DMMain.ShowHelp('editors', 'queryed');
end;

procedure TEditorQueryForm.LoadSQLScriptfromFileMIClick(
  Sender: TObject);
var theOpenDialog: TOpenDialog;
  theStringList: TStringList;
begin
  theOpenDialog:=TOpenDialog.Create(nil);
  theStringList:=TStringList.Create;
  try
{$IFDEF MSWINDOWS}
    //On Windows use native Win32 Open Dlg
    theOpenDialog.UseNativeDialog:=True;
    theOpenDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
{$ENDIF}

    theOpenDialog.Title:='Open a SQL Script ...';
    theOpenDialog.DefaultExt:='xml';
    theOpenDialog.Filter:='SQL Script (*.sql)|*.sql|Any File (*.*)|*.*';
    theOpenDialog.Width:=600;
    theOpenDialog.Height:=450;

    if(DirectoryExists(DMGUI.RecentOpenSQLScriptFileDir))then
      theOpenDialog.InitialDir:=DMGUI.RecentOpenSQLScriptFileDir
    else
      theOpenDialog.InitialDir:='';

    if(theOpenDialog.Execute)then
    begin
      theStringList.LoadFromFile(theOpenDialog.FileName);

      SetSQLMemoText(theStringList.Text);

      DMGUI.RecentOpenSQLScriptFileDir:=ExtractFilePath(theOpenDialog.FileName);
    end;
  finally
    theStringList.Free;
    theOpenDialog.Free;
  end;
end;

procedure TEditorQueryForm.PrevCmdClick(Sender: TObject);
var {HistoryCount,} i: integer;
begin
  //HistoryCount:=0;
  i:=0;
  while(i<theEERModel.StoredSQLCmds.Count)do
  begin
    if(Copy(TStoredSQLCmd(theEERModel.StoredSQLCmds[i]).StoredPosition, 1, 8)=
      'History/')then
    begin
      //inc(HistoryCount);
    end;

    inc(i);
  end;

  //dec(currentHistoryPos);
end;

procedure TEditorQueryForm.SwitchQueryLayoutBtnClick(Sender: TObject);
var StoreCurrentLayoutSettings: Boolean;
begin
  //Store current layout settings only when this is the docked Query Editor
  StoreCurrentLayoutSettings:=(QueryDockPnl.Parent.Name='QueryPnl');

  if(SQLPnl.Align=alLeft)then
    SetLayout(2, StoreCurrentLayoutSettings)
  else
    SetLayout(1, StoreCurrentLayoutSettings);

end;

procedure TEditorQueryForm.SetLayout(Layout: integer; StoreCurrentLayoutSettings: Boolean);
begin
  if(Layout=2)then
  begin
    if(StoreCurrentLayoutSettings)then
      StoreLayout(1);

    //Set layout
    SQLPnl.Align:=alTop;
    SQLPnl.Height:=DMGUI.DockedQueryPnlSQLPnlSizeM2; //144
    SQLSplitter.Align:=alTop;
    SQLSplitter.Cursor:=crVSplit;
    SQLSplitter.Height:=3;
    SQLSplitter.Top:=1000;

    SQLFuncPnl.Width:=76;
    TempSQLStorePnl.Top:=0;
    TempSQLStorePnl.Left:=42;

    //Adjust the StoredSQLPnl
    if(StoredSQLPnl.Visible<>DMGUI.DockedQueryPnlStoredSQLTreeVisibleM2)then
    begin
      StoredSQLBtnClick(self);
      StoredSQLBtn.Down:=DMGUI.DockedQueryPnlStoredSQLTreeVisibleM2;
    end;
    StoredSQLPnl.Width:=DMGUI.DockedQueryPnlStoredSQLTreeWidthM2;

    //Adjust the BlobPnl
    if(BlobPnl.Visible<>DMGUI.DockedQueryPnlBLOBPnlVisibleM2)then
      if(Not(BlobPnl.Visible))then
        ShowBLOBImgClick(self)
      else
        HideBLOBImgClick(self);
    BlobPnl.Width:=DMGUI.DockedQueryPnlBLOBPnlWidthM2;

    if(QueryDockPnl.Parent.Name='QueryPnl')then
    begin
      QueryDockPnl.Parent.Height:=DMGUI.DockedQueryPnlHeightM2; //300
      QueryDockPnl.Parent.Top:=0;
      if(QueryDockPnl.Parent.Owner.FindComponent('QuerySplitter')<>nil)then
        TSplitter(QueryDockPnl.Parent.Owner.FindComponent('QuerySplitter')).Top:=-1;
    end;

  end
  else
  begin
    if(StoreCurrentLayoutSettings)then
      StoreLayout(2);

    SQLPnl.Align:=alLeft;
    SQLPnl.Width:=DMGUI.DockedQueryPnlSQLPnlSizeM1; //450
    SQLSplitter.Align:=alLeft;
    SQLSplitter.Cursor:=crHSplit;
    SQLSplitter.Width:=3;
    SQLSplitter.Left:=1000;

    SQLFuncPnl.Width:=29;
    TempSQLStorePnl.Top:=151;
    TempSQLStorePnl.Left:=4;

    //Adjust the StoredSQLPnl
    if(StoredSQLPnl.Visible<>DMGUI.DockedQueryPnlStoredSQLTreeVisibleM1)then
    begin
      StoredSQLBtnClick(self);
      StoredSQLBtn.Down:=DMGUI.DockedQueryPnlStoredSQLTreeVisibleM1;
    end;
    StoredSQLPnl.Width:=DMGUI.DockedQueryPnlStoredSQLTreeWidthM1;

    //Adjust the BlobPnl
    if(BlobPnl.Visible<>DMGUI.DockedQueryPnlBLOBPnlVisibleM1)then
      if(Not(BlobPnl.Visible))then
        ShowBLOBImgClick(self)
      else
        HideBLOBImgClick(self);
    BlobPnl.Width:=DMGUI.DockedQueryPnlBLOBPnlWidthM1;

    if(QueryDockPnl.Parent.Name='QueryPnl')then
      QueryDockPnl.Parent.Height:=DMGUI.DockedQueryPnlHeightM1;
  end;

  //Resize Stored SQL Cmds Header
  StoredSQLSplitterMoved(self);

  DMGUI.DockedQueryPnlMode:=Layout;
end;

procedure TEditorQueryForm.StoreLayout(Layout: integer);
begin
  //Store current layout settings
  if(Layout=1)then
  begin
    DMGUI.DockedQueryPnlSQLPnlSizeM1:=SQLPnl.Width;

    DMGUI.DockedQueryPnlStoredSQLTreeVisibleM1:=StoredSQLPnl.Visible;
    DMGUI.DockedQueryPnlStoredSQLTreeWidthM1:=StoredSQLPnl.Width;

    DMGUI.DockedQueryPnlBLOBPnlVisibleM1:=BlobPnl.Visible;
    DMGUI.DockedQueryPnlBLOBPnlWidthM1:=BlobPnl.Width;


    if(QueryDockPnl.Parent.Name='QueryPnl')then
      DMGUI.DockedQueryPnlHeightM1:=QueryDockPnl.Parent.Height;
  end
  else
  begin
    DMGUI.DockedQueryPnlSQLPnlSizeM2:=SQLPnl.Height;

    DMGUI.DockedQueryPnlStoredSQLTreeVisibleM2:=StoredSQLPnl.Visible;
    DMGUI.DockedQueryPnlStoredSQLTreeWidthM2:=StoredSQLPnl.Width;

    DMGUI.DockedQueryPnlBLOBPnlVisibleM2:=BlobPnl.Visible;
    DMGUI.DockedQueryPnlBLOBPnlWidthM2:=BlobPnl.Width;


    if(QueryDockPnl.Parent.Name='QueryPnl')then
      DMGUI.DockedQueryPnlHeightM2:=QueryDockPnl.Parent.Height;
  end;
end;

procedure TEditorQueryForm.TempSQLStore1SBtnDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=False;

  if(Assigned(Source))and
    (TForm(Application.MainForm).ActiveMDIChild<>nil)then
  begin
    if(Source.ClassNameIs('TMemo'))then
      if(TMemo(Source).Name='SQLMemo')then
        Accept:=True;

{$IFDEF USE_SYNEDIT}
    if(DMGUI.UseSQLSyntaxHighlighting)then
      if(Source.ClassNameIs('TSynEdit'))then
        if(TSynEdit(Source).Name='SQLSynEdit')then
          Accept:=True;
{$ENDIF}
  end;
end;

procedure TEditorQueryForm.TempSQLStore1SBtnDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var theSQLCmd: TStoredSQLCmd;
  theOldCmdPos: integer;
  StorePos, txt: string;
begin
  if(TForm(Application.MainForm).ActiveMDIChild=nil)then
    Exit;

  if(Assigned(Source))then
  begin
    txt:='';

    if(Source.ClassNameIs('TMemo'))then
      if(TMemo(Source).Name='SQLMemo')then
      begin
        txt:=TMemo(Source).Text;
        TMemo(Source).EndDrag(False);
      end;

{$IFDEF USE_SYNEDIT}
    if(DMGUI.UseSQLSyntaxHighlighting)then
      if(Source.ClassNameIs('TSynEdit'))then
        if(TSynEdit(Source).Name='SQLSynEdit')then
        begin
          txt:=TSynEdit(Source).Text;
          //TSynEdit(Source).EndDrag(False);
        end;
{$ENDIF}

    if(txt<>'')then
    begin
      //Get dragged position
      if(Sender.ClassNameIs('TSpeedButton'))then
        StorePos:=IntToStr(TSpeedButton(Sender).Tag)
      else if(Sender.ClassNameIs('TPanel'))then
        StorePos:=IntToStr(Y div 17+1);

      theSQLCmd:=TStoredSQLCmd.Create(ct_SQLDragDropStores,
        StorePos,
        txt);


      theOldCmdPos:=theEERModel.GetStoredSQLCmdIndex(
        ct_SQLDragDropStores, StorePos);

      if(theOldCmdPos>-1)then
        theEERModel.StoredSQLCmds.Delete(theOldCmdPos);

      theEERModel.StoredSQLCmds.Add(theSQLCmd);

      if(Sender.ClassNameIs('TSpeedButton'))then
        TSpeedButton(Sender).Enabled:=True
      else if(Sender.ClassNameIs('TPanel'))then
        if(FindComponent('TempSQLStore'+StorePos+'SBtn')<>nil)then
        TSpeedButton(FindComponent('TempSQLStore'+StorePos+'SBtn')).Enabled:=True
    end;
  end;
end;

procedure TEditorQueryForm.TempSQLStore1SBtnClick(Sender: TObject);
var theStoredCmdPos: integer;
begin
  if(TForm(Application.MainForm).ActiveMDIChild=nil)then
    Exit;

  theStoredCmdPos:=theEERModel.GetStoredSQLCmdIndex(
    ct_SQLDragDropStores, IntToStr(TSpeedButton(Sender).Tag));

  if(theStoredCmdPos>-1)then
    SetSQLMemoText(TStoredSQLCmd(theEERModel.StoredSQLCmds[theStoredCmdPos]).SQLText);
end;

procedure TEditorQueryForm.RefreshTempSQLStoreBtns(theModel: TEERModel);
var i: integer;
begin
  for i:=1 to 8 do
    if(theModel<>nil)then
    begin
      if(theModel.GetStoredSQLCmdIndex(
        ct_SQLDragDropStores, IntToStr(i))>-1)then
        TSpeedButton(FindComponent('TempSQLStore'+IntToStr(i)+'SBtn')).Enabled:=True
      else
        TSpeedButton(FindComponent('TempSQLStore'+IntToStr(i)+'SBtn')).Enabled:=False;
    end
    else
      TSpeedButton(FindComponent('TempSQLStore'+IntToStr(i)+'SBtn')).Enabled:=False;
end;

procedure TEditorQueryForm.TempSQLStore1SBtnMouseEnter(Sender: TObject);
var theStoredCmdPos: integer;
begin
  if(TForm(Application.MainForm).ActiveMDIChild=nil)then
    Exit;

  //Set the Hint when mouse enters the button
  theStoredCmdPos:=theEERModel.GetStoredSQLCmdIndex(
    ct_SQLDragDropStores, IntToStr(TSpeedButton(Sender).Tag));

  if(theStoredCmdPos>-1)then
    TSpeedButton(Sender).Hint:=Trim(TStoredSQLCmd(theEERModel.StoredSQLCmds[theStoredCmdPos]).SQLText);

end;

procedure TEditorQueryForm.StoredSQLTreeViewItemEnter(Sender: TObject;
  Node: TTreeNode);
var R: TRect;
begin
  if(StoredSQLTreeView.EditingItem=nil)then
    if(Assigned(Node))then
    begin
      if(Assigned(Node.Data))then
      begin
        StoredSQLTreeView.Hint:=Trim(TStoredSQLCmd(Node.Data).SQLText);

        Application.HintPause:=0;
        theHintPauseTmr.Enabled:=False;
        theHintPauseTmr.Enabled:=True;
        Application.HideHint;
        Application.ProcessMessages;

        R:=theHintWindow.CalcHintRect(255, StoredSQLTreeView.Hint, nil);
        R.TopLeft := Mouse.CursorPos;
        //theHintWindow.
        theHintWindow.ActivateHint(R, StoredSQLTreeView.Hint);
      end;
    end;
end;

procedure TEditorQueryForm.StoredSQLTreeViewItemExitViewportEnter(
  Sender: TObject);
begin
  StoredSQLTreeView.Hint:='';
end;


procedure TEditorQueryForm.StoredSQLTreeViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if(ssLeft in Shift)or(ssRight in Shift)then
    StoredSQLTreeView.BeginDrag(False, 3);
end;

procedure TEditorQueryForm.SQLMemoDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if(Assigned(Source))then
    if(Source.ClassNameIs('TTreeView'))then
    begin
      if(TTreeView(Source).Name='StoredSQLTreeView')then
        Accept:=True;
    end
    else if(Source.ClassNameIs('TSpeedButton'))then
      if(Copy(TSpeedButton(Source).Name, 1, 12)='TempSQLStore')then
        Accept:=True;
end;

procedure TEditorQueryForm.SQLMemoDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var theStoredCmdPos: integer;
begin
  if(Assigned(Source))then
    if(Source.ClassNameIs('TTreeView'))then
    begin
      if(TTreeView(Source).Name='StoredSQLTreeView')then
      begin
        if(Assigned(TTreeView(Source).Selected))then
          if(Assigned(TTreeView(Source).Selected.Data))then
            SetSQLMemoText(TStoredSQLCmd(TTreeView(Source).Selected.Data).SQLText);
      end;
    end
    else if(Source.ClassNameIs('TSpeedButton'))then
      if(Copy(TSpeedButton(Source).Name, 1, 12)='TempSQLStore')and
        (TForm(Application.MainForm).ActiveMDIChild<>nil)then
      begin
        theStoredCmdPos:=theEERModel.GetStoredSQLCmdIndex(
          ct_SQLDragDropStores, IntToStr(TSpeedButton(Source).Tag));

        if(theStoredCmdPos>-1)then
          SetSQLMemoText(TStoredSQLCmd(theEERModel.StoredSQLCmds[theStoredCmdPos]).SQLText);
      end;
end;

procedure TEditorQueryForm.DoHintPauseTmr(Sender: TObject);
begin
  Application.HintPause:=500;
  theHintPauseTmr.Enabled:=False;
end;

procedure TEditorQueryForm.TempSQLStore1SBtnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if(ssLeft in Shift)or(ssRight in Shift)then
    TSpeedButton(Sender).BeginDrag(False, 3);
end;

procedure TEditorQueryForm.StoredSQLPopupMenuPopup(Sender: TObject);
begin
  if(StoredSQLTreeView.EditingItem<>nil)then
    raise EAbort.Create('');
end;

procedure TEditorQueryForm.StoredSQLSplitterMoved(Sender: TObject);
begin
  if(StoredSQLTreeView.Width<140)then
    StoredSQLTreeView.Columns[0].Width:=140
  else
    StoredSQLTreeView.Columns[0].Width:=StoredSQLTreeView.Width-4;

  {if(QScrollView_visibleWidth(StoredSQLTreeView.Handle)+4<>StoredSQLTreeView.Width)then
    ShowMessage(IntToStr(QScrollView_visibleWidth(StoredSQLTreeView.Handle))+', '+IntToStr(StoredSQLTreeView.Width));}
end;

procedure TEditorQueryForm.DBGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
var theTextRect: TRect;
begin
  with DBGrid.Canvas do
  begin
    if(gdSelected in State)then
    begin
      Brush.Color:=$00AAAAAA;
    end
    else
    begin
      Brush.Color:=clWhite;
    end;

    if(gdFocused in State)then
      Pen.Color:=clBlack
    else
      Pen.Style:=psClear;


    Rectangle(Rect);
    Pen.Style:=psSolid;

    theTextRect.Left:=Rect.Left+1;
    theTextRect.Top:=Rect.Top+1;
    theTextRect.Right:=Rect.Right-1;
    theTextRect.Bottom:=Rect.Bottom-1;
    TextRect(theTextRect, Rect.Left+2, Rect.Top+1, Column.Field.AsString);
  end;
end;

procedure TEditorQueryForm.OutputClientDataSetAfterOpen(DataSet: TDataSet);
var i: integer;
begin
  for i:=0 to DataSet.FieldCount-1 do
  begin
    DataSet.Fields[i].OnGetText:=DoFieldGetText;
  end;

end;

procedure TEditorQueryForm.DoFieldGetText(Sender: TField; var Text: String; DisplayText: Boolean);
begin
  //Catch empty datetime fields
  try
    Text:=TField(Sender).AsString;
  except
    on x: Exception do
    begin
      //To avoid infinite loops, set all fields with EConvertErrors to NULL
      if(x.ClassNameIs('EConvertError'))then
      begin
        //This may fail if the select is too complex...
        try
          TField(Sender).DataSet.Edit;
          TField(Sender).Clear;
        except
        end;
      end;
    end;
  end;
end;

procedure TEditorQueryForm.SetSQLMemoText(Text: string);
begin
{$IFDEF USE_SYNEDIT}
  if(DMGUI.UseSQLSyntaxHighlighting)then
    SQLSynEdit.Text:=Text
  else
{$ENDIF}
    SQLMemo.Text:=Text;
end;

function TEditorQueryForm.GetSQLMemoText: string;
var i: integer;
  s: string;
begin
{$IFDEF USE_SYNEDIT}
  if(DMGUI.UseSQLSyntaxHighlighting)then
  begin
    s:='';
    for i:=0 to SQLSynEdit.Lines.Count-1 do
    begin
      s:=s+SQLSynEdit.Lines[i];
      if(i<SQLSynEdit.Lines.Count-1)then
        s:=s+#13#10;
    end;
    GetSQLMemoText:=s;
  end
  else
{$ENDIF}
    GetSQLMemoText:=SQLMemo.Text;

end;

procedure TEditorQueryForm.PrintRecordstoPDFMIClick(Sender: TObject);
var theSaveDialog: TSaveDialog;
  RecentSaveFileAsDir, s: string;
begin
  if(OutputClientDataSet.Active)then
  begin
    theSaveDialog:=TSaveDialog.Create(nil);
    try
{$IFDEF MSWINDOWS}
      //On Windows use native Win32 Open Dlg
      theSaveDialog.UseNativeDialog:=True;
      theSaveDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
{$ENDIF}

      theSaveDialog.Title:='Print Records to PDF ...';
      theSaveDialog.Width:=600;
      theSaveDialog.Height:=450;
      theSaveDialog.DefaultExt:='pdf';

      RecentSaveFileAsDir:=DMMain.LoadValueFromSettingsIniFile('RecentDirectories', 'RecentPrintRecordsDir', '');
      if(Not(DirectoryExists(RecentSaveFileAsDir)))then
        RecentSaveFileAsDir:='';

      theSaveDialog.InitialDir:=RecentSaveFileAsDir;

      theSaveDialog.Filter:='PDF (*.pdf)';

      if(theSaveDialog.Execute)then
      begin
        s:=Copy(ExtractFileName(theSaveDialog.FileName),
            1, Length(ExtractFileName(theSaveDialog.FileName))-Length(ExtractFileExt(theSaveDialog.FileName)));

        MakePDFFromDBGrid(DBGrid, theSaveDialog.FileName, s);

        RecentSaveFileAsDir:=ExtractFilePath(theSaveDialog.FileName);
        DMMain.SaveValueInSettingsIniFile('RecentDirectories', 'RecentPrintRecordsDir', RecentSaveFileAsDir);

{$IFDEF MSWINDOWS}
        s:=theSaveDialog.FileName;
        ShellExecute(0, 'Open', PAnsiChar(s), '', '', SW_SHOW);
{$ENDIF}
      end;
    finally
      theSaveDialog.Free;
    end;
  end;
end;

end.
