unit EditorTable;

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
//
// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------
//
// Unit EditorString.pas
// ---------------------
// Version 1.2, 21.03.2003, Mike
// Description
//   Editor for the tables
//
// Changes:
//   Version 1.2, 21.03.2003, Mike
//     Get only one GlobalID for the index in NewIndexBtnClick
//   Version 1.1, 20.03.2003, Mike
//     added popupmenu to IndexColumns to change length parameter
//     and EditIndexColumnLengthMIClick procedure
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QImgList, QComCtrls, QGrids, Qt, EERModel, QExtCtrls,
  QCheckLst, QButtons, Math, QMenus, QTypes, StrUtils, QClipbrd,
  EditorTableFieldDatatypeInplace, QMask;

type
  TEditorTableForm = class(TForm)
    DatatypesImgList: TImageList;
    ColPopupMenu: TPopupMenu;
    DeleteColumnMI: TMenuItem;
    OptionsPnl: TPanel;
    TablePageControl: TPageControl;
    IndexSheet: TTabSheet;
    NewIndexBtn: TSpeedButton;
    DelIndexBtn: TSpeedButton;
    IndexGroupbox: TGroupBox;
    DeleteColFromIndexBtn: TSpeedButton;
    IndexTypeCBox: TComboBox;
    Label11: TLabel;
    IndexNameEd: TEdit;
    IndexColListBox: TListBox;
    IndexListBox: TListBox;
    StdInsertsSheet: TTabSheet;
    StdInsertMemo: TMemo;
    TblOptionsSheet: TTabSheet;
    Label4: TLabel;
    NextAutoIncEd: TEdit;
    Label5: TLabel;
    TblPasswordEd: TEdit;
    PackKeysCBox: TCheckBox;
    GroupBox1: TGroupBox;
    Label7: TLabel;
    AverageRowLengthEd: TEdit;
    RowChecksumCBox: TCheckBox;
    Label9: TLabel;
    MaxRowNumberEd: TEdit;
    Label8: TLabel;
    MinRowNumberEd: TEdit;
    RowFormatLU: TComboBox;
    Label14: TLabel;
    DelayKeyTblUpdatesCBox: TCheckBox;
    Label16: TLabel;
    TabSheet1: TTabSheet;
    RaidPnl: TGroupBox;
    RaidTypeLU: TComboBox;
    ChunksEd: TEdit;
    ChunkSizeEd: TEdit;
    ChunksLbl: TLabel;
    ChunksizeLbl: TLabel;
    UseRaidCBox: TCheckBox;
    Label15: TLabel;
    Label18: TLabel;
    Label6: TLabel;
    TblDataDirEd: TEdit;
    Label10: TLabel;
    TblIndexDirEd: TEdit;
    StdInsertsPopupMenu: TPopupMenu;
    PasteSQLInsertMI: TMenuItem;
    TopPnl: TPanel;
    Label1: TLabel;
    TableNameEd: TEdit;
    MidPnl: TPanel;
    MidLeftPnl: TPanel;
    Panel5: TPanel;
    ColumnGrid: TDrawGrid;
    MouseEditTmr: TTimer;
    CommentsSheet: TTabSheet;
    CommentsMemo: TMemo;
    TemporaryCBox: TCheckBox;
    IndexColumnsPopupMenu: TPopupMenu;
    EditIndexColumnLengthMI: TMenuItem;
    N1: TMenuItem;
    DeleteColumnFromIndexMI: TMenuItem;
    BottomPnl: TPanel;
    PageControlTreeView: TTreeView;
    PageControlTitlePnl: TPanel;
    PageControlTitleLbl: TLabel;
    IndexColumnsLbl: TLabel;
    IndexDragHintLbl: TLabel;
    BottomRightPnl: TPanel;
    SubmitBtn: TSpeedButton;
    AbortBtn: TSpeedButton;
    N2: TMenuItem;
    ClearAllSQLInsertsMI: TMenuItem;
    TopRightPnl: TPanel;
    Label3: TLabel;
    TableTypeCBox: TComboBox;
    Label17: TLabel;
    TablePrefixComboBox: TComboBox;
    nmTableCBox: TCheckBox;
    Label19: TLabel;
    OptionSplitter: TSplitter;
    PageControlTitleShape: TShape;
    N3: TMenuItem;
    AddColPrefixMI: TMenuItem;
    AddColPostfixMI: TMenuItem;
    N4: TMenuItem;
    AddColumnstoSelectedIndexMI: TMenuItem;
    MoveRowupMI: TMenuItem;
    MoveRowdownMI: TMenuItem;
    N5: TMenuItem;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplyChanges;

    procedure FormCreate(Sender: TObject);
    procedure ColumnGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure ColumnGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ColumnGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure ColumnGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ColumnGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ColumnGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure SetTable(theTable: TEERTable);
    procedure TableNameEdKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TableNameEdExit(Sender: TObject);
    procedure TablePageCBoxCloseUp(Sender: TObject);
    procedure ColumnGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure DeleteColumnMIClick(Sender: TObject);
    procedure NewIndexBtnClick(Sender: TObject);

    procedure CheckPrimaryIndex;
    procedure ShowIndex(index_id: integer);
    procedure IndexColListBoxDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure IndexColListBoxDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure DeleteColFromIndexBtnClick(Sender: TObject);

    procedure RefreshCurrentIndex;

    procedure IndexListBoxClick(Sender: TObject);
    procedure IndexListBoxDblClick(Sender: TObject);
    procedure IndexTypeCBoxCloseUp(Sender: TObject);
    procedure IndexColListBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DelIndexBtnClick(Sender: TObject);

    procedure EditDatatype;
    procedure EditCellStr(NewName: string = '');
    procedure ColumnGridDblClick(Sender: TObject);
    procedure PasteSQLInsertMIClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MouseEditTmrTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure EditIndexColumnLengthMIClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
    procedure AbortBtnClick(Sender: TObject);
    procedure SubmitBtnMouseEnter(Sender: TObject);
    procedure SubmitBtnMouseLeave(Sender: TObject);
    procedure PageControlTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure FormResize(Sender: TObject);
    procedure ClearAllSQLInsertsMIClick(Sender: TObject);
    procedure OptionSplitterMoved(Sender: TObject);
    procedure OptionSplitterCanResize(Sender: TObject;
      var NewSize: Integer; var Accept: Boolean);
    procedure GetTranslations;
    procedure AddColPrefixMIClick(Sender: TObject);
    procedure AddColumnstoSelectedIndexMIClick(Sender: TObject);
    procedure InsertColumn;
    procedure MoveRowupMIClick(Sender: TObject);
    procedure MoveRowdownMIClick(Sender: TObject);
  private
    { Private declarations }
    DiscardChanges: Boolean;

    DragStartRow: integer;
    LastClickedCol: integer;
    //DragShiftState: TShiftState;

    UndoXML: string;

    ColumnGridColumnCaptions: TStringList;
  public
    { Public declarations }
    EERModel: TEERModel;
    EERTable, SourceEERTable: TEERTable;

    EditorTableFieldEdit: TEdit;
    EditorTableFieldDatatypeInplaceEditor: TEditorTableFieldDatatypeInplaceEditor;

    DoCellEdit: Boolean;
  end;

var
  EditorTableForm: TEditorTableForm;

implementation

uses MainDM, EERDM, GUIDM, EditorTableField;

{$R *.xfm}

procedure TEditorTableForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self, True);

  DiscardChanges:=False;

  EERModel:=nil;
  EERTable:=nil;
  SourceEERTable:=nil;

  EditorTableFieldEdit:=TEditorTableFieldEdit.Create(self);
  EditorTableFieldDatatypeInplaceEditor:=TEditorTableFieldDatatypeInplaceEditor.Create(self);
  {EditorTableFieldEdit.Parent:=self;
  EditorTableFieldEdit.Visible:=False;}

  {Left:=(Screen.Width-Width) div 2-40;
  Top:=(Screen.Height-Height) div 2;}
  //DMMain.RestoreWinPos(self, True);

  ColumnGrid.Font.Name:=Font.Name;
  ColumnGrid.Font.Size:=Font.Size;
  ColumnGrid.Canvas.Font.Name:=Font.Name;
  ColumnGrid.Canvas.Font.Size:=Font.Size;

  ColumnGrid.ColCount:=9;
  //Column Icon
  ColumnGrid.ColWidths[0]:=20;
  //Column Name
  ColumnGrid.ColWidths[1]:=104;
  //Datatype Icon
  ColumnGrid.ColWidths[2]:=20;
  //Datatype
  ColumnGrid.ColWidths[3]:=115;
  //Not Null
  ColumnGrid.ColWidths[4]:=20;
  //Auto Inc
  ColumnGrid.ColWidths[5]:=20;
  //Options
  ColumnGrid.ColWidths[6]:=160;
  //Default Val
  ColumnGrid.ColWidths[7]:=80;
  //Comments
  ColumnGrid.ColWidths[8]:=100;

  ColumnGrid.Col:=1;
  ColumnGrid.Row:=1;

  ColumnGrid.RowCount:=2;

  PageControlTreeView.FullExpand;
  TablePageControl.ActivePageIndex:=0;

  IndexListBox.Clear;

  DoCellEdit:=False;

  IndexDragHintLbl.Font.Color:=clGray;
  PageControlTitleLbl.Font.Style:=[fsBold];

  ColumnGridColumnCaptions:=TStringList.Create;

  //Get translated strings
  GetTranslations;

  StdInsertMemo.Font.Name:=DMGUI.SQLTextFont;
  StdInsertMemo.Font.Size:=DMGUI.SQLTextFontSize;

  OptionsPnl.Height:=DMEER.TableSplitterPos;
  OptionSplitterMoved(self);

  BottomPnl.Top:=1000;
end;

procedure TEditorTableForm.FormDestroy(Sender: TObject);
begin
  DMMain.SaveWinPos(self, True);

  if(Assigned(EERTable))then
    EERTable.Free;

  ColumnGridColumnCaptions.Free;
end;

procedure TEditorTableForm.GetTranslations;
var theStrings: TStringList;
  i: integer;
begin
  //Translate ColumnGridColumn Captions
  ColumnGridColumnCaptions.Clear;

  DMMain.GetFormResourceStrings(self, 'ColumnGrid', ColumnGridColumnCaptions);

  if(ColumnGridColumnCaptions.Count=0)then
  begin
    ColumnGridColumnCaptions.Add('Column Name');
    ColumnGridColumnCaptions.Add('DataType');
    ColumnGridColumnCaptions.Add('NN');
    ColumnGridColumnCaptions.Add('AI');
    ColumnGridColumnCaptions.Add('Flags');
    ColumnGridColumnCaptions.Add('Default Value');
    ColumnGridColumnCaptions.Add('Comments');
  end;

  theStrings:=TStringList.Create;
  try
    //-------------------------------------
    //Translate PageControlTreeView Items
    DMMain.GetFormResourceStrings(self, 'PageControlTreeView', theStrings);

    for i:=0 to PageControlTreeView.Items.Count-1 do
      if(i<theStrings.Count)then
        PageControlTreeView.Items[i].Text:=theStrings[i];

    //-------------------------------------
    //Translate TableTypeCBox Items
    DMMain.GetFormResourceStrings(self, 'TableTypeCBox', theStrings);

    for i:=0 to TableTypeCBox.Items.Count-1 do
      if(i<theStrings.Count)then
        TableTypeCBox.Items[i]:=theStrings[i];
  finally
    theStrings.Free;
  end;

  //Translate PageControlTreeView Items
  PageControlTreeView.Items[0].Text:=DMMain.GetTranslatedMessage('Indices', 257);
  PageControlTreeView.Items[1].Text:=DMMain.GetTranslatedMessage('Table Options', 258);
  PageControlTreeView.Items[2].Text:=DMMain.GetTranslatedMessage('Advanced', 259);
  PageControlTreeView.Items[3].Text:=DMMain.GetTranslatedMessage('Standard Inserts', 260);
  PageControlTreeView.Items[4].Text:=DMMain.GetTranslatedMessage('Comments', 261);
end;


procedure TEditorTableForm.SetTable(theTable: TEERTable);
var i: integer;
begin
  SourceEERTable:=theTable;
  EERModel:=theTable.ParentEERModel;

  if(Assigned(EERTable))then
    EERTable.Free;
  EERTable:=TEERTable.Create(self, SourceEERTable.ObjName,
    EERModel.DefModelFont, EERModel.DefaultTableType,
    EERModel.DefaultTablePrefix, nil);

  //copy to temporary EERTable
  EERTable.Assign(SourceEERTable);
  //EERTable.Parent:=EERModel;

  EditorTableForm.TableNameEd.Text:=EERTable.ObjName;

  TablePrefixComboBox.Items.Assign(EERModel.TablePrefix);
  TablePrefixComboBox.ItemIndex:=EERModel.DefaultTablePrefix;

  PageControlTreeView.Selected:=PageControlTreeView.Items[EERModel.LastTableEditorPage];
  TablePageControl.ActivePageIndex:=EERModel.LastTableEditorPage;

  ColumnGrid.RowCount:=EERTable.Columns.Count+2;

  if(EERTable.Columns.Count=0)then
    TableNameEd.SelectAll;

  if(EERTable.Indices.Count>0)then
    ShowIndex(TEERIndex(EERTable.Indices[0]).obj_id);

  TablePrefixComboBox.ItemIndex:=EERTable.TablePrefix;
  TableTypeCBox.ItemIndex:=EERTable.TableType;

  nmTableCBox.Checked:=EERTable.GetnmTableStatus;

  TemporaryCBox.Checked:=EERTable.Temporary;

  StdInsertMemo.Text:=EERTable.StandardInserts.Text;

  //---------------------------------------------
  //TableOptions
  NextAutoIncEd.Text:=EERTable.TableOptions.Values['NextAutoIncVal'];
  TblPasswordEd.Text:=EERTable.TableOptions.Values['TblPassword'];
  DelayKeyTblUpdatesCBox.Checked:=(EERTable.TableOptions.Values['DelayKeyTblUpdates']='1');
  PackKeysCBox.Checked:=(EERTable.TableOptions.Values['PackKeys']='1');
  //  RowSettings
  AverageRowLengthEd.Text:=EERTable.TableOptions.Values['AverageRowLength'];
  RowChecksumCBox.Checked:=(EERTable.TableOptions.Values['RowChecksum']='1');
  MinRowNumberEd.Text:=EERTable.TableOptions.Values['MinRowNumber'];
  MaxRowNumberEd.Text:=EERTable.TableOptions.Values['MaxRowNumber'];
  if(EERTable.TableOptions.Values['RowFormat']<>'')then
    RowFormatLU.ItemIndex:=StrToInt(EERTable.TableOptions.Values['RowFormat']);

  TblDataDirEd.Text:=EERTable.TableOptions.Values['TblDataDir'];
  TblIndexDirEd.Text:=EERTable.TableOptions.Values['TblIndexDir'];
  //  Raid
  UseRaidCBox.Checked:=(EERTable.TableOptions.Values['UseRaid']='1');
  if(EERTable.TableOptions.Values['RaidType']<>'')then
    RaidTypeLU.ItemIndex:=StrToInt(EERTable.TableOptions.Values['RaidType']);

  ChunksEd.Text:=EERTable.TableOptions.Values['Chunks'];
  ChunkSizeEd.Text:=EERTable.TableOptions.Values['ChunkSize'];


  //---------------------------------------------
  //Comments
  CommentsMemo.Text:=EERTable.Comments;


  //Store previous tablename
  if(EERTable.PrevTableName='')then
    EERTable.PrevTableName:=EERTable.ObjName;


  //Store previous colname for DB-Sync
  for i:=0 to EERTable.Columns.Count-1 do
    if(TEERColumn(EERTable.Columns[i]).PrevColName='')then
      TEERColumn(EERTable.Columns[i]).PrevColName:=TEERColumn(EERTable.Columns[i]).ColName;

  if(EERModel.ReadOnly)or
    (EERTable.IsLinkedObject)then
  begin
    SubmitBtn.Visible:=False;
  end;
end;

procedure TEditorTableForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if(Not(DiscardChanges))and
    (Not(EERModel.ReadOnly))and
    (Not(EERTable.IsLinkedObject))then
    ApplyChanges;

  EERModel.LastTableEditorPage:=PageControlTreeView.Selected.AbsoluteIndex;
  DMEER.TableSplitterPos:=OptionsPnl.Height;

  Screen.Cursor:=crDefault;

  Action:=caFree;

  Application.MainForm.SetFocus;
end;

procedure TEditorTableForm.ApplyChanges;
var i, j: integer;
  s: string;
  doLogAction,
  UpdateFKColumnNames: Boolean;
  originalTableName: string;
  theTbl: TEERTable;
  theRel: TEERRel;
begin
  if(DoCellEdit)then
  begin
    if(EditorTableFieldEdit.Visible)then
      TEditorTableFieldEdit(EditorTableFieldEdit).ApplyChanges(-1);
    if(EditorTableFieldDatatypeInplaceEditor.Visible)then
      EditorTableFieldDatatypeInplaceEditor.ApplyChanges(-1);
  end;

  EERTable.PrimaryColumnsFirst;

  //Update Table Name
  if(ActiveControl=TableNameEd)then
    TableNameEdExit(self)
  else
    EERTable.ObjName:=TableNameEd.Text;

  EERTable.TableType:=TableTypeCBox.ItemIndex;
  EERTable.TablePrefix:=TablePrefixComboBox.ItemIndex;

  EERTable.SetnmTableStatus(nmTableCBox.Checked);

  EERTable.Temporary:=TemporaryCBox.Checked;

  //Remove returns from the end of the stdinserts
  s:=StdInsertMemo.Text;
  while(Copy(s, Length(s), 1)=#10)or(Copy(s, Length(s), 1)=#13)do
    s:=Copy(s, 1, Length(s)-1);

  EERTable.StandardInserts.Text:=s+#13#10;


  //---------------------------------------------
  //TableOptions
  try
    if(NextAutoIncEd.Text<>'')then
      EERTable.TableOptions.Values['NextAutoIncVal']:=IntToStr(StrToIntDef(NextAutoIncEd.Text, 0))
    else
      EERTable.TableOptions.Values['NextAutoIncVal']:='';
  except
  end;
  EERTable.TableOptions.Values['TblPassword']:=TblPasswordEd.Text;
  EERTable.TableOptions.Values['DelayKeyTblUpdates']:=IntToStr(Ord(DelayKeyTblUpdatesCBox.Checked));
  EERTable.TableOptions.Values['PackKeys']:=IntToStr(Ord(PackKeysCBox.Checked));

  //  RowSettings
  try
    if(AverageRowLengthEd.Text<>'')then
      EERTable.TableOptions.Values['AverageRowLength']:=IntToStr(StrToIntDef(AverageRowLengthEd.Text, 0))
    else
      EERTable.TableOptions.Values['AverageRowLength']:='';
  except
  end;
  EERTable.TableOptions.Values['RowChecksum']:=IntToStr(Ord(RowChecksumCBox.Checked));
  try
    if(MinRowNumberEd.Text<>'')then
      EERTable.TableOptions.Values['MinRowNumber']:=IntToStr(StrToIntDef(MinRowNumberEd.Text, 0))
    else
      EERTable.TableOptions.Values['MinRowNumber']:='';
  except
  end;
  try
    if(MaxRowNumberEd.Text<>'')then
      EERTable.TableOptions.Values['MaxRowNumber']:=IntToStr(StrToIntDef(MaxRowNumberEd.Text, 0))
    else
      EERTable.TableOptions.Values['MaxRowNumber']:='';
  except
  end;

  EERTable.TableOptions.Values['RowFormat']:=IntToStr(RowFormatLU.ItemIndex);

  EERTable.TableOptions.Values['TblDataDir']:=TblDataDirEd.Text;
  EERTable.TableOptions.Values['TblIndexDir']:=TblIndexDirEd.Text;
  //  Raid
  EERTable.TableOptions.Values['UseRaid']:=IntToStr(Ord(UseRaidCBox.Checked));
  EERTable.TableOptions.Values['RaidType']:=IntToStr(RaidTypeLU.ItemIndex);

  try
    if(ChunksEd.Text<>'')then
      EERTable.TableOptions.Values['Chunks']:=IntToStr(StrToIntDef(ChunksEd.Text, 0))
    else
      EERTable.TableOptions.Values['Chunks']:='';
  except
    EERTable.TableOptions.Values['Chunks']:='2';
  end;

  try
    if(ChunkSizeEd.Text<>'')then
      EERTable.TableOptions.Values['ChunkSize']:=IntToStr(StrToIntDef(ChunkSizeEd.Text, 0))
    else
      EERTable.TableOptions.Values['ChunkSize']:='';
  except
    EERTable.TableOptions.Values['ChunkSize']:='64';
  end;


  EERTable.Comments:=CommentsMemo.Text;

  //Store previous tablename for DB-Sync only after change
  if(EERTable.PrevTableName=EERTable.ObjName)then
  begin
    EERTable.PrevTableName:='';
    UpdateFKColumnNames:=False;
  end
  else
  begin
    UpdateFKColumnNames:=True;
    originalTableName:=SourceEERTable.ObjName;
  end;

  //Store previous colname for DB-Sync only after change
  for i:=0 to EERTable.Columns.Count-1 do
    if(TEERColumn(EERTable.Columns[i]).PrevColName=TEERColumn(EERTable.Columns[i]).ColName)then
      TEERColumn(EERTable.Columns[i]).PrevColName:='';



  //-------------------------------------------

  try
    //Get Obj XML for undo
    UndoXML:=SourceEERTable.GetObjAsXMLModel;

    //Log action only when user has changed the object
    if(SourceEERTable.ObjIsEqualTo(EERTable))then
      doLogAction:=False
    else
      doLogAction:=True;

    //Assigned changed table to original source table
    SourceEERTable.Assign(EERTable);
    //EERTable.Parent:=self;


    //-------------------------------------------
    //Do Refresh Stuff

    //Update FK Cols after table rename
    if(UpdateFKColumnNames)then
      for i:=0 to EERModel.ComponentCount-1 do
        if(EERModel.Components[i].ClassNameIs('TEERTable'))then
        begin
          theTbl:=TEERTable(EERModel.Components[i]);

          for j:=0 to theTbl.Columns.Count-1 do
            if(Copy(TEERColumn(theTbl.Columns[j]).ColName, 1, Length(originalTableName))=
              originalTableName)then
            begin
              TEERColumn(theTbl.Columns[j]).ColName:=SourceEERTable.ObjName+
                Copy(TEERColumn(theTbl.Columns[j]).ColName, Length(originalTableName)+1,
                  Length(TEERColumn(theTbl.Columns[j]).ColName));

              theTbl.RefreshObj;
              
              break;
            end;
        end
        else if(EERModel.Components[i].ClassNameIs('TEERRel'))then
        begin
          theRel:=TEERRel(EERModel.Components[i]);

          for j:=0 to theRel.FKFields.Count-1 do
            if(Copy(theRel.FKFields.ValueFromIndex[j], 1, Length(originalTableName))=
              originalTableName)then
            begin
              theRel.FKFields.ValueFromIndex[j]:=SourceEERTable.ObjName+
                Copy(theRel.FKFields.ValueFromIndex[j], Length(originalTableName)+1,
                  Length(theRel.FKFields.ValueFromIndex[j]));
            end;
        end;

    EERModel.CheckAllRelations;

    SourceEERTable.RefreshStrechedImg:=True;

    SourceEERTable.RefreshObj;
    SourceEERTable.Invalidate;

    DMEER.RefreshPalettes;

    //Log the Action
    if(doLogAction)then
      EERModel.LogAction(at_EditObj,
        SourceEERTable.Obj_id,
        'BeforeEdit='+UndoXML+#13#10+
        'AfterEdit='+SourceEERTable.GetObjAsXMLModel+#13#10);
  except
    on x: Exception do
    begin
      s:=DMMain.GetTranslatedMessage('Changes cannot be applied to object.'+#13#10+
        'The object may have been deleted.', 1);
      //Don't display access violation
      if(Copy(x.Message, 1, 10)<>'Access vio')then
        s:=s+#13#10#13#10+x.Message;

      MessageDlg(s, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TEditorTableForm.ColumnGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var i, xpos: integer;
  theSize: TSize;
  theDatatype: TEERDatatype;
begin
  with ColumnGrid.Canvas do
  begin
    if(ARow>0)then
    begin
      if(gdSelected in State)or(gdFocused in State)then
      begin
        Brush.Color:=$00FFF3E8;
        Pen.Style:=psClear;

        if(gdFocused in State)then
        begin
          Pen.Style:=psSolid;
          Pen.Color:=$00FF8A00;
        end;
      end
      else
      begin
        Pen.Style:=psClear;
        Brush.Color:=clWhite;
      end;

      //Draw Selection
      Brush.Style:=bsSolid;
      Rectangle(Rect.Left, Rect.Top, Rect.Right{-1}, Rect.Bottom-1);

      if(ARow<=EERTable.Columns.Count)then
      begin
        case ACol of
          0:
            //Primary Key or Normal Field Icon
            if(TEERColumn(EERTable.Columns[ARow-1]).PrimaryKey)then
              DatatypesImgList.Draw(ColumnGrid.Canvas,
                Rect.Left+1, Rect.Top+1, 7)
            else if(TEERColumn(EERTable.Columns[ARow-1]).IsForeignKey)then
              DatatypesImgList.Draw(ColumnGrid.Canvas,
                Rect.Left+1, Rect.Top+1, 12)
            else
              DatatypesImgList.Draw(ColumnGrid.Canvas,
                Rect.Left+1, Rect.Top+1, 6);
          1:
            //Column Name
            TextOut(Rect.Left+3, Rect.Top+2, TEERColumn(EERTable.Columns[ARow-1]).ColName);
          2:
            //Datatype Icon
            DatatypesImgList.Draw(ColumnGrid.Canvas,
              Rect.Left+3, Rect.Top+1, EERModel.GetDataTypeGroup(TEERColumn(EERTable.Columns[ARow-1]).idDatatype));
          3:
            //Datatype
            TextOut(Rect.Left+1, Rect.Top+2, EERModel.GetDataTypeName(TEERColumn(EERTable.Columns[ARow-1]).idDatatype)+
              TEERColumn(EERTable.Columns[ARow-1]).DatatypeParams);
          4:
            //Not Null
            if(TEERColumn(EERTable.Columns[ARow-1]).NotNull=True)then
              DatatypesImgList.Draw(ColumnGrid.Canvas,
                Rect.Left+1, Rect.Top+1, 8)
            else
            begin
              Brush.Color:=clWhite;
              FillRect(Rect);
            end;
          5:
            //Auto Inc
            if(TEERColumn(EERTable.Columns[ARow-1]).AutoInc=True)then
              DatatypesImgList.Draw(ColumnGrid.Canvas,
                Rect.Left+1, Rect.Top+1, 8)
            else
            begin
              Brush.Color:=clWhite;
              FillRect(Rect);
            end;
          6:
            //Options
            begin
              xpos:=Rect.Left+3;

              theDatatype:=TEERDatatype(EERModel.GetDataType(TEERColumn(EERTable.Columns[ARow-1]).idDatatype));

              for i:=0 to theDatatype.OptionCount-1 do
              begin
                if(TEERColumn(EERTable.Columns[ARow-1]).OptionSelected[i])then
                  DatatypesImgList.Draw(ColumnGrid.Canvas,
                    xpos, Rect.Top+1, 11)
                else
                  DatatypesImgList.Draw(ColumnGrid.Canvas,
                    xpos, Rect.Top+1, 9);

                TextOut(xpos+16, Rect.Top+2, theDatatype.Options[i]);
                theSize:=TextExtent(theDatatype.Options[i]);

                xpos:=xpos+theSize.cx+16+10;
              end;
            end;
          7:
            //Default value
            TextOut(Rect.Left+3, Rect.Top+2, TEERColumn(EERTable.Columns[ARow-1]).DefaultValue);
          8:
            //Comments
            TextOut(Rect.Left+3, Rect.Top+2, TEERColumn(EERTable.Columns[ARow-1]).Comments);
        end;
      end
      else
        if(ACol=0)then
          DatatypesImgList.Draw(ColumnGrid.Canvas,
            Rect.Left+1, Rect.Top+1, 5);

      //Draw Bottom Line
      Pen.Style:=psSolid;
      Pen.Color:=clGray;
      MoveTo(Rect.Left, Rect.Bottom-1);
      LineTo(Rect.Right-1{$IFDEF LINUX}+1{$ENDIF}, Rect.Bottom-1);
    end
    //Draw Header
    else
    begin
      Brush.Color:=clButton;
      Brush.Style:=bsSolid;
      FillRect(Rect);

      Pen.Color:=clWhite;
      MoveTo(Rect.Left, Rect.Top+1);
      LineTo(Rect.Right, Rect.Top+1);

      Pen.Color:=clGray;
      MoveTo(Rect.Left, Rect.Bottom-2);
      LineTo(Rect.Right-1{$IFDEF LINUX}+1{$ENDIF}, Rect.Bottom-2);
      if(ACol<>0)and(ACol<>2)then
      begin
        MoveTo(Rect.Right-2, Rect.Bottom-2);
        LineTo(Rect.Right-2, Rect.Top+1);
      end;

      Pen.Color:=clShadow;
      MoveTo(Rect.Left, Rect.Bottom-1);
      LineTo(Rect.Right-1{$IFDEF LINUX}+1{$ENDIF}, Rect.Bottom-1);
      if(ACol<>0)and(ACol<>2)then
        LineTo(Rect.Right-1{$IFDEF LINUX}+1{$ENDIF}, Rect.Top);

      if(ColumnGridColumnCaptions.Count=7)then
      begin
        Case ACol of
          1:
            TextOut(Rect.Left+1-18, Rect.Top+2, ColumnGridColumnCaptions[0]);
          3:
            TextOut(Rect.Left+1-18, Rect.Top+2, ColumnGridColumnCaptions[1]);
          4:
            TextOut(Rect.Left+1, Rect.Top+2, ColumnGridColumnCaptions[2]);
          5:
            TextOut(Rect.Left+4, Rect.Top+2, ColumnGridColumnCaptions[3]);
          6:
            TextOut(Rect.Left+3, Rect.Top+2, ColumnGridColumnCaptions[4]);
          7:
            TextOut(Rect.Left+3, Rect.Top+2, ColumnGridColumnCaptions[5]);
          8:
            TextOut(Rect.Left+3, Rect.Top+2, ColumnGridColumnCaptions[6]);
        end;
      end;
    end;
  end;
end;

procedure TEditorTableForm.ColumnGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if(ARow=0)or(ACol=0)or(ACol=2)or(ACol=4)or(ACol=5)or(ACol=6)then
    CanSelect:=False
  else
    CanSelect:=True;
end;

procedure TEditorTableForm.ColumnGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=False;
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

    if(Source=Sender)then
      Accept:=True;
  end;
end;

procedure TEditorTableForm.ColumnGridDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var ACol, ARow: Longint;
  i, j: integer;
  DropedDataType: TEERDatatype;
  theCol: TEERColumn;
begin
  DropedDataType:=nil;

  ColumnGrid.MouseToCell(X, Y, ACol, ARow);

  if(ARow>0)and(ARow<=EERTable.Columns.Count)then
  begin
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
      //If not ParamRequired, assign datatype to all selected
      if(DropedDataType.ParamRequired)or
        (ColumnGrid.Selection.Top=ColumnGrid.Selection.Bottom)then
      begin
        //Assign Datatype to the Column
        TEERColumn(EERTable.Columns[ARow-1]).idDatatype:=
          DropedDataType.id;

        //Clear DatatypeParams
        TEERColumn(EERTable.Columns[ARow-1]).DatatypeParams:='';

        //Get Option Defaults
        for i:=0 to DropedDataType.OptionCount-1 do
          TEERColumn(EERTable.Columns[ARow-1]).OptionSelected[i]:=
            DropedDataType.OptionDefaults[i];
      end
      else
      begin
        for i:=ColumnGrid.Selection.Top-1 to ColumnGrid.Selection.Bottom-1 do
        begin
          if(i>=EERTable.Columns.Count)then
            break;

          theCol:=TEERColumn(EERTable.GetColumnByIndex(i));

          if(theCol=nil)then
            continue;

          if(theCol.IsForeignKey)then
            continue;

          theCol.idDatatype:=DropedDataType.id;
          theCol.DatatypeParams:='';

          for j:=0 to DropedDataType.OptionCount-1 do
            theCol.OptionSelected[j]:=
              DropedDataType.OptionDefaults[j];
        end;
      end;

      ColumnGrid.Col:=3;
      ColumnGrid.Row:=ARow;

      ColumnGrid.Invalidate;

      if(DropedDataType.ParamRequired)then
        EditDatatype;
    end
    else if(Source=Sender)then
    begin
      if(ARow<>DragStartRow)then
      begin
        //if(ssShift in DragShiftState)and(ssCtrl in DragShiftState)then
        if(ssShift in Application.KeyState)and
          (ssCtrl in Application.KeyState)then
        begin
          //when Ctrl+Shift is pressed assign same datatype
          theCol:=TEERColumn(EERTable.Columns[ARow-1]);

          theCol.idDatatype:=TEERColumn(EERTable.Columns[DragStartRow-1]).idDatatype;
          theCol.DatatypeParams:=TEERColumn(EERTable.Columns[DragStartRow-1]).DatatypeParams;
        end
        else
        begin
          //Switch both columns in position
          EERTable.Columns.Move(DragStartRow-1, ARow-1);
        end;

        ColumnGrid.Invalidate;
      end;
    end;
  end;
end;

procedure TEditorTableForm.TableNameEdKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if(Key=Key_Return)then
  begin
    ColumnGrid.SetFocus;
    EditCellStr;
  end;
end;

procedure TEditorTableForm.ColumnGridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if(Key=Key_Tab)or(Key=Key_Right)then
  begin
    if(ColumnGrid.Col=8)and(Key=Key_Right)then
      ColumnGrid.Col:=1
    else if(ColumnGrid.Col=8)and(Key=Key_Tab)then
    begin
      ColumnGrid.Col:=1;
      //Go down one row
      if(ColumnGrid.Row<ColumnGrid.RowCount-1)then
        ColumnGrid.Row:=ColumnGrid.Row+1;

      //When in last row, cursor one down and edit, to create new
      if(ColumnGrid.Row=ColumnGrid.RowCount-1)then
        Key:=Key_Return;
    end
    else if(ColumnGrid.Col=3)then
    begin
      ColumnGrid.Col:=7;
      Key:=0;
    end
    else if(ColumnGrid.Col=1)then
      ColumnGrid.Col:=3;
  end;

  if(Key=Key_Left)then
  begin
    if(ColumnGrid.Col=3)then
      ColumnGrid.Col:=1
    else if(ColumnGrid.Col=7)then
      ColumnGrid.Col:=3
    else if(ColumnGrid.Col=1)then
    begin
      ColumnGrid.Col:=8;
      Key:=0;
    end;
  end;

  //Use StringEditor
  if(Key=Key_Return)or(Key=Key_Enter)then
  begin
    if(ColumnGrid.Col=1)or(ColumnGrid.Col=7)or(ColumnGrid.Col=8)then
      EditCellStr;

    if(ColumnGrid.Col=3)then
      EditDatatype;
  end;

  if((Not(DoCellEdit))and
    ((Key>=Ord('a'))and(Key<=Ord('z')))or
    ((Key>=Ord('A'))and(Key<=Ord('Z'))))then
  begin
    if(ColumnGrid.Col=1)or(ColumnGrid.Col=7)or(ColumnGrid.Col=8)then
      if(Shift=[ssShift])then
        EditCellStr(Chr(Key))
      else
        EditCellStr(Chr(Key-Ord('A')+Ord('a')));
  end;

  if(Key=Key_Delete)then
  begin
    DeleteColumnMIClick(self);
  end;

  if(Key=Key_Insert)then
  begin
    InsertColumn;
  end;
end;

procedure TEditorTableForm.InsertColumn;
var theColumn: TEERColumn;
  i: integer;
begin
  //Insert new column
  theColumn:=TEERColumn.Create(EERTable);
  try
    theColumn.ColName:='';
    theColumn.PrevColName:='';
    theColumn.Obj_id:=DMMain.GetNextGlobalID;
    theColumn.Pos:=ColumnGrid.Row-1;
    theColumn.idDatatype:=TEERModel(EERModel).DefaultDataType;
    theColumn.DatatypeParams:='';
    theColumn.Width:=-1;
    theColumn.Prec:=-1;
    theColumn.PrimaryKey:=(ColumnGrid.Row=0);
    theColumn.NotNull:=(ColumnGrid.Row=0);
    theColumn.AutoInc:=False;
    theColumn.IsForeignKey:=False;

    if(TEERModel(EERModel).GetDataType(theColumn.idDatatype)=nil)then
      raise EInOutError.Create('The Datatype of the column cannot be found. '#13#10+
        'idDatatype: '+IntToStr(theColumn.idDatatype));

    //Get Option Defaults
    for i:=0 to TEERDatatype(TEERModel(EERModel).GetDataType(theColumn.idDatatype)).OptionCount-1 do
      theColumn.OptionSelected[i]:=
        TEERDatatype(TEERModel(EERModel).GetDataType(theColumn.idDatatype)).OptionDefaults[i];

    theColumn.DefaultValue:='';

    EERTable.Columns.Insert(ColumnGrid.Row-1, theColumn);

    //Add an empty row an go down there
    ColumnGrid.RowCount:=ColumnGrid.RowCount+1;
  except
    theColumn.Free;
  end;

  ColumnGrid.Refresh;

  EditCellStr('');
end;

procedure TEditorTableForm.EditDatatype;
begin
  if(ColumnGrid.Row<=EERTable.Columns.Count)then
  begin
    //User must not edit datatype of FK Column
    if(Not(TEERColumn(EERTable.Columns[ColumnGrid.Row-1]).IsForeignKey))or
      (Not(DMEER.SyncDatatypesOfForeignKeys))then
    begin

      DoCellEdit:=False;

      EditorTableFieldDatatypeInplaceEditor.SetData(EERModel, self,
        TEERColumn(EERTable.Columns[ColumnGrid.Row-1]),
        EERModel.GetDataType(TEERColumn(EERTable.Columns[ColumnGrid.Row-1]).idDatatype),
        TEERColumn(EERTable.Columns[ColumnGrid.Row-1]).DatatypeParams,
        MidLeftPnl.Width+ColumnGrid.ColWidths[0]+ColumnGrid.ColWidths[1]+ColumnGrid.ColWidths[2]+1,
        MidPnl.Top+19*(ColumnGrid.Row-ColumnGrid.TopRow+1)+2);


      DoCellEdit:=True;
    end;
  end;
end;

procedure TEditorTableForm.EditCellStr(NewName: string = '');
var doIt: Boolean;
begin
  DoCellEdit:=False;

  if(ColumnGrid.Row<=EERTable.Columns.Count)or
    ((ColumnGrid.Row>EERTable.Columns.Count)and(ColumnGrid.Col=1))then
  begin
    doIt:=True;

    //User must not edit name of FK Column
    if(ColumnGrid.Row<=EERTable.Columns.Count)and(ColumnGrid.Col=1)then
      if(TEERColumn(EERTable.Columns[ColumnGrid.Row-1]).IsForeignKey)then
        doIt:=False;

    if(doIt)then
    begin
      TEditorTableFieldEdit(EditorTableFieldEdit).SetData(self, ColumnGrid.Col, ColumnGrid.Row-1, NewName);
      EditorTableFieldEdit.Show;
      EditorTableFieldEdit.BringToFront;
      EditorTableFieldEdit.SetFocus;
      if(NewName<>'')then
      begin
        EditorTableFieldEdit.SelStart:=Length(NewName);
        EditorTableFieldEdit.SelLength:=0;
      end;

      DoCellEdit:=True;
    end;
  end;
end;

procedure TEditorTableForm.ColumnGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ACol, ARow: Longint;
  theRect: TGridRect;
  tmpCol: integer;
begin
  ColumnGrid.MouseToCell(X, Y, ACol, ARow);

  if(DoCellEdit=False)and(Button=mbLeft)then
    TEditorTableFieldEdit(EditorTableFieldEdit).ApplyChanges;

  {if(ACol=3)then
  begin
    EditDatatype;
    Exit;
  end;}

  //Activate Windows like edit of cell
  if(Button=mbLeft)then
  begin
    if(MouseEditTmr.Enabled=True)and(DragStartRow=ARow)and(LastClickedCol=ACol)then
    begin
      DoCellEdit:=True;
    end
    else
    begin
      if(Not(ssShift in Shift))then
      begin
        tmpCol:=ACol;
        if(tmpCol=0)or(tmpCol=2)or(tmpCol=4)or(tmpCol=5)or(tmpCol=6)then
          tmpCol:=1;

        theRect.Left := tmpCol;
        theRect.Top := ARow;
        theRect.Right := tmpCol;
        theRect.Bottom := ARow;

        ColumnGrid.Selection:=theRect;
      end;

      DragStartRow:=ARow;
      LastClickedCol:=ACol;
      //DragShiftState:=Shift;
      ColumnGrid.BeginDrag(False, 5);

      ColumnGrid.Options:=ColumnGrid.Options-[goRangeSelect];

      //Activate Windows like edit of cell > DISABLED
      //MouseEditTmr.Enabled:=True;
    end;
  end;
end;


procedure TEditorTableForm.TableNameEdExit(Sender: TObject);
var i, j: integer;
begin
  if(EERTable.ObjName<>TableNameEd.Text)and
    (TableNameEd.Text<>'')then
  begin
    //Check Reserved Words
    if(EERModel.CheckReservedWord(TableNameEd.Text))then
      TableNameEd.Text:=TableNameEd.Text+'_2';

    //Check other Tables for same name
    j:=0;
    while(j=0)do
    begin
      j:=1;
      for i:=0 to EERModel.ComponentCount-1 do
        if(EERModel.Components[i].ClassnameIs('TEERTable'))then
        begin
          if(TEERTable(EERModel.Components[i]).ObjName=TableNameEd.Text)and
            (TEERTable(EERModel.Components[i])<>EERTable)then
          begin
            try
              TableNameEd.Text:=LeftStr(TableNameEd.Text, Length(TableNameEd.Text)-1)+IntToStr(StrToInt(RightStr(TableNameEd.Text, 1))+1)
            except
              TableNameEd.Text:=TableNameEd.Text+'_2';
            end;

            j:=0;
            break;
          end;
        end;
    end;

    EERTable.ObjName:=TableNameEd.Text;
    EERTable.Invalidate;
  end;
end;

procedure TEditorTableForm.TablePageCBoxCloseUp(Sender: TObject);
begin
  //TablePageControl.ActivePageIndex:=TablePageCBox.ItemIndex;
end;

procedure TEditorTableForm.ColumnGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ACol, ARow: Longint;
  i, xpos, xpos2: integer;
  theRect: TRect;
  theSize: TSize;
  theDatatype: TEERDatatype;
  selRect: TGridRect;
  SetAutoInc: Boolean;
begin
  if(Not(goRangeSelect in ColumnGrid.Options))then
    ColumnGrid.Options:=ColumnGrid.Options+[goRangeSelect];

  ColumnGrid.MouseToCell(X, Y, ACol, ARow);

  if(ARow>0)and(ARow<=EERTable.Columns.Count)and(Button=mbLeft)then
  begin
    //Only select Colnames (except when cursor is over DefVal)
    if(ACol<>3)and(ACol<>7)and(ACol<>8)then
    begin
      selRect.Left := 1;
      selRect.Right := 1;

      //When Shift is not pressed, select only on row
      if(not(ssShift in Shift))then
      begin
        selRect.Top := ARow;
        selRect.Bottom := ARow;
      end
      else
      begin
        selRect.Top := ColumnGrid.Selection.Top;
        selRect.Bottom := ColumnGrid.Selection.Bottom;
      end;

      ColumnGrid.Selection:=selRect;
    end;

    //Primary Key
    if(ACol=0)then
    begin
      //Not with FK Column
      if(Not(TEERColumn(EERTable.Columns[ARow-1]).IsForeignKey))then
      begin
        TEERColumn(EERTable.Columns[ARow-1]).PrimaryKey:=
          Not(TEERColumn(EERTable.Columns[ARow-1]).PrimaryKey);

        ColumnGrid.Invalidate;

        CheckPrimaryIndex;
      end;
    end;

    //NotNull
    if(ACol=4)then
    begin
      if(TEERColumn(EERTable.Columns[ARow-1]).PrimaryKey)then
        MessageDlg(DMMain.GetTranslatedMessage('Columns of a primary key must not be NULL.', 95),
          mtError, [mbOK], 0)
      else
        TEERColumn(EERTable.Columns[ARow-1]).NotNull:=
          Not(TEERColumn(EERTable.Columns[ARow-1]).NotNull);

      ColumnGrid.Invalidate;
    end;

    //Auto Inc
    if(ACol=5)then
    begin
      //There can only be one AutoInc column
      SetAutoInc:=Not(TEERColumn(EERTable.Columns[ARow-1]).AutoInc);

      for i:=0 to EERTable.Columns.Count-1 do
        TEERColumn(EERTable.Columns[i]).AutoInc:=False;

      TEERColumn(EERTable.Columns[ARow-1]).AutoInc:=SetAutoInc;

      ColumnGrid.Invalidate;
    end;

    //Options
    if(ACol=6)then
    begin
      //Not with FK Column
      if(Not(TEERColumn(EERTable.Columns[ARow-1]).IsForeignKey))then
      begin
        theDatatype:=TEERDatatype(EERModel.GetDataType(TEERColumn(EERTable.Columns[ARow-1]).idDatatype));
        theRect:=ColumnGrid.CellRect(ACol, ARow);
        xpos:=0;

        for i:=0 to theDatatype.OptionCount-1 do
        begin
          theSize:=ColumnGrid.Canvas.TextExtent(theDatatype.Options[i]);
          xpos2:=xpos;
          xpos:=xpos+theSize.cx+16+5;

          if(X-theRect.Left>=xpos2)and
            (X-theRect.Left<xpos)then
            TEERColumn(EERTable.Columns[ARow-1]).OptionSelected[i]:=
              Not(TEERColumn(EERTable.Columns[ARow-1]).OptionSelected[i]);
        end;

        ColumnGrid.Invalidate;
      end;
    end;
  end;
end;

procedure TEditorTableForm.DeleteColumnMIClick(Sender: TObject);
var theCol: TEERColumn;
 i: integer;
begin
  for i:=ColumnGrid.Selection.Top to ColumnGrid.Selection.Bottom do
  begin
    //Always take the first selected column, not i
    //because each DeleteColumn will remove one column from
    //the list
    if(ColumnGrid.Selection.Top-1>=EERTable.Columns.Count)then
      break;

    theCol:=TEERColumn(EERTable.GetColumnByIndex(ColumnGrid.Selection.Top-1));

    if(theCol=nil)then
      continue;

    if(theCol.IsForeignKey)then
      if(MessageDlg(DMMain.GetTranslatedMessage('%s is a Foreign Key Column and '+
        'was created automatically by a Relation. When this Editor is closed '+
        'the Column will be added again.'+#13#10#13#10+
        'To delete this Column permanently you have to remove the appropriate '+
        'Relation.'+#13#10#13#10+
        'Are you shure you want to delete the Column?', 2, theCol.ColName), mtConfirmation,
        [mbYes, mbNo], 0)<>mrYes)then
        continue;

    EERTable.DeleteColumn(ColumnGrid.Selection.Top-1);
  end;

  ColumnGrid.RowCount:=EERTable.Columns.Count+2;

  ColumnGrid.Repaint;

  ShowIndex(EERTable.CheckPrimaryIndex);
end;

procedure TEditorTableForm.NewIndexBtnClick(Sender: TObject);
var theName: string;
  theIndex: TEERIndex;
  theID: integer;
begin
  theID:=DMMain.GetNextGlobalID;
  theName:=EERTable.ObjName+'_index'+IntToStr(theID);
  if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Please enter the Index Name:', 96),
    DMMain.GetTranslatedMessage('Name of Index:', 97),
    theName, Length(EERTable.ObjName)+1))then
  begin
    //new(theIndex);
    theIndex:=TEERIndex.Create(EERTable);

    theIndex.Obj_id:=theID;
    theIndex.IndexName:=theName;
    theIndex.IndexKind:=ik_INDEX;
    EERTable.Indices.Add(theIndex);
    theIndex.Pos:=EERTable.Indices.Count-1;

    ShowIndex(theIndex.Obj_id);
  end;
end;

procedure TEditorTableForm.CheckPrimaryIndex;
begin
  ShowIndex(EERTable.CheckPrimaryIndex);
end;

procedure TEditorTableForm.ShowIndex(index_id: integer);
var i, j: integer;
  s: string;
  theEERColumn: TEERColumn;
begin
  //Reset Fields
  IndexListBox.Items.Clear;
  IndexNameEd.Text:='';
  IndexTypeCBox.ItemIndex:=-1;
  IndexColListBox.Clear;

  //Display the Index
  for i:=0 to EERTable.Indices.Count-1 do
  begin
    IndexListBox.Items.Add(TEERIndex(EERTable.Indices[i]).IndexName);

    if(TEERIndex(EERTable.Indices[i]).Obj_id=index_id)then
    begin
      IndexListBox.ItemIndex:=i;

      IndexNameEd.Text:=TEERIndex(EERTable.Indices[i]).IndexName;
      IndexTypeCBox.ItemIndex:=TEERIndex(EERTable.Indices[i]).IndexKind;

      j:=0;
      while(j<TEERIndex(EERTable.Indices[i]).Columns.Count)do
      begin
        s:=TEERIndex(EERTable.Indices[i]).ColumnParams.Values[TEERIndex(EERTable.Indices[i]).Columns[j]];

        //Check if column still exists
        theEERColumn:=EERTable.GetColumnByID(StrToInt(TEERIndex(EERTable.Indices[i]).Columns[j]));
        if(theEERColumn<>nil)then
        begin
          if(s<>'')then
            s:=TEERColumn(theEERColumn).ColName+'('+s+')'
          else
            s:=TEERColumn(theEERColumn).ColName;

          IndexColListBox.Items.Add(s);

          inc(j);
        end
        //if not, delete it
        else
          TEERIndex(EERTable.Indices[i]).Columns.Delete(j);
      end;

      if(CompareText(TEERIndex(EERTable.Indices[i]).IndexName, 'PRIMARY')=0)then
      begin
        IndexNameEd.Enabled:=False;
        IndexTypeCBox.Enabled:=False;
      end
      else
      begin
        IndexNameEd.Enabled:=True;
        IndexTypeCBox.Enabled:=True;
      end;

      if(TEERIndex(EERTable.Indices[i]).FKRefDef_Obj_id>-1)then
      begin
        IndexColListBox.Enabled:=False;
        IndexColListBox.Color:=clBackground;
      end
      else
      begin
        IndexColListBox.Enabled:=True;
        IndexColListBox.Color:=clBase;
      end;
    end;
  end;

  if(index_id=-1)and(IndexTypeCBox.Items.Count>0)then
    IndexTypeCBox.ItemIndex:=0;

  if(IndexColListBox.Items.Count>0)then
    IndexColListBox.ItemIndex:=0;
end;


procedure TEditorTableForm.IndexColListBoxDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=False;

  if(Source<>nil)then
  begin
    if(Source.ClassNameIs('TDrawGrid'))then
      if(TDrawGrid(Source).Name='ColumnGrid')then
        Accept:=True;

    if(Sender=Source)then
      Accept:=True;
  end;
end;

procedure TEditorTableForm.IndexColListBoxDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var theIndex: TEERIndex;
  newPos: integer;
  s: string;
begin
  if(Source.ClassNameIs('TDrawGrid'))then
    if(TDrawGrid(Source).Name='ColumnGrid')and
      (IndexListBox.ItemIndex>-1)and
      (IndexListBox.ItemIndex<=EERTable.Indices.Count-1)then
    begin
      theIndex:=TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]);

      //Every Column only once in index
      if(ColumnGrid.Row>=0)and
        (ColumnGrid.Row-1<EERTable.Columns.Count)then
      begin
        if(theIndex.Columns.IndexOf(IntToStr(TEERColumn(EERTable.Columns[ColumnGrid.Row-1]).Obj_id))=-1)then
        begin
          theIndex.Columns.Add(IntToStr(TEERColumn(EERTable.Columns[ColumnGrid.Row-1]).Obj_id));

          if(CompareText(theIndex.IndexName, 'PRIMARY')=0)then
          begin
            TEERColumn(EERTable.Columns[ColumnGrid.Row-1]).PrimaryKey:=True;

            ColumnGrid.Invalidate;
          end;

          ShowIndex(theIndex.Obj_id);
        end;
      end;
    end;
  if(Source.ClassNameIs('TListBox'))then
    if(TDrawGrid(Source).Name='IndexColListBox')then
    begin
      with IndexColListBox do
      begin
        newPos:=ItemAtPos(Point(X, Y), True);

        if(newPos>-1)and(newPos<>DragStartRow)then
        begin
          s:=Items[DragStartRow];

          Items.Delete(DragStartRow);
          Items.Insert(newPos, s);

          s:=TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]).Columns[DragStartRow];
          TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]).Columns.Delete(DragStartRow);
          TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]).Columns.Insert(newPos, s);
        end;
      end;
    end;
end;

procedure TEditorTableForm.DeleteColFromIndexBtnClick(Sender: TObject);
var theIndex: TEERIndex;
begin
  if(IndexColListBox.ItemIndex>-1)and(IndexColListBox.Enabled)then
  begin
    theIndex:=TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]);

    if(CompareText(theIndex.IndexName, 'PRIMARY')=0)then
    begin
      TEERColumn(EERTable.GetColumnByID(StrToInt(theIndex.Columns[IndexColListBox.ItemIndex]))).PrimaryKey:=False;

      ColumnGrid.Invalidate;
    end;

    theIndex.Columns.Delete(theIndex.Columns.IndexOf(theIndex.Columns[IndexColListBox.ItemIndex]));

    ShowIndex(theIndex.Obj_id);
  end;
end;

procedure TEditorTableForm.RefreshCurrentIndex;
begin
  if(IndexListBox.ItemIndex>-1)then
    ShowIndex(TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]).Obj_id);
end;

procedure TEditorTableForm.IndexListBoxClick(Sender: TObject);
begin
  RefreshCurrentIndex;
end;

procedure TEditorTableForm.IndexListBoxDblClick(Sender: TObject);
var theName: string;
begin
  if(IndexListBox.ItemIndex>-1)then
  begin
    theName:=IndexListBox.Items[IndexListBox.ItemIndex];

    if(CompareText(theName, 'PRIMARY')<>0)then
    begin
      if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Please enter the Index Name:', 3), DMMain.GetTranslatedMessage('Name of Index:', 4),
        theName))then
      begin
        TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]).IndexName:=theName;

        RefreshCurrentIndex;
      end;
    end;
  end;
end;

procedure TEditorTableForm.IndexTypeCBoxCloseUp(Sender: TObject);
begin
  if(IndexTypeCBox.ItemIndex>0)then
    TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]).IndexKind:=
      IndexTypeCBox.ItemIndex;

  if(IndexTypeCBox.ItemIndex=0)then
    IndexTypeCBox.ItemIndex:=
      TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]).IndexKind;
end;

procedure TEditorTableForm.IndexColListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if(Button=mbLeft)then
  begin
    DragStartRow:=IndexColListBox.ItemAtPos(Point(X, Y), True);
    if(DragStartRow>-1)then
      IndexColListBox.BeginDrag(False, 5);
  end
  else if(Button=mbRight)then
  begin
    IndexColumnsPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;

end;

procedure TEditorTableForm.DelIndexBtnClick(Sender: TObject);
var //theIndex: TEERIndex;
  i: integer;
  doIt: Boolean;
begin
  if(IndexListBox.ItemIndex>-1)then
  begin
    if(MessageDlg(DMMain.GetTranslatedMessage('Do you really want to delete the selected index?', 5),
      mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
    begin
      if(CompareText(IndexListBox.Items[IndexListBox.ItemIndex], 'PRIMARY')<>0)then
      begin
        //ask again if the index is a FKRefDefIndex
        if(TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]).FKRefDef_Obj_id>-1)then
        begin
          doIt:=False;

          if(MessageDlg(DMMain.GetTranslatedMessage('The selected Index was created automatically '+
            'and will be recreated after this editor is closed if the '+
            '[Automatically create Index on FK Fields] option is enabled. '+#13#10#13#10+
            'To disable automatical Index creation please check the model''s '+
            'edit options.'+#13#10#13#10+
            'Are you shure you want to delete the selected index?', 6),
            mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
          begin
            //Check all relations and set FKRefDefIndex_Obj_id=-1
            for i:=0 to EERModel.ComponentCount-1 do
              if(EERModel.Components[i].ClassNameIs('TEERRel'))then
                if(TEERRel(EERModel.Components[i]).Obj_id=
                  TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]).FKRefDef_Obj_id)then
                begin
                  TEERRel(EERModel.Components[i]).FKRefDefIndex_Obj_id:=-1;
                  doIt:=True;
                  break;
                end;
          end;
        end
        else
          doIt:=True;

        if(doIt)then
        begin
          EERTable.Indices.Delete(IndexListBox.ItemIndex);

          if(EERTable.Indices.Count>0)then
            ShowIndex(TEERIndex(EERTable.Indices[EERTable.Indices.Count-1]).Obj_id)
          else
            ShowIndex(-1);
        end;
      end
      else
      begin
        for i:=0 to EERTable.Columns.Count-1 do
          TEERColumn(EERTable.Columns[i]).PrimaryKey:=False;

        CheckPrimaryIndex;

        ColumnGrid.Invalidate;
      end;
    end;
  end;
end;

procedure TEditorTableForm.ColumnGridDblClick(Sender: TObject);
begin
  if(ColumnGrid.Row=0)then
    Exit;

  //Use StringEditor
  if((ColumnGrid.Col=1)and(LastClickedCol=1))or(ColumnGrid.Col=7)or(ColumnGrid.Col=8)then
    EditCellStr;

  if(ColumnGrid.Col=3)then
    EditDatatype;
end;

procedure TEditorTableForm.PasteSQLInsertMIClick(Sender: TObject);
var s, s2: string;
  i: integer;
begin
  //store Clipboard
  s2:=Clipboard.AsText;

  s:=EERTable.GetSQLInsertCode;

  for i:=0 to EERTable.Columns.Count-1 do
  begin
    if(i<EERTable.Columns.Count-1)then
      s:=s+', ';
  end;

  s:=s+');'+#13#10#13#10;

  StdInsertMemo.SelText:=s;
  StdInsertMemo.SelStart:=StdInsertMemo.SelStart+StdInsertMemo.SelLength;
  StdInsertMemo.SelLength:=0;

  Clipboard.AsText:=s2;
end;

procedure TEditorTableForm.FormShow(Sender: TObject);
begin
  DMMain.RestoreWinPos(self, True);
end;

procedure TEditorTableForm.MouseEditTmrTimer(Sender: TObject);
begin
  {//Simulate delayed double click
  if(DoCellEdit)then
    EditCellStr;

  MouseEditTmr.Enabled:=False;}
end;

procedure TEditorTableForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_F1)then
    DMMain.ShowHelp('editors', 'table');

  if(Key=Key_Escape)then
  begin
    if(ActiveControl=EditorTableFieldEdit)or
      (ActiveControl=EditorTableFieldDatatypeInplaceEditor.DatatypeCBox)then
      SetFocus
    else
      AbortBtnClick(self);
  end;
end;

procedure TEditorTableForm.FormDeactivate(Sender: TObject);
begin
  {if(DoCellEdit)then
  begin
    if(EditorTableFieldEdit.Visible)then
      TEditorTableFieldEdit(EditorTableFieldEdit).ApplyChanges(-1);
    if(EditorTableFieldDatatypeInplaceEditor.Visible)then
      EditorTableFieldDatatypeInplaceEditor.ApplyChanges(-1);
  end;}

  if(Not(DMMain.IsFormStayingOnTop(self)))then
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));
end;

procedure TEditorTableForm.EditIndexColumnLengthMIClick(Sender: TObject);
var theIndex: TEERIndex;
  s: string;
begin
  if(IndexColListBox.ItemIndex>-1)then
  begin
    theIndex:=TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]);

    //Look for ID in the Params
    s:=theIndex.ColumnParams.Values[theIndex.Columns[IndexColListBox.ItemIndex]];

    if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Please enter the Length Parameter:', 7), DMMain.GetTranslatedMessage('Length:', 8), s))then
    begin
      if(s<>'')then
        theIndex.ColumnParams.Values[theIndex.Columns[IndexColListBox.ItemIndex]]:=s
      else
        if(theIndex.ColumnParams.IndexOfName(theIndex.Columns[IndexColListBox.ItemIndex])>=0)then
          theIndex.ColumnParams.Delete(theIndex.ColumnParams.IndexOfName(theIndex.Columns[IndexColListBox.ItemIndex]));
    end;

    ShowIndex(theIndex.Obj_id);
  end;
end;

procedure TEditorTableForm.SubmitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TEditorTableForm.AbortBtnClick(Sender: TObject);
begin
  DiscardChanges:=True;

  Close;
end;

procedure TEditorTableForm.SubmitBtnMouseEnter(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=True;
end;

procedure TEditorTableForm.SubmitBtnMouseLeave(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=False;
end;

procedure TEditorTableForm.PageControlTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  TablePageControl.ActivePageIndex:=PageControlTreeView.Selected.AbsoluteIndex;
  PageControlTitleLbl.Caption:=PageControlTreeView.Selected.Text;
end;

procedure TEditorTableForm.FormResize(Sender: TObject);
begin
  //TableNameEd
  TableNameEd.Width:=Width-(702-164);

  //Column Name
  ColumnGrid.ColWidths[1]:=(Width-702) div 4+104;
  //Column Comments
  ColumnGrid.ColWidths[8]:=(Width-702) div 4*3+100;


  //PageControlTitle
  PageControlTitleShape.Width:=Width-(702-533);
  PageControlTitlePnl.Width:=Width-(702-531);

  //PageControl
  TablePageControl.Width:=Width-(702-541);

  //Index page
  IndexListBox.Width:=Width-(702-121);
  NewIndexBtn.Left:=Width-702+121;
  DelIndexBtn.Left:=Width-702+121;
  IndexGroupbox.Left:=156+Width-702;
  IndexColumnsLbl.Left:=322+Width-702;
  IndexDragHintLbl.Left:=366+Width-702;
end;

procedure TEditorTableForm.ClearAllSQLInsertsMIClick(Sender: TObject);
begin
  StdInsertMemo.Lines.Clear;
end;

procedure TEditorTableForm.OptionSplitterMoved(Sender: TObject);
begin
  PageControlTreeView.Height:=OptionsPnl.Height-(172-155);
  TablePageControl.Height:=OptionsPnl.Height-(172-135);
end;

procedure TEditorTableForm.OptionSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  if(NewSize<172)then
    Accept:=False;
end;

procedure TEditorTableForm.AddColPrefixMIClick(Sender: TObject);
var s: string;
  theCol: TEERColumn;
  i: integer;
  s1, s2: string;
  DoPrefix: Boolean;
begin
  DoPrefix:=True;

  if(Sender.ClassNameIs('TMenuItem'))then
    if(TMenuItem(Sender).Name='AddColPostfixMI')then
      DoPrefix:=False;

  if(DoPrefix)then
  begin
    s1:=DMMain.GetTranslatedMessage('Enter a Prefix for the selected columns ...', 98);
    s2:=DMMain.GetTranslatedMessage('Prefix:', 99);
  end
  else
  begin
    s1:=DMMain.GetTranslatedMessage('Enter a Postfix for the selected columns ...', 100);
    s2:=DMMain.GetTranslatedMessage('Postfix:', 101);
  end;


  if(DMMain.ShowStringEditor(s1, s2, s))then
    if(s<>'')then
    begin
      for i:=ColumnGrid.Selection.Top-1 to ColumnGrid.Selection.Bottom-1 do
      begin
        if(i>=EERTable.Columns.Count)then
          break;

        theCol:=TEERColumn(EERTable.GetColumnByIndex(i));

        if(theCol=nil)then
          continue;

        if(theCol.IsForeignKey)then
          if(MessageDlg(DMMain.GetTranslatedMessage('%s is a Foreign Key Column and '+
            'cannot be edited.', 102, theCol.ColName), mtConfirmation,
            [mbYes, mbNo], 0)<>mrYes)then
            continue;

        if(DoPrefix)then
          theCol.ColName:=s+theCol.ColName
        else
          theCol.ColName:=theCol.ColName+s;
      end;

      //ColumnGrid.RowCount:=EERTable.Columns.Count+2;

      ColumnGrid.Repaint;

      ShowIndex(EERTable.CheckPrimaryIndex);
    end;
end;

procedure TEditorTableForm.AddColumnstoSelectedIndexMIClick(
  Sender: TObject);
var i: integer;
  theCol: TEERColumn;
  theIndex: TEERIndex;
begin
  if(IndexListBox.ItemIndex>-1)and
    (IndexListBox.ItemIndex<=EERTable.Indices.Count-1)then
  begin
    theIndex:=TEERIndex(EERTable.Indices[IndexListBox.ItemIndex]);

    for i:=ColumnGrid.Selection.Top-1 to ColumnGrid.Selection.Bottom-1 do
    begin
      if(i>=EERTable.Columns.Count)then
        break;

      theCol:=TEERColumn(EERTable.GetColumnByIndex(i));

      if(theCol=nil)then
        continue;

      if(theIndex.Columns.IndexOf(IntToStr(theCol.Obj_id))=-1)then
      begin
        theIndex.Columns.Add(IntToStr(theCol.Obj_id));

        if(CompareText(theIndex.IndexName, 'PRIMARY')=0)then
        begin
          theCol.PrimaryKey:=True;

          ColumnGrid.Invalidate;
        end;

        ShowIndex(theIndex.Obj_id);
      end;
    end;
  end;
end;

procedure TEditorTableForm.MoveRowupMIClick(Sender: TObject);
begin
  if(ColumnGrid.Row>1)then
  begin
    EERTable.Columns.Move(ColumnGrid.Row-1, ColumnGrid.Row-2);
    ColumnGrid.Row:=ColumnGrid.Row-1;

    ColumnGrid.Invalidate;
  end;
end;

procedure TEditorTableForm.MoveRowdownMIClick(Sender: TObject);
begin
  if(ColumnGrid.Row<ColumnGrid.RowCount-2)then
  begin
    EERTable.Columns.Move(ColumnGrid.Row-1, ColumnGrid.Row);
    ColumnGrid.Row:=ColumnGrid.Row+1;

    ColumnGrid.Invalidate;
  end;
end;

end.
