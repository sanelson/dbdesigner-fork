unit OptionsModel;

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
// Unit OptionsModel.pas
// ---------------------
// Version 1.5, 08.04.2002, Mike
// Description
//   Contains the model options form class
//
// Changes:
//   Version 1.5, 08.04.2002, Mike
//     added DefaultTableType and ActivateRefDefForNewRelation
//     OnFormClose FontCBox.ItemIndex could be -1 and cause a
//       Listindex out of bounds error, fixed
//   Version 1.4, 04.04.2002, Mike / Shannon Weyrick
//     added TableNameInRefsCBox
//   Version 1.3, 28.03.2002, Mike
//     added EditOptionsSheet, support grid parameters
//   Version 1.2, 13.03.2002, Mike
//     new edit for model's VersionStr
//   Version 1.1, 13.03.2002
//     fixed bug in SetModel & PluginRecordsStringGridClick when model
//     contains no plugindata
//   Version 1.0, 04.12.2002
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, EERModel, QComCtrls, QButtons, QCheckLst, QGrids,
  Qt, EERDM;

type
  TOptionsModelForm = class(TForm)
    PageControl: TPageControl;
    GeneralSheet: TTabSheet;
    Label1: TLabel;
    ModelNameEd: TEdit;
    PluginSheet: TTabSheet;
    Label2: TLabel;
    FontCBox: TComboBox;
    Label8: TLabel;
    PluginRecordsStringGrid: TStringGrid;
    Label9: TLabel;
    PluginParamsStringGrid: TStringGrid;
    DatabaseSheet: TTabSheet;
    Label4: TLabel;
    TablePrefixCLBox: TCheckListBox;
    NewTblPrefixBtn: TSpeedButton;
    DelTblPrefixBtn: TSpeedButton;
    Label5: TLabel;
    Label6: TLabel;
    Label12: TLabel;
    ModelCommentsMemo: TMemo;
    RegionColorsMemo: TMemo;
    Label10: TLabel;
    Label11: TLabel;
    DefDatatypeCBox: TComboBox;
    Label7: TLabel;
    Label3: TLabel;
    DBTypeEd: TEdit;
    VersionEd: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    EditOptionsSheet: TTabSheet;
    GroupBox1: TGroupBox;
    GridWidthLbl: TLabel;
    GridXEd: TEdit;
    GridHeightLbl: TLabel;
    GridYEd: TEdit;
    GridWidthUnitsLbl: TLabel;
    GridHeightUnitsLbl: TLabel;
    UsePosGridCBox: TCheckBox;
    GroupBox2: TGroupBox;
    Label18: TLabel;
    DefaultTableTypeCBox: TComboBox;
    BottomPnl: TPanel;
    AbortBtn: TSpeedButton;
    SubmitBtn: TSpeedButton;
    RemovePluginData: TBitBtn;
    MakeEditSettingsDefaultBtn: TBitBtn;
    GroupBox3: TGroupBox;
    TableNameInRefsCBox: TCheckBox;
    Label23: TLabel;
    FKPrefixEd: TEdit;
    Label24: TLabel;
    FKPostfixEd: TEdit;
    CreateFKRefDefIndexCBox: TCheckBox;
    ActivateRefDefForNewRelationsCBox: TCheckBox;
    Label19: TLabel;
    PageControlTreeView: TTreeView;
    PageControlTitlePnl: TPanel;
    PageControlTitleLbl: TLabel;
    PageControlTitleShape: TShape;
    CreateSQLforLinkedObjectsCBox: TCheckBox;
    Bevel1: TBevel;
    GroupBox4: TGroupBox;
    Label20: TLabel;
    CanvasWidthEd: TEdit;
    Label21: TLabel;
    Label22: TLabel;
    CanvasHeightEd: TEdit;
    Label25: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure SetModel(theModel: TEERModel);
    procedure NewTblPrefixBtnClick(Sender: TObject);
    procedure TablePrefixCLBoxClickCheck(Sender: TObject);
    procedure TablePrefixCLBoxDblClick(Sender: TObject);
    procedure DelTblPrefixBtnClick(Sender: TObject);
    procedure PluginRecordsStringGridClick(Sender: TObject);
    procedure MakeEditSettingsDefaultBtnClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
    procedure AbortBtnClick(Sender: TObject);
    procedure ApplyChanges;
    procedure SubmitBtnMouseEnter(Sender: TObject);
    procedure SubmitBtnMouseLeave(Sender: TObject);
    procedure UsePosGridCBoxClick(Sender: TObject);
    procedure RemovePluginDataClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CanvasWidthEdChange(Sender: TObject);
    procedure CanvasHeightEdChange(Sender: TObject);
  private
    { Private declarations }
    DiscardChanges: Boolean;
  public
    { Public declarations }
    EERModel: TEERModel;
  end;

var
  OptionsModelForm: TOptionsModelForm;

implementation

uses MainDM;

{$R *.xfm}

procedure TOptionsModelForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);

  DiscardChanges:=False;

  //Translate PageControlTreeView Items
  PageControlTreeView.Items[0].Text:=DMMain.GetTranslatedMessage('General Options', 251);
  PageControlTreeView.Items[1].Text:=DMMain.GetTranslatedMessage('Editing Options', 255);
  PageControlTreeView.Items[2].Text:=DMMain.GetTranslatedMessage('Database Options', 253);
  PageControlTreeView.Items[3].Text:=DMMain.GetTranslatedMessage('Plugin Data', 256);

  FontCBox.Items.Assign(Screen.Fonts);

  PageControl.ActivePage:=GeneralSheet;

  PluginRecordsStringGrid.ColWidths[0]:=120;
  PluginRecordsStringGrid.ColWidths[1]:=40;

  PageControlTreeView.FullExpand;
  PageControl.ActivePageIndex:=0;
  PageControlTreeView.Selected:=PageControlTreeView.Items[0];

  PageControlTitleLbl.Font.Style:=[fsBold];
end;

procedure TOptionsModelForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if(Not(DiscardChanges))then
    ApplyChanges;
end;

procedure TOptionsModelForm.ApplyChanges;
var i, newWidth, newHeight: integer;
begin
  if(ModelNameEd.Text<>'')then
    EERModel.SetModelName(ModelNameEd.Text);

  if(VersionEd.Text<>'')then
    EERModel.VersionStr:=VersionEd.Text;

  EERModel.ModelComments:=ModelCommentsMemo.Text;

  EERModel.TablePrefix.Assign(TablePrefixCLBox.Items);

  for i:=0 to TablePrefixCLBox.Items.Count-1 do
    if(TablePrefixCLBox.Checked[i])then
      EERModel.DefaultTablePrefix:=i;

  EERModel.DefaultDataType:=TEERDatatype(EERModel.Datatypes[DefDatatypeCBox.ItemIndex]).id;

  EERModel.RegionColors.Text:=RegionColorsMemo.Text;

  if(FontCBox.ItemIndex>=0)then
    EERModel.DefModelFont:=FontCBox.Items[FontCBox.ItemIndex];
  EERModel.RefreshFont;

  EERModel.UsePositionGrid:=UsePosGridCBox.Checked;
  try
    EERModel.PositionGrid.X:=StrToInt(GridXEd.Text);
    EERModel.PositionGrid.Y:=StrToInt(GridYEd.Text);
  except
    EERModel.PositionGrid.X:=20;
    EERModel.PositionGrid.Y:=20;
  end;

  //Canvas size was changed
  try
    newWidth:=StrToInt(CanvasWidthEd.Text);
    newHeight:=StrToInt(CanvasHeightEd.Text);
  except
    newWidth:=4096;
    newHeight:=2842;
  end;

  if(newWidth<>EERModel.EERModel_Width)or
    (newHeight<>EERModel.EERModel_Height)then
  begin
    EERModel.EERModel_Width:=newWidth;
    EERModel.EERModel_Height:=newHeight;

    EERModel.SetZoomFac(EERModel.GetZoomFac);
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RefreshNavPalette, EERModel));
  end;


  EERModel.TableNameInRefs:=TableNameInRefsCBox.Checked;
  EERModel.DefaultTableType:=DefaultTableTypeCBox.ItemIndex;
  EERModel.ActivateRefDefForNewRelations:=ActivateRefDefForNewRelationsCBox.Checked;

  EERModel.FKPrefix:=FKPrefixEd.Text;
  EERModel.FKPostfix:=FKPostfixEd.Text;

  EERModel.CreateFKRefDefIndex:=CreateFKRefDefIndexCBox.Checked;

  //Refresh Grid Btn
  sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RefreshGridBtn, EERModel));

  EERModel.CreateSQLforLinkedObjects:=CreateSQLforLinkedObjectsCBox.Checked;

  EERModel.CheckAllRelations;

  EERModel.ModelHasChanged;
end;

procedure TOptionsModelForm.SetModel(theModel: TEERModel);
var i: integer;
begin
  EERModel:=theModel;

  ModelNameEd.Text:=EERModel.GetModelName;
  VersionEd.Text:=EERModel.VersionStr;
  ModelCommentsMemo.Text:=EERModel.ModelComments;

  DBTypeEd.Text:=EERModel.DatabaseType;

  TablePrefixCLBox.Items.Assign(EERModel.TablePrefix);
  TablePrefixCLBox.Items[0]:=TablePrefixCLBox.Items[0];
  TablePrefixCLBox.Checked[EERModel.DefaultTablePrefix]:=True;

  DefDatatypeCBox.Items.Clear;
  for i:=0 to EERModel.Datatypes.Count-1 do
    DefDatatypeCBox.Items.Add(TEERDatatype(EERModel.Datatypes[i]).TypeName);

  DefDatatypeCBox.ItemIndex:=EERModel.Datatypes.IndexOf(EERModel.GetDataType(EERModel.DefaultDataType));

  FontCBox.ItemIndex:=FontCBox.Items.IndexOf(EERModel.DefModelFont);


  RegionColorsMemo.Text:=EERModel.RegionColors.Text;


  //At lease 2 Rows (1 fixed an 1 data)
  if(EERModel.PluginData.Count>0)then
    PluginRecordsStringGrid.RowCount:=EERModel.PluginData.Count+1
  else
    PluginRecordsStringGrid.RowCount:=2;

  for i:=0 to PluginRecordsStringGrid.ColCount-1 do
    PluginRecordsStringGrid.Cols[i].Clear;

  PluginRecordsStringGrid.Cells[0, 0]:=DMMain.GetTranslatedMessage('Plugin Name', 223);
  PluginRecordsStringGrid.Cells[1, 0]:=DMMain.GetTranslatedMessage('Obj.ID', 224);

  for i:=0 to EERModel.PluginData.Count-1 do
  begin
    PluginRecordsStringGrid.Cells[0, 1+i]:=TEERPluginData(EERModel.PluginData[i]).PluginName;
    PluginRecordsStringGrid.Cells[1, 1+i]:=IntToStr(TEERPluginData(EERModel.PluginData[i]).Obj_id);
  end;

  PluginRecordsStringGrid.Row:=1;
  PluginRecordsStringGrid.Col:=0;
  PluginRecordsStringGridClick(self);


  UsePosGridCBox.Checked:=EERModel.UsePositionGrid;
  GridXEd.Text:=IntToStr(EERModel.PositionGrid.X);
  GridYEd.Text:=IntToStr(EERModel.PositionGrid.Y);
  UsePosGridCBoxClick(self);

  TableNameInRefsCBox.Checked:=EERModel.TableNameInRefs;
  DefaultTableTypeCBox.ItemIndex:=EERModel.DefaultTableType;
  ActivateRefDefForNewRelationsCBox.Checked:=EERModel.ActivateRefDefForNewRelations;

  FKPrefixEd.Text:=EERModel.FKPrefix;
  FKPostfixEd.Text:=EERModel.FKPostfix;

  CreateFKRefDefIndexCBox.Checked:=EERModel.CreateFKRefDefIndex;

  CreateSQLforLinkedObjectsCBox.Checked:=EERModel.CreateSQLforLinkedObjects;

  CanvasWidthEd.Text:=IntToStr(EERModel.EERModel_Width);
  CanvasHeightEd.Text:=IntToStr(EERModel.EERModel_Height);  
end;

procedure TOptionsModelForm.NewTblPrefixBtnClick(Sender: TObject);
var s: string;
begin
  s:='';

  DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Enter new Table Prefix', 225),
    DMMain.GetTranslatedMessage('New Table Prefix:', 226), s);
  if(s<>'')then
    TablePrefixCLBox.Items.Add(s);
end;

procedure TOptionsModelForm.TablePrefixCLBoxClickCheck(Sender: TObject);
var i: integer;
begin
  for i:=0 to TablePrefixCLBox.Items.Count-1 do
    TablePrefixCLBox.Checked[i]:=False;

  if(TablePrefixCLBox.ItemIndex>-1)then
    TablePrefixCLBox.Checked[TablePrefixCLBox.ItemIndex]:=True
  else
    TablePrefixCLBox.Checked[0]:=True;
end;

procedure TOptionsModelForm.TablePrefixCLBoxDblClick(Sender: TObject);
var s: string;
begin
  s:=TablePrefixCLBox.Items[TablePrefixCLBox.ItemIndex];

  DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Enter Table Prefix', 227),
    DMMain.GetTranslatedMessage('Table Prefix:', 228), s);
  if(s<>'')then
    TablePrefixCLBox.Items[TablePrefixCLBox.ItemIndex]:=s;
end;

procedure TOptionsModelForm.DelTblPrefixBtnClick(Sender: TObject);
begin
  if(TablePrefixCLBox.ItemIndex>0)then
    TablePrefixCLBox.Items.Delete(TablePrefixCLBox.ItemIndex);

  TablePrefixCLBoxClickCheck(self);
end;

procedure TOptionsModelForm.PluginRecordsStringGridClick(Sender: TObject);
var i: integer;
begin
  PluginParamsStringGrid.Cols[0].Clear;

  if(PluginRecordsStringGrid.Row-1<EERModel.PluginData.Count)then
    PluginParamsStringGrid.RowCount:=TEERPluginData(EERModel.PluginData[PluginRecordsStringGrid.Row-1]).Params.Count;

  if(PluginRecordsStringGrid.Row<=EERModel.PluginData.Count)then
  begin
    for i:=0 to TEERPluginData(EERModel.PluginData[PluginRecordsStringGrid.Row-1]).Params.Count-1 do
      PluginParamsStringGrid.Cells[0, i]:=TEERPluginData(EERModel.PluginData[PluginRecordsStringGrid.Row-1]).Params[i];
  end;
end;

procedure TOptionsModelForm.MakeEditSettingsDefaultBtnClick(
  Sender: TObject);
begin
  DMEER.UsePositionGrid:=UsePosGridCBox.Checked;
  try
    DMEER.PositionGrid.X:=StrToInt(GridXEd.Text);
    DMEER.PositionGrid.Y:=StrToInt(GridYEd.Text);
  except
    DMEER.PositionGrid.X:=20;
    DMEER.PositionGrid.Y:=20;
  end;

  DMEER.TableNameInRefs:=TableNameInRefsCBox.Checked;
  DMEER.DefaultTableType:=DefaultTableTypeCBox.ItemIndex;
  DMEER.ActivateRefDefForNewRelations:=ActivateRefDefForNewRelationsCBox.Checked;

  DMEER.FKPrefix:=FKPrefixEd.Text;
  DMEER.FKPostfix:=FKPostfixEd.Text;

  DMEER.CreateFKRefDefIndex:=CreateFKRefDefIndexCBox.Checked;

  MessageDlg(DMMain.GetTranslatedMessage('Default Settings changed.', 229),
    mtInformation, [mbOK], 0);
end;

procedure TOptionsModelForm.SubmitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TOptionsModelForm.AbortBtnClick(Sender: TObject);
begin
  DiscardChanges:=True;

  Close;
end;

procedure TOptionsModelForm.SubmitBtnMouseEnter(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=True;
end;

procedure TOptionsModelForm.SubmitBtnMouseLeave(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=False;
end;

procedure TOptionsModelForm.UsePosGridCBoxClick(Sender: TObject);
begin
  GridWidthLbl.Enabled:=UsePosGridCBox.Checked;
  GridXEd.Enabled:=UsePosGridCBox.Checked;
  GridWidthUnitsLbl.Enabled:=UsePosGridCBox.Checked;

  GridHeightLbl.Enabled:=UsePosGridCBox.Checked;
  GridYEd.Enabled:=UsePosGridCBox.Checked;
  GridHeightUnitsLbl.Enabled:=UsePosGridCBox.Checked;
end;

procedure TOptionsModelForm.RemovePluginDataClick(Sender: TObject);
var i: integer;
begin
  if(MessageDlg(DMMain.GetTranslatedMessage('Are you shure you want to delete the selected Plugin Data?'+
    'This action cannot be made undone.', 230), mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
  begin
    if(PluginRecordsStringGrid.Row-1<EERModel.PluginData.Count)then
      EERModel.PluginData.Delete(PluginRecordsStringGrid.Row-1);

    for i:=1 to PluginRecordsStringGrid.RowCount-1 do
      PluginRecordsStringGrid.Rows[i].Clear;

    //At lease 2 Rows (1 fixed an 1 data)
    if(EERModel.PluginData.Count>0)then
      PluginRecordsStringGrid.RowCount:=EERModel.PluginData.Count+1
    else
      PluginRecordsStringGrid.RowCount:=2;

    for i:=0 to EERModel.PluginData.Count-1 do
    begin
      PluginRecordsStringGrid.Cells[0, 1+i]:=TEERPluginData(EERModel.PluginData[i]).PluginName;
      PluginRecordsStringGrid.Cells[1, 1+i]:=IntToStr(TEERPluginData(EERModel.PluginData[i]).Obj_id);
    end;

    PluginRecordsStringGrid.Row:=1;
    PluginRecordsStringGrid.Col:=0;
    PluginRecordsStringGridClick(self);
  end;
end;

procedure TOptionsModelForm.PageControlChange(Sender: TObject);
begin
  MakeEditSettingsDefaultBtn.Visible:=(PageControl.ActivePage=EditOptionsSheet);
end;

procedure TOptionsModelForm.PageControlTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  PageControl.ActivePageIndex:=PageControlTreeView.Selected.AbsoluteIndex;
  PageControlTitleLbl.Caption:=PageControlTreeView.Selected.Text;
end;

procedure TOptionsModelForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_Escape)then
    AbortBtnClick(self);
end;

procedure TOptionsModelForm.CanvasWidthEdChange(Sender: TObject);
begin
  if(CanvasWidthEd.Text<>'')and(ActiveControl<>CanvasHeightEd)then
    CanvasHeightEd.Text:=IntToStr(Round(StrToInt(CanvasWidthEd.Text)/1.4412385643912737508796622097115));
end;

procedure TOptionsModelForm.CanvasHeightEdChange(Sender: TObject);
begin
  if(CanvasHeightEd.Text<>'')and(ActiveControl<>CanvasWidthEd)then
    CanvasWidthEd.Text:=IntToStr(Round(StrToInt(CanvasHeightEd.Text)*1.4412385643912737508796622097115));

end;

end.
