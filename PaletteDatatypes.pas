unit PaletteDatatypes;

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
// Unit PaletteDatatypes.pas
// -------------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Contains the datatype palette form class
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QComCtrls, QImgList, EERModel, StrUtils, QButtons,
  QMenus, QTypes, Qt, EERDM;

type
  TPaletteDataTypesForm = class(TForm)
    DatatypesImgList: TImageList;
    DatatypesPopupMenu: TPopupMenu;
    AddDatatypetoCommonMI: TMenuItem;
    DeleteDatatypeMI: TMenuItem;
    EditDatatypeMI: TMenuItem;
    N1: TMenuItem;
    AddnewDatatype1: TMenuItem;
    N2: TMenuItem;
    DelDatatypefromCommonDatatypesMI: TMenuItem;
    N3: TMenuItem;
    ReinitialMI: TMenuItem;
    ReplaceDatatypeMI: TMenuItem;
    MainPnl: TPanel;
    PageControl: TPageControl;
    CommonDatatypesSheet: TTabSheet;
    CommonDataTypesListView: TListView;
    AllDataTypesSheet: TTabSheet;
    Shape1: TShape;
    AllDataTypesTV: TTreeView;
    TabsPnl: TPanel;
    Tabs2Img: TImage;
    TabsImg: TImage;
    OptionsImg: TImage;
    CommonDatatypesPBox: TPaintBox;
    AllDatatypesPBox: TPaintBox;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AllDataTypesTVCustomDrawItem(Sender: TCustomViewControl;
      Item: TCustomViewItem; Canvas: TCanvas; const Rect: TRect;
      State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);

    procedure DisplayDataTypes(theModel: TEERModel);
    procedure CommonDataTypesListViewCustomDrawItem(
      Sender: TCustomViewControl; Item: TCustomViewItem; Canvas: TCanvas;
      const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure CommonDataTypesListViewDblClick(Sender: TObject);
    procedure CommonDataTypesListViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DelSBtnClick(Sender: TObject);
    procedure AddSBtnClick(Sender: TObject);
    procedure OptionsImgClick(Sender: TObject);
    procedure CommonDataTypesListViewDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure CommonDataTypesListViewDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure AddDatatypetoCommonMIShow(Sender: TObject);
    procedure DelDatatypefromCommonDatatypesMIShow(Sender: TObject);
    procedure EditDatatypeMIShow(Sender: TObject);
    procedure DelDatatypefromCommonDatatypesMIClick(Sender: TObject);
    function GetActiveDatatype: TEERDatatype;
    procedure DeleteDatatypeMIClick(Sender: TObject);
    procedure AddDatatypetoCommonMIClick(Sender: TObject);
    procedure ReinitialMIClick(Sender: TObject);
    procedure ReplaceDatatypeMIClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure PageControlResize(Sender: TObject);
    procedure CommonDataTypesListViewMouseEnter(Sender: TObject);
    procedure AllDataTypesTVMouseEnter(Sender: TObject);
    procedure AllDataTypesTVMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CommonDatatypesPBoxPaint(Sender: TObject);
    procedure CommonDatatypesPBoxClick(Sender: TObject);
    procedure AllDatatypesPBoxClick(Sender: TObject);
  private
    { Private declarations }
    CommonDataTypesListViewRowHeight: integer;
  public
    { Public declarations }
    EERModel: TEERModel;
  end;

var
  PaletteDataTypesForm: TPaletteDataTypesForm;

implementation

uses EditorDatatype, Main, MainDM, PaletteDataTypesReplace;

{$R *.xfm}

procedure TPaletteDataTypesForm.FormCreate(Sender: TObject);
var theSize: TSize;
begin
  DMMain.InitForm(self);

  CommonDataTypesListViewRowHeight:=-1;

  PageControl.Style:=tsNoTabs;
  CommonDatatypesPBoxClick(self);

  theSize:=Canvas.TextExtent('All Types');

  AllDataTypesTV.Top:=(theSize.cy+6)*-1;
  AllDataTypesTV.Height:=Height+19;
end;

procedure TPaletteDataTypesForm.FormResize(Sender: TObject);
begin
  OptionsImg.Left:=Width-OptionsImg.Width-1;
  OptionsImg.BringToFront;

  AllDataTypesTV.Height:=AllDataTypesSheet.Height+19;
  AllDataTypesTV.Width:=Width-10;
end;

procedure TPaletteDataTypesForm.DisplayDataTypes(theModel: TEERModel);
var i, j: integer;
  TheGroupNode, TheTypeNode: TTreeNode;
  TheListItem: TListItem;
  theDatatype: TEERDatatype;
  ExpandedNodes: TStringList;
begin
  ExpandedNodes:=TStringList.Create;
  try
    if(EERModel=theModel)then
    begin
      //Store expanded nodes
      for i:=0 to AllDataTypesTV.Items.Count-1 do
      begin
        if(AllDataTypesTV.Items[i].Expanded)then
          ExpandedNodes.Add(AllDataTypesTV.Items[i].Text);
      end;
    end
    else
      EERModel:=theModel;

    AllDataTypesTV.Items.Clear;

    CommonDataTypesListView.Items.Clear;

    if(theModel=nil)then
    begin
      {AddSBtn.Enabled:=False;
      DelSBtn.Enabled:=False;}
      Exit;
    end
    else
    begin
      {AddSBtn.Enabled:=True;
      DelSBtn.Enabled:=True;}
    end;

    //Display Common DataTypes
    for i:=0 to theModel.CommonDataType.Count-1 do
    begin
      theDatatype:=theModel.GetDataType(StrToInt(theModel.CommonDataType[i]));

      if(Assigned(theDatatype))then
      begin
        TheListItem:=CommonDataTypesListView.Items.Add;
        TheListItem.Caption:=theDatatype.TypeName;
        TheListItem.ImageIndex:=theDatatype.group;
        TheListItem.Data:=theDatatype;
      end;
    end;

    //Display all Datatypes
    for i:=0 to theModel.DatatypeGroups.Count-1 do
    begin

      TheGroupNode:=AllDataTypesTV.Items.Add(nil, TEERDatatypeGroup(theModel.DatatypeGroups[i]).GroupName);
      TheGroupNode.ImageIndex:=TEERDatatypeGroup(theModel.DatatypeGroups[i]).IconNr-1;
      TheGroupNode.SelectedIndex:=TEERDatatypeGroup(theModel.DatatypeGroups[i]).IconNr-1;
      TheGroupNode.Data:=nil;

      for j:=0 to theModel.Datatypes.Count-1 do
      begin
        if(TEERDatatype(theModel.Datatypes[j]).group=i)then
        begin
          TheTypeNode:=AllDataTypesTV.Items.AddChild(TheGroupNode, TEERDatatype(theModel.Datatypes[j]).TypeName);
          TheTypeNode.ImageIndex:=TheGroupNode.ImageIndex;
          TheTypeNode.SelectedIndex:=TheGroupNode.SelectedIndex;
          TheTypeNode.Data:=theModel.Datatypes[j];
        end;
      end;
    end;

    //Re-expand tree
    if(ExpandedNodes.Count>0)then
      for i:=0 to ExpandedNodes.Count-1 do
      begin
        for j:=0 to AllDataTypesTV.Items.Count-1 do
          if(ExpandedNodes[i]=AllDataTypesTV.Items[j].Text)and
            (Not(AllDataTypesTV.Items[j].Expanded))then
            begin
              AllDataTypesTV.Items[j].Expanded:=True;
              break;
            end;
      end;
  finally
    ExpandedNodes.Free;
  end;
end;

procedure TPaletteDataTypesForm.AllDataTypesTVCustomDrawItem(
  Sender: TCustomViewControl; Item: TCustomViewItem; Canvas: TCanvas;
  const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
var theSize: TSize;
  theNode: TTreeNode;
  s: string;
  i: integer;
begin
  {with TTreeNode(Item) do begin
    Offset := (Rect.Right - Rect.Left - Canvas.TextWidth(Text)) div 2;
    Canvas.Ellipse(Rect);
    Canvas.TextOut(Offset, 0, Text);
  end;}

  with Canvas do
  begin
    Pen.Color:=clWhite;
    MoveTo(0, Rect.Top);
    LineTo(Rect.Right, Rect.Top);
    Pen.Color:=clDark;
    MoveTo(0, Rect.Bottom-1);
    LineTo(AllDataTypesTV.Width+30, Rect.Bottom-1);

    theNode:=AllDataTypesTV.Items[AllDataTypesTV.Items.IndexOf(Item)];


    if(Assigned(theNode.Data))then
    begin
      if(TEERDatatype(theNode.Data).ParamCount>0)then
      begin
        theSize:=TextExtent(TEERDatatype(theNode.Data).TypeName);

        s:='(';

        for i:=0 to TEERDatatype(theNode.Data).ParamCount-1 do
        begin
          s:=s+LeftStr(TEERDatatype(theNode.Data).Param[i], 3);

          if(i<>TEERDatatype(theNode.Data).ParamCount-1)then
            s:=s+', ';
        end;

        s:=s+')';

        if(Not(cdsSelected in State))then
        begin
          if(TEERDatatype(theNode.Data).ParamRequired)then
            Font.Color:=clBlack
          else
            Font.Color:=clGray;
        end
        else
          Font.Color:=clWhite;

        TextOut(theSize.cx+22, 2, s);
      end;
    end;
  end;

  DefaultDraw := True; //item already complete
end;

procedure TPaletteDataTypesForm.CommonDataTypesListViewCustomDrawItem(
  Sender: TCustomViewControl; Item: TCustomViewItem; Canvas: TCanvas;
  const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  if(CommonDataTypesListViewRowHeight=-1)then
    CommonDataTypesListViewRowHeight:=Rect.Bottom-Rect.Top;

  with Canvas do
  begin
    Pen.Color:=clWhite;
    MoveTo(0, Rect.Top);
    LineTo(Rect.Right, Rect.Top);
    Pen.Color:=clDark;
    MoveTo(0, Rect.Bottom-1);
    LineTo(AllDataTypesTV.Width+30, Rect.Bottom-1);
  end;
end;

procedure TPaletteDataTypesForm.CommonDataTypesListViewDblClick(
  Sender: TObject);
begin
  if(GetActiveDatatype<>nil)then
  begin
    EditorDatatypeForm:=TEditorDatatypeForm.Create(Application.Mainform);
    try
      //Call the Datatype Editor
      EditorDatatypeForm.SetDataType(EERModel, GetActiveDatatype);

      EditorDatatypeForm.ShowModal;

      DisplayDataTypes(EERModel);
    finally
      EditorDatatypeForm.Free;
    end;

    Application.MainForm.SetFocus;
  end;


end;

procedure TPaletteDataTypesForm.CommonDataTypesListViewMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var theItem: TListItem;
begin
  theItem:=CommonDataTypesListView.GetItemAt(X, Y);
  if(theItem<>nil)then
    CommonDataTypesListView.Selected:=theItem;

  CommonDataTypesListView.BeginDrag(False, 5);
end;

procedure TPaletteDataTypesForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=False;

  Hide;

  TMainForm(Application.MainForm).DatatypesMI.Checked:=False;
end;

procedure TPaletteDataTypesForm.FormDestroy(Sender: TObject);
begin
  DMMain.SaveWinPos(self, False);
end;

procedure TPaletteDataTypesForm.FormShow(Sender: TObject);
begin
  DMMain.RestoreWinPos(self, False);
end;

procedure TPaletteDataTypesForm.DelSBtnClick(Sender: TObject);
begin
  if(PageControl.ActivePage=CommonDatatypesSheet)then
    DelDatatypefromCommonDatatypesMIClick(self)
  else if(PageControl.ActivePage=AllDataTypesSheet)then
    DeleteDatatypeMIClick(self);
end;

procedure TPaletteDataTypesForm.AddSBtnClick(Sender: TObject);
var newDT: TEERDatatype;
  i, newid: integer;
begin
  if(EERModel<>nil)then
  begin
    //Get next id
    newid:=1;
    for i:=0 to EERModel.Datatypes.Count-1 do
      if(newid<=TEERDatatype(EERModel.Datatypes[i]).id)then
        newid:=TEERDatatype(EERModel.Datatypes[i]).id+1;

    //add new Datatype
    //new(newDT);
    newDT:=TEERDatatype.Create(EERModel);
    try
      newDT.id:=newid;
      newDT.TypeName:=DMMain.GetTranslatedMessage('NewDatatype', 231);
      newDT.group:=4;
      newDT.description:=DMMain.GetTranslatedMessage('User defined Datatype.', 232);
      newDT.ParamCount:=0;
      newDT.OptionCount:=0;
      newDT.ParamRequired:=false;
      newDT.SynonymGroup:=0;
      EERModel.Datatypes.Add(newDT);
    except
      newDT.Free;
    end;

    if(PageControl.ActivePage=CommonDatatypesSheet)then
      EERModel.CommonDataType.Add(IntToStr(newid));

    DisplayDataTypes(EERModel);

    //Display new created datatype
    EditorDatatypeForm:=TEditorDatatypeForm.Create(Application.Mainform);
    try
      //Call the Datatype Editor
      EditorDatatypeForm.SetDataType(EERModel, newDT);

      EditorDatatypeForm.ShowModal;

      DisplayDataTypes(EERModel);
    finally
      EditorDatatypeForm.Free;
    end;

    Application.MainForm.SetFocus;
  end;
end;

procedure TPaletteDataTypesForm.OptionsImgClick(Sender: TObject);
begin
  DatatypesPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TPaletteDataTypesForm.CommonDataTypesListViewDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var newPos, oldid, p: integer;
begin
  //Droped on itself
  if(Sender=Source)and(CommonDataTypesListViewRowHeight>-1)and(CommonDataTypesListView.Selected<>nil)then
  begin
    if(Assigned(CommonDataTypesListView.Selected.Data))then
    begin
      newPos:=Y div CommonDataTypesListViewRowHeight;
      oldid:=TEERDatatype(CommonDataTypesListView.Selected.Data).id;

      p:=EERModel.CommonDataType.IndexOf(IntToStr(oldid));
      if(p>-1)then
        EERModel.CommonDataType.Delete(p);

      EERModel.CommonDataType.Insert(newPos, IntToStr(oldid));

      DisplayDataTypes(EERModel);
    end;
  end;
end;

procedure TPaletteDataTypesForm.CommonDataTypesListViewDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=False;
  if(Source<>nil)then
    Accept:=(Sender=Source);
end;

procedure TPaletteDataTypesForm.AddDatatypetoCommonMIShow(Sender: TObject);
begin
  if(PageControl.ActivePage=AllDataTypesSheet)and
    (AllDataTypesTV.Selected<>nil)then
    TMenuItem(Sender).Enabled:=Assigned(AllDataTypesTV.Selected.Data)
  else
    TMenuItem(Sender).Enabled:=False;
end;

procedure TPaletteDataTypesForm.DelDatatypefromCommonDatatypesMIShow(
  Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=(PageControl.ActivePage=CommonDataTypesSheet);
end;

procedure TPaletteDataTypesForm.EditDatatypeMIShow(Sender: TObject);
begin
  if(PageControl.ActivePage=AllDataTypesSheet)and
    (AllDataTypesTV.Selected<>nil)then
    TMenuItem(Sender).Enabled:=Assigned(AllDataTypesTV.Selected.Data)
  else
    TMenuItem(Sender).Enabled:=False;

  if(PageControl.ActivePage=CommonDataTypesSheet)and
    (CommonDataTypesListView.Selected<>nil)then
    TMenuItem(Sender).Enabled:=True;
end;

function TPaletteDataTypesForm.GetActiveDatatype: TEERDatatype;
begin
  GetActiveDatatype:=nil;

  //Get id if CommonDatatypesSheet is active
  if(PageControl.ActivePage=CommonDatatypesSheet)and
    (CommonDataTypesListView.Selected<>nil)then
  begin
    if(Assigned(CommonDataTypesListView.Selected.Data))then
      GetActiveDatatype:=TEERDatatype(CommonDataTypesListView.Selected.Data);
  end;

  //Get id if AllDatatypesSheet is active
  if(PageControl.ActivePage=AllDatatypesSheet)and
    (AllDataTypesTV.Selected<>nil)then
  begin
    if(Assigned(AllDataTypesTV.Selected.Data))then
      GetActiveDatatype:=TEERDatatype(AllDataTypesTV.Selected.Data);
  end;
end;

procedure TPaletteDataTypesForm.DelDatatypefromCommonDatatypesMIClick(
  Sender: TObject);
var p: integer;
begin
  if(GetActiveDatatype<>nil)then
  begin
    p:=EERModel.CommonDataType.IndexOf(IntToStr(GetActiveDatatype.id));
    if(p>-1)then
      EERModel.CommonDataType.Delete(p);

    DisplayDataTypes(EERModel);
  end;
end;

procedure TPaletteDataTypesForm.DeleteDatatypeMIClick(Sender: TObject);
var theDT: TEERDatatype;
  id, i, j: integer;
begin
  theDT:=GetActiveDatatype;

  if(theDT<>nil)then
  begin
    if(theDT.id=EERModel.DefaultDataType)then
      ShowMessage(DMMain.GetTranslatedMessage('You cannot delete the default datatype of the model. '+#13#10+
        'Change the default datatype in the Model Options.', 233))
    else if(MessageDlg(DMMain.GetTranslatedMessage('Do you really want to delete the selected datatype %s?'+#13#10+
      'The datatype will be replaced by the default datatype if it is used in existing tables.', 234, theDT.TypeName),
      mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
    begin
      //Delete from common Datatypes
      DelDatatypefromCommonDatatypesMIClick(self);

      id:=theDT.id;

      //Replace datatype in Tables
      for i:=0 to EERModel.ComponentCount-1 do
      begin
        if(EERModel.Components[i].ClassnameIs('TEERTable'))then
        begin
          for j:=0 to TEERTable(EERModel.Components[i]).Columns.Count-1 do
          begin
            if(TEERColumn(TEERTable(EERModel.Components[i]).Columns[j]).idDatatype=id)then
              TEERColumn(TEERTable(EERModel.Components[i]).Columns[j]).idDatatype:=EERModel.DefaultDataType;
          end;
        end;
      end;

      //Remove from Datatypes
      EERModel.Datatypes.Delete(EERModel.Datatypes.IndexOf(theDT));

      DisplayDataTypes(EERModel);
      EERModel.Refresh;
    end;
  end;
end;

procedure TPaletteDataTypesForm.AddDatatypetoCommonMIClick(
  Sender: TObject);
begin
  if(PageControl.ActivePage=AllDatatypesSheet)then
  begin
    if(GetActiveDatatype<>nil)then
    begin
      EERModel.CommonDataType.Add(IntToStr(GetActiveDatatype.id));

      DisplayDataTypes(EERModel);
    end;
  end;
end;

procedure TPaletteDataTypesForm.ReinitialMIClick(Sender: TObject);
var i, j: integer;
begin
  if(MainForm.ActiveMDIChild<>nil)then
    if(MessageDlg(DMMain.GetTranslatedMessage('Do you really want to reset the datatypes '+
        'to the initial configuration?'+#13#10+
        'Manual added datatypes will be replaced by the default datatype if they are used in existing tables.', 235),
        mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
    begin
      EERModel.LoadDataTypesFromIniFile;

      //Replace datatype in Tables
      for i:=0 to EERModel.ComponentCount-1 do
      begin
        if(EERModel.Components[i].ClassnameIs('TEERTable'))then
        begin
          for j:=0 to TEERTable(EERModel.Components[i]).Columns.Count-1 do
          begin
            if(EERModel.GetDataType(TEERColumn(TEERTable(EERModel.Components[i]).Columns[j]).idDatatype)=nil)then
              TEERColumn(TEERTable(EERModel.Components[i]).Columns[j]).idDatatype:=EERModel.DefaultDataType;
          end;
        end;
      end;

      DisplayDataTypes(EERModel);
      EERModel.Refresh;
    end;
end;

procedure TPaletteDataTypesForm.ReplaceDatatypeMIClick(Sender: TObject);
begin
  if(MainForm.ActiveMDIChild<>nil)then
  begin
    PaletteDataTypesReplaceForm:=TPaletteDataTypesReplaceForm.Create(Application.MainForm);
    try
      PaletteDataTypesReplaceForm.SetModel(EERModel);
      PaletteDataTypesReplaceForm.ShowModal;
    finally
      PaletteDataTypesReplaceForm.Free;
    end;
  end;
end;

procedure TPaletteDataTypesForm.FormDeactivate(Sender: TObject);
begin
  if(Visible)then
    if(Not(DMMain.IsFormStayingOnTop(self)))then
      sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));
end;

procedure TPaletteDataTypesForm.PageControlResize(Sender: TObject);
begin
  AllDataTypesTV.Height:=AllDataTypesSheet.Height+19;
end;

procedure TPaletteDataTypesForm.CommonDataTypesListViewMouseEnter(
  Sender: TObject);
begin
  CommonDataTypesListView.SetFocus;
end;

procedure TPaletteDataTypesForm.AllDataTypesTVMouseEnter(Sender: TObject);
begin
  AllDataTypesTV.SetFocus;
end;

procedure TPaletteDataTypesForm.AllDataTypesTVMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var theItem: TTreeNode;
begin
  theItem:=AllDataTypesTV.GetNodeAt(X, Y);
  if(theItem<>nil)then
  begin
    theItem.MakeVisible;
    AllDataTypesTV.Selected:=theItem;
  end;

  AllDataTypesTV.BeginDrag(False, 5);
end;

procedure TPaletteDataTypesForm.CommonDatatypesPBoxPaint(Sender: TObject);
begin
  //Paint Text (for XTF smooth fonts)
  TPaintBox(Sender).Canvas.Font.Color:=clBlack;
  TPaintBox(Sender).Canvas.TextOut(0, 0,
    DMMain.GetTranslatedMessage('', TPaintBox(Sender).Tag));
end;

procedure TPaletteDataTypesForm.CommonDatatypesPBoxClick(Sender: TObject);
begin
  TabsImg.BringToFront;
  CommonDatatypesPBox.BringToFront;
  AllDatatypesPBox.BringToFront;
  PageControl.ActivePage:=CommonDatatypesSheet;

  OptionsImg.BringToFront;
end;

procedure TPaletteDataTypesForm.AllDatatypesPBoxClick(Sender: TObject);
begin
  Tabs2Img.BringToFront;
  CommonDatatypesPBox.BringToFront;
  AllDatatypesPBox.BringToFront;
  PageControl.ActivePage:=AllDataTypesSheet;

  OptionsImg.BringToFront;
end;

end.
