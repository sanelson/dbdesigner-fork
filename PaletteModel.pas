unit PaletteModel;

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
// Unit PaletteModel.pas
// ---------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Contains a the model palette form class
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QComCtrls, QImgList, QButtons, EERModel, QMenus,
  QTypes, Qt;

type
  TPaletteModelFrom = class(TForm)
    ModelImgList: TImageList;
    TablesPopupMenu: TPopupMenu;
    SelectObjectMI: TMenuItem;
    EditObjectMI: TMenuItem;
    RefreshMI: TMenuItem;
    DeleteMI: TMenuItem;
    PalettePopupMenu: TPopupMenu;
    RefreshPalMI: TMenuItem;
    MainPnl: TPanel;
    TabsPnl: TPanel;
    Tabs2Img: TImage;
    TabsImg: TImage;
    OptionsImg: TImage;
    PageControl: TPageControl;
    ModelSheet: TTabSheet;
    Shape1: TShape;
    ModelTV: TTreeView;
    Panel1: TPanel;
    TablesSheet: TTabSheet;
    TablesTreeView: TTreeView;
    TableSheetTopPnl: TPanel;
    TableTreeHeaderPnl: TPanel;
    AddBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
    Label1: TLabel;
    ScrolltoselectedObjectMI: TMenuItem;
    N1: TMenuItem;
    ShowLinkedModelsMI: TMenuItem;
    N2: TMenuItem;
    RefreshLinkedObjectsMI: TMenuItem;
    TablesPBox: TPaintBox;
    ModelPBox: TPaintBox;
    N3: TMenuItem;
    ReorderTablesbyNameMI: TMenuItem;
    ReorderTablesbyRegionMI: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ModelTVCustomDrawItem(Sender: TCustomViewControl;
      Item: TCustomViewItem; Canvas: TCanvas; const Rect: TRect;
      State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure RefreshTablesTreeView(theModel: TEERModel);
    procedure TablesTreeViewDblClick(Sender: TObject);
    procedure SelectObjectMIClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RefreshMIClick(Sender: TObject);
    procedure DeleteMIClick(Sender: TObject);
    procedure OptionsImgClick(Sender: TObject);
    procedure RefreshPalMIClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ScrolltoselectedObjectMIClick(Sender: TObject);
    procedure ShowLinkedModelsMIShow(Sender: TObject);
    procedure ShowLinkedModelsMIClick(Sender: TObject);
    procedure RefreshLinkedObjectsMIClick(Sender: TObject);
    procedure TablesPBoxPaint(Sender: TObject);
    procedure TablesPBoxClick(Sender: TObject);
    procedure ModelPBoxClick(Sender: TObject);
    procedure TablesTreeViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TablesTreeViewDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TablesTreeViewDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure ReorderTablesbyNameMIClick(Sender: TObject);
    procedure ReorderTablesbyRegionMIClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    EERModel: TEERModel;
  end;

var
  PaletteModelFrom: TPaletteModelFrom;

implementation

uses EditorDatatype, Main, EditorTable, MainDM, EER, EERDM;

{$R *.xfm}

procedure TPaletteModelFrom.FormCreate(Sender: TObject);
var theSize: tSize;
begin
  DMMain.InitForm(self);
  
  PageControl.Style:=tsNoTabs;

  theSize:=Canvas.TextExtent('All Types');
  ModelTV.Left:=1;
  ModelTV.Top:=(theSize.cy+5)*-1;
  ModelTV.Height:=ModelSheet.Height-ModelTV.Top+21;
  ModelTV.Width:=ModelSheet.Width-2;
  ModelTV.Items.Clear;


  //TablesTreeView.Top:=ModelTV.Top;
  ModelTV.Left:=ModelTV.Left;
  //TablesTreeView.Height:=ModelTV.Height;
  //TablesTreeView.Width:=ModelTV.Width;
  TablesTreeView.Items.Clear;

  TablesPBoxClick(Self);
end;

procedure TPaletteModelFrom.FormResize(Sender: TObject);
begin
  OptionsImg.Left:=Width-OptionsImg.Width-1;
  OptionsImg.BringToFront;

  ModelTV.Height:=ModelSheet.Height+19;
  ModelTV.Width:=Width-10;

  //TablesTreeView.Width:=ModelTV.Width;
  //TablesTreeView.Height:=ModelTV.Height;
end;

procedure TPaletteModelFrom.ModelTVCustomDrawItem(
  Sender: TCustomViewControl; Item: TCustomViewItem; Canvas: TCanvas;
  const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
var theRegion: TEERRegion;
  s: string;
begin
  with Canvas do
  begin
    Pen.Color:=clDark;
    MoveTo(0, Rect.Bottom-1);
    LineTo(ModelTV.Width+30, Rect.Bottom-1);
  end;

  //If a table item is drawn, get bgcolor from region
  if(Item is TTreeNode)then
    if(TTreeNode(Item).Data<>nil)and(TTreeNode(Item).ImageIndex<>-1)then
      if(TObject(TTreeNode(Item).Data) is TEERTable)then
      begin
        theRegion:=TEERTable(TTreeNode(Item).Data).GetRegion;
        if(theRegion<>nil)then
        begin
          try
            s:=EERModel.RegionColors.ValueFromIndex[theRegion.RegionColor];

            Canvas.Brush.Color:=DMMain.RGB(DMMain.HexStringToInt(Copy(s, 2, 2)),
              DMMain.HexStringToInt(Copy(s, 4, 2)),
              DMMain.HexStringToInt(Copy(s, 6, 2)));

            Canvas.FillRect(Types.Rect(Rect.Left, Rect.Top,
              ModelTV.Width+30, Rect.Bottom-1));

            TablesTreeView.Images.Draw(Canvas, Rect.Left, Rect.Top,
              0, itImage, True);

            Canvas.TextOut(Rect.Left+16, Rect.Top+2, TTreeNode(Item).Text);

            if(TTreeNode(Item).Selected)then
            begin
              Canvas.Brush.Color:=clBlack;
              Canvas.FillRect(Types.Rect(Rect.Right-12, Rect.Top+6,
                Rect.Right-7, Rect.Top+11));
              Canvas.Brush.Color:=clWhite;
            end;
          except
          end;
        end;
      end;
end;

procedure TPaletteModelFrom.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=False;

  Hide;

  TMainForm(Application.MainForm).DBModelMI.Checked:=False;
end;

procedure TPaletteModelFrom.RefreshTablesTreeView(theModel: TEERModel);
var i, j: integer;
  theNode, theKindNode, theChildNode: TTreeNode;
  ExpandedNodes: TList;
  TableList: TList;
begin
  ExpandedNodes:=TList.Create;
  try
    if(EERModel=theModel)then
    begin
      //Store expanded nodes
      for i:=0 to TablesTreeView.Items.Count-1 do
      begin
        if(TablesTreeView.Items[i].Expanded)then
          ExpandedNodes.Add(TablesTreeView.Items[i].Data);
      end;
    end
    else
      EERModel:=theModel;

    TablesTreeView.Items.Clear;

    if(theModel=nil)then
      Exit;

    TableList:=TList.Create;
    try
      with EERModel do
      begin
        GetEERObjectList([EERTable], TableList);
        SortEERObjectListByOrderPos(TableList);

        //Add all Tables to Treeview
        for i:=0 to TableList.Count-1 do
        begin
          with TEERTable(TableList[i]) do
          begin
            theNode:=TablesTreeView.Items.AddObject(nil, ObjName, TableList[i]);
            theNode.ImageIndex:=0;

            //Add Colums
            if(Columns.Count>0)then
            begin
              //Store first Column with the Parent 'Columns' Node, so it can be re-expanded
              theKindNode:=TablesTreeView.Items.AddChildObject(theNode, 'Columns', Columns[0]);
              for j:=0 to Columns.Count-1 do
              begin
                theChildNode:=TablesTreeView.Items.AddChildObject(theKindNode,
                  TEERColumn(Columns[j]).ColName{+': '+
                  GetDataTypeName(TEERColumn(Columns[j]).Obj_id)}, Columns[j]);
                if(TEERColumn(Columns[j]).PrimaryKey)then
                  theChildNode.ImageIndex:=2
                else
                  theChildNode.ImageIndex:=1;
              end;
            end;

            //Add Relations
            if(RelStart.Count>0)or(RelEnd.Count>0)then
            begin
              //Store table with the Parent 'Relations' Node, so it can be re-expanded
              theKindNode:=TablesTreeView.Items.AddChildObject(theNode, 'Relations', EERModel.Components[I]);
              for j:=0 to RelStart.Count-1 do
              begin
                theChildNode:=TablesTreeView.Items.AddChildObject(theKindNode,
                  TEERRel(RelStart[j]).ObjName, RelStart[j]);
                theChildNode.ImageIndex:=3;
              end;
              for j:=0 to RelEnd.Count-1 do
              begin
                theChildNode:=TablesTreeView.Items.AddChildObject(theKindNode,
                  TEERRel(RelEnd[j]).ObjName, RelEnd[j]);
                theChildNode.ImageIndex:=3;
              end;
            end;
          end;
        end;
      end;
    finally
      TableList.Free;
    end;

    //Re-expand tree
    if(ExpandedNodes.Count>0)then
      for i:=0 to ExpandedNodes.Count-1 do
      begin
        for j:=0 to TablesTreeView.Items.Count-1 do
          if(ExpandedNodes[i]=TablesTreeView.Items[j].Data)and
            (Not(TablesTreeView.Items[j].Expanded))then
            begin
              TablesTreeView.Items[j].Expanded:=True;
              break;
            end;
      end;
  finally
    ExpandedNodes.Free;
  end;
end;

procedure TPaletteModelFrom.TablesTreeViewDblClick(Sender: TObject);
begin
  if(TablesTreeView.Selected<>nil)then
    if(Assigned(TablesTreeView.Selected.Data))then
      if(TObject(TablesTreeView.Selected.Data).ClassParent=TEERObj)then
        TEERObj(TablesTreeView.Selected.Data).ShowEditor(self);
end;

procedure TPaletteModelFrom.SelectObjectMIClick(Sender: TObject);
begin
  //Columns cannot be selected
  if(TablesTreeView.Selected<>nil)then
    if(Assigned(TablesTreeView.Selected.Data))and
      ((TablesTreeView.Selected.ImageIndex=0)or //Table
      (TablesTreeView.Selected.ImageIndex=3))then //Indices
    begin
      TEERObj(TablesTreeView.Selected.Data).SelectObj(self);
    end;
end;

procedure TPaletteModelFrom.FormDestroy(Sender: TObject);
begin
  DMMain.SaveWinPos(self, False);
end;

procedure TPaletteModelFrom.FormShow(Sender: TObject);
begin
  DMMain.RestoreWinPos(self, False);
end;

procedure TPaletteModelFrom.RefreshMIClick(Sender: TObject);
begin
  //Columns cannot be selected
  if(Assigned(TablesTreeView.Selected.Data))and
    (TablesTreeView.Selected.ImageIndex<>1)and
    (TablesTreeView.Selected.ImageIndex<>2)then
  begin
    TEERObj(TablesTreeView.Selected.Data).RefreshObj;
    TEERObj(TablesTreeView.Selected.Data).DoPaint(self);
  end;
end;

procedure TPaletteModelFrom.DeleteMIClick(Sender: TObject);
begin
  //Columns cannot be selected
  if(Assigned(TablesTreeView.Selected))then
    if(Assigned(TablesTreeView.Selected.Data))and
      (TablesTreeView.Selected.ImageIndex<>1)and
      (TablesTreeView.Selected.ImageIndex<>2)then
    begin
      TEERObj(TablesTreeView.Selected.Data).DeleteObj;

      RefreshTablesTreeView(EERModel);
    end;
end;

procedure TPaletteModelFrom.OptionsImgClick(Sender: TObject);
begin
  PalettePopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TPaletteModelFrom.RefreshPalMIClick(Sender: TObject);
begin
  if(MainForm.ActiveMDIChild<>nil)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
      RefreshTablesTreeView(TEERForm(MainForm.ActiveMDIChild).EERModel);
end;

procedure TPaletteModelFrom.FormActivate(Sender: TObject);
begin
  //Application.MainForm.SetFocus;
end;

procedure TPaletteModelFrom.AddBtnClick(Sender: TObject);
var theEERTable: TEERTable;
  theEvent: QCustomEventH;
begin
  if(MainForm.ActiveMDIChild<>nil)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
    begin
      //Create the new table
      theEERTable:=TEERForm(MainForm.ActiveMDIChild).EERModel.NewTable(100, 100, True);

      //Refresh the TreeView
      RefreshTablesTreeView(TEERForm(MainForm.ActiveMDIChild).EERModel);

      //Edit the new created table
      theEvent := QCustomEvent_create(QEventType_EditTable, theEERTable);
      sendCLXEvent(Application.MainForm.Handle, theEvent);
    end;
end;

procedure TPaletteModelFrom.DeleteBtnClick(Sender: TObject);
begin
  DeleteMIClick(self);
end;

procedure TPaletteModelFrom.FormDeactivate(Sender: TObject);
begin
  {if(Visible)then
    if(Not(DMMain.IsFormStayingOnTop(self)))then
      QApplication_sendEventAndDelete(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));}
end;

procedure TPaletteModelFrom.ScrolltoselectedObjectMIClick(Sender: TObject);
begin
  if(TablesTreeView.Selected<>nil)then
    if(Assigned(TablesTreeView.Selected.Data))and
      (TablesTreeView.Selected.ImageIndex<>1)and
      (TablesTreeView.Selected.ImageIndex<>2)then
    begin
      if(MainForm.ActiveMDIChild<>nil)then
        if(MainForm.ActiveMDIChild.Classname='TEERForm')then
          TEERForm(MainForm.ActiveMDIChild).ScrollInView(
            TEERObj(TablesTreeView.Selected.Data));
    end;
end;

procedure TPaletteModelFrom.ShowLinkedModelsMIShow(Sender: TObject);
begin
  MainForm.ShowLinkedModelsMIShow(Sender);
end;

procedure TPaletteModelFrom.ShowLinkedModelsMIClick(Sender: TObject);
begin
  MainForm.ShowLinkedModelsMIClick(Sender);
end;

procedure TPaletteModelFrom.RefreshLinkedObjectsMIClick(Sender: TObject);
begin
  MainForm.RefreshLinkedObjectsMIClick(Sender);
end;

procedure TPaletteModelFrom.TablesPBoxPaint(Sender: TObject);
begin
  //Paint Text (for XTF smooth fonts)
  TPaintBox(Sender).Canvas.Font.Color:=clBlack;
  TPaintBox(Sender).Canvas.TextOut(0, 0,
    DMMain.GetTranslatedMessage('', TPaintBox(Sender).Tag));
end;

procedure TPaletteModelFrom.TablesPBoxClick(Sender: TObject);
begin
  TabsImg.BringToFront;
  TablesPBox.BringToFront;
  ModelPBox.BringToFront;
  PageControl.ActivePage:=TablesSheet;

  OptionsImg.BringToFront;
end;

procedure TPaletteModelFrom.ModelPBoxClick(Sender: TObject);
begin
  //As long as the Model Page is not implemented
  Exit;

  Tabs2Img.BringToFront;
  TablesPBox.BringToFront;
  ModelPBox.BringToFront;
  PageControl.ActivePage:=ModelSheet;

  OptionsImg.BringToFront;
end;

procedure TPaletteModelFrom.TablesTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var theNode: TTreeNode;
begin
  if(MainForm.ActiveMDIChild=nil)then
    Exit;

  theNode:=TablesTreeView.GetNodeAt(X, Y);

  //check if a table should be dragged
  if(theNode<>nil)then
    if(theNode.Data<>nil)then
      if(TObject(theNode.Data) is TEERTable)then
      begin
        TablesTreeView.Selected:=theNode;
        TablesTreeView.BeginDrag(False, 5);
      end;
end;

procedure TPaletteModelFrom.TablesTreeViewDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if(MainForm.ActiveMDIChild=nil)then
    Exit;

  Accept:=False;

  //Accept drag onto itself
  if(Source is TTreeView)then
    if(TTreeView(Source).Name='TablesTreeView')then
      Accept:=True;
end;

procedure TPaletteModelFrom.TablesTreeViewDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var theNode: TTreeNode;
  i, newpos: integer;
  DraggedTable, TargetTable: TEERTable;
  SelectedItemsList: TList;
begin
  if(Source is TTreeView)and(MainForm.ActiveMDIChild<>nil)then
    if(TTreeView(Source).Name='TablesTreeView')then
    begin
      TargetTable:=nil;

      theNode:=TablesTreeView.GetNodeAt(X, Y);
      if(theNode<>nil)then
        if(theNode.Data<>nil)then
          if(TObject(theNode.Data) is TEERTable)then
            TargetTable:=TEERTable(theNode.Data);


      //Get gragged Table
      DraggedTable:=nil;
      if(TablesTreeView.Selected<>nil)then
        if(TablesTreeView.Selected.Data<>nil)then
          if(TObject(TablesTreeView.Selected.Data) is TEERTable)then
            DraggedTable:=TEERTable(TablesTreeView.Selected.Data);

      if(TargetTable=DraggedTable)then
        Exit;

      //One Table is dragged
      if(DraggedTable<>nil)then
      begin
        if(TargetTable<>nil)then
        begin
          if(TargetTable.OrderPos<DraggedTable.OrderPos)then
            newpos:=TargetTable.OrderPos
          else
            newpos:=TargetTable.OrderPos+1;

          //Move all Objects with an equal higher OrderPos down
          for i:=0 to EERModel.ComponentCount-1 do
            if(EERModel.Components[i].ClassParent=TEERObj)then
              if(TEERObj(EERModel.Components[i]).OrderPos>=newpos)then
                TEERObj(EERModel.Components[i]).OrderPos:=
                  TEERObj(EERModel.Components[i]).OrderPos+1;

          DraggedTable.OrderPos:=newpos;

          RefreshTablesTreeView(EERModel);
        end
        else
        begin
          //Move to last position
          newpos:=0;
          for i:=0 to EERModel.ComponentCount-1 do
            if(EERModel.Components[i].ClassParent=TEERObj)then
              if(TEERObj(EERModel.Components[i]).OrderPos>=newpos)then
              newpos:=TEERObj(EERModel.Components[i]).OrderPos+1;

          DraggedTable.OrderPos:=newpos;

          RefreshTablesTreeView(EERModel);
        end;
      end
      //Two or more tables might be dragged
      else
      begin
        SelectedItemsList:=TList.Create;
        try
          for i:=0 to TablesTreeView.Items.Count-1 do
            if(TablesTreeView.Items[i].Selected)and
              (TablesTreeView.Items[i].Data<>nil)and
              (TablesTreeView.Items[i].Level=0)then
              if(TObject(TablesTreeView.Items[i].Data) is TEERTable)then
              SelectedItemsList.Add(TablesTreeView.Items[i].Data);

          if(SelectedItemsList.Count>0)then
          begin
            //Dragged onto another table
            if(TargetTable<>nil)then
            begin
              if(TargetTable.OrderPos<TEERTable(SelectedItemsList[0]).OrderPos)then
                newpos:=TargetTable.OrderPos
              else
                newpos:=TargetTable.OrderPos+1;

              //Move all Objects with an equal higher OrderPos down
              for i:=0 to EERModel.ComponentCount-1 do
                if(EERModel.Components[i].ClassParent=TEERObj)then
                  if(TEERObj(EERModel.Components[i]).OrderPos>=newpos)then
                    TEERObj(EERModel.Components[i]).OrderPos:=
                      TEERObj(EERModel.Components[i]).OrderPos+SelectedItemsList.Count;


              for i:=0 to SelectedItemsList.Count-1 do
                TEERTable(SelectedItemsList[i]).OrderPos:=newpos+i;

              RefreshTablesTreeView(EERModel);
            end
            else
            begin
              //Move to last position
              newpos:=0;
              for i:=0 to EERModel.ComponentCount-1 do
                if(EERModel.Components[i].ClassParent=TEERObj)then
                  if(TEERObj(EERModel.Components[i]).OrderPos>=newpos)then
                  newpos:=TEERObj(EERModel.Components[i]).OrderPos+1;

              for i:=0 to SelectedItemsList.Count-1 do
                TEERTable(SelectedItemsList[i]).OrderPos:=newpos+i+1;

              RefreshTablesTreeView(EERModel);
            end;
          end;
        finally
          SelectedItemsList.Free;
        end;
      end;
    end;
end;

procedure TPaletteModelFrom.ReorderTablesbyNameMIClick(Sender: TObject);
var ObjList: TList;
  NumberList: TStringList;
  i: integer;
begin
  ObjList:=TList.Create;
  NumberList:=TStringList.Create;
  try
    EERModel.GetEERObjectList([EERTable], ObjList);
    EERModel.SortEERObjectListByObjName(ObjList);

    for i:=0 to ObjList.Count-1 do
      NumberList.Add(FormatFloat('00000000', TEERTable(ObjList[i]).OrderPos));

    NumberList.Sort;

    for i:=0 to ObjList.Count-1 do

      TEERTable(ObjList[i]).OrderPos:=StrToInt(NumberList[i]);

    RefreshTablesTreeView(EERModel);
  finally
    ObjList.Free;
    NumberList.Free;
  end;
end;

procedure TPaletteModelFrom.ReorderTablesbyRegionMIClick(Sender: TObject);
var ObjList, RegionList, AlreadySorted: TList;
  NumberList: TStringList;
  i, j, k: integer;
begin
  ObjList:=TList.Create;
  RegionList:=TList.Create;
  AlreadySorted:=TList.Create;
  NumberList:=TStringList.Create;
  try
    //Build NumberList
    EERModel.GetEERObjectList([EERTable], ObjList);
    EERModel.SortEERObjectListByObjName(ObjList);

    for i:=0 to ObjList.Count-1 do
      NumberList.Add(FormatFloat('00000000', TEERTable(ObjList[i]).OrderPos));

    NumberList.Sort;

    //Get regions
    EERModel.GetEERObjectList([EERRegion], RegionList);
    EERModel.SortEERObjectListByObjName(RegionList);

    k:=0;
    for i:=0 to RegionList.Count-1 do
    begin
      ObjList.Clear;
      TEERRegion(RegionList[i]).GetEERObjsInRegion([EERTable], ObjList, False);
      EERModel.SortEERObjectListByObjName(ObjList);

      for j:=0 to ObjList.Count-1 do
      begin
        //If the table has not already been sorted (on two regions for the same time)
        if(AlreadySorted.IndexOf(ObjList[j])=-1)then
        begin
          TEERTable(ObjList[j]).OrderPos:=StrToInt(NumberList[k]);
          AlreadySorted.Add(ObjList[j]);
          inc(k);
        end;
      end;
    end;

    //Now sort the tables which are on no region
    EERModel.GetEERObjectList([EERTable], ObjList);
    EERModel.SortEERObjectListByObjName(ObjList);

    for i:=0 to ObjList.Count-1 do
      if(AlreadySorted.IndexOf(ObjList[i])=-1)then
      begin
        TEERTable(ObjList[i]).OrderPos:=StrToInt(NumberList[k]);
        inc(k);
      end;

    RefreshTablesTreeView(EERModel);
  finally
    ObjList.Free;
    RegionList.Free;
    NumberList.Free;
    AlreadySorted.Free;
  end;
end;

end.
