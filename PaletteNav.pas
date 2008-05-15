unit PaletteNav;

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
// Unit PaletteNav.pas
// -------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Contains a the navigator palette form class
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QComCtrls, QButtons, EERModel, Qt, StrUtils, QTypes,
  QMenus, EERDM;

type
  TPaletteNavForm = class(TForm)
    TrackBarTimer: TTimer;
    ModelImgChangTmr: TTimer;
    NavPopupMenu: TPopupMenu;
    Zoom100MI: TMenuItem;
    Zoom75MI: TMenuItem;
    Zoom50MI: TMenuItem;
    MainPnl: TPanel;
    BottomPnl: TPanel;
    Shape1: TShape;
    ZoomOutSBtn: TSpeedButton;
    ZoomInSBtn: TSpeedButton;
    Panel1: TPanel;
    ZoomShape: TShape;
    ZoomLbl: TLabel;
    ZoomEd: TEdit;
    ZoomTrackBar: TTrackBar;
    PageControl: TPageControl;
    InfoSheet: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    LeftEdit: TEdit;
    TopEdit: TEdit;
    NameEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    WidthEd: TEdit;
    Label5: TLabel;
    HeightEd: TEdit;
    NavSheet: TTabSheet;
    TableSheetTopPnl: TPanel;
    NavPnl: TPanel;
    NavPBox: TPaintBox;
    TabsPnl: TPanel;
    Tabs2Img: TImage;
    TabsImg: TImage;
    OptionsImg: TImage;
    Label6: TLabel;
    Bevel1: TBevel;
    LinkEd: TEdit;
    NavigatorPBox: TPaintBox;
    InfoPBox: TPaintBox;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NavPBoxPaint(Sender: TObject);

    procedure ZoomLblClick(Sender: TObject);
    procedure ZoomEdExit(Sender: TObject);
    procedure ZoomEdKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RefreshZoomSettings;
    procedure ZoomInSBtnClick(Sender: TObject);
    procedure ZoomOutSBtnClick(Sender: TObject);
    procedure ZoomTrackBarChange(Sender: TObject);
    procedure TrackBarTimerTimer(Sender: TObject);

    procedure RefreshInfo(theModel: TEERModel);

    procedure NameEditExit(Sender: TObject);
    procedure NameEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure LeftEditExit(Sender: TObject);
    procedure LeftEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure NavPBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NavPBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NavPBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);

    procedure SetModelImg(theModel: TEERModel);
    procedure ClearModelImg;
    procedure ModelImgChangTmrTimer(Sender: TObject);

    procedure OptionsImgClick(Sender: TObject);
    procedure Zoom100MIClick(Sender: TObject);
    procedure WidthEdExit(Sender: TObject);
    procedure WidthEdKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure NavigatorPBoxPaint(Sender: TObject);
    procedure NavigatorPBoxClick(Sender: TObject);
    procedure InfoPBoxClick(Sender: TObject);
  private
    { Private declarations }
    ZoomTrackBarActive: Boolean;

    mouse_absx,
    mouse_absy,
    mouse_posx,
    mouse_posy: integer;

    MouseIsDown: Boolean;

    ModelBmp: TBitmap;
  public
    { Public declarations }
    EERObj: TEERObj;
    EERModel: TEERModel;
  end;

var
  PaletteNavForm: TPaletteNavForm;

implementation

uses MainDM, Main, EER;

{$R *.xfm}

procedure TPaletteNavForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);

  ZoomTrackBarActive:=True;

  EERObj:=nil;

  MouseIsDown:=False;

  ModelBmp:=TBitmap.Create;

  PageControl.ActivePage:=NavSheet;
end;

procedure TPaletteNavForm.FormDestroy(Sender: TObject);
begin
  ModelBmp.Free;

  DMMain.SaveWinPos(self, False);
end;

procedure TPaletteNavForm.FormShow(Sender: TObject);
begin
  DMMain.RestoreWinPos(self, False);
end;

procedure TPaletteNavForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=False;

  Hide;

  TMainForm(Application.MainForm).NavigatorInfoMI.Checked:=False;
end;

procedure TPaletteNavForm.FormResize(Sender: TObject);
begin
  OptionsImg.Left:=Width-OptionsImg.Width-1;
  OptionsImg.BringToFront;
end;


procedure TPaletteNavForm.NavPBoxPaint(Sender: TObject);
var XFac, YFac: double;
  x, y, w, h: integer;
begin
  with NavPBox.Canvas do
  begin
    Pen.Color:=clWhite;
    Brush.Style:=bsSolid;
    Brush.Color:=clWhite;

    Rectangle(0, 0, width-1, height-1);

    if(Assigned(ModelBmp))then
      Draw(0, 0, ModelBmp);

    //When sender is set to nil, don't draw SelectionRect
    if(Sender<>nil)then
      if(MainForm.ActiveMDIChild<>nil)then
        if(MainForm.ActiveMDIChild.Classname='TEERForm')then
        begin
          Pen.Color:=clNavy;
          Pen.Width:=2;
          Brush.Style:=bsClear;

          XFac:=NavPBox.Width/MainForm.ActiveMDIChild.HorzScrollBar.Range;
          YFac:=NavPBox.Height/MainForm.ActiveMDIChild.VertScrollBar.Range;

          x:=Round(MainForm.ActiveMDIChild.HorzScrollBar.Position*XFac)+1;
          y:=Round(MainForm.ActiveMDIChild.VertScrollBar.Position*YFac)+1;
          w:=Round(MainForm.ActiveMDIChild.Width*XFac);
          h:=Round(MainForm.ActiveMDIChild.Height*YFac);

          if(x+w>NavPBox.Width-1)then
            w:=NavPBox.Width-x;

          if(y+h>NavPBox.Height-1)then
            h:=NavPBox.Height-y;

          Rectangle(x, y, x+w, y+h);
        end;
  end;

  {if(MainForm.ActiveMDIChild<>nil)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
    begin
      TEERForm(MainForm.ActiveMDIChild).EERModel.PaintModel(NavPBox.Canvas,
        (NavPBox.Width/TEERForm(MainForm.ActiveMDIChild).EERModel.EERModel_Width)*100*2,
        0, 0, 0, 0, [EERTable, EERRegion, EERNote, EERImage]);
    end;}
end;

procedure TPaletteNavForm.ZoomLblClick(Sender: TObject);
begin
  ZoomEd.Left:=0;

  if(MainForm.ActiveMDIChild<>nil)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
      ZoomEd.Text:=FormatFloat('##0.##', TEERForm(MainForm.ActiveMDIChild).EERModel.GetZoomFac)+ '%';

  ZoomEd.SelectAll;
  ZoomEd.Show;
  ZoomEd.SetFocus;
end;

procedure TPaletteNavForm.ZoomEdExit(Sender: TObject);
begin
  ZoomEd.Hide;

  Application.MainForm.SetFocus;
end;

procedure TPaletteNavForm.ZoomEdKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var theNewZoomFac: double;
begin
  if(Key=Key_Return)or(Key=Key_Enter)then
  begin
    ZoomEdExit(self);

    try
      if(RightStr(ZoomEd.Text, 1)='%')then
        ZoomEd.Text:=Copy(ZoomEd.Text, 1, Length(ZoomEd.Text)-1);

      theNewZoomFac:=StrToFloat(ZoomEd.Text);

      if(MainForm.ActiveMDIChild<>nil)then
        if(MainForm.ActiveMDIChild.Classname='TEERForm')then
          TEERForm(MainForm.ActiveMDIChild).EERModel.SetZoomFac(theNewZoomFac);
    except
    end;

  end;

  if(Key=Key_Escape)then
  begin
    ZoomEdExit(self);
  end;
end;

procedure TPaletteNavForm.RefreshZoomSettings;
begin
  if(MainForm.ActiveMDIChild<>nil)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
    begin
      ZoomLbl.Caption:=FormatFloat('##0.##', TEERForm(MainForm.ActiveMDIChild).EERModel.GetZoomFac)+ '%';
      ZoomTrackBarActive:=False;
      try
        ZoomTrackBar.Position:=Round(TEERForm(MainForm.ActiveMDIChild).EERModel.GetZoomFac);
      finally
        ZoomTrackBarActive:=True;
      end;
    end;
end;

procedure TPaletteNavForm.ZoomInSBtnClick(Sender: TObject);
begin
  if(MainForm.ActiveMDIChild<>nil)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
      TEERForm(MainForm.ActiveMDIChild).EERModel.ZoomIn(MainForm.ActiveMDIChild.Width div 2,
        MainForm.ActiveMDIChild.Height div 2);

  Application.MainForm.SetFocus;
end;

procedure TPaletteNavForm.ZoomOutSBtnClick(Sender: TObject);
begin
  if(MainForm.ActiveMDIChild<>nil)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
      TEERForm(MainForm.ActiveMDIChild).EERModel.ZoomOut(MainForm.ActiveMDIChild.Width div 2,
        MainForm.ActiveMDIChild.Height div 2);

  Application.MainForm.SetFocus;
end;

procedure TPaletteNavForm.ZoomTrackBarChange(Sender: TObject);
begin
  if(ZoomTrackBarActive)then
  begin
    TrackBarTimer.Enabled:=False;
    TrackBarTimer.Enabled:=True;
  end;
end;

procedure TPaletteNavForm.TrackBarTimerTimer(Sender: TObject);
begin
  TrackBarTimer.Enabled:=False;

  if(MainForm.ActiveMDIChild<>nil)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
      TEERForm(MainForm.ActiveMDIChild).EERModel.SetZoomFac(ZoomTrackBar.Position);
end;

procedure TPaletteNavForm.RefreshInfo(theModel: TEERModel);
begin
  if(theModel.GetSelectedObjsCount<>1)then
  begin
    EERObj:=nil;

    NameEdit.Enabled:=False;
    LeftEdit.Enabled:=False;
    TopEdit.Enabled:=False;
    WidthEd.Enabled:=False;
    HeightEd.Enabled:=False;

    NameEdit.Text:='';
    LeftEdit.Text:='';
    TopEdit.Text:='';
    WidthEd.Text:='';
    HeightEd.Text:='';

    LinkEd.Text:='';
  end
  else
  begin
    EERObj:=TEERObj(theModel.GetFirstSelectedObj);

    if(Assigned(EERObj))then
    begin
      NameEdit.Enabled:=True;
      if(Not(EERObj.ClassnameIs('TEERRel')))then
      begin
        LeftEdit.Enabled:=True;
        TopEdit.Enabled:=True;
      end
      else
      begin
        LeftEdit.Enabled:=False;
        TopEdit.Enabled:=False;
      end;

      if(EERObj.ClassnameIs('TEERImage'))then
      begin
        WidthEd.Enabled:=True;
        HeightEd.Enabled:=True;
      end
      else
      begin
        WidthEd.Enabled:=False;
        HeightEd.Enabled:=False;
      end;

      NameEdit.Text:=EERObj.ObjName;
      LeftEdit.Text:=IntToStr(EERObj.Obj_X);
      TopEdit.Text:=IntToStr(EERObj.Obj_Y);
      WidthEd.Text:=IntToStr(EERObj.Obj_W);
      HeightEd.Text:=IntToStr(EERObj.Obj_H);

      if(EERObj.IsLinkedObject)then
        LinkEd.Text:=theModel.GetPlacedModelByID(EERObj.IDLinkedModel).ModelName;
    end;
  end;
end;

procedure TPaletteNavForm.NameEditExit(Sender: TObject);
begin
  if(Assigned(EERObj))then
  begin
    if(NameEdit.Text<>EERObj.ObjName)then
    begin
      EERModel.LogAction(at_RenameObj, EERObj.Obj_id,
        'NewObjName='+NameEdit.Text+#13#10+
        'OldObjName='+EERObj.ObjName+#13#10);

      EERObj.ObjName:=NameEdit.Text;
      EERObj.RefreshObj;
      EERObj.DoPaint(self);
    end;
  end;
end;

procedure TPaletteNavForm.NameEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Assigned(EERObj))and
    ((Key=Key_Return)or(Key=Key_Enter))then
  begin
    if(NameEdit.Text<>EERObj.ObjName)then
    begin
      EERModel.LogAction(at_RenameObj, EERObj.Obj_id,
        'NewObjName='+NameEdit.Text+#13#10+
        'OldObjName='+EERObj.ObjName+#13#10);
            
      EERObj.ObjName:=NameEdit.Text;
      EERObj.RefreshObj;
      EERObj.DoPaint(self);
    end;

    PageControl.SetFocus;
  end;
end;

procedure TPaletteNavForm.NavPBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var XFac, YFac: double;
  w, h: integer;
begin
  if(MainForm.ActiveMDIChild<>nil)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
      if(Button=mbLeft)then
      begin
        mouse_absx:=Mouse.CursorPos.X;
        mouse_absy:=Mouse.CursorPos.Y;

        XFac:=NavPBox.Width/MainForm.ActiveMDIChild.HorzScrollBar.Range;
        YFac:=NavPBox.Height/MainForm.ActiveMDIChild.VertScrollBar.Range;

        {mouse_posx:=Round(MainForm.ActiveMDIChild.HorzScrollBar.Position*XFac)+1;
        mouse_posy:=Round(MainForm.ActiveMDIChild.VertScrollBar.Position*YFac)+1;}

        w:=Round(MainForm.ActiveMDIChild.Width*XFac);
        h:=Round(MainForm.ActiveMDIChild.Height*YFac);

        mouse_posx:=x-w div 2;
        mouse_posy:=y-h div 2;

        MouseIsDown:=True;

        NavPBoxMouseMove(Sender, [ssLeft], X, Y);
      end;
end;

procedure TPaletteNavForm.NavPBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var XFac, YFac: double;
  newXpos, newYpos: integer;
begin
  if(Shift=[ssLeft])and(MouseIsDown)then
  begin
    XFac:=NavPBox.Width/MainForm.ActiveMDIChild.HorzScrollBar.Range;
    YFac:=NavPBox.Height/MainForm.ActiveMDIChild.VertScrollBar.Range;

    newXpos:=Round((mouse_posx+Mouse.CursorPos.X-mouse_absx)/XFac);
    newYpos:=Round((mouse_posy+Mouse.CursorPos.Y-mouse_absy)/YFac);

    if(newXpos<0)then
      newXpos:=0;

    if(newYpos<0)then
      newYpos:=0;

    if(newXpos>MainForm.ActiveMDIChild.HorzScrollBar.Range-MainForm.ActiveMDIChild.Width)then
      newXpos:=MainForm.ActiveMDIChild.HorzScrollBar.Range-MainForm.ActiveMDIChild.Width;

    if(newYpos>MainForm.ActiveMDIChild.VertScrollBar.Range-MainForm.ActiveMDIChild.Height)then
      newYpos:=MainForm.ActiveMDIChild.VertScrollBar.Range-MainForm.ActiveMDIChild.Height;


    MainForm.ActiveMDIChild.HorzScrollBar.Position:=newXpos;
    MainForm.ActiveMDIChild.VertScrollBar.Position:=newYpos;

    NavPBoxPaint(self);
  end;
end;

procedure TPaletteNavForm.NavPBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseIsDown:=False;

  Application.MainForm.SetFocus;
end;

procedure TPaletteNavForm.ClearModelImg;
begin
  EERModel:=nil;

  NavPBoxPaint(self);
end;

procedure TPaletteNavForm.SetModelImg(theModel: TEERModel);
var PreviewZoomFactor: double;
begin
  if(theModel<>nil)then
  begin
    EERModel:=theModel;

    //Set Bitmap size
    ModelBmp.Width:=NavPBox.Width;
    PreviewZoomFactor:=ModelBmp.Width/EERModel.EERModel_Width;
    ModelBmp.Height:=Round(EERModel.EERModel_Height*PreviewZoomFactor);


    //Draw to Model onto the Bitmap
    ModelBmp.Canvas.Pen.Color:=clWhite;
    ModelBmp.Canvas.Brush.Color:=clWhite;
    ModelBmp.Canvas.Rectangle(Rect(0, 0, ModelBmp.Width-1, ModelBmp.Height-1));

    //Paint with no text output
    EERModel.PaintModel(ModelBmp.Canvas,
      PreviewZoomFactor*100,
      0, 0, 0, 0, [EERTable, EERRegion, EERNote], 72, False);

    NavPBoxPaint(self);
  end
  else
  begin
    //Draw to Model onto the Bitmap
    ModelBmp.Canvas.Pen.Color:=clWhite;
    ModelBmp.Canvas.Brush.Color:=clWhite;
    ModelBmp.Canvas.Rectangle(Rect(0, 0, ModelBmp.Width-1, ModelBmp.Height-1));

    NavPBoxPaint(nil);
  end;
end;

procedure TPaletteNavForm.ModelImgChangTmrTimer(Sender: TObject);
begin
  if(MainForm.ActiveMDIChild<>nil)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
      if(TEERForm(MainForm.ActiveMDIChild).EERModel.Need2RefreshNavImg)then
      begin
        TEERForm(MainForm.ActiveMDIChild).EERModel.Need2RefreshNavImg:=False;
        SetModelImg(TEERForm(MainForm.ActiveMDIChild).EERModel);
      end;
end;

procedure TPaletteNavForm.OptionsImgClick(Sender: TObject);
begin
  NavPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TPaletteNavForm.Zoom100MIClick(Sender: TObject);
begin
  if(MainForm.ActiveMDIChild<>nil)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
      TEERForm(MainForm.ActiveMDIChild).EERModel.SetZoomFac(TMenuItem(Sender).Tag, 0, 0);
end;

procedure TPaletteNavForm.LeftEditExit(Sender: TObject);
begin
  if(Assigned(EERObj))then
  begin
    try
      StrToInt(LeftEdit.Text);
      StrToInt(TopEdit.Text);
    except
      LeftEdit.Text:=IntToStr(EERObj.Obj_X);
      TopEdit.Text:=IntToStr(EERObj.Obj_Y);
    end;

    EERModel.StartSubActionLog(at_MoveObj);
    EERModel.LogSubAction(sa_MoveFrom, EERObj.Obj_id,
      'Obj_X='+IntToStr(EERObj.Obj_X)+#13#10+'Obj_Y='+IntToStr(EERObj.Obj_Y));
    EERModel.LogSubAction(sa_MoveTo, EERObj.Obj_id,
      'Obj_X='+LeftEdit.Text+#13#10+'Obj_Y='+TopEdit.Text);
    EERModel.EndSubAction;

    EERObj.Obj_X:=StrToInt(LeftEdit.Text);
    EERObj.Obj_Y:=StrToInt(TopEdit.Text);
    EERObj.RefreshObj;
  end;
end;

procedure TPaletteNavForm.LeftEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Assigned(EERObj))and
    ((Key=Key_Return)or(Key=Key_Enter))then
  begin
    LeftEditExit(self);
  end;
end;

procedure TPaletteNavForm.WidthEdExit(Sender: TObject);
begin
  if(Assigned(EERObj))then
  begin
    try
      StrToInt(WidthEd.Text);
      StrToInt(HeightEd.Text);
    except
      WidthEd.Text:=IntToStr(EERObj.Obj_W);
      HeightEd.Text:=IntToStr(EERObj.Obj_H);
    end;

    EERModel.StartSubActionLog(at_ScaleObj);
    EERModel.LogSubAction(sa_ScaleFrom, EERObj.Obj_id,
      'Obj_W='+IntToStr(EERObj.Obj_W)+#13#10+'Obj_H='+IntToStr(EERObj.Obj_H));
    EERModel.LogSubAction(sa_ScaleTo, EERObj.Obj_id,
      'Obj_W='+WidthEd.Text+#13#10+'Obj_H='+HeightEd.Text);
    EERModel.EndSubAction;

    EERObj.Obj_W:=StrToInt(WidthEd.Text);
    EERObj.Obj_H:=StrToInt(HeightEd.Text);
    EERObj.RefreshObj;
  end;
end;

procedure TPaletteNavForm.WidthEdKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Assigned(EERObj))and
    ((Key=Key_Return)or(Key=Key_Enter))then
  begin
    WidthEdExit(self);
  end;
end;

procedure TPaletteNavForm.FormDeactivate(Sender: TObject);
begin
  if(Visible)then
    if(Not(DMMain.IsFormStayingOnTop(self)))then
      sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));
end;

procedure TPaletteNavForm.NavigatorPBoxPaint(Sender: TObject);
begin
  //Paint Text (for XTF smooth fonts)
  TPaintBox(Sender).Canvas.Font.Color:=clBlack;
  TPaintBox(Sender).Canvas.TextOut(0, 0,
    DMMain.GetTranslatedMessage('', TPaintBox(Sender).Tag));
end;

procedure TPaletteNavForm.NavigatorPBoxClick(Sender: TObject);
begin
  TabsImg.BringToFront;
  NavigatorPBox.BringToFront;
  InfoPBox.BringToFront;
  PageControl.ActivePage:=NavSheet;
  BottomPnl.Show;
end;

procedure TPaletteNavForm.InfoPBoxClick(Sender: TObject);
begin
  Tabs2Img.BringToFront;
  NavigatorPBox.BringToFront;
  InfoPBox.BringToFront;
  PageControl.ActivePage:=InfoSheet;
  BottomPnl.Hide;
end;

end.
