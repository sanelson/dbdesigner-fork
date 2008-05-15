unit PaletteTools;

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
//   Contains a the floating tools palette form class
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QButtons, Qt;

type
  TPaletteToolsForm = class(TForm)
    HeaderImg: TImage;
    wtPointerSBtn: TSpeedButton;
    wtRegionSBtn: TSpeedButton;
    wtHandSBtn: TSpeedButton;
    wtZoomSBtn: TSpeedButton;
    wtRel11SBtn: TSpeedButton;
    wtRel1nSBtn: TSpeedButton;
    wtTableSBtn: TSpeedButton;
    wtNoteSBtn: TSpeedButton;
    wtMoveSBtn: TSpeedButton;
    wtRel1nSubSBtn: TSpeedButton;
    wtRelnmSBtn: TSpeedButton;
    wtSizeSBtn: TSpeedButton;
    wtDeleteSBtn: TSpeedButton;
    wtRel11SubSBtn: TSpeedButton;
    wtImageSBtn: TSpeedButton;
    RevImg: TImage;
    SyncImg: TImage;
    QueryImg: TImage;
    Designimg: TImage;
    CreatesImg: TImage;
    procedure FormCreate(Sender: TObject);
    procedure HeaderImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HeaderImgMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure HeaderImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure wtPointerSBtnClick(Sender: TObject);
    procedure wtTableSBtnClick(Sender: TObject);
    procedure wtRel11SBtnClick(Sender: TObject);
    procedure wtRel1nSBtnClick(Sender: TObject);
    procedure wtRegionSBtnClick(Sender: TObject);
    procedure wtNoteSBtnClick(Sender: TObject);
    procedure wtHandSBtnClick(Sender: TObject);
    procedure wtZoomSBtnClick(Sender: TObject);
    procedure wtRelnmSBtnClick(Sender: TObject);
    procedure wtMoveSBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure wtRel1nSubSBtnClick(Sender: TObject);
    procedure wtDeleteSBtnClick(Sender: TObject);
    procedure wtSizeSBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure wtRel11SubSBtnClick(Sender: TObject);
    procedure wtImageSBtnClick(Sender: TObject);
    procedure DesignimgClick(Sender: TObject);
    procedure QueryImgClick(Sender: TObject);
    procedure RevImgClick(Sender: TObject);
    procedure SyncImgClick(Sender: TObject);
    procedure CreatesImgClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    posx, posy, absx, absy: integer;
    MouseIsDown: Boolean;
  public
    { Public declarations }
  end;

var
  PaletteToolsForm: TPaletteToolsForm;

implementation

uses MainDM, Main, EERDM;

{$R *.xfm}

procedure TPaletteToolsForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);
  
  width:=58;
  height:=309;

  if(DMEER.WorkMode=wmQuery)then
    QueryImg.BringToFront;
end;

procedure TPaletteToolsForm.HeaderImgMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  posx:=Left;
  posy:=Top;
  absx:=Mouse.CursorPos.X;
  absy:=Mouse.CursorPos.Y;

  MouseIsDown:=True;
end;

procedure TPaletteToolsForm.HeaderImgMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if(MouseIsDown)then
  begin
    Left:=posx+Mouse.CursorPos.X-absx;
    Top:=posy+Mouse.CursorPos.Y-absy;
  end;
end;

procedure TPaletteToolsForm.HeaderImgMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseIsDown:=False;
end;

procedure TPaletteToolsForm.wtPointerSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtPointer);
end;

procedure TPaletteToolsForm.wtTableSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtTable);
end;

procedure TPaletteToolsForm.wtRel11SBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtRel11);
end;

procedure TPaletteToolsForm.wtRel1nSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtRel1n);
end;

procedure TPaletteToolsForm.wtRel1nSubSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtRel1nSub);
end;

procedure TPaletteToolsForm.wtRegionSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtRegion);
end;

procedure TPaletteToolsForm.wtNoteSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtNote);
end;

procedure TPaletteToolsForm.wtHandSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtHand);
end;

procedure TPaletteToolsForm.wtZoomSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtZoomIn);
end;

procedure TPaletteToolsForm.wtRelnmSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtRelnm);
end;

procedure TPaletteToolsForm.wtMoveSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtMove);
end;

procedure TPaletteToolsForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=False;
  
  Hide;

  TMainForm(Application.MainForm).ToolsMI.Checked:=False;
end;

procedure TPaletteToolsForm.wtDeleteSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtDelete);
end;
               
procedure TPaletteToolsForm.wtSizeSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtSize);
end;

procedure TPaletteToolsForm.FormDestroy(Sender: TObject);
begin
  DMMain.SaveWinPos(self, False);
end;

procedure TPaletteToolsForm.FormShow(Sender: TObject);
begin
  DMMain.RestoreWinPos(self, False);
end;

procedure TPaletteToolsForm.wtRel11SubSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtRel11Sub);
end;

procedure TPaletteToolsForm.wtImageSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtImage);
end;

procedure TPaletteToolsForm.DesignimgClick(Sender: TObject);
begin
  MainForm.SetWorkMode(wmQuery);
end;

procedure TPaletteToolsForm.QueryImgClick(Sender: TObject);
begin
  MainForm.SetWorkMode(wmDesign);
end;

procedure TPaletteToolsForm.RevImgClick(Sender: TObject);
begin
  MainForm.ReverseEngineeringMIClick(self);
end;

procedure TPaletteToolsForm.SyncImgClick(Sender: TObject);
begin
  MainForm.DatabasesyncronisationMIClick(self);
end;

procedure TPaletteToolsForm.CreatesImgClick(Sender: TObject);
begin
  MainForm.SQLCreateScriptMIClick(self);
end;

procedure TPaletteToolsForm.FormDeactivate(Sender: TObject);
begin
  if(Visible)then
    if(Not(DMMain.IsFormStayingOnTop(self)))then
      sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));
end;

end.
