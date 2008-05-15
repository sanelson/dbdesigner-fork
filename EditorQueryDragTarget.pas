unit EditorQueryDragTarget;

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
// Unit EditorQueryDragTarget.pas
// ------------------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   This Form provides Drop Targets for EERTables
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------


interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QExtCtrls, Main, EERModel;

type
  TEditorQueryDragTargetForm = class(TForm)
    DropSQLSelectPnl: TPanel;
    DropSQLAddTbls2SelectPnl: TPanel;
    DropSQLJoinSelectPnl: TPanel;
    DropSQLLeftJoinSelectPnl: TPanel;
    DropSQLUpdatePnl: TPanel;
    DropSQLInsertPnl: TPanel;
    DropSQLDeletePnl: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    DropColumn1Pnl: TPanel;
    DropColumn2Pnl: TPanel;
    DropColumn3Pnl: TPanel;
    DropColumn4Pnl: TPanel;
    DropColumn5Pnl: TPanel;
    DropColumn6Pnl: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure SetModel(theModel: TEERModel; ActiveSQLColumnPositions: integer);

    procedure DropSQLAddTbls2SelectPnlDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DropSQLAddTbls2SelectPnlDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure DropSQLJoinSelectPnlDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure DropSQLLeftJoinSelectPnlDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure DropSQLSelectPnlDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure DropSQLUpdatePnlDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure DropSQLInsertPnlDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure DropSQLDeletePnlDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure DropColumn1PnlDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DropColumn1PnlDragDrop(Sender, Source: TObject; X,
      Y: Integer);

    function GetColumnPos(thePanel: TPanel): integer;
  private
    { Private declarations }
    BackgroundBmp: TBitmap;
    SelColPnlBGBmp: TBitmap;

    theEERModel: TEERModel;
  public
    { Public declarations }
  end;

var
  EditorQueryDragTargetForm: TEditorQueryDragTargetForm;

implementation

uses EditorQuery, EERDM;


{$R *.xfm}

procedure TEditorQueryDragTargetForm.FormCreate(Sender: TObject);
begin
  BackgroundBmp:=TBitmap.Create;
  SelColPnlBGBmp:=TBitmap.Create;

  SelColPnlBGBmp.Assign(DropColumn1Pnl.Bitmap);

  DropColumn1Pnl.Bitmap.FreeImage;
  DropColumn1Pnl.Bitmap:=nil;
  DropColumn1Pnl.BevelOuter:=bvRaised;

  Height:=106;
end;

procedure TEditorQueryDragTargetForm.FormDestroy(Sender: TObject);
begin
  BackgroundBmp.Free;
  SelColPnlBGBmp.Free;
end;

procedure TEditorQueryDragTargetForm.FormPaint(Sender: TObject);
begin
  Canvas.Draw(0, 0, BackgroundBmp);
end;

procedure TEditorQueryDragTargetForm.FormShow(Sender: TObject);
var P, PMain: TPoint;
  BrightBmp: TBitmap;
  i, j: integer;
begin
  Left:=Mouse.CursorPos.X-Width div 2;
  if(Left<5)then
    Left:=5;
  if(Left+Width>Screen.Width-5)then
    Left:=Screen.Width-5-Width;

  Top:=Mouse.CursorPos.Y+5;
  if(Top<5)then
    Top:=5;
  if(Top+Height>Screen.Height-5)then
    Top:=Screen.Height-5-Height;

  BackgroundBmp.Width:=Width;
  BackgroundBmp.Height:=Height;

  P:=ClientToScreen(Point(0, 0));
  PMain:=Application.MainForm.ScreenToClient(P);
  BackgroundBmp.Canvas.Brush.Color:=clWhite;
  BackgroundBmp.Canvas.FillRect(Rect(0, 0, Width, Height));
  BackgroundBmp.Canvas.CopyRect(Rect(0, 0, Width, Height), Application.MainForm.Canvas,
    Rect(PMain.X-36, PMain.Y-1, PMain.X+Width-36, PMain.Y+Height-1));

  BrightBmp:=TBitmap.Create;
  try
    BrightBmp.Width:=Width;
    BrightBmp.Height:=Height;

    //Make BG Brigter
    BrightBmp.Canvas.Brush.Color:=$00A0A0A0;
    BrightBmp.Canvas.FillRect(Rect(0, 0, Width, Height));

    BackgroundBmp.Canvas.CopyMode:=cmSrcPaint;//cmMergePaint;
    BackgroundBmp.Canvas.Draw(0, 0, BrightBmp);

    //Make Shadows
    BrightBmp.Canvas.Brush.Color:=$00FFFFFF;
    BrightBmp.Canvas.FillRect(Rect(0, 0, Width, Height));
    BrightBmp.Canvas.Brush.Color:=$00F1F1F1;
    for i:=0 to 4 do
    begin
      for j:=0 to ComponentCount-1 do
        if(Components[j].ClassNameIs('TPanel'))then
            BrightBmp.Canvas.FillRect(Rect(
              TPanel(Components[j]).Left-1+i,
              TPanel(Components[j]).Top-1+i,
              TPanel(Components[j]).Left+TPanel(Components[j]).Width+4-i,
              TPanel(Components[j]).Top+TPanel(Components[j]).Height+4-i));

      BrightBmp.Canvas.Brush.Color:=
        BrightBmp.Canvas.Brush.Color-$000E0E0E;
    end;

    BackgroundBmp.Canvas.CopyMode:=cmMergeCopy;
    BackgroundBmp.Canvas.Draw(0, 0, BrightBmp);

  finally
    BrightBmp.Free;
  end;
end;

procedure TEditorQueryDragTargetForm.SetModel(theModel: TEERModel; ActiveSQLColumnPositions: integer);
var i: integer;
  fontColor: TColor;
begin
  theEERModel:=theModel;
  if(theEERModel.GetMouseOverSubObj<>nil)then
    fontColor:=clBlack
  else
    fontColor:=clGray;

  for i:=0 to ComponentCount-1 do
    if(Components[i].ClassNameIs('TPanel'))then
      if(Copy(TPanel(Components[i]).Name, 1, 10)='DropColumn')then
      begin
        TPanel(Components[i]).Font.Color:=fontColor;

        if(GetColumnPos(TPanel(Components[i]))=ActiveSQLColumnPositions)then
        begin
          TPanel(Components[i]).Bitmap:=SelColPnlBGBmp;
          TPanel(Components[i]).BevelOuter:=bvNone;

          TPanel(Components[i]).Repaint;
        end;
      end;

end;

procedure TEditorQueryDragTargetForm.DropSQLAddTbls2SelectPnlDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=False;
  if(Source<>nil)then
    if(Source.ClassnameIs('TEERTable'))then
      Accept:=True;
end;

procedure TEditorQueryDragTargetForm.DropSQLAddTbls2SelectPnlDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  //Select: New Select
  with TEditorQueryForm(MainForm.DockedEditorQueryForm) do
  begin
    AddTableToSQLCommand(TEERTable(Source), SQLctSELECT, SQLjtNONE);
  end;
end;

procedure TEditorQueryDragTargetForm.DropSQLJoinSelectPnlDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  //Select: Join Tables
  with TEditorQueryForm(MainForm.DockedEditorQueryForm) do
  begin
    AddTableToSQLCommand(TEERTable(Source), SQLctSELECT, SQLjtINNER);
  end;
end;

procedure TEditorQueryDragTargetForm.DropSQLLeftJoinSelectPnlDragDrop(
  Sender, Source: TObject; X, Y: Integer);
begin
  //Select: Join Tables
  with TEditorQueryForm(MainForm.DockedEditorQueryForm) do
  begin
    AddTableToSQLCommand(TEERTable(Source), SQLctSELECT, SQLjtLEFTOUTER);
  end;
end;

procedure TEditorQueryDragTargetForm.DropSQLSelectPnlDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  //Select: New Select
  with TEditorQueryForm(MainForm.DockedEditorQueryForm) do
  begin
    //Clear SQLMemo, so a new CMD is created
    SetSQLMemoText('');
    AddTableToSQLCommand(TEERTable(Source), SQLctSELECT, SQLjtINNER);
  end;
end;

procedure TEditorQueryDragTargetForm.DropSQLUpdatePnlDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  //Select: Join Tables
  with TEditorQueryForm(MainForm.DockedEditorQueryForm) do
  begin
    //Clear SQLMemo, so a new CMD is created
    SetSQLMemoText('');
    AddTableToSQLCommand(TEERTable(Source), SQLctUPDATE, 0);
  end;
end;

procedure TEditorQueryDragTargetForm.DropSQLInsertPnlDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  //Select: Join Tables
  with TEditorQueryForm(MainForm.DockedEditorQueryForm) do
  begin
    //Clear SQLMemo, so a new CMD is created
    SetSQLMemoText('');
    AddTableToSQLCommand(TEERTable(Source), SQLctINSERT, 0);
  end;
end;

procedure TEditorQueryDragTargetForm.DropSQLDeletePnlDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  //Select: Join Tables
  with TEditorQueryForm(MainForm.DockedEditorQueryForm) do
  begin
    //Clear SQLMemo, so a new CMD is created
    SetSQLMemoText('');
    AddTableToSQLCommand(TEERTable(Source), SQLctDELETE, 0);
  end;
end;

procedure TEditorQueryDragTargetForm.DropColumn1PnlDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var i: integer;
begin
  Accept:=False;
  if(Source<>nil)then
    if(Source.ClassnameIs('TEERTable'))then
      if(TEERModel(TEERTable(Source).Parent).GetMouseOverSubObj<>nil)then
      begin
        Accept:=True;

        if(TPanel(Sender).Bitmap<>SelColPnlBGBmp)then
        begin
          //Clear SelImgs
          for i:=0 to ComponentCount-1 do
            if(Components[i].ClassNameIs('TPanel'))and
              (Components[i]<>Sender)then
              if(Copy(TPanel(Components[i]).Name, 1, 10)='DropColumn')then
                if(TPanel(Components[i]).Bitmap.Width>0)then
                begin
                  //TPanel(Sender).Bitmap.FreeImage;
                  TPanel(Components[i]).Bitmap:=nil;
                  TPanel(Components[i]).BevelOuter:=bvRaised;

                  TPanel(Components[i]).Repaint;
                end;

          TPanel(Sender).Bitmap:=SelColPnlBGBmp;
          TPanel(Sender).BevelOuter:=bvNone;

          TPanel(Sender).Repaint;
        end;
      end;
end;

procedure TEditorQueryDragTargetForm.DropColumn1PnlDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  if(TEERModel(TEERTable(Source).Parent).GetMouseOverSubObj=nil)then
    Exit;

  {DMEER.ActiveSQLColumnDragPositions:=GetColumnPos(TPanel(Sender));

  TEditorQueryForm(MainForm.DockedEditorQueryForm).AddColumnToSQLCommand(
    DMEER.ActiveSQLColumnDragPositions,
    PEER_Col(TEERModel(TEERTable(Source).Parent).GetMouseOverSubObj));}
end;

function TEditorQueryDragTargetForm.GetColumnPos(thePanel: TPanel): integer;
begin
  GetColumnPos:=-1;

  case thePanel.Tag of
    1: GetColumnPos:=cpSelectClause;
    2: GetColumnPos:=cpWhereClause;
    3: GetColumnPos:=cpGroupClause;
    4: GetColumnPos:=cpHavingClause;
    5: GetColumnPos:=cpOrderClause;
  end;
end;

end.
