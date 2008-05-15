//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the SimpleWebFront-DBDesigner4-Plugin.
// Copyright (C) 2003 Michael Zinner,Bayer Ulrich
//
// The SimpleWebFront-DBDesigner4-Plugin is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// SimpleWebFront-DBDesigner4-Plugin is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------

unit DialogImageSelection;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QExtCtrls, QButtons, QComCtrls, QFileCtrls, QGrids;

type
  TImageSelectionForm = class(TForm)
    ImgDrawGrid: TDrawGrid;
    DirectoryTreeView: TDirectoryTreeView;
    Label1: TLabel;
    Label2: TLabel;
    SubmitBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetData(InitialDir: string);
    procedure ImgDrawGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DirectoryTreeViewItemClick(Sender: TObject;
      Button: TMouseButton; Node: TTreeNode; const Pt: TPoint);
    procedure ImgDrawGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure SubmitBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ImgDrawGridDblClick(Sender: TObject);
  private
    { Private declarations }
    ImgList: TList;
    ImageFileNameList: TStringList;
  public
    { Public declarations }
    selectedPicture: TPicture;
    selectedFileName: string;
  end;

var
  ImageSelectionForm: TImageSelectionForm;

implementation

{$R *.xfm}

procedure TImageSelectionForm.FormCreate(Sender: TObject);
begin
  ImgList:=TList.Create;
  ImageFileNameList:=TStringList.Create;
  {$IFDEF LINUX}
    DirectoryTreeView.RootDirectory := '/';
  {$ENDIF}
end;

procedure TImageSelectionForm.FormDestroy(Sender: TObject);
begin
  ImgList.Free;
  ImageFileNameList.Free;
end;

procedure TImageSelectionForm.SetData(InitialDir: string);
begin
  //Always end with /
  if(Copy(InitialDir, Length(InitialDir), 1)<>PathDelim)then
    InitialDir:=InitialDir+PathDelim;

  DirectoryTreeView.Directory:=InitialDir;
  DirectoryTreeViewItemClick(self, mbLeft, nil, Point(0, 0));
end;

procedure TImageSelectionForm.ImgDrawGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var pos: integer;
  theRect: TRect;
begin
  pos:=ACol+ARow*6;

  if(pos<ImgList.Count)then
  begin
    ImgDrawGrid.Canvas.Brush.Color:=clWhite;
    ImgDrawGrid.Canvas.Pen.Color:=clWhite;
    ImgDrawGrid.Canvas.Rectangle(Rect);

    theRect.Left:=Rect.Left+1;
    theRect.Top:=Rect.Top+1;
    theRect.Right:=theRect.Left+64;
    theRect.Bottom:=theRect.Top+64;
    ImgDrawGrid.Canvas.StretchDraw(theRect, TPicture(ImgList[pos]).Bitmap);

    theRect.Bottom:=theRect.Bottom+14;
    ImgDrawGrid.Canvas.TextRect(theRect, theRect.Left, theRect.Top+64+1, ExtractFileName(ImageFileNameList[Pos]));
  end;
end;

procedure TImageSelectionForm.DirectoryTreeViewItemClick(Sender: TObject;
  Button: TMouseButton; Node: TTreeNode; const Pt: TPoint);
var i: integer;
  sr: TSearchRec;
  thePic: TPicture;
  CanSelect: Boolean;
  var searchMask : String;
begin
  ImgDrawGrid.RowCount:=0;

  //Clear old images
  for i:=0 to ImgList.Count-1 do
    TPicture(ImgList[i]).Free;
  ImgList.Clear;
  ImageFileNameList.Clear;



  //Get Images
  {$IFDEF MSWINDOWS}
    searchMask := IncludeTrailingPathDelimiter(DirectoryTreeView.Directory)+'*.*';
  {$ELSE}
    searchMask := IncludeTrailingPathDelimiter(DirectoryTreeView.Directory)+'*';
  {$ENDIF}
  if FindFirst(searchMask, faDirectory, sr) = 0 then
  begin
    repeat
      if (sr.Attr and faDirectory) = 0 then
      begin
        //Ignore . and ..
        if(Copy(sr.name, 1, 1)='.')then
          continue;

        if(CompareText(ExtractFileExt(sr.name), '.png')=0)or
          (CompareText(ExtractFileExt(sr.name), '.bmp')=0)then
        begin
          thePic:=TPicture.Create;
          thePic.LoadFromFile(IncludeTrailingPathDelimiter(DirectoryTreeView.Directory)+sr.name);

          ImgList.Add(thePic);
          ImageFileNameList.Add(IncludeTrailingPathDelimiter(DirectoryTreeView.Directory)+sr.name);
        end;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  ImgDrawGrid.RowCount:=Trunc((ImgList.Count+5)/6);
  if(ImgList.Count<6)then
    ImgDrawGrid.ColCount:=ImgList.Count
  else
    ImgDrawGrid.ColCount:=6;

  ImgDrawGrid.Refresh;
  ImgDrawGridSelectCell(Self, 0, 0, CanSelect);
end;

procedure TImageSelectionForm.ImgDrawGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  SubmitBtn.Enabled:=(ACol+ARow*6<ImgList.Count);
  if(SubmitBtn.Enabled)then
  begin
    selectedPicture:=TPicture(ImgList[ACol+ARow*6]);
    selectedFileName:=ImageFileNameList[ACol+ARow*6];
  end;
end;

procedure TImageSelectionForm.SubmitBtnClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TImageSelectionForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TImageSelectionForm.ImgDrawGridDblClick(Sender: TObject);
begin
  if(SubmitBtn.Enabled)then
    SubmitBtnClick(self);
end;

end.
