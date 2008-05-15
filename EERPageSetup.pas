unit EERPageSetup;

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
// Unit EERPageSetup
// ---------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Contains the TEERPageSetupForm Class which will allow the user to print
//   the EERModel
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QPrinters, QExtCtrls, QButtons, EERModel, QComCtrls,
  Math, Qt;

type
  TEERPageSetupForm = class(TForm)
    BottomPnl: TPanel;
    CenterPnl: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    MainGroupBox: TGroupBox;
    PrinterPnl: TPanel;
    WidthPnl: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    PaintBox: TPaintBox;
    Image1: TImage;
    PropertiesBtn: TButton;
    PageSizeGroupBox: TGroupBox;
    PageSizeCBox: TComboBox;
    OrientationGroupBox: TGroupBox;
    LandscapeImg: TImage;
    PortraitImg: TImage;
    LandscapeRBtn: TRadioButton;
    PortraitRBtn: TRadioButton;
    HPageSizeTBar: TTrackBar;
    WidthLbl: TLabel;
    Panel3: TPanel;
    PagesPnl: TPanel;
    GroupBox6: TGroupBox;
    HPagesSpinEdit: TSpinEdit;
    VPagesSpinEdit: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    PrintPnl: TPanel;
    GroupBox7: TGroupBox;
    PrintAllPagesCBox: TRadioButton;
    PrintSelPagesCBox: TRadioButton;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    CopiesEd: TSpinEdit;
    StartPrintBtn: TBitBtn;
    PrintDlgBtn: TBitBtn;
    PageSetupBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure SetModel(theModel: TEERModel);
    procedure SetSelPagesArraySize;
    procedure ResetSelPages;

    procedure PrinterCBoxCloseUp(Sender: TObject);
    procedure PropertiesBtnClick(Sender: TObject);
    procedure PageSizeCBoxCloseUp(Sender: TObject);

    function GetPageSizeStr(nr: TPageSize): string;
    function GetPageSizeNr(s: string): TPageSize;
    function GetPageSize(s: string): TSize;
    procedure ResizeEERModelPages;

    procedure SetPrinterValues;
    procedure PortraitRBtnClick(Sender: TObject);
    procedure StartPrintBtnClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PrintAllPagesCBoxClick(Sender: TObject);
    procedure HPageSizeTBarChange(Sender: TObject);
    procedure HPagesSpinEditChanged(Sender: TObject; NewValue: Integer);
    procedure FormShow(Sender: TObject);
    procedure VPagesSpinEditChanged(Sender: TObject; NewValue: Integer);

    procedure HideEdits;
    procedure ShowEdits;

    procedure SetModelSelPages;
    procedure StoreSelPagesInModel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PrintDlgBtnClick(Sender: TObject);
    procedure PageSetupBtnClick(Sender: TObject);

  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    EERModel: TEERModel;

    ModelBmp: TBitmap;

    //SelPages: Array of Array of Boolean;
    SelPages: Array [0..51, 0..51] of Boolean;
    lastPageX, lastPageY: integer;

    PreviewZoomFactor: double;

    SizeTBarActive,
    SpinEditActive: Boolean;
  end;

var
  EERPageSetupForm: TEERPageSetupForm;

implementation

uses MainDM;

{$R *.xfm}

procedure TEERPageSetupForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);
  
  ModelBmp:=TBitmap.Create;

  lastPageX:=0;
  lastPageY:=0;

  PreviewZoomFactor:=1;

  SizeTBarActive:=False;
  SpinEditActive:=False;
end;

procedure TEERPageSetupForm.FormDestroy(Sender: TObject);
begin
  ModelBmp.Free;
end;

procedure TEERPageSetupForm.FormShow(Sender: TObject);
begin
  if(Assigned(EERModel))then
  begin
    //Set Dialog Height
    if(Not(PrintPnl.Visible))then
      ClientHeight:=Round(EERModel.EERModel_Height*PreviewZoomFactor+160)
    else
      ClientHeight:=Round(EERModel.EERModel_Height*PreviewZoomFactor+70);
      
    MainGroupBox.Align:=alNone;
    MainGroupBox.Align:=alClient;
  end;
end;

procedure TEERPageSetupForm.SetModel(theModel: TEERModel);
var PrinterName: WideString;
begin
  EERModel:=theModel;

  //Set Bitmap size
  ModelBmp.Width:=PaintBox.Width-4;
  PreviewZoomFactor:=ModelBmp.Width/EERModel.EERModel_Width;
  ModelBmp.Height:=Round(EERModel.EERModel_Height*PreviewZoomFactor);


  //Draw to Model onto the Bitmap
  ModelBmp.Canvas.Pen.Color:=clWhite;
  ModelBmp.Canvas.Brush.Color:=clWhite;
  ModelBmp.Canvas.Rectangle(Rect(0, 0, ModelBmp.Width-1, ModelBmp.Height-1));

  //No Text Output
  EERModel.PaintModel(ModelBmp.Canvas,
    PreviewZoomFactor*100,
    0, 0, 0, 0, [EERTable, EERRegion, EERNote, EERRelation],
    72, False);

  if(EERModel.ModelPrinter<>'')then
  begin
    //!!! This isn't working at the moment
    PrinterName:=EERModel.ModelPrinter;
    QPrinter_setPrinterName(QPrinterH(Printer.Handle), PWideString(@PrinterName));

    Printer.SetPrinter(EERModel.ModelPrinter);
  end;


  //Set Spin Edits and TBars
  HPagesSpinEdit.Value:=Ceil(EERModel.HPageCount-0.01);

  HPageSizeTBar.Max:=Round(EERModel.EERModel_Width*PreviewZoomFactor);
  HPageSizeTBar.Position:=Round(EERModel.PageSize.cx*PreviewZoomFactor);

  //Initialize Selection Array
  SetSelPagesArraySize;

  //Clear Selection Array
  //ResetSelPages;

  //Set Pages as stores in Model
  SetModelSelPages;

  //Set Printer Orientation
  Printer.Orientation:=EERModel.PageOrientation;
  Printer.PrintAdapter.PageSize:=GetPageSizeNr(EERModel.PageFormat);

  SetPrinterValues;

  //Activate OnChange Events of Edits
  SizeTBarActive:=True;
  SpinEditActive:=True;

end;

procedure TEERPageSetupForm.SetSelPagesArraySize;
//var i: integer;
begin
  {SetLength(SelPages, HPagesSpinEdit.Value);
  for i:=0 to HPagesSpinEdit.Value-1 do
    SetLength(SelPages[i], VPagesSpinEdit.Value);}
end;

procedure TEERPageSetupForm.ResetSelPages;
var i, j: integer;
begin
  for i:=0 to Ceil(EERModel.HPageCount-0.01)-1 do
    for j:=0 to Ceil(EERModel.VPageCount-0.01)-1 do
      SelPages[i, j]:=False;
end;

procedure TEERPageSetupForm.SetModelSelPages;
var i, j: integer;
begin
  for i:=0 to Ceil(EERModel.HPageCount-0.01)-1 do
    for j:=0 to Ceil(EERModel.VPageCount-0.01)-1 do
    begin
      SelPages[i, j]:=EERModel.SelectedPages[i+(Ceil(EERModel.HPageCount-0.01)-1)*j];
      if(SelPages[i, j])then
        PrintSelPagesCBox.Checked:=True;
    end;
end;

procedure TEERPageSetupForm.StoreSelPagesInModel;
var i, j: integer;
begin
  for i:=0 to Ceil(EERModel.HPageCount-0.01)-1 do
    for j:=0 to Ceil(EERModel.VPageCount-0.01)-1 do
      EERModel.SelectedPages[i+(Ceil(EERModel.HPageCount-0.01)-1)*j]:=SelPages[i, j];
end;

procedure TEERPageSetupForm.PropertiesBtnClick(Sender: TObject);
begin
  if(Printer.PrintAdapter.ExecuteSetup)then
    SetPrinterValues;
end;

procedure TEERPageSetupForm.PrinterCBoxCloseUp(Sender: TObject);
begin
  //Printer.SetPrinter(PrinterCBox.Items[PrinterCBox.ItemIndex]);
end;

procedure TEERPageSetupForm.SetPrinterValues;
begin
  //Display Printer Values after changes

  //PrinterCBox.ItemIndex:=PrinterCBox.Items.IndexOf(Printer.OutputDevice);

  PageSizeCBox.ItemIndex:=PageSizeCBox.Items.IndexOf(
    GetPageSizeStr(Printer.PrintAdapter.PageSize));

  case Printer.Orientation of
    poPortrait:
    begin
      PortraitRBtn.Visible:=False;
      PortraitRBtn.Checked:=True;
      PortraitRBtn.Visible:=True;
      PortraitImg.Show;
      LandscapeImg.Hide;
    end;
    poLandscape:
    begin
      LandscapeRBtn.Visible:=False;
      LandscapeRBtn.Checked:=True;
      LandscapeRBtn.Visible:=True;
      PortraitImg.Hide;
      LandscapeImg.Show;
    end;
  end;

  // Set Model PageSettings
  ResizeEERModelPages;

  PaintBoxPaint(self);
end;

procedure TEERPageSetupForm.ResizeEERModelPages;
var PrinterName: WideString;
begin
  //Calculate Page Settings of the EERModel
  //from EERModel.PageSize.cx Value
  if(Printer.PageWidth-Printer.Margins.cx*2-1)>
    (Printer.PageHeight-Printer.Margins.cy*2-1)then
    EERModel.PageAspectRatio:=(Printer.PageWidth-Printer.Margins.cx*2-1)/
      (Printer.PageHeight-Printer.Margins.cy*2-1)
  else
    EERModel.PageAspectRatio:=(Printer.PageHeight-Printer.Margins.cy*2-1)/
      (Printer.PageWidth-Printer.Margins.cx*2-1);

  QPrinter_PrinterName(QPrinterH(Printer.Handle), PWideString(@PrinterName));
  EERModel.ModelPrinter:=PrinterName;
  EERModel.PageOrientation:=Printer.Orientation;

  if(EERModel.PageOrientation=poPortrait)then
    EERModel.PageSize.cy:=Round(EERModel.PageSize.cx*EERModel.PageAspectRatio)
  else
    EERModel.PageSize.cy:=Round(EERModel.PageSize.cx/EERModel.PageAspectRatio);

  EERModel.HPageCount:=EERModel.EERModel_Width/EERModel.PageSize.cx;
  EERModel.VPageCount:=EERModel.EERModel_Height/EERModel.PageSize.cy;

  //Refresh Spin Edits
  SpinEditActive:=False;
  try
    HPagesSpinEdit.Value:=Ceil(EERModel.HPageCount-0.01);
    VPagesSpinEdit.Value:=Ceil(EERModel.VPageCount-0.01);
  finally
    SpinEditActive:=True;
  end;

  //Refresh TBar
  SizeTBarActive:=False;
  try
    HPageSizeTBar.Position:=Round(EERModel.PageSize.cx*PreviewZoomFactor);
  finally
    SizeTBarActive:=True;
  end;
end;

function TEERPageSetupForm.GetPageSizeStr(nr: TPageSize): string;
begin
  case nr of
    psA0:
      GetPageSizeStr:='A0 (841 x 1189 mm)';
    psA1:
      GetPageSizeStr:='A1 (594 x 841 mm)';
    psA2:
      GetPageSizeStr:='A2 (420 x 594 mm)';
    psA3:
      GetPageSizeStr:='A3 (297 x 420 mm)';
    psA4:
      GetPageSizeStr:='A4 (210x297 mm, 8.26x11.7 inches)';
    psA5:
      GetPageSizeStr:='A5 (148 x 210 mm)';
    psA6:
      GetPageSizeStr:='A6 (105 x 148 mm)';
    psA7:
      GetPageSizeStr:='A7 (74 x 105 mm)';
    psA8:
      GetPageSizeStr:='A8 (52 x 74 mm)';
    psA9:
      GetPageSizeStr:='A9 (37 x 52 mm)';
    psB0:
      GetPageSizeStr:='B0 (1030 x 1456 mm)';
    psB1:
      GetPageSizeStr:='B1 (728 x 1030 mm)';
    psB10:
      GetPageSizeStr:='B10 (32 x 45 mm)';
    psB2:
      GetPageSizeStr:='B2 (515 x 728 mm)';
    psB3:
      GetPageSizeStr:='B3 (364 x 515 mm)';
    psB4:
      GetPageSizeStr:='B4 (257 x 364 mm)';
    psB5:
      GetPageSizeStr:='B5 (182x257 mm, 7.17x10.13 inches)';
    psB6:
      GetPageSizeStr:='B6 (128 x 182 mm)';
    psB7:
      GetPageSizeStr:='B7 (91 x 128 mm)';
    psB8:
      GetPageSizeStr:='B8 (64 x 91 mm)';
    psB9:
      GetPageSizeStr:='B9 (45 x 64 mm)';
    psC5E:
      GetPageSizeStr:='C5E (163 x 229 mm)';
    psComm10E:
      GetPageSizeStr:='Comm10E (105 x 241 mm, US Common #10 Envelope)';
    psDLE:
      GetPageSizeStr:='DLE (110 x 220 mm)';
    psExecutive:
      GetPageSizeStr:='Executive (7.5x10 inches, 191x254 mm)';
    psFolio:
      GetPageSizeStr:='Folio (210 x 330 mm)';
    psLedger:
      GetPageSizeStr:='Ledger (432 x 279 mm)';
    psLegal:
      GetPageSizeStr:='Legal (8.5x14 inches, 216x356 mm)';
    psLetter:
      GetPageSizeStr:='Letter (8.5x11 inches, 216x279 mm)';
    psTabloid:
      GetPageSizeStr:='Tabloid (279 x 432 mm)';
  else
    GetPageSizeStr:='A4 (210x297 mm, 8.26x11.7 inches)';
  end;
end;

function TEERPageSetupForm.GetPageSizeNr(s: string): TPageSize;
begin
  GetPageSizeNr:=psNPageSize;

  if(s='A0 (841 x 1189 mm)')then
    GetPageSizeNr:=psA0;
  if(s='A1 (594 x 841 mm)')then
    GetPageSizeNr:=psA1;
  if(s='A2 (420 x 594 mm)')then
    GetPageSizeNr:=psA2;
  if(s='A3 (297 x 420 mm)')then
    GetPageSizeNr:=psA3;
  if(s='A4 (210x297 mm, 8.26x11.7 inches)')then
    GetPageSizeNr:=psA4;
  if(s='A5 (148 x 210 mm)')then
    GetPageSizeNr:=psA5;
  if(s='A6 (105 x 148 mm)')then
    GetPageSizeNr:=psA6;
  if(s='A7 (74 x 105 mm)')then
    GetPageSizeNr:=psA7;
  if(s='A8 (52 x 74 mm)')then
    GetPageSizeNr:=psA8;
  if(s='A9 (37 x 52 mm)')then
    GetPageSizeNr:=psA9;
  if(s='B0 (1030 x 1456 mm)')then
    GetPageSizeNr:=psB0;
  if(s='B1 (728 x 1030 mm)')then
    GetPageSizeNr:=psB1;
  if(s='B10 (32 x 45 mm)')then
    GetPageSizeNr:=psB10;
  if(s='B2 (515 x 728 mm')then
    GetPageSizeNr:=psB2;
  if(s='B3 (364 x 515 mm)')then
    GetPageSizeNr:=psB3;
  if(s='B4 (257 x 364 mm)')then
    GetPageSizeNr:=psB4;
  if(s='B5 (182x257 mm, 7.17x10.13 inches)')then
    GetPageSizeNr:=psB5;
  if(s='B6 (128 x 182 mm)')then
    GetPageSizeNr:=psB6;
  if(s='B7 (91 x 128 mm)')then
    GetPageSizeNr:=psB4;
  if(s='B8 (64 x 91 mm)')then
    GetPageSizeNr:=psB5;
  if(s='B9 (45 x 64 mm)')then
    GetPageSizeNr:=psB6;
  if(s='B9 (45 x 64 mm)')then
    GetPageSizeNr:=psB6;
  if(s='C5E (163 x 229 mm)')then
    GetPageSizeNr:=psC5E;
  if(s='Comm10E (105 x 241 mm, US Common #10 Envelope)')then
    GetPageSizeNr:=psComm10E;
  if(s='DLE (110 x 220 mm)')then
    GetPageSizeNr:=psDLE;
  if(s='Executive (7.5x10 inches, 191x254 mm)')then
    GetPageSizeNr:=psExecutive;
  if(s='Folio (210 x 330 mm)')then
    GetPageSizeNr:=psFolio;
  if(s='Ledger (432 x 279 mm)')then
    GetPageSizeNr:=psLedger;
  if(s='Legal (8.5x14 inches, 216x356 mm)')then
    GetPageSizeNr:=psLegal;
  if(s='Letter (8.5x11 inches, 216x279 mm)')then
    GetPageSizeNr:=psLetter;
  if(s='Tabloid (279 x 432 mm)')then
    GetPageSizeNr:=psTabloid;
end;

function TEERPageSetupForm.GetPageSize(s: string): TSize;
var oldsep: char;
begin
  oldsep:=DecimalSeparator;

  try
    DecimalSeparator:='.';

    s:=Copy(s, Pos('(', s)+1, Length(s));

    try
      //mm
      GetPageSize.cx:=StrToInt(Trim(Copy(s, 1, Pos('x', s)-1)));
    except
      //Inch
      GetPageSize.cx:=Round(StrToFloat(Trim(Copy(s, 1, Pos('x', s)-1)))*2.54*10);
    end;

    s:=Trim(Copy(s, Pos('x', s)+1, Length(s)));

    if(Pos('m', s)<Pos('i', s))or(Pos('i', s)=0)then
    begin
      //mm
      GetPageSize.cy:=StrToInt(Trim(Copy(s, 1, Pos(' ', s)-1)));
    end
    else
    begin
      //Inch
      GetPageSize.cy:=Round(StrToFloat(Trim(Copy(s, 1, Pos(' ', s)-1)))*2.54*10);
    end;
  finally
    DecimalSeparator:=oldsep;
  end;
end;

procedure TEERPageSetupForm.PageSizeCBoxCloseUp(Sender: TObject);
begin
  //If another Paper Format is selected
  Printer.PrintAdapter.PageSize:=GetPageSizeNr(PageSizeCBox.Items[PageSizeCBox.ItemIndex]);

  SetPrinterValues;
end;

procedure TEERPageSetupForm.PortraitRBtnClick(Sender: TObject);
begin
  //if orientation has changed
  if(PortraitRBtn.Checked)then
    Printer.Orientation:=poPortrait
  else
    Printer.Orientation:=poLandscape;

  SetPrinterValues;
end;

procedure TEERPageSetupForm.StartPrintBtnClick(Sender: TObject);
var i, j, SelCount: integer;
  firstPage: Boolean;
  PrintZoomFac: double;
  CutMarksLength: integer;
begin
  //Check if at least one page is selected
  if(not(PrintAllPagesCBox.Checked))then
  begin
    SelCount:=0;

    for i:=0 to Ceil(EERModel.HPageCount-0.01)-1 do
      for j:=0 to Ceil(EERModel.VPageCount-0.01)-1 do
        if(SelPages[i, j])then
          inc(SelCount);

    if(SelCount=0)then
    begin
      ShowMessage(DMMain.GetTranslatedMessage('You have to select at least one page.', 212));
      Exit;
    end;
  end;

  with Printer do
  begin
    Title:='EERModel '+EERModel.ModelFilename;

    firstPage:=True;

    PrintZoomFac:=(PageWidth-Margins.cx*2-1-2)/EERModel.PageSize.cx;

    BeginDoc;

    try
      for j:=0 to Ceil(EERModel.VPageCount-0.01)-1 do
      begin
        for i:=0 to Ceil(EERModel.HPageCount-0.01)-1 do
        begin
          if(SelPages[i, j])or
            (PrintAllPagesCBox.Checked)then
          begin
            //Check if this is not the first page to print and
            //if it's not, create a new page
            if(not(firstPage))then
              NewPage;

            firstPage:=False;

            with Canvas do
            begin
              SetClipRect(Rect(1, 1, PageWidth-Margins.cx*2-1, PageHeight-Margins.cy*2-1));

              EERModel.PaintModel(Printer.Canvas,
                PrintZoomFac*100,
                Round((EERModel.PageSize.cx*i+1)*PrintZoomFac),
                Round((EERModel.PageSize.cy*j+1)*PrintZoomFac),
                Round((EERModel.PageSize.cx*(i+1)+1)*PrintZoomFac),
                Round((EERModel.PageSize.cy*(j+1)+1)*PrintZoomFac),
                [EERNote, EERRegion, EERRelation, EERTable, EERImage],
                XDPI);

              Pen.Color:=clBlack;
              Pen.Style:=psSolid;

              CutMarksLength:=Round(10*(XDPI/72));
              Pen.Width:=Round((XDPI/72));

              //TopLeft
              MoveTo(CutMarksLength, 0);
              LineTo(0, 0);
              LineTo(0, CutMarksLength);

              //TopRight
              MoveTo(PageWidth-Margins.cx*2-1-CutMarksLength, 0);
              LineTo(PageWidth-Margins.cx*2-1, 0);
              LineTo(PageWidth-Margins.cx*2-1, CutMarksLength);

              //BottomRight
              MoveTo(PageWidth-Margins.cx*2-1-CutMarksLength, PageHeight-Margins.cy*2-1);
              LineTo(PageWidth-Margins.cx*2-1, PageHeight-Margins.cy*2-1);
              LineTo(PageWidth-Margins.cx*2-1, PageHeight-Margins.cy*2-1-CutMarksLength);

              //BottomLeft
              MoveTo(CutMarksLength, PageHeight-Margins.cy*2-1);
              LineTo(0, PageHeight-Margins.cy*2-1);
              LineTo(0, PageHeight-Margins.cy*2-1-CutMarksLength);

              Font.Height:=12;
              TextOut(Round(4*(XDPI/72)),
                Round(4*(XDPI/72)),
                IntToStr(i+1+Ceil(EERModel.HPageCount-0.01)*j));
            end;
          end;
        end;
      end;
    finally
      EndDoc;
    end;
  end;

  ModalResult:=mrOK;
end;

procedure TEERPageSetupForm.PaintBoxPaint(Sender: TObject);
var
  i, j: integer;
  w, h: double;
begin
  with PaintBox.Canvas do
  begin
    Pen.Color:=clGray;
    Brush.Color:=clWhite;
    Brush.Style:=bsSolid;
    Rectangle(Rect(0, 0, PaintBox.Width, PaintBox.Height));

    Draw(2, 2, ModelBmp);

    Pen.Color:=clNavy;
    Brush.Style:=bsClear;

    Pen.Style:=psSolid;

    w:=EERModel.PageSize.cx*PreviewZoomFactor;
    h:=EERModel.PageSize.cy*PreviewZoomFactor;

    for i:=0 to Ceil(EERModel.HPageCount-0.01)-1 do
      for j:=0 to Ceil(EERModel.VPageCount-0.01)-1 do
      begin
        Rectangle(Rect(2+Round(w*i),
          2+Round(h*j),
          2+Round(w*(i+1))+1,
          2+Round(h*(j+1))+1));

        TextOut(2+Round(w*i)+2,
          2+Round(h*j)+2,
          IntToStr(i+1+Ceil(EERModel.HPageCount-0.01)*j));
      end;



    for i:=0 to Ceil(EERModel.HPageCount-0.01)-1 do
      for j:=0 to Ceil(EERModel.VPageCount-0.01)-1 do
        if(SelPages[i, j])then
        begin
          Pen.Color:=clWhite;
          Pen.Style:=psSolid;
          Rectangle(Rect(2+Round(w*i),
            2+Round(h*j),
            2+Round(w*(i+1))+1,
            2+Round(h*(j+1))+1));

          Pen.Color:=clNavy;
          Pen.Style:=psDot;
          Rectangle(Rect(2+Round(w*i),
            2+Round(h*j),
            2+Round(w*(i+1))+1,
            2+Round(h*(j+1))+1));
        end;

    Pen.Style:=psSolid;
  end;
end;

procedure TEERPageSetupForm.PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var w, h: integer;
  i, j: integer;
begin
  w:=Round(EERModel.PageSize.cx*PreviewZoomFactor);
  h:=Round(EERModel.PageSize.cy*PreviewZoomFactor);


  if(not(ssCtrl in Shift))then
    ResetSelPages;

  if(not(ssShift in Shift))then
    SelPages[X div w, Y div h]:=Not(SelPages[X div w, Y div h])
  else
  begin
    for i:=lastPageX to X div w do
      for j:=lastPageY to Y div h do
        SelPages[i, j]:=True;
  end;

  lastPageX:=X div w;
  lastPageY:=Y div h;

  PaintBoxPaint(self);

  PrintSelPagesCBox.Checked:=True;
end;

procedure TEERPageSetupForm.PrintAllPagesCBoxClick(Sender: TObject);
begin
  ResetSelPages;

  PaintBoxPaint(self);
end;

procedure TEERPageSetupForm.HPageSizeTBarChange(Sender: TObject);
begin
  if(SizeTBarActive)then
  begin
    EERModel.PageSize.cx:=Round(HPageSizeTBar.Position/PreviewZoomFactor);
    ResizeEERModelPages;

    PaintBoxPaint(self);
  end;
end;

procedure TEERPageSetupForm.HPagesSpinEditChanged(Sender: TObject;
  NewValue: Integer);
begin
  if(SpinEditActive)then
  begin
    EERModel.PageSize.cx:=Round(EERModel.EERModel_Width/HPagesSpinEdit.Value);
    ResizeEERModelPages;

    PaintBoxPaint(self);
  end;
end;

procedure TEERPageSetupForm.VPagesSpinEditChanged(Sender: TObject;
  NewValue: Integer);
begin
  if(SpinEditActive)then
  begin
    if(EERModel.PageOrientation=poPortrait)then
      EERModel.PageSize.cx:=Round((EERModel.Height/VPagesSpinEdit.Value)/EERModel.PageAspectRatio)
    else
      EERModel.PageSize.cx:=Round((EERModel.Height/VPagesSpinEdit.Value)*EERModel.PageAspectRatio);

    ResizeEERModelPages;

    PaintBoxPaint(self);
  end;
end;

procedure TEERPageSetupForm.HideEdits;
begin
  WidthLbl.Hide;
  HPageSizeTBar.Hide;
  WidthPnl.Height:=16;

  PageSizeGroupBox.Hide;
  OrientationGroupBox.Hide;
  PrinterPnl.Height:=8;

  PagesPnl.Hide;
  PrintPnl.Show;

  Caption:=DMMain.GetTranslatedMessage('Print', 213);
end;

procedure TEERPageSetupForm.ShowEdits;
begin
  WidthLbl.Show;
  HPageSizeTBar.Show;
  WidthPnl.Height:=40;

  PageSizeGroupBox.Show;
  OrientationGroupBox.Show;
  PrinterPnl.Height:=74;

  PagesPnl.Show;
  PrintPnl.Hide;

  Caption:=DMMain.GetTranslatedMessage('Page Setup', 214);
end;

procedure TEERPageSetupForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  StoreSelPagesInModel;

  if(Assigned(EERModel))then
    EERModel.GridPaintBox.Refresh;
end;

procedure TEERPageSetupForm.PrintDlgBtnClick(Sender: TObject);
begin
  HideEdits;

  FormShow(self);
end;

procedure TEERPageSetupForm.PageSetupBtnClick(Sender: TObject);
begin
  ShowEdits;

  FormShow(self);
end;

end.
