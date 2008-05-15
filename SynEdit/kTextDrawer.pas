{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEdit.pas, released 2000-04-07.
The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: kTextDrawer.pas,v 1.7 2003/07/02 05:27:25 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
unit kTextDrawer;

{$I SynEdit.inc}

interface

uses
  Types,
  Qt,
  QGraphics,
  QForms,
  QControls,
  Classes,
  QStdCtrls;

const
  ETO_OPAQUE = 1;
  ETO_CLIPPED = 2;

  CYHSCROLL = 16;
  CXVSCROLL = 16;
  MaxShortInt = High(ShortInt);

// system metics
  SM_CXDRAG = 1;
  SM_CYDRAG = 2;

// Clipboard formats
  CF_TEXT = 'text/plain';

type
  UINT = DWORD;

  // this is to get around some weirdness with the Kylix font code
  // where the style given by the font object isn't correct
  TFontHolder = class
    style : TFontStyles;
    font  : TFont;

    constructor Create(aFont : TFont; aStyle : TFontStyles);
  end;

  TheTextDrawer = class(TObject)
  private
    // Font information
    fBaseFont : TFont;

    // current font and properties
    fCurrentStyle : TFontStyles;
    fCurrentFont  : TFont;
    fCurrentColor : TColor;

    // current font attributes
    FBkColor: TColor;
    fCharWidth : integer;
    fCharHeight : integer;
    fakeBold : boolean;  // true if the font can't be bold

    // Begin/EndDrawing calling count
    FDrawingCount: Integer;
    fCanvas: TCanvas;

    fFontList : TList;
    function getFont(index: integer): TFont;
    function getFontCount: integer;
    function getFontStyle(index: integer): TFontStyles;
    function findFont(aStyle : TFontStyles; aColor : TColor):TFont;

    property Fonts[index : integer] : TFont read getFont;
    property FontCount:integer read getFontCount;
    property FontStyle[index : integer] : TFontStyles read getFontStyle;
  protected
    procedure UpdateCurrentFont;
    procedure UpdateFontMetrics;
    procedure ClearFontList;
  public
    constructor Create(CalcExtentBaseStyle: TFontStyles; ABaseFont: TFont); virtual;
    destructor Destroy; override;
    function GetCharWidth: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    procedure BeginDrawing(ACanvas : TCanvas); overload; virtual;

    procedure EndDrawing; virtual;
    procedure TextOut(X, Y: Integer; Text: PChar; Length: Integer); virtual;
    procedure ExtTextOut(X, Y: Integer; fuOptions: UINT; const ARect: TRect;
      Text: PChar; Length: Integer); virtual;
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetBaseStyle(const Value: TFontStyles); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;
    procedure SetForeColor(Value: TColor); virtual;
    procedure SetBackColor(Value: TColor); virtual;
    procedure SetCharExtra(Value: Integer); virtual;
    property CharWidth: Integer read GetCharWidth;
    property CharHeight: Integer read GetCharHeight;
    property BaseFont: TFont write SetBaseFont;
    property BaseStyle: TFontStyles write SetBaseStyle;
    property ForeColor: TColor write SetForeColor;
    property BackColor: TColor write SetBackColor;
    property Style: TFontStyles write SetStyle;
  end;

  TCaret = class(TGraphicControl)
  private
    fActive: boolean;
    procedure SetActive(const Value: boolean);
  protected
    procedure Blink(b : boolean);
  public
    constructor Create(AParent : TWidgetControl); reintroduce;
    destructor Destroy; override;
    procedure Paint; override;
    property Active: boolean read fActive  write SetActive;
  end;

  TSynEditScrollBar = class(TScrollBar)
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure InternalFillRect(canvas : TCanvas; rect : TRect);
function GetSystemMetrics(metric : integer):integer;

procedure CreateCaret(control : TWidgetControl; dummy, cw, ch : integer);
procedure SetCaretPos(x, y : integer);
procedure ShowCaret(control : TWidgetControl);
procedure HideCaret(control : TWidgetControl);
function  FindCaret(control : TWidgetControl):TCaret;
procedure DestroyCaret;
procedure ScrollCaret(aControl: TWidgetControl; aDeltaX, aDeltaY: integer);

implementation

uses
{$IFDEF SYN_KYLIX}
  libc,
{$ENDIF}
  QExtCtrls;

type
  TCaretManager = class
  private
    fBlinkTimer: TTimer;
    fCurrentCaret: TCaret;
    fCaretVisible: boolean;
    procedure HandleTimer(Sender : TObject);
    procedure SetCurrentCaret(const Value: TCaret);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ResetCaret;
    property CurrentCaret: TCaret read fCurrentCaret write SetCurrentCaret;
  end;

var
  CaretManager : TCaretManager;


function  FindCaret(control : TWidgetControl):TCaret;
var
  i : integer;

begin
  result := nil;

  for i:=0 to control.ControlCount-1 do
    if control.Controls[i] is TCaret then
      begin
        result := TCaret(control.Controls[i]);
        break;
      end;
end;

procedure CreateCaret(control : TWidgetControl; dummy, cw, ch : integer);
var
  aCaret : TCaret;
begin
  aCaret := FindCaret(control);

  if aCaret = nil then
    aCaret := TCaret.Create(control)
  else
    aCaret.Active := False;

  aCaret.Width := cw;
  aCaret.Height := ch;
  CaretManager.CurrentCaret := aCaret;
end;

procedure DestroyCaret;
var
  iOldCaret: TCaret;
begin
  if CaretManager.CurrentCaret <> nil then
  begin
    iOldCaret := CaretManager.CurrentCaret;
    CaretManager.CurrentCaret := nil;
    iOldCaret.Free;
  end;
end;

procedure ScrollCaret(aControl: TWidgetControl; aDeltaX, aDeltaY: integer);
var
  aCaret: TCaret;
begin
  aCaret := FindCaret( aControl );
  if aCaret <> nil then
  begin
    { fortunately these fields are not private in CLX! }
    Inc( aCaret.FLeft, aDeltaX );
    Inc( aCaret.FTop, aDeltaY );
  end;
end;

procedure SetCaretPos(x, y : integer);
var
  iCaret: TCaret;
begin
  iCaret := CaretManager.CurrentCaret;
  if (iCaret <> nil) and ( (iCaret.Left <> x) or (iCaret.Top <> y) ) then
  begin
    iCaret.Update;
    iCaret.SetBounds( x, y, iCaret.Width, iCaret.Height );
    if iCaret.Active then
    begin
      CaretManager.ResetCaret;
      iCaret.Refresh;
    end;
  end;
end;

procedure ShowCaret(control : TWidgetControl);
var
  aCaret : TCaret;
begin
  aCaret := FindCaret(control);
  if aCaret <> nil then
    aCaret.Active := True;
end;

procedure HideCaret(control : TWidgetControl);
var
  aCaret : TCaret;
begin
  aCaret := FindCaret(control);
  if aCaret <> nil then
    aCaret.Active := False;
end;

procedure InternalFillRect(canvas : TCanvas; rect : TRect);
begin
  canvas.FillRect(rect);
end;

function GetSystemMetrics(metric : integer):integer;
begin
  case metric of
    SM_CXDRAG : result := 2;
    SM_CYDRAG : result := 2;
  else
    result := -1;
  end;
end;

{ TheTextDrawer }

procedure TheTextDrawer.BeginDrawing(ACanvas: TCanvas);
begin
  if fDrawingCount=0 then
    begin
      UpdateCurrentFont;
      fCanvas := ACanvas;
      fCanvas.Font:=fCurrentFont;
       
      fCanvas.Brush.Color:=FBkColor;
    end;

  inc(FDrawingCount);
end;

procedure TheTextDrawer.ClearFontList;
var
  i : integer;

begin
  for i:=0 to FontCount-1 do
    begin
      Fonts[i].Free;
      TFontHolder(fFontList[i]).Free;
    end;

  fFontList.Clear;
end;

constructor TheTextDrawer.Create(CalcExtentBaseStyle: TFontStyles; ABaseFont: TFont);
begin
  fBaseFont := TFont.Create;
  fFontList := TList.Create;

  BaseFont:=ABaseFont;
  BaseStyle:=CalcExtentBaseStyle;
end;

destructor TheTextDrawer.Destroy;
begin
  fBaseFont.Free;
  ClearFontList;
  fFontList.Free;

  inherited Destroy;
end;

procedure TheTextDrawer.EndDrawing;
begin
  if FDrawingCount>0 then
    dec(FDrawingCount);

  if FDrawingCount=0 then
    fCanvas:=nil;
end;

procedure TheTextDrawer.ExtTextOut(X, Y: Integer; fuOptions: UINT;
  const ARect: TRect; Text: PChar; Length: Integer);
begin
  if fCanvas<>nil then
    begin
//      fCanvas.Brush.Color := random($ffffff);
      fCanvas.Brush.Style := bsSolid;
      fCanvas.FillRect(ARect);

      if Text<>nil then
        begin
          if Length=-1 then
            fCanvas.TextRect(ARect,X, Y, Text)
          else
            fCanvas.TextRect(ARect,X, Y, copy(Text,1,Length));

          if fakeBold then
            if Length=-1 then
              fCanvas.TextRect(ARect,X+1, Y, Text)
            else
              fCanvas.TextRect(ARect,X+1, Y, copy(Text,1,Length));
        end;
    end;
end;

function TheTextDrawer.findFont(aStyle: TFontStyles; aColor : TColor): TFont;
var
  i : integer;

begin
  result:=nil;

  for i:=0 to FontCount-1 do
    if (FontStyle[i] = aStyle) and (Fonts[i].Color = aColor) then
      begin
        result:=Fonts[i];
        break;
      end;

  if result=nil then
    begin
      result:=TFont.Create;
      result.assign(fBaseFont);
      result.style:=aStyle;
      result.color:=aColor;
      fFontList.Add(TFontHolder.Create(result,aStyle));
    end;
end;

function TheTextDrawer.GetCharHeight: Integer;
begin
  result := fCharHeight;
end;

function TheTextDrawer.GetCharWidth: Integer;
begin
  result := fCharWidth;
end;

function TheTextDrawer.getFont(index: integer): TFont;
begin
  result:=TFontHolder(fFontList[index]).font;
end;

function TheTextDrawer.getFontCount: integer;
begin
  result:=fFontList.Count;
end;

function TheTextDrawer.getFontStyle(index: integer): TFontStyles;
begin
  result:=TFontHolder(fFontList[index]).style;
end;

procedure TheTextDrawer.SetBackColor(Value: TColor);
begin
  if FBkColor <> Value then
  begin
    FBkColor := Value;

    if fCanvas<>nil then
      fCanvas.Brush.Color:=Value;
  end;
end;

procedure TheTextDrawer.SetBaseFont(Value: TFont);
begin
  if Value<>nil then
    begin
      fBaseFont.Assign(Value);
      UpdateFontMetrics;
      UpdateCurrentFont;
    end;
end;

procedure TheTextDrawer.SetBaseStyle(const Value: TFontStyles);
begin
  fBaseFont.Style := Value;
  UpdateFontMetrics;
end;

procedure TheTextDrawer.SetCharExtra(Value: Integer);
begin
  // do nothing
end;

procedure TheTextDrawer.SetForeColor(Value: TColor);
begin
  fCurrentColor:=Value;
  UpdateCurrentFont;
end;

procedure TheTextDrawer.SetStyle(Value: TFontStyles);
begin
  fCurrentStyle := Value;
  UpdateCurrentFont;
end;

procedure TheTextDrawer.TextOut(X, Y: Integer; Text: PChar;
  Length: Integer);
begin
  if fCanvas<>nil then
    begin
      fCanvas.Brush.Style := bsSolid;

      if Text<>nil then
        begin
          if Length=-1 then
            fCanvas.TextOut(X, Y, Text)
          else
            fCanvas.TextOut(X, Y, copy(Text,1,Length));


          if fakeBold then
            if Length=-1 then
              fCanvas.TextOut(X+1, Y, Text)
            else
              fCanvas.TextOut(X+1, Y, copy(Text,1,Length));
        end;
    end;
end;

procedure TheTextDrawer.UpdateCurrentFont;
begin
  fCurrentFont:=findFont(fCurrentStyle, fCurrentColor);
  QFont_setFixedPitch(fCurrentFont.Handle, TRUE);

  if fCanvas<>nil then
    begin
      fCanvas.Font.Assign(fCurrentFont);
    end;

  // Make sure that we can draw bold text even if the current font
  // doesn't want to do it
  fakeBold := (fsBold in fCurrentStyle) and not (fsBold in fCurrentFont.Style);
end;

procedure TheTextDrawer.UpdateFontMetrics;
var
  fm     : QFontMetricsH;
  ch     : WideChar;
  w      : integer;
  fi     : QFontInfoH;
  family : WideString;

begin
  fi := QFontInfo_create(fBaseFont.Handle);
  try
    // make sure that the font object is refering to the same
    // font that Qt is actually using, otherwise the width functions
    // don't seem to work properly :(
    if not QFontInfo_exactMatch(fi) then
      begin
        fBaseFont.Size := QFontInfo_pointSize(fi);
        QFontInfo_family(fi, @family);
        fBaseFont.Name := family;
      end;
  finally
    QFontInfo_destroy(fi);
  end;

  fm := QFontMetrics_create(fBaseFont.Handle);
  try
    ch := 'W';

    w := QFontMetrics_width(fm, @ch);
    fCharWidth := w;
    fCharHeight := QFontMetrics_height(fm);
  finally
    QFontMetrics_destroy(fm);
  end;

  ClearFontList;
end;

{ TCaret }

procedure TCaret.Blink(b: boolean);
begin
  Visible := b and Active;
end;

constructor TCaret.Create(AParent: TWidgetControl);
begin
  inherited Create(AParent);
  Visible := False;
  ControlStyle := [csOpaque];
  Cursor := AParent.Cursor;
  Parent := AParent;
end;

destructor TCaret.Destroy;
begin
  if CaretManager.CurrentCaret = self then
    CaretManager.CurrentCaret := nil;
  inherited Destroy;
end;

procedure TCaret.SetActive(const Value: boolean);
begin
  if fActive <> Value then
  begin
    fActive := Value;
    Visible := Value;
  end;
end;

procedure TCaret.Paint;
var
  iBounds: TRect;
begin
  iBounds := BoundsRect; { we're painting on Parent's Canvas... }
  Canvas.CopyMode := cmDstInvert;
  Canvas.CopyRect( iBounds, Canvas, iBounds );
end;

{ TFontHolder }

constructor TFontHolder.Create(aFont: TFont; aStyle: TFontStyles);
begin
  font:=aFont;
  style:=aStyle;
end;

{ TCaretManager }

constructor TCaretManager.Create;
begin
  fBlinkTimer := TTimer.Create(nil);
  fBlinkTimer.Interval := QApplication_cursorFlashTime div 2;
  fBlinkTimer.OnTimer := HandleTimer;
end;

destructor TCaretManager.Destroy;
begin
  fBlinkTimer.Free;

  inherited Destroy;
end;

procedure TCaretManager.HandleTimer(Sender: TObject);
begin
  if CurrentCaret<>nil then
  begin
    fCaretVisible := not fCaretVisible;
    CurrentCaret.Blink( fCaretVisible );
  end;
end;

procedure TCaretManager.ResetCaret;
begin
  CurrentCaret.Visible := True;
  fCaretVisible := True;
  { restart timer }
  fBlinkTimer.Enabled := False;
  fBlinkTimer.Enabled := True;
end;

procedure TCaretManager.SetCurrentCaret(const Value: TCaret);
begin
  if fCurrentCaret <> Value then
  begin
    if CurrentCaret <> nil then
      CurrentCaret.Active := False;
    fCurrentCaret := Value;
    fBlinkTimer.Enabled := CurrentCaret <> nil;
  end;
end;

{ TSynEditScrollBar }

constructor TSynEditScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoFocus];
  Visible := False;
end;

initialization
  CaretManager := TCaretManager.Create;
finalization
  CaretManager.Free;
end.