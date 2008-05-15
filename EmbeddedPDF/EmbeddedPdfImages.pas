unit EmbeddedPdfImages;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of fabFORCE EmbeddedPDF.
// Copyright (c) 1999-2001 T.KANNO. <takeshi_kanno@est.hi-ho.ne.jp>
// Copyright (C) 2003 Michael G. Zinner, www.fabFORCE.net
//
// EmbeddedPDF is free software; you can redistribute it and/or modify
// it under the terms of the GNU Library General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// EmbeddedPDF is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Library General Public License for more details.
//
// You should have received a copy of the GNU Library General Public License
// along with EmbeddedPDF; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------
//
// Unit PdfImages.pas
// ------------------
// Version 1.0, 12.09.2003, Mike
// Description
//   Contains the TPdfBitmapImage Class
//
// Changes:
//   Version 1.0, 12.09.2003, Mike
//     adapted version from Takezou's PowerPDF, Version 0.9 (beta), 2001.09.01
//
//----------------------------------------------------------------------------------------------------------------------


interface

{$I EmbeddedPdfConf.inc}

uses
  SysUtils, {$IFNDEF USE_CLX}Windows, Graphics, {$ELSE}QGraphics, Qt, {$ENDIF}
  Classes, EmbeddedPdfTypes, EmbeddedPdfDoc;

type
  TPdfImageCreator = class(TPersistent)
  public
    function CreateImage(AImage: TGraphic): TPdfImage; virtual;
  end;

  TPdfBitmapImage = class(TPdfImageCreator)
  private
    function CreateIndexedColorArray(ABitmap: TBitmap): TPdfArray;
  public
    function CreateImage(AImage: TGraphic): TPdfImage; override;
  end;

  EPdfInvalidImageFormat = class(Exception);

  function CreatePdfImage(AImage: TGraphic; ImageClassName: string): TPdfImage;

implementation

function CreatePdfImage(AImage: TGraphic; ImageClassName: string): TPdfImage;
var
  PdfImageCreator: TPdfImageCreator;
begin
  Result := nil;
  PdfImageCreator := TPdfImageCreator(FindClass(ImageClassName).Create);
  try
    if PdfImageCreator = nil then
      raise Exception.CreateFmt('AddImage --InvalidImageClassName:%s', [ImageClassName]);
    Result := PdfImageCreator.CreateImage(AImage);
  finally
    PdfImageCreator.Free;
  end;
end;

{ TPdfImageCreator }
function TPdfImageCreator.CreateImage(AImage: TGraphic): TPdfImage;
begin
  result := nil;
end;

{$IFDEF USE_CLX}
type
  TColorTable = array[0..MaxInt div SizeOf(QRgb)-1] of QRgb;
  PColorTable = ^TColorTable;
{$ENDIF}

function TPdfBitmapImage.CreateIndexedColorArray(ABitmap: TBitmap): TPdfArray;
var
  {$IFNDEF USE_CLX}
  PalEntries: array[0..255] of TPaletteEntry;
  {$ELSE}
  PalEntries: PColorTable;
  CRgb: Cardinal;
  pb: PByteArray;
  {$ENDIF}
  i: integer;
  ColorTable: TPdfBinary;
  NumOfColors: integer;
  S: string;
begin
  // creating color table from palette of bitmap.

  if ABitmap.PixelFormat <> pf8bit then
    raise EPdfInvalidImageFormat.Create('only 8 bit color image is allowed.');

  NumOfColors := 256;
  {$IFNDEF USE_CLX}
  if GetPaletteEntries(ABitmap.Palette, 0, NumOfColors + 1, PalEntries) = 0 then
    raise EPdfInvalidImageFormat.Create('failed to get Palette..');
  {$ELSE}
  PalEntries := PColorTable(ABitmap.ColorTable);
  {$ENDIF}
  ColorTable := TPdfBinary.Create;
  S := '<';

  {$IFNDEF USE_CLX}
  for i := 0 to NumOfColors - 1 do
    with PalEntries[i] do
      S := S + IntToHex(peRed, 2) +
           IntToHex(peGreen, 2) +
           IntToHex(peBlue, 2) +
           ' ';
  {$ELSE}
  for i := 0 to NumOfColors - 1 do
  begin
    CRgb := PalEntries[i];
    pb := PByteArray(@CRgb);
    S := S + IntToHex(pb[2], 2) +
         IntToHex(pb[1], 2) +
         IntToHex(pb[0], 2) +
           ' ';
  end;
  {$ENDIF}

  S := S + '>';
  ColorTable.Stream.Write(PChar(S)^, Length(S));

  result := TPdfArray.CreateArray(nil);
  with result do
  begin
    AddItem(TPdfName.CreateName('Indexed'));
    AddItem(TPdfName.CreateName('DeviceRGB'));
    AddItem(TPdfNumber.CreateNumber(NumOfColors - 1));
    AddItem(ColorTable);
  end;
end;

function TPdfBitmapImage.CreateImage(AImage: TGraphic): TPdfImage;
var
  ABitmap: TBitmap;
  x, y: integer;
  pb: PByteArray;
  b: Byte;
const
  {$IFDEF USE_CLX}
  PIXEL_COLOR_SIZE = 4;
  {$ELSE}
  PIXEL_COLOR_SIZE = 3;
  {$ENDIF}
begin
  result := TPdfImage.CreateStream(nil);
  with result do
  try
    with Attributes do
    begin
      AddItem('Type', TPdfName.CreateName('XObject'));
      AddItem('Subtype', TPdfName.CreateName('Image'));
    end;

    ABitmap := TBitmap.Create;
    with ABitmap do
    try
      Assign(AImage);

      // if bitmap image has less then 8 bit color, set PixelFormat to 8 bit.
      if (PixelFormat = pf1bit) or
         {$IFNDEF USE_CLX}
         (PixelFormat = pf4bit) or
         {$ENDIF}
         (PixelFormat = pf8bit) then
        PixelFormat := pf8bit
      else
        {$IFNDEF USE_CLX}
        PixelFormat := pf24Bit;
        {$ELSE}
        PixelFormat := pf32Bit;
        {$ENDIF}

      // translate TBitmap object to pdf image format.
      if PixelFormat = pf8bit then
      begin
        for y := 0 to Height - 1 do
        begin
          pb := ScanLine[y];
          Stream.Write(pb^, Width);
        end;
        Attributes.AddItem('ColorSpace', CreateIndexedColorArray(ABitmap));
      end
      else
      begin
        for y := 0 to Height - 1 do
        begin
          pb := ScanLine[y];
          x := 0;
          while x < Width * PIXEL_COLOR_SIZE - 1 do
          begin
            b := pb[x];
            pb[x] := pb[x+2];
            pb[x+2] := b;
            Stream.Write(pb[x], 3);
            x := x + PIXEL_COLOR_SIZE;
          end;
          Attributes.AddItem('ColorSpace', TPdfName.CreateName('DeviceRGB'));
        end;
      end;

      with Attributes do
      begin
        AddItem('Width', TPdfNumber.CreateNumber(Width));
        AddItem('Height', TPdfNumber.CreateNumber(Height));
        AddItem('BitsPerComponent', TPdfNumber.CreateNumber(8));
        if USE_ZLIB then
          PdfArrayByName('Filter').AddItem(TPdfName.CreateName('FlateDecode'));
      end;
    finally
      Free;
    end;
  except
    result.Free;
    raise;
  end;
end;

initialization
  RegisterClassAlias(TPdfBitmapImage, 'Pdf-Bitmap');

finalization
  UnRegisterClass(TPdfBitmapImage);

end.

