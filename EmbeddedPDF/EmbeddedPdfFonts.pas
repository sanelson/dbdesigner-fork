unit EmbeddedPdfFonts;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of fabFORCE EmbeddedPDF.
// Copyright (c) 1999-2001 Takezou. <takeshi_kanno@est.hi-ho.ne.jp>
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
// Unit PdfFonts.pas
// -----------------
// Version 1.0, 12.09.2003, Mike
// Description
//   Contains the font classes
//
// Changes:
//   Version 1.0, 12.09.2003, Mike
//     adapted version from Takezou's PowerPDF, Version 0.9 (beta), 2000.09.14
//
//----------------------------------------------------------------------------------------------------------------------


interface

uses
  SysUtils, Classes, EmbeddedPdfDoc, EmbeddedPdfTypes;

const
  TYPE1_FONT_STR_TABLE: array[0..2] of TPDF_STR_TBL =(
                         (KEY: 'Type'; VAL: 'Font'),
                         (KEY: 'Subtype'; VAL: 'Type1'),
                         (KEY: 'Encoding'; VAL: 'WinAnsiEncoding')
                         );

  TRUETYPE_FONT_STR_TABLE: array[0..2] of TPDF_STR_TBL =(
                         (KEY: 'Type'; VAL: 'Font'),
                         (KEY: 'Subtype'; VAL: 'TrueType'),
                         (KEY: 'Encoding'; VAL: 'WinAnsiEncoding')
                         );

  // FixedWidth defination
  FIXED_WIDTH_W_ARRAY: array[32..255] of Integer = (
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600);

  FIXED_WIDTH_INT_TABLE: array[0..1] of TPDF_INT_TBL =(
                       (KEY: 'FirstChar'; VAL: 32),
                       (KEY: 'LastChar'; VAL: 255)
                         );

  FIXED_WIDTH_DISC_STR_TABLE: array[0..2] of TPDF_STR_TBL =(
                         (KEY: 'Type'; VAL: 'FontDescriptor'),
                         (KEY: 'FontName'; VAL: 'Type1'),
                         (KEY: 'Encoding'; VAL: 'WinAnsiEncoding')
                         );

  FIXED_WIDTH_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 833),
                         (KEY: 'CapHeight'; VAL: 833),
                         (KEY: 'Descent'; VAL: -300),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET + PDF_FONT_FIXED_WIDTH),
                         (KEY: 'ItalicAngle'; VAL: -15),
                         (KEY: 'StemV'; VAL: 78),
                         (KEY: 'MissingWidth'; VAL: 600));

  FIXED_WIDTH_BBOX: array[0..3] of Integer = (-103,-300,836,833);

  // FixedWidth-Bold defination

  FIXED_WIDTH_BOLD_W_ARRAY: array[32..255] of Integer = (
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600);

  FIXED_WIDTH_BOLD_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 833),
                         (KEY: 'CapHeight'; VAL: 833),
                         (KEY: 'Descent'; VAL: -300),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET +
                           PDF_FONT_FOURCE_BOLD + PDF_FONT_FIXED_WIDTH),
                         (KEY: 'ItalicAngle'; VAL: 0),
                         (KEY: 'StemV'; VAL: 156),
                         (KEY: 'MissingWidth'; VAL: 600));

  FIXED_WIDTH_BOLD_BBOX: array[0..3] of Integer = (-46,-300,702,833);

  // FixedWidth-Italic defination

  FIXED_WIDTH_ITALIC_W_ARRAY: array[32..255] of Integer = (
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600);

  FIXED_WIDTH_ITALIC_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 833),
                         (KEY: 'CapHeight'; VAL: 833),
                         (KEY: 'Descent'; VAL: -300),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET +
                           PDF_FONT_FIXED_WIDTH),
                         (KEY: 'ItalicAngle'; VAL: -15),
                         (KEY: 'StemV'; VAL: 78),
                         (KEY: 'MissingWidth'; VAL: 600));

  FIXED_WIDTH_ITALIC_BBOX: array[0..3] of Integer = (-67,-300,800,833);

  // FixedWidth-BoldItalic defination

  FIXED_WIDTH_BOLDITALIC_W_ARRAY: array[32..255] of Integer = (
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,600,
    600,600);

  FIXED_WIDTH_BOLDITALIC_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 833),
                         (KEY: 'CapHeight'; VAL: 833),
                         (KEY: 'Descent'; VAL: -300),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET +
                             PDF_FONT_FIXED_WIDTH + PDF_FONT_FOURCE_BOLD),
                         (KEY: 'ItalicAngle'; VAL: -15),
                         (KEY: 'StemV'; VAL: 156),
                         (KEY: 'MissingWidth'; VAL: 600));

  FIXED_WIDTH_BOLDITALIC_BBOX: array[0..3] of Integer = (-103,-300,836,833);

  // Arial definition

  ARIAL_W_ARRAY: array[32..255] of Integer = (
    278,278,355,556,556,889,667,191,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,278,278,584,584,
    584,556,1015,667,667,722,722,667,611,778,722,278,500,667,556,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,278,278,278,
    469,556,333,556,556,500,556,556,278,556,556,222,222,500,222,833,
    556,556,556,556,333,500,278,556,500,722,500,500,500,334,260,334,
    584,0,556,0,222,556,333,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,222,222,333,333,350,556,1000,333,1000,500,333,944,0,
    500,667,0,333,556,556,556,556,260,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,556,537,278,333,333,365,556,834,834,
    834,611,667,667,667,667,667,667,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,500,556,556,556,556,278,278,
    278,278,556,556,556,556,556,556,556,584,611,556,556,556,556,500,
    556,500);


  ARIAL_INT_TABLE: array[0..1] of TPDF_INT_TBL = (
                       (KEY: 'FirstChar'; VAL: 32),
                       (KEY: 'LastChar'; VAL: 255)
                       );

  ARIAL_DISC_STR_TABLE: array[0..2] of TPDF_STR_TBL = (
                         (KEY: 'Type'; VAL: 'FontDescriptor'),
                         (KEY: 'FontName'; VAL: 'Type1'),
                         (KEY: 'Encoding'; VAL: 'WinAnsiEncoding')
                         );

  ARIAL_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL = (
                         (KEY: 'Ascent'; VAL: 905),
                         (KEY: 'CapHeight'; VAL: 905),
                         (KEY: 'Descent'; VAL: -212),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET),
                         (KEY: 'ItalicAngle'; VAL: 0),
                         (KEY: 'StemV'; VAL: 78),
                         (KEY: 'MissingWidth'; VAL: 750)
                         );

  ARIAL_BBOX: array[0..3] of Integer = (-166,-225,1000,931);

  // Arial-Bold definition

  ARIAL_BOLD_W_ARRAY: array[32..255] of Integer = (
    278,333,474,556,556,889,722,238,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,333,333,584,584,
    584,611,975,722,722,722,722,667,611,778,722,278,556,722,611,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,333,278,333,
    584,556,333,556,611,556,611,556,333,611,611,278,278,556,278,889,
    611,611,611,611,389,556,333,611,556,778,556,556,500,389,280,389,
    584,0,556,0,278,556,500,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,278,278,500,500,350,556,1000,333,1000,556,333,944,0,
    500,667,0,333,556,556,556,556,280,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,611,556,278,333,333,365,556,834,834,
    834,611,722,722,722,722,722,722,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,556,556,556,556,556,278,278,
    278,278,611,611,611,611,611,611,611,584,611,611,611,611,611,556,
    611,556);

  ARIAL_BOLD_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 905),
                         (KEY: 'CapHeight'; VAL: 905),
                         (KEY: 'Descent'; VAL: -212),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET +
                           PDF_FONT_FOURCE_BOLD),
                         (KEY: 'ItalicAngle'; VAL: 0),
                         (KEY: 'StemV'; VAL: 156),
                         (KEY: 'MissingWidth'; VAL: 750)
                         );

  ARIAL_BOLD_BBOX: array[0..3] of Integer = (-170,-228,1003,962);

  // Arial-Italic definition

  ARIAL_ITALIC_W_ARRAY: array[32..255] of Integer = (
    278,278,355,556,556,889,667,191,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,278,278,584,584,
    584,556,1015,667,667,722,722,667,611,778,722,278,500,667,556,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,278,278,278,
    469,556,333,556,556,500,556,556,278,556,556,222,222,500,222,833,
    556,556,556,556,333,500,278,556,500,722,500,500,500,334,260,334,
    584,0,556,0,222,556,333,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,222,222,333,333,350,556,1000,333,1000,500,333,944,0,
    500,667,0,333,556,556,556,556,260,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,556,537,278,333,333,365,556,834,834,
    834,611,667,667,667,667,667,667,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,500,556,556,556,556,278,278,
    278,278,556,556,556,556,556,556,556,584,611,556,556,556,556,500,
    556,500);

  ARIAL_ITALIC_DISC_STR_TABLE: array[0..2] of TPDF_STR_TBL =(
                         (KEY: 'Type'; VAL: 'FontDescriptor'),
                         (KEY: 'FontName'; VAL: 'Type1'),
                         (KEY: 'Encoding'; VAL: 'WinAnsiEncoding')
                         );

  ARIAL_ITALIC_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 905),
                         (KEY: 'CapHeight'; VAL: 905),
                         (KEY: 'Descent'; VAL: -212),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET),
                         (KEY: 'ItalicAngle'; VAL: -15),
                         (KEY: 'StemV'; VAL: 78),
                         (KEY: 'MissingWidth'; VAL: 750)
                         );

  ARIAL_ITALIC_BBOX: array[0..3] of Integer = (-170,-225,1116,931);

  // Arial-BoldItalic definition

  ARIAL_BOLDITALIC_W_ARRAY: array[32..255] of Integer = (
    278,333,474,556,556,889,722,238,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,333,333,584,584,
    584,611,975,722,722,722,722,667,611,778,722,278,556,722,611,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,333,278,333,
    584,556,333,556,611,556,611,556,333,611,611,278,278,556,278,889,
    611,611,611,611,389,556,333,611,556,778,556,556,500,389,280,389,
    584,0,556,0,278,556,500,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,278,278,500,500,350,556,1000,333,1000,556,333,944,0,
    500,667,0,333,556,556,556,556,280,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,611,556,278,333,333,365,556,834,834,
    834,611,722,722,722,722,722,722,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,556,556,556,556,556,278,278,
    278,278,611,611,611,611,611,611,611,584,611,611,611,611,611,556,
    611,556);

  ARIAL_BOLDITALIC_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 905),
                         (KEY: 'CapHeight'; VAL: 905),
                         (KEY: 'Descent'; VAL: -212),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET +
                           PDF_FONT_FOURCE_BOLD),
                         (KEY: 'ItalicAngle'; VAL: -15),
                         (KEY: 'StemV'; VAL: 156),
                         (KEY: 'MissingWidth'; VAL: 750));

  ARIAL_BOLDITALIC_BBOX: array[0..3] of Integer = (-174,-228,1114,962);

  // Times definition

  TIMES_ROMAN_W_ARRAY: array[32..255] of Integer = (
    250,333,408,500,500,833,778,180,333,333,500,564,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,278,278,564,564,
    564,444,921,722,667,667,722,611,556,722,722,333,389,722,611,889,
    722,722,556,722,667,556,611,722,722,944,722,722,611,333,278,333,
    469,500,333,444,500,444,500,444,333,500,500,278,278,500,278,778,
    500,500,500,500,333,389,278,500,500,722,500,500,444,480,200,480,
    541,0,500,0,333,500,444,1000,500,500,333,1000,556,333,889,0,
    611,0,0,333,333,444,444,350,500,1000,333,980,389,333,722,0,
    444,722,0,333,500,500,500,500,200,500,333,760,276,500,564,0,
    760,333,400,564,300,300,333,500,453,250,333,300,310,500,750,750,
    750,444,722,722,722,722,722,722,889,667,611,611,611,611,333,333,
    333,333,722,722,722,722,722,722,722,564,722,722,722,722,722,722,
    556,500,444,444,444,444,444,444,667,444,444,444,444,444,278,278,
    278,278,500,500,500,500,500,500,500,564,500,500,500,500,500,500,
    500,500);

  TIMES_INT_TABLE: array[0..1] of TPDF_INT_TBL = (
                       (KEY: 'FirstChar'; VAL: 32),
                       (KEY: 'LastChar'; VAL: 255)
                       );

  TIMES_DISC_STR_TABLE: array[0..2] of TPDF_STR_TBL =(
                         (KEY: 'Type'; VAL: 'FontDescriptor'),
                         (KEY: 'FontName'; VAL: 'Type1'),
                         (KEY: 'Encoding'; VAL: 'WinAnsiEncoding')
                         );

  TIMES_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 891),
                         (KEY: 'CapHeight'; VAL: 891),
                         (KEY: 'Descent'; VAL: -216),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET +
                           PDF_FONT_SERIF),
                         (KEY: 'ItalicAngle'; VAL: 0),
                         (KEY: 'StemV'; VAL: 78),
                         (KEY: 'MissingWidth'; VAL: 778)
                         );

  TIMES_BBOX: array[0..3] of Integer = (-168,-218,1000,898);

  // Times-Italic definition

  TIMES_ITALIC_W_ARRAY: array[32..255] of Integer = (
    250,333,420,500,500,833,778,214,333,333,500,675,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,333,333,675,675,
    675,500,920,611,611,667,722,611,611,722,722,333,444,667,556,833,
    667,722,611,722,611,500,556,722,611,833,611,556,556,389,278,389,
    422,500,333,500,500,444,500,444,278,500,500,278,278,444,278,722,
    500,500,500,500,389,389,278,500,444,667,444,444,389,400,275,400,
    541,0,500,0,333,500,556,889,500,500,333,1000,500,333,944,0,
    556,0,0,333,333,556,556,350,500,889,333,980,389,333,667,0,
    389,556,0,389,500,500,500,500,275,500,333,760,276,500,675,0,
    760,333,400,675,300,300,333,500,523,250,333,300,310,500,750,750,
    750,500,611,611,611,611,611,611,889,667,611,611,611,611,333,333,
    333,333,722,667,722,722,722,722,722,675,722,722,722,722,722,556,
    611,500,500,500,500,500,500,500,667,444,444,444,444,444,278,278,
    278,278,500,500,500,500,500,500,500,675,500,500,500,500,500,444,
    500,444);

  TIMES_ITALIC_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 891),
                         (KEY: 'CapHeight'; VAL: 891),
                         (KEY: 'Descent'; VAL: -216),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET +
                           PDF_FONT_SERIF + PDF_FONT_ITALIC),
                         (KEY: 'ItalicAngle'; VAL: -15),
                         (KEY: 'StemV'; VAL: 78),
                         (KEY: 'MissingWidth'; VAL: 778));

  TIMES_ITALIC_BBOX: array[0..3] of Integer = (-169,-217,1010,883);

  // Times-BOLD definition

  TIMES_BOLD_W_ARRAY: array[32..255] of Integer = (
    250,333,555,500,500,1000,833,278,333,333,500,570,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,333,333,570,570,
    570,500,930,722,667,722,722,667,611,778,778,389,500,778,667,944,
    722,778,611,778,722,556,667,722,722,1000,722,722,667,333,278,333,
    581,500,333,500,556,444,556,444,333,500,556,278,333,556,278,833,
    556,500,556,556,444,389,333,556,500,722,500,500,444,394,220,394,
    520,0,500,0,333,500,500,1000,500,500,333,1000,556,333,1000,0,
    667,0,0,333,333,500,500,350,500,1000,333,1000,389,333,722,0,
    444,722,0,333,500,500,500,500,220,500,333,747,300,500,570,0,
    747,333,400,570,300,300,333,556,540,250,333,300,330,500,750,750,
    750,500,722,722,722,722,722,722,1000,722,667,667,667,667,389,389,
    389,389,722,722,778,778,778,778,778,570,778,722,722,722,722,722,
    611,556,500,500,500,500,500,500,722,444,444,444,444,444,278,278,
    278,278,500,556,500,500,500,500,500,570,500,556,556,556,556,500,
    556,500);

  TIMES_BOLD_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 891),
                         (KEY: 'CapHeight'; VAL: 891),
                         (KEY: 'Descent'; VAL: -216),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET +
                           PDF_FONT_SERIF + PDF_FONT_FOURCE_BOLD),
                         (KEY: 'ItalicAngle'; VAL: 0),
                         (KEY: 'StemV'; VAL: 156),
                         (KEY: 'MissingWidth'; VAL: 778));

  TIMES_BOLD_BBOX: array[0..3] of Integer = (-168,-218,1000,935);

  // Times-BoldItalic definition

  TIMES_BOLDITALIC_W_ARRAY: array[32..255] of Integer = (
    250,389,555,500,500,833,778,278,333,333,500,570,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,333,333,570,570,
    570,500,832,667,667,667,722,667,667,722,778,389,500,667,611,889,
    722,722,611,722,667,556,611,722,667,889,667,611,611,333,278,333,
    570,500,333,500,500,444,500,444,333,500,556,278,278,500,278,778,
    556,500,500,500,389,389,278,556,444,667,500,444,389,348,220,348,
    570,0,500,0,333,500,500,1000,500,500,333,1000,556,333,944,0,
    611,0,0,333,333,500,500,350,500,1000,333,1000,389,333,722,0,
    389,611,0,389,500,500,500,500,220,500,333,747,266,500,606,0,
    747,333,400,570,300,300,333,576,500,250,333,300,300,500,750,750,
    750,500,667,667,667,667,667,667,944,667,667,667,667,667,389,389,
    389,389,722,722,722,722,722,722,722,570,722,722,722,722,722,611,
    611,500,500,500,500,500,500,500,722,444,444,444,444,444,278,278,
    278,278,500,556,500,500,500,500,500,570,500,556,556,556,556,444,
    500,444);

  TIMES_BOLDITALIC_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 891),
                         (KEY: 'CapHeight'; VAL: 891),
                         (KEY: 'Descent'; VAL: -216),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET +
                           PDF_FONT_SERIF + PDF_FONT_FOURCE_BOLD),
                         (KEY: 'ItalicAngle'; VAL: -15),
                         (KEY: 'StemV'; VAL: 156),
                         (KEY: 'MissingWidth'; VAL: 778));

  TIMES_BOLDITALIC_BBOX: array[0..3] of Integer = (-200,-218,996,921);

  SCRIPT_W_ARRAY: array[32..255] of Integer = (
    323,202,323,424,404,485,525,202,283,283,323,525,202,525,202,444,
    404,404,404,404,404,404,404,404,404,404,202,202,485,525,485,364,
    545,404,465,404,465,404,404,465,485,343,303,485,384,667,485,424,
    505,444,505,404,384,485,465,566,485,465,424,283,283,283,444,323,
    222,323,283,222,323,202,162,303,303,141,141,283,162,505,364,283,
    303,303,263,222,182,303,303,424,323,303,283,283,162,283,485,202,
    202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,
    202,222,202,202,202,202,202,202,202,202,202,202,202,202,202,202,
    202,202,222,384,283,465,162,283,404,283,323,404,404,404,283,404,
    404,404,404,404,404,384,424,404,404,404,283,404,404,404,404,364,
    404,404,404,404,404,404,566,404,404,404,404,404,343,343,343,343,
    465,485,424,424,424,424,424,323,404,485,485,485,485,465,444,444,
    323,323,323,323,323,323,384,222,202,202,202,202,141,141,141,141,
    283,364,283,283,283,283,283,404,283,303,303,303,303,303,384,303
    );

  //-----------------------------------------------------------
  // Tahoma

  STD_INT_TABLE: array[0..1] of TPDF_INT_TBL = (
                       (KEY: 'FirstChar'; VAL: 32),
                       (KEY: 'LastChar'; VAL: 255)
                       );

  Tahoma_FONT_BBOX: array[0..3] of Integer = (-250,-207,1674,1000);

  Tahoma_FONT_DISC_INT_TABLE: array[0..3] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 1000),
                         (KEY: 'CapHeight'; VAL: 1000),
                         (KEY: 'Descent'; VAL: -207),
                         (KEY: 'MissingWidth'; VAL: 384)
                         );

  // FixedWidth defination
  Tahoma_WIDTH_W_ARRAY: array[32..255] of Integer = (
    313,332,401,728,546,977,674,211,383,383,546,728,303,363,303,382,
    546,546,546,546,546,546,546,546,546,546,354,354,728,728,728,474,
    909,600,589,601,678,561,521,667,675,373,417,588,498,771,667,708,
    551,708,621,557,584,656,597,902,581,576,559,383,382,383,728,546,
    546,525,553,461,553,526,318,553,558,229,282,498,229,840,558,543,
    553,553,360,446,334,558,498,742,495,498,444,480,382,480,728,1000,
    546,1000,211,546,397,817,546,546,546,1391,557,383,977,1000,559,1000,
    1000,211,211,401,401,455,546,909,546,876,446,383,909,1000,444,576,
    313,332,546,546,546,546,382,546,546,929,493,573,728,363,929,546,
    471,728,493,493,546,568,546,354,546,493,493,573,1000,1000,1000,474,
    600,600,600,600,600,600,913,601,561,561,561,561,373,373,373,373,
    698,667,708,708,708,708,708,728,708,656,656,656,656,576,565,548,
    525,525,525,525,525,525,880,461,526,526,526,526,229,229,229,229,
    546,558,543,543,543,543,543,728,543,558,558,558,558,498,553,498);

  //-----------------------------------------------------------
  // Tahoma, Bold

  Tahoma_Bold_FONT_BBOX: array[0..3] of Integer = (-250,-207,2020,1000);

  Tahoma_Bold_FONT_DISC_INT_TABLE: array[0..10] of TPDF_INT_TBL =(
                         (KEY: 'MissingWidth'; VAL: 456),
                         (KEY: 'StemV'; VAL: 162),
                         (KEY: 'StemH'; VAL: 162),
                         (KEY: 'ItalicAngle'; VAL: 0),
                         (KEY: 'CapHeight'; VAL: 1000),
                         (KEY: 'XHeight'; VAL: 500),
                         (KEY: 'Ascent'; VAL: 1000),
                         (KEY: 'Descent'; VAL: -207),
                         (KEY: 'Leading'; VAL: 207),
                         (KEY: 'MaxWidth'; VAL: 1683),
                         (KEY: 'AvgWidth'; VAL: 506)
                         );

  // FixedWidth defination
  Tahoma_Bold_WIDTH_W_ARRAY: array[32..255] of Integer = (
    293, 343, 489, 818, 637, 1199, 781, 275, 454, 454, 637, 818, 313, 431, 313, 577,
    637, 637, 637, 637, 637, 637, 637, 637, 637, 637, 363, 363, 818, 818, 818, 566,
    920, 685, 686, 667, 757, 615, 581, 745, 764, 483, 500, 696, 572, 893, 771, 770,
    657, 770, 726, 633, 612, 739, 675, 1028, 685, 670, 623, 454, 577, 454, 818, 637,
    546, 599, 632, 527, 629, 594, 382, 629, 640, 302, 363, 603, 302, 954, 640, 617,
    629, 629, 434, 515, 416, 640, 579, 890, 604, 576, 526, 623, 637, 623, 818, 1000,
    637, 1000, 275, 637, 489, 1000, 637, 637, 546, 1676, 633, 425, 1037, 1000, 623, 1000,
    1000, 275, 275, 489, 489, 637, 637, 909, 546, 861, 515, 425, 985, 1000, 526, 670,
    293, 343, 637, 637, 637, 637, 637, 637, 546, 929, 508, 703, 818, 431, 929, 637,
    520, 818, 539, 539, 546, 651, 637, 363, 546, 539, 539, 703, 1128, 1128, 1128, 566,
    685, 685, 685, 685, 685, 685, 989, 667, 615, 615, 615, 615, 483, 483, 483, 483,
    774, 771, 770, 770, 770, 770, 770, 818, 770, 739, 739, 739, 739, 670, 659, 646,
    599, 599, 599, 599, 599, 599, 937, 527, 594, 594, 594, 594, 302, 302, 302, 302,
    620, 640, 617, 617, 617, 617, 617, 818, 617, 640, 640, 640, 640, 576, 629, 576);

  //-----------------------------------------------------------
  // TrebuchetMS

  TrebuchetMS_FONT_BBOX: array[0..3] of Integer = (-250,-222,1087,1000);

  TrebuchetMS_FONT_DISC_INT_TABLE: array[0..10] of TPDF_INT_TBL =(
                         (KEY: 'MissingWidth'; VAL: 335),
                         (KEY: 'StemV'; VAL: 83),
                         (KEY: 'StemH'; VAL: 83),
                         (KEY: 'ItalicAngle'; VAL: 0),
                         (KEY: 'CapHeight'; VAL: 939),
                         (KEY: 'XHeight'; VAL: 470),
                         (KEY: 'Ascent'; VAL: 939),
                         (KEY: 'Descent'; VAL: -222),
                         (KEY: 'Leading'; VAL: 161),
                         (KEY: 'MaxWidth'; VAL: 906),
                         (KEY: 'AvgWidth'; VAL: 454)
                         );

  // FixedWidth defination
  TrebuchetMS_WIDTH_W_ARRAY: array[32..255] of Integer = (
    301, 367, 325, 524, 524, 600, 706, 160, 367, 367, 367, 524, 367, 367, 367, 524,
    524, 524, 524, 524, 524, 524, 524, 524, 524, 524, 367, 367, 524, 524, 524, 367,
    771, 590, 566, 598, 613, 536, 525, 676, 654, 278, 477, 576, 506, 709, 638, 674,
    558, 676, 582, 481, 581, 648, 587, 852, 557, 570, 550, 367, 355, 367, 524, 524,
    524, 525, 557, 495, 557, 545, 370, 502, 546, 285, 367, 504, 295, 830, 546, 537,
    557, 557, 389, 405, 396, 546, 490, 744, 501, 493, 475, 367, 524, 367, 524, 500,
    524, 500, 367, 388, 524, 734, 459, 459, 524, 913, 481, 367, 993, 500, 550, 500,
    500, 367, 367, 524, 524, 524, 367, 734, 524, 635, 405, 367, 924, 500, 475, 570,
    301, 367, 524, 524, 524, 570, 524, 454, 524, 713, 367, 524, 524, 367, 713, 524,
    524, 524, 451, 454, 524, 546, 524, 367, 524, 451, 367, 524, 814, 814, 814, 367,
    590, 590, 590, 590, 590, 590, 867, 598, 536, 536, 536, 536, 278, 278, 278, 278,
    613, 638, 674, 674, 674, 674, 674, 524, 657, 648, 648, 648, 648, 570, 556, 546,
    525, 525, 525, 525, 525, 525, 873, 495, 545, 545, 545, 545, 285, 285, 285, 285,
    549, 546, 537, 537, 537, 537, 537, 524, 545, 546, 546, 546, 546, 493, 553, 493);

  //-----------------------------------------------------------
  // TrebuchetMS, Bold

  TrebuchetMS_Bold_FONT_BBOX: array[0..3] of Integer = (-250,-222,1133,1000);

  TrebuchetMS_Bold_FONT_DISC_INT_TABLE: array[0..10] of TPDF_INT_TBL =(
                         (KEY: 'MissingWidth'; VAL: 366),
                         (KEY: 'StemV'; VAL: 151),
                         (KEY: 'StemH'; VAL: 151),
                         (KEY: 'ItalicAngle'; VAL: 0),
                         (KEY: 'CapHeight'; VAL: 939),
                         (KEY: 'XHeight'; VAL: 470),
                         (KEY: 'Ascent'; VAL: 939),
                         (KEY: 'Descent'; VAL: -222),
                         (KEY: 'Leading'; VAL: 161),
                         (KEY: 'MaxWidth'; VAL: 944),
                         (KEY: 'AvgWidth'; VAL: 474)
                         );

  // FixedWidth defination
  TrebuchetMS_Bold_WIDTH_W_ARRAY: array[32..255] of Integer = (
    301, 367, 367, 586, 586, 684, 706, 229, 367, 367, 432, 586, 367, 367, 367, 390, 
    586, 586, 586, 586, 586, 586, 586, 586, 586, 586, 367, 367, 586, 586, 586, 438,
    771, 633, 595, 612, 643, 569, 583, 671, 684, 278, 533, 617, 553, 745, 667, 703,
    587, 709, 611, 511, 612, 678, 622, 884, 601, 613, 560, 402, 355, 402, 586, 586,
    586, 533, 582, 512, 581, 575, 370, 502, 593, 298, 367, 548, 295, 859, 590, 566,
    583, 584, 427, 431, 396, 591, 527, 784, 552, 534, 528, 434, 586, 434, 586, 500,
    586, 500, 367, 388, 524, 734, 459, 459, 586, 1036, 511, 367, 1003, 500, 560, 500,
    500, 367, 367, 586, 586, 524, 367, 734, 586, 644, 431, 367, 921, 500, 528, 613,
    301, 367, 586, 524, 586, 570, 586, 454, 586, 713, 429, 586, 586, 367, 713, 586,
    586, 586, 451, 454, 586, 547, 524, 367, 586, 451, 429, 586, 814, 814, 814, 438,
    633, 633, 633, 633, 633, 633, 935, 612, 569, 569, 569, 569, 278, 278, 278, 278,
    643, 667, 703, 703, 703, 703, 703, 586, 684, 678, 678, 678, 678, 613, 558, 546,
    533, 533, 533, 533, 533, 533, 863, 512, 575, 575, 575, 575, 298, 298, 298, 298,
    566, 590, 566, 566, 566, 566, 566, 586, 566, 591, 591, 591, 591, 534, 583, 534);


  //-----------------------------------------------------------

  SCRIPT_INT_TABLE: array[0..1] of TPDF_INT_TBL = (
                       (KEY: 'FirstChar'; VAL: 32),
                       (KEY: 'LastChar'; VAL: 255)
                         );

  SCRIPT_DISC_STR_TABLE: array[0..2] of TPDF_STR_TBL =(
                         (KEY: 'Type'; VAL: 'FontDescriptor'),
                         (KEY: 'FontName'; VAL: 'Type1'),
                         (KEY: 'Encoding'; VAL: 'WinAnsiEncoding')
                         );

  SCRIPT_DISC_INT_TABLE: array[0..6] of TPDF_INT_TBL =(
                         (KEY: 'Ascent'; VAL: 758),
                         (KEY: 'CapHeight'; VAL: 758),
                         (KEY: 'Descent'; VAL: -363),
                         (KEY: 'Flags'; VAL: PDF_FONT_STD_CHARSET + PDF_FONT_ITALIC),
                         (KEY: 'ItalicAngle'; VAL: 0),
                         (KEY: 'StemV'; VAL: 78),
                         (KEY: 'MissingWidth'; VAL: 202));

  SCRIPT_BBOX: array[0..3] of Integer = (-184,-363,505,758);

type
  TPdfType1Font = class(TPdfFont)
  private
    FFirstChar: Byte;
    FLastChar: Byte;
    FArray: array[0..255] of Word;
  public
    procedure SetData(Value: TPdfDictionary); override;
    function GetCharWidth(AText: string; APos: integer): integer; override;
  end;

  TPdfTrueTypeFont = class(TPdfFont)
  private
    FFirstChar: Byte;
    FLastChar: Byte;
    FArray: array[0..255] of Word;
  public
    procedure SetData(Value: TPdfDictionary); override;
    function GetCharWidth(AText: string; APos: integer): integer; override;
  end;

  TPdfFixedWidth = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfFixedWidthBold = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfFixedWidthItalic = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfFixedWidthBoldItalic = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfArial = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfArialBold = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfArialItalic = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfArialBoldItalic = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfTimesRoman = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfTimesBold = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfTimesItalic = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfTimesBoldItalic = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfScript = class(TPdfType1Font)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfTahoma = class(TPdfTrueTypeFont)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfTahomaBold = class(TPdfTrueTypeFont)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfTrebuchetMS = class(TPdfTrueTypeFont)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

  TPdfTrebuchetMSBold = class(TPdfTrueTypeFont)
  public
    constructor Create(AXref: TPdfXref; AName: string); override;
  end;

implementation

{ TPdfType1Font }
function TPdfType1Font.GetCharWidth(AText: string; APos: integer): integer;
begin
  result := FArray[ord(AText[APos])];
end;

procedure TPdfType1Font.SetData(Value: TPdfDictionary);
var
  i: integer;
  DefaultWidth: Word;
  Widths: TPdfArray;
begin
  inherited SetData(Value);

  // initialize char widths array by default value (if missing width parameter
  // is defined, use it as default value.)
  if Data.PdfNumberByName('MissingWidth') <> nil then
    DefaultWidth := Data.PdfNumberByName('MissingWidth').Value
  else
    DefaultWidth := 0;
  for i := 0 to 255 do
    FArray[i] := DefaultWidth;

  FFirstChar := Data.PdfNumberByName('FirstChar').Value;
  FLastChar := Data.PdfNumberByName('LastChar').Value;

  // fill width array with "Widths" table values.
  Widths := Data.PdfArrayByName('Widths');
  for i := 0 to Widths.ItemCount - 1 do
    FArray[i + FFirstChar] := TPdfNumber(Widths.Items[i]).Value;
end;

{ TPdfTrueTypeFont }
function TPdfTrueTypeFont.GetCharWidth(AText: string; APos: integer): integer;
begin
  result := FArray[ord(AText[APos])];
end;

procedure TPdfTrueTypeFont.SetData(Value: TPdfDictionary);
var
  i: integer;
  DefaultWidth: Word;
  Widths: TPdfArray;
begin
  inherited SetData(Value);

  // initialize char widths array by default value (if missing width parameter
  // is defined, use it as default value.)
  if Data.PdfNumberByName('MissingWidth') <> nil then
    DefaultWidth := Data.PdfNumberByName('MissingWidth').Value
  else
    DefaultWidth := 0;
  for i := 0 to 255 do
    FArray[i] := DefaultWidth;

  FFirstChar := Data.PdfNumberByName('FirstChar').Value;
  FLastChar := Data.PdfNumberByName('LastChar').Value;

  // fill width array with "Widths" table values.
  Widths := Data.PdfArrayByName('Widths');
  for i := 0 to Widths.ItemCount - 1 do
    FArray[i + FFirstChar] := TPdfNumber(Widths.Items[i]).Value;
end;

{ FixedWidth }
constructor TPdfFixedWidth.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);

  // make instance of TPdfDictionary and register to Xref table.
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  // adding element to the dictionary.
  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, FIXED_WIDTH_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Courier'));

  // create "Width" table of the font.
  FWidths := TPdfArray.CreateNumArray(AXref, FIXED_WIDTH_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ FixedWidthBold }
constructor TPdfFixedWidthBold.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, FIXED_WIDTH_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Courier-Bold'));

  FWidths := TPdfArray.CreateNumArray(AXref, FIXED_WIDTH_BOLD_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ FixedWidthItalic }
constructor TPdfFixedWidthItalic.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, FIXED_WIDTH_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Courier-Oblique'));

  FWidths := TPdfArray.CreateNumArray(AXref, FIXED_WIDTH_ITALIC_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ FixedWidthBoldItalic }
constructor TPdfFixedWidthBoldItalic.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, FIXED_WIDTH_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Courier-BoldOblique'));

  FWidths := TPdfArray.CreateNumArray(AXref, FIXED_WIDTH_BOLDITALIC_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ Arial }
constructor TPdfArial.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, ARIAL_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Helvetica'));

  FWidths := TPdfArray.CreateNumArray(AXref, ARIAL_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ Arial-Bold }
constructor TPdfArialBold.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, ARIAL_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Helvetica-Bold'));

  FWidths := TPdfArray.CreateNumArray(AXref, ARIAL_BOLD_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ Arial-Italic }
constructor TPdfArialItalic.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, ARIAL_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Helvetica-Oblique'));

  FWidths := TPdfArray.CreateNumArray(AXref, ARIAL_ITALIC_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ Arial-BoldItalic }
constructor TPdfArialBoldItalic.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, ARIAL_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Helvetica-BoldOblique'));

  FWidths := TPdfArray.CreateNumArray(AXref, ARIAL_BOLDITALIC_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ TPdfTimesRoman }
constructor TPdfTimesRoman.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, TIMES_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Times-Roman'));

  FWidths := TPdfArray.CreateNumArray(AXref, TIMES_ROMAN_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ TPdfTimesBold }
constructor TPdfTimesBold.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, TIMES_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Times-Bold'));

  FWidths := TPdfArray.CreateNumArray(AXref, TIMES_BOLD_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ TPdfTimesItalic }
constructor TPdfTimesItalic.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, TIMES_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Times-Italic'));

  FWidths := TPdfArray.CreateNumArray(AXref, TIMES_ITALIC_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ TPdfTimesBoldItalic }
constructor TPdfTimesBoldItalic.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, TIMES_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Times-BoldItalic'));

  FWidths := TPdfArray.CreateNumArray(AXref, TIMES_BOLDITALIC_W_ARRAY);
  FFont.AddInternalItem('Widths', FWidths);

  SetData(FFont);
end;

{ TPdfScript }
constructor TPdfScript.Create(AXref: TPdfXref; AName: string);
var
  FWidths: TPdfArray;
  FFontDescriptor: TPdfDictionary;
  FFont: TPdfDictionary;
begin
  inherited Create(AXref, AName);
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TYPE1_FONT_STR_TABLE);
  AddIntElements(FFont, SCRIPT_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Script'));

  FFontDescriptor := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFontDescriptor);

  AddStrElements(FFontDescriptor, SCRIPT_DISC_STR_TABLE);
  AddIntElements(FFontDescriptor, SCRIPT_DISC_INT_TABLE);
  FFontDescriptor.AddItem('FontBBox',
             TPdfArray.CreateNumArray(AXref, SCRIPT_BBOX));
  FFont.AddItem('FontDescriptor', FFontDescriptor);

  FWidths := TPdfArray.CreateNumArray(AXref, SCRIPT_W_ARRAY);
  FFont.AddItem('Widths', FWidths);

  SetData(FFont);
end;

{ TPdfTahoma }
constructor TPdfTahoma.Create(AXref: TPdfXref; AName: string);
var FFont: TPdfDictionary;
  FFontDescriptor: TPdfDictionary;
begin
  inherited Create(AXref, AName);

  // create font
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TRUETYPE_FONT_STR_TABLE);
  AddIntElements(FFont, STD_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Tahoma'));
  FFont.AddItem('Widths', TPdfArray.CreateNumArray(AXref, Tahoma_WIDTH_W_ARRAY));

  // create font descriptor.
  FFontDescriptor := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFontDescriptor);

  FFontDescriptor.AddNameItem('Type', 'FontDescriptor');
  FFontDescriptor.AddNameItem('BaseFont', 'Tahoma');
  FFontDescriptor.AddNumberItem('Flags', PDF_FONT_STD_CHARSET);
  FFontDescriptor.AddItem('FontBBox', TPdfArray.CreateNumArray(AXref, Tahoma_FONT_BBOX));
  AddIntElements(FFontDescriptor, Tahoma_FONT_DISC_INT_TABLE);

  //Add descriptor to font
  FFont.AddItem('FontDescriptor', FFontDescriptor);

  SetData(FFont);
end;

{ TPdfTahomaBold }
constructor TPdfTahomaBold.Create(AXref: TPdfXref; AName: string);
var FFont: TPdfDictionary;
  FFontDescriptor: TPdfDictionary;
begin
  inherited Create(AXref, AName);

  // create font
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TRUETYPE_FONT_STR_TABLE);
  AddIntElements(FFont, STD_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('Tahoma,Bold'));
  FFont.AddItem('Widths', TPdfArray.CreateNumArray(AXref, Tahoma_Bold_WIDTH_W_ARRAY));

  // create font descriptor.
  FFontDescriptor := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFontDescriptor);

  FFontDescriptor.AddNameItem('Type', 'FontDescriptor');
  FFontDescriptor.AddNameItem('BaseFont', 'Tahoma,Bold');
  FFontDescriptor.AddNumberItem('Flags', 16416);
  FFontDescriptor.AddItem('FontBBox', TPdfArray.CreateNumArray(AXref, Tahoma_Bold_FONT_BBOX));
  AddIntElements(FFontDescriptor, Tahoma_Bold_FONT_DISC_INT_TABLE);

  //Add descriptor to font
  FFont.AddItem('FontDescriptor', FFontDescriptor);

  SetData(FFont);
end;

{ TPdfTrebuchetMS }
constructor TPdfTrebuchetMS.Create(AXref: TPdfXref; AName: string);
var FFont: TPdfDictionary;
  FFontDescriptor: TPdfDictionary;
begin
  inherited Create(AXref, AName);

  // create font
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TRUETYPE_FONT_STR_TABLE);
  AddIntElements(FFont, STD_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('TrebuchetMS'));
  FFont.AddItem('Widths', TPdfArray.CreateNumArray(AXref, TrebuchetMS_WIDTH_W_ARRAY));

  // create font descriptor.
  FFontDescriptor := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFontDescriptor);

  FFontDescriptor.AddNameItem('Type', 'FontDescriptor');
  FFontDescriptor.AddNameItem('BaseFont', 'TrebuchetMS');
  FFontDescriptor.AddNumberItem('Flags', PDF_FONT_STD_CHARSET);
  FFontDescriptor.AddItem('FontBBox', TPdfArray.CreateNumArray(AXref, TrebuchetMS_FONT_BBOX));
  AddIntElements(FFontDescriptor, TrebuchetMS_FONT_DISC_INT_TABLE);

  //Add descriptor to font
  FFont.AddItem('FontDescriptor', FFontDescriptor);

  SetData(FFont);
end;

{ TPdfTrebuchetMSBold }
constructor TPdfTrebuchetMSBold.Create(AXref: TPdfXref; AName: string);
var FFont: TPdfDictionary;
  FFontDescriptor: TPdfDictionary;
begin
  inherited Create(AXref, AName);

  // create font
  FFont := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFont);

  AddStrElements(FFont, TRUETYPE_FONT_STR_TABLE);
  AddIntElements(FFont, STD_INT_TABLE);
  FFont.AddItem('BaseFont', TPdfName.CreateName('TrebuchetMS,Bold'));
  FFont.AddItem('Widths', TPdfArray.CreateNumArray(AXref, TrebuchetMS_Bold_WIDTH_W_ARRAY));

  // create font descriptor.
  FFontDescriptor := TPdfDictionary.CreateDictionary(AXref);
  AXref.AddObject(FFontDescriptor);

  FFontDescriptor.AddNameItem('Type', 'FontDescriptor');
  FFontDescriptor.AddNameItem('BaseFont', 'TrebuchetMS,Bold');
  FFontDescriptor.AddNumberItem('Flags', 16416);
  FFontDescriptor.AddItem('FontBBox', TPdfArray.CreateNumArray(AXref, TrebuchetMS_Bold_FONT_BBOX));
  AddIntElements(FFontDescriptor, TrebuchetMS_Bold_FONT_DISC_INT_TABLE);

  //Add descriptor to font
  FFont.AddItem('FontDescriptor', FFontDescriptor);

  SetData(FFont);
end;


initialization

  RegisterClassAlias(TPdfFixedWidth, 'FixedWidth');
  RegisterClassAlias(TPdfFixedWidthBold, 'FixedWidth-Bold');
  RegisterClassAlias(TPdfFixedWidthBoldItalic, 'FixedWidth-BoldItalic');
  RegisterClassAlias(TPdfFixedWidthItalic, 'FixedWidth-Italic');
  RegisterClassAlias(TPdfArial, 'Arial');
  RegisterClassAlias(TPdfArialBold, 'Arial-Bold');
  RegisterClassAlias(TPdfArialBoldItalic, 'Arial-BoldItalic');
  RegisterClassAlias(TPdfArialItalic, 'Arial-Italic');
  RegisterClassAlias(TPdfTimesRoman, 'Times-Roman');
  RegisterClassAlias(TPdfTimesBold, 'Times-Bold');
  RegisterClassAlias(TPdfTimesItalic, 'Times-Italic');
  RegisterClassAlias(TPdfTimesBoldItalic, 'Times-BoldItalic');
  RegisterClassAlias(TPdfTahoma, 'Tahoma');
  RegisterClassAlias(TPdfTahomaBold, 'Tahoma-Bold');
  RegisterClassAlias(TPdfTrebuchetMS, 'TrebuchetMS');
  RegisterClassAlias(TPdfTrebuchetMSBold, 'TrebuchetMS-Bold');
//  RegisterClassAlias(TPdfScript, 'Script');
//  RegisterClassAlias(TPdfSymbol, 'Symbol');

finalization

  UnRegisterClass(TPdfFixedWidth);
  UnRegisterClass(TPdfFixedWidthBold);
  UnRegisterClass(TPdfFixedWidthBoldItalic);
  UnRegisterClass(TPdfFixedWidthBold);
  UnRegisterClass(TPdfArial);
  UnRegisterClass(TPdfArialBold);
  UnRegisterClass(TPdfArialBoldItalic);
  UnRegisterClass(TPdfArialBold);
  UnRegisterClass(TPdfTimesRoman);
  UnRegisterClass(TPdfTimesBold);
  UnRegisterClass(TPdfTimesItalic);
  UnRegisterClass(TPdfTimesBoldItalic);
  UnRegisterClass(TPdfTahoma);
  UnRegisterClass(TPdfTahomaBold);
  UnRegisterClass(TPdfTrebuchetMS);
  UnRegisterClass(TPdfTrebuchetMSBold);
//  UnRegisterClass(TPdfScript);
//  UnRegisterClass(TPdfSymbol);

end.
