unit EmbeddedPdfDB;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of fabFORCE EmbeddedPDF.
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
// Unit PdfDB.pas
// --------------
// Version 1.0, 12.09.2003, Mike
// Description
//   Contains some utility functions to generate PDFs
//   from data sensitive components
//
// Changes:
//   Version 1.0, 12.09.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------


interface

{$I EmbeddedPdfConf.inc}

uses
  SysUtils, Classes, {$IFDEF LINUX}Types, {$ELSE}Windows, {$ENDIF}
  EmbeddedPdfTypes, EmbeddedPdfDoc,
  {$IFNDEF USE_CLX}DBGrids, {$ELSE}QDBGrids, {$ENDIF}DB;


procedure MakePDFFromDBGrid(theGrid: TDBGrid; fname, title: string; margin_left: Single=20; margin_right: Single=20; margin_top: Single=20; margin_bottom: Single=20);

implementation

procedure MakePDFFromDBGrid(theGrid: TDBGrid; fname, title: string; margin_left: Single=20; margin_right: Single=20; margin_top: Single=20; margin_bottom: Single=20);
var theDataset: TDataset;
  theBookmark: TBookmark;
  thePdfDoc: TPdfDoc;
  i: integer;
  zoomFac, ypos, x, lineheight, girdStartY: double;
  DrawHeader: Boolean;
begin
  if(theGrid.DataSource<>nil)and(theGrid.Columns.Count>0)then
    if(theGrid.DataSource.DataSet<>nil)then
    begin
      thePDFDoc:=TPdfDoc.Create;
      try
        thePDFDoc.NewDoc;
        thePDFDoc.AddPage;
        DrawHeader:=True;

        //Get the Zoom factor
        x:=0;
        for i:=0 to theGrid.Columns.Count-1 do
          x:=x+theGrid.Columns[i].Width;

        zoomFac:=(thePDFDoc.Canvas.PageWidth-margin_left-margin_right)/x;

        //Set the Text Size
        lineheight:=14*zoomFac;
        {$IFDEF LINUX}
        thePDFDoc.Canvas.SetFont('Arial', 11*zoomFac);
        {$ELSE}
        thePDFDoc.Canvas.SetFont('Tahoma', 11*zoomFac);
        {$ENDIF}

        //Draw the Title
        thePDFDoc.Canvas.TextOut(margin_left, margin_top, title);
        ypos:=margin_top+lineheight*2;

        girdStartY:=0;

        //Get dataset
        theDataset:=theGrid.DataSource.DataSet;
        try
          //Disable all controls so scrolling will be faster
          theDataset.DisableControls;

          //Store old position
          theBookmark:=theDataset.GetBookmark;
          try
            theDataset.First;
            while(Not(theDataset.EOF))do
            begin
              //Check if a new page is required
              if(ypos>thePDFDoc.Canvas.PageHeight-margin_bottom-lineheight)then
              begin
                //Draw Grid-H-lines
                x:=0;
                for i:=0 to theGrid.Columns.Count-1 do
                begin
                  thePDFDoc.Canvas.DrawLine(margin_left+x, girdStartY,
                    margin_right+x, ypos-lineheight*0.15, 0.1);

                  x:=x+theGrid.Columns[i].Width*zoomFac;
                end;
                thePDFDoc.Canvas.DrawLine(margin_left+x, girdStartY,
                  margin_right+x, ypos-lineheight*0.15, 0.1);

                ypos:=margin_top;
                thePDFDoc.AddPage;
                DrawHeader:=True;
              end;

              //Draw the column Header
              if(DrawHeader)then
              begin
                DrawHeader:=False;

                x:=0;
                for i:=0 to theGrid.Columns.Count-1 do
                begin
                  thePDFDoc.Canvas.TextRect(
                    _PdfRect(margin_left+x+2*zoomFac, ypos, margin_left+x+theGrid.Columns[i].Width*zoomFac-2*zoomFac, ypos+lineheight),
                    theGrid.Columns[i].Title.Caption,
                    paLeftJustify, True);

                  x:=x+theGrid.Columns[i].Width*zoomFac;
                end;

                thePDFDoc.Canvas.DrawLine(margin_left, ypos+lineheight*0.85,
                  thePDFDoc.Canvas.PageWidth-margin_right, ypos+lineheight*0.85, 0.4);

                girdStartY:=ypos+lineheight*0.85;
                ypos:=ypos+lineheight;
              end;

              //Draw Data Values
              x:=0;
              for i:=0 to theGrid.Columns.Count-1 do
              begin
                thePDFDoc.Canvas.TextRect(
                  _PdfRect(margin_left+x+2*zoomFac, ypos, margin_left+x+theGrid.Columns[i].Width*zoomFac-2*zoomFac, ypos+lineheight),
                  theGrid.Columns[i].Field.AsString,
                  paLeftJustify, True);

                thePDFDoc.Canvas.DrawLine(margin_left, ypos+lineheight*0.85,
                  thePDFDoc.Canvas.PageWidth-margin_right, ypos+lineheight*0.85, 0.1);

                x:=x+theGrid.Columns[i].Width*zoomFac;
              end;

              ypos:=ypos+lineheight;
              theDataset.Next;
            end;

            //Draw Grid-H-lines
            x:=0;
            for i:=0 to theGrid.Columns.Count-1 do
            begin
              thePDFDoc.Canvas.DrawLine(margin_left+x, girdStartY,
                margin_right+x, ypos-lineheight*0.15, 0.1);

              x:=x+theGrid.Columns[i].Width*zoomFac;
            end;
            thePDFDoc.Canvas.DrawLine(margin_left+x, girdStartY,
              margin_right+x, ypos-lineheight*0.15, 0.1);

            //Goto old pos
            theDataset.GotoBookmark(theBookmark);
          finally
            theDataset.FreeBookmark(theBookmark);
          end;
        finally
          theDataset.EnableControls;
        end;

        thePDFDoc.SaveToFile(fname);
      finally
        thePDFDoc.Free;
      end;
    end;
end;


end.
