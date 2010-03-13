unit Splash;

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
// Unit Splash.pas
// ---------------
// Version Fork 1.5, 13.10.2010, JP
// Version 1.0, 13.013.2003, Mike
// Description
//   Contains the splash form class
//
// Changes:
//   Version 1.1, 13.03.2003, Mike
//     initial version
// Version Fork 1.5, 13.10.2010, JP: changes in the splash screen.
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QButtons, QTypes;

type
  TSplashForm = class(TForm)
    CloseTimer: TTimer;
    InfoLbl1: TLabel;
    VersionLbl: TLabel;
    InfoLbl5: TLabel;
    InfoLbl2: TLabel;
    InfoLbl4: TLabel;
    InfoLbl3: TLabel;
    InfoLbl6: TLabel;
    InfoLbl7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKSBtnClick(Sender: TObject);
    procedure AbortSBtnClick(Sender: TObject);
    procedure CloseTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClick(Sender: TObject);
  private
    { Private declarations }
    SplashImg: TBitmap;
  public
    { Public declarations }
  end;

var
  SplashForm: TSplashForm;

implementation

uses Main;

{$R *.xfm}

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  {$IFDEF LINUX}
  Font.Name:='Nimbus Sans L';
  Font.Size:=10;
  {$ENDIF}

  SplashImg:=TBitmap.Create;
  SplashImg.LoadFromFile(ExtractFilePath(Application.ExeName)+
    'Gfx'+PathDelim+'splashscreen.png');

  VersionLbl.Left:=650;
  VersionLbl.Top:=298;

  Width:=700;
  Height:=358;

  Top:=(Screen.Height-Height) div 2;
  //2 Monitors
  if(Screen.Width=(Screen.Height/0.75)*2)or
    (Screen.Width=(Screen.Height*1.25)*2)then
    Left:=((Screen.Width div 2)-Width) div 2
  else
    Left:=(Screen.Width-Width) div 2;
  //2 Monitors, different resolutions 1280+1152
  if(Screen.Width=1280+1152)then
    Left:=(1280-Width) div 2;
end;

procedure TSplashForm.FormDestroy(Sender: TObject);
begin
  SplashImg.Free;
end;

procedure TSplashForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TSplashForm.OKSBtnClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TSplashForm.AbortSBtnClick(Sender: TObject);
begin
  ModalResult:=mrAbort;
end;

procedure TSplashForm.CloseTimerTimer(Sender: TObject);
begin
  CloseTimer.Enabled:=False;
  Close;
end;

procedure TSplashForm.FormPaint(Sender: TObject);
var i: integer;
  theLbl: TLabel;
begin
  if(Assigned(SplashImg))then
  begin
    Canvas.Draw(0, 0, SplashImg);

{$IFDEF LINUX}
    Canvas.Font.Name:='Nimbus Sans L';
    Canvas.Font.Height:=9;
{$ELSE}
    Canvas.Font.Name:='Microsoft Sans Serif';
    Canvas.Font.Height:=9;
{$ENDIF}

//    for i:=1 to 7 do
//    begin
//      theLbl:=TLabel(FindComponent('InfoLbl'+IntToStr(i)));
//      if(theLbl<>nil)then
//      begin
//        Canvas.TextOut(theLbl.Left, theLbl.Top, theLbl.Caption);
//      end;
//    end;

    Canvas.TextOut(VersionLbl.Left, VersionLbl.Top, VersionLbl.Caption);
  end;
end;

procedure TSplashForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ModalResult:=mrAbort;
end;

procedure TSplashForm.FormClick(Sender: TObject);
begin
  Close;
end;

end.
