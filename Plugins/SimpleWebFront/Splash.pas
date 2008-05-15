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
// Version 1.0, 13.013.2003, Mike
// Description
//   Contains the splash form class
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QButtons, QTypes;

type
  TSplashForm = class(TForm)
    BGImage: TImage;
    CloseTimer: TTimer;
    Written: TLabel;
    Label4: TLabel;
    VersionLbl: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CloseTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BGImageClick(Sender: TObject);
  private
    { Private declarations }
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
  Font.Name:='Helvetica';
  Font.Size:=10;
  {$ENDIF}

  BGImage.Picture.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+ DataDir + 'splashscreen_swf.png');

  VersionLbl.Left:= 337;//462;
  VersionLbl.Top:= 257; //216;      

  Width:=509;
  Height:=420;

  Top:=(Screen.Height-Height) div 2;
  //2 Monitors
  if(Screen.Width=(Screen.Height/0.75)*2)or
    (Screen.Width=(Screen.Height*1.25)*2)then
    Left:=((Screen.Width div 2)-Width) div 2
  else
    Left:=(Screen.Width-Width) div 2;
  //2 Monitors, different resolution 1280+1152
  if(Screen.Width=1280+1152)then
    Left:=(1280-Width) div 2;


  BGImage.Left:=0;
  BGImage.Top:=0;
end;

procedure TSplashForm.CloseTimerTimer(Sender: TObject);
begin
  CloseTimer.Enabled:=False;
  Close;
end;

procedure TSplashForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if(CloseTimer.Enabled)then
    Action:=caFree;
end;

procedure TSplashForm.BGImageClick(Sender: TObject);
begin
  ModalResult:=mrAbort;
end;

end.
