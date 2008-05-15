unit DBConnLogin;

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
// Unit DBConnLogin.pas
// --------------------
// Version 1.1, 26.03.2003, Mike
// Description
//   Used by the DBConn Module to let the user specify username and password
//
// Changes:
//   Version 1.1, 26.03.2003, Mike
//     Changed Echo-mode for Password field
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons;

type
  TDBConnLoginForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    UsernameEd: TEdit;
    Label2: TLabel;
    PwdEd: TEdit;
    ConnectBtn: TBitBtn;
    CancelBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);

    procedure SetData(Username: string);
    function GetUserName: string;
    function GetPassword: string;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  DBConnLoginForm: TDBConnLoginForm;

implementation

uses MainDM;

{$R *.xfm}

procedure TDBConnLoginForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);
end;

procedure TDBConnLoginForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TDBConnLoginForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TDBConnLoginForm.SetData(Username: string);
begin
  if(Username<>'')then
  begin
    UsernameEd.Text:=Username;

    ActiveControl:=PwdEd;
  end;
end;

function TDBConnLoginForm.GetUserName: string;
begin
  GetUserName:=UsernameEd.Text;
end;

function TDBConnLoginForm.GetPassword: string;
begin
  GetPassword:=PwdEd.Text;
end;

procedure TDBConnLoginForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult:=mrAbort;
end;

procedure TDBConnLoginForm.FormShow(Sender: TObject);
begin
  try
    if(CanFocus)then
      SetFocus;
  except
  end;
end;

end.
