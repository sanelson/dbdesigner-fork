//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the SimpleWebFront-DBDesigner4-Plugin.
// Copyright (C) 2003 Bayer Ulrich
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

unit DialogDirectorySelect;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QButtons, QComCtrls, QFileCtrls, QExtCtrls;

type
  TDialogDirectorySelectForm = class(TForm)
    DirectoryTreeView: TDirectoryTreeView;
    CancelBtn: TSpeedButton;
    OKBtn: TSpeedButton;
    Label1: TLabel;
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DialogDirectorySelectForm: TDialogDirectorySelectForm;

implementation

uses MainDM;

{$R *.xfm}

procedure TDialogDirectorySelectForm.OKBtnClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TDialogDirectorySelectForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TDialogDirectorySelectForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self); //set the correct font

  {$IFDEF LINUX}
    DirectoryTreeView.RootDirectory := '/';
  {$ENDIF}
end;

end.
