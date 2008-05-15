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

unit EditorGroup;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QButtons, QExtCtrls, Weboutput;

type
  TEditorGroupForm = class(TForm)
    OKBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    GrpnameEd: TEdit;
    GrpLbl: TLabel;
    Label1: TLabel;
    LineNoEd: TEdit;
    Label2: TLabel;
    ColNoEd: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    PopupCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    editGrp :TGroup;

  public
    function GetGroup() :TGroup;
    procedure SetGroup(group :TGroup);
  end;

var
  EditorGroupForm: TEditorGroupForm;
implementation

uses MainDM, Main, StringConstants, Qt;

{$R *.xfm}

procedure TEditorGroupForm.FormCreate(Sender: TObject);
var strList : TSTringList;
    grpCount :Integer;
begin
  DMMain.InitForm(self); //set the correct font
  
  strList := TStringList.Create;
  MainForm.Output.GetGroupNames(strList);
  grpCount := strList.Count;
  
  strList.Free;
  LineNoEd.Text := IntToStr(grpCount+1);
  ColNoEd.Text := '1';

  editGrp := nil;
end;

function TEditorGroupForm.GetGroup() :TGroup;
var group :TGroup;
begin
  group := TGroup.Create(GrpnameEd.Text, StrToInt(LineNoEd.Text), StrToInt(ColNoEd.Text), PopupCheckBox.Checked);
  GetGroup:= group;
end;

procedure TEditorGroupForm.SetGroup(group :TGroup);
begin
  GrpnameEd.Text := group.Name;
  LineNoEd.Text := IntToStr(group.ShowOnLine);
  ColNoEd.Text := IntToStr(group.ShowInColumn);
  PopupCheckBox.Checked := group.ViewsAsPopup;

  EditGrp := group;
end;

procedure TEditorGroupForm.OKBtnClick(Sender: TObject);
var i : Integer;
    lineNo,colNo : Integer;
begin
  if (GrpnameEd.Text = '') then
  begin
    MessageDlg(NoGroupEntered, mtError,[mbOk], 0, mbOk);
    exit;
  end;

  //a groupname must not contain any chars of the following list: \ / : * ? „ < > |
  //since these are not allowed in filenames
  for i:=1 to length(GrpnameEd.Text ) do
  begin
    if ((GrpnameEd.Text [i]= '\') or
       (GrpnameEd.Text [i]= '/') or
       (GrpnameEd.Text [i]= ':') or
       (GrpnameEd.Text [i]= '*') or
       (GrpnameEd.Text [i]= '?') or
       (GrpnameEd.Text [i]= '"') or
       (GrpnameEd.Text [i]= '<') or
       (GrpnameEd.Text [i]= '>') or
       (GrpnameEd.Text [i]= '|') ) then
    begin
      MessageDlg(Group_CharNotAllowed, mtError,[mbOk], 0, mbOk);
      Exit;
    end;
  end;

  if (LineNoEd.Text = '') then
  begin
    MessageDlg(NoLineEntered, mtError,[mbOk], 0, mbOk);
    exit;
  end;
  if (ColNoEd.Text = '') then
  begin
    MessageDlg(NoColumnEntered, mtError,[mbOk], 0, mbOk);
    exit;
  end;

  //we have to make sure, that the pair (lineNumber,ColumnNumber) doesn't exist yet
  lineNo := strToInt(lineNoEd.Text);
  colNo := StrToInt(colNoEd.Text);

  if ((NOT MainForm.Output.GroupPositionFree(lineNo, colNo)) AND
      (MainForm.Output.GetGroupAtPosition(lineNo, ColNo) <> editGrp) ) THEN
  begin
    MessageDlg(LineColumnCombinationNotUnique, mtError,[mbOk], 0, mbOk);
    exit;
  end;


  
  ModalResult := mrOK;
end;

procedure TEditorGroupForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TEditorGroupForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_Return)then
    OKBtnClick(self);
end;

end.
