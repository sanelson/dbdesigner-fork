unit EditorDatatype;

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
// Unit EditorDatatype.pas
// -----------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Editor for the Datatypes
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Qt, SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, EERModel, QExtCtrls, QCheckLst, QButtons;

type
  TEditorDatatypeForm = class(TForm)
    Label1: TLabel;
    DatatypeNameEd: TEdit;
    Label3: TLabel;
    GroupLU: TComboBox;
    DescriptionMemo: TMemo;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    ParamRequiredCBox: TCheckBox;
    GroupBox2: TGroupBox;
    OptionsCListBox: TCheckListBox;
    AddOptionBtn: TSpeedButton;
    DelOptionBtn: TSpeedButton;
    AddParamBtn: TSpeedButton;
    DelParamBtn: TSpeedButton;
    ParamsListBox: TListBox;
    SynGroupEd: TEdit;
    Label4: TLabel;
    EditValueAsStringCBox: TCheckBox;
    BottomPnl: TPanel;
    AbortBtn: TSpeedButton;
    SubmitBtn: TSpeedButton;
    Bevel1: TBevel;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    PhysicalDatatypeEd: TEdit;
    EnablePhysicalMappingCBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure SetDataType(theModel: TEERModel; theDatatype: TEERDatatype);
    procedure AddOptionBtnClick(Sender: TObject);
    procedure DelOptionBtnClick(Sender: TObject);
    procedure AddParamBtnClick(Sender: TObject);
    procedure DelParamBtnClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SubmitBtnMouseLeave(Sender: TObject);
    procedure AbortBtnClick(Sender: TObject);
    procedure SubmitBtnMouseEnter(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    EERModel: TEERModel;
    Datatype: TEERDatatype;
  end;

var
  EditorDatatypeForm: TEditorDatatypeForm;

implementation

uses MainDM, EERDM, GUIDM;

{$R *.xfm}

procedure TEditorDatatypeForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self, True);
end;

procedure TEditorDatatypeForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TEditorDatatypeForm.SetDataType(theModel: TEERModel; theDatatype: TEERDatatype);
var i: integer;
begin
  EERModel:=theModel;
  Datatype:=theDatatype;

  GroupLU.Items.Clear;
  for i:=0 to EERModel.DatatypeGroups.Count-1 do
    GroupLU.Items.Add(TEERDatatypeGroup(EERModel.DatatypeGroups[i]).GroupName);

  DatatypeNameEd.Text:=Datatype.TypeName;
  GroupLU.ItemIndex:=Datatype.group;
  DescriptionMemo.Text:=Datatype.description;

  SynGroupEd.Text:=IntToStr(Datatype.SynonymGroup);

  for i:=0 to Datatype.ParamCount-1 do
    ParamsListBox.Items.Add(Datatype.Param[i]);

  for i:=0 to Datatype.OptionCount-1 do
  begin
    OptionsCListBox.Items.Add(Datatype.Options[i]);
    OptionsCListBox.Checked[i]:=Datatype.OptionDefaults[i];
  end;

  ParamRequiredCBox.Checked:=Datatype.ParamRequired;
  EditValueAsStringCBox.Checked:=Datatype.EditParamsAsString;

  EnablePhysicalMappingCBox.Checked:=Datatype.PhysicalMapping;
  PhysicalDatatypeEd.Text:=Datatype.PhysicalTypeName;

  if(EERModel.ReadOnly)then
    SubmitBtn.Visible:=False;
end;

procedure TEditorDatatypeForm.AddOptionBtnClick(Sender: TObject);
var s: string;
begin
  if(DMMain.ShowStringEditor('New Option', 'New Option:', s))then
    if(s<>'')then
      OptionsCListBox.Items.Add(s);
end;

procedure TEditorDatatypeForm.DelOptionBtnClick(Sender: TObject);
var i: integer;
begin
  i:=0;
  while(i<OptionsCListBox.Items.Count)do
  begin
    if(OptionsCListBox.Selected[i])then
      OptionsCListBox.Items.Delete(i)
    else
      inc(i);
  end;
end;

procedure TEditorDatatypeForm.AddParamBtnClick(Sender: TObject);
var s: string;
begin
  if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('New Parameter', 191),
    DMMain.GetTranslatedMessage('New Param:', 192), s))then
    if(s<>'')then
      ParamsListBox.Items.Add(s);
end;

procedure TEditorDatatypeForm.DelParamBtnClick(Sender: TObject);
var i: integer;
begin
  i:=0;
  while(i<ParamsListBox.Items.Count)do
  begin
    if(ParamsListBox.Selected[i])then
      ParamsListBox.Items.Delete(i)
    else
      inc(i);
  end;
end;

procedure TEditorDatatypeForm.SubmitBtnClick(Sender: TObject);
var i: integer;
begin
  //Store changes
  if(DatatypeNameEd.Text<>'')then
    Datatype.TypeName:=DatatypeNameEd.Text;

  Datatype.group:=GroupLU.ItemIndex;
  Datatype.description:=DescriptionMemo.Text;

  Datatype.ParamCount:=ParamsListBox.Items.Count;
  for i:=0 to Datatype.ParamCount-1 do
    Datatype.Param[i]:=ParamsListBox.Items[i];

  Datatype.OptionCount:=OptionsCListBox.Items.Count;
  for i:=0 to Datatype.OptionCount-1 do
  begin
    Datatype.Options[i]:=OptionsCListBox.Items[i];
    Datatype.OptionDefaults[i]:=OptionsCListBox.Checked[i];
  end;

  Datatype.SynonymGroup:=StrToInt(SynGroupEd.Text);

  Datatype.ParamRequired:=ParamRequiredCBox.Checked;
  Datatype.EditParamsAsString:=EditValueAsStringCBox.Checked;

  Datatype.PhysicalMapping:=EnablePhysicalMappingCBox.Checked;
  Datatype.PhysicalTypeName:=PhysicalDatatypeEd.Text;

  ModalResult:=mrOK;
end;

procedure TEditorDatatypeForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_F1)then
    DMMain.ShowHelp('editors', 'datatypeed');

  if(Key=Key_Escape)then
    AbortBtnClick(self);
end;

procedure TEditorDatatypeForm.FormDeactivate(Sender: TObject);
begin
  if(Not(DMMain.IsFormStayingOnTop(self)))then
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));
end;

procedure TEditorDatatypeForm.FormShow(Sender: TObject);
begin
  DMMain.RestoreWinPos(self, False);
end;

procedure TEditorDatatypeForm.FormDestroy(Sender: TObject);
begin
  DMMain.SaveWinPos(self, False);
end;

procedure TEditorDatatypeForm.SubmitBtnMouseEnter(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=True;
end;

procedure TEditorDatatypeForm.SubmitBtnMouseLeave(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=False;
end;

procedure TEditorDatatypeForm.AbortBtnClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;



end.
