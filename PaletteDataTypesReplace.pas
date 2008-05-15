unit PaletteDataTypesReplace;

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
// Unit PaletteDataTypesReplace.pas
// --------------------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Contains a help from class for the datatype palette
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons, QExtCtrls, EERModel;

type
  TPaletteDataTypesReplaceForm = class(TForm)
    Bevel1: TBevel;
    SubmitBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    SourceCBox: TComboBox;
    Label1: TLabel;
    DestCBox: TComboBox;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    AllTablesRBtn: TRadioButton;
    SelTablesRBtn: TRadioButton;
    SourceParamEd: TEdit;
    MatchParamsCBox: TCheckBox;
    Label3: TLabel;
    DestParamEd: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure SetModel(theModel: TEERModel);
    procedure SourceParamEdClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SourceCBoxCloseUp(Sender: TObject);
    procedure DestCBoxCloseUp(Sender: TObject);
    procedure DestParamEdClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
    idList: TStringList;
  public
    { Public-Deklarationen }
    EERModel: TEERModel;
  end;

var
  PaletteDataTypesReplaceForm: TPaletteDataTypesReplaceForm;

implementation

uses EditorTableFieldParam, MainDM;

{$R *.xfm}

procedure TPaletteDataTypesReplaceForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);
  
  idList:=TStringList.Create;
end;

procedure TPaletteDataTypesReplaceForm.FormDestroy(Sender: TObject);
begin
  idList.Free;
end;

procedure TPaletteDataTypesReplaceForm.SetModel(theModel: TEERModel);
var i: integer;
begin
  EERModel:=theModel;

  SourceCBox.Items.Clear;
  idList.Clear;
  for i:=0 to EERModel.Datatypes.Count-1 do
  begin
    SourceCBox.Items.Add(TEERDatatype(EERModel.Datatypes[i]).TypeName);
    idList.Add(IntToStr(TEERDatatype(EERModel.Datatypes[i]).id));
  end;

  DestCBox.Items.Assign(SourceCBox.Items);

  SourceCBox.ItemIndex:=0;
  DestCBox.ItemIndex:=0;
end;

procedure TPaletteDataTypesReplaceForm.SourceParamEdClick(Sender: TObject);
begin
  EditorTableFieldParamForm:=TEditorTableFieldParamForm.Create(self);
  try
    EditorTableFieldParamForm.SetData(
      EERModel.GetDataType(StrToInt(idList[SourceCBox.ItemIndex])),
      SourceParamEd.Text,
      Left+SourceParamEd.Left-52,
      Top+SourceParamEd.Top+17);

    if(EditorTableFieldParamForm.ShowModal=mrOK)then
    begin
      SourceParamEd.Text:=EditorTableFieldParamForm.GetParams;
    end;
  finally
    EditorTableFieldParamForm.Free;
  end;
end;

procedure TPaletteDataTypesReplaceForm.DestParamEdClick(Sender: TObject);
begin
  EditorTableFieldParamForm:=TEditorTableFieldParamForm.Create(self);
  try
    EditorTableFieldParamForm.SetData(
      EERModel.GetDataType(StrToInt(idList[SourceCBox.ItemIndex])),
      DestParamEd.Text,
      Left+DestParamEd.Left-52,
      Top+DestParamEd.Top+17);

    if(EditorTableFieldParamForm.ShowModal=mrOK)then
    begin
      DestParamEd.Text:=EditorTableFieldParamForm.GetParams;
    end;
  finally
    EditorTableFieldParamForm.Free;
  end;
end;

procedure TPaletteDataTypesReplaceForm.SourceCBoxCloseUp(Sender: TObject);
begin
  SourceParamEd.Text:='';
end;

procedure TPaletteDataTypesReplaceForm.DestCBoxCloseUp(Sender: TObject);
begin
  DestParamEd.Text:='';
end;



procedure TPaletteDataTypesReplaceForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult:=mrAbort;
end;

procedure TPaletteDataTypesReplaceForm.SubmitBtnClick(Sender: TObject);
var i, j: integer;
  theDatatype: TEERDatatype;
  theCol: TEERColumn;
begin
  //Replace datatype in Tables
  for i:=0 to EERModel.ComponentCount-1 do
  begin
    if(EERModel.Components[i].ClassnameIs('TEERTable'))then
    begin
      //If table is not selected and only selected tables have to be changed
      if(SelTablesRBtn.Checked)and(Not(TEERTable(EERModel.Components[i]).Selected))then
        continue;

      for j:=0 to TEERTable(EERModel.Components[i]).Columns.Count-1 do
      begin
        theCol:=TEERColumn(TEERTable(EERModel.Components[i]).Columns[j]);
        theDatatype:=EERModel.GetDataType(theCol.idDatatype);

        //if the datatype does match
        if(theDatatype.id=StrToInt(idList[SourceCBox.ItemIndex]))then
        begin
          //and the params do match
          if(MatchParamsCBox.Checked)and(theCol.DatatypeParams<>SourceParamEd.Text)then
            continue;

          //assign new datatype
          theCol.idDatatype:=StrToInt(idList[DestCBox.ItemIndex]);
          //assign params
          theCol.DatatypeParams:=DestParamEd.Text;
        end;
      end;
    end;
  end;

  EERModel.Refresh;

  //ModalResult:=mrOK;
end;

end.
