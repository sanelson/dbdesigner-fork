unit EditorRelation;

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
//
// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------
//
// Unit EditorRelation.pas
// -----------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Editor for the Relations
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Qt, SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, EERModel, QGrids, QComCtrls, QExtCtrls, QButtons;

type
  TEditorRelationForm = class(TForm)
    Label1: TLabel;
    RelationNameEd: TEdit;
    Label17: TLabel;
    RelKindComboBox: TComboBox;
    GroupBox3: TGroupBox;
    FKGrid: TStringGrid;
    Panel1: TPanel;
    RelPageControl: TPageControl;
    CommentSheet: TTabSheet;
    OptionalSheet: TTabSheet;
    RefSheet: TTabSheet;
    CreateRefDefCBox: TCheckBox;
    MatchingLbl: TLabel;
    MatchLU: TComboBox;
    OnDeleteLbl: TLabel;
    OnDeleteLU: TComboBox;
    OnUpdateLbl: TLabel;
    OnUpdateLU: TComboBox;
    CommentsMemo: TMemo;
    OptStartCBox: TCheckBox;
    OptEndCBox: TCheckBox;
    RelPageCBox: TComboBox;
    GroupBox1: TGroupBox;
    TblSourceEd: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    TblDestEd: TEdit;
    BottomPnl: TPanel;
    AbortBtn: TSpeedButton;
    SubmitBtn: TSpeedButton;
    Label4: TLabel;
    VisibilityCBox: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure ApplyChanges;
    procedure SetRelation(theRelation: TEERRel);
    procedure CreateRefDefCBoxClick(Sender: TObject);
    procedure RelPageCBoxCloseUp(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SubmitBtnMouseEnter(Sender: TObject);
    procedure SubmitBtnMouseLeave(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
    procedure AbortBtnClick(Sender: TObject);
  private
    { Private declarations }
    DiscardChanges: Boolean;

    UndoXML: string;
  public
    { Public declarations }
    EERRel: TEERRel;
    EERModel: TEERModel;
  end;

var
  EditorRelationForm: TEditorRelationForm;

implementation

uses MainDM, EERDM, GUIDM;

{$R *.xfm}

procedure TEditorRelationForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self, True);

  DiscardChanges:=False;

  RelPageControl.ActivePageIndex:=0;
end;

procedure TEditorRelationForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if(Not(DiscardChanges))and
    (Not(EERModel.ReadOnly))and
    (Not(EERRel.IsLinkedObject))then
    ApplyChanges;

  Action:=caFree;
end;

procedure TEditorRelationForm.ApplyChanges;
var i: integer;
  s: string;
begin
  try
    if(RelationNameEd.Text<>'')then
      EERRel.ObjName:=RelationNameEd.Text;

    EERRel.RelKind:=RelKindComboBox.ItemIndex;

    EERRel.Invisible:=(VisibilityCBox.ItemIndex=2);
    EERRel.Splitted:=(VisibilityCBox.ItemIndex=1);

    EERRel.OptionalStart:=OptStartCBox.Checked;
    EERRel.OptionalEnd:=OptEndCBox.Checked;

    //store FKs
    for i:=0 to EERRel.FKFields.Count-1 do
    begin
      if(EERRel.SrcTbl=EERRel.DestTbl)and
        (EERRel.FKFields.Names[i]=FKGrid.Cells[1, i+1])then
      begin
        MessageDlg(DMMain.GetTranslatedMessage(
          'When using a recursive relation on the table itself, '+
          'you must use a different Destination Name for the column %s',
          83, EERRel.FKFields.Names[i]), mtError, [mbOK], 0);

        Abort;
      end;

      if(FKGrid.Cells[1, i+1]<>'')then
        EERRel.FKFields.ValueFromIndex[i]:=FKGrid.Cells[1, i+1];
      EERRel.FKFieldsComments[i]:=FKGrid.Cells[2, i+1];
    end;


    EERRel.CreateRefDef:=CreateRefDefCBox.Checked;

    EERRel.RefDef.Values['Matching']:=IntToStr(MatchLU.ItemIndex);
    EERRel.RefDef.Values['OnDelete']:=IntToStr(OnDeleteLU.ItemIndex);
    EERRel.RefDef.Values['OnUpdate']:=IntToStr(OnUpdateLU.ItemIndex);

    EERRel.Comments:=CommentsMemo.Text;

    TEERModel(EERRel.Parent).CheckAllRelations;
    TEERModel(EERRel.Parent).Refresh;

    TEERModel(EERRel.Parent).LastRelEditorPage:=RelPageControl.ActivePageIndex;

    //Log Action
    TEERModel(EERRel.Parent).LogAction(at_EditObj,
      EERRel.Obj_id,
      'BeforeEdit='+UndoXML+#13#10+
      'AfterEdit='+EERRel.GetObjAsXMLModel+#13#10);
  except
    on x: Exception do
    begin
      if(Not(x.ClassNameIs('EAbort')))then
      begin
        s:=DMMain.GetTranslatedMessage('Changes cannot be applied to object.'+#13#10+
          'The object may have been deleted.', 1);
        //Don't display access violation
        if(Copy(x.Message, 1, 10)<>'Access vio')then
          s:=s+#13#10#13#10+x.Message;

        MessageDlg(s, mtError, [mbOK], 0);
      end
      else
        raise;
    end;
  end;
end;

procedure TEditorRelationForm.SetRelation(theRelation: TEERRel);
var i: integer;
begin
  EERModel:=TEERModel(theRelation.Parent);
  EERRel:=theRelation;

  RelationNameEd.Text:=EERRel.ObjName;
  RelKindComboBox.ItemIndex:=EERRel.RelKind;

  if(EERRel.Invisible)then
    VisibilityCBox.ItemIndex:=2
  else if(EERRel.Splitted)then
    VisibilityCBox.ItemIndex:=1
  else
    VisibilityCBox.ItemIndex:=0;

  OptStartCBox.Checked:=EERRel.OptionalStart;
  OptEndCBox.Checked:=EERRel.OptionalEnd;


  //Clear FK-Grid
  for i:=0 to FKGrid.ColCount-1 do
    FKGrid.Cols[i].Clear;

  FKGrid.RowCount:=EERRel.FKFields.Count+1;

  //Headers
  FKGrid.Cells[0, 0]:='Source Column';
  FKGrid.Cells[1, 0]:='Dest. Name';
  FKGrid.ColWidths[1]:=100;
  FKGrid.Cells[2, 0]:='Comment';
  FKGrid.ColWidths[2]:=110;

  //FKs
  for i:=0 to EERRel.FKFields.Count-1 do
  begin
    FKGrid.Cells[0, i+1]:=EERRel.FKFields.Names[i];
    FKGrid.Cells[1, i+1]:=EERRel.FKFields.ValueFromIndex[i];
    if(i<EERRel.FKFieldsComments.Count)then
      FKGrid.Cells[2, i+1]:=EERRel.FKFieldsComments[i]
    else
      FKGrid.Cells[2, i+1]:='';
  end;

  CreateRefDefCBox.Checked:=EERRel.CreateRefDef;

  if(EERRel.RefDef.Values['Matching']<>'')then
    MatchLU.ItemIndex:=StrToInt(EERRel.RefDef.Values['Matching']);
  if(EERRel.RefDef.Values['OnDelete']<>'')then
    OnDeleteLU.ItemIndex:=StrToInt(EERRel.RefDef.Values['OnDelete']);
  if(EERRel.RefDef.Values['OnUpdate']<>'')then
    OnUpdateLU.ItemIndex:=StrToInt(EERRel.RefDef.Values['OnUpdate']);

  CreateRefDefCBoxClick(self);

  CommentsMemo.Text:=EERRel.Comments;

  RelPageCBox.ItemIndex:=TEERModel(EERRel.Parent).LastRelEditorPage;
  RelPageControl.ActivePageIndex:=TEERModel(EERRel.Parent).LastRelEditorPage;


  try
    TblSourceEd.Text:=EERRel.SrcTbl.ObjName;
    TblDestEd.Text:=EERRel.DestTbl.ObjName;
  except
  end;

  if(EERModel.ReadOnly)or
    (EERRel.IsLinkedObject)then
  begin
    SubmitBtn.Visible:=False;
  end
  else
    //Get Obj XML for undo
    UndoXML:=EERRel.GetObjAsXMLModel;
end;

procedure TEditorRelationForm.CreateRefDefCBoxClick(Sender: TObject);
begin
  MatchingLbl.Enabled:=CreateRefDefCBox.Checked;
  MatchLU.Enabled:=CreateRefDefCBox.Checked;
  OnDeleteLbl.Enabled:=CreateRefDefCBox.Checked;
  OnDeleteLU.Enabled:=CreateRefDefCBox.Checked;
  OnUpdateLbl.Enabled:=CreateRefDefCBox.Checked;
  OnUpdateLU.Enabled:=CreateRefDefCBox.Checked;
end;

procedure TEditorRelationForm.RelPageCBoxCloseUp(Sender: TObject);
begin
  RelPageControl.ActivePageIndex:=RelPageCBox.ItemIndex;
end;

procedure TEditorRelationForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_F1)then
    DMMain.ShowHelp('editors', 'relation');

  if(Key=Key_Escape)then
    AbortBtnClick(self);
end;

procedure TEditorRelationForm.FormDeactivate(Sender: TObject);
begin
  if(Not(DMMain.IsFormStayingOnTop(self)))then
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));
end;

procedure TEditorRelationForm.FormDestroy(Sender: TObject);
begin
  DMMain.SaveWinPos(self, False);
end;

procedure TEditorRelationForm.FormShow(Sender: TObject);
begin
  DMMain.RestoreWinPos(self, False);
end;

procedure TEditorRelationForm.SubmitBtnMouseEnter(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=True;
end;

procedure TEditorRelationForm.SubmitBtnMouseLeave(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=False;
end;

procedure TEditorRelationForm.SubmitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TEditorRelationForm.AbortBtnClick(Sender: TObject);
begin
  DiscardChanges:=True;

  Close;
end;

end.
