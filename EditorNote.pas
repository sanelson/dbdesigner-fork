unit EditorNote;

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
// Unit EditorNote.pas
// -------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Editor for the Notes
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, EERModel, Qt, QButtons, QExtCtrls;

type
  TEditorNoteForm = class(TForm)
    NoteMemo: TMemo;
    Label2: TLabel;
    BottomPnl: TPanel;
    AbortBtn: TSpeedButton;
    SubmitBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure ApplyChanges;
    procedure SetNote(theNote: TEERNote);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    EERNote: TEERNote;
    EERModel: TEERModel;
  end;

var
  EditorNoteForm: TEditorNoteForm;

implementation

uses MainDM, EERDM, GUIDM;

{$R *.xfm}

procedure TEditorNoteForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self, True);

  DiscardChanges:=False;
end;

procedure TEditorNoteForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if(Not(DiscardChanges))and
    (Not(EERModel.ReadOnly))and
    (Not(EERNote.IsLinkedObject))then
    ApplyChanges;

  Action:=caFree;
end;

procedure TEditorNoteForm.ApplyChanges;
var s: string;
begin
  try
    EERNote.SetNoteText(NoteMemo.Text);

    TEERModel(EERNote.Parent).Refresh;

    //Log Action
    TEERModel(EERNote.Parent).LogAction(at_EditObj,
      EERNote.Obj_id,
      'BeforeEdit='+UndoXML+#13#10+
      'AfterEdit='+EERNote.GetObjAsXMLModel+#13#10);
  except
    on x: Exception do
    begin
      s:=DMMain.GetTranslatedMessage('Changes cannot be applied to object.'+#13#10+
        'The object may have been deleted.', 1);
      //Don't display access violation
      if(Copy(x.Message, 1, 10)<>'Access vio')then
        s:=s+#13#10#13#10+x.Message;

      MessageDlg(s, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TEditorNoteForm.SetNote(theNote: TEERNote);
begin
  EERModel:=TEERModel(theNote.Parent);
  EERNote:=theNote;

  NoteMemo.Text:=EERNote.GetNoteText;

  if(Copy(EERNote.GetNoteText, 1, 10)=
    Copy(DMMain.GetTranslatedMessage('Doubleclick on the Note'+#13#10+'to edit the Text.', 21),
      1, 10))then
    NoteMemo.SelectAll;

  if(EERModel.ReadOnly)or
    (EERNote.IsLinkedObject)then
  begin
    SubmitBtn.Visible:=False;
  end
  else
    //Get Obj XML for undo
    UndoXML:=EERNote.GetObjAsXMLModel;
end;

procedure TEditorNoteForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_F1)then
    DMMain.ShowHelp('editors', 'note');

  if(Key=Key_Escape)then
    AbortBtnClick(self);
end;

procedure TEditorNoteForm.FormDeactivate(Sender: TObject);
begin
  if(Not(DMMain.IsFormStayingOnTop(self)))then
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));
end;

procedure TEditorNoteForm.FormShow(Sender: TObject);
begin
  DMMain.RestoreWinPos(self, False);
end;

procedure TEditorNoteForm.FormDestroy(Sender: TObject);
begin
  DMMain.SaveWinPos(self, False);
end;

procedure TEditorNoteForm.SubmitBtnMouseEnter(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=True;
end;

procedure TEditorNoteForm.SubmitBtnMouseLeave(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=False;
end;

procedure TEditorNoteForm.SubmitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TEditorNoteForm.AbortBtnClick(Sender: TObject);
begin
  DiscardChanges:=True;

  Close;
end;

end.
