unit EditorRegion;

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
// Unit EditorRegion.pas
// ---------------------
// Version 1.0, 13.03.2003, Mike
// Description
//   Editor for the Regions
//
// Changes:
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Qt, SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, EERModel, QButtons, QExtCtrls;

type
  TEditorRegionForm = class(TForm)
    Label1: TLabel;
    RegionNameEd: TEdit;
    ColorCBox: TComboBox;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    TableDBCBox: TComboBox;
    Label4: TLabel;
    TableTypeCBox: TComboBox;
    OverwriteTablePrefixCBox: TCheckBox;
    OverwriteTableTypeCBox: TCheckBox;
    CommentsMemo: TMemo;
    Label5: TLabel;
    BottomPnl: TPanel;
    AbortBtn: TSpeedButton;
    SubmitBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SetRegion(theRegion: TEERRegion);
    procedure ColorCBoxCloseUp(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SubmitBtnMouseEnter(Sender: TObject);
    procedure SubmitBtnMouseLeave(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
    procedure AbortBtnClick(Sender: TObject);
    procedure ApplyChanges;
  private
    { Private declarations }
    DiscardChanges: Boolean;

    EERRegion: TEERRegion;
    EERModel: TEERModel;

    UndoXML: string;

    RegionColor: integer;
  public
    { Public declarations }
  end;

var
  EditorRegionForm: TEditorRegionForm;

implementation

uses MainDM, EERDM, GUIDM;

{$R *.xfm}

procedure TEditorRegionForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self, True);

  DiscardChanges:=False;
end;

procedure TEditorRegionForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if(Not(DiscardChanges))and
    (Not(EERModel.ReadOnly))and
    (Not(EERRegion.IsLinkedObject))then
    ApplyChanges;

  Action:=caFree;
end;

procedure TEditorRegionForm.ApplyChanges;
var s: string;
begin
  try
    EERRegion.ObjName:=RegionNameEd.Text;

    EERRegion.TablePrefix:=TableDBCBox.ItemIndex;
    EERRegion.TableType:=TableTypeCBox.ItemIndex;
    //EERRegion.TemporaryTable:=TemporaryCBox.Checked;

    EERRegion.OverwriteTablePrefix:=OverwriteTablePrefixCBox.Checked;
    EERRegion.OverwriteTableType:=OverwriteTableTypeCBox.Checked;
    //EERRegion.OverwriteTemporaryTable:=OverwriteTemporaryTableCBox.Checked;

    EERRegion.Comments:=CommentsMemo.Text;

    TEERModel(EERRegion.Parent).Refresh;

    //Log Action
    TEERModel(EERRegion.Parent).LogAction(at_EditObj,
      EERRegion.Obj_id,
      'BeforeEdit='+UndoXML+#13#10+
      'AfterEdit='+EERRegion.GetObjAsXMLModel+#13#10);

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

procedure TEditorRegionForm.SetRegion(theRegion: TEERRegion);
var i: integer;
begin
  EERModel:=TEERModel(theRegion.Parent);
  EERRegion:=theRegion;

  TableDBCBox.Items.Assign(TEERModel(EERRegion.Parent).TablePrefix);

  //Initialise Region Colors
  ColorCBox.Items.Clear;
  for i:=0 to TEERModel(EERRegion.Parent).RegionColors.Count-1 do
    ColorCBox.Items.Add(Copy(TEERModel(EERRegion.Parent).RegionColors[i], 1, Pos('=', TEERModel(EERRegion.Parent).RegionColors[i])-1));


  RegionNameEd.Text:=EERRegion.ObjName;
  ColorCBox.ItemIndex:=EERRegion.RegionColor;

  TableDBCBox.ItemIndex:=EERRegion.TablePrefix;
  TableTypeCBox.ItemIndex:=EERRegion.TableType;
  //TemporaryCBox.Checked:=EERRegion.TemporaryTable;

  OverwriteTablePrefixCBox.Checked:=EERRegion.OverwriteTablePrefix;
  OverwriteTableTypeCBox.Checked:=EERRegion.OverwriteTableType;
  //OverwriteTemporaryTableCBox.Checked:=EERRegion.OverwriteTemporaryTable;

  CommentsMemo.Text:=EERRegion.Comments;

  RegionColor:=EERRegion.RegionColor;

  if(EERModel.ReadOnly)or
    (EERRegion.IsLinkedObject)then
  begin
    SubmitBtn.Visible:=False;
  end
  else
    //Get Obj XML for undo
    UndoXML:=EERRegion.GetObjAsXMLModel;
end;

procedure TEditorRegionForm.ColorCBoxCloseUp(Sender: TObject);
begin
  EERRegion.RegionColor:=ColorCBox.ItemIndex;
  EERRegion.DoPaint(self);
end;

procedure TEditorRegionForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_F1)then
    DMMain.ShowHelp('editors', 'region');

  if(Key=Key_Escape)then
    AbortBtnClick(self);
end;

procedure TEditorRegionForm.FormDeactivate(Sender: TObject);
begin
  if(Not(DMMain.IsFormStayingOnTop(self)))then
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));
end;

procedure TEditorRegionForm.FormShow(Sender: TObject);
begin
  DMMain.RestoreWinPos(self, False);
end;

procedure TEditorRegionForm.FormDestroy(Sender: TObject);
begin
  DMMain.SaveWinPos(self, False);
end;

procedure TEditorRegionForm.SubmitBtnMouseEnter(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=True;
end;

procedure TEditorRegionForm.SubmitBtnMouseLeave(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=False;
end;

procedure TEditorRegionForm.SubmitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TEditorRegionForm.AbortBtnClick(Sender: TObject);
begin
  EERRegion.RegionColor:=RegionColor;

  EERRegion.Refresh;

  DiscardChanges:=True;

  Close;
end;

end.
