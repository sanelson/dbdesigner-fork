unit EditorImage;

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
// Unit EditorImage.pas
// --------------------
// Version 1.1, 23.04.2003, Mike
// Description
//   Editor for the Images
//
// Changes:
//   Version 1.1, 23.04.2003, Mike
//     introduced ApplyChanges func and catch AV when obj has been deleted
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Qt, SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons, EERModel, QExtCtrls;

type
  TEditorImageForm = class(TForm)
    Label1: TLabel;
    ImagNameEd: TEdit;
    StrechImgCBox: TCheckBox;
    ClearImgBtn: TBitBtn;
    RestoreAspectRationBtn: TBitBtn;
    LoadImgBtn: TSpeedButton;
    RestoreSizeBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SetImage(theImg: TEERImage);
    procedure LoadImgBtnClick(Sender: TObject);
    procedure StrechImgCBoxClick(Sender: TObject);
    procedure ClearImgBtnClick(Sender: TObject);
    procedure RestoreAspectRationBtnClick(Sender: TObject);
    procedure RestoreSizeBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplyChanges;
  private
    { Private declarations }
    UndoXML: string;
  public
    { Public declarations }
    EERImage: TEERImage;
    EERModel: TEERModel;
  end;

var
  EditorImageForm: TEditorImageForm;

implementation

uses MainDM, EERDM, GUIDM;

{$R *.xfm}

procedure TEditorImageForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self, True);
end;

procedure TEditorImageForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if(Not(EERModel.ReadOnly))and
    (Not(EERImage.IsLinkedObject))then
    ApplyChanges;

  Application.MainForm.SetFocus;

  Action:=caFree;
end;

procedure TEditorImageForm.ApplyChanges;
var s: string;
begin
  try
    EERImage.ObjName:=ImagNameEd.Text;

    //Log Action
    TEERModel(EERImage.Parent).LogAction(at_EditObj,
      EERImage.Obj_id,
      'BeforeEdit='+UndoXML+#13#10+
      'AfterEdit='+EERImage.GetObjAsXMLModel+#13#10);
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

procedure TEditorImageForm.SetImage(theImg: TEERImage);
begin
  EERModel:=TEERModel(theImg.Parent);
  EERImage:=theImg;

  ImagNameEd.Text:=EERImage.ObjName;

  StrechImgCBox.Checked:=EERImage.GetStrechImg;

  if(EERModel.ReadOnly)or
    (EERImage.IsLinkedObject)then
  begin
    ImagNameEd.Enabled:=False;
    LoadImgBtn.Enabled:=False;
    StrechImgCBox.Enabled:=False;
    RestoreSizeBtn.Enabled:=False;
    RestoreAspectRationBtn.Enabled:=False;
    ClearImgBtn.Enabled:=False;
  end
  else
    //Get Obj XML for undo
    UndoXML:=EERImage.GetObjAsXMLModel;
end;

procedure TEditorImageForm.LoadImgBtnClick(Sender: TObject);
begin
  EERImage.LoadImageFromFile;
end;

procedure TEditorImageForm.StrechImgCBoxClick(Sender: TObject);
begin
  EERImage.SetStrechImg(StrechImgCBox.Checked);

  EERImage.PaintObj(EERImage.Canvas);
end;

procedure TEditorImageForm.ClearImgBtnClick(Sender: TObject);
begin
  EERImage.ClearImg;

  EERImage.PaintObj(EERImage.Canvas);
end;

procedure TEditorImageForm.RestoreAspectRationBtnClick(Sender: TObject);
var theImgSize: TSize;
begin
  theImgSize:=EERImage.GetImgSize;

  if(theImgSize.cx=0)then
    Exit;

  EERImage.Obj_H:=Round(EERImage.Obj_W*(theImgSize.cy/theImgSize.cx));

  //Repaint the Image
  EERImage.RefreshObj;
  {EERImage.StrechedImg.Width:=EERImage.Width;
  EERImage.StrechedImg.Height:=EERImage.Height;
  if(EERImage.GetStrechImg)then
    EERImage.StrechedImg.Canvas.StretchDraw(Rect(0, 0,
      EERImage.Width-2, EERImage.Height-2), EERImage.Img);}
  EERImage.PaintObj(EERImage.Canvas);
  //Picture repainted

end;

procedure TEditorImageForm.RestoreSizeBtnClick(Sender: TObject);
var theImgSize: TSize;
begin
  theImgSize:=EERImage.GetImgSize;

  EERImage.Obj_W:=theImgSize.cx;
  EERImage.Obj_H:=theImgSize.cy;

  //Repaint the Image
  EERImage.RefreshObj;
  {EERImage.StrechedImg.Width:=EERImage.Width;
  EERImage.StrechedImg.Height:=EERImage.Height;
  if(EERImage.StrechImg)then
    EERImage.StrechedImg.Canvas.StretchDraw(Rect(0, 0,
      EERImage.Width-2, EERImage.Height-2), EERImage.Img);}
  EERImage.PaintObj(EERImage.Canvas);
  //Picture repainted
end;

procedure TEditorImageForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_F1)then
    DMMain.ShowHelp('editors', 'imageed');
end;

procedure TEditorImageForm.FormDeactivate(Sender: TObject);
begin
  if(Not(DMMain.IsFormStayingOnTop(self)))then
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RestoreStayOnTopForms, self));
end;

procedure TEditorImageForm.FormShow(Sender: TObject);
begin
  DMMain.RestoreWinPos(self, False);
end;

procedure TEditorImageForm.FormDestroy(Sender: TObject);
begin
  DMMain.SaveWinPos(self, False);
end;

end.
