unit EER;

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
// Unit EER.pas
// ------------
// Version 1.1, 28.03.2003, Mike
// Description
//   Contains a MDI Child form which displays the EERModel
//
// Changes:
//   Version 1.1, 28.03.2003, Mike
//     Added QEventType_RefreshGridBtn in FormActivate
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QImgList, QMenus, QTypes, EERModel, Qt, IniFiles;

type
  TEERForm = class(TForm)
    PopupMenu1: TPopupMenu;
    Zoom100MI: TMenuItem;
    NavPaletteTimer: TTimer;
    SelectAllMI: TMenuItem;
    N1: TMenuItem;
    Zoome50MI: TMenuItem;
    Zoom75MI: TMenuItem;
    GroupBox1: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Zoom100MIClick(Sender: TObject);
    procedure NavPaletteTimerTimer(Sender: TObject);
    procedure SelectAllMIClick(Sender: TObject);
    procedure Zoome50MIClick(Sender: TObject);
    procedure Zoom75MIClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    function SaveAs: Boolean;
  private
    { Private declarations }
    PrevXPos, PrevYPos: integer;
    FormIsClosing: Boolean;
  public
    { Public declarations }

    EERModel: TEERModel;
    theFormMenuItem: TMenuItem;
  end;


var
  EERForm: TEERForm;

implementation

uses MainDM, EERDM;

{$R *.xfm}


procedure TEERForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);

  //OnKeyDown:=TForm(Sender).OnKeyDown;
  //OnKeyUp:=TForm(Sender).OnKeyUp;

  theFormMenuItem:=nil;
  FormIsClosing:=False;

  try
    EERModel:=TEERModel.Create(self);

    Font.Name:=EERModel.DefModelFont;
    Canvas.Font.Name:=EERModel.DefModelFont;
    
    EERModel.PopupMenu:=PopupMenu1;

    //Initial ModelName and Filename
    EERModel.SetModelName('Noname'+DMEER.GetNextNonameNumber);
    EERModel.ModelFilename:=EERModel.GetModelName+'.xml';

    //Caption:='DB Model | '+EERModel.GetModelName;
  except
    on x: Exception do
    begin
      MessageDlg(DMMain.GetTranslatedMessage('Not able to create EER Model:'#13#10#13#10+'%s',
        193, x.Message), mtError, [mbOK], 0);
      //Application.Terminate;
    end;
  end;

  //Use the Timer to track ScrollBar Action
  PrevXPos:=-1;
  PrevYPos:=-1;
  NavPaletteTimer.Enabled:=True;

  Cursor:=crArrow;
end;

procedure TEERForm.FormActivate(Sender: TObject);
var theEvent: QCustomEventH;
begin
  if(Not(FormIsClosing))then
  begin
    DMEER.UpdateStatusBar;

    //Show Datatypes in Palette
    theEvent := QCustomEvent_create(QEventType_RefreshDataTypesPalette, EERModel);
    sendCLXEvent(Application.MainForm.Handle, theEvent);

    //Show Tables in Palette
    theEvent := QCustomEvent_create(QEventType_RefreshModelPalette, EERModel);
    sendCLXEvent(Application.MainForm.Handle, theEvent);

    //Refresh Nav Img
    theEvent := QCustomEvent_create(QEventType_RefreshNavPalette, EERModel);
    sendCLXEvent(Application.MainForm.Handle, theEvent);

    //Refresh Grid Btn
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RefreshGridBtn, EERModel));

    if(theFormMenuItem<>nil)then
      theFormMenuItem.Checked:=True;

    theEvent := QCustomEvent_create(QEventType_SetSaveImgs, nil);
    sendCLXEvent(Application.MainForm.Handle, theEvent);
  end;
end;

procedure TEERForm.FormPaint(Sender: TObject);
var p1, p2: TPoint;
begin
  with Canvas do
  begin
    if(EERModel.Width<Width)or(EERModel.Height<Height)then
    begin
      Pen.Color:=clGray;
      Brush.Color:=clBackground;

      p1:=Point((Width-EERModel.Width) div 2-1,
       (Height-EERModel.Height) div 2-1);

      p2:=Point((Width+EERModel.Width) div 2+1,
        (Height+EERModel.Height) div 2+1);

      FillRect(Rect(0, 0, width, p1.Y));
      FillRect(Rect(0, p1.Y, width, height));
      FillRect(Rect(0, p1.Y, p1.X, p2.Y));
      FillRect(Rect(p2.X, p1.Y, width, p2.Y));

      MoveTo(p1.X, p1.Y);
      LineTo(p1.X, P2.Y);
      LineTo(p2.X, P2.Y);
      LineTo(P2.X, P1.Y);
      LineTo(P1.X, P1.Y);
    end;
  end;
end;

procedure TEERForm.FormResize(Sender: TObject);
begin
  //ATTENTION!!!!
  //ADD CODE TO CAPTURE DISAPPEAR OF SCROLLBARS
  if(EERModel.Width<Width)then
    EERModel.Left:=(Width-EERModel.Width) div 2;

  if(EERModel.Height<Height)then
    EERModel.Top:=(Height-EERModel.Height) div 2;
end;

procedure TEERForm.FormClose(Sender: TObject; var Action: TCloseAction);
var theEvent: QCustomEventH;
begin
  FormIsClosing:=True;

  //Remove the ChildForms MenuItem from the Window Menu
  theEvent := QCustomEvent_create(QEventType_RemoveChildFormsMenuItem, theFormMenuItem);
  sendCLXEvent(Application.MainForm.Handle, theEvent);

  //Clear Datatypes in Palette
  theEvent := QCustomEvent_create(QEventType_RefreshDataTypesPalette, nil);
  sendCLXEvent(Application.MainForm.Handle, theEvent);

  //Clear Tables in Palette
  theEvent := QCustomEvent_create(QEventType_RefreshModelPalette, nil);
  sendCLXEvent(Application.MainForm.Handle, theEvent);

  //Clear Img in Nav
  theEvent := QCustomEvent_create(QEventType_ClearNavImg, nil);
  sendCLXEvent(Application.MainForm.Handle, theEvent);

  theEvent := QCustomEvent_create(QEventType_EnableMainFormRefreshTmr, nil);
  sendCLXEvent(Application.MainForm.Handle, theEvent);

  Action:=caFree;
end;


procedure TEERForm.NavPaletteTimerTimer(Sender: TObject);
begin
  if(Assigned(HorzScrollBar))then
  begin
    if(HorzScrollBar.Position<>PrevXPos)or
      (VertScrollBar.Position<>PrevYPos)then
    begin
      PrevXPos:=HorzScrollBar.Position;
      PrevYPos:=VertScrollBar.Position;

      DMEER.RefreshNavPalette;
    end;
  end;
end;

procedure TEERForm.SelectAllMIClick(Sender: TObject);
var i: integer;
begin
  with EERModel do
  begin
    //Set Font for all EER-Objects
    for i:=ComponentCount-1 downto 0 do
      if(Components[I].Classparent=TEERObj)then
        TEERObj(Components[I]).SetSelected(True);
  end;

  EERModel.Refresh;
end;

procedure TEERForm.Zoom100MIClick(Sender: TObject);
begin
  EERModel.SetZoomFac(100);

  //Scroll to left corner
  {if(Assigned(HorzScrollBar))then
    HorzScrollBar.Position:=0;

  if(Assigned(VertScrollBar))then
    VertScrollBar.Position:=0;}
end;

procedure TEERForm.Zoome50MIClick(Sender: TObject);
begin
  EERModel.SetZoomFac(50);

  //Scroll to left corner
  {if(Assigned(HorzScrollBar))then
    HorzScrollBar.Position:=0;

  if(Assigned(VertScrollBar))then
    VertScrollBar.Position:=0;}
end;

procedure TEERForm.Zoom75MIClick(Sender: TObject);
begin
  EERModel.SetZoomFac(75);

  //Scroll to left corner
  {if(Assigned(HorzScrollBar))then
    HorzScrollBar.Position:=0;

  if(Assigned(VertScrollBar))then
    VertScrollBar.Position:=0;}
end;

procedure TEERForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var Res: TModalResult;
begin
  CanClose:=True;

  if(EERModel.IsChanged)then
  begin
    DMMain.NormalizeStayOnTopForms;

    Res:=MessageDlg(DMMain.GetTranslatedMessage('The model has not been saved. '+
      'Do you want to save the model before closing?', 194), mtConfirmation, [mbYes, mbNo, mbCancel], 0);

    DMMain.RestoreStayOnTopForms;

    if(Res=mrYes)then
    begin
      if(EERModel.ModelFilename='')or(CompareText(Copy(EERModel.ModelFilename, 1, Length('Noname')), 'Noname')=0)then
        CanClose:=SaveAs
      else
        EERModel.SaveToFile(EERModel.ModelFilename);
    end
    else if(Res=mrCancel)then
      CanClose:=False;
  end;
end;


function TEERForm.SaveAs: Boolean;
var theSaveDialog: TSaveDialog;
  RecentSaveFileAsDir: string;
  theFileName, s: string;
begin
  SaveAs:=False;

  theSaveDialog:=TSaveDialog.Create(nil);
  try
{$IFDEF MSWINDOWS}
    //On Windows use native Win32 Open Dlg
    theSaveDialog.UseNativeDialog:=True;
    theSaveDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
{$ENDIF}

    theSaveDialog.Title:=DMMain.GetTranslatedMessage('Save Model As ...', 195);
    theSaveDialog.Width:=600;
    theSaveDialog.Height:=450;
    theSaveDialog.DefaultExt:='xml';

    RecentSaveFileAsDir:=DMMain.LoadValueFromSettingsIniFile('RecentDirectories', 'RecentSaveFileAsDir', '');
    if(Not(DirectoryExists(RecentSaveFileAsDir)))then
      RecentSaveFileAsDir:='';

    theSaveDialog.InitialDir:=RecentSaveFileAsDir;

    theSaveDialog.Filter:=DMMain.GetTranslatedMessage('DB-Model files', 196)+' (*.xml)|*.xml';

    if(theSaveDialog.Execute)then
    begin
      theFileName:=theSaveDialog.Filename;
      if(FileExists(theFileName))then
        if(MessageDlg(DMMain.GetTranslatedMessage(
          'The file [%s] already exists. '#13#10+
          'Do you want to overwrite this file?', 197, ExtractFileName(theFileName)),
          mtInformation, [mbYes, mbNo], 0)=mrNo)then
          Exit;

      //Set Model Name is it is Noname
      if(Copy(EERModel.GetModelName, 1, 6)='Noname')then
        EERModel.SetModelName(Copy(ExtractFileName(theFileName), 1, Length(ExtractFileName(theFileName))-Length(ExtractFileExt(theFileName))));

      EERModel.ModelFilename:=theFileName;
      EERModel.SaveToFile(theFileName);
      {Caption:='DB Model | '+ExtractFileName(theFileName);
      theFormMenuItem.Caption:=EERModel.GetModelName;}

      //Add file to recent file list
      sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_AddToRecentFileList, PChar(theFileName)));

      RecentSaveFileAsDir:=ExtractFilePath(theSaveDialog.FileName);

      EERModel.IsChanged:=False;

      DMEER.RefreshSavedImg;

      SaveAs:=True;

      DMMain.SaveValueInSettingsIniFile('RecentDirectories', 'RecentSaveFileAsDir', RecentSaveFileAsDir);

      s:=DMMain.GetTranslatedMessage('The model was successfully saved to %s.', 198, theFileName);
      sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_SetStatusCaption, PChar(s)));
    end;
  finally
    theSaveDialog.Free;
  end;
end;

end.
