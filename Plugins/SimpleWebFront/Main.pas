unit Main;

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
//
// Unit MainDM.pas
// ---------------
// Version 1.1, 14.05.2003, Mike
// Description
//   the main file of the plugin, handles all the UI
//
// Changes:
//   Version 1.1, 14.05.2003, Mike
//     added
//   Version 1.0, 12.01.2003, Ulli
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, Qt, QExtCtrls, QCheckLst, QComCtrls, EERModel,
  QButtons, Weboutput, EditorView, QFileCtrls, DialogDirectorySelect, Layer,
  QMenus;

type
  TMainForm = class(TForm)
    Label19: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    FormHeightEd: TEdit;
    FormXEd: TEdit;
    FormYEd: TEdit;
    FormWidthEd: TEdit;
    PageControl: TPageControl;
    GridTabSheet: TTabSheet;
    CreateBtn: TSpeedButton;
    ModelOptionsTabSheet: TTabSheet;
    LayoutLbl: TLabel;
    FieldsVisibleCheckListBox: TCheckListBox;
    Label7: TLabel;
    Group2ComboBox: TComboBox;
    Label1: TLabel;
    ViewComboBox: TComboBox;
    Label8: TLabel;
    GroupBox1: TGroupBox;
    GridGroupBox: TGroupBox;
    FormOptionsTabSheet: TTabSheet;
    Form_FieldsVisibleCheckListBox: TCheckListBox;
    Label9: TLabel;
    Form_ViewComboBox: TComboBox;
    Form_GroupComboBox: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    HeadingLbl: TLabel;
    HeadingEd: TEdit;
    DatabaseSettings: TGroupBox;
    LangComboBox: TComboBox;
    LanguageLbl: TLabel;
    PasswordEd: TEdit;
    UsernameEd: TEdit;
    HostnameEd: TEdit;
    DatabaseEd: TEdit;
    Hostname: TLabel;
    Username: TLabel;
    Password: TLabel;
    Database: TLabel;
    LayoutComboBox: TComboBox;
    OutputDirEd: TEdit;
    OutputDir: TLabel;
    SaveDlgBtn: TSpeedButton;
    GeneralSettings: TGroupBox;
    RowsPerPageEd: TEdit;
    RowsPerPage: TLabel;
    GroupTabSheet: TTabSheet;
    GroupsListBox: TListBox;
    Label6: TLabel;
    ViewTabSheet: TTabSheet;
    AssignmentGroupBox: TGroupBox;
    Label18: TLabel;
    ViewsFromGroupListBox: TListBox;
    UnassignedViewsListBox: TListBox;
    GroupComboBox: TComboBox;
    Label12: TLabel;
    Label13: TLabel;
    GroupManagementBox: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ViewInfoGroupBox: TGroupBox;
    Label14: TLabel;
    Label15: TLabel;
    ColnameEd: TEdit;
    CompoundColNamesCheckBox: TCheckBox;
    ColWidthEd: TEdit;
    FixedWidthCheckBox: TCheckBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Help1: TMenuItem;
    CloseMI: TMenuItem;
    About1: TMenuItem;
    Bevel3: TBevel;
    TruncateCheckBox: TCheckBox;
    ColTruncateCharsEd: TEdit;
    Execute1: TMenuItem;
    PageControlTreeView: TTreeView;
    PageControlTitleShape: TShape;
    PageControlTitlePnl: TPanel;
    PageControlTitleLbl: TLabel;
    GroupBox4: TGroupBox;
    Label17: TLabel;
    AllViewsListBox: TListBox;
    ViewNameEd: TEdit;
    TablenameEd: TEdit;
    JoinTablesEd: TEdit;
    NMTablesEd: TEdit;
    WhereClauseMemo: TMemo;
    EditViewSettingsBtn: TSpeedButton;
    Label20: TLabel;
    Label23: TLabel;
    Label16: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    CloseBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    Label28: TLabel;
    Form_ColNameEd: TEdit;
    Form_ColWidthEd: TEdit;
    Form_FixedWidthCheckBox: TCheckBox;
    Label29: TLabel;
    SaveMI: TMenuItem;
    ReportingBugs1: TMenuItem;
    N1: TMenuItem;
    CreateGrpBtn: TSpeedButton;
    DelGrpBtn: TSpeedButton;
    EditGrpBtn: TSpeedButton;
    AddRegBtn: TSpeedButton;
    EditViewBtn: TSpeedButton;
    CreateViewBtn: TSpeedButton;
    DelViewBtn: TSpeedButton;
    AddTablesBtn: TSpeedButton;
    StatusBar: TStatusBar;
    Grid_UpBtn: TSpeedButton;
    Grid_DownBtn: TSpeedButton;
    LABtn: TSpeedButton;
    RABtn: TSpeedButton;
    Form_DownBtn: TSpeedButton;
    Form_UpBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    //This procedure initializes the Controls based on the selected model
    procedure InitControls;
    procedure GroupComboBoxChange(Sender: TObject);
    procedure SaveDlgBtnClick(Sender: TObject);
    procedure CreateBtnClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure HostnameEdExit(Sender: TObject);
    procedure UsernameEdExit(Sender: TObject);
    procedure PasswordEdExit(Sender: TObject);
    procedure DatabaseEdExit(Sender: TObject);
    procedure HeadingEdExit(Sender: TObject);
    procedure LayoutComboBoxChange(Sender: TObject);
    procedure RowsPerPageEdExit(Sender: TObject);
    procedure CreateGrpBtnClick(Sender: TObject);
    procedure DelGrpBtnClick(Sender: TObject);
    procedure AddRegBtnClick(Sender: TObject);
//    procedure DelRegBtnClick(Sender: TObject);
    procedure CreateViewBtnClick(Sender: TObject);
    procedure DelViewBtnClick(Sender: TObject);
    procedure LABtnClick(Sender: TObject);
    procedure RABtnClick(Sender: TObject);
    procedure AllViewsListBoxClick(Sender: TObject);
    procedure Group2ComboBoxChange(Sender: TObject);
    procedure ViewComboBoxChange(Sender: TObject);
    procedure FieldsVisibleCheckListBoxClickCheck(Sender: TObject);
    procedure AddTablesBtnClick(Sender: TObject);
//    procedure DelTablesBtnClick(Sender: TObject);
    procedure EditViewBtnClick(Sender: TObject);
    procedure OutputDirEdExit(Sender: TObject);
    procedure Form_GroupComboBoxChange(Sender: TObject);
    procedure Form_ViewComboBoxChange(Sender: TObject);
    procedure Form_FieldsVisibleCheckListBoxClickCheck(Sender: TObject);
    procedure EditGrpBtnClick(Sender: TObject);
    procedure AllViewsListBoxDblClick(Sender: TObject);
    procedure GroupsListBoxDblClick(Sender: TObject);
    procedure CloseMIClick(Sender: TObject);
    procedure CompoundColNamesCheckBoxClick(Sender: TObject);
    procedure FieldsVisibleCheckListBoxClick(Sender: TObject);
    procedure FixedWidthCheckBoxClick(Sender: TObject);
    procedure ColWidthEdExit(Sender: TObject);
    procedure ColnameEdExit(Sender: TObject);
    procedure TruncateCheckBoxClick(Sender: TObject);
    procedure ExecuteMIClick(Sender: TObject);
    procedure ColTruncateCharsEdExit(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure PopupBtnClick(Sender: TObject);
    procedure FormWidthEdExit(Sender: TObject);
    procedure FormHeightEdExit(Sender: TObject);
    procedure FormXEdExit(Sender: TObject);
    procedure FormYEdExit(Sender: TObject);
    procedure PageControlTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure SaveBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure Form_FieldsVisibleCheckListBoxClick(Sender: TObject);
    procedure Form_ColWidthEdExit(Sender: TObject);
    procedure Form_ColNameEdExit(Sender: TObject);
    procedure Form_FixedWidthCheckBoxClick(Sender: TObject);
    procedure SaveMIClick(Sender: TObject);
    procedure ReportingBugs1Click(Sender: TObject);
    procedure Grid_UpBtnClick(Sender: TObject);
    procedure Grid_DownBtnClick(Sender: TObject);
    procedure Form_UpBtnClick(Sender: TObject);
    procedure Form_DownBtnClick(Sender: TObject);
  private
    EERModel: TEERModel;     //only used for GetPlugInData and AddPluginData
    xmlFilename :String;
    function TakeSettings : Boolean;
    procedure RefreshPage(index: Integer);
    procedure RefreshGroupTab;
    procedure RefreshViewTab;
    procedure RefreshGridTab;
    procedure RefreshFormTab;
    procedure UpdateInfoFields;
    procedure RefreshColInfo;
    procedure RefreshViewInfo;
    procedure RefreshFormInfo;
    procedure RefreshFormColInfo;
    procedure UpdateTabFromOutputObject;

    //Deactivate/Activate the execute-menu entry and the execute-button 
    procedure DeactivateRun;
    procedure ActivateRun;
    function CreateViewLikeTable(table: SW_Table) : TView;
  public
    { Public declarations }
    Output  : TWeboutput;

  end;

var
  MainForm: TMainForm;

const
  Version = '0.6.4.6';
  DataDir = 'Data'+PathDelim+'Plugins'+PathDelim+'SimpleWebFront'+PathDelim;



IMPLEMENTATION


uses MainDM, EERDM,EditorString, Contnrs, EditorGroup, StringConstants,
     Splash, DialogPopupSettings, SWF_XML_Binding, XMLIntf,DialogBugs;

{$R *.xfm}


procedure TMainForm.FormCreate(Sender: TObject);
var mydata: TEERPluginData;
    password, xmlString: string;
    xmlNode : IXMLSWF_DataType;
begin

  Caption:='Simple Web Front Plugin - Version '+Version+' |';

  //Create Main DataModule, containing general functions
  DMMain:=TDMMain.Create(self);
  //Create EER DateModule, containing additional functions for the EERModel
  DMEER:=TDMEER.Create(self);

  //Initialize the Forms Font
  DMMain.InitForm(self);

  xmlFilename :=ParamStr(1);

  if ( (ParamCount = 0) or NOT(FileExists(xmlFilename)) ) then //no (valid) model-file given
  begin
    MessageDlg(ArgumentError, mtError,[mbOK],0);
    Application.Terminate;
  end;

  //create the model
  //we have to do that here since the EERModel is a panel and it expects a form as a parent
  EERModel:=TEERModel.Create(self);
  EERModel.Visible:=False;
  EERModel.LoadFromFile(ParamStr(1), True, False, False, False); //Load from file, read settings and do not append to current model
  DMEER.SetCurrentWorkTool(wtPointer);   //Set the Worktool to Pointer

  Model := SW_Model.Create(EERModel);
  Output := TPHPOutput.Create;  //at the moment only PHP is supported

  InitControls;

  //Load the model data
  mydata:=EERModel.GetPluginDataById('SimpleWebFront', 0);
  if (mydata <> nil) then
  begin
    xmlString := mydata.params[0];

    xmlNode := LoadSWF_DataFromString(xmlString);

    //should be named loadFromXMLNode
    Output.LoadFromXMLNode(xmlNode);
    UpdateTabFromOutputObject;
  end;


  if (Output.InputComplete) then
    ActivateRun()
  else
    DeactivateRun();


  //Page Control Tree View
  PageControlTreeView.FullExpand;
  PageControl.ActivePageIndex:=0;
  PageControlTreeView.Selected:=PageControlTreeView.Items[0];

  PageControlTitleLbl.Font.Style:=[fsBold];

  //Center Form
  Top:=(Screen.Height-Height) div 2;
  //2 Monitors
  if(Screen.Width=(Screen.Height/0.75)*2)or
    (Screen.Width=(Screen.Height*1.25)*2)then
    Left:=((Screen.Width div 2)-Width) div 2
  else
    Left:=(Screen.Width-Width) div 2;
  //2 Monitors, different resolution 1280+1152
  if(Screen.Width=1280+1152)then
    Left:=(1280-Width) div 2;

  //to support automatic testing
  if ((ParamCount = 3) and (ParamStr(3) = 'auto')) then
  begin
    password := ParamStr(2);
    Output.Password := password;
    if (Output.InputComplete = false) then
    begin
      ExitCode := 1;
      Application.Terminate;
    end;
    try
      Output.Run;
      MessageDlg(OutputSuccessful, mtInformation,[mbOK],0);
    except
      on e: Exception do ShowException(e,ExceptAddr);
    end;
    Application.Terminate;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Output.Free;
  Model.Free;
  EERModel.Free;
end;

procedure TMainForm.InitControls;
var path: string;
    successful: Integer;
    result: TSearchRec;
    oneLayoutFound :Boolean;
begin
  oneLayoutFound := false;
  
  //Always start with First Page
  PageControl.ActivePageIndex:=0;

  //deactive the button as long as the settings for submitting are not complete
  DeactivateRun();

  //prepare the layout-listbox
  {$IFDEF MSWINDOWS}
    path := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+PathDelim+DataDir+'*.*';
  {$ELSE}
    path := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+PathDelim+DataDir+'*';
  {$ENDIF}

  successful:= FindFirst(path,faDirectory,result);
  while(successful=0) do
  begin
    if (((result.Attr and faDirectory) <>0 ) and (result.Name<>'..') //if it is a directory
       and (result.Name<> '.') and (result.Name<> 'CVS') and (result.Name<> 'cvs'))  then
    begin
      oneLayoutFound := TRUE;
      LayoutComboBox.Items.Add(result.Name);       //addentry;
    end;
    successful := FindNext(result);
  end;
  FindClose(result);

  if (oneLayoutFound) then
  begin
    LayoutComboBox.ItemIndex:=0;
    LayoutComboBoxChange(self);
  end
  else
  begin
    MessageDlg(NoLayoutError, mtError,[mbOK],0);
    Application.Terminate;
  end;

end;


procedure TMainForm.CloseMIClick(Sender: TObject);
begin
  CloseBtnClick(Sender);
end;

//Produce the webfiles
procedure TMainForm.CreateBtnClick(Sender: TObject);
begin
  //unfortunately the onclick-event is processed before OnExit-Events that
  //logically happened before the click

  try
    if (TakeSettings) then //it could be that the change disallows executing again
    begin
       //check if the output-dir exists
      if (Output.dirDoesntExist) THEN
      begin
        if ( MessageDlg(OutputDirDoesntExist, mtConfirmation,[mbYes,mbNo],0) = mrYes) THEN
          ForceDirectories(Output.SaveDir)
        else
      Exit;
      end;

      Output.Run;
      //Application.ProcessMessages;
      //MessageDlg(OutputSuccessful, mtInformation,[mbOK],0,mbOk);
      //ShowMessage(Outputsuccessful);
      StatusBar.SimpleText := Outputsuccessful;
    end;
  except
    //on e:ETemplateFileException do MessageDlg(e.Message, mtError, [mbOK], 0);
    on e: Exception do ShowException(e,ExceptAddr);
  end;

end;

procedure TMainForm.ExecuteMIClick(Sender: TObject);
begin
  CreateBtnClick(Sender);
end;

procedure TMainForm.DeactivateRun;
begin
  //deactivate the 'Execute'-Entry in the Menu
  MainMenu1.Items[0].Items[0].Enabled := false;
  CreateBtn.Enabled := false;
end;

procedure TMainForm.ActivateRun;
begin
  //activate the 'Execute'-Entry in the Menu
  MainMenu1.Items[0].Items[0].Enabled := True;
  CreateBtn.Enabled := True;
end;

procedure TMainForm.RefreshPage(index: Integer);
begin
  if (index=1) then RefreshViewTab;
  if (index=2) then RefreshGroupTab;
  if (index=3) then RefreshGridTab;
  if (index=4) then RefreshFormTab;
end;




//since you can also change the pages with your mousewheel, we need that
procedure TMainForm.PageControlChange(Sender: TObject);
begin
  if(PageControl.ActivePageIndex<PageControlTreeView.Items.Count)then
    PageControlTreeView.Selected:=PageControlTreeView.Items[PageControl.ActivePageIndex];

  RefreshPage(PageControl.ActivePageIndex);
end;

//For all Edit-controls that propagate updates by the OnExit-Event
//call their event-handlers
//The return value says if all settings necessary for an execute-operation are done
function TMainForm.TakeSettings : Boolean;
begin
  //controls of the first tab
  LayoutComboBoxChange(self);
  HeadingEdExit(self);
  OutputDirEdExit(self);
  HostnameEdExit(self);
  UsernameEdExit(self);
  PasswordEdExit(self);
  DatabaseEdExit(self);

  //grid tab
  RowsPerPageEdExit(self);
  ColnameEdExit(self);
  ColWidthEdExit(self);
  ColTruncateCharsEdExit(self);

  //form tab
  FormWidthEdExit(self);
  FormHeightEdExit(self);
  FormXEdExit(self);
  FormYEdExit(self);
  Form_ColNameEdExit(self);
  Form_ColWidthEdExit(self);

  TakeSettings := Output.InputComplete();
end;


//*****************************************
//**********Exit/Change-Events*************
//*****************************************

procedure TMainForm.HostnameEdExit(Sender: TObject);
begin
  Output.Hostname := HostnameEd.Text;
  if (Output.InputComplete) then
    ActivateRun
  else
    DeactivateRun;
end;

procedure TMainForm.UsernameEdExit(Sender: TObject);
begin
  Output.Username := UsernameEd.Text;
  if (Output.InputComplete) then
    ActivateRun
  else
    DeactivateRun;
end;

procedure TMainForm.PasswordEdExit(Sender: TObject);
begin
  Output.Password := PasswordEd.Text;
  if (Output.InputComplete) then
    ActivateRun
  else
    DeactivateRun;
end;

procedure TMainForm.DatabaseEdExit(Sender: TObject);
begin
  Output.Databasename := DatabaseEd.Text;
  if (Output.InputComplete) then
    ActivateRun
  else
    DeactivateRun;
end;

procedure TMainForm.HeadingEdExit(Sender: TObject);
begin
  Output.Heading := HeadingEd.Text;
  if (Output.InputComplete) then
    ActivateRun
  else
    DeactivateRun;
end;

procedure TMainForm.OutputDirEdExit(Sender: TObject);
begin
  Output.SaveDir := OutputDirEd.Text;

  if (Output.InputComplete) then
    ActivateRun
  else
    DeactivateRun;
end;

procedure TMainForm.LayoutComboBoxChange(Sender: TObject);
var ind: Integer;
    name: string;
begin
  ind:= LayoutComboBox.ItemIndex;
  name:=LayoutComboBox.Items[ind];
  Output.Layout := name;
  if (Output.InputComplete) then
    ActivateRun
  else
    DeactivateRun;

end;



//*****************************************
//******1st page(General Options)**********
//*****************************************

procedure TMainForm.SaveDlgBtnClick(Sender: TObject);
var OutputDirSel :TDialogDirectorySelectForm;
begin
  OutputDirSel := TDialogDirectorySelectForm.Create(nil);
  try
    if (OutputDirSel.ShowModal = mrOK) then
      OutputDirEd.Text := IncludeTrailingPathDelimiter(OutputDirSel.DirectoryTreeView.Directory);
      Output.SaveDir := OutputDirEd.Text;
  finally
    OutputDirSel.Release;
  end;
end;


procedure TMainForm.UpdateTabFromOutputObject;
var p: Integer;
begin
  HostnameEd.Text := Output.hostname;
  UsernameEd.Text := Output.username;
  PasswordEd.Text := Output.password;
  DatabaseEd.Text := Output.databasename;
  HeadingEd.Text  := Output.heading;
  OutputDirEd.Text := Output.saveDir;

  //layout
  p := LayoutComboBox.Items.IndexOf(Output.Layout);
  if (p<> -1) then LayoutComboBox.ItemIndex := p;
end;

//*****************************************
//**********2nd page(Views)****************
//*****************************************
procedure TMainForm.RefreshViewTab;
var stringList: TStringList;
begin
  stringList:= TStringList.Create;

  Output.GetViewNames(stringList);
  AllViewsListBox.Items.Assign(stringList);
  stringList.Free;

  UpdateInfoFields;

  if (Output.InputComplete) then
    ActivateRun
  else
    DeactivateRun;
end;

procedure TMainForm.UpdateInfoFields;
var ind: Integer;
    view : TView;
    i : Integer;
    str : String;
begin
  ind:=AllViewsListBox.ItemIndex;
  if (ind = -1) then
  begin
    ViewNameEd.Text    := '-';
    TableNameEd.Text   := '-';
    JoinTablesEd.Text      := '-';
    NMTablesEd.Text        := '-';
    WhereClauseMemo.Text     := '';
  end
  else
  begin
    view:= TView(AllViewsListBox.Items.Objects[ind]);
    ViewNameEd.Text:= view.Name;
    TableNameEd.Text := view.Table.Name;
    WhereClauseMemo.Text := view.WhereClause;

    //show all JoinTables
    str:= '';
    for i:=0 to view.JoinTables.Count-1 do
      str:= str + SW_Table(view.JoinTables[i]).name + ',';
    Delete(str,Length(str),1);  //delete the last comma
    JoinTablesEd.Text := str;

    //show all NMTables
    str:='';
    for i:=0 to view.NMTables.Count-1 do
      str:= str + SW_Table(view.NMTables[i]).name + ',';
    Delete(str,Length(str),1);
    NMTablesEd.Text := str;
  end;
end;

procedure TMainForm.CreateViewBtnClick(Sender: TObject);
var ViewForm : TEditorViewForm;
begin
  ViewForm := TEditorViewForm.Create(nil);
  //nil causes a new view to be created
  ViewForm.provideView(nil);
  try
    if (ViewForm.ShowModal=mrOk) then
    begin
      //add the view to the state
      Output.AddView(ViewForm.view);
      
      //update the GUI
      RefreshViewTab;
    end;
  finally
    ViewForm.Release;
  end;

end;

procedure TMainForm.DelViewBtnClick(Sender: TObject);
var ind: Integer;
begin
  ind:= AllViewsListBox.ItemIndex;
  if (ind=-1) then Exit;
  Output.DeleteView( TView(AllViewsListBox.Items.Objects[ind]) );

  RefreshViewTab;
end;

procedure TMainForm.AddTablesBtnClick(Sender: TObject);
var stringList : TStringList;
    i: Integer;
    view : TView;
begin
  stringList := TStringList.Create;
  Model.GetAllTables(stringList);

  for i:= 0 to stringList.Count-1 do
  begin
    view := CreateViewLikeTable(SW_Table(stringList.Objects[i]));
    Output.AddView(view);
  end;

  stringList.Free;
  RefreshViewTab;
end;

//creates a view representing table
function TMainForm.CreateViewLikeTable(table: SW_Table) :TView;
var thePic : TPicture;
    exeFilepath : String;
    view :TView;
begin
  //load the default icon
  thePic:=TPicture.Create;
  exeFilepath :=  IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  thePic.LoadFromFile(exeFilePath+DataDir+'Standard'+PathDelim+'icons'+PathDelim+'icons_01.png');

  view:= TView.Create(table.name, '', table, nil,nil,thePic,'icons_01.png','');
  thePic.Free;

  CreateViewLikeTable:= view;
end;

{
procedure TMainForm.DelTablesBtnClick(Sender: TObject);
var stringList : TStringList;
    i,a: Integer;
begin
  stringList := TStringList.Create;
  Model.GetAllTables(stringList);
  for i:= 0 to stringList.Count-1 do
  begin
    a:= AllViewsListBox.Items.IndexOf(stringList[i]);
    if (a <> -1) then
      Output.DeleteView(TView(AllViewsListBox.Items.Objects[a]));
  end;

  stringList.Free;
  RefreshViewTab;
end;
}

procedure TMainForm.EditViewBtnClick(Sender: TObject);
var view: TView;
    ViewEditForm : TEditorViewForm;
begin
  if (AllViewsListBox.ItemIndex=-1) then Exit;
  view:= TView(AllViewsListBox.Items.Objects[AllViewsListBox.ItemIndex]);

  ViewEditForm := TEditorViewForm.Create(nil);
  ViewEditForm.ProvideView(view);

  if (ViewEditForm.ShowModal = mrOK) then RefreshViewTab;

  ViewEditForm.Release;
end;

procedure TMainForm.AllViewsListBoxDblClick(Sender: TObject);
begin
  EditViewBtnClick(self);
end;

procedure TMainForm.AllViewsListBoxClick(Sender: TObject);
begin
  if (AllViewsListBox.ItemIndex=-1) then Exit;

  UpdateInfoFields;
end;







//*****************************************
//**********3rd page(Groups)***************
//*****************************************
//this procedure completly refills the gui-objects on this page
//with the information that
//is stored in the WebOutput class
procedure TMainForm.RefreshGroupTab;
var group: TGroup;
    stringList: TStringList;
begin
  stringList := TStringList.Create;

  Output.GetGroupNames(stringList);
  GroupsListBox.Items.Assign(stringList);
  GroupComboBox.Items.Assign(stringList);
  if (stringList.Count = 0) then
  begin
    GroupComboBox.Enabled := false;
    LABtn.Enabled := false;
    RABtn.Enabled := false;
  end
  else
  begin
    GroupComboBox.Enabled := True;
    if (GroupComboBox.ItemIndex = -1) then GroupComboBox.ItemIndex := 0;
  end;
  stringList.Clear;

  Output.GetUnassignedViewNames(stringList);
  UnassignedViewsListBox.Items.Assign(stringList);
  if (stringList.Count = 0) then
  begin
    LABtn.Enabled := false;
    RABtn.Enabled := false;
  end;
  stringList.Clear;


  if (GroupComboBox.ItemIndex = -1) then
    ViewsFromGroupListBox.Items.Clear
  else
  begin
    group:= TGroup(GroupComboBox.Items.Objects[GroupComboBox.ItemIndex]);
    Output.GetViewNamesFromGroup(group, stringList);
    ViewsFromGroupListBox.Items.Assign(stringList);
    stringList.Clear;
  end;

  if ( ((GroupComboBox.Items.Count <> 0) and (ViewsFromGroupListBox.Items.Count <> 0)) or
       (UnassignedViewsListBox.Items.Count <> 0) ) then
  begin
    LABtn.Enabled := true;
    RABtn.Enabled := true;
  end;


  if (Output.InputComplete) then
    ActivateRun
  else
    DeactivateRun;

  stringList.Free;
end;

procedure TMainForm.CreateGrpBtnClick(Sender: TObject);
var GroupEditForm: TEditorGroupForm;
    group: TGroup;
begin
  GroupEditForm := TEditorGroupForm.Create(nil);
  try

    if (GroupEditForm.ShowModal = mrOk) then
    begin
      //add the group to our internal state

      group := GroupEditForm.GetGroup;
      Output.AddGroup(group);

      //update the groupListBox
      RefreshGroupTab;
    end;
  finally
    GroupEditForm.Release;
  end;
end;

procedure TMainForm.DelGrpBtnClick(Sender: TObject);
var group: TGroup;
begin
  if (GroupsListBox.ItemIndex = -1) then Exit;

  group := TGroup(GroupsListBox.Items.Objects[GroupsListBox.ItemIndex]);
  Output.DeleteGroup(group);

  //update the groupListBox
  GroupComboBox.ItemIndex := GroupComboBox.Items.Count-2;
  RefreshGroupTab;
end;

procedure TMainForm.EditGrpBtnClick(Sender: TObject);
var ind: Integer;
    tmpGrp : TGroup;
    GroupEditForm :TEditorGroupForm;
begin
  ind := GroupsListBox.ItemIndex;
  if (ind = -1) then Exit; //no group selected
  tmpGrp := TGroup(GroupsListBox.Items.Objects[ind]);

  GroupEditForm := TEditorGroupForm.Create(nil);
  GroupEditForm.SetGroup(tmpGrp);

  if (GroupEditForm.ShowModal = mrOk) then
  begin
    tmpGrp.Name := GroupEditForm.GrpnameEd.Text;
    tmpGrp.showOnLine := StrToInt(GroupEditForm.LineNoEd.Text);
    tmpGrp.showInColumn := StrToInt(GroupEditForm.ColNoEd.Text);
    tmpGrp.ViewsAsPopup := GroupEditForm.PopupCheckBox.Checked;
    
    //update the groupListBox
    RefreshGroupTab;
  end;

  GroupEditForm.Release;
end;

procedure TMainForm.GroupsListBoxDblClick(Sender: TObject);
begin
  EditGrpBtnClick(Sender);
end;

procedure TMainForm.AddRegBtnClick(Sender: TObject);
var stringList, tables_stringList: TStringList;
    i,j, lineNo, colNo: Integer;
    group : TGroup;
    noPosFound : Boolean;
    region : SW_Region;
    table: SW_Table;
    view : TView;
begin
  //get all regions from the EER-Model
  stringList := TStringList.Create;
  Model.GetAllRegions(stringList);

  lineNo := 1;
  colNo := 1;
  for i:= 0 to stringList.Count-1 do
  begin

    noPosFound := true;
    while (noPosFound) do
    begin
      if (Output.GroupPositionFree(lineNo, ColNo) ) then
      begin
        group := TGroup.Create(stringList[i],lineNo, colNo,false);
        Output.AddGroup(group);

        //Should the views be added too?
        region := SW_Region(stringList.Objects[i]);
        tables_stringList:= TStringList.Create;
        model.GetAllTablesInRegion(region, tables_stringList);

        if ( MessageDlg(Group_TablesOverview+' '+stringList[i]+' :'+#10+
                        tables_stringList.Text +#10 + Group_ViewsBeCreated,
                        mtConfirmation,[mbYes,mbNo],0) = mrYes) THEN
        begin
          //create views
          for j:=0 to tables_stringList.Count-1 do
          begin
            table := SW_Table(tables_stringList.Objects[j]);
            view := CreateViewLikeTable(table);
            Output.AddView(view);
            Output.AssignViewToGroup(group ,view);
            table.Free;
          end;
        end;

        tables_stringList.Free;
        noPosFound := false;
      end
      else
        lineNo:= lineNo+1;
    end;

  end;

  //update the groupListBox
  RefreshGroupTab;

  stringList.Free;
end;


procedure TMainForm.GroupComboBoxChange(Sender: TObject);
begin
  RefreshGroupTab;
end;

procedure TMainForm.LABtnClick(Sender: TObject);
var i,grpInd : Integer;
begin
  grpInd:= GroupComboBox.ItemIndex;
  if (grpInd=-1)  then Exit;

  for i:=0 to UnassignedViewsListBox.Items.Count-1 do
  begin
    if UnassignedViewsListBox.Selected[i] then
      Output.AssignViewToGroup(TGroup(GroupComboBox.Items.Objects[grpInd]),TView(UnassignedViewsListBox.Items.Objects[i]));
  end;
  RefreshGroupTab;
end;

procedure TMainForm.RABtnClick(Sender: TObject);
var i, grpInd : Integer;
begin
  grpInd:= GroupComboBox.ItemIndex;
  if (grpInd=-1) then Exit;

  for i:=0 to ViewsFromGroupListBox.Items.Count-1 do
  begin
    if (ViewsFromGroupListBox.Selected[i]) then
      Output.UnassignViewFromGroup(TGroup(GroupComboBox.Items.Objects[grpInd]),TView(ViewsFromGroupListBox.Items.Objects[i]));
  end;

  RefreshGroupTab;
end;







//*****************************************
//**********4th page(Grid)*****************
//*****************************************

procedure TMainForm.RefreshGridTab;
var stringList : TStringList;
    tmpGrp : TGroup;
begin
  stringList := TStringList.Create;
  Output.GetGroupNames(stringList);
  Group2ComboBox.Items.Assign(stringList);
  if (stringList.Count = 0) then
  begin
    Group2ComboBox.Enabled := false;
    Group2ComboBox.ItemIndex:= -1;
  end
  else
  begin
    Group2ComboBox.Enabled := true;
    if ((Group2ComboBox.ItemIndex = -1) or (Group2ComboBox.ItemIndex >= Group2ComboBox.Items.Count)) then Group2ComboBox.ItemIndex:= 0;
  end;

  stringList.Clear;


  if (Group2ComboBox.ItemIndex = -1) then      //meaning we have no groups
  begin
    ViewComboBox.Clear;
    ViewComboBox.Enabled := False;
  end
  else
  begin
    tmpGrp := TGroup(Group2ComboBox.Items.Objects[Group2ComboBox.ItemIndex]);
    Output.GetViewNamesFromGroup(tmpGrp,stringList);
    ViewComboBox.Items.Assign(stringList);
    if (stringList.Count = 0) then
    begin
      ViewComboBox.Enabled := false;
      ViewComboBox.ItemIndex := -1;
    end
    else
    begin
      ViewComboBox.Enabled := True;
      ViewComboBox.ItemIndex := 0;
    end;
  end;

  stringList.Free;
  RefreshViewInfo;
end;

//This method refreshes all controls that are dependent on the selected View
//The method is called when the currently selected view changes. (meaning when the view-pulldownbox changes)
procedure TMainForm.RefreshViewInfo;
var stringList : TStringList;
    tmpView : TView;
    i : Integer;
begin
  //3 Controls need to be taken care of:
  // FieldsVisibleCheckListBox, RowsPerPageEd, CompoundColNamesCheckBox

  if (ViewComboBox.ItemIndex = -1) then
  begin
    FieldsVisibleCheckListBox.Clear;
    FieldsVisibleCheckListBox.Enabled := false;
    RowsPerPageEd.Text := '';
    RowsPerPageEd.Enabled := false;
    CompoundColNamesCheckBox.Checked := false;
    CompoundColNamesCheckBox.Enabled := false;
    EditViewSettingsBtn.Enabled := false;
  end
  else
  begin
    tmpView := TView(ViewComboBox.Items.Objects[ViewComboBox.ItemIndex]);
    stringList := TStringList.Create;
    tmpView.GetGridSortedColumns(stringList);
    FieldsVisibleCheckListBox.Enabled := True;
    FieldsVisibleCheckListBox.Items.Assign(stringList);
    for i:=0 to FieldsVisibleCheckListBox.Items.Count-1 do
    begin
      FieldsVisibleCheckListBox.Checked[i] := SW_Column(stringList.Objects[i]).selectedForGrid;
    end;
    stringList.Clear;

    RowsPerPageEd.Enabled := True;
    RowsPerPageEd.Text := IntToStr(tmpView.RowsPerPage);
    CompoundColNamesCheckBox.Enabled := True;
    CompoundColNamesCheckBox.Checked := tmpView.UseCompoundColNames;

    EditViewSettingsBtn.Enabled := TGroup(Group2ComboBox.Items.Objects[Group2ComboBox.ItemIndex]).ViewsAsPopup;
  end;

  RefreshColInfo;
end;

//This method refreshes all controls that are dependent on the selected Column
procedure TMainForm.RefreshColInfo;
var cInd : Integer;
    column : SW_Column;
begin
  cInd := FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) then
  begin
    ColNameEd.Text := '';
    ColNameEd.Enabled := false;
    FixedWidthCheckBox.Enabled := false;
    ColWidthEd.Text := '';
    ColWidthEd.Enabled := false;
    TruncateCheckBox.Enabled := false;
    ColTruncateCharsEd.Text := '';
    ColTruncateCharsEd.Enabled := false;

    exit;
  end;

  column := SW_Column(FieldsVisibleCheckListBox.Items.Objects[cInd]);

  //we show the correct values, but the user cannot change them
  if (column.selectedForGrid = false) then
  begin
    ColNameEd.Text := column.GridName;
    ColNameEd.Enabled := false;

    FixedWidthCheckBox.Enabled := false;

    if (column.FixedWidth = -1) then
    begin
      ColWidthEd.Text := '50';
      FixedWidthCheckBox.Checked := false;
    end
    else
    begin
      ColWidthEd.Text := IntToStr(column.FixedWidth);
      FixedWidthCheckBox.Checked := true;
    end;
    ColWidthEd.Enabled := false;

    TruncateCheckBox.Enabled := false;

    if (column.TruncateChars = -1) then
    begin
      ColTruncateCharsEd.Text := '10';
      TruncateCheckBox.Checked := false;
    end
    else
    begin
      ColTruncateCharsEd.Text := IntToStr(column.TruncateChars);
      TruncateCheckBox.Checked := true;
    end;
    ColTruncateCharsEd.Enabled := false;

    exit;
  end;

  ColNameEd.Enabled := true;
  FixedWidthCheckBox.Enabled := true;
  TruncateCheckBox.Enabled := true;

  ColNameEd.Text := column.GridName;

  if (column.fixedWidth = -1) then
  begin
    FixedWidthCheckBox.Checked := false;
    ColWidthEd.Text := '50';
    ColWidthEd.Enabled := false;
  end
  else
  begin
    FixedWidthCheckBox.Checked := true;
    ColWidthEd.Enabled := true;
    ColWidthEd.Text := IntToStr(column.fixedWidth);
  end;

  if (column.truncateChars = -1) then
  begin
    TruncateCheckBox.Checked := false;
    ColTruncateCharsEd.Text := '10';
    ColTruncateCharsEd.Enabled := false;
  end
  else
  begin
    TruncateCheckBox.Checked := true;
    ColTruncateCharsEd.Enabled := true;
    ColTruncateCharsEd.Text := IntToStr(column.truncateChars);
  end;

  if (Output.InputComplete) then
    ActivateRun
  else
    DeactivateRun;
end;

procedure TMainForm.RowsPerPageEdExit(Sender: TObject);
var vInd: Integer;
    view: TView;
begin
  //what view is selected?
  vInd := ViewComboBox.ItemIndex;
  if (vInd = -1) then exit;

  view := TView(ViewComboBox.Items.Objects[vInd]);

  view.RowsPerPage := StrToInt(RowsPerPageEd.Text);
end;

procedure TMainForm.CompoundColNamesCheckBoxClick(Sender: TObject);
var vInd: Integer;
    view: TView;
    cols : TStringList;
    i: Integer;
    col: SW_Column;
begin
  //what view is selected?
  vInd := ViewComboBox.ItemIndex;
  if (vInd = -1) then exit;

  view := TView(ViewComboBox.Items.Objects[vInd]);

  view.UseCompoundColNames := CompoundColNamesCheckBox.Checked;
  cols := TStringList.Create;
  view.GetGridSortedColumns(cols);
  for i:=0 to cols.Count-1 do
  begin
    col := SW_Column(cols.Objects[i]);
    if (view.UseCompoundColNames) then
    begin
      if (col.GridName = col.name) then //we only change the names that have not been altered by the user
        col.GridName := col.Table.name + '.' + col.name;
    end
    else
    begin
      if (col.GridName = col.table.name +'.'+ col.name) then
        col.GridName := col.name;
    end;
  end;
  cols.Free;

  RefreshColInfo;
end;

procedure TMainForm.Group2ComboBoxChange(Sender: TObject);
begin
  RefreshGridTab;
end;

procedure TMainForm.ViewComboBoxChange(Sender: TObject);
begin
  RefreshViewInfo;
end;

procedure TMainForm.FieldsVisibleCheckListBoxClick(Sender: TObject);
begin
  RefreshColInfo;
end;

procedure TMainForm.FieldsVisibleCheckListBoxClickCheck(Sender: TObject);
var col : SW_Column;
begin
  col := SW_Column(FieldsVisibleCheckListBox.Items.Objects[FieldsVisibleCheckListBox.ItemIndex]);
  col.ToggleGridSelected;
  RefreshColInfo;
end;

procedure TMainForm.FixedWidthCheckBoxClick(Sender: TObject);
var cInd : Integer;
    column : SW_Column;
begin
  cInd := FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) then exit; //just to be on the safe side..(it should be deactivated actually)

  column := SW_Column(FieldsVisibleCheckListBox.Items.Objects[cInd]);
  if (FixedWidthCheckBox.Checked) then
  begin
    ColWidthEd.Enabled := True;
    if (column.FixedWidth = -1) then //supply a default value
    begin
      ColWidthEd.Text := '50';
      column.fixedWidth := 50;
    end
    else
    begin
      ColWidthEd.Text := IntToStr(column.fixedWidth);
    end;
  end
  else
  begin
    ColWidthEd.Enabled := false;
    column.fixedWidth := -1;
  end;
end;

procedure TMainForm.ColWidthEdExit(Sender: TObject);
var cInd : Integer;
    column : SW_Column;
begin
  cInd := FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) then exit;

  //we also call this method from TakeSettings, so we need the following check
  if (FixedWidthCheckBox.Checked = false) then exit;

  column := SW_Column(FieldsVisibleCheckListBox.Items.Objects[cInd]);
  column.fixedWidth := StrToInt(ColWidthEd.Text);
end;

procedure TMainForm.ColnameEdExit(Sender: TObject);
var cInd : Integer;
    column : SW_Column;
begin
  cInd := FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) then exit;

  column := SW_Column(FieldsVisibleCheckListBox.Items.Objects[cInd]);
  column.GridName := ColnameEd.Text;
end;

procedure TMainForm.ColTruncateCharsEdExit(Sender: TObject);
var cInd : Integer;
    column : SW_Column;
begin
  cInd := FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) then exit;

  //we also call this method from TakeSettings, so we need the following check
  if (TruncateCheckBox.Checked = false) then exit;

  column := SW_Column(FieldsVisibleCheckListBox.Items.Objects[cInd]);
  column.TruncateChars := StrToInt(ColTruncateCharsEd.Text);
end;


procedure TMainForm.TruncateCheckBoxClick(Sender: TObject);
var cInd : Integer;
    column : SW_Column;
begin
  cInd := FieldsVisibleCheckListBox.ItemIndex;
  assert(cInd <> -1);

  column := SW_Column(FieldsVisibleCheckListBox.Items.Objects[cInd]);
  if (TruncateCheckBox.Checked) then
  begin
    ColTruncateCharsEd.Enabled := True;
    if (column.TruncateChars = -1) then //supply a default value
    begin
      ColTruncateCharsEd.Text := '10';
      column.truncateChars := 10;
    end
    else
    begin
      ColTruncateCharsEd.Text := IntToStr(column.TruncateChars);
    end;
  end
  else
  begin
    ColTruncateCharsEd.Enabled := false;
    column.truncateChars := -1;
  end;

end;

procedure TMainForm.PopupBtnClick(Sender: TObject);
var vInd : Integer;
    view: TView;
begin
  //what view is selected?
  vInd := ViewComboBox.ItemIndex;
  assert (vInd <> -1);

  view := TView(ViewComboBox.Items.Objects[vInd]);

  DialogPopupSettingsForm:= TDialogPopupSettingsForm.Create(nil);

  DialogPopupSettingsForm.SetView(view);
  DialogPopupSettingsForm.ShowModal;

  DialogPopupSettingsForm.Release;
end;

procedure TMainForm.Grid_UpBtnClick(Sender: TObject);
var cInd,vInd : Integer;
    view: TView;
    col1,col2 :SW_Column;
begin
  cInd := FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) or (cInd-1 = -1) then exit;

  vInd := ViewComboBox.ItemIndex;
  assert(vInd <> -1);
  view := TView(ViewComboBox.Items.Objects[vInd]);

  col1 := SW_Column(FieldsVisibleCheckListBox.Items.Objects[cInd]);
  col2 := SW_Column(FieldsVisibleCheckListBox.Items.Objects[cInd-1]);

  view.ExchangeGridSortedColumns(cInd, cInd-1);
//  FieldsVisibleCheckListBox.Items.Exchange(cInd, cInd-1);
  FieldsVisibleCheckListBox.Items[cInd] := col2.table.name+'.'+col2.name;
  FieldsVisibleCheckListBox.Items[cInd-1] := col1.Table.name+'.'+col1.name;
  FieldsVisibleCheckListBox.Items.Objects[cInd] := col2;
  FieldsVisibleCheckListBox.Items.Objects[cInd-1] := col1;

  FieldsVisibleChecklistBox.Checked[cInd]:=col2.selectedForGrid;
  FieldsVisibleChecklistBox.Checked[cInd-1]:=col1.selectedForGrid;
  FieldsVisibleCheckListBox.Selected[cInd]:= false;
  FieldsVisibleCheckListBox.Selected[cInd-1] := true;
end;

procedure TMainForm.Grid_DownBtnClick(Sender: TObject);
var cInd,vInd : Integer;
    view: TView;
    col1,col2 :SW_Column;
begin
  cInd := FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) or (cInd+1 >= FieldsVisibleCheckListBox.Items.Count) then exit;

  vInd := ViewComboBox.ItemIndex;
  assert(vInd <> -1);
  view := TView(ViewComboBox.Items.Objects[vInd]);

  col1 := SW_Column(FieldsVisibleCheckListBox.Items.Objects[cInd]);
  col2 := SW_Column(FieldsVisibleCheckListBox.Items.Objects[cInd+1]);

  view.ExchangeGridSortedColumns(cInd, cInd+1);
  //FieldsVisibleCheckListBox.Items.Exchange(cInd, cInd-1);
  FieldsVisibleCheckListBox.Items[cInd] := col2.table.name+'.'+col2.name;
  FieldsVisibleCheckListBox.Items[cInd+1] := col1.Table.name+'.'+col1.name;
  FieldsVisibleCheckListBox.Items.Objects[cInd] := col2;
  FieldsVisibleCheckListBox.Items.Objects[cInd+1] := col1;

  FieldsVisibleChecklistBox.Checked[cInd]:=col2.selectedForGrid;
  FieldsVisibleChecklistBox.Checked[cInd+1]:=col1.selectedForGrid;
  FieldsVisibleCheckListBox.Selected[cInd]:= false;
  FieldsVisibleCheckListBox.Selected[cInd+1] := true;
end;

//*****************************************
//**********5th page(Form)*****************
//*****************************************




procedure TMainForm.RefreshFormTab;
var stringList : TStringList;
    tmpGrp : TGroup;
begin
  //show all groups in the GroupComboBox
  stringList := TStringList.Create;
  Output.GetGroupNames(stringList);
  Form_GroupComboBox.Items.Assign(stringList);
  if (stringList.Count = 0) then
  begin
    Form_GroupComboBox.Enabled := false;
    Form_GroupComboBox.ItemIndex:= -1;
  end
  else
  begin
    Form_GroupComboBox.Enabled := true;
    if ((Form_GroupComboBox.ItemIndex = -1) or (Form_GroupComboBox.ItemIndex >= Form_GroupComboBox.Items.Count)) then Form_GroupComboBox.ItemIndex:= 0;
  end;

  stringList.Clear;

  //if no group is chosen
  if (Form_GroupComboBox.ItemIndex = -1) then
  begin
    Form_ViewComboBox.Clear;
    Form_ViewComboBox.Enabled := False;
  end
  else
  begin
    tmpGrp := TGroup(Form_GroupComboBox.Items.Objects[Form_GroupComboBox.ItemIndex]);
    Output.GetViewNamesFromGroup(tmpGrp,stringList);
    Form_ViewComboBox.Items.Assign(stringList);
    if (stringList.Count = 0) then
    begin
      Form_ViewComboBox.Enabled := false;
      Form_ViewComboBox.ItemIndex := -1;
    end
    else
    begin
      Form_ViewComboBox.Enabled := True;
      Form_ViewComboBox.ItemIndex := 0;
    end;
  end;

  stringList.Free;
  RefreshFormInfo;
end;

//This method refreshes all controls that are dependent on the selected View
//The method is called when the currently selected view changes. (meaning when the view-pulldownbox changes)
procedure TMainForm.RefreshFormInfo;
var stringList : TStringList;
    tmpView : TView;
    i : Integer;
begin


  if (Form_ViewComboBox.ItemIndex = -1) then
  begin
    Form_FieldsVisibleCheckListBox.Clear;
    Form_FieldsVisibleCheckListBox.Enabled := false;

    FormWidthEd.Text := '';
    FormWidthEd.Enabled := false;
    FormHeightEd.Text := '';
    FormHeightEd.Enabled := false;
    FormXEd.Text := '';
    FormXEd.Enabled := false;
    FormYEd.Text := '';
    FormYEd.Enabled := false;
  end
  else
  begin
    tmpView := TView(Form_ViewComboBox.Items.Objects[Form_ViewComboBox.ItemIndex]);
    stringList := TStringList.Create;
    tmpView.GetFormSortedColumns(stringList);
    Form_FieldsVisibleCheckListBox.Enabled := True;
    Form_FieldsVisibleCheckListBox.Items.Assign(stringList);

    for i:=0 to Form_FieldsVisibleCheckListBox.Items.Count-1 do
    begin
      Form_FieldsVisibleCheckListBox.Checked[i] := SW_Column(stringList.Objects[i]).selectedForForm;
    end;
    stringList.Clear;

    FormWidthEd.Enabled := true;
    FormWidthEd.Text := IntToStr(tmpView.FormWidth);
    FormHeightEd.Enabled := true;
    FormHeightEd.Text := IntToStr(tmpView.FormHeight);
    FormXEd.Enabled := true;
    FormXEd.Text := IntToStr(tmpView.FormX);
    FormYEd.Enabled := True;
    FormYEd.Text := IntToStr(tmpView.FormY);;

  end;

  RefreshFormColInfo;
end;

//This method refreshes all controls that are dependent on the selected Column
procedure TMainForm.RefreshFormColInfo;
var cInd : Integer;
    column : SW_Column;
    tmpView: TView;
begin
  cInd := Form_FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) then
  begin
    Form_ColNameEd.Text := '';
    Form_ColNameEd.Enabled := false;
    Form_ColWidthEd.Text := '';
    Form_ColWidthEd.Enabled := false;
    Form_FixedWidthCheckBox.Enabled := false;

    exit;
  end;

  column := SW_Column(Form_FieldsVisibleCheckListBox.Items.Objects[cInd]);
  tmpView := TView(Form_ViewComboBox.Items.Objects[Form_ViewComboBox.ItemIndex]);

  //we show the correct values, but the user cannot change them
  if (column.selectedForForm = false) then
  begin
    Form_ColNameEd.Text := column.FormName;
    Form_ColNameEd.Enabled := false;

    Form_FixedWidthCheckBox.Enabled := false;

    if (column.FormWidth = -1) then
    begin
      Form_ColWidthEd.Text := '200';
      Form_FixedWidthCheckBox.Checked := false;
    end
    else
    begin
      Form_ColWidthEd.Text := IntToStr(column.FormWidth);
      Form_FixedWidthCheckBox.Checked := true;
    end;
    Form_ColWidthEd.Enabled := false;

  end
  else
  begin

    Form_ColNameEd.Enabled := true;
    Form_FixedWidthCheckBox.Enabled:= true;

    //We have to differentiate between the column of a mainTable, joinTable, or nmTable
    if (tmpView.Table = column.Table) then
    begin

      Form_ColNameEd.Text := column.FormName;
      if (column.FormWidth = -1) then
      begin
        Form_FixedWidthCheckBox.Checked := false;
        Form_ColWidthEd.Enabled := false;
        Form_ColWidthEd.Text := '200';
      end
      else
      begin
        Form_FixedWidthCheckBox.Checked := true;
        Form_ColWidthEd.Enabled := true;
        Form_ColWidthEd.Text := IntToStr(column.FormWidth);
      end;

    end
    else if (tmpView.JoinTables.IndexOf(column.Table) <> -1) then    //if it is the column of a join-table
    begin

      Form_ColNameEd.Text := column.Table.Join_ColumnName;
      if (column.Table.Join_Width = -1) then
      begin
        Form_FixedWidthCheckBox.Checked := false;
        Form_ColWidthEd.Enabled := false;
        Form_ColWidthEd.Text := '200';
      end
      else
      begin
        Form_FixedWidthCheckBox.Checked := true;
        Form_ColWidthEd.Enabled := true;
        Form_ColWidthEd.Text := IntToStr(column.Table.Join_Width);
      end;

    end
    else if (tmpView.NMTables.IndexOf(column.Table) <> -1) then    //if it is the column of an NM-table
    begin

      Form_ColNameEd.Text := column.FormName;

      if (column.Table.NM_Width = -1) then
      begin
        Form_FixedWidthCheckBox.Checked := false;
        Form_ColWidthEd.Enabled := false;
        Form_ColWidthEd.Text := '200';
      end
      else
      begin
        Form_FixedWidthCheckBox.Checked := true;
        Form_ColWidthEd.Enabled := true;
        Form_ColWidthEd.Text := IntToStr(column.Table.NM_Width);
      end;

    end;
  end;

  if (Output.InputComplete) then
    ActivateRun
  else
    DeactivateRun;
end;

procedure TMainForm.Form_ColNameEdExit(Sender: TObject);
var cInd : Integer;
    column : SW_Column;
    tmpView:TView;
begin
  cInd := Form_FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) then exit;
  if (Form_ViewComboBox.ItemIndex = -1) then exit;
  column := SW_Column(Form_FieldsVisibleCheckListBox.Items.Objects[cInd]);
  tmpView := TView(Form_ViewComboBox.Items.Objects[Form_ViewComboBox.ItemIndex]);

  //We have to differentiate if it is the colum of a mainTable, joinTable, or nmTable
  if (tmpView.Table = column.Table) then
  begin
    column.FormName := Form_ColnameEd.Text;
  end
  else if (tmpView.JoinTables.IndexOf(column.Table) <> -1) then    //if it is the column of a join-table
  begin
    column.Table.Join_ColumnName := Form_ColnameEd.Text;
  end
  else if (tmpView.NMTables.IndexOf(column.Table) <> -1) then    //if it is the column of an NM-table
  begin
    column.FormName := Form_ColnameEd.Text;
  end;

end;


procedure TMainForm.Form_ColWidthEdExit(Sender: TObject);
var cInd : Integer;
    column : SW_Column;
    tmpView:TView;
begin
  cInd := Form_FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) then exit;
  if (Form_ViewComboBox.ItemIndex = -1) then exit;
  column := SW_Column(Form_FieldsVisibleCheckListBox.Items.Objects[cInd]);
  tmpView := TView(Form_ViewComboBox.Items.Objects[Form_ViewComboBox.ItemIndex]);

  //this method is also called from TakeSettings
  if (Form_FixedWidthCheckBox.Checked = false) then exit;

  //We have to differentiate if it is the colum of a mainTable, joinTable, or nmTable
  if (tmpView.Table = column.Table) then
  begin
    column.FormWidth:= StrToInt(Form_ColWidthEd.Text);
  end
  else if (tmpView.JoinTables.IndexOf(column.Table) <> -1) then    //if it is the column of a join-table
  begin
    column.Table.Join_Width:= StrToInt(Form_ColWidthEd.Text);
  end
  else if (tmpView.NMTables.IndexOf(column.Table) <> -1) then    //if it is the column of an NM-table
  begin
    column.Table.NM_Width:= StrToInt(Form_ColWidthEd.Text);
  end;

end;

procedure TMainForm.Form_GroupComboBoxChange(Sender: TObject);
begin
  RefreshFormTab;
end;

procedure TMainForm.Form_ViewComboBoxChange(Sender: TObject);
begin
  RefreshFormInfo;
end;

procedure TMainForm.Form_FieldsVisibleCheckListBoxClickCheck(Sender: TObject);
var col : SW_Column;
begin
  col := SW_Column(Form_FieldsVisibleCheckListBox.Items.Objects[Form_FieldsVisibleCheckListBox.ItemIndex]);
  col.ToggleFormSelected;

  RefreshFormColInfo;
end;

procedure TMainForm.Form_FieldsVisibleCheckListBoxClick(Sender: TObject);
begin
  RefreshFormColInfo;
end;


procedure TMainForm.Form_FixedWidthCheckBoxClick(Sender: TObject);
var cInd : Integer;
    column : SW_Column;
    tmpView: TView;
begin
  cInd := Form_FieldsVisibleCheckListBox.ItemIndex;
  assert (cInd <> -1);
  assert (Form_ViewComboBox.ItemIndex <> -1);
  column := SW_Column(Form_FieldsVisibleCheckListBox.Items.Objects[cInd]);
  tmpView := TView(Form_ViewComboBox.Items.Objects[Form_ViewComboBox.ItemIndex]);

  if (Form_FixedWidthCheckBox.Checked) then
  begin
    Form_ColWidthEd.Enabled := True;
    if (column.FormWidth = -1) then //supply a default value
    begin
      Form_ColWidthEd.Text := '200';

      if (tmpView.Table = column.Table) then
        column.FormWidth := 200
      else if (tmpView.JoinTables.IndexOf(column.Table) <> -1) then    //if it is the column of a join-table
        column.Table.Join_Width:= 200
      else if (tmpView.NMTables.IndexOf(column.Table) <> -1) then    //if it is the column of an NM-table
        column.Table.NM_Width:= 200;

    end
    else
    begin
      if (tmpView.Table = column.Table) then
        Form_ColWidthEd.Text := IntToStr(column.formWidth)
      else if (tmpView.JoinTables.IndexOf(column.Table) <> -1) then    //if it is the column of a join-table
        Form_ColWidthEd.Text := IntToStr(column.Table.Join_Width)
      else if (tmpView.NMTables.IndexOf(column.Table) <> -1) then    //if it is the column of an NM-table
        Form_ColWidthEd.Text := IntToStr(column.Table.NM_Width);

    end;
  end
  else
  begin
    Form_ColWidthEd.Enabled := false;
    if (tmpView.Table = column.Table) then
      column.FormWidth := -1
    else if (tmpView.JoinTables.IndexOf(column.Table) <> -1) then    //if it is the column of a join-table
      column.Table.Join_Width:= -1
    else if (tmpView.NMTables.IndexOf(column.Table) <> -1) then    //if it is the column of an NM-table
      column.Table.NM_Width:= -1;
  end;
end;


procedure TMainForm.FormYEdExit(Sender: TObject);
var vInd :Integer;
    tmpView : TView;
begin
  vInd :=  Form_ViewComboBox.ItemIndex;
  if (vInd = -1) then exit;

  tmpView := TView(Form_ViewComboBox.Items.Objects[vInd]);
  tmpView.FormY := StrToInt(FormYEd.Text);
end;

procedure TMainForm.FormXEdExit(Sender: TObject);
var vInd :Integer;
    tmpView : TView;
begin
  vInd :=  Form_ViewComboBox.ItemIndex;
  if (vInd = -1) then exit;

  tmpView := TView(Form_ViewComboBox.Items.Objects[vInd]);
  tmpView.FormX := StrToInt(FormXEd.Text);
end;

procedure TMainForm.FormHeightEdExit(Sender: TObject);
var vInd :Integer;
    tmpView : TView;
begin
  vInd :=  Form_ViewComboBox.ItemIndex;
  if (vInd = -1) then exit;

  tmpView := TView(Form_ViewComboBox.Items.Objects[vInd]);

  tmpView.FormHeight := StrToInt(FormHeightEd.Text);
end;

procedure TMainForm.FormWidthEdExit(Sender: TObject);
var vInd :Integer;
    tmpView : TView;
begin
  vInd :=  Form_ViewComboBox.ItemIndex;
  if (vInd = -1) then exit;

  tmpView := TView(Form_ViewComboBox.Items.Objects[vInd]);
  tmpView.FormWidth := StrToInt(FormWidthEd.Text);
end;

procedure TMainForm.Form_UpBtnClick(Sender: TObject);
var cInd,vInd : Integer;
    view: TView;
    col1,col2 : SW_Column;
begin
  cInd := Form_FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) or (cInd-1 = -1) then exit;

  vInd := Form_ViewComboBox.ItemIndex;
  assert(vInd <> -1);
  view := TView(Form_ViewComboBox.Items.Objects[vInd]);

  col1 := SW_Column(Form_FieldsVisibleCheckListBox.Items.Objects[cInd]);
  col2 := SW_Column(Form_FieldsVisibleCheckListBox.Items.Objects[cInd-1]);

  view.ExchangeFormSortedColumns(cInd, cInd-1);
  //Form_FieldsVisibleCheckListBox.Items.Exchange(cInd, cInd-1);
  Form_FieldsVisibleCheckListBox.Items[cInd] := col2.table.name+'.'+col2.name;
  Form_FieldsVisibleCheckListBox.Items[cInd-1] := col1.Table.name+'.'+col1.name;
  Form_FieldsVisibleCheckListBox.Items.Objects[cInd] := col2;
  Form_FieldsVisibleCheckListBox.Items.Objects[cInd-1] := col1;

  Form_FieldsVisibleChecklistBox.Checked[cInd]:=col2.selectedForForm;
  Form_FieldsVisibleChecklistBox.Checked[cInd-1]:=col1.selectedForForm;

  Form_FieldsVisibleCheckListBox.Selected[cInd]:= false;
  Form_FieldsVisibleCheckListBox.Selected[cInd-1] := true;

end;

procedure TMainForm.Form_DownBtnClick(Sender: TObject);
var cInd,vInd : Integer;
    view: TView;
    col1,col2 : SW_Column;
begin
  cInd := Form_FieldsVisibleCheckListBox.ItemIndex;
  if (cInd = -1) or (cInd+1 >= Form_FieldsVisibleCheckListBox.Items.Count) then exit;

  vInd := Form_ViewComboBox.ItemIndex;
  assert(vInd <> -1);
  view := TView(Form_ViewComboBox.Items.Objects[vInd]);

  col1 := SW_Column(Form_FieldsVisibleCheckListBox.Items.Objects[cInd]);
  col2 := SW_Column(Form_FieldsVisibleCheckListBox.Items.Objects[cInd+1]);

  view.ExchangeFormSortedColumns(cInd, cInd+1);
  //Form_FieldsVisibleCheckListBox.Items.Exchange(cInd, cInd+1);
  Form_FieldsVisibleCheckListBox.Items[cInd] := col2.table.name+'.'+col2.name;
  Form_FieldsVisibleCheckListBox.Items[cInd+1] := col1.Table.name+'.'+col1.name;
  Form_FieldsVisibleCheckListBox.Items.Objects[cInd] := col2;
  Form_FieldsVisibleCheckListBox.Items.Objects[cInd+1] := col1;

  Form_FieldsVisibleChecklistBox.Checked[cInd]:=col2.selectedForForm;
  Form_FieldsVisibleChecklistBox.Checked[cInd+1]:=col1.selectedForForm;

  Form_FieldsVisibleCheckListBox.Selected[cInd]:= false;
  Form_FieldsVisibleCheckListBox.Selected[cInd+1] := true;

end;







procedure TMainForm.About1Click(Sender: TObject);
begin
  SplashForm := TSplashForm.Create(nil);
  SplashForm.VersionLbl.Caption:=Version;
  SplashForm.ShowModal;
  SplashForm.Release;
end;


procedure TMainForm.PageControlTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  PageControl.ActivePageIndex:=PageControlTreeView.Selected.AbsoluteIndex;
  PageControlTitleLbl.Caption:=PageControlTreeView.Selected.Text;
end;

procedure TMainForm.SaveMIClick(Sender: TObject);
begin
  SaveBtnClick(self);
end;

procedure TMainForm.SaveBtnClick(Sender: TObject);
var mydata: TEERPluginData;
    stringList :TStringList;
    theData: IXMLSWF_DataType;
    document : IXMLDocument;
    xmlString : String;
begin

  //unfortunately the onclick-event is processed before OnExit-Events that
  //logically happened before the click
  TakeSettings;

  theData := NewSWF_Data;
  Output.SaveToXmlNode(theData);

  document := theData.OwnerDocument;
  //save the xml-information into a normal string
  document.SaveToXml(xmlString);

  //now replace all occurrencies of '<' and '>' in the string
 // xmlString := DMMain.ReplaceString(xmlString,'<','\111');
 // xmlString := DMMain.ReplaceString(xmlString,'>','\112');

  //save the date
  stringList := TStringList.Create;
  stringList.Add(xmlString);

  //check first if we already have an entry
  mydata:=EERModel.GetPluginDataById('SimpleWebFront', 0);
  if (mydata <> nil) then
  begin
    mydata.Params := stringList;
  end
  else
  begin
    mydata:=EERModel.AddPluginData('SimpleWebFront', 0);
    mydata.Params := stringList;
  end;
  EERModel.SaveToFile(xmlFilename);
  StatusBar.SimpleText := SaveSuccessful + xmlFilename;
end;

procedure TMainForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;



procedure TMainForm.ReportingBugs1Click(Sender: TObject);
begin
  DialogBugsForm := TDialogBugsForm.Create(nil);
  try
    DialogBugsForm.ShowModal();
  finally
    DialogBugsForm.Release;
  end;
end;









end.
