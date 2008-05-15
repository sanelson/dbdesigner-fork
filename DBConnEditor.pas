unit DBConnEditor;

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
// Unit DBConnEditor.pas
// ---------------------
// Version 1.1, 01.04.2003, Mike
// Description
//   Used by the DBConn Module to let the user edit the connection parameters
//
// Changes:
//   Version 1.1, 01.04.2003, Mike
//     Removed RefreshParams call from various OnExit Events
//     causing the new entered information to be overwritten
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Qt, SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QGrids, QButtons, DBDM, SqlExpr, IniFiles, QExtCtrls, QComCtrls, StrUtils;

type
  TDBConnEditorForm = class(TForm)
    ConnectBtn: TBitBtn;
    CancelBtn: TSpeedButton;
    MainPageControl: TPageControl;
    GeneralSheet: TTabSheet;
    AdvancedSheet: TTabSheet;
    Label6: TLabel;
    DBConnEd: TEdit;
    ParamStrGrid: TStringGrid;
    ResetDefaultParamsBtn: TSpeedButton;
    AddParamBtn: TSpeedButton;
    DelParamBtn: TSpeedButton;
    ConnectionSettingsGBox: TGroupBox;
    Bevel2: TBevel;
    Bevel3: TBevel;
    HostIPLbl: TLabel;
    DatabaseLbl: TLabel;
    DBEd: TEdit;
    Label5: TLabel;
    CommentEd: TMemo;
    UsernameLbl: TLabel;
    UserNameEd: TEdit;
    PasswordEd: TEdit;
    PwdLbl: TLabel;
    PortLbl: TLabel;
    PortEd: TEdit;
    HostIPEd: TComboBox;
    DatabaseTypesCBox: TComboBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure SetData(theDBConn: TDBConn; theDBHosts: TList; dbtype: string = ''; host: string = ''; db: string = ''; user: string = '');

    procedure ResetDefaultParamsBtnClick(Sender: TObject);
    procedure ParamStrGridDblClick(Sender: TObject);
    procedure AddParamBtnClick(Sender: TObject);
    procedure DelParamBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DBEdExit(Sender: TObject);
    procedure CommentEdExit(Sender: TObject);
    procedure DBConnEdExit(Sender: TObject);

    procedure RefreshParams;
    procedure UserNameEdExit(Sender: TObject);
    procedure PasswordEdExit(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DatabaseTypesCBoxCloseUp(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckHostEdits;
    function RemoveHostCaption(Hostname: string): string;
  private
    { Private-Deklarationen }
    DBConn: TDBConn;
    DBHosts: TList;
  public
    { Public-Deklarationen }
    DBConn2Change: TDBConn;
  end;

var
  DBConnEditorForm: TDBConnEditorForm;

implementation

uses MainDM, DBConnSelect;

{$R *.xfm}

procedure TDBConnEditorForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self, True);

  //These Values must not be translated
  ParamStrGrid.Cells[0, 0]:='Param';
  ParamStrGrid.Cells[1, 0]:='Value';

  ParamStrGrid.Cells[0, 1]:='DriverName';
  ParamStrGrid.Cells[0, 2]:='GetDriverFunc';
  ParamStrGrid.Cells[0, 3]:='LibraryName';
  ParamStrGrid.Cells[0, 4]:='VendorLib';
  ParamStrGrid.Cells[0, 5]:='TableScope';
  ParamStrGrid.Cells[0, 6]:='Params';

  DatabaseTypesCBox.Items.Assign(DMDB.DatabaseTypes);
  DatabaseTypesCBox.ItemIndex:=0;

  ParamStrGrid.ColWidths[0]:=140;
  ParamStrGrid.ColWidths[1]:=220;
end;

procedure TDBConnEditorForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TDBConnEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TDBConnEditorForm.SetData(theDBConn: TDBConn; theDBHosts: TList; dbtype: string = ''; host: string = ''; db: string = ''; user: string = '');
var i: integer;
begin
  //Store the DBConn which has to be modified,
  //may be NIL if a new connection is created
  DBConn2Change:=theDBConn;

  DBHosts:=theDBHosts;

  //Create new temporary DBConn
  if(DBConn2Change=nil)then
  begin
    DBConn:=DMDB.GetNewDBConn('New Connection', False, dbtype);
    if(dbtype='MySQL')then
    begin
      DBConn.Params.Values['HostName']:=host;
      DBConn.Params.Values['User_Name']:=user;
      DBConn.Params.Values['Database']:=db;
    end;
  end
  else
  begin
    DBConn:=TDBConn.Create;
    DBConn.Assign(DBConn2Change);
  end;

  RefreshParams;

  if(DBConn.Name='New Connection')then
  begin
    DBConnEd.SelectAll;
    ActiveControl:=DBConnEd;
  end;

  HostIPEd.Items.Clear;
  for i:=0 to DBHosts.Count-1 do
    if(TDBHost(DBHosts[i]).Caption<>TDBHost(DBHosts[i]).HostName)then
      HostIPEd.Items.Add(TDBHost(DBHosts[i]).Caption+
        ' ('+TDBHost(DBHosts[i]).HostName+')')
    else
      HostIPEd.Items.Add(TDBHost(DBHosts[i]).HostName);
end;

procedure TDBConnEditorForm.ResetDefaultParamsBtnClick(Sender: TObject);
var theIni: TMemIniFile;
  theName: string;
begin
  //Copy settings from defaults
  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBConn_DefaultSettings.ini');
  try
    theName:=DBConn.Name;
    DMDB.ReadDBConnFromIniFile(theIni, DMDB.DatabaseTypes[DatabaseTypesCBox.ItemIndex], DBConn);

    DBConn.Name:=theName;

    theIni.UpdateFile;
  finally
    theIni.Free;
  end;

  RefreshParams;
end;

procedure TDBConnEditorForm.ParamStrGridDblClick(Sender: TObject);
var param, s: string;
begin
  s:=ParamStrGrid.Cells[1, ParamStrGrid.Row];
  param:=ParamStrGrid.Cells[0, ParamStrGrid.Row];
  if(DMMain.ShowStringEditor('Parameter', param, s))then
    ParamStrGrid.Cells[1, ParamStrGrid.Row]:=s;

  //Store parameters
  if(CompareText(param, 'Description')=0)then
    DBConn.Description:=s
  else if(CompareText(param, 'DriverName')=0)then
    DBConn.DriverName:=s
  else if(CompareText(param, 'GetDriverFunc')=0)then
    DBConn.GetDriverFunc:=s
  else if(CompareText(param, 'LibraryName')=0)then
    DBConn.LibraryName:=s
  else if(CompareText(param, 'VendorLib')=0)then
    DBConn.VendorLib:=s
  else if(CompareText(param, 'TableScope')=0)then
  begin
    DBConn.tablescope:=[];
    if(Pos('tsTable', s)>0)then
      DBConn.tablescope:=DBConn.tablescope+[tsTable];
    if(Pos('tsView', s)>0)then
      DBConn.tablescope:=DBConn.tablescope+[tsView];
    if(Pos('tsSysTable', s)>0)then
      DBConn.tablescope:=DBConn.tablescope+[tsSysTable];
    if(Pos('tsSynonym', s)>0)then
      DBConn.tablescope:=DBConn.tablescope+[tsSynonym];
  end
  else
    DBConn.Params.Values[param]:=s;

  RefreshParams;
end;

procedure TDBConnEditorForm.AddParamBtnClick(Sender: TObject);
var s: string;
begin
  s:='';

  if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('New Parameter', 103),
    DMMain.GetTranslatedMessage('Parameter Name:', 104), s))then
    if(s<>'')then
      DBConn.Params.Add(s+'=');

  RefreshParams;
end;

procedure TDBConnEditorForm.DelParamBtnClick(Sender: TObject);
var param: string;
begin
  param:=ParamStrGrid.Cells[0, ParamStrGrid.Row];

  if(CompareText(param, 'Description')=0)or
    (CompareText(param, 'DriverName')=0)or
    (CompareText(param, 'GetDriverFunc')=0)or
    (CompareText(param, 'LibraryName')=0)or
    (CompareText(param, 'VendorLib')=0)then
    ShowMessage(DMMain.GetTranslatedMessage('You cannot delete this Parameter.', 105))
  else
    if(MessageDlg(DMMain.GetTranslatedMessage('Do you want to delete the selected Parameter?', 106),
      mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
      DBConn.Params.Delete(DBConn.Params.IndexOfName(param));

  RefreshParams;
end;

procedure TDBConnEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_F1)then
    DMMain.ShowHelp('db', 'dbconnparamdlg');
end;

procedure TDBConnEditorForm.DBEdExit(Sender: TObject);
begin
  //Update Database
  DBConn.Params.Values['Database']:=DBEd.Text;

  //RefreshParams;
end;

procedure TDBConnEditorForm.CommentEdExit(Sender: TObject);
begin
  //Update Description
  DBConn.Description:=CommentEd.Text;
  
  //RefreshParams;
end;

procedure TDBConnEditorForm.ConnectBtnClick(Sender: TObject);
var i, j: integer;
  theDBHost: TDBHost;
  theChildTreeNode: TTreeNode;
begin
  if(Pos('(', HostIPEd.Text)>0)then
    DBConn.Params.Values['HostCaption']:=Trim(Copy(HostIPEd.Text, 1, Pos('(', HostIPEd.Text)-1))
  else
    DBConn.Params.Values['HostCaption']:=HostIPEd.Text;

  //Remove HostCaption ( ) from Hostname
  HostIPEd.Text:=RemoveHostCaption(HostIPEd.Text);
  DBConn.Params.Values['HostName']:=HostIPEd.Text;

  //Check Hosts
  theDBHost:=nil;
  for i:=0 to DBHosts.Count-1 do
  begin
    if(CompareText(TDBHost(DBHosts[i]).HostName, HostIPEd.Text)=0)then
    begin
      theDBHost:=DBHosts[i];
      break;
    end;
  end;

  //if the Host was not found, add new host
  if(theDBHost=nil)and(HostIPEd.Text<>'')then
  begin
    theDBHost:=TDBHost.Create;
    theDBHost.Caption:=DBConn.Params.Values['HostCaption'];
    theDBHost.HostName:=DBConn.Params.Values['HostName'];
    theDBHost.User_Name:='root';

    theChildTreeNode:=DBConnSelectForm.DBConnTV.Items.Insert(DBConnSelectForm.NetworkHosts.Item[DBConnSelectForm.NetworkHosts.Count-1], theDBHost.Caption);
    theChildTreeNode.Data:=theDBHost;
    theChildTreeNode.ImageIndex:=4;
    theChildTreeNode.SelectedIndex:=4;
    theChildTreeNode.SubItems.Add('HOST');
    theChildTreeNode.SubItems.Add(theDBHost.HostName);
    theChildTreeNode.SubItems.Add(theDBHost.User_Name);
    theChildTreeNode.SubItems.Add('');
  end;

  //Check if there are two connections with the same name
  j:=0;
  while(j=0)do
  begin
    j:=1;
    for i:=0 to DMDB.DBConnections.Count-1 do
      if(TDBConn(DMDB.DBConnections[i]).Name=DBConn.Name)and(DMDB.DBConnections[i]<>DBConn2Change)then
      begin
        try
          DBConn.Name:=LeftStr(DBConn.Name, Length(DBConn.Name)-1)+IntToStr(StrToInt(RightStr(DBConn.Name, 1))+1)
        except
          DBConn.Name:=DBConn.Name+'_2';
        end;

        j:=0;
        break;
      end;
  end;
  //End Check

  //Apply changes to DBConn2Change
  if(DBConn2Change<>nil)then
    DBConn2Change.Assign(DBConn)
  else
  begin
     DBConn2Change:=TDBConn.Create;
     DBConn2Change.Assign(DBConn);
     DMDB.DBConnections.Add(DBConn2Change);
  end;

  DBConn.Free;

  ModalResult:=mrOK;
end;

procedure TDBConnEditorForm.DBConnEdExit(Sender: TObject);
begin
  //Set DBConn Name
  DBConn.Name:=DBConnEd.Text;
end;

procedure TDBConnEditorForm.RefreshParams;
var tablescope: string;
  i: integer;
begin
  //Conn Name
  DBConnEd.Text:=DBConn.Name;

  //HOST & IP
  if(DBConn.Params.Values['HostCaption']<>'')and
    (DBConn.Params.Values['HostName']<>'')then
    HostIPEd.Text:=DBConn.Params.Values['HostCaption']+' ('+
      DBConn.Params.Values['HostName']+')'
  else if(DBConn.Params.Values['HostName']<>'')then
    HostIPEd.Text:=DBConn.Params.Values['HostName'];

  //Database & Type
  if(DBConn.Params.Values['Database']<>'')then
    DBEd.Text:=DBConn.Params.Values['Database'];
  DatabaseTypesCBox.ItemIndex:=DatabaseTypesCBox.Items.IndexOf(DBConn.DriverName);
  if(DatabaseTypesCBox.ItemIndex=-1)then
    DatabaseTypesCBox.ItemIndex:=DatabaseTypesCBox.Items.IndexOf(Copy(DBConn.DriverName, 5, 8));
  CheckHostEdits;

  //Username & Pwd
  if(DBConn.Params.Values['User_Name']<>'')then
    UserNameEd.Text:=DBConn.Params.Values['User_Name'];
  if(DBConn.Params.Values['Password']<>'')then
    PasswordEd.Text:=DBConn.Params.Values['Password'];

  //Comment
  CommentEd.Text:=DBConn.Description;

  //------------------
  //Advanced Grid

  ParamStrGrid.RowCount:=6+DBConn.Params.Count;
  ParamStrGrid.Cols[1].Clear;

  ParamStrGrid.Cells[1, 1]:=DBConn.DriverName;
  ParamStrGrid.Cells[1, 2]:=DBConn.GetDriverFunc;
  ParamStrGrid.Cells[1, 3]:=DBConn.LibraryName;
  ParamStrGrid.Cells[1, 4]:=DBConn.VendorLib;

  tablescope:='';
  if(tsTable in DBConn.tablescope)then
    tablescope:=tablescope+'tsTable, ';
  if(tsView in DBConn.tablescope)then
    tablescope:=tablescope+'tsView, ';
  if(tsSysTable in DBConn.tablescope)then
    tablescope:=tablescope+'tsSysTable, ';
  if(tsSynonym in DBConn.tablescope)then
    tablescope:=tablescope+'tsSynonym';

  ParamStrGrid.Cells[1, 5]:=tablescope;

  for i:=0 to DBConn.Params.Count-1 do
  begin
    ParamStrGrid.Cells[0, 6+i]:=Copy(DBConn.Params[i], 1, Pos('=', DBConn.Params[i])-1);
    ParamStrGrid.Cells[1, 6+i]:=DBConn.Params.Values[ParamStrGrid.Cells[0, 6+i]];
  end;
end;

procedure TDBConnEditorForm.UserNameEdExit(Sender: TObject);
begin
  //Update Username
  DBConn.Params.Values['User_Name']:=UserNameEd.Text;

  //RefreshParams;
end;

procedure TDBConnEditorForm.PasswordEdExit(Sender: TObject);
begin
  //Update Password
  DBConn.Params.Values['Password']:=PasswordEd.Text;

  //RefreshParams;
end;

procedure TDBConnEditorForm.CancelBtnClick(Sender: TObject);
begin
  DBConn.Free;

  ModalResult:=mrAbort;
end;

procedure TDBConnEditorForm.DatabaseTypesCBoxCloseUp(Sender: TObject);
begin
  if(DatabaseTypesCBox.ItemIndex<>0)then
  begin
    HostIPEd.Text:='';
    DBEd.Text:='';
    UserNameEd.Text:='';
    PasswordEd.Text:='';
    PortEd.Text:='';
  end;

  CheckHostEdits;

  ResetDefaultParamsBtnClick(self);

  DBEd.SelectAll;
  DBEd.SetFocus;
end;

procedure TDBConnEditorForm.CheckHostEdits;
begin
  if(CompareText(
    DatabaseTypesCBox.Items[DatabaseTypesCBox.ItemIndex], 'MySQL')=0)then
  begin
    HostIPLbl.Enabled:=True;
    HostIPEd.Enabled:=True;
    PortLbl.Enabled:=True;
    PortEd.Enabled:=False;
    if(PortEd.Text='')then
      PortEd.Text:='3306';

    DatabaseLbl.Caption:=DMMain.GetTranslatedMessage('Database Name:', 140);
  end
  else if(CompareText(
    DatabaseTypesCBox.Items[DatabaseTypesCBox.ItemIndex], 'MSSQL')=0)then
  begin
    HostIPLbl.Enabled:=True;
    HostIPEd.Enabled:=True;
    PortLbl.Enabled:=False;
    PortEd.Enabled:=False;

    DatabaseLbl.Caption:=DMMain.GetTranslatedMessage('Database Name:', 140);    
  end
  else
  begin
    HostIPLbl.Enabled:=False;
    HostIPEd.Enabled:=False;
    PortEd.Enabled:=False;
    PortLbl.Enabled:=False;

    //Rename DatabaseLbl for ODBC Conn
    if(CompareText(DatabaseTypesCBox.Items[DatabaseTypesCBox.ItemIndex], 'ODBC')=0)then
      DatabaseLbl.Caption:=DMMain.GetTranslatedMessage('ODBC DNS Name:', 141)
    else
    //Rename DatabaseLbl for ORCL Conn
    if(CompareText(DatabaseTypesCBox.Items[DatabaseTypesCBox.ItemIndex], 'Oracle')=0)then
      DatabaseLbl.Caption:=DMMain.GetTranslatedMessage('Service Name:', 142)
    else
    //Rename DatabaseLbl for SQLite
    if(CompareText(DatabaseTypesCBox.Items[DatabaseTypesCBox.ItemIndex], 'SQLite')=0)then
      DatabaseLbl.Caption:=DMMain.GetTranslatedMessage('Database File:', 143)
    else
      DatabaseLbl.Caption:=DMMain.GetTranslatedMessage('Database Name:', 140);
      
  end;


end;

procedure TDBConnEditorForm.FormShow(Sender: TObject);
begin
  try
    if(CanFocus)then
      SetFocus;
  except
  end;
end;

function TDBConnEditorForm.RemoveHostCaption(Hostname: string): string;
begin
  if(Pos('(', Hostname)>0)then
    RemoveHostCaption:=Copy(Hostname, Pos('(', Hostname)+1, Length(Hostname)-Pos('(', Hostname)-1)
  else
    RemoveHostCaption:=Hostname;
end;

end.
