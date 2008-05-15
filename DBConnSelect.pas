unit DBConnSelect;

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
// Unit DBConnLogin.pas
// --------------------
// Version 1.1, 21.03.2003, Mike
// Description
//   Contains the TDBConnSelectForm form which is used to manage database connections
//
// Changes:
//   Version 1.1, 21.03.2003, Mike
//     Prevent DBConns with the same name after drag'n'drop a
//     database onto the DBConn List
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Qt, SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QImgList, QComCtrls, DBDM, QGrids, QButtons, SqlExpr,
  QExtCtrls, IniFiles, QMenus, QTypes, StrUtils, Contnrs;

type
  TDBConnSelectForm = class(TForm)
    ConnectionPnl: TPanel;
    DBConnImgList: TImageList;
    TopPnl: TPanel;
    Label4: TLabel;
    LeftPnl: TPanel;
    HSplitter: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    CancelBtn: TSpeedButton;
    Label2: TLabel;
    UsernameEd: TEdit;
    Label3: TLabel;
    PasswdEd: TEdit;
    ConnectBtn: TBitBtn;
    Label6: TLabel;
    ConnectionsListView: TListView;
    Panel2: TPanel;
    DBConnLbl: TLabel;
    DBConnPopupMenu: TPopupMenu;
    DeleteConnectionMI: TMenuItem;
    HostsPopupMenu: TPopupMenu;
    RenameHostMI: TMenuItem;
    DeleteHostMI: TMenuItem;
    ChangeHostsIPMI: TMenuItem;
    DBConnEd: TEdit;
    Panel5: TPanel;
    Panel6: TPanel;
    DBConnTV: TTreeView;
    NewConnBtn: TSpeedButton;
    N1: TMenuItem;
    DropDatabaseMI: TMenuItem;
    AddConnection1: TMenuItem;
    N2: TMenuItem;
    DelConSBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure GetNetworkHosts;
    procedure StoreNetworkHosts;

    procedure SetData(theDBConns: TObjectList; theSelDBConn: TDBConn);
    procedure ConnectBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ConnectionsListViewSelectItem(Sender: TObject;
      Item: TListItem; Selected: Boolean);
    procedure FormShow(Sender: TObject);
    procedure ConnectionsListViewEdited(Sender: TObject; Item: TListItem;
      var S: WideString);
    procedure DelConnBtnClick(Sender: TObject);
    procedure ConnectionsListViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ConnectionsListViewDblClick(Sender: TObject);
    procedure DBConnTVExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure DBConnTVItemClick(Sender: TObject; Button: TMouseButton;
      Node: TTreeNode; const Pt: TPoint);
    procedure RenameHostMIClick(Sender: TObject);
    procedure RenameHostMIShow(Sender: TObject);
    procedure ChangeHostsIPMIClick(Sender: TObject);
    procedure DeleteHostMIClick(Sender: TObject);
    procedure DBConnTVMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DBConnTVCustomDrawItem(Sender: TCustomViewControl;
      Item: TCustomViewItem; Canvas: TCanvas; const Rect: TRect;
      State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure RefreshDBConnList;
    procedure ConnectionsListViewDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ConnectionsListViewDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure HSplitterMoved(Sender: TObject);
    procedure ConnectionsListViewClick(Sender: TObject);
    procedure NewConnBtnClick(Sender: TObject);
    procedure DropDatabaseMIShow(Sender: TObject);
    procedure DropDatabaseMIClick(Sender: TObject);
    procedure OnTypeSelectLBoxDblClick(Sender: TObject);
    procedure ResetDefaultParams(theDBConn: TDBConn);
    procedure AddConnection1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DelConSBtnClick(Sender: TObject);
  private
    { Private-Deklarationen }
    mx, my: integer;
    Localhost,
    MySqlNode, OracleNode, ODBCNode, SQLiteNode, MSSQLNode: TTreeNode;
    ModalResultIsOK: Boolean;
  public
    { Public-Deklarationen }
    SelDBConn: TDBConn;
    DBConns: TObjectList;
    DBHosts: TObjectList;
    NetworkHosts: TTreeNode;
  end;

var
  DBConnSelectForm: TDBConnSelectForm;

implementation

uses DBConnLogin, DBConnEditor, MainDM;

{$R *.xfm}

procedure TDBConnSelectForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self, True);
  
  ModalResultIsOK:=False;

  DBHosts:=TObjectList.Create;

  Left:=Application.MainForm.Left+(Application.MainForm.Width-746) div 2;
end;

procedure TDBConnSelectForm.FormDestroy(Sender: TObject);
begin
  StoreNetworkHosts;

  DBHosts.Free;
end;

procedure TDBConnSelectForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TDBConnSelectForm.GetNetworkHosts;
var i: integer;
  theTreeNode, theChildTreeNode: TTreeNode;
  theIni: TMemIniFile;
  hostcount: integer;
  theDBHost: TDBHost;
begin
  DBConnTV.Items.Clear;

  theTreeNode:=DBConnTV.Items.Add(nil, DMMain.GetTranslatedMessage('All Connections', 107));
  theTreeNode.ImageIndex:=0;
  theTreeNode.SelectedIndex:=1;

  //The MySQL Folder
  theTreeNode:=DBConnTV.Items.Add(theTreeNode, 'MySQL');
  theTreeNode.ImageIndex:=0;
  theTreeNode.SelectedIndex:=1;
  MySQLNode:=theTreeNode;

  //Make the MySQL Subs
  theDBHost:=TDBHost.Create;
  theDBHost.Caption:='Localhost';
  theDBHost.HostName:='127.0.0.1';
  theDBHost.User_Name:='root';
  DBHosts.Add(theDBHost);

  theTreeNode:=DBConnTV.Items.AddChild(theTreeNode, 'Localhost');
  theTreeNode.ImageIndex:=2;
  theTreeNode.SelectedIndex:=2;
  theTreeNode.SubItems.Add('HOST');
  theTreeNode.SubItems.Add('127.0.0.1');
  theTreeNode.SubItems.Add('root');
  theTreeNode.SubItems.Add('');
  Localhost:=theTreeNode;

  theChildTreeNode:=DBConnTV.Items.AddChild(theTreeNode,'...');
  theChildTreeNode.ImageIndex:=7;
  theChildTreeNode.SelectedIndex:=7;

  theTreeNode:=DBConnTV.Items.Add(theTreeNode, 'Network Hosts');
  theTreeNode.ImageIndex:=3;
  theTreeNode.SelectedIndex:=3;

  NetworkHosts:=theTreeNode;

  //Load Hosts from IniFile
  //Read IniFile
  try
    theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBConn_Hosts.ini');
    try
      Localhost.SubItems[2]:=
        theIni.ReadString('Hosts', 'LocalHost_User_Name', 'root');

      hostcount:=StrToInt(theIni.ReadString('Hosts', 'HostCount', '0'));

      for i:=1 to hostcount do
      begin
        theDBHost:=TDBHost.Create;
        theDBHost.Caption:=theIni.ReadString('Host'+IntToStr(i), 'HostName', 'Network Host');
        theDBHost.HostName:=theIni.ReadString('Host'+IntToStr(i), 'IP', '127.0.0.1');
        theDBHost.User_Name:=theIni.ReadString('Host'+IntToStr(i), 'User_Name', 'root');
        DBHosts.Add(theDBHost);

        theChildTreeNode:=DBConnTV.Items.AddChild(theTreeNode, theDBHost.Caption);
        theChildTreeNode.Data:=theDBHost;
        theChildTreeNode.ImageIndex:=4;
        theChildTreeNode.SelectedIndex:=4;
        theChildTreeNode.SubItems.Add('HOST');
        theChildTreeNode.SubItems.Add(theDBHost.HostName);
        theChildTreeNode.SubItems.Add(theDBHost.User_Name);
        theChildTreeNode.SubItems.Add('');

        theChildTreeNode:=DBConnTV.Items.AddChild(theChildTreeNode, '...');
        theChildTreeNode.ImageIndex:=7;
        theChildTreeNode.SelectedIndex:=7;
      end;

    finally
      theIni.Free;
    end;
  except
  end;

  theChildTreeNode:=DBConnTV.Items.AddChild(theTreeNode, '...');
  theChildTreeNode.ImageIndex:=4;
  theChildTreeNode.SelectedIndex:=4;

  theTreeNode.Expanded:=True;

  MySQLNode.Expanded:=True;

  //--------------------------------------------------------
  //The SQLite Folder
  theTreeNode:=DBConnTV.Items.Add(MySQLNode, 'SQLite');
  theTreeNode.ImageIndex:=0;
  theTreeNode.SelectedIndex:=1;
  SQLiteNode:=theTreeNode;

  //Make the SQLite Subs
  theChildTreeNode:=DBConnTV.Items.AddChild(theTreeNode, 'NewSQLiteConn');
  theChildTreeNode.ImageIndex:=7;
  theChildTreeNode.SelectedIndex:=7;
  theChildTreeNode.SubItems.Add('DB');
  theChildTreeNode.SubItems.Add('SQLite');

  //--------------------------------------------------------
  //The Oracle Folder
  theTreeNode:=DBConnTV.Items.Add(MySQLNode, 'Oracle');
  theTreeNode.ImageIndex:=0;
  theTreeNode.SelectedIndex:=1;
  OracleNode:=theTreeNode;

  //Make the Oracle Subs
  theChildTreeNode:=DBConnTV.Items.AddChild(theTreeNode, 'NewOracleConn');
  theChildTreeNode.ImageIndex:=7;
  theChildTreeNode.SelectedIndex:=7;
  theChildTreeNode.SubItems.Add('DB');
  theChildTreeNode.SubItems.Add('Oracle');

{$IFDEF MSWINDOWS}
  //--------------------------------------------------------
  //The MSSQL Folder
  theTreeNode:=DBConnTV.Items.Add(MySQLNode, 'MSSQL');
  theTreeNode.ImageIndex:=0;
  theTreeNode.SelectedIndex:=1;
  MSSQLNode:=theTreeNode;

  //Make the MSSQL Subs
  theChildTreeNode:=DBConnTV.Items.AddChild(theTreeNode, 'NewMSSQLConn');
  theChildTreeNode.ImageIndex:=7;
  theChildTreeNode.SelectedIndex:=7;
  theChildTreeNode.SubItems.Add('DB');
  theChildTreeNode.SubItems.Add('MSSQL');
{$ENDIF}

  //--------------------------------------------------------
  //The ODBC Folder
  theTreeNode:=DBConnTV.Items.Add(MySQLNode, 'ODBC');
  theTreeNode.ImageIndex:=0;
  theTreeNode.SelectedIndex:=1;
  ODBCNode:=theTreeNode;

  //Make the ODBC Subs
  theChildTreeNode:=DBConnTV.Items.AddChild(theTreeNode, 'NewODBCConn');
  theChildTreeNode.ImageIndex:=7;
  theChildTreeNode.SelectedIndex:=7;
  theChildTreeNode.SubItems.Add('DB');
  theChildTreeNode.SubItems.Add('ODBC');
end;

procedure TDBConnSelectForm.StoreNetworkHosts;
var i: integer;
  theIni: TMemIniFile;
begin
  //Load Hosts from IniFile
  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBConn_Hosts.ini');
  try
    theIni.WriteString('Hosts', 'LocalHost_User_Name', Localhost.SubItems[2]);

    theIni.WriteString('Hosts', 'HostCount', IntToStr(NetworkHosts.Count-1));

    for i:=1 to NetworkHosts.Count-1 do
    begin
      theIni.WriteString('Host'+IntToStr(i), 'HostName', NetworkHosts.Item[i-1].Text);
      theIni.WriteString('Host'+IntToStr(i), 'IP', NetworkHosts.Item[i-1].SubItems[1]);
      theIni.WriteString('Host'+IntToStr(i), 'User_Name', NetworkHosts.Item[i-1].SubItems[2]);
    end;

    theIni.UpdateFile;
  finally
    theIni.Free;
  end;
end;

procedure TDBConnSelectForm.SetData(theDBConns: TObjectList; theSelDBConn: TDBConn);
var i: integer;
begin
  DBConns:=theDBConns;

  GetNetworkHosts;
  DBConnTV.Selected:=DBConnTV.Items[0];
  DBConnTV.ScrollBy(0, -1000);

  RefreshDBConnList;

  for i:=0 to ConnectionsListView.Items.Count-1 do
    if(ConnectionsListView.Items[i].Data=theSelDBConn)then
      ConnectionsListView.Selected:=ConnectionsListView.Items[i];
end;

procedure TDBConnSelectForm.ConnectBtnClick(Sender: TObject);
begin
  if(DBConnEd.Text<>'')then
  begin
    //if the user entered an no existing host
    if(SelDBConn=nil)then
      SelDBConn:=DMDB.GetNewDBConn(DBConnEd.Text)
    else if(DBConnEd.Text<>SelDBConn.Name)then
      SelDBConn:=DMDB.GetNewDBConn(DBConnEd.Text);

    {if(TypeLU.ItemIndex<>-1)then
       SelDBConn.Params.Values['DriverName']:=TypeLU.Items[TypeLU.ItemIndex];}
    SelDBConn.Params.Values['User_Name']:=UsernameEd.Text;
    SelDBConn.Params.Values['Password']:=PasswdEd.Text;

    ModalResultIsOK:=True;

    ModalResult:=mrOK;
  end;
end;

procedure TDBConnSelectForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TDBConnSelectForm.ConnectionsListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if(ConnectionsListView.Selected<>nil)then
  begin
    if(Assigned(ConnectionsListView.Selected.Data))then
    begin
      SelDBConn:=TDBConn(ConnectionsListView.Selected.Data);

      DBConnEd.Text:=SelDBConn.Name;
      UsernameEd.Text:=SelDBConn.Params.Values['User_Name'];
      PasswdEd.Text:=SelDBConn.Params.Values['Password'];
      PasswdEd.SelectAll;
      //DescriptionMemo.Text:=SelDBConn.Description;

      if(Visible)then
        PasswdEd.SetFocus;
    end;
  end
  else
  begin
    SelDBConn:=nil;

    DBConnEd.Text:='';
    UsernameEd.Text:='';
    PasswdEd.Text:='';

    if(Visible)then
      DBConnEd.SetFocus;
  end;
end;

procedure TDBConnSelectForm.FormShow(Sender: TObject);
begin
  {try
    if(CanFocus)then
      SetFocus;
  except
  end;}
end;

procedure TDBConnSelectForm.ConnectionsListViewEdited(Sender: TObject;
  Item: TListItem; var S: WideString);
begin
  SelDBConn.Name:=s;

  DBConnEd.Text:=s;
end;

procedure TDBConnSelectForm.DelConnBtnClick(Sender: TObject);
begin
  if(SelDBConn<>nil)then
  begin
    if(MessageDlg(DMMain.GetTranslatedMessage('Do you really want to delete the selected DB-Connection?', 108),
      mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
    begin
      if(DBConns.IndexOf(SelDBConn)>-1)then
        DBConns.Delete(DBConns.IndexOf(SelDBConn));

      RefreshDBConnList;
    end;
  end;
end;

procedure TDBConnSelectForm.ConnectionsListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=X;
  my:=Y;
end;

procedure TDBConnSelectForm.OnTypeSelectLBoxDblClick(Sender: TObject);
begin
  TForm(TListBox(Sender).Parent).ModalResult:=mrOK;
end;

procedure TDBConnSelectForm.ResetDefaultParams(theDBConn: TDBConn);
var theIni: TMemIniFile;
  theName: string;
begin
  //Copy settings from defaults
  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBConn_DefaultSettings.ini');
  try
    theName:=theDBConn.Name;
    DMDB.ReadDBConnFromIniFile(theIni, theDBConn.DriverName, theDBConn);

    theDBConn.Name:=theName;
  finally
    theIni.Free;
  end;
end;

procedure TDBConnSelectForm.ConnectionsListViewDblClick(Sender: TObject);
var s: string;
  x1, x2: integer;
  TypeSelectForm: TForm;
  TypeSelectLBox: TListBox;
  theListItem: TListItem;
  i, j: integer;
begin
  //Connection Name
  x1:=0;
  x2:=ConnectionsListView.Columns[0].Width;

  //BUG in DELPHI 7
  theListItem:=ConnectionsListView.GetItemAt(10, my);
  if(theListItem<>nil)then
    ConnectionsListView.Selected:=theListItem;

  if(mx>x1)and(mx<x2)then
  begin
    s:=SelDBConn.Name;
    if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Connecion Name', 109), DMMain.GetTranslatedMessage('Name:', 110), s, 0, 2))then
      if(s<>'')then
      begin
        //Check if there are two connections with the same name
        j:=0;
        while(j=0)do
        begin
          j:=1;
          for i:=0 to DMDB.DBConnections.Count-1 do
            if(TDBConn(DMDB.DBConnections[i]).Name=s)and(DMDB.DBConnections[i]<>SelDBConn)then
            begin
              try
                s:=LeftStr(s, Length(s)-1)+IntToStr(StrToInt(RightStr(s, 1))+1)
              except
                s:=s+'_2';
              end;

              j:=0;
              break;
            end;
        end;
        //End Check

        SelDBConn.Name:=s;

        ConnectionsListView.Selected.Caption:=s;
      end;
  end;

  //Connection Type
  x1:=x2;
  x2:=x2+ConnectionsListView.Columns[1].Width;
  if(mx>x1)and(mx<x2)and(ConnectionsListView.Selected<>nil)then
  begin
    TypeSelectForm:=TForm.Create(self);
    try
      TypeSelectForm.Name:='TypeSelectForm';
      TypeSelectForm.FormStyle:=fsStayOnTop;
      {$IFDEF MSWINDOWS}
      TypeSelectForm.BorderStyle:=TFormBorderStyle(bsNone);
      {$ENDIF}
      {$IFDEF LINUX}
      TypeSelectForm.BorderStyle:=TFormBorderStyle(bsSingle);
      {$ENDIF}
      TypeSelectForm.Left:=Left+LeftPnl.Width+DBConnTV.Width+
        HSplitter.Width+ConnectionsListView.Columns[0].Width;
      TypeSelectForm.Top:=Top+TopPnl.Height+
        (ConnectionsListView.Selected.Index-
        ConnectionsListView.TopItem.Index)*18+28;

      TypeSelectLBox:=TListBox.Create(TypeSelectForm);
      TypeSelectLBox.Parent:=TypeSelectForm;
      TypeSelectLBox.Name:='TypeSelectLU';
      TypeSelectLBox.Left:=0;
      TypeSelectLBox.Top:=0;
      TypeSelectLBox.Items.Assign(DMDB.DatabaseTypes);
      TypeSelectLBox.ItemIndex:=TypeSelectLBox.Items.IndexOf(SelDBConn.DriverName);
      TypeSelectLBox.OnDblClick:=OnTypeSelectLBoxDblClick;

      TypeSelectForm.Width:=TypeSelectLBox.Width;
      TypeSelectForm.Height:=TypeSelectLBox.Height;

      TypeSelectForm.ShowModal;

      if(CompareText(SelDBConn.DriverName, TypeSelectLBox.Items[TypeSelectLBox.ItemIndex])<>0)then
      begin
        SelDBConn.DriverName:=TypeSelectLBox.Items[TypeSelectLBox.ItemIndex];
        ResetDefaultParams(SelDBConn);

        RefreshDBConnList;
      end;

    finally
      TypeSelectForm.Free;
    end;
  end;

  //HostName
  x1:=x2;
  x2:=x2+ConnectionsListView.Columns[2].Width;

  if(mx>x1)and(mx<x2)then
  begin
    s:=SelDBConn.Params.Values['HostName'];
    if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Host', 111),
      DMMain.GetTranslatedMessage('Host:', 112), s))then
      if(s<>'')then
      begin
        SelDBConn.Params.Values['HostName']:=s;

        ConnectionsListView.Selected.SubItems[1]:=s;
      end;
  end;

  //Database
  x1:=x2;
  x2:=x2+ConnectionsListView.Columns[3].Width;

  if(mx>x1)and
    (mx<x2)then
  begin
    s:=SelDBConn.Params.Values['Database'];
    if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Database', 113),
     DMMain.GetTranslatedMessage('Database:', 114), s))then
      if(s<>'')then
      begin
        SelDBConn.Params.Values['Database']:=s;

        ConnectionsListView.Selected.SubItems[2]:=s;
      end;
  end;

  //Description
  x1:=x2;
  x2:=x2+ConnectionsListView.Columns[4].Width;

  if(mx>x1)and
    (mx<x2)then
  begin
    s:=SelDBConn.Description;
    if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Description', 115),
      DMMain.GetTranslatedMessage('Description:', 116), s))then
      if(s<>'')then
      begin
        SelDBConn.Description:=s;

        ConnectionsListView.Selected.SubItems[3]:=s;
      end;
  end;

  //paramaters
  x1:=x2;
  x2:=x2+ConnectionsListView.Columns[5].Width;

  if(mx>x1)and(mx<x2)then
  begin
    DBConnEditorForm:=TDBConnEditorForm.Create(self);
    try
      DBConnEditorForm.SetData(SelDBConn, DBHosts, '', '', '', '');
      DBConnEditorForm.ShowModal;
      RefreshDBConnList;
    finally
      DBConnEditorForm.Free;
    end;
  end;

  SetFocus;
end;

procedure TDBConnSelectForm.DBConnTVExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var InfoDBConn: TDBConn;
  theChildTreeNode: TTreeNode;
  TryCount: integer;
begin
  AllowExpansion:=False;

  //Local Host Node
  if(Node.SubItems.Count>0)then
  begin
    if(Node.SubItems[0]='HOST')then
    begin
      InfoDBConn:=DMDB.GetNewDBConn('InfoDBConn', False, 'MySQL');
      try
        TryCount:=0;

        //Get User/Pwd
        DBConnLoginForm:=TDBConnLoginForm.Create(self);
        try
          InfoDBConn.Params.Values['HostName']:=Node.SubItems[1];
          InfoDBConn.Params.Values['Database']:='mysql';
          InfoDBConn.Params.Values['User_Name']:=Node.SubItems[2];
          InfoDBConn.Params.Values['Password']:=Node.SubItems[3];

          //Try as long as password matches or user selects abort
          while(1=1)do
          begin
            //The first time, try values stored with HOST
            if(TryCount>0)then
            begin
              DBConnLoginForm.SetData(InfoDBConn.Params.Values['User_Name']);

              if(DBConnLoginForm.ShowModal=mrAbort)then
                break;

              InfoDBConn.Params.Values['User_Name']:=DBConnLoginForm.GetUserName;
              InfoDBConn.Params.Values['Password']:=DBConnLoginForm.GetPassword;
            end;

            DMDB.DisconnectFromDB;

            try
              DMDB.ConnectToDB(InfoDBConn);
            except
              on x: Exception do
              begin
                if(TryCount>0)then
                  MessageDlg(DMMain.GetTranslatedMessage('Connection to database failed.'+#13#10#13#10+'%s',
                    121, x.Message), mtError, [mbOK], 0);

                inc(TryCount);
                continue;
              end;
            end;

            //if connection was successful, store username/pwd
            Node.SubItems[2]:=InfoDBConn.Params.Values['User_Name'];
            Node.SubItems[3]:=InfoDBConn.Params.Values['Password'];

            node.DeleteChildren;

            DMDB.SchemaSQLQuery.ParamCheck:=False;
            DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');
            DMDB.SchemaSQLQuery.SQL.Text:='show databases';
            DMDB.SchemaSQLQuery.Open;
            while(Not(DMDB.SchemaSQLQuery.EOF))do
            begin
              theChildTreeNode:=DBConnTV.Items.AddChild(node, DMDB.SchemaSQLQuery.Fields[0].AsString);
              theChildTreeNode.ImageIndex:=7;
              theChildTreeNode.SelectedIndex:=7;
              theChildTreeNode.SubItems.Add('DB');
              theChildTreeNode.SubItems.Add('MySQL');

              DMDB.SchemaSQLQuery.Next;
            end;
            DMDB.SchemaSQLQuery.Close;

            theChildTreeNode:=DBConnTV.Items.AddChild(node, '...');
            theChildTreeNode.ImageIndex:=7;
            theChildTreeNode.SelectedIndex:=7;

            DMDB.DisconnectFromDB;

            AllowExpansion:=True;

            DBConnTV.Selected:=node;
            RefreshDBConnList;

            break;
          end;
        finally
          DBConnLoginForm.Free;
        end;

        DBConnTV.SetFocus;
      finally
        InfoDBConn.Free;
      end;
    end;
  end
  else {if(node.Level=0)then}
    AllowExpansion:=True;
end;

procedure TDBConnSelectForm.DBConnTVItemClick(Sender: TObject;
  Button: TMouseButton; Node: TTreeNode; const Pt: TPoint);
var hostcaption, dbname, ip: string;
  theTreeNode, theChildTreeNode: TTreeNode;
  InfoDBConn: TDBConn;
  i: integer;
  theDBHost: TDBHost;
begin
  RefreshDBConnList;

  if(Node.Level>0)then
  begin
    if(Node.Parent.Text='Network Hosts')and(Node.Text='...')then
    begin
      hostcaption:='';
      if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Add new Host', 117),
        DMMain.GetTranslatedMessage('Hostname/IP:', 118), hostcaption))then
      begin
        if(hostcaption<>'')then
        begin
          //When / in hostname, extract ip
          if(Pos('/', hostcaption)>0)then
          begin
            ip:=Copy(hostcaption, Pos('/', hostcaption)+1, Length(hostcaption));
            hostcaption:=Copy(hostcaption, 1, Pos('/', hostcaption)-1);
          end
          else
            ip:=hostcaption;

          //Add to Host List
          theDBHost:=TDBHost.Create;
          theDBHost.Caption:=hostcaption;
          theDBHost.HostName:=ip;
          theDBHost.User_Name:='root';
          DBHosts.Add(theDBHost);

          //Add Tree
          theTreeNode:=DBConnTV.Items.Insert(Node, hostcaption);
          theTreeNode.Data:=theDBHost;
          theTreeNode.ImageIndex:=4;
          theTreeNode.SelectedIndex:=4;
          theTreeNode.SubItems.Add('HOST');
          theTreeNode.SubItems.Add(ip);
          theTreeNode.SubItems.Add('root');
          theTreeNode.SubItems.Add('');

          theChildTreeNode:=DBConnTV.Items.AddChild(theTreeNode, '...');
          theChildTreeNode.ImageIndex:=7;
          theChildTreeNode.SelectedIndex:=7;
        end;
      end;
    end
    else if(Node.Text='...')then
    begin
      if(Node.Parent.SubItems.Count>0)then
        if(Node.Parent.SubItems[0]='HOST')then
        begin
          dbname:='';
          if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Create new Database', 119),
            DMMain.GetTranslatedMessage('Database Name:', 120), dbname))then
          begin
            if(dbname<>'')then
            begin
              InfoDBConn:=DMDB.GetNewDBConn('InfoDBConn', False, 'MySQL');
              try
                InfoDBConn.Params.Values['HostName']:=Node.Parent.SubItems[1];
                InfoDBConn.Params.Values['Database']:='mysql';
                InfoDBConn.Params.Values['User_Name']:=Node.Parent.SubItems[2];
                InfoDBConn.Params.Values['Password']:=Node.Parent.SubItems[3];

                try
                  DMDB.ConnectToDB(InfoDBConn);
                except
                  on x: Exception do
                  begin
                    MessageDlg(DMMain.GetTranslatedMessage('Connection to database failed.', 212)+#13#10#13#10+
                      x.Message, mtError, [mbOK], 0);
                    Abort;
                  end;
                end;

                try
                  DMDB.SQLConn.ExecuteDirect('CREATE DATABASE '+dbname);
                except
                  on x: Exception do
                  begin
                    MessageDlg(DMMain.GetTranslatedMessage('Database could not be created.', 122)+#13#10#13#10+
                      x.Message, mtError, [mbOK], 0);
                    Abort;
                  end;
                end;

                DMDB.DisconnectFromDB;

                theTreeNode:=Node.Parent;
                theTreeNode.Expanded:=False;
                theTreeNode.Expanded:=True;

                //Select new created db
                for i:=0 to theTreeNode.Count-1 do
                  if(CompareText(theTreeNode.Item[i].Text, dbname)=0)then
                  begin
                    theTreeNode.Item[i].Selected:=True;
                    DBConnTVItemClick(self, mbLeft, theTreeNode.Item[i], Point(0, 0));

                    break;
                  end;
              finally
                InfoDBConn.Free;
              end;
            end;
          end;
        end;
    end;
  end;
end;

procedure TDBConnSelectForm.RenameHostMIClick(Sender: TObject);
var hostname: string;
begin
  if(DBConnTV.Selected<>nil)then
    if(DBConnTV.Selected.SubItems.Count>0)then
      if(DBConnTV.Selected.SubItems[0]='HOST')then
      begin
        hostname:=DBConnTV.Selected.Text;
        if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Rename Host', 123),
          DMMain.GetTranslatedMessage('Hostname:', 124), hostname))then
          if(hostname<>'')then
            DBConnTV.Selected.Text:=hostname;
      end;
end;

procedure TDBConnSelectForm.RenameHostMIShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=False;

  if(DBConnTV.Selected<>nil)then
    if(DBConnTV.Selected<>Localhost)then
      if(DBConnTV.Selected.SubItems.Count>0)then
        if(DBConnTV.Selected.SubItems[0]='HOST')then
          TMenuItem(Sender).Enabled:=True;

end;

procedure TDBConnSelectForm.ChangeHostsIPMIClick(Sender: TObject);
var ip: string;
begin
  if(DBConnTV.Selected<>nil)then
    if(DBConnTV.Selected.SubItems.Count>0)then
      if(DBConnTV.Selected.SubItems[0]='HOST')then
      begin
        ip:=DBConnTV.Selected.SubItems[1];
        if(DMMain.ShowStringEditor(DMMain.GetTranslatedMessage('Change Host''s IP', 125),
          DMMain.GetTranslatedMessage('IP:', 126), ip))then
          if(ip<>'')then
            DBConnTV.Selected.SubItems[1]:=ip;
      end;
end;

procedure TDBConnSelectForm.DeleteHostMIClick(Sender: TObject);
var theDBHost: TDBHost;
begin
  if(DBConnTV.Selected<>nil)then
    if(DBConnTV.Selected.SubItems.Count>0)then
      if(DBConnTV.Selected.SubItems[0]='HOST')then
      begin
        if(MessageDlg(DMMain.GetTranslatedMessage('Do you really want to delete this Host?', 127),
          mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
        begin
          //Delete Host from List
          theDBHost:=DBConnTV.Selected.Data;

          DBConnTV.Selected.Delete;

          if(theDBHost<>nil)then
          begin
            DBHosts.Delete(DBHosts.IndexOf(theDBHost));
          end;
        end;
      end;
end;

procedure TDBConnSelectForm.DBConnTVMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if(Button=mbRight)then
  begin
    DBConnTV.Selected:=DBConnTV.GetNodeAt(X, Y);
    DBConnTV.Repaint;
  end;

  if(Button=mbLeft)then
    DBConnTV.BeginDrag(False, 5);
end;

procedure TDBConnSelectForm.DBConnTVCustomDrawItem(
  Sender: TCustomViewControl; Item: TCustomViewItem; Canvas: TCanvas;
  const Rect: TRect; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
var x: integer;
  theRect: TRect;
begin
  x:=Rect.Left+2;
  theRect:=Rect;

  with Canvas do
  begin
    Brush.Color:=clWhite;
    FillRect(Rect);
    
    if(TTreeNode(Item).ImageIndex>-1)then
    begin
      if(TTreeNode(Item).Selected)then
      begin
        DBConnImgList.Draw(Canvas, x, Rect.Top+1, TTreeNode(Item).SelectedIndex);

        theRect.Left:=x+18;
        Brush.Color:=clActiveHighlight;
        FillRect(theRect);
      end
      else
        //Draw Folder Open if it is expanded
        if((TTreeNode(Item).ImageIndex=0)and(TTreeNode(Item).Expanded))then
          DBConnImgList.Draw(Canvas, x, Rect.Top+1, 1)
        else
          DBConnImgList.Draw(Canvas, x, Rect.Top+1, TTreeNode(Item).ImageIndex);
      x:=x+20;
    end;


    if(TTreeNode(Item).Selected)then
      Font.Color:=clWhite
    else
      Font.Color:=clBlack;
    TextOut(x, Rect.Top+2, TTreeNode(Item).Text);
  end;

  DefaultDraw:=False;
end;

procedure TDBConnSelectForm.RefreshDBConnList;
var theNode: TListItem;
  i, j: integer;
  ItemFound: Boolean;
  PrevSelName: string;
begin
  if(ConnectionsListView.Selected<>nil)then
    PrevSelName:=ConnectionsListView.Selected.Caption
  else
    PrevSelName:='';

  ConnectionsListView.Items.Clear;

  for i:=0 to DBConns.Count-1 do
  begin
    if(DBConnTV.Selected<>DBConnTV.Items[0])then
    begin
      if(DBConnTV.Selected=MySQLNode)then
      begin
        DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to MySQL Hosts', 128);

        if(CompareText(TDBConn(DBConns[i]).DriverName, 'MySQL')<>0)then
          continue;
      end
      else if(DBConnTV.Selected=OracleNode)then
      begin
        DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to Oracle', 129);

        if(CompareText(TDBConn(DBConns[i]).DriverName, 'Oracle')<>0)then
          continue;
      end
      else if(DBConnTV.Selected=ODBCNode)then
      begin
        DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to ODBC Databases', 130);

        if(CompareText(TDBConn(DBConns[i]).DriverName, 'OpenODBC')<>0)then
          continue;
      end
      else if(DBConnTV.Selected=SQLiteNode)then
      begin
        DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to SQLite Databases', 239);

        if(CompareText(TDBConn(DBConns[i]).DriverName, 'SQLite')<>0)then
          continue;
      end
      else if(DBConnTV.Selected=MSSQLNode)then
      begin
        DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to MSSQL Databases', 240);

        if(CompareText(TDBConn(DBConns[i]).DriverName, 'MSSQL')<>0)then
          continue;
      end
      else if(DBConnTV.Selected=NetworkHosts)then
      begin
        //Only display Networkhosts
        DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to Network Hosts', 131);

        ItemFound:=False;
        for j:=0 to NetworkHosts.Count-1 do
          if(NetworkHosts.Item[j].SubItems.Count>0)then
            if(CompareText(NetworkHosts.Item[j].SubItems[1], TDBConn(DBConns[i]).Params.Values['HostName'])=0)then
              ItemFound:=True;

        if(Not(ItemFound))then
          continue;
      end
      else if(DBConnTV.Selected=Localhost)then
      begin
        DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to Localhost', 132);

        if(CompareText(TDBConn(DBConns[i]).Params.Values['HostName'], 'localhost')<>0)and
          (CompareText(TDBConn(DBConns[i]).Params.Values['HostName'], '127.0.0.1')<>0)then
          continue;
      end
      else if(DBConnTV.Selected.SubItems.Count>0)then
      begin
        if(DBConnTV.Selected.SubItems[0]='HOST')then
        begin
          DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to the %s Host', 133, DBConnTV.Selected.SubItems[1]);

          if(CompareText(TDBConn(DBConns[i]).Params.Values['HostName'], DBConnTV.Selected.SubItems[1])<>0)then
            continue;
        end
        else if(DBConnTV.Selected.SubItems[0]='DB')then
        begin
          if(DBConnTV.Selected.SubItems[1]='MySQL')then
          begin
            DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to the %s MySQL Host', 134, DBConnTV.Selected.Parent.SubItems[1]);

            if(CompareText(TDBConn(DBConns[i]).Params.Values['HostName'], DBConnTV.Selected.Parent.SubItems[1])<>0)then
              continue;
          end
          else if(DBConnTV.Selected.SubItems[1]='Oracle')then
          begin
            DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to Oracle', 129);

            if(CompareText(TDBConn(DBConns[i]).DriverName, 'Oracle')<>0)then
              continue;
          end
          else if(DBConnTV.Selected.SubItems[1]='ODBC')then
          begin
            DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to ODBC Databases', 130);

            if(CompareText(TDBConn(DBConns[i]).DriverName, 'OpenODBC')<>0)then
              continue;
          end
          else if(DBConnTV.Selected.SubItems[1]='SQLite')then
          begin
            DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to ODBC Databases', 239);

            if(CompareText(TDBConn(DBConns[i]).DriverName, 'SQLite')<>0)then
              continue;
          end
          else if(DBConnTV.Selected.SubItems[1]='MSSQL')then
          begin
            DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to MSSQL Databases', 240);

            if(CompareText(TDBConn(DBConns[i]).DriverName, 'MSSQL')<>0)then
              continue;
          end
          else
          begin
            DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Connections to %s', 135, DBConnTV.Selected.SubItems[1]);

            if(CompareText(TDBConn(DBConns[i]).DriverName, DBConnTV.Selected.SubItems[1])<>0)then
              continue;
          end;
        end
        else
          continue;
      end
      else
        continue;
    end
    else
      DBConnLbl.Caption:=DMMain.GetTranslatedMessage('All Database Connections', 136);

    theNode:=ConnectionsListView.Items.Add;
    theNode.Caption:=TDBConn(DBConns[i]).Name;
    theNode.ImageIndex:=5;
    theNode.Data:=DBConns[i];
    theNode.SubItems.Add(TDBConn(DBConns[i]).DriverName);
    theNode.SubItems.Add(TDBConn(DBConns[i]).Params.Values['HostName']);
    theNode.SubItems.Add(TDBConn(DBConns[i]).Params.Values['Database']);
    theNode.SubItems.Add(TDBConn(DBConns[i]).Description);
    theNode.SubItems.Add('');
    theNode.SubItemImages[4]:=9;
  end;

  //Try to select prev. selected item
  if(PrevSelName<>'')then
  begin
    for i:=0 to ConnectionsListView.Items.Count-1 do
      if(ConnectionsListView.Items[i].Caption=PrevSelName)then
      begin
        ConnectionsListView.Selected:=ConnectionsListView.Items[i];
        break;
      end;
  end;
end;

procedure TDBConnSelectForm.ConnectionsListViewDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=False;

  if(Source<>nil)then
    if(Source.ClassNameIs('TTreeView'))then
      if(TTreeView(Source).Name='DBConnTV')then
        if(DBConnTV.Selected.SubItems.Count>0)then
          if(DBConnTV.Selected.SubItems[0]='DB')then
          Accept:=True;
end;

procedure TDBConnSelectForm.ConnectionsListViewDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var theDBConn: TDBConn;
  i, j: integer;
  s: string;
begin
  if(Source.ClassNameIs('TTreeView'))then
    if(TTreeView(Source).Name='DBConnTV')then
      if(DBConnTV.Selected<>nil)then
        if(DBConnTV.Selected.SubItems.Count>0)then
          if(DBConnTV.Selected.SubItems[0]='DB')then
          begin
            //Create a new DBConn of the given DatabaseType
            s:=DBConnTV.Selected.Text;

            //Check if there are two connections with the same name
            j:=0;
            while(j=0)do
            begin
              j:=1;
              for i:=0 to DMDB.DBConnections.Count-1 do
                if(TDBConn(DMDB.DBConnections[i]).Name=s)and(DMDB.DBConnections[i]<>SelDBConn)then
                begin
                  try
                    s:=LeftStr(s, Length(s)-1)+IntToStr(StrToInt(RightStr(s, 1))+1)
                  except
                    s:=s+'_2';
                  end;

                  j:=0;
                  break;
                end;
            end;
            //End Check

            theDBConn:=DMDB.GetNewDBConn(s, True, DBConnTV.Selected.SubItems[1]);
            if(CompareText(DBConnTV.Selected.SubItems[1], 'MySQL')=0)then
            begin
              theDBConn.Params.Values['HostCaption']:=DBConnTV.Selected.Parent.Text;
              theDBConn.Params.Values['HostName']:=DBConnTV.Selected.Parent.SubItems[1];
              theDBConn.Params.Values['User_Name']:=DBConnTV.Selected.Parent.SubItems[2];
              theDBConn.Params.Values['Database']:=DBConnTV.Selected.Text;
            end;
            RefreshDBConnList;

            //Select new created DBConn
            for i:=0 to ConnectionsListView.Items.Count-1 do
             if(ConnectionsListView.Items[i].Data=theDBConn)then
                ConnectionsListView.Selected:=ConnectionsListView.Items[i];

            ConnectionsListViewSelectItem(self,
              ConnectionsListView.Selected, True);
          end;
end;

procedure TDBConnSelectForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=True;

  if(Not(ModalResultIsOK))then
    ModalResult:=mrCancel;
end;

procedure TDBConnSelectForm.HSplitterMoved(Sender: TObject);
begin
  DBConnLbl.Left:=LeftPnl.Width+DBConnTV.Width+HSplitter.Width;
end;

procedure TDBConnSelectForm.ConnectionsListViewClick(Sender: TObject);
var x1, x2, i: integer;
  theData: Pointer;
  theListItem: TListItem;
begin
  x2:=0;
  for i:=0 to 4 do
    x2:=x2+ConnectionsListView.Columns[i].Width;

  //paramaters
  x1:=x2;
  x2:=x2+ConnectionsListView.Columns[5].Width;

  //BUG in DELPHI 7
  theListItem:=ConnectionsListView.GetItemAt(10, my);
  if(theListItem<>nil)then
    ConnectionsListView.Selected:=theListItem;

  if(mx>x1)and(mx<x2)and(ConnectionsListView.Selected<>nil)then
  begin
    DBConnEditorForm:=TDBConnEditorForm.Create(self);
    try
      theData:=ConnectionsListView.Selected.Data;

      DBConnEditorForm.SetData(SelDBConn, DBHosts);
      DBConnEditorForm.ShowModal;

      RefreshDBConnList;
      for i:=0 to ConnectionsListView.Items.Count-1 do
       if(ConnectionsListView.Items[i].Data=theData)then
         ConnectionsListView.Selected:=ConnectionsListView.Items[i];
    finally
      DBConnEditorForm.Free;
    end;
  end;
end;

procedure TDBConnSelectForm.NewConnBtnClick(Sender: TObject);
var db, dbtype, host, user: string;
  i: integer;
begin
  DBConnEditorForm:=TDBConnEditorForm.Create(self);
  try
    db:='';
    dbtype:='MySQL';
    host:='';
    user:='';

    if(DBConnTV.Selected<>nil)then
      if(DBConnTV.Selected.SubItems.Count>0)then
        if(DBConnTV.Selected.SubItems[0]='DB')then
        begin
          db:=DBConnTV.Selected.Text;
          dbtype:=DBConnTV.Selected.SubItems[1];

          if(CompareText(DBConnTV.Selected.SubItems[1], 'MySQL')=0)then
          begin
            host:=DBConnTV.Selected.Parent.Text;
            user:=DBConnTV.Selected.Parent.SubItems[2];
          end;
        end;

    if(DBConnTV.Selected<>nil)then
      if(DBConnTV.Selected.SubItems.Count>0)then
        if(DBConnTV.Selected.SubItems[0]='HOST')then
        begin
          host:=DBConnTV.Selected.Text;
          user:=DBConnTV.Selected.SubItems[2];
        end;

    if(DBConnTV.Selected<>nil)then
      if(DBConnTV.Selected.SelectedIndex=1)then
        if(DBConnTV.Selected.Text<>DMMain.GetTranslatedMessage('All Connections', 107))then
        begin
          dbtype:=DBConnTV.Selected.Text;
        end;

    DBConnEditorForm.SetData(nil, DBHosts, dbtype, host, db, user);
    if(DBConnEditorForm.ShowModal=mrOK)then
    begin
      RefreshDBConnList;
      for i:=0 to ConnectionsListView.Items.Count-1 do
        if(CompareText(ConnectionsListView.Items[i].Caption, DBConnEditorForm.DBConn2Change.Name)=0)then
        begin
          ConnectionsListView.Selected:=ConnectionsListView.Items[i];
          ConnectionsListViewSelectItem(self, ConnectionsListView.Items[i], True);
          break;
        end;
    end;

  finally
    DBConnEditorForm.Free;
  end;
end;

procedure TDBConnSelectForm.DropDatabaseMIShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=False;

  if(DBConnTV.Selected<>nil)then
    if(DBConnTV.Selected<>Localhost)then
      if(DBConnTV.Selected.SubItems.Count>0)then
        if(DBConnTV.Selected.SubItems[0]='DB')then
          if(DBConnTV.Selected.SubItems[1]='MySQL')then
            TMenuItem(Sender).Enabled:=True;
end;

procedure TDBConnSelectForm.DropDatabaseMIClick(Sender: TObject);
var InfoDBConn: TDBConn;
begin
  if(DBConnTV.Selected<>nil)then
    if(DBConnTV.Selected<>Localhost)then
      if(DBConnTV.Selected.SubItems.Count>0)then
        if(DBConnTV.Selected.SubItems[0]='DB')then
        begin
          if(CompareText(DBConnTV.Selected.Text, 'mysql')=0)then
          begin
            MessageDlg(DMMain.GetTranslatedMessage('You cannot delete the MySQL Database', 137),
              mtError, [mbOK], 0);

            Exit;
          end;

          if(MessageDlg(DMMain.GetTranslatedMessage('Do you want to drop the selected database %s'+
            #13#10#13#10+
            'All data will be lost!', 138, DBConnTV.Selected.Text), mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
          begin
            if(MessageDlg(DMMain.GetTranslatedMessage('Do you REALLY want to drop the selected database %s'+
              #13#10#13#10+
              'The Database cannot be restored!', 139, DBConnTV.Selected.Text), mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
            begin
              InfoDBConn:=DMDB.GetNewDBConn('InfoDBConn', False, 'MySQL');
              try
                InfoDBConn.Params.Values['HostName']:=DBConnTV.Selected.Parent.SubItems[1];
                InfoDBConn.Params.Values['Database']:='mysql';
                InfoDBConn.Params.Values['User_Name']:=DBConnTV.Selected.Parent.SubItems[2];
                InfoDBConn.Params.Values['Password']:=DBConnTV.Selected.Parent.SubItems[3];

                try
                  DMDB.ConnectToDB(InfoDBConn);
                except
                  on x: Exception do
                  begin
                    MessageDlg(DMMain.GetTranslatedMessage('Connection to database failed.'+#13#10#13#10+'%s',
                      121, x.Message), mtError, [mbOK], 0);
                    Abort;
                  end;
                end;

                try
                  DMDB.SQLConn.ExecuteDirect('DROP DATABASE '+DBConnTV.Selected.Text);
                except
                  on x: Exception do
                  begin
                    MessageDlg('Database could not be dropped.'+#13#10#13#10+
                      x.Message, mtError, [mbOK], 0);
                    Abort;
                  end;
                end;

                DMDB.DisconnectFromDB;

                DBConnTV.Items.Delete(DBConnTV.Selected);
              finally
                InfoDBConn.Free;
              end;
            end;
          end;
        end;
end;

procedure TDBConnSelectForm.AddConnection1Click(Sender: TObject);
begin
  NewConnBtnClick(self);
end;

procedure TDBConnSelectForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_F1)then
    DMMain.ShowHelp('db', 'dbconn');
end;

procedure TDBConnSelectForm.DelConSBtnClick(Sender: TObject);
begin
  DelConnBtnClick(self);
end;

end.
