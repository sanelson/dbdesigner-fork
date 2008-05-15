unit EERStoreInDatabase;

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
// Unit EERStoreInDatabase
// -----------------------
// Version 1.1, 17.03.2003, Mike
// Description
//   Contains the form for saving the model in a database
//
// Changes:
//   Version 1.1, 17.03.2003, Mike
//     changed SubmitBtnClick to update the model's VersionStr
//     before saving
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons, QExtCtrls, EERModel, SqlExpr, QImgList, QComCtrls,
  DB, QMenus, QTypes, DBDM;

type
  TEERStoreInDatabaseForm = class(TForm)
    StatusPnl: TPanel;
    ConnectionSBtn: TSpeedButton;
    StatusLbl: TLabel;
    Panel1: TPanel;
    SubmitBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    Bevel1: TBevel;
    Panel2: TPanel;
    Label1: TLabel;
    DBConnEd: TEdit;
    GetDBConnSBtn: TSpeedButton;
    SavePnl: TPanel;
    Label2: TLabel;
    ModelNameEd: TEdit;
    VersionEd: TEdit;
    AutoIncVersionCBox: TCheckBox;
    VersionLbl: TLabel;
    UseVersionCBox: TCheckBox;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    SavedModelsTV: TTreeView;
    ImageList: TImageList;
    PopupMenu: TPopupMenu;
    DeleteModelfromDatabaseMI: TMenuItem;
    DeleteVersionfromDatabaseMI: TMenuItem;
    N1: TMenuItem;
    InfoPnl: TPanel;
    Label3: TLabel;
    InfoMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

    function SetData(theModel: TEERModel; DoSave: Boolean; SelectDBConn: TDBConn = nil): Boolean;
    procedure GetDBConnSBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
    procedure UseVersionCBoxClick(Sender: TObject);
    procedure AutoIncVersionCBoxClick(Sender: TObject);
    procedure ShowModelsInDB;
    procedure SavedModelsTVDblClick(Sender: TObject);
    procedure DeleteModelfromDatabaseMIClick(Sender: TObject);
    procedure DeleteVersionfromDatabaseMIClick(Sender: TObject);
    procedure SavedModelsTVChange(Sender: TObject; Node: TTreeNode);
  private
    { Private-Deklarationen }
    EERModel: TEERModel;

    CreateTheStorageTable: Boolean;
    DialogMode: integer;

    SelectDBConn: TDBConn;
  public
    { Public-Deklarationen }
  end;

var
  EERStoreInDatabaseForm: TEERStoreInDatabaseForm;

implementation

uses MainDM;

{$R *.xfm}

procedure TEERStoreInDatabaseForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);

  SelectDBConn:=nil;

  SavedModelsTV.Items.Clear;

  CreateTheStorageTable:=False;
end;

procedure TEERStoreInDatabaseForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TEERStoreInDatabaseForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TEERStoreInDatabaseForm.FormShow(Sender: TObject);
begin
  Left:=(Screen.Width-Width) div 2;
  Top:=(Screen.Height-Height) div 2;
end;

function TEERStoreInDatabaseForm.SetData(theModel: TEERModel; DoSave: Boolean; SelectDBConn: TDBConn): Boolean;
begin
  EERModel:=theModel;

  self.SelectDBConn:=SelectDBConn;

  ModelNameEd.Text:=Copy(ExtractFileName(EERModel.ModelFilename), 1, Length(ExtractFileName(EERModel.ModelFilename))-Length(ExtractFileExt(EERModel.ModelFilename)));
  UseVersionCBox.Checked:=EERModel.UseVersionHistroy;
  AutoIncVersionCBox.Checked:=EERModel.AutoIncVersion;
  UseVersionCBoxClick(self);
  AutoIncVersionCBoxClick(self);

  //On Load
  if(Not(DoSave))then
  begin
    Caption:=DMMain.GetTranslatedMessage('Load from Database', 215);
    SubmitBtn.Caption:=DMMain.GetTranslatedMessage('Load Model', 216);
    SavePnl.Hide;
    InfoMemo.ReadOnly:=True;
    DialogMode:=1;
  end
  else
    DialogMode:=0;

  GetDBConnSBtnClick(self);

  //When not connected, close
  SetData:=(DMDB.CurrentDBConn<>nil);
end;

procedure TEERStoreInDatabaseForm.GetDBConnSBtnClick(Sender: TObject);
var SelDBConn: TDBConn;
  theTables: TStringList;
begin
  DBConnEd.Text:='';
  if(Sender.ClassNameIs('TSpeedButton'))then
    DMDB.DisconnectFromDB;

  StatusLbl.Caption:=DMMain.GetTranslatedMessage('Not connected to Database', 27);
  ConnectionSBtn.Enabled:=False;
  SubmitBtn.Enabled:=False;

  //do until a successful connection is established or the user selects abort
  while(1=1)do
  begin
    if(SelectDBConn<>nil)and(SelectDBConn<>DMDB.CurrentDBConn)then
      SelDBConn:=DMDB.GetUserSelectedDBConn(SelectDBConn.Name)
    //Let the User choose connection if there is no open connection
    else if(DMDB.CurrentDBConn=nil)or(EERModel.DefSaveDBConn<>DMDB.CurrentDBConn.Name)then
      SelDBConn:=DMDB.GetUserSelectedDBConn(EERModel.DefSaveDBConn)
    else
      SelDBConn:=DMDB.CurrentDBConn;

    if(SelDBConn<>nil)then
    begin
      //Try to connect to the DB
      try
        DMDB.ConnectToDB(SelDBConn);
      except
        on x: Exception do
        begin
          MessageDlg(DMMain.GetTranslatedMessage('Connection to database failed.'+#13#10#13#10+'%s',
            121, x.Message), mtError, [mbOK], 0);

          continue;
        end;
      end;

      ConnectionSBtn.Enabled:=True;
      SubmitBtn.Enabled:=True;
      DBConnEd.Text:=DMDB.CurrentDBConn.Name;
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Connected to Database %s)', 28,
        DMDB.CurrentDBConn.Params.Values['User_Name']+'@'+
        DMDB.CurrentDBConn.Params.Values['Database']);

      theTables:=TStringList.Create;
      try
        DMDB.GetDBTables(theTables);

        //When there is no DBDesigner4 table, create one when saving
        if(theTables.IndexOf('DBDesigner4')=-1)then
          CreateTheStorageTable:=True;

        if(Not(CreateTheStorageTable))then
          ShowModelsInDB;


      finally
        theTables.Free;
      end;

      break;
    end
    else
      break;
  end;
end;

procedure TEERStoreInDatabaseForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult:=mrAbort;
end;

procedure TEERStoreInDatabaseForm.SubmitBtnClick(Sender: TObject);
var MakeUpdate: Boolean;
  theFile: Textfile;
  theModel, s: string;
  oldidmodel, oldidversion: integer;
begin
  if(DialogMode=0)then
  begin
    //Set Model's new Version AFTER data has been saved
    oldidmodel:=EERModel.IDModel;
    oldidversion:=EERModel.IDVersion;

    try
      MakeUpdate:=True;

      DMDB.SchemaSQLQuery.ParamCheck:=False;
      DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');

      if(CreateTheStorageTable)then
      begin
        DMDB.SchemaSQLQuery.SQL.Text:='CREATE TABLE dbdesigner4('+
          'idmodel INTEGER UNSIGNED NOT NULL, '+
          'idversion INTEGER UNSIGNED NOT NULL, '+
          'name VARCHAR(45), '+
          'version VARCHAR(20), '+
          'username VARCHAR(45), '+
          'createdate DATETIME, '+
          'iscurrent INTEGER(1) UNSIGNED, '+
          'ischeckedout INTEGER(1) UNSIGNED, '+
          'info varchar(255), '+
          'model MEDIUMTEXT, '+
          'PRIMARY KEY(idmodel, idversion))';
        DMDB.SchemaSQLQuery.ExecSQL(True);
      end;


      //Check if there is a different model with the same id
      if(EERModel.IDModel<>0)then
      begin
        //Check if model already exists in DB
        DMDB.SchemaSQLQuery.SQL.Text:='SELECT name, idversion, version '+
          'from dbdesigner4 where idmodel='+IntToStr(EERModel.IDModel)+' '+
          'order by version desc';
        DMDB.SchemaSQLQuery.Open;
        if(not(DMDB.SchemaSQLQuery.EOF))then
        begin
          if(DMDB.SchemaSQLQuery.Fields[0].AsString<>ModelNameEd.Text)then
          begin
            if(MessageDlg(DMMain.GetTranslatedMessage('There may be a different model stored with the '+
              'same identifier in this database. '+#13#10+
              'Do you want to create a new identifier for this model?'+#13#10#13#10+
              'Name of the model in database: %s', 217,
              DMDB.SchemaSQLQuery.Fields[1].AsString),
              mtConfirmation, [mbYes, mbNo], 0)<>mrYes)then
            begin
              EERModel.IDModel:=0;
            end;
          end;

          if(DMDB.SchemaSQLQuery.Fields[1].AsInteger>EERModel.IDVersion)then
          begin
            if(MessageDlg(DMMain.GetTranslatedMessage('There may be a newer version of the model '+
              'in this database. '+#13#10+
              'Do you want to make this version the current version?'+#13#10#13#10+
              'Version of the model in database: %s'+#13#10+
              'This Version: %s', 218, DMDB.SchemaSQLQuery.Fields[2].AsString, VersionEd.Text),
              mtConfirmation, [mbYes, mbNo], 0)<>mrYes)then
            begin
              VersionEd.Text:=DMDB.SchemaSQLQuery.Fields[2].AsString;
              AutoIncVersionCBoxClick(self);
            end;

            EERModel.IDVersion:=DMDB.SchemaSQLQuery.Fields[1].AsInteger+1;
          end;
        end;
        DMDB.SchemaSQLQuery.Close;
      end;

      //Get new ID
      if(EERModel.IDModel=0)then
      begin
        DMDB.SchemaSQLQuery.SQL.Text:='SELECT max(idmodel)+1 as oldid FROM dbdesigner4';
        DMDB.SchemaSQLQuery.Open;
        if(DMDB.SchemaSQLQuery.EOF)then
          EERModel.IDModel:=1
        else
        begin
          if(DMDB.SchemaSQLQuery.Fields[0].AsString<>'')and
            (not(DMDB.SchemaSQLQuery.Fields[0].IsNull))then
            EERModel.IDModel:=DMDB.SchemaSQLQuery.Fields[0].AsInteger
          else
            EERModel.IDModel:=1;
        end;
        DMDB.SchemaSQLQuery.Close;

        EERModel.IDVersion:=1;

        MakeUpdate:=False;
      end;

      if(UseVersionCBox.Checked)then
      begin
        if(VersionEd.Text<>EERModel.VersionStr)then
        begin
          inc(EERModel.IDVersion);
          MakeUpdate:=False;
        end;
      end;

      //Set VersionStr before saving
      EERModel.VersionStr:=VersionEd.Text;

      //Get Model
      EERModel.SaveToFile(DMMain.SettingsPath+'temp.xml',
        True, False, False);

      AssignFile(theFile, DMMain.SettingsPath+'temp.xml');
      Reset(theFile);
      try
        theModel:='';
        while(Not(EOF(theFile)))do
        begin
          ReadLn(theFile, s);
          theModel:=theModel+s+#13#10;
        end;
      finally
        CloseFile(theFile);
        DeleteFile(DMMain.SettingsPath+'temp.xml');
      end;

      StatusLbl.Caption:=DMMain.GetTranslatedMessage(
        'Storing model in the database...', 237);
      StatusLbl.Refresh;
      Application.ProcessMessages;

      if(Not(MakeUpdate))then
      begin
        DMDB.SchemaSQLQuery.SQL.Text:='UPDATE dbdesigner4 SET '+
          'iscurrent=0 WHERE idmodel='+IntToStr(EERModel.IDModel);
        DMDB.SchemaSQLQuery.ExecSQL(True);

        theModel:=DMMain.ReplaceString(theModel, '''', '''''');
        theModel:=DMMain.ReplaceString(theModel, '\', '\\');

        DMDB.SchemaSQLQuery.SQL.Text:='INSERT INTO dbdesigner4('+
          'idmodel, idversion, name, version, username, createdate, '+
          'model, info, iscurrent, ischeckedout) '+
          'VALUES('+IntToStr(EERModel.IDModel)+', '+
          IntToStr(EERModel.IDVersion)+', '+
          DMMain.FormatText4SQL(ModelNameEd.Text)+', '+
          DMMain.FormatText4SQL(VersionEd.Text)+', '+
          DMMain.FormatText4SQL(DMDB.CurrentDBConn.Params.Values['User_Name'])+', '+
          ''''+FormatDateTime('YYYY-MM-DD HH:NN:SS', Now)+''', '+
          ''''+theModel+''', '+
          DMMain.FormatText4SQL(InfoMemo.Text)+', '+
          '1, 0)';
      end
      else
        DMDB.SchemaSQLQuery.SQL.Text:='UPDATE dbdesigner4 set '+
          'name='+DMMain.FormatText4SQL(ModelNameEd.Text)+', '+
          'version='+DMMain.FormatText4SQL(VersionEd.Text)+', '+
          'username='+DMDB.CurrentDBConn.Params.Values['User_Name']+', '+
          'model='''+theModel+''', '+
          'info='+DMMain.FormatText4SQL(InfoMemo.Text)+', '+
          'createdate='''+FormatDateTime('YYYY-MM-DD HH:NN:SS', Now)+''', '+
          'iscurrent=1, ischeckedout=0 '+
          'WHERE idmodel='+IntToStr(EERModel.IDModel)+' and idversion='+IntToStr(EERModel.IDVersion);

      DMDB.SchemaSQLQuery.ExecSQL(True);

      EERModel.ModelFilename:=ExtractFilePath(EERModel.ModelFilename)+ModelNameEd.Text+ExtractFileExt(EERModel.ModelFilename);

      EERModel.UseVersionHistroy:=UseVersionCBox.Checked;
      EERModel.AutoIncVersion:=AutoIncVersionCBox.Checked;
      EERModel.DefSaveDBConn:=DMDB.CurrentDBConn.Name;

      EERModel.ModelHasChanged;

      StatusLbl.Caption:=DMMain.GetTranslatedMessage(
        'The model was successfully stored in the database.', 238);
      StatusLbl.Refresh;
    except
      EERModel.IDModel:=oldidmodel;
      EERModel.IDVersion:=oldidversion;

      raise;
    end;

    ModalResult:=mrOK;
  end
  else if(DialogMode=1)then
  begin
    if(SavedModelsTV.Selected<>nil)then
    begin
      DMDB.SchemaSQLQuery.ParamCheck:=False;
      DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');

      DMDB.SchemaSQLQuery.SQL.Text:=
        'SELECT model, name FROM dbdesigner4 WHERE '+
        'idmodel='+
        SavedModelsTV.Selected.SubItems[3]+' and '+
        'idversion='+
        SavedModelsTV.Selected.SubItems[4];
      DMDB.SchemaSQLQuery.Open;
      s:=DMDB.SchemaSQLQuery.Fields[1].AsString;
      TMemoField(DMDB.SchemaSQLQuery.Fields[0]).SaveToFile(DMMain.SettingsPath+s+'.xml');

      DMDB.SchemaSQLQuery.Close;

      EERModel.LoadFromFile(DMMain.SettingsPath+s+'.xml');

      DeleteFile(DMMain.SettingsPath+s+'.xml');

      ModalResult:=mrOK;
    end;
  end;
end;

procedure TEERStoreInDatabaseForm.UseVersionCBoxClick(Sender: TObject);
begin
  VersionLbl.Enabled:=UseVersionCBox.Checked;
  VersionEd.Enabled:=UseVersionCBox.Checked;
  AutoIncVersionCBox.Enabled:=UseVersionCBox.Checked;
end;

procedure TEERStoreInDatabaseForm.AutoIncVersionCBoxClick(Sender: TObject);
var s, s1: string;
begin
  if(Not(AutoIncVersionCBox.Checked))or
    (Not(UseVersionCBox.Checked))or
    (EERModel.IDModel=0)then
    VersionEd.Text:=EERModel.VersionStr
  else
  begin
    s:=EERModel.VersionStr;
    while(Pos('.', s)>0)do
      s:=Copy(s, Pos('.', s)+1, Length(s));

    s1:=Copy(EERModel.VersionStr, 1, Length(EERModel.VersionStr)-Length(s));

    try
      if(s<>'')then
        VersionEd.Text:=s1+IntToStr(StrToIntDef(s, 0)+1)
      else
        VersionEd.Text:=EERModel.VersionStr;
    except
      VersionEd.Text:=EERModel.VersionStr;
    end;
  end;

end;

procedure TEERStoreInDatabaseForm.ShowModelsInDB;
var theTreeNode, theModelNode: TTreeNode;
  idmodel: integer;
begin
  //List Model in DB
  SavedModelsTV.Items.Clear;

  theModelNode:=nil;
  idmodel:=-1;


  DMDB.SchemaSQLQuery.ParamCheck:=False;
  DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');

  DMDB.SchemaSQLQuery.SQL.Text:=
    'SELECT idmodel, idversion, name, version, username, '+
    ' createdate, info FROM dbdesigner4 '+
    'ORDER BY idmodel, iscurrent desc, idversion desc';
  DMDB.SchemaSQLQuery.Open;
  while(Not(DMDB.SchemaSQLQuery.EOF))do
  begin
    if(idmodel<>DMDB.SchemaSQLQuery.FieldByName('idmodel').AsInteger)then
    begin
      idmodel:=DMDB.SchemaSQLQuery.FieldByName('idmodel').AsInteger;

      theTreeNode:=SavedModelsTV.Items.Add(nil,
        DMDB.SchemaSQLQuery.FieldByName('name').AsString);
      theModelNode:=theTreeNode;
    end
    else
      theTreeNode:=SavedModelsTV.Items.AddChild(theModelNode,
        DMDB.SchemaSQLQuery.FieldByName('name').AsString);


    theTreeNode.ImageIndex:=0;
    theTreeNode.SelectedIndex:=0;
    theTreeNode.SubItems.Add(DMDB.SchemaSQLQuery.FieldByName('version').AsString);
    theTreeNode.SubItems.Add(DMDB.SchemaSQLQuery.FieldByName('username').AsString);
    theTreeNode.SubItems.Add(FormatDateTime('yyyy-mm-dd hh:nn', DMDB.SchemaSQLQuery.FieldByName('createdate').AsDateTime));
    theTreeNode.SubItems.Add(DMDB.SchemaSQLQuery.FieldByName('idmodel').AsString);
    theTreeNode.SubItems.Add(DMDB.SchemaSQLQuery.FieldByName('idversion').AsString);
    theTreeNode.SubItems.Add(DMDB.SchemaSQLQuery.FieldByName('info').AsString);

    DMDB.SchemaSQLQuery.Next;
  end;
  DMDB.SchemaSQLQuery.Close;

  if(SavedModelsTV.Items.Count>0)then
    SavedModelsTV.Selected:=SavedModelsTV.Items[0];
end;

procedure TEERStoreInDatabaseForm.SavedModelsTVDblClick(Sender: TObject);
begin
  if(DialogMode=1)then
    SubmitBtnClick(self);
end;

procedure TEERStoreInDatabaseForm.DeleteModelfromDatabaseMIClick(
  Sender: TObject);
begin
  if(SavedModelsTV.Selected<>nil)then
    if(MessageDlg(DMMain.GetTranslatedMessage('Are you sure you want to delete the model '+
      'from database?'+#13#10+'All Versions will be deleted.', 219),
      mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
    begin
      DMDB.SchemaSQLQuery.ParamCheck:=False;
      DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');

      DMDB.SchemaSQLQuery.SQL.Text:='DELETE FROM dbdesigner4 '+
        'WHERE idmodel='+SavedModelsTV.Selected.SubItems[3];
      DMDB.SchemaSQLQuery.ExecSQL(True);

      ShowModelsInDB;
    end;
end;

procedure TEERStoreInDatabaseForm.DeleteVersionfromDatabaseMIClick(
  Sender: TObject);
begin
  if(SavedModelsTV.Selected<>nil)then
    if(MessageDlg(DMMain.GetTranslatedMessage('Are you sure you want to delete this version '+
      'of the model from database?', 220),
      mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
    begin
      DMDB.SchemaSQLQuery.ParamCheck:=False;
      DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');

      DMDB.SchemaSQLQuery.SQL.Text:='DELETE FROM dbdesigner4 '+
        'WHERE idmodel='+SavedModelsTV.Selected.SubItems[3]+' and '+
        'idversion='+SavedModelsTV.Selected.SubItems[4];
      DMDB.SchemaSQLQuery.ExecSQL(True);

      ShowModelsInDB;
    end;
end;

procedure TEERStoreInDatabaseForm.SavedModelsTVChange(Sender: TObject;
  Node: TTreeNode);
var theStrings: TStringList;
  i: integer;
begin
  if(DialogMode=1)then
    if(SavedModelsTV.Selected<>nil)then
    begin
      theStrings:=TStringList.Create;
      try
        theStrings.Text:=SavedModelsTV.Selected.SubItems.Text;
        i:=0;
        while(i<5)and(theStrings.Count>0)do
        begin
          theStrings.Delete(0);
          inc(i);
        end;
        InfoMemo.Text:=theStrings.Text;
      finally
        theStrings.Free;
      end;
    end;
end;

end.
