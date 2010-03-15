unit DBDM;

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
// Unit DBDM.pas
// -------------
// Version 1.0, 13.01.2003, Mike
// Description
//   Contains general functions which are used to work with databases
//
// Changes:
//   Version Fork 1.5, 13.10.2010, JP: Better error message for MySQL.
//   Version Fork 1.5, 13.10.2010, JP: Better integration with ODBC.   (MySQLReverseEngineer for ODBC now works)
//   Version 1.0, 13.01.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, QTypes, DBXpress, FMTBcd, DBClient, Provider, SqlExpr,
  DB, IniFiles, QForms, QControls, Qt, QDialogs, Contnrs;

type
  // Class storing a Database-Connection
  TDBConn = class(TPersistent)
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  public
    Name,                     // Name of the Datebase - Connection
    Description,              // Description of the DBConn
    DriverName,               // Parameters ...
    GetDriverFunc,
    LibraryName,
    VendorLib: string;
    TableScope: TTableScopes;
    Params: TStringList;
  end;

  // Class to store a Database-Hosts
  TDBHost = class(TPersistent)
    constructor Create;
    destructor Destroy;  override;
  public
    Caption,                  // Caption of the Datebase - Host
    HostName,                 // Name or IP of the Host
    User_Name: string;        // Default User name for connection
  end;

  TDMDB = class(TDataModule)
    SQLConn: TSQLConnection;
    OutputDataSrc: TDataSource;
    OutputQry: TSQLQuery;
    OutputDataSetProvider: TDataSetProvider;
    OutputClientDataSet: TClientDataSet;
    SchemaSQLQuery: TSQLQuery;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

    //Get all DatabaseConnections from the ini-File
    procedure GetDBConns;
    procedure ReadDBConnFromIniFile(theIni: TMemIniFile; dbconnName: string; theDBConn: TDBConn);
    procedure StoreDBConns;
    //Connects to a DBConn
    function ConnectToDB(DBConn: Pointer; theSQLConn: TSQLConnection = nil): Boolean;
    //Disconnect from DB
    procedure DisconnectFromDB;
    //Get a new DBConn and initialize with default values
    function GetNewDBConn(name: string; AddToDBConns: Boolean = True; DatabaseType: string = ''): TDBConn;

    //Let the User select a DBConnection
    function GetUserSelectedDBConn(defDBConn: string): TDBConn;

    //General OnButtonClick für DBConn Buttons
    procedure GetDBConnButtonClick(Sender: TObject; defDBConn: string = '');

    //List all DB-Tables in a Stringlist
    function GetDBTables(var tablelist: TStringList; theSQLConn: TSQLConnection = nil; theDBConn: TDBConn = nil): Boolean;

    //Execute a SQL Command
    procedure ExecSQL(s: string);

    procedure LoadSettingsFromIniFile;
    procedure SaveSettingsToIniFile;

    //Returns the Records of a Query as INSERTs
    function GetRecordsAsInserts(SQLCmd: string; theQry: TClientDataSet; limit: integer = 0): string;
    function GetRecordsAsInsertsTSQLQuery(SQLCmd: string; theQry: TSQLQuery; limit: integer = 0): string;

    //Returns first command from SQL-Script and removes this command from the list
    function GetFirstSQLCmdFromScript(var cmds: String): String;

    function ExecuteSQLCmdScript(cmds: String): integer;
  private
    { Private declarations }
    RunFirstTime: Boolean;
  public
    { Public declarations }
    DatabaseTypes: TStringList;
    DefaultDatabaseType: string;

    DBConnections: TObjectList;
    CurrentDBConn: TDBConn;
  end;

// JP: better error messages when connecting
function GetConnectErrorMessage(DriverName: string):string;

const
  QEventType_SetQueryStatusLbl = QEventType(Integer(QEventType_ClxUser) + 200);
  QEventType_CloseAllClientDatasets = QEventType(Integer(QEventType_ClxUser) + 201);

var
  DMDB: TDMDB;

implementation

{$R *.xfm}

uses DBConnSelect, MainDM;

procedure TDMDB.DataModuleCreate(Sender: TObject);
begin
  RunFirstTime:=False;

  //Create StringLists
  DatabaseTypes:=TStringList.Create;

  CurrentDBConn:=nil;
  DBConnections:=TObjectList.Create;

  LoadSettingsFromIniFile;

  if(Not(RunFirstTime))or(FileExists(DMMain.SettingsPath+'DBConn.ini'))then
    GetDBConns;
end;

procedure TDMDB.DataModuleDestroy(Sender: TObject);
begin
  SaveSettingsToIniFile;

  //Free Stringlists
  DatabaseTypes.Free;

  //Store current DBConns
  StoreDBConns;

  //Free DBConns
  DBConnections.Free;
end;

procedure TDMDB.GetDBConns;
var theIni: TMemIniFile;
  IniFileDBConns: TStringList;
  i: integer;
  dbconnName: string;
  theDBConn: TDBConn;
begin
  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBConn.ini');
  try
    IniFileDBConns:=TStringList.Create;
    try

      theIni.ReadSectionValues('DBConn', IniFileDBConns);

      //Make a Node for each DBConn
      for i:=0 to IniFileDBConns.Count-1 do
      begin
        dbconnName:=IniFileDBConns.Values['DBConnName'+IntToStr(i+1)];

        if(dbconnName<>'')then
        begin
          theDBConn:=TDBConn.Create;
          try
            theDBConn.Name:=dbconnName;

            ReadDBConnFromIniFile(theIni, dbconnName, theDBConn);

            DBConnections.Add(theDBConn);
          except
            theDBConn.Free;
          end;
        end;
      end;
    finally
      IniFileDBConns.Free;
    end;
  finally
    theIni.Free;
  end;
end;

procedure TDMDB.ReadDBConnFromIniFile(theIni: TMemIniFile; dbconnName: string; theDBConn: TDBConn);
var theDBConnParams: TStringList;
  ospostfix: string;
begin
  theDBConnParams:=TStringList.Create;
  try
    theIni.ReadSectionValues(dbconnName, theDBConnParams);


    {$IFDEF LINUX}
    ospostfix:='Linux';
    {$ELSE}
    ospostfix:='';
    {$ENDIF}

    with(theDBConnParams)do
    begin
      theDBConn.Name:=dbconnName;
      theDBConn.Description:=Values['Description'];
      theDBConn.DriverName:=Values['DriverName'];
      theDBConn.GetDriverFunc:=Values['GetDriverFunc'];

      theDBConn.LibraryName:=Values['LibraryName'+ospostfix];

      theDBConn.VendorLib:=Values['VendorLib'+ospostfix];
      if(Values['TableScope']<>'')then
      begin
        theDBConn.TableScope:=[];
        if(Pos('tsTable', Values['TableScope'])<>0)then
          theDBConn.TableScope:=theDBConn.TableScope + [tsTable];
        if(Pos('tsView', Values['TableScope'])<>0)then
          theDBConn.TableScope:=theDBConn.TableScope + [tsView];
        if(Pos('tsSysTable', Values['TableScope'])<>0)then
          theDBConn.TableScope:=theDBConn.TableScope + [tsSysTable];
        if(Pos('tsSynonym', Values['TableScope'])<>0)then
          theDBConn.TableScope:=theDBConn.TableScope + [tsSynonym];
      end
      else
        theDBConn.TableScope:=[tsTable, tsView];

      //Delete Native Params
      if(IndexOfName('Description')<>-1)then
        Delete(IndexOfName('Description'));
      if(IndexOfName('DriverName')<>-1)then
        Delete(IndexOfName('DriverName'));
      if(IndexOfName('GetDriverFunc')<>-1)then
        Delete(IndexOfName('GetDriverFunc'));
      if(IndexOfName('LibraryName')<>-1)then
        Delete(IndexOfName('LibraryName'));
      if(IndexOfName('LibraryName'+ospostfix)<>-1)then
        Delete(IndexOfName('LibraryName'+ospostfix));
      if(IndexOfName('VendorLib')<>-1)then
        Delete(IndexOfName('VendorLib'));
      if(IndexOfName('VendorLib'+ospostfix)<>-1)then
        Delete(IndexOfName('VendorLib'+ospostfix));
      if(IndexOfName('TableScope')<>-1)then
        Delete(IndexOfName('TableScope'));
    end;

    theDBConn.Params.Assign(theDBConnParams);
  finally
    theDBConnParams.Free;
  end;
end;

procedure TDMDB.StoreDBConns;
var theIni: TMemIniFile;
  i, j: integer;
  dbconnName, s, ospostfix: string;
  theDBConn: TDBConn;
begin
  {$IFDEF LINUX}
  ospostfix:='Linux';
  {$ELSE}
  ospostfix:='';
  {$ENDIF}

  //delete old file
  DeleteFile(DMMain.SettingsPath+'DBConn.ini');

  //Save IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBConn.ini');
  try
    //Make a Node for each DBConn
    for i:=0 to DBConnections.Count-1 do
    begin
      theDBConn:=TDBConn(DBConnections[i]);
      dbconnName:=theDBConn.Name;
      theIni.WriteString('DBConn', 'DBConnName'+IntToStr(i+1), dbconnName);

      if(dbconnName<>'')then
      begin
        theIni.WriteString(dbconnName, 'Description', theDBConn.Description);
        theIni.WriteString(dbconnName, 'DriverName', theDBConn.DriverName);
        theIni.WriteString(dbconnName, 'GetDriverFunc', theDBConn.GetDriverFunc);
        theIni.WriteString(dbconnName, 'LibraryName'+ospostfix, theDBConn.LibraryName);
        theIni.WriteString(dbconnName, 'VendorLib'+ospostfix, theDBConn.VendorLib);

        s:='';
        if(theDBConn.TableScope<>[])then
        begin
          s:='[';
          if(tsTable in theDBConn.TableScope)then
            s:=s + 'tsTable ,';
          if(tsView in theDBConn.TableScope)then
            s:=s + 'tsView ,';
          if(tsSysTable in theDBConn.TableScope)then
            s:=s + 'tsSysTable ,';
          if(tsSynonym in theDBConn.TableScope)then
            s:=s + 'tsSynonym ,';

          s:=Copy(s, 1, length(s)-2)+']';
        end
        else
          s:='[tsTable, tsView]';

        theIni.WriteString(dbconnName, 'TableScope', s);

        for j:=0 to theDBConn.Params.Count-1 do
          if(theDBConn.Params.Names[j]<>'Password')then
            theIni.WriteString(dbconnName, theDBConn.Params.Names[j], theDBConn.Params.Values[theDBConn.Params.Names[j]]);

      end;
    end;

    theIni.UpdateFile;
  finally
    theIni.Free;
  end;
end;

function TDMDB.ConnectToDB(DBConn: Pointer; theSQLConn: TSQLConnection = nil): Boolean;
begin
  Result:=False;

  if(CurrentDBConn<>nil)then
    DisconnectFromDB;

  if(theSQLConn=nil)then
    theSQLConn:=SQLConn;

  //Assign the params
  with(theSQLConn)do
  begin
    DriverName:=TDBConn(DBConn).DriverName;
    GetDriverFunc:=TDBConn(DBConn).GetDriverFunc;
    LibraryName:=TDBConn(DBConn).LibraryName;
    VendorLib:=TDBConn(DBConn).VendorLib;
    TableScope:=TDBConn(DBConn).TableScope;
    Params.Assign(TDBConn(DBConn).Params);

    //Open DB
    try
      Open;

      if(Connected)then
        Result:=True;
    except
      on x: Exception do
      begin
        if(theSQLConn=SQLConn)then
          CurrentDBConn:=nil;

        raise;
      end;
    end;

    //If given SQLConnection is the Std. DBConn
    if(theSQLConn=SQLConn)then
    begin
      if(SQLConn.Connected)then
      begin
        CurrentDBConn:=DBConn;

        QApplication_postEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_SetQueryStatusLbl, nil));
      end
      else
        CurrentDBConn:=nil;
    end;
  end;
end;

procedure TDMDB.DisconnectFromDB;
begin
  if(SQLConn.Connected)then
    SQLConn.Close;

  if(OutputClientDataSet.Active)then
    OutputClientDataSet.Close;

  CurrentDBConn:=nil;

  QApplication_postEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_SetQueryStatusLbl, nil));
  QApplication_postEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_CloseAllClientDatasets, nil));
end;

function TDMDB.GetNewDBConn(name: string; AddToDBConns: Boolean = True; DatabaseType: string = ''): TDBConn;
var theDBConn: TDBConn;
  theIni: TMemIniFile;
begin
  theDBConn:=TDBConn.Create;

  //Copy settings from defaults
  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBConn_DefaultSettings.ini');
  try
    if(DatabaseType='')then
      DatabaseType:=DMDB.DefaultDatabaseType;

    DMDB.ReadDBConnFromIniFile(theIni, DatabaseType, theDBConn);

    theDBConn.Name:=name;

    if(AddToDBConns)then
      DBConnections.Add(theDBConn);
  finally
    theIni.Free;
  end;

  GetNewDBConn:=theDBConn;
end;

function TDMDB.GetUserSelectedDBConn(defDBConn: string): TDBConn;
var i, s: integer;
begin
  GetUserSelectedDBConn:=nil;

  s:=-1;
  for i:=0 to DBConnections.Count-1 do
    if(TDBConn(DBConnections[i]).name=defDBConn)then
    begin
      s:=i;
      break;
    end;

  DBConnSelectForm:=TDBConnSelectForm.Create(self);
  try
    if(s>-1)then
      DBConnSelectForm.SetData(DBConnections, TDBConn(DBConnections[s]))
    else
      DBConnSelectForm.SetData(DBConnections, nil);
    if(DBConnSelectForm.ShowModal=mrOK)then
    begin
      GetUserSelectedDBConn:=DBConnSelectForm.SelDBConn;
    end;
  finally
    DBConnSelectForm.Free;
  end;
end;

// JP: better error messages when connecting
function GetConnectErrorMessage(DriverName: string):string;
var
  ErrMsg: string;
begin

  ErrMsg := 'Connection to database '+DriverName+' has failed.'+#13#10;

  if DriverName = 'MySQL' then
  begin
    ErrMsg := ErrMsg + 'Possible causes are:'+#13#10;
    ErrMsg := ErrMsg + '* User has no grants to connect from this machine.'+#13#10;
    ErrMsg := ErrMsg + '* DB Designer Fork does not connect to MySQL 5.* with password.'+#13#10;
    ErrMsg := ErrMsg + '  You may try connecting with a user that does not require password.'+#13#10;
    ErrMsg := ErrMsg + '  You may try connecting thru ODBC.'+#13#10;
    ErrMsg := ErrMsg + '  When reverse engineering, MySQL specific functions are recommended.'+#13#10;
  end else
  if DriverName = 'SQLite' then
  begin
    ErrMsg := ErrMsg + 'Possible causes are:'+#13#10;
    ErrMsg := ErrMsg + '* The database file is corrupted.'+#13#10;
    ErrMsg := ErrMsg + '* DB Designer Fork does not connect to SQLite 3.* directly.'+#13#10;
    ErrMsg := ErrMsg + '  You may try connecting thru ODBC.'+#13#10;
    ErrMsg := ErrMsg + '  DB Designer Fork is able to connect thru ODBC to SQLite 3.*, MySQL 5.* and Firebird 2.*.'+#13#10;
    ErrMsg := ErrMsg + '  When reverse engineering, ODBC functions are recommended.'+#13#10;
  end;

  ErrMsg := ErrMsg + #13#10;

  result := ErrMsg;

end;

procedure TDMDB.GetDBConnButtonClick(Sender: TObject; defDBConn: string = '');
var
  SelDBConn: TDBConn;
  ErrMsg,DriverName: string;

begin
  if(Sender.ClassNameIs('TSpeedButton'))then
    DMDB.DisconnectFromDB;

  //do until a successful connection is established or the user selects abort
  while(1=1)do
  begin
    //Let the User choose connection if there is no open connection
    if(DMDB.CurrentDBConn=nil)then
      SelDBConn:=DMDB.GetUserSelectedDBConn(defDBConn)
    else
      SelDBConn:=DMDB.CurrentDBConn;

    if(SelDBConn<>nil)then
    begin
      //Try to connect to the DB
      try
        DriverName := SelDBConn.DriverName;
        DMDB.ConnectToDB(SelDBConn);
      except
        on x: Exception do
        begin

          ErrMsg := GetConnectErrorMessage(DriverName);

          MessageDlg(ErrMsg+DMMain.GetTranslatedMessage('%s', 121,
            x.Message), mtError, [mbOK], 0);

          continue;
        end;
      end;

      break;
    end
    else
      break;
  end;
end;


function TDMDB.GetDBTables(var tablelist: TStringList; theSQLConn: TSQLConnection = nil; theDBConn: TDBConn = nil): Boolean;
var
  TableName: string;
  ResultCnt: integer;
begin
  if(SchemaSQLQuery.Active)then
    SchemaSQLQuery.Close;

  tablelist.Clear;

  if(theSQLConn<>nil)then
    SchemaSQLQuery.SQLConnection:=theSQLConn;

  if(theDBConn=nil)then
    theDBConn:=CurrentDBConn;

  try
    if(CompareText(theDBConn.DriverName, 'Oracle')=0)then
    begin
      SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');
      SchemaSQLQuery.SQL.Text:='SELECT * FROM ALL_TABLES '+
        'ORDER BY OWNER, TABLE_NAME';
      SchemaSQLQuery.Open;
      try
        while(Not(SchemaSQLQuery.EOF))do
        begin
          tablelist.Add(SchemaSQLQuery.FieldByName('OWNER').AsString+
            '.'+SchemaSQLQuery.FieldByName('TABLE_NAME').AsString);
          SchemaSQLQuery.Next;
        end;
      finally
        SchemaSQLQuery.Close;
      end;

      GetDBTables:=True;
    end
    else if(CompareText(theDBConn.DriverName, 'SQLite')=0)then
    begin
      //Meta Information is stored in the table sqlite_master
      SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');

      SchemaSQLQuery.SQL.Text:='SELECT name FROM sqlite_master '+
        'WHERE type=''table'' '+
        'ORDER BY name';
      SchemaSQLQuery.Open;
      try
        while(Not(SchemaSQLQuery.EOF))do
        begin
          tablelist.Add(SchemaSQLQuery.FieldByName('name').AsString);
          SchemaSQLQuery.Next;
        end;
      finally
        SchemaSQLQuery.Close;
      end;

      GetDBTables:=True;
    end
    else if(CompareText(theDBConn.DriverName, 'MSSQL')=0)then
    begin
      //Meta Information is stored in the table sqlite_master
      SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');

      SchemaSQLQuery.SQL.Text:='sp_tables @table_type="''TABLE''"';
      SchemaSQLQuery.Open;
      try
        while(Not(SchemaSQLQuery.EOF))do
        begin
          tablelist.Add(SchemaSQLQuery.FieldByName('TABLE_QUALIFIER').AsString+'.'+
            SchemaSQLQuery.FieldByName('TABLE_NAME').AsString);
          SchemaSQLQuery.Next;
        end;
      finally
        SchemaSQLQuery.Close;
      end;

      GetDBTables:=True;
    end
    else
    begin
      //GetTables
      //Col 0: RECNO
      //1: CATALOG_NAME
      //2: SCHEMA_NAME
      //3: TABLE_NAME
      //4: TABLE_TYPE
      SchemaSQLQuery.SetSchemaInfo(stTables, '', '');
      SchemaSQLQuery.Open;
      try
        while(Not(SchemaSQLQuery.EOF))do
        begin
          {if(CurrentDBConn.DriverName='openodbc')then
            tablelist.Add(Copy(SchemaSQLQuery.Fields[3].AsString, 2, Length(SchemaSQLQuery.Fields[3].AsString)-2))
          else}
          tablelist.Add(SchemaSQLQuery.Fields[3].AsString);

          SchemaSQLQuery.Next;
        end;
      finally
        SchemaSQLQuery.Close;
      end;

      GetDBTables:=True;
    end;
  finally
    SchemaSQLQuery.SQLConnection:=SQLConn;
  end;

  // remove special chars from output
  if tablelist.Count>0 then
  begin
    for ResultCnt := 0 to tablelist.Count-1 do
    begin
      TableName := tablelist[ResultCnt];
      TableName := SysUtils.StringReplace(TableName,'"','',[SysUtils.rfReplaceAll]);
      TableName := SysUtils.StringReplace(TableName,'`','',[SysUtils.rfReplaceAll]);
      tablelist[ResultCnt] := TableName;
    end;
  end;

end;

procedure TDMDB.ExecSQL(s: string);
begin
  //Because of Delphi BUG!
  if(SQLConn.ActiveStatements<>0)then
  begin
    SQLConn.Close;
    SQLConn.Open;
  end;

  try
    SQLConn.ExecuteDirect(s);
  except
    SQLConn.Close;
    SQLConn.Open;

    try
      SQLConn.ExecuteDirect(s);
    except
      on x: Exception do
      begin
        EDatabaseError.Create(DMMain.GetTranslatedMessage('SQL statement cannot be executed.'+#13#10+'%s', 144,
          x.Message+#13#10+#13#10+s));
      end;
    end;
  end;
end;

procedure TDMDB.LoadSettingsFromIniFile;
var theIni: TMemIniFile;
  i: integer;
begin
  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_Settings.ini');
  try
    //Check if the Program is opened the very first time
    if(theIni.ReadString('GeneralSettings', 'RunFirstTime', '0')='1')then
      RunFirstTime:=True;

    DefaultDatabaseType:=theIni.ReadString('GeneralSettings', 'DefaultDB',
      'MySQL');

    //Read all Database Types
    theIni.ReadSectionValues('DatabaseTypes', DatabaseTypes);
    for i:=0 to DatabaseTypes.Count-1 do
      DatabaseTypes[i]:=Copy(DatabaseTypes[i], Pos('=', DatabaseTypes[i])+1, Length(DatabaseTypes[i]));
  finally
    theIni.Free;
  end;
end;

procedure TDMDB.SaveSettingsToIniFile;
var theIni: TMemIniFile;
begin
  //Open IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_Settings.ini');
  try
    theIni.WriteString('GeneralSettings', 'DefaultDB',
      DefaultDatabaseType);

    theIni.UpdateFile;
  finally
    theIni.Free;
  end;
end;

function TDMDB.GetRecordsAsInserts(SQLCmd: string; theQry: TClientDataSet; limit: integer = 0): string;
var insertHeader, inserts, tablename, s: string;
  i, reccount: integer;
begin
  //Get Table name after FROM
  //SQLCmd:=theQry.SQL.Text;
  s:=Copy(SQLCmd, Pos('FROM ', UpperCase(SQLCmd))+5, Length(SQLCmd));
  if(Pos(#13#10, s)>0)then
    tablename:=Copy(s, 1, Pos(#13#10, s)-1);

  if(Pos(' ', s)>0)then
    tablename:=Copy(s, 1, Pos(' ', s)-1);


  //Build Fieldnames
  s:='INSERT INTO '+tablename+'(';
  for i:=0 to theQry.Fields.Count-1 do
    s:=s+theQry.Fields[i].DisplayName+', ';

  //Remove last ,+Space
  s:=Copy(s, 1, Length(s)-2);

  insertHeader:=s+')'+#13#10+'VALUES(';


  inserts:='';

  theQry.DisableControls;
  try
    theQry.First;
    reccount:=0;
    while(not(theQry.Eof))and((limit=0)or(reccount<limit))do
    begin
      s:='';
      for i:=0 to theQry.Fields.Count-1 do
      begin
        if(theQry.Fields[i].DataType=ftSmallint)or
          (theQry.Fields[i].DataType=ftInteger)or
          (theQry.Fields[i].DataType=ftWord)or
          (theQry.Fields[i].DataType=ftLargeint)or
          (theQry.Fields[i].DataType=ftAutoInc)then
        begin
          if(theQry.Fields[i].AsString<>'')then
            s:=s+theQry.Fields[i].AsString
          else
            s:=s+'0';
        end
        else if(theQry.Fields[i].DataType=ftString)or
          (theQry.Fields[i].DataType=ftFixedChar)or
          (theQry.Fields[i].DataType=ftWideString)then
          s:=s+''''+DMMain.ReplaceText(theQry.Fields[i].AsString, '''', '''''')+''''
        else if(theQry.Fields[i].DataType=ftDateTime)or
          (theQry.Fields[i].DataType=ftTimeStamp)then
          s:=s+''''+FormatDateTime('yyyy-mm-dd hh:nn:ss', theQry.Fields[i].AsDateTime)+''''
        else if(theQry.Fields[i].DataType=ftDate)then
          s:=s+''''+FormatDateTime('yyyy-mm-dd', theQry.Fields[i].AsDateTime)+''''
        else if(theQry.Fields[i].DataType=ftTime)then
          s:=s+''''+FormatDateTime('hh:nn:ss', theQry.Fields[i].AsDateTime)+''''
        else
          s:=s+''''+DMMain.ReplaceText(theQry.Fields[i].AsString, '''', '''''')+'''';

        if(i<theQry.Fields.Count-1)then
          s:=s+', ';
      end;

      inserts:=inserts+insertHeader+s+');'+#13#10;

      inc(reccount);

      theQry.Next;
    end;

    //theQry.First;
  finally
    theQry.EnableControls;
  end;

  GetRecordsAsInserts:=inserts;
end;

function TDMDB.GetRecordsAsInsertsTSQLQuery(SQLCmd: string; theQry: TSQLQuery; limit: integer = 0): string;
var insertHeader, inserts, tablename, s: string;
  i, reccount: integer;
begin
  //Get Table name after FROM
  //SQLCmd:=theQry.SQL.Text;
  s:=Copy(SQLCmd, Pos('FROM ', UpperCase(SQLCmd))+5, Length(SQLCmd));
  if(Pos(#13#10, s)=0)then
  begin
    if(Pos(' ', s)=0)then
      tablename:=s
    else
      tablename:=Copy(s, 1, Pos(' ', s)-1)
  end
  else
    tablename:=Copy(s, 1, Pos(#13#10, s)-1);

  //Build Fieldnames
  s:='INSERT INTO '+tablename+'(';
  for i:=0 to theQry.Fields.Count-1 do
    s:=s+theQry.Fields[i].DisplayName+', ';

  //Remove last ,+Space
  s:=Copy(s, 1, Length(s)-2);

  insertHeader:=s+')'+#13#10+'VALUES(';


  inserts:='';

  theQry.DisableControls;
  try
    //theQry.First;
    reccount:=0;
    while(not(theQry.Eof))and((limit=0)or(reccount<limit))do
    begin
      s:='';
      for i:=0 to theQry.Fields.Count-1 do
      begin
        if(theQry.Fields[i].DataType=ftSmallint)or
          (theQry.Fields[i].DataType=ftInteger)or
          (theQry.Fields[i].DataType=ftWord)or
          (theQry.Fields[i].DataType=ftLargeint)or
          (theQry.Fields[i].DataType=ftAutoInc)then
        begin
          if(theQry.Fields[i].AsString<>'')then
            s:=s+theQry.Fields[i].AsString
          else
            s:=s+'0';
        end
        else if(theQry.Fields[i].DataType=ftString)or
          (theQry.Fields[i].DataType=ftFixedChar)or
          (theQry.Fields[i].DataType=ftWideString)then
          s:=s+''''+DMMain.ReplaceText(theQry.Fields[i].AsString, '''', '''''')+''''
        else if(theQry.Fields[i].DataType=ftDateTime)or
          (theQry.Fields[i].DataType=ftTimeStamp)then
          s:=s+''''+FormatDateTime('yyyy-mm-dd hh:nn:ss', theQry.Fields[i].AsDateTime)+''''
        else if(theQry.Fields[i].DataType=ftDate)then
          s:=s+''''+FormatDateTime('yyyy-mm-dd', theQry.Fields[i].AsDateTime)+''''
        else if(theQry.Fields[i].DataType=ftTime)then
          s:=s+''''+FormatDateTime('hh:nn:ss', theQry.Fields[i].AsDateTime)+''''
        else
          s:=s+''''+DMMain.ReplaceText(theQry.Fields[i].AsString, '''', '''''')+'''';

        if(i<theQry.Fields.Count-1)then
          s:=s+', ';
      end;

      inserts:=inserts+insertHeader+s+');'+#13#10;

      inc(reccount);

      theQry.Next;
    end;

    //theQry.First;
  finally
    theQry.EnableControls;
  end;

  GetRecordsAsInsertsTSQLQuery:=inserts;
end;

function TDMDB.GetFirstSQLCmdFromScript(var cmds: String): String;
var theCmdList: TStringList;
  cmd: string;
  delimCount: integer;
  cmdComplete, delimOpen, commentLine: Boolean;
begin
  theCmdList:=TStringList.Create;
  try
    theCmdList.Text:=cmds;
    cmd:='';
    cmdComplete:=False;
    delimOpen:=False;

    while(theCmdList.Count>0)and(Not(cmdComplete))do
    begin
      commentLine:=False;

      if(Not(delimOpen))then
        if(Copy(Trim(theCmdList[0]), 1, 2)='//')then
          commentLine:=True;

      if(Not(commentLine))then
      begin
        //Add line to command
        if(cmd<>'')then
          cmd:=cmd+#13#10;
        cmd:=cmd+theCmdList[0];

        delimCount:=DMMain.GetSubStringCountInString(theCmdList[0], '''');
        if(delimCount mod 2=1)then
          delimOpen:=Not(delimOpen);

        //Check if this is the last cmd
        if(theCmdList.Count>1)then
          if((Copy(Trim(theCmdList[1]), 1, 1)='')and(Not(delimOpen)))then
            cmdComplete:=True;

        if(Copy(Trim(theCmdList[0]), Length(Trim(theCmdList[0])), 1)=';')then
          cmdComplete:=True;
      end;

      theCmdList.Delete(0);

      //Delete all empty lines till next cmd or Clear List
      if(theCmdList.Count>0)then
      begin
        while(trim(theCmdList[0])='')and(theCmdList.Count>1)do
          theCmdList.Delete(0);

        //if last list
        if(trim(theCmdList.Text)='')and(theCmdList.Count>0)then
          theCmdList.Clear;
      end;
    end;

    //Remove trailing ;
    if(Copy(trim(cmd), Length(trim(cmd)), 1)=';')then
      cmd:=Copy(trim(cmd), 1, Length(trim(cmd))-1);

    GetFirstSQLCmdFromScript:=cmd;
    cmds:=theCmdList.Text;
  finally
    theCmdList.Free;
  end;
end;

function TDMDB.ExecuteSQLCmdScript(cmds: String): integer;
var ignoreScriptErrors: Boolean;
  mRes, totalRowsAffected, RowsAffected: integer;
begin
  ignoreScriptErrors:=False;
  totalRowsAffected:=0;

  while(cmds<>'')do
  begin
    //Get first script and remove it from the cmds
    OutputQry.SQL.Text:=GetFirstSQLCmdFromScript(cmds);
    try
      RowsAffected:=OutputQry.ExecSQL(True);

      if(RowsAffected>0)then
        totalRowsAffected:=totalRowsAffected+RowsAffected;
    except
      on x: Exception do
      begin
        if(Not(ignoreScriptErrors))then
        begin
          if(cmds='')then
            MessageDlg(DMMain.GetTranslatedMessage('ERROR while executing Query: '+#13#10#13#10+'%s', 145,
              Trim(OutputQry.SQL.Text)+#13#10#13#10+
              x.Message), mtError, [mbOk], 0)
          else
          begin
            mRes:=MessageDlg(DMMain.GetTranslatedMessage('ERROR while executing Query: '+#13#10#13#10+'%s', 145,
              Trim(OutputQry.SQL.Text)+#13#10#13#10+
              x.Message+#13#10#13#10)+
              DMMain.GetTranslatedMessage('Do you want to continue executing the script? '+
              'Press [Ignore] to ignore all Errors.', 146), mtError,
              [mbYes, mbNo, mbIgnore], 0);

            if(mRes=mrNo)then
            begin
              ExecuteSQLCmdScript:=totalRowsAffected;
              Exit;
            end;

            if(mRes=mrIgnore)then
              ignoreScriptErrors:=True;
          end;
        end;
      end;
    end;
  end;

  ExecuteSQLCmdScript:=totalRowsAffected;
end;

constructor TDBConn.Create;
begin
  inherited;

  Params:=TStringList.Create;
end;

destructor TDBConn.Destroy;
begin
  Params.Free;

  inherited;
end;

procedure TDBConn.Assign(Source: TPersistent);
begin
  if(Source is TDBConn)then
  begin
    Name:=TDBConn(Source).Name;
    Description:=TDBConn(Source).Description;
    DriverName:=TDBConn(Source).DriverName;
    GetDriverFunc:=TDBConn(Source).GetDriverFunc;
    LibraryName:=TDBConn(Source).LibraryName;
    VendorLib:=TDBConn(Source).VendorLib;
    TableScope:=TDBConn(Source).TableScope;
    Params.Text:=TDBConn(Source).Params.Text;
  end;
end;

constructor TDBHost.Create;
begin
  inherited;
end;

destructor TDBHost.Destroy;
begin
  inherited;
end;

end.
