unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DBXpress, FMTBcd, ComCtrls, StdCtrls, ExtCtrls, DB, SqlExpr,
  {$IF CompilerVersion > 17.00}
  //WideStrings,
  {$IFEND}
// optional (static linking dbxoodbc.dll):
  {$IF CompilerVersion < 18.50}
  DbxOpenOdbc,
  {$IF CompilerVersion = 18.00}
  DbxOpenOdbc3,
  {$IFEND}
  {$IFEND}
// optional.
  SqlConst, DbxOpenOdbcInterface, Registry, Provider,
  DBClient, Grids, DBGrids;

type

  TQueryCursorStatus = set of (
    qcsGetNextRecordSetCalled,
    // midas: supports NextRecordSet for IProviderSupport.
    //        not tested when CDS.PacketRecords > 0.
    qcsMidasMode,
    qcsCloseCursorCalled,
    qcsInternalCloseCalled,
    qcsNextRecordSetCalled,
    qcsNextRecordSetEmpty
    // midas.
  );

  TSQLQueryEx = class(SqlExpr.TSQLQuery)
  private
    FCursorStatus: TQueryCursorStatus;
    procedure ClearCachedSchema;
    function GetMidasMode: Boolean;
    procedure SetMidasMode(Value: Boolean);
  protected
    procedure CloseCursor; override;
    procedure InternalClose; override;
    procedure InternalRefresh; override;
  public
    function NextRecordSet: TCustomSQLDataSet;
    property MidasMode: Boolean read GetMidasMode write SetMidasMode;
  end;

  // replace form classes to TSQLQueryEx:
  TSQLQuery = class(TSQLQueryEx);

  TForm1 = class(TForm)
    SQLConnection: TSQLConnection;
    SQLQuery: TSQLQuery;
    p1: TPanel;
    txt1: TStaticText;
    btn_sqlserver_connect: TButton;
    btn_sqlserver_disconnect: TButton;
    PC: TPageControl;
    tbsh_schema: TTabSheet;
    tbsh_cursors: TTabSheet;
    tbsh_results: TTabSheet;
    mem_schema_make: TMemo;
    mem_query_cursors: TMemo;
    p2: TPanel;
    btn_cursors_open: TButton;
    p3: TPanel;
    btn_cursors_nextrecordset: TButton;
    btn_cursors_reopen: TButton;
    btn_cursors_close: TButton;
    sh_connection_status: TShape;
    mem_cursors_log: TMemo;
    p4: TPanel;
    p5: TPanel;
    mem_schema_drop: TMemo;
    txt2: TStaticText;
    txt3: TStaticText;
    btn_schema_make: TButton;
    btn_schema_drop: TButton;
    SQLQuerySchema: TSQLQuery;
    tbsh_cds: TTabSheet;
    p6: TPanel;
    btn_cds_nextrecordset: TButton;
    btn_cds_reopen: TButton;
    btn_cds_close: TButton;
    SQLQuery_midas: TSQLQuery;
    grd1: TDBGrid;
    DataSource_midas: TDataSource;
    CDS: TClientDataSet;
    DSP: TDataSetProvider;
    cbx_connection_string: TComboBox;
    procedure btn_sqlserver_connectClick(Sender: TObject);
    procedure btn_sqlserver_disconnectClick(Sender: TObject);
    procedure btn_cursors_openClick(Sender: TObject);
    procedure btn_cursors_reopenClick(Sender: TObject);
    procedure btn_cursors_closeClick(Sender: TObject);
    procedure btn_cursors_nextrecordsetClick(Sender: TObject);
    procedure SQLConnectionAfterDisconnect(Sender: TObject);
    procedure SQLConnectionAfterConnect(Sender: TObject);
    procedure p1Resize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn_schema_makeClick(Sender: TObject);
    procedure btn_schema_dropClick(Sender: TObject);
    procedure btn_cds_reopenClick(Sender: TObject);
    procedure btn_cds_nextrecordsetClick(Sender: TObject);
    procedure btn_cds_closeClick(Sender: TObject);
    procedure SQLQueryAfterClose(DataSet: TDataSet);
    procedure SQLConnectionBeforeDisconnect(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure CheckConnection();
  public
    { Public declarations }
    fResultSetNum: Integer;
    procedure print_dataset(D: TDataSet; ClearMemo: Boolean = True);
  end;

var
  Form1: TForm1;

function SQLDataSet_NextRecordSet(DS: TCustomSQLDataSet): TCustomSQLDataSet;
procedure SQLDataSet_InternalOpenPlus(DS: TCustomSQLDataSet);
procedure SQLDataSet_InternalClosePlus(DS: TCustomSQLDataSet);

implementation

{$R *.dfm}

{ TSQLQueryEx }

procedure TSQLQueryEx.ClearCachedSchema;
begin
  //
  // Clear cached schema:
  //
  if Assigned(FieldDefs) then
    FieldDefs.Updated := False;
  ClearIndexDefs;
end;

function TSQLQueryEx.GetMidasMode: Boolean;
begin
  Result := qcsMidasMode in FCursorStatus;
end;

procedure TSQLQueryEx.SetMidasMode(Value: Boolean);
begin
  if Value then
    Include(FCursorStatus, qcsMidasMode)
  else
    Exclude(FCursorStatus, qcsMidasMode);
end;

procedure TSQLQueryEx.CloseCursor;
begin
  // midas:
  if qcsCloseCursorCalled in FCursorStatus then
  begin
    inherited CloseCursor;
    Exit;
  end;
  if MidasMode then
  begin
    if qcsNextRecordSetEmpty in FCursorStatus then
    begin
      Exclude(FCursorStatus, qcsNextRecordSetEmpty);
      inherited;
      ClearCachedSchema;
      DestroyFields; // ???
      DataBaseError(SqlConst.SNoCursor,Self);
    end;
    Include(FCursorStatus, qcsCloseCursorCalled);
    try
      if NextRecordSet = nil then
        Include(FCursorStatus, qcsNextRecordSetEmpty)
      else
        Exclude(FCursorStatus, qcsNextRecordSetEmpty);
    finally
      Exclude(FCursorStatus, qcsCloseCursorCalled);
    end;
  end
  else
  // midas.
    inherited;
end;

procedure TSQLQueryEx.InternalClose;
begin
  inherited;
  // midas:
  if qcsInternalCloseCalled in FCursorStatus then
  begin
    ClearCachedSchema;
    Exit;
  end;
  if MidasMode and (not (qcsNextRecordSetCalled in FCursorStatus)) then
  begin
    Include(FCursorStatus, qcsInternalCloseCalled);
    try
      if NextRecordSet = nil then
        Include(FCursorStatus, qcsNextRecordSetEmpty)
      else
        Exclude(FCursorStatus, qcsNextRecordSetEmpty);
    finally
      Exclude(FCursorStatus, qcsInternalCloseCalled);
    end;
  end
  else
    Exclude(FCursorStatus, qcsNextRecordSetEmpty);
  // midas:
  if qcsGetNextRecordSetCalled in FCursorStatus then
  begin
    Exclude(FCursorStatus, qcsGetNextRecordSetCalled);
    ClearCachedSchema;
  end;
end;

procedure TSQLQueryEx.InternalRefresh;
begin
  if qcsGetNextRecordSetCalled in FCursorStatus then
  begin
    Exclude(FCursorStatus, qcsGetNextRecordSetCalled);
    SetState(dsInactive);
    CloseCursor;
  end;
  ClearCachedSchema;
  DestroyFields;
  inherited;
end;

function TSQLQueryEx.NextRecordSet: TCustomSQLDataSet;
begin
  Include(FCursorStatus, qcsGetNextRecordSetCalled);
  // midas:
  Include(FCursorStatus, qcsNextRecordSetCalled);
  // midas.
  try
    ClearCachedSchema;
    Result := TSQLStoredProc(Self).NextRecordSet;
    // midas:
    Exclude(FCursorStatus, qcsNextRecordSetCalled);
    // midas.
  except
    // midas:
    Exclude(FCursorStatus, qcsNextRecordSetCalled);
    // midas.
    if not IsCursorOpen then // check "no next cursor"
      Result := nil
    else
      raise;
  end;

  // Delphi 2006 UP:
  {$IF CompilerVersion >= 18.00}
  if (Result = nil) and Active then
    Close;
  {$IFEND}
end;

{ TCustomSQLDataSetPro }

type
  TCustomSQLDataSetPro = class(TCustomSQLDataSet);

function SQLDataSet_NextRecordSet(DS: TCustomSQLDataSet): TCustomSQLDataSet;
begin
  try
    Result := TSQLStoredProc(DS).NextRecordSet;
  except
    if not TCustomSQLDataSetPro(DS).IsCursorOpen then // check "no next cursor"
      Result := nil
    else
      raise;
  end;
  // Delphi 2006 UP:
  {$IF CompilerVersion >= 18.00}
  if (Result = nil) and DS.Active then
    DS.Close;
  {$IFEND}
end;

procedure SQLDataSet_InternalOpenPlus(DS: TCustomSQLDataSet);
begin
  with TCustomSQLDataSetPro(DS) do
  begin
    {+}
    //
    // Clear cached schema:
    //
    if Assigned(FieldDefs) then
      FieldDefs.Updated := False;
    ClearIndexDefs;
    {+.}
  end;
end;

procedure SQLDataSet_InternalClosePlus(DS: TCustomSQLDataSet);
begin
  with TCustomSQLDataSetPro(DS) do
  begin
    {+}
    //
    // Clear cached schema:
    //
    if Assigned(FieldDefs) then
      FieldDefs.Updated := False;
    ClearIndexDefs;
    {+.}
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  PC.TabIndex := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SQLConnection.Close;
end;

procedure TForm1.CheckConnection();
begin
  if not SQLConnection.Connected then
    //Abort;
    raise Exception.Create('Set connection with server MSSQL');
end;

function IsPresentedMsSqlDriver(): Boolean;
begin
  try
    with TRegistry.Create(KEY_READ) do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\SQL Server', False) and
        ValueExists('Driver') then
      begin
         Result := True;
         Exit;
      end;
    finally
      Free;
    end;
  except
  end;
  Result := FileExists('sqlsrv32.dll');
end;

procedure TForm1.btn_sqlserver_connectClick(Sender: TObject);
var
  sConnectionString, sConnectionStringUp: string;
  IsConnectionString, DirectOdbc: Boolean;
begin
  SQLConnection.Close;

  sConnectionString := Trim(cbx_connection_string.Text);
  IsConnectionString := (sConnectionString <> '') and (sConnectionString <> '?');
  if not IsConnectionString then
    sConnectionString := '?'
  else
    sConnectionStringUp := UpperCase(sConnectionString);

  SQLConnection.DriverName    := '@MyDriver';
  {$IF CompilerVersion >= 18.00} { Delphi 2006 UP }
  //
  // DBX 3 driver:
  //
  SQLConnection.GetDriverFunc := 'getSQLDriverODBCW';
  RegisterDbXpressLibW();
  {$ELSE}
  //
  // DBX 2 driver:
  //
  SQLConnection.GetDriverFunc := 'getSQLDriverODBC';
  RegisterDbXpressLibA();
  {$IFEND}
  SQLConnection.LibraryName   := 'dbxoodbc.dll';

  //if OSAuthentication then
  //begin
  //  LoginPrompt := False;
  //  sConnectionString := 'Trusted_Connection=Yes';
  //  SQLConnection.LoginPrompt := False;
  //end
  //else
  //begin
  //  sConnectionString :=
  //      'UID=' + UserName
  //    + ';PWD=' + Password
  //  SQLConnection.LoginPrompt := UserName <> '';
  //end;
  SQLConnection.Params.Clear;
  SQLConnection.Params.Values['Trim Char'] := 'True';

  DirectOdbc := IsConnectionString and IsPresentedMsSqlDriver;
  if DirectOdbc then
    SQLConnection.VendorLib := 'sqlsrv32.dll'
  else
  begin
    SQLConnection.VendorLib := 'odbc32.dll';
    if IsConnectionString then
    begin
      if Pos('DRIVER={SQL SERVER}', sConnectionStringUp) <= 0 then
        sConnectionString := 'DRIVER={SQL Server};' + sConnectionString;
    end;
  end;

  if IsConnectionString then
  begin
    if (Pos('UID', sConnectionStringUp) <= 0)
      and (Pos('TRUSTED_CONNECTION', sConnectionStringUp) <= 0) then
      sConnectionString := sConnectionString + ';Trusted_Connection=Yes';

    SQLConnection.LoginPrompt := not ((Pos('TRUSTED_CONNECTION', sConnectionStringUp) > 0)
     or (Pos('UID', sConnectionStringUp) > 0));
  end
  else
    SQLConnection.LoginPrompt := False;

  {$IF CompilerVersion > 14.01}
     // Delphi 7 Up
     SQLConnection.Params.Values[DATABASENAME_KEY]  := '?';
     SQLConnection.Params.Values[CUSTOM_INFO] := cConnectionOptionsNames[coConnectionString] + '=' + sConnectionString;
  {$ELSE}
     // Delphi 6
     if Length(sConnectionString) > 255 then
       SetLength(sConnectionString, 255); // AV protect :(
     SQLConnection.Params.Values[DATABASENAME_KEY]  := sConnectionString;
  {$IFEND}

  SQLConnection.Open;
  PC.Enabled := True;
  PC.TabIndex := 0;

//
// test connection option 'Prepare SQL':
//
//(*
  {$IF CompilerVersion >= 18.00} { Delphi 2005 UP }
  SQLConnection.SQLConnection.SetOption(eConnPrepareSQL, LongInt(False));
  {$ELSE} { Delphi 6, 7 }
  SQLConnection.SQLConnection.SetOption(TSQLConnectionOption(xeConnPrepareSQL), LongInt(False));
  {$IFEND}
//*)

end;

procedure TForm1.SQLConnectionBeforeDisconnect(Sender: TObject);
begin
  SQLQuery.Close;
  //SQLQuery_midas.MidasMode := False;
  cds.Close;
  SQLQuery_midas.Close;
end;

procedure TForm1.btn_sqlserver_disconnectClick(Sender: TObject);
begin
  SQLConnection.Close;
  PC.Enabled := False;
  PC.TabIndex := 1;
end;

procedure TForm1.SQLConnectionAfterDisconnect(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    sh_connection_status.Brush.Color := clSilver;
end;

procedure TForm1.SQLConnectionAfterConnect(Sender: TObject);
begin
  sh_connection_status.Brush.Color := clRed;
end;

procedure TForm1.btn_schema_makeClick(Sender: TObject);
begin
  CheckConnection();
  SQLQuerySchema.Close;
  SQLQuerySchema.SQL.Text := mem_schema_make.Lines.Text;
  SQLQuerySchema.ExecSQL({Direct:}True);
//  SQLQuerySchema.ExecSQL;
end;

procedure TForm1.btn_schema_dropClick(Sender: TObject);
begin
  CheckConnection();
  SQLQuerySchema.Close;
  SQLQuerySchema.SQL.Text := mem_schema_drop.Lines.Text;
  SQLQuerySchema.ExecSQL({Direct:}True);
//  SQLQuerySchema.ExecSQL;
end;

procedure TForm1.btn_cursors_openClick(Sender: TObject);
begin
  CheckConnection();
  SQLQuery.Close;
  SQLQuery.SQL.Text := mem_query_cursors.Lines.Text;
  //SQLDataSet_InternalOpenPlus(SQLQuery);
  SQLQuery.Open;
  PC.TabIndex := 2;
  fResultSetNum := 1;
  print_dataset(SQLQuery);
end;

procedure TForm1.SQLQueryAfterClose(DataSet: TDataSet);
begin
  fResultSetNum := 0;
end;

procedure TForm1.btn_cursors_reopenClick(Sender: TObject);
begin
  CheckConnection();

  //SQLQuery.Close;
  //SQLQuery.SQL.Text := mem_query_cursors.Lines.Text;
  //SQLDataSet_InternalOpenPlus(SQLQuery);
  //SQLQuery.Open;

  if SQLQuery.Active then
    SQLQuery.Refresh
  else
  begin
    SQLQuery.SQL.Text := mem_query_cursors.Lines.Text;
    SQLQuery.Open;
  end;

  fResultSetNum := 1;

  print_dataset(SQLQuery);
end;

procedure TForm1.btn_cursors_closeClick(Sender: TObject);
begin
  CheckConnection();
  SQLQuery.Close;
  mem_cursors_log.Lines.Clear;
end;

procedure TForm1.btn_cursors_nextrecordsetClick(Sender: TObject);
begin
  if SQLQuery.Active then
  begin
    //if SQLDataSet_NextRecordSet(SQLQuery) <> nil then
    if SQLQuery.NextRecordSet <> nil then
    begin
      Inc(fResultSetNum);
      print_dataset(SQLQuery, False);
    end
    else
    begin
      mem_cursors_log.Lines.Add('');
      mem_cursors_log.Lines.Add('');
      mem_cursors_log.Lines.Add('-- no more resultset --');
    end;
  end;
end;

procedure TForm1.p1Resize(Sender: TObject);
begin
  cbx_connection_string.Width := p1.ClientWidth - 2 * cbx_connection_string.Left;
end;

procedure TForm1.print_dataset(D: TDataSet; ClearMemo: Boolean = True);
const
  iColMaxCount = 5;
  iRowMaxCount = 7;
var
  iCol, iRow, i: Integer;
  s: string;
begin
  if fResultSetNum <= 1 then
    mem_cursors_log.Lines.Clear;

  iCol := D.FieldCount-1;
  if iCol > iColMaxCount-1 then
    iCol := iColMaxCount-1;
  iRow := 0;

  with mem_cursors_log.Lines do
  begin
    Add('');
    Add(Format('ResulSet Num %d:', [fResultSetNum]));
    Add('--------------------');
  end;

  while (not D.Eof) and (iRow < iRowMaxCount) do with mem_cursors_log.Lines do
  begin
    Inc(iRow);

    Add(Format('  Row num %d: values', [iRow]));
    for i := 0 to iCol do
    begin
      with D.Fields[i] do
        S := '    ' + FieldName + ' = ' + DisplayText;
      Add(S);
    end;

    D.Next;
  end;
end;

procedure TForm1.btn_cds_reopenClick(Sender: TObject);
begin
  CheckConnection();
  SQLQuery_midas.MidasMode := False; // close with next recorset
  cds.Close;
  SQLQuery_midas.SQL.Text := mem_query_cursors.Lines.Text;
  try
    SQLQuery_midas.MidasMode := True; // disable cursor close after data transfer
    cds.Open;
  finally
    SQLQuery_midas.MidasMode := False;
  end;
end;

procedure TForm1.btn_cds_nextrecordsetClick(Sender: TObject);
begin
  CheckConnection();
  if not cds.Active then
    Exit;
  SQLQuery_midas.MidasMode := True; // disable cursor close after data transfer
  try
    cds.Close;
    cds.Open;
  finally
    SQLQuery_midas.MidasMode := False;
  end;
end;

procedure TForm1.btn_cds_closeClick(Sender: TObject);
begin
  CheckConnection();
  SQLQuery_midas.MidasMode := False; // close with next recorset
  cds.Close;
  SQLQuery_midas.Close;
  mem_cursors_log.Lines.Clear;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  p1Resize(p1);
end;

end.
