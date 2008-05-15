{
  Version: 2008.02.11
}
unit dbx_mssql_connect;

interface

uses
  Windows, SysUtils, Classes, SqlConst, SqlExpr, DbxOpenOdbcInterface, Registry;

procedure MsSqlConnect(SQLConnection: TSQLConnection;
  const ServerName, DatabaseName, UserName, Password: string;
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  OSAuthentication: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = '';
  const LanguageName: string = '';
  // Allow use of DBX3
  bUnicodeDbxDriver: Boolean = False;
  // Allow or nor use of adbc unicode api
  bUnicodeODBCAPI: Boolean = False;
  // Allow or nor use of TWideStringField
  bAnsiFields: Boolean = True
);


function GetDirectMsSqlDriver(): string;

implementation

//DSN=dsn_mssql;UID=user_name;PWD=mypassword;SERVER=server_name_or_ip;
//DATABASE=database_name;Trusted_Connection=Yes;APP=application_name;
//WSID=client_host_name;Network=DBMSSOCN

procedure MsSqlConnect(SQLConnection: TSQLConnection;
  const ServerName, DatabaseName, UserName, Password: string;
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  OSAuthentication: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = '';
  const LanguageName: string = '';
  bUnicodeDbxDriver: Boolean = False;
  bUnicodeODBCAPI: Boolean = False;
  bAnsiFields: Boolean = True
);
var
  sConnectionString: string;
begin
  SQLConnection.Close;

    SQLConnection.DriverName    := '@MyDriver';
    if not bUnicodeDbxDriver then
    begin
      if not bUnicodeODBCAPI then
      begin
        SQLConnection.GetDriverFunc := 'getSQLDriverODBC';
        RegisterDbXpressLibA();
      end
      else
      begin
        SQLConnection.GetDriverFunc := 'getSQLDriverODBCAW';
        RegisterDbXpressLibAW();
      end;
    end
    else
    begin
      if bUnicodeODBCAPI then
      begin
        SQLConnection.GetDriverFunc := 'getSQLDriverODBCW';
        RegisterDbXpressLibW();
      end
      else
      begin
        SQLConnection.GetDriverFunc := 'getSQLDriverODBCWA';
        RegisterDbXpressLibWA();
      end;
    end;

    SQLConnection.LibraryName   := 'dbxoodbc.dll';

    if UserName = '' then
      OSAuthentication := True;

    if OSAuthentication then
    begin
      LoginPrompt := False;
      sConnectionString := 'Trusted_Connection=Yes';
    end
    else
    begin
      sConnectionString :=
          'UID=' + UserName
        + ';PWD=' + Password
    end;
    SQLConnection.LoginPrompt := LoginPrompt;
    SQLConnection.Params.Clear;
    SQLConnection.Params.Values['Trim Char'] := 'True';

    SQLConnection.VendorLib := '';
    if DirectOdbc then
      SQLConnection.VendorLib := GetDirectMsSqlDriver;
    if SQLConnection.VendorLib = '' then
    begin
      DirectOdbc := False;
      SQLConnection.VendorLib := 'odbc32.dll';
    end;

    if (DNS_NAME = '') and (ServerName = '') then
      sConnectionString := sConnectionString + ';SERVER=127.0.0.1'
    else if ServerName <> '' then
      sConnectionString := sConnectionString + ';SERVER=' + ServerName;

    if DatabaseName <> '' then
      sConnectionString := sConnectionString + ';DATABASE=' + DatabaseName;

    if LanguageName <> '' then
      sConnectionString := sConnectionString + ';LANGUAGE=' + DatabaseName;

    if AdditionalOptions <> '' then
      sConnectionString := sConnectionString + ';' + AdditionalOptions;

    sConnectionString := sConnectionString + ';' + 'coCatPrefix=DATABASE';

    if DirectOdbc then
      sConnectionString := 'DRIVER={SQL Server};' + sConnectionString
    else
    begin
      if DNS_NAME <> '' then
        sConnectionString := 'DNS=' + DNS_NAME + ';' + sConnectionString
      else
        sConnectionString := 'DRIVER={SQL Server};' + sConnectionString;
    end;

    //bAnsiFields
    if not bUnicodeDbxDriver then
    begin
      //
      // DBX2 not supported TWideStringField
      //
      //if not bAnsiFields then
      //  sConnectionString := sConnectionString + ';coEnableUnicode=1';
      sConnectionString := sConnectionString + ';coEnableUnicode=0';
    end
    else
    begin
      if not bAnsiFields then
        sConnectionString := sConnectionString + ';coEnableUnicode=0';
    end;

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
end;

function GetDirectMsSqlDriver(): string;
begin
  try
    with TRegistry.Create(KEY_READ) do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\SQL Server', False) and
        ValueExists('Driver') then
      begin
         Result := Trim(ReadString('Driver'));
         Exit;
      end;
    finally
      Free;
    end;
  except
  end;

  {$IFNDEF WIN64}
  Result :='sqlsrv32.dll';
  {$ELSE}
  Result :='sqlsrv64.dll';
  {$ENDIF}

  if not FileExists(Result) then
    SetLength(Result, 0);
end;

end.
