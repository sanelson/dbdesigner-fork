{
  Version: 2006.03.07
}
unit dbx_ora_connect;

interface

uses
  Windows, SysUtils, Classes, SqlConst, SqlExpr, DbxOpenOdbcInterface, Registry;

procedure OracleConnect(SQLConnection: TSQLConnection;
  const TnsName, UserName, Password: string;
  bMicrosoftDriver: Boolean = False;
  bDirectOdbc: Boolean = True;
  bLoginPrompt: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = ''
);


function IsPresentedOracleDriver(): Boolean;
function IsPresentedMicrosoftOracleDriver(): Boolean;

implementation

procedure OracleConnect(SQLConnection: TSQLConnection;
  const TnsName, UserName, Password: string;
  bMicrosoftDriver: Boolean = False;
  bDirectOdbc: Boolean = True;
  bLoginPrompt: Boolean = False;
  const DNS_NAME: string = '';
  const AdditionalOptions: string = ''
);
var
  sConnectionString, sDrv: string;
begin
  SQLConnection.Close;

    SQLConnection.DriverName    := '@MyDriver';
    SQLConnection.GetDriverFunc := 'getSQLDriverODBC';
    SQLConnection.LibraryName   := 'dbxoodbc.dll';
    SQLConnection.LoginPrompt := bLoginPrompt;

    SQLConnection.Params.Clear;
    SQLConnection.Params.Values['Trim Char'] := 'True';

    if not bMicrosoftDriver then
    begin
      sConnectionString :=
          'UID=' + UserName
        + ';PWD=' + Password
        + ';DBQ=' + TnsName
        + ';DBA=W;APA=T;FEN=T;QTO=T;FRC=10;FDL=10;LOB=T;RST=T;FRL=F;MTS=T;CSR=T;PFC=10;TLO=0'
        + ';coLockMode=-1;coSchemFlt=1;coCatPrefix=UID';
      if bDirectOdbc then
        SQLConnection.VendorLib := 'sqora32.dll';
    end
    else
    begin
      sConnectionString :=
          'UID=' + UserName
        + ';PWD=' + Password
        + ';SERVER=' + TnsName
        + ';coLockMode=-1;coSchemFlt=1;coCatPrefix=UID';
      if bDirectOdbc then
        SQLConnection.VendorLib := 'msorcl32.dll';
    end;


    if AdditionalOptions <> '' then
      sConnectionString := sConnectionString + ';' + AdditionalOptions;

    if not bDirectOdbc then
    begin
      SQLConnection.VendorLib := 'odbc32.dll';
      if DNS_NAME <> '' then
        sConnectionString := 'DNS=' + DNS_NAME + ';' + sConnectionString
      else
      begin
        if not bMicrosoftDriver then
          sDrv := '{Oracle ODBC Driver}'
        else
          sDrv := '{Microsoft ODBC for Oracle}';
        sConnectionString := 'DRIVER=' + sDrv + ';' + sConnectionString;
      end;
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

function IsPresentedOracleDriver(): Boolean;
begin
  Result := False;
  try
    with TRegistry.Create(KEY_READ) do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\Oracle ODBC Driver', False) and
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
  Result := FileExists('sqora32.dll');
end;

function IsPresentedMicrosoftOracleDriver(): Boolean;
begin
  Result := False;
  try
    with TRegistry.Create(KEY_READ) do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey('SOFTWARE\ODBC\ODBCINST.INI\Microsoft ODBC for Oracle', False) and
        ValueExists('Driver') then
      begin
         Result := True;
      end;
    finally
      Free;
    end;
  except
  end;
end;

end.
