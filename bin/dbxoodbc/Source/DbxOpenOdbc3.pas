{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.100, 2008-02-11: Beta

  Copyright (c) 2001-2008 Vadim V.Lopushansky

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
unit DbxOpenOdbc3;

{$INCLUDE DbxOpenOdbc_options.inc}

interface

uses
  DSIntf,
  DbxOpenOdbcInterface,
  OdbcApi,
  DBXpress, FmtBcd,
  {$IFDEF _TRACE_CALLS_}
    DbxOpenOdbcTrace,
  {$ENDIF _TRACE_CALLS_}
  DbxOpenOdbc,
  //WideStrUtils,
  Classes,
  SysUtils;

{ getSQLDriverODBC is the starting point for everything else... }

// priority unicode odbc api
function getSQLDriverODBCW(sVendorLib: PAnsiChar; sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;
// priority ansi odbc api
function getSQLDriverODBCWA(sVendorLib: PAnsiChar; sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;

type

  { TSqlDriverOdbc3 implements ISQLDriver }

  TSqlDriverOdbc3 = class(TSqlDriverOdbc, ISQLDriver)
  protected
    { begin ISQLDriver methods }
    function getSQLConnection(
      out pConn: DBXpress.ISQLConnection
      ): SQLResult; stdcall;
    { end ISQLDriver methods }
  public
    constructor Create(AOdbcApi: TOdbcApiProxy; bIsUnicodeOdbcApi: Boolean);
  end;

  { TSqlConnectionOdbc implements ISQLConnection }

  TSqlConnectionOdbc3 = class(TSqlConnectionOdbc, ISQLConnection30)
  protected
    fServerName, fUserName, fPassword: WideString;
  protected
    { begin: ISQLConnection30 interface methods: }
     function connect(): SQLResult; overload; stdcall;
     function connect(ServerName: PWideChar; UserName: PWideChar;
                            Password: PWideChar): SQLResult; overload; stdcall;
     function getSQLCommand(out pComm: ISQLCommand30): SQLResult; stdcall;
     function getSQLMetaData(out pMetaData: ISQLMetaData30): SQLResult; stdcall;
     function SetOption(eConnectOption: TSQLConnectionOption;
              lValue: LongInt): SQLResult; stdcall;
     function GetOption(eDOption: TSQLConnectionOption; PropValue: Pointer;
              MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
     function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
     function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
     { end: ISQLConnection30 interface methods. }
  end;

  { TSqlCommandOdbc implements ISQLCommand }

  TSqlCommandOdbc3 = class(TSqlCommandOdbc, ISQLCommand30)
  protected
    { begin ISQLCommand methods }
    function SetOption(
       eSqlCommandOption: TSQLCommandOption;
       ulValue: Integer): SQLResult; stdcall;
    function GetOption(eSqlCommandOption: TSQLCommandOption;
       PropValue: Pointer;
       MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
    function prepare(SQL: PWideChar; ParamCount: Word): SQLResult; stdcall;
    function execute(var Cursor: ISQLCursor30): SQLResult; stdcall;
    function executeImmediate(SQL: PWideChar; var Cursor: ISQLCursor30): SQLResult; stdcall;
    function getNextCursor(var Cursor: ISQLCursor30): SQLResult; stdcall;
    function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
    function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
    { end ISQLCommand methods }
  end;

  { TSqlCursorOdbc implements ISQLCursor }

  TSqlCursorOdbc3 = class(TSqlCursorOdbc, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnNameLength(
      ColumnNumber: Word;
      var pLen: Word): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

  { TSQLMetaDataOdbc implements ISQLMetaData }

  TSQLMetaDataOdbc3 = class(TSQLMetaDataOdbc, ISQLMetaData30)
  protected
    { begin ISQLMetaData methods }
    function SetOption(
      eDOption: TSQLMetaDataOption;
      PropValue: Longint
      ): SQLResult; stdcall;
    function GetOption(
      eDOption: TSQLMetaDataOption;
      PropValue: Pointer;
      MaxLength: Smallint;
      out Length: Smallint
      ): SQLResult; stdcall;
    function getObjectList(
      eObjType: TSQLObjectType;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getTables(
      TableName: PWideChar;
      TableType: Longword;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getProcedures(
      ProcedureName: PWideChar;
      ProcType: Longword;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getColumns(
      TableName: PWideChar;
      ColumnName: PWideChar;
      ColType: Longword;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getProcedureParams(
      ProcName: PWideChar;
      ParamName: PWideChar;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getIndices(
      TableName: PWideChar;
      IndexType: Longword;
      out Cursor: ISQLCursor30
      ): SQLResult; stdcall;
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen:
      Smallint
      ): SQLResult; stdcall;
    { end ISQLMetaData methods }
  end;

  { TSqlCursorMetaDataTables - implements cursor returned by ISQLMetaData.GetTables }
  TSqlCursorMetaDataTables3 = class(TSqlCursorMetaDataTables, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

  { TSqlCursorMetaDataColumns - implements cursor returned by ISQLMetaData.GetColumns }

  TSqlCursorMetaDataColumns3 = class(TSqlCursorMetaDataColumns, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

  { TSqlCursorMetaDataIndexes - implements cursor returned by ISQLMetaData.GetIndices }

  TSqlCursorMetaDataIndexes3 = class(TSqlCursorMetaDataIndexes, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

  { TSqlCursorMetaDataProcedures - implements cursor returned by ISQLMetaData.GetProcedures }

  TSqlCursorMetaDataProcedures3 = class(TSqlCursorMetaDataProcedures, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

  { TSqlCursorMetaDataProcedureParams - implements cursor returned by ISQLMetaData.GetProcedureParams }

  TSqlCursorMetaDataProcedureParams3 = class(TSqlCursorMetaDataProcedureParams, ISQLCursor30)
  protected
    { begin ISQLCusror methods }
    function getErrorMessage(
      Error: PWideChar
      ): SQLResult; overload; stdcall;
    function getErrorMessageLen(
      out ErrorLen: Smallint
      ): SQLResult; stdcall;
    function getColumnName(
      ColumnNumber: Word;
      pColumnName: PWideChar
      ): SQLResult; stdcall;
    function getString(
      ColumnNumber: Word; Value: PChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getWideString(
      ColumnNumber: Word;
      Value: PWideChar;
      var IsBlank: LongBool
      ): SQLResult; stdcall;
    function getInt64(
       ColumnNumber: Word;
       Value: Pointer;
       var IsBlank: LongBool)
       : SQLResult; stdcall;
    { end ISQLCusror methods }
  end;

var
  DefaultSystemCodePage: Integer = 0;
  cConnectionOptionsDefault3: TConnectionOptions;

function Create_ISQLCursor30(OwnerCommand: TSqlCommandOdbc): Pointer;  {$IFDEF _INLINE_} inline; {$ENDIF}

function Get_ISQLMetaData30(SqlCursorMetaDataTables: TSqlCursorMetaDataTables3): Pointer; overload; {$IFDEF _INLINE_} inline; {$ENDIF}
function Get_ISQLMetaData30(SqlCursorMetaDataColumns: TSqlCursorMetaDataColumns3): Pointer; overload; {$IFDEF _INLINE_} inline; {$ENDIF}
function Get_ISQLMetaData30(SqlCursorMetaDataIndexes: TSqlCursorMetaDataIndexes3): Pointer; overload; {$IFDEF _INLINE_} inline; {$ENDIF}
function Get_ISQLMetaData30(SqlCursorMetaDataProcedures: TSqlCursorMetaDataProcedures3): Pointer; overload; {$IFDEF _INLINE_} inline; {$ENDIF}
function Get_ISQLMetaData30(SqlCursorMetaDataProcedureParams: TSqlCursorMetaDataProcedureParams3): Pointer; overload; {$IFDEF _INLINE_} inline; {$ENDIF}

implementation

uses
  Windows,
  DB,
  SqlExpr,
  SqlTimst, DateUtils;

function min(v1, v2: Integer): Integer;  {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if v1 <= v2 then
    Result := v1
  else
    Result := v2;
end;

function WideStringLengthFromStr(const S: PAnsiChar): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  Result := Length(WideString(string(S)));
end;

//function WideStringLengthFromString(const S: AnsiString): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
//begin
//  Result := Length(WideString(S));
//end;

function WideStringLengthFromStrings(const L: TStrings): Integer; {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if L.Count > 0 then
    Result := Length(WideString(L.Text))
  else
    Result := 0;
end;

//function StrToWideString(S: PAnsiChar): WideString; overload; {$IFDEF _INLINE_} inline; {$ENDIF}
//begin
//  if S <> nil then
//    Result  := StrPas(S)
//  else
//    SetLength(Result, 0);
//end;
//
//function WStrToString(W: PWideChar): AnsiString; {$IFDEF _INLINE_} inline; {$ENDIF}
//begin
//  if W <> nil then
//    Result := AnsiString(WideString(W))
//  else
//    SetLength(Result, 0);
//end;

function WCharFromChar(WCharDest: PWideChar; DestChars: Integer; const CharSource: PChar; SrcBytes: Integer): Integer;  {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  Result := MultiByteToWideChar(DefaultSystemCodePage, 0, CharSource, SrcBytes, WCharDest, DestChars);
end;

function StringToWideChar(const Source: AnsiString; Dest: PWideChar; DestChars: Integer): Integer; overload;
begin
  Result := MultiByteToWideChar(DefaultSystemCodePage, {Flags}0,
    {CharSource:}PChar(Source), {SrcBytes:}Length(Source), Dest, DestChars);
  Dest[Result] := cNullWideChar;
end;

//function StrToWideChar(const Source: PAnsiChar; Dest: PWideChar; DestChars: Integer): Integer; overload;
//begin
//  Result := MultiByteToWideChar(DefaultSystemCodePage, {Flags}0,
//    {CharSource:}PChar(Source), {SrcBytes:}StrLen(Source), Dest, DestChars);
//  Dest[Result] := cNullWideChar;
//end;

procedure StringToWStr(const Source: AnsiString; var Dest: PWideChar); {$IFDEF _INLINE_} inline; {$ENDIF}
var
  iLen: Integer;
begin
  iLen := Length(Source);
  if iLen > 0 then
  begin
    //WStrCopy(Dest, PWideChar(WideString(Source)))
    //StringToWideChar(Source, Dest, {DestCharCount}iLen)
    iLen := WCharFromChar(Dest, {DestCharCount}iLen, PAnsiChar(Source), {SrcBytes}iLen);
    Dest[iLen] := cNullWideChar;
  end
  else
    Dest^ := cNullWideChar;
end;

procedure StringsToWStr(const L: TStrings; var Dest: PWideChar); {$IFDEF _INLINE_} inline; {$ENDIF}
begin
  if L.Count > 0 then
    StringToWStr(L.Text, Dest)
  else
    Dest^ := cNullWideChar;
end;

{ Public function getSQLDriverODBC is the starting point for everything else... }

function getSQLDriverODBCW;//(sVendorLib: PAnsiChar; sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;
var
  OdbcApiProxy: TOdbcApiProxy;
begin
  OdbcApiProxy := LoadOdbcDriverManager(sVendorLib, {UnicodePriority:}True);
  if OdbcApiProxy = nil then
    raise EDbxError.Create('Unable to load specified Odbc Driver manager DLL: ''' + sVendorLib + '''');
  ISQLDriver(Obj) := TSqlDriverOdbc3.Create(OdbcApiProxy, {IsUnicodeOdbcApi:}True);
  Result := DBXERR_NONE;
end;

function getSQLDriverODBCWA;//(sVendorLib: PAnsiChar; sResourceFile: PAnsiChar; out Obj): SQLResult; stdcall;
var
  OdbcApiProxy: TOdbcApiProxy;
begin
  OdbcApiProxy := LoadOdbcDriverManager(sVendorLib, {UnicodePriority:}False);
  if OdbcApiProxy = nil then
    raise EDbxError.Create('Unable to load specified Odbc Driver manager DLL: ''' + sVendorLib + '''');
  ISQLDriver(Obj) := TSqlDriverOdbc3.Create(OdbcApiProxy, {IsUnicodeOdbcApi:}False);
  Result := DBXERR_NONE;
end;

function Create_ISQLCursor30(OwnerCommand: TSqlCommandOdbc): Pointer;
var
  I: ISQLCursor30;
begin
  I := TSqlCursorOdbc3.Create(OwnerCommand);
  Result := Pointer(I);
  Pointer(I) := nil; // disable call _Release
end;

function Get_ISQLMetaData30(SqlCursorMetaDataTables: TSqlCursorMetaDataTables3): Pointer;
var
  I: ISQLCursor30;
begin
  I := TSqlCursorMetaDataTables3(SqlCursorMetaDataTables);
  Result := Pointer(I);
  Pointer(I) := nil; // disable call _Release
end;

function Get_ISQLMetaData30(SqlCursorMetaDataColumns: TSqlCursorMetaDataColumns3): Pointer;
var
  I: ISQLCursor30;
begin
  I := TSqlCursorMetaDataColumns3(SqlCursorMetaDataColumns);
  Result := Pointer(I);
  Pointer(I) := nil; // disable call _Release
end;

function Get_ISQLMetaData30(SqlCursorMetaDataIndexes: TSqlCursorMetaDataIndexes3): Pointer;
var
  I: ISQLCursor30;
begin
  I := TSqlCursorMetaDataIndexes3(SqlCursorMetaDataIndexes);
  Result := Pointer(I);
  Pointer(I) := nil; // disable call _Release
end;

function Get_ISQLMetaData30(SqlCursorMetaDataProcedures: TSqlCursorMetaDataProcedures3): Pointer;
var
  I: ISQLCursor30;
begin
  I := TSqlCursorMetaDataProcedures3(SqlCursorMetaDataProcedures);
  Result := Pointer(I);
  Pointer(I) := nil; // disable call _Release
end;

function Get_ISQLMetaData30(SqlCursorMetaDataProcedureParams: TSqlCursorMetaDataProcedureParams3): Pointer;
var
  I: ISQLCursor30;
begin
  I := TSqlCursorMetaDataProcedureParams3(SqlCursorMetaDataProcedureParams);
  Result := Pointer(I);
  Pointer(I) := nil; // disable call _Release
end;

{ TSqlDriverOdbc3 }

constructor TSqlDriverOdbc3.Create(AOdbcApi: TOdbcApiProxy; bIsUnicodeOdbcApi: Boolean);
begin
  fDBXVersion := 3;
  inherited Create(AOdbcApi, bIsUnicodeOdbcApi);
end;

function TSqlDriverOdbc3.getSQLConnection;//(out pConn: DBXpress.ISQLConnection): SQLResult;
var
  pConn3: ISQLConnection30 absolute pConn;
begin
  pConn3 := TSqlConnectionOdbc3.Create(Self);
  Result := DBXERR_NONE;
end;

{ TSqlConnectionOdbc3 }

function TSqlConnectionOdbc3.connect: SQLResult;
begin
  Result := inherited connect(PAnsiChar(AnsiString(fServerName)), PAnsiChar(AnsiString(fUserName)),  PAnsiChar(AnsiString(fPassword)));
end;

function TSqlConnectionOdbc3.connect(ServerName, UserName,
  Password: PWideChar): SQLResult;
begin
  fServerName := ServerName;
  fUserName := UserName;
  fPassword := Password;
  Result := inherited connect(PAnsiChar(AnsiString(fServerName)), PAnsiChar(AnsiString(fUserName)),  PAnsiChar(AnsiString(fPassword)));
end;

function TSqlConnectionOdbc3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  if Error <> nil then
  begin
    StringsToWStr(fConnectionErrorLines, Error);
    fConnectionErrorLines.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
end;

function TSqlConnectionOdbc3.getErrorMessageLen(out ErrorLen: SmallInt): SQLResult;
begin
  ErrorLen := WideStringLengthFromStrings(fConnectionErrorLines) + 1;
  Result := DBXERR_NONE;
end;

function TSqlConnectionOdbc3.GetOption(eDOption: TSQLConnectionOption; PropValue: Pointer; MaxLength: SmallInt; out Length: SmallInt): SQLResult;
var
  xeDOption: TXSQLConnectionOption absolute eDOption;
  sOptionValue: AnsiString;
  iMaxChars: Integer;
begin
  if (xeDOption in cXSQLConnectionOptionStringTypes) and (PropValue <> nil) and (MaxLength >= SizeOf(WideChar)) then
  begin
    iMaxChars := MaxLength div SizeOf(WideChar) + 1;
    SetLength(sOptionValue, iMaxChars);
    FillChar(sOptionValue[1], iMaxChars, #0);
    Result := inherited GetOption(eDOption, PAnsiChar(sOptionValue), iMaxChars, Length);
    if Result = DBXERR_NONE then
    begin
      SetLength(sOptionValue, StrLen(PAnsiChar(sOptionValue)));
      StringToWideChar(sOptionValue, PWideChar(PropValue), iMaxChars);
    end;
  end
  else
    Result := inherited GetOption(eDOption, PropValue, MaxLength, Length);
end;

function TSqlConnectionOdbc3.SetOption(eConnectOption: TSQLConnectionOption; lValue: Integer): SQLResult;
var
  xeConnectOption: TXSQLConnectionOption absolute eConnectOption;
  sOptionValue: AnsiString;
begin
  if (xeConnectOption in cXSQLConnectionOptionStringTypes) and (lValue <> 0) then
  begin
    sOptionValue := WideString(PWideChar(lValue));
    Result := inherited SetOption(eConnectOption, Integer(PAnsiChar(sOptionValue)));
  end
  else
    Result := inherited SetOption(eConnectOption, lValue);
end;

function TSqlConnectionOdbc3.getSQLCommand(out pComm: ISQLCommand30): SQLResult;
begin
  if fConnected and (not fConnectionClosed) then
  begin
    pComm := TSqlCommandOdbc3.Create(Self);
    Result := DBXERR_NONE;
  end
  else
  begin
    CheckMaxLines(fConnectionErrorLines);
    fConnectionErrorLines.Add('getSQLCommand called but not yet connected');
    Result := DBX_FIRSTSTATICERRORS;
  end;
end;

function TSqlConnectionOdbc3.getSQLMetaData(out pMetaData: ISQLMetaData30): SQLResult;
begin
  if fConnected and (not fConnectionClosed) then
  begin
    pMetaData := TSqlMetaDataOdbc3.Create(Self);
    Result := DBXERR_NONE;
  end
  else
  begin
    CheckMaxLines(fConnectionErrorLines);
    fConnectionErrorLines.Add('getSQLMetaData called but not yet connected');
    Result := DBX_FIRSTSTATICERRORS;
  end;
end;

{ TSqlCommandOdbc3 }

function TSqlCommandOdbc3.prepare(SQL: PWideChar; ParamCount: Word): SQLResult;
var
  S: AnsiString;
begin
  //S := WideString(SQL);
  //Result := inherited prepare(PAnsiChar(S), ParamCount);
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCommandOdbc3.prepare', ['SQL =', SQL, 'ParamCount =', ParamCount, 'StoredProc =', fStoredProc]); {$ENDIF _TRACE_CALLS_}
  if SQL = nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  if TSqlDriverOdbc3(fOwnerDbxDriver).fIsUnicodeOdbcApi then
  begin
    Result := DoPrepare(PAnsiChar(SQL), ParamCount, {UpdateParams:}True,
      {bPrepareSQL:}TSqlConnectionOdbc3(fOwnerDbxConnection).fPrepareSQL, {bUseUnicodeOdbc:} True);
  end
  else
  begin
    S := WideString(SQL);
    Result := DoPrepare(PAnsiChar(S), ParamCount, {UpdateParams:}True,
      {bPrepareSQL:}TSqlConnectionOdbc3(fOwnerDbxConnection).fPrepareSQL, {bUseUnicodeOdbc:} False);
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc3.prepare', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc3.prepare'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc3.execute(var Cursor: ISQLCursor30): SQLResult;
var
  vCursor25: ISQLCursor25 absolute Cursor;
begin
  //Result := inherited execute(vCursor25);
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCommandOdbc3.execute'); {$ENDIF _TRACE_CALLS_}

  Result := DoExecute(vCursor25, {bUseUnicodeOdbc:}TSqlDriverOdbc3(fOwnerDbxDriver).fIsUnicodeOdbcApi);

  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc3.execute', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc3.execute'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc3.executeImmediate(SQL: PWideChar; var Cursor: ISQLCursor30): SQLResult;
var
  vCursor25: ISQLCursor25 absolute Cursor;
  S: AnsiString;
begin
  //S := WideString(SQL);
  //Result := inherited executeImmediate(PAnsiChar(S), vCursor25);
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCommandOdbc3.ExecuteImmediate', ['SQL =', SQL]); {$ENDIF _TRACE_CALLS_}
  if SQL <> nil then
  begin
    if TSqlDriverOdbc3(fOwnerDbxDriver).fIsUnicodeOdbcApi and (fStoredProc <> 1) then
    begin
      Result := DoExecuteImmediate(PAnsiChar(SQL), vCursor25, {bUseUnicodeOdbc:} True)
    end
    else
    begin
      S := WideString(SQL);
      Result := DoExecuteImmediate(PAnsiChar(S), vCursor25, {bUseUnicodeOdbc:} False);
    end;
  end
  else
    Result := DBXERR_INVALIDPARAM;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCommandOdbc3.ExecuteImmediate', e);  raise; end; end;
    finally LogExitProc('TSqlCommandOdbc3.ExecuteImmediate'); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCommandOdbc3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  Result := TSqlConnectionOdbc3(fOwnerDbxConnection).getErrorMessage(Error);
end;

function TSqlCommandOdbc3.getErrorMessageLen(out ErrorLen: SmallInt): SQLResult;
begin
  Result := TSqlConnectionOdbc3(fOwnerDbxConnection).getErrorMessageLen(ErrorLen);
end;

function TSqlCommandOdbc3.getNextCursor(var Cursor: ISQLCursor30): SQLResult;
var
  vCursor25: ISQLCursor25 absolute Cursor;
begin
  Result := inherited getNextCursor(vCursor25);
end;

function TSqlCommandOdbc3.GetOption(eSqlCommandOption: TSQLCommandOption; PropValue: Pointer; MaxLength: Smallint; out Length: SmallInt): SQLResult;
var
  xeSqlCommandOption: TXSQLCommandOption absolute eSqlCommandOption;
  sOptionValue: AnsiString;
  iMaxChars: Integer;
begin
  if (xeSqlCommandOption in cXSQLCommandOptionStringTypes) and (PropValue <> nil) and (MaxLength >= SizeOf(WideChar)) then
  begin
    iMaxChars := MaxLength div SizeOf(WideChar) + 1;
    SetLength(sOptionValue, iMaxChars);
    FillChar(sOptionValue[1], iMaxChars, #0);
    Result := inherited GetOption(eSqlCommandOption, PAnsiChar(sOptionValue), iMaxChars, Length);
    if Result = DBXERR_NONE then
    begin
      SetLength(sOptionValue, StrLen(PAnsiChar(sOptionValue)));
      StringToWideChar(sOptionValue, PWideChar(PropValue), iMaxChars);
    end;
  end
  else
    Result := inherited GetOption(eSqlCommandOption, PropValue, MaxLength, Length);
end;

function TSqlCommandOdbc3.SetOption(eSqlCommandOption: TSQLCommandOption; ulValue: Integer): SQLResult;
var
  xeSqlCommandOption: TXSQLCommandOption absolute eSqlCommandOption;
  sOptionValue: AnsiString;
begin
  if (xeSqlCommandOption in cXSQLCommandOptionStringTypes) and (ulValue <> 0) then
  begin
    sOptionValue := WideString(PWideChar(ulValue));
    Result := inherited SetOption(eSqlCommandOption, Integer(PAnsiChar(sOptionValue)));
  end
  else
    Result := inherited SetOption(eSqlCommandOption, ulValue);
end;

{ TSqlCursorOdbc3 }

function TSqlCursorOdbc3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  Result := TSqlConnectionOdbc3(fOwnerDbxConnection).getErrorMessage(Error);
end;

function TSqlCursorOdbc3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  Result := TSqlConnectionOdbc3(fOwnerDbxConnection).getErrorMessageLen(ErrorLen);
end;

function TSqlCursorOdbc3.getColumnNameLength(ColumnNumber: WORD; var pLen: Word): SQLResult;
begin
  pLen := min(WideStringLengthFromStr(TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]).fColName), SizeOf(DBINAME128)-1);
  Result := DBXERR_NONE;
end;

function TSqlCursorOdbc3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  StringToWideChar(TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]).fColName, pColumnName, SizeOf(DBINAME128) - 1);
  Result := DBXERR_NONE;
end;

function TSqlCursorOdbc3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  {$IFDEF _TRACE_CALLS_} Result := DBXERR_NONE; try try LogEnterProc('TSqlCursorOdbc3.getInt64', ['ColumnNumber =', ColumnNumber]); {$ENDIF _TRACE_CALLS_}
  if Value=nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  with TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]) do
  begin
    if fOdbcLateBound then
      FetchLateBoundData(ColumnNumber);
    IsBlank := (fColValueSizePtr^ = OdbcApi.SQL_NULL_DATA) or
      (fColValueSizePtr^ = OdbcApi.SQL_NO_TOTAL);
    if IsBlank then
      Int64(Value^) := 0
    else
      Int64(Value^) := fOdbcHostVarAddress.ptrSqlBigInt^;
  end;
  Result := DBXERR_NONE;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc3.getInt64', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc3.getInt64', ['Value =', Integer(Value^), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

function TSqlCursorOdbc3.getString(ColumnNumber: Word; Value: PChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Value, IsBlank);
end;

function TSqlCursorOdbc3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
var
  vColValueSize: SqlUInteger;
  RCh: PAnsiChar;
  aOdbcBindCol: TOdbcBindCol;
begin
  {$IFDEF _TRACE_CALLS_}
    if Value <> nil then
      Value^ := cNullWideChar;
    IsBlank := True;
    Result := DBXERR_NONE;
    try try
    LogEnterProc('TSqlCursorOdbc3.getWideString', ['ColumnNumber =', ColumnNumber]);
  {$ENDIF _TRACE_CALLS_}
  if Value = nil then
  begin
    Result := DBXERR_INVALIDPARAM;
    exit;
  end;
  aOdbcBindCol := TOdbcBindCol(fOdbcBindList[ColumnNumber - 1]);
  with aOdbcBindCol do
  begin
    if fOdbcLateBound then
    begin
      if not fIsBuffer then
        FetchLateBoundData(ColumnNumber)
      else
        FetchLongData(ColumnNumber);
    end;

    // check buffer overflow (for bad odbc drivers).
    vColValueSize := fColValueSizePtr^; // buffer length in  bytes
    IsBlank := (vColValueSize = OdbcApi.SQL_NULL_DATA) or
      (vColValueSize = OdbcApi.SQL_NO_TOTAL);

    if (not IsBlank) and (vColValueSize > 0) then
    begin
      if vColValueSize > fColSize * SizeOf(WideChar) then
        vColValueSize := fColSize * SizeOf(WideChar);
    end
    else
    begin
      vColValueSize := 0;
      IsBlank := True;
    end;

    if IsBlank then
    begin
      PWideChar(Value)^ := cNullWideChar
    end
    else
    begin
      if vColValueSize = 0 then
      begin
        PWideChar(Value)^ := cNullWideChar
      end
      else
      if ((fDbxSubType and fldstFIXED) <> fldstFIXED) and TSqlCommandOdbc3(fOwnerCommand).fTrimChar then
      begin
        RCh := PAnsiChar(DWORD(fOdbcHostVarAddress.ptrAnsiChar) + DWORD(vColValueSize - SizeOf(WideChar)));
        // debug: PAnsiChar(DWORD(aOdbcBindCol.fOdbcHostVarAddress.ptrAnsiChar) + DWORD(vColValueSize - SizeOf(WideChar)))
        // debug: aOdbcBindCol.fOdbcHostVarAddress.ptrWideChar
        while (RCh >= fOdbcHostVarAddress.ptrAnsiChar) and (PWideChar(RCh)^ = WideChar(' ')) do
          Dec(RCh, SizeOf(WideChar));
        vColValueSize := Integer(RCh)  + SizeOf(WideChar) - Integer(fOdbcHostVarAddress.ptrAnsiChar);
        if vColValueSize > 0 then
        begin
          Move(fOdbcHostVarAddress.ptrAnsiChar^, PWideChar(Value)^, vColValueSize);
          PWideChar(Value)[vColValueSize div SizeOf(WideChar)] := cNullWideChar;
        end
        else
          PWideChar(Value)^ := cNullWideChar;
      end
      else
      begin
        Move(fOdbcHostVarAddress.ptrWideChar^, PWideChar(Value)^, vColValueSize);
        PWideChar(Value)[vColValueSize div SizeOf(WideChar)] := cNullWideChar;
      end;
    end;
    Result := DBXERR_NONE;
  end;
  {$IFDEF _TRACE_CALLS_}
    except on e:exception do begin LogExceptProc('TSqlCursorOdbc3.getWideString', e);  raise; end; end;
    finally LogExitProc('TSqlCursorOdbc3.getWideString', ['Value =', PAnsiChar(Value), 'IsBlank =', IsBlank]); end;
  {$ENDIF _TRACE_CALLS_}
end;

{ TSQLMetaDataOdbc3 }

function TSQLMetaDataOdbc3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  if Error <> nil then
  begin
    StringsToWStr(fMetaDataErrorLines, Error);
    fMetaDataErrorLines.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
end;

function TSQLMetaDataOdbc3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  ErrorLen := WideStringLengthFromStrings(fMetaDataErrorLines) + 1;
  Result := DBXERR_NONE;
end;

function TSQLMetaDataOdbc3.GetOption(eDOption: TSQLMetaDataOption; PropValue: Pointer; MaxLength: Smallint; out Length: Smallint): SQLResult;
var
  xeDOption: TXSQLMetaDataOption absolute eDOption;
  sOptionValue: AnsiString;
  iMaxChars: Integer;
begin
  if (xeDOption in cXSQLMetaDataOptionStringTypes) and (PropValue <> nil) and (MaxLength >= SizeOf(WideChar)) then
  begin
    iMaxChars := MaxLength div SizeOf(WideChar) + 1;
    SetLength(sOptionValue, iMaxChars);
    FillChar(sOptionValue[1], iMaxChars, #0);
    Result := inherited GetOption(eDOption, PAnsiChar(sOptionValue), iMaxChars, Length);
    if Result = DBXERR_NONE then
    begin
      SetLength(sOptionValue, StrLen(PAnsiChar(sOptionValue)));
      StringToWideChar(sOptionValue, PWideChar(PropValue), iMaxChars);
    end;
  end
  else
    Result := inherited GetOption(eDOption, PropValue, MaxLength, Length);
end;

function TSQLMetaDataOdbc3.SetOption(eDOption: TSQLMetaDataOption; PropValue: Integer): SQLResult;
var
  xeDOption: TXSQLMetaDataOption absolute eDOption;
  sOptionValue: AnsiString;
begin
  if (xeDOption in cXSQLMetaDataOptionStringTypes) and (PropValue <> 0) then
  begin
    sOptionValue := WideString(PWideChar(PropValue));
    Result := inherited SetOption(eDOption, Integer(PAnsiChar(sOptionValue)));
  end
  else
    Result := inherited SetOption(eDOption, PropValue);
end;

function TSQLMetaDataOdbc3.getObjectList(eObjType: TSQLObjectType; out Cursor: ISQLCursor30): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSQLMetaDataOdbc3.getTables(TableName: PWideChar; TableType: Longword; out Cursor: ISQLCursor30): SQLResult;
var
  vCursor: ISQLCursor25 absolute Cursor;
  sOptionValue: AnsiString;
begin
  if (TableName = nil) or (Trim(WideString(TableName)) = '') then
    Result := inherited getTables(nil, TableType, vCursor)
  else
  begin
    sOptionValue := WideString(PWideChar(TableName));
    Result := inherited getTables(PAnsiChar(sOptionValue), TableType, vCursor)
  end;
end;

function TSQLMetaDataOdbc3.getProcedures(ProcedureName: PWideChar; ProcType: Longword; out Cursor: ISQLCursor30): SQLResult;
var
  vCursor: ISQLCursor25 absolute Cursor;
  sOptionValue: AnsiString;
begin
  if (ProcedureName = nil) or (Trim(WideString(ProcedureName)) = '') then
    Result := inherited getProcedures(nil, ProcType, vCursor)
  else
  begin
    sOptionValue := WideString(PWideChar(ProcedureName));
    Result := inherited getProcedures(PAnsiChar(sOptionValue), ProcType, vCursor)
  end;
end;

function TSQLMetaDataOdbc3.getColumns(TableName, ColumnName: PWideChar; ColType: Longword; out Cursor: ISQLCursor30): SQLResult;
var
  vCursor: ISQLCursor25 absolute Cursor;
  sTableName, sColumnName: AnsiString;
begin
  if (TableName <> nil) then
    sTableName := WideString(TableName);
  if (ColumnName <> nil) then
    sColumnName := WideString(ColumnName);
  Result := inherited getColumns(PAnsiChar(sTableName), PAnsiChar(sColumnName), ColType, vCursor)
end;

function TSQLMetaDataOdbc3.getIndices(TableName: PWideChar; IndexType: Longword; out Cursor: ISQLCursor30): SQLResult;
var
  vCursor: ISQLCursor25 absolute Cursor;
  sTableName: AnsiString;
begin
  if (TableName <> nil) then
    sTableName := WideString(TableName);
  Result := inherited getIndices(PAnsiChar(sTableName), IndexType, vCursor)
end;

function TSQLMetaDataOdbc3.getProcedureParams(ProcName, ParamName: PWideChar; out Cursor: ISQLCursor30): SQLResult;
var
  vCursor: ISQLCursor25 absolute Cursor;
  sProcName, sParamName: AnsiString;
begin
  if (ProcName <> nil) then
    sProcName := WideString(ProcName);
  if (ParamName <> nil) then
    sParamName := WideString(ParamName);
  Result := inherited getProcedureParams(PAnsiChar(sProcName), PAnsiChar(sParamName), vCursor)
end;

{ TSqlCursorMetaDataTables3 }

function TSqlCursorMetaDataTables3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  if Error <> nil then
  begin
    StringsToWStr(fSqlCursorErrorMsg, Error);
    fSqlCursorErrorMsg.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
end;

function TSqlCursorMetaDataTables3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  ErrorLen := WideStringLengthFromStrings(fSqlCursorErrorMsg) + 1;
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaDataTables3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  StringToWStr(PAnsiChar(fColumnNames[ColumnNumber - 1]), pColumnName);
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaDataTables3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSqlCursorMetaDataTables3.getString(ColumnNumber: Word; Value: PChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Value, IsBlank);
end;

function TSqlCursorMetaDataTables3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
var
  iMaxChars: Longword;
  sValue: AnsiString;
begin
  Result := DBXERR_NONE;
  iMaxChars := 0;
  getColumnLength(ColumnNumber, iMaxChars);
  if iMaxChars > 0 then
  begin
    SetLength(sValue, iMaxChars);
    FillChar(sValue[1], iMaxChars, #0);
    Result := inherited getString(ColumnNumber, PAnsiChar(sValue), IsBlank);
    if (Result = DBXERR_NONE) and (not IsBlank) then
      StringToWideChar(sValue, Value, iMaxChars);
  end
  else
    IsBlank := True;
end;

{ TSqlCursorMetaDataColumns3 }

function TSqlCursorMetaDataColumns3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  StringToWStr(PAnsiChar(fColumnNames[ColumnNumber - 1]), pColumnName);
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaDataColumns3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  if Error <> nil then
  begin
    StringsToWStr(fSqlCursorErrorMsg, Error);
    fSqlCursorErrorMsg.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
end;

function TSqlCursorMetaDataColumns3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  ErrorLen := WideStringLengthFromStrings(fSqlCursorErrorMsg) + 1;
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaDataColumns3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSqlCursorMetaDataColumns3.getString(ColumnNumber: Word; Value: PChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Value, IsBlank);
end;

function TSqlCursorMetaDataColumns3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
var
  iMaxChars: Longword;
  sValue: AnsiString;
begin
  Result := DBXERR_NONE;
  iMaxChars := 0;
  getColumnLength(ColumnNumber, iMaxChars);
  if iMaxChars > 0 then
  begin
    SetLength(sValue, iMaxChars);
    FillChar(sValue[1], iMaxChars, #0);
    Result := inherited getString(ColumnNumber, PAnsiChar(sValue), IsBlank);
    if (Result = DBXERR_NONE) and (not IsBlank) then
      StringToWideChar(sValue, Value, iMaxChars);
  end
  else
    IsBlank := True;
end;

{ TSqlCursorMetaDataIndexes3 }

function TSqlCursorMetaDataIndexes3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  StringToWStr(PAnsiChar(fColumnNames[ColumnNumber - 1]), pColumnName);
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaDataIndexes3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  if Error <> nil then
  begin
    StringsToWStr(fSqlCursorErrorMsg, Error);
    fSqlCursorErrorMsg.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
end;

function TSqlCursorMetaDataIndexes3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  ErrorLen := WideStringLengthFromStrings(fSqlCursorErrorMsg) + 1;
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaDataIndexes3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSqlCursorMetaDataIndexes3.getString(ColumnNumber: Word; Value: PChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Value, IsBlank);
end;

function TSqlCursorMetaDataIndexes3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
var
  iMaxChars: Longword;
  sValue: AnsiString;
begin
  Result := DBXERR_NONE;
  iMaxChars := 0;
  getColumnLength(ColumnNumber, iMaxChars);
  if iMaxChars > 0 then
  begin
    SetLength(sValue, iMaxChars);
    FillChar(sValue[1], iMaxChars, #0);
    Result := inherited getString(ColumnNumber, PAnsiChar(sValue), IsBlank);
    if (Result = DBXERR_NONE) and (not IsBlank) then
      StringToWideChar(sValue, Value, iMaxChars);
  end
  else
    IsBlank := True;
end;

{ TSqlCursorMetaDataProcedures3 }

function TSqlCursorMetaDataProcedures3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  StringToWStr(PAnsiChar(fColumnNames[ColumnNumber - 1]), pColumnName);
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaDataProcedures3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  if Error <> nil then
  begin
    StringsToWStr(fSqlCursorErrorMsg, Error);
    fSqlCursorErrorMsg.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
end;

function TSqlCursorMetaDataProcedures3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  ErrorLen := WideStringLengthFromStrings(fSqlCursorErrorMsg) + 1;
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaDataProcedures3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSqlCursorMetaDataProcedures3.getString(ColumnNumber: Word; Value: PChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Value, IsBlank);
end;

function TSqlCursorMetaDataProcedures3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
var
  iMaxChars: Longword;
  sValue: AnsiString;
begin
  Result := DBXERR_NONE;
  iMaxChars := 0;
  getColumnLength(ColumnNumber, iMaxChars);
  if iMaxChars > 0 then
  begin
    SetLength(sValue, iMaxChars);
    FillChar(sValue[1], iMaxChars, #0);
    Result := inherited getString(ColumnNumber, PAnsiChar(sValue), IsBlank);
    if (Result = DBXERR_NONE) and (not IsBlank) then
      StringToWideChar(sValue, Value, iMaxChars);
  end
  else
    IsBlank := True;
end;

{ TSqlCursorMetaDataProcedureParams3 }

function TSqlCursorMetaDataProcedureParams3.getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult;
begin
  StringToWStr(PAnsiChar(fColumnNames[ColumnNumber - 1]), pColumnName);
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaDataProcedureParams3.getErrorMessage(Error: PWideChar): SQLResult;
begin
  if Error <> nil then
  begin
    StringsToWStr(fSqlCursorErrorMsg, Error);
    fSqlCursorErrorMsg.Clear;
    Result := DBXERR_NONE;
  end
  else
    Result := DBXERR_INVALIDPARAM;
end;

function TSqlCursorMetaDataProcedureParams3.getErrorMessageLen(out ErrorLen: Smallint): SQLResult;
begin
  ErrorLen := WideStringLengthFromStrings(fSqlCursorErrorMsg) + 1;
  Result := DBXERR_NONE;
end;

function TSqlCursorMetaDataProcedureParams3.getInt64(ColumnNumber: Word; Value: Pointer; var IsBlank: LongBool): SQLResult;
begin
  Result := DBXERR_NOTSUPPORTED;
end;

function TSqlCursorMetaDataProcedureParams3.getString(ColumnNumber: Word; Value: PChar; var IsBlank: LongBool): SQLResult;
begin
  Result := inherited getString(ColumnNumber, Value, IsBlank);
end;

function TSqlCursorMetaDataProcedureParams3.getWideString(ColumnNumber: Word; Value: PWideChar; var IsBlank: LongBool): SQLResult;
var
  iMaxChars: Longword;
  sValue: AnsiString;
begin
  Result := DBXERR_NONE;
  iMaxChars := 0;
  getColumnLength(ColumnNumber, iMaxChars);
  if iMaxChars > 0 then
  begin
    SetLength(sValue, iMaxChars);
    FillChar(sValue[1], iMaxChars, #0);
    Result := inherited getString(ColumnNumber, PAnsiChar(sValue), IsBlank);
    if (Result = DBXERR_NONE) and (not IsBlank) then
      StringToWideChar(sValue, Value, iMaxChars);
  end
  else
    IsBlank := True;
end;

procedure DoRegisterDbXpressLibW();
begin
  {$IFNDEF _D11UP_}
    {$IFDEF _D10UP_}
      SqlExpr.RegisterDbXpressLib(@getSQLDriverODBCW);
    {$ELSE}
      SqlExpr.RegisterDbXpressLib(@getSQLDriverODBCAW);
    {$ENDIF}
  {$ENDIF}
end;

procedure DoRegisterDbXpressLibWA();
begin
  {$IFNDEF _D11UP_}
    {$IFDEF _D10UP_}
      SqlExpr.RegisterDbXpressLib(@getSQLDriverODBCWA);
    {$ELSE}
      SqlExpr.RegisterDbXpressLib(@getSQLDriverODBC);
    {$ENDIF}
  {$ENDIF}
end;

initialization
begin
  cConnectionOptionsDefault3 := cConnectionOptionsDefault;
  cConnectionOptionsDefault3[coMapInt64ToBcd] := osOff;
  cConnectionOptionsDefault3[coEnableUnicode] := osOn;

  RegisterDbXpressLibW := DoRegisterDbXpressLibW;
  RegisterDbXpressLibWA := DoRegisterDbXpressLibWA;
  // This allows option of static linking the DbExpress driver into your app
{$IFDEF MSWINDOWS}
  {$IFNDEF _DENT_}
    {$IFNDEF _D11UP_}
      DoRegisterDbXpressLibW();
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

end;

end.
