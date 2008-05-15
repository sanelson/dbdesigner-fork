{ *************************************************************************** }
{                                                                             }
{ Kylix and Delphi Cross-Platform Visual Component Library                    }
{                                                                             }
{ Copyright (c) 1999, 2001 Borland Software Corporation                       }
{                                                                             }
{ *************************************************************************** }


unit DBXpress;

{$Z+}

interface

{$IFDEF MSWINDOWS}
uses Windows, DBCommonTypes;
{$ENDIF}
{$IFDEF LINUX}
uses Libc;
{$ENDIF}

const

  DBXDRIVERVERSION30  = '3.0';
  DBXPRODUCTVERSION30 = '3.0';

  MAXNAMELEN       = 64;
  NAMEBUFLEN       = 32;
  SQL_ERROR        = -1;
  SQL_NULL_DATA    = 100;

  DBXERR_NONE                    = $0000;
  DBXERR_WARNING                 = $0001;
  DBXERR_NOMEMORY                = $0002;
  DBXERR_INVALIDFLDTYPE          = $0003;
  DBXERR_INVALIDHNDL             = $0004;
  DBXERR_NOTSUPPORTED            = $0005;
  DBXERR_INVALIDTIME             = $0006;
  DBXERR_INVALIDXLATION          = $0007;
  DBXERR_OUTOFRANGE              = $0008;
  DBXERR_INVALIDPARAM            = $0009;
  DBXERR_EOF                     = $000A;
  DBXERR_SQLPARAMNOTSET          = $000B;
  DBXERR_INVALIDUSRPASS          = $000C;
  DBXERR_INVALIDPRECISION        = $000D;
  DBXERR_INVALIDLEN              = $000E;
  DBXERR_INVALIDTXNISOLEVEL      = $000F;
  DBXERR_INVALIDTXNID            = $0010;
  DBXERR_DUPLICATETXNID          = $0011;
  DBXERR_DRIVERRESTRICTED        = $0012;
  DBXERR_LOCALTRANSACTIVE        = $0013;
  DBXERR_MULTIPLETRANSNOTENABLED = $0014;
  DBXERR_CONNECTIONFAILED        = $0015;
  DBXERR_DRIVERINITFAILED        = $0016;
  DBXERR_OPTLOCKFAILED           = $0017;
  DBXERR_INVALIDREF              = $0018;
  DBXERR_NOTABLE                 = $0019;
  DBXERR_MISSINGPARAMINSQL       = $001A;
  DBXERR_NOTIMPLEMENT            = $001B;
  DBXERR_DRIVERINCOMPATIBLE      = $001C;

  DBXERR_NODATA                  = $0064;
  DBX_MAXSTATICERRORS            = $0064;

  DBXERR_SQLERROR                = $0065;


// traceFlags

    trUNKNOWN   = $0000;
    trQPREPARE  = $0001;             { prepared query statements }
    trQEXECUTE  = $0002;             { executed query statements }
    trERROR     = $0004;             { vendor errors }
    trSTMT      = $0008;             { statement ops (i.e. allocate, free) }
    trCONNECT   = $0010;             { connect / disconnect }
    trTRANSACT  = $0020;             { transaction }
    trBLOB      = $0040;             { blob i/o }
    trMISC      = $0080;             { misc. }
    trVENDOR    = $0100;             { vendor calls }
    trDATAIN    = $0200;             { parameter bound data }
    trDATAOUT   = $0400;             { trace fetched data }

// eSQLTableType
    eSQLTable       = $0001;
    eSQLView        = $0002;
    eSQLSystemTable = $0004;
    eSQLSynonym     = $0008;
    eSQLTempTable   = $0010;
    eSQLLocal       = $0020;

// eSQLProcType
    eSQLProcedure   = $0001;
    eSQLFunction    = $0002;
    eSQLPackage     = $0004;
    eSQLSysProcedure = $0008;

// eSQLColType
    eSQLRowId       = $0001;
    eSQLRowVersion  = $0002;
    eSQLAutoIncr    = $0004;
    eSQLDefault     = $0008;

// eSQLIndexType
    eSQLNonUnique   = $0001;
    eSQLUnique      = $0002;
    eSQLPrimaryKey  = $0004;

{ Field Types (Logical) }

  fldUNKNOWN         = 0;
  fldZSTRING         = 1;               { Null terminated string }
  fldDATE            = 2;               { Date     (32 bit) }
  fldBLOB            = 3;               { Blob }
  fldBOOL            = 4;               { Boolean  (16 bit) }
  fldINT16           = 5;               { 16 bit signed number }
  fldINT32           = 6;               { 32 bit signed number }
  fldFLOAT           = 7;               { 64 bit floating point }
  fldBCD             = 8;               { BCD }
  fldBYTES           = 9;               { Fixed number of bytes }
  fldTIME            = 10;              { Time        (32 bit) }
  fldTIMESTAMP       = 11;              { Time-stamp  (64 bit) }
  fldUINT16          = 12;              { Unsigned 16 bit Integer }
  fldUINT32          = 13;              { Unsigned 32 bit Integer }
  fldFLOATIEEE       = 14;              { 80-bit IEEE float }
  fldVARBYTES        = 15;              { Length prefixed var bytes }
  fldLOCKINFO        = 16;              { Look for LOCKINFO typedef }
  fldCURSOR          = 17;              { For Oracle Cursor type }
  fldINT64           = 18;              { 64 bit signed number }
  fldUINT64          = 19;              { Unsigned 64 bit Integer }
  fldADT             = 20;              { Abstract datatype (structure) }
  fldARRAY           = 21;              { Array field type }
  fldREF             = 22;              { Reference to ADT }
  fldTABLE           = 23;              { Nested table (reference) }
  fldDATETIME        = 24;              { DateTime structure field }
  fldFMTBCD          = 25;              { BCD Variant type: required by Midas, same as BCD for DBExpress}
  fldWIDESTRING      = 26;              { UCS2 null terminated string }

  MAXLOGFLDTYPES     = 27;              { Number of logical fieldtypes }

{ Sub Types (Logical) }

{ fldFLOAT subtype }

  fldstMONEY         = 21;              { Money }

{ fldBLOB subtypes }

  fldstMEMO          = 22;              { Text Memo }
  fldstBINARY        = 23;              { Binary data }
  fldstFMTMEMO       = 24;              { Formatted Text }
  fldstOLEOBJ        = 25;              { OLE object (Paradox) }
  fldstGRAPHIC       = 26;              { Graphics object }
  fldstDBSOLEOBJ     = 27;              { dBASE OLE object }
  fldstTYPEDBINARY   = 28;              { Typed Binary data }
  fldstACCOLEOBJ     = 30;              { Access OLE object }
  fldstWIDEMEMO      = 32;              { WideString Memo }
  fldstHMEMO         = 33;              { CLOB }
  fldstHBINARY       = 34;              { BLOB }
  fldstBFILE         = 36;              { BFILE }

{ fldZSTRING / fldWIDESTRING subtype }

  fldstPASSWORD      = 1;               { Password }
  fldstFIXED         = 31;              { CHAR type }

{ fldINT32 subtype }

  fldstAUTOINC       = 29;

{ fldADT subtype }

  fldstADTNestedTable = 35;             { ADT for nested table (has no name) }

{ fldDATE subtype }
  fldstADTDATE       = 37;              { DATE (OCIDate) with in an ADT }

  fldstORATIMESTAMP  = 39;              { ORACLE TIMESTAMP }
  fldstORAINTERVAL   = 40;              { ORACLE INTERVAL }

type

  DBINAME32             = packed array [0..31] of Char; { holds a name }
  DBINAME128            = packed array [0..128] of WideChar; { holds a name }

  PFLDDesc30 = ^FLDDesc30;
  FLDDesc30 = packed record               { Field Descriptor }
    iFldNum         : Word;             { Field number (1..n) }
    szName          : DBINAME128;          { Field name }
    iFldType        : Word;             { Field type }
    iSubType        : Word;             { Field subtype (if applicable) }
    iUnits1         : SmallInt;         { Number of Chars, digits etc }
    iUnits2         : SmallInt;         { Decimal places etc. }
    iOffset         : Word;             { Offset in the record (computed) }
    iLen            : LongWord;             { Length in bytes (computed) }
    iNullOffset     : Word;             { For Null bits (computed) }
    efldvVchk       : FLDVchk;          { Field Has vcheck (computed) }
    efldrRights     : FLDRights;        { Field Rights (computed) }
    bCalcField      : WordBool;         { Is Calculated field (computed) }
    iUnUsed         : packed array [0..1] of Word;
  end;
  SQLFLDDesc30 = FLDDesc30;

  PFLDDesc25 = ^FLDDesc25;
  FLDDesc25 = packed record               { Field Descriptor }
    iFldNum         : Word;             { Field number (1..n) }
    szName          : DBINAME32;          { Field name }
    iFldType        : Word;             { Field type }
    iSubType        : Word;             { Field subtype (if applicable) }
    iUnits1         : SmallInt;         { Number of Chars, digits etc }
    iUnits2         : SmallInt;         { Decimal places etc. }
    iOffset         : Word;             { Offset in the record (computed) }
    iLen            : LongWord;             { Length in bytes (computed) }
    iNullOffset     : Word;             { For Null bits (computed) }
    efldvVchk       : FLDVchk;          { Field Has vcheck (computed) }
    efldrRights     : FLDRights;        { Field Rights (computed) }
    bCalcField      : WordBool;         { Is Calculated field (computed) }
    iUnUsed         : packed array [0..1] of Word;
  end;
  SQLFLDDesc25 = FLDDesc25;

  pObjAttrDesc30 = ^ObjAttrDesc30;
  ObjAttrDesc30 = packed record
    iFldNum    : Word;                  { Field id }
    pszAttributeName: PWideChar;        { Object attribute name }
  end;
  SQLObjAttrDesc30 = ObjAttrDesc30;

  pObjAttrDesc25 = ^ObjAttrDesc25;
  ObjAttrDesc25 = packed record
    iFldNum    : Word;                  { Field id }
    pszAttributeName: PChar;        { Object attribute name }
  end;
  SQLObjAttrDesc25 = ObjAttrDesc25;

  pObjTypeDesc30 = ^ObjTypeDesc30;
  ObjTypeDesc30 = packed record
    iFldNum    : Word;                  { Field id }
    szTypeName : DBINAME128;               { Object type name }
  end;
  SQLObjTypeDesc30 = ObjTypeDesc30;

  pObjTypeDesc25 = ^ObjTypeDesc25;
  ObjTypeDesc25 = packed record
    iFldNum    : Word;                  { Field id }
    szTypeName : DBINAME32;               { Object type name }
  end;
  SQLObjTypeDesc25 = ObjTypeDesc25;

  pObjParentDesc30 = ^ObjParentDesc30;
  ObjParentDesc30 = packed record
    iFldNum    : Word;                  { Field id }
    iParentFldNum : Word;               { Parent Field id }
  end;

  pObjParentDesc25 = ^ObjParentDesc25;
  ObjParentDesc25 = packed record
    iFldNum    : Word;                  { Field id }
    iParentFldNum : Word;               { Parent Field id }
  end;

  SQLResult      = Word;

var

___getSQLDriver : function(VendorLib, SResourceFile: PChar; out pDriver): SQLResult; stdcall;

type

TSQLDriverOption = (
      eDrvBlobSize, eDrvCallBack, eDrvCallBackInfo, eDrvRestrict,
      eDrvVersion, eDrvProductVersion);

TSQLConnectionOption = (
      eConnAutoCommit, eConnBlockingMode, eConnBlobSize, eConnRoleName,
      eConnWaitOnLocks, eConnCommitRetain, eConnTxnIsoLevel,
      eConnNativeHandle, eConnServerVersion, eConnCallBack, eConnHostName,
      eConnDatabaseName, eConnCallBackInfo, eConnObjectMode,
      eConnMaxActiveComm, eConnServerCharSet, eConnSqlDialect,
      eConnRollbackRetain, eConnObjectQuoteChar, eConnConnectionName,
      eConnOSAuthentication, eConnSupportsTransaction, eConnMultipleTransaction,
      eConnServerPort,eConnOnLine, eConnTrimChar, eConnQualifiedName,
      eConnCatalogName, eConnSchemaName, eConnObjectName, eConnQuotedObjectName,
      eConnCustomInfo, eConnTimeOut, eConnConnectionString, eConnTDSPacketSize,
      eConnClientHostName, eConnClientAppName, eConnCompressed, eConnEncrypted,
      eConnPrepareSQL, eConnDecimalSeparator);

TSQLCommandOption = (
      eCommRowsetSize, eCommBlobSize, eCommBlockRead, eCommBlockWrite,
      eCommParamCount, eCommNativeHandle, eCommCursorName, eCommStoredProc,
      eCommSQLDialect, eCommTransactionID, eCommPackageName, eCommTrimChar,
      eCommQualifiedName, eCommCatalogName, eCommSchemaName, eCommObjectName,
      eCommQuotedObjectName, eCommPrepareSQL, eCommDecimalSeparator);

TSQLCursorOption = (eCurObjectAttrName, eCurObjectTypeName, eCurParentFieldID);

TSQLMetaDataOption = (eMetaCatalogName, eMetaSchemaName, eMetaDatabaseName,
                  eMetaDatabaseVersion, eMetaTransactionIsoLevel, eMetaSupportsTransaction,
                  eMetaMaxObjectNameLength, eMetaMaxColumnsInTable, eMetaMaxColumnsInSelect,
                  eMetaMaxRowSize, eMetaMaxSQLLength, eMetaObjectQuoteChar,
                  eMetaSQLEscapeChar, eMetaProcSupportsCursor, eMetaProcSupportsCursors,
                  eMetaSupportsTransactions, eMetaPackageName, eMetaDefaultSchemaName);

TSQLObjectType = (eObjTypeDatabase, eObjTypeDataType, eObjTypeTable,
                  eObjTypeView, eObjTypeSynonym, eObjTypeProcedure, eObjTypeUser,
                  eObjTypeRole, eObjTypeUDT, eObjTypePackage);

TTransIsolationLevel = (xilREADCOMMITTED, xilREPEATABLEREAD, xilDIRTYREAD, xilCUSTOM);

  pTTransactionDesc = ^TTransactionDesc;
  TTransactionDesc = packed record
    TransactionID    : LongWord;             { Transaction id }
    GlobalID         : LongWord;             { Global transaction id }
    IsolationLevel   : TTransIsolationLevel; {Transaction Isolation level}
    CustomIsolation  : LongWord;             { DB specific custom isolation }
  end;

TSTMTParamType = (paramUNKNOWN, paramIN, paramOUT, paramINOUT, paramRET);

   { Function Result }
   TypedEnum      = Integer;
   SQLDATE        = Longint;
   SQLTIME        = Longint;

  pTRACECat = ^TRACECat;                { trace categories }
  TRACECat = TypedEnum;


  TSQLTraceFlag = (traceQPREPARE, traceQEXECUTE, traceERROR,
    traceSTMT, traceCONNECT, traceTRANSACT, traceBLOB, traceMISC, traceVENDOR,
    traceDATAIN, traceDATAOUT);

  TSQLTraceFlags = set of TSQLTraceFlag;

type

{ forward declarations }

   ISQLDriver = interface;

   ISQLConnection30 = interface;
   ISQLCommand30 = interface;
   ISQLCursor30 = interface;
   ISQLMetaData30 = interface;

   ISQLConnection25 = interface;
   ISQLCommand25 = interface;
   ISQLCursor25 = interface;
   ISQLMetaData25 = interface;


   ISQLConnection = interface;
   ISQLCommand = interface;
   ISQLCursor = interface;
   ISQLMetaData = interface;

{ Trace callback }

  TSQLCallbackEvent = function(
     CallType: TRACECat;
     CBInfo: Pointer): CBRType; stdcall;

{ class Pointers }

  pSQLDriver     = ^ISQLDriver;
  pSQLConnection = ^ISQLConnection;
  pSQLCommand    = ^ISQLCommand;
  pSQLCursor     = ^ISQLCursor;

ISQLDriver = interface
   function getSQLConnection(out pConn: ISQLConnection): SQLResult; stdcall;
   function SetOption(eDOption: TSQLDriverOption;
                     PropValue: LongInt): SQLResult; stdcall;
   function GetOption(eDOption: TSQLDriverOption; PropValue: Pointer;
                     MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
end;

{ Base interface }

ISQLConnection = interface
end;

ISQLCommand = interface
end;

ISQLCursor = interface
end;

ISQLMetaData = interface
end;

{ Ver 3.0 interface }
ISQLConnection30 = interface(ISQLConnection)
   function connect(): SQLResult; stdcall; overload;
   function connect(ServerName: PWideChar; UserName: PWideChar;
                          Password: PWideChar): SQLResult; stdcall; overload;
   function disconnect: SQLResult; stdcall;
   function getSQLCommand(out pComm: ISQLCommand30): SQLResult; stdcall;
   function getSQLMetaData(out pMetaData: ISQLMetaData30): SQLResult; stdcall;
   function SetOption(eConnectOption: TSQLConnectionOption;
            lValue: LongInt): SQLResult; stdcall;
   function GetOption(eDOption: TSQLConnectionOption; PropValue: Pointer;
            MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
   function beginTransaction(TranID: LongWord): SQLResult; stdcall;
   function commit(TranID: LongWord): SQLResult; stdcall;
   function rollback(TranID: LongWord): SQLResult; stdcall;
   function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
   function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
end;

ISQLCommand30 = interface(ISQLCommand)
   function SetOption(
      eSqlCommandOption: TSQLCommandOption;
      ulValue: Integer): SQLResult; stdcall;
   function GetOption(eSqlCommandOption: TSQLCommandOption;
      PropValue: Pointer;
      MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
   function setParameter(
      ulParameter: Word ;
      ulChildPos: Word ;
      eParamType: TSTMTParamType ;
      uLogType: Word;
      uSubType: Word;
      iPrecision: Integer;
      iScale: Integer;
      Length: LongWord ;
      pBuffer: Pointer;
      lInd: Integer): SQLResult; stdcall;
   function getParameter(ParameterNumber: Word; ulChildPos: Word; Value: Pointer;
      Length: Integer; var IsBlank: Integer): SQLResult; stdcall;
   function prepare(SQL: PWideChar; ParamCount: Word): SQLResult; stdcall;
   function execute(var Cursor: ISQLCursor30): SQLResult; stdcall;
   function executeImmediate(SQL: PWideChar; var Cursor: ISQLCursor30): SQLResult; stdcall;
   function getNextCursor(var Cursor: ISQLCursor30): SQLResult; stdcall;
   function getRowsAffected(var Rows: LongWord): SQLResult; stdcall;
   function close: SQLResult; stdcall;
   function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
   function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
end;

ISQLCursor30 = interface(ISQLCursor)
   function SetOption(eOption: TSQLCursorOption;
                     PropValue: LongInt): SQLResult; stdcall;
   function GetOption(eOption: TSQLCursorOption; PropValue: Pointer;
                     MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
   function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
   function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
   function getColumnCount(var pColumns: Word): SQLResult; stdcall;
   function getColumnNameLength(
      ColumnNumber: Word;
      var pLen: Word): SQLResult; stdcall;
   function getColumnName(ColumnNumber: Word; pColumnName: PWideChar): SQLResult; stdcall;
   function getColumnType(ColumnNumber: Word; var puType: Word;
      var puSubType: Word): SQLResult; stdcall;
   function  getColumnLength(ColumnNumber: Word; var pLength: LongWord): SQLResult; stdcall;
   function getColumnPrecision(ColumnNumber: Word;
      var piPrecision: SmallInt): SQLResult; stdcall;
   function getColumnScale(ColumnNumber: Word; var piScale: SmallInt): SQLResult; stdcall;
   function isNullable(ColumnNumber: Word; var Nullable: LongBool): SQLResult; stdcall;
   function isAutoIncrement(ColumnNumber: Word; var AutoIncr: LongBool): SQLResult; stdcall;
   function isReadOnly(ColumnNumber: Word; var ReadOnly: LongBool): SQLResult; stdcall;
   function isSearchable(ColumnNumber: Word; var Searchable: LongBool): SQLResult; stdcall;
   function isBlobSizeExact(ColumnNumber: Word; var IsExact: LongBool): SQLResult; stdcall;
   function next: SQLResult; stdcall;
   function getString(ColumnNumber: Word; Value: PChar;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getWideString(ColumnNumber: Word; Value: PWideChar;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getShort(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getLong(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getInt64(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getDouble(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getBcd(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getTimeStamp(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getTime(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getDate(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getBytes(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getBlobSize(ColumnNumber: Word; var Length: LongWord;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getBlob(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool; Length: LongWord): SQLResult; stdcall;
end;

ISQLMetaData30 = interface(ISQLMetaData)
   function SetOption(eDOption: TSQLMetaDataOption;
                     PropValue: LongInt): SQLResult; stdcall;
   function GetOption(eDOption: TSQLMetaDataOption; PropValue: Pointer;
                     MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
   function getObjectList(eObjType: TSQLObjectType; out Cursor: ISQLCursor30):
                     SQLResult; stdcall;
   function getTables(TableName: PWideChar; TableType: LongWord;
                     out Cursor: ISQLCursor30): SQLResult; stdcall;
   function getProcedures(ProcedureName: PWideChar; ProcType: LongWord;
                     out Cursor: ISQLCursor30): SQLResult; stdcall;
   function getColumns(TableName: PWideChar; ColumnName: PWideChar;
                     ColType: LongWord; Out Cursor: ISQLCursor30): SQLResult; stdcall;
   function getProcedureParams(ProcName: PWideChar; ParamName: PWideChar;
                     out Cursor: ISQLCursor30): SQLResult; stdcall;
   function getIndices(TableName: PWideChar; IndexType: LongWord;
                     out Cursor: ISQLCursor30): SQLResult; stdcall;
   function getErrorMessage(Error: PWideChar): SQLResult; overload; stdcall;
   function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
end;

{ Ver 2.x interface }
ISQLConnection25 = interface(ISQLConnection)
   function connect(ServerName: PChar; UserName: PChar;
                          Password: PChar): SQLResult; stdcall;
   function disconnect: SQLResult; stdcall;
   function getSQLCommand(out pComm: ISQLCommand25): SQLResult; stdcall;
   function getSQLMetaData(out pMetaData: ISQLMetaData25): SQLResult; stdcall;
   function SetOption(eConnectOption: TSQLConnectionOption;
            lValue: LongInt): SQLResult; stdcall;
   function GetOption(eDOption: TSQLConnectionOption; PropValue: Pointer;
            MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
   function beginTransaction(TranID: LongWord): SQLResult; stdcall;
   function commit(TranID: LongWord): SQLResult; stdcall;
   function rollback(TranID: LongWord): SQLResult; stdcall;
   function getErrorMessage(Error: PChar): SQLResult; overload; stdcall;
   function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
end;

ISQLCommand25 = interface(ISQLCommand)
   function SetOption(
      eSqlCommandOption: TSQLCommandOption;
      ulValue: Integer): SQLResult; stdcall;
   function GetOption(eSqlCommandOption: TSQLCommandOption;
      PropValue: Pointer;
      MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
   function setParameter(
      ulParameter: Word ;
      ulChildPos: Word ;
      eParamType: TSTMTParamType ;
      uLogType: Word;
      uSubType: Word;
      iPrecision: Integer;
      iScale: Integer;
      Length: LongWord ;
      pBuffer: Pointer;
      lInd: Integer): SQLResult; stdcall;
   function getParameter(ParameterNumber: Word; ulChildPos: Word; Value: Pointer;
      Length: Integer; var IsBlank: Integer): SQLResult; stdcall;
   function prepare(SQL: PChar; ParamCount: Word): SQLResult; stdcall;
   function execute(var Cursor: ISQLCursor25): SQLResult; stdcall;
   function executeImmediate(SQL: PChar; var Cursor: ISQLCursor25): SQLResult; stdcall;
   function getNextCursor(var Cursor: ISQLCursor25): SQLResult; stdcall;
   function getRowsAffected(var Rows: LongWord): SQLResult; stdcall;
   function close: SQLResult; stdcall;
   function getErrorMessage(Error: PChar): SQLResult; overload; stdcall;
   function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
end;

ISQLCursor25 = interface(ISQLCursor)
   function SetOption(eOption: TSQLCursorOption;
                     PropValue: LongInt): SQLResult; stdcall;
   function GetOption(eOption: TSQLCursorOption; PropValue: Pointer;
                     MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
   function getErrorMessage(Error: PChar): SQLResult; overload; stdcall;
   function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
   function getColumnCount(var pColumns: Word): SQLResult; stdcall;
   function getColumnNameLength(
      ColumnNumber: Word;
      var pLen: Word): SQLResult; stdcall;
   function getColumnName(ColumnNumber: Word; pColumnName: PChar): SQLResult; stdcall;
   function getColumnType(ColumnNumber: Word; var puType: Word;
      var puSubType: Word): SQLResult; stdcall;
   function  getColumnLength(ColumnNumber: Word; var pLength: LongWord): SQLResult; stdcall;
   function getColumnPrecision(ColumnNumber: Word;
      var piPrecision: SmallInt): SQLResult; stdcall;
   function getColumnScale(ColumnNumber: Word; var piScale: SmallInt): SQLResult; stdcall;
   function isNullable(ColumnNumber: Word; var Nullable: LongBool): SQLResult; stdcall;
   function isAutoIncrement(ColumnNumber: Word; var AutoIncr: LongBool): SQLResult; stdcall;
   function isReadOnly(ColumnNumber: Word; var ReadOnly: LongBool): SQLResult; stdcall;
   function isSearchable(ColumnNumber: Word; var Searchable: LongBool): SQLResult; stdcall;
   function isBlobSizeExact(ColumnNumber: Word; var IsExact: LongBool): SQLResult; stdcall;
   function next: SQLResult; stdcall;
   function getString(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getShort(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getLong(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getDouble(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getBcd(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getTimeStamp(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getTime(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getDate(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getBytes(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getBlobSize(ColumnNumber: Word; var Length: LongWord;
      var IsBlank: LongBool): SQLResult; stdcall;
   function getBlob(ColumnNumber: Word; Value: Pointer;
      var IsBlank: LongBool; Length: LongWord): SQLResult; stdcall;
end;

ISQLMetaData25 = interface(ISQLMetaData)
   function SetOption(eDOption: TSQLMetaDataOption;
                     PropValue: LongInt): SQLResult; stdcall;
   function GetOption(eDOption: TSQLMetaDataOption; PropValue: Pointer;
                     MaxLength: SmallInt; out Length: SmallInt): SQLResult; stdcall;
   function getObjectList(eObjType: TSQLObjectType; out Cursor: ISQLCursor25):
                     SQLResult; stdcall;
   function getTables(TableName: PChar; TableType: LongWord;
                     out Cursor: ISQLCursor25): SQLResult; stdcall;
   function getProcedures(ProcedureName: PChar; ProcType: LongWord;
                     out Cursor: ISQLCursor25): SQLResult; stdcall;
   function getColumns(TableName: PChar; ColumnName: PChar;
                     ColType: LongWord; Out Cursor: ISQLCursor25): SQLResult; stdcall;
   function getProcedureParams(ProcName: PChar; ParamName: PChar;
                     out Cursor: ISQLCursor25): SQLResult; stdcall;
   function getIndices(TableName: PChar; IndexType: LongWord;
                     out Cursor: ISQLCursor25): SQLResult; stdcall;
   function getErrorMessage(Error: PChar): SQLResult; overload; stdcall;
   function getErrorMessageLen(out ErrorLen: SmallInt): SQLResult; stdcall;
end;

implementation

end.
