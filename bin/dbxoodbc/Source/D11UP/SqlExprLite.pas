interface

{$IFDEF MSWINDOWS}
uses Windows, SysUtils, Variants, Classes, DB, DBCommon, DBCommonTypes,
  DBXpress, SqlTimSt, WideStrings;
{$ENDIF}
{$IFDEF LINUX}
uses Libc, SysUtils, Variants, Classes, DB, DBCommon, DBXpress, SqlTimSt;
{$ENDIF}

const

  SSelect         =   'select';               { Do not localize }
  SSelectStar     =   ' select * ';           { Do not localize }
  SSelectStarFrom =   ' select * from ';      { Do not localize }
  SSelectSpaces   =   ' select ';             { Do not localize }
  SWhere          =   ' where ';              { Do not localize }
  SAnd            =   ' and ';                { Do not localize }
  SOrderBy        =   ' order by ';           { Do not localize }
  SParam          =   '?';                    { Do not localize }
  DefaultCursor   =   0;
  HourGlassCursor =   -11;

{ Default Max BlobSize }

  DefaultMaxBlobSize = -1;   // values are in K; -1 means retrieve actual size

{ Default RowsetSize }

  DefaultRowsetSize = 20;

  TErrorMessageSize = 2048;

{ FieldType Mappings }

  FldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL, // 0..5
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES, // 6..12
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB, // 13..19
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldWIDESTRING, fldINT64, fldADT, // 20..26
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN, // 27..33
    fldUNKNOWN, fldZSTRING, fldDATETIME, fldBCD, // 33..37
    fldWIDESTRING, fldBLOB, fldDATETIME, fldZSTRING); // 38..41

  FldSubTypeMap: array[TFieldType] of Word = (
    0, 0, 0, 0, 0, 0, 0, fldstMONEY, 0, 0, 0, 0, 0, 0, fldstAUTOINC, // 0..14
    fldstBINARY, fldstMEMO, fldstGRAPHIC, fldstFMTMEMO, fldstOLEOBJ, // 15..19
    fldstDBSOLEOBJ, fldstTYPEDBINARY, 0, fldstFIXED, 0, // 20..24
    0, 0, 0, 0, 0, fldstHBINARY, fldstHMEMO, 0, 0, 0, 0, 0, 0, // 24..37
    fldstFIXED, fldstWIDEMEMO, fldstORATIMESTAMP, fldstORAINTERVAL); // 38 ..41
  DataTypeMap: array[0..MAXLOGFLDTYPES - 1] of TFieldType = (
    ftUnknown, ftString, ftDate, ftBlob, ftBoolean, ftSmallint,
    ftInteger, ftFloat, ftBCD, ftBytes, ftTime, ftDateTime,
    ftWord, ftInteger, ftUnknown, ftVarBytes, ftUnknown, ftCursor,
    ftLargeInt, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet,
    ftTimeStamp, ftFMTBCD, ftWideString);

  BlobTypeMap: array[fldstMEMO..fldstBFILE] of TFieldType = (
    ftMemo, ftBlob, ftFmtMemo, ftParadoxOle, ftGraphic, ftDBaseOle,
    ftTypedBinary, ftBlob, ftBlob, ftBlob, ftWideMemo, ftOraClob, ftOraBlob,
    ftBlob, ftBlob);

type

{ Forward declarations }

  TLocaleCode = Integer;

  TSQLExceptionType = (exceptConnection, exceptCommand, exceptCursor, exceptMetaData, exceptUseLast);

  PSPParamDesc = ^SPParamDesc;
  SPParamDesc = packed record           { Stored Proc Descriptor }
    iParamNum       : Word;             { Field number (1..n) }
    szName          : WideString;       { Field name }
    iArgType        : TParamType;       { Field type }
    iDataType       : TFieldType;       { Field type }
    iUnits1         : SmallInt;         { Number of Chars, digits etc }
    iUnits2         : SmallInt;         { Decimal places etc. }
    iLen            : LongWord;         { Length in bytes  }
  end;
  SQLSPParamDesc = SPParamDesc;

  TConnectionUserType = (eUserMonitor, eUserDataSet);

{ TSQLMonitor }

  pSQLTRACEDesc30 = ^SQLTRACEDesc30;
  SQLTRACEDesc30 = packed record             { trace callback info }
    pszTrace        : array [0..1023] of WideChar;
    eTraceCat       : TRACECat;
    ClientData      : Integer;
    uTotalMsgLen    : Word;
  end;

  pSQLTRACEDesc25 = ^SQLTRACEDesc25;
  SQLTRACEDesc25 = packed record             { trace callback info }
    pszTrace        : array [0..1023] of AnsiChar;
    eTraceCat       : TRACECat;
    ClientData      : Integer;
    uTotalMsgLen    : Word;
  end;

  TTraceEvent30 = procedure(Sender: TObject; CBInfo: pSQLTRACEDesc30; var LogTrace: Boolean) of object;
  TTraceLogEvent30 = procedure(Sender: TObject; CBInfo: pSQLTRACEDesc30) of object;
  TTraceEvent25 = procedure(Sender: TObject; CBInfo: pSQLTRACEDesc25; var LogTrace: Boolean) of object;
  TTraceLogEvent25 = procedure(Sender: TObject; CBInfo: pSQLTRACEDesc25) of object;

  pSQLTRACEDesc = pSQLTRACEDesc30;
  SQLTRACEDesc = SQLTRACEDesc30;
  TTraceEvent = TTraceEvent30;
  TTraceLogEvent = TTraceLogEvent30;

{ TSQLConnection }

  TLocale = Pointer;

implementation

end.
