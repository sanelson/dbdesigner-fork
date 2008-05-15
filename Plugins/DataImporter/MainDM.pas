unit MainDM;

interface

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  SysUtils, Classes, DBXpress, FMTBcd, DB, SqlExpr, Controls,
  QDialogs, QForms;

type
  PDBConn = ^TDBConn;     // Pointer for DBConn Data
  TDBConn = record
    Name,
    Description,
    DriverName,
    GetDriverFunc,
    LibraryName,
    VendorLib: string;
    TableScope: TTableScopes;
    Params: TStringList;
  end;

  TDMMain = class(TDataModule)
    SQLConn: TSQLConnection;
    SchemaSQLDataSet: TSQLDataSet;

    //Not Case Sensitive MiKe = mike
    function ReplaceText(txt, such, ers: string): string;

    //Case Sensitive MiKe <> mike
    function ReplaceString(txt, such, ers: string): string;


    //Subfunktionen
    function ReplaceText2(txt, such, ers: string): string;
    function ReplaceString2(txt, such, ers: string): string;

    function FormatText4SQL(s: string): string;
    function ShowStringEditor(ATitle, APromt: string; var value: string; SelectionStart: integer = 0; LimitChars: integer = 0): Boolean;

    function GetDBTables(var tablelist: TStringList): Boolean;

    procedure ExecSQL(s: string);

    procedure CreateProz(command, workingdir: string; show, wait4proz: integer);
    procedure KillProz;


    function GetSubStringCountInString(txt, such: string): integer;
    function FixLength(s: string; l: integer; alignLeft: boolean = True; FillChar: char = ' '): string;

    function GetColumnCountFromSepString(s, sep, delim: string): integer;
    function GetColumnFromSepString(s: string; colnr: integer; sep, delim: string): string;
    function GetColumnFromFixLengthString(s: string;
      colnr: integer; SList: TStringList): string;


  private
    { Private declarations }

    {$IFDEF MSWINDOWS}
    ProcessInfo : TProcessInformation;
    {$ENDIF}
  public
    { Public declarations }
    DatabaseTypes: TStringList;
    DefaultDatabaseType: string;
  end;

var
  DMMain: TDMMain;

implementation

uses EditorString;

{$R *.xfm}

function TDMMain.ReplaceText(txt, such, ers: string): string;
begin
  ReplaceText:=ReplaceText2(ReplaceText2(txt, such, '¢'), '¢', ers);
end;

function TDMMain.ReplaceText2(txt, such, ers: string): string;
begin
  while(Pos(UpperCase(such), UpperCase(txt))>0)do
    txt:=Copy(txt, 1, Pos(UpperCase(such), UpperCase(txt))-1)+ers+
      Copy(txt, Pos(UpperCase(such), UpperCase(txt))+Length(such), Length(txt));

  ReplaceText2:=txt;
end;

function TDMMain.ReplaceString(txt, such, ers: string): string;
begin
  ReplaceString:=ReplaceString2(ReplaceString2(txt, such, ''), '', ers);
end;

function TDMMain.ReplaceString2(txt, such, ers: string): string;
begin
  while(Pos(such, txt)>0)do
    txt:=Copy(txt, 1, Pos(such, txt)-1)+ers+
      Copy(txt, Pos(such, txt)+Length(such), Length(txt));

  ReplaceString2:=txt;
end;

function TDMMain.GetSubStringCountInString(txt, such: string): integer;
var strCount: integer;
  ers: string;
begin
  strCount:=0;
  ers:='';

  while(Pos(such, txt)>0)do
  begin
    txt:=Copy(txt, 1, Pos(such, txt)-1)+ers+
      Copy(txt, Pos(such, txt)+Length(such), Length(txt));

    inc(strCount);
  end;

  GetSubStringCountInString:=strCount;
end;


function TDMMain.GetColumnCountFromSepString(s,
  sep, delim: string): integer;
var theCount: integer;
  s1: string;
begin
  GetColumnCountFromSepString:=0;

  if(Trim(s)='')then
    Exit;

  if(sep='_tab')then
    sep:=Chr(9);

  s1:=s;

  //Ignore double-delims this time
  s1:=ReplaceString(s1, delim+delim+delim, delim+'¦¦');
  s1:=ReplaceString(s1, delim+delim, '¦¦');

  if(Copy(s1, Length(s1), 1)<>sep)then
    s1:=s1+sep;

  theCount:=0;
  while(Pos(sep, s1)>0)do
  begin
    //if sep is found, check if there is a delim before the sep
    if(DMMain.GetSubStringCountInString(Copy(s1, 1, Pos(sep, s1)-1), delim) mod 2=0)then
      inc(theCount);

    s1:=Copy(s1, Pos(sep, s1)+1, Length(s1));
  end;

  GetColumnCountFromSepString:=theCount;
end;

function TDMMain.GetColumnFromSepString(s: string;
  colnr: integer; sep, delim: string): string;
var theCount, p1, p2: integer;
  s1: string;
begin
  s1:=s;

  if(sep='_tab')then
    sep:=Chr(9);

  //Ignore double-delims this time
  s1:=ReplaceString(s1, delim+delim+delim, delim+'¦¦');
  s1:=ReplaceString(s1, delim+delim, '¦¦');

  if(Copy(s1, Length(s1), 1)<>sep)then
    s1:=s1+sep;

  theCount:=0;
  p2:=1;
  p1:=1;
  while(Pos(sep, s1)>0)and(theCount<=colnr)do
  begin
    //if sep is found, check if there is a delim before the sep
    if(DMMain.GetSubStringCountInString(Copy(s1, 1, Pos(sep, s1)-1), delim) mod 2=0)then
    begin
      inc(theCount);
      p1:=p2;
      p2:=p2+Pos(sep, s1);
      s1:=Copy(s1, Pos(sep, s1)+1, Length(s1));
    end
    else
      s1[Pos(sep, s1)]:='';
  end;

  s1:=Trim(Copy(s, p1, p2-p1-1));
  if(s1<>'')then
    if(s1[1]=delim)then
      s1:=Copy(s1, 2, Length(s1)-2);

  GetColumnFromSepString:=ReplaceString(s1, delim+delim, delim);
end;

function TDMMain.GetDBTables(var tablelist: TStringList): Boolean;
begin
  //GetTables
  //Col 0: RECNO
  //1: CATALOG_NAME
  //2: SCHEMA_NAME
  //3: TABLE_NAME
  //4: TABLE_TYPE

  if(SchemaSQLDataSet.Active)then
    SchemaSQLDataSet.Close;

  tablelist.Clear;

  SchemaSQLDataSet.SetSchemaInfo(stTables, '', '');
  SchemaSQLDataSet.Open;
  try
    while(Not(SchemaSQLDataSet.EOF))do
    begin
      {//The openodbc Driver returns the table name at Pos 2
      if(CurrentDBConn.DriverName='openodbc')then
        tablelist.Add(SchemaSQLDataSet.Fields[2].AsString)
      else}
        tablelist.Add(SchemaSQLDataSet.Fields[3].AsString);

      SchemaSQLDataSet.Next;
    end;
  finally
    SchemaSQLDataSet.Close;
  end;

  GetDBTables:=True;
end;

function TDMMain.FormatText4SQL(s: string): string;
begin
  s:=''''+ReplaceText(s, '''', '''''')+'''';

  FormatText4SQL:=s;
end;

function TDMMain.ShowStringEditor(ATitle, APromt: string; var value: string; SelectionStart: integer = 0; LimitChars: integer = 0): Boolean;
begin
  EditorStringForm:=TEditorStringForm.Create(self);
  try
    EditorStringForm.SetParams(ATitle, APromt, Value, SelectionStart, LimitChars);

    ShowStringEditor:=(EditorStringForm.ShowModal=mrOK);
    value:=EditorStringForm.ValueEd.Text;
  finally
    EditorStringForm.Free;
  end;
end;

procedure TDMMain.ExecSQL(s: string);
begin
  //Because of Delphi BUG!
  if(DMMain.SQLConn.ActiveStatements<>0)then
  begin
    DMMain.SQLConn.Close;
    DMMain.SQLConn.Open;
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
        MessageDlg('SQL statement cannot be executed.'+#13#10+
          x.Message+#13#10+#13#10+s, mtError, [mbOK], 0);

        Abort;
      end;
    end;
  end;
end;

procedure TDMMain.CreateProz(command, workingdir: string; show, wait4proz: integer);
var
  {$IFDEF MSWINDOWS}
  StartupInfo : TStartupInfo;
  {$ENDIF}
  wdir: pchar;
  {$IFDEF LINUX}
  FCmdThread: TCmdExecThread;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if(workingdir<>'')then
    wdir:=PChar(workingdir)
  else
    wdir:=nil;

  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := Sizeof(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  if(show=1)then
    StartupInfo.wShowWindow := SW_Show
  else
    StartupInfo.wShowWindow := SW_Hide;

  if(Not(CreateProcess(nil, pchar(command), nil, nil,
    true, Normal_PRIORITY_CLASS and CREATE_DEFAULT_ERROR_MODE,
    nil, wdir, StartupInfo, ProcessInfo)))then
    Raise Exception.Create('Fehler beim Start des Programms '+command+'. (Error: ' +IntToStr(GetLastError)+' )');


  //Wenn erwünscht, warten bis Programm beendet wird.
  if(wait4proz=1)then
  begin
    While(WaitForSingleObject(ProcessInfo.hProcess, 0) = WAIT_TIMEOUT)Do
      Application.ProcessMessages;

    ProcessInfo.hProcess:=0;
  end;
  {$ENDIF}

  {$IFDEF LINUX}
  if FCmdThread <> nil then
  begin
    if not FCmdThread.Done
      then raise Exception.Create('A command is already running')
    else FCmdThread.Free;
  end;
  FCmdThread := TCmdExecThread.Create;
  //FCmdThread.OnComplete := InternalComplete;
  FCmdThread.Command := command;
  FCmdThread.Resume;
  {$ENDIF}
end;

procedure TDMMain.KillProz;
begin
  {$IFDEF MSWINDOWS}
  if(ProcessInfo.hProcess<>0)then
    TerminateProcess(ProcessInfo.hProcess, 0);
  {$ENDIF}
end;

function TDMMain.FixLength(s: string; l: integer; alignLeft: boolean = True; FillChar: char = ' '): string;
begin
  if(Length(s)>l)then
    s:=copy(s, 1, l);

  if(Length(s)<l)then
    if(alignLeft)then
      s:=s+StringOfChar(FillChar, l-Length(s))
    else
      s:=StringOfChar(FillChar, l-Length(s))+s;

  FixLength:=s;
end;

function TDMMain.GetColumnFromFixLengthString(s: string;
  colnr: integer; SList: TStringList): string;
begin
  if(colnr>=0)and(colnr<SList.Count-1)then
    GetColumnFromFixLengthString:=Copy(s,
      StrToInt(SList[colnr]),
      StrToInt(SList[colnr+1])-StrToInt(SList[colnr]))
  else
    GetColumnFromFixLengthString:='';
end;

end.
