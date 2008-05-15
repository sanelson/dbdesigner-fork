unit MainDM;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of fabFORCE DBDesigner4.
// Copyright (C) 2002, 2003 Michael G. Zinner, www.fabFORCE.net
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
//
// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------
//
// Unit MainDM.pas
// ---------------
// Version 1.6, 06.05.2003, Mike
// Description
//   Contains all global used functions to CopyDir and CopyDirRecursive
//
// Changes:
//   Version 1.6, 06.05.2003, Mike
//     added GetGlobalSettingsPath
//   Version 1.5, 18.04.2003, Mike
//     fixed bug in EncodeText4XML causing special chars to be saved invalid to XML file
//   Version 1.4, 07.04.2003, Mike
//     Responde to new -disablePersonalSettings parameter (Windows only)
//   Version 1.3, 04.04.2003, Ulli
//     fixed bug in CopyDirRecursive when using PromptBeforeOverwrite flag
//   Version 1.2, 01.04.2003, Ulli
//     added PromptBeforeOverwrite parameter to
//   Version 1.1, 25.03.2003, Mike
//     2 procedures EncodeStreamForXML, DecodeStreamFromXML added to
//     support encoding and decoding for XML from streams
//   Version 1.0, 13.03.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  ActiveX, ShellAPI, ShlObj, // for SHGetSpecialFolderLocation() und SHGetPathFromIDList()
  {$ENDIF}
  SysUtils, Classes, DBXpress, DB, SqlExpr, FMTBcd, Provider, DBClient,
  DBLocal, QDialogs, QComCtrls, IniFiles, QForms, Qt,
  QButtons, QControls, QMenus,
  {$IFDEF USE_IXMLDBMODELType}
  xmldom, XMLIntf, XMLDoc,
  {$ENDIF}
  QTypes, QExtCtrls, Types, Math, QStdCtrls, QGraphics,
  GlobalSysFunctions;

type
  TDMMain = class(TDataModule)  // SQLDataSet to get Schema Info.

    //Constructor of the DataModule
    procedure DataModuleCreate(Sender: TObject);
    //Destructor of the DataModule
    procedure DataModuleDestroy(Sender: TObject);


    //Initialze a form by setting the default font
    procedure InitForm(theForm: TForm; SetFloatOnTop: Boolean = False; Translate: Boolean = True);

    procedure LoadApplicationFont;

    //Get language
    procedure LoadLanguageFromIniFile;
    procedure SaveLanguageToIniFile;

    //Reads a section from a text file that is organized like an ini file
    procedure GetSectionFromTxtFile(filename, section: string; theStringList: TStringList; GetOnlyValues: Boolean = False);
    //Translate Form
    procedure TranslateForm(theForm: TForm);
    //Get Translated strings  #
    procedure GetFormResourceStrings(theForm: TForm; name: string; theStrings: TStringList);
    procedure LoadTranslatedMessages;
    function GetTranslatedMessage(OriginalMsg: string; MsgNr: integer; StrToInsert: string = ''; StrToInsert2: string = ''): string;
    procedure ResetProgramLanguage;
    function GetLanguageCode: string;
    procedure SetLanguageCode(LanguageCode: string);

    //Copies a file
    procedure CopyDiskFile(sourcefile, destinationfile: string; PromtBeforeOverwrite: Boolean = True);

    //delete all files from a directory
    procedure DelFilesFromDir(dirname, fname: string);
    // Delete Directory
    procedure DelDir(name: string);
    // Delete Directory
    procedure DelDirRecursive(name: string);
    // Copy Directory with subdirs
    procedure CopyDir(fromdir, todir: string; PromptBeforeOverwrite: Boolean = True);
    // Copy Directory with subdirs
    procedure CopyDirRecursive(fromdir, todir: string; PromptBeforeOverwrite: Boolean = True);



    //Loads a cursor from bmp files
    procedure LoadACursor(crNumber: integer; fname, fname_mask: string; XSpot, YSpot: integer);

    //Not Case Sensitive MiKe = mike
    function ReplaceText(txt, such, ers: string): string;
    //Case Sensitive MiKe <> mike
    function ReplaceString(txt, such, ers: string): string;

    //Subfunktionen
    function ReplaceText2(txt, such, ers: string): string;
    function ReplaceString2(txt, such, ers: string): string;

    //Get an ID which is unique in the application
    function GetNextGlobalID: integer;
    procedure SetGlobalID(i: integer);

    //Show the String Editor modal
    function ShowStringEditor(ATitle, APromt: string; var value: string; SelectionStart: integer = 0; LimitChars: integer = 0): Boolean;

    //Encode normal text for the use in XML files
    function EncodeText4XML(s: string): string;
    //Decode normal text which was encoded with EncodeText4XML
    function DecodeXMLText(s: string): string;


    //Saves Windowposition into INI File
    procedure SaveWinPos(win: TForm; DoSize: Boolean);
    //Recalls Windowposition from INI File
    procedure RestoreWinPos(win: TForm; DoSize: Boolean);


    //Create prozess
    procedure CreateProz(command, workingdir: string; show, wait4proz: integer);
    //Kill prozess
    procedure KillProz;

    //Start a webbrowser and browse the given webpage
    procedure BrowsePage(s: string);

    //Format text for the use in an SQL Command, text will be enclosed by '
    function FormatText4SQL(s: string): string;

    //Display the online help web pages
    procedure ShowHelp(page, name: string);

    //Get the pointer to a form with the passed name
    function GetFormByName(name: string): TForm;

    //for data import
    function GetSubStringCountInString(txt, such: string): integer;
    function FixLength(s: string; l: integer; alignLeft: boolean = True; FillChar: char = ' '): string;

    function GetColumnCountFromSepString(s, sep, delim: string): integer;
    function GetColumnFromSepString(s: string; colnr: integer; sep, delim: string): string;
    function GetColumnFromFixLengthString(s: string;
      colnr: integer; SList: TStringList): string;

    //analizes an SQL insert command
    function GetValueFromSQLInsert(FieldName, InsertStr: string): string;


    //Reverses a TList
    procedure ReverseList(ObjList: TList);

    {$IFDEF MSWINDOWS}
    function GetWindowHandle(wTitle: String): HWnd;
    procedure SetWinPos(Handle, x, y, w, h: integer);

    //This procedure is used as a workaround of a CLX bug
    procedure OnOpenSaveDlgShow(Sender: TObject);
    {$ENDIF}

    //Save Bitmap als PNG, JPG or BMP
    procedure SaveBitmap(Handle: QPixmapH; FileName: string; FileType: string; JPGQuality: integer = 75);

    function GetFileSize(fname: string): string;
    function GetFileDate(fname: string): TDateTime;

    function LoadValueFromSettingsIniFile(section, name, default: string): string;
    procedure SaveValueInSettingsIniFile(section, name, value: string);

    function RGB(r, g, b: BYTE): integer;
    function HexStringToInt(s: string): integer;


    //-----------------------------------------
    //Workaround Code because of Delphi BUG
    procedure NormalizeStayOnTopForms;
    procedure RestoreStayOnTopForms;

    procedure NormalizeStayOnTopForm(theForm: TForm);
    procedure MakeFormStayOnTop(theForm: TForm);
    function IsFormStayingOnTop(theForm: TForm): Boolean;
    //Workaround Code because of Delphi BUG END
    //-----------------------------------------

{$IFDEF LINUX}
    procedure LinuxCorrectWinPos(Sender: TObject);
{$ENDIF}

    function EncodeStreamForXML(theStream: TStream): string;
    function DecodeStreamFromXML(XMLData: string; theStream: TStream): string;

    function CheckIniFileVersion(IniFileName: string; neededVersion: integer): Boolean;

    function GetValidObjectName(name: string): string;
  private
    { Private declarations }
    GlobalIDSequ: integer;

    WinPosCorrection: Array[0..5] of TPoint;

    //-----------------------------------------
    //Workaround Code because of Delphi BUG
    StayOnTopForms: TList;
    TopMostForm: TForm;
    ApplicationIsActive: Boolean;
    //Workaround Code because of Delphi BUG END
    //-----------------------------------------

    {$IFDEF MSWINDOWS}
    ProcessInfo : TProcessInformation;
    {$ENDIF}

    LanguageCode: String;
    MessageCaptions: TStringList;
  public
    { Public declarations }
    ProgName: string;

    SettingsPath: string;

    NormalizeEditorForms: Boolean;

    LockFormDeactivateTracking: Boolean;

{$IFDEF MSWINDOWS}
    disablePersonalSettings: Boolean;
{$ENDIF}

    HTMLBrowserAppl: string;

    ApplicationFontName: string;
    ApplicationFontSize: integer;
    ApplicationFontStyle: TFontStyles;
  end;



  TCmdExecThread = class(TThread)
  private
    { Private declarations }
  protected
    FOnComplete: TNotifyEvent;
    FCommand: String;
    FReturnValue: Integer;
    //FDone is used instead of Terminated because Terminated is
    // false in the OnComplete event handler.
    FDone: Boolean;
    procedure Execute; override;
    procedure FireCompleteEvent;
  public
    constructor Create;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property Command: String read FCommand write FCommand;
    property ReturnValue: Integer read FReturnValue;
    property Done: Boolean read FDone write FDone;
  end;

  function sendCLXEvent(receiver: QObjectH; event: QEventH): Boolean;

const
  DIGIT = ['0'..'9'];
  ALPHA_UC = ['A'..'Z'];
  ALPHA_LC = ['a'..'z'];
  ALPHANUMERIC = DIGIT + ALPHA_UC + ALPHA_LC;
  VALID_OBJECTNAME_CHARS = ALPHANUMERIC + ['_'];

var
  DMMain: TDMMain;
{$IFDEF MSWINDOWS}
  global_winname: string;

type
  PHWnd = ^HWnd;
{$ENDIF}

implementation

uses {$IFDEF LINUX}Libc, {$ENDIF}
  EditorString, StrUtils;

{$R *.xfm}

procedure TDMMain.DataModuleCreate(Sender: TObject);
var i: integer;
begin
  GlobalIDSequ:=1000;

  //Get the program name
  ProgName:=Copy(ExtractFileName(Application.ExeName), 1,
    Length(ExtractFileName(Application.ExeName))-
    Length(ExtractFileExt(Application.ExeName)));

  //Get the global Settings Path
  SettingsPath:=GetGlobalSettingsPath;

  for i:=0 to 5 do
    WinPosCorrection[i]:=Point(0, 0);


  //-----------------------------------------
  //Workaround Code because of Delphi BUG
  ApplicationIsActive:=True;
  LockFormDeactivateTracking:=False;
  StayOnTopForms:=TList.Create;
  TopMostForm:=nil;
  //Workaround Code because of Delphi BUG END
  //-----------------------------------------


  //Translation
  MessageCaptions:=TStringList.Create;

  HTMLBrowserAppl:='';

  LoadApplicationFont;

  Application.Font.Name:=ApplicationFontName;
  Application.Font.Size:=ApplicationFontSize;
  Application.Font.Style:=ApplicationFontStyle;
end;

//Destructor of the DataModule
procedure TDMMain.DataModuleDestroy(Sender: TObject);
begin
  SaveLanguageToIniFile;

  StayOnTopForms.Free;
  MessageCaptions.Free;
end;


procedure TDMMain.CopyDiskFile(sourcefile, destinationfile: string; PromtBeforeOverwrite: Boolean);
var NewFile: TFileStream;
  OldFile: TFileStream;
begin
  if(FileExists(sourcefile))then
  begin
    if(FileExists(destinationfile))and(PromtBeforeOverwrite)then
    begin
      if(MessageDlg(GetTranslatedMessage('The destination file %s does already exist. '+
        'Do you want to overwrite this file?', 22, destinationfile), mtCustom, [mbYes, mbNo], 0) = 3)then
        DeleteFile(destinationfile)
      else
        Exit;
    end;

    OldFile := TFileStream.Create(sourcefile, fmOpenRead or fmShareDenyWrite);
    try
      NewFile := TFileStream.Create(destinationfile, fmCreate{or fmShareDenyRead});

      try
        NewFile.CopyFrom(OldFile, OldFile.Size);
      finally
        FreeAndNil(NewFile);
      end;
    finally
      FreeAndNil(OldFile);
    end;
  end
  else
    MessageDlg(GetTranslatedMessage('The source file %s does not exist.', 23, sourcefile), mtError, [mbOK], 0);
end;

procedure TDMMain.LoadACursor(crNumber: integer; fname, fname_mask: string; XSpot, YSpot: integer);
var BMap, BMask: QBitMapH;
  FN : WideString;
  format: string;
begin
  //Check ist Files exist
  if(Not(FileExists(fname)))then
    raise EInOutError.Create(GetTranslatedMessage('File %s does not exist.', 24, fname));
  if(Not(FileExists(fname_mask)))then
    raise EInOutError.Create(GetTranslatedMessage('File %s does not exist.', 24, fname_mask));

  FN:=fname;
  BMap:=QBitmap_create(@FN, PChar(Format));
  FN:=fname_mask;
  BMask:=QBitmap_create(@FN, PChar(Format));
  Screen.Cursors[crNumber]:=QCursor_create(BMap, BMask, XSpot, YSpot);
  QBitmap_destroy(BMap);
  QBitmap_destroy(BMask);
end;

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

function TDMMain.GetNextGlobalID: integer;
begin
  GetNextGlobalID:=GlobalIDSequ;
  inc(GlobalIDSequ);
end;

procedure TDMMain.SetGlobalID(i: integer);
begin
  if(i>GlobalIDSequ)then
    GlobalIDSequ:=i;
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

function TDMMain.EncodeText4XML(s: string): string;
var i,j,k: integer;
    rs : String;  //our result string
    newTxt :String;
    replace : Boolean;
begin
  //theoretically each char could be of ord() > 126
  SetLength(rs, Length(s)* 4);

  //Encode special Chars
  i:=1; //the index of the source-string
  j:=1; //the index of the result-string

  while (i<= Length(s)) do
  begin
    Replace := true;

    if(Ord(s[i])>126)then newTxt:= '\'+IntToStr(Ord(s[i]))
    else if (s[i] = '\') then newTxt := '\\'
    else if (s[i] = #13) then newTxt := ''
    else if (s[i] = #10) then newTxt := '\n'
    else if (s[i] = '"') then newTxt := '\A'
    else if (s[i] = '''') then newTxt := '\a'
    else if (s[i] = '&') then newTxt := '\+'
    else if (s[i] = '<') then newTxt := '\k'
    else if (s[i] = '>') then newTxt := '\g'
    else     { nothing should be replaced }
    begin
      rs[j] := s[i];
      inc(j);
      Replace := false;
    end;
    if (Replace) then    //we have to replace s[i] with newTxt
    begin
       k:= 1;
       while (k <= length(newTxt) ) do
       begin
         rs[j] := newTxt[k];
         inc(j);
         inc(k);
       end;
    end;

    inc(i);
  end;

  EncodeText4XML:=AnsiLeftStr(rs,j-1);
end;


function TDMMain.DecodeXMLText(s: string): string;
var i,j: integer;
    rs : String;
begin
  //our optimizatin can only insert single characters
  s:=ReplaceString(s, '\n', #13#10);
  
  SetLength(rs, Length(s));

  //Decode special Chars
  i:=1;
  j:= 1;
  while(i <= Length(s))do
  begin
    if(s[i]='\') then
    begin
      if (Ord(s[i+1])>=Ord('0'))and(Ord(s[i+1])<=Ord('9'))then
      begin
        rs[j] :=  Chr(StrToInt(Copy(s, i+1, 3)));
        i := i+2;
      end
      else if (s[i+1] = 'A') then rs[j] := '"'
      else if (s[i+1] = 'a') then rs[j] := ''''
      else if (s[i+1] = '+') then rs[j] := '&'
      else if (s[i+1] = 'k') then rs[j] := '<'
      else if (s[i+1] = 'g') then rs[j] := '>'
      else if (s[i+1] = '\') then rs[j] := '\'
      else
      begin
        rs[j]:= s[i];
        dec(i);
      end;
      inc(i);
    end
    else
    begin
      rs[j] := s[i];
    end;

    inc(i);
    inc(j);
  end;

  //DecodeXMLText:=AnsiLeftStr(rs,j-1);
  DecodeXMLText:=Copy(rs, 0, j-1);
end;

procedure TDMMain.SaveWinPos(win: TForm; DoSize: Boolean);
var winname: string;
 theIni: TMemIniFile;
 P: TPoint;
begin
  winname:=win.name;

{$IFDEF MSWINDOWS}
  P:=win.ClientToScreen(Point(0, 0));
{$ELSE}
  P.X:=win.Left;
  P.Y:=win.Top;
{$ENDIF}

  //Write IniFile
  theIni:=TMemIniFile.Create(SettingsPath+ProgName+'_Settings.ini');
  try
    if(QWidget_isMaximized(win.Handle))then
      theIni.WriteInteger('WindowPositions', winname+'State', 1)
    else
    begin
      theIni.WriteInteger('WindowPositions', winname+'State', 0);

      if(P.X>0)then
        theIni.WriteInteger('WindowPositions', winname+'Left', P.X)
      else
        theIni.WriteInteger('WindowPositions', winname+'Left', 15);

      if(P.Y>0)then
        theIni.WriteInteger('WindowPositions', winname+'Top', P.Y)
      else
        theIni.WriteInteger('WindowPositions', winname+'Top', 15);

      if(DoSize)then
      begin
        theIni.WriteInteger('WindowPositions', winname+'Width', win.Width);

        theIni.WriteInteger('WindowPositions', winname+'Height', win.Height);
      end;
    end;

    theIni.UpdateFile;
  finally
    theIni.Free;
  end;
end;

procedure TDMMain.RestoreWinPos(win: TForm; DoSize: Boolean);
var theIni: TMemIniFile;
  winname: string;
{$IFDEF MSWINDOWS}
  P: TPoint;
{$ENDIF}
{$IFDEF LINUX}
  theTimer: TTimer;
{$ENDIF}
  WinPos: TPoint;
begin
  winname:=win.name;

  //Read IniFile
  theIni:=TMemIniFile.Create(SettingsPath+ProgName+'_Settings.ini');
  try
    try
      WinPos.X:=theIni.ReadInteger('WindowPositions', winname+'Left', 80);
      WinPos.Y:=theIni.ReadInteger('WindowPositions', winname+'Top', 140);
      if(WinPos.X>Screen.Width)then
        WinPos.X:=Screen.Width-50;
      if(WinPos.Y>Screen.Height)then
        WinPos.Y:=Screen.Height-50;

{$IFDEF MSWINDOWS}
      win.Left:=WinPos.X-WinPosCorrection[Ord(win.BorderStyle)].X;
      win.Top:=WinPos.Y-WinPosCorrection[Ord(win.BorderStyle)].Y;

      P:=win.ClientToScreen(Point(0, 0));
      if(P.X<>WinPos.X)or(P.Y<>WinPos.Y)then
      begin
        WinPosCorrection[Ord(win.BorderStyle)].X:=P.X-WinPos.X;
        WinPosCorrection[Ord(win.BorderStyle)].Y:=P.Y-WinPos.Y;

        win.Left:=WinPos.X-WinPosCorrection[Ord(win.BorderStyle)].X;
        win.Top:=WinPos.Y-WinPosCorrection[Ord(win.BorderStyle)].Y;
      end;

      if(win.Top<0)then
        win.Top:=0;
{$ENDIF}
{$IFDEF LINUX}
      win.Left:=WinPos.X;
      if(WinPos.Y>0)then
        win.Top:=WinPos.Y
      else
        win.Top:=0;

      //Workaround from Linux bug
      //A Form has wrong Left/Top Coordinates after it is shown
      //Create a timer to correct them
      theTimer:=TTimer.Create(win);
      theTimer.Enabled:=False;
      theTimer.Interval:=200;
      theTimer.OnTimer:=LinuxCorrectWinPos;
      theTimer.Tag:=WinPos.X*10000+WinPos.Y;
      theTimer.Enabled:=True;
{$ENDIF}

      if(DoSize)then
      begin
        win.Width:=
          theIni.ReadInteger('WindowPositions', winname+'Width', 140);
        win.Height:=
          theIni.ReadInteger('WindowPositions', winname+'Height', 140);
        if(theIni.ReadInteger('WindowPositions', winname+'State', 0)=1)then
          win.WindowState:=wsMaximized;
      end;
    except
      win.Left:=((Application.MainForm.Left+Application.MainForm.Width)-win.Width) div 2-40;
      win.Top:=((Application.MainForm.Top+Application.MainForm.Height)-win.Height) div 2;

    end;
  finally
    theIni.Free;
  end;
end;

{$IFDEF LINUX}
procedure TDMMain.LinuxCorrectWinPos(Sender: TObject);
begin
  TTimer(Sender).Enabled:=False;
  TForm(TTimer(Sender).Owner).Left:=TTimer(Sender).Tag div 10000;
  TForm(TTimer(Sender).Owner).Top:=TTimer(Sender).Tag-(TTimer(Sender).Tag div 10000*10000);
  TTimer(Sender).Free;
end;
{$ENDIF}

procedure TDMMain.CreateProz(command, workingdir: string; show, wait4proz: integer);
var
  {$IFDEF MSWINDOWS}
  StartupInfo : TStartupInfo;
  wdir: pchar;
  {$ENDIF}

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
    Raise Exception.Create(command+': '+#13#10+
      GetTranslatedMessage('The Program could not be launched. Error code: %s', 25, IntToStr(GetLastError)));


  //If requested, wait for the programm to end
  if(wait4proz=1)then
  begin
    While(WaitForSingleObject(ProcessInfo.hProcess, 0) = WAIT_TIMEOUT)Do
      Application.ProcessMessages;

    ProcessInfo.hProcess:=0;
  end;
  {$ENDIF}

  {$IFDEF LINUX}
  {if FCmdThread <> nil then
  begin
    if not FCmdThread.Done
      then raise Exception.Create('A command is already running')
    else FCmdThread.Free;
  end;}
  FCmdThread := TCmdExecThread.Create;
  //FCmdThread.OnComplete := InternalComplete;
  FCmdThread.Command := command;
  FCmdThread.Resume;

  //Wenn erwünscht, warten bis Programm beendet wird.
  if(wait4proz=1)then
  begin
    while(Not(FCmdThread.Done))do
    begin
      Sleep(500);
      //Do drawing and stuff
      Application.ProcessMessages;
    end;
  end;
  {$ENDIF}
end;

procedure TDMMain.KillProz;
begin
  {$IFDEF MSWINDOWS}
  if(ProcessInfo.hProcess<>0)then
    TerminateProcess(ProcessInfo.hProcess, 0);
  {$ENDIF}
end;

//------------------------------------------
// Code for Linux Lics System call Thread

constructor TCmdExecThread.Create;
begin
  Inherited Create(True);
  FDone := False;
end;

procedure TCmdExecThread.Execute;
begin
  {$IFDEF LINUX}
  if FCommand <> '' then FReturnValue := Libc.System(PChar(FCommand));
  {$ENDIF}
  FDone := True;
  Synchronize(FireCompleteEvent);
end;

procedure TCmdExecThread.FireCompleteEvent;
begin
  if Assigned(FOnComplete) then FOnComplete(Self);
end;

//------------------------------------------

procedure TDMMain.BrowsePage(s: string);
begin
  if(HTMLBrowserAppl='')then
  begin
{$IFDEF MSWINDOWS}
    DMMain.CreateProz('explorer '+s, '', 1, 0);
{$ENDIF}
{$IFDEF LINUX}
    DMMain.CreateProz('konqueror '+s, '', 1, 0);
{$ENDIF}
  end
  else
    DMMain.CreateProz(HTMLBrowserAppl+' '+s, '', 1, 0);
end;

function TDMMain.FormatText4SQL(s: string): string;
begin
  s:=''''+ReplaceText(s, '''', '''''')+'''';

  FormatText4SQL:=s;
end;

procedure TDMMain.ShowHelp(page, name: string);
var fname: string;
  Template: TStringList;
begin
  Template:=TStringList.Create;
  try
    fname:=SettingsPath+'tmpindex'+FormatDateTime('hhnnsszzz', now)+'.html';
    Template.LoadFromFile(ExtractFilePath(Application.ExeName)+'Doc'+PathDelim+'template.html');

    Template.Text:=ReplaceText(Template.Text, '$helpdir$', ExtractFilePath(Application.ExeName)+'Doc'+PathDelim);
    Template.Text:=ReplaceText(Template.Text, '$helplink$', page+'.html#'+name);


    Template.SaveToFile(fname);

    //Application.Minimize;
    BrowsePage(fname);
  finally
    Template.Free;
  end;

  //MainForm.WindowState:=wsMinimized;
end;

function TDMMain.GetFormByName(name: string): TForm;
var i: integer;
begin
  GetFormByName:=nil;

  for i:=0 to Screen.FormCount-1 do
    if(Screen.Forms[i].Name=name)then
    begin
      GetFormByName:=Screen.Forms[i];
      break;
    end;
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

function TDMMain.GetValueFromSQLInsert(FieldName, InsertStr: string): string;
var fpos, valpos, i: integer;
  valstr, s: string;
begin
  InsertStr:=Uppercase(Copy(InsertStr, Pos('(', InsertStr)+1, Length(InsertStr)));
  FieldName:=Uppercase(FieldName);

  //the field could be at the very beginning
  if(Copy(InsertStr, 1, Length(FieldName))=FieldName)and
    ((Copy(InsertStr, Length(FieldName)+1, 1)=',')or
      (Copy(InsertStr, Length(FieldName)+1, 1)=' '))then
    fpos:=1
  else
    fpos:=0;

  //Find field, leading by , or space and followed by , or space
  if(fpos=0)then
    fpos:=Pos(' '+FieldName+',', UpperCase(InsertStr));
  if(fpos=0)then
    fpos:=Pos(','+FieldName+',', UpperCase(InsertStr));
  if(fpos=0)then
    fpos:=Pos(' '+FieldName+' ', UpperCase(InsertStr));
  if(fpos=0)then
    fpos:=Pos(','+FieldName+' ', UpperCase(InsertStr));
  if(fpos=0)then
    fpos:=Pos(' '+FieldName+')', UpperCase(InsertStr));
  if(fpos=0)then
    fpos:=Pos(','+FieldName+')', UpperCase(InsertStr));

  if(fpos>0)then
  begin
    //get position of field
    valpos:=GetSubStringCountInString(Copy(InsertStr, 1, fpos+1), ',');

    i:=Pos('VALUES(', UpperCase(InsertStr));
    if(i=0)then
      i:=Pos('VALUES ', UpperCase(InsertStr));

    //Get string of values and eliminate possible leading (
    valstr:=trim(Copy(InsertStr, i+7, Length(InsertStr)-7-i));
    if(Copy(valstr, 1, 1)='(')then
      valstr:=Copy(valstr, 2, Length(valstr));

    s:=GetColumnFromSepString(valstr, valpos, ',', '''');

    GetValueFromSQLInsert:=Trim(s);
  end
  else
    GetValueFromSQLInsert:='NOTININSERT';

end;

procedure TDMMain.InitForm(theForm: TForm; SetFloatOnTop: Boolean = False; Translate: Boolean = True);
begin
  theForm.Font.Name:=ApplicationFontName;
  theForm.Font.Size:=ApplicationFontSize;
  theForm.Font.Style:=ApplicationFontStyle;

  //Make Editors float on top if requested by the user
  if(SetFloatOnTop)then
    theForm.FormStyle:=fsStayOnTop;

  //Make Translation
  if(Translate)then
    TranslateForm(theForm);
end;

procedure TDMMain.LoadLanguageFromIniFile;
var theIni: TMemIniFile;
begin
  //Open IniFile
  theIni:=TMemIniFile.Create(SettingsPath+'Language.ini');
  try
    LanguageCode:=theIni.ReadString('GeneralSettings', 'Language', 'en');
    if(LanguageCode='')then
      LanguageCode:='en';
  finally
    theIni.Free;
  end;
end;

procedure TDMMain.SaveLanguageToIniFile;
var theIni: TMemIniFile;
begin
  //Open IniFile
  theIni:=TMemIniFile.Create(SettingsPath+'Language.ini');
  try
    theIni.WriteString('GeneralSettings', 'Language', LanguageCode);

    theIni.UpdateFile;
  finally
    theIni.Free;
  end;
end;

procedure TDMMain.GetSectionFromTxtFile(filename, section: string; theStringList: TStringList; GetOnlyValues: Boolean = False);
var tmpStringList: TStringList;
  i: integer;
  SectionReached: Boolean;
begin
  theStringList.Clear;

  tmpStringList:=TStringList.Create;
  try
    tmpStringList.LoadFromFile(filename);
    i:=0;
    SectionReached:=False;
    while(i<tmpStringList.Count)do
    begin
      if(Not(SectionReached))then
      begin
        if(tmpStringList[i]='['+section+']')then
          SectionReached:=True;
      end
      else
      begin
        if(Copy(tmpStringList[i], 1, 1)<>'[')then
        begin
          if(Copy(tmpStringList[i], 1, 2)=LanguageCode)then
            if(GetOnlyValues)then
              theStringList.Add(tmpStringList.ValueFromIndex[i])
            else
              theStringList.Add(tmpStringList[i]);
        end
        else
          break;
      end;

      inc(i);
    end;
  finally
    tmpStringList.Free;
  end;
end;

procedure TDMMain.TranslateForm(theForm: TForm);
var theStringList: TStringList;
  i: integer;
  trans_caption, trans_hint, s1: string;
begin
  if(Not(FileExists(SettingsPath+ProgName+'_Translations.txt')))then
    Exit;

  theStringList:=TStringList.Create;
  try
    GetSectionFromTxtFile(SettingsPath+ProgName+'_Translations.txt',
      theForm.Name, theStringList);

    for i:=0 to theForm.ComponentCount-1 do
    begin
      s1:=LanguageCode+'_'+theForm.Components[i].ClassName+'_';
      s1:=s1+theForm.Components[i].Name;
      trans_caption:=ReplaceString(theStringList.Values[s1], '\039', '''');
      trans_hint:=ReplaceString(ReplaceString(theStringList.Values[s1+'_Hint'], '\039', ''''), '\n', #13#10);

      if(trans_caption<>'')then
      begin
        if(theForm.Components[i].ClassParent=TForm)then
          TForm(theForm.Components[i]).Caption:=trans_caption
        else if(theForm.Components[i].ClassNameIs('TSpeedButton'))then
          TSpeedButton(theForm.Components[i]).Caption:=trans_caption
        else if(theForm.Components[i].ClassNameIs('TBitBtn'))then
          TBitBtn(theForm.Components[i]).Caption:=trans_caption
        else if(theForm.Components[i].ClassNameIs('TTabSheet'))then
          TTabSheet(theForm.Components[i]).Caption:=trans_caption
        else if(theForm.Components[i].ClassNameIs('TLabel'))then
          TLabel(theForm.Components[i]).Caption:=trans_caption
        else if(theForm.Components[i].ClassNameIs('TPanel'))then
          TPanel(theForm.Components[i]).Caption:=trans_caption
        else if(theForm.Components[i].ClassNameIs('TGroupBox'))then
          TGroupBox(theForm.Components[i]).Caption:=trans_caption
        else if(theForm.Components[i].ClassNameIs('TMenuItem'))then
          TMenuItem(theForm.Components[i]).Caption:=trans_caption
        else if(theForm.Components[i].ClassNameIs('TCheckBox'))then
          TCheckBox(theForm.Components[i]).Caption:=trans_caption
        else if(theForm.Components[i].ClassNameIs('TRadioButton'))then
          TRadioButton(theForm.Components[i]).Caption:=trans_caption;
      end;

      if(trans_hint<>'')then
      begin
        if(theForm.Components[i].ClassParent=TForm)then
          TForm(theForm.Components[i]).Hint:=trans_hint
        else if(theForm.Components[i].ClassNameIs('TSpeedButton'))then
          TSpeedButton(theForm.Components[i]).Hint:=trans_hint
        else if(theForm.Components[i].ClassNameIs('TBitBtn'))then
          TBitBtn(theForm.Components[i]).Hint:=trans_hint
        else if(theForm.Components[i].ClassNameIs('TTabSheet'))then
          TTabSheet(theForm.Components[i]).Hint:=trans_hint
        else if(theForm.Components[i].ClassNameIs('TLabel'))then
          TLabel(theForm.Components[i]).Hint:=trans_hint
        else if(theForm.Components[i].ClassNameIs('TPanel'))then
          TPanel(theForm.Components[i]).Hint:=trans_hint
        else if(theForm.Components[i].ClassNameIs('TGroupBox'))then
          TGroupBox(theForm.Components[i]).Hint:=trans_hint
        else if(theForm.Components[i].ClassNameIs('TMenuItem'))then
          TMenuItem(theForm.Components[i]).Hint:=trans_hint
        else if(theForm.Components[i].ClassNameIs('TCheckBox'))then
          TCheckBox(theForm.Components[i]).Hint:=trans_hint
        else if(theForm.Components[i].ClassNameIs('TRadioButton'))then
          TRadioButton(theForm.Components[i]).Hint:=trans_hint;
      end;
    end;
  finally
    theStringList.Free;
  end;
end;

procedure TDMMain.GetFormResourceStrings(theForm: TForm; name: string; theStrings: TStringList);
var i: integer;
begin
  if(Not(FileExists(SettingsPath+ProgName+'_Translations.txt')))then
    Exit;
    
  GetSectionFromTxtFile(SettingsPath+ProgName+'_Translations.txt',
    theForm.Name+'_ResourceStrings', theStrings, False);

  //Filter ResourceStrings by provided name
  i:=0;
  while(i<theStrings.Count)do
  begin
    if(CompareText(Copy(theStrings[i], 4, Length(name)), name)<>0)then
      theStrings.Delete(i)
    else
    begin
      theStrings[i]:=theStrings.ValueFromIndex[i];
      inc(i);
    end;
  end;
end;

procedure TDMMain.LoadTranslatedMessages;
begin
  if(Not(FileExists(SettingsPath+ProgName+'_Translations.txt')))then
    Exit;

  GetSectionFromTxtFile(SettingsPath+ProgName+'_Translations.txt',
    'Messages', MessageCaptions, True);
end;

function TDMMain.GetTranslatedMessage(OriginalMsg: string; MsgNr: integer; StrToInsert: string = ''; StrToInsert2: string = ''): string;
var s: string;
begin
  if(MsgNr>=0)and(MsgNr<=MessageCaptions.Count)then
    s:=MessageCaptions[MsgNr-1]
  else
    s:=OriginalMsg;

  //Insert first StrToInsert
  if(Pos('%s', s)>0)then
    s:=Copy(s, 1, Pos('%s', s)-1)+StrToInsert+Copy(s, Pos('%s', s)+2, Length(s));

  //Insert second StrToInsert
  if(Pos('%s', s)>0)then
    s:=Copy(s, 1, Pos('%s', s)-1)+StrToInsert2+Copy(s, Pos('%s', s)+2, Length(s));

  s:=ReplaceString(s, '\n', #13#10);
  s:=ReplaceString(s, '\039', '''');
  s:=ReplaceString(s, '\\', '\');

  GetTranslatedMessage:=s;
end;

procedure TDMMain.ResetProgramLanguage;
var i: integer;
begin
  LoadTranslatedMessages;

  for i:=0 to Screen.FormCount-1 do
    TranslateForm(Screen.Forms[i]);
end;

function TDMMain.GetLanguageCode: string;
begin
  GetLanguageCode:=LanguageCode;
end;

procedure TDMMain.SetLanguageCode(LanguageCode: string);
begin
  self.LanguageCode:=LanguageCode;
  ResetProgramLanguage;
end;

procedure TDMMain.DelFilesFromDir(dirname, fname: string);
var result: integer;
  SearchRec: TSearchRec;
begin
  //Check if dirname is ended with a PathDelim
  if(Copy(dirname, Length(dirname), 1)<>PathDelim)then
    dirname:=dirname+PathDelim;

  Result := FindFirst(dirname+fname, 0, SearchRec);
  if(Result<>0)then
    Exit;

  try
    while Result = 0 do
    begin
      DeleteFile(dirname+PathDelim+SearchRec.Name);

      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;


procedure TDMMain.DelDir(name: string);
var result: integer;
  SearchRec: TSearchRec;
begin
  if(copy(name, Length(name), 1)<>PathDelim)then
    name:=name+PathDelim;

  Result := FindFirst(name+'*.*', 0, SearchRec);
  if(Result<>0)then
    Exit;

  try
    while Result = 0 do
    begin
      DeleteFile(name+SearchRec.Name);

      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

procedure TDMMain.DelDirRecursive(name: string);
var SearchRec: TSearchRec;
begin
  if(copy(name, Length(name), 1)<>PathDelim)then
    name:=name+PathDelim;

  if FindFirst(name+'*.*', faDirectory, SearchRec) = 0 then
  begin
    repeat
      if((SearchRec.Attr and faDirectory) = faDirectory)then
      begin
        //Ignore . and ..
        if(Copy(SearchRec.name, 1, 1)='.')then
          continue;

        //Copy the directory
        DelDir(name+SearchRec.name);

        //call recursive
        DelDirRecursive(name+SearchRec.name);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;

  //delCopy the directory
  DelDir(name);
  try
    rmdir(name);
  except
  end;
end;

procedure TDMMain.CopyDir(fromdir, todir: string; PromptBeforeOverwrite: Boolean);
var result: integer;
  Ergebnis: integer;
  SearchRec: TSearchRec;
begin
  Ergebnis:=0;

  ForceDirectories(todir);

  try
    //Copy directory
    Ergebnis := FindFirst(fromdir+'*.*', faAnyFile, SearchRec);
    result:=Ergebnis;
    while Result = 0 do
    begin
      if((SearchRec.Attr and faDirectory) <> faDirectory)then
        CopyDiskFile(fromdir+SearchRec.Name,
          todir+SearchRec.Name, PromptBeforeOverwrite);

      Result := FindNext(SearchRec);
    end;
  finally
    if(Ergebnis=0)then
      FindClose(SearchRec);
  end;
end;

procedure TDMMain.CopyDirRecursive(fromdir, todir: string; PromptBeforeOverwrite: Boolean);
var SearchRec: TSearchRec;
begin
  if(copy(fromdir, Length(fromdir), 1)<>PathDelim)then
    fromdir:=fromdir+PathDelim;

  if(copy(todir, Length(todir), 1)<>PathDelim)then
    todir:=todir+PathDelim;

  //Copy the directory
  CopyDir(fromdir, todir, PromptBeforeOverwrite);

  if FindFirst(fromdir+'*.*', faDirectory, SearchRec) = 0 then
  begin
    repeat
      if((SearchRec.Attr and faDirectory) = faDirectory)then
      begin
        //Ignore . and ..
        if(Copy(SearchRec.name, 1, 1)='.')then
          continue;

        //Copy the directory
        CopyDir(fromdir+SearchRec.name, todir+SearchRec.name, PromptBeforeOverwrite);

        //call recursive
        CopyDirRecursive(fromdir+SearchRec.name, todir+SearchRec.name, PromptBeforeOverwrite);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure TDMMain.ReverseList(ObjList: TList);
var i: integer;
begin
  for i:=0 to (ObjList.Count-2) div 2 do
    ObjList.Exchange(i, ObjList.Count-1-i);
end;

//--------------------------------------------------
// Only Win32
// get a Window handle
// uses global global_winname

{$IFDEF MSWINDOWS}
function GetText(Wnd: HWND): string;
var
  textlength: Integer; 
  Text: PChar; 
begin 
  textlength := SendMessage(Wnd, WM_GETTEXTLENGTH, 0, 0);
  if textlength = 0 then Result := '' 
  else
  begin
    GetMem(Text, textlength + 1);
    SendMessage(Wnd, WM_GETTEXT, textlength + 1, Integer(Text));
    Result := Text;
    FreeMem(Text);
  end;
end;

function EnumWindowsProc(Wnd: HWND; lParam: lParam): BOOL; stdcall;
begin
  Result := True;
  //ShowMessage('Handle: ' + IntToStr(Wnd) + ',Text:  ' + GetText(Wnd));

  if(Pos(global_winname, GetText(Wnd))>0)then
  begin
    PHWnd(lParam)^:=Wnd;
    Result := false;
  end;
end;


function TDMMain.GetWindowHandle(wTitle: String): HWnd;
var theWnd: HWnd;
begin
  theWnd := 0;
  Result:=0;
  global_winname:=wTitle;

  EnumWindows(@EnumWindowsProc, LongInt(@theWnd));
  if(theWnd<>0)then
    Result:=theWnd;
end;

procedure TDMMain.SetWinPos(Handle, x, y, w, h: integer);
begin
  SetWindowPos(Handle, HWND_NOTOPMOST, x, y, w, h, SWP_SHOWWINDOW);
end;

{$ENDIF}


{$IFDEF MSWINDOWS}
//Because of a Delphi Bug, the Open Dlg is always
//displayed on the left upper corner
//To fix this, catch OnShow Event and reposition the Dlg
procedure TDMMain.OnOpenSaveDlgShow(Sender: TObject);
var theWinHandle: integer;
  theTitle: string;
  dlg_width, dlg_height: integer;
begin
  theTitle:='';
  dlg_width:=0;
  dlg_height:=0;

  if(Sender.ClassNameIs('TOpenDialog'))then
  begin
    theTitle:=TOpenDialog(Sender).Title;
    if(TOpenDialog(Sender).Width=600)then
      TOpenDialog(Sender).Width:=660;
    if(TOpenDialog(Sender).Height=360)then
      TOpenDialog(Sender).Height:=430;
    dlg_width:=TOpenDialog(Sender).Width;
    dlg_height:=TOpenDialog(Sender).Height;
  end
  else if(Sender.ClassNameIs('TSaveDialog'))then
  begin
    theTitle:=TSaveDialog(Sender).Title;
    if(TSaveDialog(Sender).Width=600)then
      TSaveDialog(Sender).Width:=660;
    if(TSaveDialog(Sender).Height=360)then
      TSaveDialog(Sender).Height:=430;
    dlg_width:=TSaveDialog(Sender).Width;
    dlg_height:=TSaveDialog(Sender).Height;
  end;

  theWinHandle:=DMMain.GetWindowHandle(theTitle);
  if(theWinHandle<>0)then
    DMMain.SetWinPos(theWinHandle,
      Application.MainForm.Left+Application.MainForm.Width div 2-dlg_width div 2,
      Application.MainForm.Top+Application.MainForm.Height div 2-dlg_height div 2,
      dlg_width, dlg_height);
end;
{$ENDIF}

procedure TDMMain.SaveBitmap(Handle: QPixmapH; FileName: string; FileType: string; JPGQuality: integer = 75);
var lWideStr: WideString;
begin
  lWideStr:=FileName;

  if(Copy(FileType, 1, 1)='.')then
    FileType:=Copy(FileType, 2, Length(FileType));

  if(Uppercase(FileType)='PNG')or(Uppercase(FileType)='BMP')then
    QPixMap_save(Handle, @lWideStr, PChar(Uppercase(FileType)))
  else if(FileType='JPEG')or(FileType='JPG')then
    QPixMap_save(Handle, @lWideStr, PChar('JPEG'), JPGQuality);
end;

function TDMMain.GetFileSize(fname: string): string;
var f: file of Byte;
begin
  try
    AssignFile(f, fname);
    Reset(f);
    GetFileSize:=IntToStr(FileSize(f));
    CloseFile(f);

  except
    GetFileSize:='0';
  end;
end;

function TDMMain.GetFileDate(fname: string): TDateTime;
begin
  GetFileDate:=FileDateToDateTime(FileAge(fname));
end;

function TDMMain.LoadValueFromSettingsIniFile(section, name, default: string): string;
var theIni: TMemIniFile;
begin
  //Read IniFile
  theIni:=TMemIniFile.Create(SettingsPath+ProgName+'_Settings.ini');
  try
    LoadValueFromSettingsIniFile:=
      theIni.ReadString(section, name, default);
  finally
    theIni.Free;
  end;
end;

procedure TDMMain.SaveValueInSettingsIniFile(section, name, value: string);
var theIni: TMemIniFile;
begin
  //Read IniFile
  theIni:=TMemIniFile.Create(SettingsPath+ProgName+'_Settings.ini');
  try
    theIni.WriteString(section, name, value);
    theIni.UpdateFile;
  finally
    theIni.Free;
  end;
end;

function TDMMain.RGB(r, g, b: BYTE): integer;
begin
  RGB:=(r OR (g SHL 8) OR (b SHL 16));
end;

function TDMMain.HexStringToInt(s: string): integer;
var val, i, ex, z: integer;
begin
  s:=UpperCase(s);
  val:=0;
  ex:=1;
  for i:=Length(s)-1 downto 0 do
  begin
    z:=Ord(s[i+1]);
    if(z>=65)then
      z:=z-65+10
    else
      z:=z-48;

    val:=val+z*ex;
    ex:=ex*16;
  end;

  HexStringToInt:=val;
end;

//-----------------------------------------
//Workaround Code because of Delphi BUG

procedure TDMMain.NormalizeStayOnTopForm(theForm: TForm);
{$IFDEF MSWINDOWS}
var P: TPoint;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  QOpenWidget_clearWFlags(QOpenWidgetH(theForm.Handle),
    Cardinal(WidgetFlags_WStyle_StaysOnTop));

  //Get Pos
  QWidget_pos(QOpenWidgetH(theForm.Handle), @P);
  P.X:=P.X+WinPosCorrection[Ord(theForm.BorderStyle)].X;
  P.Y:=P.Y+WinPosCorrection[Ord(theForm.BorderStyle)].Y;

  if(theForm.ParentWidget<>nil)then
    QWidget_reparent(QOpenWidgetH(theForm.Handle),
      QOpenWidgetH(theForm.ParentWidget),
      QOpenWidget_getWFlags(QOpenWidgetH(theForm.Handle)),
      @P, True);
{$ENDIF}
end;

procedure TDMMain.MakeFormStayOnTop(theForm: TForm);
{$IFDEF MSWINDOWS}
var P: TPoint;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  QOpenWidget_setWFlags(QOpenWidgetH(TForm(theForm).Handle),
    Cardinal(WidgetFlags_WStyle_StaysOnTop));

  //Get Pos
  QWidget_pos(QOpenWidgetH(theForm.Handle), @P);
  P.X:=P.X+WinPosCorrection[Ord(theForm.BorderStyle)].X;
  P.Y:=P.Y+WinPosCorrection[Ord(theForm.BorderStyle)].Y;

  if(theForm.ParentWidget<>nil)then
    QWidget_reparent(QOpenWidgetH(TForm(theForm).Handle),
      QOpenWidgetH(TForm(theForm).ParentWidget),
      QOpenWidget_getWFlags(QOpenWidgetH(TForm(theForm).Handle)),
      @P, True);
{$ENDIF}
end;

function TDMMain.IsFormStayingOnTop(theForm: TForm): Boolean;
{$IFDEF MSWINDOWS}
var theWFlags: Cardinal;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  theWFlags:=QOpenWidget_getWFlags(QOpenWidgetH(theForm.Handle));

  IsFormStayingOnTop:=(theWFlags and Cardinal(WidgetFlags_WStyle_StaysOnTop))=Cardinal(WidgetFlags_WStyle_StaysOnTop);
{$ELSE}
  IsFormStayingOnTop:=True;
{$ENDIF}
end;

procedure TDMMain.NormalizeStayOnTopForms;
{$IFDEF MSWINDOWS}
var i: integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  LockFormDeactivateTracking:=True;

  TopMostForm:=Screen.ActiveForm;

  StayOnTopForms.Clear;
  for i:=Screen.FormCount-1 downto 0 do
    if(Screen.Forms[i].FormStyle=fsStayOnTop)and
      (Screen.Forms[i].Visible)then
    begin
      StayOnTopForms.Add(Screen.Forms[i]);
      DMMain.NormalizeStayOnTopForm(Screen.Forms[i]);

      Application.ProcessMessages;
    end;

  LockFormDeactivateTracking:=False;
{$ENDIF}
end;

procedure TDMMain.RestoreStayOnTopForms;
{$IFDEF MSWINDOWS}
var pos: integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  for pos:=0 to StayOnTopForms.Count-1 do
    if(pos<StayOnTopForms.Count)then
      DMMain.MakeFormStayOnTop(StayOnTopForms[pos]);

  if(TopMostForm<>nil)then
  begin
    TopMostForm.BringToFront;
    TopMostForm.SetFocus;
  end;

  StayOnTopForms.Clear;
{$ENDIF}
end;


//Workaround Code because of Delphi BUG END
//-----------------------------------------

function TDMMain.EncodeStreamForXML(theStream: TStream): string;
var s: string;
  theBuffer: Array [0..1024] of Char;
  BytesRead: integer;
  j: integer;
  s2: string;
  z1, z2: integer;
begin
  s:='';

  //Go back to Pos 0
  theStream.Position:=0;

  BytesRead:=1024;

  while(BytesRead=1024)do
  begin
    BytesRead:=theStream.Read(theBuffer, 1024);

    s2:='';
    for j:=0 to BytesRead-1 do
    begin
      z1:=Ord(theBuffer[j]) div 16;
      z2:=Ord(theBuffer[j])-z1*16;
      if(z1>9)then
        s2:=s2+Chr(Ord('A')+z1-10)
      else
        s2:=s2+Chr(Ord('0')+z1);

      if(z2>9)then
        s2:=s2+Chr(Ord('A')+z2-10)
      else
        s2:=s2+Chr(Ord('0')+z2);
    end;
    s:=s+s2;
  end;

  EncodeStreamForXML:=s;
end;


function TDMMain.DecodeStreamFromXML(XMLData: string; theStream: TStream): string;
var z1, z2: integer;
  theBuffer: Array [0..1024] of Char;
  Bytes2Write, BytesWritten: integer;
begin
  BytesWritten:=0;
  Bytes2Write:=0;
  while(BytesWritten*2<Length(XMLData))do
  begin
    z1:=Ord(XMLData[BytesWritten*2+1]);
    z2:=Ord(XMLData[BytesWritten*2+2]);

    if(z1-Ord('0')>9)then
      z1:=z1-Ord('A')+10
    else
      z1:=z1-Ord('0');

    if(z2-Ord('0')>9)then
      z2:=z2-Ord('A')+10
    else
      z2:=z2-Ord('0');

    theBuffer[Bytes2Write]:=Chr(z1*16+z2);

    inc(Bytes2Write);
    inc(BytesWritten);

    if(Bytes2Write=1024)or(BytesWritten*2=Length(XMLData))then
    begin
      theStream.Write(theBuffer, Bytes2Write);
      Bytes2Write:=0;
    end;
  end;

  //Go back to Pos 0
  theStream.Position:=0;
end;

function TDMMain.CheckIniFileVersion(IniFileName: string; neededVersion: integer): Boolean;
var theIniFile: TMemIniFile;
  s: string;
  CopyNewVersion: Boolean;
begin
  CheckIniFileVersion:=True;

  theIniFile:=TMemIniFile.Create(SettingsPath+IniFileName);
  try
    CopyNewVersion:=False;

    s:=theIniFile.ReadString('GeneralSettings', 'IniFileVersion', '0');

    try
      if(StrToInt(s)<>neededVersion)then
        CopyNewVersion:=True;
    except
      CopyNewVersion:=True
    end;

    if(CopyNewVersion)then
    begin
      CopyDiskFile(ExtractFilepath(Application.ExeName)+'Data'+PathDelim+IniFileName,
        SettingsPath+IniFileName, False);

      CheckIniFileVersion:=False;
    end;

  finally
    theIniFile.Free;
  end;
end;

function TDMMain.GetValidObjectName(name: string): string;
var i, namelength: integer;
begin
  namelength:=Length(name);
  i:=1;
  while(i<=namelength)do
  begin
    if(not(name[i] in VALID_OBJECTNAME_CHARS))then
    begin
      name:=Copy(name, 1, i-1)+Copy(name, i+1, namelength);
      dec(namelength);
    end
    else
      inc(i);
  end;

  GetValidObjectName:=name;
end;


procedure TDMMain.LoadApplicationFont;
var theIni: TMemIniFile;
  s: string;
begin
  //Read IniFile
  theIni:=TMemIniFile.Create(SettingsPath+ProgName+'_Settings.ini');
  try
    try
{$IFDEF LINUX}
      ApplicationFontName:=theIni.ReadString('GeneralSettings', 'ApplicationFontName', 'Nimbus Sans L');
      ApplicationFontSize:=StrToInt(theIni.ReadString('GeneralSettings', 'ApplicationFontSize', '11'));
{$ELSE}
      ApplicationFontName:=theIni.ReadString('GeneralSettings', 'ApplicationFontName', 'MS Sans Serif');
      ApplicationFontSize:=StrToInt(theIni.ReadString('GeneralSettings', 'ApplicationFontSize', '8'));
{$ENDIF}
      s:=theIni.ReadString('GeneralSettings', 'ApplicationFontStyle', '');
      ApplicationFontStyle:=[];
      if(Pos('bold', lowercase(s))>0)then
        ApplicationFontStyle:=ApplicationFontStyle+[fsBold]
      else if(Pos('italic', lowercase(s))>0)then
        ApplicationFontStyle:=ApplicationFontStyle+[fsItalic]
      else if(Pos('underline', lowercase(s))>0)then
        ApplicationFontStyle:=ApplicationFontStyle+[fsUnderline]
      else if(Pos('strikeout', lowercase(s))>0)then
        ApplicationFontStyle:=ApplicationFontStyle+[fsStrikeOut];
    except
{$IFDEF LINUX}
      ApplicationFontName:='Nimbus Sans L';
      ApplicationFontSize:=11;
      ApplicationFontStyle:=[];
{$ELSE}
      ApplicationFontName:='MS Sans Serif';
      ApplicationFontSize:=8;
      ApplicationFontStyle:=[];
{$ENDIF}
    end;

    Application.Font.Name:=ApplicationFontName;
    Application.Font.Size:=ApplicationFontSize;
    Application.Font.Style:=ApplicationFontStyle;
  finally
    theIni.Free;
  end;
end;

function sendCLXEvent(receiver: QObjectH; event: QEventH): Boolean;
begin
  try
    Result := QApplication_sendEvent(receiver, event);
  finally
    QEvent_destroy(event);
  end;
end;

end.


