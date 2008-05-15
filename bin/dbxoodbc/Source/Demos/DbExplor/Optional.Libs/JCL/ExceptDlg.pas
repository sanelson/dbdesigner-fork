{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is ExceptDlg.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Sample Application exception dialog replacement                                                  }
{                                                                                                  }
{ Last modified: $Date: 2006/03/07 00:00:00 $                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit ExceptDlg;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, JclDebug;

const
  UM_CREATEDETAILS = WM_USER + $100;
  {+}
  UM_ADDREPORT = UM_CREATEDETAILS + 1;
  cMaxCycledErrors = 7;
  cMaxErrors = 100; // or = MaxInt
  {+.}

  ReportToLogEnabled   = $00000001; // TExceptionDialog.Tag property
  DisableTextScrollbar = $00000002; // TExceptionDialog.Tag property

type
  TSimpleExceptionLog = class (TObject)
  private
    FLogFileHandle: THandle;
    FLogFileName: string;
    FLogWasEmpty: Boolean;
    function GetLogOpen: Boolean;
  protected
    function CreateDefaultFileName: string;
  public
    constructor Create(const ALogFileName: string = '');
    destructor Destroy; override;
    procedure CloseLog;
    procedure OpenLog;
    procedure Write(const Text: string; Indent: Integer = 0); overload;
    procedure Write(Strings: TStrings; Indent: Integer = 0); overload;
    procedure WriteStamp(SeparatorLen: Integer = 0);
    property LogFileName: string read FLogFileName;
    property LogOpen: Boolean read GetLogOpen;
  end;

  TExcDialogSystemInfo = (siStackList, siOsInfo, siModuleList, siActiveControls);
  TExcDialogSystemInfos = set of TExcDialogSystemInfo;
  {+}
  TExceptionDialog = class;
  TOnShowException = procedure(Sender: TExceptionDialog; E: Exception) of object;
  {+.}

  TExceptionDialog = class(TForm)
    OkBtn: TButton;
    DetailsMemo: TMemo;
    DetailsBtn: TButton;
    Bevel1: TBevel;
    TextLabel: TMemo;
    CopyButton: TButton;
    ShapeDetailsMemo: TShape;
    ShapeTextLabel: TShape;
    LErrorCountCaption: TLabel;
    LErrorCount: TLabel;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
  private
    FDetailsVisible: Boolean;
    FIsMainThead: Boolean;
    FLastActiveControl: TWinControl;
    FNonDetailsHeight: Integer;
    FFullHeight: Integer;
    FSimpleLog: TSimpleExceptionLog;
    FDetailsCalculated: Boolean;

    {+}
    fEMessage: string;
    fStackListCaption: string;
    fStackListFirst: TJclStackInfoList;
    fExceptAddrFirst: Pointer;
    fLastActiveControlFirst: TWinControl;
    {+.}

    procedure CreateDetails;
    function GetReportAsText: string;
    procedure ReportToLog;
    procedure SetDetailsVisible(const Value: Boolean);
    procedure UMCreateDetails(var Message: TMessage); message UM_CREATEDETAILS;
    {+}
    procedure MakeStackList();
    procedure CleanStackList();
    procedure DoCalculateDetails();
    procedure UMAddReport(var Message: TMessage); message UM_ADDREPORT;
    procedure AddReport();
    procedure DoAddReport();
    {+.}
  protected
    procedure AfterCreateDetails; dynamic;
    procedure BeforeCreateDetails; dynamic;
    procedure CreateDetailInfo; dynamic;
    procedure CreateReport(const SystemInfo: TExcDialogSystemInfos);
    function ReportMaxColumns: Integer; virtual;
    function ReportNewBlockDelimiterChar: Char; virtual;
    procedure NextDetailBlock;
    {+}
    function DelimiterStr: string;
    {+.}
    procedure UpdateTextLabelScrollbars;

  {+}
  protected
    fErrorIcon: HICON;
    fSaveDetailsVisible: Boolean;
    fSaveHeight: Integer;
    fOnShowException: TOnShowException;
    iErrorCount: Integer;

    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure DoOnShowException(E: Exception); virtual;
    procedure PaintIcon(); virtual;
  {+.}
  public
    procedure CopyReportToClipboard;
    class procedure ExceptionHandler(Sender: TObject; E: Exception);
    class procedure ExceptionThreadHandler(Thread: TJclDebugThread);
    class procedure ShowException(E: Exception; Thread: TJclDebugThread);
    property DetailsVisible: Boolean read FDetailsVisible write SetDetailsVisible;
    property ReportAsText: string read GetReportAsText;
    property SimpleLog: TSimpleExceptionLog read FSimpleLog;
  end;

  TExceptionDialogClass = class of TExceptionDialog;

var
  ExceptionDialogClass: TExceptionDialogClass = TExceptionDialog;
  {+}
  FirstExceptionImmediateStackCalculate: Boolean = False;
  {+.}
  {+}
  {English:}
  LText_OK: string = '&OK';
  LText_CopyAll: string = '&Copy All';
  LText_Details: string = '&Details';
  {}

  {Russian:}
  {
  LText_OK: string = '&Закрыть';
  LText_CopyAll: string = '&Копировать всё';
  LText_Details: string = '&Детально';
  {}

  {+.}

  {+}
  procedure InitializeHandler;
  procedure UnInitializeHandler;
  function IsInitialized: Boolean;
  {+.}

implementation

{$R *.DFM}

uses
  ClipBrd, Math,
  JclBase, JclFileUtils, JclHookExcept, JclPeImage, JclStrings, JclSysInfo, JclSysUtils;

resourcestring
  RsAppError = '%s - application error';
  RsExceptionClass = 'Exception class: %s';
  RsExceptionAddr = 'Exception address: %p';
  RsStackList = 'Stack list, generated %s';
  RsModulesList = 'List of loaded modules:';
  RsOSVersion = 'System   : %s %s, Version: %d.%d, Build: %x, "%s"';
  RsProcessor = 'Processor: %s, %s, %d MHz %s%s';
  RsScreenRes = 'Display  : %dx%d pixels, %d bpp';
  RsActiveControl = 'Active Controls hierarchy:';
  {+}
  RsInvalidAddress = 'Invalid %s Address:';
  {+.}
  RsThread = 'Thread: %s';
  RsMissingVersionInfo = '(no version info)';

var
  ExceptionDialog: TExceptionDialog;

//==================================================================================================
// Helper routines
//==================================================================================================

function GetBPP: Integer;
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES);
  ReleaseDC(0, DC);
end;

//--------------------------------------------------------------------------------------------------

function SortModulesListByAddressCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Integer(List.Objects[Index1]) - Integer(List.Objects[Index2]);
end;

//==================================================================================================
// TApplication.HandleException method code hooking for exceptions from DLLs
//==================================================================================================

// We need to catch the last line of TApplication.HandleException method:
// [...]
//   end else
//    SysUtils.ShowException(ExceptObject, ExceptAddr);
// end;

procedure HookShowException(ExceptObject: TObject; ExceptAddr: Pointer);
begin
  if JclValidateModuleAddress(ExceptAddr) and (ExceptObject.InstanceSize >= Exception.InstanceSize) then
    TExceptionDialog.ExceptionHandler(nil, Exception(ExceptObject))
  else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

//--------------------------------------------------------------------------------------------------

function HookTApplicationHandleException: Boolean;
const
  CallOffset      = $86;
  CallOffsetDebug = $94;
type
  PCALLInstruction = ^TCALLInstruction;
  TCALLInstruction = packed record
    Call: Byte;
    Address: Integer;
  end;
var
  TApplicationHandleExceptionAddr, SysUtilsShowExceptionAddr: Pointer;
  CALLInstruction: TCALLInstruction;
  CallAddress: Pointer;
  NW: DWORD;

  function CheckAddressForOffset(Offset: Cardinal): Boolean;
  begin
    try
      CallAddress := Pointer(Cardinal(TApplicationHandleExceptionAddr) + Offset);
      CALLInstruction.Call := $E8;
      Result := PCALLInstruction(CallAddress)^.Call = CALLInstruction.Call;
      if Result then
      begin
        if IsCompiledWithPackages then
          Result := PeMapImgResolvePackageThunk(Pointer(Integer(CallAddress) + Integer(PCALLInstruction(CallAddress)^.Address) + SizeOf(CALLInstruction))) = SysUtilsShowExceptionAddr
        else
          Result := PCALLInstruction(CallAddress)^.Address = Integer(SysUtilsShowExceptionAddr) - Integer(CallAddress) - SizeOf(CALLInstruction);
      end;
    except
      Result := False;
    end;
  end;

begin
  TApplicationHandleExceptionAddr := PeMapImgResolvePackageThunk(@TApplication.HandleException);
  SysUtilsShowExceptionAddr := PeMapImgResolvePackageThunk(@SysUtils.ShowException);
  Result := CheckAddressForOffset(CallOffset) or CheckAddressForOffset(CallOffsetDebug);
  if Result then
  begin
    CALLInstruction.Address := Integer(@HookShowException) - Integer(CallAddress) - SizeOf(CALLInstruction);
    Result := WriteProcessMemory(GetCurrentProcess, CallAddress, @CALLInstruction, SizeOf(CALLInstruction), NW);
    if Result then
      FlushInstructionCache(GetCurrentProcess, CallAddress, SizeOf(CALLInstruction));
  end;
end;

//==================================================================================================
// TSimpleExceptionLog
//==================================================================================================

procedure TSimpleExceptionLog.CloseLog;
begin
  if LogOpen then
  begin
    CloseHandle(FLogFileHandle);
    FLogFileHandle := INVALID_HANDLE_VALUE;
    FLogWasEmpty := False;
  end;
end;

//--------------------------------------------------------------------------------------------------

constructor TSimpleExceptionLog.Create(const ALogFileName: string);
begin
  if ALogFileName = '' then
    FLogFileName := CreateDefaultFileName
  else
    FLogFileName := ALogFileName;
  FLogFileHandle := INVALID_HANDLE_VALUE;
end;

//--------------------------------------------------------------------------------------------------

function TSimpleExceptionLog.CreateDefaultFileName: string;
begin
  Result := PathExtractFileDirFixed(ParamStr(0)) + PathExtractFileNameNoExt(ParamStr(0)) + '_Err.log';
end;

//--------------------------------------------------------------------------------------------------

destructor TSimpleExceptionLog.Destroy;
begin
  CloseLog;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TSimpleExceptionLog.GetLogOpen: Boolean;
begin
  Result := FLogFileHandle <> INVALID_HANDLE_VALUE;
end;

//--------------------------------------------------------------------------------------------------

procedure TSimpleExceptionLog.OpenLog;
begin
  if not LogOpen then
  begin
    FLogFileHandle := CreateFile(PChar(FLogFileName), GENERIC_WRITE, FILE_SHARE_READ, nil,
      OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if LogOpen then
      FLogWasEmpty := SetFilePointer(FLogFileHandle, 0, nil, FILE_END) = 0;
  end
  else
    FLogWasEmpty := False;
end;

//--------------------------------------------------------------------------------------------------

procedure TSimpleExceptionLog.Write(const Text: string; Indent: Integer);
var
  S: string;
  SL: TStringList;
  I: Integer;
begin
  if LogOpen then
  begin
    SL := TStringList.Create;
    try
      SL.Text := Text;
      for I := 0 to SL.Count - 1 do
      begin
        S := StringOfChar(' ', Indent) + StrEnsureSuffix(AnsiCrLf, TrimRight(SL[I]));
        FileWrite(Integer(FLogFileHandle), Pointer(S)^, Length(S));
      end;
    finally
      SL.Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TSimpleExceptionLog.Write(Strings: TStrings; Indent: Integer);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Write(Strings[I], Indent);
end;

//--------------------------------------------------------------------------------------------------

procedure TSimpleExceptionLog.WriteStamp(SeparatorLen: Integer);
begin
  if SeparatorLen = 0 then
    SeparatorLen := 100;
  SeparatorLen := Max(SeparatorLen, 20);
  OpenLog;
  if not FLogWasEmpty then
    Write(AnsiCrLf);
  Write(StrRepeat('=', SeparatorLen));
  Write(Format('= %-*s =', [SeparatorLen - 4, DateTimeToStr(Now)]));
  Write(StrRepeat('=', SeparatorLen));
end;

//==================================================================================================
// Exception dialog
//==================================================================================================

var
  ExceptionShowing: Boolean;

{ TExceptionDialog }

procedure TExceptionDialog.AfterCreateDetails;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.BeforeCreateDetails;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.CopyReportToClipboard;
begin
  ClipBoard.AsText := ReportAsText;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.CreateDetailInfo;
begin
  CreateReport([siStackList, siOsInfo, siModuleList, siActiveControls]);
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.CreateDetails;
begin
  Screen.Cursor := crHourGlass;
  DetailsMemo.Lines.BeginUpdate;
  try
    CreateDetailInfo;
    {+}
    if iErrorCount = 1 then
      ReportToLog;
    {+.}
    DetailsMemo.SelStart := 0;
    SendMessage(DetailsMemo.Handle, EM_SCROLLCARET, 0, 0);
    AfterCreateDetails;
  finally
    DetailsMemo.Lines.EndUpdate;
    OkBtn.Enabled := True;
    DetailsBtn.Enabled := True;
    {+}
    CopyButton.Enabled := True;
    {+.}
    OkBtn.SetFocus;
    Screen.Cursor := crDefault;
  end;
end;

//--------------------------------------------------------------------------------------------------

{+}
procedure TExceptionDialog.AddReport();
var
  S: string;
begin
  DoCalculateDetails();
  S := DelimiterStr + #13#10 + LErrorCount.Caption + ')'#13#10 +
    TextLabel.Lines.Text +
    #13#10 + DelimiterStr + #13#10 +
    DetailsMemo.Lines.Text;
  Inc(iErrorCount);
  LErrorCount.Caption := IntToStr(iErrorCount);
  LErrorCount.Visible := True;
  LErrorCountCaption.Visible := True;
  DetailsMemo.Lines.BeginUpdate;
  TextLabel.Lines.BeginUpdate;
  try
    DetailsMemo.Lines.Clear;
    TextLabel.Lines.Clear;
    CreateReport([siStackList, siActiveControls]);
    TextLabel.Lines.Text := fEMessage;
    fEMessage := '';
    DetailsMemo.Lines.Text := DetailsMemo.Lines.Text + S;
    DetailsMemo.SelStart := 0;
    SendMessage(DetailsMemo.Handle, EM_SCROLLCARET, 0, 0);
  finally
    DetailsMemo.Lines.EndUpdate;
    TextLabel.Lines.EndUpdate;
  end;
end;

procedure TExceptionDialog.DoAddReport();
begin
  if FIsMainThead and (GetWindowThreadProcessId(Handle, nil) = MainThreadID) then
    PostMessage(Handle, UM_ADDREPORT, 0, 0)
  else
    AddReport();
end;

{+.}

{+}
procedure TExceptionDialog.MakeStackList();
begin
  FreeAndNil(fStackListFirst);

  fStackListFirst := JclLastExceptStackList;
  if fStackListFirst <> nil then
  begin
    JclRemoveLastExceptStackList();
    fStackListCaption := RsStackList;
  end
  else
  begin
    fStackListCaption := '?@Long: ' + RsStackList;
    fStackListFirst := JclCreateStackList({Raw=}True, 7, ExceptAddr{nil});
    CorrectExceptStackListTop(fStackListFirst, False);
  end;

  fExceptAddrFirst := ExceptAddr;
  fLastActiveControlFirst := FLastActiveControl;
end;

procedure TExceptionDialog.CleanStackList();
begin
  if fStackListFirst <> nil then
  begin
    fStackListCaption := '';
    fLastActiveControlFirst := nil;
    fExceptAddrFirst := nil;
    FreeAndNil(fStackListFirst);
  end;
end;
{+.}

procedure TExceptionDialog.CreateReport(const SystemInfo: TExcDialogSystemInfos);
const
  MMXText: array[Boolean] of PChar = ('', 'MMX');
  FDIVText: array[Boolean] of PChar = (' [FDIV Bug]', '');
var
  SL: TStringList;
  I: Integer;
  ModuleName: TFileName;
  CpuInfo: TCpuInfo;
  C: TWinControl;
  NtHeaders: PImageNtHeaders;
  ModuleBase: Cardinal;
  ImageBaseStr: string;
  StackList: TJclStackInfoList;

  {+}
  function IsValidBlockAddr(Addr: Pointer; Size: DWord): boolean;
  begin
    Result := (FindHInstance(Pointer(Addr)) <> 0) and
      (not IsBadReadPtr(Pointer(Addr), Size));
  end;
  {+.}

begin
  SL := TStringList.Create;
  try
    {+}
    DetailsMemo.Lines.BeginUpdate;
    {+.}
    // Stack list
    if siStackList in SystemInfo then
    begin
      {+}
      if fStackListFirst <> nil then
      begin
        DetailsMemo.Lines.Add(Format(fStackListCaption, [DateTimeToStr(fStackListFirst.TimeStamp)]));
        fStackListFirst.AddToStrings(DetailsMemo.Lines, False, True, True);
        NextDetailBlock;
      end
      else
      begin
      {+.}
        StackList := JclLastExceptStackList;
        if Assigned(StackList) then
        begin
          DetailsMemo.Lines.Add(Format(RsStackList, [DateTimeToStr(StackList.TimeStamp)]));
          StackList.AddToStrings(DetailsMemo.Lines, False, True, True);
          NextDetailBlock;
        {+}
        end
        else
        begin
          StackList := JclCreateStackList({Raw=}True, 7, ExceptAddr{nil});
          CorrectExceptStackListTop(StackList, False);

          DetailsMemo.Lines.Add('?@Long: ' + Format(RsStackList, [DateTimeToStr(StackList.TimeStamp)]));
          StackList.AddToStrings(DetailsMemo.Lines, False, True, True);
          NextDetailBlock;

          StackList.Free;
        {+.}
        end;
      end;
    end;
    {+}
    // Active controls
    if (siActiveControls in SystemInfo) and (FLastActiveControl <> nil) then
    begin
      DetailsMemo.Lines.Add(RsActiveControl);

      {+}
      if fStackListFirst <> nil then
        C := fLastActiveControlFirst
      else
        C := FLastActiveControl;
      {+.}

      while C <> nil do
      begin
        if IsValidBlockAddr(C, SizeOf(C)) then
        begin
          try
            DetailsMemo.Lines.Add(Format('%s "%s"', [C.ClassName, C.Name]));
            C := C.Parent;
          except
            DetailsMemo.Lines.Add(
              Format(RsInvalidAddress, ['parent']) + IntToHex(Integer(C), 0)
            );
            C := nil;
          end;
        end
        else
        begin
          DetailsMemo.Lines.Add(
            Format(RsInvalidAddress, ['parent']) + IntToHex(Integer(C), 0)
          );
          C := nil;
        end;
      end;
      NextDetailBlock;
    end;
    {+.}
    // System and OS information
    if siOsInfo in SystemInfo then
    begin
      DetailsMemo.Lines.Add(Format(RsOSVersion, [GetWindowsVersionString, NtProductTypeString,
        Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]));
      GetCpuInfo(CpuInfo);
      with CpuInfo do
        DetailsMemo.Lines.Add(Format(RsProcessor, [Manufacturer, CpuName,
          RoundFrequency(FrequencyInfo.NormFreq),
          MMXText[MMX], FDIVText[IsFDIVOK]]));
      DetailsMemo.Lines.Add(Format(RsScreenRes, [Screen.Width, Screen.Height, GetBPP]));
      NextDetailBlock;
    end;
    // Modules list
    if (siModuleList in SystemInfo) and LoadedModulesList(SL, GetCurrentProcessId) then
    begin
      DetailsMemo.Lines.Add(RsModulesList);
      SL.CustomSort(SortModulesListByAddressCompare);
      for I := 0 to SL.Count - 1 do
      begin
        ModuleName := SL[I];
        ModuleBase := Cardinal(SL.Objects[I]);
        DetailsMemo.Lines.Add(Format('[%.8x] %s', [ModuleBase, ModuleName]));
        NtHeaders := PeMapImgNtHeaders(Pointer(ModuleBase));
        if (NtHeaders <> nil) and (NtHeaders^.OptionalHeader.ImageBase <> ModuleBase) then
          ImageBaseStr := Format('<%.8x> ', [NtHeaders^.OptionalHeader.ImageBase])
        else
          ImageBaseStr := StrRepeat(' ', 11);
        if VersionResourceAvailable(ModuleName) then
          with TJclFileVersionInfo.Create(ModuleName) do
          try
            DetailsMemo.Lines.Add(ImageBaseStr + BinFileVersion + ' - ' + FileVersion);
            if FileDescription <> '' then
              DetailsMemo.Lines.Add(StrRepeat(' ', 11) + FileDescription);
          finally
            Free;
          end
        else
          DetailsMemo.Lines.Add(ImageBaseStr + RsMissingVersionInfo);
      end;
      NextDetailBlock;
    end;
  finally
    {+}
    CleanStackList();
    DetailsMemo.Lines.EndUpdate;
    {+.}
    SL.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.DetailsBtnClick(Sender: TObject);
begin
  DetailsVisible := not DetailsVisible;
end;

//--------------------------------------------------------------------------------------------------
{+}

procedure TExceptionDialog.CopyButtonClick(Sender: TObject);
begin
  {+}
  DoCalculateDetails();
  {+.}
  CopyReportToClipboard;
  MessageBeep(MB_OK);
end;
{+.}

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialog.ExceptionHandler(Sender: TObject; E: Exception);
begin
  if ExceptionShowing then
  begin
    //
    // OLD:
    //
    //{ Application.ShowException(E); }

    //
    // NEW:
    //
    if ExceptionDialog.iErrorCount <= cMaxCycledErrors then
    begin
      ExceptionDialog.fEMessage := E.Message;
      ExceptionDialog.DoAddReport();
    end
    else
    begin
      if ExceptionDialog.iErrorCount < cMaxErrors then
      begin
        Inc(ExceptionDialog.iErrorCount);
        if ExceptionDialog.iErrorCount < cMaxErrors then
          ExceptionDialog.LErrorCount.Caption := IntToStr(ExceptionDialog.iErrorCount)
        else
          ExceptionDialog.LErrorCount.Caption := '...';
      end;
      //{ Windows.OutputDebugString( PChar('Exception cycled:' + E.Message) ); }
    end;
  end
  else
  begin
    ExceptionShowing := True;
    try
      ShowException(E, nil);
    finally
      ExceptionShowing := False;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialog.ExceptionThreadHandler(Thread: TJclDebugThread);
begin
  if ExceptionShowing then
    Application.ShowException(Thread.SyncException)
  else
  begin
    ExceptionShowing := True;
    try
      ShowException(Thread.SyncException, Thread);
    finally
      ExceptionShowing := False;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormCreate(Sender: TObject);
{+}
var
  iW, i, iDelta, iClientWidth: Integer;
{+.}
begin
  FSimpleLog := TSimpleExceptionLog.Create;
  FFullHeight := ClientHeight;
  {+}
  iErrorCount := 1;
  Caption := Format(RsAppError, [Application.Title]);
  DetailsMemo.Lines.Clear;
  TextLabel.Lines.Clear;

  fErrorIcon := LoadIcon(0, IDI_ERROR);

  OkBtn.Caption := LText_OK;
  iW := Canvas.TextWidth(LText_OK);

  CopyButton.Caption := LText_CopyAll;
  i := Canvas.TextWidth(LText_CopyAll);
  if i > iW then
    iW := i;

  DetailsBtn.Caption := LText_Details;
  DetailsVisible := False;
  i := Canvas.TextWidth(DetailsBtn.Caption);
  if i > iW then
    iW := i;

  with ClientRect do
  begin
    iClientWidth := (Right - Left) + 1;
    iDelta := (Self.Width - iClientWidth) div 2;
  end;

  Inc(iW, 4 + iDelta + iDelta);

  if iW < 75 then
    iW := 75;

  i := iClientWidth - iW - 3 - iDelta + 1;
  OkBtn.Left := i;
  CopyButton.Left := i;
  DetailsBtn.Left := i;

  OkBtn.Width := iW;
  CopyButton.Width := iW;
  DetailsBtn.Width := iW;

  TextLabel.Width := OkBtn.Left - 4 - TextLabel.Left;
  {+.}
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSimpleLog);
  {+}
  CleanStackList();
  {+.}
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  {+}
  if (Key = VK_ESCAPE) and (Shift = []) then
    Close;
  {+.}
end;

//--------------------------------------------------------------------------------------------------

{+}
procedure TExceptionDialog.PaintIcon();
begin
  DrawIcon(Canvas.Handle, TextLabel.Left - GetSystemMetrics(SM_CXICON) - 15,
    TextLabel.Top, fErrorIcon );
end;
{+.}

procedure TExceptionDialog.FormPaint(Sender: TObject);
begin
  {+}
  PaintIcon();
  {+.}
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.FormResize(Sender: TObject);
begin
  UpdateTextLabelScrollbars;
end;

//--------------------------------------------------------------------------------------------------

{+}
procedure TExceptionDialog.DoCalculateDetails();
begin
  if FDetailsCalculated then
    Exit;
  FDetailsCalculated := True;
  BeforeCreateDetails;
  MessageBeep(MB_ICONERROR);
  if FIsMainThead and (GetWindowThreadProcessId(Handle, nil) = MainThreadID) then
    PostMessage(Handle, UM_CREATEDETAILS, 0, 0)
  else
    CreateDetails;
end;
{+.}

procedure TExceptionDialog.FormShow(Sender: TObject);
begin
  {+}
  OnShow := nil;
  {+.}
  {+}
  if FirstExceptionImmediateStackCalculate then
  begin
    DetailsBtn.Enabled := False;
    DoCalculateDetails();
  end
  else
    MakeStackList();
  {+.}
end;

//--------------------------------------------------------------------------------------------------

function TExceptionDialog.GetReportAsText: string;
begin
  Result := StrEnsureSuffix(AnsiCrLf, TextLabel.Text) + AnsiCrLf + DetailsMemo.Text;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.NextDetailBlock;
begin
  DetailsMemo.Lines.Add(StrRepeat(ReportNewBlockDelimiterChar, ReportMaxColumns));
end;

{+}
function TExceptionDialog.DelimiterStr: string;
begin
  Result := StrRepeat(ReportNewBlockDelimiterChar, ReportMaxColumns);
end;
{+.}

//--------------------------------------------------------------------------------------------------

function TExceptionDialog.ReportMaxColumns: Integer;
begin
  Result := 100;
end;

//--------------------------------------------------------------------------------------------------

function TExceptionDialog.ReportNewBlockDelimiterChar: Char;
begin
  Result := '-';
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.ReportToLog;
begin
  if Tag and ReportToLogEnabled <> 0 then
  begin
    FSimpleLog.WriteStamp(ReportMaxColumns);
    try
      FSimpleLog.Write(ReportAsText);
    finally
      FSimpleLog.CloseLog;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.SetDetailsVisible(const Value: Boolean);
var
  DetailsCaption: string;
begin
  {+}
  if (FNonDetailsHeight > 0) and (FDetailsVisible = Value) then
  begin
    if not Value then
    begin
      ClientHeight := Bevel1.Top;
      FNonDetailsHeight := Height;
    end;
    Exit;
  end;
  if Value then
    DoCalculateDetails();
  {+.}
  FDetailsVisible := Value;
  DetailsCaption := Trim(StrRemoveChars(DetailsBtn.Caption, ['<', '>']));
  if Value then
  begin
    Constraints.MinHeight := FNonDetailsHeight + 100;
    Constraints.MaxHeight := Screen.Height;
    DetailsCaption := '<< ' + DetailsCaption;
    ClientHeight := FFullHeight;
    DetailsMemo.Height := FFullHeight - DetailsMemo.Top - 3;
  end
  else
  begin
    FFullHeight := ClientHeight;
    DetailsCaption := DetailsCaption + ' >>';
    if FNonDetailsHeight = 0 then
    begin
      ClientHeight := Bevel1.Top;
      FNonDetailsHeight := Height;
    end
    else
      Height := FNonDetailsHeight;
    Constraints.MinHeight := FNonDetailsHeight;
    Constraints.MaxHeight := FNonDetailsHeight
  end;
  DetailsBtn.Caption := DetailsCaption;
  DetailsMemo.Enabled := Value;
end;

//--------------------------------------------------------------------------------------------------

{+}
procedure TExceptionDialog.DoOnShowException(E: Exception);
begin
  if Assigned(fOnShowException) then
    fOnShowException(Self, E);
end;
{+.}

//--------------------------------------------------------------------------------------------------

class procedure TExceptionDialog.ShowException(E: Exception; Thread: TJclDebugThread);
begin
  if ExceptionDialog = nil then
    ExceptionDialog := ExceptionDialogClass.Create(Application);
  try
    with ExceptionDialog do
    begin
      FIsMainThead := (GetCurrentThreadId = MainThreadID);
      FLastActiveControl := Screen.ActiveControl;
      TextLabel.Text := AdjustLineBreaks(StrEnsureSuffix('.', E.Message));
      UpdateTextLabelScrollbars;
      DetailsMemo.Lines.Add(Format(RsExceptionClass, [E.ClassName]));
      if Thread = nil then
        DetailsMemo.Lines.Add(Format(RsExceptionAddr, [ExceptAddr]))
      else
        DetailsMemo.Lines.Add(Format(RsThread, [Thread.ThreadInfo]));
      NextDetailBlock;
      {+}
      DoOnShowException(E);
      {+.}
      ShowModal;
    end;
  finally
    FreeAndNil(ExceptionDialog);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.UMCreateDetails(var Message: TMessage);
begin
  Update;
  CreateDetails;
end;

{+}
procedure TExceptionDialog.UMAddReport(var Message: TMessage);
begin
  Update();
  AddReport();
end;
{+.}

//--------------------------------------------------------------------------------------------------

procedure TExceptionDialog.UpdateTextLabelScrollbars;
begin
  if Tag and DisableTextScrollbar = 0 then
  begin
    Canvas.Font := TextLabel.Font;
    if TextLabel.Lines.Count * Canvas.TextHeight('Wg') > TextLabel.ClientHeight then
      TextLabel.ScrollBars := ssVertical
    else
      TextLabel.ScrollBars := ssNone;
   end;
   {+}
   ShapeTextLabel.Left := TextLabel.Left - 1;
   ShapeTextLabel.Top := OkBtn.Top;
   TextLabel.Height := Bevel1.Top - TextLabel.Top - 1;
   ShapeTextLabel.Height := TextLabel.Top - ShapeTextLabel.Top + TextLabel.Height + 1;
   ShapeTextLabel.Width := TextLabel.Width + 2;

   ShapeDetailsMemo.Left := 1;
   ShapeDetailsMemo.Top := Bevel1.Top - 1;
   ShapeDetailsMemo.Width := ClientWidth - 2;
   ShapeDetailsMemo.Height := DetailsMemo.Top + DetailsMemo.Height - ShapeDetailsMemo.Top + 2;
   {+.}
end;

//--------------------------------------------------------------------------------------------------

{+}
procedure TExceptionDialog.WMSysCommand(var Message: TWMSysCommand);
begin
 case Message.CmdType of
   { not supported }
   //SC_MINIMIZE:
   //  begin
   //  end;
   SC_MAXIMIZE:
     begin
       DoCalculateDetails();
       if DetailsBtn.Enabled then
       begin
         fSaveDetailsVisible := DetailsVisible;
         fSaveHeight := FFullHeight;
         if not fSaveDetailsVisible then
           DetailsVisible := True;
         DetailsBtn.Enabled := False;
       end
       else
       begin
         Exit;
       end;
     end;
   SC_RESTORE:
     begin
       if not fSaveDetailsVisible then
       begin
         DetailsVisible := False;
       end;
       FFullHeight := fSaveHeight;
       DetailsBtn.Enabled := True;
     end;
  end;
 inherited;
end;
{+.}

//==================================================================================================
// Exception handler initialization code
//==================================================================================================

{+}
var
  bIsInitialized: Boolean = False;

function IsInitialized: Boolean;
begin
  Result := bIsInitialized;
end;
{+.}

procedure InitializeHandler;
begin
  {+}
  if bIsInitialized then
    exit;
  {+.}
  JclStackTrackingOptions := JclStackTrackingOptions + [stRawMode];
  {$IFNDEF HOOK_DLL_EXCEPTIONS}
  JclStackTrackingOptions := JclStackTrackingOptions + [stStaticModuleList];
  {$ENDIF HOOK_DLL_EXCEPTIONS}
  JclDebugThreadList.OnSyncException := TExceptionDialog.ExceptionThreadHandler;
  JclStartExceptionTracking;
  {$IFDEF HOOK_DLL_EXCEPTIONS}
  if HookTApplicationHandleException then
    JclTrackExceptionsFromLibraries;
  {$ENDIF HOOK_DLL_EXCEPTIONS}
  Application.OnException := TExceptionDialog.ExceptionHandler;
  {+}
  bIsInitialized := True;
  {+.}
end;

//--------------------------------------------------------------------------------------------------

procedure UnInitializeHandler;
begin
  {+}
  if not bIsInitialized then
    exit;
  {+.}
  Application.OnException := nil;
  JclDebugThreadList.OnSyncException := nil;
  JclUnhookExceptions;
  JclStopExceptionTracking;
  {+}
  bIsInitialized := False;
  {+.}
end;

//--------------------------------------------------------------------------------------------------


initialization
  //if not ( ( GetModuleHandle('delphi32.exe') <> 0 ) and ( GetModuleHandle('DCC.DLL') <> 0 ) ) then
  //if ( GetModuleHandle('delphi32.exe') <> 0 ) and ( GetModuleHandle('DCC.DLL') <> 0 ) then
  InitializeHandler;

finalization
  UnInitializeHandler;

end.
