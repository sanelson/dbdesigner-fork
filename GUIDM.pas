unit GUIDM;

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
// Unit GUIDM.pas
// --------------
// Version 1.1, 31.03.2003, Mike
// Description
//   Contains help functions for the MainForm
//
// Changes:
//   Version 1.1, 31.03.2003, Mike
//     added RecentOpenSQLScriptFileDir, loading from, saving to ini-file
//   Version 1.0, 13.03.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, QTypes, IniFiles, QForms, QStyle, QMenus, QExtCtrls,
  Qt, QGraphics;

type
  TDMGUI = class(TDataModule)
    ApplActive: TTimer;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

    procedure LoadSettingsFromIniFile;
    procedure SaveSettingsToIniFile;

    function ApplRunFirstTime: Boolean;

    procedure AddFileToRecentFilesList(fname: string);
    function GetRecentFileCount: integer;
    function GetRecentFileName(id: integer): string;
    procedure OpenRecentFile(Sender: TObject);

    procedure SetQueryStatusLbl;
    procedure SetStatusCaption(s: string);

    function LoadTip: string;

    procedure CopyInitialSettingsToPrivatSettingsDir;

    procedure CheckIniFiles;
  private
    { Private declarations }
    RunFirstTime: Boolean;
    RecentFiles: TStringList;

    RecentFilesMICounter: integer; //used for naming RecentFileMenuItems
  public
    { Public declarations }
    RecentOpenFileDir,
    RecentSaveModelAsImageDir,
    RecentExportSQLCreatesDir,
    RecentOpenSQLScriptFileDir,
    RecentImportFileDir: string;

    DelaySplashScreen: integer;

    ReopenLastFile: Boolean;

    ShowPalettesDocked: Boolean;

    FormsOnTop: TList;

    ShowTipsOnStartup: Boolean;
    LastTipShown: integer;

    SQLTextFont: string;
    SQLTextFontSize: integer;

    MinimizeOnCtrlShiftC: Boolean;

    //Parameters for the Docked Query Editor
    DockedQueryPnlMode,
    DockedQueryPnlHeightM1,
    DockedQueryPnlHeightM2,
    DockedQueryPnlStoredSQLTreeWidthM1,
    DockedQueryPnlStoredSQLTreeWidthM2,
    DockedQueryPnlBLOBPnlWidthM1,
    DockedQueryPnlBLOBPnlWidthM2,
    DockedQueryPnlSQLPnlSizeM1,
    DockedQueryPnlSQLPnlSizeM2: integer;

    DockedQueryPnlStoredSQLTreeVisibleM1,
    DockedQueryPnlStoredSQLTreeVisibleM2,
    DockedQueryPnlBLOBPnlVisibleM1,
    DockedQueryPnlBLOBPnlVisibleM2: Boolean;

    UseSQLSyntaxHighlighting: Boolean;
    IgnoreSQLHistoryChange: Boolean;
  end;

const
  QEventType_SetApplStyle = QEventType(Integer(QEventType_ClxUser) + 100);
  QEventType_SetSQLTextFont = QEventType(Integer(QEventType_ClxUser) + 101);

var
  DMGUI: TDMGUI;

implementation

uses Main, MainDM, DBDM;

{$R *.xfm}


procedure TDMGUI.DataModuleCreate(Sender: TObject);
begin
  RunFirstTime:=False;

  RecentFiles:=TStringList.Create;
  FormsOnTop:=TList.Create;

  //Copy setting files or check version
  CheckIniFiles;

  RecentOpenFileDir:='';
  RecentExportSQLCreatesDir:='';
  RecentOpenSQLScriptFileDir:='';
  RecentImportFileDir:='';
  RecentFilesMICounter:=1;

  LoadSettingsFromIniFile;

  Application.HintHidePause:=10000;
end;

procedure TDMGUI.DataModuleDestroy(Sender: TObject);
begin
  SaveSettingsToIniFile;
  
  RecentFiles.Free;
  FormsOnTop.Free;
end;

function TDMGUI.ApplRunFirstTime: Boolean;
begin
  ApplRunFirstTime:=RunFirstTime;
end;

procedure TDMGUI.LoadSettingsFromIniFile;
var theIni: TMemIniFile;
  i, ApplStyle: Integer;
  theStringList: TStringList;
begin
  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_Settings.ini');
  try
    //Check if the Program is opened the very first time
    if(theIni.ReadString('GeneralSettings', 'RunFirstTime', '0')='1')then
      RunFirstTime:=True;

    DelaySplashScreen:=StrToInt(theIni.ReadString('GeneralSettings', 'DelaySplashScreen',
      '1000'));

{$IFDEF LINUX}
    SQLTextFont:=theIni.ReadString('GeneralSettings', 'SQLTextFont', 'Helvetica');
{$ELSE}
    SQLTextFont:=theIni.ReadString('GeneralSettings', 'SQLTextFont', 'Verdana');
{$ENDIF}
    SQLTextFontSize:=StrToInt(theIni.ReadString('GeneralSettings', 'SQLTextFontSize', '8'));

    ReopenLastFile:=(StrToInt(theIni.ReadString('GeneralSettings', 'ReopenLastFile',
      '1'))=1);

    DMMain.NormalizeEditorForms:=(StrToInt(theIni.ReadString('GeneralSettings', 'EditorsFloatOnTop',
      '1'))=1);


    MinimizeOnCtrlShiftC:=(StrToInt(theIni.ReadString('GeneralSettings', 'MinimizeOnCtrlShiftC',
      '1'))=1);


    ApplStyle:=StrToInt(theIni.ReadString('GeneralSettings', 'ApplicationStyle', '4'));
    MainForm.SetApplStyle(ApplStyle);

    DMMain.HTMLBrowserAppl:=theIni.ReadString('GeneralSettings', 'HTMLBrowserAppl', '');


    DockedQueryPnlMode:=StrToInt(theIni.ReadString('DockedQueryPanel', 'Mode', '2'));
    DockedQueryPnlHeightM1:=StrToInt(theIni.ReadString('DockedQueryPanel', 'HeightM1', '226'));
    DockedQueryPnlHeightM2:=StrToInt(theIni.ReadString('DockedQueryPanel', 'HeightM2', '379'));
    DockedQueryPnlStoredSQLTreeVisibleM1:=(StrToInt(theIni.ReadString('DockedQueryPanel', 'StoredSQLTreeVisibleM1', '0'))=1);
    DockedQueryPnlStoredSQLTreeVisibleM2:=(StrToInt(theIni.ReadString('DockedQueryPanel', 'StoredSQLTreeVisibleM2', '1'))=1);
    DockedQueryPnlStoredSQLTreeWidthM1:=StrToInt(theIni.ReadString('DockedQueryPanel', 'StoredSQLTreeWidthM1', '159'));
    DockedQueryPnlStoredSQLTreeWidthM2:=StrToInt(theIni.ReadString('DockedQueryPanel', 'StoredSQLTreeWidthM2', '200'));
    DockedQueryPnlBLOBPnlVisibleM1:=(StrToInt(theIni.ReadString('DockedQueryPanel', 'BLOBPnlVisibleM1', '0'))=1);
    DockedQueryPnlBLOBPnlVisibleM2:=(StrToInt(theIni.ReadString('DockedQueryPanel', 'BLOBPnlVisibleM2', '0'))=1);
    DockedQueryPnlBLOBPnlWidthM1:=StrToInt(theIni.ReadString('DockedQueryPanel', 'BLOBPnlWidthM1', '180'));
    DockedQueryPnlBLOBPnlWidthM2:=StrToInt(theIni.ReadString('DockedQueryPanel', 'BLOBPnlWidthM2', '180'));
    DockedQueryPnlSQLPnlSizeM1:=StrToInt(theIni.ReadString('DockedQueryPanel', 'SQLPnlSizeM1', '426'));
    DockedQueryPnlSQLPnlSizeM2:=StrToInt(theIni.ReadString('DockedQueryPanel', 'SQLPnlSizeM2', '144'));

    UseSQLSyntaxHighlighting:=(StrToInt(theIni.ReadString('GeneralSettings', 'UseSQLSyntaxHighlighting', '0'))=1);

    IgnoreSQLHistoryChange:=(StrToInt(theIni.ReadString('GeneralSettings', 'IgnoreSQLHistoryChange', '1'))=1);

    //Do not Load recent data when run first
    if(Not(RunFirstTime))then
    begin
      ShowPalettesDocked:=(StrToInt(theIni.ReadString('GeneralSettings', 'ShowPalettesDocked',
        '1'))=1);

      ShowTipsOnStartup:=(StrToInt(theIni.ReadString('GeneralSettings', 'ShowTipsOnStartup', '1'))=1);
      try
        LastTipShown:=StrToInt(theIni.ReadString('GeneralSettings', 'LastTipShown', '0'));
      except
        LastTipShown:=-1;
      end;

      RecentOpenFileDir:=theIni.ReadString('RecentDirectories', 'RecentOpenFileDir', '');
      RecentSaveModelAsImageDir:=theIni.ReadString('RecentDirectories', 'RecentSaveModelAsImageDir', '');
      RecentOpenSQLScriptFileDir:=theIni.ReadString('RecentDirectories', 'RecentOpenSQLScriptFileDir', '');
      RecentImportFileDir:=theIni.ReadString('RecentDirectories', 'RecentImportFileDir', '');

      theStringList:=TStringList.Create;
      try
        //Read RecentFiles and add them to the Menu
        theIni.ReadSectionValues('RecentFiles', theStringList);
        for i:=theStringList.Count-1 downto 0 do
          AddFileToRecentFilesList(Copy(theStringList[i], Pos('=', theStringList[i])+1, Length(theStringList[i])));

      finally
        theStringList.Free;
      end;
    end
    else
    begin
      //Start with palettes docked when run first time
      ShowPalettesDocked:=True;

      ShowTipsOnStartup:=True;
      LastTipShown:=-1;
    end;

    if(RecentOpenFileDir='')then
      RecentOpenFileDir:=ExtractFilePath(Application.ExeName)+
        'Examples'+PathDelim;

    if(RecentSaveModelAsImageDir='')then
      RecentSaveModelAsImageDir:=RecentOpenFileDir;
  finally
    theIni.Free;
  end;
end;

procedure TDMGUI.SaveSettingsToIniFile;
var theIni: TMemIniFile;
  i: integer;
  s: string;
begin
  //Open IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_Settings.ini');
  try
    theIni.WriteString('GeneralSettings', 'DelaySplashScreen',
      IntToStr(DelaySplashScreen));

    case Application.Style.DefaultStyle of
      dsWindows: theIni.WriteString('GeneralSettings', 'ApplicationStyle', '1');
      dsMotifPlus: theIni.WriteString('GeneralSettings', 'ApplicationStyle', '2');
      dsQtSGI: theIni.WriteString('GeneralSettings', 'ApplicationStyle', '3');
      dsPlatinum: theIni.WriteString('GeneralSettings', 'ApplicationStyle', '4');
    end;

    theIni.WriteString('GeneralSettings', 'ShowPalettesDocked',
      IntToStr(Ord(ShowPalettesDocked)));

    theIni.WriteString('GeneralSettings', 'EditorsFloatOnTop', IntToStr(Ord(DMMain.NormalizeEditorForms)));

    theIni.WriteString('GeneralSettings', 'ReopenLastFile',
      IntToStr(Ord(ReopenLastFile)));

    theIni.WriteString('GeneralSettings', 'ShowTipsOnStartup',
      IntToStr(Ord(ShowTipsOnStartup)));

    theIni.WriteString('GeneralSettings', 'LastTipShown',
      IntToStr(LastTipShown));


    //Clear RecentFiles
    theIni.EraseSection('RecentFiles');

    for i:=0 to RecentFiles.Count-1 do
      theIni.WriteString('RecentFiles', 'File'+IntToStr(i+1), RecentFiles[i]);

    theIni.WriteString('RecentDirectories', 'RecentOpenFileDir', RecentOpenFileDir);
    theIni.WriteString('RecentDirectories', 'RecentSaveModelAsImageDir', RecentSaveModelAsImageDir);
    theIni.WriteString('RecentDirectories', 'RecentOpenSQLScriptFileDir', RecentOpenSQLScriptFileDir);
    theIni.WriteString('RecentDirectories', 'RecentImportFileDir', RecentImportFileDir);

    theIni.WriteString('GeneralSettings', 'RunFirstTime',  '0');


    theIni.WriteString('GeneralSettings', 'SQLTextFont', SQLTextFont);
    theIni.WriteString('GeneralSettings', 'SQLTextFontSize', IntToStr(SQLTextFontSize));

    theIni.WriteString('GeneralSettings', 'MinimizeOnCtrlShiftC', IntToStr(Ord(MinimizeOnCtrlShiftC)));

    theIni.WriteString('GeneralSettings', 'HTMLBrowserAppl', DMMain.HTMLBrowserAppl);

    theIni.WriteString('DockedQueryPanel', 'Mode', IntToStr(DockedQueryPnlMode));
    theIni.WriteString('DockedQueryPanel', 'HeightM1', IntToStr(DockedQueryPnlHeightM1));
    theIni.WriteString('DockedQueryPanel', 'HeightM2', IntToStr(DockedQueryPnlHeightM2));
    theIni.WriteString('DockedQueryPanel', 'StoredSQLTreeVisibleM1', IntToStr(Ord(DockedQueryPnlStoredSQLTreeVisibleM1)));
    theIni.WriteString('DockedQueryPanel', 'StoredSQLTreeVisibleM2', IntToStr(Ord(DockedQueryPnlStoredSQLTreeVisibleM2)));
    theIni.WriteString('DockedQueryPanel', 'StoredSQLTreeWidthM1', IntToStr(DockedQueryPnlStoredSQLTreeWidthM1));
    theIni.WriteString('DockedQueryPanel', 'StoredSQLTreeWidthM2', IntToStr(DockedQueryPnlStoredSQLTreeWidthM2));
    theIni.WriteString('DockedQueryPanel', 'BLOBPnlVisibleM1', IntToStr(Ord(DockedQueryPnlBLOBPnlVisibleM1)));
    theIni.WriteString('DockedQueryPanel', 'BLOBPnlVisibleM2', IntToStr(Ord(DockedQueryPnlBLOBPnlVisibleM2)));
    theIni.WriteString('DockedQueryPanel', 'BLOBPnlWidthM1', IntToStr(DockedQueryPnlBLOBPnlWidthM1));
    theIni.WriteString('DockedQueryPanel', 'BLOBPnlWidthM2', IntToStr(DockedQueryPnlBLOBPnlWidthM2));
    theIni.WriteString('DockedQueryPanel', 'SQLPnlSizeM1', IntToStr(DockedQueryPnlSQLPnlSizeM1));
    theIni.WriteString('DockedQueryPanel', 'SQLPnlSizeM2', IntToStr(DockedQueryPnlSQLPnlSizeM2));

    theIni.WriteString('GeneralSettings', 'UseSQLSyntaxHighlighting', IntToStr(Ord(UseSQLSyntaxHighlighting)));

    theIni.WriteString('GeneralSettings', 'IgnoreSQLHistoryChange', IntToStr(Ord(IgnoreSQLHistoryChange)));


    theIni.WriteString('GeneralSettings', 'ApplicationFontName', DMMain.ApplicationFontName);
    theIni.WriteString('GeneralSettings', 'ApplicationFontSize', IntToStr(DMMain.ApplicationFontSize));

    s:='';
    if(fsBold in DMMain.ApplicationFontStyle)then
      s:=s+'Bold, '
    else if(fsItalic in DMMain.ApplicationFontStyle)then
      s:=s+'Italic, '
    else if(fsUnderline in DMMain.ApplicationFontStyle)then
      s:=s+'Underline, '
    else if(fsStrikeOut in DMMain.ApplicationFontStyle)then
      s:=s+'StrikeOut, ';
    s:=Copy(s, 1, Length(s)-2);
    theIni.WriteString('GeneralSettings', 'ApplicationFontStyle', s);


    theIni.UpdateFile;
  finally
    theIni.Free;
  end;
end;

procedure TDMGUI.AddFileToRecentFilesList(fname: string);
var theMenuItem: TMenuItem;
  oldPos: integer;
begin
  //Don't insert a file twice
  if(RecentFiles.IndexOf(fname)=-1)and(FileExists(fname))then
  begin
    RecentFiles.Insert(0, fname);

    theMenuItem:=TMenuItem.Create(MainForm);
    inc(RecentFilesMICounter);
    theMenuItem.Name:='OpenRecent'+IntToStr(RecentFilesMICounter+1)+'MI';
    theMenuItem.Caption:=ExtractFileName(fname);
    theMenuItem.OnClick:=OpenRecentFile;

    MainForm.OpenRecentMI.Insert(0, theMenuItem);

    if(RecentFiles.Count>10)then
    begin
      RecentFiles.Delete(10);
      theMenuItem:=MainForm.OpenRecentMI[10];
      MainForm.OpenRecentMI.Delete(10);
      theMenuItem.Free;
    end;
  end
  else if(RecentFiles.IndexOf(fname)>0)and(FileExists(fname))then
  begin
    oldPos:=RecentFiles.IndexOf(fname);
    RecentFiles.Delete(oldPos);
    RecentFiles.Insert(0, fname);

    theMenuItem:=MainForm.OpenRecentMI[oldPos];
    MainForm.OpenRecentMI.Delete(oldPos);
    MainForm.OpenRecentMI.Insert(0, theMenuItem);
  end;
end;

function TDMGUI.GetRecentFileCount: integer;
begin
  GetRecentFileCount:=RecentFiles.Count;
end;

function TDMGUI.GetRecentFileName(id: integer): string;
begin
  GetRecentFileName:=RecentFiles[id];
end;

procedure TDMGUI.OpenRecentFile(Sender: TObject);
begin
  MainForm.OpenFile(GetRecentFileName(MainForm.OpenRecentMI.IndexOf(TMenuItem(Sender))), False);
end;

procedure TDMGUI.SetStatusCaption(s: string);
begin
  MainForm.StatusCaptionLbl.Caption:=s;
  MainForm.StatusCaptionLbl.Refresh;
end;

procedure TDMGUI.SetQueryStatusLbl;
begin
  if(DMDB.CurrentDBConn=nil)then
  begin
    MainForm.ConnectionSBtn.Enabled:=False;
    MainForm.QueryStatusLbl.Caption:=
      DMMain.GetTranslatedMessage('Not connected to a Database', 27);
  end
  else
  begin
    MainForm.ConnectionSBtn.Enabled:=True;
    MainForm.QueryStatusLbl.Caption:=
      DMMain.GetTranslatedMessage('Connected to Database %s', 28,
        DMDB.CurrentDBConn.Params.Values['User_Name']+'@'+
        DMDB.CurrentDBConn.Params.Values['Database']);
  end;

  MainForm.DBConnPnl.Width:=MainForm.QueryStatusLbl.Width+37;
end;

function TDMGUI.LoadTip: string;
var theStringList: TStringList;
begin
  LoadTip:='';

  theStringList:=TStringList.Create;
  try
    DMMain.GetSectionFromTxtFile(DMMain.SettingsPath+DMMain.ProgName+'_Translations.ini',
      'Tips', theStringList, True);

    if(theStringList.Text='')then
    begin
      LoadTip:='';
      ShowTipsOnStartup:=False;
    end
    else
    begin
      inc(LastTipShown);
      if(LastTipShown>theStringList.Count-1)then
        LastTipShown:=0;

      LoadTip:=DMMain.ReplaceText(theStringList[DMGUI.LastTipShown], '#13#10', #13#10);
    end
  finally
    theStringList.Free;
  end;
end;

procedure TDMGUI.CopyInitialSettingsToPrivatSettingsDir;
var SourcePath: string;
begin
  SourcePath:=ExtractFilepath(Application.ExeName)+'Data'+PathDelim;

  //Copy Settings
  DMMain.CopyDiskFile(SourcePath+'DBDesignerFork_Settings.ini',
    DMMain.SettingsPath+'DBDesignerFork_Settings.ini', False);
  //Copy Database Info
  DMMain.CopyDiskFile(SourcePath+'DBDesignerFork_DatabaseInfo.ini',
    DMMain.SettingsPath+'DBDesignerFork_DatabaseInfo.ini', False);
  //Copy Translations Info
  DMMain.CopyDiskFile(SourcePath+'DBDesignerFork_Translations.ini',
    DMMain.SettingsPath+'DBDesignerFork_Translations.ini', False);

  DMMain.CopyDiskFile(SourcePath+'DBDesignerFork_Translations.txt',
    DMMain.SettingsPath+'DBDesignerFork_Translations.txt', False);



  //Copy DBConn DefaultSettings
  DMMain.CopyDiskFile(SourcePath+'DBConn_DefaultSettings.ini',
    DMMain.SettingsPath+'DBConn_DefaultSettings.ini', False);

  //Copy Language settings
  if(FileExists(SourcePath+'Language.ini'))then
    DMMain.CopyDiskFile(SourcePath+'Language.ini',
      DMMain.SettingsPath+'Language.ini', False);
end;

procedure TDMGUI.CheckIniFiles;
begin
  //Check if the default settings exist
  //if not, copy them
  if(Not(FileExists(DMMain.SettingsPath+'DBDesignerFork_Settings.ini')))then
    CopyInitialSettingsToPrivatSettingsDir
  else
  begin
    //if they exist, check their version and replace personal file with up-to-date version
    DMMain.CheckIniFileVersion('DBDesignerFork_Settings.ini', INIVersion_DBDesigner4_Settings);
    DMMain.CheckIniFileVersion('DBDesignerFork_DatabaseInfo.ini', INIVersion_DBDesigner4_DatabaseInfo);
    DMMain.CheckIniFileVersion('DBConn_DefaultSettings.ini', INIVersion_DBConn_DefaultSettings);

    //if Translations.ini needs to be updated, copy Translations.txt as well
    if(Not(DMMain.CheckIniFileVersion(DMMain.ProgName+'_Translations.ini', INIVersion_DBDesigner4_Translations)))then
      DMMain.CopyDiskFile(ExtractFilepath(Application.ExeName)+'Data'+PathDelim+'DBDesignerFork_Translations.txt',
        DMMain.SettingsPath+'DBDesignerFork_Translations.txt', False);
  end;
end;

end.
