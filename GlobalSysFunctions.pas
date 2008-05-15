unit GlobalSysFunctions;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of fabFORCE DBDesigner4.
// Copyright (C) 2002 Michael G. Zinner, www.fabFORCE.net
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
// Unit GlobalSysFunctions.pas
// ---------------------------
// Version 1.0, 20.08.2003, Mike
// Description
//   Sets the Global Font of the Application
//
// Changes:
//   Version 1.0, 20.08.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------


interface

uses QForms,
  IniFiles,
  SysUtils,
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  ActiveX, ShellAPI, ShlObj, // for SHGetSpecialFolderLocation() und SHGetPathFromIDList()
  {$ENDIF}
  QGraphics;

procedure LoadApplicationFont;
{$IFDEF MSWINDOWS}
function GetSpecialFolder(Folder: Integer): String;
{$ENDIF}
function GetGlobalSettingsPath: string;

implementation

{$IFDEF MSWINDOWS}
//CSIDL_COOKIES              Cookies
//CSIDL_DESKTOPDIRECTORY     Desktop
//CSIDL_FAVORITES            Favoriten
//CSIDL_HISTORY              Internet-Verlauf
//CSIDL_INTERNET_CACHE       "Temporary Internet Files"
//CSIDL_PERSONAL             Eigene Dateien               $0005
//CSIDL_PROGRAMS             "Programme" im Startmenü
//CSIDL_RECENT               "Dokumente" im Startmenü
//CSIDL_SENDTO               "Senden an" im Kontextmenü
//CSIDL_STARTMENU            Startmenü
//CSIDL_STARTUP              Autostart

//e.g. : s:=GetSpecialFolder(CSIDL_RECENT);

function GetSpecialFolder(Folder: Integer): String;
var
  pMalloc: IMalloc;
  pidl: PItemIDList;
  Path: PChar;
begin
  // get IMalloc interface pointer
  if (SHGetMalloc(pMalloc) <> S_OK) then begin
    raise EInOutError.Create('Couldn''t get pointer to IMalloc interface.');

    Exit;
  end;

  // retrieve path
  SHGetSpecialFolderLocation(0, Folder, pidl);
  GetMem(Path, MAX_PATH);
  SHGetPathFromIDList(pidl, Path);
  Result := Path;
  FreeMem(Path);

  // free memory allocated by SHGetSpecialFolderLocation
  pMalloc.Free(pidl);
end;
{$ENDIF}

function GetGlobalSettingsPath: string;
var SettingsPath: string;
{$IFDEF MSWINDOWS}
  i: integer;
  disablePersonalSettings: Boolean;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  SettingsPath:=GetSpecialFolder(CSIDL_APPDATA)+PathDelim+'DBDesigner4'+PathDelim;

  //Create DBDesigner4 settings dir if it is not disabled by start parameter
  disablePersonalSettings:=False;
  for i:=0 to ParamCount do
    if(CompareText(ParamStr(i), '-disablePersonalSettings')=0)then
    begin
      disablePersonalSettings:=True;
      break;
    end;
  if(Not(disablePersonalSettings))then
    ForceDirectories(SettingsPath);

  //Check if a DBDesigner4 Directory has been created in the Users
  //Home Directory. if not, use the Data dir in the Appl-Dir
  if(Not(DirectoryExists(SettingsPath)))then
    SettingsPath:=ExtractFilePath(Application.ExeName)+'Data'+PathDelim;
{$ELSE}
    //the settings are stored in a dir '.DBDesigner4' in the users home dir
  SettingsPath:=GetEnvironmentVariable('HOME')+PathDelim+'.DBDesigner4'+PathDelim;
{$ENDIF}

  //Create the directory if it doesn't already exist
  ForceDirectories(SettingsPath);

  GetGlobalSettingsPath:=SettingsPath;
end;

procedure LoadApplicationFont;
var theIni: TMemIniFile;
  s: string;
begin
  //Read IniFile
  theIni:=TMemIniFile.Create(GetGlobalSettingsPath+
    Copy(ExtractFileName(Application.ExeName), 1,
    Length(ExtractFileName(Application.ExeName))-
    Length(ExtractFileExt(Application.ExeName)))+'_Settings.ini');

  try
    try
{$IFDEF LINUX}
      Application.Font.Name:=theIni.ReadString('GeneralSettings', 'ApplicationFontName', 'Nimbus Sans L');
      Application.Font.Size:=StrToInt(theIni.ReadString('GeneralSettings', 'ApplicationFontSize', '11'));
{$ELSE}
      Application.Font.Name:=theIni.ReadString('GeneralSettings', 'ApplicationFontName', 'MS Sans Serif');
      Application.Font.Size:=StrToInt(theIni.ReadString('GeneralSettings', 'ApplicationFontSize', '8'));
{$ENDIF}
      s:=theIni.ReadString('GeneralSettings', 'ApplicationFontStyle', '');
      Application.Font.Style:=[];
      if(Pos('bold', lowercase(s))>0)then
        Application.Font.Style:=Application.Font.Style+[fsBold]
      else if(Pos('italic', lowercase(s))>0)then
        Application.Font.Style:=Application.Font.Style+[fsItalic]
      else if(Pos('underline', lowercase(s))>0)then
        Application.Font.Style:=Application.Font.Style+[fsUnderline]
      else if(Pos('strikeout', lowercase(s))>0)then
        Application.Font.Style:=Application.Font.Style+[fsStrikeOut];
    except
{$IFDEF LINUX}
      Application.Font.Name:='Nimbus Sans L';
      Application.Font.Size:=11;
      Application.Font.Style:=[];
{$ELSE}
      Application.Font.Name:='MS Sans Serif';
      Application.Font.Size:=8;
      Application.Font.Style:=[];
{$ENDIF}
    end;
  finally
    theIni.Free;
  end;
end;


end.
