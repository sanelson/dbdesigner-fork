program DBDplugin_HTMLReport;

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

// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------
//
// Project DBDplugin_HTMLReport.dpr
// --------------------------------
// Version 1.0, 12.01.2003, Mike
// Description
//   Projectfile for the HTML Report Plugin
//
// Changes:
//   Version 1.1, 01.08.2003, Mike
//     added support for LibXmlParser
//   Version 1.0, 12.01.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
{$I ..\..\DBDesigner4.inc}
{$ELSE}
{$I ../../DBDesigner4.inc}
{$ENDIF}

uses
  QForms,
  Main in 'Main.pas' {MainForm},
{$IFDEF MSWINDOWS}
  EERModel in '..\..\EERModel.pas',
  EERDM in '..\..\EERDM.pas' {DMEER: TDataModule},
  MainDM in '..\..\MainDM.pas' {DMMain: TDataModule},
  EditorString in '..\..\EditorString.pas' {EditorStringForm},
  GlobalSysFunctions in '..\..\GlobalSysFunctions.pas',
{$IFDEF USE_IXMLDBMODELType}
  EERModel_XML in '..\..\EERModel_XML.pas',
{$ENDIF}
  LibXmlParser in '..\..\LibXmlParser.pas';
{$ELSE}
  EERModel in '../../EERModel.pas',
  EERDM in '../../EERDM.pas' {DMEER: TDataModule},
  MainDM in '../../MainDM.pas' {DMMain: TDataModule},
  EditorString in '../../EditorString.pas' {EditorStringForm},
  GlobalSysFunctions in '../../GlobalSysFunctions.pas',
{$IFDEF USE_IXMLDBMODELType}
  EERModel_XML in '../../EERModel_XML.pas',
{$IFDEF LINUX}
  xmldom, oxmldom,
{$ENDIF}
{$ENDIF}
  LibXmlParser in '../../LibXmlParser.pas';
{$ENDIF}

{$R *.res}

begin
{$IFDEF LINUX}
{$IFDEF USE_IXMLDBMODELType}
  DefaultDOMVendor:='Open XML';
{$ENDIF}
{$ENDIF}

  Application.Initialize;
  Application.Title := 'HTML Reporter';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
