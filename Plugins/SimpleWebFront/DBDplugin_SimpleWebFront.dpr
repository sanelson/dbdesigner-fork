//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the SimpleWebFront-DBDesigner4-Plugin.
// Copyright (C) 2003 Bayer Ulrich
//
// The SimpleWebFront-DBDesigner4-Plugin is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// SimpleWebFront-DBDesigner4-Plugin is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------

//Filename : DBDplugin_SimpleWebFront.dpr

program DBDplugin_SimpleWebFront;

{$IFDEF MSWINDOWS}
uses
  QForms,
  Main in 'Main.pas' {MainForm},
  EERModel in '..\..\EERModel.pas',
  EERDM in '..\..\EERDM.pas' {DMEER: TDataModule},
  MainDM in '..\..\MainDM.pas' {DMMain: TDataModule},
  EditorString in '..\..\EditorString.pas' {EditorStringForm},
  EERModel_XML in '..\..\EERModel_XML.pas',
  Weboutput in 'Weboutput.pas',
  SplitFns in 'SplitFns.pas',
  Layer in 'Layer.pas',
  DialogImageSelection in 'DialogImageSelection.pas' {ImageSelectionForm},
  StringConstants in 'StringConstants.pas',
  DialogDirectorySelect in 'DialogDirectorySelect.pas' {DialogDirectorySelectForm},
  EditorView in 'EditorView.pas' {EditorViewForm},
  EditorGroup in 'EditorGroup.pas' {EditorGroupForm},
  Splash in 'Splash.pas' {SplashForm},
  DialogPopupSettings in 'DialogPopupSettings.pas' {DialogPopupSettingsForm},
  SWF_XML_Binding in 'SWF_XML_Binding.pas',
  DialogBugs in 'DialogBugs.pas' {DialogBugsForm},
  LibXmlParser in '..\..\LibXmlParser.pas',
  GlobalSysFunctions in '..\..\GlobalSysFunctions.pas';
{$ELSE}
uses
  QForms,
  Main in 'Main.pas' {MainForm},
  EERModel in '../../EERModel.pas',
  EERDM in '../../EERDM.pas' {DMEER: TDataModule},
  MainDM in '../../MainDM.pas' {DMMain: TDataModule},
  EditorString in '../../EditorString.pas' {EditorStringForm},
  EERModel_XML in '../../EERModel_XML.pas',
  Weboutput in 'Weboutput.pas',
  SplitFns in 'SplitFns.pas',
  Layer in 'Layer.pas',
  DialogImageSelection in 'DialogImageSelection.pas' {ImageSelectionForm},
  StringConstants in 'StringConstants.pas',
  DialogDirectorySelect in 'DialogDirectorySelect.pas' {DialogDirectorySelectForm},
  EditorView in 'EditorView.pas' {EditorViewForm},
  EditorGroup in 'EditorGroup.pas' {EditorGroupForm},
  Splash in 'Splash.pas' {SplashForm},
  DialogPopupSettings in 'DialogPopupSettings.pas' {DialogPopupSettingsForm},
  SWF_XML_Binding in 'SWF_XML_Binding.pas',
  DialogBugs in 'DialogBugs.pas' {DialogBugsForm},
  LibXmlParser in '../../LibXmlParser.pas',  GlobalSysFunctions in '../../GlobalSysFunctions.pas',
  xmldom,oxmldom;
{$ENDIF}

{$R *.res}

begin
{$IFDEF LINUX}
  DefaultDOMVendor := 'Open XML';
{$ENDIF}

  Application.Initialize;
  Application.Title := 'Simple Web Front';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
