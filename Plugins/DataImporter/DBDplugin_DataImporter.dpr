program DBDplugin_DataImporter;

uses
  QForms,
  Main in 'Main.pas' {MainForm},
  DBImportData in 'DBImportData.pas' {DBImportDataForm},
  Progress in 'Progress.pas' {ProgressForm},
{$IFDEF MSWINDOWS}
  DBDM in '..\..\DBDM.pas' {DMDB: TDataModule},
  MainDM in '..\..\MainDM.pas' {DMMain: TDataModule},
  DBConnSelect in '..\..\DBConnSelect.pas' {DBConnSelectForm},
  DBConnLogin in '..\..\DBConnLogin.pas' {DBConnLoginForm},
  DBConnEditor in '..\..\DBConnEditor.pas' {DBConnEditorForm},
  EditorString in '..\..\EditorString.pas' {EditorStringForm},
  GlobalSysFunctions in '..\..\GlobalSysFunctions.pas';
{$ELSE}
  DBDM in '../../DBDM.pas' {DMDB: TDataModule},
  MainDM in '../../MainDM.pas' {DMMain: TDataModule},
  DBConnSelect in '../../DBConnSelect.pas' {DBConnSelectForm},
  DBConnLogin in '../../DBConnLogin.pas' {DBConnLoginForm},
  DBConnEditor in '../../DBConnEditor.pas' {DBConnEditorForm},
  EditorString in '../../EditorString.pas' {EditorStringForm},
  GlobalSysFunctions in '../../GlobalSysFunctions.pas';
{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DataImporter';

  Application.ShowMainForm := False;
  Application.CreateForm(TMainForm, MainForm);
  
  Application.Run;
end.
