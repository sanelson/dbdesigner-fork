unit EERExportSQLScript;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of DBDesigner Fork which is forked from DBDesigner 4.
//
// DBDesigner Fork is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// DBDesigner Fork is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with DBDesigner Fork; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------
//
// Unit EERExportSQLCreateScript.pas
// ---------------------------------
// Version Fork 1.0, 18.09.2006, JP
// Version 1.1, 08.04.2003, Mike
// Description
//   Contains the SQL Script Export form class
//
// Changes:
//   Version Fork 1.0, 18.09.2006, JP
//     added export SQL options for Oracle, SQL Server and FireBird.
//   Version 1.1, 08.04.2003, Mike
//     added LoadSettings / SaveSettings proc.
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons, EERModel, QClipbrd, QExtCtrls, QComCtrls,
  IniFiles, QCheckLst;

type
  TEERExportSQLScriptFrom = class(TForm)
    Settings: TGroupBox;
    PhysicalCBox: TCheckBox;
    StatusBar: TStatusBar;
    ExportSelTablesCBox: TCheckBox;
    SQLCreatesSettingGBox: TGroupBox;
    PKCBox: TCheckBox;
    FKCBox: TCheckBox;
    StdInsertsCBox: TCheckBox;
    IndicesCBox: TCheckBox;
    Panel1: TPanel;
    CopyBtn: TSpeedButton;
    ExportBtn: TSpeedButton;
    TblOptionsCBox: TCheckBox;
    CommentsCBox: TCheckBox;
    RegionsListBox: TCheckListBox;
    CloseBtn: TSpeedButton;
    AdvSQLCreatesSettingGBox: TGroupBox;
    NullCBox: TCheckBox;
    IndiceFK: TCheckBox;
    PortableIndicesCBox: TCheckBox;
    HideOnDeleteUpdateNoActionCBox: TCheckBox;
    LabelTargetDB: TLabel;
    CBTargetDataBase: TComboBox;
    DropTablesCBox: TCheckBox;
    CBAutoIncrement: TCheckBox;
    CBLastChange: TCheckBox;
    EdLastChangeDateColName: TEdit;
    LabColNameLastChange: TLabel;
    LabLastChangeUserColName: TLabel;
    EdLastChangeUserColName: TEdit;
    CBLastDelete: TCheckBox;
    CBDefaultBeforeNotNull: TCheckBox;
    EdAutoIncrementPrefix: TEdit;
    LbAutoIncrementPrefix: TLabel;
    LbAutoIncrementSeqName: TLabel;
    EdAutoIncrementSeqName: TEdit;
    LbLastChangeTriggerPrefix: TLabel;
    EdLastChangeTriggerPrefix: TEdit;
    LbLastDeleteTriggerPrefix: TLabel;
    EdLastDeleteTriggerPrefix: TEdit;
    EdLastDeleteColName: TEdit;
    LbLastDeleteColName: TLabel;
    LbLastDeleteTbName: TLabel;
    EdLastDeleteTbName: TEdit;
    GOCB: TCheckBox;
    CommitCB: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SetModel(theEERModel: TEERModel; mode: integer = 0);
    function GetSQLScript: string;
    procedure ExportBtnClick(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure FKCBoxClick(Sender: TObject);

    procedure LoadSettingsFromIniFile;
    procedure SaveSettingsToIniFile;
    procedure RegionsListBoxClickCheck(Sender: TObject);
    procedure RegionsListBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloseBtnClick(Sender: TObject);
    procedure CBTargetDataBaseChange(Sender: TObject);
    procedure CBLastChangeClick(Sender: TObject);
    procedure CBAutoIncrementClick(Sender: TObject);
    procedure CBLastDeleteClick(Sender: TObject);
  private
    { Private declarations }
    function GetSqlGeneratorOrSequence(DataBaseType:string):string;

    function GetDtExclusionSqlTableDef(DbType, TbName, ColName: string): string;
  public
    { Public declarations }
    EERModel: TEERModel;
    ScriptMode: integer;
    theRegions: TList;
  end;

var
  EERExportSQLScriptFrom: TEERExportSQLScriptFrom;

implementation

uses MainDM, EERDM, GUIDM, StrUtils;

{$R *.xfm}

procedure TEERExportSQLScriptFrom.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);

  theRegions:=TList.Create;

  LoadSettingsFromIniFile;

  CBTargetDataBaseChange(Self);
end;

procedure TEERExportSQLScriptFrom.FormDestroy(Sender: TObject);
begin
  SaveSettingsToIniFile;

  theRegions.Free;
end;

procedure TEERExportSQLScriptFrom.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TEERExportSQLScriptFrom.SetModel(theEERModel: TEERModel; mode: integer);
var i: integer;
begin
  EERModel:=theEERModel;
  ScriptMode:=mode;

  // mode=0 ... creates
  // mode=1 ... drops
  // mode=2 ... optimize
  if(mode<>0)then
  begin
    SQLCreatesSettingGBox.Visible:=False;
    AdvSQLCreatesSettingGBox.Visible := (ScriptMode=0);
//    Height:=Height-SQLCreatesSettingGBox.Height-10;
  end;

  RegionsListBox.Items.Clear;
  RegionsListBox.Items.Add('All Tables');
  EERModel.GetEERObjectList([EERRegion], theRegions, False);
  for i:=0 to theRegions.Count-1 do
    RegionsListBox.Items.Add(TEERRegion(theRegions[i]).ObjName);

  RegionsListBox.Checked[0]:=True;
end;


function TEERExportSQLScriptFrom.GetSQLScript: string;
var
  s: string;
  i: integer;
  Tables: TList;
  theEERTbl: TEERTable;
  TargetDatabase: string;
  DropIfExists: boolean;
begin
  TargetDatabase := CBTargetDataBase.Items[CBTargetDataBase.ItemIndex];

  if TargetDatabase = 'My SQL' then
  begin
    DropIfExists := true;
  end else
  begin
    DropIfExists := false;
  end;

  Tables:=TList.Create;
  try
    GetSQLScript:='';
    if(ScriptMode=0)then
      StatusBar.SimpleText:=DMMain.GetTranslatedMessage('Creating SQL Creates.', 199)
    else if(ScriptMode=1)then
      StatusBar.SimpleText:=DMMain.GetTranslatedMessage('Creating SQL Drops.', 200)
    else if(ScriptMode=2)then
      StatusBar.SimpleText:=DMMain.GetTranslatedMessage('Creating SQL Optimize Script.', 242)
    else if(ScriptMode=3)then
      StatusBar.SimpleText:=DMMain.GetTranslatedMessage('Creating SQL Repair Script.', 271);

    //get tables
    if(RegionsListBox.Checked[0])then
      EERModel.GetEERObjectList([EERTable], Tables, ExportSelTablesCBox.Checked)
    else
    begin
      //get tables on regions
      for i:=1 to RegionsListBox.Items.Count-1 do
        if(RegionsListBox.Checked[i])then
          TEERRegion(theRegions[i-1]).GetEERObjsInRegion([EERTable], Tables, ExportSelTablesCBox.Checked);
    end;

    if tables.count=0 then
    begin
      exit;
    end;

    //Remove Linked Tables if CreateSQLforLinkedObjects is deactivated
    if(Not(EERModel.CreateSQLforLinkedObjects))then
    begin
      i:=0;
      while(i<Tables.Count)do
        if(TEERTable(Tables[i]).IsLinkedObject)then
          Tables.Delete(i)
        else
          inc(i);
    end;

    //Sort tables alphabetically
    EERModel.SortEERObjectListByObjName(Tables);

    //Sort in FK order
    if(PhysicalCBox.Checked)then
      EERModel.SortEERTableListByForeignKeyReferences(Tables);

    //When dropping the tables, reverse tablelist order
    if(ScriptMode=1)then
      DMMain.ReverseList(Tables);

    //do for all tables
    s:='';

    if CBAutoIncrement.Checked then
    begin
      s :=
        s +
        GetSqlGeneratorOrSequence(TargetDatabase)+
        #13#10#13#10;
    end;

    //do for all tables
    if DropTablesCBox.Checked and (ScriptMode=0) then
    begin
      for i:=Tables.Count-1 downto 0 do
      begin
        theEERTbl:=Tables[i];
        s:=s+theEERTbl.GetSQLDropCode(DropIfExists)+#13#10#13#10
      end;
    end;


    //Create Last Delete record datetime
    if CBLastDelete.Checked then
    begin
      s := s + GetDtExclusionSqlTableDef(
                                          TargetDatabase,
                                          EdLastDeleteTbName.Text,
                                          EdLastDeleteColName.Text);
      s := s + sLineBreak;
    end;

    //do for all tables
    for i:=0 to Tables.Count-1 do
    begin
      theEERTbl:=Tables[i];

      if(ScriptMode=0)then
        s:=s+theEERTbl.GetSQLCreateCode(PKCBox.Checked,
          IndicesCBox.Checked, FKCBox.Checked,
          TblOptionsCBox.Checked, StdInsertsCBox.Checked,
          CommentsCBox.Checked,
          NullCBox.Checked,
          PortableIndicesCBox.Checked,
          HideOnDeleteUpdateNoActionCBox.Checked,
          GOCB.Checked,
          CommitCB.Checked,
          IndiceFK.Checked,
          CBDefaultBeforeNotNull.Checked,
          TargetDatabase,
          EdAutoIncrementSeqName.Text,
          EdAutoIncrementPrefix.Text,
          CBAutoIncrement.Checked,
          CBLastChange.Checked,
          EdLastChangeDateColName.Text,
          EdLastChangeUserColName.Text,
          EdLastChangeTriggerPrefix.Text,
          CBLastDelete.Checked,
          EdLastDeleteTbName.Text,
          EdLastDeleteColName.Text,
          EdLastDeleteTriggerPrefix.Text
          )
          +#13#10#13#10
      else if(ScriptMode=1)then
        s:=s+theEERTbl.GetSQLDropCode(DropIfExists)+#13#10#13#10
      else if(ScriptMode=2)then
        s:=s+'OPTIMIZE TABLE '+theEERTbl.GetSQLTableName+';'+#13#10#13#10
      else if(ScriptMode=3)then
        s:=s+'REPAIR TABLE '+theEERTbl.GetSQLTableName+';'+#13#10#13#10;

    end;

    if(DMEER.OutputLinuxStyleLineBreaks)then
      s:=DMMain.ReplaceString(s, #13#10, #10);

    GetSQLScript:=s;
  finally
    Tables.Free;
  end;
end;


procedure TEERExportSQLScriptFrom.ExportBtnClick(Sender: TObject);
var theFile: Textfile;
  theSaveDialog: TSaveDialog;
begin
  theSaveDialog:=TSaveDialog.Create(nil);
  try
{$IFDEF MSWINDOWS}
    //On Windows use native Win32 Open Dlg
    theSaveDialog.UseNativeDialog:=True;
    theSaveDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
{$ENDIF}

    if(ScriptMode=0)then
      theSaveDialog.Title:=DMMain.GetTranslatedMessage('Export SQL Creates ...', 201)
    else if(ScriptMode=1)then
      theSaveDialog.Title:=DMMain.GetTranslatedMessage('Export SQL Drops ...', 202)
    else if(ScriptMode=2)then
      theSaveDialog.Title:=DMMain.GetTranslatedMessage('Export SQL Optimize Script ...', 272)
    else if(ScriptMode=3)then
      theSaveDialog.Title:=DMMain.GetTranslatedMessage('Export SQL Repair Script ...', 273);

    theSaveDialog.Width:=600;
    theSaveDialog.Height:=450;
    theSaveDialog.DefaultExt:='sql';

    if(DirectoryExists(DMGUI.RecentExportSQLCreatesDir))then
      theSaveDialog.InitialDir:=DMGUI.RecentExportSQLCreatesDir
    else
      theSaveDialog.InitialDir:='';

    {theSaveDialog.Position:=Point((Screen.Width-theSaveDialog.Width) div 2,
      (Screen.Height-theSaveDialog.Height) div 2);}

    theSaveDialog.Filter:=DMMain.GetTranslatedMessage('SQL files', 203)+' (*.sql)';

    if(theSaveDialog.Execute)then
    begin
      if(FileExists(theSaveDialog.Filename))then
        if(MessageDlg(DMMain.GetTranslatedMessage('The file [%s] '+
          'already exists. '#13#10+
          'Do you want to overwrite this file?', 197,
          ExtractFileName(theSaveDialog.Filename)), mtInformation,
          [mbYes, mbNo], 0)=mrNo)then
          Exit;

      AssignFile(theFile, theSaveDialog.Filename);
      ReWrite(theFile);
      try
        WriteLn(theFile, GetSQLScript);
      finally
        CloseFile(theFile);
      end;

      if(ScriptMode=0)then
        StatusBar.SimpleText:=DMMain.GetTranslatedMessage('SQL Creates saved to file %s.', 204, ExtractFileName(theSaveDialog.Filename))
      else if(ScriptMode=1)then
        StatusBar.SimpleText:=DMMain.GetTranslatedMessage('SQL Drops saved to file %s.', 205, ExtractFileName(theSaveDialog.Filename))
      else if(ScriptMode=2)then
        StatusBar.SimpleText:=DMMain.GetTranslatedMessage('SQL Optimize Script saved to file %s.', 274, ExtractFileName(theSaveDialog.Filename))
      else if(ScriptMode=3)then
        StatusBar.SimpleText:=DMMain.GetTranslatedMessage('SQL Repair Script saved to file %s.', 275, ExtractFileName(theSaveDialog.Filename));

      DMGUI.RecentExportSQLCreatesDir:=ExtractFilePath(theSaveDialog.FileName);
    end;
  finally
    theSaveDialog.Free;
  end;
end;

procedure TEERExportSQLScriptFrom.CopyBtnClick(Sender: TObject);
begin
  Clipboard.AsText:=GetSQLScript;
  if(ScriptMode=0)then
    StatusBar.SimpleText:=DMMain.GetTranslatedMessage('SQL Creates copied to Clipboard.', 206)
  else if(ScriptMode=1)then
    StatusBar.SimpleText:=DMMain.GetTranslatedMessage('SQL Drops copied to Clipboard.', 207)
  else if(ScriptMode=2)then
    StatusBar.SimpleText:=DMMain.GetTranslatedMessage('SQL Optimize Script copied to Clipboard.', 276)
  else if(ScriptMode=3)then
    StatusBar.SimpleText:=DMMain.GetTranslatedMessage('SQL Repair Script copied to Clipboard.', 277);

end;

procedure TEERExportSQLScriptFrom.FKCBoxClick(Sender: TObject);
begin
  PhysicalCBox.Checked:=FKCBox.Checked;
end;

procedure TEERExportSQLScriptFrom.LoadSettingsFromIniFile;
var theIni: TMemIniFile;
begin
  //Read from IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_Settings.ini');
  try
    ExportSelTablesCBox.Checked:=(StrToInt(theIni.ReadString('ExportSQLSettings', 'ExportSelTables',
      '0'))=1);
    PhysicalCBox.Checked:=(StrToInt(theIni.ReadString('ExportSQLSettings', 'OrderTablesByForeignKeys',
      '1'))=1);

    PKCBox.Checked:=(StrToInt(theIni.ReadString('ExportSQLSettings', 'DefinePKs',
      '1'))=1);
    TblOptionsCBox.Checked:=(StrToInt(theIni.ReadString('ExportSQLSettings', 'OutputTableOptions',
      '1'))=1);
    IndicesCBox.Checked:=(StrToInt(theIni.ReadString('ExportSQLSettings', 'CreateIndices',
      '1'))=1);
    StdInsertsCBox.Checked:=(StrToInt(theIni.ReadString('ExportSQLSettings', 'OutputStdInserts',
      '1'))=1);

    FKCBox.Checked:=(StrToInt(theIni.ReadString('ExportSQLSettings', 'DefineFKReference',
      '1'))=1);

    CommentsCBox.Checked:=(StrToInt(theIni.ReadString('ExportSQLSettings', 'OutputComments',
      '1'))=1);

    // new options
    // DataBase
    CBTargetDataBase.Text:=theIni.ReadString('ExportSQLSettings', 'Database', 'Oracle');

    // Last Delete Triggers

    CBLastDelete.Checked:=(StrToInt(theIni.ReadString('ExportSQLSettings', 'LastDeleteTriggers',
    '0'))=1);

    EdLastDeleteTbName.Text:=theIni.ReadString('ExportSQLSettings', 'LastDeletedTableName', 'DELETE_DATE');

    EdLastDeleteTriggerPrefix.Text:=theIni.ReadString('ExportSQLSettings', 'EdLastDeleteTriggerPrefix', 'EXCDT_');

    EdLastDeleteColName.Text:=theIni.ReadString('ExportSQLSettings', 'LastDeletedColName', 'DELETE_DATE');

    // Last Update Triggers

    CBLastChange.Checked:=(StrToInt(theIni.ReadString('ExportSQLSettings', 'LastChangeTriggers',
    '0'))=1);

    EdLastChangeDateColName.Text:=theIni.ReadString('ExportSQLSettings', 'LastChangeDateColName', 'UPDATE_DATE');

    EdLastChangeUserColName.Text:=theIni.ReadString('ExportSQLSettings', 'LastChangeUserColName', 'USER');

    EdLastChangeTriggerPrefix.Text:=theIni.ReadString('ExportSQLSettings', 'LastChangeTriggerPrefix', 'UPDT_');

    // Auto Increment Triggers

    CBAutoIncrement.Checked:=(StrToInt(theIni.ReadString('ExportSQLSettings', 'AutoIncrementTriggers',
    '0'))=1);

    EdAutoIncrementPrefix.Text:=theIni.ReadString('ExportSQLSettings', 'AutoIncrementPrefix', 'AINC_');

    EdAutoIncrementSeqName.Text:=theIni.ReadString('ExportSQLSettings', 'AutoIncrementSeqName', 'GlobalSequence');
    

  finally
    theIni.Free;
  end;
end;

procedure TEERExportSQLScriptFrom.SaveSettingsToIniFile;
var theIni: TMemIniFile;
begin
  //Write to IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_Settings.ini');
  try
    theIni.WriteString('ExportSQLSettings', 'ExportSelTables',
      IntToStr(Ord(ExportSelTablesCBox.Checked)));
    theIni.WriteString('ExportSQLSettings', 'OrderTablesByForeignKeys',
      IntToStr(Ord(PhysicalCBox.Checked)));

    theIni.WriteString('ExportSQLSettings', 'DefinePKs',
      IntToStr(Ord(PKCBox.Checked)));
    theIni.WriteString('ExportSQLSettings', 'OutputTableOptions',
      IntToStr(Ord(TblOptionsCBox.Checked)));
    theIni.WriteString('ExportSQLSettings', 'CreateIndices',
      IntToStr(Ord(IndicesCBox.Checked)));
    theIni.WriteString('ExportSQLSettings', 'OutputStdInserts',
      IntToStr(Ord(StdInsertsCBox.Checked)));

    theIni.WriteString('ExportSQLSettings', 'DefineFKReference',
      IntToStr(Ord(FKCBox.Checked)));

    theIni.WriteString('ExportSQLSettings', 'OutputComments',
      IntToStr(Ord(CommentsCBox.Checked)));

    // new options
    // DataBase
    theIni.WriteString('ExportSQLSettings', 'Database', CBTargetDataBase.Text);

    // Last Delete Triggers

    theIni.WriteString('ExportSQLSettings', 'LastDeleteTriggers',IntToStr(Ord(CBLastDelete.Checked)));

    theIni.WriteString('ExportSQLSettings', 'LastDeletedTableName', EdLastDeleteTbName.Text);

    theIni.WriteString('ExportSQLSettings', 'EdLastDeleteTriggerPrefix', EdLastDeleteTriggerPrefix.Text);

    theIni.WriteString('ExportSQLSettings', 'LastDeletedColName', EdLastDeleteColName.Text);

    // Last Update Triggers

    theIni.WriteString('ExportSQLSettings', 'LastChangeTriggers',IntToStr(Ord(CBLastChange.Checked)) );

    theIni.WriteString('ExportSQLSettings', 'LastChangeDateColName', EdLastChangeDateColName.Text);

    theIni.WriteString('ExportSQLSettings', 'LastChangeUserColName', EdLastChangeUserColName.Text);

    theIni.WriteString('ExportSQLSettings', 'LastChangeTriggerPrefix', EdLastChangeTriggerPrefix.Text);

    // Auto Increment Triggers

    theIni.WriteString('ExportSQLSettings', 'AutoIncrementTriggers',IntToStr(Ord(CBAutoIncrement.Checked)) );

    theIni.WriteString('ExportSQLSettings', 'AutoIncrementPrefix', EdAutoIncrementPrefix.Text);

    theIni.WriteString('ExportSQLSettings', 'AutoIncrementSeqName', EdAutoIncrementSeqName.Text);

    theIni.UpdateFile;
  finally
    theIni.Free;
  end;
end;


procedure TEERExportSQLScriptFrom.RegionsListBoxClickCheck(
  Sender: TObject);
var i: integer;
  RegionChecked: Boolean;
begin
  RegionChecked:=False;

  for i:=1 to RegionsListBox.Items.Count-1 do
    if(RegionsListBox.Checked[i])then
    begin
      RegionChecked:=True;
      break;
    end;

  RegionsListBox.Checked[0]:=Not(RegionChecked);
end;

procedure TEERExportSQLScriptFrom.RegionsListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  if(RegionsListBox.ItemAtPos(Point(X, Y), True)=0)and
    (X<14)then
  begin
    for i:=1 to RegionsListBox.Items.Count-1 do
      RegionsListBox.Checked[i]:=False;

    RegionsListBox.Checked[0]:=True;
  end;
end;

procedure TEERExportSQLScriptFrom.CloseBtnClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TEERExportSQLScriptFrom.CBTargetDataBaseChange(Sender: TObject);
begin
  NullCBox.Enabled := false;
  GOCB.Enabled := false;
  CommitCB.Enabled := false;
  IndiceFK.Enabled := true;
  PortableIndicesCBox.Enabled := false;
  HideOnDeleteUpdateNoActionCBox.Enabled := false;
  CBDefaultBeforeNotNull.Enabled := false;

  if CBTargetDataBase.Text = 'My SQL' then
  begin
    NullCBox.Checked := false;
    IndiceFK.Checked := false;
    PortableIndicesCBox.Checked := false;
    HideOnDeleteUpdateNoActionCBox.Checked := false;
    GOCB.Checked := false;
    CommitCB.Checked := false;
    CBDefaultBeforeNotNull.Checked := false;
    CBAutoIncrement.Checked := false;

    CBAutoIncrement.Enabled := false;
    CBLastDelete.Enabled := false;
    CBLastChange.Enabled := false;

    CBAutoIncrement.Checked := false;
    CBLastDelete.Checked := false;
    CBLastChange.Checked := false;

  end;

  if CBTargetDataBase.Text = 'Oracle' then
  begin
    NullCBox.Checked := true;
    IndiceFK.Checked := true;
    PortableIndicesCBox.Checked := true;
    HideOnDeleteUpdateNoActionCBox.Checked := true;
    GOCB.Checked := false;
    CommitCB.Checked := true;
    CBDefaultBeforeNotNull.Checked := true;
//    CBAutoIncrement.Checked := true;

    CBAutoIncrement.Enabled := true;
    CBLastDelete.Enabled := true;
    CBLastChange.Enabled := true;

    LbAutoIncrementSeqName.Caption := 'Sequence name: ';
{    //ORA AutoInc
    EdAutoIncrementSeqName.Text := ifThen(CBAutoIncrement.Checked,
                                          EdAutoIncrementSeqName.Text,
                                          'GlobalSequence');}
  end;

  if CBTargetDataBase.Text = 'PostgreSQL' then
  begin
    NullCBox.Checked := true;
    IndiceFK.Checked := true;
    PortableIndicesCBox.Checked := true;
    HideOnDeleteUpdateNoActionCBox.Checked := true;
    GOCB.Checked := false;
    CommitCB.Checked := true;
    CBDefaultBeforeNotNull.Checked := true;
    CBAutoIncrement.Checked := false;

    CBAutoIncrement.Enabled := false;
    CBLastDelete.Enabled := false;
    CBLastChange.Enabled := false;

    CBAutoIncrement.Checked := false;
    CBLastDelete.Checked := false;
    CBLastChange.Checked := false;
  end;

  if CBTargetDataBase.Text = 'SQL Server' then
  begin
    NullCBox.Checked := true;
    IndiceFK.Checked := true;
    PortableIndicesCBox.Checked := true;
    HideOnDeleteUpdateNoActionCBox.Checked := true;
    GOCB.Checked := true;
    CommitCB.Checked := false;

    CBDefaultBeforeNotNull.Checked := false;
    CBAutoIncrement.Checked := false;

    CBAutoIncrement.Enabled := false;
    CBLastDelete.Enabled := true;
    CBLastChange.Enabled := true;

    CBAutoIncrement.Checked := false;
  end;

  if CBTargetDataBase.Text = 'FireBird' then
  begin
    NullCBox.Checked := true;
    IndiceFK.Checked := false;
    PortableIndicesCBox.Checked := true;
    HideOnDeleteUpdateNoActionCBox.Checked := false;
    GOCB.Checked := false;
    CommitCB.Checked := false;
    CBDefaultBeforeNotNull.Checked := false;
//    CBAutoIncrement.Checked := true;

    CBAutoIncrement.Enabled := true;
    CBLastDelete.Enabled := true;
    CBLastChange.Enabled := true;

    LbAutoIncrementSeqName.Caption := 'Generator name: ';
    //FBIRD AutoInc
{    EdAutoIncrementSeqName.Text := ifThen(CBAutoIncrement.Checked,
                                          EdAutoIncrementSeqName.Text,
                                          'GlobalGenerator');}
  end;

  if CBTargetDataBase.Text = 'SQLite' then
  begin
    NullCBox.Checked := true;
    IndiceFK.Checked := false;
    PortableIndicesCBox.Checked := true;
    HideOnDeleteUpdateNoActionCBox.Checked := false;
    GOCB.Checked := false;
    CommitCB.Checked := false;
    CBDefaultBeforeNotNull.Checked := false;

    CBAutoIncrement.Enabled := false;
    CBLastDelete.Enabled := false;
    CBLastChange.Enabled := false;
  end;

end;

function TEERExportSQLScriptFrom.GetSqlGeneratorOrSequence(
  DataBaseType: string): string;
begin
  if DataBaseType = 'FireBird' then
  begin
    GetSqlGeneratorOrSequence := 'CREATE GENERATOR '+EdAutoIncrementSeqName.Text+';';
  end else
  if (DataBaseType = 'Oracle') or (DataBaseType = 'PostgreSQL') then
  begin
    GetSqlGeneratorOrSequence := 'CREATE SEQUENCE '+EdAutoIncrementSeqName.Text+';';
  end;
end;

procedure TEERExportSQLScriptFrom.CBLastChangeClick(Sender: TObject);
begin
  EdLastChangeDateColName.Enabled := CBLastChange.Checked;
  EdLastChangeUserColName.Enabled := CBLastChange.Checked;
  EdLastChangeTriggerPrefix.Enabled := CBLastChange.Checked;
end;

procedure TEERExportSQLScriptFrom.CBAutoIncrementClick(Sender: TObject);
begin
  {enable/disable edits}
  EdAutoIncrementPrefix.Enabled  := CBAutoIncrement.Checked;
  EdAutoIncrementSeqName.Enabled := CBAutoIncrement.Checked;
end;

function TEERExportSQLScriptFrom.GetDtExclusionSqlTableDef(DbType, TbName,
  ColName: string): string;
var
  Str : TStringList;
begin
  //DbType is received just to maintain future compatibility.
  //This code implements a generic table creation

  Str := TStringList.Create;

  Str.Add('CREATE TABLE ' + TbName + ' (');
  Str.Add('  ' + ColName + ' VARCHAR(15), ');
  Str.Add('  TABLE_NAME VARCHAR(64) NOT NULL, ');
  Str.Add('  PRIMARY KEY (TABLE_NAME)');
  Str.Add('); ');

  GetDtExclusionSqlTableDef := Str.Text;
  Str.Free;
end;

procedure TEERExportSQLScriptFrom.CBLastDeleteClick(Sender: TObject);
begin
  EdLastDeleteTriggerPrefix.Enabled := CBLastDelete.Checked;
  EdLastDeleteColName.Enabled       := CBLastDelete.Checked;
  EdLastDeleteTbName.Enabled        := CBLastDelete.Checked;
end;

end.
