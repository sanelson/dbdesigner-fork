unit EERReverseEngineering;

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
// Unit EERReverseEngineering
// --------------------------
// Version Fork 1.0, 31.07.2007, JP  (SetInterfaceToDatabase created)
// Version 1.0, 13.03.2003, Mike
// Description
//   Contains a graphical user interface for the reverse engineering functions
//
// Changes:
//   Version Fork 1.5, 08.04.2010, JP: Select / Deselect all tables.
//   Version Fork 1.5, 13.03.2010, JP: Better support for FireBird using ODBC
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QButtons, QExtCtrls, QComCtrls, EERModel, QCheckLst, Qt, IniFiles,
  QMenus, QTypes;

type
  TEERReverseEngineeringForm = class(TForm)
    StatusPnl: TPanel;
    StatusLbl: TLabel;
    ConnectionSBtn: TSpeedButton;
    OptionPnl: TPanel;
    Bevel1: TBevel;
    SubmitBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    GroupBox1: TGroupBox;
    TblCountEd: TEdit;
    RevEngTypeCBox: TComboBox;
    Label5: TLabel;
    ImportSchemaCBox: TCheckBox;
    CollapseTablesCBox: TCheckBox;
    RelGroupBox: TGroupBox;
    BuildRelPrimKeyRBtn: TRadioButton;
    BuildRelTblNamesRBtn: TRadioButton;
    UseNativeRelationsRBtn: TRadioButton;
    BuildRelationsCBox: TCheckBox;
    DataTypeSubstGroupBox: TGroupBox;
    SubstCBox: TComboBox;
    SubstMemo: TMemo;
    UseSubstCBox: TCheckBox;
    StdInsertsGroupBox: TGroupBox;
    LimitStdInsCBox: TCheckBox;
    LimitStdInsEd: TEdit;
    CreateStdInsertsCBox: TCheckBox;
    TopPnl: TPanel;
    Label1: TLabel;
    DBConnEd: TEdit;
    GetDBConnSBtn: TSpeedButton;
    Label2: TLabel;
    Label4: TLabel;
    SchemaPnl: TPanel;
    SchemaLeftSepPnl: TPanel;
    TablesLBox: TCheckListBox;
    SchemaLookupPnl: TPanel;
    SchemaCBox: TComboBox;
    TablesPopupMenu: TPopupMenu;
    SelectallTables1: TMenuItem;
    DeselectallTables1: TMenuItem;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    PutDefValsInQuotesCBox: TCheckBox;
    ButSelectAll: TButton;
    ButDeselect: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);

    function SetData(theModel: TEERModel): Boolean;

    procedure GetDBConnSBtnClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);

    procedure SelAllTablesSBtnClick(Sender: TObject);
    procedure DeselectAllTablesSBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SubstCBoxCloseUp(Sender: TObject);
    procedure UseSubstCBoxClick(Sender: TObject);
    procedure SchemaCBoxCloseUp(Sender: TObject);

    procedure GetTranslations;

    procedure GetSubstNames;
    procedure CreateStdInsertsCBoxClick(Sender: TObject);
    procedure BuildRelationsCBoxClick(Sender: TObject);
    procedure RevEngTypeCBoxCloseUp(Sender: TObject);
    procedure ButSelectAllClick(Sender: TObject);
    procedure ButDeselectClick(Sender: TObject);
  private
    { Private-Deklarationen }
    //Use flag because of InvisibleForm error
    CheckORCLFuncCBox: Boolean;
    AllTables: TStringList;
    procedure SetInterfaceToDatabase;
  public
    { Public-Deklarationen }
    EERModel: TEERModel;
  end;

var
  EERReverseEngineeringForm: TEERReverseEngineeringForm;

implementation

uses MainDM, EERDM, DBDM, DBEERDM;

{$R *.xfm}

procedure TEERReverseEngineeringForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);

  CheckORCLFuncCBox:=False;

  AllTables:=TStringList.Create;

  GetTranslations;

  //Remove SQLite functions for the moment
  //RevEngTypeCBox.Items.Delete(RevEngTypeCBox.Items.Count-1);
end;

procedure TEERReverseEngineeringForm.GetTranslations;
var theStrings: TStringList;
begin
  theStrings:=TStringList.Create;
  try
    //Translate Methods
    RevEngTypeCBox.Items.Clear;
    DMMain.GetFormResourceStrings(self, 'RevEngMethod', theStrings);
    RevEngTypeCBox.Items.Assign(theStrings);
    if(RevEngTypeCBox.Items.Count=0)then
    begin
      RevEngTypeCBox.Items.Add('Use general functions (ODBC)');
      RevEngTypeCBox.Items.Add('Use MySQL specific functions');
      RevEngTypeCBox.Items.Add('Use Oracle specific functions');
      RevEngTypeCBox.Items.Add('Use SQLite specific functions');
      RevEngTypeCBox.Items.Add('Use MSSQL specific functions');
    end;
  finally
  end;
end;

procedure TEERReverseEngineeringForm.FormDestroy(Sender: TObject);
begin
  AllTables.Free;
end;

procedure TEERReverseEngineeringForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //After work, close connection
  DMDB.DisconnectFromDB;
end;

procedure TEERReverseEngineeringForm.FormShow(Sender: TObject);
begin
  SubstCBoxCloseUp(self);

  SetFocus;

  SetInterfaceToDatabase;
end;

procedure TEERReverseEngineeringForm.GetDBConnSBtnClick(Sender: TObject);
var SelDBConn: TDBConn;
  SelDBConnName, s, s4: string;
  theTables: TStringList;
  i: integer;
  ErrMsg,DriverName: string;
begin
  SelDBConnName:='';

  DBConnEd.Text:='';
  DMDB.DisconnectFromDB;

  StatusLbl.Caption:=DMMain.GetTranslatedMessage('Not connected to a Database', 27);
  ConnectionSBtn.Enabled:=False;
  SubmitBtn.Enabled:=False;

  //do until a successful connection is established or the user selects abort
  while(1=1)do
  begin
    //Let the User choose connection
    SelDBConn:=DMDB.GetUserSelectedDBConn('');
    if(SelDBConn<>nil)then
    begin
      SelDBConnName:=SelDBConn.Name;

      //Try to connect to the DB
      try
        DriverName := SelDBConn.DriverName;
        DMDB.ConnectToDB(SelDBConn);
      except
        on x: Exception do
        begin
          ErrMsg := GetConnectErrorMessage(DriverName);
          MessageDlg(ErrMsg+DMMain.GetTranslatedMessage('%s', 121,
            x.Message), mtError, [mbOK], 0);

          continue;
        end;
      end;

      ConnectionSBtn.Enabled:=True;
      SubmitBtn.Enabled:=True;
      DBConnEd.Text:=DMDB.CurrentDBConn.Name;
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Connected to Database %s', 28,
        DMDB.CurrentDBConn.Params.Values['User_Name']+'@'+
        DMDB.CurrentDBConn.Params.Values['Database']);

      theTables:=TStringList.Create;
      try
        DMDB.GetDBTables(theTables);

        //Ignore DBDesigner4 table
        if(theTables.IndexOf('DBDesigner4')<>-1)then
          theTables.Delete(theTables.IndexOf('DBDesigner4'));

        TablesLBox.Items.Assign(theTables);
      finally
        theTables.Free;
      end;

      AllTables.Assign(TablesLBox.Items);

      //Fill Schema CBox
      SchemaCBox.Items.Clear;
      SchemaCBox.Items.Add('All');
      for i:=0 to TablesLBox.Items.Count-1 do
      begin
        s:=TablesLBox.Items[i];
        if(Copy(s, 1, 1)='"')then
          s:=Copy(s, 2, Length(s)-2);

        if(Copy(s, 1, 3)<>'SYS')and
          (Copy(s, 1, 6)<>'CTXSYS')and
          (Copy(s, 1, 4)<>'RDB$')and
          (Copy(s, 1, 4)<>'MON$')and
          (Copy(s, 1, 5)<>'MDSYS')and
          (Copy(s, 1, 3)<>'ODM')and
          (Copy(s, 1, 7)<>'OLAPSYS')and
          (Copy(s, 1, 2)<>'QS')and
          (Copy(s, 1, 5)<>'WKSYS')and
          (Copy(s, 1, 6)<>'CTXSYS')then
          if(SchemaCBox.Items.IndexOf(Copy(TablesLBox.Items[i], 1, Pos('.', TablesLBox.Items[i])-1))=-1)then
            SchemaCBox.Items.Add(Copy(TablesLBox.Items[i], 1, Pos('.', TablesLBox.Items[i])-1));
      end;
      SchemaCBox.ItemIndex:=0;

      SelAllTablesSBtnClick(self);

      //Do for Access, unselect all tables starting with MSys
      for i:=0 to TablesLBox.Items.Count-1 do
      begin
        s := TablesLBox.Items[i];
        s := SysUtils.StringReplace(s,'"','',[SysUtils.rfReplaceAll]);
        s4 := Copy(s, 1, 4);
        if
          (Copy(TablesLBox.Items[i], 2, 4)='MSys') or
          (s4 ='RDB$')or  //Firebird
          (s4 ='MON$')or  //Firebird
          (s4 ='SYS.')or               //ORACLE
          (Copy(s, 1, 6)='MDSYS.') or  //ORACLE
          (Copy(s, 1, 6)='FLOWS_') or  //ORACLE
          (Copy(s, 1, 7)='CTXSYS.') or //ORACLE
          (Copy(s, 1, 7)='SYSTEM.')    //ORACLE
        then
          TablesLBox.Checked[i]:=False;
      end;
      break;
    end
    else
      break;
  end;
end;

procedure TEERReverseEngineeringForm.SubmitBtnClick(Sender: TObject);
var theTables: TStringList;
  theSubst: TStringList;
  i: integer;
  xcount: integer;
begin
  //Turn off the models FK settings
  EERModel.CreateFKRefDefIndex:=False;
  EERModel.TableNameInRefs:=False;
  EERModel.FKPrefix:='';
  EERModel.FKPostfix:='';

  theTables:=TStringList.Create;
  theSubst:=TStringList.Create;
  try
    for i:=0 to TablesLBox.Items.Count-1 do
      if(TablesLBox.State[i]=cbChecked)then
        theTables.Add(TablesLBox.Items[i]);

    try
      xcount:=StrToInt(TblCountEd.Text);
    except
      xcount:=5;
    end;

    if(UseSubstCBox.Checked)then
      theSubst.Text:=SubstMemo.Lines.Text
    else
      theSubst:=nil;

    //Limit StdIns Records
    i:=0;
    if(LimitStdInsCBox.Checked)then
    begin
      try
        i:=StrToInt(LimitStdInsEd.Text);
      except
        i:=20;
      end;
    end;

    //Do the reverse engineering
    case RevEngTypeCBox.ItemIndex of
      0:
        //ODBC
        DMDBEER.EERReverseEngineer(EERModel, DMDB.CurrentDBConn, theTables, xcount, BuildRelationsCBox.Checked, BuildRelPrimKeyRBtn.Checked, theSubst, StatusLbl, CreateStdInsertsCBox.Checked, i);
      1:
        DMDBEER.EERMySQLReverseEngineer(EERModel, DMDB.CurrentDBConn, theTables, xcount, BuildRelationsCBox.Checked, BuildRelPrimKeyRBtn.Checked, theSubst, StatusLbl, CreateStdInsertsCBox.Checked, i);
      2:
        DMDBEER.EERORCLReverseEngineer(EERModel, DMDB.CurrentDBConn, theTables, xcount, BuildRelationsCBox.Checked, BuildRelPrimKeyRBtn.Checked, theSubst, StatusLbl, CreateStdInsertsCBox.Checked, i,
          ImportSchemaCBox.Checked, PutDefValsInQuotesCBox.Checked);
      3:
        DMDBEER.EERSQLiteReverseEngineer(EERModel, DMDB.CurrentDBConn, theTables, xcount, BuildRelationsCBox.Checked, BuildRelPrimKeyRBtn.Checked, theSubst, StatusLbl, CreateStdInsertsCBox.Checked, i);
      4:
        DMDBEER.EERMSSQLReverseEngineer(EERModel, DMDB.CurrentDBConn, theTables, xcount, BuildRelationsCBox.Checked, BuildRelPrimKeyRBtn.Checked, theSubst, StatusLbl, CreateStdInsertsCBox.Checked, i, CollapseTablesCBox.Checked);
    end;

  finally
    theTables.Free;
    theSubst.Free;
  end;

  EERModel.ModelHasChanged;

  DMEER.RefreshPalettes;

  ModalResult:=mrOK;
end;

function TEERReverseEngineeringForm.SetData(theModel: TEERModel): Boolean;
begin
  EERModel:=theModel;

  GetSubstNames;

  GetDBConnSBtnClick(self);

  //When not connected, close
  SetData:=(DMDB.CurrentDBConn<>nil);

  CreateStdInsertsCBoxClick(self);
  BuildRelationsCBoxClick(self);
  UseSubstCBoxClick(self);
end;

procedure TEERReverseEngineeringForm.SelAllTablesSBtnClick(
  Sender: TObject);
var i: integer;
begin
  for i:=0 to TablesLBox.Items.Count-1 do
    TablesLBox.State[i]:=cbChecked;
end;

procedure TEERReverseEngineeringForm.DeselectAllTablesSBtnClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to TablesLBox.Items.Count-1 do
    TablesLBox.State[i]:=cbUnChecked;
end;

procedure TEERReverseEngineeringForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult:=mrAbort;
end;

procedure TEERReverseEngineeringForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if((Key=Key_Return)or(Key=Key_Enter))and
    (ActiveControl<>SubstMemo)then
    SubmitBtnClick(self);

  if(Key=Key_F1)then
    DMMain.ShowHelp('db', 'reveng');
end;

procedure TEERReverseEngineeringForm.GetSubstNames;
var theIniFile: TMemIniFile;
  theStringList: TStringList;
  i: integer;
begin
  SubstCBox.Items.Clear;

  theIniFile:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_DatabaseInfo.ini');
  try
    theStringList:=TStringList.Create;
    try
      theIniFile.ReadSectionValues(EERModel.DatabaseType+'_DatatypeSubst', theStringList);

      for i:=0 to theStringList.Count-1 do
        SubstCBox.Items.Add(theStringList.ValueFromIndex[i]);
    finally
      theStringList.Free;
    end;
  finally
    theIniFile.Free;
  end;
end;

procedure TEERReverseEngineeringForm.SubstCBoxCloseUp(Sender: TObject);
var theIniFile: TMemIniFile;
  s: string;
begin
  theIniFile:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_DatabaseInfo.ini');
  try
    s:=Copy(SubstCBox.Items[SubstCBox.ItemIndex], 1, Pos(' ', SubstCBox.Items[SubstCBox.ItemIndex])-1)+
      '_'+EERModel.DatabaseType+'_DatatypeSubst';
    theIniFile.ReadSectionValues(s, SubstMemo.Lines);
  finally
    theIniFile.Free;
  end;
end;

procedure TEERReverseEngineeringForm.UseSubstCBoxClick(Sender: TObject);
begin
  SubstCBox.Enabled:=UseSubstCBox.Checked;
  SubstMemo.Enabled:=UseSubstCBox.Checked;
end;

procedure TEERReverseEngineeringForm.SchemaCBoxCloseUp(Sender: TObject);
var i: integer;
begin
  TablesLBox.Items.Assign(AllTables);

  if(SchemaCBox.ItemIndex>0)then
  begin
    i:=0;
    while(i<TablesLBox.Items.Count)do
    begin
      if(SchemaCBox.Items[SchemaCBox.ItemIndex]<>Copy(TablesLBox.Items[i], 1, Pos('.', TablesLBox.Items[i])-1))then
        TablesLBox.Items.Delete(i)
      else
        inc(i);
    end;
  end;

  SelAllTablesSBtnClick(self);
end;

procedure TEERReverseEngineeringForm.CreateStdInsertsCBoxClick(
  Sender: TObject);
begin
  LimitStdInsCBox.Enabled:=CreateStdInsertsCBox.Checked;
  LimitStdInsEd.Enabled:=CreateStdInsertsCBox.Checked;
end;

procedure TEERReverseEngineeringForm.BuildRelationsCBoxClick(
  Sender: TObject);
begin
  RevEngTypeCBoxCloseUp(self);
end;

procedure TEERReverseEngineeringForm.RevEngTypeCBoxCloseUp(
  Sender: TObject);
begin
  //Native Relations only work for Oracle and MSSQL
  UseNativeRelationsRBtn.Enabled:=
    ((Pos('Oracle', RevEngTypeCBox.Items[RevEngTypeCBox.ItemIndex])>0)or
    (Pos('MSSQL', RevEngTypeCBox.Items[RevEngTypeCBox.ItemIndex])>0))and
    BuildRelationsCBox.Checked;

  BuildRelPrimKeyRBtn.Enabled:=(Not((Pos('Oracle', RevEngTypeCBox.Items[RevEngTypeCBox.ItemIndex])>0)or
    (Pos('MSSQL', RevEngTypeCBox.Items[RevEngTypeCBox.ItemIndex])>0)))and
    BuildRelationsCBox.Checked;
  BuildRelTblNamesRBtn.Enabled:=BuildRelPrimKeyRBtn.Enabled;

  //Deselect UseNativeRelationsRBtn when it's not enabled
  if(Not(UseNativeRelationsRBtn.Enabled))and
    (UseNativeRelationsRBtn.Checked)then
    BuildRelPrimKeyRBtn.Checked:=True;

  if(UseNativeRelationsRBtn.Enabled)and(Not(UseNativeRelationsRBtn.Checked))then
    UseNativeRelationsRBtn.Checked:=True;

  //ImportSchema only works with oracle
  ImportSchemaCBox.Enabled:=(Pos('Oracle', RevEngTypeCBox.Items[RevEngTypeCBox.ItemIndex])>0);

  //PutDefValsInQuotesCBox only works with oracle
  PutDefValsInQuotesCBox.Enabled:=(Pos('Oracle', RevEngTypeCBox.Items[RevEngTypeCBox.ItemIndex])>0);
end;

procedure TEERReverseEngineeringForm.SetInterfaceToDatabase;
begin
      if(CompareText(DMDB.CurrentDBConn.DriverName, 'MySQL')=0)then
        RevEngTypeCBox.ItemIndex:=1  //MySQL funcs
      else if(CompareText(DMDB.CurrentDBConn.DriverName, 'Oracle')=0)then
      begin
        //CheckORCLFuncCBox:=True;
        RevEngTypeCBox.ItemIndex:=2; //Oracle funcs
        SubstCBox.ItemIndex:=1;

        ImportSchemaCBox.Enabled:=True;

        BuildRelationsCBox.Visible:=False; //CLX Bug workaround
        BuildRelationsCBox.Checked:=True;
        BuildRelationsCBox.Visible:=True; //CLX Bug workaround

        UseNativeRelationsRBtn.Visible:=False; //CLX Bug workaround
        UseNativeRelationsRBtn.Enabled:=True;
        UseNativeRelationsRBtn.Checked:=True;
        UseNativeRelationsRBtn.Visible:=True; //CLX Bug workaround
      end
      else if(CompareText(DMDB.CurrentDBConn.DriverName, 'SQLite')=0)then
      begin
        RevEngTypeCBox.ItemIndex:=3;  //SQLite funcs
        SubstCBox.ItemIndex:=3;

      end
      else if(CompareText(DMDB.CurrentDBConn.DriverName, 'MSSQL')=0)then
      begin
        RevEngTypeCBox.ItemIndex:=4;  //MSSQL funcs
        SubstCBox.ItemIndex:=4;

        BuildRelationsCBox.Visible:=False; //CLX Bug workaround
        BuildRelationsCBox.Checked:=True;
        BuildRelationsCBox.Visible:=True; //CLX Bug workaround

        UseNativeRelationsRBtn.Visible:=False; //CLX Bug workaround
        UseNativeRelationsRBtn.Enabled:=True;
        UseNativeRelationsRBtn.Checked:=True;
        UseNativeRelationsRBtn.Visible:=True; //CLX Bug workaround
      end
      else
      begin
        RevEngTypeCBox.ItemIndex:=0;
        SubstCBox.ItemIndex:=2;
      end;

end;

procedure TEERReverseEngineeringForm.ButSelectAllClick(Sender: TObject);
  var i:integer;
begin
  for i:=0 to TablesLBox.Items.Count-1 do
    TablesLBox.State[i]:=cbChecked;
end;

procedure TEERReverseEngineeringForm.ButDeselectClick(Sender: TObject);
  var i:integer;
begin
  for i:=0 to TablesLBox.Items.Count-1 do
    TablesLBox.State[i]:=cbUnchecked;
end;

end.
