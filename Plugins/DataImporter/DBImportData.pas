unit DBImportData;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QCheckLst, QButtons, QGrids, QComCtrls, QExtCtrls,
  Qt, IniFiles, SqlExpr, DBXpress, FMTBcd, DB, MainDM, DBDM;

type
  PTableImportOptions = ^TTableImportOptions;
  TTableImportOptions = record
    Tablename: string;
    TableDirectory: string;
    Tableindex: integer;
    TextImportWithSep: Boolean;
    TextImportFirstRowHoldsColumnNames: Boolean;
    TextImportTextDelim,
    TextImportTextSep: string;
    TextColumns: TStringList;
    TextTestLines: TStringList;
    ColumnNames: TStringList;
    DestTableName: string;
    DestColumns,
    DestColumnsValues: TStringList;
    DelDestTableBeforInserts,
    TrimColumns: Boolean;
    ReplaceChars: TStringList;
    CheckReturnsInTxt: Boolean;
    PresetName: string;
  end;

  TDBImportDataForm = class(TForm)
    DirDlg: TSaveDialog;
    Label4: TLabel;
    NewPresetBtn: TSpeedButton;
    Bevel2: TBevel;
    PresetLU: TComboBox;
    PageControl: TPageControl;
    TextOptionsSheet: TTabSheet;
    DataLbl: TLabel;
    FixedLengthHintLbl: TLabel;
    Bevel3: TBevel;
    Label12: TLabel;
    FixLengthOptionsGBox: TGroupBox;
    ColumnsLbl: TLabel;
    SetColsSBtn: TSpeedButton;
    IgnoreFirstLineCBox: TCheckBox;
    ColumnsEd: TEdit;
    SepOptionsGBox: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    SepTabRBtn: TRadioButton;
    SepSemicolonRBtn: TRadioButton;
    SepCommaRBtn: TRadioButton;
    SepSpaceRBtn: TRadioButton;
    SepUserDefRBtn: TRadioButton;
    SepUserDefEd: TEdit;
    TextDelimLU: TComboBox;
    FirstRowHoldsColNameCBox: TCheckBox;
    TextWithSepRBtn: TRadioButton;
    TextGrid: TStringGrid;
    TextWithFixedLengthRBtn: TRadioButton;
    ColNameEd: TEdit;
    MappingSheet: TTabSheet;
    Label7: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    DestTblLU: TComboBox;
    SourceDG: TDrawGrid;
    DestDG: TDrawGrid;
    SpecialFieldsLBox: TListBox;
    AutoMappingBtn: TBitBtn;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    DelDestTblCBox: TCheckBox;
    TrimColumnsCBox: TCheckBox;
    CheckReturnsInTxtCBox: TCheckBox;
    GroupBox2: TGroupBox;
    ReplaceChrGrid: TStringGrid;
    StatusPnl: TPanel;
    ConnectionSBtn: TSpeedButton;
    StatusLbl: TLabel;
    ModePageControl: TPageControl;
    TextImportSheet: TTabSheet;
    DBImportSheet: TTabSheet;
    DirEd: TEdit;
    Label2: TLabel;
    BrowseDirOrDBConnBtn: TSpeedButton;
    SourceLBox: TCheckListBox;
    Label3: TLabel;
    SubmitBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    Label1: TLabel;
    SourceDBConnEd: TEdit;
    GetSourceDBConnSBtn: TSpeedButton;
    SourceTblsLBox: TCheckListBox;
    Label8: TLabel;
    Label13: TLabel;
    DBConnEd: TEdit;
    GetDBConnSBtn: TSpeedButton;
    SourceSQLConn: TSQLConnection;
    SourceSQLDataSet: TSQLDataSet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure SetData(FileMode: Boolean; DirOrDBConn: string;
      DestTable: string = ''; DefaultMappingData: string = '');
    procedure RefreshFileList;
    procedure BrowseDirOrDBConnBtnClick(Sender: TObject);
    procedure DirEdKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SourceLBoxClick(Sender: TObject);

    procedure ShowTableOptions(TblIndex: integer);
    procedure SetTableOptions(Sender: TObject);

    function GetTblOpt(nr: integer): PTableImportOptions;
    procedure TextGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure TextGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TextGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetColsSBtnClick(Sender: TObject);
    procedure TextGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TextGridKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure NewPresetBtnClick(Sender: TObject);

    procedure GetPresetList;
    procedure SavePresetToIniFile(name: string);
    procedure GetPresetsFromIniFile(name: string);

    procedure PresetLUCloseUp(Sender: TObject);
    procedure SourceDGDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DestTblLUCloseUp(Sender: TObject);
    procedure DestDGDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure SourceDGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DestDGDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DestDGDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DestDGKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpecialFieldsLBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImportBtnClick(Sender: TObject);
    procedure ColNameEdChange(Sender: TObject);

    procedure DisplayColName(Row: integer);
    procedure AutoMappingBtnClick(Sender: TObject);
    procedure ReplaceChrGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: WideString);

    procedure GetSourceDBConnSBtnClick(Sender: TObject);
    procedure ModePageControlChange(Sender: TObject);
    procedure GetDBConnSBtnClick(Sender: TObject);
    procedure FirstRowHoldsColNameCBoxClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);

    procedure SetDestDBConn(theDBConn: Pointer);

    procedure SetDirectoryOrFilename(dirOrFileName: string);
  private
    { Private declarations }
    SourceDir: string;
    OptionsForTable: integer;
    ActiveTableOptions: PTableImportOptions;
    TblOptList: TList;

    MouseInDown: Boolean;
    IsSettingOptions: Boolean;

    SourceColumn: integer;

    DefaultMapping: TStringList;

    SourceDBConn, DestDBConn: TDBConn;
  public
    { Public declarations }
    Version: string;
  end;

var
  DBImportDataForm: TDBImportDataForm;

implementation

uses EditorString, Progress;

{$R *.xfm}

function SortStringListWithInts(List: TStringList; Index1, Index2: Integer): Integer;
begin
  SortStringListWithInts:=0;
  try
    if(StrToInt(List[Index1])<StrToInt(List[Index2]))then
      SortStringListWithInts:=-1
    else if(StrToInt(List[Index1])>StrToInt(List[Index2]))then
      SortStringListWithInts:=1;
  except
  end;
end;

procedure TDBImportDataForm.FormCreate(Sender: TObject);
begin
  Version:='1.0.0.24';

  SourceDir:='';
  OptionsForTable:=-1;
  ActiveTableOptions:=nil;
  TblOptList:=TList.Create;
  MouseInDown:=False;
  IsSettingOptions:=False;

  DefaultMapping:=TStringList.Create;

  DestDG.ColCount:=2;
  DestDG.ColWidths[0]:=80;
  DestDG.ColWidths[1]:=250;

  GetPresetList;

  PageControl.ActivePageIndex:=0;

  ReplaceChrGrid.Cells[0, 0]:='Char';
  ReplaceChrGrid.Cells[1, 0]:='Replace with';

  SourceDir:=DMMain.LoadValueFromSettingsIniFile('RecentDirectories', 'RecentImportDataFileDir', '');

  //Center Window but beware of two screens
  Top:=(Screen.Height-Height) div 2;
  if(Screen.Width=(Screen.Height/0.75)*2)or
    (Screen.Width=(Screen.Height*1.25)*2)then
    Left:=((Screen.Width div 2)-Width) div 2
  else
    Left:=(Screen.Width-Width) div 2;

  StatusLbl.Caption:='Not connected to a Database. Version '+Version;

  RefreshFileList;
end;

procedure TDBImportDataForm.FormDestroy(Sender: TObject);
var i: integer;
begin
  for i:=0 to TblOptList.Count-1 do
  begin
    PTableImportOptions(TblOptList[i]).TextColumns.Free;
    PTableImportOptions(TblOptList[i]).TextTestLines.Free;
    PTableImportOptions(TblOptList[i]).ColumnNames.Free;
    PTableImportOptions(TblOptList[i]).DestColumns.Free;
    PTableImportOptions(TblOptList[i]).DestColumnsValues.Free;
    PTableImportOptions(TblOptList[i]).ReplaceChars.Free;

    dispose(TblOptList[i]);
  end;

  TblOptList.Free;

  DefaultMapping.Free;
end;

procedure TDBImportDataForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if(ModalResult<>mrOK)then
    ModalResult:=mrAbort;
end;

procedure TDBImportDataForm.SetData(FileMode: Boolean;
  DirOrDBConn: string; DestTable: string = ''; DefaultMappingData: string = '');
var theTables: TStringList;
begin
  if(FileMode)then
  begin
    ModePageControl.ActivePage:=TextImportSheet;

    //Wenn keine Filename sondern Dir übergeben wurde...
    if(ExtractFileName(DirOrDBConn)='')then
    begin
      SourceDir:=DirOrDBConn;
      RefreshFileList;
    end
    else
    begin
      SourceLBox.Clear;
      SourceDir:=ExtractFilePath(DirOrDBConn);
      DirEd.Text:=SourceDir;
      SourceLBox.Items.Add(ExtractFileName(DirOrDBConn));

      SourceLBox.ItemIndex:=0;
      SourceLBox.Checked[0]:=True;
      SourceLBoxClick(self);
    end;

  end
  else
  begin
    ModePageControl.ActivePage:=DBImportSheet;
  end;

  theTables:=TStringList.Create;
  try
    DMDB.GetDBTables(theTables);
    DestTblLU.Items.Assign(theTables);

    DestTblLU.ItemIndex:=DestTblLU.Items.IndexOf(DestTable);

    if(DestTblLU.ItemIndex=-1)then
      DestTblLU.ItemIndex:=0;
  finally
    theTables.Free;
  end;

  DefaultMapping.Text:=DefaultMappingData;

  DestTblLUCloseUp(self);
end;

procedure TDBImportDataForm.RefreshFileList;
var sr: TSearchRec;
begin
  if(ModePageControl.ActivePage=TextImportSheet)then
  begin
    //Show all files in current dir
    if(SourceDir<>'')then
    begin
      SourceLBox.Items.Clear;

      if(Copy(SourceDir, Length(SourceDir), 1)<>PathDelim)then
        SourceDir:=SourceDir+PathDelim;

      DirEd.Text:=SourceDir;

      //Add all Files
      if FindFirst(SourceDir+'*.*', faAnyFile, sr) = 0 then
      begin
        repeat
          if((sr.Attr and faDirectory)<>faDirectory)then
          begin
            SourceLBox.Items.Add(sr.Name);
          end;
        until FindNext(sr) <> 0;
        FindClose(sr);
      end;
    end;
  end
  else
  begin
    //Show DB-Tables
  end;
end;

procedure TDBImportDataForm.SetDirectoryOrFilename(dirOrFileName: string);
var i: integer;
begin
  SourceDir:=ExtractFilePath(dirOrFileName);

  RefreshFileList;

  if(FileExists(dirOrFileName))then
  begin
    for i:=0 to SourceLBox.Items.Count-1 do
      if(CompareText(SourceLBox.Items[i], ExtractFileName(dirOrFileName))=0)then
      begin
        SourceLBox.Checked[i]:=True;
        SourceLBox.ItemIndex:=i;

        //Set file
        SourceLBoxClick(self);

        break;
      end;
  end;

  DMMain.SaveValueInSettingsIniFile('RecentDirectories', 'RecentImportDataFileDir', ExtractFilePath(dirOrFileName));
end;

procedure TDBImportDataForm.BrowseDirOrDBConnBtnClick(Sender: TObject);
var DirDlg: TOpenDialog;
begin
  DirDlg:=TOpenDialog.Create(self);

  DirDlg.InitialDir:=DMMain.LoadValueFromSettingsIniFile('RecentDirectories', 'RecentImportDataFileDir', '');
  try
    //Get Dir
    DirDlg.FileName:='Select a File or Directory';
    if(DirDlg.Execute)then
    begin
      SetDirectoryOrFilename(DirDlg.FileName);
    end;
  finally
    DirDlg.Free;
  end;
end;

procedure TDBImportDataForm.GetSourceDBConnSBtnClick(Sender: TObject);
var theTables: TStringList;
begin
  SourceDBConnEd.Text:='';
  DMDB.DisconnectFromDB;

  //do until a successful connection is established or the user selects abort
  while(1=1)do
  begin
    //Let the User choose connection
    SourceDBConn:=DMDB.GetUserSelectedDBConn('');
    if(SourceDBConn<>nil)then
    begin
      //Try to connect to the DB
      try
        DMDB.ConnectToDB(SourceDBConn, SourceSQLConn);
      except
        on x: Exception do
        begin
          MessageDlg('Connection to database failed.'+#13#10#13#10+
            x.Message, mtError, [mbOK], 0);

          continue;
        end;
      end;

      SourceDBConnEd.Text:=SourceDBConn.Name;

      SourceTblsLBox.Items.Clear;

      theTables:=TStringList.Create;
      try
        DMDB.GetDBTables(theTables, SourceSQLConn, SourceDBConn);

        //Ignore DBDesigner4 table
        if(theTables.IndexOf('DBDesigner4')<>-1)then
          theTables.Delete(theTables.IndexOf('DBDesigner4'));

        SourceTblsLBox.Items.Assign(theTables);
      finally
        theTables.Free;
      end;

      break;
    end
    else
      break;
  end;
end;

procedure TDBImportDataForm.DirEdKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(key=Key_Return)or(key=Key_Enter)then
    RefreshFileList;
end;

procedure TDBImportDataForm.SourceLBoxClick(Sender: TObject);
var theTblOpt: PTableImportOptions;
begin
  if(SourceLBox.ItemIndex<0)then
    Exit;

  //when a table is selected and checked, show it's options
  if(SourceLBox.ItemIndex<>OptionsForTable)and(SourceLBox.Checked[SourceLBox.ItemIndex])then
  begin
    OptionsForTable:=SourceLBox.ItemIndex;

    ShowTableOptions(OptionsForTable);
  end
  else if(Not(SourceLBox.Checked[SourceLBox.ItemIndex]))then
  begin
    theTblOpt:=GetTblOpt(SourceLBox.ItemIndex);

    //If this table has ImportOptions delete them
    if(theTblOpt<>nil)then
    begin
      TblOptList.Delete(TblOptList.IndexOf(theTblOpt));

      theTblOpt.TextColumns.Free;
      theTblOpt.TextTestLines.Free;
      theTblOpt.ColumnNames.Free;
      theTblOpt.DestColumns.Free;
      theTblOpt.DestColumnsValues.Free;
      theTblOpt.ReplaceChars.Free;

      dispose(theTblOpt);

      ActiveTableOptions:=nil;
    end;

  end;
end;

procedure TDBImportDataForm.ShowTableOptions(TblIndex: integer);
var theTblOpt: PTableImportOptions;
  theFile: TextFile;
  i, j, maxlen: integer;
  s: string;
begin
  if(ModePageControl.ActivePage=TextImportSheet)then
  begin
    //File mode
    theTblOpt:=GetTblOpt(TblIndex);

    //If this table has no ImportOptions yet, create them
    if(theTblOpt=nil)then
    begin
      new(theTblOpt);
      theTblOpt.Tablename:=SourceLBox.Items[TblIndex];
      theTblOpt.TableDirectory:=SourceDir;
      theTblOpt.Tableindex:=TblIndex;
      theTblOpt.TextImportWithSep:=True;
      theTblOpt.TextImportFirstRowHoldsColumnNames:=True;
      theTblOpt.TextImportTextDelim:='"';
      theTblOpt.TextImportTextSep:=';';
      theTblOpt.TextColumns:=TStringList.Create;
      theTblOpt.TextColumns.Add('1');
      theTblOpt.TextTestLines:=TStringList.Create;
      theTblOpt.ColumnNames:=TStringList.Create;
      theTblOpt.DestTableName:='';
      theTblOpt.DestColumns:=TStringList.Create;
      theTblOpt.DestColumnsValues:=TStringList.Create;
      theTblOpt.DelDestTableBeforInserts:=False;
      theTblOpt.TrimColumns:=True;
      theTblOpt.ReplaceChars:=TStringList.Create;
      theTblOpt.CheckReturnsInTxt:=True;
      theTblOpt.PresetName:='';

      //Get TestLines
      AssignFile(theFile, SourceDir+theTblOpt.Tablename);
      Reset(theFile);
      try
        i:=0;
        while(Not(EOF(theFile)))and(i<20)do
        begin
          ReadLn(theFile, s);

          if(Trim(s)='')then
            continue;

          theTblOpt.TextTestLines.Add(s);
          inc(i);
        end;
      finally
        CloseFile(theFile);
      end;


      TblOptList.Add(theTblOpt);

      //Get DestTable columns
      ActiveTableOptions:=theTblOpt;
      DestTblLUCloseUp(self);
    end;

    //Show Options of selected Table
    ActiveTableOptions:=theTblOpt;

    IsSettingOptions:=True;

    TextWithSepRBtn.Checked:=theTblOpt.TextImportWithSep;
    SepOptionsGBox.Visible:=theTblOpt.TextImportWithSep;

    TextWithFixedLengthRBtn.Checked:=Not(theTblOpt.TextImportWithSep);
    FixLengthOptionsGBox.Visible:=Not(theTblOpt.TextImportWithSep);

    FixedLengthHintLbl.Visible:=TextWithFixedLengthRBtn.Checked;

    FirstRowHoldsColNameCBox.Checked:=theTblOpt.TextImportFirstRowHoldsColumnNames;
    IgnoreFirstLineCBox.Checked:=theTblOpt.TextImportFirstRowHoldsColumnNames;

    if(theTblOpt.TextImportTextDelim='"')then
      TextDelimLU.ItemIndex:=0
    else if(theTblOpt.TextImportTextDelim='''')then
      TextDelimLU.ItemIndex:=1
    else
      TextDelimLU.ItemIndex:=2;

    if(theTblOpt.TextImportTextSep='_tab')then
      SepTabRBtn.Checked:=True
    else if(theTblOpt.TextImportTextSep=';')then
      SepSemicolonRBtn.Checked:=True
    else if(theTblOpt.TextImportTextSep=',')then
      SepCommaRBtn.Checked:=True
    else if(theTblOpt.TextImportTextSep=' ')then
      SepSpaceRBtn.Checked:=True
    else
    begin
      SepUserDefRBtn.Checked:=True;
      SepUserDefEd.Text:=theTblOpt.TextImportTextSep;
    end;

    s:='';

    //Check if all values are integers
    i:=0;
    while(i<theTblOpt.TextColumns.Count)do
    begin
      try
        StrToInt(theTblOpt.TextColumns[i]);
        inc(i);
      except
        theTblOpt.TextColumns.Delete(i);
      end;
    end;

    IsSettingOptions:=False;

    TextGrid.RowCount:=theTblOpt.TextTestLines.Count;

    //Preview Grid mit TestLines auffüllen
    if(Not(theTblOpt.TextImportWithSep))then
    begin
      //get max length
      maxlen:=0;
      for i:=0 to theTblOpt.TextTestLines.Count-1 do
        if(maxlen<Length(theTblOpt.TextTestLines[i]))then
          maxlen:=Length(theTblOpt.TextTestLines[i]);

      TextGrid.ColCount:=maxlen;
      TextGrid.DefaultColWidth:=10;


      for i:=0 to theTblOpt.TextTestLines.Count-1 do
        for j:=0 to Length(theTblOpt.TextTestLines[i])-1 do
          TextGrid.Cells[j, i]:=theTblOpt.TextTestLines[i][j+1];

      //sicherstellen, daß erste und letzte Row in der ColumnList
      if(theTblOpt.TextColumns.IndexOf('1')=-1)then
        theTblOpt.TextColumns.Insert(0, '1');
      if(theTblOpt.TextColumns.IndexOf(IntToStr(maxlen+1))=-1)then
      begin
        theTblOpt.TextColumns.Add(IntToStr(maxlen+1));
        ColNameEd.Text:='Field 1';
      end;

      //Spaltenpositionen setzen
      theTblOpt.TextColumns.CustomSort(@SortStringListWithInts);
      for i:=0 to theTblOpt.TextColumns.Count-1 do
      begin
        s:=s+theTblOpt.TextColumns[i];
        if(i<theTblOpt.TextColumns.Count-1)then
          s:=s+', ';

        if(theTblOpt.ColumnNames.Count<=i)then
          theTblOpt.ColumnNames.Add('Field '+IntToStr(i+1));
      end;
      ColumnsEd.Text:=s;

      //Überflüssige ColNames löschen
      while(theTblOpt.ColumnNames.Count>=theTblOpt.TextColumns.Count)and
        (theTblOpt.ColumnNames.Count>0)do
        theTblOpt.ColumnNames.Delete(theTblOpt.ColumnNames.Count-1);
    end
    else
    begin
      //get Max Col Anz
      maxlen:=0;
      for i:=0 to theTblOpt.TextTestLines.Count-1 do
        if(maxlen<DMMain.GetColumnCountFromSepString(theTblOpt.TextTestLines[i],
          theTblOpt.TextImportTextSep,
          theTblOpt.TextImportTextDelim))then
          maxlen:=DMMain.GetColumnCountFromSepString(theTblOpt.TextTestLines[i],
            theTblOpt.TextImportTextSep,
            theTblOpt.TextImportTextDelim);

      TextGrid.ColCount:=maxlen;
      if(maxlen>1)then
        TextGrid.DefaultColWidth:=100
      else
        TextGrid.DefaultColWidth:=500;

      if(theTblOpt.TextImportFirstRowHoldsColumnNames)then
        theTblOpt.ColumnNames.Clear;

      for i:=0 to theTblOpt.TextTestLines.Count-1 do
        for j:=0 to maxlen-1 do
        begin
          TextGrid.Cells[j, i]:=DMMain.GetColumnFromSepString(theTblOpt.TextTestLines[i], j,
            theTblOpt.TextImportTextSep,
            theTblOpt.TextImportTextDelim);

          if(i=0)and
            (theTblOpt.TextImportFirstRowHoldsColumnNames)then
              theTblOpt.ColumnNames.Add(TextGrid.Cells[j, i]);

          if(theTblOpt.ColumnNames.Count<=j)then
            theTblOpt.ColumnNames.Add('Field '+IntToStr(j+1));
        end;

      DelDestTblCBox.Checked:=theTblOpt.DelDestTableBeforInserts;
      TrimColumnsCBox.Checked:=theTblOpt.TrimColumns;
    end;

    //Workaround, so Clear of Table-cells doesn't call CellEdit
    ReplaceChrGrid.Tag:=1;
    try
      ReplaceChrGrid.Cols[0].Clear;
      ReplaceChrGrid.Cols[1].Clear;
      ReplaceChrGrid.Cells[0, 0]:='Char';
      ReplaceChrGrid.Cells[1, 0]:='Replace with';

      for i:=0 to theTblOpt.ReplaceChars.Count-1 do
      begin
        ReplaceChrGrid.Cells[0, i+1]:=theTblOpt.ReplaceChars.Names[i];
        ReplaceChrGrid.Cells[1, i+1]:=theTblOpt.ReplaceChars.Values[theTblOpt.ReplaceChars.Names[i]];
      end;
    finally
      //Workaround, so Clear of Table-cells doesn't call CellEdit
      ReplaceChrGrid.Tag:=0;
    end;

    SourceDG.RowCount:=theTblOpt.ColumnNames.Count;

    TextGrid.Refresh;
    DisplayColName(TextGrid.Selection.Left+1);

    DestDG.Refresh;
  end
  else
  begin
    //DB mode
  end;
end;



procedure TDBImportDataForm.SetTableOptions(Sender: TObject);
begin
  if(ActiveTableOptions<>nil)and(Not(IsSettingOptions))then
  begin
    ActiveTableOptions.TextImportWithSep:=TextWithSepRBtn.Checked;
    
    if(ActiveTableOptions.TextImportWithSep)then
      ActiveTableOptions.TextImportFirstRowHoldsColumnNames:=FirstRowHoldsColNameCBox.Checked
    else
      ActiveTableOptions.TextImportFirstRowHoldsColumnNames:=IgnoreFirstLineCBox.Checked;

    ActiveTableOptions.TextImportTextDelim:=TextDelimLU.Items[TextDelimLU.ItemIndex];
    if(SepTabRBtn.Checked)then
      ActiveTableOptions.TextImportTextSep:='_tab'
    else if(SepSemicolonRBtn.Checked)then
      ActiveTableOptions.TextImportTextSep:=';'
    else if(SepCommaRBtn.Checked)then
      ActiveTableOptions.TextImportTextSep:=','
    else if(SepSpaceRBtn.Checked)then
      ActiveTableOptions.TextImportTextSep:=' '
    else
      ActiveTableOptions.TextImportTextSep:=SepUserDefEd.Text;

    ActiveTableOptions.DelDestTableBeforInserts:=DelDestTblCBox.Checked;
    ActiveTableOptions.TrimColumns:=TrimColumnsCBox.Checked;
    ActiveTableOptions.CheckReturnsInTxt:=CheckReturnsInTxtCBox.Checked;

    ShowTableOptions(ActiveTableOptions.Tableindex);
  end;
end;

function TDBImportDataForm.GetTblOpt(nr: integer): PTableImportOptions;
var i: integer;
begin
  GetTblOpt:=nil;

  for i:=0 to TblOptList.Count-1 do
    if(PTableImportOptions(TblOptList[i]).Tableindex=nr)then
    begin
      GetTblOpt:=TblOptList[i];
      break;
    end;
end;

procedure TDBImportDataForm.TextGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if(ActiveTableOptions<>nil)then
  begin
    with TextGrid.Canvas do
    begin

      if(Not(ActiveTableOptions.TextImportWithSep))and(
        (gdSelected in State)or
        ((ACol>=TextGrid.Selection.Left)and(ACol<=TextGrid.Selection.Right)and(Not(MouseInDown))))then
      begin
        Pen.Color:=$00C5884F;
        Brush.Color:=$00A56E3A;
      end
      else
      begin
        Pen.Color:=$00E5E5E5;
        if(ARow=0)and
          (ActiveTableOptions.TextImportFirstRowHoldsColumnNames)then
          Brush.Color:=clGray
        else
          Brush.Color:=clWhite;
      end;
      Rectangle(Rect.Left-1, Rect.Top-1, Rect.Right, Rect.Bottom);

      if(Not(ActiveTableOptions.TextImportWithSep))and(
        (gdSelected in State)or
        ((ACol>=TextGrid.Selection.Left)and(ACol<=TextGrid.Selection.Right)and(Not(MouseInDown))))then
        Font.Color:=clWhite
      else
        Font.Color:=clBlack;
      Textout(Rect.Left+1, Rect.Top+1, TextGrid.Cells[ACol, ARow]);

      if(ActiveTableOptions.TextColumns.IndexOf(IntToStr(ACol+1))>-1)then
      begin
        Pen.Color:=clRed;
        MoveTo(Rect.Left-1, Rect.Top);
        LineTo(Rect.Left-1, Rect.Bottom);
      end;

      if(ActiveTableOptions.TextColumns.IndexOf(IntToStr(ACol+2))>-1)then
      begin
        Pen.Color:=clRed;
        Pen.Width:=2;
        MoveTo(Rect.Right-1, Rect.Top);
        LineTo(Rect.Right-1, Rect.Bottom);
        Pen.Width:=1;
      end;
    end;
  end;
end;

procedure TDBImportDataForm.TextGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if(MouseInDown)and(Not(ActiveTableOptions.TextImportWithSep))then
  begin
    DataLbl.Caption:='Data ('+IntToStr(TextGrid.Selection.Right-TextGrid.Selection.Left+1)+' Characters selected)';
  end;
end;

procedure TDBImportDataForm.TextGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
  s: string;
begin
  MouseInDown:=False;

  if(Button=mbRight)and(TextWithFixedLengthRBtn.Checked)then
    if(ActiveTableOptions<>nil)then
    begin
      //Delete Columns in between
      for i:=TextGrid.Selection.Left+1 to TextGrid.Selection.Right do
        if(ActiveTableOptions.TextColumns.IndexOf(IntToStr(i+1))<>-1)then
          ActiveTableOptions.TextColumns.Delete(ActiveTableOptions.TextColumns.IndexOf(IntToStr(i+1)));

      if(TextGrid.Selection.Left>0)and
        (ActiveTableOptions.TextColumns.IndexOf(IntToStr(TextGrid.Selection.Left+1))=-1)then
        ActiveTableOptions.TextColumns.Add(IntToStr(TextGrid.Selection.Left+1));

      if(ActiveTableOptions.TextColumns.IndexOf(IntToStr(TextGrid.Selection.Right+1+1))=-1)then
        ActiveTableOptions.TextColumns.Add(IntToStr(TextGrid.Selection.Right+1+1));

      s:='';
      ActiveTableOptions.TextColumns.Sort;
      for i:=0 to ActiveTableOptions.TextColumns.Count-1 do
      begin
        s:=s+ActiveTableOptions.TextColumns[i];
        if(i<ActiveTableOptions.TextColumns.Count-1)then
          s:=s+', ';
      end;

      ColumnsEd.Text:=s;

      ShowTableOptions(ActiveTableOptions.Tableindex);
    end;

  DisplayColName(TextGrid.Selection.Left+1);


  TextGrid.Refresh;

  DataLbl.Caption:='Data';
end;

procedure TDBImportDataForm.TextGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseInDown:=True;
  TextGrid.Refresh;
end;

procedure TDBImportDataForm.SetColsSBtnClick(Sender: TObject);
var i: integer;
  sep: string;
begin
  if(ActiveTableOptions<>nil)then
  begin

    if(DMMain.GetColumnCountFromSepString(ColumnsEd.Text, ',', '')>0)then
      sep:=','
    else if(DMMain.GetColumnCountFromSepString(ColumnsEd.Text, ';', '')>0)then
      sep:=';'
    else
      sep:=' ';

    ActiveTableOptions.TextColumns.Clear;
    for i:=0 to DMMain.GetColumnCountFromSepString(ColumnsEd.Text, sep, '')-1 do
      ActiveTableOptions.TextColumns.Add(
        Trim(DMMain.GetColumnFromSepString(ColumnsEd.Text, i, sep, '')));

    //ActiveTableOptions.TextColumns.CustomSort(@SortStringListWithInts);

    //TextGrid.Refresh;

    ShowTableOptions(ActiveTableOptions.Tableindex);
  end;
end;

procedure TDBImportDataForm.TextGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  TextGrid.Refresh;
  DisplayColName(TextGrid.Selection.Left+1);
end;

procedure TDBImportDataForm.NewPresetBtnClick(Sender: TObject);
var s: string;
begin
  if(ActiveTableOptions<>nil)then
  begin
    s:=ActiveTableOptions.PresetName;
    if(DMMain.ShowStringEditor('New Preset', 'Name: ', s, 0, 1))then
      if(s<>'')then
        SavePresetToIniFile(s);
  end;
end;

procedure TDBImportDataForm.GetPresetList;
var theIni: TIniFile;
  thePresets: TStringList;
begin
  theIni:=TIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_ImportSettings.ini');
  thePresets:=TStringList.Create;
  try
    //----------------------------------------------------
    // for TextOptions
    theIni.ReadSections(thePresets);

    {i:=0;
    while(i<=thePresets.Count-1)do
    begin
      if(Copy(thePresets[i], 1, Length('TextOptions_'))<>'TextOptions_')then
        thePresets.Delete(i)
      else
      begin
        thePresets[i]:=Copy(thePresets[i], Length('TextOptions_')+1, Length(thePresets[i]));
        inc(i);
      end;
    end;}

    PresetLU.Items.Assign(thePresets);
    PresetLU.Items.Insert(0, 'Select a preset to apply the settings');
    PresetLU.ItemIndex:=0;
  finally
    thePresets.Free;
    theIni.Free;
  end;
end;

procedure TDBImportDataForm.SavePresetToIniFile(name: string);
var theIni: TIniFile;
  s: string;
  i: integer;
begin
  if(ActiveTableOptions<>nil)then
  begin
    theIni:=TIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_ImportSettings.ini');
    try
      //--------------------------------------------------------
      //TextOptions

      theIni.WriteString(name, 'TextImportWithSep',
        IntToStr(Ord(ActiveTableOptions.TextImportWithSep)));

      theIni.WriteString(name, 'TextImportFirstRowHoldsColumnNames',
        IntToStr(Ord(ActiveTableOptions.TextImportFirstRowHoldsColumnNames)));

      theIni.WriteString(name, 'TextImportTextDelim',
        ActiveTableOptions.TextImportTextDelim);
      theIni.WriteString(name, 'TextImportTextSep',
        ActiveTableOptions.TextImportTextSep);

      s:='';
      ActiveTableOptions.TextColumns.Sort;
      for i:=0 to ActiveTableOptions.TextColumns.Count-1 do
      begin
        s:=s+ActiveTableOptions.TextColumns[i];
        if(i<ActiveTableOptions.TextColumns.Count-1)then
          s:=s+', ';
      end;

      theIni.WriteString(name, 'TextColumns', s);

      s:='';
      for i:=0 to ActiveTableOptions.ColumnNames.Count-1 do
      begin
        s:=s+ActiveTableOptions.ColumnNames[i];
        if(i<ActiveTableOptions.ColumnNames.Count-1)then
          s:=s+', ';
      end;

      theIni.WriteString(name, 'ColumnNames', s);

      //--------------------------------------------------------
      //Spaltenmapping

      if(DestTblLU.ItemIndex>-1)then
      begin
        theIni.WriteString(name, 'Tablename',
          DestTblLU.Items[DestTblLU.ItemIndex]);

        for i:=0 to ActiveTableOptions.DestColumns.Count-1 do
        begin
          theIni.WriteString(name, ActiveTableOptions.DestColumns[i],
            ActiveTableOptions.DestColumnsValues[i]);
        end;
      end;

      //--------------------------------------------------------
      //Allgemeine Opt.

      theIni.WriteString(name, 'DelDestTableBeforInserts',
        IntToStr(Ord(ActiveTableOptions.DelDestTableBeforInserts)));
      theIni.WriteString(name, 'TrimColumns',
        IntToStr(Ord(ActiveTableOptions.TrimColumns)));
      theIni.WriteString(name, 'CheckReturnsInTxt',
        IntToStr(Ord(ActiveTableOptions.CheckReturnsInTxt)));


      for i:=0 to ActiveTableOptions.ReplaceChars.Count-1 do
        theIni.WriteString(name, 'ReplaceChrs'+IntToStr(i+1),
          ActiveTableOptions.ReplaceChars[i]);

    finally
      theIni.Free;
    end;

    GetPresetList;

    PresetLU.ItemIndex:=PresetLU.Items.IndexOf(name);
  end;
end;

procedure TDBImportDataForm.GetPresetsFromIniFile(name: string);
var theIni: TIniFile;
  Keys: TStringList;
  s: string;
  i: integer;
begin
  if(ActiveTableOptions<>nil)then
  begin
    theIni:=TIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_ImportSettings.ini');
    Keys:=TStringList.Create;
    try
      //--------------------------------------------------------
      //TextOptions
      ActiveTableOptions.PresetName:=name;

      ActiveTableOptions.TextImportWithSep:=(theIni.ReadString(name, 'TextImportWithSep', '1')='1');

      ActiveTableOptions.TextImportFirstRowHoldsColumnNames:=
        (theIni.ReadString(name, 'TextImportFirstRowHoldsColumnNames',
        '0')='1');

      ActiveTableOptions.TextImportTextDelim:=
        theIni.ReadString(name, 'TextImportTextDelim', ';');

      ActiveTableOptions.TextImportTextSep:=
        theIni.ReadString(name, 'TextImportTextSep', '"');

      ColumnsEd.Text:=theIni.ReadString(name, 'TextColumns', '');
      SetColsSBtnClick(self);

      s:=theIni.ReadString(name, 'ColumnNames', '');
      ActiveTableOptions.ColumnNames.Clear;
      for i:=0 to DMMain.GetColumnCountFromSepString(s, ',', '')-1 do
        ActiveTableOptions.ColumnNames.Add(
          Trim(DMMain.GetColumnFromSepString(s, i, ',', '')));

      //--------------------------------------------------------
      //Spaltenmapping

      s:=theIni.ReadString(name, 'Tablename', 'xxx');

      if(DestTblLU.Items.IndexOf(s)=-1)then
        EInOutError.Create('The table '+s+' cannot be found '+
          'in the selected database. Please choose another database connection.');

      ActiveTableOptions.DelDestTableBeforInserts:=
        (theIni.ReadString(name, 'DelDestTableBeforInserts', '1')='1');

      ActiveTableOptions.TrimColumns:=
        (theIni.ReadString(name, 'TrimColumns', '1')='1');

      DestTblLU.ItemIndex:=DestTblLU.Items.IndexOf(s);
      DestTblLUCloseUp(self);

      theIni.ReadSection(name, Keys);

      for i:=0 to Keys.Count-1 do
        if(Keys[i]<>'TextImportWithSep')and
          (Keys[i]<>'TextImportFirstRowHoldsColumnNames')and
          (Keys[i]<>'TextImportTextDelim')and
          (Keys[i]<>'TextImportTextSep')and
          (Keys[i]<>'TextColumns')and
          (Keys[i]<>'ColumnNames')and
          (Keys[i]<>'Tablename')and
          (Keys[i]<>'DelDestTableBeforInserts')and
          (Keys[i]<>'TrimColumns')and
          (Copy(Keys[i], 1, Length('ReplaceChrs'))<>'ReplaceChrs')then
        begin
          if(ActiveTableOptions.DestColumns.IndexOf(Keys[i])<>-1)then
          begin
            //Wenn diese Column nicht in den DefMappings
            if(DefaultMapping.IndexOfName(Keys[i])=-1)then
              ActiveTableOptions.DestColumnsValues[ActiveTableOptions.DestColumns.IndexOf(Keys[i])]:=
                theIni.ReadString(name, Keys[i], '');
          end;
        end;

      DestDG.Refresh;

      //--------------------------------------------------------
      //Allgemeine Opt.

      DelDestTblCBox.Checked:=ActiveTableOptions.DelDestTableBeforInserts;
      TrimColumnsCBox.Checked:=ActiveTableOptions.TrimColumns;
      CheckReturnsInTxtCBox.Checked:=ActiveTableOptions.CheckReturnsInTxt;

      i:=1;
      ActiveTableOptions.ReplaceChars.Clear;
      s:=theIni.ReadString(name, 'ReplaceChrs'+IntToStr(i), '');
      while(s<>'')do
      begin
        ActiveTableOptions.ReplaceChars.Add(s);

        inc(i);
        s:=theIni.ReadString(name, 'ReplaceChrs'+IntToStr(i), '');
      end;


      ShowTableOptions(ActiveTableOptions.Tableindex);
    finally
      Keys.Free;
      theIni.Free;
    end;
  end;
end;

procedure TDBImportDataForm.PresetLUCloseUp(Sender: TObject);
begin
  if(ActiveTableOptions<>nil)and(PresetLU.ItemIndex>0)then
    GetPresetsFromIniFile(PresetLU.Text);

  PresetLU.ItemIndex:=0;
end;

procedure TDBImportDataForm.SourceDGDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if(ActiveTableOptions<>nil)then
  begin
    with SourceDG.Canvas do
    begin
      if(gdSelected in State)then
      begin
        Brush.Color:=clNavy;
        Font.Color:=clWhite;
      end
      else
      begin
        Brush.Color:=clWhite;
        Font.Color:=clBlack;
      end;

      Brush.Style:=bsSolid;
      FillRect(Rect);

      Pen.Color:=clSilver;
      MoveTo(Rect.Left, Rect.Bottom-1);
      LineTo(Rect.Right, Rect.Bottom-1);

      if(ARow<ActiveTableOptions.ColumnNames.Count)then
        TextOut(Rect.Left+2, Rect.Top+1, ActiveTableOptions.ColumnNames[ARow]);
    end;
  end;
end;

procedure TDBImportDataForm.DestTblLUCloseUp(Sender: TObject);
begin
  if(ActiveTableOptions<>nil)then
  begin
    ActiveTableOptions.DestColumns.Clear;
    ActiveTableOptions.DestColumnsValues.Clear;
    ActiveTableOptions.DestTableName:=DestTblLU.Text;
    if(ActiveTableOptions.DestTableName<>'')then
    begin
      DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema	, '', '');
      DMDB.SchemaSQLQuery.SQL.Text:='describe '+ActiveTableOptions.DestTableName;
      DMDB.SchemaSQLQuery.Open;
      while(not(DMDB.SchemaSQLQuery.EOF))do
      begin
        ActiveTableOptions.DestColumns.Add(DMDB.SchemaSQLQuery.Fields[0].AsString);

        //if this field is in the defaultmapping list
        if(DefaultMapping.IndexOfName(DMDB.SchemaSQLQuery.Fields[0].AsString)<>-1)then
          ActiveTableOptions.DestColumnsValues.Add(DefaultMapping.Values[DefaultMapping.Names[DefaultMapping.IndexOfName(DMDB.SchemaSQLQuery.Fields[0].AsString)]])
        else
          ActiveTableOptions.DestColumnsValues.Add('');
        DMDB.SchemaSQLQuery.Next;
      end;
      DMDB.SchemaSQLQuery.Close;
    end;

    DestDG.RowCount:=ActiveTableOptions.DestColumns.Count;

    DestDG.Refresh;
  end;
end;

procedure TDBImportDataForm.DestDGDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  with DestDG.Canvas do
  begin
    if(gdSelected in State)then
    begin
      Brush.Color:=clNavy;
      Font.Color:=clWhite;
    end
    else
    begin
      Brush.Color:=clWhite;
      Font.Color:=clBlack;
    end;

    Brush.Style:=bsSolid;
    FillRect(Rect);

    Pen.Color:=clSilver;
    MoveTo(Rect.Left, Rect.Bottom-1);
    LineTo(Rect.Right-1, Rect.Bottom-1);
    LineTo(Rect.Right-1, Rect.Top);

    if(ActiveTableOptions<>nil)then
    begin
      if(ACol=0)then
      begin
        if(ARow<ActiveTableOptions.DestColumns.Count)then
          TextOut(Rect.Left+2, Rect.Top+2, ActiveTableOptions.DestColumns[ARow]);
      end
      else
        if(ARow<ActiveTableOptions.DestColumnsValues.Count)then
          TextOut(Rect.Left+2, Rect.Top+2, ActiveTableOptions.DestColumnsValues[ARow]);
    end;
  end;
end;

procedure TDBImportDataForm.SourceDGMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SourceColumn:=SourceDG.Selection.Top;
  SourceDG.BeginDrag(False, 5);
end;

procedure TDBImportDataForm.DestDGDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=False;
  if(ActiveTableOptions<>nil)and(Source<>nil)then
  begin
    if(Source.ClassNameIs('TDrawGrid'))then
      if(TDrawGrid(Source).Name='SourceDG')then
        Accept:=True;

    if(Source.ClassNameIs('TListBox'))then
      if(TListBox(Source).Name='SpecialFieldsLBox')then
        Accept:=True;
  end;
end;

procedure TDBImportDataForm.DestDGDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var ACol, ARow: Longint;
  s: string;
begin
  if(ActiveTableOptions=nil)or(Source=nil)then
    Exit;

  if(Source.ClassNameIs('TDrawGrid'))then
    if(TDrawGrid(Source).Name='SourceDG')then
    begin
      DestDG.MouseToCell(X, Y, ACol, ARow);

      if(ARow<ActiveTableOptions.DestColumnsValues.Count)then
      begin
        if(ActiveTableOptions.DestColumnsValues[ARow]<>'')then
          ActiveTableOptions.DestColumnsValues[ARow]:=ActiveTableOptions.DestColumnsValues[ARow]+'+'+
            '['+ActiveTableOptions.ColumnNames[SourceColumn]+']'
        else
          ActiveTableOptions.DestColumnsValues[ARow]:=ActiveTableOptions.DestColumnsValues[ARow]+
            '['+ActiveTableOptions.ColumnNames[SourceColumn]+']';
      end;

      DestDG.Refresh;
    end;

  if(Source.ClassNameIs('TListBox'))then
    if(TListBox(Source).Name='SpecialFieldsLBox')then
    begin
      DestDG.MouseToCell(X, Y, ACol, ARow);

      if(ARow<ActiveTableOptions.DestColumnsValues.Count)then
      begin
        if(SpecialFieldsLBox.ItemIndex>0)then
          s:=SpecialFieldsLBox.Items[SpecialFieldsLBox.ItemIndex]
        else
          s:=' ';

        if(s='SubString')then
        begin
          s:='SubString(From, Length)';
          if(DMMain.ShowStringEditor('SubString(From, Length)', 'Function: ', s, 10, 0))then
          begin
            if(s='')then
              Exit;

            if(Copy(s, Length(s), 1)<>':')then
              s:=s+':';
          end
          else
            Exit;
        end;

        if(ActiveTableOptions.DestColumnsValues[ARow]<>'')then
          ActiveTableOptions.DestColumnsValues[ARow]:=ActiveTableOptions.DestColumnsValues[ARow]+'+'+'{'+s+'}'
        else
          ActiveTableOptions.DestColumnsValues[ARow]:=ActiveTableOptions.DestColumnsValues[ARow]+'{'+s+'}';
      end;

      DestDG.Refresh;
    end;
end;

procedure TDBImportDataForm.DestDGKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_Delete)and(ActiveTableOptions<>nil)then
    if(DestDG.Selection.Top<ActiveTableOptions.DestColumnsValues.Count)then
    begin
      ActiveTableOptions.DestColumnsValues[DestDG.Selection.Top]:='';

      DestDG.Refresh;
    end;
end;

procedure TDBImportDataForm.SpecialFieldsLBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SourceColumn:=SpecialFieldsLBox.ItemIndex*-1;
  SpecialFieldsLBox.BeginDrag(False, 5);
end;

procedure TDBImportDataForm.ImportBtnClick(Sender: TObject);
var theFile: TextFile;
  theTblOpt: PTableImportOptions;
  i, j, k, l, n, id, global_id, colnr, gesanz: integer;
  s, s1, colname, SQLStr, v, m: string;
  MakeSubstring: Boolean;
  SubstringStart, SubstringLength: integer;
  TablesDeleted: TStringList;
begin
  SubstringStart:=0;
  SubstringLength:=0;
  gesanz:=0;

  ProgressForm:=TProgressForm.Create(self);
  TablesDeleted:=TStringList.Create;
  try
    ProgressForm.Show;

    global_id:=0;
    for i:=0 to TblOptList.Count-1 do
    begin
      id:=0;
      theTblOpt:=TblOptList[i];

      if(theTblOpt.DelDestTableBeforInserts)and
        (TablesDeleted.IndexOf(theTblOpt.DestTableName)=-1)then
      begin
        DMDB.ExecSQL('delete from '+theTblOpt.DestTableName);
        TablesDeleted.Add(theTblOpt.DestTableName);
      end;

      //Get TestLines
      AssignFile(theFile, theTblOpt.TableDirectory+theTblOpt.Tablename);
      Reset(theFile);
      try
        ProgressForm.SetFile(theTblOpt.TableDirectory+theTblOpt.Tablename);
        ProgressForm.SetCurrentFile(i+1, TblOptList.Count);

        while(1=1)do
        begin
          SQLStr:='INSERT INTO '+theTblOpt.DestTableName+'(';

          for j:=0 to theTblOpt.DestColumns.Count-1 do
          begin
            SQLStr:=SQLStr+theTblOpt.DestColumns[j];
            if(j<theTblOpt.DestColumns.Count-1)then
              SQLStr:=SQLStr+', ';
          end;

          SQLStr:=SQLStr+')'+#13#10+'VALUES';

          l:=0;
          while(Not(EOF(theFile)))and(l<500)do
          begin
            ReadLn(theFile, s);
            if(id=0)and(theTblOpt.TextImportFirstRowHoldsColumnNames)then
              ReadLn(theFile, s);

            if(Trim(s)='')then
              continue;

            inc(l);
            inc(id);
            inc(global_id);

            SQLStr:=SQLStr+'(';

            //Get the Values
            for j:=0 to theTblOpt.DestColumnsValues.Count-1 do
            begin
              v:='';
              MakeSubstring:=False;
              
              //Get Mapping
              for k:=0 to DMMain.GetColumnCountFromSepString(theTblOpt.DestColumnsValues[j], '+', '')-1 do
              begin
                m:=DMMain.GetColumnFromSepString(theTblOpt.DestColumnsValues[j], k, '+', '');



                //Fields
                if(Copy(m, 1, 1)='[')then
                begin
                  colnr:=theTblOpt.ColumnNames.IndexOf(Copy(m, 2, Length(m)-2));
                  if(theTblOpt.TextImportWithSep)then
                  begin
                    //if the data is continued next line read it
                    //(when not enougth seps or number of delims is odd)
                    while((DMMain.GetColumnCountFromSepString(s, theTblOpt.TextImportTextSep, theTblOpt.TextImportTextDelim)<theTblOpt.ColumnNames.Count)or
                      (DMMain.GetSubStringCountInString(s, theTblOpt.TextImportTextDelim) mod 2<>0))and(Not(EOF(theFile)))and(theTblOpt.CheckReturnsInTxt)do
                    begin
                      ReadLn(theFile, s1);

                      s:=s+#13#10+s1;
                    end;

                    //Seperated Columns
                    colname:=DMMain.GetColumnFromSepString(s, colnr, theTblOpt.TextImportTextSep, theTblOpt.TextImportTextDelim)
                  end
                  else
                    //Fixed Length columns
                    colname:=DMMain.GetColumnFromFixLengthString(s, colnr, theTblOpt.TextColumns);

                  for n:=0 to theTblOpt.ReplaceChars.Count-1 do
                    colname:=DMMain.ReplaceString(colname, theTblOpt.ReplaceChars.Names[n],
                      theTblOpt.ReplaceChars.Values[theTblOpt.ReplaceChars.Names[n]]);


                  if(MakeSubstring)then
                  begin
                    MakeSubstring:=False;
                    colname:=Copy(colname, SubstringStart, SubstringLength);
                  end;

                  v:=v+colname;
                end
                else if(Copy(m, 1, 1)='{')then
                begin
                  //Special
                  if(m='{AutoInc.}')then
                    v:=v+IntToStr(id);
                  if(m='{NULL}')then
                    v:=v+'NULL';
                  if(m='{ }')then
                    v:=v+' ';
                  if(m='{,}')then
                    v:=v+',';
                  MakeSubstring:=False;
                  if(Copy(m, 1, Length('{SubString('))='{SubString(')then
                  begin
                    MakeSubstring:=True;
                    SubstringStart:=StrToInt(Copy(m, Length('{SubString(')+1, Pos(',', m)-Length('{SubString(')-1));
                    SubstringLength:=StrToInt(Trim(Copy(m, Pos(',', m)+1, Pos(')', m)-Pos(',', m)-1)));
                  end;
                end
                else
                  v:=v+m;

              end;

              if(theTblOpt.TrimColumns)then
                v:=Trim(v);

              SQLStr:=SQLStr+DMMain.FormatText4SQL(v);
              if(j<theTblOpt.DestColumnsValues.Count-1)then
                SQLStr:=SQLStr+', ';
            end;

            SQLStr:=SQLStr+'),'+#13#10;
          end;

          //Remove last ,
          SQLStr:=Copy(SQLStr, 1, Length(SQLStr)-3);

          DMDB.ExecSQL(SQLStr);

          ProgressForm.SetCurrentLines(id);
          ProgressForm.SetTotalLines(global_id);

          if(EOF(theFile))then
            break;
        end;
      finally
        CloseFile(theFile);
      end;

      gesanz:=gesanz+id;
    end;
  finally
    TablesDeleted.Free;
    ProgressForm.Free;
  end;

  ShowMessage('Data import finished.'+#13#10+
    IntToStr(gesanz)+' Lines of Data imported.');

  //ModalResult:=mrOK;
end;

procedure TDBImportDataForm.DisplayColName(Row: integer);
var i, CurrentCol: integer;
begin
  if(TextWithFixedLengthRBtn.Checked)then
  begin
    //Get Current Col
    CurrentCol:=0;
    for i:=0 to ActiveTableOptions.TextColumns.Count-1 do
      if(Row<StrToInt(ActiveTableOptions.TextColumns[i]))then
      begin
        CurrentCol:=i-1;
        break;
      end;

    if(CurrentCol>=0)and
      (CurrentCol<ActiveTableOptions.ColumnNames.Count)then
      ColNameEd.Text:=ActiveTableOptions.ColumnNames[CurrentCol];
  end
  else
    if(Row<=ActiveTableOptions.ColumnNames.Count)then
      ColNameEd.Text:=ActiveTableOptions.ColumnNames[Row-1];
end;


procedure TDBImportDataForm.ColNameEdChange(Sender: TObject);
var i, CurrentCol: integer;
begin
  if(TextWithFixedLengthRBtn.Checked)then
  begin
    //Get Current Col
    CurrentCol:=0;
    for i:=0 to ActiveTableOptions.TextColumns.Count-1 do
      if(TextGrid.Selection.Left+1<StrToInt(ActiveTableOptions.TextColumns[i]))then
      begin
        CurrentCol:=i-1;
        break;
      end;
  end
  else
    CurrentCol:=TextGrid.Selection.Left;


  if(CurrentCol<ActiveTableOptions.ColumnNames.Count)then
    ActiveTableOptions.ColumnNames[CurrentCol]:=ColNameEd.Text;
end;

procedure TDBImportDataForm.AutoMappingBtnClick(Sender: TObject);
var i, j: integer;
begin
  if(ActiveTableOptions<>nil)then
  begin
    for i:=0 to ActiveTableOptions.DestColumnsValues.Count-1 do
      for j:=0 to ActiveTableOptions.ColumnNames.Count-1 do
        if(CompareText(ActiveTableOptions.ColumnNames[j], ActiveTableOptions.DestColumns[i])=0)then
        begin
          ActiveTableOptions.DestColumnsValues[i]:='['+ActiveTableOptions.ColumnNames[j]+']';
          break;
        end;
  end;

  DestDG.Refresh;
end;

procedure TDBImportDataForm.ReplaceChrGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: WideString);
var i: integer;
begin
  //Workaround, so Clear of Table-cells doesn't call CellEdit
  if(ReplaceChrGrid.Tag=1)then
    Exit;

  if(ActiveTableOptions<>nil)then
  begin
    i:=1;
    ActiveTableOptions.ReplaceChars.Clear;
    while(ReplaceChrGrid.Cells[0, i]<>'')do
    begin
      ActiveTableOptions.ReplaceChars.Add(
        ReplaceChrGrid.Cells[0, i]+'='+ReplaceChrGrid.Cells[1, i]);

      inc(i);
    end;
  end;

end;

procedure TDBImportDataForm.ModePageControlChange(Sender: TObject);
begin
  if(ModePageControl.ActivePage=DBImportSheet)then
    TextOptionsSheet.TabVisible:=False
  else
    TextOptionsSheet.TabVisible:=True;
end;

procedure TDBImportDataForm.GetDBConnSBtnClick(Sender: TObject);
var SelDBConnName: string;
begin
  SelDBConnName:=DMMain.LoadValueFromSettingsIniFile('RecentDBConns', 'RecentDestinationDBConn', '');

  DBConnEd.Text:='';
  DMDB.DisconnectFromDB;

  StatusLbl.Caption:='Not connected to a Database';
  ConnectionSBtn.Enabled:=False;
  SubmitBtn.Enabled:=False;

  //do until a successful connection is established or the user selects abort
  while(1=1)do
  begin
    //Let the User choose connection
    DestDBConn:=DMDB.GetUserSelectedDBConn(SelDBConnName);
    if(DestDBConn<>nil)then
    begin
      SelDBConnName:=DestDBConn.Name;

      //Try to connect to the DB
      try
        DMDB.ConnectToDB(DestDBConn);
      except
        on x: Exception do
        begin
          MessageDlg('Connection to database failed.'+#13#10#13#10+
            x.Message, mtError, [mbOK], 0);

          continue;
        end;
      end;

      DMMain.SaveValueInSettingsIniFile('RecentDBConns', 'RecentDestinationDBConn', SelDBConnName);

      SetDestDBConn(DestDBConn);

      break;
    end
    else
      break;
  end;
end;

procedure TDBImportDataForm.SetDestDBConn(theDBConn: Pointer);
var theTables: TStringList;
begin
  if(theDBConn<>nil)then
  begin
    DestDBConn:=theDBConn;

    ConnectionSBtn.Enabled:=True;
    SubmitBtn.Enabled:=True;
    DBConnEd.Text:=DMDB.CurrentDBConn.Name;
    StatusLbl.Caption:='Connected to Database '+
      DMDB.CurrentDBConn.Params.Values['User_Name']+'@'+
      DMDB.CurrentDBConn.Params.Values['Database'];

    theTables:=TStringList.Create;
    try
      DMDB.GetDBTables(theTables);

      //Ignore DBDesigner4 table
      if(theTables.IndexOf('DBDesigner4')<>-1)then
        theTables.Delete(theTables.IndexOf('DBDesigner4'));

      DestTblLU.Items.Assign(theTables);
    finally
      theTables.Free;
    end;
  end;
end;

procedure TDBImportDataForm.FirstRowHoldsColNameCBoxClick(Sender: TObject);
var i: integer;
begin
  if(ActiveTableOptions<>nil)then
  begin
    if(FirstRowHoldsColNameCBox.Checked=False)then
    begin
      for i:=0 to ActiveTableOptions.ColumnNames.Count-1 do
        ActiveTableOptions.ColumnNames[i]:='Field '+IntToStr(i+1);
    end;

    SetTableOptions(Sender);
  end;
end;

procedure TDBImportDataForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult:=mrAbort;
end;

end.
