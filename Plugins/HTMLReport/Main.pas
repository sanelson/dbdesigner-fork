unit Main;

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
// Unit Main.pas
// -------------
// Version 1.0, 12.01.2003, Mike
// Description
//   MainForm of the HTML Report Plugin
//
// Changes:
//   Version 1.0, 12.01.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------


interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, Qt, QExtCtrls, QCheckLst, QButtons, QMenus,
  EERModel; // The EERModel module has to be added to the uses clause

type
  TMainForm = class(TForm)
    MenuBevel: TBevel;
    MainPnl: TPanel;
    SubmitBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    TablesLbl: TLabel;
    TablesCheckListBox: TCheckListBox;
    OptionsGroupBox: TGroupBox;
    ListRegionsCheckBox: TCheckBox;
    ListIndicesCheckBox: TCheckBox;
    TableOptionsCheckBox: TCheckBox;
    RegionsLbl: TLabel;
    RegionsComboBox: TComboBox;
    LayoutGroupBox: TGroupBox;
    LayoutComboBox: TComboBox;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    ExitMI: TMenuItem;
    DisplayMI: TMenuItem;
    ShowControlsMI: TMenuItem;
    ShowModelMI: TMenuItem;
    Help1: TMenuItem;
    AboutMI: TMenuItem;
    TablesPopupMenu: TPopupMenu;
    SelectAllMI: TMenuItem;
    DeselectAllMI: TMenuItem;
    SortOrderPosRBtn: TRadioButton;
    SortAlphaRBtn: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    //Initialize this form's controls with data from the loaded model
    procedure InitControls;
    procedure GetLayouts;

    procedure RegionsComboBoxCloseUp(Sender: TObject);

    procedure CancelBtnClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);

    //Show About Box
    procedure AboutMIClick(Sender: TObject);

    //Show or Hide the Controls/the EER Model
    procedure ShowControlsMIClick(Sender: TObject);

    procedure TablesCheckListBoxClickCheck(Sender: TObject);
    procedure SelectAllMIClick(Sender: TObject);
    procedure DeselectAllMIClick(Sender: TObject);

    procedure StorePluginDataInModel;

    function BuildHTMLFile(layout: string): string;
    function BuildHTMLTable(theTable: TEERTable; layout: string): string;
    procedure SortOrderPosRBtnClick(Sender: TObject);
  private
    { Private declarations }
    SelectedTables: string;
    lastFileName: string;

    ModelFileName: string;
  public
    { Public declarations }

    //Public declaration of the EERModel
    EERModel: TEERModel;
  end;

var
  MainForm: TMainForm;

implementation

//Include the MainDM and EERDM modules
uses MainDM, EERDM;

{$R *.xfm}

procedure TMainForm.FormCreate(Sender: TObject);
var i: Integer;
begin
  //Create Main DataModule, containing general functions
  DMMain:=TDMMain.Create(self);
  //Create EER DateModule, containing additional functions for the EERModel
  DMEER:=TDMEER.Create(self);

  //Initialise the Forms Font (For Kylix)
  DMMain.InitForm(self);

  EERModel:=nil;

  //if a parameter is specified, it is assumed to be a filename
  for i := 1 to ParamCount do
  begin
    //Check if file exists
    if(FileExists(ParamStr(i)))then
    begin
      //Create EERModel
      EERModel:=TEERModel.Create(self);
      //Set the Model invisible at first, so the form's controls are visible
      EERModel.Visible:=False;

      //Load model from file, read settings and do not append to current model
      ModelFileName:=ParamStr(i);
      EERModel.LoadFromFile(ModelFileName, True, False, False, False);
      //Set the current worktool to Pointer
      DMEER.SetCurrentWorkTool(wtPointer);

      //Initialize this form's controls with data from the model
      InitControls;

      //Only load first file
      break;
    end;
  end;

  lastFileName:='';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //Store changes to model
  StorePluginDataInModel;

  //Free the EERModel
  EERModel.Free;
end;

procedure TMainForm.InitControls;
var thePluginData: TEERPluginData;
  theTables: TList;
  theTableNames: TStringList;
  theRegionNames: TStringList;
  i: integer;
begin
  thePluginData:=EERModel.GetPluginDataByID('HTMLReport', -1);
  if(thePluginData=nil)then
  begin
    //Get selected tables
    theTables:=TList.Create;
    try
      EERModel.GetEERObjectList([EERTable], theTables);

      SelectedTables:='';
      for i:=0 to theTables.Count-1 do
        SelectedTables:=SelectedTables+TEERTable(theTables[i]).ObjName+';';
    finally
      theTables.Free;
    end;
  end
  else
  begin
    SelectedTables:=thePluginData.Params.Values['SelectedTables'];
  end;

  //If there are Regions
  if(EERModel.GetEERObjectCount([EERRegion])>0)then
  begin
    //Create temporary Stringlist
    theRegionNames:=TStringList.Create;
    try
      //Call EERModel function which will list all regions
      EERModel.GetEERObjectNameList([EERRegion], theRegionNames);
      theRegionNames.Sorted:=True;

      //Assigned the returned Stringlist to the Listbox
      RegionsComboBox.Items.Assign(theRegionNames);
    finally
      //Free the temporary Stringlist
      theRegionNames.Free;
    end;

    RegionsComboBox.ItemIndex:=0;
    RegionsComboBoxCloseUp(self);
  end
  else
  begin
    //if there are no regions, hide Combobox
    RegionsLbl.Visible:=False;
    TablesLbl.Top:=RegionsLbl.Top;
    RegionsComboBox.Visible:=False;
    TablesCheckListBox.Height:=TablesCheckListBox.Height+
      TablesCheckListBox.Top-RegionsComboBox.Top;
    TablesCheckListBox.Top:=RegionsComboBox.Top;
    ListRegionsCheckBox.Enabled:=False;
    ListRegionsCheckBox.Checked:=False;

    //Create temporary Stringlist
    theTableNames:=TStringList.Create;
    try
      //Call EERModel function which will list all tables
      EERModel.GetEERObjectNameList([EERTable], theTableNames);

      //Assigned the returned Stringlist to the Listbox
      TablesCheckListBox.Items.Assign(theTableNames);
    finally
      //Free the temporary Stringlist
      theTableNames.Free;
    end;

    //Select all items in the Listbox which are in the SelectedTables string
    for i:=0 to TablesCheckListBox.Items.Count-1 do
      if(Pos(TablesCheckListBox.Items[i]+';', SelectedTables)>0)then
        TablesCheckListBox.Checked[i]:=True;

    TablesCheckListBox.ItemIndex:=0;
  end;

  //Get Layouts
  GetLayouts;
end;

procedure TMainForm.RegionsComboBoxCloseUp(Sender: TObject);
var theTables: TList;
  theRegion: TEERRegion;
  i: integer;
begin
  theTables:=TList.Create;
  try
    theRegion:=EERModel.GetEERObjectByName(EERRegion, RegionsComboBox.Items[RegionsComboBox.ItemIndex]);
    if(theRegion<>nil)then
    begin
      theRegion.GetEERObjsInRegion([EERTable], theTables);

      if(SortOrderPosRBtn.Checked)then
        EERModel.SortEERObjectListByOrderPos(theTables)
      else
        EERModel.SortEERObjectListByObjName(theTables);


      TablesCheckListBox.Clear;
      for i:=0 to theTables.Count-1 do
      begin
        TablesCheckListBox.Items.Add(TEERTable(theTables[i]).ObjName);
        if(Pos(TablesCheckListBox.Items[i]+';', SelectedTables)>0)then
          TablesCheckListBox.Checked[i]:=True;
      end;
    end;

  finally
    theTables.Free;
  end;
end;

procedure TMainForm.TablesCheckListBoxClickCheck(Sender: TObject);
begin
  if(TablesCheckListBox.Checked[TablesCheckListBox.ItemIndex])then
  begin
    //Add to SelectedTables if the table isn't already in the string
    if(Pos(TablesCheckListBox.Items[TablesCheckListBox.ItemIndex]+';', SelectedTables)=0)then
      SelectedTables:=SelectedTables+TablesCheckListBox.Items[TablesCheckListBox.ItemIndex]+';';
  end
  else
  begin
    //Delete from SelectedTables if the table is in the string
    if(Pos(TablesCheckListBox.Items[TablesCheckListBox.ItemIndex]+';', SelectedTables)>0)then
      SelectedTables:=DMMain.ReplaceText(SelectedTables,
        TablesCheckListBox.Items[TablesCheckListBox.ItemIndex]+';', '');
  end;
end;

procedure TMainForm.AboutMIClick(Sender: TObject);
begin
  ShowMessage('DBDesigner 4 HTML Report Plugin'#13#10+
    'Version 1.0.0.7'#13#10#13#10+
    'The HTML Report Plugin generates a HTML Report of the '#13#10+
    'EER Model. Different templates and options can be selected. ');
end;

procedure TMainForm.ShowControlsMIClick(Sender: TObject);
begin
  //This is called from ShowControlsMI and ShowModelMI
  if(EERModel=nil)then
    Exit;

  //Check the selected MenuItem, the other MenuItem will be unchecked automatically
  TMenuItem(Sender).Checked:=True;

  //Hide Model
  if(Sender=ShowControlsMI)then
  begin
    //AutoScroll:=False;
    EERModel.Visible:=False;
  end
  else
  //Show the Model
  begin
    //AutoScroll:=True;
    EERModel.Visible:=True;
    EERModel.BringToFront;
  end;
end;

procedure TMainForm.SelectAllMIClick(Sender: TObject);
var i: integer;
begin
  //Select all items in the Listbox
  for i:=0 to TablesCheckListBox.Items.Count-1 do
  begin
    TablesCheckListBox.Checked[i]:=True;

    //Add to SelectedTables if the table isn't already in the string
    if(Pos(TablesCheckListBox.Items[i]+';', SelectedTables)=0)then
      SelectedTables:=SelectedTables+TablesCheckListBox.Items[i]+';';
  end;
end;

procedure TMainForm.DeselectAllMIClick(Sender: TObject);
var i: integer;
begin
  //Select all items in the Listbox
  for i:=0 to TablesCheckListBox.Items.Count-1 do
  begin
    TablesCheckListBox.Checked[i]:=False;

    //Delete from SelectedTables if the table is in the string
    if(Pos(TablesCheckListBox.Items[i]+';', SelectedTables)>0)then
      SelectedTables:=DMMain.ReplaceText(SelectedTables,
        TablesCheckListBox.Items[i]+';', '');
  end;
end;

procedure TMainForm.CancelBtnClick(Sender: TObject);
begin
  //When the user presses Cancel, close the program
  Close;
end;

procedure TMainForm.SubmitBtnClick(Sender: TObject);
var theSaveDialog: TSaveDialog;
  RecentSaveFileAsDir: string;
  ReportAsText: TStringList;
begin
  ReportAsText:=TStringList.Create;
  try
    theSaveDialog:=TSaveDialog.Create(nil);
    try
  {$IFDEF MSWINDOWS}
      //On Windows use native Win32 Open Dlg
      theSaveDialog.UseNativeDialog:=True;
      theSaveDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
  {$ENDIF}

      theSaveDialog.Title:='Save File As ...';
      theSaveDialog.Width:=600;
      theSaveDialog.Height:=450;
      theSaveDialog.DefaultExt:='html';

      RecentSaveFileAsDir:=DMMain.LoadValueFromSettingsIniFile('RecentDirectories', 'RecentSaveFileAsDir', '');
      {if(RecentSaveFileAsDir='')or(Not(DirectoryExists(RecentSaveFileAsDir)))then
        RecentSaveFileAsDir:=ExtractFilePath(Application.ExeName)+
          'Models'+PathDelim;}

      theSaveDialog.InitialDir:=RecentSaveFileAsDir;
      theSaveDialog.FileName:=lastFileName;
      theSaveDialog.Filter:='HTML files (*.html)|*.html';

      //Build HTML File
      ReportAsText.Text:=BuildHTMLFile(LayoutComboBox.Items[LayoutComboBox.ItemIndex]);

      if(ReportAsText.Text<>'')then
      begin
        if(theSaveDialog.Execute)then
        begin
          ReportAsText.SaveToFile(theSaveDialog.FileName);

          lastFileName:=ExtractFileName(theSaveDialog.FileName);
          RecentSaveFileAsDir:=ExtractFilePath(theSaveDialog.FileName);
          DMMain.SaveValueInSettingsIniFile('RecentDirectories', 'RecentSaveFileAsDir', RecentSaveFileAsDir);

          DMMain.BrowsePage(theSaveDialog.FileName);
        end;
      end;
    finally
      theSaveDialog.Free;
    end;
  finally
    ReportAsText.Free;
  end;
end;

procedure TMainForm.GetLayouts;
var result: integer;
  SearchRec: TSearchRec;
begin
{$IFDEF MSWINDOWS}
  Result := FindFirst(ExtractFilePath(Application.ExeName)+PathDelim+
    'Data'+PathDelim+'Plugins'+PathDelim+'HTMLReport'+PathDelim+'*.*',
    faDirectory, SearchRec);
{$ELSE}
  Result := FindFirst(ExtractFilePath(Application.ExeName)+PathDelim+
    'Data'+PathDelim+'Plugins'+PathDelim+'HTMLReport'+PathDelim+'*',
    faDirectory, SearchRec);
{$ENDIF}

  if(Result<>0)then
    Exit;

  LayoutComboBox.Items.Clear;

  try
    while Result = 0 do
    begin
      if(Copy(SearchRec.Name, 1, 1)<>'.')and
        (SearchRec.Name<>'CVS')then
        LayoutComboBox.Items.Add(SearchRec.Name);

      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;

  if(LayoutComboBox.Items.IndexOf('Standard')<>-1)then
    LayoutComboBox.ItemIndex:=LayoutComboBox.Items.IndexOf('Standard')
  else
    LayoutComboBox.ItemIndex:=0;
end;

procedure TMainForm.StorePluginDataInModel;
var thePluginData: TEERPluginData;
begin
  //Store changes to model

  //Get PluginData from Model
  thePluginData:=EERModel.GetPluginDataByID('HTMLReport', -1);
  if(thePluginData=nil)then
    thePluginData:=EERModel.AddPluginData('HTMLReport', -1);
  //If a value has changed, store model
  if(thePluginData.Params.Values['SelectedTables']<>SelectedTables)then
  begin
    thePluginData.Params.Values['SelectedTables']:=SelectedTables;

    //Delete file and create new, so the new file has a different timestamp
    //DeleteFile(ModelFileName);

    EERModel.SaveToFile(ModelFileName);
  end;
end;

function TMainForm.BuildHTMLFile(layout: string): string;
var theFilePath: string;
  aReportFile: TStringList;
  theReport, s: string;
  i, j: integer;
  theRegion: TEERRegion;
  theTable: TEERTable;
  theTables: TList;
begin
  BuildHTMLFile:='';

  theFilePath:=ExtractFilePath(Application.ExeName)+PathDelim+
    'Data'+PathDelim+'Plugins'+PathDelim+'HTMLReport'+PathDelim+layout+PathDelim;

  if(Not(FileExists(theFilePath+'ReportHeader.txt')))then
    Exit;

  aReportFile:=TStringList.Create;
  theTables:=TList.Create;
  try
    aReportFile.LoadFromFile(theFilePath+'ReportHeader.txt');
    theReport:=DMMain.ReplaceText(aReportFile.Text,
      '<?=$Modelname?>', 'Database Model '+ExtractFileName(EERModel.ModelFilename));

    aReportFile.LoadFromFile(theFilePath+'Report.css');

    theReport:=DMMain.ReplaceText(theReport,
      '<? include ''Report.css''; ?>', aReportFile.Text);

    if(ListRegionsCheckBox.Checked)then
    begin
      for i:=0 to RegionsComboBox.Items.Count-1 do
      begin
        theRegion:=EERModel.GetEERObjectByName(EERRegion, RegionsComboBox.Items[i]);

        if(theRegion<>nil)then
        begin
          //Load Region Template
          aReportFile.LoadFromFile(theFilePath+'ReportRegions.txt');
          theReport:=theReport+
            DMMain.ReplaceText(aReportFile.Text, '<?=$Regionname?>',
              theRegion.ObjName);

          //Build HTMLTableList
          theTables.Clear;
          theRegion.GetEERObjsInRegion([EERTable], theTables);
          if(SortOrderPosRBtn.Checked)then
            EERModel.SortEERObjectListByOrderPos(theTables)
          else
            EERModel.SortEERObjectListByObjName(theTables);

          s:='';
          for j:=0 to theTables.Count-1 do
            s:=s+BuildHTMLTable(TEERTable(theTables[j]), layout);

          //Insert HTMLTableList into Report File
          theReport:=DMMain.ReplaceText(theReport,
            '<? include ''ReportTables.txt''; ?>', s);
        end;
      end;
    end
    else
    begin
      for i:=0 to TablesCheckListBox.Items.Count-1 do
      begin
        theTable:=EERModel.GetEERObjectByName(EERTable, TablesCheckListBox.Items[i]);
        if(theTable<>nil)then
          theReport:=theReport+BuildHTMLTable(theTable, layout);
      end;
    end;

    aReportFile.LoadFromFile(theFilePath+'ReportFooter.txt');
    theReport:=theReport+aReportFile.Text;

    BuildHTMLFile:=theReport;
  finally
    aReportFile.Free;
    theTables.Free;
  end;
end;

function TMainForm.BuildHTMLTable(theTable: TEERTable; layout: string): string;
var theFilePath: string;
  aReportFile: TStringList;
  s, s1, cols, indices, index, IndexKind: string;
  i, j: integer;
  theCol: TEERColumn;
  theDatatype: TEERDatatype;
begin
  BuildHTMLTable:='';

  theFilePath:=ExtractFilePath(Application.ExeName)+PathDelim+
    'Data'+PathDelim+'Plugins'+PathDelim+'HTMLReport'+PathDelim+layout+PathDelim;

  //Only if the table is in the SelectedTables string
  if(Pos(theTable.ObjName+';', SelectedTables)>0)then
  begin
    aReportFile:=TStringList.Create;
    try
      aReportFile.LoadFromFile(theFilePath+'ReportTables.txt');
      s:=DMMain.ReplaceText(aReportFile.Text,
        '<?=$Tablename?>', theTable.ObjName);
      s:=DMMain.ReplaceText(s,
        '<?=$TableComments?>', '<br>'+DMMain.ReplaceText(theTable.Comments, #13#10, '<br>'));

      //Insert HTML Column List
      aReportFile.LoadFromFile(theFilePath+'ReportTableColumns.txt');
      cols:='';
      for i:=0 to theTable.Columns.Count-1 do
      begin
        theCol:=TEERColumn(theTable.Columns[i]);
        theDatatype:=EERModel.GetDataType(theCol.idDatatype);

        cols:=cols+aReportFile.Text;

        if(theCol.PrimaryKey)then
        begin
          //---------------------------
          //Colname
          cols:=DMMain.ReplaceText(cols,
            '<?=ColumnName?>', '<b>'+theCol.ColName+'</b>');

          //---------------------------
          //Dataype
          cols:=DMMain.ReplaceText(cols,
            '<?=DataType?>', '<b>'+trim(theDatatype.TypeName+
              theCol.DatatypeParams)+'</b>');
        end
        else
        begin
          //---------------------------
          //Colname
          cols:=DMMain.ReplaceText(cols,
            '<?=ColumnName?>', theCol.ColName);

          //---------------------------
          //Dataype
          cols:=DMMain.ReplaceText(cols,
            '<?=DataType?>', trim(theDatatype.TypeName+
              theCol.DatatypeParams));
        end;

        //---------------------------
        //Primary Key
        if(theCol.PrimaryKey)then
          s1:='PK'
        else
          s1:='&nbsp;';
        cols:=DMMain.ReplaceText(cols,
          '<?=PrimaryKey?>', s1);

        //---------------------------
        //NotNull
        if(theCol.NotNull)then
          s1:='NN'
        else
          s1:='&nbsp;';
        cols:=DMMain.ReplaceText(cols,
          '<?=NotNull?>', s1);

        //---------------------------
        //Flags
        s1:='';
        for j:=0 to theDatatype.OptionCount do
          if(theCol.OptionSelected[j])then
            s1:=s1+theDatatype.Options[j]+' ';

        if(trim(s1)='')then
          s1:='&nbsp;';
        cols:=DMMain.ReplaceText(cols,
          '<?=Flags?>', s1);

        //---------------------------
        //DefaultValue
        if(theCol.DefaultValue<>'')then
          s1:=theCol.DefaultValue
        else
          s1:='&nbsp;';
        cols:=DMMain.ReplaceText(cols,
          '<?=DefaultValue?>', s1);

        //---------------------------
        //Comment
        if(theCol.Comments<>'')then
          s1:=theCol.Comments
        else
          s1:='&nbsp;';
        cols:=DMMain.ReplaceText(cols,
          '<?=Comment?>', s1);

        //---------------------------
        //AutoInc
        if(theCol.AutoInc)then
          s1:='AI'
        else
          s1:='&nbsp;';
        cols:=DMMain.ReplaceText(cols,
          '<?=AutoInc?>', s1);
      end;
      s:=DMMain.ReplaceText(s,
        '<? include ''ReportTableColumns.txt''; ?>', cols);

      //Insert HTML Index List
      if(Not(ListIndicesCheckBox.Checked))or
        (theTable.Indices.Count=0)then
        indices:=''
      else
      begin
        aReportFile.LoadFromFile(theFilePath+'ReportTableIndices.txt');
        indices:=aReportFile.Text;

        aReportFile.LoadFromFile(theFilePath+'ReportTableIndexValues.txt');
        s1:=aReportFile.Text;

        index:='';
        for i:=0 to theTable.Indices.Count-1 do
        begin
          case TEERIndex(theTable.Indices[i]).IndexKind of
            ik_PRIMARY:
              IndexKind:='PRIMARY';
            ik_INDEX:
              IndexKind:='Index';
            ik_UNIQUE_INDEX:
              IndexKind:='Unique Index';
            ik_FULLTEXT_INDEX:
              IndexKind:='Fulltext Index';
          else
            IndexKind:='';
          end;

          index:=index+s1;
          index:=DMMain.ReplaceText(index,
            '<?=IndexName?>', TEERIndex(theTable.Indices[i]).IndexName);

          index:=DMMain.ReplaceText(index,
            '<?=IndexType?>', IndexKind);

          cols:='';
          for j:=0 to TEERIndex(theTable.Indices[i]).Columns.Count-1 do
            cols:=cols+TEERColumn(theTable.GetColumnById(
              StrToInt(TEERIndex(theTable.Indices[i]).Columns[j])
                )).ColName+'<br>';

          index:=DMMain.ReplaceText(index,
            '<?=Columns?>', cols);
        end;

        indices:=DMMain.ReplaceText(indices,
          '<? include ''ReportTableIndexValues.txt''; ?>', index);
      end;
      s:=DMMain.ReplaceText(s,
        '<? include ''ReportTableIndices.txt''; ?>', indices);


      BuildHTMLTable:=s;
    finally
      aReportFile.Free;
    end;
  end;
end;



procedure TMainForm.SortOrderPosRBtnClick(Sender: TObject);
begin
  RegionsComboBoxCloseUp(self);
end;

end.
