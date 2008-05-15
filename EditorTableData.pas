unit EditorTableData;

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
// Unit EditorTableData.pas
// ------------------------
// Version 1.0, 25.12.2002, Mike
// Description
//   Editor for table data
//
// Changes:
//   Version 1.0, 25.12.2002, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, FMTBcd, DB, DBClient, Provider, SqlExpr, QButtons,
  QExtCtrls, QDBCtrls, QGrids, QDBGrids, QComCtrls, EERModel, IniFiles;

type
  TEditorTableDataForm = class(TForm)
    TopPnl: TPanel;
    BottomPnl: TPanel;
    TableLbl: TLabel;
    OutputQry: TSQLQuery;
    OutputDataSetProvider: TDataSetProvider;
    OutputClientDataSet: TClientDataSet;
    OutputDataSrc: TDataSource;
    Label1: TLabel;
    DBConnEd: TEdit;
    GetDBConnSBtn: TSpeedButton;
    TableCBox: TComboBox;
    CommitPnl: TPanel;
    QueryDockPnl: TPanel;
    LeftPnl: TPanel;
    OpenSQLPnlBtn: TSpeedButton;
    RightPnl: TPanel;
    OpenBlobBtn: TSpeedButton;
    QueryMainPnl: TPanel;
    BlobSplitter: TSplitter;
    SQLSplitter: TSplitter;
    BlobPnl: TPanel;
    BlobPageControl: TPageControl;
    ClearSheet: TTabSheet;
    TextSheet: TTabSheet;
    DBMemo: TDBMemo;
    ImgSheet: TTabSheet;
    DBImage: TDBImage;
    BlobSheet: TTabSheet;
    Splitter1: TSplitter;
    BlobMemo: TMemo;
    BlobHexMemo: TMemo;
    BlobFuncPnl: TPanel;
    BlobBtnPnl: TPanel;
    BlobClearBtn: TSpeedButton;
    BlobOpenBtn: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SQLPnl: TPanel;
    SQLHeaderPnl: TPanel;
    SQLCmdLU: TComboBox;
    SQLFuncPnl: TPanel;
    SQLFuncRightPnl: TPanel;
    SQLMemo: TMemo;
    GridPnl: TPanel;
    DBGrid: TDBGrid;
    GridTopPnl: TPanel;
    GridFuncPnl: TPanel;
    SubmitBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    DBNav: TDBNavigator;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    DBNavigator3: TDBNavigator;
    LoadSQLBtn: TSpeedButton;
    SaveSQLBtn: TSpeedButton;
    ClearSQLBtn: TSpeedButton;
    ExecSQLBtn: TSpeedButton;
    GetDBConn2SBtn: TSpeedButton;
    Bevel4: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure SetTable(theTable: TEERTable);
    procedure ApplyChanges;
    procedure GetDBConnSBtnClick(Sender: TObject);
    procedure TableCBoxChange(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DBGridColEnter(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BlobClearBtnClick(Sender: TObject);
    procedure BlobOpenBtnClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure OpenBlobBtnClick(Sender: TObject);
    procedure OpenSQLPnlBtnClick(Sender: TObject);
    procedure LeftPnlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RightPnlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ClearSQLBtnClick(Sender: TObject);
    procedure LoadSQLBtnClick(Sender: TObject);
    procedure SaveSQLBtnClick(Sender: TObject);
    procedure ExecSQLBtnClick(Sender: TObject);
    procedure SQLMemoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SQLMemoDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure GetDBConn2SBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    theEERTable: TEERTable;
  end;

var
  EditorTableDataForm: TEditorTableDataForm;

implementation

uses DBDM, MainDM, GUIDM, EER;

{$R *.xfm}

procedure TEditorTableDataForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);

  OutputQry.SQLConnection:=DMDB.SQLConn;

  BlobPageControl.Style:=tsNoTabs;
  BlobPnl.Width:=0;
  SQLPnl.Width:=0;

  BlobHexMemo.Text:='';
end;

procedure TEditorTableDataForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TEditorTableDataForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TEditorTableDataForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  //
end;

procedure TEditorTableDataForm.SetTable(theTable: TEERTable);
var theTables: TStringList;
  i: integer;
  theColumn: TColumn;
begin
  DBGrid.Columns.Clear;
  DBMemo.DataField:='';

  if(theTable=nil)then
    Exit;
    
  theEERTable:=theTable;

  theTables:=TStringList.Create;
  try
    TableCBox.Items.Clear;
    TEERModel(theEERTable.Parent).GetEERObjectNameList(EERTable, theTables);

    TableCBox.Items.Assign(theTables);
    TableCBox.ItemIndex:=TableCBox.Items.IndexOf(theEERTable.ObjName);
  finally
    theTables.Free;
  end;


  //when not connected to DB, connect now
  if(DMDB.CurrentDBConn=nil)then
  begin
    DMDB.GetDBConnButtonClick(self, TEERModel(theEERTable.Parent).DefQueryDBConn);
    if(DMDB.CurrentDBConn<>nil)then
      TEERModel(theEERTable.Parent).DefQueryDBConn:=DMDB.CurrentDBConn.Name
    else
      Abort;
  end
  else
  begin
    OutputQry.Close;
    OutputClientDataSet.Close;
  end;

  DBConnEd.Text:=PDBConn(DMDB.CurrentDBConn).Name;

  OutputQry.SQL.Text:='SELECT * FROM '+theEERTable.ObjName;
  OutputQry.Open;
  OutputClientDataSet.Open;

  for i:=0 to OutputClientDataSet.Fields.Count-1 do
  begin
    theColumn:=DBGrid.Columns.Add;
    theColumn.Field:=OutputClientDataSet.Fields[i];

    if(theColumn.Field.DataType=ftSmallint)or
      (theColumn.Field.DataType=ftInteger)or
      (theColumn.Field.DataType=ftWord)or
      (theColumn.Field.DataType=ftLargeint)or
      (theColumn.Field.DataType=ftAutoInc)then
      theColumn.Width:=60
    else if(theColumn.Field.DataType=ftString)or
      (theColumn.Field.DataType=ftFixedChar)or
      (theColumn.Field.DataType=ftWideString)then
      theColumn.Width:=120
    else if(theColumn.Field.DataType=ftDateTime)then
      theColumn.Width:=120
    else if(theColumn.Field.DataType=ftDate)or
      (theColumn.Field.DataType=ftTime)then
      theColumn.Width:=120
    else
      theColumn.Width:=50;
  end;

  DBGridColEnter(self);
end;

procedure TEditorTableDataForm.ApplyChanges;
begin
  //
end;

procedure TEditorTableDataForm.GetDBConnSBtnClick(Sender: TObject);
begin
  SetTable(theEERTable);
end;

procedure TEditorTableDataForm.TableCBoxChange(Sender: TObject);
var theTable: TEERTable;
begin
  theTable:=TEERModel(theEERTable.Parent).GetEERObjectByName(EERTable, TableCBox.Items[TableCBox.ItemIndex]);

  if(theTable<>nil)then
    SetTable(theTable);
end;

procedure TEditorTableDataForm.SubmitBtnClick(Sender: TObject);
begin
  if(OutputClientDataSet.Active)then
    OutputClientDataSet.ApplyUpdates(-1);
end;

procedure TEditorTableDataForm.CancelBtnClick(Sender: TObject);
begin
  if(OutputClientDataSet.Active)then
  begin
    OutputClientDataSet.Close;
    OutputClientDataSet.Open;
  end;
end;

procedure TEditorTableDataForm.DBGridColEnter(Sender: TObject);
var theStream: TMemoryStream;
  thePicture: TPicture;
  //i, toRead: integer;
  theBuffer: Array [0..1024] of Char;
  theText: Array [0..2048] of Char;
begin
  DBMemo.DataField:='';
  DBImage.DataField:='';
  //String / Text
  if(DBGrid.SelectedField.ClassNameIs('TMemoField'))or
    (DBGrid.SelectedField.ClassNameIs('TStringField'))or
    (DBGrid.SelectedField.ClassNameIs('TWideStringField'))or
    (DBGrid.SelectedField.ClassNameIs('TAutoIncField'))then
  begin
    BlobPageControl.Activepage:=TextSheet;
    DBMemo.DataField:=DBGrid.Columns[DBGrid.SelectedIndex].FieldName;
  end
  //Numeric
  else if(DBGrid.SelectedField.ClassNameIs('TAutoIncField'))or
    (DBGrid.SelectedField.ClassNameIs('TBooleanField'))or
    (DBGrid.SelectedField.ClassNameIs('TFloatField'))or
    (DBGrid.SelectedField.ClassNameIs('TGuidField'))or
    (DBGrid.SelectedField.ClassNameIs('TIntegerField'))or
    (DBGrid.SelectedField.ClassNameIs('TLargeintField'))or
    (DBGrid.SelectedField.ClassNameIs('TSmallIntField'))then
  begin
    BlobPageControl.Activepage:=TextSheet;
    DBMemo.DataField:=DBGrid.Columns[DBGrid.SelectedIndex].FieldName;
  end
  else if(DBGrid.SelectedField.ClassNameIs('TGraphicField'))or
    (DBGrid.SelectedField.ClassNameIs('TBlobField'))then
  begin
    thePicture:=TPicture.Create;
    try
      theStream:=TMemoryStream.Create;
      try
        try
          DeleteFile('blob_tmp.png');
          //TBlobField(DBGrid.SelectedField).SaveToStream(theStream);
          TBlobField(DBGrid.SelectedField).SaveToFile('blob_tmp.png');

          //Try to display as Image
          thePicture.LoadFromFile('blob_tmp.png');

          DBImage.DataField:=DBGrid.Columns[DBGrid.SelectedIndex].FieldName;
          BlobPageControl.Activepage:=ImgSheet;
        except
          //if it is in another format, display as blob
          BlobMemo.Lines.LoadFromFile('blob_tmp.png');

          //Display Hex Data
          BlobHexMemo.Text:='';
          TBlobField(DBGrid.SelectedField).SaveToStream(theStream);

          {toRead:=theStream.Size;
          if(toRead>1024)then
            toRead:=1024;
          theStream.Read(theBuffer, 1024);}

          BinToHex(theBuffer, theText, 1024);
          BlobHexMemo.Text:=String(theText);

          BlobPageControl.Activepage:=BlobSheet;
        end;
      finally
        theStream.Free;
        DeleteFile('blob_tmp.png');
      end;
    finally
      thePicture.Free;
    end;
  end
  else
  begin
    BlobPageControl.Activepage:=ClearSheet;
  end;
end;

procedure TEditorTableDataForm.FormResize(Sender: TObject);
begin
  DBConnEd.Width:=TopPnl.Width-505;
  GetDBConnSBtn.Left:=DBConnEd.Left+DBConnEd.Width+5;
end;

procedure TEditorTableDataForm.FormShow(Sender: TObject);
begin
  FormResize(self);
end;

procedure TEditorTableDataForm.BlobClearBtnClick(Sender: TObject);
begin
  if(DBGrid.SelectedField=nil)then
    Exit;

  if(Not(OutputClientDataSet.State=dsEdit))or
    (Not(OutputClientDataSet.State=dsInsert))then
    OutputClientDataSet.Edit;

  DBGrid.SelectedField.Clear;
end;

procedure TEditorTableDataForm.BlobOpenBtnClick(Sender: TObject);
var theOpenDialog: TOpenDialog;
  RecentOpenBlobFieldDir: string;
  theIni: TIniFile;
  thePicture: TPicture;
begin
  if(DBGrid.SelectedField=nil)then
    Exit;

  if(Not(OutputClientDataSet.Active))then
    Exit;

  if(Not(OutputClientDataSet.State=dsEdit))or
    (Not(OutputClientDataSet.State=dsInsert))then
    OutputClientDataSet.Edit;

  //Open IniFile
  theIni:=TIniFile.Create(ExtractFilePath(Application.ExeName)+
    'Data'+PathDelim+DMMain.ProgName+'_Settings.ini');
  try
    theOpenDialog:=TOpenDialog.Create(nil);
    try
  {$IFDEF MSWINDOWS}
      //On Windows use native Win32 Open Dlg
      theOpenDialog.UseNativeDialog:=True;
      theOpenDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
  {$ENDIF}

      theOpenDialog.Title:='Open a File ...';
      theOpenDialog.DefaultExt:='';
      theOpenDialog.Filter:='All files (*.*)|*.*';
      theOpenDialog.Width:=600;
      theOpenDialog.Height:=450;

      //Get last dir
      RecentOpenBlobFieldDir:=theIni.ReadString('RecentDirectories', 'RecentOpenBlobFieldDir', '');

      if(DirectoryExists(RecentOpenBlobFieldDir))then
        theOpenDialog.InitialDir:=RecentOpenBlobFieldDir
      else
        theOpenDialog.InitialDir:='';


      if(theOpenDialog.Execute)then
      begin
        RecentOpenBlobFieldDir:=ExtractFilePath(theOpenDialog.Filename);

        if(DBGrid.SelectedField.ClassNameIs('TBlobField'))or
          (DBGrid.SelectedField.ClassParent.ClassNameIs('TBlobField'))then
        begin
          DBImage.DataField:='';
          DBMemo.DataField:='';

          //Memo Field
          if(DBGrid.SelectedField.ClassNameIs('TMemoField'))then
          begin
            TMemoField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename);
            DBMemo.DataField:=DBGrid.Columns[DBGrid.SelectedIndex].FieldName;
          end
          else
          //Blob or Image Field
          begin
            TBlobField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename);

            thePicture:=TPicture.Create;
            try
              try
                //try to open as Image
                thePicture.LoadFromFile(theOpenDialog.Filename);

                DBImage.DataField:=DBGrid.Columns[DBGrid.SelectedIndex].FieldName;
                BlobPageControl.ActivePage:=ImgSheet;
              except
                BlobPageControl.ActivePage:=BlobSheet;
                TBlobField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename);

                DBGridColEnter(self);
              end;
            finally
              thePicture.Free;
            end;
          end;
        end;

        {if(DBGrid.SelectedField.ClassNameIs('TMemoField'))then
          TMemoField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename)
        else if(DBGrid.SelectedField.ClassNameIs('TGraphicField'))then
          TGraphicField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename)
        else if(DBGrid.SelectedField.ClassNameIs('TBlobField'))then
          TBlobField(DBGrid.SelectedField).LoadFromFile(theOpenDialog.Filename);}

        theIni.WriteString('RecentDirectories', 'RecentOpenBlobFieldDir', RecentOpenBlobFieldDir);
      end;

    finally
      theOpenDialog.Free;
    end;
  finally
    theIni.Free;
  end;
end;

procedure TEditorTableDataForm.SpeedButton1Click(Sender: TObject);
var theSaveDialog: TSaveDialog;
  RecentSaveBlobFieldDir, theFileName: string;
  theIni: TIniFile;
begin
  if(DBGrid.SelectedField=nil)then
    Exit;

  if(Not(OutputClientDataSet.Active))then
    Exit;

  //Read IniFile
  theIni:=TIniFile.Create(ExtractFilePath(Application.ExeName)+
    'Data'+PathDelim+DMMain.ProgName+'_Settings.ini');
  try

    theSaveDialog:=TSaveDialog.Create(nil);
    try
  {$IFDEF MSWINDOWS}
      //On Windows use native Win32 Open Dlg
      theSaveDialog.UseNativeDialog:=True;
      theSaveDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
  {$ENDIF}

      theSaveDialog.Title:='Save Field As ...';
      theSaveDialog.Width:=600;
      theSaveDialog.Height:=450;
      theSaveDialog.DefaultExt:='';

      RecentSaveBlobFieldDir:=theIni.ReadString('RecentDirectories', 'RecentSaveBlobFieldDir', '');
      if(Not(DirectoryExists(RecentSaveBlobFieldDir)))then
        RecentSaveBlobFieldDir:=ExtractFilePath(Application.ExeName)+
          'Models'+PathDelim;

      theSaveDialog.InitialDir:=RecentSaveBlobFieldDir;


      {theSaveDialog.Position:=Point((Screen.Width-theSaveDialog.Width) div 2,
        (Screen.Height-theSaveDialog.Height) div 2);}

      theSaveDialog.Filter:='All Files (*.*)|*.*';

      if(theSaveDialog.Execute)then
      begin
        theFileName:=theSaveDialog.Filename;
        if(FileExists(theFileName))then
          if(MessageDlg('The file ['+ExtractFileName(theFileName)+'] '+
            'already exists. '#13#10+
            'Do you want to overwrite this file?', mtInformation,
            [mbYes, mbNo], 0)=mrNo)then
            Exit;

        RecentSaveBlobFieldDir:=ExtractFilePath(theSaveDialog.FileName);

        if(DBGrid.SelectedField.ClassNameIs('TMemoField'))then
          TMemoField(DBGrid.SelectedField).SaveToFile(theFileName)
        else if(DBGrid.SelectedField.ClassNameIs('TGraphicField'))then
          TGraphicField(DBGrid.SelectedField).SaveToFile(theFileName)
        else if(DBGrid.SelectedField.ClassNameIs('TBlobField'))then
          TBlobField(DBGrid.SelectedField).SaveToFile(theFileName);

        theIni.WriteString('RecentDirectories', 'RecentSaveBlobFieldDir', RecentSaveBlobFieldDir);
      end;
    finally
      theSaveDialog.Free;
    end;
  finally
    theIni.Free;
  end;
end;

procedure TEditorTableDataForm.DBGridDblClick(Sender: TObject);
begin
  if(Not(OpenBlobBtn.Enabled))then
    RightPnlMouseDown(Self, mbLeft, [], 0, 0);
end;



procedure TEditorTableDataForm.OpenSQLPnlBtnClick(Sender: TObject);
begin
  OpenSQLPnlBtn.Enabled:=False;
  SQLSplitter.Visible:=True;
  SQLSplitter.Left:=300;
  SQLPnl.Width:=200;
end;

procedure TEditorTableDataForm.OpenBlobBtnClick(Sender: TObject);
begin
  //Hide Blob Pnl
  OpenBlobBtn.Enabled:=False;

  BlobSplitter.Visible:=False;
  BlobPnl.Width:=0;
end;

procedure TEditorTableDataForm.LeftPnlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //Show Blob Pnl
  if(Not(OpenSQLPnlBtn.Enabled))then
  begin
    OpenSQLPnlBtn.Enabled:=True;
    SQLSplitter.Visible:=False;
    SQLPnl.Width:=0;
  end;
end;

procedure TEditorTableDataForm.RightPnlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //Show Blob Pnl
  if(Not(OpenBlobBtn.Enabled))then
  begin
    OpenBlobBtn.Enabled:=True;

    BlobSplitter.Visible:=True;
    BlobSplitter.Left:=300;
    BlobPnl.Width:=240;
    RightPnl.Left:=Width-10;
  end;
end;

procedure TEditorTableDataForm.ClearSQLBtnClick(Sender: TObject);
begin
  SQLMemo.Clear;
end;

procedure TEditorTableDataForm.LoadSQLBtnClick(Sender: TObject);
begin
  //
end;

procedure TEditorTableDataForm.SaveSQLBtnClick(Sender: TObject);
begin
  //
end;

procedure TEditorTableDataForm.ExecSQLBtnClick(Sender: TObject);
var i, Output: integer;
begin
  //when not connected to DB, connect now
  if(DMDB.CurrentDBConn=nil)then
  begin
    GetDBConnSBtnClick(self);

    if(DMDB.CurrentDBConn=nil)then
      Exit;
  end
  else
  begin
    DMDB.OutputQry.Close;
    DMDB.OutputClientDataSet.Close;

    DBGrid.DataSource:=DMDB.OutputDataSrc;
  end;

  //Trim all lines
  for i:=0 to SQLMemo.Lines.Count-1 do
    SQLMemo.Lines[i]:=Trim(SQLMemo.Lines[i]);

  //Delete all rows from top which contain ''
  while(SQLMemo.Lines.Count>1)and(SQLMemo.Lines[0]='')do
    SQLMemo.Lines.Delete(0);

  //if there is no sql statement, exit
	if(SQLMemo.Text='')then
    Exit;

  Output:=1;

  if((CompareText(Copy(SQLMemo.Lines[0], 1, 6), 'update')=0)
  	or(CompareText(Copy(SQLMemo.Lines[0], 1, 6), 'insert')=0)
    or(CompareText(Copy(SQLMemo.Lines[0], 1, 6), 'delete')=0)
    or(CompareText(Copy(SQLMemo.Lines[0], 1, 4), 'drop')=0)
    or(CompareText(Copy(SQLMemo.Lines[0], 1, 5), 'alter')=0)
    or(CompareText(Copy(SQLMemo.Lines[0], 1, 6), 'create')=0)
    or(CompareText(Copy(SQLMemo.Lines[0], 1, 3), 'set')=0)
    or(CompareText(Copy(SQLMemo.Lines[0], 1, 4), 'save')=0)
    or(CompareText(Copy(SQLMemo.Lines[0], 1, 4), 'load')=0)
    or(CompareText(Copy(SQLMemo.Lines[0], 1, 4), 'move')=0)
    or(CompareText(Copy(SQLMemo.Lines[0], 1, 1), '<')=0)
    or(CompareText(Copy(SQLMemo.Lines[0], 1, 1), '/')=0))then
  	Output:=0;

  DMDB.OutputQry.SQL.Text:=SQLMemo.Text;
  if(Output=0)then
  begin
    DMDB.OutputQry.ExecSQL(True);
    DMGUI.SetStatusCaption('Query executed successfully.');
  end
  else if(Output=1)then
  begin
    DMDB.OutputQry.Open;
    DMDB.OutputClientDataSet.Open;
    DMGUI.SetStatusCaption('Query opened.');
  end;
end;

procedure TEditorTableDataForm.SQLMemoDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var theTable: TEERTable;
  theModel: TEERModel;
  i, j, k, destTblNr, joinCount: integer;
  theTableList: TList;
  theTableAliases: TStringList;
  s: string;
begin
  if(Source<>nil)then
    if(Source.ClassnameIs('TEERTable'))then
    begin
      s:='';

      theTable:=TEERTable(Source);
      theModel:=TEERModel(TEERTable(Source).Parent);

      theTableList:=TList.Create;
      theTableAliases:=TStringList.Create;
      try
        //Get all selected
        for i:=0 to theModel.ComponentCount-1 do
          if(theModel.Components[i].ClassName='TEERTable')then
            if(TEERTable(theModel.Components[i]).Selected)then
              theTableList.Add(theModel.Components[i]);

        if(theTableList.Count=0)then
          theTableList.Add(theTable);

        for i:=0 to theTableList.Count-1 do
        begin
          j:=1;
          while(theTableAliases.IndexOf(Copy(TEERTable(theTableList[i]).ObjName, 1, j))<>-1)and
            (j<10)do
            inc(j);

          theTableAliases.Add(Copy(TEERTable(theTableList[i]).ObjName, 1, j));
        end;

        s:='SELECT * FROM ';

        // list tables
        for i:=0 to theTableList.Count-1 do
        begin
          s:=s+TEERTable(theTableList[i]).ObjName+' '+theTableAliases[i];
          if(i<theTableList.Count-1)then
            s:=s+', ';
        end;

        //make joins
        if(theTableList.Count>1)then
        begin
          s:=s+#13#10+'WHERE ';

          joinCount:=0;
          for i:=0 to theTableList.Count-1 do
          begin
            for j:=0 to TEERTable(theTableList[i]).RelStart.Count-1 do
            begin
              destTblNr:=theTableList.IndexOf(TEERRel(TEERTable(theTableList[i]).RelStart[j]).DestTbl);
              if(destTblNr<>-1)then
              begin
                for k:=0 to TEERRel(TEERTable(theTableList[i]).RelStart[j]).FKFields.Count-1 do
                begin
                  if(joinCount>0)then
                    s:=s+' and ';
                  s:=s+theTableAliases[destTblNr]+'.'+
                    TEERRel(TEERTable(theTableList[i]).RelStart[j]).FKFields.ValueFromIndex[k]+'='+
                    theTableAliases[i]+'.'+
                    TEERRel(TEERTable(theTableList[i]).RelStart[j]).FKFields.Names[k];

                  inc(joinCount);
                end;
              end;
            end;
          end;
        end;

      finally
        theTableList.Free;
        theTableAliases.Free;
      end;

      SQLMemo.Text:=s;
    end;
end;

procedure TEditorTableDataForm.SQLMemoDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=False;
  if(Source<>nil)then
    if(Source.ClassnameIs('TEERTable'))then
      Accept:=True;
end;

procedure TEditorTableDataForm.GetDBConn2SBtnClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
  begin
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      DMDB.GetDBConnButtonClick(Sender, TEERForm(TForm(Application.MainForm).ActiveMDIChild).EERModel.DefQueryDBConn);
      if(DMDB.CurrentDBConn<>nil)then
        TEERForm(ActiveMDIChild).EERModel.DefQueryDBConn:=DMDB.CurrentDBConn.Name;
    end;
  end
  else
    DMDB.GetDBConnButtonClick(Sender);
end;

end.
