unit Main;

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
// Unit Main.pas
// -------------
// Version Fork 1.5, 31.03.2010, JP
// Description
//   BUGFIX: exception when deleting tables was fixed!!!!
// Version Fork 1.4, 14.05.2007, JP
// Description
//   EditorTableForm and EditorRelationForm are now modal.
//
// Version Fork 1.0, 18.09.2006, JP
//
// Version 2.1, 03.05.2003, Mike
// Description
//   Contains the TMainForm which representes the
//   application's Main Form
//
// Changes:
//   Version Fork 1.0, 18.09.2006, JP
//     added export SQL options for Oracle, SQL Server and FireBird.
//   Version 2.1, 03.05.2003, Mike
//     Added CenterModelMI
//   Version 2.0, 18.04.2003, Mike
//     Changed all Records to TObjects.
//   Version 1.6, 15.04.2003, Mike
//     Hide/Show palettes now works with docked palettes
//   Version 1.5, 07.04.2003, Mike
//     ignore application parameters starting with - and open the
//     first existing file submitted as appl. parameter
//   Version 1.4, 04.04.2003, Mike
//     Added Submit Bug Menu Item
//   Version 1.3, 31.03.2003, Mike
//     OpenFile: when appending model, assign new IDs to
//       appeded objects and don't load settings
//   Version 1.2, 28.03.2003, Mike
//     Added SnapToGridBtn and QEventType_RefreshGridBtn Event
//   Version 1.1, 20.03.2003, Mike
//     renamed Syncronisation to Synchronisation in the main menu
//     changed call of EERModel.LoadFromFile in OpenFile to correct
//       wrong setting of AddDataToExistingModel
//   Version 1.0, 13.03.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

{$I DBDesigner4.inc}

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QMenus, QTypes, QComCtrls, QGrids, QDBGrids,
  DBXpress, DB, SqlExpr, QImgList, QButtons, QDBCtrls, QT, QPrinters,
  QClipbrd, QStyle,
{$IFDEF USE_QTheming}QThemed,{$ENDIF}
  EERModel;


const
  INIVersion_DBDesigner4_Settings=4;
  INIVersion_DBDesigner4_DatabaseInfo=8;
  INIVersion_DBDesigner4_Translations=19;
  INIVersion_DBConn_DefaultSettings=4;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMI: TMenuItem;
    EditMI: TMenuItem;
    DisplayMI: TMenuItem;
    OptionsMI: TMenuItem;
    HelpMI: TMenuItem;
    ExitMI: TMenuItem;
    NewMI: TMenuItem;
    N1: TMenuItem;
    StatusPnl: TPanel;
    Shape8: TShape;
    ZoomShape: TShape;
    ZoomLbl: TLabel;
    AboutMI: TMenuItem;
    StatusCaptionLbl: TLabel;
    Bevel1: TBevel;
    MenuSepPnl: TPanel;
    HSepPn: TPanel;
    SaveMI: TMenuItem;
    OpenMI: TMenuItem;
    N2: TMenuItem;
    SaveAsMI: TMenuItem;
    EERModelOptionsMI: TMenuItem;
    N3: TMenuItem;
    DBDesignerOptionsMI: TMenuItem;
    DisplayEntityLevelMI: TMenuItem;
    DisplayAttributeLevelMI: TMenuItem;
    DisplayPrimaryKeyLevelMI: TMenuItem;
    N4: TMenuItem;
    PhysicalSchemaLevelMI: TMenuItem;
    CutMI: TMenuItem;
    CopyMI: TMenuItem;
    PasteMI: TMenuItem;
    DeleteMI: TMenuItem;
    SelectAllMI: TMenuItem;
    WindowsMI: TMenuItem;
    ToolsMI: TMenuItem;
    DBModelMI: TMenuItem;
    N6: TMenuItem;
    CascadeMI: TMenuItem;
    N7: TMenuItem;
    TileMI: TMenuItem;
    DatatypesMI: TMenuItem;
    CloseMI: TMenuItem;
    OpenRecentMI: TMenuItem;
    SaveinDatabaseMI: TMenuItem;
    ExportMI: TMenuItem;
    N8: TMenuItem;
    SQLCreateScriptMI: TMenuItem;
    DatabaseMI: TMenuItem;
    ReverseEngineeringMI: TMenuItem;
    DatabasesyncronisationMI: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    PageSetupMI: TMenuItem;
    N11: TMenuItem;
    NotationMI: TMenuItem;
    NotationStandardMI: TMenuItem;
    NotationErwinMI: TMenuItem;
    DisplayRelationNamesMI: TMenuItem;
    ResetPalettePositionsMI: TMenuItem;
    NavigatorInfoMI: TMenuItem;
    SaveModelasImageMI: TMenuItem;
    N12: TMenuItem;
    NotationStandard2MI: TMenuItem;
    ShowForeignKeysMI: TMenuItem;
    OpenfromDatabaseMI: TMenuItem;
    SQLDropScriptMI: TMenuItem;
    DisplayPageGridMI: TMenuItem;
    PrintMI: TMenuItem;
    Columns1: TMenuItem;
    N14: TMenuItem;
    N5: TMenuItem;
    UndoMI: TMenuItem;
    N13: TMenuItem;
    RedoMI: TMenuItem;
    AddLinkModelMI: TMenuItem;
    Style1: TMenuItem;
    StyleStandardMI: TMenuItem;
    StyleSGIMI: TMenuItem;
    StylePlatinumMI: TMenuItem;
    StyleMotifMI: TMenuItem;
    OnlinedocumentationMI: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    VisitHomepageMI: TMenuItem;
    CheckfornewversionsMI: TMenuItem;
    N17: TMenuItem;
    Bevel2: TBevel;
    RefreshTmr: TTimer;
    QueryPnl: TPanel;
    QuerySplitter: TSplitter;
    ShowPalettesTmr: TTimer;
    ToolsPnl: TPanel;
    Shape1: TShape;
    ToolsdockedMI: TMenuItem;
    SavedImg: TImage;
    NotSavedImg: TImage;
    Save2DiskImg: TImage;
    Save2DBImg: TImage;
    Save2DiskDisabledImg: TImage;
    Save2DBDisabledImg: TImage;
    DBConnPnl: TPanel;
    ConnectionSBtn: TSpeedButton;
    QueryStatusLbl: TLabel;
    Bevel3: TBevel;
    PluginsMI: TMenuItem;
    DockPalettesMI: TMenuItem;
    PaletteDockPnl: TPanel;
    NavPnl: TPanel;
    NavHeaderPnl: TPanel;
    DatatypesPnl: TPanel;
    DatatypesHeaderPnl: TPanel;
    ModelPnl: TPanel;
    ModelHeaderPnl: TPanel;
    Splitter2: TSplitter;
    N18: TMenuItem;
    NavFooterPnl: TPanel;
    DatatypesFooterPnl: TPanel;
    ModelFooterPnl: TPanel;
    PaletteDockSepPnl: TPanel;
    DatatypesSepPnl: TPanel;
    DatatypesLeftPnl: TPanel;
    ModelLeftPnl: TPanel;
    NavLeftPnl: TPanel;
    TableIndicesMI: TMenuItem;
    ListTableIndicesMI: TMenuItem;
    DesignModeMI: TMenuItem;
    N19: TMenuItem;
    QueryModeMI: TMenuItem;
    DesignToolsPnl: TPanel;
    CreatesImg: TImage;
    SyncImg: TImage;
    Image3: TImage;
    wtZoomSBtn: TSpeedButton;
    wtHandSBtn: TSpeedButton;
    wtImageSBtn: TSpeedButton;
    wtNoteSBtn: TSpeedButton;
    Image2: TImage;
    wtRelnmSBtn: TSpeedButton;
    wtRel11SubSBtn: TSpeedButton;
    Image6: TImage;
    wtRel11SBtn: TSpeedButton;
    wtRel1nSBtn: TSpeedButton;
    wtRel1nSubSBtn: TSpeedButton;
    wtTableSBtn: TSpeedButton;
    wtRegionSBtn: TSpeedButton;
    Image1: TImage;
    wtDeleteSBtn: TSpeedButton;
    wtSizeSBtn: TSpeedButton;
    wtMoveSBtn: TSpeedButton;
    wtPointerSBtn: TSpeedButton;
    WorkModePnl: TPanel;
    QueryImg: TImage;
    Designimg: TImage;
    Image5: TImage;
    Shape2: TShape;
    Shape3: TShape;
    QueryToolsPnl: TPanel;
    Shape5: TShape;
    wtPointerQrySBtn: TSpeedButton;
    wtMoveQrySBtn: TSpeedButton;
    Image7: TImage;
    wtHandQrySBtn: TSpeedButton;
    wtZoomQrySBtn: TSpeedButton;
    Image8: TImage;
    Image9: TImage;
    wtSQLSelectSBtn: TSpeedButton;
    wtSQLFromSBtn: TSpeedButton;
    wtSQLWhereSBtn: TSpeedButton;
    wtSQLGroupSBtn: TSpeedButton;
    wtSQLHavingSBtn: TSpeedButton;
    wtSQLOrderSBtn: TSpeedButton;
    wtSQLSetSBtn: TSpeedButton;
    KylixSpaceUpTimer: TTimer;
    SnapToGridBtn: TSpeedButton;
    Image10: TImage;
    SubmitabugfeaturerequestMI: TMenuItem;
    DeactivateTmr: TTimer;
    N20: TMenuItem;
    CenterModelMI: TMenuItem;
    Import1: TMenuItem;
    ImportERwin41XMLModelMI: TMenuItem;
    ConnecttoDatabaseMI: TMenuItem;
    DisconnectfromDatabaseMI: TMenuItem;
    ExportSelectedObjectsAsImgMi: TMenuItem;
    CopyselectedObjectsasImageMI: TMenuItem;
    CloseAllMI: TMenuItem;
    NotationCrowsFootMI: TMenuItem;
    N21: TMenuItem;
    wtRel11NonIdSBtn: TSpeedButton;
    Shape4: TShape;
    SQLOptimizeTableScriptMI: TMenuItem;
    AddLinkModelFromFileMI: TMenuItem;
    AddLinkModelfromDBMI: TMenuItem;
    N22: TMenuItem;
    ShowLinkedModelsMI: TMenuItem;
    RefreshLinkedObjectsMI: TMenuItem;
    N23: TMenuItem;
    AddLinkModelfromOnlineLibraryMI: TMenuItem;
    SQLRepairTableScriptMI: TMenuItem;
    NavInfoPBox: TPaintBox;
    DatatypePBox: TPaintBox;
    DBModelPBox: TPaintBox;
    Test1: TMenuItem;
    N24: TMenuItem;
    ExportMDBXMLFileMI: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure NewMIClick(Sender: TObject);
    function NewEERModel: TForm;
    procedure OpenFile(fname: string; AppendModel: Boolean);
    procedure ZoomLblClick(Sender: TObject);
    procedure ExitMIClick(Sender: TObject);
    procedure AboutMIClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveAsMIClick(Sender: TObject);
    procedure SaveMIClick(Sender: TObject);
    procedure DisplayEntityLevelMIClick(Sender: TObject);
    procedure PhysicalSchemaLevelMIClick(Sender: TObject);



    procedure AddToMDIWindowMenu(theForm: TForm);
    procedure MDIWindowMIClick(Sender: TObject);
    procedure CascadeMIClick(Sender: TObject);
    procedure TileMIClick(Sender: TObject);
    procedure ToolsMIClick(Sender: TObject);
    procedure DatatypesMIClick(Sender: TObject);
    procedure DBModelMIClick(Sender: TObject);
    procedure CloseMIClick(Sender: TObject);
    procedure EERModelOptionsMIClick(Sender: TObject);
    procedure DeleteMIClick(Sender: TObject);
    procedure DeleteMIShow(Sender: TObject);
    procedure NotationStandardMIClick(Sender: TObject);

    procedure SetMenuItemChecks;
    procedure DBDesignerOptionsMIClick(Sender: TObject);
    procedure DisplayRelationNamesMIClick(Sender: TObject);
    procedure ResetPalettePositionsMIClick(Sender: TObject);
    procedure PageSetupMIClick(Sender: TObject);
    procedure NavigatorInfoMIClick(Sender: TObject);
    procedure SaveModelasImageMIClick(Sender: TObject);
    procedure ShowForeignKeysMIClick(Sender: TObject);
    procedure DisplayPageGridMIClick(Sender: TObject);
    procedure PrintMIClick(Sender: TObject);
    procedure SelectAllMIClick(Sender: TObject);
    procedure ActivateEERMIOnShow(Sender: TObject);
    procedure SQLCreateScriptMIClick(Sender: TObject);
    procedure ReverseEngineeringMIClick(Sender: TObject);
    procedure CopyMIClick(Sender: TObject);
    procedure PasteMIClick(Sender: TObject);
    procedure PasteMIShow(Sender: TObject);
    procedure CutMIClick(Sender: TObject);
    procedure DatabasesyncronisationMIClick(Sender: TObject);
    procedure UndoMIShow(Sender: TObject);
    procedure UndoMIClick(Sender: TObject);
    procedure RedoMIShow(Sender: TObject);
    procedure RedoMIClick(Sender: TObject);
    procedure StyleStandardMIClick(Sender: TObject);
    procedure SQLDropScriptMIClick(Sender: TObject);
    procedure OnlinedocumentationMIClick(Sender: TObject);
    procedure VisitHomepageMIClick(Sender: TObject);
    procedure CheckfornewversionsMIClick(Sender: TObject);
    procedure SaveinDatabaseMIClick(Sender: TObject);
    procedure OpenfromDatabaseMIClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RefreshTmrTimer(Sender: TObject);
    procedure ShowPalettesTmrTimer(Sender: TObject);

    procedure DesignimgClick(Sender: TObject);
    procedure QueryImgClick(Sender: TObject);
    procedure ToolsdockedMIClick(Sender: TObject);
    procedure SyncImgClick(Sender: TObject);

    procedure DisableSaveImgs;
    procedure EnableSaveImgs;
    procedure Save2DiskImgClick(Sender: TObject);
    procedure Save2DBImgClick(Sender: TObject);

    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;

    procedure wtPointerSBtnClick(Sender: TObject);
    procedure wtPointerSBtnMouseEnter(Sender: TObject);

    procedure SetApplStyle(ApplStyle: integer);
    procedure SetWorkMode(theWorkMode: integer);
    procedure DisplaySelectedWorkTool(WorkTool: integer);

    procedure ApplicationRestore(Sender: TObject);

    procedure HidePalettes;
    procedure ShowPalettes;

    procedure BuildPluginMenus;
    procedure PluginMIClick(Sender: TObject);

    procedure DockPalettesMIClick(Sender: TObject);
    procedure ListTableIndicesMIClick(Sender: TObject);

    procedure CopyMouseOverObjToClipboard;
    procedure CopyMouseOverObjToSQLMemo(key: integer);

    procedure CopySQLToClipboard(Key: Word);
    procedure DesignModeMIShow(Sender: TObject);
    procedure DesignModeMIClick(Sender: TObject);
    procedure ConnectionSBtnClick(Sender: TObject);

    procedure CheckLinuxDesktopFile;

    procedure DoApplicationEvent(Sender: QObjectH; Event: QEventH; var Handled: Boolean);

    procedure KylixSpaceUpTimerTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SnapToGridBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SubmitabugfeaturerequestMIClick(Sender: TObject);
    procedure DeactivateTmrTimer(Sender: TObject);
    procedure CenterModelMIClick(Sender: TObject);
    procedure ImportERwin41XMLModelMIClick(Sender: TObject);
    procedure ConnecttoDatabaseMIClick(Sender: TObject);
    procedure DisconnectfromDatabaseMIClick(Sender: TObject);
    procedure DisconnectfromDatabaseMIShow(Sender: TObject);

    procedure CopyselectedObjectsasImageMIClick(Sender: TObject);
    procedure CloseAllMIClick(Sender: TObject);
    procedure SQLOptimizeTableScriptMIClick(Sender: TObject);
    procedure OpenMIClick(Sender: TObject);

    procedure PlaceModel(theModel: TEERModel; P: TPoint; PlaceFrom: integer);
    procedure AddLinkModelFromFileMIClick(Sender: TObject);
    procedure AddLinkModelfromDBMIClick(Sender: TObject);
    procedure ShowLinkedModelsMIShow(Sender: TObject);
    procedure ShowLinkedModelsMIClick(Sender: TObject);
    procedure RefreshLinkedObjectsMIClick(Sender: TObject);
    procedure AddLinkModelfromOnlineLibraryMIClick(Sender: TObject);
    procedure SQLRepairTableScriptMIClick(Sender: TObject);
    procedure NavInfoPBoxPaint(Sender: TObject);
    procedure Test1Click(Sender: TObject);
    procedure ExportMDBXMLFileMIClick(Sender: TObject);
  private
    { Private declarations }
    KeyWasUp: Boolean;

    TabHidePalettes: TList;
    TabHasBeenPressedBeforeInactive: Boolean;

    TabHasBeenPressed: Boolean;

    PluginList: TStringList;

    //The EditorQueryDragTargetForm which is created when the user dragges an EERTable
    EditorQueryDragTargetForm: TForm;
    ToolBeforeSpaceDown: integer;

    ActivateDeactivateCounter: integer;
    ApplicationIsDeactivated: Boolean;
  public
    { Public declarations }
    Version: string;

    SpaceDown: Boolean;

    keycounter: integer;

    //The EditorQueryForm which is docked to the QueryPnl
    DockedEditorQueryForm: TForm;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.xfm}

uses MainDM, ZoomSel, EER,
  PaletteTools, PaletteModel, PaletteDatatypes, OptionsModel, Options,
  EERPageSetup, PaletteNav, EERExportSQLScript, DBConnSelect,
  EERReverseEngineering, EERSynchronisation, EERStoreInDatabase, Splash,
  EERDM, EditorTable, EditorRelation, EditorRegion, EditorNote,
  EditorImage, GUIDM, DBDM, EditorQuery, EditorQueryDragTarget,
  Tips, EERPlaceModel, DBEERDM, EERExportImportDM;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnEvent:=DoApplicationEvent;

  //Get Version string, defined in DBDesigner4.dpr
  Version:=SplashForm.VersionLbl.Caption;

  //Initialize DMMain and DMGUI
  DMMain:=TDMMain.Create(self);
  DMGUI:=TDMGUI.Create(self);

  //Initialize Language Translation
  DMMain.LoadLanguageFromIniFile;
  DMMain.LoadTranslatedMessages;

  //Initialize DMDB and DMEER
  DMDB:=TDMDB.Create(self);
  DMEER:=TDMEER.Create(self);

  DMEERExportImport:=TDMEERExportImport.Create(self);

  DMMain.InitForm(self);

  //Create the QueryDragTarget for all models
  EditorQueryDragTargetForm:=TEditorQueryDragTargetForm.Create(self);

  SetMenuItemChecks;

  KeyWasUp:=True;

  SpaceDown:=False;

  Application.ShowMainForm:=True;

  //WindowState:=wsMaximized;

  Application.OnRestore:=ApplicationRestore;

  //List for Tab key to hide palettes
  TabHidePalettes:=TList.Create;

  TabHasBeenPressed:=False;
  TabHasBeenPressedBeforeInactive:=False;

  PluginList:=TStringList.Create;

  //Restore original size of ToolsPnls
  DesignToolsPnl.Height:=487;
  QueryToolsPnl.Height:=254;

{$IFDEF LINUX}
  CheckLinuxDesktopFile;
{$ENDIF}

  ActivateDeactivateCounter:=0;
  ApplicationIsDeactivated:=False;

  //Enable Timer for Palettes and Model load
  ShowPalettesTmr.Enabled:=True;

{$IFNDEF USE_IXMLDBMODELType}
  ImportERwin41XMLModelMI.Enabled:=False;
{$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  TabHidePalettes.Free;
  PluginList.Free;

  DMMain.SaveWinPos(self, True);

  EditorQueryDragTargetForm.Free;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //Clear tmpindices
  DMMain.DelFilesFromDir(DMMain.SettingsPath, 'tmpindex*.*');

  TEditorQueryForm(DockedEditorQueryForm).StoreLayout(DMGUI.DockedQueryPnlMode);
end;

procedure TMainForm.ZoomLblClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
  begin
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      ZoomSelForm:=TZoomSelForm.Create(self);
      try
        ZoomSelForm.Left:=Left+6;
        ZoomSelForm.Top:=Top+Height-ZoomSelForm.Height+15;
        ZoomSelForm.FocusZoom(TEERForm(ActiveMDIChild).EERModel.GetZoomFac);
        if(ZoomSelForm.ShowModal=mrOK)then
          TEERForm(ActiveMDIChild).EERModel.SetZoomFac(ZoomSelForm.ZoomFac);
      finally
        ZoomSelForm.Free;
      end;

      SetFocus;
    end;
  end;
end;

procedure TMainForm.ExitMIClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AboutMIClick(Sender: TObject);
begin
  {SplashForm:=TSplashForm.Create(self);
  try
    SplashForm.VersionLbl.Caption:=Version;
    SplashForm.ShowModal;
  finally
    //SplashForm.Free;
  end; }
  DMMain.BrowsePage('https://sourceforge.net/projects/dbdesigner-fork/');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //WindowState:=wsMaximized;
  DMMain.RestoreWinPos(self, True);
end;

procedure TMainForm.SaveAsMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      TEERForm(ActiveMDIChild).SaveAs;
    end;
end;

procedure TMainForm.SaveMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      if(TEERForm(ActiveMDIChild).EERModel.ModelFilename='')or(CompareText(Copy(TEERForm(ActiveMDIChild).EERModel.ModelFilename, 1, Length('Noname')), 'Noname')=0)then
        SaveAsMIClick(self)
      else
      begin
        TEERForm(ActiveMDIChild).EERModel.SaveToFile(TEERForm(ActiveMDIChild).EERModel.ModelFilename);

        TEERForm(ActiveMDIChild).EERModel.IsChanged:=False;
        DisableSaveImgs;

        DMGUI.SetStatusCaption(DMMain.GetTranslatedMessage('The model was successfully saved to %s.', 198, TEERForm(ActiveMDIChild).EERModel.ModelFilename));
      end;
    end;
end;

procedure TMainForm.OpenFile(fname: string; AppendModel: Boolean);
var theEERForm: TEERForm;
  theModelStr: TStringList;
begin
  if(Not(FileExists(fname)))then
    raise EInOutError.Create(DMMain.GetTranslatedMessage('File not found:', 11)+#13#10+
      fname);

  if(AppendModel)and(ActiveMDIChild<>nil)then
  begin
    theEERForm:=TEERForm(ActiveMDIChild);

    //Get new IDs for objects in the file
    theModelStr:=TStringList.Create;
    try
      theModelStr.LoadFromFile(fname);
      theModelStr.Text:=DMEER.AssignNewIDsToEERObjects(theModelStr.Text);

      //Create temp file
      fname:=DMMain.SettingsPath+'plugin_tmp.xml';
      DeleteFile(fname);
      theModelStr.SaveToFile(fname);
    finally
      theModelStr.Free;
    end;
  end
  else
  begin
    theEERForm:=nil;

    if(ActiveMDIChild<>nil)then
      if(ActiveMDIChild.Classname='TEERForm')then
        if(CompareText(Copy(TEERForm(ActiveMDIChild).EERModel.ModelFilename, 1, 6), 'Noname')=0)and
          (Not(TEERForm(ActiveMDIChild).EERModel.IsChanged))then
          theEERForm:=TEERForm(ActiveMDIChild);

    if(theEERForm=nil)then
      theEERForm:=TEERForm(NewEERModel);
  end;

  theEERForm.EERModel.LoadFromFile(fname, Not(((AppendModel)and(ActiveMDIChild<>nil))), ((AppendModel)and(ActiveMDIChild<>nil)),
    Not(((AppendModel)and(ActiveMDIChild<>nil))), ((AppendModel)and(ActiveMDIChild<>nil)));

  //Add file to recentopenfiles list
  if(Not((AppendModel)and(ActiveMDIChild<>nil)))then
  begin
    DMGUI.AddFileToRecentFilesList(fname);
  end;

  //Delete temp file if the model was appended
  if(AppendModel)and(ActiveMDIChild<>nil)then
    DeleteFile(fname);

  theEERForm.WindowState:=wsMaximized;

  //Because of Delphi Bug -> MainMenu is aligned right
  //Refresh Menu (because of CLX bug)
  {Menu:=nil;
  Menu:=MainMenu;}
end;

procedure TMainForm.DisplayEntityLevelMIClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:=True;
  DMEER.DisplayMode:=TMenuItem(Sender).Tag;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      TEERForm(ActiveMDIChild).EERModel.Refresh;
end;

procedure TMainForm.PhysicalSchemaLevelMIClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:=Not(TMenuItem(Sender).Checked);
  DMEER.DisplayPhysicalSchema:=TMenuItem(Sender).Checked;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      TEERForm(ActiveMDIChild).EERModel.Refresh;
end;

procedure TMainForm.NewMIClick(Sender: TObject);
var theForm: TForm;
begin
  theForm:=NewEERModel;

  theForm.WindowState:=wsMaximized;
end;

function TMainForm.NewEERModel: TForm;
var theEERForm: TEERForm;
begin
  theEERForm:=TEERForm.Create(self);

  //theEERForm.WindowState:=wsMaximized;
  AddToMDIWindowMenu(theEERForm);

  NewEERModel:=theEERForm;
end;

procedure TMainForm.AddToMDIWindowMenu(theForm: TForm);
var theMenuItem: TMenuItem;
  i: integer;
begin
  //Create the ChildForms MenuItem in the Window Menu
  for i:=0 to WindowsMI.Count-1 do
    if(WindowsMI.Items[i].GroupIndex=59)then
      WindowsMI.Items[i].Checked:=False;

  theMenuItem:=TMenuItem.Create(self);
  theMenuItem.Name:=theForm.Name+'MI';
  theMenuItem.Caption:=TEERForm(theForm).EERModel.GetModelName;
  theMenuItem.GroupIndex:=59;
  theMenuItem.RadioItem:=True;
  theMenuItem.Checked:=True;
  theMenuItem.OnClick:=MDIWindowMIClick;

  TEERForm(theForm).theFormMenuItem:=theMenuItem;

  WindowsMI.Add(theMenuItem);
end;

procedure TMainForm.MDIWindowMIClick(Sender: TObject);
var i: integer;
begin
  //Set Focus to selected ChildForm
  for i:=0 to WindowsMI.Count-1 do
    if(WindowsMI.Items[i].GroupIndex=59)then
      WindowsMI.Items[i].Checked:=False;

  for i:=0 to MDIChildCount-1 do
    if(TEERForm(MDIChildren[I]).theFormMenuItem=Sender)then
      TEERForm(MDIChildren[I]).SetFocus;

  TMenuItem(Sender).Checked:=True;
end;

procedure TMainForm.CascadeMIClick(Sender: TObject);
begin
  Cascade;
end;

procedure TMainForm.TileMIClick(Sender: TObject);
begin
  Tile;
end;

procedure TMainForm.ToolsMIClick(Sender: TObject);
begin
  ToolsMI.Checked:=Not(ToolsMI.Checked);
  PaletteToolsForm.Visible:=ToolsMI.Checked;
end;

procedure TMainForm.ToolsdockedMIClick(Sender: TObject);
begin
  ToolsdockedMI.Checked:=Not(ToolsdockedMI.Checked);
  ToolsPnl.Visible:=ToolsdockedMI.Checked;
end;

procedure TMainForm.DatatypesMIClick(Sender: TObject);
begin
  DatatypesMI.Checked:=Not(DatatypesMI.Checked);
  PaletteDataTypesForm.Visible:=DatatypesMI.Checked;
end;

procedure TMainForm.DBModelMIClick(Sender: TObject);
begin
  DBModelMI.Checked:=Not(DBModelMI.Checked);
  PaletteModelFrom.Visible:=DBModelMI.Checked;
end;

procedure TMainForm.CloseMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      ActiveMDIChild.Close;
end;

procedure TMainForm.EERModelOptionsMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      OptionsModelForm:=TOptionsModelForm.Create(self);
      try
        OptionsModelForm.SetModel(TEERForm(ActiveMDIChild).EERModel);
        OptionsModelForm.ShowModal;
      finally
        OptionsModelForm.Free;
      end;
    end;
end;

procedure TMainForm.DeleteMIClick(Sender: TObject);
var ObjectList: TList;
  i, j: integer;
  s: string;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      ObjectList:=TList.Create;
      try
        TEERForm(ActiveMDIChild).EERModel.GetEERObjectList([EERAllObjects], ObjectList, True);

        //Add ALL relations from tables
        for i:=0 to ObjectList.Count-1 do
          if(TEERObj(ObjectList[i]) is TEERTable)then
          begin
            //Add starting Relations
            for j:=0 to TEERTable(ObjectList[i]).RelStart.Count-1 do
              if(ObjectList.IndexOf(TEERTable(ObjectList[i]).RelStart[j])=-1)then
                ObjectList.Add(TEERTable(ObjectList[i]).RelStart[j]);

            //Add ending Relations
            for j:=0 to TEERTable(ObjectList[i]).RelEnd.Count-1 do
              if(ObjectList.IndexOf(TEERTable(ObjectList[i]).RelEnd[j])=-1)then
                ObjectList.Add(TEERTable(ObjectList[i]).RelEnd[j]);

          end;

        TEERForm(ActiveMDIChild).EERModel.SortEERObjectListByObjName(ObjectList);

        s:='';
        for i:=0 to ObjectList.Count-1 do
          s:=s+TEERObj(ObjectList[i]).ObjName+', ';

        s:=Copy(s, 1, Length(s)-2);

        if(MessageDlg('Are you sure you want to delete the selected Objects?'+#13#10+
          'The following Objects will be deleted:'+#13#10#13#10+
          s, mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
          TEERForm(ActiveMDIChild).EERModel.DeleteSelectedObjs;
      finally
        ObjectList.Free;
      end;
    end;
end;

procedure TMainForm.DeleteMIShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=False;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      if(TEERForm(ActiveMDIChild).EERModel.GetSelectedObjsCount>0)then
        TMenuItem(Sender).Enabled:=True;

end;

procedure TMainForm.NotationStandardMIClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:=Not(TMenuItem(Sender).Checked);
  DMEER.Notation:=TMenuItem(Sender).Tag;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      TEERForm(ActiveMDIChild).EERModel.SetZoomFac(TEERForm(ActiveMDIChild).EERModel.GetZoomFac);
      TEERForm(ActiveMDIChild).EERModel.Refresh;
    end;
end;

procedure TMainForm.SetMenuItemChecks;
begin
  case DMEER.DisplayMode of
    1: DisplayEntityLevelMI.Checked:=True;
    2: DisplayPrimaryKeyLevelMI.Checked:=True;
    3: DisplayAttributeLevelMI.Checked:=True;
  end;

  PhysicalSchemaLevelMI.Checked:=DMEER.DisplayPhysicalSchema;

  case DMEER.Notation of
    1: NotationStandardMI.Checked:=True;
    2: NotationErwinMI.Checked:=True;
    3: NotationStandard2MI.Checked:=True;
    4: NotationCrowsFootMI.Checked:=True;
  end;

  DisplayRelationNamesMI.Checked:=DMEER.DisplayRelationNames;

  ListTableIndicesMI.Checked:=DMEER.DisplayTableIndices;

end;

procedure TMainForm.DBDesignerOptionsMIClick(Sender: TObject);
begin
  OptionsForm:=TOptionsForm.Create(self);
  try
    OptionsForm.ShowModal;
  finally
    OptionsForm.Free;
  end;
end;

procedure TMainForm.DisplayRelationNamesMIClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:=Not(TMenuItem(Sender).Checked);
  DMEER.DisplayRelationNames:=TMenuItem(Sender).Checked;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      TEERForm(ActiveMDIChild).EERModel.Refresh;
end;

procedure TMainForm.ResetPalettePositionsMIClick(Sender: TObject);
begin
  PaletteToolsForm.Top:=60;
  PaletteToolsForm.Left:=8;

  PaletteNavForm.Width:=221;
  PaletteNavForm.Height:=173;
  PaletteDataTypesForm.Width:=221;
  PaletteDataTypesForm.Height:=229;
  PaletteModelFrom.Width:=221;
  PaletteModelFrom.Height:=192;

  PaletteNavForm.Top:=45;
  PaletteNavForm.Left:=Left+Width-PaletteNavForm.Width-25;

  PaletteDataTypesForm.Top:=244;
  PaletteDataTypesForm.Left:=Left+Width-PaletteDataTypesForm.Width-25;

  PaletteModelFrom.Top:=499;
  PaletteModelFrom.Left:=Left+Width-PaletteModelFrom.Width-25;

  DMMain.SaveWinPos(PaletteToolsForm, False);
  DMMain.SaveWinPos(PaletteNavForm, False);
  DMMain.SaveWinPos(PaletteDataTypesForm, False);
  DMMain.SaveWinPos(PaletteModelFrom, False);
end;

procedure TMainForm.PageSetupMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      HidePalettes;

      EERPageSetupForm:=TEERPageSetupForm.Create(self);
      try
        EERPageSetupForm.SetModel(TEERForm(ActiveMDIChild).EERModel);
        EERPageSetupForm.ShowModal;
      finally
        EERPageSetupForm.Free;
      end;

      ShowPalettes;

      SetFocus;
    end;
end;

procedure TMainForm.NavigatorInfoMIClick(Sender: TObject);
begin
  NavigatorInfoMI.Checked:=Not(NavigatorInfoMI.Checked);
  PaletteNavForm.Visible:=NavigatorInfoMI.Checked;
end;

procedure TMainForm.SaveModelasImageMIClick(Sender: TObject);
var ModelBmp: TBitmap;
  theSaveDialog: TSaveDialog;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      theSaveDialog:=TSaveDialog.Create(nil);
      try
{$IFDEF MSWINDOWS}
        //On Windows use native Win32 Open Dlg
        theSaveDialog.UseNativeDialog:=True;
        theSaveDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
{$ENDIF}

        theSaveDialog.Title:=DMMain.GetTranslatedMessage('Save Model As ...', 12);
        theSaveDialog.Width:=600;
        theSaveDialog.Height:=450;
        theSaveDialog.DefaultExt:='bmp';

        if(DirectoryExists(DMGUI.RecentSaveModelAsImageDir))then
          theSaveDialog.InitialDir:=DMGUI.RecentSaveModelAsImageDir
        else
          theSaveDialog.InitialDir:='';

        theSaveDialog.Filter:=DMMain.GetTranslatedMessage('PNG files (*.png)|*.png|Bitmap files (*.bmp)|*.bmp', 13);

        if(theSaveDialog.Execute)then
        begin
          DMGUI.RecentSaveModelAsImageDir:=ExtractFilePath(theSaveDialog.Filename);

          if(FileExists(theSaveDialog.Filename))then
            if(MessageDlg(DMMain.GetTranslatedMessage('The file [%s] '+
              'already exists. '#13#10+
              'Do you want to overwrite this file?', 14, ExtractFileName(theSaveDialog.Filename)), mtInformation,
              [mbYes, mbNo], 0)=mrNo)then
              Exit;


          //Save File
          ModelBmp:=TBitmap.Create;
          try
            TEERForm(ActiveMDIChild).EERModel.PaintModelToImage(ModelBmp, (TMenuItem(Sender).Tag=2));

            //Use this function to save PNGs, too
            DMMain.SaveBitmap(ModelBmp.Handle, theSaveDialog.Filename, ExtractFileExt(theSaveDialog.Filename));
          finally
            ModelBmp.Free;
          end;
        end;
      finally
        theSaveDialog.Free;
      end;
    end;
end;

procedure TMainForm.ShowForeignKeysMIClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:=Not(TMenuItem(Sender).Checked);
  DMEER.ShowForeignKeys:=TMenuItem(Sender).Checked;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      TEERForm(ActiveMDIChild).EERModel.Refresh;
end;

procedure TMainForm.DisplayPageGridMIClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:=Not(TMenuItem(Sender).Checked);
  DMEER.DisplayPaperGrid:=TMenuItem(Sender).Checked;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      TEERForm(ActiveMDIChild).EERModel.GridPaintBox.Visible:=TMenuItem(Sender).Checked;
end;

procedure TMainForm.ListTableIndicesMIClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:=Not(TMenuItem(Sender).Checked);
  DMEER.DisplayTableIndices:=TMenuItem(Sender).Checked;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      TEERForm(ActiveMDIChild).EERModel.Refresh;
end;

procedure TMainForm.PrintMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      HidePalettes;

      EERPageSetupForm:=TEERPageSetupForm.Create(self);
      try
        EERPageSetupForm.SetModel(TEERForm(ActiveMDIChild).EERModel);
        EERPageSetupForm.HideEdits;
        EERPageSetupForm.ShowModal;
      finally
        EERPageSetupForm.Free;
      end;

      ShowPalettes;

      SetFocus;
    end;
end;

procedure TMainForm.SelectAllMIClick(Sender: TObject);
var i: integer;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      with TEERForm(ActiveMDIChild).EERModel do
      begin
        //Set Font for all EER-Objects
        for i:=ComponentCount-1 downto 0 do
          if(Components[I].Classparent=TEERObj)then
            TEERObj(Components[I]).SetSelected(True);
      end;

      TEERForm(ActiveMDIChild).EERModel.Refresh;
    end;
end;

procedure TMainForm.ActivateEERMIOnShow(Sender: TObject);
begin
  if(Sender.ClassnameIs('TMenuItem'))then
  begin
    TMenuItem(Sender).Enabled:=False;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      TMenuItem(Sender).Enabled:=True;
  end;
end;

procedure TMainForm.SQLCreateScriptMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      EERExportSQLScriptFrom:=TEERExportSQLScriptFrom.Create(self);
      try
        EERExportSQLScriptFrom.SetModel(TEERForm(ActiveMDIChild).EERModel);
        EERExportSQLScriptFrom.ShowModal;
      finally
        EERExportSQLScriptFrom.Free;
      end;
    end;
end;

procedure TMainForm.ReverseEngineeringMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      HidePalettes;

      EERReverseEngineeringForm:=TEERReverseEngineeringForm.Create(self);
      try
        if(EERReverseEngineeringForm.SetData(TEERForm(ActiveMDIChild).EERModel))then
          EERReverseEngineeringForm.ShowModal;
      finally
        EERReverseEngineeringForm.Free;

        ShowPalettes;
      end;
    end;

  SetFocus;
end;

procedure TMainForm.CopyMIClick(Sender: TObject);
var s, ctext: string;
  f: TextFile;
  i, anz: integer;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      if(TEERForm(ActiveMDIChild).EERModel.GetSelectedObjsCount>0)then
      begin
        s:='';

        TEERForm(ActiveMDIChild).EERModel.SaveToFile(
          DMMain.SettingsPath+'clipboard.xml',
          False, True, False);

        anz:=0;
        for i:=0 to TEERForm(ActiveMDIChild).EERModel.ComponentCount-1 do
        begin
          if(TEERForm(ActiveMDIChild).EERModel.Components[i].ClassParent=TEERObj)then
            if(TEERObj(TEERForm(ActiveMDIChild).EERModel.Components[i]).Selected)then
              inc(anz);
        end;

        AssignFile(f, DMMain.SettingsPath+'clipboard.xml');
        try
          Reset(f);

          ctext:='';
          while(Not(EOF(f)))do
          begin
            ReadLn(f, s);
            ctext:=ctext+s+#13#10;
          end;

          Clipboard.AsText:=ctext;
        finally
          CloseFile(f);
        end;

        DeleteFile(DMMain.SettingsPath+'clipboard.xml');

        DMGUI.SetStatusCaption(DMMain.GetTranslatedMessage('%s Object(s) copied to clipboard.', -1, IntToStr(anz)));
      end;
end;

procedure TMainForm.PasteMIClick(Sender: TObject);
var f: TextFile;
  ctext: string;
  i, anz: integer;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      if(Clipboard.AsText<>'')then
      begin
        //ctext:=Clipboard.AsText;

        ctext:=DMEER.AssignNewIDsToEERObjects(Clipboard.AsText, True);

        AssignFile(f, DMMain.SettingsPath+'clipboard.xml');
        try
          Rewrite(f);

          Write(f, ctext);
        finally
          CloseFile(f);
        end;

        for i:=0 to TEERForm(ActiveMDIChild).EERModel.ComponentCount-1 do
          if(TEERForm(ActiveMDIChild).EERModel.Components[i].ClassParent=TEERObj)then
            if(TEERObj(TEERForm(ActiveMDIChild).EERModel.Components[i]).Selected)then
              TEERObj(TEERForm(ActiveMDIChild).EERModel.Components[i]).SetSelected(False);

        //Load Clipboard, ignore Settings, Select
        TEERForm(ActiveMDIChild).EERModel.LoadFromFile(
          DMMain.SettingsPath+'clipboard.xml',
          False, True);

        anz:=0;
        for i:=0 to TEERForm(ActiveMDIChild).EERModel.ComponentCount-1 do
        begin
          if(TEERForm(ActiveMDIChild).EERModel.Components[i].ClassParent=TEERObj)then
            if(TEERTable(TEERForm(ActiveMDIChild).EERModel.Components[i]).Selected)then
              inc(anz);
        end;


        DeleteFile(DMMain.SettingsPath+'clipboard.xml');

        DMGUI.SetStatusCaption(DMMain.GetTranslatedMessage('%s Object(s) pasted from clipboard.', -1, IntToStr(anz)));
      end;
end;

procedure TMainForm.PasteMIShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=False;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      if(Copy(Clipboard.AsText, 1, 1)='<')then
        TMenuItem(Sender).Enabled:=True;
end;

procedure TMainForm.CutMIClick(Sender: TObject);
begin
  CopyMIClick(self);
  DeleteMIClick(self);
end;

procedure TMainForm.DatabasesyncronisationMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      if(TEERForm(ActiveMDIChild).EERModel.GetEERObjectCount([EERTable])=0)then
        MessageDlg(DMMain.GetTranslatedMessage('There are no tables in the model which can be syncronised.', 15),
          mtError, [mbOK], 0)
      else
      begin
        HidePalettes;

        EERSynchronisationForm:=TEERSynchronisationForm.Create(self);
        try
          if(EERSynchronisationForm.SetData(TEERForm(ActiveMDIChild).EERModel))then
            EERSynchronisationForm.ShowModal;
        finally
          EERSynchronisationForm.Free;
          ShowPalettes;
        end;
      end;
    end;

  SetFocus;
end;

procedure TMainForm.UndoMIShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=False;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      if(TEERForm(ActiveMDIChild).EERModel.CurrentAction>-1)then
      begin
        TMenuItem(Sender).Enabled:=True;
        TMenuItem(Sender).Caption:=DMMain.GetTranslatedMessage('Undo', 16)+' '+
          TEERForm(ActiveMDIChild).EERModel.GetActionName(
            TEERActionLog(TEERForm(ActiveMDIChild).EERModel.ActionLog[
              TEERForm(ActiveMDIChild).EERModel.CurrentAction]).ActionType);
      end
      else
      begin
        TMenuItem(Sender).Caption:=DMMain.GetTranslatedMessage('Undo', 16);
      end;
end;

procedure TMainForm.UndoMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      if(TEERForm(ActiveMDIChild).EERModel.CurrentAction>-1)then
        TEERForm(ActiveMDIChild).EERModel.UndoActions(TEERForm(ActiveMDIChild).EERModel.CurrentAction);
end;

procedure TMainForm.RedoMIShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=False;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      if(TEERForm(ActiveMDIChild).EERModel.CurrentAction<
        TEERForm(ActiveMDIChild).EERModel.ActionLog.Count-1)then
      begin
        TMenuItem(Sender).Enabled:=True;
        TMenuItem(Sender).Caption:=DMMain.GetTranslatedMessage('Redo', 17)+' '+
          TEERForm(ActiveMDIChild).EERModel.GetActionName(
            TEERActionLog(TEERForm(ActiveMDIChild).EERModel.ActionLog[
              TEERForm(ActiveMDIChild).EERModel.CurrentAction+1]).ActionType);
      end
      else
      begin
        TMenuItem(Sender).Caption:=DMMain.GetTranslatedMessage('Redo', 17);
      end;
end;

procedure TMainForm.RedoMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      if(TEERForm(ActiveMDIChild).EERModel.CurrentAction<
        TEERForm(ActiveMDIChild).EERModel.ActionLog.Count-1)then
        TEERForm(ActiveMDIChild).EERModel.RedoActions(TEERForm(ActiveMDIChild).EERModel.CurrentAction+1);
end;

procedure TMainForm.StyleStandardMIClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:=Not(TMenuItem(Sender).Checked);
  case TMenuItem(Sender).Tag of
    0:
      Application.Style.DefaultStyle:=dsWindows;
    1:
      Application.Style.DefaultStyle:=dsMotifPlus;
    2:
      Application.Style.DefaultStyle:=dsQtSGI;
    3:
      begin
        Application.Style.DefaultStyle:=dsWindows;
        Application.Style.DefaultStyle:=dsPlatinum;
      end;
  end;
end;

procedure TMainForm.SQLDropScriptMIClick(Sender: TObject);
begin
  EERExportSQLScriptFrom:=TEERExportSQLScriptFrom.Create(self);
  try
    EERExportSQLScriptFrom.SetModel(TEERForm(ActiveMDIChild).EERModel, 1);
    EERExportSQLScriptFrom.ShowModal;
  finally
    EERExportSQLScriptFrom.Free;
  end;
end;

procedure TMainForm.OnlinedocumentationMIClick(Sender: TObject);
begin
  DMMain.ShowHelp('start', '');
end;

procedure TMainForm.VisitHomepageMIClick(Sender: TObject);
begin
  DMMain.BrowsePage('http://www.fabforce.net');
end;

procedure TMainForm.CheckfornewversionsMIClick(Sender: TObject);
begin
  DMMain.BrowsePage('https://sourceforge.net/projects/dbdesigner-fork/');
end;

procedure TMainForm.SaveinDatabaseMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      HidePalettes;

      EERStoreInDatabaseForm:=TEERStoreInDatabaseForm.Create(self);
      try
        if(EERStoreInDatabaseForm.SetData(TEERForm(ActiveMDIChild).EERModel, True))then
          if(EERStoreInDatabaseForm.ShowModal=mrOk)then
          begin
            TEERForm(ActiveMDIChild).EERModel.IsChanged:=False;
            DisableSaveImgs;
          end;
      finally
        EERStoreInDatabaseForm.Free;
        ShowPalettes;
      end;
    end;

  SetFocus;
end;

procedure TMainForm.OpenfromDatabaseMIClick(Sender: TObject);
var theEERForm: TEERForm;
begin
  if(ActiveMDIChild=nil)then
  begin
    theEERForm:=TEERForm(NewEERModel);
  end
  else
  begin
    theEERForm:=nil;

    if(ActiveMDIChild<>nil)then
      if(ActiveMDIChild.Classname='TEERForm')then
        if(CompareText(Copy(TEERForm(ActiveMDIChild).EERModel.ModelFilename, 1, 6), 'Noname')=0)and
          (Not(TEERForm(ActiveMDIChild).EERModel.IsChanged))then
          theEERForm:=TEERForm(ActiveMDIChild);

    if(theEERForm=nil)then
      theEERForm:=TEERForm(NewEERModel);
  end;

  theEERForm.WindowState:=wsMaximized;

  //HidePalettes;

  EERStoreInDatabaseForm:=TEERStoreInDatabaseForm.Create(self);
  try
    if(EERStoreInDatabaseForm.SetData(theEERForm.EERModel, False))then
      EERStoreInDatabaseForm.ShowModal;
  finally
    EERStoreInDatabaseForm.Free;
    //ShowPalettes;
  end;

  theEERForm.SetFocus;
  sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RefreshPalettes, nil));

end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
//var i: integer;
begin
  CanClose:=True;

  {for i:=MDIChildCount-1 downto 0 do
  begin
    TEERForm(MDIChildren[I]).FormCloseQuery(self, CanClose);
    if(CanClose=false)then
      break;
  end;}
end;

procedure TMainForm.RefreshTmrTimer(Sender: TObject);
begin
  RefreshTmr.Enabled:=False;

  if(ActiveMDIChild<>nil)then
  begin
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      if(TEERForm(ActiveMDIChild).EERModel.IsChanged)then
        EnableSaveImgs
      else
        DisableSaveImgs;
    end;
  end
  else
  begin
    DisableSaveImgs;
    if(Assigned(PaletteNavForm))then
      PaletteNavForm.ClearModelImg;

    if(Assigned(PaletteModelFrom))then
      PaletteModelFrom.TablesTreeView.Items.Clear;
  end;
end;

procedure TMainForm.ShowPalettesTmrTimer(Sender: TObject);
var theEERForm: TForm;
  TipText: string;
  i: integer;
begin
  //Workaround for CLX Bug
  //Change Font Size to change Menu Font
  Font.Size:=Font.Size-1;
  Font.Size:=DMMain.ApplicationFontSize;

  //Refresh Menu (because of CLX bug)
  Menu:=nil;
  Menu:=MainMenu;

  if(DMGUI.ShowTipsOnStartup)then
  begin
    TipText:=DMGUI.LoadTip;

    if(TipText<>'')then
    begin
      TipsForm:=TTipsForm.Create(self);
      TipsForm.TipMemo.Text:=#13#10+TipText;
      TipsForm.Show;
    end;
  end;

  //Set DockPnls Height
  NavPnl.Height:=173+20+4;
  NavHeaderPnl.Font.Color:=$00AAAAAA;
  DatatypesPnl.Height:=229+20+4;
  DatatypesHeaderPnl.Font.Color:=$00AAAAAA;
  ModelHeaderPnl.Font.Color:=$00AAAAAA;


  ShowPalettesTmr.Enabled:=False;

  //Create Main Palette
  PaletteToolsForm:=TPaletteToolsForm.Create(self);
  PaletteToolsForm.Top:=50;
  PaletteToolsForm.Left:=15;

  DMEER.SetWorkTool(wtPointer);

  //Create NavInfoPalette
  PaletteNavForm:=TPaletteNavForm.Create(self);
  PaletteNavForm.Top:=44;
  PaletteNavForm.Left:=Screen.Width-PaletteNavForm.Width-25;


  //Create DatatypesPalette
  PaletteDataTypesForm:=TPaletteDataTypesForm.Create(self);
  PaletteDataTypesForm.Top:=60;
  PaletteDataTypesForm.Left:=Screen.Width-PaletteDataTypesForm.Width-25;


  //Create ModelPalette
  PaletteModelFrom:=TPaletteModelFrom.Create(self);
  PaletteModelFrom.Top:=380;
  PaletteModelFrom.Left:=Screen.Width-PaletteModelFrom.Width-25;


  if(PaletteNavForm.Left>Screen.Width)or
    (PaletteDataTypesForm.Left>Screen.Width)or
    (PaletteModelFrom.Left>Screen.Width)or
    (DMGUI.ApplRunFirstTime)then
    ResetPalettePositionsMIClick(self);

  {$IFDEF LINUX}
  //Current BUG with Linux Window Positions
  ResetPalettePositionsMIClick(self);
  {$ENDIF}

  //Dock Query Pnl
  DockedEditorQueryForm:=TEditorQueryForm.Create(self);
  with TEditorQueryForm(DockedEditorQueryForm) do
  begin
    QueryDockPnl.Parent:=QueryPnl;
    ShowSQLImgClick(self);

    //Make the Pnls wider
    {SQLPnl.Width:=450;
    StoredSQLPnl.Width:=210;}

    //Hide Right Pnl
    RightPnl.Visible:=False;
    LeftPnl.Width:=34;

    SetLayout(DMGUI.DockedQueryPnlMode, False);
  end;


  if(Not(DMGUI.ShowPalettesDocked))then
  begin
    PaletteNavForm.Show;
    PaletteDataTypesForm.Show;
    PaletteModelFrom.Show;
  end
  else
  begin
    NavigatorInfoMI.Checked:=False;
    DatatypesMI.Checked:=False;
    DBModelMI.Checked:=False;
      
    DockPalettesMIClick(self);
  end;

  {$IFDEF LINUX}
  //Don't show Tools Palette in Linux because of win-width bug
  ToolsMI.Visible:=False;
  {$ENDIF}

  ToolsMI.Checked:=False;
  ToolsdockedMI.Checked:=True;

  if(DMEER.InitWorkMode=2)then
    DesignimgClick(self);

  BuildPluginMenus;

  //Check all params if one of them is a existing file, then open it
  for i:=1 to ParamCount do
    if(Copy(ParamStr(i), 1, 1)<>'-')then
      if(FileExists(ParamStr(i)))then
      begin
        OpenFile(ParamStr(1), False);
        break;
      end;

  //Reopen last file when no para was specified
  if(ActiveMDIChild=nil)then
  begin
    if(DMGUI.ReopenLastFile)and(DMGUI.GetRecentFileCount>0)then
      OpenFile(DMGUI.GetRecentFileName(0), False)
    else
    begin
      //or create new
      theEERForm:=NewEERModel;
      theEERForm.WindowState:=wsMaximized;
    end;
  end;

  //-----------------------------------------
  //Workaround Code because of Delphi BUG
  {if(DMMain.NormalizeEditorForms)then
    DMMain.ApplicationActiveTimer.Enabled:=True;}
  //Workaround Code because of Delphi BUG END
  //-----------------------------------------

  //SplashForm.Close;

  Cursor:=crArrow;
end;

procedure TMainForm.wtPointerSBtnClick(Sender: TObject);
begin
  DMEER.SetWorkTool(TSpeedButton(Sender).Tag);
end;

procedure TMainForm.wtPointerSBtnMouseEnter(Sender: TObject);
begin
  if(TSpeedButton(Sender).Hint='')then
    if(TSpeedButton(Sender).Tag-1<DMEER.WorkToolLabelTexts.Count)then
      TSpeedButton(Sender).Hint:=DMEER.WorkToolLabelTexts[TSpeedButton(Sender).Tag-1];
end;

procedure TMainForm.DesignimgClick(Sender: TObject);
begin
  SetWorkMode(wmQuery);
end;

procedure TMainForm.QueryImgClick(Sender: TObject);
begin
  SetWorkMode(wmDesign);
end;

procedure TMainForm.SyncImgClick(Sender: TObject);
begin
  MainForm.DatabasesyncronisationMIClick(self);
end;

procedure TMainForm.DisableSaveImgs;
begin
  NotSavedImg.SendToBack;
  Save2DiskImg.SendToBack;
  Save2DBImg.SendToBack;
end;

procedure TMainForm.EnableSaveImgs;
begin
  NotSavedImg.BringToFront;
  Save2DiskImg.BringToFront;
  Save2DBImg.BringToFront;
end;

procedure TMainForm.Save2DiskImgClick(Sender: TObject);
begin
  SaveMIClick(self);
end;

procedure TMainForm.Save2DBImgClick(Sender: TObject);
begin
  SaveinDatabaseMIClick(self);
end;

function TMainForm.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
var Pint: ^Integer;
  Ppoint: ^TPoint;
  theStr: PChar;
  pos: integer;
  theModel: TEERModel;
  theEERObj: TEERObj;
begin
  //Catch UserDefined Msg
  Result := False;

  if(QEvent_type(Event)=QEventType_EditTable)then
  begin
    if(DMEER.WorkMode=wmDesign)then
    begin
      //Only one table editor at the same time
      EditorTableForm:=TEditorTableForm(DMMain.GetFormByName('EditorTableForm'));
      if(EditorTableForm=nil)then
        EditorTableForm:=TEditorTableForm.Create(self)
      else
        EditorTableForm.ApplyChanges;

      EditorTableForm.SetTable(QCustomEvent_data(QCustomEventH(Event)));
      EditorTableForm.ShowModal;
      EditorTableForm.BringToFront;
      //Set Focus because ToolWindows don't seem to be focused automatically
      try
        if(EditorTableForm.CanFocus)then
          EditorTableForm.SetFocus;
      except
      end;
    end
    else
    begin
      //Only one table editor at the same time
      EditorQueryForm:=TEditorQueryForm(DMMain.GetFormByName('EditorTableDataForm'));
      if(EditorQueryForm=nil)then
        EditorQueryForm:=TEditorQueryForm.Create(self)
      else
        EditorQueryForm.ApplyChanges;

      if(EditorQueryForm.SetTable(QCustomEvent_data(QCustomEventH(Event))))then
      begin
        EditorQueryForm.Show;
        EditorQueryForm.BringToFront;

        //Set Focus because ToolWindows don't seem to be focused automatically
        try
          if(EditorQueryForm.CanFocus)then
            EditorQueryForm.SetFocus;
        except
        end;
      end
      else
        EditorQueryForm.Free;
    end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_EditRel)then
  begin
    EditorRelationForm:=TEditorRelationForm.Create(self);

    EditorRelationForm.SetRelation(QCustomEvent_data(QCustomEventH(Event)));
    EditorRelationForm.ShowModal;
    EditorRelationForm.BringToFront;
    //Set Focus because ToolWindows don't seem to be focused automatically
    try
      if(EditorRelationForm.CanFocus)then
        EditorRelationForm.SetFocus;
    except
    end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_EditRegion)then
  begin
    EditorRegionForm:=TEditorRegionForm.Create(self);

    EditorRegionForm.SetRegion(QCustomEvent_data(QCustomEventH(Event)));
    EditorRegionForm.Show;
    EditorRegionForm.BringToFront;
    //Set Focus because ToolWindows don't seem to be focused automatically
    try
      if(EditorRegionForm.CanFocus)then
        EditorRegionForm.SetFocus;
    except
    end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_EditNote)then
  begin
    EditorNoteForm:=TEditorNoteForm.Create(Application.MainForm);

    EditorNoteForm.SetNote(QCustomEvent_data(QCustomEventH(Event)));
    EditorNoteForm.Show;
    EditorNoteForm.BringToFront;
    //Set Focus because ToolWindows don't seem to be focused automatically
    try
      if(EditorNoteForm.CanFocus)then
        EditorNoteForm.SetFocus;
    except
    end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_EditImage)then
  begin
    EditorImageForm:=TEditorImageForm.Create(Application.MainForm);

    EditorImageForm.SetImage(QCustomEvent_data(QCustomEventH(Event)));
    EditorImageForm.Show;
    EditorImageForm.BringToFront;
    //Set Focus because ToolWindows don't seem to be focused automatically
    try
      if(EditorImageForm.CanFocus)then
        EditorImageForm.SetFocus;
    except
    end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_SetWorkTool)then
  begin
    Pint:=QCustomEvent_data(QCustomEventH(Event));
    DisplaySelectedWorkTool(Pint^);

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_RefreshPalettes)then
  begin
    if(ActiveMDIChild<>nil)then
      if(ActiveMDIChild.Classname='TEERForm')then
      begin
        PaletteModelFrom.RefreshTablesTreeView(TEERForm(ActiveMDIChild).EERModel);
        PaletteDataTypesForm.DisplayDataTypes(TEERForm(ActiveMDIChild).EERModel);
        PaletteNavForm.SetModelImg(TEERForm(ActiveMDIChild).EERModel);
        TEditorQueryForm(DockedEditorQueryForm).RefreshStoredSQLTreeView(TEERForm(ActiveMDIChild).EERModel);
        TEditorQueryForm(DockedEditorQueryForm).RefreshTempSQLStoreBtns(TEERForm(ActiveMDIChild).EERModel);
        SnapToGridBtn.Down:=TEERForm(ActiveMDIChild).EERModel.UsePositionGrid;
      end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_RedrawTableList)then
  begin
    if(ActiveMDIChild<>nil)then
      if(ActiveMDIChild.Classname='TEERForm')then
      begin
        PaletteModelFrom.TablesTreeView.Repaint;
      end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_RefreshNavPalette)then
  begin
    if(QCustomEvent_data(QCustomEventH(Event))<>nil)then
      PaletteNavForm.SetModelImg(TEERModel(QCustomEvent_data(QCustomEventH(Event))))
    else if(ActiveMDIChild<>nil)then
      if(ActiveMDIChild.Classname='TEERForm')then
      begin
        PaletteNavForm.SetModelImg(TEERForm(ActiveMDIChild).EERModel);
      end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_RefreshGridBtn)then
  begin
    if(ActiveMDIChild<>nil)then
      if(ActiveMDIChild.Classname='TEERForm')then
        SnapToGridBtn.Down:=TEERForm(ActiveMDIChild).EERModel.UsePositionGrid;

    Result:=True;

  end;

  if(QEvent_type(Event)=QEventType_ClearNavImg)then
  begin
    PaletteNavForm.SetModelImg(nil);

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_RefreshInfoPalette)then
  begin
    if(ActiveMDIChild<>nil)then
      if(ActiveMDIChild.Classname='TEERForm')then
        PaletteNavForm.RefreshInfo(TEERForm(ActiveMDIChild).EERModel);

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_UpdateStatusBar)then
  begin
    if(ActiveMDIChild<>nil)then
      if(ActiveMDIChild.Classname='TEERForm')then
      begin
        ZoomLbl.Caption:=
          FormatFloat('###0.##', TEERForm(ActiveMDIChild).EERModel.GetZoomFac)+' %';

        PaletteNavForm.RefreshZoomSettings;
      end
      else
        MainForm.ZoomLbl.Caption:='';

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_SetStatusCaption)then
  begin
    theStr:=QCustomEvent_data(QCustomEventH(Event));
    DMGUI.SetStatusCaption(theStr);

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_SetSaveImgs)then
  begin
    if(ActiveMDIChild<>nil)then
      if(ActiveMDIChild.Classname='TEERForm')then
      begin
        PaletteModelFrom.RefreshTablesTreeView(TEERForm(ActiveMDIChild).EERModel);
        PaletteDataTypesForm.DisplayDataTypes(TEERForm(ActiveMDIChild).EERModel);
        PaletteNavForm.SetModelImg(TEERForm(ActiveMDIChild).EERModel);
        TEditorQueryForm(DockedEditorQueryForm).RefreshStoredSQLTreeView(TEERForm(ActiveMDIChild).EERModel);
        TEditorQueryForm(DockedEditorQueryForm).RefreshTempSQLStoreBtns(TEERForm(ActiveMDIChild).EERModel);
        if(TEERForm(ActiveMDIChild).EERModel.IsChanged)then
          EnableSaveImgs
        else
          DisableSaveImgs;
      end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_RefreshDataTypesPalette)then
  begin
    if(Assigned(PaletteDataTypesForm))then
      PaletteDataTypesForm.DisplayDataTypes(QCustomEvent_data(QCustomEventH(Event)));

   Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_RefreshModelPalette)then
  begin
    if(Assigned(PaletteModelFrom))then
      PaletteModelFrom.RefreshTablesTreeView(QCustomEvent_data(QCustomEventH(Event)));

    //Clear StoredSQLTree when model is closed
    TEditorQueryForm(DockedEditorQueryForm).RefreshStoredSQLTreeView(QCustomEvent_data(QCustomEventH(Event)));
    TEditorQueryForm(DockedEditorQueryForm).RefreshTempSQLStoreBtns(QCustomEvent_data(QCustomEventH(Event)));
    
    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_EnableMainFormRefreshTmr)then
  begin
    RefreshTmr.Enabled:=True;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_RemoveChildFormsMenuItem)then
  begin
    pos:=WindowsMI.IndexOf(QCustomEvent_data(QCustomEventH(Event)));
    WindowsMI.Delete(pos);
    TMenuItem(QCustomEvent_data(QCustomEventH(Event))).Free;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_SetQueryStatusLbl)then
  begin
    DMGUI.SetQueryStatusLbl;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_CloseAllClientDatasets)then
  begin
    //Close Datasets in DockedEditorQueryForm
    if(TEditorQueryForm(DockedEditorQueryForm).OutputQry.Active)then
      TEditorQueryForm(DockedEditorQueryForm).OutputQry.Close;

    if(TEditorQueryForm(DockedEditorQueryForm).OutputClientDataSet.Active)then
    begin
      TEditorQueryForm(DockedEditorQueryForm).OutputClientDataSet.Close;
      TEditorQueryForm(DockedEditorQueryForm).DBGrid.Columns.Clear;
      TEditorQueryForm(DockedEditorQueryForm).DBMemo.DataField:='';
      TEditorQueryForm(DockedEditorQueryForm).DBImage.DataField:='';
    end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_AddToRecentFileList)then
  begin
    theStr:=QCustomEvent_data(QCustomEventH(Event));
    DMGUI.AddFileToRecentFilesList(theStr);

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_StartEERObjectDrag)then
  begin
    if(TObject(QCustomEvent_data(QCustomEventH(Event))).ClassNameIs('TEERTable'))then
    begin
      //EditorQueryDragTargetForm:=TEditorQueryDragTargetForm.Create(self);
      //TEditorQueryDragTargetForm(EditorQueryDragTargetForm).SetModel(TEERModel(TEERTable(QCustomEvent_data(QCustomEventH(Event))).Parent), DMEER.ActiveSQLColumnDragPositions);
      EditorQueryDragTargetForm.Show;

      DMEER.IsDragging:=True;
    end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_EndEERObjectDrag)then
  begin
    if(TObject(QCustomEvent_data(QCustomEventH(Event))).ClassNameIs('TEERTable'))then
    begin
      if(Assigned(EditorQueryDragTargetForm))then
        EditorQueryDragTargetForm.Hide;//.Free;

      DMEER.IsDragging:=False;
    end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_SelectSQLColumnFromTable)then
  begin
    Pint:=QCustomEvent_data(QCustomEventH(Event));
    CopyMouseOverObjToSQLMemo(Pint^);

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_ModelNameChanged)then
  begin
    //Catch exception in plugins
    try
      if(TEERModel(QCustomEvent_data(QCustomEventH(Event))).Parent.ClassNameIs('TEERForm'))then
      begin
        //Set MenuItem caption
        if(TEERForm(TEERModel(QCustomEvent_data(QCustomEventH(Event))).Parent).theFormMenuItem<>nil)then
        begin
          TEERForm(TEERModel(QCustomEvent_data(QCustomEventH(Event))).Parent).theFormMenuItem.Caption:=
            TEERModel(QCustomEvent_data(QCustomEventH(Event))).GetModelName;
        end;

        TEERForm(TEERModel(QCustomEvent_data(QCustomEventH(Event))).Parent).Caption:=
          DMMain.GetTranslatedMessage('DB Model', 18)+' | '+
          TEERModel(QCustomEvent_data(QCustomEventH(Event))).GetModelName;
      end;
    except
    end;

    Result:=True;
  end;

  if(QEvent_type(Event)=QEventType_DeleteObject)then
  begin
    if(TObject(QCustomEvent_data(QCustomEventH(Event))).ClassParent = TEERObj)then
    begin
      theModel:=TEERObj(QCustomEvent_data(QCustomEventH(Event))).ParentEERModel;
      theEERObj:=TEERObj(QCustomEvent_data(QCustomEventH(Event)));

      theModel.StartSubActionLog(at_DeleteObj);
      theModel.LogActions:=True;
      try
        theEERObj.DeleteObj;
      finally
        theModel.LogActions:=False;
        theModel.EndSubAction;
      end;
    end;

    Result:=True;
  end;


  if(QEvent_type(Event)=QEventType_PlaceModelFromFile)or
    (QEvent_type(Event)=QEventType_PlaceModelFromDB)or
    (QEvent_type(Event)=QEventType_PlaceModelFromLibrary)then
  begin
    Ppoint:=QCustomEvent_data(QCustomEventH(Event));

    if(ActiveMDIChild<>nil)then
      if(ActiveMDIChild.Classname='TEERForm')then
      begin
        if(QEvent_type(Event)=QEventType_PlaceModelFromFile)then
          PlaceModel(TEERForm(ActiveMDIChild).EERModel, Ppoint^, PlaceFromFile)
        else if(QEvent_type(Event)=QEventType_PlaceModelFromDB)then
          PlaceModel(TEERForm(ActiveMDIChild).EERModel, Ppoint^, PlaceFromDB)
        else if(QEvent_type(Event)=QEventType_PlaceModelFromLibrary)then
          PlaceModel(TEERForm(ActiveMDIChild).EERModel, Ppoint^, PlaceFromLibrary);
      end;

    Result:=True;
  end;

  //if this was called by another message, call default function
  if(Result=False)then
    EventFilter:=inherited EventFilter(Sender, Event);
end;

procedure TMainForm.SetApplStyle(ApplStyle: integer);
begin
{$IFNDEF USE_QTheming}
  case ApplStyle of
    1: StyleStandardMIClick(StyleStandardMI);
    2: StyleStandardMIClick(StyleMotifMI);
    3: StyleStandardMIClick(StyleSGIMI);
    4: StyleStandardMIClick(StylePlatinumMI);
  end;
{$ENDIF}
end;

procedure TMainForm.SetWorkMode(theWorkMode: integer);
begin
  DMEER.WorkMode:=theWorkMode;
  DMEER.SetWorkTool(wtPointer);

  if(theWorkMode=wmQuery)then
  begin
    MainForm.QueryImg.Show;
    MainForm.QueryImg.BringToFront;
    MainForm.Designimg.Hide;

    DesignToolsPnl.Hide;
    QueryToolsPnl.Show;
    QueryToolsPnl.Top:=1;

    PaletteToolsForm.QueryImg.BringToFront;

    if(Not(DMGUI.ShowPalettesDocked))then
    begin
      if(PaletteDataTypesForm.Visible)then
      begin
        MainForm.DatatypesMIClick(self);
        if(PaletteModelFrom.Top>PaletteDataTypesForm.Top+
          PaletteDataTypesForm.Height-30)then
          PaletteModelFrom.Top:=PaletteDataTypesForm.Top;
      end;
    end
    else
    begin
      DatatypesPnl.Visible:=False;
    end;
    
    //QueryPnl.Height:=226;
    QueryPnl.Show;
    QuerySplitter.Show;

    if(DMDB.OutputQry.Active)then
      DMDB.OutputQry.Close;
    if(DMDB.OutputClientDataSet.Active)then
      DMDB.OutputClientDataSet.Close;
  end
  else if(theWorkMode=wmDesign)then
  begin
    MainForm.Designimg.Show;
    MainForm.Designimg.BringToFront;
    MainForm.QueryImg.Hide;
    PaletteToolsForm.Designimg.BringToFront;

    DesignToolsPnl.Show;
    DesignToolsPnl.Top:=1;
    QueryToolsPnl.Hide;

    MainForm.QueryPnl.Hide;
    MainForm.QuerySplitter.Hide;

    if(Not(DMGUI.ShowPalettesDocked))then
    begin
      if(Not(PaletteDataTypesForm.Visible))and
        (TabHidePalettes.Count=0)then
      begin
        MainForm.DatatypesMIClick(self);

        if(PaletteModelFrom.Top=PaletteDataTypesForm.Top)then
          PaletteModelFrom.Top:=PaletteDataTypesForm.Top+
            PaletteDataTypesForm.Height+27;
      end;
    end
    else
    begin
      DatatypesPnl.Visible:=True;
      DatatypesPnl.Top:=74;
    end;
  end;

  StatusPnl.Top:=Height+10;
end;

procedure TMainForm.DisplaySelectedWorkTool(WorkTool: integer);
begin
  //Do actions before changing tool
  if(MainForm.MDIChildCount>0)then
    if(MainForm.ActiveMDIChild.Classname='TEERForm')then
      //Reset Rel_SrcTable if
      if(not((DMEER.CurrentWorkTool=wtRel1n)or(DMEER.CurrentWorkTool=wtRel1nSub)or(DMEER.CurrentWorkTool=wtRel11)))then
         TEERForm(MainForm.ActiveMDIChild).EERModel.Rel_SrcTable:=nil;

  //Display selected WorkTool
  with PaletteToolsForm do
  begin
    case WorkTool of
      wtPointer:
          wtPointerSBtn.Down:=True;
      wtMove:
          wtMoveSBtn.Down:=True;
      wtDelete:
          wtDeleteSBtn.Down:=True;
      wtSize:
          wtSizeSBtn.Down:=True;

      wtRegion:
          wtRegionSBtn.Down:=True;
      wtTable:
          wtTableSBtn.Down:=True;


      wtRel1n:
          wtRel1nSBtn.Down:=True;
      wtRel1nSub:
          wtRel1nSubSBtn.Down:=True;
      wtRel11:
          wtRel11SBtn.Down:=True;
      wtRelnm:
          wtRelnmSBtn.Down:=True;
      wtRel11Sub:
          wtRel11SubSBtn.Down:=True;

      wtNote:
          wtRegionSBtn.Down:=True;
      wtImage:
          wtImageSBtn.Down:=True;

      wtHand:
          wtHandSBtn.Down:=True;
      wtZoomIn:
          wtZoomSBtn.Down:=True;
      wtZoomOut:
          wtZoomSBtn.Down:=True;
    end;
  end;

  with MainForm do
  begin
    case WorkTool of
      wtPointer:
      begin
          wtPointerSBtn.Down:=True;
          wtPointerQrySBtn.Down:=True;
      end;
      wtMove:
      begin
          wtMoveSBtn.Down:=True;
          wtMoveQrySBtn.Down:=True;
      end;
      wtDelete:
          wtDeleteSBtn.Down:=True;
      wtSize:
          wtSizeSBtn.Down:=True;

      wtRegion:
          wtRegionSBtn.Down:=True;
      wtTable:
          wtTableSBtn.Down:=True;


      wtRel1n:
          wtRel1nSBtn.Down:=True;
      wtRel1nSub:
          wtRel1nSubSBtn.Down:=True;
      wtRel11:
          wtRel11SBtn.Down:=True;
      wtRelnm:
          wtRelnmSBtn.Down:=True;
      wtRel11Sub:
          wtRel11SubSBtn.Down:=True;

      wtNote:
          wtRegionSBtn.Down:=True;
      wtImage:
          wtImageSBtn.Down:=True;

      wtHand:
      begin
          wtHandSBtn.Down:=True;
          wtHandQrySBtn.Down:=True;
      end;
      wtZoomIn:
      begin
          wtZoomSBtn.Down:=True;
          wtZoomQrySBtn.Down:=True;
      end;
      wtZoomOut:
      begin
          wtZoomSBtn.Down:=True;
          wtZoomQrySBtn.Down:=True;
      end;

      wtSQLSelect:
        wtSQLSelectSBtn.Down:=True;
      wtSQLFrom:
        wtSQLFromSBtn.Down:=True;
      wtSQLWhere:
        wtSQLWhereSBtn.Down:=True;
      wtSQLGroup:
        wtSQLGroupSBtn.Down:=True;
      wtSQLHaving:
        wtSQLHavingSBtn.Down:=True;
      wtSQLOrder:
        wtSQLOrderSBtn.Down:=True;
      wtSQLSet:
        wtSQLSetSBtn.Down:=True;
    end;
  end;
end;

procedure TMainForm.ApplicationRestore(Sender: TObject);
begin
  DMMain.RestoreWinPos(PaletteToolsForm, False);
  DMMain.RestoreWinPos(PaletteNavForm, False);
  DMMain.RestoreWinPos(PaletteDataTypesForm, False);
  DMMain.RestoreWinPos(PaletteModelFrom, False);
end;

procedure TMainForm.HidePalettes;
begin
  if(TabHasBeenPressed)then
    Exit;

  if(DMGUI.ShowPalettesDocked)then
  begin
    if(ToolsPnl.Visible=True)then
      ToolsdockedMIClick(self);

    PaletteDockPnl.Visible:=False;
  end
  else
  begin
    DMMain.SaveWinPos(PaletteToolsForm, False);
    DMMain.SaveWinPos(PaletteNavForm, False);
    DMMain.SaveWinPos(PaletteDataTypesForm, False);
    DMMain.SaveWinPos(PaletteModelFrom, False);

    TabHidePalettes.Clear;
    if(PaletteToolsForm.Visible)then
    begin
      TabHidePalettes.Add(PaletteToolsForm);
      ToolsMIClick(self);
    end;
    if(ToolsPnl.Visible)then
    begin
      TabHidePalettes.Add(ToolsPnl);
      ToolsdockedMIClick(self);
    end;
    if(PaletteNavForm.Visible)then
    begin
      TabHidePalettes.Add(PaletteNavForm);
      NavigatorInfoMIClick(self);
    end;
    if(PaletteDataTypesForm.Visible)then
    begin
      TabHidePalettes.Add(PaletteDataTypesForm);
      DatatypesMIClick(self);
    end;
    if(PaletteModelFrom.Visible)then
    begin
      TabHidePalettes.Add(PaletteModelFrom);
      DBModelMIClick(self);
    end;
  end;

  TabHasBeenPressed:=True;
end;

procedure TMainForm.ShowPalettes;
var i: integer;
begin
  if(Not(TabHasBeenPressed))then
    Exit;

  if(DMGUI.ShowPalettesDocked)then
  begin
    if(ToolsPnl.Visible=False)then
      ToolsdockedMIClick(self);

    PaletteDockPnl.Visible:=True;
  end
  else
  begin
    for i:=0 to TabHidePalettes.Count-1 do
    begin
      if(TabHidePalettes[i]=PaletteToolsForm)then
        ToolsMIClick(self);

      if(TabHidePalettes[i]=PaletteNavForm)then
        NavigatorInfoMIClick(self);

      if(TabHidePalettes[i]=PaletteDataTypesForm)then
        DatatypesMIClick(self);

      if(TabHidePalettes[i]=PaletteModelFrom)then
        DBModelMIClick(self);

      if(TabHidePalettes[i]=ToolsPnl)then
        ToolsdockedMIClick(self);
    end;
  end;

  TabHasBeenPressed:=False;
end;

procedure TMainForm.BuildPluginMenus;
var result: integer;
  SearchRec: TSearchRec;
  theMenuItem: TMenuItem;
  s: string;
  i{, j}: integer;
  path: string;
begin
  PluginList.Clear;

  path:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));

{$IFDEF MSWINDOWS}
  Result := FindFirst(path+'DBDplugin_*.exe', 0, SearchRec);
{$ELSE}
  Result := FindFirst(path+'DBDplugin_*', 0, SearchRec);
{$ENDIF}
  if(Result<>0)then
    Exit;

  try
    while Result = 0 do
    begin
      PluginList.Add(SearchRec.Name);

      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;

  if(PluginList.Count>0)then
  begin
    //Show Plugins MenuItem when at least one Plugin is found
    if(PluginsMI.Visible=False)then
      PluginsMI.Visible:=True;

    for i:=0 to PluginList.Count-1 do
    begin
      theMenuItem:=TMenuItem.Create(MainForm);
      theMenuItem.Name:='Plugin'+IntToStr(MainForm.PluginsMI.Count+1)+'MI';
      s:=Copy(ExtractFileName(PluginList[i]),
        Length('DBDplugin_')+1,
        Length(ExtractFileName(PluginList[i]))-Length('DBDplugin_')-Length(ExtractFileExt(PluginList[i])));
      theMenuItem.Caption:=s;
      theMenuItem.Tag:=i;
      theMenuItem.OnClick:=PluginMIClick;

      PluginsMI.Insert(0, theMenuItem);
    end;
  end;
end;

procedure TMainForm.PluginMIClick(Sender: TObject);
var thePath, theModelFilename: string;
  theFileDate: TDateTime;
  PluginChangedModel: Boolean;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      PluginChangedModel:=False;

      thePath:=DMMain.SettingsPath;

      DeleteFile(thePath+'plugin_tmp.xml');

      //remember Filename
      theModelFilename:=TEERForm(MainForm.ActiveMDIChild).EERModel.ModelFilename;

      //Save the current Model
      TEERForm(MainForm.ActiveMDIChild).EERModel.SaveToFile(
        thePath+'plugin_tmp.xml',
        True, False, False);

      theFileDate:=DMMain.GetFileDate(thePath+'plugin_tmp.xml');

      Enabled:=False;
      HidePalettes;
      try
        //Call Plugin and wait till it's finished 
        DMMain.CreateProz(ExtractFilePath(Application.ExeName)+
          PathDelim+PluginList[TMenuItem(Sender).Tag]+' "'+
          thePath+'plugin_tmp.xml'+'"', '',
          1, 1);

        Enabled:=True;

        if(theFileDate<>DMMain.GetFileDate(thePath+'plugin_tmp.xml'))then
        begin
          //Replace current model data with the data from plugin_tmp.xml
          TEERForm(MainForm.ActiveMDIChild).EERModel.LoadFromFile(thePath+'plugin_tmp.xml',
            True, False, True, False);

          //Mark model as changed
          TEERForm(MainForm.ActiveMDIChild).EERModel.ModelHasChanged;

          TEERForm(MainForm.ActiveMDIChild).EERModel.ModelFilename:=theModelFilename;

          PluginChangedModel:=True;
        end;

        //Delete tmp file
        DeleteFile(thePath+'plugin_tmp.xml');
      finally
        Enabled:=True;
        ShowPalettes;
      end;

      if(PluginChangedModel)then
      begin
        if(MessageDlg(DMMain.GetTranslatedMessage('The current model has '+
          'been modified by a plugin. Do you want to save the changed model, now?', 236),
          mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
        begin
          SaveMIClick(self);
        end;
      end;
    end;
end;

procedure TMainForm.DockPalettesMIClick(Sender: TObject);
begin
  if(Not(DockPalettesMI.Checked))then
  begin
    PaletteDockPnl.Visible:=True;
    PaletteDockSepPnl.Visible:=True;
    PaletteDockSepPnl.Left:=0;

    PaletteNavForm.MainPnl.Parent:=NavPnl;
    NavPnl.Refresh;
    if(PaletteNavForm.Visible)then
      NavigatorInfoMIClick(self);
    NavigatorInfoMI.Enabled:=False;

    PaletteDataTypesForm.MainPnl.Parent:=DatatypesPnl;
    DatatypesPnl.Refresh;
    if(PaletteDataTypesForm.Visible)then
      DatatypesMIClick(self);
    DatatypesMI.Enabled:=False;

    PaletteModelFrom.MainPnl.Parent:=ModelPnl;
    ModelPnl.Refresh;
    if(PaletteModelFrom.Visible)then
      DBModelMIClick(self);
    DBModelMI.Enabled:=False;

    ResetPalettePositionsMI.Enabled:=False;

    DockPalettesMI.Checked:=True;

    DMGUI.ShowPalettesDocked:=True;
  end
  else
  begin
    PaletteDockPnl.Visible:=False;

    PaletteNavForm.MainPnl.Parent:=PaletteNavForm;
    NavigatorInfoMI.Enabled:=True;
    NavigatorInfoMIClick(self);


    PaletteDataTypesForm.MainPnl.Parent:=PaletteDataTypesForm;
    DatatypesMI.Enabled:=True;
    DatatypesMIClick(self);

    PaletteModelFrom.MainPnl.Parent:=PaletteModelFrom;
    DBModelMI.Enabled:=True;
    DBModelMIClick(self);

    ResetPalettePositionsMI.Enabled:=True;

    DockPalettesMI.Checked:=False;

    DMGUI.ShowPalettesDocked:=False;

    ResetPalettePositionsMIClick(self);
  end;
end;

procedure TMainForm.CopyMouseOverObjToClipboard;
var theObj: TObject;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      theObj:=TEERForm(ActiveMDIChild).EERModel.GetMouseOverObj;
      if(theObj<>nil)then
        if(theObj.ClassNameIs('TEERTable'))then
        begin
          //Copy Column name
          if(TEERForm(ActiveMDIChild).EERModel.GetMouseOverSubObj<>nil)then
          begin
            Clipboard.AsText:=TEERColumn(TEERForm(ActiveMDIChild).EERModel.GetMouseOverSubObj).ColName;
            //WindowState:=wsMinimized;
          end
          //Copy Table name
          else
          begin
            Clipboard.AsText:=TEERTable(theObj).ObjName;
            //WindowState:=wsMinimized;
          end;
        end;
    end;
end;

procedure TMainForm.CopyMouseOverObjToSQLMemo(Key: integer);
var theObj: TObject;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      theObj:=TEERForm(ActiveMDIChild).EERModel.GetMouseOverObj;
      if(theObj<>nil)then
        if(theObj.ClassNameIs('TEERTable'))then
        begin
          if(TEERForm(ActiveMDIChild).EERModel.GetMouseOverSubObj<>nil)then
          begin
            TEditorQueryForm(DockedEditorQueryForm).ProcessKey(Key,
              TEERColumn(TEERForm(ActiveMDIChild).EERModel.GetMouseOverSubObj));
          end;
        end;
    end;
end;

procedure TMainForm.CopySQLToClipboard(Key: Word);
begin
  if(Key=Key_A)then
    TEditorQueryForm(DockedEditorQueryForm).CopySQLToClipboard(1)
  else if(Key=Key_D)then
    TEditorQueryForm(DockedEditorQueryForm).CopySQLToClipboard(2)
  else if(Key=Key_P)then
    TEditorQueryForm(DockedEditorQueryForm).CopySQLToClipboard(3);
end;

procedure TMainForm.DesignModeMIShow(Sender: TObject);
begin
  DesignModeMI.Checked:=(DMEER.WorkMode=wmDesign);
  QueryModeMI.Checked:=(DMEER.WorkMode=wmQuery);
end;

procedure TMainForm.DesignModeMIClick(Sender: TObject);
begin
  if(TMenuItem(Sender).Name='DesignModeMI')and(DMEER.WorkMode<>wmDesign)then
    SetWorkMode(wmDesign)
  else if(TMenuItem(Sender).Name='QueryModeMI')and(DMEER.WorkMode<>wmQuery)then
    SetWorkMode(wmQuery);
end;

procedure TMainForm.ConnectionSBtnClick(Sender: TObject);
begin
  DMDB.DisconnectFromDB;
end;

procedure TMainForm.CheckLinuxDesktopFile;
var theDesktopFile: TStringList;
  fname: string;
begin
  //Replace the stored Icon path with the real path
  fname:=ExtractFilePath(Application.ExeName)+'startdbd.desktop';
  if(FileExists(fname))then
  begin
    theDesktopFile:=TStringList.Create;
    try
      theDesktopFile.LoadFromFile(fname);

      //Replace Script Position
      if(CompareText(ExtractFilePath(Application.ExeName), '/home/mike/DBDesigner4/')<>0)then
        theDesktopFile.Text:=DMMain.ReplaceText(theDesktopFile.Text,
          '$HOME/DBDesigner4/startdbd',
          ExtractFilePath(Application.ExeName)+'startdbd');

      //Replace Icon Position
      theDesktopFile.Text:=DMMain.ReplaceText(theDesktopFile.Text,
        '/home/mike/DBDesigner4/Gfx/Icon48.xpm',
        ExtractFilePath(Application.ExeName)+'Gfx/Icon48.xpm');

      try
        theDesktopFile.SaveToFile(fname);
      except
      end;
    finally
      theDesktopFile.Free;
    end;
  end;
end;

procedure TMainForm.DoApplicationEvent(Sender: QObjectH; Event: QEventH; var Handled: Boolean);
var TypingInMemo: Boolean;
  Key: Word;
  theShiftState: TShiftState;
  EditorIsActive: Boolean;
begin
  Handled:=False;

  //Workaround, detect if an editor is active or not
  EditorIsActive:=True;
  if(Screen.ActiveForm=nil)then
    EditorIsActive:=False;

  if(Screen.ActiveForm<>nil)then
    if(Screen.ActiveForm.Name='EERForm')or(Screen.ActiveForm.Name='MainForm')then
      EditorIsActive:=False;

  //If the user is typing into a memo
  TypingInMemo:=False;
  try
    if(ActiveControl<>nil)and(Not(Application.Terminated))then
      if(ActiveControl.ClassNameIs('TMemo'))or
        (ActiveControl.ClassNameIs('TSynEdit'))then
        TypingInMemo:=True;
  except
    //Catch Linux exception when ActiveControl is not nil but points nowhere
  end;

  // -------------------------------------------------------
  // Keydowns

  if(QEvent_type(Event)=QEventType_KeyPress)then
  begin
    Key:=QKeyEvent_key(QKeyEventH(Event));

    theShiftState:=ButtonStateToShiftState(QKeyEvent_stateAfter(QKeyEventH(Event)));

    // Space
    if(Not(TypingInMemo))and(Key=Key_Space)then
    begin
      if(Not(SpaceDown))and
        (DMEER.CurrentWorkTool<>wtHand)and
        (DMEER.CurrentWorkTool<>wtZoomIn)and
        (DMEER.CurrentWorkTool<>wtZoomOut)then
        ToolBeforeSpaceDown:=DMEER.CurrentWorkTool;

      SpaceDown:=True;

      if(theShiftState=[])then
        DMEER.SetCurrentWorkTool(wtHand)
      else if(ssAlt in theShiftState)then
        DMEER.SetCurrentWorkTool(wtZoomOut)
      else if(ssCtrl in theShiftState)then
        DMEER.SetCurrentWorkTool(wtZoomIn);
    end
    else if(SpaceDown)and(Key=Key_Alt)then
    begin
      DMEER.SetCurrentWorkTool(wtZoomOut);
    end
    else if(SpaceDown)and(Key=Key_Control)then
    begin
      DMEER.SetCurrentWorkTool(wtZoomIn);
    end

    //Tab
    else if(Not(TypingInMemo))and(Key=Key_Tab)and
      (theShiftState=[ssCtrl, ssShift])then
    begin
      if(TabHasBeenPressed)then
        ShowPalettes
      else
        HidePalettes;

      Handled:=True;
    end
    else if(Key=Key_Tab)and
      (theShiftState=[ssCtrl])then
    begin
      if(DMEER.WorkMode=wmDesign)then
        SetWorkMode(wmQuery)
      else
        SetWorkMode(wmDesign);

      Handled:=True;
    end

    //F11
    else if(Key=Key_F11)then
    begin
      WindowState:=wsNormal;
      Top:=1;
      Left:=1;
      Width:=1024-8;
      Height:=768-34;

      Handled:=True;
    end

    // -------------------------------------------------------
    // Keys in both Modes

    // 1 - 9
    else if(((Key>=Key_0)and(Key<=Key_9))or(Key=0)or(Key=30))and
      (theShiftState=[ssCtrl, ssShift])then
    begin
      if(Key=0)then //Strange behaviour of Ctrl+Shift+Key_2
        Key:=Key_2;

      if(Key=30)then //Strange behaviour of Ctrl+Shift+Key_6
        Key:=Key_6;

      if(ActiveMDIChild<>nil)then
        if(ActiveMDIChild.Classname='TEERForm')then
        begin
          TEERForm(ActiveMDIChild).EERModel.SetPositionMarker(Key-Key_0);

          StatusCaptionLbl.Caption:='Position Marker '+IntToStr(Key-Key_0)+' set.';

          Handled:=True;
        end;
    end
    else if(Key>=Key_0)and(Key<=Key_9)and
      (theShiftState=[ssCtrl])then
    begin
      if(ActiveMDIChild<>nil)then
        if(ActiveMDIChild.Classname='TEERForm')then
        begin
          TEERForm(ActiveMDIChild).EERModel.GotoPositionMarker(Key-Key_0);

          Handled:=True;
        end;
    end

    // C
    else if(Key=Key_C)and
      (theShiftState=[ssCtrl, ssShift])then
    begin
      CopyMouseOverObjToClipboard;
      if(DMGUI.MinimizeOnCtrlShiftC)then
        Application.Minimize;
      Handled:=True;
    end

    // Q
    else if(Key=Key_Q)and
      (theShiftState=[ssCtrl])then
    begin
      DMEER.SetWorkTool(wtPointer);
      Handled:=True;
    end;

    // -------------------------------------------------------
    // Keys in Design Mode
    if(DMEER.WorkMode=wmDesign)and(Handled=False)then
    begin
      // Del
      if(Key=Key_Delete)and
        (theShiftState=[ssCtrl])then
      begin
        DeleteMIClick(self);
        Handled:=True;
      end

      // A
      {else if(Key=Key_A)and
        (theShiftState=[ssCtrl])then
      begin
        SelectAllMIClick(self);
        Handled:=True;
      end}

      // C
      {else if(Key=Key_C)and
        (Key_CtrlIsDown)and(Not(Key_ShiftIsDown))and(Not(Key_AltIsDown))then
      begin
        CopyMIClick(self);
        Handled:=True;
      end}

      // E
      else if(Key=Key_E)and
        (theShiftState=[ssCtrl])then
      begin
        DMEER.SetWorkTool(wtSize);
        Handled:=True;
        Exit;
      end

      // R
      else if(Key=Key_R)and
        (theShiftState=[ssCtrl])then
      begin
        DMEER.SetWorkTool(wtRel1nSub);
        Handled:=True;
      end
      else if(Key=Key_R)and
        (theShiftState=[ssCtrl, ssShift])then
      begin
        DMEER.SetWorkTool(wtRel1n);
        Handled:=True;
      end
      else if(Key=Key_R)and
        (theShiftState=[ssCtrl, ssShift, ssAlt])then
      begin
        DMEER.SetWorkTool(wtRel11);
        Handled:=True;
      end

      // S
      else if(Key=Key_S)and
        (theShiftState=[ssCtrl])then
      begin
        SaveMIClick(self);
        Handled:=True;
      end
      else if(Key=Key_S)and
        (theShiftState=[ssCtrl, ssShift])then
      begin
        SQLCreateScriptMIClick(self);
        Handled:=True;
      end

      // T
      else if(Key=Key_T)and
        (theShiftState=[ssCtrl])then
      begin
        DMEER.SetWorkTool(wtTable);
        Handled:=True;
      end

      // O
      else if(Key=Key_O)and
        (theShiftState=[ssCtrl])then
      begin
        OpenMIClick(self);
        Handled:=True;
      end

      // V
      {else if(Key=Key_V)and
        (Key_CtrlIsDown)and(Not(Key_ShiftIsDown))and(Not(Key_AltIsDown))then
      begin
        PasteMIClick(self);
        Handled:=True;
      end}

      // W
      else if(Key=Key_W)and
        (theShiftState=[ssCtrl])then
      begin
        DMEER.SetWorkTool(wtMove);
        Handled:=True;
      end

      {// X
      else if(Key=Key_X)and
        (Key_CtrlIsDown)and(Not(Key_ShiftIsDown))and(Not(Key_AltIsDown))then
      begin
        CutMIClick(self);
        Handled:=True;
      end}

      // Z
      else if(Key=Key_Z)and
        (theShiftState=[ssCtrl])then
      begin
        UndoMIClick(self);
        Handled:=True;
      end
      else if(Key=Key_Z)and
        (theShiftState=[ssCtrl, ssShift])then
      begin
        RedoMIClick(self);
        Handled:=True;
      end

      else if(Key=Key_Up)or(Key=Key_Down)or
        (Key=Key_Left)or(Key=Key_Right)then
      begin
        try
          if(Not(EditorIsActive))then
            if(ActiveMDIChild<>nil)and(ActiveControl=nil)then
              if(ActiveMDIChild.Classname='TEERForm')then
              begin
                TEERForm(ActiveMDIChild).EERModel.MoveSelectedEERObjects(
                  (Ord(Key=Key_right)-Ord(Key=Key_Left))*
                    (1+9*Ord(theShiftState=[ssShift]))*
                    (1+49*Ord(theShiftState=[ssCtrl, ssShift])),
                  (Ord(Key=Key_Down)-Ord(Key=Key_Up))*
                    (1+9*Ord(theShiftState=[ssShift]))*
                    (1+49*Ord(theShiftState=[ssCtrl, ssShift])));

                Handled:=True;
              end;
        except
        end;

      end;
    end
    // -------------------------------------------------------
    // Keys in Query Mode
    else if(DMEER.WorkMode=wmQuery)and(Handled=False)then
    begin
      //F9
      if(Key=Key_F9)then
      begin
        TEditorQueryForm(DockedEditorQueryForm).ExecSQLBtnClick(self);
      end;

      if((Key=Key_A)and(theShiftState=[ssCtrl]))then
      begin
        TEditorQueryForm(DockedEditorQueryForm).SQLMemo.SelectAll;
        Handled:=True;
      end

      //A, P, D
      else if((Key=Key_A)or(Key=Key_P)or(Key=Key_D))and
        (theShiftState=[ssCtrl, ssShift])then
      begin
        CopySQLToClipboard(Key);
        Handled:=True;
      end

      // E
      else if(Key=Key_E)and
        (theShiftState=[ssCtrl])then
      begin
        DMEER.SetWorkTool(wtSQLSet);
        Handled:=True;
      end

      // F
      else if(Key=Key_F)and
        (theShiftState=[ssCtrl])then
      begin
        DMEER.SetWorkTool(wtSQLFrom);
        Handled:=True;
      end

      // G
      else if(Key=Key_G)and
        (theShiftState=[ssCtrl])then
      begin
        DMEER.SetWorkTool(wtSQLGroup);
        Handled:=True;
      end

      // H
      else if(Key=Key_H)and
        (theShiftState=[ssCtrl])then
      begin
        DMEER.SetWorkTool(wtSQLHaving);
        Handled:=True;
      end

      // S
      else if(Key=Key_S)and
        (theShiftState=[ssCtrl])then
      begin
        DMEER.SetWorkTool(wtSQLSelect);
        Handled:=True;
      end

      // O
      else if(Key=Key_O)and
        (theShiftState=[ssCtrl])then
      begin
        DMEER.SetWorkTool(wtSQLOrder);
        Handled:=True;
      end

      // W
      else if(Key=Key_W)and
        (theShiftState=[ssCtrl])then
      begin
        DMEER.SetWorkTool(wtSQLWhere);
        Handled:=True;
      end;
    end;
  end;

  // -------------------------------------------------------
  // Keyups

  if(QEvent_type(Event)=QEventType_KeyRelease)and
    (not(TypingInMemo))then
  begin
    Key:=QKeyEvent_key(QKeyEventH(Event));

    if(Key=Key_Space)then
    begin
      SpaceDown:=False;
{$IFDEF MSWINDOWS}
      DMEER.SetCurrentWorkTool(ToolBeforeSpaceDown);
{$ELSE}
      //Workaround for Kylix
      KylixSpaceUpTimer.Enabled:=False;
      KylixSpaceUpTimer.Enabled:=True;
{$ENDIF}

      //Handled:=True;
    end
    else if(Key=Key_Control)and(SpaceDown)then
    begin
      //Use Application.keystate because theShiftState is wrong
      if(ssAlt in Application.keystate)then
        DMEER.SetCurrentWorkTool(wtZoomOut)
      else
        DMEER.SetCurrentWorkTool(wtHand);

      Handled:=True;
    end
    else if(Key=Key_Alt)and(SpaceDown)then
    begin
      //Use Application.keystate because theShiftState is wrong
      if(ssCtrl in Application.keystate)then
        DMEER.SetCurrentWorkTool(wtZoomIn)
      else
        DMEER.SetCurrentWorkTool(wtHand);

      Handled:=True;
    end;
  end;

  if(QEvent_type(Event)=QEventType_SetSQLTextFont)then
  begin
    TEditorQueryForm(DockedEditorQueryForm).SQLMemo.Font.Name:=DMGUI.SQLTextFont;
    TEditorQueryForm(DockedEditorQueryForm).SQLMemo.Font.Size:=DMGUI.SQLTextFontSize;
  end;

{$IFDEF MSWINDOWS}
  if(QEvent_type(Event)=QEventType_WindowActivate)then
  begin
    inc(ActivateDeactivateCounter);
    DeactivateTmr.Enabled:=False;

    if(ApplicationIsDeactivated=True)then
    begin
      ApplicationIsDeactivated:=False;
      DMMain.RestoreStayOnTopForms;
    end;
  end;

  if(QEvent_type(Event)=QEventType_WindowDeactivate)then
  begin
    dec(ActivateDeactivateCounter);
    if(ActivateDeactivateCounter=-1)then
      DeactivateTmr.Enabled:=True;
  end;
{$ENDIF}
end;


procedure TMainForm.KylixSpaceUpTimerTimer(Sender: TObject);
begin
  //Workaround for Kylix
  KylixSpaceUpTimer.Enabled:=False;

  if(Not(SpaceDown))then
    DMEER.SetCurrentWorkTool(ToolBeforeSpaceDown);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_F1)then
  begin
    if(DMEER.WorkMode=wmDesign)then
      DMMain.ShowHelp('modelling', '')
    else
      DMMain.ShowHelp('querying', '');
  end;
end;

procedure TMainForm.SnapToGridBtnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      TEERForm(ActiveMDIChild).EERModel.UsePositionGrid:=Not(SnapToGridBtn.Down);
end;

procedure TMainForm.SubmitabugfeaturerequestMIClick(Sender: TObject);
begin
  DMMain.BrowsePage('https://sourceforge.net/projects/dbdesigner-fork/');
end;


procedure TMainForm.DeactivateTmrTimer(Sender: TObject);
begin
  DeactivateTmr.Enabled:=False;

  ApplicationIsDeactivated:=True;
  DMMain.NormalizeStayOnTopForms;
end;

procedure TMainForm.CenterModelMIClick(Sender: TObject);
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      DMEER.CenterModel(TEERForm(ActiveMDIChild).EERModel);
end;

procedure TMainForm.ImportERwin41XMLModelMIClick(Sender: TObject);
var theOpenDialog: TOpenDialog;
  theEERForm: TEERForm;
begin
  theOpenDialog:=TOpenDialog.Create(nil);
  try
{$IFDEF MSWINDOWS}
    //On Windows use native Win32 Open Dlg
    theOpenDialog.UseNativeDialog:=True;
    theOpenDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
{$ENDIF}

    theOpenDialog.Title:=DMMain.GetTranslatedMessage('Open a ERwin 4.1 XML Model ...', 19);
    theOpenDialog.DefaultExt:='xml';
    theOpenDialog.Filter:=DMMain.GetTranslatedMessage('ERwin 4.1 XML Model files (*.xml)|*.xml', 20);
    theOpenDialog.Width:=600;
    theOpenDialog.Height:=450;

    if(DirectoryExists(DMGUI.RecentImportFileDir))then
      theOpenDialog.InitialDir:=DMGUI.RecentImportFileDir
    else
      theOpenDialog.InitialDir:='';

    if(theOpenDialog.Execute)then
    begin
      theEERForm:=nil;

      if(ActiveMDIChild<>nil)then
        if(ActiveMDIChild.Classname='TEERForm')then
          if(CompareText(Copy(TEERForm(ActiveMDIChild).EERModel.ModelFilename, 1, 6), 'Noname')=0)and
            (Not(TEERForm(ActiveMDIChild).EERModel.IsChanged))then
            theEERForm:=TEERForm(ActiveMDIChild);

      if(theEERForm=nil)then
        theEERForm:=TEERForm(NewEERModel);

      DMEERExportImport.ImportERwin41XMLModel(theEERForm.EERModel, theOpenDialog.Filename);
      DMGUI.RecentImportFileDir:=ExtractFilePath(theOpenDialog.Filename);
    end;

  finally
    theOpenDialog.Free;
  end;
end;

procedure TMainForm.ConnecttoDatabaseMIClick(Sender: TObject);
begin
  DMDB.DisconnectFromDB;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      DMDB.GetDBConnButtonClick(Sender, TEERForm(TForm(Application.MainForm).ActiveMDIChild).EERModel.DefQueryDBConn)
    else
      DMDB.GetDBConnButtonClick(Sender, '');
end;

procedure TMainForm.DisconnectfromDatabaseMIClick(Sender: TObject);
begin
  DMDB.DisconnectFromDB;
end;

procedure TMainForm.DisconnectfromDatabaseMIShow(Sender: TObject);
begin
  DisconnectfromDatabaseMI.Enabled:=DMDB.CurrentDBConn<>nil;
end;

procedure TMainForm.CopyselectedObjectsasImageMIClick(Sender: TObject);
var ModelBmp: TBitmap;
begin
  //Save File
  ModelBmp:=TBitmap.Create;
  try
    TEERForm(ActiveMDIChild).EERModel.PaintModelToImage(ModelBmp, True);

    QClipboard_clear(Clipboard.Handle);
    QClipboard_setPixmap(Clipboard.Handle, ModelBmp.Handle);
  finally
    ModelBmp.Free;
  end;
end;

procedure TMainForm.CloseAllMIClick(Sender: TObject);
var i: integer;
begin
  i:=0;
  while(i<MDIChildCount)do
  begin
    if(MDIChildren[i] is TEERForm)then
    begin
      MDIChildren[i].Close;
      Application.ProcessMessages;
    end
    else
      inc(i);
  end;
end;

procedure TMainForm.SQLOptimizeTableScriptMIClick(Sender: TObject);
begin
  EERExportSQLScriptFrom:=TEERExportSQLScriptFrom.Create(self);
  try
    EERExportSQLScriptFrom.SetModel(TEERForm(ActiveMDIChild).EERModel, 2);
    EERExportSQLScriptFrom.ShowModal;
  finally
    EERExportSQLScriptFrom.Free;
  end;
end;

procedure TMainForm.OpenMIClick(Sender: TObject);
var theOpenDialog: TOpenDialog;
begin
  theOpenDialog:=TOpenDialog.Create(nil);
  try
{$IFDEF MSWINDOWS}
    //On Windows use native Win32 Open Dlg
    theOpenDialog.UseNativeDialog:=True;
    theOpenDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
{$ENDIF}

    theOpenDialog.Title:=DMMain.GetTranslatedMessage('Open a Model ...', 9);
    theOpenDialog.DefaultExt:='xml';
    theOpenDialog.Filter:=DMMain.GetTranslatedMessage('DB-Model files (*.xml)|*.xml', 10);
    theOpenDialog.Width:=600;
    theOpenDialog.Height:=450;

    if(DirectoryExists(DMGUI.RecentOpenFileDir))then
      theOpenDialog.InitialDir:=DMGUI.RecentOpenFileDir
    else
      theOpenDialog.InitialDir:='';

    if(theOpenDialog.Execute)then
    begin
      OpenFile(theOpenDialog.Filename, False);
      DMGUI.RecentOpenFileDir:=ExtractFilePath(theOpenDialog.Filename);
    end;

  finally
    theOpenDialog.Free;
  end;
end;

procedure TMainForm.PlaceModel(theModel: TEERModel; P: TPoint; PlaceFrom: integer);
var EERPlaceModelForm: TEERPlaceModelForm;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      EERPlaceModelForm:=TEERPlaceModelForm.Create(self);
      try
        if(EERPlaceModelForm.SetData(TEERForm(ActiveMDIChild).EERModel, P, PlaceFrom))then
          EERPlaceModelForm.ShowModal;
      finally
        EERPlaceModelForm.Free;
      end;
    end;
end;

procedure TMainForm.AddLinkModelFromFileMIClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtPlacementFromFile);
end;

procedure TMainForm.AddLinkModelfromDBMIClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtPlacementFromDB);
end;

procedure TMainForm.AddLinkModelfromOnlineLibraryMIClick(Sender: TObject);
begin
  DMEER.SetWorkTool(wtPlacementFromLibrary);
end;

procedure TMainForm.ShowLinkedModelsMIShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=False;

  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
      TMenuItem(Sender).Enabled:=(TEERForm(ActiveMDIChild).EERModel.LinkedModels.Count>0);
end;

procedure TMainForm.ShowLinkedModelsMIClick(Sender: TObject);
var EERPlaceModelForm: TEERPlaceModelForm;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      EERPlaceModelForm:=TEERPlaceModelForm.Create(self);
      try
        EERPlaceModelForm.DisplayLinkedModels(TEERForm(ActiveMDIChild).EERModel);
        EERPlaceModelForm.ShowModal;
      finally
        EERPlaceModelForm.Free;
      end;
    end;
end;

procedure TMainForm.RefreshLinkedObjectsMIClick(Sender: TObject);
var EERPlaceModelForm: TEERPlaceModelForm;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      if(MessageDlg(DMMain.GetTranslatedMessage(
        'Are you shure you want to refresh the Linked Objects?', 248), mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
      begin
        EERPlaceModelForm:=TEERPlaceModelForm.Create(self);
        try
          //Refresh all Linked Models
          EERPlaceModelForm.RefreshLinkedModel(TEERForm(ActiveMDIChild).EERModel, -1);
        finally
          EERPlaceModelForm.Free;
        end;
      end;
    end;
end;

procedure TMainForm.SQLRepairTableScriptMIClick(Sender: TObject);
begin
  EERExportSQLScriptFrom:=TEERExportSQLScriptFrom.Create(self);
  try
    EERExportSQLScriptFrom.SetModel(TEERForm(ActiveMDIChild).EERModel, 3);
    EERExportSQLScriptFrom.ShowModal;
  finally
    EERExportSQLScriptFrom.Free;
  end;
end;

procedure TMainForm.NavInfoPBoxPaint(Sender: TObject);
begin
  TPaintBox(Sender).Canvas.Font.Name:=Font.Name;
  TPaintBox(Sender).Canvas.Font.Size:=Font.Size;

  //Paint Docked Palette Headers (for XTF smooth fonts)
  TPaintBox(Sender).Canvas.Font.Color:=clGray;
  TPaintBox(Sender).Canvas.TextOut(0, 0,
    DMMain.GetTranslatedMessage('', TPaintBox(Sender).Tag));
end;

procedure TMainForm.Test1Click(Sender: TObject);
begin
  DMDBEER.BuildTableFromCreateStatement(nil, '', MySQL);
end;

procedure TMainForm.ExportMDBXMLFileMIClick(Sender: TObject);
var theSaveDialog: TSaveDialog;
begin
  if(ActiveMDIChild<>nil)then
    if(ActiveMDIChild.Classname='TEERForm')then
    begin
      theSaveDialog:=TSaveDialog.Create(nil);
      try
{$IFDEF MSWINDOWS}
        //On Windows use native Win32 Open Dlg
        theSaveDialog.UseNativeDialog:=True;
        theSaveDialog.OnShow:=DMMain.OnOpenSaveDlgShow;
{$ENDIF}

        theSaveDialog.Title:=DMMain.GetTranslatedMessage('Export Model as MDB XML File ...', -1);
        theSaveDialog.Width:=600;
        theSaveDialog.Height:=450;
        theSaveDialog.DefaultExt:='schema';

        if(DirectoryExists(DMGUI.RecentSaveModelAsImageDir))then
          theSaveDialog.InitialDir:=DMGUI.RecentSaveModelAsImageDir
        else
          theSaveDialog.InitialDir:='';

        theSaveDialog.Filter:=DMMain.GetTranslatedMessage('MDB XML files (*.schema)', -1);

        if(theSaveDialog.Execute)then
        begin
          DMGUI.RecentSaveModelAsImageDir:=ExtractFilePath(theSaveDialog.Filename);

          if(FileExists(theSaveDialog.Filename))then
            if(MessageDlg(DMMain.GetTranslatedMessage('The file [%s] '+
              'already exists. '#13#10+
              'Do you want to overwrite this file?', 14, ExtractFileName(theSaveDialog.Filename)), mtInformation,
              [mbYes, mbNo], 0)=mrNo)then
              Exit;

          DMEERExportImport.ExportMDBXMLFile(TEERForm(ActiveMDIChild).EERModel,
            theSaveDialog.Filename);
        end;
      finally
        theSaveDialog.Free;
      end;
    end;
end;

end.
