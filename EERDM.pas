unit EERDM;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of fabFORCE DBDesigner4.
// Copyright (C) 2002, 2003 Michael G. Zinner, www.fabFORCE.net
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
// Unit EERDM.pas
// --------------
// Version 2.1, 03.05.2003, Mike
// Description
//   Contains help functions for the EERModel
//
// Changes:
//   Version 2.1, 03.05.2003, Mike
//     added function CenterModel
//   Version 2.0, 18.04.2003, Mike
//     Changed all Records to TObjects.
//   Version 1.3, 08.04.2003, Mike
//     added DefaultTableType and ActivateRefDefForNewRelation
//   Version 1.2, 04.04.2003, Mike
//     added PositionGrid and TableNameInRefs params as initial defaults
//     for EERModel
//   Version 1.1, 31.03.2003, Mike
//     added AssignNewIDsToEERObjects
//   Version 1.0, 13.03.2003, Mike
//     initial version, Mike
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, QTypes, QForms, Qt, QControls,
  IniFiles, QDialogs, QExtCtrls, QMenus, Types, EERModel;

type
  TDMEER = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

    procedure SetWorkTool(WorkTool: integer);
    procedure UpdateStatusBar;

    procedure RefreshPalettes;
    procedure RefreshNavPalette;
    procedure RefreshNavImg;
    procedure RefreshInfoPalette;

    procedure SetWorkToolCurser(WorkTool: integer);
    procedure SetCurrentWorkTool(WorkTool: integer);

    //Get the Texts from a ini File
    procedure LoadWorkToolLabelTextsFromIniFile;

    procedure LoadCursors;

    procedure LoadSettingsFromIniFile;
    procedure SaveSettingsToIniFile;

    procedure RefreshSavedImg;

    function GetNextNonameNumber: String;

    //Assigns new ids to a model and optionally shifts it 30px to right/bottom
    function AssignNewIDsToEERObjects(themodel: string; shiftObjectPositions: Boolean = false): string;
    //Centers the model on the canvas
    procedure CenterModel(theModel: TEERModel);
  private
    { Private declarations }
    NonameCounter: integer;
  public
    { Public declarations }

    WorkToolLabelTexts: TStringList;
    CurrentWorkTool, SelectedWorkTool: integer;

    DisplayMode: integer;
    DisplayPhysicalSchema,
    DisplayPaperGrid,
    DisplayTableIndices: Boolean;
    Notation: integer;
    DisplayRelationNames: Boolean;
    ShowForeignKeys: Boolean;

    WorkMode: integer;

    DisablePaint,
    DisableTextOutput: Boolean;

    LimitUndoActions: Boolean;
    UndoActionLimit: integer;

    CompareTable: Pointer;

    InitWorkMode: integer;

    DefaultRegionColors: string;

    TblHeaderBGImgs: string;

    //ActiveSQLColumnDragPositions: integer;
    IsDragging: Boolean;

    UsePositionGrid: Boolean;
    PositionGrid: TPoint;

    TableNameInRefs: Boolean;
    DefaultTableType: integer;
    ActivateRefDefForNewRelations: Boolean;

    FKPrefix, FKPostfix: string;

    CreateFKRefDefIndex: Boolean;

    EncloseNames,
    RenameReservedWords: Boolean;

    DoNotUseRelNameInRefDef: Boolean;

    UseNewXMLParser: Boolean;

    TableSplitterPos: integer;

    SyncDatatypesOfForeignKeys: Boolean;
    OutputLinuxStyleLineBreaks: Boolean;

    AddQuotesToDefVals: Boolean;
  end;

  const
    crMoveCursor=2;
    crNewTableCursor=10;
    crRel1nCursor=11;
    crRel1nSubCursor=12;
    crRel11Cursor=13;
    crRelnmCursor=14;
    crRel11SubCursor=15;
    crRel11NonIdCursor=16;
    crNewRegionCursor=20;
    crNewNoteCursor=21;
    crNewImageCursor=22;
    crHandCursor=30;
    crZoomInCursor=31;
    crZoomOutCursor=32;
    crSizeCursor=40;
    crDeleteCursor=41;
    crSQLSelectCursor=50;
    crSQLFromCursor=51;
    crSQLOnCursor=52;
    crSQLWhereCursor=53;
    crSQLGroupCursor=54;
    crSQLHavingCursor=55;
    crSQLOrderCursor=56;
    crSQLSetCursor=57;
    crPlacementCursor=58;


    wtPointer=1;
    wtMove=2;
    wtTable=3;
    wtRel1n=4;
    wtRel1nSub=5;
    wtRel11=6;
    wtRegion=7;
    wtNote=8;
    wtHand=9;
    wtZoomIn=10;
    wtZoomOut=11;
    wtRelnm=12;
    wtDelete=13;
    wtSize=14;
    wtRel11Sub=15;
    wtImage=16;
    wtRel11NonId=17;

    wtSQLSelect=50;
    wtSQLFrom=51;
    wtSQLOn=52;
    wtSQLWhere=53;
    wtSQLGroup=54;
    wtSQLHaving=55;
    wtSQLOrder=56;
    wtSQLSet=57;

    wtPlacementFromFile=58;
    wtPlacementFromDB=59;
    wtPlacementFromLibrary=60;


    dmEntityLevel=1;
    dmPrimaryKeyLevel=2;
    dmAttributeLevel=3;

    noStandard=1;
    noErwin=2;
    noStandard2=3;
    noCrowsFoot=4;

    wmDesign=1;
    wmQuery=2;

const
  QEventType_EditTable = QEventType(Integer(QEventType_ClxUser) + 1);
  QEventType_EditRel = QEventType(Integer(QEventType_ClxUser) + 2);
  QEventType_EditRegion = QEventType(Integer(QEventType_ClxUser) + 3);
  QEventType_EditNote = QEventType(Integer(QEventType_ClxUser) + 4);
  QEventType_EditImage = QEventType(Integer(QEventType_ClxUser) + 5);
  QEventType_SetWorkTool = QEventType(Integer(QEventType_ClxUser) + 10);
  QEventType_RefreshPalettes = QEventType(Integer(QEventType_ClxUser) + 11);
  QEventType_RefreshNavPalette = QEventType(Integer(QEventType_ClxUser) + 12);
  QEventType_ClearNavImg = QEventType(Integer(QEventType_ClxUser) + 13);
  QEventType_RefreshInfoPalette = QEventType(Integer(QEventType_ClxUser) + 14);
  QEventType_UpdateStatusBar = QEventType(Integer(QEventType_ClxUser) + 15);
  QEventType_SetStatusCaption = QEventType(Integer(QEventType_ClxUser) + 16);
  QEventType_SetSaveImgs = QEventType(Integer(QEventType_ClxUser) + 17);
  QEventType_RefreshDataTypesPalette = QEventType(Integer(QEventType_ClxUser) + 18);
  QEventType_RefreshModelPalette = QEventType(Integer(QEventType_ClxUser) + 19);
  QEventType_EnableMainFormRefreshTmr = QEventType(Integer(QEventType_ClxUser) + 20);
  QEventType_RemoveChildFormsMenuItem = QEventType(Integer(QEventType_ClxUser) + 21);
  QEventType_AddToRecentFileList = QEventType(Integer(QEventType_ClxUser) + 22);
  QEventType_StartEERObjectDrag = QEventType(Integer(QEventType_ClxUser) + 23);
  QEventType_EndEERObjectDrag = QEventType(Integer(QEventType_ClxUser) + 24);
  QEventType_SelectSQLColumnFromTable = QEventType(Integer(QEventType_ClxUser) + 25);
  QEventType_RestoreStayOnTopForms = QEventType(Integer(QEventType_ClxUser) + 26);
  QEventType_RefreshGridBtn = QEventType(Integer(QEventType_ClxUser) + 27);
  QEventType_ModelNameChanged = QEventType(Integer(QEventType_ClxUser) + 28);
  QEventType_PlaceModelFromFile = QEventType(Integer(QEventType_ClxUser) + 29);
  QEventType_PlaceModelFromDB = QEventType(Integer(QEventType_ClxUser) + 30);
  QEventType_PlaceModelFromLibrary = QEventType(Integer(QEventType_ClxUser) + 31);
  QEventType_DeleteObject = QEventType(Integer(QEventType_ClxUser) + 32);
  QEventType_RedrawTableList = QEventType(Integer(QEventType_ClxUser) + 33);
var
  DMEER: TDMEER;

implementation

uses MainDM;

{$R *.xfm}

procedure TDMEER.DataModuleCreate(Sender: TObject);
begin
  //Create StringLists
  WorkToolLabelTexts:=TStringList.Create;


  //Load Cursors from Bitmaps
  LoadCursors;


  //Initialise Variables
  DisablePaint:=False;
  DisableTextOutput:=False;


  DisplayMode:=3;
  DisplayPhysicalSchema:=True;
  DisplayTableIndices:=False;
  DisplayPaperGrid:=False;
  Notation:=noStandard;
  DisplayRelationNames:=False;
  ShowForeignKeys:=True;

  TblHeaderBGImgs:='XP1';

  NonameCounter:=0;

  InitWorkMode:=wmDesign;
  WorkMode:=wmDesign;

  LoadSettingsFromIniFile;

  LoadWorkToolLabelTextsFromIniFile;

  //ActiveSQLColumnDragPositions:=0;
  IsDragging:=False;

  UseNewXMLParser:=True;
end;

procedure TDMEER.DataModuleDestroy(Sender: TObject);
begin
  SaveSettingsToIniFile;
  
  WorkToolLabelTexts.Free;
end;

procedure TDMEER.SetWorkTool(WorkTool: integer);
begin
  SelectedWorkTool:=WorkTool;
  SetCurrentWorkTool(SelectedWorkTool);

  //Post SetWorkTool Event
  sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_SetWorkTool, @SelectedWorkTool));

  if(Application.MainForm.Visible)then
    Application.MainForm.SetFocus;

  //Clear current WorkTool selection
  {for i:=ComponentCount-1 downto 0 do
    if(Copy(Components[I].Name, 1, 2)='wt')and
      (Components[I].Classname='TSpeedButton')then
      TSpeedButton(Components[I]).Down:=False;}
end;

procedure TDMEER.UpdateStatusBar;
begin
  if(Assigned(Application.MainForm))then
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_UpdateStatusBar, @SelectedWorkTool));
end;

procedure TDMEER.RefreshPalettes;
begin
  if(Assigned(Application.MainForm))then
    QApplication_postEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RefreshPalettes, nil));
end;

procedure TDMEER.RefreshNavPalette;
begin
  if(Assigned(Application.MainForm))then
    QApplication_postEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RefreshNavPalette, nil));
end;

procedure TDMEER.RefreshNavImg;
begin
  if(Assigned(Application.MainForm))then
    QApplication_postEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RefreshNavPalette, nil));
end;

procedure TDMEER.RefreshInfoPalette;
begin
  if(Assigned(Application.MainForm))then
    QApplication_postEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_RefreshInfoPalette, nil));
end;

procedure TDMEER.SetWorkToolCurser(WorkTool: integer);
begin
  case CurrentWorkTool of
      wtPointer:
        Screen.Cursor:=crDefault;
      wtMove:
        Screen.Cursor:=crMoveCursor;
      wtDelete:
        Screen.Cursor:=crDeleteCursor;
      wtSize:
        Screen.Cursor:=crSizeCursor;

      wtRegion:
        Screen.Cursor:=crNewRegionCursor;
      wtTable:
        Screen.Cursor:=crNewTableCursor;
      wtRel1n:
        Screen.Cursor:=crRel1nCursor;
      wtRel1nSub:
        Screen.Cursor:=crRel1nSubCursor;
      wtRel11:
        Screen.Cursor:=crRel11Cursor;
      wtRelnm:
        Screen.Cursor:=crRelnmCursor;
      wtRel11Sub:
        Screen.Cursor:=crRel11SubCursor;
      wtRel11NonId:
        Screen.Cursor:=crRel11NonIdCursor;

      wtNote:
        Screen.Cursor:=crNewNoteCursor;
      wtImage:
        Screen.Cursor:=crNewImageCursor;

      wtHand:
        Screen.Cursor:=crHandCursor;
      wtZoomIn:
        Screen.Cursor:=crZoomInCursor;
      wtZoomOut:
        Screen.Cursor:=crZoomOutCursor;

      wtSQLSelect:
        Screen.Cursor:=crSQLSelectCursor;
      wtSQLFrom:
        Screen.Cursor:=crSQLFromCursor;
      wtSQLOn:
        Screen.Cursor:=crSQLOnCursor;
      wtSQLWhere:
        Screen.Cursor:=crSQLWhereCursor;
      wtSQLGroup:
        Screen.Cursor:=crSQLGroupCursor;
      wtSQLHaving:
        Screen.Cursor:=crSQLHavingCursor;
      wtSQLOrder:
        Screen.Cursor:=crSQLOrderCursor;
      wtSQLSet:
        Screen.Cursor:=crSQLSetCursor;
      wtPlacementFromFile:
        Screen.Cursor:=crPlacementCursor;
      wtPlacementFromDB:
        Screen.Cursor:=crPlacementCursor;
      wtPlacementFromLibrary:
        Screen.Cursor:=crPlacementCursor;
  end;
end;

procedure TDMEER.SetCurrentWorkTool(WorkTool: integer);
var theEvent: QCustomEventH;
begin
  CurrentWorkTool:=WorkTool;
  SetWorkToolCurser(CurrentWorkTool);

  //Display the WorkTool Info
  if(CurrentWorkTool<=WorkToolLabelTexts.Count)then
    theEvent := QCustomEvent_create(QEventType_SetStatusCaption, PChar(WorkToolLabelTexts[CurrentWorkTool-1]))
  else
    theEvent := QCustomEvent_create(QEventType_SetStatusCaption, PChar(''));

  if(Application.MainForm<>nil)then
    sendCLXEvent(Application.MainForm.Handle, theEvent);

end;

procedure TDMEER.LoadWorkToolLabelTextsFromIniFile;
var theIni: TMemIniFile;
  i: integer;
begin
  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_Translations.ini');
  try
    theIni.ReadSectionValues('Worktool_Infos', WorkToolLabelTexts);

    i:=0;
    while(i<WorkToolLabelTexts.Count)do
    begin
      //Filter all strings in the wrong language
      if(Copy(WorkToolLabelTexts[i], 1, 2)<>DMMain.GetLanguageCode)then
        WorkToolLabelTexts.Delete(i)
      else
      begin
        //keep only translation
        WorkToolLabelTexts[i]:=WorkToolLabelTexts.ValueFromIndex[i];

        inc(i);
      end;
    end;
  finally
    theIni.Free;
  end;
end;


procedure TDMEER.LoadCursors;
var thepath: string;
begin
  thepath:=ExtractFilePath(Application.ExeName)+'Gfx'+PathDelim+'Cursor'+PathDelim;

  try
    DMMain.LoadACursor(crMoveCursor, thepath+'Move.bmp',
      thepath+'MoveMask.bmp', 0, 0);

    DMMain.LoadACursor(crNewTableCursor, thepath+'newTable.bmp',
      thepath+'newTableMask.bmp', 6, 3);

    DMMain.LoadACursor(crRel1nCursor, thepath+'Rel1n.bmp',
      thepath+'Rel1nMask.bmp', 0, 0);

    DMMain.LoadACursor(crRel1nSubCursor, thepath+'Rel1nSub.bmp',
      thepath+'Rel1nMask.bmp', 0, 0);

    DMMain.LoadACursor(crRel11Cursor, thepath+'Rel11.bmp',
      thepath+'Rel1nMask.bmp', 0, 0);

    DMMain.LoadACursor(crRelnmCursor, thepath+'Relnm.bmp',
      thepath+'Rel1nMask.bmp', 0, 0);

    DMMain.LoadACursor(crRel11SubCursor, thepath+'Rel11Sub.bmp',
      thepath+'Rel11SubMask.bmp', 0, 0);

    DMMain.LoadACursor(crRel11NonIdCursor, thepath+'Rel11NonId.bmp',
      thepath+'Rel11NonIdMask.bmp', 0, 0);

    DMMain.LoadACursor(crNewRegionCursor, thepath+'newRegion.bmp',
      thepath+'newRegionMask.bmp', 6, 3);

    DMMain.LoadACursor(crNewNoteCursor, thepath+'newNote.bmp',
      thepath+'newNoteMask.bmp', 6, 3);

    DMMain.LoadACursor(crHandCursor, thepath+'Hand.bmp',
      thepath+'HandMask.bmp', 8, 9);

    DMMain.LoadACursor(crZoomInCursor, thepath+'ZoomIn.bmp',
      thepath+'ZoomMask.bmp', 5, 5);

    DMMain.LoadACursor(crZoomOutCursor, thepath+'ZoomOut.bmp',
      thepath+'ZoomMask.bmp', 5, 5);

    DMMain.LoadACursor(crSizeCursor, thepath+'Size.bmp',
      thepath+'SizeMask.bmp', 0, 0);

    DMMain.LoadACursor(crDeleteCursor, thepath+'Delete.bmp',
      thepath+'DeleteMask.bmp', 0, 10);

    DMMain.LoadACursor(crNewImageCursor, thepath+'newImage.bmp',
      thepath+'newImageMask.bmp', 6, 3);

    //SQL Cursors
    DMMain.LoadACursor(crSQLSelectCursor, thepath+'SQLQuerySelect.bmp',
      thepath+'SQLQueryMask.bmp', 0, 0);

    DMMain.LoadACursor(crSQLFromCursor, thepath+'SQLQueryFrom.bmp',
      thepath+'SQLQueryMask.bmp', 0, 0);

    DMMain.LoadACursor(crSQLOnCursor, thepath+'SQLQueryOn.bmp',
      thepath+'SQLQueryMask.bmp', 0, 0);

    DMMain.LoadACursor(crSQLWhereCursor, thepath+'SQLQueryWhere.bmp',
      thepath+'SQLQueryMask.bmp', 0, 0);

    DMMain.LoadACursor(crSQLGroupCursor, thepath+'SQLQueryGroup.bmp',
      thepath+'SQLQueryMask.bmp', 0, 0);

    DMMain.LoadACursor(crSQLHavingCursor, thepath+'SQLQueryHaving.bmp',
      thepath+'SQLQueryMask.bmp', 0, 0);

    DMMain.LoadACursor(crSQLOrderCursor, thepath+'SQLQueryOrder.bmp',
      thepath+'SQLQueryMask.bmp', 0, 0);

    DMMain.LoadACursor(crSQLSetCursor, thepath+'SQLQuerySet.bmp',
      thepath+'SQLQueryMask.bmp', 0, 0);

    DMMain.LoadACursor(crPlacementCursor, thepath+'newPlacement.bmp',
      thepath+'newPlacementMask.bmp', 6, 3);

  except
    on x: Exception do
      MessageDlg(DMMain.GetTranslatedMessage('ERROR: One of the Cursors could not be loaded. '#13#10+
        'Please check the Gfx/Cursor directory.'#13#10'%s', 29, x.Message),
        mtError, [mbOK], 0);
  end;
end;


procedure TDMEER.LoadSettingsFromIniFile;
var theIni: TMemIniFile;
  DefRegionColorsList: TStringList;
begin
  DefRegionColorsList:=TStringList.Create;
  try
    //Read IniFile
    theIni:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_Settings.ini');
    try
      //Read several Params
      DisplayMode:=StrToInt(theIni.ReadString('GeneralSettings', 'DisplayMode',
        '3'));
      DisplayPhysicalSchema:=(StrToInt(theIni.ReadString('GeneralSettings', 'DisplayPhysicalSchema',
        '1'))=1);
      Notation:=StrToInt(theIni.ReadString('GeneralSettings', 'Notation',
        '1'));
      DisplayRelationNames:=(StrToInt(theIni.ReadString('GeneralSettings', 'DisplayRelationNames',
        '0'))=1);
      DisplayTableIndices:=(StrToInt(theIni.ReadString('GeneralSettings', 'DisplayTableIndices',
        '0'))=1);

      LimitUndoActions:=(StrToInt(theIni.ReadString('GeneralSettings', 'LimitUndoActions',
        '0'))=1);

      UndoActionLimit:=StrToInt(theIni.ReadString('GeneralSettings', 'UndoActionLimit',
        '10'));

      theIni.ReadSectionValues('RegionColors', DefRegionColorsList);
      DefaultRegionColors:=DefRegionColorsList.Text;

      if(theIni.ReadString('GeneralSettings', 'RunFirstTime', '0')='0')then
        InitWorkMode:=StrToInt(theIni.ReadString('GeneralSettings', 'WorkMode', '0'));

      TblHeaderBGImgs:=theIni.ReadString('GeneralSettings', 'TblHeaderBGImgs',
        'XP1');

      //TableNameInRefs Default
      TableNameInRefs:=(StrToInt(theIni.ReadString('EditingSettings', 'TableNameInRefs',
        '0'))=1);

      //Position Grid Default
      UsePositionGrid:=(StrToInt(theIni.ReadString('EditingSettings', 'UsePositionGrid',
        '0'))=1);
      PositionGrid.X:=StrToInt(theIni.ReadString('EditingSettings', 'PositionGridX',
        '20'));
      PositionGrid.Y:=StrToInt(theIni.ReadString('EditingSettings', 'PositionGridY',
        '20'));

      DefaultTableType:=StrToInt(theIni.ReadString('EditingSettings', 'DefaultTableType',
        '0'));
      ActivateRefDefForNewRelations:=(StrToInt(theIni.ReadString('EditingSettings', 'ActivateRefDefForNewRelations',
        '0'))=1);

      FKPrefix:=theIni.ReadString('EditingSettings', 'FKPrefix', '');
      FKPostfix:=theIni.ReadString('EditingSettings', 'FKPostfix', '');

      CreateFKRefDefIndex:=(StrToInt(theIni.ReadString('EditingSettings', 'CreateFKRefDefIndex', '0'))=1);

      EncloseNames:=(StrToInt(theIni.ReadString('EditingSettings', 'EncloseNames',
        '0'))=1);
      RenameReservedWords:=(StrToInt(theIni.ReadString('EditingSettings', 'RenameReservedWords',
        '1'))=1);

      TableSplitterPos:=StrToInt(theIni.ReadString('EditingSettings', 'TableSplitterPos',
        '172'));

      DoNotUseRelNameInRefDef:=(StrToInt(theIni.ReadString('GeneralSettings', 'DoNotUseRelNameInRefDef',
        '1'))=1);

      UseNewXMLParser:=(StrToInt(theIni.ReadString('GeneralSettings', 'UseNewXMLParser',
        '1'))=1);

      SyncDatatypesOfForeignKeys:=(StrToInt(theIni.ReadString('GeneralSettings', 'SyncDatatypesOfForeignKeys', '1'))=1);

{$IFDEF LINUX}
      OutputLinuxStyleLineBreaks:=(StrToInt(theIni.ReadString('GeneralSettings', 'OutputLinuxStyleLineBreaks', '1'))=1);
{$ELSE}
      OutputLinuxStyleLineBreaks:=(StrToInt(theIni.ReadString('GeneralSettings', 'OutputLinuxStyleLineBreaks', '0'))=1);
{$ENDIF}

      AddQuotesToDefVals:=(StrToInt(theIni.ReadString('GeneralSettings', 'AddQuotesToDefVals', '0'))=1);

    finally
      theIni.Free;
    end;
  finally
    DefRegionColorsList.Free;
  end;
end;

procedure TDMEER.SaveSettingsToIniFile;
var theIni: TMemIniFile;
  DefRegionColorsList: TStringList;
  i: integer;
begin
  DefRegionColorsList:=TStringList.Create;
  try
    //Open IniFile
    theIni:=TMemIniFile.Create(DMMain.SettingsPath+DMMain.ProgName+'_Settings.ini');
    try
      theIni.WriteString('GeneralSettings', 'DisplayMode', IntToStr(DisplayMode));
      theIni.WriteString('GeneralSettings', 'DisplayPhysicalSchema', IntToStr(Ord(DisplayPhysicalSchema)));
      theIni.WriteString('GeneralSettings', 'Notation', IntToStr(Notation));
      theIni.WriteString('GeneralSettings', 'DisplayRelationNames', IntToStr(Ord(DisplayRelationNames)));
      theIni.WriteString('GeneralSettings', 'DisplayTableIndices', IntToStr(Ord(DisplayTableIndices)));


      theIni.WriteString('GeneralSettings', 'LimitUndoActions',
        IntToStr(Ord(LimitUndoActions)));

      theIni.WriteString('GeneralSettings', 'UndoActionLimit',
        IntToStr(UndoActionLimit));

      theIni.WriteString('GeneralSettings', 'WorkMode',
        IntToStr(WorkMode));

      theIni.WriteString('GeneralSettings', 'TblHeaderBGImgs',
        TblHeaderBGImgs);

      DefRegionColorsList.Text:=DefaultRegionColors;
      theIni.EraseSection('RegionColors');
      for i:=0 to DefRegionColorsList.Count-1 do
        theIni.WriteString('RegionColors', DefRegionColorsList.Names[i],
          Trim(DefRegionColorsList.ValueFromIndex[i]));

      //TableNameInRefs Default
      theIni.WriteString('EditingSettings', 'TableNameInRefs',
        IntToStr(Ord(TableNameInRefs)));

      //Position Grid Default
      theIni.WriteString('EditingSettings', 'UsePositionGrid',
        IntToStr(Ord(UsePositionGrid)));
      theIni.WriteString('EditingSettings', 'PositionGridX',
        IntToStr(PositionGrid.X));
      theIni.WriteString('EditingSettings', 'PositionGridY',
        IntToStr(PositionGrid.Y));

      theIni.WriteString('EditingSettings', 'DefaultTableType',
        IntToStr(DefaultTableType));
      theIni.WriteString('EditingSettings', 'ActivateRefDefForNewRelations',
        IntToStr(Ord(ActivateRefDefForNewRelations)));

      theIni.WriteString('EditingSettings', 'FKPrefix',
        FKPrefix);
      theIni.WriteString('EditingSettings', 'FKPostfix',
        FKPostfix);

      theIni.WriteString('EditingSettings', 'CreateFKRefDefIndex',
        IntToStr(Ord(CreateFKRefDefIndex)));

      theIni.WriteString('EditingSettings', 'EncloseNames',
        IntToStr(Ord(EncloseNames)));

      theIni.WriteString('EditingSettings', 'RenameReservedWords',
        IntToStr(Ord(RenameReservedWords)));

      theIni.WriteString('EditingSettings', 'TableSplitterPos',
        IntToStr(Ord(TableSplitterPos)));

      theIni.WriteString('GeneralSettings', 'DoNotUseRelNameInRefDef',
        IntToStr(Ord(DoNotUseRelNameInRefDef)));

      theIni.WriteString('GeneralSettings', 'UseNewXMLParser',
        IntToStr(Ord(UseNewXMLParser)));

      theIni.WriteString('GeneralSettings', 'SyncDatatypesOfForeignKeys', IntToStr(Ord(SyncDatatypesOfForeignKeys)));

      theIni.WriteString('GeneralSettings', 'OutputLinuxStyleLineBreaks', IntToStr(Ord(OutputLinuxStyleLineBreaks)));

      theIni.WriteString('GeneralSettings', 'AddQuotesToDefVals', IntToStr(Ord(AddQuotesToDefVals)));

      theIni.UpdateFile;
    finally
      theIni.Free;
    end;
  finally
    DefRegionColorsList.Free;
  end;
end;

procedure TDMEER.RefreshSavedImg;
begin
  QApplication_postEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_SetSaveImgs, nil));
end;

function TDMEER.GetNextNonameNumber: String;
begin
  inc(NonameCounter);
  GetNextNonameNumber:=IntToStr(NonameCounter);
end;

function TDMEER.AssignNewIDsToEERObjects(themodel: string; shiftObjectPositions: Boolean): string;
var s, id, newid, SearchStr, OldPos, NewPos: string;
  i: integer;
begin
  //Assign new ids
  s:=themodel;
  while(Pos('ID="', s)>0)do
  begin
    //Get id
    id:=Copy(s, Pos('ID="', s)+4, 25);
    id:=Copy(id, 1, Pos('"', id)-1);

    s:=DMMain.ReplaceText(s, 'ID="'+id+'"', 'XX="XX"');

    //Replace with new id
    newid:=IntToStr(DMMain.GetNextGlobalID);

    themodel:=DMMain.ReplaceText(themodel, 'ID="'+id+'"', 'ID="'+newid+'"');

    //for indices
    themodel:=DMMain.ReplaceText(themodel, 'idColumn="'+id+'"', 'idColumn="'+newid+'"');
    //for relations
    themodel:=DMMain.ReplaceText(themodel, 'SrcTable="'+id+'"', 'SrcTable="'+newid+'"');
    themodel:=DMMain.ReplaceText(themodel, 'DestTable="'+id+'"', 'DestTable="'+newid+'"');
  end;

  if(shiftObjectPositions)then
  begin
    //Translate X+40, Y+40
    s:=themodel;
    for i:=0 to 1 do
    begin
      if(i=0)then
        SearchStr:='XPos'
      else
        SearchStr:='YPos';

      while(Pos(SearchStr+'="', s)>0)do
      begin
        //Get id
        OldPos:=Copy(s, Pos(SearchStr+'="', s)+6, 25);
        OldPos:=Copy(OldPos, 1, Pos('"', OldPos)-1);

        s:=DMMain.ReplaceText(s, SearchStr+'="'+OldPos+'"', 'XX="XX"');

        //Replace with new Pos
        NewPos:=IntToStr(StrToIntDef(OldPos, 0)+30);

        themodel:=DMMain.ReplaceText(themodel, SearchStr+'="'+OldPos+'"', SearchStr+'="'+NewPos+'"');
      end;
    end;
  end;

  AssignNewIDsToEERObjects:=themodel;
end;

procedure TDMEER.CenterModel(theModel: TEERModel);
var i: integer;
  m_left, m_top, m_right, m_bottom, move_x, move_y: integer;
begin
  m_left:=theModel.EERModel_Width;
  m_top:=theModel.EERModel_Height;
  m_right:=0;
  m_bottom:=0;

  for i:=0 to theModel.ComponentCount-1 do
    if(theModel.Components[i].ClassParent=TEERObj)then
    begin
      //Get model left/top/right/bottom
      if(TEERObj(theModel.Components[i]).Obj_X<m_left)then
        m_left:=TEERObj(theModel.Components[i]).Obj_X;
      if(TEERObj(theModel.Components[i]).Obj_Y<m_top)then
        m_top:=TEERObj(theModel.Components[i]).Obj_Y;
      if(TEERObj(theModel.Components[i]).Obj_X+
        TEERObj(theModel.Components[i]).Obj_W>m_right)then
        m_right:=TEERObj(theModel.Components[i]).Obj_X+
          TEERObj(theModel.Components[i]).Obj_W;
      if(TEERObj(theModel.Components[i]).Obj_Y+
        TEERObj(theModel.Components[i]).Obj_H>m_bottom)then
        m_bottom:=TEERObj(theModel.Components[i]).Obj_Y+
          TEERObj(theModel.Components[i]).Obj_H;
    end;

  //If there is at least one object
  if(m_right>0)then
  begin
    move_x:=(theModel.EERModel_Width-
      (m_right-m_left)) div 2;
    move_y:=(theModel.EERModel_Height-
      (m_bottom-m_top)) div 2;

    //Log move action
    theModel.StartSubActionLog(at_MoveObj);

    for i:=0 to theModel.ComponentCount-1 do
      if(theModel.Components[i].ClassParent=TEERObj)then
      begin
        with TEERObj(theModel.Components[i]) do
        begin
          ParentEERModel.LogSubAction(sa_MoveFrom, Obj_id,
            'Obj_X='+IntToStr(Obj_X)+#13#10+'Obj_Y='+IntToStr(Obj_Y));

          Obj_X:=Obj_X+move_x;
          Obj_Y:=Obj_Y+move_y;

          //Log Move
          ParentEERModel.LogSubAction(sa_MoveTo, Obj_id,
            'Obj_X='+IntToStr(Obj_X)+#13#10+'Obj_Y='+IntToStr(Obj_Y));

          RefreshObj;
        end;
      end;

    //Close Log
    theModel.EndSubAction;
  end;
end;


end.
