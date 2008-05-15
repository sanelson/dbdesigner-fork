unit Options;

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
// Unit Options.pas
// ---------------
// Version 1.3, 11.04.2003, Mike
// Description
//   Contains the options form class
//
// Changes:
//   Version 1.3, 11.04.2003, Mike
//     added FKPrefixEd, FKPostfixEd, ResetPersonalSettingsBtn
//   Version 1.2, 08.04.2003, Mike
//     added DefaultTableType and ActivateRefDefForNewRelation
//   Version 1.1, 04.04.2003, Mike
//     added Default Model Options Sheet
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

{$I DBDesigner4.inc}

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, QExtCtrls, QButtons, IniFiles, Qt;

type
  TOptionsForm = class(TForm)
    PageControl: TPageControl;
    GenOptSheet: TTabSheet;
    VisualOptSheet: TTabSheet;
    Label1: TLabel;
    RegionColorsMemo: TMemo;
    Label4: TLabel;
    DefEditingOptionsSheet: TTabSheet;
    BottomPnl: TPanel;
    AbortBtn: TSpeedButton;
    SubmitBtn: TSpeedButton;
    GroupBox2: TGroupBox;
    Label18: TLabel;
    DefaultTableTypeCBox: TComboBox;
    GroupBox3: TGroupBox;
    Label19: TLabel;
    TableNameInRefsCBox: TCheckBox;
    Label23: TLabel;
    FKPrefixEd: TEdit;
    Label24: TLabel;
    FKPostfixEd: TEdit;
    CreateFKRefDefIndexCBox: TCheckBox;
    ActivateRefDefForNewRelationsCBox: TCheckBox;
    UsePosGridCBox: TCheckBox;
    GroupBox1: TGroupBox;
    GridWidthLbl: TLabel;
    GridXEd: TEdit;
    GridHeightLbl: TLabel;
    GridYEd: TEdit;
    GridWidthUnitsLbl: TLabel;
    GridHeightUnitsLbl: TLabel;
    PageControlTreeView: TTreeView;
    PageControlTitlePnl: TPanel;
    PageControlTitleLbl: TLabel;
    PageControlTitleShape: TShape;
    DBOptionsSheet: TTabSheet;
    Label7: TLabel;
    DefDatabaseTypeCBox: TComboBox;
    GroupBox4: TGroupBox;
    Label2: TLabel;
    SplashScreenDelayEd: TEdit;
    Label3: TLabel;
    ReopenLastFileCBox: TCheckBox;
    ShowTipsOnStartupCBox: TCheckBox;
    Various: TGroupBox;
    LimitUndoCBox: TCheckBox;
    UndoLimitEd: TEdit;
    EditorsFloatOnTopCBox: TCheckBox;
    ReservedWordsMemo: TMemo;
    Label10: TLabel;
    MinimizeOnCtrlShiftCCBox: TCheckBox;
    GroupBox5: TGroupBox;
    Label12: TLabel;
    LanguageCBox: TComboBox;
    GroupBox6: TGroupBox;
    Label11: TLabel;
    FontCBox: TComboBox;
    SQLFontSizeCBox: TComboBox;
    RenameReservedWordsCBox: TCheckBox;
    EncloseNamesCBox: TCheckBox;
    HTMLBrowserCBox: TCheckBox;
    HTMLBrowserEd: TEdit;
    UseSQLSyntaxHighlightingCBox: TCheckBox;
    IgnoreSQLHistoryChangeCBox: TCheckBox;
    Label13: TLabel;
    FontEdit: TEdit;
    SelectFontBtn: TSpeedButton;
    GroupBox7: TGroupBox;
    TblHeaderPreviewPnl: TPanel;
    TblBorderPnl: TPanel;
    TblColumnsPnl: TPanel;
    TblHeaderBGPnl: TPanel;
    TblHeaderBGImg: TImage;
    TblShadowPnl: TPanel;
    Label6: TLabel;
    Label5: TLabel;
    TblHeaderLU: TComboBox;
    DonotuseRelNameCbox: TCheckBox;
    ActivateNewXMLParserCBox: TCheckBox;
    EditOptionsTipPanel: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    GroupBox8: TGroupBox;
    ResetPersonalSettingsBtn: TBitBtn;
    SyncDatatypesOfForeignKeysCBox: TCheckBox;
    OutputLinuxStyleLineBreaksCBox: TCheckBox;
    AddQuotesToDefValsCBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure DefDatabaseTypeCBoxChange(Sender: TObject);
    procedure TblHeaderLUChange(Sender: TObject);

    procedure GetPageHeaders;
    procedure ResetPersonalSettingsBtnClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
    procedure AbortBtnClick(Sender: TObject);

    procedure ApplyChanges;
    procedure UsePosGridCBoxClick(Sender: TObject);
    procedure SubmitBtnMouseEnter(Sender: TObject);
    procedure SubmitBtnMouseLeave(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure LanguageCBoxCloseUp(Sender: TObject);
    procedure HTMLBrowserCBoxClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SelectFontBtnClick(Sender: TObject);
  private
    { Private declarations }
    DiscardChanges: Boolean;

    LanguageList: TStringList;
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses MainDM, EERDM, DBDM, GUIDM, EER, EditorQuery;

{$R *.xfm}

procedure TOptionsForm.FormCreate(Sender: TObject);
var theIni: TMemIniFile;
  i, langIndex: integer;
begin
  DMMain.InitForm(self);

  DiscardChanges:=False;

  PageControlTreeView.FullExpand;
  PageControl.ActivePageIndex:=0;
  PageControlTreeView.Selected:=PageControlTreeView.Items[0];

  GetPageHeaders;

  //Translate PageControlTreeView Items
  PageControlTreeView.Items[0].Text:=DMMain.GetTranslatedMessage('General Options', 251);
  PageControlTreeView.Items[1].Text:=DMMain.GetTranslatedMessage('Visual Options', 252);
  PageControlTreeView.Items[2].Text:=DMMain.GetTranslatedMessage('Database Options', 253);
  PageControlTreeView.Items[3].Text:=DMMain.GetTranslatedMessage('Default Model Options', 254);


  DefDatabaseTypeCBox.Items.Assign(DMDB.DatabaseTypes);
  DefDatabaseTypeCBox.ItemIndex:=DefDatabaseTypeCBox.Items.IndexOf(DMDB.DefaultDatabaseType);

  //RegionColorsMemo.Lines.Assign(DMEER.RegionColors);
  RegionColorsMemo.Text:=DMEER.DefaultRegionColors;

  SplashScreenDelayEd.Text:=IntToStr(DMGUI.DelaySplashScreen);


  LimitUndoCBox.Checked:=DMEER.LimitUndoActions;
  UndoLimitEd.Text:=IntToStr(DMEER.UndoActionLimit);

  ReopenLastFileCBox.Checked:=DMGUI.ReopenLastFile;

  EditorsFloatOnTopCBox.Checked:=DMMain.NormalizeEditorForms;

  TblHeaderLU.ItemIndex:=TblHeaderLU.Items.IndexOf(DMEER.TblHeaderBGImgs);
  TblHeaderLUChange(self);

  UsePosGridCBox.Checked:=DMEER.UsePositionGrid;
  GridXEd.Text:=IntToStr(DMEER.PositionGrid.X);
  GridYEd.Text:=IntToStr(DMEER.PositionGrid.Y);
  UsePosGridCBoxClick(self);

  TableNameInRefsCBox.Checked:=DMEER.TableNameInRefs;
  DefaultTableTypeCBox.ItemIndex:=DMEER.DefaultTableType;
  ActivateRefDefForNewRelationsCBox.Checked:=DMEER.ActivateRefDefForNewRelations;

  FKPrefixEd.Text:=DMEER.FKPrefix;
  FKPostfixEd.Text:=DMEER.FKPostfix;

  ShowTipsOnStartupCBox.Checked:=DMGUI.ShowTipsOnStartup;

  CreateFKRefDefIndexCBox.Checked:=DMEER.CreateFKRefDefIndex;

  //MySQL_ReservedWords
  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBDesignerFork_DatabaseInfo.ini');
  try
    theIni.ReadSectionValues(DMDB.DefaultDatabaseType+'_ReservedWords', ReservedWordsMemo.Lines);
  finally
    theIni.Free;
  end;
  ReservedWordsMemo.SelStart:=0;
  ReservedWordsMemo.SelLength:=0;

  FontCBox.Items.Assign(Screen.Fonts);
  FontCBox.ItemIndex:=FontCBox.Items.IndexOf(DMGUI.SQLTextFont);

  SQLFontSizeCBox.ItemIndex:=SQLFontSizeCBox.Items.IndexOf(IntToStr(DMGUI.SQLTextFontSize));

{$IFDEF MSWINDOWS}
  if(DMMain.disablePersonalSettings)then
    ResetPersonalSettingsBtn.Visible:=False;
{$ENDIF}

  MinimizeOnCtrlShiftCCBox.Checked:=DMGUI.MinimizeOnCtrlShiftC;

  PageControlTitleLbl.Font.Style:=[fsBold];

  LanguageList:=TStringList.Create;

  theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBDesignerFork_Translations.ini');
  try
    theIni.ReadSectionValues('Languages', LanguageList);

    LanguageCBox.Items.Clear;
    langIndex:=0;
    for i:=0 to LanguageList.Count-1 do
    begin
      if(LanguageList.ValueFromIndex[i]<>'xx')then
      begin
        LanguageCBox.Items.Add(LanguageList.Names[i]);
        if(LanguageList.ValueFromIndex[i]=DMMain.GetLanguageCode)then
          langIndex:=i;
      end;
    end;
    LanguageCBox.ItemIndex:=langIndex;
  finally
    theIni.Free;
  end;

  RenameReservedWordsCBox.Checked:=DMEER.RenameReservedWords;
  EncloseNamesCBox.Checked:=DMEER.EncloseNames;

  HTMLBrowserEd.Text:=DMMain.HTMLBrowserAppl;
  HTMLBrowserCBox.Checked:=(DMMain.HTMLBrowserAppl<>'');
  HTMLBrowserCBoxClick(self);

  UseSQLSyntaxHighlightingCBox.Checked:=DMGUI.UseSQLSyntaxHighlighting;
{$IFNDEF USE_SYNEDIT}
  UseSQLSyntaxHighlightingCBox.Enabled:=False;
{$ENDIF}

  IgnoreSQLHistoryChangeCBox.Checked:=Not(DMGUI.IgnoreSQLHistoryChange);

  FontEdit.Text:=DMMain.ApplicationFontName+', '+
    IntToStr(DMMain.ApplicationFontSize);

  DonotuseRelNameCbox.Checked:=DMEER.DoNotUseRelNameInRefDef;

{$IFDEF USE_IXMLDBMODELType}
  ActivateNewXMLParserCBox.Checked:=DMEER.UseNewXMLParser;
{$ELSE}
  ActivateNewXMLParserCBox.Checked:=True;
  ActivateNewXMLParserCBox.Enabled:=False;
{$ENDIF}

  SyncDatatypesOfForeignKeysCBox.Checked:=DMEER.SyncDatatypesOfForeignKeys;

  OutputLinuxStyleLineBreaksCBox.Checked:=DMEER.OutputLinuxStyleLineBreaks;

  AddQuotesToDefValsCBox.Checked:=DMEER.AddQuotesToDefVals;
end;

procedure TOptionsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if(Not(DiscardChanges))then
    ApplyChanges;

  LanguageList.Free;
end;

procedure TOptionsForm.DefDatabaseTypeCBoxChange(Sender: TObject);
begin
  if(DefDatabaseTypeCBox.ItemIndex<>0)then
    DefDatabaseTypeCBox.ItemIndex:=0;
end;

procedure TOptionsForm.TblHeaderLUChange(Sender: TObject);
var HeaderBGFile, HeaderBGRightFile: string;
begin
  if(TblHeaderLU.ItemIndex>-1)then
  begin
    HeaderBGFile:=ExtractFilePath(Application.ExeName)+PathDelim+'Gfx'+PathDelim+'Table'+PathDelim+'Header_'+TblHeaderLU.Items[TblHeaderLU.ItemIndex]+'.bmp';
    HeaderBGRightFile:=ExtractFilePath(Application.ExeName)+PathDelim+'Gfx'+PathDelim+'Table'+PathDelim+'HeaderRight_'+TblHeaderLU.Items[TblHeaderLU.ItemIndex]+'.bmp';

    if((FileExists(HeaderBGFile))and(FileExists(HeaderBGRightFile)))then
    begin
      TblHeaderBGPnl.Bitmap.LoadFromFile(HeaderBGFile);
      TblHeaderBGPnl.Invalidate;
      TblHeaderBGImg.Picture.LoadFromFile(HeaderBGRightFile);
    end;
  end;
end;

procedure TOptionsForm.GetPageHeaders;
var result: integer;
  SearchRec: TSearchRec;
  HeaderBGRightFile,
  HeaderName: string;
begin
  TblHeaderLU.Items.Clear;

  Result := FindFirst(ExtractFilePath(Application.ExeName)+PathDelim+'Gfx'+PathDelim+'Table'+PathDelim+'Header_*.bmp', 0, SearchRec);
  if(Result<>0)then
    Exit;

  try
    while Result = 0 do
    begin
      HeaderName:=Copy(SearchRec.Name, Length('Header_')+1, Length(SearchRec.Name)-Length('Header_')-4);
      HeaderBGRightFile:=ExtractFilePath(Application.ExeName)+PathDelim+'Gfx'+PathDelim+'Table'+PathDelim+'HeaderRight_'+HeaderName+'.bmp';

      if(Copy(HeaderName, Length(HeaderName)-6, 7)<>'_Linked')and
        (FileExists(HeaderBGRightFile))then
        TblHeaderLU.Items.Add(HeaderName);

      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

procedure TOptionsForm.ResetPersonalSettingsBtnClick(Sender: TObject);
begin
  if(MessageDlg(DMMain.GetTranslatedMessage('This action will overwrite your personal settings.'+#13#10+
    'Are you shure?', 222), mtConfirmation, [mbYes, mbNo], 0)=mrYes)then
    DMGUI.CopyInitialSettingsToPrivatSettingsDir;
end;

procedure TOptionsForm.SubmitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TOptionsForm.AbortBtnClick(Sender: TObject);
begin
  DiscardChanges:=True;

  Close;
end;

procedure TOptionsForm.ApplyChanges;
var i, j: integer;
  theIni: TMemIniFile;
begin
  DMDB.DefaultDatabaseType:=DefDatabaseTypeCBox.Items[DefDatabaseTypeCBox.ItemIndex];

  if(RegionColorsMemo.Lines.Count>0)then
    DMEER.DefaultRegionColors:=RegionColorsMemo.Text;
    //DMEER.RegionColors.Assign(RegionColorsMemo.Lines);

  try
    DMGUI.DelaySplashScreen:=StrToInt(SplashScreenDelayEd.Text);
  except
  end;

  DMEER.LimitUndoActions:=LimitUndoCBox.Checked;
  try
    DMEER.UndoActionLimit:=StrToInt(UndoLimitEd.Text);
  except
  end;

  DMGUI.ReopenLastFile:=ReopenLastFileCBox.Checked;

  DMMain.NormalizeEditorForms:=EditorsFloatOnTopCBox.Checked;

  DMEER.TblHeaderBGImgs:=TblHeaderLU.Items[TblHeaderLU.ItemIndex];

  {if(DMMain.NormalizeEditorForms)then
    DMMain.ApplicationActiveTimer.Enabled:=True;}


  DMEER.UsePositionGrid:=UsePosGridCBox.Checked;
  try
    DMEER.PositionGrid.X:=StrToInt(GridXEd.Text);
    DMEER.PositionGrid.Y:=StrToInt(GridYEd.Text);
  except
    DMEER.PositionGrid.X:=20;
    DMEER.PositionGrid.Y:=20;
  end;

  DMEER.TableNameInRefs:=TableNameInRefsCBox.Checked;
  DMEER.DefaultTableType:=DefaultTableTypeCBox.ItemIndex;
  DMEER.ActivateRefDefForNewRelations:=ActivateRefDefForNewRelationsCBox.Checked;

  DMEER.FKPrefix:=FKPrefixEd.Text;
  DMEER.FKPostfix:=FKPostfixEd.Text;

  DMGUI.ShowTipsOnStartup:=ShowTipsOnStartupCBox.Checked;

  DMEER.CreateFKRefDefIndex:=CreateFKRefDefIndexCBox.Checked;


  //MySQL_ReservedWords
  //Read IniFile
  theIni:=TMemIniFile.Create(DMMain.SettingsPath+'DBDesignerFork_DatabaseInfo.ini');
  try
    theIni.EraseSection(DMDB.DefaultDatabaseType+'_ReservedWords');
    for i:=0 to ReservedWordsMemo.Lines.Count-1 do
      theIni.WriteString(DMDB.DefaultDatabaseType+'_ReservedWords', ReservedWordsMemo.Lines.Names[i],
        ReservedWordsMemo.Lines.ValueFromIndex[i]);
    theIni.UpdateFile;
  finally
    theIni.Free;
  end;

  if(FontCBox.ItemIndex>=0)then
  begin
    DMGUI.SQLTextFont:=FontCBox.Items[FontCBox.ItemIndex];
    DMGUI.SQLTextFontSize:=StrToInt(SQLFontSizeCBox.Items[SQLFontSizeCBox.ItemIndex]);
    sendCLXEvent(Application.MainForm.Handle, QCustomEvent_create(QEventType_SetSQLTextFont, self));
  end;

  DMGUI.MinimizeOnCtrlShiftC:=MinimizeOnCtrlShiftCCBox.Checked;

  DMEER.RenameReservedWords:=RenameReservedWordsCBox.Checked;
  DMEER.EncloseNames:=EncloseNamesCBox.Checked;

  DMMain.HTMLBrowserAppl:=HTMLBrowserEd.Text;

  if(DMGUI.UseSQLSyntaxHighlighting<>UseSQLSyntaxHighlightingCBox.Checked)then
  begin
    DMGUI.UseSQLSyntaxHighlighting:=UseSQLSyntaxHighlightingCBox.Checked;

{$IFDEF USE_SYNEDIT}
    for i:=0 to Screen.FormCount-1 do
      if(Screen.Forms[i] is TEditorQueryForm)then
        if(DMGUI.UseSQLSyntaxHighlighting)then
        begin
          TEditorQueryForm(Screen.Forms[i]).SQLSynEdit.Text:=
            TEditorQueryForm(Screen.Forms[i]).SQLMemo.Text;
          TEditorQueryForm(Screen.Forms[i]).SQLMemo.Visible:=False;
          TEditorQueryForm(Screen.Forms[i]).SQLSynEdit.Visible:=True;
        end
        else
        begin
          TEditorQueryForm(Screen.Forms[i]).SQLSynEdit.Visible:=False;
          TEditorQueryForm(Screen.Forms[i]).SQLMemo.Visible:=True;

          TEditorQueryForm(Screen.Forms[i]).SQLMemo.Lines.Clear;
          for j:=0 to TEditorQueryForm(Screen.Forms[i]).SQLSynEdit.Lines.Count-1 do
            TEditorQueryForm(Screen.Forms[i]).SQLMemo.Lines.Add(
              TEditorQueryForm(Screen.Forms[i]).SQLSynEdit.Lines[j]);
        end;
{$ENDIF}
  end;

  DMGUI.IgnoreSQLHistoryChange:=Not(IgnoreSQLHistoryChangeCBox.Checked);

  for i:=0 to TForm(Application.MainForm).MDIChildCount-1 do
  begin
    if(TForm(Application.MainForm).MDIChildren[i].Classname='TEERForm')then
    begin
      TEERForm(TForm(Application.MainForm).MDIChildren[i]).EERModel.RefreshTblImgs;
      TEERForm(TForm(Application.MainForm).MDIChildren[i]).EERModel.LoadReservedWordsFromIniFile;
    end;
  end;

  DMEER.DoNotUseRelNameInRefDef:=DonotuseRelNameCbox.Checked;

  DMEER.UseNewXMLParser:=ActivateNewXMLParserCBox.Checked;

  DMEER.SyncDatatypesOfForeignKeys:=SyncDatatypesOfForeignKeysCBox.Checked;

  DMEER.OutputLinuxStyleLineBreaks:=OutputLinuxStyleLineBreaksCBox.Checked;

  DMEER.AddQuotesToDefVals:=AddQuotesToDefValsCBox.Checked;
end;

procedure TOptionsForm.UsePosGridCBoxClick(Sender: TObject);
begin
  GridWidthLbl.Enabled:=UsePosGridCBox.Checked;
  GridXEd.Enabled:=UsePosGridCBox.Checked;
  GridWidthUnitsLbl.Enabled:=UsePosGridCBox.Checked;

  GridHeightLbl.Enabled:=UsePosGridCBox.Checked;
  GridYEd.Enabled:=UsePosGridCBox.Checked;
  GridHeightUnitsLbl.Enabled:=UsePosGridCBox.Checked;
end;

procedure TOptionsForm.SubmitBtnMouseEnter(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=True;
end;

procedure TOptionsForm.SubmitBtnMouseLeave(Sender: TObject);
begin
  TSpeedButton(Sender).Enabled:=False;
end;

procedure TOptionsForm.PageControlChange(Sender: TObject);
begin
  EditOptionsTipPanel.Visible:=(PageControl.ActivePage=DefEditingOptionsSheet);
end;

procedure TOptionsForm.PageControlTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  PageControl.ActivePageIndex:=PageControlTreeView.Selected.AbsoluteIndex;
  PageControlTitleLbl.Caption:=PageControlTreeView.Selected.Text;
end;

procedure TOptionsForm.LanguageCBoxCloseUp(Sender: TObject);
begin
  DMMain.SetLanguageCode(LanguageList.ValueFromIndex[LanguageCBox.ItemIndex]);

  //Translate PageControlTreeView Items
  PageControlTreeView.Items[0].Text:=DMMain.GetTranslatedMessage('General Options', 251);
  PageControlTreeView.Items[1].Text:=DMMain.GetTranslatedMessage('Visual Options', 252);
  PageControlTreeView.Items[2].Text:=DMMain.GetTranslatedMessage('Database Options', 253);
  PageControlTreeView.Items[3].Text:=DMMain.GetTranslatedMessage('Default Model Options', 254);
end;

procedure TOptionsForm.HTMLBrowserCBoxClick(Sender: TObject);
begin
  HTMLBrowserEd.Enabled:=HTMLBrowserCBox.Checked;
  if(Not(HTMLBrowserCBox.Checked))then
    HTMLBrowserEd.Text:='';
end;

procedure TOptionsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key=Key_Escape)then
    AbortBtnClick(self);
end;

procedure TOptionsForm.SelectFontBtnClick(Sender: TObject);
var theDlg: TFontDialog;
  i: integer;
begin
  theDlg:=TFontDialog.Create(self);
  try
    theDlg.Font:=Font;
    if(theDlg.Execute)then
    begin
      DMMain.ApplicationFontName:=theDlg.Font.Name;
      DMMain.ApplicationFontSize:=theDlg.Font.Size;
      DMMain.ApplicationFontStyle:=theDlg.Font.Style;

      Application.Font.Name:=DMMain.ApplicationFontName;
      Application.Font.Size:=DMMain.ApplicationFontSize;
      Application.Font.Style:=DMMain.ApplicationFontStyle;

      FontEdit.Text:=DMMain.ApplicationFontName+', '+
        IntToStr(DMMain.ApplicationFontSize);

      for i:=0 to Screen.FormCount-1 do
        DMMain.InitForm(Screen.Forms[i], False, False);
    end;
  finally
    theDlg.Free;
  end;
end;

end.
