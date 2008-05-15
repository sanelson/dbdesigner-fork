unit EERPlaceModel;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QCheckLst, QExtCtrls, QButtons, EERModel, QMenus,
  QComCtrls, QImgList;

type
  TEERPlaceModelForm = class(TForm)
    PlaceMainMenu: TMainMenu;
    PlaceOptions1: TMenuItem;
    LoadModelfromFileMI: TMenuItem;
    LoadModelfromDatabase1: TMenuItem;
    Selection1: TMenuItem;
    SelectAllMI: TMenuItem;
    N3: TMenuItem;
    RegionsMI: TMenuItem;
    DeselectAllMI: TMenuItem;
    PlaceMI: TMenuItem;
    N2: TMenuItem;
    AbortMI: TMenuItem;
    AddModelMI: TMenuItem;
    LinkModelMI: TMenuItem;
    LMPnl: TPanel;
    PageControlTitleShape: TShape;
    PageControlTitlePnl: TPanel;
    PageControlTitleLbl: TLabel;
    LMTreeView: TTreeView;
    SettingsGroupBox: TGroupBox;
    FNameLbl: TLabel;
    FilenameEd: TEdit;
    LMImgList: TImageList;
    LinkedFromFileRBtn: TRadioButton;
    LinkedFromDBRBtn: TRadioButton;
    Bevel1: TBevel;
    DBConnLbl: TLabel;
    DBConnEd: TEdit;
    DriverLbl: TLabel;
    DriverNameCBox: TComboBox;
    HostLbl: TLabel;
    HostEd: TEdit;
    IPEd: TEdit;
    IPLbl: TLabel;
    DBLbl: TLabel;
    DBEd: TEdit;
    Label1: TLabel;
    ModelIDEd: TEdit;
    UserLbl: TLabel;
    UserEd: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure FormShow(Sender: TObject);

    function SetData(EERModel: TEERModel; P: TPoint; PlaceFrom: integer): Boolean;

    function GetFileNameByOpenDialog: string;
    function LoadModelfromFile: Boolean;
    procedure LoadModelfromFileMIClick(Sender: TObject);
    function LoadModelfromDB: Boolean;
    function LoadModelfromLibrary: Boolean;

    procedure RefreshModel;

    procedure RegionsMIShow(Sender: TObject);
    procedure SelectAllMIClick(Sender: TObject);
    procedure SelectAllMIShow(Sender: TObject);
    procedure AbortMIClick(Sender: TObject);

    procedure AddModelMIClick(Sender: TObject);
    procedure LinkModelMIClick(Sender: TObject);

    procedure AddLinkModel(Link: Boolean);

    procedure DisplayLinkedModels(EERModel: TEERModel);
    procedure RefreshLinkedModelSettings;
    procedure ApplyLinkedModelSettings;
    procedure LMTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure LinkedFromFileRBtnClick(Sender: TObject);
    procedure LMTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);

    procedure RefreshLinkedModel(EERModel: TEERModel; IDLinkedModel: integer = -1);
    procedure RefreshLinkedModelByID(IDLinkedModel: integer);
  private
    { Private declarations }
    EERModel, Model2Place: TEERModel;
    PlacePos: TPoint;

    IsStoredInDB: Boolean;
  public
    { Public declarations }
  end;

const
  PlaceFromFile=1;
  PlaceFromDB=1;
  PlaceFromLibrary=1;

var
  EERPlaceModelForm: TEERPlaceModelForm;

implementation

uses MainDM, GUIDM, DBDM, EERStoreInDatabase;

{$R *.xfm}

procedure TEERPlaceModelForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);

  Model2Place:=nil;
  IsStoredInDB:=False;

  Width:=700;
  Height:=550;
end;

procedure TEERPlaceModelForm.FormDestroy(Sender: TObject);
begin
  {if(Model2Place<>nil)then
    Model2Place.Free;}
end;

procedure TEERPlaceModelForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //Action:=caFree;
  
  ApplyLinkedModelSettings;
end;

procedure TEERPlaceModelForm.FormShow(Sender: TObject);
begin
  //Because of CLX Bug
  AutoScroll:=True;
end;

function TEERPlaceModelForm.SetData(EERModel: TEERModel; P: TPoint; PlaceFrom: integer): Boolean;
begin
  SetData:=True;

  self.EERModel:=EERModel;
  self.PlacePos:=P;

  if(PlaceFrom=PlaceFromFile)then
  begin
    if(Not(LoadModelfromFile))then
      SetData:=False;
  end
  else if(PlaceFrom=PlaceFromDB)then
  begin
    if(Not(LoadModelfromDB))then
      SetData:=False;
  end
  else if(PlaceFrom=PlaceFromLibrary)then
  begin
    if(Not(LoadModelfromLibrary))then
      SetData:=False;  
  end;
end;

function TEERPlaceModelForm.GetFileNameByOpenDialog: string;
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
      GetFileNameByOpenDialog:=theOpenDialog.FileName
    else
      GetFileNameByOpenDialog:='';

  finally
    theOpenDialog.Free;
  end;
end;

function TEERPlaceModelForm.LoadModelfromFile: Boolean;
var fname: string;
begin
  LoadModelfromFile:=False;

  fname:=GetFileNameByOpenDialog;

  if(fname<>'')then
  begin
    if(Model2Place<>nil)then
      Model2Place.Free;

    Model2Place:=TEERModel.Create(self);
    Model2Place.LoadFromFile2(fname);

    Model2Place.ReadOnly:=True;

    //One must not add a model to itself
    if(Model2Place.ModelFilename=EERModel.ModelFilename)then
    begin
      MessageDlg('You must not place a Model on itself.',
        mtError, [mbOK], 0);

      Model2Place.Free;
      Model2Place:=nil;
    end
    else
    begin
      IsStoredInDB:=False;

      RefreshModel;

      DMGUI.RecentOpenFileDir:=ExtractFilePath(fname);
    end;

    if(Visible)then
      SetFocus;

    LoadModelfromFile:=True;
  end;
end;

function TEERPlaceModelForm.LoadModelfromDB: Boolean;
var EERStoreInDatabaseForm: TEERStoreInDatabaseForm;
begin
  LoadModelfromDB:=False;

  EERStoreInDatabaseForm:=TEERStoreInDatabaseForm.Create(self);
  try
    if(Model2Place<>nil)then
      Model2Place.Free;

    Model2Place:=TEERModel.Create(self);

    if(EERStoreInDatabaseForm.SetData(Model2Place, False))then
      if(EERStoreInDatabaseForm.ShowModal=mrOk)then
      begin
        IsStoredInDB:=True;

        RefreshModel;

        if(Visible)then
          SetFocus;

        LoadModelfromDB:=True;        
      end;

  finally
    EERStoreInDatabaseForm.Free;
  end;
end;

function TEERPlaceModelForm.LoadModelfromLibrary: Boolean;
begin
  LoadModelfromLibrary:=False;

  DMMain.LoadValueFromSettingsIniFile('OnlineLibrary', 'HostName', 'onlinelib.fabforce.com');
  DMMain.LoadValueFromSettingsIniFile('OnlineLibrary', 'Database', 'onlinelib');
  DMMain.LoadValueFromSettingsIniFile('OnlineLibrary', 'User_Name', 'onlinelib');
  DMMain.LoadValueFromSettingsIniFile('OnlineLibrary', 'Password', 'ol32!pw');
end;

procedure TEERPlaceModelForm.LoadModelfromFileMIClick(Sender: TObject);
begin
  LoadModelfromFile;
end;

procedure TEERPlaceModelForm.RefreshModel;
var theList: TList;
  i: integer;
  theMenuItem: TMenuItem;
begin
  if(Model2Place=nil)then
    Exit;

  theList:=TList.Create;
  try
    Model2Place.GetEERObjectList([EERRegion], theList, False);

    for i:=0 to RegionsMI.Count-1 do
    begin
      theMenuItem:=RegionsMI[i];
      RegionsMI.Delete(i);
      theMenuItem.Free;
    end;

    for i:=0 to theList.Count-1 do
    begin
      theMenuItem:=TMenuItem.Create(self);
      theMenuItem.Name:='RegionMI'+IntToStr(i);
      theMenuItem.Caption:=TEERRegion(theList[i]).ObjName;
      theMenuItem.OnClick:=RegionsMIShow;
      theMenuItem.Tag:=TEERRegion(theList[i]).Obj_id;
      RegionsMI.Add(theMenuItem);
    end;
  finally
    theList.Free;
  end;
end;

procedure TEERPlaceModelForm.RegionsMIShow(Sender: TObject);
var theEERRegion: TEERRegion;
  theList: TList;
  i: integer;
begin
  theList:=TList.Create;
  try
    theEERRegion:=Model2Place.GetEERObjectByID(TMenuItem(Sender).Tag);
    theEERRegion.GetEERObjsInRegion([EERNote, EERRegion, EERRelation, EERTable, EERImage], theList);

    for i:=0 to theList.Count-1 do
      TEERObj(theList[i]).SetSelected(True);

    theEERRegion.SetSelected(True);

    Model2Place.Refresh;
  finally
    theList.Free;
  end;
end;

procedure TEERPlaceModelForm.SelectAllMIClick(Sender: TObject);
var i: integer;
begin
  if(Model2Place<>nil)then
  begin
    for i:=0 to Model2Place.ComponentCount-1 do
      if(Model2Place.Components[i].ClassParent=TEERObj)then
        TEERObj(Model2Place.Components[i]).SetSelected(TMenuItem(Sender).Tag=1);

    Model2Place.Refresh;
  end;
end;

procedure TEERPlaceModelForm.SelectAllMIShow(Sender: TObject);
begin
  TMenuItem(Sender).Enabled:=(Model2Place<>nil);
end;

procedure TEERPlaceModelForm.AbortMIClick(Sender: TObject);
begin
  ModalResult:=mrAbort;
end;

procedure TEERPlaceModelForm.AddModelMIClick(Sender: TObject);
begin
  AddLinkModel(False);

  ModalResult:=mrOK;
end;

procedure TEERPlaceModelForm.LinkModelMIClick(Sender: TObject);
begin
  AddLinkModel(True);

  ModalResult:=mrOK;
end;

procedure TEERPlaceModelForm.AddLinkModel(Link: Boolean);
var theList: TList;
  i, j, Obj_id: integer;
  theLinkedModel: TEERLinkedModel;
  theObj: TEERObj;
  xoff, yoff: integer;
  theSrcTable, theDestTable: TEERTable;
begin
  if(Model2Place<>nil)then
  begin
    theList:=TList.Create;
    try
      xoff:=10000;
      yoff:=10000;

      for i:=0 to Model2Place.ComponentCount-1 do
        if(Model2Place.Components[i].ClassParent=TEERObj)then
          if(TEERObj(Model2Place.Components[i]).Selected)then
          begin
            theList.Add(Model2Place.Components[i]);

            if(xoff>TEERObj(Model2Place.Components[i]).Obj_X)then
              xoff:=TEERObj(Model2Place.Components[i]).Obj_X;
            if(yoff>TEERObj(Model2Place.Components[i]).Obj_Y)then
              yoff:=TEERObj(Model2Place.Components[i]).Obj_Y;
          end;

      if(theList.Count>0)then
      begin
        theLinkedModel:=nil;
        try
          if(Link)then
          begin
            //--------------------------------------------------
            //Create new TEERLinkedModel
            theLinkedModel:=TEERLinkedModel.Create(EERModel);

            theLinkedModel.ModelName:=Model2Place.GetModelName;
            theLinkedModel.IDModel:=Model2Place.IDModel;
            theLinkedModel.IsStoredInDB:=IsStoredInDB;
            if(IsStoredInDB)then
            begin
              theLinkedModel.DriverName:=DMDB.CurrentDBConn.DriverName;
              theLinkedModel.DBConnName:=DMDB.CurrentDBConn.Name;
              theLinkedModel.HostCaption:=DMDB.CurrentDBConn.Params.Values['HostCaption'];
              theLinkedModel.HostName:=DMDB.CurrentDBConn.Params.Values['HostName'];
              theLinkedModel.Database:=DMDB.CurrentDBConn.Params.Values['Database'];
              theLinkedModel.User:=DMDB.CurrentDBConn.Params.Values['User_Name'];
            end
            else
            begin
              theLinkedModel.ModelFilename:=
                ExtractRelativePath(ExtractFilePath(Application.ExeName),
                  Model2Place.ModelFilename);

              //To expand the RelativePath
              //fname:=ExpandFileName(theLinkedModel.ModelFilename)
            end;

            EERModel.LinkedModels.Add(theLinkedModel);
          end;

          //--------------------------------------------------
          //Check and add Datatypes

          //--------------------------------------------------
          //Add the Objects except Relations
          for i:=0 to theList.Count-1 do
          begin
            if(TEERObj(theList[i]).ClassNameIs('TEERRegion'))then
            begin
              theObj:=EERModel.NewRegion(0, 0,
                TEERRegion(theList[i]).Obj_W,
                TEERRegion(theList[i]).Obj_H,
                False);

              Obj_id:=theObj.Obj_id;
              TEERRegion(theObj).Assign(TEERRegion(theList[i]));
              theObj.ParentEERModel:=EERModel;
              theObj.Parent:=EERModel;
              theObj.Obj_id:=Obj_id;
              theObj.Obj_X:=TEERRegion(theList[i]).Obj_X+PlacePos.X-xoff;
              theObj.Obj_Y:=TEERRegion(theList[i]).Obj_Y+PlacePos.Y-yoff;
              if(Link)then
              begin
                theObj.IsLinkedObject:=True;
                theObj.IDLinkedModel:=theLinkedModel.IDLinkedModel;
                theObj.Obj_id_Linked:=TEERObj(theList[i]).Obj_id;
              end;

              theObj.RefreshObj;
            end
            else if(TEERObj(theList[i]).ClassNameIs('TEERImage'))then
            begin
              theObj:=EERModel.NewImage(0, 0,
                TEERImage(theList[i]).Obj_W,
                TEERImage(theList[i]).Obj_H,
                False);

              Obj_id:=theObj.Obj_id;
              TEERImage(theObj).Assign(TEERImage(theList[i]));
              theObj.ParentEERModel:=EERModel;
              theObj.Parent:=EERModel;
              theObj.Obj_id:=Obj_id;
              theObj.Obj_X:=TEERImage(theList[i]).Obj_X+PlacePos.X-xoff;
              theObj.Obj_Y:=TEERImage(theList[i]).Obj_Y+PlacePos.Y-yoff;
              if(Link)then
              begin
                theObj.IsLinkedObject:=True;
                theObj.IDLinkedModel:=theLinkedModel.IDLinkedModel;
                theObj.Obj_id_Linked:=TEERObj(theList[i]).Obj_id;
              end;

              theObj.RefreshObj;
            end
            else if(TEERObj(theList[i]).ClassNameIs('TEERNote'))then
            begin
              theObj:=EERModel.NewNote(0, 0,
                False);

              Obj_id:=theObj.Obj_id;
              TEERNote(theObj).Assign(TEERNote(theList[i]));
              theObj.ParentEERModel:=EERModel;
              theObj.Parent:=EERModel;
              theObj.Obj_id:=Obj_id;
              theObj.Obj_X:=TEERNote(theList[i]).Obj_X+PlacePos.X-xoff;
              theObj.Obj_Y:=TEERNote(theList[i]).Obj_Y+PlacePos.Y-yoff;
              if(Link)then
              begin
                theObj.IsLinkedObject:=True;
                theObj.IDLinkedModel:=theLinkedModel.IDLinkedModel;
                theObj.Obj_id_Linked:=TEERObj(theList[i]).Obj_id;
              end;

              theObj.RefreshObj;
            end
            else if(TEERObj(theList[i]).ClassNameIs('TEERTable'))then
            begin
              theObj:=EERModel.NewTable(0, 0,
                False);

              Obj_id:=theObj.Obj_id;
              TEERTable(theObj).Assign(TEERTable(theList[i]));
              theObj.ParentEERModel:=EERModel;
              theObj.Parent:=EERModel;
              theObj.Obj_id:=Obj_id;
              theObj.Obj_X:=TEERTable(theList[i]).Obj_X+PlacePos.X-xoff;
              theObj.Obj_Y:=TEERTable(theList[i]).Obj_Y+PlacePos.Y-yoff;
              if(Link)then
              begin
                theObj.IsLinkedObject:=True;
                theObj.IDLinkedModel:=theLinkedModel.IDLinkedModel;
                //theObj.Obj_id_Linked:=TEERObj(theList[i]).Obj_id;
              end;
              // Add LinkedID either way to enable reestabling relations
              theObj.Obj_id_Linked:=TEERObj(theList[i]).Obj_id;

              TEERTable(theObj).RelStart.Clear;
              TEERTable(theObj).RelEnd.Clear;
              for j:=1 to 4 do
                TEERTable(theObj).Rel[j].Clear;

              theObj.RefreshObj;
            end;
          end;

          //--------------------------------------------------
          //Add Relations
          for i:=0 to theList.Count-1 do
          begin
            if(TEERObj(theList[i]).ClassNameIs('TEERRel'))then
            begin
              theSrcTable:=EERModel.GetEERObjectByLinkedID(
                TEERRel(theList[i]).SrcTbl.Obj_id);
              theDestTable:=EERModel.GetEERObjectByLinkedID(
                TEERRel(theList[i]).DestTbl.Obj_id);

              //Only add Relation if source and dest table are in the model
              if(theSrcTable<>nil)and(theDestTable<>nil)then
              begin
                theObj:=EERModel.NewRelation(TEERRel(theList[i]).RelKind,
                  theSrcTable, theDestTable, False);

                Obj_id:=theObj.Obj_id;
                TEERRel(theObj).Assign(TEERRel(theList[i]));
                theObj.ParentEERModel:=EERModel;
                theObj.Parent:=EERModel;
                theObj.Obj_id:=Obj_id;
                theObj.Obj_X:=TEERNote(theList[i]).Obj_X+PlacePos.X-xoff;
                theObj.Obj_Y:=TEERNote(theList[i]).Obj_Y+PlacePos.Y-yoff;
                TEERRel(theObj).SrcTbl:=theSrcTable;
                TEERRel(theObj).DestTbl:=theDestTable;
                if(Link)then
                begin
                  theObj.IsLinkedObject:=True;
                  theObj.IDLinkedModel:=theLinkedModel.IDLinkedModel;
                  theObj.Obj_id_Linked:=TEERObj(theList[i]).Obj_id;
                end;

                theObj.RefreshObj;
              end;
            end;
          end;

          //If the model is not linked, remove temporary Obj_id_Linked from
          //tables which were used while adding relations
          if(Not(Link))then
            for i:=0 to theList.Count-1 do
               TEERObj(theList[i]).Obj_id_Linked:=0;

          EERModel.ModelHasChanged;

          EERModel.Refresh;
        except
          if(Link)then
            theLinkedModel.Free;
        end;
      end
      else
        MessageDlg(DMMain.GetTranslatedMessage('You have to select at least one Object.', 243),
          mtInformation, [mbOK], 0);
    finally
      theList.Free;
    end;
  end;
end;

procedure TEERPlaceModelForm.DisplayLinkedModels(EERModel: TEERModel);
var i, j: integer;
  theLMNode, theTreeNode: TTreeNode;
begin
  self.EERModel:=EERModel;

  Caption:='Linked Models';

  Menu:=nil;
  LMPnl.Visible:=True;

  Width:=LMPnl.Width;
  Height:=LMPnl.Height;

  DriverNameCBox.Items.Text:=DMDB.DatabaseTypes.Text;

  for i:=0 to EERModel.LinkedModels.Count-1 do
  begin
    theLMNode:=LMTreeView.Items.Add(nil,
      TEERLinkedModel(EERModel.LinkedModels[i]).ModelName);
    theLMNode.Data:=EERModel.LinkedModels[i];
    theLMNode.ImageIndex:=0;

    for j:=0 to EERModel.ComponentCount-1 do
      if(EERModel.Components[j].ClassParent=TEERObj)then
        if(TEERObj(EERModel.Components[j]).IsLinkedObject)and
          (TEERObj(EERModel.Components[j]).IDLinkedModel=
            TEERLinkedModel(EERModel.LinkedModels[i]).IDLinkedModel)then
        begin
          theTreeNode:=LMTreeView.Items.AddChild(theLMNode,
            TEERObj(EERModel.Components[j]).ObjName);
          theTreeNode.Data:=EERModel.Components[j];

          if(EERModel.Components[j] is TEERRegion)then
            theTreeNode.ImageIndex:=1
          else if(EERModel.Components[j] is TEERTable)then
            theTreeNode.ImageIndex:=2
          else if(EERModel.Components[j] is TEERRel)then
            theTreeNode.ImageIndex:=3
          else if(EERModel.Components[j] is TEERStoredProc)then
            theTreeNode.ImageIndex:=4
          else if(EERModel.Components[j] is TEERNote)then
            theTreeNode.ImageIndex:=5
          else if(EERModel.Components[j] is TEERImage)then
            theTreeNode.ImageIndex:=6;
        end;

  end;

  if(EERModel.LinkedModels.Count>0)then
    LMTreeView.Selected:=LMTreeView.Items[0];

  RefreshLinkedModelSettings;
end;

procedure TEERPlaceModelForm.RefreshLinkedModelSettings;
begin
  if(LMTreeView.Selected<>nil)then
    if(LMTreeView.Selected.Data<>nil)then
    begin
      if(TObject(LMTreeView.Selected.Data) is TEERLinkedModel)then
      begin
        if(Not(TEERLinkedModel(LMTreeView.Selected.Data).IsStoredInDB))then
        begin
          LinkedFromFileRBtn.Enabled:=True;
          LinkedFromDBRBtn.Enabled:=True;
          LinkedFromFileRBtn.Checked:=True;

          FilenameEd.Enabled:=True;
          FilenameEd.Text:=TEERLinkedModel(LMTreeView.Selected.Data).ModelFilename;

          DBConnEd.Enabled:=False;
          DBConnEd.Text:='';
          DriverNameCBox.Enabled:=False;
          DriverNameCBox.ItemIndex:=-1;
          HostEd.Enabled:=False;
          HostEd.Text:='';
          IPEd.Enabled:=False;
          IPEd.Text:='';
          DBEd.Enabled:=False;
          DBEd.Text:='';
          ModelIDEd.Enabled:=False;
          ModelIDEd.Text:='';
          UserEd.Enabled:=False;
          UserEd.Text:='';
        end
        else
        begin
          LinkedFromFileRBtn.Enabled:=True;
          LinkedFromDBRBtn.Enabled:=True;
          LinkedFromDBRBtn.Checked:=True;

          FilenameEd.Enabled:=False;
          FilenameEd.Text:='';

          DBConnEd.Enabled:=True;
          DBConnEd.Text:=TEERLinkedModel(LMTreeView.Selected.Data).DBConnName;
          DriverNameCBox.Enabled:=True;
          DriverNameCBox.ItemIndex:=DriverNameCBox.Items.IndexOf(TEERLinkedModel(LMTreeView.Selected.Data).DriverName);
          HostEd.Enabled:=True;
          HostEd.Text:=TEERLinkedModel(LMTreeView.Selected.Data).HostCaption;
          IPEd.Enabled:=True;
          IPEd.Text:=TEERLinkedModel(LMTreeView.Selected.Data).HostName;
          DBEd.Enabled:=True;
          DBEd.Text:=TEERLinkedModel(LMTreeView.Selected.Data).Database;
          ModelIDEd.Enabled:=True;
          ModelIDEd.Text:=IntToStr(TEERLinkedModel(LMTreeView.Selected.Data).IDModel);
          UserEd.Enabled:=True;
          UserEd.Text:=TEERLinkedModel(LMTreeView.Selected.Data).User;
        end;
      end
      else
      begin
        LinkedFromFileRBtn.Enabled:=False;
        LinkedFromDBRBtn.Enabled:=False;

        FilenameEd.Enabled:=False;
        FilenameEd.Text:='';

        DBConnEd.Enabled:=False;
        DBConnEd.Text:='';
        DriverNameCBox.Enabled:=False;
        DriverNameCBox.ItemIndex:=-1;
        HostEd.Enabled:=False;
        HostEd.Text:='';
        IPEd.Enabled:=False;
        IPEd.Text:='';
        DBEd.Enabled:=False;
        DBEd.Text:='';
        ModelIDEd.Enabled:=False;
        ModelIDEd.Text:='';
        UserEd.Enabled:=False;
        UserEd.Text:='';
      end;
    end;
end;

procedure TEERPlaceModelForm.ApplyLinkedModelSettings;
begin
  if(LMTreeView.Selected<>nil)then
    if(LMTreeView.Selected.Data<>nil)then
      if(TObject(LMTreeView.Selected.Data) is TEERLinkedModel)then
        if(LinkedFromFileRBtn.Checked)and(FilenameEd.Enabled)then
        begin
          TEERLinkedModel(LMTreeView.Selected.Data).ModelFilename:=
            FilenameEd.Text;
        end
        else if(LinkedFromDBRBtn.Checked)and(DBConnEd.Enabled)then
        begin
          TEERLinkedModel(LMTreeView.Selected.Data).DBConnName:=DBConnEd.Text;
          if(DriverNameCBox.ItemIndex>-1)then
            TEERLinkedModel(LMTreeView.Selected.Data).DriverName:=DriverNameCBox.Items[DriverNameCBox.ItemIndex]
          else
            TEERLinkedModel(LMTreeView.Selected.Data).DriverName:='';
          TEERLinkedModel(LMTreeView.Selected.Data).HostCaption:=HostEd.Text;
          TEERLinkedModel(LMTreeView.Selected.Data).HostName:=IPEd.Text;
          TEERLinkedModel(LMTreeView.Selected.Data).Database:=DBEd.Text;
          TEERLinkedModel(LMTreeView.Selected.Data).IDModel:=StrToInt(ModelIDEd.Text);
          TEERLinkedModel(LMTreeView.Selected.Data).User:=UserEd.Text;
        end;
end;

procedure TEERPlaceModelForm.LMTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  RefreshLinkedModelSettings;
end;

procedure TEERPlaceModelForm.LinkedFromFileRBtnClick(Sender: TObject);
begin
  if(LMTreeView.Selected<>nil)then
    if(LMTreeView.Selected.Data<>nil)then
      if(TObject(LMTreeView.Selected.Data) is TEERLinkedModel)then
      begin
        TEERLinkedModel(LMTreeView.Selected.Data).IsStoredInDB:=
          LinkedFromDBRBtn.Checked;

        RefreshLinkedModelSettings;
      end;
end;

procedure TEERPlaceModelForm.LMTreeViewChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  ApplyLinkedModelSettings;
end;

procedure TEERPlaceModelForm.RefreshLinkedModel(EERModel: TEERModel; IDLinkedModel: integer = -1);
var i: integer;
begin
  self.EERModel:=EERModel;

  //Refresh Linked Model with given ID
  if(IDLinkedModel>-1)then
    RefreshLinkedModelByID(IDLinkedModel)
  else
    //Refresh all Linked Models
    for i:=0 to EERModel.LinkedModels.Count-1 do
      RefreshLinkedModelByID(TEERLinkedModel(EERModel.LinkedModels[i]).IDLinkedModel);
end;

procedure TEERPlaceModelForm.RefreshLinkedModelByID(IDLinkedModel: integer);
var theLinkedModel: TEERLinkedModel;
  s, fname: string;
  i, j, Obj_id: integer;
  x, y, w, h: integer;
  theObj, theLinkedObj: TEERObj;
  MessageResult: Integer;

  tmpRelStart, tmpRelEnd: TList;
  tmpRel: Array [1..4] of TList;

  theSrcTable, theDestTable: TEERTable;

  theDBConn: TDBConn;

  EERStoreInDatabaseForm: TEERStoreInDatabaseForm;
begin
  theLinkedModel:=EERModel.GetPlacedModelByID(IDLinkedModel);
  if(theLinkedModel<>nil)then
  begin
    // ---------------------------------------------
    // Load the Linked Model

    if(theLinkedModel.IsStoredInDB)then
      s:=theLinkedModel.Database+'@'+theLinkedModel.HostCaption
    else
      s:=theLinkedModel.ModelFilename;

    DMGUI.SetStatusCaption(DMMain.GetTranslatedMessage(
      'Refresh Linked Objects from Model [%s] stored in [%s].', 249,
      theLinkedModel.ModelName, s));

    if(Model2Place<>nil)then
      Model2Place.Free;

    Model2Place:=TEERModel.Create(self);

    //Load Linked Model from File
    if(Not(theLinkedModel.IsStoredInDB))then
    begin
      ChDir(ExtractFilePath(Application.ExeName));
      fname:=ExpandFileName(theLinkedModel.ModelFilename);

      if(Not(FileExists(fname)))then
      begin
        if(MessageDlg(DMMain.GetTranslatedMessage('The Linked Model %s could not be found at the stored position.\n'+
          'Filename: %s\n\n'+
          'Do you want to select the file manually?', 246, theLinkedModel.Modelname, fname), mtError, [mbYes, mbNo], 0)=mrYes)then
        begin
          fname:=GetFileNameByOpenDialog;

          theLinkedModel.ModelFilename:=
            ExtractRelativePath(ExtractFilePath(Application.ExeName),
              fname);
        end
        else
          Exit;
      end;

      Model2Place.LoadFromFile2(fname);
    end
    else
    begin
      //Load Linked Model from DB

      //Check DBConn by Name
      theDBConn:=nil;
      for i:=0 to DMDB.DBConnections.Count-1 do
        if(CompareText(theLinkedModel.DBConnName,
            TDBConn(DMDB.DBConnections[i]).Name)=0)and
          (CompareText(theLinkedModel.DriverName,
            TDBConn(DMDB.DBConnections[i]).DriverName)=0)and
          (CompareText(theLinkedModel.HostName,
            TDBConn(DMDB.DBConnections[i]).Params.Values['HostName'])=0)and
          (CompareText(theLinkedModel.Database,
            TDBConn(DMDB.DBConnections[i]).Params.Values['Database'])=0)then
        begin
          theDBConn:=TDBConn(DMDB.DBConnections[i]);
          break;
        end;

      //If DBConn is not found, check for DBConn to same DB
      // with different DBConn Name
      if(theDBConn=nil)then
        for i:=0 to DMDB.DBConnections.Count-1 do
          if(CompareText(theLinkedModel.DriverName,
              TDBConn(DMDB.DBConnections[i]).DriverName)=0)and
            (CompareText(theLinkedModel.HostName,
            TDBConn(DMDB.DBConnections[i]).Params.Values['HostName'])=0)and
          (CompareText(theLinkedModel.Database,
            TDBConn(DMDB.DBConnections[i]).Params.Values['Database'])=0)then
          begin
            theDBConn:=TDBConn(DMDB.DBConnections[i]);
            break;
          end;

      EERStoreInDatabaseForm:=TEERStoreInDatabaseForm.Create(self);
      try
        if(EERStoreInDatabaseForm.SetData(Model2Place, False, theDBConn))then
        begin
          //Select Model in Database by ID
          for i:=0 to EERStoreInDatabaseForm.SavedModelsTV.Items.Count-1 do
            if(EERStoreInDatabaseForm.SavedModelsTV.Items[i].SubItems[3]=IntToStr(theLinkedModel.IDModel))then
            begin
              EERStoreInDatabaseForm.SavedModelsTV.Selected:=EERStoreInDatabaseForm.SavedModelsTV.Items[i];
              break;
            end;

          //If the model is found by ID, open it
          if(EERStoreInDatabaseForm.SavedModelsTV.Selected.SubItems[3]=IntToStr(theLinkedModel.IDModel))then
            EERStoreInDatabaseForm.SubmitBtnClick(self)
          else
            //Let the user choose the model
            if(EERStoreInDatabaseForm.ShowModal<>mrOK)then
              Exit;
        end
        else
          Exit;
      finally
        EERStoreInDatabaseForm.Free;
      end;
    end;

    Model2Place.ReadOnly:=True;

    // ---------------------------------------------
    //Refresh Objects

    tmpRelStart:=TList.Create;
    tmpRelEnd:=TList.Create;
    for j:=1 to 4 do
      tmpRel[j]:=TList.Create;

    try
      //All Objects except Relations
      i:=0;
      while(i<=EERModel.ComponentCount-1)do
      begin
        if(EERModel.Components[i].ClassParent=TEERObj)then
          if(TEERObj(EERModel.Components[i]).IsLinkedObject)and
            (TEERObj(EERModel.Components[i]).IDLinkedModel=theLinkedModel.IDLinkedModel)and
            (Not(TEERObj(EERModel.Components[i]) is TEERRel))then
          begin
            theObj:=TEERObj(EERModel.Components[i]);
            theLinkedObj:=Model2Place.GetEERObjectByID(TEERObj(EERModel.Components[i]).Obj_id_Linked);

            if(theLinkedObj<>nil)then
            begin
              //Store general Obj settings
              if(theObj.Classname=theLinkedObj.Classname)then
              begin
                Obj_id:=theObj.Obj_id;
                x:=theObj.Obj_X;
                y:=theObj.Obj_Y;
                w:=theObj.Obj_W;
                h:=theObj.Obj_H;

                if(theObj is TEERTable)then
                begin
                  //Store Relations Lists
                  tmpRelStart.Assign(TEERTable(theObj).RelStart);
                  tmpRelEnd.Assign(TEERTable(theObj).RelEnd);
                  for j:=1 to 4 do
                    tmpRel[j].Assign(TEERTable(theObj).Rel[j]);

                  TEERTable(theObj).Assign(TEERTable(theLinkedObj));

                  //Re-Store Relations Lists
                  TEERTable(theObj).RelStart.Assign(tmpRelStart);
                  TEERTable(theObj).RelEnd.Assign(tmpRelEnd);
                  for j:=1 to 4 do
                    TEERTable(theObj).Rel[j].Assign(tmpRel[j]);
                end
                else if(theObj is TEERRegion)then
                begin
                  TEERRegion(theObj).Assign(TEERRegion(theLinkedObj));
                end
                else if(theObj is TEERImage)then
                begin
                  TEERImage(theObj).Assign(TEERImage(theLinkedObj));
                end
                else if(theObj is TEERNote)then
                begin
                  TEERNote(theObj).Assign(TEERNote(theLinkedObj));
                end;

                //ReStore general Obj settings
                theObj.ParentEERModel:=EERModel;
                theObj.Parent:=EERModel;
                theObj.Obj_id:=Obj_id;
                theObj.Obj_X:=x;
                theObj.Obj_Y:=y;
                theObj.Obj_W:=w;
                theObj.Obj_H:=h;
                theObj.IsLinkedObject:=True;
                theObj.IDLinkedModel:=theLinkedModel.IDLinkedModel;
                theObj.Obj_id_Linked:=TEERObj(theLinkedObj).Obj_id;

                theObj.RefreshObj;
              end
              else
              begin
                //Object Type has wrong type, so ask for delete
                MessageResult:=MessageDlg(DMMain.GetTranslatedMessage(
                  'The Object %s has a different Type in the Linked Model\n'+
                  'Do you want to delete the Object or abort the Refresh?',
                  247, theObj.ObjName), mtConfirmation,
                  [mbYes, mbNo, mbAbort], 0);

                if(MessageResult=mrYes)then
                begin
                  TEERObj(EERModel.Components[i]).DeleteObj;

                  continue;
                end
                else if(MessageResult=mrAbort)then
                  Exit;
              end;
            end
            else
            begin
              //Object has been deleted
              TEERObj(EERModel.Components[i]).DeleteObj;
              continue;
            end;
          end;

        inc(i);
      end;

      //All Relations
      i:=0;
      while(i<=EERModel.ComponentCount-1)do
      begin
        if(EERModel.Components[i].ClassParent=TEERObj)then
          if(TEERObj(EERModel.Components[i]).IsLinkedObject)and
            (TEERObj(EERModel.Components[i]).IDLinkedModel=theLinkedModel.IDLinkedModel)and
            (TEERObj(EERModel.Components[i]) is TEERRel)then
          begin
            theObj:=TEERObj(EERModel.Components[i]);
            theLinkedObj:=Model2Place.GetEERObjectByID(TEERObj(EERModel.Components[i]).Obj_id_Linked);

            if(theLinkedObj<>nil)then
            begin
              theSrcTable:=EERModel.GetEERObjectByLinkedID(
                TEERRel(theLinkedObj).SrcTbl.Obj_id);
              theDestTable:=EERModel.GetEERObjectByLinkedID(
                TEERRel(theLinkedObj).DestTbl.Obj_id);

              //Only add Relation if source and dest table are in the model
              if(theSrcTable<>nil)and(theDestTable<>nil)then
              begin
                Obj_id:=theObj.Obj_id;
                x:=theObj.Obj_X;
                y:=theObj.Obj_Y;
                w:=theObj.Obj_W;
                h:=theObj.Obj_H;

                TEERRel(theObj).SrcTbl:=theSrcTable;
                TEERRel(theObj).DestTbl:=theDestTable;

                //ReStore general Obj settings
                theObj.ParentEERModel:=EERModel;
                theObj.Parent:=EERModel;
                theObj.Obj_id:=Obj_id;
                theObj.Obj_X:=x;
                theObj.Obj_Y:=y;
                theObj.Obj_W:=w;
                theObj.Obj_H:=h;
                theObj.IsLinkedObject:=True;
                theObj.IDLinkedModel:=theLinkedModel.IDLinkedModel;
                theObj.Obj_id_Linked:=TEERObj(theLinkedObj).Obj_id;

                theObj.RefreshObj;
              end
              else
              begin
                TEERObj(EERModel.Components[i]).DeleteObj;
                continue;
              end;
            end
            else
            begin
              //Object has been deleted
              TEERObj(EERModel.Components[i]).DeleteObj;
              continue;
            end;
          end;

        inc(i);
      end;
    finally
      tmpRelStart.Free;
      tmpRelEnd.Free;

      for j:=1 to 4 do
        tmpRel[j].Free;
    end;

    EERModel.ModelHasChanged;

    EERModel.Refresh;
  end;

  DMGUI.SetStatusCaption(DMMain.GetTranslatedMessage(
    'Linked Objects have been refreshed.', 250));
end;

end.
