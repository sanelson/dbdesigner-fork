unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QExtCtrls;

type
  TMainForm = class(TForm)
    ShowImportDlgTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShowImportDlgTimerTimer(Sender: TObject);
  private
    { Private declarations }
    AutoLoadSetting,
    AutoDBConn, AutoDBUser, AutoDBPwd,
    AutoFileName: string;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses MainDM, DBImportData, DBDM;

{$R *.xfm}

procedure TMainForm.FormCreate(Sender: TObject);
var i: integer;
begin
  DMMain:=TDMMain.Create(self);

  DMDB:=TDMDB.Create(self);

  //Example Params
  //-c paketierung -uroot -proot -s MorawaDgWDaten

  AutoLoadSetting:='';
  AutoFileName:='';
  for i:=1 to ParamCount-1 do
  begin
    //DBConn parameter
    if(ParamStr(i)='-c')then
      AutoDBConn:=ParamStr(i+1);

    if(Copy(ParamStr(i), 1, 2)='-u')then
      AutoDBUser:=Copy(ParamStr(i), 3, Length(ParamStr(i)));

    if(Copy(ParamStr(i), 1, 2)='-p')then
      AutoDBPwd:=Copy(ParamStr(i), 3, Length(ParamStr(i)));

    //AutoLoadSetting parameter
    if(ParamStr(i)='-s')then
      AutoLoadSetting:=ParamStr(i+1);

    //filename
    if(ParamStr(i)='-f')then
      AutoFileName:=ParamStr(i+1);
  end;

  ShowImportDlgTimer.Enabled:=True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DMDB.Free;

  DMMain.Free;
end;

procedure TMainForm.ShowImportDlgTimerTimer(Sender: TObject);
var i: integer;
  theAutoDBConn: TDBConn;
begin
  ShowImportDlgTimer.Enabled:=False;

  DBImportDataForm:=TDBImportDataForm.Create(self);
  try
    //If there is an auto-load setting and a auto-DBConn setting
    if(AutoLoadSetting<>'')and(AutoDBConn<>'')then
    begin
      if(AutoFileName='')then
        DBImportDataForm.BrowseDirOrDBConnBtnClick(self)
      else
        DBImportDataForm.SetDirectoryOrFilename(AutoFileName);

      //Connect to database
      theAutoDBConn:=nil;
      for i:=0 to DMDB.DBConnections.Count-1 do
        if(CompareText(TDBConn(DMDB.DBConnections[i]).Name, AutoDBConn)=0)then
        begin
          theAutoDBConn:=TDBConn(DMDB.DBConnections[i]);
          break;
        end;

      //If the given DBConn was found
      if(theAutoDBConn<>nil)then
      begin
        //Set the given user
        if(AutoDBUser<>'')then
          theAutoDBConn.Params.Values['User_Name']:=AutoDBUser;
        if(AutoDBPwd<>'')then
          theAutoDBConn.Params.Values['Password']:=AutoDBPwd;

        //Connect to DB
        DMDB.ConnectToDB(theAutoDBConn);

        if(DMDB.CurrentDBConn<>nil)then
        begin
          //Set DBConn as SourceDBConn
          DBImportDataForm.SetDestDBConn(theAutoDBConn);

          //Now set the Preset
          DBImportDataForm.GetPresetsFromIniFile(AutoLoadSetting);

          //Start Import
          DBImportDataForm.ImportBtnClick(self);

          //Debug:
          //DBImportDataForm.ShowModal;
        end;
      end
      else
        DBImportDataForm.ShowModal;
    end
    else
      DBImportDataForm.ShowModal;

  finally
    DBImportDataForm.Free;
  end;

  Application.Terminate;
end;

end.
