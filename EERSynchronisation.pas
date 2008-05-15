unit EERSynchronisation;

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
// Unit EERSynchronisation.pas
// ---------------------------
// Version 1.1, 20.03.2003, Mike
// Description
//   Contains a visual interface for the DB Syncronisation functions
//
// Changes:
//   Version 1.1, 20.03.2003, Mike
//     rename the form, the file and the window caption from
//       syncronisatin to synchronisation
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Qt, SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QButtons, EERModel, QComCtrls;

type
  TEERSynchronisationForm = class(TForm)
    StatusPnl: TPanel;
    ConnectionSBtn: TSpeedButton;
    StatusLbl: TLabel;
    ProgressGroupBox: TGroupBox;
    ProgressMemo: TMemo;
    BottomPnl: TPanel;
    SubmitBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    Bevel1: TBevel;
    TopPnl: TPanel;
    DBConnEd: TEdit;
    Label1: TLabel;
    GetDBConnSBtn: TSpeedButton;
    GroupBox1: TGroupBox;
    Bevel2: TBevel;
    ChangeDBRBtn: TRadioButton;
    ChangeModelRBtn: TRadioButton;
    KeepExistingTabelsCBox: TCheckBox;
    StdInsertsCBox: TCheckBox;
    SyncStdInsertsCBox: TCheckBox;
    LeftPnl: TPanel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

    function SetData(theModel: TEERModel): Boolean;
    procedure GetDBConnSBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);


  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    EERModel: TEERModel;
  end;

var
  EERSynchronisationForm: TEERSynchronisationForm;

implementation

uses MainDM, EERDM, DBDM, DBEERDM;

{$R *.xfm}

procedure TEERSynchronisationForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);
end;

procedure TEERSynchronisationForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TEERSynchronisationForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TEERSynchronisationForm.FormShow(Sender: TObject);
begin
  {Left:=(Screen.Width-Width) div 2;
  Top:=(Screen.Height-Height) div 2;}
end;

function TEERSynchronisationForm.SetData(theModel: TEERModel): Boolean;
begin
  EERModel:=theModel;

  GetDBConnSBtnClick(self);

  //When not connected, close
  SetData:=(DMDB.CurrentDBConn<>nil);
end;

procedure TEERSynchronisationForm.GetDBConnSBtnClick(Sender: TObject);
var SelDBConn: TDBConn;
  theTables: TStringList;
begin
  DBConnEd.Text:='';
  if(Sender.ClassNameIs('TSpeedButton'))then
    DMDB.DisconnectFromDB;

  StatusLbl.Caption:=DMMain.GetTranslatedMessage('Not connected to a Database', 27);
  ConnectionSBtn.Enabled:=False;
  SubmitBtn.Enabled:=False;

  //do until a successful connection is established or the user selects abort
  while(1=1)do
  begin
    //Let the User choose connection if there is no open connection
    if(DMDB.CurrentDBConn=nil)or(EERModel.DefSyncDBConn<>DMDB.CurrentDBConn.Name)then
      SelDBConn:=DMDB.GetUserSelectedDBConn(EERModel.DefSyncDBConn)
    else
      SelDBConn:=DMDB.CurrentDBConn;

    if(SelDBConn<>nil)then
    begin
      //Try to connect to the DB
      try
        DMDB.ConnectToDB(SelDBConn);
      except
        on x: Exception do
        begin
          MessageDlg(DMMain.GetTranslatedMessage('Connection to database failed.'+#13#10#13#10+'%s', 121,
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

      ProgressMemo.Lines.Add(DMMain.GetTranslatedMessage('Connected to Database %s', 28,
        DMDB.CurrentDBConn.Params.Values['User_Name']+'@'+
        DMDB.CurrentDBConn.Params.Values['Database']));

      theTables:=TStringList.Create;
      try
        DMDB.GetDBTables(theTables);

        //Ignore DBDesigner4 table
        if(theTables.IndexOf('DBDesigner4')<>-1)then
          theTables.Delete(theTables.IndexOf('DBDesigner4'));

        ProgressMemo.Lines.Add(DMMain.GetTranslatedMessage('%s Table(s) in Database, '+
          '%s Table(s) in Model.', 221,
          IntToStr(theTables.Count), IntToStr(EERModel.GetEERObjectCount([EERTable])))+#13#10+
          '-------------------------------------');
      finally
        theTables.Free;
      end;

      break;
    end
    else
      break;
  end;
end;

procedure TEERSynchronisationForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult:=mrAbort;
end;

procedure TEERSynchronisationForm.SubmitBtnClick(Sender: TObject);
begin
  EERModel.DefSyncDBConn:=DMDB.CurrentDBConn.Name;
  
  DMDBEER.EERMySQLSyncDB(EERModel, DMDB.CurrentDBConn, ProgressMemo.Lines, KeepExistingTabelsCBox.Checked, StdInsertsCBox.Checked, SyncStdInsertsCBox.Checked);
end;

procedure TEERSynchronisationForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if(Key=Key_F1)then
    DMMain.ShowHelp('db', 'dbsync');
end;

procedure TEERSynchronisationForm.FormResize(Sender: TObject);
begin
  ProgressMemo.Width:=ProgressGroupBox.Width-34;
  ProgressMemo.Height:=ProgressGroupBox.Height-37;
end;

end.
