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
// Version 1.0, 10.01.2003, Mike
// Description
//   MainForm of the Demo Plugin
//
// Changes:
//   Version 1.0, 10.01.2003, Mike
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
    MainBevel: TBevel;
    Label1: TLabel;
    TablesCheckListBox: TCheckListBox;
    SQLMemo: TMemo;
    Label2: TLabel;
    OptionsGroupBox: TGroupBox;
    DefPKCheckBox: TCheckBox;
    CreateIndicesCheckBox: TCheckBox;
    DefFKCheckBox: TCheckBox;
    TblOptionsCheckBox: TCheckBox;
    StdInsertCBox: TCheckBox;
    OutputBevel: TBevel;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Help1: TMenuItem;
    ExitMI: TMenuItem;
    DisplayMI: TMenuItem;
    ShowModelMI: TMenuItem;
    ShowControlsMI: TMenuItem;
    AboutMI: TMenuItem;
    MenuBevel: TBevel;
    SubmitBtn: TSpeedButton;
    CancelBtn: TSpeedButton;
    GroupBox1: TGroupBox;
    ActionComboBox: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    //Initialize this form's controls with data from the loaded model
    procedure InitControls;

    procedure CancelBtnClick(Sender: TObject);
    procedure SubmitBtnClick(Sender: TObject);

    //Show About Box
    procedure AboutMIClick(Sender: TObject);

    //Show or Hide the Controls/the EER Model
    procedure ShowControlsMIClick(Sender: TObject);

    //Display the options after CloseUp of the Combo Box
    procedure ActionComboBoxCloseUp(Sender: TObject);
  private
    { Private declarations }
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

  if(ParamCount<1)then
  begin
    MessageDlg('You have to specify a model. '+#13#10#13#10+
      'Usage: DBDplugin_Demo modelfilename.xml', mtError,
      [mbOK], 0);
    Application.Terminate;
  end
  else
  begin
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
        EERModel.LoadFromFile(ParamStr(i), True, False, False);
        //Set the current worktool to Pointer
        DMEER.SetCurrentWorkTool(wtPointer);

        //Initialize this form's controls with data from the model
        InitControls;

        //Only load first file
        break;
      end;
    end;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //Free the EERModel
  EERModel.Free;
end;

procedure TMainForm.InitControls;
var theTableNames: TStringList;
  i: integer;
begin
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

  //Select all items in the Listbox
  for i:=0 to TablesCheckListBox.Items.Count-1 do
    TablesCheckListBox.Checked[i]:=True;
end;

procedure TMainForm.CancelBtnClick(Sender: TObject);
begin
  //When the user presses Cancel, close the program
  Close;
end;

procedure TMainForm.SubmitBtnClick(Sender: TObject);
var i, j: integer;
  theTable: TEERTable;
  theColumn: TEERColumn;
begin
  //Clear output memo
  SQLMemo.Lines.Clear;

  //Depending on the action selected by the user
  //execute the appropriate code

  //-----------------------------------------------------
  //List all selected Tables and their fields
  if(ActionComboBox.ItemIndex=0)then
  begin
    //Do for all tables in the Listbox
    for i:=0 to TablesCheckListBox.Items.Count-1 do
    begin
      //Only if the table is selected by the user
      if(TablesCheckListBox.Checked[i])then
      begin
        //Get the table from the mode
        theTable:=EERModel.GetEERObjectByIndex(EERTable, i);

        //print the table's name
        SQLMemo.Lines.Add(theTable.ObjName+#13#10+
          StringOfChar('-', Length(theTable.ObjName)));

        //print the table's number of columns
        SQLMemo.Lines.Add('    Number of Columns: '+IntToStr(theTable.GetColumnCount));

        for j:=0 to theTable.GetColumnCount-1 do
        begin
          //Get the column
          theColumn:=theTable.GetColumnByIndex(j);

          //Output the columns name and datatype
          SQLMemo.Lines.Add('    '+theColumn.ColName+' '+
            EERModel.GetDataTypeName(theColumn.idDatatype));
        end;

        SQLMemo.Lines.Add('');
      end;
    end;
  end
  //-----------------------------------------------------
  //Display the SQL Create Commands for all selected tables
  else if(ActionComboBox.ItemIndex=1)then
  begin
    //Do for all tables in the Listbox
    for i:=0 to TablesCheckListBox.Items.Count-1 do
    begin
      //Only if the table is selected by the user
      if(TablesCheckListBox.Checked[i])then
        //Call the table's GetSQLCreateCode function
        SQLMemo.Lines.Add(
          TEERTable(EERModel.GetEERObjectByIndex(EERTable, i)).GetSQLCreateCode(
            DefPKCheckBox.Checked,
            CreateIndicesCheckBox.Checked,
            DefFKCheckBox.Checked,
            TblOptionsCheckBox.Checked,
            StdInsertCBox.Checked)+
            #13#10);
    end;
  end;
end;

procedure TMainForm.AboutMIClick(Sender: TObject);
begin
  ShowMessage('DBDesigner 4 Demo Plugin'#13#10#13#10+
    'This is a simple demonstration of the DBDesigner 4''s plugin system.'#13#10#13#10+
    'Select an action and press [Execute] to see the output.');
end;

procedure TMainForm.ShowControlsMIClick(Sender: TObject);
begin
  //This is called from ShowControlsMI and ShowModelMI

  //Check the selected MenuItem, the other MenuItem will be unchecked automatically
  TMenuItem(Sender).Checked:=True;

  //Hide Model
  if(Sender=ShowControlsMI)then
    EERModel.Visible:=False
  else
  //Show the Model
  begin
    EERModel.Visible:=True;
    EERModel.BringToFront;
  end;

end;

procedure TMainForm.ActionComboBoxCloseUp(Sender: TObject);
begin
  //Display the options if 'SQL Creates' is selected by the user
  if(ActionComboBox.ItemIndex=1)then
    OptionsGroupBox.Visible:=True
  else
    OptionsGroupBox.Visible:=False;
end;

end.
