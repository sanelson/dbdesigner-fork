unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DBXpress, DB, SqlExpr, StdCtrls;

type
  TForm1 = class(TForm)
    SQLConnection: TSQLConnection;
    TTNS: TLabel;
    ETNS: TEdit;
    LUSER: TLabel;
    EUSER: TEdit;
    LPWD: TLabel;
    EPWD: TEdit;
    LDNS: TLabel;
    EDNS: TEdit;
    CMSDriver: TCheckBox;
    CDirectOdbc: TCheckBox;
    BConnect: TButton;
    BDisconnect: TButton;
    LExample_oracle: TLabel;
    sqlcon_example_1_oracle_direct: TSQLConnection;
    sqlcon_example_2_oracle: TSQLConnection;
    LExample_microsoft: TLabel;
    sqlcon_example_1_ms_oracle_direct: TSQLConnection;
    sqlcon_example_2_ms_oracle: TSQLConnection;
    procedure BConnectClick(Sender: TObject);
    procedure BDisconnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses dbx_ora_connect;

{$R *.dfm}

procedure TForm1.BConnectClick(Sender: TObject);
begin

  if ETNS.Text = '' then
    raise Exception.Create('It is necessary to set TNS.'
      + #13#10'TNS must be indicated in a file like "C:\Oracle\9.2.0.6\network\ADMIN\tnsnames.ora"');


  dbx_ora_connect.OracleConnect(
    SQLConnection,
    {TNS=} ETNS.Text,
    {User=} EUSER.Text,
    {Password=} EPWD.Text,
    {MicrosoftDriver=} CMSDriver.Checked,
    {DirectOdbc=} CDirectOdbc.Checked,
    {LoginPrompt=} EUSER.Text <> '',
    {DNS=}EDNS.Text
  );
end;

procedure TForm1.BDisconnectClick(Sender: TObject);
begin
  SQLConnection.Connected := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  bIsOracle, bIsMicrosoft: Boolean;
begin
  bIsOracle := IsPresentedOracleDriver();
  CMSDriver.Checked := not bIsOracle;
  bIsMicrosoft := IsPresentedMicrosoftOracleDriver();
  CDirectOdbc.Checked := bIsOracle or bIsMicrosoft;
end;

end.
