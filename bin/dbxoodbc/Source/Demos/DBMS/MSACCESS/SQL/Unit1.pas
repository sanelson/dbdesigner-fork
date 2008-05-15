unit Unit1;

  { Delphi 2006 }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DBXpress, WideStrings, StdCtrls, DB, SqlExpr,
  {$IF CompilerVersion < 18.50}
  DbxOpenOdbc, // optional
  {$IF CompilerVersion = 18.00}
  DbxOpenOdbc3, // optional
  {$IFEND}
  {$IFEND}
  ExtCtrls, FMTBcd, DBClient, DBCtrls, Grids, DBGrids, Provider;

type
  TForm1 = class(TForm)
    SQLConnection: TSQLConnection;
    btn_connect: TButton;
    sh1: TShape;
    SQLQuery: TSQLQuery;
    btn_open_query: TButton;
    CDS: TClientDataSet;
    mem_sql_text: TMemo;
    btn_query_exec: TButton;
    btn_cds_open: TButton;
    DSP: TDataSetProvider;
    DataSource: TDataSource;
    mem_log: TMemo;
    grd1: TDBGrid;
    dbnav1: TDBNavigator;
    btn_cds_close: TButton;
    btn_open_close: TButton;
    btn_disconnect: TButton;
    btn_cds_apply: TButton;
    chk_unicode_dbx: TCheckBox;
    chk_ansi_string: TCheckBox;
    chk_unicode_odbc: TCheckBox;
    procedure btn_connectClick(Sender: TObject);
    procedure SQLConnectionAfterConnect(Sender: TObject);
    procedure SQLConnectionAfterDisconnect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_open_queryClick(Sender: TObject);
    procedure btn_query_execClick(Sender: TObject);
    procedure btn_cds_openClick(Sender: TObject);
    procedure btn_open_closeClick(Sender: TObject);
    procedure btn_cds_closeClick(Sender: TObject);
    procedure btn_disconnectClick(Sender: TObject);
    procedure btn_cds_applyClick(Sender: TObject);
  private
    { Private declarations }
    procedure CheckConnection();
  public
    { Public declarations }
    procedure print_dataset(D: TDataSet; ClearMemo: Boolean = True);
  end;

var
  Form1: TForm1;

implementation

uses dbx_access_connect;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SQLConnectionAfterDisconnect(SQLConnection);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
//  cds.Close;
//  SQLConnection.Connected := False; { !!!: else AV on close }
end;

procedure TForm1.btn_connectClick(Sender: TObject);
begin
{
procedure AccessConnect(SQLConnection: TSQLConnection;
  const mdb_file_name: string;
  const DNS_NAME: string = '';
  DirectOdbc: Boolean = True;
  LoginPrompt: Boolean = False;
  UserName: string = '';
  Password: string = '';
  const AdditionalOptions: string = '';
  bUnicodeOdbcApi: Boolean = False;
  bAnsiStringField: Boolean = True;
  bUnicodeDriver: Boolean = False
);
{}
  dbx_access_connect.AccessConnect(SQLConnection,
    // mdb_file_name:
      ExtractFilePath(ParamStr(0)) + 'dbdemos.mdb',
    // DNS_NAME
      '',
    // DirectOdbc
      True,
    // LoginPrompt
      False,
    // UserName
      '',
    // Password
      '',
    // AdditionalOptions
      '',
    // bUnicodeOdbcApi
      chk_unicode_odbc.Checked,
    // bAnsiStringField
      chk_ansi_string.Checked,
    // bUnicodeDriver
      chk_unicode_dbx.Checked
  );
end;

procedure TForm1.CheckConnection();
begin
  if not SQLConnection.Connected then
    //Abort;
    raise Exception.Create('Set connection with server MSAccess');
end;

procedure TForm1.btn_disconnectClick(Sender: TObject);
begin
  cds.Close;
  SQLConnection.Connected := False;
end;

procedure TForm1.SQLConnectionAfterConnect(Sender: TObject);
begin
  sh1.Brush.Color := clRed;

  chk_unicode_odbc.Enabled := False;
  chk_ansi_string.Enabled := False;
  chk_unicode_dbx.Enabled := False;

  btn_query_exec.Enabled := True;
  btn_open_query.Enabled := True;
  btn_open_close.Enabled := True;
  btn_cds_open.Enabled := True;
  btn_cds_close.Enabled := True;
  btn_cds_apply.Enabled := True;
end;

procedure TForm1.SQLConnectionAfterDisconnect(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    sh1.Brush.Color := clGray;

    chk_unicode_odbc.Enabled := True;
    chk_ansi_string.Enabled := True;
    chk_unicode_dbx.Enabled := True;

    btn_query_exec.Enabled := False;
    btn_open_query.Enabled := False;
    btn_open_close.Enabled := False;
    btn_cds_open.Enabled := False;
    btn_cds_close.Enabled := False;
    btn_cds_apply.Enabled := False;
  end;
end;

procedure TForm1.btn_query_execClick(Sender: TObject);
begin
  mem_log.Lines.Clear;
  //if not SQLConnection.Connected then
  //  Exit;
  CheckConnection();
  SQLQuery.Close;
  SQLQuery.SQL.Text := mem_sql_text.Lines.Text;
  SQLQuery.ExecSQL;
end;

procedure TForm1.btn_open_queryClick(Sender: TObject);
begin
  mem_log.Lines.Clear;
  //if not SQLConnection.Connected then
  //  Exit;
  CheckConnection();
  SQLQuery.Close;
  SQLQuery.SQL.Text := mem_sql_text.Lines.Text;
  SQLQuery.Open;
  print_dataset(SQLQuery);
end;

procedure TForm1.btn_open_closeClick(Sender: TObject);
begin
  SQLQuery.Close;
  mem_log.Lines.Clear;
end;

procedure TForm1.btn_cds_openClick(Sender: TObject);
begin
  mem_log.Lines.Clear;
  cds.Close;
  //if not SQLConnection.Connected then
  //  Exit;
  CheckConnection();
  SQLQuery.Close;
  SQLQuery.SQL.Text := mem_sql_text.Lines.Text;
  cds.Open;
end;

procedure TForm1.btn_cds_applyClick(Sender: TObject);
begin
  //if not SQLConnection.Connected then
  //  Exit;
  CheckConnection();
  if not cds.Active then
    //Exit;
    raise Exception.Create('CDS is not open');
  cds.ApplyUpdates(0);
end;

procedure TForm1.btn_cds_closeClick(Sender: TObject);
begin
  mem_log.Lines.Clear;
  cds.Close;
  SQLQuery.Close;
end;

procedure TForm1.print_dataset(D: TDataSet; ClearMemo: Boolean = True);
const
  iColMaxCount = 17;
  iRowMaxCount = 7;
var
  iCol, iRow, i: Integer;
  s: string;
begin
  if ClearMemo then
    mem_log.Lines.Clear;

  iCol := D.FieldCount-1;
  if iCol > iColMaxCount-1 then
    iCol := iColMaxCount-1;
  iRow := 0;

  while (not D.Eof) and (iRow < iRowMaxCount) do with mem_log.Lines do
  begin
    Inc(iRow);

    Add('-----------------------------');
    Add(Format('  Row num %d: values', [iRow]));
    Add('-----------------------------');
    for i := 0 to iCol do
    begin
      with D.Fields[i] do
        S := '    ' + FieldName + ' = ' + DisplayText;
      Add(S);
    end;

    D.Next;
  end;
end;

end.
