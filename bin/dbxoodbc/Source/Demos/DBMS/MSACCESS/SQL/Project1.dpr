program Project1;

  { Delphi 2006 }

uses
  Forms,
  // ExceptDlg,
  // MidasLib,
  Unit1 in 'Unit1.pas' {Form1},
  dbx_access_connect in 'dbx_access_connect.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
