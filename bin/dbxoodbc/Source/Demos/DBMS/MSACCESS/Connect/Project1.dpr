program Project1;

uses
  Forms,
  dbx_access_connect in 'dbx_access_connect.pas',
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
