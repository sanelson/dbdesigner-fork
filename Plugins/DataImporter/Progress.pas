unit Progress;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QComCtrls, QExtCtrls;

type
  TProgressForm = class(TForm)
    MainPanel: TPanel;
    Label1: TLabel;
    FileLbl: TLabel;
    Label2: TLabel;
    CurrentLinesLbl: TLabel;
    Label3: TLabel;
    TotalLinesLbl: TLabel;
    Label5: TLabel;
    ErrorsLbl: TLabel;
    Label4: TLabel;
    CurrentFileLbl: TLabel;
    procedure FormCreate(Sender: TObject);

    procedure SetFile(val: string);
    procedure SetCurrentLines(val: integer);
    procedure SetTotalLines(val: integer);
    procedure SetErrors(val: integer);
    procedure SetCurrentFile(val, total: integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ProgressForm: TProgressForm;

implementation

{$R *.xfm}

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  FileLbl.Caption:='';
end;

procedure TProgressForm.SetFile(val: string);
begin
  if(Length(val)>35)then
    val:='...'+Copy(val, Length(val)-35, 36);

  FileLbl.Caption:=val;

  Refresh;
end;


procedure TProgressForm.SetCurrentLines(val: integer);
begin
  CurrentLinesLbl.Caption:=FormatFloat('###,###,###', val);

  Refresh;
end;

procedure TProgressForm.SetTotalLines(val: integer);
begin
  TotalLinesLbl.Caption:=FormatFloat('###,###,###', val);

  Refresh;
end;

procedure TProgressForm.SetErrors(val: integer);
begin
  ErrorsLbl.Caption:=FormatFloat('###,###,###', val);

  Refresh;
end;

procedure TProgressForm.SetCurrentFile(val, total: integer);
begin
  CurrentFileLbl.Caption:=IntToStr(val)+' / '+IntToStr(total);

  Refresh;
end;


end.
