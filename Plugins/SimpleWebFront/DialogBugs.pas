unit DialogBugs;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QExtCtrls;

type
  TDialogBugsForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BugBg: TImage;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BugBgClick(Sender: TObject);
    procedure Label5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DialogBugsForm: TDialogBugsForm;

implementation

uses
 Main;
{$R *.xfm}

procedure TDialogBugsForm.FormCreate(Sender: TObject);
begin
  {$IFDEF LINUX}
  Font.Name:='Helvetica';
  Font.Size:=10;
  {$ENDIF}

  BugBg.Picture.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+ DataDir + 'bugreport.png');

  Top:=(Screen.Height-Height) div 2;
  //2 Monitors
  if(Screen.Width=(Screen.Height/0.75)*2)or
    (Screen.Width=(Screen.Height*1.25)*2)then
    Left:=((Screen.Width div 2)-Width) div 2
  else
    Left:=(Screen.Width-Width) div 2;
  //2 Monitors, different resolution 1280+1152
  if(Screen.Width=1280+1152)then
    Left:=(1280-Width) div 2;

end;

procedure TDialogBugsForm.BugBgClick(Sender: TObject);
begin
  close;
end;

procedure TDialogBugsForm.Label5Click(Sender: TObject);
begin
  close;
end;

end.
