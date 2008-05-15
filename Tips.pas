unit Tips;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QButtons, QExtCtrls, IniFiles;

type
  TTipsForm = class(TForm)
    TipMemo: TMemo;
    Label1: TLabel;
    DisableTipsOnStartupCBox: TCheckBox;
    Bevel1: TBevel;
    CancelBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TipsForm: TTipsForm;

implementation

uses MainDM, GUIDM;

{$R *.xfm}

procedure TTipsForm.FormCreate(Sender: TObject);
begin
  DMMain.InitForm(self);
end;

procedure TTipsForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TTipsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DMGUI.ShowTipsOnStartup:=Not(DisableTipsOnStartupCBox.Checked);

  Action:=caFree;
end;

procedure TTipsForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

end.
