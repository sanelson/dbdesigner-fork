unit PrinterSettings;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QPrinters;

type
  TPrinterSettingsForm = class(TForm)
    PrintersCBox: TComboBox;
    Label1: TLabel;
    XSizeEd: TEdit;
    YSizeEd: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure PrintersCBoxCloseUp(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PrinterSettingsForm: TPrinterSettingsForm;

implementation

{$R *.xfm}

procedure TPrinterSettingsForm.FormCreate(Sender: TObject);
begin
  PrintersCBox.Items.Assign(Printer.Printers);
  PrintersCBox.ItemIndex:=0;

  PrintersCBoxCloseUp(self);
end;

procedure TPrinterSettingsForm.PrintersCBoxCloseUp(Sender: TObject);
//var GlobalMode: THandle;
begin
  //GlobalAlloc(GlobalMode);

  {Printer.SetPrinter(PChar(PrintersCBox.Items[PrintersCBox.ItemIndex]),
    PChar('WINSPOOL'), PChar(''), GlobalMode);}
end;

end.
