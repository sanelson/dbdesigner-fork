object ExceptionDialog: TExceptionDialog
  Left = 363
  Top = 284
  ActiveControl = OkBtn
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'ExceptionDialog'
  ClientHeight = 341
  ClientWidth = 475
  Color = clBtnFace
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    475
    341)
  PixelsPerInch = 96
  TextHeight = 13
  object ShapeDetailsMemo: TShape
    Left = 1
    Top = 106
    Width = 472
    Height = 235
    Anchors = [akLeft, akTop, akRight, akBottom]
    Brush.Color = clBtnFace
    Pen.Color = clGray
    Pen.Style = psDot
  end
  object Bevel1: TBevel
    Left = 3
    Top = 104
    Width = 471
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    Visible = False
  end
  object ShapeTextLabel: TShape
    Left = 52
    Top = 3
    Width = 326
    Height = 101
    Anchors = [akLeft, akTop, akRight]
    Brush.Color = clBtnFace
    Pen.Color = clGray
    Pen.Style = psDot
  end
  object LErrorCountCaption: TLabel
    Left = 384
    Top = 87
    Width = 33
    Height = 13
    Caption = 'Errors:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    Visible = False
  end
  object LErrorCount: TLabel
    Left = 440
    Top = 87
    Width = 30
    Height = 13
    AutoSize = False
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object OkBtn: TButton
    Left = 383
    Top = 4
    Width = 90
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object DetailsMemo: TMemo
    Left = 4
    Top = 107
    Width = 467
    Height = 233
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    WantReturns = False
    WordWrap = False
  end
  object DetailsBtn: TButton
    Left = 383
    Top = 55
    Width = 90
    Height = 25
    Hint = 'Show or hide additional information|'
    Anchors = [akTop, akRight]
    Caption = '&Details'
    TabOrder = 2
    OnClick = DetailsBtnClick
  end
  object TextLabel: TMemo
    Left = 56
    Top = 7
    Width = 319
    Height = 96
    Hint = 'Use Ctrl+C to copy the report to the clipboard'
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    Ctl3D = True
    Lines.Strings = (
      'TextLabel')
    ParentColor = True
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 0
    WantReturns = False
  end
  object CopyButton: TButton
    Left = 383
    Top = 29
    Width = 90
    Height = 25
    Hint = 'Copy report to clipboard'
    Anchors = [akTop, akRight]
    Caption = '&Copy all'
    TabOrder = 3
    OnClick = CopyButtonClick
  end
end
