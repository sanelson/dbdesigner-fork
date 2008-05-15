object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'dbxoodbx:: demo dbx3 access to MSAccess'
  ClientHeight = 446
  ClientWidth = 632
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    632
    446)
  PixelsPerInch = 96
  TextHeight = 13
  object sh1: TShape
    Left = 8
    Top = 7
    Width = 17
    Height = 17
    Brush.Color = clGray
  end
  object btn_connect: TButton
    Left = 8
    Top = 77
    Width = 75
    Height = 25
    Caption = 'ReConnect'
    TabOrder = 4
    OnClick = btn_connectClick
  end
  object btn_open_query: TButton
    Left = 89
    Top = 108
    Width = 75
    Height = 25
    Caption = 'Open Query'
    TabOrder = 7
    OnClick = btn_open_queryClick
  end
  object mem_sql_text: TMemo
    Left = 251
    Top = 1
    Width = 377
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'select * from customer')
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object btn_query_exec: TButton
    Left = 170
    Top = 77
    Width = 75
    Height = 25
    Caption = 'Exec Query'
    TabOrder = 6
    OnClick = btn_query_execClick
  end
  object btn_cds_open: TButton
    Left = 89
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Open CDS'
    TabOrder = 10
    OnClick = btn_cds_openClick
  end
  object mem_log: TMemo
    Left = 170
    Top = 108
    Width = 458
    Height = 173
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 9
  end
  object grd1: TDBGrid
    Left = 170
    Top = 284
    Width = 458
    Height = 132
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource
    TabOrder = 13
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object dbnav1: TDBNavigator
    Left = 174
    Top = 419
    Width = 240
    Height = 25
    DataSource = DataSource
    Anchors = [akLeft, akBottom]
    TabOrder = 14
    ExplicitTop = 464
  end
  object btn_cds_close: TButton
    Left = 89
    Top = 335
    Width = 75
    Height = 25
    Caption = 'Close CDS'
    TabOrder = 11
    OnClick = btn_cds_closeClick
  end
  object btn_open_close: TButton
    Left = 89
    Top = 190
    Width = 75
    Height = 25
    Caption = 'Close Query'
    TabOrder = 8
    OnClick = btn_open_closeClick
  end
  object btn_disconnect: TButton
    Left = 89
    Top = 77
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 5
    OnClick = btn_disconnectClick
  end
  object btn_cds_apply: TButton
    Left = 89
    Top = 366
    Width = 75
    Height = 25
    Caption = 'Apply CDS'
    TabOrder = 12
    OnClick = btn_cds_applyClick
  end
  object chk_unicode_dbx: TCheckBox
    Left = 39
    Top = 7
    Width = 134
    Height = 17
    Caption = 'Unicode DBX (DBX3)'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object chk_ansi_string: TCheckBox
    Left = 39
    Top = 51
    Width = 134
    Height = 17
    Caption = 'Ansi String'
    TabOrder = 2
  end
  object chk_unicode_odbc: TCheckBox
    Left = 39
    Top = 28
    Width = 134
    Height = 17
    Caption = 'Unicode ODBC'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object SQLConnection: TSQLConnection
    AfterConnect = SQLConnectionAfterConnect
    AfterDisconnect = SQLConnectionAfterDisconnect
    Left = 148
    Top = 28
  end
  object SQLQuery: TSQLQuery
    MaxBlobSize = -1
    Params = <>
    SQLConnection = SQLConnection
    Left = 30
    Top = 114
  end
  object CDS: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'DSP'
    Left = 30
    Top = 306
  end
  object DSP: TDataSetProvider
    DataSet = SQLQuery
    Left = 32
    Top = 204
  end
  object DataSource: TDataSource
    DataSet = CDS
    Left = 32
    Top = 372
  end
end
