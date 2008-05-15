object Form1: TForm1
  Left = 415
  Top = 322
  Width = 599
  Height = 432
  Caption = 'Demo: DbxOOdbc Oracle Connect'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TTNS: TLabel
    Left = 20
    Top = 8
    Width = 50
    Height = 13
    Caption = 'TNS NAME'
  end
  object LUSER: TLabel
    Left = 20
    Top = 40
    Width = 26
    Height = 13
    Caption = 'USER'
  end
  object LPWD: TLabel
    Left = 20
    Top = 68
    Width = 51
    Height = 13
    Caption = 'PASWORD'
  end
  object LExample_oracle: TLabel
    Left = 8
    Top = 200
    Width = 209
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'ORACLE ODBC DRIVER'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Visible = False
  end
  object LExample_microsoft: TLabel
    Left = 324
    Top = 200
    Width = 265
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'MICROSOFT ODBC DRIVER FOR ORACLE'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Visible = False
  end
  object LDNS: TLabel
    Left = 20
    Top = 93
    Width = 20
    Height = 13
    Caption = 'DNS'
  end
  object ETNS: TEdit
    Left = 90
    Top = 8
    Width = 120
    Height = 21
    TabOrder = 0
    Text = 'ASM'
  end
  object EUSER: TEdit
    Left = 90
    Top = 36
    Width = 120
    Height = 21
    TabOrder = 1
    Text = 'VADIM'
  end
  object EPWD: TEdit
    Left = 90
    Top = 60
    Width = 120
    Height = 21
    TabOrder = 2
    Text = 'lopus'
  end
  object BConnect: TButton
    Left = 28
    Top = 124
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 6
    OnClick = BConnectClick
  end
  object BDisconnect: TButton
    Left = 116
    Top = 124
    Width = 85
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 7
    OnClick = BDisconnectClick
  end
  object CMSDriver: TCheckBox
    Left = 232
    Top = 8
    Width = 109
    Height = 17
    Caption = 'Microsoft Driver'
    TabOrder = 4
  end
  object EDNS: TEdit
    Left = 90
    Top = 86
    Width = 120
    Height = 21
    TabOrder = 3
  end
  object CDirectOdbc: TCheckBox
    Left = 232
    Top = 32
    Width = 109
    Height = 17
    Caption = 'Direct ODBC'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object SQLConnection: TSQLConnection
    Left = 356
    Top = 20
  end
  object sqlcon_example_1_ms_oracle_direct: TSQLConnection
    ConnectionName = 'Dbx:MS:Oracle'
    DriverName = 'DbxMSOracle'
    GetDriverFunc = 'getSQLDriverODBC'
    LibraryName = 'dbxoodbc.dll'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=DbxMSOracle'
      'Database=?'
      'User_Name='
      'Password='
      'RowsetSize=20'
      'BlobSize=-1'
      'Trim Char=True'
      
        'Custom String=coConnectionString=UID=vadim;PWD=lopus;SERVER=ASM;' +
        'coLockMode=-1;coCatPrefix=UID')
    VendorLib = 'MSORCL32.DLL'
    Left = 440
    Top = 248
  end
  object sqlcon_example_2_ms_oracle: TSQLConnection
    ConnectionName = 'Odbc'
    DriverName = 'DbxMSOracle'
    GetDriverFunc = 'getSQLDriverODBC'
    LibraryName = 'dbxoodbc.dll'
    LoginPrompt = False
    Params.Strings = (
      'DataBase=?'
      'Trim Char=True'
      
        'Custom String=coConnectionString=DRIVER={Microsoft ODBC for Orac' +
        'le};UID=vadim;PWD=lopus;SERVER=ASM;coLockMode=-1;coCatPrefix=UID')
    VendorLib = 'odbc32.dll'
    Left = 440
    Top = 316
  end
  object sqlcon_example_1_oracle_direct: TSQLConnection
    ConnectionName = 'Dbx:Oracle'
    DriverName = 'DbxOracle'
    GetDriverFunc = 'getSQLDriverODBC'
    LibraryName = 'dbxoodbc.dll'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=DbxOracle'
      'Database=?'
      'User_Name='
      'Password='
      'RowsetSize=20'
      'BlobSize=-1'
      'Trim Char=True'
      
        'Custom String=coConnectionString=DBQ=ASM;UID=vadim;PWD=lopus;DBA' +
        '=W;APA=T;FEN=T;QTO=T;FRC=10;FDL=10;LOB=T;RST=T;FRL=F;MTS=F;CSR=F' +
        ';PFC=10;TLO=0;coNetPacketSize=8192;coLockMode=-1;coBlobChunkSize' +
        '=40960;coSchemFlt=0;coCatPrefix=UID')
    VendorLib = 'SQORA32.DLL'
    Left = 88
    Top = 240
  end
  object sqlcon_example_2_oracle: TSQLConnection
    ConnectionName = 'Dbx:Oracle'
    DriverName = 'DbxOracle'
    GetDriverFunc = 'getSQLDriverODBC'
    LibraryName = 'dbxoodbc.dll'
    LoginPrompt = False
    Params.Strings = (
      'Database=?'
      'Trim Char=True'
      
        'Custom String=coConnectionString=DRIVER={Oracle ODBC Driver};DBQ' +
        '=ASM;UID=vadim;PWD=lopus;DBA=W;APA=T;FEN=T;QTO=T;FRC=10;FDL=10;L' +
        'OB=T;RST=T;FRL=F;MTS=F;CSR=F;PFC=10;TLO=0;coNetPacketSize=8192;c' +
        'oLockMode=-1;coBlobChunkSize=40960;coSchemFlt=0;coCatPrefix=UID')
    VendorLib = 'odbc32.dll'
    Left = 92
    Top = 304
  end
end
