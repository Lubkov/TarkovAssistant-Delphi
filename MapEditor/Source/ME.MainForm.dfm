object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 573
  ClientWidth = 898
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 489
    Width = 898
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button3: TButton
      AlignWithMargins = True
      Left = 10
      Top = 5
      Width = 90
      Height = 32
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Get Point'
      TabOrder = 0
      OnClick = Button3Click
    end
    object Button4: TButton
      AlignWithMargins = True
      Left = 110
      Top = 5
      Width = 90
      Height = 32
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Insert Point'
      TabOrder = 1
      OnClick = Button4Click
    end
    object Button5: TButton
      AlignWithMargins = True
      Left = 210
      Top = 5
      Width = 90
      Height = 32
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Update Point'
      TabOrder = 2
      OnClick = Button5Click
    end
    object Button8: TButton
      AlignWithMargins = True
      Left = 310
      Top = 5
      Width = 90
      Height = 32
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Delete Point'
      TabOrder = 3
      OnClick = Button8Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 393
    Width = 898
    Height = 96
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitTop = 400
    ExplicitHeight = 89
  end
  object Panel2: TPanel
    Left = 0
    Top = 531
    Width = 898
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button6: TButton
      AlignWithMargins = True
      Left = 10
      Top = 5
      Width = 90
      Height = 32
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Get Map'
      TabOrder = 0
      OnClick = Button6Click
    end
    object Button7: TButton
      AlignWithMargins = True
      Left = 110
      Top = 5
      Width = 90
      Height = 32
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Insert Map'
      TabOrder = 1
      OnClick = Button7Click
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 210
      Top = 5
      Width = 90
      Height = 32
      Margins.Left = 10
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Update Map'
      TabOrder = 2
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 898
    Height = 169
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
  end
  object Panel4: TPanel
    Left = 0
    Top = 169
    Width = 898
    Height = 224
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
  end
  object UniConnection1: TUniConnection
    ProviderName = 'SQLite'
    Database = 'e:\Projects\Delphi\EscapeFromTarkov\Bin\data.db'
    SpecificOptions.Strings = (
      'SQLite.Direct=True')
    LoginPrompt = False
    Left = 120
    Top = 112
  end
  object UniQuery1: TUniQuery
    Connection = UniConnection1
    Left = 208
    Top = 112
  end
  object SQLiteUniProvider1: TSQLiteUniProvider
    Left = 312
    Top = 112
  end
end
