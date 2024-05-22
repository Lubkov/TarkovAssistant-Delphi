object edLocalMap: TedLocalMap
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'edLocalMap'
  ClientHeight = 309
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 16
  object laMapName: TLabel
    Left = 24
    Top = 24
    Width = 33
    Height = 16
    Caption = 'Name'
  end
  object edMapName: TEdit
    Left = 24
    Top = 43
    Width = 153
    Height = 24
    TabOrder = 0
    Text = 'edMapName'
  end
  object edLeft: TEdit
    Left = 24
    Top = 96
    Width = 121
    Height = 24
    TabOrder = 1
    Text = 'edLeft'
  end
  object edTop: TEdit
    Left = 184
    Top = 96
    Width = 121
    Height = 24
    TabOrder = 2
    Text = 'edTop'
  end
  object edRight: TEdit
    Left = 24
    Top = 144
    Width = 121
    Height = 24
    TabOrder = 3
    Text = 'edRight'
  end
  object edBottom: TEdit
    Left = 184
    Top = 144
    Width = 121
    Height = 24
    TabOrder = 4
    Text = 'edBottom'
  end
  object Panel_Button: TPanel
    Left = 0
    Top = 263
    Width = 645
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    ExplicitLeft = -109
    ExplicitTop = 253
    ExplicitWidth = 744
    object btnCancel: TButton
      Left = 300
      Top = 6
      Width = 110
      Height = 32
      Action = acCancel
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageMargins.Left = 5
      ModalResult = 2
      ParentFont = False
      TabOrder = 0
    end
    object btnNext: TButton
      Left = 184
      Top = 6
      Width = 110
      Height = 32
      Action = acSuccess
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageMargins.Left = 5
      ModalResult = 1
      ParentFont = False
      TabOrder = 1
    end
  end
  object ActionList1: TActionList
    Left = 412
    Top = 97
    object acSuccess: TAction
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      ImageIndex = 0
      OnExecute = acSuccessExecute
    end
    object acCancel: TAction
      Caption = #1054#1090#1084#1077#1085#1072
      ImageIndex = 1
      OnExecute = acCancelExecute
    end
  end
end
