object Form5: TForm5
  Left = 344
  Top = 261
  BorderStyle = bsSingle
  Caption = 'Informations de connexion '#224' distance'
  ClientHeight = 149
  ClientWidth = 268
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 91
    Height = 13
    Caption = 'Nom de la machine'
  end
  object Label2: TLabel
    Left = 16
    Top = 49
    Width = 26
    Height = 13
    Caption = 'Login'
  end
  object Label3: TLabel
    Left = 16
    Top = 75
    Width = 64
    Height = 13
    Caption = 'Mot de passe'
  end
  object Edit1: TEdit
    Left = 124
    Top = 20
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 124
    Top = 46
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 124
    Top = 72
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object Button1: TButton
    Left = 97
    Top = 112
    Width = 75
    Height = 25
    Caption = '&Connexion'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
end
