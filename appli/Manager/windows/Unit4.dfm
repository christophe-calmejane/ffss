object Form4: TForm4
  Left = 282
  Top = 408
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Ajout d'#39'un utilisateur'
  ClientHeight = 154
  ClientWidth = 228
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 20
    Width = 26
    Height = 13
    Caption = 'Login'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 64
    Height = 13
    Caption = 'Mot de passe'
  end
  object Edit1: TEdit
    Left = 84
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 84
    Top = 44
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 84
    Top = 80
    Width = 97
    Height = 17
    Caption = 'Acc'#232's complet'
    TabOrder = 2
  end
  object Button1: TButton
    Left = 76
    Top = 116
    Width = 75
    Height = 25
    Caption = '&Ajouter'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
end
