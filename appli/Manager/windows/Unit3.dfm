object Form3: TForm3
  Left = 192
  Top = 107
  Width = 232
  Height = 253
  Caption = 'Utilisateurs'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 12
    Top = 16
    Width = 201
    Height = 161
    Columns = <
      item
        Caption = 'Login'
        Width = 100
      end
      item
        Caption = 'Acc'#232's complet'
        Width = 85
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListView1Change
  end
  object Button1: TButton
    Left = 28
    Top = 188
    Width = 75
    Height = 25
    Caption = '&Ajouter'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 124
    Top = 188
    Width = 75
    Height = 25
    Caption = '&Supprimer'
    Enabled = False
    TabOrder = 2
    OnClick = Button2Click
  end
end
