object Form1: TForm1
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'FFSS Server configuration'
  ClientHeight = 197
  ClientWidth = 347
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 24
    Top = 80
    Width = 92
    Height = 13
    Caption = 'Name of the master'
  end
  object Label3: TLabel
    Left = 24
    Top = 50
    Width = 106
    Height = 13
    Caption = 'Comment of the server'
  end
  object Label2: TLabel
    Left = 24
    Top = 20
    Width = 90
    Height = 13
    Caption = 'Name of the server'
  end
  object Edit2: TEdit
    Left = 156
    Top = 16
    Width = 153
    Height = 21
    Hint = 'Choose a name for your server'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object Edit3: TEdit
    Left = 156
    Top = 46
    Width = 153
    Height = 21
    Hint = 'Choose a comment for your server'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object Edit4: TEdit
    Left = 156
    Top = 76
    Width = 153
    Height = 21
    Hint = 'Select the name of your master'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object BitBtn1: TBitBtn
    Left = 140
    Top = 160
    Width = 75
    Height = 25
    Caption = '&OK'
    TabOrder = 3
    OnClick = BitBtn1Click
    Kind = bkOK
  end
  object CheckBox1: TCheckBox
    Left = 96
    Top = 120
    Width = 177
    Height = 17
    Caption = 'Import samba shares (win9x only)'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object RB1: TRegisteryBase
    Societe = 'SkyTecH Group'
    Version = '1.0'
    Left = 20
    Top = 160
  end
end
