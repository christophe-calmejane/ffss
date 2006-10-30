object Form2: TForm2
  Left = 287
  Top = 179
  BorderStyle = bsSingle
  ClientHeight = 256
  ClientWidth = 246
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 246
    Height = 184
    Align = alClient
    TabOrder = 0
    object ListView1: TListView
      Left = 1
      Top = 1
      Width = 244
      Height = 182
      Align = alClient
      Columns = <
        item
          Caption = 'IP'
          Width = 100
        end
        item
          Caption = 'Transferts'
          Width = 70
        end
        item
          Caption = 'Streaming'
          Width = 70
        end>
      HotTrack = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnClick = ListView1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 184
    Width = 246
    Height = 72
    Align = alBottom
    TabOrder = 1
    object Button1: TButton
      Left = 84
      Top = 40
      Width = 81
      Height = 25
      Caption = '&Fermer'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 8
      Width = 85
      Height = 25
      Caption = '&Ejecter'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 156
      Top = 8
      Width = 83
      Height = 25
      Caption = '&Tout le monde'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
end
