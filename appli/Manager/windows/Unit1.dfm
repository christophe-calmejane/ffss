object Form1: TForm1
  Left = 325
  Top = 109
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Gestionnaire de partages de FFSS'
  ClientHeight = 453
  ClientWidth = 352
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 12
    Width = 333
    Height = 385
    ActivePage = TabSheet2
    HotTrack = True
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Général'
      object Label3: TLabel
        Left = 16
        Top = 20
        Width = 80
        Height = 13
        Hint = 'Le nom de votre machine sur le voisinage réseau'
        Caption = 'Nom de machine'
        ParentShowHint = False
        ShowHint = True
      end
      object Label4: TLabel
        Left = 16
        Top = 56
        Width = 61
        Height = 13
        Hint = 'Le commentaire de votre machine sur le voisinage réseau'
        Caption = 'Commentaire'
        ParentShowHint = False
        ShowHint = True
      end
      object Label5: TLabel
        Left = 16
        Top = 96
        Width = 71
        Height = 13
        Hint = 'Le nom du master de votre domaine'
        Caption = 'Nom du master'
        ParentShowHint = False
        ShowHint = True
      end
      object Label6: TLabel
        Left = 16
        Top = 132
        Width = 51
        Height = 13
        Hint =
          'Le délai max d'#39'inactivité sur un partage avant de déconnecter le' +
          ' client (en sec) (ou 0 pour infini)'
        Caption = 'Délai d'#39'idle'
        ParentShowHint = False
        ShowHint = True
      end
      object Label7: TLabel
        Left = 16
        Top = 172
        Width = 92
        Height = 13
        Hint =
          'Le nombre max de connexions simultanées à votre serveur (ou 0 po' +
          'ur infini)'
        Caption = 'Max de connexions'
        ParentShowHint = False
        ShowHint = True
      end
      object Label10: TLabel
        Left = 16
        Top = 208
        Width = 111
        Height = 13
        Hint =
          'Le nombre max de transferts simultanés par connexion (ou 0 pour ' +
          'infini)'
        Caption = 'Max transferts par conn'
        ParentShowHint = False
        ShowHint = True
      end
      object Edit3: TEdit
        Left = 132
        Top = 16
        Width = 121
        Height = 21
        Hint = 'Le nom de votre machine sur le voisinage réseau'
        MaxLength = 15
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnChange = Edit3Change
      end
      object Edit4: TEdit
        Left = 132
        Top = 52
        Width = 181
        Height = 21
        Hint = 'Le commentaire de votre machine sur le voisinage réseau'
        MaxLength = 50
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnChange = Edit3Change
      end
      object Edit5: TEdit
        Left = 132
        Top = 92
        Width = 121
        Height = 21
        Hint = 'Le nom du master de votre domaine'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnChange = Edit5Change
      end
      object Edit6: TEdit
        Left = 132
        Top = 128
        Width = 40
        Height = 21
        Hint =
          'Le délai max d'#39'inactivité sur un partage avant de déconnecter le' +
          ' client (en sec) (ou 0 pour infini)'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        Text = '0'
        OnChange = Edit5Change
      end
      object Edit7: TEdit
        Left = 132
        Top = 168
        Width = 40
        Height = 21
        Hint =
          'Le nombre max de connexions simultanées à votre serveur (ou 0 po' +
          'ur infini)'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        Text = '10'
        OnChange = Edit5Change
      end
      object GroupBox2: TGroupBox
        Left = 20
        Top = 236
        Width = 185
        Height = 105
        Hint =
          'La compatibilité FTP permet de se connecter à votre serveur avec' +
          ' un client FTP'
        Caption = ' Compatibilité FTP '
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        object Label8: TLabel
          Left = 12
          Top = 64
          Width = 92
          Height = 13
          Hint =
            'Le nombre max de connexions simultanées à votre serveur FTP (ou ' +
            '0 pour infini)'
          Caption = 'Max de connexions'
          ParentShowHint = False
          ShowHint = True
        end
        object CheckBox1: TCheckBox
          Left = 12
          Top = 28
          Width = 97
          Height = 17
          Hint = 'Activer la compatibilité FTP'
          Caption = 'Active'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = CheckBox1Click
        end
        object Edit8: TEdit
          Left = 120
          Top = 60
          Width = 40
          Height = 21
          Hint =
            'Le nombre max de connexions simultanées à votre serveur FTP (ou ' +
            '0 pour infini)'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = '0'
          OnChange = Edit5Change
        end
      end
      object Edit10: TEdit
        Left = 132
        Top = 204
        Width = 41
        Height = 21
        Hint =
          'Le nombre max de transferts simultanés par connexion (ou 0 pour ' +
          'infini)'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        Text = '2'
        OnChange = Edit5Change
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Partage'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 12
        Top = 56
        Width = 297
        Height = 245
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 2
        object Label1: TLabel
          Left = 16
          Top = 24
          Width = 82
          Height = 13
          Hint = 'Le nom du partage'
          Caption = 'Nom de partage :'
          ParentShowHint = False
          ShowHint = True
        end
        object Label2: TLabel
          Left = 16
          Top = 56
          Width = 67
          Height = 13
          Hint = 'Le commentaire du partage'
          Caption = 'Commentaire :'
          ParentShowHint = False
          ShowHint = True
        end
        object Label9: TLabel
          Left = 20
          Top = 204
          Width = 98
          Height = 13
          Hint =
            'Le nombre max de connexions simultanées à ce partage (0 pour inf' +
            'ini)'
          Caption = 'Max de connexions :'
          ParentShowHint = False
          ShowHint = True
        end
        object Edit1: TEdit
          Left = 112
          Top = 24
          Width = 129
          Height = 21
          Hint = 'Le nom du partage'
          MaxLength = 20
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnChange = Edit1Change
        end
        object Edit2: TEdit
          Left = 112
          Top = 56
          Width = 169
          Height = 21
          Hint = 'Le commentaire du partage'
          MaxLength = 50
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnChange = Edit1Change
        end
        object GroupBox1: TGroupBox
          Left = 16
          Top = 96
          Width = 265
          Height = 93
          Hint =
            'Règle les droits d'#39'accès au partage, et les utilisateurs privilé' +
            'giés'
          Caption = ' Accès '
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          object CheckBox2: TCheckBox
            Left = 16
            Top = 28
            Width = 97
            Height = 17
            Hint = 'Indique si on peut écrire sur le partage'
            Caption = 'Lecture seule'
            Checked = True
            Color = clBtnFace
            ParentColor = False
            State = cbChecked
            TabOrder = 0
            OnClick = CheckBox2Click
          end
          object CheckBox3: TCheckBox
            Left = 16
            Top = 56
            Width = 97
            Height = 17
            Hint =
              'En mode privé, seuls les utilisateurs privilégiés peuvent se con' +
              'necter'
            Caption = 'Privé'
            TabOrder = 1
            OnClick = CheckBox2Click
          end
          object Button4: TButton
            Left = 164
            Top = 48
            Width = 75
            Height = 25
            Hint = 'Editer les utilisateurs privilégiés'
            Caption = 'Utilisateurs'
            TabOrder = 2
          end
        end
        object Edit9: TEdit
          Left = 128
          Top = 204
          Width = 40
          Height = 21
          Hint =
            'Le nombre max de connexions simultanées à ce partage (0 pour inf' +
            'ini)'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          Text = '0'
          OnChange = Edit1Change
        end
      end
      object RadioButton1: TRadioButton
        Tag = 1
        Left = 20
        Top = 24
        Width = 113
        Height = 17
        Caption = 'N&on partagé'
        TabOrder = 0
        OnClick = RadioButton1Click
      end
      object RadioButton2: TRadioButton
        Tag = 1
        Left = 20
        Top = 48
        Width = 121
        Height = 17
        Caption = '&Partagé en tant que : '
        TabOrder = 1
        OnClick = RadioButton2Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Etat'
      ImageIndex = 2
      object RadioGroup1: TRadioGroup
        Left = 12
        Top = 20
        Width = 297
        Height = 73
        Hint =
          'En mode silencieux, aucune connexion n'#39'est possible, mais on peu' +
          't quand même regarder la liste des partages'
        Caption = ' Etat du serveur '
        ItemIndex = 0
        Items.Strings = (
          'Actif'
          'Silencieux')
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = RadioGroup1Click
      end
      object GroupBox3: TGroupBox
        Left = 12
        Top = 108
        Width = 297
        Height = 245
        Caption = ' Liste des partages '
        TabOrder = 1
        object ListView1: TListView
          Left = 8
          Top = 24
          Width = 281
          Height = 181
          Columns = <
            item
              Caption = 'Nom'
              Width = 80
            end
            item
              Caption = 'Chemin'
              Width = 110
            end
            item
              Caption = 'Actif'
              Width = 40
            end
            item
              Caption = 'Conns'
              Width = 47
            end>
          HotTrack = True
          ReadOnly = True
          RowSelect = True
          PopupMenu = PopupMenu1
          TabOrder = 0
          ViewStyle = vsReport
        end
        object Button5: TButton
          Left = 112
          Top = 212
          Width = 75
          Height = 25
          Caption = '&Rescan'
          TabOrder = 1
          OnClick = Button5Click
        end
      end
    end
  end
  object Button1: TButton
    Left = 96
    Top = 412
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 180
    Top = 412
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 264
    Top = 412
    Width = 75
    Height = 25
    Caption = '&Appliquer'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Button3Click
  end
  object CS: TClientSocket
    Active = False
    ClientType = ctBlocking
    Host = 'localhost'
    Port = 0
    OnDisconnect = CSDisconnect
    OnError = CSError
    Left = 32
    Top = 412
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 68
    Top = 412
    object Supprimer1: TMenuItem
      Caption = '&Supprimer'
      OnClick = Supprimer1Click
    end
    object Rescan1: TMenuItem
      Caption = '&Rescaner'
      OnClick = Rescan1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Actif1: TMenuItem
      Caption = '&Activer'
      OnClick = Actif1Click
    end
    object Dsactiver1: TMenuItem
      Caption = '&Désactiver'
      OnClick = Dsactiver1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Ejecter1: TMenuItem
      Caption = '&Ejecter tout le monde'
      OnClick = Ejecter1Click
    end
  end
end
