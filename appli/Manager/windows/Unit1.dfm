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
  PopupMenu = PopupMenu2
  Position = poScreenCenter
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
    TabIndex = 1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'G'#233'n'#233'ral'
      object Label3: TLabel
        Left = 16
        Top = 20
        Width = 80
        Height = 13
        Hint = 'Le nom de votre machine sur le voisinage r'#233'seau'
        Caption = 'Nom de machine'
        ParentShowHint = False
        ShowHint = True
      end
      object Label4: TLabel
        Left = 16
        Top = 56
        Width = 61
        Height = 13
        Hint = 'Le commentaire de votre machine sur le voisinage r'#233'seau'
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
          'Le d'#233'lai max d'#39'inactivit'#233' sur un partage avant de d'#233'connecter le' +
          ' client (en sec) (ou 0 pour infini)'
        Caption = 'D'#233'lai d'#39'idle'
        ParentShowHint = False
        ShowHint = True
      end
      object Label7: TLabel
        Left = 16
        Top = 172
        Width = 92
        Height = 13
        Hint = 
          'Le nombre max de connexions simultan'#233'es '#224' votre serveur (ou 0 po' +
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
          'Le nombre max de transferts simultan'#233's par connexion (ou 0 pour ' +
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
        Hint = 'Le nom de votre machine sur le voisinage r'#233'seau'
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
        Hint = 'Le commentaire de votre machine sur le voisinage r'#233'seau'
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
          'Le d'#233'lai max d'#39'inactivit'#233' sur un partage avant de d'#233'connecter le' +
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
          'Le nombre max de connexions simultan'#233'es '#224' votre serveur (ou 0 po' +
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
          'La compatibilit'#233' FTP permet de se connecter '#224' votre serveur avec' +
          ' un client FTP'
        Caption = ' Compatibilit'#233' FTP '
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        object Label8: TLabel
          Left = 12
          Top = 64
          Width = 92
          Height = 13
          Hint = 
            'Le nombre max de connexions simultan'#233'es '#224' votre serveur FTP (ou ' +
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
          Hint = 'Activer la compatibilit'#233' FTP'
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
            'Le nombre max de connexions simultan'#233'es '#224' votre serveur FTP (ou ' +
            '0 pour infini)'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = '10'
          OnChange = Edit5Change
        end
      end
      object Edit10: TEdit
        Left = 132
        Top = 204
        Width = 41
        Height = 21
        Hint = 
          'Le nombre max de transferts simultan'#233's par connexion (ou 0 pour ' +
          'infini)'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        Text = '2'
        OnChange = Edit5Change
      end
      object CheckBox4: TCheckBox
        Left = 200
        Top = 208
        Width = 113
        Height = 17
        Caption = 'Transferts multiples'
        Checked = True
        State = cbChecked
        TabOrder = 7
        OnClick = CheckBox4Click
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
          Left = 12
          Top = 204
          Width = 98
          Height = 13
          Hint = 
            'Le nombre max de connexions simultan'#233'es '#224' ce partage (0 pour inf' +
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
            'R'#232'gle les droits d'#39'acc'#232's au partage, et les utilisateurs privil'#233 +
            'gi'#233's'
          Caption = ' Acc'#232's '
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          object CheckBox2: TCheckBox
            Left = 16
            Top = 28
            Width = 97
            Height = 17
            Hint = 'Indique si on peut '#233'crire sur le partage'
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
              'En mode priv'#233', seuls les utilisateurs privil'#233'gi'#233's peuvent se con' +
              'necter'
            Caption = 'Priv'#233
            TabOrder = 1
            OnClick = CheckBox2Click
          end
          object Button4: TButton
            Left = 164
            Top = 48
            Width = 75
            Height = 25
            Hint = 'Editer les utilisateurs privil'#233'gi'#233's'
            Caption = 'Utilisateurs'
            TabOrder = 2
            OnClick = Button4Click
          end
        end
        object Edit9: TEdit
          Left = 120
          Top = 204
          Width = 40
          Height = 21
          Hint = 
            'Le nombre max de connexions simultan'#233'es '#224' ce partage (0 pour inf' +
            'ini)'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          Text = '0'
          OnChange = Edit1Change
        end
        object CheckBox5: TCheckBox
          Left = 176
          Top = 204
          Width = 113
          Height = 17
          Caption = 'No File Checksum'
          TabOrder = 4
          OnClick = CheckBox2Click
        end
      end
      object RadioButton1: TRadioButton
        Tag = 1
        Left = 20
        Top = 24
        Width = 113
        Height = 17
        Caption = 'N&on partag'#233
        TabOrder = 0
        OnClick = RadioButton1Click
      end
      object RadioButton2: TRadioButton
        Tag = 1
        Left = 20
        Top = 48
        Width = 121
        Height = 17
        Caption = '&Partag'#233' en tant que : '
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
          't quand m'#234'me regarder la liste des partages'
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
              Width = 75
            end
            item
              Caption = 'Chemin'
              Width = 90
            end
            item
              Caption = 'Actif'
              Width = 35
            end
            item
              Caption = 'Conns'
              Width = 42
            end
            item
              Caption = 'Xfer'
              Width = 35
            end>
          HotTrack = True
          ReadOnly = True
          RowSelect = True
          PopupMenu = PopupMenu1
          TabOrder = 0
          ViewStyle = vsReport
          OnDblClick = ListView1DblClick
        end
        object Button5: TButton
          Left = 112
          Top = 212
          Width = 75
          Height = 25
          Caption = '&Rafraichir'
          TabOrder = 1
          OnClick = Button5Click
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Plugins'
      ImageIndex = 3
      object ListView2: TListView
        Left = 12
        Top = 40
        Width = 305
        Height = 253
        Columns = <
          item
            Caption = 'Nom'
            Width = 120
          end
          item
            Caption = 'Auteur'
            Width = 130
          end
          item
            Caption = 'Version'
          end>
        HotTrack = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = ListView2Change
      end
      object Button6: TButton
        Left = 28
        Top = 312
        Width = 75
        Height = 25
        Caption = '&Charger'
        TabOrder = 1
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 128
        Top = 312
        Width = 75
        Height = 25
        Caption = '&Retirer'
        Enabled = False
        TabOrder = 2
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 228
        Top = 312
        Width = 75
        Height = 25
        Caption = '&Configurer'
        Enabled = False
        TabOrder = 3
        OnClick = Button8Click
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
      Caption = '&D'#233'sactiver'
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
  object ST1: TSystemTrayIcon
    Enabled = True
    ShowHint = False
    UserID = 0
    Visible = True
    AnimInterval = 200
    AnimPingPong = False
    Animate = False
    RestoreOnClick = True
    Left = 4
    Top = 408
  end
  object RB1: TRegisteryBase
    Societe = 'SkyTecH Group'
    Version = '1.0'
    Left = 92
    Top = 408
  end
  object OpenDialog1: TOpenDialog
    Filter = 'FFSS Server Plugins (*.dll)|*.dll'
    Title = 'Choix du plugin '#224' charger'
    Left = 56
    Top = 404
  end
  object PopupMenu2: TPopupMenu
    Left = 12
    Top = 420
    object Connexiondistance1: TMenuItem
      Caption = '&Connexion '#224' distance'
      OnClick = Connexiondistance1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Quitter1: TMenuItem
      Caption = '&Quitter'
      OnClick = Quitter1Click
    end
  end
end
