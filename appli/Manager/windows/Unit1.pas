unit Unit1;

{
  TO DO :
   * Quand on ajoute un master, envoye un message UDP pour voir si le master est la
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ScktComp, Menus, SystemTray, RegisteryUnit, Winsock;

Const
  STRING_YES = 'Oui';
  STRING_NO  = 'Non';
  SHARE_DELETE = 1;
  SHARE_ADD    = 2;
  SHARE_UPDT   = 3;
  FS_CONF_PORT = 10004;
  FS_OPCODE_ADDSHARE      =  1;
  FS_OPCODE_DELSHARE      =  2;
  FS_OPCODE_GETGLOBAL     =  3;
  FS_OPCODE_GETSHARE      =  4;
  FS_OPCODE_UPDTSHARE     =  5;
  FS_OPCODE_UPDTGLOBAL    =  6;
  FS_OPCODE_SETSTATE      =  7;
  FS_OPCODE_GETSTATE      =  8;
  FS_OPCODE_GETSHRLIST    =  9;
  FS_OPCODE_RESCAN        = 10;
  FS_OPCODE_SETSHARESTATE = 11;
  FS_OPCODE_EJECT         = 12;
  FS_OPCODE_GETSHRCONNS   = 13;
  FS_OPCODE_EJECTIP       = 14;
  FS_OPCODE_ACK           = 20;
  FS_OPCODE_NACK          = 21;
  FFSS_STATE_ON    = 1;
  FFSS_STATE_OFF   = 2;
  FFSS_STATE_QUIET = 4;

type
  TGlobal = record
    Name : String;
    Comment : String;
    Master : String;
    Idle : Integer;
    MaxConn : Integer;
    MaxXFerPerConn : Integer;
    FTP : Boolean;
    FTP_MaxConn : Integer;
  end;
  PGlobal = ^TGlobal;

  TShare = record
    Name : String;
    Comment : String;
    Writeable : Boolean;
    Privat : Boolean;
    MaxConn : Integer;
  end;
  PShare = ^TShare;

  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    Edit6: TEdit;
    Label7: TLabel;
    Edit7: TEdit;
    GroupBox2: TGroupBox;
    CheckBox1: TCheckBox;
    Label8: TLabel;
    Edit8: TEdit;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Button4: TButton;
    Label9: TLabel;
    Edit9: TEdit;
    CS: TClientSocket;
    TabSheet3: TTabSheet;
    RadioGroup1: TRadioGroup;
    GroupBox3: TGroupBox;
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    Supprimer1: TMenuItem;
    Rescan1: TMenuItem;
    N1: TMenuItem;
    Actif1: TMenuItem;
    Dsactiver1: TMenuItem;
    N2: TMenuItem;
    Ejecter1: TMenuItem;
    Button5: TButton;
    Label10: TLabel;
    Edit10: TEdit;
    ST1: TSystemTrayIcon;
    RB1: TRegisteryBase;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CSDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure CSError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure Edit5Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Supprimer1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Rescan1Click(Sender: TObject);
    procedure Actif1Click(Sender: TObject);
    procedure Dsactiver1Click(Sender: TObject);
    procedure Ejecter1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetSharePath : String;
    function GetGoodSharePath : String;
    function IntLength(V : Integer) : Integer;
    function InitConnection : Boolean;
    function RequestGlobalInfo : PGlobal;
    function RequestShareInfo(Path : String) : PShare;
    procedure RequestStateInfo;
    procedure RequestSharesList;
    procedure RequestEject(ShareName : String);
    Procedure RequestConns(Path : String);
    procedure RequestEjectIP(ShareName,IP : String);
    function SetShareInfo(SharePath : String;Etat : Integer) : Boolean;
    procedure SetGlobalInfo;
    procedure SetStateInfo;
    procedure SetShareState(ShareName : String;State : Boolean);
    procedure SendRescanQuery(ShareName : String);
    function CheckGlobal : Boolean;
    function CheckShare : Boolean;
  end;

var
  Form1: TForm1;
  Global : PGlobal;
  Share : PShare;
  GblChanged : Boolean;
  ShrChanged : Boolean;
  StChanged : Boolean;
  ShareState : Integer;
  ChkError : Boolean;
  SuggestedName : String;
  Reboot : Boolean;
  OnFolder : Boolean;
  ConnInit : Boolean;
  Limited  : Boolean;

implementation

uses Unit2;

{$R *.DFM}

function TForm1.GetSharePath : String;
Begin
  Result:=ExtractFileName(ParamStr(1));
  If Result = '' Then
  Begin
    Result:=ParamStr(1);
    Delete(Result,2,2);
  End;
End;

function TForm1.GetGoodSharePath : String;
Begin
  Result:=ParamStr(1);
  If Copy(Result,2,5) = ':\' Then
    Delete(Result,3,1);
End;

function TForm1.IntLength(V : Integer) : Integer;
begin
  If V < 10 Then Result:=1
  Else If V < 100 Then Result:=2
  Else If V < 1000 Then Result:=3
  Else If V < 10000 Then Result:=4
  Else Result:=5;
end;

function TForm1.InitConnection : Boolean;
Begin
  try
    CS.Port:=FS_CONF_PORT;
    CS.Open;
    Result:=True;
  except
    Result:=False;
    Exit;
  End;
End;

procedure TForm1.FormCreate(Sender: TObject);
Var buf : Array[0..512] Of Char;
  HostEnt: PHostEnt;
  H : String;
begin
  ConnInit:=false;
  Limited:=false;
  If Not InitConnection Then
  Begin
    Application.MessageBox('FFSS server does not seem to be running. Executing in limited configuration mode','FFSS Share Info',MB_OK);
    Limited:=true;
    TabSheet2.TabVisible:=False;
    TabSheet3.TabVisible:=False;
    H:='';
    GetHostName(buf,SizeOf(buf));
    HostEnt:=gethostbyname(PChar('ffss'));
    If HostEnt <> Nil Then
    Begin
      H:=HostEnt.h_name;
      If Pos('.',HostEnt.h_name) <> 0 Then
      Begin                           
        H:=HostEnt.h_name;
      End;
    End;
    Edit3.Text:=RB1.GetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Name',Buf);
    Edit4.Text:=RB1.GetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Comment','');
    Edit5.Text:=RB1.GetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Master',H);
    Edit6.Text:=IntToStr(RB1.GetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Idle',0));
    Edit7.Text:=IntToStr(RB1.GetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_MaxConn',10));
    Edit8.Text:=IntToStr(RB1.GetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_FTP_MaxConn',10));
    Edit10.Text:=IntToStr(RB1.GetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_MaxXFerPerConn',2));
    CheckBox1.Checked:=Boolean(RB1.GetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_FTP',0));
  End
  Else
    ConnInit:=true;
  If ParamCount = 0 Then
    OnFolder:=False
  else
    OnFolder:=True;
  ST1.ShowAppIcon:=False;
  ST1.Icon:=Application.Icon;
  If Not Limited Then
  Begin
    Global:=RequestGlobalInfo;
    Edit3.Text:=Global.Name;
    Edit4.Text:=Global.Comment;
    Edit5.Text:=Global.Master;
    Edit6.Text:=IntToStr(Global.Idle);
    Edit7.Text:=IntToStr(Global.MaxConn);
    CheckBox1.Checked:=Global.FTP;
    Edit8.Text:=IntToStr(Global.FTP_MaxConn);
  End;
  If OnFolder And Not Limited Then
  Begin
    Form1.Caption:='Propri�t�s de '+GetSharePath;
    Share:=RequestShareInfo(GetGoodSharePath);
    If Share = Nil Then
    Begin
      RadioButton1.Checked:=True;
      ShareState:=SHARE_ADD;
      Edit1.Text:=SuggestedName;
    End
    Else
    Begin
      ShareState:=SHARE_UPDT;
      RadioButton2.Checked:=True;
      Edit1.Text:=Share.Name;
      Edit2.Text:=Share.Comment;
      CheckBox2.Checked:=Not Share.Writeable;
      CheckBox3.Checked:=Share.Privat;
      Edit9.Text:=IntToStr(Share.MaxConn);
    End;
    TabSheet2.TabVisible:=True;
  End
  Else
    TabSheet2.TabVisible:=False;
  If Not Limited Then
  Begin
    RequestStateInfo;
    RequestSharesList;
  End;
  Button3.Enabled:=False;
  GblChanged:=False;
  ShrChanged:=False;
  StChanged:=False;
  Reboot:=False;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

function TForm1.RequestGlobalInfo : PGlobal;
Var Gbl : PGlobal;
    Buf : Array[0..10000] Of Char;
    Size : DWord;
    Got : DWord;
    Pos : DWord;
begin
  Result:=Nil;
  Buf[0]:=Char(FS_OPCODE_GETGLOBAL);
  Size:=1;
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);
  Got:=CS.Socket.ReceiveBuf(Size,sizeof(Size));
  If Got <> Sizeof(Size) Then Exit;
  Got:=CS.Socket.ReceiveBuf(Buf,Size);
  If Got < 1 Then Exit;
  If Buf[0] <> Char(FS_OPCODE_ACK) Then Exit;
  While Got < Size Do
  Begin
    Got:=Got + CS.Socket.ReceiveBuf(Buf[Got],Size-Got);
  End;
  New(Gbl);
  Pos:=1;
  Gbl.Name:=String(Buf+Pos);
  Inc(Pos,StrLen(Buf+Pos)+1);
  Gbl.Comment:=String(Buf+Pos);
  Inc(Pos,StrLen(Buf+Pos)+1);
  Gbl.Master:=String(Buf+Pos);
  Inc(Pos,StrLen(Buf+Pos)+1);
  Gbl.Idle:=StrToInt(String(Buf+Pos));
  Inc(Pos,StrLen(Buf+Pos)+1);
  Gbl.MaxConn:=StrToInt(String(Buf+Pos));
  Inc(Pos,StrLen(Buf+Pos)+1);
  Gbl.MaxXFerPerConn:=StrToInt(String(Buf+Pos));
  Inc(Pos,StrLen(Buf+Pos)+1);
  Gbl.FTP:=Boolean(StrToInt(String(Buf+Pos)));
  Inc(Pos,StrLen(Buf+Pos)+1);
  Gbl.FTP_MaxConn:=StrToInt(String(Buf+Pos));
  Result:=Gbl;
end;

function TForm1.RequestShareInfo(Path : String) : PShare;
Var Shar : PShare;
    Buf : Array[0..10000] Of Char;
    Size : DWord;
    Got : DWord;
    Pos : DWord;
begin
  Result:=Nil;
  Buf[0]:=Char(FS_OPCODE_GETSHARE);
  Size:=1;
  StrPCopy(Buf+Size,GetSharePath);
  Inc(Size,Length(GetSharePath)+1);
  StrPCopy(Buf+Size,Path);
  Inc(Size,Length(Path)+1);
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);
  Got:=CS.Socket.ReceiveBuf(Size,sizeof(Size));
  If Got <> Sizeof(Size) Then Exit;
  Got:=CS.Socket.ReceiveBuf(Buf,Size);
  If Got < 2 Then Exit;
  If Buf[1] = Char(FS_OPCODE_ACK) Then SuggestedName:=GetSharePath
  Else SuggestedName:='';
  If Buf[0] <> Char(FS_OPCODE_ACK) Then Exit;
  While Got < Size Do
  Begin
    Got:=Got + CS.Socket.ReceiveBuf(Buf[Got],Size-Got);
  End;
  New(Shar);
  Pos:=1;
  Shar.Name:=String(Buf+Pos);
  Inc(Pos,StrLen(Buf+Pos)+1);
  Shar.Comment:=String(Buf+Pos);
  Inc(Pos,StrLen(Buf+Pos)+1);
  Shar.Writeable:=Boolean(StrToInt(String(Buf+Pos)));
  Inc(Pos,StrLen(Buf+Pos)+1);
  Shar.Privat:=Boolean(StrToInt(String(Buf+Pos)));
  Inc(Pos,StrLen(Buf+Pos)+1);
  Shar.MaxConn:=StrToInt(String(Buf+Pos));
//  Inc(Pos,StrLen(Buf+Pos)+1);
  Result:=Shar;
end;

procedure TForm1.RequestStateInfo;
Var Buf : Array[0..10] Of Char;
    Got : DWord;
    Size : DWord;
begin
  Buf[0]:=Char(FS_OPCODE_GETSTATE);
  Size:=1;
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);
  Got:=CS.Socket.ReceiveBuf(Size,sizeof(Size));
  If Got <> Sizeof(Size) Then Exit;
  Got:=CS.Socket.ReceiveBuf(Buf,Size);
  If Got < 1 Then Exit;
  If Buf[0] = Char(FFSS_STATE_ON) Then RadioGroup1.ItemIndex:=0
  Else RadioGroup1.ItemIndex:=1;
end;

procedure TForm1.RequestSharesList;
Var Shar : PShare;
    Buf : Array[0..10000] Of Char;
    Size : DWord;
    Got : DWord;
    Pos : DWord;
    Nb,I : DWord;
    Item : TListItem;
begin
  ListView1.Items.Clear;
  Buf[0]:=Char(FS_OPCODE_GETSHRLIST);
  Size:=1;
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);
  Got:=CS.Socket.ReceiveBuf(Size,sizeof(Size));
  If Got <> Sizeof(Size) Then Exit;
  Got:=CS.Socket.ReceiveBuf(Buf,Size);
  If Got < 1 Then Exit;
  If Buf[0] <> Char(FS_OPCODE_ACK) Then Exit;
  While Got < Size Do
  Begin
    Got:=Got + CS.Socket.ReceiveBuf(Buf[Got],Size-Got);
  End;
  New(Shar);
  Pos:=1;
  Nb:=StrToInt(String(Buf+Pos));
  If Nb = 0 Then Exit;
  Inc(Pos,StrLen(Buf+Pos)+1);
  For I:=0 To Nb-1 Do
  Begin
    Item:=ListView1.Items.Add;
    Item.Caption:=String(Buf+Pos);
    Inc(Pos,StrLen(Buf+Pos)+1);
    Item.SubItems.Add(String(Buf+Pos));
    Inc(Pos,StrLen(Buf+Pos)+1);
    If String(Buf+Pos) = '0' Then
      Item.SubItems.Add(STRING_YES)
    Else
      Item.SubItems.Add(STRING_NO);
    Inc(Pos,StrLen(Buf+Pos)+1);
    Item.SubItems.Add(String(Buf+Pos));
    Inc(Pos,StrLen(Buf+Pos)+1);
    Item.SubItems.Add(String(Buf+Pos));
    Inc(Pos,StrLen(Buf+Pos)+1);
  End;
end;

procedure TForm1.RequestEject(ShareName : String);
Var Buf : Array[0..1000] Of Char;
    Size : DWord;
begin
  Buf[0]:=Char(FS_OPCODE_EJECT);
  Size:=1;
  StrPCopy(Buf+Size,ShareName);
  Inc(Size,Length(ShareName)+1);
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);
end;

Procedure TForm1.RequestConns(Path : String);
Var Buf : Array[0..10000] Of Char;
    Size : DWord;
    Got : DWord;
    Pos : DWord;
    NB,I,J : DWord;
    Item : TListItem;
begin
  Buf[0]:=Char(FS_OPCODE_GETSHRCONNS);
  Size:=1;
  StrPCopy(Buf+Size,Path);
  Inc(Size,Length(Path)+1);
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);
  Got:=CS.Socket.ReceiveBuf(Size,sizeof(Size));
  If Got <> Sizeof(Size) Then Exit;
  Got:=CS.Socket.ReceiveBuf(Buf,Size);
  If Got < 1 Then Exit;
  If Buf[0] <> Char(FS_OPCODE_ACK) Then Exit;
  While Got < Size Do
  Begin
    Got:=Got + CS.Socket.ReceiveBuf(Buf[Got],Size-Got);
  End;
  Pos:=1;
  Nb:=StrToInt(String(Buf+Pos));
  If Nb = 0 Then Exit;
  Inc(Pos,StrLen(Buf+Pos)+1);
  For I:=0 To Nb-1 Do
  Begin
    Item:=Form2.ListView1.Items.Add;
    Item.Caption:=String(Buf+Pos);
    Inc(Pos,StrLen(Buf+Pos)+1);
    Item.SubItems.Add(String(Buf+Pos));
    Inc(Pos,StrLen(Buf+Pos)+1);
    Item.SubItems.Add(String(Buf+Pos));
    Inc(Pos,StrLen(Buf+Pos)+1);
    If StrToInt(Item.SubItems[0]) <> 0 Then
    Begin
      For J:=0 To StrToInt(Item.SubItems[0])-1 Do
      Begin
        // Get String(Buf+Pos)
        Inc(Pos,StrLen(Buf+Pos)+1);
        // Get %
        Inc(Pos,StrLen(Buf+Pos)+1);
      End;
    End;
    If StrToInt(Item.SubItems[1]) <> 0 Then
    Begin
      For J:=0 To StrToInt(Item.SubItems[1])-1 Do
      Begin
        // Get String(Buf+Pos)
        Inc(Pos,StrLen(Buf+Pos)+1);
        // Get %
        Inc(Pos,StrLen(Buf+Pos)+1);
      End;
    End;
  End;
end;

procedure TForm1.RequestEjectIP(ShareName,IP : String);
Var Buf : Array[0..1000] Of Char;
    Size : DWord;
begin
  Buf[0]:=Char(FS_OPCODE_EJECTIP);
  Size:=1;
  StrPCopy(Buf+Size,ShareName);
  Inc(Size,Length(ShareName)+1);
  StrPCopy(Buf+Size,IP);
  Inc(Size,Length(IP)+1);
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);
end;

function TForm1.SetShareInfo(SharePath : String;Etat : Integer) : Boolean;
Var Buf : Array[0..10000] Of Char;
    Size : DWord;
    Got : DWord;
begin
  Result:=True;
  If Etat = SHARE_DELETE Then // Del share
  Begin
    Buf[0]:=Char(FS_OPCODE_DELSHARE);
    Size:=1;
    StrPCopy(Buf+Size,SharePath);
    Inc(Size,Length(SharePath)+1);
  End
  Else
  Begin
    If Etat = SHARE_ADD Then // Add Share
      Buf[0]:=Char(FS_OPCODE_ADDSHARE)
    Else // Update Share
      Buf[0]:=Char(FS_OPCODE_UPDTSHARE);
    Size:=1;
    StrPCopy(Buf+Size,Edit1.Text);
    Inc(Size,Length(Edit1.Text)+1);
    StrPCopy(Buf+Size,SharePath);
    Inc(Size,Length(SharePath)+1);
    StrPCopy(Buf+Size,Edit2.Text);
    Inc(Size,Length(Edit2.Text)+1);
    StrPCopy(Buf+Size,IntToStr(Integer(Not CheckBox2.Checked)));
    Inc(Size,IntLength(Integer(CheckBox2.Checked))+1);
    StrPCopy(Buf+Size,IntToStr(Integer(CheckBox3.Checked)));
    Inc(Size,IntLength(Integer(CheckBox3.Checked))+1);
    StrPCopy(Buf+Size,Edit9.Text);
    Inc(Size,Length(Edit9.Text)+1);
    StrPCopy(Buf+Size,IntToStr(0)); // 0 Users for now
    Inc(Size,IntLength(0)+1);
    ShareState:=SHARE_UPDT;
  End;
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);

  Got:=CS.Socket.ReceiveBuf(Size,sizeof(Size));
  If Got <> Sizeof(Size) Then Exit;
  Got:=CS.Socket.ReceiveBuf(Buf,Size);
  If Got < 1 Then Exit;
  If Buf[0] <> Char(FS_OPCODE_ACK) Then
  Begin
    Application.MessageBox('Ce nom de partage existe d�ja','FFSS Server Error',MB_OK);
    Result:=False;
  End;
end;

procedure TForm1.SetGlobalInfo;
Var Buf : Array[0..10000] Of Char;
    Size : DWord;
    Got : DWord;
begin
  If Limited Then
  Begin
    RB1.SetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Name',Edit3.Text);
    RB1.SetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Comment',Edit4.Text);
    RB1.SetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Master',Edit5.Text);
    RB1.SetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Idle',StrToInt(Edit6.Text));
    RB1.SetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_MaxConn',StrToInt(Edit7.Text));
    RB1.SetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_FTP_MaxConn',StrToInt(Edit8.Text));
    RB1.SetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_MaxXFerPerConn',StrToInt(Edit10.Text));
    RB1.SetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_FTP',Integer(CheckBox1.Checked));
    Exit;
  End;
  Buf[0]:=Char(FS_OPCODE_UPDTGLOBAL);
  Size:=1;
  StrPCopy(Buf+Size,Edit3.Text);
  Inc(Size,Length(Edit3.Text)+1);
  StrPCopy(Buf+Size,Edit4.Text);
  Inc(Size,Length(Edit4.Text)+1);
  StrPCopy(Buf+Size,Edit5.Text);
  Inc(Size,Length(Edit5.Text)+1);
  StrPCopy(Buf+Size,Edit6.Text);
  Inc(Size,Length(Edit6.Text)+1);
  StrPCopy(Buf+Size,Edit7.Text);
  Inc(Size,Length(Edit7.Text)+1);
  StrPCopy(Buf+Size,Edit10.Text);
  Inc(Size,Length(Edit10.Text)+1);
  If CheckBox1.Checked Then
    StrPCopy(Buf+Size,'1')
  Else
    StrPCopy(Buf+Size,'0');
  Inc(Size,1+1);
  StrPCopy(Buf+Size,Edit8.Text);
  Inc(Size,Length(Edit8.Text)+1);
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);

  Got:=CS.Socket.ReceiveBuf(Size,sizeof(Size));
  If Got <> Sizeof(Size) Then Exit;
  Got:=CS.Socket.ReceiveBuf(Buf,Size);
  If Got < 1 Then Exit;
  If Buf[0] <> Char(FS_OPCODE_ACK) Then
    Application.MessageBox('Erreur lors du changement des informations du server ffss','FFSS Server Error',MB_OK)
  Else
  Begin
    If Reboot Then
      Application.MessageBox('Vous devez red�marrer le server ffss, ou votre windows','FFSS Server Info',MB_OK Or MB_ICONEXCLAMATION);
  End;
end;

procedure TForm1.SetStateInfo;
Var Buf : Array[0..10] Of Char;
    Size : DWord;
begin
  Buf[0]:=Char(FS_OPCODE_SETSTATE);
  If RadioGroup1.ItemIndex = 0 Then
    Buf[1]:=Char(FFSS_STATE_ON)
  Else
    Buf[1]:=Char(FFSS_STATE_QUIET);
  Size:=2;
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);
end;

procedure TForm1.SetShareState(ShareName : String;State : Boolean);
Var Buf : Array[0..1000] Of Char;
    Size : DWord;
begin
  Buf[0]:=Char(FS_OPCODE_SETSHARESTATE);
  Buf[1]:=Char(State);
  Size:=2;
  StrPCopy(Buf+Size,ShareName);
  Inc(Size,Length(ShareName)+1);
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);
end;

procedure TForm1.SendRescanQuery(ShareName : String);
Var Buf : Array[0..1000] Of Char;
    Size : DWord;
begin
  Buf[0]:=Char(FS_OPCODE_RESCAN);
  Size:=1;
  StrPCopy(Buf+Size,ShareName);
  Inc(Size,Length(ShareName)+1);
  CS.Socket.SendBuf(Size,sizeof(Size));
  CS.Socket.SendBuf(Buf,Size);
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  Panel1.Enabled:=False;
  Edit1.Color:=clBtnFace;
  Edit2.Color:=clBtnFace;
  Edit9.Color:=clBtnFace;
  CheckBox2.Enabled:=False;
  CheckBox3.Enabled:=False;
  Button4.Enabled:=False;
  ShrChanged:=True;
  Button3.Enabled:=True;
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  Panel1.Enabled:=True;
  Edit1.Color:=clWindow;
  Edit2.Color:=clWindow;
  Edit9.Color:=clWindow;
  CheckBox2.Enabled:=True;
  CheckBox3.Enabled:=True;
  Button4.Enabled:=True;
  ShrChanged:=True;
  Button3.Enabled:=True;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  GblChanged:=True;
  Button3.Enabled:=True;
  Reboot:=True;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  ShrChanged:=True;
  Button3.Enabled:=True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button3Click(Sender);
  If Not ChkError Then Close;
end;

procedure TForm1.Button3Click(Sender: TObject);
Var Res : Boolean;
begin
  Res:=True;
  If GblChanged Then
  Begin
    If Not CheckGlobal Then Exit;
    SetGlobalInfo;
    GblChanged:=False;
  End;
  If Limited Then
  Begin
    Button3.Enabled:=False;
    Exit;
  End;
  If ShrChanged Then
  Begin
    If Not CheckShare Then Exit;
    If RadioButton1.Checked Then Res:=SetShareInfo(GetGoodSharePath,SHARE_DELETE)
    Else Res:=SetShareInfo(GetGoodSharePath,ShareState);
    If Res Then
    Begin
      ShrChanged:=False;
      RequestSharesList;
    End;
  End;
  If StChanged Then
  Begin
    SetStateInfo;
    StChanged:=False;
  End;
  If Res Then
    Button3.Enabled:=False;
  ChkError:=Not Res;
end;

function TForm1.CheckGlobal : Boolean;
Begin
  ChkError:=True;
  Result:=False;
  If Edit3.Text = '' Then
  Begin
    PageControl1.ActivePageIndex:=0;
    Application.MessageBox('Le champ "Nom de machine" ne doit pas �tre vide','FFSS Share Error',MB_OK);
    Exit;
  End;
  Try
    StrToInt(Edit6.Text);
  Except
    PageControl1.ActivePageIndex:=0;
    Application.MessageBox('Mauvaise valeur dans le champ "D�lai d''idle"','FFSS Share Error',MB_OK);
    Exit;
  End;
  Try
    StrToInt(Edit7.Text);
  Except
    PageControl1.ActivePageIndex:=0;
    Application.MessageBox('Mauvaise valeur dans le champ "Max de connexions"','FFSS Share Error',MB_OK);
    Exit;
  End;
  Try
    StrToInt(Edit10.Text);
  Except
    PageControl1.ActivePageIndex:=0;
    Application.MessageBox('Mauvaise valeur dans le champ "Max transferts par connexions"','FFSS Share Error',MB_OK);
    Exit;
  End;
  Try
    StrToInt(Edit8.Text);
  Except
    PageControl1.ActivePageIndex:=0;
    Application.MessageBox('Mauvaise valeur dans le champ "Max de connexions"','FFSS Share Error',MB_OK);
    Exit;
  End;

  ChkError:=False;
  Result:=True;
End;

function TForm1.CheckShare : Boolean;
Begin
  ChkError:=True;
  Result:=False;
  If Edit1.Text = '' Then
  Begin
    PageControl1.ActivePageIndex:=1;
    Application.MessageBox('Le champ "Nom de partage" ne doit pas �tre vide','FFSS Share Error',MB_OK);
    Exit;
  End;
  If Pos('#',Edit1.Text) <> 0 Then
  Begin
    PageControl1.ActivePageIndex:=1;
    Application.MessageBox('Le champ "Nom de partage" ne doit pas contenir de #','FFSS Share Error',MB_OK);
    Exit;
  End;
  Try
    StrToInt(Edit9.Text);
  Except
    PageControl1.ActivePageIndex:=1;
    Application.MessageBox('Mauvaise valeur dans le champ "Max de connexions"','FFSS Share Error',MB_OK);
    Exit;
  End;


  ChkError:=False;
  Result:=True;
End;

procedure TForm1.Edit3Change(Sender: TObject);
begin
  GblChanged:=True;
  Button3.Enabled:=True;
  Reboot:=True;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  ShrChanged:=True;
  Button3.Enabled:=True;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  If OnFolder And Not Limited Then
  Begin
    If Share = Nil Then
      RadioButton1.SetFocus
    Else
      RadioButton2.SetFocus;
  End;
end;

procedure TForm1.CSDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  If ConnInit Then
    Application.MessageBox('Connection lost with the server, some error might have occured. See FFSS_Server.log for more infos','FFSS Share Error',MB_OK Or MB_ICONEXCLAMATION);
  Close;
end;

procedure TForm1.CSError(Sender: TObject; Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  If ConnInit Then
    Application.MessageBox('Connection lost with the server, some error might have occured. See FFSS_Server.log for more infos','FFSS Share Error',MB_OK Or MB_ICONEXCLAMATION);
  Close;
end;

procedure TForm1.Edit5Change(Sender: TObject);
begin
  GblChanged:=True;
  Button3.Enabled:=True;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  StChanged:=True;
  Button3.Enabled:=True;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  If ListView1.ItemFocused = Nil Then
  Begin
    Supprimer1.Enabled:=False;
    Rescan1.Enabled:=False;
    Actif1.Enabled:=False;
    Dsactiver1.Enabled:=False;
    Ejecter1.Enabled:=False;
  End
  Else
  Begin
    Supprimer1.Enabled:=True;
    Rescan1.Enabled:=True;
    If ListView1.ItemFocused.SubItems[1] = STRING_YES Then
    Begin
      Actif1.Enabled:=False;
      Dsactiver1.Enabled:=True;
    End
    Else
    Begin
      Actif1.Enabled:=True;
      Dsactiver1.Enabled:=False;
    End;
    If ListView1.ItemFocused.SubItems[2] = '0' Then
      Ejecter1.Enabled:=False
    Else
      Ejecter1.Enabled:=True;
  End;
end;

procedure TForm1.Supprimer1Click(Sender: TObject);
begin
  If Application.MessageBox('Etes vous sur de vouloir retirer ce partage ?','FFSS Share Info',MB_YESNO) = IDNO Then Exit;
  If SetShareInfo(ListView1.ItemFocused.SubItems[0],SHARE_DELETE) Then
    RequestSharesList;
end;

procedure TForm1.Rescan1Click(Sender: TObject);
begin
  If ListView1.ItemFocused.SubItems[2] <> '0' Then
  Begin
    If Application.MessageBox('ATTENTION : Faire un rescan du partage deconnecte tout le monde. Etes vous sur de vouloir continuer ?','FFSS Share Info',MB_YESNO) = IDNO Then Exit;
  End;
  SendRescanQuery(ListView1.ItemFocused.Caption);
end;

procedure TForm1.Actif1Click(Sender: TObject);
begin
  SetShareState(ListView1.ItemFocused.Caption,True);
end;

procedure TForm1.Dsactiver1Click(Sender: TObject);
begin
  SetShareState(ListView1.ItemFocused.Caption,False);
end;

procedure TForm1.Ejecter1Click(Sender: TObject);
begin
  RequestEject(ListView1.ItemFocused.Caption);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  RequestSharesList;
end;

procedure TForm1.ListView1DblClick(Sender: TObject);
begin
  If ListView1.Selected <> Nil Then
  Begin
    If ListView1.Selected.SubItems[3] <> '0' Then
    Begin
      Form2.Caption := 'Connexions � ' + ListView1.Selected.Caption;
      Form2.ListView1.Items.Clear;
      Form2.ListView1Click(Sender);
      RequestConns(ListView1.Selected.SubItems[0]);
      Form2.ShowModal;
    End;
  End;
end;

end.
