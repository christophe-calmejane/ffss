unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, RegisteryUnit, Winsock;

type
  TForm1 = class(TForm)
    Label4: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    BitBtn1: TBitBtn;
    CheckBox1: TCheckBox;
    RB1: TRegisteryBase;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
Var S,S2 : String;
    H : HKEY;
    buf : Array[0..1024] Of Char;
    idx : Integer;
begin
  S:=RB1.GetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\ShareNames','');
  S2:='';
  While S <> '' Do
  Begin
    If S2 <> '' Then
      S2:=S2+'|';
    If Pos('#',S) <> 0 Then
    Begin
      S2:=S2+Copy(S,0,Pos('#',S)-1);
      S := Copy(S,Pos('#',S)+1,255);
    End
    Else
    Begin
      S2:=S2+S;
      S:='';
    End;
  End;
  RB1.SetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\ShareNames',S2);

  If Edit2.Text <> '' Then
    RB1.SetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Name',Edit2.Text);
  If Edit3.Text <> '' Then
    RB1.SetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Comment',Edit3.Text);
  RB1.SetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Master',Edit4.Text);
  If CheckBox1.Checked And CheckBox1.Enabled Then
  Begin
    S:='HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Network\LanMan\';
    H:=RB1.OpenKeys(S,KEY_ENUMERATE_SUB_KEYS);
    idx:=0;
    S2:='';
    while RegEnumKey(H,idx,buf,SizeOf(buf)) = ERROR_SUCCESS Do
    Begin
      if S2 = '' Then
        S2:=buf
      else
        S2:=S2+'|'+buf;
      RB1.SetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\'+buf+'_Path',RB1.GetStrValue('HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Network\LanMan\'+buf+'\Path',''));
      RB1.SetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\'+buf+'_Comment',RB1.GetStrValue('HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Network\LanMan\'+buf+'\Remark',''));
      RB1.SetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\'+buf+'_Users','');
      RB1.SetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\'+buf+'_MaxConnections',0);
      RB1.SetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\'+buf+'_Private',0);
      RB1.SetIntValue('HKEY_CURRENT_USER\Software\FFSS\Server\'+buf+'_Writeable',0);
      Application.MessageBox(PChar('Importing share name '+buf+' : '+RB1.GetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\'+buf+'_Path','')),'Import share',MB_OK);
      Inc(idx);
    End;
    RB1.SetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\ShareNames',S2);
  End;
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
Var buf : Array[0..512] Of Char;
  ErrorCode: Integer;
  WSAData: TWSAData;
  HostEnt: PHostEnt;
  H : String;
  Os : TOSVersionInfo;
begin
  ErrorCode:=WSAStartup($0202, WSAData);
  If ErrorCode <> 0 Then
  Begin
    Application.MessageBox('Winsock2 not found !! Please install winsock2 and re-run setup','FFSS Error',MB_OK);
    Halt;
  End;
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
  Edit2.Text:=RB1.GetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Name',Buf);
  Edit3.Text:=RB1.GetStrValue('HKEY_CURRENT_USER\Software\FFSS\Server\Global_Comment','');
  Edit4.Text:=H;

  Os.dwOSVersionInfoSize:=SizeOf(TOSVersionInfo);
  GetVersionEx(Os);
  If OS.dwPlatformId <> VER_PLATFORM_WIN32_NT Then
    CheckBox1.Enabled:=true
  else
    CheckBox1.Enabled:=false;
end;

end.
