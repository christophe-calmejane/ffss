unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm5 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses Unit1;

{$R *.dfm}

procedure TForm5.Button1Click(Sender: TObject);
Var Len : Integer;
    res : Integer;
    buf : Array[0..512] Of char;
begin
  If (Edit1.Text = '') Or (Edit2.Text = '') Or (Edit3.Text = '') Then
  Begin
    Application.MessageBox('Tous les champs doivent être remplis','FFSS Share Info',MB_OK);
    exit;
  End;
  Form1.CS.Tag:=1;
  Form1.CS.Close;
  Form1.CS.Host:=Edit1.Text;
  Form1.CS.Open;
  If Not Form1.CS.Active Then
  Begin
    Application.MessageBox('Impossible de se connecter à l''hôte désigné','FFSS Share Info',MB_OK);
    exit;
  End;
  Try
  Len:=Length(Edit2.Text);
  StrPCopy(Buf,Edit2.Text);
  res:=Form1.CS.Socket.SendBuf(Len,sizeof(Len));
  If res <> sizeof(Len) Then
  Begin
    Application.MessageBox('Erreur lors de l''authentification à distance','FFSS Share Info',MB_OK);
    exit;
  End;
  res:=Form1.CS.Socket.SendBuf(Buf,Len);
  If res <> Len Then
  Begin
    Application.MessageBox('Erreur lors de l''authentification à distance','FFSS Share Info',MB_OK);
    exit;
  End;
  Len:=Length(Edit3.Text);
  StrPCopy(Buf,Edit3.Text);
  res:=Form1.CS.Socket.SendBuf(Len,sizeof(Len));
  If res <> sizeof(Len) Then
  Begin
    Application.MessageBox('Erreur lors de l''authentification à distance','FFSS Share Info',MB_OK);
    exit;
  End;
  res:=Form1.CS.Socket.SendBuf(Buf,Len);
  If res <> Len Then
  Begin
    Application.MessageBox('Erreur lors de l''authentification à distance','FFSS Share Info',MB_OK);
    exit;
  End;
  Form1.RequestShareAvail('toto'); // Dummy call, to check if connection still active
  Except
    Form1.CS.Close;
  End;
  If Not Form1.CS.Active Then
  Begin
    Application.MessageBox('Connexion refusée par l''hôte distant','FFSS Share Info',MB_OK);
    exit;
  End;
  ConnInit:=True;
  Limited:=false;
  Form1.GetInitValues(true);

  Form5.Close;
end;

procedure TForm5.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  If Key = VK_ESCAPE Then
    Close;
end;

end.
