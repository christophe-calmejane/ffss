unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm4 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses Unit1, Unit3;

{$R *.dfm}

procedure TForm4.FormShow(Sender: TObject);
begin
  Edit1.Text:='';
  Edit2.Text:='';
  CheckBox1.Checked:=False;
  Edit1.SetFocus;
end;

procedure TForm4.Button1Click(Sender: TObject);
Var Item : TListItem;
    Tmp : String;
begin
  If Edit1.Text = '' Then
  Begin
    Application.MessageBox('Vous devez spécifier un nom de login','FFSS Share Info',MB_OK);
    Exit;
  End;
  If Edit2.Text = '' Then
  Begin
    Application.MessageBox('Vous devez spécifier un mot de passe','FFSS Share Info',MB_OK);
    Exit;
  End;
  Item:=Form3.ListView1.Items.Add;
  Item.Caption:=Edit1.Text;
  Tmp:=Edit2.Text;
  If CheckBox1.Checked Then
    Item.SubItems.Add('Oui')
  Else
    Item.SubItems.Add('Non');
  Item.SubItems.Add(Tmp);
  ShrChanged:=True;
  Form1.Button3.Enabled:=True;
  Close;
end;

end.
