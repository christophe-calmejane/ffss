unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm3 = class(TForm)
    ListView1: TListView;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses Unit4, Unit1;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  Form4.ShowModal;
end;

procedure TForm3.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  If ListView1.Selected = Nil Then
    Button2.Enabled:=False
  Else
    Button2.Enabled:=True;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  ListView1.DeleteSelected;
  ShrChanged:=True;
  Form1.Button3.Enabled:=True;
end;

end.
