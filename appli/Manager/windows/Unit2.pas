unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    ListView1: TListView;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListView1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;                                

var
  Form2: TForm2;

implementation

uses Unit1;

{$R *.DFM}

procedure TForm2.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  If Key = VK_ESCAPE Then Close;

end;

procedure TForm2.ListView1Click(Sender: TObject);
begin
  If ListView1.Selected <> Nil Then
    Button2.Enabled := True
  Else
    Button2.Enabled := False;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  Form1.RequestEject(Form1.ListView1.ItemFocused.Caption);
  Form1.RequestSharesList;
  Close;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  Form1.RequestEjectIP(Form1.ListView1.Selected.SubItems[0],ListView1.ItemFocused.Caption);
  {ListView1.Items.Clear;
  ListView1Click(Sender);
  Form1.RequestConns(Form1.ListView1.Selected.SubItems[0]);}
  Close;
end;

end.
