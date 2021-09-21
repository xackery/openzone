unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormFill, ExtCtrls, StdCtrls, TrCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    ComboBox1: TComboBox;
    DCFormFill1: TDCFormFill;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  DCFormFill1.Visible := DCFormFill1.Visible xor true;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  DCFormFill1.FillType:='Oshawa';
  Combobox1.Text:=DCFormFill1.FillType;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  DCFormFill1.FillType:=Combobox1.Items[Combobox1.ItemIndex];
end;


end.
