unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DIFFMAKE;

type
  TForm1 = class(TForm)
    DiffMaker1: TDiffMaker;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ExtractButton: TButton;
    CompressButton: TButton;
    Button3: TButton;
    InFileEdit: TEdit;
    OutFileEdit: TEdit;
    UseFileEdit: TEdit;
    B1: TButton;
    B2: TButton;
    B3: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button3Click(Sender: TObject);
    procedure CompressButtonClick(Sender: TObject);
    procedure ExtractButtonClick(Sender: TObject);
    procedure B1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.CompressButtonClick(Sender: TObject);
begin
  DiffMaker1.Compress;
end;

procedure TForm1.ExtractButtonClick(Sender: TObject);
begin
  DiffMaker1.Extract;
end;

procedure TForm1.B1Click(Sender: TObject);
var
  fname :string;
begin
  if SaveDialog1.Execute then
    with DiffMaker1 do
    begin
      fname := SaveDialog1.FileName;
      case TButton(Sender).Tag of
        0:
          begin
            InFile := fname;
            InFileEdit.Text:= fname;
          end;
        1:
          begin
            OutFile := fname;
            OutFileEdit.Text:= fname;
          end;
        2:
          begin
            UseFile := fname;
            UseFileEdit.Text:= fname;
          end;
      end;
    end;
end;

initialization
  //RegisterFileNamePropEdits;

end.
