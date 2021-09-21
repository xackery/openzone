unit frmScriptLogUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmScriptLog = class(TForm)
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmScriptLog: TfrmScriptLog;

implementation

Uses ZoneClasses;

{$R *.dfm}

procedure TfrmScriptLog.FormShow(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.Lines.AddStrings(CompileLog); 
  Memo1.Lines.AddStrings(ParseLog);
end;

end.
