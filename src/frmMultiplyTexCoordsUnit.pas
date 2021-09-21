unit frmMultiplyTexCoordsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ClearTypeText, OZDMUnit, IAeverButton;

type
  TfrmMultiplyTexCoords = class(TForm)
    Label1: TClearTypeLabel;
    Label2: TClearTypeLabel;
    edtHorizontal: TClearTypeEdit;
    edtVertical: TClearTypeEdit;
    btnOk: TIAEverButton;
    btnCancel: TIAEverButton;
    btnHelp: TIAEverButton;
    procedure btnHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMultiplyTexCoords: TfrmMultiplyTexCoords;

implementation

{$R *.dfm}

procedure TfrmMultiplyTexCoords.btnHelpClick(Sender: TObject);
begin
  Application.HelpJump('Manipulating_objects');
end;

end.
