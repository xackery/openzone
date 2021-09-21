unit frmFlipTexCoordsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ClearTypeText, OZDMUnit, IAeverButton;

type
  TfrmFlipTexCoords = class(TForm)
    cbFlipHorizontal: TClearTypeCheckBox;
    cbFlipVertical: TClearTypeCheckBox;
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
  frmFlipTexCoords: TfrmFlipTexCoords;

implementation

{$R *.dfm}

procedure TfrmFlipTexCoords.btnHelpClick(Sender: TObject);
begin
  Application.HelpJump('Manipulating_objects');
end;

end.
