unit frmShiftZoneUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ClearTypeText, OZDMUnit, IAeverButton;

type
  TfrmShiftZone = class(TForm)
    Label1: TClearTypeLabel;
    Label2: TClearTypeLabel;
    Label3: TClearTypeLabel;
    Label4: TClearTypeLabel;
    GroupBox1: TClearTypeGroupBox;
    Label5: TClearTypeLabel;
    Label6: TClearTypeLabel;
    Label7: TClearTypeLabel;
    edtXShift: TClearTypeEdit;
    edtYShift: TClearTypeEdit;
    edtZShift: TClearTypeEdit;
    Label8: TClearTypeLabel;
    btnCancel: TIAEverButton;
    btnOk: TIAEverButton;
    btnNegate: TIAEverButton;
    procedure btnNegateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmShiftZone: TfrmShiftZone;

implementation

{$R *.dfm}

procedure TfrmShiftZone.btnNegateClick(Sender: TObject);
Var
  I : Integer;
  E : Extended;

begin
  Val(Trim(edtXShift.Text),E,I);
  If I = 0 Then edtXShift.Text := FloatToStr(-E);
  Val(Trim(edtYShift.Text),E,I);
  If I = 0 TheN edtYShift.Text := FloatToStr(-E);
  Val(Trim(edtZShift.Text),E,I);
  If I = 0 Then edtZShift.Text := FloatToStr(-E);
end;

end.
