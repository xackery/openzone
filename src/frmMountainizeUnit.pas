unit frmMountainizeUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ClearTypeText, OZDMUnit, IAeverButton;

type
  TfrmMountainize = class(TForm)
    cbNorth: TClearTypeCheckBox;
    cbSouth: TClearTypeCheckBox;
    cbEast: TClearTypeCheckBox;
    cbWest: TClearTypeCheckBox;
    btnOk: TIAEverButton;
    btnCancel: TIAEverButton;
    cbGrow: TClearTypeCheckBox;
    Label1: TClearTypeLabel;
    edtAmount: TClearTypeEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMountainize: TfrmMountainize;

implementation

{$R *.dfm}

end.
