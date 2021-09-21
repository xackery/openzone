unit frmExtendGroundUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls;

type
  TfrmExtendGround = class(TForm)
    cbNorth: TCheckBox;
    cbSouth: TCheckBox;
    cbEast: TCheckBox;
    cbWest: TCheckBox;
    Label1: TLabel;
    edtExtend: TEdit;
    UpDown1: TUpDown;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    cbRumple: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmExtendGround: TfrmExtendGround;

implementation

{$R *.dfm}

end.
