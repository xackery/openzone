unit frmAddZoneExitUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, OZDMUnit, ClearTypeText,
  IAeverButton;

type
  TfrmAddZoneExit = class(TForm)
    imgLeft: TImage;
    imgRight: TImage;
    rbLeft: TClearTypeRadioButton;
    rbRight: TClearTypeRadioButton;
    btnCancel: TIAEverButton;
    btnOk: TIAEverButton;
    procedure imgLeftClick(Sender: TObject);
    procedure imgRightClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAddZoneExit: TfrmAddZoneExit;

implementation

{$R *.dfm}

procedure TfrmAddZoneExit.imgLeftClick(Sender: TObject);
begin
  rbLeft.Checked  := True;
  rbRight.Checked := False;
end;

procedure TfrmAddZoneExit.imgRightClick(Sender: TObject);
begin
  rbLeft.Checked  := False;
  rbRight.Checked := True;
end;

end.
