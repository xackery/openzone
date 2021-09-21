unit frmSelectHotspotUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ClearTypeText, OZDMUnit, IAeverButton;

type
  TfrmSelectHotSpot = class(TForm)
    Label1: TClearTypeLabel;
    lblObjectName: TClearTypeLabel;
    lbHotspots: TClearTypeListBox;
    Label2: TClearTypeLabel;
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
  frmSelectHotSpot: TfrmSelectHotSpot;

implementation

{$R *.dfm}

procedure TfrmSelectHotSpot.btnHelpClick(Sender: TObject);
begin
  Application.HelpJump('Adding_object_placement_hotspots');
end;

end.
