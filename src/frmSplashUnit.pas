unit frmSplashUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ClearTypeText, OZDMUnit,
  IAeverButton;

type
  TfrmSplash = class(TForm)
    Bevel1: TBevel;
    Image1: TImage;
    lblVersion: TClearTypeLabel;
    btnOk: TIAEverButton;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSplash: TfrmSplash;

implementation

Uses frmMainUnit;

{$R *.dfm}

procedure TfrmSplash.FormShow(Sender: TObject);
begin
  lblVersion.Caption := 'Version ' + Version;
end;

end.
