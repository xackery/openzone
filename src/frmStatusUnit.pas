unit frmStatusUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ClearTypeText, OZDMUnit;

type
  TfrmStatus = class(TForm)
    pbStatus: TProgressBar;
    lblStatus: TClearTypeLabel;
    Panel1: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure SetCaption(St: String);
    Procedure SetPosition(Value: Single);
  end;

var
  frmStatus: TfrmStatus;

implementation

{$R *.dfm}

Procedure TfrmStatus.SetCaption(St: String);
Begin
  lblStatus.Caption := St;
  pbStatus.Position := 0;
  Application.ProcessMessages;
End; // TfrmStatus.SetCaption

Procedure TfrmStatus.SetPosition(Value: Single);
// Value should range from 0 to 1
Var I: Integer;
Begin
  I := pbStatus.Min + Round(Value * (pbStatus.Max - pbStatus.Min));
  If I <> pbStatus.Position Then
  Begin
    pbStatus.Position := I;
    Application.ProcessMessages;
  End;
End; // TfrmStatus.SetPosition

end.
