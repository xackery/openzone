program ClearTypeTest;

uses
  Forms,
  frmMainUnit in 'frmMainUnit.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
