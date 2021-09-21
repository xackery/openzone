unit frmXWFExportOptionsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ClearTypeText, OZDMUnit, IAeverButton;

type
  TfrmXWFExportOptions = class(TForm)
    GroupBox1: TClearTypeGroupBox;
    rbBSPTree: TClearTypeRadioButton;
    rbOctree: TClearTypeRadioButton;
    btnOk: TIAEverButton;
    btnCancel: TIAEverButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmXWFExportOptions: TfrmXWFExportOptions;

implementation

{$R *.dfm}

end.
