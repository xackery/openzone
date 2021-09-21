unit frmPreferencesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ShellCtrls, ExtCtrls, ClearTypeText,
  OZDMUnit, IAeverButton, FontComboBox;

type
  TfrmPreferences = class(TForm)
    Label1: TClearTypeLabel;
    btnCancel: TIAEverButton;
    btnOk: TIAEverButton;
    ShellTreeView: TShellTreeView;
    Bevel1: TBevel;
    cbUseNumericKeypad: TClearTypeCheckBox;
    cbShowZoneDimensions: TClearTypeCheckBox;
    cbClearType: TClearTypeCheckBox;
    cbClearTypeMethod: TComboBox;
    Label2: TClearTypeLabel;
    gbClearTypeSim: TClearTypeGroupBox;
    ClearTypeLabel1: TClearTypeLabel;
    ClearTypeLabel2: TClearTypeLabel;
    fcbInactive: TFontComboBox;
    fcbActive: TFontComboBox;
    ClearTypeLabel3: TClearTypeLabel;
    ClearTypeLabel4: TClearTypeLabel;
    cbInactiveSize: TComboBox;
    cbActiveSize: TComboBox;
    ClearTypeGroupBox1: TClearTypeGroupBox;
    ClearTypeLabel5: TClearTypeLabel;
    ClearTypeLabel6: TClearTypeLabel;
    edtWLDSize: TClearTypeEdit;
    edtXWFSize: TClearTypeEdit;
    ClearTypeGroupBox2: TClearTypeGroupBox;
    ClearTypeLabel7: TClearTypeLabel;
    ClearTypeLabel8: TClearTypeLabel;
    edtMoveAmount: TClearTypeEdit;
    edtRotateAmount: TClearTypeEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPreferences: TfrmPreferences;

implementation

{$R *.dfm}

end.
