unit frmTranslateUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ClearTypeText, OZDMUnit, IAeverButton;

type
  TfrmTranslate = class(TForm)
    Label1: TClearTypeLabel;
    Label2: TClearTypeLabel;
    Label3: TClearTypeLabel;
    edtTranslateX: TClearTypeEdit;
    edtTranslateY: TClearTypeEdit;
    edtTranslateZ: TClearTypeEdit;
    btnCancel: TIAEverButton;
    btnOk: TIAEverButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTranslate: TfrmTranslate;

implementation

{$R *.dfm}

end.
