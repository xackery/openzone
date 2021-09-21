unit frmNewZoneUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, IAeverButton, ClearTypeText, OZDMUnit;

type
  TfrmNewZone = class(TForm)
    GroupBox1: TClearTypeGroupBox;
    GroupBox2: TClearTypeGroupBox;
    edtNorthSouthSize: TClearTypeEdit;
    edtEastWestSize: TClearTypeEdit;
    edtUpDownSize: TClearTypeEdit;
    GroupBox3: TClearTypeGroupBox;
    edtNorthSouthPosition: TClearTypeEdit;
    edtEastWestPosition: TClearTypeEdit;
    edtUpDownPosition: TClearTypeEdit;
    Panel1: TPanel;
    pnlBottom: TPanel;
    GroupBox4: TClearTypeGroupBox;
    cbTexture: TComboBox;
    GroupBox5: TClearTypeGroupBox;
    edtInset: TClearTypeEdit;
    edtDepth: TClearTypeEdit;
    lblTextureSet: TLabel;
    Panel2: TPanel;
    cbBelowTexture: TComboBox;
    btnOk: TIAEverButton;
    btnCancel: TIAEverButton;
    btnHelp: TIAEverButton;
    ClearTypeLabel1: TClearTypeLabel;
    ClearTypeLabel2: TClearTypeLabel;
    ClearTypeLabel3: TClearTypeLabel;
    ClearTypeLabel4: TClearTypeLabel;
    ClearTypeLabel5: TClearTypeLabel;
    ClearTypeLabel6: TClearTypeLabel;
    ClearTypeLabel7: TClearTypeLabel;
    ClearTypeLabel8: TClearTypeLabel;
    ClearTypeLabel9: TClearTypeLabel;
    ClearTypeLabel10: TClearTypeLabel;
    ClearTypeLabel11: TClearTypeLabel;
    rbOutside: TClearTypeRadioButton;
    rbMidair: TClearTypeRadioButton;
    cbUseHeightmap: TClearTypeCheckBox;
    cbMakeBoundingBox: TClearTypeCheckBox;
    btnChooseLandTexture: TIAEverButton;
    btnChooseUnderwaterTexture: TIAEverButton;
    procedure FormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure rbOutsideClick(Sender: TObject);
    procedure rbMidairClick(Sender: TObject);
    procedure btnChooseLandTextureClick(Sender: TObject);
    procedure btnChooseUnderwaterTextureClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmNewZone: TfrmNewZone;

implementation

Uses ZoneClasses,frmChooseTextureUnit;

{$R *.dfm}

procedure TfrmNewZone.FormShow(Sender: TObject);
Var
  I    : Integer;
  Tex  : String;
  BTex : String;

begin
  If TextureSet <> ''
   Then lblTextureSet.Caption := TextureSet
   Else lblTextureSet.Caption := '(none)';

  // Save the previous setting

  Tex  := cbTexture.Text;
  BTex := cbBelowTexture.Text;

  // Load the texture library, skipping the "default setting" entry

  cbTexture.Clear;
  cbBelowTexture.Clear;
  For I := 1 To TextureLibrary.Count - 1 Do
  Begin
    cbTexture.Items.Add(TextureLibrary.Strings[I]);
    cbBelowTexture.Items.Add(TextureLibrary.Strings[I]);
  End; // For I

  // Try to restore the previous setting

  I := cbTexture.Items.IndexOf(Tex);
  If I >= 0 Then cbTexture.ItemIndex := I;
  I := cbBelowTexture.Items.IndexOf(BTex);
  If I >= 0 Then cbBelowTexture.ItemIndex := I;
end;

procedure TfrmNewZone.btnHelpClick(Sender: TObject);
begin
  Application.HelpJump('Creating_a_new_zone_from_scratch');
end;

procedure TfrmNewZone.rbOutsideClick(Sender: TObject);
begin
  edtNorthSouthPosition.Enabled := True;
  edtEastWestPosition.Enabled   := True;
  edtUpDownPosition.Enabled     := True;
  edtNorthSouthSize.Enabled     := True;
  edtEastWestSize.Enabled       := True;
  edtUpDownSize.Enabled         := True;
  cbUseHeightmap.Enabled        := True;
end;

procedure TfrmNewZone.rbMidairClick(Sender: TObject);
begin
  edtNorthSouthPosition.Enabled := False;
  edtEastWestPosition.Enabled   := False;
  edtUpDownPosition.Enabled     := False;
  edtNorthSouthSize.Enabled     := False;
  edtEastWestSize.Enabled       := False;
  edtUpDownSize.Enabled         := False;
  cbUseHeightmap.Enabled        := False;
end;

procedure TfrmNewZone.btnChooseLandTextureClick(Sender: TObject);
Var I: Integer;
begin
  If frmTextureChooser.ShowModal = mrOk Then
  Begin
    If (frmTextureChooser.dgTextures.Row >= 0) And
       (frmTextureChooser.dgTextures.Col >= 0) Then
    Begin
      I := frmTextureChooser.dgTextures.Row * frmTextureChooser.dgTextures.ColCount + frmTextureChooser.dgTextures.Col;
      If I < TextureLibrary.Count Then cbTexture.ItemIndex := I;
    End;
  End;
end;

procedure TfrmNewZone.btnChooseUnderwaterTextureClick(Sender: TObject);
Var I: Integer;
begin
  If frmTextureChooser.ShowModal = mrOk Then
  Begin
    If (frmTextureChooser.dgTextures.Row >= 0) And
       (frmTextureChooser.dgTextures.Col >= 0) Then
    Begin
      I := frmTextureChooser.dgTextures.Row * frmTextureChooser.dgTextures.ColCount + frmTextureChooser.dgTextures.Col;
      If I < TextureLibrary.Count Then cbBelowTexture.ItemIndex := I;
    End;
  End;
end;

end.
