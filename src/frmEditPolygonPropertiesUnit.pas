unit frmEditPolygonPropertiesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, SharedComboBox, ExtCtrls, ZoneClasses,
  CPButton, Grids, ClearTypeText, OZDMUnit, IAeverButton;

type
  TfrmEditPolygonProperties = class(TForm)
    Label1: TClearTypeLabel;
    scbTexture: TSharedComboBox;
    btnAddTexture: TIAEverButton;
    btnDeleteTexture: TIAEverButton;
    btnOk: TIAEverButton;
    btnCancel: TIAEverButton;
    Panel1: TPanel;
    btnChooseTexture: TIAEverButton;
    Bevel1: TBevel;
    cbPassThrough: TClearTypeCheckBox;
    GroupBox1: TClearTypeGroupBox;
    rbSolid: TClearTypeRadioButton;
    rbSemitransparent: TClearTypeRadioButton;
    rbTransparent: TClearTypeRadioButton;
    cbHasColor: TClearTypeCheckBox;
    cbMasked: TClearTypeCheckBox;
    Bevel2: TBevel;
    cbGlobalChange: TClearTypeCheckBox;
    Label2: TClearTypeLabel;
    scbOpacityMap: TSharedComboBox;
    btnChooseOpacityMap: TIAEverButton;
    sgTextures: TClearTypeStringGrid;
    Label3: TClearTypeLabel;
    edtAnimationTime: TClearTypeEdit;
    cbColor: TColorBox;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure scbTextureChange(Sender: TObject);
    procedure btnAddTextureClick(Sender: TObject);
    procedure btnDeleteTextureClick(Sender: TObject);
    procedure btnChooseTextureClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbGlobalChangeClick(Sender: TObject);
    procedure btnChooseOpacityMapClick(Sender: TObject);
    procedure sgTexturesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormResize(Sender: TObject);
    procedure cbHasColorClick(Sender: TObject);
  private
    { Private declarations }
    Procedure SetColWidths;
  public
    { Public declarations }
    P : TPolygon;
    Function GetTexturesAsString: String;
  end;

var
  frmEditPolygonProperties: TfrmEditPolygonProperties;

implementation

Uses frmChooseTextureUnit,Math;

{$R *.dfm}

Procedure TfrmEditPolygonProperties.SetColWidths;
Begin
  sgTextures.ColWidths[0] := (ClientWidth - 8) Div 2;
  sgTextures.ColWidths[1] := (ClientWidth - 8) Div 2;
End; // TfrmEditPolygonProperties.SetColWidths

procedure TfrmEditPolygonProperties.FormShow(Sender: TObject);
Var
//  Tex        : TTokenArray;
//  Opc        : TTokenArray;
  I          : Integer;
  St         : String;
//  Textures   : String;
//  Opacities  : String;
//  Parameters : String;
  AnimTime   : Single;

begin
  cbGlobalChange.Checked := False;
  scbTexture.Items       := TextureLibrary;
  scbOpacityMap.Items    := TextureLibrary;
  St                     := P.TextureInfo.FirstTexture;//P.Texture;
  If Copy(St,1,1) = '(' Then
  Begin
    St       := '';
    AnimTime := DefaultAnimTimePerFrame;
  End
  Else
  Begin
//    BreakupTextureString(St,Textures,Opacities,Parameters);
//    GetTokens(';',Textures,Tex);
//    GetTokens(';',Opacities,Opc);
//    Val(Trim(Parameters),AnimTime,I);
//    If I <> 0 Then AnimTime := DefaultAnimTimePerFrame * (High(Tex) + 1);
    AnimTime := P.TextureInfo.AnimTime;
    If AnimTime = 0 Then AnimTime := DefaultAnimTimePerFrame * P.TextureInfo.TextureMaps.Count;//(High(Tex) + 1);
    If AnimTime < MinimumAnimTimePerFrame * P.TextureInfo.TextureMaps.Count{(High(Tex) + 1)} Then AnimTime := MinimumAnimTimePerFrame * P.TextureInfo.TextureMaps.Count;//(High(Tex) + 1);
    If AnimTime > 100 Then AnimTime := 100;
    sgTextures.RowCount := Max(2,P.TextureInfo.TextureMaps.Count + 1{High(Tex) + 2});
    sgTextures.Cells[0,0] := 'Texture';
    sgTextures.Cells[1,0] := 'Opacity map';
    sgTextures.Cells[0,1] := '';
    sgTextures.Cells[1,1] := '';
    SetColWidths;
    For I := 0 To P.TextureInfo.TextureMaps.Count - 1 Do//High(Tex) Do
    Begin
      sgTextures.Cells[0,I + 1] := P.TextureInfo.TextureMaps.Strings[I];
      sgTextures.Cells[1,I + 1] := P.TextureInfo.OpacityMaps.Strings[I];
{
      sgTextures.Cells[0,I + 1] := Tex[I];
      If I <= High(Opc)
       Then sgTextures.Cells[1,I + 1] := Opc[I]
       Else sgTextures.Cells[1,I + 1] := '';
}
    End; // For I
//    SetLength(Tex,0);
//    SetLength(Opc,0);
  End;

  // Try to select the textures in the first row

  scbTexture.ItemIndex    := Max(0,scbTexture.Items.IndexOf(sgTextures.Cells[0,1]));
  scbOpacityMap.ItemIndex := Max(0,scbOpacityMap.Items.IndexOf(sgTextures.Cells[1,1]));

  btnAddTexture.Enabled     := (scbTexture.ItemIndex > 0);
  btnDeleteTexture.Enabled  := (sgTextures.Cells[0,1] <> '');
  edtAnimationTime.Text     := Trim(Format('%8.3f',[AnimTime]));
  cbPassThrough.Checked     := Not (P.Solid And P.HasSolid);
  rbSolid.Checked           := (P.TextureState = tsSolid);
  rbSemitransparent.Checked := (P.TextureState = tsSemitransparent);
  rbTransparent.Checked     := (P.TextureState = tsTransparent);
  cbMasked.Checked          := P.Masked And P.HasMasked;
  cbHasColor.Checked        := P.HasColor;
  cbColor.Enabled           := P.HasColor;
  edtAnimationTime.Text     := Trim(Format('%8.3f',[P.AnimTime]));
  If P.HasColor And (High(P.Colors) >= 0) Then cbColor.Selected := P.Colors[0] And $FFFFFF;
end;

procedure TfrmEditPolygonProperties.FormDestroy(Sender: TObject);
begin
  scbTexture.Items    := Nil;
  scbOpacityMap.Items := Nil;
end;

procedure TfrmEditPolygonProperties.scbTextureChange(Sender: TObject);
begin
  btnAddTexture.Enabled := (scbTexture.ItemIndex > 0);
end;

procedure TfrmEditPolygonProperties.btnAddTextureClick(Sender: TObject);
Var I,J: Integer;
begin
  If sgTextures.Cells[0,1] <> '' Then
  Begin
    // We have to back up and restore the item indexes of the drop-downs because changing the number of
    // rows in the string grid will change the drop-downs

    I := scbTexture.ItemIndex;
    J := scbOpacityMap.ItemIndex;
    sgTextures.RowCount := sgTextures.RowCount + 1;
    scbTexture.ItemIndex    := I;
    scbOpacityMap.ItemIndex := J;
  End;
  sgTextures.Cells[0,sgTextures.RowCount - 1] := scbTexture.Text;
  If scbOpacityMap.ItemIndex > 0
   Then sgTextures.Cells[1,sgTextures.RowCount - 1] := scbOpacityMap.Text
   Else sgTextures.Cells[1,sgTextures.RowCount - 1] := '';
  btnDeleteTexture.Enabled := True;
end;

procedure TfrmEditPolygonProperties.btnDeleteTextureClick(Sender: TObject);
begin
  If sgTextures.RowCount > 2 Then sgTextures.RowCount := sgTextures.RowCount - 1
  Else
  Begin
    sgTextures.Cells[0,1] := '';
    sgTextures.Cells[1,1] := '';
  End;
  btnDeleteTexture.Enabled  := (sgTextures.Cells[0,1] <> '');
end;

procedure TfrmEditPolygonProperties.btnChooseTextureClick(Sender: TObject);
Var I: Integer;
begin
  If frmTextureChooser.ShowModal = mrOk Then
  Begin
    If (frmTextureChooser.dgTextures.Row >= 0) And
       (frmTextureChooser.dgTextures.Col >= 0) Then
    Begin
      I := frmTextureChooser.dgTextures.Row * frmTextureChooser.dgTextures.ColCount + frmTextureChooser.dgTextures.Col;
      If I < TextureLibrary.Count - 1 Then scbTexture.ItemIndex := I + 1;
      btnAddTexture.Enabled := (scbTexture.ItemIndex > 0);
      btnDeleteTexture.Enabled  := (sgTextures.Cells[0,1] <> '');
    End;
  End;
end;

procedure TfrmEditPolygonProperties.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  If (ModalResult = mrOk) And (sgTextures.Cells[0,1] = '') Then
  Begin
    CanClose := (Application.MessageBox('You have specified no textures for this polygon!'#13#10 +
                                        'Set properties anyway?',
                                        'Warning',
                                        MB_OKCANCEL) = IDOK);
  End
  Else CanClose := True;
end;

procedure TfrmEditPolygonProperties.cbGlobalChangeClick(Sender: TObject);
begin
  If cbGlobalChange.Checked Then Application.MessageBox('Setting this will change ALL polygons in your zone'#13#10 +
                                                        'that match this polygon''s texture and were imported'#13#10 +
                                                        'from external sources.',
                                                        'Warning');
end;

procedure TfrmEditPolygonProperties.btnChooseOpacityMapClick(Sender: TObject);
Var I: Integer;
begin
  If frmTextureChooser.ShowModal = mrOk Then
  Begin
    If (frmTextureChooser.dgTextures.Row >= 0) And
       (frmTextureChooser.dgTextures.Col >= 0) Then
    Begin
      I := frmTextureChooser.dgTextures.Row * frmTextureChooser.dgTextures.ColCount + frmTextureChooser.dgTextures.Col;
      If I < TextureLibrary.Count - 1 Then scbOpacityMap.ItemIndex := I + 1;
    End;
  End;
end;

procedure TfrmEditPolygonProperties.sgTexturesSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  If (ARow > 0) And (sgTextures.Cells[0,ARow] <> '') Then
  Begin
    scbTexture.ItemIndex    := Max(0,scbTexture.Items.IndexOf(sgTextures.Cells[0,ARow]));
    scbOpacityMap.ItemIndex := Max(0,scbOpacityMap.Items.IndexOf(sgTextures.Cells[1,ARow]));
  End;
end;

procedure TfrmEditPolygonProperties.FormResize(Sender: TObject);
begin
  SetColWidths;
end;

Function TfrmEditPolygonProperties.GetTexturesAsString: String;
Var
  St       : String;
  St1      : String;
  I,J      : Integer;
  Found    : Boolean;
  AnimTime : Single;

Begin
  // Assemble the textures

  St := '';
  J  := 0; // Count the number of textures added
  For I := 1 To sgTextures.RowCount - 1 Do
  Begin
    If St <> '' Then St := St + ';';
    St := St + sgTextures.Cells[0,I];
    If sgTextures.Cells[0,I] <> '' Then Inc(J);
  End; // For I

  // See if any opacity maps have been set

  I     := 1;
  Found := False;
  While (I < sgTextures.RowCount) And Not Found Do
  Begin
    If sgTextures.Cells[1,I] <> ''
     Then Found := True
     Else Inc(I);
  End; // While

  // Assemble the opacity maps, if any, and add them to the texture list

  If Found Then
  Begin
    St1 := '';
    For I := 1 To sgTextures.RowCount - 1 Do
    Begin
      If I <> 1 Then St1 := St1 + ';';
      St1 := St1 + sgTextures.Cells[1,I];
    End; // For I
    St := St + '|' + St1;
  End;
  If J < 2 Then AnimTime := 0
  Else
  Begin
    Val(Trim(edtAnimationTime.Text),AnimTime,I);
    If I <> 0 Then AnimTime := DefaultAnimTimePerFrame * J;
    If AnimTime = 0
     Then AnimTime := DefaultAnimTimePerFrame * J
     Else AnimTime := Max(MinimumAnimTimePerFrame * J,AnimTime);
    If AnimTime > 100 Then AnimTime := 100;
  End;
  St := St + '+' + Trim(Format('%8.3f',[AnimTime]));
  Result := St;
End; // TfrmEditPolygonProperties.GetTexturesAsString

procedure TfrmEditPolygonProperties.cbHasColorClick(Sender: TObject);
begin
  cbColor.Enabled := cbHasColor.Checked;
end;

end.
