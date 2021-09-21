unit frmEditTextureListUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, SharedComboBox, ExtCtrls, Grids,
  ClearTypeText, OZDMUnit, IAeverButton;

type
  TfrmEditTextureList = class(TForm)
    Label1: TClearTypeLabel;
    scbTexture: TSharedComboBox;
    btnAddTexture: TIAEverButton;
    btnDeleteTexture: TIAEverButton;
    btnOk: TIAEverButton;
    btnCancel: TIAEverButton;
    Panel1: TPanel;
    btnChooseTexture: TIAEverButton;
    Bevel1: TBevel;
    Label2: TClearTypeLabel;
    edtAnimationTime: TClearTypeEdit;
    sgTextures: TClearTypeStringGrid;
    Label3: TClearTypeLabel;
    scbOpacityMap: TSharedComboBox;
    btnChooseOpacityMap: TIAEverButton;
    ClearTypeLabel1: TClearTypeLabel;
    scbNormalMap: TSharedComboBox;
    btnChooseNormalMap: TIAEverButton;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure scbTextureChange(Sender: TObject);
    procedure btnAddTextureClick(Sender: TObject);
    procedure btnDeleteTextureClick(Sender: TObject);
    procedure btnChooseTextureClick(Sender: TObject);
    procedure btnChooseOpacityMapClick(Sender: TObject);
    procedure sgTexturesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormResize(Sender: TObject);
    procedure btnChooseNormalMapClick(Sender: TObject);
  private
    { Private declarations }
    Procedure SetColWidths;
  public
    { Public declarations }
    Texture : String;
    Function GetTexturesAsString: String;
  end;

var
  frmEditTextureList: TfrmEditTextureList;

implementation

Uses ZoneClasses, frmChooseTextureUnit, Math;

{$R *.dfm}

Procedure TfrmEditTextureList.SetColWidths;
Begin
  sgTextures.ColWidths[0] := (ClientWidth - 8) Div 3;
  sgTextures.ColWidths[1] := (ClientWidth - 8) Div 3;
  sgTextures.ColWidths[2] := (ClientWidth - 8) Div 3;
End; // TfrmEditTextureList.SetColWidths

procedure TfrmEditTextureList.FormShow(Sender: TObject);
Var
//  Tex         : TTokenArray;
//  Opc         : TTokenArray;
  I           : Integer;
  St          : String;
//  Textures    : String;
//  Opacities   : String;
//  Parameters  : String;
  AnimTime    : Single;
  TextureInfo : TTextureInfo;

begin
  scbTexture.Items       := TextureLibrary;
  scbOpacityMap.Items    := TextureLibrary;
  scbNormalMap.Items     := TextureLibrary;
  TextureInfo            := TTextureInfo.Create(Texture);
  St                     := TextureInfo.FirstTexture;//Texture;
  If Copy(St,1,1) = '(' Then
  Begin
    St       := '';
    AnimTime := 0;
  End;

//  BreakupTextureString(St,Textures,Opacities,Parameters);
//  GetTokens(';',Textures,Tex);
//  GetTokens(';',Opacities,Opc);
//  Val(Trim(Parameters),AnimTime,I);
//  If I <> 0 Then AnimTime := DefaultAnimTimePerFrame * (High(Tex) + 1);
  If AnimTime = 0 Then AnimTime := DefaultAnimTimePerFrame * TextureInfo.TextureMaps.Count;//(High(Tex) + 1);
  If AnimTime < MinimumAnimTimePerFrame * TextureInfo.TextureMaps.Count{(High(Tex) + 1)} Then AnimTime := MinimumAnimTimePerFrame * TextureInfo.TextureMaps.Count;//(High(Tex) + 1);
  If AnimTime > 100 Then AnimTime := 100;
  sgTextures.RowCount := Max(2,TextureInfo.TextureMaps.Count + 1{High(Tex) + 2});
  sgTextures.Cells[0,0] := 'Texture';
  sgTextures.Cells[1,0] := 'Opacity map';
  sgTextures.Cells[2,0] := 'Normal map';
  sgTextures.Cells[0,1] := '';
  sgTextures.Cells[1,1] := '';
  sgTextures.Cells[2,1] := '';
  SetColWidths;
  For I := 0 To TextureInfo.TextureMaps.Count - 1 Do//High(Tex) Do
  Begin
    sgTextures.Cells[0,I + 1] := TextureInfo.TextureMaps.Strings[I];
    sgTextures.Cells[1,I + 1] := TextureInfo.OpacityMaps.Strings[I];
    sgTextures.Cells[2,I + 1] := TextureInfo.NormalMaps.Strings[I];
{
    sgTextures.Cells[0,I + 1] := Tex[I];
    If I <= High(Opc)
     Then sgTextures.Cells[1,I + 1] := Opc[I]
     Else sgTextures.Cells[1,I + 1] := '';
}
  End; // For I
//  SetLength(Tex,0);
//  SetLength(Opc,0);
  TextureInfo.Free;

  // Try to select the textures in the first row

  scbTexture.ItemIndex    := Max(0,scbTexture.Items.IndexOf(sgTextures.Cells[0,1]));
  scbOpacityMap.ItemIndex := Max(0,scbOpacityMap.Items.IndexOf(sgTextures.Cells[1,1]));
  scbNormalMap.ItemIndex  := Max(0,scbNormalMap.Items.IndexOf(sgTextures.Cells[2,1]));

  btnAddTexture.Enabled     := (scbTexture.ItemIndex > 0);
  btnDeleteTexture.Enabled  := (sgTextures.Cells[0,1] <> '');
  edtAnimationTime.Text     := Trim(Format('%8.3f',[AnimTime]));
end;

procedure TfrmEditTextureList.FormDestroy(Sender: TObject);
begin
  scbTexture.Items    := Nil;
  scbOpacityMap.Items := Nil;
  scbNormalMap.Items  := Nil;
end;

procedure TfrmEditTextureList.scbTextureChange(Sender: TObject);
begin
  btnAddTexture.Enabled := (scbTexture.ItemIndex > 0);
end;

procedure TfrmEditTextureList.btnAddTextureClick(Sender: TObject);
Var I,J,K: Integer;
begin
  If sgTextures.Cells[0,1] <> '' Then
  Begin
    // We have to back up and restore the item indexes of the drop-downs because changing the number of
    // rows in the string grid will change the drop-downs

    I := scbTexture.ItemIndex;
    J := scbOpacityMap.ItemIndex;
    K := scbNormalMap.ItemIndex;
    sgTextures.RowCount := sgTextures.RowCount + 1;
    scbTexture.ItemIndex    := I;
    scbOpacityMap.ItemIndex := J;
    scbNormalMap.ItemIndex  := K;
  End;
  sgTextures.Cells[0,sgTextures.RowCount - 1] := scbTexture.Text;
  If scbOpacityMap.ItemIndex > 0
   Then sgTextures.Cells[1,sgTextures.RowCount - 1] := scbOpacityMap.Text
   Else sgTextures.Cells[1,sgTextures.RowCount - 1] := '';
  If scbNormalMap.ItemIndex > 0
   Then sgTextures.Cells[2,sgTextures.RowCount - 1] := scbNormalMap.Text
   Else sgTextures.Cells[2,sgTextures.RowCount - 1] := '';
  btnDeleteTexture.Enabled := True;
end;

procedure TfrmEditTextureList.btnDeleteTextureClick(Sender: TObject);
begin
  If sgTextures.RowCount > 2 Then sgTextures.RowCount := sgTextures.RowCount - 1
  Else
  Begin
    sgTextures.Cells[0,1] := '';
    sgTextures.Cells[1,1] := '';
    sgTextures.Cells[2,1] := '';
  End;
  btnDeleteTexture.Enabled  := (sgTextures.Cells[0,1] <> '');
end;

procedure TfrmEditTextureList.btnChooseTextureClick(Sender: TObject);
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

procedure TfrmEditTextureList.btnChooseOpacityMapClick(Sender: TObject);
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

procedure TfrmEditTextureList.sgTexturesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  If (ARow > 0) And (sgTextures.Cells[0,ARow] <> '') Then
  Begin
    scbTexture.ItemIndex    := Max(0,scbTexture.Items.IndexOf(sgTextures.Cells[0,ARow]));
    scbOpacityMap.ItemIndex := Max(0,scbOpacityMap.Items.IndexOf(sgTextures.Cells[1,ARow]));
  End;
end;

procedure TfrmEditTextureList.FormResize(Sender: TObject);
begin
  SetColWidths;
end;

Function TfrmEditTextureList.GetTexturesAsString: String;
Var
{  St       : String;
  St1      : String;
  I,J      : Integer;
  Found    : Boolean;
}
  I,J         : Integer;
  AnimTime    : Single;
  TextureInfo : TTextureInfo;

Begin
  TextureInfo := TTextureInfo.Create('');
  J := 0; // Count the number of textures added
  For I := 1 To sgTextures.RowCount - 1 Do
  Begin
    If sgTextures.Cells[0,I] <> '' Then
    Begin
      TextureInfo.TextureMaps.Add(sgTextures.Cells[0,I]);
      TextureInfo.OpacityMaps.Add(sgTextures.Cells[1,I]);
      TextureInfo.NormalMaps.Add(sgTextures.Cells[2,I]);
      Inc(J);
    End;
  End; // For I
  Val(Trim(edtAnimationTime.Text),AnimTime,I);
  If I <> 0 Then AnimTime := DefaultAnimTimePerFrame * J;
  If AnimTime = 0
   Then AnimTime := DefaultAnimTimePerFrame * J
   Else AnimTime := Max(MinimumAnimTimePerFrame * J,AnimTime);
  If AnimTime > 100 Then AnimTime := 100;
  TextureInfo.AnimTime := AnimTime;
  Result := TextureInfo.ToString;
  TextureInfo.Free;

{
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
}
End; // TfrmEditTextureList.GetTexturesAsString

procedure TfrmEditTextureList.btnChooseNormalMapClick(Sender: TObject);
Var I: Integer;
begin
  If frmTextureChooser.ShowModal = mrOk Then
  Begin
    If (frmTextureChooser.dgTextures.Row >= 0) And
       (frmTextureChooser.dgTextures.Col >= 0) Then
    Begin
      I := frmTextureChooser.dgTextures.Row * frmTextureChooser.dgTextures.ColCount + frmTextureChooser.dgTextures.Col;
      If I < TextureLibrary.Count - 1 Then scbNormalMap.ItemIndex := I + 1;
    End;
  End;
end;

end.
