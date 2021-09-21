unit frmWaterPropertiesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, SharedComboBox, CPButton, Grids, frmMainUnit,
  ZoneClasses, ClearTypeText, OZDMUnit, IAeverButton;

type
  TfrmWaterProperties = class(TForm)
    lblWaterLevel: TClearTypeLabel;
    edtWaterLevel: TClearTypeEdit;
    btnCancel: TIAEverButton;
    btnOk: TIAEverButton;
    lblWaterTex: TClearTypeLabel;
    scbWaterTex: TSharedComboBox;
    cbSemiTransparent: TClearTypeCheckBox;
    gbBasicStats: TClearTypeGroupBox;
    rbWater: TClearTypeRadioButton;
    rbLava: TClearTypeRadioButton;
    gbSpecialFeatures: TClearTypeGroupBox;
    cbTinted: TClearTypeCheckBox;
    btnColor: TColorPickButton;
    lblAlpha: TClearTypeLabel;
    edtAlpha: TClearTypeEdit;
    lblTint: TClearTypeLabel;
    lblColor: TClearTypeLabel;
    gbWaterExtent: TClearTypeGroupBox;
    lblSouth: TClearTypeLabel;
    lblEast: TClearTypeLabel;
    lblNorthSouth: TClearTypeLabel;
    lblEastWest: TClearTypeLabel;
    edtSouth: TClearTypeEdit;
    edtEast: TClearTypeEdit;
    edtNorthSouth: TClearTypeEdit;
    edtEastWest: TClearTypeEdit;
    gbTextures: TClearTypeGroupBox;
    pnlTop: TPanel;
    sgWater: TClearTypeStringGrid;
    btnAdd: TIAEverButton;
    btnDelete: TIAEverButton;
    cbHasDepth: TClearTypeCheckBox;
    edtWaterDepth: TClearTypeEdit;
    rbRectangular: TClearTypeRadioButton;
    rbElliptical: TClearTypeRadioButton;
    lblHint: TClearTypeLabel;
    gbGroundTextures: TClearTypeGroupBox;
    Label1: TClearTypeLabel;
    scbLandTex: TSharedComboBox;
    Label2: TClearTypeLabel;
    scbUnderwaterTex: TSharedComboBox;
    rbPvP: TClearTypeRadioButton;
    btnAddWaterTex: TIAEverButton;
    btnDeleteWaterTex: TIAEverButton;
    lbTextures: TClearTypeListBox;
    btnChooseTexture: TIAEverButton;
    btnChooseLandTexture: TIAEverButton;
    btnChooseUnderwaterTexture: TIAEverButton;
    btnHelp: TIAEverButton;
    rbIrregular: TClearTypeRadioButton;
    pnlClient: TPanel;
    Label3: TClearTypeLabel;
    edtAnimationTime: TClearTypeEdit;
    rbIce: TClearTypeRadioButton;
    rbIceWater: TClearTypeRadioButton;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure rbWaterClick(Sender: TObject);
    procedure rbLavaClick(Sender: TObject);
    procedure edtWaterLevelChange(Sender: TObject);
    procedure edtSouthChange(Sender: TObject);
    procedure edtEastChange(Sender: TObject);
    procedure edtNorthSouthChange(Sender: TObject);
    procedure edtEastWestChange(Sender: TObject);
    procedure cbSemiTransparentClick(Sender: TObject);
    procedure cbTintedClick(Sender: TObject);
    procedure edtAlphaChange(Sender: TObject);
    procedure btnColorChangeColor(sender: TObject; color: TColor);
    procedure btnDeleteClick(Sender: TObject);
    procedure sgWaterSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure cbHasDepthClick(Sender: TObject);
    procedure edtWaterDepthChange(Sender: TObject);
    procedure rbRectangularClick(Sender: TObject);
    procedure rbEllipticalClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rbPvPClick(Sender: TObject);
    procedure btnAddWaterTexClick(Sender: TObject);
    procedure btnDeleteWaterTexClick(Sender: TObject);
    procedure lbTexturesClick(Sender: TObject);
    procedure btnChooseTextureClick(Sender: TObject);
    procedure btnChooseLandTextureClick(Sender: TObject);
    procedure btnChooseUnderwaterTextureClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure edtAnimationTimeChange(Sender: TObject);
    procedure rbIceClick(Sender: TObject);
    procedure rbIceWaterClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Water    : Array Of TWater;
    Selected : Integer;
    Procedure LoadGridFromWaterRec;
    Procedure LoadGridRow(Row: Integer);
    Procedure LoadDataFields;
    Procedure EnableFields;
    Function  GetSingle(St: String; Min,Max,Default: Single): Single;
    Function  GetInteger(St: String; Min,Max,Default: Integer): Integer;
    Procedure LoadWaterList(Index: Integer);
  end;

var
  frmWaterProperties: TfrmWaterProperties;

Implementation

Uses frmChooseTextureUnit,Math;

{$R *.dfm}

procedure TfrmWaterProperties.FormShow(Sender: TObject);
Var I: Integer;
begin
  lblHint.Caption        := 'Hint: elliptical water areas have ' + IntToStr(EllipticalSections) + ' sections.';
  scbWaterTex.Items      := TextureLibrary;
  scbLandTex.Items       := TextureLibrary;
  scbUnderwaterTex.Items := TextureLibrary;
  SetLength(Water,High(frmMain.Zone.Water) + 1);
  For I := 0 To High(Water) Do Water[I] := frmMain.Zone.Water[I];
  If High(Water) >= 0
   Then Selected := 0
   Else Selected := -1;
  scbLandTex.ItemIndex       := TextureLibrary.IndexOf(GetFirstTexture(frmMain.Zone.GetDefaultLandTexture));
  scbUnderwaterTex.ItemIndex := TextureLibrary.IndexOf(GetFirstTexture(frmMain.Zone.GetDefaultUnderwaterTexture));
  LoadGridFromWaterRec;
  LoadDataFields;
  EnableFields;
end;

procedure TfrmWaterProperties.FormDestroy(Sender: TObject);
begin
  scbWaterTex.Items      := Nil;
  scbLandTex.Items       := Nil;
  scbUnderwaterTex.Items := Nil;
end;

Procedure TfrmWaterProperties.LoadGridRow(Row: Integer);
Begin
  If (Row >= 0) And (Row <= High(Water)) Then
  Begin
    sgWater.Cells[0,Row + 1] := FloatToStr(Water[Row].Level);
    sgWater.Cells[1,Row + 1] := BoolToStr(Water[Row].SemiTrans,True);
    Case Water[Row].WType Of
      wtWater: sgWater.Cells[2,Row + 1] := 'Water';
       wtLava: sgWater.Cells[2,Row + 1] := 'Lava';
        wtPvP: sgWater.Cells[2,Row + 1] := 'PvP';
        wtIce: sgWater.Cells[2,Row + 1] := 'Ice';
   wtIceWater: sgWater.Cells[2,Row + 1] := 'Ice water';
    End; // Case
    sgWater.Cells[3,Row + 1] := BoolToStr(Water[Row].Tinted,True);
    sgWater.Cells[4,Row + 1] := '$' + IntToHex(Integer(Water[Row].Color),8);
    sgWater.Cells[5,Row + 1] := FloatToStr(Water[Row].MinX);
    sgWater.Cells[6,Row + 1] := FloatToStr(Water[Row].MinY);
    sgWater.Cells[7,Row + 1] := FloatToStr(Water[Row].XSize);
    sgWater.Cells[8,Row + 1] := FloatToStr(Water[Row].YSize);
    If Water[Row].HasDepth
     Then sgWater.Cells[9,Row + 1] := FloatToStr(Water[Row].Depth)
     Else sgWater.Cells[9,Row + 1] := 'N/A';
    Case Water[Row].Shape Of
      wsRectangular: sgWater.Cells[10,Row + 1] := 'Rectangular';
      wsElliptical:  sgWater.Cells[10,Row + 1] := 'Elliptical';
      wsIrregular:   sgWater.Cells[10,Row + 1] := 'Irregular';
    End; // Case
    EnableFields;
  End;
End; // TfrmWaterProperties.LoadGridRow;

Procedure TfrmWaterProperties.LoadGridFromWaterRec;
Var I,J: Integer;
Begin
  // Clear everything

  For I := 1 To sgWater.ColCount - 1 Do
   For J := 1 To sgWater.RowCount - 1 Do sgWater.Cells[I,J] := '';

  // Set grid rows and columns

  sgWater.Cells[0,0]  := 'Level';
  sgWater.Cells[1,0]  := 'Semitransparent';
  sgWater.Cells[2,0]  := 'Type';
  sgWater.Cells[3,0]  := 'Tinted';
  sgWater.Cells[4,0]  := 'Color';
  sgWater.Cells[5,0]  := 'South';
  sgWater.Cells[6,0]  := 'East';
  sgWater.Cells[7,0]  := 'N-S size';
  sgWater.Cells[8,0]  := 'E-W size';
  sgWater.Cells[9,0]  := 'Depth';
  sgWater.Cells[10,0] := 'Shape';

  // Load data

  sgWater.RowCount := 2;
  If High(Water) < 0 Then sgWater.RowCount := 2 Else
  Begin
    sgWater.RowCount := High(Water) + 2;
    For I := 0 To High(Water) Do LoadGridRow(I);
  End;
  If Selected >= 0 Then sgWater.Row := Selected + 1 Else sgWater.Row := 1;
End; // TfrmWaterProperties.LoadGridFromWaterRec

Procedure TfrmWaterProperties.LoadDataFields;
Var St: String;

  Procedure Select(SCB: TSharedComboBox; Var Tex: String; Default: String);
  Var
    I,J   : Integer;
    Found : Boolean;

  Begin
    I := SCB.Items.IndexOf(Tex);
    If I < 0 Then
    Begin
      J       := 0;
      Found   := False;
      Default := UpperCase(Default);
      While (J < SCB.Items.Count) And Not Found Do
      Begin
        If UpperCase(SCB.Items.Strings[J]) = Default Then Found := True Else Inc(J);
      End; // While
      If Found Then I := J;
    End;
    SCB.ItemIndex := I;
    If I >= 0 Then Tex := SCB.Text;
  End; // Select

  Procedure SelectFromList(SCB: TSharedComboBox; Var Tex: String; List: Array Of String);
  Var I: Integer;
  Begin
    I := 0;
    If I < High(List) Then
    Begin
      Repeat
        Select(SCB,Tex,List[I]);
        Inc(I);
      Until (Tex <> '') Or (I > High(List));
    End;
  End; // SelectFromList

Begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    If TextureSet <> ''
     Then St := TextureSet + '\'
     Else St := '';

    LoadWaterList(Selected);
    
    edtWaterLevel.Text    := FloatToStr(Water[Selected].Level);
    rbWater.Checked       := (Water[Selected].WType = wtWater);
    rbLava.Checked        := (Water[Selected].WType = wtLava);
    rbPvP.Checked         := (Water[Selected].WType = wtPvP);
    rbIce.Checked         := (Water[Selected].WType = wtIce);
    rbIceWater.Checked    := (Water[Selected].WType = wtIceWater);
    cbTinted.Checked      := Water[Selected].Tinted;
    btnColor.setcolor(TColor(Integer(Water[Selected].Color) And $00FFFFFF));
    edtAlpha.Text         := IntToStr(255 - TBGRA(Water[Selected].Color).A);
    edtSouth.Text         := FloatToStr(Water[Selected].MinX);
    edtEast.Text          := FloatToStr(Water[Selected].MinY);
    edtNorthSouth.Text    := FloatToStr(Water[Selected].XSize);
    edtEastWest.Text      := FloatToStr(Water[Selected].YSize);
    cbHasDepth.Checked    := Water[Selected].HasDepth;
    edtWaterDepth.Text    := FloatToStr(Water[Selected].Depth);
    rbRectangular.Checked := (Water[Selected].Shape = wsRectangular);
    rbElliptical.Checked  := (Water[Selected].Shape = wsElliptical);
    rbIrregular.Checked   := (Water[Selected].Shape = wsIrregular);
    edtAnimationTime.Text := Trim(Format('%8.3f',[Water[Selected].AnimTime]));

    cbSemitransparent.Checked := Water[Selected].SemiTrans;
  End;
  EnableFields;
End; // TfrmWaterProperties.LoadDataFields

Procedure TfrmWaterProperties.EnableFields;
Var B,B1: Boolean;
Begin
  B  := (High(Water) >= 0) And (Selected >= 0) And (Selected <= High(Water));
  B1 := Not rbPvP.Checked;
  lblWaterLevel.Enabled     := B;
  lblWaterTex.Enabled       := B And B1;
  edtWaterLevel.Enabled     := B;
  scbWaterTex.Enabled       := B And B1;
  cbSemiTransparent.Enabled := B And B1;
  rbWater.Enabled           := B;
  rbLava.Enabled            := B;
  rbPvP.Enabled             := B;
  rbIce.Enabled             := B;
  rbIceWater.Enabled        := B;
  cbTinted.Enabled          := B And B1;
  btnColor.Enabled          := B And B1;
  edtAlpha.Enabled          := B And B1;
  lblColor.Enabled          := B And B1;
  lblAlpha.Enabled          := B And B1;
  lblTint.Enabled           := B;
  lblSouth.Enabled          := B;
  lblEast.Enabled           := B;
  lblNorthSouth.Enabled     := B;
  lblEastWest.Enabled       := B;
  sgWater.Enabled           := B;
  btnDelete.Enabled         := B;
  cbHasDepth.Enabled        := B;
  rbRectangular.Enabled     := B And (Water[Selected].Shape <> wsIrregular);
  rbElliptical.Enabled      := B And (Water[Selected].Shape <> wsIrregular);
  rbIrregular.Enabled       := B And (Water[Selected].Shape =  wsIrregular);
  edtSouth.Enabled          := B And (Water[Selected].Shape <> wsIrregular);
  edtEast.Enabled           := B And (Water[Selected].Shape <> wsIrregular);
  edtNorthSouth.Enabled     := B And (Water[Selected].Shape <> wsIrregular);
  edtEastWest.Enabled       := B And (Water[Selected].Shape <> wsIrregular);
  edtWaterDepth.Enabled     := B And cbHasDepth.Checked;
End; // TfrmWaterProperties.EnableFields

procedure TfrmWaterProperties.btnAddClick(Sender: TObject);
begin
  SetLength(Water,High(Water) + 2);
  Water[High(Water)].Level      := 0;
  Water[High(Water)].SemiTrans  := False;
  Water[High(Water)].WType      := wtWater;
  Water[High(Water)].Tinted     := False;
  Water[High(Water)].Color      := clBlack;
  SetLength(Water[High(Water)].Tex,0);
  Water[High(Water)].MinX       := frmMain.Zone.ElevationGrid.MinX;
  Water[High(Water)].MinY       := frmMain.Zone.ElevationGrid.MinY;
  Water[High(Water)].XSize      := Abs(frmMain.Zone.ElevationGrid.MaxX - frmMain.Zone.ElevationGrid.MinX);
  Water[High(Water)].YSize      := Abs(frmMain.Zone.ElevationGrid.MaxY - frmMain.Zone.ElevationGrid.MinY);
  Water[High(Water)].HasDepth   := False;
  Water[High(Water)].Depth      := 0;
  Water[High(Water)].Shape      := wsRectangular;
  Water[High(Water)].AnimTime   := 0;
  Selected := High(Water);
  LoadGridFromWaterRec;
  LoadDataFields;
end;

Function TfrmWaterProperties.GetSingle(St: String; Min,Max,Default: Single): Single;
Var
  S : Single;
  I : Integer;
  
Begin
  Val(Trim(St),S,I);
  If (I = 0) And (S >= Min) And (S <= Max) Then Result := S Else Result := Default;
End; // TfrmWaterProperties.GetSingle

Function TfrmWaterProperties.GetInteger(St: String; Min,Max,Default: Integer): Integer;
Var I,J: Integer;
Begin
  Val(Trim(St),I,J);
  If (J = 0) And (I >= Min) And (I <= Max) Then Result := I Else Result := Default;
End; // TfrmWaterProperties.GetInteger

procedure TfrmWaterProperties.rbWaterClick(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].WType := wtWater;
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.rbLavaClick(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].WType := wtLava;
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.edtWaterLevelChange(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].Level := GetSingle(edtWaterLevel.Text,-32768,32767,Water[Selected].Level);
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.edtSouthChange(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].MinX := GetSingle(edtSouth.Text,-32768,32767,Water[Selected].MinX);
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.edtEastChange(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].MinY := GetSingle(edtEast.Text,-32768,32767,Water[Selected].MinY);
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.edtNorthSouthChange(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].XSize := GetSingle(edtNorthSouth.Text,-32768,32767,Water[Selected].XSize);
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.edtEastWestChange(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].YSize := GetSingle(edtEastWest.Text,-32768,32767,Water[Selected].YSize);
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.cbSemiTransparentClick(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].SemiTrans := cbSemiTransparent.Checked;
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.cbTintedClick(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].Tinted := cbTinted.Checked;
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.edtAlphaChange(Sender: TObject);
Var I: Integer;
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    I := GetInteger(edtAlpha.Text,0,255,255 - TBGRA(Water[Selected].Color).A);
    TBGRA(Water[Selected].Color).A := 255 - I;
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.btnColorChangeColor(sender: TObject;
  color: TColor);
Var C: TColor;
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    C := btnColor.color;
    TBGRA(Water[Selected].Color).R := TBGRA(C).R;
    TBGRA(Water[Selected].Color).G := TBGRA(C).G;
    TBGRA(Water[Selected].Color).B := TBGRA(C).B;
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.btnDeleteClick(Sender: TObject);
Var I: Integer;
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    If Application.MessageBox('This entry will be permanently deteted. Are you sure?',
                              'Warning',MB_OKCANCEL) = IDOK Then
    Begin
      For I := Selected To High(Water) - 1 Do Water[I] := Water[I + 1];
      SetLength(Water,High(Water));
      If Selected > High(Water) Then Dec(Selected);
      LoadGridFromWaterRec;
      LoadDataFields;
    End;
  End;
end;

procedure TfrmWaterProperties.sgWaterSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  Selected := sgWater.Row - 1;
  If High(Water) < 0 Then Selected := -1;
  LoadDataFields;
end;

procedure TfrmWaterProperties.cbHasDepthClick(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].HasDepth := cbHasDepth.Checked;
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.edtWaterDepthChange(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].Depth := GetSingle(edtWaterDepth.Text,0,32767,Water[Selected].Depth);
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.rbRectangularClick(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].Shape := wsRectangular;
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.rbEllipticalClick(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].Shape := wsElliptical;
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.FormClose(Sender: TObject;
  var Action: TCloseAction);
Var
  I     : Integer;
  Error : Boolean;

begin
  If ModalResult = mrOk Then
  Begin
    I     := 0;
    Error := False;
    While (I <= High(Water)) And Not Error Do
    Begin
      If Water[I].XSize < 0 Then
      Begin
        ShowMessage('Error in item ' + IntToStr(I + 1) + ': north-south size must be positive.');
        Error := True;
      End
      Else If Water[I].XSize = 0 Then
      Begin
        ShowMessage('Error in item ' + IntToStr(I + 1) + ': north-south size must not be zero.');
        Error := True;
      End
      Else If Water[I].YSize < 0 Then
      Begin
        ShowMessage('Error in item ' + IntToStr(I + 1) + ': east-west size must be positive.');
        Error := True;
      End
      Else If Water[I].YSize = 0 Then
      Begin
        ShowMessage('Error in item ' + IntToStr(I + 1) + ': east-west size must not be zero.');
        Error := True;
      End;
      Inc(I);
    End; // While
    If Error Then Action := caNone;
  End;
end;

procedure TfrmWaterProperties.rbPvPClick(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].WType := wtPvP;
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.btnAddWaterTexClick(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) And (scbWaterTex.Text <> '') Then
  Begin
    SetLength(Water[Selected].Tex,High(Water[Selected].Tex) + 2);
    Water[Selected].Tex[High(Water[Selected].Tex)] := scbWaterTex.Text;
    LoadGridRow(Selected);
    LoadWaterList(Selected);
    lbTextures.ItemIndex := -1;
    btnDeleteWaterTex.Enabled := False;
    If High(Water[Selected].Tex) > 0 Then
    Begin
      If Water[Selected].AnimTime = 0
       Then Water[Selected].AnimTime := (High(Water[Selected].Tex) + 1) * DefaultAnimTimePerFrame
       Else Water[Selected].AnimTime := Max((High(Water[Selected].Tex) + 1) * DefaultAnimTimePerFrame,Water[Selected].AnimTime);
    End;
  End;
end;

Procedure TfrmWaterProperties.LoadWaterList(Index: Integer);
Var I: Integer;
Begin
  lbTextures.Items.Clear;
  For I := 0 To High(Water[Index].Tex) Do lbTextures.Items.Add(Water[Index].Tex[I]);
  lbTextures.ItemIndex := -1;
  btnDeleteWaterTex.Enabled := False;
End; // TfrmWaterProperties.LoadWaterList

procedure TfrmWaterProperties.btnDeleteWaterTexClick(Sender: TObject);
Var I: Integer;
begin
  If (Selected >= 0) And
     (Selected <= High(Water)) And
     (High(Water[Selected].Tex) >= 0) And
     (lbTextures.ItemIndex >= 0) Then
  Begin
    For I := lbTextures.ItemIndex To High(Water[Selected].Tex) - 1 Do Water[Selected].Tex[I] := Water[Selected].Tex[I + 1];
    SetLength(Water[Selected].Tex,High(Water[Selected].Tex));
    LoadGridRow(Selected);
    LoadWaterList(Selected);
    lbTextures.ItemIndex := -1;
    btnDeleteWaterTex.Enabled := False;
  End;
end;

procedure TfrmWaterProperties.lbTexturesClick(Sender: TObject);
begin
  btnDeleteWaterTex.Enabled := (lbTextures.ItemIndex >= 0);
end;

procedure TfrmWaterProperties.btnChooseTextureClick(Sender: TObject);
Var I: Integer;
begin
  If frmTextureChooser.ShowModal = mrOk Then
  Begin
    If (frmTextureChooser.dgTextures.Row >= 0) And
       (frmTextureChooser.dgTextures.Col >= 0) Then
    Begin
      I := frmTextureChooser.dgTextures.Row * frmTextureChooser.dgTextures.ColCount + frmTextureChooser.dgTextures.Col;
      If I < TextureLibrary.Count - 1 Then scbWaterTex.ItemIndex := I + 1;
    End;
  End;
end;

procedure TfrmWaterProperties.btnChooseLandTextureClick(Sender: TObject);
Var I: Integer;
begin
  If frmTextureChooser.ShowModal = mrOk Then
  Begin
    If (frmTextureChooser.dgTextures.Row >= 0) And
       (frmTextureChooser.dgTextures.Col >= 0) Then
    Begin
      I := frmTextureChooser.dgTextures.Row * frmTextureChooser.dgTextures.ColCount + frmTextureChooser.dgTextures.Col;
      If I < TextureLibrary.Count - 1 Then scbLandTex.ItemIndex := I + 1;
    End;
  End;
end;

procedure TfrmWaterProperties.btnChooseUnderwaterTextureClick(
  Sender: TObject);
Var I: Integer;
begin
  If frmTextureChooser.ShowModal = mrOk Then
  Begin
    If (frmTextureChooser.dgTextures.Row >= 0) And
       (frmTextureChooser.dgTextures.Col >= 0) Then
    Begin
      I := frmTextureChooser.dgTextures.Row * frmTextureChooser.dgTextures.ColCount + frmTextureChooser.dgTextures.Col;
      If I < TextureLibrary.Count - 1 Then scbUnderwaterTex.ItemIndex := I + 1;
    End;
  End;
end;

procedure TfrmWaterProperties.btnHelpClick(Sender: TObject);
begin
  Application.HelpJump('Adding_water,_lava_or_PvP_areas');
end;

procedure TfrmWaterProperties.edtAnimationTimeChange(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    If High(Water[Selected].Tex) > 0
     Then Water[Selected].AnimTime := GetSingle(edtAnimationTime.Text,MinimumAnimTimePerFrame * (High(Water[Selected].Tex) + 1),100,Water[Selected].AnimTime)
     Else Water[Selected].AnimTime := GetSingle(edtAnimationTime.Text,0,100,Water[Selected].AnimTime);
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.rbIceClick(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].WType := wtIce;
    LoadGridRow(Selected);
  End;
end;

procedure TfrmWaterProperties.rbIceWaterClick(Sender: TObject);
begin
  If (Selected >= 0) And (Selected <= High(Water)) Then
  Begin
    Water[Selected].WType := wtIceWater;
    LoadGridRow(Selected);
  End;
end;

end.
