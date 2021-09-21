unit frmGenerateMapUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ZoneClasses, ClearTypeText, OZDMUnit,
  IAeverButton, Points3D;

type
  TMapItemType = (miLine,miPoint);
  TMapItemData = Record
   Case Integer Of
     0: (X1,Y1,X2,Y2: Integer; LenSquared,Angle: Single);
     1: (X,Y: Integer; Text: ShortString);
   End; // Case
  TMapItem = Class
    Color : TColor;
    Kind  : TMapItemType;
    Data  : TMapItemData;
  End;
  TfrmGenerateMap = class(TForm)
    Panel1: TPanel;
    Label1: TClearTypeLabel;
    Label2: TClearTypeLabel;
    edtLongName: TClearTypeEdit;
    edtShortName: TClearTypeEdit;
    btnCancel: TIAEverButton;
    btnOk: TIAEverButton;
    btnGenerate: TIAEverButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    ZDelta : T3DPoint;
    Procedure GenerateMap;
    Procedure RenderMap;
    Procedure ClearMap;
  public
    { Public declarations }
    Map    : TStringList;
    ZMin   : T3DPoint;
    ZMax   : T3DPoint;
    Procedure SaveMapToFile(FileName: String);
  end;

var
  frmGenerateMap: TfrmGenerateMap;

implementation

Uses frmMainUnit,Math,frmStatusUnit;

{$R *.dfm}

procedure TfrmGenerateMap.btnOkClick(Sender: TObject);
begin
  If (Trim(edtLongName.Text) = '') Or (Trim(edtShortName.Text) = '') Then
  Begin
    ShowMessage('You must enter a long_name and a short_name for the zone');
    ModalResult := mrNone;
  End;
end;

procedure TfrmGenerateMap.btnGenerateClick(Sender: TObject);
begin
  ClearMap;
  GenerateMap;
  RenderMap;
end;

Procedure TfrmGenerateMap.GenerateMap;
Var
  I,J,K     : Integer;
  L,M,N     : Integer;
  P,P1      : TPolygon;
  MO        : TMeshObject;
  MinPt     : T3DPoint;
  MaxPt     : T3DPoint;
  Delta     : T3DPoint;
  V,V1      : T3DPoint;
  Line      : TMapItem;
  AvgZ      : Array Of Single;
  X,Y       : Single;
  Item1     : TMapItem;
  Item2     : TMapItem;
  D1,D2     : Single;
  D,D3      : Single;
  A1,A2     : Single;
  X1,Y1     : Integer;
  X2,Y2     : Integer;
  L1,L2     : Single;
  MO1       : TMeshObject;
  MO2       : TMeshObject;
  V2,V3     : T3DPoint;
  Color     : TColor;
  Found     : Boolean;
  Segments1 : Array Of Boolean;
  Segments2 : Array Of Boolean;

Begin
  frmStatus.Show;
  frmStatus.SetCaption('Generating zone mesh');
  frmStatus.SetPosition(0);
  MO    := frmMain.Zone.BuildPolygonList(False,False,True,False);
  MinPt := T3DPoint.Create;
  MaxPt := T3DPoint.Create;
  Delta := T3DPoint.Create;
  frmStatus.SetCaption('Calculating zone bounds');
  MO.GetBounds(ZMin,ZMax);
  ZDelta.Copy(ZMax);
  ZDelta.Subtract(ZMin);
  frmStatus.SetCaption('Generating zone map...Pass 1: culling polygons');
  frmStatus.SetPosition(0);
  SetLength(AvgZ,0);
  I := 0;
  While I < MO.Polygons.Count Do
  Begin
    frmStatus.SetPosition(I / MO.Polygons.Count);
    P := TPolygon(MO.Polygons.Objects[I]);

    // Ignore transparent polygons so we don't include the zone bounds

    If P.TextureState <> tsTransparent Then
    Begin
      // Get the polygon's bounds

      For J := 0 To High(P.Vertices) Do
      Begin
        If J = 0 Then
        Begin
          MinPt.Copy(T3DPoint(MO.Vertices.Objects[P.Vertices[J]]));
          MaxPt.Copy(MinPt);
        End
        Else
        Begin
          V       := T3DPoint(MO.Vertices.Objects[P.Vertices[J]]);
          MinPt.X := Min(MinPt.X,V.X + MO.Loc.X);
          MinPt.Y := Min(MinPt.Y,V.Y + MO.Loc.Y);
          MinPt.Z := Min(MinPt.Z,V.Z + MO.Loc.Z);
          MaxPt.X := Max(MaxPt.X,V.X + MO.Loc.X);
          MaxPt.Y := Max(MaxPt.Y,V.Y + MO.Loc.Y);
          MaxPt.Z := Max(MaxPt.Z,V.Z + MO.Loc.Z);
        End;
      End; // For J

      Delta.Copy(MaxPt);
      Delta.Subtract(MinPt);
      If Abs(Delta.Z) > 0.75 * Min(Abs(Delta.X),Abs(Delta.Y)) Then
      Begin
        SetLength(AvgZ,High(AvgZ) + 2);
        AvgZ[High(AvgZ)] := (MinPt.Z + MaxPt.Z) / 2;
        Inc(I);
      End
      Else
      Begin
        P.Free;
        MO.Polygons.Delete(I);
      End;
    End
    Else
    Begin
      P.Free;
      MO.Polygons.Delete(I);
    End;
  End; // While

  // Pass 2

  frmStatus.SetCaption('Generating zone map...Pass 2: generating map lines');
  frmStatus.SetPosition(0);
  V  := T3DPoint.Create;
  V1 := T3DPoint.Create;
  For I := 0 To MO.Polygons.Count - 1 Do
  Begin
    frmStatus.SetPosition(I / MO.Polygons.Count);
    P := TPolygon(MO.Polygons.Objects[I]);

    // Now get the bounds again, but only at the polygon's average Z position

    K := 0;
    J := 0;
    While (J <= High(P.Vertices)) And (K < 2) Do
    Begin
      V.Copy(T3DPoint(MO.Vertices.Objects[P.Vertices[J]]));
      V1.Copy(T3DPoint(MO.Vertices.Objects[P.Vertices[(J + 1) Mod (High(P.Vertices) + 1)]]));
      V.Add(MO.Loc);
      V1.Add(MO.Loc);

      // We only care about this line segment if it either touches or straddles the average Z position

      If (V.Z - AvgZ[I]) * (V1.Z - AvgZ[I]) <= 0 Then
      Begin
        If V1.Z <> V.Z Then
        Begin
          X := V.X + (V1.X - V.X) * Abs(AvgZ[I] - V.Z) / Abs(V1.Z - V.Z);
          Y := V.Y + (V1.Y - V.Y) * Abs(AvgZ[I] - V.Z) / Abs(V1.Z - V.Z);
        End
        Else
        Begin
          X := V.X;
          Y := V.Y;
        End;
        If K = 0 Then
        Begin
          MinPt.X := X;
          MinPt.Y := Y;
          Inc(K);
        End
        Else
        Begin
          MaxPt.X := X;
          MaxPt.Y := Y;
          Inc(K);
        End;
      End;
      Inc(J);
    End; // While

    // Add a line from the polygon

    If K >= 2 Then
    Begin
      X1           := Round(MinPt.X);
      Y1           := Round(MinPt.Y);
      X2           := Round(MaxPt.X);
      Y2           := Round(MaxPt.Y);
      If (Abs(X2 - X1) > 0) Or (Abs(Y2 - Y1) > 0) Then
      Begin
        Line                 := TMapItem.Create;
        Line.Color           := clSilver;
        Line.Kind            := miLine;
        Line.Data.X1         := X1;
        Line.Data.Y1         := Y1;
        Line.Data.X2         := X2;
        Line.Data.Y2         := Y2;
        Line.Data.LenSquared := Sqr(X2 - X1) + Sqr(Y2 - Y1);
        Line.Data.Angle      := ArcTan2(Y2 - Y1,X2 - X1);
        Map.AddObject('',Line);
      End;
    End;
  End; // For I
  V.Free;
  V1.Free;

  frmStatus.SetCaption('Generating zone map...Pass 3: combining small lines');
  frmStatus.SetPosition(0);
  X := Sqr(Max(ZDelta.X,ZDelta.Y) / 100);
  I := 0;
  While I <= Map.Count - 2 Do
  Begin
    frmStatus.SetPosition(I / (Map.Count - 1));
    Item1 := TMapItem(Map.Objects[I]);
    L1    := Sqr(Item1.Data.X2 - Item1.Data.X1) + Sqr(Item1.Data.Y2 - Item1.Data.Y1);
    If L1 <= X Then
    Begin
      // Find a short line with the endpoint that's closest to (X2,Y2)

      K := -1;
      D := Sqr(Max(ZDelta.X,ZDelta.Y) * 1.1);
      For J := 0 To Map.Count - 1 Do
      Begin
        If J <> I Then
        Begin
          Item2 := TMapItem(Map.Objects[J]);
          If Item2.Data.LenSquared <= X Then
          Begin
            D1 := Sqr(Item2.Data.X1 - Item1.Data.X2) + Sqr(Item2.Data.Y1 - Item1.Data.Y2);
            D2 := Sqr(Item2.Data.X2 - Item1.Data.X2) + Sqr(Item2.Data.Y2 - Item1.Data.Y2);
            D3 := Min(D1,D2);
            If D3 < D Then
            Begin
              D := D3;
              K := J;
            End;
          End;
        End;
      End; // For J

      // If we found what we want, proceed

      If (K >= 0) And (D < X) Then
      Begin
        Item2 := TMapItem(Map.Objects[K]);
        D1    := Sqr(Item2.Data.X1 - Item1.Data.X2) + Sqr(Item2.Data.Y1 - Item1.Data.Y2);
        D2    := Sqr(Item2.Data.X2 - Item1.Data.X2) + Sqr(Item2.Data.Y2 - Item1.Data.Y2);
        If D1 < D2 Then
        Begin
          Item1.Data.X2 := Item2.Data.X2;
          Item1.Data.Y2 := Item2.Data.Y2;
        End
        Else
        Begin
          Item1.Data.X2 := Item2.Data.X1;
          Item1.Data.Y2 := Item2.Data.Y1;
        End;
        Item2.Free;
        Map.Delete(K);
        If K < I Then Dec(I);
      End;
    End;
    Inc(I);
  End; // While

  frmStatus.SetCaption('Generating zone map...Pass 4: combining colinear lines');
  frmStatus.SetPosition(0);
  X := 10 * Pi / 180;
  I := 0;
  While I <= Map.Count - 2 Do
  Begin
    frmStatus.SetPosition(I / (Map.Count - 1));
    Item1 := TMapItem(Map.Objects[I]);
    A1    := Item1.Data.Angle;

    // Find a line with the endpoint that's closest to (X2,Y2) and has a close angle

    K := Map.Count;
    D := Max(ZDelta.X,ZDelta.Y) * 1.1;
    For J := 0 To Map.Count - 1 Do
    Begin
      If J <> I Then
      Begin
        Item2 := TMapItem(Map.Objects[J]);
        A2    := Item2.Data.Angle;
        If Abs(A2 - A1) < X Then
        Begin
          D1 := Sqr(Item2.Data.X1 - Item1.Data.X2) + Sqr(Item2.Data.Y1 - Item1.Data.Y2);
          If D1 < D Then
          Begin
            D := D1;
            K := J + 1;
          End;
        End
        Else
        Begin
          A2 := A2 - Pi;
          If A2 < -Pi Then A2 := A2 + 2 * Pi;
          If Abs(A2 - A1) < X Then
          Begin
            D1 := Sqr(Item2.Data.X2 - Item1.Data.X2) + Sqr(Item2.Data.Y2 - Item1.Data.Y2);
            If D1 < D Then
            Begin
              D := D1;
              K := -(J + 1);
            End;
          End
        End;
      End;
    End; // For J

    // If we found what we want, proceed

    If (K <> Map.Count) And (D < X) Then
    Begin
      If K > 0 Then
      Begin
        Dec(K);
        Item2         := TMapItem(Map.Objects[K]);
        Item1.Data.X2 := Item2.Data.X2;
        Item1.Data.Y2 := Item2.Data.Y2;
        Item2.Free;
        Map.Delete(K);
        If K < I Then Dec(I);
      End
      Else
      Begin
        K := -K - 1;
        Item2         := TMapItem(Map.Objects[K]);
        Item1.Data.X2 := Item2.Data.X1;
        Item1.Data.Y2 := Item2.Data.Y1;
        Item2.Free;
        Map.Delete(K);
        If K < I Then Dec(I);
      End;
    End;

    Inc(I);
  End; // While

  MO1 := frmMain.Zone.FindMeshObject(meshHeightMapGround);
  MO2 := frmMain.Zone.FindMeshObject(meshHeightMapUnderwater);
  If (MO1 <> Nil) And (MO2 <> Nil) Then
  Begin
    frmStatus.SetCaption('Generating zone map...Pass 5: adding water/lava boundaries');
    frmStatus.SetPosition(0);

    // Find the first water/lava entry and use that for the whole zone

    I     := 0;
    Found := False;
    While (I <= High(frmMain.Zone.Water)) And Not Found Do
    Begin
      If frmMain.Zone.Water[I].WType In [wtWater,wtLava,wtIce,wtIceWater] Then Found := True Else Inc(I);
    End; // While
    If Found And (frmMain.Zone.Water[I].WType = wtLava) Then Color := clRed Else Color := clBlue;

    // Form lists of all line segments in the meshes

    J := 0;
    For I := 0 To MO1.Polygons.Count - 1 Do
    Begin
      P := TPolygon(MO1.Polygons.Objects[I]);
      Inc(J,High(P.Vertices) + 1);
    End; // For I
    SetLength(Segments1,J);
    For I := 0 To J - 1 Do Segments1[I] := False;

    J := 0;
    For I := 0 To MO2.Polygons.Count - 1 Do
    Begin
      P := TPolygon(MO2.Polygons.Objects[I]);
      Inc(J,High(P.Vertices) + 1);
    End; // For I
    SetLength(Segments2,J);
    For I := 0 To J - 1 Do Segments2[I] := False;

    M  := 0;
    V  := T3DPoint.Create;
    V1 := T3DPoint.Create;
    V2 := T3DPoint.Create;
    V3 := T3DPoint.Create;
    For I := 0 To MO1.Polygons.Count - 1 Do
    Begin
      frmStatus.SetPosition(I / MO1.Polygons.Count);
      P := TPolygon(MO1.Polygons.Objects[I]);
      For J := 0 To High(P.Vertices) Do
      Begin
        If Not Segments1[M] Then
        Begin
          V.Copy(T3DPoint(MO1.Vertices.Objects[P.Vertices[J]]));
          V1.Copy(T3DPoint(MO1.Vertices.Objects[P.Vertices[(J + 1) Mod (High(P.Vertices) + 1)]]));
          V.Add(MO1.Loc);
          V1.Add(MO1.Loc);
          N := 0;
          For K := 0 To MO2.Polygons.Count - 1 Do
          Begin
            P1 := TPolygon(MO2.Polygons.Objects[K]);
            For L := 0 To High(P1.Vertices) Do
            Begin
              If Not Segments2[N] Then
              Begin
                V2.Copy(T3DPoint(MO2.Vertices.Objects[P1.Vertices[L]]));
                V3.Copy(T3DPoint(MO2.Vertices.Objects[P1.Vertices[(L + 1) Mod (High(P1.Vertices) + 1)]]));
                V2.Add(MO2.Loc);
                V3.Add(MO2.Loc);
                If (V.Equals(V2) And V1.Equals(V3)) Or
                   (V.Equals(V3) And V1.Equals(V2)) Then
                Begin
                  Line                 := TMapItem.Create;
                  Line.Color           := Color;
                  Line.Kind            := miLine;
                  Line.Data.X1         := Round(V.X);
                  Line.Data.Y1         := Round(V.Y);
                  Line.Data.X2         := Round(V1.X);
                  Line.Data.Y2         := Round(V1.Y);
                  Line.Data.LenSquared := Sqr(Line.Data.X2 - Line.Data.X1) + Sqr(Line.Data.Y2 - Line.Data.Y1);
                  Line.Data.Angle      := ArcTan2(Line.Data.Y2 - Line.Data.Y1,Line.Data.X2 - Line.Data.X1);
                  Map.AddObject('',Line);
                  Segments1[M]         := True;
                  Segments2[N]         := True;
                End;
              End;
              Inc(N);
            End; // For L
          End; // For K
        End;
        Inc(M);
      End; // For J
    End; // For I
    V.Free;
    V1.Free;
    V2.Free;
    V3.Free;
    SetLength(Segments1,0);
    SetLength(Segments2,0);
  End;

  // Add lines for the zone bounds

  Line                 := TMapItem.Create;
  Line.Color           := clSilver;
  Line.Kind            := miLine;
  Line.Data.X1         := Round(ZMax.X);
  Line.Data.Y1         := Round(ZMax.Y);
  Line.Data.X2         := Round(ZMax.X);
  Line.Data.Y2         := Round(ZMin.Y);
  Line.Data.LenSquared := Sqr(Line.Data.X2 - Line.Data.X1) + Sqr(Line.Data.Y2 - Line.Data.Y1);
  Line.Data.Angle      := ArcTan2(Line.Data.Y2 - Line.Data.Y1,Line.Data.X2 - Line.Data.X1);
  Map.AddObject('',Line);

  Line                 := TMapItem.Create;
  Line.Color           := clSilver;
  Line.Kind            := miLine;
  Line.Data.X1         := Round(ZMax.X);
  Line.Data.Y1         := Round(ZMin.Y);
  Line.Data.X2         := Round(ZMin.X);
  Line.Data.Y2         := Round(ZMin.Y);
  Line.Data.LenSquared := Sqr(Line.Data.X2 - Line.Data.X1) + Sqr(Line.Data.Y2 - Line.Data.Y1);
  Line.Data.Angle      := ArcTan2(Line.Data.Y2 - Line.Data.Y1,Line.Data.X2 - Line.Data.X1);
  Map.AddObject('',Line);

  Line                 := TMapItem.Create;
  Line.Color           := clSilver;
  Line.Kind            := miLine;
  Line.Data.X1         := Round(ZMin.X);
  Line.Data.Y1         := Round(ZMin.Y);
  Line.Data.X2         := Round(ZMin.X);
  Line.Data.Y2         := Round(ZMax.Y);
  Line.Data.LenSquared := Sqr(Line.Data.X2 - Line.Data.X1) + Sqr(Line.Data.Y2 - Line.Data.Y1);
  Line.Data.Angle      := ArcTan2(Line.Data.Y2 - Line.Data.Y1,Line.Data.X2 - Line.Data.X1);
  Map.AddObject('',Line);

  Line                 := TMapItem.Create;
  Line.Color           := clSilver;
  Line.Kind            := miLine;
  Line.Data.X1         := Round(ZMin.X);
  Line.Data.Y1         := Round(ZMax.Y);
  Line.Data.X2         := Round(ZMax.X);
  Line.Data.Y2         := Round(ZMax.Y);
  Line.Data.LenSquared := Sqr(Line.Data.X2 - Line.Data.X1) + Sqr(Line.Data.Y2 - Line.Data.Y1);
  Line.Data.Angle      := ArcTan2(Line.Data.Y2 - Line.Data.Y1,Line.Data.X2 - Line.Data.X1);
  Map.AddObject('',Line);

  frmStatus.Hide;

  // Cleanup

  SetLength(AvgZ,0);
  MO.Free;
  MinPt.Free;
  MaxPt.Free;
  Delta.Free;
End; // TfrmGenerateMap.GenerateMap

Procedure TfrmGenerateMap.RenderMap;
Var
  I     : Integer;
  Item  : TMapItem;
  MaxX  : Integer;
  MaxY  : Integer;
  Scale : Single;
  X1,Y1 : Single;
  X2,Y2 : Single;
  X0,Y0 : Integer;

Begin
  MaxX               := ClientWidth;
  MaxY               := Panel1.Top;
  Canvas.Pen.Color   := clBlack;
  Canvas.Brush.Color := clBlack;
  Canvas.Rectangle(0,0,MaxX,MaxY);
  If (ZDelta.X > 0) And (ZDelta.Y > 0) Then
  Begin
    Scale := Min(MaxX / ZDelta.Y,MaxY / ZDelta.X);
    X0    := Round(ZDelta.X * Scale * 0.025);
    Y0    := Round(ZDelta.Y * Scale * 0.025);
    Scale := Scale * 0.95;
    Canvas.Pen.Width := 1;
    For I := 0 To Map.Count - 1 Do
    Begin
      Item              := TMapItem(Map.Objects[I]);
      Canvas.Pen.Color  := Item.Color;
      Canvas.Font.Color := Item.Color;
      Case Item.Kind Of
        miPoint:
        Begin
          X1 := ZMax.Y - Item.Data.Y;
          Y1 := ZMax.X - Item.Data.X;
          Canvas.Rectangle(X0 + Round(X1 * Scale),Y0 + Round(Y1 * Scale),X0 + Round(X1 * Scale) + 2,Y0 + Round(Y1 * Scale) + 2);
          Canvas.TextOut(X0 + Round(X1 * Scale) + 8,Y0 + Round(Y1 * Scale),Item.Data.Text);
        End;
        miLine:
        Begin
          X1 := ZMax.Y - Item.Data.Y1;
          Y1 := ZMax.X - Item.Data.X1;
          X2 := ZMax.Y - Item.Data.Y2;
          Y2 := ZMax.X - Item.Data.X2;
          Canvas.MoveTo(X0 + Round(X1 * Scale),Y0 + Round(Y1 * Scale));
          Canvas.LineTo(X0 + Round(X2 * Scale),Y0 + Round(Y2 * Scale));
        End;
      End; // Case
    End; // For I
  End;
End; // TfrmGenerateMap.RenderMap

Procedure TfrmGenerateMap.ClearMap;
Var I: Integer;
Begin
  For I := 0 To Map.Count - 1 Do Map.Objects[I].Free;
  Map.Clear;
End; // TfrmGenerateMap.ClearMap

procedure TfrmGenerateMap.FormCreate(Sender: TObject);
begin
  Map    := TStringList.Create;
  ZMin   := T3DPoint.Create;
  ZMax   := T3DPoint.Create;
  ZDelta := T3DPoint.Create;
end;

procedure TfrmGenerateMap.FormDestroy(Sender: TObject);
begin
  ClearMap;
  Map.Free;
  ZMin.Free;
  ZMax.Free;
  ZDelta.Free;
end;

procedure TfrmGenerateMap.FormPaint(Sender: TObject);
begin
  RenderMap;
end;

procedure TfrmGenerateMap.FormResize(Sender: TObject);
begin
  RenderMap;
end;

Procedure TfrmGenerateMap.SaveMapToFile(FileName: String);
Var
  F    : System.Text;
  I    : Integer;
  Item : TMapItem;
  St   : String;

Begin
  AssignFile(F,FileName);
  ReWrite(F);
  WriteLn(F,Trim(edtLongName.Text)    + ',' +
            Trim(edtShortName.Text)   + ',' +
            IntToStr(Round(ZDelta.Y)) + ',' +
            IntToStr(Round(ZDelta.X)));
  For I := 0 To Map.Count - 1 Do
  Begin
    Item := TMapItem(Map.Objects[I]);
         If Item.Color = clBlue Then St := 'blue'
    Else If Item.Color = clRed  Then St := 'red'
    Else St := 'gray';
    Case Item.Kind Of
      miPoint: WriteLn(F,'P,point_name,green,' + IntToStr(Round(Item.Data.Y)) + ',' + IntToStr(Round(Item.Data.X)));
      miLine:  WriteLn(F,'L,line_name,' + St + ',2,' + IntToStr(Round(Item.Data.Y1)) + ',' + IntToStr(Round(Item.Data.X1)) + ',' +
                                                       IntToStr(Round(Item.Data.Y2)) + ',' + IntToStr(Round(Item.Data.X2)));
    End; // Case
  End; // For I
  CloseFile(F);
End; // TfrmGenerateMap.SaveMapToFile

end.
