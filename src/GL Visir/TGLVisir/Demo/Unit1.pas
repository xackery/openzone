unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GLVisir, Math;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    SingleBtn: TButton;
    MultiBtn: TButton;
    GLVisir1: TGLVisir;
    btnSphere: TButton;
    lblWater: TLabel;
    lblLand: TLabel;
    procedure SingleBtnClick(Sender: TObject);
    procedure MultiBtnClick(Sender: TObject);
    procedure btnSphereClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.SingleBtnClick(Sender: TObject);
var
  Mdl : TModelRec; // model
  I   : Integer;

begin
  lblWater.Caption := '';
  lblLand.Caption  := '';

  //--- prepare record with model (pyramide) ---
  mdl.nv := 5; // 5 vertexes
  SetLength(mdl.v,mdl.nv); // allocate memory for vertexes
  mdl.np := 5; // 5 Faces (polygons)
  SetLength(mdl.p,mdl.np); // allocate memory for polygons
  //--- fill record ---
  //--- pyramide's vertexes ---
  mdl.v[0].x := -1; mdl.v[0].y := -1; mdl.v[0].z := 0;
  mdl.v[1].x := -1; mdl.v[1].y := 1; mdl.v[1].z := 0;
  mdl.v[2].x := 1; mdl.v[2].y := 1; mdl.v[2].z := 0;
  mdl.v[3].x := 1; mdl.v[3].y := -1; mdl.v[3].z := 0;
  mdl.v[4].x := 0; mdl.v[4].y := 0; mdl.v[4].z := 1;
  For I := 0 To 4 Do
  Begin
    Mdl.V[I].R        := 255;
    Mdl.V[I].G        := 0;
    Mdl.V[I].B        := 0;
    Mdl.V[I].A        := 255;
    Mdl.V[I].UseColor := True;
  End; // For I
  //--- Pyramide faces ---
  // base polygon
  mdl.p[0].numVertexes := 4;// mdl.p[0].col := clRed;
  mdl.p[0].n[0] := 0; mdl.p[0].n[1] := 1; mdl.p[0].n[2] := 2; mdl.p[0].n[3] := 3;
  //--- side polygons
  mdl.p[1].numVertexes := 3;// mdl.p[1].col := clRed;
  mdl.p[1].n[0] := 0; mdl.p[1].n[1] := 1; mdl.p[1].n[2] := 4;
  mdl.p[2].numVertexes := 3;// mdl.p[2].col := clRed;
  mdl.p[2].n[0] := 1; mdl.p[2].n[1] := 2; mdl.p[2].n[2] := 4;
  mdl.p[3].numVertexes := 3;// mdl.p[3].col := clRed;
  mdl.p[3].n[0] := 2; mdl.p[3].n[1] := 3; mdl.p[3].n[2] := 4;
  mdl.p[4].numVertexes := 3;// mdl.p[4].col := clRed;
  mdl.p[4].n[0] := 3; mdl.p[4].n[1] := 0; mdl.p[4].n[2] := 4;
  For I := 0 To 4 Do
  Begin
    Mdl.P[I].UseColor := False;
    Mdl.P[I].TextureID := -1;
  End; // For I

  // remove existing frames (1st time - not necessary)
  GLVisir1.Clear;
  // add model to new frame
  GLVisir1.AddModelToNewFrame(mdl,True);
  // center and zoom added models
  GLVisir1.Fit(false, // don't move coordinates system to zero,
               false  // keep proportions
               );
  GLVisir1.Scene3D.Scene.GetEntity(0).Visible := True;
  GLVisir1.Scene3D.Redraw;             
end;


procedure TForm1.MultiBtnClick(Sender: TObject);
var
  mdl : TModelRec; // animation frames
  I,J : integer;

begin
  lblWater.Caption := '';
  lblLand.Caption  := '';

  // remove existing frames (1st time - not necessary)
  GLVisir1.Clear;

  // 10 frames
  For I := 0 To 9 Do
  Begin
    //--- prepare record with model (pyramide) ---
    mdl.nv := 5; // 5 points - vertexes
    SetLength(mdl.v,mdl.nv); // allocate memory for vertexes
    mdl.np := 5; // 5 Faces - polygons
    SetLength(mdl.p,mdl.np); // allocate memory for polygons
    //--- fill record ---
    //--- pyramide vertexes ---
    mdl.v[0].x := -1; mdl.v[0].y := -1; mdl.v[0].z := 0;
    mdl.v[1].x := -1; mdl.v[1].y := 1; mdl.v[1].z := 0;
    mdl.v[2].x := 1; mdl.v[2].y := 1; mdl.v[2].z := 0;
    mdl.v[3].x := 1; mdl.v[3].y := -1; mdl.v[3].z := 0;
    mdl.v[4].x := 0; mdl.v[4].y := 0; mdl.v[4].z := i;
    For J := 0 To 4 Do
    Begin
      Mdl.V[J].R        := 255;
      Mdl.V[J].G        := 0;
      Mdl.V[J].B        := 0;
      Mdl.V[J].A        := 255;
      Mdl.V[J].UseColor := True;
    End; // For J

    //--- pyramide polygons ---
    // base polygon
    mdl.p[0].numVertexes := 4; mdl.p[0].col := clRed;
    mdl.p[0].n[0] := 0; mdl.p[0].n[1] := 1; mdl.p[0].n[2] := 2; mdl.p[0].n[3] := 3;
    //--- side polygons
    mdl.p[1].numVertexes := 3; mdl.p[1].col := clRed;
    mdl.p[1].n[0] := 0; mdl.p[1].n[1] := 1; mdl.p[1].n[2] := 4;
    mdl.p[2].numVertexes := 3; mdl.p[2].col := clRed;
    mdl.p[2].n[0] := 1; mdl.p[2].n[1] := 2; mdl.p[2].n[2] := 4;
    mdl.p[3].numVertexes := 3; mdl.p[3].col := clRed;
    mdl.p[3].n[0] := 2; mdl.p[3].n[1] := 3; mdl.p[3].n[2] := 4;
    mdl.p[4].numVertexes := 3; mdl.p[4].col := clRed;
    mdl.p[4].n[0] := 3; mdl.p[4].n[1] := 0; mdl.p[4].n[2] := 4;
    For J := 0 To 4 Do
    Begin
      Mdl.P[J].UseColor := False;
      Mdl.P[J].TextureID := -1;
    End; // For J
    // add model to new frame
    GLVisir1.AddModelToNewFrame(mdl,True);

   //--- prepare record with another model (pyramide) ---
    mdl.nv := 5; // 5 vertexes
    SetLength(mdl.v,mdl.nv); // allocate memory
    mdl.np := 5; // 5 Faces
    SetLength(mdl.p,mdl.np); // allocate memory
   //--- fill record ---
   //--- pyramide vertexes ---
    mdl.v[0].x := -i; mdl.v[0].y := -i; mdl.v[0].z := 0;
    mdl.v[1].x := -i; mdl.v[1].y := i; mdl.v[1].z := 0;
    mdl.v[2].x := i; mdl.v[2].y := i; mdl.v[2].z := 0;
    mdl.v[3].x := i; mdl.v[3].y := -i; mdl.v[3].z := 0;
    mdl.v[4].x := 0; mdl.v[4].y := 0; mdl.v[4].z := -i;
    For J := 0 To 4 Do
    Begin
      Mdl.V[J].R        := 255;
      Mdl.V[J].G        := 0;
      Mdl.V[J].B        := 0;
      Mdl.V[J].A        := 255;
      Mdl.V[J].UseColor := True;
    End; // For J
    //--- pyramide polygons ---
    // base polygon
    mdl.p[0].numVertexes := 4; mdl.p[0].col := clYellow;
    mdl.p[0].n[0] := 0; mdl.p[0].n[1] := 1; mdl.p[0].n[2] := 2; mdl.p[0].n[3] := 3;
    //--- side polygons
    mdl.p[1].numVertexes := 3; mdl.p[1].col := clYellow;
    mdl.p[1].n[0] := 0; mdl.p[1].n[1] := 1; mdl.p[1].n[2] := 4;
    mdl.p[2].numVertexes := 3; mdl.p[2].col := clYellow;
    mdl.p[2].n[0] := 1; mdl.p[2].n[1] := 2; mdl.p[2].n[2] := 4;
    mdl.p[3].numVertexes := 3; mdl.p[3].col := clYellow;
    mdl.p[3].n[0] := 2; mdl.p[3].n[1] := 3; mdl.p[3].n[2] := 4;
    mdl.p[4].numVertexes := 3; mdl.p[4].col := clYellow;
    mdl.p[4].n[0] := 3; mdl.p[4].n[1] := 0; mdl.p[4].n[2] := 4;
    For J := 0 To 4 Do
    Begin
      Mdl.P[J].UseColor := False;
      Mdl.P[J].TextureID := -1;
    End; // For J
    // add model to the same frame
    GLVisir1.AddModelToFrame(mdl,i,True);
  end;

  // center and zoom added models
  GLVisir1.Fit(false, // don't move coordinates system to zero,
               false  // keep proportions
               );

  For I := 0 To 9 Do GLVisir1.Scene3D.Scene.GetEntity(I).Visible := True;
  GLVisir1.Scene3D.Redraw;
end;

procedure TForm1.btnSphereClick(Sender: TObject);
Const
  NXZ = 128;
  NXY = 2 * NXZ;

  NPY = 256;
  NPX = NPY * 2;

  Pal : Array[0..383] Of Byte =
  (  0,0,0,   48,48,48,  1,0,43,   1,3,43,   2,5,44,   2,7,44,   3,9,45,   4,11,46,
     5,13,47,  6,15,48,  7,17,49,  8,19,50,  9,21,51, 10,22,52, 11,24,52, 12,26,54,
    13,28,54, 14,30,56, 15,32,56, 16,34,58, 17,34,58, 17,36,58, 18,38,60, 19,40,60,
    20,42,62, 21,44,62,
    
   10,31,0,
   11,31,0,11,31,1,11,32,1,12,32,1,12,32,2,12,33,2,13,33,2,14,33,3,15,33,3,15,
   34,3,15,34,4,15,35,4,16,35,4,16,35,5,16,36,5,17,36,5,17,36,6,18,37,6,18,38,
   7,19,38,8,20,39,8,20,40,9,21,40,10,22,41,10,22,42,11,23,42,12,24,43,12,24,
   44,13,25,44,14,25,45,14,26,46,15,27,46,16,27,47,17,28,47,18,28,48,19,29,49,
   19,30,49,20,30,50,21,31,51,21,32,51,22,32,52,23,33,53,23,34,53,24,34,54,25,
   35,55,25,36,55,26,36,56,27,37,57,27,38,57,27,39,57,27,41,57,27,42,57,27,43,
   57,27,44,57,27,45,57,27,46,57,27,47,57,27,49,57,27,50,57,27,51,57,27,52,57,
   27,53,57,27,55,57,27,56,57,27,57,57,27,58,57,27,58,57,26,58,57,25,58,57,24,
   58,56,23,58,55,22,58,54,20,58,53,19,58,51,18,58,50,17,58,50,16,58,49,15,58,
   48,14,58,47,13,58,46,12,58,45,11,58,44,11,58,44,10,58,43,10,58,42,9,57,41,
   8,57,40,8,56,39,7,56,38,6,55,37,5,55,35,4,54,33,4,54,31,2,32,32,32,63,63,63,
   63,63,63,63,63,63,63,63,63,48,48,48,63,63,63,63,63,63);

Type
  TRGBA = Packed Record
    R,G,B,A: Byte;
  End;

Var
  mdl    : TModelRec;
//  v      : array [0..500] of VectorType;
//  p      : array [0..1000] of PolygonType;
  v      : Array Of VectorType;
  p      : Array Of PolygonType;
  i,j,k,l : Integer;
  nv,np  : Integer;
  r      : Double;
  i1,j1  : Double;
  j2     : Integer;
  i3,j3  : Integer;
  si,ci  : Double;
  Mp     : Packed Array Of Byte;
  Colors : Array[0..255] Of TColor;
  RGBA   : TRGBA;

  Temp   : Packed Array[0..NPX] Of Byte;
  Last   : Byte;
  Next   : Byte;
  J0     : Integer;

  WA : Double;

  cx,cy  : Integer;
  v1,v2,v3,v4 : VectorType;
  x,y,z       : Double;
  a1,a2       : Double;

  Function NCol(Mc,N: Integer; DvdWater, DvdLand, DvdMountains: Single): Integer;
  Var
    Loc : Integer;
    R   : Single;

  Begin
    R  := (Mc + N - Random(2 * N)) / DvdWater;

    If R > 51 Then R := R * DvdWater / DvdLand;

    If R > 201 Then R := R * DvdLand / DvdMountains;


    Loc  := Trunc(R);
    NCol := Loc;
    If Loc > 250 Then NCol := 250;
    If Loc < 5   Then NCol := 5;
  End; { NCol }

  Procedure Plasma(X1,Y1,X2,Y2: Word);
  Var Xn,Yn,DXY,P1,P2,P3,P4: Word;
  Begin
    If (X2 - X1 < 2) And (Y2 - Y1 < 2) Then Exit;
    P1  := Mp[(Y1 * (NPX + 0)) + X1];
    P2  := Mp[(Y2 * (NPX + 0)) + X1];
    P3  := Mp[(Y1 * (NPX + 0)) + X2];
    P4  := Mp[(Y2 * (NPX + 0)) + X2];
    Xn  := (X2 + X1) Shr 1;
    Yn  := (Y2 + Y1) Shr 1;
    DXY := 9 * (X2 - X1 + Y2 - Y1) Div 3;
    If Mp[(Y1 * (NPX + 0)) + Xn] = 0 Then Mp[(Y1 * (NPX + 0)) + Xn] := NCol(P1 + P3,DXY,2,2,2);
    If Mp[(Yn * (NPX + 0)) + X1] = 0 Then Mp[(Yn * (NPX + 0)) + X1] := NCol(P1 + P2,DXY,2,2,2);
    If Mp[(Yn * (NPX + 0)) + X2] = 0 Then Mp[(Yn * (NPX + 0)) + X2] := NCol(P3 + P4,DXY,2,2,2);
    If Mp[(Y2 * (NPX + 0)) + Xn] = 0 Then Mp[(Y2 * (NPX + 0)) + Xn] := NCol(P2 + P4,DXY,2,2,2);
    Mp[(Yn * (NPX + 0)) + Xn] := NCol(P1 + P2 + P3 + P4,DXY,9.0,4.5,4.0);
    Plasma(X1,Y1,Xn,Yn);
    Plasma(Xn,Y1,X2,Yn);
    Plasma(X1,Yn,Xn,Y2);
    Plasma(Xn,Yn,X2,Y2);
  End; { Plasma }

Begin

  // Set up the palette

  RGBA.A := 0;
  For I := 0 To 127 Do
  Begin
    RGBA.R := Pal[I * 3 + 0] Shl 2;
    RGBA.G := Pal[I * 3 + 1] Shl 2;
    RGBA.B := Pal[I * 3 + 2] Shl 2;
    Colors[I * 2 + 0] := TColor(RGBA);
    Colors[I * 2 + 1] := TColor(RGBA);
  End; // For I

  Randomize;
  SetLength(Mp,(NPY + 1) * (NPX + 1));
  FillChar(Mp[0],(NPY + 1) * (NPX + 1),0);
//  SetLength(Mp,(NXY + 1) * (NXZ + 1));
//  FillChar(Mp[0],(NXY + 1)* (NXZ + 1) - 1,0);
  Mp[0] := 128;
//  Plasma(0,0,NXY,NXZ);
  Plasma(0,0,NPX,NPY);

  // Tweak plasma

  For I := 0 To NPY - 1 Do
  Begin
    Move(Mp[I * NPX],Temp[0],NPX);

    If Sin(Pi * I / (NPY - 1)) > 0
     Then X := 1 / Sin(Pi * I / (NPY - 1))
     Else X := NPX Div 2;

    X := Power(X,1.5);
//    X := Sqr(X);

    If X > NPX Div 2 Then X := NPX Div 2;

    Y    := 0;
    K    := 0;
    J0   := 0;
    Last := 0;
    Next := 0;
    For J := 0 To (NPX Div 2) - 1 Do
    Begin
      If J = K
       Then Temp[J] := Mp[I * NPX + K]
       Else Temp[J] := Round(Last + (Next - Last) * Abs((J - J0) / (K - J0))); //Temp[J - 1];
      If J >= K Then
      Begin
        J0   := J;
        Last := Mp[I * NPX + K];
        Y    := Y + X;
        K    := Trunc(Y);
        If K > (NPX Div 2) - 1 Then
        Begin
          K := (NPX Div 2) - 1;
          If X >= NPX Div 2
           Then Next := Last
           Else Next := Mp[I * NPX + (NPX Div 2) - 1];
        End
        Else Next := Mp[I * NPX + K];
      End;
    End; // For J

    Y    := 0;
    K    := NPX - 1;
    J0   := 0;
    Last := 0;
    Next := 0;
    For J := NPX - 1 DownTo NPX Div 2 Do
    Begin
      If J = K
       Then Temp[J] := Mp[I * NPX + K]
       Else Temp[J] := Round(Last + (Next - Last) * Abs((J - J0) / (K - J0))); //Temp[J + 1];
      If J <= K Then
      Begin
        J0   := J;
        Last := Mp[I * NPX + K];
        Y    := Y + X;
        K    := Trunc(NPX - 1 - Y);
        If K < NPX Div 2 Then
        Begin
          K := NPX Div 2;
          If X >= NPX Div 2
           Then Next := Last
           Else Next := Mp[I * NPX + (NPX Div 2)];
        End
        Else Next := Mp[I * NPX + K];
      End;
    End; // For J

    // Modify the next row if need be

    If I < NPY - 1 Then
     For J := 0 To NPX - 1 Do
     Begin
       If Temp[J] <> Mp[I * NPX + J] Then
        Mp[(I + 1) * NPX + J] := (Mp[(I + 1) * NPX + J] + Temp[J]) Div 2;
     End; // For J

    Move(Temp[0],Mp[I * NPX],NPX);
  End; // For I

  SetLength(v,NXY * NXZ);
  SetLength(p,NXY * NXZ);

  // remove existing frames (1st time - not necessary)
  GLVisir1.Clear;

  // Make a sphere (speed-optimized code)

  r := 1;
  For i := 0 To NXY - 1 Do
  Begin
    j2 := 0;
    i1 := i / (NXY - 1);
    i3 := (i + 1) Mod NXY;
    si := sin(2 * 3.14 * i1);
    ci := cos(2 * 3.14 * i1);
    For j := 0 To NXZ - 1 Do
    Begin
      j1 := j / (NXZ - 1);
      j3 := ((j + 1) Mod NXZ) * NXY;

      v[i+j*NXY].x    := r * ci * sin(1*3.14*j1);
      v[i+j*NXY].y    := r * si * sin(1*3.14*j1);
      v[i+j*NXY].z    := r * cos(1*3.14*j1);


{
      v[i+j*NXY].x    := r * si * cos(2*3.14*j1);
      v[i+j*NXY].y    := r * ci * cos(2*3.14*j1);
      v[i+j*NXY].z    := r * sin(2*3.14*j1);
}
      p[i+j*NXY].n[0] := i  + j2;
      p[i+j*NXY].n[1] := i3 + j2;
      p[i+j*NXY].n[2] := i3 + j3;
      p[i+j*NXY].n[3] := i  + j3;
      p[i+j*NXY].numVertexes := 4;
      p[i+j*NXY].col := clRed;
      Inc(j2,NXY);
    End; // For j
  End; // For i
  nv := NXY * NXZ;
  np := NXY * NXZ;

  // Assign colors

  WA := 0;
  For I := 0 To NP - 1 Do
  Begin
    v1 := v[p[I].n[0]];
    v2 := v[p[I].n[1]];
    v3 := v[p[I].n[2]];
    v4 := v[p[I].n[3]];
    x  := (v1.x + v2.x + v3.x + v4.x) / 4;
    y  := (v1.y + v2.y + v3.y + v4.y) / 4;
    z  := (v1.z + v2.z + v3.z + v4.z) / 4;
    If X <> 0 Then A1 := ArcTan2(Y,X)
    Else
    Begin
      If Y > 0 Then A1 := Pi / 2 Else A1 := -Pi / 2;
    End;

    R := Sqrt(sqr(X) + Sqr(Y));
    If R <> 0 Then A2 := ArcTan2(Z,R)
    Else
    Begin
      If Z > 0 Then A2 := Pi / 2 Else A2 := -Pi / 2;
    End;

    If A2 >  Pi / 2 Then A2 :=  (Pi / 2) - (A2 - Pi / 2);
    If A2 < -Pi / 2 Then A2 := (-Pi / 2) + (-Pi / 2 - A2);

    cx := (NPX Div 2) + Trunc(A1 * (NPX Div 2) / Pi);
    While cx < 0    Do Inc(cx,NPX);
    While cx >= NPX Do Dec(cx,NPX);

    cy := (NPY Div 2) - Trunc(A2 * (NPY Div 2) / (Pi / 2));
    While cy < 0    Do Inc(cy,NPY);
    While cy >= NPY Do Dec(cy,NPY);

    p[I].col := Colors[Mp[cx + cy * NPX]];

    For L := 0 To P[I].NumVertexes - 1 Do
    Begin
      V[P[I].N[L]].R        := TRGBA(P[I].Col).R;
      V[P[I].N[L]].G        := TRGBA(P[I].Col).G;
      V[P[I].N[L]].B        := TRGBA(P[I].Col).B;
      V[P[I].N[L]].A        := 255;
      V[P[I].N[L]].UseColor := True;
    End; // For L




    p[I].UseColor := False;
    p[I].TextureID := -1;

    If Mp[cx + cy * NPX] <= 51 Then
     WA := WA + (2 * Pi / NPX) * Abs(Sin(A1 + (Pi / NPY)) - Sin(A1 - (Pi / NPY)));

  End; // For I
  WA := WA / (2 * Pi);
  K := Trunc(100 * WA);
  lblWater.Caption := IntToStr(K) + '% water';
  lblLand.Caption  := IntToStr(100 - K) + '% land';

{
  r  := 1;
  nv := 0;
  np := 0;
  for i:=0 to NXY - 1 do
   for j:=0 to NXZ - 1 do
    begin
     v[nv+i+j*NXY].x := r*sin(2*3.14*i/NXY)*cos(2*3.14*j/NXZ);
     v[nv+i+j*NXY].y := r*cos(2*3.14*i/NXY)*cos(2*3.14*j/NXZ);
     v[nv+i+j*NXY].z := r*sin(2*3.14*j/NXZ);
    end;

  for i:=0 to NXY - 1 do
   for j:=0 to NXZ - 1 do
    begin
     p[np+i+j*NXY].n[0] := nv+i+j*NXY;
     p[np+i+j*NXY].n[1] := nv+((i+1) mod NXY)+j*NXY;
     p[np+i+j*NXY].n[2] := nv+((i+1) mod NXY)+((j+1) mod NXZ)*NXY;
     p[np+i+j*NXY].n[3] := nv+i+((j+1) mod NXZ)*NXY;
     p[np+i+j*NXY].numVertexes:=4;
     p[np+i+j*NXY].col := clRed;
    end;
   nv := nv + NXY * NXZ;
   np := np + NXY * NXZ;
}

  // prepare structure for AddModel call

  mdl.nv := nv;
  mdl.np := np;
  SetLength(mdl.v, mdl.nv);
  SetLength(mdl.p, mdl.np);
  for i := 0 to nv - 1 do mdl.v[i] := v[i];
  for i := 0 to np - 1 do mdl.p[i] := p[i];

  GLVisir1.AddModelToNewFrame(mdl,True);
  
  // center and zoom added models
  GLVisir1.Fit(false, // don't move coordinates system to zero,
                 false  // keep proportions
                 );

  For I := 0 To GLVisir1.Scene3D.Scene.GetNumEntities - 1 Do GLVisir1.Scene3D.Scene.GetEntity(I).Visible := True;
  GLVisir1.Scene3D.Redraw;

//  SetLength(v,0);
//  SetLength(p,0);
//  SetLength(Mp,0);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  lblWater.Caption := '';
  lblLand.Caption  := '';
end;

end.
