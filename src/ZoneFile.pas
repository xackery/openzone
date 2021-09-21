unit ZoneFile;

interface

Uses Graphics,Classes;

Type
  T2IVector = Record
    X,Y   : Integer;
  End;
  T3DVector = Record
    X,Y,Z : Single;
  End;
  TZoneTreeItem = Class
    Left   : Integer;         // Index of left node
    Right  : Integer;         // Index of right node
    Normal : T3DVector;       // Normal
    Len    : Single;          // Length of vector
    Item   : Integer;         // Index of TZoneItem
    Procedure ReadFromFile(Var F: File);
    Procedure WriteToFile(Var F: File);
  End;
  TZoneTree = Class
    TreeItems : TStringList;
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   ReadFromFile(Var F: File);
    Procedure   WriteToFile(Var F: File);
  End;
  TZoneItem = Class
    Canned : Boolean;         // True if it uses a canned item (e.g. tree, torch, etc),
                              // false if it points to a TZoneGeometry item
    Index  : Integer;         // Index if canned item, or index of TZoneGeometry item
    Loc    : T3DVector;       // Position if this points to a canned item
    AX     : Single;          // Angle of rotation about X axis, if this is a canned item
    AY     : Single;          // Angle of rotation about Y axis, if this is a canned item
    AZ     : Single;          // Angle of rotation about Z axis, if this is a canned item
    Procedure ReadFromFile(Var F: File);
    Procedure WriteToFile(Var F: File);
  End;
  TZoneTriangle = Array [0..2] Of Integer;
  TZoneGeometry = Class
    Scale           : Integer;
    Center          : T3DVector;              // Center offset to vertex coords
                                              // (makes bounding box calculations fast)
    Vertices        : Array Of T3DVector;
    Normals         : Array Of T3DVector;     // One per vertex
    TextureVertices : Array Of T2IVector;     // One per vertex
    Colors          : Array Of TColor;        // RGBA, one per vertex
    Triangles       : Array Of TZoneTriangle;
    Textures        : Array Of Integer;       // One per triangle, index of TZoneMultiTexture
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   ReadFromFile(Var F: File);
    Procedure   WriteToFile(Var F: File);
  End;
  TZoneLightDef = Class
    Color     : TColor;
    Diffusion : Single;       // 0 to 1
    Intensity : Single;       // 0 to 1
    Procedure ReadFromFile(Var F: File);
    Procedure WriteToFile(Var F: File);
  End;
  TZoneTexture = Class
    ID       : Integer;       // Positive number if canned, or -1 if not
    FileName : String;
    Procedure ReadFromFile(Var F: File);
    Procedure WriteToFile(Var F: File);
  End;
  TZoneMultiTexture = Class
    Textures : Array Of Integer; // Index of TZoneTexture
    Procedure ReadFromFile(Var F: File);
    Procedure WriteToFile(Var F: File);
  End;
  TZoneFile = Class
  Protected
    FFileName        : String;
    FTree            : TZoneTree;
    FItems           : TStringList;
    FGeometries      : TStringList;
    FSafePoint       : T3DVector;
    FLightDefs       : TStringList;
    FTextures        : TStringList;
    FMultiTextures   : TStringList;
    FDefaultLightDef : TZoneLightDef;
    Procedure   BuildTree(RL: Array Of Integer);
  Public
    Constructor Create(AFileName: String);
    Destructor  Destroy; Override;
    Procedure   ReadFromFile;
    Procedure   WriteToFile;
    Property    FileName        : String        Read FFileName;
    Property    Tree            : TZoneTree     Read FTree;
    Property    Items           : TStringList   Read FItems;
    Property    Geometries      : TStringList   Read FGeometries;
    Property    SafePoint       : T3DVector     Read FSafePoint;
    Property    LightDefs       : TStringList   Read FLightDefs;
    Property    Textures        : TStringList   Read FTextures;
    Property    MultiTextures   : TStringList   Read FMultiTextures;
    Property    DefaultLightDef : TZoneLightDef Read FDefaultLightDef;
  End;

implementation

Uses SysUtils;

// TZoneTreeItem

Procedure TZoneTreeItem.ReadFromFile(Var F: File);
Begin
  BlockRead(F,Left,SizeOf(Left));
  BlockRead(F,Right,SizeOf(Right));
  BlockRead(F,Normal.X,SizeOf(Normal.X));
  BlockRead(F,Normal.Y,SizeOf(Normal.Y));
  BlockRead(F,Normal.Z,SizeOf(Normal.Z));
  BlockRead(F,Len,SizeOf(Len));
  BlockRead(F,Item,SizeOf(Item));
End; // TZoneTreeItem.ReadFromFile

Procedure TZoneTreeItem.WriteToFile(Var F: File);
Begin
  BlockWrite(F,Left,SizeOf(Left));
  BlockWrite(F,Right,SizeOf(Right));
  BlockWrite(F,Normal.X,SizeOf(Normal.X));
  BlockWrite(F,Normal.Y,SizeOf(Normal.Y));
  BlockWrite(F,Normal.Z,SizeOf(Normal.Z));
  BlockWrite(F,Len,SizeOf(Len));
  BlockWrite(F,Item,SizeOf(Item));
End; // TZoneTreeItem.WriteToFile

// TZoneTree

Constructor TZoneTree.Create;
Begin
  TreeItems := TStringList.Create;
End; // TZoneTree.Create

Destructor TZoneTree.Destroy;
Var I: Integer;
Begin
  For I := 0 To TreeItems.Count - 1 Do TreeItems.Objects[I].Free;
  TreeItems.Free;
End; // TZoneTree.Destroy

Procedure TZoneTree.ReadFromFile(Var F: File);
Var I: Integer;
Begin
  BlockRead(F,I,SizeOf(I));
  While I > 0 Do
  Begin
    TreeItems.AddObject('',TZoneTreeItem.Create);
    (TreeItems.Objects[I] As TZoneTreeItem).ReadFromFile(F);
    Dec(I);
  End; // For I
End; // TZoneTree.ReadFromFile

Procedure TZoneTree.WriteToFile(Var F: File);
Var I: Integer;
Begin
  I := TreeItems.Count;
  BlockWrite(F,I,SizeOf(I));
  For I := 0 To TreeItems.Count - 1 Do (TreeItems.Objects[I] As TZoneTreeItem).WriteToFile(F);
End; // TZoneTree.WriteToFile

// TZoneItem

Procedure TZoneItem.ReadFromFile(Var F: File);
Begin
  BlockRead(F,Canned,SizeOf(Canned));
  BlockRead(F,Index,SizeOf(Index));
  BlockRead(F,Loc.X,SizeOf(Loc.X));
  BlockRead(F,Loc.Y,SizeOf(Loc.Y));
  BlockRead(F,Loc.Z,SizeOf(Loc.Z));
  BlockRead(F,AX,SizeOf(AX));
  BlockRead(F,AY,SizeOf(AY));
  BlockRead(F,AZ,SizeOf(AZ));
End; // TZoneItem.ReadFromFile

Procedure TZoneItem.WriteToFile(Var F: File);
Begin
  BlockWrite(F,Canned,SizeOf(Canned));
  BlockWrite(F,Index,SizeOf(Index));
  BlockWrite(F,Loc.X,SizeOf(Loc.X));
  BlockWrite(F,Loc.Y,SizeOf(Loc.Y));
  BlockWrite(F,Loc.Z,SizeOf(Loc.Z));
  BlockWrite(F,AX,SizeOf(AX));
  BlockWrite(F,AY,SizeOf(AY));
  BlockWrite(F,AZ,SizeOf(AZ));
End; // TZoneItem.WriteToFile

// TZoneGeometry

Constructor TZoneGeometry.Create;
Begin
  Scale    := 1;
  Center.X := 0;
  Center.Y := 0;
  Center.Z := 0;
  SetLength(Vertices,0);
  SetLength(Normals,0);
  SetLength(Triangles,0);
  SetLength(Textures,0);
  SetLength(TextureVertices,0);
  SetLength(Colors,0);
End; // TZoneGeometry.Create

Destructor TZoneGeometry.Destroy;
Begin
  SetLength(Vertices,0);
  SetLength(Normals,0);
  SetLength(Triangles,0);
  SetLength(Textures,0);
  SetLength(TextureVertices,0);
  SetLength(Colors,0);
End; // TZoneGeometry.Destroy

Procedure TZoneGeometry.ReadFromFile(Var F: File);
Var I,J: Integer;
Begin
  BlockRead(F,Scale,SizeOf(Scale));
  BlockRead(F,Center.X,SizeOf(Center.X));
  BlockRead(F,Center.Y,SizeOf(Center.Y));
  BlockRead(F,Center.Z,SizeOf(Center.Z));
  BlockRead(F,I,SizeOf(I));
  SetLength(Vertices,I);
  For I := 0 To High(Vertices) Do
  Begin
    BlockRead(F,Vertices[I].X,SizeOf(Vertices[I].X));
    BlockRead(F,Vertices[I].Y,SizeOf(Vertices[I].Y));
    BlockRead(F,Vertices[I].Z,SizeOf(Vertices[I].Z));
  End; // For I
  SetLength(Normals,High(Vertices) + 1);
  For I := 0 To High(Normals) Do
  Begin
    BlockRead(F,Normals[I].X,SizeOf(Normals[I].X));
    BlockRead(F,Normals[I].Y,SizeOf(Normals[I].Y));
    BlockRead(F,Normals[I].Z,SizeOf(Normals[I].Z));
  End; // For I
  SetLength(TextureVertices,High(Vertices) + 1);
  For I := 0 To High(TextureVertices) Do
  Begin
    BlockRead(F,TextureVertices[I].X,SizeOf(TextureVertices[I].X));
    BlockRead(F,TextureVertices[I].Y,SizeOf(TextureVertices[I].Y));
  End; // For I
  SetLength(Colors,High(Vertices) + 1);
  For I := 0 To High(Colors) Do BlockRead(F,Colors[I],SizeOf(Colors[I]));
  BlockRead(F,I,SizeOf(I));
  SetLength(Triangles,I);
  For I := 0 To High(Triangles) Do
   For J := 0 To 2 Do BlockRead(F,Triangles[I][J],SizeOf(Triangles[I][J]));
  SetLength(Textures,High(Triangles) + 1);
  For I := 0 To High(Textures) Do BlockRead(F,Textures[I],SizeOf(Textures[I]));
End; // TZoneGeometry.ReadFromFile

Procedure TZoneGeometry.WriteToFile(Var F: File);
Var I,J: Integer;
Begin
  BlockWrite(F,Scale,SizeOf(Scale));
  BlockWrite(F,Center.X,SizeOf(Center.X));
  BlockWrite(F,Center.Y,SizeOf(Center.Y));
  BlockWrite(F,Center.Z,SizeOf(Center.Z));
  I := High(Vertices) + 1;
  BlockWrite(F,I,SizeOf(I));
  For I := 0 To High(Vertices) Do
  Begin
    BlockWrite(F,Vertices[I].X,SizeOf(Vertices[I].X));
    BlockWrite(F,Vertices[I].Y,SizeOf(Vertices[I].Y));
    BlockWrite(F,Vertices[I].Z,SizeOf(Vertices[I].Z));
  End; // For I
  For I := 0 To High(Normals) Do
  Begin
    BlockWrite(F,Normals[I].X,SizeOf(Normals[I].X));
    BlockWrite(F,Normals[I].Y,SizeOf(Normals[I].Y));
    BlockWrite(F,Normals[I].Z,SizeOf(Normals[I].Z));
  End; // For I
  For I := 0 To High(TextureVertices) Do
  Begin
    BlockWrite(F,TextureVertices[I].X,SizeOf(TextureVertices[I].X));
    BlockWrite(F,TextureVertices[I].Y,SizeOf(TextureVertices[I].Y));
  End; // For I
  For I := 0 To High(Colors) Do BlockWrite(F,Colors[I],SizeOf(Colors[I]));
  I := High(Triangles) + 1;
  BlockWrite(F,I,SizeOf(I));
  For I := 0 To High(Triangles) Do
   For J := 0 To 2 Do BlockWrite(F,Triangles[I][J],SizeOf(Triangles[I][J]));
  For I := 0 To High(Textures) Do BlockWrite(F,Textures[I],SizeOf(Textures[I]));
End; // TZoneGeometry.WriteToFile

// TZoneLightDef

Procedure TZoneLightDef.ReadFromFile(Var F: File);
Begin
  BlockRead(F,Color,SizeOf(Color));
  BlockRead(F,Diffusion,SizeOf(Diffusion));
  BlockRead(F,Intensity,SizeOf(Intensity));
End; // TZoneLightDef.ReadFromFile

Procedure TZoneLightDef.WriteToFile(Var F: File);
Begin
  BlockWrite(F,Color,SizeOf(Color));
  BlockWrite(F,Diffusion,SizeOf(Diffusion));
  BlockWrite(F,Intensity,SizeOf(Intensity));
End; // TZoneLightDef.WriteToFile

// TZoneTexture

Procedure TZoneTexture.ReadFromFile(Var F: File);
Var St: Packed Array[0..255] Of Char;
Begin
  BlockRead(F,ID,SizeOf(ID));
  BlockRead(F,St,SizeOf(St));   // Null-terminated string
  FileName := StrPas(St);
End; // TZoneTexture.ReadFromFile

Procedure TZoneTexture.WriteToFile(Var F: File);
Var St: Packed Array[0..255] Of Char;
Begin
  BlockWrite(F,ID,SizeOf(ID));
  StrPCopy(St,FileName);
  BlockWrite(F,St,SizeOf(St));
End; // TZoneTexture.WriteToFile

// TZoneMultiTexture

Procedure TZoneMultiTexture.ReadFromFile(Var F: File);
Var I: Integer;
Begin
  BlockRead(F,I,SizeOf(I));
  SetLength(Textures,I);
  For I := 0 To High(Textures) Do BlockRead(F,Textures[I],SizeOf(Textures[I]));
End; // TZoneMultiTexture.ReadFromFile

Procedure TZoneMultiTexture.WriteToFile(Var F: File);
Var I: Integer;
Begin
  I := High(Textures) + 1;
  BlockWrite(F,I,SizeOf(I));
  For I := 0 To High(Textures) Do BlockWrite(F,Textures[I],SizeOf(Textures[I]));
End; // TZoneMultiTexture.WriteToFile

// TZoneFile

Constructor TZoneFile.Create(AFileName: String);
Begin
  FFileName        := AFileName;
  FTree            := TZoneTree.Create;
  FItems           := TStringList.Create;
  FGeometries      := TStringList.Create;
  FSafePoint.X     := 0;
  FSafePoint.Y     := 0;
  FSafePoint.Z     := 0;
  FLightDefs       := TStringList.Create;
  FTextures        := TStringList.Create;
  FMultiTextures   := TStringList.Create;
  FDefaultLightDef := TZoneLightDef.Create;
End; // TZoneFile.Create

Destructor TZoneFile.Destroy;
Var I: Integer;
Begin
  For I := 0 To FItems.Count         - 1 Do FItems.Objects[I].Free;
  For I := 0 To FGeometries.Count    - 1 Do FGeometries.Objects[I].Free;
  For I := 0 To FLightDefs.Count     - 1 Do FLightDefs.Objects[I].Free;
  For I := 0 To FTextures.Count      - 1 Do FTextures.Objects[I].Free;
  For I := 0 To FMultiTextures.Count - 1 Do FMultiTextures.Objects[I].Free;
  FTree.Free;
  FItems.Free;
  FGeometries.Free;
  FLightDefs.Free;
  FTextures.Free;
  FMultiTextures.Free;
  FDefaultLightDef.Free;
End; // TZoneFile.Destroy

Procedure TZoneFile.ReadFromFile;
Var
  I            : Integer;
  F            : File;
  Item         : TZoneItem;
  Geometry     : TZoneGeometry;
  LightDef     : TZoneLightDef;
  Texture      : TZoneTexture;
  MultiTexture : TZoneMultiTexture;

Begin
  If FileExists(FFileName) Then
  Begin
    AssignFile(F,FFileName);
    Reset(F,1);

    BlockRead(F,FSafePoint.X,SizeOf(FSafePoint.X));
    BlockRead(F,FSafePoint.Y,SizeOf(FSafePoint.Y));
    BlockRead(F,FSafePoint.Z,SizeOf(FSafePoint.Z));

    FDefaultLightDef.ReadFromFile(F);

    FTree.ReadFromFile(F);

    BlockRead(F,I,SizeOf(I));
    While I > 0 Do
    Begin
      Item := TZoneItem.Create;
      Item.ReadFromFile(F);
      FItems.AddObject('',Item);
      Dec(I);
    End; // While

    BlockRead(F,I,SizeOf(I));
    While I > 0 Do
    Begin
      Geometry := TZoneGeometry.Create;
      Geometry.ReadFromFile(F);
      FGeometries.AddObject('',Geometry);
      Dec(I);
    End; // While

    BlockRead(F,I,SizeOf(I));
    While I > 0 Do
    Begin
      LightDef := TZoneLightDef.Create;
      LightDef.ReadFromFile(F);
      FLightDefs.AddObject('',LightDef);
      Dec(I);
    End; // While

    BlockRead(F,I,SizeOf(I));
    While I > 0 Do
    Begin
      Texture := TZoneTexture.Create;
      Texture.ReadFromFile(F);
      FTextures.AddObject('',Texture);
      Dec(I);
    End; // While

    BlockRead(F,I,SizeOf(I));
    While I > 0 Do
    Begin
      MultiTexture := TZoneMultiTexture.Create;
      MultiTexture.ReadFromFile(F);
      FMultiTextures.AddObject('',MultiTexture);
      Dec(I);
    End; // While

    CloseFile(F);
  End;
End; // TZoneFile.ReadFromFile

Procedure TZoneFile.WriteToFile;
Var
  I  : Integer;
  F  : File;
  RL : Array Of Integer;

Begin
  // Build the tree, starting from the first item

  FTree.Free;
  FTree := TZoneTree.Create;
  SetLength(RL,FItems.Count);
  For I := 0 To High(RL) Do RL[I] := I;
  BuildTree(RL);

  // Write the data

  AssignFile(F,FFileName);
  ReWrite(F,1);

  BlockWrite(F,FSafePoint.X,SizeOf(FSafePoint.X));
  BlockWrite(F,FSafePoint.Y,SizeOf(FSafePoint.Y));
  BlockWrite(F,FSafePoint.Z,SizeOf(FSafePoint.Z));

  FDefaultLightDef.WriteToFile(F);

  FTree.WriteToFile(F);

  I := FItems.Count;
  BlockWrite(F,I,SizeOf(I));
  For I := 0 To FItems.Count - 1 Do (FItems.Objects[I] As TZoneItem).WriteToFile(F);

  I := FGeometries.Count;
  BlockWrite(F,I,SizeOf(I));
  For I := 0 To FGeometries.Count - 1 Do (FGeometries.Objects[I] As TZoneGeometry).WriteToFile(F);

  I := FLightDefs.Count;
  BlockWrite(F,I,SizeOf(I));
  For I := 0 To FLightDefs.Count - 1 Do (FLightDefs.Objects[I] As TZoneLightDef).WriteToFile(F);

  I := FTextures.Count;
  BlockWrite(F,I,SizeOf(I));
  For I := 0 To FTextures.Count - 1 Do (FTextures.Objects[I] As TZoneTexture).WriteToFile(F);

  I := FMultiTextures.Count;
  BlockWrite(F,I,SizeOf(I));
  For I := 0 To FMultiTextures.Count - 1 Do (FMultiTextures.Objects[I] As TZoneMultiTexture).WriteToFile(F);

  CloseFile(F);
End; // TZoneFile.WriteToFile

Procedure TZoneFile.BuildTree(RL: Array Of Integer);
Type TCountAxis = (caX,caY,caZ);
Var
  I    : Integer;
  XC   : Integer;
  YC   : Integer;
  ZC   : Integer;
  DX   : Single;
  DY   : Single;
  DZ   : Single;
  RL1  : Array Of Integer;
  RL2  : Array Of Integer;
  Axis : TCountAxis;
  S    : TZoneGeometry;
  B    : Boolean;
  TI   : TZoneTreeItem;
  TIL  : TZoneTreeItem;
  TIR  : TZoneTreeItem;

  Function Count(Axis: TCountAxis; RL: Array Of Integer; Var DivPoint: Single): Integer;
  Var
    MinS  : Array Of Single;
    MaxS  : Array Of Single;
    I,J,K : Integer;
    MinA  : Single;
    MaxA  : Single;
    MinA1 : Single;
    MaxA1 : Single;
    S     : TZoneGeometry;
    R,V   : Single;
    Found : Boolean;

  Begin

    // Determine the minimum and maximum extents

    MinA := 99999;
    MaxA := -99999;
    SetLength(MinS,0);
    SetLength(MaxS,0);
    For I := 0 To High(RL) Do
    Begin
      S     := FGeometries.Objects[RL[I]] As TZoneGeometry;
      V     := S.Scale;
      MinA1 := 99999;
      MaxA1 := -99999;
      For J := 0 To High(S.Vertices) Do
      Begin
        Case Axis Of
          caX: R := (S.Vertices[J].X / V) + S.Center.X;
          caY: R := (S.Vertices[J].Y / V) + S.Center.Y;
          caZ: R := (S.Vertices[J].Z / V) + S.Center.Z;
        Else
          R := 0;
        End; // Case
        If R < MinA  Then MinA  := R;
        If R > MaxA  Then MaxA  := R;
        If R < MinA1 Then MinA1 := R;
        If R > MaxA1 Then MaxA1 := R;
      End; // For J
      Found := False;
      For J := 0 To High(MinS) Do
      Begin

        // Have to use Round() or roundoff errors will hurt us

        If (Round(MinA1) < Round(MaxS[J])) And (Round(MaxA1) > Round(MinS[J])) Then
        Begin
          If Round(MinA1) < Round(MinS[J]) Then MinS[J] := Round(MinA1);
          If Round(MaxA1) > Round(MaxS[J]) Then MaxS[J] := Round(MaxA1);
          Found   := True;
        End;
      End; // For J
      If Not Found Then
      Begin
        SetLength(MinS,High(MinS) + 2);
        SetLength(MaxS,High(MaxS) + 2);
        MinS[High(MinS)] := Round(MinA1);
        MaxS[High(MaxS)] := Round(MaxA1);
      End;
    End; // For I

    // MinS and MaxS contain a list of possible subdivions.  Remove any duplicates,
    // sort the list, and choose the one closest to the center.

    I := 0;
    While I < High(MinS) Do
    Begin
      J := I + 1;
      While J <= High(MinS) Do
      Begin
        // Remove duplicate entries

        If (MinS[J] = MinS[I]) And (MaxS[J] = MaxS[I]) Then
        Begin
          For K := J To High(MinS) - 1 Do
          Begin
            MinS[K] := MinS[K + 1];
            MaxS[K] := MaxS[K + 1];
          End; // For K
          SetLength(MinS,High(MinS));
          SetLength(MaxS,High(MaxS));
        End
        Else
        Begin
          If (MinS[J] < MinS[I]) Or
             ((MinS[J] = MinS[I]) And (MaxS[J] < MaxS[I])) Then
          Begin
            R       := MinS[J];
            MinS[J] := MinS[I];
            MinS[I] := R;
            R       := MaxS[J];
            MaxS[J] := MaxS[I];
            MaxS[I] := R;
          End;
          Inc(J);
        End;
      End; // While
      Inc(I);
    End; // While

    // Return the division point, if any

    I := (High(MinS) Div 2) + 1;
    If I <= High(MinS) Then
    Begin
      DivPoint := MinS[I];
      Result := High(MinS) + 1;
    End
    Else
    Begin
      DivPoint := 0;
      Result   := 0;
    End;

    // Cleanup

    SetLength(MinS,0);
    SetLength(MaxS,0);
  End; // Count

Begin
  // Count how many regions lie along the x, y, and z directions, and
  // subdivide along the one with the greatest number of possible subdivisions

  XC := Count(caX,RL,DX);
  YC := Count(caY,RL,DY);
  ZC := Count(caZ,RL,DZ);

  If XC > YC Then
  Begin
    If XC > ZC Then Axis := caX Else Axis := caZ;
  End
  Else
  Begin
    If YC > ZC Then Axis := caY Else Axis := caZ;
  End;

  // Populate lists for the left and right node

  For I := 0 To High(RL) Do
  Begin
    S := FGeometries.Objects[RL[I]] As TZoneGeometry;
    Case Axis Of
      caX: B := (S.Center.X <= DX);
      caY: B := (S.Center.Y <= DY);
      caZ: B := (S.Center.Z <= DZ);
    Else
      B := True;
    End; // Case
    If B Then
    Begin
      SetLength(RL1,High(RL1) + 2);
      RL1[High(RL1)] := RL[I];
    End
    Else
    Begin
      SetLength(RL2,High(RL2) + 2);
      RL2[High(RL2)] := RL[I];
    End;
  End; // For I

  // Add a stem record

  TI := TZoneTreeItem.Create;
  Tree.TreeItems.AddObject('',TI);
  TI.Item := 0;
  If Axis = caX Then TI.Normal.X := -1 Else TI.Normal.X := 0;
  If Axis = caY Then TI.Normal.Y := -1 Else TI.Normal.Y := 0;
  If Axis = caZ Then TI.Normal.Z := -1 Else TI.Normal.Z := 0;
  Case Axis Of
    caX: TI.Len := DX;
    caY: TI.Len := DY;
    caZ: TI.Len := DZ;
  End; // Case
  TI.Left := Tree.TreeItems.Count;

  // Handle the left node

  If High(RL1) = 0 Then
  Begin
    TIL := TZoneTreeItem.Create;
    Tree.TreeItems.AddObject('',TIL);
    TIL.Item     := RL1[0] + 1;
    TIL.Normal.X := 0;
    TIL.Normal.Y := 0;
    TIL.Normal.Z := 0;
    TIL.Len      := 0;
    TIL.Left     := 0;
    TIL.Right    := 0;
  End
  Else If High(RL1) > 0 Then BuildTree(RL1);
  SetLength(RL1,0);

  // Handle the right node

  TI.Right := Tree.TreeItems.Count;
  If High(RL2) = 0 Then
  Begin
    TIR := TZoneTreeItem.Create;
    Tree.TreeItems.AddObject('',TIR);
    TIR.Item     := RL2[0] + 1;
    TIR.Normal.X := 0;
    TIR.Normal.Y := 0;
    TIR.Normal.Z := 0;
    TIR.Len      := 0;
    TIR.Left     := 0;
    TIR.Right    := 0;
  End
  Else If High(RL2) > 0 Then BuildTree(RL2);
  SetLength(RL2,0);
End; // TZoneFile.BuildTree

end.
