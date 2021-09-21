unit Anim8or;

interface

Uses Graphics,Classes,Points3D;

Type
  TRGBA = Packed Record
    R,G,B,A: Byte;
  End;
  TXYZPoint = Record
    X,Y,Z: Single;
  End;
  TEdge = Record
    Pt1,Pt2: Integer;
  End;
  TTexCoord = Record
    TX,TZ : Single;
  End;
  TXYZPointArray = Array Of TXYZPoint;
  TEdgeArray     = Array Of TEdge;
  TTexCoordArray = Array Of TTexCoord;
  TAn8File = Class;
  TAn8Texture = Class
    Name     : String;
    FileName : String;
    Parent   : TAn8File;
    Constructor Create(AParent: TAn8File);
    Procedure   ParseData(St: String);
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
  End;
  TAn8BlendMode = (abmDecal,abmDarken,abmLighten);
  TAn8AlphaMode = (aamNone,aamLayer,aamFinal);
  TAn8TextureParams = Class
    BlendMode : TAn8BlendMode;
    AlphaMode : TAn8AlphaMode;
    Percent   : Integer;
    Constructor Create;
    Procedure   ParseData(St: String);
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
  End;
  TAn8ColorType = (actAmbient,actDiffuse,actSpecular,actEmissive);
  TAn8Color = Class
    RGB           : TColor;
    Factor        : Single;
    Texture       : TAn8Texture; // Reference
    TextureParams : TAn8TextureParams;
    ColorType     : TAn8ColorType;
    An8File       : TAn8File;
    Constructor Create(AFile: TAn8File; AColorType: TAn8ColorType);
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
  End;
  TAn8Surface = Class
    RGB                : TColor;
    LockAmbientDiffuse : Boolean;
    Ambient            : TAn8Color;
    Diffuse            : TAn8Color;
    Specular           : TAn8Color;
    Emissive           : TAn8Color;
    PhongSize          : Single;
    Alpha              : Integer;
    Brilliance         : Single;
    An8File            : TAn8File;
    Constructor Create(AFile: TAn8File);
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
  End;
  TAn8Object = Class;
  TAn8Material = Class
    Name    : String;
    Surface : TAn8Surface;
    Parent  : TAn8Object;
    An8File : TAn8File;
    Constructor Create(AParent: TAn8Object); Overload;
    Constructor Create(AFile: TAn8File); Overload;
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
    Function    ReferencesTexture(Texture: TAn8Texture): Boolean;
  End;
  TAn8TexCoord = Class
    TX,TZ : Single;
    Constructor Create; Overload;
    Constructor Create(TexCoord: TAn8TexCoord); Overload;
  End;
  TAn8Edge = Class
    Pt1,Pt2 : Integer;
    Constructor Create; Overload;
    Constructor Create(Edge: TAn8Edge); Overload;
  End;
  TAn8Face = Class
    Material    : TAn8Material;  // Reference
    Points      : Array Of Integer;
    TexCoords   : Array Of Integer;
    Flags       : Integer;
    NormalIndex : Integer;       // If >= 0, is the index for the flat normal of the face itself, not used in .an8 files.
    Constructor Create; Overload;
    Constructor Create(Face: TAn8Face); Overload;
    Destructor  Destroy; Override;
    Function    ReferencesPointIndex(Index: Integer): Boolean;
  End;
  TAn8Mesh = Class;
  TAn8Object = Class
    Name       : String;
    Parent     : TAn8File;
    Materials  : TStringList;    // Contains a list of TAn8Material
    Components : TStringList;    // Contains a list of TAn8Component
    Constructor Create(AParent: TAn8File);
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Function    FindMaterialByName(St: String): TAn8Material;
    Function    FindMeshByName(St: String): TAn8Mesh;
    Procedure   MergeVeryClosePoints;
    Procedure   GetBounds(MinPt,MaxPt: T3DPoint);
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
    Function    GetMeshPointIndex(Const Mesh: TAn8Mesh): Integer;
  End;
  TAn8Bone = Class;
  TAn8Group = Class;
  TAn8Figure = Class;
  TAn8NamedObject = Class;
  TAn8Component = Class
    Name         : String;
    Parent       : TAn8Object;
    Group        : TAn8Group;
    Constructor Create(AName: String; AParent: TAn8Object; AGroup: TAn8Group);
    Destructor  Destroy; Override;
    Procedure   DeterminePrimaryBones(Obj: TAn8NamedObject; RootBone: TAn8Bone); Dynamic; Abstract;
    Procedure   ConvertToTriangles; Dynamic; Abstract;
    Procedure   GetBounds(MinPt,MaxPt: T3DPoint); Dynamic; Abstract;
    Procedure   FindAppropriateBone(Figure: TAn8Figure; Mesh: TAn8Mesh; PointIndex: Integer; Var CandidateBone: TAn8Bone); Dynamic; Abstract;
  End;
  TAn8OriginOrientation = Class
    Origin      : T3DPoint;
    Orientation : TQuaternion;
    Name        : String;
    Constructor Create(AName: String);
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
  End;
  TAn8Group = Class(TAn8Component)
    An8File      : TAn8File;
    Base         : TAn8OriginOrientation;
    Pivot        : TAn8OriginOrientation;
    Components   : TStringList;  // Contains a list of TAn8Component
    Constructor Create(AFile: TAn8File; AName: String; AParent: TAn8Object; AGroup: TAn8Group);
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Procedure   MergeVeryClosePoints;
    Procedure   DeterminePrimaryBones(Obj: TAn8NamedObject; RootBone: TAn8Bone); Override;
    Procedure   ConvertToTriangles; Override;
    Procedure   GetBounds(MinPt,MaxPt: T3DPoint); Override;
    Procedure   FindAppropriateBone(Figure: TAn8Figure; Mesh: TAn8Mesh; PointIndex: Integer; Var CandidateBone: TAn8Bone); Override;
    Function    FindMeshByName(St: String): TAn8Mesh;
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
  End;
  TAn8Mesh = Class(TAn8Component)
    An8File      : TAn8File;
    Base         : TAn8OriginOrientation;
    Pivot        : TAn8OriginOrientation;
    Material     : TAn8Material; // Reference
    SmoothAngle  : Single;
    MaterialList : TStringList;  // Contains references to TAn8Material
    Points       : TStringList;  // Contains a list of T3DPoint
    Normals      : TStringList;  // Contains a list of T3DPoint
    Edges        : TStringList;  // Contains a list of TAn8Edge
    TexCoords    : TStringList;  // Contains a list of TAn8TexCoord
    Faces        : TStringList;  // Contains a list of TAn8Face
    FaceNormals  : TStringList;  // Contains a list of T3DPoint
    PrimaryBones : TStringList;  // Contains references to TAn8Bone
    Constructor Create(AFile: TAn8File; AName: String; AParent: TAn8Object; AGroup: TAn8Group);
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Procedure   CalculateFaceNormals;
    Procedure   CalculateVertexNormals;
    Procedure   ConvertToTriangles; Override;
    Function    GetMostUsedBone: TAn8Bone;
    Procedure   Add(Mesh: TAn8Mesh);
    Procedure   DeterminePrimaryBones(Obj: TAn8NamedObject; RootBone: TAn8Bone); Override;
    Procedure   GetBounds(MinPt,MaxPt: T3DPoint); Override;
    Procedure   FindAppropriateBone(Figure: TAn8Figure; Mesh: TAn8Mesh; PointIndex: Integer; Var CandidateBone: TAn8Bone); Override;
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
    Procedure   SplitPoints;
    Procedure   DeleteEmptyFaces;
    Procedure   DeleteOrphanedPoints;
    Procedure   DeleteOrphanedTexCoords;
  End;
  TAn8DegreeOfFreedom = Record
    Minimum : Single;
    Default : Single;
    Maximum : Single;
  End;
  TAn8BoneInfluence = Record
    Center0    : Single;
    InRadius0  : Single;
    OutRadius0 : Single;
    Center1    : Single;
    InRadius1  : Single;
    OutRadius1 : Single;
  End;
  TAn8VertexWeight = Class
    BoneIndices : Array Of Integer; // Index into a TAn8NamedObject's WeightedBy list
    Weights     : Array Of Single;
    Constructor Create; Overload;
    Constructor Create(Weight: TAn8VertexWeight); Overload;
    Destructor  Destroy; Override;
    Function    HighestInfluenceIndex: Integer;
  End;
  TAn8PaintedWeights = Class
    Name    : String;
    Weights : TStringList; // List of TAn8VertexWeight
    Constructor Create; Overload;
    Constructor Create(PaintedWeights: TAn8PaintedWeights); Overload;
    Destructor  Destroy; Override;
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
    Procedure   DeletePaintedWeight(Index: Integer);
  End;
  TAn8NamedObject = Class
    Obj        : TAn8Object;        // Reference
    ObjName    : String;
    Name       : String;
    Base       : T3DPoint;
    Material   : TAn8Material;      // Reference
    WeightedBy : TStringList;       // Contains references to TAn8Bone
    Parent     : TAn8Bone;
    Weights    : TStringList;       // List of TAn8PaintedWeights          
    Constructor Create(AParent: TAn8Bone); Overload;
    Constructor Create(NO: TAn8NamedObject); Overload;
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
    Procedure   DeletePaintedWeight(Mesh: TAn8Mesh; Index: Integer);
  End;
  TAn8Bone = Class
    Name         : String;
    Len          : Single;
    Diameter     : Single;
    Orientation  : TQuaternion;
    XDof         : TAn8DegreeOfFreedom;
    YDof         : TAn8DegreeOfFreedom;
    ZDof         : TAn8DegreeOfFreedom;
    Influence    : TAn8BoneInfluence;
    Children     : TStringList;      // Contains a list of TAn8Bone
    Parent       : TAn8Bone;
    Figure       : TAn8Figure;
    NamedObjects : TStringList;      // List of TAn8NamedObject
    Index        : Integer;
    Origin       : T3DPoint;
    Constructor Create(AParent: TAn8Bone; AFigure: TAn8Figure); Overload;
    Constructor Create(Bone: TAn8Bone); Overload;
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Function    FindNamedObjects: TStringList;
    Function    FindStrongestBone(CurPos: T3DPoint; NO: TAn8NamedObject; Point: T3DPoint; Var HighestInfluence: Single): TAn8Bone;
    Procedure   AssignIndex(Var NewIndex: Integer);
    Function    FindHighestIndex: Integer;
    Procedure   GetBones(List: TStringList);
    Procedure   CalculateOrigin(X,Y,Z: Single);
    Function    IsAncestor(Bone: TAn8Bone): Boolean;
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
    Procedure   DeletePaintedWeight(Mesh: TAn8Mesh; Index: Integer);
    Procedure   ScaleLength(Scale: Single; Recurse: Boolean);
  End;
  TAn8Figure = Class
    Name      : String;
    Parent    : TAn8File;
    RootBone  : TAn8Bone;
    Materials : TStringList;    // Contains a list of TAn8Material
    Constructor Create(AParent: TAn8File);
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Function    FindBoneByName(St: String): TAn8Bone;
    Procedure   DeterminePrimaryBones;
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
    Function    ReferencesObject(Obj: TAn8Object): Boolean;
  End;
  TAn8Axis = (axX,axY,axZ);
  TAn8FloatKey = Class
    Frame    : Integer;
    Value    : Single;
    Unknown1 : Single;
    Unknown2 : Single;
    Key      : Boolean;
    Constructor Create;
  End;
  TAn8Track = Class
    Keys : TStringList;             // Contains a list of TAn8FloatKey
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
    Function    GetValueForFrame(WantedFrame: Integer): Single; // Interpolates if necessary
    Function    FindKeyForFrame(WantedFrame: Integer): TAn8FloatKey; // Returns Nil if no key exists for that frame
  End;
  TAn8Sequence = Class;
  TAn8JointAngle = Class
    Bone   : TAn8Bone;              // Reference
    Axis   : TAn8Axis;
    Track  : TAn8Track;
    Parent : TAn8Sequence;
    Constructor Create(AParent: TAn8Sequence);
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
  End;
  TAn8Sequence = Class
    Name        : String;
    Parent      : TAn8File;
    Figure      : TAn8Figure;       // Reference
    Frames      : Integer;
    JointAngles : TStringList;      // Contains a list of TAn8JointAngle
    Constructor Create(AParent: TAn8File);
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
    Function    GetRootBone(Fraction: Single): TAn8Bone;
    Procedure   WriteToFile(Var F: Text; Indent: Integer);
    Function    FindJointAngle(Bone: TAn8Bone): TAn8JointAngle; Overload;
    Function    FindJointAngle(BoneName: String; Axis: TAn8Axis): TAn8JointAngle; Overload;
    Procedure   CopyFrom(Src: TAn8Sequence);
  End;
  TAn8Header = Record
    Version : String;
    Build   : String;
  End;
  TAn8Grid = Record
    I1       : Integer;
    S2,S3,S4 : Single;
  End;
  TAn8Environment = Record
    Grid          : TAn8Grid;
    FrameRate     : Integer;
    LimitPlayback : Boolean;
  End;
  TAn8File = Class
    Header      : TAn8Header;
    Environment : TAn8Environment;
    Objects     : TStringList;      // Contains a list of TAn8Object
    Figures     : TStringList;      // Contains a list of TAn8Figure
    Sequences   : TStringList;      // Contains a list of TAn8Sequence
    Textures    : TStringList;      // Contains a list of TAn8Texture
    Materials   : TStringList;      // Contains a list of TAn8Material
    Loaded      : Boolean;
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   LoadFromFile(FileName: String);
    Procedure   SaveToFile(FileName: String);
    Procedure   Clear;
    Procedure   ParseData(St: String);
    Function    FindObjectByName(St: String): TAn8Object;
    Function    FindFigureByName(St: String): TAn8Figure;
    Procedure   GetBounds(MinPt,MaxPt: T3DPoint);
    Procedure   WriteToFile(Var F: Text);
  End;

  TDirectXMaterial = Class
    Name            : String;
    TextureFileName : String;
    Constructor Create; Overload;
    Constructor Create(Material: TDirectXMaterial); Overload;
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
  End;
  TDirectXFace = Class
    Material    : TDirectXMaterial;  // Reference
    Points      : Array Of Integer;
    Normals     : Array Of Integer;
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
  End;
  TDirectXMesh = Class
    Name         : String;
    Points       : TStringList;  // Contains a list of T3DPoint
    Normals      : TStringList;  // Contains a list of T3DPoint
    TexCoords    : TStringList;  // Contains a list of TAn8TexCoord
    Faces        : TStringList;  // Contains a list of TDirectXFace
    Materials    : TStringList;  // Contains a list of TDirectXMaterial
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
  End;
  TDirectXFrame = Class
    Name            : String;
    TransformMatrix : T4x4Matrix;
    Frames          : TStringList;  // Contains a list of TDirectXFrame
    Meshes          : TStringList;  // Contains a list of TDirectXMesh
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   ParseData(St: String);
  End;
  TDirectXFile = Class
    Frames : TStringList;           // Contains a list of TDirectXFrame
    Loaded : Boolean;
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   LoadFromFile(FileName: String);
    Procedure   Clear;
    Procedure   ParseData(St: String);
    Function    CreateAn8File: TAn8File;
  End;

implementation

Uses SysUtils,Math;

Function GetIndent(Indent: Integer): String;
Var
  S : String;
  I : Integer;

Begin
  SetLength(S,Indent);
  For I := 1 To Indent Do S[I] := ' ';
  Result := S;
End; // GetIndent

Procedure KillStringList(List: TStringList);
Var I: Integer;
Begin
  For I := 0 To List.Count - 1 Do List.Objects[I].Free;
  List.Free;
End; // KillStringList

Procedure ClearStringList(List: TStringList);
Var I: Integer;
Begin
  For I := 0 To List.Count - 1 Do List.Objects[I].Free;
  List.Clear;
End; // ClearStringList

Function GetQuotedString(Var St: String): String;
Var
  St1   : String;
  I     : Integer;
  Found : Boolean;

Begin
  St1 := '';
  St  := Trim(St);
  If (St <> '') And (St[1] = '"') Then
  Begin
    I     := 2;
    Found := False;
    While (I <= Length(St)) And Not Found Do
    Begin
      If St[I] = '"' Then Found := True Else Inc(I);
    End; // While
    If Found Then
    Begin
      St1 := Copy(St,2,I - 2);            // Strip of the double quotes
      St  := Trim(Copy(St,I + 1,Length(St)));
    End
    Else
    Begin
      // We should never get here, but if we do then punt and just get everything

      St1 := St;
      St  := '';
    End;
  End;
  Result := St1;
End; // GetQuotedString

Function GetFirstWordOrNumber(Var St: String): String;
Var
  St1 : String;
  I   : Integer;

Begin
  St1 := '';
  St  := Trim(St);

  If Copy(St,1,2) = '/*' Then
  Begin
    I := Pos('*/',St);
    If I > 1 Then St := Copy(St,I + 2,Length(St));
  End;

  If St <> '' Then
  Begin
    I := 1;
    While (I <= Length(St)) And (UpCase(St[I]) In ['0'..'9','A'..'Z']) Do Inc(I);
    St1 := Copy(St,1,I - 1);
    St  := Trim(Copy(St,I,Length(St)));
  End;
  Result := St1;
End; // GetFirstWordOrNumber

Function GetBracketedContents(Var St: String; DelimStart,DelimEnd: Char): String;
Var
  St1       : String;
  I         : Integer;
  Level     : Integer;
  InComment : Boolean;
  InString  : Boolean;

Begin
  St1 := '';
  St  := Trim(St);
  If (St <> '') And (St[1] = DelimStart) Then
  Begin
    // Find the terminating bracket, taking comments and double-quoted strings into account

    I         := 1;
    Level     := 0;
    InComment := False;
    InString  := False;
    Repeat
      // Strings take precedence, then comments, then anything else

      If Not InString Then
      Begin
        If Not InComment Then
        Begin
          If St[I] = DelimStart Then
          Begin
            Inc(Level);
            Inc(I);
          End
          Else If St[I] = DelimEnd Then
          Begin
            Dec(Level);
            Inc(I);
          End
          Else If St[I] = '"' Then
          Begin
            InString := True;
            Inc(I);
          End
          Else If (I < Length(St)) And (Copy(St,I,2) = '/*') Then
          Begin
            InComment := True;
            Inc(I,2);
          End
          Else Inc(I);
        End
        Else
        Begin
          If (I < Length(St)) And (Copy(St,I,2) = '*/') Then
          Begin
            InComment := False;
            Inc(I,2);
          End
          Else Inc(I);
        End;
      End
      Else
      Begin
        If St[I] = '"' Then InString := False;
        Inc(I);
      End;
    Until (I > Length(St)) Or (Level = 0);
    If Level = 0 Then
    Begin
      St1 := Trim(Copy(St,2,I - 3));
      St  := Trim(Copy(St,I,Length(St)));
    End
    Else
    Begin
      // We should never get here, but if we do then punt and just get everything

      St1 := St;
      St  := '';
      Raise Exception.Create('Error in GetBracketedContents(): no terminating brace found');
    End;
  End;
  Result := St1;
End; // GetBracketedContents

Function GetToken(Delim: String; Var St: String; TrimSpaces: Boolean): String;
Var I: Integer;
Begin
  If St <> '' Then
  Begin
    // Special handling for strings enclosed in double quotes

    If St[1] = '"' Then
    Begin
      St     := Copy(St,2,Length(St));
      Result := GetToken('"',St,TrimSpaces);
//      GetToken(Delim,St,TrimSpaces);
    End
    Else
    Begin
      I := Pos(Delim,St);
      If I <> 0 Then
      Begin
        If TrimSpaces Then
        Begin
          Result := Trim(Copy(St,1,I - 1));
          St     := Trim(Copy(St,I + 1,Length(St)));
        End
        Else
        Begin
          Result := Copy(St,1,I - 1);
          St     := Copy(St,I + 1,Length(St));
        End;
      End
      Else
      Begin
        If TrimSpaces
         Then Result := Trim(St)
         Else Result := St;
        St := '';
      End;
    End;
  End
  Else Result := '';
End; // GetToken

Procedure GetAn8Points(Var PointArray: TXYZPointArray; Var St: String);
Var
  I,J,K,L : Integer;
  St1     : String;

Begin
  SetLength(PointArray,0);

  // First find out how many points there are

  I := 1;
  J := 0;
  While I <= Length(St) Do
  Begin
    If St[I] = '(' Then Inc(J);
    Inc(I);
  End; // While

  // Allocate the points

  SetLength(PointArray,J);

  // Get the point data (using GetBracketedContents would make for cleaner code but this is more efficient)

  J := 1;
  L := 0;
  While J <= Length(St) Do
  Begin
    If St[J] = '(' Then
    Begin
      // Find the closing parenthesis

      K := J + 1;
      While (K <= Length(St)) And (St[K] <> ')') Do Inc(K);
      If (K <= Length(St)) And (St[K] = ')') Then
      Begin
        St1             := Trim(Copy(St,J + 1,K - J - 1));
        PointArray[L].X := StrToFloat(GetToken(' ',St1,True));
        PointArray[L].Y := StrToFloat(GetToken(' ',St1,True));
        PointArray[L].Z := StrToFloat(GetToken(' ',St1,True));
        Inc(L);
      End;
      J := K;
    End
    Else Inc(J);
  End; // While
End; // GetAn8Points

Procedure GetDXPoints(Var PointArray: TXYZPointArray; Var St: String);
Var
  I,J : Integer;
  St1 : String;

Begin
  SetLength(PointArray,0);

  // First find out how many points there are

  St1 := GetToken(';',St,True);
  Val(St1,J,I);

  // Allocate the points

  SetLength(PointArray,J);

  // Get the point data (using GetBracketedContents would make for cleaner code but this is more efficient)

  For I := 0 To J - 1 Do
  Begin
    PointArray[I].X := StrToFloat(GetToken(';',St,True));
    PointArray[I].Y := StrToFloat(GetToken(';',St,True));
    PointArray[I].Z := StrToFloat(GetToken(';',St,True));
    If I < J - 1
     Then GetToken(',',St,True)
     Else GetToken(';',St,True);
  End; // For I
End; // GetDXPoints

Procedure GetDXTexCoords(Var TexCoordArray: TTexCoordArray; Var St: String);
Var
  I,J : Integer;
  St1 : String;

Begin
  SetLength(TexCoordArray,0);

  // First find out how many points there are

  St1 := GetToken(';',St,True);
  Val(St1,J,I);

  // Allocate the texture coords

  SetLength(TexCoordArray,J);

  // Get the point data (using GetBracketedContents would make for cleaner code but this is more efficient)

  For I := 0 To J - 1 Do
  Begin
    TexCoordArray[I].TX := StrToFloat(GetToken(';',St,True));
    TexCoordArray[I].TZ := StrToFloat(GetToken(';',St,True));
    If I < J - 1
     Then GetToken(',',St,True)
     Else GetToken(';',St,True);
  End; // For I
End; // GetDXTexCoords

Procedure GetEdges(Var EdgeArray: TEdgeArray; Var St: String);
Var
  I,J,K,L : Integer;
  St1     : String;

Begin
  SetLength(EdgeArray,0);

  // First find out how many edges there are

  I := 1;
  J := 0;
  While I <= Length(St) Do
  Begin
    If St[I] = '(' Then Inc(J);
    Inc(I);
  End; // While

  // Allocate the edges

  SetLength(EdgeArray,J);

  // Get the edge data (using GetBracketedContents would make for cleaner code but this is more efficient)

  J := 1;
  L := 0;
  While J <= Length(St) Do
  Begin
    If St[J] = '(' Then
    Begin
      // Find the closing parenthesis

      K := J + 1;
      While (K <= Length(St)) And (St[K] <> ')') Do Inc(K);
      If (K <= Length(St)) And (St[K] = ')') Then
      Begin
        St1              := Trim(Copy(St,J + 1,K - J - 1));
        EdgeArray[L].Pt1 := StrToInt(GetToken(' ',St1,True));
        EdgeArray[L].Pt2 := StrToInt(GetToken(' ',St1,True));
        Inc(L);
      End;
      J := K;
    End
    Else Inc(J);
  End; // While
{
  J := 0;
  While St <> '' Do
  Begin
    If St[1] = '(' Then
    Begin
      // Find the closing parenthesis

      I := Pos(')',St);
      If I > 1 Then
      Begin
        St1 := Trim(Copy(St,2,I - 2));
        St  := Trim(Copy(St,I + 1,Length(St)));
        EdgeArray[J].Pt1 := StrToInt(GetToken(' ',St1,True));
        EdgeArray[J].Pt2 := StrToInt(GetToken(' ',St1,True));
        Inc(J);
      End
      Else St := ''; // Abort if we can't find a closing parenthesis
    End
    Else St := '';  // Abort if we encounter a problem
  End; // While
}
End; // GetEdges

Procedure GetAn8TexCoords(Var TexCoordArray: TTexCoordArray; Var St: String);
Var
  I,J,K,L : Integer;
  St1     : String;

Begin
  SetLength(TexCoordArray,0);

  // First find out how many texture coordinates there are

  I := 1;
  J := 0;
  While I <= Length(St) Do
  Begin
    If St[I] = '(' Then Inc(J);
    Inc(I);
  End; // While

  // Allocate the texcure coordinates

  SetLength(TexCoordArray,J);

  // Get the texture coordinate data (using GetBracketedContents would make for cleaner code but this is more efficient)

  J := 1;
  L := 0;
  While J <= Length(St) Do
  Begin
    If St[J] = '(' Then
    Begin
      // Find the closing parenthesis

      K := J + 1;
      While (K <= Length(St)) And (St[K] <> ')') Do Inc(K);
      If (K <= Length(St)) And (St[K] = ')') Then
      Begin
        St1                 := Trim(Copy(St,J + 1,K - J - 1));
        TexCoordArray[L].TX := StrToFloat(GetToken(' ',St1,True));
        TexCoordArray[L].TZ := StrToFloat(GetToken(' ',St1,True));
        Inc(L);
      End;
      J := K;
    End
    Else Inc(J);
  End; // While
{
  J := 0;
  While St <> '' Do
  Begin
    If St[1] = '(' Then
    Begin
      // Find the closing parenthesis

      I := Pos(')',St);
      If I > 1 Then
      Begin
        St1 := Trim(Copy(St,2,I - 2));
        St  := Trim(Copy(St,I + 1,Length(St)));
        TexCoordArray[J].TX := StrToFloat(GetToken(' ',St1,True));
        TexCoordArray[J].TZ := StrToFloat(GetToken(' ',St1,True));
        Inc(J);
      End
      Else St := ''; // Abort if we can't find a closing parenthesis
    End
    Else St := '';  // Abort if we encounter a problem
  End; // While
}
End; // GetAn8TexCoords

// ------------------------
// TAn8Texture
// ------------------------

Constructor TAn8Texture.Create(AParent: TAn8File);
Begin
  Name     := '';
  FileName := '';
  Parent   := AParent;
End; // TAn8Texture.Create

Procedure TAn8Texture.ParseData(St: String);
Var
  St1,St2 : String;
  I       : Integer;

Begin
  St1  := GetQuotedString(St);
  Name := St1;
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'file' Then
    Begin
      FileName := GetQuotedString(St2); // Strip off the double quotes
      I := Pos('\\',FileName);
      While I > 0 Do
      Begin
        Delete(FileName,I,1);
        I := Pos('\\',FileName);
      End; // While
    End;
  End; // While
End; // TAn8Texture.ParseData

Procedure TAn8Texture.WriteToFile(Var F: Text; Indent: Integer);
Var
  IndentStr : String;
  NewFile   : String;
  I         : Integer;

Begin
  IndentStr := GetIndent(Indent);
  NewFile   := FileName;
  I         := 1;
  While I <= Length(NewFile) Do
  Begin
    If NewFile[I] = '\' Then
    Begin
      Insert('\',NewFile,I);
      Inc(I);
    End;
    Inc(I);
  End; // While
  WriteLn(F,IndentStr + 'texture { "' + Name + '"');
  WriteLn(F,IndentStr + '  file { "' + NewFile + '" }');
  WriteLn(F,IndentStr + '}');
End; // TAn8Texture.WriteToFile

// ------------------------
// TAn8TextureParams
// ------------------------

Constructor TAn8TextureParams.Create;
Begin
  BlendMode := abmDecal;
  AlphaMode := aamNone;
  Percent   := 100;
End; // TAn8TextureParams.Create

Procedure TAn8TextureParams.ParseData(St: String);
Var St1,St2: String;
Begin
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'blendmode' Then
    Begin
           If St2 = 'decal'   Then BlendMode := abmDecal
      Else If St2 = 'darken'  Then BlendMode := abmDarken
      Else If St2 = 'lighten' Then BlendMode := abmLighten;
    End
    Else If St1 = 'alphamode' Then
    Begin
           If St2 = 'none'  Then AlphaMode := aamNone
      Else If St2 = 'layer' Then AlphaMode := aamLayer
      Else If St2 = 'final' Then AlphaMode := aamFinal;
    End
    Else If St1 = 'percent' Then Percent := StrToInt(St2);
  End; // While
End; // TAn8TextureParams.ParseData

Procedure TAn8TextureParams.WriteToFile(Var F: Text; Indent: Integer);
Var IndentStr : String;
Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'textureparams {');
  Write(F,IndentStr + '  blendmode { ');
  Case BlendMode Of
    abmDecal:   Write(F,'decal');
    abmDarken:  Write(F,'darken');
    abmLighten: Write(F,'lighten');
  End; // Case
  WriteLn(F,' }');
  Write(F,IndentStr + '  alphamode { ');
  Case AlphaMode Of
    aamNone:  Write(F,'none');
    aamLayer: Write(F,'layer');
    aamFinal: Write(F,'final');
  End; // Case
  WriteLn(F,' }');
  WriteLn(F,IndentStr + '  percent { ' + IntToStr(Percent) + ' }');
  WriteLn(F,IndentStr + '}');
End; // TAn8TextureParams.WriteToFile

// ------------------------
// TAn8Color
// ------------------------

Constructor TAn8Color.Create(AFile: TAn8File; AColorType: TAn8ColorType);
Begin
  RGB           := 0;
  Factor        := 1;
  Texture       := Nil;
  ColorType     := AColorType;
  An8File       := AFile;
  TextureParams := Nil;
End; // TAn8Color.Create

Destructor TAn8Color.Destroy;
Begin
  If TextureParams <> Nil Then TextureParams.Free;
  Inherited;
End; // TAn8Color.Destroy

Procedure TAn8Color.ParseData(St: String);
Var
 St1,St2 : String;
 I       : Integer;
 
Begin
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'rgb' Then
    Begin
      TRGBA(RGB).R := StrToInt(GetToken(' ',St2,True));
      TRGBA(RGB).G := StrToInt(GetToken(' ',St2,True));
      TRGBA(RGB).B := StrToInt(GetToken(' ',St2,True));
      TRGBA(RGB).A := 255;
    End
    Else If St1 = 'factor' Then Factor := StrToFloat(St2)
    Else If St1 = 'texturename' Then
    Begin
      St2 := GetQuotedString(St2);
      I := An8File.Textures.IndexOf(St2);
      If I >= 0 Then Texture := TAn8Texture(An8File.Textures.Objects[I])
      Else
      Begin
        I := An8File.Textures.IndexOf(St2);
        If I >= 0
         Then Texture := TAn8Texture(An8File.Textures.Objects[I])
         Else Texture := Nil;
      End;
    End
    Else If St1 = 'textureparams' Then
    Begin
      TextureParams := TAn8TextureParams.Create;
      TextureParams.ParseData(St2);
    End;
  End; // While
End; // TAn8Color.ParseData

Procedure TAn8Color.WriteToFile(Var F: Text; Indent: Integer);
Var IndentStr : String;
Begin
  IndentStr := GetIndent(Indent);
  Case ColorType Of
    actAmbient:  WriteLn(F,IndentStr + 'ambiant {');
    actDiffuse:  WriteLn(F,IndentStr + 'diffuse {');
    actSpecular: WriteLn(F,IndentStr + 'specular {');
    actEmissive: WriteLn(F,IndentStr + 'emissive {');
  End;
  WriteLn(F,IndentStr + '  rgb { ' + IntToStr(TRGBA(RGB).R) + ' ' + IntToStr(TRGBA(RGB).G) + ' ' + IntToStr(TRGBA(RGB).B) + ' }');
  WriteLn(F,IndentStr + '  factor { ' + Format('%7.5f',[Factor]) + ' }');
  If Texture <> Nil Then WriteLn(F,IndentStr + '  texturename { "' + Texture.Name + '" }');
  If TextureParams <> Nil Then TextureParams.WriteToFile(F,Indent + 2);
  WriteLn(F,IndentStr + '}');
End; // TAn8Color.WriteToFile

// ------------------------
// TAn8Surface
// ------------------------

Constructor TAn8Surface.Create(AFile: TAn8File);
Begin
  RGB                := 0;
  LockAmbientDiffuse := False;
  Ambient            := Nil;
  Diffuse            := Nil;
  Specular           := Nil;
  Emissive           := Nil;
  PhongSize          := 0;
  Alpha              := 255;
  Brilliance         := 1;
  An8File            := AFile;
End; // TAn8Surface.Create

Destructor TAn8Surface.Destroy;
Begin
  If Ambient  <> Nil Then Ambient.Free;
  If Diffuse  <> Nil Then Diffuse.Free;
  If Specular <> Nil Then Specular.Free;
  If Emissive <> Nil Then Emissive.Free;
  Inherited;
End; // TAn8Surface.Destroy

Procedure TAn8Surface.ParseData(St: String);
Var St1,St2: String;

  Procedure LoadColor(Var C: TAn8Color; St: String; ColorType: TAn8ColorType);
  Begin
    C := TAn8Color.Create(An8File,ColorType);
    C.ParseData(St);
  End; // LoadColor

Begin
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'rgb' Then
    Begin
      TRGBA(RGB).R := StrToInt(GetToken(' ',St2,True));
      TRGBA(RGB).G := StrToInt(GetToken(' ',St2,True));
      TRGBA(RGB).B := StrToInt(GetToken(' ',St2,True));
      TRGBA(RGB).A := 255;
    End
    Else If St1 = 'lockambiantdiffuse' Then LockAmbientDiffuse := True
    Else If St1 = 'ambiant'            Then LoadColor(Ambient,St2,actAmbient)
    Else If St1 = 'diffuse'            Then LoadColor(Diffuse,St2,actDiffuse)
    Else If St1 = 'specular'           Then LoadColor(Specular,St2,actSpecular)
    Else If St1 = 'emissive'           Then LoadColor(Emissive,St2,actEmissive)
    Else If St1 = 'brilliance'         Then Brilliance := StrToFloat(St2)
    Else If St1 = 'alpha'              Then Alpha      := StrToInt(St2)
    Else If St1 = 'phongsize'          Then PhongSize  := StrToFloat(St2);
  End; // While
End; // TAn8Surface.ParseData

Procedure TAn8Surface.WriteToFile(Var F: Text; Indent: Integer);
Var IndentStr : String;
Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'surface {');
  WriteLn(F,IndentStr + '  /* RGB chunk no longer used */');
  WriteLn(F,IndentStr + '  rgb { ' + IntToStr(TRGBA(RGB).R) + ' ' + IntToStr(TRGBA(RGB).G) + ' ' + IntToStr(TRGBA(RGB).B) + ' }');
  If LockAmbientDiffuse Then WriteLn(F,IndentStr + '  lockambiantdiffuse { }');
  WriteLn(F,IndentStr + '  alpha { ' + IntToStr(Alpha) + ' }');
  If Ambient  <> Nil Then Ambient.WriteToFile(F,Indent + 2);
  If Diffuse  <> Nil Then Diffuse.WriteToFile(F,Indent + 2);
  If Specular <> Nil Then Specular.WriteToFile(F,Indent + 2);
  If Emissive <> Nil Then Emissive.WriteToFile(F,Indent + 2);
  WriteLn(F,IndentStr + '}');
End; // TAn8Surface.WriteToFile

// ------------------------
// TAn8Material
// ------------------------

Constructor TAn8Material.Create(AParent: TAn8Object);
Begin
  Name    := '';
  Parent  := AParent;
  An8File := AParent.Parent;
  Surface := Nil;
End; // TAn8Material.Create

Constructor TAn8Material.Create(AFile: TAn8File);
Begin
  Name    := '';
  Parent  := Nil;
  An8File := AFile;
  Surface := Nil;
End; // TAn8Material.Create

Destructor TAn8Material.Destroy;
Begin
  If Surface <> Nil Then Surface.Free;
  Inherited;
End; // TAn8Material.Destroy

Function TAn8Material.ReferencesTexture(Texture: TAn8Texture): Boolean;
Begin
  Result := (Surface <> Nil) And
            (((Surface.Ambient <> Nil) And (Surface.Ambient.Texture = Texture)) Or
             ((Surface.Diffuse <> Nil) And (Surface.Diffuse.Texture = Texture)) Or
             ((Surface.Specular <> Nil) And (Surface.Specular.Texture = Texture)) Or
             ((Surface.Emissive <> Nil) And (Surface.Emissive.Texture = Texture)));
End; // TAn8Material.ReferencesTexture

Procedure TAn8Material.ParseData(St: String);
Var St1,St2: String;
Begin
  St1  := GetQuotedString(St);
  Name := St1;
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'surface' Then
    Begin
      Surface := TAn8Surface.Create(An8File);
      Surface.ParseData(St2);
    End;
  End; // While
End; // TAn8Material.ParseData

Procedure TAn8Material.WriteToFile(Var F: Text; Indent: Integer);
Var IndentStr: String;
Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'material { "' + Name + '"');
  If Surface <> Nil then Surface.WriteToFile(F,Indent + 2);
  WriteLn(F,IndentStr + '}');
End; // TAn8Material.WriteToFile

// ------------------------
// TAn8TexCoord
// ------------------------

Constructor TAn8TexCoord.Create;
Begin
  TX := 0;
  TZ := 0;
End; // TAn8TexCoord.Create

Constructor TAn8TexCoord.Create(TexCoord: TAn8TexCoord);
Begin
  TX := TexCoord.TX;
  TZ := TexCoord.TZ;
End; // TAn8TexCoord.Create

// ------------------------
// TAn8Edge
// ------------------------

Constructor TAn8Edge.Create;
Begin
  Pt1 := 0;
  Pt2 := 0;
End; // TAn8Edge.Create

Constructor TAn8Edge.Create(Edge: TAn8Edge);
Begin
  Pt1 := Edge.Pt1;
  Pt2 := Edge.Pt2;
End; // TAn8Edge.Create

// ------------------------
// TAn8Face
// ------------------------

Constructor TAn8Face.Create;
Begin
  Material := Nil;
  SetLength(Points,0);
  SetLength(TexCoords,0);
  NormalIndex := 0;
End; // TAn8Face.Create

Constructor TAn8Face.Create(Face: TAn8Face);
Var I: Integer;
Begin
  Material := Face.Material;
  SetLength(Points,High(Face.Points) + 1);
  SetLength(TexCoords,High(Face.TexCoords) + 1);
  NormalIndex := Face.NormalIndex;
  For I := 0 To High(Points) Do Points[I] := Face.Points[I];
  For I := 0 To High(TexCoords) Do TexCoords[I] := Face.TexCoords[I];
End; // TAn8Face.Create

Destructor TAn8Face.Destroy;
Begin
  SetLength(Points,0);
  SetLength(TexCoords,0);
  Inherited;
End; // TAn8Face.Destroy

Function TAn8Face.ReferencesPointIndex(Index: Integer): Boolean;
Var
  I     : Integer;
  Found : Boolean;

Begin
  I     := 0;
  Found := False;
  While (I <= High(Points)) And Not Found Do
  Begin
    If Points[I] = Index
     Then Found := True
     Else Inc(I);
  End; // While
End; // TAn8Face.ReferencesPointIndex

// ------------------------
// TAn8Component
// ------------------------

Constructor TAn8Component.Create(AName: String; AParent: TAn8Object; AGroup: TAn8Group);
Begin
  Name   := AName;
  Parent := AParent;
  Group  := AGroup;
End; // TAn8Component.Create

Destructor TAn8Component.Destroy;
Begin
  Inherited;
End; // TAn8Component.Destroy

// ------------------------
// TAn8OriginOrientation
// ------------------------

Constructor TAn8OriginOrientation.Create(AName: String);
Begin
  Name        := AName;
  Origin      := T3DPoint.Create;
  Orientation := TQuaternion.Create;
End; // TAn8OriginOrientation.Create

Destructor TAn8OriginOrientation.Destroy;
Begin
  Origin.Free;
  Orientation.Free;
  Inherited;
End; // TAn8OriginOrientation.Destroy

Procedure TAn8OriginOrientation.ParseData(St: String);
Var
  St1,St2 : String;
  Points  : TXYZPointArray;

Begin
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'origin' Then
    Begin
      GetAn8Points(Points,St2);
      If High(Points) >= 0 Then
      Begin
        Origin.X := Points[0].X;
        Origin.Y := Points[0].Y;
        Origin.Z := Points[0].Z;
      End;
      SetLength(Points,0);
    End
    Else If St1 = 'orientation' Then
    Begin
      St2           := Copy(St2,2,Length(St2) - 2);
      Orientation.X := StrToFloat(GetToken(' ',St2,True));
      Orientation.Y := StrToFloat(GetToken(' ',St2,True));
      Orientation.Z := StrToFloat(GetToken(' ',St2,True));
      Orientation.W := StrToFloat(GetToken(' ',St2,True));
    End;
  End; // While
End; // TAn8OriginOrientation.ParseData

Procedure TAn8OriginOrientation.WriteToFile(Var F: Text; Indent: Integer);
Var IndentStr: String;
Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + Name + ' {');
  WriteLn(F,IndentStr + Format('  origin { (%7.5f %7.5f %7.5f) }',[Origin.X,Origin.Y,Origin.Z]));
  WriteLn(F,IndentStr + Format('  orientation { (%7.5f %7.5f %7.5f %7.5f) }',[Orientation.X,Orientation.Y,Orientation.Z,Orientation.W]));
  WriteLn(F,IndentStr + '}');
End; // TAn8OriginOrientation.WriteToFile

// ------------------------
// TAn8Group
// ------------------------

Constructor TAn8Group.Create(AFile: TAn8File; AName: String; AParent: TAn8Object; AGroup: TAn8Group);
Begin
  Inherited Create(AName, AParent, AGroup);
  An8File    := AFile;
  Base       := TAn8OriginOrientation.Create('base');
  Pivot      := TAn8OriginOrientation.Create('pivot');
  Components := TStringList.Create
End; // TAn8Group.Create

Destructor TAn8Group.Destroy;
Begin
  Base.Free;
  Pivot.Free;
  KillStringList(Components);
  Inherited;
End; // TAn8Group.Destroy

Procedure TAn8Group.ParseData(St: String);
Var St1,St2: String;
Begin
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
         If St1 = 'name'         Then Name := Copy(St2,2,Length(St2) - 2)
    Else If St1 = 'base'         Then Base.ParseData(St2)
    Else If St1 = 'pivot'        Then Pivot.ParseData(St2)
    Else If St1 = 'mesh' Then
    Begin
      Components.AddObject('',TAn8Mesh.Create(An8File,'',Self.Parent,Self));
      TAn8Mesh(Components.Objects[Components.Count - 1]).ParseData(St2);
      Components.Strings[Components.Count - 1] := TAn8Mesh(Components.Objects[Components.Count - 1]).Name;
    End
    Else If St1 = 'group' Then
    Begin
      Components.AddObject('',TAn8Group.Create(An8File,'',Self.Parent,Self));
      TAn8Group(Components.Objects[Components.Count - 1]).ParseData(St2);
      Components.Strings[Components.Count - 1] := TAn8Group(Components.Objects[Components.Count - 1]).Name;
    End;
  End; // While
End; // TAn8Group.ParseData

Procedure TAn8Group.WriteToFile(Var F: Text; Indent: Integer);
Var
  IndentStr : String;
  I         : Integer;
  Component : TAn8Component;

Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'group {');
  WriteLn(F,IndentStr + '  name { "' + Name + '" }');
  Base.WriteToFile(F,Indent + 2);
  Pivot.WriteToFile(F,Indent + 2);
  For I := 0 To Components.Count - 1 Do
  Begin
    Component := TAn8Component(Components.Objects[I]);
    If Component <> Nil Then
    Begin
           If Component Is TAn8Mesh  Then TAn8Mesh(Component).WriteToFile(F,Indent + 2)
      Else If Component Is TAn8Group Then TAn8Group(Component).WriteToFile(F,Indent + 2);
    End;
  End; // For I
  WriteLn(F,IndentStr + '}');
End; // TAn8Group.WriteToFile

Procedure TAn8Group.MergeVeryClosePoints;
Var
  I,J   : Integer;
  Mesh  : TAn8Mesh;
  Point : T3DPoint;
  Obj   : TObject;

Begin
  For I := 0 To Components.Count - 1 Do
  Begin
    Obj := Components.Objects[I];
    If Obj Is TAn8Mesh Then
    Begin
      Mesh := TAn8Mesh(Obj);
      For J := 0 To Mesh.Points.Count - 1 Do
      Begin
        Point   := T3DPoint(Mesh.Points.Objects[J]);
        Point.X := Round(Point.X * 1000) / 1000;
        Point.Y := Round(Point.Y * 1000) / 1000;
        Point.Z := Round(Point.Z * 1000) / 1000;
        Mesh.Points.Strings[J] := Point.GetCloseID(100);
      End; // For J
    End
    Else If Obj Is TAn8Group Then TAn8Group(Obj).MergeVeryClosePoints;
  End; // For I
End; // TAn8Group.MergeVeryClosePoints

Procedure TAn8Group.DeterminePrimaryBones(Obj: TAn8NamedObject; RootBone: TAn8Bone);
Var
  I         : Integer;
  Component : TObject;

Begin
  For I := 0 To Components.Count - 1 Do
  Begin
    Component := Components.Objects[I];
    If Component Is TAn8Component Then TAn8Component(Component).DeterminePrimaryBones(Obj,RootBone);
  End; // For I
End; // TAn8Group.DeterminePrimaryBones

Procedure TAn8Group.ConvertToTriangles;
Var
  I         : Integer;
  Component : TObject;

Begin
  For I := 0 To Components.Count - 1 Do
  Begin
    Component := Components.Objects[I];
    If Component Is TAn8Component Then TAn8Component(Component).ConvertToTriangles;
  End; // For I
End; // TAn8Group.ConvertToTriangles

Procedure TAn8Group.GetBounds(MinPt,MaxPt: T3DPoint);
Var
  I         : Integer;
  Component : TObject;
  Min1,Max1 : T3DPoint;
  First     : Boolean;

Begin
  Min1  := T3DPoint.Create;
  Max1  := T3DPoint.Create;
  First := True;
  For I := 0 To Components.Count - 1 Do
  Begin
    Component := Components.Objects[I];
    If Component Is TAn8Component Then
    Begin
      TAn8Component(Component).GetBounds(Min1,Max1);
      If First Then
      Begin
        MinPt.Copy(Min1);
        MaxPt.Copy(Max1);
        First := False;
      End
      Else
      Begin
        If Min1.X < MinPt.X Then MinPt.X := Min1.X;
        If Min1.Y < MinPt.Y Then MinPt.Y := Min1.Y;
        If Min1.Z < MinPt.Z Then MinPt.Z := Min1.Z;
        If Max1.X > MaxPt.X Then MaxPt.X := Max1.X;
        If Max1.Y > MaxPt.Y Then MaxPt.Y := Max1.Y;
        If Max1.Z > MaxPt.Z Then MaxPt.Z := Max1.Z;
      End;
    End;
  End; // For I
  Max1.Free;
  Min1.Free;
End; // TAn8Group.GetBounds

Procedure TAn8Group.FindAppropriateBone(Figure: TAn8Figure; Mesh: TAn8Mesh; PointIndex: Integer; Var CandidateBone: TAn8Bone);
Var
  I         : Integer;
  Component : TObject;

Begin
  For I := 0 To Components.Count - 1 Do
  Begin
    Component := Components.Objects[I];
    If Component Is TAn8Component Then TAn8Component(Component).FindAppropriateBone(Figure,Mesh,PointIndex,CandidateBone);
  End; // For I
End; // TAn8Group.FindAppropriateBone

Function TAn8Group.FindMeshByName(St: String): TAn8Mesh;
Var
  I         : Integer;
  Component : TAn8Component;

Begin
  I      := 0;
  Result := Nil;
  While (I < Components.Count) And (Result = Nil) Do
  Begin
    Component := TAn8Component(Components.Objects[I]);
    If Component <> Nil Then
    Begin
           If Component Is TAn8Group Then Result := TAn8Group(Component).FindMeshByName(St)
      Else If (Component Is TAn8Mesh) And (TAn8Mesh(Component).Name = St) Then Result := TAn8Mesh(Component);
    End;
    Inc(I);
  End; // While
End; // TAn8Group.FindMeshByName

// ------------------------
// TAn8Mesh
// ------------------------

Constructor TAn8Mesh.Create(AFile: TAn8File; AName: String; AParent: TAn8Object; AGroup: TAn8Group);
Begin
  Inherited Create(AName, AParent, AGroup);
  An8File      := AFile;
  Base         := TAn8OriginOrientation.Create('base');
  Pivot        := TAn8OriginOrientation.Create('pivot');
  Material     := Nil;
  Parent       := AParent;
  SmoothAngle  := 45;
  MaterialList := TStringList.Create;
  Points       := TStringList.Create;
  Normals      := TStringList.Create;
  Edges        := TStringList.Create;
  TexCoords    := TStringList.Create;
  Faces        := TStringList.Create;
  FaceNormals  := TStringList.Create;
  PrimaryBones := TStringList.Create;
End; // TAn8Mesh.Create

Destructor TAn8Mesh.Destroy;
Begin
  Base.Free;
  Pivot.Free;
  MaterialList.Free;
  KillStringList(Points);
  KillStringList(Normals);
  KillStringList(Edges);
  KillStringList(TexCoords);
  KillStringList(Faces);
  KillStringList(FaceNormals);
  PrimaryBones.Free;
  Inherited;
End; // TAn8Mesh.Destroy

Procedure TAn8Mesh.DeterminePrimaryBones(Obj: TAn8NamedObject; RootBone: TAn8Bone);
Var
  CurPos      : T3DPoint;
  J,L,M       : Integer;
  MeshWeights : TAn8PaintedWeights;
  Weight      : TAn8VertexWeight;
  Influence   : Single;
  Point       : T3DPoint;
  Bone        : TAn8Bone;

Begin
  CurPos := T3DPoint.Create;
  PrimaryBones.Clear;

  // First see if the named object contains weighting values for the mesh
  // that were set with the weight painting feature in Anim8or

  L := Obj.Weights.IndexOf(Name);
  If L >= 0 Then
  Begin
    MeshWeights := TAn8PaintedWeights(Obj.Weights.Objects[L]);
    For L := 0 To MeshWeights.Weights.Count - 1 Do
    Begin
      Weight := TAn8VertexWeight(MeshWeights.Weights.Objects[L]);

      // Find the bone with the highest influence 

      M := Weight.HighestInfluenceIndex;
      If M >= 0 Then
      Begin
        M := Weight.BoneIndices[M];

        // Find the corresponding bone

        If (M >= 0) And (M < Obj.WeightedBy.Count) Then PrimaryBones.AddObject('',TAn8Bone(Obj.WeightedBy.Objects[M]));
      End;
    End; // For L
  End
  Else
  Begin
    Point := T3DPoint.Create;
    For J := 0 To Points.Count - 1 Do
    Begin
      Influence := 0;
      Point.Copy(T3DPoint(Points.Objects[J]));
      Point.Add(Obj.Base);
      CurPos.Copy(0,0,0);
      Bone := RootBone.FindStrongestBone(CurPos,Obj,Point,Influence);
      PrimaryBones.AddObject('',Bone);
    End;
    Point.Free;
  End; // For J
  CurPos.Free;
End; // TAn8Mesh.DeterminePrimaryBones

Procedure TAn8Mesh.GetBounds(MinPt,MaxPt: T3DPoint);
Var
  I : Integer;
  P : T3DPoint;

Begin
  For I := 0 To Points.Count - 1 Do
  Begin
    P := T3DPoint(Points.Objects[I]);
    If I = 0 Then
    Begin
      MinPt.Copy(P);
      MaxPt.Copy(P);
    End
    Else
    Begin
      If P.X < MinPt.X Then MinPt.X := P.X;
      If P.Y < MinPt.Y Then MinPt.Y := P.Y;
      If P.Z < MinPt.Z Then MinPt.Z := P.Z;
      If P.X > MaxPt.X Then MaxPt.X := P.X;
      If P.Y > MaxPt.Y Then MaxPt.Y := P.Y;
      If P.Z > MaxPt.Z Then MaxPt.Z := P.Z;
    End;
  End; // For I
End; // TAn8Mesh.GetBounds

Function TAn8Mesh.GetMostUsedBone: TAn8Bone;
Var
  Highest : Integer;
  Bone    : TAn8Bone;
  HiBone  : TAn8Bone;
  I,J     : Integer;
  Counts  : Array Of Integer;

Begin
  Highest := 0;
  HiBone  := Nil;
  SetLength(Counts,0);
  For I := 0 To PrimaryBones.Count - 1 Do
  Begin
    Bone := TAn8Bone(PrimaryBones.Objects[I]);
    If Bone.Index > High(Counts) Then
    Begin
      J := High(Counts) + 1;
      SetLength(Counts,Bone.Index + 1);
      While J <= High(Counts) Do
      Begin
        Counts[J] := 0;
        Inc(J);
      End; // While
    End;
    Inc(Counts[Bone.Index]);
    If Counts[Bone.Index] > Highest Then
    Begin
      Highest := Counts[Bone.Index];
      HiBone  := Bone;
    End;
  End; // For I
  SetLength(Counts,0);
  Result := HiBone;
End; // TAn8Mesh.GetMostUsedBone

Procedure TAn8Mesh.Add(Mesh: TAn8Mesh);
Var
  I,J,K,L : Integer;
  Face    : TAn8Face;
  Edge    : TAn8Edge;
  P       : T3DPoint;
  PI      : Integer;

Begin
  For I := 0 To Mesh.MaterialList.Count - 1 Do
  Begin
    If MaterialList.IndexOfObject(Mesh.MaterialList.Objects[I]) < 0 Then MaterialList.AddObject('',Mesh.MaterialList.Objects[I]);
  End; // For I
  J := Points.Count;
  L := TexCoords.Count;

  For I := 0 To Mesh.Faces.Count - 1 Do
  Begin
    Face := TAn8Face(Mesh.Faces.Objects[I]);
    For K := 0 To High(Face.Points) Do
    Begin
      P := T3DPoint.Create(T3DPoint(Mesh.Points.Objects[Face.Points[K]]));
      Mesh.Base.Orientation.Transform(P);
      P.Add(Mesh.Base.Origin);
      Points.AddObject(P.GetCloseID(100),P);

      P := T3DPoint.Create(T3DPoint(Mesh.Normals.Objects[Face.Points[K]]));
      Mesh.Base.Orientation.Transform(P);
      P.Normalize;
      Normals.AddObject(P.GetCloseID(100),P);

      PI := Face.Points[K];

      // Failsafe: If the mesh uses vertex weights and they have not been all assigned, it can have less
      // PrimaryBones objects than points.  In this case, default to the root bone.  The user will
      // eventually see that something is wrong with the object and will hopefully check the weights :)
      // This can happen if points are added to a mesh after weights have been assigned.

      If PI >= Mesh.PrimaryBones.Count Then PI := 0;

      PrimaryBones.AddObject(Mesh.PrimaryBones.Strings[PI],Mesh.PrimaryBones.Objects[PI]);
    End; // For K
    For K := 0 To High(Face.TexCoords) Do
     TexCoords.AddObject(Mesh.TexCoords.Strings[Face.TexCoords[K]],TAn8TexCoord.Create(TAn8TexCoord(Mesh.TexCoords.Objects[Face.TexCoords[K]])));
  End; // For I
{
  For I := 0 To Mesh.Points.Count - 1 Do
  Begin
    Points.AddObject(Mesh.Points.Strings[I],T3DPoint.Create(T3DPoint(Mesh.Points.Objects[I])));
    Normals.AddObject(Mesh.Normals.Strings[I],T3DPoint.Create(T3DPoint(Mesh.Normals.Objects[I])));
    TexCoords.AddObject(Mesh.TexCoords.Strings[I],TAn8TexCoord.Create(TAn8TexCoord(Mesh.TexCoords.Objects[I])));
    PrimaryBones.AddObject(Mesh.PrimaryBones.Strings[I],Mesh.PrimaryBones.Objects[I]);
  End; // For I
}
  For I := 0 To Mesh.Edges.Count - 1 Do
  Begin
    Edge := TAn8Edge.Create(TAn8Edge(Mesh.Edges.Objects[I]));
    Inc(Edge.Pt1,J);
    Inc(Edge.Pt2,J);
    Edges.AddObject(Mesh.Edges.Strings[I],Edge);
  End;
  For I := 0 To Mesh.Faces.Count - 1 Do
  Begin
    Face := TAn8Face.Create(TAn8Face(Mesh.Faces.Objects[I]));
    For K := 0 To High(Face.Points) Do
    Begin
      Face.Points[K] := J;
      Inc(J);
    End; // For K
    For K := 0 To High(Face.TexCoords) Do
    Begin
      Face.TexCoords[K] := L;
      Inc(L);
    End; // For K
    Faces.AddObject(Mesh.Faces.Strings[I],Face);
    P := T3DPoint.Create(T3DPoint(Mesh.FaceNormals.Objects[I]));
    Mesh.Base.Orientation.Transform(P);
    P.Normalize;
    FaceNormals.AddObject(Mesh.FaceNormals.Strings[I],P);
  End; // For I
End; // TAn8Mesh.Add

Procedure TAn8Mesh.CalculateFaceNormals;
Var
  I    : Integer;
  Norm : T3DPoint;
  Face : TAn8Face;

Begin
  // Clear the normals list

  ClearStringList(FaceNormals);
  For I := 0 To Faces.Count - 1 Do
  Begin
    Norm := T3DPoint.Create;
    FaceNormals.AddObject('',Norm);
    Face := TAn8Face(Faces.Objects[I]);
    If High(Face.Points) >= 2 Then
    Begin
      Norm.GetNormalTo(T3DPoint(Points.Objects[Face.Points[0]]),
                       T3DPoint(Points.Objects[Face.Points[1]]),
                       T3DPoint(Points.Objects[Face.Points[2]]));

      If Norm.IsZero Then
      Begin
        Norm.Copy(1,0,0);
      End;
    End;
  End; // For I
End; // TAn8Mesh.CalculateFaceNormals

Procedure TAn8Mesh.SplitPoints;
Const CloseToOpposite = -0.9;
Var
  I,J,K,L   : Integer;
  FaceList  : TStringList;
  List      : TStringList;
  Face      : TAn8Face;
  FaceNorm1 : T3DPoint;
  FaceNorm2 : T3DPoint;
  Opposite  : Boolean;
  Dot       : Single;

Begin
  // For each point, make a list of all faces that reference it

  FaceList := TStringList.Create;
  For I := 0 To Points.Count - 1 Do
  Begin
    List := TStringList.Create;
    FaceList.AddObject(IntToStr(I),List);
  End; // For I
  For I := 0 To Faces.Count - 1 Do
  Begin
    Face := TAn8Face(Faces.Objects[I]);
    For J := 0 To High(Face.Points) Do
    Begin
      TStringList(FaceList.Objects[Face.Points[J]]).AddObject('',Pointer(I)); // Save the face index, not the face itself
    End; // For J
  End; // For I

  // For each point, look for any faces with opposite or nearly opposite normals.  If there are none,
  // remove that point from the list.

  I := 0;
  While I < FaceList.Count Do
  Begin
    Opposite := False;
    List     := TStringList(FaceList.Objects[I]);
    J        := 0;
    While (J < List.Count) And Not Opposite Do
    Begin
      FaceNorm1 := T3DPoint(FaceNormals.Objects[Integer(List.Objects[J])]);
      K := J + 1;
      While (K < List.Count) And Not Opposite Do
      Begin
        FaceNorm2 := T3DPoint(FaceNormals.Objects[Integer(List.Objects[K])]);
        Dot       := FaceNorm1.Dot(FaceNorm2);
        If Dot < CloseToOpposite Then Opposite := True;
        Inc(K);
      End; // While
      Inc(J);
    End; // While

    If Not Opposite Then
    Begin
      List.Free;
      FaceList.Delete(I);
    End
    Else Inc(I);
  End; // While

  // For those points that are left, split them up (one for each face)

  For I := 0 To FaceList.Count - 1 Do
  Begin
    J    := StrToInt(FaceList.Strings[I]);
    List := TStringList(FaceList.Objects[I]);

    // Leave the first face alone

    For K := 1 To List.Count - 1 Do
    Begin
      Face := TAn8Face(Faces.Objects[Integer(List.Objects[K])]);
      Points.AddObject(Points.Strings[J],T3DPoint.Create(T3DPoint(Points.Objects[J])));
      For L := 0 To High(Face.Points) Do
      Begin
        If Face.Points[L] = J Then Face.Points[L] := Points.Count - 1;
      End; // For L
    End; // For K
  End; // For I

  // Cleanup

  For I := 0 To FaceList.Count - 1 Do FaceList.Objects[I].Free;
  FaceList.Free;
End; // TAn8Mesh.SplitPoints

Procedure TAn8Mesh.CalculateVertexNormals;
Const
  CloseToOpposite = -0.9;
  CloseToParallel =  0.9;

Var
  Opposite : Boolean;
  I,J,K,L  : Integer;
  FaceList : TStringList;
  List     : TStringList;
  List1    : TStringList;
  Face     : TAn8Face;
//  Face1    : TAn8Face;
  Norm     : T3DPoint;
  FaceNorm : T3DPoint;
  Point    : T3DPoint;
  CosSA    : Single;
  NormList : TStringList;
  St       : String;

Begin
  // Clear the normals list

  ClearStringList(Normals);

  // For each point, make a list of all faces that reference it

  FaceList := TStringList.Create;
  For I := 0 To Points.Count - 1 Do
  Begin
    List := TStringList.Create;
    FaceList.AddObject('',List);
  End; // For I

  // Make a list of each unique point value and all point indexes that match it by position

  NormList := TStringList.Create;
  NormList.Sorted := True;
  For I := 0 To Points.Count - 1 Do
  Begin
    Point := T3DPoint(Points.Objects[I]);
    St    := Point.GetID;
    J     := NormList.IndexOf(St);
    If J >= 0 Then TStringList(NormList.Objects[J]).AddObject('',Pointer(I))
    Else
    Begin
      List := TStringList.Create;
      List.AddObject('',Pointer(I));
      NormList.AddObject(St,List);  
    End;
  End; // For I

  // Add each face as a reference to every point it directly references

  For I := 0 To Faces.Count - 1 Do
  Begin
    Face := TAn8Face(Faces.Objects[I]);
    For J := 0 To High(Face.Points) Do
    Begin
      List := TStringList(FaceList.Objects[Face.Points[J]]);
      List.AddObject('',Pointer(I));
    End; // For J
  End; // For I
{
  // Remove faces from points if most of the other faces have opposite normals

  For I := 0 To Points.Count - 1 Do
  Begin
    List := TStringList(FaceList.Objects[I]);
    Norm := T3DPoint.Create;
    For J := 0 To List.Count - 1 Do
    Begin
      // Get the normal for the face

      FaceNorm := T3DPoint(FaceNormals.Objects[Integer(List.Objects[J])]);
      If J = 0 Then Norm.Add(FaceNorm)
      Else
      Begin
        // Add if the normals are either parallel or anti-parallel

        If Abs(Norm.Dot(FaceNorm)) > CloseToParallel Then Norm.Add(FaceNorm);
      End;
    End; // For J
    Norm.Normalize;
    J := 0;
    While J < List.Count - 1 Do
    Begin
      If T3DPoint(FaceNormals.Objects[Integer(List.Objects[J])]).Dot(Norm) < CloseToOpposite
       Then List.Delete(J)
       Else Inc(J);
    End; // While
    Norm.Free;
  End; // For I

  // Add each face as a reference to every matching point, but only under certain circumstances

  For I := 0 To Faces.Count - 1 Do
  Begin
    Face := TAn8Face(Faces.Objects[I]);
    For J := 0 To High(Face.Points) Do
    Begin
      List := TStringList(FaceList.Objects[Face.Points[J]]);

      // Continue only if the face hasn't already been added

      If List.IndexOfObject(Pointer(I)) < 0 Then
      Begin
        Point := T3DPoint(Points.Objects[Face.Points[J]]);
        St    := Point.GetID;
        K     := NormList.IndexOf(St);
        If K >= 0 Then
        Begin
          // The point has the same position as one the face references

          List := TStringList(NormList.Objects[K]);

          // List contains a list of matching point indexes

          For K := 0 To List.Count - 1 Do
          Begin
            // Get the face list for each matching point

            List1 := TStringList(FaceList.Objects[Integer(List.Objects[K])]);

            // This face is a candidate to be added.  However, only add the face if it doesn't have a nearly opposite
            // normal to any faces that directly reference the point (and not a similar point)

            Opposite := False;
            L := 0;
            While (L < List1.Count) And Not Opposite Do
            Begin
//              Face1 := TAn8Face(Faces.Objects[Integer(List1.Objects[L])]);
//              If Face1.ReferencesPointIndex(Face.Points[J]) Then
//              Begin
                If T3DPoint(FaceNormals.Objects[I]).Dot(T3DPoint(FaceNormals.Objects[Integer(List1.Objects[L])])) < CloseToOpposite
                 Then Opposite := True
                 Else Inc(L);
//              End
//              Else Inc(L);
            End; // While
            If Not Opposite Then List1.AddObject('',Pointer(I));
          End; // For K
        End;
      End;
    End; // For J
  End; // For I
}
  // Scan through the points, averaging in any face normals that we need to

  CosSA := Cos(SmoothAngle * Pi / 180);
  For I := 0 To Points.Count - 1 Do
  Begin
    List := TStringList(FaceList.Objects[I]);
    Norm := T3DPoint.Create(1,0,0);
    Normals.AddObject('',Norm);
    If List.Count > 0 Then
    Begin
      // Faces with zero area can have zero-length normals so we need to account for them

      Norm.Copy(T3DPoint(FaceNormals.Objects[Integer(List.Objects[0])]));
      If Norm.IsZero
       Then K := 0
       Else K := 1;
{      For J := 1 To List.Count - 1 Do
      Begin
        FaceNorm := T3DPoint(FaceNormals.Objects[Integer(List.Objects[J])]);
        If (Not FaceNorm.IsZero) And (Norm.IsZero Or (FaceNorm.Dot(Norm) >= CosSA)) Then
        Begin
          Norm.Add(FaceNorm);
          Inc(K);
        End;
      End; // For J}
      If K > 0 Then Norm.Divide(K);
      If Norm.IsZero Then Norm.Copy(1,0,0);
      Norm.Normalize;
    End;
  End; // For I

  // Cleanup

  For I := 0 To FaceList.Count - 1 Do FaceList.Objects[I].Free;
  For I := 0 To NormList.Count - 1 Do NormList.Objects[I].Free;
  FaceList.Free;
  NormList.Free;
End; // TAn8Mesh.CalculateVertexNormals

Procedure TAn8Mesh.ConvertToTriangles;
Var
  I,J,K : Integer;
  P     : TAn8Face;
  P1    : TAn8Face;
  PV    : Integer;
  Norm  : T3DPoint;

Begin
  // For now at least, assume all polygons are convex

  K := Faces.Count - 1;
  For I := 0 To K Do
  Begin
    P := TAn8Face(Faces.Objects[I]);

    // If this polygon has more than three vertices, break it up

    PV := High(P.Points);
    If PV > 2 Then
    Begin
      For J := 0 To PV - 3 Do
      Begin
        // Create a new polygon

        P1             := TAn8Face.Create;
        P1.Material    := P.Material;
        P1.NormalIndex := P.NormalIndex;
        P1.Flags       := P.Flags;

        SetLength(P1.Points,3);
        SetLength(P1.TexCoords,3);

        // Copy the right vertices and texture coordinates

        P1.Points[0] := P.Points[0];
        P1.Points[1] := P.Points[J + 2];
        P1.Points[2] := P.Points[J + 3];
        If High(P.TexCoords) >= 2 Then
        Begin
          P1.TexCoords[0] := P.TexCoords[0];
          P1.TexCoords[1] := P.TexCoords[J + 2];
          P1.TexCoords[2] := P.TexCoords[J + 3];
        End
        Else
        Begin
          P1.TexCoords[0] := 0;
          P1.TexCoords[1] := 0;
          P1.TexCoords[2] := 0;
        End;

        // Add the new polygon

        Faces.AddObject('',P1);

        // Add a new face normal if we have to

        If FaceNormals.Count = Faces.Count - 1 Then
        Begin
          Norm := T3DPoint.Create;
          FaceNormals.AddObject('',Norm);
          Norm.GetNormalTo(T3DPoint(Points.Objects[P1.Points[0]]),
                           T3DPoint(Points.Objects[P1.Points[1]]),
                           T3DPoint(Points.Objects[P1.Points[2]]));
        End;
      End; // For J

      // Shorten the original polygon

      SetLength(P.Points,3);
      SetLength(P.TexCoords,3);
    End;
  End; // For I
End; // TAn8Mesh.ConvertToTriangles

Procedure TAn8Mesh.FindAppropriateBone(Figure: TAn8Figure; Mesh: TAn8Mesh; PointIndex: Integer; Var CandidateBone: TAn8Bone);
Var
  I                : Integer;
  NewPrimaryBone   : TAn8Bone;
  SimilarPointBone : TAn8Bone;

Begin
  // Only proceed if this is a different mesh than the original

  If Self <> Mesh Then
  Begin
    // Look for a similar point as the one in the original mesh

    I := Points.IndexOf(T3DPoint(Mesh.Points.Objects[PointIndex]).GetCloseID(100));
    If I >= 0 Then
    Begin
      // Look for a bone with the same name as this mesh

      NewPrimaryBone := Figure.FindBoneByName(Name);

      // Failsafe: If the mesh uses vertex weights and they have not been all assigned, it can have less
      // PrimaryBones objects than points.  In this case, default to the root bone.  The user will
      // eventually see that something is wrong with the object and will hopefully check the weights :)
      // This can happen if points are added to a mesh after weights have been assigned.

      If I >= PrimaryBones.Count Then I := 0;

      // Get the assigned primary bone from this mesh for the similar point

      SimilarPointBone := TAn8Bone(PrimaryBones.Objects[I]);

      // The assigned primary bone should take preference as long as it's not the root bone

      If (NewPrimaryBone = Nil) Or ((SimilarPointBone <> Nil) And (SimilarPointBone.Parent <> Nil)) Then NewPrimaryBone := SimilarPointBone;

      // If the bone we found has a lower index, use it as long as it's not the root bone

      If (NewPrimaryBone <> Nil) And (NewPrimaryBone.Index < CandidateBone.Index) And (NewPrimaryBone.Parent <> Nil) Then CandidateBone := NewPrimaryBone;
    End;
  End;
End; // TAn8Mesh.FindAppropriateBone

Procedure TAn8Mesh.DeleteEmptyFaces;
Var
  I,J,K : Integer;
  Face  : TAn8Face;

Begin
  I := 0;
  While I < Faces.Count Do
  Begin
    Face := TAn8Face(Faces.Objects[I]);

    // Remove points that have the same position as the previous point

    J := 0;
    While J < High(Face.Points) Do
    Begin
      If J = 0
       Then K := High(Face.Points)
       Else K := J - 1;
      If (J <> K) And (Points.Strings[Face.Points[J]] = Points.Strings[Face.Points[K]]) Then
      Begin
        For K := J To High(Face.Points) - 1 Do
        Begin
          Face.Points[K]    := Face.Points[K + 1];
          Face.TexCoords[K] := Face.TexCoords[K + 1];
        End;
        SetLength(Face.Points,High(Face.Points));
        SetLength(Face.TexCoords,High(Face.TexCoords));
      End
      Else Inc(J);
    End; // While

    If High(Face.Points) < 2 Then
    Begin
      Faces.Delete(I);
      Face.Free;
    End
    Else Inc(I);
  End; // While
End; // TAn8Mesh.DeleteEmptyFaces

Procedure TAn8Mesh.DeleteOrphanedPoints;
Var
  I,J,K     : Integer;
  PointRefs : Array Of Integer;
  Face      : TAn8Face;
  Edge      : TAn8Edge;
  Figure    : TAn8Figure;

Begin
  SetLength(PointRefs,Points.Count);
  For I := 0 To High(PointRefs) Do PointRefs[I] := 0;

  // Mark points that are referenced by faces and edges

  For I := 0 To Faces.Count - 1 Do
  Begin
    Face := TAn8Face(Faces.Objects[I]);
    For J := 0 To High(Face.Points) Do PointRefs[Face.Points[J]] := 1;
  End; // For I
  For I := 0 To Edges.Count - 1 Do
  Begin
    Edge := TAn8Edge(Edges.Objects[I]);
    PointRefs[Edge.Pt1] := 1;
    PointRefs[Edge.Pt2] := 1;
  End; // For I

  // Get rid of points that aren't referenced

  For I := High(PointRefs) DownTo 0 Do
  Begin
    If PointRefs[I] = 0 Then
    Begin
      Points.Objects[I].Free;
      Points.Delete(I);
      If Normals.Count > I Then
      Begin
        Normals.Objects[I].Free;
        Normals.Delete(I);
      End;
      If PrimaryBones.Count > I Then
      Begin
        PrimaryBones.Objects[I].Free;
        PrimaryBones.Delete(I);
      End;
      For J := 0 To An8File.Figures.Count - 1 Do
      Begin
        Figure := TAn8Figure(An8File.Figures.Objects[J]);
        If Figure.RootBone <> Nil Then Figure.RootBone.DeletePaintedWeight(Self,I);
      End; // For J
    End;
  End; // For I

  // Figure out the adjustment

  J := 0;
  For I := 0 To High(PointRefs) Do
  Begin
    K := 1 - PointRefs[I];
    PointRefs[I] := J;
    Inc(J,K);
  End; // For I

  // Adjust the point references

  For I := 0 To Faces.Count - 1 Do
  Begin
    Face := TAn8Face(Faces.Objects[I]);
    For J := 0 To High(Face.Points) Do Dec(Face.Points[J],PointRefs[Face.Points[J]]);
  End; // For I
  For I := 0 To Edges.Count - 1 Do
  Begin
    Edge := TAn8Edge(Edges.Objects[I]);
    Dec(Edge.Pt1,PointRefs[Edge.Pt1]);
    Dec(Edge.Pt2,PointRefs[Edge.Pt2]);
  End; // For I
  SetLength(PointRefs,0);
End; // TAn8Mesh.DeleteOrphanedPoints

Procedure TAn8Mesh.DeleteOrphanedTexCoords;
Var
  I,J,K     : Integer;
  PointRefs : Array Of Integer;
  Face      : TAn8Face;
  Edge      : TAn8Edge;

Begin
  SetLength(PointRefs,TexCoords.Count);
  For I := 0 To High(PointRefs) Do PointRefs[I] := 0;

  // Mark texture coordinates that are referenced by faces

  For I := 0 To Faces.Count - 1 Do
  Begin
    Face := TAn8Face(Faces.Objects[I]);
    For J := 0 To High(Face.TexCoords) Do PointRefs[Face.TexCoords[J]] := 1;
  End; // For I

  // Get rid of texture coordinates that aren't referenced

  For I := High(PointRefs) DownTo 0 Do
  Begin
    If PointRefs[I] = 0 Then
    Begin
      TexCoords.Objects[I].Free;
      TexCoords.Delete(I);
    End;
  End; // For I

  // Figure out the adjustment

  J := 0;
  For I := 0 To High(PointRefs) Do
  Begin
    K := 1 - PointRefs[I];
    PointRefs[I] := J;
    Inc(J,K);
  End; // For I

  // Adjust the point references

  For I := 0 To Faces.Count - 1 Do
  Begin
    Face := TAn8Face(Faces.Objects[I]);
    For J := 0 To High(Face.TexCoords) Do Dec(Face.TexCoords[J],PointRefs[Face.TexCoords[J]]);
  End; // For I
  SetLength(PointRefs,0);
End; // TAn8Mesh.DeleteOrphanedTexCoords

Procedure TAn8Mesh.ParseData(St: String);
Var St1,St2: String;

  Procedure Load3DPoint(Var Point: T3DPoint; St: String);
  Var
    St1,St2 : String;
    Points  : TXYZPointArray;

  Begin
    While St <> '' Do
    Begin
      St1 := LowerCase(GetFirstWordOrNumber(St));
      St2 := GetBracketedContents(St,'{','}');
      If St1 = 'origin' Then
      Begin
        GetAn8Points(Points,St2);
        If High(Points) >= 0 Then
        Begin
          Point.X := Points[0].X;
          Point.Y := Points[0].Y;
          Point.Z := Points[0].Z;
        End;
        SetLength(Points,0);
      End;
    End; // While
  End; // Load3DPoint

  Procedure LoadMaterialList(St: String);
  Var St1,St2: String;
  Begin
    While St <> '' Do
    Begin
      St1 := LowerCase(GetFirstWordOrNumber(St));
      St2 := GetBracketedContents(St,'{','}');
      If St1 = 'materialname' Then MaterialList.AddObject('',Parent.FindMaterialByName(Copy(St2,2,Length(St2) - 2)))
    End; // While
  End; // LoadMaterialList

  Procedure LoadPoints(St: String);
  Var
    APoints : TXYZPointArray;
    Point   : T3DPoint;
    I       : Integer;

  Begin
    GetAn8Points(APoints,St);
    For I := 0 To High(APoints) Do
    Begin
      Point   := T3DPoint.Create;
      Point.X := APoints[I].X;
      Point.Y := APoints[I].Y;
      Point.Z := APoints[I].Z;
      Points.AddObject(Point.GetCloseID(100),Point);
    End; // For I
    SetLength(APoints,0);
  End; // LoadPoints

  Procedure LoadEdges(St: String);
  Var
    AEdges : TEdgeArray;
    Edge   : TAn8Edge;
    I      : Integer;

  Begin
    GetEdges(AEdges,St);
    For I := 0 To High(AEdges) Do
    Begin
      Edge     := TAn8Edge.Create;
      Edge.Pt1 := AEdges[I].Pt1;
      Edge.Pt2 := AEdges[I].Pt2;
      Edges.AddObject('',Edge);
    End; // For I
    SetLength(AEdges,0);
  End; // LoadEdges

  Procedure LoadTexCoords(St: String);
  Var
    ATexCoords : TTexCoordArray;
    TexCoord   : TAn8TexCoord;
    I          : Integer;

  Begin
    GetAn8TexCoords(ATexCoords,St);
    For I := 0 To High(ATexCoords) Do
    Begin
      TexCoord    := TAn8TexCoord.Create;
      TexCoord.TX := ATexCoords[I].TX;
      TexCoord.TZ := ATexCoords[I].TZ;
      TexCoords.AddObject('',TexCoord);
    End; // For I
    SetLength(ATexCoords,0);
  End; // LoadTexCoords

  Procedure LoadFaces(St: String);
  Var
    St1,St2     : String;
    NumPoints   : Integer;
    MatIndex    : Integer;
    NormalIndex : Integer;
    Face        : TAn8Face;
    I,J,K,L,M   : Integer;
    Level       : Integer;

  Begin
    I := 1;
    While I <= Length(St) Do
    Begin
      Face := TAn8Face.Create;

      // Get the number of points

      K := I;
      While (K <= Length(St)) And (St[K] = ' ') Do Inc(K);
      J := K;
      While (K <= Length(St)) And (St[K] <> ' ') Do Inc(K);
      If K <= Length(St) Then
      Begin
        NumPoints := StrToInt(Trim(Copy(St,J,K - J)));

        // Get the flags

        Inc(K);
        While (K <= Length(St)) And (St[K] = ' ') Do Inc(K);
        J := K;
        While (K <= Length(St)) And (St[K] <> ' ') Do Inc(K);
        If K <= Length(St) Then
        Begin
          Face.Flags := StrToInt(Trim(Copy(St,J,K - J)));

          // Get the material index

          Inc(K);
          While (K <= Length(St)) And (St[K] = ' ') Do Inc(K);
          J := K;
          While (K <= Length(St)) And (St[K] <> ' ') Do Inc(K);
          If K <= Length(St) Then
          Begin
            MatIndex := StrToInt(Trim(Copy(St,J,K - J)));

            // Get the normal index

            Inc(K);
            While (K <= Length(St)) And (St[K] = ' ') Do Inc(K);
            J := K;
            While (K <= Length(St)) And (St[K] <> '(') Do Inc(K);
            If K <= Length(St) Then
            Begin
              NormalIndex := StrToInt(Trim(Copy(St,J,K - J)));

              If (MatIndex >= 0) And (MatIndex < MaterialList.Count) Then Face.Material := TAn8Material(MaterialList.Objects[MatIndex]);
              SetLength(Face.Points,NumPoints);
              If (Face.Flags And 4) <> 0 Then SetLength(Face.TexCoords,NumPoints);
              Face.NormalIndex := NormalIndex;

              // Get the point/texcoord list

              J := K;
              Level := 0;
              Repeat
                Case St[K] Of
                  '(': Inc(Level);
                  ')': Dec(Level);
                End; // Case
                If Not ((Level = 0) And (St[K] = ')')) Then Inc(K);
              Until (K > Length(St)) Or ((Level = 0) And (St[K] = ')'));
              If K <= Length(St) Then
              Begin
                St1 := Trim(Copy(St,J + 1,K - J - 1));
                For L := 0 To NumPoints - 1 Do
                Begin
                  St2 := GetBracketedContents(St1,'(',')'); // Note: parentheses instead of braces
                  Face.Points[L] := StrToInt(GetToken(' ',St2,True));
                  If (Face.Flags And 4) <> 0 Then Face.TexCoords[L] := StrToInt(GetToken(' ',St2,True));
                End; // For I

                // Remove points that have the same position as the previous point

                L := 0;
                While L < High(Face.Points) Do
                Begin
                  If L = 0
                   Then M := High(Face.Points)
                   Else M := L - 1;
                  If (L <> M) And (Points.Strings[Face.Points[L]] = Points.Strings[Face.Points[M]]) Then
                  Begin
                    For M := L To High(Face.Points) - 1 Do
                    Begin
                      Face.Points[M]    := Face.Points[M + 1];
                      Face.TexCoords[M] := Face.TexCoords[M + 1];
                    End;
                    SetLength(Face.Points,High(Face.Points));
                    SetLength(Face.TexCoords,High(Face.TexCoords));
                  End
                  Else Inc(L);
                End; // While
                                         
                // Only add the face if it has nonzero area (critical to do this or vertex normals will be off)

                If High(Face.Points) >= 2
                 Then Faces.AddObject('',Face)
                 Else Face.Free;
              End;
              I := K + 1;

            End
            Else I := K;

          End
          Else I := K;

        End
        Else I := K;

      End
      Else I := K;
    End; // While

{
    While St <> '' Do
    Begin
      Face := TAn8Face.Create;
      St1  := GetToken(' ',St,True); NumPoints   := StrToInt(St1);
      St1  := GetToken(' ',St,True); Flags       := StrToInt(St1);
      St1  := GetToken(' ',St,True); MatIndex    := StrToInt(St1);
      St1  := GetToken(' ',St,True); NormalIndex := StrToInt(St1);

      // Flags bit values:
      //   Bit 0 ..... Show backside of face
      //   Bit 1 ..... Face has per vertex normal data (Not really used in .an8 files)
      //   Bit 2 ..... Face has texture coordinates

      If (MatIndex >= 0) And (MatIndex < MaterialList.Count) Then Face.Material := TAn8Material(MaterialList.Objects[MatIndex]);
      SetLength(Face.Points,NumPoints);
      If (Flags And 4) <> 0 Then SetLength(Face.TexCoords,NumPoints);
      Face.NormalIndex := NormalIndex;
      St1 := GetBracketedContents(St,'(',')'); // Note: parentheses instead of braces
      For I := 0 To NumPoints - 1 Do
      Begin
        St2 := GetBracketedContents(St1,'(',')'); // Note: parentheses instead of braces
        Face.Points[I] := StrToInt(GetToken(' ',St2,True));
        If (Flags And 4) <> 0 Then Face.TexCoords[I] := StrToInt(GetToken(' ',St2,True));
      End; // For I
      Faces.AddObject('',Face);
    End; // While
}    
  End; // LoadFaces

Begin
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
         If St1 = 'name'         Then Name := Copy(St2,2,Length(St2) - 2)
    Else If St1 = 'base'         Then Base.ParseData(St2)
    Else If St1 = 'pivot'        Then Pivot.ParseData(St2)
    Else If St1 = 'material'     Then Material    := Parent.FindMaterialByName(Copy(St2,2,Length(St2) - 2))
    Else If St1 = 'smoothangle'  Then SmoothAngle := StrToFloat(St2)
    Else If St1 = 'materiallist' Then LoadMaterialList(St2)
    Else If St1 = 'points'       Then LoadPoints(St2)
    Else If St1 = 'edges'        Then LoadEdges(St2)
    Else If St1 = 'texcoords'    Then LoadTexCoords(St2)
    Else If St1 = 'faces'        Then LoadFaces(St2);
  End; // While
  CalculateFaceNormals;
//  SplitPoints;
  CalculateVertexNormals;
{
  DeleteOrphanedPoints;
  DeleteOrphanedTexCoords;
}  
End; // TAn8Mesh.ParseData

Procedure TAn8Mesh.WriteToFile(Var F: Text; Indent: Integer);
Var
  IndentStr : String;
  I,J       : Integer;
  St        : String;
  Point     : T3DPoint;
  TexCoord  : TAn8TexCoord;
  Face      : TAn8Face;

Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'mesh {');
  WriteLn(F,IndentStr + '  name { "' + Name + '" }');
  Base.WriteToFile(F,Indent + 2);
  Pivot.WriteToFile(F,Indent + 2);
  If Material <> Nil Then WriteLn(F,IndentStr + '  material { "' + Material.Name + '" }');
  WriteLn(F,IndentStr + '  smoothangle { ' + IntToStr(Trunc(SmoothAngle)) + ' }');
  WriteLn(F,IndentStr + Format('  /* %d points, %d faces, %d uvCoords */',[Points.Count,Faces.Count,TexCoords.Count]));
  WriteLn(F,IndentStr + '  materiallist {');
  For I := 0 To MaterialList.Count - 1 Do WriteLn(F,IndentStr + '    materialname { "' + TAn8Material(MaterialList.Objects[I]).Name + '" }');
  WriteLn(F,IndentStr + '  }');
  If Points.Count > 0 Then
  Begin
    WriteLn(F,IndentStr + '  points {');
    For I := 0 To Points.Count - 1 Do
    Begin
      Point := T3DPoint(Points.Objects[I]);
      WriteLn(F,IndentStr + Format('    (%5.3f %5.3f %5.3f)',[Point.X,Point.Y,Point.Z]));
    End; // For I
    WriteLn(F,IndentStr + '  }');
  End;
  If TexCoords.Count > 0 Then
  Begin
    WriteLn(F,IndentStr + '  texcoords {');
    For I := 0 To TexCoords.Count - 1 Do
    Begin
      TexCoord := TAn8TexCoord(TexCoords.Objects[I]);
      WriteLn(F,IndentStr + Format('    (%7.5f %7.5f)',[TexCoord.TX,TexCoord.TZ]));
    End; // For I
    WriteLn(F,IndentStr + '  }');
  End;
  If Faces.Count > 0 Then
  Begin
    WriteLn(F,IndentStr + '  faces {');
    For I := 0 To Faces.Count - 1 Do
    Begin
      Face := TAn8Face(Faces.Objects[I]);
      St := '';
      For J := 0 To High(Face.Points) Do
      Begin
        St := St + '(' + IntToStr(Face.Points[J]);
        If J <= High(Face.TexCoords) Then St := St + ' ' + IntToStr(Face.TexCoords[J]);
        St := St + ') ';
      End; // For J
      J := High(Face.Points) + 1;
      WriteLn(F,IndentStr + Format('    %d %d %d -1 ( %s)',[J,Face.Flags,MaterialList.IndexOfObject(Face.Material),St]));
    End; // For I
    WriteLn(F,IndentStr + '  }');
  End;
  WriteLn(F,IndentStr + '}');
End; // TAn8Mesh.WriteToFile

// ------------------------
// TAn8Object
// ------------------------

Constructor TAn8Object.Create(AParent: TAn8File);
Begin
  Name       := '';
  Parent     := AParent;
  Materials  := TStringList.Create;
  Components := TStringList.Create;
End; // TAn8Object.Create

Destructor TAn8Object.Destroy;
Begin
  KillStringList(Materials);
  KillStringList(Components);
  Inherited;
End; // TAn8Object.Destroy

Function TAn8Object.GetMeshPointIndex(Const Mesh: TAn8Mesh): Integer;
// Returns the absolute index of points for the given mesh, if all meshes
// (nested in groups and otherwise) are treated consecutively in the
// object.  Useful for determining where a mesh's points lie in a flat
// list of points, such as a figure's list of painted weights.
Var
  Index : Integer;
  I     : Integer;
  Found : Boolean;

  Function ProcessComponent(Const Component: TAn8Component; Const Mesh: TAn8Mesh; Var Index: Integer): Boolean;
  Var
    I     : Integer;
    Found : Boolean;
    Group : TAn8Group;

  Begin
    If Component = Mesh Then Result := True
    Else
    Begin
      If Component Is TAn8Mesh Then
      Begin
        Result := False;
        Inc(Index,TAn8Mesh(Component).Points.Count);
      End
      Else If Component Is TAn8Group Then
      Begin
        I     := 0;
        Found := False;
        Group := TAn8Group(Component);
        While (I < Group.Components.Count) And Not Found Do
        Begin
          Found := ProcessComponent(TAn8Component(Group.Components.Objects[I]),Mesh,Index);
          If Not Found Then Inc(I);
        End; // While
        Result := Found;
      End;
    End;
  End; // ProcessComponent

Begin
  Index := 0;
  I     := 0;
  Found := False;
  While (I < Components.Count) And Not Found Do
  Begin
    Found := ProcessComponent(TAn8Component(Components.Objects[I]),Mesh,Index);
    If Not Found Then Inc(I);
  End; // While
  If Found
   Then Result := Index
   Else Result := -1;
End; // TAn8Object.GetMeshPointIndex

Procedure TAn8Object.GetBounds(MinPt,MaxPt: T3DPoint);
Var
  I         : Integer;
  Component : TObject;
  Min1,Max1 : T3DPoint;
  First     : Boolean;

Begin
  Min1  := T3DPoint.Create;
  Max1  := T3DPoint.Create;
  First := True;
  For I := 0 To Components.Count - 1 Do
  Begin
    Component := Components.Objects[I];
    If Component Is TAn8Component Then
    Begin
      TAn8Component(Component).GetBounds(Min1,Max1);
      If First Then
      Begin
        MinPt.Copy(Min1);
        MaxPt.Copy(Max1);
        First := False;
      End
      Else
      Begin
        If Min1.X < MinPt.X Then MinPt.X := Min1.X;
        If Min1.Y < MinPt.Y Then MinPt.Y := Min1.Y;
        If Min1.Z < MinPt.Z Then MinPt.Z := Min1.Z;
        If Max1.X > MaxPt.X Then MaxPt.X := Max1.X;
        If Max1.Y > MaxPt.Y Then MaxPt.Y := Max1.Y;
        If Max1.Z > MaxPt.Z Then MaxPt.Z := Max1.Z;
      End;
    End;
  End; // For I
  Max1.Free;
  Min1.Free;
End; // TAn8Object.GetBounds

Procedure TAn8Object.ParseData(St: String);
Var St1,St2: String;
Begin
  St1  := GetQuotedString(St);
  Name := St1;
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'material' Then
    Begin
      Materials.AddObject('',TAn8Material.Create(Self));
      TAn8Material(Materials.Objects[Materials.Count - 1]).ParseData(St2);
      Materials.Strings[Materials.Count - 1] := TAn8Material(Materials.Objects[Materials.Count - 1]).Name;
    End
    Else If St1 = 'mesh' Then
    Begin
      Components.AddObject('',TAn8Mesh.Create(Parent,'',Self,Nil));
      TAn8Mesh(Components.Objects[Components.Count - 1]).ParseData(St2);
      Components.Strings[Components.Count - 1] := TAn8Mesh(Components.Objects[Components.Count - 1]).Name;
    End
    Else If St1 = 'group' Then
    Begin
      Components.AddObject('',TAn8Group.Create(Parent,'',Self,Nil));
      TAn8Group(Components.Objects[Components.Count - 1]).ParseData(St2);
      Components.Strings[Components.Count - 1] := TAn8Group(Components.Objects[Components.Count - 1]).Name;
    End;
  End; // While
End; // TAn8Object.ParseData

Procedure TAn8Object.WriteToFile(Var F: Text; Indent: Integer);
Var
  IndentStr : String;
  I         : Integer;
  Component : TAn8Component;

Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'object { "' + Name + '"');
  For I := 0 To Materials.Count - 1 Do TAn8Material(Materials.Objects[I]).WriteToFile(F,Indent + 2);
  For I := 0 To Components.Count - 1 Do
  Begin
    Component := TAn8Component(Components.Objects[I]);
    If Component <> Nil Then
    Begin
           If Component Is TAn8Mesh  Then TAn8Mesh(Component).WriteToFile(F,Indent + 2)
      Else If Component Is TAn8Group Then TAn8Group(Component).WriteToFile(F,Indent + 2);
    End;
  End; // For I
  WriteLn(F,IndentStr + '}');
End; // TAn8Object.WriteToFile

Function TAn8Object.FindMaterialByName(St: String): TAn8Material;
Var I: Integer;
Begin
  I := Materials.IndexOf(St);
  If I >= 0 Then Result := TAn8Material(Materials.Objects[I])
  Else
  Begin
    I := Parent.Materials.IndexOf(St);
    If I >= 0
     Then Result := TAn8Material(Parent.Materials.Objects[I])
     Else Result := Nil;
  End;
End; // TAn8Object.FindMaterialByName

Function TAn8Object.FindMeshByName(St: String): TAn8Mesh;
Var
  I         : Integer;
  Component : TAn8Component;

Begin
  I      := 0;
  Result := Nil;
  While (I < Components.Count) And (Result = Nil) Do
  Begin
    Component := TAn8Component(Components.Objects[I]);
    If Component <> Nil Then
    Begin
           If Component Is TAn8Group Then Result := TAn8Group(Component).FindMeshByName(St)
      Else If (Component Is TAn8Mesh) And (TAn8Mesh(Component).Name = St) Then Result := TAn8Mesh(Component);
    End;
    Inc(I);
  End; // While
End; // TAn8Object.FindMeshByName

Procedure TAn8Object.MergeVeryClosePoints;
Var
  I,J   : Integer;
  Mesh  : TAn8Mesh;
  Point : T3DPoint;
  Obj   : TObject;

Begin
  For I := 0 To Components.Count - 1 Do
  Begin
    Obj := Components.Objects[I];
    If Obj Is TAn8Mesh Then
    Begin
      Mesh := TAn8Mesh(Obj);
      For J := 0 To Mesh.Points.Count - 1 Do
      Begin
        Point   := T3DPoint(Mesh.Points.Objects[J]);
        Point.X := Round(Point.X * 1000) / 1000;
        Point.Y := Round(Point.Y * 1000) / 1000;
        Point.Z := Round(Point.Z * 1000) / 1000;
        Mesh.Points.Strings[J] := Point.GetCloseID(100);
      End; // For J
    End
    Else If Obj Is TAn8Group Then TAn8Group(Obj).MergeVeryClosePoints;
  End; // For I
End; // TAn8Object.MergeVeryClosePoints

// ------------------------
// TAn8VertexWeight
// ------------------------

Constructor TAn8VertexWeight.Create;
Begin
  SetLength(BoneIndices,0);
  SetLength(Weights,0);
End; // TAn8VertexWeight.Create

Constructor TAn8VertexWeight.Create(Weight: TAn8VertexWeight);
Var I: Integer;
Begin
  If Weight <> Nil Then
  Begin
    SetLength(BoneIndices,High(Weight.BoneIndices) + 1);
    SetLength(Weights,High(Weight.Weights) + 1);
    For I := 0 To High(BoneIndices) Do BoneIndices[I] := Weight.BoneIndices[I];
    For I := 0 To High(Weights)     Do Weights[I]     := Weight.Weights[I];
  End
  Else
  Begin
    SetLength(BoneIndices,0);
    SetLength(Weights,0);
  End;
End; // TAn8VertexWeight.Create

Destructor TAn8VertexWeight.Destroy;
Begin
  SetLength(BoneIndices,0);
  SetLength(Weights,0);
  Inherited;
End; // TAn8VertexWeight.Destroy

Function TAn8VertexWeight.HighestInfluenceIndex: Integer;
Var
  I         : Integer;
  MaxWeight : Single;
  MaxIndex  : Integer;

Begin
  MaxWeight := 0;
  MaxIndex  := 0;
  If High(Weights) >= 0 Then MaxWeight := Weights[0];
  For I := 1 To High(Weights) Do
  Begin
    If Weights[I] > MaxWeight Then
    Begin
      MaxWeight := Weights[I];
      MaxIndex  := I;
    End;
  End; // For I
  Result := MaxIndex;
End; // TAn8VertexWeight.HighestInfluenceIndex

// ------------------------
// TAn8PaintedWeights
// ------------------------

Constructor TAn8PaintedWeights.Create;
Begin
  Name    := '';
  Weights := TStringList.Create;
End; // TAn8PaintedWeights.Create

Constructor TAn8PaintedWeights.Create(PaintedWeights: TAn8PaintedWeights);
Var I: Integer;
Begin
  Weights := TStringList.Create;
  If PaintedWeights <> Nil Then
  Begin
    Name := PaintedWeights.Name;
    For I := 0 To PaintedWeights.Weights.Count - 1 Do Weights.AddObject(PaintedWeights.Weights.Strings[I],TAn8VertexWeight.Create(TAn8VertexWeight(PaintedWeights.Weights.Objects[I])));
  End
  Else Name := '';
End; // TAn8PaintedWeights.Create

Destructor TAn8PaintedWeights.Destroy;
Var I: Integer;
Begin
  KillStringList(Weights);
  Inherited;
End; // TAn8PaintedWeights.Destroy

Procedure TAn8PaintedWeights.DeletePaintedWeight(Index: Integer);
Begin
  If (Index >= 0) And (Index < Weights.Count) Then
  Begin
    Weights.Objects[Index].Free;
    Weights.Delete(Index);
  End;
End; // TAn8PaintedWeights.DeletePaintedWeight

Procedure TAn8PaintedWeights.WriteToFile(Var F: Text; Indent: Integer);
Var
  IndentStr : String;
  I,J,K     : Integer;
  Weight    : TAn8VertexWeight;

Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'weights { "' + Name + '"');
  For I := 0 To Weights.Count - 1 Do
  Begin
    Weight := TAn8VertexWeight(Weights.Objects[I]);
    Write(F,IndentStr + '  (' + IntToStr(High(Weight.BoneIndices) + 1) + ' ');
    K := High(Weight.BoneIndices);
    For J := 0 To K Do
    Begin
      Write(F,Format('(%d %7.5f)',[Weight.BoneIndices[J],Weight.Weights[J]]));
      If J < K Then Write(F,' ');
    End; // For J
    WriteLn(F,')');
  End; // For I
  WriteLn(F,IndentStr + '}');
End; // TAn8PaintedWeights.WriteToFile

// ------------------------
// TAn8NamedObject
// ------------------------

Constructor TAn8NamedObject.Create(AParent: TAn8Bone);
Begin
  Obj        := Nil;
  Name       := '';
  Base       := T3DPoint.Create;
  Material   := Nil;
  WeightedBy := TStringList.Create;
  Weights    := TStringList.Create;
  Parent     := AParent;
End; // TAn8NamedObject.Create

Constructor TAn8NamedObject.Create(NO: TAn8NamedObject);
Var I: Integer;
Begin
  Obj        := NO.Obj;
  Name       := NO.Name;
  Base       := T3DPoint.Create(NO.Base);
  Material   := NO.Material;
  WeightedBy := TStringList.Create;
  Weights    := TStringList.Create;
  Parent     := NO.Parent;
  For I := 0 To NO.WeightedBy.Count - 1 Do WeightedBy.AddObject(NO.WeightedBy.Strings[I],NO.WeightedBy.Objects[I]);
End; // TAn8NamedObject.Create

Destructor TAn8NamedObject.Destroy;
Var I: Integer;
Begin
  Base.Free;
  WeightedBy.Free;
  KillStringList(Weights);
  Inherited;
End; // TAn8NamedObject.Destroy

Procedure TAn8NamedObject.DeletePaintedWeight(Mesh: TAn8Mesh; Index: Integer);
Var I,J: Integer;
Begin
  If Obj <> Nil Then
  Begin
    J := Obj.GetMeshPointIndex(Mesh);
    If J >= 0 Then
    Begin
      For I := 0 To Weights.Count - 1 Do TAn8PaintedWeights(Weights.Objects[I]).DeletePaintedWeight(Index + J);
    End;
  End;
End; // TAn8NamedObject.DeletePaintedWeight

Procedure TAn8NamedObject.ParseData(St: String);
Var St1,St2: String;

  Procedure Load3DPoint(Var Point: T3DPoint; St: String);
  Var
    St1,St2 : String;
    Points  : TXYZPointArray;

  Begin
    While St <> '' Do
    Begin
      St1 := LowerCase(GetFirstWordOrNumber(St));
      St2 := GetBracketedContents(St,'{','}');
      If St1 = 'origin' Then
      Begin
        GetAn8Points(Points,St2);
        If High(Points) >= 0 Then
        Begin
          Point.X := Points[0].X;
          Point.Y := Points[0].Y;
          Point.Z := Points[0].Z;
        End;
        SetLength(Points,0);
      End;
    End; // While
  End; // Load3DPoint

  Procedure LoadWeights(St: String);
  Var
    St1         : String;
    St2         : String;
    I,J,K       : Integer;
    Level       : Integer;
    MeshWeights : TAn8PaintedWeights;
    Weight      : TAn8VertexWeight;
    BoneCount   : Integer;
    BoneIndex   : Integer;
    BoneWeight  : Single;

  Begin
    MeshWeights := TAn8PaintedWeights.Create;
    MeshWeights.Name := GetQuotedString(St);

    // Process the bone/weight list

    While St <> '' Do
    Begin
      K     := 1;
      Level := 0;
      Repeat
        Case St[K] Of
          '(': Inc(Level);
          ')': Dec(Level);
        End; // Case
        If Not ((Level = 0) And (St[K] = ')')) Then Inc(K);
      Until (K > Length(St)) Or ((Level = 0) And (St[K] = ')'));

      // Parse the vertex weights list

      If K <= Length(St) Then
      Begin
        // Create a weights entry for this vertex

        Weight := TAn8VertexWeight.Create;
        MeshWeights.Weights.AddObject('',Weight);

        St2 := Trim(Copy(St,2,K - 2));           // Strip off the parentheses
        St  := Trim(Copy(St,K + 1,Length(St)));  // The remainder of the string

        // The first number in St2 is the number of bones in the sub-list for this vertex

        BoneCount := StrToInt(GetToken(' ',St2,True));
        SetLength(Weight.BoneIndices,BoneCount);
        SetLength(Weight.Weights,BoneCount);
        For J := 0 To BoneCount - 1 Do
        Begin
          St1       := GetBracketedContents(St2,'(',')'); // Note: parentheses instead of braces
          BoneIndex := StrToInt(GetToken(' ',St1,True));
          Val(St1,BoneWeight,I);
          Weight.BoneIndices[J] := BoneIndex;
          Weight.Weights[J]     := BoneWeight;
        End; // For J
      End;
    End; // While
    Weights.AddObject(MeshWeights.Name,MeshWeights);
  End; // LoadWeights

Begin
  St1     := GetQuotedString(St);
  ObjName := St1;            // The object might not actually exist so we have to save the name separately
  Obj     := Parent.Figure.Parent.FindObjectByName(St1);
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
         If St1 = 'name'       Then Name := Copy(St2,2,Length(St2) - 2)
    Else if St1 = 'base'       Then Load3DPoint(Base,St2)
    Else If (St1 = 'material') And (Obj <> Nil) Then Material := Obj.FindMaterialByName(Copy(St2,2,Length(St2) - 2))
    Else If St1 = 'weightedby' Then WeightedBy.Add(Copy(St2,2,Length(St2) - 2))
    Else If St1 = 'weights'    Then LoadWeights(St2);
  End; // While
End; // TAn8NamedObject.ParseData

Procedure TAn8NamedObject.WriteToFile(Var F: Text; Indent: Integer);
Var
  IndentStr : String;
  I         : Integer;

Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'namedobject { "' + ObjName + '"');
  WriteLn(F,IndentStr + '  name { "' + Name + '" }');
  WriteLn(F,IndentStr + '  base {');
  WriteLn(F,IndentStr + Format('    origin { (%7.5f %7.5f %7.5f) }',[Base.X,Base.Y,Base.Z]));
  WriteLn(F,IndentStr + '  }');
  If Material <> Nil
   Then WriteLn(F,IndentStr + '  material { "' + Material.Name + '" }')
   Else WriteLn(F,IndentStr + '  material { " -- default --" }');
  For I := 0 To WeightedBy.Count - 1 Do WriteLn(F,IndentStr + '  weightedby { "' + TAn8Bone(WeightedBy.Objects[I]).Name + '" }');
  For I := 0 To Weights.Count - 1 Do TAn8PaintedWeights(Weights.Objects[I]).WriteToFile(F,Indent + 2);
  WriteLn(F,IndentStr + '}');
End; // TAn8NamedObject.WriteToFile

// ------------------------
// TAn8Bone
// ------------------------

Constructor TAn8Bone.Create(AParent: TAn8Bone; AFigure: TAn8Figure);
Begin
  Name         := '';
  Len          := 0;
  Diameter     := 1;
  Children     := TStringList.Create;
  Parent       := AParent;
  Figure       := AFigure;
  NamedObjects := TStringList.Create;
  Orientation  := TQuaternion.Create;
  Origin       := T3DPoint.Create;
  FillChar(XDof,SizeOf(XDof),0);
  FillChar(YDof,SizeOf(YDof),0);
  FillChar(ZDof,SizeOf(ZDof),0);
  FillChar(Influence,SizeOf(Influence),0);
End; // TAn8Bone.Create

Constructor TAn8Bone.Create(Bone: TAn8Bone);
Var
  I     : Integer;
  Child : TAn8Bone;
  NO    : TAn8NamedObject;

Begin
  Name         := Bone.Name;
  Len          := Bone.Len;
  Diameter     := Bone.Diameter;
  Children     := TStringList.Create;
  Figure       := Bone.Figure;
  NamedObjects := TStringList.Create;
//  NamedObject := Bone.NamedObject;
  Orientation  := TQuaternion.Create(Bone.Orientation);
  Origin       := T3DPoint.Create(Bone.Origin);
  XDof         := Bone.XDof;
  YDof         := Bone.YDof;
  ZDof         := Bone.ZDof;
  Influence    := Bone.Influence;
  For I := 0 To Bone.NamedObjects.Count - 1 Do
  Begin
    NO := TAn8NamedObject(Bone.NamedObjects.Objects[I]);
    NamedObjects.AddObject(NO.Name,TAn8NamedObject.Create(NO));
  End; // For I
  For I := 0 To Bone.Children.Count - 1 Do
  Begin
    Child := TAn8Bone.Create(TAn8Bone(Bone.Children.Objects[I]));
    Child.Parent := Self;
    Children.AddObject(Bone.Children.Strings[I],Child);
  End; // For I
End; // TAn8Bone.Create

Destructor TAn8Bone.Destroy;
Begin
  KillStringList(NamedObjects);
  KillStringList(Children);
  Orientation.Free;
  Origin.Free;
  Inherited;
End; // TAn8Bone.Destroy

Procedure TAn8Bone.DeletePaintedWeight(Mesh: TAn8Mesh; Index: Integer);
Var I: Integer;
Begin
  For I := 0 To NamedObjects.Count - 1 Do TAn8NamedObject(NamedObjects.Objects[I]).DeletePaintedWeight(Mesh,Index);
  For I := 0 To Children.Count - 1 Do TAn8Bone(Children.Objects[I]).DeletePaintedWeight(Mesh,Index);
End; // TAn8Bone.DeletePaintedWeight

Procedure TAn8Bone.CalculateOrigin(X,Y,Z: Single);
Var
  BoneVec : T3DPoint;
  B       : TAn8Bone;
  I       : Integer;

Begin
  Origin.Copy(X,Y,Z);

  // Figure out this bone's orientation

  BoneVec := T3DPoint.Create(0,1,0);
  B       := Self;
  While B <> Nil Do
  Begin
    B.Orientation.Transform(BoneVec);
    B := B.Parent;
  End; // While
  BoneVec.Multiply(Len);
  If Parent <> Nil Then    // The first bone always ends at the origin
  Begin
    X := X + BoneVec.X;
    Y := Y + BoneVec.Y;
    Z := Z + BoneVec.Z;
  End;
  BoneVec.Free;
  For I := 0 To Children.Count - 1 Do TAn8Bone(Children.Objects[I]).CalculateOrigin(X,Y,Z);
End; // TAn8Bone.CalculateOrigin

Procedure TAn8Bone.ParseData(St: String);
Var
  St1,St2 : String;
  NO      : TAn8NamedObject;
  NO1     : TAn8NamedObject;
  I,J     : Integer;

  Procedure LoadDof(St: String);
  Var St1: String;

  Begin
    St1 := LowerCase(GetToken(' ',St,True)); // GetToken automatically strips the double quotes
    If St1 = 'x' Then
    Begin
      XDof.Minimum := StrToFloat(GetToken(' ',St,True));
      XDof.Default := StrToFloat(GetToken(' ',St,True));
      XDof.Maximum := StrToFloat(GetToken(' ',St,True));
    End
    Else If St1 = 'y' Then
    Begin
      YDof.Minimum := StrToFloat(GetToken(' ',St,True));
      YDof.Default := StrToFloat(GetToken(' ',St,True));
      YDof.Maximum := StrToFloat(GetToken(' ',St,True));
    End
    Else If St1 = 'z' Then
    Begin
      ZDof.Minimum := StrToFloat(GetToken(' ',St,True));
      ZDof.Default := StrToFloat(GetToken(' ',St,True));
      ZDof.Maximum := StrToFloat(GetToken(' ',St,True));
    End;
  End; // LoadDof

  Procedure FixupNamedObject(ANamedObject: TAn8NamedObject; Bone: TAn8Bone);
  Var I: Integer;
  Begin
    If Bone <> Nil Then
    Begin
      I := ANamedObject.WeightedBy.IndexOf(Bone.Name);
      If I >= 0 Then ANamedObject.WeightedBy.Objects[I] := Bone;
      For I := 0 To Bone.Children.Count - 1 Do FixupNamedObject(ANamedObject,TAn8Bone(Bone.Children.Objects[I]));
    End;
  End; // FixupNamedObject

Begin
  St1  := GetQuotedString(St);
  Name := St1;
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'bone' Then
    Begin
      Children.AddObject('',TAn8Bone.Create(Self,Figure));
      TAn8Bone(Children.Objects[Children.Count - 1]).ParseData(St2);
    End
    Else If St1 = 'length'   Then Len := StrToFloat(St2)
    Else If St1 = 'diameter' Then Diameter := StrToFloat(St2)
    Else If St1 = 'influence' Then
    Begin
      Influence.Center0    := StrToFloat(GetToken(' ',St2,True));
      Influence.InRadius0  := StrToFloat(GetToken(' ',St2,True));
      Influence.OutRadius0 := StrToFloat(GetToken(' ',St2,True));
      Influence.Center1    := StrToFloat(GetToken(' ',St2,True));
      Influence.InRadius1  := StrToFloat(GetToken(' ',St2,True));
      Influence.OutRadius1 := StrToFloat(GetToken(' ',St2,True));
    End
    Else If St1 = 'namedobject' Then
    Begin
      NO := TAn8NamedObject.Create(Self);
      NO.ParseData(St2);
      NamedObjects.AddObject(NO.Name,NO);
    End
    Else If St1 = 'orientation' Then
    Begin
      St2           := Trim(Copy(St2,2,Length(St2) - 2));
      Orientation.X := StrToFloat(GetToken(' ',St2,True));
      Orientation.Y := StrToFloat(GetToken(' ',St2,True));
      Orientation.Z := StrToFloat(GetToken(' ',St2,True));
      Orientation.W := StrToFloat(GetToken(' ',St2,True));
    End
    Else If St1 = 'dof' Then LoadDof(St2);
  End; // While

  // Copy the base and weightedby information from the first named object to all others

  If NamedObjects.Count > 1 Then
  Begin
    NO := TAn8NamedObject(NamedObjects.Objects[0]);
    For I := 1 To NamedObjects.Count - 1 Do
    Begin
      NO1 := TAn8NamedObject(NamedObjects.Objects[I]);
      NO1.Base.Copy(NO.Base);
      If NO1.WeightedBy.Count = 0 Then
      Begin
        For J := 0 To NO.WeightedBy.Count - 1 Do NO1.WeightedBy.AddObject(NO.WeightedBy.Strings[J],NO.WeightedBy.Objects[J]);
      End;
    End; // For I
  End;

  // Fixup any weightedby references

  For I := 0 To NamedObjects.Count - 1 Do FixupNamedObject(TAn8NamedObject(NamedObjects.Objects[I]),Self);
End; // TAn8Bone.ParseData

Procedure TAn8Bone.WriteToFile(Var F: Text; Indent: Integer);
Var
  IndentStr : String;
  I         : Integer;

Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'bone { "' + Name + '"');
  WriteLn(F,IndentStr + Format('  length { %7.5f }',[Len]));
  WriteLn(F,IndentStr + Format('  diameter { %7.5f }',[Diameter]));
  If Not Orientation.Equals(1,0,0,0) Then WriteLn(F,IndentStr + Format('  orientation { (%7.5f %7.5f %7.5f %7.5f) }',[Orientation.X,Orientation.Y,Orientation.Z,Orientation.W]));
  If (XDof.Minimum <> 0) Or (XDof.Default <> 0) Or (XDof.Maximum <> 0) Then WriteLn(F,IndentStr + Format('  dof { "X" %7.5f %7.5f %7.5f }',[XDof.Minimum, XDof.Default, XDof.Maximum]));
  If (YDof.Minimum <> 0) Or (YDof.Default <> 0) Or (YDof.Maximum <> 0) Then WriteLn(F,IndentStr + Format('  dof { "Y" %7.5f %7.5f %7.5f }',[YDof.Minimum, YDof.Default, YDof.Maximum]));
  If (ZDof.Minimum <> 0) Or (ZDof.Default <> 0) Or (ZDof.Maximum <> 0) Then WriteLn(F,IndentStr + Format('  dof { "Z" %7.5f %7.5f %7.5f }',[ZDof.Minimum, ZDof.Default, ZDof.Maximum]));
  If (Influence.Center0 <> 0) Or (Influence.InRadius0 <> 0) Or (Influence.OutRadius0 <> 0) Or
     (Influence.Center1 <> 0) Or (Influence.InRadius1 <> 0) Or (Influence.OutRadius1 <> 0) Then
   WriteLn(F,IndentStr + Format('  influence { %7.5f %7.5f %7.5f %7.5f %7.5f %7.5f }',
            [Influence.Center0,Influence.InRadius0,Influence.OutRadius0,Influence.Center1,Influence.InRadius1,Influence.OutRadius1]));
  For I := 0 To NamedObjects.Count - 1 Do TAn8NamedObject(NamedObjects.Objects[I]).WriteToFile(F,Indent + 2);          
  For I := 0 To Children.Count - 1 Do TAn8Bone(Children.Objects[I]).WriteToFile(F,Indent + 2);
  WriteLn(F,IndentStr + '}');
End; // TAn8Bone.WriteToFile

Function TAn8Bone.FindNamedObjects: TStringList;
Var
  I    : Integer;
  List : TStringList;

Begin
  If NamedObjects.Count > 0 Then Result := NamedObjects
  Else
  Begin
    I    := 0;
    List := Nil;
    While (I < Children.Count) And (List = Nil) Do
    Begin
      List := TAn8Bone(Children.Objects[I]).FindNamedObjects;
      Inc(I);
    End; // While
    Result := List;
  End;
End; // TAn8Bone.FindNamedObjects

Function TAn8Bone.FindStrongestBone(CurPos: T3DPoint; NO: TAn8NamedObject; Point: T3DPoint; Var HighestInfluence: Single): TAn8Bone;
Var
  Bone    : TAn8Bone;
  NewBone : TAn8Bone;
  I       : Integer;
  NewPos  : T3DPoint;
  BoneVec : T3DPoint;
  C0,C1   : T3DPoint;
  D,D0,D1 : Single;
  Plane   : TPlane;
  Normal  : T3DPoint;
  S       : Single;
  R0,R1   : Single;
  B       : TAn8Bone;

Begin
  // If this is the root bone, add it as the default

  If Parent = Nil
   Then Bone := Self
   Else Bone := Nil;

  // Figure out this bone's orientation

  BoneVec := T3DPoint.Create(0,1,0);
  B       := Self;
  While B <> Nil Do
  Begin
    B.Orientation.Transform(BoneVec);
    B := B.Parent;
  End; // While
  BoneVec.Multiply(Len);

  If Parent = Nil Then
  Begin
    CurPos.Copy(BoneVec);
    CurPos.Negate;
  End;

  // If this bone can influence vertices, see if it has greater influence

  If (NO <> Nil) And (NO.WeightedBy.IndexOfObject(Self) >= 0) Then
  Begin
    C0 := T3DPoint.Create(BoneVec);
    C1 := T3DPoint.Create(BoneVec);
    C0.Multiply(Influence.Center0);
    C1.Multiply(Influence.Center1);
    C0.Add(CurPos);
    C1.Add(CurPos);

    // See if the point is within the first inner threshold distance

    D0 := Point.DistanceFrom(C0);
    If D0 < Influence.InRadius0 Then
    Begin
      HighestInfluence := 1;
      Bone := Self;
    End
    Else
    Begin
      // See if the point is within the second inner threshold distance

      D1 := Point.DistanceFrom(C1);
      If D1 < Influence.InRadius1 Then
      Begin
        HighestInfluence := 1;
        Bone := Self;
      End
      Else
      Begin
        // Considering a plane located at CurPos with normal along BoneVec, see
        // if the point is between C0 and C1 in terms of distance from the plane

        Normal := T3DPoint.Create(BoneVec);
        Normal.Normalize;
        Plane := TPlane.Create;
        Plane.Setup(CurPos,Normal);
        Normal.Free;
        D := Plane.SignedDistanceFromPoint(Point);
        If (D >= Influence.Center0 * Len) And (D <= Influence.Center1 * Len) Then
        Begin
          // Get the fractional distance, where it is 0 at C0 and 1 at C1

          S := D;
          If Len <> 0 Then
          Begin
            S := (S / Len) - Influence.Center0;
            If C1 <> C0
             Then S := S / (Influence.Center1 - Influence.Center0)
             Else S := 0;
          End
          Else S := 0;

          // Calculate the radius in which we need to be for full influence

          R0 := Influence.InRadius0 + S * (Influence.InRadius1 - Influence.InRadius0);

          If Point.DistanceFromLine2(C0,C1) <= R0 * R0 Then
          Begin
            HighestInfluence := 1;
            Bone := Self;
          End;
        End;

        // Continue if this bone doesn't have full influence

        If HighestInfluence < 1 Then
        Begin
          // See if the point is within the first outer threshold distance

          If D0 < Influence.OutRadius0 Then
          Begin
            // Calculate the new influence and set it if it's higher than the old one
            
            S := D0 - Influence.InRadius0;
            If Influence.OutRadius0 > Influence.InRadius0
             Then S := 1 - (S / (Influence.OutRadius0 - Influence.InRadius0))
             Else S := 0;
            If S > HighestInfluence Then
            Begin
              HighestInfluence := S;
              Bone := Self;
            End;
          End
          Else
          Begin
            // See if the point is within the second outer threshold distance

            If D1 < Influence.OutRadius1 Then
            Begin
              // Calculate the new influence and set it if it's higher than the old one

              S := D1 - Influence.InRadius1;
              If Influence.OutRadius1 > Influence.InRadius1
               Then S := 1 - (S / (Influence.OutRadius1 - Influence.InRadius1))
               Else S := 0;
              If S > HighestInfluence Then
              Begin
                HighestInfluence := S;
                Bone := Self;
              End;
            End
            Else
            Begin
              If (D >= Influence.Center0 * Len) And (D <= Influence.Center1 * Len) Then
              Begin
                // Get the fractional distance, where it is 0 at C0 and 1 at C1

                S := D;
                If Len <> 0 Then
                Begin
                  S := (S / Len) - Influence.Center0;
                  If C1 <> C0
                   Then S := S / (Influence.Center1 - Influence.Center0)
                   Else S := 0;
                End
                Else S := 0;

                // Calculate the radius in which we need to be for full influence

                R0 := Influence.InRadius0  + S * (Influence.InRadius1  - Influence.InRadius0);
                R1 := Influence.OutRadius0 + S * (Influence.OutRadius1 - Influence.OutRadius0);

                D := Point.DistanceFromLine2(C0,C1);
                If D <= R1 * R1 Then
                Begin
                  S := Sqrt(D) - R0;
                  If R1 > R0
                   Then S := 1 - (S / (R1 - R0))
                   Else S := 0;
                  If S > HighestInfluence Then
                  Begin
                    HighestInfluence := S;
                    Bone := Self;
                  End;
                End;
              End;
            End;
          End;
        End;

        Plane.Free;
      End;
    End;

    C0.Free;
    C1.Free;
  End;

  If HighestInfluence < 1 Then
  Begin
    // Figure out the new position at the end of this bone

    NewPos := T3DPoint.Create(CurPos);
    NewPos.Add(BoneVec);

    // Check this bone's children

    For I := 0 To Children.Count - 1 Do
    Begin
      NewBone := TAn8Bone(Children.Objects[I]).FindStrongestBone(NewPos,NO,Point,HighestInfluence);
      If NewBone <> Nil Then Bone := NewBone;
    End; // For I
    NewPos.Free;
  End;

  // Cleanup

  BoneVec.Free;

  Result := Bone;
End; // TAn8Bone.FindStrongestBone

Procedure TAn8Bone.AssignIndex(Var NewIndex: Integer);
Var I: Integer;
Begin
  Index := NewIndex;
  Inc(NewIndex);
  For I := 0 To Children.Count - 1 Do TAn8Bone(Children.Objects[I]).AssignIndex(NewIndex);
End; // TAn8Bone.AssignIndex

Function TAn8Bone.FindHighestIndex: Integer;
Var I,J,K: Integer;
Begin
  J := Index;
  For I := 0 To Children.Count - 1 Do
  Begin
    K := TAn8Bone(Children.Objects[I]).FindHighestIndex;
    If K > J Then J := K;
  End; // For I
  Result := J;
End; // TAn8Bone.FindHighestIndex

Procedure TAn8Bone.GetBones(List: TStringList);
Var I: Integer;
Begin
  List.AddObject(Name,Self);
  For I := 0 To Children.Count - 1 Do TAn8Bone(Children.Objects[I]).GetBones(List);
End; // TAn8Bone.GetBones

Function TAn8Bone.IsAncestor(Bone: TAn8Bone): Boolean;
Begin
  Result := (Bone = Self);
  While (Bone <> Nil) And (Bone <> Self) Do
  Begin
    If Bone = Self
     Then Result := True
     Else Bone := Bone.Parent;
  End; // While
End; // TAn8Bone.IsAncestor

Procedure TAn8Bone.ScaleLength(Scale: Single; Recurse: Boolean);
Var I: Integer;
Begin
  Len := Len * Scale;
  If Recurse Then
  Begin
    For I := 0 To Children.Count - 1 Do TAn8Bone(Children.Objects[I]).ScaleLength(Scale,Recurse);
  End;
End; // TAn8Bone.ScaleLength

// ------------------------
// TAn8Figure
// ------------------------

Constructor TAn8Figure.Create(AParent: TAn8File);
Begin
  Name      := '';
  Parent    := AParent;
  RootBone  := Nil;
  Materials := TStringList.Create;
End; // TAn8Figure.Create

Destructor TAn8Figure.Destroy;
Begin
  RootBone.Free;
  KillStringList(Materials);
  Inherited;
End; // TAn8Figure.Destroy

Procedure TAn8Figure.ParseData(St: String);
Var
  St1,St2 : String;
  I       : Integer;

Begin
  St1  := GetQuotedString(St);
  Name := St1;
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'bone' Then
    Begin
      RootBone := TAn8Bone.Create(Nil,Self);
      RootBone.ParseData(St2);
    End
    Else If St1 = 'material' Then
    Begin
      Materials.AddObject('',TAn8Material.Create(Parent));
      TAn8Material(Materials.Objects[Materials.Count - 1]).ParseData(St2);
      Materials.Strings[Materials.Count - 1] := TAn8Material(Materials.Objects[Materials.Count - 1]).Name;
    End
  End; // While
  I := 0;
  If RootBone <> Nil Then
  Begin
    RootBone.AssignIndex(I);
    RootBone.CalculateOrigin(0,0,0);
  End;
End; // TAn8Figure.ParseData

Procedure TAn8Figure.WriteToFile(Var F: Text; Indent: Integer);
Var
  IndentStr : String;
  I         : Integer;

Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'figure { "' + Name + '"');
  For I := 0 To Materials.Count - 1 Do TAn8Material(Materials.Objects[I]).WriteToFile(F,Indent + 2);
  If RootBone <> Nil Then RootBone.WriteToFile(F,Indent + 2);
  WriteLn(F,IndentStr + '}');
End; // TAn8Figure.WriteToFile

Function TAn8Figure.FindBoneByName(St: String): TAn8Bone;

  Function CheckBone(Bone: TAn8Bone; St: String): TAn8Bone;
  Var
    I : Integer;
    B : TAn8Bone;

  Begin
    Result := Nil;
    If Bone <> Nil Then
    Begin
      If UpperCase(Bone.Name) = St Then Result := Bone
      Else
      Begin
        I := 0;
        B := Nil;
        While (I < Bone.Children.Count) And (B = Nil) Do
        Begin
          B := CheckBone(TAn8Bone(Bone.Children.Objects[I]),St);
          Inc(I);
        End; // While
        Result := B;
      End;
    End;
  End; // CheckBone

Begin
  St     := UpperCase(St);
  Result := CheckBone(RootBone,St);
End; // TAn8Figure.FindBoneByName

Procedure TAn8Figure.DeterminePrimaryBones;
Var
  List      : TStringList;
  I,K       : Integer;
  Component : TObject;
  Obj       : TAn8NamedObject;

Begin
  // First find the first object that the figure references, assuming that all bones
  // reference the same object

  If RootBone <> Nil Then
  Begin
    List := RootBone.FindNamedObjects;
    If List <> Nil Then
    Begin
      // Iterate through all objects in the list

      For K := 0 To List.Count - 1 Do
      Begin
        Obj := TAn8NamedObject(List.Objects[K]);
        If (Obj <> Nil) And (Obj.Obj <> Nil) Then
        Begin
          // Iterate through all meshes in the object

          For I := 0 To Obj.Obj.Components.Count - 1 Do
          Begin
            Component := Obj.Obj.Components.Objects[I];
            If Component Is TAn8Component Then TAn8Component(Component).DeterminePrimaryBones(Obj,RootBone);
          End; // For I
        End;
      End; // For K
    End;
  End;
End; // TAn8Figure.DeterminePrimaryBones

Function TAn8Figure.ReferencesObject(Obj: TAn8Object): Boolean;
Var
  NamedObjects : TStringList;
  I            : Integer;

Begin
  Result := False;
  If RootBone <> Nil Then
  Begin
    NamedObjects := RootBone.FindNamedObjects;
    If NamedObjects <> Nil Then
    Begin
      I := 0;
      While (I < NamedObjects.Count) And Not Result Do
      Begin
        If TAn8NamedObject(NamedObjects.Objects[I]).Obj = Obj
         Then Result := True
         Else Inc(I);
      End; // While
    End;
  End;
End; // TAn8Figure.ReferencesObject

// ------------------------
// TAn8FloatKey
// ------------------------

Constructor TAn8FloatKey.Create;
Begin
  Frame    := 0;
  Value    := 0;
  Unknown1 := 0;
  Unknown2 := 0;
  Key      := True;
End; // TAn8FloatKey.Create

// ------------------------
// TAn8Track
// ------------------------

Constructor TAn8Track.Create;
Begin
  Keys := TStringList.Create;
End; // TAn8Track.Create

Destructor TAn8Track.Destroy;
Begin
  KillStringList(Keys);
  Inherited;
End; // TAn8Track.Destroy

Procedure TAn8Track.ParseData(St: String);
Var
  St1,St2 : String;
  Key     : TAn8FloatKey;

Begin
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'floatkey' Then
    Begin
      Key := TAn8FloatKey.Create;
      Keys.AddObject('',Key);
      Key.Frame    := StrToInt(GetToken(' ',St2,True));
      Key.Value    := StrToFloat(GetToken(' ',St2,True));
      Key.Unknown1 := StrToFloat(GetToken(' ',St2,True));
      Key.Unknown2 := StrToFloat(GetToken(' ',St2,True));

      Key.Unknown1 := 0;
      Key.Unknown2 := 0;

      If LowerCase(GetQuotedString(St2)) = 's' Then Key.Key := True;
    End;
  End; // While
End; // TAn8Track.ParseData

Procedure TAn8Track.WriteToFile(Var F: Text; Indent: Integer);
Var
  IndentStr : String;
  I         : Integer;
  Key       : TAn8FloatKey;

Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'track {');
  For I := 0 To Keys.Count - 1 Do
  Begin
    Key := TAn8FloatKey(Keys.Objects[I]);
    If Key.Key
     Then WriteLn(F,IndentStr + Format('  floatkey { %d %5.3f %5.3f %5.3f "S" }',[Key.Frame,Key.Value,Key.Unknown1,Key.Unknown2]))
     Else WriteLn(F,IndentStr + Format('  floatkey { %d %5.3f %5.3f %5.3f " " }',[Key.Frame,Key.Value,Key.Unknown1,Key.Unknown2]));
  End; // For I
  WriteLn(F,IndentStr + '}');
End; // TAn8Track.WriteToFile

Function TAn8Track.FindKeyForFrame(WantedFrame: Integer): TAn8FloatKey;
Var
  I     : Integer;
  Found : Boolean;
  Key   : TAn8FloatKey;

Begin
  I      := 0;
  Found  := False;
  Result := Nil;
  While (I < Keys.Count) And Not Found Do
  Begin
    Key := TAn8FloatKey(Keys.Objects[I]);
    If Key.Frame = WantedFrame Then
    Begin
      Result := Key;
      Found  := True;
    End
    Else Inc(I);
  End; // While
End; // TAn8Track.FindKeyForFrame

Function TAn8Track.GetValueForFrame(WantedFrame: Integer): Single;
// Interpolates if necessary
Var
  I          : Integer;
  BelowFrame : Integer;
  BelowValue : Single;
  AboveFrame : Integer;
  AboveValue : Single;
  Key        : TAn8FloatKey;

Begin
  Result := 0;
  If (Keys.Count > 0) And (WantedFrame >= 0) Then
  Begin
    // Find the value for the lowest frame

    BelowFrame := Keys.Count;
    BelowValue := 0;
    For I := 0 To Keys.Count - 1 Do
    Begin
      Key := TAn8FloatKey(Keys.Objects[I]);
      If Key.Frame < BelowFrame Then
      Begin
        BelowFrame := Key.Frame;
        BelowValue := Key.Value;
      End;
    End; // For I

    // If the lowest frame is greater than the one we want, then interpolate

    If BelowFrame > WantedFrame Then Result := (WantedFrame / BelowFrame) * BelowValue
    Else
    Begin
      // Find the highest frame that is less than or equal to the index

      For I := 0 To Keys.Count - 1 Do
      Begin
        Key := TAn8FloatKey(Keys.Objects[I]);
        If (Key.Frame <= WantedFrame) And (Key.Frame > BelowFrame) Then
        Begin
          BelowFrame := Key.Frame;
          BelowValue := Key.Value;
        End;
      End; // For I

      // If the frame we found is equal to the one we want, then there is no need to interpolate

      If BelowFrame = WantedFrame Then Result := BelowValue
      Else
      Begin
        // Find the value for the highest frame

        AboveFrame := -1;
        AboveValue := 0;
        For I := 0 To Keys.Count - 1 Do
        Begin
          Key := TAn8FloatKey(Keys.Objects[I]);
          If Key.Frame > AboveFrame Then
          Begin
            AboveFrame := Key.Frame;
            AboveValue := Key.Value;
          End;
        End; // For I

        // If the highest frame is lower than the one we want, then that is the value (it remains constant)

        If AboveFrame <= WantedFrame Then Result := AboveValue
        Else
        Begin
          // Find the lowest frame that is greater than the index

          For I := 0 To Keys.Count - 1 Do
          Begin
            Key := TAn8FloatKey(Keys.Objects[I]);
            If (Key.Frame > WantedFrame) And (Key.Frame < AboveFrame) Then
            Begin
              AboveFrame := Key.Frame;
              AboveValue := Key.Value;
            End;
          End; // For I

          // Interpolate

          Result := BelowValue + (AboveValue - BelowValue) * (WantedFrame - BelowFrame) / (AboveFrame - BelowFrame);
        End;
      End;
    End;
  End;
End; // TAn8Track.GetValueForFrame

// ------------------------
// TAn8JointAngle
// ------------------------

Constructor TAn8JointAngle.Create(AParent: TAn8Sequence);
Begin
  Bone   := Nil;
  Axis   := axX;
  Track  := TAn8Track.Create;
  Parent := AParent;
End; // TAn8JointAbgle.Create

Destructor TAn8JointAngle.Destroy;
Begin
  Track.Free;
  Inherited;
End; // TAn8JointAngle.Destroy

Procedure TAn8JointAngle.ParseData(St: String);
Var St1,St2: String;
Begin
  St1  := GetQuotedString(St);
  Bone := Parent.Figure.FindBoneByName(St1);
  St1  := LowerCase(GetQuotedString(St));
       If St1 = 'x' Then Axis := axX
  Else If St1 = 'y' Then Axis := axY
  Else If St1 = 'z' Then Axis := axZ;
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'track' Then Track.ParseData(St2);
  End; // While
End; // TAn8JointAngle.ParseData

Procedure TAn8JointAngle.WriteToFile(Var F: Text; Indent: Integer);
Var IndentStr: String;
Begin
  IndentStr := GetIndent(Indent);
  Write(F,IndentStr + 'jointangle { ');
  If Bone <> Nil Then Write(F,'"' + Bone.Name + '" ');
  Case Axis Of
    axX: Write(F,'"X"');
    axY: Write(F,'"Y"');
    axZ: Write(F,'"Z"');
  End; // Case
  WriteLn(F);
  Track.WriteToFile(F,Indent + 2);
  WriteLn(F,IndentStr + '}');
End; // TAn8JointAngle.WriteToFile

// ------------------------
// TAn8Sequence
// ------------------------

Constructor TAn8Sequence.Create(AParent: TAn8File);
Begin
  Name        := '';
  Parent      := AParent;
  Figure      := Nil;
  Frames      := 0;
  JointAngles := TStringList.Create;
End; // TAn8Sequence.Create

Destructor TAn8Sequence.Destroy;
Begin
  KillStringList(JointAngles);
  Inherited;
End; // TAn8Sequence.Destroy

Procedure TAn8Sequence.ParseData(St: String);
Var
  St1,St2 : String;
  JA      : TAn8JointAngle;

Begin
  St1  := GetQuotedString(St);
  Name := St1;
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
         If St1 = 'figure' Then Figure := Parent.FindFigureByName(Copy(St2,2,Length(St2) - 2))
    Else If St1 = 'frames' Then Frames := StrToInt(St2)
    Else If St1 = 'jointangle' Then
    Begin
      JointAngles.AddObject('',TAn8JointAngle.Create(Self));
      TAn8JointAngle(JointAngles.Objects[JointAngles.Count - 1]).ParseData(St2);
      JA := TAn8JointAngle(JointAngles.Objects[JointAngles.Count - 1]);
      If JA.Bone = Nil Then
      Begin
        // It's possible to have the sequence reference bones that aren't in the figure.
        // In this case, get rid of the jointangle object.

        JA.Free;
        JointAngles.Delete(JointAngles.Count - 1);
      End
      Else
      Begin
        Case JA.Axis Of
          axX: JointAngles.Strings[JointAngles.Count - 1] := JA.Bone.Name + '_X';
          axY: JointAngles.Strings[JointAngles.Count - 1] := JA.Bone.Name + '_Y';
          axZ: JointAngles.Strings[JointAngles.Count - 1] := JA.Bone.Name + '_Z';
        End; // Case
      End;
    End;
  End; // While
  JointAngles.Sorted := True;
End; // TAn8Sequence.ParseData

Procedure TAn8Sequence.WriteToFile(Var F: Text; Indent: Integer);
Var
  IndentStr : String;
  I         : Integer;

Begin
  IndentStr := GetIndent(Indent);
  WriteLn(F,IndentStr + 'sequence { "' + Name + '"');
  If Figure <> Nil Then WriteLn(F,IndentStr + '  figure { "' + Figure.Name + '" }');
  WriteLn(F,IndentStr + '  frames { ' + IntToStr(Frames) + ' }');
  For I := 0 To JointAngles.Count - 1 Do TAn8JointAngle(JointAngles.Objects[I]).WriteToFile(F,Indent + 2);
  WriteLn(F,IndentStr + '}');
End; // TAn8Sequence.WriteToFile

Function TAn8Sequence.GetRootBone(Fraction: Single): TAn8Bone;
Var NewRoot: TAn8Bone;

  Procedure RotateBone(Fraction: Single; Bone: TAn8Bone; Sequence: TAn8Sequence);
  Var
    I           : Integer;
    JointAngleX : TAn8JointAngle;
    JointAngleY : TAn8JointAngle;
    JointAngleZ : TAn8JointAngle;
    Q           : TQuaternion;
    Q1          : TQuaternion;
    V           : T3DPoint;

  Begin
    I := Sequence.JointAngles.IndexOf(Bone.Name + '_X');
    If I >= 0
     Then JointAngleX := TAn8JointAngle(Sequence.JointAngles.Objects[I])
     Else JointAngleX := Nil;

    I := Sequence.JointAngles.IndexOf(Bone.Name + '_Y');
    If I >= 0
     Then JointAngleY := TAn8JointAngle(Sequence.JointAngles.Objects[I])
     Else JointAngleY := Nil;

    I := Sequence.JointAngles.IndexOf(Bone.Name + '_Z');
    If I >= 0
     Then JointAngleZ := TAn8JointAngle(Sequence.JointAngles.Objects[I])
     Else JointAngleZ := Nil;

    Q  := TQuaternion.Create(Bone.Orientation);
    Q1 := TQuaternion.Create;
    V  := T3DPoint.Create;

    If JointAngleX <> Nil Then
    Begin
      V.Copy(1,0,0);
      Q.Transform(V);
      Q1.FromAngleAxis(JointAngleX.Track.GetValueForFrame(Round(Fraction * Sequence.Frames)) * Pi / 180,V);
      Q1.Multiply(Q);
      Q.Copy(Q1);
    End;

    If JointAngleZ <> Nil Then
    Begin
      V.Copy(0,0,1);
      Q.Transform(V);
      Q1.FromAngleAxis(JointAngleZ.Track.GetValueForFrame(Round(Fraction * Sequence.Frames)) * Pi / 180,V);
      Q1.Multiply(Q);
      Q.Copy(Q1);
    End;

    If JointAngleY <> Nil Then
    Begin
      V.Copy(0,1,0);
      Q.Transform(V);
      Q1.FromAngleAxis(JointAngleY.Track.GetValueForFrame(Round(Fraction * Sequence.Frames)) * Pi / 180,V);
      Q1.Multiply(Q);
      Q.Copy(Q1);
    End;

    Bone.Orientation.Copy(Q);

    V.Free;
    Q1.Free;
    Q.Free;

    For I := 0 To Bone.Children.Count - 1 Do RotateBone(Fraction,TAn8Bone(Bone.Children.Objects[I]),Sequence);
  End; // RotateBone

Begin
  Fraction := Min(1,Max(0,Fraction));
  NewRoot  := TAn8Bone.Create(Figure.RootBone);
  RotateBone(Fraction,NewRoot,Self);
  NewRoot.CalculateOrigin(0,0,0);
  Result   := NewRoot;
End; // TAn8Sequence.GetRootBone

Function TAn8Sequence.FindJointAngle(Bone: TAn8Bone): TAn8JointAngle;
Var
  I          : Integer;
  JointAngle : TAn8JointAngle;

Begin
  Result := Nil;
  I      := 0;
  While (I < JointAngles.Count) And (Result = Nil) Do
  Begin
    JointAngle := TAn8JointAngle(JointAngles.Objects[I]);
    If (JointAngle <> Nil) And (JointAngle.Bone = Bone)
     Then Result := JointAngle
     Else Inc(I);
  End; // While
End; // TAn8Sequence.FindJointAngle

Function TAn8Sequence.FindJointAngle(BoneName: String; Axis: TAn8Axis): TAn8JointAngle;
Var
  I          : Integer;
  JointAngle : TAn8JointAngle;

Begin
  BoneName := LowerCase(Trim(BoneName));
  Result := Nil;
  I      := 0;
  While (I < JointAngles.Count) And (Result = Nil) Do
  Begin
    JointAngle := TAn8JointAngle(JointAngles.Objects[I]);
    If (JointAngle <> Nil) And (JointAngle.Axis = Axis) And (LowerCase(Trim(JointAngle.Bone.Name)) = BoneName)
     Then Result := JointAngle
     Else Inc(I);
  End; // While
End; // TAn8Sequence.FindJointAngle

Procedure TAn8Sequence.CopyFrom(Src: TAn8Sequence);
// Makes this sequence a copy of Src
Var
  I,J    : Integer;
  St     : String;
  SrcJA  : TAn8JointAngle;
  DstJA  : TAn8JointAngle;
  SrcKey : TAn8FloatKey;
  DstKey : TAn8FloatKey;
  
Begin
  // Clear this sequence

  For I := 0 To JointAngles.Count - 1 Do JointAngles.Objects[I].Free;
  JointAngles.Clear;

  // Copy the source sequence

  Frames := Src.Frames;
  For I := 0 To Src.JointAngles.Count - 1 Do
  Begin
    SrcJA := TAn8JointAngle(Src.JointAngles.Objects[I]);
    If SrcJA.Bone <> Nil Then
    Begin
      DstJA      := TAn8JointAngle.Create(Self);
      DstJA.Bone := SrcJA.Bone;
      DstJA.Axis := SrcJA.Axis;
      For J := 0 To SrcJA.Track.Keys.Count - 1 Do
      Begin
        SrcKey          := TAn8FloatKey(SrcJA.Track.Keys.Objects[J]);
        DstKey          := TAn8FloatKey.Create;
        DstKey.Frame    := SrcKey.Frame;
        DstKey.Value    := SrcKey.Value;
        DstKey.Unknown1 := SrcKey.Unknown1;
        DstKey.Unknown2 := SrcKey.Unknown2;
        DstKey.Key      := SrcKey.Key;
        DstJA.Track.Keys.AddObject('',DstKey);
      End; // For J
      Case DstJA.Axis Of
        axX: St := DstJA.Bone.Name + '_X';
        axY: St := DstJA.Bone.Name + '_Y';
        axZ: St := DstJA.Bone.Name + '_Z';
      End; // Case
      JointAngles.AddObject(St,DstJA);
    End;
  End; // For I
End; // TAn8Sequence.CopyFrom

// ------------------------
// TAn8File
// ------------------------

Constructor TAn8File.Create;
Begin
  Header.Version := '0.95';
  Header.Build   := '2006.12.02';
  FillChar(Environment,SizeOf(Environment),0);
  Objects        := TStringList.Create;
  Figures        := TStringList.Create;
  Sequences      := TStringList.Create;
  Textures       := TStringList.Create;
  Materials      := TStringList.Create;
  Loaded         := False;
End; // TAn8File.Create

Destructor TAn8File.Destroy;
Begin
  KillStringList(Objects);
  KillStringList(Figures);
  KillStringList(Sequences);
  KillStringList(Textures);
  KillStringList(Materials);
  Inherited;
End; // TAn8File.Destroy

Procedure TAn8File.GetBounds(MinPt,MaxPt: T3DPoint);
Var
  I         : Integer;
  Obj       : TObject;
  Min1,Max1 : T3DPoint;
  First     : Boolean;

Begin
  Min1  := T3DPoint.Create;
  Max1  := T3DPoint.Create;
  First := True;
  For I := 0 To Objects.Count - 1 Do
  Begin
    Obj := Objects.Objects[I];
    If Obj Is TAn8Object Then
    Begin
      TAn8Object(Obj).GetBounds(Min1,Max1);
      If First Then
      Begin
        MinPt.Copy(Min1);
        MaxPt.Copy(Max1);
        First := False;
      End
      Else
      Begin
        If Min1.X < MinPt.X Then MinPt.X := Min1.X;
        If Min1.Y < MinPt.Y Then MinPt.Y := Min1.Y;
        If Min1.Z < MinPt.Z Then MinPt.Z := Min1.Z;
        If Max1.X > MaxPt.X Then MaxPt.X := Max1.X;
        If Max1.Y > MaxPt.Y Then MaxPt.Y := Max1.Y;
        If Max1.Z > MaxPt.Z Then MaxPt.Z := Max1.Z;
      End;
    End;
  End; // For I
  Max1.Free;
  Min1.Free;
End; // TAn8File.GetBounds

Procedure TAn8File.LoadFromFile(FileName: String);
Var
  F      : Text;
  St     : String;
  St1    : String;
  List   : TStringList;
  I,J,K  : Integer;
  Obj    : TAn8Object;
  DecSep : Char;

  Procedure ProcessComponent(Component: TAn8Component);
  Var I: Integer;
  Begin
    If Component Is TAn8Mesh Then
    Begin
      TAn8Mesh(Component).DeleteOrphanedPoints;
      TAn8Mesh(Component).DeleteOrphanedTexCoords;
    End
    Else If Component Is TAn8Group Then
    Begin
      For I := 0 To TAn8Group(Component).Components.Count - 1 Do ProcessComponent(TAn8Component(TAn8Group(Component).Components.Objects[I]));
    End;
  End; // ProcessComponent

Begin
  If FileExists(FileName) Then
  Begin
    DecSep := DecimalSeparator;
    DecimalSeparator := '.';
    Clear;
    AssignFile(F,FileName);
    Reset(F);
    St := '';
    List := TStringList.Create;
    J := 0;
    While Not Eof(F) Do
    Begin
      ReadLn(F,St1);
      List.Add(St1);
      Inc(J,Length(St1));
    End; // While
    SetLength(St,J);
    J := 1;
    For I := 0 To List.Count - 1 Do
    Begin
      St1 := List.Strings[I];
      K   := Length(St1);
      If K > 0 Then
      Begin
        Move(St1[1],St[J],K);
        Inc(J,K);
      End;
    End; // For I
    List.Free;
    CloseFile(F);
    ParseData(St);

    // Do some cleanup of meshes

    For I := 0 To Objects.Count - 1 Do
    Begin
      Obj := TAn8Object(Objects.Objects[I]);
      For J := 0 To Obj.Components.Count - 1 Do ProcessComponent(TAn8Component(Obj.Components.Objects[J]));
    End; // For I
    Loaded := True;
    DecimalSeparator := DecSep;
  End;
End; // TAn8File.LoadFromFile

Procedure TAn8File.Clear;
Var I: Integer;
Begin
  Header.Version := '0.95';
  Header.Build   := '2006.12.02';
  FillChar(Environment,SizeOf(Environment),0);
  ClearStringList(Materials);
  ClearStringList(Objects);
  ClearStringList(Figures);
  ClearStringList(Sequences);
  ClearStringList(Textures);
  Loaded := False;
End; // TAn8File.Clear

Procedure TAn8File.ParseData(St: String);
Var St1,St2: String;

  Procedure LoadHeader(St: String);
  Var St1,St2: String;
  Begin
    While St <> '' Do
    Begin
      St1 := LowerCase(GetFirstWordOrNumber(St));
      St2 := GetBracketedContents(St,'{','}');
           If St1 = 'version' Then Header.Version := Copy(St2,2,Length(St2) - 2)
      Else If St1 = 'build'   Then Header.Build   := Copy(St2,2,Length(St2) - 2);
    End; // While
  End; // LoadHeader

  Procedure LoadEnvironment(St: String);
  Var St1: String;
  Begin
    While St <> '' Do
    Begin
      St1 := LowerCase(GetFirstWordOrNumber(St));
      St2 := GetBracketedContents(St,'{','}');
      If St1 = 'grid' Then
      Begin
        Environment.Grid.I1 := StrToInt(GetToken(' ',St2,True));
        Environment.Grid.S2 := StrToFloat(GetToken(' ',St2,True));
        Environment.Grid.S3 := StrToFloat(GetToken(' ',St2,True));
        Environment.Grid.S4 := StrToFloat(GetToken(' ',St2,True));
      End
      Else If St1 = 'framerate'     Then Environment.FrameRate     := StrToInt(St2)
      Else If St1 = 'limitplayback' Then Environment.LimitPlayback := True;
    End; // While
  End; // LoadEnvironment

Begin
  While St <> '' Do
  Begin
    St1 := LowerCase(GetFirstWordOrNumber(St));
    St2 := GetBracketedContents(St,'{','}');
         If St1 = 'header'      Then LoadHeader(St2)
    Else If St1 = 'environment' Then LoadEnvironment(St2)
    Else If St1 = 'object' Then
    Begin
      Objects.AddObject('',TAn8Object.Create(Self));
      TAn8Object(Objects.Objects[Objects.Count - 1]).ParseData(St2);
      Objects.Strings[Objects.Count - 1] := TAn8Object(Objects.Objects[Objects.Count - 1]).Name;
    End
    Else If St1 = 'figure' Then
    Begin
      Figures.AddObject('',TAn8Figure.Create(Self));
      TAn8Figure(Figures.Objects[Figures.Count - 1]).ParseData(St2);
      Figures.Strings[Figures.Count - 1] := TAn8Figure(Figures.Objects[Figures.Count - 1]).Name;
    End
    Else If St1 = 'sequence' Then
    Begin
      Sequences.AddObject('',TAn8Sequence.Create(Self));
      TAn8Sequence(Sequences.Objects[Sequences.Count - 1]).ParseData(St2);
      Sequences.Strings[Sequences.Count - 1] := TAn8Sequence(Sequences.Objects[Sequences.Count - 1]).Name;
    End
    Else If St1 = 'texture' Then
    Begin
      Textures.AddObject('',TAn8Texture.Create(Self));
      TAn8Texture(Textures.Objects[Textures.Count - 1]).ParseData(St2);
      Textures.Strings[Textures.Count - 1] := TAn8Texture(Textures.Objects[Textures.Count - 1]).Name;
    End
    Else If St1 = 'material' Then
    Begin
      Materials.AddObject('',TAn8Material.Create(Self));
      TAn8Material(Materials.Objects[Materials.Count - 1]).ParseData(St2);
      Materials.Strings[Materials.Count - 1] := TAn8Material(Materials.Objects[Materials.Count - 1]).Name;
    End;
  End; // While
End; // TAn8File.ParseData

Procedure TAn8File.WriteToFile(Var F: Text);
Var I: Integer;
Begin
  WriteLn(F,'header {');
  WriteLn(F,'  version { "' + Header.Version + '" }');
  WriteLn(F,'  build { "' + Header.Build + '" }');
  WriteLn(F,'}');
  WriteLn(F,'environment {');
  WriteLn(F,Format('  grid { %d %5.3f %5.3f %5.3f }',[Environment.Grid.I1,Environment.Grid.S2,Environment.Grid.S3,Environment.Grid.S4]));
  WriteLn(F,'  framerate { ' + IntToStr(Environment.FrameRate) + ' }');
  If Environment.LimitPlayback Then WriteLn(F,'  limitplayback { }');
  WriteLn(F,'}');
  For I := 0 To Textures.Count - 1 Do TAn8Texture(Textures.Objects[I]).WriteToFile(F,0);
  For I := 0 To Materials.Count - 1 Do TAn8Material(Materials.Objects[I]).WriteToFile(F,0);
  For I := 0 To Objects.Count - 1 Do TAn8Object(Objects.Objects[I]).WriteToFile(F,0);
  For I := 0 To Figures.Count - 1 Do TAn8Figure(Figures.Objects[I]).WriteToFile(F,0);
  For I := 0 To Sequences.Count - 1 Do TAn8Sequence(Sequences.Objects[I]).WriteToFile(F,0);
End; // TAn8File.WriteToFile

Procedure TAn8File.SaveToFile(FileName: String);
Var F: Text;
Begin
  AssignFile(F,FileName);
  ReWrite(F);
  WriteToFile(F);
  CloseFile(F);
End; // TAn8File.SaveToFile

Function TAn8File.FindObjectByName(St: String): TAn8Object;
Var
  I       : Integer;
  Found   : Boolean;
  _Object : TAn8Object;

Begin
  I       := 0;
  Found   := False;
  _Object := Nil;
  While (I < Objects.Count) And Not Found Do
  Begin
    If TAn8Object(Objects.Objects[I]).Name = St Then
    Begin
      Found   := True;
      _Object := TAn8Object(Objects.Objects[I]);
    End
    Else Inc(I);
  End; // While
  Result := _Object;
End; // TAn8File.FindObjectByName

Function TAn8File.FindFigureByName(St: String): TAn8Figure;
Var
  I      : Integer;
  Found  : Boolean;
  Figure : TAn8Figure;

Begin
  I      := 0;
  Found  := False;
  Figure := Nil;
  While (I < Figures.Count) And Not Found Do
  Begin
    If TAn8Figure(Figures.Objects[I]).Name = St Then
    Begin
      Found  := True;
      Figure := TAn8Figure(Figures.Objects[I]);
    End
    Else Inc(I);
  End; // While
  Result := Figure;
End; // TAn8File.FindObjectByName

// ------------------------
// TDirectXMaterial
// ------------------------

Constructor TDirectXMaterial.Create;
Begin
  TextureFileName := '';
End; // TDirectXMaterial.Create

Constructor TDirectXMaterial.Create(Material: TDirectXMaterial);
Begin
  Name            := Material.Name;
  TextureFileName := Material.TextureFileName;
End; // TDirectXMaterial.Create

Destructor TDirectXMaterial.Destroy;
Begin
End; // TDirectXMaterial.Destroy

Procedure TDirectXMaterial.ParseData(St: String);
Var St1,St2: String;
Begin
  While St <> '' Do
  Begin
    St1 := LowerCase(GetToken(' ',St,True));
    If Copy(St,1,1) = '{' Then St2 := GetBracketedContents(St,'{','}');
    If St1 = 'texturefilename' Then TextureFileName := GetQuotedString(St2);
  End; // While
End; // TDirectXMaterial.ParseData

// ------------------------
// TDirectXFace
// ------------------------

Constructor TDirectXFace.Create;
Begin
  Material := Nil;
  SetLength(Points,0);
  SetLength(Normals,0);
End; // TDirectXFace.Create

Destructor TDirectXFace.Destroy;
Begin
  SetLength(Points,0);
  SetLength(Normals,0);
End; // TDirectXFace.Destroy

Procedure TDirectXFace.ParseData(St: String);
Begin
End; // TDirectXFace.ParseData

// ------------------------
// TDirectXMesh
// ------------------------

Constructor TDirectXMesh.Create;
Begin
  Points    := TStringList.Create;
  Normals   := TStringList.Create;
  TexCoords := TStringList.Create;
  Faces     := TStringList.Create;
  Materials := TStringList.Create;
End; // TDirectXMesh.Create

Destructor TDirectXMesh.Destroy;
Begin
  KillStringList(Points);
  KillStringList(Normals);
  KillStringList(TexCoords);
  KillStringList(Faces);
  KillStringList(Materials);
End; // TDirectXMesh.Destroy

Procedure TDirectXMesh.ParseData(St: String);
Var St1,St2: String;

  Procedure LoadPoints(Var St: String);
  Var
    APoints : TXYZPointArray;
    Point   : T3DPoint;
    I       : Integer;

  Begin
    GetDXPoints(APoints,St);
    For I := 0 To High(APoints) Do
    Begin
      Point   := T3DPoint.Create;
      Point.X := APoints[I].X;
      Point.Y := APoints[I].Y;
      Point.Z := APoints[I].Z;
      Points.AddObject(Point.GetCloseID(100),Point);
    End; // For I
    SetLength(APoints,0);
  End; // LoadPoints

  Procedure LoadFaces(Var St: String);
  Var
    I,J,K,L : Integer;
    Face    : TDirectXFace;
    St1     : String;

  Begin
    // First find out how many faces there are

    St1 := GetToken(';',St,True);
    Val(St1,J,I);

    // Allocate the faces and get the data

    For I := 0 To J - 1 Do
    Begin
      Face := TDirectXFace.Create;
      Faces.AddObject('',Face);
      Val(GetToken(';',St,True),K,L);
      SetLength(Face.Points,K);
      St1 := GetToken(';',St,True);
      For K := 0 To High(Face.Points) Do Val(GetToken(',',St1,True),Face.Points[K],L);
      If I < J - 1
       Then GetToken(',',St,True)
       Else GetToken(';',St,True);
    End; // For I
  End; // LoadFaces

  Procedure LoadNormals(Var St: String);
  Var
    APoints : TXYZPointArray;
    Point   : T3DPoint;
    I       : Integer;

  Begin
    GetDXPoints(APoints,St);
    For I := 0 To High(APoints) Do
    Begin
      Point   := T3DPoint.Create;
      Point.X := APoints[I].X;
      Point.Y := APoints[I].Y;
      Point.Z := APoints[I].Z;
      Normals.AddObject(Point.GetCloseID(100),Point);
    End; // For I
    SetLength(APoints,0);
  End; // LoadNormals

  Procedure LoadFaceNormals(Var St: String);
  Var
    I,J,K,L : Integer;
    Face    : TDirectXFace;
    St1     : String;

  Begin
    // First find out how many faces there are

    St1 := GetToken(';',St,True);
    Val(St1,J,I);

    // Allocate the faces and get the data

    For I := 0 To J - 1 Do
    Begin
      Face := TDirectXFace(Faces.Objects[I]);
      Val(GetToken(';',St,True),K,L);
      SetLength(Face.Normals,K);
      St1 := GetToken(';',St,True);
      For K := 0 To High(Face.Normals) Do Val(GetToken(',',St1,True),Face.Normals[K],L);
      If I < J - 1
       Then GetToken(',',St,True)
       Else GetToken(';',St,True);
    End; // For I
  End; // LoadFaceNormals

  Procedure LoadTexCoords(St: String);
  Var
    ATexCoords : TTexCoordArray;
    TexCoord   : TAn8TexCoord;
    I          : Integer;

  Begin
    GetDXTexCoords(ATexCoords,St);
    For I := 0 To High(ATexCoords) Do
    Begin
      TexCoord    := TAn8TexCoord.Create;
      TexCoord.TX := ATexCoords[I].TX;
      TexCoord.TZ := ATexCoords[I].TZ;
      TexCoords.AddObject('',TexCoord);
    End; // For I
    SetLength(ATexCoords,0);
  End; // LoadTexCoords

  Procedure LoadMaterials(St: String);
  Var
    I,J          : Integer;
    NumMaterials : Integer;
    NumFaces     : Integer;
    MatIndices   : Array Of Integer;
    Face         : TDirectXFace;
    St1          : String;
    St2          : String;
    St3          : String;
    Material     : TDirectXMaterial;

  Begin
    // First find out how many materials there are

    St1 := GetToken(';',St,True);
    Val(St1,NumMaterials,I);

    // Next find out how many faces there are

    St1 := GetToken(';',St,True);
    Val(St1,NumFaces,I);

    // Build a material index list

    SetLength(MatIndices,NumFaces);
    St1 := GetToken(';',St,True);
    For I := 0 To High(MatIndices) Do Val(GetToken(',',St1,True),MatIndices[I],J);
    GetToken(';',St,True);

    // Look for materials

    While St <> '' Do
    Begin
      St1 := LowerCase(GetToken(' ',St,True));
      If St1 = 'material' Then
      Begin
        St2       := GetToken(' ',St,True);
        St3       := GetBracketedContents(St,'{','}');
        Material  := TDirectXMaterial.Create;
        Material.Name := St2;
        Materials.AddObject(Material.Name,Material);
        Material.ParseData(St3);
      End
      Else
      Begin
        // The stuff in the brackets is a material name

        St2 := GetBracketedContents(St,'{','}');
      End;
    End; // While

    // Fixup materials

    For I := 0 To High(MatIndices) Do
    Begin
      Face := TDirectXFace(Faces.Objects[I]);
      If (MatIndices[I] >= 0) And (MatIndices[I] < Materials.Count)
       Then Face.Material := TDirectXMaterial(Materials.Objects[MatIndices[I]])
       Else Face.Material := Nil;
    End; // For I

    // Cleanup

    SetLength(MatIndices,0);
  End; // LoadMaterials

Begin
  LoadPoints(St);
  LoadFaces(St);
  While St <> '' Do
  Begin
    St1 := LowerCase(GetToken(' ',St,True));
    St2 := GetBracketedContents(St,'{','}');
    If St1 = 'meshnormals' Then
    Begin
      LoadNormals(St2);
      LoadFaceNormals(St2);
    End
    Else If St1 = 'meshtexturecoords' Then LoadTexCoords(St2)
    Else If St1 = 'meshmateriallist'  Then LoadMaterials(St2);
  End; // While
End; // TDirectXMesh.ParseData

// ------------------------
// TDirectXFrame
// ------------------------

Constructor TDirectXFrame.Create;
Begin
  TransformMatrix := T4x4Matrix.Create;
  Meshes          := TStringList.Create;
  Frames          := TStringList.Create;
  Name            := '';
  TransformMatrix.LoadIdentity;
End; // TDirectXFrame.Create

Destructor TDirectXFrame.Destroy;
Begin
  TransformMatrix.Free;
  KillStringList(Meshes);
  KillStringList(Frames);
End; // TDirectXFrame.Destroy

Procedure TDirectXFrame.ParseData(St: String);
Var
  St1,St2,St3 : String;
  Mesh        : TDirectXMesh;
  Frame       : TDirectXFrame;
  I           : Integer;
  S           : Array[0..15] Of Single;

Begin
  While St <> '' Do
  Begin
    St1 := LowerCase(GetToken(' ',St,True));
    If St1 = 'frametransformmatrix' Then
    Begin
      St3 := GetBracketedContents(St,'{','}');
      St3 := GetToken(';',St3,True);
      For I := 0 To 15 Do S[I] := StrToFloat(GetToken(',',St3,True));
      TransformMatrix.M[1,1] := S[0];
      TransformMatrix.M[2,1] := S[1];
      TransformMatrix.M[3,1] := S[2];
      TransformMatrix.M[4,1] := S[3];
      TransformMatrix.M[1,2] := S[4];
      TransformMatrix.M[2,2] := S[5];
      TransformMatrix.M[3,2] := S[6];
      TransformMatrix.M[4,2] := S[7];
      TransformMatrix.M[1,3] := S[8];
      TransformMatrix.M[2,3] := S[9];
      TransformMatrix.M[3,3] := S[10];
      TransformMatrix.M[4,3] := S[11];
      TransformMatrix.M[1,4] := S[12];
      TransformMatrix.M[2,4] := S[13];
      TransformMatrix.M[3,4] := S[14];
      TransformMatrix.M[4,4] := S[15];
    End
    Else If St1 = 'mesh' Then
    Begin
      St2  := GetToken(' ',St,True);
      St3  := GetBracketedContents(St,'{','}');
      Mesh := TDirectXMesh.Create;
      Mesh.Name := St2;
      Meshes.AddObject(Mesh.Name,Mesh);
      Mesh.ParseData(St3);
    End
    Else If St1 = 'frame' Then
    Begin
      St2  := GetToken(' ',St,True);
      St3  := GetBracketedContents(St,'{','}');
      Frame := TDirectXFrame.Create;
      Frame.Name := St2;
      Frames.AddObject(Frame.Name,Frame);
      Frame.ParseData(St3);
    End;
  End; // While
End; // TDirectXFrame.ParseData

// ------------------------
// TDirectXFile
// ------------------------

Constructor TDirectXFile.Create;
Begin
  Frames := TStringList.Create;
  Loaded := False;
End; // TDirectXFile.Create

Destructor TDirectXFile.Destroy;
Begin
  KillStringList(Frames);
End; // TDirectXFile.Destroy

Procedure TDirectXFile.LoadFromFile(FileName: String);
Var
  F      : Text;
  St     : String;
  St1    : String;
  List   : TStringList;
  I,J,K  : Integer;
  Frame  : TDirectXFrame;
  DecSep : Char;

  Procedure ProcessFrame(Frame: TDirectXFrame; Path: String);
  Var
    I,J      : Integer;
    Mesh     : TDirectXMesh;
    Material : TDirectXMaterial;

  Begin
    For I := 0 To Frame.Frames.Count - 1 Do ProcessFrame(TDirectXFrame(Frame.Frames.Objects[I]),Path);
    For I := 0 To Frame.Meshes.Count - 1 Do
    Begin
      Mesh := TDirectXMesh(Frame.Meshes.Objects[I]);
      For J := 0 To Mesh.Materials.Count - 1 Do
      Begin
        Material := TDirectXMaterial(Mesh.Materials.Objects[J]);
        Material.TextureFileName := Path + Material.TextureFileName;
      End; // For J
    End; // For I
  End; // ProcessFrame

Begin
  If FileExists(FileName) Then
  Begin
    DecSep := DecimalSeparator;
    DecimalSeparator := '.';
    Clear;
    AssignFile(F,FileName);
    Reset(F);
    St := '';
    List := TStringList.Create;
    J := 0;
    While Not Eof(F) Do
    Begin
      ReadLn(F,St1);
      St1 := St1 + ' ';
      If Copy(Trim(St1),1,1) <> '#' Then
      Begin
        List.Add(St1);
        Inc(J,Length(St1));
      End;
    End; // While
    SetLength(St,J);
    J := 1;
    For I := 0 To List.Count - 1 Do
    Begin
      St1 := List.Strings[I];
      K   := Length(St1);
      If K > 0 Then
      Begin
        Move(St1[1],St[J],K);
        Inc(J,K);
      End;
    End; // For I
    List.Free;
    CloseFile(F);
    ParseData(St);

    // Fixup material paths

    For I := 0 To Frames.Count - 1 Do ProcessFrame(TDirectXFrame(Frames.Objects[I]),ExtractFilePath(FileName));

    Loaded := True;
    DecimalSeparator := DecSep;
  End;
End; // TDirectXFile.LoadFromFile

Procedure TDirectXFile.Clear;
Var I: Integer;
Begin
  ClearStringList(Frames);
  Loaded := False;
End; // TDirectXFile.Clear

Procedure TDirectXFile.ParseData(St: String);
Var
  St1,St2,St3  : String;
  Frame        : TDirectXFrame;
  DefaultFrame : TDirectXFrame;
  Mesh         : TDirectXMesh;
  Material     : TDirectXMaterial;
  Materials    : TStringList;
  I            : Integer;

Begin
  DefaultFrame := Nil;
  Materials    := TStringList.Create;
  While St <> '' Do
  Begin
    St1 := LowerCase(GetToken(' ',St,True));
    If St1 = 'frame' Then
    Begin
      St2 := GetToken(' ',St,True);
      If Copy(St,1,1) = '{' Then St3 := GetBracketedContents(St,'{','}');
      Frame := TDirectXFrame.Create;
      Frame.Name := St2;
      Frames.AddObject(Frame.Name,Frame);
      Frame.ParseData(St3);
    End
    Else If St1 = 'mesh' Then
    Begin
      If DefaultFrame = Nil Then
      Begin
        DefaultFrame      := TDirectXFrame.Create;
        DefaultFrame.Name := 'Default';
        Frames.InsertObject(0,DefaultFrame.Name,DefaultFrame);
      End;
      St2  := GetToken(' ',St,True);
      St3  := GetBracketedContents(St,'{','}');
      Mesh := TDirectXMesh.Create;
      Mesh.Name := St2;
      DefaultFrame.Meshes.AddObject(Mesh.Name,Mesh);

      // Add any materials we found

      For I := 0 To Materials.Count - 1 Do
      Begin
        Material := TDirectXMaterial(Materials.Objects[I]);

        // Add a copy of each material because we might have to add them to multiple meshes

        Mesh.Materials.AddObject(Material.Name,TDirectXMaterial.Create(Material));
      End; // For I
      Mesh.ParseData(St3);
    End
    Else If St1 = 'material' Then
    Begin
      If DefaultFrame = Nil Then
      Begin
        DefaultFrame      := TDirectXFrame.Create;
        DefaultFrame.Name := 'Default';
        Frames.InsertObject(0,DefaultFrame.Name,DefaultFrame);
      End;
      St2       := GetToken(' ',St,True);
      St3       := GetBracketedContents(St,'{','}');
      Material  := TDirectXMaterial.Create;
      Material.Name := St2;
      Materials.AddObject(Material.Name,Material);
      Material.ParseData(St3);
    End
    Else If St1 = 'header' Then
    Begin
      St3 := GetBracketedContents(St,'{','}');
    End;
  End; // While
  KillStringList(Materials);
End; // TDirectXFile.ParseData

Function TDirectXFile.CreateAn8File: TAn8File;
Var
  An8File : TAn8File;
  I       : Integer;
  Frame   : TDirectXFrame;
  Matrix  : T4x4Matrix;
  An8Obj  : TAn8Object;

  Procedure ProcessFrame(Frame: TDirectXFrame; An8File: TAn8File; An8Obj: TAn8Object; Matrix: T4x4Matrix);
  Var
    J,K,L   : Integer;
    Mesh    : TDirectXMesh;
    An8Mesh : TAn8Mesh;
    Face    : TDirectXFace;
    An8Face : TAn8Face;
    Mat     : TDirectXMaterial;
    An8Mat  : TAn8Material;
    An8Tex  : TAn8Texture;
    M1      : T4x4Matrix;
    Frame1  : TDirectXFrame;
    HasTC   : Boolean;

  Begin

    For J := 0 To Frame.Meshes.Count - 1 Do
    Begin
      Mesh    := TDirectXMesh(Frame.Meshes.Objects[J]);
      An8Mesh := TAn8Mesh.Create(An8File,Mesh.Name,An8Obj,Nil);
      An8Obj.Components.AddObject(An8Mesh.Name,An8Mesh);
      For K := 0 To Mesh.Points.Count - 1 Do
      Begin
        An8Mesh.Points.AddObject('',T3DPoint.Create(T3DPoint(Mesh.Points.Objects[K])));
        Matrix.Multiply(T3DPoint(An8Mesh.Points.Objects[K]));
      End; // For K
      For K := 0 To Mesh.Normals.Count - 1 Do An8Mesh.Normals.AddObject('',T3DPoint.Create(T3DPoint(Mesh.Normals.Objects[K])));
      For K := 0 To Mesh.TexCoords.Count - 1 Do
      Begin
        An8Mesh.TexCoords.AddObject('',TAn8TexCoord.Create(TAn8TexCoord(Mesh.TexCoords.Objects[K])));
        TAn8TexCoord(An8Mesh.TexCoords.Objects[K]).TZ := 1 - TAn8TexCoord(An8Mesh.TexCoords.Objects[K]).TZ;
      End; // For K
      For K := 0 To Mesh.Materials.Count - 1 Do
      Begin
        Mat    := TDirectXMaterial(Mesh.Materials.Objects[K]);
        An8Mat := TAn8Material.Create(An8Obj);
        An8Mat.Name := Mat.Name;
        An8Mat.An8File := An8File;
        An8Mat.Surface := TAn8Surface.Create(An8File);
        An8Tex := TAn8Texture.Create(An8File);
        An8Tex.Name := 'tex' + IntToStr(An8File.Textures.Count);
        An8Tex.FileName := Mat.TextureFileName;
        An8File.Textures.AddObject(An8Tex.Name,An8Tex);
        An8Obj.Materials.AddObject(An8Mat.Name,An8Mat);
        An8Mat.Surface.LockAmbientDiffuse := True;
        An8Mat.Surface.RGB := $00FFFFFF;
        An8Mat.Surface.Ambient := TAn8Color.Create(An8File,actAmbient);
        An8Mat.Surface.Ambient.RGB     := $00FFFFFF;
        An8Mat.Surface.Ambient.Factor  := 0.7;
        An8Mat.Surface.Diffuse := TAn8Color.Create(An8File,actDiffuse);
        An8Mat.Surface.Diffuse.Texture := An8Tex;
        An8Mat.Surface.Diffuse.RGB     := $00FFFFFF;
        An8Mat.Surface.Diffuse.Factor  := 1;
        An8Mat.Surface.Diffuse.TextureParams := TAn8TextureParams.Create;
        An8Mat.Surface.Diffuse.TextureParams.BlendMode := abmDecal;
        An8Mat.Surface.Diffuse.TextureParams.AlphaMode := aamNone;
        An8Mat.Surface.Specular := TAn8Color.Create(An8File,actSpecular);
        An8Mat.Surface.Specular.RGB     := $00FFFFFF;
        An8Mat.Surface.Specular.Factor  := 0.7;
        An8Mat.Surface.Emissive := TAn8Color.Create(An8File,actEmissive);
        An8Mat.Surface.Emissive.RGB     := $00000000;
        An8Mat.Surface.Emissive.Factor  := 0.7;
      End; // For K
      For K := 0 To Mesh.Faces.Count - 1 Do
      Begin
        Face := TDirectXFace(Mesh.Faces.Objects[K]);
        An8Face := TAn8Face.Create;
        An8Mesh.Faces.AddObject('',An8Face);
        HasTC := (Mesh.TexCoords.Count >= Mesh.Points.Count);
        SetLength(An8Face.Points,High(Face.Points) + 1);
        If HasTC Then SetLength(An8Face.TexCoords,High(Face.Points) + 1);
        For L := 0 To High(Face.Points) Do
        Begin
          An8Face.Points[L] := Face.Points[High(Face.Points) - L];
          If HasTC Then An8Face.TexCoords[L] := Face.Points[High(Face.Points) - L];
        End;
        If HasTC Then An8Face.Flags := 4;
        An8Face.Material := TAn8Material(An8Obj.Materials.Objects[An8Obj.Materials.Count - Mesh.Materials.Count + Mesh.Materials.IndexOfObject(Face.Material)]);
      End; // For K
      An8Mesh.Material := TAn8Material(An8Obj.Materials.Objects[An8Obj.Materials.Count - Mesh.Materials.Count]);
      For K := 0 To Mesh.Materials.Count - 1 Do
      Begin
        An8Mat := TAn8Material(An8Obj.Materials.Objects[An8Obj.Materials.Count - Mesh.Materials.Count + K]);
        An8Mesh.MaterialList.AddObject(An8Mat.Name,An8Mat);
      End; // For K
    End; // For J
    For J := 0 To Frame.Frames.Count - 1 Do
    Begin
      Frame1 := TDirectXFrame(Frame.Frames.Objects[J]);
      M1 := T4x4Matrix.Create(Frame1.TransformMatrix);
      M1.Multiply(Matrix);
      ProcessFrame(Frame1,An8File,An8Obj,M1);
      M1.Free;
    End; // For J
  End; // ProcessFrame

Begin
  An8File := TAn8File.Create;
  Matrix := T4x4Matrix.Create;
  Matrix.LoadIdentity;
  An8Obj := TAn8Object.Create(An8File);
  If Frames.Count > 0
   Then An8Obj.Name := TDirectXFrame(Frames.Objects[0]).Name
   Else An8Obj.Name := 'object_1';
  An8File.Objects.AddObject(An8Obj.Name,An8Obj);
  For I := 0 To Frames.Count - 1 Do
  Begin
    Frame  := TDirectXFrame(Frames.Objects[I]);
    ProcessFrame(Frame,An8File,An8Obj,Matrix);
  End; // For I
  Matrix.Free;
  Result := An8File;
End; // TDirectXFile.CreateAn8File

end.

