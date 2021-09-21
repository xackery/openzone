Unit ZoneClasses;

Interface

Uses Windows,Expressions,Classes,SharedComboBox,Graphics,Model_3DSMax,DesignEditors,
     DesignIntf,frmEditTextureListUnit,Controls,VCLEditors,Types,U3DPolys,Anim8or,
     Points3D,MyHashes;

Const
  Rescale                 = 34 / 10;
  RegionSize              = 128;
  MaxParseCount           = 500;
  TextureSize             = 256;
  EllipticalSections      = 24;
  BoundOffset             = 200; // Distance above/below vertical zone limits
  ZonePlaneThickness      = 16;
  GridSize                = 64;
  MinimumAnimTimePerFrame = 0.01;
  DefaultAnimTimePerFrame = 0.1;
  meshHeightMapGround     = 'HeightMapGround';
  meshHeightMapUnderwater = 'HeightMapUnderwater';
  meshBoundingBox         = 'BoundingBox';
  meshBoundingPolygons    = 'BoundingPolygons';
  mesh3DSMesh             = '3DSMesh';
  mesh3DSLight            = '3DSLight';
  TransparentTexture      = #240 + 'transparent';

Type
  TSQLRef = (srNone,srDoors,srObjects);
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = Array Of Integer;
  TTagListArray = TIntegerArray;
  TConfigurableItem = Class
    Constructor Create;                        Overload; Dynamic; Abstract;
    Constructor Create(CI: TConfigurableItem); Overload; Dynamic; Abstract;
    Procedure   Copy(CI: TConfigurableItem);   Dynamic; Abstract;
    Function    MakeCopy: TConfigurableItem;   Dynamic; Abstract;
  End;
  TZoneBound = Class(TConfigurableItem)
    X1,Y1,Z1,X2,Y2,Z2: Single;
    Constructor Create;                          Overload; Override;
    Constructor Create(CI: TConfigurableItem);   Overload; Override;
    Constructor Create(AX1,AY1,AZ1,AX2,AY2,AZ2: Single); Overload;
    Procedure   Copy(CI: TConfigurableItem);     Override;
    Function    MakeCopy: TConfigurableItem;     Override;
  End;
  TTextureInfo = Class
    TextureMaps    : TStringList;
    OpacityMaps    : TStringList;
    NormalMaps     : TStringList;
    AnimTime       : Single;
    FragmentShader : String;
    Constructor Create(TextureInfo: String);
    Destructor  Destroy; Override;
    Function    ToString: String;
    Function    FirstTexture: String;
    Function    ToStringNoParms: String;
    Procedure   ConvertTexturesToUpperCase;
  End;
//  TTextureInfoArray = Array Of TTextureInfo;
  TSoundRec = Packed Record
    L1     : Array[1..4] Of LongWord;
    X      : Single;
    Y      : Single;
    Z      : Single;
    Radius : Single;
    L2     : Array[1..4] Of LongWord;
    Sound1 : LongWord;
    Sound2 : LongWord;
    B1     : Byte;
    B2     : Byte;
    B3     : Byte;
    B4     : Byte;
    L3     : Array[1..2] Of LongWord;
    Dist1  : LongWord;
    Dist2  : LongWord;
    L4     : Array[1..2] Of LongWord;
  End;
  TSound = Class(TConfigurableItem)
    DayName   : String;
    NightName : String;
    Area      : Boolean;
    X         : Single;
    Y         : Single;
    Z         : Single;
    Radius    : Single;
    Constructor Create;                          Overload; Override;
    Constructor Create(CI: TConfigurableItem);   Overload; Override;
    Procedure   Copy(CI: TConfigurableItem);     Override;
    Function    MakeCopy: TConfigurableItem;     Override;
  End;
  TZoneType       = (ztIndoor,ztOutdoor);
//  T3DPoint        = Class;
  TElevationGrid = Class
    MinX    : Single;
    MinY    : Single;
    MinZ    : Single;
    MaxX    : Single;
    MaxY    : Single;
    MaxZ    : Single;
    NX      : Integer;
    NY      : Integer;
    Heights : Array Of Single;
    Visible : Array Of Boolean;
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   Clear;
    Function    GetHeight(X,Y: Integer): Single;
    Function    IsVisible(X,Y: Integer): Boolean;
    Function    IsVisibleAtAbsolute(X,Y: Single): Boolean;
    Procedure   SetHeight(X,Y: Integer; S: Single);
    Procedure   SetVisible(X,Y: Integer; B: Boolean);
    Function    GetFaceNENormal(X,Y: Integer): T3DPoint;
    Function    GetFaceSWNormal(X,Y: Integer): T3DPoint;
    Function    GetHeightAtAbsolute(X,Y: Single): Single;
    Function    CanGetHeightAtAbsolute(X,Y: Single): Boolean;
  End;
  TZonePlane = Class(TConfigurableItem)
    X1,Y1,Z1,X2,Y2,Z2 : Single;
    InfiniteZ         : Boolean;
    DestZoneID        : Integer;
    DestX             : Single;
    DestY             : Single;
    DestZ             : Single;
    DestAngle         : Integer;
    HasDestX          : Boolean;
    HasDestY          : Boolean;
    HasDestZ          : Boolean;
    HasDestAngle      : Boolean;
    Constructor Create;                        Overload; Override;
    Constructor Create(CI: TConfigurableItem); Overload; Override;
    Procedure   Copy(CI: TConfigurableItem);   Override;
    Function    MakeCopy: TConfigurableItem;   Override;
  End;
  TWaterType  = (wtWater,wtLava,wtPvP,wtIce,wtIceWater);
  TWaterShape = (wsRectangular,wsElliptical,wsIrregular);
  TWaterIrregularRec = Record
    X1,Y1,X2,Y2 : Single; // Endpoints of polygon
    NX,NY,NZ    : Single; // Splitting plane normal
    Dist        : Single; // Splitting plane Hessian distance
  End;
  TWater = Record
    Level       : Single;
    SemiTrans   : Boolean;
    WType       : TWaterType;
    Tinted      : Boolean;
    Color       : TColor;
    MinX        : Single;
    MinY        : Single;
    XSize       : Single;
    YSize       : Single;
    Tex         : Array Of String;
    HasDepth    : Boolean;
    Depth       : Single;
    Shape       : TWaterShape;
    Irregular   : Array Of TWaterIrregularRec;
    AnimTime    : Single;
  End;
  TAxis = (taX,taY,taZ);
  TTokenArray = Array Of String;
{
  T3DPoint = Class(TPersistent)
  Protected
    FX,FY,FZ   : Single;
  Public
    FBoneIndex : Integer;
    Constructor Create;                     Overload;
    Constructor Create(AX,AY,AZ: Single);   Overload;
    Constructor Create(P: T3DPoint);        Overload;
    Constructor Create(P1,P2: T3DPoint);    Overload;
    Procedure   Normalize;
    Procedure   Add(P: T3DPoint);           Overload;
    Procedure   Add(AX,AY,AZ: Single);      Overload;
    Procedure   Subtract(P: T3DPoint);      Overload;
    Procedure   Subtract(AX,AY,AZ: Single); Overload;
    Procedure   Multiply(S: Single);        Overload;
    Procedure   Multiply(AX,AY,AZ: Single); Overload;
    Procedure   Multiply(P: T3DPoint);      Overload;
    Procedure   Divide(S: Single);
    Function    Equals(P: T3DPoint): Boolean; Overload;
    Function    Equals(AX,AY,AZ: Single): Boolean; Overload;
    Procedure   Copy(P: T3DPoint);          Overload;
    Procedure   Copy(AX,AY,AZ: Single);     Overload;
    Function    Dot(P: T3DPoint): Single;
    Procedure   Cross(P: T3DPoint);
    Procedure   Rotate(P: T3DPoint);
    Procedure   Exchange(P: T3DPoint);
    Function    GetLength: Single;
    Procedure   GetNormalTo(P1,P2,P3: T3DPoint);
    Procedure   GetParallelTo(P: T3DPoint); Overload;
    Procedure   GetParallelTo(P1,P2,P3: T3DPoint); Overload;
    Procedure   GetPerpendicularTo(P: T3DPoint); Overload;
    Procedure   GetPerpendicularTo(P1,P2,P3: T3DPoint); Overload;
    Function    DistanceFrom(P: T3DPoint): Single;   Overload;
    Function    DistanceFrom(CX,CY,CZ: Single): Single; Overload;
    Function    DistanceFrom2(P: T3DPoint): Single;   Overload;
    Function    DistanceFrom2(CX,CY,CZ: Single): Single; Overload;
    Function    GetSinAngleBetween(P: T3DPoint): Single;
    Function    GetCosAngleBetween(P: T3DPoint): Single;
//    Function    GetHessianDistance(P1,P2,P3: T3DPoint): Single; Overload;
//    Function    GetHessianDistance(Normal,P: T3DPoint): Single; Overload;
    Function    GetID: String;
    Function    ToString: String;
  Published
    Property    X : Single Read FX Write FX;
    Property    Y : Single Read FY Write FY;
    Property    Z : Single Read FZ Write FZ;
  End;
}
  PEQEmuMapVertex = ^TEQEmuMapVertex;
  TEQEmuMapVertex = Packed Record
    X : Single;
    Y : Single;
    Z : Single;
  End;

  PEQEmuMapFace = ^TEQEmuMapFace;
  TEQEmuMapFace = Packed Record
    V  : Packed Array[0..2] Of TEQEmuMapVertex;
    NX : Single;
    NY : Single;
    NZ : Single;
    ND : Single;
  End;

  PEQEmuMapHeader = ^TEQEmuMapHeader;
  TEQEmuMapHeader = Packed Record
    Version        : LongWord;
    Face_Count     : LongWord;
    Node_Count     : Word;
    FaceList_Count : LongWord;
  End;

  PEQEmuMapNodesUnion = ^TEQEmuMapNodesUnion;
  TEQEmuMapNodesUnion = Packed Record
    // Nodes is used when the node is NOT a leaf node: Count and Offset are used when it is
    Case Integer Of
      0: (Nodes : Packed Array[0..3] Of Word); // index 0 means NULL, not root
      1: (Count  : LongWord;  // Number of faces
          Offset : LongWord); // Offset of face offsets in the face list
    End; // Case

  PEQEmuMapNode = ^TEQEmuMapNode;
  TEQEmuMapNode = Packed Record
    // bounding box of this node
    // there is no reason that these could not be unsigned
    // shorts other than we have to compare them to floats
    // all over the place, so they stay floats for now.
    // changing it could save 8 bytes per node record (~320k for huge maps)

    MinX  : Single;
    MinY  : Single;
    MaxX  : Single;
    MaxY  : Single;
    Flags : Byte;
    Union : TEQEmuMapNodesUnion;
  End;

  T4x4Matrix = Class
    M: Array[1..4,1..4] of Single;
    Constructor Create; Overload;
    Constructor Create(Matrix: T4x4Matrix); Overload;
    Constructor Create(M11,M12,M13,M14,M21,M22,M23,M24,M31,M32,M33,M34,M41,M42,M43,M44: Single); Overload;
    Procedure   Copy(Matrix: T4x4Matrix);
    Function    Determinant: Single;
    Procedure   Multiply(P: T3DPoint);
    Procedure   Invert;
    Procedure   Adjoint;
    Procedure   Divide(S: Single);
  End;

  T3DPointProperty = Class(TClassProperty,ICustomPropertyDrawing)
  Public
    Function  GetAttributes: TPropertyAttributes; Override;
    Procedure GetProperties(Proc: TGetPropProc);  Override;
    Function  GetValue: String;                   Override;
    // ICustomPropertyDrawing
    Procedure PropDrawName(ACanvas: TCanvas;  Const ARect: TRect; ASelected: Boolean);
    Procedure PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
  End;

  T3DPointElementProperty = Class(TNestedProperty)
  Protected
    FName    : String;
    FParent  : TPropertyEditor;
    FElement : Integer;
  Public
    Constructor Create(AParent: TPropertyEditor; AElement: Integer; AName: String); Reintroduce;
    Function    GetValue: String;                   Override;
    Function    GetName: String;                    Override;
    Procedure   SetValue(Const Value: String);      Override;
  End;

  THeading = Class(TPersistent)
  Protected
    FXAngle : Single;
    FYAngle : Single;
    FZAngle : Single;
  Public
    Constructor Create;                                  Overload;
    Constructor Create(H: THeading);                     Overload;
    Constructor Create(_XAngle,_YAngle,_ZAngle: Single); Overload;
    Destructor  Destroy;                                 Override;
    Procedure   Rotate(P: T3DPoint);                     Overload;
    Procedure   NegativeRotate(P: T3DPoint);             Overload;
    Procedure   Copy(H: THeading);                       Overload;
    Procedure   Copy(XA,YA,ZA: Single);                  Overload;
    Procedure   Standardize;
    Function    Equals(H: THeading): Boolean;
    Procedure   Multiply(H: THeading);
  Published
    Property XAngle : Single Read FXAngle Write FXAngle;
    Property YAngle : Single Read FYAngle Write FYAngle;
    Property ZAngle : Single Read FZAngle Write FZAngle;
  End;

  THeadingProperty = Class(TClassProperty,ICustomPropertyDrawing)
  Public
    Function  GetAttributes: TPropertyAttributes; Override;
    Procedure GetProperties(Proc: TGetPropProc);  Override;
    Function  GetValue: String;                   Override;
    // ICustomPropertyDrawing
    Procedure PropDrawName(ACanvas: TCanvas;  Const ARect: TRect; ASelected: Boolean);
    Procedure PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
  End;

  THeadingElementProperty = Class(TNestedProperty)
  Protected
    FName    : String;
    FParent  : TPropertyEditor;
    FElement : Integer;
  Public
    Constructor Create(AParent: TPropertyEditor; AElement: Integer; AName: String); Reintroduce;
    Function    GetValue: String;                   Override;
    Function    GetName: String;                    Override;
    Procedure   SetValue(Const Value: String);      Override;
  End;

  TSQLRefProperty = Class(TOrdinalProperty,ICustomPropertyDrawing,ICustomPropertyListDrawing)
  Protected
    FParent  : TPropertyEditor;
    FElement : Integer;
  Public
    Function    GetAttributes: TPropertyAttributes; Override;
    Function    GetValue: String;                   Override;
    Function    GetName: String;                    Override;
    Procedure   SetValue(Const Value: String);      Override;
    Procedure   GetValues(Proc: TGetStrProc);       Override;
    Function    AutoFill: Boolean;                  Override;
    // ICustomPropertyDrawing
    Procedure   PropDrawName(ACanvas: TCanvas;  Const ARect: TRect; ASelected: Boolean);
    Procedure   PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
    // ICustomPropertyListDrawing
    Procedure   ListMeasureWidth(Const Value: String; ACanvas: TCanvas; Var AWidth: Integer);
    Procedure   ListMeasureHeight(Const Value: String; ACanvas: TCanvas; Var AHeight: Integer);
    Procedure   ListDrawValue(Const Value: String; ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
  End;

  TTextureState = (tsSolid,tsSemiTransparent,tsTransparent);
  TMeshObject = Class;

  TPolygon = Class
  Protected
    FTexture     : String;
    Procedure   SetTexture(St: String);
  Public
    Vertices     : Array Of LongInt;                   // Array of indices
    TX           : Array Of Single;
    TZ           : Array Of Single;
    TextureInfo  : TTextureInfo;
    Colors       : Array Of TColor;
    HasTexCoords : Boolean; // True if the texture coordinates were already supplied
    HasColor     : Boolean; // True if a color was supplied
    HasAngle     : Boolean;
    TextureState : TTextureState;
    HasSolid     : Boolean;
    Solid        : Boolean;
    HasMasked    : Boolean;
    Masked       : Boolean;
    HasTag       : Boolean;
    NeedsMask    : Boolean;
    Tag          : LongWord; // Used by the ground editor
    Angle        : Integer;
    TextureRef   : TTexture;
    AnimTime     : Single;   // Time in seconds to complete an animation cycle (for animated textures)
    Constructor Create;                                         Overload;
    Constructor Create(P: TPolygon);                            Overload;
    Constructor Create(Const V: Array Of LongInt; Tex: String); Overload;
    Constructor CreateBasic(P: TPolygon);
    Destructor  Destroy;                                        Override;
    Procedure   Add(Index: Integer);
    Procedure   GetIntersection(Mesh: TMeshObject; V1,V2: T3DPoint; P1,P2: Integer; U1,U2: Single; I: T3DPoint; Out _TX,_TZ: Single; Var _Color: TColor);
    Procedure   InsertVertex(Mesh: TMeshObject; V: T3DPoint; Index: Integer; _TX,_TZ: Single; _Color: TColor);
    Function    EqualVertices(P: TPolygon; MO,MO1: TMeshObject): Boolean;
    Procedure   CalculateAngle(MO: TMeshObject);
    Procedure   SetTexCoordsFromAngle(MO: TMeshObject; MaxX,MaxY: Double; GX,GY: Integer);
    Function    GetNormal(Mesh: TMeshObject): T3DPoint;
    Function    GetCenter(Mesh: TMeshObject): T3DPoint;
    Function    GetCenterRelativeToMesh(Mesh: TMeshObject): T3DPoint;
    Procedure   GetBounds(Mesh: TMeshObject; Var MinX,MinY,MinZ,MaxX,MaxY,MaxZ: Single);
//    Procedure   CopyTextureFromPolygon(P: TPolygon);
    Procedure   ConvertTexturesToUpperCase;
//    Function    GetTexturesAsString: String;
    Procedure   SetSingleTexture(TextureMapName,OpacityMapName: String);
    Function    GetColorString: String;
    Function    IntersectsPolygon(P: TPolygon; MO1,MO2: TMeshObject): Boolean;
    Function    IntersectsSegment(V1,V2: T3DPoint; Mesh: TMeshObject): Boolean; Overload;
    Function    IntersectsSegment(V1,V2: T3DPoint; Mesh: TMeshObject; Intersection: T3DPoint): Boolean; Overload;
    Function    FacesPolygon(P: TPolygon; MO1,MO2: TMeshObject; EitherSide: Boolean): Boolean;
    Function    CenterFacesPolygon(P: TPolygon; MO1,MO2: TMeshObject; EitherSide: Boolean): Boolean;
    Function    IsVisibleTo(Position,Direction: T3DPoint; Mesh: TMeshObject; EitherSide: Boolean): Boolean;
    Function    CenterIsVisibleTo(Position,Direction: T3DPoint; Mesh: TMeshObject; EitherSide: Boolean): Boolean;
    Procedure   Invert;
    Property    Texture : String Read FTexture Write SetTexture;
  End;

  TZone        = Class;
  TGroupObject = Class;

  TZoneObject = Class(TPersistent)
  Protected
    FName    : String;
    FLoc     : T3DPoint;
    FHeading : THeading;                            // Normalized vector
    FSize    : T3DPoint;
    FParent  : TGroupObject;
    FZone    : TZone;
  Public
    Constructor Create;                                           Overload;
    Constructor Create(ZO: TZoneObject);                          Overload;
    Constructor Create(AName: String);                            Overload;
    Constructor Create(AName: String; AX,AY,AZ: Single);          Overload;
    Constructor Create(AName: String; AX,AY,AZ,SX,SY,SZ: Single); Overload;
    Destructor  Destroy; Override;
    Procedure   AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean); Dynamic; Abstract;
    Function    GetName: String;
    Procedure   SetName(St: String);
    Function    GetParent: TGroupObject;
    Procedure   SetParent(GO: TGroupObject);
    Function    GetZone: TZone;
    Function    GetHighestParent: TZoneObject;
    Function    GetIndent(Indent: Integer): String;
    Function    LoadFromFile(Var F: System.Text): Boolean; Dynamic;
    Procedure   SaveToFile(Var F: System.Text; Indent: Integer); Dynamic;
    Procedure   SetZone(Z: TZone); Dynamic;
    Function    MakeCopy: TZoneObject; Dynamic; Abstract;
    Procedure   ChangeToAbsolute(Parent: TGroupObject);
    Function    GetAbsoluteLocation: T3DPoint; Overload;
    Function    GetAbsoluteLocation(P: T3DPoint): T3DPoint; Overload;
    Procedure   MakeAbsolute(P: T3DPoint);
    Function    GetRelativeLocation(P: T3DPoint): T3DPoint;
    Function    NameExists(St: String; ToUpper: Boolean = True): Boolean; Dynamic;
    Function    FindObjectByName(St: String): TZoneObject; Dynamic;
    Procedure   GetSize(Out MinPos,MaxPos: T3DPoint; Var FirstTex: String); Dynamic;
    Procedure   TranslateAbsolute(P: T3DPoint);
    Function    IsSpecialObject: Boolean;
  Published
    Property    Loc    : T3DPoint Read FLoc;
    Property    Rotate : THeading Read FHeading;
    Property    Size   : T3DPoint Read FSize;
  End;

  TLightObject = Class(TZoneObject)
  Protected
    FRadius  : Single;
    FColor   : TColor;
    FFlicker : Single;  // Percent intensity variance
    Procedure   SetRadius(R: Single);
    Procedure   SetColor(C: TColor);
    Procedure   SetFlicker(F: Single);
  Public
    Constructor Create;                  Overload;
    Constructor Create(AName: String);   Overload;
    Constructor Create(L: TLightObject); Overload;
    Procedure   AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean); Override;
    Function    LoadFromFile(Var F: System.Text): Boolean; Override;
    Procedure   SaveToFile(Var F: System.Text; Indent: Integer); Override;
    Function    MakeCopy: TZoneObject; Override;
    Procedure   GetSize(Out MinPos,MaxPos: T3DPoint; Var FirstTex: String); Override;
  Published
    Property Radius  : Single Read FRadius  Write SetRadius;
    Property Color   : TColor Read FColor   Write SetColor;
    Property Flicker : Single Read FFlicker Write SetFlicker;
  End;

  TModelOrigin = Class(TZoneObject)
  Public
    Constructor Create;                  Overload;
    Constructor Create(AName: String);   Overload;
    Constructor Create(MO: TModelOrigin); Overload;
    Procedure   AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean); Override;
    Function    LoadFromFile(Var F: System.Text): Boolean; Override;
    Procedure   SaveToFile(Var F: System.Text; Indent: Integer); Override;
    Function    MakeCopy: TZoneObject; Override;
    Procedure   GetSize(Out MinPos,MaxPos: T3DPoint; Var FirstTex: String); Override;
  End;

  THotSpot = Class(TZoneObject)
  Public
    Constructor Create;                Overload;
    Constructor Create(AName: String); Overload;
    Constructor Create(HS: THotSpot);  Overload;
    Procedure   AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean); Override;
    Function    LoadFromFile(Var F: System.Text): Boolean; Override;
    Procedure   SaveToFile(Var F: System.Text; Indent: Integer); Override;
    Function    MakeCopy: TZoneObject; Override;
    Procedure   GetSize(Out MinPos,MaxPos: T3DPoint; Var FirstTex: String); Override;
  End;

  TGroupObject = Class(TZoneObject)
  Public
    Objects  : TStringList;
    Category : String;          // Used only for mesh library objects
    Constructor Create;                   Overload;
    Constructor Create(GO: TGroupObject); Overload;
    Constructor Create(AName: String);    Overload;
    Destructor  Destroy; Override;
    Procedure   Group(ZO: TZoneObject);
    Procedure   UnGroupAll;
    Procedure   UnGroup(ZO: TZoneObject);
    Procedure   AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean); Override;
    Function    LoadFromFile(Var F: System.Text): Boolean; Override;
    Procedure   SaveToFile(Var F: System.Text; Indent: Integer); Override;
    Procedure   SetZone(Z: TZone); Override;
    Function    MakeCopy: TZoneObject; Override;
    Function    NameExists(St: String; ToUpper: Boolean = True): Boolean; Override;
    Function    FindObjectByName(St: String): TZoneObject; Override;
    Function    FindModelOrigin: TModelOrigin;
    Function    ContainsObject(ZO: TZoneObject): Boolean;
    Function    GetHotSpots: TStringList; // List of THotSpot
  End;

  TAnimatedGroupObject = Class(TGroupObject)
  Protected
    FSubdivisions : T3DPoint;
    FAmplitude    : T3DPoint;
    FFrequency    : T3DPoint;
    FPhase        : T3DPoint;
    FAttenuation  : T3DPoint;
    FFrames       : Integer;
    Procedure   SetSubdivisions(P: T3DPoint);
    Procedure   SetAmplitude(P: T3DPoint);
    Procedure   SetFrequency(P: T3DPoint);
    Procedure   SetPhase(P: T3DPoint);
    Procedure   SetAttenuation(P: T3DPoint);
  Public
    Constructor Create;                                          Overload;
    Constructor Create(GO: TAnimatedGroupObject);                Overload;
    Constructor Create(AName: String);                           Overload;
    Destructor  Destroy;                                         Override;
    Function    LoadFromFile(Var F: System.Text): Boolean;       Override;
    Procedure   SaveToFile(Var F: System.Text; Indent: Integer); Override;
    Function    MakeCopy: TZoneObject;                           Override;
  Published
    Property    Subdivisions : T3DPoint Read FSubdivisions Write SetSubdivisions;
    Property    Amplitude    : T3DPoint Read FAmplitude    Write SetAmplitude;
    Property    Frequency    : T3DPoint Read FFrequency    Write SetFrequency;
    Property    Phase        : T3DPoint Read FPhase        Write SetPhase;
    Property    Attenuation  : T3DPoint Read FAttenuation  Write SetAttenuation;
    Property    Frames       : Integer  Read FFrames       Write FFrames;
  End;

//  TTree = Class;
  TLightTreeHash = Class;

  TMeshObject = Class(TZoneObject)
  Public
    Vertices    : TStringList;                      // List of T3DPoint
    Polygons    : TStringList;                      // List of TPolygon
    Normals     : TStringList;                      // List of T3DPoint
    BoneIndices : Array Of Integer;
    Constructor Create;                                  Overload;
    Constructor Create(MO: TMeshObject);                 Overload;
    Constructor Create(AName: String);                   Overload;
    Constructor Create(AName: String; AX,AY,AZ: Single); Overload;
    Constructor Create(AName: String; AX,AY,AZ,SX,SY,SZ: Single); Overload;
    Destructor  Destroy;                                 Override;
    Procedure   AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean); Override;
    Function    IndexOfVertex(P: T3DPoint): LongInt;
    Procedure   ConvertToTriangles;
    Procedure   Coalesce;
//    Procedure   SplitAlong(Axis: TAxis; DivPt: Single);
//    Procedure   BreakIntoRegions;
    Procedure   RemoveAlongPlane(Normal: T3DPoint; Dist: Single; RemoveOutside: Boolean);
    Procedure   RemoveUnusedVertices;
    Procedure   GetCenterOfVolumeAndMaxDistance(Center: T3DPoint; Var Dist: Single);
    Procedure   GetBounds(MinPt,MaxPt: T3DPoint);
    Function    LoadFromFile(Var F: System.Text): Boolean; Override;
    Procedure   SaveToFile(Var F: System.Text; Indent: Integer); Override;
    Function    MakeCopy: TZoneObject; Override;
    Procedure   CalcNormals;
    Procedure   MatchNormalCountToVertexCount;
    Procedure   BlendNormals;
    Procedure   CalcTextureCoords(P: TPolygon); Overload;
    Procedure   CalcTextureCoords(P: TPolygon; MinPt,MaxPt: T3DPoint); Overload;
    Procedure   ClearNormals;
    Procedure   AddCopyOfPolygon(P: TPolygon; M: TMeshObject);
    Procedure   ShadePolygonsByLights(LightTreeHash: TLightTreeHash; PolygonList: PIntegerArray; Center: T3DPoint; DistToCenter: Single);
  End;

  TMeshLibraryObjectReference = Class(TZoneObject)
  Protected
    FGroup      : TGroupObject;
    FGravity    : Boolean;
    FInsertMesh : Boolean;
    FSQLRef     : TSQLRef;
    FSQLParms   : TStringList;
    Procedure   SetGroup(GO: TGroupObject);
    Procedure   SetGravity(B: Boolean);
    Procedure   SetInsertMesh(B: Boolean);
    Procedure   SetSQLRef(Ref: TSQLRef);
    Function    GetSQLParmCount: Integer;
    Function    GetSQLParm(Index: Integer): String;
  Public
    Constructor Create; Overload;
    Constructor Create(GO: TGroupObject); Overload;
    Constructor Create(ML: TMeshLibraryObjectReference); Overload;
    Destructor  Destroy; Override;
    Procedure   AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean); Override;
    Function    LoadFromFile(Var F: System.Text): Boolean; Override;
    Procedure   SaveToFile(Var F: System.Text; Indent: Integer); Override;
    Function    MakeCopy: TZoneObject; Override;
    Property    Group                   : TGroupObject Read FGroup      Write SetGroup;
    Property    SQLParmCount            : Integer      Read GetSQLParmCount;
    Property    SQLParm[Index: Integer] : String       Read GetSQLParm;
  Published
    Property    Gravity    : Boolean      Read FGravity    Write SetGravity;
    Property    InsertMesh : Boolean      Read FInsertMesh Write SetInsertMesh;
    Property    SQLRef     : TSQLRef      Read FSQLRef     Write SetSQLRef;
  End;

  TCreatureLibraryObjectReference = Class(TZoneObject)
  Protected
    FAn8File    : TAn8File;
    FGravity    : Boolean;
    FMinPt      : T3DPoint;
    FMaxPt      : T3DPoint;
    FSizeSet    : Boolean;
    Procedure   SetAn8File(An8: TAn8File);
    Procedure   SetGravity(B: Boolean);
  Public
    Constructor Create; Overload;
    Constructor Create(An8: TAn8File); Overload;
    Constructor Create(CL: TCreatureLibraryObjectReference); Overload;
    Destructor  Destroy; Override;
    Procedure   AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean); Override;
    Function    LoadFromFile(Var F: System.Text): Boolean; Override;
    Procedure   SaveToFile(Var F: System.Text; Indent: Integer); Override;
    Function    MakeCopy: TZoneObject; Override;
    Procedure   LoadCreature;
    Procedure   GetSize(Out MinPos,MaxPos: T3DPoint; Var FirstTex: String); Override;
    Procedure   SetSize(AMinPt,AMaxPt: T3DPoint);
    Procedure   RespondToGravity;
    Property    An8File    : TAn8File     Read FAn8File    Write SetAn8File;
  Published
    Property    Gravity    : Boolean      Read FGravity    Write SetGravity;
  End;

  TScriptParamType = (sptInteger,sptFloat,sptBoolean,sptString,sptScript);
  TScriptGlobal    = (sglPlaceX,sglPlaceY,sglPlaceZ,sglRotateX,sglRotateY,sglRotateZ,
                      sglSizeX,sglSizeY,sglSizeZ);
  TScriptPredicate = (spdTriangle,spdTriangleTex,spdRectangle,spdRectangleTex,
                      spdIf,spdElse,spdElseIf,spdEndIf,spdWhile,spdWend,spdRepeat,spdUntil,
                      spdScript,spdVariableScript,spdAssignment,spdSQLParm);
  TScriptDefault = Record
   Case Integer Of
     0: (Int   : LongInt);
     1: (Float : Single);
     2: (Bool  : Boolean);
     3: (St    : ShortString);
     4: (Glob  : TScriptGlobal);
   End; // Case

  TScriptParam = Class
    Name     : String;
    _Type    : TScriptParamType;
    Default  : TScriptDefault;
  End;

  TScriptStatement = Class
    Predicate : TScriptPredicate;
  End;

  TScriptStatementTriangle = Class(TScriptStatement)
    X1        : TExpression;
    Y1        : TExpression;
    Z1        : TExpression;
    X2        : TExpression;
    Y2        : TExpression;
    Z2        : TExpression;
    X3        : TExpression;
    Y3        : TExpression;
    Z3        : TExpression;
    TX1       : TExpression;
    TZ1       : TExpression;
    TX2       : TExpression;
    TZ2       : TExpression;
    TX3       : TExpression;
    TZ3       : TExpression;
    NX1       : TExpression;
    NY1       : TExpression;
    NZ1       : TExpression;
    NX2       : TExpression;
    NY2       : TExpression;
    NZ2       : TExpression;
    NX3       : TExpression;
    NY3       : TExpression;
    NZ3       : TExpression;
    Tex       : TExpression;
    Trans     : TExpression;
    STrans    : TExpression;
    Solid     : TExpression;
    Color     : TExpression;
    HasColor  : TExpression;
    Masked    : TExpression;
    HasNormal : TExpression;
    Constructor Create;
    Destructor  Destroy; Override;
  End;

  TScriptStatementRectangle = Class(TScriptStatement)
    X1        : TExpression;
    Y1        : TExpression;
    Z1        : TExpression;
    X2        : TExpression;
    Y2        : TExpression;
    Z2        : TExpression;
    X3        : TExpression;
    Y3        : TExpression;
    Z3        : TExpression;
    X4        : TExpression;
    Y4        : TExpression;
    Z4        : TExpression;
    TX1       : TExpression;
    TZ1       : TExpression;
    TX2       : TExpression;
    TZ2       : TExpression;
    TX3       : TExpression;
    TZ3       : TExpression;
    TX4       : TExpression;
    TZ4       : TExpression;
    NX1       : TExpression;
    NY1       : TExpression;
    NZ1       : TExpression;
    NX2       : TExpression;
    NY2       : TExpression;
    NZ2       : TExpression;
    NX3       : TExpression;
    NY3       : TExpression;
    NZ3       : TExpression;
    NX4       : TExpression;
    NY4       : TExpression;
    NZ4       : TExpression;
    Tex       : TExpression;
    Trans     : TExpression;
    STrans    : TExpression;
    Solid     : TExpression;
    Color     : TExpression;
    HasColor  : TExpression;
    Masked    : TExpression;
    HasNormal : TExpression;
    Constructor Create;
    Destructor  Destroy; Override;
  End;

  TScriptStatementSQLParm = Class(TScriptStatement)
    Table  : TExpression;
    Column : TExpression;
    Value  : TExpression;
    Constructor Create;
    Destructor  Destroy; Override;
  End;

  TScriptStatementSingleExpression = Class(TScriptStatement)
    E : TExpression;
    Constructor Create;
    Destructor  Destroy; Override;
  End;

  TScriptStatementScriptCall = Class(TScriptStatement)
    ScriptName  : String;
    ScriptParms : String;
  End;

  TScriptStatementVariableScriptCall = Class(TScriptStatementScriptCall)
  End;

  TScriptStatementAssignment = Class(TScriptStatement)
    Source   : TExpression;
    Dest     : TExpressionOp; // Dest does NOT get deallocated; it will already reside in Variables
    Constructor Create;
    Destructor  Destroy; Override;
  End;

  TScriptedObject = Class;

  TScripted = Class
    Name       : String;
    Category   : String;
    Script     : TStringList;
    VDefaults  : TStringList; // List of TScriptStatement; default assignments to variables
    Compiled   : TStringList; // List of TScriptStatement; main script body
    Variables  : TStringList; // List of TExpressionOp
    ParamNames : TStringList; // Parameter names, in uppercase
    Constructor Create;
    Destructor  Destroy; Override;
    Function    MakeMeshObject(Owner: TScriptedObject; Parameters: TStringList): TMeshObject;
    Procedure   Compile;
    Function    GetParamType(Param: String): TScriptParamType;
  End;

  TScriptedObjectParameters = Class(TPersistent)
  Protected
    FParent     : TScriptedObject;
    FParameters : TStringList;
    FParmValues : TStringList; // List of what the parms really hold, taking defaults into account
  Public
    Constructor Create(Parent: TScriptedObject);
    Destructor  Destroy; Override;
  Published
  End;

  TScriptedObjectParametersProperty = Class(TClassProperty,ICustomPropertyDrawing)
  Public
    Function  GetAttributes: TPropertyAttributes; Override;
    Procedure GetProperties(Proc: TGetPropProc);  Override;
    Function  GetName: String;                    Override;
    Function  GetValue: String;                   Override;
    // ICustomPropertyDrawing
    Procedure PropDrawName(ACanvas: TCanvas;  Const ARect: TRect; ASelected: Boolean);
    Procedure PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
  End;

  TScriptParameterProperty = Class(TNestedProperty,ICustomPropertyDrawing)
  Protected
    FParent  : TPropertyEditor;
    FElement : Integer;
    FDefault : Boolean;
    FValue   : String;
  Public
    Constructor Create(AParent: TPropertyEditor; AElement: Integer); Reintroduce;
    Function    GetAttributes: TPropertyAttributes; Override;
    Function    GetValue: String;                   Override;
    Function    GetName: String;                    Override;
    Procedure   SetValue(Const Value: String);      Override;
    // ICustomPropertyDrawing
    Procedure   PropDrawName(ACanvas: TCanvas;  Const ARect: TRect; ASelected: Boolean);
    Procedure   PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
  End;

  TScriptBooleanParameterProperty = Class(TOrdinalProperty,ICustomPropertyDrawing,ICustomPropertyListDrawing)
  Protected
    FParent  : TPropertyEditor;
    FElement : Integer;
    FDefault : Boolean;
  Public
    Function    GetAttributes: TPropertyAttributes; Override;
    Constructor Create(AParent: TPropertyEditor; AElement: Integer); Reintroduce;
    Function    GetValue: String;                   Override;
    Function    GetName: String;                    Override;
    Procedure   SetValue(Const Value: String);      Override;
    Procedure   GetValues(Proc: TGetStrProc);       Override;
    Function    AutoFill: Boolean;                  Override;
    // ICustomPropertyDrawing
    Procedure   PropDrawName(ACanvas: TCanvas;  Const ARect: TRect; ASelected: Boolean);
    Procedure   PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
    // ICustomPropertyListDrawing
    Procedure   ListMeasureWidth(Const Value: String; ACanvas: TCanvas; Var AWidth: Integer);
    Procedure   ListMeasureHeight(Const Value: String; ACanvas: TCanvas; Var AHeight: Integer);
    Procedure   ListDrawValue(Const Value: String; ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
  End;

  TScriptTextureParameterProperty = Class(TOrdinalProperty,ICustomPropertyDrawing,ICustomPropertyListDrawing)
  Protected
    FParent  : TPropertyEditor;
    FElement : Integer;
    FDefault : Boolean;
  Public
    Function    GetAttributes: TPropertyAttributes; Override;
    Constructor Create(AParent: TPropertyEditor; AElement: Integer); Reintroduce;
    Function    GetValue: String;                   Override;
    Function    GetName: String;                    Override;
    Procedure   SetValue(Const Value: String);      Override;
    Procedure   GetValues(Proc: TGetStrProc);       Override;
    Function    AutoFill: Boolean;                  Override;
    Procedure   Edit;                               Override;
    // ICustomPropertyDrawing
    Procedure   PropDrawName(ACanvas: TCanvas;  Const ARect: TRect; ASelected: Boolean);
    Procedure   PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
    // ICustomPropertyListDrawing
    Procedure   ListMeasureWidth(Const Value: String; ACanvas: TCanvas; Var AWidth: Integer);
    Procedure   ListMeasureHeight(Const Value: String; ACanvas: TCanvas; Var AHeight: Integer);
    Procedure   ListDrawValue(Const Value: String; ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
  End;

  TScriptedObject = Class(TZoneObject)
  Protected
    FParameters : TScriptedObjectParameters;
    FScriptName : String;
  Public
    Constructor Create;                                  Overload;
    Constructor Create(SO: TScriptedObject);             Overload;
    Constructor Create(AName: String);                   Overload;
    Constructor Create(AName: String; AX,AY,AZ: Single); Overload;
    Destructor  Destroy; Override;
    Procedure   AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean); Override;
    Function    GetParameterNamesAndValues(Var Values,RealValues: String): String;
    Function    GetScript: TScripted;
    Procedure   ClearParameters;
    Procedure   AddParameter(St: String);
    Procedure   SetScriptName(St: String);
    Function    LoadFromFile(Var F: System.Text): Boolean; Override;
    Procedure   SaveToFile(Var F: System.Text; Indent: Integer); Override;
    Function    MakeCopy: TZoneObject; Override;
  Published
    Property    Parameters: TScriptedObjectParameters Read FParameters Write FParameters;
  End;

  TRegionAttribute = (raNone,raWater,raLava,raPVP,raZoneLine,raIce,raIceWater);
  TRegionAttributes = Set Of TRegionAttribute;
  TRegionAttributeRec = Packed Record
    Attr  : TRegionAttribute;
    Value : Integer;
  End;

  TRegion = Class
    Mesh         : TMeshObject;
    Flag         : Boolean;
    Tag          : Integer;
    Attribute    : TRegionAttributeRec;
    Left         : TRegion;
    Right        : TRegion;
    Parent       : TRegion;
    Polygons     : TIntegerArray;
    SplitNorm    : T3DPoint;
    SplitDist    : Single;
    BoundMesh    : TMeshObject;
    MinPt        : T3DPoint;
    MaxPt        : T3DPoint;
    Sphere       : TSphere;    // Sphere that surrounds the region
    Constructor Create(AMesh: TMeshObject; CreateBoundMesh: Boolean); Overload;
    Constructor Create(Region: TRegion);    Overload;
    Destructor  Destroy; Override;
    Procedure   SetFlags;
    Procedure   ClearFlags;
    Procedure   SetFlaggedToAttribute(Attr: TRegionAttribute; Value: Integer);
    Procedure   SetUnflaggedToAttribute(Attr: TRegionAttribute; Value: Integer);
    Procedure   SetFlaggedToTexture(Texture: String);
    Procedure   SetFlaggedFromTextureToTexture(OldTexture,NewTexture: String);
    Procedure   SetUnflaggedToTexture(Texture: String);
    Procedure   SetUnflaggedFromTextureToTexture(OldTexture,NewTexture: String);
    Procedure   SplitAlongPlane(Normal: T3DPoint; Dist: Single; FlaggedOnly,ForceSplit: Boolean);
    Procedure   ConvertToTriangles;
    Function    CountNodes: Integer;
    Function    CountLeafNodes: Integer;
    Function    CountLeafNodesWithAttrs(Attrs: TRegionAttributes): Integer;
    Function    GetMaxDistanceFromPoint(V: T3DPoint): Single;
    Procedure   GetCenterOfVolumeAndMaxDistance(Center: T3DPoint; Var Dist: Single);
    Procedure   TagLeafNodes(Var TagNumber: Integer);
    Procedure   GetTagList(Var TagList: TTagListArray);
    Procedure   GetRegionsInVolume(List: TStringList; VMinPt,VMaxPt: T3DPoint; LeavesOnly: Boolean);
    Procedure   GetTagsOfRegionsInVolume(Var TagList: TTagListArray; Var TagCount: Integer; VMinPt,VMaxPt: T3DPoint);
    Procedure   CalcBounds; 
    Procedure   SetFlaggedToColor(C: TColor);  // For testing
    Function    GetMaxPolyCount(Var Count: Integer): Integer;
    Procedure   Split(MaxPolysPerGrid,MaxVertsPerGrid: Integer);
    Procedure   GetAbsoluteBounds(MinPt,MaxPt: T3DPoint);
    Procedure   AddToMesh(MO: TMeshObject; FlagValue,CheckFlag: Boolean);
    Function    GetOctreeChild(XLeft,YLeft,ZLeft: Boolean): TRegion;
    Procedure   GetNearRegions(NearList: TStringList; Loc: T3DPoint; Dist: Single);
    Procedure   ShadePolygonsByLights(LightTreeHash: TLightTreeHash);
  End;

  TTree = Class
    Root : TRegion;
    Constructor Create(Mesh: TMeshObject; CreateBoundMesh: Boolean);
    Destructor  Destroy; Override;
    Procedure   SplitAlongGrid(MaxPolysPerGrid,MaxVertsPerGrid: Integer);
    Procedure   SplitAlongAxis(Region: TRegion; Axis: TAxis; DivPt: Single; FlaggedOnly,NormalHigh: Boolean);
    Function    SplitOffMesh(Attrs: TRegionAttributes): TMeshObject;
    Procedure   SplitAlongWater(Var Water: TWater);
    Procedure   SplitAlongZonePlane(Var ZonePlane: TZonePlane);
    Function    GetMaxHeightAt(X,Y: Single; HighestPoint,PV1,PV2,PV3: T3DPoint; MinZ,MaxZ: Single): TPolygon;
    Procedure   MakeOctree;
  End;

  TLightTreeHash = Class
    Lights      : TStringList; // List of TLightObject
    RegionLists : TIntegerObjectHash;
    Constructor Create(Zone: TZone; Tree: TTree);
    Destructor  Destroy; Override;
  End;

  TZone = Class(TStringList)
  Protected
    DefaultLandTexture       : String;
    DefaultUnderwaterTexture : String;
  Public
    ElevationGrid            : TElevationGrid;
    Bounds                   : TStringList;
    BoundTex                 : String;
    BoundMinZ,BoundMaxZ      : Single;
    ZonePlanes               : TStringList;
    LongName                 : String;
    ShortName                : String;
    ZoneType                 : TZoneType;
    Sounds                   : TStringList;
    Water                    : Array Of TWater;
    ExtraMeshes              : TStringList;
    Creatures                : TStringList;
    Import3DSCache           : TStringList;
    Constructor Create;
    Destructor  Destroy; Override;
    Function    BuildPolygonList(ExcludeMeshReferences,ExcludeGround,ExcludeInserts,SQLOnly: Boolean): TMeshObject;
    Function    BuildPolygonLists: TStringList; Overload;                  // List of TMeshObject
    Function    BuildPolygonLists(ZO: TZoneObject): TStringList; Overload; // List of TMeshObject
    Procedure   BuildBoundingPolygons;
    Procedure   LoadFromFile(FileName: String); Reintroduce;
    Procedure   SaveToFile(FileName: String; SelectedObjects: TStringList);
    Function    CoalescePolygonList(List: TStringList): TMeshObject;
    Procedure   ImportFrom3DSFile(FileName: String);
    Function    ImportObjectFrom3DSFile(FileName: String): TZoneObject; 
    Procedure   ImportGroundFrom3DSFile(FileName: String);
    Function    AddObject(Const S: String; AObject: TObject): Integer; Override;
    Procedure   InsertObject(Index: Integer; Const S: String; AObject: TObject); Override;
    Function    NameExists(St: String): Boolean;
    Function    FindObjectByName(St: String): TZoneObject;
    Function    GetObjectIndex(ZO: TZoneObject): Integer;
    Function    GetObjectMeshCount: Integer;
    Function    GetZoneObject(I: Integer): TZoneObject;
    Procedure   BuildElevationGrid;
    Procedure   BuildBounds;
    Function    FindMeshObject(MeshName: String): TMeshObject;
    Procedure   FindDefaultLandTexture;
    Procedure   FindDefaultUnderwaterTexture;
    Function    GetDefaultLandTexture: String;
    Function    GetDefaultUnderwaterTexture: String;
    Procedure   SetDefaultLandTexture(St: String);
    Procedure   SetDefaultUnderwaterTexture(St: String);
    Procedure   AddBound(X1,Y1,Z1,X2,Y2,Z2: Single);
    Procedure   BreakUpBounds;
    Function    GetAllLightSources: TStringList;
    Procedure   FixElevationGridBounds;
    Procedure   ShiftZone(X,Y,Z: Single);
    Procedure   ExportToXWFFile(Stream: TStream; TexList,CharTexList: TStringList; ExportAsOctree: Boolean; CustomScale: Single);
    Procedure   ImportFromXWFFile(Stream: TStream);
    Function    ImportObjectFromAN8File(FileName: String; Rotate,Center: Boolean): TZoneObject;
    Function    ImportObjectFromDirectXFile(FileName: String; Rotate,Center: Boolean): TZoneObject;
    Function    ImportAn8Objects(BaseName: String; An8File: TAn8File; List: TStringList; Rotate,Center: Boolean): TZoneObject;
    Procedure   ImportFromAN8File(FileName: String; Rotate: Boolean);
    Procedure   ImportFromDirectXFile(FileName: String; Rotate: Boolean);
    Function    ExportTo3DSFile(FileName: String; TexList: TStringList): Integer;
    Function    ExportToAn8File(FileName: String; TexList: TStringList): Integer;
    Procedure   AddWaterToMesh(Mesh: TMeshObject);
    Procedure   ImportCreature(Index: Integer);
    Procedure   AddAn8BodyHeadPair(An8: TAn8File; NewMeshes: TStringList; Obj: TAn8Object; FigureIndex,BodyIndex,HeadIndex: Integer; SeparateHeads,IncludeEquipment: Boolean);
    Procedure   BuildAn8ObjectList(An8: TAn8File; ObjectList: TStringList; Out SeparateHeads: Boolean);
    Procedure   BuildAn8HeadBodyMeshes(An8: TAn8File; ObjectList,NewMeshes: TStringList; FigureIndex: Integer; SeparateHeads,IncludeEquipment: Boolean;
                                       Out BaseMeshCount: Integer; Out ModelHeight,ModelMaxLen: Single; Out HeadCount,BodyCount: Integer);
    Procedure   BuildEquipmentMeshes(An8: TAn8File; NewMeshes: TStringList);                                   
    Procedure   SortAn8VerticesByBone(An8: TAn8File; FigureIndex: Integer; Mesh: TAn8Mesh; PreloadedList: Boolean);
    Procedure   SortAn8FacesByTexture(Mesh: TAn8Mesh; Textures: TStringList);
  End;

Const
  warnNone  = 0;
  warnLFN   = 1;
  warnNoTex = 2;
  ScriptGlobalNames : Array[TScriptGlobal] Of String =
   ('PLACEX','PLACEY','PLACEZ','ROTATEX','ROTATEY','ROTATEZ','SIZEX','SIZEY','SIZEZ');
  ScriptGlobalTypes : Array[TScriptGlobal] Of TScriptParamType =
   (sptFloat,sptFloat,sptFloat,sptFloat,sptFloat,sptFloat,sptFloat,sptFloat,sptFloat);
  ReservedWords : Array[0..33] Of String =
   ('PARAM','DEFAULT','INTEGER','FLOAT','BOOLEAN',
    'TRUNC','ROUND','ROUNDUP','FRAC','ABS','SGN','SQRT','SIN',
    'COS','TAN','CEIL','FLOOR','EXP','INT','LN','LOG10','LOG2',
    'SQR','PI','MOD','AND','OR','XOR','NOT','SIN','COS','TAN',
    'SHL','SHR');

Var
  CompileLog       : TStringList;
  ParseLog         : TStringList;
  ScriptLibrary    : TStringList;
  MeshLibrary      : TStringList;
  CreatureLibrary  : TStringList;
  TextureLibrary   : TSharedComboBoxStrings;
  TextureSet       : String;
  TextureLibraries : TStringList;
  PlaceX           : Single;
  PlaceY           : Single;
  PlaceZ           : Single;
  RotateX          : Single;
  RotateY          : Single;
  RotateZ          : Single;
  SizeX            : Single;
  SizeY            : Single;
  SizeZ            : Single;
  IntersectX       : Array Of Single;
  IntersectY       : Array Of Single;
  IntersectDist    : Array Of Single;
  VertIndex        : Array Of Integer;
  TexIndex         : Array Of Integer;
  SortAn8Mesh      : TAn8Mesh;
  SortTexList      : TStringList;
  VertTexList      : TStringList;

Function TokenizeAn8SequenceName(St: String; Out SequenceName,Prefix: String; Out DropLastFrame: Boolean; Out MillisecondsPerFrame: Integer): Boolean;
Function TokenizeAn8ObjectName(St: String; Out BodyMesh,BodyTex,HeadMesh,HeadTex: Integer): Boolean;
Function  GetFirstTexture(Texture: String): String;
//Procedure BreakupTextureString(Texture: String; Out Textures,Opacities,Parameters: String);
Procedure FreeListAndItsObjects(L: TStringList);
Function  GetToken(Delim: String; Var St: String; PreserveDoubleQuotes: Boolean = False): String;
Procedure GetTokens(Delim: String; Var St: String; Var TokenArray: TTokenArray; PreserveDoubleQuotes: Boolean = False);
Function  GetIntValue(Token: String; Var V: Integer): Boolean;
Function  GetIntValues(Token: String; Var V: TIntegerArray): Boolean;
Function  GetSingleValue(Token: String; Var V: Single): Boolean;
Function  GetDoubleValue(Token: String; Var V: Double): Boolean;
Function  GetBooleanValue(Token: String; Var V: Boolean): Boolean;
Procedure SetTextureSet(St: String);
Function  CompressTextureList(St: String): String;
//Function  LineIntersection(X1,Y1,X2,Y2,X3,Y3,X4,Y4: Single; Var X,Y: Single): Integer;
//Function  DistanceFromLine(X1,Y1,X2,Y2,X,Y: Single): Single;
//Function  DistanceFromLineSegment(X1,Y1,X2,Y2,X,Y: Single): Single;
Function  MeshLibraryObjectIndex(St: String): Integer;
//Function  LineFacet(P1,P2,PA,PB,PC,Intersect: T3DPoint): Boolean;
//Procedure gts_point_segment_closest(P,P1,P2,Closest: T3DPoint);
//Function  gts_segment_triangle_intersection(S1,S2,T1,T2,T3: T3DPoint; Boundary: Boolean): T3DPoint;
//Function  GetHessianDistance(P1,P2,P3: T3DPoint): Single; Overload;
//Function  GetHessianDistance(Normal,P: T3DPoint): Single; Overload;
Function  ExtractFileNameNoExt(St: String): String;
Function  GetVisualSQLRefValue(Ref: TSQLRef): String;
Function  GetSQLRefValueFromVisual(St: String): TSQLRef;
Procedure ConvertEuroNumbersToUS(Var St: String);

Implementation

Uses Forms,SysUtils,frmMainUnit,Math,frmStatusUnit,Sorter,XWFFiles,WinSock,OZDMUnit,Dialogs;

Type
  TTextureInfoDelimiter = (tidTextures,tidOpacities,tidNormals,tidFragmentShader,tidParameters);
  TMeshType             = (mtHead,mtBody,
                           mtCloak1,mtCloak2,mtCloak3,mtCloak4,mtCloak5,mtCloak6,mtCloak7,mtCloak8,mtCloak9,mtCloak10,
                           mtBeltPouch,mtBeltBag1,mtBeltBag2,mtEquipment);

Const
  MeshTypeID : Array[TMeshType] Of String = ('head','body',
                                             'cloak_1','cloak_2','cloak_3','cloak_4','cloak_5','cloak_6','cloak_7','cloak_8','cloak_9','cloak_10',
                                             'belt_pouch','belt_bag_1','belt_bag_2','equipment');
  TextureInfoDelimiters : Array[TTextureInfoDelimiter] Of String = ('','|','?','*','+');

// ------------------------------
// Procedures and functions
// ------------------------------

Procedure ConvertEuroNumbersToUS(Var St: String);
Var
  I                       : Integer;
  CommaSeparators         : Integer;
  CommasBetweenNumbers    : Integer;
  PeriodsBetweenNumbers   : Integer;
  CanConvert              : Boolean;
  LastCommaSeparator      : Integer;
  LastCommaBetweenNumbers : Integer;

Begin
  CommaSeparators         := 0;
  CommasBetweenNumbers    := 0;
  PeriodsBetweenNumbers   := 0;
  LastCommaSeparator      := 0;
  LastCommaBetweenNumbers := -1;  // Initial value must be less than LastCommaSeparator

  // Before we can proceed, make sure the string satisfies certain conditions

  CanConvert := True;
  I          := 2;
  While (I < Length(St)) And CanConvert Do
  Begin
    Case St[I] Of
      ',': Begin
             If (St[I - 1] In ['0'..'9']) And (St[I + 1] In ['0'..'9']) Then
             Begin
               // We might have detected a Euro decimal separator, but we might have also detected
               // a hand-edited US file where no space was put between elements.  We'll try to
               // tell the difference by looking for a comma-space combination coming before this.

               If LastCommaSeparator <= LastCommaBetweenNumbers Then CanConvert := False;
               Inc(CommasBetweenNumbers);
               LastCommaBetweenNumbers := I;
             End;
             If St[I + 1] = ' ' Then
             Begin
               Inc(CommaSeparators);
               LastCommaSeparator := I;
             End;
           End;
      '.': Begin
             // If we detect a US decimal separator, don't try to convert the string

             If (St[I - 1] In ['0'..'9']) And (St[I + 1] In ['0'..'9']) Then CanConvert := False;
           End;
    End;
    Inc(I);
  End;

  // Convert the string if we can

  If CanConvert Then
  Begin
    For I := 2 To Length(St) - 1 Do
    Begin
      If (St[I] = ',') And (St[I - 1] In ['0'..'9']) And (St[I + 1] In ['0'..'9']) Then St[I] := '.';
    End;
  End;
End;

Function GetVisualSQLRefValue(Ref: TSQLRef): String;
Begin
  // Can't use an array in case Ref is outside the legal range
  
  Case Ref Of
    srDoors: Result := 'Doors';
  srObjects: Result := 'Objects';
  Else
    Result := 'None';
  End; // Case
End; // GetVisualSQLRefValue

Function GetSQLRefValueFromVisual(St: String): TSQLRef;
Begin
       If St = 'Doors'   Then Result := srDoors
  Else If St = 'Objects' Then Result := srObjects
  Else Result := srNone;
End; // GetSQLRefValueFromVisual

Function TokenizeAn8ObjectName(St: String; Out BodyMesh,BodyTex,HeadMesh,HeadTex: Integer): Boolean;
// Tokenizes an Anim8or object name according to my naming convention.
// The naming convention for object names is:
//
// body <mesh> <textureset> _head <mesh> <textureset>
//
// The definitions for the mesh ID's are:
//
// type     mesh-ID  meaning
// ----------------------------
// body     00       normal
//          01       robed
//
// head     00       bare
//          01       leather
//          02       chain
//          03       plate
//
// The definitions for the textureset ID's are:
//
// textureset-ID     meaning
// ----------------------------
//          00       bare
//          01       leather
//          02       chain
//          03       plate
//
// TODO: robed textureset ID's
Var BM,BT,HM,HT,E1,E2,E3,E4: Integer;
Begin
  St := LowerCase(St);
  If (Length(St) = 17) And (Copy(St,1,4) = 'body') And (Copy(St,10,4) = 'head') Then
  Begin
    Val(Copy(St, 5,2),BM,E1);
    Val(Copy(St, 7,2),BT,E2);
    Val(Copy(St,14,2),HM,E3);
    Val(Copy(St,16,2),HT,E4);
    If (E1 = 0) And (E2 = 0) And (E3 = 0) And (E4 = 0) Then
    Begin
      BodyMesh := BM;
      BodyTex  := BT;
      HeadMesh := HM;
      HeadTex  := HT;
      Result   := True;
    End
    Else
    Begin
      BodyMesh := 0;
      BodyTex  := 0;
      HeadMesh := 0;
      HeadTex  := 0;
      Result   := False;
    End;
  End
  Else Result := False;
End; // TokenizeAn8ObjectName

Function TokenizeAn8SequenceName(St: String; Out SequenceName,Prefix: String; Out DropLastFrame: Boolean; Out MillisecondsPerFrame: Integer): Boolean;
Var
  I,J : Integer;
  St1 : String;

Begin
  // Load defaults

  SequenceName         := St;
  Prefix               := '';
  DropLastFrame        := False;
  MillisecondsPerFrame := 100;
  Result               := False;

  // Find the prefix (e.g. C05, L01, etc.)

  I := LastDelimiter('_',St);
  If I > 0 Then
  Begin
    Prefix := Copy(St,I + 1,Length(St));
    St     := Copy(St,1,I - 1);

    // Find the time in ms per frame

    I := LastDelimiter('_',St);
    If I > 0 Then
    Begin
      Val(Copy(St,I + 1,Length(St)),MillisecondsPerFrame,J);
      St     := Copy(St,1,I - 1);

      // Find the indicator whether to drop the last frame.  Anim8or doesn't interpolate
      // fron the last frame to the first one again for cylic animations, so for these
      // animations I made sure the last frame was the same as the first.

      I := LastDelimiter('_',St);
      If I > 0 Then
      Begin
        St1 := Copy(St,I + 1,Length(St));
        DropLastFrame := (St1 = 'C'); // C = cyclic animation (and therefore drop the last frame), N = keep it (e.g. for death animation)

        // The rest is the normal name of the sequence

        SequenceName := St;
      End;
      Result := True;
    End;
  End;
End; // TokenizeAn8SequenceName

Function GetFirstTexture(Texture: String): String;
Var TextureInfo: TTextureInfo;
//  Textures   : String;
//  Opacities  : String;
//  Parameters : String;

Begin
  TextureInfo := TTextureInfo.Create(Texture);
  Result      := TextureInfo.FirstTexture;
  TextureInfo.Free;
//  BreakupTextureString(Texture,Textures,Opacities,Parameters);
//  Result := GetToken(';',Textures);
End; // GetFirstTexture
{
Procedure BreakupTextureString(Texture: String; Out Textures,Opacities,Parameters: String);
Var St: String;
Begin
  St         := GetToken('+',Texture);
  Textures   := GetToken('|',St);
  Opacities  := St;
  Parameters := Texture;
End; // BreakupTextureString
}
Function ExtractFileNameNoExt(St: String): String;
Begin
  St := ExtractFileName(St);
  Result := Copy(St,1,Length(St) - Length(ExtractFileExt(St)));
End; // ExtractFileNameNoExt
{
Function GetHessianDistance(P1,P2,P3: T3DPoint): Single;
// P1, P2, and P3 are any three points on a polygon
Var NX,NY,NZ,NL: Single;
Begin
  // Calculate the normal to the polygon

  NX := (P1.Y - P2.Y) * (P3.Z - P2.Z) - (P1.Z - P2.Z) * (P3.Y - P2.Y);
  NY := (P1.Z - P2.Z) * (P3.X - P2.X) - (P1.X - P2.X) * (P3.Z - P2.Z);
  NZ := (P1.X - P2.X) * (P3.Y - P2.Y) - (P1.Y - P2.Y) * (P3.X - P2.X);
  NL := Sqrt(Sqr(NX) + Sqr(NY) + Sqr(NZ));
  If NL <> 0 Then
  Begin
    NX := NX / NL;
    NY := NY / NL;
    NZ := NZ / NL;
  End;
  Result := -(NX * P1.X + NY * P1.Y + NZ * P1.Z);
End; // GetHessianDistance

Function GetHessianDistance(Normal,P: T3DPoint): Single;
// Normal is the normal to a polygon and P is any point on the polygon
Begin
  Result := -Normal.Dot(P);
End; // GetHessianDistance
}
Function det2x2(A,B,C,D: Single): Single;
Begin
  Result := A * D - B * C;
End; // det2x2

Function det3x3(A1,A2,A3,B1,B2,B3,C1,C2,C3: Single): Single;
Begin
  Result := a1 * det2x2 (b2, b3, c2, c3)
          - b1 * det2x2 (a2, a3, c2, c3)
          + c1 * det2x2 (a2, a3, b2, b3);
End; // det3x3
{
// Determine whether or not the line segment p1,p2
// Intersects the 3 vertex facet bounded by pa,pb,pc
// Return the intersection point in intersect if there is one or
// leaves it unmodified otherwise.  Returns true if an intersection
// was found.
//
// The equation of the line is p = p1 + mu (p2 - p1)
// The equation of the plane is a x + b y + c z + d = 0
//                              n.x x + n.y y + n.z z + d = 0

Function LineFacet(P1,P2,PA,PB,PC,Intersect: T3DPoint): Boolean;
Const Epsilon = 0.01;
Var
  D        : Double;
  A1,A2,A3 : Double;
  Total    : Double;
  Denom    : Double;
  Mu       : Double;
  N        : T3DPoint;
  PA1      : T3DPoint;
  PA2      : T3DPoint;
  PA3      : T3DPoint;
  P        : T3DPoint;

  PBA      : T3DPoint;
  PCA      : T3DPoint;
  P21      : T3DPoint;

Begin
  Result := False;

  // Calculate the parameters for the plane

  PBA := T3DPoint.Create(PB);
  PCA := T3DPoint.Create(PC);
  PBA.Subtract(PA);
  PCA.Subtract(PA);

  N   := T3DPoint.Create(PBA);
  N.Cross(PCA);
  N.Normalize;

  D := N.Dot(PA);

  // Calculate the position on the line that intersects the plane

  P21 := T3DPoint.Create(P2);
  P21.Subtract(P1);

  Denom := N.Dot(P21);

  // Do the line and plane intersect?

  If Abs(Denom) >= Epsilon Then
  Begin
    Mu := (D - N.Dot(P1)) / Denom;

    // Is the intersection along the line segment?

    If (Mu >= 0) And (Mu <= 1) Then
    Begin
      P := T3DPoint.Create(P21);
      P.Multiply(Mu);
      P.Add(P1);

      // Determine whether or not the intersection point is bounded by pa,pb,pc

      PA1 := T3DPoint.Create(PA);
      PA1.Subtract(P);
      PA1.Normalize;

      PA2 := T3DPoint.Create(PB);
      PA2.Subtract(P);
      PA2.Normalize;

      PA3 := T3DPoint.Create(PC);
      PA3.Subtract(P);
      PA3.Normalize;

      A1 := PA1.Dot(PA2);
      A2 := PA2.Dot(PA3);
      A3 := PA3.Dot(PA1);

      // Roundoff error can cause us to be outside -1..1

      If A1 > 1  Then A1 := 1;
      If A2 > 1  Then A2 := 1;
      If A3 > 1  Then A3 := 1;
      If A1 < -1 Then A1 := -1;
      If A2 < -1 Then A2 := -1;
      If A3 < -1 Then A3 := -1;

      Total := ArcCos(A1) + ArcCos(A2) + ArcCos(A3);

      If Abs((2 * Pi) - Total) < Epsilon Then
//      If Total < 2 * Pi + Epsilon
      Begin
        Intersect.Copy(P);
        Result := True;
      End;
      P.Free;

      PA1.Free;
      PA2.Free;
      PA3.Free;
    End;
  End;

  // Cleanup

  PBA.Free;
  PCA.Free;
  N.Free;
  P21.Free;
End; // LineFacet

* lines_intersect:  AUTHOR: Mukesh Prasad
 *
 *   This function computes whether two line segments,
 *   respectively joining the input points (x1,y1) -- (x2,y2)
 *   and the input points (x3,y3) -- (x4,y4) intersect.
 *   If the lines intersect, the output variables x, y are
 *   set to coordinates of the point of intersection.
 *
 *   All values are in integers.  The returned value is rounded
 *   to the nearest integer point.
 *
 *   If non-integral grid points are relevant, the function
 *   can easily be transformed by substituting floating point
 *   calculations instead of integer calculations.
 *
 *   Entry
 *        x1, y1,  x2, y2   Coordinates of endpoints of one segment.
 *        x3, y3,  x4, y4   Coordinates of endpoints of other segment.
 *
 *   Exit
 *        x, y              Coordinates of intersection point.
 *
 *   The value returned by the function is one of:
 *
 *        DONT_INTERSECT    0
 *        DO_INTERSECT      1
 *        COLLINEAR         2
 *
 * Error conditions:
 *
 *     Depending upon the possible ranges, and particularly on 16-bit
 *     computers, care should be taken to protect from overflow.
 *
 *     In the following code, 'long' values have been used for this
 *     purpose, instead of 'int'.
 *
 * ===================================================================
 *
 * Converted to floating-point
 *
Function LineIntersection(X1,Y1,X2,Y2,X3,Y3,X4,Y4: Single; Var X,Y: Single): Integer;
Var
  A1,A2,B1,B2,C1,C2 : Single; // Coefficients of line eqns.
  R1,R2,R3,R4       : Single; // 'Sign' values
  Denom,Num         : Single; // Intermediate values

Begin
  // Compute a1, b1, c1, where line joining points 1 and 2 is "a1 x  +  b1 y  +  c1  =  0".

  a1 := y2 - y1;
  b1 := x1 - x2;
  c1 := x2 * y1 - x1 * y2;

  // Compute r3 and r4.

  r3 := a1 * x3 + b1 * y3 + c1;
  r4 := a1 * x4 + b1 * y4 + c1;

  // Check signs of r3 and r4.  If both point 3 and point 4 lie on
  // same side of line 1, the line segments do not intersect.

  If (r3 <> 0) And (r4 <> 0) And (Sign(R3) = Sign(R4)) Then Result := DONT_INTERSECT
  Else
  Begin
    // Compute a2, b2, c2

    a2 := y4 - y3;
    b2 := x3 - x4;
    c2 := x4 * y3 - x3 * y4;

    // Compute r1 and r2 

    r1 := a2 * x1 + b2 * y1 + c2;
    r2 := a2 * x2 + b2 * y2 + c2;

    // Check signs of r1 and r2.  If both point 1 and point 2 lie
    // on same side of second line segment, the line segments do
    // not intersect.

    If (r1 <> 0) And (r2 <> 0) And (Sign(R1) = Sign(R2)) Then Result := DONT_INTERSECT
    Else
    Begin
      // Line segments intersect: compute intersection point.

      Denom := a1 * b2 - a2 * b1;
      If Denom = 0 Then Result := COLLINEAR
      Else
      Begin
        Num := b1 * c2 - b2 * c1;
        X   := Num / Denom;

        Num := a2 * c1 - a1 * c2;
        Y   := Num / Denom;

        Result := DO_INTERSECT;
      End;
    End;
  End;
End; // LineIntersection

// Returns the SIGNED distance from a line given by (x1,y1)-(x2,y2).  By signed this means
// that a point on one side will give a positive distance, and on the other side it will give
// a negative distance (which can be useful).
Function DistanceFromLine(X1,Y1,X2,Y2,X,Y: Single): Single;
Var
  Num,Denom : Single;
Begin
  Denom := Sqr(X2 - X1) + Sqr(Y2 - Y1);
  If Denom <> 0 Then
  Begin
    Num := (Y1 - Y2) * X + (X2 - X1) * Y + (X1 * Y2 - X2 * Y1);
    Result := Num / Sqrt(Denom);
  End
  Else Result := 0; // The point is on the line
End; // DistanceFromLine

// DistanceFromLineSegment: get the distance of a point to a segment.
//    Input:  a Point P and a Segment S (in any dimension)
//    Return: the shortest distance from P to S
Function DistanceFromLineSegment(X1,Y1,X2,Y2,X,Y: Single): Single;
Type
  Vec2D = Record
    X,Y: Single;
  End;

Var
  P,S1,S2 : Vec2D;
  V,W,PB  : Vec2D;
  C1,C2,B : Single;

  Function Dot(Const V1,V2: Vec2D): Single;
  Begin
    Result := V1.X * V2.X + V1.Y * V2.Y;
  End; // Dot

  Function Norm(Const V: Vec2D): Single;
  Begin
    Result := Sqrt(Dot(V,V));
  End; // Norm

  Function Distance(Const V1,V2: Vec2D): Single;
  Var V: Vec2D;
  Begin
    V.X := V1.X - V2.X;
    V.Y := V1.Y - V2.Y;
    Result := Norm(V);
  End; // Distance

Begin
  P.X  := X;
  P.Y  := Y;
  S1.X := X1;
  S1.Y := Y1;
  S2.X := X2;
  S2.Y := Y2;

  V.X  := X2 - X1;
  V.Y  := Y2 - Y1;
  W.X  := X - X1;
  W.Y  := Y - Y1;
  C1   := Dot(W,V);
  If C1 <= 0 Then Result := Distance(P,S1)
  Else
  Begin
    C2 := Dot(V,V);
    If C2 <= C1 Then Result := Distance(P,S2)
    Else
    Begin
      B      := C1 / C2;
      PB.X   := S1.X + B * V.X;
      PB.Y   := S1.Y + B * V.Y;
      Result := Distance(P,PB);
    End;
  End;
End; // DistanceFromLineSegment

**
 * gts_point_segment_closest:
 * @p: a #GtsPoint.
 * @s: a #GtsSegment.
 * @closest: a #GtsPoint.
 *
 * Set the coordinates of @closest to the coordinates of the point belonging
 * to @s closest to @p.
 *)
Procedure gts_point_segment_closest(P,P1,P2,Closest: T3DPoint);
Var T,NS2: Double;
Begin
  NS2 := P1.DistanceFrom2(P2);
  If NS2 = 0 Then Closest.Copy(P1)
  Else
  Begin
    T := ((P2.X - P1.X) * (P.X - P1.X) +
          (P2.Y - P1.Y) * (P.Y - P1.Y) +
          (P2.Z - P1.Z) * (P.Z - P1.Z)) / NS2;
         If T > 1 Then Closest.Copy(P2)
    Else if T < 0 Then Closest.Copy(P1)
    Else Closest.Copy((1 - T) * P1.X + T * P2.X,
                      (1 - T) * P1.Y + T * P2.Y,
                      (1 - T) * P1.Z + T * P2.Z);
  End;
End; // gts_point_segment_closest

(**
 * gts_point_orientation_3d:
 * @p1: a #GtsPoint.
 * @p2: a #GtsPoint.
 * @p3: a #GtsPoint.
 * @p4: a #GtsPoint.
 *
 * Checks if @p4 lies above, below or on the plane passing through the
 * points @p1, @p2 and @p3. Below is defined so that @p1, @p2 and @p3
 * appear in counterclockwise order when viewed from above the
 * plane. The returned value is an approximation of six times the
 * signed volume of the tetrahedron defined by the four points. This
 * function uses adaptive floating point arithmetic and is
 * consequently geometrically robust.
 *
 * Returns: a positive value if @p4 lies below, a negative value if
 * @p4 lies above the plane, zero if the four points are coplanar.
 *)
Function gts_point_orientation_3d(P1,P2,P3,P4: T3DPoint): Double;
Var
  ADX,BDX,CDX : Double;
  ADY,BDY,CDY : Double;
  ADZ,BDZ,CDZ : Double;

Begin
  ADX := P1.X - P4.X;
  BDX := P2.X - P4.X;
  CDX := P3.X - P4.X;
  ADY := P1.Y - P4.Y;
  BDY := P2.Y - P4.Y;
  CDY := P3.Y - P4.Y;
  ADZ := P1.Z - P4.Z;
  BDZ := P2.Z - P4.Z;
  CDZ := P3.Z - P4.Z;

  Result := ADX * (BDY * CDZ - BDZ * CDY) +
            BDX * (CDY * ADZ - CDZ * ADY) +
            CDX * (ADY * BDZ - ADZ * BDY);
End; // gts_point_orientation_3d

**
 * gts_segment_triangle_intersection:
 * @s: a #GtsSegment.
 * @t: a #GtsTriangle.
 * @boundary: if %TRUE, the boundary of @t is taken into account.
 * @klass: a #GtsPointClass to be used for the new point.
 *
 * Checks if @s intersects @t. If this is the case, creates a new
 * point pi intersection of @s with @t.
 *
 * This function is geometrically robust in the sense that it will not
 * return a point if @s and @t do not intersect and will return a
 * point if @s and @t do intersect. However, the point coordinates are
 * subject to round-off errors.
 *
 * Note that this function will not return any point if @s is contained in
 * the plane defined by @t.
 *
 * Returns: a summit of @t (if @boundary is set to %TRUE), one of the endpoints
 * of @s or a new #GtsPoint, intersection of @s with @t or %NULL if @s
 * and @t don't intersect.
 *)
Function gts_segment_triangle_intersection(S1,S2,T1,T2,T3: T3DPoint; Boundary: Boolean): T3DPoint;
Var
  A,B,C,D,E                       : T3DPoint;
  ABCE,ABCD,ADCE,ABDE,BCDE,CC,Tmp : Double;

Begin
  A      := T1;
  B      := T2;
  C      := T3;
  D      := T3DPoint.Create(S1);
  E      := T3DPoint.Create(S2);
  Result := Nil;
  ABCE   := gts_point_orientation_3d(A, B, C, E);
  ABCD   := gts_point_orientation_3d(A, B, C, D);
  If (ABCE < 0) Or (ABCD > 0) Then
  Begin
    E.Exchange(D);
    Tmp  := ABCE;
    ABCE := ABCD;
    ABCD := Tmp;
  End;

  If (ABCE < 0) Or (ABCD > 0) Then
  Begin
    D.Free;
    E.Free;
    Exit;
  End;

  ADCE := gts_point_orientation_3d(A, D, C, E);
  If (Boundary And (ADCE < 0)) Or ((ADCE <= 0) And Not Boundary) Then
  Begin
    D.Free;
    E.Free;
    Exit;
  End;

  ABDE := gts_point_orientation_3d(A, B, D, E);
  If (Boundary And (ABDE < 0)) Or ((ABDE <= 0) And Not Boundary) Then
  Begin
    D.Free;
    E.Free;
    Exit;
  End;

  BCDE := gts_point_orientation_3d(B, C, D, E);
  If (Boundary And (BCDE < 0)) Or ((BCDE <= 0) And Not Boundary) Then
  Begin
    D.Free;
    E.Free;
    Exit;
  End;

  If ABCE = 0 Then
  Begin
    // is s contained in the plane defined by t?

    If ABCD = 0 Then
    Begin
      D.Free;
      E.Free;
      Exit;
    End;
    Result := T3DPoint.Create(E);
    D.Free;
    E.Free;
    Exit;
  End;
  If ABCD = 0 Then
  Begin
    Result := T3DPoint.Create(D);
    D.Free;
    E.Free;
    Exit;
  End;
  If Boundary Then  // corners of @t
  Begin
    If ABDE = 0 Then
    Begin
      If ADCE = 0 Then
      Begin
        Result := T3DPoint.Create(A);
        D.Free;
        E.Free;
        Exit;
      End;
      If BCDE = 0 Then
      Begin
        Result := T3DPoint.Create(B);
        D.Free;
        E.Free;
        Exit;
      End;
    End
    Else If (BCDE = 0) And (ADCE = 0) Then
    Begin
      Result := T3DPoint.Create(C);
      D.Free;
      E.Free;
      Exit;
    End;
  End;
  CC     := ABCE / (ABCE - ABCD);
  Result := T3DPoint.Create(E.X + CC * (D.X - E.X),
                            E.Y + CC * (D.Y - E.Y),
                            E.Z + CC * (D.Z - E.Z));
  D.Free;
  E.Free;
End; // gts_segment_triangle_intersection
}
Procedure LogParseError(St: String);
Begin
  ParseLog.Add(St);
  If ParseLog.Count > MaxParseCount Then ParseLog.Delete(0);
End; // LogParseError

Procedure LoadScriptLibrary;
Var
  S      : TSearchRec;
  SO     : TScripted;
  DecSep : Char;

Begin
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  ScriptLibrary := TStringList.Create;
  If FindFirst(ExtractFilePath(Application.EXEName) + 'library\scripts\*.SCP',faAnyFile,S) = 0 Then
  Begin
    Repeat
      SO      := TScripted.Create;
      SO.Name := Copy(S.Name,1,Length(S.Name) - Length(ExtractFileExt(S.Name)));
      SO.Script.LoadFromFile(ExtractFilePath(Application.EXEName) + 'library\scripts\' + S.Name);
      SO.Compile;
      ScriptLibrary.AddObject(SO.Name,SO);
    Until FindNext(S) <> 0;
  End;
  FindClose(S);
  DecimalSeparator := DecSep;
End; // LoadScriptLibrary

Procedure FreeScriptLibrary;
Var I: LongInt;
Begin
  For I := 0 To ScriptLibrary.Count - 1 Do ScriptLibrary.Objects[I].Free;
  ScriptLibrary.Free;
End; // FreeScriptLibrary

Procedure LoadMeshAndLightLibrary;
Var
  S  : TSearchRec;
  GO : TGroupObject;
  F  : System.Text;
  St : String;

Begin
  MeshLibrary  := TStringList.Create;
  If FindFirst(ExtractFilePath(Application.EXEName) + 'library\meshes\*.MSH',faAnyFile,S) = 0 Then
  Begin
    Repeat
      GO := TGroupObject.Create;
      AssignFile(F,ExtractFilePath(Application.EXEName) + 'library\meshes\' + S.Name);
      Reset(F);

      // Must skip the group declaration

      St := '';
      While (St <> 'GROUP') And Not Eof(F) Do
      Begin
        ReadLn(F,St);
        St := Trim(UpperCase(St));
      End; // While

      GO.LoadFromFile(F);

      CloseFile(F);
      MeshLibrary.AddObject(GO.GetName,GO);
    Until FindNext(S) <> 0;
    MeshLibrary.Sort;
  End;
  FindClose(S);
End; // LoadMeshAndLightLibrary

Procedure FreeMeshAndLightLibrary;
Var I: LongInt;
Begin
  For I := 0 To MeshLibrary.Count  - 1 Do MeshLibrary.Objects[I].Free;
  MeshLibrary.Free;
End; // FreeMeshAndLightLibrary

Procedure LoadCreatureLibrary;
Var S: TSearchRec;
Begin
  CreatureLibrary := TStringList.Create;
  If FindFirst(ExtractFilePath(Application.EXEName) + 'library\creatures\*.an8',faAnyFile,S) = 0 Then
  Begin
    Repeat
      // Don't load it now since it takes too long and will cause the program to
      // take forever to start up.  We'll load them as we need them when exporting.

      CreatureLibrary.AddObject(ExtractFileNameNoExt(S.Name),TAn8File.Create);
    Until FindNext(S) <> 0;
    CreatureLibrary.Sort;
  End;
  FindClose(S);
End; // LoadCreatureLibrary

Procedure FreeCreatureLibrary;
Var I: LongInt;
Begin
  For I := 0 To CreatureLibrary.Count  - 1 Do CreatureLibrary.Objects[I].Free;
  CreatureLibrary.Free;
End; // FreeCreatureLibrary

Function MeshLibraryObjectIndex(St: String): Integer;
Var
  I     : Integer;
  Found : Boolean;

Begin
  I     := 0;
  Found := False;
  St    := UpperCase(St);
  While (I < MeshLibrary.Count) And Not Found Do
  Begin
    If UpperCase(MeshLibrary.Strings[I]) = St Then Found := True Else Inc(I);
  End; // While
  If Found Then Result := I Else Result := -1;
End; // MeshLibraryObjectIndex

Procedure BuildTextureList;
Var
  S : TSearchRec;
  L : TStringList;

Begin
  TextureLibrary.Clear;
  TextureLibrary.Add('(default setting)');
  L        := TStringList.Create;
  L.Sorted := True;

  // Load the default textures

  If FindFirst(ExtractFilePath(Application.EXEName) + 'library\textures\*.BMP',faAnyFile,S) = 0 Then
  Begin
    Repeat
      L.Add(Copy(S.Name,1,Length(S.Name) - Length(ExtractFileExt(S.Name))));
    Until FindNext(S) <> 0;
  End;
  FindClose(S);
//  TextureLibrary.AddStrings(L);

//  L.Clear;
  If FindFirst(ExtractFilePath(Application.EXEName) + 'library\textures\*.TGA',faAnyFile,S) = 0 Then
  Begin
    Repeat
      L.Add(Copy(S.Name,1,Length(S.Name) - Length(ExtractFileExt(S.Name))));
    Until FindNext(S) <> 0;
  End;
  FindClose(S);
  TextureLibrary.AddStrings(L);

  // Load the textures from the currently selected texture set

  If TextureSet <> '' Then
  Begin
    L.Clear;
    If FindFirst(ExtractFilePath(Application.EXEName) + 'library\textures\' + TextureSet + '\*.BMP',faAnyFile,S) = 0 Then
    Begin
      Repeat
        L.Add(TextureSet + '\' + Copy(S.Name,1,Length(S.Name) - Length(ExtractFileExt(S.Name))));
      Until FindNext(S) <> 0;
    End;
    FindClose(S);
//    TextureLibrary.AddStrings(L);

//    L.Clear;
    If FindFirst(ExtractFilePath(Application.EXEName) + 'library\textures\' + TextureSet + '\*.TGA',faAnyFile,S) = 0 Then
    Begin
      Repeat
        L.Add(TextureSet + '\' + Copy(S.Name,1,Length(S.Name) - Length(ExtractFileExt(S.Name))));
      Until FindNext(S) <> 0;
    End;
    FindClose(S);
    TextureLibrary.AddStrings(L);
  End;
  L.Free;
End; // BuildTextureList

Procedure SetTextureSet(St: String);
Begin
  TextureSet := St;
  BuildTextureList;
End; // SetTextureSet

Procedure LoadTextureLibraries;
Var S: TSearchRec;
Begin
  TextureLibrary   := TSharedComboBoxStrings.Create;
  TextureSet       := '';
  TextureLibraries := TStringList.Create;
  TextureLibraries.CaseSensitive := False;
  TextureLibraries.Sorted        := True;
  BuildTextureList;

  // Find out all the texture libraries

  If FindFirst(ExtractFilePath(Application.EXEName) + 'library\textures\*.*',faAnyFile,S) = 0 Then
  Begin
    Repeat
      If ((S.Attr And faDirectory) <> 0) And
         (S.Name <> '.')                 And
         (S.Name <> '..')                Then TextureLibraries.Add(S.Name);
    Until FindNext(S) <> 0;
  End;
  FindClose(S);
End; // LoadTextureLibraries

Procedure FreeTextureLibraries;
Var I: Integer;
Begin
  TextureLibrary.Free;
  For I := 0 To TextureLibraries.Count - 1 Do TextureLibraries.Objects[I].Free;
  TextureLibraries.Free;
End; // FreeTextureLibraries

Function GetToken(Delim: String; Var St: String; PreserveDoubleQuotes: Boolean): String;
Var I: Integer;
Begin
  If St <> '' Then
  Begin
    // Special handling for strings enclosed in double quotes

    If St[1] = '"' Then
    Begin
      St     := Copy(St,2,Length(St));
      If PreserveDoubleQuotes
       Then Result := '"' + GetToken('"',St) + '"'
       Else Result := GetToken('"',St);
      GetToken(Delim,St);
    End
    Else
    Begin
      I := Pos(Delim,St);
      If I <> 0 Then
      Begin
        Result := Trim(Copy(St,1,I - 1));
        St     := Trim(Copy(St,I + Length(Delim),Length(St)));
      End
      Else
      Begin
        Result := Trim(St);
        St     := '';
      End;
    End;
  End
  Else Result := '';
End; // GetToken

Procedure GetTokens(Delim: String; Var St: String; Var TokenArray: TTokenArray; PreserveDoubleQuotes: Boolean);
Var
  Token          : String;
  C              : Char;
  I,J{,K,L}      : Integer;
  P0,PJ,PK,PL,P1 : PChar;
  Len            : Integer;

Begin
  SetLength(TokenArray,0);
  St := Trim(St);
  If (St <> '') And (Pos('"',St) = 0) And (Length(Delim) = 1) Then
  Begin
    // Since this string has no double-quote characters, and the delimiter is only one
    // character, we can quickly determine the number of tokens in the string and pre-
    // allocate them -- it will save time in heap management

    C   := Delim[1];
    J   := 0;
    Len := Length(St);
    For I := 1 To Len Do
    Begin
      If St[I] = C Then Inc(J);
    End; // For I
    If St[Len] <> C Then Inc(J);
    SetLength(TokenArray,J);

    // The code below depends on St being an AnsiString (single-byte characters)

    P0 := @St[1];
    PJ := P0;
    PK := P0;
    P1 := P0;
    Inc(LongWord(P1),Len);
    For I := 0 To High(TokenArray) Do
    Begin
      While (PK^ <> C) And (PK <> P1) Do Inc(LongWord(PK));
      If LongWord(PK) < LongWord(P1) Then
      Begin
        While (PJ^ = ' ') And (LongWord(PJ) < LongWord(PK)) Do Inc(LongWord(PJ));
        PL := PK;
        While (PL^ = ' ') And (LongWord(PL) > LongWord(PJ)) Do Dec(LongWord(PL));

        If LongWord(PL) > LongWord(PJ) Then
        Begin
          SetLength(TokenArray[I],LongWord(PL) - LongWord(PJ));
          Move(PJ^,TokenArray[I][1],LongWord(PL) - LongWord(PJ));
        End;

        Inc(PK);
        PJ := PK;
      End
      Else
      Begin
        J := LongWord(PJ) - LongWord(P0) + 1;
        TokenArray[I] := Trim(Copy(St,J,Len));
      End;
    End; // For I

{
    // This remarked-out code doesn't depend on characters being only one byte

    J := 1;
    K := 1;
    For I := 0 To High(TokenArray) Do
    Begin
      While (St[K] <> C) And (K <= Len) Do Inc(K);
      If K <= Len Then
      Begin
        While (St[J] = ' ') And (J < K) Do Inc(J);
        L := K;
        While (St[L] = ' ') And (L > J) Do Dec(L);

        TokenArray[I] := Copy(St,J,L - J);
        Inc(K);
        J := K;
      End
      Else TokenArray[I] := Trim(Copy(St,J,Len));
    End; // For I
}
    St := '';
  End
  Else
  Begin
    While St <> '' Do
    Begin
      Token := GetToken(Delim,St,PreserveDoubleQuotes);
      SetLength(TokenArray,High(TokenArray) + 2);
      TokenArray[High(TokenArray)] := Token;
    End; // While
  End;
End; // GetTokens

Function CompressTextureList(St: String): String;
Var
  Tokens : TTokenArray;
  I      : Integer;
  S      : String;
Begin
  // Remove all whitespace between textures in the list. This is important because,
  // when we export to .WLD, we'll have to compare textures against a master list
  // to see if they've already been added to the file.  We can't do this unless
  // they've all been standardized.
  //
  // Note how multiple textures are delimited by semicolons, not colons, so the
  // token count in other code isn't thrown off.
  
  GetTokens(';',St,Tokens);
  S := '';
  For I := 0 To High(Tokens) Do
  Begin
    S := S + Tokens[I];
    If I < High(Tokens) Then S := S + ';';
  End; // For I
  SetLength(Tokens,0);
  Result := S;
End; // CompressTextureList

Function GetIntValue(Token: String; Var V: Integer): Boolean;
Var I: Integer;
Begin
  Val(Token,V,I);
  Result := (I = 0);
End; // GetIntValue

Function GetIntValues(Token: String; Var V: TIntegerArray): Boolean;
Var I,J: Integer;
  Tokens : TTokenArray;
Begin
  GetTokens('|',Token,Tokens);
  SetLength(V,High(Tokens) + 1);
  J := 0;
  Repeat
    Val(Tokens[J],V[J],I);
    Inc(J);
  Until (J > High(Tokens)) Or (I <> 0);
  Result := (I = 0);
End; // GetIntValues

Function GetSingleValue(Token: String; Var V: Single): Boolean;
Var I: Integer;
Begin
  Val(Token,V,I);
  Result := (I = 0);
End; // GetSingleValue

Function GetDoubleValue(Token: String; Var V: Double): Boolean;
Var I: Integer;
Begin
  Val(Token,V,I);
  Result := (I = 0);
End; // GetDoubleValue

Function GetBooleanValue(Token: String; Var V: Boolean): Boolean;
Begin
  Token := UpperCase(Token);
  If Token = 'TRUE' Then
  Begin
    V      := True;
    Result := True;
  End
  Else If Token = 'FALSE' Then
  Begin
    V      := False;
    Result := True;
  End
  Else Result := False;
End; // GetBooleanValue

// ------------------------------
// TZoneBound
// ------------------------------

Constructor TZoneBound.Create;
Begin
  X1 := 0;
  Y1 := 0;
  Z1 := 0;
  X2 := 0;
  Y2 := 0;
  Z2 := 0;
End; // TZoneBound.Create

Constructor TZoneBound.Create(CI: TConfigurableItem);
Begin
  Copy(CI);
End; // TZoneBound.Create

Constructor TZoneBound.Create(AX1,AY1,AZ1,AX2,AY2,AZ2: Single);
Begin
  X1 := AX1;
  Y1 := AY1;
  Z1 := AZ1;
  X2 := AX2;
  Y2 := AY2;
  Z2 := AZ2;
End; // TZoneBound.Create

Procedure TZoneBound.Copy(CI: TConfigurableItem);
Begin
  X1 := TZoneBound(CI).X1;
  Y1 := TZoneBound(CI).Y1;
  Z1 := TZoneBound(CI).Z1;
  X2 := TZoneBound(CI).X2;
  Y2 := TZoneBound(CI).Y2;
  Z2 := TZoneBound(CI).Z2;
End; // TZoneBound.Copy

Function TZoneBound.MakeCopy: TConfigurableItem;
Begin
  Result := TZoneBound.Create(Self);
End; // TZoneBound.MakeCopy

// ------------------------------
// TZonePlane
// ------------------------------

Constructor TZonePlane.Create;
Begin
  X1           := 0;
  Y1           := 0;
  Z1           := 0;
  X2           := 0;
  Y2           := 0;
  Z2           := 0;
  InfiniteZ    := False;
  DestZoneID   := 0;
  DestX        := 0;
  DestY        := 0;
  DestZ        := 0;
  DestAngle    := 0;
  HasDestX     := False;
  HasDestY     := False;
  HasDestZ     := False;
  HasDestAngle := False;
End; // TZonePlane.Create

Constructor TZonePlane.Create(CI: TConfigurableItem);
Begin
  Copy(CI);
End; // TZonePlane.Create

Procedure TZonePlane.Copy(CI: TConfigurableItem);
Begin
  X1           := TZonePlane(CI).X1;
  Y1           := TZonePlane(CI).Y1;
  Z1           := TZonePlane(CI).Z1;
  X2           := TZonePlane(CI).X2;
  Y2           := TZonePlane(CI).Y2;
  Z2           := TZonePlane(CI).Z2;
  InfiniteZ    := TZonePlane(CI).InfiniteZ;
  DestZoneID   := TZonePlane(CI).DestZoneID;
  DestX        := TZonePlane(CI).DestX;
  DestY        := TZonePlane(CI).DestY;
  DestZ        := TZonePlane(CI).DestZ;
  DestAngle    := TZonePlane(CI).DestAngle;
  HasDestX     := TZonePlane(CI).HasDestX;
  HasDestY     := TZonePlane(CI).HasDestY;
  HasDestZ     := TZonePlane(CI).HasDestZ;
  HasDestAngle := TZonePlane(CI).HasDestAngle;
End; // TZonePlane.Copy

Function TZonePlane.MakeCopy: TConfigurableItem;
Begin
  Result := TZonePlane.Create(Self);
End; // TZonePlane.MakeCopy

// ------------------------------
// TSound
// ------------------------------

Constructor TSound.Create;
Begin
  DayName   := '';
  NightName := '';
  Area      := False;
  X         := 0;
  Y         := 0;
  Z         := 0;
  Radius    := 0;
End; // TSound.Create

Constructor TSound.Create(CI: TConfigurableItem);
Begin
  Copy(CI);
End; // TSound.Create

Procedure TSound.Copy(CI: TConfigurableItem);
Begin
  DayName   := TSound(CI).DayName;
  NightName := TSound(CI).NightName;
  Area      := TSound(CI).Area;
  X         := TSound(CI).X;
  Y         := TSound(CI).Y;
  Z         := TSound(CI).Z;
  Radius    := TSound(CI).Radius;
End; // TSound.Copy

Function TSound.MakeCopy: TConfigurableItem;
Begin
  Result := TSound.Create(Self);
End; // TSound.MakeCopy

// ------------------------------
// TTree
// ------------------------------

Constructor TTree.Create(Mesh: TMeshObject; CreateBoundMesh: Boolean);
Var I: Integer;
Begin
  Root := TRegion.Create(Mesh,CreateBoundMesh);
  SetLength(Root.Polygons,Mesh.Polygons.Count);
  For I := 0 To High(Root.Polygons) Do Root.Polygons[I] := I;
End; // TTree.Create

Destructor TTree.Destroy;
Begin
  Root.Free;
End; // TTree.Destroy

Procedure TTree.SplitAlongAxis(Region: TRegion; Axis: TAxis; DivPt: Single; FlaggedOnly,NormalHigh: Boolean);
Var Normal: T3DPoint;
Begin
  Normal := T3DPoint.Create(0,0,0);
  If NormalHigh Then
  Begin
    Case Axis Of
      taX: Normal.X := 1;
      taY: Normal.Y := 1;
      taZ: Normal.Z := 1;
    End; // Case
    Region.SplitAlongPlane(Normal,-DivPt,FlaggedOnly,False);
  End
  Else
  Begin
    Case Axis Of
      taX: Normal.X := -1;
      taY: Normal.Y := -1;
      taZ: Normal.Z := -1;
    End; // Case
    Region.SplitAlongPlane(Normal,DivPt,FlaggedOnly,False);
  End;
  Normal.Free;
End; // TTree.SplitAlongAxis

Procedure TTRee.MakeOctree;
Var
  V         : T3DPoint;
  MinPt     : T3DPoint;
  MaxPt     : T3DPoint;
  I         : LongInt;
  SplitSize : Integer;

  Procedure Split(Region: TRegion);
  Var
    Normal : T3DPoint;
    DivPt  : T3DPoint;

  Begin
    If (Region <> Nil) And (High(Region.Polygons) >= 0) Then
    Begin
      Region.GetAbsoluteBounds(Region.MinPt,Region.MaxPt);
      If (Region.MaxPt.X - Region.MinPt.X > SplitSize) Or
         (Region.MaxPt.Y - Region.MinPt.Y > SplitSize) Or
         (Region.MaxPt.Z - Region.MinPt.Z > SplitSize) Then
      Begin
        DivPt  := T3DPoint.Create(Region.MinPt);
        DivPt.Add(Region.MaxPt);
        DivPt.Divide(2);

        Normal := T3DPoint.Create(1,0,0);
        If Region.MaxPt.X - Region.MinPt.X > SplitSize
         Then Region.SplitAlongPlane(Normal,-DivPt.X,False,False)
         Else Region.SplitAlongPlane(Normal,-(Region.MaxPt.X - SplitSize),False,False);

        Normal.Copy(0,1,0);
        If Region.MaxPt.Y - Region.MinPt.Y > SplitSize
         Then Region.SplitAlongPlane(Normal,-DivPt.Y,False,False)
         Else Region.SplitAlongPlane(Normal,-(Region.MaxPt.Y - SplitSize),False,False);

        Normal.Copy(0,0,1);
        If Region.MaxPt.Z - Region.MinPt.Z > SplitSize
         Then Region.SplitAlongPlane(Normal,-DivPt.Z,False,False)
         Else Region.SplitAlongPlane(Normal,-(Region.MaxPt.Z - SplitSize),False,False);

        Split(Region.GetOctreeChild(False,False,False));
        Split(Region.GetOctreeChild(False,False,True));
        Split(Region.GetOctreeChild(False,True, False));
        Split(Region.GetOctreeChild(False,True, True));
        Split(Region.GetOctreeChild(True, False,False));
        Split(Region.GetOctreeChild(True, False,True));
        Split(Region.GetOctreeChild(True, True, False));
        Split(Region.GetOctreeChild(True, True, True));

        Normal.Free;
        DivPt.Free;
      End;
    End;
  End; // Split

Begin
  // Initialize boundary points

  MinPt := T3DPoint.Create(9999999,9999999,9999999);
  MaxPt := T3DPoint.Create(-9999999,-9999999,-9999999);

  // Determine the zone bounds

  V := T3DPoint.Create;
  For I := 0 To Root.Mesh.Vertices.Count - 1 Do
  Begin
    V.Copy(T3DPoint(Root.Mesh.Vertices.Objects[I]));
    Root.Mesh.MakeAbsolute(V);
    If V.X < MinPt.X Then MinPt.X := V.X;
    If V.Y < MinPt.Y Then MinPt.Y := V.Y;
    If V.Z < MinPt.Z Then MinPt.Z := V.Z;
    If V.X > MaxPt.X Then MaxPt.X := V.X;
    If V.Y > MaxPt.Y Then MaxPt.Y := V.Y;
    If V.Z > MaxPt.Z Then MaxPt.Z := V.Z;
  End; // For I
  V.Free;

  // Figure out an appropriate splitting size that keeps the total number of
  // regions down.

  SplitSize := RegionSize;

  // Initialize the root region's bounds

  Root.BoundMesh.Free;
  Root.BoundMesh := TMeshObject.Create;
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MinPt.Y,MinPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MinPt.Y,MaxPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MinPt.Y,MaxPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MinPt.Y,MinPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MaxPt.Y,MinPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MaxPt.Y,MaxPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MaxPt.Y,MaxPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MaxPt.Y,MinPt.Z));
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([0,1,2,3],'')); // Front
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([7,6,5,4],'')); // Back
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([4,5,1,0],'')); // Left
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([3,2,6,7],'')); // Right
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([1,5,6,2],'')); // Top
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([3,7,4,0],'')); // Bottom
  Root.CalcBounds;
  Root.BoundMesh.CalcNormals;

  Split(Root);

  // Cleanup (we must *not* call TMeshObject.Coalesce or vertex normals get screwed up.
  // Sharing vertices is BAD, BAD, BAD)

  MinPt.Free;
  MaxPt.Free;
End; // TTRee.MakeOctree

Procedure TTree.SplitAlongWater(Var Water: TWater);
Var
  Normal   : T3DPoint;
  Dist     : Single;
  A,B      : Single;
  T        : Single;
  T1,T2    : Single;
  C        : Single;
  S        : Single;
  Hyp      : Single;
  X0,Y0,Z0 : Single;
  CX,CY    : Single;
  I,J      : Integer;
  V        : T3DPoint;
  MinPt    : T3DPoint;
  MaxPt    : T3DPoint;
  List     : TStringList;
  Region   : TRegion;

Begin
  // Initialize boundary points

  MinPt := T3DPoint.Create(9999999,9999999,9999999);
  MaxPt := T3DPoint.Create(-9999999,-9999999,-9999999);

  // Determine the zone bounds

  For I := 0 To Root.Mesh.Vertices.Count - 1 Do
  Begin
    V := T3DPoint(Root.Mesh.Vertices.Objects[I]);
    If V.X < MinPt.X Then MinPt.X := V.X;
    If V.Y < MinPt.Y Then MinPt.Y := V.Y;
    If V.Z < MinPt.Z Then MinPt.Z := V.Z;
    If V.X > MaxPt.X Then MaxPt.X := V.X;
    If V.Y > MaxPt.Y Then MaxPt.Y := V.Y;
    If V.Z > MaxPt.Z Then MaxPt.Z := V.Z;
  End; // For I

  // Make sure we have a bounding mesh

  If Root.BoundMesh = Nil Then
  Begin
    Root.BoundMesh := TMeshObject.Create;
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MinPt.Y,MinPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MinPt.Y,MaxPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MinPt.Y,MaxPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MinPt.Y,MinPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MaxPt.Y,MinPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MaxPt.Y,MaxPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MaxPt.Y,MaxPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MaxPt.Y,MinPt.Z));
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([0,1,2,3],'')); // Front
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([7,6,5,4],'')); // Back
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([4,5,1,0],'')); // Left
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([3,2,6,7],'')); // Right
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([1,5,6,2],'')); // Top
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([3,7,4,0],'')); // Bottom
    Root.CalcBounds;
    Root.BoundMesh.CalcNormals;
  End;

  Root.ClearFlags;
  List := TStringList.Create;

  // Make a list of all regions that intersect the water volume

  MinPt.X := Water.MinX;
  MinPt.Y := Water.MinY;
  If Water.HasDepth Then MinPt.Z := Water.Level - Water.Depth;
  MaxPt.Copy(Water.MinX + Water.XSize,Water.MinY + Water.YSize,Water.Level);
  Root.GetRegionsInVolume(List,MinPt,MaxPt,False);

  // Flag those regions

  For J := 0 To List.Count - 1 Do
  Begin
    Region := TRegion(List.Objects[J]);
    Region.Flag := True;
  End; // For J

  // Split only the flagged regions

  SplitAlongAxis(Root,taZ,Water.Level,True,False);

  If Water.Shape = wsIrregular Then
  Begin
    Normal := T3DPoint.Create;
    For I := 0 To High(Water.Irregular) Do
    Begin
      Normal.Copy(Water.Irregular[I].NX,Water.Irregular[I].NY,Water.Irregular[I].NZ);
      Root.SplitAlongPlane(Normal,Water.Irregular[I].Dist,True,False);
    End; // For I
    Normal.Free;
  End
  Else
  Begin
    SplitAlongAxis(Root,taX,Water.MinX,True,True);
    SplitAlongAxis(Root,taX,Water.MinX + Water.XSize,True,False);
    SplitAlongAxis(Root,taY,Water.MinY,True,True);
    SplitAlongAxis(Root,taY,Water.MinY + Water.YSize,True,False);
    If Water.HasDepth Then SplitAlongAxis(Root,taZ,Water.Level - Water.Depth,True,True);
    If Water.Shape = wsElliptical Then
    Begin
      Normal := T3DPoint.Create;
      A      := Abs(Water.XSize) / 2;
      B      := Abs(Water.YSize) / 2;
      CX     := Water.MinX + Water.XSize / 2;
      CY     := Water.MinY + Water.YSize / 2;
      Z0     := Water.Level;
      If (A <> 0) And (B <> 0) Then
      Begin
        For I := 0 To EllipticalSections - 1 Do
        Begin
          T        := I * (2 * Pi) / EllipticalSections;
          T1       := (I - 0.5) * (2 * Pi) / EllipticalSections;
          T2       := (I + 0.5) * (2 * Pi) / EllipticalSections;
          C        := (Cos(T1) + Cos(T2)) / 2;
          S        := (Sin(T1) + Sin(T2)) / 2;
          Hyp      := Sqrt(Sqr(B * Cos(T)) + Sqr(A * Sin(T)));
          Normal.X := -B * Cos(T) / Hyp;
          Normal.Y := -A * Sin(T) / Hyp;
          Normal.Z := 0;
          Normal.Normalize;
          X0       := CX + A * C;
          Y0       := CY + B * S;
          Dist     := -Normal.X * X0 - Normal.Y * Y0 - Normal.Z * Z0;
          Root.SplitAlongPlane(Normal,Dist,True,False);
        End; // For I
      End;
      Normal.Free;
    End;
  End;
  List.Free;
  MinPt.Free;
  MaxPt.Free;
End; // TTree.SplitAlongWater

Procedure TTree.SplitAlongZonePlane(Var ZonePlane: TZonePlane);
Var
  Normal   : T3DPoint;
  Dist     : Single;
  I,J      : Integer;
  V        : T3DPoint;
  MinPt    : T3DPoint;
  MaxPt    : T3DPoint;
  List     : TStringList;
  Region   : TRegion;

Begin
  // Initialize boundary points

  MinPt := T3DPoint.Create(9999999,9999999,9999999);
  MaxPt := T3DPoint.Create(-9999999,-9999999,-9999999);

  // Determine the zone bounds

  For I := 0 To Root.Mesh.Vertices.Count - 1 Do
  Begin
    V := T3DPoint(Root.Mesh.Vertices.Objects[I]);
    If V.X < MinPt.X Then MinPt.X := V.X;
    If V.Y < MinPt.Y Then MinPt.Y := V.Y;
    If V.Z < MinPt.Z Then MinPt.Z := V.Z;
    If V.X > MaxPt.X Then MaxPt.X := V.X;
    If V.Y > MaxPt.Y Then MaxPt.Y := V.Y;
    If V.Z > MaxPt.Z Then MaxPt.Z := V.Z;
  End; // For I

  // Make sure we have a bounding mesh

  If Root.BoundMesh = Nil Then
  Begin
    Root.BoundMesh := TMeshObject.Create;
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MinPt.Y,MinPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MinPt.Y,MaxPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MinPt.Y,MaxPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MinPt.Y,MinPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MaxPt.Y,MinPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MaxPt.Y,MaxPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MaxPt.Y,MaxPt.Z));
    Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MaxPt.Y,MinPt.Z));
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([0,1,2,3],'')); // Front
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([7,6,5,4],'')); // Back
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([4,5,1,0],'')); // Left
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([3,2,6,7],'')); // Right
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([1,5,6,2],'')); // Top
    Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([3,7,4,0],'')); // Bottom
    Root.CalcBounds;
    Root.BoundMesh.CalcNormals;
  End;

  Root.ClearFlags;
  List := TStringList.Create;

  // Make a list of all regions that might intersect the zone line volume

  MinPt.X := Min(ZonePlane.X1,ZonePlane.X2) - ZonePlaneThickness / 2;
  MinPt.Y := Min(ZonePlane.Y1,ZonePlane.Y2) - ZonePlaneThickness / 2;
  If Not ZonePlane.InfiniteZ Then MinPt.Z := Min(ZonePlane.Z1,ZonePlane.Z2) - ZonePlaneThickness / 2;
  MaxPt.X := Max(ZonePlane.X1,ZonePlane.X2) + ZonePlaneThickness / 2;
  MaxPt.Y := Max(ZonePlane.Y1,ZonePlane.Y2) + ZonePlaneThickness / 2;
  If Not ZonePlane.InfiniteZ Then MaxPt.Z := Max(ZonePlane.Z1,ZonePlane.Z2) + ZonePlaneThickness / 2;
  Root.GetRegionsInVolume(List,MinPt,MaxPt,False);

  // Flag those regions

  For J := 0 To List.Count - 1 Do
  Begin
    Region      := TRegion(List.Objects[J]);
    Region.Flag := True;
  End; // For J

  // Split only the flagged regions

  Dist     := DistanceFromLine(ZonePlane.X1,ZonePlane.Y1,ZonePlane.X2,ZonePlane.Y2,0,0);
  Normal   := T3DPoint.Create;
  Normal.X := ZonePlane.Y2 - ZonePlane.Y1;
  Normal.Y := -(ZonePlane.X2 - ZonePlane.X1);
  Normal.Normalize;
  Normal.Multiply(-1);
  Root.SplitAlongPlane(Normal,Dist + ZonePlaneThickness / 2,True,False);
  Normal.Multiply(-1);
  Root.SplitAlongPlane(Normal,-(Dist - ZonePlaneThickness / 2),True,False);
  Normal.Free;

  List.Free;
  MinPt.Free;
  MaxPt.Free;
End; // TTree.SplitAlongZonePlane

Procedure TTree.SplitAlongGrid(MaxPolysPerGrid,MaxVertsPerGrid: Integer);
Var
  V         : T3DPoint;
  MinPt     : T3DPoint;
  MaxPt     : T3DPoint;
  I{,J}       : LongInt;
  Normal    : T3DPoint;
  SplitSize : Integer;

  Procedure Split(Normal: T3DPoint; ZoneMin,ZoneMax: Single);
  Var DivPt: Single;
  Begin
    If ZoneMax - ZoneMin > SplitSize Then
    Begin
      DivPt := (ZoneMin + ZoneMax) / 2;
      Root.SplitAlongPlane(Normal,-DivPt,False,False);
      Split(Normal,ZoneMin,DivPt);
      Split(Normal,DivPt,ZoneMax);
    End;
  End; // Split

Begin
  // Initialize boundary points

  MinPt := T3DPoint.Create(9999999,9999999,9999999);
  MaxPt := T3DPoint.Create(-9999999,-9999999,-9999999);

  // Determine the zone bounds

  V := T3DPoint.Create;
  For I := 0 To Root.Mesh.Vertices.Count - 1 Do
  Begin
    V.Copy(T3DPoint(Root.Mesh.Vertices.Objects[I]));
    Root.Mesh.MakeAbsolute(V);
    If V.X < MinPt.X Then MinPt.X := V.X;
    If V.Y < MinPt.Y Then MinPt.Y := V.Y;
    If V.Z < MinPt.Z Then MinPt.Z := V.Z;
    If V.X > MaxPt.X Then MaxPt.X := V.X;
    If V.Y > MaxPt.Y Then MaxPt.Y := V.Y;
    If V.Z > MaxPt.Z Then MaxPt.Z := V.Z;
  End; // For I
  V.Free;

  // Figure out an appropriate splitting size that keeps the total number of
  // regions down.

  SplitSize := RegionSize;
  While (Round((MaxPt.X - MinPt.X) / SplitSize) *
         Round((MaxPt.Y - MinPt.Y) / SplitSize) *
         Round((MaxPt.Z - MinPt.Z) / SplitSize) > 4096) And
        (SplitSize < 8192) Do SplitSize := SplitSize * 2;

  Normal := T3DPoint.Create;

  // Initialize the root region's bounds

  Root.BoundMesh.Free;
  Root.BoundMesh := TMeshObject.Create;
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MinPt.Y,MinPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MinPt.Y,MaxPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MinPt.Y,MaxPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MinPt.Y,MinPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MaxPt.Y,MinPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MaxPt.Y,MaxPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MaxPt.Y,MaxPt.Z));
  Root.BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MaxPt.Y,MinPt.Z));
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([0,1,2,3],'')); // Front
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([7,6,5,4],'')); // Back
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([4,5,1,0],'')); // Left
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([3,2,6,7],'')); // Right
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([1,5,6,2],'')); // Top
  Root.BoundMesh.Polygons.AddObject('',TPolygon.Create([3,7,4,0],'')); // Bottom
  Root.CalcBounds;
  Root.BoundMesh.CalcNormals;

  // A recursive split like this results in a nicely balanced tree

  Root.Split(MaxPolysPerGrid,MaxVertsPerGrid);
{
    Normal.X := 1;
    Normal.Y := 0;
    Normal.Z := 0;
    Split(Normal,MinPt.X,MaxPt.X);

    Normal.X := 0;
    Normal.Y := 1;
    Normal.Z := 0;
    Split(Normal,MinPt.Y,MaxPt.Y);

    Normal.X := 0;
    Normal.Y := 0;
    Normal.Z := 1;
    Split(Normal,MinPt.Z,MaxPt.Z);
}
{
  Repeat
    Normal.X := 1;
    Normal.Y := 0;
    Normal.Z := 0;
    Split(Normal,MinPt.X,MaxPt.X);

    Normal.X := 0;
    Normal.Y := 1;
    Normal.Z := 0;
    Split(Normal,MinPt.Y,MaxPt.Y);

    Normal.X := 0;
    Normal.Y := 0;
    Normal.Z := 1;
    Split(Normal,MinPt.Z,MaxPt.Z);
    I := 0;
    I := Root.GetMaxPolyCount(I);
    If I > 1024 Then
    Begin
      J := Round(Power((I Div 1024) + 1,0.3333));
      If J < 2 Then J := 2;
      SplitSize := SplitSize Div J;
    End;
  Until I <= 1024; // The absolute limit is 16384 but it's better for the client to have less polygons to process
}
  // Cleanup (we must *not* call TMeshObject.Coalesce or vertex normals get screwed up.
  // Sharing vertices is BAD, BAD, BAD)

  Normal.Free;
  MinPt.Free;
  MaxPt.Free;
End; // TTree.SplitAlongGrid

Function TTree.SplitOffMesh(Attrs: TRegionAttributes): TMeshObject;
// Creates a new TMeshObject containing only those polygons inside regions with the given
// attribute.  Removes those polygons and associated regions from this mesh and region tree.
Var
  MO1    : TMeshObject;
  I      : Integer;
  X,Y    : Integer;
  VAlias : Array Of Integer;
  PAlias : Array Of Integer;
  P      : TPolygon;

  Procedure MoveWaterPolygons(Region: TRegion; MO1: TMeshObject);
  Var
    I,X{,OldV}   : Integer;
    P          : TPolygon;
    V,N        : T3DPoint;

  Begin
    If Region.Attribute.Attr In Attrs Then
    Begin
      I       := 0;
      While I <= High(Region.Polygons) Do
      Begin
        P := TPolygon(Region.Mesh.Polygons.Objects[Region.Polygons[I]]);
        Region.Mesh.Polygons.Objects[Region.Polygons[I]] := Nil; // Placeholder for later culling
        MO1.Polygons.AddObject('',P);
        For X := 0 To High(P.Vertices) Do
        Begin
          V := T3DPoint(Region.Mesh.Vertices.Objects[P.Vertices[X]]);
          N := T3DPoint(Region.Mesh.Normals.Objects[P.Vertices[X]]);
          If VAlias[P.Vertices[X]] < 0 Then
          Begin
//            OldV                  := P.Vertices[X];
            VAlias[P.Vertices[X]] := Y;
            P.Vertices[X]         := Y;
            MO1.Vertices.AddObject('',T3DPoint.Create(V));
            MO1.Normals.AddObject('',T3DPoint.Create(N));
            Inc(Y);
          End
          Else P.Vertices[X] := VAlias[P.Vertices[X]];
        End; // For X
        Inc(I);
      End; // While
      SetLength(Region.Polygons,0);
    End;
    If Region.Left  <> Nil Then MoveWaterPolygons(Region.Left,MO1);
    If Region.Right <> Nil Then MoveWaterPolygons(Region.Right,MO1);

    // If either or both subregions were totally drained, get rid of them and
    // move any remainder back up to this region

    If (Region.Left <> Nil) And (Region.Right <> Nil) And
       ((High(Region.Left.Polygons) < 0) Or (High(Region.Right.Polygons) < 0)) Then
    Begin
      If High(Region.Left.Polygons) >= 0 Then
      Begin
        Region.Flag := Region.Left.Flag;
        SetLength(Region.Polygons,High(Region.Left.Polygons) + 1);
        For I := 0 To High(Region.Polygons) Do Region.Polygons[I] := Region.Left.Polygons[I];
      End
      Else If High(Region.Right.Polygons) >= 0 Then
      Begin
        Region.Flag := Region.Right.Flag;
        SetLength(Region.Polygons,High(Region.Right.Polygons) + 1);
        For I := 0 To High(Region.Polygons) Do Region.Polygons[I] := Region.Right.Polygons[I];
      End
      Else Region.Flag  := False;
      Region.Left.Free;
      Region.Right.Free;
      Region.Left  := Nil;
      Region.Right := Nil;
    End;
  End; // MoveWaterPolygons

  Procedure RemapPolygons(Region: TRegion);
  Var I: Integer;
  Begin
    For I := 0 To High(Region.Polygons) Do Region.Polygons[I] := PAlias[Region.Polygons[I]];
    If Region.Left  <> Nil Then RemapPolygons(Region.Left);
    If Region.Right <> Nil Then RemapPolygons(Region.Right);
  End; // RemapPolygons

Begin
  // Create a new mesh object

  MO1 := TMeshObject.Create;

  // Move the polygons to the new mesh

  Y := 0;
  SetLength(VAlias,Root.Mesh.Vertices.Count);
  For I := 0 To High(VAlias) Do VAlias[I] := -1;
  MoveWaterPolygons(Root,MO1);

  // Figure out the remapped polygon indices

  Y := 0;
  SetLength(PAlias,Root.Mesh.Polygons.Count);
  For I := 0 To High(PAlias) Do
  Begin
    PAlias[I] := I - Y;
    If Root.Mesh.Polygons.Objects[I] = Nil Then Inc(Y);
  End; // For I

  // The polygons have been moved to the new mesh and Nil has been put in the
  // old mesh in their places.  Delete the Nil references.

  I := 0;
  While I < Root.Mesh.Polygons.Count Do
  Begin
    If Root.Mesh.Polygons.Objects[I] = Nil
     Then Root.Mesh.Polygons.Delete(I)
     Else Inc(I);
  End; // While   

  // Remap the polygon indices in the remaining regions

  RemapPolygons(Root);

  // Find out how many vertices we can delete from the source mesh

  For I := 0 To High(VAlias) Do VAlias[I] := 0;
  For I := 0 To Root.Mesh.Polygons.Count - 1 Do
  Begin
    P := TPolygon(Root.Mesh.Polygons.Objects[I]);
    For X := 0 To High(P.Vertices) Do Inc(VAlias[P.Vertices[X]]);
  End; // For I

  // First deallocate those vertices and put in Nil as a placeholder, just like
  // we did for the polygons

  For I := 0 To Root.Mesh.Vertices.Count - 1 Do
  Begin
    If VAlias[I] = 0 Then
    Begin
      Root.Mesh.Vertices.Objects[I].Free;
      Root.Mesh.Vertices.Objects[I] := Nil;
      Root.Mesh.Normals.Objects[I].Free;
      Root.Mesh.Normals.Objects[I] := Nil;
    End;
  End; // For I

  // Now get rid of the empty entries from the mesh

  I := 0;
  While I < Root.Mesh.Vertices.Count Do
  Begin
    If Root.Mesh.Vertices.Objects[I] = Nil Then Root.Mesh.Vertices.Delete(I) Else Inc(I);
  End; // While
  I := 0;
  While I < Root.Mesh.Normals.Count Do
  Begin
    If Root.Mesh.Normals.Objects[I] = Nil Then Root.Mesh.Normals.Delete(I) Else Inc(I);
  End; // While

  // Calculate the new vertex indixes

  X := 0;
  For I := 0 To High(VAlias) Do
  Begin
    If VAlias[I] > 0 Then VAlias[I] := I - X Else Inc(X);
  End; // For I

  // Point the polygons to the new indices

  For I := 0 To Root.Mesh.Polygons.Count - 1 Do
  Begin
    P := TPolygon(Root.Mesh.Polygons.Objects[I]);
    For X := 0 To High(P.Vertices) Do P.Vertices[X] := VAlias[P.Vertices[X]];
  End; // For I

  // Cleanup

  SetLength(VAlias,0);
  SetLength(PAlias,0);
  Result := MO1;
End; // TTree.SplitOffMesh

Function TTree.GetMaxHeightAt(X,Y: Single; HighestPoint,PV1,PV2,PV3: T3DPoint; MinZ,MaxZ: Single): TPolygon;
Var
  I,J       : Integer;
  Intersect : T3DPoint;
  Regions   : Array Of TRegion;
  P         : TPolygon;
  V1,V2,V3  : T3DPoint;
  P1,P2     : T3DPoint;
  Z         : Single;
  Region    : TRegion;
  Norm      : Single;

  Procedure CheckBranch(X,Y: Single; Region: TRegion);
  Var D: Single;
  Begin
    If Region <> Nil Then
    Begin
      If (Region.Left <> Nil) Or (Region.Right <> Nil) Then
      Begin
        // We're not on a leaf node yet

        If Region.SplitNorm.Z <> 0 Then
        Begin
          // The plane normal has a Z component, which means we can potentially fall through both nodes

          CheckBranch(X,Y,Region.Left);
          CheckBranch(X,Y,Region.Right);
        End
        Else
        Begin
          // The plane normal is perpendicular to the Z axis.  See which side of the plane we're on.

          D := Region.SplitNorm.X * X + Region.SplitNorm.Y * Y + Region.SplitDist;
          If D >= 0
           Then CheckBranch(X,Y,Region.Right)
           Else CheckBranch(X,Y,Region.Left);
        End;
      End
      Else
      Begin
        // If we got to a leaf node then this region is one we'll fall through

        SetLength(Regions,High(Regions) + 2);
        Regions[High(Regions)] := Region;
      End;
    End;
  End; // CheckBranch

Begin
  // Set the default value

  HighestPoint.Z := 0;
  Result         := Nil;

  // Allocate objects

  Intersect := T3DPoint.Create;
  P1        := T3DPoint.Create;
  P2        := T3DPoint.Create;

  // Build a list of all regions under this point

  SetLength(Regions,0);
  CheckBranch(X,Y,Root);

  // Scan all of the regions we found, looking for the highest top-facing polygon

{
  Z  := -999999; // Show be low enough
  P1.Copy(X,Y,999999);
  P2.Copy(X,Y,-999999);
}
  Z := MinZ - 1;
  P1.Copy(X,Y,MaxZ + 1);
  P2.Copy(X,Y,MinZ - 1);

  For I := 0 To High(Regions) Do
  Begin
    Region := Regions[I];

    // Scan all of the polygons in the region

    For J := 0 To High(Region.Polygons) Do
    Begin
      P := TPolygon(Root.Mesh.Polygons.Objects[Region.Polygons[J]]);

      // Make sure the polygon is solid (ignore transparent and semitransparent polygons)

      If P.TextureState = tsSolid Then
      Begin
        // Make sure the polygon faces up by checking the vertex normals

        V1   := T3DPoint(Root.Mesh.Normals.Objects[P.Vertices[0]]);
        V2   := T3DPoint(Root.Mesh.Normals.Objects[P.Vertices[1]]);
        V3   := T3DPoint(Root.Mesh.Normals.Objects[P.Vertices[2]]);
        Norm := V1.Z + V2.Z + V3.Z;
        If Norm > 0 Then
        Begin
          // Get the polygon vertices and find the intersection point, if any

          V1 := T3DPoint(Root.Mesh.Vertices.Objects[P.Vertices[0]]);
          V2 := T3DPoint(Root.Mesh.Vertices.Objects[P.Vertices[1]]);
          V3 := T3DPoint(Root.Mesh.Vertices.Objects[P.Vertices[2]]);

          // First make sure our point is capable of intersecting the polygon and
          // then check to see that it indeed does intersect it.  We want to check it in
          // this order because the intersection check is expensive in terms of CPU cycles.

          If (X >= Min(Min(V1.X,V2.X),V3.X)) And
             (X <= Max(Max(V1.X,V2.X),V3.X)) And
             (Y >= Min(Min(V1.Y,V2.Y),V3.Y)) And
             (Y <= Max(Max(V1.Y,V2.Y),V3.Y)) And
             LineFacet(P1,P2,V1,V2,V3,Intersect) Then
          Begin
            // If the intersection point is higher than what we already have, save it as well as
            // references to the polygon and its vertices

            If Intersect.Z > Z Then
            Begin
              Z := Intersect.Z;
              HighestPoint.Copy(Intersect);
              PV1.Copy(V1);
              PV2.Copy(V2);
              PV3.Copy(V3);
              Result := P;
            End;
          End;
        End;
      End;
    End; // For J
  End; // For I

  // Cleanup

  SetLength(Regions,0);
  Intersect.Free;
  P1.Free;
  P2.Free;
End; // TTree.GetMaxHeightAt

// ------------------------------
// TRegion
// ------------------------------

Constructor TRegion.Create(AMesh: TMeshObject; CreateBoundMesh: Boolean);
Var
  I : Integer;
  V : T3DPoint;

Begin
  Mesh            := AMesh;
  Flag            := False;
  Tag             := 0;
  Attribute.Attr  := raNone;
  Attribute.Value := 0;
  Left            := Nil;
  Right           := Nil;
  Parent          := Nil;
  SplitNorm       := T3DPoint.Create;
  SplitDist       := 0;
  BoundMesh       := Nil;
  MinPt           := T3DPoint.Create;
  MaxPt           := T3DPoint.Create;
  Sphere          := TSphere.Create;
  SetLength(Polygons,0);

  If CreateBoundMesh Then
  Begin
    SetLength(Polygons,Mesh.Polygons.Count);
    For I := 0 To High(Polygons) Do Polygons[I] := I;

    // Initialize boundary points

    MinPt.Copy(9999999,9999999,9999999);
    MaxPt.Copy(-9999999,-9999999,-9999999);

    // Determine the zone bounds

    For I := 0 To Mesh.Vertices.Count - 1 Do
    Begin
      V := T3DPoint(Mesh.Vertices.Objects[I]);
      If V.X < MinPt.X Then MinPt.X := V.X;
      If V.Y < MinPt.Y Then MinPt.Y := V.Y;
      If V.Z < MinPt.Z Then MinPt.Z := V.Z;
      If V.X > MaxPt.X Then MaxPt.X := V.X;
      If V.Y > MaxPt.Y Then MaxPt.Y := V.Y;
      If V.Z > MaxPt.Z Then MaxPt.Z := V.Z;
    End; // For I

    // Make sure we have a bounding mesh

    BoundMesh := TMeshObject.Create;
    BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MinPt.Y,MinPt.Z));
    BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MinPt.Y,MaxPt.Z));
    BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MinPt.Y,MaxPt.Z));
    BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MinPt.Y,MinPt.Z));
    BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MaxPt.Y,MinPt.Z));
    BoundMesh.Vertices.AddObject('',T3DPoint.Create(MinPt.X,MaxPt.Y,MaxPt.Z));
    BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MaxPt.Y,MaxPt.Z));
    BoundMesh.Vertices.AddObject('',T3DPoint.Create(MaxPt.X,MaxPt.Y,MinPt.Z));
    BoundMesh.Polygons.AddObject('',TPolygon.Create([0,1,2,3],'')); // Front
    BoundMesh.Polygons.AddObject('',TPolygon.Create([7,6,5,4],'')); // Back
    BoundMesh.Polygons.AddObject('',TPolygon.Create([4,5,1,0],'')); // Left
    BoundMesh.Polygons.AddObject('',TPolygon.Create([3,2,6,7],'')); // Right
    BoundMesh.Polygons.AddObject('',TPolygon.Create([1,5,6,2],'')); // Top
    BoundMesh.Polygons.AddObject('',TPolygon.Create([3,7,4,0],'')); // Bottom
    CalcBounds;
    BoundMesh.CalcNormals;
  End;
End; // TRegion.Create

Constructor TRegion.Create(Region: TRegion);
Begin
  Mesh      := Region.Mesh;
  Flag      := False;
  Tag       := 0;
  Attribute := Region.Attribute;
  Left      := Nil;
  Right     := Nil;
  Parent    := Region;
  SplitNorm := T3DPoint.Create;
  SplitDist := 0;
  BoundMesh := Nil;
  MinPt     := T3DPoint.Create(Region.MinPt);
  MaxPt     := T3DPoint.Create(Region.MaxPt);
  Sphere    := TSphere.Create(Region.Sphere);
  SetLength(Polygons,0);
End; // TRegion.Create

Destructor TRegion.Destroy;
Begin
  SplitNorm.Free;
  SetLength(Polygons,0);
  Left.Free;
  Right.Free;
  BoundMesh.Free;
  MinPt.Free;
  MaxPt.Free;
  Sphere.Free;
End; // TRegion.Destroy

Procedure TRegion.GetNearRegions(NearList: TStringList; Loc: T3DPoint; Dist: Single);
Const FudgeScale = 1.1;
Begin
  If (High(Polygons) >= 0) And (Sphere.Center.DistanceFrom(Loc) < Dist + FudgeScale * Sphere.Radius) Then NearList.AddObject('',Self);
  If Left  <> Nil Then Left.GetNearRegions(NearList,Loc,Dist);
  If Right <> Nil Then Right.GetNearRegions(NearList,Loc,Dist);
End; // TRegion.GetNearRegions

Procedure TRegion.ShadePolygonsByLights(LightTreeHash: TLightTreeHash);
Begin
  Mesh.ShadePolygonsByLights(LightTreeHash,@Polygons,Sphere.Center,Sphere.Radius);
  If Left  <> Nil Then Left.ShadePolygonsByLights(LightTreeHash);
  If Right <> Nil Then Right.ShadePolygonsByLights(LightTreeHash);
End; // TRegion.ShadePolygonsByLights

Function TRegion.GetOctreeChild(XLeft,YLeft,ZLeft: Boolean): TRegion;
Var R,R1: TRegion;
Begin
  If XLeft Then R := Left Else R := Right;
  If R <> Nil Then
  Begin
    If YLeft Then R1 := R.Left Else R1 := R.Right;
    If R1 <> Nil Then
    Begin
      R := R1;
      If ZLeft Then R1 := R.Left Else R1 := R.Right;
      If R1 <> Nil Then R := R1;
    End;
  End;
  Result := R;
End; // TRegion.GetOctreeChild

Procedure TRegion.AddToMesh(MO: TMeshObject; FlagValue,CheckFlag: Boolean);
Var I: Integer;
Begin
  If (Left = Nil) And (Right = Nil) Then
  Begin
    If (Flag = FlagValue) Or Not CheckFlag Then
    Begin
      For I := 0 To High(Polygons) Do
       MO.AddCopyOfPolygon(TPolygon(Mesh.Polygons.Objects[Polygons[I]]),Mesh);
    End;
  End
  Else
  Begin
    If Left  <> Nil Then Left.AddToMesh(MO,FlagValue,CheckFlag);
    If Right <> Nil Then Right.AddToMesh(MO,FlagValue,CheckFlag);
  End;
End; // TRegion.AddToMesh

Procedure TRegion.Split(MaxPolysPerGrid,MaxVertsPerGrid: Integer);
Var
  MinPt    : T3DPoint;
  MaxPt    : T3DPoint;
  Center   : T3DPoint;
  Normal   : T3DPoint;

Begin
  If (Left = Nil) And (Right = Nil) And
     ((High(Polygons) > (MaxPolysPerGrid - 1)) Or
      ((High(Polygons) + 1) * 3 > MaxVertsPerGrid)) Then
  Begin
    MinPt  := T3DPoint.Create;
    MaxPt  := T3DPoint.Create;

    GetAbsoluteBounds(MinPt,MaxPt);
    
    Center := T3DPoint.Create(MinPt);
    Center.Add(MaxPt);
    Center.Divide(2);

    Normal := T3DPoint.Create(1,0,0);
    SplitAlongPlane(Normal,-Center.X,False,False);
    Normal.Copy(0,1,0);
    SplitAlongPlane(Normal,-Center.Y,False,False);
    Normal.Copy(0,0,1);
    SplitAlongPlane(Normal,-Center.Z,False,False);

    MinPt.Free;
    MaxPt.Free;
    Center.Free;
    Normal.Free;
  End;
  If Left  <> Nil Then Left.Split(MaxPolysPerGrid,MaxVertsPerGrid);
  If Right <> Nil Then Right.Split(MaxPolysPerGrid,MaxVertsPerGrid);
End; // TRegion.Split

Procedure TRegion.CalcBounds;
Begin
  BoundMesh.GetBounds(MinPt,MaxPt);
  Sphere.Center.Copy(MaxPt);
  Sphere.Center.Add(MinPt);
  Sphere.Center.Multiply(0.5);
  Sphere.Radius := Sphere.Center.DistanceFrom(MinPt);
End; // TRegion.CalcBounds

Function TRegion.GetMaxPolyCount(Var Count: Integer): Integer;
Begin
  If (Left = Nil) And (Right = Nil) Then
  Begin
    If (High(Polygons) + 1) > Count Then Count := High(Polygons) + 1;
    Result := Count;
  End
  Else
  Begin
    Result := 0;
    If Left  <> Nil Then Result := Left.GetMaxPolyCount(Count);
    If Right <> Nil Then Result := Right.GetMaxPolyCount(Count);
  End;
End; // TRegion.GetMaxPolyCount

Function TRegion.CountNodes: Integer;
// Avoid using local varibales to save stack space
Begin
  If Left <> Nil Then
  Begin
    If Right <> Nil
     Then Result := Left.CountNodes + Right.CountNodes + 1
     Else Result := Left.CountNodes + 1;
  End
  Else If Right <> Nil
   Then Result := Right.CountNodes + 1
   Else Result := 1;
End; // TRegion.CountNodes

Function TRegion.CountLeafNodes: Integer;
// Avoid using local varibales to save stack space
Begin
  If Left <> Nil Then
  Begin
    If Right <> Nil
     Then Result := Left.CountLeafNodes + Right.CountLeafNodes
     Else Result := Left.CountLeafNodes;
  End
  Else If Right <> Nil
   Then Result := Right.CountLeafNodes
   Else Result := 1;
End; // TRegion.CountLeafNodes

Function TRegion.CountLeafNodesWithAttrs(Attrs: TRegionAttributes): Integer;
// Avoid using local varibales to save stack space
Begin
  If Left <> Nil Then
  Begin
    If Right <> Nil
     Then Result := Left.CountLeafNodesWithAttrs(Attrs) + Right.CountLeafNodesWithAttrs(Attrs)
     Else Result := Left.CountLeafNodesWithAttrs(Attrs);
  End
  Else If Right <> Nil Then Result := Right.CountLeafNodesWithAttrs(Attrs)
  Else If Attribute.Attr In Attrs
   Then Result := 1
   Else Result := 0;
End; // TRegion.CountLeafNodesWithAttrs

Procedure TRegion.GetTagList(Var TagList: TTagListArray);
Begin
  If High(Polygons) >= 0 Then
  Begin
    SetLength(TagList,High(TagList) + 2);
    TagList[High(TagList)] := Tag;
  End
  Else
  Begin
    If Left  <> Nil Then Left.GetTagList(TagList);
    If Right <> Nil Then Right.GetTagList(TagList);
  End;
End; // TRegion.GetTagList

Procedure TRegion.TagLeafNodes(Var TagNumber: Integer);
Begin
  If (Left = Nil) And (Right = Nil) Then
  Begin
    Tag := TagNumber;
    Inc(TagNumber);
  End
  Else
  Begin
    If Left  <> Nil Then Left.TagLeafNodes(TagNumber);
    If Right <> Nil Then Right.TagLeafNodes(TagNumber);
  End;
End; // TRegion.TagLeafNodes

Procedure TRegion.SetFlags;
Begin
  Flag := True;
  If Left  <> Nil Then Left.SetFlags;
  If Right <> Nil Then Right.SetFlags;
End; // TRegion.SetFlags

Procedure TRegion.ClearFlags;
Begin
  Flag := False;
  If Left  <> Nil Then Left.ClearFlags;
  If Right <> Nil Then Right.ClearFlags;
End; // TRegion.ClearFlags

Procedure TRegion.SetFlaggedToAttribute(Attr: TRegionAttribute; Value: Integer);
Begin
  If Flag Then
  Begin
    Attribute.Attr  := Attr;
    Attribute.Value := Value;
  End;
  If Left  <> Nil Then Left.SetFlaggedToAttribute(Attr,Value);
  If Right <> Nil Then Right.SetFlaggedToAttribute(Attr,Value);
End; // TRegion.SetFlaggedToAttribute

Procedure TRegion.SetUnflaggedToAttribute(Attr: TRegionAttribute; Value: Integer);
Begin
  If Not Flag Then
  Begin
    Attribute.Attr  := Attr;
    Attribute.Value := Value;
  End;
  If Left  <> Nil Then Left.SetFlaggedToAttribute(Attr,Value);
  If Right <> Nil Then Right.SetFlaggedToAttribute(Attr,Value);
End; // TRegion.SetUnflaggedToAttribute

Procedure TRegion.SetFlaggedToTexture(Texture: String);
Var
  I : Integer;
  P : TPolygon;

Begin
  If Flag Then
  Begin
    For I := 0 To High(Polygons) Do
    Begin
      P := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
      P.Texture := Texture;
    End; // For I
  End;
  If Left  <> Nil Then Left.SetFlaggedToTexture(Texture);
  If Right <> Nil Then Right.SetFlaggedToTexture(Texture);
End; // TRegion.SetFlaggedToTexture

Procedure TRegion.SetFlaggedFromTextureToTexture(OldTexture,NewTexture: String);
Var
  I : Integer;
  P : TPolygon;

Begin
  OldTexture := UpperCase(OldTexture);
  If Flag Then
  Begin
    For I := 0 To High(Polygons) Do
    Begin
      P := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
      If UpperCase(P.Texture) = OldTexture Then P.Texture := NewTexture;
    End; // For I
  End;
  If Left  <> Nil Then Left.SetFlaggedFromTextureToTexture(OldTexture,NewTexture);
  If Right <> Nil Then Right.SetFlaggedFromTextureToTexture(OldTexture,NewTexture);
End; // TRegion.SetFlaggedFromTextureToTexture

Procedure TRegion.SetUnflaggedToTexture(Texture: String);
Var
  I : Integer;
  P : TPolygon;

Begin
  If Flag Then
  Begin
    For I := 0 To High(Polygons) Do
    Begin
      P := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
      P.Texture := Texture;
    End; // For I
  End;
  If Left  <> Nil Then Left.SetUnflaggedToTexture(Texture);
  If Right <> Nil Then Right.SetUnflaggedToTexture(Texture);
End; // TRegion.SetUnflaggedToTexture

Procedure TRegion.SetUnflaggedFromTextureToTexture(OldTexture,NewTexture: String);
Var
  I : Integer;
  P : TPolygon;

Begin
  OldTexture := UpperCase(OldTexture);
  If Flag Then
  Begin
    For I := 0 To High(Polygons) Do
    Begin
      P := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
      If UpperCase(P.Texture) = OldTexture Then P.Texture := NewTexture;
    End; // For I
  End;
  If Left  <> Nil Then Left.SetUnflaggedFromTextureToTexture(OldTexture,NewTexture);
  If Right <> Nil Then Right.SetUnflaggedFromTextureToTexture(OldTexture,NewTexture);
End; // TRegion.SetUnflaggedFromTextureToTexture

Procedure TRegion.SetFlaggedToColor(C: TColor);
Var
  I,J : Integer;
  P   : TPolygon;

Begin
  If Flag Then
  Begin
    For I := 0 To High(Polygons) Do
    Begin
      P          := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
      P.HasColor := True;
      SetLength(P.Colors,High(P.Vertices) + 1);
      For J := 0 To High(P.Colors) Do P.Colors[J] := C;
    End; // For I
  End;
  If Left  <> Nil Then Left.SetFlaggedToColor(C);
  If Right <> Nil Then Right.SetFlaggedToColor(C);
End; // TRegion.SetFlaggedToColor

Procedure TRegion.GetRegionsInVolume(List: TStringList; VMinPt,VMaxPt: T3DPoint; LeavesOnly: Boolean);
Begin
  If (BoundMesh <> Nil) And (BoundMesh.Vertices.Count > 0) Then
  Begin
    // If this region intersects the given volume, either add it to the list
    // or process its children, if any

    If (MinPt.X < VMaxPt.X) And (MaxPt.X > VMinPt.X) And
       (MinPt.Y < VMaxPt.Y) And (MaxPt.Y > VMinPt.Y) And
       (MinPt.Z < VMaxPt.Z) And (MaxPt.Z > VMinPt.Z) Then
    Begin
      If (Left <> Nil) And (Right <> Nil) Then
      Begin
        Left.GetRegionsInVolume(List,VMinPt,VMaxPt,LeavesOnly);
        Right.GetRegionsInVolume(List,VMinPt,VMaxPt,LeavesOnly);
      End;
      If (Left = Nil) Or (Right = Nil) Or Not LeavesOnly Then List.AddObject('',Self);
    End;
  End;
End; // TRegion.GetRegionsInVolume

Procedure TRegion.GetTagsOfRegionsInVolume(Var TagList: TTagListArray; Var TagCount: Integer; VMinPt,VMaxPt: T3DPoint);
Begin
  If (BoundMesh <> Nil) And (BoundMesh.Vertices.Count > 0) Then
  Begin
    // If this region intersects the given volume, either add it to the list
    // or process its children, if any

    If (MinPt.X < VMaxPt.X) And (MaxPt.X > VMinPt.X) And
       (MinPt.Y < VMaxPt.Y) And (MaxPt.Y > VMinPt.Y) And
       (MinPt.Z < VMaxPt.Z) And (MaxPt.Z > VMinPt.Z) Then
    Begin
      If (Left <> Nil) And (Right <> Nil) Then
      Begin
        Left.GetTagsOfRegionsInVolume(TagList,TagCount,VMinPt,VMaxPt);
        Right.GetTagsOfRegionsInVolume(TagList,TagCount,VMinPt,VMaxPt);
      End
      Else
      Begin
        If High(TagList) < TagCount Then SetLength(TagList,TagCount + 1);
        TagList[TagCount] := Tag;
        Inc(TagCount);
      End;
    End;
  End;
End; // TRegion.GetTagsOfRegionsInVolume

Procedure TRegion.GetAbsoluteBounds(MinPt,MaxPt: T3DPoint);
// This works as long as we don't have unused vertices
Var
  V         : T3DPoint;
  P         : TPolygon;
  I,J       : Integer;
  First     : Boolean;
  XMin,XMax : Single;
  YMin,YMax : Single;
  ZMin,ZMax : Single;

Begin
  // Calculate the center of the volume

  XMin  := 0;
  YMin  := 0;
  ZMin  := 0;
  XMax  := 0;
  YMax  := 0;
  ZMax  := 0;
  First := True;
  V     := T3DPoint.Create;
  For I := 0 To High(Polygons) Do
  Begin
    P := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
    For J := 0 To High(P.Vertices) Do
    Begin
      V.Copy(T3DPoint(Mesh.Vertices.Objects[P.Vertices[J]]));
      Mesh.MakeAbsolute(V);
      If First Then
      Begin
        XMin  := V.X;
        YMin  := V.Y;
        ZMin  := V.Z;
        XMax  := XMin;
        YMax  := YMin;
        ZMax  := ZMin;
        First := False;
      End
      Else
      Begin
        If V.X < XMin Then XMin := V.X;
        If V.Y < YMin Then YMin := V.Y;
        If V.Z < ZMin Then ZMin := V.Z;
        If V.X > XMax Then XMax := V.X;
        If V.Y > YMax Then YMax := V.Y;
        If V.Z > ZMax Then ZMax := V.Z;
      End;
    End; // For J
  End; // For I
  V.Free;
  MinPt.Copy(XMin,YMin,ZMin);
  MaxPt.Copy(XMax,YMax,ZMax);
End; // TRegion.GetAbsoluteBounds

Procedure TRegion.GetCenterOfVolumeAndMaxDistance(Center: T3DPoint; Var Dist: Single);
// This works as long as we don't have unused vertices
Var
  V,V1      : T3DPoint;
  P         : TPolygon;
  I,J       : Integer;
  First     : Boolean;
  D         : Single;
  XMin,XMax : Single;
  YMin,YMax : Single;
  ZMin,ZMax : Single;

Begin
  // Calculate the center of the volume

  XMin  := 0;
  YMin  := 0;
  ZMin  := 0;
  XMax  := 0;
  YMax  := 0;
  ZMax  := 0;
  First := True;
  V     := T3DPoint.Create;
  For I := 0 To High(Polygons) Do
  Begin
    P := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
    For J := 0 To High(P.Vertices) Do
    Begin
      V.Copy(T3DPoint(Mesh.Vertices.Objects[P.Vertices[J]]));
      Mesh.MakeAbsolute(V);
      If First Then
      Begin
        XMin  := V.X;
        YMin  := V.Y;
        ZMin  := V.Z;
        XMax  := XMin;
        YMax  := YMin;
        ZMax  := ZMin;
        First := False;
      End
      Else
      Begin
        If V.X < XMin Then XMin := V.X;
        If V.Y < YMin Then YMin := V.Y;
        If V.Z < ZMin Then ZMin := V.Z;
        If V.X > XMax Then XMax := V.X;
        If V.Y > YMax Then YMax := V.Y;
        If V.Z > ZMax Then ZMax := V.Z;
      End;
    End; // For J
  End; // For I
  V.Free;
  Center.X := (XMin + XMax) / 2;
  Center.Y := (YMin + YMax) / 2;
  Center.Z := (ZMin + ZMax) / 2;

  // Get the longest distance from the center

  Dist := 0;
  V1   := T3DPoint.Create;
  For I := 0 To High(Polygons) Do
  Begin
    P := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
    For J := 0 To High(P.Vertices) Do
    Begin
      V1.Copy(T3DPoint(Mesh.Vertices.Objects[P.Vertices[J]]));
      Mesh.MakeAbsolute(V1);
      V1.Subtract(Center);
      D := V1.GetLength;
      If D > Dist Then Dist := D;
    End; // For J
  End; // For I
  V1.Free;
End; // TRegion.GetCenterOfVolumeAndMaxDistance

Function TRegion.GetMaxDistanceFromPoint(V: T3DPoint): Single;
Var
  V1   : T3DPoint;
  P    : TPolygon;
  I,J  : Integer;
  D,D1 : Single;

Begin
  D := 0;
  If High(Polygons) >= 0 Then
  Begin
    V1 := T3DPoint.Create;
    For I := 0 To High(Polygons) Do
    Begin
      P := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
      For J := 0 To High(P.Vertices) Do
      Begin
        V1.Copy(T3DPoint(Mesh.Vertices.Objects[P.Vertices[J]]));
        V1.Subtract(V);
        D1 := V1.GetLength;
        If D1 > D Then D := D1;
      End; // For J
    End; // For I
    V1.Free;
  End;
  Result := D;
End; // TRegion.GetMaxDistanceFromPoint

Procedure TRegion.ConvertToTriangles;
Var
  I,J,K,L : Integer;
  P       : TPolygon;
  P1      : TPolygon;
  PV      : Integer;

Begin
  If High(Polygons) >= 0 Then
  Begin
    // For now at least, assume all polygons are convex

    // First pass: find out how many polygons we'll be adding

    J := 0;
    For I := 0 To High(Polygons) Do
    Begin
      P := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
      K := High(P.Vertices);
      If K > 2 Then Inc(J,K - 2);
    End; // For I

    // Second pass: create the new polygons

    K := High(Polygons) + 1;
    SetLength(Polygons,K + J);
    L := K - 1;
    For I := 0 To L Do
    Begin
      P := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);

      // If this polygon has more than three vertices, break it up

      PV := High(P.Vertices);
      If PV > 2 Then
      Begin
        For J := 0 To PV - 3 Do
        Begin
          // Create a new polygon

          P1 := TPolygon.CreateBasic(P);
          SetLength(P1.Vertices,3);
          If P1.HasTexCoords Then
          Begin
            SetLength(P1.TX,3);
            SetLength(P1.TZ,3);
          End;
          If P1.HasColor Then
          Begin
            SetLength(P1.Colors,3);
            P1.Colors[0] := P.Colors[0];
            P1.Colors[1] := P.Colors[J + 2];
            P1.Colors[2] := P.Colors[J + 3];
          End;

          // Copy the right vertices and texture coordinates

          P1.Vertices[0] := P.Vertices[0];
          P1.Vertices[1] := P.Vertices[J + 2];
          P1.Vertices[2] := P.Vertices[J + 3];
          If P1.HasTexCoords Then
          Begin
            P1.TX[0] := P.TX[0];
            P1.TX[1] := P.TX[J + 2];
            P1.TX[2] := P.TX[J + 3];
            P1.TZ[0] := P.TZ[0];
            P1.TZ[1] := P.TZ[J + 2];
            P1.TZ[2] := P.TZ[J + 3];
          End;

          // Add the new polygon

          Mesh.Polygons.AddObject('',P1);
          Polygons[K] := Mesh.Polygons.Count - 1;
          Inc(K);
        End; // For J

        // Shorten the original polygon

        SetLength(P.Vertices,3);
        If P.HasTexCoords Then
        Begin
          SetLength(P.TX,3);
          SetLength(P.TZ,3);
        End;
        If P.HasColor Then SetLength(P.Colors,3);
      End;
    End; // For I
  End
  Else
  Begin
    If Left  <> Nil Then Left.ConvertToTriangles;
    If Right <> Nil Then Right.ConvertToTriangles;
  End;
End; // TRegion.ConvertToTriangles

Procedure TRegion.SplitAlongPlane(Normal: T3DPoint; Dist: Single; FlaggedOnly,ForceSplit: Boolean);
// Have to have some fuzziness or roundoff error will give us infinite loops.  This design
// should ensure that roundoff error will still have split polygons exactly mate up.
Const MinDist = 0.001;
Var
  I,J,K,L   : Integer;
  J1,K1     : Integer;
  P         : TPolygon;
  D0,D1,D2  : Single;
  Found     : Boolean;
  VJ,VJ1    : T3DPoint;
  VK,VK1    : T3DPoint;
  I1,I2     : T3DPoint;
  Moving    : Boolean;
  TX1,TZ1   : Single;
  TX2,TZ2   : Single;
  P1,P2     : TPolygon;
  MoveCount : Integer;
  C1,C2     : TColor;
  OldFlag   : Boolean;

  Function CountNodes(Start,Finish,Size: Integer): Integer;
  Begin
    While Finish < Start Do Inc(Finish,Size);
    Result := (Finish - Start) + 1;
  End; // CountNodes

Begin
  If Flag Or Not FlaggedOnly Then
  Begin
    If (High(Polygons) >= 0) And (Flag Or Not FlaggedOnly) Then
    Begin
      // First perform a test split of the bounding mesh.  If it turns out that the bounding mesh,
      // after being "split", goes entirely to one leaf region then we don't want to do anything.

      Left            := TRegion.Create(Self);
      Right           := TRegion.Create(Self);
      Left.BoundMesh  := TMeshObject.Create(BoundMesh);
      Right.BoundMesh := TMeshObject.Create(BoundMesh);
      Left.BoundMesh.RemoveAlongPlane(Normal,Dist,False);
      Right.BoundMesh.RemoveAlongPlane(Normal,Dist,True);
      I := Min(Left.BoundMesh.Polygons.Count,Right.BoundMesh.Polygons.Count);
      J := Left.BoundMesh.Polygons.Count;
      OldFlag := Flag;
      If Right.BoundMesh.Polygons.Count = 0 Then Flag := False;
      Left.Free;
      Right.Free;
      Left  := Nil;
      Right := Nil;
      If (I = 0) And ForceSplit Then
      Begin
        SplitNorm.Copy(Normal);
        SplitDist       := Dist;
        Left            := TRegion.Create(Self);
        Right           := TRegion.Create(Self);
        Left.BoundMesh  := TMeshObject.Create(BoundMesh);
        Right.BoundMesh := TMeshObject.Create(BoundMesh);
        Left.BoundMesh.RemoveAlongPlane(Normal,Dist,False);
        Right.BoundMesh.RemoveAlongPlane(Normal,Dist,True);
        Right.Flag      := OldFlag;
        If J = 0 Then
        Begin
          // Move everything to the right node

          SetLength(Left.Polygons,0);
          SetLength(Right.Polygons,High(Polygons) + 1);
          For J := 0 To High(Polygons) Do Right.Polygons[J] := Polygons[J];
          SetLength(Polygons,0);
        End
        Else
        Begin
          // Move everything to the left node

          SetLength(Right.Polygons,0);
          SetLength(Left.Polygons,High(Polygons) + 1);
          For J := 0 To High(Polygons) Do Left.Polygons[J] := Polygons[J];
          SetLength(Polygons,0);
        End;
        Left.CalcBounds;
        Right.CalcBounds;
      End
      Else If I > 0 Then
      Begin
        I1        := T3DPoint.Create;
        I2        := T3DPoint.Create;
        I         := 0;
        MoveCount := 0;
        While I <= High(Polygons) Do
        Begin
          If Polygons[I] >= 0 Then
          Begin
            P      := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
            Moving := False;

            // Make sure the polygon has at least three vertices

            If High(P.Vertices) >= 2 Then
            Begin
              // Find the first point that is not on the plane

              D0    := 0;
              J     := 0;
              Found := False;
              While (J <= High(P.Vertices)) And Not Found Do
              Begin
                // Starting with the first point not on the plane, find out which side it's on

                D0 := Normal.Dot(T3DPoint(Mesh.Vertices.Objects[P.Vertices[J]])) + Dist; // Speed
                If Abs(D0) > MinDist
                 Then Found := True
                 Else Inc(J);
              End; // While

              // If we actually didn't find anything then all points are on the plane...we'll leave the
              // polygon in the same region

              If Found Then
              Begin
                // Now look for the first point that crosses the plane

                Inc(J);
                Found := False;
                D1    := 0;
                While (J <= High(P.Vertices)) And Not Found Do
                Begin
                  D1 := Normal.Dot(T3DPoint(Mesh.Vertices.Objects[P.Vertices[J]])) + Dist; // Speed
                  If (Abs(D1) > MinDist) And (Sign(D1) <> Sign(D0)) Then Found := True Else Inc(J);
                End; // While

                // We want to split or move the polygon under two circumstances:
                // 1. It crosses the plane
                // 2. It lies entirely on the "inside" side of the plane

                K := 0;
                If Found Then
                Begin
                  // We've definitely crossed the plane; see where it crosses back or even touches the plane

                  If J < High(P.Vertices) Then
                  Begin
                    K     := J + 1;
                    Found := False;
                    While (K <= High(P.Vertices)) And Not Found Do
                    Begin
                      D2 := Normal.Dot(T3DPoint(Mesh.Vertices.Objects[P.Vertices[K]])) + Dist; // Speed
                      If (Abs(D2) < MinDist) Or (Sign(D2) <> Sign(D1)) Then Found := True Else Inc(K);
                    End; // While
                    If Not Found Then K := High(P.Vertices) Else Dec(K);
                  End
                  Else K := J;

                  // J and K are on one side of the plane.  Get the points on the other side.

                  J1 := J - 1;
                  K1 := (K + 1) Mod (High(P.Vertices) + 1);

                  // Find the two intersection points on the plane

                  VJ  := T3DPoint(Mesh.Vertices.Objects[P.Vertices[J]]);
                  VJ1 := T3DPoint(Mesh.Vertices.Objects[P.Vertices[J1]]);
                  P.GetIntersection(Mesh,VJ,VJ1,J,J1,Normal.Dot(VJ) + Dist,Normal.Dot(VJ1) + Dist,I1,TX1,TZ1,C1);

                  VK  := T3DPoint(Mesh.Vertices.Objects[P.Vertices[K]]);
                  VK1 := T3DPoint(Mesh.Vertices.Objects[P.Vertices[K1]]);
                  P.GetIntersection(Mesh,VK,VK1,K,K1,Normal.Dot(VK) + Dist,Normal.Dot(VK1) + Dist,I2,TX2,TZ2,C2);

                  // If the intersection points are different, insert them in the polygon and
                  // set them as endpoints for the new polygon

                  If Not (I1.Equals(VJ) Or I1.Equals(VJ1)) Then
                  Begin
                    P.InsertVertex(Mesh,I1,J,TX1,TZ1,C1);
                    Inc(K);
                    K1 := (K + 1) Mod (High(P.Vertices) + 1);
                  End
                  Else If I1.Equals(VJ1) Then J := J1;

                  If Not (I2.Equals(VK) Or I2.Equals(VK1)) Then
                  Begin
                    P.InsertVertex(Mesh,I2,K1,TX2,TZ2,C2);
                    If K1 < J Then J := (J + 1) Mod (High(P.Vertices) + 1);
                    K := K1;
                  End
                  Else If I2.Equals(VK1) Then K := K1;

                  // J to K (inclusive) point to a polygon that will be split off.  The region it will
                  // go in depends on D1 (if D1 > 0 then it goes into the "right" one)

                  If D1 < 0 Then
                  Begin
                    J1 := J;
                    J  := K;
                    K  := J1;
                  End;
                  Moving := True;
                End
                Else If D0 > 0 Then
                Begin
                  // The polygon doesn't cross the plane, but it's on the "inside", so
                  // move the entire polygon

                  J      := 0;
                  K      := High(P.Vertices);
                  Moving := True;
                End;
                If Moving Then
                Begin
                  // Split the polgyon.  Points J to K (inclusive) go to the "right" node.

                  If CountNodes(J,K,High(P.Vertices) + 1) = High(P.Vertices) + 1 Then
                  Begin
                    // Just move the whole polygon

                    Inc(MoveCount);
                    Polygons[I] := -Polygons[I] - 1;  // A negative value flags it as to be moved
                    Inc(I);
                  End
                  Else
                  Begin
                    // Make a new polygon

                    P1 := TPolygon.Create(P);
                    J1 := 0; // Counter
                    K1 := (K + 1) Mod (High(P.Vertices) + 1);
                    L  := J; // Save original starting point
                    Repeat
                      P1.Vertices[J1] := P.Vertices[J];
                      If P1.HasTexCoords Then
                      Begin
                        P1.TX[J1] := P.TX[J];
                        P1.TZ[J1] := P.TZ[J];
                      End;
                      If P1.HasColor Then P1.Colors[J1] := P.Colors[J];
                      Inc(J1);
                      J := (J + 1) Mod (High(P.Vertices) + 1);
                    Until J = K1;
                    SetLength(P1.Vertices,J1);
                    If P1.HasTexCoords Then
                    Begin
                      SetLength(P1.TX,J1);
                      SetLength(P1.TZ,J1);
                    End;
                    If P1.HasColor Then SetLength(P1.Colors,J1);
                    Mesh.Polygons.AddObject('',P1);
                    SetLength(Polygons,High(Polygons) + 2);
                    Polygons[High(Polygons)] := -Mesh.Polygons.Count; // A negative value flags it as to be moved
                    Inc(MoveCount);

                    // Now cut down the original one

                    J  := L;
                    J1 := 0; // Counter
                    P2 := TPolygon.Create(P);
                    J  := (J + 1) Mod (High(P.Vertices) + 1);
                    Repeat
                      P.Vertices[J1] := P2.Vertices[K];
                      If P.HasTexCoords Then
                      Begin
                        P.TX[J1] := P2.TX[K];
                        P.TZ[J1] := P2.TZ[K];
                      End;
                      If P.HasColor Then P.Colors[J1] := P2.Colors[K];
                      Inc(J1);
                      K := (K + 1) Mod (High(P.Vertices) + 1);
                    Until K = J;
                    P2.Free;
                    SetLength(P.Vertices,J1);
                    If P.HasTexCoords Then
                    Begin
                      SetLength(P.TX,J1);
                      SetLength(P.TZ,J1);
                    End;
                    If P.HasColor Then SetLength(P.Colors,J1);

                    // We want to reprocess this polygon again, so don't increment I
                  End;
                End
                Else Inc(I);
              End
              Else Inc(I);
            End
            Else Inc(I);
          End
          Else Inc(I);
        End; // While

        // Move the polygons to the appropriate nodes

        If MoveCount > 0 Then
        Begin
          SplitNorm.Copy(Normal);
          SplitDist  := Dist;
          Left       := TRegion.Create(Self);
          Right      := TRegion.Create(Self);
          Right.Flag := Flag;
          SetLength(Left.Polygons,(High(Polygons) + 1) - MoveCount);
          SetLength(Right.Polygons,MoveCount);
          J := 0; // Left counter
          K := 0; // Right counter
          For I := 0 To High(Polygons) Do
          Begin
            If Polygons[I] >= 0 Then
            Begin
              Left.Polygons[J] := Polygons[I];
              Inc(J);
            End
            Else
            Begin
              Right.Polygons[K] := -Polygons[I] - 1;
              Inc(K);
            End;
          End; // For I
          SetLength(Polygons,0);


  {
          // Only do any moves if some of the polygons will go to the left sub-region

          If MoveCount < High(Polygons) + 1 Then
          Begin
            SplitNorm.Copy(Normal);
            SplitDist  := Dist;
            Left       := TRegion.Create(Self);
            Right      := TRegion.Create(Self);
            Right.Flag := Flag;
            SetLength(Left.Polygons,(High(Polygons) + 1) - MoveCount);
            SetLength(Right.Polygons,MoveCount);
            J := 0; // Left counter
            K := 0; // Right counter
            For I := 0 To High(Polygons) Do
            Begin
              If Polygons[I] >= 0 Then
              Begin
                Left.Polygons[J] := Polygons[I];
                Inc(J);
              End
              Else
              Begin
                Right.Polygons[K] := -Polygons[I] - 1;
                Inc(K);
              End;
            End; // For I
            SetLength(Polygons,0);
          End
          Else
          Begin
            // ALL of the polygons are slated to be moved to the right sub-region.  In that case,
            // there isn't even a need to have sub-regions.

            For I := 0 To High(Polygons) Do
            Begin
              If Polygons[I] < 0 Then Polygons[I] := -Polygons[I] - 1;
            End; // For I
          End;
  }
        End
        Else Flag := False;

        // Cleanup

        I1.Free;
        I2.Free;

        // Now do the bounding polygons

        If Left <> Nil Then
        Begin
          Left.BoundMesh  := TMeshObject.Create(BoundMesh);
          Left.BoundMesh.RemoveAlongPlane(Normal,Dist,False);
          Left.CalcBounds;
        End;
        If Right <> Nil Then
        Begin
          Right.BoundMesh := TMeshObject.Create(BoundMesh);
          Right.BoundMesh.RemoveAlongPlane(Normal,Dist,True);
          Right.CalcBounds;
        End;
      End;
    End
    Else
    Begin
      If Left  <> Nil Then Left.SplitAlongPlane(Normal,Dist,FlaggedOnly,ForceSplit);
      If Right <> Nil Then Right.SplitAlongPlane(Normal,Dist,FlaggedOnly,ForceSplit);
    End;
  End;
End; // TRegion.SplitAlongPlane

// ------------------------------
// TLightTreeHash
// ------------------------------

Constructor TLightTreeHash.Create(Zone: TZone; Tree: TTree);
Var
  I           : Integer;
  NearRegions : TStringList;
  Light       : TLightObject;

Begin
  Inherited Create;
  Lights      := Zone.GetAllLightSources;
  RegionLists := TIntegerObjectHash.Create(False,True);
  For I := 0 To Lights.Count - 1 Do
  Begin
    Light       := TLightObject(Lights.Objects[I]);
    NearRegions := TStringList.Create;
    If Tree <> Nil Then Tree.Root.GetNearRegions(NearRegions,Light.Loc,Light.Radius);
    RegionLists.Put(I,NearRegions);
  End; // For I
End; // TLightTreeHash.Create

Destructor  TLightTreeHash.Destroy;
Var I: Integer;
Begin
//  For I := 0 To RegionLists.Count - 1 Do TStringList(RegionLists.Items[I].Value.Ptr).Free;
  RegionLists.Free;
  Lights.Free;
  Inherited;
End; // TLightTreeHash.Destroy

// ------------------------------
// THeading
// ------------------------------

Constructor THeading.Create;
Begin
  Inherited;
  FXAngle := 0;
  FYAngle := 0;
  FZAngle := 0;
End; // THeading.Create

Constructor THeading.Create(H: THeading);
Begin
  Inherited Create;
  FXAngle := H.FXAngle;
  FYAngle := H.FYAngle;
  FZAngle := H.FZAngle;
End; // THeading.Create

Constructor THeading.Create(_XAngle,_YAngle,_ZAngle: Single);
Begin
  Inherited Create;
  FXAngle := _XAngle;
  FYAngle := _YAngle;
  FZAngle := _ZAngle;
End; // THeading.Create

Destructor THeading.Destroy;
Begin
  Inherited;
End; // THeading.Destroy

Procedure THeading.Rotate(P: T3DPoint);
// Counterclockwise rotation
Var
  X,Y,Z    : Single;
  XA,YA,ZA : Single;

Begin
  XA := FXAngle * Pi / 180;
  YA := FYAngle * Pi / 180;
  ZA := FZAngle * Pi / 180;

  X   := P.X * Cos(ZA) - P.Y * Sin(ZA);
  Y   := P.Y * Cos(ZA) + P.X * Sin(ZA);
  P.X := X;
  P.Y := Y;

  Y   := P.Y * Cos(XA) - P.Z * Sin(XA);
  Z   := P.Z * Cos(XA) + P.Y * Sin(XA);
  P.Y := Y;
  P.Z := Z;

  X   := P.X * Cos(YA) - P.Z * Sin(YA);       // Should not have used these...they are for left-handed coordinate systems...
  Z   := P.Z * Cos(YA) + P.X * Sin(YA);
//  X   := P.X * Cos(YA) + P.Z * Sin(YA);     // Should have used these instead :(
//  Z   := P.Z * Cos(YA) - P.X * Sin(YA);
  P.X := X;
  P.Z := Z;
End; // THeading.Rotate

Procedure THeading.NegativeRotate(P: T3DPoint);
// Clockwise rotation
Var
  X,Y,Z    : Single;
  XA,YA,ZA : Single;

Begin
  XA := -FXAngle * Pi / 180;
  YA := -FYAngle * Pi / 180;
  ZA := -FZAngle * Pi / 180;

  X   := P.X * Cos(ZA) - P.Y * Sin(ZA);
  Y   := P.Y * Cos(ZA) + P.X * Sin(ZA);
  P.X := X;
  P.Y := Y;

  Y   := P.Y * Cos(XA) - P.Z * Sin(XA);
  Z   := P.Z * Cos(XA) + P.Y * Sin(XA);
  P.Y := Y;
  P.Z := Z;

  X   := P.X * Cos(YA) - P.Z * Sin(YA);       // Should not have used these...they are for left-handed coordinate systems...
  Z   := P.Z * Cos(YA) + P.X * Sin(YA);
//  X   := P.X * Cos(YA) + P.Z * Sin(YA);     // Should have used these instead :(
//  Z   := P.Z * Cos(YA) - P.X * Sin(YA);
  P.X := X;
  P.Z := Z;
End; // THeading.NegativeRotate

Procedure THeading.Copy(H: THeading);
Begin
  FXAngle := H.XAngle;
  FYAngle := H.YAngle;
  FZAngle := H.ZAngle;
End; // THeading.Copy

Procedure THeading.Copy(XA,YA,ZA: Single);
Begin
  FXAngle := XA;
  FYAngle := YA;
  FZAngle := ZA;
End; // THeading.Copy

Procedure THeading.Standardize;
Begin
  While FXAngle <  0   Do FXAngle := FXAngle + 360;
  While FYAngle <  0   Do FYAngle := FYAngle + 360;
  While FZAngle <  0   Do FZAngle := FZAngle + 360;
  While FXAngle >= 360 Do FXAngle := FXAngle - 360;
  While FYAngle >= 360 Do FYAngle := FYAngle - 360;
  While FZAngle >= 360 Do FZAngle := FZAngle - 360;
End; // THeading.Standardize

Function THeading.Equals(H: THeading): Boolean;
Begin
  Result := (FXAngle = H.FXAngle) And (FYAngle = H.FYAngle) And (FZAngle = H.FZAngle);
End; // THeading.Equals

Procedure THeading.Multiply(H: THeading);
// If this heading is H0, then makes this heading = H0' = H0 * H;
Var
  M1,M2,M3    : Array[1..3,1..3] Of Single;
  XA1,YA1,ZA1 : Single;
  XA2,YA2,ZA2 : Single;
  XA3,YA3,ZA3 : Single;
  Row,Col,I   : Integer;
  RX,RY,RZ    : Single;

Begin
  // Get the angles in radians

  XA1 :=    FXAngle * Pi / 180;
  YA1 :=   -FYAngle * Pi / 180; // Like an idiot I set THeading up for lef-handed coordinate systems...
  ZA1 :=    FZAngle * Pi / 180;
  XA2 :=  H.FXAngle * Pi / 180;
  YA2 := -H.FYAngle * Pi / 180; // Like an idiot I set THeading up for lef-handed coordinate systems...
  ZA2 :=  H.FZAngle * Pi / 180;

  // Build the rotation matrix from this heading (RZ1 * RX1 * RY1)

  M1[1,1] :=  Cos(YA1) * Cos(ZA1) + Sin(XA1) * Sin(YA1) * Sin(ZA1);
  M1[1,2] :=  Cos(XA1) * Sin(ZA1);
  M1[1,3] := -Sin(YA1) * Cos(ZA1) + Sin(XA1) * Cos(YA1) * Sin(ZA1);
  M1[2,1] := -Cos(YA1) * Sin(ZA1) + Sin(XA1) * Sin(YA1) * Cos(ZA1);
  M1[2,2] :=  Cos(XA1) * Cos(ZA1);
  M1[2,3] :=  Sin(YA1) * Sin(ZA1) + Sin(XA1) * Cos(YA1) * Cos(ZA1);
  M1[3,1] :=  Cos(XA1) * Sin(YA1);
  M1[3,2] := -Sin(XA1);
  M1[3,3] :=  Cos(XA1) * Cos(YA1);

  // Build the rotation matrix from the other heading (RZ2 * RX2 * RY2)

  M2[1,1] :=  Cos(YA2) * Cos(ZA2) + Sin(XA2) * Sin(YA2) * Sin(ZA2);
  M2[1,2] :=  Cos(XA2) * Sin(ZA2);
  M2[1,3] := -Sin(YA2) * Cos(ZA2) + Sin(XA2) * Cos(YA2) * Sin(ZA2);
  M2[2,1] := -Cos(YA2) * Sin(ZA2) + Sin(XA2) * Sin(YA2) * Cos(ZA2);
  M2[2,2] :=  Cos(XA2) * Cos(ZA2);
  M2[2,3] :=  Sin(YA2) * Sin(ZA2) + Sin(XA2) * Cos(YA2) * Cos(ZA2);
  M2[3,1] :=  Cos(XA2) * Sin(YA2);
  M2[3,2] := -Sin(XA2);
  M2[3,3] :=  Cos(XA2) * Cos(YA2);

  // Make M3 = M1 * M2

  For Row := 1 To 3 Do
  Begin
    For Col := 1 To 3 Do
    Begin
      M3[Row,Col] := 0;
      For I := 1 To 3 Do
      Begin
        M3[Row,Col] := M3[Row,Col] + M1[Row,I] * M2[I,Col];
      End; // For I
    End; // For Col
  End; // For Row

  // Round everything to four decimal places...roundoff error can really bite us here

  For Row := 1 To 3 Do
  Begin
    For Col := 1 To 3 Do
    Begin
      M3[Row,Col] := Round(M3[Row,Col] * 10000) / 10000;
    End; // For Col
  End; // For Row

  // Build new angles from the new matrix

  If ((M3[3,1] = 0) And (M3[3,3] = 0)) Or
     ((M3[1,2] = 0) And (M3[2,2] = 0)) Then
  Begin
    // cos(x') = 0; use sin(x') to determine the angle

    If M3[3,2] < 0
     Then RX :=  Pi / 2
     Else RX := -Pi / 2;

    // In this case, the rotation reduces to only a rotate around Z and X.  The
    // value for the Z rotation depends on which way we rotated around X.  In
    // either case, we can get the Z rotation from the same two matrix elements.

    RY := 0;
    RZ := ArcTan2(-M3[2,1],M3[1,1]);
  End
  Else
  Begin
    // cos(x') <> 0

    RY := ArcTan2(M3[3,1],M3[3,3]);
    RZ := ArcTan2(M3[1,2],M3[2,2]);
    If M3[3,3] <> 0
     Then RX := ArcTan2(-M3[3,2],M3[3,3] / Cos(RY))
     Else RX := ArcTan2(-M3[3,2],M3[3,1] / Sin(RY));
  End;
  FXAngle :=  RX * 180 / Pi;
  FYAngle := -RY * 180 / Pi; // Like an idiot I set THeading up for lef-handed coordinate systems...
  FZAngle :=  RZ * 180 / Pi;
End; // THeading.Multiply
(*
// ------------------------------
// T3DPoint
// ------------------------------

Constructor T3DPoint.Create;
Begin
  X := 0;
  Y := 0;
  Z := 0;
  FBoneIndex := -1;
End; // T3DPoint.Create

Constructor T3DPoint.Create(AX,AY,AZ: Single);
Begin
  X := AX;
  Y := AY;
  Z := AZ;
  FBoneIndex := -1;
End; // T3DPoint.Create

Constructor T3DPoint.Create(P: T3DPoint);
Begin
  X := P.X;
  Y := P.Y;
  Z := P.Z;
  FBoneIndex := P.FBoneIndex;
End; // T3DPoint.Create

Constructor T3DPoint.Create(P1,P2: T3DPoint);
Begin
  X := P2.X - P1.X;
  Y := P2.Y - P1.Y;
  Z := P2.Z - P1.Z;
  FBoneIndex := -1;
End; // T3DPoint.Create

Function T3DPoint.ToString: String;
Begin
  Result := '(' + FloatToStr(X) + ', ' + FloatToStr(Y) + ', ' + FloatToStr(Z) + ')';
End; // T3DPoint.ToString

Procedure T3DPoint.Normalize;
Var Len,X1,Y1,Z1: Extended;
Begin
  Len := Sqr(X) + Sqr(Y) + Sqr(Z);
  If Len > 0 Then
  Begin
    Len := Sqrt(Len);
    X1  := X / Len;
    Y1  := Y / Len;
    Z1  := Z / Len;
    If Abs(X1) < 1E-8 Then X1 := 0;
    If Abs(Y1) < 1E-8 Then Y1 := 0;
    If Abs(Z1) < 1E-8 Then Z1 := 0;
    If X1 >  1E8 Then X1 :=  1E8;
    If Y1 >  1E8 Then Y1 :=  1E8;
    If Z1 >  1E8 Then Z1 :=  1E8;
    If X1 < -1E8 Then X1 := -1E8;
    If Y1 < -1E8 Then Y1 := -1E8;
    If Z1 < -1E8 Then Z1 := -1E8;
    X := X1;
    Y := Y1;
    Z := Z1;
  End;
End; // T3DPoint.Normalize

Procedure T3DPoint.Add(P: T3DPoint);
Begin
  X := X + P.X;
  Y := Y + P.Y;
  Z := Z + P.Z;
End; // T3DPoint.Add

Procedure T3DPoint.Add(AX,AY,AZ: Single);
Begin
  X := X + AX;
  Y := Y + AY;
  Z := Z + AZ;
End; // T3DPoint.Add

Procedure T3DPoint.Subtract(P: T3DPoint);
Begin
  X := X - P.X;
  Y := Y - P.Y;
  Z := Z - P.Z;
End; // T3DPoint.Subtract

Procedure T3DPoint.Subtract(AX,AY,AZ: Single);
Begin
  X := X - AX;
  Y := Y - AY;
  Z := Z - AZ;
End; // T3DPoint.Subtract

Procedure T3DPoint.Multiply(S: Single);
Begin
  X := X * S;
  Y := Y * S;
  Z := Z * S;
End; // T3DPoint.Multiply

Procedure T3DPoint.Multiply(AX,AY,AZ: Single);
Begin
  X := X * AX;
  Y := Y * AY;
  Z := Z * AZ;
End; // T3DPoint.Multiply

Procedure T3DPoint.Multiply(P: T3DPoint);
Begin
  X := X * P.X;
  Y := Y * P.Y;
  Z := Z * P.Z;
End; // T3DPoint.Multiply

Procedure T3DPoint.Divide(S: Single);
Begin
  If S <> 0 Then
  Begin
    X := X / S;
    Y := Y / S;
    Z := Z / S;
  End;
End; // T3DPoint.Divide

Function T3DPoint.Equals(P: T3DPoint): Boolean;
Begin
  Result := (X = P.X) And (Y = P.Y) And (Z = P.Z);
End; // T3DPoint.Equals

Function T3DPoint.Equals(AX,AY,AZ: Single): Boolean;
Begin
  Result := (X = AX) And (Y = AY) And (Z = AZ);
End; // T3DPoint.Equals

Procedure T3DPoint.Copy(P: T3DPoint);
Begin
  X := P.X;
  Y := P.Y;
  Z := P.Z;
  FBoneIndex := P.FBoneIndex;
End; //  T3DPoint.Copy

Procedure T3DPoint.Copy(AX,AY,AZ: Single);
Begin
  X := AX;
  Y := AY;
  Z := AZ;
End; // T3DPoint.Copy

Function T3DPoint.Dot(P: T3DPoint): Single;
Begin
  Result := X * P.X + Y * P.Y + Z * P.Z;
End; // T3DPoint.Dot

Procedure T3DPoint.Cross(P: T3DPoint);
Var X1,Y1: Single;
Begin
  X1 := Y * P.Z - Z * P.Y;
  Y1 := Z * P.X - X * P.Z;
  Z  := X * P.Y - Y * P.X;
  X  := X1;
  Y  := Y1;
End; // T3DPoint.Cross

Procedure T3DPoint.Rotate(P: T3DPoint);
Begin
End; // T3DPoint.Rotate

Procedure T3DPoint.Exchange(P: T3DPoint);
Var AX,AY,AZ: Single;
Begin
  AX := X;
  AY := Y;
  AZ := Z;
  Copy(P);
  P.X := AX;
  P.Y := AY;
  P.Z := AZ;
End; // T3DPoint.Exchange

Function T3DPoint.GetLength: Single;
Var D: Single;
Begin
  D := Sqr(X) + Sqr(Y) + Sqr(Z);
  If D > 0 Then Result := Sqrt(D) Else Result := 0;
End; // T3DPoint.GetLength

Function T3DPoint.DistanceFrom(P: T3DPoint): Single;
Var D: Single;
Begin
  D := Sqr(X - P.X) + Sqr(Y - P.Y) + Sqr(Z - P.Z);
  If D > 0 Then Result := Sqrt(D) Else Result := 0;
End; // T3DPoint.DistanceFrom

Function T3DPoint.DistanceFrom(CX,CY,CZ: Single): Single;
Var D: Single;
Begin
  D := Sqr(X - CX) + Sqr(Y - CY) + Sqr(Z - CZ);
  If D > 0 Then Result := Sqrt(D) Else Result := 0;
End; // T3DPoint.DistanceFrom

Function T3DPoint.DistanceFrom2(P: T3DPoint): Single;
Var D: Single;
Begin
  D := Sqr(X - P.X) + Sqr(Y - P.Y) + Sqr(Z - P.Z);
  If D > 0 Then Result := D Else Result := 0;
End; // T3DPoint.DistanceFrom2

Function T3DPoint.DistanceFrom2(CX,CY,CZ: Single): Single;
Var D: Single;
Begin
  D := Sqr(X - CX) + Sqr(Y - CY) + Sqr(Z - CZ);
  If D > 0 Then Result := D Else Result := 0;
End; // T3DPoint.DistanceFrom2

Function T3DPoint.GetID: String;
Type LPtr = ^LongWord;
Begin
  Result := IntToHex(LPtr(@X)^,8) + IntToHex(LPtr(@Y)^,8) + IntToHex(LPtr(@Z)^,8);
   //FloatToStr(X) + ',' + FloatToStr(Y) + ',' + FloatToStr(Z);
End; // T3DPoint.GetID

Procedure T3DPoint.GetNormalTo(P1,P2,P3: T3DPoint);
// Efficient way to get the normal to the plane represented by P1, P2, and P3
// (P1 - P2) x (P3 - P2)
Begin
  X := (P1.Y - P2.Y) * (P3.Z - P2.Z) - (P1.Z - P2.Z) * (P3.Y - P2.Y);
  Y := (P1.Z - P2.Z) * (P3.X - P2.X) - (P1.X - P2.X) * (P3.Z - P2.Z);
  Z := (P1.X - P2.X) * (P3.Y - P2.Y) - (P1.Y - P2.Y) * (P3.X - P2.X);
  Normalize;
End; // T3DPoint.GetNormalTo

Procedure T3DPoint.GetParallelTo(P: T3DPoint);
// Removes any part of this ray that is perpendicular to P
Var L1,L2: Single;
Begin
  L1 := P.GetLength;
  If L1 <> 0 Then
  Begin
    L2 := Dot(P);
    Copy(P);
    Multiply(L2 / Sqr(L1));
  End;
End; // T3DPoint.GetParallelTo

Procedure T3DPoint.GetParallelTo(P1,P2,P3: T3DPoint);
// Removes any part of this ray that is perpendicular to the plane represented
// by P1, P2, and P3.
Var S1,S2: T3DPoint;
Begin
  S1 := T3DPoint.Create;
  S1.GetNormalTo(P1,P2,P3);
  S2 := T3DPoint.Create(Self);
  S2.GetParallelTo(S1);
  Subtract(S2);
  S1.Free;
  S2.Free;
End; // T3DPoint.GetParallelTo

Procedure T3DPoint.GetPerpendicularTo(P: T3DPoint);
// Removes any part of this ray that is parallel to P
Var S: T3DPoint;
Begin
  S := T3DPoint.Create(Self);
  S.GetParallelTo(P);
  Subtract(S);
  S.Free;
End; // T3DPoint.GetPerpendicularTo

Procedure T3DPoint.GetPerpendicularTo(P1,P2,P3: T3DPoint);
// Removes any part of this ray that is parallel to the plane represented by
// P1, P2, and P3.
Var S1: T3DPoint;
Begin
  S1 := T3DPoint.Create;
  S1.GetNormalTo(P1,P2,P3);
  GetParallelTo(S1);
  S1.Free;
End; // T3DPoint.GetPerpendicularTo

Function T3DPoint.GetSinAngleBetween(P: T3DPoint): Single;
Var P1: T3DPoint;
Begin
  P1 := T3DPoint.Create(Self);
  P1.Cross(P);
  Result := P1.GetLength / (GetLength * P.GetLength);
  P1.Free;
End; // T3DPoint.GetSinAngleBetween

Function T3DPoint.GetCosAngleBetween(P: T3DPoint): Single;
Begin
  Result := Dot(P) / (GetLength * P.GetLength);
End; // T3DPoint.GetCosAngleBetween
{
Function T3DPoint.GetHessianDistance(P1,P2,P3: T3DPoint): Single;
// P1, P2, and P3 are any three points on a polygon
Var NX,NY,NZ,NL: Single;
Begin
  // Calculate the normal to the polygon

  NX := (P1.Y - P2.Y) * (P3.Z - P2.Z) - (P1.Z - P2.Z) * (P3.Y - P2.Y);
  NY := (P1.Z - P2.Z) * (P3.X - P2.X) - (P1.X - P2.X) * (P3.Z - P2.Z);
  NZ := (P1.X - P2.X) * (P3.Y - P2.Y) - (P1.Y - P2.Y) * (P3.X - P2.X);
  NL := Sqrt(Sqr(NX) + Sqr(NY) + Sqr(NZ));
  If NL <> 0 Then
  Begin
    NX := NX / NL;
    NY := NY / NL;
    NZ := NZ / NL;
  End;
  Result := -(NX * P1.X + NY * P1.Y + NZ * P1.Z);
End; // T3DPoint.GetHessianDistance

Function T3DPoint.GetHessianDistance(Normal,P: T3DPoint): Single;
// Normal is the normal to a polygon and P is any point on the polygon
Begin
  Result := -Normal.Dot(P);
End; // T3DPoint.GetHessianDistance
}
*)
// -----------------------------
// T4x4Matrix
// -----------------------------

Constructor T4x4Matrix.Create;
Var I,J: Integer;
Begin
  For I := 1 To 4 Do
   For J := 1 To 4 Do M[I,J] := 0;
End; // T4x4Matrix.Create

Constructor T4x4Matrix.Create(Matrix: T4x4Matrix);
Begin
  Copy(Matrix);
End; // T4x4Matrix.Create

Constructor T4x4Matrix.Create(M11,M12,M13,M14,M21,M22,M23,M24,M31,M32,M33,M34,M41,M42,M43,M44: Single);
Begin
  M[1,1] := M11;
  M[1,2] := M12;
  M[1,3] := M13;
  M[1,4] := M14;

  M[2,1] := M21;
  M[2,2] := M22;
  M[2,3] := M23;
  M[2,4] := M24;

  M[3,1] := M31;
  M[3,2] := M32;
  M[3,3] := M33;
  M[3,4] := M34;

  M[4,1] := M41;
  M[4,2] := M42;
  M[4,3] := M43;
  M[4,4] := M44;
End; // T4x4Matrix.Create

Procedure T4x4Matrix.Copy(Matrix: T4x4Matrix);
Var I,J: Integer;
Begin
  For I := 1 To 4 Do
   For J := 1 To 4 Do M[I,J] := Matrix.M[I,J];
End; // T4x4Matrix.Copy

Procedure T4x4Matrix.Divide(S: Single);
Var I,J: Integer;
Begin
  For I := 1 To 4 Do
   For J := 1 To 4 Do M[I,J] := M[I,J] / S;
End; // T4x4Matrix.Divide

Function T4x4Matrix.Determinant: Single;
Var A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4: Single;
Begin
  a1 := m[1,1]; b1 := m[1,2];
  c1 := m[1,3]; d1 := m[1,4];

  a2 := m[2,1]; b2 := m[2,2];
  c2 := m[2,3]; d2 := m[2,4];

  a3 := m[3,1]; b3 := m[3,2];
  c3 := m[3,3]; d3 := m[3,4];

  a4 := m[4,1]; b4 := m[4,2];
  c4 := m[4,3]; d4 := m[4,4];

  Result := a1 * det3x3 (b2, b3, b4, c2, c3, c4, d2, d3, d4) -
            b1 * det3x3 (a2, a3, a4, c2, c3, c4, d2, d3, d4) +
            c1 * det3x3 (a2, a3, a4, b2, b3, b4, d2, d3, d4) -
            d1 * det3x3 (a2, a3, a4, b2, b3, b4, c2, c3, c4);
End; // T4x4Matrix.Determinant

Procedure T4x4Matrix.Multiply(P: T3DPoint);
Var X,Y,Z: Single;
Begin
  X := M[1,1] * P.X + M[2,1] * P.Y + M[3,1] * P.Z + M[4,1];
  Y := M[1,2] * P.X + M[2,2] * P.Y + M[3,2] * P.Z + M[4,2];
  Z := M[1,3] * P.X + M[2,3] * P.Y + M[3,3] * P.Z + M[4,3];
  P.Copy(X,Y,Z);
End; // T4x4Matrix.Multiply

Procedure T4x4Matrix.Invert;
Var
  Temp: Single;     {Work-space}
  Amat: T4x4Matrix; {Temporary matrix for storing cofactors}

Begin
  Temp := Determinant;
  Amat := T4x4Matrix.Create(Self);
  Amat.Adjoint;    {Computes adjoint matrix of cofactors and divides that}
  Copy(Amat);         {matrix by the Determinant of original matrix to arrive}
  If (Temp <> 0) Then    {at the inverse:      -1     [Ajk]  }
    Divide(Temp)         {                  [A]    =  -----  }
  Else                   {                             |A|   }
    Raise Exception.Create('Matrix is singular; Can not invert');
  Amat.Free;
End; // T4x4Matrix.Invert

Procedure T4x4Matrix.Adjoint;
{Returns adjoint matrix of cofactors.  Assumes Adjm has NOT been Created}
Var A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4,D1,D2,D3,D4: Single;
Begin
  a1 := m[1,1]; b1 := m[1,2];
  c1 := m[1,3]; d1 := m[1,4];

  a2 := m[2,1]; b2 := m[2,2];
  c2 := m[2,3]; d2 := m[2,4];

  a3 := m[3,1]; b3 := m[3,2];
  c3 := m[3,3]; d3 := m[3,4];

  a4 := m[4,1]; b4 := m[4,2];
  c4 := m[4,3]; d4 := m[4,4];

  // row column labeling reversed since we transpose rows & columns

  m[1,1] :=  det3x3 (b2, b3, b4, c2, c3, c4, d2, d3, d4);
  m[2,1] := -det3x3 (a2, a3, a4, c2, c3, c4, d2, d3, d4);
  m[3,1] :=  det3x3 (a2, a3, a4, b2, b3, b4, d2, d3, d4);
  m[4,1] := -det3x3 (a2, a3, a4, b2, b3, b4, c2, c3, c4);

  m[1,2] := -det3x3 (b1, b3, b4, c1, c3, c4, d1, d3, d4);
  m[2,2] :=  det3x3 (a1, a3, a4, c1, c3, c4, d1, d3, d4);
  m[3,2] := -det3x3 (a1, a3, a4, b1, b3, b4, d1, d3, d4);
  m[4,2] :=  det3x3 (a1, a3, a4, b1, b3, b4, c1, c3, c4);

  m[1,3] :=  det3x3 (b1, b2, b4, c1, c2, c4, d1, d2, d4);
  m[2,3] := -det3x3 (a1, a2, a4, c1, c2, c4, d1, d2, d4);
  m[3,3] :=  det3x3 (a1, a2, a4, b1, b2, b4, d1, d2, d4);
  m[4,3] := -det3x3 (a1, a2, a4, b1, b2, b4, c1, c2, c4);

  m[1,4] := -det3x3 (b1, b2, b3, c1, c2, c3, d1, d2, d3);
  m[2,4] :=  det3x3 (a1, a2, a3, c1, c2, c3, d1, d2, d3);
  m[3,4] := -det3x3 (a1, a2, a3, b1, b2, b3, d1, d2, d3);
  m[4,4] :=  det3x3 (a1, a2, a3, b1, b2, b3, c1, c2, c3);
End; // T4x4Matrix.Adjoint

// ------------------------------
// TTextureInfo
// ------------------------------

Constructor TTextureInfo.Create(TextureInfo: String);
Var
  St            : String;
  Textures      : String;
  Opacities     : String;
  Normals       : String;
  Parameters    : String;
  TextureTokens : TTokenArray;
  OpacityTokens : TTokenArray;
  NormalTokens  : TTokenArray;
  I,J           : Integer;

  Procedure FixSlashes(Var St: String);
  Var I: Integer;
  Begin
    I := Pos('/',St);
    Repeat
      If I > 0 Then St[I] := '\';
      I := Pos('/',St);
    Until I < 1;
  End; // FixSlashes

  Function Get(DelimiterIndex: TTextureInfoDelimiter; Var Source: String): String;
  Var
    I,J       : TTextureInfoDelimiter;
    Locations : Array[TTextureInfoDelimiter] Of Integer;
    Lowest    : Integer;
    Found     : Boolean;

  Begin
    If Source <> '' Then
    Begin
      // Find all the delimiters

      Lowest := Length(Source) + 1;
      For I := Low(TTextureInfoDelimiter) To High(TTextureInfoDelimiter) Do
      Begin
        // Skip the first one as it doesn't have a leading delimiter

        If I <> Low(TTextureInfoDelimiter) Then
        Begin
          Locations[I] := Pos(TextureInfoDelimiters[I],Source);
          If (Locations[I] > 0) And (Locations[I] < Lowest) Then Lowest := Locations[I];
        End;
      End; // For I

      // The first one is a special case as it doesn't have a leading delimiter, so we find out
      // if it's there by checking to see if anything else is at the beginning of the string

      If Lowest > 1
       Then Locations[Low(TTextureInfoDelimiter)] := 0
       Else Locations[Low(TTextureInfoDelimiter)] := -1;

      // Continue only if the delimiter we want is present 

      If (Locations[DelimiterIndex] > 0) And (DelimiterIndex <> Low(TTextureInfoDelimiter)) Or (Locations[DelimiterIndex] = 0) Then
      Begin
        // Look for the next delimiter after the one we're looking for

        Lowest := Length(Source) + 1;
        Found  := False;
        For I := Low(TTextureInfoDelimiter) To High(TTextureInfoDelimiter) Do
        Begin
          If (I <> DelimiterIndex) And (Locations[I] > 0) And (Locations[I] < Lowest) And (Locations[I] > Locations[DelimiterIndex]) Then
          Begin
            J      := I;
            Lowest := Locations[I];
            Found  := True;
          End;
        End; // For I

        // Extract the string

        If Found Then
        Begin
          // There is something after this one

          Result := Copy(Source,Locations[DelimiterIndex] + 1,Lowest - Locations[DelimiterIndex] - 1);
          Source := Copy(Source,1,Locations[DelimiterIndex] - 1) + Copy(Source,Lowest,Length(Source));
        End
        Else
        Begin
          // There is nothing after this one

          Result := Copy(Source,Locations[DelimiterIndex] + 1,Length(Source));
          Source := Copy(Source,1,Locations[DelimiterIndex] - 1);
        End;
      End;
    End
    Else Result := '';
  End; // Get

Begin
  TextureMaps    := TStringList.Create;
  OpacityMaps    := TStringList.Create;
  NormalMaps     := TStringList.Create;
  AnimTime       := 0;

  // More flexible way of extracting delimited items

  St             := TextureInfo;
  Textures       := Get(tidTextures,St);
  Opacities      := Get(tidOpacities,St);
  Normals        := Get(tidNormals,St);
  FragmentShader := Get(tidFragmentShader,St);
  Parameters     := Get(tidParameters,St);

{
  St             := GetToken('+',TextureInfo);
  Textures       := GetToken('|',St);
  Opacities      := GetToken('?',St);
  Normals        := GetToken('*',St);
  FragmentShader := St;
  Parameters     := TextureInfo;
}

  FixSlashes(Textures);
  FixSlashes(Opacities);
  FixSlashes(Normals);
  FixSlashes(FragmentShader);

  GetTokens(';',Textures,TextureTokens);
  GetTokens(';',Opacities,OpacityTokens);
  GetTokens(';',Normals,NormalTokens);

  J := Max(Max(High(TextureTokens),High(OpacityTokens)),High(NormalTokens));
  For I := 0 To J Do
  Begin
    If I <= High(TextureTokens)
     Then TextureMaps.Add(TextureTokens[I])
     Else TextureMaps.Add('');
    If I <= High(OpacityTokens)
     Then OpacityMaps.Add(OpacityTokens[I])
     Else OpacityMaps.Add('');
    If I <= High(NormalTokens)
     Then NormalMaps.Add(NormalTokens[I])
     Else NormalMaps.Add('');
  End; // For I

  If Parameters <> '' Then Val(Parameters,AnimTime,I);
  If TextureMaps.Count > 1 Then
  Begin
    If AnimTime = 0
     Then AnimTime := DefaultAnimTimePerFrame * TextureMaps.Count
     Else AnimTime := Max(MinimumAnimTimePerFrame * TextureMaps.Count,AnimTime);
  End;

  SetLength(TextureTokens,0);
  SetLength(OpacityTokens,0);
  SetLength(NormalTokens,0);
End; // TTextureInfo.Create

Destructor TTextureInfo.Destroy;
Begin
  TextureMaps.Free;
  OpacityMaps.Free;
  NormalMaps.Free;
End; // TTextureInfo.Destroy

Procedure TTextureInfo.ConvertTexturesToUpperCase;
Var I: Integer;
Begin
  For I := 0 To TextureMaps.Count - 1 Do TextureMaps.Strings[I] := UpperCase(TextureMaps.Strings[I]);
  For I := 0 To OpacityMaps.Count - 1 Do OpacityMaps.Strings[I] := UpperCase(OpacityMaps.Strings[I]);
  For I := 0 To NormalMaps.Count  - 1 Do NormalMaps.Strings[I]  := UpperCase(NormalMaps.Strings[I]);
End; // TTextureInfo.ConvertTexturesToUpperCase

Function TTextureInfo.ToStringNoParms: String;
Var
  St   : String;
  Tex  : String;
  Opac : String;
  Norm : String;
  I    : Integer;

Begin
  Tex := '';
  For I := 0 To TextureMaps.Count - 1 Do
  Begin
    St := Trim(TextureMaps.Strings[I]);
    If St <> '' Then
    Begin
      If Tex <> '' Then Tex := Tex + ';';
      Tex := Tex + St;
    End;
  End; // For I

  Opac := '';
  For I := 0 To OpacityMaps.Count - 1 Do
  Begin
    St := Trim(OpacityMaps.Strings[I]);
    If St <> '' Then
    Begin
      If Opac <> '' Then Opac := Opac + ';';
      Opac := Opac + St;
    End;
  End; // For I

  Norm := '';
  For I := 0 To NormalMaps.Count - 1 Do
  Begin
    St := Trim(NormalMaps.Strings[I]);
    If St <> '' Then
    Begin
      If Norm <> '' Then Norm := Norm + ';';
      Norm := Norm + St;
    End;
  End; // For I

  If Opac <> '' Then Tex := Tex + TextureInfoDelimiters[tidOpacities] + Opac;
  If Norm <> '' Then Tex := Tex + TextureInfoDelimiters[tidNormals] + Norm;
  If FragmentShader <> '' Then Tex := Tex + TextureInfoDelimiters[tidFragmentShader] + FragmentShader;

  Result := Tex;
End; // TTextureInfo.ToStringNoParms

Function TTextureInfo.ToString: String;
Var St: String;
Begin
  St := ToStringNoParms;
  If AnimTime <> 0 Then St := St + TextureInfoDelimiters[tidParameters] + FloatToStr(AnimTime);
  Result := St;
End; // TTextureInfo.ToString

Function TTextureInfo.FirstTexture: String;
Begin
  If TextureMaps.Count > 0
   Then Result := TextureMaps.Strings[0]
   Else Result := '';
End; // TTextureInfo.FirstTexture

// ------------------------------
// TPolygon
// ------------------------------

Constructor TPolygon.Create;
Begin
  SetLength(Vertices,0);
  SetLength(Colors,0);
//  SetLength(TextureInfo,0);
  HasTexCoords := False;
  HasColor     := False;
  TextureState := tsSolid;
  HasSolid     := True;
  Solid        := True;
  HasMasked    := False;
  Masked       := False;
  Tag          := 0;
  HasTag       := False;
  HasAngle     := False;
  Angle        := 0;
  TextureRef   := Nil;
  NeedsMask    := False;
  AnimTime     := 0;
  TextureInfo  := TTextureInfo.Create('');
  FTexture     := '';
//  FTexture     := GetTexturesAsString;
  SetLength(TX,0);
  SetLength(TZ,0);
End; // TPolygon.Create

Constructor TPolygon.Create(P: TPolygon);
Var I: LongInt;
Begin
//  CopyTextureFromPolygon(P);
  SetLength(Vertices,High(P.Vertices) + 1);
  SetLength(Colors,High(P.Colors) + 1);
  For I := 0 To High(P.Vertices) Do Vertices[I] := P.Vertices[I];
  For I := 0 To High(P.Colors)   Do Colors[I]   := P.Colors[I];
  HasTexCoords := P.HasTexCoords;
  HasColor     := P.HasColor;
  TextureState := P.TextureState;
  HasSolid     := P.HasSolid;
  Solid        := P.Solid;
  HasMasked    := P.HasMasked;
  Masked       := P.Masked;
  Tag          := P.Tag;
  HasTag       := P.HasTag;
  HasAngle     := P.HasAngle;
  Angle        := P.Angle;
  TextureRef   := P.TextureRef;
  NeedsMask    := P.NeedsMask;
  AnimTime     := P.AnimTime;
  Texture      := P.Texture; // Note how I'm not just writing to FTexture
  SetLength(TX,High(P.TX) + 1);
  SetLength(TZ,High(P.TZ) + 1);
  For I := 0 To High(P.TX) Do TX[I] := P.TX[I];
  For I := 0 To High(P.TZ) Do TZ[I] := P.TZ[I];
{
  SetLength(TextureInfo,High(P.TextureInfo) + 1);
  For I := 0 To High(TextureInfo) Do
  Begin
    TextureInfo[I].TextureMapName := P.TextureInfo[I].TextureMapName;
    TextureInfo[I].OpacityMapName := P.TextureInfo[I].OpacityMapName;
  End; // For I
}  
End; // TPolygon.Create

Constructor TPolygon.CreateBasic(P: TPolygon);
// Like Create(TPolygon), but no vertices or texture coordinates
Var I: LongInt;
Begin
//  CopyTextureFromPolygon(P);
  SetLength(Vertices,0);
  SetLength(Colors,High(P.Colors) + 1);
  For I := 0 To High(P.Colors) Do Colors[I] := P.Colors[I];
  HasTexCoords := P.HasTexCoords;
  HasColor     := P.HasColor;
  TextureState := P.TextureState;
  HasSolid     := P.HasSolid;
  Solid        := P.Solid;
  HasMasked    := P.HasMasked;
  Masked       := P.Masked;
  Tag          := P.Tag;
  HasTag       := P.HasTag;
  HasAngle     := P.HasAngle;
  Angle        := P.Angle;
  TextureRef   := P.TextureRef;
  NeedsMask    := P.NeedsMask;
  AnimTime     := P.AnimTime;
  Texture      := P.Texture; // Note how I'm not just writing to FTexture
  SetLength(TX,0);
  SetLength(TZ,0);
End; // TPolygon.CreateBasic

Constructor TPolygon.Create(Const V: Array Of LongInt; Tex: String);
Var I: LongInt;
Begin
  SetLength(Vertices,High(V) + 1);
  SetLength(Colors,0);
  For I := 0 To High(V) Do Vertices[I] := V[I];
  HasTexCoords := False;
  HasColor     := False;
  TextureState := tsSolid;
  HasSolid     := True;
  Solid        := True;
  HasMasked    := False;
  Masked       := False;
  Tag          := 0;
  HasTag       := False;
  HasAngle     := False;
  Angle        := 0;
  TextureRef   := Nil;
  NeedsMask    := False;
  AnimTime     := 0;
  SetLength(TX,0);
  SetLength(TZ,0);
  Texture      := Tex;
End; // TPolygon.Create

Destructor TPolygon.Destroy;
Begin
  TextureInfo.Free;
  SetLength(Vertices,0);
  SetLength(Colors,0);
  SetLength(TX,0);
  SetLength(TZ,0);
End; // TPolygon.Destroy

Procedure TPolygon.Invert;
Var
  I,J,K : Integer;
  S     : Single;

Begin
  J := High(Vertices);
  For I := 0 To ((J + 1) Div 2) - 1 Do
  Begin
    K := Vertices[J];
    Vertices[J] := Vertices[I];
    Vertices[I] := K;
    Dec(J);
  End; // For I

  J := High(TX);
  For I := 0 To ((J + 1) Div 2) - 1 Do
  Begin
    S := TX[J];
    TX[J] := TX[I];
    TX[I] := S;
    Dec(J);
  End; // For I

  J := High(TZ);
  For I := 0 To ((J + 1) Div 2) - 1 Do
  Begin
    S := TZ[J];
    TZ[J] := TZ[I];
    TZ[I] := S;
    Dec(J);
  End; // For I

  J := High(Colors);
  For I := 0 To ((J + 1) Div 2) - 1 Do
  Begin
    K := Colors[J];
    Colors[J] := Colors[I];
    Colors[I] := K;
    Dec(J);
  End; // For I
End; // TPolygon.Invert

Function TPolygon.IntersectsSegment(V1,V2: T3DPoint; Mesh: TMeshObject): Boolean;
Var
  I     : Integer;
  Found : Boolean;
  ISect : T3DPoint;
  T1    : T3DPoint;
  T2    : T3DPoint;
  T3    : T3DPoint;

Begin
  // Check each triangle in the polygon

  I     := 0;
  Found := False;
  T1    := T3DPoint.Create;
  T2    := T3DPoint.Create;
  T3    := T3DPoint.Create;
  ISect := T3DPoint.Create;
  While (I < High(Vertices) - 1) And Not Found Do
  Begin
    T1.Copy(T3DPoint(Mesh.Vertices.Objects[Vertices[0]]));
    T2.Copy(T3DPoint(Mesh.Vertices.Objects[Vertices[I + 1]]));
    T3.Copy(T3DPoint(Mesh.Vertices.Objects[Vertices[I + 2]]));
    Mesh.MakeAbsolute(T1);
    Mesh.MakeAbsolute(T2);
    Mesh.MakeAbsolute(T3);
    If gts_segment_triangle_intersection(V1,V2,T1,T2,T3,False,ISect) Then Found := True;
    Inc(I);
  End; // While
  T1.Free;
  T2.Free;
  T3.Free;
  ISect.Free;
  Result := Found;
End; // TPolygon.IntersectsSegment

Function TPolygon.IntersectsSegment(V1,V2: T3DPoint; Mesh: TMeshObject; Intersection: T3DPoint): Boolean;
Var
  I     : Integer;
  Found : Boolean;
  T1    : T3DPoint;
  T2    : T3DPoint;
  T3    : T3DPoint;

Begin
  // Check each triangle in the polygon

  I     := 0;
  Found := False;
  T1    := T3DPoint.Create;
  T2    := T3DPoint.Create;
  T3    := T3DPoint.Create;
  While (I < High(Vertices) - 1) And Not Found Do
  Begin
    T1.Copy(T3DPoint(Mesh.Vertices.Objects[Vertices[0]]));
    T2.Copy(T3DPoint(Mesh.Vertices.Objects[Vertices[I + 1]]));
    T3.Copy(T3DPoint(Mesh.Vertices.Objects[Vertices[I + 2]]));
    Mesh.MakeAbsolute(T1);
    Mesh.MakeAbsolute(T2);
    Mesh.MakeAbsolute(T3);
    If gts_segment_triangle_intersection(V1,V2,T1,T2,T3,False,Intersection) Then Found := True;
    Inc(I);
  End; // While
  T1.Free;
  T2.Free;
  T3.Free;
  Result := Found;
End; // TPolygon.IntersectsSegment

Function TPolygon.IntersectsPolygon(P: TPolygon; MO1,MO2: TMeshObject): Boolean;
// MO1 is this polygon's mesh and MO2 is polygon P's mesh.
Var
  I,J   : Integer;
  V1,V2 : T3DPoint;
  Found : Boolean;
  ISect : T3DPoint;
  T1    : T3DPoint;
  T2    : T3DPoint;
  T3    : T3DPoint;

Begin
  // Check the first polygon against the second one

  I     := 0;
  Found := False;
  V1    := T3DPoint.Create;
  V2    := T3DPoint.Create;
  T1    := T3DPoint.Create;
  T2    := T3DPoint.Create;
  T3    := T3DPoint.Create;
  ISect := T3DPoint.Create;
  While (I <= High(Vertices)) And Not Found Do
  Begin
    // Get the line segment

    V1.Copy(T3DPoint(MO1.Vertices.Objects[Vertices[I]]));
    V2.Copy(T3DPoint(MO1.Vertices.Objects[Vertices[(I + 1) Mod (High(Vertices) + 1)]]));
    MO1.MakeAbsolute(V1);
    MO1.MakeAbsolute(V2);

    // Check each triangle in the destination polygon

    J := 0;
    While (J < High(P.Vertices) - 1) And Not Found Do
    Begin
      T1.Copy(T3DPoint(MO2.Vertices.Objects[P.Vertices[0]]));
      T2.Copy(T3DPoint(MO2.Vertices.Objects[P.Vertices[J + 1]]));
      T3.Copy(T3DPoint(MO2.Vertices.Objects[P.Vertices[J + 2]]));
      MO2.MakeAbsolute(T1);
      MO2.MakeAbsolute(T2);
      MO2.MakeAbsolute(T3);
      If gts_segment_triangle_intersection(V1,V2,T1,T2,T3,False,ISect) Then Found := True;
      Inc(J);
    End; // While
    Inc(I);
  End; // While


  // Now we have to check the second polygon against the first one, since one might
  // be inside the other

  I     := 0;
  While (I <= High(P.Vertices)) And Not Found Do
  Begin
    // Get the line segment

    V1.Copy(T3DPoint(MO2.Vertices.Objects[P.Vertices[I]]));
    V2.Copy(T3DPoint(MO2.Vertices.Objects[P.Vertices[(I + 1) Mod (High(P.Vertices) + 1)]]));
    MO2.MakeAbsolute(V1);
    MO2.MakeAbsolute(V2);

    // Check each triangle in the destination polygon

    J := 0;
    While (J < High(Vertices) - 1) And Not Found Do
    Begin
      T1.Copy(T3DPoint(MO1.Vertices.Objects[Vertices[0]]));
      T2.Copy(T3DPoint(MO1.Vertices.Objects[Vertices[J + 1]]));
      T3.Copy(T3DPoint(MO1.Vertices.Objects[Vertices[J + 2]]));
      MO1.MakeAbsolute(T1);
      MO1.MakeAbsolute(T2);
      MO1.MakeAbsolute(T3);
      If gts_segment_triangle_intersection(V1,V2,T1,T2,T3,False,ISect) Then Found := True;
      Inc(J);
    End; // While
    Inc(I);
  End; // While
  V1.Free;
  V2.Free;
  T1.Free;
  T2.Free;
  T3.Free;
  ISect.Free;
  Result := Found;
End; // TPolygon.IntersectsPolygon

Function TPolygon.FacesPolygon(P: TPolygon; MO1,MO2: TMeshObject; EitherSide: Boolean): Boolean;
// MO1 is this polygon's mesh and MO2 is polygon P's mesh.
Var
  I      : Integer;
  B      : Boolean;
  Normal : T3DPoint;
  V      : T3DPoint;

Begin
  I      := 0;
  B      := False;
  Normal := P.GetNormal(MO2);
  V      := T3DPoint.Create;
  While (I <= High(P.Vertices)) And Not B Do
  Begin
    V.Copy(T3DPoint(MO2.Vertices.Objects[P.Vertices[I]]));
    MO2.MakeAbsolute(V);
    B := B Or IsVisibleTo(V,Normal,MO1,EitherSide);
    Inc(I);
  End; // While
  V.Free;
  Normal.Free;
  Result := B;
End; // TPolygon.FacesPolygon

Function TPolygon.CenterFacesPolygon(P: TPolygon; MO1,MO2: TMeshObject; EitherSide: Boolean): Boolean;
// MO1 is this polygon's mesh and MO2 is polygon P's mesh.
Var
  I      : Integer;
  B      : Boolean;
  Normal : T3DPoint;
  V      : T3DPoint;

Begin
  I      := 0;
  B      := False;
  Normal := P.GetNormal(MO2);
  V      := T3DPoint.Create;
  While (I <= High(P.Vertices)) And Not B Do
  Begin
    V.Copy(T3DPoint(MO2.Vertices.Objects[P.Vertices[I]]));
    MO2.MakeAbsolute(V);
    B := B Or CenterIsVisibleTo(V,Normal,MO1,EitherSide);
    Inc(I);
  End; // While
  V.Free;
  Normal.Free;
  Result := B;
End; // TPolygon.CenterFacesPolygon

Function TPolygon.IsVisibleTo(Position,Direction: T3DPoint; Mesh: TMeshObject; EitherSide: Boolean): Boolean;
Const Threshold = -0.001; // Have to have a nonzero threshold due to rounding error
Var
  Normal   : T3DPoint;
  V        : T3DPoint;
  I        : Integer;
  MinDot   : Single;

Begin
  If High(Vertices) >= 2 Then
  Begin
    Normal := GetNormal(Mesh);
    MinDot := 0;
    V      := T3DPoint.Create;
    For I := 0 To High(Vertices) Do
    Begin
      V.Copy(T3DPoint(Mesh.Vertices.Objects[Vertices[I]]));
      Mesh.MakeAbsolute(V);
      V.Subtract(Position);
      If (V.Dot(Direction) > -Threshold) Then
      Begin
        MinDot := Min(MinDot,Normal.Dot(V));
        If EitherSide Then MinDot := Min(MinDot,-Normal.Dot(V));
      End;
    End; // For I
    V.Free;
    Result := MinDot < Threshold;
    Normal.Free;
  End
  Else Result := False; // Could actually test the line segment but it's not a "legal" polygon anyway
End; // TPolygon.IsVisibleTo

Function TPolygon.CenterIsVisibleTo(Position,Direction: T3DPoint; Mesh: TMeshObject; EitherSide: Boolean): Boolean;
Const Threshold = -0.001; // Have to have a nonzero threshold due to rounding error
Var
  Normal   : T3DPoint;
  Center   : T3DPoint;
  MinDot   : Single;

Begin
  If High(Vertices) >= 2 Then
  Begin
    Normal := GetNormal(Mesh);
    Center := GetCenter(Mesh);
    Center.Subtract(Position);
    MinDot := 0;
    If (Center.Dot(Direction) > -Threshold) Then
    Begin
      MinDot := Min(MinDot,Normal.Dot(Center));
      If EitherSide Then MinDot := Min(MinDot,-Normal.Dot(Center));
    End;
    Result := MinDot < Threshold;
    Normal.Free;
    Center.Free;
  End
  Else Result := False; // Could actually test the line segment but it's not a "legal" polygon anyway
End; // TPolygon.CenterIsVisibleTo

Function TPolygon.GetColorString: String;
Var
  St : String;
  I  : Integer;

Begin
  St := '';
  For I := 0 To High(Colors) Do
  Begin
    St := St + '$' + IntToHex(Integer(Colors[I]),8);
    If I < High(Colors) Then St := St + '|';
  End; // For I
  Result := St;
End; // TPolygon.GetColorString
{
Procedure TPolygon.CopyTextureFromPolygon(P: TPolygon);
Var I: Integer;
Begin
  SetLength(TextureInfo,High(P.TextureInfo) + 1);
  For I := 0 To High(TextureInfo) Do
  Begin
    TextureInfo[I].TextureMapName := P.TextureInfo[I].TextureMapName;
    TextureInfo[I].OpacityMapName := P.TextureInfo[I].OpacityMapName;
  End; // For I
  Texture := P.Texture;
End; // TPolygon.CopyTextureFromPolygon
}
Procedure TPolygon.ConvertTexturesToUpperCase;
Begin
  TextureInfo.ConvertTexturesToUpperCase;
  FTexture := TextureInfo.ToString;
End; // TPolygon.ConvertTexturesToUpperCase
{
Function TPolygon.GetTexturesAsString: String;
Var
  St  : String;
  St1 : String;
  St2 : String;
  I   : Integer;
  B   : Boolean;

Begin
  St := '';
  For I := 0 To High(TextureInfo) Do
  Begin
    St := St + Trim(TextureInfo[I].TextureMapName);
    If I < High(TextureInfo) Then St := St + ';';
  End; // For I

  St1 := '';
  B   := False;
  For I := 0 To High(TextureInfo) Do
  Begin
    St2 := Trim(TextureInfo[I].OpacityMapName);
    If St2 <> '' Then B := True;
    St1 := St1 + St2;
    If I < High(TextureInfo) Then St1 := St1 + ';';
  End; // For I

  If B Then St := St + '|' + St1;
  St := St + '+' + FloatToStr(AnimTime);
  Result := St;
End; // TPolygon.GetTexturesAsString
}
Procedure TPolygon.SetTexture(St: String);
{Var
  Textures      : String;
  Opacities     : String;
  Parameters    : String;
  TextureTokens : TTokenArray;
  OpacityTokens : TTokenArray;
  I             : Integer;
}
Begin
  TextureInfo.Free;
  TextureInfo := TTextureInfo.Create(St);
  FTexture    := TextureInfo.ToString;
{
  BreakupTextureString(St,Textures,Opacities,Parameters);
  GetTokens(';',Textures,TextureTokens);
  GetTokens(';',Opacities,OpacityTokens);
  SetLength(TextureInfo,Max(High(TextureTokens),High(OpacityTokens)) + 1);
  For I := 0 To High(TextureInfo) Do
  Begin
    TextureInfo[I].TextureMapName := '';
    TextureInfo[I].OpacityMapName := '';
  End; // For I
  For I := 0 To High(TextureTokens) Do TextureInfo[I].TextureMapName := TextureTokens[I];
  For I := 0 To High(OpacityTokens) Do
  Begin
    If OpacityTokens[I] <> '' Then
    Begin
      Masked    := True;
      HasMasked := True;
    End;
    TextureInfo[I].OpacityMapName := OpacityTokens[I];
  End; // For I
  If Parameters <> '' Then Val(Parameters,AnimTime,I);
  If High(TextureInfo) > 0 Then
  Begin
    If AnimTime = 0
     Then AnimTime := DefaultAnimTimePerFrame * (High(TextureInfo) + 1)
     Else AnimTime := Max(MinimumAnimTimePerFrame * (High(TextureInfo) + 1),AnimTime);
  End;
  SetLength(TextureTokens,0);
  SetLength(OpacityTokens,0);
  FTexture := GetTexturesAsString;
}
End; // TPolygon.SetTexture

Procedure TPolygon.SetSingleTexture(TextureMapName,OpacityMapName: String);
Begin
  TextureMapName := Trim(TextureMapName);
  OpacityMapName := Trim(OpacityMapName);
  If OpacityMapName <> '' Then
  Begin
    Texture   := TextureMapName + '|' + OpacityMapName + '+' + FloatToStr(AnimTime);
    Masked    := True;
    HasMasked := True;
  End
  Else Texture := TextureMapName + '+' + FloatToStr(AnimTime);
End; // TPolygon.SetSingleTexture

Function TPolygon.GetNormal(Mesh: TMeshObject): T3DPoint;
//Var V,V1,V2: T3DPoint;
Var V,V1,V2,V3: T3DPoint;
Begin
  If High(Vertices) >= 2 Then
  Begin
    V  := T3DPoint.Create;
    V1 := T3DPoint(Mesh.Vertices.Objects[Vertices[0]]);
    V2 := T3DPoint(Mesh.Vertices.Objects[Vertices[1]]);
    V3 := T3DPoint(Mesh.Vertices.Objects[Vertices[2]]);
    V1 := Mesh.GetAbsoluteLocation(V1);
    V2 := Mesh.GetAbsoluteLocation(V2);
    V3 := Mesh.GetAbsoluteLocation(V3);

    V.GetNormalTo(V1,V2,V3);

    V1.Free;
    V2.Free;
    V3.Free;
    Result := V;
{
    V  := T3DPoint.Create;
    V1 := T3DPoint.Create;
    V2 := T3DPoint.Create;
    V.Copy(T3DPoint(Mesh.Vertices.Objects[Vertices[1]]));
    V1.Copy(T3DPoint(Mesh.Vertices.Objects[Vertices[0]]));
    V2.Copy(T3DPoint(Mesh.Vertices.Objects[Vertices[2]]));
    V1.Subtract(V);
    V2.Subtract(V);
    V1.Cross(V2);
    V1.Normalize;
    Result := V1;
    V.Free;
    V2.Free;
}
  End
  Else Result := T3DPoint.Create;
End; // TPolygon.GetNormal

Function TPolygon.GetCenter(Mesh: TMeshObject): T3DPoint;
Var
  I      : Integer;
  Center : T3DPoint;

Begin
  Center := T3DPoint.Create;
  For I := 0 To High(Vertices) Do Center.Add(T3DPoint(Mesh.Vertices.Objects[Vertices[I]]));
  If High(Vertices) >= 0 Then Center.Divide(High(Vertices) + 1);
  Mesh.MakeAbsolute(Center);
  Result := Center;
End; // TPolygon.GetCenter

Function TPolygon.GetCenterRelativeToMesh(Mesh: TMeshObject): T3DPoint;
// Returns the center of the polygon, RELATIVE TO THE MESH (not an absolute position)
Var
  I      : Integer;
  Center : T3DPoint;

Begin
  Center := T3DPoint.Create;
  For I := 0 To High(Vertices) Do Center.Add(T3DPoint(Mesh.Vertices.Objects[Vertices[I]]));
  If High(Vertices) >= 0 Then Center.Divide(High(Vertices) + 1);
  Result := Center;
End; // TPolygon.GetCenterRelativeToMesh

Procedure TPolygon.GetBounds(Mesh: TMeshObject; Var MinX,MinY,MinZ,MaxX,MaxY,MaxZ: Single);
Var
  I : Integer;
  V : T3DPoint;

Begin
  For I := 0 To High(Vertices) Do
  Begin
    V := T3DPoint(Mesh.Vertices.Objects[Vertices[I]]);
    If I = 0 Then
    Begin
      MinX := V.X;
      MinY := V.Y;
      MinZ := V.Z;
      MaxX := MinX;
      MaxY := MinY;
      MaxZ := MinZ;
    End
    Else
    Begin
      MinX := Min(MinX,V.X);
      MinY := Min(MinY,V.Y);
      MinZ := Min(MinZ,V.Z);
      MaxX := Max(MaxX,V.X);
      MaxY := Max(MaxY,V.Y);
      MaxZ := Max(MaxZ,V.Z);
    End;
  End; // For I
End; // TPolygon.GetBounds

Procedure TPolygon.CalculateAngle(MO: TMeshObject);
Const Noise = 0.1;
Var
  I1,I2 : Integer;
  T1,T2 : Single;
  K     : Integer;
  Found : Boolean;
  V1,V2 : T3DPoint;

Begin
  K  := 0;

  // Try to determine which way the texture points

  If HasTexCoords Then
  Begin
    I1    := 0;
    Found := False;
    While (I1 <= High(Vertices)) And Not Found Do
    Begin
      I2 := (I1 + 1) Mod High(Vertices);
      V1 := T3DPoint(MO.Vertices.Objects[Vertices[I1]]);
      V2 := T3DPoint(MO.Vertices.Objects[Vertices[I2]]);
      If Abs(TX[I1] - TX[I2]) > Abs(TZ[I1] - TZ[I2]) Then
      Begin
        T1 := TX[I1];
        T2 := TX[I2];
        If (Abs(T1 - T2) > Noise) And ((Abs(V1.Y - V2.Y) - Abs(V1.X - V2.X)) > Noise) Then
        Begin
          If Abs(V1.Y - V2.Y) > Abs(V1.X - V2.X) Then
          Begin
            // Either 0 or 180

            If V1.Y > V2.Y Then
            Begin
              If T1 > T2 Then K := 2; // 180
            End
            Else
            Begin
              If T1 < T2 Then K := 2; // 180
            End;
          End
          Else
          Begin
            // Either 90 or 270

            If V1.X > V2.X Then
            Begin
              If T1 > T2
               Then K := 3    // 270
               Else K := 1;   // 90
            End
            Else
            Begin
              If T1 < T2
               Then K := 3    // 270
               Else K := 1;   // 90
            End;
          End;
          Found := True;
        End;
      End
      Else
      Begin
        T1 := TZ[I1];
        T2 := TZ[I2];
        If (Abs(T1 - T2) > Noise) And ((Abs(V1.Y - V2.Y) - Abs(V1.X - V2.X)) > Noise) Then
        Begin
          If Abs(V1.X - V2.X) > Abs(V1.Y - V2.Y) Then
          Begin
            // Either 0 or 180

            If V1.X > V2.X Then
            Begin
              If T1 < T2 Then K := 2; // 180
            End
            Else
            Begin
              If T1 > T2 Then K := 2; // 180
            End;
          End
          Else
          Begin
            // Either 90 or 270

            If V1.Y > V2.Y Then
            Begin
              If T1 < T2
               Then K := 3    // 270
               Else K := 1;   // 90
            End
            Else
            Begin
              If T1 > T2
               Then K := 3    // 270
               Else K := 1;   // 90
            End;
          End;
          Found := True;
        End;
      End;
      Inc(I1);
    End; // While
  End;
  Case K Of
    0: Angle := 0;
    1: Angle := 90;
    2: Angle := 180;
    3: Angle := 270;
  Else
    Angle := 0;
  End; // Case
  HasAngle := True;
End; // TPolygon.CalculateAngle

Procedure TPolygon.SetTexCoordsFromAngle(MO: TMeshObject; MaxX,MaxY: Double; GX,GY: Integer);
Const
  XStep = GridSize;
  YStep = GridSize;
  
Var
  I     : Integer;
  V     : T3DPoint;
  CX,CY : Double;

Begin
  CX := MaxY - GX * YStep;
  CY := MaxX - GY * XStep;
  For I := 0 To High(Vertices) Do
  Begin
    V := T3DPoint(MO.Vertices.Objects[Vertices[I]]);
    Case Angle Of
      0: Begin
           TX[I] := ((GX + 1) * 1{TextureSize}) + 1/TextureSize - (V.Y - CX) * ((1{TextureSize} - 2/TextureSize) / YStep);
           TZ[I] := (GY       * 1{TextureSize}) - 1/TextureSize + (V.X - CY) * ((1{TextureSize} - 2/TextureSize) / XStep);
         End;
     90: Begin
           TZ[I] := (GX       * 1{TextureSize}) - 1/TextureSize + (V.Y - CX) * ((1{TextureSize} - 2/TextureSize) / YStep);
           TX[I] := (GY       * 1{TextureSize}) - 1/TextureSize + (V.X - CY) * ((1{TextureSize} - 2/TextureSize) / XStep);
         End;
    180: Begin
           TX[I] := (GX       * 1{TextureSize}) - 1/TextureSize + (V.Y - CX) * ((1{TextureSize} - 2/TextureSize) / YStep);
           TZ[I] := ((GY + 1) * 1{TextureSize}) + 1/TextureSize - (V.X - CY) * ((1{TextureSize} - 2/TextureSize) / XStep);
         End;
    270: Begin
           TZ[I] := ((GX + 1) * 1{TextureSize}) + 1/TextureSize - (V.Y - CX) * ((1{TextureSize} - 2/TextureSize) / YStep);
           TX[I] := ((GY + 1) * 1{TextureSize}) + 1/TextureSize - (V.X - CY) * ((1{TextureSize} - 2/TextureSize) / XStep);
         End;
    End; // Case
{
    Case Angle Of
      0: Begin
           TX[I] := Round(((GX + 1) * TextureSize) + 1 - (V.Y - CX) * ((TextureSize - 2) / YStep));
           TZ[I] := Round((GY       * TextureSize) - 1 + (V.X - CY) * ((TextureSize - 2) / XStep));
         End;
     90: Begin
           TZ[I] := Round((GX       * TextureSize) - 1 + (V.Y - CX) * ((TextureSize - 2) / YStep));
           TX[I] := Round((GY       * TextureSize) - 1 + (V.X - CY) * ((TextureSize - 2) / XStep));
         End;
    180: Begin
           TX[I] := Round((GX       * TextureSize) - 1 + (V.Y - CX) * ((TextureSize - 2) / YStep));
           TZ[I] := Round(((GY + 1) * TextureSize) + 1 - (V.X - CY) * ((TextureSize - 2) / XStep));
         End;
    270: Begin
           TZ[I] := Round(((GX + 1) * TextureSize) + 1 - (V.Y - CX) * ((TextureSize - 2) / YStep));
           TX[I] := Round(((GY + 1) * TextureSize) + 1 - (V.X - CY) * ((TextureSize - 2) / XStep));
         End;
    End; // Case
}
  End; // For I
End; // TPolygon.SetTexCoordsFromAngle

Function TPolygon.EqualVertices(P: TPolygon; MO,MO1: TMeshObject): Boolean;
Var
  I,V1,V2 : Integer;
  B       : Boolean;

Begin
  If High(Vertices) = High(P.Vertices) Then
  Begin
    B := True;
    I := 0;
    While (I <= High(Vertices)) And B Do
    Begin
      V1 := Vertices[I];
      V2 := P.Vertices[I];
      B := B And T3DPoint(MO.Vertices.Objects[V1]).Equals(T3DPoint(MO1.Vertices.Objects[V2]));
      Inc(I);
    End; // For I
    Result := B;
  End
  Else Result := False;
End; // TPolygon.EqualVertices

Procedure TPolygon.Add(Index: Integer);
Var I: LongInt;
Begin
  For I := 0 To High(Vertices) Do Inc(Vertices[I],Index);
End; // TPolygon.Add

Procedure TPolygon.GetIntersection(Mesh: TMeshObject; V1,V2: T3DPoint; P1,P2: Integer; U1,U2: Single; I: T3DPoint; Out _TX,_TZ: Single; Var _Color: TColor);
Var
  S           : Single;
  R1,G1,B1,A1 : Integer;
  R2,G2,B2,A2 : Integer;
  R3,G3,B3,A3 : Integer;

Begin
  U1 := Abs(U1);
  U2 := Abs(U2);
  If U1 + U2 = 0 Then
  Begin
    I.Copy(V1);
    If HasTexCoords Then
    Begin
      _TX := TX[P1];
      _TZ := TZ[P1];
    End
    Else
    Begin
      _TX := 0;
      _TZ := 0;
    End;
    If HasColor
     Then _Color := Colors[P1]
     Else _Color := clBlack;
  End
  Else
  Begin
    S    := U1 / (U1 + U2);
    I.X := V1.X + S * (V2.X - V1.X);
    I.Y := V1.Y + S * (V2.Y - V1.Y);
    I.Z := V1.Z + S * (V2.Z - V1.Z);
    If HasTexCoords Then
    Begin
      _TX := TX[P1] + S * (TX[P2] - TX[P1]);
      _TZ := TZ[P1] + S * (TZ[P2] - TZ[P1]);
    End
    Else
    Begin
      _TX := 0;
      _TZ := 0;
    End;
    If HasColor Then
    Begin
      R1 := TRGBA(Colors[P1]).R;
      G1 := TRGBA(Colors[P1]).G;
      B1 := TRGBA(Colors[P1]).B;
      A1 := TRGBA(Colors[P1]).A;

      R2 := TRGBA(Colors[P2]).R;
      G2 := TRGBA(Colors[P2]).G;
      B2 := TRGBA(Colors[P2]).B;
      A2 := TRGBA(Colors[P2]).A;

      R3 := Round(R1 + S * (R2 - R1));
      G3 := Round(G1 + S * (G2 - G1));
      B3 := Round(B1 + S * (B2 - B1));
      A3 := Round(A1 + S * (A2 - A1));

      If R3 < 0 Then R3 := 0;
      If G3 < 0 Then G3 := 0;
      If B3 < 0 Then B3 := 0;
      If A3 < 0 Then A3 := 0;

      If R3 > 255 Then R3 := 255;
      If G3 > 255 Then G3 := 255;
      If B3 > 255 Then B3 := 255;
      If A3 > 255 Then A3 := 255;

      TRGBA(_Color).R := R3;
      TRGBA(_Color).G := G3;
      TRGBA(_Color).B := B3;
      TRGBA(_Color).A := A3;
    End
    Else _Color := clBlack;
  End;
End; // GetIntersection

Procedure TPolygon.InsertVertex(Mesh: TMeshObject; V: T3DPoint; Index: Integer; _TX,_TZ: Single; _Color: TColor);
Var
  I,J    : Integer;
  V1,V2  : T3DPoint;
  N1,N2  : T3DPoint;
  V3     : T3DPoint;
  Dist,D : Single;

Begin
  Mesh.CalcNormals;
  Mesh.Vertices.AddObject('',T3DPoint.Create(V));
  SetLength(Vertices,High(Vertices) + 2);
  For I := High(Vertices) DownTo Index + 1 Do Vertices[I] := Vertices[I - 1];
  Vertices[Index] := Mesh.Vertices.Count - 1;

  // Insert the new texture coordinates

  If HasTexCoords Then
  Begin
    SetLength(TX,High(TX) + 2);
    SetLength(TZ,High(TZ) + 2);
    For I := High(TX) DownTo Index + 1 Do
    Begin
      TX[I] := TX[I - 1];
      TZ[I] := TZ[I - 1];
    End; // For I
    TX[Index] := _TX;
    TZ[Index] := _TZ;
  End;

  // Insert colors

  If HasColor Then
  Begin
    SetLength(Colors,High(Colors) + 2);
    For I := High(Colors) DownTo Index + 1 Do Colors[I] := Colors[I - 1];
    Colors[Index] := _Color;
  End;

  // Insert the new normal

  I  := Index - 1;
  If I < 0 Then I := High(Vertices);
  J  := (Index + 1) Mod (High(Vertices) + 1);
  V1 := T3DPoint(Mesh.Vertices.Objects[Vertices[I]]);
  V2 := T3DPoint(Mesh.Vertices.Objects[Vertices[J]]);
  N1 := T3DPoint(Mesh.Normals.Objects[Vertices[I]]);
  N2 := T3DPoint(Mesh.Normals.Objects[Vertices[J]]);

  V3 := T3DPoint.Create(V2);
  V3.Subtract(V1);
  Dist := V3.GetLength;

  V3.Copy(V);
  V3.Subtract(V1);
  D := V3.GetLength;

  If Dist <> 0 Then
  Begin
    V3.Copy(N2);
    V3.Subtract(N1);
    V3.Multiply(D / Dist);
    V3.Add(N1);
    Mesh.Normals.AddObject('',T3DPoint.Create(V3));
  End
  Else Mesh.Normals.AddObject('',T3DPoint.Create(N1));
  V3.Free;
End; // TPolygon.InsertVertex

// ------------------------------
// TElevationGrid
// ------------------------------

Constructor TElevationGrid.Create;
Begin
  Clear;
End; // TElevationGrid.Create

Destructor TElevationGrid.Destroy;
Begin
  Clear;
End; // TElevationGrid.Destroy

Procedure TElevationGrid.Clear;
Begin
  MinX := 0;
  MinY := 0;
  MinZ := 0;
  MaxX := 0;
  MaxY := 0;
  MaxZ := 0;
  NX   := 0;
  NY   := 0;
  SetLength(Heights,0);
  SetLength(Visible,0);
End; // TElevationGrid.Clear

Function TElevationGrid.GetHeight(X,Y: Integer): Single;
Begin
  If (X >= 0) And (X < NX) And
     (Y >= 0) And (Y < NY)
   Then Result := Heights[Y * NX + X]
   Else Result := 0;
End; // TElevationGrid.GetHeight

Procedure TElevationGrid.SetHeight(X,Y: Integer; S: Single);
Begin
  If (X >= 0) And (X < NX) And
     (Y >= 0) And (Y < NY) Then Heights[Y * NX + X] := S;
End; // TElevationGrid.SetHeight

Function TElevationGrid.IsVisible(X,Y: Integer): Boolean;
Begin
  If (X >= 0) And (X < NX - 1) And
     (Y >= 0) And (Y < NY - 1)
   Then Result := Visible[Y * (NX - 1) + X]
   Else Result := False;
End; // TElevationGrid.IsVisible

Function TElevationGrid.IsVisibleAtAbsolute(X,Y: Single): Boolean;
Const Step = GridSize;
Var X1,Y1: Integer;
Begin
  Result := False;
  If (X >= MinX) And (Y >= MinY) And (X <= MaxX) And (Y <= MaxY) Then
  Begin
    // Find out which ground mesh square we're interested in

    X1 := Trunc((MaxY - Y) / Step);
    Y1 := Trunc((MaxX - X) / Step);

    If (X1 >= 0) And (X1 < NX) And (Y1 >= 0) And (Y1 < NY) Then Result := IsVisible(X1,Y1);
  End;
End; // TElevationGrid.IsVisibleAtAbsolute

Procedure TElevationGrid.SetVisible(X,Y: Integer; B: Boolean);
Begin
  If (X >= 0) And (X < NX - 1) And
     (Y >= 0) And (Y < NY - 1) Then Visible[Y * (NX - 1) + X] := B;
End; // TElevationGrid.SetVisible

Function TElevationGrid.CanGetHeightAtAbsolute(X,Y: Single): Boolean;
Const Step = GridSize;
Var X1,Y1: Integer;
Begin
  Result := False;
  If (X >= MinX) And (Y >= MinY) And (X <= MaxX) And (Y <= MaxY) Then
  Begin
    // Find out which ground mesh square we're interested in

    X1 := Trunc((MaxY - Y) / Step);
    Y1 := Trunc((MaxX - X) / Step);

    If (X1 >= 0) And (X1 < NX) And (Y1 >= 0) And (Y1 < NY) Then Result := True;
  End;
End; // TElevationGrid.CanGetHeightAtAbsolute

Function TElevationGrid.GetHeightAtAbsolute(X,Y: Single): Single;
Const Step = GridSize;
Var
  E1,E2,E3,E4,E5,E6,E : Single;
  X1,Y1               : Integer;
  XR,YR               : Single;
  Index               : Integer;

Begin
  If (X >= MinX) And (Y >= MinY) And (X <= MaxX) And (Y <= MaxY) Then
  Begin
    // Find out which ground mesh square we're interested in

    X1 := Trunc((MaxY - Y) / Step);
    Y1 := Trunc((MaxX - X) / Step);

    If (X1 >= 0) And (X1 < NX) And (Y1 >= 0) And (Y1 < NY) Then
    Begin
      // Get the four elevation points at the corners of the square

      Index := Y1 * NX + X1;
      E1    := Heights[Index];
//      E1 := GetHeight(X1,Y1);
      If X1 < NX - 1 Then E2 := Heights[Index + 1]{GetHeight(X1 + 1,Y1)} Else E2 := E1;
      If Y1 < NY - 1 Then
      Begin
        E3 := Heights[Index + NX];//GetHeight(X1,Y1 + 1);
        If X1 < NX - 1 Then E4 := Heights[Index + NX + 1]{GetHeight(X1 + 1,Y1 + 1)} Else E4 := E3;
      End
      Else
      Begin
        E3 := E1;
        E4 := E2;
      End;
      XR := ((MaxY - (X1 * Step)) - Y) / Step;
      YR := ((MaxX - (Y1 * Step)) - X) / Step;

      // Each ground polygon is actually made of two triangles that generally aren't
      // coplanar.  The divider between them always runs from northwest to southeast.
      // This arrangement leaves one of the four elevation points out, so it has to
      // be adjusted based on which triangle the point intersects.

      If XR <= YR Then
      Begin
        E  := (E1 + E4) / 2;
        E2 := E - (E3 - E);
      End
      Else
      Begin
        E  := (E1 + E4) / 2;
        E3 := E - (E2 - E);
      End;

      E5     := E1 + (E2 - E1) * XR;
      E6     := E3 + (E4 - E3) * XR;
      Result := E5 + (E6 - E5) * YR;
    End
    Else Result := 0;
  End
  Else Result := 0;
End; // TElevationGrid.GetHeightAtAbsolute

Function TElevationGrid.GetFaceSWNormal(X,Y: Integer): T3DPoint;
Var
  P0,P1,P2 : T3DPoint;
  TS       : Integer;

Begin
  TS := GridSize;
  P0 := T3DPoint.Create(X * TS,(Y + 1) * TS,GetHeight(X,Y + 1));
  P1 := T3DPoint.Create(X * TS,Y * TS,GetHeight(X,Y));
  P2 := T3DPoint.Create((X + 1) * TS,(Y + 1) * TS,GetHeight(X + 1,Y + 1));
  P1.Subtract(P0);
  P2.Subtract(P0);
  P1.Cross(P2);
  P1.Normalize;
  P0.Free;
  P2.Free;
  Result := P1;
End; // TElevationGrid.GetFaceSWNormal

Function TElevationGrid.GetFaceNENormal(X,Y: Integer): T3DPoint;
Var
  P0,P1,P2 : T3DPoint;
  TS       : Integer;

Begin
  TS := GridSize;
  P0 := T3DPoint.Create((X + 1) * TS,Y * TS,GetHeight(X + 1,Y));
  P1 := T3DPoint.Create((X + 1) * TS,(Y + 1) * TS,GetHeight(X + 1,Y + 1));
  P2 := T3DPoint.Create(X * TS,Y * TS,GetHeight(X,Y));
  P1.Subtract(P0);
  P2.Subtract(P0);
  P1.Cross(P2);
  P1.Normalize;
  P0.Free;
  P2.Free;
  Result := P1;
End; // TElevationGrid.GetFaceNENormal

// ------------------------------
// TZone
// ------------------------------

Constructor TZone.Create;
Begin
  Inherited;
  ElevationGrid            := TElevationGrid.Create;
  DefaultLandTexture       := '';
  DefaultUnderwaterTexture := '';
  LongName                 := '';
  ShortName                := '';
  ZoneType                 := ztOutdoor;
  Bounds                   := TStringList.Create;
  ZonePlanes               := TStringList.Create;
  Sounds                   := TStringList.Create;
  ExtraMeshes              := TStringList.Create;
  Creatures                := TStringList.Create;
  Import3DSCache           := TStringList.Create;
  Import3DSCache.Sorted    := True;
  SetLength(Water,0);
End; // TZone.Create

Destructor TZone.Destroy;
Var I: LongInt;
Begin
  For I := 0 To High(Water) Do SetLength(Water[I].Irregular,0);
  SetLength(Water,0);
  For I := 0 To Count - 1 Do Objects[I].Free;
  ElevationGrid.Free;
  For I := 0 To Bounds.Count - 1 Do Bounds.Objects[I].Free;
  For I := 0 To ZonePlanes.Count - 1 Do ZonePlanes.Objects[I].Free;
  For I := 0 To Sounds.Count - 1 Do Sounds.Objects[I].Free;
  For I := 0 To Import3DSCache.Count - 1 Do Import3DSCache.Objects[I].Free;
  Bounds.Free;
  ZonePlanes.Free;
  Sounds.Free;
  ExtraMeshes.Free;
  Creatures.Free;
  Import3DSCache.Free;
  Inherited;
End; // TZone.Destroy

Procedure TZone.ShiftZone(X,Y,Z: Single);
Var
  I  : Integer;
  ZB : TZoneBound;
  ZP : TZonePlane;
  S  : TSound;
  ZO : TZoneObject;

Begin
  // Shift water

  For I := 0 To High(Water) Do
  Begin
    Water[I].MinX  := Water[I].MinX  + X;
    Water[I].MinY  := Water[I].MinY  + Y;
    Water[I].Level := Water[I].Level + Z;
  End; // For I

  // Shift bounds

  BoundMinZ := BoundMinZ + Z;
  BoundMaxZ := BoundMaxZ + Z;
  For I := 0 To Bounds.Count - 1 Do
  Begin
    ZB    := TZoneBound(Bounds.Objects[I]);
    ZB.X1 := ZB.X1 + X;
    ZB.Y1 := ZB.Y1 + Y;
    ZB.X2 := ZB.X2 + X;
    ZB.Y2 := ZB.Y2 + Y;
  End; // For I

  // Shift zone planes

  For I := 0 To ZonePlanes.Count - 1 Do
  Begin
    ZP    := TZonePlane(ZonePlanes.Objects[I]);
    ZP.X1 := ZP.X1 + X;
    ZP.Y1 := ZP.Y1 + Y;
    ZP.X2 := ZP.X2 + X;
    ZP.Y2 := ZP.Y2 + Y;
  End; // For I

  // Shift sounds

  For I := 0 To Sounds.Count - 1 Do
  Begin
    S   := TSound(Sounds.Objects[I]);
    S.X := S.X + X;
    S.Y := S.Y + Y;
    S.Z := S.Z + Z;
  End; // For I

  // Shift objects

  For I := 0 To Count - 1 Do
  Begin
    ZO       := TZoneObject(Objects[I]);
    ZO.Loc.X := ZO.Loc.X + X;
    ZO.Loc.Y := ZO.Loc.Y + Y;
    ZO.Loc.Z := ZO.Loc.Z + Z;
  End; // For I

  // Shift the elevation grid

  ElevationGrid.MinX := ElevationGrid.MinX + X;
  ElevationGrid.MinY := ElevationGrid.MinY + Y;
  ElevationGrid.MinZ := ElevationGrid.MinZ + Z;
  ElevationGrid.MaxX := ElevationGrid.MaxX + X;
  ElevationGrid.MaxY := ElevationGrid.MaxY + Y;
  ElevationGrid.MaxZ := ElevationGrid.MaxZ + Z;
  For I := 0 To High(ElevationGrid.Heights) Do ElevationGrid.Heights[I] := ElevationGrid.Heights[I] + Z;
End; // TZone.ShiftZone

Procedure TZone.SetDefaultLandTexture(St: String);
Begin
  DefaultLandTexture := St;
End; // TZone.SetDefaultLandTexture

Procedure TZone.SetDefaultUnderwaterTexture(St: String);
Begin
  DefaultUnderwaterTexture := St;
End; // TZone.SetDefaultUnderwaterTexture

Function TZone.GetDefaultLandTexture: String;
Begin
  Result := DefaultLandTexture;
End; // TZone.GetDefaultLandTexture

Function TZone.GetDefaultUnderwaterTexture: String;
Begin
  Result := DefaultUnderwaterTexture;
End; // TZone.GetDefaultUnderwaterTexture

Function TZone.NameExists(St: String): Boolean;
Var
  I     : Integer;
  ZO    : TZoneObject;
  Found : Boolean;

Begin
  St    := UpperCase(St);
  I     := 0;
  Found := False;
  While (I < Count) And Not Found Do
  Begin
    ZO := TZoneObject(Objects[I]);
    Found := Found Or ZO.NameExists(St,False);
    Inc(I);
  End; // While
  Result := Found;
End; // TZone.NameExists

Function TZone.FindObjectByName(St: String): TZoneObject;
Var
  I     : Integer;
  ZO    : TZoneObject;
  ZO1   : TZoneObject;

Begin
  I     := 0;
  St    := UpperCase(St);
  ZO1   := Nil;
  While (I < Count) And (ZO1 = Nil) Do
  Begin
    ZO  := TZoneObject(Objects[I]);
    ZO1 := ZO.FindObjectByName(St);
    Inc(I);
  End; // While
  Result := ZO1;
End; // TZone.FindObjectByName

Function TZone.AddObject(Const S: String; AObject: TObject): Integer;
Begin
  Result := Inherited AddObject(S,AObject);
  If AObject Is TZoneObject Then
  Begin
    TZoneObject(AObject).SetZone(Self);
    TZoneObject(AObject).SetParent(Nil);
  End;
End; // TZone.AddObject

Procedure TZone.InsertObject(Index: Integer; Const S: String; AObject: TObject);
Begin
  Inherited InsertObject(Index,S,AObject);
  If AObject Is TZoneObject Then
  Begin
    TZoneObject(AObject).SetZone(Self);
    TZoneObject(AObject).SetParent(Nil);
  End;
End; // TZone.InsertObject

Function TZone.CoalescePolygonList(List: TStringList): TMeshObject;
Var
  I,J,K,N  : LongInt;
  MO       : TMeshObject;
  M        : TMeshObject;
  P        : TPolygon;
  V        : T3DPoint;
  Remap    : Array Of LongInt;
//  L     : TStringList;
  St       : String;
  Index    : Integer;
  Normal   : T3DPoint;
  UseBones : Boolean;

Begin
  MO              := TMeshObject.Create;
//  L               := TStringList.Create;
//  L.Sorted        := True;
//  L.CaseSensitive := True;

  // First find out if any meshes use bone indices

  UseBones := False;
  I        := 0;
  While (I < List.Count) And Not UseBones Do
  Begin
    M := TMeshObject(List.Objects[I]);
    UseBones := (High(M.BoneIndices) >= 0);
    Inc(I);
  End; // While

  // If any meshes use bones then we need to set bone indices

  If UseBones Then
  Begin
    // Find out how many points there are

    J := 0;
    For I := 0 To List.Count - 1 Do
    Begin
      M := TMeshObject(List.Objects[I]);
      Inc(J,M.Vertices.Count);
    End; // For I
    SetLength(MO.BoneIndices,J);
    For I := 0 To J - 1 Do MO.BoneIndices[I] := -1;
  End;

  N := 0;
  For I := 0 To List.Count - 1 Do
  Begin
    M := TMeshObject(List.Objects[I]);

    // Copy all vertices, remapping duplicates
    // *** We don't want to remap duplicates or it screws up lots of things, like texture coordinates ***

    SetLength(Remap,M.Vertices.Count);
    If M.Normals.Count <> M.Vertices.Count Then M.CalcNormals;
    For J := 0 To M.Vertices.Count - 1 Do
    Begin
      V      := T3DPoint(M.Vertices.Objects[J]);
      Normal := T3DPoint(M.Normals.Objects[J]);
//      St     := V.GetID + Normal.GetID;
//      Index  := L.IndexOf(St);
//      If Index < 0 Then
//      Begin
        MO.Vertices.AddObject('',T3DPoint.Create(V));
        MO.Normals.AddObject('',T3DPoint.Create(Normal));
        K := MO.Vertices.Count - 1;
//        L.AddObject(St,Pointer(K));
//      End
//      Else K := Integer(L.Objects[Index]);
      Remap[J] := K;
    End; // For J

    If UseBones Then
    Begin
      For J := 0 To High(M.BoneIndices) Do MO.BoneIndices[N + J] := M.BoneIndices[J];
      Inc(N,M.Vertices.Count);
    End;

    // If the number of normals doesn't match the number of vertices in the resulting
    // mesh, then not all the source meshes had normals.  In this case, recalculate all
    // normals.

    If MO.Normals.Count <> MO.Vertices.Count Then MO.CalcNormals;

    // Copy the polygons, remapping as we go

    For J := 0 To M.Polygons.Count - 1 Do
    Begin
      P := TPolygon.Create(TPolygon(M.Polygons.Objects[J]));
      For K := 0 To High(P.Vertices) Do P.Vertices[K] := Remap[P.Vertices[K]];
      MO.Polygons.AddObject('',P);
    End; // For J
    SetLength(Remap,0);
  End; // For I
//  L.Free;
  Result := MO;
End; // TZone.CoalescePolygonList

Function TZone.BuildPolygonList(ExcludeMeshReferences,ExcludeGround,ExcludeInserts,SQLOnly: Boolean): TMeshObject;
Var
  I,J,K : LongInt;
  List  : TStringList;

Begin
  // Never allow inserts if we allow mesh references

  If Not ExcludeMeshReferences Then ExcludeInserts := True;

  List := TStringList.Create;
  J    := IndexOf(meshHeightMapGround);
  K    := IndexOf(meshHeightMapUnderwater);
  For I := 0 To Count - 1 Do
  Begin
    If frmStatus.Visible Then frmStatus.SetPosition(I / Count);
    If Not (Objects[I] Is TCreatureLibraryObjectReference) Then
    Begin
      If (Not ExcludeMeshReferences) Or (Not (Objects[I] Is TMeshLibraryObjectReference)) Or TMeshLibraryObjectReference(Objects[I]).InsertMesh Then
      Begin
        If ((I <> J) And (I <> K)) Or Not ExcludeGround Then TZoneObject(Objects[I]).AddToPolygonList(List,ExcludeMeshReferences,True,ExcludeInserts,True);
      End;
    End;
  End;
  If Not SQLOnly Then
  Begin
    // Blend the ground mesh normals to make them look more realistic

    If (J >= 0) And Not ExcludeGround Then
    Begin
      If frmStatus.Visible Then
      Begin
        frmStatus.SetCaption('Blending land ground area polygon normals');
        frmStatus.SetPosition(0);
      End;
      TMeshObject(List.Objects[J]).BlendNormals;
    End;

    If (K >= 0) And Not ExcludeGround Then
    Begin
      If frmStatus.Visible Then
      Begin
        frmStatus.SetCaption('Blending underwater ground area polygon normals');
        frmStatus.SetPosition(0);
      End;
      TMeshObject(List.Objects[K]).BlendNormals;
    End;

    // We now have a list of TMeshObjects.  Coalesce them into one TMeshObject.

    If frmStatus.Visible Then
    Begin
      frmStatus.SetCaption('Building polygon mesh');
      frmStatus.SetPosition(0);
    End;
    Result := CoalescePolygonList(List);
  End
  Else Result := Nil;

  // Get rid of the list

  For I := 0 To List.Count - 1 Do List.Objects[I].Free;
  List.Free;
End; // TZone.BuildPolygonList

Function TZone.GetObjectMeshCount: Integer;
Var
  I,J   : Integer;

  Function GetCount(Z: TZoneObject): Integer;
  Var
    I,J : Integer;
    GO  : TGroupObject;

  Begin
    I := 0;
    If Z Is TGroupObject Then
    Begin
      GO := TGroupObject(Z);
      For J := 0 To GO.Objects.Count - 1 Do Inc(I,GetCount(TZoneObject(GO.Objects.Objects[J])));
    End
    Else Inc(I);
    Result := I;
  End; // GetCount

Begin
  I := 0;
  For J := 0 To Count - 1 Do Inc(I,GetCount(TZoneObject(Objects[J])));
  Result := I;
End; // TZone.GetObjectMeshCount

Function TZone.GetZoneObject(I: Integer): TZoneObject;
// Gets the object based on absolute index (this will delve into groups to get the object)
Var
  J,K : Integer;
  ZO  : TZoneObject;

  Function GetInternalObject(Var Index: Integer; Z: TZoneObject): TZoneObject;
  Var
    J  : Integer;
    GO : TGroupObject;
    ZO : TZoneObject;

  Begin
    ZO := Nil;
    If Z Is TGroupObject Then
    Begin
      GO := TGroupObject(Z);
      J  := 0;
      While (J < GO.Objects.Count) And (ZO = Nil) Do
      Begin
        ZO := GetInternalObject(Index,TZoneObject(GO.Objects.Objects[J]));
        Inc(J);
      End; // While
    End
    Else If Index > 0 Then Dec(Index)
    Else ZO := Z;
    Result := ZO;
  End; // GetInternalObject

Begin
  ZO := Nil;
  K  := GetObjectMeshCount;
  If (I >= 0) And (I < K) Then
  Begin
    J := 0;
    While (J < Count) And (ZO = Nil) Do
    Begin
      ZO := GetInternalObject(I,TZoneObject(Objects[J]));
      Inc(J);
    End; // While
  End;
  Result := ZO;
End; // TZone.GetZoneObject

Procedure TZone.ImportCreature(Index: Integer);
// It takes too long to load every .an8 file in the creature library at program
// startup, to the loads are deferred until it's time to export zones.  This
// method is called at export time and gets the zone to load the library
// creature referenced by Index and put it into the master creature library as
// well as reference it.
Var
  An8File : TAn8File;
  Obj     : TAn8Object;
  I,J,K   : Integer;
  St      : String;

Begin
  If (Index >= 0) And (Index < Creatures.Count) And ((Creatures.Objects[Index] = Nil) Or Not TAn8File(Creatures.Objects[Index]).Loaded) Then
  Begin
    St := Creatures.Strings[Index];
    I  := Pos(',',St);
    If I > 0 Then St := Trim(Copy(St,I + 1,Length(St)));
    I := CreatureLibrary.IndexOf(St);
    If I >= 0 Then
    Begin
      If CreatureLibrary.Objects[I] <> Nil Then
      Begin
        An8File := TAn8File(CreatureLibrary.Objects[I]);
        An8File.LoadFromFile(ExtractFilePath(Application.EXEName) + 'library\creatures\' + St + '.an8');
        For K := 0 To An8File.Objects.Count - 1 Do
        Begin
          Obj := TAn8Object(An8File.Objects.Objects[K]);
          For J := 0 To Obj.Components.Count - 1 Do TAn8Component(Obj.Components.Objects[J]).ConvertToTriangles;
          Obj.MergeVeryClosePoints;
        End; // For K
        For K := 0 To An8File.Figures.Count - 1 Do TAn8Figure(An8File.Figures.Objects[K]).DeterminePrimaryBones;
      End;
      Creatures.Objects[Index] := CreatureLibrary.Objects[I];
    End;
  End;
End; // TZone.ImportCreature

Function TZone.BuildPolygonLists: TStringList;
Var
  I    : LongInt;
  List : TStringList;

Begin
  List := TStringList.Create;
  For I := 0 To Count - 1 Do TZoneObject(Objects[I]).AddToPolygonList(List,False,False,True,True);
  Result := List;
End; // TZone.BuildPolygonLists

Function TZone.BuildPolygonLists(ZO: TZoneObject): TStringList;
Var List: TStringList;
Begin
  List := TStringList.Create;
  ZO.AddToPolygonList(List,False,False,True,True);
  Result := List;
End; // TZone.BuildPolygonLists

Function TZone.GetObjectIndex(ZO: TZoneObject): Integer;
Var
  I,J   : Integer;
  Found : Boolean;

  Function Check(Z: TZoneObject; Var Index: Integer): Boolean;
  Var
    I  : Integer;
    GO : TGroupObject;
    B  : Boolean;

  Begin
    If Z <> ZO Then
    Begin
      B := False;
      If Z Is TGroupObject Then
      Begin
        GO := TGroupObject(Z);
        I  := 0;
        While (I < GO.Objects.Count) And Not B Do
        Begin
          B := B Or Check(TZoneObject(GO.Objects.Objects[I]),Index);
          Inc(I);
        End; // While
      End
      Else Inc(Index);
      Result := B;
    End
    Else Result := True;
  End; // Check

Begin
  I     := 0;
  J     := 0;
  Found := False;

  While (J < Count) And Not Found Do
  Begin
    Found := Check(TZoneObject(Objects[J]),I);
    If Not Found Then Inc(J);
  End; // While
//  While (J < Count) And Not Check(TZoneObject(Objects[J]),I) Do Inc(J);
  If Found Then Result := I Else Result := -1;
End; // TZone.GetObjectIndex

Procedure TZone.ImportGroundFrom3DSFile(FileName: String);
Var
  Model       : Model_3DS;
  I,J,K,L     : Integer;
  MO,MO1      : TMeshObject;
  Obj         : Object_3DS;
  P,Q1,Q2     : TPolygon;
  Index       : Integer;
  St          : String;
  Meshes      : TStringList;
  MinPt       : T3DPoint;
  MaxPt       : T3DPoint;
  X,Y         : Single;
  SX,SY       : Integer;
  Intersect   : T3DPoint;
  Found       : Boolean;
  P1,P2       : T3DPoint;
  V1,V2,V3    : T3DPoint;
  Textures    : TStringList;
  Z1,Z2,Z3,Z4 : Single;
  M           : T4x4Matrix;
  V           : T3DPoint;

Begin
  frmStatus.Show;
  Intersect := T3DPoint.Create;
  Meshes    := TStringList.Create;
  If FileExists(FileName) Then
  Begin
    Model := Model_3DS.Create;
    Model.Load(FileName);
    frmStatus.SetCaption('Importing ground .3DS file...Pass 1 of 5');
    For I := 0 To Model.NumObjects - 1 Do
    Begin
      frmStatus.SetPosition(I / Model.NumObjects);
      MO       := TMeshObject.Create;
      Obj      := Model.Objects[I];

      // Add all of the vertices

      // Now get the box bounds

      M := T4x4Matrix.Create(Obj.LocalMatrix[0,0],Obj.LocalMatrix[0,1],Obj.LocalMatrix[0,2],0,
                             Obj.LocalMatrix[1,0],Obj.LocalMatrix[1,1],Obj.LocalMatrix[1,2],0,
                             Obj.LocalMatrix[2,0],Obj.LocalMatrix[2,1],Obj.LocalMatrix[2,2],0,
                             Obj.LocalMatrix[3,0],Obj.LocalMatrix[3,1],Obj.LocalMatrix[3,2],1);
      M.Invert;
      For J := 0 To Obj.NumVerts - 1 Do
      Begin
        V := T3DPoint.Create(Obj.Vertexes[J].X,Obj.Vertexes[J].Y,Obj.Vertexes[J].Z);
        M.Multiply(V);
        MO.Vertices.AddObject('',V);
      End; // For J
      M.Free;

      // Process all of the faces

      For J := 0 To Obj.NumFaces - 1 Do
      Begin
        P := TPolygon.Create;
        SetLength(P.Vertices,3);
        SetLength(P.TX,3);
        SetLength(P.TZ,3);

        // Assume all triangles

        For K := 0 To 2 Do
        Begin
          // Have to reverse the vertex order

          P.Vertices[K]  := Obj.Faces[J].Index[2 - K];
          P.TX[K]        := Obj.TexCoords[P.Vertices[K]].TX;
          P.TZ[K]        := Obj.TexCoords[P.Vertices[K]].TZ;
          P.HasTexCoords := True;
          Index          := Obj.Faces[J].MatIndex;
          If Index >= 0 Then
          Begin
            // Have to strip off the path and extension

            St        := ExtractFileName(Model.Materials[Index].TexName);
            St        := Copy(St,1,Length(St) - Length(ExtractFileExt(St)));
            If TextureSet <> '' Then St := TextureSet + '\' + St;
            P.SetSingleTexture(St,'');
{
            If Model.Materials[Index].DiffuseSet Then
            Begin
              SetLength(P.Colors,High(P.Vertices) + 1);
              TRGBA(P.Colors[K]).R := Model.Materials[Index].Diffuse.R;
              TRGBA(P.Colors[K]).G := Model.Materials[Index].Diffuse.G;
              TRGBA(P.Colors[K]).B := Model.Materials[Index].Diffuse.B;
              TRGBA(P.Colors[K]).A := 255 - Model.Materials[Index].Diffuse.A;
              P.HasColor := True;
            End
            Else} If Model.Materials[Index].AmbientSet Then
            Begin
              If (Model.Materials[Index].Ambient.R <> 0) Or
                 (Model.Materials[Index].Ambient.G <> 0) Or
                 (Model.Materials[Index].Ambient.B <> 0) Then
              Begin
                SetLength(P.Colors,High(P.Vertices) + 1);
                TRGBA(P.Colors[K]).R := Model.Materials[Index].Ambient.R;
                TRGBA(P.Colors[K]).G := Model.Materials[Index].Ambient.G;
                TRGBA(P.Colors[K]).B := Model.Materials[Index].Ambient.B;
                TRGBA(P.Colors[K]).A := 255 - Model.Materials[Index].Ambient.A;
                P.HasColor := True;
              End;
            End;
          End;
        End; // For K
        MO.Polygons.AddObject('',P);
      End; // For J

      Meshes.AddObject('',MO);

      // We must *not* call TMeshObject.Coalesce or vertex normals get screwed up
      // (sharing vertices is BAD, BAD, BAD)
//      MO.Coalesce;
    End; // For I
    Model.Free;
  End;

  // We've loaded the .3DS file.  If there were any objects, coalesce them all into a single
  // ground mesh and build an elevation grid.

  If Meshes.Count > 0 Then
  Begin
    MO    := CoalescePolygonList(Meshes);
    MinPt := T3DPoint.Create;
    MaxPt := T3DPoint.Create;
    MO.ConvertToTriangles;
    MO.GetBounds(MinPt,MaxPt);

    ElevationGrid.Clear;
    ElevationGrid.MaxX := MaxPt.X;
    ElevationGrid.MaxY := MaxPt.Y;
    ElevationGrid.MaxZ := MaxPt.Z;
    ElevationGrid.MinZ := MinPt.Z;

    MaxPt.Subtract(MinPt);
    SX := GridSize;
    SY := GridSize;
    X  := MaxPt.Y / SX;
    Y  := MaxPt.X / SX;
    If Frac(X) <> 0
     Then ElevationGrid.NX := Trunc(X) + 1
     Else ElevationGrid.NX := Trunc(X);
    If Frac(Y) <> 0
     Then ElevationGrid.NY := Trunc(Y) + 1
     Else ElevationGrid.NY := Trunc(Y);
    ElevationGrid.MinX := ElevationGrid.MaxX - ElevationGrid.NY * SX;
    ElevationGrid.MinY := ElevationGrid.MaxY - ElevationGrid.NX * SX;
    Inc(ElevationGrid.NX);
    Inc(ElevationGrid.NY);
    SetLength(ElevationGrid.Heights,ElevationGrid.NX * ElevationGrid.NY);
    SetLength(ElevationGrid.Visible,(ElevationGrid.NX - 1) * (ElevationGrid.NY - 1));
    For I := 0 To High(ElevationGrid.Visible) Do ElevationGrid.Visible[I] := True;
    P1 := T3DPoint.Create;
    P2 := T3DPoint.Create;
    frmStatus.SetCaption('Importing ground .3DS file...Pass 2 of 5');
    For I := 0 To ElevationGrid.NX - 1 Do
    Begin
      frmStatus.SetPosition(I / (ElevationGrid.NX - 1));
      For J := 0 To ElevationGrid.NY - 1 Do
      Begin
        ElevationGrid.SetHeight(I,J,ElevationGrid.MinZ);
        K     := 0;
        Found := False;
        While (K < MO.Polygons.Count) And Not Found Do
        Begin
          P    := TPolygon(MO.Polygons.Objects[K]);
          V1   := T3DPoint(MO.Vertices.Objects[P.Vertices[0]]);
          V2   := T3DPoint(MO.Vertices.Objects[P.Vertices[1]]);
          V3   := T3DPoint(MO.Vertices.Objects[P.Vertices[2]]);
          P1.X := ElevationGrid.MaxX - J * SX;
          P1.Y := ElevationGrid.MaxY - I * SX;
          P1.Z := Min(Min(V1.Z,V2.Z),V3.Z) - 1;
          P2.X := P1.X;
          P2.Y := P1.Y;
          P2.Z := Max(Max(V1.Z,V2.Z),V3.Z) + 1;
          If LineFacet(P1,P2,V1,V2,V3,Intersect) Then Found := True Else Inc(K);
        End; // While
        If Found Then ElevationGrid.SetHeight(I,J,Intersect.Z);
      End; // For J
    End; // For I

    // Now determine the default land texture by choosing the texture that's most prevalent

    Textures        := TStringList.Create;
    Textures.Sorted := True;
    frmStatus.SetCaption('Importing ground .3DS file...Pass 3 of 5');
    For I := 0 To MO.Polygons.Count - 1 Do
    Begin
      frmStatus.SetPosition(I / (MO.Polygons.Count - 1));
      P  := TPolygon(MO.Polygons.Objects[I]);
      St := Trim(LowerCase(P.Texture));
      J  := Textures.IndexOf(St);
      If J < 0
       Then Textures.Add(St)
       Else Textures.Objects[J] := Pointer(LongInt(Textures.Objects[J]) + 1);
    End; // For I
    J := -1;
    L := -1;
    For I := 0 To Textures.Count - 1 Do
    Begin
      K := LongInt(Textures.Objects[I]);
      If K > J Then
      Begin
        J := K;
        L := I;
      End;
    End; // For I
    If L >= 0 Then DefaultLandTexture := Textures.Strings[L];
    Textures.Free;

    // Here's the tricky part: we need to generate a ground mesh that conforms to the elevation grid,
    // while preserving the textures in the original mesh as much as possible.  There doesn't seem to
    // be an easy way to do this, so we're going to use the texture that occupies the center of each
    // elevation square.

    // First generate a mesh that conforms to the elevation grid

    MO1 := TMeshObject.Create(meshHeightMapGround);
    K   := 0;
    frmStatus.SetCaption('Importing ground .3DS file...Pass 4 of 5');
    For J := 0 To ElevationGrid.NY - 2 Do
    Begin
      frmStatus.SetPosition(J / (ElevationGrid.NY - 2));
      For I := 0 To ElevationGrid.NX - 2 Do
      Begin
        Z1 := ElevationGrid.GetHeight(I,J);
        Z2 := ElevationGrid.GetHeight(I + 1,J);
        Z3 := ElevationGrid.GetHeight(I,J + 1);
        Z4 := ElevationGrid.GetHeight(I + 1,J + 1);

        // Start with triangles to save us a step later

        MO1.Vertices.AddObject('',T3DPoint.Create(ElevationGrid.MaxX - J * SX,       ElevationGrid.MaxY - I * SY,       Z1));
        MO1.Vertices.AddObject('',T3DPoint.Create(ElevationGrid.MaxX - J * SX,       ElevationGrid.MaxY - (I + 1) * SY, Z2));
        MO1.Vertices.AddObject('',T3DPoint.Create(ElevationGrid.MaxX - (J + 1) * SX, ElevationGrid.MaxY - (I + 1) * SY, Z4));
        P := TPolygon.Create([K,K + 1,K + 2],DefaultLandTexture);
        P.HasTexCoords := True;
        SetLength(P.TX,3);
        SetLength(P.TZ,3);
        P.TX[0] := I + 1 / TextureSize;
        P.TX[1] := I + 1 - 1 / TextureSize;
        P.TX[2] := P.TX[1];
        P.TZ[0] := J + 1 / TextureSize;
        P.TZ[1] := P.TZ[0];
        P.TZ[2] := J + 1 - 1 / TextureSize;
{
        P.TX[0] := Round(I       * TextureSize + 1);
        P.TX[1] := Round((I + 1) * TextureSize - 1);
        P.TX[2] := P.TX[1];
        P.TZ[0] := Round(J       * TextureSize + 1);
        P.TZ[1] := P.TZ[0];
        P.TZ[2] := Round((J + 1) * TextureSize - 1);
}

        // We're going to use Tag to contain the grid position.  It will be
        // automatically copied if the polygon is broken up.

        P.Tag    := J * (ElevationGrid.NX - 1) + I;
        P.HasTag := True;
        MO1.Polygons.AddObject('',P);
        Inc(K,3);

        MO1.Vertices.AddObject('',T3DPoint.Create(ElevationGrid.MaxX - J * SX,       ElevationGrid.MaxY - I * SY,       Z1));
        MO1.Vertices.AddObject('',T3DPoint.Create(ElevationGrid.MaxX - (J + 1) * SX, ElevationGrid.MaxY - (I + 1) * SY, Z4));
        MO1.Vertices.AddObject('',T3DPoint.Create(ElevationGrid.MaxX - (J + 1) * SX, ElevationGrid.MaxY - I * SY,       Z3));
        P := TPolygon.Create([K,K + 1,K + 2],DefaultLandTexture);
        P.HasTexCoords := True;
        SetLength(P.TX,3);
        SetLength(P.TZ,3);
        P.TX[0] := I + 1 / TextureSize;
        P.TX[1] := I + 1 - 1 / TextureSize;
        P.TX[2] := P.TX[0];
        P.TZ[0] := J + 1 / TextureSize;
        P.TZ[1] := J + 1 - 1 / TextureSize;
        P.TZ[2] := P.TZ[1];
{
        P.TX[0] := Round(I       * TextureSize + 1);
        P.TX[1] := Round((I + 1) * TextureSize - 1);
        P.TX[2] := P.TX[0];
        P.TZ[0] := Round(J       * TextureSize + 1);
        P.TZ[1] := Round((J + 1) * TextureSize - 1);
        P.TZ[2] := P.TZ[1];
}

        // We're going to use Tag to contain the grid position.  It will be
        // automatically copied if the polygon is broken up.

        P.Tag    := J * (ElevationGrid.NX - 1) + I;
        P.HasTag := True;
        MO1.Polygons.AddObject('',P);
        Inc(K,3);
      End; // For J
    End; // For I
    MO.CalcNormals;

    // Now change its textures to best match the ones in the original mesh

    L := 0;
    frmStatus.SetCaption('Importing ground .3DS file...Pass 5 of 5');
    For J := 0 To ElevationGrid.NY - 2 Do
    Begin
      frmStatus.SetPosition(J / (ElevationGrid.NY - 2));
      For I := 0 To ElevationGrid.NX - 2 Do
      Begin
        Q1        := TPolygon(MO1.Polygons.Objects[L]);
        Q2        := TPolygon(MO1.Polygons.Objects[L + 1]);
        K         := 0;
        Found     := False;
        P         := Nil;
        While (K < MO.Polygons.Count) And Not Found Do
        Begin
          P    := TPolygon(MO.Polygons.Objects[K]);
          V1   := T3DPoint(MO.Vertices.Objects[P.Vertices[0]]);
          V2   := T3DPoint(MO.Vertices.Objects[P.Vertices[1]]);
          V3   := T3DPoint(MO.Vertices.Objects[P.Vertices[2]]);
          P1.X := ElevationGrid.MaxX - J * SX - (SX Div 2);
          P1.Y := ElevationGrid.MaxY - I * SX - (SX Div 2);
          P1.Z := Min(Min(V1.Z,V2.Z),V3.Z) - 1;
          P2.X := P1.X;
          P2.Y := P1.Y;
          P2.Z := Max(Max(V1.Z,V2.Z),V3.Z) + 1;
          If LineFacet(P1,P2,V1,V2,V3,Intersect) Then Found := True Else Inc(K);
        End; // While
        If Found Then
        Begin
          Q1.Texture := P.Texture;
          Q2.Texture := P.Texture;
//          Q1.CopyTextureFromPolygon(P);
//          Q2.CopyTextureFromPolygon(P);
        End;
        Inc(L,2);
      End; // For I
    End; // For J

    // Add the new mesh

    AddObject(meshHeightMapGround,MO1);

    // Cleanup

    MO.Free;
    P1.Free;
    P2.Free;
    MinPt.Free;
    MaxPt.Free;
  End;
  frmStatus.Hide;
  For I := 0 To Meshes.Count - 1 Do Meshes.Objects[I].Free;
  Meshes.Free;
  Intersect.Free;
End; // TZone.ImportGroundFrom3DSFile

Function TZone.ImportObjectFrom3DSFile(FileName: String): TZoneObject;
Var
  Model    : Model_3DS;
  I,J,K,L  : Integer;
  MO       : TMeshObject;
  GO       : TGroupObject;
  Obj      : Object_3DS;
  P        : TPolygon;
  Index    : Integer;
  St       : String;
  St1      : String;
  V        : T3DPoint;
  M        : T4x4Matrix;
  Light    : TLightObject;
  X0,Y0,Z0 : Single;
  X1,Y1,Z1 : Single;
  X,Y,Z    : Single;
  Trans    : Integer;
  V1,V2,V3 : T3DPoint;
  Normal   : T3DPoint;
  CX,CY    : Single;
  Found    : Boolean;
  Center   : T3DPoint;

Begin
  Result := Nil;
  If FileExists(FileName) Then
  Begin
    Model := Model_3DS.Create;
    Model.Load(FileName);
    If Model.NumObjects > 1 Then
    Begin
      GO     := TGroupObject.Create;
      Result := GO;
    End;
    For I := 0 To Model.NumObjects - 1 Do
    Begin
      Obj := Model.Objects[I];

      St := UpperCase(Obj.Name);

      // Is this a "special" object that defines a water, lava, or PvP volume?

      If (Copy(St,1,8) = 'OZ_WATER') Or
         (Copy(St,1,7) = 'OZ_LAVA')  Or
         (Copy(St,1,6) = 'OZ_PVP')   Then
      Begin
        // If this is a water or lava object then we need to get the texture name from the top face

        Trans := 0;
        If (Copy(St,1,8) = 'OZ_WATER') Or
           (Copy(St,1,7) = 'OZ_LAVA')  Then
        Begin
          J   := 0;
          St1 := '';
          While (J < Obj.NumFaces) And (St1 = '') Do
          Begin
            K := Obj.Faces[J].MatIndex;

            // Have to strip off the path and extension

            If K >= 0 Then St1 := Trim(ExtractFileName(Model.Materials[K].TexName));
            If St1 <> '' Then Trans := Model.Materials[K].Transparency;
            Inc(J);
          End; // While
        End;

        // Now get the box bounds

        M := T4x4Matrix.Create(Obj.LocalMatrix[0,0],Obj.LocalMatrix[0,1],Obj.LocalMatrix[0,2],0,
                               Obj.LocalMatrix[1,0],Obj.LocalMatrix[1,1],Obj.LocalMatrix[1,2],0,
                               Obj.LocalMatrix[2,0],Obj.LocalMatrix[2,1],Obj.LocalMatrix[2,2],0,
                               Obj.LocalMatrix[3,0],Obj.LocalMatrix[3,1],Obj.LocalMatrix[3,2],1);
        M.Invert;
        V  := T3DPoint.Create;
        X0 := 0;
        Y0 := 0;
        Z0 := 0;
        X1 := 0;
        Y1 := 0;
        Z1 := 0;
        CX := 0;
        CY := 0;
        For J := 0 To Obj.NumFaces - 1 Do
        Begin
          For K := 0 To 2 Do
          Begin
            If (J = 0) And (K = 0) Then
            Begin
              L  := Obj.Faces[J].Index[2 - K];
              V.Copy(Obj.Vertexes[L].X,Obj.Vertexes[L].Y,Obj.Vertexes[L].Z);
              M.Multiply(V);
              X0 := V.X;
              Y0 := V.Y;
              Z0 := V.Z;
              X1 := X0;
              Y1 := Y0;
              Z1 := Z0;
            End
            Else
            Begin
              L  := Obj.Faces[J].Index[2 - K];
              V.Copy(Obj.Vertexes[L].X,Obj.Vertexes[L].Y,Obj.Vertexes[L].Z);
              M.Multiply(V);
              If V.X < X0 Then X0 := V.X;
              If V.Y < Y0 Then Y0 := V.Y;
              If V.Z < Z0 Then Z0 := V.Z;
              If V.X > X1 Then X1 := V.X;
              If V.Y > Y1 Then Y1 := V.Y;
              If V.Z > Z1 Then Z1 := V.Z;
            End;
            CX := CX + V.X;
            CY := CY + V.Y;
          End; // For K
        End; // For J
        If Obj.NumFaces > 0 Then
        Begin
          CX := CX / (Obj.NumFaces * 3);
          CY := CY / (Obj.NumFaces * 3);
        End;
        V.Free;

        // Create the region

        St1 := Copy(St1,1,Length(St1) - Length(ExtractFileExt(St1)));
        If (Copy(St,1,8) = 'OZ_WATER') And (St1 <> '') Then
        Begin
          SetLength(Water,High(Water) + 2);
          Water[High(Water)].Level      := Z1;
          Water[High(Water)].HasDepth   := True;
          Water[High(Water)].SemiTrans  := (Trans < 75);
          Water[High(Water)].WType      := wtWater;
          Water[High(Water)].Tinted     := False;
          Water[High(Water)].Color      := clBlack;
          Water[High(Water)].MinX       := X0;
          Water[High(Water)].MinY       := Y0;
          Water[High(Water)].XSize      := X1 - X0;
          Water[High(Water)].YSize      := Y1 - Y0;
          Water[High(Water)].Depth      := Z1 - Z0;
          Water[High(Water)].Shape      := wsRectangular;
          SetLength(Water[High(Water)].Irregular,0);
          SetLength(Water[High(Water)].Tex,1);
          Water[High(Water)].Tex[0]     := St1;
          Water[High(Water)].AnimTime   := DefaultAnimTimePerFrame;
        End
        Else If (Copy(St,1,7) = 'OZ_LAVA') And (St1 <> '') Then
        Begin
          SetLength(Water,High(Water) + 2);
          Water[High(Water)].Level      := Z1;
          Water[High(Water)].HasDepth   := True;
          Water[High(Water)].SemiTrans  := (Trans < 75);
          Water[High(Water)].WType      := wtLava;
          Water[High(Water)].Tinted     := False;
          Water[High(Water)].Color      := clBlack;
          Water[High(Water)].MinX       := X0;
          Water[High(Water)].MinY       := Y0;
          Water[High(Water)].XSize      := X1 - X0;
          Water[High(Water)].YSize      := Y1 - Y0;
          Water[High(Water)].Depth      := Z1 - Z0;
          Water[High(Water)].Shape      := wsRectangular;
          SetLength(Water[High(Water)].Irregular,0);
          SetLength(Water[High(Water)].Tex,1);
          Water[High(Water)].Tex[0]     := St1;
          Water[High(Water)].AnimTime   := DefaultAnimTimePerFrame;
        End
        Else If (Copy(St,1,7) = 'OZ_PVP') And (St1 <> '') Then
        Begin
          SetLength(Water,High(Water) + 2);
          Water[High(Water)].Level      := Z1;
          Water[High(Water)].HasDepth   := True;
          Water[High(Water)].SemiTrans  := (Trans < 75);
          Water[High(Water)].WType      := wtPvP;
          Water[High(Water)].Tinted     := False;
          Water[High(Water)].Color      := clBlack;
          Water[High(Water)].MinX       := X0;
          Water[High(Water)].MinY       := Y0;
          Water[High(Water)].XSize      := X1 - X0;
          Water[High(Water)].YSize      := Y1 - Y0;
          Water[High(Water)].Depth      := Z1 - Z0;
          Water[High(Water)].Shape      := wsRectangular;
          Water[High(Water)].AnimTime   := 0;
          SetLength(Water[High(Water)].Irregular,0);
          SetLength(Water[High(Water)].Tex,0);
        End
        Else If (Copy(St,1,8) = 'OZ_ICE') And (St1 <> '') Then
        Begin
          SetLength(Water,High(Water) + 2);
          Water[High(Water)].Level      := Z1;
          Water[High(Water)].HasDepth   := True;
          Water[High(Water)].SemiTrans  := (Trans < 75);
          Water[High(Water)].WType      := wtIce;
          Water[High(Water)].Tinted     := False;
          Water[High(Water)].Color      := clBlack;
          Water[High(Water)].MinX       := X0;
          Water[High(Water)].MinY       := Y0;
          Water[High(Water)].XSize      := X1 - X0;
          Water[High(Water)].YSize      := Y1 - Y0;
          Water[High(Water)].Depth      := Z1 - Z0;
          Water[High(Water)].Shape      := wsRectangular;
          SetLength(Water[High(Water)].Irregular,0);
          SetLength(Water[High(Water)].Tex,1);
          Water[High(Water)].Tex[0]     := St1;
          Water[High(Water)].AnimTime   := DefaultAnimTimePerFrame;
        End
        Else If (Copy(St,1,8) = 'OZ_ICEWATER') And (St1 <> '') Then
        Begin
          SetLength(Water,High(Water) + 2);
          Water[High(Water)].Level      := Z1;
          Water[High(Water)].HasDepth   := True;
          Water[High(Water)].SemiTrans  := (Trans < 75);
          Water[High(Water)].WType      := wtIceWater;
          Water[High(Water)].Tinted     := False;
          Water[High(Water)].Color      := clBlack;
          Water[High(Water)].MinX       := X0;
          Water[High(Water)].MinY       := Y0;
          Water[High(Water)].XSize      := X1 - X0;
          Water[High(Water)].YSize      := Y1 - Y0;
          Water[High(Water)].Depth      := Z1 - Z0;
          Water[High(Water)].Shape      := wsRectangular;
          SetLength(Water[High(Water)].Irregular,0);
          SetLength(Water[High(Water)].Tex,1);
          Water[High(Water)].Tex[0]     := St1;
          Water[High(Water)].AnimTime   := DefaultAnimTimePerFrame;
        End;
        If Water[High(Water)].Depth = 0 Then Water[High(Water)].HasDepth := False;

        Water[High(Water)].Shape := wsIrregular;

        V1     := T3DPoint.Create;
        V2     := T3DPoint.Create;
        V3     := T3DPoint.Create;
        Normal := T3DPoint.Create;
        Center := T3DPoint.Create(CX,CY,Z1);
        For J := 0 To Obj.NumFaces - 1 Do
        Begin
          L := Obj.Faces[J].Index[2]; V1.Copy(Obj.Vertexes[L].X,Obj.Vertexes[L].Y,Obj.Vertexes[L].Z);
          L := Obj.Faces[J].Index[1]; V2.Copy(Obj.Vertexes[L].X,Obj.Vertexes[L].Y,Obj.Vertexes[L].Z);
          L := Obj.Faces[J].Index[0]; V3.Copy(Obj.Vertexes[L].X,Obj.Vertexes[L].Y,Obj.Vertexes[L].Z);
          M.Multiply(V1);
          M.Multiply(V2);
          M.Multiply(V3);
          Normal.GetNormalTo(V1,V2,V3);
          If Abs(Normal.Z) < Max(Abs(Normal.X),Abs(Normal.Y)) Then
          Begin
            K := 0;
            Found := False;
            While (K <= High(Water[High(Water)].Irregular)) And Not Found Do
            Begin
              If Normal.Equals(Water[High(Water)].Irregular[K].NX,Water[High(Water)].Irregular[K].NY,Water[High(Water)].Irregular[K].NZ)
               Then Found := True
               Else Inc(K);
            End; // While
            If Not Found Then
            Begin
              SetLength(Water[High(Water)].Irregular,High(Water[High(Water)].Irregular) + 2);
              K := High(Water[High(Water)].Irregular);
              Water[High(Water)].Irregular[K].NX   := Normal.X;
              Water[High(Water)].Irregular[K].NY   := Normal.Y;
              Water[High(Water)].Irregular[K].NZ   := Normal.Z;
              Water[High(Water)].Irregular[K].Dist := GetHessianDistance(Normal,V1);
              If Abs(V1.Z - V2.Z) < Abs(V2.Z - V3.Z) Then
              Begin
                Normal.GetNormalTo(Center,V1,V2);
                If Normal.Z > 0 Then
                Begin
                  Water[High(Water)].Irregular[K].X1 := V2.X;
                  Water[High(Water)].Irregular[K].Y1 := V2.Y;
                  Water[High(Water)].Irregular[K].X2 := V1.X;
                  Water[High(Water)].Irregular[K].Y2 := V1.Y;
                End
                Else
                Begin
                  Water[High(Water)].Irregular[K].X1 := V1.X;
                  Water[High(Water)].Irregular[K].Y1 := V1.Y;
                  Water[High(Water)].Irregular[K].X2 := V2.X;
                  Water[High(Water)].Irregular[K].Y2 := V2.Y;
                End;
              End
              Else
              Begin
                Normal.GetNormalTo(Center,V2,V3);
                If Normal.Z > 0 Then
                Begin
                  Water[High(Water)].Irregular[K].X1 := V3.X;
                  Water[High(Water)].Irregular[K].Y1 := V3.Y;
                  Water[High(Water)].Irregular[K].X2 := V2.X;
                  Water[High(Water)].Irregular[K].Y2 := V2.Y;
                End
                Else
                Begin
                  Water[High(Water)].Irregular[K].X1 := V2.X;
                  Water[High(Water)].Irregular[K].Y1 := V2.Y;
                  Water[High(Water)].Irregular[K].X2 := V3.X;
                  Water[High(Water)].Irregular[K].Y2 := V3.Y;
                End;
              End;
            End;
          End;
        End; // For J
        V1.Free;
        V2.Free;
        V3.Free;
        Normal.Free;
        Center.Free;
        M.Free;
      End
      Else
      Begin
        // Process any light sources

        If Obj.HasLight Then
        Begin
          Light        := TLightObject.Create;
          Light.Radius := Abs(Obj.Light.OuterRange * Obj.Light.Multiplier);
          Light.Color  := TColor(LongWord(Obj.Light.Color) Xor $FF000000); // Set alpha to 0
          Light.Loc.X  := Obj.Light.Location.X;
          Light.Loc.Y  := Obj.Light.Location.Y;
          Light.Loc.Z  := Obj.Light.Location.Z;

          // Let's find a unique name for the object

          J  := 0;
          St := Trim(Obj.Name);
          If St = '' Then St := mesh3DSLight;
          While NameExists(St + IntToStr(J)) Do Inc(J);
          Light.FName := St + IntToStr(J);
          AddObject(St + IntToStr(J),Light);
        End;

        // Only create a mesh object if this object actually has faces

        If Obj.NumFaces > 0 Then
        Begin

          MO  := TMeshObject.Create;

          // Add all of the vertices

          M := T4x4Matrix.Create(Obj.LocalMatrix[0,0],Obj.LocalMatrix[0,1],Obj.LocalMatrix[0,2],0,
                                 Obj.LocalMatrix[1,0],Obj.LocalMatrix[1,1],Obj.LocalMatrix[1,2],0,
                                 Obj.LocalMatrix[2,0],Obj.LocalMatrix[2,1],Obj.LocalMatrix[2,2],0,
                                 Obj.LocalMatrix[3,0],Obj.LocalMatrix[3,1],Obj.LocalMatrix[3,2],1);
          M.Invert;
          For J := 0 To Obj.NumVerts - 1 Do
          Begin
            V := T3DPoint.Create(Obj.Vertexes[J].X,Obj.Vertexes[J].Y,Obj.Vertexes[J].Z);
            M.Multiply(V);
            MO.Vertices.AddObject('',V);

            V := T3DPoint.Create(Obj.Normals[J].X,Obj.Normals[J].Y,Obj.Normals[J].Z);
            MO.Normals.AddObject('',V);
          End; // For J
          M.Free;

          // Process all of the faces

          For J := 0 To Obj.NumFaces - 1 Do
          Begin
            P := TPolygon.Create;
            SetLength(P.Vertices,3);
            SetLength(P.TX,3);
            SetLength(P.TZ,3);

            // Assume all triangles

            For K := 0 To 2 Do
            Begin
              // Have to reverse the vertex order

              P.Vertices[K]  := Obj.Faces[J].Index[2 - K];
              P.TX[K]        := Obj.TexCoords[P.Vertices[K]].TX;
              P.TZ[K]        := Obj.TexCoords[P.Vertices[K]].TZ;
              P.HasTexCoords := True;
              Index          := Obj.Faces[J].MatIndex;
              If Index >= 0 Then
              Begin
                // Have to strip off the path and extension

                St := ExtractFileName(Model.Materials[Index].TexName);
                St := Copy(St,1,Length(St) - Length(ExtractFileExt(St)));
                If TextureSet <> '' Then St  := TextureSet + '\' + St;
                If Model.Materials[Index].OpacityMap Then
                Begin
                  St1 := ExtractFileName(Model.Materials[Index].OpacityMapName);
                  St1 := Copy(St1,1,Length(St1) - Length(ExtractFileExt(St1)));
                  If TextureSet <> '' Then St1 := TextureSet + '\' + St1;
                End
                Else St1 := '';
                P.SetSingleTexture(St,St1);
{                If Model.Materials[Index].DiffuseSet Then
                Begin
                  SetLength(P.Colors,High(P.Vertices) + 1);
                  TRGBA(P.Colors[K]).R := Model.Materials[Index].Diffuse.R;
                  TRGBA(P.Colors[K]).G := Model.Materials[Index].Diffuse.G;
                  TRGBA(P.Colors[K]).B := Model.Materials[Index].Diffuse.B;
                  TRGBA(P.Colors[K]).A := 255 - Model.Materials[Index].Diffuse.A;
                  P.HasColor := True;
                End
                Else} If Model.Materials[Index].AmbientSet Then
                Begin
                  If (Model.Materials[Index].Ambient.R <> 0) Or
                     (Model.Materials[Index].Ambient.G <> 0) Or
                     (Model.Materials[Index].Ambient.B <> 0) Then
                  Begin
                    SetLength(P.Colors,High(P.Vertices) + 1);
                    TRGBA(P.Colors[K]).R := Model.Materials[Index].Ambient.R;
                    TRGBA(P.Colors[K]).G := Model.Materials[Index].Ambient.G;
                    TRGBA(P.Colors[K]).B := Model.Materials[Index].Ambient.B;
                    TRGBA(P.Colors[K]).A := 255 - Model.Materials[Index].Ambient.A;
                    P.HasColor := True;
                  End;  
                End;
                If (Model.Materials[Index].Transparency > 25) And
                   (Model.Materials[Index].Transparency < 75) Then P.TextureState := tsSemiTransparent
                Else If Model.Materials[Index].Transparency >= 75 Then P.TextureState := tsTransparent;
              End;
            End; // For K
            MO.Polygons.AddObject('',P);
          End; // For J
  //        MO.CalcNormals;

          // Let's find a unique name for the object

          J  := 0;
          St := Trim(Obj.Name);
          If St = '' Then St := mesh3DSMesh;
          If NameExists(St) Then
          Begin
            While NameExists(St + IntToStr(J)) Do Inc(J);
            St := St + IntToStr(J);
          End;
          MO.FName := St;
          If Model.NumObjects > 1
           Then GO.Group(MO)
           Else Result := MO;
        End;
      End;

      // We must *not* call TMeshObject.Coalesce or vertex normals get screwed up
      // (sharing vertices is BAD, BAD, BAD)
//      MO.Coalesce;
    End; // For I

    J  := 0;
    St := ExtractFileNameNoExt(FileName);
    If St = '' Then St := mesh3DSMesh;
    If NameExists(St) Then
    Begin
      While NameExists(St + IntToStr(J)) Do Inc(J);
      St := St + IntToStr(J);
    End;
    Result.FName := St;

    Model.Free;
  End;
End; // TZone.ImportObjectFrom3DSFile

Procedure TZone.ImportFrom3DSFile(FileName: String);
Var
  I  : Integer;
  ZO : TZoneObject;
  St : String;

Begin
  ZO := ImportObjectFrom3DSFile(FileName);
  If ZO <> Nil Then
  Begin
    // Let's find a unique name for the object

    I  := 0;
    St := Trim(ExtractFileNameNoExt(FileName));
    If St = '' Then St := mesh3DSMesh;
    If NameExists(St) Then
    Begin
      While NameExists(St + IntToStr(I)) Do Inc(I);
      St := St + IntToStr(I);
    End;
    ZO.FName := St;
    AddObject(St,ZO);
  End;
End; // TZone.ImportFrom3DSFile

Function TZone.ExportTo3DSFile(FileName: String; TexList: TStringList): Integer;
Var
  Model      : Model_3DS;
  I,J,K      : Integer;
  Warnings   : Integer;
  List       : TStringList;
  FaceList   : TStringList;
  TexUsed    : TStringList;
  ZO         : TZoneObject;
  MO         : TMeshObject;
  Polygon    : TPolygon;
  V1         : T3DPoint;
  V2         : T3DPoint;
  V3         : T3DPoint;
  MinPt      : T3DPoint;
  MaxPt      : T3DPoint;
  Normal     : T3DPoint;
//  Textures   : String;
//  Opacities  : String;
//  Parameters : String;
  St         : String;

  Function ConvertSpacesToUnderscores(St: String): String;
  Var I: Integer;
  Begin
    I := Pos(' ',St);
    While I > 0 Do
    Begin
      St[I] := '_';
      I := Pos(' ',St);
    End; // While
    Result := St;
  End; // ConvertSpacesToUnderscores

  Procedure FillVertex(Var Vertex: Vector_3DS; P: T3DPoint);
  Begin
    Vertex.X := P.X;
    Vertex.Y := P.Y;
    Vertex.Z := P.Z;
  End; // FillVertex

Begin
  Warnings         := warnNone;
  TexUsed          := TStringList.Create;
  TexUsed.Sorted   := True;
  Normal           := T3DPoint.Create;
  Model            := Model_3DS.Create;
  Model.NumObjects := Count;
  SetLength(Model.Objects,Model.NumObjects);

  // Create a default material

  Model.NumMaterials := 2;
  SetLength(Model.Materials,Model.NumMaterials);

  Model.Materials[0].Name              := 'OZ_trans';
  Model.Materials[0].Textured          := False;
  Model.Materials[0].Diffuse.R         := 255;
  Model.Materials[0].Diffuse.G         := 255;
  Model.Materials[0].Diffuse.B         := 255;
  Model.Materials[0].Diffuse.A         := 255;
  Model.Materials[0].DiffuseSet        := True;
  Model.Materials[0].AmbientSet        := False;
  Model.Materials[0].SpecularSet       := False;
  Model.Materials[0].Shininess         := 0;
  Model.Materials[0].ShininessStrength := 0;
  Model.Materials[0].Transparency      := 100;

  Model.Materials[1].Name              := 'OZ_red';
  Model.Materials[1].Textured          := False;
  Model.Materials[1].Diffuse.R         := 255;
  Model.Materials[1].Diffuse.G         := 0;
  Model.Materials[1].Diffuse.B         := 0;
  Model.Materials[1].Diffuse.A         := 255;
  Model.Materials[1].DiffuseSet        := True;
  Model.Materials[1].AmbientSet        := False;
  Model.Materials[1].SpecularSet       := False;
  Model.Materials[1].Shininess         := 0;
  Model.Materials[1].ShininessStrength := 0;
  Model.Materials[1].Transparency      := 0;

  // Add a material for each texture used

  Inc(Model.NumMaterials,TexList.Count);
  SetLength(Model.Materials,Model.NumMaterials);
  For I := 0 To TexList.Count - 1 Do
  Begin
    Model.Materials[I + 2].Name              := 'tex' + IntToStr(I);
    Model.Materials[I + 2].TexName           := LowerCase(ExtractFileName(TexList.Strings[I]));
    Model.Materials[I + 2].Textured          := True;
    Model.Materials[I + 2].DiffuseSet        := True;
    Model.Materials[I + 2].AmbientSet        := False;
    Model.Materials[I + 2].SpecularSet       := False;
    Model.Materials[I + 2].Shininess         := 0;
    Model.Materials[I + 2].ShininessStrength := 0;
    Model.Materials[I + 2].Transparency      := 0;
    Model.Materials[I + 2].Diffuse.R         := 255;
    Model.Materials[I + 2].Diffuse.G         := 255;
    Model.Materials[I + 2].Diffuse.B         := 255;
    Model.Materials[I + 2].Diffuse.A         := 255;
    If Length(TexList.Strings[I]) > 8 Then Warnings := Warnings Or warnLFN;
  End; // For I

  List := TStringList.Create;
  For I := 0 To Count - 1 Do
  Begin
    ZO := TZoneObject(Objects[I]);
    ZO.AddToPolygonList(List,False,True,True,True);
    MO := CoalescePolygonList(List);
    MO.ConvertToTriangles;

    // Get the mesh bounds

    MinPt := T3DPoint.Create;
    MaxPt := T3DPoint.Create;
    MO.GetBounds(MinPt,MaxPt);

    Model.Objects[I].Name         := ConvertSpacesToUnderscores(ZO.GetName);
    Model.Objects[I].NumTexCoords := MO.Polygons.Count * 3;
    Model.Objects[I].Textured     := True;

    For J := 0 To 3 Do
     For K := 0 To 2 Do Model.Objects[I].LocalMatrix[J,K] := 0;
    Model.Objects[I].LocalMatrix[0,0] := 1;
    Model.Objects[I].LocalMatrix[1,1] := 1;
    Model.Objects[I].LocalMatrix[2,2] := 1;

    SetLength(Model.Objects[I].TexCoords,Model.Objects[I].NumTexCoords);
    SetLength(Model.Objects[I].FaceSmoothGroups,0);

    Model.Objects[I].NumFaces := MO.Polygons.Count;
    Model.Objects[I].NumVerts := MO.Polygons.Count * 3;
    SetLength(Model.Objects[I].Vertexes,Model.Objects[I].NumVerts);
    SetLength(Model.Objects[I].Normals,Model.Objects[I].NumVerts);
    SetLength(Model.Objects[I].Faces,Model.Objects[I].NumFaces);
    SetLength(Model.Objects[I].FaceNormals,Model.Objects[I].NumFaces);

    For J := 0 To MO.Polygons.Count - 1 Do
    Begin
      Polygon := TPolygon(MO.Polygons.Objects[J]);
      If Not Polygon.HasTexCoords Then MO.CalcTextureCoords(Polygon,MinPt,MaxPt);
      V1      := T3DPoint(MO.Vertices.Objects[Polygon.Vertices[2]]);
      V2      := T3DPoint(MO.Vertices.Objects[Polygon.Vertices[1]]);
      V3      := T3DPoint(MO.Vertices.Objects[Polygon.Vertices[0]]);

      // Load the vertices for the triangle

      FillVertex(Model.Objects[I].Vertexes[J * 3],    V1);
      FillVertex(Model.Objects[I].Vertexes[J * 3 + 1],V2);
      FillVertex(Model.Objects[I].Vertexes[J * 3 + 2],V3);

      // Load the texture coordinates

      For K := 0 To 2 Do
      Begin
        Model.Objects[I].TexCoords[J * 3 + K].TX := Polygon.TX[2 - K];
        Model.Objects[I].TexCoords[J * 3 + K].TZ := Polygon.TZ[2 - K];
      End; // For K

      // Calculate the normal

      Normal.GetNormalTo(V1,V2,V3);

      // Fill in the face normal

      FillVertex(Model.Objects[I].FaceNormals[J],Normal);

      // Fill in the vertex normals

      FillVertex(Model.Objects[I].Normals[J * 3],    Normal);
      FillVertex(Model.Objects[I].Normals[J * 3 + 1],Normal);
      FillVertex(Model.Objects[I].Normals[J * 3 + 2],Normal);

      // Fill in the face vertex indices

      Model.Objects[I].Faces[J].Index[0] := J * 3;
      Model.Objects[I].Faces[J].Index[1] := J * 3 + 1;
      Model.Objects[I].Faces[J].Index[2] := J * 3 + 2;

      If Polygon.TextureState = tsTransparent Then K := -2
      Else
      Begin
        // Fill in the face material indices

//        BreakupTextureString(Polygon.FTexture,Textures,Opacities,Parameters);
        K := TexList.IndexOf(LowerCase(ExtractFilePath(Application.ExeName) + 'library\textures\' + Polygon.TextureInfo.FirstTexture{GetToken(';',Textures)} + '.bmp'));
        If K < 0 Then
        Begin
          Warnings := Warnings Or warnNoTex;
          K := -1;
        End;
      End;
      Model.Objects[I].Faces[J].MatIndex := K + 2;
    End; // For J

    // Tally up all textures used in this object

    For J := 0 To Model.Objects[I].NumFaces - 1 Do
    Begin
           If Model.Objects[I].Faces[J].MatIndex > 1 Then St := TexList.Strings[Model.Objects[I].Faces[J].MatIndex - 2]
      Else If Model.Objects[I].Faces[J].MatIndex > 0 Then St := 'OZ_red'
      Else St := 'OZ_trans';
      K  := TexUsed.IndexOf(St);
      If K < 0 Then
      Begin
        FaceList := TStringList.Create;
        FaceList.AddObject('',Pointer(J));
        TexUsed.AddObject(St,FaceList);
      End
      Else
      Begin
        FaceList := TStringList(TexUsed.Objects[K]);
        FaceList.AddObject('',Pointer(J));
      End;
    End; // For J

    // Build a list of faces for each material used

    Model.Objects[I].NumMatFaces := TexUsed.Count;
    SetLength(Model.Objects[I].MatFaces,Model.Objects[I].NumMatFaces);
    For J := 0 To TexUsed.Count - 1 Do
    Begin
      FaceList := TStringList(TexUsed.Objects[J]);
           If TexUsed.Strings[J] = 'OZ_trans' Then Model.Objects[I].MatFaces[J].MatIndex := 0
      Else If TexUsed.Strings[J] = 'OZ_red' Then Model.Objects[I].MatFaces[J].MatIndex := 1
      Else Model.Objects[I].MatFaces[J].MatIndex := TexList.IndexOf(TexUsed.Strings[J]) + 2;
      Model.Objects[I].MatFaces[J].NumSubFaces := FaceList.Count;
      SetLength(Model.Objects[I].MatFaces[J].SubFaces,Model.Objects[I].MatFaces[J].NumSubFaces);
      For K := 0 To FaceList.Count - 1 Do
      Begin
        Model.Objects[I].MatFaces[J].SubFaces[K] := Model.Objects[I].Faces[Integer(FaceList.Objects[K])];
      End; // For K
    End; // For J

    MinPt.Free;
    MaxPt.Free;
    MO.Free;
    For J := 0 To List.Count - 1 Do List.Objects[J].Free;
    List.Clear;
    For J := 0 To TexUsed.Count - 1 Do TexUsed.Objects[J].Free;
    TexUsed.Clear;
  End; // For I
  Model.Save(FileName);

  // Cleanup

  Normal.Free;
  Model.Free;
  List.Free;
  TexUsed.Free;
  Result := Warnings;
End; // TZone.ExportTo3DSFile

Function TZone.ExportToAn8File(FileName: String; TexList: TStringList): Integer;
Var
  An8File    : TAn8File;
  F          : System.Text;
  I,J,K      : Integer;
  Texture    : TAn8Texture;
  Material   : TAn8Material;
  List       : TStringList;
  ZO         : TZoneObject;
  MO         : TMeshObject;
  MinPt      : T3DPoint;
  MaxPt      : T3DPoint;
  Obj        : TAn8Object;
  Mesh       : TAn8Mesh;
  Pt         : T3DPoint;
  Polygon    : TPolygon;
  Point      : Points3D.T3DPoint;
  Face       : TAn8Face;
  TexCoord   : TAn8TexCoord;
  Warnings   : Integer;
  TexUsed    : TStringList;
//  Textures   : String;
//  Opacities  : String;
//  Parameters : String;

Begin
  Warnings := warnNone;
  TexUsed  := TStringList.Create; // Must NOT be sorted

  An8File := TAn8File.Create;
  AssignFile(F,FileName);
  ReWrite(F);

  // Set up file header and environment

  An8File.Environment.Grid.I1       := 1;
  An8File.Environment.Grid.S2       := 10;
  An8File.Environment.Grid.S3       := 50;
  An8File.Environment.Grid.S4       := 50;
  An8File.Environment.FrameRate     := 10;
  An8File.Environment.LimitPlayback := True;

  // Export textures and materials

  Material                                         := TAn8Material.Create(An8File);
  Material.Name                                    := 'Unassigned';
  Material.Surface                                 := TAn8Surface.Create(An8File);
  Material.Surface.RGB                             := $FFFFFFFF;
  Material.Surface.LockAmbientDiffuse              := True;
  Material.Surface.Ambient                         := TAn8Color.Create(An8File, actAmbient);
  Material.Surface.Diffuse                         := TAn8Color.Create(An8File, actDiffuse);
  Material.Surface.Specular                        := TAn8Color.Create(An8File, actSpecular);
  Material.Surface.Emissive                        := TAn8Color.Create(An8File, actEmissive);
  Material.Surface.Ambient.RGB                     := $FFFFFFFF;
  Material.Surface.Diffuse.RGB                     := $FFFFFFFF;
  Material.Surface.Specular.RGB                    := 0;
  Material.Surface.Emissive.RGB                    := 0;
  Material.Surface.Ambient.Factor                  := 0.7;
  Material.Surface.Diffuse.Factor                  := 1;
  Material.Surface.Specular.Factor                 := 0.7;
  Material.Surface.Emissive.Factor                 := 0.7;
  An8File.Materials.AddObject(Material.Name,Material);

  Material                                         := TAn8Material.Create(An8File);
  Material.Name                                    := 'Transparent';
  Material.Surface                                 := TAn8Surface.Create(An8File);
  Material.Surface.RGB                             := $FFFFFFFF;
  Material.Surface.Alpha                           := 0;
  Material.Surface.LockAmbientDiffuse              := True;
  Material.Surface.Ambient                         := TAn8Color.Create(An8File, actAmbient);
  Material.Surface.Diffuse                         := TAn8Color.Create(An8File, actDiffuse);
  Material.Surface.Specular                        := TAn8Color.Create(An8File, actSpecular);
  Material.Surface.Emissive                        := TAn8Color.Create(An8File, actEmissive);
  Material.Surface.Ambient.RGB                     := $FFFFFFFF;
  Material.Surface.Diffuse.RGB                     := $FFFFFFFF;
  Material.Surface.Specular.RGB                    := 0;
  Material.Surface.Emissive.RGB                    := 0;
  Material.Surface.Ambient.Factor                  := 0.7;
  Material.Surface.Diffuse.Factor                  := 1;
  Material.Surface.Specular.Factor                 := 0.7;
  Material.Surface.Emissive.Factor                 := 0.7;
  Material.Surface.Diffuse.Texture                 := Nil;
  Material.Surface.Diffuse.TextureParams           := TAn8TextureParams.Create;
  Material.Surface.Diffuse.TextureParams.BlendMode := abmDecal;
  Material.Surface.Diffuse.TextureParams.AlphaMode := aamNone;
  Material.Surface.Diffuse.TextureParams.Percent   := 100;
  An8File.Materials.AddObject(Material.Name,Material);

  For I := 0 To TexList.Count - 1 Do
  Begin
    Texture          := TAn8Texture.Create(An8File);
    Texture.Name     := 'tex' + IntToStr(I);
    Texture.FileName := TexList.Strings[I];
    An8File.Textures.AddObject(Texture.Name,Texture);

    Material                                         := TAn8Material.Create(An8File);
    Material.Name                                    := 'mat' + IntToStr(I);
    Material.Surface                                 := TAn8Surface.Create(An8File);
    Material.Surface.RGB                             := $FFFFFFFF;
    Material.Surface.LockAmbientDiffuse              := True;
    Material.Surface.Ambient                         := TAn8Color.Create(An8File, actAmbient);
    Material.Surface.Diffuse                         := TAn8Color.Create(An8File, actDiffuse);
    Material.Surface.Specular                        := TAn8Color.Create(An8File, actSpecular);
    Material.Surface.Emissive                        := TAn8Color.Create(An8File, actEmissive);
    Material.Surface.Ambient.RGB                     := $FFFFFFFF;
    Material.Surface.Diffuse.RGB                     := $FFFFFFFF;
    Material.Surface.Specular.RGB                    := 0;
    Material.Surface.Emissive.RGB                    := 0;
    Material.Surface.Ambient.Factor                  := 0.7;
    Material.Surface.Diffuse.Factor                  := 1;
    Material.Surface.Specular.Factor                 := 0.7;
    Material.Surface.Emissive.Factor                 := 0.7;
    Material.Surface.Diffuse.Texture                 := Texture;
    Material.Surface.Diffuse.TextureParams           := TAn8TextureParams.Create;
    Material.Surface.Diffuse.TextureParams.BlendMode := abmDecal;
    Material.Surface.Diffuse.TextureParams.AlphaMode := aamNone;
    Material.Surface.Diffuse.TextureParams.Percent   := 100;
    An8File.Materials.AddObject(Material.Name,Material);

    If I = 0 Then TAn8Material(An8File.Materials.Objects[An8File.Materials.Count - 2]).Surface.Diffuse.Texture := Texture;
  End; // For I

  // Create a single Animor object for the entire zone

  Obj      := TAn8Object.Create(An8File);
  An8File.Objects.AddObject(Obj.Name,Obj);
  Obj.Name := 'zone';

  // Export objects

  List := TStringList.Create;
  For I := 0 To Count - 1 Do
  Begin
    ZO := TZoneObject(Objects[I]);
    ZO.AddToPolygonList(List,False,True,True,True);
    MO := CoalescePolygonList(List);
    MO.ConvertToTriangles;

    // Get the mesh bounds

    MinPt := T3DPoint.Create;
    MaxPt := T3DPoint.Create;
    MO.GetBounds(MinPt,MaxPt);

    // Create an object and mesh

    Mesh := TAn8Mesh.Create(An8File,ZO.GetName,Obj,Nil);
    Obj.Components.AddObject(Mesh.Name,Mesh);

    // Export vertices

    For J := 0 To MO.Vertices.Count - 1 Do
    Begin
      Point := Points3D.T3DPoint.Create;
      Pt    := T3DPoint(MO.Vertices.Objects[J]);
      Point.Copy(Pt.X,Pt.Y,Pt.Z);
      Mesh.Points.AddObject('',Point);
    End; // For J

    // Export polygons

    For J := 0 To MO.Polygons.Count - 1 Do
    Begin
      Polygon          := TPolygon(MO.Polygons.Objects[J]);
      If Not Polygon.HasTexCoords Then MO.CalcTextureCoords(Polygon,MinPt,MaxPt);
      Face             := TAn8Face.Create;
      Face.Flags       := 0;
      If High(Polygon.TX) = High(Polygon.Vertices) Then Face.Flags := 4;
      Face.NormalIndex := -1;
      SetLength(Face.Points,High(Polygon.Vertices) + 1);
      SetLength(Face.TexCoords,High(Polygon.TX) + 1);
      For K := 0 To High(Polygon.Vertices) Do Face.Points[K] := Polygon.Vertices[K];
      For K := 0 To High(Polygon.TX) Do
      Begin
        TexCoord := TAn8TexCoord.Create;
        TexCoord.TX := Polygon.TX[K];
        TexCoord.TZ := Polygon.TZ[K];
        Face.TexCoords[K] := Mesh.TexCoords.Count;
        Mesh.TexCoords.AddObject('',TexCoord);
      End; // For K

      // Set the face material

      If Polygon.TextureState = tsTransparent Then K := -2
      Else
      Begin
        // Fill in the face material indices

//        BreakupTextureString(Polygon.FTexture,Textures,Opacities,Parameters);
        K := TexList.IndexOf(LowerCase(ExtractFilePath(Application.ExeName) + 'library\textures\' + Polygon.TextureInfo.FirstTexture{GetToken(';',Textures)} + '.bmp'));
        If K < 0 Then
        Begin
          Warnings := Warnings Or warnNoTex;
          K := -1;
        End;
      End;
           If K >= 0 Then Face.Material := TAn8Material(An8File.Materials.Objects[K + 2])
      Else If K = -2 Then Face.Material := TAn8Material(An8File.Materials.Objects[1])
      Else Face.Material := TAn8Material(An8File.Materials.Objects[0]);

      // Remember if we used a material so we can make a master list later

      If (Face.Material <> Nil) And (TexUsed.IndexOf(Face.Material.Name) < 0) Then TexUsed.AddObject(Face.Material.Name,Face.Material);

      Mesh.Faces.AddObject('',Face);
    End; // For J

    // Make a material master list for the object

    For J := 0 To TexUsed.Count - 1 Do Mesh.MaterialList.AddObject(TexUsed.Strings[J],TexUsed.Objects[J]);
    If Mesh.MaterialList.Count > 0 Then Mesh.Material := TAn8Material(Mesh.MaterialList.Objects[0]);

    // Cleanup for the next object

    MinPt.Free;
    MaxPt.Free;
    MO.Free;
    For J := 0 To List.Count - 1 Do List.Objects[J].Free;
    List.Clear;
    TexUsed.Clear;
  End; // For I

  // Write to the file and close it

  An8File.WriteToFile(F);
  CloseFile(F);

  // Cleanup

  List.Free;
  TexUsed.Free;
  An8File.Free;
  Result := Warnings;
End; // TZone.ExportToAn8File

Procedure TZone.ExportToXWFFile(Stream: TStream; TexList,CharTexList: TStringList; ExportAsOctree: Boolean; CustomScale: Single);
Var
  I,J,K         : Integer;
  WldAtom       : TXWFAtom;
  Octree        : TXWFOctreeNode;
  BSPTree       : TXWFBSPTreeNode;
  ObjAtom       : TXWFIDAtom;
  ObjPAtom      : TXWFContainer;
  NameAtom      : TXWFHashAtom;
  LightsAtom    : TXWFContainer;
  Vertices      : Pointer;
  ObjLocs       : Pointer;
  Mesh          : TMeshObject;
  MinPt         : T3DPoint;
  MaxPt         : T3DPoint;
  Tree          : TTree;
  List          : TStringList;
  List1         : TStringList;
  OutTexList    : TStringList;
  ZO            : TZoneObject;
  ML            : TMeshLibraryObjectReference;
  CL            : TCreatureLibraryObjectReference;
  MLMesh        : TMeshObject;
  Hash          : TIntegerObjectHash;
  Enumeration   : TEnumeration;
  P             : Pointer;
  ZP            : TZonePlane;
  MeshType      : Array Of Boolean;
  MeshHash      : TIntegerStringHash;
  St            : String;
  LightTreeHash : TLightTreeHash;
  Center        : T3DPoint;
  RSize         : T3DPoint;
  BaseLoc       : T3DPoint;
  BaseHeading   : THeading;
  BaseScale     : T3DPoint;

  Procedure ProcessOctree(Region: TRegion; Octree: TXWFOctreeNode; TexList: TStringList);
  Var
    I,J,K      : Integer;
    R          : Array[0..7] Of TRegion;
    Child      : TXWFOctreeNode;
    PolyAtom   : TXWFGroupContainer;
    Polygons   : Pointer;
    P          : Pointer;
    Polygon    : TPolygon;
    TextureID  : Integer;
    Children   : TStringList;
    Textures   : String;
    Opacities  : String;
    Parameters : String;

  Begin
    Octree.Center[0]  := (Region.MinPt.X + Region.MaxPt.X) / 2;
    Octree.Center[1]  := (Region.MinPt.Y + Region.MaxPt.Y) / 2;
    Octree.Center[2]  := (Region.MinPt.Z + Region.MaxPt.Z) / 2;
    Octree.BoxSize[0] := (Region.MaxPt.X - Region.MinPt.X) / 2;
    Octree.BoxSize[1] := (Region.MaxPt.Y - Region.MinPt.Y) / 2;
    Octree.BoxSize[2] := (Region.MaxPt.Z - Region.MinPt.Z) / 2;

    If High(Region.Polygons) >= 0 Then
    Begin
      GetMem(Polygons,(High(Region.Polygons) + 1) * SizeOf(TXWFPolygonRec));
      P := Polygons;
      For I := 0 To High(Region.Polygons) Do
      Begin
        Polygon := TPolygon(Region.Mesh.Polygons.Objects[Region.Polygons[I]]);
        PXWFPolygonRec(P)^.V1 := LongWord(htonl(Polygon.Vertices[0]));
        PXWFPolygonRec(P)^.V2 := LongWord(htonl(Polygon.Vertices[1]));
        PXWFPolygonRec(P)^.V3 := LongWord(htonl(Polygon.Vertices[2]));
        J := TexList.IndexOf(Polygon.Texture);
        If J >= 0 Then TextureID := Integer(TexList.Objects[J]) Else TextureID := -1;
        K := 0;
        If TextureID >= 0 Then
        Begin
          PXWFPolygonRec(P)^.TextureID := LongWord(htonl(TextureID));
          Case Polygon.TextureState Of
            tsSolid:           K := 0;
            tsSemiTransparent: K := 4;
            tsTransparent:     K := 2;
          End; // Case
        End
        Else
        Begin
          PXWFPolygonRec(P)^.TextureID := LongWord(htonl(0));
          K := 2;// Set the transparent flag
        End;
        If Not Polygon.Solid Then K := K Or 1;
        PXWFPolygonRec(P)^.Flags := LongWord(htonl(K));
        Inc(LongWord(P),SizeOf(TXWFPolygonRec));
      End; // For I
      PolyAtom := TXWFGroupContainer.Create(fourccPolygonGroup,0,SizeOf(TXWFPolygonRec),High(Region.Polygons) + 1,Polygons);
      Octree.AddChild(PolyAtom);
      FreeMem(Polygons,(High(Region.Polygons) + 1) * SizeOf(TXWFPolygonRec));
    End;

    Children := TStringList.Create;
    R[0] := Region.GetOctreeChild(False,False,False);
    R[1] := Region.GetOctreeChild(False,False,True);
    R[2] := Region.GetOctreeChild(False,True, False);
    R[3] := Region.GetOctreeChild(False,True, True);
    R[4] := Region.GetOctreeChild(True, False,False);
    R[5] := Region.GetOctreeChild(True, False,True);
    R[6] := Region.GetOctreeChild(True, True, False);
    R[7] := Region.GetOctreeChild(True, True, True);
    For I := 0 To 7 Do
    Begin
      If (R[I] <> Nil) And (Children.IndexOfObject(R[I]) < 0) Then
      Begin
        Children.AddObject('',R[I]);
        Child := TXWFOctreeNode.Create;
        Octree.AddChild(Child);
        ProcessOctree(R[I],Child,TexList);
      End;
    End;
    Children.Free;
  End; // ProcessOctree

  Procedure ProcessBSPTree(Region: TRegion; BSPTree: TXWFBSPTreeNode; TexList: TStringList);
  Var
    I,J,K      : Integer;
    R          : Array[0..7] Of TRegion;
    Child      : TXWFBSPTreeNode;
    PolyAtom   : TXWFGroupContainer;
    Polygons   : Pointer;
    P          : Pointer;
    Polygon    : TPolygon;
    TextureID  : Integer;
    Textures   : String;
    Opacities  : String;
    Parameters : String;
    MinPt      : T3DPoint;
    MaxPt      : T3DPoint;

  Begin
    BSPTree.Normal[0] := Region.SplitNorm.X;
    BSPTree.Normal[1] := Region.SplitNorm.Y;
    BSPTree.Normal[2] := Region.SplitNorm.Z;
    BSPTree.Distance  := Region.SplitDist;
    Case Region.Attribute.Attr Of
      raNone: BSPTree.Add('type','normal');
     raWater: BSPTree.Add('type','water');
      raLava: BSPTree.Add('type','lava');
       raPVP: BSPTree.Add('type','pvp');
       raIce: BSPTree.Add('type','ice');
  raIceWater: BSPTree.Add('type','icewater');
  raZoneLine: Begin
                BSPTree.Add('type','zoneline');
                BSPTree.Add('dest_zone_id',IntToStr(Region.Attribute.Value + 1)); // Indices are one-based, not zero-based
              End;
    End; // Case

    MinPt := T3DPoint.Create;
    MaxPt := T3DPoint.Create;
    Region.BoundMesh.GetBounds(MinPt,MaxPt);
    BSPTree.Center[0] := (MinPt.X + MaxPt.X) / 2;
    BSPTree.Center[1] := (MinPt.Y + MaxPt.Y) / 2;
    BSPTree.Center[2] := (MinPt.Z + MaxPt.Z) / 2;
    BSPTree.Radius    := MinPt.DistanceFrom(MaxPt) / 2;
    MinPt.Free;
    MaxPt.Free;

    If High(Region.Polygons) >= 0 Then
    Begin
      GetMem(Polygons,(High(Region.Polygons) + 1) * SizeOf(TXWFPolygonRec));
      P := Polygons;
      For I := 0 To High(Region.Polygons) Do
      Begin
        Polygon := TPolygon(Region.Mesh.Polygons.Objects[Region.Polygons[I]]);
        PXWFPolygonRec(P)^.V1 := LongWord(htonl(Polygon.Vertices[0]));
        PXWFPolygonRec(P)^.V2 := LongWord(htonl(Polygon.Vertices[1]));
        PXWFPolygonRec(P)^.V3 := LongWord(htonl(Polygon.Vertices[2]));
        J := TexList.IndexOf(Polygon.Texture);
        If J >= 0 Then TextureID := Integer(TexList.Objects[J]) Else TextureID := -1;
        K := 0;
        If TextureID >= 0 Then
        Begin
          PXWFPolygonRec(P)^.TextureID := LongWord(htonl(TextureID));
          Case Polygon.TextureState Of
            tsSolid:           K := 0;
            tsSemiTransparent: K := 4;
            tsTransparent:     K := 2;
          End; // Case
        End
        Else
        Begin
          PXWFPolygonRec(P)^.TextureID := LongWord(htonl(0));
          K := 2;// Set the transparent flag
        End;
        If Not Polygon.Solid Then K := K Or 1;
        PXWFPolygonRec(P)^.Flags := LongWord(htonl(K));
        Inc(LongWord(P),SizeOf(TXWFPolygonRec));
      End; // For I
      PolyAtom := TXWFGroupContainer.Create(fourccPolygonGroup,0,SizeOf(TXWFPolygonRec),High(Region.Polygons) + 1,Polygons);
      BSPTree.AddChild(PolyAtom);
      FreeMem(Polygons,(High(Region.Polygons) + 1) * SizeOf(TXWFPolygonRec));
    End;

    If Region.Left <> Nil Then
    Begin
      Child := TXWFBSPTreeNode.Create;
      BSPTree.AddChild(Child);
      ProcessBSPTree(Region.Left,Child,TexList);
    End;
    If Region.Right <> Nil Then
    Begin
      Child := TXWFBSPTreeNode.Create;
      BSPTree.AddChild(Child);
      ProcessBSPTree(Region.Right,Child,TexList);
    End;
  End; // ProcessBSPTree

  Procedure AddPolygonAtoms(ParentAtom: TXWFAtom; Mesh: TMeshObject; TexList: TStringList);
  Var
    Polygons   : Pointer;
    P          : Pointer;
    I,J,K      : Integer;
    Polygon    : TPolygon;
    Textures   : String;
    Opacities  : String;
    Parameters : String;
    TextureID  : Integer;
    PolyAtom   : TXWFGroupContainer;

  Begin
    If Mesh.Polygons.Count > 0 Then
    Begin
      GetMem(Polygons,Mesh.Polygons.Count * SizeOf(TXWFPolygonRec));
      P := Polygons;
      For I := 0 To Mesh.Polygons.Count - 1 Do
      Begin
        Polygon := TPolygon(Mesh.Polygons.Objects[I]);
        PXWFPolygonRec(P)^.V1 := LongWord(htonl(Polygon.Vertices[0]));
        PXWFPolygonRec(P)^.V2 := LongWord(htonl(Polygon.Vertices[1]));
        PXWFPolygonRec(P)^.V3 := LongWord(htonl(Polygon.Vertices[2]));
        J := TexList.IndexOf(Polygon.Texture);
        If J >= 0 Then TextureID := Integer(TexList.Objects[J]) Else TextureID := -1;
        K := 0;
        If TextureID >= 0 Then
        Begin
          PXWFPolygonRec(P)^.TextureID := LongWord(htonl(TextureID));
          Case Polygon.TextureState Of
            tsSolid:           K := 0;
            tsSemiTransparent: K := 4;
            tsTransparent:     K := 2;
          End; // Case
        End
        Else
        Begin
          PXWFPolygonRec(P)^.TextureID := LongWord(htonl(0));
          K := 2; // Set the transparent flag
        End;
        If Not Polygon.Solid Then K := K Or 1;
        PXWFPolygonRec(P)^.Flags := LongWord(htonl(K));
        Inc(LongWord(P),SizeOf(TXWFPolygonRec));
      End; // For I
      PolyAtom := TXWFGroupContainer.Create(fourccPolygonGroup,0,SizeOf(TXWFPolygonRec),Mesh.Polygons.Count,Polygons);
      ParentAtom.AddChild(PolyAtom);
      FreeMem(Polygons,Mesh.Polygons.Count * SizeOf(TXWFPolygonRec));
    End;
  End; // AddPolygonAtoms

  Procedure AddTextureAtoms(ParentAtom: TXWFAtom; Mesh: TMeshObject; TexList,OutTexList: TStringList);
  Var
    I,J,K       : Integer;
    TexAtom     : TXWFHashAtom;
    Polygon     : TPolygon;
    St          : String;
//    Textures    : String;
//    Opacities   : String;
//    Parameters  : String;
//    TexTokens   : TTokenArray;
//    OpacTokens  : TTokenArray;
//    AnimTime    : Single;
    TextureInfo : TTextureInfo;

  Begin
    If frmStatus.Visible Then frmStatus.SetCaption('Adding texture atoms...');
    OutTexList.Clear;

    // Scan the mesh and add only those textures that are used.  This also allows
    // us to take animated textures into account.

    For I := 0 To Mesh.Polygons.Count - 1 Do
    Begin
      If frmStatus.Visible Then frmStatus.SetPosition(I / Mesh.Polygons.Count);
      Polygon := TPolygon(Mesh.Polygons.Objects[I]);
      St      := Polygon.Texture;
      If OutTexList.IndexOf(St) < 0 Then OutTexList.AddObject(St,Pointer(OutTexList.Count));
    End; // For I

    // Scan through the texture list we built and make texture atoms

    For I := 0 To OutTexList.Count - 1 Do
    Begin
      If frmStatus.Visible Then frmStatus.SetPosition(I / OutTexList.Count);
      TexAtom := TXWFHashAtom.Create(fourccTexture);
      TextureInfo := TTextureInfo.Create(OutTexList.Strings[I]);
//      BreakupTextureString(OutTexList.Strings[I],Textures,Opacities,Parameters);
//      GetTokens(';',Textures,TexTokens);
//      GetTokens(';',Opacities,OpacTokens);
//      If Parameters <> '' Then Val(Parameters,AnimTime,J);
      TexAtom.Add('numfiles',IntToStr(Max(TextureInfo.TextureMaps.Count{High(TexTokens) + 1},TextureInfo.OpacityMaps.Count{High(OpacTokens) + 1})));

      // The polygons only contain texture names with no path or extension.  Make sure we find the texture in the master
      // list, where the master list contains fully qualified names.

      For J := 0 To TextureInfo.TextureMaps.Count - 1 Do//High(TexTokens) Do
      Begin
        K := TexList.IndexOf(LowerCase(ExtractFilePath(Application.ExeName) + 'library\textures\' + TextureInfo.TextureMaps.Strings[J]{TexTokens[J]} + '.bmp'));
        If K >= 0 Then TexAtom.Add('texture_' + IntToStr(J),ExtractFileName(TexList.Strings[K])); // Name + extension, no path
      End; // For J
      For J := 0 To TextureInfo.OpacityMaps.Count - 1 Do//High(OpacTokens) Do
      Begin
        K := TexList.IndexOf(LowerCase(ExtractFilePath(Application.ExeName) + 'library\textures\' + TextureInfo.OpacityMaps.Strings[J]{OpacTokens[J]} + '.bmp'));
        If K >= 0 Then TexAtom.Add('opacity_' + IntToStr(J),ExtractFileName(TexList.Strings[K])); // Name + extension, no path
      End; // For J
      If (TextureInfo.TextureMaps.Count > 1) And (TextureInfo.AnimTime <> 0) Then TexAtom.Add('animtime',FloatToStr(TextureInfo.AnimTime));
//      If (Parameters <> '') And (AnimTime <> 0) And (High(TexTokens) > 0) Then TexAtom.Add('animtime',Parameters);
      TexAtom.ID := Integer(OutTexList.Objects[I]);
      ParentAtom.AddChild(TexAtom);
//      SetLength(TexTokens,0);
//      SetLength(OpacTokens,0);
      TextureInfo.Free;
    End; // For I
  End; // AddTextureAtoms

  Procedure AddVertexAtoms(ParentAtom: TXWFAtom; Mesh: TMeshObject);
  Var
    Vertices    : Pointer;
    P           : Pointer;
    I,J         : Integer;
    Vertex      : T3DPoint;
    Normal      : T3DPoint;
    Polygon     : TPolygon;
    Textures    : String;
    Opacities   : String;
    Parameters  : String;
    TextureID   : Integer;
    VertAtom    : TXWFGroupContainer;

  Begin
    Vertices := Nil;
    If Mesh.Vertices.Count > 0 Then
    Begin
      If frmStatus.Visible Then frmStatus.SetCaption('Building the vertex list...');
      GetMem(Vertices,Mesh.Vertices.Count * SizeOf(TXWFVertexRec));
      P := Vertices;
      For I := 0 To Mesh.Vertices.Count - 1 Do
      Begin
        If frmStatus.Visible Then frmStatus.SetPosition(I / Mesh.Vertices.Count);
        Vertex := T3DPoint(Mesh.Vertices.Objects[I]);
        Normal := T3DPoint(Mesh.Normals.Objects[I]);
        PXWFVertexRec(P)^.X := Vertex.X;
        PXWFVertexRec(P)^.Y := Vertex.Y;
        PXWFVertexRec(P)^.Z := Vertex.Z;
        PXWFVertexRec(P)^.I := Normal.X;
        PXWFVertexRec(P)^.J := Normal.Y;
        PXWFVertexRec(P)^.K := Normal.Z;
        PXWFVertexRec(P)^.U := 0;
        PXWFVertexRec(P)^.V := 0;
        Inc(LongWord(P),SizeOf(TXWFVertexRec));
      End; // For I

      // Fixup the texture coordinates, which are contained in the polygons

      If frmStatus.Visible Then frmStatus.SetCaption('Copying texture coordinates...');
      For I := 0 To Mesh.Polygons.Count - 1 Do
      Begin
        If frmStatus.Visible Then frmStatus.SetPosition(I / Mesh.Polygons.Count);
        Polygon := TPolygon(Mesh.Polygons.Objects[I]);
        If Not Polygon.HasTexCoords Then Mesh.CalcTextureCoords(Polygon,MinPt,MaxPt);
        If Polygon.HasTexCoords Then
        Begin
          For J := 0 To 2 Do
          Begin
            P := Vertices;
            Inc(LongWord(P),Polygon.Vertices[J] * SizeOf(TXWFVertexRec));
            PXWFVertexRec(P)^.U := Polygon.TX[J];
            PXWFVertexRec(P)^.V := Polygon.TZ[J];
          End; // For J
        End;
      End; // For I

      // Add the vertex atom

      VertAtom := TXWFGroupContainer.Create(fourccVertexGroup,0,SizeOf(TXWFVertexRec),Mesh.Vertices.Count,Vertices);
      ParentAtom.AddChild(VertAtom);
      If Mesh.Vertices.Count > 0 Then FreeMem(Vertices,Mesh.Vertices.Count * SizeOf(TXWFVertexRec));
    End;
  End; // AddVertexAtoms

  Procedure AddVertexColorAtoms(ParentAtom: TXWFAtom; Mesh: TMeshObject);
  Var
    I,J      : Integer;
    Polygon  : TPolygon;
    Vertices : Pointer;
    VertHash : TIntegerIntegerHash;
    VertAtom : TXWFGroupContainer;
    P        : PXWFVertexColorRec;
    A        : Single;

  Begin
    Vertices := Nil;
    If Mesh.Vertices.Count > 0 Then
    Begin
      If frmStatus.Visible Then frmStatus.SetCaption('Building the vertex color list...');

      // Not every vertex has a color.  Using the polygons, determine which ones do
      // and save the color for each.

      VertHash := TIntegerIntegerHash.Create(False);
      For I := 0 To Mesh.Polygons.Count - 1 Do
      Begin
        Polygon := TPolygon(Mesh.Polygons.Objects[I]);
        If Polygon.HasColor Then
        Begin
          For J := 0 To High(Polygon.Vertices) Do VertHash.Put(Polygon.Vertices[J],Polygon.Colors[J]); 
        End;
      End; // For I

      // If any vertices have colors applied to them, create an atom to contain them

      If VertHash.Count > 0 Then
      Begin
        GetMem(Vertices,VertHash.Count * SizeOf(TXWFVertexColorRec));
        P := Vertices;
        For I := 0 To VertHash.Count - 1 Do
        Begin
          P.VertexIndex    := htonl(VertHash.Items[I].Key.Int);
          J                := VertHash.Items[I].Value.AsInteger;
          A                := TRGBA(J).A / 255;
          TRGBA(P.Color).R := Min(255,Round(255 * A * (TRGBA(J).R / 255)));
          TRGBA(P.Color).G := Min(255,Round(255 * A * (TRGBA(J).G / 255)));
          TRGBA(P.Color).B := Min(255,Round(255 * A * (TRGBA(J).B / 255)));
          TRGBA(P.Color).A := 255;
          Inc(LongWord(P),SizeOf(TXWFVertexColorRec));
        End; // For I

        // Using the group ID to store the color type, which is always emissive in this case

        VertAtom := TXWFGroupContainer.Create(fourccVertexColors,colortypeEmissive,SizeOf(TXWFVertexColorRec),VertHash.Count,Vertices);
        ParentAtom.AddChild(VertAtom);
        FreeMem(Vertices,VertHash.Count * SizeOf(TXWFVertexColorRec));
      End;

      // Cleanup

      VertHash.Free;
    End;
  End; // AddVertexColorAtoms

  Procedure EncodeCreature(ParentAtom: TXWFAtom; An8: TAn8File; Const Model: String; CustomScale: Single);
  Const FigureIndex = 0;
  Var
    I,J,K         : Integer;
    ObjIndex      : Integer;
    CreatureAtom  : TXWFHashAtom;
    AltAtom       : TXWFHashAtom;
    ObjAtom       : TXWFIDAtom;
    ObjectList    : TStringList;
    NewMeshes     : TStringList;
    SeparateHeads : Boolean;
    BaseMeshCount : Integer;
    DZ            : Single;
    ML            : Single;
    HeadCount     : Integer;
    BodyCount     : Integer;
    Sequence      : TAn8Sequence;
    SeqName       : String;
    SeqPrefix     : String;
    DropLastFrame : Boolean;
    MSPerFrame    : Integer;
    Textures      : TStringList;
    Mesh          : TAn8Mesh;
    BodyMesh      : Integer;
    BodyTex       : Integer;
    HeadMesh      : Integer;
    HeadTex       : Integer;
    MeshTexHash   : TIntegerIntegerHash;
    Enumeration   : TEnumeration;

    Function GetEquipmentMeshType(Mesh: TAn8Mesh): TMeshType;
    Var
      St       : String;
      MeshType : TMeshType;
      Found    : Boolean;

    Begin
      St := Mesh.Name;
      St := LowerCase(Copy(St,11,Length(St)));  // Strip off 'equipment_'
      MeshType := Low(TMeshType);
      Found    := False;
      While (MeshType <= High(TMeshType)) And Not Found Do
      Begin
        If St = MeshTypeID[MeshType]
         Then Found    := True
         Else MeshType := Succ(MeshType);
      End; // While
      If Found
       Then Result := MeshType
       Else Result := mtEquipment; 
    End; // GetEquipmentMeshType

    Procedure AddBoneAtoms(ParentAtom: TXWFAtom; Bone: TAn8Bone; DZ: Single);
    Var
      BoneAtom : TXWFBoneAtom;
      S        : Single;
      VX,VY,VZ : Single;
      I        : Integer;
      Frame    : TXWFBoneFrameRec;
      St       : String;

    Begin
      BoneAtom := TXWFBoneAtom.Create;
      VX := 0;
      VY := 1;
      VZ := 0;
      St := UpperCase(Bone.Name);
      If Bone.Index <> 0 Then
      Begin
        BoneAtom.Add('name',St + '_TRACK');
        S := Bone.Parent.Len * CustomScale;
        If Bone.Index = 1 Then VY := 0;
      End
      Else
      Begin
        BoneAtom.Add('name','');
        S := -DZ * CustomScale;
      End;
      If Copy(St,Length(St) - 5,6) = '_POINT'
       Then BoneAtom.Add('point','true')
       Else BoneAtom.Add('point','false');
      BoneAtom.ID          := Bone.Index;
      Frame.Orientation[0] := Bone.Orientation.W;
      Frame.Orientation[1] := Bone.Orientation.Z;
      Frame.Orientation[2] := Bone.Orientation.X;
      Frame.Orientation[3] := Bone.Orientation.Y;
      Frame.Position[0]    := VZ * S;
      Frame.Position[1]    := VX * S;
      Frame.Position[2]    := VY * S;
      BoneAtom.AddFrame(Frame);
      For I := 0 To Bone.Children.Count - 1 Do AddBoneAtoms(BoneAtom,TAn8Bone(Bone.Children.Objects[I]),DZ);
      ParentAtom.AddChild(BoneAtom);
    End; // AddBoneAtoms

    Procedure AddSequenceBoneAtoms(ParentAtom: TXWFAtom; Bone: TAn8Bone; DZ: Single; Sequence: TAn8Sequence;
                                   DropLastFrame: Boolean; MSPerFrame: Integer);
    Var
      BoneAtom    : TXWFBoneAtom;
      S           : Single;
      VX,VY,VZ    : Single;
      I,J,K,L     : Integer;
      St          : String;
      Frame       : TXWFBoneFrameRec;
      Q           : TQuaternion;
      Q1          : TQuaternion;
      V           : Points3D.T3DPoint;
      JointAngleX : TAn8JointAngle;
      JointAngleY : TAn8JointAngle;
      JointAngleZ : TAn8JointAngle;

    Begin
      If Bone.Parent <> Nil
//      If I > 0
       Then St := UpperCase(Bone.Name)
       Else St := '';

      // Don't show bones for holding items, showing the creature's name, etc.

      If Copy(St,Length(St) - 5,6) <> '_POINT' Then
      Begin
        If DropLastFrame
         Then J := Max(1,Sequence.Frames - 1)
         Else J := Sequence.Frames;

        // Create a basic quaternion, a work quaternion, and a work point

        Q  := TQuaternion.Create;
        Q1 := TQuaternion.Create;
        V  := Points3D.T3DPoint.Create;

        // Find the joint angles

        L := Sequence.JointAngles.IndexOf(Bone.Name + '_X');
        If L >= 0
         Then JointAngleX := TAn8JointAngle(Sequence.JointAngles.Objects[L])
         Else JointAngleX := Nil;

        L := Sequence.JointAngles.IndexOf(Bone.Name + '_Y');
        If L >= 0
         Then JointAngleY := TAn8JointAngle(Sequence.JointAngles.Objects[L])
         Else JointAngleY := Nil;

        L := Sequence.JointAngles.IndexOf(Bone.Name + '_Z');
        If L >= 0
         Then JointAngleZ := TAn8JointAngle(Sequence.JointAngles.Objects[L])
         Else JointAngleZ := Nil;

        If ((JointAngleX = Nil) Or ((JointAngleX.Track.Keys.Count = 1) And (TAn8FloatKey(JointAngleX.Track.Keys.Objects[0]).Frame = 0))) And
           ((JointAngleY = Nil) Or ((JointAngleY.Track.Keys.Count = 1) And (TAn8FloatKey(JointAngleY.Track.Keys.Objects[0]).Frame = 0))) And
           ((JointAngleZ = Nil) Or ((JointAngleZ.Track.Keys.Count = 1) And (TAn8FloatKey(JointAngleZ.Track.Keys.Objects[0]).Frame = 0))) Then J := 1;

        BoneAtom := TXWFBoneAtom.Create;
        VX := 0;
        VY := 1;
        VZ := 0;
        BoneAtom.MSPerFrame := MSPerFrame;
        If Bone.Index <> 0 Then
        Begin
          BoneAtom.Add('name',UpperCase(Bone.Name) + '_TRACK');
          S := Bone.Parent.Len * CustomScale;
          If Bone.Index = 1 Then VY := 0;
        End
        Else
        Begin
          BoneAtom.Add('name','');
          S := 0;//-DZ * CustomScale;
        End;
        If Copy(St,Length(St) - 5,6) = '_POINT'
         Then BoneAtom.Add('point','true')
         Else BoneAtom.Add('point','false');
        BoneAtom.ID := Bone.Index;

        // Loop through the frames

        For K := 0 To J - 1 Do
        Begin
          // Look for the X, Y, Z components of the adjustment to this bone

          Q.Copy(Bone.Orientation);

          If JointAngleX <> Nil Then
          Begin
            V.Copy(1,0,0);
            Q.Transform(V);
            Q1.FromAngleAxis(JointAngleX.Track.GetValueForFrame(K) * Pi / 180,V);
            Q1.Multiply(Q);
            Q.Copy(Q1);
          End;

          If JointAngleZ <> Nil Then
          Begin
            V.Copy(0,0,1);
            Q.Transform(V);
            Q1.FromAngleAxis(JointAngleZ.Track.GetValueForFrame(K) * Pi / 180,V);
            Q1.Multiply(Q);
            Q.Copy(Q1);
          End;

          If JointAngleY <> Nil Then
          Begin
            V.Copy(0,1,0);
            Q.Transform(V);
            Q1.FromAngleAxis(JointAngleY.Track.GetValueForFrame(K) * Pi / 180,V);
            Q1.Multiply(Q);
            Q.Copy(Q1);
          End;

          Frame.Orientation[0] := Q.W;
          Frame.Orientation[1] := Q.Z;
          Frame.Orientation[2] := Q.X;
          Frame.Orientation[3] := Q.Y;
          Frame.Position[0]    := VZ * S;
          Frame.Position[1]    := VX * S;
          Frame.Position[2]    := VY * S;

          BoneAtom.AddFrame(Frame);
        End; // For K
        V.Free;
        Q1.Free;
        Q.Free;
        For I := 0 To Bone.Children.Count - 1 Do AddSequenceBoneAtoms(BoneAtom,TAn8Bone(Bone.Children.Objects[I]),DZ,Sequence,DropLastFrame,MSPerFrame);
        ParentAtom.AddChild(BoneAtom);
      End;
    End; // AddSequenceBoneAtoms

    Procedure AddAnimation(ParentAtom: TXWFAtom; Bone: TAn8Bone; Sequence: TAn8Sequence; DZ,ModelMaxLen: Single);
    Var
      I             : Integer;
      AnimAtom      : TXWFHashAtom;
      SeqName       : String;
      SeqPrefix     : String;
      DropLastFrame : Boolean;
      MSPerFrame    : Integer;

    Begin
      AnimAtom := TXWFHashAtom.Create(fourccAnimation);
      If Sequence = Nil Then
      Begin
        AddBoneAtoms(AnimAtom,Bone,DZ);
        AnimAtom.Add('prefix','');
        AnimAtom.Add('maxlen',FloatToStr(ModelMaxLen * CustomScale));
      End
      Else
      Begin
        TokenizeAn8SequenceName(Sequence.Name,SeqName,SeqPrefix,DropLastFrame,MSPerFrame);
        AnimAtom.Add('prefix',UpperCase(SeqPrefix));
        AnimAtom.Add('maxlen',FloatToStr(ModelMaxLen * CustomScale));
        AddSequenceBoneAtoms(AnimAtom,Bone,DZ,Sequence,DropLastFrame,MSPerFrame);
      End;
      ParentAtom.AddChild(AnimAtom);
    End; // AddAnimation

    Procedure AddVertexAtoms(ParentAtom: TXWFAtom; Mesh: TAn8Mesh);
    Var
      Vertices   : Pointer;
      P          : Pointer;
      I,J,K      : Integer;
      Bone       : TAn8Bone;
      Bone1      : TAn8Bone;
      Point      : Points3D.T3DPoint;
      Normal     : Points3D.T3DPoint;
      VertAtom   : TXWFGroupContainer;
      Q          : TQuaternion;
      Q1         : TQuaternion;
      V          : Points3D.T3DPoint;
      NO         : TAn8NamedObject;
      Face       : TAn8Face;
      St         : String;
      VertSorter : TQuickSorterProc;

      Function CompareAn8Vertices(Index0,Index1: Integer): Integer;

        Function GetVertIndex(Index: Integer): Integer;
        Var
          I    : Integer;
          Face : TAn8Face;

        Begin
          I      := Integer(VertTexList.Objects[VertIndex[Index]]);
          Face   := TAn8Face(SortAn8Mesh.Faces.Objects[(I And $7FFFFFFF) Shr 2]);
          Result := TAn8Bone(SortAn8Mesh.PrimaryBones.Objects[Face.Points[I And 3]]).Index;
        End; // GetVertIndex

      Begin
        Result := GetVertIndex(Index0) - GetVertIndex(Index1);
      End; // CompareAn8Vertices

      Procedure ExchangeAn8Vertices(Index0,Index1: Integer);
      Var I: Integer;
      Begin
        I                 := VertIndex[Index0];
        VertIndex[Index0] := VertIndex[Index1];
        VertIndex[Index1] := I;
      End; // ExchangeAn8Vertices

    Begin
      Vertices := Nil;
      VertTexList.Clear;
      SetLength(VertIndex,0);

      If Mesh.Points.Count > 0 Then
      Begin
        // Allocate some helpful objects

        Q  := TQuaternion.Create;
        Q1 := TQuaternion.Create;
        V  := Points3D.T3DPoint.Create;
        NO := TAn8NamedObject(TAn8Figure(An8.Figures.Objects[FigureIndex]).RootBone.FindNamedObjects.Objects[0]);

        // Build the unique vertex-texture coordinate list

        VertTexList.Clear;
        For I := 0 To Mesh.Faces.Count - 1 Do
        Begin
          Face := TAn8Face(Mesh.Faces.Objects[I]);
          If High(Face.TexCoords) = High(Face.Points) Then
          Begin
            For J := 0 To High(Face.Points) Do
            Begin
              St := IntToStr(Face.Points[J]) + ',' + IntToStr(Face.TexCoords[J]);
              If VertTexList.IndexOf(St) < 0 Then VertTexList.AddObject(St,Pointer((I Shl 2) + J));
            End; // For J
          End
          Else
          Begin
            For J := 0 To High(Face.Points) Do
            Begin
              St := IntToStr(Face.Points[J]);
              If VertTexList.IndexOf(St) < 0 Then VertTexList.AddObject(St,Pointer(((I Shl 2) + J) Or $80000000));
            End; // For J
          End;
        End; // For I

        // Initialize the vertex index list

        SetLength(VertIndex,VertTexList.Count);
        For J := 0 To High(VertIndex) Do VertIndex[J] := J;

        // Sort the vertices by bone

        SortAn8Mesh         := Mesh;
        VertSorter          := TQuickSorterProc.Create;
        VertSorter.Compare  := @CompareAn8Vertices;
        VertSorter.Exchange := @ExchangeAn8Vertices;
        VertSorter.Sort(0,High(VertIndex));
        VertSorter.Free;

        // Allocate an array of vertices

        GetMem(Vertices,VertTexList.Count * SizeOf(TXWFVertexRec));
        P := Vertices;

        For K := 0 To VertTexList.Count - 1 Do
        Begin
          J    := Integer(VertTexList.Objects[VertIndex[K]]);
          Face := TAn8Face(Mesh.Faces.Objects[(J And $7FFFFFFF) Shr 2]);
          I    := Face.Points[J And 3];

          // Get the bone for this point

          Bone := TAn8Bone(Mesh.PrimaryBones.Objects[I]);

          // Each vertex has a normal vector for phong shading

          Normal := Points3D.T3DPoint(Mesh.Normals.Objects[I]);

          // Get the point

          Point := Points3D.T3DPoint(Mesh.Points.Objects[I]);

          // Transform the normal

          V.Copy(Normal);
          Q.Copy(Bone.Orientation);
          Bone1 := Bone.Parent;
          While Bone1 <> Nil Do
          Begin
            Q1.Copy(Bone1.Orientation);
            Q1.Multiply(Q);
            Q.Copy(Q1);
            Bone1 := Bone1.Parent;
          End; // While
          Q.Invert;
          Q.Transform(V);

          PXWFVertexRec(P)^.I := V.Z;
          PXWFVertexRec(P)^.J := V.X;
          PXWFVertexRec(P)^.K := V.Y;

          // Transform the point

          V.Copy(Point);
          V.Subtract(Bone.Origin);

          If NO <> Nil Then V.Add(NO.Base);
          Q.Copy(Bone.Orientation);
          Bone1 := Bone.Parent;
          While Bone1 <> Nil Do
          Begin
            Q1.Copy(Bone1.Orientation);
            Q1.Multiply(Q);
            Q.Copy(Q1);
            Bone1 := Bone1.Parent;
          End; // While
          Q.Invert;
          Q.Transform(V);

          PXWFVertexRec(P)^.X := V.Z * CustomScale;
          PXWFVertexRec(P)^.Y := V.X * CustomScale;
          PXWFVertexRec(P)^.Z := V.Y * CustomScale;

          // Set the texture coordinate

          If (J And $80000000) = 0 Then
          Begin
            PXWFVertexRec(P)^.U := TAn8TexCoord(Mesh.TexCoords.Objects[Face.TexCoords[J And 3]]).TX;
            PXWFVertexRec(P)^.V := TAn8TexCoord(Mesh.TexCoords.Objects[Face.TexCoords[J And 3]]).TZ;
          End
          Else
          Begin
            PXWFVertexRec(P)^.U := 0;
            PXWFVertexRec(P)^.V := 0;
          End;

          // Move to the next point

          Inc(LongWord(P),SizeOf(TXWFVertexRec));
        End; // For K

        // Add the vertex atom

        VertAtom := TXWFGroupContainer.Create(fourccVertexGroup,0,SizeOf(TXWFVertexRec),Mesh.Points.Count,Vertices);
        ParentAtom.AddChild(VertAtom);
        If Mesh.Points.Count > 0 Then FreeMem(Vertices,Mesh.Points.Count * SizeOf(TXWFVertexRec));

        // Cleanup

        V.Free;
        Q1.Free;
        Q.Free;
      End;
    End; // AddVertexAtoms

    Procedure AddTextureAtoms(ParentAtom: TXWFAtom; Mesh: TAn8Mesh; OutTexList: TStringList; MeshIndex,TexIndex: Integer; MeshType: TMeshType);
    Var
      I            : Integer;
      Mat          : TAn8Material;
      MaterialList : TStringList;
      TexAtom      : TXWFHashAtom;
      St           : String;

    Begin
      OutTexList.Clear;
      MaterialList := TStringList.Create;
      MaterialList.Sorted := True;
      For I := 0 To Mesh.MaterialList.Count - 1 Do
      Begin
        Mat := TAn8Material(Mesh.MaterialList.Objects[I]);

        // Only process each material once

        If Mat <> Nil Then
        Begin
          If MaterialList.IndexOf(Mat.Name) < 0 Then
          Begin
            MaterialList.AddObject(Mat.Name,Mat);

            // The texture should be attached to the diffuse color; make sure there is one

            If (Mat.Surface.Diffuse.Texture <> Nil) And
               (Mat.Surface.Diffuse.Texture.FileName <> '') Then
            Begin
              St := ExtractFileName(LowerCase(Mat.Surface.Diffuse.Texture.FileName)); // Name + extension, no path
              TexAtom := TXWFHashAtom.Create(fourccTexture);
              TexAtom.ID := OutTexList.Count;
              TexAtom.Add('numfiles','1'); // Not animated
              TexAtom.Add('texture_0',St);
              TexAtom.Add('meshindex',IntToStr(MeshIndex));    
              TexAtom.Add('texindex',IntToStr(TexIndex));
              If Mat.Name = 'face_00' Then TexAtom.Add('faceindex','0');

              // Although the mesh type is part of the texture atom, it will be attached to the mesh by the DLL importer.
              // That way the client will be able to know what a mesh represents.

              TexAtom.Add('meshtype',MeshTypeID[MeshType]);
              If CharTexList.IndexOf(St) < 0 Then CharTexList.Add(St);
              ParentAtom.AddChild(TexAtom);
              OutTexList.Add(Mat.Surface.Diffuse.Texture.Name);
            End;
          End;
        End
        Else ShowMessage('AddTextureAtoms: Invalid material in mesh ' + Mesh.Name + ' at index ' + IntToStr(I));
      End; // For I
      MaterialList.Free;
    End; // AddTextureAtoms

    Procedure AddPlayerFaceTextureAtoms(ParentAtom: TXWFAtom);
    Var
      I,J          : Integer;
      Mat          : TAn8Material;
      MaterialList : TStringList;
      TexAtom      : TXWFHashAtom;
      St           : String;

    Begin
      // Look for alternate faces (for player models)

      MaterialList := TStringList.Create;
      MaterialList.Sorted := True;
      For I := 1 To 7 Do 
      Begin
        J := An8.Materials.IndexOf(Format('face_%.2d',[I]));
        If J >= 0 Then
        Begin
          Mat := TAn8Material(An8.Materials.Objects[J]);
          If MaterialList.IndexOf(Mat.Name) < 0 Then
          Begin
            MaterialList.AddObject(Mat.Name,Mat);

            // The texture should be attached to the diffuse color

            If (Mat.Surface.Diffuse.Texture <> Nil) And
               (Mat.Surface.Diffuse.Texture.FileName <> '') Then
            Begin
              St := ExtractFileName(LowerCase(Mat.Surface.Diffuse.Texture.FileName)); // Name + extension, no path
              TexAtom := TXWFHashAtom.Create(fourccTexture);
              TexAtom.Add('numfiles','1'); // Not animated
              TexAtom.Add('texture_0',St);
              TexAtom.Add('faceindex',IntToStr(I));
              TexAtom.ID := I;
              If CharTexList.IndexOf(St) < 0 Then CharTexList.Add(St);
              ParentAtom.AddChild(TexAtom);
            End;
          End;
        End;
      End; // For I
      MaterialList.Free;
    End; // AddPlayerFaceTextureAtoms

    Procedure AddPolygonAtoms(ParentAtom: TXWFAtom; Mesh: TAn8Mesh; TexList: TStringList);
    Var
      Polygons   : Pointer;
      P          : PXWFPolygonRec;
      I,J,K,L    : Integer;
      Face       : TAn8Face;
      Textures   : String;
      Opacities  : String;
      Parameters : String;
      TextureID  : Integer;
      PolyAtom   : TXWFGroupContainer;
      Texture    : String;
      St         : String;

    Begin
      // Change the vertex-texcoord list so it returns the sorted index

      For K := 0 To High(VertIndex) Do VertTexList.Objects[VertIndex[K]] := Pointer(K);

      // Process the polygons

      If Mesh.Faces.Count > 0 Then
      Begin
        GetMem(Polygons,Mesh.Faces.Count * SizeOf(TXWFPolygonRec));
        P := Polygons;
        For I := 0 To Mesh.Faces.Count - 1 Do
        Begin
          Face := TAn8Face(Mesh.Faces.Objects[I]);

          For J := 0 To High(Face.Points) Do
          Begin
            // Build the lookup string from the face

            If High(Face.TexCoords) = High(Face.Points)
             Then St := IntToStr(Face.Points[J]) + ',' + IntToStr(Face.TexCoords[J])
             Else St := IntToStr(Face.Points[J]);

            // Find the vertex index

            L := VertTexList.IndexOf(St);
            If L >= 0
             Then L := Integer(VertTexList.Objects[L])
             Else L := 0; // Should never happen

            Case J Of
              0: P^.V1 := LongWord(htonl(L));
              1: P^.V2 := LongWord(htonl(L));
              2: P^.V3 := LongWord(htonl(L));
            End; // Case
          End; // For J

          If (Face.Material.Surface.Diffuse.Texture <> Nil) And
             (Face.Material.Surface.Diffuse.Texture.FileName <> '')
           Then Texture := Face.Material.Surface.Diffuse.Texture.Name
           Else Texture := '';
          TextureID := TexList.IndexOf(Texture);
          If TextureID >= 0 Then
          Begin
            P^.TextureID := LongWord(htonl(TextureID));
            P^.Flags := LongWord(htonl(0)); // Solid
          End
          Else
          Begin
            P^.TextureID := LongWord(htonl(0));
            P^.Flags     := LongWord(htonl(2)); // Set the transparent flag
          End;
          Inc(LongWord(P),SizeOf(TXWFPolygonRec));
        End; // For I
        PolyAtom := TXWFGroupContainer.Create(fourccPolygonGroup,0,SizeOf(TXWFPolygonRec),Mesh.Faces.Count,Polygons);
        ParentAtom.AddChild(PolyAtom);
        FreeMem(Polygons,Mesh.Faces.Count * SizeOf(TXWFPolygonRec));
      End;
    End; // AddPolygonAtoms

    Procedure AddVertexBoneAtoms(ParentAtom: TXWFAtom; Mesh: TAn8Mesh);
    Var
      Vertices : Pointer;
      P        : PXWFVertexBoneRec;
      I,J      : Integer;
      Bone     : TAn8Bone;
      VertAtom : TXWFGroupContainer;
      Face     : TAn8Face;

    Begin
      Vertices := Nil;
      If Mesh.Points.Count > 0 Then
      Begin
        GetMem(Vertices,VertTexList.Count * SizeOf(TXWFVertexBoneRec));
        P := Vertices;
        For I := 0 To VertTexList.Count - 1 Do
        Begin
          J    := Integer(VertTexList.Objects[VertIndex[I]]);
          Face := TAn8Face(Mesh.Faces.Objects[(J And $7FFFFFFF) Shr 2]);

          // Get the bone for this point

          Bone := TAn8Bone(Mesh.PrimaryBones.Objects[Face.Points[J And 3]]);
          P.BoneIndex := ntohl(Bone.Index);
          Inc(LongWord(P),SizeOf(TXWFVertexBoneRec));
        End; // For I

        // Add the vertex atom

        VertAtom := TXWFGroupContainer.Create(fourccVertBoneGroup,0,SizeOf(TXWFVertexBoneRec),Mesh.Points.Count,Vertices);
        ParentAtom.AddChild(VertAtom);
        If Mesh.Points.Count > 0 Then FreeMem(Vertices,Mesh.Points.Count * SizeOf(TXWFVertexBoneRec));
      End;
    End; // AddVertexBoneAtoms

  Begin
    // Make sure we have an object, a figure, and a sequence

    If (An8.Objects.Count > 0) And
       (An8.Figures.Count > 0) And
       (An8.Sequences.Count > 0) Then
    Begin
      // Create a list that will be used for vertices and texture coordinates. The reason for
      // this is that Anim8or files share vertices but have different texture coordinates and
      // XWF files don't, so we need a way to create unique instances.

      VertTexList        := TStringList.Create;
      VertTexList.Sorted := True;

      // Create a creature atom that will hold everything

      CreatureAtom := TXWFHashAtom.Create(fourccCreature);
      CreatureAtom.Add('model',Model + '_ACTORDEF');

      // Don't rely on the Anim8or file to specify the order in which to process objects.
      // The first object must always be the one with the base body and head.

      ObjectList := TStringList.Create;
      BuildAn8ObjectList(An8,ObjectList,SeparateHeads);

      // Combine the meshes into head meshes and body meshes

      NewMeshes := TStringList.Create;
      BuildAn8HeadBodyMeshes(An8,ObjectList,NewMeshes,FigureIndex,SeparateHeads,False,BaseMeshCount,DZ,ML,HeadCount,BodyCount);
      DZ := DZ / 2;
      ML := ML;

      // Add the base skeleton

      AddAnimation(CreatureAtom,TAn8Bone(TAn8Figure(An8.Figures.Objects[FigureIndex]).RootBone),Nil,DZ,ML);

      // Add the subsequent animations

      For I := 0 To An8.Sequences.Count - 1 Do
      Begin
        AddAnimation(CreatureAtom,TAn8Bone(TAn8Figure(An8.Figures.Objects[FigureIndex]).RootBone),
                     TAn8Sequence(An8.Sequences.Objects[I]),DZ,ML);
      End; // For I

      // Create a texture list that we'll use for adding objects

      Textures    := TStringList.Create; // Contains textures in order of usage
      MeshTexHash := TIntegerIntegerHash.Create(False);

      // Find the first texture set for each mesh type (all other texture sets won't be initially added)

      For I := 0 To ObjectList.Count - 1 Do
      Begin
        If TokenizeAn8ObjectName(ObjectList.Strings[I],BodyMesh,BodyTex,HeadMesh,HeadTex) Then
        Begin
          If MeshTexHash.IndexOf(HeadMesh)       < 0 Then MeshTexHash.Put(HeadMesh,HeadTex);
          If MeshTexHash.IndexOf(BodyMesh + 100) < 0 Then MeshTexHash.Put(BodyMesh + 100,BodyTex + 100);
        End;
      End; // For I

      // Add the head meshes

      ObjIndex := 0;
      For I := 0 To NewMeshes.Count - 1 Do
      Begin
        Mesh := TAn8Mesh(NewMeshes.Objects[I]);
        If UpperCase(Copy(Mesh.Name,1,2)) = 'HE' Then
        Begin
          ObjAtom := TXWFIDAtom.Create(fourccObject);
          Val(Copy(Mesh.Name,3,2),J,K);
          If K <> 0 Then J := 0;
          ObjAtom.ID := ObjIndex;//J + MeshTexHash.Get(J) * 20; // ID's 0-399 mean head meshes
          Inc(ObjIndex);
          AddTextureAtoms(ObjAtom,Mesh,Textures,J,J,mtHead); // Head meshes always have matching textures
          AddVertexAtoms(ObjAtom,Mesh);
          AddVertexBoneAtoms(ObjAtom,Mesh);
          AddPolygonAtoms(ObjAtom,Mesh,Textures);
          CreatureAtom.AddChild(ObjAtom);
        End;
      End; // For I

      // Add the body meshes (if they're head+body meshes, that's okay as body will be the default type)

      For I := 0 To NewMeshes.Count - 1 Do
      Begin
        Mesh := TAn8Mesh(NewMeshes.Objects[I]);
        If UpperCase(Copy(Mesh.Name,1,2)) <> 'HE' Then
        Begin
          ObjAtom := TXWFIDAtom.Create(fourccObject);
          Val(Copy(Mesh.Name,1,2),J,K);
          If K <> 0 Then J := 0;
          ObjAtom.ID := ObjIndex;//J + MeshTexHash.Get(J + 100) * 20 + 400; // ID's 400-799 mean body meshes
          Inc(ObjIndex);
          AddTextureAtoms(ObjAtom,Mesh,Textures,J,MeshTexHash.Get(J + 100) - 100,mtBody);
          AddVertexAtoms(ObjAtom,Mesh);
          AddVertexBoneAtoms(ObjAtom,Mesh);
          AddPolygonAtoms(ObjAtom,Mesh,Textures);
          CreatureAtom.AddChild(ObjAtom);
        End;
      End; // For I

      // Exchange the keys and values of the hash so it indexes by texture set instead of mesh

      MeshTexHash.GetKeysAndValues(Enumeration);
      MeshTexHash.Clear;
      For I := 0 To High(Enumeration) Do MeshTexHash.Put(Enumeration[I].Value.AsInteger,Enumeration[I].Key.Int);

      // Add extra head and body texture sets

      For I := 0 To ObjectList.Count - 1 Do
      Begin
        If TokenizeAn8ObjectName(ObjectList.Strings[I],BodyMesh,BodyTex,HeadMesh,HeadTex) Then
        Begin
          If SeparateHeads Then
          Begin
            If MeshTexHash.IndexOf(HeadTex) < 0 Then
            Begin
              MeshTexHash.Put(HeadTex,HeadMesh);
              AddAn8BodyHeadPair(An8,NewMeshes,TAn8Object(ObjectList.Objects[I]),FigureIndex,-1,HeadMesh,SeparateHeads,False);

              ObjAtom    := TXWFIDAtom.Create(fourccObject);
              Mesh       := TAn8Mesh(NewMeshes.Objects[NewMeshes.Count - 1]);
              ObjAtom.ID := ObjIndex;//HeadMesh + HeadTex * 20;
              Inc(ObjIndex);

              AddTextureAtoms(ObjAtom,Mesh,Textures,HeadMesh,HeadTex,mtHead);
              CreatureAtom.AddChild(ObjAtom);
            End;
          End;
          If MeshTexHash.IndexOf(BodyTex + 100) < 0 Then
          Begin
            MeshTexHash.Put(BodyTex + 100,BodyMesh + 100);
            AddAn8BodyHeadPair(An8,NewMeshes,TAn8Object(ObjectList.Objects[I]),FigureIndex,BodyMesh,HeadMesh,SeparateHeads,False);

            ObjAtom    := TXWFIDAtom.Create(fourccObject);
            Mesh       := TAn8Mesh(NewMeshes.Objects[NewMeshes.Count - 1]);
            ObjAtom.ID := ObjIndex;//BodyMesh + BodyTex * 20 + 400;
            Inc(ObjIndex);

            AddTextureAtoms(ObjAtom,Mesh,Textures,BodyMesh,BodyTex,mtBody);
            CreatureAtom.AddChild(ObjAtom);
          End;
        End;
      End; // For I

      // Add alternate face textures (for player models)

      AddPlayerFaceTextureAtoms(CreatureAtom);

      // Add equipment meshes

      BuildEquipmentMeshes(An8,NewMeshes);
      For I := 0 To NewMeshes.Count - 1 Do
      Begin
        Mesh       := TAn8Mesh(NewMeshes.Objects[I]);
        ObjAtom    := TXWFIDAtom.Create(fourccObject);
        ObjAtom.ID := ObjIndex;
        Inc(ObjIndex);
        AddTextureAtoms(ObjAtom,Mesh,Textures,I,I,GetEquipmentMeshType(Mesh));
        AddVertexAtoms(ObjAtom,Mesh);
        AddVertexBoneAtoms(ObjAtom,Mesh);
        AddPolygonAtoms(ObjAtom,Mesh,Textures);
        CreatureAtom.AddChild(ObjAtom);
      End; // For I

      // Add the creature atom

      ParentAtom.AddChild(CreatureAtom);

      // Cleanup

      ObjectList.Free;
      For I := 0 To NewMeshes.Count - 1 Do NewMeshes.Objects[I].Free;
      NewMeshes.Free;
      Textures.Free;
      MeshTexHash.Free;
      VertTexList.Free;
    End;
  End; // EncodeCreature

  Procedure AddLight(LightsAtom: TXWFContainer; ZO: TZoneObject; Var Index: Integer);
  Var
    I      : Integer;
    GO     : TGroupObject;
    ML     : TMeshLibraryObjectReference;
    Light  : TLightObject;
    Loc    : T3DPoint;
    XLight : PXWFLightRec;

  Begin
    If ZO Is TLightObject Then
    Begin
      Light  := TLightObject(ZO);
      Loc    := Light.GetAbsoluteLocation;
      XLight := PXWFLightRec(LightsAtom.Items[Index]);
      If XLight <> Nil Then
      Begin
        Loc.Multiply(BaseScale);
        BaseHeading.Rotate(Loc);
        Loc.Add(BaseLoc);
        XLight.X       := Loc.X;
        XLight.Y       := Loc.Y;
        XLight.Z       := Loc.Z;
        XLight.R       := TRGBA(Light.Color).R / 255;
        XLight.G       := TRGBA(Light.Color).G / 255;
        XLight.B       := TRGBA(Light.Color).B / 255;
        XLight.Radius  := Light.Radius;
        XLight.Ambient := 0;//False
      End;
      Inc(Index);
      Loc.Free;
    End
    Else If ZO Is TMeshLibraryObjectReference Then
    Begin
      Loc := ZO.GetAbsoluteLocation;
      BaseLoc.Add(Loc);
      BaseHeading.Copy(ZO.Rotate);
      BaseScale.Multiply(ZO.Size);
      ML := TMeshLibraryObjectReference(ZO);
      If ML.Group <> Nil Then AddLight(LightsAtom,ML.Group,Index);
      BaseLoc.Subtract(Loc);
      BaseHeading.Copy(0,0,0);
      If ZO.Size.X <> 0 Then BaseScale.X := BaseScale.X / ZO.Size.X;
      If ZO.Size.Y <> 0 Then BaseScale.Y := BaseScale.Y / ZO.Size.Y;
      If ZO.Size.Z <> 0 Then BaseScale.Z := BaseScale.Z / ZO.Size.Z;
      Loc.Free;
    End
    Else If ZO Is TGroupObject Then
    Begin
      GO := TGroupObject(ZO);
      For I := 0 To GO.Objects.Count - 1 Do AddLight(LightsAtom,TZoneObject(GO.Objects.Objects[I]),Index);
    End;
  End; // AddLight

  Procedure CountLights(ZO: TZoneObject; Var LightCount: Integer);
  Var
    I  : Integer;
    GO : TGroupObject;
    ML : TMeshLibraryObjectReference;

  Begin
    If ZO Is TLightObject Then Inc(LightCount)
    Else If ZO Is TMeshLibraryObjectReference Then
    Begin
      ML := TMeshLibraryObjectReference(ZO);
      If ML.Group <> Nil Then CountLights(ML.Group,LightCount);
    End
    Else If ZO Is TGroupObject Then
    Begin
      GO := TGroupObject(ZO);
      For I := 0 To GO.Objects.Count - 1 Do CountLights(TZoneObject(GO.Objects.Objects[I]),LightCount);
    End;
  End; // CountLights

Begin
  // Generate a mesh containing the entire zone

  If frmStatus.Visible Then
  Begin
    frmStatus.SetCaption('Building mesh...');
    frmStatus.SetPosition(0);
  End;
  Mesh := BuildPolygonList(True,False,True,False);
  AddWaterToMesh(Mesh);
  Mesh.ConvertToTriangles;
  OutTexList := TStringList.Create;
  OutTexList.Sorted := True;

  MinPt := T3DPoint.Create;
  MaxPt := T3DPoint.Create;
  Tree  := Nil;
  If Mesh.Polygons.Count > 0 Then
  Begin
    // Get the mesh bounds

    Mesh.GetBounds(MinPt,MaxPt);

    // Split the mesh to make an octree or BSP tree

    Tree := TTree.Create(Mesh,False);
    If ExportAsOctree Then
    Begin
      If frmStatus.Visible Then
      Begin
        frmStatus.SetCaption('Building octree...');
        frmStatus.SetPosition(0);
      End;
      Tree.MakeOctree;
    End
    Else
    Begin
      If frmStatus.Visible Then
      Begin
        frmStatus.SetCaption('Building BSP tree...');
        frmStatus.SetPosition(0);
      End;
      Tree.SplitAlongGrid(1024,1024 * 3);
      If High(Water) >= 0 Then
      Begin
        For J := 0 To High(Water) Do
        Begin
          Tree.SplitAlongWater(Water[J]);
          Case Water[J].WType Of
            wtWater: Tree.Root.SetFlaggedToAttribute(raWater,0);
             wtLava: Tree.Root.SetFlaggedToAttribute(raLava,0);
              wtPvP: Tree.Root.SetFlaggedToAttribute(raPvP,0);
              wtIce: Tree.Root.SetFlaggedToAttribute(raIce,0);
         wtIceWater: Tree.Root.SetFlaggedToAttribute(raIceWater,0);
          End; // Case
        End; // For J
      End;
      If ZonePlanes.Count > 0 Then
      Begin
        For I := 0 To ZonePlanes.Count - 1 Do
        Begin
          ZP := TZonePlane(ZonePlanes.Objects[I]);
          Tree.SplitAlongZonePlane(ZP);
          Tree.Root.SetFlaggedToAttribute(raZoneLine,I);
        End; // For I
      End;
    End;
    Tree.Root.ConvertToTriangles;
  End;

  // Create the world atom

  WldAtom := TXWFAtom.Create(fourccWorld);

  // Add the texture atoms

  If Mesh.Polygons.Count > 0 Then AddTextureAtoms(WldAtom,Mesh,TexList,OutTexList);

  // Build the vertex list

  If Mesh.Polygons.Count > 0 Then AddVertexAtoms(WldAtom,Mesh);

  // Shade polygons by light sources

  LightTreeHash := TLightTreeHash.Create(Self,Tree);
  
  If Tree <> Nil Then Tree.Root.ShadePolygonsByLights(LightTreeHash);

  // Build the vertex color list

  If Mesh.Polygons.Count > 0 Then AddVertexColorAtoms(WldAtom,Mesh);

  // Add the octree/bsp tree atom

  If frmStatus.Visible Then
  Begin
    frmStatus.SetCaption('Building octree atoms...');
    frmStatus.SetPosition(0);
  End;
  If Mesh.Polygons.Count > 0 Then
  Begin
    If ExportAsOctree Then
    Begin
      Octree := TXWFOctreeNode.Create;
      ProcessOctree(Tree.Root,Octree,OutTexList);
      WldAtom.AddChild(Octree);
    End
    Else
    Begin
      BSPTree := TXWFBSPTreeNode.Create;
      ProcessBSPTree(Tree.Root,BSPTree,OutTexList);
      WldAtom.AddChild(BSPTree);
    End;
  End;

  // Make a list of all mesh library object references

  List := TStringList.Create;
  J := GetObjectMeshCount;
  If frmStatus.Visible Then
  Begin
    frmStatus.SetCaption('Making a list of all mesh library object references...');
    frmStatus.SetPosition(0);
  End;
  For I := 0 To J Do
  Begin
    If frmStatus.Visible Then frmStatus.SetPosition(I / (J + 1));
    ZO := GetZoneObject(I);
    If ZO Is TMeshLibraryObjectReference Then List.AddObject('',ZO);
  End; // For I

  // Make a unique list of mesh library objects

  Hash := TIntegerObjectHash.Create(False,False);
  For I := 0 To List.Count - 1 Do
  Begin
    If frmStatus.Visible Then frmStatus.SetPosition(I / List.Count);
    ML := TMeshLibraryObjectReference(List.Objects[I]);
    J  := MeshLibrary.IndexOfObject(ML.Group);
    Hash.Put(J,ML.Group);
  End; // For I

  // Export the mesh library objects

  Hash.GetKeysAndValues(Enumeration);
  List1 := TStringList.Create;
  If frmStatus.Visible Then
  Begin
    frmStatus.SetCaption('Exporting mesh library objects...');
    frmStatus.SetPosition(0);
  End;
  For I := 0 To High(Enumeration) Do
  Begin
    If frmStatus.Visible Then frmStatus.SetPosition(I / (High(Enumeration) + 1));
    ML := TMeshLibraryObjectReference(Enumeration[I].Value.AsObject);
    ML.AddToPolygonList(List1,False,True,True,True);
    MLMesh := CoalescePolygonList(List1);
    MLMesh.ConvertToTriangles;
    ObjAtom := TXWFIDAtom.Create(fourccObject);
    ObjAtom.ID := Enumeration[I].Key.Int;
    AddTextureAtoms(ObjAtom,MLMesh,TexList,OutTexList);
    AddVertexAtoms(ObjAtom,MLMesh);
    AddPolygonAtoms(ObjAtom,MLMesh,OutTexList);
    WldAtom.AddChild(ObjAtom);
    MLMesh.Free;
    For J := 0 To List1.Count - 1 Do List1.Objects[J].Free;
    List1.Clear;
  End; // For I

  // Add the object locations

  Center := T3DPoint.Create;
  RSize  := T3DPoint.Create;
  If List.Count > 0 Then
  Begin
    If frmStatus.Visible Then
    Begin
      frmStatus.SetCaption('Exporting object locations...');
      frmStatus.SetPosition(0);
    End;
    For I := 0 To List.Count - 1 Do
    Begin
      If frmStatus.Visible Then frmStatus.SetPosition(I / List.Count);
      ZO := TMeshLibraryObjectReference(List.Objects[I]);
      If Not (TMeshLibraryObjectReference(ZO).InsertMesh Or (TMeshLibraryObjectReference(ZO).SQLRef <> srNone)) Then
      Begin
        GetMem(ObjLocs,SizeOf(TXWFObjectPlacementRec));
        P  := ObjLocs;
        ML := TMeshLibraryObjectReference(ZO.MakeCopy);
        ML.ChangeToAbsolute(ML.GetParent);
        ML.SetParent(Nil);
        J  := MeshLibrary.IndexOfObject(ML.Group);
        PXWFObjectPlacementRec(P)^.ID := ntohl(J);
        PXWFObjectPlacementRec(P)^.X  := ML.Loc.X;
        PXWFObjectPlacementRec(P)^.Y  := ML.Loc.Y;
        PXWFObjectPlacementRec(P)^.Z  := ML.Loc.Z;
        PXWFObjectPlacementRec(P)^.RX := ML.Rotate.XAngle * Pi / 180;
        PXWFObjectPlacementRec(P)^.RY := -ML.Rotate.YAngle * Pi / 180;
        PXWFObjectPlacementRec(P)^.RZ := ML.Rotate.ZAngle * Pi / 180;
        PXWFObjectPlacementRec(P)^.SX := ML.Size.X;
        PXWFObjectPlacementRec(P)^.SY := ML.Size.Y;
        PXWFObjectPlacementRec(P)^.SZ := ML.Size.Z;
        ObjPAtom := TXWFContainer.Create(fourccObjectPlacement,SizeOf(TXWFObjectPlacementRec),1,ObjLocs);
        FreeMem(ObjLocs,SizeOf(TXWFObjectPlacementRec));
        WldAtom.AddChild(ObjPAtom);

        ML.AddToPolygonList(List1,False,True,True,True);
        MLMesh := CoalescePolygonList(List1);
        MLMesh.ConvertToTriangles;
        MLMesh.CalcNormals;
        MLMesh.GetBounds(MinPt,MaxPt);
        RSize.Copy(MaxPt);
        RSize.Subtract(MinPt);
        Center.Copy(MinPt);
        Center.Add(MaxPt);
        Center.Divide(2);

        MLMesh.ShadePolygonsByLights(LightTreeHash,Nil,Center,RSize.GetLength);
        If MLMesh.Polygons.Count > 0 Then AddVertexColorAtoms(ObjPAtom,MLMesh);
        MLMesh.Free;
        For J := 0 To List1.Count - 1 Do List1.Objects[J].Free;
        List1.Clear;

        ML.Free;
      End;
    End; // For I
  End;

  // Add any objects the user explicity added in File...Properties (e.g. door meshes)

  SetLength(MeshType,MeshLibrary.Count);
  For I := 0 To High(MeshType) Do MeshType[I] := False;
  MeshHash := TIntegerStringHash.Create(False);
  If frmStatus.Visible Then
  Begin
    frmStatus.SetCaption('Exporting explicitly added objects...');
    frmStatus.SetPosition(0);
  End;
  For I := 0 To ExtraMeshes.Count - 1 Do
  Begin
    St := ExtraMeshes.Strings[I];

    // The format is <originalname>/<exportname>.  Get the original name and look for it in the mesh library.

    K := Pos('/',St);
    If K > 0 Then St := Copy(St,1,K - 1);
    J := MeshLibrary.IndexOf(St);
    If J >= 0 Then
    Begin
      MeshType[J] := True;
      St := ExtraMeshes.Strings[I];
      If K > 0 Then St := Copy(St,K + 1,Length(St));
      MeshHash.Put(J,St); // Save the export name
    End;
  End; // For I

  // Encode each of the object types

  K := 0;
  For I := 0 To High(MeshType) Do
  Begin
    If frmStatus.Visible Then frmStatus.SetPosition(I / (High(MeshType) + 1));
    If MeshType[I] Then
    Begin
      TGroupObject(MeshLibrary.Objects[I]).AddToPolygonList(List1,False,True,True,True);
      MLMesh := CoalescePolygonList(List1);
      MLMesh.ConvertToTriangles;
      ObjAtom := TXWFIDAtom.Create(fourccObject);
      ObjAtom.ID := I;
      AddTextureAtoms(ObjAtom,MLMesh,TexList,OutTexList);
      AddVertexAtoms(ObjAtom,MLMesh);
      AddPolygonAtoms(ObjAtom,MLMesh,OutTexList);
      WldAtom.AddChild(ObjAtom);
      MLMesh.Free;
      For J := 0 To List1.Count - 1 Do List1.Objects[J].Free;
      List1.Clear;
    End;
  End; // For I

  // Add zone meshes to the list so we can encode their names

  For I := 0 To List.Count - 1 Do
  Begin
    If frmStatus.Visible Then frmStatus.SetPosition(I / List.Count);
    ZO := TMeshLibraryObjectReference(List.Objects[I]);
    J  := MeshLibrary.IndexOf(TMeshLibraryObjectReference(ZO).Group.GetName);
    If J >= 0 Then
    Begin
      MeshType[J] := True;
      MeshHash.Put(J,MeshLibrary.Strings[J]);
    End;
  End;

  // If any extra meshes were exported, add a name atom that links the object ID's and the export names

  If (List.Count > 0) Or (ExtraMeshes.Count > 0) Then
  Begin
    NameAtom := TXWFHashAtom.Create(fourccNames);
    For I := 0 To MeshLibrary.Count - 1 Do
    Begin
      If MeshType[I] Then
      Begin
        St := MeshHash.Get(I);
        NameAtom.Add('obj' + IntToStr(I),St);
      End;
    End; // For I
    WldAtom.AddChild(NameAtom);
  End;

  // Encode light sources

  J := 0;
  For I := 0 To Count - 1 Do CountLights(TZoneObject(Objects[I]),J);
  If J > 0 Then
  Begin
    LightsAtom  := TXWFContainer.Create(fourccLights,SizeOf(TXWFLightRec),J,Nil);
    J           := 0;
    BaseLoc     := T3DPoint.Create;
    BaseHeading := THeading.Create;
    BaseScale   := T3DPoint.Create;
    For I := 0 To Count - 1 Do
    Begin
      BaseLoc.Copy(0,0,0);
      BaseHeading.Copy(0,0,0);
      BaseScale.Copy(1,1,1);
      AddLight(LightsAtom,TZoneObject(Objects[I]),J);
    End; // For I
    BaseScale.Free;
    BaseHeading.Free;
    BaseLoc.Free;
    WldAtom.AddChild(LightsAtom);
  End;

  // Encode the creatures to be added

  If frmStatus.Visible Then
  Begin
    frmStatus.SetCaption('Exporting creatures...');
    frmStatus.SetPosition(0);
  End;

  // First export the creatures referenced by zone objects

  List1.Clear;
  For I := 0 To Count - 1 Do
  Begin
    If frmStatus.Visible Then frmStatus.SetPosition(I / Count);
    ZO := TZoneObject(Objects[I]);
    If ZO Is TCreatureLibraryObjectReference Then
    Begin
      CL := TCreatureLibraryObjectReference(ZO);
      If List1.IndexOfObject(CL.An8File) < 0 Then
      Begin
        CL.LoadCreature;
        J := CreatureLibrary.IndexOfObject(CL.An8File);
        If J >= 0 Then // Should always be true
        Begin
          EncodeCreature(WldAtom,CL.An8File,UpperCase(CreatureLibrary.Strings[J]),CustomScale);
          List1.AddObject('',CL.An8File);
        End;
      End;
    End;
  End; // For I

  // Now export creatures the user has explicitly added

  For I := 0 To Creatures.Count - 1 Do
  Begin
    If frmStatus.Visible Then frmStatus.SetPosition(I / Creatures.Count);
    St := Creatures.Strings[I];
    J  := Pos(',',St);
    If J > 0 Then St := UpperCase(Copy(St,1,J - 1));

    // If the creature hasn't yet been imported, import it now

    If (Creatures.Objects[I] = Nil) Or Not TAn8File(Creatures.Objects[I]).Loaded Then ImportCreature(I);

    If List1.IndexOfObject(TAn8File(Creatures.Objects[I])) < 0 Then
    Begin
      EncodeCreature(WldAtom,TAn8File(Creatures.Objects[I]),St,CustomScale);
      List1.AddObject('',TAn8File(Creatures.Objects[I]));
    End;
  End; // For I
  List1.Clear;

  // Export

  If frmStatus.Visible Then
  Begin
    frmStatus.SetCaption('Writing file to disk...');
    frmStatus.SetPosition(0);
  End;
  WldAtom.WriteToStream(Stream);

  // Cleanup

  LightTreeHash.Free;
  Center.Free;
  RSize.Free;
  SetLength(MeshType,0);
  MeshHash.Free;
  SetLength(Enumeration,0);
  Hash.Free;
  List.Free;
  List1.Free;
  WldAtom.Free;
  If Mesh.Polygons.Count > 0 Then
  Begin
    Tree.Free;
    Mesh.Free;
  End;
  MinPt.Free;
  MaxPt.Free;
  OutTexList.Free;
End; // TZone.ExportToXWFFile

Procedure TZone.ImportFromXWFFile(Stream: TStream);
Var
  WldAtom  : TXWFAtom;
  TexAtoms : TIntegerObjectHash;  // Hash of TXWFTexture
  ObjAtoms : TStringList;         // List of TXWFObject
  VertAtom : TXWFGroupContainer;
  PolyAtom : TXWFGroupContainer;
  ObjAtom  : TXWFIDAtom;
  ObjPAtom : TXWFContainer;
  Octree   : TXWFOctreeNode;
  BSPTree  : TXWFBSPTreeNode;
  Mesh     : TMeshObject;
  I,J,K    : Integer;
  Found    : Boolean;
  P        : PXWFVertexRec;
  P1       : PXWFObjectPlacementRec;
  TexU     : Array Of Double;
  TexV     : Array Of Double;
  LastAdd  : Integer;

  Procedure AssignNameAndAdd(Mesh: TMeshObject);
  Begin
    If Mesh.Polygons.Count > 0 Then
    Begin
      While NameExists('ImportedXWF' + IntToStr(LastAdd)) Do Inc(LastAdd);
      Mesh.SetName('ImportedXWF' + IntToStr(LastAdd));
      AddObject(Mesh.GetName,Mesh);
    End
    Else Mesh.Free;
  End; // AssignNameAndAdd

  Procedure AddVertices(Mesh: TMeshObject; VertAtom: TXWFGroupContainer);
  Var
    I : Integer;
    P : PXWFVertexRec;

  Begin
    For I := 0 To VertAtom.Count - 1 Do
    Begin
      P := PXWFVertexRec(VertAtom.Items[I]);
      Mesh.Vertices.AddObject('',T3DPoint.Create(P^.X,P^.Y,P^.Z));
      Mesh.Normals.AddObject('',T3DPoint.Create(P^.I,P^.J,P^.K));
    End; // For I
    SetLength(TexU,Mesh.Vertices.Count);
    SetLength(TexV,Mesh.Vertices.Count);
    For I := 0 To VertAtom.Count - 1 Do
    Begin
      P := PXWFVertexRec(VertAtom.Items[I]);
      TexU[I] := P^.U;
      TexV[I] := P^.V;
    End; // For I
  End; // AddVertices

  Procedure AddPolygons(Mesh: TMeshObject; PolyAtom: TXWFGroupContainer; TexAtoms: TIntegerObjectHash);
  Var
    I,J,K   : Integer;
    W,H     : Integer;
    P       : PXWFPolygonRec;
    Tex     : TXWFHashAtom;
    Polygon : TPolygon;
    St      : String;

  Begin
    For J := 0 To PolyAtom.Count - 1 Do
    Begin
      P   := PXWFPolygonRec(PolyAtom.Items[J]);
      Tex := TXWFHashAtom(TexAtoms.Get(htonl(P^.TextureID)));
      If Tex <> Nil Then
      Begin
        Val(Tex.Get('width'),W,K);
        If K <> 0 Then W := 256;
        Val(Tex.Get('height'),H,K);
        If K <> 0 Then H := 256;
        Val(Tex.Get('numfiles'),I,K);
        If K <> 0 Then I := 1;
        St := '';
        For K := 0 To I - 1 Do
        Begin
          If K <> 0 Then St := St + ';';
          St := St + ExtractFilenameNoExt(Tex.Get('texture_' + IntToStr(K)));
        End; // For K
        St := St + '|';
        For K := 0 To I - 1 Do
        Begin
          If K <> 0 Then St := St + ';';
          St := St + ExtractFilenameNoExt(Tex.Get('opacity_' + IntToStr(K)));
        End; // For K
        St := St + '+' + Tex.Get('animtime');
      End
      Else
      Begin
        W  := 256;
        H  := 256;
        St := '';
      End;
      Polygon := TPolygon.Create([htonl(P^.V1),htonl(P^.V2),htonl(P^.V3)],St);//ExtractFilenameNoExt(Tex.Get('filename')));
      SetLength(Polygon.TX,3);
      SetLength(Polygon.TZ,3);
      Polygon.TX[0] := TexU[htonl(P^.V1)];
      Polygon.TX[1] := TexU[htonl(P^.V2)];
      Polygon.TX[2] := TexU[htonl(P^.V3)];
      Polygon.TZ[0] := TexV[htonl(P^.V1)];
      Polygon.TZ[1] := TexV[htonl(P^.V2)];
      Polygon.TZ[2] := TexV[htonl(P^.V3)];
      Polygon.HasTexCoords := True;
      Polygon.Solid := ((htonl(P^.Flags) And 1) = 0);
      Polygon.HasSolid := True;
      If (htonl(P^.Flags) And 4) <> 0 Then Polygon.TextureState := tsSemiTransparent;
      If (htonl(P^.Flags) And 2) <> 0 Then Polygon.TextureState := tsTransparent;
      Mesh.Polygons.AddObject('',Polygon);
    End; // For J
  End; // AddPolygons

  Procedure ProcessOctree(Octree: TXWFOctreeNode);
  Var
    I        : Integer;
    Atom     : TXWFAtom;
    PolyAtom : TXWFGroupContainer;

  Begin
    For I := 0 To Octree.ChildCount - 1 Do
    Begin
      Atom := Octree.Children[I];
           If Atom.FourCC = fourccOctreeNode Then ProcessOctree(TXWFOctreeNode(Atom))
      Else If Atom.FourCC = fourccPolygonGroup Then
      Begin
        PolyAtom := TXWFGroupContainer(Atom);
        AddPolygons(Mesh,PolyAtom,TexAtoms);
      End;
    End; // For I
  End; // ProcessOctree

  Procedure ProcessBSPTree(BSP: TXWFBSPTreeNode);
  Var
    I        : Integer;
    Atom     : TXWFAtom;
    PolyAtom : TXWFGroupContainer;

  Begin
    For I := 0 To BSP.ChildCount - 1 Do
    Begin
      Atom := BSP.Children[I];
           If Atom.FourCC = fourccBSPTreeNode Then ProcessBSPTree(TXWFBSPTreeNode(Atom))
      Else If Atom.FourCC = fourccPolygonGroup Then
      Begin
        PolyAtom := TXWFGroupContainer(Atom);
        AddPolygons(Mesh,PolyAtom,TexAtoms);
      End;
    End; // For I
  End; // ProcessBSPTree

Begin
  LastAdd  := 0;
  Mesh     := TMeshObject.Create;
  WldAtom  := TXWFAtom.Create(Stream);
  TexAtoms := TIntegerObjectHash.Create(False,False);
  VertAtom := TXWFGroupContainer(WldAtom.FindChild(fourccVertexGroup));
  Octree   := TXWFOctreeNode(WldAtom.FindChild(fourccOctreeNode));
  BSPTree  := TXWFBSPTreeNode(WldAtom.FindChild(fourccBSPTreeNode));
  For I := 0 To WldAtom.ChildCount - 1 Do
  Begin
    If WldAtom.Children[I].FourCC = fourccTexture Then TexAtoms.Put(TXWFHashAtom(WldAtom.Children[I]).ID,WldAtom.Children[I]);
  End; // For I
  If (VertAtom <> Nil) And ((Octree <> Nil) Or (BSPTree <> Nil)) Then
  Begin
    // Add the vertices

    AddVertices(Mesh,VertAtom);

    // Add the polygons

    If Octree <> Nil
     Then ProcessOctree(Octree)
     Else ProcessBSPTree(BSPTree)
  End;

  AssignNameAndAdd(Mesh);

  // Get placeable objects

  ObjAtoms := TStringList.Create;
  For I := 0 To WldAtom.ChildCount - 1 Do
   If WldAtom.Children[I].FourCC = fourccObject Then ObjAtoms.AddObject('',WldAtom.Children[I]);
  ObjPAtom := TXWFContainer(WldAtom.FindChild(fourccObjectPlacement));
  If ObjPAtom <> Nil Then
  Begin
    If frmStatus.Visible Then
    Begin
      frmStatus.SetCaption('Importing placeable objects...');
      frmStatus.SetPosition(0);
    End;
    For J := 0 To ObjPAtom.Count - 1 Do
    Begin
      If frmStatus.Visible Then frmStatus.SetPosition(J / ObjPAtom.Count);
      P1    := PXWFObjectPlacementRec(ObjPAtom.Items[J]);
      I     := 0;
      Found := False;
      While (I < ObjAtoms.Count) And Not Found Do
      Begin
        ObjAtom := TXWFIDAtom(ObjAtoms.Objects[I]);
        If ObjAtom.ID = ntohl(P1^.ID) Then
        Begin
          Found    := True;
          VertAtom := TXWFGroupContainer(ObjAtom.FindChild(fourccVertexGroup));
          PolyAtom := TXWFGroupContainer(ObjAtom.FindChild(fourccPolygonGroup));
          TexAtoms.Clear;
          For K := 0 To ObjAtom.ChildCount - 1 Do
          Begin
            If ObjAtom.Children[K].FourCC = fourccTexture Then TexAtoms.Put(TXWFHashAtom(ObjAtom.Children[K]).ID,ObjAtom.Children[K]);
          End; // For K
          If (VertAtom <> Nil) And (PolyAtom <> Nil) Then
          Begin
            Mesh := TMeshObject.Create;
            AddVertices(Mesh,VertAtom);
            AddPolygons(Mesh,PolyAtom,TexAtoms);
            Mesh.Loc.Copy(P1^.X,P1^.Y,P1^.Z);
            Mesh.Rotate.Copy(P1^.RX * 180 / Pi,P1^.RY * 180 / Pi,P1^.RZ * 180 / Pi);
            Mesh.Size.Copy(P1^.SX,P1^.SY,P1^.SZ);
            AssignNameAndAdd(Mesh);
          End;
        End
        Else Inc(I);
      End; // While
    End;
  End;

  // Cleanup

  ObjAtoms.Free;
  TexAtoms.Free;
  WldAtom.Free;
  SetLength(TexU,0);
  SetLength(TexV,0);
End; // TZone.ImportFromXWFFile

Function TZone.ImportAn8Objects(BaseName: String; An8File: TAn8File; List: TStringList; Rotate,Center: Boolean): TZoneObject;
Var
  I,J         : Integer;
  An8Obj      : TAn8Object;
  Component   : TAn8Component;
  MaxPt       : Points3D.T3DPoint;
  MinPt       : Points3D.T3DPoint;
  GO          : TGroupObject;
  ObjectCount : Integer;
  Textures    : TStringList;
  Mesh        : TMeshObject;
  St          : String;
  FoundGroup  : Boolean;

  Function ProcessMesh(An8Mesh: TAn8Mesh; GO: TGroupObject): TMeshObject;
  Var
    K,L,M       : Integer;
    Mesh        : TMeshObject;
    P0          : T3DPoint;
    P1          : T3DPoint;
    P2          : T3DPoint;
    Face        : TAn8Face;
    Polygon     : TPolygon;
    Texture     : TTexture;
    Q           : TQuaternion;
    
  Begin
    Mesh := TMeshObject.Create;
    Mesh.SetName('An8_' + An8Obj.Name + '_' + An8Mesh.Name);
    If Rotate
     Then Mesh.Loc.Copy(An8Mesh.Base.Origin.Z,An8Mesh.Base.Origin.X,An8Mesh.Base.Origin.Y)
     Else Mesh.Loc.Copy(An8Mesh.Base.Origin.X,An8Mesh.Base.Origin.Y,An8Mesh.Base.Origin.Z);
    P2 := T3DPoint.Create;
    SetLength(Mesh.BoneIndices,An8Mesh.Points.Count);

    // Add the points and normals

    For K := 0 To An8Mesh.Points.Count - 1 Do
    Begin
      P2.Copy(T3DPoint(An8Mesh.Points.Objects[K]));
      If Center Then P2.Subtract(MaxPt.X,MaxPt.Y,MaxPt.Z);
      An8Mesh.Base.Orientation.Transform(P2);
      If Rotate
       Then P0 := T3DPoint.Create(P2.Z,P2.X,P2.Y)
       Else P0 := T3DPoint.Create(P2.X,P2.Y,P2.Z);
      Mesh.Vertices.AddObject(P0.GetID,P0);

      // Set the bone index for the point if we can

      If An8Mesh.PrimaryBones.Count = An8Mesh.Points.Count Then Mesh.BoneIndices[K] := TAn8Bone(An8Mesh.PrimaryBones.Objects[K]).Index;

      P2.Copy(T3DPoint(An8Mesh.Normals.Objects[K]));
      If Rotate
       Then P0 := T3DPoint.Create(P2.Z,P2.X,P2.Y)
       Else P0 := T3DPoint.Create(P2.X,P2.Y,P2.Z);
      Mesh.Normals.AddObject(P0.GetID,P0);
    End; // For K

    // Add the faces

    For K := 0 To An8Mesh.Faces.Count - 1 Do
    Begin
      Face    := TAn8Face(An8Mesh.Faces.Objects[K]);
      Polygon := TPolygon.Create;
      If (Face.Material <> Nil) And
         (Face.Material.Surface.Diffuse.Texture <> Nil) Then
       Polygon.Texture := ExtractFileNameNoExt(Face.Material.Surface.Diffuse.Texture.FileName);
      SetLength(Polygon.Vertices,High(Face.Points) + 1);
      SetLength(Polygon.TX,High(Face.TexCoords) + 1);
      SetLength(Polygon.TZ,High(Face.TexCoords) + 1);
      For L := 0 To High(Polygon.Vertices) Do Polygon.Vertices[L] := Face.Points[L];
      For L := 0 To High(Polygon.TX) Do
      Begin
        Polygon.TX[L] := TAn8TexCoord(An8Mesh.TexCoords.Objects[Face.TexCoords[L]]).TX;
        Polygon.TZ[L] := TAn8TexCoord(An8Mesh.TexCoords.Objects[Face.TexCoords[L]]).TZ;
        If (Face.Material <> Nil) And
           (Face.Material.Surface.Diffuse.Texture <> Nil) And
           FileExists(ExtractFilePath(Application.ExeName) + 'library\textures\' + ExtractFileNameNoExt(Face.Material.Surface.Diffuse.Texture.FileName) + '.bmp') Then
        Begin
          St := ExtractFileNameNoExt(Face.Material.Surface.Diffuse.Texture.FileName);
          M  := Textures.IndexOf(St);
          If M >= 0 Then Texture := TTexture(Textures.Objects[M]) Else
          Begin
            Texture := TTexture.Create(Nil,Nil);
            Texture.LoadTexture(ExtractFilePath(Application.ExeName) + 'library\textures\' + St + '.bmp','',False);
            Textures.AddObject(St,Texture);
          End;
{
          If Texture <> Nil Then
          Begin
            Polygon.TX[L] := TAn8TexCoord(An8Mesh.TexCoords.Objects[Face.TexCoords[L]]).TX;
            Polygon.TZ[L] := TAn8TexCoord(An8Mesh.TexCoords.Objects[Face.TexCoords[L]]).TZ;
          End;
}
        End;
      End; // For L
      Polygon.HasTexCoords := (High(Polygon.TX) = High(Polygon.Vertices));
      Mesh.Polygons.AddObject('',Polygon);
    End; // For K
    P2.Free;
    If GO <> Nil Then GO.Group(Mesh);
    Result := Mesh;
  End; // ProcessMesh

  Procedure ProcessGroup(An8Group: TAn8Group; GO: TGroupObject);
  Var
    I         : Integer;
    Component : TAn8Component;
    GO1       : TGroupObject;

  Begin
    GO1 := TGroupObject.Create(An8Group.Name);
    GO1.Loc.Copy(An8Group.Base.Origin.X,An8Group.Base.Origin.Y,An8Group.Base.Origin.Z);
    For I := 0 To An8Group.Components.Count - 1 Do
    Begin
      Component := TAn8Component(An8Group.Components.Objects[I]);
      If Component <> Nil Then
      Begin
             If Component Is TAn8Mesh  Then ProcessMesh(TAn8Mesh(Component),GO1)
        Else If Component Is TAn8Group Then ProcessGroup(TAn8Group(Component),GO1);
      End;
    End; // For I
    If GO <> Nil Then GO.Group(GO1);
  End; // ProcessGroup

Begin
  Result := Nil;
  MinPt  := Points3D.T3DPoint.Create;
  MaxPt  := Points3D.T3DPoint.Create;

  Textures := TStringList.Create;
  Textures.Sorted := True;

  // First find out how many meshes there are

  ObjectCount := 0;
  FoundGroup  := False;
  For I := 0 To List.Count - 1 Do
  Begin
    An8Obj := TAn8Object(List.Objects[I]);
    If An8Obj <> Nil Then
    Begin
      Inc(ObjectCount,An8Obj.Components.Count);
      For J := 0 To An8Obj.Components.Count - 1 Do
      Begin
        Component := TAn8Component(An8Obj.Components.Objects[J]);
        If (Component <> Nil) And (Component Is TAn8Group) Then FoundGroup := True;
      End; // For J
    End;
  End; // For I

  // If there is more than one mesh then the result is a group object

  If (ObjectCount > 1) Or FoundGroup Then
  Begin
    GO     := TGroupObject.Create;
    Result := GO;
  End
  Else GO := Nil;

  // Iterate through all objects in the file to determine the center of the file

  If Center Then
  Begin
    An8File.GetBounds(MinPt,MaxPt);
    MaxPt.Add(MinPt);
    MaxPt.Divide(2);
  End;

  // Create objects for everything in the file

  Mesh := Nil;
  For I := 0 To List.Count - 1 Do
  Begin
    An8Obj := TAn8Object(List.Objects[I]);
    If An8Obj <> Nil Then
    Begin
      For J := 0 To An8Obj.Components.Count - 1 Do
      Begin
        Component := TAn8Component(An8Obj.Components.Objects[J]);
        If Component <> Nil Then
        Begin
               If Component Is TAn8Mesh  Then Mesh := ProcessMesh(TAn8Mesh(Component),GO)
          Else If Component Is TAn8Group Then ProcessGroup(TAn8Group(Component),GO);
        End;
      End; // For J
    End;
  End; // For I
  If GO = Nil Then Result := Mesh;
  For I := 0 To Textures.Count - 1 Do Textures.Objects[I].Free;
  Textures.Free;
  MinPt.Free;
  MaxPt.Free;

  J  := 0;
  St := BaseName;
  If St = '' Then St := mesh3DSMesh;
  If NameExists(St) Then
  Begin
    While NameExists(St + IntToStr(J)) Do Inc(J);
    St := St + IntToStr(J);
  End;
  Result.FName := St;
End; // TZone.ImportAn8Objects

Function TZone.ImportObjectFromAN8File(FileName: String; Rotate,Center: Boolean): TZoneObject;
Var
  I       : Integer;
  St      : String;
  An8File : TAn8File;
  List    : TStringList;

Begin
  Result := Nil;
  If FileExists(FileName) Then
  Begin
    An8File := TAn8File.Create;
    An8File.LoadFromFile(FileName);
    For I := 0 To An8File.Figures.Count - 1 Do TAn8Figure(An8File.Figures.Objects[I]).DeterminePrimaryBones;

    List := TStringList.Create;
    For I := 0 To An8File.Objects.Count - 1 Do List.AddObject('',An8File.Objects.Objects[I]);
    Result := ImportAn8Objects(ExtractFileNameNoExt(FileName),An8File,List,Rotate,Center);

    An8File.Free;
  End;
End; // TZone.ImportObjectFromAN8File

Function TZone.ImportObjectFromDirectXFile(FileName: String; Rotate,Center: Boolean): TZoneObject;
Var
  I       : Integer;
  St      : String;
  DXFile  : TDirectXFile;
  An8File : TAn8File;
  List    : TStringList;

Begin
  Result := Nil;
  If FileExists(FileName) Then
  Begin
    DXFile := TDirectXFile.Create;
    DXFile.LoadFromFile(FileName);
    An8File := DXFile.CreateAn8File;
    For I := 0 To An8File.Figures.Count - 1 Do TAn8Figure(An8File.Figures.Objects[I]).DeterminePrimaryBones;

    List := TStringList.Create;
    For I := 0 To An8File.Objects.Count - 1 Do List.AddObject('',An8File.Objects.Objects[I]);
    Result := ImportAn8Objects(ExtractFileNameNoExt(FileName),An8File,List,Rotate,Center);

    An8File.Free;
    DXFile.Free;
  End;
End; // TZone.ImportObjectFromDirectXFile

Procedure TZone.ImportFromAN8File(FileName: String; Rotate: Boolean);
Var
  I  : Integer;
  ZO : TZoneObject;
  St : String;

Begin
  ZO := ImportObjectFromAn8File(FileName,Rotate,False);
  If ZO <> Nil Then
  Begin
    // Let's find a unique name for the object

    I  := 0;
    St := Trim(ExtractFileNameNoExt(FileName));
    If St = '' Then St := mesh3DSMesh;
    If NameExists(St) Then
    Begin
      While NameExists(St + IntToStr(I)) Do Inc(I);
      St := St + IntToStr(I);
    End;
    ZO.FName := St;
    AddObject(St,ZO);
  End;
End; // TZone.ImportFromAN8File

Procedure TZone.ImportFromDirectXFile(FileName: String; Rotate: Boolean);
Var
  I  : Integer;
  ZO : TZoneObject;
  St : String;

Begin
  ZO := ImportObjectFromDirectXFile(FileName,Rotate,False);
  If ZO <> Nil Then
  Begin
    // Let's find a unique name for the object

    I  := 0;
    St := Trim(ExtractFileNameNoExt(FileName));
    If St = '' Then St := mesh3DSMesh;
    If NameExists(St) Then
    Begin
      While NameExists(St + IntToStr(I)) Do Inc(I);
      St := St + IntToStr(I);
    End;
    ZO.FName := St;
    AddObject(St,ZO);
  End;
End; // TZone.ImportFromDirectXFile

Procedure TZone.AddAn8BodyHeadPair(An8: TAn8File; NewMeshes: TStringList; Obj: TAn8Object; FigureIndex,BodyIndex,HeadIndex: Integer; SeparateHeads,IncludeEquipment: Boolean);
Var
  I         : Integer;
  Component : TAn8Component;
  HeadMesh  : TAn8Mesh;
  BodyMesh  : TAn8Mesh;
  Figure    : TAn8Figure;

  Procedure ProcessMesh(Mesh: TAn8Mesh; Figure: TAn8Figure; IncludeEquipment: Boolean);
  Var
    J,L       : Integer;
    Bone      : TAn8Bone;
    Bone0     : TAn8Bone;
    Bone1     : TAn8Bone;
    Component : TAn8Component;
    Point     : Points3D.T3DPoint;
    BoneMesh  : TAn8Mesh;

  Begin
    // Before we do anything, see if this mesh should be skipped because it is for a piece of equipment

    If (UpperCase(Copy(Mesh.Name,1,10)) <> 'EQUIPMENT_') Or IncludeEquipment Then
    Begin
      // First look for a bone that has the same name as the mesh. If there is a match, just use that bone

      Bone0 := TAn8Figure(An8.Figures.Objects[FigureIndex]).FindBoneByName(Mesh.Name);
      For L := 0 To Mesh.PrimaryBones.Count - 1 Do
      Begin
        // Start with the primary bone

        Bone  := Bone0;
        Bone1 := TAn8Bone(Mesh.PrimaryBones.Objects[L]);

        // If the primary bone is the root bone then we need to find an appropriate one

        If (Bone1 <> Nil) And (Bone1.Parent = Nil) And (Bone0 <> Nil) Then
        Begin
          Bone1 := Bone0;

          For J := 0 To Obj.Components.Count - 1 Do
          Begin
            Component := TAn8Component(Obj.Components.Objects[I]);
            If (Component <> Nil) And (Component Is TAn8Component) Then Component.FindAppropriateBone(Figure,Mesh,L,Bone1);
          End; // For J
        End;

        // We only want to change to the new bone if there is a mesh with the same name and that mesh contains a similar point.
        // We have to check to make sure we haven't exceeded the points list because the Anim8or file can contain more bone
        // weights than points, which will give us more PrimaryBones than points.

        If (Bone  <> Nil) And
           (Bone1 <> Nil) And
           (Bone <> Bone1) And
           (L < Mesh.Points.Count) {And
           ((Bone1 = Bone.Parent) Or
            (Bone.Children.IndexOfObject(Bone1) >= 0))} Then
        Begin
          Point := Points3D.T3DPoint(Mesh.Points.Objects[L]);
          BoneMesh := Obj.FindMeshByName(Bone1.Name);
          If (BoneMesh <> Nil) And (BoneMesh.Points.IndexOf(Point.GetCloseID(100)) >= 0) Then Bone := Bone1;
        End;

        // Reassign the primary bone

        If Bone <> Nil Then Mesh.PrimaryBones.Objects[L] := Bone;
      End; // For L
      Bone0 := TAn8Figure(An8.Figures.Objects[FigureIndex]).FindBoneByName('HE'); // Find the head bone
      Bone  := Mesh.GetMostUsedBone;
      If SeparateHeads And ((UpperCase(Mesh.Name) = 'HE') Or (UpperCase(Mesh.Name) = 'HEAD') Or ((Bone0 <> Nil) And Bone.IsAncestor(Bone0))) Then
      Begin
        // Add to the head mesh

        If (HeadIndex >= 0) And SeparateHeads Then HeadMesh.Add(Mesh);
      End
      Else
      Begin
        // Add to the body mesh

        If BodyIndex >= 0 Then BodyMesh.Add(Mesh);
      End;
    End;
  End; // ProcessMesh

  Procedure ProcessGroup(Group: TAn8Group; Figure: TAn8Figure; IncludeEquipment: Boolean);
  Var
    I         : Integer;
    Component : TAn8Component;

  Begin
    For I := 0 To Group.Components.Count - 1 Do
    Begin
      Component := TAn8Component(Group.Components.Objects[I]);
      If Component <> Nil Then
      Begin
             If Component Is TAn8Mesh  Then ProcessMesh(TAn8Mesh(Component),Figure,IncludeEquipment)
        Else If Component Is TAn8Group Then ProcessGroup(TAn8Group(Component),Figure,IncludeEquipment);
      End;
    End; // For I
  End; // ProcessGroup

Begin
  // Create a head mesh if we have to

  If (HeadIndex >= 0) And SeparateHeads Then
  Begin
    HeadMesh := TAn8Mesh.Create(An8,'HE' + Format('%.2d',[HeadIndex]),Obj,Nil); // Head
    NewMeshes.AddObject('',HeadMesh); // Head
  End
  Else HeadMesh := Nil;

  // Create a body mesh if we have to 

  If BodyIndex >= 0 Then
  Begin
    BodyMesh := TAn8Mesh.Create(An8,'',Obj,Nil); // Body
    If BodyIndex = 0
     Then BodyMesh.Name := ''
     Else BodyMesh.Name := Format('%.2d',[BodyIndex]);
    NewMeshes.AddObject('',BodyMesh);  // Body
  End
  Else BodyMesh := Nil;

  // Scan all of the components in the object

  Figure := TAn8Figure(An8.Figures.Objects[FigureIndex]);
  For I := 0 To Obj.Components.Count - 1 Do
  Begin
    Component := TAn8Component(Obj.Components.Objects[I]);
    If Component <> Nil Then
    Begin
           If Component Is TAn8Mesh  Then ProcessMesh(TAn8Mesh(Component),Figure,IncludeEquipment)
      Else If Component Is TAn8Group Then ProcessGroup(TAn8Group(Component),Figure,IncludeEquipment);
    End;
  End; // For I

  // Get rid of any empty meshes

  While (NewMeshes.Count > 0) And (TAn8Mesh(NewMeshes.Objects[NewMeshes.Count - 1]).Points.Count = 0) Do
  Begin
    NewMeshes.Objects[NewMeshes.Count - 1].Free;
    NewMeshes.Delete(NewMeshes.Count - 1);
  End; // While
End; // TZone.AddAn8BodyHeadPair

Procedure TZone.BuildAn8ObjectList(An8: TAn8File; ObjectList: TStringList; Out SeparateHeads: Boolean);
Var
  I,J,K     : Integer;
  St        : String;
  BodyMesh  : Integer;
  BodyTex   : Integer;
  HeadMesh  : Integer;
  HeadTex   : Integer;
  Obj       : TAn8Object;
  Component : TAn8Component;

  Procedure ProcessMesh(Mesh: TAn8Mesh; Out SeparateHeads: Boolean);
  Begin
    // 'head' is used when we want a separate head mesh but don't want the entire mesh to be assigned to a single bone
    // (e.g. parts of the head have their own bones and move independently)

    If (UpperCase(Mesh.Name) = 'HE') Or (UpperCase(Mesh.Name) = 'HEAD') Then SeparateHeads := True;
  End; // ProcessMesh

  Procedure ProcessGroup(Group: TAn8Group; Out SeparateHeads: Boolean);
  Var
    I         : Integer;
    Component : TAn8Component;

  Begin
    For I := 0 To Group.Components.Count - 1 Do
    Begin
      Component := TAn8Component(Group.Components.Objects[I]);
      If Component <> Nil Then
      Begin
             If Component Is TAn8Mesh  Then ProcessMesh(TAn8Mesh(Component),SeparateHeads)
        Else If Component Is TAn8Group Then ProcessGroup(TAn8Group(Component),SeparateHeads);
      End;
    End; // For I
  End; // ProcessGroup

Begin
  // If there is only one object then it's pretty obvious what to do

  SeparateHeads := False;
  If An8.Objects.Count = 1 Then
  Begin
    ObjectList.AddObject(An8.Objects.Strings[0],An8.Objects.Objects[0]);
    I := 0;
  End
  Else
  Begin
    // Find the object that is called "body0000head0000"

    I := An8.Objects.IndexOf('body0000_head0000');
    If I >= 0 Then
    Begin
      ObjectList.AddObject(An8.Objects.Strings[I],An8.Objects.Objects[I]);

      // Add the rest, skipping the initial one

      For J := 0 To An8.Objects.Count - 1 Do
      Begin
        If J <> I Then
        Begin
          ObjectList.AddObject(An8.Objects.Strings[J],An8.Objects.Objects[J]);
          St := An8.Objects.Strings[J];
          If TokenizeAn8ObjectName(St,BodyMesh,BodyTex,HeadMesh,HeadTex) And
             ((HeadMesh <> 0) Or (HeadTex <> 0)) Then SeparateHeads := True;
        End;
      End; // For J
    End
    Else
    Begin
      // We should never get here according to the naming convention: just punt and add them all

      For J := 0 To An8.Objects.Count - 1 Do ObjectList.AddObject(An8.Objects.Strings[J],An8.Objects.Objects[J]);
    End;
  End;

  // If a mesh has the name "HE" or "HEAD" then we are using separate head meshes

  If I >= 0 Then
  Begin
    Obj := TAn8Object(An8.Objects.Objects[I]);
    J   := 0;
    While (J < Obj.Components.Count) And Not SeparateHeads Do
    Begin
      Component := TAn8Component(Obj.Components.Objects[J]);
      If Component <> Nil Then
      Begin
             If Component Is TAn8Mesh  Then ProcessMesh(TAn8Mesh(Component),SeparateHeads)
        Else If Component Is TAn8Group Then ProcessGroup(TAn8Group(Component),SeparateHeads);
      End;
      Inc(J);
    End; // While
  End;
End; // TZone.BuildAn8ObjectList

Procedure TZone.BuildEquipmentMeshes(An8: TAn8File; NewMeshes: TStringList);
Var
  I,J       : Integer;
  Obj       : TAn8Object;
  Component : TAn8Component;

  Procedure ProcessMesh(Mesh: TAn8Mesh; NewMeshes: TStringList);
  Begin
    If (UpperCase(Copy(Mesh.Name,1,10)) = 'EQUIPMENT_') Then NewMeshes.AddObject(Mesh.Name,Mesh);
  End; // ProcessMesh

  Procedure ProcessGroup(Group: TAn8Group; NewMeshes: TStringList);
  Var
    I         : Integer;
    Component : TAn8Component;

  Begin
    For I := 0 To Group.Components.Count - 1 Do
    Begin
      Component := TAn8Component(Group.Components.Objects[I]);
      If Component <> Nil Then
      Begin
             If Component Is TAn8Mesh  Then ProcessMesh(TAn8Mesh(Component),NewMeshes)
        Else If Component Is TAn8Group Then ProcessGroup(TAn8Group(Component),NewMeshes);
      End;
    End; // For I
  End; // ProcessGroup

Begin
  NewMeshes.Clear;
  For I := 0 To An8.Objects.Count - 1 Do
  Begin
    Obj := TAn8Object(An8.Objects.Objects[I]);
    For J := 0 To Obj.Components.Count - 1 Do
    Begin
      Component := TAn8Component(Obj.Components.Objects[J]);
      If Component <> Nil Then
      Begin
             If Component Is TAn8Mesh  Then ProcessMesh(TAn8Mesh(Component),NewMeshes)
        Else If Component Is TAn8Group Then ProcessGroup(TAn8Group(Component),NewMeshes);
      End;
    End; // For J
  End; // For I
End;

Procedure TZone.BuildAn8HeadBodyMeshes(An8: TAn8File; ObjectList,NewMeshes: TStringList; FigureIndex: Integer; SeparateHeads,IncludeEquipment: Boolean;
                                       Out BaseMeshCount: Integer; Out ModelHeight,ModelMaxLen: Single; Out HeadCount,BodyCount: Integer);
Var
  I,J       : Integer;
  Found     : Boolean;
  BaseIndex : Array[0..1] Of Integer;
  MaxPt     : Points3D.T3DPoint;
  MinPt     : Points3D.T3DPoint;
  MaxPt0    : Points3D.T3DPoint;
  MinPt0    : Points3D.T3DPoint;
  Point     : Points3D.T3DPoint;
  Mesh      : TAn8Mesh;
  BodyMesh  : Integer;
  BodyTex   : Integer;
  HeadMesh  : Integer;
  HeadTex   : Integer;

Begin
  // Combine the meshes for the first object into a head mesh and a body mesh

  AddAn8BodyHeadPair(An8,NewMeshes,TAn8Object(ObjectList.Objects[0]),FigureIndex,0,0,SeparateHeads,IncludeEquipment);

  BaseMeshCount := NewMeshes.Count;

  // Look for any other head meshes

  For I := 1 To 4 Do
  Begin
    J     := 0;
    Found := False;
    While (J < ObjectList.Count) And Not Found Do
    Begin
      If TokenizeAn8ObjectName(ObjectList.Strings[J],BodyMesh,BodyTex,HeadMesh,HeadTex) And (HeadMesh = I) Then
      Begin
        Found := True;
        AddAn8BodyHeadPair(An8,NewMeshes,TAn8Object(ObjectList.Objects[J]),FigureIndex,-1,I,SeparateHeads,IncludeEquipment);
      End
      Else Inc(J);
    End; // While
  End; // For I

  // Move the body mesh to the end so that all the head meshes will be together
  // (and consequently all the body meshes, too)

  BaseIndex[0] := 0;
  If BaseMeshCount = 2 Then
  Begin
    BaseIndex[1] := NewMeshes.Count - 1;
    NewMeshes.AddObject(NewMeshes.Strings[1],NewMeshes.Objects[1]);
    NewMeshes.Delete(1);
  End;

  // Establish how many head meshes there are

  HeadCount := NewMeshes.Count - 1;

  // If there are 4 or more heads then move the last one to the start

  If (BaseMeshCount = 2) And (NewMeshes.Count >= 5) Then
  Begin
    NewMeshes.InsertObject(0,NewMeshes.Strings[HeadCount - 1],NewMeshes.Objects[HeadCount - 1]);
    NewMeshes.Delete(HeadCount);
    BaseIndex[0] := 1;
  End;

  // Establish how many body meshes there are

  BodyCount := 1;

  // Look for any other body meshes (e.g. robed mesh)

  I := 1;
  Repeat
    J     := 0;
    Found := False;
    While (J < ObjectList.Count) And Not Found Do
    Begin
      If TokenizeAn8ObjectName(ObjectList.Strings[J],BodyMesh,BodyTex,HeadMesh,HeadTex) And (BodyMesh = I) Then
      Begin
        Found := True;
        Inc(BodyCount);
        AddAn8BodyHeadPair(An8,NewMeshes,TAn8Object(ObjectList.Objects[J]),FigureIndex,I,-1,SeparateHeads,IncludeEquipment);
      End
      Else Inc(J);
    End; // While
    Inc(I);
  Until Not Found;

  // If a robed mesh was found, move it so it's after the first head mesh.  We're using a
  // check on separate head meshes as well because we want to allow multiple (non-robed)
  // body meshes for non-player models (to do this we must disallow separate head meshes
  // for them).

  If (BodyCount = 2) And SeparateHeads Then
  Begin
    I := NewMeshes.Count - 1;
    NewMeshes.InsertObject(1,NewMeshes.Strings[I],NewMeshes.Objects[I]);
    NewMeshes.Delete(I + 1);
    Inc(BaseIndex[0]);
    Inc(BaseIndex[1]);
  End;

  // Encode and add the meshes

  MaxPt  := Points3D.T3DPoint.Create;
  MinPt  := Points3D.T3DPoint.Create;
  MaxPt0 := Points3D.T3DPoint.Create(-99999,-99999,-99999);
  MinPt0 := Points3D.T3DPoint.Create(99999,99999,99999);

  // Determine the model height, only looking at the first body-head pair

  ModelHeight := 0;
  For I := 0 To BaseMeshCount - 1 Do
  Begin
    Mesh := TAn8Mesh(NewMeshes.Objects[BaseIndex[I]]);
    For J := 0 To Mesh.Points.Count - 1 Do
    Begin
      Point := Points3D.T3DPoint(Mesh.Points.Objects[J]);
      If (I = 0) And (J = 0) Then
      Begin
        MinPt.Copy(Point);
        MaxPt.Copy(Point);
      End
      Else
      Begin
        If Point.X < MinPt.X Then MinPt.X := Point.X;
        If Point.Y < MinPt.Y Then MinPt.Y := Point.Y;
        If Point.Z < MinPt.Z Then MinPt.Z := Point.Z;
        If Point.X > MaxPt.X Then MaxPt.X := Point.X;
        If Point.Y > MaxPt.Y Then MaxPt.Y := Point.Y;
        If Point.Z > MaxPt.Z Then MaxPt.Z := Point.Z;
      End;
    End; // For J
  End; // For I
  ModelHeight := MaxPt.Y - MinPt.Y;
  ModelMaxLen := Max(MaxPt.X - MinPt.X,Max(MaxPt.Y - MinPt.Y,MaxPt.Z - MinPt.Z));
End; // TZone.BuildAn8HeadBodyMeshes

Procedure TZone.SortAn8VerticesByBone(An8: TAn8File; FigureIndex: Integer; Mesh: TAn8Mesh; PreloadedList: Boolean);
Var
  I          : Integer;
  VertSorter : TQuickSorterProc;

  Function CompareAn8Vertices(Index0,Index1: Integer): Integer;

    Function GetVertIndex(Index: Integer): Integer;
    Begin
      Result := TAn8Bone(SortAn8Mesh.PrimaryBones.Objects[VertIndex[Index]]).Index;
    End; // GetVertIndex

  Begin
    Result := GetVertIndex(Index0) - GetVertIndex(Index1);
  End; // CompareAn8Vertices

  Procedure ExchangeAn8Vertices(Index0,Index1: Integer);
  Var I: Integer;
  Begin
    I                 := VertIndex[Index0];
    VertIndex[Index0] := VertIndex[Index1];
    VertIndex[Index1] := I;
  End; // ExchangeAn8Vertices

Begin
  If Mesh <> Nil Then
  Begin
    If Not PreloadedList Then
    Begin
      SetLength(VertIndex,Mesh.Points.Count);
      For I := 0 To High(VertIndex) Do VertIndex[I] := I;
    End;
    If (An8 <> Nil) And
       (FigureIndex >= 0) And
       (FigureIndex < An8.Figures.Count) And
       (TAn8Figure(An8.Figures.Objects[FigureIndex]).RootBone <> Nil) Then
    Begin
      SortAn8Mesh         := Mesh;
      VertSorter          := TQuickSorterProc.Create;
      VertSorter.Compare  := @CompareAn8Vertices;
      VertSorter.Exchange := @ExchangeAn8Vertices;
      VertSorter.Sort(0,High(VertIndex));
      VertSorter.Free;
    End;
  End
  Else SetLength(VertIndex,0);
End; // TZone.SortAn8VerticesByBone

Procedure TZone.SortAn8FacesByTexture(Mesh: TAn8Mesh; Textures: TStringList);
Var
  I             : Integer;
  TextureSorter : TQuickSorterProc;

  Function CompareAn8Textures(Index0,Index1: Integer): Integer;

    Function GetTexIndex(Index: Integer): Integer;
    Var
      Poly : TAn8Face;
      St   : String;

    Begin
      Poly := TAn8Face(SortAn8Mesh.Faces.Objects[TexIndex[Index]]);
      If (Poly.Material.Surface.Diffuse.Texture <> Nil) And
         (Poly.Material.Surface.Diffuse.Texture.FileName <> '')
       Then St := Poly.Material.Surface.Diffuse.Texture.Name
       Else St := TransparentTexture;
      Result := SortTexList.IndexOf(St);
    End; // GetTexIndex

  Begin
    Result := GetTexIndex(Index0) - GetTexIndex(Index1);
  End; // CompareAn8Textures

  Procedure ExchangeAn8Textures(Index0,Index1: Integer);
  Var I: Integer;
  Begin
    I                := TexIndex[Index0];
    TexIndex[Index0] := TexIndex[Index1];
    TexIndex[Index1] := I;
  End; // ExchangeAn8Textures

Begin
  If Mesh <> Nil Then
  Begin
    SetLength(TexIndex,Mesh.Faces.Count);
    For I := 0 To High(TexIndex) Do TexIndex[I] := I;

    SortAn8Mesh := Mesh;
    SortTexList := Textures;

    TextureSorter          := TQuickSorterProc.Create;
    TextureSorter.Compare  := @CompareAn8Textures;
    TextureSorter.Exchange := @ExchangeAn8Textures;
    TextureSorter.Sort(0,Mesh.Faces.Count - 1);
    TextureSorter.Free;
  End
  Else SetLength(TexIndex,0);
End; // TZone.SortAn8FacesByTexture

Procedure TZone.AddWaterToMesh(Mesh: TMeshObject);
Var
  I,J,K  : Integer;
  P      : TPolygon;
  CX,CY  : Single;
  X1,Y1  : Single;
  X2,Y2  : Single;
  Normal : T3DPoint;
  St     : String;

  Procedure AddDuplicatePolygon(P: TPolygon);
  // Makes a duplicate that faces down so it can be seen from underwater (we have to have separate vertices
  // or the face/vertex normals get screwed up)
  Var
    I,J,K  : Integer;
    P1     : TPolygon;
    Normal : T3DPoint;

  Begin
    If High(P.Vertices) >= 0 Then
    Begin
      P1 := TPolygon.Create(P);
      K  := Mesh.Vertices.Count;
      For J := High(P.Vertices) DownTo 0 Do
       Mesh.Vertices.AddObject('',T3DPoint.Create(T3DPoint(Mesh.Vertices.Objects[P.Vertices[J]])));
      For I := 0 To High(P1.Vertices) Do P1.Vertices[I] := K + I;
      Mesh.Polygons.AddObject('',P1);
      Normal := P1.GetNormal(Mesh);
      For I := 0 To High(P1.Vertices) Do Mesh.Normals.AddObject('',T3DPoint.Create(Normal));
      Normal.Free;
    End;
  End; // AddDuplicatePolygon

  Function CreateBasicWaterPolygon: TPolygon;
  Var
    P : TPolygon;
    I : Integer;

  Begin
    P          := TPolygon.Create;
    P.HasSolid := True;
    P.Solid    := False;
    If Water[J].SemiTrans Then P.TextureState := tsSemiTransparent;
    If Water[J].Tinted Then
    Begin
      P.HasColor := True;
      SetLength(P.Colors,High(P.Vertices) + 1);
      For I := 0 To High(P.Colors) Do P.Colors[I] := Water[J].Color;
    End;
    Result := P;
  End; // CreateBasicWaterPolygon

Begin
  If High(Water) >= 0 Then
  Begin
    For J := 0 To High(Water) Do
    Begin
      St := '';
      If Water[J].WType <> wtPvP Then
      Begin
        For K := 0 To High(Water[J].Tex) Do
        Begin
          If St <> '' Then St := St + ';';
          St := St + Water[J].Tex[K];
        End; // For K
        St := St + '+' + FloatToStr(Water[J].AnimTime);
      End;

      If Water[J].WType <> wtPvP Then
      Begin
        Case Water[J].Shape Of
          wsElliptical:
          Begin
            CX := Water[J].MinX + Water[J].XSize / 2;
            CY := Water[J].MinY + Water[J].YSize / 2;

            X1 := (Water[J].XSize / 2) * Cos(-0.5 * (2 * Pi) / EllipticalSections);
            Y1 := (Water[J].YSize / 2) * Sin(-0.5 * (2 * Pi) / EllipticalSections);

            For K := 0 To EllipticalSections - 1 Do
            Begin
              I := Mesh.Vertices.Count;

              X2 := (Water[J].XSize / 2) * Cos((K + 0.5) * (2 * Pi) / EllipticalSections);
              Y2 := (Water[J].YSize / 2) * Sin((K + 0.5) * (2 * Pi) / EllipticalSections);

              Mesh.Vertices.AddObject('',T3DPoint.Create(CX,CY,Water[J].Level));
              Mesh.Vertices.AddObject('',T3DPoint.Create(CX + X1,CY + Y1,Water[J].Level));
              Mesh.Vertices.AddObject('',T3DPoint.Create(CX + X2,CY + Y2,Water[J].Level));

              P := CreateBasicWaterPolygon;
              P.Texture := St;
              SetLength(P.Vertices,3);
              P.Vertices[0] := I;
              P.Vertices[1] := I + 1;
              P.Vertices[2] := I + 2;
              Mesh.Polygons.AddObject('',P);
              Normal := P.GetNormal(Mesh);
              For I := 0 To High(P.Vertices) Do Mesh.Normals.AddObject('',T3DPoint.Create(Normal));
              Normal.Free;
              AddDuplicatePolygon(P);
              X1 := X2;
              Y1 := Y2;
            End; // For K
          End;
          wsRectangular:
          Begin
            I := Mesh.Vertices.Count;
            Mesh.Vertices.AddObject('',T3DPoint.Create(Water[J].MinX,Water[J].MinY,Water[J].Level));
            Mesh.Vertices.AddObject('',T3DPoint.Create(Water[J].MinX,Water[J].MinY + Water[J].YSize,Water[J].Level));
            Mesh.Vertices.AddObject('',T3DPoint.Create(Water[J].MinX + Water[J].XSize,Water[J].MinY + Water[J].YSize,Water[J].Level));
            Mesh.Vertices.AddObject('',T3DPoint.Create(Water[J].MinX + Water[J].XSize,Water[J].MinY,Water[J].Level));
            P := CreateBasicWaterPolygon;
            P.Texture := St;
            SetLength(P.Vertices,4);
            P.Vertices[0] := I;
            P.Vertices[1] := I + 1;
            P.Vertices[2] := I + 2;
            P.Vertices[3] := I + 3;
            Mesh.Polygons.AddObject('',P);
            Normal := P.GetNormal(Mesh);
            For I := 0 To High(P.Vertices) Do Mesh.Normals.AddObject('',T3DPoint.Create(Normal));
            Normal.Free;
            AddDuplicatePolygon(P);
          End;
          wsIrregular:
          Begin
            If High(Water[J].Irregular) >= 0 Then
            Begin
              CX := 0;
              CY := 0;
              For I := 0 To High(Water[J].Irregular) Do
              Begin
                CX := CX + Water[J].Irregular[I].X1;
                CY := CY + Water[J].Irregular[I].Y1;
              End; // For I
              CX := CX / (High(Water[J].Irregular) + 1);
              CY := CY / (High(Water[J].Irregular) + 1);
              For I := 0 To High(Water[J].Irregular) Do
              Begin
                Mesh.Vertices.AddObject('',T3DPoint.Create(CX,CY,Water[J].Level));
                Mesh.Vertices.AddObject('',T3DPoint.Create(Water[J].Irregular[I].X2,Water[J].Irregular[I].Y2,Water[J].Level));
                Mesh.Vertices.AddObject('',T3DPoint.Create(Water[J].Irregular[I].X1,Water[J].Irregular[I].Y1,Water[J].Level));
                K := Mesh.Vertices.Count;
                P := CreateBasicWaterPolygon;
                P.Texture := St;
                SetLength(P.Vertices,3);
                P.Vertices[0] := K - 3;
                P.Vertices[1] := K - 2;
                P.Vertices[2] := K - 1;
                Mesh.Polygons.AddObject('',P);
                Normal := P.GetNormal(Mesh);
                For K := 0 To High(P.Vertices) Do Mesh.Normals.AddObject('',T3DPoint.Create(Normal));
                Normal.Free;
                AddDuplicatePolygon(P);
              End; // For I
            End;
          End;
        End; // Case
      End;
    End; // For J
  End;
End; // TZone.AddWaterToMesh

Function TZone.GetAllLightSources: TStringList;
// Creates a TStringList and populates it with TLightObjects
Var
  I    : Integer;
  List : TStringList;

  Procedure CheckObject(ZO: TZoneObject);
  Var
    L  : TLightObject;
    GO : TGroupObject;
    ML : TMeshLibraryObjectReference;
    I  : Integer;
    V  : T3DPoint;

  Begin
    If ZO Is TLightObject Then
    Begin
      L := TLightObject.Create(TLightObject(ZO));
      V := ZO.GetAbsoluteLocation;
      L.FLoc.Copy(V);
      List.AddObject(ZO.FName,L);
      V.Free;
    End
    Else If ZO Is TMeshLibraryObjectReference Then
    Begin
      ML := TMeshLibraryObjectReference(ZO);
      If ML.Group <> Nil Then// CheckObject(ML.Group);
      Begin
        GO         := TGroupObject.Create(ML.Group);
        GO.FParent := ZO.FParent;
        GO.FZone   := Self;
        GO.FLoc.Copy(ML.FLoc);
        GO.FHeading.Copy(ML.FHeading);
        GO.FSize.Copy(ML.FSize);
        CheckObject(GO);
        GO.Free;
      End;
    End
    Else If ZO Is TGroupObject Then
    Begin
      GO := TGroupObject(ZO);
      For I := 0 To GO.Objects.Count - 1 Do CheckObject(TZoneObject(GO.Objects.Objects[I]));
    End;
  End; // CheckObject

Begin
  List := TStringList.Create;
  For I := 0 To Count - 1 Do CheckObject(TZoneObject(Objects[I]));
  Result := List;
End; // TZone.GetAllLightSources

Procedure TZone.AddBound(X1,Y1,Z1,X2,Y2,Z2: Single);
Var
  I  : Integer;
  MO : TMeshObject;
  P  : TPolygon;
  V1 : T3DPoint;
  V2 : T3DPoint;
  V3 : T3DPoint;

Begin
  Bounds.AddObject('',TZoneBound.Create(X1,Y1,Z1,X2,Y2,Z2));
  I := IndexOf(meshBoundingPolygons);
  If I >= 0 Then
  Begin
    MO := TMeshObject(Objects[I]);
    I  := MO.Vertices.Count;
    P  := TPolygon.Create([I,I + 1,I + 2,I + 3],BoundTex);
    P.TextureState := tsTransparent;
    If Z1 = Z2 Then
    Begin
      V1 := T3DPoint.Create(X1,Y1,BoundMinZ - BoundOffset);
      V2 := T3DPoint.Create(X1,Y1,BoundMaxZ + BoundOffset);
      V3 := T3DPoint.Create(X2,Y2,BoundMaxZ + BoundOffset);
    End
    Else
    Begin
      V1 := T3DPoint.Create(X1,Y1,Z1);
      V2 := T3DPoint.Create(X1,Y1,Z2);
      V3 := T3DPoint.Create(X2,Y2,Z2);
    End;
    MO.Vertices.AddObject('',T3DPoint.Create(V1.X,V1.Y,V1.Z));
    MO.Vertices.AddObject('',T3DPoint.Create(V2.X,V2.Y,V2.Z));
    MO.Vertices.AddObject('',T3DPoint.Create(V3.X,V3.Y,V3.Z));
    MO.Vertices.AddObject('',T3DPoint.Create(V3.X,V3.Y,V1.Z));
    MO.Polygons.AddObject('',P);

    // Calculate the normal and add it

    V1.Subtract(V2);
    V3.Subtract(V2);
    V1.Cross(V3);
    V1.Normalize;
    V2.Copy(V1);
    V3.Copy(V1);
    MO.Normals.AddObject('',V1);
    MO.Normals.AddObject('',V2);
    MO.Normals.AddObject('',V3);
  End
  Else BuildBoundingPolygons;
End; // TZone.AddBound

Procedure TZone.BreakUpBounds;
Var
  I,J : Integer;
  X,Y : Single;
  ZB1 : TZoneBound;
  ZB2 : TZoneBound;

Begin
  I := 0;
  While I < Bounds.Count - 1 Do
  Begin
    ZB1 := TZoneBound(Bounds.Objects[I]);
    J   := I + 1;
    While J < Bounds.Count Do
    Begin
      ZB2 := TZoneBound(Bounds.Objects[J]);
      If LineIntersection(ZB1.X1,ZB1.Y1,ZB1.X2,ZB1.Y2,
                          ZB2.X1,ZB2.Y1,ZB2.X2,ZB2.Y2,X,Y) = DO_INTERSECT Then
      Begin
        If Not (((Abs(X - ZB1.X1) < 0.1) And (Abs(Y - ZB1.Y1) < 0.1)) Or
                ((Abs(X - ZB1.X2) < 0.1) And (Abs(Y - ZB1.Y2) < 0.1)) Or
                ((Abs(X - ZB2.X1) < 0.1) And (Abs(Y - ZB2.Y1) < 0.1)) Or
                ((Abs(X - ZB2.X2) < 0.1) And (Abs(Y - ZB2.Y2) < 0.1))) Then
        Begin
          Bounds.AddObject('',TZoneBound.Create(X,Y,ZB1.Z1,ZB1.X2,ZB1.Y2,ZB1.Z2));
          ZB1.X2 := X;
          ZB1.Y2 := Y;

          Bounds.AddObject('',TZoneBound.Create(X,Y,ZB2.Z1,ZB2.X2,ZB2.Y2,ZB2.Z2));
          ZB2.X2 := X;
          ZB2.Y2 := Y;
        End;
      End;
      Inc(J);
    End; // While
    Inc(I);
  End; // While
End; // TZone.BreakUpBounds

Procedure TZone.BuildBoundingPolygons;
Var
  I,J         : Integer;
  ZO          : TZoneObject;
  MO          : TMeshObject;
  P           : TPolygon;
  B           : TZoneBound;
  MinPos      : T3DPoint;
  MaxPos      : T3DPoint;
  MinPos0     : T3DPoint;
  MaxPos0     : T3DPoint;
  X,Y         : Integer;
  X1,Y1       : Single;
  X2,Y2       : Single;
  Z1,Z2       : Single;
  ZTop        : Single;
  IX,IY       : Single;
  QuickSorter : TQuickSorterProc;

  Procedure AddIntersection(X1,Y1,X2,Y2,IX,IY: Single);
  Begin
    If ((IX <> X1) Or (IY <> Y1)) And ((IX <> X2) Or (IY <> Y2)) Then
    Begin
      SetLength(IntersectX,High(IntersectX) + 2);
      SetLength(IntersectY,High(IntersectY) + 2);
      SetLength(IntersectDist,High(IntersectDist) + 2);
      IntersectX[High(IntersectX)] := IX;
      IntersectY[High(IntersectY)] := IY;
      IntersectDist[High(IntersectDist)] := Sqrt(Sqr(IX - X1) + Sqr(IY - Y1));
    End;
  End; // AddIntersection

  Function CompareDistance(Index0,Index1: Integer): Integer;
  Begin
         If IntersectDist[Index0] < IntersectDist[Index1] Then Result := -1
    Else If IntersectDist[Index0] > IntersectDist[Index1] Then Result := 1
    Else Result := 0;
  End; // CompareDistance

  Procedure ExchangeDistance(Index0,Index1: Integer);
  Var S: Single;
  Begin
    S                  := IntersectX[Index0];
    IntersectX[Index0] := IntersectX[Index1];
    IntersectX[Index1] := S;

    S                  := IntersectY[Index0];
    IntersectY[Index0] := IntersectY[Index1];
    IntersectY[Index1] := S;

    S                     := IntersectDist[Index0];
    IntersectDist[Index0] := IntersectDist[Index1];
    IntersectDist[Index1] := S;
  End; // ExchangeDistance

Begin
  // If we have a BoundingBox object, get rid of it since we'll replace it with a BoundingPolygons one

  I := IndexOf(meshBoundingBox);
  If I >= 0 Then
  Begin
    ZO := TZoneObject(Objects[I]);
    ZO.Free;
    Delete(I);
  End;

  // If we have a BoundingPolygons object, get rid of it since we're going to regenerate it

  I := IndexOf(meshBoundingPolygons);
  If I >= 0 Then
  Begin
    ZO := TZoneObject(Objects[I]);
    ZO.Free;
    Delete(I);
  End;

  // Determine the zone bounds

  MinPos0  := T3DPoint.Create;
  MaxPos0  := T3DPoint.Create;
  BoundTex := '';
  For I := 0 To Count - 1 Do
  Begin
    ZO     := TZoneObject(Objects[I]);
    MinPos := Nil;
    MaxPos := Nil;
    ZO.GetSize(MinPos,MaxPos,BoundTex);
    If I = 0 Then
    Begin
      MinPos0.Copy(MinPos);
      MaxPos0.Copy(MaxPos);
    End
    Else
    Begin
      If MinPos.X < MinPos0.X Then MinPos0.X := MinPos.X;
      If MinPos.Y < MinPos0.Y Then MinPos0.Y := MinPos.Y;
      If MinPos.Z < MinPos0.Z Then MinPos0.Z := MinPos.Z;
      If MaxPos.X > MaxPos0.X Then MaxPos0.X := MaxPos.X;
      If MaxPos.Y > MaxPos0.Y Then MaxPos0.Y := MaxPos.Y;
      If MaxPos.Z > MaxPos0.Z Then MaxPos0.Z := MaxPos.Z;
    End;
    MinPos.Free;
    MaxPos.Free;
  End; // For I

  // Now build a new mesh object containaing all of the bounding polygons

  MO := TMeshObject.Create(meshBoundingPolygons);
  J  := 0;
  For I := 0 To Bounds.Count - 1 Do
  Begin
    B := TZoneBound(Bounds.Objects[I]);

    // For the entire ground mesh, build a list of intersections in X-Y space

    SetLength(IntersectX,0);
    SetLength(IntersectY,0);
    SetLength(IntersectDist,0);
    For X := 0 To ElevationGrid.NX - 1 Do
    Begin
      // Vertical spar

      X1 := ElevationGrid.MaxX;
      Y1 := ElevationGrid.MaxY - X * GridSize;
      X2 := ElevationGrid.MinX;
      Y2 := Y1;
      If LineIntersection(X1,Y1,X2,Y2,B.X1,B.Y1,B.X2,B.Y2,IX,IY) = DO_INTERSECT Then AddIntersection(B.X1,B.Y1,B.X2,B.Y2,IX,IY);

      If X < ElevationGrid.NX - 1 Then
      Begin
        // Diagonal spar

        Y  := Max((ElevationGrid.NX - 1) - X,ElevationGrid.NY - 1);
        X2 := X1 - Y * GridSize;
        Y2 := Y1 - Y * GridSize;
        If LineIntersection(X1,Y1,X2,Y2,B.X1,B.Y1,B.X2,B.Y2,IX,IY) = DO_INTERSECT Then AddIntersection(B.X1,B.Y1,B.X2,B.Y2,IX,IY);
      End;
    End; // For X

    For Y := 0 To ElevationGrid.NY - 1 Do
    Begin
      // Horizontal spar

      X1 := ElevationGrid.MaxX - Y * GridSize;
      Y1 := ElevationGrid.MaxY;
      X2 := X1;
      Y2 := ElevationGrid.MinY;
      If LineIntersection(X1,Y1,X2,Y2,B.X1,B.Y1,B.X2,B.Y2,IX,IY) = DO_INTERSECT Then AddIntersection(B.X1,B.Y1,B.X2,B.Y2,IX,IY);

      If (Y > 0) And (Y < ElevationGrid.NY - 1) Then
      Begin
        // Diagonal spar

        X  := Max((ElevationGrid.NY - 1) - Y,ElevationGrid.NX - 1);
        X2 := X1 - X * GridSize;
        Y2 := Y1 - X * GridSize;
        If LineIntersection(X1,Y1,X2,Y2,B.X1,B.Y1,B.X2,B.Y2,IX,IY) = DO_INTERSECT Then AddIntersection(B.X1,B.Y1,B.X2,B.Y2,IX,IY);
      End;
    End; // For Y

    // Are there any intersections?

    If High(IntersectX) >= 0 Then
    Begin
      // Sort the intersections in order of distance from the bound's first endpoint

      QuickSorter          := TQuickSorterProc.Create;
      QuickSorter.Compare  := @CompareDistance;
      QuickSorter.Exchange := @ExchangeDistance;
      QuickSorter.Sort(0,High(IntersectX));
      QuickSorter.Free;

      ZTop := MaxPos0.Z + BoundOffset;

      // Create a polygon from the first endpoint to the first intersection

      If ElevationGrid.CanGetHeightAtAbsolute(B.X1,B.Y1)
       Then Z1 := ElevationGrid.GetHeightAtAbsolute(B.X1,B.Y1)
       Else Z1 := MinPos0.Z - BoundOffset;
      Z2 := ElevationGrid.GetHeightAtAbsolute(IntersectX[0],IntersectY[0]);

      P := TPolygon.Create([J,J + 1,J + 2,J + 3],BoundTex);
      P.TextureState := tsTransparent;
      MO.Vertices.AddObject('',T3DPoint.Create(IntersectX[0],IntersectY[0],Z2));
      MO.Vertices.AddObject('',T3DPoint.Create(IntersectX[0],IntersectY[0],ZTop));
      MO.Vertices.AddObject('',T3DPoint.Create(B.X1,B.Y1,ZTop));
      MO.Vertices.AddObject('',T3DPoint.Create(B.X1,B.Y1,Z1));
      MO.Polygons.AddObject('',P);
      Inc(J,4);

      // Create polygons between all of the intersections

      For X := 0 To High(IntersectX) - 1 Do
      Begin
        Z1 := ElevationGrid.GetHeightAtAbsolute(IntersectX[X],IntersectY[X]);
        Z2 := ElevationGrid.GetHeightAtAbsolute(IntersectX[X + 1],IntersectY[X + 1]);
        P := TPolygon.Create([J,J + 1,J + 2,J + 3],BoundTex);
        P.TextureState := tsTransparent;
        MO.Vertices.AddObject('',T3DPoint.Create(IntersectX[X + 1],IntersectY[X + 1],Z2));
        MO.Vertices.AddObject('',T3DPoint.Create(IntersectX[X + 1],IntersectY[X + 1],ZTop));
        MO.Vertices.AddObject('',T3DPoint.Create(IntersectX[X],IntersectY[X],ZTop));
        MO.Vertices.AddObject('',T3DPoint.Create(IntersectX[X],IntersectY[X],Z1));
        MO.Polygons.AddObject('',P);
        Inc(J,4);
      End; // For X

      // Create a polygon from the last intersection to the last endpoint

      If ElevationGrid.CanGetHeightAtAbsolute(B.X2,B.Y2)
       Then Z2 := ElevationGrid.GetHeightAtAbsolute(B.X2,B.Y2)
       Else Z2 := MinPos0.Z - BoundOffset;
      Z1 := ElevationGrid.GetHeightAtAbsolute(IntersectX[High(IntersectX)],IntersectY[High(IntersectY)]);

      P := TPolygon.Create([J,J + 1,J + 2,J + 3],BoundTex);
      P.TextureState := tsTransparent;
      MO.Vertices.AddObject('',T3DPoint.Create(B.X2,B.Y2,Z2));
      MO.Vertices.AddObject('',T3DPoint.Create(B.X2,B.Y2,ZTop));
      MO.Vertices.AddObject('',T3DPoint.Create(IntersectX[High(IntersectX)],IntersectY[High(IntersectY)],ZTop));
      MO.Vertices.AddObject('',T3DPoint.Create(IntersectX[High(IntersectX)],IntersectY[High(IntersectY)],Z1));
      MO.Polygons.AddObject('',P);
      Inc(J,4);
    End
    Else
    Begin
      // If there are no intersections then just create a polygon for the bound

      P := TPolygon.Create([J,J + 1,J + 2,J + 3],BoundTex);
      P.TextureState := tsTransparent;
      MO.Vertices.AddObject('',T3DPoint.Create(B.X2,B.Y2,MinPos0.Z - BoundOffset));
      MO.Vertices.AddObject('',T3DPoint.Create(B.X2,B.Y2,MaxPos0.Z + BoundOffset));
      MO.Vertices.AddObject('',T3DPoint.Create(B.X1,B.Y1,MaxPos0.Z + BoundOffset));
      MO.Vertices.AddObject('',T3DPoint.Create(B.X1,B.Y1,MinPos0.Z - BoundOffset));
      MO.Polygons.AddObject('',P);
      Inc(J,4);
    End;

    // Cleanup

    SetLength(IntersectX,0);
    SetLength(IntersectY,0);
    SetLength(IntersectDist,0);
  End; // For I

  // Add a polygon below the zone

  P := TPolygon.Create([J,J + 1,J + 2,J + 3],BoundTex);
  P.TextureState := tsTransparent;
  MO.Vertices.AddObject('',T3DPoint.Create(MinPos0.X,MinPos0.Y,MinPos0.Z - BoundOffset));
  MO.Vertices.AddObject('',T3DPoint.Create(MinPos0.X,MaxPos0.Y,MinPos0.Z - BoundOffset));
  MO.Vertices.AddObject('',T3DPoint.Create(MaxPos0.X,MaxPos0.Y,MinPos0.Z - BoundOffset));
  MO.Vertices.AddObject('',T3DPoint.Create(MaxPos0.X,MinPos0.Y,MinPos0.Z - BoundOffset));
  MO.Polygons.AddObject('',P);
  Inc(J,4);

  // Add a polygon above the zone

  P := TPolygon.Create([J,J + 1,J + 2,J + 3],BoundTex);
  P.TextureState := tsTransparent;
  MO.Vertices.AddObject('',T3DPoint.Create(MinPos0.X,MinPos0.Y,MaxPos0.Z + BoundOffset));
  MO.Vertices.AddObject('',T3DPoint.Create(MaxPos0.X,MinPos0.Y,MaxPos0.Z + BoundOffset));
  MO.Vertices.AddObject('',T3DPoint.Create(MaxPos0.X,MaxPos0.Y,MaxPos0.Z + BoundOffset));
  MO.Vertices.AddObject('',T3DPoint.Create(MinPos0.X,MaxPos0.Y,MaxPos0.Z + BoundOffset));
  MO.Polygons.AddObject('',P);
  MO.CalcNormals;

  // Save the minimum and maximum Z position

  BoundMinZ := MinPos0.Z;
  BoundMaxZ := MaxPos0.Z;

  // Place the new object just after the ground meshes

  I := IndexOf(meshHeightMapGround);
  J := IndexOf(meshHeightMapUnderwater);
  If I < J Then I := J;
  InsertObject(I + 1,meshBoundingPolygons,MO);

  // Cleanup

  MinPos0.Free;
  MaxPos0.Free;
End; // TZone.BuildBoundingPolygons

Function TZone.FindMeshObject(MeshName: String): TMeshObject;
Var
  I: Integer;
Begin
  I := IndexOf(MeshName);
  If (I >= 0) Then
  Begin
    If (Objects[I] <> Nil) And (Objects[I] Is TMeshObject) Then
    Begin
      Result := TMeshObject(Objects[I]);
    End
    Else Result := Nil;
  End
  Else Result := Nil;
End; // TZone.FindMeshObject

Procedure TZone.BuildBounds;
Var
  I  : Integer;
  ZO : TZoneObject;
  MO : TMeshObject;
  P  : TPolygon;
  V1 : T3DPoint;
  V2 : T3DPoint;
  V3 : T3DPoint;
  N1 : T3DPoint;
  N2 : T3DPoint;
  S  : Single;

Begin
  For I := 0 To Bounds.Count - 1 Do Bounds.Objects[I].Free;
  Bounds.Clear;
  I := IndexOf('BoundingBox');
  If I >= 0 Then
  Begin
    ZO := TZoneObject(Objects[I]);
    If ZO Is TMeshObject Then
    Begin
      MO := TMeshObject(ZO);
      For I := 0 To MO.Polygons.Count - 1 Do
      Begin
        P := TPolygon(MO.Polygons.Objects[I]);
        If High(P.Vertices) = 0 Then
        Begin
          V1 := T3DPoint(MO.Vertices.Objects[P.Vertices[0]]);
          Bounds.AddObject('',TZoneBound.Create(V1.X,V1.Y,0,V1.X,V1.Y,0));
        End
        Else If High(P.Vertices) = 1 Then
        Begin
          V1 := T3DPoint(MO.Vertices.Objects[P.Vertices[0]]);
          V2 := T3DPoint(MO.Vertices.Objects[P.Vertices[1]]);
          If V1.Z = V2.Z Then Bounds.AddObject('',TZoneBound.Create(V1.X,V1.Y,0,V2.X,V2.Y,0));
        End
        Else If High(P.Vertices) >= 2 Then
        Begin
          V1 := T3DPoint(MO.Vertices.Objects[P.Vertices[0]]);
          V2 := T3DPoint(MO.Vertices.Objects[P.Vertices[1]]);
          V3 := T3DPoint(MO.Vertices.Objects[P.Vertices[2]]);
          N1 := T3DPoint.Create(V1);
          N1.Subtract(V2);
          N2 := T3DPoint.Create(V3);
          N2.Subtract(V2);
          N1.Cross(N2);
          If N1.Z = 0 Then
          Begin
            N2.Copy(V3);
            N2.Subtract(V1);
            S    := N2.X;
            N2.X := -N2.Y;
            N2.Y := S;
            N2.Z := 0;

            If N2.Dot(N1) > 0
             Then Bounds.AddObject('',TZoneBound.Create(V1.X,V1.Y,0,V3.X,V3.Y,0))
             Else Bounds.AddObject('',TZoneBound.Create(V3.X,V3.Y,0,V1.X,V1.Y,0));
          End;
          N1.Free;
          N2.Free;
        End
      End; // For I
    End;
  End;
End; // TZone.BuildBounds

Procedure TZone.BuildElevationGrid;
Var
  MO1,MO2 : TMeshObject;
  I,J     : Integer;
  X,Y     : Integer;
  V       : T3DPoint;
  MinX    : Single;
  MinY    : Single;
  MaxX    : Single;
  MaxY    : Single;
  SX,SY   : Integer;
  NSX,NSY : Integer;
  szNS    : Integer;
  szEW    : Integer;

Begin
  ElevationGrid.Clear;

  MO1 := FindMeshObject(meshHeightMapGround);
  MO2 := FindMeshObject(meshHeightMapUnderwater);

  If (MO1 <> Nil) Or (MO2 <> Nil) Then
  Begin
    // Get the zone extents

    MinX :=  9999999;
    MinY :=  9999999;
    MaxX := -9999999;
    MaxY := -9999999;

    J := 0; // Total vertex count
    If MO1 <> Nil Then
    Begin
      For I := 0 To MO1.Vertices.Count - 1 Do
      Begin
        V := T3DPoint(MO1.Vertices.Objects[I]);
        If V.X < MinX Then MinX := V.X;
        If V.Y < MinY Then MinY := V.Y;
        If V.X > MaxX Then MaxX := V.X;
        If V.Y > MaxY Then MaxY := V.Y;
        Inc(J);
      End; // For I
    End;
    If MO2 <> Nil Then
    Begin
      For I := 0 To MO2.Vertices.Count - 1 Do
      Begin
        V := T3DPoint(MO2.Vertices.Objects[I]);
        If V.X < MinX Then MinX := V.X;
        If V.Y < MinY Then MinY := V.Y;
        If V.X > MaxX Then MaxX := V.X;
        If V.Y > MaxY Then MaxY := V.Y;
        Inc(J);
      End; // For I
    End;

    // Don't proceed unless we have at least one vertex

    If J > 0 Then
    Begin
      SX   := GridSize;
      SY   := GridSize;
      szNS := Round(MaxX - MinX);
      szEW := Round(MaxY - MinY);

      // Round the size up to a texture boundary (this is vital)

      If (szNS Div SX) * SX < szNS Then szNS := ((szNS Div SX) + 1) * SX;
      If (szEW Div SY) * SY < szEW Then szEW := ((szEW Div SY) + 1) * SY;

      NSY := szNS Div SX;
      NSX := szEW Div SY;

      // Move the minimums to match the recalculated size

      MinX := MaxX - szNS;
      MinY := MaxY - szEW;

      // Set up the elevation grid

      ElevationGrid.MinX := MinX;
      ElevationGrid.MinY := MinY;
      ElevationGrid.MinZ := 9999999;
      ElevationGrid.MaxX := MaxX;
      ElevationGrid.MaxY := MaxY;
      ElevationGrid.MaxZ := -9999999;
      ElevationGrid.NX   := NSX + 1;
      ElevationGrid.NY   := NSY + 1;
      SetLength(ElevationGrid.Heights,(NSX + 1) * (NSY + 1));
      SetLength(ElevationGrid.Visible,NSX * NSY);
      For I := 0 To High(ElevationGrid.Visible) Do ElevationGrid.Visible[I] := True;

      If MO1 <> Nil Then
      Begin
        For I := 0 To MO1.Vertices.Count - 1 Do
        Begin
          V := T3DPoint(MO1.Vertices.Objects[I]);
          If (Frac((MaxX - V.X) / SX) = 0) And (Frac((MaxY - V.Y) / SY) = 0) Then
          Begin
            Y := Trunc((MaxX - V.X) / SX);
            X := Trunc((MaxY - V.Y) / SY);
            ElevationGrid.SetHeight(X,Y,V.Z);
            If V.Z < ElevationGrid.MinZ Then ElevationGrid.MinZ := V.Z;
            If V.Z > ElevationGrid.MaxZ Then ElevationGrid.MaxZ := V.Z;
          End;
        End; // For I
      End;
      If MO2 <> Nil Then
      Begin
        For I := 0 To MO2.Vertices.Count - 1 Do
        Begin
          V := T3DPoint(MO2.Vertices.Objects[I]);
          If (Frac((MaxX - V.X) / SX) = 0) And (Frac((MaxY - V.Y) / SY) = 0) Then
          Begin
            Y := Trunc((MaxX - V.X) / SX);
            X := Trunc((MaxY - V.Y) / SY);
            ElevationGrid.SetHeight(X,Y,V.Z);
            If V.Z < ElevationGrid.MinZ Then ElevationGrid.MinZ := V.Z;
            If V.Z > ElevationGrid.MaxZ Then ElevationGrid.MaxZ := V.Z;
          End;
        End; // For I
      End;
    End;
  End;
End; // TZone.BuildElevationGrid

Procedure TZone.FixElevationGridBounds;
Var
  MO1    : TMeshObject;
  MO2    : TMeshObject;
  MinPt1 : T3DPoint;
  MaxPt1 : T3DPoint;
  MinPt2 : T3DPoint;
  MaxPt2 : T3DPoint;

Begin
  MO1    := FindMeshObject(meshHeightMapGround);
  MO2    := FindMeshObject(meshHeightMapUnderwater);
  MinPt1 := T3DPoint.Create;
  MaxPt1 := T3DPoint.Create;
  MinPt2 := T3DPoint.Create;
  MaxPt2 := T3DPoint.Create;
  If MO1 <> Nil Then
  Begin
    MO1.GetBounds(MinPt1,MaxPt1);
    MinPt1.Add(MO1.Loc);
    MaxPt1.Add(MO1.Loc);
  End;
  If MO2 <> Nil Then
  Begin
    MO2.GetBounds(MinPt2,MaxPt2);
    MinPt2.Add(MO2.Loc);
    MaxPt2.Add(MO2.Loc);
    MinPt1.X := Min(MinPt1.X,MinPt2.X);
    MinPt1.Y := Min(MinPt1.Y,MinPt2.Y);
    MinPt1.Z := Min(MinPt1.Z,MinPt2.Z);
    MaxPt1.X := Max(MaxPt1.X,MaxPt2.X);
    MaxPt1.Y := Max(MaxPt1.Y,MaxPt2.Y);
    MaxPt1.Z := Max(MaxPt1.Z,MaxPt2.Z);
  End;
  ElevationGrid.MinX := MinPt1.X;
  ElevationGrid.MinY := MinPt1.Y;
  ElevationGrid.MinZ := MinPt1.Z;
  ElevationGrid.MaxX := MaxPt1.X;
  ElevationGrid.MaxY := MaxPt1.Y;
  ElevationGrid.MaxZ := MaxPt1.Z;
  MinPt1.Free;
  MaxPt1.Free;
  MinPt2.Free;
  MaxPt2.Free;
End; // TZone.FixElevationGridBounds

Procedure TZone.FindDefaultLandTexture;
Var
  MO    : TMeshObject;
  L     : TStringList;
  I,J,K : Integer;
  C     : Integer;
  P     : TPolygon;
  P1    : Pointer;
  St    : String;

Begin
  MO := FindMeshObject(meshHeightMapGround);
  If MO <> Nil Then
  Begin
    L        := TStringList.Create;
    L.Sorted := True;
    For I := 0 To MO.Polygons.Count - 1 Do
    Begin
      P  := TPolygon(MO.Polygons.Objects[I]);
      St := P.Texture;
      J  := L.IndexOf(St);
      If J >= 0 Then
      Begin
        P1 := L.Objects[J];
        Inc(LongInt(P1));
        L.Objects[J] := P1;
      End
      Else L.AddObject(St,Pointer(1));
    End; // For I
    J := -1;
    K := -1;
    For I := 0 To L.Count - 1 Do
    Begin
      C := LongInt(L.Objects[I]);
      If C > K Then
      Begin
        K := C;
        J := I;
      End;
    End; // For I
    If J >= 0 Then DefaultLandTexture := L.Strings[J];
    L.Free;
  End;
End; // TZone.FindDefaultLandTexture

Procedure TZone.FindDefaultUnderwaterTexture;
Var
  MO    : TMeshObject;
  L     : TStringList;
  I,J,K : Integer;
  C     : Integer;
  P     : TPolygon;
  P1    : Pointer;
  St    : String;

Begin
  MO := FindMeshObject(meshHeightMapUnderwater);
  If MO <> Nil Then
  Begin
    L        := TStringList.Create;
    L.Sorted := True;
    For I := 0 To MO.Polygons.Count - 1 Do
    Begin
      P  := TPolygon(MO.Polygons.Objects[I]);
      St := P.Texture;
      J  := L.IndexOf(St);
      If J >= 0 Then
      Begin
        P1 := L.Objects[J];
        Inc(LongInt(P1));
        L.Objects[J] := P1;
      End
      Else L.AddObject(St,Pointer(1));
    End; // For I
    J := -1;
    K := -1;
    For I := 0 To L.Count - 1 Do
    Begin
      C := LongInt(L.Objects[I]);
      If C > K Then
      Begin
        K := C;
        J := I;
      End;
    End; // For I
    If J >= 0 Then DefaultUnderwaterTexture := L.Strings[J];
    L.Free;
  End;
End; // TZone.FindDefaultUnderwaterTexture

Procedure TZone.LoadFromFile(FileName: String);
Var
  F              : System.Text;
  Valid          : Boolean;
  Line           : String;
  S              : String; // Mirrors Line, but always uppercase
  Command        : String; // Always uppercase
  Cmd            : String; // Mirrors Command, but preserves case
  I              : Integer;
  DecSep         : Char;

  Procedure ParseMesh;
  Var MO: TMeshObject;
  Begin
    MO    := TMeshObject.Create;
    MO.SetZone(Self); // Need to set this so the mesh can call back to here to import a 3DS file if it has to
    Valid := Valid And MO.LoadFromFile(F);
    If Valid Then
    Begin
      AddObject(MO.FName,MO);
      MO.SetZone(Self);

      // We must *not* call TMeshObject.Coalesce or vertex normals get screwed up
      // (sharing vertices is BAD, BAD, BAD)

//      MO.Coalesce;
    End;
  End; // ParseMesh

  Procedure ParseMeshLibraryReference;
  Var ML: TMeshLibraryObjectReference;
  Begin
    ML    := TMeshLibraryObjectReference.Create;
    Valid := Valid And ML.LoadFromFile(F);
    If Valid Then
    Begin
      AddObject(ML.FName,ML);
      ML.SetZone(Self);
    End;
  End; // ParseMeshLibraryReference

  Procedure ParseCreatureLibraryReference;
  Var CL: TCreatureLibraryObjectReference;
  Begin
    CL    := TCreatureLibraryObjectReference.Create;
    Valid := Valid And CL.LoadFromFile(F);
    If Valid Then
    Begin
      AddObject(CL.FName,CL);
      CL.SetZone(Self);
    End;
  End; // ParseCreatureLibraryReference

  Procedure ParseLight;
  Var L: TLightObject;
  Begin
    L     := TLightObject.Create;
    Valid := Valid And L.LoadFromFile(F);
    If Valid Then
    Begin
      AddObject(L.FName,L);
      L.SetZone(Self);
    End;
  End; // ParseLight

  Procedure ParseModelOrigin;
  Var MO: TModelOrigin;
  Begin
    MO    := TModelOrigin.Create;
    Valid := Valid And MO.LoadFromFile(F);
    If Valid Then
    Begin
      AddObject(MO.FName,MO);
      MO.SetZone(Self);
    End;
  End; // ParseModelOrigin

  Procedure ParseHotSpot;
  Var HS: THotSpot;
  Begin
    HS    := THotSpot.Create;
    Valid := Valid And HS.LoadFromFile(F);
    If Valid Then
    Begin
      AddObject(HS.FName,HS);
      HS.SetZone(Self);
    End;
  End; // ParseHotSpot

  Procedure ParseGroup;
  Var GO: TGroupObject;
  Begin
    GO := TGroupObject.Create;
    Valid := Valid And GO.LoadFromFile(F);
    If Valid Then
    Begin
      AddObject(GO.FName,GO);
      GO.SetZone(Self);
    End;
  End; // ParseGroup

  Procedure ParseScript;
  Var SO: TScriptedObject;
  Begin
    SO := TScriptedObject.Create;
    SO.FScriptName := GetToken(' ',Line);
    Valid := Valid And SO.LoadFromFile(F);
    If Valid Then
    Begin
      AddObject(SO.FName,SO);
      SO.SetZone(Self);
    End;
  End; // ParseScript

  Procedure ParseWater;
  Var
    Tokens     : TTokenArray;
    WaterLevel : Single;
    SemiTrans  : Boolean;
    Tinted     : Boolean;
    Color      : Integer;
    MinX       : Single;
    MinY       : Single;
    XSize      : Single;
    YSize      : Single;
    HasDepth   : Boolean;
    Depth      : Single;
    Elliptical : Boolean;
    AnimTime   : Single;
    I,J        : Integer;

    Function GetFirstTexture(Texture: String): String;
    Var Textures: TTokenArray;
    Begin
      GetTokens(';',Texture,Textures);
      If High(Textures) >= 0 Then Result := Textures[0] Else Result := '';
      SetLength(Textures,0);
    End; // GetFirstTexture

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) In [12..16] Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],WaterLevel) And
                         GetBooleanValue(Tokens[2],SemiTrans) And
                         GetBooleanValue(Tokens[3],Tinted)    And
                         GetIntValue(Tokens[4],Color)         And
                         GetSingleValue(Tokens[5],MinX)       And
                         GetSingleValue(Tokens[6],MinY)       And
                         GetSingleValue(Tokens[7],XSize)      And
                         GetSingleValue(Tokens[8],YSize);
      HasDepth   := False;
      Depth      := 0;
      Elliptical := False;
      AnimTime   := 0;
      If High(Tokens) >= 13 Then Valid := Valid And GetBooleanValue(Tokens[13],HasDepth);
      If High(Tokens) >= 14 Then Valid := Valid And GetSingleValue(Tokens[14],Depth);
      If High(Tokens) >= 15 Then Valid := Valid And GetBooleanValue(Tokens[15],Elliptical);
      If High(Tokens) >= 16 Then Valid := Valid And GetSingleValue(Tokens[16],AnimTime);
      Tokens[1] := UpperCase(Tokens[1]);
      If (Tokens[1] <> 'WATER') And (Tokens[1] <> 'LAVA') And (Tokens[1] <> 'PVP') And (Tokens[1] <> 'ICE') And (Tokens[1] <> 'ICEWATER') Then Valid := False;
      If Valid Then
      Begin
        SetLength(Water,High(Water) + 2);
        Water[High(Water)].Level := WaterLevel;
             If Tokens[1] = 'WATER'    Then Water[High(Water)].WType := wtWater
        Else If Tokens[1] = 'LAVA'     Then Water[High(Water)].WType := wtLava
        Else If Tokens[1] = 'PVP'      Then Water[High(Water)].WType := wtPvP
        Else If Tokens[1] = 'ICE'      Then Water[High(Water)].WType := wtIce
        Else If Tokens[1] = 'ICEWATER' Then Water[High(Water)].WType := wtIceWater;
        SetLength(Water[High(Water)].Tex,4);
        Water[High(Water)].SemiTrans  := SemiTrans;
        Water[High(Water)].Tinted     := Tinted;
        Water[High(Water)].Color      := TColor(Color);
        Water[High(Water)].MinX       := MinX;
        Water[High(Water)].MinY       := MinY;
        Water[High(Water)].XSize      := XSize;
        Water[High(Water)].YSize      := YSize;
        Water[High(Water)].Tex[0]     := GetFirstTexture(Tokens[9]);
        Water[High(Water)].Tex[1]     := GetFirstTexture(Tokens[10]);
        Water[High(Water)].Tex[2]     := GetFirstTexture(Tokens[11]);
        Water[High(Water)].Tex[3]     := GetFirstTexture(Tokens[12]);
        Water[High(Water)].HasDepth   := HasDepth;
        Water[High(Water)].Depth      := Depth;
        Water[High(Water)].AnimTime   := AnimTime;
        If Elliptical
         Then Water[High(Water)].Shape := wsElliptical
         Else Water[High(Water)].Shape := wsRectangular;
        SetLength(Water[High(Water)].Irregular,0);
        J := 0;
        For I := 0 To High(Water[High(Water)].Tex) Do If Water[High(Water)].Tex[I] <> '' Then Inc(J);
        If High(Tokens) >= 16
         Then Water[High(Water)].AnimTime := Max(Water[High(Water)].AnimTime,J * MinimumAnimTimePerFrame)
         Else Water[High(Water)].AnimTime := J * DefaultAnimTimePerFrame;
      End;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseWater

  Procedure ParseWater2;
  Var
    Tokens     : TTokenArray;
    TexTokens  : TTokenArray;
    WaterLevel : Single;
    SemiTrans  : Boolean;
    Tinted     : Boolean;
    Color      : Integer;
    MinX       : Single;
    MinY       : Single;
    XSize      : Single;
    YSize      : Single;
    HasDepth   : Boolean;
    Depth      : Single;
    Elliptical : Boolean;
    Irregular  : Integer;
    I,J        : Integer;

    Function GetFirstTexture(Texture: String): String;
    Var Textures: TTokenArray;
    Begin
      GetTokens(';',Texture,Textures);
      If High(Textures) >= 0 Then Result := Textures[0] Else Result := '';
      SetLength(Textures,0);
    End; // GetFirstTexture

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) >= 12 Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],WaterLevel) And
                         GetBooleanValue(Tokens[2],SemiTrans) And
                         GetBooleanValue(Tokens[3],Tinted)    And
                         GetIntValue(Tokens[4],Color)         And
                         GetSingleValue(Tokens[5],MinX)       And
                         GetSingleValue(Tokens[6],MinY)       And
                         GetSingleValue(Tokens[7],XSize)      And
                         GetSingleValue(Tokens[8],YSize);
      HasDepth   := False;
      Depth      := 0;
      Elliptical := False;
      Irregular  := 0;
      If High(Tokens) >= 10 Then Valid := Valid And GetBooleanValue(Tokens[10],HasDepth);
      If High(Tokens) >= 11 Then Valid := Valid And GetSingleValue(Tokens[11],Depth);
      If High(Tokens) >= 12 Then Valid := Valid And GetBooleanValue(Tokens[12],Elliptical);
      If High(Tokens) >= 13 Then Valid := Valid And GetIntValue(Tokens[13],Irregular);
      Tokens[1] := UpperCase(Tokens[1]);
      If (Tokens[1] <> 'WATER') And (Tokens[1] <> 'LAVA') And (Tokens[1] <> 'PVP') And (Tokens[1] <> 'ICE') And (Tokens[1] <> 'ICEWATER') Then Valid := False;
      If Valid Then
      Begin
        SetLength(Water,High(Water) + 2);
        Water[High(Water)].Level := WaterLevel;
             If Tokens[1] = 'WATER'    Then Water[High(Water)].WType := wtWater
        Else If Tokens[1] = 'LAVA'     Then Water[High(Water)].WType := wtLava
        Else If Tokens[1] = 'PVP'      Then Water[High(Water)].WType := wtPvP
        Else If Tokens[1] = 'ICE'      Then Water[High(Water)].WType := wtIce
        Else If Tokens[1] = 'ICEWATER' Then Water[High(Water)].WType := wtIceWater;
        GetTokens(';',Tokens[9],TexTokens);
        SetLength(Water[High(Water)].Tex,High(TexTokens) + 1);
        For I := 0 To High(TexTokens) Do Water[High(Water)].Tex[I] := TexTokens[I];
        Water[High(Water)].SemiTrans  := SemiTrans;
        Water[High(Water)].Tinted     := Tinted;
        Water[High(Water)].Color      := TColor(Color);
        Water[High(Water)].MinX       := MinX;
        Water[High(Water)].MinY       := MinY;
        Water[High(Water)].XSize      := XSize;
        Water[High(Water)].YSize      := YSize;
        Water[High(Water)].HasDepth   := HasDepth;
        Water[High(Water)].Depth      := Depth;
        Water[High(Water)].AnimTime   := 0;
             If Irregular > 0 Then Water[High(Water)].Shape := wsIrregular
        Else If Elliptical    Then Water[High(Water)].Shape := wsElliptical
        Else Water[High(Water)].Shape := wsRectangular;
        SetLength(Water[High(Water)].Irregular,Irregular);
        I := 0;
        While (I < Irregular) And Valid Do
        Begin
          If High(Tokens) >= 14 + I * 8
           Then Valid := Valid And GetSingleValue(Tokens[14 + I * 8],Water[High(Water)].Irregular[I].X1)
           Else Valid := False;
          If High(Tokens) >= 15 + I * 8
           Then Valid := Valid And GetSingleValue(Tokens[15 + I * 8],Water[High(Water)].Irregular[I].Y1)
           Else Valid := False;
          If High(Tokens) >= 16 + I * 8
           Then Valid := Valid And GetSingleValue(Tokens[16 + I * 8],Water[High(Water)].Irregular[I].X2)
           Else Valid := False;
          If High(Tokens) >= 17 + I * 8
           Then Valid := Valid And GetSingleValue(Tokens[17 + I * 8],Water[High(Water)].Irregular[I].Y2)
           Else Valid := False;
          If High(Tokens) >= 18 + I * 8
           Then Valid := Valid And GetSingleValue(Tokens[18 + I * 8],Water[High(Water)].Irregular[I].NX)
           Else Valid := False;
          If High(Tokens) >= 19 + I * 8
           Then Valid := Valid And GetSingleValue(Tokens[19 + I * 8],Water[High(Water)].Irregular[I].NY)
           Else Valid := False;
          If High(Tokens) >= 20 + I * 8
           Then Valid := Valid And GetSingleValue(Tokens[20 + I * 8],Water[High(Water)].Irregular[I].NZ)
           Else Valid := False;
          If High(Tokens) >= 21 + I * 8
           Then Valid := Valid And GetSingleValue(Tokens[21 + I * 8],Water[High(Water)].Irregular[I].Dist)
           Else Valid := False;
          Inc(I);
        End; // While
        J := 0;
        For I := 0 To High(Water[High(Water)].Tex) Do If Water[High(Water)].Tex[I] <> '' Then Inc(J);
        If High(Tokens) >= 14 + Irregular * 8 Then
        Begin
          Valid := Valid And GetSingleValue(Tokens[14 + Irregular * 8],Water[High(Water)].AnimTime);
          Water[High(Water)].AnimTime := Max(Water[High(Water)].AnimTime,J * MinimumAnimTimePerFrame);
        End
        Else Water[High(Water)].AnimTime := J * DefaultAnimTimePerFrame;
      End;
    End
    Else Valid := False;
    SetLength(Tokens,0);
    SetLength(TexTokens,0);
  End; // ParseWater2

  Procedure ParseTextureSet;
  Begin
    SetTextureSet(Line);
  End; // ParseTextureSet

  Procedure ParseElevationGrid;
  Var
    Tokens : TTokenArray;
    MinX   : Single;
    MinY   : Single;
    MinZ   : Single;
    MaxX   : Single;
    MaxY   : Single;
    MaxZ   : Single;
    NX,NY  : Integer;
    I      : Integer;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) = 7 Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],MinX) And
                         GetSingleValue(Tokens[1],MinY) And
                         GetSingleValue(Tokens[2],MinZ) And
                         GetSingleValue(Tokens[3],MaxX) And
                         GetSingleValue(Tokens[4],MaxY) And
                         GetSingleValue(Tokens[5],MaxZ) And
                         GetIntValue(Tokens[6],NX)      And
                         GetIntValue(Tokens[7],NY);
      If Valid Then
      Begin
        ElevationGrid.MinX := MinX;
        ElevationGrid.MinY := MinY;
        ElevationGrid.MinZ := MinZ;
        ElevationGrid.MaxX := MaxX;
        ElevationGrid.MaxY := MaxY;
        ElevationGrid.MaxZ := MaxZ;
        ElevationGrid.NX   := NX;
        ElevationGrid.NY   := NY;
        SetLength(ElevationGrid.Heights,NX * NY);
        SetLength(ElevationGrid.Visible,(NX - 1) * (NY - 1));
        For I := 0 To High(ElevationGrid.Visible) Do ElevationGrid.Visible[I] := True;
      End;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseElevationGrid

  Procedure ParseElevationGridHeights;
  Var
    Tokens : TTokenArray;
    I      : Integer;
    Start  : Integer;
    Finish : Integer;
    H      : Single;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) > 1 Then
    Begin
      Valid := Valid And GetIntValue(Tokens[0],Start) And
                         GetIntValue(Tokens[1],Finish);
      Valid := Valid And (Start >= 0) And
                         (Finish >= Start) And
                         (Finish < ElevationGrid.NX * ElevationGrid.NY) And
                         ((Finish - Start) = (High(Tokens) - 2));
      If Valid Then
      Begin
        I := 2;
        While (I <= High(Tokens)) And Valid Do
        Begin
          Valid := Valid And GetSingleValue(Tokens[I],H);
          If Valid Then ElevationGrid.Heights[I - 2 + Start] := H;
          Inc(I);
        End; // While
      End;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseElevationGridHeights

  Procedure ParseElevationGridVisible;
  Var
    Tokens : TTokenArray;
    I      : Integer;
    Start  : Integer;
    Finish : Integer;
    B      : Boolean;

  Begin
    GetTokens(',',S,Tokens);
    If High(Tokens) > 1 Then
    Begin
      Valid := Valid And GetIntValue(Tokens[0],Start) And
                         GetIntValue(Tokens[1],Finish);
      Valid := Valid And (Start >= 0) And
                         (Finish >= Start) And
                         (Finish < (ElevationGrid.NX - 1) * (ElevationGrid.NY - 1)) And
                         ((Finish - Start) = (High(Tokens) - 2));
      If Valid Then
      Begin
        I := 2;
        While (I <= High(Tokens)) And Valid Do
        Begin
          Valid := Valid And GetBooleanValue(Tokens[I],B);
          If Valid Then ElevationGrid.Visible[I - 2 + Start] := B;
          Inc(I);
        End; // While
      End;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseElevationGridVisible

  Procedure ParseLandTex;
  Begin
    DefaultLandTexture := Line;
  End; // ParseLandTex

  Procedure ParseUnderwaterTex;
  Begin
    DefaultUnderwaterTexture := Line;
  End; // ParseUnderwaterTex

  Procedure ParseBound;
  Var
    Tokens : TTokenArray;
    X1,Y1  : Single;
    X2,Y2  : Single;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) > 2 Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],X1) And
                         GetSingleValue(Tokens[1],Y1) And
                         GetSingleValue(Tokens[2],X2) And
                         GetSingleValue(Tokens[3],Y2);
      If Valid Then Bounds.AddObject('',TZoneBound.Create(X1,Y1,0,X2,Y2,0));
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseBound

  Procedure ParseBound2;
  Var
    Tokens   : TTokenArray;
    X1,Y1,Z1 : Single;
    X2,Y2,Z2 : Single;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) > 2 Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],X1) And
                         GetSingleValue(Tokens[1],Y1) And
                         GetSingleValue(Tokens[2],Z1) And
                         GetSingleValue(Tokens[3],X2) And
                         GetSingleValue(Tokens[4],Y2) And
                         GetSingleValue(Tokens[5],Z2);
      If Valid Then Bounds.AddObject('',TZoneBound.Create(X1,Y1,Z1,X2,Y2,Z2));
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseBound2

  Procedure ParseZonePlane;
  Var
    Tokens       : TTokenArray;
    X1,Y1,Z1     : Single;
    X2,Y2,Z2     : Single;
    InfiniteZ    : Boolean;
    DestZoneID   : Integer;
    DestX        : Single;
    DestY        : Single;
    DestZ        : Single;
    DestAngle    : Integer;
    HasDestX     : Boolean;
    HasDestY     : Boolean;
    HasDestZ     : Boolean;
    HasDestAngle : Boolean;
    ZP           : TZonePlane;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) >= 15 Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],X1) And
                         GetSingleValue(Tokens[1],Y1) And
                         GetSingleValue(Tokens[2],Z1) And
                         GetSingleValue(Tokens[3],X2) And
                         GetSingleValue(Tokens[4],Y2) And
                         GetSingleValue(Tokens[5],Z2) And
                         GetBooleanValue(Tokens[6],InfiniteZ) And
                         GetIntValue(Tokens[7],DestZoneID) And
                         GetSingleValue(Tokens[8],DestX) And
                         GetSingleValue(Tokens[9],DestY) And
                         GetSingleValue(Tokens[10],DestZ) And
                         GetIntValue(Tokens[11],DestAngle) And
                         GetBooleanValue(Tokens[12],HasDestX) And
                         GetBooleanValue(Tokens[13],HasDestY) And
                         GetBooleanValue(Tokens[14],HasDestZ) And
                         GetBooleanValue(Tokens[15],HasDestAngle);
      If Valid Then
      Begin
        ZP := TZonePlane.Create;
        ZonePlanes.AddObject('',ZP);
        ZP.X1           := X1;
        ZP.Y1           := Y1;
        ZP.Z1           := Z1;
        ZP.X2           := X2;
        ZP.Y2           := Y2;
        ZP.Z2           := Z2;
        ZP.InfiniteZ    := InfiniteZ;
        ZP.DestZoneID   := DestZoneID;
        ZP.DestX        := DestX;
        ZP.DestY        := DestY;
        ZP.DestZ        := DestZ;
        ZP.DestAngle    := DestAngle;
        ZP.HasDestX     := HasDestX;
        ZP.HasDestY     := HasDestY;
        ZP.HasDestZ     := HasDestZ;
        ZP.HasDestAngle := HasDestAngle;
      End;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseZonePlane

  Procedure ParseZoneType;
  Begin
         If S = 'INDOOR'  Then ZoneType := ztIndoor
    Else If S = 'OUTDOOR' Then ZoneType := ztOutdoor
    Else Valid := False;
  End; // ParseZoneType

  Procedure ParseSound;
  Var
    Tokens    : TTokenArray;
    Tokens1   : TTokenArray;
    DayName   : String;
    NightName : String;
    Area      : Boolean;
    X         : Single;
    Y         : Single;
    Z         : Single;
    Radius    : Single;
    Sound     : TSound;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) >= 6 Then
    Begin
      ConvertEuroNumbersToUS(Line);
      GetTokens(',',Line,Tokens1);
      DayName   := Tokens1[0];
      NightName := Tokens1[1];
      Valid     := Valid And GetBooleanValue(Tokens[2],Area) And
                             GetSingleValue(Tokens[3],X) And
                             GetSingleValue(Tokens[4],Y) And
                             GetSingleValue(Tokens[5],Z) And
                             GetSingleValue(Tokens[6],Radius);
      If Valid Then
      Begin
        Sound           := TSound.Create;
        Sounds.AddObject('',Sound);
        Sound.DayName   := DayName;
        Sound.NightName := NightName;
        Sound.Area      := Area;
        Sound.X         := X;
        Sound.Y         := Y;
        Sound.Z         := Z;
        Sound.Radius    := Radius;
      End;
    End
    Else Valid := False;
    SetLength(Tokens,0);
    SetLength(Tokens1,0);
  End; // ParseSound

  Procedure ParseExtraMesh;
  Var Tokens: TTokenArray;
  Begin
    GetTokens(',',Line,Tokens);
    If High(Tokens) >= 0 Then
    Begin
      If ExtraMeshes.IndexOf(Tokens[0]) < 0 Then ExtraMeshes.Add(Tokens[0]);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseExtraMesh

  Procedure ParseCreature;
  Var Tokens: TTokenArray;
  Begin
    GetTokens(',',Line,Tokens);
    If High(Tokens) >= 0 Then
    Begin
      If Creatures.IndexOf(Tokens[0] + ', ' + Tokens[1]) < 0 Then Creatures.Add(Tokens[0] + ', ' + Tokens[1]);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseCreature

Begin
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  If FileExists(FileName) Then
  Begin
    ElevationGrid.Clear;
    AssignFile(F,FileName);
    Reset(F);
    Valid := True;
    For I := 0 To High(Water) Do SetLength(Water[I].Irregular,0);
    SetLength(Water,0);
    While Valid And Not Eof(F) Do
    Begin
      ReadLn(F,Line);
      Line := Trim(Line);
      S    := UpperCase(Line);
      If S <> '' Then
      Begin
        Command := GetToken(' ',S);
        Cmd     := GetToken(' ',Line);
             If Command = 'MESH'                     Then ParseMesh
        Else If Command = 'MESHLIBRARYREFERENCE'     Then ParseMeshLibraryReference
        Else If Command = 'CREATURELIBRARYREFERENCE' Then ParseCreatureLibraryReference
        Else If Command = 'LIGHT'                    Then ParseLight
        Else If Command = 'MODELORIGIN'              Then ParseModelOrigin
        Else If Command = 'HOTSPOT'                  Then ParseHotSpot
        Else If Command = 'GROUP'                    Then ParseGroup
        Else If Command = 'SCRIPT'                   Then ParseScript
        Else If Command = 'WATER'                    Then ParseWater
        Else If Command = 'WATER2'                   Then ParseWater2
        Else If Command = 'TEXTURESET'               Then ParseTextureSet
        Else If Command = 'ELEVATIONGRID'            Then ParseElevationGrid
        Else If Command = 'ELEVATIONGRIDHEIGHTS'     Then ParseElevationGridHeights
        Else If Command = 'ELEVATIONGRIDVISIBLE'     Then ParseElevationGridVisible
        Else If Command = 'LANDTEX'                  Then ParseLandTex
        Else If Command = 'UNDERWATERTEX'            Then ParseUnderwaterTex
        Else If Command = 'BOUND'                    Then ParseBound
        Else If Command = 'BOUND2'                   Then ParseBound2
        Else If Command = 'ZONEPLANE'                Then ParseZonePlane
        Else If Command = 'LONGNAME'                 Then LongName := Line
        Else If Command = 'SHORTNAME'                Then ShortName := Line
        Else If Command = 'ZONETYPE'                 Then ParseZoneType
        Else If Command = 'SOUND'                    Then ParseSound
        Else If Command = 'EXTRAMESH'                Then ParseExtraMesh
        Else If Command = 'CREATURE'                 Then ParseCreature
        Else Valid := False;
      End;
    End; // While
    CloseFile(F);
    If (ElevationGrid.NX = 0) And (ElevationGrid.NY = 0) Then BuildElevationGrid;
    FixElevationGridBounds;
    If Bounds.Count             = 0  Then BuildBounds;
    If DefaultLandTexture       = '' Then FindDefaultLandTexture;
    If DefaultUnderwaterTexture = '' Then FindDefaultUnderwaterTexture;
    If Bounds.Count             > 0  Then BuildBoundingPolygons;
    For I := 0 To Import3DSCache.Count - 1 Do Import3DSCache.Objects[I].Free;
    Import3DSCache.Clear;
  End;
  DecimalSeparator := DecSep;
End; // TZone.LoadFromFile

Procedure TZone.SaveToFile(FileName: String; SelectedObjects: TStringList);
Var
  I      : Integer;
  F      : System.Text;
  Indent : Integer;
  DecSep : Char;

  Procedure WriteWaterSetting;
  Var
    I,J : Integer;
    St  : String;
    St1 : String;

  Begin
    For I := 0 To High(Water) Do
    Begin
      Write(F,'Water2 ' + FloatToStr(Water[I].Level) + ', ');
      Case Water[I].WType Of
        wtWater: Write(F,'water, ');
         wtLava: Write(F,'lava, ');
          wtPvP: Write(F,'pvp, ');
          wtIce: Write(F,'ice, ');
     wtIceWater: Write(F,'icewater, ');
      End; // Case
      St := '';
      For J := 0 To High(Water[I].Tex) Do
      Begin
        If St <> '' Then St := St + ';';
        St := St + Water[I].Tex[J];
      End; // For J
      St1 := BoolToStr(Water[I].SemiTrans,True) + ', ' +
             BoolToStr(Water[I].Tinted,True) + ', $' +
             IntToHex(Water[I].Color,8) + ', ' +
             FloatToStr(Water[I].MinX) + ', ' +
             FloatToStr(Water[I].MinY) + ', ' +
             FloatToStr(Water[I].XSize) + ', ' +
             FloatToStr(Water[I].YSize) + ', ' +
             St + ', ' +
             BoolToStr(Water[I].HasDepth,True) + ', ' +
             FloatToStr(Water[I].Depth) + ', ' +
             BoolToStr(Water[I].Shape = wsElliptical,True);
      If Water[I].Shape = wsIrregular Then
      Begin
        St1 := St1 + ', ' + IntToStr(High(Water[I].Irregular) + 1);
        If High(Water[I].Irregular) >= 0 Then St1 := St1 + ', ';
        For J := 0 To High(Water[I].Irregular) Do
        Begin
          St1 := St1 + FloatToStr(Water[I].Irregular[J].X1) + ', ' +
                       FloatToStr(Water[I].Irregular[J].Y1) + ', ' +
                       FloatToStr(Water[I].Irregular[J].X2) + ', ' +
                       FloatToStr(Water[I].Irregular[J].Y2) + ', ' +
                       FloatToStr(Water[I].Irregular[J].NX) + ', ' +
                       FloatToStr(Water[I].Irregular[J].NY) + ', ' +
                       FloatToStr(Water[I].Irregular[J].NZ) + ', ' +
                       FloatToStr(Water[I].Irregular[J].Dist);
          If J < High(Water[I].Irregular) Then St1 := St1 + ', '; 
        End; // For J
        St1 := St1 + ', ' + FloatToStr(Water[I].AnimTime);
      End;
      WriteLn(F,St1);
    End; // For I
  End; // WriteWaterSetting

  Procedure WriteElevationGrid;
  Var
    I,J     : Integer;
    Heights : Integer;
    St      : String;

  Begin
    If (ElevationGrid.NX > 0) And (ElevationGrid.NY > 0) Then
    Begin
      WriteLn(F,'ElevationGrid ' + FloatToStr(ElevationGrid.MinX) + ', ' +
                                   FloatToStr(ElevationGrid.MinY) + ', ' +
                                   FloatToStr(ElevationGrid.MinZ) + ', ' +
                                   FloatToStr(ElevationGrid.MaxX) + ', ' +
                                   FloatToStr(ElevationGrid.MaxY) + ', ' +
                                   FloatToStr(ElevationGrid.MaxZ) + ', ' +
                                   IntToStr(ElevationGrid.NX)     + ', ' +
                                   IntToStr(ElevationGrid.NY));

      // Write elevation data

      Heights := ElevationGrid.NX * ElevationGrid.NY;
      For I := 1 To Heights Div 10 Do
      Begin
        St := '';
        For J := 0 To 9 Do
        Begin
          If St <> '' Then St := St + ', ';
          St := St + FloatToStr(ElevationGrid.Heights[(I - 1) * 10 + J]);
        End; // For J
        WriteLn(F,'ElevationGridHeights ' + IntToStr((I - 1) * 10) + ', ' + IntToStr((I - 1) * 10 + 9) + ', ' + St);
      End; // For I
      St := '';
      J  := Heights Div 10;
      For I := 1 To Heights Mod 10 Do
      Begin
        If St <> '' Then St := St + ', ';
        St := St + FloatToStr(ElevationGrid.Heights[J * 10 + I - 1]);
      End; // For I
      If St <> '' Then WriteLn(F,'ElevationGridHeights ' + IntToStr(J * 10) + ', ' + IntToStr(J * 10 + (Heights Mod 10) - 1) + ', ' + St);

      // Write patch visibility

      Heights := (ElevationGrid.NX - 1) * (ElevationGrid.NY - 1);
      For I := 1 To Heights Div 10 Do
      Begin
        St := '';
        For J := 0 To 9 Do
        Begin
          If St <> '' Then St := St + ', ';
          St := St + BoolToStr(ElevationGrid.Visible[(I - 1) * 10 + J],True);
        End; // For J
        WriteLn(F,'ElevationGridVisible ' + IntToStr((I - 1) * 10) + ', ' + IntToStr((I - 1) * 10 + 9) + ', ' + St);
      End; // For I
      St := '';
      J  := Heights Div 10;
      For I := 1 To Heights Mod 10 Do
      Begin
        If St <> '' Then St := St + ', ';
        St := St + BoolToStr(ElevationGrid.Visible[J * 10 + I - 1],True);
      End; // For I
      If St <> '' Then WriteLn(F,'ElevationGridVisible ' + IntToStr(J * 10) + ', ' + IntToStr(J * 10 + (Heights Mod 10) - 1) + ', ' + St);
    End;
  End; // WriteElevationGrid

  Procedure WriteBounds;
  Var
    I  : Integer;
    ZB : TZoneBound;

  Begin
    For I := 0 To Bounds.Count - 1 Do
    Begin
      ZB := TZoneBound(Bounds.Objects[I]);
      WriteLn(F,'Bound2 ' + FloatToStr(ZB.X1) + ', ' +
                            FloatToStr(ZB.Y1) + ', ' +
                            FloatToStr(ZB.Z1) + ', ' +
                            FloatToStr(ZB.X2) + ', ' +
                            FloatToStr(ZB.Y2) + ', ' +
                            FloatToStr(ZB.Z2));
    End; // For I
  End; // WriteBounds

  Procedure WriteZonePlanes;
  Var
    I  : Integer;
    ZP : TZonePlane;

  Begin
    For I := 0 To ZonePlanes.Count - 1 Do
    Begin
      ZP := TZonePlane(ZonePlanes.Objects[I]);
      WriteLn(F,'ZonePlane ' + FloatToStr(ZP.X1) + ', ' +
                               FloatToStr(ZP.Y1) + ', ' +
                               FloatToStr(ZP.Z1) + ', ' +
                               FloatToStr(ZP.X2) + ', ' +
                               FloatToStr(ZP.Y2) + ', ' +
                               FloatToStr(ZP.Z2) + ', ' +
                               BoolToStr(ZP.InfiniteZ,True) + ', ' +
                               IntToStr(ZP.DestZoneID) + ', ' +
                               FloatToStr(ZP.DestX) + ', ' +
                               FloatToStr(ZP.DestY) + ', ' +
                               FloatToStr(ZP.DestZ) + ', ' +
                               IntToStr(ZP.DestAngle) + ', ' +
                               BoolToStr(ZP.HasDestX,True) + ', ' +
                               BoolToStr(ZP.HasDestY,True) + ', ' +
                               BoolToStr(ZP.HasDestZ,True) + ', ' +
                               BoolToStr(ZP.HasDestAngle,True));
    End; // For I
  End; // WriteZonePlanes

  Procedure WriteSounds;
  Var
    I     : Integer;
    Sound : TSound;

  Begin
    For I := 0 To Sounds.Count - 1 Do
    Begin
      Sound := TSound(Sounds.Objects[I]);
      WriteLn(F,'Sound ' + Sound.DayName + ', ' +
                           Sound.NightName + ', ' +
                           BoolToStr(Sound.Area,True) + ', ' +
                           FloatToStr(Sound.X) + ', ' +
                           FloatToStr(Sound.Y) + ', ' +
                           FloatToStr(Sound.Z) + ', ' +
                           FloatToStr(Sound.Radius));
    End; // For I
  End; // WriteSounds

  Procedure WriteExtraMeshes;
  Var I: Integer;
  Begin
    For I := 0 To ExtraMeshes.Count - 1 Do WriteLn(F,'ExtraMesh ' + ExtraMeshes.Strings[I]);
  End; // WriteExtraMeshes

  Procedure WriteCreatures;
  Var I: Integer;
  Begin
    For I := 0 To Creatures.Count - 1 Do WriteLn(F,'Creature ' + Creatures.Strings[I]);
  End; // WriteCreatures

Begin
  Indent := 0;
  AssignFile(F,FileName);
  ReWrite(F);
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';

  // Save the texture set

  If TextureSet <> '' Then WriteLn(F,'TextureSet ' + TextureSet);
  If LongName   <> '' Then WriteLn(F,'LongName '   + LongName);
  If ShortName  <> '' Then WriteLn(F,'ShortName '  + ShortName);
  If ZoneType = ztOutDoor
   Then WriteLn(F,'ZoneType Outdoor')
   Else WriteLn(F,'ZoneType Indoor');

  If SelectedObjects = Nil Then
  Begin
    // Save the water settings

    WriteWaterSetting;
    WriteLn(F); // Spacer

    // Save the creatures

    WriteCreatures;
    WriteLn(F); // Spacer

    // Save the elevation grid

    WriteElevationGrid;
    WriteLn(F); // Spacer

    // Write the zone bounds

    WriteBounds;
    If Bounds.Count > 0 Then WriteLn(F); // Spacer

    // Write the zone planes

    WriteZonePlanes;
    If ZonePlanes.Count > 0 Then WriteLn(F); // Spacer

    // Write the sounds

    WriteSounds;
    If Sounds.Count > 0 Then WriteLn(F); // Spacer

    // Write the extra meshes (e.g. door meshes)

    WriteExtraMeshes;
    If ExtraMeshes.Count > 0 Then WriteLn(F); // Spacer

    // Now save the objects

    For I := 0 To Count - 1 Do TZoneObject(Objects[I]).SaveToFile(F,Indent);
  End
  Else
  Begin
    For I := 0 To SelectedObjects.Count - 1 Do TZoneObject(SelectedObjects.Objects[I]).SaveToFile(F,Indent);
  End;
  CloseFile(F);
  DecimalSeparator := DecSep;
End; // TZone.SaveToFile

// ------------------------------
// TZoneObject
// ------------------------------

Constructor TZoneObject.Create;
Begin
  FName    := '';
  FParent  := Nil;
  FZone    := Nil;
  FLoc     := T3DPoint.Create(0,0,0);
  FHeading := THeading.Create(0,0,0);
  FSize    := T3DPoint.Create(1,1,1);
End; // TZoneObject.Create

Constructor TZoneObject.Create(ZO: TZoneObject);
Begin
  FName    := ZO.FName;
  FLoc     := T3DPoint.Create(ZO.FLoc);
  FHeading := THeading.Create(ZO.FHeading);
  FSize    := T3DPoint.Create(ZO.FSize);
  FParent  := ZO.FParent;
  FZone    := ZO.FZone;
End; // TZoneObject.Create

Constructor TZoneObject.Create(AName: String);
Begin
  FName    := AName;
  FParent  := Nil;
  FZone    := Nil;
  FLoc     := T3DPoint.Create(0,0,0);
  FHeading := THeading.Create(0,0,0);
  FSize    := T3DPoint.Create(1,1,1);
End; // TZoneObject.Create

Constructor TZoneObject.Create(AName: String; AX,AY,AZ: Single);
Begin
  FName    := AName;
  FParent  := Nil;
  FZone    := Nil;
  FLoc     := T3DPoint.Create(AX,AY,AZ);
  FHeading := THeading.Create(0,0,0);
  FSize    := T3DPoint.Create(1,1,1);
End; // TZoneObject.Create

Constructor TZoneObject.Create(AName: String; AX,AY,AZ,SX,SY,SZ: Single);
Begin
  FName    := AName;
  FParent  := Nil;
  FZone    := Nil;
  FLoc     := T3DPoint.Create(AX,AY,AZ);
  FHeading := THeading.Create(0,0,0);
  FSize    := T3DPoint.Create(SX,SY,SZ);
End; // TZoneObject.Create

Destructor TZoneObject.Destroy;
Begin
  FLoc.Free;
  FHeading.Free;
  FSize.Free;
End; // TZoneObject.Destroy

Function TZoneObject.IsSpecialObject: Boolean;
Begin
  Result := (FName = meshHeightMapGround) Or
            (FName = meshHeightMapUnderwater) Or
            (FName = meshBoundingBox) Or
            (FName = meshBoundingPolygons);
End; // TZoneObject.IsSpecialObject

Procedure TZoneObject.GetSize(Out MinPos,MaxPos: T3DPoint; Var FirstTex: String);
Var
  I,J : Integer;
  V   : T3DPoint;
  L   : TStringList;
  MO  : TMeshObject;

Begin
  MinPos := Nil;
  MaxPos := Nil;
  L      := TStringList.Create;
  AddToPolygonList(L,False,False,True,True);
  For I := 0 To L.Count - 1 Do
  Begin
    MO := TMeshObject(L.Objects[I]);
    For J := 0 To MO.Vertices.Count - 1 Do
    Begin
      V := T3DPoint(MO.Vertices.Objects[J]);
      If MinPos = Nil Then
      Begin
        MinPos := T3DPoint.Create(V);
        MaxPos := T3DPoint.Create(V);
      End
      Else
      Begin
        If V.X < MinPos.X Then MinPos.X := V.X;
        If V.Y < MinPos.Y Then MinPos.Y := V.Y;
        If V.Z < MinPos.Z Then MinPos.Z := V.Z;
        If V.X > MaxPos.X Then MaxPos.X := V.X;
        If V.Y > MaxPos.Y Then MaxPos.Y := V.Y;
        If V.Z > MaxPos.Z Then MaxPos.Z := V.Z;
      End;
    End; // For J
    J := 0;
    While (FirstTex = '') And (J < MO.Polygons.Count) Do
    Begin
      FirstTex := TPolygon(MO.Polygons.Objects[J]).Texture;
      Inc(J);
    End; // While
    MO.Free;
  End; // For I
  If MinPos = Nil Then
  Begin
    MinPos := T3DPoint.Create(FLoc);
    MaxPos := T3DPoint.Create(FLoc);
  End;
  L.Free;
End; // TZoneObject.GetSize

Function TZoneObject.NameExists(St: String; ToUpper: Boolean): Boolean;
Begin
  If ToUpper
   Then Result := (UpperCase(St) = UpperCase(FName))
   Else Result := (St = UpperCase(FName));
End; // TZoneObject.NameExists

Function TZoneObject.FindObjectByName(St: String): TZoneObject;
Begin
  If UpperCase(St) = UpperCase(FName)
   Then Result := Self
   Else Result := Nil;
End; // TZoneObject.FindObjectByName

Procedure TZoneObject.ChangeToAbsolute(Parent: TGroupObject);
// Changes this object's statistics so they are absolute instead of being relative to a group object.
// This doesn't actually remove the object from a group, and the group doesn't in fact even have to
// be this object's parent (this method is primarily for making a clipboard copy use absolute
// coordinates).
Var P: T3DPoint;
Begin
  P := T3DPoint.Create;
  While Parent <> Nil Do
  Begin
    // Find out where to place this object relative to the group, accounting for overall group size and rotation

    P.X := Parent.FSize.X * FLoc.X;
    P.Y := Parent.FSize.Y * FLoc.Y;
    P.Z := Parent.FSize.Z * FLoc.Z;
    Parent.FHeading.Rotate(P);

    // Set the object's position

    FLoc.X := Parent.FLoc.X + P.X;
    FLoc.Y := Parent.FLoc.Y + P.Y;
    FLoc.Z := Parent.FLoc.Z + P.Z;

    // Set the object's rotational angles

{
    FHeading.FXAngle := FHeading.FXAngle + Parent.FHeading.FXAngle;
    FHeading.FYAngle := FHeading.FYAngle + Parent.FHeading.FYAngle;
    FHeading.FZAngle := FHeading.FZAngle + Parent.FHeading.FZAngle;
}
    FHeading.Multiply(Parent.FHeading);
    FHeading.Standardize;

    // Set the object's size

    FSize.X := FSize.X * Parent.FSize.X;
    FSize.Y := FSize.Y * Parent.FSize.Y;
    FSize.Z := FSize.Z * Parent.FSize.Z;

    Parent := Parent.FParent;
  End; // While
  P.Free;
End; // TZoneObject.ChangeToAbsolute

Function TZoneObject.GetAbsoluteLocation: T3DPoint;
// Changes this object's statistics so they are absolute instead of being relative to a group object.
// This doesn't actually remove the object from a group, and the group doesn't in fact even have to
// be this object's parent (this method is primarily for making a clipboard copy use absolute
// coordinates).
Var
  P   : T3DPoint;
  Par : TGroupObject;

Begin
  P   := T3DPoint.Create(FLoc);
  Par := FParent;
  While Par <> Nil Do
  Begin
    // Find out where to place this object relative to the group, accounting for overall group size and rotation

    P.X := Par.FSize.X * P.X;
    P.Y := Par.FSize.Y * P.Y;
    P.Z := Par.FSize.Z * P.Z;
    Par.FHeading.Rotate(P);

    // Set the object's position

    P.Add(Par.FLoc);

    // Move up the chain

    Par := Par.FParent;
  End; // While
  Result := P;
End; // TZoneObject.GetAbsoluteLocation

Function TZoneObject.GetAbsoluteLocation(P: T3DPoint): T3DPoint;
// Given a point P that is relative to this object, returns that point's absolute location
Var
//  Par: TZoneObject;
  P1 : T3DPoint;

Begin
  P1  := T3DPoint.Create(P);
  MakeAbsolute(P1);
{
  Par := Self;
  While Par <> Nil Do
  Begin
    // Find out where to place this object relative to the group, accounting for overall group size and rotation

    P1.X := Par.FSize.X * P1.X;
    P1.Y := Par.FSize.Y * P1.Y;
    P1.Z := Par.FSize.Z * P1.Z;
    Par.FHeading.Rotate(P);

    // Set the object's position

    P1.Add(Par.FLoc);

    // Move up the chain

    Par := Par.FParent;
  End; // While
}
  Result := P1;
End; // TZoneObject.GetAbsoluteLocation

Procedure TZoneObject.MakeAbsolute(P: T3DPoint);
// Given a point P that is relative to this object, changes it to an absolute location
Var Par: TZoneObject;
Begin
  Par := Self;
  While Par <> Nil Do
  Begin
    // Find out where to place this object relative to the group, accounting for overall group size and rotation

    P.X := Par.FSize.X * P.X;
    P.Y := Par.FSize.Y * P.Y;
    P.Z := Par.FSize.Z * P.Z;
    Par.FHeading.Rotate(P);

    // Set the object's position

    P.Add(Par.FLoc);

    // Move up the chain

    Par := Par.FParent;
  End; // While
End; // TZoneObject.MakeAbsolute

Function TZoneObject.GetRelativeLocation(P: T3DPoint): T3DPoint;
Var P1: T3DPoint;

  Procedure Process(ZO: TZoneObject; P: T3DPoint);
  Begin
    If ZO.FParent <> Nil Then Process(ZO.FParent,P);
    P.Subtract(ZO.FLoc);
    If ZO.Size.X <> 0 Then P.X := P.X / ZO.Size.X;
    If ZO.Size.Y <> 0 Then P.Y := P.Y / ZO.Size.Y;
    If ZO.Size.Z <> 0 Then P.Z := P.Z / ZO.Size.Z;
    ZO.FHeading.NegativeRotate(P);
  End; // Process

Begin
  P1 := T3DPoint.Create(P);
  Process(Self,P1);
  Result := P1;
End; // TZoneObject.GetRelativeLocation

Procedure TZoneObject.TranslateAbsolute(P: T3DPoint);

  Procedure DoParent(ZO: TZoneObject);
  Begin
    If ZO.FParent <> Nil Then DoParent(ZO.FParent);
    P.Subtract(FLoc);
    If FSize.X <> 0 Then P.X := P.X / FSize.X Else P.X := 0;
    If FSize.Y <> 0 Then P.Y := P.Y / FSize.Y Else P.Y := 0;
    If FSize.Z <> 0 Then P.Z := P.Z / FSize.Z Else P.Z := 0;
    FHeading.NegativeRotate(P);
  End; // DoParent

Begin
  If FParent <> Nil Then DoParent(FParent);
  Loc.Add(P);
End; // TZoneObject.TranslateAbsolute

Function TZoneObject.LoadFromFile(Var F: System.Text): Boolean;
Var
  Valid      : Boolean;
  Line       : String;
  S          : String; // Mirrors Line, but always uppercase
  Command    : String; // Always uppercase
  Cmd        : String; // Mirrors Command, but preserves case
  HasName    : Boolean;
  HasLoc     : Boolean;
  HasHeading : Boolean;
  HasSize    : Boolean;
  DecSep     : Char;

  Procedure ParseName;
  Begin
    FName   := Line;
    HasName := True;
  End; // ParseName

  Procedure ParseLoc;
  Var Tokens: TTokenArray;
  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) = 2 Then
    Begin
      Valid := Valid And FLoc.ParseTokens(Tokens[0],Tokens[1],Tokens[2]);
    End
    Else Valid := False;
    HasLoc := Valid;
    SetLength(Tokens,0);
  End; // ParseLoc

  Procedure ParseSize;
  Var Tokens: TTokenArray;
  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) = 2 Then
    Begin
      Valid := Valid And FSize.ParseTokens(Tokens[0],Tokens[1],Tokens[2]);
    End
    Else Valid := False;
    HasSize := Valid;
    SetLength(Tokens,0);
  End; // ParseSize

  Procedure ParseHeading;
  Var Tokens: TTokenArray;
  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) = 2 Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],FHeading.FXAngle) And
                         GetSingleValue(Tokens[1],FHeading.FYAngle) And
                         GetSingleValue(Tokens[2],FHeading.FZAngle);
    End
    Else Valid := False;
    HasHeading := Valid;
    SetLength(Tokens,0);
  End; // ParseHeading

Begin
  Valid      := True;
  HasName    := False;
  HasLoc     := False;
  HasHeading := False;
  HasSize    := False;
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  While Valid And (Not (HasName And HasLoc And HasHeading And HasSize)) And Not Eof(F) Do
  Begin
    ReadLn(F,Line);
    Line := Trim(Line);
    S    := UpperCase(Line);
    If S <> '' Then
    Begin
      Command := GetToken(' ',S);
      Cmd     := GetToken(' ',Line);
           If Command = 'NAME'             Then ParseName
      Else If Command = 'LOC'              Then ParseLoc
      Else If Command = 'HEADING'          Then ParseHeading
      Else If Command = 'SIZE'             Then ParseSize
      Else Valid := False;
    End;
  End; // While
  DecimalSeparator := DecSep;
  Result := Valid;
End; // TZoneObject.LoadFromFile

Procedure TZoneObject.SaveToFile(Var F: System.Text; Indent: Integer);
Var St: String;
Begin
  St := GetIndent(Indent);
  WriteLn(F,St + '  DecimalSeparator ' + DecimalSeparator);
  WriteLn(F,St + '  Name    ' + FName);
  WriteLn(F,St + '  Loc     ' + FloatToStr(FLoc.X)           + ', ' + FloatToStr(FLoc.Y)           + ', ' + FloatToStr(FLoc.Z));
  WriteLn(F,St + '  Heading ' + FloatToStr(FHeading.FXAngle) + ', ' + FloatToStr(FHeading.FYAngle) + ', ' + FloatToStr(FHeading.FZAngle));
  WriteLn(F,St + '  Size    ' + FloatToStr(FSize.X)          + ', ' + FloatToStr(FSize.Y)          + ', ' + FloatToStr(FSize.Z));
End; // TZoneObject.SaveToFile

Procedure TZoneObject.SetZone(Z: TZone);
Begin
  FZone := Z;
End; // TZoneObject.SetZone

Function TZoneObject.GetParent: TGroupObject;
Begin
  Result := FParent;
End; // TZoneObject.GetParent

Procedure TZoneObject.SetParent(GO: TGroupObject);
Begin
  FParent := GO;
End; // TZoneObject.SetParent

Function TZoneObject.GetZone: TZone;
Begin
  Result := FZone;
End; // TZoneObject.GetZone

Function TZoneObject.GetHighestParent: TZoneObject;
Var P: TGroupObject;
Begin
  P := FParent;
  While (P <> Nil) And (P.FParent <> Nil) Do P := P.FParent;
  If P <> Nil Then Result := P Else Result := Self;
End; // TZoneObject.GetHighestParent

Function TZoneObject.GetName: String;
Begin
  Result := FName;
End; // TZoneObject.GetName

Procedure TZoneObject.SetName(St: String);
Var I: Integer;
Begin
  FName := St;
  If FParent <> Nil Then
  Begin
    I := FParent.Objects.IndexOfObject(Self);
    If I >= 0 Then FParent.Objects.Strings[I] := St;
  End
  Else If FZone <> Nil Then
  Begin
    I := FZone.IndexOfObject(Self);
    If I >= 0 Then FZone.Strings[I] := St;
  End;
End; // TZoneObject.SetName

Function TZoneObject.GetIndent(Indent: Integer): String;
Var St: String;
Begin
  St := '';
  While Indent > 0 Do
  Begin
    St := St + '  ';
    Dec(Indent);
  End; // While
  Result := St;
End; // TZoneObject.GetIndent

// ------------------------------
// TGroupObject
// ------------------------------

Constructor TGroupObject.Create;
Begin
  Inherited;
  Objects  := TStringList.Create;
  Category := '';
End; // TGroupObject.Create

Constructor TGroupObject.Create(GO: TGroupObject);
Var
  I  : Integer;
  ZO : TZoneObject;

Begin
  Inherited Create(GO);
  Objects  := TStringList.Create;
  Category := GO.Category;
  For I := 0 To GO.Objects.Count - 1 Do
  Begin
    ZO := TZoneObject(GO.Objects.Objects[I]).MakeCopy;
    Objects.AddObject(ZO.FName,ZO);
    ZO.FParent := Self;
  End; // For I
End; // TGroupObject.Create

Constructor TGroupObject.Create(AName: String);
Begin
  Inherited Create(AName);
  Objects  := TStringList.Create;
  Category := '';
End; // TGroupObject.Create

Destructor TGroupObject.Destroy;
Var I: Integer;
Begin
  For I := 0 To Objects.Count - 1 Do Objects.Objects[I].Free;
  Objects.Free;
  Inherited;
End; // TGroupObject.Destroy

Function TGroupObject.NameExists(St: String; ToUpper: Boolean): Boolean;
Var
  I     : Integer;
  ZO    : TZoneObject;
  Found : Boolean;

Begin
  I     := 0;
  If ToUpper Then St := UpperCase(St);
  Found := Inherited NameExists(St,ToUpper);
  While (I < Objects.Count) And Not Found Do
  Begin
    ZO := TZoneObject(Objects.Objects[I]);
    If UpperCase(ZO.FName) = St Then Found := True Else
    Begin
      If ZO Is TGroupObject Then Found := ZO.NameExists(St,ToUpper);
      Inc(I);
    End;
  End; // While
  Result := Found;
End; // NameExists

Function TGroupObject.FindObjectByName(St: String): TZoneObject;
Var
  I   : Integer;
  ZO  : TZoneObject;
  ZO1 : TZoneObject;

Begin
  I   := 0;
  St  := UpperCase(St);
  ZO1 := Inherited FindObjectByName(St);
  While (I < Objects.Count) And (ZO1 = Nil) Do
  Begin
    ZO := TZoneObject(Objects.Objects[I]);
    If UpperCase(ZO.FName) = St Then ZO1 := ZO Else
    Begin
      If ZO Is TGroupObject Then ZO1 := ZO.FindObjectByName(St);
      Inc(I);
    End;
  End; // While
  Result := ZO1;
End; // TGroupObject.FindObjectByName

Function TGroupObject.ContainsObject(ZO: TZoneObject): Boolean;
Var
  I     : Integer;
  Found : Boolean;
  ZO1   : TZoneObject;

Begin
  I     := 0;
  Found := False;
  While (I < Objects.Count) And Not Found Do
  Begin
    ZO1 := TZoneObject(Objects.Objects[I]);
         If ZO1 = ZO Then Found := True
    Else If ZO1 Is TGroupObject Then Found := Found Or TGroupObject(ZO1).ContainsObject(ZO)
    Else Inc(I);
  End; // While
  Result := Found;
End; // TGroupObject.ContainsObject

Function TGroupObject.GetHotSpots: TStringList;
Var List: TStringList;

  Procedure Process(GO: TGroupObject; List: TStringList);
  Var
    I  : Integer;
    ZO : TZoneObject;

  Begin
    I := 0;
    While I < GO.Objects.Count Do
    Begin
      ZO := TZoneObject(GO.Objects.Objects[I]);
      If ZO Is THotSpot Then
      Begin
        List.AddObject(ZO.FName,ZO);
        Inc(I);
      End
      Else If ZO Is TGroupObject Then Process(TGroupObject(ZO),List)
      Else Inc(I);
    End; // While
  End; // Process

Begin
  List := TStringList.Create;
  Process(Self,List);
  Result := List;
End; // TGroupObject.GetHotSpots

Function TGroupObject.FindModelOrigin: TModelOrigin;
Var
  I  : Integer;
  ZO : TZoneObject;
  MO : TModelOrigin;

Begin
  I  := 0;
  MO := Nil;
  While (I < Objects.Count) And (MO = Nil) Do
  Begin
    ZO := TZoneObject(Objects.Objects[I]);
         If ZO Is TModelOrigin Then MO := TModelOrigin(ZO)
    Else If ZO Is TGroupObject Then MO := TGroupObject(ZO).FindModelOrigin;
    Inc(I);
  End; // While
  Result := MO;
End; // TGroupObject.FindModelOrigin

Function TGroupObject.MakeCopy: TZoneObject;
Begin
  Result := TGroupObject.Create(Self);
End; // TGroupObject.MakeCopy

Function TGroupObject.LoadFromFile(Var F: System.Text): Boolean;
Var
  Valid   : Boolean;
  Done    : Boolean;
  Line    : String;
  S       : String; // Mirrors Line, but always uppercase
  Command : String; // Always uppercase
  Cmd     : String; // Mirrors Command, but preserves case

  Procedure ParseCount;
  Begin
  End; // ParseCount

  Procedure ParseMesh;
  Var MO: TMeshObject;
  Begin
    MO := TMeshObject.Create;
    Valid := Valid And MO.LoadFromFile(F);
    If Valid Then
    Begin
      Objects.AddObject(MO.FName,MO);
      MO.FParent := Self;
      MO.SetZone(FZone);

      // We must *not* call TMeshObject.Coalesce or vertex normals get screwed up
      // (sharing vertices is BAD, BAD, BAD)

//      MO.Coalesce;
    End;
  End; // ParseMesh

  Procedure ParseMeshLibraryReference;
  Var ML: TMeshLibraryObjectReference;
  Begin
    ML    := TMeshLibraryObjectReference.Create;
    Valid := Valid And ML.LoadFromFile(F);
    If Valid Then
    Begin
      Objects.AddObject(ML.FName,ML);
      ML.FParent := Self;
      ML.SetZone(FZone);
    End;
  End; // ParseMeshLibraryReference

  Procedure ParseCreatureLibraryReference;
  Var CL: TCreatureLibraryObjectReference;
  Begin
    CL    := TCreatureLibraryObjectReference.Create;
    Valid := Valid And CL.LoadFromFile(F);
    If Valid Then
    Begin
      Objects.AddObject(CL.FName,CL);
      CL.FParent := Self;
      CL.SetZone(FZone);
    End;
  End; // ParseCreatureLibraryReference

  Procedure ParseLight;
  Var L: TLightObject;
  Begin
    L     := TLightObject.Create;
    Valid := Valid And L.LoadFromFile(F);
    If Valid Then
    Begin
      Objects.AddObject(L.FName,L);
      L.FParent := Self;
      L.SetZone(FZone);
    End;
  End; // ParseLight

  Procedure ParseModelOrigin;
  Var MO: TModelOrigin;
  Begin
    MO    := TModelOrigin.Create;
    Valid := Valid And MO.LoadFromFile(F);
    If Valid Then
    Begin
      Objects.AddObject(MO.FName,MO);
      MO.FParent := Self;
      MO.SetZone(FZone);
    End;
  End; // ParseModelOrigin

  Procedure ParseHotSpot;
  Var HS: THotSpot;
  Begin
    HS    := THotSpot.Create;
    Valid := Valid And HS.LoadFromFile(F);
    If Valid Then
    Begin
      Objects.AddObject(HS.FName,HS);
      HS.FParent := Self;
      HS.SetZone(FZone);
    End;
  End; // ParseHotSpot

  Procedure ParseGroup;
  Var GO: TGroupObject;
  Begin
    GO := TGroupObject.Create;
    Valid := Valid And GO.LoadFromFile(F);
    If Valid Then
    Begin
      Objects.AddObject(GO.FName,GO);
      GO.FParent := Self;
      GO.SetZone(FZone);
    End;
  End; // ParseGroup

  Procedure ParseScript;
  Var SO: TScriptedObject;
  Begin
    SO := TScriptedObject.Create;
    SO.FScriptName := GetToken(' ',Line);
    Valid := Valid And SO.LoadFromFile(F);
    If Valid Then
    Begin
      Objects.AddObject(SO.FName,SO);
      SO.FParent := Self;
      SO.SetZone(FZone);
    End;
  End; // ParseScript

  Procedure ParseCategory;
  Begin
    Category := Line;
  End; // ParseCategory

Begin
  Valid := Inherited LoadFromFile(F);
  Done  := False;
  While Valid And (Not Done) And Not Eof(F) Do
  Begin
    ReadLn(F,Line);
    Line := Trim(Line);
    S    := UpperCase(Line);
    If S <> '' Then
    Begin
      Command := GetToken(' ',S);
      Cmd     := GetToken(' ',Line);
           If Command = 'COUNT'                Then ParseCount
      Else If Command = 'CATEGORY'             Then ParseCategory                   
      Else If Command = 'MESH'                 Then ParseMesh
      Else If Command = 'MESHLIBRARYREFERENCE' Then ParseMeshLibraryReference
      Else If Command = 'LIGHT'                Then ParseLight
      Else If Command = 'MODELORIGIN'          Then ParseModelOrigin
      Else If Command = 'HOTSPOT'              Then ParseHotSpot
      Else If Command = 'GROUP'                Then ParseGroup
      Else If Command = 'SCRIPT'               Then ParseScript
      Else If Command = 'END'                  Then Done := True
      Else Valid := False;
    End;
  End; // While
  Result := Valid;
End; // TGroupObject.LoadFromFile

Procedure TGroupObject.SaveToFile(Var F: System.Text; Indent: Integer);
Var
  I      : Integer;
  St     : String;
  DecSep : Char;

Begin
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  St := GetIndent(Indent);
  WriteLn(F,St + 'Group');
  Inherited SaveToFile(F,Indent);
  WriteLn(F,St + '  Count   ' + IntToStr(Objects.Count));
  If Category <> '' Then WriteLn(F,St + '  Category ' + Category);
  For I := 0 To Objects.Count - 1 Do TZoneObject(Objects.Objects[I]).SaveToFile(F,Indent + 1);
  WriteLn(F,St + 'End');
  DecimalSeparator := DecSep;
End; // TGroupObject.SaveToFile

Procedure TGroupObject.SetZone(Z: TZone);
Var I: Integer;
Begin
  Inherited SetZone(Z);
  For I := 0 To Objects.Count - 1 Do TZoneObject(Objects.Objects[I]).SetZone(Z);
End; // TGroupObject.SetZone

Procedure TGroupObject.AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean);
Var
  I  : Integer;
  ZO : TZoneObject;

Begin
  For I := 0 To Objects.Count - 1 Do
  Begin
    ZO := TZoneObject(Objects.Objects[I]);

    // Add the sub-object's polygons (it will change itself to absolute coordinates)

    If ((Not (ExcludeMeshReferences     And (ZO Is TMeshLibraryObjectReference))) And
        (Not (ExcludeCreatureReferences And (ZO Is TCreatureLibraryObjectReference)))) Or
       ((ZO Is TMeshLibraryObjectReference) And TMeshLibraryObjectReference(ZO).InsertMesh And Not ExcludeInserts) Then
     ZO.AddToPolygonList(List,ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform);
  End; // For I
End; // TGroupObject.AddToPolygonList

Procedure TGroupObject.Group(ZO: TZoneObject);
Begin
  // Not totally accurate (needs to take group rotation into account)

  If (FParent <> ZO.FParent) And (FParent <> Nil) Then FParent.Group(ZO);
  ZO.FLoc.Subtract(FLoc);
  If Size.X <> 0 Then ZO.Loc.X := ZO.Loc.X / Size.X;
  If Size.Y <> 0 Then ZO.Loc.Y := ZO.Loc.Y / Size.Y;
  If Size.Z <> 0 Then ZO.Loc.Z := ZO.Loc.Z / Size.Z;
  FHeading.NegativeRotate(ZO.FLoc);
  If Size.X <> 0 Then ZO.Size.X := ZO.Size.X / Size.X;
  If Size.Y <> 0 Then ZO.Size.Y := ZO.Size.Y / Size.Y;
  If Size.Z <> 0 Then ZO.Size.Z := ZO.Size.Z / Size.Z;
  If ZO.FParent <> Nil
   Then ZO.FParent.Objects.Delete(ZO.FParent.Objects.IndexOfObject(ZO))
   Else If ZO.FZone <> Nil Then ZO.FZone.Delete(ZO.FZone.IndexOfObject(ZO));
  ZO.FParent := Self;
  ZO.SetZone(FZone);
  Objects.AddObject(ZO.FName,ZO);
End; // TGroupObject.Group

Procedure TGroupObject.UnGroup(ZO: TZoneObject);
Var
  I     : Integer;
  P     : T3DPoint;
  Index : Integer;

Begin
  I := Objects.IndexOfObject(ZO);
  If I >= 0 Then
  Begin
    // Find out where in our parent object to place the moved sub-object

         If FParent <> Nil Then Index := FParent.Objects.IndexOfObject(Self)
    Else If FZone   <> Nil Then Index := FZone.IndexOfObject(Self)
    Else Index := -1;

    // Find out where to place the sub-object relative to the group, accounting for overall group size and rotation

    P   := T3DPoint.Create;
    P.X := FSize.X * ZO.FLoc.X;
    P.Y := FSize.Y * ZO.FLoc.Y;
    P.Z := FSize.Z * ZO.FLoc.Z;
    FHeading.Rotate(P);

    // Set the sub-object's position

    ZO.FLoc.X := FLoc.X + P.X;
    ZO.FLoc.Y := FLoc.Y + P.Y;
    ZO.FLoc.Z := FLoc.Z + P.Z;
    P.Free;

    // Set the sub-object's rotational angles
{
    ZO.FHeading.FXAngle := ZO.FHeading.FXAngle + FHeading.FXAngle;
    ZO.FHeading.FYAngle := ZO.FHeading.FYAngle + FHeading.FYAngle;
    ZO.FHeading.FZAngle := ZO.FHeading.FZAngle + FHeading.FZAngle;
}
    ZO.FHeading.Multiply(FHeading);
    ZO.FHeading.Standardize;

    // Set the sub-object's size

    ZO.FSize.X := ZO.FSize.X * FSize.X;
    ZO.FSize.Y := ZO.FSize.Y * FSize.Y;
    ZO.FSize.Z := ZO.FSize.Z * FSize.Z;

    // Set the sub-object's parent, and if there is no parent, then add it to the zone directly

    ZO.FParent := FParent;
         If FParent <> Nil Then FParent.Objects.InsertObject(Index,ZO.FName,ZO)
    Else If FZone   <> Nil Then FZone.InsertObject(Index,ZO.FName,ZO);

    // Get rid of the sub-object

    Objects.Delete(I);
  End;
End; // TGroupObject.UnGroup

Procedure TGroupObject.UnGroupAll;
Begin
  While Objects.Count > 0 Do Ungroup(TZoneObject(Objects.Objects[0]));
End; // TGroupObject.UnGroupAll

// ------------------------------
// TAnimatedGroupObject
// ------------------------------

Constructor TAnimatedGroupObject.Create;
Begin
  Inherited;
  FFrames       := 0;
  FAmplitude    := T3DPoint.Create;
  FAttenuation  := T3DPoint.Create;
  FFrequency    := T3DPoint.Create;
  FPhase        := T3DPoint.Create;
  FSubdivisions := T3DPoint.Create;
End; // TAnimatedGroupObject.Create

Constructor TAnimatedGroupObject.Create(GO: TAnimatedGroupObject);
Begin
  Inherited Create(GO);
  FFrames       := 0;
  FAmplitude    := T3DPoint.Create(GO.FAmplitude);
  FAttenuation  := T3DPoint.Create(GO.FAttenuation);
  FFrequency    := T3DPoint.Create(GO.FFrequency);
  FPhase        := T3DPoint.Create(GO.FPhase);
  FSubdivisions := T3DPoint.Create(GO.FSubdivisions);
End; // TAnimatedGroupObject.Create

Constructor TAnimatedGroupObject.Create(AName: String);
Begin
  Inherited Create(AName);
  FFrames       := 0;
  FAmplitude    := T3DPoint.Create;
  FAttenuation  := T3DPoint.Create;
  FFrequency    := T3DPoint.Create;
  FPhase        := T3DPoint.Create;
  FSubdivisions := T3DPoint.Create;
End; // TAnimatedGroupObject.Create

Destructor TAnimatedGroupObject.Destroy;
Begin
  Inherited;
  FAmplitude.Free;
  FAttenuation.Free;
  FFrequency.Free;
  FPhase.Free;
  FSubdivisions.Free;
End; // TAnimatedGroupObject.Destroy

Function TAnimatedGroupObject.MakeCopy: TZoneObject;
Begin
  Result := TAnimatedGroupObject.Create(Self);
End; // TAnimatedGroupObject.MakeCopy

Function TAnimatedGroupObject.LoadFromFile(Var F: System.Text): Boolean;
Var
  Valid   : Boolean;
  Done    : Boolean;
  Line    : String;
  S       : String; // Mirrors Line, but always uppercase
  Command : String; // Always uppercase
  Cmd     : String; // Mirrors Command, but preserves case

  Procedure ParseAmplitude;
  Var Tokens: TTokenArray;
  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) = 2 Then
    Begin
      Valid := Valid And FAmplitude.ParseTokens(Tokens[0],Tokens[1],Tokens[2]);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseAmplitude

  Procedure ParseAttenuation;
  Var Tokens: TTokenArray;
  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) = 2 Then
    Begin
      Valid := Valid And FAttenuation.ParseTokens(Tokens[0],Tokens[1],Tokens[2]);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseAttenuation

  Procedure ParseFrequency;
  Var Tokens: TTokenArray;
  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) = 2 Then
    Begin
      Valid := Valid And FFrequency.ParseTokens(Tokens[0],Tokens[1],Tokens[2]);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseFrequency

  Procedure ParsePhase;
  Var Tokens: TTokenArray;
  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) = 2 Then
    Begin
      Valid := Valid And FPhase.ParseTokens(Tokens[0],Tokens[1],Tokens[2]);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParsePhase

  Procedure ParseSubdivisions;
  Var Tokens: TTokenArray;
  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) = 2 Then
    Begin
      Valid := Valid And FSubdivisions.ParseTokens(Tokens[0],Tokens[1],Tokens[2]);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseSubdivisions

  Procedure ParseFrames;
  Var Tokens: TTokenArray;
  Begin
    GetTokens(',',S,Tokens);
    If High(Tokens) = 1
     Then Valid := Valid And GetIntValue(Tokens[0],FFrames)
     Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseFrames

Begin
  Valid := Inherited LoadFromFile(F);
  Done  := False;
  While Valid And (Not Done) And Not Eof(F) Do
  Begin
    ReadLn(F,Line);
    Line := Trim(Line);
    S    := UpperCase(Line);
    If S <> '' Then
    Begin
      Command := GetToken(' ',S);
      Cmd     := GetToken(' ',Line);
           If Command = 'AMPLITUDE'            Then ParseAmplitude
      Else If Command = 'ATTENUATION'          Then ParseAttenuation
      Else If Command = 'FREQUENCY'            Then ParseFrequency
      Else If Command = 'PHASE'                Then ParsePhase
      Else If Command = 'SUBDIVISIONS'         Then ParseSubdivisions
      Else If Command = 'FRAMES'               Then ParseFrames
      Else If Command = 'END'                  Then Done := True
      Else Valid := False;
    End;
  End; // While
  Result := Valid;
End; // TAnimatedGroupObject.LoadFromFile

Procedure TAnimatedGroupObject.SaveToFile(Var F: System.Text; Indent: Integer);
Var
  I      : Integer;
  St     : String;
  DecSep : Char;

Begin
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  St := GetIndent(Indent);
  WriteLn(F,St + 'AnimatedGroup');
  WriteLn(F,St + '  Count   ' + IntToStr(Objects.Count));
  For I := 0 To Objects.Count - 1 Do TZoneObject(Objects.Objects[I]).SaveToFile(F,Indent + 1);
  WriteLn(F,St + '  Frames  '      + IntToStr(FFrames));
  WriteLn(F,St + '  Amplitude '    + FloatToStr(FAmplitude.X)    + ', ' + FloatToStr(FAmplitude.Y)    + ', ' + FloatToStr(FAmplitude.Z));
  WriteLn(F,St + '  Attenuation '  + FloatToStr(FAttenuation.X)  + ', ' + FloatToStr(FAttenuation.Y)  + ', ' + FloatToStr(FAttenuation.Z));
  WriteLn(F,St + '  Frequency '    + FloatToStr(FFrequency.X)    + ', ' + FloatToStr(FFrequency.Y)    + ', ' + FloatToStr(FFrequency.Z));
  WriteLn(F,St + '  Phase '        + FloatToStr(FPhase.X)        + ', ' + FloatToStr(FPhase.Y)        + ', ' + FloatToStr(FPhase.Z));
  WriteLn(F,St + '  Subdivisions ' + FloatToStr(FSubdivisions.X) + ', ' + FloatToStr(FSubdivisions.Y) + ', ' + FloatToStr(FSubdivisions.Z));
  WriteLn(F,St + 'End');
  DecimalSeparator := DecSep;
End; // TAnimatedGroupObject.SaveToFile

Procedure TAnimatedGroupObject.SetAmplitude(P: T3DPoint);
Begin
  FAmplitude.Copy(P);
End; // TAnimatedGroupObject.SetAmplitude

Procedure TAnimatedGroupObject.SetAttenuation(P: T3DPoint);
Begin
  FAttenuation.Copy(P);
End; // TAnimatedGroupObject.SetAttenuation

Procedure TAnimatedGroupObject.SetFrequency(P: T3DPoint);
Begin
  FFrequency.Copy(P);
End; // TAnimatedGroupObject.SetFrequency

Procedure TAnimatedGroupObject.SetPhase(P: T3DPoint);
Begin
  FPhase.Copy(P);
End; // TAnimatedGroupObject.SetPhase

Procedure TAnimatedGroupObject.SetSubdivisions(P: T3DPoint);
Begin
  FSubdivisions.Copy(P);
End; // TAnimatedGroupObject.SetSubdivisions

// ------------------------------
// TLightObject
// ------------------------------

Constructor TLightObject.Create;
Begin
  Inherited;
  FRadius  := 100;
  FColor   := clWhite;
  FFlicker := 0;
End; // TLightObject.Create

Constructor TLightObject.Create(AName: String);
Begin
  Inherited Create(AName);
  FRadius  := 100;
  FColor   := clWhite;
  FFlicker := 0;
End; // TLightObject.Create

Constructor TLightObject.Create(L: TLightObject);
Begin
  Inherited Create(L);
  FRadius  := L.FRadius;
  FColor   := L.FColor;
  FFlicker := L.FFlicker;
End; // TLightObject.Create

Procedure TLightObject.AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean);
Begin
  // Add an empty mesh object

  List.AddObject(FName,TMeshObject.Create(FName));
End; // TLightObject.AddToPolygonList

Procedure TLightObject.GetSize(Out MinPos,MaxPos: T3DPoint; Var FirstTex: String);
Var AbsLoc: T3DPoint;
Begin
  AbsLoc := GetAbsoluteLocation;
  MinPos := T3DPoint.Create(AbsLoc);
  MaxPos := T3DPoint.Create(AbsLoc);
  MinPos.Subtract(Radius,Radius,Radius);
  MaxPos.Add(Radius,Radius,Radius);
  FirstTex := '';
  AbsLoc.Free;
End; // TLightObject.GetSize

Function TLightObject.LoadFromFile(Var F: System.Text): Boolean;
Var
  Valid   : Boolean;
  Done    : Boolean;
  Line    : String;
  S       : String; // Mirrors Line, but always uppercase
  Command : String; // Always uppercase
  Cmd     : String; // Mirrors Command, but preserves case

  Procedure ParseRadius;
  Var
    R      : Single;
    Tokens : TTokenArray;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) = 0 Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],R);
      If Valid Then FRadius := R;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseRadius

  Procedure ParseFlicker;
  Var
    R      : Single;
    Tokens : TTokenArray;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) = 0 Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],R);
      If Valid Then FFlicker := R;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseFlicker

  Procedure ParseColor;
  Var
    C      : Integer;
    Tokens : TTokenArray;

  Begin
    GetTokens(',',S,Tokens);
    If High(Tokens) = 0 Then
    Begin
      Valid := Valid And GetIntValue(Tokens[0],C);
      If Valid Then FColor := TColor(C);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseColor

Begin
  Valid := Inherited LoadFromFile(F);
  Done  := False;
  While Valid And (Not Done) And Not Eof(F) Do
  Begin
    ReadLn(F,Line);
    Line := Trim(Line);
    S    := UpperCase(Line);
    If S <> '' Then
    Begin
      Command := GetToken(' ',S);
      Cmd     := GetToken(' ',Line);
           If Command = 'RADIUS'  Then ParseRadius
      Else If Command = 'COLOR'   Then ParseColor
      Else If Command = 'FLICKER' Then ParseFlicker
      Else If Command = 'END'     Then Done := True
      Else Valid := False;
    End;
  End; // While
  Result := Valid;
End; // TLightObject.LoadFromFile

Procedure TLightObject.SaveToFile(Var F: System.Text; Indent: Integer);
Var
  St     : String;
  DecSep : Char;

Begin
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  St := GetIndent(Indent);
  WriteLn(F,St + 'Light');
  Inherited SaveToFile(F,Indent);
  WriteLn(F,St + '  Radius ' + FloatToStr(Radius));
  WriteLn(F,St + '  Color  $' + IntToHex(LongWord(Color),8));
  WriteLn(F,St + '  Flicker ' + FloatToStr(Flicker));
  WriteLn(F,St + 'End');
  DecimalSeparator := DecSep;
End; // TLightObject.SaveToFile

Function TLightObject.MakeCopy: TZoneObject;
Begin
  Result := TLightObject.Create(Self);
End; // TLightObject.MakeCopy

Procedure TLightObject.SetRadius(R: Single);
Begin
  FRadius := R;
  frmMain.RefreshObject(Self,True,True,True);
  Application.ProcessMessages;
End; // TLightObject.SetRadius

Procedure TLightObject.SetColor(C: TColor);
Begin
  FColor := C;
  frmMain.RefreshObject(Self,True,True,True);
  Application.ProcessMessages;
End; // TLightObject.SetColor

Procedure TLightObject.SetFlicker(F: Single);
Begin
  FFlicker := F;
  frmMain.RefreshObject(Self,True,True,True);
  Application.ProcessMessages;
End; // TLightObject.SetFlicker

// ------------------------------
// TModelOrigin
// ------------------------------

Constructor TModelOrigin.Create;
Begin
  Inherited;
End; // TModelOrigin.Create

Constructor TModelOrigin.Create(AName: String);
Begin
  Inherited Create(AName);
End; // TModelOrigin.Create

Constructor TModelOrigin.Create(MO: TModelOrigin);
Begin
  Inherited Create(MO);
End; // TModelOrigin.Create

Procedure TModelOrigin.AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean);
Begin
  // Add an empty mesh object

  List.AddObject(FName,TMeshObject.Create(FName));
End; // TModelOrigin.AddToPolygonList

Procedure TModelOrigin.GetSize(Out MinPos,MaxPos: T3DPoint; Var FirstTex: String);
Var AbsLoc: T3DPoint;
Begin
  AbsLoc := GetAbsoluteLocation;
  MinPos := T3DPoint.Create(AbsLoc);
  MaxPos := T3DPoint.Create(AbsLoc);
  MinPos.Subtract(0.5,0.5,0.5);
  MaxPos.Add(0.5,0.5,0.5);
  FirstTex := '';
  AbsLoc.Free;
End; // TModelOrigin.GetSize

Function TModelOrigin.LoadFromFile(Var F: System.Text): Boolean;
Var
  Valid   : Boolean;
  Done    : Boolean;
  Line    : String;
  S       : String; // Mirrors Line, but always uppercase
  Command : String; // Always uppercase
  Cmd     : String; // Mirrors Command, but preserves case

Begin
  Valid := Inherited LoadFromFile(F);
  Done  := False;
  While Valid And (Not Done) And Not Eof(F) Do
  Begin
    ReadLn(F,Line);
    Line := Trim(Line);
    S    := UpperCase(Line);
    If S <> '' Then
    Begin
      Command := GetToken(' ',S);
      Cmd     := GetToken(' ',Line);
      If Command = 'END'
       Then Done  := True
       Else Valid := False;
    End;
  End; // While
  Result := Valid;
End; // TModelOrigin.LoadFromFile

Procedure TModelOrigin.SaveToFile(Var F: System.Text; Indent: Integer);
Var
  St     : String;
  DecSep : Char;
  
Begin
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  St := GetIndent(Indent);
  WriteLn(F,St + 'ModelOrigin');
  Inherited SaveToFile(F,Indent);
  WriteLn(F,St + 'End');
  DecimalSeparator := DecSep;
End; // TModelOrigin.SaveToFile

Function TModelOrigin.MakeCopy: TZoneObject;
Begin
  Result := TModelOrigin.Create(Self);
End; // TModelOrigin.MakeCopy

// ------------------------------
// THotSpot
// ------------------------------

Constructor THotSpot.Create;
Begin
  Inherited;
End; // THotSpot.Create

Constructor THotSpot.Create(AName: String);
Begin
  Inherited Create(AName);
End; // THotSpot.Create

Constructor THotSpot.Create(HS: THotSpot);
Begin
  Inherited Create(HS);
End; // THotSpot.Create

Procedure THotSpot.AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean);
Begin
  // Add an empty mesh object

  List.AddObject(FName,TMeshObject.Create(FName));
End; // THotSpot.AddToPolygonList

Procedure THotSpot.GetSize(Out MinPos,MaxPos: T3DPoint; Var FirstTex: String);
Var AbsLoc: T3DPoint;
Begin
  AbsLoc := GetAbsoluteLocation;
  MinPos := T3DPoint.Create(AbsLoc);
  MaxPos := T3DPoint.Create(AbsLoc);
  FirstTex := '';
  AbsLoc.Free;
End; // THotSpot.GetSize

Function THotSpot.LoadFromFile(Var F: System.Text): Boolean;
Var
  Valid   : Boolean;
  Done    : Boolean;
  Line    : String;
  S       : String; // Mirrors Line, but always uppercase
  Command : String; // Always uppercase
  Cmd     : String; // Mirrors Command, but preserves case

Begin
  Valid := Inherited LoadFromFile(F);
  Done  := False;
  While Valid And (Not Done) And Not Eof(F) Do
  Begin
    ReadLn(F,Line);
    Line := Trim(Line);
    S    := UpperCase(Line);
    If S <> '' Then
    Begin
      Command := GetToken(' ',S);
      Cmd     := GetToken(' ',Line);
      If Command = 'END'
       Then Done  := True
       Else Valid := False;
    End;
  End; // While
  Result := Valid;
End; // THotSpot.LoadFromFile

Procedure THotSpot.SaveToFile(Var F: System.Text; Indent: Integer);
Var
  St     : String;
  DecSep : Char;

Begin
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  St := GetIndent(Indent);
  WriteLn(F,St + 'HotSpot');
  Inherited SaveToFile(F,Indent);
  WriteLn(F,St + 'End');
  DecimalSeparator := DecSep;
End; // THotSpot.SaveToFile

Function THotSpot.MakeCopy: TZoneObject;
Begin
  Result := THotSpot.Create(Self);
End; // THotSpot.MakeCopy

// ------------------------------
// TMeshObject
// ------------------------------

Constructor TMeshObject.Create;
Begin
  Inherited Create;
  Vertices := TStringList.Create;
  Polygons := TStringList.Create;
  Normals  := TStringList.Create;
End; // TMeshObject.Create

Constructor TMeshObject.Create(MO: TMeshObject);
Var I: Integer;
Begin
  Inherited Create(MO);
  Vertices := TStringList.Create;
  Polygons := TStringList.Create;
  Normals  := TStringList.Create;
  For I := 0 To MO.Vertices.Count - 1 Do Vertices.AddObject(MO.Vertices.Strings[I],T3DPoint.Create(T3DPoint(MO.Vertices.Objects[I])));
  For I := 0 To MO.Polygons.Count - 1 Do Polygons.AddObject(MO.Polygons.Strings[I],TPolygon.Create(TPolygon(MO.Polygons.Objects[I])));
  For I := 0 To MO.Normals.Count  - 1 Do Normals.AddObject(MO.Normals.Strings[I],T3DPoint.Create(T3DPoint(MO.Normals.Objects[I])));
  SetLength(BoneIndices,High(MO.BoneIndices) + 1);
  For I := 0 To High(BoneIndices) Do BoneIndices[I] := MO.BoneIndices[I];
  CalcNormals;
End; // TMeshObject.Create

Constructor TMeshObject.Create(AName: String);
Begin
  Inherited Create(AName);
  Vertices := TStringList.Create;
  Polygons := TStringList.Create;
  Normals  := TStringList.Create;
End; // TMeshObject.Create

Constructor TMeshObject.Create(AName: String; AX,AY,AZ: Single);
Begin
  Inherited Create(AName,AX,AY,AZ);
  Vertices := TStringList.Create;
  Polygons := TStringList.Create;
  Normals  := TStringList.Create;
End; // TMeshObject.Create

Constructor TMeshObject.Create(AName: String; AX,AY,AZ,SX,SY,SZ: Single);
Begin
  Inherited Create(AName,AX,AY,AZ,SX,SY,SZ);
  Vertices := TStringList.Create;
  Polygons := TStringList.Create;
  Normals  := TStringList.Create;
End; // TMeshObject.Create

Destructor TMeshObject.Destroy;
Var I: LongInt;
Begin
  For I := 0 To Vertices.Count - 1 Do Vertices.Objects[I].Free;
  For I := 0 To Polygons.Count - 1 Do Polygons.Objects[I].Free;
  For I := 0 To Normals.Count  - 1 Do Normals.Objects[I].Free;
  Vertices.Free;
  Polygons.Free;
  Normals.Free;
  SetLength(BoneIndices,0);
  Inherited;
End; // TMeshObject.Destroy

Procedure TMeshObject.ShadePolygonsByLights(LightTreeHash: TLightTreeHash; PolygonList: PIntegerArray; Center: T3DPoint; DistToCenter: Single);
Var
  I,J,K,L     : Integer;
  PolyIndex   : Integer;
  RegionIndex : Integer;
  CR          : Integer;
  CG          : Integer;
  CB          : Integer;
  Ray         : T3DPoint;
  PolyNormal  : T3DPoint;
  PointNormal : T3DPoint;
  ToCenter    : T3DPoint;
  Vertex      : T3DPoint;
  PolyCenter  : T3DPoint;
  Intersect   : T3DPoint;
  AbsCenter   : T3DPoint;
  NearRegions : TStringList; // List of TRegion
  Light       : TLightObject;
  Dot         : Single;
  Intensity   : Single;
  P           : TPolygon;
  P1          : TPolygon;
  Found       : Boolean;
  LightColor  : TColor;
  Region      : TRegion;
  PolyCount   : Integer;
  PolyNormals : TStringList;
  PolyCenters : TStringList;
  MatchingNormals : Array Of Boolean;

Begin
  Ray         := T3DPoint.Create;
  PointNormal := T3DPoint.Create;
  ToCenter    := T3DPoint.Create;
  AbsCenter   := T3DPoint.Create(Center);
  Vertex      := T3DPoint.Create;
  Intersect   := T3DPoint.Create;
  PolyNormals := TStringList.Create;
  PolyCenters := TStringList.Create;
  MakeAbsolute(AbsCenter);

  // Iterate through all light sources

  For K := 0 To LightTreeHash.Lights.Count - 1 Do
  Begin
    // Check this light only if the mesh/region is within its radius

    Light := TLightObject(LightTreeHash.Lights.Objects[K]);
    If Light.Loc.DistanceFrom(AbsCenter) <= Abs(Light.Radius) + DistToCenter Then
    Begin
      // Get a list of all nearby regions (regions that intersect the light volume)

      NearRegions := TStringList(LightTreeHash.RegionLists.Get(K));

      // For each polygon in this mesh/region, see if there are any solid polygons in the way

      If PolygonList <> Nil
       Then PolyCount := High(PolygonList^) + 1
       Else PolyCount := Polygons.Count;

      // Preload the polygon normals and centers lists

      If High(MatchingNormals) < 0 Then
      Begin
        SetLength(MatchingNormals,PolyCount);
        For I := 0 To PolyCount - 1 Do MatchingNormals[I] := False;
      End;
      If PolyNormals.Count = 0 Then
      Begin
        For I := 0 To PolyCount - 1 Do PolyNormals.AddObject('',Nil);
      End;
      If PolyCenters.Count = 0 Then
      Begin
        For I := 0 To PolyCount - 1 Do PolyCenters.AddObject('',Nil);
      End;

      For I := 0 To PolyCount - 1 Do
      Begin
        If PolygonList <> Nil
         Then P := TPolygon(Polygons.Objects[PolygonList^[I]])
         Else P := TPolygon(Polygons.Objects[I]);

//        P := TPolygon(Mesh.Polygons.Objects[Polygons[I]]);
//        If Not P.HasColor Then
//        Begin

          // Find the center of the polygon, using the average of its vertices (really its center of gravity)

          If PolyCenters.Objects[I] = Nil Then
          Begin
            PolyCenter := P.GetCenter(Self);
            PolyCenters.Objects[I] := PolyCenter;
          End
          Else PolyCenter := T3DPoint(PolyCenters.Objects[I]);

          // Only proceed if the polygon faces the light source

          If PolyNormals.Objects[I] = Nil Then
          Begin
            PolyNormal := P.GetNormal(Self);
            PolyNormals.Objects[I] := PolyNormal;
          End
          Else PolyNormal := T3DPoint(PolyNormals.Objects[I]);
          Ray.Copy(Light.Loc);
          Ray.Subtract(PolyCenter);
          Dot := Ray.Dot(PolyNormal);
          If (Dot > 0){ Or Not MatchingNormals[I]} Then
          Begin
            MatchingNormals[I] := True;
            
            // Check each of the polygon's vertices

            For J := 0 To High(P.Vertices) Do
            Begin
              L := P.Vertices[J];
              PointNormal.Copy(T3DPoint(Normals.Objects[L])); // Assumes the point normals are absolute, not relative
              If Not PointNormal.Equals(PolyNormal) Then MatchingNormals[I] := False;
              Vertex.Copy(T3DPoint(Vertices.Objects[L]));
              MakeAbsolute(Vertex);

              // We're going to check to see if any polygons are in the way, but we're really interested in
              // whether the light can illuminate the polygon, not merely its vertex (which is at the polygon's edge).
              // I've found that roundoff error can cause polygons to be lit when they're actually obscured, so
              // we'll choose a point that's a short distance from the vertex as our target point instead.

              ToCenter.Copy(PolyCenter);
              ToCenter.Subtract(Vertex);
              ToCenter.Multiply(0.05); // Move towards the polygon's center by 5 percent of the distance
              Vertex.Add(ToCenter);

              // Only check if the polygon vertex is within range of the light source

              If Vertex.DistanceFrom(Light.Loc) <= Abs(Light.Radius) Then
              Begin
                // Draw a ray from the light source to the vertex we're going to test and make sure that the polygon
                // we're testing doesn't face away from the light source

                Ray.Copy(Light.Loc);
                Ray.Subtract(Vertex);
                Dot := Ray.Dot(PointNormal);

                // Only check if the light is pointing toward the polygon

                If Dot > 0 Then
                Begin
                  // Check all regions that are near enough to possibly be in the way of the light ray
                  // for obstructing polygons

                  RegionIndex := 0;
                  Found       := False;
                  LightColor  := Light.Color;
                  While (RegionIndex < NearRegions.Count) And Not Found Do
                  Begin
                    Region    := TRegion(NearRegions.Objects[RegionIndex]);
                    PolyIndex := 0;
                    While (PolyIndex <= High(Region.Polygons)) And Not Found Do
                    Begin
                      P1 := TPolygon(Region.Mesh.Polygons.Objects[Region.Polygons[PolyIndex]]);
                      If (P1.TextureState <> tsTransparent) And (P1 <> P) Then
                      Begin
                        If (P1.Solid Or Not P1.HasSolid) And (High(P1.Vertices) = 2) Then
                        Begin
                          If P1.IntersectsSegment(Light.Loc,Vertex,Region.Mesh,Intersect) Then
  {
                          If LineFacet(Light.Loc,Vertex,
                                       T3DPoint(MeshVerts.Objects[P1.Vertices[0]]),
                                       T3DPoint(MeshVerts.Objects[P1.Vertices[1]]),
                                       T3DPoint(MeshVerts.Objects[P1.Vertices[2]]),Intersect) Then
  }
                          Begin
                            If Not Intersect.Equals(Vertex) Then
                            Begin
                              If (P1.TextureState = tsSemiTransparent) And (P1.TextureRef <> Nil) Then
                              Begin
                                // Semitransparent polys affect the light color but let the light continue

                                CR := TRGBA(LightColor).R - (255 - TRGBA(P1.TextureRef.AvgColor).R);
                                CG := TRGBA(LightColor).G - (255 - TRGBA(P1.TextureRef.AvgColor).G);
                                CB := TRGBA(LightColor).B - (255 - TRGBA(P1.TextureRef.AvgColor).B);
                                If P1.HasColor Then
                                Begin
                                  // Just look at the first color since this is expensive to do

                                  CR := TRGBA(LightColor).R - (255 - TRGBA(P1.Colors[0]).R);
                                  CG := TRGBA(LightColor).G - (255 - TRGBA(P1.Colors[0]).G);
                                  CB := TRGBA(LightColor).B - (255 - TRGBA(P1.Colors[0]).B);
                                End;
                                If CR < 0 Then CR := 0;
                                If CG < 0 Then CG := 0;
                                If CB < 0 Then CB := 0;
                                TRGBA(LightColor).R := CR;
                                TRGBA(LightColor).G := CG;
                                TRGBA(LightColor).B := CB;
                              End
                              Else Found := True;
                            End;
                          End;
                        End;
                      End;
                      Inc(PolyIndex);
                    End; // While
                    Inc(RegionIndex);
                  End; // While

                  // If no polygons were found that block the light then illuminate the polygon

                  If Not Found Then
                  Begin
                    // Light intensity is a function of 1/r^2

                    Intensity := 1 / Sqr((Ray.GetLength * (4 / Abs(Light.Radius))) + 1);

                    // Make the dot product (which represents the angle of incidence with the
                    // polygon) nonlinear.  We're doing this for two reasons: 1) We can only shade
                    // at vertices, which proabably isn't the closest point of the polygon, and
                    // 2) rougher surfaces will scatter light more.

                    Intensity := Sqrt(Intensity);

                    // Premultiply intensity by the dot product for speed

                    Intensity := Intensity * Dot;

                    // Change the polygon's color

                    If High(P.Colors) < High(P.Vertices) Then SetLength(P.Colors,High(P.Vertices) + 1);
                    If P.HasColor Then
                    Begin
                      CR := TRGBA(P.Colors[J]).R;
                      CG := TRGBA(P.Colors[J]).G;
                      CB := TRGBA(P.Colors[J]).B;
                    End
                    Else
                    Begin
                      CR := 0;
                      CG := 0;
                      CB := 0;
                    End;
                    Inc(CR,Round(TRGBA(LightColor).R * Intensity));
                    Inc(CG,Round(TRGBA(LightColor).G * Intensity));
                    Inc(CB,Round(TRGBA(LightColor).B * Intensity));
                    If CR > 255 Then CR := 255;
                    If CG > 255 Then CG := 255;
                    If CB > 255 Then CB := 255;
                    TRGBA(P.Colors[J]).R := CR;
                    TRGBA(P.Colors[J]).G := CG;
                    TRGBA(P.Colors[J]).B := CB;
                    TRGBA(P.Colors[J]).A := 255;
                    P.HasColor := True;
                  End;
                End;
              End;
            End; // For J
          End;
//          PolyCenter.Free;
//        End;
      End; // For I
      SetLength(MatchingNormals,0);
    End;
  End; // For K

  // Cleanup

  For I := 0 To PolyNormals.Count - 1 Do
  Begin
    If PolyNormals.Objects[I] <> Nil Then PolyNormals.Objects[I].Free;
  End; // For I
  For I := 0 To PolyCenters.Count - 1 Do
  Begin
    If PolyCenters.Objects[I] <> Nil Then PolyCenters.Objects[I].Free;
  End; // For I
  Intersect.Free;
  PolyNormals.Free;
  PolyCenters.Free;
  Vertex.Free;
  AbsCenter.Free;
  ToCenter.Free;
  PointNormal.Free;
  Ray.Free;
End; // TMeshObject.ShadePolygonsByLights

Procedure TMeshObject.AddCopyOfPolygon(P: TPolygon; M: TMeshObject);
Var
  P1   : TPolygon;
  I    : Integer;
  V,V1 : T3DPoint;

Begin
  If P <> Nil Then
  Begin
    P1 := TPolygon.Create(P);
    Polygons.AddObject('',P1);
    V := T3DPoint.Create;
    For I := 0 To High(P1.Vertices) Do
    Begin
      P1.Vertices[I] := Vertices.Count;
      V.Copy(T3DPoint(M.Vertices.Objects[P.Vertices[I]]));
      M.MakeAbsolute(V);
      V1             := GetRelativeLocation(V);
      Vertices.AddObject('',V1);
      Normals.AddObject('',T3DPoint.Create(T3DPoint(M.Normals.Objects[P.Vertices[I]])));
    End; // For I
    V.Free;
  End;
End; // TMeshObject.AddCopyOfPolygon

Function TMeshObject.MakeCopy: TZoneObject;
Begin
  Result := TMeshObject.Create(Self);
End; // TMeshObject.MakeCopy

Procedure TMeshObject.BlendNormals;
Var
  I,J      : Integer;
  V        : T3DPoint;
  Normal   : T3DPoint;
  IDs      : TStringList;
  St       : String;
  List     : TStringList;

Begin
  ClearNormals;
  CalcNormals;
  If Normals.Count = Vertices.Count Then
  Begin
    IDs        := TStringList.Create;
    IDs.Sorted := True;
    Normal     := T3DPoint.Create;
    For I := 0 To Vertices.Count - 1 Do
    Begin
      If frmStatus.Visible Then frmStatus.SetPosition(I / Vertices.Count);
      V  := T3DPoint(Vertices.Objects[I]);
      St := V.GetID;
      J  := IDs.IndexOf(St);
      If J < 0 Then
      Begin
        List := TStringList.Create;
        IDs.AddObject(St,List);
      End
      Else List := TStringList(IDs.Objects[J]);
      List.AddObject('',Pointer(I));
    End; // For I
    For I := 0 To IDs.Count - 1 Do
    Begin
      If frmStatus.Visible Then frmStatus.SetPosition(I / IDs.Count);
      List := TStringList(IDs.Objects[I]);
      Normal.Copy(0,0,0);
      For J := 0 To List.Count - 1 Do Normal.Add(T3DPoint(Normals.Objects[Integer(List.Objects[J])]));
      Normal.Normalize;
      For J := 0 To List.Count - 1 Do T3DPoint(Normals.Objects[Integer(List.Objects[J])]).Copy(Normal);
    End; // For I
    Normal.Free;
    For I := 0 To IDs.Count - 1 Do IDs.Objects[I].Free;
    IDs.Free;
  End;
End; // TMeshObject.BlendNormals

Function TMeshObject.LoadFromFile(Var F: System.Text): Boolean;
Var
  Valid   : Boolean;
  Done    : Boolean;
  Line    : String;
  S       : String; // Mirrors Line, but always uppercase
  Command : String; // Always uppercase
  Cmd     : String; // Mirrors Command, but preserves case

  Procedure ParseTriangle;
  Var
    Tokens          : TTokenArray;
    X1,Y1,Z1        : Single;
    X2,Y2,Z2        : Single;
    X3,Y3,Z3        : Single;
    NX1,NY1,NZ1     : Single;
    NX2,NY2,NZ2     : Single;
    NX3,NY3,NZ3     : Single;
    N1,N2,N3        : T3DPoint;
    I,J             : Integer;
    Transparent     : Boolean;
    SemiTransparent : Boolean;
    Solid           : Boolean;
    Tag             : Integer;
    Angle           : Integer;
    Masked          : Boolean;
    P               : TPolygon;
    HasColor        : Boolean;
    HasTag          : Boolean;
    HasAngle        : Boolean;
    HasMasked       : Boolean;
    HasNormal       : Boolean;
    Colors          : TIntegerArray;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) In [9..30] Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],X1) And
                         GetSingleValue(Tokens[1],Y1) And
                         GetSingleValue(Tokens[2],Z1) And
                         GetSingleValue(Tokens[3],X2) And
                         GetSingleValue(Tokens[4],Y2) And
                         GetSingleValue(Tokens[5],Z2) And
                         GetSingleValue(Tokens[6],X3) And
                         GetSingleValue(Tokens[7],Y3) And
                         GetSingleValue(Tokens[8],Z3);
      If High(Tokens) >= 10 Then Valid := Valid And GetBooleanValue(Tokens[10],Transparent)     Else Transparent     := False;
      If High(Tokens) >= 11 Then Valid := Valid And GetBooleanValue(Tokens[11],SemiTransparent) Else SemiTransparent := False;
      If High(Tokens) >= 12 Then Valid := Valid And GetBooleanValue(Tokens[12],Solid)           Else Solid           := True;
      If High(Tokens) >= 13 Then Valid := Valid And GetIntValues(Tokens[13],Colors);
      If High(Tokens) >= 14 Then Valid := Valid And GetBooleanValue(Tokens[14],HasColor)        Else HasColor        := (High(Tokens) >= 13);
      If High(Tokens) >= 15 Then Valid := Valid And GetIntValue(Tokens[15],Tag);
      If High(Tokens) >= 16 Then Valid := Valid And GetBooleanValue(Tokens[16],HasTag)          Else HasTag          := (High(Tokens) >= 15);
      If High(Tokens) >= 17 Then Valid := Valid And GetIntValue(Tokens[17],Angle);
      If High(Tokens) >= 18 Then Valid := Valid And GetBooleanValue(Tokens[18],HasAngle)        Else HasAngle        := (High(Tokens) >= 17);
      If High(Tokens) >= 19 Then Valid := Valid And GetBooleanValue(Tokens[19],Masked);
      If High(Tokens) >= 20 Then Valid := Valid And GetBooleanValue(Tokens[20],HasMasked)       Else HasMasked       := (High(Tokens) >= 19);
      If High(Tokens) >= 21 Then Valid := Valid And GetBooleanValue(Tokens[21],HasNormal)       Else HasNormal       := False;
      If HasNormal Then
      Begin
        If High(Tokens) >= 30 Then
        Begin
          Valid := Valid And GetSingleValue(Tokens[22],NX1) And
                             GetSingleValue(Tokens[23],NY1) And
                             GetSingleValue(Tokens[24],NZ1) And
                             GetSingleValue(Tokens[25],NX2) And
                             GetSingleValue(Tokens[26],NY2) And
                             GetSingleValue(Tokens[27],NZ2) And
                             GetSingleValue(Tokens[28],NX3) And
                             GetSingleValue(Tokens[29],NY3) And
                             GetSingleValue(Tokens[30],NZ3);
        End
        Else Valid := False;
      End;
      I := Vertices.Count;
      Vertices.AddObject('',T3DPoint.Create(X1,Y1,Z1));
      Vertices.AddObject('',T3DPoint.Create(X2,Y2,Z2));
      Vertices.AddObject('',T3DPoint.Create(X3,Y3,Z3));

      If HasNormal Then
      Begin
        N1 := T3DPoint.Create(NX1,NY1,NZ1);
        N2 := T3DPoint.Create(NX2,NY2,NZ2);
        N3 := T3DPoint.Create(NX3,NY3,NZ3);
      End
      Else
      Begin
        N1 := T3DPoint.Create(X1,Y1,Z1);
        N2 := T3DPoint.Create(X2,Y2,Z2);
        N3 := T3DPoint.Create(X3,Y3,Z3);
        N1.Subtract(N2);
        N3.Subtract(N2);
        N1.Cross(N3);
        N2.Copy(N1);
        N3.Copy(N1);
      End;
      N1.Normalize;
      N2.Normalize;
      N3.Normalize;
      Normals.AddObject('',N1);
      Normals.AddObject('',N2);
      Normals.AddObject('',N3);

      P := TPolygon.Create([I,I + 1,I + 2],CompressTextureList(Tokens[9]));
      Polygons.AddObject('',P);
           If Transparent     Then P.TextureState := tsTransparent
      Else If SemiTransparent Then P.TextureState := tsSemiTransparent;
      If High(Tokens) >= 12 Then
      Begin
        P.HasSolid := True;
        P.Solid    := Solid;
      End;
      If High(Tokens) >= 13 Then
      Begin
        SetLength(P.Colors,3);
        For J := 0 To 2 Do P.Colors[J] := TColor(Colors[Min(J,High(Colors))]);
      End;
      P.HasColor := HasColor;
      If High(Tokens) >= 15 Then P.Tag := Tag;
      P.HasTag := HasTag;
      If High(Tokens) >= 17 Then P.Angle := Angle;
      P.HasAngle := HasAngle;
      If High(Tokens) >= 19 Then P.Masked := Masked;
      P.HasMasked := HasMasked;
      If Not P.HasAngle Then P.CalculateAngle(Self);
      SetLength(Colors,0);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseTriangle

  Procedure ParseTriangleTex;
  Var
    Tokens          : TTokenArray;
    X1,Y1,Z1        : Single;
    X2,Y2,Z2        : Single;
    X3,Y3,Z3        : Single;
    NX1,NY1,NZ1     : Single;
    NX2,NY2,NZ2     : Single;
    NX3,NY3,NZ3     : Single;
    N1,N2,N3        : T3DPoint;
    TX1,TZ1         : Single;
    TX2,TZ2         : Single;
    TX3,TZ3         : Single;
    I,J             : Integer;
    P               : TPolygon;
    Transparent     : Boolean;
    SemiTransparent : Boolean;
    Solid           : Boolean;
    Tag             : Integer;
    Angle           : Integer;
    Masked          : Boolean;
    HasColor        : Boolean;
    HasTag          : Boolean;
    HasAngle        : Boolean;
    HasMasked       : Boolean;
    HasNormal       : Boolean;
    Colors          : TIntegerArray;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) In [15..36] Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],X1)   And
                         GetSingleValue(Tokens[1],Y1)   And
                         GetSingleValue(Tokens[2],Z1)   And
                         GetSingleValue(Tokens[3],X2)   And
                         GetSingleValue(Tokens[4],Y2)   And
                         GetSingleValue(Tokens[5],Z2)   And
                         GetSingleValue(Tokens[6],X3)   And
                         GetSingleValue(Tokens[7],Y3)   And
                         GetSingleValue(Tokens[8],Z3)   And
                         GetSingleValue(Tokens[9],TX1)  And
                         GetSingleValue(Tokens[10],TZ1) And
                         GetSingleValue(Tokens[11],TX2) And
                         GetSingleValue(Tokens[12],TZ2) And
                         GetSingleValue(Tokens[13],TX3) And
                         GetSingleValue(Tokens[14],TZ3);
      If High(Tokens) >= 16 Then Valid := Valid And GetBooleanValue(Tokens[16],Transparent)     Else Transparent     := False;
      If High(Tokens) >= 17 Then Valid := Valid And GetBooleanValue(Tokens[17],SemiTransparent) Else SemiTransparent := False;
      If High(Tokens) >= 18 Then Valid := Valid And GetBooleanValue(Tokens[18],Solid)           Else Solid           := True;
      If High(Tokens) >= 19 Then Valid := Valid And GetIntValues(Tokens[19],Colors);
      If High(Tokens) >= 20 Then Valid := Valid And GetBooleanValue(Tokens[20],HasColor)        Else HasColor        := (High(Tokens) >= 19);
      If High(Tokens) >= 21 Then Valid := Valid And GetIntValue(Tokens[21],Tag);
      If High(Tokens) >= 22 Then Valid := Valid And GetBooleanValue(Tokens[22],HasTag)          Else HasTag          := (High(Tokens) >= 21);
      If High(Tokens) >= 23 Then Valid := Valid And GetIntValue(Tokens[23],Angle);
      If High(Tokens) >= 24 Then Valid := Valid And GetBooleanValue(Tokens[24],HasAngle)        Else HasAngle        := (High(Tokens) >= 23);
      If High(Tokens) >= 25 Then Valid := Valid And GetBooleanValue(Tokens[25],Masked);
      If High(Tokens) >= 26 Then Valid := Valid And GetBooleanValue(Tokens[26],HasMasked)       Else HasMasked       := (High(Tokens) >= 25);
      If High(Tokens) >= 27 Then Valid := Valid And GetBooleanValue(Tokens[27],HasNormal)       Else HasNormal       := False;
      If HasNormal Then
      Begin
        If High(Tokens) >= 36 Then
        Begin
          Valid := Valid And GetSingleValue(Tokens[28],NX1) And
                             GetSingleValue(Tokens[29],NY1) And
                             GetSingleValue(Tokens[30],NZ1) And
                             GetSingleValue(Tokens[31],NX2) And
                             GetSingleValue(Tokens[32],NY2) And
                             GetSingleValue(Tokens[33],NZ2) And
                             GetSingleValue(Tokens[34],NX3) And
                             GetSingleValue(Tokens[35],NY3) And
                             GetSingleValue(Tokens[36],NZ3);
        End
        Else Valid := False;
      End;
      I := Vertices.Count;
      Vertices.AddObject('',T3DPoint.Create(X1,Y1,Z1));
      Vertices.AddObject('',T3DPoint.Create(X2,Y2,Z2));
      Vertices.AddObject('',T3DPoint.Create(X3,Y3,Z3));

      If HasNormal Then
      Begin
        N1 := T3DPoint.Create(NX1,NY1,NZ1);
        N2 := T3DPoint.Create(NX2,NY2,NZ2);
        N3 := T3DPoint.Create(NX3,NY3,NZ3);
      End
      Else
      Begin
        N1 := T3DPoint.Create(X1,Y1,Z1);
        N2 := T3DPoint.Create(X2,Y2,Z2);
        N3 := T3DPoint.Create(X3,Y3,Z3);
        N1.Subtract(N2);
        N3.Subtract(N2);
        N1.Cross(N3);
        N2.Copy(N1);
        N3.Copy(N1);
      End;
      N1.Normalize;
      N2.Normalize;
      N3.Normalize;
      Normals.AddObject('',N1);
      Normals.AddObject('',N2);
      Normals.AddObject('',N3);

      P := TPolygon.Create([I,I + 1,I + 2],CompressTextureList(Tokens[15]));
      Polygons.AddObject('',P);
      P.HasTexCoords := True;
      SetLength(P.TX,3);
      SetLength(P.TZ,3);
      P.TX[0] := TX1;
      P.TZ[0] := TZ1;
      P.TX[1] := TX2;
      P.TZ[1] := TZ2;
      P.TX[2] := TX3;
      P.TZ[2] := TZ3;
           If Transparent     Then P.TextureState := tsTransparent
      Else If SemiTransparent Then P.TextureState := tsSemiTransparent;
      If High(Tokens) >= 18 Then
      Begin
        P.HasSolid := True;
        P.Solid    := Solid;
      End;
      If High(Tokens) >= 19 Then
      Begin
        SetLength(P.Colors,3);
        For J := 0 To 2 Do P.Colors[J] := TColor(Colors[Min(J,High(Colors))]);
      End;
      P.HasColor := HasColor;
      If High(Tokens) >= 21 Then P.Tag := Tag;
      P.HasTag := HasTag;
      If High(Tokens) >= 23 Then P.Angle := Angle;
      P.HasAngle := HasAngle;
      If High(Tokens) >= 25 Then P.Masked := Masked;
      P.HasMasked := HasMasked;
      If Not P.HasAngle Then P.CalculateAngle(Self);
      SetLength(Colors,0);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseTriangleTex

  Procedure ParseRectangle;
  Var
    Tokens          : TTokenArray;
    X1,Y1,Z1        : Single;
    X2,Y2,Z2        : Single;
    X3,Y3,Z3        : Single;
    X4,Y4,Z4        : Single;
    NX1,NY1,NZ1     : Single;
    NX2,NY2,NZ2     : Single;
    NX3,NY3,NZ3     : Single;
    NX4,NY4,NZ4     : Single;
    N1,N2,N3,N4     : T3DPoint;
    I,J             : Integer;
    Transparent     : Boolean;
    SemiTransparent : Boolean;
    Solid           : Boolean;
    Tag             : Integer;
    Angle           : Integer;
    Masked          : Boolean;
    P               : TPolygon;
    HasColor        : Boolean;
    HasTag          : Boolean;
    HasAngle        : Boolean;
    HasMasked       : Boolean;
    HasNormal       : Boolean;
    Colors          : TIntegerArray;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) In [12..36] Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],X1)  And
                         GetSingleValue(Tokens[1],Y1)  And
                         GetSingleValue(Tokens[2],Z1)  And
                         GetSingleValue(Tokens[3],X2)  And
                         GetSingleValue(Tokens[4],Y2)  And
                         GetSingleValue(Tokens[5],Z2)  And
                         GetSingleValue(Tokens[6],X3)  And
                         GetSingleValue(Tokens[7],Y3)  And
                         GetSingleValue(Tokens[8],Z3)  And
                         GetSingleValue(Tokens[9],X4)  And
                         GetSingleValue(Tokens[10],Y4) And
                         GetSingleValue(Tokens[11],Z4);
      If High(Tokens) >= 13 Then Valid := Valid And GetBooleanValue(Tokens[13],Transparent)     Else Transparent     := False;
      If High(Tokens) >= 14 Then Valid := Valid And GetBooleanValue(Tokens[14],SemiTransparent) Else SemiTransparent := False;
      If High(Tokens) >= 15 Then Valid := Valid And GetBooleanValue(Tokens[15],Solid)           Else Solid           := True;
      If High(Tokens) >= 16 Then Valid := Valid And GetIntValues(Tokens[16],Colors);
      If High(Tokens) >= 17 Then Valid := Valid And GetBooleanValue(Tokens[17],HasColor)        Else HasColor        := (High(Tokens) >= 16);
      If High(Tokens) >= 18 Then Valid := Valid And GetIntValue(Tokens[18],Tag);
      If High(Tokens) >= 19 Then Valid := Valid And GetBooleanValue(Tokens[19],HasTag)          Else HasTag          := (High(Tokens) >= 18);
      If High(Tokens) >= 20 Then Valid := Valid And GetIntValue(Tokens[20],Angle);
      If High(Tokens) >= 21 Then Valid := Valid And GetBooleanValue(Tokens[21],HasAngle)        Else HasAngle        := (High(Tokens) >= 20);
      If High(Tokens) >= 22 Then Valid := Valid And GetBooleanValue(Tokens[22],Masked);
      If High(Tokens) >= 23 Then Valid := Valid And GetBooleanValue(Tokens[23],HasMasked)       Else HasMasked       := (High(Tokens) >= 22);
      If High(Tokens) >= 24 Then Valid := Valid And GetBooleanValue(Tokens[24],HasNormal)       Else HasNormal       := False;
      If HasNormal Then
      Begin
        If High(Tokens) >= 36 Then
        Begin
          Valid := Valid And GetSingleValue(Tokens[25],NX1) And
                             GetSingleValue(Tokens[26],NY1) And
                             GetSingleValue(Tokens[27],NZ1) And
                             GetSingleValue(Tokens[28],NX2) And
                             GetSingleValue(Tokens[29],NY2) And
                             GetSingleValue(Tokens[30],NZ2) And
                             GetSingleValue(Tokens[31],NX3) And
                             GetSingleValue(Tokens[32],NY3) And
                             GetSingleValue(Tokens[33],NZ3) And
                             GetSingleValue(Tokens[34],NX4) And
                             GetSingleValue(Tokens[35],NY4) And
                             GetSingleValue(Tokens[36],NZ4);
        End
        Else Valid := False;
      End;
      I := Vertices.Count;
      Vertices.AddObject('',T3DPoint.Create(X1,Y1,Z1));
      Vertices.AddObject('',T3DPoint.Create(X2,Y2,Z2));
      Vertices.AddObject('',T3DPoint.Create(X3,Y3,Z3));
      Vertices.AddObject('',T3DPoint.Create(X4,Y4,Z4));

      If HasNormal Then
      Begin
        N1 := T3DPoint.Create(NX1,NY1,NZ1);
        N2 := T3DPoint.Create(NX2,NY2,NZ2);
        N3 := T3DPoint.Create(NX3,NY3,NZ3);
        N4 := T3DPoint.Create(NX4,NY4,NZ4);
      End
      Else
      Begin
        N1 := T3DPoint.Create(X1,Y1,Z1);
        N2 := T3DPoint.Create(X2,Y2,Z2);
        N3 := T3DPoint.Create(X3,Y3,Z3);
        N1.Subtract(N2);
        N3.Subtract(N2);
        N1.Cross(N3);
        N2.Copy(N1);
        N3.Copy(N1);
        N4 := T3DPoint.Create(N1);
      End;
      N1.Normalize;
      N2.Normalize;
      N3.Normalize;
      N4.Normalize;
      Normals.AddObject('',N1);
      Normals.AddObject('',N2);
      Normals.AddObject('',N3);
      Normals.AddObject('',N4);

      P := TPolygon.Create([I,I + 1,I + 2,I + 3],CompressTextureList(Tokens[12]));
      Polygons.AddObject('',P);
           If Transparent     Then P.TextureState := tsTransparent
      Else If SemiTransparent Then P.TextureState := tsSemiTransparent;
      If High(Tokens) >= 15 Then
      Begin
        P.HasSolid := True;
        P.Solid    := Solid;
      End;
      If High(Tokens) >= 16 Then
      Begin
        SetLength(P.Colors,4);
        For J := 0 To 3 Do P.Colors[J] := TColor(Colors[Min(J,High(Colors))]);
      End;
      P.HasColor := HasColor;
      If High(Tokens) >= 18 Then P.Tag := Tag;
      P.HasTag := HasTag;
      If High(Tokens) >= 20 Then P.Angle := Angle;
      P.HasAngle := HasAngle;
      If High(Tokens) >= 22 Then P.Masked := Masked;
      P.HasMasked := HasMasked;
      If Not P.HasAngle Then P.CalculateAngle(Self);
      SetLength(Colors,0);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseRectangle

  Procedure ParseRectangleTex;
  Var
    Tokens          : TTokenArray;
    X1,Y1,Z1        : Single;
    X2,Y2,Z2        : Single;
    X3,Y3,Z3        : Single;
    X4,Y4,Z4        : Single;
    NX1,NY1,NZ1     : Single;
    NX2,NY2,NZ2     : Single;
    NX3,NY3,NZ3     : Single;
    NX4,NY4,NZ4     : Single;
    N1,N2,N3,N4     : T3DPoint;
    TX1,TX2         : Single;
    TX3,TX4         : Single;
    TZ1,TZ2         : Single;
    TZ3,TZ4         : Single;
    I,J             : Integer;
    P               : TPolygon;
    Transparent     : Boolean;
    SemiTransparent : Boolean;
    Solid           : Boolean;
    Tag             : Integer;
    Angle           : Integer;
    Masked          : Boolean;
    HasColor        : Boolean;
    HasTag          : Boolean;
    HasAngle        : Boolean;
    HasMasked       : Boolean;
    HasNormal       : Boolean;
    Colors          : TIntegerArray;

  Begin
    ConvertEuroNumbersToUS(S);
    GetTokens(',',S,Tokens);
    If High(Tokens) In [20..44] Then
    Begin
      Valid := Valid And GetSingleValue(Tokens[0],X1)   And
                         GetSingleValue(Tokens[1],Y1)   And
                         GetSingleValue(Tokens[2],Z1)   And
                         GetSingleValue(Tokens[3],X2)   And
                         GetSingleValue(Tokens[4],Y2)   And
                         GetSingleValue(Tokens[5],Z2)   And
                         GetSingleValue(Tokens[6],X3)   And
                         GetSingleValue(Tokens[7],Y3)   And
                         GetSingleValue(Tokens[8],Z3)   And
                         GetSingleValue(Tokens[9],X4)   And
                         GetSingleValue(Tokens[10],Y4)  And
                         GetSingleValue(Tokens[11],Z4)  And
                         GetSingleValue(Tokens[12],TX1) And
                         GetSingleValue(Tokens[13],TZ1) And
                         GetSingleValue(Tokens[14],TX2) And
                         GetSingleValue(Tokens[15],TZ2) And
                         GetSingleValue(Tokens[16],TX3) And
                         GetSingleValue(Tokens[17],TZ3) And
                         GetSingleValue(Tokens[18],TX4) And
                         GetSingleValue(Tokens[19],TZ4);
      If High(Tokens) >= 21 Then Valid := Valid And GetBooleanValue(Tokens[21],Transparent)     Else Transparent     := False;
      If High(Tokens) >= 22 Then Valid := Valid And GetBooleanValue(Tokens[22],SemiTransparent) Else SemiTransparent := False;
      If High(Tokens) >= 23 Then Valid := Valid And GetBooleanValue(Tokens[23],Solid)           Else Solid           := True;
      If High(Tokens) >= 24 Then Valid := Valid And GetIntValues(Tokens[24],Colors);
      If High(Tokens) >= 25 Then Valid := Valid And GetBooleanValue(Tokens[25],HasColor)        Else HasColor        := (High(Tokens) >= 24);
      If High(Tokens) >= 26 Then Valid := Valid And GetIntValue(Tokens[26],Tag);
      If High(Tokens) >= 27 Then Valid := Valid And GetBooleanValue(Tokens[27],HasTag)          Else HasTag          := (High(Tokens) >= 26);
      If High(Tokens) >= 28 Then Valid := Valid And GetIntValue(Tokens[28],Angle);
      If High(Tokens) >= 29 Then Valid := Valid And GetBooleanValue(Tokens[29],HasAngle)        Else HasAngle        := (High(Tokens) >= 28);
      If High(Tokens) >= 30 Then Valid := Valid And GetBooleanValue(Tokens[30],Masked);
      If High(Tokens) >= 31 Then Valid := Valid And GetBooleanValue(Tokens[31],HasMasked)       Else HasMasked       := (High(Tokens) >= 30);
      If High(Tokens) >= 32 Then Valid := Valid And GetBooleanValue(Tokens[32],HasNormal)       Else HasNormal       := False;
      If HasNormal Then
      Begin
        If High(Tokens) >= 44 Then
        Begin
          Valid := Valid And GetSingleValue(Tokens[33],NX1) And
                             GetSingleValue(Tokens[34],NY1) And
                             GetSingleValue(Tokens[35],NZ1) And
                             GetSingleValue(Tokens[36],NX2) And
                             GetSingleValue(Tokens[37],NY2) And
                             GetSingleValue(Tokens[38],NZ2) And
                             GetSingleValue(Tokens[39],NX3) And
                             GetSingleValue(Tokens[40],NY3) And
                             GetSingleValue(Tokens[41],NZ3) And
                             GetSingleValue(Tokens[42],NX4) And
                             GetSingleValue(Tokens[43],NY4) And
                             GetSingleValue(Tokens[44],NZ4);
        End
        Else Valid := False;
      End;
      I := Vertices.Count;
      Vertices.AddObject('',T3DPoint.Create(X1,Y1,Z1));
      Vertices.AddObject('',T3DPoint.Create(X2,Y2,Z2));
      Vertices.AddObject('',T3DPoint.Create(X3,Y3,Z3));
      Vertices.AddObject('',T3DPoint.Create(X4,Y4,Z4));

      If HasNormal Then
      Begin
        N1 := T3DPoint.Create(NX1,NY1,NZ1);
        N2 := T3DPoint.Create(NX2,NY2,NZ2);
        N3 := T3DPoint.Create(NX3,NY3,NZ3);
        N4 := T3DPoint.Create(NX4,NY4,NZ4);
      End
      Else
      Begin
        N1 := T3DPoint.Create(X1,Y1,Z1);
        N2 := T3DPoint.Create(X2,Y2,Z2);
        N3 := T3DPoint.Create(X3,Y3,Z3);
        N1.Subtract(N2);
        N3.Subtract(N2);
        N1.Cross(N3);
        N2.Copy(N1);
        N3.Copy(N1);
        N4 := T3DPoint.Create(N1);
      End;
      N1.Normalize;
      N2.Normalize;
      N3.Normalize;
      N4.Normalize;
      Normals.AddObject('',N1);
      Normals.AddObject('',N2);
      Normals.AddObject('',N3);
      Normals.AddObject('',N4);

      P := TPolygon.Create([I,I + 1,I + 2,I + 3],CompressTextureList(Tokens[20]));
      Polygons.AddObject('',P);
      P.HasTexCoords := True;
      SetLength(P.TX,4);
      SetLength(P.TZ,4);
      P.TX[0] := TX1;
      P.TZ[0] := TZ1;
      P.TX[1] := TX2;
      P.TZ[1] := TZ2;
      P.TX[2] := TX3;
      P.TZ[2] := TZ3;
      P.TX[3] := TX4;
      P.TZ[3] := TZ4;
           If Transparent     Then P.TextureState := tsTransparent
      Else If SemiTransparent Then P.TextureState := tsSemiTransparent;
      If High(Tokens) >= 23 Then
      Begin
        P.HasSolid := True;
        P.Solid    := Solid;
      End;
      If High(Tokens) >= 24 Then
      Begin
        SetLength(P.Colors,4);
        For J := 0 To 3 Do P.Colors[J] := TColor(Colors[Min(J,High(Colors))]);
      End;
      P.HasColor := HasColor;
      If High(Tokens) >= 26 Then P.Tag := Tag;
      P.HasTag := HasTag;
      If High(Tokens) >= 28 Then P.Angle := Angle;
      P.HasAngle := HasAngle;
      If High(Tokens) >= 30 Then P.Masked := Masked;
      P.HasMasked := HasMasked;
      If Not P.HasAngle Then P.CalculateAngle(Self);
      SetLength(Colors,0);
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseRectangleTex

  Procedure Parse3DSFileName;
  Var
    I,J  : Integer;
    ZO   : TZoneObject;
    MO   : TMeshObject;
    List : TStringList;

  Begin
    // There is only one token -- the name of the .3DS file to import.  We want to cache any
    // imported objects so we don't waste time importing the same object over and over.

    If FZone.Import3DSCache.IndexOf(S) < 0 Then
    Begin
      If FileExists(S) Then
      Begin
        ZO := FZone.ImportObjectFrom3DSFile(S);
        If ZO <> Nil Then FZone.Import3DSCache.AddObject(S,ZO);
      End;
    End;
    I := FZone.Import3DSCache.IndexOf(S);
    If I >= 0 Then
    Begin
      ZO   := TZoneObject(FZone.Import3DSCache.Objects[I]);
      List := TStringList.Create;
      ZO.AddToPolygonList(List,True,True,True,True);
      For I := 0 To List.Count - 1 Do
      Begin
        MO := TMeshObject(List.Objects[I]);

        // We have to have the source mesh have the same location, rotation, and size as the destination for
        // the copy to proceed properly

        MO.Loc.Copy(Loc);
        MO.Rotate.Copy(Rotate);
        MO.Size.Copy(Size);
        For J := 0 To MO.Polygons.Count - 1 Do AddCopyOfPolygon(TPolygon(MO.Polygons.Objects[J]),MO); // This might take a while...
      End; // For I
      For I := 0 To List.Count - 1 Do List.Objects[I].Free;
      List.Free;
    End;
  End; // Parse3DSFileName

Begin
  Valid := Inherited LoadFromFile(F);
  Done  := False;
  While Valid And (Not Done) And Not Eof(F) Do
  Begin
    ReadLn(F,Line);
    Line := Trim(Line);
    S    := UpperCase(Line);
    If S <> '' Then
    Begin
      Command := GetToken(' ',S);
      Cmd     := GetToken(' ',Line);
           If Command = 'TRIANGLE'          Then ParseTriangle
      Else If Command = 'TRIANGLETEX'       Then ParseTriangleTex
      Else If Command = 'RECTANGLE'         Then ParseRectangle
      Else If Command = 'RECTANGLETEX'      Then ParseRectangleTex
      Else If Command = '3DSFILENAME'       Then Parse3DSFileName
      Else If Command = 'END'               Then Done := True
      Else Valid := False;
    End;
  End; // While
  Result := Valid;
End; // TMeshObject.LoadFromFile

Procedure TMeshObject.SaveToFile(Var F: System.Text; Indent: Integer);
Var
  I           : Integer;
  P1,P2,P3,P4 : T3DPoint;
  P           : TPolygon;
  St,St1      : String;
  DecSep      : Char;

Begin
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  ConvertToTriangles; // Important
  St1 := GetIndent(Indent);
  WriteLn(F,St1 + 'Mesh');
  Inherited SaveToFile(F,Indent);
  For I := 0 To Polygons.Count - 1 Do
  Begin
    P := TPolygon(Polygons.Objects[I]);
    If High(P.Vertices) = 3 Then      // Rectangle
    Begin
      P1 := T3DPoint(Vertices.Objects[P.Vertices[0]]);
      P2 := T3DPoint(Vertices.Objects[P.Vertices[1]]);
      P3 := T3DPoint(Vertices.Objects[P.Vertices[2]]);
      P4 := T3DPoint(Vertices.Objects[P.Vertices[3]]);
      If P.HasTexCoords
       Then St := '  RectangleTex ' + FloatToStr(P1.X) + ', ' +
                                      FloatToStr(P1.Y) + ', ' +
                                      FloatToStr(P1.Z) + ', ' +
                                      FloatToStr(P2.X) + ', ' +
                                      FloatToStr(P2.Y) + ', ' +
                                      FloatToStr(P2.Z) + ', ' +
                                      FloatToStr(P3.X) + ', ' +
                                      FloatToStr(P3.Y) + ', ' +
                                      FloatToStr(P3.Z) + ', ' +
                                      FloatToStr(P4.X) + ', ' +
                                      FloatToStr(P4.Y) + ', ' +
                                      FloatToStr(P4.Z) + ', ' +
                                      FloatToStr(P.TX[0]) + ', ' +
                                      FloatToStr(P.TZ[0]) + ', ' +
                                      FloatToStr(P.TX[1]) + ', ' +
                                      FloatToStr(P.TZ[1]) + ', ' +
                                      FloatToStr(P.TX[2]) + ', ' +
                                      FloatToStr(P.TZ[2]) + ', ' +
                                      FloatToStr(P.TX[3]) + ', ' +
                                      FloatToStr(P.TZ[3]) + ', ' +
                                      P.Texture
       Else St := '  Rectangle ' + FloatToStr(P1.X) + ', ' +
                                   FloatToStr(P1.Y) + ', ' +
                                   FloatToStr(P1.Z) + ', ' +
                                   FloatToStr(P2.X) + ', ' +
                                   FloatToStr(P2.Y) + ', ' +
                                   FloatToStr(P2.Z) + ', ' +
                                   FloatToStr(P3.X) + ', ' +
                                   FloatToStr(P3.Y) + ', ' +
                                   FloatToStr(P3.Z) + ', ' +
                                   FloatToStr(P4.X) + ', ' +
                                   FloatToStr(P4.Y) + ', ' +
                                   FloatToStr(P4.Z) + ', ' +
                                   P.Texture;
      Case P.TextureState Of
            tsTransparent: St := St + ', true, false';
        tsSemiTransparent: St := St + ', false, true';
      Else
        St := St + ', false, false';
      End; // Case
      If P.HasSolid  Then St := St + ', ' + BoolToStr(P.Solid,True)                                           Else St := St + ', true';
      If P.HasColor  Then St := St + ', ' + P.GetColorString + ', ' + BoolToStr(P.HasColor,True) Else St := St + ', 0, false';
      If P.HasTag    Then St := St + ', ' + IntToStr(P.Tag) + ', ' + BoolToStr(P.HasTag,True)                 Else St := St + ', 0, false';
      If P.HasAngle  Then St := St + ', ' + IntToStr(P.Angle) + ', ' + BoolToStr(P.HasAngle,True)             Else St := St + ', 0, false';
      If P.HasMasked Then St := St + ', ' + BoolToStr(P.Masked,True) + ', ' + BoolToStr(P.HasMasked,True)     Else St := St + ', false, false';
      If Normals.Count = Vertices.Count Then
      Begin
        P1 := T3DPoint(Normals.Objects[P.Vertices[0]]);
        P2 := T3DPoint(Normals.Objects[P.Vertices[1]]);
        P3 := T3DPoint(Normals.Objects[P.Vertices[2]]);
        P4 := T3DPoint(Normals.Objects[P.Vertices[3]]);
        St := St + ', true, ';
        St := St + FloatToStr(P1.X) + ', ' +
                   FloatToStr(P1.Y) + ', ' +
                   FloatToStr(P1.Z) + ', ' +
                   FloatToStr(P2.X) + ', ' +
                   FloatToStr(P2.Y) + ', ' +
                   FloatToStr(P2.Z) + ', ' +
                   FloatToStr(P3.X) + ', ' +
                   FloatToStr(P3.Y) + ', ' +
                   FloatToStr(P3.Z) + ', ' +
                   FloatToStr(P4.X) + ', ' +
                   FloatToStr(P4.Y) + ', ' +
                   FloatToStr(P4.Z);
      End;
      WriteLn(F,St1 + St);
    End
    Else If High(P.Vertices) = 2 Then   // Triangle
    Begin
      P1 := T3DPoint(Vertices.Objects[P.Vertices[0]]);
      P2 := T3DPoint(Vertices.Objects[P.Vertices[1]]);
      P3 := T3DPoint(Vertices.Objects[P.Vertices[2]]);
      If P.HasTexCoords
       Then St := '  TriangleTex ' + FloatToStr(P1.X) + ', ' +
                                     FloatToStr(P1.Y) + ', ' +
                                     FloatToStr(P1.Z) + ', ' +
                                     FloatToStr(P2.X) + ', ' +
                                     FloatToStr(P2.Y) + ', ' +
                                     FloatToStr(P2.Z) + ', ' +
                                     FloatToStr(P3.X) + ', ' +
                                     FloatToStr(P3.Y) + ', ' +
                                     FloatToStr(P3.Z) + ', ' +
                                     FloatToStr(P.TX[0]) + ', ' +
                                     FloatToStr(P.TZ[0]) + ', ' +
                                     FloatToStr(P.TX[1]) + ', ' +
                                     FloatToStr(P.TZ[1]) + ', ' +
                                     FloatToStr(P.TX[2]) + ', ' +
                                     FloatToStr(P.TZ[2]) + ', ' +
                                     P.Texture
       Else St := '  Triangle ' + FloatToStr(P1.X) + ', ' +
                                  FloatToStr(P1.Y) + ', ' +
                                  FloatToStr(P1.Z) + ', ' +
                                  FloatToStr(P2.X) + ', ' +
                                  FloatToStr(P2.Y) + ', ' +
                                  FloatToStr(P2.Z) + ', ' +
                                  FloatToStr(P3.X) + ', ' +
                                  FloatToStr(P3.Y) + ', ' +
                                  FloatToStr(P3.Z) + ', ' +
                                  P.Texture;
      Case P.TextureState Of
            tsTransparent: St := St + ', true, false';
        tsSemiTransparent: St := St + ', false, true';
      Else
        St := St + ', false, false';
      End; // Case
      If P.HasSolid  Then St := St + ', ' + BoolToStr(P.Solid,True)                                           Else St := St + ', true';
      If P.HasColor  Then St := St + ', ' + P.GetColorString + ', ' + BoolToStr(P.HasColor,True) Else St := St + ', 0, false';
      If P.HasTag    Then St := St + ', ' + IntToStr(P.Tag) + ', ' + BoolToStr(P.HasTag,True)                 Else St := St + ', 0, false';
      If P.HasAngle  Then St := St + ', ' + IntToStr(P.Angle) + ', ' + BoolToStr(P.HasAngle,True)             Else St := St + ', 0, false';
      If P.HasMasked Then St := St + ', ' + BoolToStr(P.Masked,True) + ', ' + BoolToStr(P.HasMasked,True)     Else St := St + ', false, false';
      If Normals.Count = Vertices.Count Then
      Begin
        P1 := T3DPoint(Normals.Objects[P.Vertices[0]]);
        P2 := T3DPoint(Normals.Objects[P.Vertices[1]]);
        P3 := T3DPoint(Normals.Objects[P.Vertices[2]]);
        St := St + ', true, ';
        St := St + FloatToStr(P1.X) + ', ' +
                   FloatToStr(P1.Y) + ', ' +
                   FloatToStr(P1.Z) + ', ' +
                   FloatToStr(P2.X) + ', ' +
                   FloatToStr(P2.Y) + ', ' +
                   FloatToStr(P2.Z) + ', ' +
                   FloatToStr(P3.X) + ', ' +
                   FloatToStr(P3.Y) + ', ' +
                   FloatToStr(P3.Z);
      End;
      WriteLn(F,St1 + St);
    End;
  End; // For I
  WriteLn(F,St1 + 'End');
  DecimalSeparator := DecSep;
End; // TMeshObject.SaveToFile

Procedure TMeshObject.AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean);
Var
  I   : LongInt;
  MO  : TMeshObject;
  P   : TPolygon;
  Pt  : T3DPoint;

  L   : T3DPoint;
  S   : T3DPoint;
  H   : THeading;
  Par : TGroupObject;

Begin
  L := T3DPoint.Create;
  H := THeading.Create;
  S := T3DPoint.Create;

  // Save the parent, position, rotation, and size

  Par := FParent;
  L.Copy(FLoc);
  H.Copy(FHeading);
  S.Copy(FSize);

  // Temporarily transform to absolute coordinates

  ChangeToAbsolute(FParent);

  // Create the mesh

  MO := TMeshObject.Create;
  SetLength(MO.BoneIndices,High(BoneIndices) + 1);
  For I := 0 To High(BoneIndices) Do MO.BoneIndices[I] := BoneIndices[I];
  For I := 0 To Vertices.Count - 1 Do
  Begin
    Pt := T3DPoint.Create(T3DPoint(Vertices.Objects[I]));
    If Transform Then
    Begin
      Pt.X := Pt.X * FSize.X;
      Pt.Y := Pt.Y * FSize.Y;
      Pt.Z := Pt.Z * FSize.Z;
      FHeading.Rotate(Pt);
      Pt.Add(FLoc);
    End;
    MO.Vertices.AddObject('',Pt);
  End; // For I
  For I := 0 To Normals.Count - 1 Do //MO.Normals.AddObject('',T3DPoint.Create(T3DPoint(Normals.Objects[I])));
  Begin
    Pt := T3DPoint.Create(T3DPoint(Normals.Objects[I]));
    If Transform Then
    Begin
      FHeading.Rotate(Pt);
      Pt.Normalize;
    End;
    MO.Normals.AddObject('',Pt);
  End; // For I

  For I := 0 To Polygons.Count - 1 Do MO.Polygons.AddObject('',TPolygon.Create(TPolygon(Polygons.Objects[I])));
  List.AddObject(FName,MO);

  // Restore parent status and relative coordinates

  FParent := Par;
  FLoc.Copy(L);
  FHeading.Copy(H);
  FSize.Copy(S);

  // If this mesh is a ground mesh, get rid of any polygons that we've hidden (for dungeon entrances and such)

  If (FZone <> Nil) And ((FName = meshHeightMapGround) Or (FName = meshHeightMapUnderwater)) Then
  Begin
    I := 0;
    While I < MO.Polygons.Count Do
    Begin
      P := TPolygon(MO.Polygons.Objects[I]);
      If P.HasTag Then
      Begin
        If Not FZone.ElevationGrid.Visible[P.Tag] Then
        Begin
          P.Free;
          MO.Polygons.Delete(I);
        End
        Else Inc(I);
      End
      Else Inc(I);
    End; // While
    MO.RemoveUnusedVertices;
  End;

  // Cleanup

  L.Free;
  H.Free;
  S.Free;
End; // TMeshObject.AddToPolygonList

Function TMeshObject.IndexOfVertex(P: T3DPoint): LongInt;
Var
  I     : LongInt;
  Found : Boolean;

Begin
  I     := 0;
  Found := False;
  While (I < Vertices.Count) And Not Found Do
  Begin
    If T3DPoint(Vertices.Objects[I]).Equals(P) Then Found := True Else Inc(I);
  End; // While
  If Found Then Result := I Else Result := -1;
End; // TMeshObject.IndexOfVertex

Procedure TMeshObject.ConvertToTriangles;
Var
  I,J,K : Integer;
  P     : TPolygon;
  P1    : TPolygon;
  PV    : Integer;

Begin
  // For now at least, assume all polygons are convex

  K := Polygons.Count - 1;
  For I := 0 To K Do
  Begin
    P := TPolygon(Polygons.Objects[I]);

    // If this polygon has more than three vertices, break it up

    PV := High(P.Vertices);
    If PV > 2 Then
    Begin
      For J := 0 To PV - 3 Do
      Begin
        // Create a new polygon

        P1 := TPolygon.CreateBasic(P);
        SetLength(P1.Vertices,3);
        If P1.HasTexCoords Then
        Begin
          SetLength(P1.TX,3);
          SetLength(P1.TZ,3);
        End;
        If P1.HasColor Then
        Begin
          SetLength(P1.Colors,3);
          P1.Colors[0] := P.Colors[0];
          P1.Colors[1] := P.Colors[J + 2];
          P1.Colors[2] := P.Colors[J + 3];
        End;

        // Copy the right vertices and texture coordinates

        P1.Vertices[0] := P.Vertices[0];
        P1.Vertices[1] := P.Vertices[J + 2];
        P1.Vertices[2] := P.Vertices[J + 3];
        If P1.HasTexCoords Then
        Begin
          P1.TX[0] := P.TX[0];
          P1.TX[1] := P.TX[J + 2];
          P1.TX[2] := P.TX[J + 3];
          P1.TZ[0] := P.TZ[0];
          P1.TZ[1] := P.TZ[J + 2];
          P1.TZ[2] := P.TZ[J + 3];
        End;

        // Add the new polygon

        Polygons.AddObject('',P1);
      End; // For J

      // Shorten the original polygon

      SetLength(P.Vertices,3);
      If P.HasTexCoords Then
      Begin
        SetLength(P.TX,3);
        SetLength(P.TZ,3);
      End;
    End;
  End; // For I
End; // TMeshObject.ConvertToTriangles

Procedure TMeshObject.ClearNormals;
Begin
  While Normals.Count > 0 Do
  Begin
    Normals.Objects[Normals.Count - 1].Free;
    Normals.Delete(Normals.Count - 1);
  End; // While
End; // TMeshObject.ClearNormals

Procedure TMeshObject.MatchNormalCountToVertexCount;
Begin
  While Normals.Count > Vertices.Count Do
  Begin
    Normals.Objects[Normals.Count - 1].Free;
    Normals.Delete(Normals.Count - 1);
  End; // While
  While Normals.Count < Vertices.Count Do Normals.AddObject('',T3DPoint.Create);
End; // TMeshObject.MatchNormalCountToVertexCount

Procedure TMeshObject.CalcNormals;
Var
  I,J     : LongInt;
  V,V1,V2 : T3DPoint;
  P       : TPolygon;

Begin
  If Normals.Count <> Vertices.Count Then
  Begin
    MatchNormalCountToVertexCount;
    V  := T3DPoint.Create;
    V1 := T3DPoint.Create;
    V2 := T3DPoint.Create;
    For I := 0 To Polygons.Count - 1 Do
    Begin
      P := TPolygon(Polygons.Objects[I]);
      If High(P.Vertices) >= 2 Then
      Begin
        // Need speed here, so don't use the "as" operator

        V.Copy(T3DPoint(Vertices.Objects[P.Vertices[1]]));
        V1.Copy(T3DPoint(Vertices.Objects[P.Vertices[0]]));
        V2.Copy(T3DPoint(Vertices.Objects[P.Vertices[2]]));
        V1.Subtract(V);
        V2.Subtract(V);
        V1.Cross(V2);
        V1.Normalize;
        For J := 0 To High(P.Vertices) Do T3DPoint(Normals.Objects[P.Vertices[J]]).Copy(V1);
      End;
    End; // For I
    V.Free;
    V1.Free;
    V2.Free;
  End;
End; // TMeshObject.CalcNormals

Procedure TMeshObject.CalcTextureCoords(P: TPolygon);
Var
  MinPt       : T3DPoint;
  MaxPt       : T3DPoint;

Begin
  // If texture coordinates were already supplied then simply copy them, otherwise
  // we'll try to figure out some good ones

  If Not P.HasTexCoords Then
  Begin
    MinPt := T3DPoint.Create;
    MaxPt := T3DPoint.Create;
    GetBounds(MinPt,MaxPt);
    CalcTextureCoords(P,MinPt,MaxPt);
    MinPt.Free;
    MaxPt.Free;
  End;
End; // TMeshObject.CalcTextureCoords

Procedure TMeshObject.CalcTextureCoords(P: TPolygon; MinPt,MaxPt: T3DPoint);
Var
  I           : Integer;
  MinX        : Single;
  MinY        : Single;
  MinZ        : Single;
  MaxX        : Single;
  MaxY        : Single;
  MaxZ        : Single;
  P1          : T3DPoint;
  P2          : T3DPoint;
  P3          : T3DPoint;
  Normal      : T3DPoint;
  N1          : T3DPoint;

Begin
  // If texture coordinates were already supplied then simply copy them, otherwise
  // we'll try to figure out some good ones

  If Not P.HasTexCoords Then
  Begin
    P1 := T3DPoint.Create;
    P2 := T3DPoint.Create;
    P3 := T3DPoint.Create;
    CalcNormals;

    P.HasTexCoords := True;
    SetLength(P.TX,High(P.Vertices) + 1);
    SetLength(P.TZ,High(P.Vertices) + 1);

    // Get the extents of the polygon

    MinX := 9999999;
    MinY := 9999999;
    MinZ := 9999999;
    MaxX := -9999999;
    MaxY := -9999999;
    MaxZ := -9999999;

    // Get the polygon's extents

    For I := 0 To High(P.Vertices) Do
    Begin
      P1.Copy(T3DPoint(Vertices.Objects[P.Vertices[I]]));
      If P1.x < MinX Then MinX := P1.x;
      If P1.y < MinY Then MinY := P1.y;
      If P1.z < MinZ Then MinZ := P1.z;
      If P1.x > MaxX Then MaxX := P1.x;
      If P1.y > MaxY Then MaxY := P1.y;
      If P1.z > MaxZ Then MaxZ := P1.z;
    End; // For I

    P1.Copy(T3DPoint(Vertices.Objects[P.Vertices[0]]));
    P2.Copy(T3DPoint(Vertices.Objects[P.Vertices[1]]));
    P3.Copy(T3DPoint(Vertices.Objects[P.Vertices[2]]));

    // Adjust to the mesh's origin

    P1.Subtract(RegionSize * Trunc(MinPt.X / RegionSize),
                RegionSize * Trunc(MinPt.Y / RegionSize),
                RegionSize * Trunc(MinPt.Z / RegionSize));

    P2.Subtract(RegionSize * Trunc(MinPt.X / RegionSize),
                RegionSize * Trunc(MinPt.Y / RegionSize),
                RegionSize * Trunc(MinPt.Z / RegionSize));

    P3.Subtract(RegionSize * Trunc(MinPt.X / RegionSize),
                RegionSize * Trunc(MinPt.Y / RegionSize),
                RegionSize * Trunc(MinPt.Z / RegionSize));


    Normal := T3DPoint.Create(P1);
    N1     := T3DPoint.Create(P3);
    Normal.Subtract(P2);
    N1.Subtract(P2);
    Normal.Cross(N1);
    Normal.Normalize;
    N1.Free;

    // Here's where the magic is.  It bases the texture coordinates
    // on the direction in which the polygon faces.

    If Abs(Normal.Z) > Abs(Normal.X) Then
    Begin
      If Abs(Normal.Z) > Abs(Normal.Y) Then
      Begin
{
    If Abs(MaxX - MinX) >= Abs(MaxZ - MinZ) Then
    Begin
      If Abs(MaxY - MinY) > Abs(MaxZ - MinZ) Then
      Begin
}
        P1.X := Rescale * P1.X / RegionSize;
        P1.Y := Rescale * P1.Y / RegionSize;
        P2.X := Rescale * P2.X / RegionSize;
        P2.Y := Rescale * P2.Y / RegionSize;
        P3.X := Rescale * P3.X / RegionSize;
        P3.Y := Rescale * P3.Y / RegionSize;
      End
      Else
      Begin
        P1.X := Rescale * P1.X / RegionSize;
        P1.Y := Rescale * P1.Z / RegionSize;
        P2.X := Rescale * P2.X / RegionSize;
        P2.Y := Rescale * P2.Z / RegionSize;
        P3.X := Rescale * P3.X / RegionSize;
        P3.Y := Rescale * P3.Z / RegionSize;
      End;
    End
    Else
    Begin
      If Abs(Normal.Y) > Abs(Normal.X) Then
 //     If Abs(MaxX - MinX) > Abs(MaxY - MinY) Then
      Begin
        P1.X := Rescale * P1.X / RegionSize;
        P1.Y := Rescale * P1.Z / RegionSize;
        P2.X := Rescale * P2.X / RegionSize;
        P2.Y := Rescale * P2.Z / RegionSize;
        P3.X := Rescale * P3.X / RegionSize;
        P3.Y := Rescale * P3.Z / RegionSize;
      End
      Else
      Begin
        P1.X := Rescale * P1.Y / RegionSize;
        P1.Y := Rescale * P1.Z / RegionSize;
        P2.X := Rescale * P2.Y / RegionSize;
        P2.Y := Rescale * P2.Z / RegionSize;
        P3.X := Rescale * P3.Y / RegionSize;
        P3.Y := Rescale * P3.Z / RegionSize;
      End;
    End;

    // Copy the texture and normal information back to the model

    P.TX[0] := P1.X;
    P.TZ[0] := P1.Y;
    P.TX[1] := P2.X;
    P.TZ[1] := P2.Y;
    P.TX[2] := P3.X;
    P.TZ[2] := P3.Y;

    Normal.Free;
    P1.Free;
    P2.Free;
    P3.Free;
  End;
End; // TMeshObject.CalcTextureCoords

Procedure TMeshObject.Coalesce;
Var
  I,J     : LongInt;
  Remap   : Array Of LongInt;
  V       : T3DPoint;
  P       : TPolygon;
  L       : TStringList;
  St      : String;
  Index   : Integer;
  Highest : Integer;
  Normal  : T3DPoint;

Begin
  L               := TStringList.Create;
  L.Sorted        := True;
  L.CaseSensitive := True;

  // Round all vertices to three decimal points

  For J := 0 To Vertices.Count - 1 Do
  Begin
    V   := T3DPoint(Vertices.Objects[J]);
    V.X := Round(V.X * 1000) / 1000;
    V.Y := Round(V.Y * 1000) / 1000;
    V.Z := Round(V.Z * 1000) / 1000;
  End; // For J

  // If we don't have normals yet, we need to create them

  CalcNormals;

  // Copy all vertices, remapping duplicates

  SetLength(Remap,Vertices.Count);
  Highest := -1;
  For J := 0 To Vertices.Count - 1 Do
  Begin
    V      := T3DPoint(Vertices.Objects[J]);
    Normal := T3DPoint(Normals.Objects[J]);
    St     := V.GetID + Normal.GetID;
    Index := L.IndexOf(St);
    If Index < 0 Then
    Begin
      Inc(Highest);
      I := Highest;
      L.AddObject(St,Pointer(I));
    End
    Else I := Integer(L.Objects[Index]);
    Remap[J] := I;
  End; // For J

  // Move the vertex data

  For I := 0 To Vertices.Count - 1 Do
  Begin
    T3DPoint(Vertices.Objects[Remap[I]]).Copy(T3DPoint(Vertices.Objects[I]));
    T3DPoint(Normals.Objects[Remap[I]]).Copy(T3DPoint(Normals.Objects[I]));
  End; // For I

  // Remap the polygons

  For J := 0 To Polygons.Count - 1 Do
  Begin
    P := TPolygon(Polygons.Objects[J]);
    For I := 0 To High(P.Vertices) Do P.Vertices[I] := Remap[P.Vertices[I]];
  End; // For J
  SetLength(Remap,0);
  L.Free;

  // Get rid of remapped vertices

  While Vertices.Count > Highest + 1 Do
  Begin
    Vertices.Objects[Vertices.Count - 1].Free;
    Vertices.Delete(Vertices.Count - 1);
    Normals.Objects[Vertices.Count - 1].Free;
    Normals.Delete(Vertices.Count - 1);
  End; // While
End; // TMeshObject.Coalesce
(*
Procedure TMeshObject.SplitAlong(Axis: TAxis; DivPt: Single);
Var
  V        : T3DPoint;
  I,J,K    : LongInt;
  L        : LongInt;
  R0       : Boolean;
  R,R1,R2  : Boolean;
  P        : TPolygon;
  P1       : TPolygon;
  P2       : TPolygon;
  V1       : T3DPoint;
  V2       : T3DPoint;
  V3       : T3DPoint;
  Found    : Boolean;
  Len0     : LongInt;
  Len1     : LongInt;
  Boundary : Boolean;
  I1       : T3DPoint;
  I2       : T3DPoint;
  New1     : LongInt;
  New2     : LongInt;
  New1TX   : LongInt;
  New1TZ   : LongInt;
  New2TX   : LongInt;
  New2TZ   : LongInt;
  KMod     : LongInt;
  AddedJ   : LongInt;
  AddedK   : LongInt;
  TP       : T3DPoint;
  Dist1    : Double;
  Dist2    : Double;

  Function GetIntersection(I1,I2: LongInt): T3DPoint;
  Var
    P1    : T3DPoint;
    P2    : T3DPoint;
    P3    : T3DPoint;
    Ratio : Double;

  Begin
    P1 := T3DPoint(Vertices.Objects[I1]);  // Within the boundary
    P2 := T3DPoint(Vertices.Objects[I2]);  // Outside the bounddary
    P3 := T3DPoint.Create(P1);
    Case Axis Of
      taX:
      Begin
        P3.X := DivPt;
        If P2.X <> P1.X Then                         // Create the two other coordinates on the same line
        Begin
          Ratio := (P3.X - P1.X) / (P2.X - P1.X);
          P3.Y  := P1.Y + (P2.Y - P1.Y) * Ratio;
          P3.Z  := P1.Z + (P2.Z - P1.Z) * Ratio;
        End;
      End;
      taY:
      Begin
        P3.Y := DivPt;
        If P2.Y <> P1.Y Then                         // Create the two other coordinates on the same line
        Begin
          Ratio := (P3.Y - P1.Y) / (P2.Y - P1.Y);
          P3.X  := P1.X + (P2.X - P1.X) * Ratio;
          P3.Z  := P1.Z + (P2.Z - P1.Z) * Ratio;
        End;
      End;
      taZ:
      Begin
        P3.Z := DivPt;
        If P2.Z <> P1.Z Then                         // Create the two other coordinates on the same line
        Begin
          Ratio := (P3.Z - P1.Z) / (P2.Z - P1.Z);
          P3.X  := P1.X + (P2.X - P1.X) * Ratio;
          P3.Y  := P1.Y + (P2.Y - P1.Y) * Ratio;
        End;
      End;
    End; // Case
    Result := P3;
  End; // GetIntersection

  Procedure CheckPolyRegion(P: TPolygon);
  Var
    I     : Integer;
    Found : Boolean;
    V     : T3DPoint;

  Begin
    I     := 0;
    Found := False;
    While (I <= High(P.Vertices)) And Not Found Do
    Begin
      V := T3DPoint(Vertices.Objects[P.Vertices[I]]);
      Case Axis Of
        taX: Found := (V.X > DivPt);
        taY: Found := (V.Y > DivPt);
        taZ: Found := (V.Z > DivPt);
      End; // Case
      Inc(I);
    End; // While
    If Found Then
    Begin
      Case Axis Of
        taX: Inc(P.RegionX);
        taY: Inc(P.RegionY);
        taZ: Inc(P.RegionZ);
      End; // Case
    End;
  End; // CheckPolyRegion

Begin
  // Break up any polygons that cross the division point (the process requires that
  // we NOT assume that all polygons are triangles

  I := 0;
  While I < Polygons.Count Do
  Begin
    P  := TPolygon(Polygons.Objects[I]);

    // Find out what region the first point is in

    V1 := T3DPoint(Vertices.Objects[P.Vertices[0]]);

    R := False;
    Case Axis Of
      taX: R := (V1.X >= DivPt);
      taY: R := (V1.Y >= DivPt);
      taZ: R := (V1.Z >= DivPt);
    End; // Case

    // Trace around the polygon until we either find a segment that crosses a
    // region boundary or reach the end of the polygon

    // The last segment can't possibly be the first one that crosses a boundary
    // (the polygon has to cross and then cross back)

    J        := 1;
    Boundary := False;
    AddedJ   := 0;
    I1       := Nil;
    New1     := -1;
    New1TX   := -1;
    New1TZ   := -1;
    R1       := R;
    R2       := R;
    While (J <= High(P.Vertices)) And Not Boundary Do
    Begin
      V2 := T3DPoint(Vertices.Objects[P.Vertices[J]]);
      Case Axis Of
        taX: R1 := (V2.X >= DivPt);
        taY: R1 := (V2.Y >= DivPt);
        taZ: R1 := (V2.Z >= DivPt);
      End; // Case
      If R1 <> R Then Boundary := True Else Inc(J);
      If Boundary Then
      Begin
        I1 := GetIntersection(P.Vertices[J - 1],P.Vertices[J]);
        If P.HasTexCoords Then
        Begin
          // Calculate texture coordinates

          TP := T3DPoint.Create(T3DPoint(Vertices.Objects[P.Vertices[J]]));
          TP.Subtract(T3DPoint(Vertices.Objects[P.Vertices[J - 1]]));
          Dist1 := TP.GetLength;
          TP.Copy(I1);
          TP.Subtract(T3DPoint(Vertices.Objects[P.Vertices[J - 1]]));
          Dist2 := TP.GetLength;
          TP.Free;
          If Dist1 <> 0 Then
          Begin
            New1TX := Round(P.TX[J - 1] + (Dist2 / Dist1) * (P.TX[J] - P.TX[J - 1]));
            New1TZ := Round(P.TZ[J - 1] + (Dist2 / Dist1) * (P.TZ[J] - P.TZ[J - 1]));
          End
          Else
          Begin
            New1TX := P.TX[J - 1];
            New1TZ := P.TX[J - 1];
          End;
        End;
        If I1.Equals(V2) Then
        Begin
          I1.Free;
          New1   := P.Vertices[J];
          AddedJ := 1;
          If P.HasTexCoords Then
          Begin
            New1TX := P.TX[J];
            New1TZ := P.TZ[J];
          End;
        End
        Else If I1.Equals(T3DPoint(Vertices.Objects[P.Vertices[J - 1]])) Then
        Begin
          I1.Free;
          New1   := P.Vertices[J - 1];
          AddedJ := 2;
          If P.HasTexCoords Then
          Begin
            New1TX := P.TX[J - 1];
            New1TZ := P.TZ[J - 1];
          End;
        End
        Else New1 := -1; // New1TX and New1TZ have already been set in this case
      End;
    End; // While
    If Boundary Then
    Begin
      // There must be at least two segments that cross the boundary. Search for
      // the last one that crosses the SAME boundary

      K     := High(P.Vertices);
      Found := False;
      V3    := Nil;
      While (K >= J) And Not Found Do
      Begin
        V3  := T3DPoint(Vertices.Objects[P.Vertices[K]]);
        Case Axis Of
          taX: R2 := (V3.X >= DivPt);
          taY: R2 := (V3.Y >= DivPt);
          taZ: R2 := (V3.Z >= DivPt);
        End; // Case
        If R2 <> R Then Found := True Else Dec(K);
      End; // While
      If Found Then
      Begin
        KMod := (K + 1) Mod (High(P.Vertices) + 1);

        // Find the intersection points on both segments with the boundary and
        // add them to the vertex list

        If New1 < 0 Then
        Begin
          Vertices.AddObject('',I1);
          New1 := Vertices.Count - 1;
          If High(FNormalX) >= 0 Then
          Begin
            AddNormal;
            FNormalX[High(FNormalX)] := FNormalX[P.Vertices[J]];
            FNormalY[High(FNormalY)] := FNormalY[P.Vertices[J]];
            FNormalZ[High(FNormalZ)] := FNormalZ[P.Vertices[J]];
          End;
        End;

        AddedK := 0;
        I2 := GetIntersection(P.Vertices[KMod],P.Vertices[K]);
        If P.HasTexCoords Then
        Begin
          // Calculate texture coordinates

          TP := T3DPoint.Create(T3DPoint(Vertices.Objects[P.Vertices[K]]));
          TP.Subtract(T3DPoint(Vertices.Objects[P.Vertices[KMod]]));
          Dist1 := TP.GetLength;
          TP.Copy(I2);
          TP.Subtract(T3DPoint(Vertices.Objects[P.Vertices[KMod]]));
          Dist2 := TP.GetLength;
          TP.Free;
          If Dist1 <> 0 Then
          Begin
            New2TX := Round(P.TX[KMod] + (Dist2 / Dist1) * (P.TX[K] - P.TX[KMod]));
            New2TZ := Round(P.TZ[KMod] + (Dist2 / Dist1) * (P.TZ[K] - P.TZ[KMod]));
          End
          Else
          Begin
            New2TX := P.TX[KMod];
            New2TZ := P.TZ[KMod];
          End;
        End;
        If I2.Equals(V3) Then
        Begin
          I2.Free;
          New2   := P.Vertices[K];
          AddedK := 1;
          If P.HasTexCoords Then
          Begin
            New2TX := P.TX[K];
            New2TZ := P.TZ[K];
          End;
        End
        Else If I2.Equals(T3DPoint(Vertices.Objects[P.Vertices[KMod]])) Then
        Begin
          I2.Free;
          New2   := P.Vertices[KMod];
          AddedK := 2;
          If P.HasTexCoords Then
          Begin
            New2TX := P.TX[KMod];
            New2TZ := P.TZ[KMod];
          End;
        End
        Else
        Begin
          Vertices.AddObject('',I2);
          New2 := Vertices.Count - 1;  // New2TX and New2TZ have already been set in this case
          If High(FNormalX) >= 0 Then
          Begin
            AddNormal;
            FNormalX[High(FNormalX)] := FNormalX[P.Vertices[K]];
            FNormalY[High(FNormalY)] := FNormalY[P.Vertices[K]];
            FNormalZ[High(FNormalZ)] := FNormalZ[P.Vertices[K]];
          End;
        End;

        // Create a new polygon from the points on the other side of the boundary

        If (K > J + 1) Or (AddedJ <> 1) Or (AddedK <> 1) Then
        Begin
          // Base the new polygon on the old one

          P1 := TPolygon.Create(P);

          // Copy the points

          If AddedJ = 1 Then
          Begin
            If AddedK = 1 Then
            Begin
              SetLength(P1.Vertices,K - J + 1);
              If P1.HasTexCoords Then
              Begin
                SetLength(P1.TX,High(P1.Vertices) + 1);
                SetLength(P1.TZ,High(P1.Vertices) + 1);
              End;
              For L := J To K Do
              Begin
                P1.Vertices[L - J] := P.Vertices[L];
                If P1.HasTexCoords Then
                Begin
                  P1.TX[L - J] := P.TX[L];
                  P1.TZ[L - J] := P.TZ[L];
                End;
              End; // For L
            End
            Else
            Begin
              SetLength(P1.Vertices,K - J + 2);
              If P1.HasTexCoords Then
              Begin
                SetLength(P1.TX,High(P1.Vertices) + 1);
                SetLength(P1.TZ,High(P1.Vertices) + 1);
              End;
              For L := J To K Do
              Begin
                P1.Vertices[L - J] := P.Vertices[L];
                If P1.HasTexCoords Then
                Begin
                  P1.TX[L - J] := P.TX[L];
                  P1.TZ[L - J] := P.TZ[L];
                End;
              End; // For L
            End;
          End
          Else
          Begin
            If AddedK = 1 Then
            Begin
              SetLength(P1.Vertices,K - J + 2);
              If P1.HasTexCoords Then
              Begin
                SetLength(P1.TX,High(P1.Vertices) + 1);
                SetLength(P1.TZ,High(P1.Vertices) + 1);
              End;
              For L := J To K Do
              Begin
                P1.Vertices[L - J + 1] := P.Vertices[L];
                If P1.HasTexCoords Then
                Begin
                  P1.TX[L - J + 1] := P.TX[L];
                  P1.TZ[L - J + 1] := P.TZ[L];
                End;
              End; // For L
            End
            Else
            Begin
              SetLength(P1.Vertices,K - J + 3);
              If P1.HasTexCoords Then
              Begin
                SetLength(P1.TX,High(P1.Vertices) + 1);
                SetLength(P1.TZ,High(P1.Vertices) + 1);
              End;
              For L := J To K Do
              Begin
                P1.Vertices[L - J + 1] := P.Vertices[L];
                If P1.HasTexCoords Then
                Begin
                  P1.TX[L - J + 1] := P.TX[L];
                  P1.TZ[L - J + 1] := P.TZ[L];
                End;
              End; // For L
            End;
          End;

          // Set the first and last vertices in the new polygon

          P1.Vertices[0]                 := New1;
          P1.Vertices[High(P1.Vertices)] := New2;
          If P1.HasTexCoords Then
          Begin
            P1.TX[0]           := New1TX;
            P1.TZ[0]           := New1TZ;
            P1.TX[High(P1.TX)] := New2TX;
            P1.TZ[High(P1.TZ)] := New2TZ;
          End;

          // Trim the current polygon down to all points within the boundary

          Len0 := High(P.Vertices) + 1;
          Len1 := (J + 1) + (High(P.Vertices) - K + 1);
          If AddedJ = 2 Then Dec(Len1);
          If AddedK = 2 Then Dec(Len1);

          // It's possible that the new polygon isn't viable, which can happen if the
          // only "valid" segment is on a region boundary

          If (Len1 < 3) Or (High(P1.Vertices) < 2) Then P1.Free
          Else
          Begin
            Polygons.InsertObject(I + 1,'',P1);

            // If we wound up with more vertices than before, increase the size of the arrays

            P2 := TPolygon.Create(P);
            If Len1 > Len0 Then
            Begin
              SetLength(P2.Vertices,Len1);
              If P2.HasTexCoords Then
              Begin
                SetLength(P2.TX,Len1);
                SetLength(P2.TZ,Len1);
              End;
            End;

            // Copy the points

            If AddedJ = 2 Then
            Begin
              If AddedK = 2 Then
              Begin
                For L := K + 2 To Len0 - 1 Do
                Begin
                  P2.Vertices[(J + 1) + L - (K + 2)] := P.Vertices[L];
                  If P2.HasTexCoords Then
                  Begin
                    P2.TX[(J + 1) + L - (K + 2)] := P.TX[L];
                    P2.TZ[(J + 1) + L - (K + 2)] := P.TZ[L];
                  End;
                End; // For L
              End
              Else
              Begin
                For L := K + 1 To Len0 - 1 Do
                Begin
                  P2.Vertices[(J + 1) + L - (K + 1)] := P.Vertices[L];
                  If P2.HasTexCoords Then
                  Begin
                    P2.TX[(J + 1) + L - (K + 1)] := P.TX[L];
                    P2.TZ[(J + 1) + L - (K + 1)] := P.TZ[L];
                  End;
                End; // For L
              End;
            End
            Else
            Begin
              If AddedK = 2 Then
              Begin
                For L := K + 2 To Len0 - 1 Do
                Begin
                  P2.Vertices[(J + 2) + L - (K + 2)] := P.Vertices[L];
                  If P2.HasTexCoords Then
                  Begin
                    P2.TX[(J + 2) + L - (K + 2)] := P.TX[L];
                    P2.TZ[(J + 2) + L - (K + 2)] := P.TZ[L];
                  End;
                End; // For L
              End
              Else
              Begin
                For L := K + 1 To Len0 - 1 Do
                Begin
                  P2.Vertices[(J + 2) + L - (K + 1)] := P.Vertices[L];
                  If P2.HasTexCoords Then
                  Begin
                    P2.TX[(J + 2) + L - (K + 1)] := P.TX[L];
                    P2.TZ[(J + 2) + L - (K + 1)] := P.TZ[L];
                  End;
                End; // For L
              End;
            End;

            // A region was removed, so the vertices just ouside that region will be different

            If AddedJ = 2 Then
            Begin
              P2.Vertices[J - 1] := New1;
              P2.Vertices[J]     := New2;
              If P2.HasTexCoords Then
              Begin
                P2.TX[J - 1] := New1TX;
                P2.TZ[J - 1] := New1TZ;
                P2.TX[J]     := New2TX;
                P2.TZ[J]     := New2TZ;
              End;
            End
            Else
            Begin
              P2.Vertices[J]     := New1;
              P2.Vertices[(J + 1) Mod (High(P2.Vertices) + 1)] := New2;
              If P2.HasTexCoords Then
              Begin
                P2.TX[J]     := New1TX;
                P2.TZ[J]     := New1TZ;
                P2.TX[(J + 1) Mod (High(P2.Vertices) + 1)] := New2TX;
                P2.TZ[(J + 1) Mod (High(P2.Vertices) + 1)] := New2TZ;
              End;
            End;

            // If we wound up with less vertices than before, decrease the size of the arrays

            If Len1 < Len0 Then
            Begin
              SetLength(P2.Vertices,Len1);
              If P2.HasTexCoords Then
              Begin
                SetLength(P2.TX,Len1);
                SetLength(P2.TZ,Len1);
              End;
            End;

            // Copy everything back

            SetLength(P.Vertices,High(P2.Vertices) + 1);
            For J := 0 To High(P2.Vertices) Do P.Vertices[J] := P2.Vertices[J];
            SetLength(P.TX,High(P2.TX) + 1);
            SetLength(P.TZ,High(P2.TZ) + 1);
            For J := 0 To High(P2.TX) Do P.TX[J] := P2.TX[J];
            For J := 0 To High(P2.TZ) Do P.TZ[J] := P2.TZ[J];
            P2.Free;
          End;
        End;
        CheckPolyRegion(P);
        Inc(I);
      End
      Else
      Begin
        // Should never get here but if we do, punt (maybe the segment exactly ends on a boundary)

        CheckPolyRegion(P);
        Inc(I);
      End;
    End
    Else
    Begin
      CheckPolyRegion(P);
      Inc(I);
    End;
  End; // While

  // Don't turn into triangles or anything...
End; // TMeshObject.SplitAlong

Procedure TMeshObject.BreakIntoRegions;
Var
  V        : T3DPoint;
  MinPt    : T3DPoint;
  MaxPt    : T3DPoint;
  I        : LongInt;

Begin
  // Initialize boundary points

  MinPt := T3DPoint.Create(9999999,9999999,9999999);
  MaxPt := T3DPoint.Create(-9999999,-9999999,-9999999);

  // Determine the zone bounds

  For I := 0 To Vertices.Count - 1 Do
  Begin
    V := T3DPoint(Vertices.Objects[I]);
    If V.X < MinPt.X Then MinPt.X := V.X;
    If V.Y < MinPt.Y Then MinPt.Y := V.Y;
    If V.Z < MinPt.Z Then MinPt.Z := V.Z;
    If V.X > MaxPt.X Then MaxPt.X := V.X;
    If V.Y > MaxPt.Y Then MaxPt.Y := V.Y;
    If V.Z > MaxPt.Z Then MaxPt.Z := V.Z;
  End; // For I

  For I := 0 To Trunc((MaxPt.X - MinPt.X) / RegionSize) Do SplitAlong(taX,I * RegionSize + MinPt.X);
  For I := 0 To Trunc((MaxPt.Y - MinPt.Y) / RegionSize) Do SplitAlong(taY,I * RegionSize + MinPt.Y);
  For I := 0 To Trunc((MaxPt.Z - MinPt.Z) / RegionSize) Do SplitAlong(taZ,I * RegionSize + MinPt.Z);

  // Convert all remaining polygons to triangles

  ConvertToTriangles;

  // Get rid of any redundant vertices that we may have created

  // We must *not* call TMeshObject.Coalesce or vertex normals get screwed up
  // (sharing vertices is BAD, BAD, BAD)

//  Coalesce;

  // Cleanup

  MinPt.Free;
  MaxPt.Free;
End; // TMeshObject.BreakIntoRegions
*)

Procedure TMeshObject.RemoveUnusedVertices;
Var
  I,J   : Integer;
  Shift : Array Of Integer;
  Used  : Array Of Boolean;
  P     : TPolygon;

Begin
  SetLength(Shift,Vertices.Count);
  SetLength(Used,Vertices.Count);
  For I := 0 To High(Used) Do Used[I] := False;
  For I := 0 To Polygons.Count - 1 Do
  Begin
    P := TPolygon(Polygons.Objects[I]);
    For J := 0 To High(P.Vertices) Do Used[P.Vertices[J]] := True;
  End; // For I
  J := 0;
  For I := 0 To High(Shift) Do
  Begin
    If Not Used[I] Then Inc(J);
    Shift[I] := J;
  End; // For I

  // Deallocate unused vertices

  I := Vertices.Count - 1;
  While I >= 0 Do
  Begin
    If Not Used[I] Then
    Begin
      Vertices.Objects[I].Free;
      Vertices.Delete(I);
      Normals.Objects[I].Free;
      Normals.Delete(I);
    End;
    Dec(I);
  End; // While

  // Shift normals

  If Normals.Count > 0 Then
  Begin
    For I := 0 To Normals.Count - 1 Do
     If Shift[I] < Normals.Count Then T3DPoint(Normals.Objects[I]).Copy(T3DPoint(Normals.Objects[Shift[I]]));
    MatchNormalCountToVertexCount;
  End;

  // Fixup polygon vertex indices

  For I := 0 To Polygons.Count - 1 Do
  Begin
    P := TPolygon(Polygons.Objects[I]);
    For J := 0 To High(P.Vertices) Do Dec(P.Vertices[J],Shift[P.Vertices[J]]);
  End; // For I
  SetLength(Shift,0);
  SetLength(Used,0);
End; // TMeshObject.RemoveUnusedVertices

Procedure TMeshObject.RemoveAlongPlane(Normal: T3DPoint; Dist: Single; RemoveOutside: Boolean);
// This routine is written specifically for BOUNDING MESHES, meshes that describe the
// bounds of an entire region.
//
// We have to have some fuzziness or roundoff error will give us infinite loops.  This design
// should ensure that roundoff error will still have split polygons exactly mate up.
Const MinDist = 0.001;
Var
  Norm      : T3DPoint;
  I,J,K     : Integer;
  J1,K1     : Integer;
  P         : TPolygon;
  D0,D1,D2  : Single;
  Found     : Boolean;

  VJ,VJ1    : T3DPoint;
  VK,VK1    : T3DPoint;
  I1,I2     : T3DPoint;
  Moving    : Boolean;
  TX1,TZ1   : Single;
  TX2,TZ2   : Single;
  P1        : TPolygon;
  OldPCount : Integer;
  Added     : Boolean;
  C1,C2     : TColor;

  Function CountNodes(Start,Finish,Size: Integer): Integer;
  Var I: Integer;
  Begin
    I := 1;
    While Start <> Finish Do
    Begin
      Inc(I);
      Inc(Start);
      If Start >= Size Then Start := 0;
    End; // While
    Result := I;
  End; // CountNodes

Begin
  If Polygons.Count > 0 Then
  Begin
    Norm      := T3DPoint.Create(Normal);
    I1        := T3DPoint.Create;
    I2        := T3DPoint.Create;

    // Remember how many polygons we start with so we know where any added line segments will start

    OldPCount := Polygons.Count;

    // We are either removing those areas that are inside or outside the area sought

    If Not RemoveOutside Then
    Begin
      Norm.Multiply(-1);
      Dist := -Dist;
    End;

    // Iterate through the polygon list

    I := 0;
    While I < Polygons.Count Do
    Begin
      P      := TPolygon(Polygons.Objects[I]);
      Moving := False;

      // Make sure the polygon has at least three vertices

      If High(P.Vertices) >= 2 Then
      Begin
        // Find the first point that is not on the plane

        D0    := 0;
        J     := 0;
        Found := False;
        While (J <= High(P.Vertices)) And Not Found Do
        Begin
          // Starting with the first point not on the plane, find out which side it's on

          D0 := Norm.Dot(T3DPoint(Vertices.Objects[P.Vertices[J]])) + Dist;
          If Abs(D0) > MinDist
           Then Found := True
           Else Inc(J);
        End; // While

        // If we actually didn't find anything then all points are on the plane, so keep the polygon.

        If Found Then
        Begin
          // Now look for the first point that crosses the plane

          Inc(J);
          Found := False;
          D1    := 0;
          While (J <= High(P.Vertices)) And Not Found Do
          Begin
            D1 := Norm.Dot(T3DPoint(Vertices.Objects[P.Vertices[J]])) + Dist;
            If (Abs(D1) > MinDist) And (Sign(D1) <> Sign(D0)) Then Found := True Else Inc(J);
          End; // While

          // We want to split or keep the polygon under two circumstances:
          // 1. It crosses the plane
          // 2. It lies entirely on the "inside" side of the plane

          K := 0;
          If Found Then
          Begin
            // We've definitely crossed the plane; see where it crosses back or even touches the plane

            If J < High(P.Vertices) Then
            Begin
              K     := J + 1;
              Found := False;
              While (K <= High(P.Vertices)) And Not Found Do
              Begin
                D2 := Norm.Dot(T3DPoint(Vertices.Objects[P.Vertices[K]])) + Dist;
                If (Abs(D2) < MinDist) Or (Sign(D2) <> Sign(D1)) Then Found := True Else Inc(K);
              End; // While
              If Not Found Then K := High(P.Vertices) Else Dec(K);
            End
            Else K := J;

            // J and K are on one side of the plane.  Get the points on the other side.

            J1 := J - 1;
            K1 := (K + 1) Mod (High(P.Vertices) + 1);

            // Find the two intersection points on the plane

            VJ  := T3DPoint(Vertices.Objects[P.Vertices[J]]);
            VJ1 := T3DPoint(Vertices.Objects[P.Vertices[J1]]);
            P.GetIntersection(Self,VJ,VJ1,J,J1,Norm.Dot(VJ) + Dist,Norm.Dot(VJ1) + Dist,I1,TX1,TZ1,C1);

            VK  := T3DPoint(Vertices.Objects[P.Vertices[K]]);
            VK1 := T3DPoint(Vertices.Objects[P.Vertices[K1]]);
            P.GetIntersection(Self,VK,VK1,K,K1,Norm.Dot(VK) + Dist,Norm.Dot(VK1) + Dist,I2,TX2,TZ2,C2);

            // If the intersection points are different, insert them into the polygon and
            // set them as endpoints for the new polygon

            If Not (I1.Equals(VJ) Or I1.Equals(VJ1)) Then
            Begin
              P.InsertVertex(Self,I1,J,TX1,TZ1,C1);
              Inc(K);
              K1 := (K + 1) Mod (High(P.Vertices) + 1);
            End
            Else If I1.Equals(VJ1) Then J := J1;

            If Not (I2.Equals(VK) Or I2.Equals(VK1)) Then
            Begin
              P.InsertVertex(Self,I2,K1,TX2,TZ2,C2);
              If K1 < J Then J := (J + 1) Mod (High(P.Vertices) + 1);
              K := K1;
            End
            Else If I2.Equals(VK1) Then K := K1;

            // J to K (inclusive) point to a polygon that will be split off.  The region it will
            // go in depends on D1 (if D1 > 0 then we keep it)

            If D1 < 0 Then
            Begin
              J1 := J;
              J  := K;
              K  := J1;
            End;
            Moving := True;
          End
          Else If D0 > 0 Then
          Begin
            // The polygon doesn't cross the plane, but it's on the "inside", so
            // keep the entire polygon

            J      := 0;
            K      := High(P.Vertices);
            Moving := True;
          End;
          If Moving Then
          Begin
            // Split the polgyon.  Points J to K (inclusive) go to the polygon we're keeping.

            If CountNodes(J,K,High(P.Vertices) + 1) < High(P.Vertices) + 1 Then
            Begin
              // Make a new polygon and replace the original one with this one

              P1 := TPolygon.Create(P);
              J1 := 0; // Counter
              K1 := (K + 1) Mod (High(P.Vertices) + 1);
              Repeat
                P1.Vertices[J1] := P.Vertices[J];
                If P1.HasTexCoords Then
                Begin
                  P1.TX[J1] := P.TX[J];
                  P1.TZ[J1] := P.TZ[J];
                End;
                If P1.HasColor Then P1.Colors[J1] := P.Colors[J];
                Inc(J1);
                J := (J + 1) Mod (High(P.Vertices) + 1);
              Until J = K1;
              SetLength(P1.Vertices,J1);
              If P1.HasTexCoords Then
              Begin
                SetLength(P1.TX,J1);
                SetLength(P1.TZ,J1);
              End;
              If P1.HasColor Then SetLength(P1.Colors,J1);
              Polygons.Objects[I] := P1;

              // Now change the original one to a line segment and add it to the end

              SetLength(P.Vertices,2);
              P.Vertices[0] := P1.Vertices[0];
              P.Vertices[1] := P1.Vertices[High(P1.Vertices)];
              Polygons.AddObject('',P);

              // The new polygon by definition has to be entirely inside the region we want, so move on to the next one

              Inc(I);
            End
            Else Inc(I);
          End
          Else
          Begin
            // Get rid of the polygon and adjust the point at which line segments begin

            Polygons.Objects[I].Free;
            Polygons.Delete(I);
            Dec(OldPCount);
          End;
        End
        Else Inc(I);
      End
      Else Inc(I);
    End; // While
    I1.Free;
    I2.Free;
    Norm.Free;

    // Make vertices shared

    Coalesce;

    // We might have unused vertices.  Get rid of the unused ones.

    RemoveUnusedVertices;

    // Assuming the bounding solid was convex, we should be able to coalesce the line segments into one polygon

    I := OldPCount;
    While I < Polygons.Count - 1 Do
    Begin
      Added := False;
      P     := TPolygon(Polygons.Objects[I]);
      J     := I + 1;
      While J < Polygons.Count Do
      Begin
        // The "polygons" we're dealing with here all start out as line segments (they only have
        // two vertices).  The idea here is to concatenate the segments as much as we can, and the
        // result should be one polygon (or more than one, but that seems extremely unlikely--it
        // should in fact be impossible as long as the initial bounding volume was convex).

        P1 := TPolygon(Polygons.Objects[J]);
        If P.Vertices[0] = P1.Vertices[0] Then
        Begin
          SetLength(P.Vertices,High(P.Vertices) + 2);
          For J1 := High(P.Vertices) DownTo 1 Do P.Vertices[J1] := P.Vertices[J1 - 1];
          P.Vertices[0] := P1.Vertices[1];
          P1.Free;
          Polygons.Delete(J);
          Added := True;
        End
        Else If P.Vertices[0] = P1.Vertices[1] Then
        Begin
          SetLength(P.Vertices,High(P.Vertices) + 2);
          For J1 := High(P.Vertices) DownTo 1 Do P.Vertices[J1] := P.Vertices[J1 - 1];
          P.Vertices[0] := P1.Vertices[0];
          P1.Free;
          Polygons.Delete(J);
          Added := True;
        End
        Else If P.Vertices[High(P.Vertices)] = P1.Vertices[0] Then
        Begin
          SetLength(P.Vertices,High(P.Vertices) + 2);
          P.Vertices[High(P.Vertices)] := P1.Vertices[1];
          P1.Free;
          Polygons.Delete(J);
          Added := True;
        End
        Else If P.Vertices[High(P.Vertices)] = P1.Vertices[1] Then
        Begin
          SetLength(P.Vertices,High(P.Vertices) + 2);
          P.Vertices[High(P.Vertices)] := P1.Vertices[0];
          P1.Free;
          Polygons.Delete(J);
          Added := True;
        End
        Else Inc(J);
      End; // While

      // If we did any concatenations, we need to reprocess this segment

      If Not Added Then Inc(I);
    End; // While
  End;
End; // TMeshObject.RemoveAlongPlane

Procedure TMeshObject.GetCenterOfVolumeAndMaxDistance(Center: T3DPoint; Var Dist: Single);
// This works as long as we don't have unused vertices
Var
  V,V1      : T3DPoint;
  I         : Integer;
  D         : Single;
  XMin,XMax : Single;
  YMin,YMax : Single;
  ZMin,ZMax : Single;

Begin
  // Calculate the center of the volume

  XMin := 0;
  YMin := 0;
  ZMin := 0;
  XMax := 0;
  YMax := 0;
  ZMax := 0;
  For I := 0 To Vertices.Count - 1 Do
  Begin
    V := T3DPoint(Vertices.Objects[I]);
    If I = 0 Then
    Begin
      XMin := V.X;
      YMin := V.Y;
      ZMin := V.Z;
      XMax := XMin;
      YMax := YMin;
      ZMax := ZMin;
    End
    Else
    Begin
      If V.X < XMin Then XMin := V.X;
      If V.Y < YMin Then YMin := V.Y;
      If V.Z < ZMin Then ZMin := V.Z;
      If V.X > XMax Then XMax := V.X;
      If V.Y > YMax Then YMax := V.Y;
      If V.Z > ZMax Then ZMax := V.Z;
    End;
  End; // For I
  Center.X := (XMin + XMax) / 2;
  Center.Y := (YMin + YMax) / 2;
  Center.Z := (ZMin + ZMax) / 2;

  // Get the longest distance from the center

  Dist := 0;
  V1   := T3DPoint.Create;
  For I := 0 To Vertices.Count - 1 Do
  Begin
    V1.Copy(T3DPoint(Vertices.Objects[I]));
    V1.Subtract(Center);
    D := V1.GetLength;
    If D > Dist Then Dist := D;
  End; // For I
  V1.Free;
End; // TMeshObject.GetCenterOfVolumeAndMaxDistance

Procedure TMeshObject.GetBounds(MinPt,MaxPt: T3DPoint);
// This works as long as we don't have unused vertices
Var
  V         : T3DPoint;
  I         : Integer;
  XMin,XMax : Single;
  YMin,YMax : Single;
  ZMin,ZMax : Single;

Begin
  // Calculate the center of the volume

  XMin := 0;
  YMin := 0;
  ZMin := 0;
  XMax := 0;
  YMax := 0;
  ZMax := 0;
  If Vertices.Count > 0 Then
  Begin
    V := T3DPoint(Vertices.Objects[0]);
    XMin := V.X;
    YMin := V.Y;
    ZMin := V.Z;
    XMax := XMin;
    YMax := YMin;
    ZMax := ZMin;
  End;
  For I := 1 To Vertices.Count - 1 Do
  Begin
    V := T3DPoint(Vertices.Objects[I]);
    If V.X < XMin Then XMin := V.X;
    If V.Y < YMin Then YMin := V.Y;
    If V.Z < ZMin Then ZMin := V.Z;
    If V.X > XMax Then XMax := V.X;
    If V.Y > YMax Then YMax := V.Y;
    If V.Z > ZMax Then ZMax := V.Z;
  End; // For I

  // Sanity check to prevent overflows

  If XMin < -100000000 Then XMin := -100000000;
  If YMin < -100000000 Then YMin := -100000000;
  If ZMin < -100000000 Then ZMin := -100000000;
  If XMax >  100000000 Then XMax :=  100000000;
  If YMax >  100000000 Then YMax :=  100000000;
  If ZMax >  100000000 Then ZMax :=  100000000;

  MinPt.Copy(XMin,YMin,ZMin);
  MaxPt.Copy(XMax,YMax,ZMax);
End; // TMeshObject.GetBounds

// ------------------------------
// TMeshLibraryObjectReference
// ------------------------------

Constructor TMeshLibraryObjectReference.Create;
Begin
  Inherited Create;
  FGravity  := True;
  FGroup    := Nil;
  FSQLParms := TStringList.Create;
End; // TMeshLibraryObjectReference.Create

Constructor TMeshLibraryObjectReference.Create(GO: TGroupObject);
Begin
  Inherited Create;
  FGroup    := GO;
  FGravity  := True;
  FSQLParms := TStringList.Create;
End; // TMeshLibraryObjectReference.Create

Constructor TMeshLibraryObjectReference.Create(ML: TMeshLibraryObjectReference);
Var I: Integer;
Begin
  Inherited Create(ML);
  FGroup    := ML.FGroup;
  FGravity  := ML.FGravity;
  FSQLParms := TStringList.Create;
  For I := 0 To ML.FSQLParms.Count - 1 Do FSQLParms.Add(ML.FSQLParms.Strings[I]);
End; // TMeshLibraryObjectReference.Create

Destructor TMeshLibraryObjectReference.Destroy;
Begin
  FSQLParms.Free;
  Inherited;
End; // TMeshLibraryObjectReference.Destroy

Function TMeshLibraryObjectReference.MakeCopy: TZoneObject;
Begin
  Result := TMeshLibraryObjectReference.Create(Self);
End; // TMeshLibraryObjectReference.MakeCopy

Procedure TMeshLibraryObjectReference.SetGroup(GO: TGroupObject);
Begin
  FGroup := GO;
End; // TMeshLibraryObjectReference.SetMesh

Procedure TMeshLibraryObjectReference.SetInsertMesh(B: Boolean);
Begin
  FInsertMesh := B;
End; // TMeshLibraryObjectReference.SetInsertMesh

Procedure TMeshLibraryObjectReference.SetSQLRef(Ref: TSQLRef);
Begin
  FSQLRef := Ref;
End; // TMeshLibraryObjectReference.SetSQLRef

Procedure TMeshLibraryObjectReference.SetGravity(B: Boolean);
Begin
  FGravity := B;
  frmMain.RefreshObject(Self,True,True,True);
  Application.ProcessMessages;
End; // TMeshLibraryObjectReference.SetGravity

Function TMeshLibraryObjectReference.GetSQLParmCount: Integer;
Begin
  Result := FSQLParms.Count;
End; // TMeshLibraryObjectReference.GetSQLParmCount

Function TMeshLibraryObjectReference.GetSQLParm(Index: Integer): String;
Begin
  If (Index >= 0) And (Index < FSQLParms.Count)
   Then Result := FSQLParms.Strings[Index]
   Else Result := '';
End; // TMeshLibraryObjectReference.GetSQLParm

Procedure TMeshLibraryObjectReference.AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean);
Var
  I            : Integer;
  MO           : TMeshObject;
  L            : TStringList;
  MaxPt        : T3DPoint;
  MinPt        : T3DPoint;
  V            : T3DPoint;
  Offset       : Single;
  Height       : Single;
  MinX         : Single;
  MinY         : Single;
  MaxX         : Single;
  MaxY         : Single;
  MinZ         : Single;
  MinZ1        : Single;
  XLoc         : Single;
  YLoc         : Single;
  AFLoc        : T3DPoint;
  AFHeading    : THeading;
  AFSize       : T3DPoint;
  AParent      : TGroupObject;
  Loc0         : T3DPoint;
  Found        : Boolean;
  HighestPoint : T3DPoint;
  HPV1         : T3DPoint;
  HPV2         : T3DPoint;
  HPV3         : T3DPoint;
  Z            : Single;

Begin
  L  := TStringList.Create;
  FGroup.AddToPolygonList(L,ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform);
  MO := FZone.CoalescePolygonList(L);
//  MO := TMeshObject(L.Objects[0]);

//  // Get rid of any extra mesh objects, since they are probably placeholders for light sources

  For I := 0{1} To L.Count - 1 Do L.Objects[I].Free;
  L.Free;

  // We need the absolute position of this object for the purpose of dealing with the gravity setting

  If FParent <> Nil Then
  Begin
    AFLoc     := T3DPoint.Create(FLoc);
    AFHeading := THeading.Create(FHeading);
    AFSize    := T3DPoint.Create(FSize);
    AParent   := FParent;

    ChangeToAbsolute(FParent);

    MO.FLoc.Copy(FLoc);
    MO.FHeading.Copy(FHeading);
    MO.FSize.Copy(FSize);

    FLoc.Copy(AFLoc);
    FHeading.Copy(AFHeading);
    FSize.Copy(AFSize);
    FParent := AParent;

    AFLoc.Free;
    AFHeading.Free;
    AFSize.Free;
  End
  Else
  Begin
    MO.FLoc.Copy(FLoc);
    MO.FHeading.Copy(FHeading);
    MO.FSize.Copy(FSize);
  End;

  MO.FName   := FName;
  MO.FZone   := FZone;

  // If gravity is switched on, then if the mesh lies above or below the ground, move it until there
  // is NO gap between its bottom and the ground.  This allows things like trees to move with the
  // ground when the ground's elevation is changed in the editor.

  If Gravity Then
  Begin
    MinPt        := T3DPoint.Create;
    MaxPt        := T3DPoint.Create;
    MO.GetBounds(MinPt,MaxPt);
    HighestPoint := T3DPoint.Create;
    HPV1         := T3DPoint.Create;
    HPV2         := T3DPoint.Create;
    HPV3         := T3DPoint.Create;

    // Save the position of the mesh object (it's in absolute coordinates)

    Loc0 := T3DPoint.Create(MO.Loc);

    // Find all points that are at the bottom of the mesh and find out how far the object
    // needs to move up or down to meet the ground

    MinZ   := MinPt.Z;
    MinZ1  := MinPt.Z + MO.FLoc.Z;
    Offset := 9999999;
    MinX   := FZone.ElevationGrid.MinX;
    MinY   := FZone.ElevationGrid.MinY;
    MaxX   := FZone.ElevationGrid.MaxX;
    MaxY   := FZone.ElevationGrid.MaxY;
    XLoc   := MO.FLoc.X;
    YLoc   := MO.FLoc.Y;
    Found  := False;
         
    For I := 0 To MO.Vertices.Count - 1 Do
    Begin
      V := T3DPoint(MO.Vertices.Objects[I]);
      If V.Z = MinZ Then
      Begin
        If frmMain.GetHighestZ(V.X + XLoc,V.Y + YLoc,Z) Then
        Begin
          Height := Z;
          If Height < Offset Then Offset := Height;
          Found := True;
        End;
      End;
    End; // For I

    // Only make the object rise or fall if there is ground above or below it

    If Found Then
    Begin
      // Move the object up or down to meet the ground

      // Change the original location by the amount that we moved the mesh

      FLoc.Z    := FLoc.Z    + (Offset - MO.FLoc.Z);
      MO.FLoc.Z := MO.FLoc.Z + (Offset - MO.FLoc.Z);
    End;

    // Cleanup

    Loc0.Free;
    MinPt.Free;
    MaxPt.Free;
    HighestPoint.Free;
    HPV1.Free;
    HPV2.Free;
    HPV3.Free;
  End;

  MO.AddToPolygonList(List,ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform);
  MO.Free;
End; // TMeshLibraryObjectReference.AddToPolygonList

Function TMeshLibraryObjectReference.LoadFromFile(Var F: System.Text): Boolean;
Var
  Valid   : Boolean;
  Done    : Boolean;
  Line    : String;
  S       : String; // Mirrors Line, but always uppercase
  Command : String; // Always uppercase
  Cmd     : String; // Mirrors Command, but preserves case

  Procedure ParseMesh;
  Var I: Integer;
  Begin
    I := MeshLibrary.IndexOf(Line);
    If I >= 0
     Then FGroup := TGroupObject(MeshLibrary.Objects[I])
     Else Valid := False;
  End; // ParseMesh

  Procedure ParseGravity;
  Var
    G      : Boolean;
    Tokens : TTokenArray;
    
  Begin
    GetTokens(',',S,Tokens);
    If High(Tokens) = 0 Then
    Begin
      Valid := Valid And GetBooleanValue(Tokens[0],G);
      If Valid Then FGravity := G;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseGravity

  Procedure ParseInsertMesh;
  Var
    G      : Boolean;
    Tokens : TTokenArray;

  Begin
    GetTokens(',',S,Tokens);
    If High(Tokens) = 0 Then
    Begin
      Valid := Valid And GetBooleanValue(Tokens[0],G);
      If Valid Then FInsertMesh := G;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseInsertMesh

  Procedure ParseDoorRef;
  Var
    G      : Boolean;
    Tokens : TTokenArray;

  Begin
    GetTokens(',',S,Tokens);
    If High(Tokens) = 0 Then
    Begin
      Valid := Valid And GetBooleanValue(Tokens[0],G);
      If Valid Then
      Begin
        If G Then FSQLRef := srDoors;
      End;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseDoorRef

  Procedure ParseObjectRef;
  Var
    G      : Boolean;
    Tokens : TTokenArray;

  Begin
    GetTokens(',',S,Tokens);
    If High(Tokens) = 0 Then
    Begin
      Valid := Valid And GetBooleanValue(Tokens[0],G);
      If Valid Then
      Begin
        If G Then FSQLRef := srObjects;
      End;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseObjectRef

  Procedure ParseSQLRef;
  Var Tokens: TTokenArray;
  Begin
    GetTokens(',',S,Tokens);
    If High(Tokens) = 0 Then
    Begin
      Tokens[0] := LowerCase(Tokens[0]);
           If Tokens[0] = 'none'    Then FSQLRef := srNone
      Else If Tokens[0] = 'doors'   Then FSQLRef := srDoors
      Else If Tokens[0] = 'objects' Then FSQLRef := srObjects
      Else Valid := False;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseSQLRef

Begin
  Valid := Inherited LoadFromFile(F);
  Done  := False;
  While Valid And (Not Done) And Not Eof(F) Do
  Begin
    ReadLn(F,Line);
    Line := Trim(Line);
    S    := UpperCase(Line);
    If S <> '' Then
    Begin
      Command := GetToken(' ',S);
      Cmd     := GetToken(' ',Line);
           If Command = 'MESHOBJECT' Then ParseMesh
      Else If Command = 'GRAVITY'    Then ParseGravity
      Else If Command = 'INSERTMESH' Then ParseInsertMesh
      Else If Command = 'DOORREF'    Then ParseDoorRef      // Needed for backward compatibility
      Else If Command = 'OBJECTREF'  Then ParseObjectRef
      Else If Command = 'SQLREF'     Then ParseSQLRef
      Else If Command = 'END'        Then Done := True
      Else Valid := False;
    End;
  End; // While
  Result := Valid;
End; // TMeshLibraryObjectReference.LoadFromFile

Procedure TMeshLibraryObjectReference.SaveToFile(Var F: System.Text; Indent: Integer);
Var
  I      : Integer;
  St     : String;
  DecSep : Char;

Begin
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  St := GetIndent(Indent);
  WriteLn(F,St + 'MeshLibraryReference');
  Inherited SaveToFile(F,Indent);
  I := MeshLibrary.IndexOfObject(FGroup);
  WriteLn(F,St + '  MeshObject ' + MeshLibrary.Strings[I]);
  WriteLn(F,St + '  Gravity '    + BoolToStr(Gravity,True));
  WriteLn(F,St + '  InsertMesh ' + BoolToStr(InsertMesh,True));
  Case FSQLRef Of
    srNone: WriteLn(F,St + '  SQLRef none');
   srDoors: WriteLn(F,St + '  SQLRef doors');
 srObjects: WriteLn(F,St + '  SQLRef objects');
  End; // Case
  WriteLn(F,St + 'End');
  DecimalSeparator := DecSep;
End; // TMeshLibraryObjectReference.SaveToFile

// ------------------------------
// TCreatureLibraryObjectReference
// ------------------------------

Constructor TCreatureLibraryObjectReference.Create;
Begin
  Inherited Create;
  FGravity := True;
  FAn8File := Nil;
  FMinPt   := T3DPoint.Create;
  FMaxPt   := T3DPoint.Create;
  FSizeSet := False;
End; // TCreatureLibraryObjectReference.Create

Constructor TCreatureLibraryObjectReference.Create(An8: TAn8File);
Begin
  Inherited Create;
  FGravity := True;
  FAn8File := An8;
  FMinPt   := T3DPoint.Create;
  FMaxPt   := T3DPoint.Create;
  FSizeSet := False;
End; // TCreatureLibraryObjectReference.Create

Constructor TCreatureLibraryObjectReference.Create(CL: TCreatureLibraryObjectReference);
Begin
  Inherited Create(CL);
  FGravity := CL.Gravity;
  FAn8File := CL.An8File;
  FMinPt   := T3DPoint.Create;
  FMaxPt   := T3DPoint.Create;
  FSizeSet := False;
End; // TCreatureLibraryObjectReference.Create

Destructor TCreatureLibraryObjectReference.Destroy;
Begin
  FMinPt.Free;
  FMaxPt.Free;
  Inherited;
End; // TCreatureLibraryObjectReference.Destroy

Procedure TCreatureLibraryObjectReference.SetAn8File(An8: TAn8File);
Begin
  FAn8File := An8;
End; // TCreatureLibraryObjectReference.SetAn8File

Procedure TCreatureLibraryObjectReference.SetGravity(B: Boolean);
Begin
  FGravity := B;
  frmMain.RefreshObject(Self,True,True,True);
  Application.ProcessMessages;
End; // TCreatureLibraryObjectReference.SetGravity

Procedure TCreatureLibraryObjectReference.GetSize(Out MinPos,MaxPos: T3DPoint; Var FirstTex: String);
Begin
  Inherited GetSize(MinPos,MaxPos,FirstTex);
  FHeading.Rotate(MinPos);
  FHeading.Rotate(MaxPos);
  MinPos.Add(FLoc);
  MaxPos.Add(FLoc);
End; // TCreatureLibraryObjectReference.GetSize

Procedure TCreatureLibraryObjectReference.SetSize(AMinPt,AMaxPt: T3DPoint);
Begin
  FMinPt.Copy(AMinPt);
  FMaxPt.Copy(AMaxPt);
  FSizeSet := True;
End;

Procedure TCreatureLibraryObjectReference.LoadCreature;
Var
  I,J : Integer;
  Obj : TAn8Object;

Begin
  If Not FAn8File.Loaded Then
  Begin
    I := CreatureLibrary.IndexOfObject(FAn8File);
    If I >= 0 Then FAn8File.LoadFromFile(ExtractFilePath(Application.EXEName) + 'library\creatures\' + CreatureLibrary.Strings[I] + '.an8');
    For I := 0 To FAn8File.Objects.Count - 1 Do
    Begin
      Obj := TAn8Object(FAn8File.Objects.Objects[I]);
      For J := 0 To Obj.Components.Count - 1 Do TAn8Component(Obj.Components.Objects[J]).ConvertToTriangles;
      Obj.MergeVeryClosePoints;
    End; // For I
    For I := 0 To FAn8File.Figures.Count - 1 Do TAn8Figure(FAn8File.Figures.Objects[I]).DeterminePrimaryBones;
  End;
End;

Procedure TCreatureLibraryObjectReference.RespondToGravity;
Var Z1,Z2,Z3,Z4: Single;
Begin
  // If gravity is switched on, then if the mesh lies above or below the ground, move it until there
  // is NO gap between its bottom and the ground.  This allows things like trees to move with the
  // ground when the ground's elevation is changed in the editor.

  If Gravity And FSizeSet Then
  Begin
    If frmMain.GetHighestZ(FMinPt.X, FMinPt.Y, Z1) And
       frmMain.GetHighestZ(FMaxPt.X, FMinPt.Y, Z2) And
       frmMain.GetHighestZ(FMinPt.X, FMaxPt.Y, Z3) And
       frmMain.GetHighestZ(FMaxPt.X, FMaxPt.Y, Z4) Then
    Begin
      Z1 := Max(Max(Max(Z1,Z2),Z3),Z4);

      // Move the object up or down to meet the ground

      // Change the original location by the amount that we moved the mesh

      FLoc.Z := Z1;
    End;
  End;
End; // TCreatureLibraryObjectReference.RespondToGravity

Procedure TCreatureLibraryObjectReference.AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean);
Var
  I,J      : Integer;
  Obj      : TAn8Object;
  L        : TStringList;
  ZO       : TZoneObject;
  Mesh     : TMeshObject;
  V        : T3DPoint;
  V1       : T3DPoint;

  NV       : T3DPoint;
  NV1      : T3DPoint;

  Sequence : TAn8Sequence;
  NO       : TAn8NamedObject;
  Bones    : TStringList;
  Q        : TQuaternion;
  Q1       : TQuaternion;
  Bone     : TAn8Bone;
  Bone1    : TAn8Bone;
  Z1       : Single;
  Z2       : Single;
  Z3       : Single;
  Z4       : Single;

Begin
  // Add an empty mesh object

//  List.AddObject(FName,TMeshObject.Create(FName));

  LoadCreature;

  RespondToGravity;

  If FAn8File.Objects.Count > 0 Then
  Begin
    Obj := TAn8Object(FAn8File.Objects.Objects[0]);
    L   := TStringList.Create;
    L.AddObject('',Obj);
    ZO := FZone.ImportAn8Objects('an8',FAn8File,L,True,False);
    L.Clear;
    If ZO <> Nil Then
    Begin
      ZO.AddToPolygonList(L,False,True,True,True);
      Mesh := FZone.CoalescePolygonList(L);
      For I := 0 To L.Count - 1 Do L.Objects[I].Free;

      // Subtract the bone origin from all points

      V := T3DPoint.Create;
      NV := T3DPoint.Create;
      If An8File.Sequences.Count > 0 Then
      Begin
        Sequence := TAn8Sequence(An8File.Sequences.Objects[0]);
        If (Sequence <> Nil) And (Sequence.Figure <> Nil) And (Sequence.Figure.RootBone <> Nil) Then
        Begin
          NO := TAn8NamedObject(Sequence.Figure.RootBone.FindNamedObjects.Objects[0]);

          Bones := TStringList.Create;
          Sequence.Figure.RootBone.GetBones(Bones);
          Q  := TQuaternion.Create;
          Q1 := TQuaternion.Create;

          // Transform each vertex

          For I := 0 To Mesh.Vertices.Count - 1 Do
          Begin
            // Get the vertex

            V1  := T3DPoint(Mesh.Vertices.Objects[I]);
            NV1 := T3DPoint(Mesh.Normals.Objects[I]);

            // Continue only if the vertex is attached to a bone

            If (High(Mesh.BoneIndices) >= I) And (Mesh.BoneIndices[I] >= 0) And (Mesh.BoneIndices[I] < Bones.Count) Then
            Begin
              Bone := TAn8Bone(Bones.Objects[Mesh.BoneIndices[I]]);

              // Make a copy of the vertex that is relative to the bone origin

              V.Copy(V1.Y,V1.Z,V1.X);
              V.Subtract(Bone.Origin);

              // Add any base position adjustment

              If NO <> Nil Then V.Add(NO.Base);

              // Get an absolute transformation by walking up the bone chain

              Q.Copy(Bone.Orientation);
              Bone1 := Bone.Parent;
              While Bone1 <> Nil Do
              Begin
                Q1.Copy(Bone1.Orientation);
                Q1.Multiply(Q);
                Q.Copy(Q1);
                Bone1 := Bone1.Parent;
              End; // While
              Q.Invert;

              // Transform the vertex

              Q.Transform(V);

              V1.Copy(V.Z,V.X,V.Y);

              // Transform the normal

              NV.Copy(NV1.Y,NV1.Z,NV1.X);
              Q.Transform(NV);
              NV1.Copy(NV.Z,NV.X,NV.Y);

            End;
          End; // For I
          Q.Free;
          Q1.Free;
          Bones.Free;
        End;
      End;
      V.Free;
      NV.Free;

      L.Clear;
      List.AddObject(FName,Mesh);
      ZO.Free;
    End
    Else List.AddObject(FName,TMeshObject.Create(FName));
    L.Free;
  End
  Else List.AddObject(FName,TMeshObject.Create(FName));
End; // TCreatureLibraryObjectReference.AddToPolygonList

Function TCreatureLibraryObjectReference.LoadFromFile(Var F: System.Text): Boolean;
Var
  Valid   : Boolean;
  Done    : Boolean;
  Line    : String;
  S       : String; // Mirrors Line, but always uppercase
  Command : String; // Always uppercase
  Cmd     : String; // Mirrors Command, but preserves case

  Procedure ParseCreature;
  Var I: Integer;
  Begin
    I := CreatureLibrary.IndexOf(Line);
    If I >= 0
     Then FAn8File := TAn8File(CreatureLibrary.Objects[I])
     Else Valid    := False;
  End; // ParseCreature

  Procedure ParseGravity;
  Var
    G      : Boolean;
    Tokens : TTokenArray;

  Begin
    GetTokens(',',S,Tokens);
    If High(Tokens) = 0 Then
    Begin
      Valid := Valid And GetBooleanValue(Tokens[0],G);
      If Valid Then FGravity := G;
    End
    Else Valid := False;
    SetLength(Tokens,0);
  End; // ParseGravity

Begin
  Valid := Inherited LoadFromFile(F);
  Done  := False;
  While Valid And (Not Done) And Not Eof(F) Do
  Begin
    ReadLn(F,Line);
    Line := Trim(Line);
    S    := UpperCase(Line);
    If S <> '' Then
    Begin
      Command := GetToken(' ',S);
      Cmd     := GetToken(' ',Line);
           If Command = 'CREATUREOBJECT' Then ParseCreature
      Else If Command = 'GRAVITY'    Then ParseGravity
      Else If Command = 'END'        Then Done := True
      Else Valid := False;
    End;
  End; // While
  Result := Valid;
End; // TCreatureLibraryObjectReference.LoadFromFile

Procedure TCreatureLibraryObjectReference.SaveToFile(Var F: System.Text; Indent: Integer);
Var
  I      : Integer;
  St     : String;
  DecSep : Char;

Begin
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  St := GetIndent(Indent);
  WriteLn(F,St + 'CreatureLibraryReference');
  Inherited SaveToFile(F,Indent);
  I := CreatureLibrary.IndexOfObject(FAn8File);
  WriteLn(F,St + '  CreatureObject ' + CreatureLibrary.Strings[I]);
  WriteLn(F,St + '  Gravity '    + BoolToStr(Gravity,True));
  WriteLn(F,St + 'End');
  DecimalSeparator := DecSep;
End; // TCreatureLibraryObjectReference.SaveToFile

Function TCreatureLibraryObjectReference.MakeCopy: TZoneObject;
Begin
  Result := TCreatureLibraryObjectReference.Create(Self);
End; // TCreatureLibraryObjectReference.MakeCopy

{
// ------------------------------
// TMeshLibraryObjectReferenceParameters
// ------------------------------

Constructor TMeshLibraryObjectReferenceParameters.Create(Parent: TScriptedObject);
Begin
  FParent := Parent;
End; // TMeshLibraryObjectReferenceParameters.Create

Destructor TMeshLibraryObjectReferenceParameters.Destroy;
Begin
End; // TScriptedObjectParameters.Destroy
}
// ------------------------------
// TScriptedObjectParameters
// ------------------------------

Constructor TScriptedObjectParameters.Create(Parent: TScriptedObject);
Begin
  FParent     := Parent;
  FParameters := TStringList.Create;
  FParmValues := TStringList.Create;
End; // TScriptedObjectParameters.Create

Destructor TScriptedObjectParameters.Destroy;
Begin
  FParameters.Free;
  FParmValues.Free;
End; // TScriptedObjectParameters.Destroy

// ------------------------------
// TScriptedObjectParametersProperty
// ------------------------------

Function TScriptedObjectParametersProperty.GetAttributes: TPropertyAttributes;
Begin
  Result := [paReadOnly,paSubProperties];
End; // TScriptedObjectParametersProperty.GetAttributes

Procedure TScriptedObjectParametersProperty.GetProperties(Proc: TGetPropProc);
Var
  I,J        : Integer;
  SP         : TScriptedObjectParameters;
  Names      : String;
  Values     : String;
  RealValues : String;
  Name       : String;
  UpName     : String;
  PT         : TScriptParamType;
  V          : TExpressionOp;

Begin
  If PropCount > 0 Then
  Begin
    J := GetOrdValueAt(0);
    If J <> 0 Then
    Begin
      SP    := TScriptedObjectParameters(GetOrdValueAt(0));
      Names := Trim(SP.FParent.GetParameterNamesAndValues(Values,RealValues));
      I     := 0;
      While Names <> '' Do
      Begin
        Name   := GetToken(' ',Names);
        UpName := UpperCase(Name);
        PT     := SP.FParent.GetScript.GetParamType(UpName);
        J      := SP.FParent.GetScript.Variables.IndexOf(UpName);
        V      := TExpressionOp(SP.FParent.GetScript.Variables.Objects[J]);
        If Not V.Data.Operand.Hidden Then
        Begin
          Case PT Of
            sptBoolean: Proc(TScriptBooleanParameterProperty.Create(Self,I));
             sptString: Proc(TScriptTextureParameterProperty.Create(Self,I));
          Else
            Proc(TScriptParameterProperty.Create(Self,I));
          End; // Case
        End;
        Inc(I);
      End; // While
    End;
  End;
End; // TScriptedObjectParametersProperty.GetProperties

Function TScriptedObjectParametersProperty.GetName: String;
Begin
  Result := 'Properties';
End; // TScriptedObjectParametersProperty.GetName

Function TScriptedObjectParametersProperty.GetValue: String;
Begin
  Result := '(properties)';
End; // TScriptedObjectParametersProperty.GetValue

Procedure TScriptedObjectParametersProperty.PropDrawName(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Begin
//  DefaultPropertyDrawName(Self,ACanvas,ARect);
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,GetName,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
End; // TScriptedObjectParametersProperty.PropDrawName

Procedure TScriptedObjectParametersProperty.PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Var
  C  : TColor;
  St : String;

Begin
  // Draw it in dark red

  St := GetValue;
  C  := ACanvas.Font.Color;
  ACanvas.Font.Color := clMaroon;
//  ACanvas.TextRect(ARect,ARect.Left + 1,ARect.Top + 1,St);
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,St,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
  ACanvas.Font.Color := C;
End; // TScriptedObjectParametersProperty.PropDrawValue

// ------------------------------
// TScriptParameterProperty
// ------------------------------

Constructor TScriptParameterProperty.Create(AParent: TPropertyEditor; AElement: Integer);
Begin
  Inherited Create(AParent);
  FParent  := AParent;
  FElement := AElement;
  FValue   := '';
End; // TScriptParameterProperty.Create

Function TScriptParameterProperty.GetAttributes: TPropertyAttributes;
Var
  SPP   : TScriptedObjectParametersProperty;
  SP    : TScriptedObjectParameters;
  I,J,K : Integer;
  V     : TExpressionOp;
  Name  : String;

Begin
  Result := Inherited GetAttributes;
  If FParent Is TScriptedObjectParametersProperty Then
  Begin
    SPP := TScriptedObjectParametersProperty(FParent);
    If SPP.PropCount > 0 Then
    Begin
      I := SPP.GetOrdValueAt(0);
      If I <> 0 Then
      Begin
        SP   := TScriptedObjectParameters(SPP.GetOrdValueAt(0));
        J    := FElement;
        Name := SP.FParent.GetScript.ParamNames.Strings[J];
        K    := SP.FParent.GetScript.Variables.IndexOf(Name);
        V    := TExpressionOp(SP.FParent.GetScript.Variables.Objects[K]);
        If V.Data.Operand.ReadOnly Then Result := Result + [paReadOnly];
      End;
    End;
  End;
End; // TScriptParameterProperty.GetAttributes

Function TScriptParameterProperty.GetValue: String;
Var
  SPP        : TScriptedObjectParametersProperty;
  SP         : TScriptedObjectParameters;
  I,J        : Integer;
  Names      : String;
  Values     : String;
  RealValues : String;
  Value      : String;
  RealValue  : String;

Begin
  FDefault := False;
  FValue   := 'Value';
  If FParent Is TScriptedObjectParametersProperty Then
  Begin
    SPP := TScriptedObjectParametersProperty(FParent);
    If SPP.PropCount > 0 Then
    Begin
      I := SPP.GetOrdValueAt(0);
      If I <> 0 Then
      Begin
        SP    := TScriptedObjectParameters(SPP.GetOrdValueAt(0));
        Names := SP.FParent.GetParameterNamesAndValues(Values,RealValues);
        J     := FElement;
        Repeat
          Value     := GetToken(',',Values);
          RealValue := GetToken(',',RealValues);
          Dec(J);
        Until J < 0;
        FDefault := (Value = '');
        If Value <> ''
         Then FValue := Value
         Else FValue := '(' + RealValue + ')';
      End;
    End;
  End;
  Result := FValue;
End; // TScriptParameterProperty.GetValue

Function TScriptParameterProperty.GetName: String;
Var
  SPP        : TScriptedObjectParametersProperty;
  SP         : TScriptedObjectParameters;
  I,J        : Integer;
  Names      : String;
  Values     : String;
  RealValues : String;
  Name       : String;

Begin
  Result := 'ParmName';
  If FParent Is TScriptedObjectParametersProperty Then
  Begin
    SPP := TScriptedObjectParametersProperty(FParent);
    If SPP.PropCount > 0 Then
    Begin
      I := SPP.GetOrdValueAt(0);
      If I <> 0 Then
      Begin
        SP    := TScriptedObjectParameters(SPP.GetOrdValueAt(0));
        Names := SP.FParent.GetParameterNamesAndValues(Values,RealValues);
        J     := FElement;
        Repeat
          Name := GetToken(' ',Names);
          Dec(J);
        Until J < 0;
        Result := LowerCase(Name);
      End;
    End;
  End;
End; // TScriptParameterProperty.GetName

Procedure TScriptParameterProperty.SetValue(Const Value: String);
Begin
  frmMain.UpdateParameter(Value,FElement);
End; // TScriptParameterProperty.SetValue

Procedure TScriptParameterProperty.PropDrawName(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Var
  R    : TRect;
  C    : TColor;
  St   : String;
  Attr : TPropertyAttributes;

Begin
  R := ARect;

  // If it's readonly then override the color

  Attr := GetAttributes;
  GetValue; // This calculates FDefault and sets FValue
  If FDefault And (paReadOnly In Attr) Then
  Begin
    St := GetName;
    C  := ACanvas.Font.Color;
    ACanvas.Font.Color := clGreen;
//    ACanvas.TextRect(R,R.Left + 1,R.Top + 1,St);
    DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,St,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
    ACanvas.Font.Color := C;
  End
  Else DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,GetName,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);//DefaultPropertyDrawName(Self,ACanvas,ARect);
End; // TScriptParameterProperty.PropDrawName

Procedure TScriptParameterProperty.PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Var
  R    : TRect;
  C    : TColor;
  St   : String;
  Attr : TPropertyAttributes;

Begin
  R := ARect;
  Inc(R.Left,10);

  // If it's a default then override the color

  If FDefault Then
  Begin
    St   := FValue; // PropDrawName has already set FDefault and FValue
    Attr := GetAttributes;
    If paReadOnly In Attr Then
    Begin
      C := ACanvas.Font.Color;
      ACanvas.Font.Color := clMaroon;
//      ACanvas.TextRect(R,R.Left + 1,R.Top + 1,Copy(St,2,Length(St) - 2));
      DataModule1.ClearTypeText.ExtTextOut(R.Left + 1,R.Top + 1,Copy(St,2,Length(St) - 2),ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@R);
      ACanvas.Font.Color := C;
    End
    Else
    Begin
      C := ACanvas.Font.Color;
      ACanvas.Font.Color := clBlue;
      ACanvas.TextRect(R,R.Left + 1,R.Top + 1,St);
      DataModule1.ClearTypeText.ExtTextOut(R.Left + 1,R.Top + 1,St,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@R);
      ACanvas.Font.Color := C;
    End;
  End
  Else DataModule1.ClearTypeText.ExtTextOut(R.Left + 1,R.Top + 1,GetVisualValue,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@R);  //DefaultPropertyDrawValue(Self,ACanvas,R);

  // Draw a blank rectangle before the value

  C := ACanvas.Pen.Color;
  ACanvas.Pen.Color := ACanvas.Brush.Color;
  ACanvas.Rectangle(ARect.Left,ARect.Top,ARect.Left + 10,ARect.Bottom);
  ACanvas.Pen.Color := C;
End; // TScriptParameterProperty.PropDrawValue

// ------------------------------
// TScriptBooleanParameterProperty
// ------------------------------

Constructor TScriptBooleanParameterProperty.Create(AParent: TPropertyEditor; AElement: Integer);
Begin
  Inherited Create(AParent.Designer,1);
  FParent  := AParent;
  FElement := AElement;
End; // TScriptBooleanParameterProperty.Create

Function TScriptBooleanParameterProperty.GetAttributes: TPropertyAttributes;
Begin
  Result := [paValueList, paRevertable];
End; // TScriptBooleanParameterProperty.GetAttributes

Function TScriptBooleanParameterProperty.AutoFill: Boolean;
Begin
  Result := True;
End; // TScriptBooleanParameterProperty.AutoFill

Function TScriptBooleanParameterProperty.GetValue: String;
Var
  SPP        : TScriptedObjectParametersProperty;
  SP         : TScriptedObjectParameters;
  I,J        : Integer;
  Names      : String;
  Values     : String;
  RealValues : String;
  Value      : String;
  RealValue  : String;

Begin
  FDefault := False;
  Result   := 'Value';
  If FParent Is TScriptedObjectParametersProperty Then
  Begin
    SPP := TScriptedObjectParametersProperty(FParent);
    If SPP.PropCount > 0 Then
    Begin
      I := SPP.GetOrdValueAt(0);
      If I <> 0 Then
      Begin
        SP    := TScriptedObjectParameters(SPP.GetOrdValueAt(0));
        Names := SP.FParent.GetParameterNamesAndValues(Values,RealValues);
        J     := FElement;
        Repeat
          Value     := GetToken(',',Values);
          RealValue := GetToken(',',RealValues);
          Dec(J);
        Until J < 0;
        FDefault := (Value = '');
        If Value <> ''
         Then Result := Value
         Else Result := '(' + RealValue + ')';
      End;
    End;
  End;
End; // TScriptBooleanParameterProperty.GetValue

Function TScriptBooleanParameterProperty.GetName: String;
Var
  SPP        : TScriptedObjectParametersProperty;
  SP         : TScriptedObjectParameters;
  I,J        : Integer;
  Names      : String;
  Values     : String;
  RealValues : String;
  Name       : String;

Begin
  Result := 'ParmName';
  If FParent Is TScriptedObjectParametersProperty Then
  Begin
    SPP := TScriptedObjectParametersProperty(FParent);
    If SPP.PropCount > 0 Then
    Begin
      I := SPP.GetOrdValueAt(0);
      If I <> 0 Then
      Begin
        SP    := TScriptedObjectParameters(SPP.GetOrdValueAt(0));
        Names := SP.FParent.GetParameterNamesAndValues(Values,RealValues);
        J     := FElement;
        Repeat
          Name := GetToken(' ',Names);
          Dec(J);
        Until J < 0;
        Result := LowerCase(Name);
      End;
    End;
  End;
End; // TScriptBooleanParameterProperty.GetName

Procedure TScriptBooleanParameterProperty.SetValue(Const Value: String);
Begin
  If Value <> '(default value)'
   Then frmMain.UpdateParameter(Value,FElement)
   Else frmMain.UpdateParameter('',FElement);
End; // TScriptBooleanParameterProperty.SetValue

Procedure TScriptBooleanParameterProperty.GetValues(Proc: TGetStrProc);
Begin
  Proc('(default value)');
  Proc('False');
  Proc('True');
End; // TScriptBooleanParameterProperty.GetValues

Procedure TScriptBooleanParameterProperty.PropDrawName(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Begin
//  DefaultPropertyDrawName(Self,ACanvas,ARect);
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,GetName,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
End; // TScriptBooleanParameterProperty.PropDrawName

Procedure TScriptBooleanParameterProperty.PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Var
  R  : TRect;
  C  : TColor;
  St : String;

Begin
  R := ARect;
  Inc(R.Left,10);

  // If it's a default then override the color

  St := GetValue;
  If FDefault Then
  Begin
    C := ACanvas.Font.Color;
    ACanvas.Font.Color := clBlue;
//    ACanvas.TextRect(R,R.Left + 1,R.Top + 1,St);
    DataModule1.ClearTypeText.ExtTextOut(R.Left + 1,R.Top + 1,St,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@R);
    ACanvas.Font.Color := C;
  End
  Else DataModule1.ClearTypeText.ExtTextOut(R.Left + 1,R.Top + 1,GetVisualValue,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@R);//DefaultPropertyDrawValue(Self,ACanvas,R);

  // Draw a blank rectangle before the value

  C := ACanvas.Pen.Color;
  ACanvas.Pen.Color := ACanvas.Brush.Color;
  ACanvas.Rectangle(ARect.Left,ARect.Top,ARect.Left + 10,ARect.Bottom);
  ACanvas.Pen.Color := C;
End; // TScriptBooleanParameterProperty.PropDrawValue

Procedure TScriptBooleanParameterProperty.ListMeasureWidth(Const Value: String; ACanvas: TCanvas; Var AWidth: Integer);
Begin
End; // TScriptBooleanParameterProperty.ListMeasureWidth

Procedure TScriptBooleanParameterProperty.ListMeasureHeight(Const Value: String; ACanvas: TCanvas; Var AHeight: Integer);
Begin
  AHeight := ACanvas.TextHeight('Mg') + 2;
End; // TScriptBooleanParameterProperty.ListMeasureHeight

Procedure TScriptBooleanParameterProperty.ListDrawValue(Const Value: String; ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Begin
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,Value,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
End; // TScriptBooleanParameterProperty.ListDrawValue

// ------------------------------
// TScriptTextureParameterProperty
// ------------------------------

Constructor TScriptTextureParameterProperty.Create(AParent: TPropertyEditor; AElement: Integer);
Begin
  Inherited Create(AParent.Designer,1);
  FParent  := AParent;
  FElement := AElement;
  FDefault := False;
End; // TScriptTextureParameterProperty.Create

Function TScriptTextureParameterProperty.GetAttributes: TPropertyAttributes;
Begin
  Result := [paValueList, paRevertable, paDialog];
End; // TScriptTextureParameterProperty.GetAttributes

Function TScriptTextureParameterProperty.AutoFill: Boolean;
Begin
  Result := True;
End; // TScriptTextureParameterProperty.AutoFill

Function TScriptTextureParameterProperty.GetValue: String;
Var
  SPP        : TScriptedObjectParametersProperty;
  SP         : TScriptedObjectParameters;
  I,J        : Integer;
  Names      : String;
  Values     : String;
  RealValues : String;
  Value      : String;
  RealValue  : String;

Begin
  FDefault := False;
  Result   := 'Value';
  If FParent Is TScriptedObjectParametersProperty Then
  Begin
    SPP := TScriptedObjectParametersProperty(FParent);
    If SPP.PropCount > 0 Then
    Begin
      I := SPP.GetOrdValueAt(0);
      If I <> 0 Then
      Begin
        SP    := TScriptedObjectParameters(SPP.GetOrdValueAt(0));
        Names := SP.FParent.GetParameterNamesAndValues(Values,RealValues);
        J     := FElement;
        Repeat
          Value     := GetToken(',',Values);
          RealValue := GetToken(',',RealValues);
          Dec(J);
        Until J < 0;
        FDefault := (Value = '');
        If Value <> ''
         Then Result := Value
         Else Result := '(' + RealValue + ')';
      End;
    End;
  End;
End; // TScriptTextureParameterProperty.GetValue

Function TScriptTextureParameterProperty.GetName: String;
Var
  SPP        : TScriptedObjectParametersProperty;
  SP         : TScriptedObjectParameters;
  I,J        : Integer;
  Names      : String;
  Values     : String;
  RealValues : String;
  Name       : String;

Begin
  Result := 'ParmName';
  If FParent Is TScriptedObjectParametersProperty Then
  Begin
    SPP := TScriptedObjectParametersProperty(FParent);
    If SPP.PropCount > 0 Then
    Begin
      I := SPP.GetOrdValueAt(0);
      If I <> 0 Then
      Begin
        SP    := TScriptedObjectParameters(SPP.GetOrdValueAt(0));
        Names := SP.FParent.GetParameterNamesAndValues(Values,RealValues);
        J     := FElement;
        Repeat
          Name := GetToken(' ',Names);
          Dec(J);
        Until J < 0;
        Result := LowerCase(Name);
      End;
    End;
  End;
End; // TScriptTextureParameterProperty.GetName

Procedure TScriptTextureParameterProperty.SetValue(Const Value: String);
Begin
  If Value <> TextureLibrary.Strings[0]
   Then frmMain.UpdateParameter(Value,FElement)
   Else frmMain.UpdateParameter('',FElement);
End; // TScriptTextureParameterProperty.SetValue

Procedure TScriptTextureParameterProperty.GetValues(Proc: TGetStrProc);
Var I: Integer;
Begin
  For I := 0 To TextureLibrary.Count - 1 Do Proc(TextureLibrary.Strings[I]);
End; // TScriptTextureParameterProperty.GetValues

Procedure TScriptTextureParameterProperty.Edit;
Var St: String;
Begin
  frmEditTextureList.Texture := GetValue;
  frmEditTextureList.ShowModal;
  If frmEditTextureList.ModalResult = mrOk Then
  Begin
    St := frmEditTextureList.GetTexturesAsString;
    frmMain.UpdateParameter(St,FElement);
    frmEditTextureList.Texture := St;
  End;
End; // TScriptTextureParameterProperty.Edit

Procedure TScriptTextureParameterProperty.PropDrawName(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Begin
//  DefaultPropertyDrawName(Self,ACanvas,ARect);
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,GetName,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
End; // TScriptTextureParameterProperty.PropDrawName

Procedure TScriptTextureParameterProperty.PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Var
  R  : TRect;
  C  : TColor;
  St : String;

Begin
  R := ARect;
  Inc(R.Left,10);

  // If it's a default then override the color

  St := GetValue;
  If FDefault Then
  Begin
    C := ACanvas.Font.Color;
    ACanvas.Font.Color := clBlue;
//    ACanvas.TextRect(R,R.Left + 1,R.Top + 1,St);
    DataModule1.ClearTypeText.ExtTextOut(R.Left + 1,R.Top + 1,St,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@R);
    ACanvas.Font.Color := C;
  End
  Else DataModule1.ClearTypeText.ExtTextOut(R.Left + 1,R.Top + 1,GetVisualValue,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@R);//DefaultPropertyDrawValue(Self,ACanvas,R);

  // Draw a blank rectangle before the value

  C := ACanvas.Pen.Color;
  ACanvas.Pen.Color := ACanvas.Brush.Color;
  ACanvas.Rectangle(ARect.Left,ARect.Top,ARect.Left + 10,ARect.Bottom);
  ACanvas.Pen.Color := C;
End; // TScriptTextureParameterProperty.PropDrawValue

Procedure TScriptTextureParameterProperty.ListMeasureWidth(Const Value: String; ACanvas: TCanvas; Var AWidth: Integer);
Begin
End;

Procedure TScriptTextureParameterProperty.ListMeasureHeight(Const Value: String; ACanvas: TCanvas; Var AHeight: Integer);
Begin
  AHeight := ACanvas.TextHeight('Mg') + 2;
End;

Procedure TScriptTextureParameterProperty.ListDrawValue(Const Value: String; ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Begin
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,Value,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
End;

{
// ------------------------------
// TBooleanProperty
// ------------------------------

Constructor TBooleanProperty.Create(AParent: TPropertyEditor; AElement: Integer);
Begin
  Inherited Create(AParent.Designer,1);
  FParent  := AParent;
  FElement := AElement;
End; // TBooleanProperty.Create

Function TBooleanProperty.GetAttributes: TPropertyAttributes;
Begin
  Result := [paValueList, paRevertable];
End; // TBooleanProperty.GetAttributes

Function TBooleanProperty.GetValue: String;
Var MPP: TMeshLibraryObjectReferenceParametersProperty;
Begin
  FDefault := False;
  Result   := 'Value';
  If FParent Is TMeshLibraryObjectReferenceParametersProperty Then
  Begin
    MPP    := TMeshLibraryObjectReferenceParametersProperty(FParent);
    Result := BoolToStr(MPP.FParent.Gravity,True);
  End;
End; // TBooleanProperty.GetValue

Function TBooleanProperty.GetName: String;
Var
  SPP        : TScriptedObjectParametersProperty;
  SP         : TScriptedObjectParameters;
  I,J        : Integer;
  Names      : String;
  Values     : String;
  RealValues : String;
  Name       : String;

Begin
  Result := 'ParmName';
  If FParent Is TScriptedObjectParametersProperty Then
  Begin
    SPP := TScriptedObjectParametersProperty(FParent);
    If SPP.PropCount > 0 Then
    Begin
      I := SPP.GetOrdValueAt(0);
      If I <> 0 Then
      Begin
        SP    := TScriptedObjectParameters(SPP.GetOrdValueAt(0));
        Names := SP.FParent.GetParameterNamesAndValues(Values,RealValues);
        J     := FElement;
        Repeat
          Name := GetToken(' ',Names);
          Dec(J);
        Until J < 0;
        Result := LowerCase(Name);
      End;
    End;
  End;
End; // TBooleanProperty.GetName

Procedure TBooleanProperty.SetValue(Const Value: String);
Begin
  If Value <> '(default value)'
   Then frmMain.UpdateParameter(Value,FElement)
   Else frmMain.UpdateParameter('',FElement);
End; // TBooleanProperty.SetValue

Procedure TBooleanProperty.GetValues(Proc: TGetStrProc);
Begin
  Proc('False');
  Proc('True');
End; // TBooleanProperty.GetValues

Procedure TBooleanProperty.PropDrawName(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Begin
  DefaultPropertyDrawName(Self,ACanvas,ARect);
End; // TBooleanProperty.PropDrawName

Procedure TBooleanProperty.PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Var
  R  : TRect;
  C  : TColor;
  St : String;

Begin
  R := ARect;
  Inc(R.Left,10);

  // If it's a default then override the color

  St := GetValue;
  If FDefault Then
  Begin
    C := ACanvas.Font.Color;
    ACanvas.Font.Color := clBlue;
    ACanvas.TextRect(R,R.Left + 1,R.Top + 1,St);
    ACanvas.Font.Color := C;
  End
  Else DefaultPropertyDrawValue(Self,ACanvas,R);

  // Draw a blank rectangle before the value

  C := ACanvas.Pen.Color;
  ACanvas.Pen.Color := ACanvas.Brush.Color;
  ACanvas.Rectangle(ARect.Left,ARect.Top,ARect.Left + 10,ARect.Bottom);
  ACanvas.Pen.Color := C;
End; // TBooleanProperty.PropDrawValue
}
// ------------------------------
// T3DPointProperty
// ------------------------------

Function T3DPointProperty.GetAttributes: TPropertyAttributes;
Begin
  Result := [paReadOnly,paSubProperties];
End; // T3DPointProperty.GetAttributes

Procedure T3DPointProperty.GetProperties(Proc: TGetPropProc);
Begin
  Proc(T3DPointElementProperty.Create(Self,0,'X'));
  Proc(T3DPointElementProperty.Create(Self,1,'Y'));
  Proc(T3DPointElementProperty.Create(Self,2,'Z'));
End; // T3DPointProperty.GetProperties

Function T3DPointProperty.GetValue: String;
Begin
  Result := '(3DPoint)';
End; // T3DPointProperty.GetValue

Procedure T3DPointProperty.PropDrawName(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Begin
//  DefaultPropertyDrawName(Self,ACanvas,ARect);
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,GetName,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
End; // T3DPointProperty.PropDrawName

Procedure T3DPointProperty.PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Var
  C  : TColor;
  St : String;

Begin
  // Draw it in dark red

  St := GetValue;
  C  := ACanvas.Font.Color;
  ACanvas.Font.Color := clMaroon;
//  ACanvas.TextRect(ARect,ARect.Left + 1,ARect.Top + 1,St);
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,St,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
  ACanvas.Font.Color := C;
End; // T3DPointProperty.PropDrawValue

// ------------------------------
// T3DPointElementProperty
// ------------------------------

Constructor T3DPointElementProperty.Create(AParent: TPropertyEditor; AElement: Integer; AName: String);
Begin
  Inherited Create(AParent);
  FParent  := AParent;
  FElement := AElement;
  FName    := AName;
End; // T3DPointElementProperty.Create

Function T3DPointElementProperty.GetValue: String;
Var
  P3DP : T3DPointProperty;
  V    : T3DPoint;
  I    : Integer;

Begin
  Result := 'Value';
  If FParent Is T3DPointProperty Then
  Begin
    P3DP := T3DPointProperty(FParent);
    If P3DP.PropCount > 0 Then
    Begin
      I := GetOrdValueAt(0);
      If I <> 0 Then
      Begin
        V := T3DPoint(GetOrdValueAt(0));
        If V <> Nil Then
        Begin
          Case FElement Of
            0: Result := FloatToStr(Round(V.X * 1000) / 1000);
            1: Result := FloatToStr(Round(V.Y * 1000) / 1000);
            2: Result := FloatToStr(Round(V.Z * 1000) / 1000);
          Else
            Result := '(unknown)';
          End; // Case
        End;
      End;
    End;
  End;
End; // T3DPointElementProperty.GetValue

Function T3DPointElementProperty.GetName: String;
Begin
  Result := FName;
End; // T3DPointElementProperty.GetName

Procedure T3DPointElementProperty.SetValue(Const Value: String);
Var P3DP: T3DPointProperty;
Begin
  If FParent Is T3DPointProperty Then
  Begin
    P3DP := T3DPointProperty(FParent);
         If P3DP.GetName = 'Loc'  Then frmMain.UpdatePosition(Value,FElement)
    Else If P3DP.GetName = 'Size' Then frmMain.UpdateSize(Value,FElement);
  End;
End; // T3DPointElementProperty.SetValue

// ------------------------------
// THeadingProperty
// ------------------------------

Function THeadingProperty.GetAttributes: TPropertyAttributes;
Begin
  Result := [paReadOnly,paSubProperties];
End; // THeadingProperty.GetAttributes

Procedure THeadingProperty.GetProperties(Proc: TGetPropProc);
Begin
  Proc(THeadingElementProperty.Create(Self,0,'X'));
  Proc(THeadingElementProperty.Create(Self,1,'Y'));
  Proc(THeadingElementProperty.Create(Self,2,'Z'));
End; // THeadingProperty.GetProperties

Function THeadingProperty.GetValue: String;
Begin
  Result := '(Rotation)';
End; // THeadingProperty.GetValue

Procedure THeadingProperty.PropDrawName(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Begin
//  DefaultPropertyDrawName(Self,ACanvas,ARect);
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,GetName,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
End; // THeadingProperty.PropDrawName

Procedure THeadingProperty.PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Var
  C  : TColor;
  St : String;

Begin
  // Draw it in dark red

  St := GetValue;
  C  := ACanvas.Font.Color;
  ACanvas.Font.Color := clMaroon;
//  ACanvas.TextRect(ARect,ARect.Left + 1,ARect.Top + 1,St);
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,St,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
  ACanvas.Font.Color := C;
End; // THeadingProperty.PropDrawValue

// ------------------------------
// THeadingElementProperty
// ------------------------------

Constructor THeadingElementProperty.Create(AParent: TPropertyEditor; AElement: Integer; AName: String);
Begin
  Inherited Create(AParent);
  FParent  := AParent;
  FElement := AElement;
  FName    := AName;
End; // THeadingElementProperty.Create

Function THeadingElementProperty.GetValue: String;
Var
  P3DP : THeadingProperty;
  V    : THeading;
  I    : Integer;

Begin
  Result := 'Value';
  If FParent Is THeadingProperty Then
  Begin
    P3DP := THeadingProperty(FParent);
    If P3DP.PropCount > 0 Then
    Begin
      I := GetOrdValueAt(0);
      If I <> 0 Then
      Begin
        V := THeading(GetOrdValueAt(0));
        Case FElement Of
          0: Result := FloatToStr(Round(V.FXAngle * 1000) / 1000);
          1: Result := FloatToStr(Round(V.FYAngle * 1000) / 1000);
          2: Result := FloatToStr(Round(V.FZAngle * 1000) / 1000);
        Else
          Result := '(unknown)';
        End; // Case
      End;
    End;
  End;
End; // THeadingElementProperty.GetValue

Function THeadingElementProperty.GetName: String;
Begin
  Result := FName;
End; // THeadingElementProperty.GetName

Procedure THeadingElementProperty.SetValue(Const Value: String);
Begin
  If FParent Is THeadingProperty Then frmMain.UpdateRotation(Value,FElement);
End; // THeadingElementProperty.SetValue

// ------------------------------
// TSQLRefProperty
// ------------------------------

Function TSQLRefProperty.GetAttributes: TPropertyAttributes;
Begin
  Result := [paValueList, paRevertable];
End; // TSQLRefProperty.GetAttributes

Function TSQLRefProperty.AutoFill: Boolean;
Begin
  Result := True;
End; // TSQLRefProperty.AutoFill

Function TSQLRefProperty.GetValue: String;
Begin
  Result := GetVisualSQLRefValue(TSQLRef(GetOrdValue));
End; // TSQLRefProperty.GetValue

Function TSQLRefProperty.GetName: String;
Begin
  Result := 'SQLRef';
End; // TSQLRefProperty.GetName

Procedure TSQLRefProperty.SetValue(Const Value: String);
Begin
  frmMain.UpdateSQLRef(Value,FElement);
End; // TSQLRefProperty.SetValue

Procedure TSQLRefProperty.GetValues(Proc: TGetStrProc);
Var Ref: TSQLRef;
Begin
  For Ref := Low(TSQLRef) To High(TSQLRef) Do Proc(GetVisualSQLRefValue(Ref));
End; // TSQLRefProperty.GetValues

Procedure TSQLRefProperty.PropDrawName(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Begin
//  DefaultPropertyDrawName(Self,ACanvas,ARect);
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,GetName,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
End; // TSQLRefProperty.PropDrawName

Procedure TSQLRefProperty.PropDrawValue(ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Var
  R  : TRect;
  C  : TColor;
  St : String;

Begin
  R := ARect;
  Inc(R.Left,10);

  St := GetValue;

  // If it's a default then override the color

  If St = 'None' Then
  Begin
    C := ACanvas.Font.Color;
    ACanvas.Font.Color := clBlue;
//    ACanvas.TextRect(R,R.Left + 1,R.Top + 1,St);
    DataModule1.ClearTypeText.ExtTextOut(R.Left + 1,R.Top + 1,St,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@R);
    ACanvas.Font.Color := C;
  End
  Else DataModule1.ClearTypeText.ExtTextOut(R.Left + 1,R.Top + 1,GetVisualValue,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@R);//DefaultPropertyDrawValue(Self,ACanvas,R);

  // Draw a blank rectangle before the value

  C := ACanvas.Pen.Color;
  ACanvas.Pen.Color := ACanvas.Brush.Color;
  ACanvas.Rectangle(ARect.Left,ARect.Top,ARect.Left + 10,ARect.Bottom);
  ACanvas.Pen.Color := C;
End; // TSQLRefProperty.PropDrawValue

Procedure TSQLRefProperty.ListMeasureWidth(Const Value: String; ACanvas: TCanvas; Var AWidth: Integer);
Begin
End; // TSQLRefProperty.ListMeasureWidth

Procedure TSQLRefProperty.ListMeasureHeight(Const Value: String; ACanvas: TCanvas; Var AHeight: Integer);
Begin
  AHeight := ACanvas.TextHeight('Mg') + 2;
End; // TSQLRefProperty.ListMeasureHeight

Procedure TSQLRefProperty.ListDrawValue(Const Value: String; ACanvas: TCanvas; Const ARect: TRect; ASelected: Boolean);
Begin
  DataModule1.ClearTypeText.ExtTextOut(ARect.Left + 1,ARect.Top + 1,Value,ETO_CLIPPED Or ETO_OPAQUE,ACanvas,ACanvas.Brush.Color,@ARect);
End; // TSQLRefProperty.ListDrawValue

// ------------------------------
// TScriptedObject
// ------------------------------

Constructor TScriptedObject.Create;
Begin
  Inherited Create;
  FScriptName := '';
  FParameters := TScriptedObjectParameters.Create(Self);
End; // TScriptedObject.Create

Constructor TScriptedObject.Create(SO: TScriptedObject);
Begin
  Inherited Create(SO);
  FScriptName := SO.FScriptName;
  FParameters := TScriptedObjectParameters.Create(Self);
  FParameters.FParameters.AddStrings(SO.FParameters.FParameters);
  FParameters.FParmValues.AddStrings(SO.FParameters.FParmValues);
End; // TScriptedObject.Create

Constructor TScriptedObject.Create(AName: String);
Begin
  Inherited Create(AName);
  FScriptName := '';
  FParameters := TScriptedObjectParameters.Create(Self);
End; // TScriptedObject.Create

Constructor TScriptedObject.Create(AName: String; AX,AY,AZ: Single);
Begin
  Inherited Create(AName,AX,AY,AZ);
  FScriptName := '';
  FParameters := TScriptedObjectParameters.Create(Self);
End; // TScriptedObject.Create

Destructor TScriptedObject.Destroy;
Begin
  FParameters.Free;
  Inherited;
End; // TScriptedObject.Destroy

Function TScriptedObject.MakeCopy: TZoneObject;
Begin
  Result := TScriptedObject.Create(Self);
End; // TScriptedObject.MakeCopy

Function TScriptedObject.LoadFromFile(Var F: System.Text): Boolean;
Var
  Valid   : Boolean;
  Done    : Boolean;
  Line    : String;
  S       : String; // Mirrors Line, but always uppercase
  Command : String; // Always uppercase
  Cmd     : String; // Mirrors Command, but preserves case

  Procedure ParseScriptParam;
  Var S1,S2: String;
  Begin
    S1 := S;
    S2 := GetToken(' ',S1);
    FParameters.FParameters.Add(Cmd + ' ' + Line);
  End; // ParseScriptParam

Begin
  Valid := Inherited LoadFromFile(F);
  Done  := False;
  While Valid And (Not Done) And Not Eof(F) Do
  Begin
    ReadLn(F,Line);
    Line := Trim(Line);
    S    := UpperCase(Line);
    If S <> '' Then
    Begin
      Command := GetToken(' ',S);
      Cmd     := GetToken(' ',Line);
      If Command = 'END'
       Then Done := True
       Else ParseScriptParam;
    End;
  End; // While
  Result := Valid;
End; // TScriptedObject.LoadFromFile

Procedure TScriptedObject.SaveToFile(Var F: System.Text; Indent: Integer);
Var
  Names      : String;
  Values     : String;
  RealValues : String;
  Name       : String;
  Value      : String;
  St         : String;
  DecSep     : Char;

Begin
  DecSep := DecimalSeparator;
  DecimalSeparator := '.';
  St := GetIndent(Indent);
  WriteLn(F,St + 'Script ' + FScriptName);
  Inherited SaveToFile(F,Indent);
  Names := GetParameterNamesAndValues(Values,RealValues);
  While Names <> '' Do
  Begin
    Name  := GetToken(' ',Names);
    Value := GetToken(',',Values);
    If Value <> '' Then WriteLn(F,St + '  ' + Name + ' ' + Value);
  End; // While
  WriteLn(F,St + 'End');
  DecimalSeparator := DecSep;
End; // TScriptedObject.SaveToFile

Procedure TScriptedObject.ClearParameters;
Begin
  Parameters.FParameters.Clear;
End; // TScriptedObject.ClearParameters

Procedure TScriptedObject.AddParameter(St: String);
Begin
  Parameters.FParameters.Add(St);
End; // TScriptedObject.AddParameter

Procedure TScriptedObject.SetScriptName(St: String);
Begin
  FScriptName := St;
End; // TScriptedObject.SetScriptName

Function TScriptedObject.GetParameterNamesAndValues(Var Values,RealValues: String): String;
Var
  I,J   : LongInt;
  Found : Boolean;
  S     : TScripted;
  St    : String;
  St1   : String;
  USt1  : String;
  St2   : String;
  PV    : String;
  FParm : String;

Begin
  St     := '';
  Values := '';
  S := GetScript;
  If S <> Nil Then
  Begin
    St := '';
    For I := 0 To S.ParamNames.Count - 1 Do
    Begin
      // Get the parameter name

      St1  := S.ParamNames.Strings[I];
      USt1 := UpperCase(St1);

      // Match up the parameter with those passed to the script

      J     := 0;
      Found := False;
      While (J < Parameters.FParameters.Count) And Not Found Do
      Begin
        FParm := UpperCase(Parameters.FParameters.Strings[J]);
        St2   := GetToken(' ',FParm);
        If St2 = USt1 Then Found := True Else Inc(J);
//        If Copy(UpperCase(Parameters.FParameters.Strings[J]),1,Length(St1)) = UpperCase(St1) Then Found := True Else Inc(J);
      End; // While
      If Found Then
      Begin
        PV := Parameters.FParameters.Strings[J];
        PV := Trim(Copy(PV,Length(St1) + 1,Length(PV)));  // Strip off the parameter name
      End
      Else PV := '';
      Values := Values + PV;
      If I < S.ParamNames.Count - 1 Then Values := Values + ',';
      If St = '' Then St := St1 Else St := St + ' ' + St1;
    End; // For I
    Result := St;

    // Get the actual values, taking defaults into account

    St := '';
    For I := 0 To Parameters.FParmValues.Count - 1 Do
    Begin
      If I > 0 Then St := St + ', ';
      St := St + Parameters.FParmValues.Strings[I];
    End; // For I
    RealValues := St;
  End;
End; // TScriptedObject.GetParameterNamesAndValues

Function TScriptedObject.GetScript: TScripted;
Var
  I     : Integer;
  Found : Boolean;
  S     : TScripted;

Begin
  I     := 0;
  Found := False;
  S     := Nil;
  While (I < ScriptLibrary.Count) And Not Found Do
  Begin
    S := TScripted(ScriptLibrary.Objects[I]);
    If (S <> Nil) And (UpperCase(S.Name) = UpperCase(FScriptName)) Then Found := True Else Inc(I);
  End; // While
  If Found Then Result := S Else Result := Nil;
End; // TScriptedObject.GetScript

Procedure TScriptedObject.AddToPolygonList(List: TStringList; ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,Transform: Boolean);
Var
  I,J    : LongInt;
  Script : TScripted;
  MO     : TMeshObject;
  L1,L2  : TStringList;
  Parm   : String;
  St     : String;

  L   : T3DPoint;
  S   : T3DPoint;
  H   : THeading;
  Par : TGroupObject;  

Begin
  Script := GetScript;
  If Script <> Nil Then
  Begin
    L := T3DPoint.Create;
    H := THeading.Create;
    S := T3DPoint.Create;

    // Save the parent, position, rotation, and size

    Par := FParent;
    L.Copy(FLoc);
    H.Copy(FHeading);
    S.Copy(FSize);

    // Temporarily transform to absolute coordinates

    ChangeToAbsolute(FParent);

    // Create the mesh

    PlaceX  := FLoc.X;
    PlaceY  := FLoc.Y;
    PlaceZ  := FLoc.Z;
    RotateX := FHeading.XAngle;
    RotateY := FHeading.YAngle;
    RotateZ := FHeading.ZAngle;
    SizeX   := FSize.X;
    SizeY   := FSize.Y;
    SizeZ   := FSize.Z;
    L1      := TStringList.Create;
    L2      := TStringList.Create;
    While L1.Count < Script.ParamNames.Count Do L1.Add(UpperCase(Script.ParamNames.Strings[L1.Count]));
    For I := 0 To Parameters.FParameters.Count - 1 Do
    Begin
      St   := Parameters.FParameters.Strings[I];
      Parm := GetToken(' ',St);
      J    := L1.IndexOf(UpperCase(Parm));
      If J >= 0 Then
      Begin
        While L2.Count < J + 1 Do L2.AddObject('',Nil);
        L2.Strings[J] := St;
      End;
    End; // For I

    MO := Script.MakeMeshObject(Self,L2);
    L2.Free;
    L1.Free;
    MO.FLoc.Copy(FLoc);
    MO.FHeading.Copy(FHeading);
    MO.FSize.Copy(FSize);
    MO.AddToPolygonList(List,ExcludeMeshReferences,ExcludeCreatureReferences,ExcludeInserts,False);
    MO.Free;

    // Restore parent status and relative coordinates

    FParent := Par;
    FLoc.Copy(L);
    FHeading.Copy(H);
    FSize.Copy(S);

    // Cleanup

    L.Free;
    H.Free;
    S.Free;
  End;
End; // TScriptedObject.AddToPolygonList

// ------------------------------
// TScripted
// ------------------------------

Constructor TScripted.Create;
Begin
  Inherited Create;
  Name             := '';
  Category         := 'OTHER';
  Script           := TStringList.Create;
  VDefaults        := TStringList.Create;
  Compiled         := TStringList.Create;
  Variables        := TStringList.Create;
  ParamNames       := TStringList.Create;
  Variables.Sorted := True;
End; // TScripted.Create

Destructor TScripted.Destroy;
Begin
  Script.Free;
  FreeListAndItsObjects(VDefaults);
  FreeListAndItsObjects(Compiled);
  FreeListAndItsObjects(Variables);
  ParamNames.Free;
End; // TScripted.Destroy

Function TScripted.MakeMeshObject(Owner: TScriptedObject; Parameters: TStringList): TMeshObject;
Var
  ErrorText   : String;
  MO          : TMeshObject;
  M           : TMeshObject;
  I,K         : LongInt;
  Command     : TScriptPredicate;
  S           : TScriptStatement;
  Valid       : Boolean;
  Found       : Boolean;
  SO          : TScripted;
  PlaceX0     : Single;
  PlaceY0     : Single;
  PlaceZ0     : Single;
  RotateX0    : Single;
  RotateY0    : Single;
  RotateZ0    : Single;
  SizeX0      : Single;
  SizeY0      : Single;
  SizeZ0      : Single;
  Polygon     : TPolygon;
  WhileStack  : Array Of LongInt;
  RepeatStack : Array Of LongInt;
  Parms       : TStringList;

  Procedure ParseTriangle(SST: TScriptStatementTriangle);
  Var
    I,J       : LongInt;
    Value     : TOperandRec;
    Param     : Array[0..25] Of Single;
    TexName   : String;
    L         : TExpression;
    H         : THeading;
    P1        : T3DPoint;
    P2        : T3DPoint;
    P3        : T3DPoint;
    NP1       : T3DPoint;
    NP2       : T3DPoint;
    NP3       : T3DPoint;
    P         : T3DPoint;
    Polygon   : TPolygon;
    HasTrans  : Boolean;
    HasSTrans : Boolean;
    HasSolid  : Boolean;
    HasColor  : Boolean;
    HasMasked : Boolean;
    Trans     : Boolean;
    STrans    : Boolean;
    Solid     : Boolean;
    Color     : Integer;
    Masked    : Boolean;
    HasNormal : Boolean;

  Begin
    I         := 0;
    HasTrans  := True;
    HasSTrans := True;
    HasSolid  := True;
    HasColor  := True;
    HasMasked := True;
    HasNormal := True;
    Trans     := False;
    STrans    := False;
    Solid     := True;
    Color     := clBlack;
    Masked    := False;
    While (I < 26) And Valid Do
    Begin
      If I <> 9 Then
      Begin
        Case I Of
          0: L := SST.X1;
          1: L := SST.Y1;
          2: L := SST.Z1;
          3: L := SST.X2;
          4: L := SST.Y2;
          5: L := SST.Z2;
          6: L := SST.X3;
          7: L := SST.Y3;
          8: L := SST.Z3;
         10: L := SST.Trans;
         11: L := SST.STrans;
         12: L := SST.Solid;
         13: L := SST.Color;
         14: L := SST.HasColor;
         15: L := SST.Masked;
         16: L := SST.HasNormal;
         17: L := SST.NX1;
         18: L := SST.NY1;
         19: L := SST.NZ1;
         20: L := SST.NX2;
         21: L := SST.NY2;
         22: L := SST.NZ2;
         23: L := SST.NX3;
         24: L := SST.NY3;
         25: L := SST.NZ3;
        Else
          L := Nil;
        End; // Case
        If (I > 9) And (I < 17) Then
        Begin
          If L = Nil Then
          Begin
            Case I Of
              10: HasTrans  := False;
              11: HasSTrans := False;
              12: HasSolid  := False;
              13: HasColor  := False;
              // Don't do anything for HasColor
              15: HasMasked := False;
              16: HasNormal := False;
            End; // Case
          End
          Else
          Begin
            Value := L.Evaluate(Variables);
            Case I Of
              10: Begin
                    If Value.OperandType = onBoolean Then Trans := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              11: Begin
                    If Value.OperandType = onBoolean Then STrans := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              12: Begin
                    If Value.OperandType = onBoolean Then Solid := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              13: Begin
                    If Value.OperandType = onInteger Then Color := Value.Value.Int
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to an integer.');
                      Valid := False;
                    End;
                  End;
              14: Begin
                    If Value.OperandType = onBoolean Then HasColor := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              15: Begin
                    If Value.OperandType = onBoolean Then Masked := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              16: Begin
                    If Value.OperandType = onBoolean Then HasNormal := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
            End; // Case
          End;
        End
        Else
        Begin
          If (I < 9) Or
             ((I >= 17) And (I <= 25) And HasNormal) Then
          Begin
            Value := L.Evaluate(Variables);
            If Value.OperandType In [onInteger,onReal] Then
            Begin
              Case Value.OperandType Of
                onInteger: Param[I] := Value.Value.Int;
                   onReal: Param[I] := Value.Value.Ext;
              Else
                LogParseError('Error executing TRIANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                              'Parameter number ' + IntToStr(I + 1) +
                              ' needs to evaluate to an integer or float.');
                Valid := False;
              End; // Case
            End
            Else
            Begin
              LogParseError('Error executing TRIANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                            'Parameter number ' + IntToStr(I + 1) +
                            ' could not be evaluated.');
              Valid := False;
            End;
          End;
        End;
      End
      Else
      Begin
        Value := SST.Tex.Evaluate(Variables);
        If Value.OperandType <> onString Then
        Begin
          LogParseError('Error executing TRIANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                        'The texture could not be resolved.');
        End;
        TexName := Value.Value.St;
      End;
      Inc(I);
    End; // While
    If Valid Then
    Begin
      I  := MO.Vertices.Count;
      P1 := T3DPoint.Create(Param[0] * SizeX,Param[1] * SizeY,Param[2] * SizeZ);
      P2 := T3DPoint.Create(Param[3] * SizeX,Param[4] * SizeY,Param[5] * SizeZ);
      P3 := T3DPoint.Create(Param[6] * SizeX,Param[7] * SizeY,Param[8] * SizeZ);
      If Not (P1.Equals(P2) Or P1.Equals(P3) Or P2.Equals(P3)) Then
      Begin
        P  := T3DPoint.Create(PlaceX,PlaceY,PlaceZ);
        H  := THeading.Create(RotateX,RotateY,RotateZ);
        H.Rotate(P1);
        H.Rotate(P2);
        H.Rotate(P3);
        P1.Add(P);
        P2.Add(P);
        P3.Add(P);
        P.Free;

        // Always add a normal, even if it's just the polygon's face normal

        If HasNormal Then
        Begin
          NP1 := T3DPoint.Create(Param[17],Param[18],Param[19]);
          NP2 := T3DPoint.Create(Param[20],Param[21],Param[22]);
          NP3 := T3DPoint.Create(Param[23],Param[24],Param[25]);
        End
        Else
        Begin
          NP1 := T3DPoint.Create(P1);
          NP3 := T3DPoint.Create(P3);
          NP1.Subtract(P2);
          NP3.Subtract(P2);
          NP1.Cross(NP3);
          NP2 := T3DPoint.Create(NP1);
          NP3.Copy(NP1);
        End;
        NP1.Normalize;
        NP2.Normalize;
        NP3.Normalize;
        MO.Normals.AddObject('',NP1);
        MO.Normals.AddObject('',NP2);
        MO.Normals.AddObject('',NP3);

        // Add the vertices

        H.Free;
        MO.Vertices.AddObject('',P1);
        MO.Vertices.AddObject('',P2);
        MO.Vertices.AddObject('',P3);

        // Add the polygon

        Polygon := TPolygon.Create([I,I + 1,I + 2],CompressTextureList(TexName));
        MO.Polygons.AddObject('',Polygon);
        If HasSTrans Then If STrans Then Polygon.TextureState := tsSemiTransparent;
        If HasTrans  Then If  Trans Then Polygon.TextureState := tsTransparent;
        If HasColor Then
        Begin
          Begin
            SetLength(Polygon.Colors,3);
            For J := 0 To 2 Do Polygon.Colors[J] := TColor(Color);
          End;
          Polygon.HasColor := True;
        End;
        If HasSolid Then
        Begin
          Polygon.Solid    := Solid;
          Polygon.HasSolid := True;
        End;
        If HasMasked Then
        Begin
          Polygon.Masked    := Masked;
          Polygon.HasMasked := True;
        End;
      End
      Else
      Begin
        P1.Free;
        P2.Free;
        P3.Free;
      End;
    End;
  End; // ParseTriangle

  Procedure ParseTriangleTex(SST: TScriptStatementTriangle);
  Var
    I,J       : LongInt;
    Value     : TOperandRec;
    Param     : Array[0..31] Of Single;
    TexName   : String;
    L         : TExpression;
    H         : THeading;
    P1        : T3DPoint;
    P2        : T3DPoint;
    P3        : T3DPoint;
    NP1       : T3DPoint;
    NP2       : T3DPoint;
    NP3       : T3DPoint;
    P         : T3DPoint;
    Polygon   : TPolygon;
    HasTrans  : Boolean;
    HasSTrans : Boolean;
    HasSolid  : Boolean;
    HasColor  : Boolean;
    HasMasked : Boolean;
    HasNormal : Boolean;
    Trans     : Boolean;
    STrans    : Boolean;
    Solid     : Boolean;
    Color     : Integer;
    Masked    : Boolean;

  Begin
    I         := 0;
    HasTrans  := True;
    HasSTrans := True;
    HasSolid  := True;
    HasColor  := True;
    HasMasked := True;
    HasNormal := True;
    Trans     := False;
    STrans    := False;
    Solid     := True;
    Color     := clBlack;
    Masked    := False;
    While (I < 32) And Valid Do
    Begin
      If I <> 15 Then
      Begin
        Case I Of
          0: L := SST.X1;
          1: L := SST.Y1;
          2: L := SST.Z1;
          3: L := SST.X2;
          4: L := SST.Y2;
          5: L := SST.Z2;
          6: L := SST.X3;
          7: L := SST.Y3;
          8: L := SST.Z3;
          9: L := SST.TX1;
         10: L := SST.TZ1;
         11: L := SST.TX2;
         12: L := SST.TZ2;
         13: L := SST.TX3;
         14: L := SST.TZ3;
         16: L := SST.Trans;
         17: L := SST.STrans;
         18: L := SST.Solid;
         19: L := SST.Color;
         20: L := SST.HasColor;
         21: L := SST.Masked;
         22: L := SST.HasNormal;
         23: L := SST.NX1;
         24: L := SST.NY1;
         25: L := SST.NZ1;
         26: L := SST.NX2;
         27: L := SST.NY2;
         28: L := SST.NZ2;
         29: L := SST.NX3;
         30: L := SST.NY3;
         31: L := SST.NZ3;
        Else
          L := Nil;
        End; // Case
        If (I > 15) And (I < 23) Then
        Begin
          If L = Nil Then
          Begin
            Case I Of
              16: HasTrans  := False;
              17: HasSTrans := False;
              18: HasSolid  := False;
              19: HasColor  := False;
              // Don't do anything for HasColor
              21: HasMasked := False;
              22: HasNormal := False;
            End; // Case
          End
          Else
          Begin
            Value := L.Evaluate(Variables);
            Case I Of
              16: Begin
                    If Value.OperandType = onBoolean Then Trans := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              17: Begin
                    If Value.OperandType = onBoolean Then STrans := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              18: Begin
                    If Value.OperandType = onBoolean Then Solid := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              19: Begin
                    If Value.OperandType = onInteger Then Color := Value.Value.Int
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to an integer.');
                      Valid := False;
                    End;
                  End;
              20: Begin
                    If Value.OperandType = onBoolean Then HasColor := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              21: Begin
                    If Value.OperandType = onBoolean Then Masked := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              22: Begin
                    If Value.OperandType = onBoolean Then HasNormal := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing TRIANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
            End; // Case
          End;
        End
        Else
        Begin
          If (I < 15) Or
             ((I >= 23) And (I <= 31) And HasNormal) Then
          Begin
            Value := L.Evaluate(Variables);
            If Value.OperandType In [onInteger,onReal] Then
            Begin
              Case Value.OperandType Of
                onInteger: Param[I] := Value.Value.Int;
                   onReal: Param[I] := Value.Value.Ext;
              Else
                LogParseError('Error executing TRIANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                              'Parameter number ' + IntToStr(I + 1) +
                              ' needs to evaluate to an integer or float.');
                Valid := False;
              End; // Case
            End
            Else
            Begin
              LogParseError('Error executing TRIANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                            'Parameter number ' + IntToStr(I + 1) +
                            ' could not be evaluated.');
              Valid := False;
            End;
          End;
        End;
      End
      Else
      Begin
        Value := SST.Tex.Evaluate(Variables);
        If Value.OperandType <> onString Then
        Begin
          LogParseError('Error executing TRIANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                        'The texture could not be resolved.');
        End;
        TexName := Value.Value.St;
      End;
      Inc(I);
    End; // While
    If Valid Then
    Begin
      I  := MO.Vertices.Count;
      P1 := T3DPoint.Create(Param[0] * SizeX,Param[1] * SizeY,Param[2] * SizeZ);
      P2 := T3DPoint.Create(Param[3] * SizeX,Param[4] * SizeY,Param[5] * SizeZ);
      P3 := T3DPoint.Create(Param[6] * SizeX,Param[7] * SizeY,Param[8] * SizeZ);
      If Not (P1.Equals(P2) Or P1.Equals(P3) Or P2.Equals(P3)) Then
      Begin
        P  := T3DPoint.Create(PlaceX,PlaceY,PlaceZ);
        H  := THeading.Create(RotateX,RotateY,RotateZ);
        H.Rotate(P1);
        H.Rotate(P2);
        H.Rotate(P3);
        P1.Add(P);
        P2.Add(P);
        P3.Add(P);
        P.Free;

        // Always add a normal, even if it's just the polygon's face normal

        If HasNormal Then
        Begin
          NP1 := T3DPoint.Create(Param[23],Param[24],Param[25]);
          NP2 := T3DPoint.Create(Param[26],Param[27],Param[28]);
          NP3 := T3DPoint.Create(Param[29],Param[30],Param[31]);
        End
        Else
        Begin
          NP1 := T3DPoint.Create(P1);
          NP3 := T3DPoint.Create(P3);
          NP1.Subtract(P2);
          NP3.Subtract(P2);
          NP1.Cross(NP3);
          NP2 := T3DPoint.Create(NP1);
          NP3.Copy(NP1);
        End;
        NP1.Normalize;
        NP2.Normalize;
        NP3.Normalize;
        MO.Normals.AddObject('',NP1);
        MO.Normals.AddObject('',NP2);
        MO.Normals.AddObject('',NP3);

        // Add the vertices

        H.Free;
        MO.Vertices.AddObject('',P1);
        MO.Vertices.AddObject('',P2);
        MO.Vertices.AddObject('',P3);

        // Add the polygon

        Polygon := TPolygon.Create([I,I + 1,I + 2],CompressTextureList(TexName));
        MO.Polygons.AddObject('',Polygon);
        Polygon.HasTexCoords := True;
        SetLength(Polygon.TX,3);
        SetLength(Polygon.TZ,3);
        Polygon.TX[0] := Param[9];
        Polygon.TZ[0] := Param[10];
        Polygon.TX[1] := Param[11];
        Polygon.TZ[1] := Param[12];
        Polygon.TX[2] := Param[13];
        Polygon.TZ[2] := Param[14];
        If HasSTrans Then If STrans Then Polygon.TextureState := tsSemiTransparent;
        If HasTrans  Then If  Trans Then Polygon.TextureState := tsTransparent;
        If HasColor Then
        Begin
          Begin
            SetLength(Polygon.Colors,3);
            For J := 0 To 2 Do Polygon.Colors[J] := TColor(Color);
          End;
          Polygon.HasColor := True;
        End;
        If HasSolid Then
        Begin
          Polygon.Solid    := Solid;
          Polygon.HasSolid := True;
        End;
        If HasMasked Then
        Begin
          Polygon.Masked    := Masked;
          Polygon.HasMasked := True;
        End;
      End
      Else
      Begin
        P1.Free;
        P2.Free;
        P3.Free;
      End;
    End;
  End; // ParseTriangleTex

  Procedure ParseRectangle(SSR: TScriptStatementRectangle);
  Var
    I,J       : LongInt;
    Value     : TOperandRec;
    Param     : Array[0..31] Of Single;
    TexName   : String;
    L         : TExpression;
    H         : THeading;
    P1        : T3DPoint;
    P2        : T3DPoint;
    P3        : T3DPoint;
    P4        : T3DPoint;
    NP1       : T3DPoint;
    NP2       : T3DPoint;
    NP3       : T3DPoint;
    NP4       : T3DPoint;
    P         : T3DPoint;
    Polygon   : TPolygon;
    HasTrans  : Boolean;
    HasSTrans : Boolean;
    HasSolid  : Boolean;
    HasColor  : Boolean;
    HasMasked : Boolean;
    HasNormal : Boolean;
    Trans     : Boolean;
    STrans    : Boolean;
    Solid     : Boolean;
    Color     : Integer;
    Masked    : Boolean;

  Begin
    I         := 0;
    HasTrans  := True;
    HasSTrans := True;
    HasSolid  := True;
    HasColor  := True;
    HasMasked := True;
    HasNormal := True;
    Trans     := False;
    STrans    := False;
    Solid     := True;
    Color     := clBlack;
    Masked    := False;
    While (I < 32) And Valid Do
    Begin
      If I <> 12 Then
      Begin
        Case I Of
          0: L := SSR.X1;
          1: L := SSR.Y1;
          2: L := SSR.Z1;
          3: L := SSR.X2;
          4: L := SSR.Y2;
          5: L := SSR.Z2;
          6: L := SSR.X3;
          7: L := SSR.Y3;
          8: L := SSR.Z3;
          9: L := SSR.X4;
         10: L := SSR.Y4;
         11: L := SSR.Z4;
         13: L := SSR.Trans;
         14: L := SSR.STrans;
         15: L := SSR.Solid;
         16: L := SSR.Color;
         17: L := SSR.HasColor;
         18: L := SSR.Masked;
         19: L := SSR.HasNormal;
         20: L := SSR.NX1;
         21: L := SSR.NY1;
         22: L := SSR.NZ1;
         23: L := SSR.NX2;
         24: L := SSR.NY2;
         25: L := SSR.NZ2;
         26: L := SSR.NX3;
         27: L := SSR.NY3;
         28: L := SSR.NZ3;
         29: L := SSR.NX4;
         30: L := SSR.NY4;
         31: L := SSR.NZ4;
        Else
          L := Nil;
        End; // Case
        If (I > 12) And (I < 20) Then
        Begin
          If L = Nil Then
          Begin
            Case I Of
              13: HasTrans  := False;
              14: HasSTrans := False;
              15: HasSolid  := False;
              16: HasColor  := False;
              // Don't do anything for HasColor
              18: HasMasked := False;
              19: HasNormal := False;
            End; // Case
          End
          Else
          Begin
            Value := L.Evaluate(Variables);
            Case I Of
              13: Begin
                    If Value.OperandType = onBoolean Then Trans := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              14: Begin
                    If Value.OperandType = onBoolean Then STrans := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              15: Begin
                    If Value.OperandType = onBoolean Then Solid := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              16: Begin
                    If Value.OperandType = onInteger Then Color := Value.Value.Int
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to an integer.');
                      Valid := False;
                    End;
                  End;
              17: Begin
                    If Value.OperandType = onBoolean Then HasColor := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              18: Begin
                    If Value.OperandType = onBoolean Then Masked := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              19: Begin
                    If Value.OperandType = onBoolean Then HasNormal := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
            End; // Case
          End;
        End
        Else
        Begin
          If (I < 12) Or
             ((I >= 20) And (I <= 31) And HasNormal) Then
          Begin
            Value := L.Evaluate(Variables);
            If Value.OperandType In [onInteger,onReal] Then
            Begin
              Case Value.OperandType Of
                onInteger: Param[I] := Value.Value.Int;
                   onReal: Param[I] := Value.Value.Ext;
              Else
                LogParseError('Error executing RECTANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                              'Parameter number ' + IntToStr(I + 1) +
                              ' needs to evaluate to an integer or float.');
                Valid := False;
              End; // Case
            End
            Else
            Begin
              LogParseError('Error executing RECTANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                            'Parameter number ' + IntToStr(I + 1) +
                            ' could not be evaluated.');
              Valid := False;
            End;
          End;
        End;
      End
      Else
      Begin
        Value := SSR.Tex.Evaluate(Variables);
        If Value.OperandType <> onString Then
        Begin
          LogParseError('Error executing RECTANGLE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                        'The texture could not be resolved.');
        End;
        TexName := Value.Value.St;
      End;
      Inc(I);
    End; // While
    If Valid Then
    Begin
      I  := MO.Vertices.Count;
      P  := T3DPoint.Create(PlaceX,PlaceY,PlaceZ);
      P1 := T3DPoint.Create(Param[0] *  SizeX,Param[1] *  SizeY,Param[2] * SizeZ);
      P2 := T3DPoint.Create(Param[3] *  SizeX,Param[4] *  SizeY,Param[5] * SizeZ);
      P3 := T3DPoint.Create(Param[6] *  SizeX,Param[7] *  SizeY,Param[8] * SizeZ);
      P4 := T3DPoint.Create(Param[9] * SizeX,Param[10] * SizeY,Param[11] * SizeZ);
      H  := THeading.Create(RotateX,RotateY,RotateZ);
      H.Rotate(P1);
      H.Rotate(P2);
      H.Rotate(P3);
      H.Rotate(P4);
      P1.Add(P);
      P2.Add(P);
      P3.Add(P);
      P4.Add(P);
      P.Free;

      // Always add a normal, even if it's just the polygon's face normal

      If HasNormal Then
      Begin
        NP1 := T3DPoint.Create(Param[20],Param[21],Param[22]);
        NP2 := T3DPoint.Create(Param[23],Param[24],Param[25]);
        NP3 := T3DPoint.Create(Param[26],Param[27],Param[28]);
        NP4 := T3DPoint.Create(Param[29],Param[30],Param[31]);
      End
      Else
      Begin
        NP1 := T3DPoint.Create(P1);
        NP3 := T3DPoint.Create(P3);
        NP1.Subtract(P2);
        NP3.Subtract(P2);
        NP1.Cross(NP3);
        NP2 := T3DPoint.Create(NP1);
        NP4 := T3DPoint.Create(NP1);
        NP3.Copy(NP1);
      End;
      NP1.Normalize;
      NP2.Normalize;
      NP3.Normalize;
      NP4.Normalize;
      MO.Normals.AddObject('',NP1);
      MO.Normals.AddObject('',NP2);
      MO.Normals.AddObject('',NP3);
      MO.Normals.AddObject('',NP4);

      // Add the vertices

      H.Free;
      MO.Vertices.AddObject('',P1);
      MO.Vertices.AddObject('',P2);
      MO.Vertices.AddObject('',P3);
      MO.Vertices.AddObject('',P4);

      // Add the polygon

      Polygon := TPolygon.Create([I,I + 1,I + 2,I + 3],CompressTextureList(TexName));
      MO.Polygons.AddObject('',Polygon);
      If HasSTrans Then If STrans Then Polygon.TextureState := tsSemiTransparent;
      If HasTrans  Then If  Trans Then Polygon.TextureState := tsTransparent;
      If HasColor Then
      Begin
        Begin
          SetLength(Polygon.Colors,4);
          For J := 0 To 3 Do Polygon.Colors[J] := TColor(Color);
        End;
        Polygon.HasColor := True;
      End;
      If HasSolid Then
      Begin
        Polygon.Solid    := Solid;
        Polygon.HasSolid := True;
      End;
      If HasMasked Then
      Begin
        Polygon.Masked    := Masked;
        Polygon.HasMasked := True;
      End;
    End;
  End; // ParseRectangle

  Procedure ParseRectangleTex(SSR: TScriptStatementRectangle);
  Var
    I,J       : LongInt;
    Value     : TOperandRec;
    Param     : Array[0..39] Of Single;
    TexName   : String;
    L         : TExpression;
    H         : THeading;
    P1        : T3DPoint;
    P2        : T3DPoint;
    P3        : T3DPoint;
    P4        : T3DPoint;
    NP1       : T3DPoint;
    NP2       : T3DPoint;
    NP3       : T3DPoint;
    NP4       : T3DPoint;
    P         : T3DPoint;
    Polygon   : TPolygon;
    HasTrans  : Boolean;
    HasSTrans : Boolean;
    HasSolid  : Boolean;
    HasColor  : Boolean;
    HasMasked : Boolean;
    HasNormal : Boolean;
    Trans     : Boolean;
    STrans    : Boolean;
    Solid     : Boolean;
    Color     : Integer;
    Masked    : Boolean;

  Begin
    I         := 0;
    HasTrans  := True;
    HasSTrans := True;
    HasSolid  := True;
    HasColor  := True;
    HasMasked := True;
    HasNormal := True;
    Trans     := False;
    STrans    := False;
    Solid     := True;
    Color     := clBlack;
    Masked    := False;
    While (I < 40) And Valid Do
    Begin
      If I <> 20 Then
      Begin
        Case I Of
          0: L := SSR.X1;
          1: L := SSR.Y1;
          2: L := SSR.Z1;
          3: L := SSR.X2;
          4: L := SSR.Y2;
          5: L := SSR.Z2;
          6: L := SSR.X3;
          7: L := SSR.Y3;
          8: L := SSR.Z3;
          9: L := SSR.X4;
         10: L := SSR.Y4;
         11: L := SSR.Z4;
         12: L := SSR.TX1;
         13: L := SSR.TZ1;
         14: L := SSR.TX2;
         15: L := SSR.TZ2;
         16: L := SSR.TX3;
         17: L := SSR.TZ3;
         18: L := SSR.TX4;
         19: L := SSR.TZ4;
         21: L := SSR.Trans;
         22: L := SSR.STrans;
         23: L := SSR.Solid;
         24: L := SSR.Color;
         25: L := SSR.HasColor;
         26: L := SSR.Masked;
         27: L := SSR.HasNormal;
         28: L := SSR.NX1;
         29: L := SSR.NY1;
         30: L := SSR.NZ1;
         31: L := SSR.NX2;
         32: L := SSR.NY2;
         33: L := SSR.NZ2;
         34: L := SSR.NX3;
         35: L := SSR.NY3;
         36: L := SSR.NZ3;
         37: L := SSR.NX4;
         38: L := SSR.NY4;
         39: L := SSR.NZ4;
        Else
          L := Nil;
        End; // Case
        If (I > 20) And (I < 28) Then
        Begin
          If L = Nil Then
          Begin
            Case I Of
              21: HasTrans  := False;
              22: HasSTrans := False;
              23: HasSolid  := False;
              24: HasColor  := False;
              // Don't do anything for HasColor
              26: HasMasked := False;
              27: HasNormal := False;
            End; // Case
          End
          Else
          Begin
            Value := L.Evaluate(Variables);
            Case I Of
              21: Begin
                    If Value.OperandType = onBoolean Then Trans := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              22: Begin
                    If Value.OperandType = onBoolean Then STrans := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              23: Begin
                    If Value.OperandType = onBoolean Then Solid := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              24: Begin
                    If Value.OperandType = onInteger Then Color := Value.Value.Int
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to an integer.');
                      Valid := False;
                    End;
                  End;
              25: Begin
                    If Value.OperandType = onBoolean Then HasColor := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              26: Begin
                    If Value.OperandType = onBoolean Then Masked := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
              27: Begin
                    If Value.OperandType = onBoolean Then HasNormal := Value.Value.Bool
                    Else
                    Begin
                      LogParseError('Error executing RECTANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                                    'Parameter number ' + IntToStr(I + 1) +
                                    ' needs to evaluate to a boolean.');
                      Valid := False;
                    End;
                  End;
            End; // Case
          End;
        End
        Else
        Begin
          If (I < 20) Or
             ((I >= 28) And (I <= 39) And HasNormal) Then
          Begin
            Value := L.Evaluate(Variables);
            If Value.OperandType In [onInteger,onReal] Then
            Begin
              Case Value.OperandType Of
                onInteger: Param[I] := Value.Value.Int;
                   onReal: Param[I] := Value.Value.Ext;
              Else
                LogParseError('Error executing RECTANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                              'Parameter number ' + IntToStr(I + 1) +
                              ' needs to evaluate to an integer or float.');
                Valid := False;
              End; // Case
            End
            Else
            Begin
              LogParseError('Error executing RECTANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                            'Parameter number ' + IntToStr(I + 1) +
                            ' could not be evaluated.');
              Valid := False;
            End;
          End;
        End;
      End
      Else
      Begin
        Value := SSR.Tex.Evaluate(Variables);
        If Value.OperandType <> onString Then
        Begin
          LogParseError('Error executing RECTANGLETEX statement in script ' + Name + ', ' + ErrorText + '.  ' +
                        'The texture could not be resolved.');
        End;
        TexName := Value.Value.St;
      End;
      Inc(I);
    End; // While
    If Valid Then
    Begin
      I  := MO.Vertices.Count;
      P  := T3DPoint.Create(PlaceX,PlaceY,PlaceZ);
      P1 := T3DPoint.Create(Param[0] *  SizeX,Param[1] *  SizeY,Param[2] * SizeZ);
      P2 := T3DPoint.Create(Param[3] *  SizeX,Param[4] *  SizeY,Param[5] * SizeZ);
      P3 := T3DPoint.Create(Param[6] *  SizeX,Param[7] *  SizeY,Param[8] * SizeZ);
      P4 := T3DPoint.Create(Param[9] * SizeX,Param[10] * SizeY,Param[11] * SizeZ);
      H  := THeading.Create(RotateX,RotateY,RotateZ);
      H.Rotate(P1);
      H.Rotate(P2);
      H.Rotate(P3);
      H.Rotate(P4);
      P1.Add(P);
      P2.Add(P);
      P3.Add(P);
      P4.Add(P);
      P.Free;

      // Always add a normal, even if it's just the polygon's face normal

      If HasNormal Then
      Begin
        NP1 := T3DPoint.Create(Param[28],Param[29],Param[30]);
        NP2 := T3DPoint.Create(Param[31],Param[32],Param[33]);
        NP3 := T3DPoint.Create(Param[34],Param[35],Param[36]);
        NP4 := T3DPoint.Create(Param[37],Param[38],Param[39]);
      End
      Else
      Begin
        NP1 := T3DPoint.Create(P1);
        NP3 := T3DPoint.Create(P3);
        NP1.Subtract(P2);
        NP3.Subtract(P2);
        NP1.Cross(NP3);
        NP2 := T3DPoint.Create(NP1);
        NP4 := T3DPoint.Create(NP1);
        NP3.Copy(NP1);
      End;
      NP1.Normalize;
      NP2.Normalize;
      NP3.Normalize;
      NP4.Normalize;
      MO.Normals.AddObject('',NP1);
      MO.Normals.AddObject('',NP2);
      MO.Normals.AddObject('',NP3);
      MO.Normals.AddObject('',NP4);

      // Add the vertices

      H.Free;
      MO.Vertices.AddObject('',P1);
      MO.Vertices.AddObject('',P2);
      MO.Vertices.AddObject('',P3);
      MO.Vertices.AddObject('',P4);

      // Add the polygon

      Polygon := TPolygon.Create([I,I + 1,I + 2,I + 3],CompressTextureList(TexName));
      MO.Polygons.AddObject('',Polygon);
      Polygon.HasTexCoords := True;
      SetLength(Polygon.TX,4);
      SetLength(Polygon.TZ,4);
      Polygon.TX[0] := Param[12];
      Polygon.TZ[0] := Param[13];
      Polygon.TX[1] := Param[14];
      Polygon.TZ[1] := Param[15];
      Polygon.TX[2] := Param[16];
      Polygon.TZ[2] := Param[17];
      Polygon.TX[3] := Param[18];
      Polygon.TZ[3] := Param[19];
      If HasSTrans Then If STrans Then Polygon.TextureState := tsSemiTransparent;
      If HasTrans  Then If  Trans Then Polygon.TextureState := tsTransparent;
      If HasColor Then
      Begin
        Begin
          SetLength(Polygon.Colors,4);
          For J := 0 To 3 Do Polygon.Colors[J] := TColor(Color);
        End;
        Polygon.HasColor := True;
      End;
      If HasSolid Then
      Begin
        Polygon.Solid    := Solid;
        Polygon.HasSolid := True;
      End;
      If HasMasked Then
      Begin
        Polygon.Masked    := Masked;
        Polygon.HasMasked := True;
      End;
    End;
  End; // ParseRectangleTex

  Procedure ParseSQLParm(SQP: TScriptStatementSQLParm);
  Var
    TValue : TOperandRec;
    CValue : TOperandRec;
    VValue : TOperandRec;

    Procedure SetSQLParm(Parent: TGroupObject; Table,Column,Value: String);
    Var I: Integer;

      Procedure ProcessObject(ZO: TZoneObject; Table,Column,Value: String);
      Var
        I     : Integer;
        Found : Boolean;
        St    : String;
        St1   : String;

      Begin
             If ZO Is TGroupObject Then SetSQLParm(TGroupObject(ZO),Table,Column,Value)
        Else If (ZO Is TMeshLibraryObjectReference) And (TMeshLibraryObjectReference(ZO).SQLRef <> srNone) Then
        Begin
          I     := 0;
          Found := False;
          St    := Table + ',' + Column + ',';
          While (I < TMeshLibraryObjectReference(ZO).FSQLParms.Count) And Not Found Do
          Begin
            St1 := TMeshLibraryObjectReference(ZO).FSQLParms.Strings[I];
            If Copy(St1,1,Length(St)) = St
             Then Found := True
             Else Inc(I);
          End; // While
          If Found
           Then TMeshLibraryObjectReference(ZO).FSQLParms.Strings[I] := Table + ',' + Column + ',' + Value
           Else TMeshLibraryObjectReference(ZO).FSQLParms.Add(Table + ',' + Column + ',' + Value);
        End;
      End; // ProcessObject

    Begin
      If Parent = Nil Then
      Begin
        For I := 0 To Owner.FZone.Count - 1 Do ProcessObject(TZoneObject(Owner.FZone.Objects[I]),Table,Column,Value);
      End
      Else
      Begin
        For I := 0 To Parent.Objects.Count - 1 Do ProcessObject(TZoneObject(Parent.Objects.Objects[I]),Table,Column,Value);
      End;
    End; // SetSQLParm

  Begin
    If SQP <> Nil Then
    Begin
      If SQP.Table <> Nil Then
      Begin
        TValue := SQP.Table.Evaluate(Variables);
        If TValue.OperandType = onString Then
        Begin
          If SQP.Column <> Nil Then
          Begin
            CValue := SQP.Column.Evaluate(Variables);
            If CValue.OperandType = onString Then
            Begin
              If SQP.Value <> Nil Then
              Begin
                VValue := SQP.Value.Evaluate(Variables);
                Case VValue.OperandType Of
                  onInteger: SetSQLParm(Owner.GetParent,TValue.Value.St,CValue.Value.St,IntToStr(VValue.Value.Int));
                     onReal: SetSQLParm(Owner.GetParent,TValue.Value.St,CValue.Value.St,FloatToStr(VValue.Value.Ext));
                  onBoolean: SetSQLParm(Owner.GetParent,TValue.Value.St,CValue.Value.St,BoolToStr(VValue.Value.Bool));
                   onString: SetSQLParm(Owner.GetParent,TValue.Value.St,CValue.Value.St,VValue.Value.St);
                End; // Case
              End
              Else
              Begin
                Valid := False;
                LogParseError('Error executing SQLPARM statement in script ' + Name + '.  ' +
                              'The value parameter was Nil.');
              End;
            End
            Else
            Begin
              LogParseError('Error executing SQLPARM statement in script ' + Name + ', ' + ErrorText + '.  ' +
                            'The column could not be resolved.');
            End;
          End
          Else
          Begin
            Valid := False;
            LogParseError('Error executing SQLPARM statement in script ' + Name + '.  ' +
                          'The column parameter was Nil.');
          End;
        End
        Else
        Begin
          LogParseError('Error executing SQLPARM statement in script ' + Name + ', ' + ErrorText + '.  ' +
                        'The table could not be resolved.');
        End;
      End
      Else
      Begin
        Valid := False;
        LogParseError('Error executing SQLPARM statement in script ' + Name + '.  ' +
                      'The table parameter was Nil.');
      End;
    End
    Else
    Begin
      Valid := False;
      LogParseError('Error executing SQLPARM statement in script ' + Name + '.  ' +
                    'The script statement was Nil.');
    End;
  End; // ParseSQLParm

  Procedure ParseAssignment(SSA: TScriptStatementAssignment);
  Var
    B     : Boolean;
    Value : TOperandRec;

  Begin
    Value := SSA.Source.Evaluate(Variables);
    Case SSA.Dest.Data.Operand.OperandType Of
      onInteger: B := Value.OperandType = onInteger;
         onReal: B := Value.OperandType In [onInteger,onReal];
      onBoolean: B := Value.OperandType = onBoolean;
       onString: B := Value.OperandType = onString;
    Else
      Valid := False;
      If Not Valid Then
      Begin
        LogParseError('Error executing assignment statement to ' + SSA.Dest.Data.Operand.Name + ' in script ' + Name + ', ' + ErrorText + '.  ' +
                      'The expression could not be evaluated.');
      End;
      B     := False;
    End; // Case
    If B Then
    Begin
      Case SSA.Dest.Data.Operand.OperandType Of
        onInteger: SSA.Dest.Data.Operand.Value.Int  := Value.Value.Int;
           onReal: If Value.OperandType = onInteger
                    Then SSA.Dest.Data.Operand.Value.Ext := Value.Value.Int
                    Else SSA.Dest.Data.Operand.Value.Ext := Value.Value.Ext;
        onBoolean: SSA.Dest.Data.Operand.Value.Bool := Value.Value.Bool;
         onString: SSA.Dest.Data.Operand.Value.St   := Value.Value.St;
      End; // Case
    End
    Else
    Begin
      LogParseError('Error executing assignment statement to ' + SSA.Dest.Data.Operand.Name + ' in script ' + Name + ', ' + ErrorText + '.  ' +
                    'The expression could not be evaluated.');
      Valid := False;
    End;
  End; // ParseAssignment

  Procedure ParseIf(SSI: TScriptStatementSingleExpression);
  Var
    J     : Integer;
    Found : Boolean;
    Value : TOperandRec;
    Cmd   : TScriptPredicate;
    Level : Integer;

  Label Top;
  Begin
Top:
     Value := SSI.E.Evaluate(Variables);
    If Value.OperandType = onBoolean Then
    Begin
      If Not Value.Value.Bool Then
      Begin
        J     := I + 1;
        Found := False;
        Level := 0;
        Cmd   := spdIf;
        While (J < Compiled.Count) And Not Found Do
        Begin
          Cmd := TScriptStatement(Compiled.Objects[J]).Predicate;
          If (Level = 0) And (Cmd In [spdElse,spdEndIf,spdElseIf]) Then Found := True Else
          Begin
            If Cmd In [spdIf,spdWhile,spdRepeat]  Then Inc(Level);
            If Cmd In [spdEndIf,spdWend,spdUntil] Then Dec(Level);
            Inc(J);
          End;
        End; // While
        If Found And (Cmd = spdElseIf) Then
        Begin
          I   := J;
          S   := TScriptStatement(Compiled.Objects[J]); // Simulate a read from the main loop
          SSI := TScriptStatementSingleExpression(S);
          Goto Top;                       // I hate Goto statements but this is easiest
        End;
        If Found Then I := J Else
        Begin
          LogParseError('Error executing IF statement in script ' + Name + ', ' + ErrorText + '.  ' +
                        'Could not find matching ELSE, ENDIF, or ELSEIF.');

          Valid := False;
        End;
      End;
    End
    Else
    Begin
      LogParseError('Error executing IF statement in script ' + Name + ', ' + ErrorText + '.  ' +
                    'Could not evaluate expression.');
      Valid := False;
    End;
  End; // ParseIf

  Function FindCommand(_Cmd: TScriptPredicate): LongInt;
  Var
    J     : Integer;
    Found : Boolean;
    Cmd   : TScriptPredicate;
    Level : Integer;

  Begin
    J     := I + 1;
    Found := False;
    Level := 0;
    While (J < Compiled.Count) And Not Found Do
    Begin
      Cmd := TScriptStatement(Compiled.Objects[J]).Predicate;
      If (Level = 0) And (Cmd = _Cmd) Then Found := True Else
      Begin
        If Cmd In [spdIf,spdWhile,spdRepeat]  Then Inc(Level);
        If Cmd In [spdEndIf,spdWend,spdUntil] Then Dec(Level);
        Inc(J);
      End;
    End; // While
    If Found Then Result := J Else Result := -1;
  End; // FindCommand

  Procedure ParseElse;
  Var J: LongInt;
  Begin
    J := FindCommand(spdEndIf);
    If J >= 0 Then I := J Else
    Begin
      LogParseError('Error executing ELSE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                    'Could not find matching ENDIF.');
      Valid := False;
    End;
  End; // ParseElse

  Procedure ParseWhile(SSW: TScriptStatementSingleExpression);
  Var
    J     : LongInt;
    Value : TOperandRec;

  Begin
    SetLength(WhileStack,High(WhileStack) + 2);
    WhileStack[High(WhileStack)] := I;
    Value := SSW.E.Evaluate(Variables);
    If Value.OperandType <> onBoolean Then
    Begin
      LogParseError('Error executing WHILE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                    'Could not evaluate expression.');
      Valid := False;
    End
    Else
    Begin
      If Not Value.Value.Bool Then
      Begin
        J := FindCommand(spdWend);
        If J >= 0 Then I := J Else
        Begin
          LogParseError('Error executing WHILE statement in script ' + Name + ', ' + ErrorText + '.  ' +
                        'Could not find matching WEND.');
          Valid := False;
        End;
        If High(WhileStack) >= 0 Then SetLength(WhileStack,High(WhileStack));
      End;
    End;
  End; // ParseWhile

  Procedure ParseWend;
  Var J: LongInt;
  Begin
    J := WhileStack[High(WhileStack)];
    If High(WhileStack) >= 0 Then
    Begin
      SetLength(WhileStack,High(WhileStack));
      I := J - 1;
    End
    Else Valid := False;
  End; // ParseWend

  Procedure ParseRepeat;
  Begin
    SetLength(RepeatStack,High(RepeatStack) + 2);
    RepeatStack[High(RepeatStack)] := I;
  End; // ParseRepeat

  Procedure ParseUntil(SSU: TScriptStatementSingleExpression);
  Var
    J     : LongInt;
    Value : TOperandRec;

  Begin
    J := RepeatStack[High(RepeatStack)];
    If High(RepeatStack) >= 0 Then
    Begin
      SetLength(RepeatStack,High(RepeatStack));
      Value := SSU.E.Evaluate(Variables);
      If Value.OperandType <> onBoolean Then
      Begin
        LogParseError('Error executing UNTIL statement in script ' + Name + ', ' + ErrorText + '.  ' +
                      'Could not evaluate expression.');
        Valid := False;
      End
      Else
      Begin
        If Not Value.Value.Bool Then I := J - 1;
      End;
    End
    Else
    Begin
      LogParseError('Error executing UNTIL statement in script ' + Name + ', ' + ErrorText + '.  ' +
                    'Could not find matching REPEAT.');
      Valid := False;
    End;
  End; // ParseUntil

  Function EvaluateParms(Parms: String): String;
  Var
    Expression : String;
    St         : String;
    Value      : TOperandRec;
    Exp        : TExpression;

  Begin
    St := '';
    Repeat
      If St <> '' Then St := St + ', ';
      Expression := GetToken(',',Parms);
      Exp        := TExpression.Create(Expression);
      Value      := Exp.Evaluate(Variables);
      Exp.Free;
      If Value.OperandType In [onInteger,onReal,onBoolean,onString] Then
      Begin
        Case Value.OperandType Of
          onInteger: St := St + IntToStr(Value.Value.Int);
             onReal: St := St + FloatToStr(Value.Value.Ext);
          onBoolean: St := St + BoolToStr(Value.Value.Bool,True);
           onString: St := St + '"' + Value.Value.St + '"';
        Else
          LogParseError('Error evaluating parameter expression in script ' + Name + ', ' + ErrorText + '.  ' +
                        'Result of expression "' + Expression +
                        '" must evaluate to an integer, float, boolean, or string.');
          Valid  := False;
        End; // Case
      End
      Else
      Begin
        LogParseError('Error evaluating parameter expression in script ' + Name + ', ' + ErrorText + '.  ' +
                      'Could not evaluate expression "' + Expression + '".');
        Valid  := False;
      End;
    Until (Expression = '') Or (Parms = '') Or Not Valid;
    Result := St;
  End; // EvaluateParms

  Function MakeParmList(SO: TScripted; Parms: String): TStringList;
  Var L: TStringList;
  Begin
    L := TStringList.Create;
    While Parms <> '' Do L.Add(GetToken(',',Parms));
    Result := L;
  End; // MakeParmList

  Procedure ParseScript(SSS: TScriptStatementScriptCall);
  Var
    J  : Integer;
    S  : String;
    St : String;
//    X,Y,Z : Single;
//    X0,Y0,Z0 : Single;
    P : T3DPoint;
    H : THeading;
    H0 : THeading;
    H1 : THeading;
    RX : Single;
    RY : Single;
    RZ : Single;

    Function ExpToFloat(Expression: String): Single;
    Var
      Value : TOperandRec;
      Exp   : TExpression;

    Begin
      Result := 0;
      Exp    := TExpression.Create(Expression);
      Value  := Exp.Evaluate(Variables);
      Exp.Free;
      If Value.OperandType In [onInteger,onReal] Then
      Begin
        Case Value.OperandType Of
          onInteger: Result := Value.Value.Int;
             onReal: Result := Value.Value.Ext;
        Else
          LogParseError('Error evaluating expression in call to script ' + SSS.ScriptName +
                        ' in script ' + Name + ', ' + ErrorText + '.  ' +
                        'Result of expression "' + Expression + '" must evaluate to an integer or float.');
          Valid  := False;
        End; // Case
      End
      Else
      Begin
        LogParseError('Error evaluating expression in call to ' + SSS.ScriptName +
                      ' in script ' + Name + ', ' + ErrorText + '.  ' +
                      'Could not evaluate expression "' + Expression + '".');
        Valid := False;
      End;
    End; // ExpToFloat

  Begin
    // Find the script

    J     := 0;
    Found := False;
    SO    := Nil;
    While (J < ScriptLibrary.Count) And Not Found Do
    Begin
      SO := TScripted(ScriptLibrary.Objects[J]);
      If UpperCase(SO.Name) = SSS.ScriptName Then Found := True Else Inc(J);
    End; // While
    If Found Then
    Begin
      // Proceed with the script call

      PlaceX0  := PlaceX;
      PlaceY0  := PlaceY;
      PlaceZ0  := PlaceZ;
      RotateX0 := RotateX;
      RotateY0 := RotateY;
      RotateZ0 := RotateZ;
      SizeX0   := SizeX;
      SizeY0   := SizeY;
      SizeZ0   := SizeZ;

      S  := SSS.ScriptParms;
      
      P  := T3DPoint.Create;
      H  := THeading.Create(RotateX,RotateY,RotateZ);
      St := GetToken(',',S); P.X := SizeX * ExpToFloat(St);
      St := GetToken(',',S); P.Y := SizeY * ExpToFloat(St);
      St := GetToken(',',S); P.Z := SizeZ * ExpToFloat(St);
      H.Rotate(P);
      PlaceX := PlaceX + P.X;
      PlaceY := PlaceY + P.Y;
      PlaceZ := PlaceZ + P.Z;
      P.Free;
      H.Free;

      // Apply the parent rotation to the one we are about to undertake

      St := GetToken(',',S); RX := ExpToFloat(St);
      St := GetToken(',',S); RY := ExpToFloat(St);
      St := GetToken(',',S); RZ := ExpToFloat(St);
      H0 := THeading.Create(RotateX,RotateY,RotateZ);
      H1 := THeading.Create(RX,RY,RZ);
      H1.Multiply(H0);
      RotateX := H1.FXAngle;
      RotateY := H1.FYAngle;
      RotateZ := H1.FZAngle;
      H1.Free;
      H0.Free;

      St := GetToken(',',S); SizeX   := SizeX   * ExpToFloat(St);
      St := GetToken(',',S); SizeY   := SizeY   * ExpToFloat(St);
      St := GetToken(',',S); SizeZ   := SizeZ   * ExpToFloat(St);

      // Evaluate all of the parameter expressions and call the script

      Parms    := MakeParmList(SO,EvaluateParms(S));
      M        := SO.MakeMeshObject(Nil,Parms);
      Parms.Free;
      PlaceX   := PlaceX0;
      PlaceY   := PlaceY0;
      PlaceZ   := PlaceZ0;
      RotateX  := RotateX0;
      RotateY  := RotateY0;
      RotateZ  := RotateZ0;
      SizeX    := SizeX0;
      SizeY    := SizeY0;
      SizeZ    := SizeZ0;

      // Copy the polygons over

      K := MO.Vertices.Count;
      For J := 0 To M.Polygons.Count - 1 Do
      Begin
        Polygon := TPolygon.Create(TPolygon(M.Polygons.Objects[J]));
        Polygon.Add(K);
        MO.Polygons.AddObject('',Polygon);
      End; // For J

      // Transform the vertices and copy them over

      For J := 0 To M.Vertices.Count - 1 Do MO.Vertices.AddObject('',T3DPoint.Create(T3DPoint(M.Vertices.Objects[J])));
      For J := 0 To M.Normals.Count  - 1 Do MO.Normals.AddObject('',T3DPoint.Create(T3DPoint(M.Normals.Objects[J])));
      M.Free;
    End;
  End; // ParseScript

  Procedure ParseVariableScript(SSV: TScriptStatementVariableScriptCall);
  Var
    I     : Integer;
    V     : TExpressionOp;
    Found : Boolean;
    SSS   : TScriptStatementScriptCall;
    St    : String;

  Begin
    I     := 0;
    Found := False;
    St    := UpperCase(SSV.ScriptName);
    V     := Nil;
    While (I < Variables.Count) And Not Found Do
    Begin
      V := TExpressionOp(Variables.Objects[I]);
      If (V.Data.Operand.OperandType = onScript) And (Variables.Strings[I] = UpperCase(St))
       Then Found := True
       Else Inc(I);
    End; // While
    If Found Then
    Begin
      SSS             := TScriptStatementScriptCall.Create;
      SSS.ScriptName  := V.Data.Operand.Value.St;
      SSS.ScriptParms := SSV.ScriptParms;
      ParseScript(SSS);
      SSS.Free;
    End;
  End; // ParseVariableScript

  Procedure FillInParameters;
  Var
    I,J   : Integer;
    St    : String;
    V     : TExpressionOp;
    Value : TOperandRec;
    B     : Boolean;
    Exp   : TExpression;
    P     : Pointer;

  Begin
    I := 0;
    While (I < Parameters.Count) And Valid Do
    Begin
      St := Parameters.Strings[I];
      If St <> '' Then
      Begin
        P := ParamNames.Objects[I];
        If P = Nil Then
        Begin
          J := Variables.IndexOf(ParamNames.Strings[I]);
          ParamNames.Objects[I] := Pointer(J + 1);
        End
        Else J := Integer(P) - 1;
        If J >= 0 Then
        Begin
          V := TExpressionOp(Variables.Objects[J]);
          If V.Data.Operand.OperandType In [onInteger,onReal,onBoolean] Then
          Begin
            Exp   := TExpression.Create(St);
            Value := Exp.Evaluate(Variables);
            Exp.Free;
          End;

          Case V.Data.Operand.OperandType Of
            onInteger: B := (Value.OperandType = onInteger);
               onReal: B := (Value.OperandType In [onInteger,onReal]);
            onBoolean: B := (Value.OperandType = onBoolean);
             onString,
             onScript: B := True;
          Else
            LogParseError('Error evaluating parameter expression in script ' + Name + ', ' + ErrorText + '.  ' +
                          'Result of expression "' + St + '" must evaluate to an integer, float, boolean, or string.');
            Valid := False;
            B     := False;
          End; // Case
          If B Then
          Begin
            Case V.Data.Operand.OperandType Of
              onInteger: V.Data.Operand.Value.Int   := Value.Value.Int;
                 onReal: If Value.OperandType = onInteger
                          Then V.Data.Operand.Value.Ext := Value.Value.Int
                          Else V.Data.Operand.Value.Ext := Value.Value.Ext;
              onBoolean: V.Data.Operand.Value.Bool  := Value.Value.Bool;
               onString,
               onScript: V.Data.Operand.Value.St    := St;
            End; // Case
          End
          Else
          Begin
            LogParseError('Error evaluating parameter expression in script ' + Name + ', ' + ErrorText + '.  ' +
                          'Could not evaluate expression "' + St + '".');
            Valid := False;
          End;
        End;
      End;
      Inc(I);
    End; // While
  End; // FillInParameters

  Function WasSetInParms(V: TExpressionOp): Boolean;
  Var
    I     : Integer;
    Found : Boolean;
    St    : String;

  Begin
    I     := 0;
    Found := False;
    St    := V.Data.Operand.Name;
    While (I < ParamNames.Count) And Not Found Do
    Begin
      If ParamNames.Strings[I] = St Then Found := True Else Inc(I);
    End; // While
    If Found
     Then Result := (I < Parameters.Count) And (Parameters.Strings[I] <> '')
     Else Result := False;
  End; // WasSetInParms

  Procedure ListActualParmValues;
  Var
    I,J : Integer;
    V   : TExpressionOp;

  Begin
    If Owner <> Nil Then
    Begin
      Owner.Parameters.FParmValues.Clear;
      For I := 0 To ParamNames.Count - 1 Do
      Begin
        J := Variables.IndexOf(ParamNames.Strings[I]);
        If J >= 0 Then
        Begin
          V := TExpressionOp(Variables.Objects[J]);
          Case V.Data.Operand.OperandType Of
            onInteger: Owner.Parameters.FParmValues.Add(IntToStr(V.Data.Operand.Value.Int));
               OnReal: Owner.Parameters.FParmValues.Add(FloatToStr(V.Data.Operand.Value.Ext));
            onBoolean: Owner.Parameters.FParmValues.Add(BoolToStr(V.Data.Operand.Value.Bool,True));
             onString: Owner.Parameters.FParmValues.Add(V.Data.Operand.Value.St);
          End; // Case
        End;
      End; // For I
    End;
  End; // ListActualParmValues

Begin
  MO := TMeshObject.Create(Name,PlaceX,PlaceY,PlaceZ,SizeX,SizeY,SizeZ);

  // Evaluate the parameters to the script

  Valid := True;
  FillInParameters;

  // Parse default assignments to parameters and variables

  I     := 0;
  Valid := True;
  While (I < VDefaults.Count) And Valid Do
  Begin
    S := TScriptStatement(VDefaults.Objects[I]);
    If S.Predicate = spdAssignment Then
    Begin
      ErrorText := 'parameter/variable number ' + IntToStr(I + 1);

      // Only perform the assignment if the variable is uninitialized

      If Not WasSetInParms(TScriptStatementAssignment(S).Dest) Then
       ParseAssignment(TScriptStatementAssignment(S));
    End;
    Inc(I);
  End; // While

  // Build the owner's list of actual parameter values

  ListActualParmValues;

  // Execute the script

  SetLength(WhileStack,0);
  SetLength(RepeatStack,0);
  I := 0;
  While (I < Compiled.Count) And Valid Do
  Begin
    ErrorText := 'statement number ' + IntToStr(I + 1);
    S         := TScriptStatement(Compiled.Objects[I]);

    // Get the command string

    Command := S.Predicate;

    // Execute the statement (ignore ENDIF statements)

    Case Command Of
         spdTriangle: ParseTriangle(TScriptStatementTriangle(S));
      spdTriangleTex: ParseTriangleTex(TScriptStatementTriangle(S));
        spdRectangle: ParseRectangle(TScriptStatementRectangle(S));
     spdRectangleTex: ParseRectangleTex(TScriptStatementRectangle(S));
               spdIf: ParseIf(TScriptStatementSingleExpression(S));
             spdElse: ParseElse;
           spdElseIf: ParseElse; // This is on purpose
            spdWhile: ParseWhile(TScriptStatementSingleExpression(S));
             spdWend: ParseWend;
           spdRepeat: ParseRepeat;
            spdUntil: ParseUntil(TScriptStatementSingleExpression(S));
           spdScript: ParseScript(TScriptStatementScriptCall(S));
   spdVariableScript: ParseVariableScript(TScriptStatementVariableScriptCall(S));
       spdAssignment: ParseAssignment(TScriptStatementAssignment(S));
          spdSQLParm: ParseSQLParm(TScriptStatementSQLParm(S));
    End; // Case
{
    If Not Valid Then
    Begin
      Case Command Of
        spdAssignment: Msg := 'Assignment: Variable = ' + TScriptStatementAssignment(S).Dest.Value.Name;
      Else
        Msg := '';
      End; // Case
      ShowMessage('Invalid on statement ' + IntToStr(I) +
                  ', predicate = ' + IntToStr(Integer(Command)) + #13#10 + Msg);
    End;
}
    Inc(I);
  End; // While
  SetLength(WhileStack,0);
  SetLength(RepeatStack,0);
  Result := MO;
End; // TScripted.MakeMeshObject

Procedure TScripted.Compile;
Var
  I,J         : LongInt;
  Command     : String;
  S           : String;
  OrigS       : String;             
  Valid       : Boolean;
  Found       : Boolean;
  V           : TExpressionOp;
  Line        : String;

  Function IsReservedWord(St: String): Boolean;
  Var
    I     : LongInt;
    Found : Boolean;

  Begin
    I     := 0;
    Found := False;
    St    := UpperCase(St);
    While (I <= High(ReservedWords)) And Not Found Do
    Begin
      If ReservedWords[I] = St Then Found := True Else Inc(I);
    End; // While
    Result := Found;
  End; // IsReservedWord

  Function IsGlobalVariable(St: String): LongInt;
  Var
    I     : LongInt;
    Found : Boolean;

  Begin
    I     := 0;
    Found := False;
    St    := UpperCase(St);
    While (TScriptGlobal(I) <= High(ScriptGlobalNames)) And Not Found Do
    Begin
      If ScriptGlobalNames[TScriptGlobal(I)] = St Then Found := True Else Inc(I);
    End; // While
    If Found Then Result := I Else Result := -1;
  End; // IsGlobalVariable

  Function IsLocalVariable(St: String): LongInt;
  Begin
    Result := Variables.IndexOf(UpperCase(St));
  End; // IsLocalVariable

  Procedure ParseParam;
  Var
    I        : Integer;
    St,St1   : String;
    V        : TExpressionOp;
    SSA      : TScriptStatementAssignment;
    VT       : TVariableType;
    ReadOnly : Boolean;
    Hidden   : Boolean;
    P        : TScriptParam;

  Begin
    SSA      := Nil; // Important! Needed to signal usage
    P        := TScriptParam.Create;
    P.Name   := GetToken(' ',S);
    ReadOnly := False;
    Hidden   := False;
    VT       := vaParameter;
    If (P.Name <> '') And
       (P.Name[1] In ['A'..'Z','a'..'z']) And
       (Not IsReservedWord(P.Name)) And
       (IsGlobalVariable(P.Name) < 0) Then
    Begin
      St := GetToken(' ',S);
      If St = 'INTEGER' Then
      Begin
        P._Type       := sptInteger;
        P.Default.Int := 0;
      End
      Else If St = 'FLOAT' Then
      Begin
        P._Type         := sptFloat;
        P.Default.Float := 0;
      End
      Else If St = 'BOOLEAN' Then
      Begin
        P._Type        := sptBoolean;
        P.Default.Bool := False;
      End
      Else If St = 'STRING' Then
      Begin
        P._Type      := sptString;
        P.Default.St := '';
      End
      Else If St = 'SCRIPT' Then
      Begin
        P._Type      := sptScript;
        P.Default.St := '';
      End
      Else
      Begin
        CompileLog.Add('Error compiling parameter in script ' + Name +
                       '.  Parameter "' + P.Name + '" must be an integer, float, boolean, or string.');
        Valid := False;
      End;
      If Valid Then
      Begin
        St := GetToken(' ',S);
        If St <> '' Then
        Begin
          If St = 'DEFAULT' Then
          Begin
            St := GetToken(' ',S,True);
            If St <> '' Then
            Begin
              If Not IsReservedWord(St) Then
              Begin
                // Look for global variables

                I := IsGlobalVariable(St);
                If I >= 0 Then
                Begin
                  If P._Type = ScriptGlobalTypes[TScriptGlobal(I)] Then
                  Begin
                    SSA := TScriptStatementAssignment.Create;
                    VT  := TVariableType(I);
                  End
                  Else
                  Begin
                    CompileLog.Add('Error compiling parameter in script ' + Name +
                                   '.  Parameter "' + P.Name + '" is the wrong data type.');
                    Valid := False;
                  End;
                End
                Else
                Begin
                  // Look for local variables, parameters, and constants

                  SSA := TScriptStatementAssignment.Create;
                  VT  := vaParameter;
                  St1 := GetToken(' ',S);
                       If St1 = 'READONLY' Then ReadOnly := True
                  Else If St1 = 'HIDDEN'   Then Hidden   := True;
                  St1 := GetToken(' ',S);
                       If St1 = 'READONLY' Then ReadOnly := True
                  Else If St1 = 'HIDDEN'   Then Hidden   := True;
                End;
              End
              Else
              Begin
                CompileLog.Add('Error compiling parameter in script ' + Name +
                               '.  Parameter "' + St + '" is a reserved word.');
                Valid := False;
              End;
            End;
          End
          Else
          Begin
            CompileLog.Add('Error compiling parameter in script ' + Name +
                           '.  Unknown modifier "' + St + '".');
            Valid := False;
          End;
        End;
        If Valid Then
        Begin
          ParamNames.AddObject(P.Name,Nil);
          V := TExpressionOp.CreateVariableOperand(VT,P.Name,ReadOnly,Hidden);
          If SSA <> Nil Then
          Begin
            SSA.Predicate := spdAssignment;
            SSA.Dest      := V;
            SSA.Source    := TExpression.Create(St);
            VDefaults.AddObject(UpperCase(P.Name),SSA);
          End;
          Case P._Type Of
            sptInteger: V.Data.Operand.OperandType := onInteger;
              sptFloat: V.Data.Operand.OperandType := onReal;
            sptBoolean: V.Data.Operand.OperandType := onBoolean;
             sptString: V.Data.Operand.OperandType := onString;
             sptScript: V.Data.Operand.OperandType := onScript;
          End; // Case
          Variables.AddObject(UpperCase(P.Name),V);
        End;
        P.Free;
      End;
    End
    Else
    Begin
           If P.Name = ''                            Then CompileLog.Add('Error compiling parameter declaration in script ' + Name + '.  Empty parameter name.')
      Else If Not (P.Name[1] In ['A'..'Z','a'..'z']) Then CompileLog.Add('Error compiling parameter declaration in script ' + Name + '.  Name of parameter "' + P.Name + '" is invalid.')
      Else If IsReservedWord(P.Name)                 Then CompileLog.Add('Error compiling parameter declaration in script ' + Name + '.  Parameter "' + P.Name + '" is a reserved word.')
      Else If IsGlobalVariable(P.Name) >= 0          Then CompileLog.Add('Error compiling parameter declaration in script ' + Name + '.  Parameter "' + P.Name + '" is a global variable.');
      Valid := False;
    End;
  End; // ParseParam

  Procedure ParseHint;
  Var
    ParamName : String;
    V         : TExpressionOp;
    J         : Integer;
    St        : String;

  Begin
    St := Script.Strings[I - 1]; // I is coming from the parent
    GetToken(' ',St);            // Skip the 'HINT' string
    GetToken(' ',St);            // Skip the parameter name
    ParamName := UpperCase(GetToken(' ',S));
    J         := Variables.IndexOf(ParamName);
    If J >= 0 Then
    Begin
      V := TExpressionOp(Variables.Objects[J]);
      V.HintText.Add(St);
    End;
  End; // ParseHint

  Procedure ParseVariable;
  Var
    I,J     : LongInt;
    R       : Extended;
    B       : Boolean;
    VarName : String;
    St      : String;
    V       : TExpressionOp;
    SSA     : TScriptStatementAssignment;

  Begin
    VarName := GetToken(' ',S);
    If (VarName <> '') And
       (VarName[1] In ['A'..'Z','a'..'z']) And
       (Not IsReservedWord(VarName)) And
       (IsGlobalVariable(VarName) < 0) Then
    Begin
      St := GetToken(' ',S);
      If St = 'INTEGER' Then
      Begin
        St := GetToken(' ',S);
        I  := 0;
        If St <> '' Then
        Begin
          Val(St,I,J);
          If J <> 0 Then
          Begin
            CompileLog.Add('Error compiling variable declaration in script ' + Name +
                           '.  Variable "' + VarName + '": could not evaluate "' + St + '" to an integer.');
            Valid := False;
          End;
        End;
        If Valid Then
        Begin
          V                 := TExpressionOp.CreateVariableOperand(vaVariable,VarName,False,False);
          V.Data.Operand.OperandType := onInteger;
          V.Data.Operand.Value.Int   := I;
          Variables.AddObject(UpperCase(VarName),V);
          If St <> '' Then
          Begin
            SSA                := TScriptStatementAssignment.Create;
            SSA.Predicate      := spdAssignment;
            SSA.Source         := TExpression.Create(St);
            SSA.Dest           := V;
            VDefaults.AddObject(UpperCase(VarName),SSA);
          End;
        End;
      End
      Else If St = 'FLOAT' Then
      Begin
        St := GetToken(' ',S);
        R  := 0;
        If St <> '' Then
        Begin
          Val(St,R,J);
          If J <> 0 Then
          Begin
            CompileLog.Add('Error compiling variable declaration in script ' + Name +
                           '.  Variable "' + VarName + '": could not evaluate "' + St + '" to a float.');
            Valid := False;
          End;
        End;
        If Valid Then
        Begin
          V                          := TExpressionOp.CreateVariableOperand(vaVariable,VarName,False,False);
          V.Data.Operand.OperandType := onReal;
          V.Data.Operand.Value.Ext   := R;
          Variables.AddObject(UpperCase(VarName),V);
          If St <> '' Then
          Begin
            SSA                  := TScriptStatementAssignment.Create;
            SSA.Predicate        := spdAssignment;
            SSA.Source           := TExpression.Create(St);
            SSA.Dest             := V;
            VDefaults.AddObject(UpperCase(VarName),SSA);
          End;
        End;
      End
      Else If St = 'BOOLEAN' Then
      Begin
        St := GetToken(' ',S);
        B  := False;
        If St <> '' Then
        Begin
               If St = 'TRUE'  Then B := True
          Else If St = 'FALSE' Then B := False
          Else
          Begin
            CompileLog.Add('Error compiling variable declaration in script ' + Name +
                           '.  Variable "' + VarName + '" must be either true or false.');
            Valid := False;
          End;
        End;
        If Valid Then
        Begin
          V                          := TExpressionOp.CreateVariableOperand(vaVariable,VarName,False,False);
          V.Data.Operand.OperandType := onBoolean;
          V.Data.Operand.Value.Bool  := B;
          Variables.AddObject(UpperCase(VarName),V);
          If St <> '' Then
          Begin
            SSA                 := TScriptStatementAssignment.Create;
            SSA.Predicate       := spdAssignment;
            SSA.Source          := TExpression.Create(St);
            SSA.Dest            := V;
            VDefaults.AddObject(UpperCase(VarName),SSA);
          End;
        End;
      End
      Else
      Begin
        CompileLog.Add('Error compiling variable declaration in script ' + Name +
                       '.  Variable "' + VarName + '": must be either an integer, float, or boolean.');
        Valid := False;
      End;
    End
    Else
    Begin
           If VarName = ''                            Then CompileLog.Add('Error compiling variable declaration in script ' + Name + '.  Empty variable name.')
      Else If Not (VarName[1] In ['A'..'Z','a'..'z']) Then CompileLog.Add('Error compiling variable declaration in script ' + Name + '.  Name of variable "' + VarName + '" is invalid.')
      Else If IsReservedWord(VarName)                 Then CompileLog.Add('Error compiling variable declaration in script ' + Name + '.  Variable "' + VarName + '" is a reserved word.')
      Else If IsGlobalVariable(VarName) >= 0          Then CompileLog.Add('Error compiling variable declaration in script ' + Name + '.  Variable "' + VarName + '" is a global variable.');
      Valid := False;
    End;
  End; // ParseVariable

  Procedure ParseTriangle;
  Var
    I        : LongInt;
    St       : String;
    SST      : TScriptStatementTriangle;
    Original : String;

  Begin
    I             := 0;
    SST           := TScriptStatementTriangle.Create;
    SST.Predicate := spdTriangle;
    Original      := S;
    While (I < 26) And Valid Do
    Begin
      St := GetToken(',',S);
      If St <> '' Then
      Begin
        Case I Of
          0: SST.X1        := TExpression.Create(St);
          1: SST.Y1        := TExpression.Create(St);
          2: SST.Z1        := TExpression.Create(St);
          3: SST.X2        := TExpression.Create(St);
          4: SST.Y2        := TExpression.Create(St);
          5: SST.Z2        := TExpression.Create(St);
          6: SST.X3        := TExpression.Create(St);
          7: SST.Y3        := TExpression.Create(St);
          8: SST.Z3        := TExpression.Create(St);
          9: SST.Tex       := TExpression.Create(St);
         10: SST.Trans     := TExpression.Create(St);
         11: SST.STrans    := TExpression.Create(St);
         12: SST.Solid     := TExpression.Create(St);
         13: SST.Color     := TExpression.Create(St);
         14: SST.HasColor  := TExpression.Create(St);
         15: SST.Masked    := TExpression.Create(St);
         16: SST.HasNormal := TExpression.Create(St);
         17: SST.NX1       := TExpression.Create(St);
         18: SST.NY1       := TExpression.Create(St);
         19: SST.NZ1       := TExpression.Create(St);
         20: SST.NX2       := TExpression.Create(St);
         21: SST.NY2       := TExpression.Create(St);
         22: SST.NZ2       := TExpression.Create(St);
         23: SST.NX3       := TExpression.Create(St);
         24: SST.NY3       := TExpression.Create(St);
         25: SST.NZ3       := TExpression.Create(St);
        End; // Case
      End
      Else
      Begin
        If I < 10 Then
        Begin
          CompileLog.Add('Error compiling TRIANGLE statement in script ' + Name + '.  ' +
                         'Parameter number ' + IntToStr(I + 1) + ' is empty.'#13#10 +
                         'Command: TRIANGLE ' + Original);
          Valid := False;
        End;
      End;
      Inc(I);
    End; // While
    If Valid
     Then Compiled.AddObject('',SST)
     Else SST.Free;
  End; // ParseTriangle

  Procedure ParseTriangleTex;
  Var
    I        : LongInt;
    St       : String;
    SST      : TScriptStatementTriangle;
    Original : String;

  Begin
    I             := 0;
    SST           := TScriptStatementTriangle.Create;
    SST.Predicate := spdTriangleTex;
    Original      := S;
    While (I < 32) And Valid Do
    Begin
      St := GetToken(',',S);
      If St <> '' Then
      Begin
        Case I Of
          0: SST.X1        := TExpression.Create(St);
          1: SST.Y1        := TExpression.Create(St);
          2: SST.Z1        := TExpression.Create(St);
          3: SST.X2        := TExpression.Create(St);
          4: SST.Y2        := TExpression.Create(St);
          5: SST.Z2        := TExpression.Create(St);
          6: SST.X3        := TExpression.Create(St);
          7: SST.Y3        := TExpression.Create(St);
          8: SST.Z3        := TExpression.Create(St);
          9: SST.TX1       := TExpression.Create(St);
         10: SST.TZ1       := TExpression.Create(St);
         11: SST.TX2       := TExpression.Create(St);
         12: SST.TZ2       := TExpression.Create(St);
         13: SST.TX3       := TExpression.Create(St);
         14: SST.TZ3       := TExpression.Create(St);
         15: SST.Tex       := TExpression.Create(St);
         16: SST.Trans     := TExpression.Create(St);
         17: SST.STrans    := TExpression.Create(St);
         18: SST.Solid     := TExpression.Create(St);
         19: SST.Color     := TExpression.Create(St);
         20: SST.HasColor  := TExpression.Create(St);
         21: SST.Masked    := TExpression.Create(St);
         22: SST.HasNormal := TExpression.Create(St);
         23: SST.NX1       := TExpression.Create(St);
         24: SST.NY1       := TExpression.Create(St);
         25: SST.NZ1       := TExpression.Create(St);
         26: SST.NX2       := TExpression.Create(St);
         27: SST.NY2       := TExpression.Create(St);
         28: SST.NZ2       := TExpression.Create(St);
         29: SST.NX3       := TExpression.Create(St);
         30: SST.NY3       := TExpression.Create(St);
         31: SST.NZ3       := TExpression.Create(St);
        End; // Case
      End
      Else
      Begin
        If I < 16 Then
        Begin
          CompileLog.Add('Error compiling TRIANGLETEX statement in script ' + Name + '.  ' +
                         'Parameter number ' + IntToStr(I + 1) + ' is empty.'#13#10 +
                         'Command: TRIANGLETEX ' + Original);
          Valid := False;
        End;
      End;
      Inc(I);
    End; // While
    If Valid
     Then Compiled.AddObject('',SST)
     Else SST.Free;
  End; // ParseTriangleTex

  Procedure ParseRectangle;
  Var
    I        : LongInt;
    St       : String;
    SSR      : TScriptStatementRectangle;
    Original : String;

  Begin
    I             := 0;
    SSR           := TScriptStatementRectangle.Create;
    SSR.Predicate := spdRectangle;
    Original      := S;
    While (I < 32) And Valid Do
    Begin
      St := GetToken(',',S);
      If St <> '' Then
      Begin
        Case I Of
          0: SSR.X1        := TExpression.Create(St);
          1: SSR.Y1        := TExpression.Create(St);
          2: SSR.Z1        := TExpression.Create(St);
          3: SSR.X2        := TExpression.Create(St);
          4: SSR.Y2        := TExpression.Create(St);
          5: SSR.Z2        := TExpression.Create(St);
          6: SSR.X3        := TExpression.Create(St);
          7: SSR.Y3        := TExpression.Create(St);
          8: SSR.Z3        := TExpression.Create(St);
          9: SSR.X4        := TExpression.Create(St);
         10: SSR.Y4        := TExpression.Create(St);
         11: SSR.Z4        := TExpression.Create(St);
         12: SSR.Tex       := TExpression.Create(St);
         13: SSR.Trans     := TExpression.Create(St);
         14: SSR.STrans    := TExpression.Create(St);
         15: SSR.Solid     := TExpression.Create(St);
         16: SSR.Color     := TExpression.Create(St);
         17: SSR.HasColor  := TExpression.Create(St);
         18: SSR.Masked    := TExpression.Create(St);
         19: SSR.HasNormal := TExpression.Create(St);
         20: SSR.NX1       := TExpression.Create(St);
         21: SSR.NY1       := TExpression.Create(St);
         22: SSR.NZ1       := TExpression.Create(St);
         23: SSR.NX2       := TExpression.Create(St);
         24: SSR.NY2       := TExpression.Create(St);
         25: SSR.NZ2       := TExpression.Create(St);
         26: SSR.NX3       := TExpression.Create(St);
         27: SSR.NY3       := TExpression.Create(St);
         28: SSR.NZ3       := TExpression.Create(St);
         29: SSR.NX4       := TExpression.Create(St);
         30: SSR.NY4       := TExpression.Create(St);
         31: SSR.NZ4       := TExpression.Create(St);
        End; // Case
      End
      Else
      Begin
        If I < 13 Then
        Begin
          CompileLog.Add('Error compiling RECTANGLE statement in script ' + Name + '.  ' +
                         'Parameter number ' + IntToStr(I + 1) + ' is empty.'#13#10 +
                         'Command: RECTANGLE ' + Original);
          Valid := False;
        End;
      End;
      Inc(I);
    End; // While
    If Valid
     Then Compiled.AddObject('',SSR)
     Else SSR.Free;
  End; // ParseRectangle

  Procedure ParseRectangleTex;
  Var
    I        : LongInt;
    St       : String;
    SSR      : TScriptStatementRectangle;
    Original : String;

  Begin
    I             := 0;
    SSR           := TScriptStatementRectangle.Create;
    SSR.Predicate := spdRectangleTex;
    Original      := S;
    While (I < 40) And Valid Do
    Begin
      St := GetToken(',',S);
      If St <> '' Then
      Begin
        Case I Of
          0: SSR.X1        := TExpression.Create(St);
          1: SSR.Y1        := TExpression.Create(St);
          2: SSR.Z1        := TExpression.Create(St);
          3: SSR.X2        := TExpression.Create(St);
          4: SSR.Y2        := TExpression.Create(St);
          5: SSR.Z2        := TExpression.Create(St);
          6: SSR.X3        := TExpression.Create(St);
          7: SSR.Y3        := TExpression.Create(St);
          8: SSR.Z3        := TExpression.Create(St);
          9: SSR.X4        := TExpression.Create(St);
         10: SSR.Y4        := TExpression.Create(St);
         11: SSR.Z4        := TExpression.Create(St);
         12: SSR.TX1       := TExpression.Create(St);
         13: SSR.TZ1       := TExpression.Create(St);
         14: SSR.TX2       := TExpression.Create(St);
         15: SSR.TZ2       := TExpression.Create(St);
         16: SSR.TX3       := TExpression.Create(St);
         17: SSR.TZ3       := TExpression.Create(St);
         18: SSR.TX4       := TExpression.Create(St);
         19: SSR.TZ4       := TExpression.Create(St);
         20: SSR.Tex       := TExpression.Create(St);
         21: SSR.Trans     := TExpression.Create(St);
         22: SSR.STrans    := TExpression.Create(St);
         23: SSR.Solid     := TExpression.Create(St);
         24: SSR.Color     := TExpression.Create(St);
         25: SSR.HasColor  := TExpression.Create(St);
         26: SSR.Masked    := TExpression.Create(St);
         27: SSR.HasNormal := TExpression.Create(St);
         28: SSR.NX1       := TExpression.Create(St);
         29: SSR.NY1       := TExpression.Create(St);
         30: SSR.NZ1       := TExpression.Create(St);
         31: SSR.NX2       := TExpression.Create(St);
         32: SSR.NY2       := TExpression.Create(St);
         33: SSR.NZ2       := TExpression.Create(St);
         34: SSR.NX3       := TExpression.Create(St);
         35: SSR.NY3       := TExpression.Create(St);
         36: SSR.NZ3       := TExpression.Create(St);
         37: SSR.NX4       := TExpression.Create(St);
         38: SSR.NY4       := TExpression.Create(St);
         39: SSR.NZ4       := TExpression.Create(St);
        End; // Case
      End
      Else
      Begin
        If I < 21 Then
        Begin
          CompileLog.Add('Error compiling RECTANGLETEX statement in script ' + Name + '.  ' +
                         'Parameter number ' + IntToStr(I + 1) + ' is empty.'#13#10 +
                         'Command: RECTANGLETEX ' + Original);
          Valid := False;
        End;
      End;
      Inc(I);
    End; // While
    If Valid
     Then Compiled.AddObject('',SSR)
     Else SSR.Free;
  End; // ParseRectangleTex

  Procedure ParseAssignment(Dest: TExpressionOp);
  Var SSA: TScriptStatementAssignment;
  Begin
    If (S <> '') And (S[1] = '=') Then
    Begin
      S             := Trim(Copy(S,2,Length(S) - 1));
      SSA           := TScriptStatementAssignment.Create;
      SSA.Predicate := spdAssignment;
      SSA.Source    := TExpression.Create(S);
      SSA.Dest      := Dest;
      Compiled.AddObject('',SSA);
    End
    Else
    Begin
      If S = ''
       Then CompileLog.Add('Error compiling assignment statement in script ' + Name + '.  Empty statement.')
       Else CompileLog.Add('Error compiling assignment statement in script ' + Name + '.  No equal sign found.');
      Valid := False;
    End;  
  End; // ParseAssignment

  Procedure ParseSQLParm;
  Var
    SQP      : TScriptStatementSQLParm;
    St       : String;
    St1      : String;
    Original : String;
    I        : Integer;

  Begin
    SQP           := TScriptStatementSQLParm.Create;
    SQP.Predicate := spdSQLParm;
    I             := 0;
    Original      := S;
    While (I < 3) And Valid Do
    Begin
      St  := GetToken(',',S,True);
      St1 := GetToken(',',OrigS,True);
      If Copy(St,1,1) = '"' Then St := St1;
      If St <> '' Then
      Begin
        Case I Of
          0: SQP.Table  := TExpression.Create(St);
          1: SQP.Column := TExpression.Create(St);
          2: SQP.Value  := TExpression.Create(St);
        End; // Case
      End
      Else
      Begin
        If I < 3 Then
        Begin
          CompileLog.Add('Error compiling SQLPARM statement in script ' + Name + '.  ' +
                         'Parameter number ' + IntToStr(I + 1) + ' is empty.'#13#10 +
                         'Command: SQLPARM ' + Original);
          Valid := False;
        End;
      End;
      Inc(I);
    End; // While
    If Valid
     Then Compiled.AddObject('',SQP)
     Else SQP.Free;
  End; // ParseSQLParm

  Procedure ParseIf;
  Var SSI: TScriptStatementSingleExpression;
  Begin
    SSI           := TScriptStatementSingleExpression.Create;
    SSI.Predicate := spdIf;
    SSI.E         := TExpression.Create(S);
    Compiled.AddObject('',SSI);
  End; // ParseIf

  Procedure ParseElseIf;
  Var SSE: TScriptStatementSingleExpression;
  Begin
    SSE           := TScriptStatementSingleExpression.Create;
    SSE.Predicate := spdElseIf;
    SSE.E         := TExpression.Create(S);
    Compiled.AddObject('',SSE);
  End; // ParseElseIf

  Procedure ParseElse;
  Var SS: TScriptStatement;
  Begin
    SS           := TScriptStatement.Create;
    SS.Predicate := spdElse;
    Compiled.AddObject('',SS);
  End; // ParseElse

  Procedure ParseEndIf;
  Var SS: TScriptStatement;
  Begin
    SS           := TScriptStatement.Create;
    SS.Predicate := spdEndIf;
    Compiled.AddObject('',SS);
  End; // ParseEndIf

  Procedure ParseWhile;
  Var SSW: TScriptStatementSingleExpression;
  Begin
    SSW           := TScriptStatementSingleExpression.Create;
    SSW.Predicate := spdWhile;
    SSW.E         := TExpression.Create(S);
    Compiled.AddObject('',SSW);
  End; // ParseWhile

  Procedure ParseWend;
  Var SS: TScriptStatement;
  Begin
    SS           := TScriptStatement.Create;
    SS.Predicate := spdWend;
    Compiled.AddObject('',SS);
  End; // ParseWend

  Procedure ParseRepeat;
  Var SS: TScriptStatement;
  Begin
    SS           := TScriptStatement.Create;
    SS.Predicate := spdRepeat;
    Compiled.AddObject('',SS);
  End; // ParseRepeat

  Procedure ParseUntil;
  Var SSU: TScriptStatementSingleExpression;
  Begin
    SSU           := TScriptStatementSingleExpression.Create;
    SSU.Predicate := spdUntil;
    SSU.E         := TExpression.Create(S);
    Compiled.AddObject('',SSU);
  End; // ParseUntil

  Procedure ParseScript;
  Var SSS: TScriptStatementScriptCall;
  Begin
    SSS             := TScriptStatementScriptCall.Create;
    SSS.Predicate   := spdScript;
    SSS.ScriptName  := GetToken(' ',S);
    SSS.ScriptParms := S;
    Compiled.AddObject('',SSS);
  End; // ParseScript

  Procedure ParseVariableScript;
  Var SSV: TScriptStatementVariableScriptCall;
  Begin
    SSV             := TScriptStatementVariableScriptCall.Create;
    SSV.Predicate   := spdVariableScript;
    SSV.ScriptName  := GetToken(' ',S);
    SSV.ScriptParms := S;
    Compiled.AddObject('',SSV);
  End; // ParseVariableScript

  Function GetCommand(Var I: Integer): String;
  Var
    Done    : Boolean;
    S,St    : String;
    J,K     : Integer;
    Command : String;

  Begin
    Done  := False;
    S     := '';
    OrigS := '';
    K     := I;
    While (I < Script.Count) And Not Done Do
    Begin
      St := Trim(UpperCase(Script[I])); // The scripting language is case INSENSITIVE
      J  := Pos('''',St);               // Strip out trailing remarks
      If J <> 0 Then St := Trim(Copy(St,1,J - 1));
      If Copy(St,Length(St),1) = ';' Then Done := True;
      If S = '' Then
      Begin
        S     := St;
        OrigS := Copy(Script[I],1,Length(St));
      End
      Else
      Begin
        S := S + ' ' + St;
        OrigS := OrigS + ' ' + Copy(Script[I],1,Length(St));
      End;

      // Only script calls need terminating semicolons

      If K = I Then
      Begin
        Command := GetToken(' ',St);
        If Command <> 'SCRIPT' Then Done := True;
      End;  
      Inc(I);
    End; // While
    If Copy(S,Length(S),1) = ';' Then
    Begin
      S     := Copy(S,1,Length(S) - 1);
      OrigS := Copy(OrigS,1,Length(OrigS) - 1);
    End;
    Result := S;
  End; // GetCommand

Begin
  I        := 0;
  Valid    := True;
  While (I < Script.Count) And Valid Do
  Begin
    S    := GetCommand(I);
    Line := S;

    // Get the command string

    Command := GetToken(' ',S);
    GetToken(' ',OrigS);

    // Don't parse empty lines

    If Command <> '' Then
    Begin
           If Command = 'PARAM'             Then ParseParam
      Else If Command = 'VARIABLE'          Then ParseVariable
      Else If Command = 'TRIANGLE'          Then ParseTriangle
      Else If Command = 'TRIANGLETEX'       Then ParseTriangleTex
      Else If Command = 'RECTANGLE'         Then ParseRectangle
      Else If Command = 'RECTANGLETEX'      Then ParseRectangleTex
      Else If Command = 'SQLPARM'           Then ParseSQLParm
      Else If Command = 'IF'                Then ParseIf
      Else If Command = 'ELSE'              Then ParseElse
      Else If Command = 'ELSEIF'            Then ParseElseIf
      Else If Command = 'ENDIF'             Then ParseEndIf
      Else If Command = 'WHILE'             Then ParseWhile
      Else If Command = 'WEND'              Then ParseWend
      Else If Command = 'REPEAT'            Then ParseRepeat
      Else If Command = 'UNTIL'             Then ParseUntil
      Else If Command = 'SCRIPT'            Then ParseScript
      Else If Command = 'VARSCRIPT'         Then ParseVariableScript
      Else If Command = 'CATEGORY'          Then Category := UpperCase(S) // Doesn't get compiled
      Else If Command = 'HINT'              Then ParseHint
      Else
      Begin
        // It may be an assignment statement; look for the variable in the
        // variable list

        J       := 0;
        Found   := False;
        V       := Nil;
        While (J < Variables.Count) And Not Found Do
        Begin
          V := TExpressionOp(Variables.Objects[J]);
          If Command = V.Data.Operand.Name + '=' Then
          Begin
            Found := True;
            S := '=' + S;
            OrigS := '=' + OrigS;
          End
          Else If Command = V.Data.Operand.Name Then Found := True
          Else Inc(J);
        End; // While
        If Found Then ParseAssignment(V) Else Valid := False;
      End;
      If Not Valid Then CompileLog.Add('Error compiling ' + name + '. Line = "' + Line + '"');

//      If Not Valid Then ShowMessage('INVALID: line ' + IntToStr(I) + ' ' + Command + ' >' + S + '<');
    End;
  End; // While
End; // TScripted.Compile

Function TScripted.GetParamType(Param: String): TScriptParamType;
Var
  I     : Integer;
  Found : Boolean;
  V     : TExpressionOp;

Begin
  I     := 0;
  Found := False;
  Param := UpperCase(Trim(Param));
  V     := Nil;
  While (I < Variables.Count) And Not Found Do
  Begin
    V := TExpressionOp(Variables.Objects[I]);
    If Trim(UpperCase(V.Data.Operand.Name)) = Param Then Found := True Else Inc(I);
  End; // While
  If Found Then
  Begin
    Case V.Data.Operand.OperandType Of
      onInteger: Result := sptInteger;
         onReal: Result := sptFloat;
      onBoolean: Result := sptBoolean;
       onString: Result := sptString;
       onScript: Result := sptScript;
    Else
      Result := sptInteger;
    End; // Case
  End
  Else Result := sptInteger;
End; // TScripted.GetParamType

Procedure FreeListAndItsObjects(L: TStringList);
Var I: Integer;
Begin
  For I := 0 To L.Count - 1 Do
  Begin
    If L.Objects[I] <> Nil Then
    Begin
      If L.Objects[I] Is TStringList
       Then FreeListAndItsObjects(TStringList(L.Objects[I]))
       Else L.Objects[I].Free;
    End;
  End; // For I
  L.Free;
End; // FreeListAndItsObjects

// ------------------------------
// TScriptStatementTriangle
// ------------------------------

Constructor TScriptStatementTriangle.Create;
Begin
  Inherited;
  X1        := Nil;
  Y1        := Nil;
  Z1        := Nil;
  X2        := Nil;
  Y2        := Nil;
  Z2        := Nil;
  X3        := Nil;
  Y3        := Nil;
  Z3        := Nil;
  TX1       := Nil;
  TZ1       := Nil;
  TX2       := Nil;
  TZ2       := Nil;
  TX3       := Nil;
  TZ3       := Nil;
  NX1       := Nil;
  NY1       := Nil;
  NZ1       := Nil;
  NX2       := Nil;
  NY2       := Nil;
  NZ2       := Nil;
  NX3       := Nil;
  NY3       := Nil;
  NZ3       := Nil;
  Tex       := Nil;
  Trans     := Nil;
  STrans    := Nil;
  Solid     := Nil;
  Color     := Nil;
  HasColor  := Nil;
  Masked    := Nil;
  HasNormal := Nil;
End; // TScriptStatementTriangle.Create

Destructor TScriptStatementTriangle.Destroy;
Begin
  X1.Free;
  Y1.Free;
  Z1.Free;
  X2.Free;
  Y2.Free;
  Z2.Free;
  X3.Free;
  Y3.Free;
  Z3.Free;
  TX1.Free;
  TZ1.Free;
  TX2.Free;
  TZ2.Free;
  TX3.Free;
  TZ3.Free;
  NX1.Free;
  NY1.Free;
  NZ1.Free;
  NX2.Free;
  NY2.Free;
  NZ2.Free;
  NX3.Free;
  NY3.Free;
  NZ3.Free;
  Tex.Free;
  Trans.Free;
  STrans.Free;
  Solid.Free;
  Color.Free;
  HasColor.Free;
  Masked.Free;
  HasNormal.Free;
  Inherited;
End; // TScriptStatementTriangle.Destroy

// ------------------------------
// TScriptStatementRectangle
// ------------------------------

Constructor TScriptStatementRectangle.Create;
Begin
  Inherited;
  X1        := Nil;
  Y1        := Nil;
  Z1        := Nil;
  X2        := Nil;
  Y2        := Nil;
  Z2        := Nil;
  X3        := Nil;
  Y3        := Nil;
  Z3        := Nil;
  X4        := Nil;
  Y4        := Nil;
  Z4        := Nil;
  TX1       := Nil;
  TZ1       := Nil;
  TX2       := Nil;
  TZ2       := Nil;
  TX3       := Nil;
  TZ3       := Nil;
  TX4       := Nil;
  TZ4       := Nil;
  NX1       := Nil;
  NY1       := Nil;
  NZ1       := Nil;
  NX2       := Nil;
  NY2       := Nil;
  NZ2       := Nil;
  NX3       := Nil;
  NY3       := Nil;
  NZ3       := Nil;
  NX4       := Nil;
  NY4       := Nil;
  NZ4       := Nil;
  Tex       := Nil;
  Trans     := Nil;
  STrans    := Nil;
  Solid     := Nil;
  Color     := Nil;
  HasColor  := Nil;
  Masked    := Nil;
  HasNormal := Nil;
End; // TScriptStatementRectangle.Create

Destructor TScriptStatementRectangle.Destroy;
Begin
  X1.Free;
  Y1.Free;
  Z1.Free;
  X2.Free;
  Y2.Free;
  Z2.Free;
  X3.Free;
  Y3.Free;
  Z3.Free;
  X4.Free;
  Y4.Free;
  Z4.Free;
  TX1.Free;
  TZ1.Free;
  TX2.Free;
  TZ2.Free;
  TX3.Free;
  TZ3.Free;
  TX4.Free;
  TZ4.Free;
  NX1.Free;
  NY1.Free;
  NZ1.Free;
  NX2.Free;
  NY2.Free;
  NZ2.Free;
  NX3.Free;
  NY3.Free;
  NZ3.Free;
  NX4.Free;
  NY4.Free;
  NZ4.Free;
  Tex.Free;
  Trans.Free;
  STrans.Free;
  Solid.Free;
  Color.Free;
  HasColor.Free;
  Masked.Free;
  HasNormal.Free;
  Inherited;
End; // TScriptStatementRectangle.Destroy

// ------------------------------
// TScriptSQLParm
// ------------------------------

Constructor TScriptStatementSQLParm.Create;
Begin
  Inherited;
  Table  := Nil;
  Column := Nil;
  Value  := Nil;
End; // TScriptStatementSQLParm.Create

Destructor TScriptStatementSQLParm.Destroy;
Begin
  Table.Free;
  Column.Free;
  Value.Free;
  Inherited;
End; // TScriptStatementSQLParm.Destroy

// ------------------------------
// TScriptStatementSingleExpression
// ------------------------------

Constructor TScriptStatementSingleExpression.Create;
Begin
  Inherited;
  E := Nil;
End; // TScriptStatementSingleExpression.Create

Destructor TScriptStatementSingleExpression.Destroy;
Begin
  E.Free;
    Inherited;
End; // TScriptStatementSingleExpression.Destroy

// ------------------------------
// TScriptStatementAssignment
// ------------------------------

Constructor TScriptStatementAssignment.Create;
Begin
  Inherited;
  Source := Nil;
  Dest   := Nil;
End; // TScriptStatementAssignment.Create

Destructor TScriptStatementAssignment.Destroy;
Begin
  Source.Free;
  Inherited;
End; // TScriptStatementAssignment.Destroy

Procedure ConvertAll;
Var
  S : TSearchRec;
  B : TBitmap;

Begin
  B := TBitmap.Create;
  If FindFirst(ExtractFilePath(Application.EXEName) + 'library/textures/ecommons/*.BMP',faAnyFile,S) = 0 Then
  Begin
    Repeat
      B.LoadFromFile(ExtractFilePath(Application.EXEName) + 'library/textures/ecommons/' + S.Name);
//      B.PixelFormat := pf32Bit;
      B.SaveToFile(ExtractFilePath(Application.EXEName) + 'library/textures/ecommons/' + S.Name);
    Until FindNext(S) <> 0;
  End;
  FindClose(S);
  B.Free;
End; // ConvertAll

Procedure TestHeading;
Var H1,H2: THeading;
Begin
  H1 := THeading.Create(270,0,0);
  H2 := THeading.Create(0,0,30);
  H1.Multiply(H2);
  H1.Free;
  H2.Free;
End; // TestHeading;

Initialization
//  RegisterPropertyEditor(TypeInfo(Boolean),Nil,'',TBooleanProperty);
  RegisterPropertyEditor(TypeInfo(T3DPoint),Nil,'',T3DPointProperty);
  RegisterPropertyEditor(TypeInfo(THeading),Nil,'',THeadingProperty);
  RegisterPropertyEditor(TypeInfo(TSQLRef),Nil,'',TSQLRefProperty);
  RegisterPropertyEditor(TypeInfo(TScriptedObjectParameters),Nil,'',TScriptedObjectParametersProperty);
  ConvertAll;
  CompileLog := TStringList.Create;
  ParseLog   := TStringList.Create;
  LoadScriptLibrary;
  LoadTextureLibraries;
  LoadMeshAndLightLibrary;
  LoadCreatureLibrary;
  PlaceX  := 0;
  PlaceY  := 0;
  PlaceZ  := 0;
  RotateX := 0;
  RotateY := 0;
  RotateZ := 0;
  SizeX   := 1;
  SizeY   := 1;
  SizeZ   := 1;

//  TestHeading;

Finalization
  CompileLog.Free;
  ParseLog.Free;
  FreeScriptLibrary;
  FreeTextureLibraries;
  FreeMeshAndLightLibrary;
  FreeCreatureLibrary;
End.
