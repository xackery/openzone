{this unit has a group of useful routines for use with OpenGL}
{  TSceneGL: a 3D scene, has a group of entities and a group of lights.
   TLight: a light, has color, position and orientation.
   T3DMouse: enables a normal mouse to interact easily with one 3D object

   By: Ricardo Sarmiento
       delphiman@hotmail.com
}

Unit URickGL;

Interface

Uses Windows, Messages, Dialogs, SysUtils, Classes, Forms, ExtCtrls, Controls, SyncObjs, Graphics, GLU,
     U3DPolys, Points3D,{ Points3D_X86, Points3D_SSE2, Points3D_SSE3,} MyHashes, Sorter, dglOpenGL, Exentia, Exentia_Extension, DXMath3,Threads;

Const
  ReplaceTexSetID = 99;

  clSpot      = 1;    // a spot light, with position and direction, like a flashlamp
  clAmbiental = 2;    // an ambiental light, like indirect ilumination
  clStar      = 3;    // a light with position but omnidirectional, like a star

  clConstant  = 1;    // constant light attenuation
  clLinear    = 2;    // linear light attenuation
  clQuadratic = 3;    // quadratic light attenuation

  // TFace flags

  ffAlphaMask = $FF;
  ffHidden    = $10000000;
  ffHasTrans  = $20000000;
  ffSolid     = $40000000;
  ffHasAlpha  = $80000000;

Type
  TRenderMessageType = (rmRedraw,rmInitDC,rmInitRC,rmReleaseRC,rmUpdateArea,rmSetActive,rmLoadTexture,
                        rmAddLight,rmFreeTexture,rmFreeEntity,rmFreeAllTextures,rmFreeAllEntities,
                        rmMakeImagePanel);
  TLogProc = Procedure(St: String);

Const
  RenderMessageID : Array[TRenderMessageType] of String = ('rmRedraw','rmInitDC','rmInitRC','rmReleaseRC','rmUpdateArea','rmSetActive','rmLoadTexture',
                                                           'rmAddLight','rmFreeTexture','rmFreeEntity','rmFreeAllTextures','rmFreeAllEntities',
                                                           'rmMakeImagePanel');

Type
  SPtr = ^Single;
  IPtr = ^Integer;
  PPanel = ^TPanel;

  TSceneGL = Class;

  TRenderMessage = Class
    Msg  : TRenderMessageType;
    L1   : LongInt;
    L2   : LongInt;
    L3   : LongInt;
    L4   : LongInt;
    Done : Boolean;
    Wait : Boolean;
    Constructor Create(AType: TRenderMessageType; P1,P2,P3,P4: LongInt; WaitUntilFinished: Boolean);
  End;

  TRenderThread = Class(TThread)
  Protected
    FRunning          : Boolean;
    FSceneGL          : TSceneGL;
    FMessages         : TStringList;
    FQueueMutex       : TCriticalSection;
    RenderThreadMutex : TCriticalSection;
    FWindowHandle     : THandle;
    FFreeTexQueue     : TStringList;
    Procedure   AddMessage(Msg: TRenderMessage; AllowDuplicates: Boolean);
    Procedure   ProcessMessage(Msg: TRenderMessage);
  Public
    Constructor Create(SceneGL: TSceneGL);
    Destructor  Destroy; Override;
    Procedure   Execute; Override;
    Procedure   Redraw;
    Procedure   InitDC;
    Procedure   InitRC;
    Procedure   ReleaseRC;
    Procedure   UpdateArea(Left,Top,Width,Height: Integer);
    Procedure   SetActive(B: Boolean);
    Procedure   LoadTexture(Texture: TTexture);
//    Procedure   AddLight(Num: Integer);
    Procedure   FreeTexture(Texture: TTexture);
    Procedure   FreeEntity(Entity: TEntity);
    Procedure   FreeAllTextures;
    Procedure   FreeAllEntities;
    Procedure   QueueFreeTextures(List: TStringList);
    Procedure   MakeImagePanel(Panel: PPanel);
    Procedure   SetWindowHandle(iHandle: THandle);
    Function    ContainsMessage(AType: TRenderMessageType): Boolean;
    Property    Running: Boolean Read FRunning;
  End;

  PBGRA = ^TBGRA;
  TBGRA = Packed Record
    B,G,R,A: Byte;
  End;

  TVertex = Packed Record
//    Position    : T3DPoint;
//    Normal      : T3DPoint;
    TX,TZ       : Single;            // Texture X and Z coordinates
//    Color       : TColor;            // vertex´s color, rgba  -- Must be bytes and in this order for glColor4ubv() to work
//    Constructor Create; Overload;
//    Constructor Create(Vertex: TVertex); Overload;
//    Destructor  Destroy; Override;
//    Procedure   Copy(Vertex: TVertex);
  End;

  TFace = Record//Class
//    Vertices : Array[0..2] Of Integer;
//    NumVerts : Integer;
    Texture  : Integer;      // Index into renderable's textureset list
    Flags    : LongWord;     // Alpha in low 8 bits
//    Normal   : T3DPoint;
//    Friction : Single;
//    HasAlpha : Boolean;
//    Alpha    : Byte;
//    Solid    : Boolean;
//    CircumSphere : TSphere;
//    Constructor Create;
//    Destructor  Destroy; Override;
//    Procedure   CalculateCircumSphere(P0,P1,P2: T3DPoint);
//    Procedure   Flip;
  End;

  TRGBAFloat = Packed Record
    R,G,B,A: GLFloat;
  End;

  TModelColorType = (mccAmbientAndDiffuse,mccEmission);

  TRenderable = Class;
  TSkeletonFrame = Class;

  TTextureList = Array Of TTexture;

  TTextureRec = Record
    Index    : Integer; // Index into the texture list
    Count    : Integer; // The number of consecutive textures to use from the list, starting at index
    Interval : Integer; // Time in milliseconds to render each texture in the list
    CurIndex : Integer; // The currently rendered index, based on time
    MatID    : Integer; // Material ID (-1 for none) 
  End;

  TTextureSet = Class
  Protected
    FTextures : TTextureList;
    FInfo     : Array Of TTextureRec;
    FOwner    : TSceneGL;
  Public
    Constructor Create(AOwner: TSceneGL); Overload;
    Constructor Create(TextureSet: TTextureSet); Overload;
    Destructor  Destroy; Override;
    Procedure   AddTextures(Const Textures: TTextureList; Const Interval: Integer);
    Procedure   ReplaceTextures(Const Index: Integer; Const Textures: TTextureList; Const Interval: Integer);
    Function    GetTexture(Index: Integer): TTexture;
    Function    GetMaterialID(Index: Integer): Integer;
    Procedure   SetCurIndices;
    Function    NumTextures: Integer;
    Procedure   CopyFrom(TextureSet: TTextureSet);
  End;

  // Materials are used with shaders

  TMaterial = Class
  Protected
    FOwner    : TSceneGL;
  Public
    Name           : String;
    VertexShader   : String;
    FragmentShader : String;
    Shininess      : Single;
    Constructor Create; Overload;
    Constructor Create(AOwner: TSceneGL); Overload;
    Procedure   Activate;
  End;

  TDynamicLight = Class
  Protected
    FOwner    : TSceneGL;
  Public
    Sphere    : TSphere;
    Color     : Packed Array[0..3] Of GLFloat;
    CurColor  : Packed Array[0..3] Of GLFloat;
    OldColor  : Packed Array[0..3] Of GLFloat;
    DestColor : Packed Array[0..3] Of GLFloat;
    Flicker   : Single;
    CalcTime  : Integer;
    X,Y,Z     : Single;
    Constructor Create; Overload;
    Constructor Create(AOwner: TSceneGL); Overload;
    Constructor Create(Light: TDynamicLight); Overload;
    Destructor  Destroy; Override;
    Procedure   CalculateActualLight(Time: Integer);
    Procedure   SetShaderParameter(Index: Integer);
  End;

  TModel = Class
  Protected
    FOwner              : TSceneGL;
    FBox                : TAxisAlignedBox;
    FSphere             : TSphere;
    FCylinder           : TCylinder;
    FRayWork            : T3DPoint{I3dPoint};
    FP1Work             : T3DPoint{I3dPoint};
    FP2Work             : T3DPoint{I3dPoint};
    FP3Work             : T3DPoint{I3dPoint};
    FP4Work             : T3DPoint{I3dPoint};
    FP5Work             : T3DPoint{I3dPoint};
    FAA                 : Byte;
    FTint               : LongWord;
    FLastBoundTextureID : Integer;
    FHasAlpha           : Boolean;
    FHasTrans           : Boolean;
    FSetHasAlphaTrans   : Boolean;
    FAnimated           : Boolean;
    FCheckOcclusion     : Boolean;
    FVBOVertices        : Packed Array Of Integer;
    FVBOTexCoords       : Packed Array Of Integer;
    FVBOColors          : Packed Array Of Integer;
    FVBONormals         : Packed Array Of Integer;
    FTriedVBOs          : Boolean;
    FUsingVBOs          : Boolean;
    FSkipFirstVBO       : Boolean;
    FSubst              : TTextureSet;
    Procedure   RedrawTriangles(Const VertexColors: Array Of LongWord; AlphaNotOneOnly,Emissive: Boolean; Frame: TSkeletonFrame;
                                Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList);
    Procedure   RedrawQuads(Const VertexColors: Array Of LongWord; AlphaNotOneOnly,Emissive: Boolean; Frame: TSkeletonFrame;
                            Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList);
    Procedure   FreeTextureSets;
    Function    SubstituteTexture(Const Tex: TTexture; Const ReplaceFromTexInfo,ReplaceToTexInfo: String): TTexture;
    Function    FindNearestDynamicLight(NearDynamicLights: TStringList; Vertex: PGLFloat): TDynamicLight;
  Public
    Positions    : TFVector;
    FNormals     : TFVector;
    VNormals     : Packed Array Of LongWord;
    Vertices     : Packed Array Of TVertex;
    Faces        : Packed Array Of TFace;
    PieceIndices : Packed Array Of Integer;
    Colors       : Packed Array Of TColor;  // vertex´s color, rgba  -- Must be bytes and in this order for glColor4ubv() to work
    TexCounts    : Packed Array Of TFace;
    ColorType    : TModelColorType;
    BaseHeight   : Single;
    TextureSets  : TIntegerPointerHash;     // Hash of TTextureSet
    Constructor Create(AOwner: TSceneGL);
    Destructor  Destroy; Override;
    Procedure   Clear;
    Procedure   FlipFaces;
    Procedure   GeneratePolyhedron(Points: Array Of Single; FacePointCount,FaceDefs: Array Of Integer);
    Procedure   GenerateCloudDeck;
    Procedure   GenerateTetrahedron;
    Procedure   GenerateCube;
    Procedure   GenerateOctahedron;
    Procedure   GenerateDodecahedron;
    Procedure   GenerateIcosahedron;
    Procedure   GenerateTruncatedTetrahedron;
    Procedure   GenerateTruncatedCube;
    Procedure   GenerateTruncatedOctahedron;
    Procedure   GenerateTruncatedDodecahedron;
    Procedure   GenerateTruncatedIcosahedron;        // Soccer ball
    Procedure   GenerateRhombicDodecahedron;
    Procedure   GeneratePentakisDodecahedron;        // Tessellated dodecahedron
    Procedure   GenerateCuboctahedron;
    Procedure   GenerateTriangularCupola;            // Johnston solid J3 - Triangular cupola
    Procedure   GenerateSquareCupola;                // Johnston solid J4 - Square cupola
    Procedure   GeneratePentagonalCupola;            // Johnston solid J5 - Pentagonal cupola
    Procedure   GeneratePentagonalRotunda;           // Johnston solid J6 - Pentagonal rotunda
    Procedure   GenerateBilunabiRotunda;             // Johnston solid J91 - Bilunabirotunda
    Procedure   GenerateTriangularHebesphenorotunda; // Johnston solid J92 - Triangular Hebesphenorotunda
    Procedure   GenerateSphere;
    Procedure   GenerateSkySphere;
    Procedure   DoCheckOcclusion;
    Procedure   Rescale(SX,SY,SZ: Single);
    Function    AddFaces(Count: Integer): Integer;
    Function    AddVertices(Count: Integer): Integer;
    Procedure   Redraw(Const VertexColors: Array Of LongWord; AlphaNotOneOnly: Boolean; Frame: TSkeletonFrame;
                       Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList);
    Procedure   CalcExtents(BoxIsOk: Boolean);
    Procedure   CalcNormals(CalcVertexNormals: Boolean);
    Procedure   CalcRoundedNormals;
    Procedure   SetVertex(Index: Integer; X,Y,Z,NX,NY,NZ: Single);
    Procedure   SetFace(Index: Integer; VertIndices: Array Of Integer);
    Procedure   SetColor(R,G,B,A: Byte);
    Procedure   MapVertices(Width,Height: Integer; Clouds: Boolean);
    Function    IsIntersectedBy(Source,Dest: T3DPoint{I3dPoint}): Boolean; // WARNING: This modifies Dest if the result is true
    Procedure   FacesCollideWithEllipsoid(Ellipsoid: TEllipsoid; Velocity: T3DPoint{I3dPoint}; MovementSphere: TSphere; Var NearestDistance: Single;
                                          ClosestPoint: T3DPoint{I3dPoint}; Var Friction: Single);
    Procedure   SetTexture(Texture: TTexture);
    Procedure   AddTexCoords(TX,TZ: Single);
    Procedure   PutTextureSet(Index: Integer; TextureSet: TTextureSet);
    Procedure   CopyTextureSets(Model: TModel);
    Function    FindMinMaxZPoints(Const X,Y: Single; Out MinZ,MaxZ: Single): Boolean;
    Procedure   SortFacesByTexture(Quads: Boolean);
    Property    Box              : TAxisAlignedBox Read FBox;
    Property    Sphere           : TSphere         Read FSphere;
    Property    Cylinder         : TCylinder       Read FCylinder;
    Property    HasAlpha         : Boolean         Read FHasAlpha; // Has alpha in [1..254]
    Property    HasTrans         : Boolean         Read FHasTrans; // Has alpha = 0
    Property    SetHasAlphaTrans : Boolean         Read FSetHasAlphaTrans Write FSetHasAlphaTrans;
    Property    CheckOcclusion   : Boolean         Read FCheckOcclusion Write FCheckOcclusion;
  End;

  TModelArray = Array Of TModel;
  TIntegerArray = Array Of Integer;

  TFaceArea = Record
    Index : Integer;
    Area  : Single;
  End;
{
  TModelFaceSorter = Class
  Protected
    FSorter : TQuickSorter;
    FList   : Array Of TFaceArea;
    Function    Compare(Index0,Index1: Integer): Integer;
    Procedure   Exchange(Index0,Index1: Integer);
  Public
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   SortFacesByArea(Model: TModel);
  End;
}
  TRenderable = Class
  Protected
    FModels     : TStringList;     // References to TModel instances
    FHasAlpha   : Boolean;
    FHasTrans   : Boolean;
    FScale      : Single;
    FAnimSpeed  : Single;          // Only stored here, but will be placed in the entity later (inverse seconds to complete the animation)
    FBaseHeight : Single;
    Function    GetModel(Index: Integer): TModel;
    Function    GetModelID(Index: Integer): String;
    Procedure   SetModelID(Index: Integer; ID: String);
    Function    GetCount: Integer;
  Public
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   AddModel(ID: String; Model: TModel);
    Procedure   CheckOcclusion(Frame: Single; ModelTypes: String); Dynamic;
    Procedure   Redraw(Frame: Single; Const VertexColors: Array Of LongWord; AlphaNotOneOnly: Boolean; ModelTypes: String;
                       Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList); Dynamic;
    Function    IsIntersectedBy(Frame: Single; Source,Dest: T3DPoint{I3dPoint}; ModelTypes: String): Boolean; Dynamic; // WARNING: This modifies Dest if the result is true
    Function    FindMinMaxZPoints(Frame: Single; Const X,Y: Single; Out MinZ,MaxZ: Single): Boolean;
    Procedure   FindIntersection(Frame: Single; Ellipsoid: TEllipsoid; Velocity: T3DPoint{I3dPoint}; MovementSphere: TSphere; Var NearestDistance: Single;
                                 ClosestPoint: T3DPoint{I3dPoint}; Var Friction: Single);
    Function    ContainsModel(ID: String): Boolean;
    Function    NumFrames: Integer; Dynamic;
    Function    LastFramePos: Single;
    Function    GetFramePos(Frame: Integer): Single;
    Procedure   CalcBaseHeight; Dynamic;
    Property    Models[Index: Integer] : TModel  Read GetModel;
    Property    ModelID[Index: Integer]: String  Read GetModelID Write SetModelID;
    Property    Count                  : Integer Read GetCount;
    Property    HasAlpha               : Boolean Read FHasAlpha; // Has alpha in [1..254]
    Property    HasTrans               : Boolean Read FHasTrans; // Has alpha = 0
    Property    Scale                  : Single  Read FScale     Write FScale;
    Property    AnimSpeed              : Single  Read FAnimSpeed Write FAnimSpeed;
    Property    BaseHeight             : Single  Read FBaseHeight;
  End;

  PSkeletonPiece = ^TSkeletonPiece;
  TSkeletonPiece = Packed Record
    Name               : String;
    X,Y,Z              : Single;                     // Translations and rotations
    M                  : Array[1..3,1..3] Of Single;
    QW,QX,QY,QZ : Single;
    QW0,QX0,QY0,QZ0    : Single;
    Angle,AX,AY,AZ     : Single;
    TX,TY,TZ           : Single;
    Angle0,AX0,AY0,AZ0 : Single;
    TX0,TY0,TZ0        : Single;


    M1                 : Array[1..3,1..3] Of Single;
    Angle1,AX1,AY1,AZ1 : Single;
    TX1,TY1,TZ1        : Single;

    Parent             : Integer;
  End;

  TSkeletonRenderable = Class;

  TSkeletonFrame = Class
  Protected
    Function    GetNumPieces: Integer;
  Public
    FrameBox    : TAxisAlignedBox;
    FrameSphere : TSphere;
    FrameBoxOk  : Boolean;
    Frame       : Single;
    Box         : Array Of TAxisAlignedBox;
    BoxOk       : Array Of Boolean;
    Pieces      : Array Of TSkeletonPiece;
    Parent      : TSkeletonRenderable;
    Constructor Create(AParent: TSkeletonRenderable);
    Destructor  Destroy; Override;
    Procedure   SetupModelBox(Model: TModel; ModelIndex: Integer; Scale: Single);
    Procedure   SetupFrameBox(Model: TModelArray; ModelIndex: TIntegerArray; Scale: Single; DoModelBox: Boolean);
    Procedure   Copy(Frame,ReferenceFrame: TSkeletonFrame);
    Procedure   AddPiece(AName: String; X,Y,Z,A11,A12,A13,A21,A22,A23,A31,A32,A33,QW,QX,QY,QZ,QW0,QX0,QY0,QZ0,Angle,AX,AY,AZ,TX,TY,TZ,Angle0,AX0,AY0,AZ0,TX0,TY0,TZ0: Single; ParentIndex: Integer);
    Procedure   SetPiece(Index: Integer; AName: String; X,Y,Z,A11,A12,A13,A21,A22,A23,A31,A32,A33,QW,QX,QY,QZ,QW0,QX0,QY0,QZ0,Angle,AX,AY,AZ,TX,TY,TZ,Angle0,AX0,AY0,AZ0,TX0,TY0,TZ0: Single; ParentIndex: Integer);
    Procedure   CalculateBoundingBox(Model: TModel; Scale: Single; Out MinX,MinY,MinZ,MaxX,MaxY,MaxZ: Single);
    Procedure   CheckOcclusion(Model: TModel; ModelIndex: Integer; Scale: Single; FrameNum: Single);
    Procedure   Redraw(Model: TModel; ModelIndex: Integer; Const VertexColors: Array Of LongWord; AlphaNotOneOnly: Boolean; Scale: Single;
                       Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList; FrameNum: Single);
    Function    IsIntersectedBy(Model: TModel; ModelIndex: Integer; Source,Dest: T3DPoint{I3dPoint}; Scale: Single): Boolean; // WARNING: This modifies Dest if the result is true
    Property    NumPieces    : Integer Read GetNumPieces;
  End;

  TSkeletonRenderable = Class(TRenderable)
  Protected
    FFrames : Array Of TSkeletonFrame;
    FFrame  : Integer;
    Function    GetFrame(Index: Integer): TSkeletonFrame;
  Public
//    RenderAllModels : Boolean;
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   AddFrame(Frame: TSkeletonFrame);
    Procedure   CheckOcclusion(Frame: Single; ModelTypes: String); Override;
    Procedure   Redraw(Frame: Single; Const VertexColors: Array Of LongWord; AlphaNotOneOnly: Boolean; ModelTypes: String;
                       Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList); Override;
    Function    IsIntersectedBy(Frame: Single; Source,Dest: T3DPoint{I3dPoint}; ModelTypes: String): Boolean; Override; // WARNING: This modifies Dest if the result is true
    Procedure   BuildModelList(ModelTypes: String; Var ModelList: TModelArray; Var ModelIndices: TIntegerArray);
    Function    NumFrames: Integer; Override;
    Procedure   CalcBaseHeight; Override;
    Property    Frame[Index: Integer] : TSkeletonFrame Read GetFrame;
    Property    CurrentFrame          : Integer        Read FFrame;
  End;

  TCamera = Class
    Position    : T3DPoint{I3dPoint};       // Position of the camera,  x,y,z
    SceneCenter : T3DPoint{I3dPoint};       // Position for the center of the scene
    UpVector    : T3DPoint{I3dPoint};       // Vector pointing "up" from the viewer´s perspective
    Frustum     : TFrustum;
    Owner       : TSceneGL;
//    Entity      : TEntity;
    Constructor Create(Scene: TSceneGL);
    Destructor  Destroy; Override;
    Procedure   Redraw(DoOpenGL: Boolean);
    Procedure   SetPosition(ix,iy,iz: GLDouble);
    Procedure   LookAt(ix,iy,iz: GLDouble);
    Procedure   SetVectorUp(ix,iy,iz: GLDouble);
    Procedure   SetPosLookAtUp(PX,PY,PZ,LX,LY,LZ,UX,UY,UZ: GLDouble);
  End; // TCamera

  TOnCreateItem = Function(Option: Integer): TObject Of Object;
  TThreadSafeList = Class
  Protected
    FItems        : Array Of TObject;
    FMutex        : TCriticalSection;
    FOnCreateItem : TOnCreateItem;
    FMaxIndex     : Integer;
    Function    GetCount: Integer;
    Function    GetItem(Index: Integer): TObject;
    Procedure   Allocate(Amount: Integer);
    Procedure   Lock;
    Procedure   Unlock;
  Public
    Constructor Create;
    Destructor  Destroy; Override;
    Function    GetNew(Option: Integer): TObject; Overload;
    Function    GetNew(Count,Option: Integer): TList; Overload;
    Procedure   FreeAll;
    Procedure   FreeItems(List: TList);
    Procedure   FreeItem(Item: TObject);
    Procedure   Add(Item: TObject);
    Function    GetIndexOf(Item: TObject): Integer;
    Procedure   Delete(Index: Integer);
    Procedure   Clear;
    Property    OnCreateItem          : TOnCreateItem Read FOnCreateItem Write FOnCreateItem;
    Property    Count                 : Integer       Read GetCount;
    Property    Items[Index: Integer] : TObject       Read GetItem; Default;
  End;

  // The occlusion manager implements a lazy occlusion grid

  TTrianglePoints = Array[0..2] Of TPoint;
  TOcclusionManager = Class
  Protected
    FOwner         : TSceneGL;
    FWidth         : Integer;
    FHeight        : Integer;
    FGridWidth     : Integer;
    FGridHeight    : Integer;
    FMatrix        : T4x4Matrix;
    FRotateXMatrix : T4x4Matrix;
    FRotateYMatrix : T4x4Matrix;
    FRotateZMatrix : T4x4Matrix;
    FScreenPoints  : TTrianglePoints;
    FZBufferSize   : Integer;
    FIdentity      : Boolean;
    FaceVerts      : TFVector;
//    FaceVerts1     : TFVectorExt;
    FaceNorms      : TFVector;
    FaceIndices    : Array Of Integer;
    Procedure   FillSimpleScreenPolygonHorz(Var Polygon: TSimpleScreenPolygon);
    Procedure   FillSimpleScreenPolygonVert(Var Polygon: TSimpleScreenPolygon);
  Public
    FZBuffer       : Packed Array Of LongInt;    // MUST be "packed"!!!
    FGrid          : Packed Array Of LongInt;    // -1 means dirty     MUST be "packed"!!!
    Observer       : TFVector;
    Frustum        : TFrustum;
    Enabled        : Boolean;
    Constructor Create(AOwner: TSceneGL);
    Destructor  Destroy; Override;
    Procedure   ClearZBuffer;
    Procedure   LoadIdentity;
    Procedure   XRotate(Angle: Single);
    Procedure   YRotate(Angle: Single);
    Procedure   ZRotate(Angle: Single);
    Procedure   Rotate(P: T3DPoint{I3dPoint});
    Procedure   Translate(P: T3DPoint{I3dPoint});
    Procedure   Scale(P: T3DPoint{I3dPoint});
    Procedure   Setup(AWidth,AHeight: Integer);
    Procedure   RenderTriangleToZBuffer(Positions,Normals: SPtr);
    Procedure   SSE_3DNow_RenderTriangleToZBuffer(Model: TModel);
    Procedure   FillSimpleScreenPolygon(Var Polygon: TSimpleScreenPolygon; Horizontal: Boolean);
    Function    IsOccluded(Box: TAxisAlignedBox): Boolean;
    Function    Is2DBoxOccluded(MinPt,MaxPt: TPoint; Depth: Integer): Boolean;
    Property    GridWidth  : Integer Read FGridWidth;
    Property    GridHeight : Integer Read FGridHeight;
  End;

  TSkySphere = Class;
  TStarField = Class;
  TSatellite = Class;
  TPrecipitation = Class;

  TOcclusionThread = Class(TThread)
  Protected
    FSceneGL : TSceneGL;
  Public
    Constructor Create(SceneGL: TSceneGL);
    Procedure   Execute; Override;
  End;

  TRedrawState = (rdsBeforeDraw,rdsBeforeSwap,rdsAfterSwap);

  TGLSLShaderType = (stVertex,stFragment);

  TGLSLShaderInfo = Record
    VertexShaderName   : String; // The name of the vertex shader (NOT the shader text)
    FragmentShaderName : String; // The name of the fragment shader (NOT the shader text)
  End;

  TGLSLShaderManager = Class
  Protected
    FVertexShaderSource   : TStringStringHash;      // Key is the shader name, value is the shader source
    FFragmentShaderSource : TStringStringHash;      // Key is the shader name, value is the shader source
    FVertexShaders        : TStringList;            // String is the shader name, object is the GLhandleARB to the shader
    FFragmentShaders      : TStringList;            // String is the shader name, object is the GLhandleARB to the shader
    FCurrentProgram       : String;                 // Vertex-fragment shader name combination
    FDefaultProgram       : String;                 // Vertex-fragment shader name combination
    FPrograms             : TStringList;            // String is a vertex-fragment name combination, object is the OpenGL program index
    FCurrentProgramHandle : GLhandleARB;
    FEnabled              : Boolean;
    Function    LoadShader(Const Src: String; Const ShaderType: GLenum): GLhandleARB;
    Function    GetOpenGLInfoLog(Const S: GLhandleARB): String;
    Function    GetProgramName(Const VertexShader,FragmentShader: String): String;
    Procedure   GetShaderNames(Const ProgramName: String; Out VertexShader,FragmentShader: String);
    Function    CreateShader(Const ShaderType: TGLSLShaderType; Const ShaderName,ShaderText: String): Boolean;
    Procedure   DestroyPrograms;
    Procedure   DestroyShaders;
    Function    CreateProgram(Const VertexShader,FragmentShader: String): Boolean; Overload;
    Function    CreateProgram(Const ProgramName: String): Boolean; Overload;
    Procedure   SetEnabled(B: Boolean);
  Public
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   AddShader(Const ShaderType: TGLSLShaderType; Const ShaderName,ShaderText: String);
    Procedure   RegisterProgram(Const VertexShader,FragmentShader: String);
    Procedure   SetDefaultProgram(Const VertexShader,FragmentShader: String);
    Procedure   SetCurrentProgram(Const VertexShader,FragmentShader: String);
    Procedure   GetDefaultProgram(Out VertexShader,FragmentShader: String);
    Function    IsAvailable: Boolean;
    Function    CreatePrograms: Boolean;
    Function    CreateShaders: Boolean;
    Property    CurrentProgramHandle : GLhandleARB Read FCurrentProgramHandle;
    Property    Enabled              : Boolean     Read FEnabled Write SetEnabled;
  End;

  TSceneGL = Class
  Protected
    FUpdatedArea       : Boolean;
    FAddingTexture     : Boolean;
    FBSPTreeRoot       : TBSPTreeNode;
    FBSPTreeMutex      : TCriticalSection;
    FWaitMutex         : TCriticalSection;
    FSkySphere         : TSkySphere;
    FSkySphere1        : TSkySphere;
    FStarField         : TStarField;
    FPrecipitation     : TPrecipitation;
    FSun               : TSatellite;
    FMoon              : TSatellite;
    FMovementSphere    : TSphere;
    FNewCenterPoint    : T3DPoint{I3dPoint};
    FNewVelocity       : T3DPoint{I3dPoint};
    FGravity           : T3DPoint{I3dPoint};
    FWinClassName      : Packed Array[0..63] Of Char;
    FWindowIsVisible   : Boolean;
    FLastActiveWindow  : THandle;
    FOcclusionManager  : TOcclusionManager;
    FAspect            : GLDouble;
    FOnUIRepaint       : TNotifyEvent;
    FCameraNode        : TBSPTreeNode;
    FCheckHidden       : Boolean;
    FFrameCount        : Integer;
    FRepaintMonitor    : TMonitor;
    FShaderManager     : TGLSLShaderManager;
    FCanCreateShaders  : Boolean;
    FCanCreatePrograms : Boolean;
    Procedure DoRedraw;
    Procedure DoInitRC;
    Procedure DoInitDC;
    Procedure DoReleaseRC;
    Procedure DoUpdateArea(Left,Top,Width,Height: Integer);
    Procedure DoSetActive(B: Boolean);
    Procedure DoLoadTexture(Texture: TTexture);
    Procedure SetDCPixelFormat;
    Procedure DoAddLight(Num: Integer);
    Procedure DoFreeTexture(Texture: TTexture);
    Procedure DoFreeTextures(List: TStringList);
    Procedure DoFreeAllTextures;
    Procedure DoMakeImagePanel(Panel: PPanel);
    Function  CreateEntity(Option: Integer): TObject;
    Function  CreateRenderable(Option: Integer): TObject;
    Function  CreateModel(Option: Integer): TObject;
    Function  CreateDynamicLight(Option: Integer): TObject;
    Procedure DoOcclusion;
    Procedure SetShaderAmbientLight;
    Procedure CalculateActualDynamicLights(Time: Integer);
    Procedure RegisterMaterialPrograms;
  Public

//    RedrawPossible    : Integer;
//    Redrawn           : Integer;

    DidRedraw         : Boolean;
    DidRepaint        : Boolean;
    DC                : HDC;                     // This is the Device Context
    HRC               : HGLRC;                   // Somewhat private
    WindowHandle      : THandle;                 // Handle to the host window, a Tpanel most times
    Lights            : TList;                   // Available lights in the scene
    Cameras           : TList;                   // Available cameras in the scene
    Active            : Boolean;                 // True: InitRC has been called already
    FPerspective      : Boolean;                 // True: use perspective projection,  false: use orthogonal projection
    DistNear          : Single;                  // Distance to near clipping plane
    DistFar           : Single;                  // Distance to far clipping plane
    Angle             : Single;                  // Field-of-view angle
    Texturing         : Boolean;                 // True: use textures and texture information, false: don´t use textures
    BackR,BackG,BackB : Single;                  // Background color (RGB), between 0.0 and 1.0
    FogColor          : Array [0..3] Of GLFloat; // Fog color (RGBA), between 0.0 and 1.0
    FogDensity        : GLFloat;
    FogMinDist        : GLFloat;
    FogMaxDist        : GLFloat;
    FogEnabled        : Boolean;
    FogType           : GLInt;
    ActiveCamera      : TCamera;                 // Points to the camera from which the scene is being "filmed"
    DefaultCamera     : TCamera;                 // Points to the default camera, this camera shouldn´t never be destroyed
    bTranslucentMode  : Boolean;                 // Ignore depth buffer?
    RenderThread      : TRenderThread;
    Textures          : TStringList;
    WindowLeft        : Integer;
    WindowTop         : Integer;
    WindowWidth       : Integer;
    WindowHeight      : Integer;
    Redrawing         : Boolean;
    Owner             : TObject;                 // Really a TScene3D
//    UI                : TUserInterface;
    SampleText        : T3DText;                 // font sample for ticks on axis
    Entities          : TThreadSafeList;         // Available entities in the scene
    Renderables       : TThreadSafeList;
    Models            : TThreadSafeList;
    DynamicLights     : TThreadSafeList;         // List of TDynamicLight -- used only when shaders are supported
    Materials         : TThreadSafeList;         // List of TMaterial -- used only when shaders are supported
    Panel             : TPanel;
    RedrawQueue       : TList;
    RedrawTime        : LongInt;
//    SkinUI            : TGLUI;
    Name              : String;
    FrustumCulling    : Boolean;
    LogProc           : TLogProc;
    Constructor Create(AOwner: TObject);
    Destructor  Destroy; Override;
    Procedure   StartRenderThread;
    Procedure   InitRC;
    Procedure   InitDC;
    Function    RedrawQueued: Boolean;
    Function    Redraw: Boolean;
    Procedure   ReleaseRC;
    Procedure   UpdateArea(Left,Top,Width,Height: Integer);
    Procedure   SetPerspective(iAngle,iDistNear,iDistFar: Single);
    Procedure   UseCamera(CameraNumber: Integer);
    Procedure   SetActive(B: Boolean);
    Procedure   LoadTexture(Texture: TTexture);
    Procedure   AddLight(Num: Integer);
    Function    AddTexture(FileName,OpacityFileName: String; NeedsMask: Boolean): TTexture;
    Function    AddTextureFromBitmap(FileName: String; BMP: TBitmap; NeedsMask: Boolean; CheckRedraw: Boolean = True): TTexture;
    Function    AddTextureFromRaster32(FileName: String; R32: TRaster32; NeedsMask: Boolean; CheckRedraw: Boolean = True): TTexture;
    Function    AddTexturesFromBitmaps(FileNamesAndBMPs: TStringList; NeedsMask: Boolean): TStringList;
    Function    AddTexturesFromRaster32s(FileNamesAndR32s: TStringList; NeedsMask: Boolean): TStringList;
    Procedure   FreeTextures(List: TStringList);
    Procedure   FreeTexture(Texture: TTexture);
    Procedure   ClearScene(ClearRenderables,ClearModels: Boolean);
    Procedure   MoveEntityToNode(Entity: TEntity; NewNode: TBSPTreeNode);
    Procedure   LockBSPTree(ThreadName: String);
    Procedure   UnlockBSPTree;
    Procedure   WaitUntilDoneRepainting;
//    Procedure   LockRenderable;
//    Procedure   UnlockRenderable;
    Function    MoveObject(ObjectEllipsoid: TEllipsoid; ObjectVelocity: T3DPoint{I3dPoint}; GravityAmount: Single; ExcludeEntity: TEntity): Boolean;
    Procedure   InternalMoveObject(ObjectEllipsoid: TEllipsoid; ObjectVelocity: T3DPoint{I3dPoint}; GravityAmount: Single; ExcludeEntity: TEntity);
    Procedure   MakeImagePanel(Panel: PPanel);
    Procedure   FreeAllTextures;
    Procedure   SetVSync(VSyncOn: Boolean = True);
    Function    GetNearbyDynamicLights(Sphere: TSphere; Position: T3DPoint): TStringList;
    Procedure   LoadDefaultShaders;
    Property    BSPTreeRoot      : TBSPTreeNode       Read FBSPTreeRoot;
    Property    SkySphere        : TSkySphere         Read FSkySphere;
    Property    SkySphere1       : TSkySphere         Read FSkySphere1;
    Property    StarField        : TStarField         Read FStarField;
    Property    Precipitation    : TPrecipitation     Read FPrecipitation;
    Property    Sun              : TSatellite         Read FSun;
    Property    Moon             : TSatellite         Read FMoon;
    Property    WindowIsVisible  : Boolean            Read FWindowIsVisible;
    Property    OcclusionManager : TOcclusionManager  Read FOcclusionManager;
    Property    Aspect           : GLDouble           Read FAspect;
    Property    OnUIRepaint      : TNotifyEvent       Read FOnUIRepaint Write FOnUIRepaint;
    Property    CheckHidden      : Boolean            Read FCheckHidden Write FCheckHidden;
    Property    FrameCount       : Integer            Read FFrameCount;
    Property    RepaintMonitor   : TMonitor           Read FRepaintMonitor;
    Property    ShaderManager    : TGLSLShaderManager Read FShaderManager;
  End; // TSceneGL

  TLight = Class
    Position        : T3DPoint{I3dPoint};
    Normal          : T3DPoint{I3dPoint};
    Source          : TVertex;                   // Position and orientation of the light
    FAmbient        : Packed Array[0..3] Of GLFloat;    // Ambient component
    FDiffuse        : Packed Array[0..3] Of GLFloat;    // Diffuse component
    FSpecular       : Packed Array[0..3] Of GLFloat;    // Specular component
    Number          : LongInt;                   // number of the light, from 1 to 8
    LightType       : Integer;                   // The type of light, spot, ambiental, omni
    CutOffAngle     : GLFloat;                   // the Cut-off angle for spot lights
    SpotExponent    : GLFloat;                   // the shininess of the spot light
    Attenuation     : GLFloat;                   // attenuation amount
    AttenuationType : Integer;                   // attenuation type: constant, linear or quadratic
    Enabled         : Boolean;                   // indicates if the light is on or off
    Rotation        : Array[1..3] Of Single;     // Orientation, Rx,Ry,Rz
    Scale           : Array[1..3] Of Single;     // Zoom x,y,z
    DynamicLight    : TDynamicLight;
    Constructor Create(Num: Integer);
    Destructor  Destroy; Override;
    Procedure   Ambient(R,G,B,A: GLFloat);
    Procedure   Diffuse(R,G,B,A: GLFloat);
    Procedure   Specular(R,G,B,A: GLFloat);
    Procedure   SetOrientation(NX,NY,NZ: GLFloat);
    Procedure   Redraw;
  End; // TLight

{
  // Easy manipulation of 3D objects with the mouse

  T3DMouse = Class
    Button1     : Boolean;                 // Left button on mouse
    Button2     : Boolean;                 // Right button on mouse
    Mode        : Integer;                 // Movement mode:  1-move x,y-z, 2-turn rx,ry-rz, 3-turn rx,ry-rz+move z.
    Entity      : TEntity;                 // Points to the entity to be modified, just one at a time
    Start       : Array[1..6] Of Single;   // x,y,z - rx,ry,rz   saves the initial position when dragging mouse
    Scaling     : Array[1..6] Of Single;   // x,y,z - rx,ry,rz   stores the speed relations between mouse and 3D
    BlockStatus : Array[1..6] Of Boolean;  // x,y,z - rx,ry,rz   which movements are blocked
    Constructor Create(iEntity: TEntity);
    Procedure   Move(X,Y: Single; Buttons: TShiftState);
    Procedure   Scale(X,Y,Z,RX,RY,RZ: Single);
    Procedure   Block(Num: Integer; Valor: Boolean);
//    Procedure   FindVertex(X,Y: Integer; Scene: TSceneGL; Var PT: TVertex);
  End; // T3DMouse
}
  TSkySphere = Class
  Protected
    FModel        : TModel;
    FVisible      : Boolean;
    FSize         : Single;
    FUpdating     : Boolean;
    FOwner        : TSceneGL;
    FUseAlpha     : Boolean;
    FXRotate      : Single;
    FYRotate      : Single;
    FZRotate      : Single;
    FXRotate0     : Single;
    FYRotate0     : Single;
    FZRotate0     : Single;
    FXRotate1     : Single;
    FYRotate1     : Single;
    FZRotate1     : Single;
    FClouds       : Boolean;
    FVertexColors : Array Of LongWord;
    Procedure   SetVisible(B: Boolean);
    Procedure   SetSize(S: Single);
    Procedure   SetUseAlpha(B: Boolean);
  Public
    Constructor Create(AOwner: TSceneGL);
    Destructor  Destroy; Override;
    Procedure   Redraw;
    Procedure   SetTexture(Texture: TTexture; Clouds: Boolean);
    Procedure   SetRotate(RX,RY,RZ: Single);
    Procedure   SetRotateWithMoon(RSX,RSY,RSZ,RMX,RMY,RMZ,MoonAlbedo: Single);
    Property    Visible  : Boolean Read FVisible  Write SetVisible;
    Property    Size     : Single  Read FSize     Write SetSize;
    Property    UseAlpha : Boolean Read FUseAlpha Write SetUseAlpha;
    Property    Clouds   : Boolean Read FClouds;
    Property    Model    : TModel  Read FModel;
  End;

  TSatelliteType = (stSun,stMoonOrPlanet);

  TSatellite = Class
  Protected
    FVisible   : Boolean;
    FSize      : Single;
    FOwner     : TSceneGL;
    FColor     : TColor;
    FTextures  : Array Of TTexture;
    FXRotate   : Single;
    FYRotate   : Single;
    FZRotate   : Single;
    FAlpha     : Byte;
    FSatType   : TSatelliteType;
    FPhase     : Single;
    Procedure   SetVisible(B: Boolean);
    Procedure   SetSize(S: Single);
    Procedure   SetColor(C: TColor);
    Procedure   SetAlpha(A: Byte);
    Procedure   SetSatType(AType: TSatelliteType);
  Public
    Constructor Create(AOwner: TSceneGL);
    Destructor  Destroy; Override;
    Procedure   Redraw;
    Procedure   SetRotate(X,Y,Z: Single);
    Procedure   SetPhase(R32: TRaster32; S: Single);
    Procedure   AddTexture(Texture: TTexture);
    Property    Visible   : Boolean        Read FVisible   Write SetVisible;
    Property    Size      : Single         Read FSize      Write SetSize;
    Property    Color     : TColor         Read FColor     Write SetColor;
    Property    Alpha     : Byte           Read FAlpha     Write SetAlpha;
    Property    SatType   : TSatelliteType Read FSatType   Write SetSatType;
  End;

  PPrecipitationItem = ^TPrecipitationItem;
  TPrecipitationItem = Packed Record
    Color    : TColor;        // RGBA
    X,Y,Z    : Single;        // Position
    Velocity : Single;        // Z velocity, positive moves UPWARD 
    MinZ     : Single;        // Z coordinate at which it will vanish
    Counter  : Integer;
  End;

  TPrecipitation = Class
  Protected
    FVisible   : Boolean;
    FOwner     : TSceneGL;
    FColor     : TColor;
    FCount     : Integer;
    FItems     : PPrecipitationItem; // Pointer to a packed array
    FVelocity  : Single;
    FLastTime  : Integer;
    FArea      : Single;
    FLines     : Boolean;
    Procedure   SetVisible(B: Boolean);
    Procedure   SetLines(B: Boolean);
    Procedure   SetColor(C: TColor);
    Procedure   SetVelocity(V: Single);
    Procedure   SetArea(A: Single);
    Procedure   SetCount(ACount: Integer);
  Public
    Constructor Create(AOwner: TSceneGL);
    Destructor  Destroy; Override;
    Procedure   Redraw;
    Procedure   InitItems;
  Published
    Property    Owner    : TSceneGL Read FOwner;
    Property    Count    : Integer  Read FCount    Write SetCount;
    Property    Visible  : Boolean  Read FVisible  Write SetVisible;
    Property    Lines    : Boolean  Read FLines    Write SetLines;
    Property    Color    : TColor   Read FColor    Write SetColor;
    Property    Velocity : Single   Read FVelocity Write SetVelocity;
    Property    Area     : Single   Read FArea     Write SetArea;
  End;

  TStar = Class
    X,Y,Z : Single;
    Color : TColor;
  End;

  TStarField = Class
  Protected
    FStars    : TStringList;
    FVisible  : Boolean;
    FOwner    : TSceneGL;
    FXRotate  : Single;
    FYRotate  : Single;
    FZRotate  : Single;
    FAlpha    : Byte;
    Procedure   SetVisible(B: Boolean);
    Procedure   SetAlpha(A: Byte);
  Public
    Constructor Create(AOwner: TSceneGL);
    Destructor  Destroy; Override;
    Procedure   Clear;
    Procedure   AddStars(Stars: TStringList);
    Procedure   Redraw;
    Procedure   SetRotate(X,Y,Z: Single);
    Property    Visible  : Boolean Read FVisible  Write SetVisible;
    Property    Alpha    : Byte    Read FAlpha    Write SetAlpha;
  End;

Const MaxHits = 200;

Var
  UseVBOExtension : Boolean;
  VSyncOn         : Boolean;

//Var
//  XXX,YYY,NNN : Integer;
//  NumFound    : Integer;
//  VertexHits  : Array[1..MaxHits] Of Integer;

Function  ConvertBMP(BMPName: String; RGBAName: String; Transparent: LongInt): Integer;
Function  MaskExceptions: Pointer;
Procedure UnmaskExceptions(OldMask: Pointer);
Procedure glWindowPos4fMESAemulate(X,Y,Z,W: GLFloat);
Procedure glWindowPos2fMESAemulate(X,Y: GLFloat);
Procedure SRotateX(Angle: Single; Var X,Y,Z: Single);
Procedure SRotateY(Angle: Single; Var X,Y,Z: Single);
Procedure SRotateZ(Angle: Single; Var X,Y,Z: Single);
Procedure DRotateX(Angle: Double; Var X,Y,Z: Double);
Procedure DRotateY(Angle: Double; Var X,Y,Z: Double);
Procedure DRotateZ(Angle: Double; Var X,Y,Z: Double);
Procedure GetAbsolutePosition(Control: TWinControl; Var X,Y: Integer);
Procedure LogToFile(St: String; DoLog: Boolean = False);

Procedure LogToOCCFile(St: String);

Function  GetUltimateOwner(Component: TComponent): TComponent;

//Var ModelFaceSorter: TModelFaceSorter;
Var
  SceneFullScreen                 : Boolean;
  SceneFullScreenWidth            : Integer;
  SceneFullScreenHeight           : Integer;
  SceneFullScreenPixelDepth       : Integer;
  SceneFullScreenHasKeyboardFocus : Boolean;
  OnFullScreenKeyDown             : TKeyEvent;
  OnFullScreenKeyUp               : TKeyEvent;
  OnFullScreenMouseDown           : TMouseEvent;
  OnFullScreenMouseUp             : TMouseEvent;
  OnFullScreenMouseMove           : TMouseMoveEvent;

Implementation

Uses GLVisir, Math, HSLUtils;

Var
  MaxLights         : GLInt;
  LogMutex          : TCriticalSection;
//  RedrawMutex       : TCriticalSection;
  WindowHash        : TIntegerPointerHash;
  SceneHash         : TIntegerPointerHash;
  LastMouseButton   : TMouseButton;

Procedure DoColor(Color,Tint: LongWord);
Var
  C : LongWord;
  P : Pointer;

Begin
  P := @(MultAlpha[0]);
  Asm
    PUSH  EBX

    MOV   EAX,Color
    MOV   ECX,Tint
    MOV   EBX,P
    SUB   EDX,EDX

    MOV   DL,CL
    MOV   DH,AL
    MOV   AL,BYTE PTR [EBX+EDX]

    SHR   ECX,8
    ROR   EAX,8

    MOV   DL,CL
    MOV   DH,AL
    MOV   AL,BYTE PTR [EBX+EDX]

    SHR   ECX,8
    ROR   EAX,8

    MOV   DL,CL
    MOV   DH,AL
    MOV   AL,BYTE PTR [EBX+EDX]

    ROL   EAX,16
    MOV   C,EAX

    POP   EBX
  End; // Asm
{
  C := Color;
  TRGBA(C).R := MultAlpha[TRGBA(Color).R * 256 + TRGBA(Tint).R];
  TRGBA(C).G := MultAlpha[TRGBA(Color).G * 256 + TRGBA(Tint).G];
  TRGBA(C).B := MultAlpha[TRGBA(Color).B * 256 + TRGBA(Tint).B];
}
  glColor4ubv(@C);
//    glColor4ub(R,G,B,TRGBA(Color).A);
End; // DoColor

Function GetUltimateOwner(Component: TComponent): TComponent;
Var Owner: TComponent;
Begin
  Owner := Component;
  While (Owner <> Nil) And (Owner.Owner <> Nil) And Not (Owner.Owner Is TApplication) Do Owner := Owner.Owner;
  Result := Owner;
End; // GetUltimateOwner

Procedure GetAbsolutePosition(Control: TWinControl; Var X,Y: Integer);
Var
  Parent: TWinControl;
  BRect : TRect;
  CRect : TRect;
  I     : Integer;
  BW,BH : Integer;
  CW,CH : Integer;

Begin
  BRect  := Control.BoundsRect;
  X      := BRect.Left;
  Y      := BRect.Top;
  Parent := Control.Parent;
  While Parent <> Nil Do
  Begin
    BRect := Parent.BoundsRect;
    CRect := Parent.ClientRect;
    BW    := BRect.Right - BRect.Left;
    CW    := CRect.Right - CRect.Left;
    BH    := BRect.Bottom - BRect.Top;
    CH    := CRect.Bottom - CRect.Top;
    Inc(X,BRect.Left);
    Inc(Y,BRect.Top);
    I := (BW - CW) Div 2;
    Inc(X,I);
    Inc(Y,BH - CH - I);
    Parent := Parent.Parent;
  End; // While
End; // GetAbsolutePosition

function WindowProc(HWindow: HWnd; Message, WParam: Longint; LParam: Longint): Longint; StdCall;
//Var Msg: TMessage;
Var
  Panel       : TPanel;
  Scene       : TSceneGL;
  MouseButton : TMouseButton;
  Shift       : TShiftState;
  Key         : Word;

Begin

//  LogToFile('WindowProc(' + IntToStr(HWindow) + '): $' + IntToHex(Message,4));

  Case Message Of
(*    WM_CREATE:
    Begin
      LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_CREATE');
      Scene := TSceneGL(SceneHash.Get(GetParent(HWindow)));
      If Scene <> Nil Then
      Begin
//        Scene.InitDC;
//        Scene.InitRC;
//        Scene.SetActive(True);
//        Scene.Redraw;
      End
      Else LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_CREATE...ERROR: Scene is Nil!');
    End;*)
    WM_DESTROY:
    Begin
//      LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_DESTROY');
      Scene := TSceneGL(SceneHash.Get(GetParent(HWindow)));
      If Scene <> Nil Then
      Begin
        Scene.Active := False;
        While Scene.Redrawing Do Sleep(1);
        if SceneFullscreen then             // Change back to non fullscreen
        begin
          ChangeDisplaySettings(devmode(nil^), 0);
        end;
        wglMakeCurrent(0, 0);
        wglDeleteContext(Scene.HRC);
        ReleaseDC(Scene.WindowHandle, Scene.DC);
        Scene.DC  := 0;
        Scene.HRC := 0;
        Scene.WindowHandle := 0;
        Scene.SampleText.Free;
        Scene.SampleText := Nil;
      End;
//      Else LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_DESTROY...ERROR: Scene is Nil!');

//      Scene.WindowHandle := 0;
      WindowHash.Put(HWindow,Nil);
      SceneHash.Put(HWindow,Nil);
    End;
(*    WM_NCDESTROY:
    Begin
      LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_NCDESTROY');
    End;
    WM_PAINT:
    Begin
      LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_PAINT');
{
      // Never want to paint from here -- just let the TGLForm handle it

      Scene := TSceneGL(SceneHash.Get(GetParent(HWindow)));
      If Scene <> Nil Then
      Begin
//        Scene.Redraw;
      End;

//      Result := 0;
}
    End;
    WM_SHOWWINDOW:
    Begin
      LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_SHOWWINDOW');
{
      Scene := TSceneGL(SceneHash.Get(GetParent(HWindow)));
      If Scene <> Nil Then
      Begin
        Scene.UpdateArea(Scene.WindowLeft,Scene.WindowTop,Scene.WindowWidth,Scene.WindowHeight);
      End;
}
    End;
    WM_SIZE:
    Begin
      If WParam = SIZENORMAL Then
      Begin
{
        LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_SIZE');
        If Not SetWindowPos(HWindow, 0, 0, 0, LParam And $FFFF, LParam Shr 16, SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE) Then
         LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_SIZE: Error = $' + IntToHex(GetLastError,8));
}
      End;

//      Result := 0;
    End;
    WM_MOVE:
    Begin
{
      LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_MOVE');
      If Not SetWindowPos(HWindow, 0, 0, 0, LParam And $FFFF, LParam Shr 16, SWP_NOSIZE + SWP_NOZORDER + SWP_NOACTIVATE) Then
       LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_MOVE: Error = $' + IntToHex(GetLastError,8));
}
//      Result := 0;
    End;
    WM_CLOSE:
    Begin
{
      PostQuitMessage(0);
      Result := 0;
}
    End;*)
  Else
//    Result := DefWindowProc(HWindow, Message, WParam, LParam);
  End; // Case

//  LogToFile('WindowProc: $' + IntToHex(Message,4));

  If SceneFullScreen Then
  Begin
    Case Message Of

      WM_LBUTTONDOWN,
      WM_RBUTTONDOWN,
      WM_MBUTTONDOWN:
      Begin
        MouseButton := mbLeft;

             If (wParam And MK_LBUTTON) <> 0 Then MouseButton := mbLeft
        Else If (wParam And MK_RBUTTON) <> 0 Then MouseButton := mbRight
        Else If (wParam And MK_MBUTTON) <> 0 Then MouseButton := mbMiddle;

             If Message = WM_RBUTTONDOWN Then MouseButton := mbRight
        Else If Message = WM_MBUTTONDOWN Then MouseButton := mbMiddle;

        LastMouseButton := MouseButton;
        Shift := [];
        If (wParam And MK_SHIFT) <> 0 Then Shift := [ssShift];
        If Assigned(OnFullScreenMouseDown) Then OnFullScreenMouseDown(Nil,MouseButton,Shift,lParam And $FFFF,lParam Shr 16);
//        FullScreenTriggerMouseDown(MouseButton,Shift,lParam And $FFFF,lParam Shr 16);
        Result := 0;
      End;
      WM_LBUTTONUP,
      WM_RBUTTONUP,
      WM_MBUTTONUP:
      Begin
        MouseButton := mbLeft;

             If (wParam And MK_LBUTTON) <> 0 Then MouseButton := mbLeft
        Else If (wParam And MK_RBUTTON) <> 0 Then MouseButton := mbRight
        Else If (wParam And MK_MBUTTON) <> 0 Then MouseButton := mbMiddle;

             If Message = WM_RBUTTONUP Then MouseButton := mbRight
        Else If Message = WM_MBUTTONUP Then MouseButton := mbMiddle;

        Shift := [];
        If (wParam And MK_SHIFT) <> 0 Then Shift := [ssShift];
        If Assigned(OnFullScreenMouseUp) Then OnFullScreenMouseUp(Nil,MouseButton,Shift,lParam And $FFFF,lParam Shr 16);
//        FullScreenTriggerMouseUp(LastMouseButton,Shift,lParam And $FFFF,lParam Shr 16);
        Result := 0;
      End;
      WM_MOUSEMOVE:
      Begin
        MouseButton := mbLeft;

             If (wParam And MK_LBUTTON) <> 0 Then MouseButton := mbLeft
        Else If (wParam And MK_RBUTTON) <> 0 Then MouseButton := mbRight
        Else If (wParam And MK_MBUTTON) <> 0 Then MouseButton := mbMiddle;
        Shift := [];
        If (wParam And MK_SHIFT) <> 0 Then Shift := [ssShift];
        If Assigned(OnFullScreenMouseMove) Then OnFullScreenMouseMove(Nil,Shift,lParam And $FFFF,lParam Shr 16);
//        FullScreenTriggerMouseMove(Shift,lParam And $FFFF,lParam Shr 16);
        Result := 0;
      End;

      WM_SETFOCUS:
      Begin
        Result := DefWindowProc(HWindow, Message, WParam, LParam);
        SceneFullScreenHasKeyboardFocus := True;
      End;
      WM_KILLFOCUS:
      Begin
        Result := DefWindowProc(HWindow, Message, WParam, LParam);
        SceneFullScreenHasKeyboardFocus := False;
      End;

      WM_KEYDOWN:
      Begin
        Shift := [];
        Key   := (lParam Shr 16) And $FF;
        If Assigned(OnFullScreenKeyDown) Then OnFullScreenKeyDown(Nil,Key,Shift);
      End;
      WM_KEYUP:
      Begin
        Shift := [];
        Key   := (lParam Shr 16) And $FF;
        If Assigned(OnFullScreenKeyUp) Then OnFullScreenKeyUp(Nil,Key,Shift);
      End;
{
      WM_KEYFIRST..WM_KEYLAST:
      Begin
        FullScreenTriggerPostMessage(Message, WParam, LParam);
        Result := DefWindowProc(HWindow, Message, WParam, LParam);
      End;
}
    Else
      Result := DefWindowProc(HWindow, Message, WParam, LParam);
    End; // Case
  End
  Else
  Begin
    If (Message >= WM_MOUSEFIRST) And (Message <= WM_MOUSELAST) Then
    Begin
      Panel := TPanel(WindowHash.Get(HWindow));
      If Panel <> Nil Then PostMessage(Panel.Parent.Handle,Message,WParam,LParam);
    End;
    Result := DefWindowProc(HWindow, Message, WParam, LParam)
  End;

//  Result := 0;

//  If (Message = WM_DESTROY) Or (Message = WM_NCDESTROY) Then Result := 0 Else
End; // WindowProc

Var LogTime0: Integer;

Procedure LogToFile(St: String; DoLog: Boolean);
Var
  FileName : String;
  F        : System.Text;

Begin
  If DoLog Then
  Begin
    Try
      LogMutex.Enter;
      FileName := ExtractFilePath(Application.ExeName) + 'glenginelog.txt';
      AssignFile(F,FileName);
      If FileExists(FileName) Then Append(F)
      Else
      Begin
        ReWrite(F);
        LogTime0 := GetTickCount;
      End;
      WriteLn(F,Format('%8d: [Thread $%.8x] %s',[GetTickCount - LogTime0,GetCurrentThreadID,St]));
      Flush(F);
      CloseFile(F);
    Finally
      LogMutex.Leave;
    End;
  End;
End; // LogToFile

Procedure LogToOCCFile(St: String);
Var
  FileName : String;
  F        : System.Text;

Begin
  Try
    LogMutex.Enter;
    FileName := ExtractFilePath(Application.ExeName) + 'glengineocclog.txt';
    AssignFile(F,FileName);
    If FileExists(FileName)
     Then Append(F)
     Else ReWrite(F);
    WriteLn(F,'[Thread $' + IntToHex(GetCurrentThreadID,8) + '] ' + St);
    Flush(F);
    CloseFile(F);
  Finally
    LogMutex.Leave;
  End;
End; // LogToOCCFile

Procedure LogGLError(St: String);
Var I: GLInt;
Begin
  I := glGetError;
  If I <> GL_NO_ERROR Then LogToFile('!!! OpenGL Error (' + St + '): ' + IntToStr(I));
End; // LogGLError

Procedure SRotateX(Angle: Single; Var X,Y,Z: Single);
Var A: Double;
Begin
  A := Cos(-Angle) * Y - Sin(-Angle) * Z;
  Z := Sin(-Angle) * Y + Cos(-Angle) * Z;
  Y := A;
End; // SRotateX

Procedure SRotateY(Angle: Single; Var X,Y,Z: Single);
Var A: Double;
Begin
  A := Cos(-Angle) * Z - Sin(-Angle) * X;
  X := Sin(-Angle) * Z + Cos(-Angle) * X;
  Z := A;
End; // SRotateY

Procedure SRotateZ(Angle: Single; Var X,Y,Z: Single);
Var A: Double;
Begin
  A := Cos(-Angle) * X - Sin(-Angle) * Y;
  Y := Sin(-Angle) * X + Cos(-Angle) * Y;
  X := A;
End; // SRotateZ

Procedure DRotateX(Angle: Double; Var X,Y,Z: Double);
Var A: Double;
Begin
  A := Cos(-Angle) * Y - Sin(-Angle) * Z;
  Z := Sin(-Angle) * Y + Cos(-Angle) * Z;
  Y := A;
End; // DRotateX

Procedure DRotateY(Angle: Double; Var X,Y,Z: Double);
Var A: Double;
Begin
  A := Cos(-Angle) * Z - Sin(-Angle) * X;
  X := Sin(-Angle) * Z + Cos(-Angle) * X;
  Z := A;
End; // DRotateY

Procedure DRotateZ(Angle: Double; Var X,Y,Z: Double);
Var A: Double;
Begin
  A := Cos(-Angle) * X - Sin(-Angle) * Y;
  Y := Sin(-Angle) * X + Cos(-Angle) * Y;
  X := A;
End; // DRotateZ

Procedure glWindowPos4fMESAemulate(X,Y,Z,W: GLFloat);
Var FX,FY: GLFloat;
Begin
  // Push current matrix mode and viewport attributes

  glPushAttrib(GL_TRANSFORM_BIT Or GL_VIEWPORT_BIT);

    // Setup projection parameters

    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
      glLoadIdentity;
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix;
        glLoadIdentity;
        glDepthRange(Z,Z);
        glViewport(Trunc(X) - 1,Trunc(Y) - 1,2,2);

        // Set the raster (window) position

        FX := Frac(X);
        FY := Frac(Y);
        glRasterPos4f(FX,FY,0,W);

        // Restore matrices, viewport, and matrix mode

      glPopMatrix;
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
  glPopAttrib;
End; // glWindowPos4fMESAemulate

Procedure glWindowPos2fMESAemulate(X,Y: GLFloat);
Begin
  glWindowPos4fMESAemulate(X,Y,0,1);
End; // glWindowPos2fMESAemulate

// This is to avoid certain problems with Borland tools.  Don´t call them yourself
Function MaskExceptions: Pointer;
Var OldMask: Pointer;
Begin
  Asm
    fnstcw WORD PTR OldMask;
    mov eax, OldMask;
    or eax, $3f;
    mov WORD PTR OldMask+2,ax;
    fldcw WORD PTR OldMask+2;
  End; // Asm
  Result := OldMask;
End; // MaskExceptions

Procedure UnmaskExceptions(OldMask: Pointer);
Begin
  Asm
    fnclex;
    fldcw WORD PTR OldMask;
  End; // Asm
End; // UnmaskExceptions

// This function can convert a BMP file into a RGBA file
//   BMPName:     Name of the BMP file
//   RGBAName:    Name of the new file
//   Transparent: Color that will be treated as transparent
Function  ConvertBMP(BMPName: String; RGBAName: String; Transparent: LongInt): Integer;
Begin
  // This will convert a RGB bmp to a RGBA bmp, it is not ready yet
  Result := 0;
End; // ConvertBMP

// ---------------------------
// TRenderMessage
// ---------------------------

Constructor TRenderMessage.Create(AType: TRenderMessageType; P1,P2,P3,P4: LongInt; WaitUntilFinished: Boolean);
Begin
  Msg  := AType;
  L1   := P1;
  L2   := P2;
  L3   := P3;
  L4   := P4;
  Done := False;
  Wait := WaitUntilFinished;
End; // TRenderMessage.Create

// ---------------------------
// TRenderThread
// ---------------------------

//PROFILE-NO
Constructor TRenderThread.Create(SceneGL: TSceneGL);
Begin
  LogToFile('TRenderThread.Create() begin');
  Inherited Create(True); // Always create suspended
  FSceneGL          := SceneGL;
  FRunning          := False;
  FMessages         := TStringList.Create;
  FFreeTexQueue     := TStringList.Create;
  FQueueMutex       := TCriticalSection.Create;
  FWindowHandle     := 0;
  RenderThreadMutex := TCriticalSection.Create;

  AttachThreadInput(ThreadID,MainThreadID,True);

  LogToFile('TRenderThread.Create() end');
End; // TRenderThread.Create

Destructor TRenderThread.Destroy;
Var I: Integer;
Begin
  LogToFile('TRenderThread.Destroy() begin');
  For I := 0 To FMessages.Count - 1 Do FMessages.Objects[I].Free;
  FMessages.Free;
  RenderThreadMutex.Free;
  FQueueMutex.Free;
  FFreeTexQueue.Free;
  LogToFile('TRenderThread.Destroy() end');
End; // TRenderThread.Destroy

Procedure TRenderThread.SetWindowHandle(iHandle: THandle);
Begin
  Try
    RenderThreadMutex.Enter;
    Try
//      FRunning      := True;
      FWindowHandle := iHandle;
    Except
      ShowMessage('Exception in TRenderThread.SetWindowHandle');
    End;
  Finally
    RenderThreadMutex.Leave;
  End;
End; // TRenderThread.SetWindowHandle

Procedure TRenderThread.Execute;
Var
  Msg  : TRenderMessage;
  Wait : Boolean;
  WMsg : TMsg;

Begin
  Try
    LogToFile('TRenderThread.Execute() begin');
    While Not Terminated Do
    Begin
      // Must set FRunning to True before going after the thread mutex

      FRunning := True;

      // Process any window messages

      Try
        RenderThreadMutex.Enter;
        Try
          // Free any textures queued to be freed

          If FFreeTexQueue.Count > 0 Then
          Begin
            FSceneGL.DoFreeTextures(FFreeTexQueue);
            FFreeTexQueue.Clear;
          End;

          // Process messages

          If FWindowHandle <> 0 Then
          Begin
            While PeekMessage(WMsg, FWindowHandle, 0, 0, PM_REMOVE) Do
            Begin
              TranslateMessage(WMsg);
              DispatchMessage(WMsg);
            End; // While
          End;
        Except
          ShowMessage('Exception in TRenderThread.Execute while processing OpenGL window messages');
        End;
      Finally
        RenderThreadMutex.Leave;
      End;

      // Process queued messages

      Try
        Msg := Nil;
        Try
          Try
            FQueueMutex.Enter;
            If FMessages.Count > 0 Then
            Begin
              Msg := TRenderMessage(FMessages.Objects[0]);
              FMessages.Delete(0);
            End;
          Finally
            FQueueMutex.Leave;
          End;
        Except
          ShowMessage('Exception in TRenderThread.Execute while trying to fetch message from queue');
        End;
        Wait := False;
        If Msg <> Nil Then
        Begin
          Try
            Wait := Msg.Wait;
            ProcessMessage(Msg);
          Except
            On E: Exception Do
             ShowMessage('Caught exception in TRenderThread.Execute while executing TRenderThread.ProcessMessage: message type was ' + IntToStr(Integer(Msg.Msg)) + #13#10 +
                         'Exception message was ' + E.Message);
          End;
          Try
            If Not Wait Then Msg.Free;
          Except
            ShowMessage('Exception in TRenderThread.Execute while either checking Msg.Wait or disposing of Msg');
          End;
        End
        Else
        Begin
          Try
            Sleep(1);
          Except
            ShowMessage('Exception in TRenderThread.Execute while trying to sleep');
          End;
        End;
      Except
        ShowMessage('Exception in TRenderThread.Execute loop');
      End;
    End;// While
    FRunning := False;
  Except
    ShowMessage('Exception in TRenderThread.Execute');
  End;
  LogToFile('TRenderThread.Execute() end');
End; // TRenderThread.Execute

Procedure TRenderThread.AddMessage(Msg: TRenderMessage; AllowDuplicates: Boolean);
Const Timeout = 10000;
Var
  Added : Boolean;
  Count : Integer;
  Wait  : Boolean;
  AddIt : Boolean;
//  I     : Integer;
  Found : Boolean;

Begin
  Try
    FQueueMutex.Enter;
    If CreateDummyFinished And Suspended Then Resume;

{
    LogToFile('TRenderThread.AddMessage(): Msg = ' + IntToStr(Integer(Msg.Msg)) +
              ', Wait = ' + BoolToStr(Msg.Wait,True)   , true);
}

    Wait  := Msg.Wait;
    Added := False;
    If Not AllowDuplicates Then
    Begin
      Found := (FMessages.IndexOf(RenderMessageID[Msg.Msg]) >= 0);
      AddIt := Not Found;
    End
    Else AddIt := True;
    If AddIt Then
    Begin
      FMessages.AddObject(RenderMessageID[Msg.Msg],Msg);
      Added := True;
    End
    Else Msg.Free;
  Finally
    FQueueMutex.Leave;
  End;

  If Added And Wait Then
  Begin
    Count := Timeout;
    While (Count > 0) And Not Msg.Done Do
    Begin
      Sleep(1);
      If (Not (Msg.Msg In [rmAddLight,rmRedraw])) And ((Count And $F) = 0) Then Application.ProcessMessages;
      Dec(Count);
    End; // While
    If Not Msg.Done Then
     LogToFile('RenderTnread timeout!!! msg = ' + IntToStr(Integer(Msg.Msg)));
    Msg.Free;
  End;

End; // TRenderThread.AddMessage

Procedure TRenderThread.ProcessMessage(Msg: TRenderMessage);
Begin
  Try

//    LogToFile('Processing message ' + IntToStr(Integer(Msg.Msg)), true);

    Case Msg.Msg Of
      rmRedraw:          FSceneGL.DoRedraw;
      rmInitDC:          FSceneGL.DoInitDC;
      rmInitRC:          FSceneGL.DoInitRC;
      rmReleaseRC:       FSceneGL.DoReleaseRC;
      rmUpdateArea:      FSceneGL.DoUpdateArea(Msg.L1,Msg.L2,Msg.L3,Msg.L4);
      rmSetActive:       FSceneGL.DoSetActive(Boolean(Msg.L1));
      rmLoadTexture:     FSceneGL.DoLoadTexture(TTexture(Msg.L1));
      rmAddLight:        FSceneGL.DoAddLight(Msg.L1);
      rmFreeTexture:     FSceneGL.DoFreeTexture(TTexture(Msg.L1));
      rmFreeAllTextures: FSceneGL.DoFreeAllTextures;
      rmMakeImagePanel:  FSceneGL.DoMakeImagePanel(PPanel(Msg.L1));
    End; // Case
  Finally

//    LogToFile('Message ' + IntToStr(Integer(Msg.Msg)) + ' has finished', true);

    Msg.Done := True;
  End;
End; // TRenderThread.ProcessMessage

Function TRenderThread.ContainsMessage(AType: TRenderMessageType): Boolean;
Begin
  Try
    Result := False;
    FQueueMutex.Enter;
    Result := (FMessages.IndexOf(RenderMessageID[AType]) >= 0);
  Finally
    FQueueMutex.Leave;
  End;
End; // TRenderThread.ContainsMessage

Procedure TRenderThread.Redraw;
Begin
  AddMessage(TRenderMessage.Create(rmRedraw,0,0,0,0,False),False);
End; // TRenderThread.Redraw

Procedure TRenderThread.InitDC;
Begin
  AddMessage(TRenderMessage.Create(rmInitDC,0,0,0,0,False),False);
End; // TRenderThread.InitDC

Procedure TRenderThread.InitRC;
Begin
  AddMessage(TRenderMessage.Create(rmInitRC,0,0,0,0,False),False);
End; // TRenderThread.InitRC

Procedure TRenderThread.ReleaseRC;
Begin
  AddMessage(TRenderMessage.Create(rmReleaseRC,0,0,0,0,True),False);
End; // TRenderThread.ReleaseRC

Procedure TRenderThread.UpdateArea(Left,Top,Width,Height: Integer);
Begin
  AddMessage(TRenderMessage.Create(rmUpdateArea,Left,Top,Width,Height,false),False);
End; // TRenderThread.UpdateArea

Procedure TRenderThread.SetActive(B: Boolean);
Begin
  AddMessage(TRenderMessage.Create(rmSetActive,LongWord(B),0,0,0,False),False);
End; // TRenderThread.SetActive

Procedure TRenderThread.LoadTexture(Texture: TTexture);
Begin
  AddMessage(TRenderMessage.Create(rmLoadTexture,LongWord(Texture),0,0,0,True),True);
End; // TRenderThread.LoadTexture
{
Procedure TRenderThread.AddLight(Num: Integer);
Begin
  AddMessage(TRenderMessage.Create(rmAddLight,Num,0,0,0,True),True);
End; // TRenderThread.LoadTexture
}
Procedure TRenderThread.FreeTexture(Texture: TTexture);
Begin
  AddMessage(TRenderMessage.Create(rmFreeTexture,LongWord(Texture),0,0,0,True),True);
End; // TRenderThread.FreeTexture

Procedure TRenderThread.FreeEntity(Entity: TEntity);
Begin
  AddMessage(TRenderMessage.Create(rmFreeEntity,LongWord(Entity),0,0,0,True),True);
End; // TRenderThread.FreeEntity

Procedure TRenderThread.FreeAllTextures;
Begin
  AddMessage(TRenderMessage.Create(rmFreeAllTextures,0,0,0,0,True),False);
End; // TRenderThread.FreeAllTextures

Procedure TRenderThread.FreeAllEntities;
Begin
  AddMessage(TRenderMessage.Create(rmFreeAllEntities,0,0,0,0,True),False);
End; // TRenderThread.FreeAllEntities

Procedure TRenderThread.MakeImagePanel(Panel: PPanel);
Begin
  // We have to wait for the result or TGLVisir.Init() won't have a valid window handle

  AddMessage(TRenderMessage.Create(rmMakeImagePanel,LongWord(Panel),0,0,0,{False}true{True}),False);
End; // TRenderThread.MakeImagePanel

Procedure TRenderThread.QueueFreeTextures(List: TStringList);
Var I: Integer;
Begin
  Try
    RenderThreadMutex.Enter;
    For I := 0 To List.Count - 1 Do
    Begin
      If List.Objects[I] <> Nil Then FFreeTexQueue.AddObject('',List.Objects[I]);
    End; // For I
  Finally
    RenderThreadMutex.Leave;
  End;
End; // TRenderThread.QueueFreeTextures
//PROFILE-YES

// ---------------------------
// TOcclusionThread
// ---------------------------

Constructor TOcclusionThread.Create(SceneGL: TSceneGL);
Begin
  Inherited Create(True); // Always create suspended
  FreeOnTerminate := False;
  FSceneGL := SceneGL;
End; // TOcclusionThread.Create

Procedure TOcclusionThread.Execute;
Begin
  FSceneGL.DoOcclusion;
End; // TOcclusionThread.Execute

// ---------------------------
// TCamera
// ---------------------------

Constructor TCamera.Create(Scene: TSceneGL);
Begin
  Inherited Create;
  Owner    := Scene;
//  Entity   := TEntity.Create(Owner);//TEntity(Owner.Entities.GetNew(0));
//  Entity.Visible := False;
  Position    := T3DPoint.Create{Point};
  SceneCenter := T3DPoint.Create{Point};
  UpVector    := T3DPoint.Create{Point};
  Frustum  := TFrustum.Create(0,0,8,0,0,-100,0,1,0,Scene.DistNear,Scene.DistFar,Scene.Angle,Scene.WindowWidth,Scene.WindowHeight);
  LookAt(0,0,-100);    // The camera is looking to the bottom of the screen
  SetVectorUp(0,1,0);  // The camera is rotated normally
  SetPosition(0,0,8);  // The camera is in the center of the space
End; // TCamera.Create

Destructor TCamera.Destroy;
Begin
//  Entity.Free;
  Frustum.Free;
  Position.Free;
  SceneCenter.Free;
  UpVector.Free;
//  Position    := Nil;
//  SceneCenter := Nil;
//  UpVector    := Nil;
End; // TCamera.Destroy

Procedure TCamera.Redraw(DoOpenGL: Boolean);
Var NearDist,FarDist,Angle: Single;
Begin
  NearDist          := Frustum.NearDist;
  FarDist           := Frustum.FarDist;
  Angle             := Frustum.ViewAngle;
  Frustum.NearDist  := Owner.DistNear;
  Frustum.FarDist   := Owner.DistFar;
  Frustum.ViewAngle := Owner.Angle;
  If (NearDist <> Frustum.NearDist) Or (FarDist <> Frustum.FarDist) Or (Angle <> Frustum.ViewAngle) Then
  Begin
    Frustum.SetupPlanes;
    Owner.DoUpdateArea(Owner.WindowLeft,Owner.WindowTop,Owner.WindowWidth,Owner.WindowHeight);
  End;
  If DoOpenGL Then
  Begin
    GluLookAt(Position.X,    Position.Y,    Position.Z,
              SceneCenter.X, SceneCenter.Y, SceneCenter.Z,
              UpVector.X,    UpVector.Y,    UpVector.Z);
  End;
End; // TCamera.Redraw

Procedure TCamera.SetPosition(ix,iy,iz: GLDouble);
Begin
  // Don't set Position's dirty flag if we don't have to, so TSceneGL.DoRedraw() won't have to
  // traverse the whole BSP tree to find the camera node 

  If Not Position.Equals(IX,IY,IZ) Then
  Begin
    Position.Copy(IX,IY,IZ);
    Frustum.SetObserver(Position);
    Owner.OcclusionManager.Observer[0] := IX;
    Owner.OcclusionManager.Observer[1] := IY;
    Owner.OcclusionManager.Observer[2] := IZ;
  End;
End; // TCamera.SetPosition

Procedure TCamera.LookAt(ix,iy,iz: GLDouble);
Begin
  SceneCenter.Copy(iX,iY,iZ);
  Frustum.SetLookAt(iX,iY,iZ);
End; // TCamera.LookAt

Procedure TCamera.SetVectorUp(ix,iy,iz: GLDouble);
Begin
  UpVector.Copy(iX,iY,iZ);
  Frustum.SetUp(iX,iY,iZ);
End; // TCamera.SetVectorUp

Procedure TCamera.SetPosLookAtUp(PX,PY,PZ,LX,LY,LZ,UX,UY,UZ: GLDouble);
Var SamePos: Boolean;
Begin
  // Don't set Position's dirty flag if we don't have to, so TSceneGL.DoRedraw() won't have to
  // traverse the whole BSP tree to find the camera node

  SamePos := Position.Equals(PX,PY,PZ);
  If Not (SamePos And SceneCenter.Equals(LX,LY,LZ) And UpVector.Equals(UX,UY,UZ)) Then
  Begin
    If Not SamePos Then Position.Copy(PX,PY,PZ);
    SceneCenter.Copy(LX,LY,LZ);
    UpVector.Copy(UX,UY,UZ);
    Owner.OcclusionManager.Observer[0] := PX;
    Owner.OcclusionManager.Observer[1] := PY;
    Owner.OcclusionManager.Observer[2] := PZ;
    Frustum.SetPosLookAtUp(PX,PY,PZ,LX,LY,LZ,UX,UY,UZ);
  End;
End; // TCamera.SetPosLookAtUp

// ---------------------------
// TOcclusionManager
// ---------------------------

Constructor TOcclusionManager.Create(AOwner: TSceneGL);
Begin
  FOwner         := AOwner;
  FWidth         := 0;
  FHeight        := 0;
  FMatrix        := T4x4Matrix.Create(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
  FRotateXMatrix := T4x4Matrix.Create(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
  FRotateYMatrix := T4x4Matrix.Create(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
  FRotateZMatrix := T4x4Matrix.Create(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1);
  FZBufferSize   := 0;
  FIdentity      := True;
  Frustum        := Nil;
  Observer       := TFVectorExt.BuildVector(4);
  FaceVerts      := TFVectorExt.BuildVector(4);
  FaceNorms      := TFVectorExt.BuildVector(4);
//  FaceVerts1     := TFVectorExt(TFVectorExt.BuildUsingAlignedArrayPart(FaceVerts.DataArray,4));
  Observer[0]    := 0;
  Observer[1]    := 0;
  Observer[2]    := 0;
  Observer[3]    := 0;

  Enabled        := True;//False;

  Setup(640,480);
End; // TOcclusionManager.Create

Destructor TOcclusionManager.Destroy;
Begin
  FMatrix.Free;
  FRotateXMatrix.Free;
  FRotateYMatrix.Free;
  FRotateZMatrix.Free;
  SetLength(FZBuffer,0);
  SetLength(FaceIndices,0);
  Observer.Free;
//  FaceVerts1.Free;
  FaceVerts.Free;
  FaceNorms.Free;
End; // TOcclusionManager.Destroy

Procedure TOcclusionManager.ClearZBuffer;
//Var I: Integer;
Var
  P1,P2 : PLongWord;
  I1,I2 : Integer;

Begin
  P1 := @(FZBuffer[0]);
  P2 := @(FGrid[0]);
  I1 := High(FZBuffer) + 1;
  I2 := High(FGrid) + 1;
  Asm
    MOV   EAX, 99999 * 256;

    MOV   ECX,I1
    MOV   EDX,P1
@L1:
    MOV   [EDX],EAX
    ADD   EDX,4
    DEC   ECX
    JNZ   @L1

    MOV   ECX,I2
    MOV   EDX,P2
@L2:
    MOV   [EDX],EAX
    ADD   EDX,4
    DEC   ECX
    JNZ   @L2
  End; // Asm
//  For I := 0 To High(FZBuffer) Do FZBuffer[I] := 99999 * 256;
//  For I := 0 To High(FGrid)    Do FGrid[I]    := 99999 * 256;
End; // TOcclusionManager.ClearZBuffer

Procedure TOcclusionManager.LoadIdentity;
Begin
  FMatrix.LoadIdentity;
  FIdentity := True;
End; // TOcclusionManager.LoadIdentity

Procedure TOcclusionManager.XRotate(Angle: Single);
// Angle must be in degrees
Var C,S: Single;
Begin
  If Angle <> 0 Then
  Begin
    C := Cos(-Angle * Pi / 180);
    S := Sin(-Angle * Pi / 180);
    FRotateXMatrix.M[2,2] := C;
    FRotateXMatrix.M[2,3] := S;
    FRotateXMatrix.M[3,2] := -S;
    FRotateXMatrix.M[3,3] := C;
    FMatrix.Multiply(FRotateXMatrix);
    FIdentity := False;
  End;
End; // TOcclusionManager.XRotate

Procedure TOcclusionManager.YRotate(Angle: Single);
// Angle must be in degrees
Var C,S: Single;
Begin
  If Angle <> 0 Then
  Begin
    C := Cos(-Angle * Pi / 180);
    S := Sin(-Angle * Pi / 180);
    FRotateYMatrix.M[1,1] := C;
    FRotateYMatrix.M[1,3] := -S;
    FRotateYMatrix.M[3,1] := S;
    FRotateYMatrix.M[3,3] := C;
    FMatrix.Multiply(FRotateYMatrix);
    FIdentity := False;
  End;
End; // TOcclusionManager.YRotate

Procedure TOcclusionManager.ZRotate(Angle: Single);
// Angle must be in degrees
Var C,S: Single;
Begin
  If Angle <> 0 Then
  Begin
    C := Cos(-Angle * Pi / 180);
    S := Sin(-Angle * Pi / 180);
    FRotateZMatrix.M[1,1] := C;
    FRotateZMatrix.M[1,2] := S;
    FRotateZMatrix.M[2,1] := -S;
    FRotateZMatrix.M[2,2] := C;
    FMatrix.Multiply(FRotateZMatrix);
    FIdentity := False;
  End;
End; // TOcclusionManager.ZRotate

Procedure TOcclusionManager.Rotate(P: T3DPoint{I3dPoint});
Begin
  XRotate(P.X);
  YRotate(P.Y);
  ZRotate(P.Z);
End; // TOcclusionManager.Rotate

Procedure TOcclusionManager.Translate(P: T3DPoint{I3dPoint});
Begin
  If Not P.IsZero Then
  Begin
    FMatrix.M[1,4] := FMatrix.M[1,4] + P.X;
    FMatrix.M[2,4] := FMatrix.M[1,4] + P.Y;
    FMatrix.M[3,4] := FMatrix.M[1,4] + P.Z;
    FIdentity := False;
  End;
End; // TOcclusionManager.Translate

Procedure TOcclusionManager.Scale(P: T3DPoint{I3dPoint});
Var I: Integer;
Begin
  If Not P.IsOne Then
  Begin
    For I := 1 To 4 Do
    Begin
      FMatrix.M[1,I] := FMatrix.M[1,I] * P.X;
      FMatrix.M[2,I] := FMatrix.M[2,I] * P.Y;
      FMatrix.M[3,I] := FMatrix.M[3,I] * P.Z;
    End; // For I
    FIdentity := False;
  End;
End; // TOcclusionManager.Scale

Procedure TOcclusionManager.Setup(AWidth,AHeight: Integer);
Var Size: Integer;
Begin
  Size    := AWidth * AHeight;
  FWidth  := AWidth;
  FHeight := AHeight;
  If High(FZBuffer) <> Size - 1 Then
  Begin
    SetLength(FZBuffer,Size);
    FZBufferSize := Size * SizeOf(Single);
    FillChar(FZBuffer[0],FZBufferSize,0);
    FGridWidth  := AWidth  Shr 5;
    FGridHeight := AHeight Shr 5;
    If FGridWidth  Shl 5 < AWidth  Then Inc(FGridWidth);
    If FGridHeight Shl 5 < AHeight Then Inc(FGridHeight);
    SetLength(FGrid,FGridWidth * FGridHeight);
  End;
End; // TOcclusionManager.Setup

Procedure TOcclusionManager.SSE_3DNow_RenderTriangleToZBuffer(Model: TModel);
Const SMALL_FACE = 8;
Var
  I,J,K  ,L      : Integer;
  NumFaces     : Integer;
  NumFaces1    : Integer;
  ToAbsMatrix  : TFVector;
  PVert        : PVector3;
  PNorm        : PVector3;

  Polygon      : TSimplePolygon;
  CPolygon     : TSimplePolygon;
  SPolygon     : TSimpleScreenPolygon;
  MinPt        : TSimpleScreenPoint;
  MaxPt        : TSimpleScreenPoint;
  SPoint       : TPoint;
  FW           : Integer;
  FH           : Integer;
  FW1          : Integer;
  FH1          : Integer;
  FW2          : Integer;
  FH2          : Integer;

  PVerts       : PSingleArray;
  PVertsJ      : PSingleArray;
  PNormsK      : PSingleArray;
  PLookup      : PLongWordArray;
  DNear        : Single;
  DFar         : Single;
  DThreshold   : Single;
  PXYZ0        : SPtr;
  PXYZ1        : SPtr;
  PXYZ2        : SPtr;
  PXYZ3        : SPtr;
  PFNXYZ0      : SPtr;
  PFNXYZ       : SPtr;
  SOne         : Single;

Begin
  If Not FIdentity Then Exit;

  // Find out how many faces we will do

  NumFaces := 0;
  For I := 0 To High(Model.Faces) Do
  Begin
    If (Model.Faces[I].Flags And (ffHasAlpha Or ffHasTrans)) = 0 Then Inc(NumFaces);
  End; // For I

  // Continue only if there are faces to process

  If NumFaces > 0 Then
  Begin
    SetLength(FaceIndices,NumFaces);

    // Allocate vertices for each face's vertex (assume each face is a triangle -- no quads)

    FaceVerts.SetLength(NumFaces * 12);
//    FaceVerts1.UpdateUsingAlignedArrayPart(FaceVerts.DataArray,NumFaces * 12);
    FaceNorms.SetLength(NumFaces * 4);

    PVerts  := FaceVerts.DataArray;
    PVertsJ := PVerts;
    PNormsK := FaceNorms.DataArray;

    // Copy the vertices to our new array

    SOne    := 1;
    PXYZ0   := Pointer(Model.Positions.DataArray);
    PFNXYZ0 := Pointer(Model.FNormals.DataArray);
    J       := 0;
    K       := 0;

    L := 0;

    For I := 0 To High(Model.Faces) Do
    Begin
      Model.Faces[I].Flags := Model.Faces[I].Flags And Not ffHidden;

      // Don't bother with faces that are transparent in one way or another

      If (Model.Faces[I].Flags And (ffHasAlpha Or ffHasTrans)) = 0 Then
      Begin
        FaceIndices[L] := I;
        Inc(L);

        PXYZ1  := Pointer(Integer(PXYZ0) + K{Face.Vertices[0]} * 12);
        PXYZ2  := Pointer(Integer(PXYZ0) + (K + 1){Face.Vertices[1]} * 12);
        PXYZ3  := Pointer(Integer(PXYZ0) + (K + 2){Face.Vertices[2]} * 12);
        PFNXYZ := Pointer(Integer(PFNXYZ0) + J);
{
        PXYZ1  := @(Model.Vertices[Face.Vertices[0]].Position.X);
        PXYZ2  := @(Model.Vertices[Face.Vertices[1]].Position.X);
        PXYZ3  := @(Model.Vertices[Face.Vertices[2]].Position.X);
        PFNXYZ := @(Face.Normal.X);
}
        Asm
          PUSH  EBX

          MOV   EBX,SOne
          MOV   EAX,PVertsJ

          MOV   EDX,PXYZ1
          MOV   ECX,[EDX]      // X0
          MOV   [EAX],ECX      //     PVerts[<index> + 0]
          MOV   ECX,[EDX+4]    // Y0
          MOV   [EAX+4],ECX    //     PVerts[<index> + 1]
          MOV   ECX,[EDX+8]    // Z0
          MOV   [EAX+8],ECX    //     PVerts[<index> + 2]
          MOV   [EAX+12],EBX   // 1   PVerts[<index> + 3]

          MOV   EDX,PXYZ2
          MOV   ECX,[EDX]      // X1
          MOV   [EAX+16],ECX   //     PVerts[<index> + 4]
          MOV   ECX,[EDX+4]    // Y1
          MOV   [EAX+20],ECX   //     PVerts[<index> + 5]
          MOV   ECX,[EDX+8]    // Z1
          MOV   [EAX+24],ECX   //     PVerts[<index> + 6]
          MOV   [EAX+28],EBX   // 1   PVerts[<index> + 7]

          MOV   EDX,PXYZ3
          MOV   ECX,[EDX]      // X2
          MOV   [EAX+32],ECX   //     PVerts[<index> + 8]
          MOV   ECX,[EDX+4]    // Y2
          MOV   [EAX+36],ECX   //     PVerts[<index> + 9]
          MOV   ECX,[EDX+8]    // Z2
          MOV   [EAX+40],ECX   //     PVerts[<index> + 10]
          MOV   [EAX+44],EBX   // 1   PVerts[<index> + 11]

          // --------------------------------------

          MOV   EAX,PNormsK
          MOV   EDX,PFNXYZ
          MOV   ECX,[EDX]      // X
          MOV   [EAX],ECX      //     PNorms[<index> + 0]
          MOV   ECX,[EDX+4]    // Y
          MOV   [EAX+4],ECX    //     PNorms[<index> + 1]
          MOV   ECX,[EDX+8]    // Z
          MOV   [EAX+8],ECX    //     PNorms[<index> + 2]
          MOV   [EAX+12],EBX   // 1   PNorms[<index> + 3]

          POP   EBX
        End; // Asm
        Inc(LongWord(PVertsJ),12 * 4);
        Inc(LongWord(PNormsK),4 * 4);
      End;
      Inc(J,12);
      Inc(K,3);
    End; // For I

    // Subtract the observer's position from all of the vertices

    FaceVerts.SubSingleVec(Observer);

    // Transform the normals if we have to (doesn't work properly yet, so we won't get in here for now)

    If Not FIdentity Then
    Begin
      // Build a transposed matrix that is adjusted to be relative to the observer

      ToAbsMatrix     := TFVectorExt.BuildVector(16);
      ToAbsMatrix[ 0] := FMatrix.M[1,1];
      ToAbsMatrix[ 1] := FMatrix.M[2,1];
      ToAbsMatrix[ 2] := FMatrix.M[3,1];
      ToAbsMatrix[ 3] := FMatrix.M[4,1];
      ToAbsMatrix[ 4] := FMatrix.M[1,2];
      ToAbsMatrix[ 5] := FMatrix.M[2,2];
      ToAbsMatrix[ 6] := FMatrix.M[3,2];
      ToAbsMatrix[ 7] := FMatrix.M[4,2];
      ToAbsMatrix[ 8] := FMatrix.M[1,3];
      ToAbsMatrix[ 9] := FMatrix.M[2,3];
      ToAbsMatrix[10] := FMatrix.M[3,3];
      ToAbsMatrix[11] := FMatrix.M[4,3];
      ToAbsMatrix[12] := 0;//FMatrix.M[1,4] - OX;
      ToAbsMatrix[13] := 0;//FMatrix.M[2,4] - OY;
      ToAbsMatrix[14] := 0;//FMatrix.M[3,4] - OZ;
      ToAbsMatrix[15] := FMatrix.M[4,4];
      FaceNorms.MulMatrix(ToAbsMatrix);
      ToAbsMatrix.Free;
    End;

    // Find out how many faces aren't backfaces and condense to just those

    NumFaces1 := 0;
    PVert     := PVector3(FaceVerts.DataArray);
    PNorm     := PVector3(FaceNorms.DataArray);
    J         := 0;
    PLookup   := PLongWordArray(PNorm); // We're going to reuse the normal array because we don't need it after this check
    For I := 0 To NumFaces - 1 Do
    Begin
      K := FaceIndices[I];
      If fDotProduct(PVert^,PNorm^) < 0 Then
      Begin
        PLookup[NumFaces1] := J;
        Inc(NumFaces1);
      End
      Else Model.Faces[K].Flags := Model.Faces[K].Flags Or ffHidden;
      Inc(LongWord(PVert),12 * 4);   // Four singles per vertex, three vertices per face
      Inc(LongWord(PNorm), 4 * 4);
      Inc(J,12);
    End; // For I

    // Continue only if some faces are potentially visible

    If NumFaces1 > 0 Then
    Begin
      // Transform and semi-project the vertices (will have to divide by z later)

      FaceVerts.MulMatrix(Frustum.EViewMatrix);

      // Determine the minimum and maximum Z values and store them in the unused areas
      // (we allocated four singles -- x,y,z,w -- per vertex for this reason).

      For K := 0 To NumFaces1 - 1 Do
      Begin
        I := PLookup[K] + 2;                           // Point to the Z component
        If PVerts[I] < PVerts[I + 4] Then              // If Z0 < Z1
        Begin
          If PVerts[I] < PVerts[I + 8]                 // If Z0 < Z2
           Then PVerts[I + 1] := PVerts[I]
           Else PVerts[I + 1] := PVerts[I + 8];
          If PVerts[I + 4] < PVerts[I + 8]             // If Z1 < Z2
           Then PVerts[I + 5] := PVerts[I + 8]
           Else PVerts[I + 5] := PVerts[I + 4];
        End
        Else
        Begin
          If PVerts[I + 4] < PVerts[I + 8]             // If Z1 < Z2
           Then PVerts[I + 1] := PVerts[I + 4]
           Else PVerts[I + 1] := PVerts[I + 8];
          If PVerts[I] < PVerts[I + 8]                 // If Z0 < Z2
           Then PVerts[I + 5] := PVerts[I + 8]
           Else PVerts[I + 5] := PVerts[I];
        End;
      End; // For K

      // Process each polygon

      FW    := FWidth;
      FH    := FHeight;
      FW1   := FW - 1;
      FH1   := FH - 1;
      FW2   := FW Div 2;
      FH2   := FH Div 2;
      DNear := FOwner.DistNear;
      DFar  := FOwner.DistFar;
      DThreshold := DNear + (DFar - DNear) / 2;
      For K := 0 To NumFaces1 - 1 Do
      Begin
        J := PLookup[K];

        // Make sure that at least something is visible

        If (PVerts[J + 7] >= DNear) And        // MaxZ
           (PVerts[J + 3] <  DFar)  Then       // MinZ 
        Begin
          // Clip the polygon to the frustum's near plane...but only if we have to

          If PVerts[J + 3] < DNear Then // MinZ
          Begin
            // The polygon crosses the near plane

            Polygon.NumPoints   := 3;
            Polygon.Points[0].X := PVerts[J +  0];
            Polygon.Points[0].Y := PVerts[J +  1];
            Polygon.Points[0].Z := PVerts[J +  2];
            Polygon.Points[1].X := PVerts[J +  4];
            Polygon.Points[1].Y := PVerts[J +  5];
            Polygon.Points[1].Z := PVerts[J +  6];
            Polygon.Points[2].X := PVerts[J +  8];
            Polygon.Points[2].Y := PVerts[J +  9];
            Polygon.Points[2].Z := PVerts[J + 10];

            Frustum.ClipTransformedPolygonToNearDist(Polygon,@CPolygon);

            // Make sure there's something left to render

            If CPolygon.NumPoints > 2 Then
            Begin
              // Project the coordinates to screen coordinates and get the screen extents

              MinPt.X := FW;
              MinPt.Y := FH;
              MaxPt.X := -1;
              MaxPt.Y := -1;
              SPolygon.NumPoints := CPolygon.NumPoints;
              For I := 0 To CPolygon.NumPoints - 1 Do
              Begin
                SPoint.X := Round(CPolygon.Points[I].X / CPolygon.Points[I].Z) + FW2;
                SPoint.Y := Round(CPolygon.Points[I].Y / CPolygon.Points[I].Z) + FH2;
                Asm
                  MOV   EAX,SPoint.X
                  MOV   ECX,SPoint.Y
                  CMP   EAX,MinPt.X
                  JGE   @NotMinX
                  MOV   MinPt.X,EAX
@NotMinX:
                  CMP   ECX,MinPt.Y
                  JGE   @NotMinY
                  MOV   MinPt.Y,ECX
@NotMinY:
                  CMP   EAX,MaxPt.X
                  JLE   @NotMaxX
                  MOV   MaxPt.X,EAX
@NotMaxX:
                  CMP   ECX,MaxPt.Y
                  JLE   @NotMaxY
                  MOV   MaxPt.Y,ECX
@NotMaxY:
                End; // Asm
                SPolygon.Points[I].X := SPoint.X;
                SPolygon.Points[I].Y := SPoint.Y;
                SPolygon.Points[I].Z := Round(CPolygon.Points[I].Z * 256);
              End; // For I

              // Make sure at least part of the polygon is visible

              If (MaxPt.X >= 0) And (MaxPt.Y >= 0) And (MinPt.X <= FW1) And (MinPt.Y <= FH1) Then
              Begin
                // Clip the polygon to the screen...but only if we have to

                If (MinPt.X < 0) Or (MinPt.Y < 0) Or (MaxPt.X > FW1) Or (MaxPt.Y > FH1) Then
                Begin
                  Frustum.ClipIntegerPolygonToScreen(SPolygon);
                End;

                // Make sure there's something left to render

                If SPolygon.NumPoints > 2 Then FillSimpleScreenPolygon(SPolygon,MaxPt.X - MinPt.X > MaxPt.Y - MinPt.Y);
              End;
            End;
          End
          Else
          Begin
            // The polygon doesn't cross the near plane -- we will get here most of the time

            MinPt.X := FW;
            MinPt.Y := FH;
            MaxPt.X := -1;
            MaxPt.Y := -1;
            SPolygon.NumPoints := 3;

            // Finish projecting vertex 1 to screen coordinates

            SPoint.X := Round(PVerts[J + 0] / PVerts[J + 2]) + FW2;
            SPoint.Y := Round(PVerts[J + 1] / PVerts[J + 2]) + FH2;
            SPolygon.Points[0].Z := Round(PVerts[J + 2] * 256);         // Make an 8-bit fixed point verion of Z
            Asm
              MOV   EAX,SPoint.X
              MOV   ECX,SPoint.Y
              CMP   EAX,MinPt.X
              JGE   @NotMinX
              MOV   MinPt.X,EAX
@NotMinX:
              CMP   ECX,MinPt.Y
              JGE   @NotMinY
              MOV   MinPt.Y,ECX
@NotMinY:
              CMP   EAX,MaxPt.X
              JLE   @NotMaxX
              MOV   MaxPt.X,EAX
@NotMaxX:
              CMP   ECX,MaxPt.Y
              JLE   @NotMaxY
              MOV   MaxPt.Y,ECX
@NotMaxY:
            End; // Asm
            SPolygon.Points[0].X := SPoint.X;
            SPolygon.Points[0].Y := SPoint.Y;

            // Finish projecting vertex 2 to screen coordinates

            SPoint.X := Round(PVerts[J + 4] / PVerts[J + 6]) + FW2;
            SPoint.Y := Round(PVerts[J + 5] / PVerts[J + 6]) + FH2;
            SPolygon.Points[1].Z := Round(PVerts[J + 6] * 256);         // Make an 8-bit fixed point verion of Z
            Asm
              MOV   EAX,SPoint.X
              MOV   ECX,SPoint.Y
              CMP   EAX,MinPt.X
              JGE   @NotMinX
              MOV   MinPt.X,EAX
@NotMinX:
              CMP   ECX,MinPt.Y
              JGE   @NotMinY
              MOV   MinPt.Y,ECX
@NotMinY:
              CMP   EAX,MaxPt.X
              JLE   @NotMaxX
              MOV   MaxPt.X,EAX
@NotMaxX:
              CMP   ECX,MaxPt.Y
              JLE   @NotMaxY
              MOV   MaxPt.Y,ECX
@NotMaxY:
            End; // Asm
            SPolygon.Points[1].X := SPoint.X;
            SPolygon.Points[1].Y := SPoint.Y;

            // Finish projecting vertex 3 to screen coordinates

            SPoint.X := Round(PVerts[J + 8] / PVerts[J + 10]) + FW2;
            SPoint.Y := Round(PVerts[J + 9] / PVerts[J + 10]) + FH2;
            SPolygon.Points[2].Z := Round(PVerts[J + 10] * 256);        // Make an 8-bit fixed point verion of Z
            Asm
              MOV   EAX,SPoint.X
              MOV   ECX,SPoint.Y
              CMP   EAX,MinPt.X
              JGE   @NotMinX
              MOV   MinPt.X,EAX
@NotMinX:
              CMP   ECX,MinPt.Y
              JGE   @NotMinY
              MOV   MinPt.Y,ECX
@NotMinY:
              CMP   EAX,MaxPt.X
              JLE   @NotMaxX
              MOV   MaxPt.X,EAX
@NotMaxX:
              CMP   ECX,MaxPt.Y
              JLE   @NotMaxY
              MOV   MaxPt.Y,ECX
@NotMaxY:
            End; // Asm
            SPolygon.Points[2].X := SPoint.X;
            SPolygon.Points[2].Y := SPoint.Y;

            // Make sure at least part of the polygon is visible

//            If (MaxPt.X Or MaxPt.Y Or (FW1 - MinPt.X) Or (FH1 - MinPt.Y)) >= 0 Then
            If (MaxPt.X >= 0) And (MaxPt.Y >= 0) And (MinPt.X <= FW1) And (MinPt.Y <= FH1) Then
            Begin
              // Clip the polygon to the screen...but only if we have to

//              If (MinPt.X Or MinPt.Y Or (FW1 - MaxPt.X) Or (FH1 - MaxPt.Y)) < 0 Then
              If (MinPt.X < 0) Or (MinPt.Y < 0) Or (MaxPt.X > FW1) Or (MaxPt.Y > FH1) Then
              Begin
                Frustum.ClipIntegerPolygonToScreen(SPolygon);
              End;

              // If the polygon is small or thin and distant, don't bother with it

//              If ((MaxPt.X - MinPt.X > SMALL_FACE) And (MaxPt.Y - MinPt.Y > SMALL_FACE)) Or (PVerts[J + 3] < DThreshold) Then
              Begin
                // Make sure there's something left to render

                If SPolygon.NumPoints > 2 Then FillSimpleScreenPolygon(SPolygon,MaxPt.X - MinPt.X > MaxPt.Y - MinPt.Y);
              End;
            End;
          End;
        End;
      End; // For K
    End;
  End;
End; // TOcclusionManager.SSERenderTriangleToZBuffer

Procedure TOcclusionManager.RenderTriangleToZBuffer(Positions,Normals: SPtr);
Var
  Polygon  : TSimplePolygon;
  SPolygon : TSimpleScreenPolygon;
  I        : Integer;
  SPoint   : TPoint;
  NX,NY,NZ : Single;
  MinZ     : Single;
  MaxZ     : Single;
  MinPt    : TSimpleScreenPoint;
  MaxPt    : TSimpleScreenPoint;
  FW1      : Integer;
  FH1      : Integer;

Begin
  If Not FIdentity Then Exit;
  
  // Each polygon's coordinates will be relative to its parent entity.  Convert them to absolute coordinates.

  Polygon.NumPoints   := 3;
  Polygon.Points[0].X := SPtr(LongWord(Positions) +  0)^;
  Polygon.Points[0].Y := SPtr(LongWord(Positions) +  4)^;
  Polygon.Points[0].Z := SPtr(LongWord(Positions) +  8)^;
  Polygon.Points[1].X := SPtr(LongWord(Positions) + 12)^;
  Polygon.Points[1].Y := SPtr(LongWord(Positions) + 16)^;
  Polygon.Points[1].Z := SPtr(LongWord(Positions) + 20)^;
  Polygon.Points[2].X := SPtr(LongWord(Positions) + 24)^;
  Polygon.Points[2].Y := SPtr(LongWord(Positions) + 28)^;
  Polygon.Points[2].Z := SPtr(LongWord(Positions) + 32)^;

  // The vast majority of entities we test will have no transformations (main zone geometry).
  // It's worthwhile to know if there is no transformation so we can skip it.

  If Not FIdentity Then
  Begin
    For I := 0 To 2 Do
    Begin
      FMatrix.Multiply(Polygon.Points[I].X,Polygon.Points[I].Y,Polygon.Points[I].Z,Polygon.Points[I].X,Polygon.Points[I].Y,Polygon.Points[I].Z);
    End; // For I
    NX := (Polygon.Points[0].Y - Polygon.Points[1].Y) * (Polygon.Points[2].Z - Polygon.Points[1].Z) - (Polygon.Points[0].Z - Polygon.Points[1].Z) * (Polygon.Points[2].Y - Polygon.Points[1].Y);
    NY := (Polygon.Points[0].Z - Polygon.Points[1].Z) * (Polygon.Points[2].X - Polygon.Points[1].X) - (Polygon.Points[0].X - Polygon.Points[1].X) * (Polygon.Points[2].Z - Polygon.Points[1].Z);
    NZ := (Polygon.Points[0].X - Polygon.Points[1].X) * (Polygon.Points[2].Y - Polygon.Points[1].Y) - (Polygon.Points[0].Y - Polygon.Points[1].Y) * (Polygon.Points[2].X - Polygon.Points[1].X);
  End
  Else
  Begin
    NX := SPtr(LongWord(Normals) + 0)^;
    NY := SPtr(LongWord(Normals) + 4)^;
    NZ := SPtr(LongWord(Normals) + 8)^;
  End;

  // Backface culling

  If (Polygon.Points[0].X - Observer[0]) * NX +
     (Polygon.Points[0].Y - Observer[1]) * NY +
     (Polygon.Points[0].Z - Observer[2]) * NZ < 0 Then
  Begin
    // Transform the coordinates so they are relative to the current view

    MinPt.X := FWidth;
    MinPt.Y := FHeight;
    MaxPt.X := -1;
    MaxPt.Y := -1;
    MinZ    := FOwner.DistFar + 1;
    MaxZ    := 0;
    For I := 0 To Polygon.NumPoints - 1 Do
    Begin
      Frustum.ViewMatrix.Multiply(Polygon.Points[I]);
      If Polygon.Points[I].Z < MinZ Then MinZ := Polygon.Points[I].Z;
      If Polygon.Points[I].Z > MaxZ Then MaxZ := Polygon.Points[I].Z;
    End; // For I

    // Make sure that at least something is visible

    If (MaxZ >= FOwner.DistNear) And (MinZ < FOwner.DistFar) Then
    Begin
      // Clip the polygon to the frustum's near plane...but only if we have to

      If MinZ < FOwner.DistNear Then
      Begin
        Frustum.ClipTransformedPolygonToNearDist(Polygon,Nil);
        MinZ := FOwner.DistNear;
      End;

      // Make sure there's something left to render

      If Polygon.NumPoints > 2 Then
      Begin
        // Project the coordinates to screen coordinates and get the screen extents

        SPolygon.NumPoints := Polygon.NumPoints;
        For I := 0 To Polygon.NumPoints - 1 Do
        Begin
          Frustum.ProjectTransformedPoint(Polygon.Points[I],SPoint);
          If SPoint.X < MinPt.X Then MinPt.X := SPoint.X;
          If SPoint.X > MaxPt.X Then MaxPt.X := SPoint.X;
          If SPoint.Y < MinPt.Y Then MinPt.Y := SPoint.Y;
          If SPoint.Y > MaxPt.Y Then MaxPt.Y := SPoint.Y;
          SPolygon.Points[I].X := SPoint.X;
          SPolygon.Points[I].Y := SPoint.Y;
          SPolygon.Points[I].Z := Round(Polygon.Points[I].Z * 256);
        End; // For I

        // Make sure the polygon is visible

        FW1 := FWidth  - 1;
        FH1 := FHeight - 1;
        If (MaxPt.X >= 0) And (MaxPt.Y >= 0) And (MinPt.X <= FW1) And (MinPt.Y <= FH1) Then
        Begin
          // Clip the polygon to the screen...but only if we have to

          If (MinPt.X < 0) Or (MinPt.Y < 0) Or (MaxPt.X > FW1) Or (MaxPt.Y > FH1) Then
          Begin
            Frustum.ClipIntegerPolygonToScreen(SPolygon);
            MinPt.X := Max(0,MinPt.X);
            MinPt.Y := Max(0,MinPt.Y);
            MaxPt.X := Min(FWidth  - 1,MaxPt.X);
            MaxPt.Y := Min(FHeight - 1,MaxPt.Y);
          End;

          // Make sure there's something left to render

          If SPolygon.NumPoints > 2 Then
          Begin
{
            PMinPt.X := MinPt.X;
            PMinPt.Y := MinPt.Y;
            PMaxPt.X := MaxPt.X;
            PMaxPt.Y := MaxPt.Y;

            // Draw the polygon if we have to

              If Not Is2DBoxOccluded(PMinPt,PMaxPt,Trunc(MinZ * 256)) Then} FillSimpleScreenPolygon(SPolygon,MaxPt.X - MinPt.X > MaxPt.Y - MinPt.Y);
          End;
        End;
      End;
    End;
  End;
End; // TOcclusionManager.RenderTriangleToZBuffer

Function TOcclusionManager.IsOccluded(Box: TAxisAlignedBox): Boolean;
Const PlanePoints : Array[0..5,0..3] Of Integer = ((0,1,7,2),(3,5,4,6),(1,6,4,7),(0,2,5,3),(0,3,6,1),(2,7,4,5));
Var
  I        : Integer;
  SPoint   : TPoint;
  MinDepth : Single;
  MaxDepth : Single;
  SMinPt   : TPoint;
  SMaxPt   : TPoint;
  Points   : Array[0..7] Of TSimplePoint;

Begin
  MinDepth := FOwner.DistFar + 1;
  MaxDepth := 0;

  Points[0].X := Box.MinPt.X;
  Points[0].Y := Box.MinPt.Y;
  Points[0].Z := Box.MinPt.Z;

  Points[4].X := Box.MaxPt.X;
  Points[4].Y := Box.MaxPt.Y;
  Points[4].Z := Box.MaxPt.Z;


  Points[1].X := Points[4].X;
  Points[1].Y := Points[0].Y;
  Points[1].Z := Points[0].Z;

  Points[2].X := Points[0].X;
  Points[2].Y := Points[4].Y;
  Points[2].Z := Points[0].Z;

  Points[3].X := Points[0].X;
  Points[3].Y := Points[0].Y;
  Points[3].Z := Points[4].Z;

  Points[5].X := Points[0].X;
  Points[5].Y := Points[4].Y;
  Points[5].Z := Points[4].Z;

  Points[6].X := Points[4].X;
  Points[6].Y := Points[0].Y;
  Points[6].Z := Points[4].Z;

  Points[7].X := Points[4].X;
  Points[7].Y := Points[4].Y;
  Points[7].Z := Points[0].Z;

  For I := 0 To 7 Do
  Begin
    Frustum.ViewMatrix.Multiply(Points[I]);
    If Points[I].Z < MinDepth Then MinDepth := Points[I].Z;
    If Points[I].Z > MaxDepth Then MaxDepth := Points[I].Z;
  End; // For I

  // Make sure at least part of the bounding box lies beyond the near clipping plane.
  // Otherwise we can treat it as occluded since it isn't visible anyway.

  If MaxDepth >= FOwner.DistNear Then
  Begin
    // If part of the bounding box lies closer than the near clipping plane then assume
    // that the object isn't occluded

    If MinDepth >= FOwner.DistNear Then
    Begin
      // Project the remamining points and save the maximum screen extents.  No clippling is
      // necessary because we've already made sure that the entire box lies beyond the near
      // clipping plane.  Our screen bounds-checking will take care of any part that lies
      // outside the view frustum.

      SMinPt.X := FWidth;
      SMinPt.Y := FHeight;
      SMaxPt.X := -1;
      SMaxPt.Y := -1;
      For I := 0 To 7 Do
      Begin
        Frustum.ProjectTransformedPoint(Points[I],SPoint);
        If SPoint.X < SMinPt.X Then SMinPt.X := SPoint.X;
        If SPoint.Y < SMinPt.Y Then SMinPt.Y := SPoint.Y;
        If SPoint.X > SMaxPt.X Then SMaxPt.X := SPoint.X;
        If SPoint.Y > SMaxPt.Y Then SMaxPt.Y := SPoint.Y;
      End; // For I

      // Make sure the box is visible

      If (SMaxPt.X >= 0) And (SMaxPt.Y >= 0) And (SMinPt.X < FWidth) And (SMinPt.Y < FHeight) Then
      Begin
        // Bounds-check the extents to the screen edges

        If SMinPt.X < 0        Then SMinPt.X := 0;
        If SMinPt.Y < 0        Then SMinPt.Y := 0;
        If SMaxPt.X >= FWidth  Then SMaxPt.X := FWidth  - 1;
        If SMaxPt.Y >= FHeight Then SMaxPt.Y := FHeight - 1;

        // Find out if the box we built is occluded

        Result := Is2DBoxOccluded(SMinPt,SMaxPt,Trunc(MinDepth * 256));
      End
      Else Result := True;
    End
    Else Result := False;
  End
  Else Result := True;
End; // TOcclusionManager.IsOccluded

Function TOcclusionManager.Is2DBoxOccluded(MinPt,MaxPt: TPoint; Depth: Integer): Boolean;
Var
  GIndex  : Integer;
  I,J     : Integer;
  I1,J1   : Integer;
  I2,J2   : Integer;
  GRows   : Integer;
  GCols   : Integer;
  PRow    : Integer;
  Found   : Boolean;
  GMinPt  : TPoint;
  GMaxPt  : TPoint;
  S       : Integer;
  Dirty   : Boolean;
  PZP     : IPtr;
  PZP1    : IPtr;
  FW,FH   : Integer;
  FGW,FGH : Integer;
  FGW1    : Integer;
  FW_2    : Integer;
  FW32_2  : Integer;
  FGP     : IPtr;
  FGP1    : IPtr;
  HasCMOV : Boolean;

Begin
  // First check the grid for any non-negative values that have a higher depth

  GMinPt.X := MinPt.X Shr 5;
  GMinPt.Y := MinPt.Y Shr 5;
  GMaxPt.X := MaxPt.X Shr 5;
  GMaxPt.Y := MaxPt.Y Shr 5;
  FGW      := FGridWidth;
  GIndex   := GMinPt.Y * FGW + GMinPt.X;
  GRows    := GMaxPt.Y - GMinPt.Y + 1;
  GCols    := GMaxPt.X - GMinPt.X + 1;
  Found    := False;
  Dirty    := False;
  PZP      := @(FGrid[GIndex]);
  Asm
    PUSH  EBX
    PUSH  ESI
    PUSH  EDI
    MOV   EBX,FGW
    MOV   ESI,PZP
    SUB   EBX,GCols
    MOV   CH,BYTE PTR GRows   // Limits screen resolution to 8192x8192
    SHL   EBX,2
    MOV   EDX,Depth
    MOV   AL,BYTE PTR GCols   // Limits screen resolution to 8192x8192
@RowLoop:
    MOV   CL,AL
@ColLoop:
    MOV   EDI,[ESI]
    TEST  EDI,EDI
    JNS   @Positive
    MOV   Dirty,1             // At least one grid element was marked as "dirty"
    JMP   @Negative
@RowLoop1:
    MOV   CL,AL
@ColLoop1:
    CMP   [ESI],EDX
    JGE   @Found
@Negative:
    ADD   ESI,4
    DEC   CL
    JNZ   @ColLoop1
    ADD   ESI,EBX
    DEC   CH
    JNZ   @RowLoop1
    JMP   @NotFound
@Positive:
    CMP   EDI,EDX
    JGE   @Found
    ADD   ESI,4
    DEC   CL
    JNZ   @ColLoop
    ADD   ESI,EBX
    DEC   CH
    JNZ   @RowLoop
    JMP   @NotFound
@Found:
    MOV   BYTE PTR Found,1
@NotFound:
    POP   EDI
    POP   ESI
    POP   EBX
  End; // Asm
  If Dirty And Not Found Then
  Begin
    // We didn't find any grid elements behind the square, but at least some were dirty.
    // Resolve the depths in the dirty ones and try again.

    FW     := FWidth;
    FH     := FHeight;
    FGH    := FGridHeight;
    FGW1   := FGridWidth - 1;
    FW_2   := FW Shl 2;
    FW32_2 := (FW - 32) Shl 2;
    J      := 0;
    J1     := GMinPt.Y;
    J2     := J1 Shl 5;
    PZP    := @(FZBuffer[(J2 * FW + (GMinPt.X Shl 5))]);
    FGP    := @(FGrid[GIndex]);
    HasCMOV := TFVector.isCMOV;
    While (J < GRows) And Not Found Do
    Begin
      I  := 0;
      I1 := GMinPt.X;
      I2 := I1 Shl 5;

      If J1 < FGH - 1
       Then PRow := 32
       Else PRow := FH - J2;

      PZP1 := PZP;
      FGP1 := FGP;
      While (I < GCols) And Not Found Do
      Begin
        S := FGP1^;
        If S < 0 Then
        Begin
          Asm
            PUSH  EBX
            PUSH  ESI
            PUSH  EDI

            MOV   EDI,PZP1
            MOV   EDX,S

            MOV   ESI,FGW1
            CMP   ESI,I1
            JAE   @FullGridSquare
            MOV   EAX,FW
            SUB   EAX,I2

            // We are at the end of a grid row, which might not be a full 32 pixels wide

            MOV   CH,BYTE PTR PRow      // Row counter
            MOV   EBX,FW                // Calculate row address offset
            SUB   EBX,EAX
            SHL   EBX,2
@RowLoop:
            MOV   CL,AL                 // Column counter
@ColLoop:
            MOV   ESI,[EDI]
            CMP   ESI,EDX
            JLE   @Closer
            MOV   EDX,ESI
@Closer:
            ADD   EDI,4
            DEC   CL
            JNZ   @ColLoop
            ADD   EDI,EBX
            DEC   CH
            JNZ   @RowLoop
            JMP   @Done

            // The grid element is a full 32 pixels wide

@FullGridSquare:
            MOV   ECX,PRow
            MOV   EDX,[EDI]             // We're starting with -1, so the first element has to be better
            MOV   EBX,FW_2
            TEST  BYTE PTR HasCMOV,0FFh
            JZ    @RowLoopFullNoSSE

@RowLoopFullSSE:
            // Row loop, taking advantage of P6 CMOVxx instructions

            MOV   EBX,EDX

            MOV   ESI,[EDI+4]
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+8]
            MOV   ESI,[EDI+12]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+16]
            MOV   ESI,[EDI+20]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+24]
            MOV   ESI,[EDI+28]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+32]
            MOV   ESI,[EDI+36]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+40]
            MOV   ESI,[EDI+44]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+48]
            MOV   ESI,[EDI+52]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+56]
            MOV   ESI,[EDI+60]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+64]
            MOV   ESI,[EDI+68]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+72]
            MOV   ESI,[EDI+76]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+80]
            MOV   ESI,[EDI+84]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+88]
            MOV   ESI,[EDI+92]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+96]
            MOV   ESI,[EDI+100]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+104]
            MOV   ESI,[EDI+108]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+112]
            MOV   ESI,[EDI+116]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            MOV   EAX,[EDI+120]
            MOV   ESI,[EDI+124]
            CMP   EDX,EAX
            CMOVB EDX,EAX
            CMP   EBX,ESI
            CMOVB EBX,ESI

            CMP   EDX,EBX
            CMOVB EDX,EBX

            ADD   EDI,FW_2
            SUB   ECX,1     // According to Intel, this is better than "DEC ECX" because it removes all flag dependencies
            JNZ   @RowLoopFullSSE
            JMP   @Done



@RowLoopFullNoSSE:
            MOV   ESI,[EDI]
            CMP   ESI,EDX
            JLE   @CloserFull_1
            MOV   EDX,ESI
@CloserFull_1:
            MOV   ESI,[EDI+4]
            CMP   ESI,EDX
            JLE   @CloserFull_2
            MOV   EDX,ESI
@CloserFull_2:
            MOV   ESI,[EDI+8]
            CMP   ESI,EDX
            JLE   @CloserFull_3
            MOV   EDX,ESI
@CloserFull_3:
            MOV   ESI,[EDI+12]
            CMP   ESI,EDX
            JLE   @CloserFull_4
            MOV   EDX,ESI
@CloserFull_4:
            MOV   ESI,[EDI+16]
            CMP   ESI,EDX
            JLE   @CloserFull_5
            MOV   EDX,ESI
@CloserFull_5:
            MOV   ESI,[EDI+20]
            CMP   ESI,EDX
            JLE   @CloserFull_6
            MOV   EDX,ESI
@CloserFull_6:
            MOV   ESI,[EDI+24]
            CMP   ESI,EDX
            JLE   @CloserFull_7
            MOV   EDX,ESI
@CloserFull_7:
            MOV   ESI,[EDI+28]
            CMP   ESI,EDX
            JLE   @CloserFull_8
            MOV   EDX,ESI
@CloserFull_8:
            MOV   ESI,[EDI+32]
            CMP   ESI,EDX
            JLE   @CloserFull_9
            MOV   EDX,ESI
@CloserFull_9:
            MOV   ESI,[EDI+36]
            CMP   ESI,EDX
            JLE   @CloserFull_10
            MOV   EDX,ESI
@CloserFull_10:
            MOV   ESI,[EDI+40]
            CMP   ESI,EDX
            JLE   @CloserFull_11
            MOV   EDX,ESI
@CloserFull_11:
            MOV   ESI,[EDI+44]
            CMP   ESI,EDX
            JLE   @CloserFull_12
            MOV   EDX,ESI
@CloserFull_12:
            MOV   ESI,[EDI+48]
            CMP   ESI,EDX
            JLE   @CloserFull_13
            MOV   EDX,ESI
@CloserFull_13:
            MOV   ESI,[EDI+52]
            CMP   ESI,EDX
            JLE   @CloserFull_14
            MOV   EDX,ESI
@CloserFull_14:
            MOV   ESI,[EDI+56]
            CMP   ESI,EDX
            JLE   @CloserFull_15
            MOV   EDX,ESI
@CloserFull_15:
            MOV   ESI,[EDI+60]
            CMP   ESI,EDX
            JLE   @CloserFull_16
            MOV   EDX,ESI
@CloserFull_16:
            MOV   ESI,[EDI+64]
            CMP   ESI,EDX
            JLE   @CloserFull_17
            MOV   EDX,ESI
@CloserFull_17:
            MOV   ESI,[EDI+68]
            CMP   ESI,EDX
            JLE   @CloserFull_18
            MOV   EDX,ESI
@CloserFull_18:
            MOV   ESI,[EDI+72]
            CMP   ESI,EDX
            JLE   @CloserFull_19
            MOV   EDX,ESI
@CloserFull_19:
            MOV   ESI,[EDI+76]
            CMP   ESI,EDX
            JLE   @CloserFull_20
            MOV   EDX,ESI
@CloserFull_20:
            MOV   ESI,[EDI+80]
            CMP   ESI,EDX
            JLE   @CloserFull_21
            MOV   EDX,ESI
@CloserFull_21:
            MOV   ESI,[EDI+84]
            CMP   ESI,EDX
            JLE   @CloserFull_22
            MOV   EDX,ESI
@CloserFull_22:
            MOV   ESI,[EDI+88]
            CMP   ESI,EDX
            JLE   @CloserFull_23
            MOV   EDX,ESI
@CloserFull_23:
            MOV   ESI,[EDI+92]
            CMP   ESI,EDX
            JLE   @CloserFull_24
            MOV   EDX,ESI
@CloserFull_24:
            MOV   ESI,[EDI+96]
            CMP   ESI,EDX
            JLE   @CloserFull_25
            MOV   EDX,ESI
@CloserFull_25:
            MOV   ESI,[EDI+100]
            CMP   ESI,EDX
            JLE   @CloserFull_26
            MOV   EDX,ESI
@CloserFull_26:
            MOV   ESI,[EDI+104]
            CMP   ESI,EDX
            JLE   @CloserFull_27
            MOV   EDX,ESI
@CloserFull_27:
            MOV   ESI,[EDI+108]
            CMP   ESI,EDX
            JLE   @CloserFull_28
            MOV   EDX,ESI
@CloserFull_28:
            MOV   ESI,[EDI+112]
            CMP   ESI,EDX
            JLE   @CloserFull_29
            MOV   EDX,ESI
@CloserFull_29:
            MOV   ESI,[EDI+116]
            CMP   ESI,EDX
            JLE   @CloserFull_30
            MOV   EDX,ESI
@CloserFull_30:
            MOV   ESI,[EDI+120]
            CMP   ESI,EDX
            JLE   @CloserFull_31
            MOV   EDX,ESI
@CloserFull_31:
            MOV   ESI,[EDI+124]
            CMP   ESI,EDX
            JLE   @CloserFull_32
            MOV   EDX,ESI
@CloserFull_32:



            ADD   EDI,EBX
            SUB   ECX,1     // According to Intel, this is better than "DEC ECX" because it removes all flag dependencies
            JNZ   @RowLoopFullNoSSE
@Done:
            MOV   S,EDX
            POP   EDI
            POP   ESI
            POP   EBX
          End; // Asm
          FGP1^ := S;
          If S > Depth Then Found := True;
        End;
        Inc(I);
        Inc(I1);
        Inc(I2,32);
        Inc(LongWord(PZP1),32 * 4);
        Inc(LongWord(FGP1),4);
      End; // While
      Inc(J);
      Inc(J1);
      Inc(J2,32);
      Inc(LongWord(PZP),FW * 32 * 4);
      Inc(LongWord(FGP),FGW * 4);
    End; // While
  End;
  Result := Not Found;
End; // TOcclusionManager.Is2DBoxOccluded

Procedure TOcclusionManager.FillSimpleScreenPolygonVert(Var Polygon: TSimpleScreenPolygon);
Type
  TEdgeData = Record
    Error   : Integer;
    Step    : Integer;
    Move    : Integer;
    MoveDir : Integer;
    Pos     : Integer;
  End;
  TSideData = Record
    SEdge  : TEdgeData;
    ZEdge  : TEdgeData;
    Width  : Integer;
    Height : Integer;
    Depth  : Integer;
  End;

  TPolyData = Record
    T     : TSideData;
    B     : TSideData;
    GXLR0 : Integer;
  End;

Var
  Lookup: Array[-1..21] Of Integer;

  I             : Integer;
  MinIndexT     : Integer;
  MinIndexB     : Integer;
  MinPoint_X    : Integer;
  MaxPoint_X    : Integer;
  ScanLines     : Integer;

  CurIndexT     : Integer;
  PrevIndexT    : Integer;
  CurIndexB     : Integer;
  PrevIndexB    : Integer;

  XIndex        : Integer;
  ZP            : Pointer;
  GP            : Pointer;
  X             : Integer;
  GIndex        : Integer;
  GW            : Integer;
  GW4           : Integer;

  PolyData      : TPolyData;
  FW            : Integer;
  FW4           : Integer;
  FH            : Integer;
  FH1           : Integer;
  CanScanGrid   : Integer;
  MaxGridVal    : Integer;

  PPrevT        : PSimpleScreenPoint;
  PPrevB        : PSimpleScreenPoint;
  PCurrT        : PSimpleScreenPoint;
  PCurrB        : PSimpleScreenPoint;

Begin
  Lookup[-1] := Polygon.NumPoints - 1;
  Lookup[Polygon.NumPoints] := 0;
  For I := 0 To Polygon.NumPoints - 1 Do Lookup[I] := I;

  // Scan the list to find the top and bottom of the polygon

  MinIndexT  := 0;
  MaxPoint_X := Polygon.Points[0].X;
  MinPoint_X := MaxPoint_X;

  For I := 1 To Polygon.NumPoints - 1 Do
  Begin
    If Polygon.Points[I].X < MinPoint_X Then
    Begin
      MinIndexT  := I;
      MinPoint_X := Polygon.Points[I].X; // New left
    End
    Else If Polygon.Points[I].X > MaxPoint_X Then
    Begin
      MaxPoint_X := Polygon.Points[I].X; // New right
    End;
  End; // For I

  // If the polygon is 0-width, exit to avoid an infinite loop

  If MinPoint_X <> MaxPoint_X Then
  Begin
    // Scan in ascending order to find the first left-edge point

    MinIndexB := MinIndexT;
    While Polygon.Points[MinIndexB].X = MinPoint_X Do MinIndexB := Lookup[MinIndexB + 1];  // Move counterclockwise
    MinIndexB := Lookup[MinIndexB - 1]; // Back up to first left-edge point

    // Now scan in descending order to find the last left-edge point

    While Polygon.Points[MinIndexT].X = MinPoint_X Do MinIndexT := Lookup[MinIndexT - 1];  // Move clockwise
    MinIndexT := Lookup[MinIndexT + 1]; // Back up to last left-edge point

    FillChar(PolyData,SizeOf(PolyData),0);

    ScanLines  := MaxPoint_X - MinPoint_X + 1;
    PrevIndexT := MinIndexT;
    PrevIndexB := MinIndexB;
    CurIndexT  := Lookup[MinIndexT - 1];
    CurIndexB  := Lookup[MinIndexB + 1];
    X          := MinPoint_X;
    XIndex     := X;
    GIndex     := X Shr 5;
    GW         := FGridWidth;
    GW4        := FGridWidth * 4;
    ZP         := @(FZBuffer[0]);
    GP         := @(FGrid[0]);
    FW         := FWidth;
    FW4        := FWidth * 4;
    FH         := FHeight;
    FH1        := FHeight - 1;
    PolyData.GXLR0 := -1;
    CanScanGrid    := 1;
    MaxGridVal     := -1;

    PPrevT     := @(Polygon.Points[PrevIndexT]);
    PPrevB     := @(Polygon.Points[PrevIndexB]);
    PCurrT     := @(Polygon.Points[CurIndexT]);
    PCurrB     := @(Polygon.Points[CurIndexB]);

    While ScanLines > 0 Do
    Begin
      // Get Y coordinate of top edge

      If PolyData.T.Width <= 0 Then
      Begin
        PolyData.T.SEdge.Error := 0;
        PolyData.T.ZEdge.Error := 0;
        PolyData.T.SEdge.Pos   := PPrevT.Y;//Polygon.Points[PrevIndexL].X;
        PolyData.T.ZEdge.Pos   := PPrevT.Z;//Polygon.Points[PrevIndexL].Z;
        PolyData.T.Height      := PCurrT.Y - PolyData.T.SEdge.Pos;//Polygon.Points[CurIndexL].X - PolyData.XL;
        PolyData.T.Depth       := PCurrT.Z - PolyData.T.ZEdge.Pos;//Polygon.Points[CurIndexL].Z - PolyData.ZL;
        PolyData.T.Width       := PCurrT.X - PPrevT.X;//Polygon.Points[CurIndexL].Y - Polygon.Points[PrevIndexL].Y;
        If PolyData.T.Width <> 0 Then
        Begin
          // Calculate the move and step values

          Asm
            PUSH  EBX
            PUSH  ESI

            MOV   ECX,PolyData.T.Width
            MOV   EAX,PolyData.T.Height
            SUB   ESI,ESI

            TEST  EAX,EAX
            JZ    @Zero
            JNS   @Positive

            SUB   EDX,EDX
            NEG   EAX
            DEC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Height) / Abs(Width)
            MOV   WORD PTR PolyData.T.SEdge.Step[2],AX
            SHR   EAX,16
            NEG   EAX
            JMP   @NotZero
  @Positive:
            SUB   EDX,EDX
            INC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Height) / Abs(Width)
            MOV   WORD PTR PolyData.T.SEdge.Step[2],AX
            SHR   EAX,16
            JMP   @NotZero
  @Zero:
            MOV   WORD PTR PolyData.T.SEdge.Step[2],AX
  @NotZero:
            MOV   PolyData.T.SEdge.Move,EAX
            MOV   PolyData.T.SEdge.MoveDir,ESI

            // ------------------------------------------

            MOV   EAX,PolyData.T.Depth
            SUB   ESI,ESI

            TEST  EAX,EAX
            JZ    @ZeroZ
            JNS   @PositiveZ

            SUB   EDX,EDX
            NEG   EAX
            DEC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Height) / Abs(Width)
            MOV   WORD PTR PolyData.T.ZEdge.Step[2],AX
            SHR   EAX,16
            NEG   EAX
            JMP   @NotZeroZ
  @PositiveZ:
            SUB   EDX,EDX
            INC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Height) / Abs(Width)
            MOV   WORD PTR PolyData.T.ZEdge.Step[2],AX
            SHR   EAX,16
            JMP   @NotZeroZ
  @ZeroZ:
            MOV   WORD PTR PolyData.T.ZEdge.Step[2],AX
  @NotZeroZ:
            MOV   PolyData.T.ZEdge.Move,EAX
            MOV   PolyData.T.ZEdge.MoveDir,ESI

            POP   ESI
            POP   EBX
          End; // Asm
        End
        Else
        Begin
          PolyData.T.SEdge.Move    := 0;
          PolyData.T.SEdge.Step    := 0;
          PolyData.T.SEdge.MoveDir := 0;
          PolyData.T.ZEdge.Move    := 0;
          PolyData.T.ZEdge.Step    := 0;
          PolyData.T.ZEdge.MoveDir := 0;
        End;
      End
      Else
      Begin
        // Move to the next pixel using the move value and any error rollover

        Asm
          MOV   ECX,PolyData.T.SEdge.Pos
          MOV   EAX,PolyData.T.SEdge.Step
          MOV   EDX,PolyData.T.SEdge.Move
          ADD   PolyData.T.SEdge.Error,EAX
          JNC   @NoStep
          ADD   EDX,PolyData.T.SEdge.MoveDir
@NoStep:
          ADD   ECX,EDX
          JNS   @Positive
          SUB   ECX,ECX
          JMP   @Valid
@Positive:
          CMP   ECX,FH
          JB    @Valid
          MOV   ECX,FH1
@Valid:
          MOV   PolyData.T.SEdge.Pos,ECX

          // -------------------------------

          MOV   EAX,PolyData.T.ZEdge.Step
          MOV   EDX,PolyData.T.ZEdge.Move
          ADD   PolyData.T.ZEdge.Error,EAX
          JNC   @NoStepZ
          ADD   EDX,PolyData.T.ZEdge.MoveDir
@NoStepZ:
          ADD   PolyData.T.ZEdge.Pos,EDX
        End; // Asm
      End;

      // Get Y coordinate of bottom edge

      If PolyData.B.Width <= 0 Then
      Begin
        PolyData.B.SEdge.Error := 0;
        PolyData.B.ZEdge.Error := 0;
        PolyData.B.SEdge.Pos   := PPrevB.Y;//Polygon.Points[PrevIndexR].X;
        PolyData.B.ZEdge.Pos   := PPrevB.Z;//Polygon.Points[PrevIndexR].Z;
        PolyData.B.Height      := PCurrB.Y - PolyData.B.SEdge.Pos;//Polygon.Points[CurIndexR].X - PolyData.XR;
        PolyData.B.Depth       := PCurrB.Z - PolyData.B.ZEdge.Pos;//Polygon.Points[CurIndexR].Z - PolyData.ZR;
        PolyData.B.Width       := PCurrB.X - PPrevB.X;//Polygon.Points[CurIndexR].Y - Polygon.Points[PrevIndexR].Y;
        If PolyData.B.Width <> 0 Then
        Begin
          // Calculate the move and step values

          Asm
            PUSH  EBX
            PUSH  ESI

            MOV   ECX,PolyData.B.Width
            MOV   EAX,PolyData.B.Height
            SUB   ESI,ESI

            TEST  EAX,EAX
            JZ    @Zero
            JNS   @Positive

            SUB   EDX,EDX
            NEG   EAX
            DEC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Height) / Abs(Width)
            MOV   WORD PTR PolyData.B.SEdge.Step[2],AX
            SHR   EAX,16
            NEG   EAX
            JMP   @NotZero
  @Positive:
            SUB   EDX,EDX
            INC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Height) / Abs(Width)
            MOV   WORD PTR PolyData.B.SEdge.Step[2],AX
            SHR   EAX,16
            JMP   @NotZero
  @Zero:
            MOV   WORD PTR PolyData.B.SEdge.Step[2],AX
  @NotZero:
            MOV   PolyData.B.SEdge.Move,EAX
            MOV   PolyData.B.SEdge.MoveDir,ESI

            // ------------------------------------------

            MOV   EAX,PolyData.B.Depth
            SUB   ESI,ESI

            TEST  EAX,EAX
            JZ    @ZeroZ
            JNS   @PositiveZ

            SUB   EDX,EDX
            NEG   EAX
            DEC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Height) / Abs(Width)
            MOV   WORD PTR PolyData.B.ZEdge.Step[2],AX
            SHR   EAX,16
            NEG   EAX
            JMP   @NotZeroZ
  @PositiveZ:
            SUB   EDX,EDX
            INC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Height) / Abs(Width)
            MOV   WORD PTR PolyData.B.ZEdge.Step[2],AX
            SHR   EAX,16
            JMP   @NotZeroZ
  @ZeroZ:
            MOV   WORD PTR PolyData.B.ZEdge.Step[2],AX
  @NotZeroZ:
            MOV   PolyData.B.ZEdge.Move,EAX
            MOV   PolyData.B.ZEdge.MoveDir,ESI

            POP   ESI
            POP   EBX
          End; // Asm
        End
        Else
        Begin
          PolyData.B.SEdge.Move    := 0;
          PolyData.B.SEdge.Step    := 0;
          PolyData.B.SEdge.MoveDir := 0;
          PolyData.B.ZEdge.Move    := 0;
          PolyData.B.ZEdge.Step    := 0;
          PolyData.B.ZEdge.MoveDir := 0;
        End;
      End
      Else
      Begin
        // Move to the next pixel using the move value and any error rollover

        Asm
          MOV   ECX,PolyData.B.SEdge.Pos
          MOV   EAX,PolyData.B.SEdge.Step
          MOV   EDX,PolyData.B.SEdge.Move
          ADD   PolyData.B.SEdge.Error,EAX
          JNC   @NoStep
          ADD   EDX,PolyData.B.SEdge.MoveDir
@NoStep:
          ADD   ECX,EDX
          JNS   @Positive
          SUB   ECX,ECX
          JMP   @Valid
@Positive:
          CMP   ECX,FH
          JB    @Valid
          MOV   ECX,FH1
@Valid:
          MOV   PolyData.B.SEdge.Pos,ECX

          // -------------------------------

          MOV   EAX,PolyData.B.ZEdge.Step
          MOV   EDX,PolyData.B.ZEdge.Move
          ADD   PolyData.B.ZEdge.Error,EAX
          JNC   @NoStepZ
          ADD   EDX,PolyData.B.ZEdge.MoveDir
@NoStepZ:
          ADD   PolyData.B.ZEdge.Pos,EDX
        End; // Asm
      End;

      // Draw the scanline.  We need to do a sanity check on the top and bottom positions because roundoff error
      // can make them flip.

      If PolyData.B.SEdge.Pos >= PolyData.T.SEdge.Pos Then
      Begin
        Asm
          // Figure out the move and step values for the depth gradient for this scan line

          PUSH  EDI
          PUSH  EBX
          PUSH  ESI

          // Get the scan line length and establish the start and end values

          MOV   EBX,PolyData.B.SEdge.Pos
          MOV   EDI,PolyData.T.SEdge.Pos
          MOV   ECX,EBX
          SUB   ECX,EDI
          INC   ECX             // ECX will contain the scan line length

          // Scan the grid buffer to determine the maximum (deepest) value.  If the scan line we want
          // to draw is deeper than this, then we can skip it.

          SHR   EDI,5           // Top grid index
          SHR   EBX,5           // Bottom grid index

          MOV   EDX,EDI
          SHL   EDX,16
          OR    EDX,EBX
          XOR   EDX,PolyData.GXLR0    // Get the last L+R grid indices we checked (-1 means we haven't check anything)
          JNZ   @GridChanged    // If either the grid row or end(s) have changed, rescan the grid row
          TEST  CanScanGrid,1   // Don't scan if we either found a dirty grid value or marked one as dirty
          JZ    @WriteScanLine
          MOV   EAX,MaxGridVal  // Same grid row and extents (and we haven't written anything yet), so the maximum value is the same
          JMP   @DoneScanGrid
@GridChanged:
          MOV   CanScanGrid,1   // The grid ends have changed (or the grid row has), so rescan it
@ScanGrid:
          SUB   EBX,EDI

          MOV   EAX,GW          // Multiply by the grid width
          MUL   EDI
          MOV   EDI,EAX

          ADD   EDI,GIndex      // Add the offset to the grid row
          INC   EBX             // Number of grid elements to check
          SHL   EDI,2           // Change to a pointer to DWORD'S
          ADD   EDI,GP          // GP points to the grid z-buffer
          SUB   EAX,EAX         // 0 ........ EAX will eventually contain the maximum grid value we find
@GridScanLoop:
          MOV   ESI,[EDI]       // Read the grid value
          TEST  ESI,ESI         // If we find an element marked "dirty", don't bother scanning anymore
          JS    @FoundDirty
          CMP   ESI,EAX         // Compare with our max depth value
          JLE   @NotGreater
          MOV   EAX,ESI         // Store the maxiumum (farthest) grid value
@NotGreater:
          ADD   EDI,GW4         // Move to the next grid element
          DEC   EBX
          JNZ   @GridScanLoop
          MOV   MaxGridVal,EAX  // Save the maximum grid value so we can just reuse it instead of rescanning
@DoneScanGrid:
          // If both ends of the scanline are farther away than the grid depth then skip it entirely.
          // Conversely, if either end is closer than the grid depth then we want to draw the scanline.

          CMP   PolyData.T.ZEdge.Pos,EAX
          JL    @FoundDirty     // If the left edge is closer then draw the scanline
          CMP   PolyData.B.ZEdge.Pos,EAX
          JGE   @Done           // If the right edge is farther (and to get here the left edge is farther as well) then skip the scanline

          // If we're going to write the scanline (whether because of a dirty grid or because our scanline isn't deeper), we don't want to scan this grid row anymore
@FoundDirty:
          MOV   CanScanGrid,0
@WriteScanLine:

          // Calculate the starting Z-buffer index

          MOV   EAX,FW
          MUL   DWORD PTR PolyData.T.SEdge.Pos
          MOV   EDI,EAX
          ADD   EDI,XIndex
          SHL   EDI,2
          ADD   EDI,ZP                 // ZP points to the Z buffer
          MOV   ESI,PolyData.T.ZEdge.Pos // Get the current depth value

          // Figure out the move and step values for the depth gradient for this scan line

          SUB   EDX,EDX
          MOV   EAX,PolyData.B.ZEdge.Pos
          SUB   EAX,PolyData.T.ZEdge.Pos
          JZ    @ZeroMoveDirZ
          JNS   @Positive

          NEG   EAX             // Make it positive: we'll subtract rather than add so we can handle carry in one instruction
          SHL   EAX,16          // Convert to fixed-point
          DIV   ECX             // Abs(Width) / Abs(Height)
          SHRD  EDX,EAX,16      // StepZ
          SHR   EAX,16
          JMP   @NegMoveDirZ
@Positive:


          SHL   EAX,16          // Convert to fixed-point
          DIV   ECX             // Abs(Width) / Abs(Height)
          SHRD  EDX,EAX,16      // StepZ
          SHR   EAX,16          // EAX will now contain the MoveZ value

          // Write the scanline to the Z buffer

          // MoveDirZ is 1

          SUB   EBX,EBX                // Start with depth error term = 0
@PixelLoopPos:
          CMP   ESI,[EDI]              // Only write the pixel if it's closer than what's already there
          JAE   @AlreadyCloserPos
          MOV   [EDI],ESI
@AlreadyCloserPos:
          ADD   EDI,FW4
          ADD   EBX,EDX         // Add StepZ to ErrorZ
          ADC   ESI,EAX         // Add MoveZ to the current depth value, adding carry also if necessary

          DEC   ECX
          JNZ   @PixelLoopPos
          JMP   @MarkGridEntries

          // MoveDirZ is -1

@NegMoveDirZ:

          SUB   EBX,EBX                // Start with depth error term = 0
@PixelLoopNeg:
          CMP   ESI,[EDI]              // Only write the pixel if it's closer than what's already there
          JAE   @AlreadyCloserNeg
          MOV   [EDI],ESI
@AlreadyCloserNeg:
          ADD   EDI,FW4
          ADD   EBX,EDX         // Add StepZ to ErrorZ
          SBB   ESI,EAX         // Subtract MoveZ from the current depth value, subtracting carry also if necessary
          DEC   ECX
          JNZ   @PixelLoopNeg
          JMP   @MarkGridEntries

          // MoveDirZ is 0, which means that so are MoveZ and StepZ

@ZeroMoveDirZ:

@PixelLoopZero:
          CMP   ESI,[EDI]
          JAE   @AlreadyCloserZero
          MOV   [EDI],ESI
@AlreadyCloserZero:
          ADD   EDI,FW4
          DEC   ECX
          JNZ   @PixelLoopZero

@MarkGridEntries:
          // Mark the grid entries as dirty

          MOV   EDI,PolyData.T.SEdge.Pos
          MOV   ECX,PolyData.B.SEdge.Pos
          SHR   EDI,5
          SHR   ECX,5

          MOV   EDX,EDI
          SHL   EDX,16
          OR    EDX,ECX
          MOV   EAX,EDX
          XOR   EDX,PolyData.GXLR0
          JZ    @NoGridChange
          MOV   PolyData.GXLR0,EAX        // Save our combined L+R grid indices
          SUB   ECX,EDI

          MOV   EAX,GW          // Multiply by the grid width
          MUL   EDI
          MOV   EDI,EAX

          ADD   EDI,GIndex
          INC   ECX
          SHL   EDI,2
          MOV   EAX,0FFFFFFFFh  // -1
          ADD   EDI,GP
          CLD
@MarkGridLoop:
          MOV   [EDI],EAX
          ADD   EDI,GW4
          DEC   ECX
          JNZ   @MarkGridLoop
@NoGridChange:

@Done:
          POP   ESI
          POP   EBX
          POP   EDI
        End; // Asm
      End;

      // Move to the next scanline

      Inc(X);
      Inc(XIndex);
      Dec(ScanLines);
      Dec(PolyData.T.Width);
      Dec(PolyData.B.Width);
      If (X And $1F) = 0 Then
      Begin
        // Move to the next grid row

        Inc(GIndex);
        PolyData.GXLR0 := -1;
      End;
      If PolyData.T.Width <= 0 Then
      Begin
        PrevIndexT := CurIndexT;
        PPrevT     := PCurrT;
        CurIndexT  := Lookup[CurIndexT - 1];
        PCurrT     := @(Polygon.Points[CurIndexT]);
      End;
      If PolyData.B.Width <= 0 Then
      Begin
        PrevIndexB := CurIndexB;
        PPrevB     := PCurrB;
        CurIndexB  := Lookup[CurIndexB + 1];
        PCurrB     := @(Polygon.Points[CurIndexB]);
      End;
    End; // While
  End;
End; // TOcclusionManager.FillSimpleScreenPolygonVert

Procedure TOcclusionManager.FillSimpleScreenPolygonHorz(Var Polygon: TSimpleScreenPolygon);
Type
  TEdgeData = Record
    Error   : Integer;
    Step    : Integer;
    Move    : Integer;
    MoveDir : Integer;
    Pos     : Integer;
  End;
  TSideData = Record
    SEdge  : TEdgeData;
    ZEdge  : TEdgeData;
    Width  : Integer;
    Height : Integer;
    Depth  : Integer;
  End;

  TPolyData = Record
    L     : TSideData;
    R     : TSideData;
    GXLR0 : Integer;
  End;

Var
  Lookup: Array[-1..21] Of Integer;

  I             : Integer;
  MinIndexL     : Integer;
  MinIndexR     : Integer;
  MinPoint_Y    : Integer;
  MaxPoint_Y    : Integer;
  ScanLines     : Integer;

  CurIndexL     : Integer;
  PrevIndexL    : Integer;
  CurIndexR     : Integer;
  PrevIndexR    : Integer;

  YIndex        : Integer;
  ZP            : Pointer;
  GP            : Pointer;
  Y             : Integer;
  GIndex        : Integer;

  PolyData      : TPolyData;
  FW            : Integer;
  FW1           : Integer;
  CanScanGrid   : Integer;
  MaxGridVal    : Integer;

  PPrevL        : PSimpleScreenPoint;
  PPrevR        : PSimpleScreenPoint;
  PCurrL        : PSimpleScreenPoint;
  PCurrR        : PSimpleScreenPoint;

Begin
  Lookup[-1] := Polygon.NumPoints - 1;
  Lookup[Polygon.NumPoints] := 0;
  For I := 0 To Polygon.NumPoints - 1 Do Lookup[I] := I;

  // Scan the list to find the top and bottom of the polygon

  MinIndexL  := 0;
  MaxPoint_Y := Polygon.Points[0].Y;
  MinPoint_Y := MaxPoint_Y;

  For I := 1 To Polygon.NumPoints - 1 Do
  Begin
    If Polygon.Points[I].Y < MinPoint_Y Then
    Begin
      MinIndexL  := I;
      MinPoint_Y := Polygon.Points[I].Y; // New top
    End
    Else If Polygon.Points[I].Y > MaxPoint_Y Then
    Begin
      MaxPoint_Y := Polygon.Points[I].Y; // New bottom
    End;
  End; // For I

  // If the polygon is 0-height, exit to avoid an infinite loop

  If MinPoint_Y <> MaxPoint_Y Then
  Begin
    // Scan in ascending order to find the last top-edge point

    MinIndexR := MinIndexL;
    While Polygon.Points[MinIndexR].Y = MinPoint_Y Do MinIndexR := Lookup[MinIndexR - 1];  // Move clockwise
    MinIndexR := Lookup[MinIndexR + 1]; // Back up to last top-edge point

    // Now scan in descending order to find the first top-edge point

    While Polygon.Points[MinIndexL].Y = MinPoint_Y Do MinIndexL := Lookup[MinIndexL + 1];  // Move counterclockwise
    MinIndexL := Lookup[MinIndexL - 1]; // Back up to first top-edge point

    FillChar(PolyData,SizeOf(PolyData),0);

    ScanLines  := MaxPoint_Y - MinPoint_Y + 1;
    PrevIndexL := MinIndexL;
    PrevIndexR := MinIndexR;
    CurIndexL  := Lookup[MinIndexL + 1];
    CurIndexR  := Lookup[MinIndexR - 1];
    Y          := MinPoint_Y;
    YIndex     := Y * FWidth;
    GIndex     := (Y Shr 5) * FGridWidth;
    ZP         := @(FZBuffer[0]);
    GP         := @(FGrid[0]);
    FW         := FWidth;
    FW1        := FW - 1;
    PolyData.GXLR0 := -1;
    CanScanGrid    := 1;
    MaxGridVal     := -1;

    PPrevL     := @(Polygon.Points[PrevIndexL]);
    PPrevR     := @(Polygon.Points[PrevIndexR]);
    PCurrL     := @(Polygon.Points[CurIndexL]);
    PCurrR     := @(Polygon.Points[CurIndexR]);

    While ScanLines > 0 Do
    Begin
      // Get X coordinate of left edge

      If PolyData.L.Height <= 0 Then
      Begin
        PolyData.L.SEdge.Error := 0;
        PolyData.L.ZEdge.Error := 0;
        PolyData.L.SEdge.Pos   := PPrevL.X;//Polygon.Points[PrevIndexL].X;
        PolyData.L.ZEdge.Pos   := PPrevL.Z;//Polygon.Points[PrevIndexL].Z;
        PolyData.L.Width       := PCurrL.X - PolyData.L.SEdge.Pos;//Polygon.Points[CurIndexL].X - PolyData.XL;
        PolyData.L.Depth       := PCurrL.Z - PolyData.L.ZEdge.Pos;//Polygon.Points[CurIndexL].Z - PolyData.ZL;
        PolyData.L.Height      := PCurrL.Y - PPrevL.Y;//Polygon.Points[CurIndexL].Y - Polygon.Points[PrevIndexL].Y;
        If PolyData.L.Height <> 0 Then
        Begin
          // Calculate the move and step values

          Asm
            PUSH  EBX
            PUSH  ESI

            MOV   ECX,PolyData.L.Height
            MOV   EAX,PolyData.L.Width
            SUB   ESI,ESI

            TEST  EAX,EAX
            JZ    @Zero
            JNS   @Positive

            SUB   EDX,EDX
            NEG   EAX
            DEC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Width) / Abs(Height)
            MOV   WORD PTR PolyData.L.SEdge.Step[2],AX
            SHR   EAX,16
            NEG   EAX
            JMP   @NotZero
  @Positive:
            SUB   EDX,EDX  
            INC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Width) / Abs(Height)
            MOV   WORD PTR PolyData.L.SEdge.Step[2],AX
            SHR   EAX,16
            JMP   @NotZero
  @Zero:
            MOV   WORD PTR PolyData.L.SEdge.Step[2],AX
  @NotZero:
            MOV   PolyData.L.SEdge.Move,EAX
            MOV   PolyData.L.SEdge.MoveDir,ESI

            // ------------------------------------------

            MOV   EAX,PolyData.L.Depth
            SUB   ESI,ESI

            TEST  EAX,EAX
            JZ    @ZeroZ
            JNS   @PositiveZ

            SUB   EDX,EDX
            NEG   EAX
            DEC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Width) / Abs(Height)
            MOV   WORD PTR PolyData.L.ZEdge.Step[2],AX
            SHR   EAX,16
            NEG   EAX
            JMP   @NotZeroZ
  @PositiveZ:
            SUB   EDX,EDX
            INC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Width) / Abs(Height)
            MOV   WORD PTR PolyData.L.ZEdge.Step[2],AX
            SHR   EAX,16
            JMP   @NotZeroZ
  @ZeroZ:
            MOV   WORD PTR PolyData.L.ZEdge.Step[2],AX
  @NotZeroZ:
            MOV   PolyData.L.ZEdge.Move,EAX
            MOV   PolyData.L.ZEdge.MoveDir,ESI

            POP   ESI
            POP   EBX
          End; // Asm
        End
        Else
        Begin
          PolyData.L.SEdge.Move    := 0;
          PolyData.L.SEdge.Step    := 0;
          PolyData.L.SEdge.MoveDir := 0;
          PolyData.L.ZEdge.Move    := 0;
          PolyData.L.ZEdge.Step    := 0;
          PolyData.L.ZEdge.MoveDir := 0;
        End;
      End
      Else
      Begin
        // Move to the next pixel using the move value and any error rollover

        Asm
          MOV   ECX,PolyData.L.SEdge.Pos
          MOV   EAX,PolyData.L.SEdge.Step
          MOV   EDX,PolyData.L.SEdge.Move
          ADD   PolyData.L.SEdge.Error,EAX
          JNC   @NoStep
          ADD   EDX,PolyData.L.SEdge.MoveDir
@NoStep:
          ADD   ECX,EDX
          JNS   @Positive
          SUB   ECX,ECX
          JMP   @Valid
@Positive:
          CMP   ECX,FW
          JB    @Valid
          MOV   ECX,FW1
@Valid:
          MOV   PolyData.L.SEdge.Pos,ECX

          // -------------------------------

          MOV   EAX,PolyData.L.ZEdge.Step
          MOV   EDX,PolyData.L.ZEdge.Move
          ADD   PolyData.L.ZEdge.Error,EAX
          JNC   @NoStepZ
          ADD   EDX,PolyData.L.ZEdge.MoveDir
@NoStepZ:
          ADD   PolyData.L.ZEdge.Pos,EDX
        End; // Asm
      End;

      // Get X coordinate of right edge

      If PolyData.R.Height <= 0 Then
      Begin
        PolyData.R.SEdge.Error := 0;
        PolyData.R.ZEdge.Error := 0;
        PolyData.R.SEdge.Pos   := PPrevR.X;//Polygon.Points[PrevIndexR].X;
        PolyData.R.ZEdge.Pos   := PPrevR.Z;//Polygon.Points[PrevIndexR].Z;
        PolyData.R.Width       := PCurrR.X - PolyData.R.SEdge.Pos;//Polygon.Points[CurIndexR].X - PolyData.XR;
        PolyData.R.Depth       := PCurrR.Z - PolyData.R.ZEdge.Pos;//Polygon.Points[CurIndexR].Z - PolyData.ZR;
        PolyData.R.Height      := PCurrR.Y - PPrevR.Y;//Polygon.Points[CurIndexR].Y - Polygon.Points[PrevIndexR].Y;
        If PolyData.R.Height <> 0 Then
        Begin
          // Calculate the move and step values

          Asm
            PUSH  EBX
            PUSH  ESI

            MOV   ECX,PolyData.R.Height
            MOV   EAX,PolyData.R.Width
            SUB   ESI,ESI

            TEST  EAX,EAX
            JZ    @Zero
            JNS   @Positive

            SUB   EDX,EDX
            NEG   EAX
            DEC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Width) / Abs(Height)
            MOV   WORD PTR PolyData.R.SEdge.Step[2],AX
            SHR   EAX,16
            NEG   EAX
            JMP   @NotZero
  @Positive:
            SUB   EDX,EDX
            INC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Width) / Abs(Height)
            MOV   WORD PTR PolyData.R.SEdge.Step[2],AX
            SHR   EAX,16
            JMP   @NotZero
  @Zero:
            MOV   WORD PTR PolyData.R.SEdge.Step[2],AX
  @NotZero:
            MOV   PolyData.R.SEdge.Move,EAX
            MOV   PolyData.R.SEdge.MoveDir,ESI

            // ------------------------------------------

            MOV   EAX,PolyData.R.Depth
            SUB   ESI,ESI

            TEST  EAX,EAX
            JZ    @ZeroZ
            JNS   @PositiveZ

            SUB   EDX,EDX
            NEG   EAX
            DEC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Width) / Abs(Height)
            MOV   WORD PTR PolyData.R.ZEdge.Step[2],AX
            SHR   EAX,16
            NEG   EAX
            JMP   @NotZeroZ
  @PositiveZ:
            SUB   EDX,EDX
            INC   ESI
            SHL   EAX,16          // Convert to fixed-point
            DIV   ECX             // Abs(Width) / Abs(Height)
            MOV   WORD PTR PolyData.R.ZEdge.Step[2],AX
            SHR   EAX,16
            JMP   @NotZeroZ
  @ZeroZ:
            MOV   WORD PTR PolyData.R.ZEdge.Step[2],AX
  @NotZeroZ:
            MOV   PolyData.R.ZEdge.Move,EAX
            MOV   PolyData.R.ZEdge.MoveDir,ESI

            POP   ESI
            POP   EBX
          End; // Asm
        End
        Else
        Begin
          PolyData.R.SEdge.Move    := 0;
          PolyData.R.SEdge.Step    := 0;
          PolyData.R.SEdge.MoveDir := 0;
          PolyData.R.ZEdge.Move    := 0;
          PolyData.R.ZEdge.Step    := 0;
          PolyData.R.ZEdge.MoveDir := 0;
        End;
      End
      Else
      Begin
        // Move to the next pixel using the move value and any error rollover

        Asm
          MOV   ECX,PolyData.R.SEdge.Pos
          MOV   EAX,PolyData.R.SEdge.Step
          MOV   EDX,PolyData.R.SEdge.Move
          ADD   PolyData.R.SEdge.Error,EAX
          JNC   @NoStep
          ADD   EDX,PolyData.R.SEdge.MoveDir
@NoStep:
          ADD   ECX,EDX
          JNS   @Positive
          SUB   ECX,ECX
          JMP   @Valid
@Positive:
          CMP   ECX,FW
          JB    @Valid
          MOV   ECX,FW1
@Valid:
          MOV   PolyData.R.SEdge.Pos,ECX

          // -------------------------------

          MOV   EAX,PolyData.R.ZEdge.Step
          MOV   EDX,PolyData.R.ZEdge.Move
          ADD   PolyData.R.ZEdge.Error,EAX
          JNC   @NoStepZ
          ADD   EDX,PolyData.R.ZEdge.MoveDir
@NoStepZ:
          ADD   PolyData.R.ZEdge.Pos,EDX
        End; // Asm
      End;

      // Draw the scanline.  We need to do a sanity check on the left and right positions because roundoff error
      // can make them flip.

      If PolyData.R.SEdge.Pos >= PolyData.L.SEdge.Pos Then
      Begin
        Asm
          // Figure out the move and step values for the depth gradient for this scan line

          PUSH  EDI
          PUSH  EBX
          PUSH  ESI

          // Get the scan line length and establish the start and end values

          MOV   EBX,PolyData.R.SEdge.Pos
          MOV   EDI,PolyData.L.SEdge.Pos
          MOV   ECX,EBX
          SUB   ECX,EDI
          INC   ECX             // ECX will contain the scan line length

          // Scan the grid buffer to determine the maximum (deepest) value.  If the scan line we want
          // to draw is deeper than this, then we can skip it.

          SHR   EDI,5           // Left grid index
          SHR   EBX,5           // Right grid index

          MOV   EDX,EDI
          SHL   EDX,16
          OR    EDX,EBX
          XOR   EDX,PolyData.GXLR0    // Get the last L+R grid indices we checked (-1 means we haven't check anything)
          JNZ   @GridChanged    // If either the grid row or end(s) have changed, rescan the grid row
          TEST  CanScanGrid,1   // Don't scan if we either found a dirty grid value or marked one as dirty
          JZ    @WriteScanLine
          MOV   EAX,MaxGridVal  // Same grid row and extents (and we haven't written anything yet), so the maximum value is the same
          JMP   @DoneScanGrid
@GridChanged:
          MOV   CanScanGrid,1   // The grid ends have changed (or the grid row has), so rescan it
@ScanGrid:
          SUB   EBX,EDI
          ADD   EDI,GIndex      // Add the offset to the grid row
          INC   EBX             // Number of grid elements to check
          SHL   EDI,2           // Change to a pointer to DWORD'S
          ADD   EDI,GP          // GP points to the grid z-buffer
          SUB   EAX,EAX         // 0 ........ EAX will eventually contain the maximum grid value we find
@GridScanLoop:
          MOV   ESI,[EDI]       // Read the grid value
          TEST  ESI,ESI         // If we find an element marked "dirty", don't bother scanning anymore
          JS    @FoundDirty
          CMP   ESI,EAX         // Compare with our max depth value
          JLE   @NotGreater
          MOV   EAX,ESI         // Store the maxiumum (farthest) grid value
@NotGreater:
          ADD   EDI,4           // Move to the next grid element
          DEC   EBX
          JNZ   @GridScanLoop
          MOV   MaxGridVal,EAX  // Save the maximum grid value so we can just reuse it instead of rescanning
@DoneScanGrid:
          // If both ends of the scanline are farther away than the grid depth then skip it entirely.
          // Conversely, if either end is closer than the grid depth then we want to draw the scanline.

          CMP   PolyData.L.ZEdge.Pos,EAX
          JL    @FoundDirty     // If the left edge is closer then draw the scanline
          CMP   PolyData.R.ZEdge.Pos,EAX
          JGE   @Done           // If the right edge is farther (and to get here the left edge is farther as well) then skip the scanline

          // If we're going to write the scanline (whether because of a dirty grid or because our scanline isn't deeper), we don't want to scan this grid row anymore
@FoundDirty:
          MOV   CanScanGrid,0
@WriteScanLine:

          // Figure out the move and step values for the depth gradient for this scan line

          SUB   EDX,EDX
          MOV   EAX,PolyData.R.ZEdge.Pos
          SUB   EAX,PolyData.L.ZEdge.Pos
          JZ    @ZeroMoveDirZ
          JNS   @Positive

          NEG   EAX             // Make it positive: we'll subtract rather than add so we can handle carry in one instruction
          SHL   EAX,16          // Convert to fixed-point
          DIV   ECX             // Abs(Width) / Abs(Height)
          SHRD  EDX,EAX,16      // StepZ
          SHR   EAX,16
          JMP   @NegMoveDirZ
@Positive:
          

          SHL   EAX,16          // Convert to fixed-point
          DIV   ECX             // Abs(Width) / Abs(Height)
          SHRD  EDX,EAX,16      // StepZ
          SHR   EAX,16          // EAX will now contain the MoveZ value

          // Write the scanline to the Z buffer

          // MoveDirZ is 1

          MOV   EDI,YIndex
          SUB   EBX,EBX                // Start with depth error term = 0
          ADD   EDI,PolyData.L.SEdge.Pos
          SHL   EDI,2
          MOV   ESI,PolyData.L.ZEdge.Pos // Get the current depth value
          ADD   EDI,ZP                 // ZP points to the Z buffer
@PixelLoopPos:
          CMP   ESI,[EDI]              // Only write the pixel if it's closer than what's already there
          JAE   @AlreadyCloserPos
          MOV   [EDI],ESI
@AlreadyCloserPos:
          ADD   EDI,4
          ADD   EBX,EDX         // Add StepZ to ErrorZ
          ADC   ESI,EAX         // Add MoveZ to the current depth value, adding carry also if necessary

          DEC   ECX
          JNZ   @PixelLoopPos
          JMP   @MarkGridEntries

          // MoveDirZ is -1

@NegMoveDirZ:

          MOV   EDI,YIndex
          SUB   EBX,EBX                // Start with depth error term = 0
          ADD   EDI,PolyData.L.SEdge.Pos
          SHL   EDI,2
          MOV   ESI,PolyData.L.ZEdge.Pos // Get the current depth value
          ADD   EDI,ZP                 // ZP points to the Z buffer
@PixelLoopNeg:
          CMP   ESI,[EDI]              // Only write the pixel if it's closer than what's already there
          JAE   @AlreadyCloserNeg
          MOV   [EDI],ESI
@AlreadyCloserNeg:
          ADD   EDI,4
          ADD   EBX,EDX         // Add StepZ to ErrorZ
          SBB   ESI,EAX         // Subtract MoveZ from the current depth value, subtracting carry also if necessary
          DEC   ECX
          JNZ   @PixelLoopNeg
          JMP   @MarkGridEntries

          // MoveDirZ is 0, which means that so are MoveZ and StepZ

@ZeroMoveDirZ:

          MOV   EDI,YIndex
          ADD   EDI,PolyData.L.SEdge.Pos
          SHL   EDI,2
          MOV   ESI,PolyData.L.ZEdge.Pos
          ADD   EDI,ZP
@PixelLoopZero:
          CMP   ESI,[EDI]
          JAE   @AlreadyCloserZero
          MOV   [EDI],ESI
@AlreadyCloserZero:
          ADD   EDI,4
          DEC   ECX
          JNZ   @PixelLoopZero

@MarkGridEntries:
          // Mark the grid entries as dirty

          MOV   EDI,PolyData.L.SEdge.Pos
          MOV   ECX,PolyData.R.SEdge.Pos
          SHR   EDI,5
          SHR   ECX,5

          MOV   EDX,EDI
          SHL   EDX,16
          OR    EDX,ECX
          MOV   EAX,EDX
          XOR   EDX,PolyData.GXLR0
          JZ    @NoGridChange
          MOV   PolyData.GXLR0,EAX        // Save our combined L+R grid indices

          SUB   ECX,EDI
          ADD   EDI,GIndex
          INC   ECX
          SHL   EDI,2
          MOV   EAX,0FFFFFFFFh  // -1
          ADD   EDI,GP
          CLD
          REP   STOSD
@NoGridChange:

@Done:
          POP   ESI
          POP   EBX
          POP   EDI
        End; // Asm
      End;

      // Move to the next scanline

      Inc(Y);
      Inc(YIndex,FW);
      Dec(ScanLines);
      Dec(PolyData.L.Height);
      Dec(PolyData.R.Height);
      If (Y And $1F) = 0 Then
      Begin
        // Move to the next grid row
        
        Inc(GIndex,FGridWidth);
        PolyData.GXLR0 := -1;
      End;
      If PolyData.L.Height <= 0 Then
      Begin
        PrevIndexL := CurIndexL;
        PPrevL     := PCurrL;
        CurIndexL  := Lookup[CurIndexL + 1];
        PCurrL     := @(Polygon.Points[CurIndexL]);
      End;
      If PolyData.R.Height <= 0 Then
      Begin
        PrevIndexR := CurIndexR;
        PPrevR     := PCurrR;
        CurIndexR  := Lookup[CurIndexR - 1];
        PCurrR     := @(Polygon.Points[CurIndexR]);
      End;
    End; // While
  End;
End; // TOcclusionManager.FillSimpleScreenPolygonHorz

Procedure TOcclusionManager.FillSimpleScreenPolygon(Var Polygon: TSimpleScreenPolygon; Horizontal: Boolean);
Begin
  If Horizontal
   Then FillSimpleScreenPolygonHorz(Polygon)
   Else FillSimpleScreenPolygonVert(Polygon);
End; // TOcclusionManager.FillSimpleScreenPolygon

// ---------------------------
// TGLSLShaderManager
// ---------------------------

Constructor TGLSLShaderManager.Create;
Begin
  Inherited;
  FVertexShaderSource     := TStringStringHash.Create(True);   // Must be thread-safe because the app thread will put into it and the rendering thread will read from it
  FFragmentShaderSource   := TStringStringHash.Create(True);   // Must be thread-safe because the app thread will put into it and the rendering thread will read from it
  FVertexShaders          := TStringList.Create;
  FFragmentShaders        := TStringList.Create;
  FPrograms               := TStringList.Create;
  FVertexShaders.Sorted   := True;
  FFragmentShaders.Sorted := True;
  FPrograms.Sorted        := True;
  FEnabled                := True;
End; // TGLSLShaderManager.Create

Destructor TGLSLShaderManager.Destroy;
Begin
  DestroyPrograms;
  DestroyShaders;
  FVertexShaders.Free;
  FFragmentShaders.Free;
  FPrograms.Free;
  FVertexShaderSource.Free;
  FFragmentShaderSource.Free;
  Inherited;
End; // TGLSLShaderManager.Destroy

Procedure TGLSLShaderManager.SetEnabled(B: Boolean);
Begin
  FEnabled := B;
End; // TGLSLShaderManager.SetEnabled

Function TGLSLShaderManager.IsAvailable: Boolean;
Begin
  Result := GL_ARB_shader_objects And
            GL_ARB_shading_language_100 And
            GL_ARB_vertex_shader And
            GL_ARB_fragment_shader;
End; // TGLSLShaderManager.IsAvailable

Procedure TGLSLShaderManager.DestroyPrograms;
// Destroys any OpenGL shader programs that have been created
Var I,J: Integer;
Begin
  For I := 0 To FPrograms.Count - 1 Do
  Begin
    J := Integer(FPrograms.Objects[I]);
    If J <> 0 Then glDeleteObjectARB(Integer(FPrograms.Objects[I]));
  End; // For I
  FPrograms.Clear;
End; // TGLSLShaderManager.DestroyPrograms

Procedure TGLSLShaderManager.DestroyShaders;
// Destroys any OpenGL shader objects that have been created
Var I: Integer;
Begin
  For I := 0 To FVertexShaders.Count - 1 Do glDeleteObjectARB(Integer(FVertexShaders.Objects[I]));
  For I := 0 To FFragmentShaders.Count - 1 Do glDeleteObjectARB(Integer(FFragmentShaders.Objects[I]));
  FVertexShaders.Clear;
  FFragmentShaders.Clear;
End; // TGLSLShaderManager.DestroyShaders

Function TGLSLShaderManager.CreateShader(Const ShaderType: TGLSLShaderType; Const ShaderName,ShaderText: String): Boolean;
Var Shader: GLhandleARB;
Begin
  Result := False;
  If ShaderType = stVertex Then
  Begin
    Try
      Shader := LoadShader(ShaderText,GL_VERTEX_SHADER_ARB);
      FVertexShaders.AddObject(ShaderName,Pointer(Shader));
      Result := True;
    Except
    End;
  End
  Else
  Begin
    Try
      Shader := LoadShader(ShaderText,GL_FRAGMENT_SHADER_ARB);
      FFragmentShaders.AddObject(ShaderName,Pointer(Shader));
      Result := True;
    Except
    End;
  End;
End; // TGLSLShaderManager.CreateShader

Procedure TGLSLShaderManager.AddShader(Const ShaderType: TGLSLShaderType; Const ShaderName,ShaderText: String);
Begin
  If IsAvailable Then
  Begin
    If ShaderType = stVertex
     Then FVertexShaderSource.Put(ShaderName,ShaderText)
     Else FFragmentShaderSource.Put(ShaderName,ShaderText);
  End;
End; // TGLSLShaderManager.AddShader

Function TGLSLShaderManager.CreateShaders: Boolean;
// Iterates through all registered shaders and creates OpenGL shader objects
Var
  I              : Integer;
  ShaderName     : String;
  ShaderSource   : String;
  CreateVertex   : Boolean;
  CreateFragment : Boolean;

Begin
  CreateVertex   := True;
  CreateFragment := True;
  If IsAvailable Then
  Begin
    Try
      FVertexShaderSource.Lock;
      For I := 0 To FVertexShaderSource.Count - 1 Do CreateVertex := CreateVertex And CreateShader(stVertex,StrPas(FVertexShaderSource.Items[I].Key.Str),StrPas(FVertexShaderSource.Items[I].Value.Str));
      FVertexShaderSource.Clear;
    Finally
      FVertexShaderSource.Unlock;
    End;

    Try
      FFragmentShaderSource.Lock;
      For I := 0 To FFragmentShaderSource.Count - 1 Do CreateFragment := CreateFragment And CreateShader(stFragment,StrPas(FFragmentShaderSource.Items[I].Key.Str),StrPas(FFragmentShaderSource.Items[I].Value.Str));
      FFragmentShaderSource.Clear;
    Finally
      FFragmentShaderSource.Unlock;
    End;
  End;
  Result := CreateVertex Or CreateFragment;
End; // TGLSLShaderManager.CreateShaders

Function TGLSLShaderManager.CreateProgram(Const VertexShader,FragmentShader: String): Boolean;
// This has to be called in the render thread to work
Begin
  Result := CreateProgram(GetProgramName(VertexShader,FragmentShader));
End; // TGLSLShaderManager.CreateProgram

Function TGLSLShaderManager.CreateProgram(Const ProgramName: String): Boolean;
// This has to be called in the render thread to work
Var
  I,J,K,L        : Integer;
  VertexShader   : String;
  FragmentShader : String;
  ProgramObject  : GLhandleARB;
  Linked         : Integer;
  Log            : String;

Begin
  // Find the index of the program

  I := FPrograms.IndexOf(ProgramName);

  // Get the program handle

  J := Integer(FPrograms.Objects[I]);

  // If the handle is 0, it hasn't been created yet

  If J = 0 Then
  Begin
    GetShaderNames(ProgramName,VertexShader,FragmentShader);
    If (VertexShader <> '') Or (FragmentShader <> '') Then
    Begin
      K := FVertexShaders.IndexOf(VertexShader);
      L := FFragmentShaders.IndexOf(FragmentShader);
      If ((VertexShader <> '') And (K >= 0)) Or ((FragmentShader <> '') And (L >= 0)) Then
      Begin
        ProgramObject := glCreateProgramObjectARB;
        If K >= 0 Then glAttachObjectARB(ProgramObject, Integer(FVertexShaders.Objects[K]));
        If L >= 0 Then glAttachObjectARB(ProgramObject, Integer(FFragmentShaders.Objects[L]));
        glLinkProgramARB(ProgramObject);
        glGetObjectParameterivARB(ProgramObject, GL_OBJECT_LINK_STATUS_ARB, @Linked);
        Log := GetOpenGLInfoLog(ProgramObject);
        If Linked <> GL_TRUE Then
        Begin
          Result := False;
          Raise Exception.Create(Log);
        End;
        FPrograms.Objects[I] := Pointer(ProgramObject);
      End;
    End;
  End;
End; // TGLSLShaderManager.CreateProgram

Function TGLSLShaderManager.CreatePrograms: Boolean;
// Iterates through all registered program names and creates OpenGL shader programs
//
// This has to be called in the render thread to work
Var I: Integer;
Begin
  Result := True;
  If IsAvailable Then
  Begin
    For I := 0 To FPrograms.Count - 1 Do Result := Result And CreateProgram(FPrograms.Strings[I]);
  End;
End; // TGLSLShaderManager.CreatePrograms

Procedure TGLSLShaderManager.RegisterProgram(Const VertexShader,FragmentShader: String);
Var ProgramName: String;
Begin
  If IsAvailable Then
  Begin
    ProgramName := GetProgramName(VertexShader,FragmentShader);
    If FPrograms.IndexOf(ProgramName) < 0 Then FPrograms.AddObject(ProgramName,Nil);   // Initially the handle is 0, i.e. the program hasn't been created yet
  End;
End; // TGLSLShaderManager.RegisterProgram

Procedure TGLSLShaderManager.SetCurrentProgram(Const VertexShader,FragmentShader: String);
// This has to be called in the render thread to work
Var
  I           : Integer;
  ProgramName : String;
  
Begin
  If IsAvailable Then
  Begin
    If Enabled Then
    Begin
      ProgramName := GetProgramName(VertexShader,FragmentShader);
      If ProgramName <> '' Then
      Begin
        I := FPrograms.IndexOf(ProgramName);
        If I >= 0
         Then FCurrentProgramHandle := Integer(FPrograms.Objects[I])  // If the program hasn't been created then the value will be 0, which turns shaders off anyway
         Else FCurrentProgramHandle := 0;
      End
      Else FCurrentProgramHandle := 0;
      glUseProgramObjectARB(FCurrentProgramHandle);
    End
    Else glUseProgramObjectARB(0);
  End;
End; // TGLSLShaderManager.SetCurrentProgram

Function TGLSLShaderManager.GetProgramName(Const VertexShader,FragmentShader: String): String;
// Internal universal mechanism for creating a program name as a hash of the vertex and fragment shaders that make it up
Begin
  Result := VertexShader + '*' + FragmentShader;
End; // TGLSLShaderManager.GetProgramName

Procedure TGLSLShaderManager.GetShaderNames(Const ProgramName: String; Out VertexShader,FragmentShader: String);
// Internal universal mechanism for retrieving the vertex and fragment shader names from a program name
Var I: Integer;
Begin
  I := Pos('*',ProgramName);
  If I > 0 Then
  Begin
    VertexShader   := Copy(ProgramName,1,I - 1);
    FragmentShader := Copy(ProgramName,I + 1,Length(ProgramName));
  End
  Else
  Begin
    // There should always be an asterisk, so if we get here default to no shaders
    
    VertexShader   := '';
    FragmentShader := '';
  End;
End; // TGLSLShaderManager.GetShaderNames

Procedure TGLSLShaderManager.SetDefaultProgram(Const VertexShader,FragmentShader: String);
Begin
  If IsAvailable Then FDefaultProgram := GetProgramName(VertexShader,FragmentShader);
End; // TGLSLShaderManager.SetDefaultProgram

Procedure TGLSLShaderManager.GetDefaultProgram(Out VertexShader,FragmentShader: String);
Begin
  If IsAvailable Then GetShaderNames(FDefaultProgram,VertexShader,FragmentShader)
  Else
  Begin
    VertexShader   := '';
    FragmentShader := '';
  End;
End; // TGLSLShaderManager.GetDefaultProgram

Function TGLSLShaderManager.GetOpenGLInfoLog(Const S: GLhandleARB): String;
Var
  BLen    : Integer;
  SLen    : Integer;
  InfoLog : Packed Array Of Char;

Begin
  glGetObjectParameterivARB(S, GL_OBJECT_INFO_LOG_LENGTH_ARB, @BLen);
  If BLen > 1 then
  Begin
    SetLength(InfoLog, BLen);
    glGetInfoLogARB(S, BLen, SLen, @InfoLog[0]);
    Result := String(InfoLog);
  End
  Else Result := '';
End; // TGLSLShaderManager.GetOpenGLInfoLog

Function TGLSLShaderManager.LoadShader(Const Src: String; Const ShaderType: GLenum): GLhandleARB;
// Src is the shader text
// ShaderType is either GL_VERTEX_SHADER_ARB or GL_FRAGMENT_SHADER_ARB
//
// This has to be called in the render thread to work
Var
  Source   : PChar;
  Compiled : Integer;
  Len      : Integer;
  Log      : String;

Begin
  Source := PChar(Src);
  Len    := Length(Src);
  Result := glCreateShaderObjectARB(ShaderType);            // glCreateShader in OpenGL 2.0

  glShaderSourceARB(Result, 1, @Source, @Len);
  glCompileShaderARB(Result);
  glGetObjectParameterivARB(Result, GL_OBJECT_COMPILE_STATUS_ARB, @Compiled);
  Log := GetOpenGLInfoLog(Result);

  If Compiled <> GL_TRUE then
  Begin
    Raise Exception.Create(Log);  
  End;
End; // TGLSLShaderManager.LoadShader

// ---------------------------
// TMaterial
// ---------------------------

Constructor TMaterial.Create;
Begin
  Inherited Create;
  FOwner         := Nil;
  Name           := '';
  VertexShader   := '';
  FragmentShader := '';
  Shininess      := 1;
End; // TMaterial.Create

Constructor TMaterial.Create(AOwner: TSceneGL);
Begin
  Inherited Create;
  FOwner         := AOwner;
  Name           := '';
  VertexShader   := '';
  FragmentShader := '';
  Shininess      := 1;
End; // TMaterial.Create

Procedure TMaterial.Activate;
Begin
  If FOwner <> Nil Then FOwner.ShaderManager.SetCurrentProgram(VertexShader,FragmentShader);
End; // TMaterial.Activate

// ---------------------------
// TDynamicLight
// ---------------------------

Constructor TDynamicLight.Create;
Begin
  Inherited Create;
  FOwner       := Nil;
  Sphere       := TSphere.Create;
  Color[0]     := 1;
  Color[1]     := 1;
  Color[2]     := 1;
  Color[3]     := 1;
  Flicker      := 0;
  CalcTime     := 0;
  CurColor[0]  := 1;
  CurColor[1]  := 1;
  CurColor[2]  := 1;
  CurColor[3]  := 1;
  OldColor[0]  := 1;
  OldColor[1]  := 1;
  OldColor[2]  := 1;
  OldColor[3]  := 1;
  DestColor[0] := 1;
  DestColor[1] := 1;
  DestColor[2] := 1;
  DestColor[3] := 1;
  X            := 0;
  Y            := 0;
  Z            := 0;
End; // TDynamicLight.Create

Constructor TDynamicLight.Create(AOwner: TSceneGL);
Begin
  Inherited Create;
  FOwner       := AOwner;
  Sphere       := TSphere.Create;
  Color[0]     := 1;
  Color[1]     := 1;
  Color[2]     := 1;
  Color[3]     := 1;
  Flicker      := 0;
  CalcTime     := 0;
  CurColor[0]  := 1;
  CurColor[1]  := 1;
  CurColor[2]  := 1;
  CurColor[3]  := 1;
  OldColor[0]  := 1;
  OldColor[1]  := 1;
  OldColor[2]  := 1;
  OldColor[3]  := 1;
  DestColor[0] := 1;
  DestColor[1] := 1;
  DestColor[2] := 1;
  DestColor[3] := 1;
  X            := 0;
  Y            := 0;
  Z            := 0;
End; // TDynamicLight.Create

Constructor TDynamicLight.Create(Light: TDynamicLight);
Begin
  Inherited Create;
  FOwner       := Light.FOwner;
  Sphere       := TSphere.Create(Light.Sphere);
  Color[0]     := Light.Color[0];
  Color[1]     := Light.Color[1];
  Color[2]     := Light.Color[2];
  Color[3]     := Light.Color[3];
  Flicker      := Light.Flicker;
  CalcTime     := Light.CalcTime;
  CurColor[0]  := Light.CurColor[0];
  CurColor[1]  := Light.CurColor[1];
  CurColor[2]  := Light.CurColor[2];
  CurColor[3]  := Light.CurColor[3];
  OldColor[0]  := Light.OldColor[0];
  OldColor[1]  := Light.OldColor[1];
  OldColor[2]  := Light.OldColor[2];
  OldColor[3]  := Light.OldColor[3];
  DestColor[0] := Light.DestColor[0];
  DestColor[1] := Light.DestColor[1];
  DestColor[2] := Light.DestColor[2];
  DestColor[3] := Light.DestColor[3];
  X            := Light.X;
  Y            := Light.Y;
  Z            := Light.Z;
End; // TDynamicLight.Create

Destructor TDynamicLight.Destroy;
Begin
  Sphere.Free;
  Inherited;
End; // TDynamicLight.Destroy

Procedure TDynamicLight.CalculateActualLight(Time: Integer);
Const Interval = 150;
Var
  F        : Single;
  C        : TColor;
  H,S,L    : Double;
  R,G,B    : Integer;

Begin
  If FOwner.ShaderManager.IsAvailable And FOwner.ShaderManager.Enabled Then
  Begin
    If Time > CalcTime + Interval Then
    Begin
      // Calculate position

      X := Sphere.Center.X;
      Y := Sphere.Center.Y;
      Z := Sphere.Center.Z;

      If Flicker > 0 Then
      Begin
        // Slightly perturb the light position

        X := X + (Random * 0.5) - 0.25;
        Y := Y + (Random * 0.5) - 0.25;
        Z := Z + (Random * 0.5) - 0.25;
      End;

      // Calculate brightness

      OldColor[0]      := DestColor[0];
      OldColor[1]      := DestColor[1];
      OldColor[2]      := DestColor[2];

      DestColor[0]     := Color[0];
      DestColor[1]     := Color[1];
      DestColor[2]     := Color[2];

      // Calculate the color based on flicker

      If Flicker > 0 Then
      Begin
        F := Min(Flicker,1);

        // The correct way to do this is to convert to HSL space, then back to RGB

        R := Round(Color[2] * 255);
        G := Round(Color[1] * 255);
        B := Round(Color[0] * 255);

        C := R Or (G Shl 8) Or (B Shl 16);
        RGBtoHSL(C,H,S,L);
        L := L * (1 - Random * F);
        C := HSLToRGB(H,S,L);
        DestColor[2] := (C And $FF) / 255;
        DestColor[1] := ((C Shr 8) And $FF) / 255;
        DestColor[0] := ((C Shr 16) And $FF) / 255;
      End;
      CalcTime := Time;
      CurColor[0] := OldColor[0];
      CurColor[1] := OldColor[1];
      CurColor[2] := OldColor[2];
    End
    Else
    Begin
      // Interpolate the current color (doing this with RGB as opposed to HSL)

      CurColor[0] := OldColor[0] + (DestColor[0] - OldColor[0]) * Min(Interval,(Time - CalcTime)) / Interval;
      CurColor[1] := OldColor[1] + (DestColor[1] - OldColor[1]) * Min(Interval,(Time - CalcTime)) / Interval;
      CurColor[2] := OldColor[2] + (DestColor[2] - OldColor[2]) * Min(Interval,(Time - CalcTime)) / Interval;
    End;
  End;
End; // TDynamicLight.CalculateActualLight

Procedure TDynamicLight.SetShaderParameter(Index: Integer);
// Updates the shader parameters
Var
  IUniform : Integer;
  St       : String;
  NUniform : Packed Array [0..31] Of Char;
  Col      : Array[0..2] Of Single;

Begin
  If FOwner.ShaderManager.IsAvailable And FOwner.ShaderManager.Enabled Then
  Begin
    St       := 'LightPosition[' + IntToStr(Index) + ']';
    StrPCopy(NUniform,St);
    IUniform := glGetUniformLocationARB(FOwner.ShaderManager.CurrentProgramHandle, @NUniform[0]);
    If (IUniform <> GL_INVALID_VALUE) And (IUniform <> GL_INVALID_OPERATION) And (IUniform <> -1) Then glUniform3fARB(IUniform,X,Y,Z);

    St       := 'LightColor[' + IntToStr(Index) + ']';
    StrPCopy(NUniform,St);
    IUniform := glGetUniformLocationARB(FOwner.ShaderManager.CurrentProgramHandle, @NUniform[0]);
    If (IUniform <> GL_INVALID_VALUE) And (IUniform <> GL_INVALID_OPERATION) And (IUniform <> -1) Then glUniform3fARB(IUniform,CurColor[0],CurColor[1],CurColor[2]);

    St       := 'LightRadius[' + IntToStr(Index) + ']';
    StrPCopy(NUniform,St);
    IUniform := glGetUniformLocationARB(FOwner.ShaderManager.CurrentProgramHandle, @NUniform[0]);
    If (IUniform <> GL_INVALID_VALUE) And (IUniform <> GL_INVALID_OPERATION) And (IUniform <> -1) Then glUniform1fARB(IUniform,Sphere.Radius);
  End;
End; // TDynamicLight.SetShaderParameter

// ---------------------------
// TSceneGL
// ---------------------------

Constructor TSceneGL.Create(AOwner: TObject);
Begin
  LogToFile('TSceneGL.Create() begin');

  StrPCopy(FWinClassName,'MY_WINDOWS_CLASS_' + IntToHex(Integer(@Self),8));
  Owner                      := AOwner;
  LogProc                    := Nil;
  WindowHandle               := 0;
  FBSPTreeRoot               := TBSPTreeNode.Create(Self);
  FBSPTreeMutex              := TCriticalSection.Create;
  FWaitMutex                 := TCriticalSection.Create;
  Lights                     := TList.Create;
  Cameras                    := TList.Create;
  Entities                   := TThreadSafeList.Create;
  Renderables                := TThreadSafeList.Create;
  Models                     := TThreadSafeList.Create;
  DynamicLights              := TThreadSafeList.Create;
  Materials                  := TThreadSafeList.Create;
  Entities.OnCreateItem      := CreateEntity;
  Renderables.OnCreateItem   := CreateRenderable;
  Models.OnCreateItem        := CreateModel;
  DynamicLights.OnCreateItem := CreateDynamicLight;
  FOcclusionManager          := TOcclusionManager.Create(Self);
  DefaultCamera              := TCamera.Create(Self);
  ActiveCamera               := DefaultCamera;
  FOcclusionManager.Frustum  := DefaultCamera.Frustum;
  Cameras.Add(DefaultCamera);        // Add the default camera to the cameras in the scene
  Active                     := False;
  FPerspective               := True;
  FogEnabled                 := False;
  Angle                      := 30;            // Degrees for Field-of-view angle
  DistNear                   := 1;             // 1 unit of distance to near clipping plane
  DistFar                    := 100;           // Distance to far clipping plane
  Texturing                  := True;          // Use textures
  bTranslucentMode           := False;         // Don't ignore depth buffer
  Textures                   := TStringList.Create;
  Redrawing                  := False;
  FCameraNode                := Nil;
  FrustumCulling             := True;
  FCheckHidden               := True;
  FFrameCount                := 0;
  FRepaintMonitor            := TMonitor.Create;

  FCanCreateShaders          := True;
  FCanCreatePrograms         := True;

  FShaderManager             := TGLSLShaderManager.Create;

//  UI                         := TUserInterface.Create(Self);
//  SkinUI                     := TGLUI.Create(Self);
  FUpdatedArea               := False;
  FAddingTexture             := False;
  SampleText                 := Nil;
  DC                         := 0;
  HRC                        := 0;
  FLastActiveWindow          := 0;
  RedrawTime                 := GetTickCount;

  FSkySphere                 := TSkySphere.Create(Self);
  FSkySphere1                := TSkySphere.Create(Self);
  FStarField                 := TStarField.Create(Self);
  FSun                       := TSatellite.Create(Self);
  FMoon                      := TSatellite.Create(Self);
  FPrecipitation             := TPrecipitation.Create(Self);
  FSun.Visible               := False;
  FMoon.Visible              := False;

  RedrawQueue                := TList.Create;

  FMovementSphere            := TSphere.Create;
  FNewCenterPoint            := T3DPoint.Create{Point};
  FNewVelocity               := T3DPoint.Create{Point};
  FGravity                   := T3DPoint.Create{Point};

  // Create the render thread

  LogToFile('TSceneGL.Create(): creating render thread');
  RenderThread := TRenderThread.Create(Self);
  RenderThread.FreeOnTerminate := False;
{
  // Start the render thread

  LogToFile('TSceneGL.Create(): starting render thread');
  RenderThread.Resume;

  // Wait until the render thread loop has started

  LogToFile('TSceneGL.Create(): waiting until render thread is running');
  While Not RenderThread.Running Do Sleep(100);
}  
  LogToFile('TSceneGL.Create() end');
End; // TSceneGL.Create

Destructor TSceneGL.Destroy;
Var I: Integer;
Begin
  LogToFile('TSceneGL.Destroy(' + Name + '): Begin, Active = ' + BoolToStr(Active,True));

  If Not RenderThread.Running Then
  Begin
    RenderThread.Resume;
    While Not RenderThread.Running Do Sleep(1);
  End;







  If Active Then
  Begin
    SetActive(False);
    ReleaseRC;
  End;
  While Redrawing Do Sleep(1);



//  SkinUI.Free;
//  UI.Free;
  For I := 0 To Cameras.Count  - 1 Do TObject(Cameras.Items[I]).Free;
  For I := 0 To Lights.Count   - 1 Do TObject(Lights.Items[I]).Free;


  DynamicLights.FreeAll;
  Entities.FreeAll;
  Renderables.FreeAll;
  Models.FreeAll;
  Materials.FreeAll;

  FreeAllTextures;

  Textures.Free;
  Cameras.Free;
  Lights.Free;
  Entities.Free;
  Renderables.Free;
  Models.Free;
  DynamicLights.Free;
  Materials.Free;


  FGravity.Free;
  FNewVelocity.Free;
  FNewCenterPoint.Free;
{
  FGravity        := Nil;
  FNewVelocity    := Nil;
  FNewCenterPoint := Nil;
}  
  FMovementSphere.Free;



  FPrecipitation.Free;
  FMoon.Free;
  FSun.Free;
  FStarField.Free;
  FSkySphere.Free;
  FSkySphere1.Free;


  // Tell the render thread to stop

  RenderThread.Terminate;

  // Wait until the render thread has stopped

  While RenderThread.Running Do Sleep(100);

  FRepaintMonitor.Free;

  RenderThread.Free;

  RedrawQueue.Free;

  FOcclusionManager.Free;

  FShaderManager.Free;

  FBSPTreeRoot.Free;
  FBSPTreeMutex.Free;
  FWaitMutex.Free;
  LogToFile('TSceneGL.Destroy(): End');
  Inherited;
End; // TSceneGL.Destroy

Function TSceneGL.GetNearbyDynamicLights(Sphere: TSphere; Position: T3DPoint): TStringList;
Var
  List     : TStringList;
  I        : Integer;
  Light    : TDynamicLight;
  NewLight : TDynamicLight;

Begin
  // Implementation designed for speed

  List := Nil;

  // Don't bother if we don't have shaders

  If FShaderManager.IsAvailable And FShaderManager.Enabled Then
  Begin
    Try
      DynamicLights.Lock;
      For I := 0 To DynamicLights.FMaxIndex Do
      Begin
        Light := TDynamicLight(DynamicLights.FItems[I]);
        If (Light.Sphere.Radius > 0) And  Light.Sphere.Intersects(Sphere) Then
        Begin
          If List = Nil Then
          Begin
            List := TStringList.Create;
            List.Sorted := True;
          End;
          NewLight := TDynamicLight.Create(Light);
          NewLight.Sphere.Center.Subtract(Position);
          List.AddObject(IntToStr(Round(Light.Sphere.Center.DistanceFrom2(Position) * 1000)),NewLight); // Sort by distance, nearest first
        End;
      End; // For I
    Finally
      DynamicLights.Unlock;
    End;
  End;
  Result := List;
End; // TSceneGL.GetNearbyDynamicLights

Procedure TSceneGL.LoadDefaultShaders;
Var VertexShader,FragmentShader: String;
Begin
  FShaderManager.GetDefaultProgram(VertexShader,FragmentShader);
  FShaderManager.SetCurrentProgram(VertexShader,FragmentShader);
End; // TSceneGL.LoadDefaultShaders

Procedure TSceneGL.SetVSync(VSyncOn: Boolean);
Begin
  If @wglSwapIntervalEXT <> Nil Then
  Begin
    If VSyncOn
     Then wglSwapIntervalEXT(1)
     Else wglSwapIntervalEXT(0);
  End;
End; // TSceneGL.SetVSync

Procedure TSceneGL.StartRenderThread;
Begin
  // Start the render thread

  LogToFile('TSceneGL.StartRenderThread(): starting render thread');
  RenderThread.Resume;

  // Wait until the render thread loop has started

  LogToFile('TSceneGL.StartRenderThread(): waiting until render thread is running');
  While Not RenderThread.Running Do Sleep(100);
  
  LogToFile('TSceneGL.StartRenderThread(): End');
End; // TSceneGL.StartRenderThread

//PROFILE-NO
Procedure TSceneGL.LockBSPTree(ThreadName: String);
Begin
  FBSPTreeMutex.Enter;
End; // TSceneGL.LockBSPTree
//PROFILE-YES

Procedure TSceneGL.UnlockBSPTree;
Begin
  FBSPTreeMutex.Leave;
End; // TSceneGL.UnlockBSPTree

Procedure TSceneGL.WaitUntilDoneRepainting;
Begin
  FWaitMutex.Enter;
  FWaitMutex.Leave;
End; // TSceneGL.WaitUntilDoneRepainting
{
Procedure TSceneGL.UnlockRenderable;
Begin
  FRenderableMutex.Leave;
End; // TSceneGL.UnlockRenderable
}
Procedure TSceneGL.ClearScene(ClearRenderables,ClearModels: Boolean);
Var B: Boolean;
Begin
//  LogToFile('TSceneGL.ClearScene(' + Name + ') Begin');

  B      := Active;
  Active := False;
  While Redrawing Do Sleep(1);
  Try
    LockBSPTree('TSceneGL.ClearScene');
    Entities.FreeAll;
    If ClearRenderables Then Renderables.FreeAll;
    If ClearModels      Then Models.FreeAll;
    DynamicLights.FreeAll;
    Materials.FreeAll;
    RedrawQueue.Clear;
    FBSPTreeRoot.Free;
    FBSPTreeRoot := TBSPTreeNode.Create(Self);
    FCameraNode  := Nil;
//    ActiveCamera.Entity.Free;
//    ActiveCamera.Entity := TEntity.Create(Self);//TEntity(Owner.Entities.GetNew(0));
//    ActiveCamera.Entity.Visible := False;
  Finally
    UnlockBSPTree;
  End;
  Active := B;

//  LogToFile('TSceneGL.ClearScene(' + Name + ') End');
End; // TSceneGL.ClearScene

Procedure TSceneGL.MoveEntityToNode(Entity: TEntity; NewNode: TBSPTreeNode);
Begin
  Try
    LockBSPTree('TSceneGL.MoveEntityToNode');
    If Entity.BSPTreeNode <> NewNode Then
    Begin
      If Entity.BSPTreeNode <> Nil Then Entity.BSPTreeNode.RemoveEntity(Entity);
      NewNode.AddEntity(Entity);
    End;
  Finally
    UnlockBSPTree;
  End;
End; // TSceneGL.MoveEntityToNode

Function TSceneGL.AddTexture(FileName,OpacityFileName: String; NeedsMask: Boolean): TTexture;
Var
  I       : Integer;
  Texture : TTexture;

Begin
//  LogToFile('TSceneGL.AddTexture(' + Name + ') Begin');

  // Load the texture either if we have never loaded it before or if we are
  // changing the opacity mask

  I := Textures.IndexOf(FileName);
  If (I < 0) Or (TTexture(Textures.Objects[I]).Opacity <> OpacityFileName) Then
  Begin
    Texture           := TTexture.Create(Self,Nil);
    Texture.Opacity   := OpacityFileName;
    Texture.NeedsMask := NeedsMask;
    Texture.LoadTexture(FileName,OpacityFileName,False);
//    Texture.Loaded    := (Texture.LoadTexture(FileName,OpacityFileName) >= 0);

    FAddingTexture := True;
    While Redrawing Do Sleep(1);
    Try
      LockBSPTree('TSceneGL.AddTexture');

    // If we are changing the opacity mask of an existing texture then we need to kill the existing one
    // and insert the new one in its place

      If I >= 0 Then
      Begin
        TTexture(Textures.Objects[I]).Free;
        Textures.Objects[I] := Texture;
        Texture.Index       := I;
      End
      Else
      Begin
        Textures.AddObject(FileName,Texture);
        Texture.Index := Textures.Count - 1;
      End;
    Finally
      UnlockBSPTree;
    End;
    FAddingTexture := False;

    Result := Texture;
  End
  Else Result := TTexture(Textures.Objects[I]);

//  LogToFile('TSceneGL.AddTexture(' + Name + ') End');
End; // TSceneGL.AddTexture

Function TSceneGL.AddTextureFromBitmap(FileName: String; BMP: TBitmap; NeedsMask,CheckRedraw: Boolean): TTexture;
Var
  I       : Integer;
  Texture : TTexture;

Begin
//  LogToFile('TSceneGL.AddTextureFromBitmap(' + Name + ') Begin');

  I := Textures.IndexOf(FileName);
  If I < 0 Then
  Begin
    Texture           := TTexture.Create(Self,Nil);
    Texture.NeedsMask := NeedsMask;
    Texture.LoadTextureFromBitmap(BMP);
//    Texture.Loaded    := (Texture.LoadTextureFromBitmap(BMP) >= 0);

    If CheckRedraw Then
    Begin
      FAddingTexture := True;
      While Redrawing Do Sleep(1);
    End;

    Try
      LockBSPTree('TSceneGL.AddTextureFromBitmap');

      Textures.AddObject(FileName,Texture);
      Texture.Index := Textures.Count - 1;

    Finally
      UnlockBSPTree;
    End;

    If CheckRedraw Then FAddingTexture := False;

    Result := Texture;
  End
  Else Result := TTexture(Textures.Objects[I]);

//  LogToFile('TSceneGL.AddTextureFromBitmap(' + Name + ') End');
End; // TSceneGL.AddTextureFromBitmap

Function TSceneGL.AddTextureFromRaster32(FileName: String; R32: TRaster32; NeedsMask,CheckRedraw: Boolean): TTexture;
Var
  I       : Integer;
  Texture : TTexture;

Begin
  I := Textures.IndexOf(FileName);
  If I < 0 Then
  Begin
    Texture           := TTexture.Create(Self,Nil);
    Texture.NeedsMask := NeedsMask;
    Texture.LoadTextureFromRaster32(R32);
    If CheckRedraw Then
    Begin
      FAddingTexture := True;
      While Redrawing Do Sleep(1);
    End;

    Try
      LockBSPTree('TSceneGL.AddTextureFromRaster32');

      Textures.AddObject(FileName,Texture);
      Texture.Index := Textures.Count - 1;

    Finally
      UnlockBSPTree;
    End;

    If CheckRedraw Then FAddingTexture := False;

    Result := Texture;
  End
  Else Result := TTexture(Textures.Objects[I]);
End; // TSceneGL.AddTextureFromRaster32

Function TSceneGL.AddTexturesFromBitmaps(FileNamesAndBMPs: TStringList; NeedsMask: Boolean): TStringList;
Var
  I,J     : Integer;
  Texture : TTexture;
  List    : TStringList;

Begin
//  LogToFile('TSceneGL.AddTexturesFromBitmaps(' + Name + ') Begin');

  List := TStringList.Create;
  FAddingTexture := True;
  While Redrawing Do Sleep(1);
  Try
    LockBSPTree('TSceneGL.AddTexturesFromBitmaps');
    For J := 0 To FileNamesAndBMPs.Count - 1 Do
    Begin
      I := Textures.IndexOf(FileNamesAndBMPs.Strings[J]);
      If I < 0 Then
      Begin
        Texture           := TTexture.Create(Self,Nil);
        Texture.NeedsMask := NeedsMask;
        Texture.LoadTextureFromBitmap(TBitmap(FileNamesAndBMPs.Objects[J]));
    //    Texture.Loaded    := (Texture.LoadTextureFromBitmap(BMP) >= 0);
        Textures.AddObject(FileNamesAndBMPs.Strings[J],Texture);
        Texture.Index := Textures.Count - 1;
        List.AddObject('',Texture);
      End
      Else List.AddObject('',TTexture(Textures.Objects[I]));
    End; // For J
  Finally
    UnlockBSPTree;
  End;
  FAddingTexture := False;
  Result := List;

//  LogToFile('TSceneGL.AddTexturesFromBitmaps(' + Name + ') End');
End; // TSceneGL.AddTexturesFromBitmaps

Function TSceneGL.AddTexturesFromRaster32s(FileNamesAndR32s: TStringList; NeedsMask: Boolean): TStringList;
Var
  J       : Integer;
  Texture : TTexture;
  List    : TStringList;

Begin
//  LogToFile('TSceneGL.AddTexturesFromRaster32s(' + Name + ') Begin');

  List := TStringList.Create;
  FAddingTexture := True;
  While Redrawing Do Sleep(1);
  Try
    LockBSPTree('TSceneGL.AddTexturesFromRaster32s');
    For J := 0 To FileNamesAndR32s.Count - 1 Do
    Begin
//    I := Textures.IndexOf(FileNamesAndR32s.Strings[J]);
//    If I < 0 Then
//    Begin
      Texture           := TTexture.Create(Self,Nil);
      Texture.NeedsMask := NeedsMask;
      Texture.LoadTextureFromRaster32(TRaster32(FileNamesAndR32s.Objects[J]));
  //    Texture.Loaded    := (Texture.LoadTextureFromBitmap(BMP) >= 0);
      Textures.AddObject(FileNamesAndR32s.Strings[J],Texture);
      Texture.Index := Textures.Count - 1;
      List.AddObject('',Texture);
//    End
//    Else List.AddObject('',TTexture(Textures.Objects[I]));
    End; // For J
  Finally
    UnlockBSPTree;
  End;
  FAddingTexture := False;
  Result := List;

//  LogToFile('TSceneGL.AddTexturesFromRaster32s(' + Name + ') End');
End; // TSceneGL.AddTexturesFromRaster32s

Procedure TSceneGL.FreeTextures(List: TStringList);
Begin
  RenderThread.QueueFreeTextures(List);
End; // TSceneGL.FreeTextures

Procedure TSceneGL.DoFreeTextures(List: TStringList);
// Given a list of TTexture objects, frees and removes them
Var I,J: Integer;
Begin
  FAddingTexture := True; // Serves the same purpose here
  While Redrawing Do Sleep(1);
  Try
    LockBSPTree('TSceneGL.FreeTextures');
    If @LogProc <> Nil Then LogProc('Scene (FreeTextures): Freeing ' + IntToStr(List.Count) + ' textures');
    For I := 0 To List.Count - 1 Do
    Begin
      If List.Objects[I] = Nil Then
      Begin
        If @LogProc <> Nil Then LogProc('Scene (FreeTextures): WARNING! texture ' + IntToStr(I) + ' is Nil');
      End;
      If @LogProc <> Nil Then LogProc('Scene (FreeTextures): Checking texture ' + IntToStr(I));
      J := Textures.IndexOfObject(List.Objects[I]);
      If (J >= 0) And (J < Textures.Count) Then
      Begin
        If @LogProc <> Nil Then LogProc('Scene (FreeTextures): Freeing texture ' + IntToStr(I));
        Textures.Objects[J].Free;
        If @LogProc <> Nil Then LogProc('Scene (FreeTextures): Removing texture ' + IntToStr(I));
        Textures.Delete(J);
        If @LogProc <> Nil Then LogProc('Scene (FreeTextures): Adjusting texture indices');
        While J < Textures.Count Do
        Begin
          Dec(TTexture(Textures.Objects[J]).Index);
          Inc(J);
        End; // While
      End;
    End; // For I
    If @LogProc <> Nil Then LogProc('Scene (FreeTextures): Freed textures');
  Finally
    UnlockBSPTree;
  End;
  FAddingTexture := False;
End; // TSceneGL.DoFreeTextures

Procedure TSceneGL.FreeTexture(Texture: TTexture);
Begin
  RenderThread.FreeTexture(Texture);
End; // TSceneGL.FreeTexture

Procedure TSceneGL.DoFreeTexture(Texture: TTexture);
Begin
//  LogToFile('TSceneGL.DoFreeTexture(' + Name + ') Begin');

  Texture.Free;

//  LogToFile('TSceneGL.DoFreeTexture(' + Name + ') End');
End; // TSceneGL.DoFreeTexture

Procedure TSceneGL.FreeAllTextures;
Begin
  RenderThread.FreeAllTextures;
End; // TSceneGL.FreeAllTextures

Procedure TSceneGL.DoFreeAllTextures;
Var I: Integer;
Begin
//  LogToFile('TSceneGL.DoFreeAllTextures(' + Name + ') Begin');

  For I := 0 To Textures.Count - 1 Do TTexture(Textures.Objects[I]).Free;
  Textures.Clear;

//  LogToFile('TSceneGL.DoFreeAllTextures(' + Name + ') End');
End; // TSceneGL.DoFreeAllTextures

Procedure TSceneGL.AddLight(Num: Integer);
Var Light: TLight;
Begin
  Try
    LockBSPTree('TSceneGL.AddLight');
    Light              := TLight.Create(Num);
    Light.LightType    := clStar;
    Light.CutOffAngle  := 5;
    Light.SpotExponent := 200;
    Light.SetOrientation(1,1,1);
    Light.Position.Copy(-10,0,-5);
    Lights.Add(Light);
  Finally
    UnlockBSPTree;
  End;
//  RenderThread.AddLight(Num);
End; // TSceneGL.AddLight

Procedure TSceneGL.DoAddLight(Num: Integer);
Var Light: TLight;
Begin
//  LogToFile('TSceneGL.DoAddLight(' + Name + ') Begin');

  // Create the light

  Light              := TLight.Create(Num);
  Light.LightType    := clStar;
  Light.CutOffAngle  := 5;
  Light.SpotExponent := 200;
  Light.SetOrientation(1,1,1);
  Light.Position.Copy(-10,0,-5);
  Lights.Add(Light);

//  LogToFile('TSceneGL.DoAddLight(' + Name + ') End');
End; // TSceneGL.DoAddLight

Procedure TSceneGL.SetActive(B: Boolean);
Begin
  If Active <> B Then RenderThread.SetActive(B);
End; // TSceneGL.SetActive

Procedure TSceneGL.DoSetActive(B: Boolean);
Begin
  If B Then
  Begin
    If (WindowHandle <> 0) And (DC <> 0) And (HRC <> 0) And Not Active Then
    Begin
      LogToFile('TSceneGL.DoSetActive(' + Name + '): making window active');

      // Signal that we'll try to create shaders

      FCanCreateShaders  := True;
      FCanCreatePrograms := True;

      // Make this scene active

      Active := True;

      //  Enable depth testing and back face rendering

      wglMakeCurrent(DC, HRC);
      glEnable(GL_DEPTH_TEST);
      glEnable(GL_CULL_FACE);
      glEnable(GL_LIGHTING);    // Enable lights
      LogToFile('TSceneGL.DoSetActive(' + Name + ') end');
    End;
  End
  Else
  Begin
    If Active Then
    Begin
      LogToFile('TSceneGL.DoSetActive(' + Name + '): making window inactive');

      Active := False;
      While Redrawing Do Sleep(1);
      if SceneFullscreen then             // Change back to non fullscreen
      begin
        ChangeDisplaySettings(devmode(nil^), 0);
      end;
      wglMakeCurrent(0, 0);
      FUpdatedArea := False;
      LogToFile('TSceneGL.DoSetActive(' + Name + ') end');
    End;
  End;
End; // TSceneGL.DoSetActive
//PROFILE-NO
Procedure TSceneGL.MakeImagePanel(Panel: PPanel);
Begin
  RenderThread.MakeImagePanel(Panel);
End; // TSceneGL.MakeImagePanel
//PROFILE-YES

Procedure TSceneGL.DoMakeImagePanel(Panel: PPanel);
Var
//  Owner       : TComponent;
//  Parent      : TWinControl;
//  P,P0        : TGLPanel;
//  OnMouseDown : TMouseEvent;
//  OnMouseUp   : TMouseEvent;
//  OnMouseMove : TMouseMoveEvent;
//  Color       : TColor;
//  BorderStyle : TBorderStyle;

  WinClass    : TWndClassEx;
  dwStyle     : Cardinal;
  dwExStyle   : Cardinal;
  dmScreenSettings : DEVMODE;   // Screen settings (fullscreen, etc...)


Begin
  LogToFile('TSceneGL.DoMakeImagePanel(' + Name + ') Begin');
{
  P0             := Panel^;
  P              := TGLPanel.Create(P0.Owner);
  P.Parent       := P0;
  P.Align        := alClient;
  P.OnMouseDown  := P0.OnMouseDown;
  P.OnMouseUp    := P0.OnMouseUp;
  P.OnMouseMove  := P0.OnMouseMove;
  P.Color        := P0.Color;
  P.BorderStyle  := P0.BorderStyle;
  P0.Color       := clWhite;
  P0.OnMouseDown := Nil;
  P0.OnMouseMove := Nil;
  P0.OnMouseUp   := Nil;
  Self.Panel     := P;
}
  Self.Panel := Panel^;

//  LogToFile('TSceneGL.DoMakeImagePanel(' + Name + '): window handle = ' + IntToStr(P.Handle));

{
  P := Panel^;
  Owner  := P.Owner;
  Parent := P.Parent;
  OnMouseDown := P.OnMouseDown;
  OnMouseUp   := P.OnMouseUp;
  OnMouseMove := P.OnMouseMove;
  Color       := P.Color;
  BorderStyle := P.BorderStyle;
  P.Parent := Nil;
  P.Free;
  P := TGLPanel.Create(Owner);
  P.Parent := Parent;
  P.Align := alClient;
  P.OnMouseDown := OnMouseDown;
  P.OnMouseUp   := OnMouseUp;
  P.OnMouseMove := OnMouseMove;
  P.Color       := Color;
  P.BorderStyle := BorderStyle;
  Panel^ := P;
}

//P.Visible := False;
//P.Parent.Visible := False;
//P.Parent.Parent.Visible := False;


  WinClass.lpszClassName := FWinClassName;
  WinClass.cbSize        := SizeOf(WinClass);
  WinClass.style         := CS_HREDRAW Or CS_VREDRAW Or CS_OWNDC;
  WinClass.lpfnWndProc   := @WindowProc;
  WinClass.hInstance     := hInstance;
  WinClass.hIcon         := 0;
  WinClass.hIconSm       := 0;
  WinClass.hCursor       := LoadCursor(0, IDC_ARROW);
  WinClass.hbrBackground := 0;
  WinClass.lpszMenuName  := Nil;
  WinClass.cbClsExtra    := 0;
  WinClass.cbWndExtra    := 0;

  // Change to fullscreen if so desired
  if SceneFullScreen then
  begin
    ZeroMemory(@dmScreenSettings, SizeOf(dmScreenSettings));
    with dmScreenSettings do begin              // Set parameters for the screen setting
      dmSize       := SizeOf(dmScreenSettings);
      dmPelsWidth  := SceneFullScreenWidth;                    // Window width
      dmPelsHeight := SceneFullScreenHeight;                   // Window height
      dmBitsPerPel := SceneFullScreenPixelDepth;               // Window color depth
      dmFields     := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL;
    end;

    // Try to change screen mode to fullscreen
    if (ChangeDisplaySettings(dmScreenSettings, CDS_FULLSCREEN) = DISP_CHANGE_FAILED) then
    begin
      SceneFullscreen := False;
    end;
  end;

  LogToFile('TSceneGL.DoMakeImagePanel(' + Name + '): Registering window class');

  If RegisterClassEx(WinClass) = 0 Then LogToFile('TSceneGL.DoMakeImagePanel(' + Name + '): unable to register window class')
  Else
  Begin                   
//    dwStyle := WS_CLIPCHILDREN Or WS_CLIPSIBLINGS Or WS_POPUP;
    If SceneFullScreen Then
    Begin
      dwStyle := WS_POPUP or                // Creates a popup window
                 WS_CLIPCHILDREN            // Doesn't draw within child windows
                 or WS_CLIPSIBLINGS;        // Doesn't draw within sibling windows
      dwExStyle := WS_EX_APPWINDOW;         // Top level window
    End
    Else
    Begin
      dwStyle   := WS_CLIPCHILDREN Or WS_CLIPSIBLINGS Or WS_CHILD;
      dwExStyle := 0;
    End;
    FWindowIsVisible := False;
{
    If Self.Panel.Visible And TForm(Self.Panel.Owner).Visible Then
    Begin
      dwStyle := dwStyle Or WS_VISIBLE;
      FWindowIsVisible := True;
    End
    Else FWindowIsVisible := False;
}
//    WindowHandle := CreateWindowEx(0, FWinClassName, Nil, dwStyle, 0, 0, 640, 480, 0, 0, hInstance, Nil);

    LogToFile('TSceneGL.DoMakeImagePanel(' + Name + '): Creating window');

    SceneHash.Put(Self.Panel.Handle,Self);
//    WindowHash.Put(WindowHandle,Self.Panel);

    If SceneFullScreen
     Then WindowHandle := CreateWindowEx(dwExStyle, FWinClassName, Nil, dwStyle, 0, 0, 640, 480, 0,                 0, hInstance, Nil)
     Else WindowHandle := CreateWindowEx(dwExStyle, FWinClassName, Nil, dwStyle, 0, 0, 640, 480, Self.Panel.Handle, 0, hInstance, Nil);
    If WindowHandle = 0 Then
    Begin
      LogToFile('TSceneGL.DoMakeImagePanel(' + Name + '): unable to create OpenGL window');
      SceneHash.Put(WindowHandle,Nil);
      WindowHash.Put(WindowHandle,Nil);
    End
    Else
    Begin

      LogToFile('TSceneGL.DoMakeImagePanel(' + Name + '): Window has been created');

      RenderThread.SetWindowHandle(WindowHandle);


      WindowHash.Put(WindowHandle,Self.Panel);

//      SceneHash.Put(WindowHandle,Self);

//      ShowWindow(WindowHandle, SW_HIDE);//CmdShow);
//      UpdateWindow(WindowHandle);

//      Windows.SetParent(WindowHandle,Self.Panel.Handle);
//      SetWindowPos(WindowHandle,HWND_TOP,P.Left,P.Top,P.Width,P.Height,0);


//      DoInitRC(WindowHandle);

//      RenderThread.SetWindowHandle(WindowHandle);

    End;
  End;
  LogToFile('TSceneGL.DoMakeImagePanel(' + Name + ') End');
End; // TSceneGL.DoMakeImagePanel

Procedure TSceneGL.InitDC;
Begin
  If DC = 0 Then RenderThread.InitDC;
End; // TSceneGL.InitDC

Procedure TSceneGL.DoInitDC;
Begin
  // Set the area where the OpenGL rendering will take place

//  If WindowHandle = 0 Then DoMakeImagePanel(@Panel);


//  SetCursor(LoadCursor(0, IDC_ARROW));

  LogToFile('TSceneGL.DoInitDC(' + Name + '): Begin: WindowHandle = ' + IntToStr(WindowHandle));
//  wglMakeCurrent(0, 0);
//  WindowHandle := iHandle;
  DC           := GetDC(WindowHandle);
  LogToFile('TSceneGL.DoInitDC(' + Name + '): DC = ' + IntToStr(DC));
  SetDCPixelFormat;

  LogToFile('TSceneGL.DoInitDC(' + Name + '): End');
End; // TSceneGL.DoInitDC

// Sets values for the pixel´s format.  Don´t call this Function directly, call instead InitRC.
Procedure TSceneGL.SetDCPixelFormat;
Var
  nPixelFormat : Integer;
  PFD          : TPixelFormatDescriptor;
  Code         : Integer;

Begin
  LogToFile('TSceneGL.SetDCPixelFormat(' + Name + '): Begin, WindowHandle = ' + IntToStr(WindowHandle));
  FillChar(PFD, SizeOf(PFD),0);
  With PFD Do
  Begin
    nSize      := SizeOf(PFD);                               // Size of this structure
    nVersion   := 1;                                         // Version number
    dwFlags    := PFD_DRAW_TO_WINDOW Or
                  PFD_SUPPORT_OPENGL Or
                  PFD_DOUBLEBUFFER;                          // Flags
    iPixelType := PFD_TYPE_RGBA;                             // RGBA pixel values
    cColorBits := 24;                                        // 24-bit color
    cDepthBits := 32;                                        // 32-bit depth buffer


//    cAlphaBits := 8;
//    cStencilBits := 8;


    iLayerType := PFD_MAIN_PLANE;                            // Layer type
  End;
  LogToFile('TSceneGL.SetDCPixelFormat(' + Name + '): Calling ChoosePixelFormat()');
  nPixelFormat := ChoosePixelFormat(DC, @PFD);
  If nPixelFormat = 0
   Then LogToFile('TSceneGL.SetDCPixelFormat(' + Name + '): error in ChoosePixelFormat, code = ' + IntToStr(GetLastError))
   Else LogToFile('TSceneGL.SetDCPixelFormat(' + Name + '): nPixelFormat = ' + IntToStr(nPixelFormat));
  If Not SetPixelFormat(DC, nPixelFormat, @PFD) Then
  Begin
    Code := GetLastError;
    LogToFile('TSceneGL.SetDCPixelFormat(' + Name + '): error in SetPixelFormat(DC=' + IntToStr(DC) + '), code = ' + IntToStr(Code));
  End;
  If Not DescribePixelFormat(DC, nPixelFormat, SizeOf(TPixelFormatDescriptor), PFD) Then
   LogToFile('TSceneGL.SetDCPixelFormat(' + Name + '): error in DescribePixelFormat, code = ' + IntToStr(GetLastError));
  LogToFile('TSceneGL.SetDCPixelFormat(' + Name + '): End');
End; // TSceneGL.SetDCPixelFormat

// Initializes the rendering context, receives the handle of the display control, frequently it is a TPanel
Procedure TSceneGL.InitRC;
Begin
  If HRC = 0 Then RenderThread.InitRC;
End; // TSceneGL.InitRC

Procedure TSceneGL.DoInitRC;
Begin
  // Set the area where the OpenGL rendering will take place

//  If WindowHandle = 0 Then DoMakeImagePanel(@Panel);

{
  SetCursor(LoadCursor(0, IDC_ARROW));

  LogToFile('TSceneGL.DoInitRC(' + Name + ') begin');
//  wglMakeCurrent(0, 0);
//  WindowHandle := iHandle;
  DC           := GetDC(WindowHandle);
  LogToFile('TSceneGL.DoInitRC(' + Name + '): DC = ' + IntToStr(DC));
  SetDCPixelFormat;
}  
  HRC          := wglCreateContext(DC);

  If HRC = 0
   Then LogToFile('TSceneGL.DoInitRC(' + Name + '): Unable to create OpenGL rendering context. Error code = ' + IntToStr(glGetError))
   Else LogToFile('TSceneGL.DoInitRC(' + Name + '): HRC = ' + IntToStr(HRC));

  If (DC <> 0) And (HRC <> 0) Then
  Begin
    ActivateRenderingContext(DC,HRC);
    LogGLError('DoInitRC wglMakeCurrent');
  End;
{
  ARBOcclusion := (@glGenQueriesARB        <> Nil) And
                  (@glDeleteQueriesARB     <> Nil) And
                  (@glIsQueryARB           <> Nil) And
                  (@glBeginQueryARB        <> Nil) And
                  (@glEndQueryARB          <> Nil) And
                  (@glGetQueryivARB        <> Nil) And
                  (@glGetQueryObjectivARB  <> Nil) And
                  (@glGetQueryObjectuivARB <> Nil);
}
  LogToFile('TSceneGL.DoInitRC(' + Name + '): Creating SampleText');

  SampleText := T3DText.Create(Self, DC, 1024);//1);
  SampleText.FontScale[1] := TScene3D(Owner).FontScale;
  SampleText.FontScale[2] := TScene3D(Owner).FontScale;
  SampleText.FontScale[3] := TScene3D(Owner).FontScale;
  SampleText.ID := 1;

//  wglMakeCurrent(0, 0);

  //  Enable depth testing and back face rendering

//  glEnable(GL_DEPTH_TEST);
//  glEnable(GL_CULL_FACE);
//  glEnable(GL_LIGHTING);    // Enable lights

//  Active := True;

  LogToFile('TSceneGL.DoInitRC(' + Name + ') end');
End; // TSceneGL.DoInitRC

// Frees all resources taken by InitRC.  Don´t call this Function directly.
Procedure TSceneGL.ReleaseRC;
Begin
  If WindowHandle <> 0 Then RenderThread.ReleaseRC;
End; // TSceneGL.ReleaseRC

Procedure TSceneGL.DoReleaseRC;
Begin
  LogToFile('TSceneGL.DoReleaseRC() begin');
{
  SampleText.Free;
  SampleText := Nil;
  wglMakeCurrent(0, 0);
  wglDeleteContext(HRC);
  ReleaseDC(WindowHandle, DC);
  DC  := 0;
  HRC := 0;
  WindowHandle := 0;
  Active := False;
}
  Windows.UnregisterClass(FWinClassName, hInstance);

  LogToFile('TSceneGL.DoReleaseRC() end');
End; // TSceneGL.DoReleaseRC

Procedure TSceneGL.LoadTexture(Texture: TTexture);
Begin
  RenderThread.LoadTexture(Texture);
End; // TSceneGL.LoadTexture

Procedure TSceneGL.DoLoadTexture(Texture: TTexture);
Begin
  LogToFile('TSceneGL.DoLoadTexture(' + Name + ') Begin');

  If (DC <> 0) And (HRC <> 0) Then wglMakeCurrent(DC, HRC);
  Texture.LoadTextureIntoOpenGL;

  LogToFile('TSceneGL.DoLoadTexture(' + Name + ') End');
End; // TSceneGL.LoadTexture

// Reflects changes in the width and height (ancho,alto) of the display control
Procedure TSceneGL.UpdateArea(Left,Top,Width,Height: Integer);
Var
  Vis      : Boolean;
  TheOwner : TComponent;

Begin
  If SceneFullScreen Then
  Begin
    Left   := 0;
    Top    := 0;
    Width  := SceneFullScreenWidth;
    Height := SceneFullScreenHeight;
  End;

//  LogToFile('TSceneGL.UpdateArea(' + Name + ') Begin: (' + IntToStr(Left) + ', ' + IntToStr(Top) + '), (' + IntToStr(Width) + ' x ' + IntToStr(Height) + ')');

  If ((Panel <> Nil) Or SceneFullScreen) And (DC <> 0) And Not Redrawing Then
  Begin
//    LogToFile('TSceneGL.UpdateArea(' + Name + '): Not redrawing');

    TheOwner := GetUltimateOwner(Panel);

//    LogToFile('TSceneGL.UpdateArea(' + Name + '): TheOwner = $' + IntToHex(Integer(TheOwner),8));

    Vis      := SceneFullScreen Or (Panel.Visible And TForm(TheOwner).Visible And (TForm(TheOwner).Showing) And (TForm(TheOwner).WindowState <> wsMinimized));

//    LogToFile('TSceneGL.UpdateArea(' + Name + '): Vis = ' + BoolToStr(Vis,True));

    If (WindowWidth <> Width) Or (WindowHeight <> Height) Or (WindowLeft <> Left) Or (WindowTop <> Top) Or (Vis <> FWindowIsVisible) Then
    Begin
//      LogToFile('TSceneGL.UpdateArea(' + Name + '): State changed: updating area');

      WindowLeft   := Left;
      WindowTop    := Top;
      WindowWidth  := Width;
      WindowHeight := Height;
      RenderThread.UpdateArea(Left,Top,Width,Height);
    End;
  End;

//  LogToFile('TSceneGL.UpdateArea(' + Name + ') End');
End; // TSceneGL.UpdateArea

Procedure TSceneGL.DoUpdateArea(Left,Top,Width,Height: Integer);
Var
  Ratio,Range : GLFloat;
  I           : Integer;
  Vis         : Boolean;
//  X,Y         : Integer;
  TheOwner    : TComponent;
//  ActiveHWND  : THandle;

Begin
  // Redefine the visible volume and the viewport when the window´s size is modified

  If Active And (DC <> 0) And (HRC <> 0) Then
  Begin
    LogToFile('TSceneGL.DoUpdateArea(' + Name + ') Begin');

    If WindowHandle <> 0 Then
    Begin
      TheOwner := GetUltimateOwner(Panel);
      Vis := SceneFullScreen Or (Panel.Visible And TForm(TheOwner).Visible And (TForm(TheOwner).WindowState <> wsMinimized) And TForm(TheOwner).Showing);
      If Vis <> FWindowIsVisible Then
      Begin
        LogToFile('TSceneGL.DoUpdateArea(' + Name + '): Window visibility has changed...visibility is now ' + BoolToStr(Vis,True));
        If Vis
         Then ShowWindow(WindowHandle,SW_SHOW)
         Else ShowWindow(WindowHandle,SW_HIDE);
        LogToFile('TSceneGL.DoUpdateArea(' + Name + '): Finished calling ShowWindow()');
        FWindowIsVisible := Vis;
      End;
      If FWindowIsVisible Then
      Begin
//        LogToFile('TSceneGL.DoUpdateArea(' + Name + '): Calculating panel''s absolute position');
//        GetAbsolutePosition(Panel,X,Y,True);
//        LogToFile('TSceneGL.DoUpdateArea(' + Name + '): Setting window position to ' + IntToStr(X) + ', ' + IntToStr(Y));

        If Not SetWindowPos(WindowHandle,0,Left,Top,Width,Height,SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOZORDER) Then
         LogToFile('TSceneGL.DoUpdateArea(' + IntToStr(WindowHandle) + '): Error = $' + IntToHex(GetLastError,8));

//        LogToFile('TSceneGL.DoUpdateArea(' + Name + '): Setting window position to ' + IntToStr(Left) + ', ' + IntToStr(Top));
{
        ActiveHWND := GetActiveWindow;
        If ActiveHWND <> FLastActiveWindow Then
        Begin
          SetWindowPos(WindowHandle,HWND_TOPMOST,Left,Top,Width,Height,0);

          SetWindowPos(WindowHandle,HWND_NOTOPMOST,0,0,0,0,SWP_NOMOVE Or SWP_NOSIZE);
          SetWindowPos(WindowHandle,Panel.Handle,0,0,0,0,SWP_NOMOVE Or SWP_NOSIZE);

          SetWindowPos(ActiveHWND,HWND_TOP,0,0,0,0,SWP_NOMOVE Or SWP_NOSIZE);
          FLastActiveWindow := ActiveHWND;
        End
        Else
        Begin
//          SetWindowPos(WindowHandle,Panel.Handle,Left,Top,Width,Height,0);
          SetWindowPos(WindowHandle,HWND_BOTTOM,Left,Top,Width,Height,0);
          SetWindowPos(WindowHandle,Panel.Handle,0,0,0,0,SWP_NOMOVE Or SWP_NOSIZE);
        End;
}
      End;
    End;
//    LogToFile('TSceneGL.DoUpdateArea(' + Name + '): Calling wglMakeCurrent()');
    wglMakeCurrent(DC, HRC);
//    LogGLError('DoUpdateArea wglMakeCurrent');
    For I := 0 To Cameras.Count - 1 Do TCamera(Cameras.Items[I]).Frustum.SetWidthAndHeight(Width,Height);
    glMatrixMode(GL_PROJECTION);
//    LogGLError('DoUpdateArea glMatrixMode #1');
    glLoadIdentity;
//    LogGLError('DoUpdateArea glLoadIdentity');
    If FPerspective Then
    Begin
      FAspect := Width / Height;
      gluPerspective(Angle,          // Field-of-view angle
                     FAspect,       // Aspect ratio of viewing volume
                     DistNear,       // Distance to near clipping plane
                     DistFar);       // Distance to far clipping plane
//      LogGLError('DoUpdateArea gluPerspective');
    End
    Else
    Begin
      // Orthogonal projection

      Range := 12;
      If Width <= Height Then
      Begin
        Ratio := Height / Width;
        GlOrtho(-Range,Range,-Range * Ratio,Range * Ratio,-Range * 4,Range * 4);
      End
      Else
      Begin
        Ratio := Width / Height;
        GlOrtho(-Range * Ratio,Range * Ratio,-Range,Range,-Range * 4,Range * 4);
      End;
    End;
    FOcclusionManager.Setup(Width, Height);
    glViewport(0, 0, Width, Height);
//    LogGLError('DoUpdateArea glViewport');
    glMatrixMode(GL_MODELVIEW);  {reset translation and rotation values}
//    LogGLError('DoUpdateArea glMatrixMode #2');
    FUpdatedArea := True;
    LogToFile('TSceneGL.DoUpdateArea(' + Name + ') End');
  End;
End; // TSceneGL.DoUpdateArea

Procedure TSceneGL.DoOcclusion;
Var
  I      : Integer;
  Entity : TEntity;

  Procedure Sort(Index0,Index1: Integer);
  Var Pivot: Integer;

    Function Partition(Index0,Index1: Integer): Integer;
    Var
      I,J : Integer;
      D0  : Single;
      D1  : Single;

    Begin
      I := Index0 - 1;
      J := Index1 + 1;
      D1 := TEntity(RedrawQueue.Items[Index0]).DistanceFromCamera2;
      While True Do
      Begin
        Repeat
          Dec(J);
          D0 := TEntity(RedrawQueue.Items[J]).DistanceFromCamera2;
        Until D1 >= D0;//FCompare(J,Index0) <= 0;
        Repeat
          Inc(I);
          D0 := TEntity(RedrawQueue.Items[I]).DistanceFromCamera2;
        Until D1 <= D0;//FCompare(I,Index0) >= 0;
        If I < J Then RedrawQueue.Exchange(I,J)//FExchange(I,J)
        Else
        Begin
          Partition := J;
          Exit;
        End;
      End; // While
    End; // Partition

  Begin
    If Index0 < Index1 Then
    Begin
      Pivot := Partition(Index0,Index1);
      Sort(Index0,Pivot);
      Sort(Pivot + 1,Index1);
    End;
  End; // Sort

Begin
    Try
      LockBSPTree('TSceneGL.DoOcclusion');
      Try
        Renderables.FMutex.Enter;
        Try
          Entities.FMutex.Enter;

          // First update all entities from last frame

          For I := 0 To Entities.Count - 1 Do TEntity(Entities.FItems[I]).Update;

          // Find out what BSP node contains the camera

          If ActiveCamera.Position.Dirty Then
          Begin
            ActiveCamera.Position.Dirty := False;
            FCameraNode := FBSPTreeRoot.GetNodeContainingPoint(ActiveCamera.Position);
          End;

          // Mark all entities as not visible

          For I := 0 To Entities.Count - 1 Do TEntity(Entities.Items[I]).VisibleInFrustum := False;

          // Determine which entities need to be redrawn (which will mark them as visible in the viewing frustum)

          RedrawQueue.Clear;
          If FCameraNode <> Nil Then FCameraNode.RedrawUp(False,FCameraNode);

          // Sort the entity list so the closest ones are drawn first

          Sort(0,RedrawQueue.Count - 1);

          // Continue with occlusion checking only if the manager is enabled

          If FOcclusionManager.Enabled Then
          Begin
            FOcclusionManager.ClearZBuffer;
            FOcclusionManager.LoadIdentity;

            // Fill in the occlusion buffer

            I := 0;
            While I < RedrawQueue.Count Do
            Begin
              Entity := TEntity(RedrawQueue.Items[I]);

              // Only draw the entity if either the occlusion manager is disabled or the entity isn't occluded
              // (or if it isn't time to check it again because it wasn't occluded last time)

              Dec(Entity.OccludeCounter);
              If Entity.OccludeCounter < 0 Then
              Begin
                If Not OcclusionManager.IsOccluded(Entity.Box) Then
                Begin
                  Entity.OccludeCounter := OccludeSkipFrames;
                  Entity.CheckOcclusion;
                End
                Else
                Begin
                  // If the entity is occluded, mark it as not visible

                  Entity.OccludeCounter      := 0;
                  Entity.VisibleInFrustum    := False;
                  Entity.OldVisibleInFrustum := False;
                End;
              End
              Else Entity.CheckOcclusion;
              Inc(I);
            End; // While
          End;
        Finally
          Entities.FMutex.Leave;
        End;
      Finally
        Renderables.FMutex.Leave;
      End;
    Finally
      UnlockBSPTree;
    End;  
End; // TSceneGL.DoOcclusion

Procedure TSceneGL.CalculateActualDynamicLights(Time: Integer);
Var
  I     : Integer;
  Light : TDynamicLight;

Begin
  If FShaderManager.IsAvailable And ShaderManager.Enabled Then
  Begin
    Try
      DynamicLights.Lock;
      For I := 0 To DynamicLights.FMaxIndex Do
      Begin
        Light := TDynamicLight(DynamicLights.FItems[I]);
        Light.CalculateActualLight(Time);
      End; // For I
    Finally
      DynamicLights.Unlock;
    End;
  End;
End; // TSceneGL.CalculateActualDynamicLights

Procedure TSceneGL.SetShaderAmbientLight;
// Updates the shader parameters
Var
  IUniform : Integer;
  St       : String;
  NUniform : Packed Array [0..31] Of Char;
  I        : Integer;
  Light    : TLight;
  AmbientR : Single;
  AmbientG : Single;
  AmbientB : Single;

Begin
  If FShaderManager.IsAvailable And ShaderManager.Enabled Then
  Begin
    // Calculate the ambient light from the light sources

    AmbientR := 0;
    AmbientG := 0;
    AmbientB := 0;
    For I := 0 To Lights.Count - 1 Do
    Begin
      Light := TLight(Lights.Items[I]);
      If (Light <> Nil) And (Light.Attenuation = 0) Then
      Begin
        AmbientR := AmbientR + Light.FAmbient[0];
        AmbientG := AmbientG + Light.FAmbient[1];
        AmbientB := AmbientB + Light.FAmbient[2];
      End;
    End; // For I

    St       := 'AmbientLight';
    StrPCopy(NUniform,St);
    IUniform := glGetUniformLocationARB(ShaderManager.CurrentProgramHandle, @NUniform[0]);
    If (IUniform <> GL_INVALID_VALUE) And (IUniform <> GL_INVALID_OPERATION) And (IUniform <> -1) Then glUniform3fARB(IUniform,AmbientR,AmbientG,AmbientB);
  End;
End; // TSceneGL.SetShaderAmbientLight

Procedure TSceneGL.RegisterMaterialPrograms;
Var
  I        : Integer;
  Material : TMaterial;

Begin
  If FShaderManager.IsAvailable Then
  Begin
    Try
      Materials.Lock;
      For I := 0 To Materials.FMaxIndex Do
      Begin
        Material := TMaterial(Materials.FItems[I]);
        ShaderManager.RegisterProgram(Material.VertexShader,Material.FragmentShader);
      End; // For I
    Finally
      Materials.Unlock;
    End;
  End;
End; // TSceneGL.RegisterMaterialPrograms

Function TSceneGL.RedrawQueued: Boolean;
Begin
  Result := RenderThread.ContainsMessage(rmRedraw);
End; // TSceneGL.RedrawQueued

// Redraws the scene
Function TSceneGL.Redraw: Boolean;
//Var PS: TPaintStruct;
Begin
  Result := False;
//  LogToFile('TSceneGL.Redraw(' + Name + ') Begin');
//FWaitMutex.Enter;
//  If Not Redrawing Then
  Begin
//    LogToFile('TSceneGL.Redraw(' + Name + '): Not redrawing');

    If Active And (WindowHandle <> 0) Then
    Begin
//      LogToFile('TSceneGL.Redraw(' + Name + '): Calling DoRedraw through the rendering thread');

      If Not RenderThread.ContainsMessage(rmRedraw) Then
      Begin
        RenderThread.Redraw;
        Result := True;
{
        Try
          FRepaintMonitor.Enter;
          FRepaintMonitor.Wait;
        Finally
          FRepaintMonitor.Leave;
        End;
}        
      End;
     
//      BeginPaint(WindowHandle, PS); // Can only do this from the VCL thread
//      EndPaint(WindowHandle, PS);
    End;
  End;
//FWaitMutex.Leave;
//  LogToFile('TSceneGL.Redraw(' + Name + ') End');
End; // TSceneGL.Redraw

//PROFILE-NO
Procedure TSceneGL.DoRedraw;
Const
  glfMaterialColor : Array[0..3] Of GLFloat = (0.5, 1.0, 0.5, 1.0);
  AmbientLight     : Array[0..3] Of GLFloat = (0.0, 0.0, 0.0, 0.0);

Var
  I             : Integer;
//  PS         : TPaintStruct;
//  PP         : Pointer;
  bDepthTest    : Boolean;
  PS            : TPaintStruct;
  DoSwapBuffers : Boolean;
  OThread       : TOcclusionThread;

  Procedure Redraw_No_ARB_Occlusion;
  Var
    I           : Integer;
    Entity      : TEntity;
{
    Procedure Sort(Index0,Index1: Integer);
    Var Pivot: Integer;

      Function Partition(Index0,Index1: Integer): Integer;
      Var
        I,J : Integer;
        D0  : Single;
        D1  : Single;

      Begin
        I := Index0 - 1;
        J := Index1 + 1;
        D1 := TEntity(RedrawQueue.Items[Index0]).DistanceFromCamera2;
        While True Do
        Begin
          Repeat
            Dec(J);
            D0 := TEntity(RedrawQueue.Items[J]).DistanceFromCamera2;
          Until D1 >= D0;//FCompare(J,Index0) <= 0;
          Repeat
            Inc(I);
            D0 := TEntity(RedrawQueue.Items[I]).DistanceFromCamera2;
          Until D1 <= D0;//FCompare(I,Index0) >= 0;
          If I < J Then RedrawQueue.Exchange(I,J)//FExchange(I,J)
          Else
          Begin
            Partition := J;
            Exit;
          End;
        End; // While
      End; // Partition

    Begin
      If Index0 < Index1 Then
      Begin
        Pivot := Partition(Index0,Index1);
        Sort(Index0,Pivot);
        Sort(Pivot + 1,Index1);
      End;
    End; // Sort
}
  Begin
    SetVSync(VSyncOn);

    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @AmbientLight);
    glLoadIdentity;

    // Set the default shaders (won't do anything if shaders aren't supported)

    LoadDefaultShaders;

    // Set camera viewpoint

    ActiveCamera.Redraw(True);
{
    // Place lights

    For I := 0 To Lights.Count - 1 Do TLight(Lights.Items[I]).Redraw;

    // Disable all other lights

    For I := Lights.Count To MaxLights - 1 Do glDisable(I + GL_LIGHT0);
}
    // Clear depth and color buffers

    glClearColor(BackR,BackG,BackB,1);                   // Specify background color
    Try
      glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT); // Clear the rendering area
    Except
      LogToFile('TSceneGL.DoRedraw(' + Name + '): Exception calling glClear()');
    End;

    // If translucent mode - ignore depth buffer

    glGetBooleanv(GL_DEPTH_TEST,@bDepthTest);

    glEnable(GL_DEPTH_TEST);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);

{
    If bTranslucentMode Then
    Begin
//      if (bDepthTest) then glDisable(GL_DEPTH_TEST);      // Original
      If Not bDepthTest Then glEnable(GL_DEPTH_TEST);

//      glBlendFunc(GL_SRC_ALPHA,GL_ONE); // Original
      glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); // OpenZone
//      glBlendFunc(GL_ONE,GL_ONE); // Blend *everything*

      glEnable(GL_BLEND);               // Original


//      glDepthMask (GL_FALSE);
    End
    Else
    Begin
      If Not bDepthTest Then glEnable(GL_DEPTH_TEST);
      glDisable(GL_BLEND);
//      glDepthMask (GL_TRUE);

    End;
}

    glEnable(GL_ALPHA_TEST);
//    glDisable(GL_ALPHA_TEST);
{
    glAlphaFunc(GL_GREATER, 0.5);
    glEnable(GL_ALPHA_TEST);
}
{
    glAlphaFunc(GL_NOTEQUAL, 0);
    glEnable(GL_ALPHA_TEST);
}
//    glEnable(GL_ALPHA_TEST);
//    glEnable(GL_BLEND);
//    glEnable(GL_RGBA_MODE);
//    glBlendFunc(GL_SRC_ALPHA,GL_ONE);

    glEnable(GL_COLOR_MATERIAL);  {tell OpenGL to pay attention to GlColor orders}

    // Fog

    If FogEnabled Then
    Begin
      glEnable(GL_fog);

      {too slow: glhint(gl_fog_hint, gl_nicest);}  // good fog, but a little slow

      glfogi(gl_fog_mode, FogType);
      glfogfv(gl_fog_color, @FogColor);

      // Density doesnt work with linear fog

{      If FogType <> GL_LINEAR then }glfogf(gl_fog_density, FogDensity);
      glfogf(GL_fog_start, FogMinDist);
      glfogf(GL_fog_end, FogMaxDist);
    End
    Else
    Begin
      glfogf(gl_fog_density, 0);
      glfogf(GL_fog_start, 10000);
      glfogf(GL_fog_end, 20000);
      glDisable(GL_fog);
    End;

    // Textures

    If Texturing Then
    Begin
      glDisable(GL_texture_1d);
      glEnable(gl_texture_2d);
    End
    Else
    Begin
      glDisable(GL_texture_1d);
      glDisable(gl_texture_2d);
    End;

    // Names
{
    If PutNames Then
    Begin
      glInitNames;      // Init the name stack, not necessary If your objects aren´t named
      glPushName(0);    // Init the name stack
    End;
}
    // Redraw all the textures

//    For I := 0 To Textures.Count - 1 Do TTexture(Textures.Objects[I]).Redraw;

    // Start at a known texturing state

    glDisable(GL_TEXTURE_2D);
    Tex2D := False;

    // Sky spheres and star fields should always be rendered

    glDepthMask(TRUE);
    glAlphaFunc(GL_ALWAYS, 0);

    // Start with default color settings

    glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
    glColor4ub(255,255,255,255);

    // Draw objects in the sky

    If Not FSkySphere1.UseAlpha Then FSkySphere1.Redraw;
    If Not FSkySphere.UseAlpha Then FSkySphere.Redraw;
    FStarField.Redraw;
    FSun.Redraw;
    FMoon.Redraw;

    // If the sky sphere uses alpha blending, assume that it's being used for clouds

//    glDepthMask(FALSE);
//glDisable(GL_ALPHA_TEST);
    If FSkySphere1.UseAlpha Then FSkySphere1.Redraw;
    If FSkySphere.UseAlpha Then FSkySphere.Redraw;
//glEnable(GL_ALPHA_TEST);
//    glDepthMask(TRUE);

    // To properly render transparency we need to render all of our entities twice:
    // first only where the alpha is 1 and then when the alpha isn't 1

    glAlphaFunc(GL_EQUAL, 1);
{
    If FOcclusionManager.Enabled Then
    Begin
      FOcclusionManager.ClearZBuffer;
      FOcclusionManager.LoadIdentity;
    End;
}
    If MaxLights < 0 Then
    Begin
      glGetIntegerv(GL_MAX_LIGHTS,@MaxLights);
      LogGLError('Attempting to determine the maximum number of lights');
    End;

    // Place lights

    For I := 0 To Min(MaxLights,Lights.Count) - 1 Do TLight(Lights.Items[I]).Redraw;

    // Disable all other lights

    For I := Lights.Count To MaxLights - 1 Do glDisable(I + GL_LIGHT0);
(*!!
    // Find the BSP node that contains the camera

    If ActiveCamera.Position.Dirty Then
    Begin
{
      ActiveCamera.Entity.Position.Copy(ActiveCamera.Position);
      ActiveCamera.Entity.Sphere.Copy(ActiveCamera.Frustum.Sphere);
      ActiveCamera.Entity.Sphere.Radius := 0;
      FBSPTreeRoot.CheckEntity(ActiveCamera.Entity);
}
      ActiveCamera.Position.Dirty := False;
{
      If ActiveCamera.Entity.BSPTreeNode = Nil Then FBSPTreeRoot.CheckEntity(ActiveCamera.Entity)
      Else
      Begin
        FCameraNode := ActiveCamera.Entity.BSPTreeNode.ContainsEntityWithParents(ActiveCamera.Entity);
        FCameraNode.CheckEntity(ActiveCamera.Entity);
      End;
}
//      If ActiveCamera.Entity.BSPTreeNode <> Nil Then ActiveCamera.Entity.BSPTreeNode.RemoveEntity(Entity);


      FCameraNode := FBSPTreeRoot.GetNodeContainingPoint(ActiveCamera.Position);

    End;
*)    
(*!!
    // Mark all entities as not visible

    For I := 0 To Entities.Count - 1 Do TEntity(Entities.Items[I]).VisibleInFrustum := False;

    // Determine which entities need to be redrawn (which will mark them as visible in the viewing frustum)

    RedrawQueue.Clear;
    If {ActiveCamera.Entity.BSPTreeNode}FCameraNode <> Nil Then {ActiveCamera.Entity.BSPTreeNode}FCameraNode.RedrawUp(False,FCameraNode{ActiveCamera.Entity.BSPTreeNode});

    // Sort the entity list so the closest ones are drawn first

    Sort(0,RedrawQueue.Count - 1);

    // Update those entities that are visible

//    For I := 0 To Entities.Count - 1 Do TEntity(Entities.FItems[I]).Update;
*)
    // Draw visible entities, first only accepting fragments where the alpha is 1

//    RedrawPossible := RedrawQueue.Count;
//    Redrawn        := 0;
(*!!
    I := 0;
    While I < RedrawQueue.Count Do
    Begin
      Entity := TEntity(RedrawQueue.Items[I]);

      // Only draw the entity if either the occlusion manager is disabled or the entity isn't occluded
      // (or if it isn't time to check it again because it wasn't occluded last time)

      Dec(Entity.OccludeCounter);
      If Entity.OccludeCounter < 0 Then
      Begin
        If Not (OcclusionManager.Enabled And OcclusionManager.IsOccluded(Entity.Box)) Then
        Begin
          Entity.OccludeCounter := OccludeSkipFrames;
          Entity.Redraw(False);

//          Inc(Redrawn);

        End
        Else
        Begin
          // If the entity is occluded, mark it as not visible

          Entity.OccludeCounter      := 0;
          Entity.VisibleInFrustum    := False;
          Entity.OldVisibleInFrustum := False;
        End;
      End
      Else
      Begin
        Entity.Redraw(False);

//        Inc(Redrawn);

      End;
      Inc(I);
    End; // While
*)
    For I := 0 To RedrawQueue.Count - 1 Do
    Begin
      Entity := TEntity(RedrawQueue.Items[I]);
      If Entity.VisibleInFrustum Then Entity.Redraw(False);
    End; // For I

    // Now we need to disable writing to the depth buffer (but still perform depth testing)

    glDepthMask(FALSE);
    glAlphaFunc(GL_NOTEQUAL, 1);

    // Start with default color settings

    glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
    glColor4ub(255,255,255,255);

    // Draw entities again, now only accepting fragments where the alpha isn't 0 or 1

    For I := 0 To RedrawQueue.Count - 1 Do
    Begin
      Entity := TEntity(RedrawQueue.Items[I]);
      If Entity.HasAlpha And Entity.VisibleInFrustum Then TEntity(RedrawQueue.Items[I]).Redraw(True);
    End; // For I

    // Go back to the default color settings

      glColorMaterial(GL_FRONT,GL_EMISSION);
      glColor4ub(0,0,0,0);     // No emission
    glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
    glColor4ub(255,255,255,255);




    // Precipitation (rain/snow)

    FPrecipitation.Redraw;



    // User interface

    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glAlphaFunc(GL_ALWAYS,0);

//    UI.Redraw(False);

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);

    // We're done: re-enable writing to the depth buffer

    glDepthMask(TRUE);



    glDisable(GL_LIGHTING);
    If Assigned(FOnUIRepaint) Then FOnUIRepaint(Self);
//    SkinUI.Repaint;
    glEnable(GL_LIGHTING);
  End; // Redraw_No_ARB_Occlusion

Begin
//  Try
  //  FWaitMutex.Enter;
    DidRedraw     := False;
    DidRepaint    := False;
    DoSwapBuffers := False;
    Sleep(1);
    If Active And (WindowHandle <> 0) And (DC <> 0) And (HRC <> 0) then
    Begin
      While FAddingTexture Do Sleep(1);
      Try
        LockBSPTree('TSceneGL.DoRedraw');
        Try
          Renderables.FMutex.Enter;
          Try
            Entities.FMutex.Enter;

            // Pulse threads waiting on the monitor so they can attempt to lock the BSP tree.
            // This way they'll run as soon as we're done drawing.

            Try
              FRepaintMonitor.Enter;
              FRepaintMonitor.PulseAll;
            Finally
              FRepaintMonitor.Leave;
            End;

            // Initialization

            Redrawing := True;

            // Register the shader programs for any materials

            RegisterMaterialPrograms;

            // Create shaders and programs if necessary

            FCanCreateShaders  := FCanCreateShaders And FShaderManager.CreateShaders;
            Try
              FCanCreatePrograms := FCanCreateShaders And FCanCreatePrograms And FShaderManager.CreatePrograms;
            Except
              FCanCreatePrograms := False;
            End;

            // Set the shader ambient light level based on light objects

            SetShaderAmbientLight;

            // Calculate actual dynamic light levels (they might flicker)

            CalculateActualDynamicLights(GetTickCount);

            // Perform occlusion culling

            DoOcclusion;

  //          Try
  //            RedrawMutex.Enter;

              SetVSync(VSyncOn);
              wglMakeCurrent(DC, HRC);
    //      PP := MaskExceptions;


              // Reset the time counter

              RedrawTime := GetTickCount;

              // First update all entities from last frame

  //            For I := 0 To Entities.Count - 1 Do TEntity(Entities.FItems[I]).Update;

              // Redraw the scene

              Redraw_No_ARB_Occlusion;


  //          Finally
  //            RedrawMutex.Leave;
  //          End;
          Finally
            Entities.FMutex.Leave;
          End;
        Finally
          Renderables.FMutex.Leave;
        End;
      Finally
        UnlockBSPTree;

        DidRepaint := True;

        DoSwapBuffers := True;
  (*
              // Swap the rendering buffers so there is no flickering
              SwapBuffers(DC);
        //      UnmaskExceptions(PP);

              BeginPaint(WindowHandle, PS); // Can only do this from the thread that created the window
              EndPaint(WindowHandle, PS);

        DidRedraw := True;  // Must do this first
        Redrawing := False;
  *)
      End;
    End
    Else
    Begin
      Try
        FRepaintMonitor.Enter;
        FRepaintMonitor.PulseAll;
      Finally
        FRepaintMonitor.Leave;
      End;
    End;

  //  FWaitMutex.Leave;


    If DoSwapBuffers Then
    Begin
  //    OThread := TOcclusionThread.Create(Self);
  //    OThread.Resume;


              // Swap the rendering buffers so there is no flickering
  //PROFILE-BEGIN
              SetVSync(VSyncOn);
              SwapBuffers(DC);
  //PROFILE-END
        //      UnmaskExceptions(PP);

              BeginPaint(WindowHandle, PS); // Can only do this from the thread that created the window
              EndPaint(WindowHandle, PS);

        Inc(FFrameCount);

        DidRedraw := True;  // Must do this first
        Redrawing := False;

  //      OThread.WaitFor;
  //      OThread.Free;


    End;

    DidRepaint := True;
    DidRedraw  := True;
  //  FWaitMutex.Leave;
//  Except
//    On E: Exception Do Raise Exception.Create(E.Message + ' (TSceneGL.DoRedraw)' );
//  End;
End; // TSceneGL.DoRedraw
//PROFILE-YES

// Sets the perspective values for Field-of-view angle, distance to near clipping plane, distance to far clipping plane
Procedure TSceneGL.SetPerspective(iAngle,iDistNear,iDistFar: Single);
Begin
  Angle    := iAngle;
  DistNear := iDistNear;
  DistFar  := iDistFar;
End; // TSceneGL.SetPerspective

// Activates a different camera, by number of camera
Procedure TSceneGL.UseCamera(CameraNumber: Integer);
Begin
  If (CameraNumber >= 1) And (CameraNumber <= Cameras.Count) Then ActiveCamera := Cameras.Items[CameraNumber - 1];
End; // TSceneGL.UseCamera

Function TSceneGL.CreateEntity(Option: Integer): TObject;
Begin
  Try
    LockBSPTree('TSceneGL.CreateEntity');
    Result := TEntity.Create(Self);
    FBSPTreeRoot.AddEntity(TEntity(Result));
  Finally
    UnlockBSPTree;
  End;
End; // TSceneGL.CreateEntity

Function TSceneGL.CreateRenderable(Option: Integer): TObject;
Begin
  If Option = 1
   Then Result := TSkeletonRenderable.Create
   Else Result := TRenderable.Create;
End; // TSceneGL.CreateRenderable

Function TSceneGL.CreateModel(Option: Integer): TObject;
Begin
  Result := TModel.Create(Self);
End; // TSceneGL.CreateModel

Function TSceneGL.CreateDynamicLight(Option: Integer): TObject;
Begin
  Result := TDynamicLight.Create(Self);
End; // TSceneGL.CreateDynamicLight

Function TSceneGL.MoveObject(ObjectEllipsoid: TEllipsoid; ObjectVelocity: T3DPoint{I3dPoint}; GravityAmount: Single; ExcludeEntity: TEntity): Boolean;
// Returns True if the gravity check resulted in a collision
Const MaxRecurseDepth = 5;
Var
  RecurseDepth    : Integer;
  MovementSphere  : TSphere;
  NewCenterPoint  : T3DPoint{I3dPoint};
  NewVelocity     : T3DPoint{I3dPoint};
  Gravity         : T3DPoint{I3dPoint};

  Procedure Log(St: String);
  Var F: System.Text;
  Begin
    AssignFile(F,'coll.txt');
    If FileExists('coll.txt')
     Then Append(F)
     Else ReWrite(F);
    WriteLn(F,St);
    Flush(F);
    CloseFile(F);
  End; // Log

  Function CollideWithWorld(Ellipsoid: TEllipsoid; Velocity: T3DPoint{I3dPoint}; Sphere: TSphere; NewCen,NewVel: T3DPoint{I3dPoint}; UseFriction: Boolean; ExcludeEntity: TEntity; PushBack: Boolean): Boolean;
  Var NearestDistance,Friction,Nearest,Len: Single;
  Begin
    If Not Velocity.IsZero Then
    Begin
      NearestDistance := 10000000;
      Friction        := 1;
      Nearest         := NearestDistance;

      MovementSphere.Center.Copy(Velocity);
      MovementSphere.Center.Divide(2);
      MovementSphere.Center.Add(Ellipsoid.Center);
      MovementSphere.Radius := 2 * Ellipsoid.Radius.GetLength + Velocity.GetLength;

      BSPTreeRoot.MoveToIntersection(Ellipsoid,Velocity,Sphere,Nearest,Friction,NewCen,NewVel,UseFriction,ExcludeEntity,PushBack);
      If Nearest <> NearestDistance Then
      Begin
        Ellipsoid.Center.Copy(NewCen);
        Velocity.Copy(NewVel);
        Inc(RecurseDepth);
        If (RecurseDepth < MaxRecurseDepth) And (Velocity.GetLength > 2 * VeryCloseDistance) Then
         CollideWithWorld(Ellipsoid,Velocity,Sphere,NewCen,NewVel,UseFriction,ExcludeEntity,PushBack);
        Result := True;
      End
      Else
      Begin
        NewVel.Copy(Velocity);
        Len := NewVel.GetLength;
        If Len > 0 Then NewVel.Multiply(Max(Len - VeryCloseDistance,0) / Len);
  {
        Log(Format('No collision: moving by (%8.2f,%8.2f,%8.2f), from (%8.2f,%8.2f,%8.2f) to (%8.2f,%8.2f,%8.2f)',
                   [NewVel.X,NewVel.Y,NewVel.Z,
                    Ellipsoid.Center.X,Ellipsoid.Center.Y,Ellipsoid.Center.Z,
                    Ellipsoid.Center.X + NewVel.X,Ellipsoid.Center.Y + NewVel.Y,Ellipsoid.Center.Z + NewVel.Z]));
  }
        Ellipsoid.Center.Add(NewVel);
        Result := False;
      End;
//      Else Ellipsoid.Center.Add(Velocity);
    End
    Else Result := True;
  End; // CollideWithWorld

Begin
  RecurseDepth    := 0;
  MovementSphere  := TSphere.Create;
  NewCenterPoint  := T3DPoint.Create{Point}(ObjectEllipsoid.Center);
  NewVelocity     := T3DPoint.Create{Point};
  Gravity         := T3DPoint.Create{Point}(0,0,-GravityAmount);

//  Log('TSceneGL.MoveObject');

  CollideWithWorld(ObjectEllipsoid,ObjectVelocity,MovementSphere,NewCenterPoint,NewVelocity,False,ExcludeEntity,False);

  // Do again for gravity

  Result := CollideWithWorld(ObjectEllipsoid,Gravity,MovementSphere,NewCenterPoint,NewVelocity,True,ExcludeEntity,True);

  Gravity.Free;
  NewVelocity.Free;
  NewCenterPoint.Free;
//  Gravity        := Nil;
//  NewVelocity    := Nil;
//  NewCenterPoint := Nil;
  MovementSphere.Free;
End; // TSceneGL.MoveObject

Procedure TSceneGL.InternalMoveObject(ObjectEllipsoid: TEllipsoid; ObjectVelocity: T3DPoint{I3dPoint}; GravityAmount: Single; ExcludeEntity: TEntity);
Const MaxRecurseDepth = 5;
Var RecurseDepth: Integer;

  Procedure Log(St: String);
  Var F: System.Text;
  Begin
    AssignFile(F,'coll.txt');
    If FileExists('coll.txt')
     Then Append(F)
     Else ReWrite(F);
    WriteLn(F,St);
    Flush(F);
    CloseFile(F);
  End; // Log

  Procedure CollideWithWorld(Ellipsoid: TEllipsoid; Velocity: T3DPoint{I3dPoint}; Sphere: TSphere; NewCen,NewVel: T3DPoint{I3dPoint}; UseFriction: Boolean; ExcludeEntity: TEntity; PushBack: Boolean);
  Var NearestDistance,Friction,Nearest,Len: Single;
  Begin
    If Not Velocity.IsZero Then
    Begin
      NearestDistance := 10000000;
      Friction        := 1;
      Nearest         := NearestDistance;

      FMovementSphere.Center.Copy(Velocity);
      FMovementSphere.Center.Divide(2);
      FMovementSphere.Center.Add(Ellipsoid.Center);
      FMovementSphere.Radius := 2 * Ellipsoid.Radius.GetLength + Velocity.GetLength;

      BSPTreeRoot.MoveToIntersection(Ellipsoid,Velocity,Sphere,Nearest,Friction,NewCen,NewVel,UseFriction,ExcludeEntity,PushBack);
      If Nearest <> NearestDistance Then
      Begin
        Ellipsoid.Center.Copy(NewCen);
        Velocity.Copy(NewVel);
        Inc(RecurseDepth);
        If (RecurseDepth < MaxRecurseDepth) And (Velocity.GetLength > 2 * VeryCloseDistance) Then
         CollideWithWorld(Ellipsoid,Velocity,Sphere,NewCen,NewVel,UseFriction,ExcludeEntity,PushBack);
      End
      Else
      Begin
        NewVel.Copy(Velocity);
        Len := NewVel.GetLength;
        If Len > 0 Then NewVel.Multiply(Max(Len - VeryCloseDistance,0) / Len);
  {
        Log(Format('No collision: moving by (%8.2f,%8.2f,%8.2f), from (%8.2f,%8.2f,%8.2f) to (%8.2f,%8.2f,%8.2f)',
                   [NewVel.X,NewVel.Y,NewVel.Z,
                    Ellipsoid.Center.X,Ellipsoid.Center.Y,Ellipsoid.Center.Z,
                    Ellipsoid.Center.X + NewVel.X,Ellipsoid.Center.Y + NewVel.Y,Ellipsoid.Center.Z + NewVel.Z]));
  }
        Ellipsoid.Center.Add(NewVel);
      End;
//      Else Ellipsoid.Center.Add(Velocity);
    End;
  End; // CollideWithWorld

Begin
  RecurseDepth := 0;
  FNewCenterPoint.Copy(ObjectEllipsoid.Center);
  FGravity.Copy(0,0,-GravityAmount);

//  Log('TSceneGL.MoveObject');

  CollideWithWorld(ObjectEllipsoid,ObjectVelocity,FMovementSphere,FNewCenterPoint,FNewVelocity,False,ExcludeEntity,False);

  // Do again for gravity

  If GravityAmount <> 0 Then CollideWithWorld(ObjectEllipsoid,FGravity,FMovementSphere,FNewCenterPoint,FNewVelocity,True,ExcludeEntity,True);
End; // TSceneGL.InternalMoveObject

// ---------------------------
// TLight
// ---------------------------

// Creates a new, white ambient light.  Num goes from 1 to 8
Constructor TLight.Create(Num: Integer);
Begin
  Inherited Create;
  Position        := T3DPoint.Create{Point};
  Normal          := T3DPoint.Create{Point};
//  Source          := TVertex.Create;
  Rotation[1]     := 0;
  Rotation[2]     := 0;
  Rotation[3]     := 0;
  Scale[1]        := 1;
  Scale[2]        := 1;
  Scale[3]        := 1;
  Ambient(0.5,0.5,0.5,1);
  Diffuse(0.25,0.25,0.25,1);
  Specular(0.1,0.1,0.1,1);
  Enabled         := True;                       // By default the light is enabled
  LightType       := clAmbiental;                // By default the light is ambiental
  CutOffAngle     := 60;                         // But the default cut-off angle is 60 degrees
  SpotExponent    := 100;                        // And the spot exponent is very shiny
  Attenuation     := 0.01;                       // Attenuation default value
  AttenuationType := clLinear;                   // Attenuation type
  DynamicLight    := TDynamicLight.Create;
{
  MaxLights := 1;
  glGetIntegerv(GL_MAX_LIGHTS,@MaxLights);
  LogGLError('Attempting to determine the maximum number of lights');
  If Num >= MaxLights Then Num := MaxLights - 1;
}
  Number := Num + GL_LIGHT0;
End; // TLight.Create

Destructor TLight.Destroy;
Begin
//  Source.Free;
  Normal.Free;
  Position.Free;
  DynamicLight.Free;
//  Normal   := Nil;
//  Position := Nil;
End; // TLight.Destroy

// Changes the ambient component of the light
Procedure TLight.Ambient(R,G,B,A: GLFloat);
Begin
  FAmbient[0] := R;
  FAmbient[1] := G;
  FAmbient[2] := B;
  FAmbient[3] := A;
End; // TLight.Ambient

// Changes the diffuse component of the light
Procedure TLight.Diffuse(R,G,B,A: GLFloat);
Begin
  FDiffuse[0] := R;
  FDiffuse[1] := G;
  FDiffuse[2] := B;
  FDiffuse[3] := A;
End; // TLight.Diffuse

// Changes the specular component of the light
Procedure TLight.Specular(R,G,B,A: GLFloat);
Begin
  FSpecular[0] := R;
  FSpecular[1] := G;
  FSpecular[2] := B;
  FSpecular[3] := A;
End; // TLight.Specular

// Changes the direction where the light comes from
Procedure TLight.SetOrientation(NX,NY,NZ: GLFloat);
Begin
  Normal.Copy(NX,NY,NZ);
End; // TLight.SetOrientation

// Executes the OpenGL code for this light
Procedure TLight.Redraw;
Var
  FPosition : Packed Array[0..3] Of GLFloat;
  FRotation : Packed Array[0..3] Of GLFloat;
//  RX,RY,RZ  : GLFloat;
//  R1,R2,R3  : GLFloat;

Begin
  If Enabled Then
  Begin
    glEnable(Number);
    If LightType <> clAmbiental Then
    Begin
      // The light has a location

      FPosition[0] := Position.X{ * Scale[1]};
      FPosition[1] := Position.Y{ * Scale[2]};
      FPosition[2] := Position.Z{ * Scale[3]};
      FPosition[3] := 1.0;  {indicates that the light is actually present at that location}
                          {if it were zero, the rays would be parallel like a sun}

      FRotation[0] := Normal.X;
      FRotation[1] := Normal.Y;
      FRotation[2] := Normal.Z;

{
      RX := Source.NX;
      RY := Source.NY;
      RZ := Source.NZ;

      R1 := Rotation[1] * Pi / 180;
      R2 := Rotation[2] * Pi / 180;
      R3 := Rotation[3] * Pi / 180;

      FRotation[1] := RY * Cos(R1) - RZ * Sin(R1);
      FRotation[2] := RZ * Cos(R1) + RY * Sin(R1);
      RY := FRotation[1];
      RZ := FRotation[2];

      FRotation[0] := RX * Cos(R2) - RZ * Sin(R2);
      FRotation[2] := RZ * Cos(R2) + RX * Sin(R2);
      RX := FRotation[0];

      FRotation[0] := RX * Cos(R3) - RY * Sin(R3);
      FRotation[1] := RY * Cos(R3) + RX * Sin(R3);
}
      FRotation[3]:=1.0;  // Don´t know what this indicates
      If LightType = clSpot Then
      Begin
        glLightF(Number, GL_Spot_Cutoff, CutOffAngle);
        glLightF(Number, GL_Spot_Exponent, SpotExponent);
        glLightfv(Number, GL_Spot_Direction, @FRotation);
      End;
      glLightfv(Number, GL_POSITION, @FPosition);
    End;
    GlLightfv(Number, GL_AMBIENT, @FAmbient);
    GlLightfv(Number, GL_DIFFUSE, @FDiffuse);
    GlLightfv(Number, GL_SPECULAR,@FSpecular);
    Case AttenuationType Of
       CLconstant: GlLightf(Number, GL_Constant_attenuation, Attenuation);
         CLlinear: GlLightf(Number, GL_Linear_attenuation, Attenuation);
      CLquadratic: GlLightf(Number, GL_Quadratic_attenuation, Attenuation);
    End; {case}
//    glEnable(Number);   // Enable light number N
  End
  Else glDisable(Number);
End; // TLight.Redraw
(*
// ---------------------------
// T3DMouse
// ---------------------------

// iEntity is the pointer to one entity in the scene
Constructor T3DMouse.Create(iEntity: TEntity);
Var I: Integer;
Begin
  Inherited Create;
  Entity  := iEntity;
  Mode    := 3;         // This is the mode used by defect, just because it fits my needs
  Button1 := False;
  Button2 := False;
  For I := 1 To 6 Do BlockStatus[I] := False;
End; // T3DMouse.Create

// Move or turn the entity according to the mode
Procedure T3DMouse.Move(X,Y: Single; Buttons: TShiftState);
Begin
  If Assigned(Entity) Then
  Begin
    If Not Button1 Then
    Begin
      If ssLeft In Buttons Then
      Begin
        If Mode = 1 Then  // X,Y,Z
        Begin
          Start[1] := X - Entity.Position.X / Scaling[1]; // X
          Start[2] := Y - Entity.Position.Y / Scaling[2]; // Y
        End;
        If Mode In [2,3] Then  // 2: RX,RY,RZ     3: RX,RY,RZ,Z
        Begin
          Start[6] := X - Entity.Rotation.Z / Scaling[6]; // RX
          Start[4] := Y - Entity.Rotation.X / Scaling[4]; // RY
        End;
        Button1 := True;
      End;
    End
    Else
    Begin
      If ssLeft In Buttons Then
      Begin
        If Mode = 1 Then
        Begin
          If Not BlockStatus[1] Then Entity.Position.X := (X - Start[1]) * Scaling[1]; // X
          If Not BlockStatus[2] Then Entity.Position.Y := (Y - Start[2]) * Scaling[2]; // Y
        End;
        If Mode In [2,3] Then
        Begin
          If Not BlockStatus[4] Then Entity.Rotation.Z := (X - Start[6]) * Scaling[6]; // RX
          If Not BlockStatus[5] Then Entity.Rotation.X := (Y - Start[4]) * Scaling[4]; // RY
        End;
      End
      Else Button1 := False;
    End;
    If Not Button2 Then
    Begin
      If ssRight In Buttons Then
      Begin
        If Mode In [1,3] Then Start[3] := Y - Entity.Position.Z / Scaling[3]; // Z
        If Mode In [2,3] Then Start[5] := X - Entity.Rotation.Y / Scaling[5]; // RZ
        Button2 := True;
      End;
    End
    Else
    Begin
      If ssRight In Buttons Then
      Begin
        If (Mode In [1,3]) And Not BlockStatus[3] Then Entity.Position.Z := (Y - Start[3]) * Scaling[3];  // Z
        If (Mode In [2,3]) And Not BlockStatus[6] Then Entity.Rotation.Y := (X - Start[5]) * Scaling[5];  // RZ
      End
      Else Button2 := False;
    End;
  End;
End; // T3DMouse.Move

// Controls the relative speed between the mouse and the object
Procedure T3DMouse.Scale(X,Y,Z,RX,RY,RZ: Single);
Begin
  Scaling[1] := X;
  Scaling[2] := Y;
  Scaling[3] := Z;
  Scaling[4] := RX;
  Scaling[5] := RY;
  Scaling[6] := RZ;
End; // T3DMouse.Scale

// Blocks movement and/or rotation on any of 6 axis.
// Num goes from 1 to 6,  Valor=True: block, Valor=False: don´t block movement
Procedure T3DMouse.Block(Num: Integer; Valor: Boolean);
Begin
  If Num In [1..6] Then BlockStatus[Num] := Valor;
End; // T3DMouse.Block
*)
(*
// Returns a pointer to the vertex which was nearer to the mouse
// Scene is the scene to be evaluated, pt is the chosen vertex, can be nil If none was found under the mouse
// x,y are the coordinates of the mouse
Procedure T3DMouse.FindVertex(X,Y: Integer; Scene: TSceneGL; Var PT: TVertex);
Const
  Tambuffer = 8000;  // 8000 items reserved for the rendering buffer
  Radio     = 15;    // This is the search radius

Var
  Buffer           : Array[0..TamBuffer] Of GLFloat;
  Size,I,J,Count   : Integer;
  NX,NY            : Integer;
  PreviousPutNames : boolean;
  ActualVertex     : LongInt;
  NumFound         : Integer;
  VertexHits       : Array[1..MaxHits] Of Integer;

Begin
  PreviousPutNames := PutNames;  // Preserve the previous value of PutNames
  PutNames         := True;      // Put names on each vertex
  NumFound         := 0;
  GlFeedBackBuffer(TamBuffer,GL_2D,@Buffer);
  GlRenderMode(GL_feedBack);
  Scene.Redraw;
  Size := GlRenderMode(GL_render);

  // Now that we got the 2D coordinates of each vertex, find which vertex is near

  I := 0;
  Try
    While I < Size Do
    Begin
      // The G.P.T.T. is a marker to divide the buffer in sections

      If Buffer[I] = GL_Pass_Through_Token Then
      Begin
        If Buffer[I + 1] = Entity.ID Then    
        Begin
          // This is the entity we are dealing with

          Inc(I,2);

          // Continue cycling until we find another object marker

          While Buffer[I] <> GL_Pass_Through_Token Do
          Begin
            ActualVertex := Round(Buffer[I + 1]);
            If Buffer[I + 2] = GL_Polygon_Token Then
            Begin
              // This is a polygon; let´s check it out
              
              Count := Trunc(Buffer[I + 3]);
              Inc(I,4);

              // Let´s take a look at each vertex of this polygon

              For J := 0 To Count - 1 Do
              Begin
                NX := Round(Buffer[I]);        // X coordinate of a vertex
                NY := Round(Buffer[I + 1]);    // Y coordinate of a vertex
                If (NX + Radio > X)     And
                   (NX - Radio < X)     And
                   (NY + Radio > Y)     And
                   (NY - Radio < Y)     And
                   (NumFound < MaxHits) Then
                Begin
                  Inc(NumFound);
                  VertexHits[NumFound] := ActualVertex + J;
                End;
                Inc(I,2); // X and Y
              End; // For J
            End
            Else Inc(I,2);
          End; // While
        End;
      End;
      Inc(I);
    End; // While
  Except
  End;
  PutNames := PreviousPutNames;  // Restore the previous value of PutNames
End; // T3DMouse.FindVertex
*)

// ---------------------------
// TVertex
// ---------------------------
(*
Constructor TVertex.Create;
Begin
//  Position := T3DPoint.Create;
//  Normal   := T3DPoint.Create;
  TX       := 0;
  TZ       := 0;
  Color    := 0;
End; // TVertex.Create

Constructor TVertex.Create(Vertex: TVertex);
Begin
//  Position := T3DPoint.Create(Vertex.Position);
//  Normal   := T3DPoint.Create(Vertex.Normal);
  TX       := Vertex.TX;
  TZ       := Vertex.TZ;
  Color    := Vertex.Color;
End; // TVertex.Create

Destructor TVertex.Destroy;
Begin
//  Position.Free;
//  Normal.Free;
End; // TVertex.Destroy
{
Procedure TVertex.Copy(Vertex: TVertex);
Begin
  Position.Copy(Vertex.Position);
  Normal.Copy(Vertex.Normal);
  TX    := Vertex.TX;
  TZ    := Vertex.TZ;
  Color := Vertex.Color;
End; // TVertex.Copy
}
*)
// ---------------------------
// TFace
// ---------------------------
(*
Constructor TFace.Create;
Begin
//  Normal       := T3DPoint.Create;
//  HasAlpha     := False;
//  CircumSphere := TSphere.Create;
  Texture      := Nil;
  Flags        := ffSolid;
//  Solid        := True;
End; // TFace.Create

Destructor TFace.Destroy;
Begin
//  Normal.Free;
//  CircumSphere.Free;
End; // TFace.Destroy
{
Procedure TFace.Flip;
// Makes it face "inward" instead of "outward", i.e. flips the way it faces
Var I,J: Integer;
Begin
  For I := 0 To (NumVerts Div 2) - 1 Do
  Begin
    J                            := Vertices[I];
    Vertices[I]                  := Vertices[(NumVerts - 1) - I];
    Vertices[(NumVerts - 1) - I] := J;
  End; // For I
End; // TFace.Flip
}
{
Procedure TFace.CalculateCircumSphere(P0,P1,P2: T3DPoint);
Var A,B,C: Single;
Begin
  A := P1.DistanceFrom2(P0);
  B := P2.DistanceFrom2(P1);
  C := P0.DistanceFrom2(P2);
  CircumSphere.Radius := 0;
  If A > B Then
  Begin
    If A > C Then
    Begin
      If A <> 0 Then
      Begin
        CircumSphere.Center.Copy(P1);
        CircumSphere.Center.Add(P0);
        CircumSphere.Radius := Sqrt(A) / 2;
      End;
    End
    Else
    Begin
      If C <> 0 Then
      Begin
        CircumSphere.Center.Copy(P2);
        CircumSphere.Center.Add(P0);
        CircumSphere.Radius := Sqrt(C) / 2;
      End;
    End;
  End
  Else
  Begin
    If B > C Then
    Begin
      If B <> 0 Then
      Begin
        CircumSphere.Center.Copy(P1);
        CircumSphere.Center.Add(P2);
        CircumSphere.Radius := Sqrt(B) / 2;
      End;
    End
    Else
    Begin
      If C <> 0 Then
      Begin
        CircumSphere.Center.Copy(P2);
        CircumSphere.Center.Add(P0);
        CircumSphere.Radius := Sqrt(C) / 2;
      End;
    End;
  End;
  CircumSphere.Center.Divide(2);
End; // TFace.CalculateCircumSphere
}
*)
(*
// ---------------------------
// TModelFaceSorter
// ---------------------------

Constructor TModelFaceSorter.Create;
Begin
  FSorter := TQuickSorter.Create;
  SetLength(FList,0);
  FSorter.CompareMethod  := Compare;
  FSorter.ExchangeMethod := Exchange;
End; // TModelFaceSorter.Create

Destructor TModelFaceSorter.Destroy;
Begin
  SetLength(FList,0);
  FSorter.Free;
End; // TModelFaceSorter.Destroy

Function TModelFaceSorter.Compare(Index0,Index1: Integer): Integer;
Begin
  Result := Sign(FList[Index0].Area - FList[Index1].Area);
End; // TModelFaceSorter.Compare

Procedure TModelFaceSorter.Exchange(Index0,Index1: Integer);
Var IA: TFaceArea;
Begin
  IA            := FList[Index0];
  FList[Index0] := FList[Index1];
  FList[Index1] := IA;
End; // TModelFaceSorter.Exchange

Procedure TModelFaceSorter.SortFacesByArea(Model: TModel);
Var
  I,J      : Integer;
  Face     : TFace;
  V1       : T3DPoint;
  V2       : T3DPoint;
  P1,P2,P3 : T3DPoint;

Begin
  V1 := T3DPoint.Create;
  V2 := T3DPoint.Create;
  P1 := T3DPoint.Create;
  P2 := T3DPoint.Create;
  P3 := T3DPoint.Create;
  SetLength(FList,High(Model.Faces) + 1);

  // Calculate the face areas

  J := 0;
  For I := 0 To High(FList) Do
  Begin
    Face := Model.Faces[I];
    FList[I].Index := I;

    P1.Copy(Model.Positions.DataArray[J + 0],Model.Positions.DataArray[J + 1],Model.Positions.DataArray[J + 2]);
    P2.Copy(Model.Positions.DataArray[J + 3],Model.Positions.DataArray[J + 4],Model.Positions.DataArray[J + 5]);
    P3.Copy(Model.Positions.DataArray[J + 6],Model.Positions.DataArray[J + 7],Model.Positions.DataArray[J + 8]);

    V1.Copy(P2,P1);
    V2.Copy(P2,P3);

    V2.Cross(V1);
    FList[I].Area := V2.GetLength / 2;
    Inc(J,9);
  End; // For I

  // Sort the list by area (sorts in ascending order)

  FSorter.Sort(0,High(FList));

  // Reassign the faces from greatest area to least area

  For I := 0 To High(FList) Do //Model.Faces[I] := FList[High(FList) - I].Face;
  Begin
  End; // For I

  // Cleanup

  SetLength(FList,0);
  P3.Free;
  P2.Free;
  P1.Free;
  V2.Free;
  V1.Free;
End; // TModelFaceSorter.SortFacesByArea
*)
// ---------------------------
// TModel
// ---------------------------

Constructor TModel.Create(AOwner: TSceneGL);
Begin
  FOwner            := AOwner;
  SetLength(Faces,0);
  SetLength(Vertices,0);
  SetLength(PieceIndices,0);
  SetLength(Colors,0);
  SetLength(VNormals,0);
  SetLength(TexCounts,0);
  FBox              := TAxisAlignedBox.Create;
  FSphere           := TSphere.Create;
  FCylinder         := TCylinder.Create;
  ColorType         := mccAmbientAndDiffuse;
  FRayWork          := T3DPoint.Create{Point};
  FP1Work           := T3DPoint.Create{Point};
  FP2Work           := T3DPoint.Create{Point};
  FP3Work           := T3DPoint.Create{Point};
  FP4Work           := T3DPoint.Create{Point};
  FP5Work           := T3DPoint.Create{Point};
  FHasAlpha         := False;
  FHasTrans         := False;
  FSetHasAlphaTrans := False;
  FAnimated         := False;
  Positions         := TFVector.BuildVector(4);
  FNormals          := TFVector.BuildVector(4);
  BaseHeight        := -1;
  FCheckOcclusion   := True;
  TextureSets       := TIntegerPointerHash.Create(False);
  FTriedVBOs        := False;
  FUsingVBOs        := False;
  FSkipFirstVBO     := False;
  SetLength(FVBOVertices,0);
  SetLength(FVBOTexCoords,0);
  SetLength(FVBOColors,0);
  SetLength(FVBONormals,0);
End; // TModel.Create

Destructor TModel.Destroy;
Begin
  Clear;
  FSphere.Free;
  FCylinder.Free;
  FBox.Free;
  FRayWork.Free;
  FP1Work.Free;
  FP2Work.Free;
  FP3Work.Free;
  FP4Work.Free;
  FP5Work.Free;
{  FRayWork := Nil;
  FP1Work  := Nil;
  FP2Work  := Nil;
  FP3Work  := Nil;
  FP4Work  := Nil;
  FP5Work  := Nil;}
  Positions.Free;
  FNormals.Free;
  TextureSets.Free;
End; // TModel.Destroy

Procedure TModel.Clear;
Var I,J: Integer;
Begin
  If FUsingVBOs Then
  Begin
{
    Try
//      For I := 0 To High(FVBOVertices) Do glDeleteBuffersARB(1, @(FVBOVertices[I]));
      glDeleteBuffersARB(High(FVBOVertices)  + 1, @(FVBOVertices[0]));
    Finally
      SetLength(FVBOVertices,0);
    End;
    Try
//      For I := 0 To High(FVBOTexCoords) Do glDeleteBuffersARB(1, @(FVBOTexCoords[I]));
      If High(FVBOTexCoords) >= 0 Then glDeleteBuffersARB(High(FVBOTexCoords) + 1, @(FVBOTexCoords[0]));
    Finally
      SetLength(FVBOTexCoords,0);
    End;
    Try
//      For I := 0 To High(FVBOColors) Do glDeleteBuffersARB(1, @(FVBOColors[I]));
      If High(FVBOColors)    >= 0 Then glDeleteBuffersARB(High(FVBOColors)    + 1, @(FVBOColors[0]));
    Finally
      SetLength(FVBOColors,0);
    End;
    Try
//      For I := 0 To High(FVBONormals) Do glDeleteBuffersARB(1, @(FVBONormals[I]));
      If High(FVBONormals)   >= 0 Then glDeleteBuffersARB(High(FVBONormals)   + 1, @(FVBONormals[0]));
    Finally
      SetLength(FVBONormals,0);
    End;
}

    If FSkipFirstVBO Then J := 1 Else J := 0;

    If High(FVBOVertices)  >= 0 Then
    Begin
      For I := J To High(FVBOVertices) Do
       glDeleteBuffersARB(1, @FVBOVertices[I]);
{      If TexCounts[0].Texture >= -1
       Then glDeleteBuffersARB(High(FVBOVertices) + 1, @FVBOVertices[0])
       Else glDeleteBuffersARB(High(FVBOVertices) + 0, @FVBOVertices[1]);}
//      SetLength(FVBOVertices,0);
    End;
    If High(FVBOTexCoords) >= 0 Then
    Begin
      For I := J To High(FVBOTexCoords) Do
       glDeleteBuffersARB(1, @FVBOTexCoords[I]);
{      If TexCounts[0].Texture >= -1
       Then glDeleteBuffersARB(High(FVBOTexCoords) + 1, @FVBOTexCoords[0])
       Else glDeleteBuffersARB(High(FVBOTexCoords) + 0, @FVBOTexCoords[1]);}
//      SetLength(FVBOTexCoords,0);
    End;
    If High(FVBOColors)    >= 0 Then
    Begin
      For I := J To High(FVBOColors) Do
       glDeleteBuffersARB(1, @FVBOColors[I]);
{      If TexCounts[0].Texture >= -1
       Then glDeleteBuffersARB(High(FVBOColors) + 1, @FVBOColors[0])
       Else glDeleteBuffersARB(High(FVBOColors) + 0, @FVBOColors[1]);}
//      SetLength(FVBOColors,0);
    End;
    If High(FVBONormals)   >= 0 Then
    Begin
      For I := J To High(FVBONormals) Do
       glDeleteBuffersARB(1, @FVBONormals[I]);
{      If TexCounts[0].Texture >= -1
       Then glDeleteBuffersARB(High(FVBONormals) + 1, @FVBONormals[0])
       Else glDeleteBuffersARB(High(FVBONormals) + 0, @FVBONormals[1]);}
//      SetLength(FVBONormals,0);
    End;

    FUsingVBOs := False;
  End;

  SetLength(FVBOVertices,0);
  SetLength(FVBOTexCoords,0);
  SetLength(FVBOColors,0);
  SetLength(FVBONormals,0);

  SetLength(Faces,0);
  SetLength(Colors,0);
  SetLength(Vertices,0);
  SetLength(PieceIndices,0);
  SetLength(VNormals,0);
  SetLength(TexCounts,0);
  Positions.SetLength(0);
  FNormals.SetLength(0);
  FreeTextureSets;
  BaseHeight := -1;
  FTriedVBOs := False;
  FSkipFirstVBO := False;
End; // TModel.Clear

Procedure TModel.SetTexture(Texture: TTexture);
Var I: Integer;
Begin
  If (Texture <> Nil) And (FOwner <> Nil) Then
  Begin
    For I := 0 To High(Faces) Do Faces[I].Texture := FOwner.Textures.IndexOfObject(Texture);
  End
  Else
  Begin
    For I := 0 To High(Faces) Do Faces[I].Texture := -1;
  End;
End; // TModel.SetTexture

Procedure TModel.FlipFaces;
// Makes the faces face "inward" instead of outward, i.e. flips which way they face
Var
  I,J,K : Integer;
  X,Y,Z : Single;
  Norm  : LongWord;

Begin
  J := 0;
  K := 0;
  For I := 0 To High(Faces) Do
  Begin
    X                          := Positions.DataArray[J];
    Y                          := Positions.DataArray[J + 1];
    Z                          := Positions.DataArray[J + 2];
    Positions.DataArray[J + 0] := Positions.DataArray[J + 6];
    Positions.DataArray[J + 1] := Positions.DataArray[J + 7];
    Positions.DataArray[J + 2] := Positions.DataArray[J + 8];
    Positions.DataArray[J + 6] := X;
    Positions.DataArray[J + 7] := Y;
    Positions.DataArray[J + 8] := Z;
    Norm                       := VNormals[K];
    VNormals[K]                := VNormals[K + 2];
    VNormals[K + 2]            := Norm;
    Inc(J,9);
    Inc(K,3);
  End; // For I
End; // TModel.FlipFaces

Procedure TModel.Rescale(SX,SY,SZ: Single);
Var
  I,J      : Integer;
  RX,RY,RZ : Single;

Begin
  CalcExtents(False);
  RX := FBox.MaxPt.X - FBox.MinPt.X;
  RY := FBox.MaxPt.Y - FBox.MinPt.Y;
  RZ := FBox.MaxPt.Z - FBox.MinPt.Z;
  If RX <> 0 Then RX := SX / RX;
  If RY <> 0 Then RY := SY / RY;
  If RZ <> 0 Then RZ := SZ / RZ;

  J := 0;
  For I := 0 To High(Vertices) Do
  Begin
    Positions.DataArray[J]     := Positions.DataArray[J]     * RX;
    Positions.DataArray[J + 1] := Positions.DataArray[J + 1] * RY;
    Positions.DataArray[J + 2] := Positions.DataArray[J + 2] * RZ;
    Inc(J,3);
  End; // For I

  CalcExtents(False);
End; // TModel.Rescale

Procedure TModel.GeneratePolyhedron(Points: Array Of Single; FacePointCount,FaceDefs: Array Of Integer);
Var I,J,K: Integer;

  Procedure SetupFaces(PointCount,TriangleIndex,DefIndex: Integer);
  Var
    I,J,K,L  : Integer;
    CX,CY,CZ : Single;

  Begin
    If PointCount = 3 Then
    Begin
      For I := 0 To PointCount - 1 Do
      Begin
        K := (TriangleIndex * 3 + I) * 3;
        Positions.DataArray[K]     := Points[FaceDefs[DefIndex + (PointCount - 1) - I] * 3 + 0];
        Positions.DataArray[K + 1] := Points[FaceDefs[DefIndex + (PointCount - 1) - I] * 3 + 1];
        Positions.DataArray[K + 2] := Points[FaceDefs[DefIndex + (PointCount - 1) - I] * 3 + 2];
      End; // For I
    End
    Else If PointCount = 4 Then
    Begin
      K := TriangleIndex * 3 * 3;

      Positions.DataArray[K]      := Points[FaceDefs[DefIndex + PointCount - 1] * 3 + 0];
      Positions.DataArray[K + 1]  := Points[FaceDefs[DefIndex + PointCount - 1] * 3 + 1];
      Positions.DataArray[K + 2]  := Points[FaceDefs[DefIndex + PointCount - 1] * 3 + 2];

      Positions.DataArray[K + 3]  := Points[FaceDefs[DefIndex + PointCount - 2] * 3 + 0];
      Positions.DataArray[K + 4]  := Points[FaceDefs[DefIndex + PointCount - 2] * 3 + 1];
      Positions.DataArray[K + 5]  := Points[FaceDefs[DefIndex + PointCount - 2] * 3 + 2];

      Positions.DataArray[K + 6]  := Points[FaceDefs[DefIndex + PointCount - 3] * 3 + 0];
      Positions.DataArray[K + 7]  := Points[FaceDefs[DefIndex + PointCount - 3] * 3 + 1];
      Positions.DataArray[K + 8]  := Points[FaceDefs[DefIndex + PointCount - 3] * 3 + 2];

      Positions.DataArray[K + 9]  := Points[FaceDefs[DefIndex + PointCount - 3] * 3 + 0];
      Positions.DataArray[K + 10] := Points[FaceDefs[DefIndex + PointCount - 3] * 3 + 1];
      Positions.DataArray[K + 11] := Points[FaceDefs[DefIndex + PointCount - 3] * 3 + 2];

      Positions.DataArray[K + 12] := Points[FaceDefs[DefIndex + PointCount - 4] * 3 + 0];
      Positions.DataArray[K + 13] := Points[FaceDefs[DefIndex + PointCount - 4] * 3 + 1];
      Positions.DataArray[K + 14] := Points[FaceDefs[DefIndex + PointCount - 4] * 3 + 2];

      Positions.DataArray[K + 15] := Points[FaceDefs[DefIndex + PointCount - 1] * 3 + 0];
      Positions.DataArray[K + 16] := Points[FaceDefs[DefIndex + PointCount - 1] * 3 + 1];
      Positions.DataArray[K + 17] := Points[FaceDefs[DefIndex + PointCount - 1] * 3 + 2];
    End
    Else
    Begin
      // First find the center

      CX := 0;
      CY := 0;
      CZ := 0;
      For I := 0 To PointCount - 1 Do
      Begin
        CX := CX + Points[FaceDefs[DefIndex + I] * 3 + 0];
        CY := CY + Points[FaceDefs[DefIndex + I] * 3 + 1];
        CZ := CZ + Points[FaceDefs[DefIndex + I] * 3 + 2];
      End; // For I
      CX := CX / PointCount;
      CY := CY / PointCount;
      CZ := CZ / PointCount;

      // Assign the vertices

      For I := 0 To PointCount - 1 Do
      Begin
        K := (PointCount - 1) - I;
        L := K - 1;
        If L < 0 Then L := PointCount - 1;

        J := (TriangleIndex * 3 + I * 3) * 3;

        Positions.DataArray[J]     := Points[FaceDefs[DefIndex + K] * 3 + 0];
        Positions.DataArray[J + 1] := Points[FaceDefs[DefIndex + K] * 3 + 1];
        Positions.DataArray[J + 2] := Points[FaceDefs[DefIndex + K] * 3 + 2];

        Positions.DataArray[J + 3] := Points[FaceDefs[DefIndex + L] * 3 + 0];
        Positions.DataArray[J + 4] := Points[FaceDefs[DefIndex + L] * 3 + 1];
        Positions.DataArray[J + 5] := Points[FaceDefs[DefIndex + L] * 3 + 2];

        Positions.DataArray[J + 6] := CX;
        Positions.DataArray[J + 7] := CY;
        Positions.DataArray[J + 8] := CZ;
      End; // For I
    End;
  End; // SetupFaces

Begin
  Clear;

  // Figure out how many triangles we'll need from the number of sides in each face

  J := 0;
  For I := 0 To High(FacePointCount) Do
  Begin
         If FacePointCount[I] = 3 Then Inc(J)
    Else If FacePointCount[I] = 4 Then Inc(J,2)
    Else Inc(J,FacePointCount[I]);
  End; // For I
  AddFaces(J);
  AddVertices(J * 3);
  For I := 0 To High(Faces) Do
  Begin
    Faces[I].Texture  := -1;
    Faces[I].Flags    := ffSolid;
  End; // For I
  For I := 0 To High(Colors) Do
  Begin
    TBGRA(Colors[I]).B := Round(Random * 255);
    TBGRA(Colors[I]).G := Round(Random * 255);
    TBGRA(Colors[I]).R := Round(Random * 255);
    TBGRA(Colors[I]).A := 255;
  End; // For I

  J := 0;
  K := 0;
  For I := 0 To High(FacePointCount) Do
  Begin
    SetupFaces(FacePointCount[I],J,K);
         If FacePointCount[I] = 3 Then Inc(J)
    Else If FacePointCount[I] = 4 Then Inc(J,2)
    Else Inc(J,FacePointCount[I]);
    Inc(K,FacePointCount[I]);
  End; // For I

  Rescale(2,2,2); // Have points range from -1..1

  CalcNormals(True);
  CalcExtents(False);
End; // TModel.GeneratePolyhedron

Procedure TModel.GenerateCloudDeck;
Const
  Edge = 10;              // MUST be even!
  Arc  = 20 * Pi / 180;   // Arcs from +Arc to -Arc
  
Var
  Points         : Array Of Single;
  FacePointCount : Array Of Integer;
  FaceDefs       : Array Of Integer;
  I,J            : Integer;
  R,H,W,Q        : Single;

Begin
  SetLength(Points,3 * Sqr(Edge + 1));
  SetLength(FacePointCount,Sqr(Edge));
  SetLength(FaceDefs,Sqr(Edge) * 4);

  R := 1 / Sin(Arc);
  H := R * Cos(Arc);

  For I := 0 To Edge Do
  Begin
    For J := 0 To Edge Do
    Begin
      Points[(I * (Edge + 1) + J) * 3 + 0] := -1 + (J / Edge) * 2;
      Points[(I * (Edge + 1) + J) * 3 + 1] :=  1 - (I / Edge) * 2;

      W := Sqrt(Sqr(((Edge Div 2) - I) / (Edge Div 2)) + Sqr(((Edge Div 2) - J) / (Edge Div 2)));
      Q := ArcTan(W / H);
      Points[(I * (Edge + 1) + J) * 3 + 2] := R * Cos(Q) - H;
//      Q := Sqrt(Sqr(W) + Sqr(H));
//      Points[(I * (Edge + 1) + J) * 3 + 2] := ArcSin(Max(-1,Min(1,W / Q)));

    End; // For J
  End; // For I

  For I := 0 To High(FacePointCount) Do FacePointCount[I] := 4;

  For I := 0 To Edge - 1 Do
  Begin
    For J := 0 To Edge - 1 Do
    Begin
      FaceDefs[(I * Edge + J) * 4 + 0] := I * (Edge + 1) + J;
      FaceDefs[(I * Edge + J) * 4 + 1] := I * (Edge + 1) + J + 1;
      FaceDefs[(I * Edge + J) * 4 + 2] := (I + 1) * (Edge + 1) + J + 1;
      FaceDefs[(I * Edge + J) * 4 + 3] := (I + 1) * (Edge + 1) + J;
    End; // For J
  End; // For I

  GeneratePolyhedron(Points,FacePointCount,FaceDefs);

  Rescale(2,2,R - H);
  CalcNormals(True);
  CalcExtents(False);

  SetLength(Points,0);
  SetLength(FacePointCount,0);
  SetLength(FaceDefs,0);
End; // TModel.GenerateCloudDeck

Procedure TModel.GenerateSphere;
Const
  LatInc  = 10;   // MUST be evenly divisible into 180
  LongInc = 10;  // MUST be evenly divisible into 360

Var
  I,J,K          : Integer;
  Quads          : Integer;
  Tris           : Integer;
  PointsAround   : Integer;
  QuadRings      : Integer;
  Lat            : Integer;
  Long           : Integer;
  Points         : Array Of Single;
  FacePointCount : Array Of Integer;
  FaceDefs       : Array Of Integer;

Begin
  // Helper variables

  PointsAround := 360 Div LongInc;
  QuadRings    := Max(0,(180 Div LatInc) - 2);
  Quads        := QuadRings * PointsAround;
  Tris         := 2 * PointsAround;

  // Allocate arrays

  SetLength(Points,3 * ((QuadRings + 1) * PointsAround + 2));          // The first point is the bottom and the last point is the top
  SetLength(FacePointCount,Quads + Tris);
  SetLength(FaceDefs,Quads * 4 + Tris * 3); // First and last sets are triangles, all others are quads

  // Set the bottom point

  Points[0] := 0;
  Points[1] := 0;
  Points[2] := -1;

  // Set the points, bottom to top

  K := 3;
  For I := 0 To QuadRings Do
  Begin
    For J := 0 To PointsAround - 1 Do
    Begin
      Points[K + 0] := Cos(J * LongInc * Pi / 180) * Sin((I + 1) * LatInc * Pi / 180);
      Points[K + 1] := Sin(J * LongInc * Pi / 180) * Sin((I + 1) * LatInc * Pi / 180);
      Points[K + 2] := -Cos((I + 1) * LatInc * Pi / 180);
      Inc(K,3);
    End; // For J
  End; // For I

  // Set the top point

  Points[High(Points) - 2] := 0;
  Points[High(Points) - 1] := 0;
  Points[High(Points) - 0] := 1;

  // Set the face point counts

  For I := 0 To (Tris Div 2) - 1 Do FacePointCount[I] := 3;
  For I := 0 To Quads - 1 Do FacePointCount[(Tris Div 2) + I] := 4;
  For I := 0 To (Tris Div 2) - 1 Do FacePointCount[(Tris Div 2) + Quads + I] := 3;

  // Set the quads, defining each face clockwise

  K := PointsAround * 3;
  For I := 0 To QuadRings - 1 Do
  Begin
    For J := 0 To PointsAround - 1 Do
    Begin
      FaceDefs[K + 0] := I * PointsAround + J + 1;
      If J = 0
       Then FaceDefs[K + 1] := FaceDefs[K + 0] + PointsAround - 1
       Else FaceDefs[K + 1] := FaceDefs[K + 0] - 1;
      FaceDefs[K + 2] := FaceDefs[K + 1] + PointsAround;
      FaceDefs[K + 3] := FaceDefs[K + 0] + PointsAround;
      Inc(K,4);
    End; // For J
  End; // For I

  // Set the bottom triangles, defining each face clockwise

  K := 0;
  For I := 0 To PointsAround - 1 Do
  Begin
    FaceDefs[K + 0] := I + 1;
    FaceDefs[K + 1] := 0;
    If I = 0
     Then FaceDefs[K + 2] := FaceDefs[K + 0] + PointsAround - 1
     Else FaceDefs[K + 2] := FaceDefs[K + 0] - 1;
    Inc(K,3);
  End; // For I

  // Set the top triangles, defining each face clockwise

  K := High(FaceDefs) - 3 * PointsAround + 1;
  For I := 0 To PointsAround - 1 Do
  Begin
    FaceDefs[K + 0] := QuadRings * PointsAround + I + 1;
    If I = 0
     Then FaceDefs[K + 1] := FaceDefs[K + 0] + PointsAround - 1
     Else FaceDefs[K + 1] := FaceDefs[K + 0] - 1;
    FaceDefs[K + 2] := High(Points) Div 3;
    Inc(K,3);
  End; // For I

  // Create the object

  GeneratePolyhedron(Points,FacePointCount,FaceDefs);
  CalcNormals(True);
  CalcExtents(False);

  // Cleanup

  SetLength(Points,0);
  SetLength(FacePointCount,0);
  SetLength(FaceDefs,0);
End; // TModel.GenerateSphere

Procedure TModel.GenerateSkySphere;
Const
  ConstLats : Array [0..15] Of Integer = (1,2,3,4,6,8,10,14,18,22,30,40,50,60,70,80);
//  ConstLats : Array [0..25] Of Integer = (1,2,3,4,6,8,10,13,16,20,25,30,35,40,45,50,55,60,65,70,75,80,85,87,88,89);
  LongInc = 10;  // MUST be evenly divisible into 360

Var
  I,J,K          : Integer;
  Quads          : Integer;
  Tris           : Integer;
  PointsAround   : Integer;
  QuadRings      : Integer;
  Lat            : Integer;
  Long           : Integer;
  Lats           : Array Of Integer;
  Points         : Array Of Single;
  FacePointCount : Array Of Integer;
  FaceDefs       : Array Of Integer;

Begin
  // Create the latitudes array

  J := High(ConstLats) + 1;
  SetLength(Lats,2 * J + 1);
  Lats[J] := 0;
  For I := 0 To High(ConstLats) Do
  Begin
    Lats[I] := -ConstLats[J - 1 - I];
    Lats[High(Lats) - I] := ConstLats[J - 1 - I];
  End; // For I

  // Helper variables

  PointsAround := 360 Div LongInc;
  QuadRings    := Max(0,High(Lats));
  Quads        := QuadRings * PointsAround;
  Tris         := 2 * PointsAround;

  // Allocate arrays

  SetLength(Points,3 * ((QuadRings + 1) * PointsAround + 2));          // The first point is the bottom and the last point is the top
  SetLength(FacePointCount,Quads + Tris);
  SetLength(FaceDefs,Quads * 4 + Tris * 3); // First and last sets are triangles, all others are quads

  // Set the bottom point

  Points[0] := 0;
  Points[1] := 0;
  Points[2] := -1;

  // Set the points, bottom to top

  K := 3;
  For I := 0 To QuadRings Do
  Begin
    For J := 0 To PointsAround - 1 Do
    Begin
      Points[K + 0] := Cos(J * LongInc * Pi / 180) * Cos(Lats[I] * Pi / 180);
      Points[K + 1] := Sin(J * LongInc * Pi / 180) * Cos(Lats[I] * Pi / 180);
      Points[K + 2] := Sin(Lats[I] * Pi / 180);
      Inc(K,3);
    End; // For J
  End; // For I

  // Set the top point

  Points[High(Points) - 2] := 0;
  Points[High(Points) - 1] := 0;
  Points[High(Points) - 0] := 1;

  // Set the face point counts

  For I := 0 To (Tris Div 2) - 1 Do FacePointCount[I] := 3;
  For I := 0 To Quads - 1 Do FacePointCount[(Tris Div 2) + I] := 4;
  For I := 0 To (Tris Div 2) - 1 Do FacePointCount[(Tris Div 2) + Quads + I] := 3;

  // Set the quads, defining each face clockwise

  K := PointsAround * 3;
  For I := 0 To QuadRings - 1 Do
  Begin
    For J := 0 To PointsAround - 1 Do
    Begin
      FaceDefs[K + 0] := I * PointsAround + J + 1;
      If J = 0
       Then FaceDefs[K + 1] := FaceDefs[K + 0] + PointsAround - 1
       Else FaceDefs[K + 1] := FaceDefs[K + 0] - 1;
      FaceDefs[K + 2] := FaceDefs[K + 1] + PointsAround;
      FaceDefs[K + 3] := FaceDefs[K + 0] + PointsAround;
      Inc(K,4);
    End; // For J
  End; // For I

  // Set the bottom triangles, defining each face clockwise

  K := 0;
  For I := 0 To PointsAround - 1 Do
  Begin
    FaceDefs[K + 0] := I + 1;
    FaceDefs[K + 1] := 0;
    If I = 0
     Then FaceDefs[K + 2] := FaceDefs[K + 0] + PointsAround - 1
     Else FaceDefs[K + 2] := FaceDefs[K + 0] - 1;
    Inc(K,3);
  End; // For I

  // Set the top triangles, defining each face clockwise

  K := High(FaceDefs) - 3 * PointsAround + 1;
  For I := 0 To PointsAround - 1 Do
  Begin
    FaceDefs[K + 0] := QuadRings * PointsAround + I + 1;
    If I = 0
     Then FaceDefs[K + 1] := FaceDefs[K + 0] + PointsAround - 1
     Else FaceDefs[K + 1] := FaceDefs[K + 0] - 1;
    FaceDefs[K + 2] := High(Points) Div 3;
    Inc(K,3);
  End; // For I

  // Create the object

  GeneratePolyhedron(Points,FacePointCount,FaceDefs);
  CalcNormals(True);
  CalcExtents(False);

  // Cleanup

  SetLength(Points,0);
  SetLength(FacePointCount,0);
  SetLength(FaceDefs,0);
  SetLength(Lats,0);
End; // TModel.GenerateSkySphere

Procedure TModel.GenerateTetrahedron;
Begin
  GeneratePolyhedron([ 1,  1, -1,
                      -1,  1,  1,
                       1, -1,  1,
                      -1, -1, -1],
                     [3, 3, 3, 3],
                     [0, 2, 1,
                      3, 0, 1,
                      2, 3, 1,
                      3, 2, 0]);
End; // TModel.GenerateTetrahedron

Procedure TModel.GenerateCube;
Begin
  GeneratePolyhedron([ 1,   1,   1,
                       1,   1,  -1,
                       1,  -1,   1,
                       1,  -1,  -1,
                      -1,   1,   1,
                      -1,   1,  -1,
                      -1,  -1,   1,
                      -1,  -1,  -1],
                     [4, 4, 4, 4, 4, 4],
                     [6, 4, 0, 2,
                      5, 1, 0, 4,
                      7, 5, 4, 6,
                      1, 3, 2, 0,
                      3, 7, 6, 2,
                      7, 3, 1, 5]);
End; // TModel.GenerateCube

Procedure TModel.GenerateOctahedron;
Begin
  GeneratePolyhedron([ 0,   0,   1,
                       0,   1,   0,
                      -1,   0,   0,
                       1,   0,   0,
                       0,  -1,   0,
                       0,   0,  -1],
                      [3, 3, 3, 3, 3, 3, 3, 3],
                      [1, 0, 2,
                       0, 1, 3,
                       5, 1, 2,
                       1, 5, 3,
                       4, 0, 3,
                       0, 4, 2,
                       4, 5, 2,
                       5, 4, 3]);
End; // TModel.GenerateOctahedron

Procedure TModel.GenerateDodecahedron;
Var A,B,C: Single;
Begin
  A := 1 / Sqrt(3);
  B := Sqrt((3 - Sqrt(5)) / 6);
  C := Sqrt((3 + Sqrt(5)) / 6);

  GeneratePolyhedron([ A,  A,  A,
                       0,  C, -B,
                      -A,  A,  A,
                       0,  C,  B,
                      -C,  B,  0,
                      -A,  A, -A,
                       C, -B,  0,
                       C,  B,  0,
                       A,  A, -A,
                       A, -A,  A,
                       B,  0,  C,
                      -B,  0,  C,
                      -C, -B,  0,
                      -A, -A,  A,
                       0, -C,  B,
                       B,  0, -C,
                       A, -A, -A,
                       0, -C, -B,
                      -B,  0, -C,
                      -A, -A, -A],
                     [5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5],
                     [ 2,  4,  5,  1,  3,
                       0,  7,  6,  9, 10,
                       8, 15, 16,  6,  7,
                       8,  7,  0,  3,  1,
                       5, 18, 15,  8,  1,
                       0, 10, 11,  2,  3,
                       6, 16, 17, 14,  9,
                      11, 13, 12,  4,  2,
                      11, 10,  9, 14, 13,
                      19, 17, 16, 15, 18,
                      19, 12, 13, 14, 17,
                      18,  5,  4, 12, 19]);
End; // TModel.GenerateDodecahedron

Procedure TModel.GenerateIcosahedron;
Var Z: Single;
Begin
  Z := (Sqrt(5) - 1) / 2;
  GeneratePolyhedron([-1,  0,  Z,
                       1,  0,  Z,
                      -1,  0, -Z,
                       1,  0, -Z,
                       0,  Z,  1,
                       0,  Z, -1,
                       0, -Z,  1,
                       0, -Z, -1,
                       Z,  1,  0,
                      -Z,  1,  0,
                       Z, -1,  0,
                      -Z, -1,  0],
                      [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3],
                      [ 4,  8,  1,
                       10,  6,  1,
                        4,  6,  0,
                        6,  4,  1,
                        8,  3,  1,
                        3, 10,  1,
                        4,  9,  8,
                        2,  9,  0,
                        9,  4,  0,
                       11,  6, 10,
                       11,  2,  0,
                        6, 11,  0,
                        5,  3,  8,
                        9,  5,  8,
                        5,  9,  2,
                        3,  7, 10,
                        7, 11, 10,
                       11,  7,  2,
                        7,  5,  2,
                        5,  7,  3]);
End; // TModel.GenerateIcosahedron

Procedure TModel.GenerateTruncatedTetrahedron;
Begin
  GeneratePolyhedron([ 1,   1,  -3,
                       3,   1,  -1,
                       1,   3,  -1,
                      -1,   1,   3,
                      -3,   1,   1,
                      -1,   3,   1,
                       1,  -1,   3,
                       3,  -1,   1,
                       1,  -3,   1,
                      -1,  -1,  -3,
                      -3,  -1,  -1,
                      -1,  -3,  -1],
                      [3, 3, 3, 3, 6, 6, 6, 6],
                      [ 0,  1,  2,
                        3,  4,  5,
                        6,  7,  8,
                        9, 10, 11,
                        0,  2,  5,  4, 10,  9,
                        5,  2,  1,  7,  6,  3,
                        4,  3,  6,  8, 11, 10,
                        1,  0,  9, 11,  8,  7]);
End; // TModel.GenerateTruncatedTetrahedron

Procedure TModel.GenerateTruncatedCube;
Begin
  GeneratePolyhedron([ 0,          0,          1.042011,
                       0.5621693,  0,          0.8773552,
                      -0.4798415,  0.2928932,  0.8773552,
                       0.2569714, -0.5,        0.8773552,
                       0.8773552,  0.2928932,  0.4798415,
                      -0.9014684,  0.2071068,  0.4798415,
                      -0.5962706,  0.7071068,  0.4798415,
                       0.1405423, -0.9142136,  0.4798415,
                       1.017898,   0.2071068, -0.08232778,
                       0.7609261,  0.7071068,  0.08232778,
                      -1.017898,  -0.2071068,  0.08232778,
                      -0.2810846,  1,          0.08232778,
                      -0.2810846, -1,          0.08232778,
                       0.2810846, -1,         -0.08232778,
                       0.9014684, -0.2071068, -0.4798415,
                       0.2810846,  1,         -0.08232778,
                      -0.7609261, -0.7071068, -0.08232778,
                      -0.8773552, -0.2928932, -0.4798415,
                      -0.1405423,  0.9142136, -0.4798415,
                       0.5962706, -0.7071068, -0.4798415,
                       0.4798415, -0.2928932, -0.8773552,
                      -0.5621693,  0,         -0.8773552,
                      -0.2569714,  0.5,       -0.8773552,
                       0,          0,         -1.042011],
                      [3,3,3,3,3,3,3,3,8,8,8,8,8,8],
                      [ 1,  3,  0,
                        5,  6,  2,
                        9,  8,  4,
                       13, 12,  7,
                       16, 17, 10,
                       18, 15, 11,
                       20, 19, 14,
                       23, 22, 21,
                        2,  6, 11, 15,  9,  4,  1,  0,
                        3,  7, 12, 16, 10,  5,  2,  0,
                        4,  8, 14, 19, 13,  7,  3,  1,
                       10, 17, 21, 22, 18, 11,  6,  5,
                        9, 15, 18, 22, 23, 20, 14,  8,
                       13, 19, 20, 23, 21, 17, 16, 12]);
End; // TModel.GenerateTruncatedCube

Procedure TModel.GenerateTruncatedOctahedron;
Begin
  GeneratePolyhedron([ 0,  1,  2,
                       0,  2,  1,
                       1,  0,  2,
                       1,  2,  0,
                       2,  0,  1,
                       2,  1,  0,
                       0, -1,  2,
                       0,  2, -1,
                      -1,  0,  2,
                      -1,  2,  0,
                       2,  0, -1,
                       2, -1,  0,
                       0,  1, -2,
                       0, -2,  1,
                       1,  0, -2,
                       1, -2,  0,
                      -2,  0,  1,
                      -2,  1,  0,
                       0, -1, -2,
                       0, -2, -1,
                      -1,  0, -2,
                      -1, -2,  0,
                      -2,  0, -1,
                      -2, -1,  0],
                      [4,6,6,4,4,6,6,4,6,6,4,6,6,4],
                      [ 7,  3,  1,  9,
                       12,  7,  9, 17, 22, 20,
                        1,  0,  8, 16, 17,  9,
                       11,  4,  5, 10,
                       16, 23, 22, 17,
                       14, 10,  5,  3,  7, 12,
                        4,  2,  0,  1,  3,  5,
                       21, 13, 15, 19,
                       19, 18, 20, 22, 23, 21,
                       14, 18, 19, 15, 11, 10,
                       18, 14, 12, 20,
                       23, 16,  8,  6, 13, 21,
                       15, 13,  6,  2,  4, 11,
                        2,  6,  8,  0]);
End; // TModel.GenerateTruncatedOctahedron

Procedure TModel.GenerateTruncatedDodecahedron;
Begin
  GeneratePolyhedron([ 0,           0,          1.014485,
                       0.3367628,   0,          0.9569589,
                      -0.2902233,   0.1708204,  0.9569589,
                       0.1634681,  -0.2944272,  0.9569589,
                       0.5914332,   0.1708204,  0.806354,
                      -0.5963465,   0.1527864,  0.806354,
                      -0.4230517,   0.4472136,  0.806354,
                       0.1377417,  -0.6,        0.806354,
                       0.8302037,   0.1527864,  0.5626702,
                       0.6667356,   0.4472136,  0.6201961,
                      -0.8014407,  -0.0472136,  0.6201961,
                      -0.3477493,   0.7236068,  0.6201961,
                      -0.06735256, -0.8,        0.6201961,
                       0.2694102,  -0.8,        0.5626702,
                       0.9618722,  -0.0472136,  0.3189863,
                       0.5339072,   0.7236068,  0.4695912,
                      -0.8271671,  -0.3527864,  0.4695912,
                      -0.9599955,  -0.0763932,  0.3189863,
                      -0.3992021,   0.8763932,  0.3189863,
                      -0.09307895,  0.8944272,  0.4695912,
                      -0.3734757,  -0.818034,   0.4695912,
                       0.5081808,  -0.818034,   0.3189863,
                       0.9361459,  -0.3527864,  0.1683814,
                       1.011448,   -0.0763932, -0.0177765,
                       0.4824544,   0.8763932,  0.1683814,
                       0.2436839,   0.8944272,  0.4120653,
                      -0.663699,   -0.6472136,  0.4120653,
                      -1.011448,    0.0763932,  0.0177765,
                      -0.5577569,   0.8472136,  0.0177765,
                      -0.5320305,  -0.8472136,  0.1683814,
                       0.5577569,  -0.8472136, -0.0177765,
                       0.7628511,  -0.6472136,  0.1683814,
                       0.9599955,   0.0763932, -0.3189863,
                       0.5320305,   0.8472136, -0.1683814,
                      -0.9618722,   0.0472136, -0.3189863,
                      -0.9361459,   0.3527864, -0.1683814,
                      -0.7628511,   0.6472136, -0.1683814,
                      -0.5081808,   0.818034,  -0.3189863,
                      -0.4824544,  -0.8763932, -0.1683814,
                       0.3992021,  -0.8763932, -0.3189863,
                       0.8014407,   0.0472136, -0.6201961,
                       0.8271671,   0.3527864, -0.4695912,
                       0.663699,    0.6472136, -0.4120653,
                       0.3734757,   0.818034,  -0.4695912,
                      -0.8302037,  -0.1527864, -0.5626702,
                      -0.2694102,   0.8,       -0.5626702,
                      -0.5339072,  -0.7236068, -0.4695912,
                      -0.2436839,  -0.8944272, -0.4120653,
                       0.09307895, -0.8944272, -0.4695912,
                       0.3477493,  -0.7236068, -0.6201961,
                       0.5963465,  -0.1527864, -0.806354,
                       0.06735256,  0.8,       -0.6201961,
                      -0.6667356,  -0.4472136, -0.6201961,
                      -0.5914332,  -0.1708204, -0.806354,
                      -0.1377417,   0.6,       -0.806354,
                       0.4230517,  -0.4472136, -0.806354,
                       0.2902233,  -0.1708204, -0.9569589,
                      -0.3367628,   0,         -0.9569589,
                      -0.1634681,   0.2944272, -0.9569589,
                       0,           0,         -1.014485],
                      [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                       10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10],
                      [ 1,  3,  0,
                        5,  6,  2,
                        9,  8,  4,
                       13, 12,  7,
                       16, 17, 10,
                       18, 19, 11,
                       23, 22, 14,
                       25, 24, 15,
                       29, 26, 20,
                       31, 30, 21,
                       34, 35, 27,
                       36, 37, 28,
                       41, 40, 32,
                       43, 42, 33,
                       47, 46, 38,
                       49, 48, 39,
                       52, 53, 44,
                       54, 51, 45,
                       56, 55, 50,
                       59, 58, 57,
                        2,  6, 11, 19, 25, 15,  9,  4,  1,  0,
                        3,  7, 12, 20, 26, 16, 10,  5,  2,  0,
                        4,  8, 14, 22, 31, 21, 13,  7,  3,  1,
                       10, 17, 27, 35, 36, 28, 18, 11,  6,  5,
                        9, 15, 24, 33, 42, 41, 32, 23, 14,  8,
                       13, 21, 30, 39, 48, 47, 38, 29, 20, 12,
                       26, 29, 38, 46, 52, 44, 34, 27, 17, 16,
                       28, 37, 45, 51, 43, 33, 24, 25, 19, 18,
                       23, 32, 40, 50, 55, 49, 39, 30, 31, 22,
                       44, 53, 57, 58, 54, 45, 37, 36, 35, 34,
                       41, 42, 43, 51, 54, 58, 59, 56, 50, 40,
                       47, 48, 49, 55, 56, 59, 57, 53, 52, 46]);
End; // TModel.GenerateTruncatedDodecahedron

Procedure TModel.GenerateTruncatedIcosahedron;
Begin
  GeneratePolyhedron([ 0,           0,           1.021,
                       0.4035482,   0,           0.9378643,
                      -0.2274644,   0.3333333,   0.9378643,
                      -0.1471226,  -0.375774,    0.9378643,
                       0.579632,    0.3333333,   0.7715933,
                       0.5058321,  -0.375774,    0.8033483,
                      -0.6020514,   0.2908927,   0.7715933,
                      -0.05138057,  0.6666667,   0.7715933,
                       0.1654988,  -0.6080151,   0.8033483,
                      -0.5217096,  -0.4182147,   0.7715933,
                       0.8579998,   0.2908927,   0.4708062,
                       0.3521676,   0.6666667,   0.6884578,
                       0.7841999,  -0.4182147,   0.5025612,
                      -0.657475,    0.5979962,   0.5025612,
                      -0.749174,   -0.08488134,  0.6884578,
                      -0.3171418,   0.8302373,   0.5025612,
                       0.1035333,  -0.8826969,   0.5025612,
                      -0.5836751,  -0.6928964,   0.4708062,
                       0.8025761,   0.5979962,   0.2017741,
                       0.9602837,  -0.08488134,  0.3362902,
                       0.4899547,   0.8302373,   0.3362902,
                       0.7222343,  -0.6928964,   0.2017741,
                      -0.8600213,   0.5293258,   0.1503935,
                      -0.9517203,  -0.1535518,   0.3362902,
                      -0.1793548,   0.993808,    0.1503935,
                       0.381901,   -0.9251375,   0.2017741,
                      -0.2710537,  -0.9251375,   0.3362902,
                      -0.8494363,  -0.5293258,   0.2017741,
                       0.8494363,   0.5293258,  -0.2017741,
                       1.007144,   -0.1535518,  -0.06725804,
                       0.2241935,   0.993808,    0.06725804,
                       0.8600213,  -0.5293258,  -0.1503935,
                      -0.7222343,   0.6928964,  -0.2017741,
                      -1.007144,    0.1535518,   0.06725804,
                      -0.381901,    0.9251375,  -0.2017741,
                       0.1793548,  -0.993808,   -0.1503935,
                      -0.2241935,  -0.993808,   -0.06725804,
                      -0.8025761,  -0.5979962,  -0.2017741,
                       0.5836751,   0.6928964,  -0.4708062,
                       0.9517203,   0.1535518,  -0.3362902,
                       0.2710537,   0.9251375,  -0.3362902,
                       0.657475,   -0.5979962,  -0.5025612,
                      -0.7841999,   0.4182147,  -0.5025612,
                      -0.9602837,   0.08488134, -0.3362902,
                      -0.1035333,   0.8826969,  -0.5025612,
                       0.3171418,  -0.8302373,  -0.5025612,
                      -0.4899547,  -0.8302373,  -0.3362902,
                      -0.8579998,  -0.2908927,  -0.4708062,
                       0.5217096,   0.4182147,  -0.7715933,
                       0.749174,    0.08488134, -0.6884578,
                       0.6020514,  -0.2908927,  -0.7715933,
                      -0.5058321,   0.375774,   -0.8033483,
                      -0.1654988,   0.6080151,  -0.8033483,
                       0.05138057, -0.6666667,  -0.7715933,
                      -0.3521676,  -0.6666667,  -0.6884578,
                      -0.579632,   -0.3333333,  -0.7715933,
                       0.1471226,   0.375774,   -0.9378643,
                       0.2274644,  -0.3333333,  -0.9378643,
                      -0.4035482,   0,          -0.9378643,
                       0,           0,          -1.021],
                      [5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6],
                      [ 1,  5,  8,  3,  0,
                        6, 13, 15,  7,  2,
                       11, 20, 18, 10,  4,
                       17, 27, 23, 14,  9,
                       19, 29, 31, 21, 12,
                       25, 35, 36, 26, 16,
                       33, 43, 42, 32, 22,
                       34, 44, 40, 30, 24,
                       38, 48, 49, 39, 28,
                       46, 54, 55, 47, 37,
                       50, 57, 53, 45, 41,
                       58, 59, 56, 52, 51,
                        2,  7, 11,  4,  1,  0,
                        3,  9, 14,  6,  2,  0,
                        4, 10, 19, 12,  5,  1,
                        8, 16, 26, 17,  9,  3,
                       12, 21, 25, 16,  8,  5,
                       14, 23, 33, 22, 13,  6,
                       15, 24, 30, 20, 11,  7,
                       18, 28, 39, 29, 19, 10,
                       22, 32, 34, 24, 15, 13,
                       26, 36, 46, 37, 27, 17,
                       20, 30, 40, 38, 28, 18,
                       31, 41, 45, 35, 25, 21,
                       27, 37, 47, 43, 33, 23,
                       39, 49, 50, 41, 31, 29,
                       42, 51, 52, 44, 34, 32,
                       45, 53, 54, 46, 36, 35,
                       40, 44, 52, 56, 48, 38,
                       43, 47, 55, 58, 51, 42,
                       56, 59, 57, 50, 49, 48,
                       57, 59, 58, 55, 54, 53]);
End; // TModel.GenerateTruncatedIcosahedron

Procedure TModel.GenerateRhombicDodecahedron;
Begin
  GeneratePolyhedron([ 0.0,  0.0,  1.0,
                       0.5,  0.5,  0.5,
                      -0.5,  0.5,  0.5,
                      -0.5, -0.5,  0.5,
                       0.5, -0.5,  0.5,
                       1.0,  0.0,  0.0,
                       0.0,  1.0,  0.0,
                      -1.0,  0.0,  0.0,
                       0.0, -1.0,  0.0,
                       0.5,  0.5, -0.5,
                      -0.5,  0.5, -0.5,
                      -0.5, -0.5, -0.5,
                       0.5, -0.5, -0.5,
                       0.0,  0.0, -1.0],
                      [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4],
                      [ 1,  5,  4,  0,
                        2,  6,  1,  0,
                        3,  7,  2,  0,
                        4,  8,  3,  0,
                        6,  9,  5,  1,
                        7, 10,  6,  2,
                        8, 11,  7,  3,
                        5, 12,  8,  4,
                        9, 13, 12,  5,
                       10, 13,  9,  6,
                       11, 13, 10,  7,
                       12, 13, 11,  8]);
End; // TModel.GenerateRhombicDodecahedron

Procedure TModel.GeneratePentakisDodecahedron;
Begin
  GeneratePolyhedron([ 0.2017741,   0.381966,    0.9794321,
                      -0.4292385,  -0.04863268,  0.9794321,
                       0.2017741,  -0.295686,    0.9794321,
                       0.7814061,  -0.04863268,  0.7300256,
                      -0.4035482,   0.591372,    0.758194,
                      -0.2395937,  -0.745356,    0.7300256,
                       0.6703638,   0.591372,    0.536956,
                       0.5086258,  -0.745356,    0.5758839,
                      -0.9219869,   0.254644,    0.4806191,
                       0.09901297,  0.9513673,   0.4806191,
                      -0.7950671,  -0.408628,    0.536956,
                       1.036877,    0.254644,    0.07707085,
                       0.9425591,  -0.408628,    0.1789853,
                      -0.5955095,   0.872678,    0.1723357,
                       0.03687301, -1.026662,    0.1789853,
                      -0.6151351,  -0.872678,    0.07707085,
                       0.6151351,   0.872678,   -0.07707085,
                       0.5955095,  -0.872678,   -0.1723357,
                      -0.9425591,   0.408628,   -0.1789853,
                      -1.036877,   -0.254644,   -0.07707085,
                      -0.03687301,  1.026662,   -0.1789853,
                       0.7950671,   0.408628,   -0.536956,
                       0.9219869,  -0.254644,   -0.4806191,
                      -0.5086258,   0.745356,   -0.5758839,
                      -0.09901297, -0.9513673,  -0.4806191,
                      -0.6703638,  -0.591372,   -0.536956,
                       0.2395937,   0.745356,   -0.7300256,
                       0.4035482,  -0.591372,   -0.758194,
                      -0.7814061,   0.04863268, -0.7300256,
                       0.4292385,   0.04863268, -0.9794321,
                      -0.2017741,   0.295686,   -0.9794321,
                      -0.2017741,  -0.381966,   -0.9794321],
                      [3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                       3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                       3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                       3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                       3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                       3, 3, 3, 3, 3, 3, 3, 3, 3, 3],
                      [ 1,  0,  2,
                        0,  3,  2,
                        1,  4,  0,
                        2,  5,  1,
                        6,  3,  0,
                        7,  2,  3,
                        8,  4,  1,
                        9,  0,  4,
                        7,  5,  2,
                       10,  1,  5,
                        6, 11,  3,
                        0,  9,  6,
                        3, 12,  7,
                        8, 13,  4,
                        1, 10,  8,
                        4, 13,  9,
                        7, 14,  5,
                        5, 15, 10,
                       16, 11,  6,
                       12,  3, 11,
                       16,  6,  9,
                       17,  7, 12,
                       18, 13,  8,
                       19,  8, 10,
                       20,  9, 13,
                       17, 14,  7,
                       15,  5, 14,
                       19, 10, 15,
                       16, 21, 11,
                       11, 22, 12,
                        9, 20, 16,
                       12, 22, 17,
                       18, 23, 13,
                        8, 19, 18,
                       13, 23, 20,
                       17, 24, 14,
                       14, 24, 15,
                       15, 25, 19,
                       26, 21, 16,
                       22, 11, 21,
                       26, 16, 20,
                       27, 17, 22,
                       28, 23, 18,
                       28, 18, 19,
                       26, 20, 23,
                       27, 24, 17,
                       25, 15, 24,
                       28, 19, 25,
                       26, 29, 21,
                       21, 29, 22,
                       22, 29, 27,
                       28, 30, 23,
                       23, 30, 26,
                       27, 31, 24,
                       24, 31, 25,
                       25, 31, 28,
                       30, 29, 26,
                       31, 27, 29,
                       31, 30, 28,
                       30, 31, 29]);
End; // TModel.GeneratePentakisDodecahedron

Procedure TModel.GenerateCuboctahedron;
Begin
  GeneratePolyhedron([ 1,  0,  1,
                       0,  1,  1,
                      -1,  0,  1,
                       0, -1,  1,
                       1,  1,  0,
                      -1,  1,  0,
                      -1, -1,  0,
                       1, -1,  0,
                       1,  0, -1,
                       0,  1, -1,
                      -1,  0, -1,
                       0, -1, -1],
                     [4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3],
                     [ 3,  2,  1,  0,
                       0,  4,  8,  7,
                       1,  5,  9,  4,
                       2,  6, 10,  5,
                       3,  7, 11,  6,
                       8,  9, 10, 11,
                       0,  1,  4,
                       1,  2,  5,
                       2,  3,  6,
                       3,  0,  7,
                       4,  9,  8,
                       5, 10,  9,
                       6, 11, 10,
                       7,  8, 11]);
End; // TModel.GenerateCuboctahedron

Procedure TModel.GeneratePentagonalRotunda;
Begin
  GeneratePolyhedron([-0.2922021821678, -0.3253719115610, -0.8993058455851,
                      -0.4727930623346,  0.1717521333659, -0.6507438231218,
                      -0.7649952445025, -0.3253719115610, -0.5558015788531,
                       0.2922021821682, -0.3253719115609, -0.8993058455850,
                      -0.0000000000000,  0.4789916897556, -0.4971240449269,
                       0.4727930623347,  0.1717521333660, -0.6507438231216,
                      -0.4727930623347,  0.4789916897556, -0.1536197781950,
                      -0.9455861246693, -0.3253719115609, -0.0000000000001,
                       0.7649952445027, -0.3253719115608, -0.5558015788529,
                       0.4727930623346,  0.4789916897557, -0.1536197781947,
                      -0.7649952445028,  0.1717521333659,  0.2485620224632,
                      -0.2922021821680,  0.4789916897556,  0.4021818006582,
                       0.2922021821679,  0.4789916897557,  0.4021818006582,
                      -0.7649952445027, -0.3253719115609,  0.5558015788530,
                       0.9455861246694, -0.3253719115607,  0.0000000000001,
                       0.7649952445026,  0.1717521333661,  0.2485620224635,
                      -0.0000000000000,  0.1717521333661,  0.8043636013164,
                      -0.2922021821681, -0.3253719115607,  0.8993058455850,
                       0.7649952445025, -0.3253719115607,  0.5558015788532,
                       0.2922021821679, -0.3253719115607,  0.8993058455852],
                     [10, 5, 5, 3, 3, 5, 3, 3, 5, 3, 3, 5, 3, 3, 5, 3, 3],
                     [ 7, 13, 17, 19, 18, 14,  8,  3,  0,  2,
                       6,  4,  9, 12, 11,
                      16, 17, 13, 10, 11,
                      12, 16, 11,
                      19, 17, 16,
                      15, 18, 19, 16, 12,
                       9, 15, 12,
                      14, 18, 15,
                       5,  8, 14, 15,  9,
                       4,  5,  9,
                       3,  8,  5,
                       1,  0,  3,  5,  4,
                       6,  1,  4,
                       2,  0,  1,
                      10,  7,  2,  1,  6,
                      11, 10,  6,
                      13,  7, 10]);
End; // TModel.GeneratePentagonalRotunda

Procedure TModel.GeneratePentagonalCupola;
Begin
  GeneratePolyhedron([ 0.3072203524955, -0.1076768650553, -0.9455270211604,
                      -0.3072203524955, -0.1076768650553, -0.9455270211603,
                       0.8043133248687, -0.1076768650553, -0.5843678363586,
                       0.3072203524954,  0.2153537301107, -0.4228525387755,
                      -0.3072203524954,  0.2153537301108, -0.4228525387755,
                      -0.8043133248688, -0.1076768650553, -0.5843678363585,
                       0.9941859447466, -0.1076768650555,  0.0000000000000,
                       0.4970929723734,  0.2153537301107,  0.1615152975830,
                      -0.4970929723733,  0.2153537301107,  0.1615152975831,
                      -0.9941859447466, -0.1076768650553, -0.0000000000000,
                       0.0000000000001,  0.2153537301107,  0.5226744823849,
                       0.8043133248689, -0.1076768650555,  0.5843678363585,
                      -0.8043133248687, -0.1076768650553,  0.5843678363586,
                       0.3072203524955, -0.1076768650555,  0.9455270211604,
                      -0.3072203524954, -0.1076768650554,  0.9455270211604],
                     [10, 5, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3],
                     [ 0,  1,  5,  9, 12, 14, 13, 11,  6,  2,
                       7, 10,  8,  4,  3,
                       1,  0,  3,  4,
                       5,  1,  4,
                       9,  5,  4,  8,
                      12,  9,  8,
                      14, 12,  8, 10,
                      13, 14, 10,
                      11, 13, 10,  7,
                       6, 11,  7,
                       2,  6,  7,  3,
                       0,  2,  3]);
End; // TModel.GeneratePentagonalCupola

Procedure TModel.GenerateSquareCupola;
Begin
  GeneratePolyhedron([-0.9092035593026, -0.1775330380273, -0.3766044452210,
                      -0.9092035593026, -0.1775330380272,  0.3766044452211,
                      -0.3766044452211,  0.3550660760544, -0.3766044452211,
                      -0.3766044452211,  0.3550660760544,  0.3766044452210,
                      -0.3766044452210, -0.1775330380273, -0.9092035593026,
                      -0.3766044452211, -0.1775330380272,  0.9092035593026,
                       0.3766044452211,  0.3550660760544, -0.3766044452210,
                       0.3766044452211,  0.3550660760544,  0.3766044452210,
                       0.3766044452210, -0.1775330380272, -0.9092035593025,
                       0.3766044452211, -0.1775330380271,  0.9092035593026,
                       0.9092035593026, -0.1775330380272, -0.3766044452211,
                       0.9092035593026, -0.1775330380272,  0.3766044452211],
                     [8, 4, 4, 3, 4, 3, 4, 3, 4, 3],
                     [ 0,  1,  5,  9, 11, 10,  8,  4,
                       6,  7,  3,  2,
                       1,  0,  2,  3,
                       5,  1,  3,
                       9,  5,  3,  7,
                      11,  9,  7,
                      10, 11,  7,  6,
                       8, 10,  6,
                       4,  8,  6,  2,
                       0,  4,  2]);
End; // TModel.GenerateSquareCupola

Procedure TModel.GenerateTriangularCupola;
Begin
  GeneratePolyhedron([ 0.4824506406769, -0.2626128657195,  0.8356290217967,
                       0.9649012813541, -0.2626128657195,  0.0000000000001,
                       0.4824506406770,  0.5252257314389,  0.2785430072656,
                      -0.4824506406771, -0.2626128657194,  0.8356290217967,
                      -0.4824506406770,  0.5252257314389,  0.2785430072655,
                       0.4824506406770, -0.2626128657195, -0.8356290217967,
                       0.0000000000000,  0.5252257314389, -0.5570860145312,
                      -0.9649012813540, -0.2626128657194, -0.0000000000001,
                      -0.4824506406770, -0.2626128657195, -0.8356290217968],
                     [6, 3, 4, 3, 4, 3, 4, 3],
                     [1, 5, 8, 7, 3, 0,
                      4, 6, 2,
                      5, 1, 2, 6,
                      8, 5, 6,
                      7, 8, 6, 4,
                      3, 7, 4,
                      0, 3, 4, 2,
                      1, 0, 2]);
End; // TModel.GenerateTriangularCupola

Procedure TModel.GenerateBilunabiRotunda;
Begin
  GeneratePolyhedron([-0.3568208640762,  0.5773494923060,  0.3568221361762,
                      -0.9341706430643,  0.3568235664566,  0.0000011693106,
                      -0.3568195315341,  0.5773506879951, -0.3568205757834,
                       0.3568204361938,  0.5773499696446,  0.3568214298460,
                       0.0000000000000, -0.0000006638879,  0.5773500903746,
                      -0.9341727318502, -0.3568211135382,  0.0000008510871,
                       0.3568217687359,  0.5773489125022, -0.3568212821136,
                      -0.3568221305999, -0.5773506601272,  0.3568225595176,
                       0.9341702812004,  0.3568235664565, -0.0000015109356,
                      -0.0000003618638, -0.0000006638877, -0.5773504319997,
                      -0.3568229112621, -0.5773506601272, -0.3568221204805,
                       0.3568225493982, -0.5773506601272,  0.3568217788554,
                       0.9341723699864, -0.3568211135384, -0.0000011927120,
                       0.3568217687360, -0.5773506601272, -0.3568229011426],
                     [4, 3, 3, 3, 3, 5, 5, 5, 5, 3, 3, 3, 3, 4],
                     [10,  7, 11, 13,
                      12, 13, 11,
                       4, 11,  7,
                       5,  7, 10,
                       9, 10, 13,
                       8,  6,  9, 13, 12,
                       4,  3,  8, 12, 11,
                       1,  0,  4,  7,  5,
                       9,  2,  1,  5, 10,
                       6,  8,  3,
                       0,  3,  4,
                       0,  1,  2,
                       6,  2,  9,
                       3,  0,  2,  6]);
End; // TModel.GenerateBilunabiRotunda

Procedure TModel.GenerateTriangularHebesphenorotunda;
Begin
  GeneratePolyhedron([-0.3303042690173, -0.4356950866333,  0.5721037787695,
                      -0.6606085715265, -0.4356950866334,  0.0000000005532,
                       0.3303042877736, -0.4356950866333,  0.5721037509254,
                      -0.8647480351422, -0.0542928347901,  0.4992625236701,
                      -0.3303043172447, -0.4356950866334, -0.5721038055071,
                       0.6606085420553, -0.4356950866333, -0.0000000551349,
                       0.3303042395462, -0.4356950866334, -0.5721038333512,
                      -0.3303044983204,  0.1814268197426,  0.8078244142174,
                       0.8647480477581, -0.0542928347899,  0.4992624507732,
                       0.3303043673421,  0.1814267250992,  0.8078241649093,
                      -0.8647484780115,  0.1814267250991, -0.1178601375303,
                      -0.0000000568225, -0.0542928347902, -0.9985250563157,
                      -0.5344442610874,  0.1814268197425, -0.6899643218134,
                       0.8647487152011,  0.1814268197427, -0.1178601742766,
                       0.0000000000000,  0.5628296196298,  0.3814033943658,
                       0.5344440664625,  0.1814267250991, -0.6899641092516,
                      -0.3303047999208,  0.5628295611368, -0.1907015563086,
                       0.3303050209540,  0.5628292088785, -0.1907014286944],
                     [5, 3, 3, 3, 4, 3, 5, 3, 3, 3, 3, 4, 3, 5, 3, 3, 3, 4, 3, 6],
                     [15, 17, 16, 12, 11,
                       4, 11, 12,
                       4,  6, 11,
                      15, 11,  6,
                       5, 13, 15,  6,
                      17, 15, 13,
                       9, 14, 17, 13,  8,
                       8, 13,  5,
                       8,  5,  2,
                       9,  8,  2,
                      14,  9,  7,
                       0,  7,  9,  2,
                      17, 14, 16,
                      14,  7,  3, 10, 16,
                       7,  0,  3,
                       0,  1,  3,
                      10,  3,  1,
                      10,  1,  4, 12,
                      16, 10, 12,
                       0,  2,  5,  6,  4,  1]);
End; // TModel.GenerateTriangularHebesphenorotunda

Function TModel.AddFaces(Count: Integer): Integer;
Var I,J: Integer;
Begin
  I := High(Faces) + 1;
  SetLength(Faces,I + Count);
  J := I;
  While J <= High(Faces) Do
  Begin
    Faces[J].Texture := -1;
    Faces[J].Flags   := ffSolid;
    Inc(J);
  End; // While
  FNormals.SetLength((High(Faces) + 1) * 3);
  Result := I;
End; // TModel.AddFaces

Function TModel.AddVertices(Count: Integer): Integer;
Var I: Integer;
Begin
  I := High(Vertices) + 1;
  SetLength(Vertices,I + Count);
  SetLength(Colors,I + Count);
  SetLength(VNormals,I + Count);
  Positions.SetLength((High(Vertices) + 1) * 3);
  Result := I;
End; // TModel.AddVertices

Var SortModel: TModel;

Function SortModelCompare(Index0,Index1: Integer): Integer;
Begin
  Result := SortModel.Faces[Index0].Texture - SortModel.Faces[Index1].Texture;
End; // SortModelCompare

Procedure SortModelExchange(Index0,Index1: Integer);
Var
  I,J,K    : Integer;
  Face     : TFace;
  Quads    : Boolean;
  Normal   : LongWord;
  Vert     : Single;
  C        : TColor;
  TexCoord : TVertex;

Begin
  // Exchange faces

  Face                    := SortModel.Faces[Index0];
  SortModel.Faces[Index0] := SortModel.Faces[Index1];
  SortModel.Faces[Index1] := Face;

  Quads := ((High(SortModel.Faces) + 1) * 12 = SortModel.Positions.Length);

  If Quads Then J := 4 Else J := 3;
  K := J * 3;

  // Exchange vertices

  If SortModel.Positions.Length > 0 Then
  Begin
    For I := 0 To K - 1 Do
    Begin
      Vert := SortModel.Positions.DataArray[Index0 * K + I];
      SortModel.Positions.DataArray[Index0 * K + I] := SortModel.Positions.DataArray[Index1 * K + I];
      SortModel.Positions.DataArray[Index1 * K + I] := Vert;
    End; // For I
  End;

  // Exchange vertex normals

  If High(SortModel.VNormals) >= 0 Then
  Begin
    For I := 0 To J - 1 Do
    Begin
      Normal := SortModel.VNormals[Index0 * J + I];
      SortModel.VNormals[Index0 * J + I] := SortModel.VNormals[Index1 * J + I];
      SortModel.VNormals[Index1 * J + I] := Normal;
    End; // For I
  End;

  // Exchange vertex colors

  If High(SortModel.Colors) >= 0 Then
  Begin
    For I := 0 To J - 1 Do
    Begin
      C := SortModel.Colors[Index0 * J + I];
      SortModel.Colors[Index0 * J + I] := SortModel.Colors[Index1 * J + I];
      SortModel.Colors[Index1 * J + I] := C;
    End; // For I
  End;

  // Exchange texture coordinates

  If High(SortModel.Vertices) >= 0 Then
  Begin
    For I := 0 To J - 1 Do
    Begin
      TexCoord := SortModel.Vertices[Index0 * J + I];
      SortModel.Vertices[Index0 * J + I] := SortModel.Vertices[Index1 * J + I];
      SortModel.Vertices[Index1 * J + I] := TexCoord;
    End; // For I
  End;

  // Exchange face normals

  If SortModel.FNormals.Length > 0 Then
  Begin
    For I := 0 To J - 1 Do
    Begin
      Vert := SortModel.FNormals.DataArray[Index0 * J + I];
      SortModel.FNormals.DataArray[Index0 * J + I] := SortModel.FNormals.DataArray[Index1 * J + I];
      SortModel.FNormals.DataArray[Index1 * J + I] := Vert;
    End; // For I
  End;
End; // SortModelExchange

Procedure TModel.SortFacesByTexture(Quads: Boolean);
Var
  QS      : TQuickSorterProc;
  I,J     : Integer;
  Tex     : Integer;
  LastTex : Integer;
  C       : TColor;

Begin
  // If there are any transparent textures, set the texture ID to -2 so they appear at the beginning

  J := 0;
  For I := 0 To High(Faces) Do
  Begin
    C := 0;
    If Quads Then
    Begin
      C := C Or Colors[J] Or Colors[J + 1] Or Colors[J + 2] Or Colors[J + 3];
      Inc(J,4);
    End
    Else
    Begin
      C := C Or Colors[J] Or Colors[J + 1] Or Colors[J + 2];
      Inc(J,3);
    End;
    If (C And $FF000000) = 0 Then Faces[I].Texture := -2; 
  End; // For I    

  SortModel   := Self;
  QS          := TQuickSorterProc.Create;
  QS.Compare  := @SortModelCompare;
  QS.Exchange := @SortModelExchange;
  QS.Sort(0,High(Faces));
  QS.Free;

  // Set texture counts

  SetLength(TexCounts,0);
  LastTex := -3;
  For I := 0 To High(Faces) Do
  Begin
    Tex := Faces[I].Texture;
    If Tex <> LastTex Then
    Begin
      SetLength(TexCounts,High(TexCounts) + 2);
      TexCounts[High(TexCounts)].Texture := Tex;
      TexCounts[High(TexCounts)].Flags   := 1;
      LastTex := Tex;
    End
    Else If High(TexCounts) >= 0 Then Inc(TexCounts[High(TexCounts)].Flags);
  End; // For I

  // Change -2 texture ID's back to -1

  For I := 0 To High(Faces) Do
  Begin
    If Faces[I].Texture = -2 Then Faces[I].Texture := -1;
  End; // For I
End; // TModel.SortFacesByTexture

Procedure TModel.DoCheckOcclusion;
Var
  Tris      : Boolean;
  FaceIndex : Integer;

Begin
  Tris := ((High(Faces) + 1) * 9  = Positions.Length);
  If Tris And FCheckOcclusion And FOwner.OcclusionManager.Enabled And Not FAnimated Then
  Begin
    If TFVector.isSSE Or TFVector.is3DNow Then FOwner.OcclusionManager.SSE_3DNow_RenderTriangleToZBuffer(Self)
    Else
    Begin
      For FaceIndex := 0 To High(Faces) Do
      Begin
        If ((Faces[FaceIndex].Flags And (ffHasAlpha Or ffHasTrans)) = 0) Then
        Begin
          Faces[FaceIndex].Flags := Faces[FaceIndex].Flags And Not ffHidden;
          FOwner.OcclusionManager.RenderTriangleToZBuffer(SPtr(LongWord(Positions.DataArray) + (FaceIndex * 36)),
                                                          SPtr(LongWord(FNormals.DataArray) + FaceIndex * 12));
        End;
      End; // For FaceIndex
    End;
  End;
End; // TModel.DoCheckOcclusion

Procedure TModel.Redraw(Const VertexColors: Array Of LongWord; AlphaNotOneOnly: Boolean; Frame: TSkeletonFrame;
                        Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList);
// ---------------------------------------------------------------------
// This method must be called from within the scene's render thread
// ---------------------------------------------------------------------
Var
  I,J,K      : Integer;
  Alpha      : Integer;
  A          : Byte;
  Texture    : TTextureSet;
  Quads      : Boolean;
  Tris       : Boolean;
  Tex        : TTexture;
  FaceIndex  : Integer;
  SSE        : Boolean;
  CurTexSet  : Integer;
  TextureSet : TTextureSet;
  UseVBO     : Boolean;

  Procedure CheckTextures;
  // Run through the textures to see if any have any transparency
  Var
    I,J        : Integer;
    CurTexSet  : Integer;
    TextureSet : TTextureSet;
    Tex        : TTexture;

  Begin
    FHasAlpha := False;
    FHasTrans := False;
    FSubst    := TTextureSet(TextureSets.Get(ReplaceTexSetID));
    For I := 0 To High(TexCounts) Do
    Begin
      // If no texture set has been specified, then the faces contain absolute texture indices

      If TextureSets.Count = 0 Then
      Begin
        If (TexCounts[I].Texture >= 0) And (TexCounts[I].Texture < FOwner.Textures.Count)
         Then Tex := TTexture(FOwner.Textures.Objects[0{TexCounts[I].Texture}])
         Else Tex := Nil;
      End
      Else
      Begin
        If TexCounts[I].Texture >= 0 Then
        Begin
          If High(PieceTextures) >= 0
           Then CurTexSet := PieceTextures[0].TextureID
           Else CurTexSet := 0;
          TextureSet := TTextureSet(TextureSets.Get(CurTexSet));
          If TextureSet = Nil Then TextureSet := TTextureSet(TextureSets.Items[0].Value.Ptr);
          J := TextureSet.FInfo[TexCounts[I].Texture].CurIndex;
          If (J >= 0) And (J <= High(TextureSet.FTextures))
           Then Tex := SubstituteTexture(TextureSet.FTextures[J],ReplaceFromTexInfo,ReplaceToTexInfo)
           Else Tex := Nil;
        End
        Else Tex := Nil;
      End;

      If Tex <> Nil Then
      Begin
        If Not Tex.Loaded Then
        Begin
          // It's safe to call this directly because we're already in the render thread

          Tex.LoadTextureIntoOpenGL;
          If Tex.HasAlpha Then Faces[0].Flags := Faces[0].Flags Or ffHasAlpha; // Doesn't matter which one we set
          If Tex.HasTrans Then Faces[0].Flags := Faces[0].Flags Or ffHasTrans;
        End;
        FHasAlpha := FHasAlpha Or Tex.HasAlpha;
        FHasTrans := FHasTrans Or Tex.HasTrans;
        If Tex.HasAlpha Then Faces[0].Flags := Faces[0].Flags Or ffHasAlpha; // Doesn't matter which one we set
        If Tex.HasTrans Then Faces[0].Flags := Faces[0].Flags Or ffHasTrans;
      End;
    End; // For I
  End; // CheckTextures

Begin
  Try
    For I := 0 To TextureSets.Count - 1 Do TTextureSet(TextureSets.Items[I].Value.Ptr).SetCurIndices;
    FLastBoundTextureID := -1;
    J := High(VertexColors) + 1;
    If Not FSetHasAlphaTrans Then
    Begin
      FHasAlpha := False;
      FHasTrans := False;
      I         := 0;
      While (I < J) And Not FHasAlpha Do
      Begin
        FHasAlpha := FHasAlpha Or (TBGRA(VertexColors[I]).A In [1..254]);
        Inc(I);
      End; // While
      I := 0;
      While (I < J) And Not FHasTrans Do
      Begin
        FHasTrans := FHasTrans Or (TBGRA(VertexColors[I]).A = 0);
        Inc(I);
      End; // While
      I := 0;
      While (I <= High(Vertices)) And Not FHasAlpha Do
      Begin
        FHasAlpha := FHasAlpha Or (TBGRA(Colors[I]).A In [1..254]);
        Inc(I);
      End; // While
      I := 0;
      While (I <= High(Vertices)) And Not FHasTrans Do
      Begin
        FHasTrans := FHasTrans Or (TBGRA(Colors[I]).A = 0);
        Inc(I);
      End; // While
      FSetHasAlphaTrans := True;

      For I := 0 To High(Faces) Do
      Begin
        Faces[I].Flags := Faces[I].Flags And Not (ffHasAlpha Or ffHasTrans);
        Alpha             := 0;
        For K := 0 To 2 Do
        Begin
          A    := TBGRA(Colors[I * 3 + K]).A;
          If A In [1..254] Then Faces[I].Flags := Faces[I].Flags Or ffHasAlpha And Not ffHidden;
          If A = 0         Then Faces[I].Flags := Faces[I].Flags Or ffHasTrans And Not ffHidden;
          Alpha := Alpha + A;
        End; // For K
        Faces[I].Flags := (Faces[I].Flags And Not ffAlphaMask) Or (Round(Alpha / 3) And ffAlphaMask);
      End; // For I
    End;
    FAA   := 255;
    FTint := $00FFFFFF;
    glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
    glColor4ub(255,255,255,255);
    If ColorType <> mccAmbientAndDiffuse Then
    Begin
      glColorMaterial(GL_FRONT,GL_EMISSION);
      glColor4ub(255,255,255,255);
    End
    Else
    Begin
      glColorMaterial(GL_FRONT,GL_EMISSION);
      glColor4ub(0,0,0,0);     // No emission
      glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
    End;

    Quads := ((High(Faces) + 1) * 12 = Positions.Length);
    Tris  := ((High(Faces) + 1) * 9  = Positions.Length);
{
    If FCheckOcclusion And
       ((TFVector.isSSE Or TFVector.is3DNow) And FOwner.OcclusionManager.Enabled) And Tris And
       Not AlphaNotOneOnly Then
     FOwner.OcclusionManager.SSE_3DNow_RenderTriangleToZBuffer(Self);
}
    UseVBO := False;
    If {False Then}Tris {And (Positions.Length < 63)} And (FUsingVBOs Or Not FTriedVBOs) And UseVBOExtension And (Frame = Nil) And{ (High(PieceTextures) < 0) And }(High(VertexColors) < 0) And (@glGenBuffersARB <> Nil) And Not (FAnimated Or AlphaNotOneOnly Or FHasAlpha Or FHasTrans) Then
    Begin
      FTriedVBOs := True;
      UseVBO := True;
      If Quads Then J := 4 Else J := 3;

      // Allocate vertices

      If (High(FVBOVertices) < 0) And (Positions.Length > 0) Then
      Begin
        SortFacesByTexture(Quads);
        CheckTextures;
        If Not FHasAlpha Or FHasTrans Then
        Begin
          If (High(TexCounts) >= 1) Or
             ((High(TexCounts) >= 0) And (TexCounts[0].Texture >= -1)) Then
          Begin
            FSkipFirstVBO := (TexCounts[0].Texture < -1);

            SetLength(FVBOVertices,High(TexCounts) + 1);
            K := 0;
            I := 0;
            While (I <= High(FVBOVertices)) And UseVBO Do
            Begin
              FVBOVertices[I] := -1;
              
              // Don't draw textures with ID < -1 as they're transparent

              If TexCounts[I].Texture >= -1 Then
              Begin
                glGenBuffersARB(1, @(FVBOVertices[I]));
                glBindBufferARB(GL_ARRAY_BUFFER_ARB, FVBOVertices[I]);
                glBufferDataARB(GL_ARRAY_BUFFER_ARB, TexCounts[I].Flags * J * SizeOf(Single) * 3, @(Positions.DataArray[K]), GL_STATIC_DRAW_ARB);
                If glGetError = GL_OUT_OF_MEMORY Then
                Begin
                  glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
                  glDeleteBuffersARB(1, @(FVBOVertices[I]));
                  UseVBO := False;
                End
                Else glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
              End;
              Inc(K,TexCounts[I].Flags * J * 3);
              Inc(I);
            End; // While
          End
          Else UseVBO := False;
        End
        Else UseVBO := False;
      End;

      // Allocate texture coordinates

      If (High(Vertices) >= 0) And UseVBO Then
      Begin
        SetLength(FVBOTexCoords,High(TexCounts) + 1);
        K := 0;
        I := 0;
        While (I <= High(FVBOTexCoords)) And UseVBO Do
        Begin
          FVBOTexCoords[I] := -1;

          // Don't draw textures with ID < -1 as they're transparent

          If TexCounts[I].Texture >= -1 Then
          Begin
            glGenBuffersARB(1, @(FVBOTexCoords[I]));
            glBindBufferARB(GL_ARRAY_BUFFER_ARB, FVBOTexCoords[I]);
            glBufferDataARB(GL_ARRAY_BUFFER_ARB, TexCounts[I].Flags * J * SizeOf(TVertex), @(Vertices[K]), GL_STATIC_DRAW_ARB);
            If glGetError = GL_OUT_OF_MEMORY Then
            Begin
              glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
              glDeleteBuffersARB(1, @(FVBOTexCoords[I]));
              UseVBO := False;
            End
            Else glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
          End;
          Inc(K,TexCounts[I].Flags * J);
          Inc(I);
        End; // While
      End;

      // Allocate colors

      If (High(Colors) >= 0) And UseVBO Then
      Begin
        SetLength(FVBOColors,High(TexCounts) + 1);
        K := 0;
        I := 0;
        While (I <= High(FVBOColors)) And UseVBO Do
        Begin
          FVBOColors[I] := -1;

          // Don't draw textures with ID < -1 as they're transparent

          If TexCounts[I].Texture >= -1 Then
          Begin
            glGenBuffersARB(1, @(FVBOColors[I]));
            glBindBufferARB(GL_ARRAY_BUFFER_ARB, FVBOColors[I]);
            glBufferDataARB(GL_ARRAY_BUFFER_ARB, TexCounts[I].Flags * J * SizeOf(TColor), @(Colors[K]), GL_STATIC_DRAW_ARB);
            If glGetError = GL_OUT_OF_MEMORY Then
            Begin
              glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
              glDeleteBuffersARB(1, @(FVBOColors[I]));
              UseVBO := False;
            End
            Else glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
          End;
          Inc(K,TexCounts[I].Flags * J);
          Inc(I);
        End; // While
      End;

      // Allocate normals

      If (High(VNormals) >= 0) And UseVBO Then
      Begin
        SetLength(FVBONormals,High(TexCounts) + 1);
        K := 0;
        I := 0;
        While (I <= High(FVBONormals)) And UseVBO Do
        Begin
          FVBONormals[I] := -1;

          // Don't draw textures with ID < -1 as they're transparent

          If TexCounts[I].Texture >= -1 Then
          Begin
            glGenBuffersARB(1, @(FVBONormals[I]));
            glBindBufferARB(GL_ARRAY_BUFFER_ARB, FVBONormals[I]);
            glBufferDataARB(GL_ARRAY_BUFFER_ARB, TexCounts[I].Flags * J * SizeOf(LongWord), @(VNormals[K]), GL_STATIC_DRAW_ARB);
            If glGetError = GL_OUT_OF_MEMORY Then
            Begin
              glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
              glDeleteBuffersARB(1, @(FVBONormals[I]));
              UseVBO := False;
            End
            Else glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
          End;
          Inc(K,TexCounts[I].Flags * J);
          Inc(I);
        End; // While
      End;
    End;

    If Not UseVBO Then
    Begin
      If High(FVBOVertices)  >= 0 Then
      Begin
        If FSkipFirstVBO
         Then glDeleteBuffersARB(High(FVBOVertices) + 1, @FVBOVertices[0])
         Else glDeleteBuffersARB(High(FVBOVertices) + 0, @FVBOVertices[1]);
        SetLength(FVBOVertices,0);
      End;
      If High(FVBOTexCoords) >= 0 Then
      Begin
        If FSkipFirstVBO
         Then glDeleteBuffersARB(High(FVBOTexCoords) + 1, @FVBOTexCoords[0])
         Else glDeleteBuffersARB(High(FVBOTexCoords) + 0, @FVBOTexCoords[1]);
        SetLength(FVBOTexCoords,0);
      End;
      If High(FVBOColors)    >= 0 Then
      Begin
        If FSkipFirstVBO
         Then glDeleteBuffersARB(High(FVBOColors) + 1, @FVBOColors[0])
         Else glDeleteBuffersARB(High(FVBOColors) + 0, @FVBOColors[1]);
        SetLength(FVBOColors,0);
      End;
      If High(FVBONormals)   >= 0 Then
      Begin
        If FSkipFirstVBO
         Then glDeleteBuffersARB(High(FVBONormals) + 1, @FVBONormals[0])
         Else glDeleteBuffersARB(High(FVBONormals) + 0, @FVBONormals[1]);
        SetLength(FVBONormals,0);
      End;
      FUsingVBOs := False;

      If Quads
       Then RedrawQuads(VertexColors,AlphaNotOneOnly,(ColorType <> mccAmbientAndDiffuse),Frame,PieceTextures,ReplaceFromTexInfo,ReplaceToTexInfo,NearLight,NearDynamicLights)
       Else RedrawTriangles(VertexColors,AlphaNotOneOnly,(ColorType <> mccAmbientAndDiffuse),Frame,PieceTextures,ReplaceFromTexInfo,ReplaceToTexInfo,NearLight,NearDynamicLights);
    End
    Else
    Begin
      FUsingVBOs := True;
      For I := 0 To High(TexCounts) Do
      Begin
        // Don't draw textures with ID < -1 as they're transparent

        If TexCounts[I].Texture >= -1 Then
        Begin
          // If no texture set has been specified, then the faces contain absolute texture indices

          If TextureSets.Count = 0 Then
          Begin
            If (TexCounts[I].Texture >= 0) And (TexCounts[I].Texture < FOwner.Textures.Count)
             Then Tex := TTexture(FOwner.Textures.Objects[TexCounts[I].Texture])
             Else Tex := Nil;
          End
          Else
          Begin
            If TexCounts[I].Texture >= 0 Then
            Begin
              If High(PieceTextures) >= 0
               Then CurTexSet := PieceTextures[0].TextureID
               Else CurTexSet := 0;
              TextureSet := TTextureSet(TextureSets.Get(CurTexSet));
              If TextureSet = Nil Then TextureSet := TTextureSet(TextureSets.Items[0].Value.Ptr);
              J := TextureSet.FInfo[TexCounts[I].Texture].CurIndex;
              If (J >= 0) And (J <= High(TextureSet.FTextures))
               Then Tex := TextureSet.FTextures[J]
               Else Tex := Nil;
            End
            Else Tex := Nil;
          End;

          If Tex <> Nil Then
          Begin
            If Not Tex.Loaded Then
            Begin
              // It's safe to call this directly because we're already in the render thread

              Tex.LoadTextureIntoOpenGL;
              If Tex.HasAlpha Then Faces[0].Flags := Faces[0].Flags Or ffHasAlpha; // Doesn't matter which one we set
              If Tex.HasTrans Then Faces[0].Flags := Faces[0].Flags Or ffHasTrans;
            End;
            FHasAlpha := FHasAlpha Or Tex.HasAlpha;
            FHasTrans := FHasTrans Or Tex.HasTrans;
            If Tex.HasAlpha Then Faces[0].Flags := Faces[0].Flags Or ffHasAlpha; // Doesn't matter which one we set
            If Tex.HasTrans Then Faces[0].Flags := Faces[0].Flags Or ffHasTrans;
            If Not Tex2D Then glEnable(GL_TEXTURE_2D);
            Tex2D  := True;
  //          Manual := Not Texture.Automatic;
            If Tex.ID <> FLastBoundTextureID Then
            Begin
              glBindTexture(GL_TEXTURE_2D, Tex.ID);
              FLastBoundTextureID := Tex.ID;
            End;
          End
          Else
          Begin
            If Tex2D Then glDisable(GL_TEXTURE_2D);
            Tex2D  := False;
  //          Manual := False;
            FLastBoundTextureID := -1;
          End;

          glEnableClientState(GL_VERTEX_ARRAY);

          If FVBONormals[I] >= 0 Then
          Begin
            glEnableClientState(GL_NORMAL_ARRAY);
            glBindBufferARB(GL_ARRAY_BUFFER_ARB, FVBONormals[I]);
            glNormalPointer(GL_BYTE, 1, Nil);
          End;

          If FVBOColors[I] >= 0 Then
          Begin
            glEnableClientState(GL_COLOR_ARRAY);
            glBindBufferARB(GL_ARRAY_BUFFER_ARB, FVBOColors[I]);
            glColorPointer(4, GL_UNSIGNED_BYTE, 0, Nil);
          End;

          If FVBOTexCoords[I] >= 0 Then
          Begin
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            glBindBufferARB(GL_ARRAY_BUFFER_ARB, FVBOTexCoords[I]);
            glTexCoordPointer(2, GL_FLOAT, 0, Nil);
          End;

          glBindBufferARB(GL_ARRAY_BUFFER_ARB, FVBOVertices[I]);
          If Quads
           Then glVertexPointer(4, GL_FLOAT, 0, Nil)
           Else glVertexPointer(3, GL_FLOAT, 0, Nil);

          If Quads
           Then glDrawArrays(GL_QUADS, 0, TexCounts[I].Flags * 4)
           Else glDrawArrays(GL_TRIANGLES, 0, TexCounts[I].Flags * 3);

          glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);

          glDisableClientState(GL_NORMAL_ARRAY);
          glDisableClientState(GL_COLOR_ARRAY);
          glDisableClientState(GL_VERTEX_ARRAY);
          glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        End;
      End; // For I


{      For FaceIndex := 0 To High(Faces) Do
      Begin
        SSE        := TFVector.isSSE;
        If FCheckOcclusion And
           (Not AlphaNotOneOnly) And
           (FOwner.OcclusionManager.Enabled) And
           (Not (SSE Or TFVector.is3DNow))
           And (Not FAnimated)
           And ((Faces[FaceIndex].Flags And (ffHasAlpha Or ffHasTrans)) = 0) Then
        Begin
          Faces[FaceIndex].Flags := Faces[FaceIndex].Flags And Not ffHidden;
          FOwner.OcclusionManager.RenderTriangleToZBuffer(SPtr(LongWord(Positions.DataArray) + (FaceIndex * 36)),
                                                          SPtr(LongWord(FNormals.DataArray) + FaceIndex * 12));
        End;
      End; // For FaceIndex
}      
    End;
{
    If ((TFVector.isSSE Or TFVector.is3DNow) And FOwner.OcclusionManager.Enabled) And
       ((High(Faces) + 1) * 12 <> Positions.Length) And
       Not AlphaNotOneOnly Then
     FOwner.OcclusionManager.SSE_3DNow_RenderTriangleToZBuffer(Self);
}
  Except
  End;
End; // TModel.Redraw

Procedure TModel.CalcExtents(BoxIsOk: Boolean);
Var
  I,J      : Integer;
  X,Y,Z    : Single;
  X1,Y1,Z1 : Single;
  X2,Y2,Z2 : Single;

begin
  If Not BoxIsOk Then
  Begin
    If High(Vertices) >= 0 Then
    Begin
      X1 :=  100000000;
      Y1 :=  100000000;
      Z1 :=  100000000;
      X2 := -100000000;
      Y2 := -100000000;
      Z2 := -100000000;
      J  := 0;
      For I := 0 To High(Vertices) Do
      Begin
        X := Positions.DataArray[J + 0];
        Y := Positions.DataArray[J + 1];
        Z := Positions.DataArray[J + 2];
        If X < X1 Then X1 := X;
        If X > X2 Then X2 := X;
        If Y < Y1 Then Y1 := Y;
        If Y > Y2 Then Y2 := Y;
        If Z < Z1 Then Z1 := Z;
        If Z > Z2 Then Z2 := Z;
        Inc(J,3);
      End; // For I

      // Sanity check to prevent overflows

      If X1 < -100000000 Then X1 := -100000000;
      If Y1 < -100000000 Then Y1 := -100000000;
      If Z1 < -100000000 Then Z1 := -100000000;
      If X2 >  100000000 Then X2 :=  100000000;
      If Y2 >  100000000 Then Y2 :=  100000000;
      If Z2 >  100000000 Then Z2 :=  100000000;
      FBox.Setup(X1,Y1,Z1,X2,Y2,Z2);
    End
    Else FBox.Setup(0,0,0,0,0,0);
  End;
  If BaseHeight < 0 Then BaseHeight := Max(FBox.MaxPt.Z - FBox.MinPt.Z,Max(FBox.MaxPt.X - FBox.MinPt.X,FBox.MaxPt.Y - FBox.MinPt.Y));
  FSphere.Center.Copy(FBox.Center);
  FSphere.Radius := Box.MinPt.DistanceFrom(Box.MaxPt) / 2;
  FCylinder.Setup(Box.MinPt,Box.MaxPt);
End; // TModel.CalcExtents

Procedure TModel.CalcNormals(CalcVertexNormals: Boolean);
var
  I,J,K       : Integer;
  X1,Y1,Z1    : Single;
  X2,Y2,Z2    : Single;
  X3,Y3,Z3    : Single;
  NX,NY,NZ    : Single;
  BNX,BNY,BNZ : ShortInt;
  Len         : Single;

begin
  J := 0;
  K := 0;
  For I := 0 To High(Faces) Do
  Begin
    X1 := Positions.DataArray[J + 6];
    Y1 := Positions.DataArray[J + 7];
    Z1 := Positions.DataArray[J + 8];

    X2 := Positions.DataArray[J + 3];
    Y2 := Positions.DataArray[J + 4];
    Z2 := Positions.DataArray[J + 5];

    X3 := Positions.DataArray[J + 0];
    Y3 := Positions.DataArray[J + 1];
    Z3 := Positions.DataArray[J + 2];

    // Get the face normal

    NX := (Y1 - Y2) * (Z3 - Z2) - (Z1 - Z2) * (Y3 - Y2);
    NY := (Z1 - Z2) * (X3 - X2) - (X1 - X2) * (Z3 - Z2);
    NZ := (X1 - X2) * (Y3 - Y2) - (Y1 - Y2) * (X3 - X2);

    Len := Sqr(NX) + Sqr(NY) + Sqr(NZ);
    If Len <> 0 Then
    Begin
      Len := Sqrt(Len);
      NX  := NX / Len;
      NY  := NY / Len;
      NZ  := NZ / Len;
    End;

    FNormals.DataArray[K]     := NX;
    FNormals.DataArray[K + 1] := NY;
    FNormals.DataArray[K + 2] := NZ;

    // Set the vertex normals to the face normal

    If CalcVertexNormals Then
    Begin
      BNX := Round(NX * 127);
      BNY := Round(NY * 127);
      BNZ := Round(NZ * 127);

      TNormal(VNormals[K]).X := BNX;
      TNormal(VNormals[K]).Y := BNY;
      TNormal(VNormals[K]).Z := BNZ;

      TNormal(VNormals[K + 1]).X := BNX;
      TNormal(VNormals[K + 1]).Y := BNY;
      TNormal(VNormals[K + 1]).Z := BNZ;

      TNormal(VNormals[K + 2]).X := BNX;
      TNormal(VNormals[K + 2]).Y := BNY;
      TNormal(VNormals[K + 2]).Z := BNZ;
    End;

    Inc(J,9);
    Inc(K,3);
  End; // For I
End; // TModel.CalcNormals

Procedure TModel.CalcRoundedNormals;
// This method is highly inefficient and should only be called for low-poly
// models (e.g. those generated by the Generatexxx() polyhedron methods)
Var
  I,J,K,L,M,N : Integer;
  V1,V2,V3    : T3DPoint{I3dPoint};
  V4,V5,V6    : T3DPoint{I3dPoint};
  N1,N2,N3,N4 : T3DPoint{I3dPoint};
  C1,C2,C3    : Integer;

Begin
  CalcNormals(True);
  V1 := T3DPoint.Create{Point};
  V2 := T3DPoint.Create{Point};
  V3 := T3DPoint.Create{Point};
  V4 := T3DPoint.Create{Point};
  V5 := T3DPoint.Create{Point};
  V6 := T3DPoint.Create{Point};
  N1 := T3DPoint.Create{Point};
  N2 := T3DPoint.Create{Point};
  N3 := T3DPoint.Create{Point};
  N4 := T3DPoint.Create{Point};
  J  := 0;
  K  := 0;
  For I := 0 To High(Faces) Do
  Begin
    V1.Copy(Positions.DataArray[J + 0],Positions.DataArray[J + 1],Positions.DataArray[J + 2]);
    V2.Copy(Positions.DataArray[J + 3],Positions.DataArray[J + 4],Positions.DataArray[J + 5]);
    V3.Copy(Positions.DataArray[J + 6],Positions.DataArray[J + 7],Positions.DataArray[J + 8]);
    N1.Copy(FNormals.DataArray[K + 0],FNormals.DataArray[K + 1],FNormals.DataArray[K + 2]);
    N2.Copy(N1);
    N3.Copy(N1);
    C1 := 1;
    C2 := 1;
    C3 := 1;
    For L := 0 To High(Faces) Do
    Begin
      If L <> I Then
      Begin
        M := L * 9;
        N := L * 3;
        V4.Copy(Positions.DataArray[M + 0],Positions.DataArray[M + 1],Positions.DataArray[M + 2]);
        V5.Copy(Positions.DataArray[M + 3],Positions.DataArray[M + 4],Positions.DataArray[M + 5]);
        V6.Copy(Positions.DataArray[M + 6],Positions.DataArray[M + 7],Positions.DataArray[M + 8]);
        N4.Copy(FNormals.DataArray[N + 0],FNormals.DataArray[N + 1],FNormals.DataArray[N + 2]);
        If V4.Equals(V1) Or V5.Equals(V1) Or V6.Equals(V1) Then
        Begin
          N1.Add(N4);
          Inc(C1);
        End;
        If V4.Equals(V2) Or V5.Equals(V2) Or V6.Equals(V2) Then
        Begin
          N2.Add(N4);
          Inc(C2);
        End;
        If V4.Equals(V3) Or V5.Equals(V3) Or V6.Equals(V3) Then
        Begin
          N3.Add(N4);
          Inc(C3);
        End;
      End;
    End; // For L
    N1.Divide(C1);
    N2.Divide(C2);
    N3.Divide(C3);
    N1.Normalize;
    N2.Normalize;
    N3.Normalize;
    TNormal(VNormals[K + 0]).X := Round(N1.X * 127);
    TNormal(VNormals[K + 0]).Y := Round(N1.Y * 127);
    TNormal(VNormals[K + 0]).Z := Round(N1.Z * 127);

    TNormal(VNormals[K + 1]).X := Round(N2.X * 127);
    TNormal(VNormals[K + 1]).Y := Round(N2.Y * 127);
    TNormal(VNormals[K + 1]).Z := Round(N2.Z * 127);

    TNormal(VNormals[K + 2]).X := Round(N3.X * 127);
    TNormal(VNormals[K + 2]).Y := Round(N3.Y * 127);
    TNormal(VNormals[K + 2]).Z := Round(N3.Z * 127);
    Inc(J,9);
    Inc(K,3);
  End; // For I

  N4.Free;
  N3.Free;
  N2.Free;
  N1.Free;
  V6.Free;
  V5.Free;
  V4.Free;
  V3.Free;
  V2.Free;
  V1.Free;
{
  N4 := Nil;
  N3 := Nil;
  N2 := Nil;
  N1 := Nil;
  V6 := Nil;
  V5 := Nil;
  V4 := Nil;
  V3 := Nil;
  V2 := Nil;
  V1 := Nil;
}  
End; // TModel.CalcRoundedNormals

Function TModel.SubstituteTexture(Const Tex: TTexture; Const ReplaceFromTexInfo,ReplaceToTexInfo: String): TTexture;
Var
  I,J   : Integer;
  Tex1  : TTexture;
  Found : Boolean;

Begin
  Result := Tex;
  If (Tex <> Nil) And (ReplaceFromTexInfo <> '') And (ReplaceToTexInfo <> '') And (Tex.TexInfo = ReplaceFromTexInfo) Then
  Begin
    If FSubst <> Nil Then
    Begin
      // Look for a texture with the matching info string

      I     := 0;
      Found := False;
      While (I < FSubst.NumTextures) And Not Found Do
      Begin

        J := FSubst.FInfo[I].CurIndex;
        If (J >= 0) And (J <= High(FSubst.FTextures))
         Then Tex1 := FSubst.FTextures[J]
         Else Tex1 := Nil;

//        Tex1 := Subst.GetTexture(I);
        If (Tex1 <> Nil) And (Tex1.TexInfo = ReplaceToTexInfo) Then
        Begin
          Result := Tex1;
          Found  := True;
        End
        Else Inc(I);
      End; // While
    End;
  End;
End; // TModel.SubstituteTexture

Function TModel.FindNearestDynamicLight(NearDynamicLights: TStringList; Vertex: PGLFloat): TDynamicLight;
Var
  I            : Integer;
  DynamicLight : TDynamicLight;
  MinDist2     : Single;
  Dist2        : Single;
  X,Y,Z        : Single;

Begin
  Result   := Nil;
  MinDist2 := 0;
  X        := PSingle(Vertex)^;
  Y        := PSingle(LongWord(Vertex) + 4)^;
  Z        := PSingle(LongWord(Vertex) + 8)^;
  For I := 0 To NearDynamicLights.Count - 1 Do
  Begin
    DynamicLight := TDynamicLight(NearDynamicLights.Objects[I]);
    Dist2        := DynamicLight.Sphere.Center.DistanceFrom2(X,Y,Z);
    If Dist2 < Sqr(DynamicLight.Sphere.Radius) Then
    Begin
      If Result = Nil Then
      Begin
        Result   := DynamicLight;
        MinDist2 := Dist2;
      End
      Else If Dist2 < MinDist2 Then
      Begin
        Result   := DynamicLight;
        MinDist2 := Dist2;
      End;
    End;
  End; // For I
End; // TModel.FindNearestDynamicLight

Procedure TModel.RedrawTriangles(Const VertexColors: Array Of LongWord; AlphaNotOneOnly,Emissive: Boolean; Frame: TSkeletonFrame;
                                 Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList);
Const MaxDynamicLights = 3;
Type
  LPtr = ^LongWord;
  TTints = Packed Record
    T0,T1,T2 : LongWord;
  End;

Var
  Manual          : Boolean;
  Texture         : TTexture;
  Vert            : Integer;
  Verts           : PGLFloat;
  Norms           : PGLByte;
  FaceIndex       : Integer;
  NumVertexColors : Integer;
  PColors         : Pointer;
  PVertexColors   : Pointer;
  PTexCoords      : Pointer;
  PColor          : Pointer;
  NumTexSets      : Integer;
  CurPiece        : Integer;
  I0,I1,I2        : Integer;
  CurTexSet       : Integer;
  LastTexSet      : Integer;
  ContainsCurSet  : Boolean;
  Tints           : TTints;
  SSE             : Boolean;
  TextureSet      : TTextureSet;
  LastTextureSet  : TTextureSet;
  C0,C1,C2,C3     : LongWord;
  PMultAlpha      : Pointer;
  CheckHidden     : Boolean;
  I,J             : Integer;
  MustBegin       : Boolean;
  DynamicLight    : TDynamicLight;
  Light           : TLight;

  Procedure MoveToCorrectPiece(PieceIndex: Integer);
  Begin

            If Not MustBegin Then
            Begin
              glEnd;
              MustBegin := True;
            End;

    If CurPiece < 0 Then glPushMatrix Else
    Begin
      glPopMatrix;
      glPushMatrix;
    End;
    CurPiece := PieceIndex;
    glTranslatef(Frame.Pieces[CurPiece].TX1,Frame.Pieces[CurPiece].TY1,Frame.Pieces[CurPiece].TZ1);
    glRotatef(Frame.Pieces[CurPiece].Angle1,Frame.Pieces[CurPiece].AX1,Frame.Pieces[CurPiece].AY1,Frame.Pieces[CurPiece].AZ1);
  End; // MoveToCorrectPiece
{
  Procedure DoColor(Color,Tint: LongWord);
  Var R,G,B: Byte;
  Begin
    R := MultAlpha[TBGRA(Color).R * 256 + TBGRA(Tint).R];
    G := MultAlpha[TBGRA(Color).G * 256 + TBGRA(Tint).G];
    B := MultAlpha[TBGRA(Color).B * 256 + TBGRA(Tint).B];
    glColor4ub(R,G,B,TBGRA(Color).A);
  End; // DoColor
}
  Procedure RenderSpanningTriangle;
  Var
    I,J      : Integer;
    X,Y,Z    : Single;
    X1,Y1,Z1 : Single;
    NX0,NY0,NZ0 : Single;
    NX,NY,NZ : Single;
    PSP      : PSkeletonPiece;

  Begin

    If Not MustBegin Then
    Begin
      glEnd;
      MustBegin := True;
    End;

    If CurPiece >= 0 Then glPopMatrix;
    glBegin(GL_TRIANGLES);//POLYGON);
    MustBegin := False;
    For I := 0 To 2 Do
    Begin
      X  := PGLFloat(LongWord(Verts))^;
      Y  := PGLFloat(LongWord(Verts) + 4)^;
      Z  := PGLFloat(LongWord(Verts) + 8)^;

      J  := PieceIndices[Vert + I];

      If (J >= 0) And (J <= High(Frame.Pieces)) Then
      Begin
        PSP := @(Frame.Pieces[J]);

        // Get the transformed vertex

        X1 := PSP.M1[1,1] * X + PSP.M1[1,2] * Y + PSP.M1[1,3] * Z + PSP.TX1;
        Y1 := PSP.M1[2,1] * X + PSP.M1[2,2] * Y + PSP.M1[2,3] * Z + PSP.TY1;
        Z1 := PSP.M1[3,1] * X + PSP.M1[3,2] * Y + PSP.M1[3,3] * Z + PSP.TZ1;

        // Get the transformed normal

        NX0 := PNormal(Norms).X / 127;
        NY0 := PNormal(Norms).Y / 127;
        NZ0 := PNormal(Norms).Z / 127;

        NX := PSP.M1[1,1] * NX0 + PSP.M1[1,2] * NY0 + PSP.M1[1,3] * NZ0;
        NY := PSP.M1[2,1] * NX0 + PSP.M1[2,2] * NY0 + PSP.M1[2,3] * NZ0;
        NZ := PSP.M1[3,1] * NX0 + PSP.M1[3,2] * NY0 + PSP.M1[3,3] * NZ0;

        Try
          If NearLight <> 0 Then
          Begin
            If (NearLight <> C0) Or (Tints.T0 <> C1) Then
            Begin
              C0 := LongWord(NearLight);
              C1 := Tints.T0;
              DoColor(C0,C1);
            End;
            Inc(LongWord(PColor),4);
            If Manual Then // Manual can only be true if there is a texture
            Begin
              {glColor4ubv(@NearLight);}//DoColor(NearLight,Tints.T0);        Inc(LongWord(PColor),4);
              glNormal3f(NX,NY,NZ);{glNormal3bv(Norms);}             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3f(X1,Y1,Z1);           Inc(LongWord(Verts),12);
            End
            Else
            Begin
              {glColor4ubv(@NearLight);}//DoColor(NearLight,Tints.T0);        Inc(LongWord(PColor),4);
              glNormal3f(NX,NY,NZ);{glNormal3bv(Norms);}             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3f(X1,Y1,Z1);           Inc(LongWord(Verts),12);
            End;
          End
          Else
          Begin
            If (LPtr(PColor)^ <> C0) Or (Tints.T0 <> C1) Then
            Begin
              C0 := LPtr(PColor)^;
              C1 := Tints.T0;
              DoColor(C0,C1);
            End;
            Inc(LongWord(PColor),4);
            If Manual Then // Manual can only be true if there is a texture
            Begin
              {glColor4ubv(PColor);}//DoColor(LPtr(PColor)^,Tints.T0);            Inc(LongWord(PColor),4);
              glNormal3f(NX,NY,NZ);{glNormal3bv(Norms);}             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3f(X1,Y1,Z1);           Inc(LongWord(Verts),12);
            End
            Else
            Begin
              {glColor4ubv(PColor);}//DoColor(LPtr(PColor)^,Tints.T0);            Inc(LongWord(PColor),4);
              glNormal3f(NX,NY,NZ);{glNormal3bv(Norms);}             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3f(X1,Y1,Z1);           Inc(LongWord(Verts),12);
            End;
          End;
        Except
        End;
      End
      Else Raise Exception.Create('RenderSpanningTriangle: piece index ' + IntToStr(PieceIndices[Vert + I]) + ' is out of the range [0..' + IntToStr(High(Frame.Pieces)) + ']');
    End; // For I
    glEnd;
    If CurPiece >= 0 Then
    Begin
      glPushMatrix;
      MoveToCorrectPiece(CurPiece);

      glBegin(GL_TRIANGLES);//POLYGON);
      MustBegin := False;
    End;
  End; // RenderSpanningTriangle

  Procedure SetShaderLightCountParameter(Count: Integer);
  // Updates the shader parameters
  Var
    IUniform : Integer;
    St       : String;
    NUniform : Packed Array [0..31] Of Char;

  Begin
    If FOwner.ShaderManager.IsAvailable And FOwner.ShaderManager.Enabled Then
    Begin
      St       := 'LightCount' + #0;
      StrPCopy(NUniform,St);
      IUniform := glGetUniformLocationARB(FOwner.ShaderManager.CurrentProgramHandle, @NUniform[0]);
      If (IUniform <> GL_INVALID_VALUE) And (IUniform <> GL_INVALID_OPERATION) And (IUniform <> -1) Then glUniform1iARB(IUniform,Count);
    End;
  End; // SetShaderLightCountParameter

Begin
  SSE         := TFVector.isSSE;
  Verts       := PGLFloat(Positions.DataArray);
  Norms       := PGLByte(@VNormals[0]);
  PColors     := @(Colors[0]);
  PTexCoords  := @(Vertices[0]);
  NumVertexColors := High(VertexColors) + 1;
  If NumVertexColors > 0
   Then PVertexColors := @(VertexColors[0])
   Else PVertexColors := Nil;
  Vert        := 0;
  NumTexSets  := TextureSets.Count;
  CurPiece    := -1;
  I0          := 0;
  I1          := 0;
  I2          := 0;
  Manual      := False;
  C0          := $FFFFFFFF;
  C1          := $FFFFFFFF;
  C2          := $FFFFFFFF;
  C3          := $FFFFFFFF;
  PMultAlpha  := @(MultAlpha[0]);
  CheckHidden := FOwner.CheckHidden;
  LastTexSet  := -1;
  ContainsCurSet := False;
  LastTextureSet := Nil;
  FSubst         := TTextureSet(TextureSets.Get(ReplaceTexSetID));




              // If we are using dynamic lighting, find the nearest one

              If NearDynamicLights <> Nil Then
              Begin
                // First set the data for static lights
{
                J := 0;

                For I := 0 To FOwner.Lights.Count - 1 Do
                Begin
                  Light := TLight(FOwner.Lights.Items[I]);
                  If Light <> Nil Then
                  Begin
                    Light.DynamicLight.FOwner := FOwner;
                    Light.DynamicLight.Sphere.Center.Copy(Light.Position);
                    Light.DynamicLight.Sphere.Radius := 100000;
                    Light.DynamicLight.Color[0] := Light.FDiffuse[0];
                    Light.DynamicLight.Color[1] := Light.FDiffuse[1];
                    Light.DynamicLight.Color[2] := Light.FDiffuse[2];
                    Light.DynamicLight.Color[3] := Light.FDiffuse[3];
                    Light.DynamicLight.SetShaderParameter(J);
                    Inc(J);
                  End;
                End; // For I
}
                // Set the light count now that we know how many static lights we have

                SetShaderLightCountParameter(Min(MaxDynamicLights,NearDynamicLights.Count));

                // Set the data for the dynamic lights

                For I := 0 To Min(MaxDynamicLights,NearDynamicLights.Count) - 1 Do
                Begin
                  DynamicLight := TDynamicLight(NearDynamicLights.Objects[I]);
                  If DynamicLight <> Nil Then DynamicLight.SetShaderParameter(I);
                End; // For I
              End
              Else SetShaderLightCountParameter(0);



        glBegin(GL_TRIANGLES);//POLYGON);
        MustBegin := False;



  For FaceIndex := 0 To High(Faces) Do
  Begin

//MustBegin := False;

{
    If FCheckOcclusion And
       (Not AlphaNotOneOnly) And
       (FOwner.OcclusionManager.Enabled) And
       (Not (SSE Or TFVector.is3DNow))
       And (Not FAnimated)
       And ((Faces[FaceIndex].Flags And (ffHasAlpha Or ffHasTrans)) = 0) Then
    Begin
      Faces[FaceIndex].Flags := Faces[FaceIndex].Flags And Not ffHidden;
      FOwner.OcclusionManager.RenderTriangleToZBuffer(SPtr(LongWord(Positions.DataArray) + (FaceIndex * 36)),
                                                      SPtr(LongWord(FNormals.DataArray) + FaceIndex * 12));
    End;
}
    If ((Not CheckHidden) Or ((Faces[FaceIndex].Flags And ffHidden) = 0)) And
       (((Faces[FaceIndex].Flags And ffHasAlpha) <> 0) Or Not AlphaNotOneOnly) Then
    Begin
      Tints.T0 := $00FFFFFF;
      Tints.T1 := $00FFFFFF;
      Tints.T2 := $00FFFFFF;
      If (Faces[FaceIndex].Texture >= 0) {And (NumTexSets > 0)} Then
      Begin
        If Frame <> Nil Then
        Begin
          I0 := PieceIndices[Vert];
          If (I0 >= 0) And (I0 <= High(PieceTextures)) Then
          Begin
            I0       := PieceTextures[I0].TextureID;
            Tints.T0 := PieceTextures[I0].TextureTint And $00FFFFFF;
          End
          Else If High(PieceTextures) >= 0 Then I0 := PieceTextures[0].TextureID
          Else I0 := 0;

          I1 := PieceIndices[Vert + 1];
          If (I1 >= 0) And (I1 <= High(PieceTextures)) Then
          Begin
            I1       := PieceTextures[I1].TextureID;
            Tints.T1 := PieceTextures[I1].TextureTint And $00FFFFFF;
          End
          Else If High(PieceTextures) >= 0 Then I1 := PieceTextures[0].TextureID
          Else I1 := 0;

          I2 := PieceIndices[Vert + 2];
          If (I2 >= 0) And (I2 <= High(PieceTextures)) Then
          Begin
            I2       := PieceTextures[I2].TextureID;
            Tints.T2 := PieceTextures[I2].TextureTint And $00FFFFFF;
          End
          Else If High(PieceTextures) >= 0 Then I2 := PieceTextures[0].TextureID
          Else I2 := 0;

          If I1 > I0 Then
          Begin
            I0       := I1;
            Tints.T0 := Tints.T1;
          End;
          If I2 > I0 Then
          Begin
            I0       := I2;
            Tints.T0 := Tints.T2;
          End;
          Tints.T1 := Tints.T0;
          Tints.T2 := Tints.T0;
//          I0 := Max(Max(I0,I1),I2);
          CurTexSet := I0;
        End
        Else
        Begin
          If High(PieceTextures) >= 0
           Then CurTexSet := PieceTextures[0].TextureID
           Else CurTexSet := 0;
        End;
        If CurTexSet <> LastTexSet Then
        Begin
          If CurTexSet >= 0
           Then ContainsCurSet := TextureSets.ContainsKey(CurTexSet)
           Else ContainsCurSet := False;
        End;
        If (CurTexSet < 0) Or Not ContainsCurSet Then
        Begin
          If NumTexSets > 0
           Then CurTexSet := TextureSets.Items[0].Key.Int
           Else CurTexSet := 0;
          Tints.T0  := $00FFFFFF;
          Tints.T1  := $00FFFFFF;
          Tints.T2  := $00FFFFFF;
        End;

        // If no texture set has been specified, then the faces contain absolute texture indices

        If NumTexSets = 0 Then
        Begin
          If (Faces[FaceIndex].Texture >= 0) And (Faces[FaceIndex].Texture < FOwner.Textures.Count)
           Then Texture := TTexture(FOwner.Textures.Objects[Faces[FaceIndex].Texture])
           Else Texture := Nil;
        End
        Else
        Begin
          If CurTexSet <> LastTexSet
           Then TextureSet := TTextureSet(TextureSets.Get(CurTexSet))
           Else TextureSet := LastTextureSet;
          If TextureSet = Nil Then TextureSet := TTextureSet(TextureSets.Items[0].Value.Ptr);
          I := TextureSet.FInfo[Faces[FaceIndex].Texture].CurIndex;

          // After we've chosen the right texture from the right texture set, substitute textures based on additional info (e.g. for player faces)

          If (I >= 0) And (I <= High(TextureSet.FTextures))
           Then Texture := SubstituteTexture(TextureSet.FTextures[I],ReplaceFromTexInfo,ReplaceToTexInfo)
           Else Texture := Nil;
        End;

        // Save the current texture set info

        LastTextureSet := TextureSet;
        LastTexSet     := CurTexSet;

        If Texture <> Nil Then
        Begin
          If Not Texture.Loaded Then
          Begin
            // It's safe to call this directly because we're already in the render thread

            If Not MustBegin Then
            Begin
              glEnd;
              MustBegin := True;
            End;

            Texture.LoadTextureIntoOpenGL;
            If Texture.HasAlpha Then Faces[FaceIndex].Flags := Faces[FaceIndex].Flags Or ffHasAlpha;
            If Texture.HasTrans Then Faces[FaceIndex].Flags := Faces[FaceIndex].Flags Or ffHasTrans;
          End;
          FHasAlpha := FHasAlpha Or Texture.HasAlpha;
          FHasTrans := FHasTrans Or Texture.HasTrans;
          If Texture.HasAlpha Then Faces[FaceIndex].Flags := Faces[FaceIndex].Flags Or ffHasAlpha;
          If Texture.HasTrans Then Faces[FaceIndex].Flags := Faces[FaceIndex].Flags Or ffHasTrans;
          If Not Tex2D Then
          Begin

            If Not MustBegin Then
            Begin
              glEnd;
              MustBegin := True;
            End;

            glEnable(GL_TEXTURE_2D);
          End;
          Tex2D  := True;
          Manual := Not Texture.Automatic;
          If Texture.ID <> FLastBoundTextureID Then
          Begin

            If Not MustBegin Then
            Begin
              glEnd;
              MustBegin := True;
            End;

            glBindTexture(GL_TEXTURE_2D, Texture.ID);
            Texture.SetShaderParameter;
            FLastBoundTextureID := Texture.ID;
          End;
        End
        Else
        Begin
          If Tex2D Then
          Begin

            If Not MustBegin Then
            Begin
              glEnd;
              MustBegin := True;
            End;

            glDisable(GL_TEXTURE_2D);
          End;
          Tex2D  := False;
          Manual := False;
          FLastBoundTextureID := -1;
        End;
      End
      Else
      Begin
        If Tex2D Then
        Begin

          If Not MustBegin Then
          Begin
            glEnd;
            MustBegin := True;
          End;

          glDisable(GL_TEXTURE_2D);
        End;
        Tex2D  := False;
        Manual := False;
        FLastBoundTextureID := -1;
      End;

      // When using emissive color in OpenGL, we can't change alpha transparency.  The engine deals with this
      // by rendering in two passes -- first it renders opaque elements and then it renders non-opaque elements.
      // When rendering non-opaque elements, we change the alpha by temporarily changing to ambient/diffuse
      // color mode.

      If Emissive Then
      Begin
        If ((Faces[FaceIndex].Flags And ffAlphaMask) <> FAA) Or
           (Tints.T0 <> FTint) Then
        Begin

          If Not MustBegin Then
          Begin
            glEnd;
            MustBegin := True;
          End;

          glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
          FAA := Faces[FaceIndex].Flags And ffAlphaMask;
          glColor4ub(TBGRA(Tints.T0).R,TBGRA(Tints.T0).G,TBGRA(Tints.T0).B,FAA);

//          glColor4ub(255,255,255,FAA);

//          If Tint0 <> $00FFFFFF
//           Then glColor4ub(TBGRA(Tint0).R,TBGRA(Tint0).G,TBGRA(Tint0).B,FAA)
//           Else glColor4ub(255,255,255,FAA);
{
          glColorMaterial(GL_FRONT,GL_DIFFUSE);
          glColor4ub(TBGRA(Tint0).R,TBGRA(Tint0).G,TBGRA(Tint0).B,FAA);
}
          glColorMaterial(GL_FRONT,GL_EMISSION);
  C0         := 0;//$FFFFFFFF;
  C1         := 0;//$FFFFFFFF;
  C2         := 0;//$FFFFFFFF;
  C3         := 0;//$FFFFFFFF;
          glColor4ub(0,0,0,0);

          FTint := Tints.T0;
        End;
      End;

      If Vert + 3 <= NumVertexColors
       Then PColor := PVertexColors
       Else PColor := PColors;

      If Frame <> Nil Then
      Begin
        I0 := PieceIndices[Vert];
        I1 := PieceIndices[Vert + 1];
        I2 := PieceIndices[Vert + 2];
      End;

      If (Frame <> Nil) And ((I0 <> I1) Or (I0 <> I2) Or (I1 <> I2)) Then RenderSpanningTriangle
      Else
      Begin
        If (Frame <> Nil) And (CurPiece <> I0) Then MoveToCorrectPiece(I0);

        If MustBegin Then glBegin(GL_TRIANGLES);//POLYGON);
        MustBegin := False;
        Try
          If (NearLight <> 0) Then
          Begin
            If (LongWord(NearLight) <> C0) Or (Tints.T0 <> C1) Then
            Begin
              C0 := LongWord(NearLight);
              C1 := Tints.T0;
              Asm
                PUSH  EBX

                MOV   EAX,C0
                MOV   ECX,C1
                MOV   EBX,PMultAlpha
                SUB   EDX,EDX

                // Red

                MOV   DL,CL
                MOV   DH,AL
                MOV   AL,BYTE PTR [EBX+EDX]

                // Green

                SHR   ECX,8
                ROR   EAX,8

                MOV   DL,CL
                MOV   DH,AL
                MOV   AL,BYTE PTR [EBX+EDX]

                // Blue

                SHR   ECX,8
                ROR   EAX,8

                MOV   DL,CL
                MOV   DH,AL
                MOV   AL,BYTE PTR [EBX+EDX]

                ROL   EAX,16
                MOV   C3,EAX

                POP   EBX
              End; // Asm
              glColor4ubv(@C3);
//              DoColor(C0,C1);
            End;

            If Manual Then // Manual can only be true if there is a texture
            Begin
              {glColor4ubv(@NearLight);}{DoColor(NearLight,Tints.T0);}        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              {glColor4ubv(@NearLight);}{DoColor(NearLight,Tints.T0);}        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              {glColor4ubv(@NearLight);}{DoColor(NearLight,Tints.T0);}        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);
            End
            Else
            Begin
              {glColor4ubv(@NearLight);}{DoColor(NearLight,Tints.T0);}        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              {glColor4ubv(@NearLight);}{DoColor(NearLight,Tints.T0);}        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              {glColor4ubv(@NearLight);}{DoColor(NearLight,Tints.T0);}        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);
            End;
          End
          Else
          Begin
            // NearLight = 0 or we are using dynamic lights if we are here

            If Manual Then // Manual can only be true if there is a texture
            Begin
              C2 := LPtr(PColor)^;
              If (C2 <> C0) Or (Tints.T0 <> C1) Then
              Begin
                C0 := C2;
                C1 := Tints.T0;
                Asm
                  PUSH  EBX

                  MOV   EAX,C0
                  MOV   ECX,C1
                  MOV   EBX,PMultAlpha
                  SUB   EDX,EDX

                  // Red

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Green

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Blue

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  ROL   EAX,16
                  MOV   C3,EAX

                  POP   EBX
                End; // Asm
                glColor4ubv(@C3);
//                DoColor(C0,C1);
              End;

              Inc(LongWord(PColor),4);
              {glColor4ubv(PColor);}//DoColor(LPtr(PColor)^,Tints.T0);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              C2 := LPtr(PColor)^;
              If (C2 <> C0) Or (Tints.T0 <> C1) Then
              Begin
                C0 := C2;
                C1 := Tints.T0;
                Asm
                  PUSH  EBX

                  MOV   EAX,C0
                  MOV   ECX,C1
                  MOV   EBX,PMultAlpha
                  SUB   EDX,EDX

                  // Red

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Green

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Blue

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  ROL   EAX,16
                  MOV   C3,EAX

                  POP   EBX
                End; // Asm
                glColor4ubv(@C3);
//                DoColor(C0,C1);
              End;

              Inc(LongWord(PColor),4);
              {glColor4ubv(PColor);}//DoColor(LPtr(PColor)^,Tints.T0);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              C2 := LPtr(PColor)^;
              If (C2 <> C0) Or (Tints.T0 <> C1) Then
              Begin
                C0 := C2;
                C1 := Tints.T0;
                Asm
                  PUSH  EBX

                  MOV   EAX,C0
                  MOV   ECX,C1
                  MOV   EBX,PMultAlpha
                  SUB   EDX,EDX

                  // Red

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Green

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Blue

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  ROL   EAX,16
                  MOV   C3,EAX

                  POP   EBX
                End; // Asm
                glColor4ubv(@C3);
//                DoColor(C0,C1);
              End;

              Inc(LongWord(PColor),4);
              {glColor4ubv(PColor);}//DoColor(LPtr(PColor)^,Tints.T0);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);
            End
            Else
            Begin
              // No texture

              C2 := LPtr(PColor)^;
              If (C2 <> C0) Or (Tints.T0 <> C1) Then
              Begin
                C0 := C2;
                C1 := Tints.T0;
                Asm
                  PUSH  EBX

                  MOV   EAX,C0
                  MOV   ECX,C1
                  MOV   EBX,PMultAlpha
                  SUB   EDX,EDX

                  // Red

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Green

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Blue

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  ROL   EAX,16
                  MOV   C3,EAX

                  POP   EBX
                End; // Asm
                glColor4ubv(@C3);
//                DoColor(C0,C1);
              End;

              Inc(LongWord(PColor),4);
              {glColor4ubv(PColor);}//DoColor(LPtr(PColor)^,Tints.T0);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              C2 := LPtr(PColor)^;
              If (C2 <> C0) Or (Tints.T0 <> C1) Then
              Begin
                C0 := C2;
                C1 := Tints.T0;
                Asm
                  PUSH  EBX

                  MOV   EAX,C0
                  MOV   ECX,C1
                  MOV   EBX,PMultAlpha
                  SUB   EDX,EDX

                  // Red

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Green

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Blue

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  ROL   EAX,16
                  MOV   C3,EAX

                  POP   EBX
                End; // Asm
                glColor4ubv(@C3);
//                DoColor(C0,C1);
              End;

              Inc(LongWord(PColor),4);
              {glColor4ubv(PColor);}//DoColor(LPtr(PColor)^,Tints.T0);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              C2 := LPtr(PColor)^;
              If (C2 <> C0) Or (Tints.T0 <> C1) Then
              Begin
                C0 := C2;
                C1 := Tints.T0;
                Asm
                  PUSH  EBX

                  MOV   EAX,C0
                  MOV   ECX,C1
                  MOV   EBX,PMultAlpha
                  SUB   EDX,EDX

                  // Red

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Green

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  // Blue

                  SHR   ECX,8
                  ROR   EAX,8

                  MOV   DL,CL
                  MOV   DH,AL
                  MOV   AL,BYTE PTR [EBX+EDX]

                  ROL   EAX,16
                  MOV   C3,EAX

                  POP   EBX
                End; // Asm
                glColor4ubv(@C3);
//                DoColor(C0,C1);
              End;

              Inc(LongWord(PColor),4);
              {glColor4ubv(PColor);}//DoColor(LPtr(PColor)^,Tints.T0);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);
            End;
          End;
        Except
        End;
//        glEnd;
      End;


{
      If (Not AlphaNotOneOnly) And
         (FOwner.OcclusionManager.Enabled) And
         (Not (SSE Or TFVector.is3DNow))
         And (Not FAnimated)
         And ((Faces[FaceIndex].Flags And (ffHasAlpha Or ffHasTrans)) = 0) Then
      Begin
        FOwner.OcclusionManager.RenderTriangleToZBuffer(SPtr(LongWord(Positions.DataArray) + (FaceIndex * 36)),
                                                        SPtr(LongWord(FNormals.DataArray) + FaceIndex * 12));
      End;
}
    End
    Else
    Begin
      Inc(LongWord(PColor),12);
      Inc(LongWord(Norms),12);
      Inc(LongWord(PTexCoords),24);
      Inc(LongWord(Verts),36);
    End;
    Inc(Vert,3);

    Inc(LongWord(PColors),12);
    Inc(LongWord(PVertexColors),12);
  End; // For FaceIndex

  If Not MustBegin Then glEnd;

  If CurPiece <> -1 Then glPopMatrix;
End; // TModel.RedrawTriangles

Procedure TModel.RedrawQuads(Const VertexColors: Array Of LongWord; AlphaNotOneOnly,Emissive: Boolean; Frame: TSkeletonFrame;
                             Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList);
Var
  Manual          : Boolean;
  Texture         : TTexture;
  Vert            : Integer;
  Verts           : PGLFloat;
  Norms           : PGLByte;
  FaceIndex       : Integer;
  NumVertexColors : Integer;
  PColors         : Pointer;
  PVertexColors   : Pointer;
  PTexCoords      : Pointer;
  PColor          : Pointer;
  NumTexSets      : Integer;
  CurPiece        : Integer;
  I0,I1,I2,I3     : Integer;
  CurTexSet       : Integer;
  TextureSet      : TTextureSet;
  I               : Integer;

  Procedure MoveToCorrectPiece(PieceIndex: Integer);
  Begin
    If CurPiece < 0 Then glPushMatrix Else
    Begin
      glPopMatrix;
      glPushMatrix;
    End;
    CurPiece := PieceIndex;
    glTranslatef(Frame.Pieces[CurPiece].TX1,Frame.Pieces[CurPiece].TY1,Frame.Pieces[CurPiece].TZ1);
    glRotatef(Frame.Pieces[CurPiece].Angle1,Frame.Pieces[CurPiece].AX1,Frame.Pieces[CurPiece].AY1,Frame.Pieces[CurPiece].AZ1);
  End; // MoveToCorrectPiece

  Procedure RenderSpanningQuad;
  Var
    I,J      : Integer;
    X,Y,Z    : Single;
    X1,Y1,Z1 : Single;

  Begin
    If CurPiece >= 0 Then glPopMatrix;
    glBegin(GL_POLYGON);
    For I := 0 To 3 Do
    Begin
      X  := PGLFloat(LongWord(Verts))^;
      Y  := PGLFloat(LongWord(Verts) + 4)^;
      Z  := PGLFloat(LongWord(Verts) + 8)^;

      J  := PieceIndices[Vert + I];

      X1 := Frame.Pieces[J].M1[1,1] * X + Frame.Pieces[J].M1[1,2] * Y + Frame.Pieces[J].M1[1,3] * Z + Frame.Pieces[J].TX1;
      Y1 := Frame.Pieces[J].M1[2,1] * X + Frame.Pieces[J].M1[2,2] * Y + Frame.Pieces[J].M1[2,3] * Z + Frame.Pieces[J].TY1;
      Z1 := Frame.Pieces[J].M1[3,1] * X + Frame.Pieces[J].M1[3,2] * Y + Frame.Pieces[J].M1[3,3] * Z + Frame.Pieces[J].TZ1;

      Try
        If NearLight <> 0 Then
        Begin
          If Manual Then // Manual can only be true if there is a texture
          Begin
            glColor4ubv(@NearLight);        Inc(LongWord(PColor),4);
            glNormal3bv(Norms);             Inc(LongWord(Norms),4);
            glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
            glVertex3f(X1,Y1,Z1);           Inc(LongWord(Verts),12);
          End
          Else
          Begin
            glColor4ubv(@NearLight);        Inc(LongWord(PColor),4);
            glNormal3bv(Norms);             Inc(LongWord(Norms),4);
            {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
            glVertex3f(X1,Y1,Z1);           Inc(LongWord(Verts),12);
          End;
        End
        Else
        Begin
          If Manual Then // Manual can only be true if there is a texture
          Begin
            glColor4ubv(PColor);            Inc(LongWord(PColor),4);
            glNormal3bv(Norms);             Inc(LongWord(Norms),4);
            glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
            glVertex3f(X1,Y1,Z1);           Inc(LongWord(Verts),12);
          End
          Else
          Begin
            glColor4ubv(PColor);            Inc(LongWord(PColor),4);
            glNormal3bv(Norms);             Inc(LongWord(Norms),4);
            {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
            glVertex3f(X1,Y1,Z1);           Inc(LongWord(Verts),12);
          End;
        End;
      Except
      End;
    End; // For I
    glEnd;
    If CurPiece >= 0 Then
    Begin
      glPushMatrix;
      glTranslatef(Frame.Pieces[CurPiece].TX,Frame.Pieces[CurPiece].TY,Frame.Pieces[CurPiece].TZ);
      glRotatef(Frame.Pieces[CurPiece].Angle,Frame.Pieces[CurPiece].AX,Frame.Pieces[CurPiece].AY,Frame.Pieces[CurPiece].AZ);
    End;
  End; // RenderSpanningQuad

Begin
  Verts      := PGLFloat(Positions.DataArray);
  Norms      := PGLByte(@VNormals[0]);
  PColors    := @(Colors[0]);
  PTexCoords := @(Vertices[0]);
  NumVertexColors := High(VertexColors) + 1;
  If NumVertexColors > 0
   Then PVertexColors := @(VertexColors[0])
   Else PVertexColors := Nil;
  Vert       := 0;
  NumTexSets := TextureSets.Count;
  CurPiece   := -1;
  I0         := 0;
  I1         := 0;
  I2         := 0;
  I3         := 0;
  Manual     := False;
  FSubst     := TTextureSet(TextureSets.Get(ReplaceTexSetID));

  For FaceIndex := 0 To High(Faces) Do
  Begin
    If ((Faces[FaceIndex].Flags And ffHasAlpha) <> 0) Or Not AlphaNotOneOnly Then
    Begin
      If (Faces[FaceIndex].Texture >= 0) {And (NumTexSets > 0)} Then
      Begin
        If Frame <> Nil Then
        Begin
          I0 := PieceIndices[Vert];
               If (I0 >= 0) And (I0 <= High(PieceTextures)) Then I0 := PieceTextures[I0].TextureID
          Else If High(PieceTextures) >= 0 Then I0 := PieceTextures[0].TextureID
          Else I0 := 0;

          I1 := PieceIndices[Vert + 1];
               If (I1 >= 0) And (I1 <= High(PieceTextures)) Then I1 := PieceTextures[I1].TextureID
          Else If High(PieceTextures) >= 0 Then I1 := PieceTextures[0].TextureID
          Else I1 := 0;

          I2 := PieceIndices[Vert + 2];
               If (I2 >= 0) And (I2 <= High(PieceTextures)) Then I2 := PieceTextures[I2].TextureID
          Else If High(PieceTextures) >= 0 Then I2 := PieceTextures[0].TextureID
          Else I2 := 0;

          I3 := PieceIndices[Vert + 3];
               If (I3 >= 0) And (I3 <= High(PieceTextures)) Then I3 := PieceTextures[I3].TextureID
          Else If High(PieceTextures) >= 0 Then I3 := PieceTextures[0].TextureID
          Else I3 := 0;

          I0 := Max(Max(Max(I0,I1),I2),I3);
          CurTexSet := I0;
        End
        Else
        Begin
          If High(PieceTextures) >= 0
           Then CurTexSet := PieceTextures[0].TextureID
           Else CurTexSet := 0;
        End;
        If (CurTexSet < 0) Or (CurTexSet >= NumTexSets) Then CurTexSet := 0;
        
        // If no texture set has been specified, then the faces contain absolute texture indices

        If NumTexSets = 0 Then
        Begin
          If (Faces[FaceIndex].Texture >= 0) And (Faces[FaceIndex].Texture < FOwner.Textures.Count)
           Then Texture := TTexture(FOwner.Textures.Objects[Faces[FaceIndex].Texture])
           Else Texture := Nil;
        End
        Else 
        Begin
          TextureSet := TTextureSet(TextureSets.Get(CurTexSet));
          If TextureSet = Nil Then TextureSet := TTextureSet(TextureSets.Items[0].Value.Ptr);
          I := TextureSet.FInfo[Faces[FaceIndex].Texture].CurIndex;
          If (I >= 0) And (I <= High(TextureSet.FTextures))
           Then Texture := SubstituteTexture(TextureSet.FTextures[I],ReplaceFromTexInfo,ReplaceToTexInfo)
           Else Texture := Nil;
        End;

        If Texture <> Nil Then
        Begin
          If Not Texture.Loaded Then
          Begin
            // It's safe to call this directly because we're already in the render thread

            Texture.LoadTextureIntoOpenGL;
            If Texture.HasAlpha Then Faces[FaceIndex].Flags := Faces[FaceIndex].Flags Or ffHasAlpha;
            If Texture.HasTrans Then Faces[FaceIndex].Flags := Faces[FaceIndex].Flags Or ffHasTrans;
          End;
          FHasAlpha := FHasAlpha Or Texture.HasAlpha;
          FHasTrans := FHasTrans Or Texture.HasTrans;
          If Texture.HasAlpha Then Faces[FaceIndex].Flags := Faces[FaceIndex].Flags Or ffHasAlpha;
          If Texture.HasTrans Then Faces[FaceIndex].Flags := Faces[FaceIndex].Flags Or ffHasTrans;
          If Not Tex2D Then glEnable(GL_TEXTURE_2D);
          Tex2D  := True;
          Manual := Not Texture.Automatic;
          If Texture.ID <> FLastBoundTextureID Then
          Begin
            glBindTexture(GL_TEXTURE_2D, Texture.ID);
            Texture.SetShaderParameter;
            FLastBoundTextureID := Texture.ID;
          End;
        End
        Else
        Begin
          If Tex2D Then glDisable(GL_TEXTURE_2D);
          Tex2D  := False;
          Manual := False;
          FLastBoundTextureID := -1;
        End;
      End
      Else
      Begin
        If Tex2D Then glDisable(GL_TEXTURE_2D);
        Tex2D  := False;
        Manual := False;
        FLastBoundTextureID := -1;
      End;

      // When using emissive color in OpenGL, we can't change alpha transparency.  The engine deals with this
      // by rendering in two passes -- first it renders opaque elements and then it renders non-opaque elements.
      // When rendering non-opaque elements, we change the alpha by temporarily changing to ambient/diffuse
      // color mode.

      If Emissive Then
      Begin
        If (Faces[FaceIndex].Flags And ffAlphaMask) <> FAA Then
        Begin
          glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
          glColor4ub(255,255,255,Faces[FaceIndex].Flags And ffAlphaMask);
          glColorMaterial(GL_FRONT,GL_EMISSION);
          glColor4ub(0,0,0,0);
          FAA := Faces[FaceIndex].Flags And ffAlphaMask;
        End;
      End;

      If Vert + 4 <= NumVertexColors
       Then PColor := PVertexColors
       Else PColor := PColors;

      If Frame <> Nil Then
      Begin
        I0 := PieceIndices[Vert];
        I1 := PieceIndices[Vert + 1];
        I2 := PieceIndices[Vert + 2];
        I3 := PieceIndices[Vert + 3];
      End;

      If (Frame <> Nil) And ((I0 <> I1) Or (I0 <> I2) Or (I0 <> I3) Or (I1 <> I2) Or (I1 <> I3) Or (I2 <> I3)) Then RenderSpanningQuad
      Else
      Begin
        If (Frame <> Nil) And (CurPiece <> I0) Then MoveToCorrectPiece(I0);

        glBegin(GL_POLYGON);
        Try
          If NearLight <> 0 Then
          Begin
            If Manual Then // Manual can only be true if there is a texture
            Begin
              glColor4ubv(@NearLight);        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(@NearLight);        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(@NearLight);        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(@NearLight);        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);
            End
            Else
            Begin
              glColor4ubv(@NearLight);        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(@NearLight);        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(@NearLight);        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(@NearLight);        Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);
            End;
          End
          Else
          Begin
            If Manual Then // Manual can only be true if there is a texture
            Begin
              glColor4ubv(PColor);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(PColor);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(PColor);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(PColor);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              glTexCoord2fv(PTexCoords);      Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);
            End
            Else
            Begin
              glColor4ubv(PColor);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(PColor);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(PColor);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);

              glColor4ubv(PColor);            Inc(LongWord(PColor),4);
              glNormal3bv(Norms);             Inc(LongWord(Norms),4);
              {glTexCoord2fv(PTexCoords);}    Inc(LongWord(PTexCoords),8);
              glVertex3fv(Verts);             Inc(LongWord(Verts),12);              
            End;
          End;
        Except
        End;
        glEnd;
      End;
{
      If (Not AlphaNotOneOnly) And
         (FOwner.OcclusionManager.Enabled) And
         (Not (TFVector.isSSE Or TFVector.is3DNow)) And
         (Not FAnimated)
         And ((Faces[FaceIndex].Flags And (ffHasAlpha Or ffHasTrans)) = 0) Then
      Begin
        FOwner.OcclusionManager.RenderTriangleToZBuffer(SPtr(LongWord(Positions.DataArray) + (FaceIndex * 36)),
                                                        SPtr(LongWord(FNormals.DataArray) + FaceIndex * 12));
      End;
}
    End
    Else
    Begin
      Inc(LongWord(PColor),16);
      Inc(LongWord(Norms),16);
      Inc(LongWord(PTexCoords),32);
      Inc(LongWord(Verts),48);
    End;
    Inc(Vert,4);

    Inc(LongWord(PColors),16);
    Inc(LongWord(PVertexColors),16);
  End; // For FaceIndex

  If CurPiece <> -1 Then glPopMatrix;
End; // TModel.RedrawQuads

Procedure TModel.FreeTextureSets;
Var I: Integer;
Begin
  For I := 0 To TextureSets.Count - 1 Do TTextureSet(TextureSets.Items[I].Value.Ptr).Free;
  TextureSets.Clear;
End; // TModel.FreeTextureSets

Procedure TModel.PutTextureSet(Index: Integer; TextureSet: TTextureSet);
Var T: TTextureSet;
Begin
  T := TTextureSet(TextureSets.Get(Index));
  If T <> Nil Then T.Free;
  TextureSets.Put(Index,TextureSet);
End; // TModel.PutTextureSet

Procedure TModel.CopyTextureSets(Model: TModel);
Var I: Integer;
Begin
  FreeTextureSets;
  For I := 0 To Model.TextureSets.Count - 1 Do
   TextureSets.Put(Model.TextureSets.Items[I].Key.Int,TTextureSet.Create(TTextureSet(Model.TextureSets.Items[I].Value.Ptr)));
End; // TModel.CopyTextureSets

Procedure TModel.SetVertex(Index: Integer; X,Y,Z,NX,NY,NZ: Single);
Var BNX,BNY,BNZ: ShortInt;
Begin
  Positions.DataArray[Index * 3]     := X;
  Positions.DataArray[Index * 3 + 1] := Y;
  Positions.DataArray[Index * 3 + 2] := Z;

  BNX := Round(NX * 127);
  BNY := Round(NY * 127);
  BNZ := Round(NZ * 127);

  TNormal(VNormals[Index]).X := BNX;
  TNormal(VNormals[Index]).Y := BNY;
  TNormal(VNormals[Index]).Z := BNZ;
{
  TNormal(VNormals[Index * 3]).X := BNX;
  TNormal(VNormals[Index * 3]).Y := BNY;
  TNormal(VNormals[Index * 3]).Z := BNZ;

  TNormal(VNormals[Index * 3 + 1]).X := BNX;
  TNormal(VNormals[Index * 3 + 1]).Y := BNY;
  TNormal(VNormals[Index * 3 + 1]).Z := BNZ;

  TNormal(VNormals[Index * 3 + 2]).X := BNX;
  TNormal(VNormals[Index * 3 + 2]).Y := BNY;
  TNormal(VNormals[Index * 3 + 2]).Z := BNZ;
}
End; // TModel.SetVertex

Procedure TModel.SetFace(Index: Integer; VertIndices: Array Of Integer);
//Var I: Integer;
Begin
//  Faces[Index].NumVerts := High(VertIndices) + 1;
//  For I := 0 To High(VertIndices) Do Faces[Index].Vertices[I] := VertIndices[I];
End; // TModel.SetFace

Procedure TModel.SetColor(R,G,B,A: Byte);
Var
  I : Integer;
  C : TColor;

Begin
  TBGRA(C).R := R;
  TBGRA(C).G := G;
  TBGRA(C).B := B;
  TBGRA(C).A := A;
  For I := 0 To High(Colors) Do Colors[I] := C;
End; // TModel.SetColor

Procedure TModel.MapVertices(Width,Height: Integer; Clouds: Boolean);
// Considers the texture an all-sky map and performs a spherical projection (used by sky spheres)
Var
  I,J,K    : Integer;
  P        : T3DPoint{I3dPoint};
  MinTheta : Single;
  MinPhi   : Single;
  MaxTheta : Single;
  MaxPhi   : Single;
  Theta    : Array Of Single;
  Phi      : Array Of Single;

Begin
  P := T3DPoint.Create{Point};
  For I := 0 To High(Faces) Do
  Begin
    SetLength(Theta,3);
    SetLength(Phi,3);
    MinTheta := 2 * Pi;
    MaxTheta := 0;
    MinPhi   := 2 * Pi;
    MaxPhi   := 0;
    For J := 0 To 2 Do
    Begin
      K := (I * 3 + J) * 3;
      P.Copy(Positions.DataArray[K],Positions.DataArray[K + 1],Positions.DataArray[K + 2]);
      If Clouds Then
      Begin
        Theta[J] := (1 - P.Y) * Pi / 2;
        Phi[J]   := (1 - P.X) * Pi / 2;
      End
      Else
      Begin
        P.Normalize;
        Theta[J] := ArcTan2(P.Y,P.X);
        Phi[J]   := ArcTan2(P.Z,Sqrt(Sqr(P.X) + Sqr(P.Y)));
      End;
      While Theta[J] <  0      Do Theta[J] := Theta[J] + 2 * Pi;
      While Theta[J] >= 2 * Pi Do Theta[J] := Theta[J] - 2 * Pi;
      If Not Clouds Then Phi[J] := Phi[J] + Pi / 2;
      If J = 0 Then
      Begin
        MinTheta := Theta[J];
        MaxTheta := Theta[J];
        MinPhi   := Phi[J];
        MaxPhi   := Phi[J];
      End
      Else
      Begin
        If Theta[J] < MinTheta Then MinTheta := Theta[J];
        If Theta[J] > MaxTheta Then MaxTheta := Theta[J];
        If Phi[J]   < MinPhi   Then MinPhi   := Phi[J];
        If Phi[J]   > MaxPhi   Then MaxPhi   := Phi[J];
      End;
    End; // For J

    // Handle wraparound

    If MaxTheta > MinTheta + Pi Then
    Begin
      For J := 0 To 2 Do
      Begin
        If Theta[J] < Pi Then Theta[J] := Theta[J] + 2 * Pi;
      End; // For J
    End;

    // Set the texture coordinates

    For J := 0 To 2 Do
    Begin
      If Clouds
       Then Vertices[I * 3 + J].TX := Theta[J] / Pi
       Else Vertices[I * 3 + J].TX := Theta[J] / (2 * Pi);
      Vertices[I * 3 + J].TZ := Phi[J] / Pi;
    End; // For J
  End; // For I
  SetLength(Theta,0);
  SetLength(Phi,0);
  P.Free;
//  P := Nil;
End; // TModel.MapVertices

Procedure TModel.AddTexCoords(TX,TZ: Single);
Var I: Integer;
Begin
  For I := 0 To High(Vertices) Do
  Begin
    Vertices[I].TX := Vertices[I].TX + TX;
    Vertices[I].TZ := Vertices[I].TZ + TZ;
  End; // For I
End; // TModel.AddTexCoords

Function TModel.IsIntersectedBy(Source,Dest: T3DPoint{I3dPoint}): Boolean;
// This method does not take skeletal animation into account
Var
  I,J,K    : Integer;
  Found    : Boolean;
  NX,NY,NZ : Single;

Begin
  If FBox.IntersectionWithLineSegment(Source,Dest) >= 0 Then
  Begin
    FRayWork.Copy(Dest);
    FRayWork.Subtract(Source);
    I     := 0;
    J     := 0;
    K     := 0;
    Found := False;
    While (I <= High(Faces)) And Not Found Do
    Begin
      NX := FNormals.DataArray[J];
      NY := FNormals.DataArray[J + 1];
      NZ := FNormals.DataArray[J + 2];
      If FRayWork.X * NX + FRayWork.Y * NY + FRayWork.Z * NZ < 0 Then
      Begin
        FP1Work.Copy(Positions.DataArray[K + 6],Positions.DataArray[K + 7],Positions.DataArray[K + 8]);
        FP2Work.Copy(Positions.DataArray[K + 3],Positions.DataArray[K + 4],Positions.DataArray[K + 5]);
        FP3Work.Copy(Positions.DataArray[K + 0],Positions.DataArray[K + 1],Positions.DataArray[K + 2]);
        If gts_segment_triangle_intersection(Source,Dest,FP1Work,FP2Work,FP3Work,True,Dest) Then Found := True;
      End;
      Inc(I);
      Inc(J,3);
      Inc(K,9);
    End; // While
    Result := Found;
  End
  Else Result := False;
End; // TModel.IsIntersectedBy

Function TModel.FindMinMaxZPoints(Const X,Y: Single; Out MinZ,MaxZ: Single): Boolean;
// This method does not take skeletal animation into account
Var
  I,J,K    : Integer;
  Found    : Boolean;
  NX,NY,NZ : Single;

Begin
  // Create a ray pointing straight down at the given X,Y point

  FP4Work.Copy(X,Y,FBox.MaxPt.Z + 1);
  FP5Work.Copy(X,Y,FBox.MinPt.Z - 1);

  // Make sure the ray intesects the bounding box

  Found := False;
//  If FBox.IntersectionWithLineSegment(FP4Work,FP5Work) >= 0 Then
  Begin
    MaxZ  := FBox.MinPt.Z;
    MinZ  := FBox.MaxPt.Z;
    J     := 0;  // Face normal index
    K     := 0;  // Vertex index
    For I := 0 To High(Faces) Do
    Begin
      NZ := FNormals.DataArray[J + 2];

      // Make sure the triangle faces up

      If NZ > 0 Then
      Begin
        FP1Work.Copy(Positions.DataArray[K + 6],Positions.DataArray[K + 7],Positions.DataArray[K + 8]);
        FP2Work.Copy(Positions.DataArray[K + 3],Positions.DataArray[K + 4],Positions.DataArray[K + 5]);
        FP3Work.Copy(Positions.DataArray[K + 0],Positions.DataArray[K + 1],Positions.DataArray[K + 2]);
        If gts_segment_triangle_intersection(FP4Work,FP5Work,FP1Work,FP2Work,FP3Work,True,FP1Work) Then
        Begin
          If Not Found Then
          Begin
            MinZ := FP1Work.Z;
            MaxZ := FP1Work.Z;
          End
          Else
          Begin
            MinZ := Min(MinZ,FP1Work.Z);
            MaxZ := Max(MaxZ,FP1Work.Z);
          End;
          Found := True;
        End;
      End;
      Inc(J,3);
      Inc(K,9);
    End; // For I
  End;
  Result := Found;
End; // TModel.FindMinMaxZPoints

Procedure TModel.FacesCollideWithEllipsoid(Ellipsoid: TEllipsoid; Velocity: T3DPoint{I3dPoint}; MovementSphere: TSphere; Var NearestDistance: Single; ClosestPoint: T3DPoint{I3dPoint}; Var Friction: Single);
// Note that this only works if the face is a triangle.  It assumes the sphere has radius 1.  This can work for spheres or
// ellipsoids of any dimension by dividing Center and Velocity by the radius before passing it to this procedure.
Var
  Plane                  : TPlane;
  T0,T1,T,Swap           : Single;
  T00                    : Single;
  EmbeddedInPlane        : Boolean;
  SignedDistToPlane      : Single;
  NormalDotVelocity      : Single;
  CollisionPossible      : Boolean;
  FoundCollision         : Boolean;
  PlaneIntersectionPoint : T3DPoint{I3dPoint};
  V1                     : T3DPoint{I3dPoint};
  VelocitySquaredLength  : Single;
  VelocityLength         : Single;
  A,B,C                  : Single;
  NewT                   : Single;
  P                      : Array[0..2] Of T3DPoint{I3dPoint};
//  PP                     : Array[0..2] Of P3DPointPointer;
  VertexToCenter         : T3DPoint{I3dPoint};
  Edge                   : T3DPoint{I3dPoint};
  EdgeSquaredLength      : Single;
  EdgeDotVelocity        : Single;
  EdgeDotVertexToCenter  : Single;
  Interval               : Single;
  I,J                    : Integer;
  Index                  : Integer;
  DistToCollision        : Single;
  CollisionPoint         : T3DPoint{I3dPoint};
  ScaledCenter           : T3DPoint{I3dPoint};
//  ScaledCenterP          : P3DPointPointer;
  ScaledVelocity         : T3DPoint{I3dPoint};
  ScaledVelocityZ        : Single;
//  ScaledVelocityP        : P3DPointPointer;
  VelocityZ              : Single;
  DistOffset             : Single;
  ERX,ERY,ERZ            : Single;
  ERY_ERZ                : Single;
  ERX_ERZ                : Single;
  ERX_ERY                : Single;
  ERX_ERY_ERZ            : Single;
  MovingDownOnly         : Boolean;
  VerticalOnly           : Boolean;
  GotVelLen              : Boolean;
  PlaneDotVelocity       : Single;
  SVDotSC                : Single;
  SVDotPI                : Array[0..2] Of Single;
  NormLen                : Single;
  PNZ                    : Single;  // Plane.Normal.Z ... done to avoid calls to GetZ
  SCZ                    : Single;  // ScaledCenter.Z ... done to avoid calls to GetZ
//  VelocityP              : P3DPointPointer;
//  MovementSphereCenterP  : P3DPointPointer;
  I0,I1,I2               : Integer;
  CollisionType          : Integer;

  Function GetLowestRoot(A,B,C,MaxR: Single; Out Root: Single): Boolean;
  Var
    Determinant : Single;
    SqrtD       : Single;
    R1,R2,R     : Single;

  Begin
    // Default result

    Root   := 0; // Put something in there
    Result := False;

    If A <> 0 Then
    Begin
      // Check if a solution exists

      Determinant := B * B - 4 * A * C;

      // Get the solution if we can

      If Determinant = 0 Then
      Begin
        // There is just one root

        R := -B / (2 * A);
        If (R > 0) And (R < MaxR) Then
        Begin
          Root   := R;
          Result := True;
        End;
      End
      Else If Determinant > 0 Then
      Begin
        // Calculate the two roots and get the lowest one

        SqrtD := Sqrt(Determinant);
        R1    := (-B - SqrtD) / (2 * A);
        R2    := (-B + SqrtD) / (2 * A);

        // Make sure R1 < R2

        If R1 > R2 Then
        Begin
          R  := R1;
          R1 := R2;
          R2 := R;
        End;

        // Get the lowest root

        If (R1 > 0) And (R1 < MaxR) Then
        Begin
          Root   := R1;
          Result := True;
        End
        Else
        Begin
          // We might want R2 if R1 < 0

          If (R2 > 0) And (R2 < MaxR) Then
          Begin
            Root   := R2;
            Result := True;
          End;
        End;
      End;
    End
    Else
    Begin
      If B <> 0 Then
      Begin
        R1 := -C / B;
        If (R1 > 0) And (R1 < MaxR) Then
        Begin
          Root   := R1;
          Result := True;
        End;
      End;
    End;
  End; // GetLowestRoot

  Procedure Log(St: String);
  Var F: System.Text;
  Begin
    AssignFile(F,'coll.txt');
    If FileExists('coll.txt')
     Then Append(F)
     Else ReWrite(F);
    WriteLn(F,St);
    Flush(F);
    CloseFile(F);
  End; // Log

Begin
  // Preallocate and precalculate things

  If Ellipsoid.Radius.NoneAreZero And FCylinder.IntersectsSphere(MovementSphere) Then
  Begin
    VertexToCenter         := T3DPoint.Create{Point};
    Edge                   := T3DPoint.Create{Point};
    CollisionPoint         := T3DPoint.Create{Point};
    Plane                  := TPlane.Create;
    PlaneIntersectionPoint := T3DPoint.Create{Point};
    V1                     := T3DPoint.Create{Point};
    ScaledCenter           := T3DPoint.Create{Point}(Ellipsoid.Center);
    ScaledVelocity         := T3DPoint.Create{Point}(Velocity);
    ScaledCenter.Divide(Ellipsoid.Radius);
//    ScaledCenterP          := ScaledCenter.PointPointer;
    ScaledVelocity.Divide(Ellipsoid.Radius);
    ScaledVelocityZ        := ScaledVelocity.Z;
//    ScaledVelocityP        := ScaledVelocity.PointPointer;
    VelocitySquaredLength  := ScaledVelocity.GetSquaredLength;
    P[0]                   := T3DPoint.Create{Point};
    P[1]                   := T3DPoint.Create{Point};
    P[2]                   := T3DPoint.Create{Point};
    DistOffset             := 0;
    ERX                    := Ellipsoid.Radius.X;
    ERY                    := Ellipsoid.Radius.Y;
    ERZ                    := Ellipsoid.Radius.Z;
    ERY_ERZ                := ERY * ERZ;
    ERX_ERZ                := ERX * ERZ;
    ERX_ERY                := ERX * ERY;
    ERX_ERY_ERZ            := ERX_ERY * ERZ;
    VerticalOnly           := (ScaledVelocity.X = 0) And (ScaledVelocity.Y = 0);
    MovingDownOnly         := VerticalOnly And (ScaledVelocityZ < 0);
    GotVelLen              := False;
    SVDotSC                := ScaledVelocity.Dot(ScaledCenter);
    VelocityZ              := Velocity.Z;
//    VelocityP              := Velocity.PointPointer;
//    MovementSphereCenterP  := MovementSphere.Center.PointPointer;

    I0 := 0;
    I1 := 0;

    For Index := 0 To High(Faces) Do
    Begin
      // Make sure it's at least a triangle

      If ((Faces[Index].Flags And ffSolid) <> 0) Then
      Begin
        // Make copies of the vertex points and scale them to the ellipsoid radius

//        V1.Copy(@(FNormals.DataArray[I1]));

        // Make sure the plane faces our direction of motion

        If VerticalOnly Then PlaneDotVelocity := FNormals.DataArray[I1 + 2] * VelocityZ
        Else
        Begin
          V1.Copy(@(FNormals.DataArray[I1]));
          PlaneDotVelocity := V1.Dot(Velocity{P});
        End;
        If PlaneDotVelocity <= 0 Then
        Begin
          P[0].Copy(@(Positions.DataArray[I0]));
          If VerticalOnly Then
          Begin
            Plane.Normal.Copy(@(FNormals.DataArray[I1]));
            Plane.Distance := -Plane.Normal.Dot(P[0]);
          End
          Else Plane.Setup(P[0],V1);

          // Make sure we can reach the triangle's infinite plane

          If Plane.DistanceFromPoint(MovementSphere.Center{P}) <= MovementSphere.Radius Then
          Begin
            // Make a plane for this triangle that is scaled to ellipsoid space

            // We can recalculate the normal without using a cross product

            Plane.Normal.Divide(ERY_ERZ,ERX_ERZ,ERX_ERY);
            NormLen        := Plane.Normal.Normalize;
            Plane.Distance := Plane.Distance / (NormLen * ERX_ERY_ERZ);
                                             
            // Get the interval of the plane intersection

            EmbeddedInPlane := False;

            // Calculate the signed distance from the sphere's center to the plane

            SignedDistToPlane := Plane.SignedDistanceFromPoint(ScaledCenter{P});

            // Check if the sphere is traveling parallel to the triangle's infinite plane

            If PlaneDotVelocity = 0 Then
            Begin
              T0 := 0;
              T1 := 1;

              // If the distance is greater or equal to the sphere radius (NOT the movement
              // sphere's radius) then no collision is possible (remember that the sphere is
              // moving parallel to the plane, so for instance the plane could be too far
              // above below, or to one side of the sphere)

              CollisionPossible := (Abs(SignedDistToPlane) < 1);

              // The sphere is traveling parallel to the plane

              If CollisionPossible Then
              Begin
                // The sphere is embedded in the infinite plane.  It intersects in the whole range [0..1].
                // This says nothing about intersecting the actual triangle, just its infinite plane.

                EmbeddedInPlane   := True;
              End;
            End
            Else
            Begin
              // The sphere is not traveling parallel to the triangle's infinite plane.  Calculate the intersection interval

              If VerticalOnly Then
              Begin
                PNZ               := Plane.Normal.Z;
                NormalDotVelocity := PNZ * ScaledVelocityZ;
              End
              Else NormalDotVelocity := Plane.Normal.Dot(ScaledVelocity{P});
              T0 := (-1 - SignedDistToPlane) / NormalDotVelocity;
              T1 := ( 1 - SignedDistToPlane) / NormalDotVelocity;

              // Make sure that T0 has the smaller value

              If T0 > T1 Then
              Begin
                Swap := T0;
                T0   := T1;
                T1   := Swap;
              End;

              // Check that at least one result is within range

              CollisionPossible := ((T0 <= 1) And (T1 >= 0));

              // If a collision is possible, clamp the values to [0,1]

              If CollisionPossible Then
              Begin
                // !!
(*
                // !! Remarking out the pushback code !!
                // The problem with this code is that it causes a pushback if we
                // intersect the triangle's infinite plane as opposed to if we
                // actually intersect the triangle (which isn't checked until later)
                // Remarking this code out means that we'll pass through polygons
                // if we're embedded within them.


                // If one point is less than 0 then the sphere is in the polygon.  Let's
                // first back away from the polygon along its normal and try again

                If MovingDownOnly And
                   (Plane.Normal.X = 0) And (Plane.Normal.Y = 0) And (Plane.Normal.Z > 0) And
                   (T0 < 0) And (T0 >= 1 / NormalDotVelocity) Then
                Begin
                  If Not GotVelLen Then
                  Begin
                    GotVelLen      := True;
                    VelocityLength := Sqrt(VelocitySquaredLength);
                  End;

                  If DoIntersectionLog Then
                  Begin
                    P[1].Copy(@(Positions.DataArray[I0 + 3]));
                    P[2].Copy(@(Positions.DataArray[I0 + 6]));

                    LogToFile('PUSHBACK',True);
                    LogToFile(Format('Pushback: ScaledCenter = %s',[ScaledCenter.ToString]), True);
                    LogToFile(Format('Pushback: VelocityLength = %8.3f',[VelocityLength]), True);
                    LogToFile(Format('Pushback: T0 = %8.3f, T1 = %8.3f, Plane = %s',[T0,T1,Plane.ToString]), True);
                    LogToFile(Format('Pushback: P[0] = %s',[P[0].ToString]), True);
                    LogToFile(Format('Pushback: P[1] = %s',[P[1].ToString]), True);
                    LogToFile(Format('Pushback: P[2] = %s',[P[2].ToString]), True);
                  End;

                  T0         := T0 - 0.1;
                  DistOffset := DistOffset - T0;
                  T0         := -T0 * {ScaledVelocity.Dot(Plane.Normal)}NormalDotVelocity / VelocityLength;
                  ScaledCenter.Z := ScaledCenter.Z -{+} ScaledVelocityZ * T0; // ScaledVelocity.X and ScaledVelocity.Y are zero if MovingDownOnly is True
{
                  ScaledCenter.Add(ScaledVelocity.X * T0,
                                   ScaledVelocity.Y * T0,
                                   ScaledVelocity.Z * T0);
}
                  SVDotSC := ScaledVelocityZ * ScaledCenter.Z; // ScaledVelocity.X and ScaledVelocity.Y are zero if MovingDownOnly is True
//                  SVDotSC := ScaledVelocity.Dot(ScaledCenter);
                  SignedDistToPlane := Plane.Normal.Z * ScaledCenter.Z + Plane.Distance; //Plane.SignedDistanceFromPoint(ScaledCenter);
                  T0 := (-1 - SignedDistToPlane) / NormalDotVelocity;
                  T1 := ( 1 - SignedDistToPlane) / NormalDotVelocity;

                  // Make sure that T0 has the smaller value

                  If T0 > T1 Then
                  Begin
                    Swap := T0;
                    T0   := T1;
                    T1   := Swap;
                  End;
                End;

                // !!
*)
                T00 := T0; // Save the original value so pushback code can work

                If T0 < 0 Then T0 := 0;
                If T1 < 0 Then T1 := 0;
                If T0 > 1 Then T0 := 1;
                If T1 > 1 Then T1 := 1;

              End;
            End;

            // If a collision is possible, continue.  T0 and T1 represent time values between which the
            // swept sphere (scaled ellipsoid) intersects with the triangle plane.  Any collision must
            // occur within this interval.

            If CollisionPossible Then
            Begin
              FoundCollision := False;
              T              := 1;

              // Scale to ellipsoid space

              P[0].Divide(ERX,ERY,ERZ);
              P[1].Copy(@(Positions.DataArray[I0 + 3]));
              P[2].Copy(@(Positions.DataArray[I0 + 6]));
              P[1].Divide(ERX,ERY,ERZ);
              P[2].Divide(ERX,ERY,ERZ);

              // First check for a collision inside the triangle.  Such an event can only happen at time
              // T0 since the sphere will rest on the front side of the triangle.  It cannot happen if
              // the sphere is embedded in the triangle's infinite plane.

              If Not EmbeddedInPlane Then
              Begin
                PlaneIntersectionPoint.Copy(Plane.Normal{.PointPointer},ScaledCenter{P});
                If VerticalOnly Then PlaneIntersectionPoint.Add(0,0,ScaledVelocityZ * T0)
                Else
                Begin
                  V1.Copy(ScaledVelocity);
                  V1.Multiply(T0);
                  PlaneIntersectionPoint.Add(V1);
                End;

                If PlaneIntersectionPoint.CheckPointInTriangle(P[2],P[1],P[0]) Then
                Begin



                  // PUSHBACK CODE (necessary for lifts to work)
                  // If one point is less than 0 then the sphere is in the polygon.  Let's
                  // first back away from the polygon along its normal and try again

                  If MovingDownOnly And
                     Plane.Normal.IsUpOnly And
                     (T00 < 0) And (T00 >= 1 / NormalDotVelocity) Then
                  Begin
                    If Not GotVelLen Then
                    Begin
                      GotVelLen      := True;
                      VelocityLength := Sqrt(VelocitySquaredLength);
                    End;

                    If DoIntersectionLog Then
                    Begin
                      P[0].Copy(@(Positions.DataArray[I0 + 0]));
                      P[1].Copy(@(Positions.DataArray[I0 + 3]));
                      P[2].Copy(@(Positions.DataArray[I0 + 6]));

                      LogToFile('PUSHBACK',True);
                      LogToFile(Format('Pushback: ScaledCenter = %s',[ScaledCenter.ToString]), True);
                      LogToFile(Format('Pushback: VelocityLength = %8.3f',[VelocityLength]), True);
                      LogToFile(Format('Pushback: T00 = %8.3f, T1 = %8.3f, Plane = %s',[T00,T1,Plane.ToString]), True);
                      LogToFile(Format('Pushback: P[0] = %s',[P[0].ToString]), True);
                      LogToFile(Format('Pushback: P[1] = %s',[P[1].ToString]), True);
                      LogToFile(Format('Pushback: P[2] = %s',[P[2].ToString]), True);

                      P[0].Divide(ERX,ERY,ERZ);
                      P[1].Divide(ERX,ERY,ERZ);
                      P[2].Divide(ERX,ERY,ERZ);
                    End;

                    T0         := T00 - 1 / NormalDotVelocity;//0.1;
                    DistOffset := DistOffset - T00;
                    T0         := -T00 * {ScaledVelocity.Dot(Plane.Normal)}NormalDotVelocity / VelocityLength;
                    ScaledCenter.Subtract(0,0,ScaledVelocityZ * T00); // ScaledVelocity.X and ScaledVelocity.Y are zero if MovingDownOnly is True
  {
                    ScaledCenter.Add(ScaledVelocity.X * T00,
                                     ScaledVelocity.Y * T00,
                                     ScaledVelocity.Z * T00);
  }
                    SCZ     := ScaledCenter.Z;;
                    SVDotSC := ScaledVelocityZ * SCZ; // ScaledVelocity.X and ScaledVelocity.Y are zero if MovingDownOnly is True
  //                  SVDotSC := ScaledVelocity.Dot(ScaledCenter);
                    SignedDistToPlane := PNZ * SCZ + Plane.Distance; //Plane.SignedDistanceFromPoint(ScaledCenter);
                    T0 := (-1 - SignedDistToPlane) / NormalDotVelocity;
                    T1 := ( 1 - SignedDistToPlane) / NormalDotVelocity;

                    // Make sure that T0 has the smaller value

                    If T0 > T1 Then
                    Begin
                      Swap := T0;
                      T0   := T1;
                      T1   := Swap;
                    End;
                  End;

                  // END PUSHBACK CODE



                  FoundCollision := True;
                  T              := T0;
                  CollisionPoint.Copy(PlaneIntersectionPoint);

                  CollisionType := 0;

                End;
              End;

              // If no collision has ben found yet then sweep the sphere against the points and edges of the triangle

              If Not FoundCollision Then
              Begin
                // Have to solve a quadratic equation for each vertex or edge: at^2 + bt + c = 0

                If VerticalOnly Then
                Begin
                  SVDotPI[0] := ScaledVelocityZ * P[0].Z; // Optimization
                  SVDotPI[1] := ScaledVelocityZ * P[1].Z;
                  SVDotPI[2] := ScaledVelocityZ * P[2].Z;
                End
                Else
                Begin
                  SVDotPI[0] := ScaledVelocity.Dot(P[0]);
                  SVDotPI[1] := ScaledVelocity.Dot(P[1]);
                  SVDotPI[2] := ScaledVelocity.Dot(P[2]);
                End;
{
                PP[0] := P[0].PointPointer;
                PP[1] := P[1].PointPointer;
                PP[2] := P[2].PointPointer;
}
                J := 1;
                For I := 0 To 2 Do
                Begin
                  VertexToCenter.Copy({P}P[I],ScaledCenter{P});

                  // Check against the vertex

                  A := VelocitySquaredLength;
                  B := 2 * (SVDotSC - SVDotPI[I]); //ScaledVelocity.Dot(VertexToCenter);
                  C := VertexToCenter.GetSquaredLength - 1;
                  If GetLowestRoot(A,B,C,T,NewT) Then
                  Begin
                    T              := NewT;
                    FoundCollision := True;
                    CollisionPoint.Copy({PSingleArray(P}P[I]{)});

                    CollisionType := 1;

                  End;

                  // Check against the edge linking this vertex and the next one

                  Edge.Copy({P}P[I],{P}P[J]);
                  EdgeSquaredLength     := Edge.GetSquaredLength;
                  EdgeDotVelocity       := SVDotPI[J] - SVDotPI[I]; //Edge.Dot(ScaledVelocity);
                  EdgeDotVertexToCenter := Edge.Dot(VertexToCenter);
                  A                     := -EdgeSquaredLength * A + Sqr(EdgeDotVelocity);
                  B                     := -EdgeSquaredLength * B + 2 * EdgeDotVelocity * EdgeDotVertexToCenter;
                  C                     := -EdgeSquaredLength * C + Sqr(EdgeDotVertexToCenter);

                  // Check for a collision with the infinite edge

                  If GetLowestRoot(A,B,C,T,NewT) Then
                  Begin
                    // Check for a collision within the line segment

                    Interval := (EdgeDotVelocity * NewT + EdgeDotVertexToCenter) / EdgeSquaredLength;
                    If (Interval >= 0) And (Interval <= 1) Then
                    Begin
                      T              := NewT;
                      FoundCollision := True;
                      CollisionPoint.Copy(Edge);
                      CollisionPoint.Multiply(Interval);
                      CollisionPoint.Add({PSingleArray(P}P[I]{)});

                      CollisionType := 2;

                    End;
                  End;
                  Inc(J);
                  If J = 3 Then J := 0;
                End; // For I

              End;

              // Did we find a collision?

              If FoundCollision Then
              Begin
                // T is the time of collision: calculate the distance

                If Not GotVelLen Then VelocityLength := Sqrt(VelocitySquaredLength); // If not moving down then it wasn't set yet
                DistToCollision := (T +{-} DistOffset) * VelocityLength;             // DistOffset is the pushback amount

                // If this is the first or closest collision then we want to keep it

                If DistToCollision < NearestDistance Then
                Begin

                  NearestDistance := DistToCollision;// * Ellipsoid.Radius.GetLength;
                  ClosestPoint.Copy(CollisionPoint);
//                  Friction := Faces[Index].Friction;
                  Friction := 1; // For now, faces don't have friction


                  If DoIntersectionLog Then
                  Begin
                    LogToFile(Format('Collision: P[0]          = %s',[P[0].ToString]), True);
                    LogToFile(Format('Collision: P[1]          = %s',[P[1].ToString]), True);
                    LogToFile(Format('Collision: P[2]          = %s',[P[2].ToString]), True);
                    LogToFile(Format('Collision: CollisionType = %d',[CollisionType]), True);
                  End;

                End;
              End;
            End;
          End;
        End;
      End;
      Inc(I0,9);
      Inc(I1,3);
    End; // For Index

    P[0].Free;
    P[1].Free;
    P[2].Free;
    ScaledCenter.Free;
    ScaledVelocity.Free;
    VertexToCenter.Free;
    Edge.Free;
    CollisionPoint.Free;
    PlaneIntersectionPoint.Free;
    V1.Free;
{
    P[0]           := Nil;
    P[1]           := Nil;
    P[2]           := Nil;
    ScaledCenter   := Nil;
    ScaledVelocity := Nil;
    VertexToCenter := Nil;
    Edge           := Nil;
    CollisionPoint := Nil;
    PlaneIntersectionPoint := Nil;
    V1                     := Nil;
}
    Plane.Free;
  End;
End; // TModel.FacesCollideWithEllipsoid

// ---------------------------
// TTextureSet
// ---------------------------

Constructor TTextureSet.Create(AOwner: TSceneGL);
Begin
  FOwner := AOwner;
  SetLength(FTextures,0);
  SetLength(FInfo,0);
End; // TTextureSet.Create

Constructor TTextureSet.Create(TextureSet: TTextureSet);
Begin
  CopyFrom(TextureSet);
End; // TTextureSet.Create

Destructor TTextureSet.Destroy;
Begin
  SetLength(FTextures,0);
  SetLength(FInfo,0);
End; // TTextureSet.Destroy

Procedure TTextureSet.AddTextures(Const Textures: TTextureList; Const Interval: Integer);
Var I,J: Integer;
Begin
  SetLength(FInfo,High(FInfo) + 2);
  FInfo[High(FInfo)].Index    := High(FTextures) + 1;
  FInfo[High(FInfo)].Count    := High(Textures)  + 1;
  FInfo[High(FInfo)].Interval := Interval;
  FInfo[High(FInfo)].MatID    := -1;
  SetLength(FTextures,High(FTextures) + High(Textures) + 2);
  J := FInfo[High(FInfo)].Index;
  For I := 0 To High(Textures) Do FTextures[J + I] := Textures[I];
End; // TTextureSet.AddTextures

Procedure TTextureSet.ReplaceTextures(Const Index: Integer; Const Textures: TTextureList; Const Interval: Integer);
Var I,J: Integer;
Begin
  If (Index >= 0) And (Index <= High(FInfo)) Then
  Begin
    // First, grow or shrink the texture list as necessary to accommodate the incoming textures

    If FInfo[Index].Count > High(Textures) + 1 Then
    Begin
      // Shrink the existing list

      J := FInfo[Index].Count - (High(Textures) + 1);
      For I := FInfo[Index].Index To High(Textures) - J Do FTextures[I] := FTextures[I + J];
      For I := Index + 1 To High(FInfo) Do Dec(FInfo[I].Index,J);
      SetLength(FTextures,High(FTextures) + 1 - J);
      FInfo[Index].Count := High(Textures) + 1;
    End
    Else If FInfo[Index].Count < High(Textures) + 1 Then
    Begin
      // Grow the existing list

      J := (High(Textures) + 1) - FInfo[Index].Count;
      SetLength(FTextures,High(FTextures) + 1 + J);
      For I := High(FTextures) DownTo FInfo[Index].Index + J Do FTextures[I] := FTextures[I - J];
      For I := Index + 1 To High(FInfo) Do Inc(FInfo[I].Index,J);
      FInfo[Index].Count := High(Textures) + 1;
    End;

    FInfo[Index].Interval := Interval;
    FInfo[Index].MatID    := -1;
    For I := 0 To High(Textures) Do FTextures[FInfo[Index].Index + I] := Textures[I];
  End;
End; // TTextureSet.ReplaceTextures

Procedure TTextureSet.SetCurIndices;
Var I,J,Count,Interval: Integer;
Begin
  For J := 0 To High(FInfo) Do
  Begin
    Count := FInfo[J].Count;  // Copy to a local variable for speed purposes -- this routine gets called a *lot*
    If Count > 1 Then
    Begin
      Interval := FInfo[J].Interval;  // Copy to a local variable for speed purposes -- this routine gets called a *lot*
      If Interval > 0
       Then I := (FOwner.RedrawTime Mod (Count * Interval)) Div Interval
       Else I := 0;
      FInfo[J].CurIndex := FInfo[J].Index + I;
    End
    Else If Count = 1 Then FInfo[J].CurIndex := FInfo[J].Index
    Else FInfo[J].CurIndex := -1;
  End; // For J
End; // TTextureSet.SetCurIndices

Function TTextureSet.GetTexture(Index: Integer): TTexture;
Var I,Count,Interval: Cardinal;
Begin
  If (Index >= 0) And (Index <= High(FInfo)) Then
  Begin
    Count := FInfo[Index].Count;  // Copy to a local variable for speed purposes -- this routine gets called a *lot*
    If Count > 1 Then
    Begin
      Interval := FInfo[Index].Interval;  // Copy to a local variable for speed purposes -- this routine gets called a *lot*
      If Interval > 0
       Then I := (FOwner.RedrawTime Mod (Count * Interval)) Div Interval
       Else I := 0;
      Result := FTextures[FInfo[Index].Index + I];
    End
    Else Result := FTextures[FInfo[Index].Index];
  End
  Else Result := Nil;
End; // TTextureSet.GetTexture

Function TTextureSet.GetMaterialID(Index: Integer): Integer;
Var I,Count,Interval: Cardinal;
Begin
  If (Index >= 0) And (Index <= High(FInfo))
   Then Result := FInfo[Index].MatID
   Else Result := -1;
End; // TTextureSet.GetMaterialID

Function TTextureSet.NumTextures: Integer;
Begin
  Result := High(FInfo) + 1;
End; // TTextureSet.NumTextures

Procedure TTextureSet.CopyFrom(TextureSet: TTextureSet);
Var I: Integer;
Begin
  FOwner := TextureSet.FOwner;
  SetLength(FInfo,High(TextureSet.FInfo) + 1);
  SetLength(FTextures,High(TextureSet.FTextures) + 1);
  For I := 0 To High(FInfo)     Do FInfo[I]     := TextureSet.FInfo[I];
  For I := 0 To High(FTextures) Do FTextures[I] := TextureSet.FTextures[I];
End; // TTextureSet.CopyFrom

// ---------------------------
// TRenderable
// ---------------------------

Constructor TRenderable.Create;
Begin
  FModels        := TStringList.Create; // MUST NOT BE SORTED!!!
  FHasAlpha      := False;
  FHasTrans      := False;
  FScale         := 1;
  FAnimSpeed     := 1;
  FBaseHeight    := 1;
End; // TRenderable.Create

Destructor TRenderable.Destroy;
Begin
  FModels.Free;
End; // TRenderable.Destroy

Function TRenderable.ContainsModel(ID: String): Boolean;
Begin
  Result := (FModels.IndexOf(ID) >= 0);
End; // TRenderable.ContainsModel

Procedure TRenderable.AddModel(ID: String; Model: TModel);
Var I: Integer;
Begin
  FModels.AddObject(ID,Model);
  If FModels.Count > 1 Then
  Begin
    For I := 0 To FModels.Count - 1 Do TModel(FModels.Objects[I]).FAnimated := True;
  End;
End; // TRenderable.AddModel

Function TRenderable.NumFrames: Integer;
Begin
  Result := FModels.Count;
End; // TRenderable.NumFrames

Function TRenderable.LastFramePos: Single;
Var S: Single;
Begin
  S := NumFrames;
  If S > 0
   Then Result := (S - 1) / S
   Else Result := 0;
End; // TRenderable.LastFramePos

Function TRenderable.GetFramePos(Frame: Integer): Single;
Var S: Single;
Begin
  S := NumFrames;
  If Frame < 0 Then Result := 0
  Else If Frame >= S Then Result := LastFramePos
  Else If S > 0 Then Result := Frame / S
  Else Result := 0;
End; // TRenderable.GetFramePos

Procedure TRenderable.CheckOcclusion(Frame: Single; ModelTypes: String);
Var
  Index : Integer;
  Model : TModel;

Begin
  Index := Min(FModels.Count - 1,Trunc(Frame * FModels.Count));
  If (Index >= 0) And (Index < FModels.Count) Then
  Begin
    Model := TModel(FModels.Objects[Index]);
    Model.DoCheckOcclusion;
  End;
End; // TRenderable.CheckOcclusion

Procedure TRenderable.Redraw(Frame: Single; Const VertexColors: Array Of LongWord; AlphaNotOneOnly: Boolean; ModelTypes: String;
                             Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList);
// ---------------------------------------------------------------------
// This method must be called from within the scene's render thread
// ---------------------------------------------------------------------
Var
  Index : Integer;
  Model : TModel;

Begin
  Index := Min(FModels.Count - 1,Trunc(Frame * FModels.Count));
  If (Index >= 0) And (Index < FModels.Count) Then
  Begin
    Model := TModel(FModels.Objects[Index]);
    Model.Redraw(VertexColors,AlphaNotOneOnly,Nil,PieceTextures,ReplaceFromTexInfo,ReplaceToTexInfo,NearLight,NearDynamicLights);
    FHasAlpha   := FHasAlpha Or Model.HasAlpha;
    FHasTrans   := FHasTrans Or Model.HasTrans;
    FBaseHeight := TModel(FModels.Objects[Index]).BaseHeight;
  End;
End; // TRenderable.Redraw

Procedure TRenderable.CalcBaseHeight;
Begin
  If FModels.Count > 0 Then FBaseHeight := TModel(FModels.Objects[0]).BaseHeight;
End; // TRenderable.CalcBaseHeight

Function TRenderable.GetModel(Index: Integer): TModel;
Begin
  If (Index >= 0) And (Index < FModels.Count)
   Then Result := TModel(FModels.Objects[Index])
   Else Result := Nil;
End; // TRenderable.GetModel

Function TRenderable.GetModelID(Index: Integer): String;
Begin
  If (Index >= 0) And (Index < FModels.Count)
   Then Result := FModels.Strings[Index]
   Else Result := '';
End; // TRenderable.GetModelID

Procedure TRenderable.SetModelID(Index: Integer; ID: String);
Begin
  If (Index >= 0) And (Index < FModels.Count) Then FModels.Strings[Index] := ID;
End; // TRenderable.SetModelID

Function TRenderable.GetCount: Integer;
Begin
  Result := FModels.Count;
End; // TRenderable.GetCount

Function TRenderable.IsIntersectedBy(Frame: Single; Source,Dest: T3DPoint{I3dPoint}; ModelTypes: String): Boolean;
// WARNING: This modifies Dest if the result is true
Var Index: Integer;
Begin
  Index := Min(FModels.Count - 1,Trunc(Frame * FModels.Count));
  If (Index >= 0) And (Index < FModels.Count)
   Then Result := TModel(FModels.Objects[Index]).IsIntersectedBy(Source,Dest)
   Else Result := False;
End; // TRenderable.IsIntersectedBy

Procedure TRenderable.FindIntersection(Frame: Single; Ellipsoid: TEllipsoid; Velocity: T3DPoint{I3dPoint}; MovementSphere: TSphere; Var NearestDistance: Single; ClosestPoint: T3DPoint{I3dPoint}; Var Friction: Single);
Var Index: Integer;
Begin
  Index := Min(FModels.Count - 1,Trunc(Frame * FModels.Count));
  If (Index >= 0) And (Index < FModels.Count) Then TModel(FModels.Objects[Index]).FacesCollideWithEllipsoid(Ellipsoid,Velocity,MovementSphere,NearestDistance,ClosestPoint,Friction);
End; // TRenderable.FindIntersection

Function TRenderable.FindMinMaxZPoints(Frame: Single; Const X,Y: Single; Out MinZ,MaxZ: Single): Boolean;
Var Index: Integer;
Begin
  Index := Min(FModels.Count - 1,Trunc(Frame * FModels.Count));
  If (Index >= 0) And (Index < FModels.Count)
   Then Result := TModel(FModels.Objects[Index]).FindMinMaxZPoints(X,Y,MinZ,MaxZ)
   Else Result := False;
End; // TRenderable.FindMinMaxZPoints

// ---------------------------
// TSkeletonFrame
// ---------------------------

Constructor TSkeletonFrame.Create(AParent: TSkeletonRenderable);
Begin
  SetLength(Pieces,0);
  SetLength(Box,0);
  SetLength(BoxOk,0);
  FrameBox    := TAxisAlignedBox.Create;
  FrameSphere := TSphere.Create;
  FrameBoxOk  := False;
  Parent      := AParent;
End; // TSkeletonFrame.Create

Destructor TSkeletonFrame.Destroy;
Var I: Integer;
Begin
  SetLength(Pieces,0);
  For I := 0 To High(Box) Do Box[I].Free;
  SetLength(Box,0);
  SetLength(BoxOk,0);
  FrameBox.Free;
  FrameSphere.Free;
End; // TSkeletonFrame.Destroy

Procedure TSkeletonFrame.Copy(Frame,ReferenceFrame: TSkeletonFrame);
Var
  I,J,K : Integer;
  Found : Boolean;
  
Begin
  // Find out how many pieces have matching names

  K := 0;
  For I := 1 To ReferenceFrame.GetNumPieces - 1 Do
  Begin
    For J := 1 To Frame.GetNumPieces - 1 Do
    Begin
      If ReferenceFrame.Pieces[I].Name = Frame.Pieces[J].Name Then Inc(K);
    End; // For J
  End; // For I

  // Allocate enough pieces for the target skeleton. It might have more pieces than the source
  // skeleton, so we'll copy the pieces in the same order as the reference.

  SetLength(Pieces,K + Max(High(Frame.Pieces) - K + 1,0) + Max(High(ReferenceFrame.Pieces) - K + 1,0));
  For I := 0 To High(Pieces) Do
  Begin
    If I < High(ReferenceFrame.Pieces)
     Then Pieces[I] := ReferenceFrame.Pieces[I]
     Else Pieces[I] := Frame.Pieces[0];
  End; // For I

  // Copy the matching pieces, in the same order as they are in the reference frame

  For I := 1 To ReferenceFrame.GetNumPieces - 1 Do
  Begin
    For J := 1 To Frame.GetNumPieces - 1 Do
    Begin
      If ReferenceFrame.Pieces[I].Name = Frame.Pieces[J].Name Then Pieces[I] := Frame.Pieces[J];
    End; // For J
  End; // For I

  // Copy any pieces that were in the one we want to copy but aren't in the reference.  These
  // are usually the "point" pieces that denote where to place equipment/names/etc.

  K := ReferenceFrame.NumPieces;
  For I := 1 To Frame.NumPieces - 1 Do
  Begin
    J     := 1;
    Found := False;
    While (J < ReferenceFrame.NumPieces) And Not Found Do
    Begin
      If Frame.Pieces[I].Name = ReferenceFrame.Pieces[J].Name Then Found := True Else Inc(J);
    End; // While
    If (K <= High(Pieces)) And Not Found Then
    Begin
      Pieces[K] := Frame.Pieces[I];
      Inc(K);
    End;
  End; // For I

  // Make it recalculate the bounding box since we are probably attaching it to a different model

  For I := 0 To High(Box) Do Box[I].Free;
  SetLength(Box,High(Frame.Box) + 1);
  SetLength(BoxOk,High(Frame.BoxOk) + 1);
  For I := 0 To High(Box) Do
  Begin
    Box[I] := TAxisAlignedBox.Create;
    Box[I].Copy(Frame.Box[I]);
    BoxOk[I] := False;
  End; // For I
//  FBox.Copy(Frame.FBox);
//  BoxOk := False;
  FrameBoxOk := False;
End; // TSkeletonFrame.Copy

Procedure TSkeletonFrame.AddPiece(AName: String; X,Y,Z,A11,A12,A13,A21,A22,A23,A31,A32,A33,QW,QX,QY,QZ,QW0,QX0,QY0,QZ0,Angle,AX,AY,AZ,TX,TY,TZ,Angle0,AX0,AY0,AZ0,TX0,TY0,TZ0: Single; ParentIndex: Integer);
Begin
  SetLength(Pieces,High(Pieces) + 2);
  Pieces[High(Pieces)].Name   := AName;
  Pieces[High(Pieces)].X      := X; // Basically A14
  Pieces[High(Pieces)].Y      := Y; // Basically A24
  Pieces[High(Pieces)].Z      := Z; // Basically A34
  Pieces[High(Pieces)].M[1,1] := A11;
  Pieces[High(Pieces)].M[1,2] := A12;
  Pieces[High(Pieces)].M[1,3] := A13;
  Pieces[High(Pieces)].M[2,1] := A21;
  Pieces[High(Pieces)].M[2,2] := A22;
  Pieces[High(Pieces)].M[2,3] := A23;
  Pieces[High(Pieces)].M[3,1] := A31;
  Pieces[High(Pieces)].M[3,2] := A32;
  Pieces[High(Pieces)].M[3,3] := A33;
  Pieces[High(Pieces)].QW     := QW;    // Quaternion QW
  Pieces[High(Pieces)].QX     := QX;    // Quaternion QX
  Pieces[High(Pieces)].QY     := QY;    // Quaternion QY
  Pieces[High(Pieces)].QZ     := QZ;    // Quaternion QZ
  Pieces[High(Pieces)].QW0    := QW0;   // Quaternion QW
  Pieces[High(Pieces)].QX0    := QX0;   // Quaternion QX
  Pieces[High(Pieces)].QY0    := QY0;   // Quaternion QY
  Pieces[High(Pieces)].QZ0    := QZ0;   // Quaternion QZ
  Pieces[High(Pieces)].Angle  := Angle; // Angle in degrees
  Pieces[High(Pieces)].AX     := AX;    // Axis X
  Pieces[High(Pieces)].AY     := AY;    // Axis Y
  Pieces[High(Pieces)].AZ     := AZ;    // Axis Z
  Pieces[High(Pieces)].TX     := TX;    // Relative move X
  Pieces[High(Pieces)].TY     := TY;    // Relative move Y
  Pieces[High(Pieces)].TZ     := TZ;    // Relative move Z
  Pieces[High(Pieces)].Angle0 := Angle0; // Angle in degrees
  Pieces[High(Pieces)].AX0    := AX0;    // Axis X
  Pieces[High(Pieces)].AY0    := AY0;    // Axis Y
  Pieces[High(Pieces)].AZ0    := AZ0;    // Axis Z
  Pieces[High(Pieces)].TX0    := TX0;    // Relative move X
  Pieces[High(Pieces)].TY0    := TY0;    // Relative move Y
  Pieces[High(Pieces)].TZ0    := TZ0;    // Relative move Z
  Pieces[High(Pieces)].Parent := ParentIndex;    // Parent index
End; // TSkeletonFrame.AddPiece

Procedure TSkeletonFrame.SetPiece(Index: Integer; AName: String; X,Y,Z,A11,A12,A13,A21,A22,A23,A31,A32,A33,QW,QX,QY,QZ,QW0,QX0,QY0,QZ0,Angle,AX,AY,AZ,TX,TY,TZ,Angle0,AX0,AY0,AZ0,TX0,TY0,TZ0: Single; ParentIndex: Integer);
Begin
  If High(Pieces) < Index Then SetLength(Pieces,Index + 1);
  Pieces[Index].Name   := AName;
  Pieces[Index].X      := X; // Basically A14
  Pieces[Index].Y      := Y; // Basically A24
  Pieces[Index].Z      := Z; // Basically A34
  Pieces[Index].M[1,1] := A11;
  Pieces[Index].M[1,2] := A12;
  Pieces[Index].M[1,3] := A13;
  Pieces[Index].M[2,1] := A21;
  Pieces[Index].M[2,2] := A22;
  Pieces[Index].M[2,3] := A23;
  Pieces[Index].M[3,1] := A31;
  Pieces[Index].M[3,2] := A32;
  Pieces[Index].M[3,3] := A33;
  Pieces[Index].QW     := QW;    // Quaternion QW
  Pieces[Index].QX     := QX;    // Quaternion QX
  Pieces[Index].QY     := QY;    // Quaternion QY
  Pieces[Index].QZ     := QZ;    // Quaternion QZ
  Pieces[Index].QW0    := QW0;   // Quaternion QW
  Pieces[Index].QX0    := QX0;   // Quaternion QX
  Pieces[Index].QY0    := QY0;   // Quaternion QY
  Pieces[Index].QZ0    := QZ0;   // Quaternion QZPieces[Index].Angle  := Angle; // Angle in degrees
  Pieces[Index].AX     := AX;    // Axis X
  Pieces[Index].AY     := AY;    // Axis Y
  Pieces[Index].AZ     := AZ;    // Axis Z
  Pieces[Index].TX     := TX;    // Relative move X
  Pieces[Index].TY     := TY;    // Relative move Y
  Pieces[Index].TZ     := TZ;    // Relative move Z
  Pieces[Index].Angle0 := Angle0; // Angle in degrees
  Pieces[Index].AX0    := AX0;    // Axis X
  Pieces[Index].AY0    := AY0;    // Axis Y
  Pieces[Index].AZ0    := AZ0;    // Axis Z
  Pieces[Index].TX0    := TX0;    // Relative move X
  Pieces[Index].TY0    := TY0;    // Relative move Y
  Pieces[Index].TZ0    := TZ0;    // Relative move Z
  Pieces[Index].Parent := ParentIndex;    // Parent index
End; // TSkeletonFrame.SetPiece

Procedure TSkeletonFrame.CalculateBoundingBox(Model: TModel; Scale: Single; Out MinX,MinY,MinZ,MaxX,MaxY,MaxZ: Single);
Var
  I,J,K     : Integer;
  X,Y,Z     : Single;
  X1,Y1,Z1  : Single;
  Piece     : PSkeletonPiece;

Begin
  I := High(Model.Vertices) + 1;
  K := 0;
  MinX :=  999999;
  MinY :=  999999;
  MinZ :=  999999;
  MaxX := -999999;
  MaxY := -999999;
  MaxZ := -999999;
  For J := 0 To I - 1 Do
  Begin
    If (Model.PieceIndices[J] >= 0) And (Model.PieceIndices[J] <= High(Pieces)) Then
    Begin
      Piece := @(Pieces[Model.PieceIndices[J]]);
      X     := Model.Positions.DataArray[K];
      Y     := Model.Positions.DataArray[K + 1];
      Z     := Model.Positions.DataArray[K + 2];
      X1    := (Piece.M[1,1] * X + Piece.M[1,2] * Y + Piece.M[1,3] * Z + Piece.X) * Scale;
      Y1    := (Piece.M[2,1] * X + Piece.M[2,2] * Y + Piece.M[2,3] * Z + Piece.Y) * Scale;
      Z1    := (Piece.M[3,1] * X + Piece.M[3,2] * Y + Piece.M[3,3] * Z + Piece.Z) * Scale;
      If X1 < MinX Then MinX := X1;
      If Y1 < MinY Then MinY := Y1;
      If Z1 < MinZ Then MinZ := Z1;
      If X1 > MaxX Then MaxX := X1;
      If Y1 > MaxY Then MaxY := Y1;
      If Z1 > MaxZ Then MaxZ := Z1;
    End;
//    Else Raise Exception.Create('CalculateBoundingBox: piece index ' + IntToStr(Model.PieceIndices[J]) + ' is out of the range [0..' + IntToStr(High(Pieces)) + ']');
    Inc(K,3);
  End; // For J
End; // TSkeletonFrame.CalculateBoundingBox

Procedure TSkeletonFrame.SetupFrameBox(Model: TModelArray; ModelIndex: TIntegerArray; Scale: Single; DoModelBox: Boolean);
Var
  I,J : Integer;

Begin
  J := Min(High(Model),High(ModelIndex));
  For I := 0 To J Do
  Begin
    If DoModelBox Then SetupModelBox(Model[I],ModelIndex[I],Scale);
    If I = 0
     Then FrameBox.Copy(Model[I].Box)
     Else FrameBox.Expand(Model[I].Box);
  End; // For I
  FrameSphere.Center.Copy(FrameBox.Center);
  FrameSphere.Radius := FrameBox.MinPt.DistanceFrom(FrameBox.MaxPt) / 2;
  FrameBoxOk := True;
End; // TSkeletonFrame.SetupFrameBox

Procedure TSkeletonFrame.SetupModelBox(Model: TModel; ModelIndex: Integer; Scale: Single);
Var MinX,MinY,MinZ,MaxX,MaxY,MaxZ: Single;
Begin
  If (High(BoxOk) < ModelIndex) Or Not BoxOk[ModelIndex] Then
  Begin
    While High(BoxOk) < ModelIndex Do
    Begin
      SetLength(Box,High(Box) + 2);
      SetLength(BoxOk,High(BoxOk) + 2);
      BoxOk[High(BoxOk)] := False;
      Box[High(Box)] := TAxisAlignedBox.Create;
    End; // While
    CalculateBoundingBox(Model,Scale,MinX,MinY,MinZ,MaxX,MaxY,MaxZ);
    Box[ModelIndex].MinPt.Copy(MinX,MinY,MinZ);
    Box[ModelIndex].MaxPt.Copy(MaxX,MaxY,MaxZ);
    BoxOk[ModelIndex] := True;
  End;
  Model.Box.Copy(Box[ModelIndex]);
  Model.CalcExtents(True);
End; // TSkeletonFrame.SetupModelBox

Procedure TSkeletonFrame.CheckOcclusion(Model: TModel; ModelIndex: Integer; Scale: Single; FrameNum: Single);
Begin
  // Do nothing
End; // TSkeletonFrame.CheckOcclusion

Procedure TSkeletonFrame.Redraw(Model: TModel; ModelIndex: Integer; Const VertexColors: Array Of LongWord; AlphaNotOneOnly: Boolean;
                                Scale: Single; Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList; FrameNum: Single);
// ---------------------------------------------------------------------
// This method must be called from within the scene's render thread
// ---------------------------------------------------------------------
Var
  I          : Integer;
  IntFrame   : Integer;
  IntFrame1  : Integer;
  FracFrame  : Single;
  Q,Q0,Q1    : TQuaternion;
  V,V1       : T3DPoint{I3dPoint};
  M          : T3x3Matrix;
  NextFrame  : TSkeletonFrame;      

  Procedure ProcessParent(Const Frame,NextFrame: TSkeletonFrame; Piece: Integer; FracFrame: Single; Q,Q0,Q1: TQuaternion; V,V1: T3DPoint{I3dPoint});
  Var I: Integer;
  Begin
    I := Frame.Pieces[Piece].Parent;
    If (I >= 0) And (I < Piece) Then ProcessParent(Frame,NextFrame,I,FracFrame,Q,Q0,Q1,V,V1);

    Q0.Copy(Frame.Pieces[Piece].QW0,     Frame.Pieces[Piece].QX0,     Frame.Pieces[Piece].QY0,     Frame.Pieces[Piece].QZ0);
    Q1.Copy(NextFrame.Pieces[Piece].QW0, NextFrame.Pieces[Piece].QX0, NextFrame.Pieces[Piece].QY0, NextFrame.Pieces[Piece].QZ0);

    V1.Copy(Frame.Pieces[Piece].TX0,Frame.Pieces[Piece].TY0,Frame.Pieces[Piece].TZ0);
    Q.Transform(V1);
    V.Add(V1);

    Q0.Slerp(Q1,FracFrame);
    Q.Multiply(Q0);
    Q.Normalize;
  End; // ProcessParent

Begin
  Try
    Frame := FrameNum;
    SetupModelBox(Model,ModelIndex,Scale);
    glPushMatrix;
    glScalef(Scale,Scale,Scale);

    FracFrame := Frac(Frame);
    IntFrame  := Trunc(Frame);
    IntFrame1 := (IntFrame + 1) Mod (High(Parent.FFrames) + 1);
    NextFrame := Parent.Frame[IntFrame1];
    Q  := TQuaternion.Create;
    Q0 := TQuaternion.Create;
    Q1 := TQuaternion.Create;
    V  := T3DPoint.Create{Point};
    V1 := T3DPoint.Create{Point};
    M  := T3x3Matrix.Create;
    For I := 0 To High(Pieces) Do
    Begin
      Q.Copy(1,0,0,0);
      V.Copy(0,0,0);
      ProcessParent(Self,NextFrame,I,FracFrame,Q,Q0,Q1,V,V1);
      Pieces[I].TX1 := V.X;
      Pieces[I].TY1 := V.Y;
      Pieces[I].TZ1 := V.Z;
      Q.ToAngleAxis(Pieces[I].Angle1,Pieces[I].AX1,Pieces[I].AY1,Pieces[I].AZ1);
      Pieces[I].Angle1 := Pieces[I].Angle1 * 180 / Pi;
      Q.ToRotationMatrix(M);
      Pieces[I].M1[1,1] := M.M[1,1];
      Pieces[I].M1[1,2] := M.M[1,2];
      Pieces[I].M1[1,3] := M.M[1,3];
      Pieces[I].M1[2,1] := M.M[2,1];
      Pieces[I].M1[2,2] := M.M[2,2];
      Pieces[I].M1[2,3] := M.M[2,3];
      Pieces[I].M1[3,1] := M.M[3,1];
      Pieces[I].M1[3,2] := M.M[3,2];
      Pieces[I].M1[3,3] := M.M[3,3];
    End; // For I
    M.Free;
    Q.Free;
    Q0.Free;
    Q1.Free;
    V.Free;
    V1.Free;
//    V  := Nil;
//    V1 := Nil;

    Model.Redraw(VertexColors,AlphaNotOneOnly,Self,PieceTextures,ReplaceFromTexInfo,ReplaceToTexInfo,NearLight,NearDynamicLights);
    glPopMatrix;
  Except
    On E: Exception Do Raise Exception.Create(E.Message + ' (TSkeletonFrame.Redraw)' );
  End;
End; // TSkeletonFrame.Redraw

Function TSkeletonFrame.IsIntersectedBy(Model: TModel; ModelIndex: Integer; Source,Dest: T3DPoint{I3dPoint}; Scale: Single): Boolean;
// WARNING: This modifies Dest if the result is true
Var
  I,J,K,L  : Integer;
  Found    : Boolean;
  P        : Array[0..2] Of T3DPoint{I3dPoint};
  N        : T3DPoint{I3dPoint};
  Piece    : PSkeletonPiece;
  X1,Y1,Z1 : Single;

Begin
  SetupModelBox(Model,ModelIndex,Scale);

  If Box[ModelIndex].IntersectionWithLineSegment(Source,Dest) >= 0 Then
  Begin
    Model.FRayWork.Copy(Dest);
    Model.FRayWork.Subtract(Source);
    I     := 0;
    J     := 0;
    K     := 0;
    Found := False;
    P[0]  := T3DPoint.Create{Point};
    P[1]  := T3DPoint.Create{Point};
    P[2]  := T3DPoint.Create{Point};
    N     := T3DPoint.Create{Point};
    While (I <= High(Model.Faces)) And Not Found Do
    Begin
      For L := 0 To 2 Do
      Begin
        P[L].Copy(Model.Positions.DataArray[K + 0 + L * 3],
                  Model.Positions.DataArray[K + 1 + L * 3],
                  Model.Positions.DataArray[K + 2 + L * 3]);
        If (Model.PieceIndices[J + L] >= 0) And (Model.PieceIndices[J + L] <= High(Pieces)) Then
        Begin
          Piece := @(Pieces[Model.PieceIndices[J + L]]);

          X1    := (Piece^.M1[1,1] * P[L].X + Piece^.M1[1,2] * P[L].Y + Piece^.M1[1,3] * P[L].Z + Piece^.TX1) * Scale;
          Y1    := (Piece^.M1[2,1] * P[L].X + Piece^.M1[2,2] * P[L].Y + Piece^.M1[2,3] * P[L].Z + Piece^.TY1) * Scale;
          Z1    := (Piece^.M1[3,1] * P[L].X + Piece^.M1[3,2] * P[L].Y + Piece^.M1[3,3] * P[L].Z + Piece^.TZ1) * Scale;
{
          X1    := (Piece^.M[1,1] * P[L].X + Piece^.M[1,2] * P[L].Y + Piece^.M[1,3] * P[L].Z + Piece^.X) * Scale;
          Y1    := (Piece^.M[2,1] * P[L].X + Piece^.M[2,2] * P[L].Y + Piece^.M[2,3] * P[L].Z + Piece^.Y) * Scale;
          Z1    := (Piece^.M[3,1] * P[L].X + Piece^.M[3,2] * P[L].Y + Piece^.M[3,3] * P[L].Z + Piece^.Z) * Scale;
}
          P[L].Copy(X1,Y1,Z1);
        End
        Else Raise Exception.Create('IsIntersectedBy: piece index ' + IntToStr(Model.PieceIndices[J + L]) + ' is out of the range [0..' + IntToStr(High(Pieces)) + ']');
      End; // For L
      N.GetNormalTo(P[0],P[1],P[2]);
      If Model.FRayWork.Dot(N) < 0 Then
      Begin
        If gts_segment_triangle_intersection(Source,Dest,P[2],P[1],P[0],True,Dest) Then Found := True;
      End;
      Inc(I);
      Inc(J,3);
      Inc(K,9);
    End; // While

    N.Free;
    P[2].Free;
    P[1].Free;
    P[0].Free;
{
    N    := Nil;
    P[2] := Nil;
    P[1] := Nil;
    P[0] := Nil;
}    
    Result := Found;
  End
  Else Result := False;
End; // TSkeletonFrame.IsIntersectedBy

Function TSkeletonFrame.GetNumPieces: Integer;
Begin
  Result := High(Pieces) + 1;
End; // TSkeletonFrame.GetNumPieces

// ---------------------------
// TSkeletonRenderable
// ---------------------------

Constructor TSkeletonRenderable.Create;
Begin
  Inherited;
  SetLength(FFrames,0);
  FFrame := -1;
//  RenderAllModels := False;
End; // TSkeletonRenderable.Create

Destructor TSkeletonRenderable.Destroy;
Var I: Integer;
Begin
  For I := 0 To High(FFrames) Do FFrames[I].Free;
  SetLength(FFrames,0);
  Inherited;
End; // TSkeletonRenderable.Destroy

Procedure TSkeletonRenderable.AddFrame(Frame: TSkeletonFrame);
Begin
  SetLength(FFrames,High(FFrames) + 2);
  FFrames[High(FFrames)] := Frame;
  FFrame := -1;
End; // TSkeletonRenderable.AddFrame

Function TSkeletonRenderable.NumFrames: Integer;
Begin
  Result := High(FFrames) + 1;
End; // TSkeletonRenderable.NumFrames

Procedure TSkeletonRenderable.CheckOcclusion(Frame: Single; ModelTypes: String);
Begin
  // Do nothing
End; // TSkeletonRenderable.CheckOcclusion

Procedure TSkeletonRenderable.Redraw(Frame: Single; Const VertexColors: Array Of LongWord; AlphaNotOneOnly: Boolean; ModelTypes: String;
                                     Const PieceTextures: TTextureSetList; Const ReplaceFromTexInfo,ReplaceToTexInfo: String; NearLight: TColor; NearDynamicLights: TStringList);
Var
  Index      : Integer;
  I          : Integer;
  Model      : TModel;
  ID         : String;
  Default    : String;
  FrameIndex : Single;
  ModelList  : TModelArray;
  ModelIndex : TIntegerArray;

Begin
//  Try
    Frame      := Min(Frame,0.9999);
    FrameIndex := Frame * (High(FFrames) + 1);  // Have to do this to avoid nasty roundoff error bugs
    Index      := Trunc(FrameIndex);
    If (Index >= 0) And (Index <= High(FFrames)) Then
    Begin
      If FFrame < 0 Then
      Begin
        For I := 0 To FModels.Count - 1 Do
        Begin
          Model := TModel(FModels.Objects[I]);
          Model.FAnimated := True;
          Model.BaseHeight := -1; // Reset the model's BaseHeight value so it can be recalculated properly
          If I <= High(FFrames[Index].BoxOk) Then FFrames[Index].BoxOk[I] := False;
        End; // For I
      End;
      FHasAlpha := False;
      FHasTrans := False;

      If ModelTypes = '' Then//RenderAllModels Then
      Begin
        FBaseHeight := 0;
        If Not FFrames[Index].FrameBoxOk Then
        Begin
          SetLength(ModelList,FModels.Count);
          SetLength(ModelIndex,FModels.Count);
        End;
        For I := 0 To FModels.Count - 1 Do
        Begin
          Model := TModel(FModels.Objects[I]);
          Model.FAnimated := True;
          FHasAlpha := FHasAlpha Or Model.HasAlpha;
          FHasTrans := FHasTrans Or Model.HasTrans;
          FFrames[Index].Redraw(Model,I,VertexColors,AlphaNotOneOnly,FScale,PieceTextures,ReplaceFromTexInfo,ReplaceToTexInfo,NearLight,NearDynamicLights,FrameIndex);
  //        FBaseHeight := Max(FBaseHeight,Max(Model.Box.MaxPt.Z - Model.Box.MinPt.Z,Max(Model.Box.MaxPt.X - Model.Box.MinPt.X,Model.Box.MaxPt.Y - Model.Box.MinPt.Y)));

          If Not FFrames[Index].FrameBoxOk Then
          Begin
            ModelList[I] := Model;
            ModelIndex[I] := I;
          End;
        End; // For I
        If Not FFrames[Index].FrameBoxOk Then
        Begin
          FFrames[Index].SetupFrameBox(ModelList,ModelIndex,FScale,False);
          SetLength(ModelList,0);
          SetLength(ModelIndex,0);
        End;

        FBaseHeight := Max(FFrames[Index].FrameBox.MaxPt.Z - FFrames[Index].FrameBox.MinPt.Z,Max(FFrames[Index].FrameBox.MaxPt.X - FFrames[Index].FrameBox.MinPt.X,FFrames[Index].FrameBox.MaxPt.Y - FFrames[Index].FrameBox.MinPt.Y));
      End
      Else
      Begin
        // Render all the models that ModelTypes calls out (it contains a comma-delimited list of model ID's)

        FBaseHeight := 0;
        If Not FFrames[Index].FrameBoxOk Then
        Begin
          SetLength(ModelList,0);
          SetLength(ModelIndex,0);
        End;
        While HasMoreTokens(',',ModelTypes) Do
        Begin
          ID := GetNextToken(',',ModelTypes);
          If ID <> '' Then
          Begin
            I := Pos('/',ID);
            If I > 0 Then
            Begin
              Default := Trim(Copy(ID,I + 1,Length(ID)));
              ID := Trim(Copy(ID,1,I - 1));
            End
            Else Default := '';
            I := FModels.IndexOf(ID);
            If (I < 0) And (Default <> '') Then
            Begin
              I := FModels.IndexOf(Default);
            End;
            If I >= 0 Then
            Begin
              Model := TModel(FModels.Objects[I]);
              Model.FAnimated := True;
              FFrames[Index].Redraw(Model,I,VertexColors,AlphaNotOneOnly,FScale,PieceTextures,ReplaceFromTexInfo,ReplaceToTexInfo,NearLight,NearDynamicLights,Frame * (High(FFrames) + 1));
              FHasAlpha := FHasAlpha Or Model.HasAlpha;
              FHasTrans := FHasTrans Or Model.HasTrans;
  //            FBaseHeight := Max(FBaseHeight,Max(Model.Box.MaxPt.Z - Model.Box.MinPt.Z,Max(Model.Box.MaxPt.X - Model.Box.MinPt.X,Model.Box.MaxPt.Y - Model.Box.MinPt.Y)));

              If Not FFrames[Index].FrameBoxOk Then
              Begin
                SetLength(ModelList,High(ModelList) + 2);
                SetLength(ModelIndex,High(ModelIndex) + 2);
                ModelList[High(ModelList)] := Model;
                ModelIndex[High(ModelIndex)] := I;
              End;
            End;
            If Not FFrames[Index].FrameBoxOk Then
            Begin
              FFrames[Index].SetupFrameBox(ModelList,ModelIndex,FScale,False);
              SetLength(ModelList,0);
              SetLength(ModelIndex,0);
            End;
          End;
        End; // While
        FBaseHeight := Max(FFrames[Index].FrameBox.MaxPt.Z - FFrames[Index].FrameBox.MinPt.Z,Max(FFrames[Index].FrameBox.MaxPt.X - FFrames[Index].FrameBox.MinPt.X,FFrames[Index].FrameBox.MaxPt.Y - FFrames[Index].FrameBox.MinPt.Y));
  (*
        // First render the base "body" mesh (if the model type 256 or greater, then the last model is an alternate body model
        // and we should use that instead)

        If Abs(ModelType) >= 256 Then I := FModels.Count - 1 Else I := 0;
        Model := TModel(FModels.Objects[I]);
        Model.FAnimated := True;
        FFrames[Index].Redraw(Model,I,VertexColors,AlphaNotOneOnly,FScale,PieceTextures,NearLight);
        FHasAlpha := FHasAlpha Or Model.HasAlpha;
        FHasTrans := FHasTrans Or Model.HasTrans;

        // Now try to render the alternate "head" mesh.  There might be more texturesets than models
        // so make a separate model index.

        I := (Abs(ModelType) And $FF) + 1;
  {
        If ModelType < 0
         Then I := ((-ModelType) And $FF) + 1
         Else I := (ModelType And $FF) + 1;
  }
        If I >= FModels.Count Then I := 1;
        If I < FModels.Count Then
        Begin
          Model := TModel(FModels.Objects[I]);
          Model.FAnimated := True;
          FFrames[Index].Redraw(Model,I,VertexColors,AlphaNotOneOnly,FScale,PieceTextures,NearLight);
          FHasAlpha := FHasAlpha Or Model.HasAlpha;
          FHasTrans := FHasTrans Or Model.HasTrans;
        End;
  *)
      End;
      FFrame := Index;
    End;
//  Except
//    On E: Exception Do Raise Exception.Create(E.Message + ' (TSkeletonRenderable.Redraw)' );
//  End;
End; // TSkeletonRenderable.Redraw

Procedure TSkeletonRenderable.CalcBaseHeight;
Var
  I          : Integer;
  Model      : TModel;
  ModelList  : TModelArray;
  ModelIndex : TIntegerArray;

Begin
  If High(FFrames) >= 0 Then
  Begin
    SetLength(ModelList,FModels.Count);
    SetLength(ModelIndex,FModels.Count);
    For I := 0 To FModels.Count - 1 Do
    Begin
      Model := TModel(FModels.Objects[I]);
      ModelList[I] := Model;
      ModelIndex[I] := I;
    End; // For I
    If Not FFrames[0].FrameBoxOk Then FFrames[0].SetupFrameBox(ModelList,ModelIndex,FScale,True);
    SetLength(ModelList,0);
    SetLength(ModelIndex,0);
    FBaseHeight := Max(FFrames[0].FrameBox.MaxPt.Z - FFrames[0].FrameBox.MinPt.Z,Max(FFrames[0].FrameBox.MaxPt.X - FFrames[0].FrameBox.MinPt.X,FFrames[0].FrameBox.MaxPt.Y - FFrames[0].FrameBox.MinPt.Y));
  End;
End; // TSkeletonRenderable.CalcBaseHeight

Procedure TSkeletonRenderable.BuildModelList(ModelTypes: String; Var ModelList: TModelArray; Var ModelIndices: TIntegerArray);
Var
  St      : String;
  ID      : String;
  Default : String;
  I,J     : Integer;

Begin
  // Count the number of models we will need

  St := ModelTypes;
  J  := 0;
  While HasMoreTokens(',',St) Do
  Begin
    ID := GetNextToken(',',St);
    If ID <> '' Then
    Begin
      I := Pos('/',ID);
      If I > 0 Then
      Begin
        Default := Trim(Copy(ID,I + 1,Length(ID)));
        ID := Trim(Copy(ID,1,I - 1));
      End
      Else Default := '';
      I := FModels.IndexOf(ID);
      If (I < 0) And (Default <> '') Then
      Begin
        I := FModels.IndexOf(Default);
      End;
      If I >= 0 Then Inc(J);
    End;
  End; // While

  // Allocate space for the models

  SetLength(ModelList,J);
  SetLength(ModelIndices,J);

  // Build the lists

  J := 0;
  While HasMoreTokens(',',ModelTypes) Do
  Begin
    ID := GetNextToken(',',ModelTypes);
    If ID <> '' Then
    Begin
      I := Pos('/',ID);
      If I > 0 Then
      Begin
        Default := Trim(Copy(ID,I + 1,Length(ID)));
        ID := Trim(Copy(ID,1,I - 1));
      End
      Else Default := '';
      I := FModels.IndexOf(ID);
      If (I < 0) And (Default <> '') Then
      Begin
        I := FModels.IndexOf(Default);
      End;
      If I >= 0 Then
      Begin
        ModelList[J]    := TModel(FModels.Objects[I]);
        ModelIndices[J] := I;
        Inc(J);
      End;
    End;
  End; // While
End; // TSkeletonRenderable.BuildModelList

Function TSkeletonRenderable.IsIntersectedBy(Frame: Single; Source,Dest: T3DPoint{I3dPoint}; ModelTypes: String): Boolean;
// WARNING: This modifies Dest if the result is true
Var
  I       : Integer;
  Index   : Integer;
  Found   : Boolean;
  ID      : String;
  Default : String;

Begin
  Index := Trunc(Frame * High(FFrames));
  If (Index >= 0) And (Index <= High(FFrames)) Then
  Begin
    If ModelTypes = '' Then//RenderAllModels Then
    Begin
      I := 0;
      Found := False;
      While (I < FModels.Count) And Not Found Do
      Begin
        Found := Found Or FFrames[Index].IsIntersectedBy(TModel(FModels.Objects[I]),I,Source,Dest,Scale);
        Inc(I);
      End; // While
      Result := Found;
    End
    Else
    Begin
      Found := False;
      While HasMoreTokens(',',ModelTypes) And Not Found Do
      Begin
        ID := GetNextToken(',',ModelTypes);
        If ID <> '' Then
        Begin
          I := Pos('/',ID);
          If I > 0 Then
          Begin
            Default := Trim(Copy(ID,I + 1,Length(ID)));
            ID := Trim(Copy(ID,1,I - 1));
          End
          Else Default := '';
          I := FModels.IndexOf(ID);
          If (I < 0) And (Default <> '') Then
          Begin
            I := FModels.IndexOf(Default);
          End;
          If I >= 0 Then
          Begin
            Found := Found Or FFrames[Index].IsIntersectedBy(TModel(FModels.Objects[I]),I,Source,Dest,Scale);
          End;
        End;
      End; // While
      Result := Found;
    End;
  End
  Else Result := False;
End; // TSkeletonRenderable.IsIntersectedBy

Function TSkeletonRenderable.GetFrame(Index: Integer): TSkeletonFrame;
Begin
  If (Index >= 0) And (Index <= High(FFrames))
   Then Result := FFrames[Index]
   Else Result := Nil;
End; // TSkeletonRenderable.GetFrame

// ---------------------------
// TThreadSafeList
// ---------------------------

Constructor TThreadSafeList.Create;
Begin
  SetLength(FItems,16);
  FMutex        := TCriticalSection.Create;
  FOnCreateItem := Nil;
  FMaxIndex     := -1;
End; // TThreadSafeList.Create

Destructor TThreadSafeList.Destroy;
Begin
  FreeAll;
  SetLength(FItems,0);
  FMutex.Free;
End; // TThreadSafeList.Destroy

Procedure TThreadSafeList.Lock;
Begin
  FMutex.Enter;
End; // TThreadSafeList.Lock

Procedure TThreadSafeList.Unlock;
Begin
  FMutex.Leave;
End; // TThreadSafeList.Unlock

Function TThreadSafeList.GetNew(Option: Integer): TObject;
Var Obj: TObject;
Begin
  Result := Nil;
  If Assigned(FOnCreateItem) Then
  Begin
    Obj    := FOnCreateItem(Option);
    Result := Obj;
    Try
      FMutex.Enter;
      Allocate(1);
      FItems[FMaxIndex] := Obj;
    Finally
      FMutex.Leave;
    End;
  End;
End; // TThreadSafeList.GetNew

Function TThreadSafeList.GetNew(Count,Option: Integer): TList;
Var
  List : TList;
  I,J  : Integer;
  Obj  : TObject;

Begin
  List   := TList.Create;
  Result := List;
  If Assigned(FOnCreateItem) Then
  Begin
    Try
      FMutex.Enter;
      J := FMaxIndex + 1;
      Allocate(Count);
      For I := 1 To Count Do
      Begin
        Obj       := FOnCreateItem(Option);
        FItems[J] := Obj;
        List.Add(Obj);
        Inc(J);
      End; // For I
    Finally
      FMutex.Leave;
    End;
  End;
End; // TThreadSafeList.GetNew

Procedure TThreadSafeList.FreeAll;
Var I: Integer;
Begin
  Try
    FMutex.Enter;
    For I := 0 To FMaxIndex Do FItems[I].Free;
    SetLength(FItems,16);
    FMaxIndex := -1;
  Finally
    FMutex.Leave;
  End;
End; // TThreadSafeList.FreeAll

Procedure TThreadSafeList.Clear;
Begin
  Try
    FMutex.Enter;
    SetLength(FItems,16);
    FMaxIndex := -1;
  Finally
    FMutex.Leave;
  End;
End; // TThreadSafeList.Clear

Function TThreadSafeList.GetCount: Integer;
Begin
  Try
    FMutex.Enter;
    Result := FMaxIndex + 1;
  Finally
    FMutex.Leave;
  End;
End; // TThreadSafeList.GetCount

Function TThreadSafeList.GetItem(Index: Integer): TObject;
Begin
  Try
    FMutex.Enter;
    If (Index >= 0) And (Index <= FMaxIndex)
     Then Result := FItems[Index]
     Else Result := Nil;
  Finally
    FMutex.Leave;
  End;
End; // TThreadSafeList.GetItem

Procedure TThreadSafeList.Add(Item: TObject);
Begin
  Try
    FMutex.Enter;
    Allocate(1);
    FItems[FMaxIndex] := Item;
  Finally
    FMutex.Leave;
  End;
End; // TThreadSafeList.Add

Procedure TThreadSafeList.FreeItems(List: TList);
Var
  I    : Integer;
  Item : Pointer;

Begin
  Try
    FMutex.Enter;
    For I := 0 To List.Count - 1 Do
    Begin
      Item := List.Items[I];
      FreeItem(Item);
    End; // For I
  Finally
    FMutex.Leave;
  End;
End; // TThreadSafeList.FreeItems

Procedure TThreadSafeList.FreeItem(Item: TObject);
Var
  I     : Integer;
  Found : Boolean;

Begin
  Try
    FMutex.Enter;
    I     := 0;
    Found := False;
    While (I <= FMaxIndex) And Not Found Do
    Begin
      If FItems[I] = Item Then
      Begin
        Found := True;
        FItems[I].Free;
        While I < FMaxIndex Do
        Begin
          FItems[I] := FItems[I + 1];
          Inc(I);
        End; // While
        Dec(FMaxIndex);
      End
      Else Inc(I);
    End; // While
  Finally
    FMutex.Leave;
  End;
End;

Procedure TThreadSafeList.Allocate(Amount: Integer);
Var I: Integer;
Begin
  Try
    FMutex.Enter;
    If FMaxIndex + Amount > High(FItems) Then
    Begin
      I := High(FItems) + 1;
      If I < Amount Then I := Amount;
      SetLength(FItems,(High(FItems) + 1) + I);
    End;
    Inc(FMaxIndex,Amount);
  Finally
    FMutex.Leave;
  End;
End; // TThreadSafeList.Allocate

Function TThreadSafeList.GetIndexOf(Item: TObject): Integer;
Var
  I     : Integer;
  Found : Boolean;

Begin
  Try
    FMutex.Enter;
    I     := 0;
    Found := False;
    While (I <= FMaxIndex) And Not Found Do
    Begin
      If FItems[I] = Item Then Found := True Else Inc(I);
    End; // While
    If Found
     Then Result := I
     Else Result := -1;
  Finally
    FMutex.Leave;
  End;
End; // TThreadSafeList.GetIndexOf

Procedure TThreadSafeList.Delete(Index: Integer);
Var I: Integer;
Begin
  Try
    FMutex.Enter;
    If (Index >= 0) And (Index <= FMaxIndex) Then
    Begin
      For I := Index To FMaxIndex - 1 Do FItems[I] := FItems[I + 1];
      Dec(FMaxIndex);
    End;
  Finally
    FMutex.Leave;
  End;
End; // TThreadSafeList.Delete

// ---------------------------
// TSkySphere
// ---------------------------

Constructor TSkySphere.Create(AOwner: TSceneGL);
Begin
  FOwner    := AOwner;
  FVisible  := False;
  FSize     := 100;
  FXRotate  := 0;
  FYRotate  := 0;
  FZRotate  := 0;
  FXRotate0 := -999;
  FYRotate0 := -999;
  FZRotate0 := -999;
  FXRotate1 := -999;
  FYRotate1 := -999;
  FZRotate1 := -999;
  SetLength(FVertexColors,0);
  FModel    := TModel.Create(AOwner);
  FModel.CheckOcclusion := False;
End; // TSkySphere.Create

Destructor TSkySphere.Destroy;
Begin
  FModel.Free;
  SetLength(FVertexColors,0);
End; // TSkySphere.Destroy

Procedure TSkySphere.Redraw;
Var
//  VertexColors : Array Of LongWord;
  bDepthTest   : Boolean;
  TexSet       : TTextureSetList;

Begin
  If FVisible And Not FUpdating Then
  Begin
    glGetBooleanv(GL_DEPTH_TEST,@bDepthTest);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glEnable(GL_TEXTURE_2D);

    Tex2D := True;
    glPushMatrix;
    glTranslatef(FOwner.DefaultCamera.Position.X,FOwner.DefaultCamera.Position.Y,FOwner.DefaultCamera.Position.Z);

    If Clouds Then glTranslatef(0,0,-20);  // Bring down a bit toward the horizon

    glRotatef(FYRotate,0,1,0);
    glRotatef(FZRotate,0,0,1);
    glRotatef(FXRotate,1,0,0);

    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    SetLength(TexSet,1);
    TexSet[0].TextureID   := 0;
    TexSet[0].TextureTint := 0;
    TexSet[0].TextureEmit := 0;
//    SetLength(VertexColors,0);
    FModel.Redraw(FVertexColors,False,Nil,TexSet,'','',0,Nil);
    SetLength(TexSet,0);
    glPopMatrix;
    If bDepthTest Then glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);
  End;
End; // TSkySphere.Redraw

Procedure TSkySphere.SetRotate(RX,RY,RZ: Single);
Var
  I     : Integer;
  ToSun : T3DPoint{I3dPoint};
  Iw    : T3DPoint{I3dPoint};
  Rot   : T3DPoint{I3dPoint};

  ThetaS   : Single;
  PhiS     : Single;
  Theta2   : Single;
  Theta3   : Single;
  T        : Single;
  T2       : Single;
  Chi      : Single;
  ZenithY1 : Single;
  ZenithX  : Single;
  ZenithY  : Single;
  PerezY1  : Array[0..4] Of Single;
  PerezX   : Array[0..4] Of Single;
  PerezY   : Array[0..4] Of Single;
  SkyCol   : TColor;
  Theta    : Single;
  Phi      : Single;
  Gamma    : Single;
  X        : Single;
  Y        : Single;
  Y1       : Single;
  X1       : Single;
  Z1       : Single;
  SkyR     : Single;
  SkyG     : Single;
  SkyB     : Single;

  Procedure SetupSkyColor(ToSun,Iw: T3DPoint{I3dPoint}; Turb: Single = 3);
  Begin
    ToSun.Normalize;

    ThetaS     := ArcCos(ToSun.Z);
    PhiS       := ArcTan2(ToSun.Y, ToSun.X);

    Theta2     := ThetaS * ThetaS;
    Theta3     := Theta2 * ThetaS;
    T          := Turb;
    T2         := Turb * Turb;

    Chi        := (4 / 9 - T / 120) * (PI - 2 * ThetaS);
    ZenithY1   := (4.0453 * T - 4.9710) * Tan(Chi) - 0.2155 * T + 2.4192;
    ZenithY1   := Max(0,ZenithY1 * 1000);  // conversion from kcd/m^2 to cd/m^2

    // Compute chromaticity

    ZenithX    := ( 0.00165 * Theta3 - 0.00375 * Theta2 + 0.00209 * ThetaS + 0.0)     * T2 +
                  (-0.02903 * Theta3 + 0.06377 * Theta2 - 0.03202 * ThetaS + 0.00394) * T +
                  ( 0.11693 * Theta3 - 0.21196 * Theta2 + 0.06052 * ThetaS + 0.25886);

    ZenithY    := ( 0.00275 * Theta3 - 0.00610 * Theta2 + 0.00317 * ThetaS + 0.0)     * T2 +
                  (-0.04214 * Theta3 + 0.08970 * Theta2 - 0.04153 * ThetaS + 0.00516) * T +
                  ( 0.15346 * Theta3 - 0.26756 * Theta2 + 0.06670 * ThetaS + 0.26688);

    PerezY1[0] :=  0.17872 * T - 1.46303;
    PerezY1[1] := -0.35540 * T + 0.42749;
    PerezY1[2] := -0.02266 * T + 5.32505;
    PerezY1[3] :=  0.12064 * T - 2.57705;
    PerezY1[4] := -0.06696 * T + 0.37027;

    PerezX[0]  := -0.01925 * T - 0.25922;
    PerezX[1]  := -0.06651 * T + 0.00081;
    PerezX[2]  := -0.00041 * T + 0.21247;
    PerezX[3]  := -0.06409 * T - 0.89887;
    PerezX[4]  := -0.00325 * T + 0.04517;

    PerezY[0]  := -0.01669 * T - 0.26078;
    PerezY[1]  := -0.09495 * T + 0.00921;
    PerezY[2]  := -0.00792 * T + 0.21023;
    PerezY[3]  := -0.04405 * T - 1.65369;
    PerezY[4]  := -0.01092 * T + 0.05291;
  End; // SetupSkyColor

  Function SkyColor(ToSun,Iw: T3DPoint{I3dPoint}; Turb: Single = 3): TColor;

    Function PerezFunction(A,B,C,D,E,Theta,Gamma,ThetaS,LVZ: Single): Single;
    Var Den,Num: Single;
    Begin
      Den := (1 + A * Exp(B)) * (1 + C * Exp(D * ThetaS) + E * Cos(ThetaS) * Cos(ThetaS));
      Num := (1 + A * Exp(B / Cos(Theta))) * (1 + C * Exp(D * Gamma) + E * Cos(Gamma) * Cos(Gamma));
      Result := LVZ * Num / Den;
    End; // PerezFunction

    Function AngleBetween(ThetaV, PhiV, Theta, Phi: Single): Single;
    Var CosPsi: Single;
    Begin
      CosPsi := Sin(ThetaV) * Sin(Theta) * Cos(Phi - PhiV) + Cos(ThetaV) * Cos(Theta);
           If CosPsi > 1  Then Result := 0
      Else If CosPsi < -1 Then Result := Pi
      Else Result := ArcCos(CosPsi);
    End; // AngleBetween

  Begin


    Iw.Normalize;
//    vector Iw = normalize(transform("world", I));
    Theta      := ArcCos(Iw.Z);
    Phi        := ArcTan2(Iw.Y, Iw.X);

//    Gamma      := AngleBetween(Theta, Phi, ThetaS, PhiS);
    Gamma      := ArcCos(Iw.Dot(ToSun));

    // Compute xyY values

    X          := PerezFunction(PerezX[0],  PerezX[1],  PerezX[2],  PerezX[3],  PerezX[4],  Theta, Gamma, ThetaS, ZenithX);
    Y          := PerezFunction(PerezY[0],  PerezY[1],  PerezY[2],  PerezY[3],  PerezY[4],  Theta, Gamma, ThetaS, ZenithY);
    Y1         := PerezFunction(PerezY1[0], PerezY1[1], PerezY1[2], PerezY1[3], PerezY1[4], Theta, Gamma, ThetaS, ZenithY1);

    // Convert xyY to XYZ (Y1 doesn't change in the conversion)

    X1         := (X / Y) * Y1;
    Z1         := ((1 - X - Y) / Y) * Y1;

    // Convert XYZ to RGB

    SkyR       := Min(1,Max(0,1 - Exp(-(1/15000) * ( 3.240479 * X1 - 1.537150 * Y1 - 0.498535 * Z1))));
    SkyG       := Min(1,Max(0,1 - Exp(-(1/15000) * (-0.969256 * X1 + 1.875992 * Y1 + 0.041556 * Z1))));
    SkyB       := Min(1,Max(0,1 - Exp(-(1/15000) * ( 0.055648 * X1 - 0.204043 * Y1 + 1.057311 * Z1))));

    // Set the color

    SkyCol     := Round(SkyR * 255) + (Round(SkyG * 255) Shl 8) + (Round(SkyB * 255) Shl 16) + $FF000000;

    If Iw.Z < 0 Then SkyCol := $FF000000;

//    Oi = 1;
//    Ci = skycolor;
    Result := SkyCol;
  End; // SkyColor

Begin
  If (RX <> FXRotate0) Or (RY <> FYRotate0) Or (RZ <> FZRotate0) Then
  Begin
    Try
      FOwner.LockBSPTree('TSkySphere.SetRotate');

      If Not FClouds Then
      Begin
        Rot   := T3DPoint.Create{Point}(RX * Pi / 180,RY * Pi / 180,RZ * Pi / 180);
        ToSun := T3DPoint.Create{Point}(0,-1,0);
        ToSun.CounterClockwiseRotate(Rot,True,True,True,False);

    {
        ToSun.CounterClockwiseRotate(Rot,True,False,False,False);
        ToSun.CounterClockwiseRotate(Rot,False,False,True,False);
        ToSun.CounterClockwiseRotate(Rot,False,True,False,False);
    }
        Iw    := T3DPoint.Create{Point};
        SetLength(FVertexColors,High(Model.Vertices) + 1);
        SetupSkyColor(ToSun,Iw,3);
        For I := 0 To High(Model.Vertices) Do
        Begin
          Iw.Copy(Model.Positions.DataArray[I * 3 + 0],Model.Positions.DataArray[I * 3 + 1],Model.Positions.DataArray[I * 3 + 2]);

          // Eliminate the fade-to-black at the horizon by replicating the 1-degree color all the way down

          If Iw.Z <= 0 Then Iw.Z := Sin(1);

          FVertexColors[I] := SkyColor(ToSun,Iw,3);
        End; // For I
        Model.ColorType := mccEmission;
        ToSun.Free;
        Iw.Free;
        Rot.Free;
{
        ToSun := Nil;
        Iw    := Nil;
        Rot   := Nil;
}
      End
      Else
      Begin
        FXRotate := RX;
        FYRotate := RY;
        FZRotate := RZ;
      End;
      FXRotate0 := RX;
      FYRotate0 := RY;
      FZRotate0 := RZ;
    Finally
      FOwner.UnlockBSPTree;
    End;
  End;
End; // TSkySphere.SetRotate

Procedure TSkySphere.SetRotateWithMoon(RSX,RSY,RSZ,RMX,RMY,RMZ,MoonAlbedo: Single);
// Expanded to allow for the moon causing sky glow and optimized for speed
Type
  TLightSource = Record
    ThetaS   : Single;
    PhiS     : Single;
    Theta2   : Single;
    Theta3   : Single;
    Chi      : Single;
    ZenithY1 : Single;
    ZenithX  : Single;
    ZenithY  : Single;
    Gamma    : Single;
    X        : Single;
    Y        : Single;
    Y1       : Single;
    X1       : Single;
    Z1       : Single;
    SkyR     : Single;
    SkyG     : Single;
    SkyB     : Single;
  End;

Var
  I       : Integer;
  ToSun   : T3DPoint{I3dPoint};
  ToMoon  : T3DPoint{I3dPoint};
//  ToSunP  : P3DPointPointer;
//  ToMoonP : P3DPointPointer;
  Iw      : T3DPoint{I3dPoint};
  RotSun  : T3DPoint{I3dPoint};
  RotMoon : T3DPoint{I3dPoint};

  T        : Single;
  T2       : Single;
  PerezY1  : Array[0..5] Of Single;
  PerezX   : Array[0..5] Of Single;
  PerezY   : Array[0..5] Of Single;

  Sun      : TLightSource;
  Moon     : TLightSource;

  CTheta   : Single;
  C2ThetaS : Single;
  C2ThetaM : Single;

  DenS1    : Single;
  DenS2    : Single;
  DenS3    : Single;
  DenM1    : Single;
  DenM2    : Single;
  DenM3    : Single;

  Procedure SetupCommon(Turb: Single = 3);
  Begin
    T          := Turb;
    T2         := Turb * Turb;

    PerezY1[0] :=  0.17872 * T - 1.46303;
    PerezY1[1] := -0.35540 * T + 0.42749;
    PerezY1[2] := -0.02266 * T + 5.32505;
    PerezY1[3] :=  0.12064 * T - 2.57705;
    PerezY1[4] := -0.06696 * T + 0.37027;
    PerezY1[5] := PerezY1[0] * Exp(PerezY1[1]) + 1;

    PerezX[0]  := -0.01925 * T - 0.25922;
    PerezX[1]  := -0.06651 * T + 0.00081;
    PerezX[2]  := -0.00041 * T + 0.21247;
    PerezX[3]  := -0.06409 * T - 0.89887;
    PerezX[4]  := -0.00325 * T + 0.04517;
    PerezX[5]  := PerezX[0] * Exp(PerezX[1]) + 1;

    PerezY[0]  := -0.01669 * T - 0.26078;
    PerezY[1]  := -0.09495 * T + 0.00921;
    PerezY[2]  := -0.00792 * T + 0.21023;
    PerezY[3]  := -0.04405 * T - 1.65369;
    PerezY[4]  := -0.01092 * T + 0.05291;
    PerezY[5]  := PerezY[0] * Exp(PerezY[1]) + 1;
  End; // SetupCommon

  Procedure SetupSkyColor(Var LightSource: TLightSource; ToLightSource: T3DPoint{I3dPoint}; Albedo: Single; Turb: Single = 3);
  Begin
    ToLightSource.Normalize;

    LightSource.ThetaS     := ArcCos(ToLightSource.Z);
    LightSource.PhiS       := ArcTan2(ToLightSource.Y, ToLightSource.X);

    LightSource.Theta2     := LightSource.ThetaS * LightSource.ThetaS;
    LightSource.Theta3     := LightSource.Theta2 * LightSource.ThetaS;

    LightSource.Chi        := (4 / 9 - T / 120) * (PI - 2 * LightSource.ThetaS);
    LightSource.ZenithY1   := (4.0453 * T - 4.9710) * Tan(LightSource.Chi) - 0.2155 * T + 2.4192;
    LightSource.ZenithY1   := Max(0,LightSource.ZenithY1 * 1000);  // conversion from kcd/m^2 to cd/m^2
    LightSource.ZenithY1   := LightSource.ZenithY1 * Albedo;       // The moon is dimmer

    // Compute chromaticity

    LightSource.ZenithX    := ( 0.00165 * LightSource.Theta3 - 0.00375 * LightSource.Theta2 + 0.00209 * LightSource.ThetaS + 0.0)     * T2 +
                              (-0.02903 * LightSource.Theta3 + 0.06377 * LightSource.Theta2 - 0.03202 * LightSource.ThetaS + 0.00394) * T +
                              ( 0.11693 * LightSource.Theta3 - 0.21196 * LightSource.Theta2 + 0.06052 * LightSource.ThetaS + 0.25886);

    LightSource.ZenithY    := ( 0.00275 * LightSource.Theta3 - 0.00610 * LightSource.Theta2 + 0.00317 * LightSource.ThetaS + 0.0)     * T2 +
                              (-0.04214 * LightSource.Theta3 + 0.08970 * LightSource.Theta2 - 0.04153 * LightSource.ThetaS + 0.00516) * T +
                              ( 0.15346 * LightSource.Theta3 - 0.26756 * LightSource.Theta2 + 0.06670 * LightSource.ThetaS + 0.26688);
  End; // SetupSkyColor

  Function SkyColor({ToSun,ToMoon: P3DPointPointer; }Iw: T3DPoint{I3dPoint}; Turb: Single = 3): TColor;
  Var
    SkyCol   : TColor;
//    CTheta   : Single;
    C2Gamma  : Single;
//    C2ThetaS : Single;
//    C2ThetaM : Single;

    Function PerezFunction(A,B,C,D,E,Theta,Gamma,ThetaS,LVZ: Single): Single;
    Var Den,Num: Single;
    Begin
      Den := (1 + A * Exp(B)) * (1 + C * Exp(D * ThetaS) + E * Cos(ThetaS) * Cos(ThetaS));
      Num := (1 + A * Exp(B / Cos(Theta))) * (1 + C * Exp(D * Gamma) + E * Cos(Gamma) * Cos(Gamma));
      If Den <> 0
       Then Result := LVZ * Num / Den
       Else Result := 0;
    End; // PerezFunction

    Function PerezFunctionMod(Den,A,B,C,D,E,{F,}CTheta,Gamma,C2Gamma,ThetaS,C2ThetaS,LVZ: Single): Single;
    // Same logic as PerezFunction but optimized
    Var {Den,}Num: Single;
    Begin
//      Den := F * (1 + C * Exp(D * ThetaS) + E * C2ThetaS);
      Num := (1 + A * Exp(B / CTheta)) * (1 + C * Exp(D * Gamma) + E * C2Gamma);
      If Den <> 0
       Then Result := LVZ * Num / Den
       Else Result := 0;
    End; // PerezFunctionMod

  Begin

    // Don't bother with vertices that aren't visible

    If CTheta > -0.1 Then
    Begin
      // Process sunlight

      C2Gamma    := Iw.Dot(ToSun);
      Sun.Gamma  := ArcCos(C2Gamma);
      C2Gamma    := Sqr(C2Gamma);

      // Compute xyY values

      Sun.X      := PerezFunctionMod(DenS1, PerezX[0],  PerezX[1],  PerezX[2],  PerezX[3],  PerezX[4],  {PerezX[5], } CTheta, Sun.Gamma, C2Gamma, Sun.ThetaS, C2ThetaS, Sun.ZenithX);
      Sun.Y      := PerezFunctionMod(DenS2, PerezY[0],  PerezY[1],  PerezY[2],  PerezY[3],  PerezY[4],  {PerezY[5], } CTheta, Sun.Gamma, C2Gamma, Sun.ThetaS, C2ThetaS, Sun.ZenithY);
      Sun.Y1     := PerezFunctionMod(DenS3, PerezY1[0], PerezY1[1], PerezY1[2], PerezY1[3], PerezY1[4], {PerezY1[5],} CTheta, Sun.Gamma, C2Gamma, Sun.ThetaS, C2ThetaS, Sun.ZenithY1);

      // Convert xyY to XYZ (Y1 doesn't change in the conversion)

      Sun.X1     := (Sun.X / Sun.Y) * Sun.Y1;
      Sun.Z1     := ((1 - Sun.X - Sun.Y) / Sun.Y) * Sun.Y1;

      // Convert XYZ to RGB

      Sun.SkyR   := Min(1,Max(0,1 - Exp(-(1/15000) * ( 3.240479 * Sun.X1 - 1.537150 * Sun.Y1 - 0.498535 * Sun.Z1))));
      Sun.SkyG   := Min(1,Max(0,1 - Exp(-(1/15000) * (-0.969256 * Sun.X1 + 1.875992 * Sun.Y1 + 0.041556 * Sun.Z1))));
      Sun.SkyB   := Min(1,Max(0,1 - Exp(-(1/15000) * ( 0.055648 * Sun.X1 - 0.204043 * Sun.Y1 + 1.057311 * Sun.Z1))));

      // Process moonlight

      C2Gamma    := Iw.Dot(ToMoon);
      Moon.Gamma := ArcCos(C2Gamma);
      C2Gamma    := Sqr(C2Gamma);

      // Compute xyY values

      Moon.X     := PerezFunctionMod(DenM1, PerezX[0],  PerezX[1],  PerezX[2],  PerezX[3],  PerezX[4],  {PerezX[5], } CTheta, Moon.Gamma, C2Gamma, Moon.ThetaS, C2ThetaM, Moon.ZenithX);
      Moon.Y     := PerezFunctionMod(DenM2, PerezY[0],  PerezY[1],  PerezY[2],  PerezY[3],  PerezY[4],  {PerezY[5], } CTheta, Moon.Gamma, C2Gamma, Moon.ThetaS, C2ThetaM, Moon.ZenithY);
      Moon.Y1    := PerezFunctionMod(DenM3, PerezY1[0], PerezY1[1], PerezY1[2], PerezY1[3], PerezY1[4], {PerezY1[5],} CTheta, Moon.Gamma, C2Gamma, Moon.ThetaS, C2ThetaM, Moon.ZenithY1);

      // Convert xyY to XYZ (Y1 doesn't change in the conversion)

      Moon.X1    := (Moon.X / Moon.Y) * Moon.Y1;
      Moon.Z1    := ((1 - Moon.X - Moon.Y) / Moon.Y) * Moon.Y1;

      // Convert XYZ to RGB

      Moon.SkyR  := Min(1,Max(0,1 - Exp(-(1/15000) * ( 3.240479 * Moon.X1 - 1.537150 * Moon.Y1 - 0.498535 * Moon.Z1))));
      Moon.SkyG  := Min(1,Max(0,1 - Exp(-(1/15000) * (-0.969256 * Moon.X1 + 1.875992 * Moon.Y1 + 0.041556 * Moon.Z1))));
      Moon.SkyB  := Min(1,Max(0,1 - Exp(-(1/15000) * ( 0.055648 * Moon.X1 - 0.204043 * Moon.Y1 + 1.057311 * Moon.Z1))));

      // Combine sun and moon light

      Sun.SkyR   := Min(1,Sun.SkyR + Moon.SkyR);
      Sun.SkyG   := Min(1,Sun.SkyG + Moon.SkyG);
      Sun.SkyB   := Min(1,Sun.SkyB + Moon.SkyB);

      // Set the color

      SkyCol     := Round(Sun.SkyR * 255) + (Round(Sun.SkyG * 255) Shl 8) + (Round(Sun.SkyB * 255) Shl 16) + $FF000000;
    End
    Else SkyCol := $FF000000;
    Result := SkyCol;
  End; // SkyColor

Begin
  If (RSX <> FXRotate0) Or (RSY <> FYRotate0) Or (RSZ <> FZRotate0) Or (RMX <> FXRotate1) Or (RMY <> FYRotate1) Or (RMZ <> FZRotate1) Then
  Begin
    Try
      FOwner.LockBSPTree('TSkySphere.SetRotateWithMoon');

      If Not FClouds Then
      Begin
        RotSun  := T3DPoint.Create{Point}(RSX * Pi / 180,RSY * Pi / 180,RSZ * Pi / 180);
        RotMoon := T3DPoint.Create{Point}(RMX * Pi / 180,RMY * Pi / 180,RMZ * Pi / 180);
        ToSun   := T3DPoint.Create{Point}(0,-1,0);
        ToMoon  := T3DPoint.Create{Point}(0,-1,0);
        ToSun.CounterClockwiseRotate(RotSun,True,True,True,False);
        ToMoon.CounterClockwiseRotate(RotMoon,True,True,True,False);
        ToSun.Normalize;
        ToMoon.Normalize;
        Iw      := T3DPoint.Create{Point};
        SetLength(FVertexColors,High(Model.Vertices) + 1);
        SetupCommon;
        SetupSkyColor(Sun,ToSun,1,3);
        SetupSkyColor(Moon,ToMoon,MoonAlbedo,3);

        C2ThetaS := Sqr(ToSun.Z);
        C2ThetaM := Sqr(ToMoon.Z);

//        ToSunP  := ToSun.PointPointer;
//        ToMoonP := ToMoon.PointPointer;

        DenS1 := PerezX[5]  * (1 + PerezX[2]  * Exp(PerezX[3]  * Sun.ThetaS)  + PerezX[4]  * C2ThetaS);
        DenS2 := PerezY[5]  * (1 + PerezY[2]  * Exp(PerezY[3]  * Sun.ThetaS)  + PerezY[4]  * C2ThetaS);
        DenS3 := PerezY1[5] * (1 + PerezY1[2] * Exp(PerezY1[3] * Sun.ThetaS)  + PerezY1[4] * C2ThetaS);
        DenM1 := PerezX[5]  * (1 + PerezX[2]  * Exp(PerezX[3]  * Moon.ThetaS) + PerezX[4]  * C2ThetaS);
        DenM2 := PerezY[5]  * (1 + PerezY[2]  * Exp(PerezY[3]  * Moon.ThetaS) + PerezY[4]  * C2ThetaS);
        DenM3 := PerezY1[5] * (1 + PerezY1[2] * Exp(PerezY1[3] * Moon.ThetaS) + PerezY1[4] * C2ThetaS);

        For I := 0 To High(Model.Vertices) Do
        Begin
          CTheta := Model.Positions.DataArray[I * 3 + 2];

          // Eliminate the fade-to-black at the horizon by replicating the 1-degree color all the way down

          If CTheta <= 0 Then CTheta := Sin(1);

          Iw.Copy(Model.Positions.DataArray[I * 3 + 0],Model.Positions.DataArray[I * 3 + 1],CTheta);
          CTheta := Iw.Normalize(nrZ);

          FVertexColors[I] := SkyColor({ToSunP,ToMoonP,}Iw,3);
        End; // For I
        Model.ColorType := mccEmission;
        ToSun.Free;
        ToMoon.Free;
        Iw.Free;
        RotSun.Free;
        RotMoon.Free;
{
        ToSun   := Nil;
        ToMoon  := Nil;
        Iw      := Nil;
        RotSun  := Nil;
        RotMoon := Nil;
}        
      End
      Else
      Begin
        FXRotate := RSX;
        FYRotate := RSY;
        FZRotate := RSZ;
      End;
      FXRotate0 := RSX;
      FYRotate0 := RSY;
      FZRotate0 := RSZ;
      FXRotate1 := RMX;
      FYRotate1 := RMY;
      FZRotate1 := RMZ;
    Finally
      FOwner.UnlockBSPTree;
    End;
  End;
End; // TSkySphere.SetRotateWithMoon

Procedure TSkySphere.SetVisible(B: Boolean);
Begin
  If FVisible <> B Then
  Begin
    Try
      FOwner.LockBSPTree('TSkySphere.SetVisible');
      FUpdating := True;
//      While FOwner.Redrawing Do Sleep(1);
      FVisible := B;
    Finally
      FUpdating := False;
      FOwner.UnlockBSPTree;
    End;
  End;
End; // TSkySphere.SetVisible

Procedure TSkySphere.SetUseAlpha(B: Boolean);
Begin
  Try
    FOwner.LockBSPTree('TSkySphere.SetUseAlpha');
    FUpdating := True;
//    While FOwner.Redrawing Do Sleep(1);
    FUseAlpha := B;
  Finally
    FUpdating := False;
    FOwner.UnlockBSPTree;
  End;
End; // TSkySphere.SetUseAlpha

Procedure TSkySphere.SetSize(S: Single);
Begin
  Try
    FOwner.LockBSPTree('TSkySphere.SetSize');
    FUpdating := True;
//    While FOwner.Redrawing Do Sleep(1);
    If FSize <> S Then
    Begin
      FSize := S;
      FModel.Rescale(FSize,FSize,FSize);
    End;
  Finally
    FUpdating := False;
    FOwner.UnlockBSPTree;
  End;
End; // TSkySphere.SetSize

Procedure TSkySphere.SetTexture(Texture: TTexture; Clouds: Boolean);
Begin
  Try
    FOwner.LockBSPTree('TSkySphere.SetTexture');
    FUpdating := True;
//    While FOwner.Redrawing Do Sleep(1);

    FClouds   := Clouds;
    If Clouds
     Then FModel.GenerateCloudDeck
     Else FModel.GenerateSkySphere;

    FModel.FlipFaces;
    FModel.CheckOcclusion := False;
    If Texture <> Nil Then FModel.MapVertices(Texture.BMPInfo.Width,Texture.BMPInfo.Height,Clouds);

    If Not Clouds Then
    Begin
      FModel.Rescale(FSize * (FModel.Box.MaxPt.X - FModel.Box.MinPt.X) * 3,
                     FSize * (FModel.Box.MaxPt.Y - FModel.Box.MinPt.Y) * 3,
                     FSize * (FModel.Box.MaxPt.Z - FModel.Box.MinPt.Z) * 3);
    End
    Else
    Begin
      FModel.Rescale(FSize * (FModel.Box.MaxPt.X - FModel.Box.MinPt.X * 4),
                     FSize * (FModel.Box.MaxPt.Y - FModel.Box.MinPt.Y * 4),
                     FSize * (FModel.Box.MaxPt.Z - FModel.Box.MinPt.Z) * 3);
    End;

    FModel.SetColor(255,255,255,255);
    FModel.SetTexture(Texture);
  Finally
    FUpdating := False;
    FOwner.UnlockBSPTree;
  End;
End; // TSkySphere.SetTexture

// ---------------------------
// TSatellite
// ---------------------------

Constructor TSatellite.Create(AOwner: TSceneGL);
Begin
  FOwner     := AOwner;
  FVisible   := False;
  FSize      := 3; // Good default sun/moon size
  SetLength(FTextures,0);
  FColor     := $FFFFFFFF;
  FXRotate   := 0;
  FYRotate   := 0;
  FZRotate   := 0;
  FAlpha     := 255;
  FSatType   := stMoonOrPlanet;
  FPhase     := 0;
End; // TSatellite.Create

Destructor TSatellite.Destroy;
Begin
  SetLength(FTextures,0);
End; // TSatellite.Destroy

Procedure TSatellite.Redraw;
Const RScale = 1;//100;
Var
  bDepthTest : Boolean;
  bBlend     : Boolean;
  I          : Integer;

Begin
  If FVisible And (High(FTextures) >= 0) Then
  Begin
    glGetBooleanv(GL_DEPTH_TEST,@bDepthTest);
    glGetBooleanv(GL_BLEND,@bBlend);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glEnable(GL_TEXTURE_2D);
    Tex2D := True;
    glPushMatrix;

    // It's safe to call this directly because we're already in the render thread

    For I := 0 To High(FTextures) Do
     If Not FTextures[I].Loaded Then FTextures[I].LoadTextureIntoOpenGL;

    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_ALWAYS, 0);
{    If FAlpha <> 255
     Then }glBlendFunc(GL_SRC_ALPHA,GL_ONE);{
     Else glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);}
    glEnable(GL_BLEND);

    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

    glEnable(GL_POLYGON_SMOOTH);
    glHint(GL_POLYGON_SMOOTH_HINT,GL_NICEST);

    If FSatType = stMoonOrPlanet Then
    Begin
      glBindTexture(GL_TEXTURE_2D, FTextures[Round(FPhase * High(FTextures) / (2 * Pi))].ID);
      FTextures[Round(FPhase * High(FTextures) / (2 * Pi))].SetShaderParameter;
      glTranslatef(FOwner.DefaultCamera.Position.X,FOwner.DefaultCamera.Position.Y,FOwner.DefaultCamera.Position.Z);

      glRotatef(FYRotate,0,1,0);
      glRotatef(FZRotate,0,0,1);
      glRotatef(FXRotate,1,0,0);

      glBegin(GL_POLYGON);
      TBGRA(FColor).A := FAlpha; // This might cause the moon to not block stars, but it's only during twilight (the stars will fade away)
      glColor4ubv(@FColor);

      glNormal3f(0,1,0);
      glTexCoord2f(0,0);
      glVertex3f(-FSize * RScale,-99.9 * RScale,-FSize * RScale);

      glNormal3f(0,1,0);
      glTexCoord2f(1,0);
      glVertex3f(-FSize * RScale,-99.9 * RScale, FSize * RScale);

      glNormal3f(0,1,0);
      glTexCoord2f(1,1);
      glVertex3f( FSize * RScale,-99.9 * RScale, FSize * RScale);

      glNormal3f(0,1,0);
      glTexCoord2f(0,1);
      glVertex3f( FSize * RScale,-99.9 * RScale,-FSize * RScale);
      glEnd;
    End
    Else
    Begin
      // If it's a sun then draw all textures, centered on each other

      glTranslatef(FOwner.DefaultCamera.Position.X,FOwner.DefaultCamera.Position.Y,FOwner.DefaultCamera.Position.Z);

      glRotatef(FYRotate,0,1,0);
      glRotatef(FZRotate,0,0,1);
      glRotatef(FXRotate,1,0,0);
      For I := 0 To High(FTextures) Do
      Begin
        glBindTexture(GL_TEXTURE_2D, FTextures[I].ID);
        FTextures[I].SetShaderParameter;

        glBegin(GL_POLYGON);
        TBGRA(FColor).A := FAlpha; // This might cause the moon to not block stars, but it's only during twilight (the stars will fade away)
        glColor4ubv(@FColor);

        glNormal3f(0,1,0);
        glTexCoord2f(0,0);
        glVertex3f(-FSize * RScale,-100 * RScale,-FSize * RScale);

        glNormal3f(0,1,0);
        glTexCoord2f(1,0);
        glVertex3f(-FSize * RScale,-100 * RScale, FSize * RScale);

        glNormal3f(0,1,0);
        glTexCoord2f(1,1);
        glVertex3f( FSize * RScale,-100 * RScale, FSize * RScale);

        glNormal3f(0,1,0);
        glTexCoord2f(0,1);
        glVertex3f( FSize * RScale,-100 * RScale,-FSize * RScale);
        glEnd;
      End; // For I
    End;

    glHint(GL_POLYGON_SMOOTH_HINT,GL_FASTEST);
    glDisable(GL_POLYGON_SMOOTH);

    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

    glPopMatrix;
    If bDepthTest Then glEnable(GL_DEPTH_TEST);
    If Not bBlend Then glDisable(GL_BLEND);
    glEnable(GL_LIGHTING);
  End;
End; // TSatellite.Redraw

Procedure TSatellite.SetPhase(R32: TRaster32; S: Single);
Type LPtr = ^LongWord;
Var
  R   : TRaster32;
  X,Y : Integer;
  I   : Integer;
  P   : LPtr;
  P1  : LPtr;
  W2  : Integer;
  H2  : Integer;
  A   : Single;
  C   : TColor;

Begin
  Try
    FOwner.LockBSPTree('TSatellite.SetPhase');
    If High(FTextures) >= 0 Then
    Begin
      While S < 0 Do S := S + 2 * Pi;
      While S >= 2 * Pi Do S := S - 2 * Pi;
      FPhase := S;

      // If there is only one texture then we need to make the phase ourselves

      If High(FTextures) = 0 Then
      Begin
        R := TRaster32.Create(R32);

        // Blot out part of the moon: the earth is casting a shadow

        P  := R.Data;
        W2 := R.Width  Div 2;
        H2 := R.Height Div 2;
        If FAlpha = 255
         Then C := $FF000000    // Night
         Else C := 0;           // This might cause the moon to not block stars, but it's only during twilight (the stars will fade away)
        For Y := 0 To R.Height - 1 Do
        Begin
          If Sin(S) >= 0 Then
          Begin
            A  := ArcCos((H2 - Y) / H2);
            I  := W2 + Round(W2 * Cos(S) * Sin(A));
            P1 := P;
            Inc(LongWord(P1),I * 4);
            For X := I To R.Width - 1 Do
            Begin
              If P1^ <> 0 Then P1^ := C;
              Inc(LongWord(P1),4);
            End; // For X
          End
          Else
          Begin
            A  := ArcCos((H2 - Y) / H2);
            I  := W2 - Round(W2 * Cos(S) * Sin(A));
            P1 := P;
            For X := 0 To I - 1 Do
            Begin
              If P1^ <> 0 Then P1^ := C;
              Inc(LongWord(P1),4);
            End; // For X
          End;
          Inc(LongWord(P),R.Width * 4);
        End; // For Y

        FTextures[0].LoadTextureFromRaster32(R);
        FTextures[0].Loaded := False;
        R.Free;
      End;
    End;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TSatellite.SetPhase

Procedure TSatellite.SetVisible(B: Boolean);
Begin
  Try
    FOwner.LockBSPTree('TSatellite.SetVisible');
    FVisible := B;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TSatellite.SetVisible

Procedure TSatellite.SetSatType(AType: TSatelliteType);
Begin
  Try
    FOwner.LockBSPTree('TSatellite.SetSatType');
    FSatType := AType;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TSatellite.SetSatType

Procedure TSatellite.SetSize(S: Single);
Begin
  Try
    FOwner.LockBSPTree('TSatellite.SetSize');
    FSize := S;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TSatellite.SetSize

Procedure TSatellite.AddTexture(Texture: TTexture);
Begin
  Try
    FOwner.LockBSPTree('TSatellite.AddTexture');
    SetLength(FTextures,High(FTextures) + 2);
    FTextures[High(FTextures)] := Texture;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TSatellite.SetTexture

Procedure TSatellite.SetColor(C: TColor);
Begin
  Try
    FOwner.LockBSPTree('TSatellite.SetColor');
    FColor := C;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TSatellite.SetColor

Procedure TSatellite.SetRotate(X,Y,Z: Single);
Begin
  Try
    FOwner.LockBSPTree('TSatellite.SetRotate');
    FXRotate := X;
    FYRotate := Y;
    FZRotate := Z;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TSatellite.SetRotate

Procedure TSatellite.SetAlpha(A: Byte);
Begin
  Try
    FOwner.LockBSPTree('TSatellite.SetAlpha');
    FAlpha := A;
  Finally
    FOwner.UnlockBSPTree;
  End;    
End; // TSatellite.SetAlpha

// ---------------------------
// TPrecipitation
// ---------------------------

Constructor TPrecipitation.Create(AOwner: TSceneGL);
Begin
  FVisible   := False;
  FOwner     := AOwner;
  FColor     := $80FFFFFF;
  FCount     := 0;
  FItems     := Nil;
  FVelocity  := 0;
  FArea      := 100;
  FLastTime  := GetTickCount;
End; // TPrecipitation.Create

Destructor TPrecipitation.Destroy;
Begin
  If FItems <> Nil Then FreeMem(FItems);
End; // TPrecipitation.Destroy

Procedure TPrecipitation.SetVisible(B: Boolean);
Begin
  Try
    FOwner.LockBSPTree('TPrecipitation.SetVisible');
    FVisible := B;
    If B Then FLastTime := GetTickCount;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TPrecipitation.SetVisible

Procedure TPrecipitation.SetLines(B: Boolean);
Begin
  Try
    FOwner.LockBSPTree('TPrecipitation.SetLines');
    FLines := B;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TPrecipitation.SetLines

Procedure TPrecipitation.SetColor(C: TColor);
Var
  I : Integer;
  P : PPrecipitationItem;

Begin
  Try
    FOwner.LockBSPTree('TPrecipitation.SetColor');
    FColor := C;
    P      := FItems;
    For I := 0 To FCount - 1 Do
    Begin
      P.Color := C;
      Inc(LongWord(P),SizeOf(TPrecipitationItem));
    End; // For I
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TPrecipitation.SetColor

Procedure TPrecipitation.SetVelocity(V: Single);
Var
  I : Integer;
  P : PPrecipitationItem;

Begin
  Try
    FOwner.LockBSPTree('TPrecipitation.SetVelocity');
    FVelocity := V;
    P         := FItems;
    For I := 0 To FCount - 1 Do
    Begin
      P.Velocity := V;
      Inc(LongWord(P),SizeOf(TPrecipitationItem));
    End; // For I
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TPrecipitation.SetVelocity

Procedure TPrecipitation.SetArea(A: Single);
Begin
  Try
    FOwner.LockBSPTree('TPrecipitation.SetArea');
    FArea := A;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TPrecipitation.SetArea

Procedure TPrecipitation.SetCount(ACount: Integer);
Var
  I : Integer;
  P : PPrecipitationItem;

Begin
  If ACount <> FCount Then
  Begin
    Try
      FOwner.LockBSPTree('TPrecipitation.SetCount');
      If FItems <> Nil Then FreeMem(FItems);
      FItems := Nil;
      FCount := ACount;
      If FCount > 0 Then
      Begin
        GetMem(FItems,FCount * SizeOf(TPrecipitationItem));
        P := FItems;
        For I := 0 To FCount - 1 Do
        Begin
          P.Color    := FColor;
          P.X        := 0;
          P.Y        := 0;
          P.Z        := 0;
          P.Velocity := FVelocity;
          P.MinZ     := 0;
          P.Counter  := Trunc(Random * 10);
          Inc(LongWord(P),SizeOf(TPrecipitationItem));
        End; // For I
      End;
    Finally
      FOwner.UnlockBSPTree;
    End;
  End;
End; // TPrecipitation.SetCount

Procedure TPrecipitation.InitItems;
Var
  I    : Integer;
  P    : PPrecipitationItem;
  MinZ : Single;

Begin
  If Owner.ActiveCamera <> Nil Then
  Begin
    P := FItems;
    For I := 0 To FCount - 1 Do
    Begin
      P.X    := Owner.ActiveCamera.Position.X + Random * 2 * FArea - FArea;
      P.Y    := Owner.ActiveCamera.Position.Y + Random * 2 * FArea - FArea;
      P.Z    := Owner.ActiveCamera.Position.Z + Random * 2 * FArea - FArea;
      P.MinZ := -999999;
      MinZ   := 999999;
      If Owner.BSPTreeRoot <> Nil Then Owner.BSPTreeRoot.FindMinMaxZPoints(P.X,P.Y,MinZ,P.MinZ);
      P.Counter  := Trunc(Random * 10);
      Inc(LongWord(P),SizeOf(TPrecipitationItem));
    End; // For I
  End;
End; // TPrecipitation.InitItems

Procedure TPrecipitation.Redraw;
Var
  I          : Integer;
  Time       : Integer;
  P          : PPrecipitationItem;
  bTexture2D : Boolean;
  MinZ       : Single;

Begin
  If FVisible Then
  Begin
    // Advance elements

    Time := GetTickCount;
    P    := FItems;
    For I := 0 To FCount - 1 Do
    Begin
      P.Z := P.Z + P.Velocity * (Time - FLastTime) / 1000;
      If Owner.ActiveCamera <> Nil Then
      Begin
        If (P.Z < P.MinZ) Or (P.Z < Owner.ActiveCamera.Position.Z - FArea) Then
        Begin
          Begin
            P.Z := Owner.ActiveCamera.Position.Z + Random * 2 * FArea - FArea;

            // It's very expensive to scan the zone to determine where the precipitation needs
            // to stop, so we're not going to move it's X,Y position every time it hits bottom

            Inc(P.Counter);
            If P.Counter >= 10 Then
            Begin
              P.X    := Owner.ActiveCamera.Position.X + Random * 2 * FArea - FArea;
              P.Y    := Owner.ActiveCamera.Position.Y + Random * 2 * FArea - FArea;
              P.MinZ := -999999;
              MinZ   := 999999;
              If Owner.BSPTreeRoot <> Nil Then Owner.BSPTreeRoot.FindMinMaxZPoints(P.X,P.Y,MinZ,P.MinZ);
              P.Counter := 0;
            End;
          End;
        End;
      End;
      Inc(LongWord(P),SizeOf(TPrecipitationItem));
    End; // For I
    FLastTime := Time;

    // Render

    glGetBooleanv(GL_TEXTURE_2D,@bTexture2D);
    glDisable(GL_TEXTURE_2D);
    P := FItems;

    glEnable(GL_POINT_SMOOTH);
    glEnable(GL_LINE_SMOOTH);
    glPointSize(1.75);
    glLineWidth(1.25);
    glHint(GL_POINT_SMOOTH_HINT,GL_NICEST);
    glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);

    glPushMatrix;
      If FLines
       Then glBegin(GL_LINES)
       Else glBegin(GL_POINTS);

        For I := 0 To FCount - 1 Do
        Begin
          If P.Z >= P.MinZ Then
          Begin
            glColor4ubv(@P.Color);
            glVertex3fv(@P.X);

            If FLines Then
            Begin
              glColor4ub(TBGRA(P.Color).R,TBGRA(P.Color).G,TBGRA(P.Color).B,0);
              glVertex3f(P.X,P.Y,P.Z - P.Velocity / 20);
            End;
          End;
          Inc(LongWord(P),SizeOf(TPrecipitationItem));
        End; // For I
      glEnd;
    glPopMatrix;

    glLineWidth(1);
    glPointSize(1);
    glHint(GL_POINT_SMOOTH_HINT,GL_FASTEST);
    glHint(GL_LINE_SMOOTH_HINT,GL_FASTEST);
    glDisable(GL_LINE_SMOOTH);
    glDisable(GL_POINT_SMOOTH);

    If bTexture2D Then glEnable(GL_TEXTURE_2D);
  End;
End; // TPrecipitation.Redraw

// ---------------------------
// TStarField
// ---------------------------

Constructor TStarField.Create(AOwner: TSceneGL);
Begin
  FOwner    := AOwner;
  FVisible  := False;
  FStars    := TStringList.Create;
  FXRotate  := 0;
  FYRotate  := 0;
  FZRotate  := 0;
  FAlpha    := 255;
End; // TStarField.Create

Destructor TStarField.Destroy;
Begin
  Clear;
  FStars.Free;
End; // TStarField.Destroy

Procedure TStarField.Clear;
Var I: Integer;
Begin
  For I := 0 To FStars.Count - 1 Do FStars.Objects[I].Free;
  FStars.Clear;
End; // TStarField.Clear

Procedure TStarField.AddStars(Stars: TStringList);
Var
  I    : Integer;
  Star : TStar;

Begin
  Try
    FOwner.LockBSPTree('StarField.AddStars');
    For I := 0 To Stars.Count - 1 Do FStars.AddObject('',Stars.Objects[I]);
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TStarField.AddStars

Procedure TStarField.Redraw;
Var
  bDepthTest : Boolean;
  bBlend     : Boolean;
  bLighting  : Boolean;
  bTexture2D : Boolean;
  I          : Integer;
  Star       : TStar;
  Color      : TColor;

Begin
  If FVisible Then
  Begin
    glGetBooleanv(GL_DEPTH_TEST,@bDepthTest);
    glGetBooleanv(GL_BLEND,@bBlend);
    glGetBooleanv(GL_LIGHTING,@bLighting);
    glGetBooleanv(GL_TEXTURE_2D,@bTexture2D);

    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);
//    Tex2D := False;
{
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_ALWAYS, 0);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
}
    glPushMatrix;
      glTranslatef(FOwner.DefaultCamera.Position.X,FOwner.DefaultCamera.Position.Y,FOwner.DefaultCamera.Position.Z);


      glRotatef(FYRotate,0,1,0);
      glRotatef(FZRotate,0,0,1);
      glRotatef(FXRotate,1,0,0);


  {
      // Normal

      glRotatef(FZRotate,0,0,1);
      glRotatef(FYRotate,0,1,0);
      glRotatef(FXRotate,1,0,0);
  }

      // It would be nice to make the stars twinkle, but turning on antialiasing makes them twinkle way too much

  //    glEnable(GL_POINT_SMOOTH);
  //    glPointSize(1.25);
  //    glHint(GL_POINT_SMOOTH_HINT,GL_NICEST);
  //    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

        glBegin(GL_POINTS);
          For I := 0 To FStars.Count - 1 Do
          Begin
            Star           := TStar(FStars.Objects[I]);
            Color          := Star.Color;
            TBGRA(Color).A := FAlpha;
            glColor4ubv(@Color);
            glVertex3fv(@Star.X);
          End; // For I
        glEnd;
      glColor4ub(255,255,255,255);
  //    glPointSize(1);
  //    glHint(GL_POINT_SMOOTH_HINT,GL_FASTEST);
  //    glDisable(GL_POINT_SMOOTH);
    glPopMatrix;
    
    If bDepthTest Then glEnable(GL_DEPTH_TEST);
//    If Not bBlend Then glDisable(GL_BLEND);
    If bLighting  Then glEnable(GL_LIGHTING);
    If bTexture2D Then glEnable(GL_TEXTURE_2D);
  End;
End; // TStarField.Redraw

Procedure TStarField.SetVisible(B: Boolean);
Begin
  If FVisible <> B Then
  Begin
    Try
      FOwner.LockBSPTree('TStarField.SetVisible');
      FVisible := B;
    Finally
      FOwner.UnlockBSPTree;
    End;
  End;
End; // TStarField.SetVisible

Procedure TStarField.SetRotate(X,Y,Z: Single);
Begin
  Try
    FOwner.LockBSPTree('TStarField.SetRotate');
    FXRotate := X;
    FYRotate := Y;
    FZRotate := Z;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TStarField.SetRotate

Procedure TStarField.SetAlpha(A: Byte);
Begin
  Try
    FOwner.LockBSPTree('TStarField.SetAlpha');
    FAlpha := A;
  Finally
    FOwner.UnlockBSPTree;
  End;
End; // TStarField.SetAlpha

Procedure TestModel(Bottom,Inside: Boolean);
Var
  Model : TModel;
  F     : System.Text;
  I,J,K : Integer;
  St    : String;

Begin
  Model := TModel.Create(Nil);
  Model.GenerateOctahedron;
  Model.CalcRoundedNormals;
  AssignFile(F,'library\scripts\test.scp');
  ReWrite(F);
  WriteLn(F,'Category Polyhedra');
  WriteLn(F);
  WriteLn(F,'Param Tex1        String');
  If Bottom Then WriteLn(F,'Param Bottom      Boolean Default True');
  If Inside Then WriteLn(F,'Param Inside      Boolean Default False');
  WriteLn(F,'Param SemiTrans   Boolean Default False');
  WriteLn(F,'Param Transparent Boolean Default False');
  WriteLn(F,'Param Solid       Boolean Default True');
  WriteLn(F,'Param Color       Integer Default 0');
  WriteLn(F,'Param HasColor    Boolean Default False');
  WriteLn(F,'Param Masked      Boolean Default False');
  WriteLn(F,'Param CalcNormals Boolean Default True');
  WriteLn(F);
  J := 0;
  K := 0;
  For I := 0 To High(Model.Faces) Do
  Begin


    St := Format('Triangle %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, Tex1, Transparent, SemiTrans, Solid, Color, HasColor, Masked, CalcNormals, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f',
                 [Model.Positions.DataArray[J + 6] / 2,Model.Positions.DataArray[J + 7] / 2,Model.Positions.DataArray[J + 8] / 2,
                  Model.Positions.DataArray[J + 3] / 2,Model.Positions.DataArray[J + 4] / 2,Model.Positions.DataArray[J + 5] / 2,
                  Model.Positions.DataArray[J + 0] / 2,Model.Positions.DataArray[J + 1] / 2,Model.Positions.DataArray[J + 2] / 2,
                  TNormal(Model.VNormals[K + 2]).X / 127,TNormal(Model.VNormals[K + 2]).Y / 127,TNormal(Model.VNormals[K + 2]).Z / 127,
                  TNormal(Model.VNormals[K + 1]).X / 127,TNormal(Model.VNormals[K + 1]).Y / 127,TNormal(Model.VNormals[K + 1]).Z / 127,
                  TNormal(Model.VNormals[K + 0]).X / 127,TNormal(Model.VNormals[K + 0]).Y / 127,TNormal(Model.VNormals[K + 0]).Z / 127]);

    WriteLn(F,St);
    Inc(J,9);
    Inc(K,3);
  End; // For I
  If Inside Then
  Begin
    WriteLn(F);
    WriteLn(F,'If Inside');
    J := 0;
    K := 0;
    For I := 0 To High(Model.Faces) Do
    Begin


      St := Format('  Triangle %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, Tex1, Transparent, SemiTrans, Solid, Color, HasColor, Masked, CalcNormals, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f, %8.3f',
                   [Model.Positions.DataArray[J + 6] / 2,Model.Positions.DataArray[J + 7] / 2,Model.Positions.DataArray[J + 8] / 2,
                    Model.Positions.DataArray[J + 3] / 2,Model.Positions.DataArray[J + 4] / 2,Model.Positions.DataArray[J + 5] / 2,
                    Model.Positions.DataArray[J + 0] / 2,Model.Positions.DataArray[J + 1] / 2,Model.Positions.DataArray[J + 2] / 2,
                    -TNormal(Model.VNormals[K + 2]).X / 127,-TNormal(Model.VNormals[K + 2]).Y / 127,-TNormal(Model.VNormals[K + 2]).Z / 127,
                    -TNormal(Model.VNormals[K + 1]).X / 127,-TNormal(Model.VNormals[K + 1]).Y / 127,-TNormal(Model.VNormals[K + 1]).Z / 127,
                    -TNormal(Model.VNormals[K + 0]).X / 127,-TNormal(Model.VNormals[K + 0]).Y / 127,-TNormal(Model.VNormals[K + 0]).Z / 127]);

      WriteLn(F,St);
      Inc(J,9);
      Inc(K,3);
    End; // For I
    WriteLn(F,'EndIf');
  End;
  CloseFile(F);
End; // TestModel

Initialization
  MaxLights := -1;

  SceneFullScreen                 := False;
  SceneFullScreenWidth            := 1024;
  SceneFullScreenHeight           := 768;
  SceneFullScreenPixelDepth       := 32;
  SceneFullScreenHasKeyboardFocus := False;
  LastMouseButton                 := mbLeft;
  OnFullScreenKeyDown             := Nil;
  OnFullScreenKeyUp               := Nil;
  OnFullScreenMouseDown           := Nil;
  OnFullScreenMouseUp             := Nil;
  OnFullScreenMouseMove           := Nil;

  UseVBOExtension                 := False;
  VSyncOn                         := True;

  Try
    InitOpenGL;
    ReadExtensions;
    ReadImplementationProperties;
  Finally
  End;
  WindowHash := TIntegerPointerHash.Create(True);
  SceneHash  := TIntegerPointerHash.Create(True);
  If FileExists(ExtractFilePath(Application.ExeName) + 'glenginelog.txt') Then
   DeleteFile(ExtractFilePath(Application.ExeName) + 'glenginelog.txt');
  LogMutex    := TCriticalSection.Create;
//  RedrawMutex := TCriticalSection.Create;
//  ModelFaceSorter := TModelFaceSorter.Create;
Finalization
//  ModelFaceSorter.Free;
  LogMutex.Free;
//  RedrawMutex.Free;
  WindowHash.Free;
  SceneHash.Free;


//  TestModel(False,False);
End.

