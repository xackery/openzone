{HACER QUE TODO TPoint APUNTE SIEMPRE A SUS VERTICES Y QUE DEJE DE HACERLO SI
 UN VERTICE SE DESPRENDE}
 {ELIMINAR CAMPO REFERENCES DE TPoint}

{3D object handling.  this unit is related to uRickGL

  TEntity:  an entity or object
      TFace: a face of an entity, each entity has many faces
         TVertex: a vertex of a face, each face has many vertices.

   By: Ricardo Sarmiento
       delphiman@hotmail.com
 }

Unit U3Dpolys;

Interface

Uses Windows, Classes, SysUtils, {GL,} GLU, Dialogs, Graphics, SyncObjs, Points3D, OpenGL15;

Const
  OccludeSkipFrames = 5;

{***************     special record definition    *******************}
Type
  TTextureSetRec = Record
    TextureID   : Integer;     // Texture set ID
    TextureTint : LongWord;    // Ambient/diffuse tint
    TextureEmit : LongWord;    // Emissive tint
  End;
  TTextureSetList = Array Of TTextureSetRec;

  GlyphMetricsFloat = Record  {this one is used in the 3D text entity}
    gmfblackBoxX     : Single;
    gmfblackboxY     : Single;
    gmfptglyphorigin : Pointer;
    gmfcellincX      : Single;
    gmfcellincY      : Single;
  End;

Type
  PTyBuff  = ^TyBuff;
  PTyBuffA = ^TyBuffA;
  TyBuff   = Array[1..1280000] Of Packed Record
                                   R: Byte;
                                   G: Byte;
                                   B: Byte;
                                 End;
  TyBuffA  = Packed Array[1..1280000] Of Packed Record
                                   R: Byte;
                                   G: Byte;
                                   B: Byte;
                                   A: Byte;
                                 End;

{MWMWMWMWMWMWMWMWMWMW    Classes definition  MWMWMWMWMWMWMWMWMWMWMWMW}
Type
  TTexture = Class;

  TRaster32 = Class
  Protected
    Function    GetScanLine(Y: Integer): Pointer;
  Public
    Width  : Integer;
    Height : Integer;
    Data   : Pointer; // Packed RGBA data, *always* (width * height * 4) bytes
    Constructor Create(AWidth,AHeight: Integer); Overload;
    Constructor Create(BMP: TBitmap); Overload; // The bitmap's PixelFormat must be pf32bit!
    Constructor Create(R32: TRaster32); Overload; // The bitmap's PixelFormat must be pf32bit!
//    Constructor Create(BMP: TJCLBitmap32); Overload; // The bitmap's PixelFormat must be pf32bit!
    Constructor Create(FileName: String); Overload;
    Destructor  Destroy; Override;
    Procedure   CopyFrom(BMP: TBitmap); Overload;
    Procedure   CopyFrom(R32: TRaster32); Overload;
//    Procedure   CopyFrom(BMP: TJCLBitmap32); Overload;
//    Procedure   CopyFrom(BMP: TJCLBitmap32; Color,Alpha: LongWord); Overload;
//    Procedure   CopyFrom(BMP: TJCLBitmap32; Alpha: LongWord); Overload;
    Procedure   FlipAlpha;
    Procedure   SetAlpha(Color: LongWord; Alpha: Byte);
    Procedure   SetAllAlphaTo255;
    Procedure   ExchangeRedAndBlue;
    Procedure   BlitFrom(R: TRaster32; SourceX,SourceY,SourceWidth,SourceHeight,DestX,DestY: Integer);
    Procedure   LoadFromFile(FileName: String);
    Procedure   SaveToFile(FileName: String);
    Procedure   BlitTo(BMP: TBitmap; PremultiplyAlpha: Boolean);
    Property    ScanLine[Y: Integer] : Pointer Read  GetScanLine;
  End;

  TLoadTexProcedure    = Function(FileName: String): TBitmap;
  TLoadTexProcedureR32 = Function(FileName: String): TRaster32;

  TBSPTreeNode = Class;

  // Entity or object or 3D mesh

  TEntity = Class
  Protected
    FOldTime              : LongInt;
    FRenderable           : TObject;               // Really a TRenderable
    FPosition             : T3DPoint;
    FLocalPosition        : T3DPoint;
    FRotation             : T3DPoint;
    FScale                : T3DPoint;
    FLocalScale           : T3DPoint;
    FLastPosition         : T3DPoint;
    FLastRotation         : T3DPoint;
    FLastScale            : T3DPoint;
    FDeltaPosition        : T3DPoint;
    FDeltaRotation        : T3DPoint;
    FDeltaScale           : T3DPoint;
    FDeltaPosWork         : T3DPoint;
    FDeltaRotWork         : T3DPoint;
    FDeltaSclWork         : T3DPoint;
    FRotationWork         : T3DPoint;
    FBox                  : TAxisAlignedBox;
    FSphere               : TSphere;
    FSphereWork           : TSphere;
    FSphereWork2          : TSphere;
    FBSPTreeNode          : TBSPTreeNode;
    FAnimFrame            : Single;                // Ranges from 0 to 1
    FLastAnimFrame        : Single;
    FSourceWork           : T3DPoint;              // Used by IsIntersectedBy()
    FDestWork             : T3DPoint;              // Used by IsIntersectedBy()
    FEllipseWork          : TEllipsoid;
    FEllipsoid            : TEllipsoid;
    FHasAlpha             : Boolean;
    FOneShotRenderable    : TObject;
    FStopOnLastFrame      : Boolean;
    FRevertToPositive     : Boolean;
    FZCenter              : Single;
    FTransformMatrix      : T4x4Matrix;
    FMinZ0                : Single;
    FOnRedraw             : TNotifyEvent;
    FOrientation          : TQuaternion;
    FSetTransformMatrix   : Boolean;
    Function    GetRenderable: TObject;           // Really gets a TRenderable
    Procedure   SetAnimFrame(NewFrame: Single);
  Public
    BoundR,BoundG,BoundB  : Byte;                  // bound lines color
    AnimSpeed             : Single;                // Frames per second
    OneShotAnimSpeed      : Single;
    ID                    : GLuInt;                // entity's identIfier, optional
    WireFrame             : Byte;                  // 0: use points, 1:use wireframes, 2: use solid poligons
    Visible               : Boolean;               // indicates if the object can be seen or not
    Owner                 : TObject;               // Really a TSceneGL
    Redrawing             : Boolean;
    CanChangeTreeNodes    : Boolean;
    VertexColors          : Array Of LongWord;
    CollisionAvoidance    : Boolean;
    Name                  : String;
    OccludeCounter        : Integer;
    ModelType             : Integer;
    TextureSet            : TTextureSetList;
    Tag                   : Integer;
    NearLight             : TColor;
    VisibleInFrustum      : Boolean;
    OldVisibleInFrustum   : Boolean;
    NewlyVisibleInFrustum : Boolean;
    Gravity               : Single;
    AltCollisionModel     : TObject;              // TModel used for collision checking...only used when it differs from what is displayed 
    Constructor Create(AOwner: TObject);          // create a zero-polygon entity (owner is a TSceneGL)
    Destructor  Destroy; Override;
    Procedure   SetBoundColor(iR,iG,iB: Byte);    // change entity's bounds color
    Procedure   Redraw(AlphaNotOneOnly: Boolean); Virtual;                  // construct the entity using OpenGL commands
    Procedure   GetDiapasones(Out X1,X2,Y1,Y2,Z1,Z2: Double); // find the diapasones of x,y,z values
    Procedure   GetExtents;
    Procedure   SetRenderable(ARenderable: TObject);
    Procedure   Update;
    Function    IsIntersectedBy(Source,Dest: T3DPoint): Boolean; // WARNING: This modifies Dest if the result is true
    Procedure   FindIntersection(Ellipsoid: TEllipsoid; Velocity: T3DPoint; MovementSphere: TSphere; Var NearestDistance,Friction: Single; NewCenter,NewVelocity: T3DPoint; UseFriction: Boolean);
    Procedure   Init;
    Procedure   SetOneShotRenderable(R: TObject; StopOnLastFrame,RevertToPositive: Boolean);
    Procedure   RebuildTransformMatrix;
    Property    Renderable        : TObject         Read GetRenderable Write SetRenderable;
    Property    OneShotRenderable : TObject         Read FOneShotRenderable;
    Property    Position          : T3DPoint        Read FPosition;
    Property    LocalPosition     : T3DPoint        Read FLocalPosition;
    Property    Rotation          : T3DPoint        Read FRotation;  // Values are in degrees
    Property    Scale             : T3DPoint        Read FScale;
    Property    LocalScale        : T3DPoint        Read FLocalScale;
    Property    DeltaPosition     : T3DPoint        Read FDeltaPosition;
    Property    DeltaRotation     : T3DPoint        Read FDeltaRotation;  // Values are in degrees
    Property    DeltaScale        : T3DPoint        Read FDeltaScale;
    Property    Box               : TAxisAlignedBox Read FBox;
    Property    Sphere            : TSphere         Read FSphere;
    Property    BSPTreeNode       : TBSPTreeNode    Read FBSPTreeNode;
    Property    HasAlpha          : Boolean         Read FHasAlpha;
    Property    AnimFrame         : Single          Read FAnimFrame Write SetAnimFrame;
    Property    OnRedraw          : TNotifyEvent    Read FOnRedraw  Write FOnRedraw;
    Property    Orientation       : TQuaternion     Read FOrientation;
  End;

  TBSPTreeNode = Class
  Protected
    FEntities       : TList;
    FParent         : TBSPTreeNode;
    FChild1         : TBSPTreeNode;        // Child1 is in front of the plane
    FChild2         : TBSPTreeNode;        // Child2 is behind the plane
    FPlane          : TPlane;
    FOwner          : TObject;             // Really a TSceneGL
    FTag            : Integer;
    FUserTag        : Integer;
    FLeafTag        : Integer;
    FMasterPVS      : Packed Array Of TBSPTreeNode;
    FMasterPVS2     : Packed Array Of TBSPTreeNode;
    FVisited        : Boolean;             // Used by portal renderer to prevent infinite recursion
    FPortals        : TStringList;         // Doesn't have to be a TThreadSafeList if we're careful about where we touch it
    FNumLeaves      : Integer;
    FID             : Integer;             // Different from Tag: used to identify partitions (non-leaf nodes)
    FBoundingBox    : TAxisAlignedBox;
    FBoundingSphere : TSphere;
    Procedure   SetChild1(Child: TBSPTreeNode);
    Procedure   SetChild2(Child: TBSPTreeNode);
  Public
    PVS        : Array Of Integer;
    Constructor Create(AOwner: TObject);
    Destructor  Destroy; Override;
    Procedure   AddEntity(Entity: TEntity);
    Procedure   RemoveEntity(Entity: TEntity);
    Procedure   Redraw(AlphaNotOneOnly: Boolean; CameraNode: TBSPTreeNode);
    Procedure   RedrawUp(AlphaNotOneOnly: Boolean; CameraNode: TBSPTreeNode);
    Function    ContainsEntity(Entity: TEntity): Boolean;
    Procedure   CheckEntity(Entity: TEntity);
    Function    FindClosestEntity(Source,Dest: T3DPoint): TEntity;
    Procedure   MoveToIntersection(Ellipsoid: TEllipsoid; Velocity: T3DPoint; MovementSphere: TSphere; Var NearestDistance,Friction: Single; NewCenter,NewVelocity: T3DPoint; UseFriction: Boolean; ExcludeEntity: TEntity);
    Function    GetNodeContainingPoint(P: T3DPoint): TBSPTreeNode;
    Function    PVSContainsTag(I: Integer): Boolean;
    Procedure   BuildMasterNodeList;
    Function    GetNodeWithTag(I: Integer): TBSPTreeNode;
    Function    GetNodeWithID(I: Integer): TBSPTreeNode;
    Function    GetNumPartitions: Integer;
    Procedure   AssignIDs(Var BaseID: Integer);
    Procedure   BuildBoundingSpheres;
    Property    Plane          : TPlane          Read FPlane;
    Property    Child1         : TBSPTreeNode    Read FChild1  Write SetChild1; // Should only be used by the TSceneGL to be thread-safe
    Property    Child2         : TBSPTreeNode    Read FChild2  Write SetChild2; // Should only be used by the TSceneGL to be thread-safe
    Property    Tag            : Integer         Read FTag     Write FTag;
    Property    UserTag        : Integer         Read FUserTag Write FUserTag;
    Property    LeafTag        : Integer         Read FLeafTag Write FLeafTag;
    Property    Parent         : TBSPTreeNode    Read FParent;
    Property    NumLeaves      : Integer         Read FNumLeaves;
    Property    BoundingBox    : TAxisAlignedBox Read FBoundingBox;
    Property    BoundingSphere : TSphere         Read FBoundingSphere;
  End;

  TPortal = Class(TBasicPolygon)
    ID           : Integer;
    PartitionID  : Integer;
    FromSectorID : Integer;
    ToSectorID   : Integer;
    IsMirror     : Boolean;
    Constructor Create;
    Function    Clone: TPortal;
  End;

  TSectorManager = Class
  Protected
    BSPRoot    : TBSPTreeNode;
    Sectors    : Array Of TBSPTreeNode;
    Procedure   CreateSectors;
    Function    CreatePortalList: TStringList;             // List of TPortal
    Procedure   AddPortalsToSectors(Portals: TStringList); // List of TPortal
    Procedure   AddPortal(Portal: TPortal; BSPNode: TBSPTreeNode);
    Procedure   FindTruePortals;
    Function    CreateLargePortal(BspNode: TBSPTreeNode): TPortal;
    Function    ClassifyPortal(Portal: TPortal; Plane: TPlane): TAlphaClassification;
    Function    CheckForSinglePortal(SectorIndex,PortalIndex: Integer): Boolean;
    Procedure   CheckForSinglePortals;
    Procedure   ClipPortalToSector(Portal: TPortal; Side: TAlphaClassification);
    Procedure   MakePortalsInward;
    Procedure   RemoveExtraPortals;
  Public
    Constructor Create;
    Procedure   Setup(Root: TBSPTreeNode);
  End;
{
  TUIPanel = Class;

  TUserInterface = Class(TEntity)
  Protected
    FPanels : TList;
    FZOrder : Array Of Integer;
    FZSort  : Array Of Integer;
    Function    GetPanel(Index: Integer): TUIPanel;
    Function    GetNumPanels: Integer;
  Public
    Constructor Create(AOwner: TObject);
    Destructor  Destroy; Override;
    Procedure   Redraw(AlphaNotOneOnly: Boolean); Override;
    Function    AddPanel: Integer;
    Procedure   RemovePanel(Index: Integer);
    Procedure   RemoveAllPanels;
    Procedure   MoveToFront(Index: Integer);
    Property    Panels[Index: Integer] : TUIPanel Read GetPanel;
    Property    NumPanels              : Integer  Read GetNumPanels;
  End;

  TUIPanel = Class
  Protected
    FOwner    : TEntity;
    R,G,B,A   : Byte;
    FBuffer   : TRaster32;
    FUpdating : Boolean;
    FAlpha    : Single; // 0=transparent, 1=opaque
    FVisible  : Boolean;
    FLeft     : Integer;
    FTop      : Integer;
    FWidth    : Integer;
    FHeight   : Integer;
    Procedure SetAlpha(S: Single);
    Procedure SetVisible(B: Boolean);
  Public
    Constructor Create(Owner: TEntity);
    Destructor  Destroy; Override;
    Procedure   Redraw;
    Procedure   Update(BMP: TJCLBitmap32); //(BMP: TBitmap);
    Procedure   SetColor(iR,iG,iB,iA: Byte);
    Procedure   SetRect(ALeft,ATop,AWidth,AHeight: Integer);
    Property    Alpha   : Single  Read FAlpha   Write SetAlpha;
    Property    Visible : Boolean Read FVisible Write SetVisible;
    Property    Left    : Integer Read FLeft;
    Property    Top     : Integer Read FTop;
    Property    Width   : Integer Read FWidth;
    Property    Height  : Integer Read FHeight;
  End;
}
  // A special kind of entity

  T3DText = Class(TEntity)
    R,G,B,A       : Byte;
    LocalRotation : Array[1..3] Of Single;    {Orientation about own center, Rx,Ry,Rz}
    FontScale     : Array[1..3] Of Single;    {Scale of font in x,y,z}
    FontStyle     : TLogFontA;                {the font settings}
    ExtrudeDepth  : Single;                   {the Z-axis depth when creating the 3D font}
    ListStart     : Integer;                  {indicates where to have the display lists, two fonts cannot use the same space}
                                              {each font uses 256 spaces}

    // Call ResetFont after changing one or more of the above properties to apply the changes

    Text          : Array[0..255] Of Char;    {the message to be written}
    TextLength    : Integer;
    MyDC          : HDC;                      {the Device Context of the TsceneGL}
    Constructor Create(AOwner: TObject; DC: HDC; iListStart: Integer); Overload;  
    Constructor Create(AOwner: TObject; OtherText: T3DText);           Overload;
    Destructor  Destroy; Override;
    Procedure   ResetFont;                     // recreate the 3D font, useful after changing the properties
    Procedure   Redraw(AlphaNotOneOnly: Boolean); Override;              // redraw the 3D text
    Procedure   SetText(iText: String);        // change the text property
    Procedure   LocalRotate(RX,RY,RZ: Single); // turn entity to new orientation
    Procedure   SetColor(iR,iG,iB,iA: Byte); Virtual;
    {the following methods are not available for 3Dtext.
         Procedure   Load
         Procedure   Save
         Procedure   LoadDXF
         Procedure   CalcNormals;   (is not necessary)
         Procedure   Center;        (could be done, but I don?t have the patience)
         Function    AddFace:TFace;
         Function    FindPoint      (find a point in the display lists ?, I need a volunteer, or a hero)
    }
  End; // T3DText

  // A texture

  TTexBMPHeader = Packed Record
    FileType  : Word;     {always MB}
    Size      : LongInt;
    Reserved1 : Word;     {reserved for future purposes}
    Reserved2 : word;     {reserved for future purposes}
    Offset    : LongInt;  {offset to image in bytes}
  End; {header}
  TTexBMPInfo = Packed Record
    Size         : LongInt;    {size of BMPinfo in bytes}
    Width        : LongInt;   {width of the image in pixels}
    Height       : LongInt;       {height of the image in pixels}
    Planes       : Word;          {number of planes (always 1)}
    ColorBits    : Word;       {number of bits used to describe color in each pixel}
    Compression  : LongInt;  {compression used}
    ImageSize    : LongInt;    {image size in bytes}
    XPixPerMeter : LongInt; {pixels per meter in X}
    YPixPerMeter : LongInt; {pixels per meter in Y}
    ColorUsed    : LongInt;   {number of the color used ??????}
    Important    : LongInt;   {number of "important" colors}
  End; {info}

  TTexture = Class
    ID              : Integer;
    Automatic       : Boolean;              {use or not automatic texture coordinates generation}
    AutoXMult       : Array[0..3] Of GLInt; {multiply values for X coordinate}
    AutoZMult       : Array[0..3] Of GLInt; {multiply values for Z coordinate}
    AutoGenModeX    : GLInt;                {coordinate calculation algorithm to be used: }
    AutoGenModeZ    : GLInt;                {GL_object_linear, GL_Eye_linear or GL_Sphere_map}
    WrapSMode       : GLInt;
    WrapTMode       : GLInt;
    MagFilter       : GLInt;
    MinFilter       : GLInt;
    EnvironmentMode : GLInt;                {GL_decal, GL_modulate or GL_blend}
    EnvBlendColor   : Array[0..3] Of Byte;  {RGBA color if EnvironmentMode is blend}
    Owner           : TEntity;              {a texture can be owned by an Entity}
//    MyList          : GLSizeI;              {number of the display list for this texture}

    ImgSize         : LongInt;
    OImgSize        : LongInt;
    Buffer          : Pointer;
    OBuffer         : Pointer;
    Header          : TTexBMPHeader;
    BMPInfo         : TTexBMPInfo;
    OHeader         : TTexBMPHeader;
    OBMPInfo        : TTexBMPInfo;
    Buffer3         : Pointer;              { Pointer to 32-bit color data }
    OBuffer3        : Pointer;              { Pointer to 32-bit color data }
    Size            : Integer;
    OSize           : Integer;
    AvgColor        : TColor;
    Loaded          : Boolean;
    Scene           : TObject;              // Really a TSceneGL;
    ExchangeRB      : Boolean;
    NeedsMask       : Boolean;
    Index           : Integer;
    HasAlpha        : Boolean;
    Opacity         : String;

//    TexFileName     : String;

    Constructor Create(AScene: TObject; iOwner: TEntity);    {set defaults for all fields}
    Destructor  Destroy; Override;          {destroy the display list}
    Function    LoadTexture(TexMap,OpacMap: String; FlipR32: Boolean): Integer;  {load a new texture file, return 0 if ok, negative number if error}
    Function    LoadTextureFromBitmap(Bitmap: TBitmap): Integer;  {load a new texture file, return 0 if ok, negative number if error}
    Function    LoadTextureFromRaster32(R32: TRaster32): Integer;  {load a new texture file, return 0 if ok, negative number if error}
    Procedure   Redraw;                     {call the display list, it?s not really a redraw, it?s part of one}
    Procedure   LoadTextureIntoOpenGL;
    Procedure   FlipAlpha;
    Procedure   SetAlpha(Color: LongWord; Alpha: Byte);
  End; // TTexture

Const
  MinimalDistance = 0.0001;   {If two points are this far from each other, they can be considered to be in the same position}
  VeryCloseDistance = 0.01;//0.005 * 0.04;

Var
  //  These two variables are used in this unit and in unit UrickGL:
//  PutNames           : Boolean;  {If true put names to vertex and entity primitives when rendering}
  ActualVertexNumber  : LongInt;  {used to mark every vertex with a dIfferent name in every entity}
  Tex2D               : Boolean;
  LoadTexProc         : TLoadTexProcedure;
  LoadTexProcR32      : TLoadTexProcedureR32;
  DefaultExtrudeDepth : Single;
  MultAlpha           : Array[0..255,0..255] Of Byte;

Implementation
{MWMWMWMWMWMWMWMWMWMW  IMPLEMENTATION of the CLASSES  MWMWMWMWMWMWMWMWMWMWMWMW}

Uses URickGL,JPEG,Targa,Math,GLVisir,Exentia;

Type
  PRGBA = ^TRGBA;
  TRGBA = Packed Record
    R,G,B,A: Byte;
  End;

Function PowerOf2(I: Integer): Integer;
// Round I up to the nearest power of 2
Var J: Integer;
Begin
  J := 1;
  While J < I Do J := J * 2;
  Result := J;
End; // PowerOf2

// ---------------------------
// TRaster32
// ---------------------------

Constructor TRaster32.Create(AWidth,AHeight: Integer);
Begin
  Width  := AWidth;
  Height := AHeight;
  GetMem(Data,Width * Height * 4);
  FillChar(Data^,Width * Height * 4,0);
End; // TRaster32.Create

Constructor TRaster32.Create(BMP: TBitmap);
// The bitmap's PixelFormat must be pf32bit!  This class is so we don't consume a ton of
// GDI resources with lots of TBitmaps.
Begin
  Width  := BMP.Width;
  Height := BMP.Height;
  GetMem(Data,Width * Height * 4);
  CopyFrom(BMP);
End; // TRaster32.Create

Constructor TRaster32.Create(R32: TRaster32);
Begin
  Width  := R32.Width;
  Height := R32.Height;
  GetMem(Data,Width * Height * 4);
  CopyFrom(R32);
End; // TRaster32.Create
{
Constructor TRaster32.Create(BMP: TJCLBitmap32);
// The bitmap's PixelFormat must be pf32bit!  This class is so we don't consume a ton of
// GDI resources with lots of TBitmaps.
Begin
  Width  := BMP.Width;
  Height := BMP.Height;
  GetMem(Data,Width * Height * 4);
  CopyFrom(BMP);
End; // TRaster32.Create
}
Constructor TRaster32.Create(FileName: String);
Begin
  LoadFromFile(FileName);
End; // TRaster32.Create

Destructor TRaster32.Destroy;
Begin
  FreeMem(Data,Width * Height * 4);
End; // TRaster32.Destroy

Procedure TRaster32.CopyFrom(BMP: TBitmap);
Var
  I : Integer;
  P : Pointer;

Begin
  P := Data;
  For I := 0 To BMP.Height - 1 Do
  Begin
    Move(BMP.ScanLine[BMP.Height - 1 - I]^,P^,Width * 4);
    Inc(LongWord(P),Width * 4);
  End; // For I
End; // TRaster32.CopyFrom

Procedure TRaster32.CopyFrom(R32: TRaster32);
Begin
  Move(R32.Data^,Data^,Width * Height * 4);
End; // TRaster32.CopyFrom
{
Procedure TRaster32.CopyFrom(BMP: TJCLBitmap32);
Var
  I  : Integer;
  P  : Pointer;
  P1 : Pointer;
  W4 : Integer;

Begin
  P  := Data;
  P1 := BMP.ScanLine[BMP.Height - 1];
  W4 := Width * 4;
  For I := 0 To BMP.Height - 1 Do
  Begin
    Move(P1^,P^,W4);
    Inc(LongWord(P),W4);
    Dec(LongWord(P1),W4);
  End; // For I
End; // TRaster32.CopyFrom

Procedure TRaster32.CopyFrom(BMP: TJCLBitmap32; Color,Alpha: LongWord);
Var
  Dest   : Pointer;
  Src    : Pointer;
  W      : Integer;
  H      : Integer;
  W8     : Integer;
  Color1 : LongWord;
  BCMOV  : Boolean;

Begin
  Dest   := Data;
  W      := Width;
  H      := Height;
  Src    := BMP.ScanLine[H - 1];
  W8     := W * 8;
  Alpha  := Alpha Shl 24;
  Color1 := (Color And $00FFFFFF) Or Alpha;
  Color  := Color Or $FF000000;
  BCMOV  := TFVector.isCMOV;
  Asm
    PUSH  ESI
    PUSH  EDI
    PUSH  EBX
    MOV   EDX,H
    MOV   ESI,Src
    MOV   EDI,Dest
    MOV   EBX,W8
    SUB   EDI,4
    TEST  BYTE PTR BCMOV,0FFh
    JZ    @RowLoop

    // CMOVcc instructions are available

@RowLoopCMOV:
    MOV   ECX,W
@ColLoopCMOV:
    MOV   EAX,[ESI]
    ADD   ESI,4
    OR    EAX,$FF000000
    ADD   EDI,4
    CMP   EAX,Color
    CMOVE EAX,Color1
    MOV   [EDI],EAX
    DEC   ECX
    JNZ   @ColLoopCMOV
    SUB   ESI,EBX
    DEC   EDX
    JNZ   @RowLoopCMOV
    JMP   @Done

    // No CMOVcc instructions are available

@RowLoop:
    MOV   ECX,W
@ColLoop:
    MOV   EAX,[ESI]
    ADD   ESI,4
    OR    EAX,$FF000000
    ADD   EDI,4
    CMP   EAX,Color
    JNE   @NotSame
    MOV   EAX,Color1
@NotSame:
    MOV   [EDI],EAX
    DEC   ECX
    JNZ   @ColLoop
    SUB   ESI,EBX
    DEC   EDX
    JNZ   @RowLoop
@Done:    
    POP   EBX
    POP   EDI
    POP   ESI
  End; // Asm
End; // TRaster32.CopyFrom

Procedure TRaster32.CopyFrom(BMP: TJCLBitmap32; Alpha: LongWord);
Var
  Dest   : Pointer;
  Src    : Pointer;
  W      : Integer;
  H      : Integer;
  W8     : Integer;

Begin
  Dest   := Data;
  W      := Width;
  H      := Height;
  Src    := BMP.ScanLine[H - 1];
  W8     := W * 8;
  Alpha  := Alpha Shl 24;
  Asm
    PUSH  ESI
    PUSH  EDI
    PUSH  EBX
    MOV   EDX,H
    MOV   ESI,Src
    MOV   EDI,Dest
    MOV   EBX,W8
    SUB   EDI,4
@RowLoop:
    MOV   ECX,W
@ColLoop:
    MOV   EAX,[ESI]
    ADD   ESI,4
    AND   EAX,$00FFFFFF
    ADD   EDI,4
    OR    EAX,Alpha
    MOV   [EDI],EAX
    DEC   ECX
    JNZ   @ColLoop
    SUB   ESI,EBX
    DEC   EDX
    JNZ   @RowLoop
    POP   EBX
    POP   EDI
    POP   ESI
  End; // Asm
End; // TRaster32.CopyFrom
}
Procedure TRaster32.FlipAlpha;
Var
  I : Integer;
  P : ^LongWord;

Begin
  If Data <> Nil Then
  Begin
    P := Data;
    For I := 0 To Width * Height - 1 Do
    Begin
      TRGBA(P^).A := 255 - TRGBA(P^).A;
      Inc(LongWord(P),4);
    End; // For I
  End;
End; // TRaster32.FlipAlpha

Procedure TRaster32.ExchangeRedAndBlue;
Var
  I : Integer;
  P : ^LongWord;
  R : Byte;

Begin
  If Data <> Nil Then
  Begin
    P := Data;
    For I := 0 To Width * Height - 1 Do
    Begin
      R           := TRGBA(P^).R;
      TRGBA(P^).R := TRGBA(P^).B;
      TRGBA(P^).B := R;
      Inc(LongWord(P),4);
    End; // For I
  End;
End; // TRaster32.ExchangeRB

Procedure TRaster32.SetAlpha(Color: LongWord; Alpha: Byte);
Var
  I : Integer;
  P : ^LongWord;

Begin
  If Data <> Nil Then
  Begin
    P     := Data;
    Color := Color And $00FFFFFF;
    For I := 0 To Width * Height - 1 Do
    Begin
      If (P^ And $00FFFFFF) = Color Then TRGBA(P^).A := Alpha;
      Inc(LongWord(P),4);
    End; // For I
  End;
End; // TRaster32.SetAlpha

Procedure TRaster32.SetAllAlphaTo255;
Var
  I : Integer;
  P : ^LongWord;

Begin
  If Data <> Nil Then
  Begin
    P := Data;
    For I := 0 To Width * Height - 1 Do
    Begin
      P^ := P^ Or $FF000000;
      Inc(LongWord(P),4);
    End; // For I
  End;
End; // TRaster32.SetAllAlphaTo255

Procedure TRaster32.BlitFrom(R: TRaster32; SourceX,SourceY,SourceWidth,SourceHeight,DestX,DestY: Integer);
Var
  Y,W,H : Integer;
  PS,PD : ^LongWord;

Begin
  If (SourceX >= 0) And (SourceY >= 0) And (SourceX < R.Width) And (SourceY < R.Height) And
     (DestX   >= 0) And (DestY   >= 0) And (DestX   < Width)   And (DestY   < Height)   Then
  Begin
    W := SourceWidth;
    H := SourceHeight;
    If DestX   + W > Width    Then W := Width    - DestX;
    If DestY   + H > Height   Then H := Height   - DestY;
    If SourceX + W > R.Width  Then W := R.Width  - SourceX;
    If SourceY + H > R.Height Then H := R.Height - SourceY;
    If (W > 0) And (H > 0) Then
    Begin
      PS := Pointer(LongWord(R.Data) + (SourceY * R.Width + SourceX) * 4);
      PD := Pointer(LongWord(Data)   + (DestY   * Width   + DestX)   * 4);
      For Y := DestY To DestY + H - 1 Do
      Begin
        Move(PS^,PD^,W * 4);
        Inc(LongWord(PS),R.Width * 4);
        Inc(LongWord(PD),Width   * 4);
      End; // For Y
    End;
  End;
End; // TRaster32.BlitFrom

Procedure TRaster32.BlitTo(BMP: TBitmap; PremultiplyAlpha: Boolean);
// Assumes the BMP is 32-bit rgba with a width that is evenly divisible by 4.  Also assumes
// that the bitmap's pixels are in bottom-up order.
Var
  Src,Dest : Pointer;
  I,J      : Integer;
  P        : PRGBA;
  RGBA     : TRGBA;

Begin
  Src  := Data;
  Dest := BMP.ScanLine[0];
  If (BMP.Width >= Width) And (BMP.Height >= Height) Then
  Begin
    For I := 0 To Height - 1 Do
    Begin
      Move(Src^,Dest^,Width * 4);
      Inc(LongWord(Src),Width * 4);
      Dec(LongWord(Dest),BMP.Width * 4);
    End; // For I

    If PremultiplyAlpha Then
    Begin
      Dest := BMP.ScanLine[0];
      For I := 0 To Height - 1 Do
      Begin
        P := Dest;
        For J := 0 To Width - 1 Do
        Begin
          RGBA   := P^;
          RGBA.R := MultAlpha[RGBA.R,RGBA.A];
          RGBA.G := MultAlpha[RGBA.G,RGBA.A];
          RGBA.B := MultAlpha[RGBA.B,RGBA.A];
          P^     := RGBA;
          Inc(LongWord(P),4);
        End; // For J
        Dec(LongWord(Dest),BMP.Width * 4);
      End; // For I
    End;
  End;
End; // TRaster32.BlitTo

Procedure TRaster32.LoadFromFile(FileName: String);
Const MB = 19778;
Var
  F       : File;
  Header  : TTexBMPHeader;
  BMPInfo : TTexBMPInfo;
  I       : Integer;
  Dest    : Pointer;

Begin
  If FileExists(FileName) Then
  Begin
    AssignFile(F,FileName);
    Reset(F,1);
    BlockRead(F,Header,SizeOf(Header));
    BlockRead(F,BMPinfo,SizeOf(BMPInfo));

    // Make sure the texture is a Windows .BMP file

    If Header.FileType = MB Then
    Begin
      If Data <> Nil Then FreeMem(Data,Width * Height);
      Data := Nil;
      If BMPInfo.ImageSize = 0 Then
      Begin
        Case BMPInfo.ColorBits Of
           8: BMPInfo.ImageSize := 1 * BMPinfo.Width * BMPInfo.Height;
          16: BMPInfo.ImageSize := 2 * BMPinfo.Width * BMPInfo.Height;
          24: BMPInfo.ImageSize := 3 * BMPinfo.Width * BMPInfo.Height;
          32: BMPInfo.ImageSize := 4 * BMPinfo.Width * BMPInfo.Height;
        End; // Case
      End;

      // We only support uncompressed 32-bit RGBA BMP files

      If BMPInfo.ImageSize = 4 * BMPinfo.Width * BMPInfo.Height Then
      Begin
        GetMem(Data,BMPInfo.ImageSize);
        Seek(F,Header.Offset);
        Width  := BMPInfo.Width;
        Height := BMPInfo.Height;
        Dest   := Data;
        Inc(LongWord(Dest),Width * (Height - 1) * 4);
        For I := 0 To Height - 1 Do
        Begin
          BlockRead(F,Dest^,Width * 4);
          Dec(LongWord(Dest),Width * 4);
        End; // For I
      End;
    End;
    CloseFile(F);
    ExchangeRedAndBlue;
  End;
End; // TRaster32.LoadFromFile

Procedure TRaster32.SaveToFile(FileName: String);
Var
  F      : File;
  Header : TBitmapFileHeader;
  Info   : TBitmapInfoHeader;
  Y      : Integer;
  Size   : Integer;

Begin
  Size := Width * Height * 4;
  AssignFile(F,FileName);
  ReWrite(F,1);
  Header.bfType        := $4D42;
  Header.bfSize        := SizeOf(Header) + SizeOf(Info) + Size;
  Header.bfReserved1   := 0;
  Header.bfReserved2   := 0;
  Header.bfOffBits     := SizeOf(Header) + SizeOf(Info);
  Info.biSize          := SizeOf(Info);
  Info.biWidth         := Width;
  Info.biHeight        := Height;
  Info.biPlanes        := 1;
  Info.biBitCount      := 32;
  Info.biCompression   := 0;
  Info.biSizeImage     := 0;
  Info.biXPelsPerMeter := 3000;
  Info.biYPelsPerMeter := 3000;
  Info.biClrUsed       := 0;
  Info.biClrImportant  := 0;
  BlockWrite(F,Header,SizeOf(Header));
  BlockWrite(F,Info,SizeOf(Info));
  For Y := 0 To Height - 1 Do BlockWrite(F,ScanLine[Y]^,Width * 4);
  CloseFile(F);
End; // TRaster32.SaveToFile

Function TRaster32.GetScanLine(Y: Integer): Pointer;
Type LPtr = ^LongWord;
Var P: LPtr;
Begin
  If (Data <> Nil) And (Y >= 0) And (Y < Height) Then
  Begin
    P := Data;
    Inc(LongWord(P),((Height - 1) - Y) * Width * 4);
    Result := P;
  End
  Else Result := Nil;
End; // TRaster32.GetScanLine

// ---------------------------
// TBSPTreeNode
// ---------------------------

Constructor TBSPTreeNode.Create(AOwner: TObject);
Begin
  FOwner          := AOwner;
  FEntities       := TList.Create;
  FPlane          := TPlane.Create;
  FParent         := Nil;
  FChild1         := Nil;
  FChild2         := Nil;
  FUserTag        := -1;
  FTag            := -1;
  FLeafTag        := -1;
  FVisited        := False;
  FPortals        := TStringList.Create;
  FNumLeaves      := 0;
  FID             := -1;
  FBoundingBox    := TAxisAlignedBox.Create;
  FBoundingSphere := TSphere.Create;
  SetLength(FMasterPVS,0);
End; // TBSPTreeNode.Create

Destructor TBSPTreeNode.Destroy;
Var I: Integer;
Begin
  FEntities.Free;
  FPlane.Free;
  FChild1.Free;
  FChild2.Free;
  SetLength(FMasterPVS,0);
  For I := 0 To FPortals.Count - 1 Do FPortals.Objects[I].Free;
  FPortals.Free;
  FBoundingBox.Free;
  FBoundingSphere.Free;
End; // TBSPTreeNode.Destroy

Procedure TBSPTreeNode.AddEntity(Entity: TEntity);
Begin
  Try
    If FOwner <> Nil Then TSceneGL(FOwner).LockBSPTree;
    FEntities.Add(Entity);
    Entity.FBSPTreeNode := Self;
  Finally
    If FOwner <> Nil Then TSceneGL(FOwner).UnlockBSPTree;
  End;
End; // TBSPTreeNode.AddEntity

Procedure TBSPTreeNode.RemoveEntity(Entity: TEntity);
Var I: Integer;
Begin
  I := FEntities.IndexOf(Entity);
  If I >= 0 Then FEntities.Delete(I);
End; // TBSPTreeNode.RemoveEntity

Function TBSPTreeNode.GetNodeWithTag(I: Integer): TBSPTreeNode;
Var Node: TBSPTreeNode;
Begin
  If Tag = I Then Result := Self
  Else
  Begin
    Node := Nil;
    If FChild1 <> Nil Then Node := FChild1.GetNodeWithTag(I);
    If (Node = Nil) And (FChild2 <> Nil) Then Node := FChild2.GetNodeWithTag(I);
    Result := Node;
  End;
End; // TBSPTreeNode.GetNodeWithTag

Function TBSPTreeNode.GetNodeWithID(I: Integer): TBSPTreeNode;
Var Node: TBSPTreeNode;
Begin
  If FID = I Then Result := Self
  Else
  Begin
    Node := Nil;
    If FChild1 <> Nil Then Node := FChild1.GetNodeWithTag(I);
    If (Node = Nil) And (FChild2 <> Nil) Then Node := FChild2.GetNodeWithTag(I);
    Result := Node;
  End;
End; // TBSPTreeNode.GetNodeWithID

Function TBSPTreeNode.GetNumPartitions: Integer;
// Relies on FLeafTag to tell if this is a leaf node or not
Var I1,I2: Integer;
Begin
  If FLeafTag >= 0 Then Result := 0
  Else
  Begin
    I1 := 0;
    I2 := 0;
    If FChild1 <> Nil Then I1 := FChild1.GetNumPartitions;
    If FChild2 <> Nil Then I2 := FChild2.GetNumPartitions;
    Result := I1 + I2 + 1;
  End;
End; // TBSPTreeNode.GetNumPartitions

Procedure TBSPTreeNode.AssignIDs(Var BaseID: Integer);
// Relies on FLeafTag to tell if this is a leaf node or not
Begin
  If FLeafTag < 0 Then
  Begin
    FID := BaseID;
    Inc(BaseID);
    If FChild1 <> Nil Then FChild1.AssignIDs(BaseID);
    If FChild2 <> Nil Then FChild2.AssignIDs(BaseID);
  End;
End; // TBSPTreeNode.AssignIDs

Procedure TBSPTreeNode.BuildBoundingSpheres;
Var
  V      : T3DPoint;
  Length : Single;
  Ratio  : Single;

Begin
  If FBoundingSphere.Radius = 0 Then
  Begin
    If (FChild1 <> Nil) And (FChild1.BoundingSphere.Radius = 0) Then FChild1.BuildBoundingSpheres;
    If (FChild2 <> Nil) And (FChild2.BoundingSphere.Radius = 0) Then FChild2.BuildBoundingSpheres;
    If (FChild1 <> Nil) And (FChild2 <> Nil) Then
    Begin
      V      := T3DPoint.Create(FChild1.BoundingSphere.Center,FChild2.BoundingSphere.Center);
      Length := V.GetLength;
      BoundingSphere.Center.Copy(FChild1.BoundingSphere.Center);
      If Length <> 0 Then
      Begin
        Ratio := (1 + ((FChild2.BoundingSphere.Radius - FChild1.BoundingSphere.Radius) / Length)) / 2;
        V.Multiply(Ratio);
        BoundingSphere.Center.Add(V);
        BoundingSphere.Radius := Length * Ratio; 
      End
      Else BoundingSphere.Radius := Max(FChild1.BoundingSphere.Radius,FChild2.BoundingSphere.Radius);
      V.Free;
    End
    Else
    Begin
           If FChild1 <> Nil Then BoundingSphere.Copy(FChild1.BoundingSphere)
      Else If FChild2 <> Nil Then BoundingSphere.Copy(FChild2.BoundingSphere);
    End;
  End;
End; // TBSPTreeNode.BuildBoundingSpheres

Procedure TBSPTreeNode.BuildMasterNodeList;
// Only callable by the root node
Var
  MaxTag,OldMaxTag : Integer;

  Procedure GetHighestTag(Node: TBSPTreeNode; Var T: Integer);
  Begin
    If Node <> Nil Then
    Begin
      If Node.Tag >= 0 Then
      Begin
        If Node.Tag > T Then T := Node.Tag;
      End
      Else
      Begin
        GetHighestTag(Node.Child1,T);
        GetHighestTag(Node.Child2,T);
      End;
    End;
  End; // GetHighestTag

  Procedure AssignNewTag(Node: TBSPTreeNode; Var T: Integer);
  Begin
    // Tag is -1 by default whenever a node is created

    If (Node <> Nil) And (Node.Tag < 0) Then
    Begin
      Inc(T);
      Node.Tag := T;
      AssignNewTag(Node.Child1,T);
      AssignNewTag(Node.Child2,T);
    End;
  End; // AssignNewTag

  Procedure LoadNodeIntoMasterPVS(Node,Root: TBSPTreeNode);
  Begin
    If Node <> Nil Then
    Begin
      Root.FMasterPVS[Node.Tag] := Node;
      LoadNodeIntoMasterPVS(Node.Child1,Root);
      LoadNodeIntoMasterPVS(Node.Child2,Root);
    End;
  End; // LoadNodeIntoMasterPVS

  Procedure AddParentReferences(Node,Root: TBSPTreeNode; OldMaxTag: Integer);
  Var
    I,J,K : Integer;
    N     : TBSPTreeNode;
    Done  : Boolean;

  Begin
    If Node <> Nil Then
    Begin
      If Node.Tag <= OldMaxTag Then
      Begin
        // Only want to do this to leaf nodes

        FillChar(Root.FMasterPVS2[0], (High(Root.FMasterPVS2) + 1) * SizeOf(Pointer),0);
        J := 0;
        For I := 0 To High(Node.PVS) Do
        Begin
          K := Node.PVS[I];
          If Root.FMasterPVS2[K] = Nil Then
          Begin
            N := Root.FMasterPVS[K];
            Root.FMasterPVS2[K] := N;
            Inc(J);
            N    := N.Parent;
            Done := False;
            While (N <> Nil) And Not Done Do
            Begin
              K := N.Tag;
              If Root.FMasterPVS2[K] = Nil Then
              Begin
                Root.FMasterPVS2[K] := N;
                Inc(J);
                N := N.Parent;
              End
              Else Done := True;
            End; // While
          End;
        End; // For I
        SetLength(Node.PVS,J);
        J := 0;
        For I := 0 To High(Root.FMasterPVS2) Do
        Begin
          If Root.FMasterPVS2[I] <> Nil Then
          Begin
            Node.PVS[J] := I;
            Inc(J);
          End;
        End; // For I
      End;
      AddParentReferences(Node.Child1,Root,OldMaxTag);
      AddParentReferences(Node.Child2,Root,OldMaxTag);
    End;
  End; // AddParentReferences

Begin
  If FParent = Nil Then
  Begin
    // First find the highest tag

    MaxTag := -1;
    GetHighestTag(Self,MaxTag);
    FNumLeaves := MaxTag + 1;
    OldMaxTag := MaxTag;

    // Assign new tags to the intermediate nodes

    AssignNewTag(Self,MaxTag);

    // Allocate the master node list

    SetLength(FMasterPVS, MaxTag + 1);
    SetLength(FMasterPVS2,MaxTag + 1);

    // Fill in the master list with references to all nodes, sorted by tag

    LoadNodeIntoMasterPVS(Self,Self);

    // For each node, add references to any parents that are visible

    AddParentReferences(Self,Self,OldMaxTag);

    // Cleanup

    SetLength(FMasterPVS,0);
    SetLength(FMasterPVS2,0);
  End;
End; // TBSPTreeNode.BuildMasterNodeList

Function TBSPTreeNode.PVSContainsTag(I: Integer): Boolean;
// Performs a binary search for the node tag
Var
  Index    : Integer;
  Step     : Integer;
  Found    : Boolean;
  Done     : Boolean;
  MaxIndex : Integer;

Begin
  MaxIndex := High(PVS);
  If MaxIndex >= 0 Then
  Begin
    Index := 0;
    Step  := MaxIndex;
    If MaxIndex = 0 Then
    Begin
      Found := (PVS[0] = I);
      Done  := Found;
    End
    Else
    Begin
      Found := False;
      Done  := False;
    End;
    While (Step <> 0) And Not (Found Or Done) Do
    Begin
      If I < PVS[Index] Then
      Begin
        If Step < 0 Then
        Begin
          If Index > 0 Then
          Begin
            Index := Index + Step;
            If Index < 0 Then Index := 0;
          End
          Else Done := True; // The key we're searching for definitely isn't in the list
        End
        Else
        Begin
          Step := -Step Div 2;
          If (Step <> 0) And (Index > 0) Then
          Begin
            Inc(Index,Step);
            If Index < 0 Then Index := 0;
          End
          Else Done := True;
        End;
      End
      Else If I > PVS[Index] Then
      Begin
        If Step > 0 Then
        Begin
          If Index < MaxIndex Then
          Begin
            Index := Index + Step;
            If Index > MaxIndex Then Index := MaxIndex;
          End
          Else Done := True; // The key we're searching for definitely isn't in the list
        End
        Else
        Begin
          Step := -Step Div 2;
          If (Step <> 0) And (Index < MaxIndex) Then
          Begin
            Inc(Index,Step);
            If Index > MaxIndex Then Index := MaxIndex;
          End
          Else Done := True;
        End;
      End
      Else Found := True;
    End; // While
    Result := Found;
  End
  Else Result   := True;
End; // TBSPTreeNode.PVSContainsTag

Procedure TBSPTreeNode.Redraw(AlphaNotOneOnly: Boolean; CameraNode: TBSPTreeNode);
// Redraws nodes in top-down order
Var
  I           : Integer;
  Camera      : TCamera;
  Entity      : TEntity;
  D1,D2       : Single;

Begin
  // Make sure we can be seen from the camera

  If CameraNode.PVSContainsTag(Tag) Then
  Begin
    Camera := TSceneGL(FOwner).ActiveCamera;
    If Camera <> Nil Then
    Begin
      If FPlane.ContainsSphere(Camera.Frustum.Sphere) <> scrOutside Then
      Begin
        For I := 0 To FEntities.Count - 1 Do
        Begin
          Entity := TEntity(FEntities.Items[I]);
          Entity.VisibleInFrustum      := (Camera.Frustum.ContainsSphere(Entity.Sphere) <> scrOutside);
          Entity.NewlyVisibleInFrustum := Entity.VisibleInFrustum And Not Entity.OldVisibleInFrustum;
          Entity.OldVisibleInFrustum   := Entity.VisibleInFrustum;
          If (Entity.HasAlpha Or Not AlphaNotOneOnly) And Entity.VisibleInFrustum Then
          Begin
            TSceneGL(FOwner).RedrawQueue.Add(Entity);
          End;
        End; // For I

        If (FChild1 <> Nil) And (FChild2 <> Nil) Then
        Begin
          // Draw whichever one is closer to the camera first

          D1 := FChild1.FBoundingSphere.Center.DistanceFrom2(Camera.Position);
          D2 := FChild2.FBoundingSphere.Center.DistanceFrom2(Camera.Position);
          If D1 < D2 Then
          Begin
            FChild1.Redraw(AlphaNotOneOnly,CameraNode);
            FChild2.Redraw(AlphaNotOneOnly,CameraNode);
          End
          Else
          Begin
            FChild2.Redraw(AlphaNotOneOnly,CameraNode);
            FChild1.Redraw(AlphaNotOneOnly,CameraNode);
          End;
        End
        Else
        Begin
               If FChild1 <> Nil Then FChild1.Redraw(AlphaNotOneOnly,CameraNode)
          Else If FChild2 <> Nil Then FChild2.Redraw(AlphaNotOneOnly,CameraNode);
        End;
      End;
    End;
  End;
End; // TBSPTreeNode.Redraw

Procedure TBSPTreeNode.RedrawUp(AlphaNotOneOnly: Boolean; CameraNode: TBSPTreeNode);
// Redraws nodes in bottom-up order. CameraNode should be the first node called using this method.
Var
  I           : Integer;
  Camera      : TCamera;
  Entity      : TEntity;

Begin
  // Make sure we can be seen from the camera

  If (Self = CameraNode) Or CameraNode.PVSContainsTag(Tag) Then
  Begin
    Camera := TSceneGL(FOwner).ActiveCamera;
    If Camera <> Nil Then
    Begin
      If FPlane.ContainsSphere(Camera.Frustum.Sphere) <> scrOutside Then
      Begin
        For I := 0 To FEntities.Count - 1 Do
        Begin
          Entity := TEntity(FEntities.Items[I]);
          Entity.VisibleInFrustum      := (Camera.Frustum.ContainsSphere(Entity.Sphere) <> scrOutside);
          Entity.NewlyVisibleInFrustum := Entity.VisibleInFrustum And Not Entity.OldVisibleInFrustum;
          Entity.OldVisibleInFrustum   := Entity.VisibleInFrustum;          
          If (Entity.HasAlpha Or Not AlphaNotOneOnly) And Entity.VisibleInFrustum Then
          Begin
            TSceneGL(FOwner).RedrawQueue.Add(Entity);
          End;
        End; // For I
        If FParent <> Nil Then
        Begin
               If (FParent.Child1 <> Self) And (FParent.Child1 <> Nil) Then FParent.Child1.Redraw(AlphaNotOneOnly,CameraNode)
          Else If FParent.Child2 <> Nil Then FParent.Child2.Redraw(AlphaNotOneOnly,CameraNode);
          FParent.RedrawUp(AlphaNotOneOnly,CameraNode);
        End;
      End;
    End;
  End;
End; // TBSPTreeNode.RedrawUp

Function TBSPTreeNode.ContainsEntity(Entity: TEntity): Boolean;
Begin
  // For now all we can do is check the entity's TSphere since its TAxisAlignedBox doesn't take its
  // position, rotation, or scale into account (position and scale would be easy but rotation would
  // be CPU intensive).

  // Entities must be allowed to move to parent nodes since they can span more than one leaf node.
  // Otherwise collision avoidance won't work correctly.

  If FParent <> Nil
   Then Result := (FPlane.ContainsSphere(Entity.Sphere) = scrInside)
   Else Result := True;
End; // TBSPTreeNode.ContainsEntity

//PROFILE-NO
Function TBSPTreeNode.GetNodeContainingPoint(P: T3DPoint): TBSPTreeNode;
Var N: TBSPTreeNode;
Begin
  If (FParent = Nil) Or (FPlane.ContainsPoint(P)) Then
  Begin
    N := Nil;
    If FChild1 <> Nil Then N := FChild1.GetNodeContainingPoint(P);
    If (N = Nil) And (FChild2 <> Nil) Then N := FChild2.GetNodeContainingPoint(P);
    If N = Nil Then N := Self;
    Result := N;
  End
  Else Result := Nil;
End; // TBSPTreeNode.GetNodeContainingPoint
//PROFILE-YES

Procedure TBSPTreeNode.SetChild1(Child: TBSPTreeNode);
Begin
  Try
    If FOwner <> Nil Then TSceneGL(FOwner).LockBSPTree;
    If Child <> Nil Then Child.FParent := Self;
    FChild1 := Child;
  Finally
    If FOwner <> Nil Then TSceneGL(FOwner).UnlockBSPTree;
  End;
End; // TBSPTreeNode.SetChild1

Procedure TBSPTreeNode.SetChild2(Child: TBSPTreeNode);
Begin
  Try
    If FOwner <> Nil Then TSceneGL(FOwner).LockBSPTree;
    If Child <> Nil Then Child.FParent := Self;
    FChild2 := Child;
  Finally
    If FOwner <> Nil Then TSceneGL(FOwner).UnlockBSPTree;
  End;
End; // TBSPTreeNode.SetChild2

Procedure TBSPTreeNode.CheckEntity(Entity: TEntity);
Var B: Boolean;
Begin
  If ContainsEntity(Entity) Then
  Begin
    // Check child nodes and move the entity down if we can

    B := False;
    If FChild1 <> Nil Then
    Begin
      If FChild1.ContainsEntity(Entity) Then
      Begin
        FChild1.AddEntity(Entity);
        RemoveEntity(Entity);
        FChild1.CheckEntity(Entity);
        B := True;
      End;
    End;
    If (FChild2 <> Nil) And Not B Then
    Begin
      If FChild2.ContainsEntity(Entity) Then
      Begin
        FChild2.AddEntity(Entity);
        RemoveEntity(Entity);
        FChild2.CheckEntity(Entity);
      End;
    End;
  End
  Else
  Begin
    // Try to move the entity to our parent 

    If FParent <> Nil Then
    Begin
      FParent.AddEntity(Entity);
      RemoveEntity(Entity);
      FParent.CheckEntity(Entity);
    End;
  End;
End; // TBSPTreeNode.CheckEntity

Function TBSPTreeNode.FindClosestEntity(Source,Dest: T3DPoint): TEntity;
Var
  ContainsSegment : Boolean;
  I               : Integer;
  Entity          : TEntity;
  ChildEntity     : TEntity;

Begin
  If FParent <> Nil
   Then ContainsSegment := FPlane.ContainsPoint(Source) Or FPlane.ContainsPoint(Dest)
   Else ContainsSegment := True;
  Result := Nil;
  If ContainsSegment Then
  Begin
    For I := 0 To FEntities.Count - 1 Do
    Begin
      Entity := TEntity(FEntities.Items[I]);
      If Entity.IsIntersectedBy(Source,Dest) Then Result := Entity;
    End; // For I

    // Check first child node

    If FChild1 <> Nil Then
    Begin
      ChildEntity := FChild1.FindClosestEntity(Source,Dest);
      If ChildEntity <> Nil Then Result := ChildEntity;
    End;

    // Check second child node

    If FChild2 <> Nil Then
    Begin
      ChildEntity := FChild2.FindClosestEntity(Source,Dest);
      If ChildEntity <> Nil Then Result := ChildEntity;
    End;
  End;
End; // TBSPTreeNode.FindClosestEntity

Procedure TBSPTreeNode.MoveToIntersection(Ellipsoid: TEllipsoid; Velocity: T3DPoint; MovementSphere: TSphere; Var NearestDistance,Friction: Single; NewCenter,NewVelocity: T3DPoint; UseFriction: Boolean; ExcludeEntity: TEntity);
Var
  ContainsSphere : Boolean;
  I              : Integer;
  Entity1        : TEntity;

Begin
  If FParent <> Nil
   Then ContainsSphere := (FPlane.ContainsSphere(MovementSphere) <> scrOutside)
   Else ContainsSphere := True;
  If ContainsSphere Then
  Begin
    For I := 0 To FEntities.Count - 1 Do
    Begin
      Entity1 := TEntity(FEntities.Items[I]);
      If (Entity1 <> ExcludeEntity) And ((ExcludeEntity = Nil) Or Not Entity1.CollisionAvoidance {Entity1.CanChangeTreeNodes}) Then
       Entity1.FindIntersection(Ellipsoid,Velocity,MovementSphere,NearestDistance,Friction,NewCenter,NewVelocity,UseFriction);
    End; // For I

    // Check first child node

    If FChild1 <> Nil Then FChild1.MoveToIntersection(Ellipsoid,Velocity,MovementSphere,NearestDistance,Friction,NewCenter,NewVelocity,UseFriction,ExcludeEntity);

    // Check second child node

    If FChild2 <> Nil Then FChild2.MoveToIntersection(Ellipsoid,Velocity,MovementSphere,NearestDistance,Friction,NewCenter,NewVelocity,UseFriction,ExcludeEntity);
  End;
End; // TBSPTreeNode.MoveToIntersection

// ---------------------------
// TPortal
// ---------------------------

Constructor TPortal.Create;
Begin
  ID           := 0;
  PartitionID  := 0;
  FromSectorID := 0;
  ToSectorID   := 0;
  IsMirror     := False;
End; // TPortal.Create

Function TPortal.Clone: TPortal;
Var
  I      : Integer;
  Portal : TPortal;

Begin
  Portal := TPortal.Create;
  Portal.Allocate(High(Vertices) + 1);
  Portal.Plane.Copy(Plane);
  Portal.Flag := Flag;
  For I := 0 To High(Vertices) Do Portal.Vertices[I].Copy(Vertices[I]);
  Portal.FromSectorID := FromSectorID;
  Portal.ID           := ID;
  Portal.IsMirror     := IsMirror;
  Portal.PartitionID  := PartitionID;
  Portal.ToSectorID   := ToSectorID;
  Result := Portal;
End; // TPortal.Clone

// ---------------------------
// TSectorManager
// ---------------------------

Constructor TSectorManager.Create;
Begin
  BSPRoot    := Nil;
  SetLength(Sectors,0);
End; // TSectorManager.Create

Procedure TSectorManager.Setup(Root: TBSPTreeNode);
Var
  I       : Integer;
  Portals : TStringList;

Begin
  BSPRoot := Root;
  I       := 0;
  BSPRoot.AssignIDs(I);
  CreateSectors;
  Portals := CreatePortalList;
  AddPortalsToSectors(Portals);

  // Remove all portals from the list and delete the portal list object

  For I := 0 To Portals.Count - 1 Do Portals.Objects[I].Free;
  Portals.Free;

  FindTruePortals;
End; // TSectorManager.Setup

Procedure TSectorManager.CreateSectors;
Var I: Integer;
Begin
  SetLength(Sectors,BSPRoot.NumLeaves);
  For I := 0 To High(Sectors) Do Sectors[I] := BSPRoot.GetNodeWithTag(I);
End; // TSectorManager.CreateSectors

Function TSectorManager.CreateLargePortal(BspNode: TBSPTreeNode): TPortal;
Var
  Portal      : TPortal;
  AxisFlag    : Integer;
  PlaneNormal : T3DPoint;
  X,Y,Z       : Single;
  A,B,C,D     : Single;

Begin
  Portal := TPortal.Create;
  Portal.Allocate(4);
  AxisFlag := 0;
  PlaneNormal := T3DPoint.Create(BSPNode.Plane.Normal);
  X           := PlaneNormal.X;
  Y           := PlaneNormal.Y;
  Z           := PlaneNormal.Z;

  If (Abs(X) > Abs(Y)) And (Abs(X) > Abs(Z)) Then
  Begin
    AxisFlag := 1;
    Portal.Vertices[0].Y := BSPNode.BoundingBox.MinPt.Y;
    Portal.Vertices[0].Z := BSPNode.BoundingBox.MaxPt.Z;
    Portal.Vertices[1].Y := BSPNode.BoundingBox.MinPt.Y;
    Portal.Vertices[1].Z := BSPNode.BoundingBox.MinPt.Z;
    Portal.Vertices[2].Y := BSPNode.BoundingBox.MaxPt.Y;
    Portal.Vertices[2].Z := BSPNode.BoundingBox.MinPt.Z;
    Portal.Vertices[3].Y := BSPNode.BoundingBox.MaxPt.Y;
    Portal.Vertices[3].Z := BSPNode.BoundingBox.MaxPt.Z;
  End
  Else If (Abs(Y) > Abs(X)) And (Abs(Y) > Abs(Z)) Then
  Begin
    AxisFlag := 2;
    Portal.Vertices[0].X := BSPNode.BoundingBox.MinPt.X;
    Portal.Vertices[0].Z := BSPNode.BoundingBox.MaxPt.Z;
    Portal.Vertices[1].X := BSPNode.BoundingBox.MaxPt.X;
    Portal.Vertices[1].Z := BSPNode.BoundingBox.MaxPt.Z;
    Portal.Vertices[2].X := BSPNode.BoundingBox.MaxPt.X;
    Portal.Vertices[2].Z := BSPNode.BoundingBox.MinPt.Z;
    Portal.Vertices[3].X := BSPNode.BoundingBox.MinPt.X;
    Portal.Vertices[3].Z := BSPNode.BoundingBox.MinPt.Z;
  End
  Else
  Begin
    AxisFlag := 3;
    Portal.Vertices[0].X := BSPNode.BoundingBox.MinPt.X;
    Portal.Vertices[0].Y := BSPNode.BoundingBox.MinPt.Y;
    Portal.Vertices[1].X := BSPNode.BoundingBox.MaxPt.X;
    Portal.Vertices[1].Y := BSPNode.BoundingBox.MinPt.Y;
    Portal.Vertices[2].X := BSPNode.BoundingBox.MaxPt.X;
    Portal.Vertices[2].Y := BSPNode.BoundingBox.MaxPt.Y;
    Portal.Vertices[3].X := BSPNode.BoundingBox.MinPt.X;
    Portal.Vertices[3].Y := BSPNode.BoundingBox.MaxPt.Y;
  End;

  D := BSPNode.Plane.Distance;
  Case AxisFlag Of
    1: Begin
         // YZ Plane

          A := -(Y * BSPNode.BoundingBox.MinPt.Y + Z * BSPNode.BoundingBox.MaxPt.Z + D) / X;          Portal.Vertices[0].X := A;
          A := -(Y * BSPNode.BoundingBox.MinPt.Y + Z * BSPNode.BoundingBox.MinPt.Z + D) / X;          Portal.Vertices[1].X := A;
          A := -(Y * BSPNode.BoundingBox.MaxPt.Y + Z * BSPNode.BoundingBox.MinPt.Z + D) / X;          Portal.Vertices[2].X := A;
          A := -(Y * BSPNode.BoundingBox.MaxPt.Y + Z * BSPNode.BoundingBox.MaxPt.Z + D) / X;          Portal.Vertices[3].X := A;
       End;
    2: Begin
         // XZ Plane

          B := -(X * BSPNode.BoundingBox.MinPt.X + Z * BSPNode.BoundingBox.MaxPt.Z + D) / Y;          Portal.Vertices[0].Y := B;
          B := -(X * BSPNode.BoundingBox.MaxPt.X + Z * BSPNode.BoundingBox.MaxPt.Z + D) / Y;          Portal.Vertices[1].Y := B;
          B := -(X * BSPNode.BoundingBox.MaxPt.X + Z * BSPNode.BoundingBox.MinPt.Z + D) / Y;          Portal.Vertices[2].Y := B;
          B := -(X * BSPNode.BoundingBox.MinPt.X + Z * BSPNode.BoundingBox.MinPt.Z + D) / Y;          Portal.Vertices[3].Y := B;
       End;
    3: Begin
         // XY Plane

          C := -(X * BSPNode.BoundingBox.MinPt.X + Y * BSPNode.BoundingBox.MinPt.Y + D) / Z;          Portal.Vertices[0].Z := C;
          C := -(X * BSPNode.BoundingBox.MaxPt.X + Y * BSPNode.BoundingBox.MinPt.Y + D) / Z;          Portal.Vertices[1].Z := C;
          C := -(X * BSPNode.BoundingBox.MaxPt.X + Y * BSPNode.BoundingBox.MaxPt.Y + D) / Z;          Portal.Vertices[2].Z := C;
          C := -(X * BSPNode.BoundingBox.MinPt.X + Y * BSPNode.BoundingBox.MaxPt.Y + D) / Z;          Portal.Vertices[3].Z := C;
       End;
  End; // Case

  Portal.CalculatePlane;
  If Not PlaneNormal.Equals(Portal.Plane.Normal) Then Portal.Flip;
  
  PlaneNormal.Free;
  Result := Portal;
End; // TSectorManager.CreateLargePortal

Function TSectorManager.ClassifyPortal(Portal: TPortal; Plane: TPlane): TAlphaClassification;
Var
  I      : Integer;
  NumPos : Integer;
  NumNeg : Integer;
  NumCo  : Integer;
  RetVal : TAlphaClassification;

Begin
  NumPos := 0;
  NumNeg := 0;
  NumCo  := 0;
  For I := 0 To High(Portal.Vertices) Do
  Begin
    RetVal := Plane.ClassifyPoint(Portal.Vertices[I]);
    Case RetVal Of
      acInFront: Inc(NumPos);
       acBehind: Inc(NumNeg);
   acCoinciding: Inc(NumCo);
    End; // Case
  End; // For I
       If (NumPos > 0) And (NumNeg = 0) Then Result := acInFront
  Else If (NumNeg > 0) And (NumPos = 0) Then Result := acBehind
  Else If (NumPos = 0) And (NumNeg = 0) Then Result := acCoinciding
  Else Result := acSpanning;
End; // TSectorManager.ClassifyPortal

Function TSectorManager.CreatePortalList: TStringList;
Var
  I,J            : Integer;
  NumPartitions  : Integer;
  Portals        : TStringList;
  BSPNode        : TBSPTreeNode;
  Portal         : TPortal;
  Front          : TPortal;
  Back           : TPortal;
  HasMorePortals : Boolean;
  RetVal         : TAlphaClassification;
  RetSplit       : Boolean;

Begin
  NumPartitions := BspRoot.GetNumPartitions;
  Portals       := TStringList.Create;

  // Create base portals

  For I := 0 To NumPartitions - 1 Do
  Begin
    BSPNode := BSPRoot.GetNodeWithID(I);
    Portal  := CreateLargePortal(BSPNode);
    Portal.PartitionID := I;
    Portals.AddObject('',Portal);
  End; // For I

  // Split any portals along any BSP planes

  For J := 0 To NumPartitions - 1 Do
  Begin
    BSPNode        := BSPRoot.GetNodeWithID(J);
    I              := Portals.Count - 1;
    HasMorePortals := True;
    While HasMorePortals Do
    Begin
      Portal := TPortal(Portals.Objects[I]);
      RetVal := ClassifyPortal(Portal,BSPNode.FPlane);
      If RetVal = acSpanning Then
      Begin
        Front    := TPortal.Create;
        Back     := TPortal.Create;
        RetSplit := Portal.SplitPolygon(BSPNode.FPlane,Front,Back);
        If RetSplit Then
        Begin
          Front.PartitionID := Portal.PartitionID;
          Back.PartitionID  := Portal.PartitionID;
          Portals.AddObject('',Front);
          Portals.AddObject('',Back);
          Portals.Objects[I].Free;
          Portals.Delete(I);
        End
        Else
        Begin
          Front.Free;
          Back.Free;
        End;
      End;
      If I <> 0 Then Dec(I) Else HasMorePortals := False;
    End; // While
  End; // For J

  // Assign IDs to the resulting portals

  For I := 0 To Portals.Count - 1 Do TPortal(Portals.Objects[I]).ID := I;
  Result := Portals;
End; // TSectorManager.CreatePortalList

Procedure TSectorManager.AddPortal(Portal: TPortal; BSPNode: TBSPTreeNode);
// Pushes the portal down the BSP tree until it finds one or more leaf nodes corresponding to it
Var RetVal: TAlphaClassification;
Begin
  If BSPNode.LeafTag < 0 Then
  Begin
    RetVal := ClassifyPortal(Portal,BSPNode.Plane);
    Case RetVal Of
      acInFront:
      Begin
        // The portal is inside the splitting plane

        If BSPNode.Child1 <> Nil Then AddPortal(Portal,BSPNode.Child1);
      End;
      acBehind:
      Begin
        // The portal is outside the splitting plane

        If BSPNode.Child2 <> Nil Then AddPortal(Portal,BSPNode.Child2);
      End;
      acCoinciding:
      Begin
        // The portal is coplanar with the splitting plane

        If BSPNode.Child1 <> Nil Then AddPortal(Portal,      BSPNode.Child1);
        If BSPNode.Child2 <> Nil Then AddPortal(Portal.Clone,BSPNode.Child2);
      End;
    End; // Case
  End
  Else BSPNode.FPortals.AddObject('',Portal);
End; // TSectorManager.AddPortal

Procedure TSectorManager.AddPortalsToSectors(Portals: TStringList);
Var
  I      : Integer;
  Portal : TPortal;

Begin
  For I := 0 To Portals.Count - 1 Do
  Begin
    Portal := TPortal(Portals.Objects[I]).Clone;
    AddPortal(Portal,BSPRoot);
  End; // For I
End; // TSectorManager.AddPortalsToSectors

Function TSectorManager.CheckForSinglePortal(SectorIndex,PortalIndex: Integer): Boolean;
Var
  I,J    : Integer;
  Test   : TPortal;
  Portal : TPortal;
  Found  : Boolean;

Begin
  Test  := TPortal(Sectors[SectorIndex].FPortals.Objects[PortalIndex]);
  Found := False;
  I     := 0;
  While (I <= High(Sectors)) And Not Found Do
  Begin
    If I <> SectorIndex Then
    Begin
      J := 0;
      While (J < Sectors[I].FPortals.Count) And Not Found Do
      Begin
        Portal := TPortal(Sectors[I].FPortals.Objects[J]);
        If Portal.ID = Test.ID Then
        Begin
          Portal.FromSectorID := I;
          Portal.ToSectorID   := SectorIndex;

          Test.FromSectorID   := SectorIndex;
          Test.ToSectorID     := I;

          Found               := True;
        End;
        Inc(J);
      End; // While
    End;
    Inc(I);
  End; // While
  Result := Found;
End; // TSectorManager.CheckForSinglePortal

Procedure TSectorManager.ClipPortalToSector(Portal: TPortal; Side: TAlphaClassification);
Var
  I,J,K,L,M  : Integer;
  SectorID   : Integer;
  RetVal     : TAlphaClassification;
  Front      : TPortal;
  Back       : TPortal;
  SideP      : TPortal;
  RetSplit   : Boolean;
  BSPNode    : TBSPTreeNode;
  Entity     : TEntity;
  Renderable : TRenderable;
  Model      : TModel;
  Transform  : Boolean;
  Face       : TFace;
  FacePlane  : TPlane;
  P          : Array[0..2] Of T3DPoint;

Begin
  If Side = acInFront
   Then SectorID := Portal.FromSectorID
   Else SectorID := Portal.ToSectorID;
  BSPNode   := Sectors[SectorID];
  FacePlane := TPlane.Create;
  P[0]      := T3DPoint.Create;
  P[1]      := T3DPoint.Create;
  P[2]      := T3DPoint.Create;
  For J := 0 To BSPNode.FEntities.Count - 1 Do
  Begin
    Entity     := TEntity(BSPNode.FEntities.Items[J]);
    If Not Entity.CanChangeTreeNodes Then
    Begin
      Renderable := TRenderable(Entity.Renderable);
      If (Renderable <> Nil) And (Renderable.Count > 0) Then
      Begin
        Model := Renderable.Models[0];
        For K := 0 To High(Model.Faces) Do
        Begin
          Face := Model.Faces[K];
//          If Face.NumVerts > 2 Then
//          Begin
            For L := 0 To 2 Do
            Begin
//              M := Face.Vertices[L] * 3;
              M := (K * 3 + L) * 3;
              P[L].Copy(Model.Positions.DataArray[M],Model.Positions.DataArray[M + 1],Model.Positions.DataArray[M + 2]);
//              P[L].Copy(Model.Vertices[Face.Vertices[L]].Position);
              P[L].CounterClockwiseRotate(Entity.Rotation,False,False,True);
              P[L].CounterClockwiseRotate(Entity.Rotation,False,True,False);
              P[L].CounterClockwiseRotate(Entity.Rotation,True,False,False);
              P[L].Multiply(Entity.Scale);
              P[L].Add(Entity.Position);
            End; // For L
            FacePlane.Setup(P[2],P[1],P[0]);
            RetVal := ClassifyPortal(Portal, FacePlane);
            If RetVal = acSpanning Then
            Begin
              // The polygon's plane slices through the portal

              Front := TPortal.Create;
              Back  := TPortal.Create;

              // The remarked-out portion doesn't look right.  I think we should always be using the front side...

              SideP := Front;
{
              If Side = acInFront
               Then SideP := Front
               Else SideP := Back;
}
              RetSplit := Portal.SplitPolygon(FacePlane, Front, Back);
              If RetSplit Then
              Begin
                Portal.Allocate(High(SideP.Vertices) + 1);
                For I := 0 To High(SideP.Vertices) Do Portal.Vertices[I].Copy(SideP.Vertices[I]);
                Portal.CalculatePlane;
              End;
              Front.Free;
              Back.Free;
            End;
//          End;
        End; // For K
      End;
    End;
  End; // For J
  P[0].Free;
  P[1].Free;
  P[2].Free;
  FacePlane.Free;
End; // TSectorManager.ClipPortalToSector

Procedure TSectorManager.CheckForSinglePortals;
Var
  I              : Integer;
  NumPortal      : Integer;
  HasMorePortals : Boolean;
  Portal         : TPortal;

Begin
  For I := 0 To High(Sectors) Do
  Begin
    NumPortal      := Sectors[I].FPortals.Count;
    HasMorePortals := False;

    If NumPortal > 0 Then
    Begin
      HasMorePortals := True;
      Dec(NumPortal);
    End;

    While HasMorePortals Do
    Begin
      // Look for a corresponding portal in another sector

      If CheckForSinglePortal(I, NumPortal) Then
      Begin
        // The portal has a corresponding twin in another sector

        Portal := TPortal(Sectors[I].FPortals.Objects[NumPortal]);

        // Clip the portal so that it only covers the open area between the sectors

        ClipPortalToSector(Portal, acInFront);
        ClipPortalToSector(Portal, acBehind);
      End
      Else
      Begin
        // No corresponding portal could be found in another sector.  This means that there are no
        // sectors/polygons on the other side of this portal, so get rid of it.  It likely means
        // that this portal is a bogus one that resulted from a split elsewhere along the BSP plane.

        Sectors[I].FPortals.Objects[NumPortal].Free;
        Sectors[I].FPortals.Delete(NumPortal);
      End;
      
      If NumPortal <> 0
       Then Dec(NumPortal)
       Else HasMorePortals := False;
    End; // While
  End; // For I
End; // TSectorManager.CheckForSinglePortals

Procedure TSectorManager.MakePortalsInward;
Var
  I,J    : Integer;
  Portal : TPortal;

Begin
  // There is a better way to do this.  Since each sector is convex (and we have a
  // bounding sphere for each of them), by definition each portal should be made
  // to face the center of the bounding sphere.

  For I := 0 To High(Sectors) Do
  Begin
    For J := 0 To Sectors[I].FPortals.Count - 1 Do
    Begin
      Portal := TPortal(Sectors[I].FPortals.Objects[J]);
      If Not Portal.Plane.ContainsPoint(Sectors[I].BoundingSphere.Center) Then Portal.Flip;
    End; // For J
  End; // For I

{
  // MAKE SURE EVERY POLYGON IN THE SECTOR IS IN FRONT OR COINCIDING WITH ITS PORTAL

  For I := 0 To High(Sectors) Do
  Begin
    For J := 0 To Sectors[I].FPortals.Count - 1 Do
    Begin
      Portal := TPortal(Sectors[I].FPortals.Objects[J]);
      CPolygon* pPolygon = pSector[i].pPolygon;
      while (pPolygon != 0)
      Begin
        if (pPolygon->classify(pPortal->plane) == ALPHA_BEHIND)
        Begin
          pPortal->flip();
          pPolygon = pSector[i].pPolygon;
        End
        Else pPolygon = pPolygon->pNext;
      End; // While
    End; // For J
  End; // For I
}
End; // TSectorManager.MakePortalsInward

Procedure TSectorManager.RemoveExtraPortals;
Var
  I,J       : Integer;
  CheckMore : Boolean;
  Invalid   : Boolean;
  Portal    : TPortal;

Begin
  For I := 0 To High(Sectors) Do
  Begin
    CheckMore := True;
    While CheckMore Do
    Begin
      CheckMore := False;
      J := 0;
      While J < Sectors[I].FPortals.Count Do
      Begin
        Invalid := false;
        Portal  := TPortal(Sectors[I].FPortals.Objects[j]);

        // There is a better way to do this.  As in MakePortalsInward, we can use the other
        // sector's bounding sphere to validate portals.

        If Portal.Plane.ContainsPoint(Sectors[Portal.ToSectorID].BoundingSphere.Center) Then
        Begin
          // The portal should never point at the target sector; therefore, the portal is invalid

          Sectors[I].FPortals.Objects[J].Free;
          Sectors[I].FPortals.Delete(J);
        End
        Else Inc(J);

{
        // Temporarily flip the portal

        Portal.Flip;

        // Classify against all polygons in the toSector

        unsigned int toSector = pPortal->toSectorId;
        CPolygon* pPolygon = pSector[toSector].pPolygon;
        while (pPolygon != 0)
        Begin
          int retval = pPortal->classify(pPolygon->plane);
          if (retval == ALPHA_BEHIND)
          Begin
            // delete this portal
            invalid = true;
            break;
          End;
          if (retval == ALPHA_COINCIDING && pPortal->plane.normal != pPolygon->plane.normal)
          Begin
            invalid = true;
            break;
          End;
          pPolygon = pPolygon->pNext;
        End; // While
        If Invalid Then
        Begin
          pSector[i].portalList.remove(j);
          checkMore = true;
          break;
        End
        Else
        Begin
          // This portal is good

          Portal.Flip;
        End;
}
      End; // While
    End; // While
  End; // For I
End; // TSectorManager.RemoveExtraPortals

Procedure TSectorManager.FindTruePortals;
Begin
  CheckForSinglePortals;
  MakePortalsInward;
  RemoveExtraPortals;
End; // TSectorManager.FindTruePortals

// ---------------------------
// TEntity
// ---------------------------

Constructor TEntity.Create(AOwner: TObject);
Begin
  Inherited Create;
  Visible             := False;            // By default the entity is *not* visible --
                                           // we don't want anything trying to draw it until we're ready
  Redrawing           := False;
  id                  := 0;                // Initiallly this value has no importance
  WireFrame           := 2;                // By default use solid poligons for rendering
  FPosition           := T3DPoint.Create(0,0,0);
  FLocalPosition      := T3DPoint.Create(0,0,0);
  FRotation           := T3DPoint.Create(0,0,0);
  FScale              := T3DPoint.Create(1,1,1);
  FLocalScale         := T3DPoint.Create(1,1,1);
  FDeltaPosition      := T3DPoint.Create(0,0,0);
  FDeltaRotation      := T3DPoint.Create(0,0,0);
  FDeltaScale         := T3DPoint.Create(0,0,0);
  FDeltaPosWork       := T3DPoint.Create;
  FDeltaRotWork       := T3DPoint.Create;
  FDeltaSclWork       := T3DPoint.Create;
  FRotationWork       := T3DPoint.Create;
  FLastPosition       := T3DPoint.Create;
  FLastRotation       := T3DPoint.Create;
  FLastScale          := T3DPoint.Create; // Note that this doesn't have the same initial value as FScale!!!
  FBox                := TAxisAlignedBox.Create;
  FSphere             := TSphere.Create;
  FSphereWork         := TSphere.Create;
  FSphereWork2        := TSphere.Create;
  FSourceWork         := T3DPoint.Create;
  FDestWork           := T3DPoint.Create;
  FEllipseWork        := TEllipsoid.Create;
  FEllipsoid          := TEllipsoid.Create;
  FTransformMatrix    := T4x4Matrix.Create;
  FOrientation        := TQuaternion.Create;
  BoundR              := 255;
  BoundG              := 255;
  BoundB              := 255;
  FOldTime            := 0;
  FAnimFrame          := 0;
  FLastAnimFrame      := -1;
  Owner               := AOwner;
  FRenderable         := Nil;
  FBSPTreeNode        := Nil;
  FOneShotRenderable  := Nil;
  CanChangeTreeNodes  := False;
  FHasAlpha           := False;
  CollisionAvoidance  := False;
  Gravity             := 9.8;
  AltCollisionModel   := Nil;
  AnimSpeed           := 1;
  OneShotAnimSpeed    := 1;

  Init;
  Visible := True;
End; // TEntity.Create

Destructor TEntity.Destroy;
Begin
  Visible := False;
  While Redrawing Do Sleep(1);
  FPosition.Free;
  FLocalPosition.Free;
  FRotation.Free;
  FScale.Free;
  FLocalScale.Free;
  FLastPosition.Free;
  FLastRotation.Free;
  FLastScale.Free;
  FDeltaPosition.Free;
  FDeltaRotation.Free;
  FDeltaScale.Free;
  FDeltaPosWork.Free;
  FDeltaRotWork.Free;
  FDeltaSclWork.Free;
  FRotationWork.Free;
  FBox.Free;
  FSphere.Free;
  FSphereWork.Free;
  FSphereWork2.Free;
  FSourceWork.Free;
  FDestWork.Free;
  FEllipseWork.Free;
  FEllipsoid.Free;
  FTransformMatrix.Free;
  FOrientation.Free;
  SetLength(VertexColors,0);
  SetLength(TextureSet,0);
  AltCollisionModel.Free;
  Inherited;
End; // TEntity.Destroy

Procedure TEntity.Init;
Begin
  Name                := '';
  ID                  := 0;                // Initiallly this value has no importance
  WireFrame           := 2;                // By default use solid poligons for rendering
  Visible             := True;             // By default the entity is visible
  FPosition.Copy(0,0,0);
  FLocalPosition.Copy(0,0,0);
  FRotation.Copy(0,0,0);
  FScale.Copy(1,1,1);
  FLocalScale.Copy(1,1,1);
  FDeltaPosition.Copy(0,0,0);
  FDeltaRotation.Copy(0,0,0);
  FDeltaScale.Copy(0,0,0);
  FDeltaPosWork.Copy(0,0,0);
  FDeltaRotWork.Copy(0,0,0);
  FDeltaSclWork.Copy(0,0,0);
  FRotationWork.Copy(0,0,0);
  FLastPosition.Copy(0,0,0);
  FLastRotation.Copy(0,0,0);
  FLastScale.Copy(0,0,0);  // Note that this doesn't have the same initial value as FScale!!!
  FBox.Setup(0,0,0,0,0,0);
  FSphere.Setup(0,0,0,0,0,0);
  FSphereWork.Setup(0,0,0,0,0,0);
  FSphereWork2.Setup(0,0,0,0,0,0);
  FSourceWork.Copy(0,0,0);
  FDestWork.Copy(0,0,0);
  FEllipseWork.Center.Copy(0,0,0);
  FEllipseWork.Radius.Copy(0,0,0);
  FEllipsoid.Center.Copy(0,0,0);
  FEllipsoid.Radius.Copy(0,0,0);
  FTransformMatrix.LoadIdentity;
  BoundR               := 255;
  BoundG               := 255;
  BoundB               := 255;
  FOldTime             := 0;
  FAnimFrame           := 0;
  FLastAnimFrame       := -1;
  CanChangeTreeNodes   := False;
  FHasAlpha            := False;
  FOneShotRenderable   := Nil;
  FStopOnLastFrame     := False;
  FRevertToPositive    := True;
  VisibleInFrustum     := False;
  OccludeCounter       := 0;
  FZCenter             := 0;
  FMinZ0               := 0;
  ModelType            := 0;
  SetLength(TextureSet,1);
  TextureSet[0].TextureID   := 0;
  TextureSet[0].TextureTint := $00FFFFFF;
  TextureSet[0].TextureEmit := 0;
  FOnRedraw            := Nil;
  Tag                  := 0;
  NearLight            := 0;
  FSetTransformMatrix  := False;
  OldVisibleInFrustum  := False;
  NewlyVisibleInFrustum := False;
End; // TEntity.Init

Function TEntity.IsIntersectedBy(Source,Dest: T3DPoint): Boolean;
Var
  SX,SY,SZ : Single;
  B        : Boolean;
  R        : TRenderable;

Begin
  Result := False;
  If FSphereWork.IntersectsLineSegment(Source,Dest) Then
  Begin
//    Try
//      If Owner <> Nil Then TSceneGL(Owner).LockRenderable;
      If FRenderable <> Nil Then
      Begin
        If FOneShotRenderable <> Nil
         Then R := TRenderable(FOneShotRenderable)
         Else R := TRenderable(FRenderable);

        SX := FScale.X;
        SY := FScale.Y;
        SZ := FScale.Z;
        If (SX <> 0) And (SY <> 0) And (SZ <> 0) And (FLastAnimFrame >= 0) Then
        Begin
          FSourceWork.Copy(Source);
          FDestWork.Copy(Dest);

          FSourceWork.Subtract(FPosition);
          FDestWork.Subtract(FPosition);

          FSourceWork.Multiply(1 / SX,1 / SY,1 / SZ);
          FDestWork.Multiply(1 / SX,1 / SY,1 / SZ);

          FSourceWork.ClockwiseRotate(FRotationWork,True,True,True);
          FDestWork.ClockwiseRotate(FRotationWork,True,True,True);

          B := R.IsIntersectedBy(FLastAnimFrame,FSourceWork,FDestWork);
          If B Then
          Begin
            FDestWork.CounterClockwiseRotate(FRotationWork,False,False,True);
            FDestWork.CounterClockwiseRotate(FRotationWork,False,True,False);
            FDestWork.CounterClockwiseRotate(FRotationWork,True,False,False);
            FDestWork.Multiply(SX,SY,SZ);
            FDestWork.Add(FPosition);
            Dest.Copy(FDestWork);
          End;
          Result := B;
        End;
      End
      Else Result := False;
//    Finally
//      If Owner <> Nil Then TSceneGL(Owner).UnlockRenderable;
//    End;
  End
End; // TEntity.IsIntersectedBy

Procedure TEntity.FindIntersection(Ellipsoid: TEllipsoid; Velocity: T3DPoint; MovementSphere: TSphere; Var NearestDistance,Friction: Single; NewCenter,NewVelocity: T3DPoint; UseFriction: Boolean);
Var
  SX,SY,SZ,SL         : Single;
  Nearest             : Single;
  Len                 : Single;
  DestinationPoint    : T3DPoint;
  NewBasePoint        : T3DPoint;
  V                   : T3DPoint;
  SlidePlane          : TPlane;
  NewDestinationPoint : T3DPoint;
  HasRotate           : Boolean;

  PushBack : Boolean;

//  NewVelocityVector   : T3DPoint;

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
//  Log('TEntity.FindIntersection');
(*
  If (FSPhereWork.Radius > 100) And (Name = 'SirensBane') { And (FSphereWork.Radius < 71)} Then
  Begin
    Log(Format('Checking if entity intersects movement sphere...checking entity (FSphereWork = (%8.2f, %8.2f, %8.2f), %8.2f, MovementSphere = (%8.2f, %8.2f, %8.2f), %8.2f',
               [FSphereWork.Center.X, FSphereWork.Center.Y, FSphereWork.Center.Z, FSphereWork.Radius,
                MovementSphere.Center.X, MovementSphere.Center.Y, MovementSphere.Center.Z, MovementSphere.Radius]));
    Log(Format('Ellipsoid.Center = (%8.2f, %8.2f, %8.2f)',
               [Ellipsoid.Center.X,Ellipsoid.Center.Y,Ellipsoid.Center.Z]));
    Log(Format('Velocity = (%8.2f, %8.2f, %8.2f)',
               [Velocity.X,Velocity.Y,Velocity.Z]));
  End;
*)
  If (FLastAnimFrame >= 0) And FSphereWork.Intersects(MovementSphere) Then
  Begin
(*
    If (FSPhereWork.Radius > 100) And (Name = 'SirensBane') { And (FSphereWork.Radius < 71)} Then
    Begin
      Log(Format('Entity intersects movement sphere...checking entity (FSphereWork = (%8.2f, %8.2f, %8.2f), %8.2f, MovementSphere = (%8.2f, %8.2f, %8.2f), %8.2f',
                 [FSphereWork.Center.X, FSphereWork.Center.Y, FSphereWork.Center.Z, FSphereWork.Radius,
                  MovementSphere.Center.X, MovementSphere.Center.Y, MovementSphere.Center.Z, MovementSphere.Radius]));
    End;
*)
//      ModelCanLog := True;
//    End
//    Else ModelCanLog := False;

//    Try
//      If Owner <> Nil Then TSceneGL(Owner).LockRenderable;
      If FRenderable <> Nil Then
      Begin
        SX := FScale.X;
        SY := FScale.Y;
        SZ := FScale.Z;
        SL := FScale.GetLength;
        If (SX <> 0) And (SY <> 0) And (SZ <> 0) Then
        Begin
          PushBack  := (Velocity.X = 0) And (Velocity.Y = 0) And (Velocity.Z < 0);
          HasRotate := Not FRotationWork.IsZero;

          FEllipseWork.Copy(Ellipsoid);
          FEllipseWork.Center.Subtract(FPosition);
          FEllipseWork.Center.Multiply(1 / SX,1 / SY,1 / SZ);
          FEllipseWork.Radius.Multiply(1 / SX,1 / SY,1 / SZ);
          If HasRotate Then
          Begin
            FEllipseWork.Center.ClockwiseRotate(FRotationWork,False,False,True);
            FEllipseWork.Center.ClockwiseRotate(FRotationWork,False,True,False);
            FEllipseWork.Center.ClockwiseRotate(FRotationWork,True,False,False);
          End;

          FSourceWork.Copy(Velocity);
          FSourceWork.Multiply(1 / SX,1 / SY,1 / SZ);
          If HasRotate Then
          Begin
            FSourceWork.ClockwiseRotate(FRotationWork,False,False,True);
            FSourceWork.ClockwiseRotate(FRotationWork,False,True,False);
            FSourceWork.ClockwiseRotate(FRotationWork,True,False,False);
          End;

          FSphereWork2.Copy(MovementSphere);
          FSphereWork2.Center.Subtract(FPosition);
          FSphereWork2.Center.Multiply(1 / SX,1 / SY,1 / SZ);
          If HasRotate Then
          Begin
            FSphereWork2.Center.ClockwiseRotate(FRotationWork,False,False,True);
            FSphereWork2.Center.ClockwiseRotate(FRotationWork,False,True,False);
            FSphereWork2.Center.ClockwiseRotate(FRotationWork,True,False,False);
          End;
          FSphereWork2.Radius := FSphereWork2.Radius * Max(Max(1 / SX,1 / SY),1 / SZ);

          If (FEllipseWork.Radius.X <> 0) And (FEllipseWork.Radius.Y <> 0) And (FEllipseWork.Radius.Z <> 0) Then
          Begin
{
            FEllipseWork.Center.Divide(FEllipseWork.Radius);
            FSourceWork.Divide(FEllipseWork.Radius);
}
            Nearest := NearestDistance;

            If AltCollisionModel <> Nil
             Then TModel(AltCollisionModel).FacesCollideWithEllipsoid(FEllipseWork,FSourceWork,FSphereWork2,Nearest,FDestWork,Friction)
             Else TRenderable(FRenderable).FindIntersection(FLastAnimFrame,FEllipseWork,FSourceWork,FSphereWork2,Nearest,FDestWork,Friction);

(*
            If (FSPhereWork.Radius > 100) And (Name = 'SirensBane') { And (FSphereWork.Radius < 71)} Then
            Begin
              Log(Format('Nearest = %8.2f, Ellipsoid.Center = (%8.2f, %8.2f, %8.2f)',
                         [Nearest,Ellipsoid.Center.X,Ellipsoid.Center.Y,Ellipsoid.Center.Z]));
              Log('');
            End;
*)

            If Nearest < NearestDistance Then
            Begin
              NearestDistance := Nearest;
//              NearestDistance := NearestDistance * FEllipseWork.Radius.GetLength;
//              ClosestPoint.Multiply(FEllipseWork.Radius);


{
              FDestWork.Rotate(FRotationWork,True,False,False);
              FDestWork.Multiply(SX,SY,SZ);
              FDestWork.Add(FPosition);
}
//              ClosestPoint.Divide(Ellipsoid.Radius);  // Convert to ellipse space
{
              FDestWork.Rotate(FRotationWork,True,False,False);
              FDestWork.Multiply(SX,SY,SZ);
              FDestWork.Normalize;
              FDestWork.Divide(Ellipsoid.Radius);  // Convert to ellipse space
              FDestWork.Normalize;
}
//              FDestWork.Copy(Velocity);
//              FDestWork.Divide(Ellipsoid.Radius); // Convert to ellipse space

//              If FRotationWork.Z <> 0 Then Log(Format('Ellipsoid.Center = (%8.2f,%8.2f,%8.2f)',[Ellipsoid.Center.X,Ellipsoid.Center.Y,Ellipsoid.Center.Z]));
//              If FRotationWork.Z <> 0 Then Log(Format('FEllipseWork.Center = (%8.2f,%8.2f,%8.2f)',[FEllipseWork.Center.X,FEllipseWork.Center.Y,FEllipseWork.Center.Z]));

              NewBasePoint     := T3DPoint.Create(FEllipseWork.Center);
              NewBasePoint.Divide(FEllipseWork.Radius); // Convert to ellipse space

//              If FRotationWork.Z <> 0 Then Log(Format('NewBasePoint (unadjusted) = (%8.2f,%8.2f,%8.2f)',[NewBasePoint.X,NewBasePoint.Y,NewBasePoint.Z]));

              DestinationPoint := T3DPoint.Create(FEllipseWork.Center);
              DestinationPoint.Add(FSourceWork);
              DestinationPoint.Divide(FEllipseWork.Radius); // Convert to ellipse space

//              If FRotationWork.Z <> 0 Then Log(Format('DestinationPoint = (%8.2f,%8.2f,%8.2f)',[DestinationPoint.X,DestinationPoint.Y,DestinationPoint.Z]));

              // Only update if we are not already very close, and if so then we only want to move very close to the
              // intersection, not the exact spot

              If (NearestDistance >= 2 * VeryCloseDistance) Or ((NearestDistance < 0) And PushBack) Then  // !!
              Begin
                V   := T3DPoint.Create(FSourceWork);
                V.Divide(Ellipsoid.Radius); // Convert to ellipse space
                Len := V.GetLength;
                If Len > 0 Then
                Begin
                  If (NearestDistance < 0) And PushBack
                   Then V.Multiply((NearestDistance - 2 * VeryCloseDistance) / Len)
                   Else V.Multiply(Max(NearestDistance - 2 * VeryCloseDistance,0) / Len); // !!
                End;
                NewBasePoint.Add(V);
{
                V   := T3DPoint.Create(Velocity);
                Len := V.GetLength;
                If Len > 0 Then V.Multiply((NearestDistance - VeryCloseDistance) / Len);
                NewBasePoint.Add(V);
}
{
              NearestDistance := NearestDistance - VeryCloseDistance;
              If NearestDistance < 0 Then NearestDistance := 0;
}

                // Adjust polygon intersection point so the sliding plane will be unaffected by the fact that we move
                // slightly less than the collision tells us to

                V.Copy(FSourceWork);
                V.Divide(Ellipsoid.Radius); // Convert to ellipse space
                Len := V.GetLength;
                If Len > 0 Then V.Multiply(VeryCloseDistance / Len);
//                V.Normalize;
//                V.Multiply(VeryCloseDistance);
                FDestWork.Subtract(V);


                V.Free;

              End;

//              If FRotationWork.Z <> 0 Then Log(Format('NewBasePoint (adjusted) = (%8.2f,%8.2f,%8.2f)',[NewBasePoint.X,NewBasePoint.Y,NewBasePoint.Z]));

              // Determine the sliding plane (in ellipse space)

              SlidePlane := TPlane.Create;
              SlidePlane.Normal.Copy(NewBasePoint);
              SlidePlane.Normal.Subtract(FDestWork);
              SlidePlane.Normal.Normalize;
              SlidePlane.Distance := -SlidePlane.Normal.Dot(FDestWork);
{
              SlidePlane := TPlane.Create;
              SlidePlane.Normal.Copy(FDestWork);
//              SlidePlane.Normal.Copy(NewBasePoint);
//              SlidePlane.Normal.Subtract(ClosestPoint);
              SlidePlane.Distance := -SlidePlane.Normal.Dot(ClosestPoint);
}
              // Get the new destination point

              NewDestinationPoint := T3DPoint.Create(SlidePlane.Normal);
              NewDestinationPoint.Multiply(-SlidePlane.SignedDistanceFromPoint(DestinationPoint));
              NewDestinationPoint.Add(DestinationPoint);

//              If FRotationWork.Z <> 0 Then Log(Format('NewDestinationPoint = (%8.2f,%8.2f,%8.2f)',[NewDestinationPoint.X,NewDestinationPoint.Y,NewDestinationPoint.Z]));

              // Generate the slide vector, wihch will become our new velocity vector for the next iteration

//              NewVelocityVector := T3DPoint.Create(FDestWork,NewDestinationPoint);



              NewVelocity.Copy(FDestWork,NewDestinationPoint);

//              If FRotationWork.Z <> 0 Then Log(Format('NewVelocity = (%8.2f,%8.2f,%8.2f)',[NewVelocity.X,NewVelocity.Y,NewVelocity.Z]));

              NewVelocity.Multiply(FEllipseWork.Radius);  // Convert back to R3 space
              If HasRotate Then NewVelocity.CounterClockwiseRotate(FRotationWork,True,True,True);
              If SL <> 0 Then NewVelocity.Multiply(SX / SL,SY / SL,SZ / SL);

              If UseFriction Then
              Begin
                If SlidePlane.Normal.Z < Friction Then NewVelocity.Copy(0,0,0);
              End;


              // Move the ellipsoid

//              Ellipsoid.Center.Copy(NewBasePoint);

//              ClosestPoint.Multiply(Ellipsoid.Radius);  // Convert back to R3 space

              NewCenter.Copy(NewBasePoint);
              NewCenter.Multiply(FEllipseWork.Radius);  // Convert back to R3 space

//              If FRotationWork.Z <> 0 Then Log(Format('NewCenter (unadjusted) = (%8.2f,%8.2f,%8.2f)',[NewCenter.X,NewCenter.Y,NewCenter.Z]));

              If HasRotate Then NewCenter.CounterClockwiseRotate(FRotationWork,True,True,True);
              NewCenter.Multiply(SX,SY,SZ);
              NewCenter.Add(FPosition);

//              If FRotationWork.Z <> 0 Then Log(Format('NewCenter (adjusted) = (%8.2f,%8.2f,%8.2f)',[NewCenter.X,NewCenter.Y,NewCenter.Z]));

              // Cleanup

//              NewVelocityVector.Free;
              NewDestinationPoint.Free;
              SlidePlane.Free;
              DestinationPoint.Free;
              NewBasePoint.Free;
            End;
          End;
        End;
      End;
//    Finally
//      If Owner <> Nil Then TSceneGL(Owner).UnlockRenderable;
//    End;
//    Nearest := NearestDistance;
  End;
End; // TEntity.FindIntersection

Procedure TEntity.SetBoundColor(iR,iG,iB: Byte);
Begin
  BoundR := iR;
  BoundG := iG;
  BoundB := iB;
End; // TEntity.SetBoundColor

Function TEntity.GetRenderable: TObject;
Begin
//  Try
//    If Owner <> Nil Then TSceneGL(Owner).LockRenderable;
    Result := FRenderable;
//  Finally
//    If Owner <> Nil Then TSceneGL(Owner).UnlockRenderable;
//  End;
End; // TEntity.GetRenderable

Procedure TEntity.SetRenderable(ARenderable: TObject);
Var V: Boolean;
Begin
  V       := Visible;
  Visible := False;
  While Redrawing Do Sleep(1);
//  Try
//    If Owner <> Nil Then TSceneGL(Owner).LockRenderable;
    FRenderable := ARenderable;
    GetExtents;
//  Finally
//    If Owner <> Nil Then TSceneGL(Owner).UnlockRenderable;
//  End;
  Visible := V;
End; // TEntity.SetRenderable

Procedure TEntity.Update;
Const Fudge = 1;
Var
  Time             : LongInt;
  ElapsedTime      : Single; // Elapsed time in seconds
  LastAnimFrame    : Single;
  DiffPosition     : Boolean;
  DiffRotation     : Boolean;
  DiffScale        : Boolean;
  CurrentAnimSpeed : Single; // The speed we're currently using (normal or one-shot)

Begin
  DiffPosition := Not FPosition.Equals(FLastPosition);
  DiffRotation := Not FRotation.Equals(FLastRotation);
  DiffScale    := Not FScale.Equals(FLastScale);

  FLastPosition.Copy(FPosition);
  FLastRotation.Copy(FRotation);
  FLastScale.Copy(FScale);

  If Visible And VisibleInFrustum Then
  Begin
    Time := LongInt(GetTickCount);
    If (Time > FOldTime) Or NewlyVisibleInFrustum Then
    Begin
      FDeltaPosWork.Copy(FDeltaPosition);
      FDeltaRotWork.Copy(FDeltaRotation);
      FDeltaSclWork.Copy(FDeltaScale);
      If FOldTime <> 0 Then
      Begin
        ElapsedTime := (Time - FOldTime) / 1000;
        FDeltaPosWork.Multiply(ElapsedTime);
        FDeltaRotWork.Multiply(ElapsedTime);
        FDeltaSclWork.Multiply(ElapsedTime);
      End
      Else ElapsedTime := 1;

      If CollisionAvoidance And CanChangeTreeNodes Then
      Begin
//        If VisibleInFrustum Then
//        Begin
          If NewlyVisibleInFrustum Or
             DiffPosition Or
             DiffRotation Or
             DiffScale    Or
             Not (FDeltaPosition.IsZero And
                  FDeltaRotation.IsZero And
                  FDeltaScale.IsZero)   Then
          Begin
            FEllipsoid.Center.Copy(FPosition.X,FPosition.Y,FPosition.Z + FMinZ0 * FScale.Z + FEllipsoid.Radius.Z);

            // Have to offset by the bounding box's center to account for off-center objects

            TSceneGL(Owner).InternalMoveObject(FEllipsoid,FDeltaPosWork,Gravity,Self);
            FPosition.X := FEllipsoid.Center.X;
            FPosition.Y := FEllipsoid.Center.Y;
            FPosition.Z := FEllipsoid.Center.Z - FEllipsoid.Radius.Z - FMinZ0 * FScale.Z;
          End;
//        End;
      End
      Else FPosition.Add(FDeltaPosWork);

      FRotation.Add(FDeltaRotWork);
      FScale.Add(FDeltaSclWork);
      LastAnimFrame := FAnimFrame;

      If FOneShotRenderable <> Nil
       Then CurrentAnimSpeed := OneShotAnimSpeed
       Else CurrentAnimSpeed := AnimSpeed;

//      FAnimFrame := Frac(FAnimFrame + CurrentAnimSpeed * ElapsedTime);
//      If FAnimFrame < 0 Then FAnimFrame := FAnimFrame + 1;
      FAnimFrame := FAnimFrame + CurrentAnimSpeed * ElapsedTime;
      If ((FAnimFrame > 1) And (CurrentAnimSpeed > 0)) Or
         ((FAnimFrame < 0) And (CurrentAnimSpeed < 0)) Then
      Begin
        If FAnimFrame < 0 Then FAnimFrame := FAnimFrame + 1;
        FAnimFrame := Frac(FAnimFrame);
        If (FOneShotRenderable <> Nil) And FStopOnLastFrame Then
        Begin
          FAnimFrame       := 1;
//          AnimSpeed        := 0;
          OneShotAnimSpeed := 0;
          GetExtents;
          If CanChangeTreeNodes And (FBSPTreeNode <> Nil) Then FBSPTreeNode.CheckEntity(Self);
        End
        Else
        Begin
          If (FOneShotRenderable <> Nil) {And (FAnimFrame > FLastAnimFrame)} And (CurrentAnimSpeed < 0) And FRevertToPositive Then
          Begin
//            If AnimSpeed < 0 Then AnimSpeed := -AnimSpeed;
            FAnimFrame := 0;
            GetExtents;
            If CanChangeTreeNodes And (FBSPTreeNode <> Nil) Then FBSPTreeNode.CheckEntity(Self);
          End;
          FOneShotRenderable := Nil;
        End;
      End;
      FOldTime := Time;
    End;
  End;

  If FAnimFrame < 0 Then FAnimFrame := 0;

  If (FAnimFrame <> FLastAnimFrame) Or DiffPosition Or DiffRotation Or DiffScale Or Not FSetTransformMatrix Then
  Begin
    If DiffPosition Or DiffRotation Or DiffScale Or Not FSetTransformMatrix Then RebuildTransformMatrix;
    GetExtents;

    // If this entity are attached to a BSP tree node, ask the node to reevaluate this entity so
    // it can be placed in the smallest node possible

    If CanChangeTreeNodes And (FBSPTreeNode <> Nil) And (DiffPosition Or DiffRotation Or DiffScale) Then FBSPTreeNode.CheckEntity(Self);
//    FLastAnimFrame := FAnimFrame;
  End;
End; // TEntity.Update

Procedure TEntity.RebuildTransformMatrix;
Var
  CX,SX : Single;
  CY,SY : Single;
  CZ,SZ : Single;

Begin
  // Rebuild our transformation matrix

  CX := Cos(-FRotation.X * Pi / 180);
  SX := Sin(-FRotation.X * Pi / 180);
  CY := Cos(-FRotation.Y * Pi / 180);
  SY := Sin(-FRotation.Y * Pi / 180);
  CZ := Cos(-FRotation.Z * Pi / 180);
  SZ := Sin(-FRotation.Z * Pi / 180);
  FTransformMatrix.M[1,1] := FScale.X * (CY * CZ);
  FTransformMatrix.M[1,2] := FScale.X * (CY * SZ);
  FTransformMatrix.M[1,3] := FScale.X * (-SY);
  FTransformMatrix.M[1,4] := FPosition.X;
  FTransformMatrix.M[2,1] := FScale.Y * (SX * SY * CZ - CX * SZ);
  FTransformMatrix.M[2,2] := FScale.Y * (SX * SY * SZ + CX * CZ);
  FTransformMatrix.M[2,3] := FScale.Y * (SX * CY);
  FTransformMatrix.M[2,4] := FPosition.Y;
  FTransformMatrix.M[3,1] := FScale.Z * (CX * SY * CZ + SX * SZ);
  FTransformMatrix.M[3,2] := FScale.Z * (CX * SY * SZ - SX * CZ);
  FTransformMatrix.M[3,3] := FScale.Z * (CX * CY);
  FTransformMatrix.M[3,4] := FPosition.Z;
  FOrientation.From4x4Matrix(FTransformMatrix);
  FSetTransformMatrix := True;
End; // TEntity.RebuildTransformMatrix

Procedure TEntity.Redraw;
Var
  Step     : Integer;
//  Camera   : TCamera;
  R        : TRenderable;
  T        : Integer;

  Procedure TestPos;
  Var I: Integer;
  Begin
    I := Pos('(',Name);
    If I > 0 Then
    Begin
      Name := Trim(Copy(Name,1,I - 1)) + ' (' +
              Trim(Format('%8.2f',[FBox.MaxPt.X - FBox.MinPt.X])) + ',' +
              Trim(Format('%8.2f',[FBox.MaxPt.Y - FBox.MinPt.Y])) + ',' +
              Trim(Format('%8.2f',[FBox.MaxPt.Z - FBox.MinPt.Z])) + ')';
    End
    Else
    Begin
      Name := Name + ' (' +
              Trim(Format('%8.2f',[FBox.MaxPt.X - FBox.MinPt.X])) + ',' +
              Trim(Format('%8.2f',[FBox.MaxPt.Y - FBox.MinPt.Y])) + ',' +
              Trim(Format('%8.2f',[FBox.MaxPt.Z - FBox.MinPt.Z])) + ')';
    End;
  End; // TestPos

Begin
//  LogToFile('TEntity.Redraw(): Begin');

  If Visible Then
  Begin
//    LogToFile('TEntity.Redraw(): Test 1');

    // First get the TRenderable instance

    R := Nil;
//    Try
//      If Owner <> Nil Then TSceneGL(Owner).LockRenderable;
      If FOneShotRenderable <> Nil
       Then R := TRenderable(FOneShotRenderable)
       Else R := TRenderable(FRenderable);
//    Finally
//      If Owner <> Nil Then TSceneGL(Owner).UnlockRenderable;
//    End;

//    LogToFile('TEntity.Redraw(): Test 2');

    // Make sure we have a renderable

    If R <> Nil Then
    Begin
      // The frustum check was moved up to the BSP node level

{
      // Make sure the entity is visible to our camera

      VisibleInFrustum := False;
      If TSceneGL(Owner).Cameras.Count > 0 Then
      Begin
        // First check to make sure that the entity is visible within the far rendering radius

        Camera := TSceneGL(Owner).DefaultCamera;
        If Camera <> Nil Then
        Begin
          VisibleInFrustum := (Camera.Frustum.ContainsSphere(FSphereWork) <> scrOutside);// And
//                    Camera.Frustum.ContainsAxisAlignedBox(FBox);
        End;
      End
      Else VisibleInFrustum := True;

//      LogToFile('TEntity.Redraw(): Test 3');

      If VisibleInFrustum Then
      Begin
}


        T := GetTickCount;
        If (T < TSceneGL(Owner).RedrawTime) Or (T > TSceneGL(Owner).RedrawTime + 250) Then
        Begin
          TSceneGL(Owner).RedrawTime := T;
          Sleep(2);
        End;




        Redrawing := True;

        // Start at a known state

        glDisable(GL_TEXTURE_2D);
        Tex2D  := False;
        
        If TSceneGL(Owner).OcclusionManager.Enabled Then TSceneGL(Owner).OcclusionManager.LoadIdentity;

        Step := 0;
        glPushMatrix;
{
        FLastPosition.Copy(FPosition);
        FLastRotation.Copy(FRotation);
        FLastScale.Copy(FScale);
}
        FLastAnimFrame := FAnimFrame;



//        TestPos;

        

        glScalef(FLocalScale.X,FLocalScale.Y,FLocalScale.Z);

        glTranslatef(FLastPosition.X,FLastPosition.Y,FLastPosition.Z);   {translate, not the object but the coordinate system}
        glRotatef(FLastRotation.X, 1.0, 0.0, 0.0);   {same, rotate the coordinate system}
        glRotatef(FLastRotation.Y, 0.0, 1.0, 0.0);
        glRotatef(FLastRotation.Z, 0.0, 0.0, 1.0);

        glTranslatef(1.5 * FLocalPosition.X,1.5 * FLocalPosition.Y,1.5 * FLocalPosition.Z);   {translate, not the object but the coordinate system}

        glScalef(FLastScale.X,FLastScale.Y,FLastScale.Z);

        If TSceneGL(Owner).OcclusionManager.Enabled Then
        Begin
          TSceneGL(Owner).OcclusionManager.Scale(FLastScale);
          TSceneGL(Owner).OcclusionManager.XRotate(FLastRotation.X);
          TSceneGL(Owner).OcclusionManager.YRotate(FLastRotation.Y);
          TSceneGL(Owner).OcclusionManager.ZRotate(FLastRotation.Z);
          TSceneGL(Owner).OcclusionManager.Translate(FLastPosition);
        End;


        While Step < 2 Do
        Begin
          Case WireFrame Of
            0: glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
            1: glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
            2: glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
            3: If Step = 0 Then glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
               Else
               Begin
                 glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
                 glLineWidth(2);
               End;
          End; // Case
{
          If PutNames then        // If this is active then each entity has it?s own name
          Begin
            glLoadName(ID);
            glPassThrough(ID);    // it should be:  GLfloat(id), but it wouldn?t compile
          End;
}

          R.Redraw(FLastAnimFrame,VertexColors,AlphaNotOneOnly,ModelType,TextureSet,NearLight);

          If Assigned(FOnRedraw) Then FOnRedraw(Self);

          FHasAlpha := R.HasAlpha;
          If WireFrame <> 3 Then Step := 2 Else Inc(Step);
        End; // While


        glLineWidth(1);
        glPopMatrix;
        Redrawing := False;
//      End;
    End;
  End;

End; // TEntity.Redraw

Procedure TEntity.SetAnimFrame(NewFrame: Single);
Begin
//  Try
//    If Owner <> Nil Then TSceneGL(Owner).LockRenderable;
    FAnimFrame     := NewFrame;
    FLastAnimFrame := NewFrame;
    GetExtents;

    // If this entity are attached to a BSP tree node, ask the node to reevaluate this entity so
    // it can be placed in the smallest node possible

    If CanChangeTreeNodes And (FBSPTreeNode <> Nil) Then FBSPTreeNode.CheckEntity(Self);
//  Finally
//    If Owner <> Nil Then TSceneGL(Owner).UnlockRenderable;
//  End;
End; // TEntity.SetAnimFrame

Procedure TEntity.SetOneShotRenderable(R: TObject; StopOnLastFrame,RevertToPositive: Boolean);
Begin
//  Try
//    If Owner <> Nil Then TSceneGL(Owner).LockRenderable;
    If R <> FOneShotRenderable Then FAnimFrame := 0;
    FOneShotRenderable := R;
    FStopOnLastFrame   := StopOnLastFrame;
    FRevertToPositive  := RevertToPositive;
//  Finally
//    If Owner <> Nil Then TSceneGL(Owner).UnlockRenderable;
//  End;
End; // TEntity.SetOneShotRenderable

Procedure TEntity.GetDiapasones(Out X1,X2,Y1,Y2,Z1,Z2: Double);
Var  DBox : TAxisAlignedBox;
Begin
  GetExtents;

  DBox := TAxisAlignedBox.Create;
  DBox.Copy(FBox);
  DBox.CounterClockwiseRotate(FRotation,True,True,True,True);

  X1 := DBox.MinPt.X;
  Y1 := DBox.MinPt.Y;
  Z1 := DBox.MinPt.Z;
  X2 := DBox.MaxPt.X;
  Y2 := DBox.MaxPt.Y;
  Z2 := DBox.MaxPt.Z;

  DBox.Free;
End; // TEntity.GetDiapasones

Procedure TEntity.GetExtents;
Var
  I,J  : Integer;
  R    : TRenderable;
  SR   : TSkeletonRenderable;
  S    : Single;
  MO   : Array Of TModel;
  MI   : Array of Integer;

Begin
//  Try
//    If Owner <> Nil Then TSceneGL(Owner).LockRenderable;
    If FRenderable <> Nil Then
    Begin
      If FOneShotRenderable <> Nil
       Then R := TRenderable(FOneShotRenderable)
       Else R := TRenderable(FRenderable);
      If R Is TSkeletonRenderable Then
      Begin
        SR := TSkeletonRenderable(R);
        I := Trunc(AnimFrame * (SR.NumFrames - 1));
        If SR.RenderAllModels Then
        Begin
          // Ask the frame to figure out the box and sphere for itself

          If Not SR.Frame[I].FrameBoxOk Then
          Begin
            SetLength(MO,R.Count);
            SetLength(MI,R.Count);
            For J := 0 To R.Count - 1 Do
            Begin
              MO[J] := R.Models[J];
              MI[J] := J;
            End; // For J
            SR.Frame[I].SetupFrameBox(MO,MI,R.Scale);
            SetLength(MI,0);
            SetLength(MO,0);
          End;
          FBox.Copy(SR.Frame[I].FrameBox);
          FSphere.Copy(SR.Frame[I].FrameSphere);
        End
        Else
        Begin



          If R.Count > 1 Then
          Begin
            // Body and head

            If Not SR.Frame[I].FrameBoxOk Then SR.Frame[I].SetupFrameBox([R.Models[0],R.Models[1]],[0,1],R.Scale);
            FBox.Copy(SR.Frame[I].FrameBox);
            FSphere.Copy(SR.Frame[I].FrameSphere);
          End
          Else
          Begin
            // There is only one model

            If (I > High(SR.Frame[I].Box)) Or Not SR.Frame[I].BoxOk[0] Then SR.Frame[I].SetupModelBox(R.Models[0],0,R.Scale);
            FBox.Copy(SR.Frame[I].Box[0]);
            FSphere.Copy(R.Models[0].Sphere);
          End;
        End;

{
        If (I > High(SR.Frame[I].Box)) Or Not SR.Frame[I].BoxOk[0] Then SR.Frame[I].SetupModelBox(R.Models[0],0,R.Scale);
        FBox.Copy(SR.Frame[I].Box[0]);
        FSphere.Copy(R.Models[0].Sphere);
}
      End
      Else
      Begin
        I := Trunc(FAnimFrame * (R.Count - 1));
        FBox.Copy(R.Models[I].Box);
        FSphere.Copy(R.Models[I].Sphere);
      End;
    End
    Else
    Begin
      FBox.Setup(0,0,0,0,0,0);
      FSphere.Center.Copy(0,0,0);
      FSphere.Radius := 0;
    End;

    FEllipsoid.Radius.Copy((FBox.MaxPt.X - FBox.MinPt.X) / 2,(FBox.MaxPt.Y - FBox.MinPt.Y) / 2,(FBox.MaxPt.Z - FBox.MinPt.Z) / 2);
    FEllipsoid.Radius.Multiply(FScale);


//    FEllipsoid.Center.Copy(FPosition.X,FPosition.Y,FPosition.Z + FEllipsoid.Radius.Z);  // !!


    FSphereWork.Copy(FSphere);
    FRotationWork.Copy(FRotation);
    FRotationWork.Multiply(Pi / 180);

    S := Max(Abs(FScale.X),Max(Abs(FScale.Y),Abs(FScale.Z)));

    FSphereWork.Center.CounterClockwiseRotate(FRotationWork,True,True,True);
    FSphereWork.Center.Multiply(S);
    FSphereWork.Center.Add(FPosition);
    FSphereWork.Radius := S * FSphereWork.Radius;

    FSphereWork.Center.Add(FLocalPosition);
    FSphereWork.Center.Multiply(FLocalScale);
    FSphereWork.Radius := Max(Abs(FLocalScale.X),Max(Abs(FLocalScale.Y),Abs(FLocalScale.Z))) * FSphereWork.Radius;

    FSphere.Copy(FSphereWork);

    FZCenter := (FBox.MinPt.Z + FBox.MaxPt.Z) / 2;
    FMinZ0   := FBox.MinPt.Z;

    FTransformMatrix.Multiply(FBox.MinPt);
    FTransformMatrix.Multiply(FBox.MaxPt);
    FTransformMatrix.Multiply(FBox.Center);

//    FBox.CounterClockwiseRotate(FRotationWork,True,True,True,False); // Takes too long :(
//    FBox.Scale(FScale,False); // Just do this so we can get an object's size
//    FBox.Move(FPosition);

//  Finally
//    If Owner <> Nil Then TSceneGL(Owner).UnlockRenderable;
//  End;
End; // TEntity.GetExtents

{
Procedure TEntity.Center;
Var
  I,J            : Integer;
  Face           : TFace;
  X,Y,Z          : Single;
  MaxX,MaxY,MaxZ : Single;
  MinX,MinY,MinZ : Single;
  CX,CY,CZ       : Single;
  B              : Boolean;

Begin
  MaxX:=-100000000;  MaxY:=-100000000;  MaxZ:=-100000000;
  MinX:= 100000000;  MinY:= 100000000;  MinZ:= 100000000;

  // Obtain the farthest vertices

  For I := 0 To High(Faces) Do
  Begin
    Face := Faces[I];
    For J := 0 To High(Face.Vertices) Do
    Begin
      X := Face.Vertices[J].x;
      Y := Face.Vertices[J].Y;
      Z := Face.Vertices[J].Z;
      If X < MinX Then MinX := X;
      If X > MaxX Then MaxX := X;
      If Y < MinY Then MinY := Y;
      If Y > MaxY Then MaxY := Y;
      If Z < MinZ Then MinZ := Z;
      If Z > MaxZ Then MaxZ := Z;
    End; // For J
  End; // For I

  // Calculate the center coordinates

  CX := MinX + (MaxX - MinX) / 2;
  CY := MinY + (MaxY - MinY) / 2;
  CZ := MinZ + (MaxZ - MinZ) / 2;

  // Now move the vertices

  B       := Visible;
  Visible := False;
  While Redrawing Do Sleep(1);
  For I := 0 To High(Faces) Do
  Begin
    Face := Faces[I];
    For J := 0 To High(Face.Vertices) Do
    Begin
      Face.Vertices[J].X := Face.Vertices[J].X - CX;
      Face.Vertices[J].Y := Face.Vertices[J].Y - CY;
      Face.Vertices[J].Z := Face.Vertices[J].Z - CZ;
    End; // For J
  End; // For I
  Visible := B;
End; // TEntity.Center

Function TEntity.FindPoint(ix,iy,iz: Single): TVertex;
// Search for a point near to x,y,z
Var
  I,J   : Integer;
  Face  : TFace;
  Found : Boolean;

Begin
  Result := Nil;
  I      := 0;
  Found  := False;
  While (I <= High(Faces)) And Not Found Do
  Begin
    Face := Faces[I];
    J    := 0;
    While (J <= High(Face.Vertices)) And Not Found Do
    Begin
      If Sqr(Face.Vertices[J].X - ix) + Sqr(Face.Vertices[J].Y - iy) + Sqr(Face.Vertices[J].Z - iz) < MinimalDistance Then
      Begin
        Found := True;
        Result := Face.Vertices[J];
      End;
      Inc(J);
    End; // While
    Inc(I);
  End; // While
End; // TEntity.FindPoint
}
// ---------------------------
// T3DText
// ---------------------------

Constructor T3DText.Create(AOwner: TObject; DC: HDC; iListStart: Integer);
Begin
  Inherited Create(Owner);
  Scale.Copy(1,1,1);
  FontScale[1] := 1;
  FontScale[2] := 1;
  FontScale[3] := 1;

  // By default use the following font style

  FontStyle.lfHeight         := -10;
  FontStyle.lfWidth          := 0;
  FontStyle.lfEscapement     := 0;
  FontStyle.lfOrientation    := 0;
  FontStyle.lfWeight         := FW_BOLD;  // FW_NORMAL, FW_BOLD
  FontStyle.lfItalic         := 0;        // use 0 for false, and 1 for true
  FontStyle.lfUnderline      := 0;        // use 0 for false, and 1 for true
  FontStyle.lfStrikeOut      := 0;        // use 0 for false, and 1 for true
  FontStyle.lfCharSet        := ANSI_CHARSET;
  FontStyle.lfOutPrecision   := OUT_DEFAULT_PRECIS;
  FontStyle.lfClipPrecision  := CLIP_DEFAULT_PRECIS;
  FontStyle.lfQuality        := DEFAULT_QUALITY;
  FontStyle.lfPitchAndFamily := DEFAULT_PITCH;
  StrCopy(FontStyle.lfFaceName, 'Arial');
  ListStart                  := iListStart;
  ExtrudeDepth               := DefaultExtrudeDepth;      // By default
  MyDC                       := DC;
  SetText('OpenGL');
  ResetFont;                              // Fill the list
End; // T3DText.Create

Constructor T3DText.Create(AOwner: TObject; OtherText: T3DText);
Begin
  Inherited Create(Owner);
  Scale.Copy(1,1,1);
  FontScale[1] := OtherText.FontScale[1];
  FontScale[2] := OtherText.FontScale[2];
  FontScale[3] := OtherText.FontScale[3];

  // By default use the following font style

  FontStyle.lfHeight         := -10;
  FontStyle.lfWidth          := 0;
  FontStyle.lfEscapement     := 0;
  FontStyle.lfOrientation    := 0;
  FontStyle.lfWeight         := FW_BOLD;  // FW_NORMAL, FW_BOLD
  FontStyle.lfItalic         := 0;        // Use 0 for false, and 1 for true
  FontStyle.lfUnderline      := 0;        // Use 0 for false, and 1 for true
  FontStyle.lfStrikeOut      := 0;        // Use 0 for false, and 1 for true
  FontStyle.lfCharSet        := ANSI_CHARSET;
  FontStyle.lfOutPrecision   := OUT_DEFAULT_PRECIS;
  FontStyle.lfClipPrecision  := CLIP_DEFAULT_PRECIS;
  FontStyle.lfQuality        := DEFAULT_QUALITY;
  FontStyle.lfPitchAndFamily := DEFAULT_PITCH;
  StrCopy(FontStyle.lfFaceName, 'Arial');
  ListStart                  := OtherText.ListStart;
  ExtrudeDepth               := DefaultExtrudeDepth;      // By default
  MyDC                       := OtherText.MyDC;
  SetText(OtherText.Text);

  R := OtherText.R;
  G := OtherText.G;
  B := OtherText.B;
End; // T3DText.Create

Destructor T3DText.Destroy;
Begin
  Visible := False;
  While Redrawing Do Sleep(1);
  GlDeleteLists(ListStart, 256);  {delete the display lists}
  Inherited;
End; // T3DText.Destroy

Procedure T3DText.ResetFont;
Var
  PGMF   : PGlyphMetricsFloat;
  GMF    : Array [0..255] Of TGlyphMetricsFloat;
  MyFont : HFont;
  B      : Boolean;

Begin
  B := Visible;
  Visible := False;
  While Redrawing Do Sleep(1);
  GlDeleteLists(ListStart, 256);  {delete the display lists, if they exist}
  PGMF   := @GMF;
  MyFont := CreateFontIndirect(FontStyle);
  SelectObject(MyDC, MyFont);
  wglUseFontOutlines(MyDC, 0, 255, ListStart, 0.0, ExtrudeDepth, WGL_FONT_POLYGONS, PGMF);
  DeleteObject(MyFont);
  Visible := B;
End; // T3DText.ResetFont

Procedure T3DText.Redraw(AlphaNotOneOnly: Boolean);
Begin
  // This part was copied and pasted from TEntity.redraw

  If Visible Then
  Begin
    Redrawing := True;
    glPushMatrix;
    Case WireFrame of
      0: glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
      1: glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      2: glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    End; // Case

    glScalef(Scale.X,Scale.Y,Scale.Z);
    glRotatef(Rotation.X, 1.0, 0.0, 0.0);               // same, rotate the coordinate system
    glRotatef(Rotation.Y, 0.0, 1.0, 0.0);
    glRotatef(Rotation.Z, 0.0, 0.0, 1.0);
    glTranslatef(Position.X,Position.Y,Position.Z);   // translate, not the object but the coordinate system
    glRotatef(LocalRotation[1], 1.0, 0.0, 0.0);          // same, rotate the coordinate system
    glRotatef(LocalRotation[2], 0.0, 1.0, 0.0);
    glRotatef(LocalRotation[3], 0.0, 0.0, 1.0);
    glScalef(FontScale[1],FontScale[2],FontScale[3]);

//    If Textures.Count > 0 Then TTexture(Textures[0]).Redraw;
{
    If PutNames then       // If this is active then each entity has it?s own name
    Begin
      GlLoadName(ID);
      GLPassThrough(ID);   // it should be:  GLfloat(id), but it wouldn?t compile
    End;
}
    // This is the text part

    glColor4ub(R,G,B,A);   // Set the color
    glListBase(ListStart);
    glCallLists(TextLength, GL_UNSIGNED_BYTE, @(Text[0]));
    glListBase(0);

    // Until here

    GlPopMatrix;
    Redrawing := False;
  End;
End; // T3DText.Redraw

Procedure T3DText.SetText(iText: String);
Var B: Boolean;
Begin
  B := Visible;
  Visible := False;
  While Redrawing Do Sleep(1);
  iText := Copy(iText,1,255); // Failsafe
  StrPCopy(Text, iText);
  TextLength := Length(iText);
  Visible := B;
End; // T3DText.SetText

Procedure T3DText.SetColor(iR,iG,iB,iA: Byte);
Var V: Boolean;
Begin
  V       := Visible;
  Visible := False;
  While Redrawing Do Sleep(1);
  R := iR;
  G := iG;
  B := iB;
  A := iA;
  Visible := V;
//  ResetFont;
End; // T3DText.SetColor

Procedure T3DText.LocalRotate(RX,RY,RZ: Single);
Begin
  LocalRotation[1] := RX;
  LocalRotation[2] := RY;
  LocalRotation[3] := RZ;
End; // T3DText.LocalRotate
(*
// ---------------------------
// TUserInterface
// ---------------------------

Constructor TUserInterface.Create(AOwner: TObject);
Begin
  Inherited Create(AOwner);
  FPanels := TList.Create;
  SetLength(FZOrder,0);
  SetLength(FZSort,0);
End; // TUserInterface.Create

Destructor TUserInterface.Destroy;
Begin
  Visible := False;
  While Redrawing Do Sleep(1);
  RemoveAllPanels;
  FPanels.Free;
  SetLength(FZOrder,0);
  SetLength(FZSort,0);
  Inherited;
End; // TUserInterface.Destroy

Procedure TUserInterface.Redraw(AlphaNotOneOnly: Boolean);
Var I: Integer;
Begin
  If Visible Then
  Begin
    Redrawing := True;
    For I := 0 To High(FZOrder) Do FZSort[FZOrder[I]] := I;
    For I := High(FZSort) DownTo 0 Do TUIPanel(FPanels.Items[FZSort[I]]).Redraw;
    Redrawing := False;
  End;
End; // TUserInterface.Redraw

Function TUserInterface.AddPanel: Integer;
Var V: Boolean;
Begin
  V       := Visible;
  Visible := False;
  While Redrawing Do Sleep(1);
  FPanels.Add(TUIPanel.Create(Self));
  Result  := FPanels.Count;
  SetLength(FZOrder,FPanels.Count);
  SetLength(FZSort,FPanels.Count);
  FZOrder[High(FZOrder)] := High(FZOrder);
  Visible := V;
End; // TUserInterface.AddPanel

Procedure TUserInterface.RemovePanel(Index: Integer);
Var
  V   : Boolean;
  I,J : Integer;

Begin
  V       := Visible;
  Visible := False;
  While Redrawing Do Sleep(1);
  If (Index >= 0) And (Index < FPanels.Count) Then
  Begin
    TUIPanel(FPanels.Items[Index]).Free;
    FPanels.Delete(Index);
    J := FZOrder[Index];
    For I := Index To High(FZOrder) - 1 Do FZOrder[I] := FZOrder[I + 1];
    SetLength(FZOrder,High(FZOrder));
    SetLength(FZSort,High(FZSort));
    For I := 0 To High(FZOrder) Do If FZOrder[I] > J Then Dec(FZOrder[I]);
  End;
  Visible := V;
End; // TUserInterface.RemovePanel

Procedure TUserInterface.RemoveAllPanels;
Var
  V : Boolean;
  I : Integer;

Begin
  V       := Visible;
  Visible := False;
  While Redrawing Do Sleep(1);
  For I := 0 To FPanels.Count - 1 Do TUIPanel(FPanels.Items[I]).Free;
  FPanels.Clear;
  SetLength(FZOrder,0);
  SetLength(FZSort,0);
  Visible := V;
End; // TUserInterface.RemoveAllPanels

Function TUserInterface.GetPanel(Index: Integer): TUIPanel;
Begin
  // Not thread-safe

  If (Index >= 0) And (Index < FPanels.Count)
   Then Result := TUIPanel(FPanels.Items[Index])
   Else Result := Nil;
End; // TUserInterface.GetPanel

Function TUserInterface.GetNumPanels: Integer;
Begin
  Result := FPanels.Count;
End; // TUserInterface.GetNumPanels

Procedure TUserInterface.MoveToFront(Index: Integer);
Var
  V : Boolean;
  I : Integer;

Begin
  If (Index >= 0) And (Index <= High(FZOrder)) And (FZOrder[Index] <> 0) Then
  Begin
    V       := Visible;
    Visible := False;
    While Redrawing Do Sleep(1);
    For I := 0 To High(FZOrder) Do If FZOrder[I] < FZOrder[Index] Then Inc(FZOrder[I]);
    FZOrder[Index] := 0;
    Visible := V;
  End;
End; // TUserInterface.MoveToFront

// ---------------------------
// TUIPanel
// ---------------------------

Constructor TUIPanel.Create(Owner: TEntity);
Begin
  FOwner              := Owner;
  FLeft               := 10;
  FTop                := 10;
  FWidth              := 10;
  FHeight             := 10;
  FBuffer             := TRaster32.Create(FWidth,FHeight);
  R                   := 255;
  G                   := 255;
  B                   := 255;
  A                   := 128;
  FAlpha              := 0.5;
  FVisible            := True;
  FUpdating           := False;
End; // TUIPanel.Create

Destructor TUIPanel.Destroy;
Begin
  FUpdating := True;
  While FOwner.Redrawing Do Sleep(1);
  FBuffer.Free;
End; // TUIPanel.Destroy

Procedure TUIPanel.Redraw;
Begin
  If FVisible And Not FUpdating Then
  Begin
    glDisable(GL_TEXTURE_2D);
    Tex2D := False;
    glWindowPos2fMESAemulate(FLeft,TSceneGL(FOwner.Owner).WindowHeight - FTop - FHeight);
    glColor4b(ShortInt(R),ShortInt(G),ShortInt(B),ShortInt(A));
    glDrawPixels(FWidth,FHeight,GL_RGBA,GL_UNSIGNED_BYTE,FBuffer.Data);
  End;
End; // TUIPanel.Redraw

Procedure TUIPanel.Update(BMP: TJCLBitmap32);
Begin
  FUpdating := True;
  While FOwner.Redrawing Do Sleep(1);
  FWidth  := BMP.Width;
  FHeight := BMP.Height;
  If (FBuffer.Width <> FWidth) Or (FBuffer.Height <> FHeight) Then
  Begin
    FBuffer.Free;
    FBuffer := TRaster32.Create(BMP);
  End;
  If A = 0
   Then FBuffer.CopyFrom(BMP,0,A)
   Else FBuffer.CopyFrom(BMP,A);
  FUpdating := False;
End; // TUIPanel.Update

Procedure TUIPanel.SetRect(ALeft,ATop,AWidth,AHeight: Integer);
Begin
  FUpdating := True;
  While FOwner.Redrawing Do Sleep(1);
  FLeft     := ALeft;
  FTop      := ATop;
  FWidth    := AWidth;
  FHeight   := AHeight;
  FUpdating := False;
End; // TUIPanel.SetRect

Procedure TUIPanel.SetColor(iR,iG,iB,iA: Byte);
Begin
  R := iR;
  G := iG;
  B := iB;
  A := iA;
End; // TUIPanel.SetColor

Procedure TUIPanel.SetAlpha(S: Single);
Begin
  If S < 0 Then S := 0;
  If S > 1 Then S := 1;
  FAlpha := S;
  A := Round(FAlpha * 255);
End; // TUIPanel.SetAlpha

Procedure TUIPanel.SetVisible(B: Boolean);
Begin
  FVisible := B;
End; // TUIPanel.SetVisible
*)
// ---------------------------
// TTexture
// ---------------------------

Constructor TTexture.Create(AScene: TObject; iOwner: TEntity);
Begin
  Scene              := aScene;
  Owner              := iOwner;
//  MyList:=glgenlists(1);  {generate an empty list for this texture}
  WrapSmode          := gl_repeat;   {tile the texture in X}
  WrapTmode          := gl_repeat;   {tile the texture in Z}

  MinFilter := GL_LINEAR;
  MagFilter := GL_LINEAR;

{
  MagFilter          := gl_nearest;  // texture using the nearest texel
  MinFilter          := gl_nearest;  // texture using the nearest texel
}
  Automatic          := False;       {by default the texture coordinates have to be calculated by hand}
  AutoGenModeX       := gl_object_linear;
  AutoGenModeZ       := gl_object_linear;
  AutoXMult[0]       := 1;   AutoXMult[1] := 0;
  AutoXMult[2]       := 0;   AutoXMult[3] := 0;
  AutoZMult[0]       := 0;   AutoZMult[1] := 0;
  AutoZMult[2]       := 1;   AutoZMult[3] := 0;
  EnvironmentMode    := GL_decal;    {like the decals in a racing car}
  EnvBlendColor[0]   := 0;
  EnvBlendColor[1]   := 0;
  EnvBlendColor[2]   := 0;
  EnvBlendColor[3]   := 255; {alpha is by default 1}
  Buffer             := Nil;
  OBuffer            := Nil;
  ImgSize            := 0;
  OImgSize           := 0;
  Buffer3            := Nil;
  OBuffer3           := Nil;
  Size               := 0;
  OSize              := 0;
  NeedsMask          := False;
  Loaded             := False;
  Index              := 0;
  BMPInfo.ImageSize  := 0;
  OBMPInfo.ImageSize := 0;
  ID                 := -1;
  HasAlpha           := False;
  Opacity            := '';
End; // TTexture.Create

Destructor TTexture.Destroy;
Begin
  If Loaded Then glDeleteTextures(1,@ID);
  If Buffer   <> Nil Then FreeMem(Buffer,ImgSize);
  If Buffer3  <> Nil Then FreeMem(Buffer3,Size);
  If OBuffer  <> Nil Then FreeMem(OBuffer,OImgSize);
  If OBuffer3 <> Nil Then FreeMem(OBuffer3,OSize);
//  GLdeleteLists(MyList,1);  {destroy the display list of the texture}
  Inherited;
End; // TTexture.Destroy

Procedure TTexture.FlipAlpha;
Var
  I : Integer;
  P : ^LongWord;

Begin
  If Buffer <> Nil Then
  Begin
    P := Buffer;
    For I := 0 To (BMPInfo.ImageSize Div 4) - 1 Do
    Begin
      TRGBA(P^).A := 255 - TRGBA(P^).A;
      Inc(LongWord(P),4);
    End; // For I
  End;
End; // TTexture.FlipAlpha

Procedure TTexture.SetAlpha(Color: LongWord; Alpha: Byte);
Var
  I : Integer;
  P : ^LongWord;

Begin
  If Buffer <> Nil Then
  Begin
    P     := Buffer;
    Color := Color And $00FFFFFF;
    For I := 0 To (BMPInfo.ImageSize Div 4) - 1 Do
    Begin
      If (P^ And $00FFFFFF) = Color Then TRGBA(P^).A := Alpha;
      Inc(LongWord(P),4);
    End; // For I
  End;
End; // TTexture.SetAlpha

Function TTexture.LoadTextureFromBitmap(Bitmap: TBitmap): Integer;

  Procedure LoadTexFromBMP(BMP: TBitmap);
  Var
    H    : Integer;
    Line : ^LongWord;

  Begin
    If BMPInfo.ImageSize <> BMP.Width * BMP.Height * 4 Then
    Begin
      If Buffer <> Nil Then FreeMem(Buffer, BMPInfo.ImageSize);
      BMPInfo.Width     := BMP.Width;
      BMPInfo.Height    := BMP.Height;
      BMPInfo.ImageSize := BMP.Width * BMP.Height * 4;
      BMPInfo.ColorBits := 32;
      GetMem(Buffer,BMPInfo.ImageSize);
    End;
    If OBuffer <> Nil Then FreeMem(OBuffer,OBMPInfo.ImageSize);
    ImgSize := BMPInfo.ImageSize;
    Line    := Buffer;
    Inc(LongWord(Line),BMP.Width * (BMP.Height - 1) * 4);
    For H := 0 To BMP.Height - 1 Do
    Begin
      Move(BMP.ScanLine[H]^,Line^,BMP.Width * 4);
      Dec(LongWord(Line),BMP.Width * 4);
    End; // For H
    OBuffer := Nil;
  End; // LoadTexFromBMP

Begin
  ExchangeRB := False;
  LoadTexFromBMP(Bitmap);
//  TSceneGL(Scene).LoadTexture(Self);
  Result := 0;
End; // TTexture.LoadTextureFromBitmap

Function TTexture.LoadTextureFromRaster32(R32: TRaster32): Integer;

  Procedure LoadTexFromR32(R32: TRaster32);
  Var
    H    : Integer;
    Line : ^LongWord;
    P    : Pointer;

  Begin
    If BMPInfo.ImageSize <> R32.Width * R32.Height * 4 Then
    Begin
      If Buffer <> Nil Then FreeMem(Buffer, BMPInfo.ImageSize);
      BMPInfo.Width     := R32.Width;
      BMPInfo.Height    := R32.Height;
      BMPInfo.ImageSize := R32.Width * R32.Height * 4;
      BMPInfo.ColorBits := 32;
      GetMem(Buffer,BMPInfo.ImageSize);
    End;
    If OBuffer <> Nil Then FreeMem(OBuffer,OBMPInfo.ImageSize);
    ImgSize := BMPInfo.ImageSize;
    Line    := Buffer;
    Inc(LongWord(Line),R32.Width * (R32.Height - 1) * 4);
    P := R32.Data;
    For H := 0 To R32.Height - 1 Do
    Begin
      Move(P^,Line^,R32.Width * 4);
      Dec(LongWord(Line),R32.Width * 4);
      Inc(LongWord(P),R32.Width * 4);
    End; // For H
    OBuffer := Nil;
  End; // LoadTexFromBMP

Begin
  ExchangeRB := False;
  LoadTexFromR32(R32);
//  TSceneGL(Scene).LoadTexture(Self);
  Result := 0;
End; // TTexture.LoadTextureFromRaster32

Function TTexture.LoadTexture(TexMap,OpacMap: String; FlipR32: Boolean): Integer;
Const MB = 19778;
Var
  F      : File;
  JPG    : TJPEGImage;
  BMP    : TBitmap;
  BMP1   : TBitmap;
  R32    : TRaster32;

  Procedure LoadTexFromBMP(BMP: TBitmap);
  Var
    H    : Integer;
    Line : ^LongWord;

  Begin
    If Buffer <> Nil Then FreeMem(Buffer, BMPInfo.ImageSize);
    BMPInfo.Width     := BMP.Width;
    BMPInfo.Height    := BMP.Height;
    BMPInfo.ImageSize := BMP.Width * BMP.Height * 4;
    BMPInfo.ColorBits := 32;
    GetMem(Buffer,BMPInfo.ImageSize);
    ImgSize := BMPInfo.ImageSize;
    Line    := Buffer;
    Inc(LongWord(Line),BMP.Width * (BMP.Height - 1) * 4);
    For H := 0 To BMP.Height - 1 Do
    Begin
      Move(BMP.ScanLine[H]^,Line^,BMP.Width * 4);
      Dec(LongWord(Line),BMP.Width * 4);
    End; // For H
  End; // LoadTexFromBMP

  Procedure LoadOpacityFromBMP(BMP: TBitmap);
  Var
    H    : Integer;
    Line : ^LongWord;

  Begin
    If OBuffer <> Nil Then FreeMem(OBuffer, OBMPInfo.ImageSize);
    OBMPInfo.Width     := BMP.Width;
    OBMPInfo.Height    := BMP.Height;
    OBMPInfo.ImageSize := BMP.Width * BMP.Height * 4;
    OBMPInfo.ColorBits := 32;
    GetMem(OBuffer,OBMPInfo.ImageSize);
    OImgSize := OBMPInfo.ImageSize;
    Line     := OBuffer;
    Inc(LongWord(Line),BMP.Width * (BMP.Height - 1) * 4);
    For H := 0 To BMP.Height - 1 Do
    Begin
      Move(BMP.ScanLine[H]^,Line^,BMP.Width * 4);
      Dec(LongWord(Line),BMP.Width * 4);
    End; // For H
  End; // LoadOpacityFromBMP

  Procedure LoadTexFromR32(R32: TRaster32);
  Var
    H    : Integer;
    Src  : ^LongWord;
    Dest : ^LongWord;

  Begin
    If Buffer <> Nil Then FreeMem(Buffer, BMPInfo.ImageSize);
    BMPInfo.Width     := R32.Width;
    BMPInfo.Height    := R32.Height;
    BMPInfo.ImageSize := R32.Width * R32.Height * 4;
    BMPInfo.ColorBits := 32;
    GetMem(Buffer,BMPInfo.ImageSize);
    ImgSize := BMPInfo.ImageSize;
    If FlipR32 Then
    Begin
      Src  := R32.Data;
      Dest := Buffer;
      Inc(LongWord(Dest),R32.Width * (R32.Height - 1) * 4);
      For H := 0 To R32.Height - 1 Do
      Begin
        Move(Src^,Dest^,R32.Width * 4);
        Inc(LongWord(Src),R32.Width * 4);
        Dec(LongWord(Dest),R32.Width * 4);
      End; // For H
    End
    Else Move(R32.Data^,Buffer^,ImgSize);
  End; // LoadTexFromR32

  Procedure LoadOpacityFromR32(R32: TRaster32);
  Var
    H    : Integer;
    Src  : ^LongWord;
    Dest : ^LongWord;

  Begin
    If OBuffer <> Nil Then FreeMem(OBuffer, OBMPInfo.ImageSize);
    OBMPInfo.Width     := R32.Width;
    OBMPInfo.Height    := R32.Height;
    OBMPInfo.ImageSize := R32.Width * R32.Height * 4;
    OBMPInfo.ColorBits := 32;
    GetMem(OBuffer,OBMPInfo.ImageSize);
    OImgSize := OBMPInfo.ImageSize;
    If FlipR32 Then
    Begin
      Src  := R32.Data;
      Dest := OBuffer;
      Inc(LongWord(Dest),R32.Width * (R32.Height - 1) * 4);
      For H := 0 To R32.Height - 1 Do
      Begin
        Move(Src^,Dest^,R32.Width * 4);
        Inc(LongWord(Src),R32.Width * 4);
        Dec(LongWord(Dest),R32.Width * 4);
      End; // For H
    End
    Else Move(R32.Data^,OBuffer^,ImgSize);
  End; // LoadOpacityFromR32

  Procedure FlipAlpha(Buffer: Pointer);
  Var
    Line : ^LongWord;
    H    : Integer;
  Begin
    Line := Buffer;
    For H := 0 To BMPInfo.Width * BMPInfo.Height - 1 Do
    Begin
      TRGBA(Line^).A := 255 - TRGBA(Line^).A;
      Inc(LongWord(Line),4);
    End; // For H
  End; // FlipAlpha

Begin
//  TexFileName := TexMap;


  

  If Not FileExists(TexMap) Then
  Begin
    TexMap := Copy(TexMap,1,Length(TexMap) - Length(ExtractFileExt(TexMap))) + '.jpg';
  End;

  If Not FileExists(TexMap) Then
  Begin
    TexMap := Copy(TexMap,1,Length(TexMap) - Length(ExtractFileExt(TexMap))) + '.tga';
  End;

  If Not FileExists(OpacMap) Then
  Begin
    OpacMap := Copy(OpacMap,1,Length(OpacMap) - Length(ExtractFileExt(OpacMap))) + '.jpg';
  End;

  If Not FileExists(OpacMap) Then
  Begin
    OpacMap := Copy(OpacMap,1,Length(OpacMap) - Length(ExtractFileExt(OpacMap))) + '.tga';
  End;

  If Not FileExists(TexMap) Then
  Begin
    Result := -1;  {file not found}
    Exit;
  End;

  If Assigned(LoadTexProc) Then
  Begin
    ExchangeRB := False;

    BMP := LoadTexProc(TexMap);
    LoadTexFromBMP(BMP);
    BMP.free;

    If (OpacMap <> '') And FileExists(OpacMap) Then
    Begin
      BMP := LoadTexProc(OpacMap);
      LoadOpacityFromBMP(BMP);
      BMP.Free;
    End;
  End
  Else If Assigned(LoadTexProcR32) Then
  Begin
    R32 := LoadTexProcR32(TexMap);
    LoadTexFromR32(R32);
    R32.free;

    If (OpacMap <> '') And FileExists(OpacMap) Then
    Begin
      R32 := LoadTexProcR32(OpacMap);
      LoadOpacityFromR32(R32);
      R32.Free;
    End;
  End
  Else
  Begin
    ExchangeRB := True;
    If UpperCase(ExtractFileExt(TexMap)) = '.JPG' Then
    Begin
      JPG := TJPEGImage.Create;
      JPG.LoadFromFile(TexMap);

      // Create Bitmap

      BMP             := TBitmap.Create;
      BMP.Pixelformat := pf32bit;
      BMP.Width       := JPG.Width;
      BMP.Height      := JPG.Height;
      BMP.Canvas.Draw(0,0,JPG);        // Copy the JPEG onto the Bitmap

      LoadTexFromBMP(BMP);
      FlipAlpha(Buffer);

      BMP.free;
      JPG.free;
    End
    Else If UpperCase(ExtractFileExt(TexMap)) = '.TGA' Then
    Begin
      BMP1 := TBitmap.Create;
      LoadFromFileX(TexMap,BMP1);

      // Create Bitmap

      BMP             := TBitmap.Create;
      BMP.Pixelformat := pf32bit;
      BMP.Width       := BMP1.Width;
      BMP.Height      := BMP1.Height;
      BMP.Canvas.Draw(0,0,BMP1);        // Copy the Targa onto the Bitmap

      LoadTexFromBMP(BMP);
      FlipAlpha(Buffer);

      BMP.Free;
      BMP1.Free;
    End
    Else
    Begin
      // Load the texture

      AssignFile(F,TexMap);
      Reset(F,1);
      BlockRead(F,Header,SizeOf(Header));
      BlockRead(F,BMPinfo,SizeOf(BMPInfo));

      // Make sure the texture is a Windows .BMP file

      If Header.FileType <> MB Then
      Begin
        CloseFile(F);
        Result := -2;   {file type is not BMP}
        Exit;
      End;

      If Buffer  <> Nil Then FreeMem(Buffer,ImgSize);
      If OBuffer <> Nil Then FreeMem(OBuffer,OImgSize);

      Buffer  := Nil;
      OBuffer := Nil;

      If BMPInfo.ImageSize = 0 Then
      Begin
        Case BMPInfo.ColorBits Of
           8: BMPInfo.ImageSize := 1 * BMPinfo.Width * BMPInfo.Height;
          16: BMPInfo.ImageSize := 2 * BMPinfo.Width * BMPInfo.Height;
          24: BMPInfo.ImageSize := 3 * BMPinfo.Width * BMPInfo.Height;
          32: BMPInfo.ImageSize := 4 * BMPinfo.Width * BMPInfo.Height;
        End; // Case
      End;

      GetMem(Buffer,BMPInfo.ImageSize);
      ImgSize := BMPInfo.ImageSize;
      Seek(F,Header.Offset);
      BlockRead(F,Buffer^,BMPInfo.ImageSize);
      CloseFile(F);
    End;

    // Now see if we can load the opacity map

    If (OpacMap <> '') And FileExists(OpacMap) Then
    Begin
      If UpperCase(ExtractFileExt(OpacMap)) = '.JPG' Then
      Begin
        JPG := TJPEGImage.Create;
        JPG.LoadFromFile(OpacMap);

        // Create Bitmap

        BMP             := TBitmap.Create;
        BMP.Pixelformat := pf32bit;
        BMP.Width       := JPG.Width;
        BMP.Height      := JPG.Height;
        BMP.Canvas.Draw(0,0,JPG);        // Copy the JPEG onto the Bitmap

        LoadOpacityFromBMP(BMP);
        FlipAlpha(OBuffer);

        BMP.free;
        JPG.free;
      End
      Else If UpperCase(ExtractFileExt(OpacMap)) = '.TGA' Then
      Begin
        BMP1 := TBitmap.Create;
        LoadFromFileX(TexMap,BMP1);

        // Create Bitmap

        BMP             := TBitmap.Create;
        BMP.Pixelformat := pf32bit;
        BMP.Width       := BMP1.Width;
        BMP.Height      := BMP1.Height;
        BMP.Canvas.Draw(0,0,BMP1);        // Copy the Targa onto the Bitmap

        LoadOpacityFromBMP(BMP);
        FlipAlpha(OBuffer);

        BMP.Free;
        BMP1.Free;
      End
      Else
      Begin
        AssignFile(F,OpacMap);
        Reset(F,1);
        BlockRead(F,OHeader,SizeOf(OHeader));
        BlockRead(F,OBMPinfo,SizeOf(OBMPInfo));
        If OHeader.FileType = MB Then
        Begin
          If OBMPInfo.ImageSize = 0 Then
          Begin
            Case OBMPInfo.ColorBits Of
               8: OBMPInfo.ImageSize := 1 * OBMPinfo.Width * OBMPInfo.Height;
              16: OBMPInfo.ImageSize := 2 * OBMPinfo.Width * OBMPInfo.Height;
              24: OBMPInfo.ImageSize := 3 * OBMPinfo.Width * OBMPInfo.Height;
              32: OBMPInfo.ImageSize := 4 * OBMPinfo.Width * OBMPInfo.Height;
            End; // Case
          End;
          GetMem(OBuffer,OBMPInfo.ImageSize);
          OImgSize := OBMPInfo.ImageSize;
          Seek(F,OHeader.Offset);
          BlockRead(F,OBuffer^,OBMPInfo.ImageSize);
        End;
        CloseFile(F);
      End;
    End;
  End;
//  TSceneGL(Scene).LoadTexture(Self);
  Result := 0;
End;

Procedure TTexture.LoadTextureIntoOpenGL;
Var
  I,J       : Integer;

  Buffer2b  : PTyBuff;
  Buffer3b  : PTyBuffA;
  Buffer4b  : PTyBuffA;

  OBuffer2b : PTyBuff;
  OBuffer3b : PTyBuffA;
  OBuffer4b : PTyBuffA;

  AvgR      : LongWord;
  AvgG      : LongWord;
  AvgB      : LongWord;
  TotR      : LongWord;
  TotG      : LongWord;
  TotB      : LongWord;
  R,G,B     : LongWord;
  NumColors : Integer;

  NewSize   : Integer;
  NewOSize  : Integer;

Begin
  If ID < 0 Then
  Begin
    glGenTextures(1,@ID);
    Size  := 0;
    OSize := 0;
  End
  Else
  Begin
    // Reloading the texture or loading a different one

    glDeleteTextures(1,@ID);
    glGenTextures(1,@ID);
  End;

  If BMPInfo.ColorBits = 24
   Then NewSize := BMPInfo.ImageSize * 4 Div 3
   Else NewSize := BMPInfo.ImageSize;
  If Size <> NewSize Then
  Begin
    If Buffer3  <> Nil Then FreeMem(Buffer3,Size);
    GetMem(Buffer3,NewSize);
    Size := NewSize;
  End;
  Buffer2b  := PTyBuff(Buffer);
  Buffer3b  := PTyBuffA(Buffer3);
  OBuffer3b := Nil;

  If OBuffer <> Nil Then
  Begin
    If OBMPInfo.ColorBits = 24
     Then NewOSize := OBMPInfo.ImageSize * 4 Div 3
     Else NewOSize := OBMPInfo.ImageSize;
    If OSize <> NewOSize Then
    Begin
      If OBuffer3 <> Nil Then FreeMem(OBuffer3,OSize);
      GetMem(OBuffer3,NewOSize);
      OSize := NewOSize;
    End;
    OBuffer2b := PTyBuff(OBuffer);
    OBuffer3b := PTyBuffA(OBuffer3);

    If OBMPInfo.ColorBits = 24 Then
    Begin
      For I := 1 To OBMPInfo.ImageSize Div 3 Do
      Begin
        If ExchangeRB Then
        Begin
          OBuffer3b^[I].R := OBuffer2b^[I].B;
          OBuffer3b^[I].G := OBuffer2b^[I].G;
          OBuffer3b^[I].B := OBuffer2b^[I].R;
        End
        Else
        Begin
          OBuffer3b^[I].R := OBuffer2b^[I].R;
          OBuffer3b^[I].G := OBuffer2b^[I].G;
          OBuffer3b^[I].B := OBuffer2b^[I].B;
        End;
        OBuffer3b^[I].A := EnvBlendColor[3];   {obtain blend alpha from envblendcolor.alpha}
      End; //  For I
    End
    Else
    Begin
      OBuffer4b := PTyBuffA(OBuffer);
      For I := 1 To OBMPInfo.ImageSize Div 4 Do
      Begin
        If ExchangeRB Then
        Begin
          OBuffer3b^[I].R := OBuffer4b^[I].B;
          OBuffer3b^[I].G := OBuffer4b^[I].G;
          OBuffer3b^[I].B := OBuffer4b^[I].R;
          OBuffer3b^[I].A := OBuffer4b^[I].A;
        End
        Else OBuffer3b^[I] := OBuffer4b^[I];
      End; //  For I
    End;
  End;

  // Now do the texture, exchanging red and blue

  NumColors := 0;
  If BMPInfo.ColorBits = 24 Then
  Begin
    For I := 1 To BMPInfo.ImageSize Div 3 Do
    Begin
      If ExchangeRB Then
      Begin
        Buffer3b^[I].R := Buffer2b^[I].B;
        Buffer3b^[I].G := Buffer2b^[I].G;
        Buffer3b^[I].B := Buffer2b^[I].R;
      End
      Else
      Begin
        Buffer3b^[I].R := Buffer2b^[I].R;
        Buffer3b^[I].G := Buffer2b^[I].G;
        Buffer3b^[I].B := Buffer2b^[I].B;
      End;
      If OBuffer <> Nil
       Then Buffer3b^[I].A := OBuffer3B^[I].R
       Else Buffer3b^[I].A := EnvBlendColor[3];   {obtain blend alpha from envblendcolor.alpha}
      Inc(NumColors);
    End; //  For I
  End
  Else
  Begin
    Buffer4b := PTyBuffA(Buffer);
    For I := 1 To BMPInfo.ImageSize Div 4 Do
    Begin
      If ExchangeRB Then
      Begin
        Buffer3b^[I].R := Buffer4b^[I].B;
        Buffer3b^[I].G := Buffer4b^[I].G;
        Buffer3b^[I].B := Buffer4b^[I].R;
      End
      Else
      Begin
        Buffer3b^[I].R := Buffer4b^[I].R;
        Buffer3b^[I].G := Buffer4b^[I].G;
        Buffer3b^[I].B := Buffer4b^[I].B;
      End;
      If OBuffer <> Nil
       Then Buffer3b^[I].A := OBuffer3B^[I].R
       Else Buffer3b^[I].A := Buffer4b^[I].A;
      Inc(NumColors);
    End; //  For I
  End;

  // If the texture needs to be masked, then do it here

  If NeedsMask Then
  Begin
    For I := 1 To BMPInfo.ImageSize Div 4 Do Buffer3b^[I].A := Max(Max(Buffer3b^[I].R,Buffer3b^[I].G),Buffer3b^[I].B);
  End;

  // Calculate the average color

  AvgR := 0;
  AvgG := 0;
  AvgB := 0;
  TotR := 0;
  TotG := 0;
  TotB := 0;
  J    := 0;
  HasAlpha := False;
  For I := 1 To NumColors Do
  Begin
    R := Buffer3b^[I].R;
    G := Buffer3b^[I].G;
    B := Buffer3b^[I].B;
    Inc(TotR,R);
    Inc(TotG,G);
    Inc(TotB,B);
    If Buffer3b^[I].A <> 255 Then HasAlpha := True;
    If Buffer3b^[I].A <> 0 Then
    Begin
      Inc(AvgR,R);
      Inc(AvgG,G);
      Inc(AvgB,B);
      Inc(J);
    End;
  End; // For I
  If J = 0 Then
  Begin
    J    := NumColors;
    AvgR := TotR;
    AvgG := TotG;
    AvgB := TotB;
  End;
  If J > 0 Then
  Begin
    AvgR := Round(AvgR / J);
    AvgG := Round(AvgG / J);
    AvgB := Round(AvgB / J);
    If AvgR > 255 Then AvgR := 255;
    If AvgG > 255 Then AvgG := 255;
    If AvgB > 255 Then AvgB := 255;
    TRGBA(AvgColor).R := AvgR;
    TRGBA(AvgColor).G := AvgG;
    TRGBA(AvgColor).B := AvgB;
    TRGBA(AvgColor).A := 0;
  End;

  // Load the texture into OpenGL

  glBindTexture(GL_TEXTURE_2D, ID);
  glPixelStorei(GL_UNPACK_ALIGNMENT,4);                {OpenGL 1.0 ignores this one}
  glPixelStorei(GL_UNPACK_ROW_LENGTH,0);
  glPixelStorei(GL_UNPACK_SKIP_ROWS,0);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS,0);

  If Automatic Then  {automatic texture coordinates generation}
  Begin
    glTexGeni (GL_s,gl_texture_gen_mode,AutoGenModeX);
    glTexGeniv(GL_s,gl_object_plane,addr(AutoXmult));
    glTexGeni (GL_t,gl_texture_gen_mode,AutoGenModeZ);
    glTexGeniv(GL_t,gl_object_plane,addr(AutoZmult));
    glEnable(GL_TEXTURE_GEN_S);
    glEnable(GL_TEXTURE_GEN_T);
  End;

  glTexParameteri(gl_texture_2d,gl_texture_wrap_s,WrapSmode);
  glTexParameteri(gl_texture_2d,gl_texture_wrap_t,WrapTmode);
  glTexParameteri(gl_texture_2d,gl_texture_mag_filter,MagFilter);
  glTexParameteri(gl_texture_2d,gl_texture_min_filter,MinFilter);

  // Have to remark out this code so alpha-masked textures work (default OpenGL environment
  // setting is GL_MODULATE)

{
  If BMPInfo.ColorBits = 24 Then
  Begin
    glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,EnvironmentMode);
    glEnable(GL_BLEND);
    glDisable(GL_ALPHA_TEST);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  End
  Else
  Begin
    glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);
    glDisable(GL_BLEND);
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GEQUAL, 0.1);
  End;
}
  {for GLteximage2D the parameters are:
      gl_Texture_2d,
      level of detail (0 unless using mipmapped textures)
      components: 3 for RGB, 4 for RGBA  1 for indexed 256 color
      width, height
      border: width of the border, between 0 and 2.
      Format: gl_color_index, GL_RGB, GL_rgbA, GL_luminance are the most used
      type of the data for each pixel
      pointer to image data}

  glTexImage2d(GL_TEXTURE_2D,0,GL_RGBA,BMPinfo.Width,BMPinfo.Height,0,GL_RGBA,GL_UNSIGNED_BYTE,Buffer3);



  Loaded := True;

//  glendlist;
//  Result := 0;    {no error}

//  FreeMem(Buffer3,Size);
End; // TTexture.LoadTextureIntoOpenGL

Procedure TTexture.Redraw;   {call the display list, it?s not really a redraw, it?s part of one}
Begin

(*
  // Load the texture into OpenGL

  glBindTexture(GL_TEXTURE_2D, ID);
  glPixelStorei(GL_UNPACK_ALIGNMENT,4);                {OpenGL 1.0 ignores this one}
  glPixelStorei(GL_UNPACK_ROW_LENGTH,0);
  glPixelStorei(GL_UNPACK_SKIP_ROWS,0);
  glPixelStorei(GL_UNPACK_SKIP_PIXELS,0);

  If Automatic Then  {automatic texture coordinates generation}
  Begin
    glTexGeni (GL_s,gl_texture_gen_mode,AutoGenModeX);
    glTexGeniv(GL_s,gl_object_plane,addr(AutoXmult));
    glTexGeni (GL_t,gl_texture_gen_mode,AutoGenModeZ);
    glTexGeniv(GL_t,gl_object_plane,addr(AutoZmult));
    glEnable(GL_TEXTURE_GEN_S);
    glEnable(GL_TEXTURE_GEN_T);
  End;

  glTexParameteri(gl_texture_2d,gl_texture_wrap_s,WrapSmode);
  glTexParameteri(gl_texture_2d,gl_texture_wrap_t,WrapTmode);
  glTexParameteri(gl_texture_2d,gl_texture_mag_filter,MagFilter);
  glTexParameteri(gl_texture_2d,gl_texture_min_filter,MinFilter);

  glTexImage2d(gl_texture_2d,0,GL_RGBA,BMPinfo.width,BMPinfo.height,0,GL_RGBA,gl_unsigned_byte,buffer3^);
*)








  Exit; // WC: I'm calculating and providing the texture coordinates myself

  If Automatic Then  // automatic texture coordinates generation
  Begin
    gltexgeni (GL_s,gl_texture_gen_mode,AutoGenModeX);
    gltexgeniv(GL_s,gl_object_plane,addr(AutoXmult));
    gltexgeni (GL_t,gl_texture_gen_mode,AutoGenModeZ);
    gltexgeniv(GL_t,gl_object_plane,addr(AutoZmult));
    glenable(GL_TEXTURE_GEN_S);
    glenable(GL_TEXTURE_GEN_T);
  End;

  gltexparameteri(gl_texture_2d,gl_texture_wrap_s,WrapSmode);
  gltexparameteri(gl_texture_2d,gl_texture_wrap_t,WrapTmode);
  gltexparameteri(gl_texture_2d,gl_texture_mag_filter,MagFilter);
  gltexparameteri(gl_texture_2d,gl_texture_min_filter,MinFilter);
  gltexEnvf(gl_texture_env,gl_texture_env_mode,EnvironmentMode);

//  glcalllist(MyList);
End; // TTexture.Redraw

Procedure SetupMultAlpha;
Var I,J: Integer;
Begin
  For I := 0 To 255 Do
  Begin
    For J := 0 To 255 Do MultAlpha[I,J] := Round(I * (J / 255));
  End; // For I
End; // SetupMultAlpha

Initialization
  LoadTexProc    := Nil;
  LoadTexProcR32 := Nil;
//  PutNames       := False;  {initially, the primitives will not be named}
//  MasterTextures := TStringList.Create;
  Tex2D          := False;
  DefaultExtrudeDepth := 0.3;
  SetupMultAlpha;
Finalization
//  FreeMasterTextures;
End.
