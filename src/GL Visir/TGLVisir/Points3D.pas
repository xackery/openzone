unit Points3D;

interface

Uses Classes,Types,Exentia,SimpleInterfacedObjects;

Const
  FrontPlane = 0; // Near plane
  BackPlane  = 1; // Far plane
  Quaternion_ms_fEpsilon = 1e-03;
  DONT_INTERSECT = 0;
  DO_INTERSECT   = 1;
  COLLINEAR      = 2;

Type
  TGTSIntersect        = (gtsOn,gtsIn,gtsOut);
  TSphereCullResult    = (scrInside,scrIntersects,scrOutside);
  TAlphaClassification = (acInFront,acBehind,acCoinciding,acSpanning);
  PSimplePoint = ^TSimplePoint;
  TSimplePoint = Packed Record
    X,Y,Z,W: Single;
  End;
  TSimplePointList = Packed Array[0..19] Of TSimplePoint;
  PSimplePolygon = ^TSimplePolygon;
  TSimplePolygon = Packed Record
    NumPoints : Integer;
    Points    : TSimplePointList;
  End;
  PSimpleScreenPoint = ^TSimpleScreenPoint;
  TSimpleScreenPoint = Packed Record
    X,Y,Z: Integer;
  End;
  TSimpleScreenPointList = Packed Array[0..19] Of TSimpleScreenPoint;
  PSimpleScreenPolygon = ^TSimpleScreenPolygon;
  TSimpleScreenPolygon = Packed Record
    NumPoints : Integer;
    Points    : TSimpleScreenPointList;
  End;
  T2x2Double = Array[0..3] Of Double;

  TPlane = Class;
{
  P3DPointPointer = ^T3DPointPointer;
  T3DPointPointer = Packed Record
    X,Y,Z,W: Single;
  End;
}
  TNormResult = (nrLength,nrX,nrY,nrZ);
(*
  T3DPoint{I3dPoint} = Interface
  ['{12345678-ABCD-0000-C000-000000ABCD48}']
    Function    PointPointer: P3DPointPointer;
    Procedure   glTranslate;
    Procedure   glRotate;
    Procedure   glScale;
    Procedure   MakeAbsolute;
    Procedure   Average(P1,P2: T3DPoint{I3dPoint});
    Function    AbsMaximum: Single;
    Function    Maximum: Single;
    Function    IsUpOnly: Boolean;
    Function    NoneAreZero: Boolean;
    Function    CheckPointInTriangle(PA,PB,PC: T3DPoint{I3dPoint}): Boolean;
    Function    Normalize: Single; Overload;
    Function    Normalize(NormResult: TNormResult): Single; Overload;
    Procedure   Negate;
    Procedure   Add(P: T3DPoint{I3dPoint});           Overload;
    Procedure   Add(P: PSingleArray);       Overload;
    Procedure   Add(AX,AY,AZ: Single);      Overload;
    Procedure   Subtract(P: T3DPoint{I3dPoint});      Overload;
    Procedure   Subtract(AX,AY,AZ: Single); Overload;
    Procedure   Multiply(S: Single);        Overload;
    Procedure   Multiply(AX,AY,AZ: Single); Overload;
    Procedure   Multiply(P: T3DPoint{I3dPoint});      Overload;
    Procedure   Divide(S: Single);          Overload;
    Procedure   Divide(AX,AY,AZ: Single);   Overload;
    Procedure   Divide(P: T3DPoint{I3dPoint});        Overload;
    Function    Equals(P: T3DPoint{I3dPoint}): Boolean; Overload;
    Function    Equals(Const AX,AY,AZ: Single): Boolean; Overload;
    Procedure   Copy(P: T3DPoint{I3dPoint});          Overload;
    Procedure   Copy(AX,AY,AZ: Single);     Overload;
    Procedure   Copy(P: PSingleArray);      Overload;
    Procedure   Copy(P1,P2: T3DPoint{I3dPoint});      Overload;     // P2 - P1, or a vector from P1 to P2
    Procedure   Copy(P1,P2: P3DPointPointer); Overload;   // P2 - P1, or a vector from P1 to P2
    Function    ParseTokens(S1,S2,S3: String): Boolean;
    Procedure   SetToZero;
    Function    Dot(P: T3DPoint{I3dPoint}): Single; Overload;
    Function    Dot(P: P3DPointPointer): Single; Overload;
    Function    Dot(Var P: TSimplePoint): Single; Overload;
    Function    Dot(AX,AY,AZ: Single): Single; Overload;
    Procedure   Cross(P: T3DPoint{I3dPoint});
    Procedure   CounterClockwiseRotate(P: T3DPoint{I3dPoint}; RX,RY,RZ: Boolean; ReverseOrder: Boolean);
    Procedure   ClockwiseRotate(P: T3DPoint{I3dPoint}; RX,RY,RZ: Boolean; ReverseOrder: Boolean);
    Procedure   Exchange(P: T3DPoint{I3dPoint});
    Function    GetLength: Single;
    Function    GetSquaredLength: Single;
    Procedure   GetNormalTo(P1,P2,P3: T3DPoint{I3dPoint}); Overload;
    Procedure   GetNormalTo(Var P1,P2,P3: TSimplePoint); Overload;
    Procedure   GetParallelTo(P: T3DPoint{I3dPoint}); Overload;
    Procedure   GetParallelTo(P1,P2,P3: T3DPoint{I3dPoint}); Overload;
    Procedure   GetParallelTo(Plane: TPlane); Overload;
    Procedure   GetPerpendicularTo(P: T3DPoint{I3dPoint}); Overload;
    Procedure   GetPerpendicularTo(P1,P2,P3: T3DPoint{I3dPoint}); Overload;
    Procedure   GetPerpendicularTo(Plane: TPlane); Overload;
    Function    DistanceFrom(P: T3DPoint{I3dPoint}): Single;   Overload;
    Function    DistanceFrom(CX,CY,CZ: Single): Single; Overload;
    Function    DistanceFrom2(P: T3DPoint{I3dPoint}): Single;   Overload;
    Function    DistanceFrom2(CX,CY,CZ: Single): Single; Overload;
    Function    DistanceFromLine2(P1,P2: T3DPoint{I3dPoint}): Single;
    Function    DistanceFromRay2(P1,Direction: T3DPoint{I3dPoint}): Single;
    Function    GetSinAngleBetween(P: T3DPoint{I3dPoint}): Single;
    Function    GetCosAngleBetween(P: T3DPoint{I3dPoint}): Single;
    Function    GetID: String;
    Function    GetCloseID(Multiplier: Single): String;
    Function    IsZero: Boolean;
    Function    IsOne: Boolean;
    Function    ToString: String;
    Function    GetX: Single;
    Function    GetY: Single;
    Function    GetZ: Single;
    Procedure   SetX(Value: Single);
    Procedure   SetY(Value: Single);
    Procedure   SetZ(Value: Single);
    Function    GetDirty: Boolean;
    Procedure   SetDirty(Value: Boolean);
    Property    X : Single Read GetX Write SetX;
    Property    Y : Single Read GetY Write SetY;
    Property    Z : Single Read GetZ Write SetZ;
    Property    Dirty : Boolean Read GetDirty Write SetDirty;
  End;
*)

  T3DPoint = Class
  Protected
    FX,FY,FZ{,FW} : Single;
    FDirty      : Boolean;
  Public
    Constructor Create;                     Overload;
    Constructor Create(AX,AY,AZ: Single);   Overload;
    Constructor Create(P: T3DPoint);        Overload;
    Constructor Create(P1,P2: T3DPoint);    Overload;   // P2 - P1, or a vector from P1 to P2
    Procedure   glTranslate;
    Procedure   glRotate;
    Procedure   glScale;
    Procedure   MakeAbsolute;
    Procedure   Average(P1,P2: T3DPoint);
    Function    AbsMaximum: Single;
    Function    Maximum: Single;
    Function    IsUpOnly: Boolean;
    Function    NoneAreZero: Boolean;
    Function    CheckPointInTriangle(PA,PB,PC: T3DPoint): Boolean;
    Function    Normalize: Single; Overload;
    Function    Normalize(NormResult: TNormResult): Single; Overload;
    Procedure   Negate;
    Procedure   Add(P: T3DPoint);           Overload;
    Procedure   Add(P: PSingleArray);       Overload;
    Procedure   Add(AX,AY,AZ: Single);      Overload;
    Procedure   Subtract(P: T3DPoint);      Overload;
    Procedure   Subtract(AX,AY,AZ: Single); Overload;
    Procedure   Multiply(S: Single);        Overload;
    Procedure   Multiply(AX,AY,AZ: Single); Overload;
    Procedure   Multiply(P: T3DPoint);      Overload;
    Procedure   Divide(S: Single);          Overload;
    Procedure   Divide(AX,AY,AZ: Single);   Overload;
    Procedure   Divide(P: T3DPoint);        Overload;
    Function    Equals(P: T3DPoint): Boolean; Overload;
    Function    Equals(Const AX,AY,AZ: Single): Boolean; Overload;
    Procedure   Copy(P: T3DPoint);          Overload;
    Procedure   Copy(AX,AY,AZ: Single);     Overload;
    Procedure   Copy(P: PSingleArray);      Overload;
    Procedure   Copy(P1,P2: T3DPoint);      Overload;     // P2 - P1, or a vector from P1 to P2
//    Procedure   Copy(P1,P2: P3DPointPointer); Overload;   // P2 - P1, or a vector from P1 to P2
    Function    ParseTokens(S1,S2,S3: String): Boolean;
    Procedure   SetToZero;
    Function    Dot(P: T3DPoint): Single; Overload;
//    Function    Dot(P: P3DPointPointer): Single; Overload;
    Function    Dot(Var P: TSimplePoint): Single; Overload;
    Function    Dot(AX,AY,AZ: Single): Single; Overload;
    Procedure   Cross(P: T3DPoint);
    Procedure   CounterClockwiseRotate(P: T3DPoint; RX,RY,RZ: Boolean; ReverseOrder: Boolean);
    Procedure   ClockwiseRotate(P: T3DPoint; RX,RY,RZ: Boolean; ReverseOrder: Boolean);
    Procedure   Exchange(P: T3DPoint);
    Function    GetLength: Single;
    Function    GetSquaredLength: Single;
    Procedure   GetNormalTo(P1,P2,P3: T3DPoint); Overload;
    Procedure   GetNormalTo(Var P1,P2,P3: TSimplePoint); Overload;
    Procedure   GetParallelTo(P: T3DPoint); Overload;
    Procedure   GetParallelTo(P1,P2,P3: T3DPoint); Overload;
    Procedure   GetParallelTo(Plane: TPlane); Overload;
    Procedure   GetPerpendicularTo(P: T3DPoint); Overload;
    Procedure   GetPerpendicularTo(P1,P2,P3: T3DPoint); Overload;
    Procedure   GetPerpendicularTo(Plane: TPlane); Overload;
    Function    DistanceFrom(P: T3DPoint): Single;   Overload;
    Function    DistanceFrom(CX,CY,CZ: Single): Single; Overload;
    Function    DistanceFrom2(P: T3DPoint): Single;   Overload;
    Function    DistanceFrom2(CX,CY,CZ: Single): Single; Overload;
    Function    DistanceFromLine2(P1,P2: T3DPoint): Single;
    Function    DistanceFromRay2(P1,Direction: T3DPoint): Single;
    Function    GetSinAngleBetween(P: T3DPoint): Single;
    Function    GetCosAngleBetween(P: T3DPoint): Single;
    Function    GetID: String;
    Function    GetCloseID(Multiplier: Single): String;
    Function    IsZero: Boolean;
    Function    IsOne: Boolean;
    Function    ToString: String;
//    Function    GetX: Single;
//    Function    GetY: Single;
//    Function    GetZ: Single;
//    Procedure   SetX(Value: Single);
//    Procedure   SetY(Value: Single);
//    Procedure   SetZ(Value: Single);
//    Function    GetDirty: Boolean;
//    Procedure   SetDirty(Value: Boolean);
    Property    Dirty : Boolean Read FDirty Write FDirty;
  Published
    Property    X : Single Read FX Write FX;
    Property    Y : Single Read FY Write FY;
    Property    Z : Single Read FZ Write FZ;
  End;

(*
  I3DObjectFactory = Interface
    Function Priority: Integer;
    Function CanCreate: Boolean;
  End;

  T3DPoint{I3dPoint}Factory = Interface(I3DObjectFactory)
    Function CreatePoint: T3DPoint{I3dPoint};                     Overload;
    Function CreatePoint(AX,AY,AZ: Single): T3DPoint{I3dPoint};   Overload;
    Function CreatePoint(P: T3DPoint{I3dPoint}): T3DPoint{I3dPoint};        Overload;
    Function CreatePoint(P1,P2: T3DPoint{I3dPoint}): T3DPoint{I3dPoint};    Overload;   // P2 - P1, or a vector from P1 to P2
  End;

  T3DPoint = Class
  Protected
    FRegistry : TStringList;    // List of T3DPoint{I3dPoint}Factory
  Public
    Constructor Create;
    Destructor  Destroy;
    Class Procedure Register(Factory: T3DPoint{I3dPoint}Factory);
    Class Function  CreatePoint: T3DPoint{I3dPoint};                     Overload;
    Class Function  CreatePoint(AX,AY,AZ: Single): T3DPoint{I3dPoint};   Overload;
    Class Function  CreatePoint(P: T3DPoint{I3dPoint}): T3DPoint{I3dPoint};        Overload;
    Class Function  CreatePoint(P1,P2: T3DPoint{I3dPoint}): T3DPoint{I3dPoint};    Overload;   // P2 - P1, or a vector from P1 to P2
  End;
*)
  TSphere = Class
    Center : T3DPoint{I3dPoint};
    Radius : Single;
    Constructor Create; Overload;
    Constructor Create(Sphere: TSphere); Overload;
    Destructor  Destroy; Override;
    Function    Intersects(Sphere: TSphere): Boolean;
    Procedure   Copy(Sphere: TSphere);
    Procedure   Setup(P1,P2: T3DPoint{I3dPoint}); Overload;
    Procedure   Setup(X1,Y1,Z1,X2,Y2,Z2: Single); Overload;
    Function    IntersectsLineSegment(P1,P2: T3DPoint{I3dPoint}): Boolean; Overload;
    Function    IntersectsLineSegment(X1,Y1,Z1,X2,Y2,Z2: Single): Boolean; Overload;
    Function    IntersectionsWithLineSegment(P1,P2,I1,I2: T3DPoint{I3dPoint}): Integer;
    Function    IntersectionWithRay(RayOrigin,RayDirection: T3DPoint{I3dPoint}): Single; Overload;
    Function    ContainsPoint(P: T3DPoint{I3dPoint}): Boolean;
    Function    ToString: String;
  End;

  TCylinder = Class
    Position : T3DPoint{I3dPoint};
    Height   : Single;
    Radius   : Single;
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   Copy(Cylinder: TCylinder);
    Procedure   Setup(P1,P2: T3DPoint{I3dPoint}); Overload;
    Procedure   Setup(X1,Y1,Z1,X2,Y2,Z2: Single); Overload;
    Function    ContainsPoint(P: T3DPoint{I3dPoint}): Boolean; Overload;
    Function    ContainsPoint(X,Y,Z: Single): Boolean; Overload;
    Function    IntersectsSphere(Sphere: TSphere): Boolean;
  End;

  TEllipsoid = Class
    Center : T3DPoint{I3dPoint};
    Radius : T3DPoint{I3dPoint};
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   Copy(Ellipsoid: TEllipsoid);
    Function    IntersectionWithRay(RayOrigin,RayDirection: T3DPoint{I3dPoint}): Single;
    Function    ToString: String;
  End;

  TFrustum = Class;
  TPlane = Class
    Normal   : T3DPoint{I3dPoint};
    Distance : Single;
    Constructor Create; Overload;
    Constructor Create(Plane: TPlane); Overload;
    Destructor  Destroy; Override;
    Procedure   Flip;
    Function    ContainsSphere(Sphere: TSphere): TSphereCullResult;
    Function    ContainsPoint(P: T3DPoint{I3dPoint}): Boolean;   Overload;
    Function    ContainsPoint(X,Y,Z: Single): Boolean; Overload;
    Function    ContainsFrustum(Frustum: TFrustum): Boolean;  
    Procedure   Setup(P1,P2,P3: T3DPoint{I3dPoint});             Overload;
    Procedure   Setup(AOrigin,ANormal: T3DPoint{I3dPoint});      Overload;
    Procedure   Copy(Plane: TPlane);
    Function    IntersectionWithRay(RayOrigin,RayDirection: T3DPoint{I3dPoint}): Single; Overload;
    Function    IntersectionWithRay(Var RayOrigin,RayDirection: TSimplePoint): Single; Overload;
    Function    IntersectionWithSegment(P1,P2: T3DPoint{I3dPoint}): Single; Overload;
    Function    IntersectionWithSegment(P1,P2: TSimplePoint): Single; Overload;
    Function    DistanceFromPoint(P: T3DPoint{I3dPoint}): Single; Overload;
//    Function    DistanceFromPoint(P: P3DPointPointer): Single; Overload;
    Function    SignedDistanceFromPoint(P: T3DPoint{I3dPoint}): Single; Overload;
//    Function    SignedDistanceFromPoint(P: P3DPointPointer): Single; Overload;
    Function    ClassifyPoint(P: T3DPoint{I3dPoint}): TAlphaClassification; Overload;
    Function    ClassifyPoint(X,Y,Z: Single): TAlphaClassification; Overload;
    Function    ClassifyPoint(Var P: TSimplePoint): TAlphaClassification; Overload;
    Procedure   SplitSimplePolygon(Var Polygon,PosPolygon,NegPolygon: TSimplePolygon);
    Procedure   SplitSimplePolygonPosOnly(Var Polygon,PosPolygon: TSimplePolygon);
    Function    ToString: String;
//    Procedure   SplitFixedPointPolygon(Var Polygon,PosPolygon,NegPolygon: TSimpleScreenPolygon);
  End;

  TAxisAlignedBox = Class
  Protected
//    Procedure   SetupCorners;
  Public
    MinPt  : T3DPoint{I3dPoint};
    MaxPt  : T3DPoint{I3dPoint};
    Center : T3DPoint{I3dPoint};
//    Corner : Array [0..7] Of T3DPoint{I3dPoint};
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   Setup(P1,P2: T3DPoint{I3dPoint}); Overload;
    Procedure   Setup(X1,Y1,Z1,X2,Y2,Z2: Single); Overload;
    Procedure   Copy(Box: TAxisAlignedBox);
    Function    IntersectionWithLineSegment(P1,P2: T3DPoint{I3dPoint}): Single;
    Procedure   CounterClockwiseRotate(P: T3DPoint{I3dPoint}; RX,RY,RZ,RotateAroundCenter: Boolean);
    Procedure   ClockwiseRotate(P: T3DPoint{I3dPoint}; RX,RY,RZ,RotateAroundCenter: Boolean);
    Procedure   Move(P: T3DPoint{I3dPoint});
    Procedure   Scale(P: T3DPoint{I3dPoint}; ScaleAroundCenter: Boolean);
    Procedure   Expand(Box: TAxisAlignedBox);
    Function    ContainsPoint(P: T3DPoint{I3dPoint}): Boolean;    
  End;

  T3x3Matrix = Class
    M: Array[1..3,1..3] Of Single;
    Constructor Create; Overload;
    Constructor Create(Matrix: T3x3Matrix); Overload;
    Constructor Create(M11,M12,M13,M21,M22,M23,M31,M32,M33: Single); Overload;
    Procedure   Copy(M11,M12,M13,M21,M22,M23,M31,M32,M33: Single);
    Function    Determinant: Single;
    Procedure   Multiply(P: T3DPoint{I3dPoint});        Overload;
    Procedure   Multiply(Var X,Y,Z: Single);  Overload;
    Procedure   Multiply(Var X,Y,Z: Double);  Overload;
    Procedure   Multiply(Matrix: T3x3Matrix); Overload;
    Procedure   Multiply(S: Single);          Overload;
    Procedure   Transpose;
    Procedure   Negate;
  End;

  T3x3FixedPointMatrix = Packed Array[1..3,1..3] Of Integer;

  T4x4Matrix = Class
    M: Array[1..4,1..4] Of Single;
    Constructor Create; Overload;
    Constructor Create(Matrix: T4x4Matrix); Overload;
    Constructor Create(M11,M12,M13,M14,M21,M22,M23,M24,M31,M32,M33,M34,M41,M42,M43,M44: Single); Overload;
    Procedure   Load(M11,M12,M13,M14,M21,M22,M23,M24,M31,M32,M33,M34,M41,M42,M43,M44: Single);
    Procedure   LoadIdentity;
    Procedure   LoadRotationMatrix(Matrix: T3x3Matrix);
    Function    Determinant: Single;
    Procedure   Multiply(P: T3DPoint{I3dPoint});         Overload;
    Procedure   Multiply(Var P: TSimplePoint); Overload;
    Procedure   Multiply(Var X,Y,Z: Single);   Overload;
    Procedure   Multiply(Var X,Y,Z: Double);   Overload;
    Procedure   Multiply(X,Y,Z: Single; Var X1,Y1,Z1: Single);   Overload;
    Procedure   Multiply(Matrix: T4x4Matrix);  Overload;
    Procedure   Transpose;
  End;

  T4x4FixedPointMatrix = Packed Array[1..4,1..4] Of Integer;

  TQuaternion = Class
    W,X,Y,Z: Single;
    Constructor Create; Overload;
    Constructor Create(fW,fX,fY,fZ: Single); Overload;
    Constructor Create(Q: TQuaternion); Overload;

    Procedure   Copy(Q: TQuaternion); Overload;
    Procedure   Copy(fW,fX,fY,fZ: Single); Overload;
    Procedure   FromRotationMatrix(Const kRot: T3x3Matrix);
    Procedure   From4x4Matrix(Const kRot: T4x4Matrix);
    Function    ToRotationMatrix: T3x3Matrix; Overload;
    Procedure   ToRotationMatrix(kRot: T3x3Matrix); Overload;
    Procedure   FromAngleAxis(Const rfAngle: Single; Const rkAxis: T3DPoint{I3dPoint});
    Procedure   ToAngleAxis(Var rfAngle: Single; Var rkAxis: T3DPoint{I3dPoint}); Overload;
    Procedure   ToAngleAxis(Var rfAngle,rkAxisX,rkAxisY,rkAxisZ: Single); Overload;
    Procedure   FromAxes(Const akAxis: Array Of T3DPoint{I3dPoint});   Overload;
    Procedure   FromAxes(Const xAxis,yAxis,zAxis: T3DPoint{I3dPoint}); Overload;
    Procedure   ToAxes(Var akAxis: Array Of T3DPoint{I3dPoint});           Overload;
    Procedure   ToAxes(Var xAxis,yAxis,zAxis: T3DPoint{I3dPoint});         Overload;

    Procedure   Add(Const Q: TQuaternion);
    Procedure   Subtract(Const Q: TQuaternion);
    Procedure   Multiply(Const Q: TQuaternion);                   Overload;
    Procedure   Multiply(fScalar: Single);                        Overload;
    Procedure   Negate;
    Function    Equals(Const Q: TQuaternion): Boolean; Overload;
    Function    Equals(fW,fX,fY,fZ: Single): Boolean; Overload;

    Function    Dot(Const Q: TQuaternion): Single;           // Dot product
    Function    Norm: Single;                                // Squared-length
    Procedure   Normalize;
    Procedure   Invert;                                      // Apply to non-zero quaternion
    Procedure   UnitInvert;                                  // Apply to unit-length quaternion
    Procedure   Exp;
    Procedure   Log;
    Procedure   Slerp(Q1: TQuaternion; T: Single);

    Procedure   Transform(Var P: T3DPoint{I3dPoint}); Overload;
    Procedure   Transform(Var PX,PY,PZ: Single); Overload;

    Procedure   InverseTransform(Var P: T3DPoint{I3dPoint}); Overload;
  End;

  TBasicPolygon = Class
    Plane    : TPlane;
    Vertices : Array Of T3DPoint{I3dPoint};
    Flag     : Boolean;
    Constructor Create;
    Destructor  Destroy; Override;
    Function    Classify(Plane: TPlane): TAlphaClassification;
    Function    Allocate(NumVertices: Integer): Boolean;
    Procedure   CalculatePlane;
    Procedure   InsertVert(V: T3DPoint{I3dPoint});
    Function    SplitPolygon(Plane: TPlane; PosPolygon,NegPolygon: TBasicPolygon): Boolean;
    Procedure   Flip;
  End;

  TFrustumPoints = Array[0..7] Of T3DPoint{I3dPoint};
  TFrustum = Class
  Protected
    FPoints        : TFrustumPoints; // FLL, FUL, FUR, FLR, NLL, NUL, NUR, NLR, as seen from camera
    FNearDist      : Single;
    FFarDist       : Single;
    FViewAngle     : Single;
    FWidth         : Integer;
    FHeight        : Integer;
    FObserver      : T3DPoint{I3dPoint};
    FObserverFixed : TSimpleScreenPoint;
    FNormal        : T3DPoint{I3dPoint};
    FUp            : T3DPoint{I3dPoint};
    FSphere        : TSphere;
    FConeFOVAngle  : Single;
    FConeSin2      : Single;
    FConeCos2      : Single;
    FConeSinRecip  : Single;
    FDist1         : T3DPoint{I3dPoint};
    FDist2         : T3DPoint{I3dPoint};
    FOrientation   : TQuaternion;
    FViewMatrix    : T4x4Matrix;
    FProjectF      : Single;
    FProjectG      : Single;
    FProjectFFixed : Integer;
    FProjectGFixed : Integer;
    Procedure   SetViewAngle(AViewAngle: Single);
    Procedure   SetNearDist(ANearDist: Single);
    Procedure   SetFarDist(AFarDist: Single);
    Procedure   UpdateViewMatrix;
  Public
    EViewMatrix    : TFVector;
    ViewMatrixF    : T4x4FixedPointMatrix;
    FPlane         : Array Of TPlane;
    Constructor Create; Overload;
    Constructor Create(Observer,LookAt,Up: T3DPoint{I3dPoint}; ANearDist,AFarDist,AViewAngle: Single; Width,Height: Integer); Overload;
    Constructor Create(OX,OY,OZ,LX,LY,LZ,UX,UY,UZ,ANearDist,AFarDist,AViewAngle: Single; Width,Height: Integer); Overload;
    Destructor  Destroy; Override;
    Function    PointIsInside(P: T3DPoint{I3dPoint}): Boolean; Overload;
    Function    PointIsInside(X,Y,Z: Single): Boolean; Overload;
    Function    PolygonIsInside(Polygon: TBasicPolygon): Boolean;
    Function    TriangleIsInside(P1,P2,P3: T3DPoint{I3dPoint}): Boolean;
//    Function    ContainsAxisAlignedBox(Box: TAxisAlignedBox): Boolean; Overload;
    Function    ContainsSphere(Sphere: TSphere): TSphereCullResult;
    Procedure   SetWidthAndHeight(Width,Height: Integer);
    Procedure   SetObserver(P: T3DPoint{I3dPoint}); Overload;
    Procedure   SetObserver(X,Y,Z: Single); Overload;
    Procedure   SetLookAt(P: T3DPoint{I3dPoint}); Overload;
    Procedure   SetLookAt(X,Y,Z: Single); Overload;
    Procedure   SetUp(P: T3DPoint{I3dPoint}); Overload;
    Procedure   SetUp(X,Y,Z: Single); Overload;
    Procedure   SetPosLookAtUp(PX,PY,PZ,LX,LY,LZ,UX,UY,UZ: Single);
    Procedure   SetupPlanes;
    Function    Adjust(Polygon: TBasicPolygon; Position: T3DPoint{I3dPoint}): TFrustum;
    Procedure   AllocatePlanes(NumPlanes: Integer);
    Procedure   ClipSimplePolygon(Var Polygon: TSimplePolygon);
    Procedure   ClipFixedPointPolygon(Var Polygon: TSimpleScreenPolygon);
    Procedure   ProjectTransformedPoint(Var Source: TSimplePoint; Var Dest: TPoint);
    Procedure   ProjectTransformedFixedPoint(SourceX,SourceY,SourceZ: Integer; Var Dest: TPoint);
    Procedure   ProjectTransformedFixedPointToFixedPoint(SourceX,SourceY,SourceZ: Integer; Var Dest: TPoint);
    Procedure   ClipFixedPointPolygonToScreen(Var Polygon: TSimpleScreenPolygon);
    Procedure   ClipPolygonToScreen(Var Polygon: TSimplePolygon);
    Procedure   ClipIntegerPolygonToScreen(Var Polygon: TSimpleScreenPolygon);
    Procedure   ClipTransformedPolygonToNearDist(Var Polygon: TSimplePolygon; Dest: PSimplePolygon);
    Property    ViewAngle     : Single     Read FViewAngle Write SetViewAngle;
    Property    NearDist      : Single     Read FNearDist  Write SetNearDist;
    Property    FarDist       : Single     Read FFarDist   Write SetFarDist;
    Property    Sphere        : TSphere    Read FSphere;
    Property    ViewMatrix    : T4x4Matrix Read FViewMatrix;
    Property    ObserverFixed : TSimpleScreenPoint Read FObserverFixed;
    Property    Orientation   : TQuaternion        Read FOrientation;
    Property    Points        : TFrustumPoints Read FPoints;
  End;

Function  gts_segments_are_intersecting(P1,P2,P3,P4: T3DPoint{I3dPoint}): TGTSIntersect;
Function  gts_point_segment_distance2(P,P1,P2: T3DPoint{I3dPoint}): Double;
Function  gts_point_segment_distance(P,S1,S2: T3DPoint{I3dPoint}): Double;
Function  gts_point_triangle_distance2(P,P1,P2,P3: T3DPoint{I3dPoint}): Double;
Function  gts_point_triangle_distance(P,T1,T2,T3: T3DPoint{I3dPoint}): Double;
Procedure gts_point_segment_closest(P,P1,P2,Closest: T3DPoint{I3dPoint});
Procedure gts_point_triangle_closest(P,P1,P2,P3,Closest: T3DPoint{I3dPoint});
Function  gts_point_orientation_3d(P1,P2,P3,P4: T3DPoint{I3dPoint}): Double;
Function  gts_point_orientation(P1,P2,P3: T3DPoint{I3dPoint}): Double;
Function  gts_point_orientation_int(X1,Y1,X2,Y2,X3,Y3: Integer): Integer;
Function  gts_point_is_in_triangle(P,V1,V2,V3: T3DPoint{I3dPoint}): TGTSIntersect;
Function  gts_point_is_in_triangle_int(X,Y,X1,Y1,X2,Y2,X3,Y3: Integer): TGTSIntersect;
Function  gts_segment_triangle_intersection(S1,S2,T1,T2,T3: T3DPoint{I3dPoint}; Boundary: Boolean; Intersection: T3DPoint{I3dPoint}): Boolean;
Function  LineIntersection(X1,Y1,X2,Y2,X3,Y3,X4,Y4: Single; Var X,Y: Single): Integer;
Function  LinePlaneIntersection(L1,L2,P1,P2,P3: T3DPoint{I3dPoint}): T3DPoint{I3dPoint};
Function  LineFacet(P1,P2,PA,PB,PC,Intersect: T3DPoint{I3dPoint}): Boolean;
Function  DistanceFromLine(X1,Y1,X2,Y2,X,Y: Single): Single;
Function  DistanceFromLineSegment(X1,Y1,X2,Y2,X,Y: Single): Single;
Function  GetHessianDistance(P1,P2,P3: T3DPoint{I3dPoint}): Single; Overload;
Function  GetHessianDistance(Normal,P: T3DPoint{I3dPoint}): Single; Overload;
Function  Sphere_Sphere_Intersects(CX1,CY1,CZ1,CX2,CY2,CZ2,R1,R2: Single): Boolean;

Function  lines_exp_int_2d(X1,Y1,X2,Y2,X3,Y3,X4,Y4: Double; Var X,Y: Double): Integer;
Procedure line_exp2imp_2d(X1,Y1,X2,Y2: Double; Var A,B,C: Double);
Function  lines_imp_int_2d(A1,B1,C1,A2,B2,C2: Double; Var X,Y: Double): Integer;
Function  dmat_inverse_2d(A: T2x2Double; Var B: T2x2Double): Boolean;

Procedure T3x3FixedPointMatrix_Multiply(Var M: T3x3FixedPointMatrix; Var X,Y,Z: Integer); Overload;
Procedure T3x3FixedPointMatrix_Transpose(Var M: T3x3FixedPointMatrix);
Procedure T3x3FixedPointMatrix_Negate(Var M: T3x3FixedPointMatrix);

Procedure T4x4FixedPointMatrix_LoadIdentity(Var M: T4x4FixedPointMatrix);
Procedure T4x4FixedPointMatrix_LoadRotationMatrix(Var M: T4x4FixedPointMatrix; Var M3: T3x3FixedPointMatrix);
Procedure T4x4FixedPointMatrix_Multiply(Var M: T4x4FixedPointMatrix; Var X,Y,Z: Integer); Overload;
Procedure T4x4FixedPointMatrix_Transpose(Var M: T4x4FixedPointMatrix);

Function FixedMul(M1,M2: Integer): Integer;
Function FixedDiv(Dividend,Divisor: Integer): Integer;

Function IsMMXEnabled: Boolean;
Function IsSSEEnabled: Boolean;
Function IsSSE2Enabled: Boolean;
Function IsSSE3Enabled: Boolean;
Function IsCMOVEnabled: Boolean;

implementation

Uses SysUtils, Math, dglOpenGL;

Var
//  PointCreator : T3DPoint;
  EnableMMX    : Boolean;
  EnableSSE    : Boolean;
  EnableSSE2   : Boolean;
  EnableSSE3   : Boolean;
  EnableCMOV   : Boolean;

Function IsMMXEnabled: Boolean;
Begin
  Result := EnableMMX;
End; // IsMMXEnabled

Function IsSSEEnabled: Boolean;
Begin
  Result := EnableSSE;
End; // IsSSEEnabled

Function IsSSE2Enabled: Boolean;
Begin
  Result := EnableSSE2;
End; // IsSSE2Enabled

Function IsSSE3Enabled: Boolean;
Begin
  Result := EnableSSE3;
End; // IsSSE3Enabled

Function IsCMOVEnabled: Boolean;
Begin
  Result := EnableCMOV;
End; // IsCMOVEnabled

Procedure FindMMX;
Var B: Boolean;
Begin
  // Determine if MMX is available

  EnableMMX := False;
  B      := False;

  // Find out if this processor supports MMX

  Asm
    // Find out if the CPUID instruction is available

    PUSHAD

    SUB   EDX,EDX
    PUSHFD
    POP   ECX                 // Get original EFLAGS
    MOV   EAX,ECX
    XOR   EAX,$200000         // Flip ID bit in EFLAGS
    PUSH  EAX                 // Save new EFLAGS value on stack
    POPFD                     // Replace current EFLAGS value
    PUSHFD                    // Get new EFLAGS
    POP   EAX                 // Store new EFLAGS in EAX
    XOR   EAX,ECX             // Can't toggle ID bit,
    JNE   @MMX                // If zero, Processor = 80486 or lower
    PUSH  ECX
    POPFD
    JMP   @Done
@MMX:
    PUSH  ECX
    POPFD

    // See if MMX is available

    MOV   EAX,1               // Get feature flag info
    DB    0Fh,0A2h            // CPUID
    TEST  EDX,$800000         // See if MMX is available
    JZ    @Done
    MOV   BYTE PTR B,1        // It's available

@Done:

    POPAD
  End; // Asm
  EnableMMX := B;
End;

Procedure FindSSE;
Begin
  Try
    EnableSSE  := False;
    EnableSSE2 := False;
    EnableSSE3 := False;
    EnableCMOV := False;
    Asm

    PUSHFD

      MOV   EAX, 1
      DB    $0F,$A2               /// cpuid

      TEST  edx,(1 Shl 25)
      JNZ   @SSEFound
      MOV   EnableSSE,0
      JMP   @END_SSE
@SSEFound:
      MOV   EnableSSE,1
@END_SSE:

      TEST  edx,(1 Shl 26)
      JNZ   @SSE2Found
      MOV   EnableSSE2,0
      JMP   @END_SSE2
@SSE2Found:
      MOV   EnableSSE2,1

      TEST  ECX,1
      JZ    @END_SSE2
      MOV   EnableSSE3,1

@END_SSE2:

      TEST  EDX,(1 Shl 15)
      JNZ   @CMOVFound
      MOV   EnableCMOV,0
      JMP   @END_CMOV
@CMOVFound:
      MOV   EnableCMOV,1
@END_CMOV:

    POPFD

    End;
  Except
    EnableSSE  := False;
    EnableSSE2 := False;
    EnableSSE3 := False;
    EnableCMOV := False;
  End;
End;

Function FixedMul(M1,M2: Integer): Integer;
Asm
  IMUL EDX
  SHRD    EAX,EDX,16
End; // FixedMul

Function FixedDiv(Dividend,Divisor: Integer): Integer;
Asm
  MOV   ECX,EDX
  MOV   EDX,EAX
  SUB   EAX,EAX
  SHRD  EAX,EDX,16           // Position so that result ends up in EAX
  SAR   EDX,16
  IDIV  ECX
End; // FixedDiv

Function lines_exp_int_2d(X1,Y1,X2,Y2,X3,Y3,X4,Y4: Double; Var X,Y: Double): Integer;
//******************************************************************************
//
//  Purpose:
//
//    LINES_EXP_INT_2D determines where two explicit lines intersect in 2D.
//
//  Formula:
//
//    The explicit form of a line in 2D is:
//
//      (X1,Y1), (X2,Y2).
//
//  Modified:
//
//    02 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X1, Y1, X2, Y2, define the first line.
//
//    Input, double X3, Y3, X4, Y4, define the second line.
//
//    Output, int *IVAL, reports on the intersection:
//    0, no intersection, the lines may be parallel or degenerate.
//    1, one intersection point, returned in X, Y.
//    2, infinitely many intersections, the lines are identical.
//
//    Output, double *X, *Y, if IVAl = 1, then X, Y contains
//    the intersection point.  Otherwise, X = 0, Y = 0.
//
Var
  A1      : Double;
  A2      : Double;
  B1      : Double;
  B2      : Double;
  C1      : Double;
  C2      : Double;
  Point_1 : Boolean;
  Point_2 : Boolean;

Begin
  Result := 0;
  X      := 0;
  Y      := 0;

  //  Check whether either line is a point.

  Point_1 := (X1 = X2) And (Y1 = Y2);
  Point_2 := (X3 = X4) And (Y3 = Y4);

  //  Convert the lines to ABC format.

  If Not Point_1 Then line_exp2imp_2d(X1, Y1, X2, Y2, A1, B1, C1);

  If Not Point_2 Then line_exp2imp_2d(X3, Y3, X4, Y4, A2, B2, C2);

  //  Search for intersection of the lines.

  If Point_1 And Point_2 Then
  Begin
    If (X1 = X3) And (Y1 = Y3) Then
    Begin
      Result := 1;
      X      := X1;
      Y      := Y1;
    End;
  End
  Else If Point_1 Then
  Begin
    If A2 * X1 + B2 * Y1 = C2 Then
    Begin
      Result := 1;
      X      := X1;
      Y      := Y1;
    End;
  End
  Else If Point_2 Then
  Begin
    If A1 * X3 + B1 * Y3 = C1 Then
    Begin
      Result := 1;
      X      := X3;
      Y      := Y3;
    End;
  End
  Else Result := lines_imp_int_2d(A1, B1, C1, A2, B2, C2, X, Y);
End; // lines_exp_int_2d

Procedure line_exp2imp_2d(X1,Y1,X2,Y2: Double; Var A,B,C: Double);
//********************************************************************
//
//  Purpose:
//
//    LINE_EXP2IMP_2D converts an explicit line to implicit form in 2D.
//
//  Formula:
//
//    The explicit form of a line in 2D is:
//
//      (X1,Y1), (X2,Y2).
//
//    The implicit form of a line in 2D is:
//
//      A * X + B * Y + C = 0
//
//  Modified:
//
//    22 April 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X1, Y1, X2, Y2.  (X1,Y1) and (X2,Y2) are
//    two points on the line. (X1,Y1) must be different
//    from (X2,Y2).
//
//    Output, double *A, *B, *C, three coefficients which describe
//    the line that passes through (X1,Y1) and (X2,Y2).
//
Begin
  //  Take care of degenerate cases.

  If (X1 = X2) And (Y1 = Y2) Then
  Begin
    // Should never get here
  End
  Else
  Begin
    A := Y2 - Y1;
    B := X1 - X2;
    C := X2 * Y1 - X1 * Y2;
  End;
End; // line_exp2imp_2d

Function lines_imp_int_2d(A1,B1,C1,A2,B2,C2: Double; Var X,Y: Double): Integer;
//********************************************************************
//
//  Purpose:
//
//    LINES_IMP_INT_2D determines where two implicit lines intersect in 2D.
//
//  Formula:
//
//    The implicit form of a line in 2D is:
//
//      A * X + B * Y + C = 0
//
//  Discussion:
//
//    22 May 2004: Thanks to John Asmuth for pointing out that the
//    B array was not being deallocated on exit.
//
//  Modified:
//
//    27 June 1999
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A1, B1, C1, define the first line.
//    At least one of A1 and B1 must be nonzero.
//
//    Input, double A2, B2, C2, define the second line.
//    At least one of A2 and B2 must be nonzero.
//
//    Output, int *IVAL, reports on the intersection.
//
//    -1, both A1 and B1 were zero.
//    -2, both A2 and B2 were zero.
//     0, no intersection, the lines are parallel.
//     1, one intersection point, returned in X, Y.
//     2, infinitely many intersections, the lines are identical.
//
//    Output, double *X, *Y, if IVAL = 1, then X, Y contains
//    the intersection point.  Otherwise, X = 0, Y = 0.
//
Var
  A : T2x2Double;
  B : T2x2Double;

Begin
  X := 0;
  Y := 0;

  //  Refuse to handle degenerate lines.

       If (A1 = 0) And (B1 = 0) Then Result := -1
  Else If (A2 = 0) And (B2 = 0) Then Result := -2
  Else
  Begin
    //  Set up a linear system, and compute its inverse.

    A[0 + 0 * 2] := A1;
    A[0 + 1 * 2] := B1;
    A[1 + 0 * 2] := A2;
    A[1 + 1 * 2] := B2;

    //  If the inverse exists, then the lines intersect.
    //  Multiply the inverse times -C to get the intersection point.

    If dmat_inverse_2d(A,B) Then
    Begin
      Result := 1;
      x      := -B[0+0*2] * C1 - B[0+1*2] * C2;
      y      := -B[1+0*2] * C1 - B[1+1*2] * C2;
    End
    Else
    Begin
      //  If the inverse does not exist, then the lines are parallel
      //  or coincident.  Check for parallelism by seeing if the
      //  C entries are in the same ratio as the A or B entries.

      Result := 0;
      If A1 = 0 Then
      Begin
        If B2 * C1 = C2 * B1 Then Result := 2;
      End
      Else
      Begin
        If A2 * C1 = C2 * A1 Then Result := 2;
      End;
    End;
  End;
End; // lines_imp_int_2d

Function dmat_inverse_2d(A: T2x2Double; Var B: T2x2Double): Boolean;
//********************************************************************
//
//  Purpose:
//
//    DMAT_INVERSE_2D inverts a 2 by 2 double matrix using Cramer's rule.
//
//  Discussion:
//
//    The two dimensional array is stored as a one dimensional vector,
//    by COLUMNS.
//
//  Modified:
//
//    23 September 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double A[2*2], the matrix to be inverted.
//
//    Output, double RMAT2_INVERSE[2*2], the inverse of the matrix A.
//
Var
  Det : Double;
  I,J : Integer;

Begin
  //  Compute the determinant of A.

  Det := A[0+0*2] * A[1+1*2] - A[0+1*2] * A[1+0*2];

  //  If the determinant is zero, bail out.

  If Det = 0 Then Result := False
  Else
  Begin
    //  Compute the entries of the inverse matrix using an explicit formula.

    B[0+0*2] :=  A[1+1*2] / Det;
    B[0+1*2] := -A[0+1*2] / Det;
    B[1+0*2] := -A[1+0*2] / Det;
    B[1+1*2] :=  A[0+0*2] / Det;
    Result   := True;
  End;
End; // dmat_inverse_2d

Function Sphere_Sphere_Intersects(CX1,CY1,CZ1,CX2,CY2,CZ2,R1,R2: Single): Boolean;
Begin
  Result := Sqr(CX1 - CX2) + Sqr(CY1 - CY2) + Sqr(CZ1 - CZ2) < Sqr(R1 + R2);
End; // Sphere_Sphere_Intersects

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

(* lines_intersect:  AUTHOR: Mukesh Prasad
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
 *)
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

Function GetHessianDistance(P1,P2,P3: T3DPoint{I3dPoint}): Single;
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

Function GetHessianDistance(Normal,P: T3DPoint{I3dPoint}): Single;
// Normal is the normal to a polygon and P is any point on the polygon
Begin
  Result := -Normal.Dot(P);
End; // GetHessianDistance

(**
 * gts_point_segment_distance2:
 * @p: a #GtsPoint.
 * @s: a #GtsSegment.
 *
 * Returns: the square of the minimun Euclidean distance between @p and @s.
 *)
Function gts_point_segment_distance2(P,P1,P2: T3DPoint{I3dPoint}): Double;
Var
  T     : Double;
  NS2   : Double;
  X,Y,Z : Double;

Begin
  NS2 := P1.DistanceFrom2(P2);
  If NS2 = 0 Then Result := P.DistanceFrom2(P1)
  Else
  Begin
    T := ((P2.X - P1.X) * (P.X - P1.X) +
          (P2.Y - P1.Y) * (P.Y - P1.Y) +
          (P2.Z - P1.Z) * (P.Z - P1.Z)) / NS2;
         If T > 1 Then Result := P.DistanceFrom2(P2)
    Else If T < 0 Then Result := P.DistanceFrom2(P1)
    Else
    Begin
      X      := (1 - T) * P1.X + T * P2.X - P.X;
      Y      := (1 - T) * P1.Y + T * P2.Y - P.Y;
      Z      := (1 - T) * P1.Z + T * P2.Z - P.Z;
      Result := Sqr(X) + Sqr(Y) + Sqr(Z);
    End;
  End;
End; // gts_point_segment_distance2

(**
 * gts_point_segment_distance:
 * @p: a #GtsPoint.
 * @s: a #GtsSegment.
 *
 * Returns: the minimun Euclidean distance between @p and @s.
 *)
Function gts_point_segment_distance(P,S1,S2: T3DPoint{I3dPoint}): Double;
Begin
  Result := Sqrt(gts_point_segment_distance2(P,S1,S2));
End; // gts_point_segment_distance

(**
 * gts_point_triangle_distance2:
 * @p: a #GtsPoint.
 * @t: a #GtsTriangle.
 *
 * Returns: the square of the minimun Euclidean distance between @p and @t.
 *)
Function gts_point_triangle_distance2(P,P1,P2,P3: T3DPoint{I3dPoint}): Double;
Var
  P1P2      : T3DPoint{I3dPoint};
  P1P3      : T3DPoint{I3dPoint};
  PP1       : T3DPoint{I3dPoint};
  A,B,C,D,E : Double;
  Det       : Double;
  T1,T2     : Double;
  X,Y,Z     : Double;
  D1,D2     : Double;

Begin
  P1P2 := T3DPoint.Create{Point}(P2);
  P1P2.Subtract(P1);

  P1P3 := T3DPoint.Create{Point}(P3);
  P1P3.Subtract(P1);

  PP1 := T3DPoint.Create{Point}(P1);
  PP1.Subtract(P);

  B := P1P3.Dot(P1P2);
  E := P1P2.Dot(P1P2);
  C := P1P3.Dot(P1P3);

  Det := B * B - E * C;

  If Det = 0 Then
  Begin
    // P1P2 and P1P3 are colinear

    D1     := gts_point_segment_distance2(P,P1,P2);
    D2     := gts_point_segment_distance2(P,P3,P1);
    Result := Min(D1,D2);
  End
  Else
  Begin
    A  := P1P3.Dot(PP1);
    D  := P1P2.Dot(PP1);
    T1 := (D * C - A * B) / Det;
    T2 := (A * E - D * B) / Det;
         If T1 < 0      Then Result := gts_point_segment_distance2(P,P3,P1)
    Else If T2 < 0      Then Result := gts_point_segment_distance2(P,P1,P2)
    Else If T1 + T2 > 1 Then Result := gts_point_segment_distance2(P,P2,P3)
    Else
    Begin
      X      := PP1.X + T1 * P1P2.X + T2 * P1P3.X;
      Y      := PP1.Y + T1 * P1P2.Y + T2 * P1P3.Y;
      Z      := PP1.Z + T1 * P1P2.Z + T2 * P1P3.Z;
      Result := Sqr(X) + Sqr(Y) + Sqr(Z);
    End;
  End;
  P1P2.Free;
  P1P3.Free;
  PP1.Free;
{
  P1P2 := Nil;
  P1P3 := Nil;
  PP1  := Nil;
}  
End; // gts_point_triangle_distance2

(**
 * gts_point_triangle_distance:
 * @p: a #GtsPoint.
 * @t: a #GtsTriangle.
 *
 * Returns: the minimun Euclidean distance between @p and @t.
 *)
Function gts_point_triangle_distance(P,T1,T2,T3: T3DPoint{I3dPoint}): Double;
Begin
  Result := Sqrt(gts_point_triangle_distance2(P,T1,T2,T3));
End; // gts_point_triangle_distance

(**
 * gts_point_segment_closest:
 * @p: a #GtsPoint.
 * @s: a #GtsSegment.
 * @closest: a #GtsPoint.
 *
 * Set the coordinates of @closest to the coordinates of the point belonging
 * to @s closest to @p.
 *)
Procedure gts_point_segment_closest(P,P1,P2,Closest: T3DPoint{I3dPoint});
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
 * gts_segments_are_intersecting:
 * @s1: a #GtsSegment.
 * @s2: a #GtsSegment.
 *
 * Returns: %GTS_IN if @s1 and @s2 are intersecting, %GTS_ON if one of the
 * endpoints of @s1 (resp. @s2) lies on @s2 (resp. @s1), %GTS_OUT otherwise.
 *)
Function gts_segments_are_intersecting(P1,P2,P3,P4: T3DPoint{I3dPoint}): TGTSIntersect;
Var D1,D2,D3,D4: Double;
Begin
  D1 := gts_point_orientation(P1,P2,P3);
  D2 := gts_point_orientation(P1,P2,P4);
  If ((D1 > 0) And (D2 > 0)) Or
     ((D1 < 0) And (D2 < 0)) Then Result := gtsOut
  Else
  Begin
    D3 := gts_point_orientation(P3,P4,P1);
    D4 := gts_point_orientation(P3,P4,P2);
    If ((D3 > 0) And (D4 > 0)) Or
       ((D3 < 0) And (D4 < 0)) Then Result := gtsOut
    Else
    Begin
      If (D1 = 0) Or (D2 = 0) Or(D3 = 0) Or(D4 = 0)
       Then Result := gtsOn
       Else Result := gtsIn;
    End;
  End;
End; // gts_segments_are_intersecting

(**
 * gts_point_triangle_closest:
 * @p: a #GtsPoint.
 * @t: a #GtsTriangle.
 * @closest: a #GtsPoint.
 *
 * Set the coordinates of @closest to those of the point belonging to @t and
 * closest to @p.
 *)
Procedure gts_point_triangle_closest(P,P1,P2,P3,Closest: T3DPoint{I3dPoint});
Var
  P1P2      : T3DPoint{I3dPoint};
  P1P3      : T3DPoint{I3dPoint};
  PP1       : T3DPoint{I3dPoint};
  CP        : T3DPoint{I3dPoint};
  A,B,C,D,E : Double;
  Det       : Double;
  T1,T2     : Double;

Begin
  P1P2 := T3DPoint.Create{Point}(P2);
  P1P2.Subtract(P1);

  P1P3 := T3DPoint.Create{Point}(P3);
  P1P3.Subtract(P1);

  PP1 := T3DPoint.Create{Point}(P1);
  PP1.Subtract(P);

  B := P1P3.Dot(P1P2);
  E := P1P2.Dot(P1P2);
  C := P1P3.Dot(P1P3);

  Det := B * B - E * C;

  If Det = 0 Then
  Begin
    CP := T3DPoint.Create{Point};
    gts_point_segment_closest(P,P1,P2,CP);
    gts_point_segment_closest(P,P3,P1,Closest);
    If CP.DistanceFrom2(P) < Closest.DistanceFrom2(P) Then Closest.Copy(CP);
    CP.Free;
//    CP := Nil;
  End
  Else
  Begin
    A  := P1P3.Dot(PP1);
    D  := P1P2.Dot(PP1);
    T1 := (D * C - A * B) / Det;
    T2 := (A * E - D * B) / Det;
         If T1 < 0      Then gts_point_segment_closest(P,P3,P1,Closest)
    Else if T2 < 0      Then gts_point_segment_closest(P,P1,P2,Closest)
    Else If T1 + T2 > 1 Then gts_point_segment_closest(P,P2,P3,Closest)
    Else Closest.Copy(P1.X + T1 * P1P2.X + T2 * P1P3.X,
                      P1.Y + T1 * P1P2.Y + T2 * P1P3.Y,
                      P1.Z + T1 * P1P2.Z + T2 * P1P3.Z);
  End;
  P1P2.Free;
  P1P3.Free;
  PP1.Free;
{
  P1P2 := Nil;
  P1P3 := Nil;
  PP1  := Nil;
}  
End; // gts_point_triangle_closest

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
Function gts_point_orientation_3d(P1,P2,P3,P4: T3DPoint{I3dPoint}): Double;
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

(**
 * gts_point_orientation:
 * @p1: a #GtsPoint.
 * @p2: a #GtsPoint.
 * @p3: a #GtsPoint.
 *
 * Checks for orientation of the projection of three points on the
 * (x,y) plane. The result is also an approximation of twice the
 * signed area of the triangle defined by the three points. This
 * function uses adaptive floating point arithmetic and is
 * consequently geometrically robust.
 *
 * Returns: a positive value if @p1, @p2 and @p3 appear in
 * counterclockwise order, a negative value if they appear in
 * clockwise order and zero if they are colinear.
 *)
Function gts_point_orientation(P1,P2,P3: T3DPoint{I3dPoint}): Double;
Var ACX,BCX,ACY,BCY: Double;
Begin
  ACX    := P1.X - P3.X;
  BCX    := P2.X - P3.X;
  ACY    := P1.Y - P3.Y;
  BCY    := P2.Y - P3.Y;
  Result := ACX * BCY - ACY * BCX;
End; // gts_point_orientation

Function gts_point_orientation_int(X1,Y1,X2,Y2,X3,Y3: Integer): Integer;
Var ACX,BCX,ACY,BCY: Integer;
Begin
  ACX    := X1 - X3;
  BCX    := X2 - X3;
  ACY    := Y1 - Y3;
  BCY    := Y2 - Y3;
  Result := ACX * BCY - ACY * BCX;
End; // gts_point_orientation_int

(**
 * gts_point_is_in_triangle:
 * @p: a #GtsPoint.
 * @t: a #GtsTriangle.
 *
 * Tests if the planar projection (x, y) of @p is inside, outside or
 * on the boundary of the planar projection of @t.  This function is
 * geometrically robust.
 *
 * Returns: %GTS_IN if @p is inside @t, %GTS_ON if @p is on the boundary of
 * @t, %GTS_OUT otherwise.
 *)
Function gts_point_is_in_triangle(P,V1,V2,V3: T3DPoint{I3dPoint}): TGTSIntersect;
Var D1,D2,D3: Double;
Begin
  Result := gtsOut;
  D1 := gts_point_orientation(V1,V2,P);
  If D1 >= 0 Then
  Begin
    D2 := gts_point_orientation(V2,V3,P);
    If D2 >= 0 Then
    Begin
      D3 := gts_point_orientation(V3,V1,P);
      If D3 >= 0 Then
      Begin
        If (D1 = 0) Or (D2 = 0) Or (D3 = 0)
         Then Result := gtsOn
         Else Result := gtsIn;
      End;
    End;
  End;
End; // gts_point_is_in_triangle

(**
 * gts_point_is_in_triangle:
 * @p: a #GtsPoint.
 * @t: a #GtsTriangle.
 *
 * Tests if the planar projection (x, y) of @p is inside, outside or
 * on the boundary of the planar projection of @t.  This function is
 * geometrically robust.
 *
 * Returns: %GTS_IN if @p is inside @t, %GTS_ON if @p is on the boundary of
 * @t, %GTS_OUT otherwise.
 *)
Function gts_point_is_in_triangle_int(X,Y,X1,Y1,X2,Y2,X3,Y3: Integer): TGTSIntersect;
Var D1,D2,D3: Integer;
Begin
  D1 := gts_point_orientation_int(X1,Y1,X2,Y2,X,Y);
  D2 := gts_point_orientation_int(X2,Y2,X3,Y3,X,Y);
  D3 := gts_point_orientation_int(X3,Y3,X1,Y1,X,Y);
  If (D1 = 0) Or (D2 = 0) Or (D3 = 0) Then Result := gtsOn
  Else
  Begin
    If (Sign(D1) = Sign(D2)) And (Sign(D1) = Sign(D3))
     Then Result := gtsIn
     Else Result := gtsOut;
  End;
{
  D1 := gts_point_orientation_int(X1,Y1,X2,Y2,X,Y);
  If D1 >= 0 Then
  Begin
    D2 := gts_point_orientation_int(X2,Y2,X3,Y3,X,Y);
    If D2 >= 0 Then
    Begin
      D3 := gts_point_orientation_int(X3,Y3,X1,Y1,X,Y);
      If D3 >= 0 Then
      Begin
        If (D1 = 0) Or (D2 = 0) Or (D3 = 0)
         Then Result := gtsOn
         Else Result := gtsIn;
      End;
    End;
  End;
}
End; // gts_point_is_in_triangle_int

(**
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
Function gts_segment_triangle_intersection(S1,S2,T1,T2,T3: T3DPoint{I3dPoint}; Boundary: Boolean; Intersection: T3DPoint{I3dPoint}): Boolean;
Var
  A,B,C,D,E                       : T3DPoint{I3dPoint};
  ABCE,ABCD,ADCE,ABDE,BCDE,CC,Tmp : Double;

Begin
  A      := T1;
  B      := T2;
  C      := T3;
  D      := S1;
  E      := S2;
  Result := False;
  ABCE   := gts_point_orientation_3d(A, B, C, E);
  ABCD   := gts_point_orientation_3d(A, B, C, D);
  If (ABCE < 0) Or (ABCD > 0) Then
  Begin
    D    := S2;
    E    := S1;
    Tmp  := ABCE;
    ABCE := ABCD;
    ABCD := Tmp;
  End;

  If (ABCE < 0) Or (ABCD > 0) Then Exit;

  ADCE := gts_point_orientation_3d(A, D, C, E);
  If (Boundary And (ADCE < 0)) Or ((ADCE <= 0) And Not Boundary) Then Exit;

  ABDE := gts_point_orientation_3d(A, B, D, E);
  If (Boundary And (ABDE < 0)) Or ((ABDE <= 0) And Not Boundary) Then Exit;

  BCDE := gts_point_orientation_3d(B, C, D, E);
  If (Boundary And (BCDE < 0)) Or ((BCDE <= 0) And Not Boundary) Then Exit;

  If ABCE = 0 Then
  Begin
    // is s contained in the plane defined by t?

    If ABCD = 0 Then Exit;
    Intersection.Copy(E);
    Result := True;
    Exit;
  End;
  If ABCD = 0 Then
  Begin
    Intersection.Copy(D);
    Result := True;
    Exit;
  End;
  If Boundary Then  // corners of @t
  Begin
    If ABDE = 0 Then
    Begin
      If ADCE = 0 Then
      Begin
        Intersection.Copy(A);
        Result := True;
        Exit;
      End;
      If BCDE = 0 Then
      Begin
        Intersection.Copy(B);
        Result := True;
        Exit;
      End;
    End
    Else If (BCDE = 0) And (ADCE = 0) Then
    Begin
      Intersection.Copy(C);
      Result := True;
      Exit;
    End;
  End;
  CC := ABCE / (ABCE - ABCD);
  Intersection.Copy(E.X + CC * (D.X - E.X),
                    E.Y + CC * (D.Y - E.Y),
                    E.Z + CC * (D.Z - E.Z));
  Result := True;
End; // gts_segment_triangle_intersection

Function check_intersect_tri(P1,P2,P3,Norm,LinePt,Vect,Intersection: T3DPoint{I3dPoint}): Boolean;
// Checks for an intersection between the triangle defined by P1-P3 and the infinite line defined by LinePt and Vect.
// Returns True if they intersect and fills in Intersection.
Var
  V1X, V1Y, V1Z : Single;
  V2X, V2Y, V2Z : Single;
  DotProd       : Single;
  T             : Single;
  IX,IY,IZ      : Single;

  Function check_same_clock_dir(P1,P2: T3DPoint{I3dPoint}; IX,IY,IZ: Single; {P3,}Norm: T3DPoint{I3dPoint}): Boolean;
  Var I,J,K,DotProd: Single;
  Begin
    // normal of trinagle

    I := (((P2.Y - P1.Y) * (IZ{P3.Z} - P1.Z)) - ((IY{P3.Y} - P1.Y) * (P2.Z - P1.Z)));
    J := (((P2.Z - P1.Z) * (IX{P3.X} - P1.X)) - ((IZ{P3.Z} - P1.Z) * (P2.X - P1.X)));
    K := (((P2.X - P1.X) * (IY{P3.Y} - P1.Y)) - ((IX{P3.X} - P1.X) * (P2.Y - P1.Y)));

    // Dot product with triangle normal

    DotProd := Norm.Dot(I,J,K);

    //answer

    Result := (DotProd >= 0);
  End; // check_same_clock_dir

Begin
   // vector form triangle pt1 to pt2

   V1X := P2.X - P1.X;
   V1Y := P2.Y - P1.Y;
   V1Z := P2.Z - P1.Z;

   // vector form triangle pt2 to pt3

   V2X := P3.X - P2.X;
   V2Y := P3.Y - P2.Y;
   V2Z := P3.Z - P2.Z;

   // dot product of normal and line's vector if zero line is parallel to triangle

   DotProd := Norm.Dot(Vect);
   Result  := False;
   If DotProd < 0 Then
   Begin
     // Find point of intersect to triangle plane.
     // Find t to intersect point

     T := -(Norm.X * (LinePt.X - P1.X) + Norm.Y * (LinePt.Y - P1.Y) + Norm.Z * (LinePt.Z - P1.Z)) /
           (Norm.X * Vect.X + Norm.Y * Vect.Y + Norm.Z * Vect.Z);

     // if ds is neg line started past triangle so can't hit triangle.

     If T >= 0 Then
     Begin
       IX := LinePt.X + Vect.X * T;
       IY := LinePt.Y + Vect.Y * T;
       IZ := LinePt.Z + Vect.Z * T;

       If check_same_clock_dir(P1, P2, IX,IY,IZ{pt_int}, Norm) Then
       Begin
         If check_same_clock_dir(P2, P3, IX,IY,IZ{pt_int}, Norm) Then
         Begin
           If check_same_clock_dir(P3, P1, IX,IY,IZ{pt_int}, Norm) Then
           Begin
             // answer in pt_int is insde triangle

             Result := True;
             Intersection.Copy(IX,IY,IZ);
           End;
         End;
       End;
     End;
   End;
End; // check_intersect_tri

// Determine whether or not the line segment p1,p2
// Intersects the 3 vertex facet bounded by pa,pb,pc
// Return the intersection point in intersect if there is one or
// leaves it unmodified otherwise.  Returns true if an intersection
// was found.
//
// The equation of the line is p = p1 + mu (p2 - p1)
// The equation of the plane is a x + b y + c z + d = 0
//                              n.x x + n.y y + n.z z + d = 0

Function LineFacet(P1,P2,PA,PB,PC,Intersect: T3DPoint{I3dPoint}): Boolean;
Const
  Epsilon1 = 0.01;
  Epsilon2 = 0.1;

Var
  D        : Double;
  A1,A2,A3 : Double;
  Total    : Double;
  Denom    : Double;
  Mu       : Double;
  N        : T3DPoint{I3dPoint};
  PA1      : T3DPoint{I3dPoint};
  PA2      : T3DPoint{I3dPoint};
  PA3      : T3DPoint{I3dPoint};
  P        : T3DPoint{I3dPoint};

  PBA      : T3DPoint{I3dPoint};
  PCA      : T3DPoint{I3dPoint};
  P21      : T3DPoint{I3dPoint};

Begin
  Result := False;

  // Calculate the parameters for the plane

  PBA := T3DPoint.Create{Point}(PB);
  PCA := T3DPoint.Create{Point}(PC);
  PBA.Subtract(PA);
  PCA.Subtract(PA);

  N   := T3DPoint.Create{Point}(PBA);
  N.Cross(PCA);
  N.Normalize;

  D := {-}N.Dot(PA);

  // Calculate the position on the line that intersects the plane

  P21 := T3DPoint.Create{Point}(P2);
  P21.Subtract(P1);

  Denom := N.Dot(P21);

  // Do the line and plane intersect?

  If Abs(Denom) >= Epsilon1 Then
  Begin
//    Mu := -(D + N.Dot(P1)) / Denom;
    Mu := (D - N.Dot(P1)) / Denom;

    // Is the intersection along the line segment?

    If (Mu >= 0) And (Mu <= 1) Then
    Begin
      P := T3DPoint.Create{Point}(P21);
      P.Multiply(Mu);
      P.Add(P1);

      // Determine whether or not the intersection point is bounded by pa,pb,pc

      PA1 := T3DPoint.Create{Point}(PA);
      PA1.Subtract(P);
      PA1.Normalize;

      PA2 := T3DPoint.Create{Point}(PB);
      PA2.Subtract(P);
      PA2.Normalize;

      PA3 := T3DPoint.Create{Point}(PC);
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

      If Abs((2 * Pi) - Total) < Epsilon2 Then
//      If Total < 2 * Pi + Epsilon
      Begin
        Intersect.Copy(P);
        Result := True;
      End;
      P.Free;
      PA1.Free;
      PA2.Free;
      PA3.Free;
{
      P   := Nil;
      PA1 := Nil;
      PA2 := Nil;
      PA3 := Nil;
}
    End;
  End;

  // Cleanup

  PBA.Free;
  PCA.Free;
  N.Free;
  P21.Free;
{
  PBA := Nil;
  PCA := Nil;
  N   := Nil;
  P21 := Nil;
}  
End; // LineFacet

Function LinePlaneIntersection(L1,L2,P1,P2,P3: T3DPoint{I3dPoint}): T3DPoint{I3dPoint};
// --------------------------------------------------------------------------
// Returns the intersection between a line passing through L1 and L2 with an
// infinite plane represented by P1, P2, and P3.  Returns Nil if the
// intersection can't be computed.
// --------------------------------------------------------------------------
Var
  MNum,MDen : T4x4Matrix;
  Num,Den   : Double;
  T         : Double;
  P         : T3DPoint{I3dPoint};

Begin
  Result := Nil;
  MDen   := T4x4Matrix.Create(1,1,1,0, P1.X,P2.X,P3.X,L2.X - L1.X, P1.Y,P2.Y,P3.Y,L2.Y - L1.Y, P1.Z,P2.Z,P3.Z,L2.Z - L1.Z);
  Den    := MDen.Determinant;
  If Den <> 0 Then
  Begin
    MNum   := T4x4Matrix.Create(1,1,1,1, P1.X,P2.X,P3.X,L1.X, P1.Y,P2.Y,P3.Y,L1.Y, P1.Z,P2.Z,P3.Z,L1.Z);
    Num    := MNum.Determinant;
    T      := Num / Den;
    MNum.Free;

    // The intersection point is L1 + (L1 - L2) * t

    P      := T3DPoint.Create{Point}(L2,L1);
    P.Multiply(T);
    P.Add(L1);
    Result := P;
  End;
  MDen.Free;
End; // LinePlaneIntersection

// -----------------------------
// T3DPoint
// -----------------------------

Constructor T3DPoint.Create;
Begin
  FX     := 0;
  FY     := 0;
  FZ     := 0;
//  FW     := 0;
  FDirty := True;
End; // T3DPoint.Create

Constructor T3DPoint.Create(AX,AY,AZ: Single);
Begin
  FX     := AX;
  FY     := AY;
  FZ     := AZ;
//  FW     := 0;
  FDirty := True;
End; // T3DPoint.Create

Constructor T3DPoint.Create(P: T3DPoint{I3dPoint});
Begin
  FX     := P.X;
  FY     := P.Y;
  FZ     := P.Z;
//  FW     := 0;
  FDirty := True;
End; // T3DPoint.Create

Constructor T3DPoint.Create(P1,P2: T3DPoint{I3dPoint});
Begin
  FX     := P2.X - P1.X;
  FY     := P2.Y - P1.Y;
  FZ     := P2.Z - P1.Z;
//  FW     := 0;
  FDirty := True;
End; // T3DPoint.Create

Procedure T3DPoint.SetToZero;
Begin
  FX    := 0;
  FY    := 0;
  FZ    := 0;
  FDirty := True;
End; // T3DPoint.SetToZero

Procedure T3DPoint.glTranslate;
Begin
  glTranslatef(FX,FY,FZ);
End; // T3DPoint.glTranslate

Procedure T3DPoint.glRotate;
Begin
  glRotatef(FX, 1.0, 0.0, 0.0);
  glRotatef(FY, 0.0, 1.0, 0.0);
  glRotatef(FZ, 0.0, 0.0, 1.0);
End; // T3DPoint.glRotate

Procedure T3DPoint.glScale;
Begin
  glScalef(FX,FY,FZ);
End; // T3DPoint.glScale

Procedure T3DPoint.MakeAbsolute;
Begin
  FX := Abs(FX);
  FY := Abs(FY);
  FZ := Abs(FZ);
End; // T3DPoint.MakeAbsolute

Procedure T3DPoint.Average(P1,P2: T3DPoint{I3dPoint});
Begin
  FX     := (P2.X + P1.X) / 2;
  FY     := (P2.Y + P1.Y) / 2;
  FZ     := (P2.Z + P1.Z) / 2;
  FDirty := True;
End; // T3DPoint.Average

Function T3DPoint.AbsMaximum: Single;
Begin
  Result := Max(Max(Abs(FX),Abs(FY)),Abs(FZ));
End; // T3DPoint.AbsMaximum

Function T3DPoint.Maximum: Single;
Begin
  Result := Max(Max(FX,FY),FZ);
End; // T3DPoint.Maximum

Function T3DPoint.IsUpOnly: Boolean;
Begin
  Result := (Abs(FX) = 0{< 0.1}) And (Abs(FY) = 0{< 0.1}) And (FZ > 0);
End; // T3DPoint.IsUpOnly

Function T3DPoint.NoneAreZero: Boolean;
Begin
  Result := (FX <> 0) And (FY <> 0) And (FZ <> 0);
End; // T3DPoint.NoneAreZero

Function T3DPoint.CheckPointInTriangle(PA,PB,PC: T3DPoint{I3dPoint}): Boolean;
Var
  E10X,E10Y,E10Z : Single;
  E20X,E20Y,E20Z : Single;
  VPX,VPY,VPZ    : Single;
  AC_BB          : Single;
  A,B,C,D,E      : Single;
  XX,YY,ZZ       : Single;
  IX,IY,IZ       : ^LongWord;

  PPA,PPB,PPC,PPD : Pointer;

Begin
{  If EnableSSE3 Then
  Begin
    PPA := @PA.FX;
    PPB := @PB.FX;
    PPC := @PC.FX;
    PPD := @FX;
    Asm
      MOV      EAX,DWORD PTR PPA
      MOV      EDX,DWORD PTR PPB
      MOV      ECX,DWORD PTR PPC
      MOVUPS   XMM0,DQWORD PTR [EAX]    // XMM0 = PA
      MOVUPS   XMM1,DQWORD PTR [EDX]    // XMM1 = PB
      MOVUPS   XMM2,DQWORD PTR [ECX]    // XMM2 = PC
      SUBPS    XMM1,XMM0                // XMM1 = E10 (was PB)
      SUBPS    XMM2,XMM0                // XMM2 = E20 (was PC)

      MOV      EAX,DWORD PTR PPD
      MOVUPS   XMM3,DQWORD PTR [EAX]
      SUBPS    XMM3,XMM0
      MOVAPS   XMM0,XMM3                // XMM0 = VP

      MOVAPS   XMM3,XMM1                // XMM3 = A (eventually)
      MOVAPS   XMM4,XMM1                // XMM4 = B (eventually)
      MOVAPS   XMM5,XMM2                // XMM5 = C (eventually)
      MULPS    XMM3,XMM1
      MULPS    XMM4,XMM2
      MULPS    XMM5,XMM2
      DB       0F2h,0Fh,7Ch,11011011b //  HADDPS   XMM3,XMM3
      DB       0F2h,0Fh,7Ch,11011011b //  HADDPS   XMM3,XMM3
      DB       0F2h,0Fh,7Ch,11100100b //  HADDPS   XMM4,XMM4
      DB       0F2h,0Fh,7Ch,11100100b //  HADDPS   XMM4,XMM4
      DB       0F2h,0Fh,7Ch,11101101b //  HADDPS   XMM5,XMM5
      DB       0F2h,0Fh,7Ch,11101101b //  HADDPS   XMM5,XMM5

      MOVSS    XMM6,XMM3
      MULSS    XMM6,XMM5
      MOVSS    XMM7,XMM4
      MULSS    XMM7,XMM7
      SUBSS    XMM7,XMM6                // XMM7 = -AC_BB

      MOVAPS   XMM6,XMM0
      MULPS    XMM6,XMM1                // XMM6 = D (eventually)
      MULPS    XMM0,XMM2                // XMM0 = E (eventually) (was VP)
      DB       0F2h,0Fh,7Ch,11110110b //  HADDPS   XMM6,XMM6
      DB       0F2h,0Fh,7Ch,11110110b //  HADDPS   XMM6,XMM6
      DB       0F2h,0Fh,7Ch,11000000b //  HADDPS   XMM0,XMM0
      DB       0F2h,0Fh,7Ch,11000000b //  HADDPS   XMM0,XMM0

      MOVSS    XMM1,XMM6
      MULSS    XMM1,XMM5
      MOVSS    XMM2,XMM0
      MULSS    XMM2,XMM4
      SUBSS    XMM1,XMM2                // XMM1 = XX (was E10)

      MOVSS    XMM2,XMM0
      MULSS    XMM2,XMM3
      MOVSS    XMM5,XMM6
      MULSS    XMM5,XMM4
      SUBSS    XMM2,XMM5                // XMM2 = YY (was E20)

      ADDSS    XMM7,XMM1
      ADDSS    XMM7,XMM2                // XMM7 = ZZ

      MOVSS    DWORD PTR XX,XMM1
      MOVSS    DWORD PTR YY,XMM2
      MOVSS    DWORD PTR ZZ,XMM7
    End;
    IX     := @XX;
    IY     := @YY;
    IZ     := @ZZ;
    Result := (((IZ^ And (Not (IX^ Or IY^))) And $80000000) <> 0);
  End
  Else}
  Begin
    E10X   := PB.X - PA.X;
    E10Y   := PB.Y - PA.Y;
    E10Z   := PB.Z - PA.Z;
    E20X   := PC.X - PA.X;
    E20Y   := PC.Y - PA.Y;
    E20Z   := PC.Z - PA.Z;
    A      := E10X * E10X + E10Y * E10Y + E10Z * E10Z;
    B      := E10X * E20X + E10Y * E20Y + E10Z * E20Z;
    C      := E20X * E20X + E20Y * E20Y + E20Z * E20Z;
    AC_BB  := (A * C) - (B * B);
    VPX    := FX - PA.X;
    VPY    := FY - PA.Y;
    VPZ    := FZ - PA.Z;
    D      := VPX * E10X + VPY * E10Y + VPZ * E10Z;
    E      := VPX * E20X + VPY * E20Y + VPZ * E20Z;
    XX     := (D * C) - (E * B);
    YY     := (E * A) - (D * B);
    ZZ     := XX + YY - AC_BB;
    IX     := @XX;
    IY     := @YY;
    IZ     := @ZZ;
    Result := (((IZ^ And (Not (IX^ Or IY^))) And $80000000) <> 0);
  End;
End; // T3DPoint.CheckPointInTriangle

Function T3DPoint.Normalize: Single;
Const
  Small = 1E-5;
  Large = 1E8;

Var Len,X1,Y1,Z1: Extended;
Begin
  Len := Sqr(FX) + Sqr(FY) + Sqr(FZ);
  If Len > 0 Then
  Begin
    Len := Sqrt(Len);
    X1  := FX / Len;
    Y1  := FY / Len;
    Z1  := FZ / Len;
    If Abs(X1) < Small Then X1 := 0;
    If Abs(Y1) < Small Then Y1 := 0;
    If Abs(Z1) < Small Then Z1 := 0;
    If X1 >  Large Then X1 :=  Large;
    If Y1 >  Large Then Y1 :=  Large;
    If Z1 >  Large Then Z1 :=  Large;
    If X1 < -Large Then X1 := -Large;
    If Y1 < -Large Then Y1 := -Large;
    If Z1 < -Large Then Z1 := -Large;
    FX := X1;
    FY := Y1;
    FZ := Z1;
    FDirty := True;
  End;
  Result := Len;
End; // T3DPoint.Normalize

Function T3DPoint.Normalize(NormResult: TNormResult): Single;
Const
  Small = 1E-5;
  Large = 1E8;

Var Len,X1,Y1,Z1: Extended;
Begin
  Len := Sqr(FX) + Sqr(FY) + Sqr(FZ);
  If Len > 0 Then
  Begin
    Len := Sqrt(Len);
    X1  := FX / Len;
    Y1  := FY / Len;
    Z1  := FZ / Len;
    If Abs(X1) < Small Then X1 := 0;
    If Abs(Y1) < Small Then Y1 := 0;
    If Abs(Z1) < Small Then Z1 := 0;
    If X1 >  Large Then X1 :=  Large;
    If Y1 >  Large Then Y1 :=  Large;
    If Z1 >  Large Then Z1 :=  Large;
    If X1 < -Large Then X1 := -Large;
    If Y1 < -Large Then Y1 := -Large;
    If Z1 < -Large Then Z1 := -Large;
    FX := X1;
    FY := Y1;
    FZ := Z1;
    FDirty := True;
  End;
  Case NormResult Of
    nrLength: Result := Len;
    nrX:      Result := FX;
    nrY:      Result := FY;
    nrZ:      Result := FZ;
  End; // Case
End; // T3DPoint.Normalize

Procedure T3DPoint.Negate;
Begin
  FX := -FX;
  FY := -FY;
  FZ := -FZ;
  FDirty := True;
End; // T3DPoint.Negate

Function T3DPoint.IsZero: Boolean;
Begin
  Result := ((FX = 0) And (FY = 0) And (FZ = 0));
End; // T3DPoint.IsZero

Function T3DPoint.IsOne: Boolean;
Begin
  Result := ((FX = 1) And (FY = 1) And (FZ = 1));
End; // T3DPoint.IsOne

Procedure T3DPoint.Add(P: T3DPoint{I3dPoint});
Begin
  FX := FX + P.X;
  FY := FY + P.Y;
  FZ := FZ + P.Z;
  FDirty := True;
End; // T3DPoint.Add

Procedure T3DPoint.Add(P: PSingleArray);
Begin
  FX := FX + P^[0];
  FY := FY + P^[1];
  FZ := FZ + P^[2];
  FDirty := True;
End; // T3DPoint.Add

Procedure T3DPoint.Add(AX,AY,AZ: Single);
Begin
  FX := FX + AX;
  FY := FY + AY;
  FZ := FZ + AZ;
  FDirty := True;
End; // T3DPoint.Add

Procedure T3DPoint.Subtract(P: T3DPoint{I3dPoint});
Begin
  FX := FX - P.X;
  FY := FY - P.Y;
  FZ := FZ - P.Z;
  FDirty := True;
End; // T3DPoint.Subtract

Procedure T3DPoint.Subtract(AX,AY,AZ: Single);
Begin
  FX := FX - AX;
  FY := FY - AY;
  FZ := FZ - AZ;
  FDirty := True;
End; // T3DPoint.Subtract

Procedure T3DPoint.Multiply(S: Single);
Begin
  FX := FX * S;
  FY := FY * S;
  FZ := FZ * S;
  FDirty := True;
End; // T3DPoint.Multiply

Procedure T3DPoint.Multiply(AX,AY,AZ: Single);
Begin
  FX := FX * AX;
  FY := FY * AY;
  FZ := FZ * AZ;
  FDirty := True;
End; // T3DPoint.Multiply

Procedure T3DPoint.Multiply(P: T3DPoint{I3dPoint});
Begin
  FX := FX * P.X;
  FY := FY * P.Y;
  FZ := FZ * P.Z;
  FDirty := True;
End; // T3DPoint.Multiply

Procedure T3DPoint.Divide(S: Single);
Begin
  If S <> 0 Then
  Begin
    FX := FX / S;
    FY := FY / S;
    FZ := FZ / S;
    FDirty := True;
  End;
End; // T3DPoint.Divide

Procedure T3DPoint.Divide(AX,AY,AZ: Single);
Begin
  If AX <> 0 Then FX := FX / AX;
  If AY <> 0 Then FY := FY / AY;
  If AZ <> 0 Then FZ := FZ / AZ;
  FDirty := True;
End; // T3DPoint.Divide

Procedure T3DPoint.Divide(P: T3DPoint{I3dPoint});
Begin
  If P.X <> 0 Then FX := FX / P.X;
  If P.Y <> 0 Then FY := FY / P.Y;
  If P.Z <> 0 Then FZ := FZ / P.Z;
  FDirty := True;
End; // T3DPoint.Divide

Function T3DPoint.Equals(P: T3DPoint{I3dPoint}): Boolean;
Begin
  Result := (FX = P.X) And (FY = P.Y) And (FZ = P.Z);
End; // T3DPoint.Equals

Function T3DPoint.Equals(Const AX,AY,AZ: Single): Boolean;
Begin
  Result := (FX = AX) And (FY = AY) And (FZ = AZ);
End; // T3DPoint.Equals

Procedure T3DPoint.Copy(P: T3DPoint{I3dPoint});
Begin
  FX := P.X;
  FY := P.Y;
  FZ := P.Z;
  FDirty := True;
End; //  T3DPoint.Copy

Procedure T3DPoint.Copy(AX,AY,AZ: Single);
Begin
  FX := AX;
  FY := AY;
  FZ := AZ;
  FDirty := True;
End; // T3DPoint.Copy

Procedure T3DPoint.Copy(P: PSingleArray);
Begin
  FX := P^[0];
  FY := P^[1];
  FZ := P^[2];
  FDirty := True;  
End; // T3DPoint.Copy

Procedure T3DPoint.Copy(P1,P2: T3DPoint{I3dPoint});
Begin
  FX := P2.X - P1.X;
  FY := P2.Y - P1.Y;
  FZ := P2.Z - P1.Z;
  FDirty := True;
End; //  T3DPoint.Copy
{
Procedure T3DPoint.Copy(P1,P2: P3DPointPointer);
Begin
  FX := P2.X - P1.X;
  FY := P2.Y - P1.Y;
  FZ := P2.Z - P1.Z;
  FDirty := True;
End; //  T3DPoint.Copy
}
Function T3DPoint.ParseTokens(S1,S2,S3: String): Boolean;
Var I,J,K: Integer;
Begin
  Val(S1,FX,I);
  Val(S2,FY,J);
  Val(S3,FZ,K);
  Result := (I = 0) And (J = 0) And (K = 0);
End; // T3DPoint.ParseTokens

Function T3DPoint.Dot(P: T3DPoint{I3dPoint}): Single;
Begin
  Result := FX * P.X + FY * P.Y + FZ * P.Z;
End; // T3DPoint.Dot
{
Function T3DPoint.Dot(P: P3DPointPointer): Single;
Begin
  Result := FX * P.X + FY * P.Y + FZ * P.Z;
End; // T3DPoint.Dot
}
Function T3DPoint.Dot(Var P: TSimplePoint): Single;
Begin
  Result := FX * P.X + FY * P.Y + FZ * P.Z;
End; // T3DPoint.Dot

Function T3DPoint.Dot(AX,AY,AZ: Single): Single;
Begin
  Result := FX * AX + FY * AY + FZ * AZ;
End; // T3DPoint.Dot

Procedure T3DPoint.Cross(P: T3DPoint{I3dPoint});
Var X1,Y1: Single;
Begin
  X1 := FY * P.Z - FZ * P.Y;
  Y1 := FZ * P.X - FX * P.Z;
  FZ := FX * P.Y - FY * P.X;
  FX := X1;
  FY := Y1;
  FDirty := True;
End; // T3DPoint.Cross

Procedure T3DPoint.CounterClockwiseRotate(P: T3DPoint{I3dPoint}; RX,RY,RZ: Boolean; ReverseOrder: Boolean);
// Assumes that P contains angle amounts in radians.
Var A: Single;
Begin
  If ReverseOrder Then
  Begin
    If RZ Then
    Begin
      // Rotate around Z axis

      A  := Cos(P.Z) * FX - Sin(P.Z) * FY;
      FY := Sin(P.Z) * FX + Cos(P.Z) * FY;
      FX := A;
    End;

    If RY Then
    Begin
      // Rotate around Y axis

      A  := Cos(P.Y) * FZ - Sin(P.Y) * FX;
      FX := Sin(P.Y) * FZ + Cos(P.Y) * FX;
      FZ := A;
    End;

    If RX Then
    Begin
      // Rotate around X axis

      A  := Cos(P.X) * FY - Sin(P.X) * FZ;
      FZ := Sin(P.X) * FY + Cos(P.X) * FZ;
      FY := A;
    End;
  End
  Else
  Begin
    If RX Then
    Begin
      // Rotate around X axis

      A  := Cos(P.X) * FY - Sin(P.X) * FZ;
      FZ := Sin(P.X) * FY + Cos(P.X) * FZ;
      FY := A;
    End;

    If RY Then
    Begin
      // Rotate around Y axis

      A  := Cos(P.Y) * FZ - Sin(P.Y) * FX;
      FX := Sin(P.Y) * FZ + Cos(P.Y) * FX;
      FZ := A;
    End;

    If RZ Then
    Begin
      // Rotate around Z axis

      A  := Cos(P.Z) * FX - Sin(P.Z) * FY;
      FY := Sin(P.Z) * FX + Cos(P.Z) * FY;
      FX := A;
    End;
  End;
  FDirty := True;
End; // T3DPoint.CounterClockwiseRotate

Procedure T3DPoint.ClockwiseRotate(P: T3DPoint{I3dPoint}; RX,RY,RZ: Boolean; ReverseOrder: Boolean);
// Assumes that P contains angle amounts in radians.
Var A: Single;
Begin
  If ReverseOrder Then
  Begin
    If RZ Then
    Begin
      // Rotate around Z axis

      A  := Cos(-P.Z) * FX - Sin(-P.Z) * FY;
      FY := Sin(-P.Z) * FX + Cos(-P.Z) * FY;
      FX := A;
    End;

    If RY Then
    Begin
      // Rotate around Y axis

      A  := Cos(-P.Y) * FZ - Sin(-P.Y) * FX;
      FX := Sin(-P.Y) * FZ + Cos(-P.Y) * FX;
      FZ := A;
    End;

    If RX Then
    Begin
      // Rotate around X axis

      A  := Cos(-P.X) * FY - Sin(-P.X) * FZ;
      FZ := Sin(-P.X) * FY + Cos(-P.X) * FZ;
      FY := A;
    End;
  End
  Else
  Begin
    If RX Then
    Begin
      // Rotate around X axis

      A  := Cos(-P.X) * FY - Sin(-P.X) * FZ;
      FZ := Sin(-P.X) * FY + Cos(-P.X) * FZ;
      FY := A;
    End;

    If RY Then
    Begin
      // Rotate around Y axis

      A  := Cos(-P.Y) * FZ - Sin(-P.Y) * FX;
      FX := Sin(-P.Y) * FZ + Cos(-P.Y) * FX;
      FZ := A;
    End;

    If RZ Then
    Begin
      // Rotate around Z axis

      A  := Cos(-P.Z) * FX - Sin(-P.Z) * FY;
      FY := Sin(-P.Z) * FX + Cos(-P.Z) * FY;
      FX := A;
    End;
  End;
  FDirty := True;
End; // T3DPoint.ClockwiseRotate

(*
Procedure T3DPoint.Rotate(P: T3DPoint{I3dPoint}; RX,RY,RZ: Boolean);
// Rotates clockwise.  Assumes that P contains angle amounts in radians.
Var A: Single;
Begin
  If RX Then
  Begin
    // Rotate around X axis

    A :=  Cos(P.X) * Y + Sin(P.X) * Z;
    Z := -Sin(P.X) * Y + Cos(P.X) * Z;
    Y := A;
  End;

  If RY Then
  Begin
    // Rotate around Y axis

    A := Cos(P.Y) * X - Sin(P.Y) * Z;
    Z := Sin(P.Y) * X + Cos(P.Y) * Z;
    X := A;
  End;

  If RZ Then
  Begin
    // Rotate around Z axis

    A :=  Cos(P.Z) * X + Sin(P.Z) * Y;
    Y := -Sin(P.Z) * X + Cos(P.Z) * Y;
    X := A;
  End;
  FDirty := True;
End; // T3DPoint.Rotate

Procedure T3DPoint.NegativeRotate(P: T3DPoint{I3dPoint}; RX,RY,RZ: Boolean);
// Rotates counterclockwise.  Assumes that P contains angle amounts in radians.
Var A: Single;
Begin
  If RX Then
  Begin
    // Rotate around X axis

    A :=  Cos(-P.X) * Y + Sin(-P.X) * Z;
    Z := -Sin(-P.X) * Y + Cos(-P.X) * Z;
    Y := A;
  End;

  If RY Then
  Begin
    // Rotate around Y axis

    A := Cos(-P.Y) * X - Sin(-P.Y) * Z;
    Z := Sin(-P.Y) * X + Cos(-P.Y) * Z;
    X := A;
  End;

  If RZ Then
  Begin
    // Rotate around Z axis

    A :=  Cos(-P.Z) * X + Sin(-P.Z) * Y;
    Y := -Sin(-P.Z) * X + Cos(-P.Z) * Y;
    X := A;
  End;
  FDirty := True;
End; // T3DPoint.NegativeRotate
*)
Procedure T3DPoint.Exchange(P: T3DPoint{I3dPoint});
Var AX,AY,AZ: Single;
Begin
  AX := FX;
  AY := FY;
  AZ := FZ;
  Copy(P);
  P.X := AX;
  P.Y := AY;
  P.Z := AZ;
  FDirty := True;
End; // T3DPoint.Exchange

Function T3DPoint.GetLength: Single;
Var D: Single;
Begin
  D := Sqr(FX) + Sqr(FY) + Sqr(FZ);
  If D > 0 Then Result := Sqrt(D) Else Result := 0;
End; // T3DPoint.GetLength

Function T3DPoint.GetSquaredLength: Single;
Begin
  Result := Sqr(FX) + Sqr(FY) + Sqr(FZ);
End; // T3DPoint.GetSquaredLength

Function T3DPoint.DistanceFrom(P: T3DPoint{I3dPoint}): Single;
Var D: Single;
Begin
  D := Sqr(FX - P.X) + Sqr(FY - P.Y) + Sqr(FZ - P.Z);
  If D > 0 Then Result := Sqrt(D) Else Result := 0;
End; // T3DPoint.DistanceFrom

Function T3DPoint.DistanceFrom(CX,CY,CZ: Single): Single;
Var D: Single;
Begin
  D := Sqr(FX - CX) + Sqr(FY - CY) + Sqr(FZ - CZ);
  If D > 0 Then Result := Sqrt(D) Else Result := 0;
End; // T3DPoint.DistanceFrom

Function T3DPoint.DistanceFrom2(P: T3DPoint{I3dPoint}): Single;
Begin
  Result := Sqr(FX - P.X) + Sqr(FY - P.Y) + Sqr(FZ - P.Z);
End; // T3DPoint.DistanceFrom2

Function T3DPoint.DistanceFrom2(CX,CY,CZ: Single): Single;
Begin
  Result := Sqr(FX - CX) + Sqr(FY - CY) + Sqr(FZ - CZ);
End; // T3DPoint.DistanceFrom2

Function T3DPoint.DistanceFromLine2(P1,P2: T3DPoint{I3dPoint}): Single;
// Returns the square of the distance from the line passing through P1 and P2
Var
  P3,P4 : T3DPoint{I3dPoint};
  Len2  : Single;

Begin
  P3   := T3DPoint.Create{Point}(P1,P2);
  Len2 := P3.GetSquaredLength;
  If Len2 <> 0 Then
  Begin
    P4 := T3DPoint.Create{Point}(Self,P1);
    P3.Cross(P4);
//    P4.Free;
    Result := P3.GetSquaredLength / Len2;
  End
  Else Result := DistanceFrom2(P1);
//  P3.Free;
End; // T3DPoint.DistanceFromLine2

Function T3DPoint.DistanceFromRay2(P1,Direction: T3DPoint{I3dPoint}): Single;
// Returns the square of the distance from the line passing through P1 and pointing along Direction
Var
  P3,P4 : T3DPoint{I3dPoint};
  Len2  : Single;

Begin
  P3   := T3DPoint.Create{Point}(Direction);
  Len2 := P3.GetSquaredLength;
  If Len2 <> 0 Then
  Begin
    P4 := T3DPoint.Create{Point}(Self,P1);
    P3.Cross(P4);
//    P4.Free;
    Result := P3.GetSquaredLength / Len2;
  End
  Else Result := DistanceFrom2(P1);
//  P3.Free;
End; // T3DPoint.DistanceFromRay2

Function T3DPoint.GetID: String;
Begin
  Result := FloatToStr(FX) + ',' + FloatToStr(FY) + ',' + FloatToStr(FZ);
End; // T3DPoint.GetID

Function T3DPoint.ToString: String;
Begin
  Result := '(' + Format('%8.3f',[FX]) + ', ' + Format('%8.3f',[FY]) + ', ' + Format('%8.3f',[FZ]) + ')';
End; // T3DPoint.GetID

Function T3DPoint.GetCloseID(Multiplier: Single): String;
Begin
  Result := IntToStr(Round(FX * Multiplier)) + ',' +
            IntToStr(Round(FY * Multiplier)) + ',' +
            IntToStr(Round(FZ * Multiplier));
End; // T3DPoint.GetCloseID

Procedure T3DPoint.GetNormalTo(P1,P2,P3: T3DPoint{I3dPoint});
// Efficient way to get the normal to the plane represented by P1, P2, and P3
// (P1 - P2) x (P3 - P2)
Begin
  FX := (P1.Y - P2.Y) * (P3.Z - P2.Z) - (P1.Z - P2.Z) * (P3.Y - P2.Y);
  FY := (P1.Z - P2.Z) * (P3.X - P2.X) - (P1.X - P2.X) * (P3.Z - P2.Z);
  FZ := (P1.X - P2.X) * (P3.Y - P2.Y) - (P1.Y - P2.Y) * (P3.X - P2.X);
  FDirty := True;
  Normalize;
End; // T3DPoint.GetNormalTo

Procedure T3DPoint.GetNormalTo(Var P1,P2,P3: TSimplePoint);
// Efficient way to get the normal to the plane represented by P1, P2, and P3
// (P1 - P2) x (P3 - P2)
Begin
  FX := (P1.Y - P2.Y) * (P3.Z - P2.Z) - (P1.Z - P2.Z) * (P3.Y - P2.Y);
  FY := (P1.Z - P2.Z) * (P3.X - P2.X) - (P1.X - P2.X) * (P3.Z - P2.Z);
  FZ := (P1.X - P2.X) * (P3.Y - P2.Y) - (P1.Y - P2.Y) * (P3.X - P2.X);
  FDirty := True;
  Normalize;
End; // T3DPoint.GetNormalTo

Procedure T3DPoint.GetParallelTo(P: T3DPoint{I3dPoint});
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

Procedure T3DPoint.GetParallelTo(P1,P2,P3: T3DPoint{I3dPoint});
// Removes any part of this ray that is perpendicular to the plane represented
// by P1, P2, and P3.
Var S1,S2: T3DPoint{I3dPoint};
Begin
  S1 := T3DPoint.Create{Point};
  S1.GetNormalTo(P1,P2,P3);
  S2 := T3DPoint.Create{Point}(Self);
  S2.GetParallelTo(S1);
  Subtract(S2);
//  S1.Free;
//  S2.Free;
End; // T3DPoint.GetParallelTo

Procedure T3DPoint.GetParallelTo(Plane: TPlane);
// Removes any part of this ray that is perpendicular to the plane
Var P: T3DPoint{I3dPoint};
Begin
  P := T3DPoint.Create{Point}(Self);
  P.GetParallelTo(Plane.Normal);
  Subtract(P);
//  P.Free;
End; // T3DPoint.GetParallelTo

Procedure T3DPoint.GetPerpendicularTo(P: T3DPoint{I3dPoint});
// Removes any part of this ray that is parallel to P
Var S: T3DPoint{I3dPoint};
Begin
  S := T3DPoint.Create{Point}(Self);
  S.GetParallelTo(P);
  Subtract(S);
//  S.Free;
End; // T3DPoint.GetPerpendicularTo

Procedure T3DPoint.GetPerpendicularTo(P1,P2,P3: T3DPoint{I3dPoint});
// Removes any part of this ray that is parallel to the plane represented by
// P1, P2, and P3.
Var S1: T3DPoint{I3dPoint};
Begin
  S1 := T3DPoint.Create{Point};
  S1.GetNormalTo(P1,P2,P3);
  GetParallelTo(S1);
//  S1.Free;
End; // T3DPoint.GetPerpendicularTo

Procedure T3DPoint.GetPerpendicularTo(Plane: TPlane);
// Removes any part of this ray that is parallel to the plane
Begin
  GetParallelTo(Plane.Normal);
End; // T3DPoint.GetPerpendicularTo

Function T3DPoint.GetSinAngleBetween(P: T3DPoint{I3dPoint}): Single;
Var P1: T3DPoint{I3dPoint};
Begin
  P1 := T3DPoint.Create{Point}(Self);
  P1.Cross(P);
  Result := P1.GetLength / (GetLength * P.GetLength);
//  P1.Free;
End; // T3DPoint.GetSinAngleBetween

Function T3DPoint.GetCosAngleBetween(P: T3DPoint{I3dPoint}): Single;
Begin
  Result := Dot(P) / (GetLength * P.GetLength);
End; // T3DPoint.GetCosAngleBetween

(*
Constructor T3DPoint.Create;
Begin
  FRegistry        := TStringList.Create;
  FRegistry.Sorted := True;
End; // T3DPoint.Create

Destructor T3DPoint.Destroy;
Var I: Integer;
Begin
  For I := 0 To FRegistry.Count - 1 Do FRegistry.Objects[I].Free;
  FRegistry.Free;
End; // T3DPoint.Destroy

Class Procedure T3DPoint.Register(Factory: T3DPoint{I3dPoint}Factory);
Begin
  PointCreator.FRegistry.AddObject(IntToStr(Factory.Priority),Pointer(Factory));
End; // T3DPoint.Register

Class Function T3DPoint.CreatePoint: T3DPoint{I3dPoint};
Var
  I         : Integer;
  CanCreate : Boolean;
  Factory   : T3DPoint{I3dPoint}Factory;

Begin
  I         := PointCreator.FRegistry.Count - 1;
  CanCreate := False;
  Factory   := Nil;
  While (I >= 0) And Not CanCreate Do
  Begin
    Factory := T3DPoint{I3dPoint}Factory(Pointer(PointCreator.FRegistry.Objects[I]));
    If Factory.CanCreate
     Then CanCreate := True
     Else Dec(I);
  End; // While
  If CanCreate And (Factory <> Nil)
   Then Result := Factory.CreatePoint
   Else Result := Nil;
End; // T3DPoint.CreatePoint

Class Function T3DPoint.Create{Point}(AX,AY,AZ: Single): T3DPoint{I3dPoint};
Var
  I         : Integer;
  CanCreate : Boolean;
  Factory   : T3DPoint{I3dPoint}Factory;

Begin
  I         := PointCreator.FRegistry.Count - 1;
  CanCreate := False;
  Factory   := Nil;
  While (I >= 0) And Not CanCreate Do
  Begin
    Factory := T3DPoint{I3dPoint}Factory(Pointer(PointCreator.FRegistry.Objects[I]));
    If Factory.CanCreate
     Then CanCreate := True
     Else Dec(I);
  End; // While
  If CanCreate And (Factory <> Nil)
   Then Result := Factory.Create{Point}(AX,AY,AZ)
   Else Result := Nil;
End; // T3DPoint.CreatePoint

Class Function T3DPoint.Create{Point}(P: T3DPoint{I3dPoint}): T3DPoint{I3dPoint};
Var
  I         : Integer;
  CanCreate : Boolean;
  Factory   : T3DPoint{I3dPoint}Factory;

Begin
  I         := PointCreator.FRegistry.Count - 1;
  CanCreate := False;
  Factory   := Nil;
  While (I >= 0) And Not CanCreate Do
  Begin
    Factory := T3DPoint{I3dPoint}Factory(Pointer(PointCreator.FRegistry.Objects[I]));
    If Factory.CanCreate
     Then CanCreate := True
     Else Dec(I);
  End; // While
  If CanCreate And (Factory <> Nil)
   Then Result := Factory.Create{Point}(P)
   Else Result := Nil;
End; // T3DPoint.CreatePoint

Class Function T3DPoint.Create{Point}(P1,P2: T3DPoint{I3dPoint}): T3DPoint{I3dPoint};
Var
  I         : Integer;
  CanCreate : Boolean;
  Factory   : T3DPoint{I3dPoint}Factory;

Begin
  I         := PointCreator.FRegistry.Count - 1;
  CanCreate := False;
  Factory   := Nil;
  While (I >= 0) And Not CanCreate Do
  Begin
    Factory := T3DPoint{I3dPoint}Factory(Pointer(PointCreator.FRegistry.Objects[I]));
    If Factory.CanCreate
     Then CanCreate := True
     Else Dec(I);
  End; // While
  If CanCreate And (Factory <> Nil)
   Then Result := Factory.Create{Point}(P1,P2)
   Else Result := Nil;
End; // T3DPoint.CreatePoint
*)
// -----------------------------
// T3x3Matrix
// -----------------------------

Constructor T3x3Matrix.Create;
Begin
  FillChar(M,SizeOf(M),0);
End; // T3x3Matrix.Create

Constructor T3x3Matrix.Create(Matrix: T3x3Matrix);
Begin
  M := Matrix.M;
End; // T3x3Matrix.Create

Constructor T3x3Matrix.Create(M11,M12,M13,M21,M22,M23,M31,M32,M33: Single);
Begin
  Copy(M11,M12,M13,M21,M22,M23,M31,M32,M33);
End; // T3x3Matrix.Create

Procedure T3x3Matrix.Copy(M11,M12,M13,M21,M22,M23,M31,M32,M33: Single);
Begin
  M[1,1] := M11;
  M[1,2] := M12;
  M[1,3] := M13;

  M[2,1] := M21;
  M[2,2] := M22;
  M[2,3] := M23;

  M[3,1] := M31;
  M[3,2] := M32;
  M[3,3] := M33;
End; // T3x3Matrix.Copy

Function T3x3Matrix.Determinant: Single;
Begin
  Result := M[1,1] * (M[2,2] * M[3,3] - M[2,3] * M[3,2]) -
            M[1,2] * (M[2,1] * M[3,3] - M[2,3] * M[3,1]) +
            M[1,3] * (M[2,1] * M[3,2] - M[2,2] * M[3,1]);
End; // T3x3Matrix.Determinant

Procedure T3x3Matrix.Multiply(P: T3DPoint{I3dPoint});
// Sets the vector to this * vector
Var X,Y,Z: Single;
Begin
  X := M[1,1] * P.X + M[1,2] * P.Y + M[1,3] * P.Z;
  Y := M[2,1] * P.X + M[2,2] * P.Y + M[2,3] * P.Z;
  Z := M[3,1] * P.X + M[3,2] * P.Y + M[3,3] * P.Z;
  P.Copy(X,Y,Z);
End; // T3x3Matrix.Multiply

Procedure T3x3Matrix.Multiply(Var X,Y,Z: Single);
// Sets the vector to this * vector
Var X1,Y1,Z1: Single;
Begin
  X1 := M[1,1] * X + M[1,2] * Y + M[1,3] * Z;
  Y1 := M[2,1] * X + M[2,2] * Y + M[2,3] * Z;
  Z1 := M[3,1] * X + M[3,2] * Y + M[3,3] * Z;
  X  := X1;
  Y  := Y1;
  Z  := Z1;
End; // T3x3Matrix.Multiply

Procedure T3x3Matrix.Multiply(Var X,Y,Z: Double);
// Sets the vector to this * vector
Var X1,Y1,Z1: Double;
Begin
  X1 := M[1,1] * X + M[1,2] * Y + M[1,3] * Z;
  Y1 := M[2,1] * X + M[2,2] * Y + M[2,3] * Z;
  Z1 := M[3,1] * X + M[3,2] * Y + M[3,3] * Z;
  X  := X1;
  Y  := Y1;
  Z  := Z1;
End; // T3x3Matrix.Multiply

Procedure T3x3Matrix.Multiply(Matrix: T3x3Matrix);
// Sets this matrix equal to M * this
Var
  Row,Col,I : Integer;
  MM        : Array[1..3,1..3] Of Single;

Begin
  For Row := 1 To 3 Do
  Begin
    For Col := 1 To 3 Do
    Begin
      MM[Row,Col] := 0;
      For I := 1 To 3 Do MM[Row,Col] := MM[Row,Col] + Matrix.M[Row,I] * M[I,Col];
    End; // For Col
  End; // For Row
  For Row := 1 To 3 Do
  Begin
    For Col := 1 To 3 Do M[Row,Col] := MM[Row,Col];
  End; // For Row
End; // T3x3Matrix.Multiply

Procedure T3x3Matrix.Multiply(S: Single);
Var Row,Col: Integer;
Begin
  For Row := 0 To 2 Do
  Begin
    For Col := 0 To 2 Do M[Row,Col] := M[Row,Col] * S;
  End; // For Row
End; // T3x3Matrix.Multiply

Procedure T3x3Matrix.Transpose;
Var
  MM      : Array[1..3,1..3] Of Single;
  Row,Col : Integer;

Begin
  For Row := 1 To 3 Do
  Begin
    For Col := 1 To 3 Do MM[Row,Col] := M[Col,Row];
  End; // For Row
  For Row := 1 To 3 Do
  Begin
    For Col := 1 To 3 Do M[Row,Col] := MM[Row,Col];
  End; // For Row
End; // T3x3Matrix.Transpose

Procedure T3x3Matrix.Negate;
Var Row,Col: Integer;
Begin
  For Row := 0 To 2 Do
  Begin
    For Col := 0 To 2 Do M[Row,Col] := -M[Row,Col];
  End; // For Row
End; // T3x3Matrix.Negate

// -----------------------------
// T3x3FixedPointMatrix
// -----------------------------

Procedure T3x3FixedPointMatrix_Multiply(Var M: T3x3FixedPointMatrix; Var X,Y,Z: Integer);
Var X1,Y1,Z1: Integer;
Begin
  Asm
    PUSH  EBX
    PUSH  ESI
    PUSH  EDI

    MOV   EBX,X
    MOV   ESI,Y
    MOV   EDI,Z
    MOV   EBX,[EBX]
    MOV   ESI,[ESI]
    MOV   EDI,[EDI]

    SUB   ECX,ECX

    MOV   EAX,M
    MOV   EAX,[EAX + 0]    // M[1,1]
    IMUL  EBX
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 4]    // M[1,2]
    IMUL  ESI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 8]    // M[1,3]
    IMUL  EDI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   X1,ECX

    SUB   ECX,ECX

    MOV   EAX,M
    MOV   EAX,[EAX + 12]   // M[2,1]
    IMUL  EBX
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 16]   // M[2,2]
    IMUL  ESI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 20]   // M[2,3]
    IMUL  EDI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   Y1,ECX

    SUB   ECX,ECX

    MOV   EAX,M
    MOV   EAX,[EAX + 24]   // M[3,1]
    IMUL  EBX
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 28]   // M[3,2]
    IMUL  ESI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 32]   // M[3,3]
    IMUL  EDI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   Z1,ECX

    POP   EDI
    POP   ESI
    POP   EBX
  End; // Asm
{
  X1 := ((M[1,1] * X) Shr 16) + ((M[1,2] * Y) Shr 16) + ((M[1,3] * Z) Shr 16);
  Y1 := ((M[2,1] * X) Shr 16) + ((M[2,2] * Y) Shr 16) + ((M[2,3] * Z) Shr 16);
  Z1 := ((M[3,1] * X) Shr 16) + ((M[3,2] * Y) Shr 16) + ((M[3,3] * Z) Shr 16);
}
  X  := X1;
  Y  := Y1;
  Z  := Z1;

End; // T3x3FixedPointMatrix_Multiply

Procedure T3x3FixedPointMatrix_Transpose(Var M: T3x3FixedPointMatrix);
Var
  MM      : Array[1..3,1..3] Of Integer;
  Row,Col : Integer;

Begin
  For Row := 1 To 3 Do
  Begin
    For Col := 1 To 3 Do MM[Row,Col] := M[Col,Row];
  End; // For Row
  For Row := 1 To 3 Do
  Begin
    For Col := 1 To 3 Do M[Row,Col] := MM[Row,Col];
  End; // For Row
End; // T3x3FixedPointMatrix_Transpose

Procedure T3x3FixedPointMatrix_Negate(Var M: T3x3FixedPointMatrix);
Var Row,Col: Integer;
Begin
  For Row := 0 To 2 Do
  Begin
    For Col := 0 To 2 Do M[Row,Col] := -M[Row,Col];
  End; // For Row
End; // T3x3FixedPointMatrix_Negate

// -----------------------------
// T4x4Matrix
// -----------------------------

Constructor T4x4Matrix.Create;
Begin
  FillChar(M,SizeOf(M),0);
End; // T4x4Matrix.Create

Constructor T4x4Matrix.Create(Matrix: T4x4Matrix);
Begin
  M := Matrix.M;
End; // T4x4Matrix.Create

Constructor T4x4Matrix.Create(M11,M12,M13,M14,M21,M22,M23,M24,M31,M32,M33,M34,M41,M42,M43,M44: Single);
Begin
  Load(M11,M12,M13,M14, M21,M22,M23,M24, M31,M32,M33,M34, M41,M42,M43,M44);
End; // T4x4Matrix.Create

Procedure T4x4Matrix.Load(M11,M12,M13,M14,M21,M22,M23,M24,M31,M32,M33,M34,M41,M42,M43,M44: Single);
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
End; // T4x4Matrix.Load

Procedure T4x4Matrix.LoadIdentity;
Begin
  M[1,1] := 1;
  M[1,2] := 0;
  M[1,3] := 0;
  M[1,4] := 0;

  M[2,1] := 0;
  M[2,2] := 1;
  M[2,3] := 0;
  M[2,4] := 0;

  M[3,1] := 0;
  M[3,2] := 0;
  M[3,3] := 1;
  M[3,4] := 0;

  M[4,1] := 0;
  M[4,2] := 0;
  M[4,3] := 0;
  M[4,4] := 1;
End; // T4x4Matrix.LoadIdentity

Procedure T4x4Matrix.LoadRotationMatrix(Matrix: T3x3Matrix);
Begin
  M[1,1] := Matrix.M[1,1];
  M[1,2] := Matrix.M[1,2];
  M[1,3] := Matrix.M[1,3];

  M[2,1] := Matrix.M[2,1];
  M[2,2] := Matrix.M[2,2];
  M[2,3] := Matrix.M[2,3];

  M[3,1] := Matrix.M[3,1];
  M[3,2] := Matrix.M[3,2];
  M[3,3] := Matrix.M[3,3];
End; // T4x4Matrix.LoadRotationMatrix

Function T4x4Matrix.Determinant: Single;
Begin
  Result := M[1,1] * (M[2,2] * (M[3,3] * M[4,4] - M[3,4] * M[4,3]) -
                      M[2,3] * (M[3,2] * M[4,4] - M[3,4] * M[4,2]) +
                      M[2,4] * (M[3,2] * M[4,3] - M[3,3] * M[4,2])) -

            M[1,2] * (M[2,1] * (M[3,3] * M[4,4] - M[3,4] * M[4,3]) -
                      M[2,3] * (M[3,1] * M[4,4] - M[3,4] * M[4,1]) +
                      M[2,4] * (M[3,1] * M[4,3] - M[3,3] * M[4,1])) +

            M[1,3] * (M[2,1] * (M[3,2] * M[4,4] - M[3,4] * M[4,2]) -
                      M[2,2] * (M[3,1] * M[4,4] - M[3,4] * M[4,1]) +
                      M[2,4] * (M[3,1] * M[4,2] - M[3,2] * M[4,1])) -

            M[1,4] * (M[2,1] * (M[3,2] * M[4,3] - M[3,3] * M[4,2]) -
                      M[2,2] * (M[3,1] * M[4,3] - M[3,3] * M[4,1]) +
                      M[2,3] * (M[3,1] * M[4,2] - M[3,2] * M[4,1]));
End; // T4x4Matrix.Determinant

Procedure T4x4Matrix.Multiply(P: T3DPoint{I3dPoint});
// Sets the vector to this * vector. Assumes that all vectors have 1 as their fourth component.
Var X,Y,Z: Single;
Begin
  X := M[1,1] * P.X + M[1,2] * P.Y + M[1,3] * P.Z + M[1,4];
  Y := M[2,1] * P.X + M[2,2] * P.Y + M[2,3] * P.Z + M[2,4];
  Z := M[3,1] * P.X + M[3,2] * P.Y + M[3,3] * P.Z + M[3,4];
  P.Copy(X,Y,Z);
End; // T3x3Matrix.Multiply

Procedure T4x4Matrix.Multiply(Var P: TSimplePoint);
// Sets the vector to this * vector. Assumes that all vectors have 1 as their fourth component.
Var X,Y,Z: Single;
Begin
  X   := M[1,1] * P.X + M[1,2] * P.Y + M[1,3] * P.Z + M[1,4];
  Y   := M[2,1] * P.X + M[2,2] * P.Y + M[2,3] * P.Z + M[2,4];
  Z   := M[3,1] * P.X + M[3,2] * P.Y + M[3,3] * P.Z + M[3,4];
  P.X := X;
  P.Y := Y;
  P.Z := Z;
End; // T3x3Matrix.Multiply

Procedure T4x4Matrix.Multiply(Var X,Y,Z: Single);
// Sets the vector to this * vector. Assumes that all vectors have 1 as their fourth component.
Var X1,Y1,Z1: Single;
Begin
  X1 := M[1,1] * X + M[1,2] * Y + M[1,3] * Z + M[1,4];
  Y1 := M[2,1] * X + M[2,2] * Y + M[2,3] * Z + M[2,4];
  Z1 := M[3,1] * X + M[3,2] * Y + M[3,3] * Z + M[3,4];
  X  := X1;
  Y  := Y1;
  Z  := Z1;
End; // T4x4Matrix.Multiply

Procedure T4x4Matrix.Multiply(Var X,Y,Z: Double);
// Sets the vector to this * vector. Assumes that all vectors have 1 as their fourth component.
Var X1,Y1,Z1: Double;
Begin
  X1 := M[1,1] * X + M[1,2] * Y + M[1,3] * Z + M[1,4];
  Y1 := M[2,1] * X + M[2,2] * Y + M[2,3] * Z + M[2,4];
  Z1 := M[3,1] * X + M[3,2] * Y + M[3,3] * Z + M[3,4];
  X  := X1;
  Y  := Y1;
  Z  := Z1;
End; // T4x4Matrix.Multiply

Procedure T4x4Matrix.Multiply(X,Y,Z: Single; Var X1,Y1,Z1: Single);
// Sets the vector to this * vector. Assumes that all vectors have 1 as their fourth component.
Var X2,Y2,Z2: Single;
Begin
  X2 := M[1,1] * X + M[1,2] * Y + M[1,3] * Z + M[1,4];
  Y2 := M[2,1] * X + M[2,2] * Y + M[2,3] * Z + M[2,4];
  Z2 := M[3,1] * X + M[3,2] * Y + M[3,3] * Z + M[3,4];
  X1 := X2;
  Y1 := Y2;
  Z1 := Z2;
End; // T4x4Matrix.Multiply

Procedure T4x4Matrix.Multiply(Matrix: T4x4Matrix);
// Sets this matrix equal to M * this
Var
  Row,Col,I : Integer;
  MM        : Array[1..4,1..4] Of Single;

Begin
  For Row := 1 To 4 Do
  Begin
    For Col := 1 To 4 Do
    Begin
      MM[Row,Col] := 0;
      For I := 1 To 4 Do MM[Row,Col] := MM[Row,Col] + Matrix.M[Row,I] * M[I,Col];
    End; // For Col
  End; // For Row
  For Row := 1 To 4 Do
  Begin
    For Col := 1 To 4 Do M[Row,Col] := MM[Row,Col];
  End; // For Row
End; // T4x4Matrix.Multiply

Procedure T4x4Matrix.Transpose;
Var
  MM      : Array[1..4,1..4] Of Single;
  Row,Col : Integer;

Begin
  For Row := 1 To 4 Do
  Begin
    For Col := 1 To 4 Do MM[Row,Col] := M[Col,Row];
  End; // For Row
  For Row := 1 To 4 Do
  Begin
    For Col := 1 To 4 Do M[Row,Col] := MM[Row,Col];
  End; // For Row
End; // T4x4Matrix.Transpose

// -----------------------------
// T4x4FixedPointMatrix
// -----------------------------

Procedure T4x4FixedPointMatrix_LoadIdentity(Var M: T4x4FixedPointMatrix);
Begin
  Asm
    MOV   EAX,M
    SUB   ECX,ECX
    MOV   EDX,$00010000

    MOV   [EAX +  0],EDX
    MOV   [EAX +  4],ECX
    MOV   [EAX +  8],ECX
    MOV   [EAX + 12],ECX

    MOV   [EAX + 16],ECX
    MOV   [EAX + 20],EDX
    MOV   [EAX + 24],ECX
    MOV   [EAX + 28],ECX

    MOV   [EAX + 32],ECX
    MOV   [EAX + 36],ECX
    MOV   [EAX + 40],EDX
    MOV   [EAX + 44],ECX

    MOV   [EAX + 48],ECX
    MOV   [EAX + 52],ECX
    MOV   [EAX + 56],ECX
    MOV   [EAX + 60],EDX
  End; // Asm
End; // T4x4FixedPointMatrix_LoadIdentity

Procedure T4x4FixedPointMatrix_LoadRotationMatrix(Var M: T4x4FixedPointMatrix; Var M3: T3x3FixedPointMatrix);
Begin
  M[1,1] := M3[1,1];
  M[1,2] := M3[1,2];
  M[1,3] := M3[1,3];

  M[2,1] := M3[2,1];
  M[2,2] := M3[2,2];
  M[2,3] := M3[2,3];

  M[3,1] := M3[3,1];
  M[3,2] := M3[3,2];
  M[3,3] := M3[3,3];
End; // T4x4FixedPointMatrix_LoadRotationMatrix

Procedure T4x4FixedPointMatrix_Multiply(Var M: T4x4FixedPointMatrix; Var X,Y,Z: Integer);
// Sets the vector to this * vector. Assumes that all vectors have 1 as their fourth component.
Var X1,Y1,Z1: Integer;
Begin
  Asm
    PUSH  EBX
    PUSH  ESI
    PUSH  EDI

    MOV   EBX,X
    MOV   ESI,Y
    MOV   EDI,Z
    MOV   EBX,[EBX]
    MOV   ESI,[ESI]
    MOV   EDI,[EDI]

    SUB   ECX,ECX

    MOV   EAX,M
    MOV   EAX,[EAX + 0]    // M[1,1]
    IMUL  EBX
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 4]    // M[1,2]
    IMUL  ESI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 8]    // M[1,3]
    IMUL  EDI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    ADD   ECX,[EAX + 12]   // M[1,4]

    MOV   X1,ECX

    SUB   ECX,ECX

    MOV   EAX,M
    MOV   EAX,[EAX + 16]   // M[2,1]
    IMUL  EBX
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 20]   // M[2,2]
    IMUL  ESI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 24]   // M[2,3]
    IMUL  EDI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    ADD   ECX,[EAX + 28]   // M[2,4]

    MOV   Y1,ECX

    SUB   ECX,ECX

    MOV   EAX,M
    MOV   EAX,[EAX + 32]   // M[3,1]
    IMUL  EBX
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 36]   // M[3,2]
    IMUL  ESI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    MOV   EAX,[EAX + 40]   // M[3,3]
    IMUL  EDI
    SHRD  EAX,EDX,16
    ADD   ECX,EAX

    MOV   EAX,M
    ADD   ECX,[EAX + 44]   // M[3,4]

    MOV   Z1,ECX

    POP   EDI
    POP   ESI
    POP   EBX
  End; // Asm
{
  X1 := ((M[1,1] * X) Shr 16) + ((M[1,2] * Y) Shr 16) + ((M[1,3] * Z) Shr 16) + M[1,4];
  Y1 := ((M[2,1] * X) Shr 16) + ((M[2,2] * Y) Shr 16) + ((M[2,3] * Z) Shr 16) + M[2,4];
  Z1 := ((M[3,1] * X) Shr 16) + ((M[3,2] * Y) Shr 16) + ((M[3,3] * Z) Shr 16) + M[3,4];
}
  X  := X1;
  Y  := Y1;
  Z  := Z1;
End; // T4x4FixedPointMatrix_Multiply

Procedure T4x4FixedPointMatrix_Transpose(Var M: T4x4FixedPointMatrix);
Var
  MM      : Array[1..4,1..4] Of Integer;
  Row,Col : Integer;

Begin
  For Row := 1 To 4 Do
  Begin
    For Col := 1 To 4 Do MM[Row,Col] := M[Col,Row];
  End; // For Row
  For Row := 1 To 4 Do
  Begin
    For Col := 1 To 4 Do M[Row,Col] := MM[Row,Col];
  End; // For Row
End; // T4x4FixedPointMatrix_Transpose

// -----------------------------
// TAxisAlignedBox
// -----------------------------

Constructor TAxisAlignedBox.Create;
Begin
  MinPt     := T3DPoint.Create{Point};
  MaxPt     := T3DPoint.Create{Point};
  Center    := T3DPoint.Create{Point};
{
  Corner[0] := T3DPoint.CreatePoint;
  Corner[1] := T3DPoint.CreatePoint;
  Corner[2] := T3DPoint.CreatePoint;
  Corner[3] := T3DPoint.CreatePoint;
  Corner[4] := T3DPoint.CreatePoint;
  Corner[5] := T3DPoint.CreatePoint;
  Corner[6] := T3DPoint.CreatePoint;
  Corner[7] := T3DPoint.CreatePoint;
}
End; // TAxisAlignedBox.Create

Destructor TAxisAlignedBox.Destroy;
//Var I: Integer;
Begin
  MinPt.Free;
  MaxPt.Free;
  Center.Free;
{
  MinPt  := Nil;
  MaxPt  := Nil;
  Center := Nil;
}
//  For I := 0 To 7 Do Corner[I].Free;
End; // TAxisAlignedBox.Destroy

Procedure TAxisAlignedBox.Setup(P1,P2: T3DPoint{I3dPoint});
Begin
  MinPt.Copy(Min(P1.X,P2.X),Min(P1.Y,P2.Y),Min(P1.Z,P2.Z));
  MaxPt.Copy(Max(P1.X,P2.X),Max(P1.Y,P2.Y),Max(P1.Z,P2.Z));
  Center.Copy(MinPt);
  Center.Add(MaxPt);
  Center.Divide(2);
//  SetupCorners;
End; // TAxisAlignedBox.Setup

Procedure TAxisAlignedBox.Setup(X1,Y1,Z1,X2,Y2,Z2: Single);
Begin
  MinPt.Copy(Min(X1,X2),Min(Y1,Y2),Min(Z1,Z2));
  MaxPt.Copy(Max(X1,X2),Max(Y1,Y2),Max(Z1,Z2));
  Center.Copy(MinPt);
  Center.Add(MaxPt);
  Center.Divide(2);
//  SetupCorners;
End; // TAxisAlignedBox.Setup
{
Procedure TAxisAlignedBox.SetupCorners;
Begin
  Corner[0].Copy(MinPt.X,MinPt.Y,MinPt.Z);
  Corner[1].Copy(MaxPt.X,MinPt.Y,MinPt.Z);
  Corner[2].Copy(MinPt.X,MaxPt.Y,MinPt.Z);
  Corner[3].Copy(MinPt.X,MinPt.Y,MaxPt.Z);
  Corner[4].Copy(MaxPt.X,MaxPt.Y,MaxPt.Z);
  Corner[5].Copy(MinPt.X,MaxPt.Y,MaxPt.Z);
  Corner[6].Copy(MaxPt.X,MinPt.Y,MaxPt.Z);
  Corner[7].Copy(MaxPt.X,MaxPt.Y,MinPt.Z);
End; // TAxisAlignedBox.SetupCorners
}
Procedure TAxisAlignedBox.Copy(Box: TAxisAlignedBox);
//Var I: Integer;
Begin
  MinPt.Copy(Box.MinPt);
  MaxPt.Copy(Box.MaxPt);
  Center.Copy(Box.Center);
//  For I := 0 To 7 Do Corner[I].Copy(Box.Corner[I]);
End; // TAxisAlignedBox.Copy

Function TAxisAlignedBox.IntersectionWithLineSegment(P1,P2: T3DPoint{I3dPoint}): Single;
Var
  St,Et,FSt,FEt,D : Single;
  I1,I2,M1,M2     : Array[1..3] Of Single;
  I               : Integer;
  Ok              : Boolean;

Begin
  St     := 0;
  Et     := 1;
  FSt    := 0;
  FEt    := 1;
  Result := -1;

  // Load X parameters

  I1[1] := P1.X;
  I2[1] := P2.X;
  M1[1] := MinPt.X;
  M2[1] := MaxPt.X;

  // Load Y parameters

  I1[2] := P1.Y;
  I2[2] := P2.Y;
  M1[2] := MinPt.Y;
  M2[2] := MaxPt.Y;

  // Load Z parameters

  I1[3] := P1.Z;
  I2[3] := P2.Z;
  M1[3] := MinPt.Z;
  M2[3] := MaxPt.Z;

  Ok := True;
  I  := 1;
  While (I < 4) And Ok Do
  Begin
    D := I2[I] - I1[I];
    If D > 0 Then
    Begin
      If (I1[I] > M2[I]) Or (I2[I] < M1[I]) Then Ok := False
      Else
      Begin
        If I1[I] < M1[I] Then St := (M1[I] - I1[I]) / D Else St := 0;
        If I2[I] > M2[I] Then Et := (M2[I] - I1[I]) / D Else Et := 1;
      End;
    End
    Else If D < 0 Then
    Begin
      If (I2[I] > M2[I]) Or (I1[I] < M1[I]) Then Ok := False
      Else
      Begin
        If I1[I] > M2[I] Then St := (M2[I] - I1[I]) / D Else St := 0;
        If I2[I] < M1[I] Then Et := (M1[I] - I1[I]) / D Else Et := 1;
      End;  
    End
    Else Ok := False;
    If Ok Then
    Begin
      If St > FSt Then FSt := St;
      If Et < FEt Then FEt := Et;
      If FEt < FSt Then Ok := False;
    End;
    Inc(I);
  End; // While

  If Ok Then Result := FSt;
End; // TAxisAlignedBox.IntersectionWithLineSegment

Function TAxisAlignedBox.ContainsPoint(P: T3DPoint{I3dPoint}): Boolean;
Begin
  Result := (P.X >= MinPt.X) And (P.Y >= MinPt.Y) And (P.Z >= MinPt.Z) And
            (P.X <= MaxPt.X) And (P.Y <= MaxPt.Y) And (P.Z <= MaxPt.Z)
End; // TAxisAlignedBox.ContainsPoint

Procedure TAxisAlignedBox.CounterClockwiseRotate(P: T3DPoint{I3dPoint}; RX,RY,RZ,RotateAroundCenter: Boolean);
//Var I: Integer;
Begin
  If RotateAroundCenter Then
  Begin
    MinPt.Subtract(Center);
    MinPt.CounterClockwiseRotate(P,RX,RY,RZ,False);
    MinPt.Add(Center);

    MaxPt.Subtract(Center);
    MaxPt.CounterClockwiseRotate(P,RX,RY,RZ,False);
    MaxPt.Add(Center);
{
    For I := 0 To 7 Do
    Begin
      Corner[I].Subtract(Center);
      Corner[I].CounterClockwiseRotate(P,RX,RY,RZ);
      Corner[I].Add(Center);
    End; // For I
}
  End
  Else
  Begin
    MinPt.CounterClockwiseRotate(P,RX,RY,RZ,False);
    MaxPt.CounterClockwiseRotate(P,RX,RY,RZ,False);
//    For I := 0 To 7 Do Corner[I].CounterClockwiseRotate(P,RX,RY,RZ);
  End;
End; // TAxisAlignedBox.CounterClockwiseRotate

Procedure TAxisAlignedBox.ClockwiseRotate(P: T3DPoint{I3dPoint}; RX,RY,RZ,RotateAroundCenter: Boolean);
//Var I: Integer;
Begin
  If RotateAroundCenter Then
  Begin
    MinPt.Subtract(Center);
    MinPt.ClockwiseRotate(P,RX,RY,RZ,False);
    MinPt.Add(Center);

    MaxPt.Subtract(Center);
    MaxPt.ClockwiseRotate(P,RX,RY,RZ,False);
    MaxPt.Add(Center);
{
    For I := 0 To 7 Do
    Begin
      Corner[I].Subtract(Center);
      Corner[I].ClockwiseRotate(P,RX,RY,RZ);
      Corner[I].Add(Center);
    End; // For I
}
  End
  Else
  Begin
    MinPt.ClockwiseRotate(P,RX,RY,RZ,False);
    MaxPt.ClockwiseRotate(P,RX,RY,RZ,False);
//    For I := 0 To 7 Do Corner[I].ClockwiseRotate(P,RX,RY,RZ);
  End;
End; // TAxisAlignedBox.ClockwiseRotate

Procedure TAxisAlignedBox.Move(P: T3DPoint{I3dPoint});
//Var I: Integer;
Begin
  MinPt.Add(P);
  MaxPt.Add(P);
  Center.Add(P);
//  For I := 0 To 7 Do Corner[I].Add(P);
End; // TAxisAlignedBox.Move

Procedure TAxisAlignedBox.Scale(P: T3DPoint{I3dPoint}; ScaleAroundCenter: Boolean);
//Var I: Integer;
Begin
  If ScaleAroundCenter Then
  Begin
    MinPt.Subtract(Center);
    MinPt.Multiply(P);
    MinPt.Add(Center);

    MaxPt.Subtract(Center);
    MaxPt.Multiply(P);
    MaxPt.Add(Center);
{
    For I := 0 To 7 Do
    Begin
      Corner[I].Subtract(Center);
      Corner[I].Multiply(P);
      Corner[I].Add(Center);
    End; // For I
}
  End
  Else
  Begin
    MinPt.Multiply(P);
    MaxPt.Multiply(P);
    Center.Multiply(P);
//    For I := 0 To 7 Do Corner[I].Multiply(P);
  End;
End; // TAxisAlignedBox.Scale

Procedure TAxisAlignedBox.Expand(Box: TAxisAlignedBox);
Begin
  // Ignore zero-size boxes
  If (Box.MinPt.X < Box.MaxPt.X) And (Box.MinPt.Y < Box.MaxPt.Y) And (Box.MinPt.Z < Box.MaxPt.Z) Then
  Begin
    If Box.MinPt.X < MinPt.X Then MinPt.X := Box.MinPt.X;
    If Box.MinPt.Y < MinPt.Y Then MinPt.Y := Box.MinPt.Y;
    If Box.MinPt.Z < MinPt.Z Then MinPt.Z := Box.MinPt.Z;
    If Box.MaxPt.X > MaxPt.X Then MaxPt.X := Box.MaxPt.X;
    If Box.MaxPt.Y > MaxPt.Y Then MaxPt.Y := Box.MaxPt.Y;
    If Box.MaxPt.Z > MaxPt.Z Then MaxPt.Z := Box.MaxPt.Z;
    Center.Copy(MinPt);
    Center.Add(MaxPt);
    Center.Divide(2);
  End;
End; // TAxisAlignedBox.Expand

// -----------------------------
// TSphere
// -----------------------------

Constructor TSphere.Create;
Begin
  Center := T3DPoint.Create{Point};
  Radius := 0;
End; // TSphere.Create

Constructor TSphere.Create(Sphere: TSphere);
Begin
  Center := T3DPoint.Create{Point}(Sphere.Center);
  Radius := Sphere.Radius;
End; // TSphere.Create

Destructor TSphere.Destroy;
Begin
  Center.Free;
//  Center := Nil;
End; // TSphere.Destroy

Function TSphere.ContainsPoint(P: T3DPoint{I3dPoint}): Boolean;
Begin
  Result := (Center.DistanceFrom2(P) < Sqr(Radius));
End; // TSphere.ContainsPoint

Function TSphere.ToString;
Begin
  Result := 'Center: ' + Center.ToString + ', Radius: ' + Format('%8.3f',[Radius]);
End; // TSphere.ToString

Function TSphere.Intersects(Sphere: TSphere): Boolean;
Begin
  Result := Center.DistanceFrom2(Sphere.Center) < Sqr(Radius + Sphere.Radius);
End; // TSphere.Intersects

Procedure TSphere.Copy(Sphere: TSphere);
Begin
  Center.Copy(Sphere.Center);
  Radius := Sphere.Radius;
End; // TSphere.Copy

Procedure TSphere.Setup(P1,P2: T3DPoint{I3dPoint});
Begin
  Center.Copy(P1);
  Center.Add(P2);
  Center.Divide(2);
  Radius := P1.DistanceFrom(P2) / 2;
End; // TSphere.Setup

Procedure TSphere.Setup(X1,Y1,Z1,X2,Y2,Z2: Single);
Begin
  Center.Copy(X1,Y1,Z1);
  Center.Add(X2,Y2,Z2);
  Center.Divide(2);
  Radius := Sqrt(Sqr(X2 - X1) + Sqr(Y2 - Y1) + Sqr(Z2 - Z1)) / 2;
End; // TSphere.Setup

Function TSphere.IntersectsLineSegment(P1,P2: T3DPoint{I3dPoint}): Boolean;
Var
  A,B,C,V  : Single;
  PX,PY,PZ : Single;

Begin
(*
  P := T3DPoint.Create{Point}(P1,P2);
  Q := T3DPoint.Create{Point}(Center,P1);
  A := P1.DistanceFrom2(P2);
  B := 2 * P.Dot(Q);
  C := Center.Dot(Center) + P1.Dot(P1) - 2 * Center.Dot(P1) - Sqr(Radius);
  V := Sqr(B) - 4 * A * C;
  P.Free;
  Q.Free;
  Result := (V >= 0);
*)
  PX := P2.X - P1.X;
  PY := P2.Y - P1.Y;
  PZ := P2.Z - P1.Z;

  A := Sqr(PX) + Sqr(PY) + Sqr(PZ);

  B := 2 * (PX * (P1.X - Center.X) + PY * (P1.Y - Center.Y) + PZ * (P1.Z - Center.Z));

  C := Sqr(Center.X) + Sqr(Center.Y) + Sqr(Center.Z) + Sqr(P1.X) + Sqr(P1.Y) + Sqr(P1.Z) -
       2 * (Center.X * P1.X + Center.Y * P1.Y + Center.Z * P1.Z) - Sqr(Radius);

  V := Sqr(B) - 4 * A * C;

  Result := (V >= 0);
End; // TSphere.IntersectsLineSegment

Function TSphere.IntersectsLineSegment(X1,Y1,Z1,X2,Y2,Z2: Single): Boolean;
Var
  A,B,C,V  : Single;
  PX,PY,PZ : Single;

Begin
  PX := X2 - X1;
  PY := Y2 - Y1;
  PZ := Z2 - Z1;

  A := Sqr(PX) + Sqr(PY) + Sqr(PZ);

  B := 2 * (PX * (X1 - Center.X) + PY * (Y1 - Center.Y) + PZ * (Z1 - Center.Z));

  C := Sqr(Center.X) + Sqr(Center.Y) + Sqr(Center.Z) + Sqr(X1) + Sqr(Y1) + Sqr(Z1) -
       2 * (Center.X * X1 + Center.Y * Y1 + Center.Z * Z1) - Sqr(Radius);

  V := Sqr(B) - 4 * A * C;

  Result := (V >= 0);
End; // TSphere.IntersectsLineSegment

Function TSphere.IntersectionsWithLineSegment(P1,P2,I1,I2: T3DPoint{I3dPoint}): Integer;
Var
  A,B,C    : Single;
  PX,PY,PZ : Single;
  V,U      : Single;
  SV,U1,U2 : Single;
  I        : Integer;
  Point    : T3DPoint{I3dPoint};

Begin
(*
  P := T3DPoint.Create{Point}(P1,P2);
  Q := T3DPoint.Create{Point}(Center,P1);
  A := P1.DistanceFrom2(P2);
  B := 2 * P.Dot(Q);
  C := Center.Dot(Center) + P1.Dot(P1) - 2 * Center.Dot(P1) - Sqr(Radius);
  V := Sqr(B) - 4 * A * C;
  P.Free;
  Q.Free;
  Result := (V >= 0);
*)
  PX := P2.X - P1.X;
  PY := P2.Y - P1.Y;
  PZ := P2.Z - P1.Z;

  A := Sqr(PX) + Sqr(PY) + Sqr(PZ);

  B := 2 * (PX * (P1.X - Center.X) + PY * (P1.Y - Center.Y) + PZ * (P1.Z - Center.Z));

  C := Sqr(Center.X) + Sqr(Center.Y) + Sqr(Center.Z) + Sqr(P1.X) + Sqr(P1.Y) + Sqr(P1.Z) -
       2 * (Center.X * P1.X + Center.Y * P1.Y + Center.Z * P1.Z) - Sqr(Radius);

  V := Sqr(B) - 4 * A * C;

  If V = 0 Then
  Begin
    // There is only one intersection point

    U := -B / (2 * A);
    I1.Copy(P1.X + U * PX, P1.Y + U * PY, P1.Z + U * PZ);
    Result := 1;
  End
  Else If V > 0 Then
  Begin
    SV := Sqrt(V); // this way we calculate the sqrt only once.
    U1 := (-B + SV) / (2 * A);
    U2 := (-B - SV) / (2 * A);
    I  := 0;

    // this part is needed to check if the line segment ends before it intersects the sphere or the far side of the sphere
    // or maybe even intersects before the first point, the only intersections we need are between the first and last
    // point of the line segment

    Point := I1;
    If (U1 >= 0) And (U1 <= 1) Then
    Begin
      I1.Copy(P1.X + U1 * PX, P1.Y + U1 * PY, P1.Z + U1 * PZ);
      Inc(I);
      Point := I2;
    End;

    If (U2 >= 0) And (U2 <= 1) Then
    Begin
      Point.Copy(P1.X + U2 * PX, P1.Y + U2 * PY, P1.Z + U2 * PZ);
      Inc(I);
    End;
    Result := I;
  End
  Else Result := 0;
End; // TSphere.IntersectionsWithLineSegment

Function TSphere.IntersectionWithRay(RayOrigin,RayDirection: T3DPoint{I3dPoint}): Single;
// The ray defines an infinite line in BOTH directions (i.e. this doesn't take ray origin into account).
// The returned value is the distance from the ray origin to the first intersection point.
Var QX,QY,QZ,C,V,D: Single;
Begin
  QX := Center.X - RayOrigin.X; // I could create a T3DPoint for cleaner code but it would take longer to execute
  QY := Center.Y - RayOrigin.Y;
  QZ := Center.Z - RayOrigin.Z;
  C  := Sqrt(Sqr(QX) + Sqr(QY) + Sqr(QZ));
  V  := RayDirection.Dot(QX,QY,QZ);
  D  := Sqr(Radius) - (Sqr(C) - Sqr(V));

  // If there was no intersection, return -1.  Otherwise, return the distance
  // to the first intersecting point

       If D < 0 Then Result := -1
  Else If D = 0 Then Result := V
  Else Result := V - Sqrt(D);
End; // TSphere.IntersectionWithRay

// -----------------------------
// TEllipsoid
// -----------------------------

Constructor TEllipsoid.Create;
Begin
  Center := T3DPoint.Create{Point};
  Radius := T3DPoint.Create{Point}(1,1,1); // Spherical by default
End; // TEllipsoid.Create

Destructor TEllipsoid.Destroy;
Begin
  Center.Free;
  Radius.Free;
{
  Center := Nil;
  Radius := Nil;
}  
End; // TEllipsoid.Destroy

Function TEllipsoid.ToString;
Begin
  Result := 'Center: ' + Center.ToString + ', Radius: ' + Radius.ToString;
End; // TEllipsoid.ToString

Procedure TEllipsoid.Copy(Ellipsoid: TEllipsoid);
Begin
  Center.Copy(Ellipsoid.Center);
  Radius.Copy(Ellipsoid.Radius);
End; // TEllipsoid.Copy

Function TEllipsoid.IntersectionWithRay(RayOrigin,RayDirection: T3DPoint{I3dPoint}): Single;
Var QX,QY,QZ,C,V,D: Single;
Begin
  QX := Center.X - RayOrigin.X; // I could create a T3DPoint for cleaner code but it would take longer to execute
  QY := Center.Y - RayOrigin.Y;
  QZ := Center.Z - RayOrigin.Z;
  C  := Sqrt(Sqr(QX) + Sqr(QY) + Sqr(QZ));
  V  := RayDirection.Dot(QX,QY,QZ);
  D  := Radius.Dot(Radius) - (Sqr(C) - Sqr(V));

  // If there was no intersection, return -1.  Otherwise, return the distance
  // to the first intersecting point

       If D < 0 Then Result := -1
  Else If D = 0 Then Result := V
  Else Result := V - Sqrt(D);
End; // TEllipsoid.IntersectionWithRay

// -----------------------------
// TCylinder
// -----------------------------

Constructor TCylinder.Create;
Begin
  Position := T3DPoint.Create{Point};
  Height   := 0;
  Radius   := 0;
End; // TCylinder.Create

Destructor TCylinder.Destroy;
Begin
  Position.Free;
//  Position := Nil;
End; // TCylinder.Destroy

Procedure TCylinder.Copy(Cylinder: TCylinder);
Begin
  Position.Copy(Cylinder.Position);
  Height := Cylinder.Height;
  Radius := Cylinder.Radius;
End; // TCylinder.Copy

Procedure TCylinder.Setup(P1,P2: T3DPoint{I3dPoint});
Begin
  Position.Copy((P1.X + P2.X) / 2,(P1.Y + P2.Y) / 2,Min(P1.Z,P2.Z));
  Height := Abs(P2.Z - P1.Z);
  Radius := Sqrt(Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y));
End; // TCylinder.Setup

Procedure TCylinder.Setup(X1,Y1,Z1,X2,Y2,Z2: Single);
Begin
  Position.Copy((X1 + X2) / 2,(Y1 + Y2) / 2,Min(Z1,Z2));
  Height := Abs(Z2 - Z1);
  Radius := Sqrt(Sqr(X2 - X1) + Sqr(Y2 - Y1));
End; // TCylinder.Setup

Function TCylinder.ContainsPoint(P: T3DPoint{I3dPoint}): Boolean;
Begin
  If (P.Z > Position.Z) And (P.Z < Position.Z + Height) Then
  Begin
    Result := (Sqr(P.X - Position.X) + Sqr(P.Y - Position.Y) < Sqr(Radius));
  End
  Else Result := False;
End; // TCylinder.ContainsPoint

Function TCylinder.ContainsPoint(X,Y,Z: Single): Boolean;
Begin
  If (Z > Position.Z) And (Z < Position.Z + Height) Then
  Begin
    Result := (Sqr(X - Position.X) + Sqr(Y - Position.Y) < Sqr(Radius));
  End
  Else Result := False;
End; // TCylinder.ContainsPoint

Function TCylinder.IntersectsSphere(Sphere: TSphere): Boolean;
Var XYCenterDist2,SphereRad: Single;
Begin
  Result := False;
  If Sphere.Radius > 0 Then
  Begin
    XYCenterDist2 := Sqr(Sphere.Center.X - Position.X) + Sqr(Sphere.Center.Y - Position.Y);
    If Sphere.Center.Z >= Position.Z Then
    Begin
      If Sphere.Center.Z <= Position.Z + Height Then
      Begin
        Result := (XYCenterDist2 < Sqr(Radius + Sphere.Radius));
      End
      Else If Sphere.Center.Z < Position.Z + Height + Sphere.Radius Then
      Begin
        SphereRad := Sqrt(Sqr(Sphere.Radius) - Sqr(Sphere.Center.Z - (Position.Z + Height)));
        Result    := (XYCenterDist2 < Sqr(Radius + SphereRad));
      End;
    End
    Else If Sphere.Center.Z > Position.Z - Sphere.Radius Then
    Begin
      SphereRad := Sqrt(Sqr(Sphere.Radius) - Sqr(Position.Z - Sphere.Center.Z));
      Result    := (XYCenterDist2 < Sqr(Radius + SphereRad));
    End;
  End;
End; // TCylinder.IntersectsSphere

// -----------------------------
// TPlane
// -----------------------------

Constructor TPlane.Create;
Begin
  Normal   := T3DPoint.Create{Point}(0,0,1);
  Distance := 0;
End; // TPlane.Create

Constructor TPlane.Create(Plane: TPlane);
Begin
  Normal   := T3DPoint.Create{Point}(Plane.Normal);
  Distance := Plane.Distance;
End; // TPlane.Create

Destructor TPlane.Destroy;
Begin
  Normal.Free;
//  Normal := Nil;
End; // TPlane.Destroy

Function TPlane.ToString;
Begin
  Result := 'Normal: ' + Normal.ToString + ', Distance: ' + Format('%8.3f',[Distance]);
End; // TPlane.ToString

Procedure TPlane.Flip;
Begin
  Normal.Multiply(-1);
  Distance := -Distance;
End; // TPlane.Flip

Function TPlane.ContainsSphere(Sphere: TSphere): TSphereCullResult;
Var Dist: Single;
Begin
  Dist := Normal.Dot(Sphere.Center) + Distance;
       If Dist < -Sphere.Radius Then Result := scrOutside             // If this distance is < -sphere.radius, we are outside
  Else If Abs(Dist) < Sphere.Radius Then Result := scrIntersects // Else if the distance is between +- radius, then we intersect
  Else Result := scrInside;
End; // TPlane.ContainsSphere

Function TPlane.ContainsPoint(P: T3DPoint{I3dPoint}): Boolean;
Begin
  Result := ((Normal.Dot(P) + Distance) >= 0);
End; // TPlane.ContainsPoint

Function TPlane.ContainsPoint(X,Y,Z: Single): Boolean;
Begin
  Result := ((Normal.Dot(X,Y,Z) + Distance) >= 0);
End; // TPlane.ContainsPoint

Function TPlane.ContainsFrustum(Frustum: TFrustum): Boolean;
// Returns true if any part of the frustum is within the area denoted by the plane
Begin
  Result := ContainsPoint(Frustum.Points[0]) Or // Order is designed to shorten the time required to find it
            ContainsPoint(Frustum.Points[6]) Or
            ContainsPoint(Frustum.Points[4]) Or
            ContainsPoint(Frustum.Points[2]) Or
            ContainsPoint(Frustum.Points[3]) Or
            ContainsPoint(Frustum.Points[5]) Or
            ContainsPoint(Frustum.Points[7]) Or
            ContainsPoint(Frustum.Points[1]);
End; // TPlane.ContainsFrustum

Function TPlane.ClassifyPoint(P: T3DPoint{I3dPoint}): TAlphaClassification;
Const Epsilon = 0.05;
Var Value: Single;
Begin
  Value := Normal.Dot(P) + Distance;
       If Value < -Epsilon Then Result := acBehind
  Else If Value >  Epsilon Then Result := acInFront
  Else Result := acCoinciding;
End; // TPlane.ClassifyPoint

Function TPlane.ClassifyPoint(X,Y,Z: Single): TAlphaClassification;
Const Epsilon = 0.05;
Var Value: Single;
Begin
  Value := Normal.Dot(X,Y,Z) + Distance;
       If Value < -Epsilon Then Result := acBehind
  Else If Value >  Epsilon Then Result := acInFront
  Else Result := acCoinciding;
End; // TPlane.ClassifyPoint

Function TPlane.ClassifyPoint(Var P: TSimplePoint): TAlphaClassification;
Const Epsilon = 0.05;
Var Value: Single;
Begin
  Value := Normal.Dot(P) + Distance;
       If Value < -Epsilon Then Result := acBehind
  Else If Value >  Epsilon Then Result := acInFront
  Else Result := acCoinciding;
End; // TPlane.ClassifyPoint

Procedure TPlane.Setup(P1,P2,P3: T3DPoint{I3dPoint});
Begin
  Normal.GetNormalTo(P1,P2,P3);
  Distance := GetHessianDistance(Normal,P1);
End; // TPlane.Setup

Procedure TPlane.Setup(AOrigin,ANormal: T3DPoint{I3dPoint});
Begin
  Normal.Copy(ANormal);
  Distance := -Normal.Dot(AOrigin);
End; // TPlane.Setup

Procedure TPlane.Copy(Plane: TPlane);
Begin
  Normal.Copy(Plane.Normal);
  Distance := Plane.Distance;
End; // TPlane.Copy

Function TPlane.IntersectionWithRay(RayOrigin,RayDirection: T3DPoint{I3dPoint}): Single;
// If the result is 0 or greater, then it intersects with the ray
Var Numer,Denom: Single;
Begin
  Numer  := Normal.Dot(RayOrigin) + Distance;
  Denom  := Normal.Dot(RayDirection);
  If Denom <> 0
   Then Result := -Numer / Denom
   Else Result := -1;
End; // TPlane.IntersectionWithRay

Function TPlane.IntersectionWithRay(Var RayOrigin,RayDirection: TSimplePoint): Single;
// If the result is 0 or greater, then it intersects with the ray
Var Numer,Denom: Single;
Begin
  Numer  := Normal.Dot(RayOrigin) + Distance;
  Denom  := Normal.Dot(RayDirection);
  If Denom <> 0
   Then Result := -Numer / Denom
   Else Result := -1;
End; // TPlane.IntersectionWithRay

Function TPlane.IntersectionWithSegment(P1,P2: T3DPoint{I3dPoint}): Single;
// Intersects with the segment if the result is in the range [0..1]
Var D1,D2: Single;
Begin
  D1 := Normal.Dot(P1) + Distance;
  D2 := Normal.Dot(P2) + Distance;
  If D1 <> D2
   Then Result := -D1 / (D2 - D1)
   Else Result := -1;
End; // TPlane.IntersectionWithSegment

Function TPlane.IntersectionWithSegment(P1,P2: TSimplePoint): Single;
// Intersects with the segment if the result is in the range [0..1]
Var D1,D2: Single;
Begin
  D1 := Normal.Dot(P1) + Distance;
  D2 := Normal.Dot(P2) + Distance;
  If D1 <> D2
   Then Result := -D1 / (D2 - D1)
   Else Result := -1;
End; // TPlane.IntersectionWithSegment

Function TPlane.DistanceFromPoint(P: T3DPoint{I3dPoint}): Single;
Begin
  Result := Abs(Normal.Dot(P) + Distance);
End; // TPlane.DistanceFromPoint
{
Function TPlane.DistanceFromPoint(P: P3DPointPointer): Single;
Begin
  Result := Abs(Normal.Dot(P) + Distance);
End; // TPlane.DistanceFromPoint
}
Function TPlane.SignedDistanceFromPoint(P: T3DPoint{I3dPoint}): Single;
// Returns a positive value if the plane "faces" the point
Begin
  Result := Normal.Dot(P) + Distance;
End; // TPlane.SignedDistanceFromPoint
{
Function TPlane.SignedDistanceFromPoint(P: P3DPointPointer): Single;
// Returns a positive value if the plane "faces" the point
Begin
  Result := Normal.Dot(P) + Distance;
End; // TPlane.SignedDistanceFromPoint
}
Procedure TPlane.SplitSimplePolygon(Var Polygon,PosPolygon,NegPolygon: TSimplePolygon);
Var
  I              : Integer;
  PtA            : Integer;
  PtB            : Integer;
  SideA          : TAlphaClassification;
  SideB          : TAlphaClassification;
  RayDirection   : TSimplePoint;
  Intersection   : Single;
  IntersectionPt : TSimplePoint;

Begin
  PosPolygon.NumPoints := 0;
  NegPolygon.NumPoints := 0;
  If Polygon.NumPoints > 0 Then
  Begin
    PtA            := Polygon.NumPoints - 1;
    SideA          := ClassifyPoint(Polygon.Points[PtA]);
    For I := 0 To Polygon.NumPoints - 1 Do
    Begin
      PtB   := I;
      SideB := ClassifyPoint(Polygon.Points[PtB]);
      If SideB = acInFront Then
      Begin
        If SideA = acBehind Then
        Begin
          RayDirection.X := Polygon.Points[PtB].X - Polygon.Points[PtA].X;
          RayDirection.Y := Polygon.Points[PtB].Y - Polygon.Points[PtA].Y;
          RayDirection.Z := Polygon.Points[PtB].Z - Polygon.Points[PtA].Z;
          Intersection   := IntersectionWithRay(Polygon.Points[PtA],RayDirection);
          If Intersection >= 0 Then
          Begin
            IntersectionPt.X := RayDirection.X * Intersection + Polygon.Points[PtA].X;
            IntersectionPt.Y := RayDirection.Y * Intersection + Polygon.Points[PtA].Y;
            IntersectionPt.Z := RayDirection.Z * Intersection + Polygon.Points[PtA].Z;
            PosPolygon.Points[PosPolygon.NumPoints] := IntersectionPt;
            Inc(PosPolygon.NumPoints);
            NegPolygon.Points[NegPolygon.NumPoints] := IntersectionPt;
            Inc(NegPolygon.NumPoints);
          End;
        End;
        PosPolygon.Points[PosPolygon.NumPoints] := Polygon.Points[PtB];
        Inc(PosPolygon.NumPoints);
      End
      Else If SideB = acBehind Then
      Begin
        If SideA = acInFront Then
        Begin
          RayDirection.X := Polygon.Points[PtB].X - Polygon.Points[PtA].X;
          RayDirection.Y := Polygon.Points[PtB].Y - Polygon.Points[PtA].Y;
          RayDirection.Z := Polygon.Points[PtB].Z - Polygon.Points[PtA].Z;
          Intersection   := IntersectionWithRay(Polygon.Points[PtA],RayDirection);
          If Intersection >= 0 Then
          Begin
            IntersectionPt.X := RayDirection.X * Intersection + Polygon.Points[PtA].X;
            IntersectionPt.Y := RayDirection.Y * Intersection + Polygon.Points[PtA].Y;
            IntersectionPt.Z := RayDirection.Z * Intersection + Polygon.Points[PtA].Z;
            PosPolygon.Points[PosPolygon.NumPoints] := IntersectionPt;
            Inc(PosPolygon.NumPoints);
            NegPolygon.Points[NegPolygon.NumPoints] := IntersectionPt;
            Inc(NegPolygon.NumPoints);
          End;
        End;
        NegPolygon.Points[NegPolygon.NumPoints] := Polygon.Points[PtB];
        Inc(NegPolygon.NumPoints);
      End
      Else
      Begin
        PosPolygon.Points[PosPolygon.NumPoints] := Polygon.Points[PtB];
        Inc(PosPolygon.NumPoints);
        NegPolygon.Points[NegPolygon.NumPoints] := Polygon.Points[PtB];
        Inc(NegPolygon.NumPoints);
      End;
      PtA   := PtB;
      SideA := SideB;
    End; // For I
  End;
End; // TPlane.SplitSimplePolygon

Procedure TPlane.SplitSimplePolygonPosOnly(Var Polygon,PosPolygon: TSimplePolygon);
Var
  I              : Integer;
  PtA            : Integer;
  PtB            : Integer;
  SideA          : TAlphaClassification;
  SideB          : TAlphaClassification;
  RayDirection   : TSimplePoint;
  Intersection   : Single;
  IntersectionPt : TSimplePoint;

Begin
  PosPolygon.NumPoints := 0;
  If Polygon.NumPoints > 0 Then
  Begin
    PtA            := Polygon.NumPoints - 1;
    SideA          := ClassifyPoint(Polygon.Points[PtA]);
    For I := 0 To Polygon.NumPoints - 1 Do
    Begin
      PtB   := I;
      SideB := ClassifyPoint(Polygon.Points[PtB]);
      If SideB = acInFront Then
      Begin
        If SideA = acBehind Then
        Begin
          RayDirection.X := Polygon.Points[PtB].X - Polygon.Points[PtA].X;
          RayDirection.Y := Polygon.Points[PtB].Y - Polygon.Points[PtA].Y;
          RayDirection.Z := Polygon.Points[PtB].Z - Polygon.Points[PtA].Z;
          Intersection   := IntersectionWithRay(Polygon.Points[PtA],RayDirection);
          If Intersection >= 0 Then
          Begin
            IntersectionPt.X := RayDirection.X * Intersection + Polygon.Points[PtA].X;
            IntersectionPt.Y := RayDirection.Y * Intersection + Polygon.Points[PtA].Y;
            IntersectionPt.Z := RayDirection.Z * Intersection + Polygon.Points[PtA].Z;
            PosPolygon.Points[PosPolygon.NumPoints] := IntersectionPt;
            Inc(PosPolygon.NumPoints);
          End;
        End;
        PosPolygon.Points[PosPolygon.NumPoints] := Polygon.Points[PtB];
        Inc(PosPolygon.NumPoints);
      End
      Else If SideB = acBehind Then
      Begin
        If SideA = acInFront Then
        Begin
          RayDirection.X := Polygon.Points[PtB].X - Polygon.Points[PtA].X;
          RayDirection.Y := Polygon.Points[PtB].Y - Polygon.Points[PtA].Y;
          RayDirection.Z := Polygon.Points[PtB].Z - Polygon.Points[PtA].Z;
          Intersection   := IntersectionWithRay(Polygon.Points[PtA],RayDirection);
          If Intersection >= 0 Then
          Begin
            IntersectionPt.X := RayDirection.X * Intersection + Polygon.Points[PtA].X;
            IntersectionPt.Y := RayDirection.Y * Intersection + Polygon.Points[PtA].Y;
            IntersectionPt.Z := RayDirection.Z * Intersection + Polygon.Points[PtA].Z;
            PosPolygon.Points[PosPolygon.NumPoints] := IntersectionPt;
            Inc(PosPolygon.NumPoints);
          End;
        End;
      End
      Else
      Begin
        PosPolygon.Points[PosPolygon.NumPoints] := Polygon.Points[PtB];
        Inc(PosPolygon.NumPoints);
      End;
      PtA   := PtB;
      SideA := SideB;
    End; // For I
  End;
End; // TPlane.SplitSimplePolygonPosOnly
{
Procedure TPlane.SplitFixedPointPolygon(Var Polygon,PosPolygon,NegPolygon: TSimpleScreenPolygon);
Var
  I              : Integer;
  PtA            : Integer;
  PtB            : Integer;
  SideA          : TAlphaClassification;
  SideB          : TAlphaClassification;
  RayDirection   : TSimplePoint;
  Intersection   : Single;
  IntersectionPt : TSimplePoint;

Begin
  PosPolygon.NumPoints := 0;
  NegPolygon.NumPoints := 0;
  If Polygon.NumPoints > 0 Then
  Begin
    PtA            := Polygon.NumPoints - 1;
    SideA          := ClassifyPoint(Polygon.Points[PtA]);
    For I := 0 To Polygon.NumPoints - 1 Do
    Begin
      PtB   := I;
      SideB := ClassifyPoint(Polygon.Points[PtB]);
      If SideB = acInFront Then
      Begin
        If SideA = acBehind Then
        Begin
          RayDirection.X := Polygon.Points[PtB].X - Polygon.Points[PtA].X;
          RayDirection.Y := Polygon.Points[PtB].Y - Polygon.Points[PtA].Y;
          RayDirection.Z := Polygon.Points[PtB].Z - Polygon.Points[PtA].Z;
          Intersection   := IntersectionWithRay(Polygon.Points[PtA],RayDirection);
          If Intersection >= 0 Then
          Begin
            IntersectionPt.X := RayDirection.X * Intersection + Polygon.Points[PtA].X;
            IntersectionPt.Y := RayDirection.Y * Intersection + Polygon.Points[PtA].Y;
            IntersectionPt.Z := RayDirection.Z * Intersection + Polygon.Points[PtA].Z;
            PosPolygon.Points[PosPolygon.NumPoints] := IntersectionPt;
            Inc(PosPolygon.NumPoints);
            NegPolygon.Points[NegPolygon.NumPoints] := IntersectionPt;
            Inc(NegPolygon.NumPoints);
          End;
        End;
        PosPolygon.Points[PosPolygon.NumPoints] := Polygon.Points[PtB];
        Inc(PosPolygon.NumPoints);
      End
      Else If SideB = acBehind Then
      Begin
        If SideA = acInFront Then
        Begin
          RayDirection.X := Polygon.Points[PtB].X - Polygon.Points[PtA].X;
          RayDirection.Y := Polygon.Points[PtB].Y - Polygon.Points[PtA].Y;
          RayDirection.Z := Polygon.Points[PtB].Z - Polygon.Points[PtA].Z;
          Intersection   := IntersectionWithRay(Polygon.Points[PtA],RayDirection);
          If Intersection >= 0 Then
          Begin
            IntersectionPt.X := RayDirection.X * Intersection + Polygon.Points[PtA].X;
            IntersectionPt.Y := RayDirection.Y * Intersection + Polygon.Points[PtA].Y;
            IntersectionPt.Z := RayDirection.Z * Intersection + Polygon.Points[PtA].Z;
            PosPolygon.Points[PosPolygon.NumPoints] := IntersectionPt;
            Inc(PosPolygon.NumPoints);
            NegPolygon.Points[NegPolygon.NumPoints] := IntersectionPt;
            Inc(NegPolygon.NumPoints);
          End;
        End;
        NegPolygon.Points[NegPolygon.NumPoints] := Polygon.Points[PtB];
        Inc(NegPolygon.NumPoints);
      End
      Else
      Begin
        PosPolygon.Points[PosPolygon.NumPoints] := Polygon.Points[PtB];
        Inc(PosPolygon.NumPoints);
        NegPolygon.Points[NegPolygon.NumPoints] := Polygon.Points[PtB];
        Inc(NegPolygon.NumPoints);
      End;
      PtA       := PtB;
      SideA     := SideB;
    End; // For I
  End;
End; // TPlane.SplitFixedPointPolygon
}
// -----------------------------
// TQuaternion
// -----------------------------

Constructor TQuaternion.Create;
Begin
  W    := 1;
  X    := 0;
  Y    := 0;
  Z    := 0;
End; // TQuaternion.Create

Constructor TQuaternion.Create(fW,fX,fY,fZ: Single);
Begin
  W    := fW;
  X    := fX;
  Y    := fY;
  Z    := fZ;
End; // TQuaternion.Create

Constructor TQuaternion.Create(Q: TQuaternion);
Begin
  W    := Q.W;
  X    := Q.X;
  Y    := Q.Y;
  Z    := Q.Z;
End; // TQuaternion.Create

Procedure TQuaternion.Copy(Q: TQuaternion);
Begin
  W := Q.W;
  X := Q.X;
  Y := Q.Y;
  Z := Q.Z;
End; // TQuaternion.Copy

Procedure TQuaternion.Copy(fW,fX,fY,fZ: Single);
Begin
  W := fW;
  X := fX;
  Y := fY;
  Z := fZ;
End; // TQuaternion.Copy

Procedure TQuaternion.FromRotationMatrix(Const kRot: T3x3Matrix);
Const s_iNext : Array[1..3] Of Integer = (2,3,1);
Var
  fTrace : Single;
  fRoot  : Single;
  I,J,K  : Integer;

Begin
  // Algorithm in Ken Shoemake's article in 1987 SIGGRAPH course notes
  // article "Quaternion Calculus and Fast Animation".

  fTrace := kRot.M[1,1] + kRot.M[2,2] + kRot.M[3,3];

  If fTrace > 0 Then
  Begin
    // |w| > 1/2, may as well choose w > 1/2
    fRoot := Sqrt(fTrace + 1);  // 2w
    W     := 0.5 * fRoot;
    fRoot := 0.5 / fRoot;  // 1/(4w)
    X     := (kRot.M[3,2] - kRot.M[2,3]) * fRoot;
    Y     := (kRot.M[1,3] - kRot.M[3,1]) * fRoot;
    Z     := (kRot.M[2,1] - kRot.M[1,2]) * fRoot;
  End
  Else
  Begin
    // |w| <= 1/2
    I := 1;
    If kRot.M[2,2] > kRot.M[1,1] Then I := 2;
    If kRot.M[3,3] > kRot.M[I,I] Then I := 3;
    J := s_iNext[I];
    K := s_iNext[J];

    fRoot := Sqrt(kRot.M[I,I] - kRot.M[J,J] - kRot.M[K,K] + 1);
    Case I Of
      1: Begin
           X     := 0.5 * fRoot;
           fRoot := 0.5 / fRoot;
           Y     := (kRot.M[J,I] + kRot.M[I,J]) * fRoot;
           Z     := (kRot.M[K,I] + kRot.M[I,K]) * fRoot;
         End;
      2: Begin
           Y     := 0.5 * fRoot;
           fRoot := 0.5 / fRoot;
           Z     := (kRot.M[J,I] + kRot.M[I,J]) * fRoot;
           X     := (kRot.M[K,I] + kRot.M[I,K]) * fRoot;
         End;
      3: Begin
           Z     := 0.5 * fRoot;
           fRoot := 0.5 / fRoot;
           X     := (kRot.M[J,I] + kRot.M[I,J]) * fRoot;
           Y     := (kRot.M[K,I] + kRot.M[I,K]) * fRoot;
         End;
    End; // Case
    W := (kRot.M[K,J] - kRot.M[J,K]) * fRoot;
  End;
End; // TQuaternion.FromRotationMatrix

Procedure TQuaternion.From4x4Matrix(Const kRot: T4x4Matrix);
Const s_iNext : Array[1..3] Of Integer = (2,3,1);
Var
  fTrace : Single;
  fRoot  : Single;
  I,J,K  : Integer;

Begin
  // Algorithm in Ken Shoemake's article in 1987 SIGGRAPH course notes
  // article "Quaternion Calculus and Fast Animation".

  fTrace := kRot.M[1,1] + kRot.M[2,2] + kRot.M[3,3];

  If fTrace > 0 Then
  Begin
    // |w| > 1/2, may as well choose w > 1/2
    fRoot := Sqrt(fTrace + 1);  // 2w
    W     := 0.5 * fRoot;
    fRoot := 0.5 / fRoot;  // 1/(4w)
    X     := (kRot.M[3,2] - kRot.M[2,3]) * fRoot;
    Y     := (kRot.M[1,3] - kRot.M[3,1]) * fRoot;
    Z     := (kRot.M[2,1] - kRot.M[1,2]) * fRoot;
  End
  Else
  Begin
    // |w| <= 1/2
    I := 1;
    If kRot.M[2,2] > kRot.M[1,1] Then I := 2;
    If kRot.M[3,3] > kRot.M[I,I] Then I := 3;
    J := s_iNext[I];
    K := s_iNext[J];

    fRoot := Sqrt(kRot.M[I,I] - kRot.M[J,J] - kRot.M[K,K] + 1);
    Case I Of
      1: Begin
           X     := 0.5 * fRoot;
           fRoot := 0.5 / fRoot;
           Y     := (kRot.M[J,I] + kRot.M[I,J]) * fRoot;
           Z     := (kRot.M[K,I] + kRot.M[I,K]) * fRoot;
         End;
      2: Begin
           Y     := 0.5 * fRoot;
           fRoot := 0.5 / fRoot;
           Z     := (kRot.M[J,I] + kRot.M[I,J]) * fRoot;
           X     := (kRot.M[K,I] + kRot.M[I,K]) * fRoot;
         End;
      3: Begin
           Z     := 0.5 * fRoot;
           fRoot := 0.5 / fRoot;
           X     := (kRot.M[J,I] + kRot.M[I,J]) * fRoot;
           Y     := (kRot.M[K,I] + kRot.M[I,K]) * fRoot;
         End;
    End; // Case
    W := (kRot.M[K,J] - kRot.M[J,K]) * fRoot;
  End;
End; // TQuaternion.From4x4Matrix

Procedure TQuaternion.Transform(Var P: T3DPoint{I3dPoint});
Var
  fTx  : Single;
  fTy  : Single;
  fTz  : Single;
  fTwx : Single;
  fTwy : Single;
  fTwz : Single;
  fTxx : Single;
  fTxy : Single;
  fTxz : Single;
  fTyy : Single;
  fTyz : Single;
  fTzz : Single;
  X1,Y1,Z1 : Single;
  M        : Array[1..3,1..3] Of Single;

Begin
  fTx  := 2   * X;
  fTy  := 2   * Y;
  fTz  := 2   * Z;
  fTwx := fTx * W;
  fTwy := fTy * W;
  fTwz := fTz * W;
  fTxx := fTx * X;
  fTxy := fTy * X;
  fTxz := fTz * X;
  fTyy := fTy * Y;
  fTyz := fTz * Y;
  fTzz := fTz * Z;

  M[1,1] := 1 - (fTyy + fTzz);
  M[1,2] := fTxy - fTwz;
  M[1,3] := fTxz + fTwy;
  M[2,1] := fTxy + fTwz;
  M[2,2] := 1 - (fTxx + fTzz);
  M[2,3] := fTyz - fTwx;
  M[3,1] := fTxz - fTwy;
  M[3,2] := fTyz + fTwx;
  M[3,3] := 1 - (fTxx + fTyy);

  X1 := M[1,1] * P.X + M[1,2] * P.Y + M[1,3] * P.Z;
  Y1 := M[2,1] * P.X + M[2,2] * P.Y + M[2,3] * P.Z;
  Z1 := M[3,1] * P.X + M[3,2] * P.Y + M[3,3] * P.Z;

  P.X  := X1;
  P.Y  := Y1;
  P.Z  := Z1;
End; // TQuaternion.Transform

Procedure TQuaternion.Transform(Var PX,PY,PZ: Single);
Var
  fTx  : Single;
  fTy  : Single;
  fTz  : Single;
  fTwx : Single;
  fTwy : Single;
  fTwz : Single;
  fTxx : Single;
  fTxy : Single;
  fTxz : Single;
  fTyy : Single;
  fTyz : Single;
  fTzz : Single;
  X1,Y1,Z1 : Single;
  M        : Array[1..3,1..3] Of Single;

Begin
  fTx  := 2   * X;
  fTy  := 2   * Y;
  fTz  := 2   * Z;
  fTwx := fTx * W;
  fTwy := fTy * W;
  fTwz := fTz * W;
  fTxx := fTx * X;
  fTxy := fTy * X;
  fTxz := fTz * X;
  fTyy := fTy * Y;
  fTyz := fTz * Y;
  fTzz := fTz * Z;

  M[1,1] := 1 - (fTyy + fTzz);
  M[1,2] := fTxy - fTwz;
  M[1,3] := fTxz + fTwy;
  M[2,1] := fTxy + fTwz;
  M[2,2] := 1 - (fTxx + fTzz);
  M[2,3] := fTyz - fTwx;
  M[3,1] := fTxz - fTwy;
  M[3,2] := fTyz + fTwx;
  M[3,3] := 1 - (fTxx + fTyy);

  X1 := M[1,1] * PX + M[1,2] * PY + M[1,3] * PZ;
  Y1 := M[2,1] * PX + M[2,2] * PY + M[2,3] * PZ;
  Z1 := M[3,1] * PX + M[3,2] * PY + M[3,3] * PZ;

  PX  := X1;
  PY  := Y1;
  PZ  := Z1;
End; // TQuaternion.Transform

Procedure TQuaternion.InverseTransform(Var P: T3DPoint{I3dPoint});
Var
  fTx  : Single;
  fTy  : Single;
  fTz  : Single;
  fTwx : Single;
  fTwy : Single;
  fTwz : Single;
  fTxx : Single;
  fTxy : Single;
  fTxz : Single;
  fTyy : Single;
  fTyz : Single;
  fTzz : Single;
  X1,Y1,Z1 : Single;
  M        : Array[1..3,1..3] Of Single;

Begin
  fTx  := 2   * X;
  fTy  := 2   * Y;
  fTz  := 2   * Z;
  fTwx := fTx * W;
  fTwy := fTy * W;
  fTwz := fTz * W;
  fTxx := fTx * X;
  fTxy := fTy * X;
  fTxz := fTz * X;
  fTyy := fTy * Y;
  fTyz := fTz * Y;
  fTzz := fTz * Z;

  M[1,1] := 1 - (fTyy + fTzz);
  M[1,2] := fTxy - fTwz;
  M[1,3] := fTxz + fTwy;
  M[2,1] := fTxy + fTwz;
  M[2,2] := 1 - (fTxx + fTzz);
  M[2,3] := fTyz - fTwx;
  M[3,1] := fTxz - fTwy;
  M[3,2] := fTyz + fTwx;
  M[3,3] := 1 - (fTxx + fTyy);

  X1 := M[1,1] * P.X + M[2,1] * P.Y + M[3,1] * P.Z;
  Y1 := M[1,2] * P.X + M[2,2] * P.Y + M[3,2] * P.Z;
  Z1 := M[1,3] * P.X + M[2,3] * P.Y + M[3,3] * P.Z;

  P.X  := X1;
  P.Y  := Y1;
  P.Z  := Z1;
End; // TQuaternion.InverseTransform

Function TQuaternion.ToRotationMatrix: T3x3Matrix;
Var
  fTx  : Single;
  fTy  : Single;
  fTz  : Single;
  fTwx : Single;
  fTwy : Single;
  fTwz : Single;
  fTxx : Single;
  fTxy : Single;
  fTxz : Single;
  fTyy : Single;
  fTyz : Single;
  fTzz : Single;
  kRot : T3x3Matrix;

Begin
  fTx  := 2   * X;
  fTy  := 2   * Y;
  fTz  := 2   * Z;
  fTwx := fTx * W;
  fTwy := fTy * W;
  fTwz := fTz * W;
  fTxx := fTx * X;
  fTxy := fTy * X;
  fTxz := fTz * X;
  fTyy := fTy * Y;
  fTyz := fTz * Y;
  fTzz := fTz * Z;

  kRot := T3x3Matrix.Create;
  kRot.M[1,1] := 1 - (fTyy + fTzz);
  kRot.M[1,2] := fTxy - fTwz;
  kRot.M[1,3] := fTxz + fTwy;
  kRot.M[2,1] := fTxy + fTwz;
  kRot.M[2,2] := 1 - (fTxx + fTzz);
  kRot.M[2,3] := fTyz - fTwx;
  kRot.M[3,1] := fTxz - fTwy;
  kRot.M[3,2] := fTyz + fTwx;
  kRot.M[3,3] := 1 - (fTxx + fTyy);

  Result := kRot;
End; // TQuaternion.ToRotationMatrix

Procedure TQuaternion.ToRotationMatrix(kRot: T3x3Matrix);
Var
  fTx  : Single;
  fTy  : Single;
  fTz  : Single;
  fTwx : Single;
  fTwy : Single;
  fTwz : Single;
  fTxx : Single;
  fTxy : Single;
  fTxz : Single;
  fTyy : Single;
  fTyz : Single;
  fTzz : Single;

Begin
  fTx  := 2   * X;
  fTy  := 2   * Y;
  fTz  := 2   * Z;
  fTwx := fTx * W;
  fTwy := fTy * W;
  fTwz := fTz * W;
  fTxx := fTx * X;
  fTxy := fTy * X;
  fTxz := fTz * X;
  fTyy := fTy * Y;
  fTyz := fTz * Y;
  fTzz := fTz * Z;

  kRot.M[1,1] := 1 - (fTyy + fTzz);
  kRot.M[1,2] := fTxy - fTwz;
  kRot.M[1,3] := fTxz + fTwy;
  kRot.M[2,1] := fTxy + fTwz;
  kRot.M[2,2] := 1 - (fTxx + fTzz);
  kRot.M[2,3] := fTyz - fTwx;
  kRot.M[3,1] := fTxz - fTwy;
  kRot.M[3,2] := fTyz + fTwx;
  kRot.M[3,3] := 1 - (fTxx + fTyy);
End; // TQuaternion.ToRotationMatrix

Procedure TQuaternion.FromAngleAxis(Const rfAngle: Single; Const rkAxis: T3DPoint{I3dPoint});
Var fHalfAngle,fSin: Single;
Begin
  // assert:  axis[] is unit length
  //
  // The quaternion representing the rotation is
  //   q = cos(A/2)+sin(A/2)*(x*i+y*j+z*k)

  fHalfAngle := 0.5 * rfAngle;
  fSin       := Sin(fHalfAngle);
  w          := Cos(fHalfAngle);
  x          := fSin * rkAxis.X;
  y          := fSin * rkAxis.Y;
  z          := fSin * rkAxis.Z;
End; // TQuaternion.FromAngleAxis

Procedure TQuaternion.ToAngleAxis(Var rfAngle: Single; Var rkAxis: T3DPoint{I3dPoint});
Var fSqrLength,fInvlength: Single;
Begin
  // The quaternion representing the rotation is
  //   q = cos(A/2)+sin(A/2)*(x*i+y*j+z*k)

  fSqrLength := x * x + y * y + z * z;
  If fSqrLength > 0 Then
  Begin
    rfAngle    := 2 * ArcCos(w);
    fInvLength := 1 / Sqrt(fSqrLength);
    rkAxis.X   := x * fInvLength;
    rkAxis.Y   := y * fInvLength;
    rkAxis.Z   := z * fInvLength;
  End
  Else
  Begin
    // angle is 0 (mod 2*pi), so any axis will do
    rfAngle  := 0;
    rkAxis.X := 1;
    rkAxis.Y := 0;
    rkAxis.Z := 0;
  End;
End; // TQuaternion.ToAngleAxis

Procedure TQuaternion.ToAngleAxis(Var rfAngle,rkAxisX,rkAxisY,rkAxisZ: Single);
Var fSqrLength,fInvlength: Single;
Begin
  // The quaternion representing the rotation is
  //   q = cos(A/2)+sin(A/2)*(x*i+y*j+z*k)

  fSqrLength := x * x + y * y + z * z;
  If fSqrLength > 0 Then
  Begin
    rfAngle    := 2 * ArcCos(w);
    fInvLength := 1 / Sqrt(fSqrLength);
    rkAxisX    := x * fInvLength;
    rkAxisY    := y * fInvLength;
    rkAxisZ    := z * fInvLength;
  End
  Else
  Begin
    // angle is 0 (mod 2*pi), so any axis will do
    rfAngle := 0;
    rkAxisX := 1;
    rkAxisY := 0;
    rkAxisZ := 0;
  End;
End; // TQuaternion.ToAngleAxis

Procedure TQuaternion.FromAxes(Const akAxis: Array Of T3DPoint{I3dPoint});
Var
  kRot : T3x3Matrix;
  iCol : Integer;

Begin
  kRot := T3x3Matrix.Create;
  For iCol := 1 To 3 Do
  Begin
    kRot.M[1,iCol] := akAxis[iCol].X;
    kRot.M[2,iCol] := akAxis[iCol].Y;
    kRot.M[3,iCol] := akAxis[iCol].Z;
  End; // For iCol
  kRot.Free;
  FromRotationMatrix(kRot);
End; // TQuaternion.FromAxes

Procedure TQuaternion.FromAxes(Const xAxis,yAxis,zAxis: T3DPoint{I3dPoint});
Var kRot: T3x3Matrix;
Begin
  kRot := T3x3Matrix.Create;

  kRot.M[1,1] := xAxis.X;
  kRot.M[2,1] := xAxis.Y;
  kRot.M[3,1] := xAxis.Z;

  kRot.M[1,2] := yAxis.X;
  kRot.M[2,2] := yAxis.Y;
  kRot.M[3,2] := yAxis.Z;

  kRot.M[1,3] := zAxis.X;
  kRot.M[2,3] := zAxis.Y;
  kRot.M[3,3] := zAxis.Z;

  FromRotationMatrix(kRot);
  kRot.Free;
End; // TQuaternion.FromAxes

Procedure TQuaternion.ToAxes(Var akAxis: Array Of T3DPoint{I3dPoint});
Var
  kRot : T3x3Matrix;
  iCol : Integer;

Begin
  kRot := ToRotationMatrix;
  For iCol := 1 To 3 Do
  Begin
    akAxis[iCol].X := kRot.M[1,iCol];
    akAxis[iCol].Y := kRot.M[2,iCol];
    akAxis[iCol].Z := kRot.M[3,iCol];
  End; // For iCol
  kRot.Free;
End; // TQuaternion.ToAxes

Procedure TQuaternion.ToAxes(Var xAxis,yAxis,zAxis: T3DPoint{I3dPoint});
Var kRot: T3x3Matrix;
Begin
  kRot    := ToRotationMatrix;

  xAxis.X := kRot.M[1,1];
  xAxis.Y := kRot.M[2,1];
  xAxis.Z := kRot.M[3,1];

  yAxis.X := kRot.M[1,2];
  yAxis.Y := kRot.M[2,2];
  yAxis.Z := kRot.M[3,2];

  zAxis.X := kRot.M[1,3];
  zAxis.Y := kRot.M[2,3];
  zAxis.Z := kRot.M[3,3];

  kRot.Free;
End; // TQuaternion.ToAxes

Procedure TQuaternion.Add(Const Q: TQuaternion);
Begin
  W := W + Q.W;
  X := X + Q.X;
  Y := Y + Q.Y;
  Z := Z + Q.Z;
End; // TQuaternion.Add

Procedure TQuaternion.Subtract(Const Q: TQuaternion);
Begin
  W := W - Q.W;
  X := X - Q.X;
  Y := Y - Q.Y;
  Z := Z - Q.Z;
End; // TQuaternion.Subtract

Procedure TQuaternion.Multiply(Const Q: TQuaternion);
// If this is Q1, then the result is Q1 * Q
Var W1,X1,Y1,Z1: Single;
Begin
  // NOTE:  Multiplication is not generally commutative, so in most
  // cases p*q != q*p.

  W1 := w * Q.w - x * Q.x - y * Q.y - z * Q.z;
  X1 := w * Q.x + x * Q.w + y * Q.z - z * Q.y;
  Y1 := w * Q.y + y * Q.w + z * Q.x - x * Q.z;
  Z1 := w * Q.z + z * Q.w + x * Q.y - y * Q.x;

  W := W1;
  X := X1;
  Y := Y1;
  Z := Z1;
End; // TQuaternion.Multiply

Procedure TQuaternion.Multiply(fScalar: Single);
Begin
  W := W * fScalar;
  X := X * fScalar;
  Y := Y * fScalar;
  Z := Z * fScalar;
End; // TQuaternion.Multiply

Procedure TQuaternion.Negate;
Begin
  W := -W;
  X := -X;
  Y := -Y;
  Z := -Z;
End; // TQuaternion.Negate

Function TQuaternion.Dot(Const Q: TQuaternion): Single;
Begin
  Result := w * Q.w + x * Q.x + y * Q.y + z * Q.z;
End; // TQuaternion.Dot

Function TQuaternion.Norm: Single;
Begin
  Result := w * w + x * x + y * y + z * z;
End; // TQuaternion.Norm

Procedure TQuaternion.Normalize;
Var S: Single;
Begin
  S := Norm;
  If S > 0 Then
  Begin
    S := Sqrt(S);
    W := W / S;
    X := X / S;
    Y := Y / S;
    Z := Z / S;
  End;
End; // TQuaternion.Normalize

Procedure TQuaternion.Invert;
Var fNorm,fInvNorm,W1,X1,Y1,Z1: Single;
Begin
  fNorm := Norm;
  If fNorm > 0.0 Then
  Begin
    fInvNorm := 1.0 / fNorm;

    W1 :=  W * fInvNorm;
    X1 := -X * fInvNorm;
    Y1 := -Y * fInvNorm;
    Z1 := -Z * fInvNorm;

    W := W1;
    X := X1;
    Y := Y1;
    Z := Z1;
  End
  Else
  Begin
    // return an invalid result to flag the error

    W := 0;
    X := 0;
    Y := 0;
    Z := 0;
  End;
End; // TQuaternion.Invert

Procedure TQuaternion.UnitInvert;
Begin
  // assert:  'this' is unit length
  X := -X;
  Y := -Y;
  Z := -Z;
End; // TQuaternion.UnitInvert

Procedure TQuaternion.Exp;
Var
  fAngle  : Single;
  fSin    : Single;
  fCoeff  : Single;

Begin
  // If q = A*(x*i+y*j+z*k) where (x,y,z) is unit length, then
  // exp(q) = cos(A)+sin(A)*(x*i+y*j+z*k).  If sin(A) is near zero,
  // use exp(q) = cos(A)+A*(x*i+y*j+z*k) since A/sin(A) has limit 1.

  fAngle := Sqrt(x * x + y * y + z * z);
  fSin   := Sin(fAngle);
  w      := Cos(fAngle);

  If Abs(fSin) >= Quaternion_ms_fEpsilon Then
  Begin
    fCoeff := fSin / fAngle;
    x      := fCoeff * x;
    y      := fCoeff * y;
    z      := fCoeff * z;
  End;
End; // TQuaternion.Exp

Procedure TQuaternion.Log;
Var
  fAngle  : Single;
  fSin    : Single;
  fCoeff  : Single;

Begin
  // If q = cos(A)+sin(A)*(x*i+y*j+z*k) where (x,y,z) is unit length, then
  // log(q) = A*(x*i+y*j+z*k).  If sin(A) is near zero, use log(q) =
  // sin(A)*(x*i+y*j+z*k) since sin(A)/A has limit 1.

  If Abs(w) < 1.0 Then
  Begin
    fAngle := ArcCos(w);
    fSin   := Sin(fAngle);
    If Abs(fSin) >= Quaternion_ms_fEpsilon Then
    Begin
      fCoeff := fAngle / fSin;
      x      := fCoeff * x;
      y      := fCoeff * y;
      z      := fCoeff * z;
    End
    Else W := 0;
  End
  Else W := 0;
End; // TQuaternion.Log

Function TQuaternion.Equals(Const Q: TQuaternion): Boolean;
Begin
  Result := (x = Q.x) And (y = Q.y) And (z = Q.z) And (w = Q.w);
End; // Quaternion_Equals

Function TQuaternion.Equals(fW,fX,fY,fZ: Single): Boolean;
Begin
  Result := (x = fX) And (y = fY) And (z = fZ) And (w = fW);
End; // Quaternion_Equals

Procedure TQuaternion.Slerp(Q1: TQuaternion; T: Single);
// Slerp = Spherical Linear Interpolation
Const DOT_THRESHOLD = 0.9995;
Var
  DotProd : Single;
  Theta_0 : Single;
  Theta   : Single;
  W1      : Single;
  X1      : Single;
  Y1      : Single;
  Z1      : Single;
  ST      : Single;
  CT      : Single;

Begin
  // This and Q1 should be unit length or else something broken will happen.

  // Compute the cosine of the angle between the two vectors.

  DotProd := Dot(Q1);

  If DotProd > DOT_THRESHOLD Then
  Begin
    // If the inputs are too close for comfort, linearly interpolate
    // and normalize the result.

    W := W + T * (Q1.W - W);
    X := X + T * (Q1.X - X);
    Y := Y + T * (Q1.Y - Y);
    Z := Z + T * (Q1.Z - Z);
    Normalize;
  End
  Else
  Begin
    DotProd := Min(1,Max(-1,DotProd));      // Robustness: Stay within domain of acos()
    Theta_0 := ArcCos(DotProd);             // theta_0 = angle between input vectors
    Theta   := Theta_0 * T;                 // theta = angle between v0 and result

    CT      := Cos(Theta);
    ST      := Sin(Theta);

    W1 := W * CT;
    X1 := X * CT;
    Y1 := Y * CT;
    Z1 := Z * CT;

    Copy(Q1.W - W * DotProd, Q1.X - X * DotProd, Q1.Y - Y * DotProd, Q1.Z - Z * DotProd);
    Normalize;        // { v0, v2 } is now an orthonormal basis
    W := W * ST + W1;
    X := X * ST + X1;
    Y := Y * ST + Y1;
    Z := Z * ST + Z1;
  End;
End;

// -----------------------------
// TBasicPolygon
// -----------------------------

Constructor TBasicPolygon.Create;
Begin
  Flag  := False;
  Plane := TPlane.Create;
  SetLength(Vertices,0);
End; // TBasicPolygon.Create

Destructor TBasicPolygon.Destroy;
Var I: Integer;
Begin
  Plane.Free;
  For I := 0 To High(Vertices) Do {Vertices[I] := Nil;}Vertices[I].Free;
  SetLength(Vertices,0);
End; // TBasicPolygon.Destroy

Function TBasicPolygon.Classify(Plane: TPlane): TAlphaClassification;
Var
  NumPos : Integer;
  NumNeg : Integer;
  I      : Integer;
  RetVal : TAlphaClassification;

Begin
  NumPos := 0;
  NumNeg := 0;
  For I := 0 To High(Vertices) Do
  Begin
    RetVal := Plane.ClassifyPoint(Vertices[I]);
    Case RetVal Of
      acInFront: Inc(NumPos);
       acBehind: Inc(NumNeg);
    End; // Case
  End; // For I
       If (NumPos > 0) And (NumNeg = 0) Then Result := acInFront
  Else If (NumNeg > 0) And (NumPos = 0) Then Result := acBehind
  Else If (NumPos = 0) And (NumNeg = 0) Then Result := acCoinciding
  Else Result := acSpanning;
End; // TBasicPolygon.Classify

Function TBasicPolygon.Allocate(NumVertices: Integer): Boolean;
Var I: Integer;
Begin
  If NumVertices > 0 Then
  Begin
    If High(Vertices) >= 0 Then
    Begin
      For I := 0 To High(Vertices) Do {Vertices[I] := Nil;}Vertices[I].Free;
      SetLength(Vertices,0);
    End;
    SetLength(Vertices,NumVertices);
    For I := 0 To High(Vertices) Do Vertices[I] := T3DPoint.Create{Point};
    Result := True;
  End
  Else Result := False;
End; // TBasicPolygon.Allocate

Procedure TBasicPolygon.CalculatePlane;
Begin
  If High(Vertices) >= 2 Then
  Begin
    // Note how this goes in counterclockwise fashion (I usually go in clockwise fashion)

    Plane.Setup(Vertices[2],Vertices[1],Vertices[0]);
  End;
End; //TBasicPolygon.CalculatePlane

Procedure TBasicPolygon.InsertVert(V: T3DPoint{I3dPoint});
Begin
  SetLength(Vertices,High(Vertices) + 2);
  Vertices[High(Vertices)] := T3DPoint.Create{Point}(V);
  CalculatePlane;
End; // TBasicPolygon.InsertVert

Function TBasicPolygon.SplitPolygon(Plane: TPlane; PosPolygon,NegPolygon: TBasicPolygon): Boolean;
Var
  I              : Integer;
  PtA            : T3DPoint{I3dPoint};
  PtB            : T3DPoint{I3dPoint};
  SideA          : TAlphaClassification;
  SideB          : TAlphaClassification;
  RayDirection   : T3DPoint{I3dPoint};
  Intersection   : Single;
  IntersectionPt : T3DPoint{I3dPoint};

Begin
  Result := False;
  If (High(Vertices) >= 0) And (PosPolygon <> Nil) And (NegPolygon <> Nil) Then
  Begin
    PosPolygon.Plane.Copy(Plane);
    NegPolygon.Plane.Copy(Plane);
    PtA            := Vertices[High(Vertices)];
    SideA          := Plane.ClassifyPoint(PtA);
    RayDirection   := T3DPoint.Create{Point};
    IntersectionPt := T3DPoint.Create{Point};
    For I := 0 To High(Vertices) Do
    Begin
      PtB   := Vertices[I];
      SideB := Plane.ClassifyPoint(PtB);
      If SideB = acInFront Then
      Begin
        If SideA = acBehind Then
        Begin
          RayDirection.Copy(PtA,PtB);
          Intersection := Plane.IntersectionWithRay(PtA,RayDirection);
          If Intersection >= 0 Then
          Begin
            IntersectionPt.Copy(RayDirection);
            IntersectionPt.Multiply(Intersection);
            IntersectionPt.Add(PtA);
            PosPolygon.InsertVert(IntersectionPt);
            NegPolygon.InsertVert(IntersectionPt);
          End;
        End;
        PosPolygon.InsertVert(PtB);
      End
      Else If SideB = acBehind Then
      Begin
        If SideA = acInFront Then
        Begin
          RayDirection.Copy(PtA,PtB);
          Intersection := Plane.IntersectionWithRay(PtA,RayDirection);
          If Intersection >= 0 Then
          Begin
            IntersectionPt.Copy(RayDirection);
            IntersectionPt.Multiply(Intersection);
            IntersectionPt.Add(PtA);
            PosPolygon.InsertVert(IntersectionPt);
            NegPolygon.InsertVert(IntersectionPt);
          End;
        End;
        NegPolygon.InsertVert(PtB);
      End
      Else
      Begin
        PosPolygon.InsertVert(PtB);
        NegPolygon.InsertVert(PtB);
      End;
      PtA       := PtB;
      SideA     := SideB;
    End; // For I
    RayDirection.Free;
    IntersectionPt.Free;
{
    RayDirection   := Nil;
    IntersectionPt := Nil;
}    
    Result := True;
  End;
End; // TBasicPolygon.SplitPolygon

Procedure TBasicPolygon.Flip;
Var
  I,J,K : Integer;
  TV    : T3DPoint{I3dPoint};

Begin
  If High(Vertices) >= 0 Then
  Begin
    J := High(Vertices) + 1;
    For I := 0 To J Div 2 - 1 Do
    Begin
      K           := J - 1 - I;
      TV          := Vertices[K];
      Vertices[K] := Vertices[I];
      Vertices[I] := TV;
    End; // For I
    CalculatePlane;
  End;
End; // TBasicPolygon.Flip

// -----------------------------
// TFrustum
// -----------------------------

Constructor TFrustum.Create;
Var I: Integer;
Begin
  FDist1        := T3DPoint.Create{Point};
  FDist2        := T3DPoint.Create{Point};
  FObserver     := T3DPoint.Create{Point};
  FNormal       := T3DPoint.Create{Point}(1,0,0);
  FUp           := T3DPoint.Create{Point}(0,0,1);
  FNearDist     := 1;
  FFarDist      := 100;
  FViewAngle    := 30;
  FWidth        := 640;
  FHeight       := 480;
  SetLength(FPlane,0); // No planes at first
  FSphere       := TSphere.Create;
  FOrientation  := TQuaternion.Create;
  FViewMatrix   := T4x4Matrix.Create;
  FObserverFixed.X := 0;
  FObserverFixed.Y := 0;
  FObserverFixed.Z := 0;
  EViewMatrix      := TFVector.BuildVector(16);
  For I := 0 To 7 Do FPoints[I] := T3DPoint.Create{Point};
  UpdateViewMatrix;
End; // TFrustum.Create

Constructor TFrustum.Create(Observer,LookAt,Up: T3DPoint{I3dPoint}; ANearDist,AFarDist,AViewAngle: Single; Width,Height: Integer);
Var I: Integer;
Begin
  FDist1        := T3DPoint.Create{Point};
  FDist2        := T3DPoint.Create{Point};
  FObserver     := T3DPoint.Create{Point}(Observer);
  FNormal       := T3DPoint.Create{Point}(Observer,LookAt);
  FNormal.Normalize;
  FUp           := T3DPoint.Create{Point}(Up);
  FUp.Normalize;
  FNearDist     := ANearDist;
  FFarDist      := AFarDist;
  FViewAngle    := AViewAngle;
  FWidth        := Width;
  FHeight       := Height;
  SetLength(FPlane,6);
  For I := 0 To 5 Do FPlane[I] := TPlane.Create;
  For I := 0 To 7 Do FPoints[I] := T3DPoint.Create{Point};
  FSphere       := TSphere.Create;
  FOrientation  := TQuaternion.Create;
  FViewMatrix   := T4x4Matrix.Create;
  EViewMatrix      := TFVector.BuildVector(16);
  FObserverFixed.X := Round(FObserver.X * 65536);
  FObserverFixed.Y := Round(FObserver.Y * 65536);
  FObserverFixed.Z := Round(FObserver.Z * 65536);
  UpdateViewMatrix;
  SetupPlanes;
End; // TFrustum.Create

Constructor TFrustum.Create(OX,OY,OZ,LX,LY,LZ,UX,UY,UZ,ANearDist,AFarDist,AViewAngle: Single; Width,Height: Integer);
Var I: Integer;
Begin
  FDist1        := T3DPoint.Create{Point};
  FDist2        := T3DPoint.Create{Point};
  FObserver     := T3DPoint.Create{Point}(OX,OY,OZ);
  FNormal       := T3DPoint.Create{Point}(LX,LY,LZ);
  FNormal.Subtract(FObserver);
  FNormal.Normalize;
  FUp           := T3DPoint.Create{Point}(UX,UY,UZ);
  FUp.Normalize;
  FNearDist     := ANearDist;
  FFarDist      := AFarDist;
  FViewAngle    := AViewAngle;
  FWidth        := Width;
  FHeight       := Height;
  SetLength(FPlane,6);
  For I := 0 To 5 Do FPlane[I] := TPlane.Create;
  For I := 0 To 7 Do FPoints[I] := T3DPoint.Create{Point};
  FSphere       := TSphere.Create;
  FOrientation  := TQuaternion.Create;
  FViewMatrix   := T4x4Matrix.Create;
  EViewMatrix      := TFVector.BuildVector(16);
  FObserverFixed.X := Round(FObserver.X * 65536);
  FObserverFixed.Y := Round(FObserver.Y * 65536);
  FObserverFixed.Z := Round(FObserver.Z * 65536);
  UpdateViewMatrix;
  SetupPlanes;
End; // TFrustum.Create

Destructor TFrustum.Destroy;
Var I: Integer;
Begin
  For I := 0 To High(FPlane) Do FPlane[I].Free;
  For I := 0 To High(FPoints) Do {FPoints[I] := Nil;}FPoints[I].Free;
  SetLength(FPlane,0);
  FObserver.Free;
  FNormal.Free;
  FUp.Free;
  FDist1.Free;
  FDist2.Free;
{
  FObserver := Nil;
  FNormal   := Nil;
  FUp       := Nil;
  FDist1    := Nil;
  FDist2    := Nil;
}
  FSphere.Free;
  FOrientation.Free;
  FViewMatrix.Free;
  EViewMatrix.Free;
End; // TFrustum.Destroy

Function TFrustum.PointIsInside(P: T3DPoint{I3dPoint}): Boolean;
Var
  I : Integer;
  B : Boolean;

Begin
  I := 0;
  B := True;
  While (I <= High(FPlane)) And B Do
  Begin
    B := B And FPlane[I].ContainsPoint(P);
    Inc(I);
  End; // While
  Result := B;
End; // TFrustum.PointIsInside

Function TFrustum.PointIsInside(X,Y,Z: Single): Boolean;
Var
  I : Integer;
  B : Boolean;

Begin
  I := 0;
  B := True;
  While (I <= High(FPlane)) And B Do
  Begin
    B := B And FPlane[I].ContainsPoint(X,Y,Z);
    Inc(I);
  End; // While
  Result := B;
End; // TFrustum.PointIsInside

Function TFrustum.PolygonIsInside(Polygon: TBasicPolygon): Boolean;
Var
  I      : Integer;
  Inside : Boolean;

Begin
  I      := 0;
  Inside := True;
  While (I <= High(FPlane)) And Inside Do
  Begin
    If Polygon.Classify(FPlane[I]) = acBehind Then Inside := False;
  End; // While
  Result := Inside;
End; // TFrustum.PolygonIsInside

Function TFrustum.TriangleIsInside(P1,P2,P3: T3DPoint{I3dPoint}): Boolean;
Var
  I       : Integer;
  Visible : Boolean;
  Behind  : Boolean;

Begin
  I       := 0;
  Visible := True;
  While (I <= High(FPlane)) And Visible Do
  Begin
    Behind := (Not FPlane[I].ContainsPoint(P1)) And
              (Not FPlane[I].ContainsPoint(P2)) And
              (Not FPlane[I].ContainsPoint(P3));
    Visible := Visible And Not Behind;
    Inc(I);
  End; // While
  Result := Visible;
End; // TFrustum.TriangleIsInside
{
Function TFrustum.ContainsAxisAlignedBox(Box: TAxisAlignedBox): Boolean;
Var
  I,J     : Integer;
  Visible : Boolean;
  Behind  : Boolean;

Begin
  I       := 0;
  Visible := True;
  While (I <= High(FPlane)) And Visible Do
  Begin
    J      := 0;
    Behind := True;
    While (J < 8) And Behind Do
    Begin
      Behind := Behind And Not FPlane[I].ContainsPoint(Box.Corner[J]);
      Inc(J);
    End; // While
    Visible := Visible And Not Behind;
    Inc(I);
  End; // While
  Result := Visible;
End; // TFrustum.ContainsAxisAlignedBox
}
Procedure TFrustum.SetWidthAndHeight(Width,Height: Integer);
Begin
  If (FWidth <> Width) Or (FHeight <> Height) Then
  Begin
    FWidth  := Width;
    FHeight := Height;
    SetupPlanes;
  End;
End; // TFrustum.SetWidthAndHeight

Procedure TFrustum.SetViewAngle(AViewAngle: Single);
Begin
  If FViewAngle <> AViewAngle Then
  Begin
    FViewAngle := AViewAngle;
    SetupPlanes;
  End;
End; // TFrustum.SetViewAngle

Procedure TFrustum.SetNearDist(ANearDist: Single);
Begin
  If FNearDist <> ANearDist Then
  Begin
    FNearDist := ANearDist;
    SetupPlanes;
  End;
End; // TFrustum.SetNearDist

Procedure TFrustum.SetFarDist(AFarDist: Single);
Begin
  If FFarDist <> AFarDist Then
  Begin
    FFarDist := AFarDist;
    SetupPlanes;
  End;
End; // TFrustum.SetFarDist

Procedure TFrustum.SetObserver(P: T3DPoint{I3dPoint});
Begin
  If Not FObserver.Equals(P) Then
  Begin
    FObserver.Copy(P);
    FObserverFixed.X := Round(FObserver.X * 65536);
    FObserverFixed.Y := Round(FObserver.Y * 65536);
    FObserverFixed.Z := Round(FObserver.Z * 65536);
    SetupPlanes;
    UpdateViewMatrix;
  End;
End; // TFrustum.SetObserver

Procedure TFrustum.SetObserver(X,Y,Z: Single);
Begin
  If (FObserver.X <> X) Or (FObserver.Y <> Y) Or (FObserver.Z <> Z) Then
  Begin
    FObserver.Copy(X,Y,Z);
    FObserverFixed.X := Round(FObserver.X * 65536);
    FObserverFixed.Y := Round(FObserver.Y * 65536);
    FObserverFixed.Z := Round(FObserver.Z * 65536);
    SetupPlanes;
    UpdateViewMatrix;
  End;
End; // TFrustum.SetObserver

Procedure TFrustum.SetLookAt(P: T3DPoint{I3dPoint});
Var NX,NY,NZ: Single;
Begin
  NX := FNormal.X;
  NY := FNormal.Y;
  NZ := FNormal.Z;
  FNormal.Copy(P);
  FNormal.Subtract(FObserver);
  FNormal.Normalize;
  If (FNormal.X <> NX) Or (FNormal.Y <> NY) Or (FNormal.Z <> NZ) Then
  Begin
    SetupPlanes;
    UpdateViewMatrix;
  End;
End; // TFrustum.SetLookAt

Procedure TFrustum.SetLookAt(X,Y,Z: Single);
Var NX,NY,NZ: Single;
Begin
  NX := FNormal.X;
  NY := FNormal.Y;
  NZ := FNormal.Z;
  FNormal.Copy(X,Y,Z);
  FNormal.Subtract(FObserver);
  FNormal.Normalize;
  If (FNormal.X <> NX) Or (FNormal.Y <> NY) Or (FNormal.Z <> NZ) Then
  Begin
    SetupPlanes;
    UpdateViewMatrix;
  End;
End; // TFrustum.SetLookAt

Procedure TFrustum.SetUp(P: T3DPoint{I3dPoint});
Var UX,UY,UZ: Single;
Begin
  UX := FUp.X;
  UY := FUp.Y;
  UZ := FUp.Z;
  FUp.Copy(P);
  FUp.Normalize;
  If (FUp.X <> UX) Or (FUp.Y <> UY) Or (FUp.Z <> UZ) Then
  Begin
    SetupPlanes;
    UpdateViewMatrix;
  End;
End; // TFrustum.SetUp

Procedure TFrustum.SetUp(X,Y,Z: Single);
Var UX,UY,UZ: Single;
Begin
  UX := FUp.X;
  UY := FUp.Y;
  UZ := FUp.Z;
  FUp.Copy(X,Y,Z);
  FUp.Normalize;
  If (FUp.X <> UX) Or (FUp.Y <> UY) Or (FUp.Z <> UZ) Then
  Begin
    SetupPlanes;
    UpdateViewMatrix;
  End;
End; // TFrustum.SetUp

Procedure TFrustum.SetPosLookAtUp(PX,PY,PZ,LX,LY,LZ,UX,UY,UZ: Single);
Var
  NX,NY,NZ : Single;
  VX,VY,VZ : Single;

Begin
  If Not FObserver.Equals(PX,PY,PZ) Then
  Begin
    FObserver.Copy(PX,PY,PZ);
    FObserverFixed.X := Round(PX * 65536);
    FObserverFixed.Y := Round(PY * 65536);
    FObserverFixed.Z := Round(PZ * 65536);
    FNormal.Copy(LX,LY,LZ);
    FNormal.Subtract(FObserver);
    FNormal.Normalize;
    FUp.Copy(UX,UY,UZ);
    FUp.Normalize;
    SetupPlanes;
    UpdateViewMatrix;
  End
  Else
  Begin
    NX := FNormal.X;
    NY := FNormal.Y;
    NZ := FNormal.Z;
    FNormal.Copy(LX,LY,LZ);
    FNormal.Subtract(FObserver);
    FNormal.Normalize;
    If Not FNormal.Equals(NX,NY,NZ) Then
    Begin
      FObserver.Copy(PX,PY,PZ);
      FObserverFixed.X := Round(PX * 65536);
      FObserverFixed.Y := Round(PY * 65536);
      FObserverFixed.Z := Round(PZ * 65536);
      FUp.Copy(UX,UY,UZ);
      FUp.Normalize;
      SetupPlanes;
      UpdateViewMatrix;
    End
    Else
    Begin
      VX := FUp.X;
      VY := FUp.Y;
      VZ := FUp.Z;
      FUp.Copy(UX,UY,UZ);
      FUp.Normalize;
      If Not FUp.Equals(VX,VY,VZ) Then
      Begin
        FObserver.Copy(PX,PY,PZ);
        FObserverFixed.X := Round(PX * 65536);
        FObserverFixed.Y := Round(PY * 65536);
        FObserverFixed.Z := Round(PZ * 65536);
        SetupPlanes;
        UpdateViewMatrix;
      End;
    End;
  End;
End; // TFrustum.SetPosLookAtUp

Procedure TFrustum.AllocatePlanes(NumPlanes: Integer);
Var I: Integer;
Begin
  For I := 0 To High(FPlane) Do FPlane[I].Free;
  SetLength(FPlane,NumPlanes);
  For I := 0 To High(FPlane) Do FPlane[I] := TPlane.Create;
End; // TFrustum.AllocatePlanes

Procedure TFrustum.SetupPlanes;
Var
  I      : Integer;
  Vert   : T3DPoint{I3dPoint}; // Points up
  Horz   : T3DPoint{I3dPoint}; // Points to the left
  Depth  : Single;   // Cone depth
  Corner : Single;   // Distance from the center of the screen to a corner
  VA2Rad : Single;
  V      : Array[0..7] Of T3DPoint{I3dPoint};

Begin
  If (FHeight <> 0) And (ViewAngle <> 0) And (Abs(ViewAngle) <> 180) Then
  Begin
    For I := 0 To 7 Do V[I] := T3DPoint.Create{Point};

    // ViewAngle is the vertical angle; the horizontal angle depends on the window aspect ratio
{
    FProjectG := FHeight / Tan(ViewAngle * (Pi / 180));
    FProjectF := FWidth  / Tan((FWidth / FHeight) * ViewAngle * (Pi / 180));
}

    VA2Rad := ViewAngle * (Pi / 180) / 2;

    FProjectG := (FHeight / 2) / Tan(VA2Rad);
    FProjectF := (FWidth  / 2) / Tan((FWidth / FHeight) * VA2Rad);

    FProjectGFixed := Round(FProjectG * 65536);
    FProjectFFixed := Round(FProjectF * 65536);

    Vert := T3DPoint.Create{Point}(FUp);
    Vert.Multiply(Tan(VA2Rad));
    Horz := T3DPoint.Create{Point}(FUp);
    Horz.Cross(FNormal);
    Horz.Multiply(Tan(VA2Rad));
    Horz.Multiply(FWidth / FHeight);

    // Set up the corner points, first for a distance of 1

    // Lower left, as seen from the camera

//    V[0].SetToZero;
    V[0].Subtract(Vert);
    V[0].Add(Horz);
    V[0].Add(FNormal);

    // Upper left, as seen from the camera

    V[1].Copy(Vert);
    V[1].Add(Horz);
    V[1].Add(FNormal);

    // Upper right, as seen from the camera

    V[2].Copy(Vert);
    V[2].Subtract(Horz);
    V[2].Add(FNormal);

    // Lower right, as seen from the camera

//    V[3].SetToZero;
    V[3].Subtract(Vert);
    V[3].Subtract(Horz);
    V[3].Add(FNormal);

    // Copy the points to form the near set

    V[4].Copy(V[0]);
    V[5].Copy(V[1]);
    V[6].Copy(V[2]);
    V[7].Copy(V[3]);

    // Now scale the points to put them at the correct distances

    V[0].Multiply(FFarDist);
    V[1].Multiply(FFarDist);
    V[2].Multiply(FFarDist);
    V[3].Multiply(FFarDist);
    V[4].Multiply(FNearDist);
    V[5].Multiply(FNearDist);
    V[6].Multiply(FNearDist);
    V[7].Multiply(FNearDist);

    // Add the center position to each point

    For I := 0 To 7 Do V[I].Add(FObserver);

    // If this is a custom frustum with an unusual amount of planes, get rid of them and go
    // back to the standard six planes

    If High(FPlane) <> 5 Then AllocatePlanes(6);

    // Now that we know where the frustum bounds are, figure out the bounding planes

    FPlane[0].Setup(V[7],V[6],V[5]); // Near plane
    FPlane[1].Setup(V[0],V[1],V[2]); // Far plane
    FPlane[2].Setup(V[4],V[5],V[1]); // Left plane
    FPlane[3].Setup(V[3],V[2],V[6]); // Right plane
    FPlane[4].Setup(V[1],V[5],V[6]); // Top plane
    FPlane[5].Setup(V[0],V[3],V[7]); // Bottom plane

    // Calculate the frustum sphere center and size

    FSphere.Center.Copy(FNormal);
    FSphere.Center.Multiply(FNearDist + (FFarDist - FNearDist) / 2);
    FSphere.Center.Add(FObserver);
    FSphere.Radius := FSphere.Center.DistanceFrom(V[0]);

    // Calculate the frustum bounding cone angle (we already know the center and normal vector)

    If (Sin(VA2Rad) <> 0) And (Cos(VA2Rad) <> 0) Then
    Begin
      Depth         := (FHeight / 2) / Tan(VA2Rad);
      Corner        := Sqrt(Sqr(FWidth / 2) + Sqr(FHeight / 2));
      FConeFOVAngle := ArcTan(Corner / Depth);
      FConeSin2     := Sqr(Sin(FConeFOVAngle));
      FConeCos2     := Sqr(Cos(FConeFOVAngle));
      FConeSinRecip := 1 / Sin(FConeFOVAngle);
    End
    Else FConeFOVAngle := 0;

    // Cleanup

    For I := 0 To 7 Do
    Begin
      FPoints[I].Copy(V[I]);
      V[I].Free;
//      V[I] := Nil;
    End; // For I
    Vert.Free;
    Horz.Free;
//    Vert := Nil;
//    Horz := Nil;
  End;
End; // TFrustum.SetupPlanes

Function TFrustum.ContainsSphere(Sphere: TSphere): TSphereCullResult;
Var
  I           : Integer;
  Check       : TSphereCullResult;
  RadSinRecip : Single;
  DSqr        : Single;
  E           : Single;
  SRad        : Single;

Begin
  // Set the default value

  Check := scrOutside;

  // Make sure the object and our frustum have a nonzero size

  SRad := Sphere.Radius;
  If (SRad > 0) And (FSphere.Radius > 0) And (FConeFOVAngle <> 0) Then
  Begin
    // Do a quick sphere-sphere intersection test

    If FSphere.Intersects(Sphere) Then
    Begin
      // Now test against the frustum cone

      RadSinRecip := SRad * FConeSinRecip;
      FDist1.Copy(FObserver,Sphere.Center); // Sphere.Center - FObserver
      FDist2.Copy(FNormal);
      FDist2.Multiply(RadSinRecip);
      FDist2.Add(FDist1);
      DSqr := FDist2.GetSquaredLength;
      E    := FNormal.Dot(FDist2);
      If (E > 0) And (Sqr(E) >= DSqr * FConeCos2) Then
      Begin
        DSqr  := FDist1.GetSquaredLength;
        E     := -FNormal.Dot(FDist1);
        Check := scrOutside;
        If (E > 0) And (Sqr(E) >= DSqr * FConeSin2) Then
        Begin
          If DSqr <= SRad * SRad Then Check := scrInside;
        End
        Else Check := scrInside;

        // Continue only if the sphere is inside or intersects the cone

        If Check = scrInside Then
        Begin
          // Calculate our distances to each of the planes

          I     := 0;
          Check := scrInside;
          While (I <= High(FPlane)) And (Check <> scrOutside) Do
          Begin
            Check := FPlane[I].ContainsSphere(Sphere);
            Inc(I);
          End; // For I
        End;
      End;
    End;
  End;
  Result := Check;
End; // TFrustum.ContainsSphere

Function TFrustum.Adjust(Polygon: TBasicPolygon; Position: T3DPoint{I3dPoint}): TFrustum;
Var
  I,J,K    : Integer;
  Front    : TBasicPolygon;
  Back     : TBasicPolygon;
  Frustum  : TFrustum;
  DoDelete : Boolean;
  RetVal   : TAlphaClassification;

Begin
  Frustum  := TFrustum.Create;
  DoDelete := False;
  For I := 0 To High(FPlane) Do
  Begin
    RetVal := Polygon.Classify(FPlane[I]);
    If RetVal = acSpanning Then
    Begin
      Front := TBasicPolygon.Create;
      Back  := TBasicPolygon.Create;
      Polygon.SplitPolygon(FPlane[I],Front,Back);
      Back.Free;
      If DoDelete Then Polygon.Free;
      DoDelete := True;
      Polygon  := Front;
    End;
  End; // For I
  Frustum.AllocatePlanes(High(Polygon.Vertices) + 3); // Make room for the front and back planes
  K  := High(Polygon.Vertices) + 1;
  For I := 0 To K - 1 Do
  Begin
    J := (I + 1) Mod K;
    Frustum.FPlane[I + 2].Setup(Polygon.Vertices[J],Position,Polygon.Vertices[I]);
  End; // For I
  Frustum.FPlane[BackPlane].Copy(FPlane[BackPlane]);
  Frustum.FPlane[FrontPlane].Copy(FPlane[FrontPlane]);

  // Clean up

  If DoDelete Then Polygon.Free;
  Result := Frustum;
End; // TFrustum.Adjust

Procedure TFrustum.ClipSimplePolygon(Var Polygon: TSimplePolygon);
Var
  I   : Integer;
  Pos : TSimplePolygon;

Begin
  For I := 0 To High(FPlane) Do
  Begin
    FPlane[I].SplitSimplePolygonPosOnly(Polygon,Pos);
    Polygon.NumPoints := Pos.NumPoints;
    Move(Pos.Points,Polygon.Points,SizeOf(Polygon.Points));
  End; // For I
End; // TFrustum.ClipSimplePolygon

Procedure TFrustum.ClipFixedPointPolygon(Var Polygon: TSimpleScreenPolygon);
Var
  I   : Integer;
  Pos : TSimpleScreenPolygon;

Begin
  For I := 0 To High(FPlane) Do
  Begin
//    FPlane[I].SplitFixedPointPolygon(Polygon,Pos,Neg);
    Polygon.NumPoints := Pos.NumPoints;
    Move(Pos.Points,Polygon.Points,SizeOf(Polygon.Points));
  End; // For I
End; // TFrustum.ClipFixedPointPolygon

Procedure TFrustum.UpdateViewMatrix;
Var
  Left        : T3DPoint{I3dPoint};
  Rot         : T3x3Matrix;
  Trans       : T3DPoint{I3dPoint};
  I,J         : Integer;

Begin
  // ----------------------
  // Update the view matrix
  // ----------------------

  // View matrix is:
  //
  //  [ Lx  Uy  Dz  Tx  ]         <-- Left, Up, Direction, Translate
  //  [ Lx  Uy  Dz  Ty  ]
  //  [ Lx  Uy  Dz  Tz  ]
  //  [ 0   0   0   1   ]
  //
  // Where T = -(Transposed(Rot) * Pos)

  // This is most efficiently done using 3x3 Matrices

  // Get orientation from quaternion

  Left := T3DPoint.Create{Point};
  Left.Copy(FUp);
  Left.Cross(FNormal);
  FOrientation.FromAxes(Left,FUp,FNormal);
  Rot := FOrientation.ToRotationMatrix;

  // Make the translation relative to new axes

  Rot.Transpose;
  Trans := T3DPoint.Create{Point}(FObserver);
  Rot.Multiply(Trans);
  Trans.Negate;

  // Make final matrix

  FViewMatrix.LoadIdentity;
  FViewMatrix.LoadRotationMatrix(Rot);
  FViewMatrix.M[1,4] := Trans.X;
  FViewMatrix.M[2,4] := Trans.Y;
  FViewMatrix.M[3,4] := Trans.Z;

  // Put in projection terms

  FViewMatrix.M[1,1] := FViewMatrix.M[1,1] * -FProjectF;
  FViewMatrix.M[1,2] := FViewMatrix.M[1,2] * -FProjectF;
  FViewMatrix.M[1,3] := FViewMatrix.M[1,3] * -FProjectF;
  FViewMatrix.M[1,4] := FViewMatrix.M[1,4] * -FProjectF;
  FViewMatrix.M[2,1] := FViewMatrix.M[2,1] * -FProjectG;
  FViewMatrix.M[2,2] := FViewMatrix.M[2,2] * -FProjectG;
  FViewMatrix.M[2,3] := FViewMatrix.M[2,3] * -FProjectG;
  FViewMatrix.M[2,4] := FViewMatrix.M[2,4] * -FProjectG;

  // This matrix will be a transposed version that is relative to the observer

  For I := 0 To 15 Do EViewMatrix[I] := FViewMatrix.M[(I Mod 4) + 1, (I Div 4) + 1];
  EViewMatrix[12] := 0;
  EViewMatrix[13] := 0;
  EViewMatrix[14] := 0;
{
  // Put in projection terms

  EViewMatrix[0]  := EViewMatrix[0]  * -FProjectF;
  EViewMatrix[4]  := EViewMatrix[4]  * -FProjectF;
  EViewMatrix[8]  := EViewMatrix[8]  * -FProjectF;
  EViewMatrix[12] := EViewMatrix[12] * -FProjectF;
  EViewMatrix[1]  := EViewMatrix[1]  * FProjectG;
  EViewMatrix[5]  := EViewMatrix[5]  * FProjectG;
  EViewMatrix[9]  := EViewMatrix[9]  * FProjectG;
  EViewMatrix[13] := EViewMatrix[13] * FProjectG;
}
  // Make a fixed-point version

  For I := 1 To 4 Do
   For J := 1 To 4 Do ViewMatrixF[I,J] := Min(Max(-32767 * 65536,Round(FViewMatrix.M[I,J] * 65536)),32767 * 65536);

  // Make the fixed-point matrix relative to the observer position so we can avoid any
  // possibility of overflow

  ViewMatrixF[1,4] := 0;
  ViewMatrixF[2,4] := 0;
  ViewMatrixF[3,4] := 0;

  // Cleanup

  Rot.Free;
  Left.Free;
  Trans.Free;
//  Left := Nil;
//  Trans := Nil;
End; // TFrustum.UpdateViewMatrix

Procedure TFrustum.ProjectTransformedPoint(Var Source: TSimplePoint; Var Dest: TPoint);
Begin
  Dest.X := Round({-FProjectF *} (Source.X / Source.Z)) + (FWidth  Div 2);
  Dest.Y := Round({-FProjectG *} (Source.Y / Source.Z)) + (FHeight Div 2);
End; // TFrustum.ProjectTransformedPoint

Procedure TFrustum.ProjectTransformedFixedPoint(SourceX,SourceY,SourceZ: Integer; Var Dest: TPoint);
Begin
  Dest.X := (-FixedMul(FProjectFFixed,FixedDiv(SourceX,SourceZ)) Div 65536) + (FWidth  Div 2);
  Dest.Y := (-FixedMul(FProjectGFixed,FixedDiv(SourceY,SourceZ)) Div 65536) + (FHeight Div 2);
End; // TFrustum.ProjectTransformedFixedPoint

Procedure TFrustum.ProjectTransformedFixedPointToFixedPoint(SourceX,SourceY,SourceZ: Integer; Var Dest: TPoint);
Begin
  Dest.X := (-FixedMul(FProjectFFixed,FixedDiv(SourceX,SourceZ))) + ((FWidth  Div 2) Shl 16);
  Dest.Y := (-FixedMul(FProjectGFixed,FixedDiv(SourceY,SourceZ))) + ((FHeight Div 2) Shl 16);
End; // TFrustum.ProjectTransformedFixedPointToFixedPoint

Procedure TFrustum.ClipFixedPointPolygonToScreen(Var Polygon: TSimpleScreenPolygon);
Var
  I,J,K,L  : Integer;
  Bound    : Array[-1..3] Of TSimpleScreenPolygon;
  OutsideI : Integer;
  OutsideJ : Integer;
  WFixed   : Integer;
  HFixed   : Integer;
  Ratio    : Integer;
  Delta    : Integer;
  IPt      : Integer;
  JPt      : Integer;

Begin
  // Make fixed-point versions of the screen dimensions

  WFixed := (FWidth  - 1) Shl 16;
  HFixed := (FHeight - 1) Shl 16;

  Move(Polygon,Bound[-1],SizeOf(Polygon));

  // Clip to the screen edges

  L := 0;
  For K := -1 To 2 Do
  Begin
    Bound[L].NumPoints := 0;
    If Bound[K].NumPoints > 0 Then
    Begin
      I := Bound[K].NumPoints - 1;
      Case K Of
       0,1: IPt := Bound[K].Points[I].X;
      -1,2: IPt := Bound[K].Points[I].Y;
      End; // Case
      Case K Of
     -1,0: OutsideI := -IPt;            // Top,Left
        1: OutsideI := IPt - WFixed;    // Right
        2: OutsideI := IPt - HFixed;    // Bottom
      End; // Case
      For J := 0 To Bound[K].NumPoints - 1 Do
      Begin
        Case K Of
         0,1: JPt := Bound[K].Points[J].X;
        -1,2: JPt := Bound[K].Points[J].Y;
        End; // Case
        Case K Of
       -1,0: OutsideJ := -JPt;            // Top,Left
          1: OutsideJ := JPt - WFixed;    // Right
          2: OutsideJ := JPt - HFixed;    // Bottom
        End; // Case
        Delta := JPt - IPt;
        If OutsideJ <= 0 Then
        Begin
          If OutsideI > 0 Then
          Begin
            Ratio := Abs(FixedDiv(OutsideI,Delta));
            Case K Of
             -1: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := Bound[K].Points[I].X + FixedMul(Bound[K].Points[J].X - Bound[K].Points[I].X,Ratio);
                   Bound[L].Points[Bound[L].NumPoints].Y := 0;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[I].Z + FixedMul(Bound[K].Points[J].Z - Bound[K].Points[I].Z,Ratio);
                 End;
              0: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := 0;
                   Bound[L].Points[Bound[L].NumPoints].Y := Bound[K].Points[I].Y + FixedMul(Bound[K].Points[J].Y - Bound[K].Points[I].Y,Ratio);
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[I].Z + FixedMul(Bound[K].Points[J].Z - Bound[K].Points[I].Z,Ratio);
                 End;
              1: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := WFixed;
                   Bound[L].Points[Bound[L].NumPoints].Y := Bound[K].Points[I].Y + FixedMul(Bound[K].Points[J].Y - Bound[K].Points[I].Y,Ratio);
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[I].Z + FixedMul(Bound[K].Points[J].Z - Bound[K].Points[I].Z,Ratio);
                 End;
              2: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := Bound[K].Points[I].X + FixedMul(Bound[K].Points[J].X - Bound[K].Points[I].X,Ratio);
                   Bound[L].Points[Bound[L].NumPoints].Y := HFixed;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[I].Z + FixedMul(Bound[K].Points[J].Z - Bound[K].Points[I].Z,Ratio);
                 End;
            End; // Case
            Inc(Bound[L].NumPoints);
          End;
          Bound[L].Points[Bound[L].NumPoints] := Bound[K].Points[J];
          Inc(Bound[L].NumPoints);
        End
        Else
        Begin
          If OutsideI <= 0 Then
          Begin
            Ratio := Abs(FixedDiv(OutsideJ,Delta));
            Case K Of
             -1: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := Bound[K].Points[J].X + FixedMul(Bound[K].Points[I].X - Bound[K].Points[J].X,Ratio);
                   Bound[L].Points[Bound[L].NumPoints].Y := 0;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[J].Z + FixedMul(Bound[K].Points[I].Z - Bound[K].Points[J].Z,Ratio);
                 End;
              0: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := 0;
                   Bound[L].Points[Bound[L].NumPoints].Y := Bound[K].Points[J].Y + FixedMul(Bound[K].Points[I].Y - Bound[K].Points[J].Y,Ratio);
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[J].Z + FixedMul(Bound[K].Points[I].Z - Bound[K].Points[J].Z,Ratio);
                 End;
              1: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := WFixed;
                   Bound[L].Points[Bound[L].NumPoints].Y := Bound[K].Points[J].Y + FixedMul(Bound[K].Points[I].Y - Bound[K].Points[J].Y,Ratio);
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[J].Z + FixedMul(Bound[K].Points[I].Z - Bound[K].Points[J].Z,Ratio);
                 End;
              2: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := Bound[K].Points[J].X + FixedMul(Bound[K].Points[I].X - Bound[K].Points[J].X,Ratio);
                   Bound[L].Points[Bound[L].NumPoints].Y := HFixed;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[J].Z + FixedMul(Bound[K].Points[I].Z - Bound[K].Points[J].Z,Ratio);
                 End;
            End; // Case
            Inc(Bound[L].NumPoints);
          End;
        End;
        OutsideI := OutsideJ;
        I        := J;
        IPt      := JPt;
      End; // For J
    End;
    Inc(L);
  End; // For K
  Polygon.NumPoints := Bound[3].NumPoints;
  Polygon.Points    := Bound[3].Points;
End; // TFrustum.ClipFixedPointPolygonToScreen

Procedure TFrustum.ClipPolygonToScreen(Var Polygon: TSimplePolygon);
Var
  I,J,K,L  : Integer;
  Bound    : Array[-1..3] Of TSimplePolygon;
  OutsideI : Single;
  OutsideJ : Single;
  W        : Single;
  H        : Single;
  Ratio    : Single;
  Delta    : Single;
  IPt      : Single;
  JPt      : Single;

Begin
  // Make fixed-point versions of the screen dimensions

  W := FWidth  - 1;
  H := FHeight - 1;

  Move(Polygon,Bound[-1],SizeOf(Polygon));

  // Clip to the screen edges

  L := 0;
  For K := -1 To 2 Do
  Begin
    Bound[L].NumPoints := 0;
    If Bound[K].NumPoints > 0 Then
    Begin
      I := Bound[K].NumPoints - 1;
      Case K Of
       0,1: IPt := Bound[K].Points[I].X;
      -1,2: IPt := Bound[K].Points[I].Y;
      End; // Case
      Case K Of
     -1,0: OutsideI := -IPt;            // Top,Left
        1: OutsideI := IPt - W;         // Right
        2: OutsideI := IPt - H;         // Bottom
      End; // Case
      For J := 0 To Bound[K].NumPoints - 1 Do
      Begin
        Case K Of
         0,1: JPt := Bound[K].Points[J].X;
        -1,2: JPt := Bound[K].Points[J].Y;
        End; // Case
        Case K Of
       -1,0: OutsideJ := -JPt;            // Top,Left
          1: OutsideJ := JPt - W;         // Right
          2: OutsideJ := JPt - H;         // Bottom
        End; // Case
        Delta := JPt - IPt;
        If OutsideJ <= 0 Then
        Begin
          If OutsideI > 0 Then
          Begin
            Ratio := Abs(OutsideI / Delta);
            Case K Of
             -1: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := Bound[K].Points[I].X + (Bound[K].Points[J].X - Bound[K].Points[I].X) * Ratio;
                   Bound[L].Points[Bound[L].NumPoints].Y := 0;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[I].Z + (Bound[K].Points[J].Z - Bound[K].Points[I].Z) * Ratio;
                 End;
              0: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := 0;
                   Bound[L].Points[Bound[L].NumPoints].Y := Bound[K].Points[I].Y + (Bound[K].Points[J].Y - Bound[K].Points[I].Y) * Ratio;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[I].Z + (Bound[K].Points[J].Z - Bound[K].Points[I].Z) * Ratio;
                 End;
              1: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := W;
                   Bound[L].Points[Bound[L].NumPoints].Y := Bound[K].Points[I].Y + (Bound[K].Points[J].Y - Bound[K].Points[I].Y) * Ratio;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[I].Z + (Bound[K].Points[J].Z - Bound[K].Points[I].Z) * Ratio;
                 End;
              2: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := Bound[K].Points[I].X + (Bound[K].Points[J].X - Bound[K].Points[I].X) * Ratio;
                   Bound[L].Points[Bound[L].NumPoints].Y := H;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[I].Z + (Bound[K].Points[J].Z - Bound[K].Points[I].Z) * Ratio;
                 End;
            End; // Case
            Inc(Bound[L].NumPoints);
          End;
          Bound[L].Points[Bound[L].NumPoints] := Bound[K].Points[J];
          Inc(Bound[L].NumPoints);
        End
        Else
        Begin
          If OutsideI <= 0 Then
          Begin
            Ratio := Abs(OutsideJ / Delta);
            Case K Of
             -1: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := Bound[K].Points[J].X + (Bound[K].Points[I].X - Bound[K].Points[J].X) * Ratio;
                   Bound[L].Points[Bound[L].NumPoints].Y := 0;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[J].Z + (Bound[K].Points[I].Z - Bound[K].Points[J].Z) * Ratio;
                 End;
              0: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := 0;
                   Bound[L].Points[Bound[L].NumPoints].Y := Bound[K].Points[J].Y + (Bound[K].Points[I].Y - Bound[K].Points[J].Y) * Ratio;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[J].Z + (Bound[K].Points[I].Z - Bound[K].Points[J].Z) * Ratio;
                 End;
              1: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := W;
                   Bound[L].Points[Bound[L].NumPoints].Y := Bound[K].Points[J].Y + (Bound[K].Points[I].Y - Bound[K].Points[J].Y) * Ratio;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[J].Z + (Bound[K].Points[I].Z - Bound[K].Points[J].Z) * Ratio;
                 End;
              2: Begin
                   Bound[L].Points[Bound[L].NumPoints].X := Bound[K].Points[J].X + (Bound[K].Points[I].X - Bound[K].Points[J].X) * Ratio;
                   Bound[L].Points[Bound[L].NumPoints].Y := H;
                   Bound[L].Points[Bound[L].NumPoints].Z := Bound[K].Points[J].Z + (Bound[K].Points[I].Z - Bound[K].Points[J].Z) * Ratio;
                 End;
            End; // Case
            Inc(Bound[L].NumPoints);
          End;
        End;
        OutsideI := OutsideJ;
        I        := J;
        IPt      := JPt;
      End; // For J
    End;
    Inc(L);
  End; // For K
  Polygon.NumPoints := Bound[3].NumPoints;
  Polygon.Points    := Bound[3].Points;
End; // TFrustum.ClipPolygonToScreen

Procedure TFrustum.ClipIntegerPolygonToScreen(Var Polygon: TSimpleScreenPolygon);
Var
  I,J,K,L  : Integer;
  IBound   : Array[0..2] Of TSimpleScreenPolygon;
  OutsideI : Integer;
  OutsideJ : Integer;
  W        : Integer;
  H        : Integer;
  Ratio    : Single;
  Delta    : Integer;
  IPt      : Integer;
  JPt      : Integer;
  PIPt     : PSimpleScreenPoint;
  PJPt     : PSimpleScreenPoint;
  PLPt     : PSimpleScreenPoint;
  PSSP     : Array[0..4] Of PSimpleScreenPolygon;
  PLNP     : PInteger;
  KNP      : Integer;

Begin
  // Set up polygon pointers so we don't have to copy anything

  PSSP[0] := @Polygon;
  PSSP[1] := @IBound[0];
  PSSP[2] := @IBound[1];
  PSSP[3] := @IBound[2];
  PSSP[4] := @Polygon;

  // Make fixed-point versions of the screen dimensions

  W := FWidth  - 1;
  H := FHeight - 1;

  // Clip to the screen edges

  L := 1;

  // The value of K determines which screen edge we test against
  //
  // 0 .... Top edge
  // 1 .... Left edge
  // 2 .... Right edge
  // 3 .... Bottom edge

  For K := 0 To 3 Do
  Begin
    PLNP  := @(PSSP[L].NumPoints);
    PLNP^ := 0;
    PLPt  := @(PSSP[L].Points[0]);
    I     := PSSP[K].NumPoints - 1;
    If I >= 1 Then
    Begin
      PIPt := @(PSSP[K].Points[I]);
      Case K Of
       1,2: IPt := PIPt.X;   // Left,Right
       0,3: IPt := PIPt.Y;   // Top,Bottom
      End; // Case
      Case K Of
      0,1: OutsideI := -IPt;            // Top,Left
        2: OutsideI := IPt - W;         // Right
        3: OutsideI := IPt - H;         // Bottom
      End; // Case
      PJPt := @(PSSP[K].Points[0]);
      KNP  := PSSP[K].NumPoints;
      For J := 0 To KNP - 1 Do
      Begin
        Case K Of
         1,2: JPt := PJPt.X;  // Left,Right
         0,3: JPt := PJPt.Y;  // Top,Bottom
        End; // Case
        Case K Of
        0,1: OutsideJ := -JPt;            // Top,Left
          2: OutsideJ := JPt - W;         // Right
          3: OutsideJ := JPt - H;         // Bottom
        End; // Case

        If OutsideJ <= 0 Then
        Begin
          // Point J is visible

          If OutsideI > 0 Then
          Begin
            // Point I is not visible; create a point between them at the screen edge

            Delta := JPt - IPt;
            Ratio := Abs(OutsideI / Delta);
            Case K Of
              0: Begin
                   PLPt.X := PIPt.X + Round((PJPt.X - PIPt.X) * Ratio);
                   PLPt.Y := 0;
                   PLPt.Z := PIPt.Z + Round((PJPt.Z - PIPt.Z) * Ratio);
                 End;
              1: Begin
                   PLPt.X := 0;
                   PLPt.Y := PIPt.Y + Round((PJPt.Y - PIPt.Y) * Ratio);
                   PLPt.Z := PIPt.Z + Round((PJPt.Z - PIPt.Z) * Ratio);
                 End;
              2: Begin
                   PLPt.X := W;
                   PLPt.Y := PIPt.Y + Round((PJPt.Y - PIPt.Y) * Ratio);
                   PLPt.Z := PIPt.Z + Round((PJPt.Z - PIPt.Z) * Ratio);
                 End;
              3: Begin
                   PLPt.X := PIPt.X + Round((PJPt.X - PIPt.X) * Ratio);
                   PLPt.Y := H;
                   PLPt.Z := PIPt.Z + Round((PJPt.Z - PIPt.Z) * Ratio);
                 End;
            End; // Case
            Inc(PLNP^);
            Inc(LongWord(PLPt),SizeOf(TSimpleScreenPoint));
          End;

          // Create a copy of point J

          PLPt.X := PJPt.X;
          PLPt.Y := PJPt.Y;
          PLPt.Z := PJPt.Z;
          Inc(PLNP^);
          Inc(LongWord(PLPt),SizeOf(TSimpleScreenPoint));
        End
        Else
        Begin
          // Point J is not visible

          If OutsideI <= 0 Then
          Begin
            // Point I is visible; create a point between them at the screen edge

            Delta := JPt - IPt;
            Ratio := Abs(OutsideJ / Delta);
            Case K Of
              0: Begin
                   PLPt.X := PJPt.X + Round((PIPt.X - PJPt.X) * Ratio);
                   PLPt.Y := 0;
                   PLPt.Z := PJPt.Z + Round((PIPt.Z - PJPt.Z) * Ratio);
                 End;
              1: Begin
                   PLPt.X := 0;
                   PLPt.Y := PJPt.Y + Round((PIPt.Y - PJPt.Y) * Ratio);
                   PLPt.Z := PJPt.Z + Round((PIPt.Z - PJPt.Z) * Ratio);
                 End;
              2: Begin
                   PLPt.X := W;
                   PLPt.Y := PJPt.Y + Round((PIPt.Y - PJPt.Y) * Ratio);
                   PLPt.Z := PJPt.Z + Round((PIPt.Z - PJPt.Z) * Ratio);
                 End;
              3: Begin
                   PLPt.X := PJPt.X + Round((PIPt.X - PJPt.X) * Ratio);
                   PLPt.Y := H;
                   PLPt.Z := PJPt.Z + Round((PIPt.Z - PJPt.Z) * Ratio);
                 End;
            End; // Case
            Inc(PLNP^);
            Inc(LongWord(PLPt),SizeOf(TSimpleScreenPoint));
          End;
        End;
        OutsideI := OutsideJ;
        I        := J;
        IPt      := JPt;
        PIPt     := PJPt;
        Inc(LongWord(PJPt),SizeOf(TSimpleScreenPoint));
      End; // For J
    End;
    Inc(L);
  End; // For K
End; // TFrustum.ClipIntegerPolygonToScreen

Procedure TFrustum.ClipTransformedPolygonToNearDist(Var Polygon: TSimplePolygon; Dest: PSimplePolygon);
Var
  I,J      : Integer;
  Pos      : PSimplePolygon;
  IPos     : TSimplePolygon;
  OutsideI : Single;
  OutsideJ : Single;
  Ratio    : Single;
  Delta    : Single;
  IPt      : Single;
  JPt      : Single;
  PIPt     : PSimplePoint;
  PJPt     : PSimplePoint;
  PLPt     : PSimplePoint;

Begin
  // Make fixed-point versions of the screen dimensions

  If Polygon.NumPoints > 1 Then
  Begin
    If Dest <> Nil
     Then Pos := Dest
     Else Pos := @IPos;

    Pos.NumPoints := 0;
    PLPt          := @(Pos.Points[0]);
    I             := Polygon.NumPoints - 1;
    PIPt          := @(Polygon.Points[I]);
    IPt           := PIPt.Z;
    OutsideI      := FNearDist - IPt;
    PJPt          := @(Polygon.Points[0]);
    For J := 0 To Polygon.NumPoints - 1 Do
    Begin
      JPt      := PJPt.Z;
      OutsideJ := FNearDist - JPt;
      Delta    := JPt - IPt;
      If OutsideJ <= 0 Then
      Begin
        // Point J is visible

        If OutsideI > 0 Then
        Begin
          // Point I is not visible; create a point between them at the near plane

          Ratio := Abs(OutsideI / Delta);
          PLPt.X := PIPt.X + (PJPt.X - PIPt.X) * Ratio;
          PLPt.Y := PIPt.Y + (PJPt.Y - PIPt.Y) * Ratio;
          PLPt.Z := FNearDist;
          Inc(Pos.NumPoints);
          Inc(LongWord(PLPt),SizeOf(TSimplePoint));
        End;

        // Create a copy of point J

        PLPt.X := PJPt.X;
        PLPt.Y := PJPt.Y;
        PLPt.Z := PJPt.Z;
        Inc(Pos.NumPoints);
        Inc(LongWord(PLPt),SizeOf(TSimplePoint));
      End
      Else
      Begin
        // Point J is not visible

        If OutsideI <= 0 Then
        Begin
          // Point I is visible; create a point between them at the near plane

          Ratio := Abs(OutsideJ / Delta);
          PLPt.X := PJPt.X + (PIPt.X - PJPt.X) * Ratio;
          PLPt.Y := PJPt.Y + (PIPt.Y - PJPt.Y) * Ratio;
          PLPt.Z := FNearDist;
          Inc(Pos.NumPoints);
          Inc(LongWord(PLPt),SizeOf(TSimplePoint));
        End;
      End;
      OutsideI := OutsideJ;
      I        := J;
      IPt      := JPt;
      PIPt     := PJPt;
      Inc(LongWord(PJPt),SizeOf(TSimplePoint));
    End; // For J
    If Dest = Nil Then
    Begin
      Polygon.NumPoints := Pos.NumPoints;
      Polygon.Points    := Pos.Points;
    End;
  End;
End; // TFrustum.ClipTransformedPolygonToNearDist

Initialization
  EnableMMX  := False;
  EnableSSE  := False;
  EnableSSE2 := False;
  EnableSSE3 := False;
  FindMMX;
  If EnableMMX Then FindSSE;
//  PointCreator := T3DPoint.Create;
Finalization
//  PointCreator.Free;  
end.
