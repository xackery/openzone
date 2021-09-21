// *****************************************************************************
//                              Exentia Library
//                 Design by:   Stefano Tommesani 2000->2004
//                 Extended by: Joao Paulo Schwarz Schuler
//                              Robert Lee
//                              Chris Rorden
//                              Patrick van Laake
//                              Roman "GoodOk" Gudchenko
//
//                              version 0.6.0
//                  Web: http://www.tommesani.com/Exentia.html
//                       E-mail: stefano@tommesani.com
//
// The contents of this file are subject to the Mozilla Public License Version
// 1.0; you may not use this file except in compliance with the License.
// You may obtain a copy of the License at http://www.mozilla.org/MPL/
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// for the specific language governing rights and limitations under the License.
// *****************************************************************************

{$DEFINE DELPHI_CODE}
unit Exentia;

interface

{$IFDEF LINUX}
uses SysUtils,Qdialogs;
{$ELSE}
uses SysUtils,dialogs;
{$ENDIF}

// lib version info:
const
  ExentiaVersionMajor   = 0;
  ExentiaVersionMinor   = 6;
  ExentiaVersionRelease = 0;

{$I Exentia.inc}

// processor depends constant:
{$DEFINE PENTIUM4}
{.$DEFINE ATHLON}
{$IFDEF PENTIUM4}
const CacheLineLength = 128;
{$ELSE}
{$IFDEF ATHLON}
const CacheLineLength = 128;
{$ELSE}
const CacheLineLength = 64;
{$ENDIF}
{$ENDIF}

type
  TSingleArray = packed array [0..1000000] of single;
  PSingleArray = ^TSingleArray;

  TPointerArray = packed array [0..1000000] of pointer;
  PPointerArray = ^TPointerArray;

  TLongWordArray = packed array [0..1000000] of LongWord;
  PLongWordArray = ^TLongWordArray;

type

  { container "vector of aligned vectors"}
  TFMatrix = class;

  { container "matrix of aligned vectors"}
  TFCube = class;

  TFVector = class(TObject)
  private
    function GetDataElement(index : integer) : single;
    procedure SetDataElement(index : integer; const value : single);
  protected
    // Had to make the members protected so we can override the classes

    AlignedArray   : PSingleArray;
    AllocatedArray : pointer;
    NumElements    : integer;
    fSourceLength  : integer;
    function CheckIndexParams(xStartInd, xCount: integer) : boolean;
  public
    property DataArray : PSingleArray read AlignedArray;
    property Data[index : integer] : single read GetDataElement write SetDataElement; default;
    constructor Create(const ALength : integer);
    constructor CreateUsingArray(var A:array of Single);
    constructor CreateUsingAlignedArrayPart(xSA : pSingleArray; const xLength : integer); 
    class function BuildUsingAlignedArrayPart(xSA : pSingleArray; const xLength : integer) : TFVector;
    constructor Copy(const V : TFVector); // Self = V
    destructor Destroy;override;
    class function BuildVector(const ALength : integer) : TFVector;
    class function BuildUsingArray(var A:array of Single) : TFVector;
    class function CopyVector(const V : TFVector) : TFVector;
    class function isSSE : boolean;
    class function is3DNow : boolean;
    class function isCMOV : boolean; // Not used here but useful nevertheless
    procedure UseArray(var A:array of single);
    procedure UseMatrixRow(xMtx : TFMatrix; xRowInd : integer);
    procedure UseCubeCell(xCube : TFCube; xRowInd, xColInd : integer);
    procedure SetLength(const ALength : integer);
    procedure CopyArray(const V : TFVector);
    procedure ImportArray(const Source : PSingleArray; const Length : integer = 0);
    procedure ExportArray(Destination : PSingleArray; const Length : integer = 0);
    procedure ClearArray; // Self = 0
    procedure SetArray(value : single);  // Self[i] = value
    function Equal(V : TFVector) : boolean;
    // basic operations
    procedure Add(V : TFVector); overload; virtual; abstract;  // Self = Self + V
    procedure Sub(V : TFVector); overload; virtual; abstract;  // Self = Self - V
    procedure Mul(V : TFVector); overload; virtual; abstract;  // Self = Self * V
    procedure Divide(V : TFVector); overload; virtual; abstract;  // Self = Self / V

    procedure Sqrt(V : TFVector); virtual; abstract;  // Self = Sqrt(V)
    procedure Reciprocal(V : TFVector); virtual; abstract;  // Self = 1 / V
    procedure RecSqrt(V : TFVector); virtual; abstract;  // Self = 1 / Sqrt(V)
    procedure Max(V : TFVector); overload; virtual; abstract;  // Self = Max(Self, V)
    procedure Min(V : TFVector); overload; virtual; abstract;  // Self = Min(Self, V)

    procedure LogicalAnd(V : TFVector); virtual; abstract;  // Self = Self and V
    procedure LogicalOr(V : TFVector); virtual; abstract;   // Self = Self or V
    procedure LogicalXor(V : TFVector); virtual; abstract;  // Self = Self xor V
    procedure CmpGreater(V : TFVector); virtual; abstract;       // Self = Self > V
    procedure CmpLower(V : TFVector); virtual; abstract;         // Self = Self < V
    procedure CmpEqual(V : TFVector); virtual; abstract;         // Self = Self == V
    procedure CmpGreaterEqual(V : TFVector); virtual; abstract;  // Self = Self >= V
    procedure CmpLowerEqual(V : TFVector); virtual; abstract;    // Self = Self <= V

    // complex operations
    procedure AddSquare(V : TFVector); overload; virtual; abstract;   // Self = Self + V^2
    function InnerSum : single; virtual; abstract;  // result = Sum(Self[i])
    function InnerSumAbs : single; virtual; abstract;  // result = Sum(abs(Self[i]))
    function DotProduct(V : TFVector) : single; virtual; abstract;  // result = Sum(Self[i] * V[i])
    function MaxValue : single; virtual; abstract;  // result = Max(Self[i])
    function MaxAbsValue : single; virtual; abstract;  // result = Max(abs(Self[i]))
    function IndexMaxValue : integer; virtual; abstract;  // result = index of Max(Self)
    function MinValue : single; virtual; abstract;  // result = Min(Self[i])
    function MinAbsValue : single; virtual; abstract;  // result = Min(abs(Self[i]))
    function IndexMinValue : integer; virtual; abstract;  // result = index of Min(Self)
    function Mean : single; virtual; abstract;  // result = Sum(Self[i]) / NumElements
    procedure Scale(value : single); virtual; abstract;  // Self = value * Self

    // basic dual operations
    procedure Add(V1, V2 : TFVector); overload; virtual; abstract;  // Self := V1 + V2
    procedure Add(V1, V2 : TFVector; const xStartInd, xCount : integer); overload; virtual;  abstract;  // Self = V1 + V2; applies for elements [StartInd] ... [StartInd + Count -1]
    procedure Sub(V1, V2 : TFVector); overload; virtual; abstract;  // Self := V1 - V2
    procedure Sub(V1, V2 : TFVector; const xStartInd, xCount : integer); overload; virtual;  abstract;
    procedure Mul(V1, V2 : TFVector); overload; virtual; abstract;  // Self := V1*V2
    procedure Mul(V1, V2 : TFVector; const xStartInd, xCount : integer); overload; virtual;  abstract;
    procedure Divide(V1, V2 : TFVector); overload; virtual; abstract;  // Self := V1/V2
    procedure Divide(V1, V2 : TFVector; const xStartInd, xCount : integer); overload; virtual;  abstract;
    procedure MulDiv(V1, V2 : TFVector); virtual; abstract;  // Self = Self*V1 / V2

    // additional operations
    procedure Sqr(V : TFVector); virtual; abstract;  // Self = Sqr(V)
    procedure AddSquare(V1, V2 : TFVector); overload; virtual; abstract;   // Self = V1^2 + V2^2
    procedure AddSquare(V1, V2 : TFVector; const xStartInd, xCount : integer); overload; virtual;  abstract;
    procedure Max(V1, V2 : TFVector); overload; virtual; abstract;  // Self = Max(Self, V)
    procedure Max(V1, V2 : TFVector; const xStartInd, xCount : integer); overload; virtual; abstract;  // Self = Max(Self, V)
    procedure Min(V1, V2 : TFVector); overload; virtual; abstract;  // Self = Max(V1, V2)
    procedure Min(V1, V2 : TFVector; const xStartInd, xCount : integer); overload; virtual; abstract;  // Self = Max(V1, V2)

    procedure Add(value : single); overload; virtual; abstract; // Self = Self + value
    procedure Combine(V1, V2 : tFVector; xA, xB : single); overload; virtual; abstract; // Self := A*V1 + B*V2
    procedure Combine(V : tFVector; xA : single); overload; virtual; abstract; // Self := Self + A*V1
    procedure Lerp(V : tFVector; xT : single); overload; virtual; abstract;       // Self := Self + (V - Self) * t;
    procedure Lerp(V1, V2 : tFVector; xT : single); overload; virtual; abstract;       // Self := V1 + (V2 - V1) * t;

    function InnerSqrSum : single; virtual; abstract;  // result = Sum(sqr(Self[i]))
    function NormL1(V : TFVector) : single; virtual; abstract; // result = Sum(abs(Self[i] - V[i])
    function NormL2(V : TFVector) : single; virtual; abstract; // result = Sun(sqr(Self[i] - V[i]);

    procedure MinMaxValues(var yMin, yMax : single);  overload; virtual; abstract;
    procedure MinMaxValues(var yMin, yMax : single; xStartInd, xCount : integer);  overload; virtual; abstract;
    procedure IndexMinMaxValues(var yMinInd, yMaxInd : integer);  overload; virtual; abstract;
    procedure IndexMinMaxValues(var yMinInd, yMaxInd : integer; const xStartInd, xCount : integer);  overload; virtual; abstract;
    procedure MinMaxAbsValues(var yMin, yMax : single);  overload; virtual; abstract;
    procedure MinMaxAbsValues(var yMin, yMax : single; xStartInd, xCount : integer);  overload; virtual; abstract;
    procedure IndexMinMaxAbsValues(var yMinInd, yMaxInd : integer);  overload; virtual; abstract;
    procedure IndexMinMaxAbsValues(var yMinInd, yMaxInd : integer; const xStartInd, xCount : integer);  overload; virtual; abstract;

    property Length : integer read fSourceLength;        //
    property GetNumElements : integer read NumElements;  // Actual length of allocated array



    // New Operations -- not implemented here, but in Exentia_Extension.pas

    Procedure AddSingleVec(V : TFVector); Virtual; Abstract;        // Self = Self + V
    Procedure SubSingleVec(V : TFVector); Virtual; Abstract;        // Self = Self - V
    Procedure MulMatrix(V: TFVector); Virtual; Abstract;            // Self = V * Self, where V is a *transposed* 4x4 matrix
  end;

  
  TX87Vector = class(TFVector)
  public
  public
    constructor Create(const ALength : integer);
    constructor CreateUsingArray(var A:array of single);
    destructor Destroy;override;

    procedure Add(V : TFVector); override;
    procedure Sub(V : TFVector); override;
    procedure Mul(V : TFVector); override;
    procedure Divide(V : TFVector); override;
    procedure Sqrt(V : TFVector); override;
    procedure Reciprocal(V : TFVector); override;
    procedure RecSqrt(V : TFVector); override;

    procedure Max(V : TFVector); override;
    procedure Min(V : TFVector); override;

    procedure LogicalAnd(V : TFVector); override;
    procedure LogicalOr(V : TFVector); override;
    procedure LogicalXor(V : TFVector); override;
    procedure CmpGreater(V : TFVector); override;
    procedure CmpLower(V : TFVector); override;
    procedure CmpEqual(V : TFVector); override;
    procedure CmpGreaterEqual(V : TFVector); override;
    procedure CmpLowerEqual(V : TFVector); override;

    procedure AddSquare(V : TFVector); override;

    function InnerSum : single; override;
    function InnerSumAbs : single; override;
    function DotProduct(V : TFVector) : single; override;
    function MaxValue : single; override;
    function MaxAbsValue : single; override;
    function IndexMaxValue : integer; override;
    function MinValue : single; override;
    function MinAbsValue : single; override;
    function IndexMinValue : integer; override;
    function Mean : single; override;
    procedure Scale(value : single); override;

    procedure Add(V1, V2 : TFVector); override;
    procedure Add(V1, V2 : TFVector; const xStartInd, xCount : integer); override;
    procedure Sub(V1, V2 : TFVector); override;
    procedure Sub(V1, V2 : TFVector; const xStartInd, xCount : integer); override;
    procedure Mul(V1, V2 : TFVector); override;
    procedure Mul(V1, V2 : TFVector; const xStartInd, xCount : integer); override;
    procedure Divide( V1, V2 : TFVector); override;
    procedure Divide(V1, V2 : TFVector; const xStartInd, xCount : integer); override;

    procedure Sqr(V : TFVector); override;
    procedure Max(V1, V2 : TFVector); override;
    procedure Max(V1, V2 : TFVector; const xStartInd, xCount : integer); override;
    procedure Min(V1, V2 : TFVector); override;
    procedure Min(V1, V2 : TFVector; const xStartInd, xCount : integer); override;

    procedure MinMaxValues(var yMin, yMax : single);  override;
    procedure MinMaxValues(var yMin, yMax : single; xStartInd, xCount : integer);  override;
    procedure IndexMinMaxValues(var yMinInd, yMaxInd : integer);  override;
    procedure IndexMinMaxValues(var yMinInd, yMaxInd : integer; const xStartInd, xCount : integer);  override;
    procedure MinMaxAbsValues(var yMin, yMax : single);  override;
    procedure MinMaxAbsValues(var yMin, yMax : single; xStartInd, xCount : integer);  override;
    procedure IndexMinMaxAbsValues(var yMinInd, yMaxInd : integer);  override;
    procedure IndexMinMaxAbsValues(var yMinInd, yMaxInd : integer; const xStartInd, xCount : integer);  override;

    function InnerSqrSum : single; override;
    function NormL1(V : TFVector) : single; override;
    function NormL2(V : TFVector) : single; override;

    procedure AddSquare(V1, V2 : TFVector); override;
    procedure AddSquare(V1, V2 : TFVector; const xStartInd, xCount : integer); override;
    procedure Add(value : single); override;
    procedure Combine(V1, V2 : tFVector; xA, xB : single); override;
    procedure Combine(V : tFVector; xA : single); override;
    procedure Lerp(V : tFVector; xT : single); override;
    procedure Lerp(V1, V2 : tFVector; xT : single); override;

{
    Procedure AddSingleVec(V : TFVector); Override;
    Procedure SubSingleVec(V : TFVector); Override;
    Procedure MulMatrix(V: TFVector);     Override;
}    
  end;


  TSSEVector = class(TX87Vector)
  public
    constructor Create(const ALength : integer);
    constructor CreateUsingArray(var A:array of Single);
    destructor Destroy;override;

    procedure Add(V : TFVector); override;
    procedure Sub(V : TFVector); override;
    procedure Mul(V : TFVector); override;
    procedure Divide(V : TFVector); override;

    procedure Add( V1, V2 : TFVector); override;
    procedure Sub( V1, V2 : TFVector); override;
    procedure Mul( V1, V2 : TFVector); override;
    procedure Divide( V1, V2 : TFVector); override;
    procedure Max( V1, V2 : TFVector); override;
    procedure Min( V1, V2 : TFVector); override;
    function InnerSum : single; override;
    function DotProduct(V : TFVector) : single; override;    
    function NormL2(V : TFVector) : single; override;
    procedure Lerp(V : tFVector; xT : single); override;

    procedure Sqrt(V : TFVector); override;
    procedure Reciprocal(V : TFVector); override;
    procedure RecSqrt(V : TFVector); override;
    procedure Max(V : TFVector); override;
    procedure Min(V : TFVector); override;
    procedure LogicalAnd(V : TFVector); override;
    procedure LogicalOr(V : TFVector); override;
    procedure LogicalXor(V : TFVector); override;
    procedure CmpGreater(V : TFVector); override;
    procedure CmpLower(V : TFVector); override;
    procedure CmpEqual(V : TFVector); override;
    procedure CmpGreaterEqual(V : TFVector); override;
    procedure CmpLowerEqual(V : TFVector); override;
    procedure AddSquare(V : TFVector); override;

{
    Procedure AddSingleVec(V : TFVector); Override;
    Procedure SubSingleVec(V : TFVector); Override;
    Procedure MulMatrix(V: TFVector);     Override;
}    
  end;


  T3DNowVector = class(TX87Vector)
  private
  public
    constructor Create(const ALength : integer);
    constructor CreateUsingArray(var A:array of Single);
    destructor Destroy;override;
    procedure Add(V : TFVector); override;
    procedure Sub(V : TFVector); override;
    procedure Mul(V : TFVector); override;
    procedure Divide(V : TFVector); override;

    procedure Add( V1, V2 : TFVector); override;
    procedure Sub( V1, V2 : TFVector); override;
    procedure Mul( V1, V2 : TFVector); override;
    procedure Divide( V1, V2 : TFVector); override;
    procedure Max( V1, V2 : TFVector); override;
    procedure Min( V1, V2 : TFVector); override;
    function InnerSum : single; override;
    function DotProduct(V : TFVector) : single; override;
    function NormL2(V : TFVector) : single; override;
    procedure Lerp(V : tFVector; xT : single); override;

    // procedure Sqrt(V : TFVector); override;
    procedure Reciprocal(V : TFVector); override;
    // procedure RecSqrt(V : TFVector); override;
    procedure Max(V : TFVector); override;
    procedure Min(V : TFVector); override;
    procedure LogicalAnd(V : TFVector); override;
    procedure LogicalOr(V : TFVector); override;
    procedure LogicalXor(V : TFVector); override;
    procedure CmpGreater(V : TFVector); override;
    procedure CmpLower(V : TFVector); override;
    procedure CmpEqual(V : TFVector); override;
    procedure CmpGreaterEqual(V : TFVector); override;
    procedure CmpLowerEqual(V : TFVector); override;
    procedure AddSquare(V : TFVector); override;
    procedure Scale(value : single); override;

{
    Procedure AddSingleVec(V : TFVector); Override;
    Procedure SubSingleVec(V : TFVector); Override;
    Procedure MulMatrix(V: TFVector);     Override;
}    
  end;


  tFMatrix = class
  private
    function GetDataElement(index1, index2: integer): single;
    function GetRowsArray(index: integer): PSingleArray;
    procedure SetDataElement(index1, index2: integer; const Value: single);
  protected
    fAlignedArray       : PSingleArray;
    fAllocatedArray     : pointer;
    fNumElements        : integer;
    fRows              : integer; // actual row count
    fCols              : integer; // not aligned value
    fActualCols        : integer; // aligned actual value
  public
    property Data[index1, index2 : integer] : single read GetDataElement write SetDataElement; default;
    property DataArray : PSingleArray read fAlignedArray;                  // direct access to all elements
    property RowsArray[index : integer] : PSingleArray  read GetRowsArray; // direct access to row

    constructor Create(const xRows, xCols : integer); virtual;
    destructor Destroy;override;

    class function BuildMatrix(const xRows, xCols : integer) : TFMatrix;
    class function isSSE : boolean;
    class function is3DNow : boolean;
    class function isCMOV : boolean; // Not used here but useful nevertheless
    procedure SetSize(const xRows, xCols : integer);
    procedure SetValue(xValue : single); // all elements := value
    procedure CopyMatrix(const M : tFMatrix);
    procedure ClearArray;

    property ActualCols : integer read fActualCols;
    property Cols : integer read fCols;
    property Rows : integer read fRows;
    property NumElements : integer read fNumElements;
  end;

  tX87Matrix = class(TFMatrix);

  tSSEMatrix = class(tX87Matrix);

  t3DNowMatrix = class(tX87Matrix);

  TFCube = class
  private
    function GetDataElement(index1, index2, index3: integer): single;
    procedure SetDataElement(index1, index2, index3: integer;  const Value: single);
    function GetCellsArray(xRowInd, xColInd: integer): PSingleArray;
  protected
    fAlignedArray       : PSingleArray;
    fAllocatedArray     : pointer;
    fNumElements        : integer;
    fRows: integer;          // actual rows count
    fCols: integer;          // actual cols count
    fLayers: integer;        // layers cound (source value)
    fActualLayers: integer;  //actual layers count
   public
    constructor Create(const xRows, xCols, xLayers : integer); virtual;
    destructor Destroy;override;

    property Data[index1, index2, index3 : integer] : single read GetDataElement write SetDataElement; default;
    property DataArray : PSingleArray read fAlignedArray;                  // direct access to all elements

    class function BuildCube(const xRows, xCols, xLayers : integer) : TFCube;
    class function isSSE : boolean;
    class function is3DNow : boolean;
    class function isCMOV : boolean; // Not used here but useful nevertheless
    procedure SetSize(const xRows, xCols, xLayers : integer);
    procedure SetValue(xValue : single); // all elements := value

    property CellsArray[xRowInd, xColInd : integer] : PSingleArray read GetCellsArray;

    property ActualLayers : integer read fActualLayers;
    property Layers : integer read fLayers;
    property Cols : integer read fCols;
    property Rows : integer read fRows;
    property NumElements : integer read fNumElements;
  end;

  tX87Cube   = class(TFCube);
  t3DNowCube = class(tX87Cube);
  tSSECube   = class(tX87Cube);

type
  ETestException = class(Exception);

//procedure TestBinaryBasicOps( var TestVector1, TestVector2,TestVector:TFVector);
//procedure TestBinaryComplexOps( var TestVector1, TestVector2,TestVector:TFVector);

implementation

{$R-}

const cTestVectorSize = 100;

var
  EnableSSE   : boolean;
  Enable3DNow : boolean;
  EnableCMOV  : boolean;

// *************************************************** //
// *                      TFVector                   * //
// *************************************************** //

constructor TFVector.Create(const ALength : integer);
begin
  inherited Create;
  AlignedArray := nil;
  AllocatedArray := nil;
  NumElements := 0;
  fSourceLength := 0;
  if ALength <> 0 then
    SetLength(ALength);
end;

constructor TFVector.CreateUsingArray(var A:array of Single);
begin
  inherited Create;
  AlignedArray := addr(A);
  AllocatedArray := nil;
  NumElements := System.Length(A);
end;

constructor TFVector.CreateUsingAlignedArrayPart(xSA : pSingleArray; const xLength : integer);
// assumes that xSA is part of aligned array and remaining length equals 4 singles at least }
begin
  inherited Create;
  fSourceLength := xLength;

  AlignedArray := xSA;
  AllocatedArray := nil;
  if (xLength and 3) > 0
       then NumElements := (xLength and ($FFFFFFFF - 3)) + 4
       else NumElements := xLength;
end;

function TFVector.CheckIndexParams(xStartInd, xCount: integer) : boolean;
begin
  // TODO test xStartInd + xCount <= Length + 1
end;

procedure TFVector.UseArray(var A:array of single);
begin
  if AllocatedArray <> nil
     then FreeMem(AllocatedArray);
  AlignedArray := addr(A);
  AllocatedArray := nil;
  NumElements := System.Length(A);
end;

procedure TFVector.UseMatrixRow(xMtx : TFMatrix; xRowInd : integer);
begin
  if AllocatedArray <> nil
    then FreeMem(AllocatedArray);
  AlignedArray := xMtx.RowsArray[xRowInd];
  AllocatedArray := nil;
  fSourceLength := xMtx.Cols;
  if (xMtx.Cols  and 3) > 0
       then NumElements := (xMtx.Cols and ($FFFFFFFF - 3)) + 4
       else NumElements := xMtx.Cols;
end;

procedure TFVector.UseCubeCell(xCube : TFCube; xRowInd, xColInd : integer);
begin
  if AllocatedArray <> nil
    then FreeMem(AllocatedArray);
  AllocatedArray := nil;
  fSourceLength := xCube.Layers;
  AlignedArray := xCube.CellsArray[xRowInd, xColInd];
  if (xCube.Layers  and 3) > 0
       then NumElements := (xCube.Layers and ($FFFFFFFF - 3)) + 4
       else NumElements := xCube.Layers;
end;

constructor TFVector.Copy(const V: TFVector);
begin
  inherited Create;
  AlignedArray := nil;
  AllocatedArray := nil;
  NumElements := 0;
  SetLength(V.NumElements);
  move(V.AlignedArray^, AlignedArray^, NumElements * SizeOf(Single));
end;

destructor TFVector.Destroy;
begin
  if AllocatedArray <> nil
     then FreeMem(AllocatedArray);
  AllocatedArray := nil;
  inherited Destroy;
end;

class function TFVector.BuildVector(const ALength : integer) : TFVector;
begin
  if EnableSSE
     then result := TSSEVector.Create(ALength)
     else if Enable3DNow
             then result := T3DNowVector.Create(ALength)
             else result := TX87Vector.Create(ALength);
end;

class function TFVector.BuildUsingArray(var A:array of Single) : TFVector;
begin
  if EnableSSE
     then result := TSSEVector.CreateUsingArray(A)
     else if Enable3DNow
             then result := T3DNowVector.CreateUsingArray(A)
             else result := TX87Vector.CreateUsingArray(A);
end;

class function TFVector.BuildUsingAlignedArrayPart(xSA : pSingleArray; const xLength : integer)  : TFVector;
begin
  if EnableSSE
     then result := TSSEVector.CreateUsingAlignedArrayPart(xSA, xLength)
     else if Enable3DNow
             then result := T3DNowVector.CreateUsingAlignedArrayPart(xSA, xLength)
             else result := TX87Vector.CreateUsingAlignedArrayPart(xSA, xLength);
end;

class function TFVector.CopyVector(const V : TFVector) : TFVector;
begin
  if EnableSSE
     then result := TSSEVector.Copy(V)
     else if Enable3DNow
             then result := T3DNowVector.Copy(V)
             else result := TX87Vector.Copy(V);
end;

class function TFVector.isSSE : boolean;
begin
  result := EnableSSE;
end;

class function TFVector.is3DNow : boolean;
begin
  result := Enable3DNow;
end;

class function TFVector.isCMOV : boolean;
begin
  result := EnableCMOV;
end;

function TFVector.GetDataElement(index : integer) : single;
begin
  result := AlignedArray[index];
end;

procedure TFVector.SetDataElement(index : integer; const value : single);
begin
  AlignedArray[index] := value;
end;

procedure TFVector.SetLength(const ALength : integer);
Var
  I       : Integer;
  NeedNew : Boolean;

begin
  // no need reallocation if vector length didn't change
  if (AllocatedArray <> nil) and (ALength = fSourceLength)
     then exit;

  // Only deallocate if we really have to...otherwise we'll just have some extra slack
  if (AllocatedArray <> nil) And (ALength > FSourceLength) Then
  Begin
    FreeMem(AllocatedArray);
    NeedNew := True;
  End
  Else NeedNew := (AllocatedArray = Nil);

  fSourceLength := ALength;

  // if we set length to zero we should free aligned array
  if ALength = 0 then
    begin
       If AllocatedArray <> Nil Then FreeMem(AllocatedArray);
       NumElements := 0;
       AllocatedArray := nil;
       AlignedArray := nil;
       exit;
    end;

  // rounds length on blocks of 4 elements
  if (ALength and 3) > 0
    then NumElements := (ALength and ($FFFFFFFF - 3)) + 4
    else NumElements := ALength;
  try
    If NeedNew Then
    Begin
      GetMem(AllocatedArray, (NumElements * SizeOf(single)) + 16);
      // AlignedArray is aligned on 16 bytes boundary
      AlignedArray := PSingleArray((integer(AllocatedArray) and $FFFFFFF0) + 16);
    End;  
  except
    on EOutOfMemory do
       begin
       NumElements := 0;
       fSourceLength := 0;
       AllocatedArray := nil;
       AlignedArray := nil;
       raise;  // re-raise exception to calling function
       end;
  end;

  // fill zeros to the extra slots
  for i:=1 to (ALength and 3) do
    AlignedArray[NumElements-i] := 0;

end;

procedure TFVector.CopyArray(const V : TFVector);
begin
  if NumElements <> V.NumElements
     then SetLength(V.NumElements);
  move(V.AlignedArray^, AlignedArray^, SizeOf(single) * NumElements);
end;

procedure TFVector.ImportArray(const Source : PSingleArray; const Length : integer = 0);
begin
  if (Length > 0) and (Length <> NumElements)
     then begin
          SetLength(Length);
          move(Source^, AlignedArray^, Length * SizeOf(Single));
          end
     else move(Source^, AlignedArray^, NumElements * SizeOf(Single));
end;

procedure TFVector.ExportArray(Destination : PSingleArray; const Length : integer = 0);
begin
  if Length > 0
     then move(AlignedArray^, Destination^, Length * SizeOf(Single))
     else move(AlignedArray^, Destination^, NumElements * SizeOf(Single));
end;

procedure TFVector.SetArray(value : single);
var counter : integer;
begin
  for counter := 0 to (NumElements - 1) do
      AlignedArray[counter] := value;
end;

procedure TFVector.ClearArray;
begin
  fillchar(AlignedArray^, NumElements * SizeOf(Single), 0);
end;

function TFVector.Equal(V : TFVector) : boolean;
var counter : integer;
begin
  result := true;
  if NumElements <> V.NumElements
     then begin
          result := false;
          exit;
          end;
  for counter := 0 to (NumElements - 1) do
      if Self[counter] <> V[counter]
         then begin
              result := false;
              break;
              end;
end;

// *************************************************** //
// *                    TSSEVector                   * //
// *************************************************** //

constructor TSSEVector.Create(const ALength : integer);
begin
  inherited Create(ALength);
end;

constructor TSSEVector.CreateUsingArray(var A:array of single);
begin
  inherited CreateUsingArray(A);
end;

destructor TSSEVector.Destroy;
begin
  inherited Destroy;
end;

procedure TSSEVector.Add(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop
@LargeAddLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength] {$ENDIF}
  addps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength] {$ENDIF}
  movaps [eax],xmm0

  movaps xmm1,[eax+16]
  addps xmm1,[edx+16]
  movaps [eax+16],xmm1

  movaps xmm2,[eax+32]
  addps xmm2,[edx+32]
  movaps [eax+32],xmm2

  movaps xmm3,[eax+48]
  addps xmm3,[edx+48]
  movaps [eax+48],xmm3
{$ELSE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength] {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength] {$ENDIF}
  addps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  addps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  addps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  addps xmm6,xmm7
  movaps [eax+48],xmm6
{$ENDIF}

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallAddLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  addps xmm0,xmm1
  movaps [eax],xmm0

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
end;

procedure TSSEVector.Add( V1, V2 : TFVector);
asm
  push ebx
  mov ebx,[V1.AlignedArray]
  mov edx,[V2.AlignedArray]

  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop
@LargeAddLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [ebx+CacheLineLength]  {$ENDIF}
  addps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  movaps [eax],xmm0

  movaps xmm1,[ebx+16]
  addps xmm1,[edx+16]
  movaps [eax+16],xmm1

  movaps xmm2,[ebx+32]
  addps xmm2,[edx+32]
  movaps [eax+32],xmm2

  movaps xmm3,[ebx+48]
  addps xmm3,[edx+48]
  movaps [eax+48],xmm3
{$ELSE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [ebx+CacheLineLength]  {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  addps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[ebx+16]
  movaps xmm3,[edx+16]
  addps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[ebx+32]
  movaps xmm5,[edx+32]
  addps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[ebx+48]
  movaps xmm7,[edx+48]
  addps xmm6,xmm7
  movaps [eax+48],xmm6
{$ENDIF}

  add eax,64
  add edx,64
  add ebx,64

  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallAddLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[ebx]
  addps xmm0,[edx]
  movaps [eax],xmm0
{$ELSE}
  movaps xmm0,[ebx]
  movaps xmm1,[edx]
  addps xmm0,xmm1
  movaps [eax],xmm0
{$ENDIF}
  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
   pop ebx
end;

function TSSEVector.InnerSum : single;
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  xorps xmm0, xmm0
  xorps xmm1, xmm1
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop
@LargeAddLoop:
{$IFDEF CISC_STYLE}
  addps   xmm1, [eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  addps   xmm0, [eax + 16]
  addps   xmm1, [eax + 32]
  addps   xmm0, [eax + 48]
{$ELSE}
  movaps  xmm2, [eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  addps   xmm1, xmm2

  movaps  xmm3, [eax + 16]
  addps   xmm0, xmm3

  movaps  xmm2, [eax + 32]
  addps   xmm1, xmm2

  movaps  xmm3, [eax + 48]
  addps   xmm0, xmm3
{$ENDIF}

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4

@SmallAddLoop:
  addps  xmm0, [eax]

  add eax,16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
  addps   xmm0, xmm1
                           // xmm0 = f3 f2 f1 f0
  shufps xmm1, xmm0, 40h   // xmm1 = f1 f0 f0 f0    //40h =   {01} {00} {00} {00}
  addps  xmm0, xmm1        // xmm0 = f1+f3 f2+f0 ... ...
  shufps xmm1, xmm0, 30h   // xmm1 = ...  f1+f3 ... ...   //  30h ={00} {10} {00} {00}
  addps xmm0, xmm1         // xmm0  = ... f0+f1+f2+f3 ... ...
  shufps xmm0, xmm0, 0AAh  // xmm0  = f0+f1+f2+f3 f0+f1+f2+f3 f0+f1+f2+f3 f0+f1+f2+f3  // AAh = {10} {10} {10} {10}

  movss Result, xmm0
end;

function TSSEVector.DotProduct(V : TFVector) : single;
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  xorps xmm0, xmm0
  xorps xmm1, xmm1

  push ecx
  shr ecx,2  // number of small iterations = number of elements / 4
  jz @SmallAddLoop
  shr ecx,2  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop
@LargeAddLoop:
 {$IFDEF CISC_STYLE}
  movaps xmm2, [eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength] {$ENDIF}
  mulps xmm2, [edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength] {$ENDIF}
  addps xmm0, xmm2

  movaps xmm3, [eax+16]
  mulps xmm3, [edx+16]
  addps xmm1, xmm3

  movaps xmm2, [eax+32]
  mulps xmm2, [edx+32]
  addps xmm0, xmm2

  movaps xmm3, [eax+48]
  mulps xmm3, [edx+48]
  addps xmm1, xmm3
  {$ELSE}
  movaps xmm2, [eax]
 {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength] {$ENDIF}
  movaps xmm3, [edx]
 {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength] {$ENDIF}
  mulps xmm2, xmm3
  addps xmm0, xmm2

  movaps xmm4, [eax+16]
  movaps xmm5, [edx+16]
  mulps xmm4, xmm5
  addps xmm1, xmm4

  movaps xmm2, [eax+32]
  movaps xmm3, [edx+32]
  mulps xmm2, xmm3
  addps xmm0, xmm2

  movaps xmm4, [eax+48]
  movaps xmm5, [edx+48]
  mulps xmm4, xmm5
  addps xmm1, xmm4
  {$ENDIF}

  add eax,64
  add edx, 64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallAddLoop:

  movaps xmm2, [eax]
  movaps xmm3, [edx]
  mulps xmm2, xmm3
  addps xmm0, xmm2

  add eax,16
  add edx, 16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
  addps xmm0, xmm1
  
  shufps xmm1, xmm0, 40h
  addps  xmm0, xmm1
  shufps xmm1, xmm0, 30h
  addps xmm0, xmm1
  shufps xmm0, xmm0, 0AAh

  movss Result, xmm0
end;


function TSSEVector.NormL2(V : TFVector) : single;
asm
  // TODO - realize large loop and prefetch CacheLineLength
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  xorps xmm0, xmm0
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4

@SmallAddLoop:
  {$IFDEF CISC_STYLE}
  movaps xmm2, [eax]
  subps xmm2, [edx]
  mulps xmm2, xmm2
  addps xmm0, xmm2
  {$ELSE}
  movaps xmm2, [eax]
  movaps xmm3, [edx]
  subps xmm2, xmm3
  mulps xmm2, xmm2
  addps xmm0, xmm2
  {$ENDIF}

  add eax,16
  add edx, 16
  dec ecx
  jnz @SmallAddLoop

  shufps xmm1, xmm0, 40h   
  addps  xmm0, xmm1
  shufps xmm1, xmm0, 30h
  addps xmm0, xmm1
  shufps xmm0, xmm0, 0AAh

  movss Result, xmm0
end;

procedure TSSEVector.Lerp(V : tFVector; xT : single);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]

  movss xmm2, xT
  shufps xmm2, xmm2, 0h           // unpack the xT value  to (xT | xT | xT |xT)

  push ecx
  shr ecx,2  // number of small iterations = number of elements / 4
  jz @SmallAddLoop
  shr ecx,2  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop
@LargeAddLoop:
  movaps xmm0, [eax]
  movaps xmm1, [edx]              // xmm1 := eax - edx
  subps xmm1, xmm0                //
  mulps xmm1, xmm2                // mm1 :=  vT*(V-Self)
  addps xmm0, xmm1
  movaps [eax], xmm0

  movaps xmm3, [eax+16]
  movaps xmm1, [edx+16]
  subps xmm1, xmm3
  mulps xmm1, xmm2
  addps xmm3, xmm1
  movaps [eax+16], xmm3

  movaps xmm0, [eax+32]
  movaps xmm1, [edx+32]              // xmm1 := eax - edx
  subps xmm1, xmm0                //
  mulps xmm1, xmm2                // mm1 :=  vT*(V-Self)
  addps xmm0, xmm1
  movaps [eax+32], xmm0

  movaps xmm3, [eax+48]
  movaps xmm1, [edx+48]
  subps xmm1, xmm3
  mulps xmm1, xmm2
  addps xmm3, xmm1
  movaps [eax+48], xmm3

  add eax,64
  add edx, 64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallAddLoop:

  movaps xmm0, [eax]
  movaps xmm1, [edx]              // xmm1 := eax - edx
  subps xmm1, xmm0                //
  mulps xmm1, xmm2                // mm1 :=  vT*(V-Self)
  addps xmm0, xmm1
  movaps [eax], xmm0

  add eax,16
  add edx, 16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
end;

procedure TSSEVector.Sub(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeSubLoop
@LargeSubLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  subps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  movaps [eax],xmm0

  movaps xmm1,[eax+16]
  subps xmm1,[edx+16]
  movaps [eax+16],xmm1

  movaps xmm2,[eax+32]
  subps xmm2,[edx+32]
  movaps [eax+32],xmm2

  movaps xmm3,[eax+48]
  subps xmm3,[edx+48]
  movaps [eax+48],xmm3
{$ELSE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  subps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  subps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  subps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  subps xmm6,xmm7
  movaps [eax+48],xmm6
{$ENDIF}
  add eax,64
  add edx,64
  dec ecx
  jnz @LargeSubLoop
@SkipLargeSubLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndSub
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallSubLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  subps xmm0,xmm1
  movaps [eax],xmm0

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallSubLoop

@EndSub:
end;

procedure TSSEVector.Sub( V1, V2 : TFVector);
asm
  push ebx
  mov ebx,[V1.AlignedArray]
  mov edx,[V2.AlignedArray]

  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeSubLoop
@LargeSubLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [ebx+CacheLineLength]  {$ENDIF}
  subps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  movaps [eax],xmm0

  movaps xmm1,[ebx+16]
  subps xmm1,[edx+16]
  movaps [eax+16],xmm1

  movaps xmm2,[ebx+32]
  subps xmm2,[edx+32]
  movaps [eax+32],xmm2

  movaps xmm3,[ebx+48]
  subps xmm3,[edx+48]
  movaps [eax+48],xmm3
{$ELSE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [ebx+CacheLineLength]  {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  subps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[ebx+16]
  movaps xmm3,[edx+16]
  subps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[ebx+32]
  movaps xmm5,[edx+32]
  subps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[ebx+48]
  movaps xmm7,[edx+48]
  subps xmm6,xmm7
  movaps [eax+48],xmm6
  {$ENDIF}

  add eax,64
  add edx,64
  add ebx,64
  dec ecx
  jnz @LargeSubLoop

@SkipLargeSubLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndSub
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallSubLoop:
  movaps xmm0,[ebx]
  movaps xmm1,[edx]
  subps xmm0,xmm1
  movaps [eax],xmm0
  
  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallSubLoop

@EndSub:
  pop ebx
end;

procedure TSSEVector.Mul(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMulLoop
@LargeMulLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  mulps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  movaps [eax],xmm0

  movaps xmm1,[eax+16]
  mulps xmm1,[edx+16]
  movaps [eax+16],xmm1

  movaps xmm2,[eax+32]
  mulps xmm2,[edx+32]
  movaps [eax+32],xmm2

  movaps xmm3,[eax+48]
  mulps xmm3,[edx+48]
  movaps [eax+48],xmm3
{$ELSE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  mulps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  mulps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  mulps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  mulps xmm6,xmm7
  movaps [eax+48],xmm6
  {$ENDIF}
  add eax,64
  add edx,64
  dec ecx
  jnz @LargeMulLoop

@SkipLargeMulLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMul
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMulLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  mulps xmm0,xmm1
  movaps [eax],xmm0

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallMulLoop

@EndMul:
end;
    
procedure TSSEVector.Mul( V1, V2 : TFVector);
asm
  push ebx

  mov ebx,[V1.AlignedArray]
  mov edx,[V2.AlignedArray]

  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMulLoop
@LargeMulLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [ebx+CacheLineLength]  {$ENDIF}
  mulps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  movaps [eax],xmm0

  movaps xmm1,[ebx+16]
  mulps xmm1,[edx+16]
  movaps [eax+16],xmm1

  movaps xmm2,[ebx+32]
  mulps xmm2,[edx+32]
  movaps [eax+32],xmm2

  movaps xmm3,[ebx+48]
  mulps xmm3,[edx+48]
  movaps [eax+48],xmm3
{$ELSE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [ebx+CacheLineLength]  {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  mulps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[ebx+16]
  movaps xmm3,[edx+16]
  mulps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[ebx+32]
  movaps xmm5,[edx+32]
  mulps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[ebx+48]
  movaps xmm7,[edx+48]
  mulps xmm6,xmm7
  movaps [eax+48],xmm6
{$ENDIF}
  add eax,64
  add edx,64
  add ebx,64
  dec ecx
  jnz @LargeMulLoop

@SkipLargeMulLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMul
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMulLoop:
  movaps xmm0,[ebx]
  movaps xmm1,[edx]
  mulps xmm0,xmm1
  movaps [ebx],xmm0

  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallMulLoop

@EndMul:
  pop ebx
end;

procedure TSSEVector.Divide(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeDivLoop
@LargeDivLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  divps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  movaps [eax],xmm0

  movaps xmm1,[eax+16]
  divps xmm1,[edx+16]
  movaps [eax+16],xmm1

  movaps xmm2,[eax+32]
  divps xmm2,[edx+32]
  movaps [eax+32],xmm2

  movaps xmm3,[eax+48]
  divps xmm3,[edx+48]
  movaps [eax+48],xmm3
{$ELSE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  divps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  divps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  divps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  divps xmm6,xmm7
  movaps [eax+48],xmm6
  {$ENDIF}
  add eax,64
  add edx,64
  dec ecx
  jnz @LargeDivLoop

@SkipLargeDivLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndDiv
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallDivLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  divps xmm0,xmm1
  movaps [eax],xmm0

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallDivLoop

@EndDiv:
end;

procedure TSSEVector.Divide( V1, V2: TFVector);
asm
  push ebx

  mov ebx,[V1.AlignedArray]
  mov edx,[V2.AlignedArray]

  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeDivLoop
@LargeDivLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  divps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  movaps [eax],xmm0

  movaps xmm1,[ebx+16]
  divps xmm1,[edx+16]
  movaps [eax+16],xmm1

  movaps xmm2,[ebx+32]
  divps xmm2,[edx+32]
  movaps [eax+32],xmm2

  movaps xmm3,[ebx+48]
  divps xmm3,[edx+48]
  movaps [eax+48],xmm3
{$ELSE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [ebx+CacheLineLength]  {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  divps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[ebx+16]
  movaps xmm3,[edx+16]
  divps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[ebx+32]
  movaps xmm5,[edx+32]
  divps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[ebx+48]
  movaps xmm7,[edx+48]
  divps xmm6,xmm7
  movaps [eax+48],xmm6
  {$ENDIF}
  add eax,64
  add edx,64
  add ebx,64

  dec ecx
  jnz @LargeDivLoop

@SkipLargeDivLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndDiv
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallDivLoop:
  movaps xmm0,[ebx]
  movaps xmm1,[edx]
  divps xmm0,xmm1
  movaps [eax],xmm0

  add eax,16
  add edx,16
  add ebx,16
  
  dec ecx
  jnz @SmallDivLoop

@EndDiv:
  pop ebx
end;

procedure TSSEVector.Sqrt(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeSqrtLoop
@LargeSqrtLoop:
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  sqrtps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  sqrtps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  sqrtps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  sqrtps xmm6,xmm7
  movaps [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeSqrtLoop

@SkipLargeSqrtLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndSqrt
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallSqrtLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  sqrtps xmm0,xmm1
  movaps [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallSqrtLoop

@EndSqrt:
end;

procedure TSSEVector.Reciprocal(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeReciprocalLoop
@LargeReciprocalLoop:
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  rcpps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  rcpps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  rcpps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  rcpps xmm6,xmm7
  movaps [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeReciprocalLoop

@SkipLargeReciprocalLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndReciprocal
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallReciprocalLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  rcpps xmm0,xmm1
  movaps [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallReciprocalLoop

@EndReciprocal:
end;

procedure TSSEVector.RecSqrt(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeRSqrtLoop
@LargeRSqrtLoop:
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  rsqrtps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  rsqrtps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  rsqrtps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  rsqrtps xmm6,xmm7
  movaps [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeRSqrtLoop

@SkipLargeRSqrtLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndRSqrt
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallRSqrtLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  rsqrtps xmm0,xmm1
  movaps [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallRSqrtLoop

@EndRSqrt:
end;

procedure TSSEVector.Max(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMaxLoop
@LargeMaxLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength] {$ENDIF}
  maxps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength] {$ENDIF}
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  maxps xmm2,[edx+16]
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  maxps xmm4,[edx+32]
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  maxps xmm6,[edx+48]
  movaps [eax+48],xmm6
{$ELSE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  maxps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  maxps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  maxps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  maxps xmm6,xmm7
  movaps [eax+48],xmm6
  {$ENDIF}

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeMaxLoop

@SkipLargeMaxLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMax
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMaxLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  maxps xmm0,xmm1
  movaps [eax],xmm0

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallMaxLoop

@EndMax:
end;

procedure TSSEVector.Max( V1, V2 : TFVector);
asm
  push ebx
  mov ebx,[V1.AlignedArray]
  mov edx,[V2.AlignedArray]

  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMaxLoop
@LargeMaxLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [ebx+CacheLineLength] {$ENDIF}
  maxps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength] {$ENDIF}
  movaps [eax],xmm0

  movaps xmm2,[ebx+16]
  maxps xmm2,[edx+16]
  movaps [eax+16],xmm2

  movaps xmm4,[ebx+32]
  maxps xmm4,[edx+32]
  movaps [eax+32],xmm4

  movaps xmm6,[ebx+48]
  maxps xmm6,[edx+48]
  movaps [eax+48],xmm6
{$ELSE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [ebx+CacheLineLength]  {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  maxps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[ebx+16]
  movaps xmm3,[edx+16]
  maxps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[ebx+32]
  movaps xmm5,[edx+32]
  maxps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[ebx+48]
  movaps xmm7,[edx+48]
  maxps xmm6,xmm7
  movaps [eax+48],xmm6
  {$ENDIF}
  add eax,64
  add edx,64
  add ebx,64
  dec ecx
  jnz @LargeMaxLoop

@SkipLargeMaxLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMax
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMaxLoop:
  movaps xmm0,[ebx]
  movaps xmm1,[edx]
  maxps xmm0,xmm1
  movaps [ebx],xmm0

  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallMaxLoop

@EndMax:
  pop ebx
end;

procedure TSSEVector.Min(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMinLoop
@LargeMinLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength] {$ENDIF}
  minps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength] {$ENDIF}
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  minps xmm2,[edx+16]
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  minps xmm4,[edx+32]
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  minps xmm6,[edx+48]
  movaps [eax+48],xmm6
{$ELSE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  minps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  minps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  minps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  minps xmm6,xmm7
  movaps [eax+48],xmm6
  {$ENDIF}

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeMinLoop

@SkipLargeMinLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMin
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMinLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  minps xmm0,xmm1
  movaps [eax],xmm0

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallMinLoop

@EndMin:
end;

procedure TSSEVector.Min( V1, V2 : TFVector);
asm
  push ebx

  mov ebx,[V1.AlignedArray]
  mov edx,[V2.AlignedArray]

  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMinLoop
@LargeMinLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [ebx+CacheLineLength] {$ENDIF}
  minps xmm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength] {$ENDIF}
  movaps [eax],xmm0

  movaps xmm2,[ebx+16]
  minps xmm2,[edx+16]
  movaps [eax+16],xmm2

  movaps xmm4,[ebx+32]
  minps xmm4,[edx+32]
  movaps [eax+32],xmm4

  movaps xmm6,[ebx+48]
  minps xmm6,[edx+48]
  movaps [eax+48],xmm6

{$ELSE}
  movaps xmm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [ebx+CacheLineLength]  {$ENDIF}
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  minps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[ebx+16]
  movaps xmm3,[edx+16]
  minps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[ebx+32]
  movaps xmm5,[edx+32]
  minps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[ebx+48]
  movaps xmm7,[edx+48]
  minps xmm6,xmm7
  movaps [eax+48],xmm6
{$ENDIF}

  add eax,64
  add edx,64
  add ebx,64
  dec ecx
  jnz @LargeMinLoop

@SkipLargeMinLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMin
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMinLoop:
  movaps xmm0,[ebx]
  movaps xmm1,[edx]
  minps xmm0,xmm1
  movaps [eax],xmm0

  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallMinLoop

@EndMin:
  pop ebx
end;

procedure TSSEVector.LogicalAnd(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeLogicalAndLoop
@LargeLogicalAndLoop:
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  andps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  andps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  andps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  andps xmm6,xmm7
  movaps [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeLogicalAndLoop

@SkipLargeLogicalAndLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndLogicalAnd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallLogicalAndLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  andps xmm0,xmm1
  movaps [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallLogicalAndLoop

@EndLogicalAnd:
end;

procedure TSSEVector.LogicalOr(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeLogicalOrLoop
@LargeLogicalOrLoop:
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  orps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  orps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  orps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  orps xmm6,xmm7
  movaps [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeLogicalOrLoop

@SkipLargeLogicalOrLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndLogicalOr
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallLogicalOrLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  orps xmm0,xmm1
  movaps [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallLogicalOrLoop

@EndLogicalOr:
end;

procedure TSSEVector.LogicalXor(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeLogicalXorLoop
@LargeLogicalXorLoop:
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  xorps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  xorps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  xorps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  xorps xmm6,xmm7
  movaps [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeLogicalXorLoop

@SkipLargeLogicalXorLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndLogicalXor
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallLogicalXorLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  xorps xmm0,xmm1
  movaps [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallLogicalXorLoop

@EndLogicalXor:
end;

procedure TSSEVector.CmpGreater(V : TFVector);
// Self > V becomes V < Self
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeCmpGreaterLoop
@LargeCmpGreaterLoop:
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  cmpltps xmm1,xmm0
  movaps [eax],xmm1

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  cmpltps xmm3,xmm2
  movaps [eax+16],xmm3

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  cmpltps xmm5,xmm4
  movaps [eax+32],xmm5

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  cmpltps xmm7,xmm6
  movaps [eax+48],xmm7

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpGreaterLoop

@SkipLargeCmpGreaterLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndCmpGreater
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallCmpGreaterLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  cmpltps xmm1,xmm0
  movaps [eax],xmm1
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpGreaterLoop

@EndCmpGreater:
end;

procedure TSSEVector.CmpLower(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeCmpLowerLoop
@LargeCmpLowerLoop:
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  cmpltps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  cmpltps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  cmpltps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  cmpltps xmm6,xmm7
  movaps [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpLowerLoop

@SkipLargeCmpLowerLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndCmpLower
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallCmpLowerLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  cmpltps xmm0,xmm1
  movaps [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpLowerLoop

@EndCmpLower:
end;

procedure TSSEVector.CmpEqual(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeCmpEqualLoop
@LargeCmpEqualLoop:
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  cmpeqps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  cmpeqps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  cmpeqps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  cmpeqps xmm6,xmm7
  movaps [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpEqualLoop

@SkipLargeCmpEqualLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndCmpEqual
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallCmpEqualLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  cmpeqps xmm0,xmm1
  movaps [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpEqualLoop

@EndCmpEqual:
end;

procedure TSSEVector.CmpGreaterEqual(V : TFVector);
// A >= B becomes B <= A
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeCmpGreaterEqualLoop
@LargeCmpGreaterEqualLoop:
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  cmpleps xmm1,xmm0
  movaps [eax],xmm1

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  cmpleps xmm3,xmm2
  movaps [eax+16],xmm3

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  cmpleps xmm5,xmm4
  movaps [eax+32],xmm5

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  cmpleps xmm7,xmm6
  movaps [eax+48],xmm7

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpGreaterEqualLoop

@SkipLargeCmpGreaterEqualLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndCmpGreaterEqual
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallCmpGreaterEqualLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  cmpleps xmm1,xmm0
  movaps [eax],xmm1
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpGreaterEqualLoop

@EndCmpGreaterEqual:
end;

procedure TSSEVector.CmpLowerEqual(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeCmpLowerEqualLoop
@LargeCmpLowerEqualLoop:
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  cmpleps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  movaps xmm3,[edx+16]
  cmpleps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  movaps xmm5,[edx+32]
  cmpleps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  movaps xmm7,[edx+48]
  cmpleps xmm6,xmm7
  movaps [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpLowerEqualLoop

@SkipLargeCmpLowerEqualLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndCmpLowerEqual
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallCmpLowerEqualLoop:
  movaps xmm0,[eax]
  movaps xmm1,[edx]
  cmpleps xmm0,xmm1
  movaps [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpLowerEqualLoop

@EndCmpLowerEqual:
end;

procedure TSSEVector.AddSquare(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeAddSquareLoop
@LargeAddSquareLoop:
  movaps xmm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$4A,CacheLineLength  {$ENDIF}     /// prefetcht0 [edx+CacheLineLength]
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$18,$48,CacheLineLength  {$ENDIF}     /// prefetcht0 [eax+CacheLineLength]
  mulps xmm1,xmm1
  addps xmm0,xmm1
  movaps [eax],xmm0

  movaps xmm3,[edx+16]
  movaps xmm2,[eax+16]
  mulps xmm3,xmm3
  addps xmm2,xmm3
  movaps [eax+16],xmm2

  movaps xmm5,[edx+32]
  movaps xmm4,[eax+32]
  mulps xmm5,xmm5
  addps xmm4,xmm5
  movaps [eax+32],xmm4

  movaps xmm7,[edx+48]
  movaps xmm6,[eax+48]
  mulps xmm7,xmm7
  addps xmm6,xmm7
  movaps [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeAddSquareLoop

@SkipLargeAddSquareLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAddSquare
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallAddSquareLoop:
  movaps xmm1,[edx]
  movaps xmm0,[eax]
  mulps xmm1,xmm1
  addps xmm0,xmm1
  movaps [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallAddSquareLoop

@EndAddSquare:
end;

// *************************************************** //
// *                    TX87Vector                   * //
// *************************************************** //

constructor TX87Vector.Create(const ALength : integer);
begin
  inherited Create(ALength);
end;

constructor TX87Vector.CreateUsingArray(var A:array of single);
begin
  inherited CreateUsingArray(A);
end;

destructor TX87Vector.Destroy;
begin
  inherited Destroy;
end;

procedure TX87Vector.Add(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;

  while Counter<-3 do
  begin
    Data[counter] := Data[counter] + Source[counter];
    Data[counter+1] := Data[counter+1] + Source[counter+1];
    Data[counter+2] := Data[counter+2] + Source[counter+2];
    Data[counter+3] := Data[counter+3] + Source[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Data[counter] + Source[counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.Sub(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;

  while Counter<-3 do
  begin
    Data[counter] := Data[counter] - Source[counter];
    Data[counter+1] := Data[counter+1] - Source[counter+1];
    Data[counter+2] := Data[counter+2] - Source[counter+2];
    Data[counter+3] := Data[counter+3] - Source[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Data[counter] - Source[counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.Mul(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;

  while Counter<-3 do
  begin
    Data[counter] := Data[counter] * Source[counter];
    Data[counter+1] := Data[counter+1] * Source[counter+1];
    Data[counter+2] := Data[counter+2] * Source[counter+2];
    Data[counter+3] := Data[counter+3] * Source[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Data[counter] * Source[counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.Divide(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
  endInd  : integer;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;

  endInd := fSourceLength - NumElements;
  while Counter<-4 do
  begin
    Data[counter] := Data[counter] / Source[counter];
    Data[counter+1] := Data[counter+1] / Source[counter+1];
    Data[counter+2] := Data[counter+2] / Source[counter+2];
    Data[counter+3] := Data[counter+3] / Source[counter+3];
    inc(Counter,4);
  end;
  while Counter< endInd do
  begin
    Data[counter] := Data[counter] / Source[counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.Sqrt(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    Data[counter] := System.Sqrt(Source[counter]);
    inc(Counter);
  end;
end;

procedure TX87Vector.Reciprocal(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    Data[counter] := 1 / Source[counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.RecSqrt(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    Data[counter] := 1 / system.Sqrt(Source[counter]);
    inc(Counter);
  end;
end;

procedure TX87Vector.Max(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    if Source[counter+0] > Data[counter+0] then Data[Counter+0]:= Source[Counter+0];
    if Source[counter+1] > Data[counter+1] then Data[Counter+1]:= Source[Counter+1];
    if Source[counter+2] > Data[counter+2] then Data[Counter+2]:= Source[Counter+2];
    if Source[counter+3] > Data[counter+3] then Data[Counter+3]:= Source[Counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    if Source[counter] > Data[counter] then
      Data[Counter]:= Source[Counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.Min(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    if Source[counter+0] < Data[counter+0] then Data[Counter+0]:= Source[Counter+0];
    if Source[counter+1] < Data[counter+1] then Data[Counter+1]:= Source[Counter+1];
    if Source[counter+2] < Data[counter+2] then Data[Counter+2]:= Source[Counter+2];
    if Source[counter+3] < Data[counter+3] then Data[Counter+3]:= Source[Counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    if Source[counter] < Data[counter] then
      Data[Counter]:= Source[Counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.LogicalAnd(V : TFVector);
{$IFDEF DELPHI_CODE}
var counter : integer;
    AuxP:pointer;
    DestVec,SrcVec: PLongWordArray;
begin
  AuxP:=AlignedArray; DestVec:=AuxP;
  AuxP:=V.AlignedArray; SrcVec:=AuxP;
  for counter := 0 to (NumElements - 1) do
      DestVec^[counter] := DestVec^[counter] and SrcVec^[counter];
end;
{$ELSE}
asm
   mov ecx,[eax + NumElements]
   mov edx,[V.AlignedArray]
   mov eax,[eax + AlignedArray]
 @AndLoop:
   mov ebx, [edx]
   and [eax], ebx
   add edx,4
   add eax,4
   dec ecx
   jnz @AndLoop
end;
{$ENDIF}

procedure TX87Vector.LogicalOr(V : TFVector);
{$IFDEF DELPHI_CODE}
var counter : integer;
    AuxP:pointer;
    DestVec,SrcVec: PLongWordArray;
begin
  AuxP:=AlignedArray; DestVec:=AuxP;
  AuxP:=V.AlignedArray; SrcVec:=AuxP;
  for counter := 0 to (NumElements - 1) do
      DestVec^[counter] := DestVec^[counter] or SrcVec^[counter];
end;
{$ELSE}
asm
   mov ecx,[eax + NumElements]
   mov edx,[V.AlignedArray]
   mov eax,[eax + AlignedArray]
 @AndLoop:
   mov ebx, [edx]
   or [eax], ebx
   add edx,4
   add eax,4
   dec ecx
   jnz @AndLoop
end;
{$ENDIF}

procedure TX87Vector.LogicalXor(V : TFVector);
{$IFDEF DELPHI_CODE}
var counter : integer;
    AuxP:pointer;
    DestVec,SrcVec: PLongWordArray;
begin
  AuxP:=AlignedArray; DestVec:=AuxP;
  AuxP:=V.AlignedArray; SrcVec:=AuxP;
  for counter := 0 to (NumElements - 1) do
      DestVec^[counter] := DestVec^[counter] xor SrcVec^[counter];
end;
{$ELSE}
asm
   mov ecx,[eax + NumElements]
   mov edx,[V.AlignedArray]
   mov eax,[eax + AlignedArray]
 @AndLoop:
   mov ebx, [edx]
   xor [eax], ebx
   add edx,4
   add eax,4
   dec ecx
   jnz @AndLoop
end;
{$ENDIF}

procedure TX87Vector.CmpGreater(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    if Data[counter] > Source[counter] then
      Pointer(Data[counter]):=pointer($FFFFFFFF)
    else
      Pointer(Data[counter]):=pointer(0);
    inc(Counter);
  end;
end;

procedure TX87Vector.CmpLower(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    if Data[counter] < Source[counter] then
      Pointer(Data[counter]):=pointer($FFFFFFFF)
    else
      Pointer(Data[counter]):=pointer(0);
    inc(Counter);
  end;
end;

procedure TX87Vector.CmpEqual(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    if Data[counter] = Source[counter] then
      Pointer(Data[counter]):=pointer($FFFFFFFF)
    else
      Pointer(Data[counter]):=pointer(0);
    inc(Counter);
  end;
end;

procedure TX87Vector.CmpGreaterEqual(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    if Data[counter] >= Source[counter] then
      Pointer(Data[counter]):=pointer($FFFFFFFF)
    else
      Pointer(Data[counter]):=pointer(0);
    inc(Counter);
  end;
end;

procedure TX87Vector.CmpLowerEqual(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    if Data[counter] >= Source[counter] then
      Pointer(Data[counter]):=pointer($FFFFFFFF)
    else
      Pointer(Data[counter]):=pointer(0);
    inc(Counter);
  end;
end;

procedure TX87Vector.AddSquare(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    Data[counter+0] := Data[counter+0] + system.sqr(Source[counter+0]);
    Data[counter+1] := Data[counter+1] + system.sqr(Source[counter+1]);
    Data[counter+2] := Data[counter+2] + system.sqr(Source[counter+2]);
    Data[counter+3] := Data[counter+3] + system.sqr(Source[counter+3]);
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Data[counter] + system.sqr(Source[counter]);
    inc(Counter);
  end;
end;

function TX87Vector.InnerSum : single;
{$IFDEF DELPHI_CODE}
var
  counter : integer;
  Data:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    result := result + Data[counter] + Data[counter+1]+Data[counter+2]+ Data[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    result := result + Data[counter];
    inc(Counter);
  end;
end;
{$ELSE}
asm
    mov ecx,[eax + NumElements]
    mov eax,[eax + AlignedArray]
    fldz
  @InnerSumLoop:
    fadd DWORD PTR [eax]
    add eax,4
    dec ecx
    jnz @InnerSumLoop
end;
{$ENDIF}

function TX87Vector.InnerSumAbs : single;
{$IFDEF DELPHI_CODE}
var
  counter : integer;
  Data:PSingleArray;
begin
  result := 0.0;
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    result := result + abs(Data[counter]);
    inc(Counter);
  end;
end;
{$ELSE}
asm
    mov ecx,[eax + NumElements]
    mov eax,[eax + AlignedArray]
    fldz
  @InnerSumLoop:
    fld DWORD PTR [eax]
    fabs
    faddp ST(1),ST(0)
    add eax,4
    dec ecx
    jnz @InnerSumLoop
end;
{$ENDIF}

function TX87Vector.DotProduct(V : TFVector) : single;
{$IFDEF DELPHI_CODE}
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data := @AlignedArray[NumElements];
  Source := @V.AlignedArray[NumElements];
  Counter := -NumElements;
  result := 0;
  while Counter<-3 do
  begin
    result := result + Data[Counter+0]*Source[Counter+0]
                     + Data[Counter+1]*Source[Counter+1]
                     + Data[Counter+2]*Source[Counter+2]
                     + Data[Counter+3]*Source[Counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    result := result + Data[Counter]*Source[Counter];
    inc(Counter);
  end;
end;
{$ELSE}
asm
   mov ecx,[eax + NumElements]
   mov edx,[V.AlignedArray]
   mov eax,[eax + AlignedArray]
   fldz
 @DotProductLoop:
   fld DWORD PTR [eax]
   fmul DWORD PTR [edx]
   add eax,4
   add edx,4
   faddp ST(1),ST(0)
   dec ecx
   jnz @DotProductLoop
end;
{$ENDIF}

function TX87Vector.MaxValue : single;
var
  counter : integer;
  Data:PSingleArray;
begin
  result := AlignedArray[0];
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    if Data[counter+0] > result then result:=Data[counter+0];
    if Data[counter+1] > result then result:=Data[counter+1];
    if Data[counter+2] > result then result:=Data[counter+2];
    if Data[counter+3] > result then result:=Data[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    if Data[counter] > result then
      result:=Data[counter];
    inc(Counter);
  end;
end;

function TX87Vector.MinValue : single;
var
  counter : integer;
  Data:PSingleArray;
begin
  result := AlignedArray[0];
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    if Data[counter+0] < result then result:=Data[counter+0];
    if Data[counter+1] < result then result:=Data[counter+1];
    if Data[counter+2] < result then result:=Data[counter+2];
    if Data[counter+3] < result then result:=Data[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    if Data[counter] < result then
      result:=Data[counter];
    inc(Counter);
  end;
end;

function TX87Vector.MaxAbsValue : single;
var
  counter : integer;
  Data:PSingleArray;
begin
  result := AlignedArray[0];
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    if abs(Data[counter]) > result then
      result:=abs(Data[counter]);
    inc(Counter);
  end;
end;

function TX87Vector.MinAbsValue : single;
var
  counter : integer;
  Data:PSingleArray;
begin
  result := AlignedArray[0];
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    if abs(Data[counter]) < result then
      result:=abs(Data[counter]);
    inc(Counter);
  end;
end;

function TX87Vector.IndexMaxValue : integer;
var
  counter : integer;
  CurrentMaxValue : single;
  Data:PSingleArray;
begin
  result := 0;
  CurrentMaxValue := AlignedArray[0];
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    if Data[counter] > CurrentMaxValue then
    begin
      CurrentMaxValue:=Data[counter];
      result:=counter;
    end;
    inc(Counter);
  end;
end;

function TX87Vector.IndexMinValue : integer;
var
  counter : integer;
  CurrentMaxValue : single;
  Data:PSingleArray;
begin
  result := 0;
  CurrentMaxValue := AlignedArray[0];
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    if Data[counter] < CurrentMaxValue then
    begin
      CurrentMaxValue:=Data[counter];
      result:=counter;
    end;
    inc(Counter);
  end;
end;

function TX87Vector.Mean : single;
{$IFDEF DELPHI_CODE}
var
  counter : integer;
  Data:PSingleArray;
begin
  result := 0;
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    result:=result + Data[counter+0] + Data[counter+1] + Data[counter+2] + Data[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    result:=result + Data[counter];
    inc(Counter);
  end;
  result:=result / NumElements;
end;
{$ELSE}
asm
  mov ecx,[eax + NumElements]
  mov edx,[eax + AlignedArray]
  fldz
 @MeanLoop:
  fadd DWORD PTR [edx]
  add edx,4
  dec ecx
  jnz @MeanLoop
  fild DWORD PTR [eax + NumElements]
  fdivp
end;
{$ENDIF}

procedure TX87Vector.Scale(value : single);
{$IFDEF DELPHI_CODE}
var
  counter : integer;
  Data:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    Data[counter+0]:=Data[counter+0] * value;
    Data[counter+1]:=Data[counter+1] * value;
    Data[counter+2]:=Data[counter+2] * value;
    Data[counter+3]:=Data[counter+3] * value;
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter]:=Data[counter] * value;
    inc(Counter);
  end;
end;
{$ELSE}
asm
   mov ecx,[eax + NumElements]
   mov edx,[eax + AlignedArray]
   fld DWORD PTR [ebp + 8]  // load value
 @ScaleLoop:
   fld DWORD PTR [edx]
   fmul ST(0),ST(1)
   fstp DWORD PTR [edx]
   add edx,4
   dec ecx
   jnz @ScaleLoop
end;
{$ENDIF}

procedure TX87Vector.Add(V1, V2 : TFVector);
(*{$IFDEF OPT_STAGE_1}
asm
  push ebx
  mov ebx,[V1.AlignedArray]
  mov edx,[V2.AlignedArray]
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
{$IFDEF VARIANT_AMDDOC1}
 lea eax,[eax+ecx*4 ]
  lea ebx,[ebx+ecx*4 ]
  lea edx,[edx+ecx*4 ]
  neg ecx
@addloop:
    fld dword ptr [edx+ecx*4+28 ]
    fadd dword ptr [ebx+ecx*4+28 ]
    fld dword ptr [edx+ecx*4+24 ]
    fadd dword ptr [ebx+ecx*4+24 ]
    fld dword ptr [edx+ecx*4+20 ]
    fadd dword ptr [ebx+ecx*4+20 ]
    fld dword ptr [edx+ecx*4+16 ]
    fadd dword ptr [ebx+ecx*4+16 ]
    fld dword ptr [edx+ecx*4+12 ]
    fadd dword ptr [ebx+ecx*4+12 ]
    fld dword ptr [edx+ecx*4+8 ]
    fadd dword ptr [ebx+ecx*4+8 ]
    fld dword ptr [edx+ecx*4+4 ]
    fadd dword ptr [ebx+ecx*4+4 ]
    fld dword ptr [edx+ecx*4+0 ]
    fadd dword ptr [ebx+ecx*4+0 ]

    fstp dword ptr [eax+ecx*4+0 ]
    fstp dword ptr [eax+ecx*4+4 ]
    fstp dword ptr [eax+ecx*4+8 ]
    fstp dword ptr [eax+ecx*4+12 ]
    fstp dword ptr [eax+ecx*4+16 ]
    fstp dword ptr [eax+ecx*4+20 ]
    fstp dword ptr [eax+ecx*4+24 ]
    fstp dword ptr [eax+ecx*4+28 ]
    add ecx,8
  jnz @addloop
{$ELSE}

  push ecx
  shr ecx,3  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop

@addloop:
    fld dword ptr [edx+28 ]
    fadd dword ptr [ebx+28 ]
    fld dword ptr [edx+24 ]
    fadd dword ptr [ebx+24 ]
    fld dword ptr [edx+20 ]
    fadd dword ptr [ebx+20 ]
    fld dword ptr [edx+16 ]
    fadd dword ptr [ebx+16 ]
    fld dword ptr [edx+12 ]
    fadd dword ptr [ebx+12 ]
    fld dword ptr [edx+8 ]
    fadd dword ptr [ebx+8 ]
    fld dword ptr [edx+4 ]
    fadd dword ptr [ebx+4 ]
    fld dword ptr [edx+0 ]
    fadd dword ptr [ebx+0 ]

    fstp dword ptr [eax+0 ]
    fstp dword ptr [eax+4 ]
    fstp dword ptr [eax+8 ]
    fstp dword ptr [eax+12 ]
    fstp dword ptr [eax+16 ]
    fstp dword ptr [eax+20 ]
    fstp dword ptr [eax+24 ]
    fstp dword ptr [eax+28 ]
    add eax, 32
    add ebx, 32
    add edx, 32
    dec ecx
  jnz @addloop
{ }

@SkipLargeAddLoop:
  pop ecx
  and ecx,$00000007
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallAddLoop:
  fld dword ptr [edx+12 ]
  fadd dword ptr [ebx+12 ]
  fld dword ptr [edx+8 ]
  fadd dword ptr [ebx+8 ]
  fld dword ptr [edx+4 ]
  fadd dword ptr [ebx+4 ]
  fld dword ptr [edx+0 ]
  fadd dword ptr [ebx+0 ]

  fstp dword ptr [eax+0 ]
  fstp dword ptr [eax+4 ]
  fstp dword ptr [eax+8 ]
  fstp dword ptr [eax+12 ]

  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallAddLoop
{$ENDIF}
@EndAdd:
   pop ebx
end;
{$ELSE}           *)
var
  counter : integer;
  Data, Source1, Source2 :PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source1:=@V1.AlignedArray[NumElements];
  Source2:=@V2.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-7 do
  begin
    Data[counter] := Source1[counter] + Source2[counter];
    Data[counter+1] := Source1[counter+1] + Source2[counter+1];
    Data[counter+2] := Source1[counter+2] + Source2[counter+2];
    Data[counter+3] := Source1[counter+3] + Source2[counter+3];
    Data[counter+4] := Source1[counter+4] + Source2[counter+4];
    Data[counter+5] := Source1[counter+5] + Source2[counter+5];
    Data[counter+6] := Source1[counter+6] + Source2[counter+6];
    Data[counter+7] := Source1[counter+7] + Source2[counter+7];
    inc(Counter,8);
  end;                 
  while Counter<-3 do
  begin
    Data[counter] := Source1[counter] + Source2[counter];
    Data[counter+1] := Source1[counter+1] + Source2[counter+1];
    Data[counter+2] := Source1[counter+2] + Source2[counter+2];
    Data[counter+3] := Source1[counter+3] + Source2[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Source1[counter] + Source2[counter];
    inc(Counter);
  end;
end;
(*{$ENDIF} *)

procedure TX87Vector.Add(V1, V2 : TFVector; const xStartInd, xCount : integer);
begin
  Add(V1, V2);
end;

procedure TX87Vector.Sub(V1, V2 : TFVector);
var
  counter : integer;
  Data, Source1, Source2 :PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source1:=@V1.AlignedArray[NumElements];
  Source2:=@V2.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    Data[counter] := Source1[counter] - Source2[counter];
    Data[counter+1] := Source1[counter+1] - Source2[counter+1];
    Data[counter+2] := Source1[counter+2] - Source2[counter+2];
    Data[counter+3] := Source1[counter+3] - Source2[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Source1[counter] - Source2[counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.Sub(V1, V2 : TFVector; const xStartInd, xCount : integer);
begin
  Sub(V1, V2);
end;

procedure TX87Vector.Mul(V1, V2 : TFVector);
var
  counter : integer;
  Data, Source1, Source2 :PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source1:=@V1.AlignedArray[NumElements];
  Source2:=@V2.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    Data[counter] := Source1[counter] * Source2[counter];
    Data[counter+1] := Source1[counter+1] * Source2[counter+1];
    Data[counter+2] := Source1[counter+2] * Source2[counter+2];
    Data[counter+3] := Source1[counter+3] * Source2[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Source1[counter] * Source2[counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.Mul(V1, V2 : TFVector; const xStartInd, xCount : integer);
begin
  Mul(V1, V2);
end;

procedure TX87Vector.Divide( V1, V2 : TFVector);
var
  counter : integer;
  Data, Source1, Source2:PSingleArray;
  endInd  : integer;
begin
  Data:=@AlignedArray[NumElements];
  Source1:=@V1.AlignedArray[NumElements];
  Source2:=@V2.AlignedArray[NumElements];
  Counter:=-NumElements;

  endInd := fSourceLength - NumElements;
  while Counter<-4 do
  begin
    Data[counter] := Source1[counter] / Source2[counter];
    Data[counter+1] := Source1[counter+1] / Source2[counter+1];
    Data[counter+2] := Source1[counter+2] / Source2[counter+2];
    Data[counter+3] := Source1[counter+3] / Source2[counter+3];
    inc(Counter,4);
  end;
  while Counter < endInd do
  begin
    Data[counter] := Source1[counter] / Source2[counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.Divide(V1, V2 : TFVector; const xStartInd, xCount : integer);
begin
  Divide(V1, V2);
end;

procedure TX87Vector.Sqr(V : TFVector);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    Data[counter] := System.Sqr(Source[counter]);
    inc(Counter);
  end;
end;

procedure TX87Vector.Max(V1, V2 : TFVector);
var
  counter : integer;
  Data,Source1, Source2 :PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source1:=@V1.AlignedArray[NumElements];
  Source2:=@V2.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    if Source1[counter] > Source2[counter]
      then Data[Counter]:= Source1[Counter]
      else Data[Counter]:= Source2[Counter];
    if Source1[counter+1] > Source2[counter+1]
      then Data[Counter+1]:= Source1[Counter+1]
      else Data[Counter+1]:= Source2[Counter+1];
    if Source1[counter+2] > Source2[counter+2]
      then Data[Counter+2]:= Source1[Counter+2]
      else Data[Counter+2]:= Source2[Counter+2];
    if Source1[counter+3] > Source2[counter+3]
      then Data[Counter+3]:= Source1[Counter+3]
      else Data[Counter+3]:= Source2[Counter+3];

    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    if Source1[counter] > Source2[counter] then
      Data[Counter]:= Source1[Counter]
    else
      Data[Counter]:= Source2[Counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.Max(V1, V2 : TFVector; const xStartInd, xCount : integer);
begin
  Max(V1, V2);
end;

procedure TX87Vector.Min(V1, V2 : TFVector);
var
  counter : integer;
  Data,Source1, Source2 :PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source1:=@V1.AlignedArray[NumElements];
  Source2:=@V2.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    if Source1[counter] < Source2[counter]
      then Data[Counter]:= Source1[Counter]
      else Data[Counter]:= Source2[Counter];
    if Source1[counter+1] < Source2[counter+1]
      then Data[Counter+1]:= Source1[Counter+1]
      else Data[Counter+1]:= Source2[Counter+1];
    if Source1[counter+2] < Source2[counter+2]
      then Data[Counter+2]:= Source1[Counter+2]
      else Data[Counter+2]:= Source2[Counter+2];
    if Source1[counter+3] < Source2[counter+3]
      then Data[Counter+3]:= Source1[Counter+3]
      else Data[Counter+3]:= Source2[Counter+3];

    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    if Source1[counter] < Source2[counter] then
      Data[Counter]:= Source1[Counter]
    else
      Data[Counter]:= Source2[Counter];
    inc(Counter);
  end;

end;

procedure TX87Vector.Min(V1, V2 : TFVector; const xStartInd, xCount : integer);
begin
  Min(V1, V2);
end;

procedure TX87Vector.MinMaxValues(var yMin, yMax : single);
begin
  // to be implemented
end;

procedure TX87Vector.MinMaxValues(var yMin, yMax : single; xStartInd, xCount : integer);
begin
  // to be implemented
end;

procedure TX87Vector.IndexMinMaxValues(var yMinInd, yMaxInd : integer);
begin
  // to be implemented
end;

procedure TX87Vector.IndexMinMaxValues(var yMinInd, yMaxInd : integer; const xStartInd, xCount : integer);
begin
  // to be implemented
end;

procedure TX87Vector.MinMaxAbsValues(var yMin, yMax : single);
begin
  // to be implemented
end;

procedure TX87Vector.MinMaxAbsValues(var yMin, yMax : single; xStartInd, xCount : integer);
begin
  // to be implemented
end;

procedure TX87Vector.IndexMinMaxAbsValues(var yMinInd, yMaxInd : integer);
begin
  // to be implemented
end;

procedure TX87Vector.IndexMinMaxAbsValues(var yMinInd, yMaxInd : integer; const xStartInd, xCount : integer);
begin
  // to be implemented
end;

procedure TX87Vector.AddSquare(V1, V2 : TFVector);
begin
  // to be implemented
end;

procedure TX87Vector.AddSquare(V1, V2 : TFVector; const xStartInd, xCount : integer);
begin
  AddSquare(V1, V2);
end;

procedure TX87Vector.Add(value : single);
var
  counter : integer;
  Data:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    Data[counter+0]:=Data[counter+0] + value;
    Data[counter+1]:=Data[counter+1] + value;
    Data[counter+2]:=Data[counter+2] + value;
    Data[counter+3]:=Data[counter+3] + value;
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter]:=Data[counter] + value;
    inc(Counter);
  end;
end;

procedure TX87Vector.Combine(V1, V2 : tFVector; xA, xB : single);
var
  counter : integer;
  Data, Source1, Source2 :PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source1:=@V1.AlignedArray[NumElements];
  Source2:=@V2.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    Data[counter] := xA*Source1[counter] + xB*Source2[counter];
    Data[counter+1] := xA*Source1[counter+1] + xB*Source2[counter+1];
    Data[counter+2] := xA*Source1[counter+2] + xB*Source2[counter+2];
    Data[counter+3] := xA*Source1[counter+3] + xB*Source2[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := xA*Source1[counter] + xB*Source2[counter];
    inc(Counter);
  end;
end;

procedure TX87Vector.Combine(V : tFVector; xA : single);
var
  counter : integer;
  Data, Source :PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    Data[counter] := Data[counter] + xA*Source[counter];
    Data[counter+1] := Data[counter+1] + xA*Source[counter+1];
    Data[counter+2] := Data[counter+2] + xA*Source[counter+2];
    Data[counter+3] := Data[counter+3] + xA*Source[counter+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Data[counter] + xA*Source[counter];
    inc(Counter);
  end;
end;

// Self := Self + (V - Self) * t
procedure TX87Vector.Lerp(V : tFVector; xT : single);
var
  counter : integer;
  Data,Source:PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source:=@V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    Data[counter] := Data[counter] + xT*(Source[counter] - Data[counter]);
    Data[counter+1] := Data[counter+1] + xT*(Source[counter+1] - Data[counter+1]);
    Data[counter+2] := Data[counter+2] + xT*(Source[counter+2] - Data[counter+2]);
    Data[counter+3] := Data[counter+3] + xT*(Source[counter+3] - Data[counter+3]);
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Data[counter] + xT*(Source[counter] - Data[counter]);
    inc(Counter);
  end;
end;

// Self := V1 + (V2 - V1) * t;
procedure TX87Vector.Lerp(V1, V2 : tFVector; xT : single);
var
  counter : integer;
  Data, Source1, Source2 :PSingleArray;
begin
  Data:=@AlignedArray[NumElements];
  Source1:=@V1.AlignedArray[NumElements];
  Source2:=@V2.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    Data[counter] := Source1[counter] + xT*(Source2[counter] - Source1[counter]);
    Data[counter+1] := Source1[counter+1] + xT*(Source2[counter+1] - Source1[counter+1]);
    Data[counter+2] := Source1[counter+2] + xT*(Source2[counter+2] - Source1[counter+2]);
    Data[counter+3] := Source1[counter+3] + xT*(Source2[counter+3] - Source1[counter+3]);
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Source1[counter] + xT*(Source2[counter] - Source1[counter]);
    inc(Counter);
  end;
end;

function TX87Vector.InnerSqrSum : single;
var
  counter : integer;
  Data:PSingleArray;
begin
  result := 0.0;
  Data:=@AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<-3 do
  begin
    result := result + system.sqr(Data[counter]) + system.sqr(Data[counter+1])
                     + system.sqr(Data[counter+2])+ system.sqr(Data[counter+3]);
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    result := result + system.sqr(Data[counter]);
    inc(Counter);
  end;
end;

function TX87Vector.NormL1(V : TFVector) : single;
var
  counter : integer;
  S1, S2:PSingleArray;
begin
  result := 0.0;
  S1:=@AlignedArray[NumElements];
  S2 := @V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    result := result + abs(S1[counter] - S2[counter]);
    inc(Counter);
  end;
end;

function TX87Vector.NormL2(V : TFVector) : single;
var
  counter : integer;
  S1, S2:PSingleArray;
begin
  result := 0.0;
  S1 :=@AlignedArray[NumElements];
  S2 := @V.AlignedArray[NumElements];
  Counter:=-NumElements;
  while Counter<0 do
  begin
    result := result + system.sqr(S1[counter] - S2[counter]);
    inc(Counter);
  end;
end;

// *************************************************** //
// *                  T3DNowVector                   * //
// *************************************************** //

constructor T3DNowVector.Create(const ALength : integer);
begin
  inherited Create(ALength);
end;

constructor T3DNowVector.CreateUsingArray(var A:array of single);
begin
  inherited CreateUsingArray(A);
end;

destructor T3DNowVector.Destroy;
begin
  inherited Destroy;
end;

procedure T3DNowVector.Add(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop
@LargeAddLoop:
{$IFDEF CISC_STYLE}
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetchw [eax+CacheLineLength] {$ENDIF}
  pfadd mm0,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [edx+CacheLineLength]  {$ENDIF}
  movq [eax],mm0

  movq mm2,[eax+8]
  pfadd mm2,[edx+8]
  movq [eax+8],mm2

  movq mm4,[eax+16]
  pfadd mm4,[edx+16]
  movq [eax+16],mm4

  movq mm6,[eax+24]
  pfadd mm6,[edx+24]
  movq [eax+24],mm6

  movq mm0,[eax+32]
  pfadd mm0,[edx+32]
  movq [eax+32],mm0

  movq mm2,[eax+40]
  pfadd mm2,[edx+40]
  movq [eax+40],mm2

  movq mm2,[eax+48]
  pfadd mm2,[edx+48]
  movq [eax+48],mm2

  movq mm4,[eax+56]
  pfadd mm4,[edx+56]
  movq [eax+56],mm4
  {$ELSE}
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}   prefetchw [eax+CacheLineLength] {$ENDIF}
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}   prefetch [edx+CacheLineLength] {$ENDIF}
  pfadd mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfadd mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pfadd mm4,mm5
  movq [eax+16],mm4

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pfadd mm6,mm7
  movq [eax+24],mm6

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pfadd mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pfadd mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pfadd mm4,mm5
  movq [eax+48],mm4

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pfadd mm6,mm7
  movq [eax+56],mm6
{$ENDIF}

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallAddLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pfadd mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfadd mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
  emms
end;

procedure T3DNowVector.Add( V1, V2 : TFVector);
asm
  push ebx
  mov ebx,[V2.AlignedArray]
  mov edx,[V1.AlignedArray]
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop
@LargeAddLoop:
  movq mm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}   prefetch [ebx+CacheLineLength] {$ENDIF}
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}   prefetch [edx+CacheLineLength] {$ENDIF}
  pfadd mm0,mm1
  movq [eax],mm0

  movq mm2,[ebx+8]
  movq mm3,[edx+8]
  pfadd mm2,mm3
  movq [eax+8],mm2

  movq mm4,[ebx+16]
  movq mm5,[edx+16]
  pfadd mm4,mm5
  movq [eax+16],mm4

  movq mm6,[ebx+24]
  movq mm7,[edx+24]
  pfadd mm6,mm7
  movq [eax+24],mm6

  movq mm0,[ebx+32]
  movq mm1,[edx+32]
  pfadd mm0,mm1
  movq [eax+32],mm0

  movq mm2,[ebx+40]
  movq mm3,[edx+40]
  pfadd mm2,mm3
  movq [eax+40],mm2

  movq mm4,[ebx+48]
  movq mm5,[edx+48]
  pfadd mm4,mm5
  movq [eax+48],mm4

  movq mm6,[ebx+56]
  movq mm7,[edx+56]
  pfadd mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  add ebx,64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallAddLoop:
  movq mm0,[ebx]
  movq mm1,[edx]
  pfadd mm0,mm1
  movq [eax],mm0

  movq mm2,[ebx+8]
  movq mm3,[edx+8]
  pfadd mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
  emms
  pop ebx
end;

function T3DNowVector.InnerSum : single;
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  pxor mm0, mm0  // mm0 will be sum result

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop

@LargeAddLoop:
{$IFDEF CISC_STYLE}
  // start work wity mm1
  movq mm1, [eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength] {$ENDIF}
  pfadd mm1, [eax+8]
  // while cpu busy with mm1 start work with mm2
  movq mm2, [eax + 16]
  pfadd mm2, [eax + 24]

  // while cpu busy with mm2 start work with mm3
  movq mm3,  [eax + 32]
  pfadd mm3,  [eax + 40]

  // hope, that mm1 is free from cpu
  pfadd mm1, [eax + 48]
  pfadd mm2, [eax + 56]

  pfadd mm0, mm1 //
  pfadd mm2, mm3 // while mm0, mm1 busy work with mm2, mm3
  pfadd mm0, mm2

{$ELSE}
  movq mm1, [eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength] {$ENDIF}
  movq mm2, [eax+8]
  pfadd mm1, mm2
  pfadd mm0, mm1

  movq mm1, [eax + 16]
  movq mm2, [eax + 24]
  pfadd mm1, mm2
  pfadd mm0, mm1

  movq mm1, [eax + 32]
  movq mm2, [eax + 40]
  pfadd mm1, mm2
  pfadd mm0, mm1

  movq mm1, [eax + 48]
  movq mm2, [eax + 56]
  pfadd mm1, mm2
  pfadd mm0, mm1
{$ENDIF}

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4

@SmallAddLoop:

  movq mm1, [eax]
  movq mm2, [eax+8]
  pfadd mm1, mm2
  pfadd mm0, mm1
{ not correct!!:
  pfadd mm0, mm1
  fadd mm0, mm2 }

  add eax,16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:

  pfacc mm0, mm0
  movd Result, mm0

  emms
end;

function T3DNowVector.DotProduct(V : TFVector) : single;
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  pxor mm0, mm0
  pxor mm1, mm1

  push ecx
  shr ecx,2  // number of small iterations = number of elements / 4
  jz @SmallAddLoop
  shr ecx,2  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop
@LargeAddLoop:
  movq mm2,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength] {$ENDIF}
  pfmul mm2,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength] {$ENDIF}
  pfadd mm0,mm2

  movq mm3,[eax + 8]
  pfmul mm3,[edx + 8]
  pfadd mm1,mm3

  movq mm2,[eax + 16]
  pfmul mm2,[edx + 16]
  pfadd mm0,mm2

  movq mm3,[eax + 24]
  pfmul mm3,[edx + 24]
  pfadd mm1,mm3

  movq mm2,[eax + 32]
  pfmul mm2,[edx + 32]
  pfadd mm0,mm2

  movq mm3,[eax + 40]
  pfmul mm3,[edx + 40]
  pfadd mm1,mm3

  movq mm2,[eax + 48]
  pfmul mm2,[edx + 48]
  pfadd mm0,mm2

  movq mm3,[eax + 56]
  pfmul mm3,[edx + 56]
  pfadd mm1,mm3

  add eax,64
  add edx, 64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallAddLoop:
  movq mm2,[eax]
  pfmul mm2,[edx]
  pfadd mm0,mm2

  movq mm3,[eax + 8]
  pfmul mm3,[edx + 8]
  pfadd mm1,mm3


  add eax,16
  add edx, 16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
  pfadd mm0, mm1
  pfacc mm0, mm0
  movd Result, mm0

  emms
end;


function T3DNowVector.NormL2(V : TFVector) : single;
asm
  // TODO - realize large loop and prefetch CacheLineLength
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  pxor mm0, mm0
  pxor mm1, mm1
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4

@SmallAddLoop:
  movq mm2, [eax]
  movq mm3, [edx]
  pfsub mm2, mm3
  pfmul mm2, mm2
  pfadd mm0, mm2

  movq mm4, [eax+8]
  movq mm5, [edx+8]
  pfsub mm4, mm5
  pfmul mm4, mm4
  pfadd mm1, mm4

  add eax,16
  add edx, 16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
  pfadd mm0, mm1
  pfacc mm0, mm0
  movd Result, mm0

  emms
end;

procedure T3DNowVector.Lerp(V : tFVector; xT : single);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]

  movd mm2, xT
  punpckldq   mm2,mm2             // unpack the xT value int (xT | xT)

  push ecx
  shr ecx,2  // number of small iterations = number of elements / 4
  jz @SmallAddLoop
  shr ecx,2  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop
@LargeAddLoop:
  movq mm0, [eax]
  movq mm1, [edx]
  pfsub mm1, mm0                //
  pfmul mm1, mm2                // mm1 :=  vT*(V-Self)
  pfadd mm0, mm1
  movq [eax], mm0

  movq mm3, [eax + 8]
  movq mm4, [edx + 8]
  pfsub mm4, mm3
  pfmul mm4, mm2
  pfadd mm3, mm4
  movq [eax+8], mm3

  movq mm5, [eax + 16]
  movq mm6, [edx + 16]
  pfsub mm6, mm5
  pfmul mm6, mm2
  pfadd mm5, mm6
  movq [eax + 16], mm5

  movq mm0, [eax + 24]
  movq mm1, [edx + 24]
  pfsub mm1, mm0
  pfmul mm1, mm2
  pfadd mm0, mm1
  movq [eax + 24], mm0

  movq mm3, [eax + 32]
  movq mm4, [edx + 32]
  pfsub mm4, mm3
  pfmul mm4, mm2
  pfadd mm3, mm4
  movq [eax+ 32], mm3

  movq mm5, [eax + 40]
  movq mm6, [edx + 40]
  pfsub mm6, mm5
  pfmul mm6, mm2
  pfadd mm5, mm6
  movq [eax+40], mm5

  movq mm0, [eax + 48]
  movq mm1, [edx + 48]
  pfsub mm1, mm0
  pfmul mm1, mm2
  pfadd mm0, mm1
  movq [eax + 48], mm0

  movq mm3, [eax + 56]
  movq mm4, [edx + 56]
  pfsub mm4, mm3
  pfmul mm4, mm2
  pfadd mm3, mm4
  movq [eax+56], mm3

  add eax,64
  add edx, 64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallAddLoop:
  movq mm0, [eax]
  movq mm1, [edx]
  pfsub mm1, mm0                //
  pfmul mm1, mm2                // mm1 :=  vT*(V-Self)
  pfadd mm0, mm1
  movq [eax], mm0

  movq mm3, [eax + 8]
  movq mm4, [edx + 8]
  pfsub mm4, mm3
  pfmul mm4, mm2
  pfadd mm3, mm4
  movq [eax+8], mm3

  add eax,16
  add edx, 16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
  emms

end;

procedure T3DNowVector.Sub(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeSubLoop
@LargeSubLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetchw [eax+CacheLineLength] {$ENDIF}
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [edx+CacheLineLength] {$ENDIF}
  pfsub mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfsub mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pfsub mm4,mm5
  movq [eax+16],mm4

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pfsub mm6,mm7
  movq [eax+24],mm6

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pfsub mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pfsub mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pfsub mm4,mm5
  movq [eax+48],mm4

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pfsub mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeSubLoop

@SkipLargeSubLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndSub
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallSubLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pfsub mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfsub mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallSubLoop

@EndSub:
  emms
end;

procedure T3DNowVector.Sub( V1, V2 : TFVector);
asm
  push ebx
  mov ebx,[V1.AlignedArray]
  mov edx,[V2.AlignedArray]
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeSubLoop
@LargeSubLoop:
  movq mm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [ebx+CacheLineLength] {$ENDIF}
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [edx+CacheLineLength] {$ENDIF}
  pfsub mm0,mm1
  movq [eax],mm0

  movq mm2,[ebx+8]
  movq mm3,[edx+8]
  pfsub mm2,mm3
  movq [eax+8],mm2

  movq mm4,[ebx+16]
  movq mm5,[edx+16]
  pfsub mm4,mm5
  movq [eax+16],mm4

  movq mm6,[ebx+24]
  movq mm7,[edx+24]
  pfsub mm6,mm7
  movq [eax+24],mm6

  movq mm0,[ebx+32]
  movq mm1,[edx+32]
  pfsub mm0,mm1
  movq [eax+32],mm0

  movq mm2,[ebx+40]
  movq mm3,[edx+40]
  pfsub mm2,mm3
  movq [eax+40],mm2

  movq mm4,[ebx+48]
  movq mm5,[edx+48]
  pfsub mm4,mm5
  movq [eax+48],mm4

  movq mm6,[ebx+56]
  movq mm7,[edx+56]
  pfsub mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  add ebx,64
  dec ecx
  jnz @LargeSubLoop

@SkipLargeSubLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndSub
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallSubLoop:
  movq mm0,[ebx]
  movq mm1,[edx]
  pfsub mm0,mm1
  movq [eax],mm0

  movq mm2,[ebx+8]
  movq mm3,[edx+8]
  pfsub mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallSubLoop

@EndSub:
  emms
  pop ebx
end;

procedure T3DNowVector.Mul(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMulLoop
@LargeMulLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetchw [eax+CacheLineLength] {$ENDIF}
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [edx+CacheLineLength] {$ENDIF}
  pfmul mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfmul mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pfmul mm4,mm5
  movq [eax+16],mm4

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pfmul mm6,mm7
  movq [eax+24],mm6

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pfmul mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pfmul mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pfmul mm4,mm5
  movq [eax+48],mm4

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pfmul mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeMulLoop

@SkipLargeMulLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMul
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMulLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pfmul mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfmul mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallMulLoop

@EndMul:
  emms
end;

procedure T3DNowVector.Mul( V1, V2 : TFVector);
asm
  push ebx
  mov ebx,[V1.AlignedArray]
  mov edx,[V2.AlignedArray]
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMulLoop
@LargeMulLoop:
  movq mm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [ebx+CacheLineLength]{$ENDIF}
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [edx+CacheLineLength]{$ENDIF}
  pfmul mm0,mm1
  movq [eax],mm0

  movq mm2,[ebx+8]
  movq mm3,[edx+8]
  pfmul mm2,mm3
  movq [eax+8],mm2

  movq mm4,[ebx+16]
  movq mm5,[edx+16]
  pfmul mm4,mm5
  movq [eax+16],mm4

  movq mm6,[ebx+24]
  movq mm7,[edx+24]
  pfmul mm6,mm7
  movq [eax+24],mm6

  movq mm0,[ebx+32]
  movq mm1,[edx+32]
  pfmul mm0,mm1
  movq [eax+32],mm0

  movq mm2,[ebx+40]
  movq mm3,[edx+40]
  pfmul mm2,mm3
  movq [eax+40],mm2

  movq mm4,[ebx+48]
  movq mm5,[edx+48]
  pfmul mm4,mm5
  movq [eax+48],mm4

  movq mm6,[ebx+56]
  movq mm7,[edx+56]
  pfmul mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  add ebx,64
  dec ecx
  jnz @LargeMulLoop

@SkipLargeMulLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMul
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMulLoop:
  movq mm0,[ebx]
  movq mm1,[edx]
  pfmul mm0,mm1
  movq [eax],mm0

  movq mm2,[ebx+8]
  movq mm3,[edx+8]
  pfmul mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallMulLoop

@EndMul:
  emms
  pop ebx
end;

procedure T3DNowVector.Divide(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeDivideLoop
@LargeDivideLoop:
  // cmp. AMD Athlon Processor x86 Code Optimization, August 1999, Chapter 10, page 97
  movq mm0,[edx]    // load y | x
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [edx+CacheLineLength]  {$ENDIF}
  pfrcp mm1,mm0     // approx 1/x | 1/x
  movq mm2,mm0      // y | x
  punpckhdq mm0,mm0 // y | y
  pfrcp mm0,mm0     // approx 1/y | 1/y
  punpckldq mm1,mm0 // approx 1/y | 1/x
  movq mm0,[eax]    // z | w
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetchw [eax+CacheLineLength] {$ENDIF}
  pfrcpit1 mm2,mm1  // intermediate 1/y | 1/x
  pfrcpit2 mm2,mm1  // final 1/y | 1/x
  pfmul mm0,mm2     // z/y | w/x
  movq [eax],mm0

  movq mm0,[edx+8]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[eax+8]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+8],mm0

  movq mm0,[edx+16]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[eax+16]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+16],mm0

  movq mm0,[edx+24]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[eax+24]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+24],mm0

  movq mm0,[edx+32]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[eax+32]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+32],mm0

  movq mm0,[edx+40]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[eax+40]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+40],mm0

  movq mm0,[edx+48]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[eax+48]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+48],mm0

  movq mm0,[edx+56]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[eax+56]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+56],mm0

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeDivideLoop

@SkipLargeDivideLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndDivide
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallDivideLoop:
  movq mm0,[edx]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[eax]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax],mm0

  movq mm0,[edx+8]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[eax+8]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+8],mm0

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallDivideLoop

@EndDivide:
  emms
end;

procedure T3DNowVector.Divide(V1, V2 : TFVector);
asm
  push ebx
  mov ebx,[V1.AlignedArray]
  mov edx,[V2.AlignedArray]
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeDivideLoop
  
@LargeDivideLoop:
  movq mm0,[edx]    // load y | x
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [edx+CacheLineLength] {$ENDIF}
  pfrcp mm1,mm0     // approx 1/x | 1/x
  movq mm2,mm0      // y | x
  punpckhdq mm0,mm0 // y | y
  pfrcp mm0,mm0     // approx 1/y | 1/y
  punpckldq mm1,mm0 // approx 1/y | 1/x
  movq mm0,[ebx]    // z | w
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [ebx+CacheLineLength] {$ENDIF}
  pfrcpit1 mm2,mm1  // intermediate 1/y | 1/x
  pfrcpit2 mm2,mm1  // final 1/y | 1/x
  pfmul mm0,mm2     // z/y | w/x
  movq [eax],mm0

  movq mm0,[edx+8]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[ebx+8]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+8],mm0

  movq mm0,[edx+16]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[ebx+16]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+16],mm0

  movq mm0,[edx+24]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[ebx+24]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+24],mm0

  movq mm0,[edx+32]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[ebx+32]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+32],mm0

  movq mm0,[edx+40]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[ebx+40]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+40],mm0

  movq mm0,[edx+48]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[ebx+48]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+48],mm0

  movq mm0,[edx+56]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[ebx+56]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+56],mm0

  add eax,64
  add edx,64
  add ebx,64
  dec ecx
  jnz @LargeDivideLoop

@SkipLargeDivideLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndDivide
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallDivideLoop:
  movq mm0,[edx]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[ebx]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax],mm0

  movq mm0,[edx+8]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  movq mm0,[ebx+8]
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  pfmul mm0,mm2
  movq [eax+8],mm0

  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallDivideLoop

@EndDivide:
  emms
  pop ebx
end;

procedure T3DNowVector.Reciprocal(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeReciprocalLoop
@LargeReciprocalLoop:
  // cmp. AMD Athlon Processor x86 Code Optimization, August 1999, Chapter 10, page 97
  movq mm0,[edx]    // load y | x
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$42,CacheLineLength  {$ENDIF}     /// prefetch [edx+CacheLineLength]
  pfrcp mm1,mm0     // approx 1/x | 1/x
  movq mm2,mm0      // y | x
  punpckhdq mm0,mm0 // y | y
  pfrcp mm0,mm0     // approx 1/y | 1/y
  punpckldq mm1,mm0 // approx 1/y | 1/x
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$48,CacheLineLength  {$ENDIF}     /// prefetchw [eax+CacheLineLength]
  pfrcpit1 mm2,mm1  // intermediate 1/y | 1/x
  pfrcpit2 mm2,mm1  // final 1/y | 1/x
  movq [eax],mm2

  movq mm0,[edx+8]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  movq [eax+8],mm2

  movq mm0,[edx+16]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  movq [eax+16],mm2

  movq mm0,[edx+24]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  movq [eax+24],mm2

  movq mm0,[edx+32]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  movq [eax+32],mm2

  movq mm0,[edx+40]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  movq [eax+40],mm2

  movq mm0,[edx+48]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  movq [eax+48],mm2

  movq mm0,[edx+56]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  movq [eax+56],mm2

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeReciprocalLoop

@SkipLargeReciprocalLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndReciprocal
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallReciprocalLoop:
  movq mm0,[edx]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  movq [eax],mm2

  movq mm0,[edx+8]
  pfrcp mm1,mm0
  movq mm2,mm0
  punpckhdq mm0,mm0
  pfrcp mm0,mm0
  punpckldq mm1,mm0
  pfrcpit1 mm2,mm1
  pfrcpit2 mm2,mm1
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallReciprocalLoop

@EndReciprocal:
  emms
end;

procedure T3DNowVector.Max(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMaxLoop
@LargeMaxLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetchw [eax+CacheLineLength] {$ENDIF}
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetch [edx+CacheLineLength] {$ENDIF}
  pfmax mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfmax mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pfmax mm4,mm5
  movq [eax+16],mm4

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pfmax mm6,mm7
  movq [eax+24],mm6

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pfmax mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pfmax mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pfmax mm4,mm5
  movq [eax+48],mm4

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pfmax mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeMaxLoop

@SkipLargeMaxLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMax
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMaxLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pfmax mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfmax mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallMaxLoop

@EndMax:
  emms
end;

procedure T3DNowVector.Max( V1, V2 : TFVector);
asm
  push ebx
  mov ebx,[V1.AlignedArray]
  mov edx,[V2.AlignedArray]
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMaxLoop
@LargeMaxLoop:
  movq mm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetch [ebx+CacheLineLength] {$ENDIF}
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetch [edx+CacheLineLength] {$ENDIF}
  pfmax mm0,mm1
  movq [eax],mm0

  movq mm2,[ebx+8]
  movq mm3,[edx+8]
  pfmax mm2,mm3
  movq [eax+8],mm2

  movq mm4,[ebx+16]
  movq mm5,[edx+16]
  pfmax mm4,mm5
  movq [eax+16],mm4

  movq mm6,[ebx+24]
  movq mm7,[edx+24]
  pfmax mm6,mm7
  movq [eax+24],mm6

  movq mm0,[ebx+32]
  movq mm1,[edx+32]
  pfmax mm0,mm1
  movq [eax+32],mm0

  movq mm2,[ebx+40]
  movq mm3,[edx+40]
  pfmax mm2,mm3
  movq [eax+40],mm2

  movq mm4,[ebx+48]
  movq mm5,[edx+48]
  pfmax mm4,mm5
  movq [eax+48],mm4

  movq mm6,[ebx+56]
  movq mm7,[edx+56]
  pfmax mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add ebx,64
  add edx,64

  dec ecx
  jnz @LargeMaxLoop

@SkipLargeMaxLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMax
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMaxLoop:
  movq mm0,[ebx]
  movq mm1,[edx]
  pfmax mm0,mm1
  movq [eax],mm0

  movq mm2,[ebx+8]
  movq mm3,[edx+8]
  pfmax mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallMaxLoop

@EndMax:
  emms
  pop ebx
end;

procedure T3DNowVector.Min(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMinLoop
@LargeMinLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetchw [eax+CacheLineLength] {$ENDIF}
   movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch  [edx+CacheLineLength] {$ENDIF}
  pfmin mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfmin mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pfmin mm4,mm5
  movq [eax+16],mm4

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pfmin mm6,mm7
  movq [eax+24],mm6

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pfmin mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pfmin mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pfmin mm4,mm5
  movq [eax+48],mm4

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pfmin mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeMinLoop

@SkipLargeMinLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMin
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMinLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pfmin mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfmin mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallMinLoop

@EndMin:
  emms
end;

procedure T3DNowVector.Min( V1, V2 : TFVector);
asm
  push ebx
  mov ebx,[V2.AlignedArray]
  mov edx,[V2.AlignedArray]
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeMinLoop
@LargeMinLoop:
  movq mm0,[ebx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}   prefetch [ebx+CacheLineLength] {$ENDIF}
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}   prefetch [edx+CacheLineLength] {$ENDIF}
  pfmin mm0,mm1
  movq [eax],mm0

  movq mm2,[ebx+8]
  movq mm3,[edx+8]
  pfmin mm2,mm3
  movq [eax+8],mm2

  movq mm4,[ebx+16]
  movq mm5,[edx+16]
  pfmin mm4,mm5
  movq [eax+16],mm4

  movq mm6,[ebx+24]
  movq mm7,[edx+24]
  pfmin mm6,mm7
  movq [eax+24],mm6

  movq mm0,[ebx+32]
  movq mm1,[edx+32]
  pfmin mm0,mm1
  movq [eax+32],mm0

  movq mm2,[ebx+40]
  movq mm3,[edx+40]
  pfmin mm2,mm3
  movq [eax+40],mm2

  movq mm4,[ebx+48]
  movq mm5,[edx+48]
  pfmin mm4,mm5
  movq [eax+48],mm4

  movq mm6,[ebx+56]
  movq mm7,[edx+56]
  pfmin mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  add ebx,64
  dec ecx
  jnz @LargeMinLoop

@SkipLargeMinLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndMin
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallMinLoop:
  movq mm0,[ebx]
  movq mm1,[edx]
  pfmin mm0,mm1
  movq [eax],mm0

  movq mm2,[ebx+8]
  movq mm3,[edx+8]
  pfmin mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  add ebx,16
  dec ecx
  jnz @SmallMinLoop

@EndMin:
  emms
  pop ebx
end;

procedure T3DNowVector.LogicalAnd(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeLogicalAndLoop
@LargeLogicalAndLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$48,CacheLineLength  {$ENDIF}     /// prefetchw [eax+CacheLineLength]
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$42,CacheLineLength  {$ENDIF}     /// prefetch [edx+CacheLineLength]
  pand mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pand mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pand mm4,mm5
  movq [eax+16],mm4

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pand mm6,mm7
  movq [eax+24],mm6

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pand mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pand mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pand mm4,mm5
  movq [eax+48],mm4

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pand mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeLogicalAndLoop

@SkipLargeLogicalAndLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndLogicalAnd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallLogicalAndLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pand mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pand mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallLogicalAndLoop

@EndLogicalAnd:
  emms
end;

procedure T3DNowVector.LogicalOr(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeLogicalOrLoop
@LargeLogicalOrLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$48,CacheLineLength  {$ENDIF}     /// prefetchw [eax+CacheLineLength]
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$42,CacheLineLength  {$ENDIF}     /// prefetch [edx+CacheLineLength]
  por mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  por mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  por mm4,mm5
  movq [eax+16],mm4

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  por mm6,mm7
  movq [eax+24],mm6

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  por mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  por mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  por mm4,mm5
  movq [eax+48],mm4

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  por mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeLogicalOrLoop

@SkipLargeLogicalOrLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndLogicalOr
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallLogicalOrLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  por mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  por mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallLogicalOrLoop

@EndLogicalOr:
  emms
end;

procedure T3DNowVector.LogicalXor(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeLogicalXorLoop
@LargeLogicalXorLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$48,CacheLineLength  {$ENDIF}     /// prefetchw [eax+CacheLineLength]
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$42,CacheLineLength  {$ENDIF}     /// prefetch [edx+CacheLineLength]
  pxor mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pxor mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pxor mm4,mm5
  movq [eax+16],mm4

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pxor mm6,mm7
  movq [eax+24],mm6

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pxor mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pxor mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pxor mm4,mm5
  movq [eax+48],mm4

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pxor mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeLogicalXorLoop

@SkipLargeLogicalXorLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndLogicalXor
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallLogicalXorLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pxor mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pxor mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallLogicalXorLoop

@EndLogicalXor:
  emms
end;

procedure T3DNowVector.CmpGreater(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeCmpGreaterLoop
@LargeCmpGreaterLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$48,CacheLineLength  {$ENDIF}     /// prefetchw [eax+CacheLineLength]
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$42,CacheLineLength  {$ENDIF}     /// prefetch [edx+CacheLineLength]
  pfcmpgt mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfcmpgt mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pfcmpgt mm4,mm5
  movq [eax+16],mm4

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pfcmpgt mm6,mm7
  movq [eax+24],mm6

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pfcmpgt mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pfcmpgt mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pfcmpgt mm4,mm5
  movq [eax+48],mm4

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pfcmpgt mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpGreaterLoop

@SkipLargeCmpGreaterLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndCmpGreater
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallCmpGreaterLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pfcmpgt mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfcmpgt mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpGreaterLoop

@EndCmpGreater:
  emms
end;

procedure T3DNowVector.CmpLower(V : TFVector);
// A < B decomes B > A
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeCmpLowerLoop
@LargeCmpLowerLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$48,CacheLineLength  {$ENDIF}     /// prefetchw [eax+CacheLineLength]
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$42,CacheLineLength  {$ENDIF}     /// prefetch [edx+CacheLineLength]
  pfcmpgt mm1,mm0
  movq [eax],mm1

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfcmpgt mm3,mm2
  movq [eax+8],mm3

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pfcmpgt mm5,mm4
  movq [eax+16],mm5

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pfcmpgt mm7,mm6
  movq [eax+24],mm7

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pfcmpgt mm1,mm0
  movq [eax+32],mm1

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pfcmpgt mm3,mm2
  movq [eax+40],mm3

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pfcmpgt mm5,mm4
  movq [eax+48],mm5

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pfcmpgt mm7,mm6
  movq [eax+56],mm7

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpLowerLoop

@SkipLargeCmpLowerLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndCmpLower
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallCmpLowerLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pfcmpgt mm1,mm0
  movq [eax],mm1

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfcmpgt mm3,mm2
  movq [eax+8],mm3

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpLowerLoop

@EndCmpLower:
  emms
end;

procedure T3DNowVector.CmpEqual(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeCmpEqualLoop
@LargeCmpEqualLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$48,CacheLineLength  {$ENDIF}     /// prefetchw [eax+CacheLineLength]
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$42,CacheLineLength  {$ENDIF}     /// prefetch [edx+CacheLineLength]
  pfcmpeq mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfcmpeq mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pfcmpeq mm4,mm5
  movq [eax+16],mm4

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pfcmpeq mm6,mm7
  movq [eax+24],mm6

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pfcmpeq mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pfcmpeq mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pfcmpeq mm4,mm5
  movq [eax+48],mm4

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pfcmpeq mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpEqualLoop

@SkipLargeCmpEqualLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndCmpEqual
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallCmpEqualLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pfcmpeq mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfcmpeq mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpEqualLoop

@EndCmpEqual:
  emms
end;

procedure T3DNowVector.CmpGreaterEqual(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeCmpGreaterEqualLoop
@LargeCmpGreaterEqualLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$48,CacheLineLength  {$ENDIF}     /// prefetchw [eax+CacheLineLength]
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$42,CacheLineLength  {$ENDIF}     /// prefetch [edx+CacheLineLength]
  pfcmpge mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfcmpge mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pfcmpge mm4,mm5
  movq [eax+16],mm4

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pfcmpge mm6,mm7
  movq [eax+24],mm6

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pfcmpge mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pfcmpge mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pfcmpge mm4,mm5
  movq [eax+48],mm4

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pfcmpge mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpGreaterEqualLoop

@SkipLargeCmpGreaterEqualLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndCmpGreaterEqual
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallCmpGreaterEqualLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pfcmpge mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfcmpge mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpGreaterEqualLoop

@EndCmpGreaterEqual:
  emms
end;

procedure T3DNowVector.CmpLowerEqual(V : TFVector);
// A <= B becomes B >= A
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeCmpLowerEqualLoop
@LargeCmpLowerEqualLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$48,CacheLineLength  {$ENDIF}     /// prefetchw [eax+CacheLineLength]
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  db $0F,$0D,$42,CacheLineLength  {$ENDIF}     /// prefetch [edx+CacheLineLength]
  pfcmpge mm1,mm0
  movq [eax],mm1

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfcmpge mm3,mm2
  movq [eax+8],mm3

  movq mm4,[eax+16]
  movq mm5,[edx+16]
  pfcmpge mm5,mm4
  movq [eax+16],mm5

  movq mm6,[eax+24]
  movq mm7,[edx+24]
  pfcmpge mm7,mm6
  movq [eax+24],mm7

  movq mm0,[eax+32]
  movq mm1,[edx+32]
  pfcmpge mm1,mm0
  movq [eax+32],mm1

  movq mm2,[eax+40]
  movq mm3,[edx+40]
  pfcmpge mm3,mm2
  movq [eax+40],mm3

  movq mm4,[eax+48]
  movq mm5,[edx+48]
  pfcmpge mm5,mm4
  movq [eax+48],mm5

  movq mm6,[eax+56]
  movq mm7,[edx+56]
  pfcmpge mm7,mm6
  movq [eax+56],mm7

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpLowerEqualLoop

@SkipLargeCmpLowerEqualLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndCmpLowerEqual
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallCmpLowerEqualLoop:
  movq mm0,[eax]
  movq mm1,[edx]
  pfcmpge mm1,mm0
  movq [eax],mm1

  movq mm2,[eax+8]
  movq mm3,[edx+8]
  pfcmpge mm3,mm2
  movq [eax+8],mm3

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpLowerEqualLoop

@EndCmpLowerEqual:
  emms
end;

procedure T3DNowVector.AddSquare(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeAddSquareLoop
@LargeAddSquareLoop:
  movq mm1,[edx]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetch [edx+CacheLineLength]  {$ENDIF}
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetchw [eax+CacheLineLength] {$ENDIF}
  pfmul mm1,mm1
  pfadd mm0,mm1
  movq [eax],mm0

  movq mm3,[edx+8]
  movq mm2,[eax+8]
  pfmul mm3,mm3      // DB = 11 011 011
  pfadd mm2,mm3      // D3 = 11 010 011
  movq [eax+8],mm2

  movq mm5,[edx+16]
  movq mm4,[eax+16]
  pfmul mm5,mm5
  pfadd mm4,mm5
  movq [eax+16],mm4

  movq mm7,[edx+24]
  movq mm6,[eax+24]
  pfmul mm7,mm7
  pfadd mm6,mm7
  movq [eax+24],mm6

  movq mm1,[edx+32]
  movq mm0,[eax+32]
  pfmul mm1,mm1
  pfadd mm0,mm1
  movq [eax+32],mm0

  movq mm3,[edx+40]
  movq mm2,[eax+40]
  pfmul mm3,mm3
  pfadd mm2,mm3
  movq [eax+40],mm2

  movq mm5,[edx+48]
  movq mm4,[eax+48]
  pfmul mm5,mm5
  pfadd mm4,mm5
  movq [eax+48],mm4

  movq mm7,[edx+56]
  movq mm6,[eax+56]
  pfmul mm7,mm7
  pfadd mm6,mm7
  movq [eax+56],mm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeAddSquareLoop

@SkipLargeAddSquareLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAddSquare
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
@SmallAddSquareLoop:
  movq mm1,[edx]
  movq mm0,[eax]
  pfmul mm1,mm1
  pfadd mm0,mm1
  movq [eax],mm0

  movq mm3,[edx+8]
  movq mm2,[eax+8]
  pfmul mm3,mm3
  pfadd mm2,mm3
  movq [eax+8],mm2

  add eax,16
  add edx,16
  dec ecx
  jnz @SmallAddSquareLoop

@EndAddSquare:
  emms
end;

procedure T3DNowVector.Scale(value : single);
asm
   mov ecx,[eax + NumElements]
   mov edx,[eax + AlignedArray]
   shr ecx,1
   movd mm0,DWORD PTR [ebp + 8] // load value
   movq mm1,mm0
   psllq mm1,32
   por mm0,mm1
 @ScaleLoop:
   movq mm2,[edx]
   pfmul mm2,mm0
   movq [edx],mm2
   add edx,8
   dec ecx
   jnz @ScaleLoop

   emms
end;

// *************************************************** //
// *                  TFMatrix                       * //
// *************************************************** //

class function TFMatrix.BuildMatrix(const xRows, xCols: integer): TFMatrix;
begin
  if EnableSSE
     then result := TSSEMatrix.Create(xRows, xCols)
     else if Enable3DNow
             then result := T3DNowMatrix.Create(xRows, xCols)
             else result := TX87Matrix.Create(xRows, xCols);
end;

procedure tFMatrix.CopyMatrix(const M: tFMatrix);
begin
  if fNumElements <> M.fNumElements
     then SetSize(M.Rows, M.ActualCols);
  move(M.fAlignedArray^, fAlignedArray^, SizeOf(single) * fNumElements);
end;

constructor TFMatrix.Create(const xRows, xCols: integer);
begin
  inherited Create;
  fAlignedArray := nil;
  fAllocatedArray := nil;
  fNumElements := 0;
  fActualCols := 0;
  fRows := 0;
  fCols := 0;

  SetSize(xRows, xCols);
end;

destructor TFMatrix.Destroy;
begin
  if fAllocatedArray <> nil
     then FreeMem(fAllocatedArray);
  fAllocatedArray := nil;
  inherited Destroy;
end;

function TFMatrix.GetDataElement(index1, index2: integer): single;
begin
  result := fAlignedArray[index1*fActualCols + index2];
end;


function TFMatrix.GetRowsArray(index: integer): PSingleArray;
begin
  result := @fAlignedArray[index*fActualCols];
end;

class function TFMatrix.is3DNow: boolean;
begin
  result := Enable3DNow;
end;

class function TFMatrix.isSSE: boolean;
begin
  result := EnableSSE;
end;

class function TFMatrix.isCMOV : boolean;
begin
  result := EnableCMOV;
end;

procedure TFMatrix.SetDataElement(index1, index2: integer; const Value: single);
begin
  fAlignedArray[index1*fActualCols + index2] := Value;
end;

procedure TFMatrix.SetSize(const xRows, xCols: integer);
begin
  fCols := xCols;
  fRows := xRows;
  // rounds ColsCount on blocks of 4 elements

  if (xCols and 3) > 0
     then fActualCols  := (xCols and ($FFFFFFFF - 3)) + 4
     else fActualCols  := xCols;


    if (fAllocatedArray <> nil) and (fRows*fActualCols = fNumElements)
     then exit;
    if fAllocatedArray <> nil
     then FreeMem(fAllocatedArray);

  fNumElements := fRows*fActualCols;
  if fNumElements = 0 then  // one of xRows, xCols was zero
    begin
    fRows := 0;
    fCols := 0;
    fAllocatedArray := nil;
    fAlignedArray := nil;
    exit;
    end;
  try
      GetMem(fAllocatedArray, (fNumElements * SizeOf(single)) + 16);
      // AlignedArray is aligned on 16 bytes boundary
      fAlignedArray := PSingleArray((integer(fAllocatedArray) and $FFFFFFF0) + 16);
  except
    on EOutOfMemory do
       begin
       fNumElements := 0;
       fActualCols := 0;
       fRows := 0;
       fCols := 0;
       fAllocatedArray := nil;
       fAlignedArray := nil;
       raise;  // re-raise exception to calling function
       end;
  end;
end;

procedure TFMatrix.SetValue(xValue: single);
var
  counter : integer;
begin
  // TODO : it is very simple and not effective realization - needs redo
  for counter := 0 to (fNumElements - 1) do
      fAlignedArray[counter] := xValue;
end;

procedure TFMatrix.ClearArray;
begin
  fillchar(fAlignedArray^, fNumElements * SizeOf(Single), 0);
end;

// testing functions
procedure SyncVectors(var V1,V2 : TFVector);
begin
  V1.CopyArray(V2);
end;
procedure Riteln(lStr: String);
begin
    {$IFDEF console}
    writeln(lStr);
    {$ELSE}
    Showmessage(lStr);
    {$ENDIF}
end;

procedure TestLogicalOps( var TestVector1, TestVector2,TestVector:TFVector);
var    counter : integer;
       AuxP:pointer;
       P1,P2,P:PLongWordArray;
begin
  AuxP:=TestVector1.AlignedArray; P1:=AuxP;
  AuxP:=TestVector2.AlignedArray; P2:=AuxP;
  AuxP:=TestVector.AlignedArray;  P:=AuxP;
  riteln('Testing AndOrXor ops');
  Randomize;
  for counter := 0 to (cTestVectorSize - 1) do
      begin
      TestVector1[counter] := random * 1000;
      TestVector[counter] := TestVector1[counter];
      TestVector2[counter] := random * 1000;
      end;

  // test AND
  for counter := 0 to (cTestVectorSize - 1) do
      P^[counter] := P^[counter] and P2^[counter];

  TestVector1.LogicalAnd(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln('Error: And')
     else riteln('And WORKED!');
  SyncVectors(TestVector, TestVector1);

  // test OR
  for counter := 0 to (cTestVectorSize - 1) do
      P^[counter] := P^[counter] or P2^[counter];

  TestVector1.LogicalOr(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln('Error: Or')
     else riteln('Or WORKED!');
  SyncVectors(TestVector, TestVector1);

  // test XOR
  for counter := 0 to (cTestVectorSize - 1) do
      P^[counter] := P^[counter] xor P2^[counter];

  TestVector1.LogicalXOr(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln('Error: XOr')
     else riteln('XOr WORKED!');
  SyncVectors(TestVector, TestVector1);
end;

procedure TestBinaryBasicOps( var TestVector1, TestVector2,TestVector:TFVector);
var    counter : integer;
begin
  riteln('Testing simple ops');
  Randomize;
  for counter := 0 to (cTestVectorSize - 1) do
      begin
      TestVector1[counter] := random * 1000;
      if TestVector1[counter] = 0
        then TestVector1[counter] := 1.0;
      TestVector[counter] := TestVector1[counter];
      TestVector2[counter] := random * 1000;
      if TestVector2[counter] = 0
        then TestVector2[counter] := 1.0;
      end;

  // test ADD
  for counter := 0 to (cTestVectorSize - 1) do
      TestVector[counter] := TestVector[counter] + TestVector2[counter];
  TestVector1.Add(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln('Error: Add');
  SyncVectors(TestVector, TestVector1);

  // test SUB
  for counter := 0 to (cTestVectorSize - 1) do
      TestVector[counter] := TestVector[counter] - TestVector2[counter];
  TestVector1.Sub(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln('Error: Sub');
  SyncVectors(TestVector, TestVector1);

  // test MUL
  for counter := 0 to (cTestVectorSize - 1) do
      TestVector[counter] := TestVector[counter] * TestVector2[counter];
  TestVector1.Mul(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln('Error: Mul');
  SyncVectors(TestVector, TestVector1);

  // test DIV
  for counter := 0 to (cTestVectorSize - 1) do
      TestVector[counter] := TestVector[counter] / TestVector2[counter];
  TestVector1.Divide(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln('Error: Divide');

end;

procedure TestBinaryComplexOps( var TestVector1, TestVector2,TestVector:TFVector);
var counter : integer;
    res : single;
    otherres : single;
    lStr1,lStr2,lStr3: string;

    procedure ShowError(S:string);
    begin
     riteln(S);
     Str((res-otherres):12:5,lStr1);
     Str((res):12:5,lStr2);
     Str((otherres):12:5,lStr3);
     riteln(' ERROR = '+lStr1+'   Exentia:'+lStr2+' Pascal:'+lStr3);
    end;

begin
  riteln('Testing complex ops');
  Randomize;
  for counter := 0 to (cTestVectorSize - 1) do
      begin
      TestVector1[counter] := (random-0.5) * 1000;
      if TestVector1[counter] = 0
        then TestVector1[counter] := 1.0;
      TestVector[counter] := TestVector1[counter];
      TestVector2[counter] := (random-0.5) * 1000;
      if TestVector2[counter] = 0
        then TestVector2[counter] := 1.0;
      end;

  // test INNER SUM
  res := 0;
  for counter := 0 to (cTestVectorSize - 1) do
      res := res + TestVector1.Data[counter];
  otherres := TestVector1.InnerSum;
  // Note: these results are different (due to rounding error/s?)
  if res <> otherres
     then ShowError('Error: InnerSum:');

  // test INNER ABS SUM
  res := 0;
  for counter := 0 to (cTestVectorSize - 1) do
      res := res + Abs(TestVector1.Data[counter]);
  otherres := TestVector1.InnerSumAbs;
  // Note: these results are different (due to rounding errors?)
  if res <> otherres
     then ShowError('Error: InnerSumAbs:');

  // test ADD + SQUARE
  for counter := 0 to (cTestVectorSize - 1) do
      TestVector[counter] := TestVector[counter] + TestVector2[counter] * TestVector2[counter];
  TestVector1.AddSquare(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then ShowError('Error: Add Square');
  SyncVectors(TestVector, TestVector1);
end;

{ TFCube }

class function TFCube.BuildCube(const xRows, xCols,
  xLayers: integer): TFCube;
begin
  if EnableSSE
     then result := TSSECube.Create(xRows, xCols, xLayers)
     else if Enable3DNow
             then result := T3DNowCube.Create(xRows, xCols, xLayers)
             else result := TX87Cube.Create(xRows, xCols, xLayers);
end;

constructor TFCube.Create(const xRows, xCols, xLayers: integer);
begin
  inherited Create;
  fAlignedArray := nil;
  fAllocatedArray := nil;
  fNumElements := 0;
  SetSize(xRows, xCols, xLayers);
end;

destructor TFCube.Destroy;
begin
  if fAllocatedArray <> nil
     then FreeMem(fAllocatedArray);
  fAllocatedArray := nil;
  inherited Destroy;
end;

function TFCube.GetCellsArray(xRowInd, xColInd: integer): PSingleArray;
begin
  // not tested!
  result := @fAlignedArray[(xRowInd*fCols + xColInd)*fActualLayers];
end;

function TFCube.GetDataElement(index1, index2, index3: integer): single;
begin
  // not tested
  result := fAlignedArray[(index2 + index1*fCols)*fActualLayers + index3];
end;

class function TFCube.is3DNow: boolean;
begin
  result := Enable3DNow;
end;

class function TFCube.isSSE: boolean;
begin
 result := EnableSSE;
end;

class function TFCube.isCMOV : boolean;
begin
  result := EnableCMOV;
end;

procedure TFCube.SetDataElement(index1, index2, index3: integer; const Value: single);
begin
  try
  fAlignedArray[(index2 + index1*fCols)*fActualLayers + index3] := Value;
  except
    ShowMessageFmt('Cols = %d; AcLayers = %d;  [%d : %d : %d]', [fCols, fActualLayers, index1, index2, index3]);
  end;
end;

procedure TFCube.SetSize(const xRows, xCols, xLayers: integer);
begin
  fCols := xCols;
  fRows := xRows;
  flayers := xLayers;
  // rounds LayersCount on blocks of 4 elements
  if (xLayers and 3) > 0
     then fActualLayers  := (xLayers and ($FFFFFFFF - 3)) + 4
     else fActualLayers  := xLayers;

  if (fAllocatedArray <> nil) and (fRows*fCols*fActualLayers = fNumElements)
     then exit;
  if fAllocatedArray <> nil
     then FreeMem(fAllocatedArray);

  fNumElements := fRows*fCols*fActualLayers;

  if fNumElements = 0 then
    begin
    fCols := 0;
    fRows := 0;
    fLayers := 0;
    fActualLayers := 0;
    fAllocatedArray := nil;
    fAlignedArray := nil;
    exit;
    end;

  try
      GetMem(fAllocatedArray, (fNumElements * SizeOf(single)) + 16);
      // AlignedArray is aligned on 16 bytes boundary }
      fAlignedArray := PSingleArray((integer(fAllocatedArray) and $FFFFFFF0) + 16);
  except
    on EOutOfMemory do
       begin
       fNumElements := 0;
       fActualLayers := 0;
       fLayers := 0;
       fRows := 0;
       fCols := 0;
       fAllocatedArray := nil;
       fAlignedArray := nil;
       raise;  // re-raise exception to calling function
       end;
  end;
end;

procedure TFCube.SetValue(xValue: single);
var
  counter : integer;
begin
  // TODO : it is very simple and not effective realization - needs redo
  for counter := 0 to (fNumElements - 1) do
      fAlignedArray[counter] := xValue;
end;

initialization
  try
    EnableSSE  := false;
    EnableCMOV := False;
    asm
      mov eax, 1
      db $0F,$A2               /// cpuid
      test edx,(1 shl 25)
      jnz @SSEFound
      mov EnableSSE,0
      jmp @END_SSE
    @SSEFound:
      mov EnableSSE,1
    @END_SSE:

      test edx,(1 shl 15)
      jnz @CMOVFound
      mov EnableCMOV,0
      jmp @END_CMOV
    @CMOVFound:
      mov EnableCMOV,1
    @END_CMOV:
    end;
  except
    EnableSSE  := false;
    EnableCMOV := False;
  end;
  if (EnableSSE)
     then begin
          Enable3DNow := false;
          end
     else begin
          Enable3DNow := false;
          // check for 3DNow!
          asm
            //test whether extended function 80000001h is supported
            mov eax, 80000000h      //call extended function 80000000h
            db $0F,$A2               /// cpuid                   //reports back highest supported ext. function
            cmp eax, 80000000h      //supports functions > 80000000h?
            jbe @NO_EXTENDED        //no 3DNow! support, either
            //test if function 80000001h indicates 3DNow! support
            mov eax, 80000001h      //call extended function 80000001h
            db $0F,$A2               /// cpuid                   //reports back extended feature flags
            test edx, 80000000h     //bit 31 in extended features
            jz @NO_3DNow            //if set, 3DNow! is supported
            mov Enable3DNow,1
            jmp @END_3DNow
          @NO_EXTENDED:
          @NO_3DNow:
            mov Enable3DNow,0
            jmp @END_3DNow
          @END_3DNow:
          end;
          end;
end.

