
//exentia64 - SSE2 Exentia for double precision0
// *****************************************************************************
//                   Exentia64 Library
//    64-bit modifications by Tom Womack 15/8/2001
//    http://www.tom.womack.net/x86FAQ/faq_features.html
//    64-bit modifications tested by Chris Rorden 15/8/2001
//    http://www.psychology.nottingham.ac.uk/staff/cr1/simd.html
//
//    Web: http://exentia.tommesani.com
//    E-mail: stefano@tommesani.com
//
// The contents of this file are subject to the Mozilla Public License Version
// 1.0; you may not use this file except in compliance with the License.
// You may obtain a copy of the License at http://www.mozilla.org/MPL/
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// for the specific language governing rights and limitations under the License.
// *****************************************************************************

{$DEFINE NDELPHI_CODE}

unit Exentia64;

interface

{$IFDEF LINUX}
uses SysUtils,Qdialogs;
{$else}
uses SysUtils,dialogs;
{$ENDIF}

type
  TDoubleArray = array [0..0] of double;
  PDoubleArray = ^TDoubleArray;

  TPointerArray = array [0..0] of pointer; //JPSS 14/3/2001
  PPointerArray = ^TPointerArray;          //JPSS 14/3/2001

  //TLongWordArray = array [0..0] of LongWord; //JPSS 14/3/2001
  //PLongWordArray = ^TLongWordArray;          //JPSS 14/3/2001


  TInt64Array = array [0..0] of INT64; //JPSS 14/3/2001
  PInt64Array = ^TInt64Array;          //JPSS 14/3/2001

type
  TFVector64= class(TObject)
  private
    AlignedArray64 : PDoubleArray;
    AllocatedArray64 : pointer;
    NumElements64 : integer;
    function GetDataElement(index : integer) : double;
    procedure SetDataElement(index : integer; const value : double);
  public
    property DataArray : PDoubleArray read AlignedArray64;
    property Data[index : integer] : double read GetDataElement write SetDataElement; default;
    constructor Create(const ALength : integer);
    constructor CreateUsingArray(var A:array of Double);                // JPSS  28/02/2001
    constructor Copy(const V : TFVector64); // Self = V
    destructor Destroy;override;
    class function BuildVector(const ALength : integer) : TFVector64;
    class function BuildUsingArray(var A:array of double) : TFVector64;   // JPSS  28/02/2001
    class function CopyVector(const V : TFVector64) : TFVector64;
    class function isSSE64 : boolean;

    procedure UseArray(var A:array of double);                          // JPSS  28/02/2001
    procedure SetLength(const ALength : integer);
    procedure CopyArray(const V : TFVector64);                            // FIXED by JPSS 7/4/2001
    procedure ImportArray(const Source : PdoubleArray; const Length : integer = 0);
    procedure ExportArray(Destination : PdoubleArray; const Length : integer = 0);
    procedure ClearArray; // Self = 0
    procedure SetArray(value : double);  // Self[i] = value
    function Equal(V : TFVector64) : boolean;
    // basic operations
    procedure Add(V : TFVector64); virtual; abstract;  // Self = Self + V
    procedure Sub(V : TFVector64); virtual; abstract;  // Self = Self - V
    procedure Mul(V : TFVector64); virtual; abstract;  // Self = Self * V
    procedure Divide(V : TFVector64); virtual; abstract;  // Self = Self / V
    procedure Sqrt(V : TFVector64); virtual; abstract;  // Self = Sqrt(V)
    procedure Reciprocal(V : TFVector64); virtual; abstract;  // Self = 1 / V
    procedure RecSqrt(V : TFVector64); virtual; abstract;  // Self = 1 / Sqrt(V)
    procedure Max(V : TFVector64); virtual; abstract;  // Self = Max(Self, V)
    procedure Min(V : TFVector64); virtual; abstract;  // Self = Min(Self, V)
    procedure LogicalAnd(V : TFVector64); virtual; abstract;  // Self = Self and V
    procedure LogicalOr(V : TFVector64); virtual; abstract;   // Self = Self or V
    procedure LogicalXor(V : TFVector64); virtual; abstract;  // Self = Self xor V
    procedure CmpGreater(V : TFVector64); virtual; abstract;       // Self = Self > V
    procedure CmpLower(V : TFVector64); virtual; abstract;         // Self = Self < V
    procedure CmpEqual(V : TFVector64); virtual; abstract;         // Self = Self == V
    procedure CmpGreaterEqual(V : TFVector64); virtual; abstract;  // Self = Self >= V
    procedure CmpLowerEqual(V : TFVector64); virtual; abstract;    // Self = Self <= V
    // complex operations
    procedure AddSquare(V : TFVector64); virtual; abstract;   // Self = Self + V^2
    function InnerSum : double; virtual; abstract;  // result = Sum(Self[i])
    function InnerSumAbs : double; virtual; abstract;  // result = Sum(abs(Self[i]))
    function DotProduct(V : TFVector64) : double; virtual; abstract;  // result = Sum(Self[i] * V[i])

    function MaxValue : double; virtual; abstract;  // result = Max(Self[i])
    function MaxAbsValue : double; virtual; abstract;  // result = Max(abs(Self[i]))
    function IndexMaxValue : integer; virtual; abstract;  // result = index of Max(Self)
    function MinValue : double; virtual; abstract;  // result = Min(Self[i])
    function MinAbsValue : double; virtual; abstract;  // result = Min(abs(Self[i]))
    function IndexMinValue : integer; virtual; abstract;  // result = index of Min(Self)
    function Mean : double; virtual; abstract;  // result = Sum(Self[i]) / NumElements64
    procedure Scale(value : double); virtual; abstract;  // Self = value * Self
    procedure Deg2Rad; virtual; abstract;
    procedure Rad2Deg; virtual; abstract;
    procedure Sin(V : TFVector64); virtual; abstract;
    procedure Cos(V : TFVector64); virtual; abstract;
    procedure Tan(V : TFVector64); virtual; abstract;
    procedure ArcSin(V : TFVector64); virtual; abstract;
    procedure ArcCos(V : TFVector64); virtual; abstract;
    procedure ArcTan(V : TFVector64); virtual; abstract;    
  end;

type
  TX87Vector64 = class(TFVector64)
  public
    constructor Create(const ALength : integer);
    constructor CreateUsingArray(var A:array of double);
    destructor Destroy;override;
    procedure Add(V : TFVector64); override;
    procedure Sub(V : TFVector64); override;
    procedure Mul(V : TFVector64); override;
    procedure Divide(V : TFVector64); override;
    procedure Sqrt(V : TFVector64); override;
    procedure Reciprocal(V : TFVector64); override;
    procedure RecSqrt(V : TFVector64); override;
    procedure Max(V : TFVector64); override;
    procedure Min(V : TFVector64); override;
    procedure LogicalAnd(V : TFVector64); override;  // JPSS FIXED 7/4/2001
    procedure LogicalOr(V : TFVector64); override;   // JPSS FIXED 7/4/2001
    procedure LogicalXor(V : TFVector64); override;  // JPSS FIXED 7/4/2001
    procedure CmpGreater(V : TFVector64); override;  // JPSS FIXED 7/4/2001
    procedure CmpLower(V : TFVector64); override;    // JPSS FIXED 7/4/2001
    procedure CmpEqual(V : TFVector64); override;    // JPSS FIXED 7/4/2001
    procedure CmpGreaterEqual(V : TFVector64); override; // JPSS FIXED 7/4/2001
    procedure CmpLowerEqual(V : TFVector64); override;   // JPSS FIXED 7/4/2001
    procedure AddSquare(V : TFVector64); override;
    function InnerSum : double; override;
    function InnerSumAbs : double; override;
    function DotProduct(V : TFVector64) : double; override;

    function MaxValue : double; override;
    function MaxAbsValue : double; override;
    function IndexMaxValue : integer; override;
    function MinValue : double; override;
    function MinAbsValue : double; override;
    function IndexMinValue : integer; override;
    function Mean : double; override;
    procedure Scale(value : double); override;
    procedure Deg2Rad; override;
    procedure Rad2Deg; override;
    procedure Sin(V : TFVector64); override;
    procedure Cos(V : TFVector64); override;
    procedure Tan(V : TFVector64); override;
    procedure ArcSin(V : TFVector64); override;
    procedure ArcCos(V : TFVector64); override;
    procedure ArcTan(V : TFVector64); override;
  end;

type
  TSSEVector64 = class(TX87Vector64)
  public
    constructor Create(const ALength : integer);
    constructor CreateUsingArray(var A:array of double);                // JPSS  28/02/2001
    destructor Destroy;override;
    procedure Add(V : TFVector64); override;
    procedure Sub(V : TFVector64); override;
    procedure Mul(V : TFVector64); override;
    procedure Divide(V : TFVector64); override;
    procedure Sqrt(V : TFVector64); override;
    procedure Reciprocal(V : TFVector64); override;
    procedure RecSqrt(V : TFVector64); override;
    procedure Max(V : TFVector64); override;
    procedure Min(V : TFVector64); override;
    procedure LogicalAnd(V : TFVector64); override;
    procedure LogicalOr(V : TFVector64); override;
    procedure LogicalXor(V : TFVector64); override;
    procedure CmpGreater(V : TFVector64); override;
    procedure CmpLower(V : TFVector64); override;
    procedure CmpEqual(V : TFVector64); override;
    procedure CmpGreaterEqual(V : TFVector64); override;
    procedure CmpLowerEqual(V : TFVector64); override;
    procedure AddSquare(V : TFVector64); override;
  end;


type
  ETestException = class(Exception);

procedure TestBuildingVectors64;
procedure TestUsingArray64;
procedure TestX8764;  // JPSS 19/3/2001
procedure FullTest64;
//procedure TestBinaryBasicOps( var TestVector1, TestVector2,TestVector:TFVector64);
//procedure TestBinaryComplexOps( var TestVector1, TestVector2,TestVector:TFVector64);


implementation
{$R-}     // JPSS 28/02/2001

const cTestVectorSize = 100;

var
  EnableSSE64 : boolean;

// *************************************************** //
// *                      TFVector64                   * //
// *************************************************** //

constructor TFVector64.Create(const ALength : integer);
begin
  inherited Create;
  AlignedArray64 := nil;
  AllocatedArray64 := nil;
  NumElements64 := 0;
  SetLength(ALength);
end;

constructor TFVector64.CreateUsingArray(var A:array of double);
begin
  inherited Create;
  AlignedArray64 := addr(A);
  AllocatedArray64 := nil;
  NumElements64 := Length(A);
end;

procedure TFVector64.UseArray(var A:array of double); // JPSS  28/02/2001
begin
  if AllocatedArray64 <> nil
     then FreeMem(AllocatedArray64);
  AlignedArray64 := addr(A);
  AllocatedArray64 := nil;
  NumElements64 := Length(A);
end;

constructor TFVector64.Copy(const V: TFVector64);
begin
  inherited Create;
  AlignedArray64 := nil;
  AllocatedArray64 := nil;
  NumElements64 := 0;
  SetLength(V.NumElements64);
  move(V.AlignedArray64, AlignedArray64, NumElements64 * SizeOf(double));
end;

destructor TFVector64.Destroy;
begin
  if AllocatedArray64 <> nil
     then FreeMem(AllocatedArray64);
  AllocatedArray64 := nil;
  inherited Destroy;
end;

class function TFVector64.BuildVector(const ALength : integer) : TFVector64;
begin
  if EnableSSE64
     then result := TSSEVector64.Create(ALength)
     else result := TX87Vector64.Create(ALength);
end;

class function TFVector64.BuildUsingArray(var A:array of double) : TFVector64;
begin
  if EnableSSE64
     then result := TSSEVector64.CreateUsingArray(A)
     else result := TX87Vector64.CreateUsingArray(A);
end;

class function TFVector64.CopyVector(const V : TFVector64) : TFVector64;
begin
  if EnableSSE64
     then result := TSSEVector64.Copy(V)
             else result := TX87Vector64.Copy(V);
end;

class function TFVector64.isSSE64 : boolean;
begin
  result := EnableSSE64;
end;


function TFVector64.GetDataElement(index : integer) : double;
begin
  result := AlignedArray64[index];
end;

procedure TFVector64.SetDataElement(index : integer; const value : double);
begin
  AlignedArray64[index] := value;
end;

procedure TFVector64.SetLength(const ALength : integer);
begin
  if (AllocatedArray64 <> nil) and (ALength = NumElements64)
     then exit;
  if AllocatedArray64 <> nil
     then FreeMem(AllocatedArray64);
  // rounds length on blocks of 4 elements
  if (ALength and 3) > 0
     then NumElements64 := (ALength and ($FFFFFFFF - 3)) + 4
     else NumElements64 := ALength;
  try
    GetMem(AllocatedArray64, (NumElements64 * SizeOf(double)) + 16);
    // AlignedArray64 is aligned on 16 bytes boundary
    AlignedArray64 := PdoubleArray((integer(AllocatedArray64) and $FFFFFFF0) + 16);
  except
    on EOutOfMemory do
       begin
       NumElements64 := 0;
       AllocatedArray64 := nil;
       AlignedArray64 := nil;
       raise;  // re-raise exception to calling function
       end;
  end;
end;

procedure TFVector64.CopyArray(const V : TFVector64);   // JPSS FIXED 7/4/2001
begin
  if NumElements64 <> V.NumElements64
     then SetLength(V.NumElements64);
  move(V.AlignedArray64^, AlignedArray64^, SizeOf(double) * NumElements64);
end;

procedure TFVector64.ImportArray(const Source : PdoubleArray; const Length : integer = 0);
begin
  if (Length > 0) and (Length <> NumElements64)
     then begin
          SetLength(Length);
          move(Source, AlignedArray64, Length * SizeOf(double));
          end
     else move(Source, AlignedArray64, NumElements64 * SizeOf(double));
end;

procedure TFVector64.ExportArray(Destination : PdoubleArray; const Length : integer = 0);
begin
  if Length > 0
     then move(AlignedArray64, Destination, Length * SizeOf(double))
     else move(AlignedArray64, Destination, NumElements64 * SizeOf(double));
end;

procedure TFVector64.SetArray(value : double);
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      AlignedArray64[counter] := value;
end;

procedure TFVector64.ClearArray;
begin
  fillchar(AlignedArray64, NumElements64 * SizeOf(double), 0);
end;

function TFVector64.Equal(V : TFVector64) : boolean;
var counter : integer;
begin
  result := true;
  if NumElements64 <> V.NumElements64
     then begin
          result := false;
          exit;
          end;
  for counter := 0 to (NumElements64 - 1) do
      if Self[counter] <> V[counter]
         then begin
              showmessage(inttostr(counter)+'/'+inttostr(NumElements64));
              result := false;
              break;
              end;
end;

// *************************************************** //
// *                    TSSEVector                   * //
// *************************************************** //

constructor TSSEVector64.Create(const ALength : integer);
begin
  inherited Create(ALength);
end;

constructor TSSEVector64.CreateUsingArray(var A:array of double);
begin
  inherited CreateUsingArray(A);
end;

destructor TSSEVector64.Destroy;
begin
  inherited Destroy;
end;

procedure TSSEVector64.Add(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
  shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeAddLoop
@LargeAddLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$80       /// prefetcht0 [eax+128]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$80       /// prefetcht0 [edx+128]
  db $66,$0F,$58,$C1           /// addpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$58,$D3           /// addpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$58,$E5           /// addpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$58,$F7           /// addpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$00000007
  jz @EndAdd
  shr ecx,1 // number of small iterations = (number of elements modulo 8) / 2
@SmallAddLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$58,$C1           /// addpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
end;

procedure TSSEVector64.Sub(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
  shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeSubLoop
@LargeSubLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$5C,$C1           /// subpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$5C,$D3           /// subpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$5C,$E5           /// subpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$5C,$F7           /// subpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeSubLoop

@SkipLargeSubLoop:
  pop ecx
  and ecx,$00000007
  jz @EndSub
    shr ecx,1 // number of small iterations = (number of elements modulo 8) / 2
@SmallSubLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$5C,$C1           /// subpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallSubLoop

@EndSub:
end;

procedure TSSEVector64.Mul(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
  shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeMulLoop
@LargeMulLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$59,$C1           /// mulpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$59,$D3           /// mulpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$59,$E5           /// mulpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$59,$F7           /// mulpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeMulLoop

@SkipLargeMulLoop:
  pop ecx
  and ecx,$00000007
  jz @EndMul
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallMulLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$59,$C1           /// mulpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallMulLoop

@EndMul:
end;

procedure TSSEVector64.Divide(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
  shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeDivLoop
@LargeDivLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$5E,$C1           /// divpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$5E,$D3           /// divpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$5E,$E5           /// divpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$5E,$F7           /// divpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeDivLoop

@SkipLargeDivLoop:
  pop ecx
  and ecx,$00000007
  jz @EndDiv
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallDivLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$5E,$C1           /// divpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallDivLoop

@EndDiv:
end;

procedure TSSEVector64.Sqrt(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
  shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeSqrtLoop
@LargeSqrtLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$51,$C1           /// sqrtpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$51,$D3           /// sqrtpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$51,$E5           /// sqrtpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$51,$F7           /// sqrtpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeSqrtLoop

@SkipLargeSqrtLoop:
  pop ecx
  and ecx,$00000007
  jz @EndSqrt
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallSqrtLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$51,$C1           /// sqrtpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallSqrtLoop

@EndSqrt:
end;

procedure TSSEVector64.Reciprocal(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
  shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeReciprocalLoop
@LargeReciprocalLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$53,$C1           /// rcppd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$53,$D3           /// rcppd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$53,$E5           /// rcppd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$53,$F7           /// rcppd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeReciprocalLoop

@SkipLargeReciprocalLoop:
  pop ecx
  and ecx,$00000007
  jz @EndReciprocal
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallReciprocalLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$53,$C1           /// rcppd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallReciprocalLoop

@EndReciprocal:
end;

procedure TSSEVector64.RecSqrt(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeRSqrtLoop
@LargeRSqrtLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$52,$C1           /// rsqrtpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$52,$D3           /// rsqrtpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$52,$E5           /// rsqrtpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$52,$F7           /// rsqrtpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeRSqrtLoop

@SkipLargeRSqrtLoop:
  pop ecx
  and ecx,$00000007
  jz @EndRSqrt
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallRSqrtLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$52,$C1           /// rsqrtpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallRSqrtLoop

@EndRSqrt:
end;

procedure TSSEVector64.Max(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeMaxLoop
@LargeMaxLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$5F,$C1           /// maxpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$5F,$D3           /// maxpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$5F,$E5           /// maxpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$5F,$F7           /// maxpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeMaxLoop

@SkipLargeMaxLoop:
  pop ecx
  and ecx,$00000007
  jz @EndMax
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallMaxLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$5F,$C1           /// maxpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallMaxLoop

@EndMax:
end;

procedure TSSEVector64.Min(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeMinLoop
@LargeMinLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$5D,$C1           /// minpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$5D,$D3           /// minpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$5D,$E5           /// minpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$5D,$F7           /// minpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeMinLoop

@SkipLargeMinLoop:
  pop ecx
  and ecx,$00000007
  jz @EndMin
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallMinLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$5D,$C1           /// minpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallMinLoop

@EndMin:
end;

procedure TSSEVector64.LogicalAnd(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeLogicalAndLoop
@LargeLogicalAndLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$54,$C1           /// andpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$54,$D3           /// andpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$54,$E5           /// andpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$54,$F7           /// andpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeLogicalAndLoop

@SkipLargeLogicalAndLoop:
  pop ecx
  and ecx,$00000007
  jz @EndLogicalAnd
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallLogicalAndLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$54,$C1           /// andpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16 //{16}8
  add edx,16 //16
  dec ecx
  jnz @SmallLogicalAndLoop

@EndLogicalAnd:
end;

procedure TSSEVector64.LogicalOr(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeLogicalOrLoop
@LargeLogicalOrLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$56,$C1           /// orpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$56,$D3           /// orpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$56,$E5           /// orpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$56,$F7           /// orpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeLogicalOrLoop

@SkipLargeLogicalOrLoop:
  pop ecx
  and ecx,$00000007
  jz @EndLogicalOr
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallLogicalOrLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$56,$C1           /// orpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallLogicalOrLoop

@EndLogicalOr:
end;

procedure TSSEVector64.LogicalXor(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeLogicalXorLoop
@LargeLogicalXorLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$57,$C1           /// xorpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$57,$D3           /// xorpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$57,$E5           /// xorpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$57,$F7           /// xorpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeLogicalXorLoop

@SkipLargeLogicalXorLoop:
  pop ecx
  and ecx,$00000007
  jz @EndLogicalXor
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallLogicalXorLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$57,$C1           /// xorpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallLogicalXorLoop

@EndLogicalXor:
end;

procedure TSSEVector64.CmpGreater(V : TFVector64);
// Self > V becomes V < Self
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeCmpGreaterLoop
@LargeCmpGreaterLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$C2,$C8,$01       /// cmpltpd xmm1,xmm0
  db $66,$0F,$29,$08           /// movapd [eax],xmm1

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$C2,$DA,$01       /// cmpltpd xmm3,xmm2
  db $66,$0F,$29,$58,$10       /// movapd [eax+16],xmm3

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$C2,$EC,$01       /// cmpltpd xmm5,xmm4
  db $66,$0F,$29,$68,$20       /// movapd [eax+32],xmm5

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$C2,$FE,$01       /// cmpltpd xmm7,xmm6
  db $66,$0F,$29,$78,$30       /// movapd [eax+48],xmm7

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpGreaterLoop

@SkipLargeCmpGreaterLoop:
  pop ecx
  and ecx,$00000007
  jz @EndCmpGreater
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallCmpGreaterLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$C2,$C8,$01       /// cmpltpd xmm1,xmm0
  db $66,$0F,$29,$08           /// movapd [eax],xmm1
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpGreaterLoop

@EndCmpGreater:
end;

procedure TSSEVector64.CmpLower(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeCmpLowerLoop
@LargeCmpLowerLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$C2,$C1,$01       /// cmpltpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$C2,$D3,$01       /// cmpltpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$C2,$E5,$01       /// cmpltpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$C2,$F7,$01       /// cmpltpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpLowerLoop

@SkipLargeCmpLowerLoop:
  pop ecx
  and ecx,$00000007
  jz @EndCmpLower
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallCmpLowerLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$C2,$C1,$01       /// cmpltpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpLowerLoop

@EndCmpLower:
end;

procedure TSSEVector64.CmpEqual(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeCmpEqualLoop
@LargeCmpEqualLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$C2,$C1,$00       /// cmpeqpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$C2,$D3,$00       /// cmpeqpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$C2,$E5,$00       /// cmpeqpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$C2,$F7,$00       /// cmpeqpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpEqualLoop

@SkipLargeCmpEqualLoop:
  pop ecx
  and ecx,$00000007
  jz @EndCmpEqual
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallCmpEqualLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$C2,$C1,$00       /// cmpeqpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpEqualLoop

@EndCmpEqual:
end;

procedure TSSEVector64.CmpGreaterEqual(V : TFVector64);
// A >= B becomes B <= A
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeCmpGreaterEqualLoop
@LargeCmpGreaterEqualLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$C2,$C8,$02       /// cmplepd xmm1,xmm0
  db $66,$0F,$29,$08           /// movapd [eax],xmm1

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$C2,$DA,$02       /// cmplepd xmm3,xmm2
  db $66,$0F,$29,$58,$10       /// movapd [eax+16],xmm3

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$C2,$EC,$02       /// cmplepd xmm5,xmm4
  db $66,$0F,$29,$68,$20       /// movapd [eax+32],xmm5

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$C2,$FE,$02       /// cmplepd xmm7,xmm6
  db $66,$0F,$29,$78,$30       /// movapd [eax+48],xmm7

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpGreaterEqualLoop

@SkipLargeCmpGreaterEqualLoop:
  pop ecx
  and ecx,$00000007
  jz @EndCmpGreaterEqual
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallCmpGreaterEqualLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$C2,$C8,$02       /// cmplepd xmm1,xmm0
  db $66,$0F,$29,$08           /// movapd [eax],xmm1
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpGreaterEqualLoop

@EndCmpGreaterEqual:
end;

procedure TSSEVector64.CmpLowerEqual(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeCmpLowerEqualLoop
@LargeCmpLowerEqualLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$C2,$C1,$02       /// cmplepd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$C2,$D3,$02       /// cmplepd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$C2,$E5,$02       /// cmplepd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$C2,$F7,$02       /// cmplepd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeCmpLowerEqualLoop

@SkipLargeCmpLowerEqualLoop:
  pop ecx
  and ecx,$00000007
  jz @EndCmpLowerEqual
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallCmpLowerEqualLoop:
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$C2,$C1,$02       /// cmplepd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallCmpLowerEqualLoop

@EndCmpLowerEqual:
end;

procedure TSSEVector64.AddSquare(V : TFVector64);
asm
  mov ecx,[eax + NumElements64]
  mov eax,[eax + AlignedArray64]
  mov edx,[V.AlignedArray64]
  push ecx
shr ecx,3  // number of large iterations = number of elements / 8 TOW
  jz @SkipLargeAddSquareLoop
@LargeAddSquareLoop:
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $0F,$18,$4A,$40       /// prefetcht0 [edx+64]
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $0F,$18,$48,$40       /// prefetcht0 [eax+64]
  db $66,$0F,$59,$C9           /// mulpd xmm1,xmm1
  db $66,$0F,$58,$C1           /// addpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0

  db $66,$0F,$28,$5A,$10       /// movapd xmm3,[edx+16]
  db $66,$0F,$28,$50,$10       /// movapd xmm2,[eax+16]
  db $66,$0F,$59,$DB           /// mulpd xmm3,xmm3
  db $66,$0F,$58,$D3           /// addpd xmm2,xmm3
  db $66,$0F,$29,$50,$10       /// movapd [eax+16],xmm2

  db $66,$0F,$28,$6A,$20       /// movapd xmm5,[edx+32]
  db $66,$0F,$28,$60,$20       /// movapd xmm4,[eax+32]
  db $66,$0F,$59,$ED           /// mulpd xmm5,xmm5
  db $66,$0F,$58,$E5           /// addpd xmm4,xmm5
  db $66,$0F,$29,$60,$20       /// movapd [eax+32],xmm4

  db $66,$0F,$28,$7A,$30       /// movapd xmm7,[edx+48]
  db $66,$0F,$28,$70,$30       /// movapd xmm6,[eax+48]
  db $66,$0F,$59,$FF           /// mulpd xmm7,xmm7
  db $66,$0F,$58,$F7           /// addpd xmm6,xmm7
  db $66,$0F,$29,$70,$30       /// movapd [eax+48],xmm6

  add eax,64
  add edx,64
  dec ecx
  jnz @LargeAddSquareLoop

@SkipLargeAddSquareLoop:
  pop ecx
  and ecx,$00000007
  jz @EndAddSquare
  shr ecx,1 // number of small iterations = (number of elements modulo 16) / 2
@SmallAddSquareLoop:
  db $66,$0F,$28,$0A           /// movapd xmm1,[edx]
  db $66,$0F,$28,$00           /// movapd xmm0,[eax]
  db $66,$0F,$59,$C9           /// mulpd xmm1,xmm1
  db $66,$0F,$58,$C1           /// addpd xmm0,xmm1
  db $66,$0F,$29,$00           /// movapd [eax],xmm0
  add eax,16
  add edx,16
  dec ecx
  jnz @SmallAddSquareLoop

@EndAddSquare:
end;

// *************************************************** //
// *                    TX87Vector                   * //
// *************************************************** //

constructor TX87Vector64.Create(const ALength : integer);
begin
  inherited Create(ALength);
end;

constructor TX87Vector64.CreateUsingArray(var A:array of double);
begin
  inherited CreateUsingArray(A);
end;

destructor TX87Vector64.Destroy;
begin
  inherited Destroy;
end;

procedure TX87Vector64.Add(V : TFVector64);
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      AlignedArray64[counter] := AlignedArray64[counter] + V.AlignedArray64[counter];
end;

procedure TX87Vector64.Sub(V : TFVector64);
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      AlignedArray64[counter] := AlignedArray64[counter] - V.AlignedArray64[counter];
end;

procedure TX87Vector64.Mul(V : TFVector64);
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      AlignedArray64[counter] := AlignedArray64[counter] * V.AlignedArray64[counter];
end;

procedure TX87Vector64.Divide(V : TFVector64);
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      AlignedArray64[counter] := AlignedArray64[counter] / V.AlignedArray64[counter];
end;

procedure TX87Vector64.Sqrt(V : TFVector64);
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      AlignedArray64[counter] := System.Sqrt(V.AlignedArray64[counter]);
end;

procedure TX87Vector64.Reciprocal(V : TFVector64);
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      AlignedArray64[counter] := 1 / V.AlignedArray64[counter];
end;

procedure TX87Vector64.RecSqrt(V : TFVector64);
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      AlignedArray64[counter] := 1 / System.Sqrt(V.AlignedArray64[counter]);
end;

procedure TX87Vector64.Max(V : TFVector64);
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      if V.AlignedArray64[counter] > AlignedArray64[counter]
         then AlignedArray64[counter] := V.AlignedArray64[counter];
end;

procedure TX87Vector64.Min(V : TFVector64);
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      if V.AlignedArray64[counter] < AlignedArray64[counter]
         then AlignedArray64[counter] := V.AlignedArray64[counter];
end;

procedure TX87Vector64.LogicalAnd(V : TFVector64);       // JPSS FIXED 7/4/2001
(*{$IFDEF DELPHI_CODE}
*)var counter : integer;
    AuxP:pointer;
    DestVec,SrcVec: PInt64Array;
begin
  AuxP:=AlignedArray64; DestVec:=AuxP;
  AuxP:=V.AlignedArray64; SrcVec:=AuxP;
  for counter := 0 to (NumElements64 - 1) do
      DestVec^[counter] := DestVec^[counter] and SrcVec^[counter];
end;
(*{$ELSE}
asm
   mov ecx,[eax + NumElements64]
   mov edx,[V.AlignedArray64]
   mov eax,[eax + AlignedArray64]
 @AndLoop:
   mov ebx, [edx]
   and [eax], ebx
   add edx,4
   add eax,4
   dec ecx
   jnz @AndLoop
end;
{$ENDIF}
  *)
procedure TX87Vector64.LogicalOr(V : TFVector64);        // JPSS FIXED 7/4/2001
//{$IFDEF DELPHI_CODE}
var counter : integer;
    AuxP:pointer;
    DestVec,SrcVec: PInt64Array;
begin
  AuxP:=AlignedArray64; DestVec:=AuxP;
  AuxP:=V.AlignedArray64; SrcVec:=AuxP;
  for counter := 0 to (NumElements64 - 1) do
      DestVec^[counter] := DestVec^[counter] or SrcVec^[counter];
end;
(*{$ELSE}
asm
   mov ecx,[eax + NumElements64]
   mov edx,[V.AlignedArray64]
   mov eax,[eax + AlignedArray64]
 @AndLoop:
   mov ebx, [edx]
   or [eax], ebx
   add edx,4
   add eax,4
   dec ecx
   jnz @AndLoop
end;
{$ENDIF}
  *)
procedure TX87Vector64.LogicalXor(V : TFVector64);         // JPSS FIXED 7/4/2001
//{$IFDEF DELPHI_CODE}
var counter : integer;
    AuxP:pointer;
    DestVec,SrcVec: PInt64Array;
begin
  AuxP:=AlignedArray64; DestVec:=AuxP;
  AuxP:=V.AlignedArray64; SrcVec:=AuxP;
  for counter := 0 to (NumElements64 - 1) do
      DestVec^[counter] := DestVec^[counter] xor SrcVec^[counter];
end;
(*{$ELSE}
asm
   mov ecx,[eax + NumElements64]
   mov edx,[V.AlignedArray64]
   mov eax,[eax + AlignedArray64]
 @AndLoop:
   mov ebx, [edx]
   xor [eax], ebx
   add edx,4
   add eax,4
   dec ecx
   jnz @AndLoop
end;
{$ENDIF}
  *)
procedure TX87Vector64.CmpGreater(V : TFVector64);
var counter : integer;
    AuxP:pointer;
    DestVec: PInt64Array;
begin
  AuxP:=AlignedArray64; DestVec:=AuxP;
  for counter := 0 to (NumElements64 - 1) do
      if AlignedArray64[counter] > V.AlignedArray64[counter]
         then DestVec[counter] := $FFFFFFFF
         else DestVec[counter] := 0;
end;

procedure TX87Vector64.CmpLower(V : TFVector64);
var counter : integer;
    AuxP:pointer;
    DestVec: PInt64Array;
begin
  AuxP:=AlignedArray64; DestVec:=AuxP;
  for counter := 0 to (NumElements64 - 1) do
      if AlignedArray64[counter] < V.AlignedArray64[counter]
         then DestVec[counter] := $FFFFFFFF
         else DestVec[counter] := 0;
end;

procedure TX87Vector64.CmpEqual(V : TFVector64);
var counter : integer;
    AuxP:pointer;
    DestVec: PInt64Array;
begin
  AuxP:=AlignedArray64; DestVec:=AuxP;
  for counter := 0 to (NumElements64 - 1) do
      if AlignedArray64[counter] = V.AlignedArray64[counter]
         then DestVec[counter] := $FFFFFFFF
         else DestVec[counter] := 0;
end;

procedure TX87Vector64.CmpGreaterEqual(V : TFVector64);
var counter : integer;
    AuxP:pointer;
    DestVec: PInt64Array;
begin
  AuxP:=AlignedArray64; DestVec:=AuxP;
  for counter := 0 to (NumElements64 - 1) do
      if AlignedArray64[counter] >= V.AlignedArray64[counter]
         then DestVec[counter] := $FFFFFFFF
         else DestVec[counter] := 0;
end;

procedure TX87Vector64.CmpLowerEqual(V : TFVector64);
var counter : integer;
    AuxP:pointer;
    DestVec: PInt64Array;
begin
  AuxP:=AlignedArray64; DestVec:=AuxP;
  for counter := 0 to (NumElements64 - 1) do
      if AlignedArray64[counter] <= V.AlignedArray64[counter]
         then DestVec[counter] := $FFFFFFFF
         else DestVec[counter] := 0;
end;

procedure TX87Vector64.AddSquare(V : TFVector64);
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      AlignedArray64[counter] := AlignedArray64[counter] + Sqr(V.AlignedArray64[counter]);
end;

function TX87Vector64.InnerSum : double;
{$IFDEF DELPHI_CODE}
var counter : integer;
begin
  result := 0;
  for counter := 0 to (NumElements64 - 1) do
      result := result + AlignedArray64[counter];
end;
{$ELSE}
asm
    mov ecx,[eax + NumElements64]
    mov eax,[eax + AlignedArray64]
    fldz
  @InnerSumLoop:
    fadd QWORD PTR [eax]
    add eax,8
    dec ecx
    jnz @InnerSumLoop
end;
{$ENDIF}

function TX87Vector64.InnerSumAbs : double;
{$IFDEF DELPHI_CODE}
var counter : integer;
begin
  result := 0;
  for counter := 0 to (NumElements64 - 1) do
      result := result + Abs(AlignedArray64[counter]);
end;
{$ELSE}
asm
    mov ecx,[eax + NumElements64]
    mov eax,[eax + AlignedArray64]
    fldz
  @InnerSumLoop:
    fld QWORD PTR [eax]
    fabs
    faddp ST(1),ST(0)
    add eax,8
    dec ecx
    jnz @InnerSumLoop
end;
{$ENDIF}

function TX87Vector64.DotProduct(V : TFVector64) : double;
{$IFDEF DELPHI_CODE}
var counter : integer;
begin
  result := 0;
  for counter := 0 to (NumElements64 - 1) do
      result := result + (AlignedArray64[counter] * V.AlignedArray64[counter]);
end;
{$ELSE}
asm
   mov ecx,[eax + NumElements64]
   mov edx,[V.AlignedArray64]
   mov eax,[eax + AlignedArray64]
   fldz
 @DotProductLoop:
   fld QWORD PTR [eax]
   fmul QWORD PTR [edx]
   add eax,8
   add edx,8
   faddp ST(1),ST(0)
   dec ecx
   jnz @DotProductLoop
end;
{$ENDIF}
function TX87Vector64.MaxValue : double;
var counter : integer;
begin
  result := AlignedArray64[0];
  for counter := 1 to (NumElements64 - 1) do
      if AlignedArray64[counter] > result
         then result := AlignedArray64[counter];
end;

function TX87Vector64.MinValue : double;
var counter : integer;
begin
  result := AlignedArray64[0];
  for counter := 1 to (NumElements64 - 1) do
      if AlignedArray64[counter] < result
         then result := AlignedArray64[counter];
end;

function TX87Vector64.MaxAbsValue : double;
var counter : integer;
begin
  result := Abs(AlignedArray64[0]);
  for counter := 1 to (NumElements64 - 1) do
      if Abs(AlignedArray64[counter]) > result
         then result := Abs(AlignedArray64[counter]);
end;

function TX87Vector64.MinAbsValue : double;
var counter : integer;
begin
  result := Abs(AlignedArray64[0]);
  for counter := 1 to (NumElements64 - 1) do
      if Abs(AlignedArray64[counter]) < result
         then result := Abs(AlignedArray64[counter]);
end;

function TX87Vector64.IndexMaxValue : integer;
var counter : integer;
    CurrentMaxValue : double;
begin
  result := 0;
  CurrentMaxValue := AlignedArray64[0];
  for counter := 1 to (NumElements64 - 1) do
      if AlignedArray64[counter] > CurrentMaxValue
         then begin
              CurrentMaxValue := AlignedArray64[counter];
              result := counter;
              end;
end;

function TX87Vector64.IndexMinValue : integer;
var counter : integer;
    CurrentMinValue : double;
begin
  result := 0;
  CurrentMinValue := AlignedArray64[0];
  for counter := 1 to (NumElements64 - 1) do
      if AlignedArray64[counter] < CurrentMinValue
         then begin
              CurrentMinValue := AlignedArray64[counter];
              result := counter;
              end;
end;

function TX87Vector64.Mean : double;
{$IFDEF DELPHI_CODE}
var counter : integer;
begin
  result := 0;
  for counter := 0 to (NumElements64 - 1) do
      result := result + AlignedArray64[counter];
  result := result / NumElements64;
end;
{$ELSE}
asm
  mov ecx,[eax + NumElements64]
  mov edx,[eax + AlignedArray64]
  fldz
 @MeanLoop:
  fadd QWORD PTR [edx]
  add edx,8
  dec ecx
  jnz @MeanLoop
  fild DWORD PTR [eax + NumElements64]
  fdivp
end;
{$ENDIF}

procedure TX87Vector64.Scale(value : double);
{$IFDEF DELPHI_CODE}
var counter : integer;
begin
  for counter := 0 to (NumElements64 - 1) do
      AlignedArray64[counter] := AlignedArray64[counter] * value;
end;
{$ELSE}
asm
   mov ecx,[eax + NumElements64]
   mov edx,[eax + AlignedArray64]
   fld QWORD PTR [ebp + 8]  // load value
 @ScaleLoop:
   fld QWORD PTR [edx]
   fmul ST(0),ST(1)
   fstp QWORD PTR [edx]
   add edx,8
   dec ecx
   jnz @ScaleLoop
end;
{$ENDIF}

procedure TX87Vector64.Deg2Rad;
begin
  Scale(Pi/180);
end;

procedure TX87Vector64.Rad2Deg;
begin
  Scale(180/Pi);
end;

procedure TX87Vector64.Sin(V : TFVector64);
asm
   mov ecx,[eax + NumElements64]
   mov eax,[eax + AlignedArray64]
   mov edx,[V.AlignedArray64]

 @SinLoop:
   fld  qword ptr [edx]
   fsin
   fstp qword ptr [eax]
   add  edx,8
   add  eax,8
   dec  ecx
   jnz  @SinLoop
end;

procedure TX87Vector64.Cos(V : TFVector64);
asm
   mov ecx,[eax + NumElements64]
   mov eax,[eax + AlignedArray64]
   mov edx,[V.AlignedArray64]

 @CosLoop:
   fld  qword ptr [edx]
   fcos
   fstp qword ptr [eax]
   add  edx,8
   add  eax,8
   dec  ecx
   jnz  @CosLoop
end;

procedure TX87Vector64.Tan(V : TFVector64);
asm
   mov ecx,[eax + NumElements64]
   mov eax,[eax + AlignedArray64]
   mov edx,[V.AlignedArray64]

 @TanLoop:
   fld  qword ptr [edx]
   fptan                     // Tan(V^[]) -> ST(1); 1.0 -> ST(0)
   fstp st(0)                // flush ST(0)
   fstp qword ptr [eax]      // ST(0) -> AlignedArray64^[]
   add  edx,8
   add  eax,8
   dec  ecx
   jnz  @TanLoop
end;

procedure TX87Vector64.ArcSin(V : TFVector64);
asm
   mov ecx,[eax + NumElements64]
   mov eax,[eax + AlignedArray64]
   mov edx,[V.AlignedArray64]

@ArcSinLoop:
   fld1                      // 1.0 -> ST(0)
   fld  qword ptr [edx]      // V^[] -> ST(0); 1.0 -> ST(1)
   fst  st(2)                // copy V^[] -> ST(2)
   fmul st(0),st(0)          // Sqr(V^[]) -> ST(0); 1.0 -> ST(1); V^[] -> ST(2)
   fsubp                     // 1.0 - Sqr(V^[]) -> ST(0); V^[] -> ST(1)
   fsqrt                     // Sqrt(1.0 - Sqr(V^[])) -> ST(0); V^[] -> ST(1)
   fpatan                    // ArcTan(V^[]/Sqrt(1.0 - Sqr(V^[]))) -> ST(0)
   fstp qword ptr [eax]      // ST(0) -> AlignedArray64^[]
   add  edx,8
   add  eax,8
   dec  ecx
   jnz  @ArcSinLoop
end;

procedure TX87Vector64.ArcCos(V : TFVector64);
asm
   mov ecx,[eax + NumElements64]
   mov eax,[eax + AlignedArray64]
   mov edx,[V.AlignedArray64]

 @ArcCosLoop:
   fld1                      // 1.0 -> ST(0)
   fld  qword ptr [edx]      // V^[] -> ST(0); 1.0 -> ST(1)
   fst  st(2)                // copy V^[] -> ST(2)
   fmul st(0),st(0)          // Sqr(V^[]) -> ST(0); 1.0 -> ST(1); V^[] -> ST(2)
   fsubp                     // 1.0 - Sqr(V^[]) -> ST(0); V^[] -> ST(1)
   fsqrt                     // Sqrt(1.0 - Sqr(V^[])) -> ST(0); V^[] -> ST(1)
   fxch                      // V^[] -> ST(0); Sqrt(1.0 - Sqr(V^[])) -> ST(1)
   fpatan                    // ArcTan(Sqrt(1.0 - Sqr(V^[]))/ V^[]) -> ST(0)
   fstp qword ptr [eax]      // ST(0) -> AlignedArray64^[]
   add  edx,8
   add  eax,8
   dec  ecx
   jnz  @ArcCosLoop
end;

procedure TX87Vector64.ArcTan(V : TFVector64);
// Self := ArcTan(V) returns range -Pi/2 ~ Pi/2
asm
   mov ecx,[eax + NumElements64]
   mov eax,[eax + AlignedArray64]
   mov edx,[V.AlignedArray64]
 @ArcTanLoop:
   fld  qword ptr [edx]      // V^[] -> ST(0)
   fld1                      // 1.0 -> ST(0)
   fpatan                    // ArcTan(V^[]) -> ST(0)
   fstp qword ptr [eax]      // ST(0) -> AlignedArray64^[]
   add  edx,8
   add  eax,8
   dec  ecx
   jnz  @ArcTanLoop
end;


// testing functions
procedure SyncVectors(var V1,V2 : TFVector64); // modification JPSS 6/4/2001
begin
V1.CopyArray(V2);             // JPSS 6/4/2001
end;

procedure Riteln(lStr: String); //added by CR: allows GUI use
begin
    {$IFDEF console}
    writeln('Exentia64: '+lStr);
    {$ELSE}
    Showmessage('Exentia64: '+lStr);
    {$ENDIF}
end;

procedure TestLogicalOps( var TestVector1, TestVector2,TestVector:TFVector64; lSSE: boolean); // JPSS 14/3/2001
var    counter : integer;
       AuxP:pointer;
       P1,P2,P:PInt64Array;
       lStr: string;
begin
  if lSSE then
     lStr := 'SSE64 '
  else
      lStr := '64bitNotSSE ';
  AuxP:=TestVector1.AlignedArray64; P1:=AuxP;
  AuxP:=TestVector2.AlignedArray64; P2:=AuxP;
  AuxP:=TestVector.AlignedArray64;  P:=AuxP;
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
     then riteln(lStr+'ERROR: And')
     else riteln(lStr+'And worked');
  SyncVectors(TestVector, TestVector1);

  // test OR
  for counter := 0 to (cTestVectorSize - 1) do
      P^[counter] := P^[counter] or P2^[counter];

  TestVector1.LogicalOr(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln(lStr+'ERROR: Or')
     else riteln(lStr+'Or worked!');
  SyncVectors(TestVector, TestVector1);

  // test XOR
  for counter := 0 to (cTestVectorSize - 1) do
      P^[counter] := P^[counter] xor P2^[counter];

  TestVector1.LogicalXOr(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln(lStr+'ERROR: XOr')
     else riteln(lStr+'XOr worked!');
  SyncVectors(TestVector, TestVector1);
end;

procedure TestBinaryBasicOps( var TestVector1, TestVector2,TestVector:TFVector64; lSSE: boolean);
var    counter : integer;
       lStr: string;
begin
  if lSSE then
     lStr := 'SSE64 '
  else
      lStr := '64bitNotSSE ';
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
     then riteln(lStr+'ERROR: Add');
  SyncVectors(TestVector, TestVector1);

  // test SUB
  for counter := 0 to (cTestVectorSize - 1) do
      TestVector[counter] := TestVector[counter] - TestVector2[counter];
  TestVector1.Sub(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln(lStr+'ERROR: Sub');
  SyncVectors(TestVector, TestVector1);

  // test MUL
  for counter := 0 to (cTestVectorSize - 1) do
      TestVector[counter] := TestVector[counter] * TestVector2[counter];
  TestVector1.Mul(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln(lStr+'ERROR: Mul');
  SyncVectors(TestVector, TestVector1);

  // test DIV
  for counter := 0 to (cTestVectorSize - 1) do
      TestVector[counter] := TestVector[counter] / TestVector2[counter];
  TestVector1.Divide(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then riteln(lStr+'ERROR: Divide');

end;

procedure TestBinaryComplexOps( var TestVector1, TestVector2,TestVector:TFVector64; lSSE: boolean);
var counter : integer;
    res : double;
    otherres : double;
    lStr,lStr1,lStr2,lStr3: string;
procedure ShowERROR(S:string); // JPSS 6/4/2001
    begin
     riteln(lStr+S);
     Str((res-otherres):12:5,lStr1);
     Str((res):12:5,lStr2);
     Str((otherres):12:5,lStr3);
     riteln(' ERROR = '+lStr1+'   Exentia:'+lStr2+' Pascal:'+lStr3);
    end;
begin
  if lSSE then
     lStr := 'SSE64 '
  else
      lStr := '64bitNotSSE ';

  riteln('Testing complex ops');
  Randomize;
  for counter := 0 to (cTestVectorSize - 1) do
      begin
      TestVector1[counter] := (random-0.5) * 1000;   // JPSS modification 6/4/2001
      if TestVector1[counter] = 0
        then TestVector1[counter] := 1.0;
      TestVector[counter] := TestVector1[counter];
      TestVector2[counter] := (random-0.5) * 1000;  // JPSS modification 6/4/2001
      if TestVector2[counter] = 0
        then TestVector2[counter] := 1.0;
      end;

  // test INNER SUM
  res := 0;
  for counter := 0 to (cTestVectorSize - 1) do
      res := res + TestVector1.Data[counter];
  otherres := TestVector1.InnerSum;
  // Note: these results are different (due to rounding ERROR/s?)
  if res <> otherres
     then ShowERROR('[rounding?] ERROR: InnerSum:'); // JPSS modification 6/4/2001

  // test INNER ABS SUM
  res := 0;
  for counter := 0 to (cTestVectorSize - 1) do
      res := res + Abs(TestVector1.Data[counter]);
  otherres := TestVector1.InnerSumAbs;
  // Note: these results are different (due to rounding ERRORs?)
  if res <> otherres
     then ShowERROR('[rounding?] ERROR: InnerSumAbs:'); // JPSS modification 6/4/2001
  // test ADD + SQUARE
  for counter := 0 to (cTestVectorSize - 1) do
      TestVector[counter] := TestVector[counter] + TestVector2[counter] * TestVector2[counter];
  TestVector1.AddSquare(TestVector2);
  if not(TestVector1.Equal(TestVector))
     then ShowERROR('[rounding?] ERROR: Add Square'); // JPSS modification 6/4/2001
  SyncVectors(TestVector, TestVector1);

end;

procedure FullTest64;
begin
  TestX8764;
  riteln('###Next: Vectors Press Enter ###############');
  {$IFDEF console} Readln;{$ENDIF}
  TestBuildingVectors64;
  riteln('###Next: TestArray Press Enter ###############');
  {$IFDEF console} Readln;{$ENDIF}
  TestUsingArray64;
end;

procedure TestBuildingVectors64;
var TestVector1, TestVector2,TestVector:TFVector64;
begin
  riteln(' Testing Building Vectors');
  riteln('-----------------------------------');
  TestVector1 := TFVector64.BuildVector(cTestVectorSize);
  TestVector2 := TFVector64.BuildVector(cTestVectorSize);
  TestVector  := TFVector64.BuildVector(cTestVectorSize);
  TestLogicalOps( TestVector1, TestVector2, TestVector,TestVector1.isSSE64 );
  TestBinaryBasicOps( TestVector1, TestVector2, TestVector,TestVector1.isSSE64 );
  TestBinaryComplexOps( TestVector1, TestVector2, TestVector,TestVector1.isSSE64 );
  TestVector1.Free;
  TestVector2.Free;
  TestVector.Free;
end;

procedure TestX8764;  // JPSS 19/3/2001
var TestVector1, TestVector2,TestVector:TFVector64;
begin
  riteln(' Testing X87');
  riteln('-----------------------------------');
  TestVector1 := TX87Vector64.Create(cTestVectorSize);
  TestVector2 := TX87Vector64.Create(cTestVectorSize);
  TestVector  := TX87Vector64.Create(cTestVectorSize);
  TestLogicalOps( TestVector1, TestVector2, TestVector,false );
  TestBinaryBasicOps( TestVector1, TestVector2, TestVector ,false);
  TestBinaryComplexOps( TestVector1, TestVector2, TestVector,false );
  TestVector1.Free;
  TestVector2.Free;
  TestVector.Free;
end;

procedure TestUsingArray64;
var TestVector1, TestVector2,TestVector:TFVector64;
    A1,A2,A: array[0..cTestVectorSize-1] of double;
begin
  riteln(' Testing Using Arrays');
  riteln('-----------------------------------');
  TestVector1 := TFVector64.BuildUsingArray(A1);
  TestVector2 := TFVector64.BuildUsingArray(A2);
  TestVector  := TFVector64.BuildUsingArray(A);
  TestLogicalOps( TestVector1, TestVector2, TestVector,TestVector1.isSSE64 );
  TestBinaryBasicOps( TestVector1, TestVector2, TestVector ,TestVector1.isSSE64);
  TestBinaryComplexOps( TestVector1, TestVector2, TestVector,TestVector1.isSSE64 );
  TestVector1.Free;
  TestVector2.Free;
  TestVector.Free;
end;

initialization
  try
    EnableSSE64 := false;
    asm
      mov eax, 1
      db $0F,$A2 /// CPUID
      test edx,(1 shl 26)
      jnz @SSE2Found
      mov EnableSSE64,0
      jmp @END_SSE
    @SSE2Found:
      mov EnableSSE64,1
    @END_SSE:
    end;
  except
    EnableSSE64 := false;
  end;
end.

