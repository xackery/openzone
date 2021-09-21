// *****************************************************************************
//                UPExentia Library for Vectorial Computation
//                    by Joao Paulo Schwarz Schuler 2001
//                              version 0.1.9
//      This Library uses the Exentia Library by Stefano Tommesani 2000-2001 - http://www.tommesani.com/Exentia.html
//                  Web: http://www.schulers.com/jpss
//                       E-mail: jpss@schulers.com
//
// The contents of this file are subject to the Mozilla Public License Version
// 1.0; you may not use this file except in compliance with the License.
// You may obtain a copy of the License at http://www.mozilla.org/MPL/
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// for the specific language governing rights and limitations under the License.
// *****************************************************************************

// You can use the SSVO object if you DO NOT HAVE THREADs. If you have
// many THREADs you should allocate 1 TSSVO object for each THREAD.

// Exemple:
// var A1,A2: array[0..9] of Single;
// begin
// ...
// SSVO.ADD(A1,A2); // A:=A+B or A[0..9]:=A[0..9]+B[0..9] or A[I]:=A[I]+B[I]

unit UPExentia;

interface

uses Exentia;
type TSSVO = class (TObject) // T Simple Single Vector Operations
      private
        AR:array[0..0] of Single; // useless array
        A,B:TFVector;
      public
        constructor Create;
        destructor Destroy;override;
        procedure Add(var X,Y:array of Single);
        procedure Sub(var X,Y:array of Single);
        procedure Mul(var X,Y:array of Single);
        procedure Divide(var X,Y:array of Single);
        procedure Sqrt(var X,Y:array of Single);
        procedure Reciprocal(var X,Y:array of Single);
        procedure RecSqrt(var X,Y:array of Single);
        procedure Max(var X,Y:array of Single);
        procedure Min(var X,Y:array of Single);
        procedure LogicalAnd(var X,Y:array of Single);
        procedure LogicalOr(var X,Y:array of Single);
        procedure LogicalXOr(var X,Y:array of Single);
        procedure CmpGreater(var X,Y:array of Single);
        procedure CmpLower(var X,Y:array of Single);
        procedure CmpEqual(var X,Y:array of Single);
        procedure CmpGreaterEqual(var X,Y:array of Single);
        procedure CmpLowerEqual(var X,Y:array of Single);
        procedure AddSquare(var X,Y:array of Single);
        function InnerSum(var X:array of Single):Single;
        function InnerSumAbs (var X:array of Single):Single;
        function DotProduct (var X,Y:array of Single):Single;
        function MaxValue(var X:array of Single):Single;
        function MaxAbsValue(var X:array of Single):Single;
        function IndexMaxValue(var X:array of Single) : integer;
        function IndexMinValue(var X:array of Single) : integer;
        function MinValue(var X:array of Single):Single;
        function MinAbsValue(var X:array of Single):Single;
        function Mean (var X:array of Single):Single;
        procedure Scale(var X:array of Single; value : single);
        end;

procedure TestLoadFromAddr;
// Pointer Operations
procedure AddrToData( PADs:pointer; NumElements:longint);
procedure LoadFromAddr(var ADs:array of pointer);
procedure PointerVectToSingleVect({output}var Data: array of Single;{input} var ADs:array of pointer);

var SSVO:TSSVO;
implementation

constructor TSSVO.Create;
begin
  inherited Create;
  A:=TFVector.BuildUsingArray(AR);
  B:=TFVector.BuildUsingArray(AR);
end;


destructor TSSVO.Destroy;
begin
  A.Free;
  B.Free;
  inherited Destroy;
end;

procedure TSSVO.Add(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.Add(B);
end;

procedure TSSVO.Sub(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.Sub(B);
end;

procedure TSSVO.Mul(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.Mul(B);
end;

procedure TSSVO.Divide(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.Divide(B);
end;

procedure TSSVO.Sqrt(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.Sqrt(B);
end;

procedure TSSVO.Reciprocal(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.Reciprocal(B);
end;

procedure TSSVO.RecSqrt(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.RecSqrt(B);
end;

procedure TSSVO.Max(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.Max(B);
end;

procedure TSSVO.Min(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.Min(B);
end;

procedure TSSVO.LogicalAnd(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.LogicalAnd(B);
end;

procedure TSSVO.LogicalOr(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.LogicalOr(B);
end;

procedure TSSVO.LogicalXOr(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.LogicalXOr(B);
end;

procedure TSSVO.CmpGreater(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.CmpGreater(B);
end;

procedure TSSVO.CmpLower(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.CmpLower(B);
end;

procedure TSSVO.CmpEqual(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.CmpEqual(B);
end;

procedure TSSVO.CmpGreaterEqual(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.CmpGreaterEqual(B);
end;

procedure TSSVO.CmpLowerEqual(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.CmpLowerEqual(B);
end;

procedure TSSVO.AddSquare(var X,Y:array of Single);
begin
A.UseArray(X);
B.UseArray(Y);
A.AddSquare(B);
end;

function TSSVO.InnerSum(var X:array of Single):Single;
begin
A.UseArray(X);
InnerSum:=A.InnerSum;
end;

function TSSVO.InnerSumAbs (var X:array of Single):Single;
begin
A.UseArray(X);
InnerSumAbs:=A.InnerSumAbs;
end;

function TSSVO.DotProduct (var X,Y:array of Single):Single;
begin
A.UseArray(X);
B.UseArray(Y);
DotProduct:=A.DotProduct(B);
end;

function TSSVO.MaxValue(var X:array of Single):Single;
begin
A.UseArray(X);
MaxValue:=A.MaxValue ;
end;

function TSSVO.MaxAbsValue(var X:array of Single):Single;
begin
A.UseArray(X);
MaxAbsValue:=A.MaxAbsValue ;
end;

function TSSVO.IndexMaxValue(var X:array of Single) : integer;
begin
A.UseArray(X);
IndexMaxValue:=A.IndexMaxValue ;
end;

function TSSVO.IndexMinValue(var X:array of Single) : integer;
begin
A.UseArray(X);
IndexMinValue:=A.IndexMinValue ;
end;

function TSSVO.MinValue(var X:array of Single):Single;
begin
A.UseArray(X);
MinValue:=A.MinValue ;
end;

function TSSVO.MinAbsValue(var X:array of Single):Single;
begin
A.UseArray(X);
MinAbsValue:=A.MinAbsValue ;
end;

function TSSVO.Mean (var X:array of Single):Single;
begin
A.UseArray(X);
Mean:=A.Mean;
end;

procedure TSSVO.Scale(var X:array of Single; value : single);
begin
A.UseArray(X);
A.Scale(value);
end;

procedure AddrToData( PADs:pointer; NumElements:longint);
begin
asm
(*  mov edx,PADs
  mov ecx,NumElements
  //  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop
@LargeAddLoop:
  mov EAX,[EDX]
  mov EAX,[EAX]
  mov [EDX],EAX
  add EDX,4
  dec ecx
  jnz @LargeAddLoop
@SkipLargeAddLoop:*)
  pusha
  mov edx,PADs
  mov ecx,NumElements
  shr ecx,1  // number of large iterations = number of elements / 2
  jz @SkipLargeAddLoop
@LargeAddLoop:
  mov EAX,[EDX]
  mov EBX,[EDX+4]
  mov EAX,[EAX]
  mov EBX,[EBX]
  mov [EDX],EAX
  mov [EDX+4],EBX
  add EDX,8
  dec ecx
  jnz @LargeAddLoop
@SkipLargeAddLoop:
  popa
end; // of asm
end;

procedure LoadFromAddr(var ADs:array of pointer);
begin
AddrToData(addr(ADs),Length(ADs));
end; // of procedure

procedure PointerVectToSingleVect(var Data: array of Single; var ADs:array of pointer);
begin
move(ADs,Data,Sizeof(Data));
AddrToData(Addr(Data),Length(Data));
end;

procedure TestLoadFromAddr;
const MAX=10;

type TData = array[0..MAX-1] of Single;
     TDataPtr = ^TData;

var C:longint;
    X,Z: TData;
    YPtr:TDataPtr;
    Y: array[0..MAX-1] of pointer;
begin
for C:=0 to MAX-1
    do begin
       X[C]:=10+C;// define pointers
       Y[C]:=Addr(X[C]);
       end;
PointerVectToSingleVect(Z,Y);
for C:=0 to MAX-1
    do if (Z[C]<>X[C]) then writeln('Error at PointerVectToSingleVect');

LoadFromAddr(Y);
YPtr:=Addr(Y);
for C:=0 to MAX-1
    do if (YPtr^[C]<>X[C]) then writeln('Error at TestLoadFromAddr');
end;


initialization
SSVO:=TSSVO.Create;
finalization
SSVO.Free;
end.
