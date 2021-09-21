unit DXMath3;

//
// DXMath3 - Geometry routines
//   DXMath3RDTSC - timed version of geometry routines
//   DXMathRef - reference version of geometry routines
//
//    by Henri Hakl   (henri@cs.sun.ac.za)
//
// Highly optimized geometry routines for 3- and 4-component vectors and
// 3x3, 4x4 matrices. Several routines are available as both assembler and
// 3DNow optimized.
//
// Often Math units such as this one claim huge performance gains by using
// assembler, SSE and 3DNow instructions, however, generally speed increases
// are only within 10% to 20%; if compared to well implemented high-level
// equivalents. Odd exceptions do exist though, for example the assembler
// transpose is four times faster then the high-level equivalent.
//
// *** Acknowledgements, thanks go out to:
//   Eric Grange                 (GLScene, for his "shameless plug" from AMD's library)
//   Sergei "WarLand" Kuratiov   (for his asm routines for cgLib)
//   Rawhed                      (for his MMX tutorial, in HUGI19)
//   Chris Dragan                (for his invaluable MMX, SSE and 3DNow! tutorial in HUGI20)
//   Cort Stratton               (for his article on "Optimizing for SSE", in HUGI25)
//
// *** Speed notation: xx / yy
//   means that this function rated xx, where the standard (usually assembler)
//   version rated yy; rating was performed by repeating each procedure
//   10 000 000 times. The 3-component vector and 3x3 matrix procedures were
//   used for testing; generally however the 4-component vector and 4x4 matrix
//   procedures are even more efficient then their 3-size counterparts.
//
// *** Note that it is always faster to use Procedures, rather then Functions,
//   i.e. it is better to use:
//
//     pMatrixMul(R, M, N);
//     pMatrixMul(S, M, R);   {FASTER!!}
//     pMatrixMul(T, M, S);
//
//   then to use:
//
//     T := fMatrixMul(M, fMatrixMul(M, fMatrixMul(M, N)));   {SLOWER!!}
//
// *** Note that SSE and SSE2 are supported on Athlon machines, however the
//   implementation is not nearly as fast on Athlon machines as it is on Intel
//   machines, you will have to test which functions work best on your machine
//   or the target machine you are developing for.
//

//
// Version History
//   0.0.0 - work started 23rd of October 2002
//   1.0.0 - completed first draft on the 27th of October 2002
//   1.0.1 - revision, tests, bug fixes - completed on the 29th of October 2002
//


interface

type
  TReal = single;

  PVector3 = ^TVector3;
  TVector3 = array[0..3] of TReal;
  PVector4 = ^TVector4;
  TVector4 = array[0..3] of TReal;

  PMatrix3 = ^TMatrix3;
  TMatrix3 = array[0..15] of TReal;
  PMatrix4 = ^TMatrix4;
  TMatrix4 = array[0..15] of TReal;

var
  my_timer : cardinal;
  bSSE     : boolean;// = false;
  bSSE2    : boolean;// = false;
  b3DNow   : boolean;// = false;
  b3DNowX  : boolean;// = false;
  svendor  : string;

function GetRDTSC : cardinal;

function  fSSESupport : boolean;
function  f3DNowSupport : boolean;
procedure SetCPUSupport;

function  fGetVector(x, y, z : TReal) : TVector3; overload;
function  fGetVector(x, y, z, w : TReal) : TVector4; overload;
function  fGetMatrix(m00, m10, m20, m01, m11, m21, m02, m12, m22 : TReal) : TMatrix3; overload;
function  fGetMatrix(m00, m10, m20, m30, m01, m11, m21, m31, m02, m12, m22, m32, m03, m13, m23, m33 : TReal) : TMatrix4; overload;

procedure p3DNowSizeOf(const a : TVector3; var r : TReal); overload;
procedure p3DNowSizeOf15(const a : TVector3; var r : TReal); overload;
procedure p3DNowSquareSizeOf(const a : TVector3; var r : TReal); overload;
procedure p3DNowSizeOf(const a : TVector4; var r : TReal); overload;
procedure p3DNowSizeOf15(const a : TVector4; var r : TReal); overload;
procedure p3DNowSquareSizeOf(const a : TVector4; var r : TReal); overload;

function  fSizeOf(const a : TVector3) : TReal; overload;
function  fSizeOf(const a : TVector4) : TReal; overload;
function  fSquareSizeOf(const a : TVector3) : TReal; overload;
function  fSquareSizeOf(const a : TVector4) : TReal; overload;
function  fDistance(const a, b : TVector3) : TReal; overload;
function  fDistance(const a, b : TVector4) : TReal; overload;
function  fSquareDistance(const a, b : TVector3) : TReal; overload;
function  fSquareDistance(const a, b : TVector4) : TReal; overload;

function  f3DNowVectorSub(const a, b : TVector3) : TVector3; overload;
function  f3DNowVectorSub(const a, b : TVector4) : TVector4; overload;
procedure p3DNowVectorSub(out r : TVector3; const a, b : TVector3); overload;
procedure p3DNowVectorSub(out r : TVector4; const a, b : TVector4); overload;
function  f3DNowVectorAdd(const a, b : TVector3) : TVector3; overload;
function  f3DNowVectorAdd(const a, b : TVector4) : TVector4; overload;
procedure p3DNowVectorAdd(out r : TVector3; const a, b : TVector3); overload;
procedure p3DNowVectorAdd(out r : TVector4; const a, b : TVector4); overload;

function  fSSEVectorSub(const a, b : TVector3) : TVector3; overload;
function  fSSEVectorSub(const a, b : TVector4) : TVector4; overload;
procedure pSSEVectorSub(out r : TVector3; const a, b : TVector3); overload;
procedure pSSEVectorSub(out r : TVector4; const a, b : TVector4); overload;
function  fSSEVectorAdd(const a, b : TVector3) : TVector3; overload;
function  fSSEVectorAdd(const a, b : TVector4) : TVector4; overload;
procedure pSSEVectorAdd(out r : TVector3; const a, b : TVector3); overload;
procedure pSSEVectorAdd(out r : TVector4; const a, b : TVector4); overload;

function  fVectorSub(const a, b : TVector3) : TVector3; overload;
function  fVectorSub(const a, b : TVector4) : TVector4; overload;
procedure pVectorSub(out r : TVector3; const a, b : TVector3); overload;
procedure pVectorSub(out r : TVector4; const a, b : TVector4); overload;
function  fVectorAdd(const a, b : TVector3) : TVector3; overload;
function  fVectorAdd(const a, b : TVector4) : TVector4; overload;
procedure pVectorAdd(out r : TVector3; const a, b : TVector3); overload;
procedure pVectorAdd(out r : TVector4; const a, b : TVector4); overload;

function  fVectorEqual(const a, b : TVector3) : boolean; overload
function  fVectorEqual(const a, b : TVector4) : boolean; overload;
function  fVectorAbsSmaller(const a, b : TVector3) : boolean; overload;
function  fVectorAbsSmaller(const a, b : TVector4) : boolean; overload;

function  fDotProduct(const a, b : TVector3) : single; overload;
function  fDotProduct(const a, b : TVector4) : single; overload;
function  fCrossProduct(const a, b : TVector3) : TVector3; overload;
procedure pCrossProduct(var r : TVector3; const a, b : TVector3); overload;
procedure pCrossProduct2(var r : TVector3; const a, b : TVector3); overload;
procedure pCrossProduct3(var r : TVector3; const a, b : TVector3); register;
procedure pSSE2CrossProduct(var r : TVector3; const a, b : TVector3); register;

function  f3DNowNormalize(const a : TVector3) : TVector3; overload;
function  f3DNowNormalize(const a : TVector4) : TVector4; overload;
procedure p3DNowNormalize(var a : TVector3); overload;
procedure p3DNowNormalize(var a : TVector4); overload;
procedure p3DNowNormalize(out r : TVector3; const a : TVector3); overload;
procedure p3DNowNormalize(out r : TVector4; const a : TVector4); overload;

function  fNormalize(const a : TVector3) : TVector3; overload;
function  fNormalize(const a : TVector4) : TVector4; overload;
procedure pNormalize(var a : TVector3); overload;
procedure pNormalize(var a : TVector4); overload;
procedure pNormalize(out r : TVector3; const a : TVector3); overload;
procedure pNormalize(out r : TVector4; const a : TVector4); overload;

procedure p3DNowVectorScale(var a : TVector3; k : TReal); overload;
procedure p3DNowVectorScale(var a : TVector4; k : TReal); overload;

function  fVectorScale(const a : TVector3; k : TReal) : TVector3; overload;
function  fVectorScale(const a : TVector4; k : TReal) : TVector4; overload;
procedure pVectorScale(var a : TVector3; k : TReal); overload;
procedure pVectorScale(var a : TVector4; k : TReal); overload;

procedure p3DNowVectorLERP(out r : TVector3; const a, b : TVector3; k : TReal); overload;
procedure p3DNowVectorLERP(out r : TVector4; const a, b : TVector4; k : TReal); overload;

function  fVectorLERP(const a, b : TVector3; k : TReal) : TVector3; overload;
function  fVectorLERP(const a, b : TVector4; k : TReal) : TVector4; overload;
procedure pVectorLERP(out r : TVector3; const a, b : TVector3; k : TReal); overload;
procedure pVectorLERP(out r : TVector4; const a, b : TVector4; k : TReal); overload;

procedure p3DNowMatrixAdd(out R : TMatrix3; const M, N : TMatrix3); overload;
procedure p3DNowMatrixAdd(out R : TMatrix4; const M, N : TMatrix4); overload;

function  fMatrixAdd(const M, N : TMatrix3) : TMatrix3; overload;
function  fMatrixAdd(const M, N : TMatrix4) : TMatrix4; overload;
procedure pMatrixAdd(out R : TMatrix3; const M, N : TMatrix3); overload;
procedure pMatrixAdd(out R : TMatrix4; const M, N : TMatrix4); overload;

procedure p3DNowMatrixSub(out R : TMatrix3; const M, N : TMatrix3); overload;
procedure p3DNowMatrixSub(out R : TMatrix4; const M, N : TMatrix4); overload;

function  fMatrixSub(const M, N : TMatrix3) : TMatrix3; overload;
function  fMatrixSub(const M, N : TMatrix4) : TMatrix4; overload;
procedure pMatrixSub(out R : TMatrix3; const M, N : TMatrix3); overload;
procedure pMatrixSub(out R : TMatrix4; const M, N : TMatrix4); overload;

procedure p3DNowMatrixScale(var A : TMatrix3; k : TReal); overload;
procedure p3DNowMatrixScale(var A : TMatrix4; k : TReal); overload;

procedure pMatrixScale(var A : TMatrix3; k : TReal); overload;
procedure pMatrixScale(var A : TMatrix4; k : TReal); overload;

procedure pSSETranspose(var R : TMatrix3); overload;
procedure pSSETranspose(var R : TMatrix4); overload;
procedure pSSETranspose(out R : TMatrix3; const A : TMatrix3); overload;
procedure pSSETranspose(out R : TMatrix4; const A : TMatrix4); overload;

function  fTranspose(const R : TMatrix3) : TMatrix3; overload;
function  fTranspose(const R : TMatrix4) : TMatrix4; overload;
procedure pTranspose(var R : TMatrix3); overload;
procedure pTranspose(var R : TMatrix4); overload;
procedure pTranspose(out R : TMatrix3; const A : TMatrix3); overload;
procedure pTranspose(out R : TMatrix4; const A : TMatrix4); overload;

procedure p3DNowMatrixVector(var r : TVector3; const M : TMatrix3; const a : TVector3); overload;
procedure p3DNowMatrixVector(var r : TVector4; const M : TMatrix4; const a : TVector4); overload;

procedure pSSEMatrixVector(out r : TVector3; const M : TMatrix3; const a : TVector3); overload;
procedure pSSEMatrixVector(out r : TVector4; const M : TMatrix4; const a : TVector4); overload;

procedure pMatrixVector(out r : TVector3; const M : TMatrix3; const a : TVector3); overload;
procedure pMatrixVector(out r : TVector4; const M : TMatrix4; const a : TVector4); overload;

function  f3DNowMatrixMul(const M, N : TMatrix3) : TMatrix3; overload;
function  f3DNowMatrixMul(const M, N : TMatrix4) : TMatrix4; overload;
procedure p3DNowMatrixMul(out R : TMatrix3; const M, N : TMatrix3); overload;
procedure p3DNowMatrixMul(out R : TMatrix4; const M, N : TMatrix4); overload;

procedure pSSEMatrixMul(out R : TMatrix3; const M, N : TMatrix3); overload;
procedure pSSEMatrixMul(out R : TMatrix4; const M, N : TMatrix4); overload;
procedure pSSEMatrixMulIntel(out R : TMatrix4; const M, N : TMatrix4); overload;
procedure pSSEMatrixMulModifiedIntel(out R : TMatrix4; const M, N : TMatrix4); overload;
procedure pSSE2MatrixMul(out R : TMatrix4; const M, N : TMatrix4); overload;
procedure pSSE2MatrixMul2(out R : TMatrix4; const M, N : TMatrix4); overload;

procedure pMatrixMul(out R : TMatrix3; const M, N : TMatrix3); overload;
procedure pMatrixMul(out R : TMatrix4; const M, N : TMatrix4); overload;

function  fMatrixSkew(const a : TVector3) : TMatrix3;
procedure pMatrixSkew(out R : TMatrix3; const a : TVector3);


implementation

function GetRDTSC : cardinal;
asm
  rdtsc
end;

function f3DNowSupport : boolean;
// returns true if PC supports 3DNow instructions
asm
  push	ebx
  mov	eax, $80000000
  cpuid
  cmp	eax, $80000001
  jb	@no_3dnow
  mov	eax, $80000001
  cpuid
  test	edx, $80000000
  jz	@no_3dnow
  mov	eax, 1
  jmp	@ok
@no_3dnow:
  xor	eax, eax
@ok:
  pop	ebx
end;

function fSSESupport : boolean;
// returns true if PC supports SSE instructions
asm
  push	ebx
  xor	eax, eax
  cpuid
  cmp	eax, 1
  jl	@no_sse
  mov	eax, 1
  cpuid
  test	edx, $02000000
  jz	@no_sse
  mov	eax, 1
  jmp	@ok
@no_sse:
  xor	eax, eax
@ok:
  pop	ebx
end;

procedure SetCPUSupport;
var
  count : integer;
  cvendor : array[0..12] of char;
  flags : cardinal;
begin
  bSSE := false;
  bSSE2 := false;
  b3DNow := false;
  b3DNowX := false;
  asm
    push ebx
    xor eax, eax
    cpuid
    mov dword ptr [cvendor], ebx
    mov dword ptr [cvendor + 4], edx
    mov dword ptr [cvendor + 8], ecx
    mov count, eax
    pop ebx
  end;
  svendor := cvendor;
  if count >= 1 then begin
    if svendor = 'AuthenticAMD' then begin
      asm
        push ebx
        mov eax, $80000001
        cpuid
        mov [flags], edx
        pop ebx
      end;
      if flags and $80000000 <> 0 then b3DNow := true;
      if flags and $40000000 <> 0 then b3DNowX := true;
      asm
        push ebx
        mov eax, 1
        cpuid
        mov [flags], edx
        pop ebx
      end;
      if flags and $02000000 <> 0 then bSSE := true;
    end else if svendor = 'GenuineIntel' then begin
      asm
        push ebx
        mov eax, 1
        cpuid
        mov [flags], edx
        pop ebx
      end;
      if flags and $02000000 <> 0 then bSSE := true;
      if flags and $04000000 <> 0 then bSSE2 := true;
    end;
  end;
end;

function fGetVector(x, y, z : TReal) : TVector3;
// returns vector
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
end;

function fGetVector(x, y, z, w : TReal) : TVector4;
// returns vector
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
  Result[3] := w;
end;

function fGetMatrix(m00, m10, m20, m01, m11, m21, m02, m12, m22 : TReal) : TMatrix3;
// returns matrix
begin
  Result[0] := m00;
  Result[1] := m10;
  Result[2] := m20;

  Result[4] := m01;
  Result[5] := m11;
  Result[6] := m21;

  Result[8] := m02;
  Result[9] := m12;
  Result[10]:= m22;
end;

function fGetMatrix(m00, m10, m20, m30, m01, m11, m21, m31, m02, m12, m22, m32, m03, m13, m23, m33 : TReal) : TMatrix4;
// returns matrix
begin
  Result[0] := m00;
  Result[1] := m10;
  Result[2] := m20;
  Result[3] := m30;

  Result[4] := m01;
  Result[5] := m11;
  Result[6] := m21;
  Result[7] := m31;

  Result[8] := m02;
  Result[9] := m12;
  Result[10]:= m22;
  Result[11]:= m32;

  Result[12]:= m03;
  Result[13]:= m13;
  Result[14]:= m23;
  Result[15]:= m33;
end;

procedure p3DNowSizeOf(const a : TVector3; var r : TReal);
// compute size of a, return in r - not as accurate as FPU
// about 24bit accuracy, about 2 times as fast as FPU
asm
  movq        mm0,[eax]    // x  y
  movd        mm1,[eax+8]  // z  -
  pfmul       mm0,mm0      // xx yy
  pfmul       mm1,mm1      // zz -
  pfacc       mm0,mm0      // xx+yy    xx+yy
  pfadd       mm0,mm1      // xx+yy+zz -

  pfrsqrt     mm1,mm0      // sqrt(xx+yy+zz)
  movq        mm2,mm1      // make backup of first result
  pfmul       mm1,mm1      // square the first sqrt solution
  pfrsqit1    mm1,mm0      // sqrt - next iteration, uses original value
  pfrcpit2    mm1,mm2      // sqrt - next iteration, uses first result
  punpckldq   mm1,mm1      // finish

  pfmul       mm0,mm1
  movd        [edx], mm0

  femms
end;

procedure p3DNowSizeOf15(const a : TVector3; var r : TReal);
// compute size of a, return in r - low accuracy 15bit version
// about 3 times as fast as FPU
asm
  movq        mm0,[eax]    // x  y
  movd        mm1,[eax+8]  // z
  pfmul       mm0,mm0      // xx yy
  pfmul       mm1,mm1      // zz ww
  pfacc       mm0,mm0      // xx+yy    xx+yy
  pfadd       mm0,mm1      // xx+yy+zz
  pfrsqrt     mm1,mm0      // sqrt(xx+yy+zz)
  pfmul       mm0,mm1
  movd        [edx], mm0
  femms
end;

procedure p3DNowSizeOf(const a : TVector4; var r : TReal);
// compute size of a, return in r - not as accurate as FPU
// about 24bit accuracy, about 2 times as fast as FPU
asm
  movq        mm0,[eax]    // x  y
  movq        mm1,[eax+8]  // z  w
  pfmul       mm0,mm0      // xx yy
  pfmul       mm1,mm1      // zz ww
  pfacc       mm0,mm0      // xx+yy    xx+yy
  pfacc       mm1,mm1      // zz+ww    zz+ww
  pfadd       mm0,mm1      // xx+yy+zz+ww

  pfrsqrt     mm1,mm0      // sqrt(xx+yy+zz)
  movq        mm2,mm1      // make backup of first result
  pfmul       mm1,mm1      // square the first sqrt solution
  pfrsqit1    mm1,mm0      // sqrt - next iteration, uses original value
  pfrcpit2    mm1,mm2      // sqrt - next iteration, uses first result
  punpckldq   mm1,mm1      // finish

  pfmul       mm0,mm1
  movd        [edx], mm0

  femms
end;

procedure p3DNowSizeOf15(const a : TVector4; var r : TReal);
// compute size of a, return in r - low accuracy 15bit version
// about 3 times as fast as FPU
asm
  movq        mm0,[eax]    // x  y
  movq        mm1,[eax+8]  // z  w
  pfmul       mm0,mm0      // xx yy
  pfmul       mm1,mm1      // zz ww
  pfacc       mm0,mm0      // xx+yy    xx+yy
  pfacc       mm1,mm1      // zz+ww    zz+ww
  pfadd       mm0,mm1      // xx+yy+zz+ww
  pfrsqrt     mm1,mm0      // sqrt(xx+yy+zz)
  pfmul       mm0,mm1
  movd        [edx], mm0
  femms
end;

procedure p3DNowSquareSizeOf(const a : TVector3; var r : TReal);
// very fast, returns square size of vector a into r
asm
  movq        mm0,[eax]    // x  y
  movd        mm1,[eax+8]  // z  -
  pfmul       mm0,mm0      // xx yy
  pfmul       mm1,mm1      // zz -
  pfacc       mm0,mm0      // xx+yy    xx+yy
  pfadd       mm0,mm1      // xx+yy+zz -
  movd        [edx], mm0   // store result
  femms
end;

procedure p3DNowSquareSizeOf(const a : TVector4; var r : TReal);
// very fast, returns square size of vector a into r
asm
  movq        mm0,[eax]    // x  y
  movq        mm1,[eax+8]  // z  w
  pfmul       mm0,mm0      // xx yy
  pfmul       mm1,mm1      // zz ww
  pfacc       mm0,mm0      // xx+yy    xx+yy
  pfacc       mm1,mm1      // zz+ww    zz+ww
  pfadd       mm0,mm1      // xx+yy+zz+ww
  movd        [edx], mm0   // store result
  femms
end;

function fSizeOf(const a : TVector3) : TReal;
// returns size of vector
asm
  fld  [eax]
  fmul st(0), st(0)

  fld  [eax + 4]
  fmul st(0), st(0)
  faddp

  fld  [eax + 8]
  fmul st(0), st(0)
  faddp

  fsqrt
end;

function fSizeOf(const a : TVector4) : TReal;
// returns size of vector
asm
  fld  [eax]
  fmul st(0), st(0)

  fld  [eax + 4]
  fmul st(0), st(0)
  faddp

  fld  [eax + 8]
  fmul st(0), st(0)
  faddp

  fld  [eax + 12]
  fmul st(0), st(0)
  faddp

  fsqrt
end;

function fSquareSizeOf(const a : TVector3) : TReal;
// returns square size of vector
asm
  fld  [eax]
  fmul st(0), st(0)

  fld  [eax + 4]
  fmul st(0), st(0)
  faddp

  fld  [eax + 8]
  fmul st(0), st(0)
  faddp
end;

function fSquareSizeOf(const a : TVector4) : TReal;
// returns square size of vector
asm
  fld  [eax]
  fmul st(0), st(0)

  fld  [eax + 4]
  fmul st(0), st(0)
  faddp

  fld  [eax + 8]
  fmul st(0), st(0)
  faddp

  fld  [eax + 12]
  fmul st(0), st(0)
  faddp
end;

function fDistance(const a, b : TVector3) : TReal; register;
// returns distance between two vectors
asm
  fld  [eax]
  fsub [edx]
  fmul st, st
  fld  [eax + 4]
  fsub [edx + 4]
  fmul st, st
  fadd
  fld  [eax + 8]
  fsub [edx + 8]
  fmul st, st
  fadd
  fsqrt
end;

function fDistance(const a, b : TVector4) : TReal; register;
// returns distance between two vectors
asm
  fld  [eax]
  fsub [edx]
  fmul st, st
  fld  [eax + 4]
  fsub [edx + 4]
  fmul st, st
  fadd
  fld  [eax + 8]
  fsub [edx + 8]
  fmul st, st
  fadd
  fld  [eax + 12]
  fsub [edx + 12]
  fmul st, st
  fadd
  fsqrt
end;

function fSquareDistance(const a, b : TVector3) : TReal; register;
// returns square distance between two vectors
asm
  fld  [eax]
  fsub [edx]
  fmul st, st
  fld  [eax + 4]
  fsub [edx + 4]
  fmul st, st
  fadd
  fld  [eax + 8]
  fsub [edx + 8]
  fmul st, st
  fadd
end;

function fSquareDistance(const a, b : TVector4) : TReal; register;
// returns square distance between two vectors
asm
  fld  [eax]
  fsub [edx]
  fmul st, st
  fld  [eax + 4]
  fsub [edx + 4]
  fmul st, st
  fadd
  fld  [eax + 8]
  fsub [edx + 8]
  fmul st, st
  fadd
  fld  [eax + 12]
  fsub [edx + 12]
  fmul st, st
  fadd
end;

function f3DNowVectorSub(const a, b : TVector3) : TVector3; register;
// returns vector a - b
asm
  movq  mm3, [edx + 8]
  movq  mm0, [eax]
  movq  mm1, [edx]
  movq  mm2, [eax + 8]
  pfsub mm0, mm1
  pfsub mm2, mm3
  movq  [ecx], mm0
  movq  [ecx + 8], mm2
  femms
end;

function f3DNowVectorSub(const a, b : TVector4) : TVector4; register;
// returns vector a - b
asm
  movq  mm3, [edx + 8]
  movq  mm0, [eax]
  movq  mm1, [edx]
  movq  mm2, [eax + 8]
  pfsub mm0, mm1
  pfsub mm2, mm3
  movq  [ecx], mm0
  movq  [ecx + 8], mm2
  femms
end;

procedure p3DNowVectorSub(out r : TVector3; const a, b : TVector3); register;
// returns vector r = a - b
asm
  movq  mm3, [ecx + 8]
  movq  mm0, [edx]
  movq  mm1, [ecx]
  movq  mm2, [edx + 8]
  pfsub mm0, mm1
  pfsub mm2, mm3
  movq  [eax], mm0
  movq  [eax + 8], mm2
  femms
end;

procedure p3DNowVectorSub(out r : TVector4; const a, b : TVector4); register;
// returns vector r = a - b
asm
  movq  mm3, [ecx + 8]
  movq  mm0, [edx]
  movq  mm1, [ecx]
  movq  mm2, [edx + 8]
  pfsub mm0, mm1
  pfsub mm2, mm3
  movq  [eax], mm0
  movq  [eax + 8], mm2
  femms
end;

function f3DNowVectorAdd(const a, b : TVector3) : TVector3; register;
// returns vector a + b
asm
  movq  mm3, [edx + 8]
  movq  mm0, [eax]
  movq  mm1, [edx]
  movq  mm2, [eax + 8]
  pfadd mm0, mm1
  pfadd mm3, mm2
  movq  [ecx], mm0
  movq  [ecx + 8], mm3
  femms
end;

function f3DNowVectorAdd(const a, b : TVector4) : TVector4; register;
// returns vector a + b
asm
  movq  mm3, [edx + 8]
  movq  mm0, [eax]
  movq  mm1, [edx]
  movq  mm2, [eax + 8]
  pfadd mm0, mm1
  pfadd mm3, mm2
  movq  [ecx], mm0
  movq  [ecx + 8], mm3
  femms
end;

procedure p3DNowVectorAdd(out r : TVector3; const a, b : TVector3); register;
// returns vector r = a + b
asm
  movq  mm3, [ecx + 8]
  movq  mm0, [edx]
  movq  mm1, [ecx]
  movq  mm2, [edx + 8]
  pfadd mm0, mm1
  pfadd mm3, mm2
  movq  [eax], mm0
  movq  [eax + 8], mm3
  femms
end;

procedure p3DNowVectorAdd(out r : TVector4; const a, b : TVector4); register;
// returns vector r = a + b
asm
  movq  mm3, [ecx + 8]
  movq  mm0, [edx]
  movq  mm1, [ecx]
  movq  mm2, [edx + 8]
  pfadd mm0, mm1
  pfadd mm3, mm2
  movq  [eax], mm0
  movq  [eax + 8], mm3
  femms
end;

function fSSEVectorSub(const a, b : TVector3) : TVector3; register;
// returns vector a - b
asm
  movaps xmm0, [eax]
  movaps xmm1, [edx]
  subps  xmm0, xmm1
  movaps [ecx], xmm0
end;

function fSSEVectorSub(const a, b : TVector4) : TVector4; register;
// returns vector a - b
asm
  movaps xmm0, [eax]
  movaps xmm1, [edx]
  subps  xmm0, xmm1
  movaps [ecx], xmm0
end;

procedure pSSEVectorSub(out r : TVector3; const a, b : TVector3); register;
// returns vector a - b
asm
  movaps xmm0, [edx]
  movaps xmm1, [ecx]
  subps  xmm0, xmm1
  movaps [eax], xmm0
end;

procedure pSSEVectorSub(out r : TVector4; const a, b : TVector4); register;
// returns vector a - b
asm
  movaps xmm0, [edx]
  movaps xmm1, [ecx]
  subps  xmm0, xmm1
  movaps [eax], xmm0
end;

function fSSEVectorAdd(const a, b : TVector3) : TVector3; register;
// returns vector a - b
asm
  movaps xmm0, [eax]
  movaps xmm1, [edx]
  addps  xmm0, xmm1
  movaps [ecx], xmm0
end;

function fSSEVectorAdd(const a, b : TVector4) : TVector4; register;
// returns vector a - b
asm
  movaps xmm0, [eax]
  movaps xmm1, [edx]
  addps  xmm0, xmm1
  movaps [ecx], xmm0
end;

procedure pSSEVectorAdd(out r : TVector3; const a, b : TVector3); register;
// returns vector a - b
asm
  movaps xmm0, [edx]
  movaps xmm1, [ecx]
  addps  xmm0, xmm1
  movaps [eax], xmm0
end;

procedure pSSEVectorAdd(out r : TVector4; const a, b : TVector4); register;
// returns vector a - b
asm
  movaps xmm0, [edx]
  movaps xmm1, [ecx]
  addps  xmm0, xmm1
  movaps [eax], xmm0
end;

function fVectorSub(const a, b : TVector3) : TVector3; register;
// returns vector a - b
asm
 fld  [eax]
 fsub [edx]
 fstp [ecx]

 fld  [eax + 4]
 fsub [edx + 4]
 fstp [ecx + 4]

 fld  [eax + 8]
 fsub [edx + 8]
 fstp [ecx + 8]
end;

function fVectorSub(const a, b : TVector4) : TVector4; register;
// returns vector a - b
asm
 fld  [eax]
 fsub [edx]
 fstp [ecx]

 fld  [eax + 4]
 fsub [edx + 4]
 fstp [ecx + 4]

 fld  [eax + 8]
 fsub [edx + 8]
 fstp [ecx + 8]

 fld  [eax + 12]
 fsub [edx + 12]
 fstp [ecx + 12]
end;

procedure pVectorSub(out r : TVector3; const a, b : TVector3); register;
// returns vector r = a - b
asm
 fld  [edx]
 fsub [ecx]
 fstp [eax]

 fld  [edx + 4]
 fsub [ecx + 4]
 fstp [eax + 4]

 fld  [edx + 8]
 fsub [ecx + 8]
 fstp [eax + 8]
end;

procedure pVectorSub(out r : TVector4; const a, b : TVector4); register;
// returns vector r = a - b
asm
 fld  [edx]
 fsub [ecx]
 fstp [eax]

 fld  [edx + 4]
 fsub [ecx + 4]
 fstp [eax + 4]

 fld  [edx + 8]
 fsub [ecx + 8]
 fstp [eax + 8]

 fld  [edx + 12]
 fsub [ecx + 12]
 fstp [eax + 12]
end;

function fVectorAdd(const a, b : TVector3) : TVector3; register;
// returns vector a + b
asm
 fld  [eax]
 fadd [edx]
 fstp [ecx]

 fld  [eax + 4]
 fadd [edx + 4]
 fstp [ecx + 4]

 fld  [eax + 8]
 fadd [edx + 8]
 fstp [ecx + 8]
end;

function fVectorAdd(const a, b : TVector4) : TVector4; register;
// returns vector a + b
asm
 fld  [eax]
 fadd [edx]
 fstp [ecx]

 fld  [eax + 4]
 fadd [edx + 4]
 fstp [ecx + 4]

 fld  [eax + 8]
 fadd [edx + 8]
 fstp [ecx + 8]

 fld  [eax + 12]
 fadd [edx + 12]
 fstp [ecx + 12]
end;

procedure pVectorAdd(out r : TVector3; const a, b : TVector3); register;
// returns vector r = a + b
asm
 fld  [edx]
 fadd [ecx]
 fstp [eax]

 fld  [edx + 4]
 fadd [ecx + 4]
 fstp [eax + 4]

 fld  [edx + 8]
 fadd [ecx + 8]
 fstp [eax + 8]
end;

procedure pVectorAdd(out r : TVector4; const a, b : TVector4); register;
// returns vector r = a + b
asm
 fld  [edx]
 fadd [ecx]
 fstp [eax]

 fld  [edx + 4]
 fadd [ecx + 4]
 fstp [eax + 4]

 fld  [edx + 8]
 fadd [ecx + 8]
 fstp [eax + 8]

 fld  [edx + 12]
 fadd [ecx + 12]
 fstp [eax + 12]
end;

function fVectorEqual(const a, b : TVector3) : boolean; register;
// returns true if a = b, otherwise false
asm
  mov ecx, eax
  mov eax, [edx]
  cmp eax, [ecx]
  jne @diff
  mov eax, [edx+$4]
  cmp eax, [ecx+$4]
  jne @diff
  mov eax, [edx+$8]
  cmp eax, [ecx+$8]
  jne @diff
@equal:
  mov al, 1
  ret
@diff:
  xor eax, eax
end;

function fVectorEqual(const a, b : TVector4) : boolean; register;
// returns true if a = b, otherwise false
asm
  mov ecx, eax
  mov eax, [edx]
  cmp eax, [ecx]
  jne @diff
  mov eax, [edx+$4]
  cmp eax, [ecx+$4]
  jne @diff
  mov eax, [edx+$8]
  cmp eax, [ecx+$8]
  jne @diff
  mov eax, [edx+$C]
  cmp eax, [ecx+$C]
  jne @diff
@equal:
  mov al, 1
  ret
@diff:
  xor eax, eax
end;

function fVectorAbsSmaller(const a, b : TVector3) : boolean;
// returns true if component-abs(a) < component-abs(b)
begin
  Result := (abs(a[0]) < abs(b[0])) and (abs(a[1]) < abs(b[1])) and (abs(a[2]) < abs(b[2]));
end;

function fVectorAbsSmaller(const a, b : TVector4) : boolean;
// returns true if component-abs(a) < component-abs(b)
begin
  Result := (abs(a[0]) < abs(b[0])) and (abs(a[1]) < abs(b[1])) and (abs(a[2]) < abs(b[2])) and (abs(a[3]) < abs(b[3]));
end;

function fDotProduct(const a, b : TVector3) : single; register;
// returns dot-product a.b
asm
  fld  [eax]
  fmul [edx]
  fld  [eax + 4]
  fmul [edx + 4]
  faddp
  fld  [eax + 8]
  fmul [edx + 8]
  faddp
end;

function fDotProduct(const a, b : TVector4) : single; register;
// returns dot-product a.b
asm
  fld  [eax]
  fmul [edx]
  fld  [eax + 4]
  fmul [edx + 4]
  faddp
  fld  [eax + 8]
  fmul [edx + 8]
  faddp
  fld  [eax + 12]
  fmul [edx + 12]
  faddp
end;

function fCrossProduct(const a, b : TVector3) : TVector3; register;
// returns cross-product a x b
// compute the cross product - only defined for Vector3, not Vector4
asm
  fld  [eax + 4]
  fmul [edx + 8]
  fld  [edx + 4]
  fmul [eax + 8]
  fsubp
  fstp [ecx]

  fld  [edx]
  fmul [eax + 8]
  fld  [eax]
  fmul [edx + 8]
  fsubp
  fstp [ecx + 4]

  fld  [eax]
  fmul [edx + 4]
  fld  [edx]
  fmul [eax + 4]
  fsubp
  fstp [ecx + 8]
end;

procedure pCrossProduct(var r : TVector3; const a, b : TVector3); register;
// the standard cross-product; slightly better then cross-product3, but slower
// then cross-product1 - performs 12 memory loads
// offers best results of three methods when not accessing the procedure
// repeatedly
asm
  fld  [edx + 4]
  fmul [ecx + 8]
  fld  [edx + 8]
  fmul [ecx + 4]
  fsubp
  fstp [eax]

  fld  [ecx]
  fmul [edx + 8]
  fld  [ecx + 8]
  fmul [edx]
  fsubp
  fstp [eax + 4]

  fld  [edx]
  fmul [ecx + 4]
  fld  [edx + 4]
  fmul [ecx]
  fsubp
  fstp [eax + 8]
end;

procedure pCrossProduct2(var r : TVector3; const a, b : TVector3); register;
// returns cross-product a x b
// compute the cross product - only defined for Vector3, not Vector4
// >this< crossproduct is generally the fastest of the 3 versions available
// by 5-10% if performing several iterations - performs 9 memory loads
asm
  fld  [edx + 8]     // 0: z1
  fld  [edx + 4]     // 1: y1
  fld  st            // 2: y1
  fmul [ecx + 8]     // 2: y1z2
  fld  st(2)         // 3: z1
  fmul [ecx + 4]     // 3: z1y2
  fsubp              // 2: z1y2 - y1z2
  fstp [eax]         // 1: y1

  fmul [ecx]         // 1: y1x2
  fld  [edx]         // 2: x1
  fxch st(1)         // 1: x1,  2: y1x2
  fld  st(1)         // 3: x1
  fmul [ecx + 4]     // 3: x1y2
  fsubrp             // 2: y1x2 - x1y2
  fstp [eax + 8]     // 1: x1

  fmul [ecx + 8]     // 1: x1z2
  fxch st(1)         // 0: x1z2,  1: z1
  fmul [ecx]         // 1: z1x2
  fsubrp             // 0: z1x2 - x1z2
  fstp [eax + 4]     // --
end;

procedure pCrossProduct3(var r : TVector3; const a, b : TVector3); register;
// crossproduct - marginally the slowest of the 3 crossproducts - but uses only
// 6 memory loads and is potentially the best for high-cache hit environments
// or on older/slower RAM
asm
  fld  [ecx]         // 0: x1
  fld  [edx + 4]     // 1: y2
  fld  st            // 2: y2
  fmul st,st(2)      // 2: x1y2
  fld  [ecx + 4]     // 3: y1
  fld  [edx]         // 4: x2
  fld  st            // 5: x2
  fmul st,st(2)      // 5: y1x2
  fsub st,st(3)      // 5: x1y2 - y1x2
  fstp [eax + 8]     // 4: x2

  fld  [ecx + 8]     // 5: z1
  fmul st(1),st      // 4: z1x2
  fld  [edx + 8]     // 6: z2
  fmul st(6),st      // 1: x1z2
  fmul st,st(3)      // 6: y1z2
  fld  st(1)         // 7: z1
  fmul st,st(6)      // 7: z1y2
  fsubrp             // 6: y1z2 - z1y2
  fstp [eax]         // 5: z1
  fstp st            // 4: z1x2

  fsubr st,st(4)     // 4: x1z2 - z1x2
  fstp [eax + 4]     // 3: --

  fstp st            // 2:
  fstp st            // 1:
  fstp st            // --
  fstp st            // --
end;

procedure pSSE2CrossProduct(var r : TVector3; const a, b : TVector3); register;
// very fast SSE2 crossproduct
asm          // 1  2  3  4 -> 4  3  2  1
             // x  y  z  w
             // 00 01 10 11
  pshufd xmm0, [edx], $16  // x  y  y  z   (00 01 01 10) !a!
  pshufd xmm1, [ecx], $61  // y  z  x  y   (01 10 00 01) !b!
  pshufd xmm2, xmm0,  $02  // x  x  x  z   (00 00 00 10) !a!
  pshufd xmm3, xmm1,  $c8  // z  z  z  x   (10 10 10 00) !b!

  mulps  xmm0, xmm1        // xy yz yx zy  !ab!
  mulps  xmm2, xmm3        // xz xz xz zx  !ab!

  pshufd xmm1, xmm0, $ee   // -- -- xy yz (11 10 11 10) !ab!
  pshufd xmm3, xmm2, $ff   // -- -- -- xz (11 11 11 11) !ab!

  subps  xmm0, xmm1        // -- -- (z) (x)
  subps  xmm3, xmm2        // -- -- --  (y)

  unpcklps xmm0, xmm1      // -- (z) (y) (x)
  movaps [eax], xmm0
end;

function  f3DNowNormalize(const a : TVector3) : TVector3; register;
// returns normalized vector (a / |a|)
asm
  movq        mm0,[eax]    // x  y
  movd        mm1,[eax+8]  // z  -
  movq        mm4,mm0      // x  y
  movq        mm3,mm1      // z  -
  pfmul       mm0,mm0      // xx yy
  pfmul       mm1,mm1      // zz -
  pfacc       mm0,mm0      // xx+yy    xx+yy
  pfadd       mm0,mm1      // xx+yy+zz -

  pfrsqrt     mm1,mm0      // sqrt(xx+yy+zz)
  movq        mm2,mm1      //
  pfmul       mm1,mm1      //
  pfrsqit1    mm1,mm0      // sqrt - next iteration
  pfrcpit2    mm1,mm2      // sqrt - next iteration
  punpckldq   mm1,mm1      // sqrt  sqrt

  pfmul       mm3,mm1      // x*sqrt  y*sqrt
  pfmul       mm4,mm1      // z*sqrt
  movd        [edx+8],mm3  // store result
  movq        [edx],mm4    // store result
  femms
end;

function  f3DNowNormalize(const a : TVector4) : TVector4; register;
// returns normalized vector (a / |a|)
asm
  movq        mm0,[eax]
  movq        mm1,[eax+8]
  movq        mm4,mm0
  movq        mm3,mm1
  pfmul       mm0,mm0
  pfmul       mm1,mm1
  pfacc       mm0,mm0
  pfacc       mm1,mm1
  pfadd       mm0,mm1
  pfrsqrt     mm1,mm0
  movq        mm2,mm1

  pfmul       mm1,mm1
  pfrsqit1    mm1,mm0
  pfrcpit2    mm1,mm2
  punpckldq   mm1,mm1
  pfmul       mm3,mm1
  pfmul       mm4,mm1
  movq        [edx+8],mm3
  movq        [edx],mm4
  femms
end;

procedure p3DNowNormalize(var a : TVector3); register;
// returns normalized vector a = (a / |a|)
asm
  movq        mm0,[eax]
  movq        mm1,[eax+8]
  movq        mm4,mm0
  movq        mm3,mm1
  pfmul       mm0,mm0
  pfmul       mm1,mm1
  pfacc       mm0,mm0
  pfadd       mm0,mm1
  pfrsqrt     mm1,mm0
  movq        mm2,mm1

  pfmul       mm1,mm1
  pfrsqit1    mm1,mm0
  pfrcpit2    mm1,mm2
  punpckldq   mm1,mm1
  pfmul       mm3,mm1
  pfmul       mm4,mm1
  movq        [eax+8],mm3
  movq        [eax],mm4
  femms
end;

procedure p3DNowNormalize(var a : TVector4); register;
// returns normalized vector a = (a / |a|)
asm
  movq        mm0,[eax]
  movq        mm1,[eax+8]
  movq        mm4,mm0
  movq        mm3,mm1
  pfmul       mm0,mm0
  pfmul       mm1,mm1
  pfacc       mm0,mm0
  pfacc       mm1,mm1
  pfadd       mm0,mm1
  pfrsqrt     mm1,mm0
  movq        mm2,mm1

  pfmul       mm1,mm1
  pfrsqit1    mm1,mm0
  pfrcpit2    mm1,mm2
  punpckldq   mm1,mm1
  pfmul       mm3,mm1
  pfmul       mm4,mm1
  movq        [eax+8],mm3
  movq        [eax],mm4
  femms
end;

procedure p3DNowNormalize(out r : TVector3; const a : TVector3); register;
// returns normalized vector r = (a / |a|)
asm
  movq        mm0,[edx]
  movd        mm1,[edx+8]
  movq        mm4,mm0
  movq        mm3,mm1
  pfmul       mm0,mm0
  pfmul       mm1,mm1
  pfacc       mm0,mm0
  pfadd       mm0,mm1
  pfrsqrt     mm1,mm0
  movq        mm2,mm1

  pfmul       mm1,mm1
  pfrsqit1    mm1,mm0
  pfrcpit2    mm1,mm2
  punpckldq   mm1,mm1
  pfmul       mm3,mm1
  pfmul       mm4,mm1
  movd        [eax+8],mm3
  movq        [eax],mm4
  femms
end;

procedure p3DNowNormalize(out r : TVector4; const a : TVector4); register;
// returns normalized vector r = (a / |a|)
asm
  movq        mm0,[edx]
  movq        mm1,[edx+8]
  movq        mm4,mm0
  movq        mm3,mm1
  pfmul       mm0,mm0
  pfmul       mm1,mm1
  pfacc       mm0,mm0
  pfacc       mm1,mm1
  pfadd       mm0,mm1
  pfrsqrt     mm1,mm0
  movq        mm2,mm1

  pfmul       mm1,mm1
  pfrsqit1    mm1,mm0
  pfrcpit2    mm1,mm2
  punpckldq   mm1,mm1
  pfmul       mm3,mm1
  pfmul       mm4,mm1
  movq        [eax+8],mm3
  movq        [eax],mm4
  femms
end;

function  fNormalize(const a : TVector3) : TVector3; register;
// returns normalized vector (a / |a|)
asm
  fld  [eax]
  fmul st, st
  fld  [eax + 4]
  fmul st, st
  fadd
  fld  [eax + 8]
  fmul st, st
  fadd
  fsqrt
  fld1
  fdivr
  fld  st
  fmul [eax]
  fstp [edx]
  fld  st
  fmul [eax + 4]
  fstp [edx + 4]
  fmul [eax + 8]
  fstp [edx + 8]
end;

function  fNormalize(const a : TVector4) : TVector4; register;
// returns normalized vector (a / |a|)
asm
  fld  [eax]
  fmul st, st
  fld  [eax + 4]
  fmul st, st
  fadd
  fld  [eax + 8]
  fmul st, st
  fadd
  fld  [eax + 12]
  fmul st, st
  fadd
  fsqrt
  fld1
  fdivr
  fld  st
  fmul [eax]
  fstp [edx]
  fld  st
  fmul [eax + 4]
  fstp [edx + 4]
  fld  st
  fmul [eax + 8]
  fstp [edx + 8]
  fmul [eax + 12]
  fstp [edx + 12]
end;

procedure pNormalize(var a : TVector3); register;
// returns normalized vector a = (a / |a|)
asm
  fld  [eax]
  fmul st, st
  fld  [eax + 4]
  fmul st, st
  fadd
  fld  [eax + 8]
  fmul st, st
  fadd
  fsqrt
  fld1
  fdivr
  fld  st
  fmul [eax]
  fstp [eax]
  fld  st
  fmul [eax + 4]
  fstp [eax + 4]
  fmul [eax + 8]
  fstp [eax + 8]
end;

procedure pNormalize(var a : TVector4); register;
// returns normalized vector a = (a / |a|)
asm
  fld  [eax]
  fmul st, st
  fld  [eax + 4]
  fmul st, st
  fadd
  fld  [eax + 8]
  fmul st, st
  fadd
  fld  [eax + 12]
  fmul st, st
  fadd
  fsqrt
  fld1
  fdivr
  fld  st
  fmul [eax]
  fstp [eax]
  fld  st
  fmul [eax + 4]
  fstp [eax + 4]
  fld  st
  fmul [eax + 8]
  fstp [eax + 8]
  fmul [eax + 12]
  fstp [eax + 12]
end;

procedure pNormalize(out r : TVector3; const a : TVector3); register;
// returns normalized vector r = (a / |a|)
asm
  fld  [edx]
  fmul st, st
  fld  [edx + 4]
  fmul st, st
  fadd
  fld  [edx + 8]
  fmul st, st
  fadd
  fsqrt
  fld1
  fdivr
  fld  st
  fmul [edx]
  fstp [eax]
  fld  st
  fmul [edx + 4]
  fstp [eax + 4]
  fmul [edx + 8]
  fstp [eax + 8]
end;

procedure pNormalize(out r : TVector4; const a : TVector4); register;
// returns normalized vector r = (a / |a|)
asm
  fld  [edx]
  fmul st, st
  fld  [edx + 4]
  fmul st, st
  fadd
  fld  [edx + 8]
  fmul st, st
  fadd
  fld  [edx + 12]
  fmul st, st
  fadd
  fsqrt
  fld1
  fdivr
  fld  st
  fmul [edx]
  fstp [eax]
  fld  st
  fmul [edx + 4]
  fstp [eax + 4]
  fld  st
  fmul [edx + 8]
  fstp [eax + 8]
  fmul [edx + 12]
  fstp [eax + 12]
end;

procedure p3DNowVectorScale(var a : TVector3; k : TReal); register;
// returns scaled vector a = ka
asm
  movd mm2, [ebp + 8]
  movq mm0, [eax]
  punpckldq mm2, mm2
  movq mm1, [eax + 8]
  pfmul mm0, mm2
  pfmul mm1, mm2
  movq [eax], mm0
  movq [eax + 8], mm1
  femms
end;

procedure p3DNowVectorScale(var a : TVector4; k : TReal); register;
// returns scaled vector a = ka
asm
  movd mm2, [ebp + 8]
  movq mm0, [eax]
  punpckldq mm2, mm2
  movq mm1, [eax + 8]
  pfmul mm0, mm2
  pfmul mm1, mm2
  movq [eax], mm0
  movq [eax + 8], mm1
  femms
end;

function  fVectorScale(const a : TVector3; k : TReal) : TVector3; register;
// returns scaled vector ka
asm
  fld  [ebp + 8]
  fld  st
  fmul [eax]
  fstp [edx]
  fld  st
  fmul [eax + 4]
  fstp [edx + 4]
  fmul [eax + 8]
  fstp [edx + 8]
end;

function  fVectorScale(const a : TVector4; k : TReal) : TVector4; register;
// returns scaled vector ka
asm
  fld  [ebp + 8]
  fld  st
  fmul [eax]
  fstp [edx]
  fld  st
  fmul [eax + 4]
  fstp [edx + 4]
  fld  st
  fmul [eax + 8]
  fstp [edx + 8]
  fmul [eax + 12]
  fstp [edx + 12]
end;

procedure pVectorScale(var a : TVector3; k : TReal); register;
// returns scaled vector a = ka
asm
  fld  [ebp + 8]
  fld  st
  fmul [eax]
  fstp [eax]
  fld  st
  fmul [eax + 4]
  fstp [eax + 4]
  fmul [eax + 8]
  fstp [eax + 8]
end;

procedure pVectorScale(var a : TVector4; k : TReal); register;
// returns scaled vector a = ka
asm
  fld  [ebp + 8]
  fld  st
  fmul [eax]
  fstp [eax]
  fld  st
  fmul [eax + 4]
  fstp [eax + 4]
  fld  st
  fmul [eax + 8]
  fstp [eax + 8]
  fmul [eax + 12]
  fstp [eax + 12]
end;

procedure p3DNowVectorLERP(out r : TVector3; const a, b : TVector3; k : TReal); register;
// returns vector interpolation r = a + k(b - a)
asm
  movd  mm7, [ebp + 8]
  movq  mm0, [edx]
  movq  mm1, [ecx]
  punpckldq mm7, mm7
  pfsub mm1, mm0
  movd  mm2, [edx+8]
  pfmul mm1, mm7
  pfadd mm0, mm1
  movq  mm3, [ecx+8]
  pfsub mm3, mm2
  pfmul mm3, mm7
  movq  [eax], mm0
  pfadd mm2, mm3
  movd  [eax+8], mm2
  femms
end;

procedure p3DNowVectorLERP(out r : TVector4; const a, b : TVector4; k : TReal); register;
// returns vector interpolation r = a + k(b - a)
asm
  movd  mm7, [ebp + 8]
  movq  mm0, [edx]
  movq  mm1, [ecx]
  punpckldq mm7, mm7
  pfsub mm1, mm0
  movd  mm2, [edx+8]
  pfmul mm1, mm7
  pfadd mm0, mm1
  movq  mm3, [ecx+8]
  pfsub mm3, mm2
  pfmul mm3, mm7
  movq  [eax], mm0
  pfadd mm2, mm3
  movd  [eax+8], mm2
  femms
end;

function fVectorLERP(const a, b : TVector3; k : TReal) : TVector3; overload;
// returns vector interpolation a + k(b - a)
asm
  fld  [ebp + 8]
  fld  st
  fld  [edx]
  fsub [eax]
  fmul
  fadd [eax]
  fstp [ecx]

  fld  st
  fld  [edx + 4]
  fsub [eax + 4]
  fmul
  fadd [eax + 4]
  fstp [ecx + 4]

  fld  [edx + 8]
  fsub [eax + 8]
  fmul
  fadd [eax + 8]
  fstp [ecx + 8]
end;

function fVectorLERP(const a, b : TVector4; k : TReal) : TVector4; overload;
// returns vector interpolation a + k(b - a)
asm
  fld  [ebp + 8]
  fld  st
  fld  [edx]
  fsub [eax]
  fmul
  fadd [eax]
  fstp [ecx]

  fld  st
  fld  [edx + 4]
  fsub [eax + 4]
  fmul
  fadd [eax + 4]
  fstp [ecx + 4]

  fld  st
  fld  [edx + 8]
  fsub [eax + 8]
  fmul
  fadd [eax + 8]
  fstp [ecx + 8]

  fld  [edx + 12]
  fsub [eax + 12]
  fmul
  fadd [eax + 12]
  fstp [ecx + 12]
end;

procedure pVectorLERP(out r : TVector3; const a, b : TVector3; k : TReal); register;
// returns vector interpolation r = a + k(b - a)
asm
  fld  [ebp + 8]
  fld  st
  fld  [ecx]
  fsub [edx]
  fmul
  fadd [edx]
  fstp [eax]

  fld  st
  fld  [ecx + 4]
  fsub [edx + 4]
  fmul
  fadd [edx + 4]
  fstp [eax + 4]

  fld  [ecx + 8]
  fsub [edx + 8]
  fmul
  fadd [edx + 8]
  fstp [eax + 8]
end;

procedure pVectorLERP(out r : TVector4; const a, b : TVector4; k : TReal); register;
// returns vector interpolation r = a + k(b - a)
asm
  fld  [ebp + 8]
  fld  st
  fld  [ecx]
  fsub [edx]
  fmul
  fadd [edx]
  fstp [eax]

  fld  st
  fld  [ecx + 4]
  fsub [edx + 4]
  fmul
  fadd [edx + 4]
  fstp [eax + 4]

  fld  st
  fld  [ecx + 8]
  fsub [edx + 8]
  fmul
  fadd [edx + 8]
  fstp [eax + 8]

  fld  [ecx + 12]
  fsub [edx + 12]
  fmul
  fadd [edx + 12]
  fstp [eax + 12]
end;

procedure p3DNowMatrixAdd(out R : TMatrix3; const M, N : TMatrix3); register;
// returns matrix R = M + N
asm
  movq  mm1, [ecx]
  movq  mm0, [edx]
  movq  mm3, [ecx + 8]
  movq  mm2, [edx + 8]
  pfadd mm1, mm0
  pfadd mm3, mm2
  movq  mm4, [ecx + 16]
  movq  mm5, [edx + 16]
  movq  [eax], mm1
  movq  mm6, [ecx + 24]
  movq  mm7, [edx + 24]
  movq  [eax + 8], mm3
  pfadd mm4, mm5
  pfadd mm6, mm7
  movq  mm0, [ecx + 32]
  movq  mm1, [edx + 32]
  movq  [eax + 16], mm4
  movq  mm2, [ecx + 40]
  movq  mm3, [edx + 40]
  movq  [eax + 24], mm6
  pfadd mm0, mm1
  pfadd mm2, mm3
  movq  [eax + 32], mm0
  movq  [eax + 40], mm2
  femms
end;

procedure p3DNowMatrixAdd(out R : TMatrix4; const M, N : TMatrix4); register;
// returns matrix R = M + N
asm
  movq  mm1, [ecx]
  movq  mm0, [edx]
  movq  mm3, [ecx + 8]
  movq  mm2, [edx + 8]
  pfadd mm1, mm0
  pfadd mm3, mm2
  movq  mm4, [ecx + 16]
  movq  mm5, [edx + 16]
  movq  [eax], mm1
  movq  mm6, [ecx + 24]
  movq  mm7, [edx + 24]
  movq  [eax + 8], mm3
  pfadd mm4, mm5
  pfadd mm6, mm7
  movq  mm0, [ecx + 32]
  movq  mm1, [edx + 32]
  movq  [eax + 16], mm4
  movq  mm2, [ecx + 40]
  movq  mm3, [edx + 40]
  movq  [eax + 24], mm6
  pfadd mm0, mm1
  pfadd mm2, mm3
  movq  mm4, [ecx + 48]
  movq  mm5, [edx + 48]
  movq  [eax + 32], mm0
  movq  mm6, [ecx + 56]
  movq  mm7, [edx + 56]
  movq  [eax + 40], mm2
  pfadd mm4, mm5
  pfadd mm6, mm7
  movq  [eax + 48], mm4
  movq  [eax + 56], mm6
  femms
end;

function fMatrixAdd(const M, N : TMatrix3) : TMatrix3; overload;
asm
  fld  [eax]
  fadd [edx]
  fstp [ecx]
  fld  [eax + 4]
  fadd [edx + 4]
  fstp [ecx + 4]
  fld  [eax + 8]
  fadd [edx + 8]
  fstp [ecx + 8]
  fld  [eax + 16]
  fadd [edx + 16]
  fstp [ecx + 16]
  fld  [eax + 20]
  fadd [edx + 20]
  fstp [ecx + 20]
  fld  [eax + 24]
  fadd [edx + 24]
  fstp [ecx + 24]
  fld  [eax + 32]
  fadd [edx + 32]
  fstp [ecx + 32]
  fld  [eax + 36]
  fadd [edx + 36]
  fstp [ecx + 36]
  fld  [eax + 40]
  fadd [edx + 40]
  fstp [ecx + 40]
end;

function fMatrixAdd(const M, N : TMatrix4) : TMatrix4; overload;
asm
  fld  [eax]
  fadd [edx]
  fstp [ecx]
  fld  [eax + 4]
  fadd [edx + 4]
  fstp [ecx + 4]
  fld  [eax + 8]
  fadd [edx + 8]
  fstp [ecx + 8]
  fld  [eax + 12]
  fadd [edx + 12]
  fstp [ecx + 12]
  fld  [eax + 16]
  fadd [edx + 16]
  fstp [ecx + 16]
  fld  [eax + 20]
  fadd [edx + 20]
  fstp [ecx + 20]
  fld  [eax + 24]
  fadd [edx + 24]
  fstp [ecx + 24]
  fld  [eax + 28]
  fadd [edx + 28]
  fstp [ecx + 28]
  fld  [eax + 32]
  fadd [edx + 32]
  fstp [ecx + 32]
  fld  [eax + 36]
  fadd [edx + 36]
  fstp [ecx + 36]
  fld  [eax + 40]
  fadd [edx + 40]
  fstp [ecx + 40]
  fld  [eax + 44]
  fadd [edx + 44]
  fstp [ecx + 44]
  fld  [eax + 48]
  fadd [edx + 48]
  fstp [ecx + 48]
  fld  [eax + 52]
  fadd [edx + 52]
  fstp [ecx + 52]
  fld  [eax + 56]
  fadd [edx + 56]
  fstp [ecx + 56]
  fld  [eax + 60]
  fadd [edx + 60]
  fstp [ecx + 60]
end;

procedure pMatrixAdd(out R : TMatrix3; const M, N : TMatrix3); register;
asm
  fld  [edx]
  fadd [ecx]
  fstp [eax]
  fld  [edx + 4]
  fadd [ecx + 4]
  fstp [eax + 4]
  fld  [edx + 8]
  fadd [ecx + 8]
  fstp [eax + 8]
  fld  [edx + 16]
  fadd [ecx + 16]
  fstp [eax + 16]
  fld  [edx + 20]
  fadd [ecx + 20]
  fstp [eax + 20]
  fld  [edx + 24]
  fadd [ecx + 24]
  fstp [eax + 24]
  fld  [edx + 32]
  fadd [ecx + 32]
  fstp [eax + 32]
  fld  [edx + 36]
  fadd [ecx + 36]
  fstp [eax + 36]
  fld  [edx + 40]
  fadd [ecx + 40]
  fstp [eax + 40]
end;

procedure pMatrixAdd(out R : TMatrix4; const M, N : TMatrix4); register;
asm
  fld  [edx]
  fadd [ecx]
  fstp [eax]
  fld  [edx + 4]
  fadd [ecx + 4]
  fstp [eax + 4]
  fld  [edx + 8]
  fadd [ecx + 8]
  fstp [eax + 8]
  fld  [edx + 12]
  fadd [ecx + 12]
  fstp [eax + 12]
  fld  [edx + 16]
  fadd [ecx + 16]
  fstp [eax + 16]
  fld  [edx + 20]
  fadd [ecx + 20]
  fstp [eax + 20]
  fld  [edx + 24]
  fadd [ecx + 24]
  fstp [eax + 24]
  fld  [edx + 28]
  fadd [ecx + 28]
  fstp [eax + 28]
  fld  [edx + 32]
  fadd [ecx + 32]
  fstp [eax + 32]
  fld  [edx + 36]
  fadd [ecx + 36]
  fstp [eax + 36]
  fld  [edx + 40]
  fadd [ecx + 40]
  fstp [eax + 40]
  fld  [edx + 44]
  fadd [ecx + 44]
  fstp [eax + 44]
  fld  [edx + 48]
  fadd [ecx + 48]
  fstp [eax + 48]
  fld  [edx + 52]
  fadd [ecx + 52]
  fstp [eax + 52]
  fld  [edx + 56]
  fadd [ecx + 56]
  fstp [eax + 56]
  fld  [edx + 60]
  fadd [ecx + 60]
  fstp [eax + 60]
end;

procedure p3DNowMatrixSub(out R : TMatrix3; const M, N : TMatrix3); register;
// returns matrix R = M + N
asm
  movq  mm1, [edx]
  movq  mm0, [ecx]
  movq  mm3, [edx + 8]
  movq  mm2, [ecx + 8]
  pfsub mm1, mm0
  pfsub mm3, mm2
  movq  mm4, [edx + 16]
  movq  mm5, [ecx + 16]
  movq  [eax], mm1
  movq  mm6, [edx + 24]
  movq  mm7, [ecx + 24]
  movq  [eax + 8], mm3
  pfsub mm4, mm5
  pfsub mm6, mm7
  movq  mm0, [edx + 32]
  movq  mm1, [ecx + 32]
  movq  [eax + 16], mm4
  movq  mm2, [edx + 40]
  movq  mm3, [ecx + 40]
  movq  [eax + 24], mm6
  pfsub mm0, mm1
  pfsub mm2, mm3
  movq  [eax + 32], mm0
  movq  [eax + 40], mm2
  femms
end;

procedure p3DNowMatrixSub(out R : TMatrix4; const M, N : TMatrix4); register;
// returns matrix R = M + N
asm
  movq  mm1, [edx]
  movq  mm0, [ecx]
  movq  mm3, [edx + 8]
  movq  mm2, [ecx + 8]
  pfsub mm1, mm0
  pfsub mm3, mm2
  movq  mm4, [edx + 16]
  movq  mm5, [ecx + 16]
  movq  [eax], mm1
  movq  mm6, [edx + 24]
  movq  mm7, [ecx + 24]
  movq  [eax + 8], mm3
  pfsub mm4, mm5
  pfsub mm6, mm7
  movq  mm0, [edx + 32]
  movq  mm1, [ecx + 32]
  movq  [eax + 16], mm4
  movq  mm2, [edx + 40]
  movq  mm3, [ecx + 40]
  movq  [eax + 24], mm6
  pfsub mm0, mm1
  pfsub mm2, mm3
  movq  mm4, [edx + 48]
  movq  mm5, [ecx + 48]
  movq  [eax + 32], mm0
  movq  mm6, [edx + 56]
  movq  mm7, [ecx + 56]
  movq  [eax + 40], mm2
  pfsub mm4, mm5
  pfsub mm6, mm7
  movq  [eax + 48], mm4
  movq  [eax + 56], mm6
  femms
end;

function fMatrixSub(const M, N : TMatrix3) : TMatrix3; overload;
asm
  fld  [eax]
  fsub [edx]
  fstp [ecx]
  fld  [eax + 4]
  fsub [edx + 4]
  fstp [ecx + 4]
  fld  [eax + 8]
  fsub [edx + 8]
  fstp [ecx + 8]
  fld  [eax + 16]
  fsub [edx + 16]
  fstp [ecx + 16]
  fld  [eax + 20]
  fsub [edx + 20]
  fstp [ecx + 20]
  fld  [eax + 24]
  fsub [edx + 24]
  fstp [ecx + 24]
  fld  [eax + 32]
  fsub [edx + 32]
  fstp [ecx + 32]
  fld  [eax + 36]
  fsub [edx + 36]
  fstp [ecx + 36]
  fld  [eax + 40]
  fsub [edx + 40]
  fstp [ecx + 40]
end;

function fMatrixSub(const M, N : TMatrix4) : TMatrix4; overload;
asm
  fld  [eax]
  fsub [edx]
  fstp [ecx]
  fld  [eax + 4]
  fsub [edx + 4]
  fstp [ecx + 4]
  fld  [eax + 8]
  fsub [edx + 8]
  fstp [ecx + 8]
  fld  [eax + 12]
  fsub [edx + 12]
  fstp [ecx + 12]
  fld  [eax + 16]
  fsub [edx + 16]
  fstp [ecx + 16]
  fld  [eax + 20]
  fsub [edx + 20]
  fstp [ecx + 20]
  fld  [eax + 24]
  fsub [edx + 24]
  fstp [ecx + 24]
  fld  [eax + 28]
  fsub [edx + 28]
  fstp [ecx + 28]
  fld  [eax + 32]
  fsub [edx + 32]
  fstp [ecx + 32]
  fld  [eax + 36]
  fsub [edx + 36]
  fstp [ecx + 36]
  fld  [eax + 40]
  fsub [edx + 40]
  fstp [ecx + 40]
  fld  [eax + 44]
  fsub [edx + 44]
  fstp [ecx + 44]
  fld  [eax + 48]
  fsub [edx + 48]
  fstp [ecx + 48]
  fld  [eax + 52]
  fsub [edx + 52]
  fstp [ecx + 52]
  fld  [eax + 56]
  fsub [edx + 56]
  fstp [ecx + 56]
  fld  [eax + 60]
  fsub [edx + 60]
  fstp [ecx + 60]
end;

procedure pMatrixSub(out R : TMatrix3; const M, N : TMatrix3); register;
asm
  fld  [edx]
  fsub [ecx]
  fstp [eax]
  fld  [edx + 4]
  fsub [ecx + 4]
  fstp [eax + 4]
  fld  [edx + 8]
  fsub [ecx + 8]
  fstp [eax + 8]
  fld  [edx + 16]
  fsub [ecx + 16]
  fstp [eax + 16]
  fld  [edx + 20]
  fsub [ecx + 20]
  fstp [eax + 20]
  fld  [edx + 24]
  fsub [ecx + 24]
  fstp [eax + 24]
  fld  [edx + 32]
  fsub [ecx + 32]
  fstp [eax + 32]
  fld  [edx + 36]
  fsub [ecx + 36]
  fstp [eax + 36]
  fld  [edx + 40]
  fsub [ecx + 40]
  fstp [eax + 40]
end;

procedure pMatrixSub(out R : TMatrix4; const M, N : TMatrix4); register;
asm
  fld  [edx]
  fsub [ecx]
  fstp [eax]
  fld  [edx + 4]
  fsub [ecx + 4]
  fstp [eax + 4]
  fld  [edx + 8]
  fsub [ecx + 8]
  fstp [eax + 8]
  fld  [edx + 12]
  fsub [ecx + 12]
  fstp [eax + 12]
  fld  [edx + 16]
  fsub [ecx + 16]
  fstp [eax + 16]
  fld  [edx + 20]
  fsub [ecx + 20]
  fstp [eax + 20]
  fld  [edx + 24]
  fsub [ecx + 24]
  fstp [eax + 24]
  fld  [edx + 28]
  fsub [ecx + 28]
  fstp [eax + 28]
  fld  [edx + 32]
  fsub [ecx + 32]
  fstp [eax + 32]
  fld  [edx + 36]
  fsub [ecx + 36]
  fstp [eax + 36]
  fld  [edx + 40]
  fsub [ecx + 40]
  fstp [eax + 40]
  fld  [edx + 44]
  fsub [ecx + 44]
  fstp [eax + 44]
  fld  [edx + 48]
  fsub [ecx + 48]
  fstp [eax + 48]
  fld  [edx + 52]
  fsub [ecx + 52]
  fstp [eax + 52]
  fld  [edx + 56]
  fsub [ecx + 56]
  fstp [eax + 56]
  fld  [edx + 60]
  fsub [ecx + 60]
  fstp [eax + 60]
end;

procedure p3DNowMatrixScale(var A : TMatrix3; k : TReal); register;
// returns scaled vector a = ka
asm
  movd  mm7, [ebp + 8]
  movq  mm0, [eax]
  punpckldq mm7, mm7
  movq  mm1, [eax + 8]
  pfmul mm0, mm7
  movq  mm2, [eax + 16]
  pfmul mm1, mm7
  movq  [eax], mm0
  movq  mm4, [eax + 24]
  pfmul mm2, mm7
  movq  [eax + 8], mm1
  pfmul mm4, mm7
  movq  mm0, [eax + 32]
  movq  [eax + 16], mm2
  movq  mm1, [eax + 40]
  pfmul mm0, mm7
  movq  [eax + 24], mm4
  pfmul mm1, mm7
  movq  [eax + 32], mm0
  movq  [eax + 40], mm1
  femms
end;

procedure p3DNowMatrixScale(var A : TMatrix4; k : TReal); register;
// returns scaled matrix A = kA
asm
  movd  mm7, [ebp + 8]
  movq  mm0, [eax]
  punpckldq mm7, mm7
  movq  mm1, [eax + 8]
  pfmul mm0, mm7
  movq  mm2, [eax + 16]
  pfmul mm1, mm7
  movq  [eax], mm0
  movq  mm4, [eax + 24]
  pfmul mm2, mm7
  movq  [eax + 8], mm1
  pfmul mm4, mm7
  movq  mm0, [eax + 32]
  movq  [eax + 16], mm2
  movq  mm1, [eax + 40]
  pfmul mm0, mm7
  movq  [eax + 24], mm4
  movq  mm2, [eax + 48]
  pfmul mm1, mm7
  movq  mm4, [eax + 56]
  movq  [eax + 32], mm0
  pfmul mm2, mm7
  movq  [eax + 40], mm1
  pfmul mm4, mm7
  movq  [eax + 48], mm2
  movq  [eax + 56], mm4
  femms
end;

procedure pMatrixScale(var A : TMatrix3; k : TReal);
asm
  fld  [ebp + 8]
  fld  st
  fmul [eax]
  fstp [eax]
  fld  st
  fmul [eax + 4]
  fstp [eax + 4]
  fld  st
  fmul [eax + 8]
  fstp [eax + 8]

  fld  st
  fmul [eax + 16]
  fstp [eax + 16]
  fld  st
  fmul [eax + 20]
  fstp [eax + 20]
  fld  st
  fmul [eax + 24]
  fstp [eax + 24]

  fld  st
  fmul [eax + 32]
  fstp [eax + 32]
  fld  st
  fmul [eax + 36]
  fstp [eax + 36]
  fmul [eax + 40]
  fstp [eax + 40]
end;

procedure pMatrixScale(var A : TMatrix4; k : TReal);
asm
  fld  [ebp + 8]
  fld  st
  fmul [eax]
  fstp [eax]
  fld  st
  fmul [eax + 4]
  fstp [eax + 4]
  fld  st
  fmul [eax + 8]
  fstp [eax + 8]
  fld  st
  fmul [eax + 12]
  fstp [eax + 12]

  fld  st
  fmul [eax + 16]
  fstp [eax + 16]
  fld  st
  fmul [eax + 20]
  fstp [eax + 20]
  fld  st
  fmul [eax + 24]
  fstp [eax + 24]
  fld  st
  fmul [eax + 28]
  fstp [eax + 28]

  fld  st
  fmul [eax + 32]
  fstp [eax + 32]
  fld  st
  fmul [eax + 36]
  fstp [eax + 36]
  fld  st
  fmul [eax + 40]
  fstp [eax + 40]
  fld  st
  fmul [eax + 44]
  fstp [eax + 44]

  fld  st
  fmul [eax + 48]
  fstp [eax + 48]
  fld  st
  fmul [eax + 52]
  fstp [eax + 52]
  fld  st
  fmul [eax + 56]
  fstp [eax + 56]
  fmul [eax + 60]
  fstp [eax + 60]
end;

procedure pSSETranspose(var R : TMatrix3); register;
// still open to improvement!! (currently performs full 4x4 transpose)
asm
  movaps xmm0, [eax]       // a b c d
  movaps xmm1, [eax + 16]  // e f g h
  movaps xmm2, [eax + 32]  // i j k l
  movaps xmm3, [eax + 48]  // m n o p
  movaps xmm6, xmm0        // a b c d
  movaps xmm7, xmm1        // e f g h

  unpckhps xmm0, xmm2      // a i b j
  unpckhps xmm1, xmm3      // e m f n
  movaps   xmm4, xmm0      // a i b j
  unpckhps xmm0, xmm1      // a e i m ---
  unpcklps xmm4, xmm1      // b f j n ---

  unpcklps xmm6, xmm2      // c k d l
  unpcklps xmm7, xmm3      // g o h p
  movaps   xmm5, xmm6      // c k d l
  unpckhps xmm6, xmm7      // c g k o ---
  unpcklps xmm5, xmm7      // d h l p ---

  movaps [eax], xmm5
  movaps [eax + 16], xmm6
  movaps [eax + 32], xmm4
  movaps [eax + 48], xmm0
end;

procedure pSSETranspose(var R : TMatrix4); register;
asm
  movaps xmm0, [eax]       // a b c d
  movaps xmm1, [eax + 16]  // e f g h
  movaps xmm2, [eax + 32]  // i j k l
  movaps xmm3, [eax + 48]  // m n o p
  movaps xmm6, xmm0        // a b c d
  movaps xmm7, xmm1        // e f g h

  unpckhps xmm0, xmm2      // a i b j
  unpckhps xmm1, xmm3      // e m f n
  movaps   xmm4, xmm0      // a i b j
  unpckhps xmm0, xmm1      // a e i m ---
  unpcklps xmm4, xmm1      // b f j n ---

  unpcklps xmm6, xmm2      // c k d l
  unpcklps xmm7, xmm3      // g o h p
  movaps   xmm5, xmm6      // c k d l
  unpckhps xmm6, xmm7      // c g k o ---
  unpcklps xmm5, xmm7      // d h l p ---

  movaps [eax], xmm5
  movaps [eax + 16], xmm6
  movaps [eax + 32], xmm4
  movaps [eax + 48], xmm0
end;

procedure pSSETranspose(out R : TMatrix3; const A : TMatrix3); overload;
// still open to improvement!! (currently performs full 4x4 transpose)
asm
  movaps xmm0, [edx]       // a b c d
  movaps xmm1, [edx + 16]  // e f g h
  movaps xmm2, [edx + 32]  // i j k l
  movaps xmm3, [edx + 48]  // m n o p
  movaps xmm6, xmm0        // a b c d
  movaps xmm7, xmm1        // e f g h

  unpckhps xmm0, xmm2      // a i b j
  unpckhps xmm1, xmm3      // e m f n
  movaps   xmm4, xmm0      // a i b j
  unpckhps xmm0, xmm1      // a e i m ---
  unpcklps xmm4, xmm1      // b f j n ---

  unpcklps xmm6, xmm2      // c k d l
  unpcklps xmm7, xmm3      // g o h p
  movaps   xmm5, xmm6      // c k d l
  unpckhps xmm6, xmm7      // c g k o ---
  unpcklps xmm5, xmm7      // d h l p ---

  movaps [eax], xmm5
  movaps [eax + 16], xmm6
  movaps [eax + 32], xmm4
  movaps [eax + 48], xmm0
end;

procedure pSSETranspose(out R : TMatrix4; const A : TMatrix4); overload;
asm
  movaps xmm0, [edx]       // a b c d
  movaps xmm1, [edx + 16]  // e f g h
  movaps xmm2, [edx + 32]  // i j k l
  movaps xmm3, [edx + 48]  // m n o p
  movaps xmm6, xmm0        // a b c d
  movaps xmm7, xmm1        // e f g h

  unpckhps xmm0, xmm2      // a i b j
  unpckhps xmm1, xmm3      // e m f n
  movaps   xmm4, xmm0      // a i b j
  unpckhps xmm0, xmm1      // a e i m ---
  unpcklps xmm4, xmm1      // b f j n ---

  unpcklps xmm6, xmm2      // c k d l
  unpcklps xmm7, xmm3      // g o h p
  movaps   xmm5, xmm6      // c k d l
  unpckhps xmm6, xmm7      // c g k o ---
  unpcklps xmm5, xmm7      // d h l p ---

  movaps [eax], xmm5
  movaps [eax + 16], xmm6
  movaps [eax + 32], xmm4
  movaps [eax + 48], xmm0
end;

function fTranspose(const R : TMatrix3) : TMatrix3;
asm
  push ebx
  mov ebx, [eax]
  mov ecx, [eax + 20]
  mov [edx], ebx
  mov [edx + 20], ecx
  mov ecx, [eax + 4]
  mov ebx, [eax + 16]
  mov [edx + 16], ecx
  mov [edx + 4], ebx
  mov ecx, [eax + 8]
  mov ebx, [eax + 32]
  mov [edx + 32], ecx
  mov [edx + 8], ebx
  mov ecx, [eax + 24]
  mov ebx, [eax + 36]
  mov [edx + 36], ecx
  mov [edx + 24], ebx
  mov ecx, [eax + 40]
  mov [edx + 40], ecx
  pop ebx
end;

function fTranspose(const R : TMatrix4) : TMatrix4; register;
asm
  push ebx
  mov ebx, [eax]
  mov ecx, [eax + 20]
  mov [edx], ebx
  mov [edx+ 20], ecx
  mov ebx, [eax + 40]
  mov ecx, [eax + 60]
  mov [edx + 40], ebx
  mov [edx + 60], ecx
  mov ecx, [eax + 4]
  mov ebx, [eax + 16]
  mov [edx + 16], ecx
  mov [edx + 4], ebx
  mov ecx, [eax + 8]
  mov ebx, [eax + 32]
  mov [edx + 32], ecx
  mov [edx + 8], ebx
  mov ecx, [eax + 24]
  mov ebx, [eax + 36]
  mov [edx + 36], ecx
  mov [edx + 24], ebx

  mov ecx, [eax + 12]
  mov ebx, [eax + 48]
  mov [edx + 48], ecx
  mov [edx + 12], ebx
  mov ecx, [eax + 28]
  mov ebx, [eax + 52]
  mov [edx + 52], ecx
  mov [edx + 28], ebx
  mov ecx, [eax + 44]
  mov ebx, [eax + 56]
  mov [edx + 56], ecx
  mov [edx + 44], ebx
  pop ebx
end;

procedure pTranspose(var R : TMatrix3); register;
asm
  mov ecx, [eax + 4]
  mov edx, [eax + 16]
  mov [eax + 16], ecx
  mov [eax + 4], edx
  mov ecx, [eax + 8]
  mov edx, [eax + 32]
  mov [eax + 32], ecx
  mov [eax + 8], edx
  mov ecx, [eax + 24]
  mov edx, [eax + 36]
  mov [eax + 36], ecx
  mov [eax + 24], edx
end;

procedure pTranspose(var R : TMatrix4); register;
asm
  push ebx
  mov ecx, [eax + 4]
  mov edx, [eax + 16]
  mov [eax + 16], ecx
  mov [eax + 4], edx
  mov ecx, [eax + 8]
  mov edx, [eax + 32]
  mov [eax + 32], ecx
  mov [eax + 8], edx
  mov ecx, [eax + 24]
  mov edx, [eax + 36]
  mov [eax + 36], ecx
  mov [eax + 24], edx

  mov ecx, [eax + 12]
  mov edx, [eax + 48]
  mov [eax + 48], ecx
  mov [eax + 12], edx
  mov ecx, [eax + 28]
  mov edx, [eax + 52]
  mov [eax + 52], ecx
  mov [eax + 28], edx
  mov ecx, [eax + 44]
  mov edx, [eax + 56]
  mov [eax + 56], ecx
  mov [eax + 44], edx
  pop ebx
end;

procedure pTranspose(out R : TMatrix3; const A : TMatrix3); overload;
asm
  push ebx
  mov ebx, [edx]
  mov ecx, [edx + 20]
  mov [eax], ebx
  mov [eax + 20], ecx
  mov ecx, [edx + 4]
  mov ebx, [edx + 16]
  mov [eax + 16], ecx
  mov [eax + 4], ebx
  mov ecx, [edx + 8]
  mov ebx, [edx + 32]
  mov [eax + 32], ecx
  mov [eax + 8], ebx
  mov ecx, [edx + 24]
  mov ebx, [edx + 36]
  mov [eax + 36], ecx
  mov [eax + 24], ebx
  mov ecx, [eax + 40]
  mov [eax + 40], ecx
  pop ebx
end;

procedure pTranspose(out R : TMatrix4; const A : TMatrix4); overload;
asm
  push ebx
  mov ebx, [edx]
  mov ecx, [edx + 20]
  mov [eax], ebx
  mov [eax+ 20], ecx
  mov ebx, [edx + 40]
  mov ecx, [edx + 60]
  mov [eax + 40], ebx
  mov [eax + 60], ecx
  mov ecx, [edx + 4]
  mov ebx, [edx + 16]
  mov [eax + 16], ecx
  mov [eax + 4], ebx
  mov ecx, [edx + 8]
  mov ebx, [edx + 32]
  mov [eax + 32], ecx
  mov [eax + 8], ebx
  mov ecx, [edx + 24]
  mov ebx, [edx + 36]
  mov [eax + 36], ecx
  mov [eax + 24], ebx

  mov ecx, [edx + 12]
  mov ebx, [edx + 48]
  mov [eax + 48], ecx
  mov [eax + 12], ebx
  mov ecx, [edx + 28]
  mov ebx, [edx + 52]
  mov [eax + 52], ecx
  mov [eax + 28], ebx
  mov ecx, [edx + 44]
  mov ebx, [edx + 56]
  mov [eax + 56], ecx
  mov [eax + 44], ebx
  pop ebx
end;

procedure p3DNowMatrixVector(var r : TVector3; const M : TMatrix3; const a : TVector3); register;
asm
  movq      mm0, [ecx]           // b a
  movq      mm1, [ecx + 8]       // ? c
  movq      mm7, [edx + 8]       // ? C
  movq      mm2, mm0             // b a
  movq      mm6, [edx + 24]      // ? F
  punpckldq mm1, mm1             // c c
  pfmul     mm0, [edx]           // bB aA
  movq      mm3, mm2             // b a
  punpckldq mm7, mm6             // F C
  movq      mm4, mm1             // c c
  pfmul     mm3, [edx + 32]      // bH aG
  pfmul     mm1, mm7             // cF cC
  pfmul     mm2, [edx + 16]      // bE aD
  pfacc     mm3, mm3             // aG+bH  aG+bH
  pfmul     mm4, [edx + 40]      // c? cI
  pfacc     mm0, mm2             // aD+bE  aA+bB
  pfadd     mm0, mm1             // aD+bE+cF  aA+bB+cC
  pfadd     mm3, mm4             // aG+bH+c?  aG+bH+cI
  movq      [eax], mm0           // write resultant x and y
  movq      [eax + 8], mm3       // write resultant z
  femms
end;

procedure p3DNowMatrixVector(var r : TVector4; const M : TMatrix4; const a : TVector4); register;
// 188 / 266
asm
  movq  mm2, [edx]          // B A
  movq  mm3, [edx + 8]      // D C
  movq  mm0, [ecx]          // b a
  movq  mm1, [ecx + 8]      // d c

  movq  mm4, [edx + 16]     // F E
  pfmul mm2, mm0            // bB aA
  movq  mm5, [edx + 24]     // H G
  pfmul mm3, mm1            // dD cC
  pfmul mm4, mm0            // bF aE
  pfacc mm2, mm3            // bB+dD aA+cC
  pfmul mm5, mm1            // dH cG
  pfacc mm2, mm2            // bB+dD+aA+cC  bB+dD+aA+cC

  pfacc mm4, mm5            // bF+dH aE+cG
  movq  mm3, [edx + 40]     // L K
  pfacc mm4, mm4            // bF+dH+aE+cG  bF+dH+aE+cG
  movq  mm5, [edx + 56]     // P O
  psrlq mm2, 32             // 0 bB+dD+aA+cC
  psllq mm4, 32             // bF+dH+aE+cG  0
  pfacc mm2, mm4            // bF+dH+aE+cG  bB+dD+aA+cC
  pfmul mm5, mm1            // dP cO

  movq  [eax], mm2          // write resultant: a b
  movq  mm4, [edx + 48]     // N M

  pfmul mm3, mm1            // dL cK
  movq  mm2, [edx + 32]     // J I
  pfmul mm4, mm0            // bN aM
  pfmul mm2, mm0            // bJ aI
  pfacc mm4, mm5            // bN+aM bN+aM
  pfacc mm2, mm3            // dL+bJ aI+cK
  pfacc mm4, mm4            // dP+cO+bN+aM  dP+cO+bN+aM
  pfacc mm2, mm2            // dL+bJ+aI+cK  dL+bJ+aI+cK

  psrlq mm2, 32             // 0  dL+bJ+aI+cK
  psllq mm4, 32             // dP+cO+bN+aM  0
  pfacc mm2, mm4            // dP+cO+bN+aM  dL+bJ+aI+cK
  movq  [eax + 8], mm2      // write resultant: c d

  femms
end;

procedure pSSEMatrixVector(out r : TVector3; const M : TMatrix3; const a : TVector3); register;
asm
  movaps xmm0, [edx]
  movaps xmm1, [edx + 16]
  movaps xmm2, [edx + 32]
  movaps xmm3, [edx + 48]

  movaps xmm6, xmm0
  movaps xmm7, xmm1

  unpckhps xmm0, xmm2
  unpckhps xmm1, xmm3
  movaps   xmm4, xmm0
  unpcklps xmm4, xmm1

  movaps xmm1, [ecx]

  unpcklps xmm6, xmm2
  unpcklps xmm7, xmm3
  movaps   xmm5, xmm6
  unpckhps xmm6, xmm7
  unpcklps xmm5, xmm7

  movaps   xmm2, xmm1
  movaps   xmm3, xmm1
  shufps   xmm1, xmm1, $00 // broadcast x
  shufps   xmm2, xmm2, $55 // broadcast y
  shufps   xmm3, xmm3, $AA // broadcast z

  mulps	   xmm1, xmm5
  mulps	   xmm2, xmm6
  mulps	   xmm3, xmm4

  addps	   xmm1, xmm2
  addps	   xmm3, xmm1

  movaps   [eax], xmm3
end;

procedure pSSEMatrixVector(out r : TVector4; const M : TMatrix4; const a : TVector4); register;
asm
  movaps xmm0, [edx]     
  movaps xmm1, [edx + 16]
  movaps xmm2, [edx + 32]
  movaps xmm3, [edx + 48]

  movaps xmm6, xmm0      
  movaps xmm7, xmm1      

  unpckhps xmm0, xmm2    
  unpckhps xmm1, xmm3    
  movaps   xmm4, xmm0    
  unpckhps xmm0, xmm1    
  unpcklps xmm4, xmm1    

  movaps xmm1, [ecx]     

  unpcklps xmm6, xmm2    
  unpcklps xmm7, xmm3
  movaps   xmm5, xmm6
  unpckhps xmm6, xmm7
  unpcklps xmm5, xmm7

  movaps   xmm2, xmm1
  movaps   xmm3, xmm1
  movaps   xmm7, xmm1
  shufps   xmm1, xmm1, $00  // broadcast x
  shufps   xmm2, xmm2, $55  // broadcast y
  shufps   xmm3, xmm3, $AA  // broadcast z
  shufps   xmm7, xmm7, $FF  // broadcast w

  mulps	   xmm1, xmm5
  mulps	   xmm2, xmm6
  mulps	   xmm3, xmm4
  mulps	   xmm7, xmm0

  addps	   xmm1, xmm2
  addps	   xmm3, xmm7
  addps	   xmm1, xmm3

  movaps   [eax], xmm1
end;

procedure pMatrixVector(out r : TVector3; const M : TMatrix3; const a : TVector3); register;
asm
  fld   [ecx]
  fmul  [edx]
  fld   [ecx + $04]
  fmul  [edx + $04]
  faddp
  fld   [ecx + $08]
  fmul  [edx + $08]
  faddp
  fstp  [eax]

  fld   [ecx]
  fmul  [edx + $10]
  fld   [ecx + $04]
  fmul  [edx + $14]
  faddp
  fld   [ecx + $08]
  fmul  [edx + $18]
  faddp
  fstp  [eax + $04]

  fld   [ecx]
  fmul  [edx + $20]
  fld   [ecx + $04]
  fmul  [edx + $24]
  faddp
  fld   [ecx + $08]
  fmul  [edx + $28]
  faddp
  fstp  [eax + $08]
end;

procedure pMatrixVector(out r : TVector4; const M : TMatrix4; const a : TVector4); register;
asm
  fld   [ecx]
  fmul  [edx]
  fld   [ecx + $04]
  fmul  [edx + $04]
  faddp
  fld   [ecx + $08]
  fmul  [edx + $08]
  faddp
  fld   [ecx + $0c]
  fmul  [edx + $0c]
  faddp
  fstp  [eax]

  fld   [ecx]
  fmul  [edx + $10]
  fld   [ecx + $04]
  fmul  [edx + $14]
  faddp
  fld   [ecx + $08]
  fmul  [edx + $18]
  faddp
  fld   [ecx + $0c]
  fmul  [edx + $1c]
  faddp
  fstp  [eax + $04]

  fld   [ecx]
  fmul  [edx + $20]
  fld   [ecx + $04]
  fmul  [edx + $24]
  faddp
  fld   [ecx + $08]
  fmul  [edx + $28]
  faddp
  fld   [ecx + $0c]
  fmul  [edx + $2c]
  faddp
  fstp  [eax + $08]

  fld   [ecx]
  fmul  [edx + $30]
  fld   [ecx + $04]
  fmul  [edx + $34]
  faddp
  fld   [ecx + $08]
  fmul  [edx + $38]
  faddp
  fld   [ecx + $0c]
  fmul  [edx + $3c]
  faddp
  fstp  [eax + $0c]
end;

function f3DNowMatrixMul(const M, N : TMatrix3) : TMatrix3; register;
asm
  movq        mm0,[eax]
  movq        mm1,[eax+8]
  movq        mm4,[edx]
  punpckhdq   mm2,mm0
  movq        mm5,[edx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[edx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0, [edx+8]
  movq        mm7,[edx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[edx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[edx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[edx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  movq        mm0,[eax+16]
  pfadd       mm3,mm2
  movq        mm1,[eax+24]
  movq        [ecx],mm7
  movq        mm4,[edx]
  movq        [ecx+8],mm3

  punpckhdq   mm2,mm0
  movq        mm5,[edx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[edx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0,[edx+8]
  movq        mm7,[edx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[edx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[edx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[edx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  movq        mm0,[eax+32]
  pfadd       mm3,mm2
  movq        mm1,[eax+40]
  movq        [ecx+16],mm7
  movq        mm4,[edx]
  movq        [ecx+24],mm3

  punpckhdq   mm2,mm0
  movq        mm5,[edx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[edx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0,[edx+8]
  movq        mm7,[edx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[edx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[edx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[edx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  pfadd       mm3,mm2
  movq        [ecx+32],mm7
  movq        [ecx+40],mm3

  femms
end;

function f3DNowMatrixMul(const M, N : TMatrix4) : TMatrix4; register;
asm
  movq        mm0,[eax]
  movq        mm1,[eax+8]
  movq        mm4,[edx]
  punpckhdq   mm2,mm0
  movq        mm5,[edx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[edx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0, [edx+8]
  movq        mm7,[edx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[edx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[edx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[edx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  movq        mm0,[eax+16]
  pfadd       mm3,mm2
  movq        mm1,[eax+24]
  movq        [ecx],mm7
  movq        mm4,[edx]
  movq        [ecx+8],mm3

  punpckhdq   mm2,mm0
  movq        mm5,[edx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[edx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0,[edx+8]
  movq        mm7,[edx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[edx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[edx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[edx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  movq        mm0,[eax+32]
  pfadd       mm3,mm2
  movq        mm1,[eax+40]
  movq        [ecx+16],mm7
  movq        mm4,[edx]
  movq        [ecx+24],mm3

  punpckhdq   mm2,mm0
  movq        mm5,[edx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[edx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0,[edx+8]
  movq        mm7,[edx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[edx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[edx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[edx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  movq        mm0,[eax+48]
  pfadd       mm3,mm2
  movq        mm1,[eax+56]
  movq        [ecx+32],mm7
  movq        mm4,[edx]
  movq        [ecx+40],mm3

  punpckhdq   mm2,mm0
  movq        mm5,[edx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[edx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0,[edx+8]
  movq        mm7,[edx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[edx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[edx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[edx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  pfadd       mm3,mm2
  movq        [ecx+48],mm7
  movq        [ecx+56],mm3
  femms
end;

procedure p3DNowMatrixMul(out R : TMatrix3; const M, N : TMatrix3); register;
asm
  movq        mm0,[edx]
  movq        mm1,[edx+8]
  movq        mm4,[ecx]
  punpckhdq   mm2,mm0
  movq        mm5,[ecx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[ecx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0, [ecx+8]
  movq        mm7,[ecx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[ecx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[ecx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[ecx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  movq        mm0,[edx+16]
  pfadd       mm3,mm2
  movq        mm1,[edx+24]
  movq        [eax],mm7
  movq        mm4,[ecx]
  movq        [eax+8],mm3

  punpckhdq   mm2,mm0
  movq        mm5,[ecx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[ecx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0,[ecx+8]
  movq        mm7,[ecx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[ecx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[ecx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[ecx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  movq        mm0,[edx+32]
  pfadd       mm3,mm2
  movq        mm1,[edx+40]
  movq        [eax+16],mm7
  movq        mm4,[ecx]
  movq        [eax+24],mm3

  punpckhdq   mm2,mm0
  movq        mm5,[ecx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[ecx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0,[ecx+8]
  movq        mm7,[ecx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[ecx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[ecx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[ecx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  pfadd       mm3,mm2
  movq        [eax+32],mm7
  movq        [eax+40],mm3

  femms
end;

procedure p3DNowMatrixMul(out R : TMatrix4; const M, N : TMatrix4); register;
asm
  movq        mm0,[edx]
  movq        mm4,[ecx]
  movq        mm1,[edx+8]
  punpckhdq   mm2,mm0
  movq        mm5,[ecx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[ecx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0, [ecx+8]
  movq        mm7,[ecx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[ecx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[ecx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[ecx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  movq        mm0,[edx+16]
  pfadd       mm3,mm2
  movq        mm1,[edx+24]
  movq        [eax],mm7
  movq        mm4,[ecx]
  movq        [eax+8],mm3

  punpckhdq   mm2,mm0
  movq        mm5,[ecx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[ecx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0,[ecx+8]
  movq        mm7,[ecx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[ecx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[ecx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[ecx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  movq        mm0,[edx+32]
  pfadd       mm3,mm2
  movq        mm1,[edx+40]
  movq        [eax+16],mm7
  movq        mm4,[ecx]
  movq        [eax+24],mm3

  punpckhdq   mm2,mm0
  movq        mm5,[ecx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[ecx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0,[ecx+8]
  movq        mm7,[ecx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[ecx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[ecx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[ecx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  movq        mm0,[edx+48]
  pfadd       mm3,mm2
  movq        mm1,[edx+56]
  movq        [eax+32],mm7
  movq        mm4,[ecx]
  movq        [eax+40],mm3

  punpckhdq   mm2,mm0
  movq        mm5,[ecx+16]
  punpckhdq   mm3,mm1
  movq        mm6,[ecx+32]
  punpckldq   mm0,mm0
  punpckldq   mm1,mm1
  pfmul       mm4,mm0
  punpckhdq   mm2,mm2
  pfmul       mm0,[ecx+8]
  movq        mm7,[ecx+48]
  pfmul       mm5,mm2
  punpckhdq   mm3,mm3
  pfmul       mm2,[ecx+24]
  pfmul       mm6,mm1
  pfadd       mm5,mm4
  pfmul       mm1,[ecx+40]
  pfadd       mm2,mm0
  pfmul       mm7,mm3
  pfadd       mm6,mm5
  pfmul       mm3,[ecx+56]
  pfadd       mm2,mm1
  pfadd       mm7,mm6
  pfadd       mm3,mm2
  movq        [eax+48],mm7
  movq        [eax+56],mm3
  femms
end;

procedure pSSEMatrixMul(out R : TMatrix3; const M, N : TMatrix3); overload;
asm
  movaps   xmm0, [edx]
  movaps   xmm4, [ecx]
  movaps   xmm5, [ecx + 16]
  movaps   xmm6, [ecx + 32]

  movaps   xmm1, xmm0
  movaps   xmm2, xmm0
  shufps   xmm0, xmm0, $00
  shufps   xmm1, xmm1, $55
  shufps   xmm2, xmm2, $aa

  mulps    xmm0, xmm4
  mulps    xmm1, xmm5
  mulps    xmm2, xmm6
  addps    xmm0, xmm1
  movaps   xmm3, [edx + 16]
  addps    xmm2, xmm0

  movaps   xmm1, xmm3
  movaps   xmm0, xmm3

  movaps   [eax], xmm2

  shufps   xmm3, xmm3, $00
  shufps   xmm1, xmm1, $55
  shufps   xmm0, xmm0, $aa

  mulps    xmm3, xmm4
  mulps    xmm1, xmm5
  mulps    xmm0, xmm6
  addps    xmm3, xmm1
  movaps   xmm2, [edx + 32]
  addps    xmm0, xmm3

  movaps   xmm1, xmm2
  movaps   xmm3, xmm2

  movaps   [eax + 16], xmm0

  shufps   xmm2, xmm2, $00
  shufps   xmm1, xmm1, $55
  shufps   xmm3, xmm3, $aa

  mulps    xmm2, xmm4
  mulps    xmm1, xmm5
  mulps    xmm3, xmm6
  addps    xmm2, xmm1
  addps    xmm3, xmm2

  movaps   [eax + 32], xmm3
end;

procedure pSSEMatrixMulIntel(out R : TMatrix4; const M, N : TMatrix4); overload;
asm
  movss	 xmm0, dword ptr [edx]
  movaps xmm1, [ecx]
  shufps xmm0, xmm0, 0
  movss	 xmm2, dword ptr [edx+4]
  mulps	 xmm0, xmm1
  shufps xmm2, xmm2, 0
  movaps xmm3, [ecx+10h]
  movss	 xmm7, dword ptr [edx+8]
  mulps	 xmm2, xmm3
  shufps xmm7, xmm7, 0
  addps	 xmm0, xmm2
  movaps xmm4, [ecx+20h]
  movss	 xmm2, dword ptr [edx+0Ch]
  mulps	 xmm7, xmm4
  shufps xmm2, xmm2, 0
  addps	 xmm0, xmm7
  movaps xmm5, [ecx+30h]
  movss	 xmm6, dword ptr [edx+10h]
  mulps	 xmm2, xmm5
  movss	 xmm7, dword ptr [edx+14h]
  shufps xmm6, xmm6, 0
  addps	 xmm0, xmm2
  shufps xmm7, xmm7, 0
  movlps qword ptr [eax], xmm0
  movhps qword ptr [eax+8], xmm0
  mulps	 xmm7, xmm3
  movss	 xmm0, dword ptr [edx+18h]
  mulps	 xmm6, xmm1
  shufps xmm0, xmm0, 0
  addps	 xmm6, xmm7
  mulps	 xmm0, xmm4
  movss	 xmm2, dword ptr [edx+24h]
  addps	 xmm6, xmm0
  movss	 xmm0, dword ptr [edx+1Ch]
  movss	 xmm7, dword ptr [edx+20h]
  shufps xmm0, xmm0, 0
  shufps xmm7, xmm7, 0
  mulps	 xmm0, xmm5
  mulps	 xmm7, xmm1
  addps	 xmm6, xmm0
  shufps xmm2, xmm2, 0
  movlps qword ptr [eax+10h], xmm6
  movhps qword ptr [eax+18h], xmm6
  mulps	 xmm2, xmm3
  movss	 xmm6, dword ptr [edx+28h]
  addps	 xmm7, xmm2
  shufps xmm6, xmm6, 0
  movss	 xmm2, dword ptr [edx+2Ch]
  mulps	 xmm6, xmm4
  shufps xmm2, xmm2, 0
  addps	 xmm7, xmm6
  mulps	 xmm2, xmm5
  movss	 xmm0, dword ptr [edx+34h]
  addps	 xmm7, xmm2
  shufps xmm0, xmm0, 0
  movlps qword ptr [eax+20h], xmm7
  movss	 xmm2, dword ptr [edx+30h]
  movhps qword ptr [eax+28h], xmm7
  mulps	 xmm0, xmm3
  shufps xmm2, xmm2, 0
  movss	 xmm6, dword ptr [edx+38h]
  mulps	 xmm2, xmm1
  shufps xmm6, xmm6, 0
  addps	 xmm2, xmm0
  mulps	 xmm6, xmm4
  movss	 xmm7, dword ptr [edx+3Ch]
  shufps xmm7, xmm7, 0
  addps	 xmm2, xmm6
  mulps	 xmm7, xmm5
  addps	 xmm2, xmm7
  movaps [eax+30h], xmm2
end;

procedure pSSEMatrixMulModifiedIntel(out R : TMatrix4; const M, N : TMatrix4); overload;
asm
  movss	 xmm0, dword ptr [edx]
  movaps xmm1, [ecx]
  movss	 xmm2, dword ptr [edx+4]
  shufps xmm0, xmm0, 0
  mulps	 xmm0, xmm1
  movaps xmm3, [ecx+10h]
  shufps xmm2, xmm2, 0
  movss	 xmm7, dword ptr [edx+8]
  mulps	 xmm2, xmm3
  shufps xmm7, xmm7, 0
  movaps xmm4, [ecx+20h]
  addps	 xmm0, xmm2
  movss	 xmm2, dword ptr [edx+0Ch]
  mulps	 xmm7, xmm4
  shufps xmm2, xmm2, 0
  movaps xmm5, [ecx+30h]
  addps	 xmm0, xmm7
  movss	 xmm6, dword ptr [edx+10h]
  mulps	 xmm2, xmm5
  movss	 xmm7, dword ptr [edx+14h]
  shufps xmm6, xmm6, 0
  addps	 xmm0, xmm2
  shufps xmm7, xmm7, 0
  movlps qword ptr [eax], xmm0
  movhps qword ptr [eax+8], xmm0
  mulps	 xmm7, xmm3
  movss	 xmm0, dword ptr [edx+18h]
  mulps	 xmm6, xmm1
  shufps xmm0, xmm0, 0
  addps	 xmm6, xmm7
  mulps	 xmm0, xmm4
  movss	 xmm2, dword ptr [edx+24h]
  movss	 xmm7, dword ptr [edx+20h]
  addps	 xmm6, xmm0
  movss	 xmm0, dword ptr [edx+1Ch]
  shufps xmm0, xmm0, 0
  shufps xmm7, xmm7, 0
  mulps	 xmm0, xmm5
  shufps xmm2, xmm2, 0
  mulps	 xmm7, xmm1
  addps	 xmm6, xmm0
  movlps qword ptr [eax+10h], xmm6
  mulps	 xmm2, xmm3  // up?
  movhps qword ptr [eax+18h], xmm6
  movss	 xmm6, dword ptr [edx+28h]
  addps	 xmm7, xmm2
  shufps xmm6, xmm6, 0
  movss	 xmm2, dword ptr [edx+2Ch]
  mulps	 xmm6, xmm4
  shufps xmm2, xmm2, 0
  addps	 xmm7, xmm6
  mulps	 xmm2, xmm5
  movss	 xmm0, dword ptr [edx+34h]
  addps	 xmm7, xmm2
  shufps xmm0, xmm0, 0
  movss	 xmm2, dword ptr [edx+30h]
  movlps qword ptr [eax+20h], xmm7
  movhps qword ptr [eax+28h], xmm7
  shufps xmm2, xmm2, 0
  movss	 xmm6, dword ptr [edx+38h]
  mulps	 xmm0, xmm3
  mulps	 xmm2, xmm1
  shufps xmm6, xmm6, 0
  addps	 xmm2, xmm0
  mulps	 xmm6, xmm4
  movss	 xmm7, dword ptr [edx+3Ch]
  shufps xmm7, xmm7, 0
  addps	 xmm2, xmm6
  mulps	 xmm7, xmm5
  addps	 xmm2, xmm7
  movaps [eax+30h], xmm2
end;

procedure pSSEMatrixMul(out R : TMatrix4; const M, N : TMatrix4); overload;
asm
  movaps   xmm0, [edx]       // d c b a --- get first vector
  movaps   xmm4, [ecx]       // D C B A
  movaps   xmm1, xmm0        // d c b a
  movaps   xmm2, xmm0        // d c b a
  movaps   xmm5, [ecx + 16]  // H G F E
  movaps   xmm3, xmm0        // d c b a
  shufps   xmm1, xmm1, $55   // b b b b
  shufps   xmm0, xmm0, $00   // a a a a   $00 = 00 00 00 00
  movaps   xmm6, [ecx + 32]  // L K J I
  shufps   xmm2, xmm2, $aa   // c c c c   $aa = 10 10 10 10
  movaps   xmm7, [ecx + 48]  // P O N M
  shufps   xmm3, xmm3, $ff   // d d d d   $ee = 11 11 11 11
  mulps    xmm0, xmm4        // aD aC aB aA
  mulps    xmm1, xmm5        // bH bG bF bE
  mulps    xmm2, xmm6        // cL cK cJ cI
  mulps    xmm3, xmm7        // dP dO dN dM
  addps    xmm0, xmm1        // aD+bH aC+bG aB+bF aA+bE
  addps    xmm2, xmm3        // cL+dp cK+dO cJ+dN cI+dM
  movaps   xmm1, [edx + 16]  // get second vector
  addps    xmm0, xmm2        // aD+bH+cL+dp  aC+bG+cK+dO  aB+bF+cJ+dN  aA+bE+cI+dM
  movaps   xmm3, xmm1        // prepare for second cycle
  movaps   xmm2, xmm1        // prepare for second cycle
  movlps   [eax], xmm0       // store first part of result
  movhps   [eax + 8], xmm0   // store second part of result

  shufps   xmm3, xmm3, $55
  movaps   xmm0, xmm1
  shufps   xmm2, xmm2, $00
  shufps   xmm1, xmm1, $aa
  shufps   xmm0, xmm0, $ff
  mulps    xmm2, xmm4
  mulps    xmm3, xmm5
  mulps    xmm1, xmm6
  mulps    xmm0, xmm7
  addps    xmm2, xmm3
  addps    xmm1, xmm0
  movaps   xmm3, [edx + 32]  // get third vector
  addps    xmm2, xmm1
  movaps   xmm0, xmm3
  movaps   xmm1, xmm3
  movlps   [eax + 16], xmm2
  movhps   [eax + 24], xmm2

  shufps   xmm0, xmm0, $ff
  movaps   xmm2, xmm3
  shufps   xmm1, xmm1, $aa
  shufps   xmm3, xmm3, $55
  shufps   xmm2, xmm2, $00
  mulps    xmm0, xmm7
  mulps    xmm1, xmm6
  mulps    xmm3, xmm7
  mulps    xmm2, xmm4
  addps    xmm0, xmm1
  addps    xmm3, xmm2
  movaps   xmm2, [edx + 48]  // get fourth vector
  addps    xmm0, xmm1
  movaps   xmm3, xmm2
  movaps   xmm1, xmm2
  movlps   [eax + 32], xmm0
  movhps   [eax + 40], xmm0

  shufps   xmm3, xmm3, $55
  movaps   xmm0, xmm2
  shufps   xmm1, xmm1, $00
  shufps   xmm2, xmm2, $ff
  shufps   xmm0, xmm0, $aa
  mulps    xmm3, xmm5
  mulps    xmm1, xmm4
  mulps    xmm2, xmm7
  mulps    xmm0, xmm6
  addps    xmm3, xmm1
  addps    xmm2, xmm0
  addps    xmm3, xmm2
  movlps   [eax + 48], xmm3
  movhps   [eax + 56], xmm3
end;

procedure pSSE2MatrixMul(out R : TMatrix4; const M, N : TMatrix4); overload;
asm
  movaps   xmm0, [edx]       // d c b a --- get first vector
  movaps   xmm4, [ecx]       // D C B A
  movaps   xmm5, [ecx + 16]  // H G F E
  pshufd   xmm1, xmm0, $00   // a a a a
  pshufd   xmm2, xmm0, $55   // b b b b
  movaps   xmm6, [ecx + 32]  // L K J I
  pshufd   xmm3, xmm0, $aa   // c c c c
  shufps   xmm0, xmm0, $ff   // d d d d
  movaps   xmm7, [ecx + 48]  // P O N M
  mulps    xmm1, xmm4        // aD aC aB aA
  mulps    xmm2, xmm5        // bH bG bF bE
  mulps    xmm3, xmm6        // cL cK cJ cI
  mulps    xmm0, xmm7        // dP dO dN dM
  addps    xmm1, xmm2        // aD+bH aC+bG aB+bF aA+bE
  addps    xmm3, xmm0        // cL+dp cK+dO cJ+dN cI+dM
  movaps   xmm0, [edx + 16]  // get second vector
  addps    xmm1, xmm3        // aD+bH+cL+dp  aC+bG+cK+dO  aB+bF+cJ+dN  aA+bE+cI+dM
  movlps   [eax], xmm1
  movhps   [eax + 8], xmm1

  pshufd   xmm3, xmm0, $00
  pshufd   xmm2, xmm0, $55
  pshufd   xmm1, xmm0, $aa
  shufps   xmm0, xmm0, $ff
  mulps    xmm3, xmm4
  mulps    xmm2, xmm5
  mulps    xmm1, xmm6
  mulps    xmm0, xmm7
  addps    xmm3, xmm2
  addps    xmm1, xmm0
  movaps   xmm0, [edx + 32]  // get third vector
  addps    xmm3, xmm1
  movlps   [eax + 16], xmm3
  movhps   [eax + 24], xmm3

  pshufd   xmm2, xmm0, $00
  pshufd   xmm1, xmm0, $55
  pshufd   xmm3, xmm0, $aa
  shufps   xmm0, xmm0, $ff
  mulps    xmm2, xmm4
  mulps    xmm1, xmm5
  mulps    xmm3, xmm6
  mulps    xmm0, xmm7
  addps    xmm2, xmm1
  addps    xmm3, xmm0
  movaps   xmm0, [edx + 48]  // get fourth vector
  addps    xmm2, xmm3
  movlps   [eax + 32], xmm2
  movhps   [eax + 40], xmm2

  pshufd   xmm1, xmm0, $00
  pshufd   xmm3, xmm0, $55
  pshufd   xmm2, xmm0, $aa
  shufps   xmm0, xmm0, $ff
  mulps    xmm1, xmm4
  mulps    xmm3, xmm5
  mulps    xmm2, xmm6
  mulps    xmm0, xmm7
  addps    xmm1, xmm3
  addps    xmm2, xmm0
  addps    xmm1, xmm2
  movlps   [eax + 48], xmm1
  movhps   [eax + 56], xmm1
end;

procedure pSSE2MatrixMul2(out R : TMatrix4; const M, N : TMatrix4); overload;
asm
  movlps   xmm4, [ecx]
  movlps   xmm0, [edx]
  movlps   xmm5, [ecx + 16]
  movhps   xmm4, [ecx + 8]
  movhps   xmm0, [edx + 8]
  movhps   xmm5, [edx + 24]
  pshufd   xmm1, xmm0, $00
  pshufd   xmm2, xmm0, $55
  movaps   xmm6, [ecx + 32]
  pshufd   xmm3, xmm0, $aa
  movlps   xmm7, [ecx + 48]
  pshufd   xmm0, xmm0, $ff
  movhps   xmm7, [ecx + 56]
  mulps    xmm1, xmm4
  mulps    xmm2, xmm5
  mulps    xmm3, xmm6
  mulps    xmm0, xmm7
  addps    xmm1, xmm2
  addps    xmm3, xmm0
  movlps   xmm0, [edx + 16]
  movhps   xmm0, [edx + 24]
  addps    xmm1, xmm2
  movlps   [eax], xmm1
  movhps   [eax + 8], xmm1

  pshufd   xmm3, xmm0, $00
  pshufd   xmm2, xmm0, $55
  pshufd   xmm1, xmm0, $aa
  pshufd   xmm0, xmm0, $ff
  mulps    xmm3, xmm4
  mulps    xmm2, xmm5
  mulps    xmm1, xmm6
  mulps    xmm0, xmm7
  addps    xmm3, xmm2
  addps    xmm1, xmm0
  movlps   xmm0, [edx + 32]
  movhps   xmm0, [edx + 40]
  addps    xmm3, xmm1
  movlps   [eax + 16], xmm3
  movhps   [eax + 24], xmm3

  pshufd   xmm2, xmm0, $00
  pshufd   xmm1, xmm0, $55
  pshufd   xmm3, xmm0, $aa
  pshufd   xmm0, xmm0, $ff
  mulps    xmm2, xmm4
  mulps    xmm1, xmm5
  mulps    xmm3, xmm6
  mulps    xmm0, xmm7
  addps    xmm2, xmm1
  addps    xmm3, xmm0
  movlps   xmm0, [edx + 48]
  movhps   xmm0, [edx + 56]
  addps    xmm2, xmm3
  movlps   [eax + 32], xmm2
  movhps   [eax + 40], xmm2

  pshufd   xmm1, xmm0, $00
  pshufd   xmm3, xmm0, $55
  pshufd   xmm2, xmm0, $aa
  pshufd   xmm0, xmm0, $ff
  mulps    xmm1, xmm4
  mulps    xmm3, xmm5
  mulps    xmm2, xmm6
  mulps    xmm0, xmm7
  addps    xmm1, xmm3
  addps    xmm2, xmm0
  addps    xmm1, xmm2
  movlps   [eax + 48], xmm1
  movhps   [eax + 56], xmm1
end;

procedure pMatrixMul(out R : TMatrix3; const M, N : TMatrix3); register;
asm
  fld   [edx]
  fmul  [ecx]
  fld   [edx + $04]
  fmul  [ecx + $10]
  faddp
  fld   [edx + $08]
  fmul  [ecx + $20]
  faddp
  fstp  [eax]

  fld   [edx]
  fmul  [ecx + $04]
  fld   [edx + $04]
  fmul  [ecx + $14]
  faddp
  fld   [edx + $08]
  fmul  [ecx + $24]
  faddp
  fstp  [eax + $04]

  fld   [edx]
  fmul  [ecx + $08]
  fld   [edx + $04]
  fmul  [ecx + $18]
  faddp
  fld   [edx + $08]
  fmul  [ecx + $28]
  faddp
  fstp  [eax + $08]

  fld   [edx + $10]
  fmul  [ecx]
  fld   [edx + $14]
  fmul  [ecx + $10]
  faddp
  fld   [edx + $18]
  fmul  [ecx + $20]
  faddp
  fstp  [eax + $10]

  fld   [edx + $10]
  fmul  [ecx + $04]
  fld   [edx + $14]
  fmul  [ecx + $14]
  faddp
  fld   [edx + $18]
  fmul  [ecx + $24]
  faddp
  fstp  [eax + $14]

  fld   [edx + $10]
  fmul  [ecx + $08]
  fld   [edx + $14]
  fmul  [ecx + $18]
  faddp
  fld   [edx + $18]
  fmul  [ecx + $28]
  faddp
  fstp  [eax + $18]

  fld   [edx + $20]
  fmul  [ecx]
  fld   [edx + $24]
  fmul  [ecx + $10]
  faddp
  fld   [edx + $28]
  fmul  [ecx + $20]
  faddp
  fstp  [eax + $20]

  fld   [edx + $20]
  fmul  [ecx + $04]
  fld   [edx + $24]
  fmul  [ecx + $14]
  faddp
  fld   [edx + $28]
  fmul  [ecx + $24]
  faddp
  fstp  [eax + $24]

  fld   [edx + $20]
  fmul  [ecx + $08]
  fld   [edx + $24]
  fmul  [ecx + $18]
  faddp
  fld   [edx + $28]
  fmul  [ecx + $28]
  faddp
  fstp  [eax + $28]
end;

procedure pMatrixMul(out R : TMatrix4; const M, N : TMatrix4); register;
asm
  fld   [edx]
  fmul  [ecx]
  fld   [edx + $04]
  fmul  [ecx + $10]
  faddp
  fld   [edx + $08]
  fmul  [ecx + $20]
  faddp
  fld   [edx + $0c]
  fmul  [ecx + $30]
  faddp
  fstp  [eax]

  fld   [edx]
  fmul  [ecx + $04]
  fld   [edx + $04]
  fmul  [ecx + $14]
  faddp
  fld   [edx + $08]
  fmul  [ecx + $24]
  faddp
  fld   [edx + $0c]
  fmul  [ecx + $34]
  faddp
  fstp  [eax + $04]

  fld   [edx]
  fmul  [ecx + $08]
  fld   [edx + $04]
  fmul  [ecx + $18]
  faddp
  fld   [edx + $08]
  fmul  [ecx + $28]
  faddp
  fld   [edx + $0c]
  fmul  [ecx + $38]
  faddp
  fstp  [eax + $08]

  fld   [edx]
  fmul  [ecx + $0c]
  fld   [edx + $04]
  fmul  [ecx + $1c]
  faddp
  fld   [edx + $08]
  fmul  [ecx + $2c]
  faddp
  fld   [edx + $0c]
  fmul  [ecx + $3c]
  faddp
  fstp  [eax + $0c]

  fld   [edx + $10]
  fmul  [ecx]
  fld   [edx + $14]
  fmul  [ecx + $10]
  faddp
  fld   [edx + $18]
  fmul  [ecx + $20]
  faddp
  fld   [edx + $1c]
  fmul  [ecx + $30]
  faddp
  fstp  [eax + $10]

  fld   [edx + $10]
  fmul  [ecx + $04]
  fld   [edx + $14]
  fmul  [ecx + $14]
  faddp
  fld   [edx + $18]
  fmul  [ecx + $24]
  faddp
  fld   [edx + $1c]
  fmul  [ecx + $34]
  faddp
  fstp  [eax + $14]

  fld   [edx + $10]
  fmul  [ecx + $08]
  fld   [edx + $14]
  fmul  [ecx + $18]
  faddp
  fld   [edx + $18]
  fmul  [ecx + $28]
  faddp
  fld   [edx + $1c]
  fmul  [ecx + $38]
  faddp
  fstp  [eax + $18]

  fld   [edx + $10]
  fmul  [ecx + $0c]
  fld   [edx + $14]
  fmul  [ecx + $1c]
  faddp
  fld   [edx + $18]
  fmul  [ecx + $2c]
  faddp
  fld   [edx + $1c]
  fmul  [ecx + $3c]
  faddp
  fstp  [eax + $1c]

  fld   [edx + $20]
  fmul  [ecx]
  fld   [edx + $24]
  fmul  [ecx + $10]
  faddp
  fld   [edx + $28]
  fmul  [ecx + $20]
  faddp
  fld   [edx + $2c]
  fmul  [ecx + $30]
  faddp
  fstp  [eax + $20]

  fld   [edx + $20]
  fmul  [ecx + $04]
  fld   [edx + $24]
  fmul  [ecx + $14]
  faddp
  fld   [edx + $28]
  fmul  [ecx + $24]
  faddp
  fld   [edx + $2c]
  fmul  [ecx + $34]
  faddp
  fstp  [eax + $24]

  fld   [edx + $20]
  fmul  [ecx + $08]
  fld   [edx + $24]
  fmul  [ecx + $18]
  faddp
  fld   [edx + $28]
  fmul  [ecx + $28]
  faddp
  fld   [edx + $2c]
  fmul  [ecx + $38]
  faddp
  fstp  [eax + $28]

  fld   [edx + $20]
  fmul  [ecx + $0c]
  fld   [edx + $24]
  fmul  [ecx + $1c]
  faddp
  fld   [edx + $28]
  fmul  [ecx + $2c]
  faddp
  fld   [edx + $2c]
  fmul  [ecx + $3c]
  faddp
  fstp  [eax + $2c]

  fld   [edx + $30]
  fmul  [ecx]
  fld   [edx + $34]
  fmul  [ecx + $10]
  faddp
  fld   [edx + $38]
  fmul  [ecx + $20]
  faddp
  fld   [edx + $3c]
  fmul  [ecx + $30]
  faddp
  fstp  [eax + $30]

  fld   [edx + $30]
  fmul  [ecx + $04]
  fld   [edx + $34]
  fmul  [ecx + $14]
  faddp
  fld   [edx + $38]
  fmul  [ecx + $24]
  faddp
  fld   [edx + $3c]
  fmul  [ecx + $34]
  faddp
  fstp  [eax + $34]

  fld   [edx + $30]
  fmul  [ecx + $08]
  fld   [edx + $34]
  fmul  [ecx + $18]
  faddp
  fld   [edx + $38]
  fmul  [ecx + $28]
  faddp
  fld   [edx + $3c]
  fmul  [ecx + $38]
  faddp
  fstp  [eax + $38]

  fld   [edx + $30]
  fmul  [ecx + $0c]
  fld   [edx + $34]
  fmul  [ecx + $1c]
  faddp
  fld   [edx + $38]
  fmul  [ecx + $2c]
  faddp
  fld   [edx + $3c]
  fmul  [ecx + $3c]
  faddp
  fstp  [eax + $3c]
end;

function  fMatrixSkew(const a : TVector3) : TMatrix3;
asm
  fldz
  fst  [edx]
  fst  [edx + 20]
  fstp [edx + 40]
  fld  [eax]
  fst  [edx + 36]
  fchs
  fstp [edx + 24]
  fld  [eax + 4]
  fst  [edx + 8]
  fchs
  fstp [edx + 32]
  fld  [eax + 8]
  fst  [edx + 16]
  fchs
  fstp [edx + 4]
end;

procedure pMatrixSkew(out R : TMatrix3; const a : TVector3);
asm
  fldz
  fst  [eax]
  fst  [eax + 20]
  fstp [eax + 40]
  fld  [edx]
  fst  [eax + 36]
  fchs
  fstp [eax + 24]
  fld  [edx + 4]
  fst  [eax + 8]
  fchs
  fstp [eax + 32]
  fld  [edx + 8]
  fst  [eax + 16]
  fchs
  fstp [eax + 4]
end;

function  fOrthoNormalize(const M : TMatrix3) : TMatrix3;
var
  x,y,z : TVector3;
begin
  x[0] := M[0];
  x[1] := M[1];
  x[2] := M[2];

  y[0] := M[4];
  y[1] := M[5];
  y[2] := M[6];

  pNormalize(x);
  pCrossProduct(z, x, y);
  pNormalize(z);
  pCrossProduct(y, z, x);
  pNormalize(y);

  Result[0] := x[0];
  Result[1] := x[1];
  Result[2] := x[2];

  Result[4] := y[0];
  Result[5] := y[1];
  Result[6] := y[2];

  Result[8] := z[0];
  Result[9] := z[1];
  Result[10]:= z[2];
end;

procedure pOrthoNormalize(var M : TMatrix3); overload;
var
  x,y,z : TVector3;
begin
  x[0] := M[0];
  x[1] := M[1];
  x[2] := M[2];

  y[0] := M[4];
  y[1] := M[5];
  y[2] := M[6];

  pNormalize(x);
  pCrossProduct(z, x, y);
  pNormalize(z);
  pCrossProduct(y, z, x);
  pNormalize(y);

  M[0] := x[0];
  M[1] := x[1];
  M[2] := x[2];

  M[4] := y[0];
  M[5] := y[1];
  M[6] := y[2];

  M[8] := z[0];
  M[9] := z[1];
  M[10]:= z[2];
end;

procedure pOrthoNormalize(out R : TMatrix3; const M : TMatrix3); overload;
var
  x,y,z : TVector3;
begin
  x[0] := M[0];
  x[1] := M[1];
  x[2] := M[2];

  y[0] := M[4];
  y[1] := M[5];
  y[2] := M[6];

  pNormalize(x);
  pCrossProduct(z, x, y);
  pNormalize(z);
  pCrossProduct(y, z, x);
  pNormalize(y);

  R[0] := x[0];
  R[1] := x[1];
  R[2] := x[2];

  R[4] := y[0];
  R[5] := y[1];
  R[6] := y[2];

  R[8] := z[0];
  R[9] := z[1];
  R[10]:= z[2];
end;

Initialization
  Try
    SetCPUSupport;
  Finally
  End;
end.

