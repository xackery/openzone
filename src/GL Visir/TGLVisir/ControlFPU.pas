unit ControlFPU;
// This unit allows the programmer to set the control word of the FPU. It's
// primary use would be to mask exceptions in the FPU, so they don't generate
// signals that end up being Delphi-style exceptions. On the downside, you might
// get exotic results, such as NaNs, +/- infinite or denormalized numbers (rare)
// and not know about it (unless you use the tests in this unit). This is not
// always bad; for instance, ArcTan(+infinite) neatly returns the correct Pi/2.
// The use of this unit should be limited to those applications that use heavy
// math on well-behaved numbers, where infinites are harmless, or in
// applications that use vectorization techniques to exploit FPU register
// re-use. Thus, for your average Math unit routine exception masking makes no
// sense, but my VecX87s/d libraries wouldn't perform as well without it.
//
// Author   : Patrick Van Laake
//            patrick.vanlaake@powersurfr.com
//            http://plaza.powersurfr.com/at-nadir
//
// Version  : 2002.02.03  0.1  Initial development release
//
// The code in this unit is copy-lefted to you by @Nadir under the GPL license
// (http://www.gnu.org/copyleft/gpl.html). In plain English this means that you
// can freely use any of the code supplied to you, including modifying it and
// further distributing it, as long as @Nadir is acknowledged as the original
// author and no restrictions beyond any in the GPL are placed on it or on any
// products derived from it. If you wish to use this code in a commercial
// product, please contact @Nadir directly.

interface

function GetCurrentFPUControlWord : word;
function SetCurrentFPUControlWord(cw : word) : word; stdcall;
function SetDefaultFPUControlWord : word;

function MaskAllFPUExceptions : word;
function MaskFPUZeroDivide : word;
function MaskFPUOverflow : word;
function MaskFPUUnderflow : word;

function IsValid(v : single) : boolean; overload;
function IsValid(v : double) : boolean; overload;
function IsValid(v : extended) : boolean; overload;

function IsNaN(v : single) : boolean; overload;
function IsNaN(v : double) : boolean; overload;
function IsNaN(v : extended) : boolean; overload;

function IsInfinite(v : single) : boolean; overload;
function IsInfinite(v : double) : boolean; overload;
function IsInfinite(v : extended) : boolean; overload;

implementation

function GetCurrentFPUControlWord : word;
asm
  fstcw Result
end;

function SetCurrentFPUControlWord(cw : word) : word; stdcall;
// Call stdcall, because fldcw needs memory location, not register
asm
  fstcw Result         // return the old control word
  fldcw cw             // store new control word in the FPU
end;

function SetDefaultFPUControlWord : word;
// The Default8087CW variable is defined in the System unit and you should
// always set the FPU control word to this value after you're done with the
// exotic parts of your code. Several Delphi routines depend on the default
// FPU control state.
begin
  Result := GetCurrentFPUControlWord;
  Set8087CW(Default8087CW);
end;

function MaskAllFPUExceptions : word;
var cw : word;
asm
  fstcw cw             // get the current control word
  mov   cx,cw
  mov   Result,cx      // give it back to the caller
  or    cx,$003F       // mask in the exceptions
  mov   cw,cx
  fldcw cw             // store control word in the FPU
end;

function MaskFPUZeroDivide : word;
var cw : word;
asm
  fstcw cw             // get the current control word
  mov   cx,cw
  mov   Result,cx      // give it back to the caller
  or    cx,$0004       // mask in the zero-divide exception
  mov   cw,cx
  fldcw cw             // store control word in the FPU
end;

function MaskFPUOverflow : word;
var cw : word;
asm
  fstcw cw             // get the current control word
  mov   cx,cw
  mov   Result,cx      // give it back to the caller
  or    cx,$0008       // mask in the overflow exception
  mov   cw,cx
  fldcw cw             // store control word in the FPU
end;

function MaskFPUUnderflow : word;
var cw : word;
asm
  fstcw cw             // get the current control word
  mov   cx,cw
  mov   Result,cx      // give it back to the caller
  or    cx,$0010       // mask in the underflow exception
  mov   cw,cx
  fldcw cw             // store control word in the FPU
end;

function IsValid(v : single) : boolean; overload;
asm
  fld   dword ptr v    // 0: v
  fxam                 // see what we've got, C2 set if valid number
  fstsw ax             // store C0-C3 in ax,
  sahf                 //   then in EFLAGS register: C2 -> PF
  mov   Result,1       // assume true
  jp    @Valid         // jump if PF set
  mov   Result,0       // set result to false
@Valid:
  ffree st(0)          // clear FPU register
end;

function IsValid(v : double) : boolean; overload;
asm
  fld   qword ptr v    // 0: v
  fxam                 // see what we've got, C2 set if valid number
  fstsw ax             // store C0-C3 in ax,
  sahf                 //   then in EFLAGS register: C2 -> PF
  mov   Result,1       // assume true
  jp    @Valid         // jump if PF set
  mov   Result,0       // set result to false
@Valid:
  ffree st(0)          // clear FPU register
end;

function IsValid(v : extended) : boolean; overload;
asm
  fld   tbyte ptr v    // 0: v
  fxam                 // see what we've got, C2 set if valid number
  fstsw ax             // store C0-C3 in ax,
  sahf                 //   then in EFLAGS register: C2 -> PF
  mov   Result,1       // assume true
  jp    @Valid         // jump if PF set
  mov   Result,0       // set result to false
@Valid:
  ffree st(0)          // clear FPU register
end;

function IsNaN(v : single) : boolean; overload;
asm
  fld   dword ptr v    // 0: v
  fxam                 // see what we've got, C0 set if NaN
  fstsw ax             // store C0-C3 in ax,
  sahf                 //   then in EFLAGS register: C0 -> CF
  mov   Result,0       // assume false
  jae   @NoNaN         // jump if CF not set
  mov   Result,1       // set result to true
@NoNaN:
  ffree st(0)          // clear FPU register
end;

function IsNaN(v : double) : boolean; overload;
asm
  fld   qword ptr v    // 0: v
  fxam                 // see what we've got, C0 set if NaN
  fstsw ax             // store C0-C3 in ax,
  sahf                 //   then in EFLAGS register: C0 -> CF
  mov   Result,0       // assume false
  je    @NoNaN         // jump if CF not set
  mov   Result,1       // set result to true
@NoNaN:
  ffree st(0)          // clear FPU register
end;

function IsNaN(v : extended) : boolean; overload;
asm
  fld   tbyte ptr v    // 0: v
  fxam                 // see what we've got, C0 set if NaN
  fstsw ax             // store C0-C3 in ax,
  sahf                 //   then in EFLAGS register: C0 -> CF
  mov   Result,0       // assume false
  je    @NoNaN         // jump if CF not set
  mov   Result,1       // set result to true
@NoNaN:
  ffree st(0)          // clear FPU register
end;

function IsInfinite(v : single) : boolean; overload;
asm
  fld   dword ptr v    // 0: v
  fxam                 // see what we've got, C0 & C2 set if +/-Inf
  fstsw ax             // store C0-C3 in ax: C0 -> CF; C2 -> PF
  test  ax,$0500       // see if both C0 & C2 are set
  mov   Result,0       // assume false
  je    @NoInf         // jump if CF not set
  mov   Result,1       // set result to true
@NoInf:
  ffree st(0)          // clear FPU register
end;

function IsInfinite(v : double) : boolean; overload;
asm
  fld   qword ptr v    // 0: v
  fxam                 // see what we've got, C0 & C2 set if +/-Inf
  fstsw ax             // store C0-C3 in ax: C0 -> CF; C2 -> PF
  test  ax,$0500       // see if both C0 & C2 are set
  mov   Result,0       // assume false
  je    @NoInf         // jump if CF not set
  mov   Result,1       // set result to true
@NoInf:
  ffree st(0)          // clear FPU register
end;

function IsInfinite(v : extended) : boolean; overload;
asm
  fld   tbyte ptr v    // 0: v
  fxam                 // see what we've got, C0 & C2 set if +/-Inf
  fstsw ax             // store C0-C3 in ax: C0 -> CF; C2 -> PF
  test  ax,$0500       // see if both C0 & C2 are set
  mov   Result,0       // assume false
  je    @NoInf         // jump if CF not set
  mov   Result,1       // set result to true
@NoInf:
  ffree st(0)          // clear FPU register
end;

end.
