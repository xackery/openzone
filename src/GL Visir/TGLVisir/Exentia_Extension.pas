Unit Exentia_Extension;

Interface

Uses Exentia;

Type
  TFVectorExt = Class(TFVector)
    Class Function BuildVector(Const ALength: Integer) : TFVector;
    Class Function BuildUsingAlignedArrayPart(xSA: pSingleArray; Const xLength: Integer): TFVector;
    Procedure      UpdateUsingAlignedArrayPart(xSA: pSingleArray; Const xLength: Integer);
  End;

  TX87VectorExt = class(TX87Vector)
    Procedure AddSingleVec(V : TFVector);      Override;
    Procedure SubSingleVec(V : TFVector);      Override;
    Procedure MulMatrix(V: TFVector);          Override;
  End;

  TSSEVectorExt = class(TSSEVector)
    Procedure AddSingleVec(V : TFVector); Override;
    Procedure SubSingleVec(V : TFVector); Override;
    Procedure MulMatrix(V: TFVector);     Override;
  End;

  T3DNowVectorExt = class(T3DNowVector)
    Procedure AddSingleVec(V : TFVector); Override;
    Procedure SubSingleVec(V : TFVector); Override;
    Procedure MulMatrix(V: TFVector);     Override;
  End;

Implementation

// ---------------------------
// TFVectorExt
// ---------------------------

Class Function TFVectorExt.BuildVector(Const ALength: Integer): TFVector;
Begin
       If isSSE   Then Result := TSSEVectorExt.Create(ALength)
  Else If is3DNow Then Result := T3DNowVectorExt.Create(ALength)
  Else Result := TX87VectorExt.Create(ALength);
End; // TFVectorExt.BuildVector

Class Function TFVectorExt.BuildUsingAlignedArrayPart(xSA: pSingleArray; Const xLength: Integer): TFVector;
Begin
       If isSSE   Then Result := TSSEVectorExt.CreateUsingAlignedArrayPart(xSA, xLength)
  Else If is3DNow Then Result := T3DNowVectorExt.CreateUsingAlignedArrayPart(xSA, xLength)
  Else Result := TX87VectorExt.CreateUsingAlignedArrayPart(xSA, xLength);
End; // TFVectorExt.BuildUsingAlignedArrayPart

Procedure TFVectorExt.UpdateUsingAlignedArrayPart(xSA: pSingleArray; Const xLength: Integer);
Begin
  fSourceLength  := xLength;
  AlignedArray   := xSA;
  AllocatedArray := nil;
  if (xLength and 3) > 0
       then NumElements := (xLength and ($FFFFFFFF - 3)) + 4
       else NumElements := xLength;
End; // TFVectorExt.UpdateUsingAlignedArrayPart

// ---------------------------
// TX87VectorExt
// ---------------------------

procedure TX87VectorExt.AddSingleVec(V : TFVector);
var
  counter,sc : integer;
  Data,Source:PSingleArray;
begin
  Data:=@DataArray[GetNumElements];
  Source:=@V.DataArray[0];
  Counter:=-GetNumElements;
  sc := 0;

  while Counter<-3 do
  begin
    Data[counter]   := Data[counter]   + Source[sc];
    Data[counter+1] := Data[counter+1] + Source[sc+1];
    Data[counter+2] := Data[counter+2] + Source[sc+2];
    Data[counter+3] := Data[counter+3] + Source[sc+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Data[counter] + Source[counter+3];
    inc(Counter);
  end;
end; // TX87VectorExt.AddSingleVec

procedure TX87VectorExt.SubSingleVec(V : TFVector);
var
  counter,sc : integer;
  Data,Source:PSingleArray;
begin
  Data:=@DataArray[GetNumElements];
  Source:=@V.DataArray[0];
  Counter:=-GetNumElements;
  sc := 0;

  while Counter<-3 do
  begin
    Data[counter]   := Data[counter]   - Source[sc];
    Data[counter+1] := Data[counter+1] - Source[sc+1];
    Data[counter+2] := Data[counter+2] - Source[sc+2];
    Data[counter+3] := Data[counter+3] - Source[sc+3];
    inc(Counter,4);
  end;
  while Counter<0 do
  begin
    Data[counter] := Data[counter] - Source[counter+3];
    inc(Counter);
  end;
end; // TX87VectorExt.SubSingleVec

procedure TX87VectorExt.MulMatrix(V : TFVector);
var
  counter,sc : integer;
  Data,Source:PSingleArray;
begin
  Data:=@DataArray[GetNumElements];
  Source:=@V.DataArray[GetNumElements];
  Counter:=-GetNumElements;
  sc := 0;

  while Counter<-3 do
  begin
    Data[counter+0] := Data[counter+0] * Source[sc+0] + Data[counter+1] * Source[sc+4] + Data[counter+2] * Source[sc+8]  + Data[counter+3] * Source[sc+12];
    Data[counter+1] := Data[counter+0] * Source[sc+1] + Data[counter+1] * Source[sc+5] + Data[counter+2] * Source[sc+9]  + Data[counter+3] * Source[sc+13];
    Data[counter+2] := Data[counter+0] * Source[sc+2] + Data[counter+1] * Source[sc+6] + Data[counter+2] * Source[sc+10] + Data[counter+3] * Source[sc+14];
    Data[counter+3] := Data[counter+0] * Source[sc+3] + Data[counter+1] * Source[sc+7] + Data[counter+2] * Source[sc+11] + Data[counter+3] * Source[sc+15];
    inc(Counter,4);
  end;
end; // TX87VectorExt.MulMatrix

// ---------------------------
// TSSEVectorExt
// ---------------------------

procedure TSSEVectorExt.AddSingleVec(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]

  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop

  movaps xmm7,[edx]
@LargeAddLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength] {$ENDIF}
  addps xmm0,xmm7
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength] {$ENDIF}
  movaps [eax],xmm0

  movaps xmm1,[eax+16]
  addps xmm1,xmm7
  movaps [eax+16],xmm1

  movaps xmm2,[eax+32]
  addps xmm2,xmm7
  movaps [eax+32],xmm2

  movaps xmm3,[eax+48]
  addps xmm3,xmm7
  movaps [eax+48],xmm3
{$ELSE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength] {$ENDIF}
  addps xmm0,xmm7
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength] {$ENDIF}
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  addps xmm2,xmm7
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  addps xmm4,xmm7
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  addps xmm6,xmm7
  movaps [eax+48],xmm6
{$ENDIF}

  add eax,64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
  movaps xmm1,[edx]
@SmallAddLoop:
  movaps xmm0,[eax]
  addps xmm0,xmm1
  movaps [eax],xmm0

  add eax,16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
end; // TSSEVectorExt.AddSingleVec

procedure TSSEVectorExt.SubSingleVec(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeSubLoop
  movaps xmm7,[edx]
@LargeSubLoop:
{$IFDEF CISC_STYLE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  subps xmm0,xmm7
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  movaps [eax],xmm0

  movaps xmm1,[eax+16]
  subps xmm1,xmm7
  movaps [eax+16],xmm1

  movaps xmm2,[eax+32]
  subps xmm2,xmm7
  movaps [eax+32],xmm2

  movaps xmm3,[eax+48]
  subps xmm3,xmm7
  movaps [eax+48],xmm3
{$ELSE}
  movaps xmm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [eax+CacheLineLength]  {$ENDIF}
  subps xmm0,xmm7
  {$IFNDEF NO_SOFTWARE_PREFETCHING}  prefetcht0 [edx+CacheLineLength]  {$ENDIF}
  movaps [eax],xmm0

  movaps xmm2,[eax+16]
  subps xmm2,xmm7
  movaps [eax+16],xmm2

  movaps xmm4,[eax+32]
  subps xmm4,xmm7
  movaps [eax+32],xmm4

  movaps xmm6,[eax+48]
  subps xmm6,xmm7
  movaps [eax+48],xmm6
{$ENDIF}
  add eax,64
  dec ecx
  jnz @LargeSubLoop
@SkipLargeSubLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndSub
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
  movaps xmm1,[edx]
@SmallSubLoop:
  movaps xmm0,[eax]
  subps xmm0,xmm1
  movaps [eax],xmm0

  add eax,16
  dec ecx
  jnz @SmallSubLoop

@EndSub:
end; // TSSEVectorExt.SubSingleVec

procedure TSSEVectorExt.MulMatrix(V : TFVector);
asm
  mov      ecx,[eax + NumElements]
  mov      eax,[eax + AlignedArray]
  mov      edx,[V.AlignedArray]
  test     ecx,ecx
  shr      ecx,2
  jz       @EndMul
  movaps   xmm0,[edx]        // First column:  xmm0 = [R30, R20, R10, R00]
  movaps   xmm1,[edx+16]     // Second column: xmm1 = [R31, R21, R11, R01]
  movaps   xmm2,[edx+32]     // Third column:  xmm2 = [R32, R22, R12, R02]
  movaps   xmm3,[edx+48]     // Fourth column: xmm3 = [R33, R23, R13, R03]

@MulLoop:
  movaps   xmm4,[eax]        // xmm4 = [v2, v3, v1, v0]
  movhlps  xmm6,xmm4         // xmm6 = [  ,   , v3, v2]
//  movlps   xmm4,[eax]        // xmm4 = [  ,   , v1, v0]
//  movlps   xmm6,[eax+8]      // xmm6 = [  ,   , v3, v2]
  unpcklps xmm4,xmm4         // xmm4 = [v1, v1, v0, v0]
  unpcklps xmm6,xmm6         // xmm6 = [v3, v3, v2, v2]
  movhlps  xmm5,xmm4         // xmm5 = [  ,   , v1, v1]
  movhlps  xmm7,xmm6         // xmm7 = [  ,   , v3, v3]
  movlhps  xmm4,xmm4         // xmm4 = [v0, v0, v0, v0]
  mulps    xmm4,xmm0         // xmm4 = [R30*v0, R20*v0, R10*v0, R00*v0]
  movlhps  xmm5,xmm5         // xmm5 = [v1, v1, v1, v1]
  mulps    xmm5,xmm1         // xmm5 = [R31*v1, R21*v1, R11*v1, R01*v1]
  movlhps  xmm6,xmm6         // xmm5 = [v2, v2, v2, v2]
  mulps    xmm6,xmm2         // xmm6 = [R32*v2, R22*v2, R12*v2, R02*v2]
  addps    xmm4,xmm5         // xmm4 = [R30*v0+R31*v1, R20*v0+R21*v1, R10*v0+r11*v1, R00*v0+r01*v1]
  movlhps  xmm7,xmm7         // xmm5 = [v3, v3, v3, v3]
  mulps    xmm7,xmm3         // xmm6 = [R33*v3, R23*v3, R13*v3, R03*v3]
  addps    xmm6,xmm7         // xmm6 = [R32*v2+R33*v3, R22*v2+R23*v3, R12*v2+r13*v3, R02*v2+r03*v3]
  addps    xmm4,xmm6         // xmm4 = Transformed vertex
  movaps   [eax],xmm4

  add      eax,16
  dec      ecx
  jnz      @MulLoop
@EndMul:
end; // TSSEVectorExt.MulMatrix

// ---------------------------
// T3DNowVectorExt
// ---------------------------

procedure T3DNowVectorExt.AddSingleVec(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeAddLoop
  movq mm1,[edx]
  movq mm3,[edx+8]
@LargeAddLoop:
{$IFDEF CISC_STYLE}
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetchw [eax+CacheLineLength] {$ENDIF}
  pfadd mm0,mm1
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [edx+CacheLineLength]  {$ENDIF}
  movq [eax],mm0

  movq mm2,[eax+8]
  pfadd mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  pfadd mm4,mm1
  movq [eax+16],mm4

  movq mm6,[eax+24]
  pfadd mm6,mm3
  movq [eax+24],mm6

  movq mm0,[eax+32]
  pfadd mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  pfadd mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  pfadd mm4,mm1
  movq [eax+48],mm4

  movq mm6,[eax+56]
  pfadd mm6,mm3
  movq [eax+56],mm6
  {$ELSE}
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING}   prefetchw [eax+CacheLineLength] {$ENDIF}
  pfadd mm0,mm1
  {$IFNDEF NO_SOFTWARE_PREFETCHING}   prefetch [edx+CacheLineLength] {$ENDIF}
  movq [eax],mm0

  movq mm2,[eax+8]
  pfadd mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  pfadd mm4,mm1
  movq [eax+16],mm4

  movq mm6,[eax+24]
  pfadd mm6,mm3
  movq [eax+24],mm6

  movq mm0,[eax+32]
  pfadd mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  pfadd mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  pfadd mm4,mm1
  movq [eax+48],mm4

  movq mm6,[eax+56]
  pfadd mm6,mm3
  movq [eax+56],mm6
{$ENDIF}

  add eax,64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
  movq mm1,[edx]
  movq mm3,[edx+8]
@SmallAddLoop:
  movq mm0,[eax]
  pfadd mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  pfadd mm2,mm3
  movq [eax+8],mm2

  add eax,16
  dec ecx
  jnz @SmallAddLoop

@EndAdd:
  emms
end; // T3DNowVectorExt.AddSingleVec

procedure T3DNowVectorExt.SubSingleVec(V : TFVector);
asm
  mov ecx,[eax + NumElements]
  mov eax,[eax + AlignedArray]
  mov edx,[V.AlignedArray]
  push ecx
  shr ecx,4  // number of large iterations = number of elements / 16
  jz @SkipLargeSubLoop
  movq mm1,[edx]
  movq mm3,[edx+8]
@LargeSubLoop:
  movq mm0,[eax]
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetchw [eax+CacheLineLength] {$ENDIF}
  pfsub mm0,mm1
  {$IFNDEF NO_SOFTWARE_PREFETCHING} prefetch [edx+CacheLineLength] {$ENDIF}
  movq [eax],mm0

  movq mm2,[eax+8]
  pfsub mm2,mm3
  movq [eax+8],mm2

  movq mm4,[eax+16]
  pfsub mm4,mm1
  movq [eax+16],mm4

  movq mm6,[eax+24]
  pfsub mm6,mm3
  movq [eax+24],mm6

  movq mm0,[eax+32]
  pfsub mm0,mm1
  movq [eax+32],mm0

  movq mm2,[eax+40]
  pfsub mm2,mm3
  movq [eax+40],mm2

  movq mm4,[eax+48]
  pfsub mm4,mm1
  movq [eax+48],mm4

  movq mm6,[eax+56]
  pfsub mm6,mm3
  movq [eax+56],mm6

  add eax,64
  dec ecx
  jnz @LargeSubLoop

@SkipLargeSubLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndSub
  shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4
  movq mm1,[edx]
  movq mm3,[edx+8]
@SmallSubLoop:
  movq mm0,[eax]
  pfsub mm0,mm1
  movq [eax],mm0

  movq mm2,[eax+8]
  pfsub mm2,mm3
  movq [eax+8],mm2

  add eax,16
  dec ecx
  jnz @SmallSubLoop

@EndSub:
  emms
end; // T3DNowVectorExt.SubSingleVec

procedure T3DNowVectorExt.MulMatrix(V : TFVector);
asm
  mov       ecx,[eax + NumElements]
  mov       eax,[eax + AlignedArray]
  mov       edx,[V.AlignedArray]
  test      ecx,ecx
  shr       ecx,2
  jz        @EndMul
  femms                      // Clear MMX state
@MulLoop:
  movq      mm0,[eax]         // MM0 = [y, x]
  movq      mm1,[eax+8]       // MM1 = [w, z]
  movq      mm2,mm0           // MM2 = [y, x]
  movq      mm3,[edx]         // MM3 = [R01, R00]
  punpckldq mm0,mm0           // MM0 = [x, x]
  movq      mm4,[edx+16]      // MM4 = [R11, R10]
  pfmul     mm3,mm0           // MM3 = [x*R01, x*R00]
  punpckhdq mm2,mm2           // MM2 = [y, y]
  pfmul     mm4,mm2           // MM4 = [y*R11, y*R10]
  movq      mm5,[edx+8]       // MM5 = [R03, R02]
  movq      mm7,[edx+24]      // MM7 = [R13, R12]
  movq      mm6,mm1           // MM6 = [w, z]
  pfmul     mm5,mm0           // MM5 = [x*R03, x*R02]
  movq      mm0,[edx+32]      // MM0 = [R21, R20]
  punpckldq mm1,mm1           // MM1 = [z, z]
  pfmul     mm7,mm2           // MM7 = [y*R13, y*R12]
  movq      mm2,[edx+40]      // MM2 = [R23, R22]
  pfmul     mm0,mm1           // MM0 = [z*R21, z*R20]
  pfadd     mm3,mm4           // MM3 = [x*R01+y*R11, x*R00+y*R10]
  movq      mm4,[edx+48]      // MM4 = [R31,R30]
  pfmul     mm2,mm1           // MM2 = [z*R23, z*R22]
  pfadd     mm5,mm7           // MM5 = [x*R03+y*R13, x*R02+y*R12]
  movq      mm1,[edx+56]      // MM1 = [R33, R32]
  punpckhdq mm6,mm6           // MM6 = [w, w]
  pfadd     mm3,mm0           // MM3 = [x*R01+y*R11+z*R21, x*R00+y*R10+z*R20]
  pfmul     mm4,mm6           // MM4 = [w*R31, w*R30]
  pfmul     mm1,mm6           // MM1 = [w*R33, w*R32]
  pfadd     mm5,mm2           // MM5 = [x*R03+y*R13+z*R23, x*R02+y*R12+z*R22]
  pfadd     mm3,mm4           // MM3 = [x*R01+y*R11+z*R21+w*31, x*R00+y*R10+z*R20+w*30]
  movntq    [eax],mm3         // Store lower quadword of transformed vertex
  pfadd     mm5,mm1           // MM5 = [x*R03+y*R13+z*R23+w*R33, x*R02+y*R12+z*R22+w*R32]
  movntq    [eax+8],mm5       // Store upper quadword of transformed vertex
  dec      ecx
  jnz      @MulLoop
@EndMul:
end; // T3DNowVectorExt.MulMatrix

End.
