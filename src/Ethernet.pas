Unit Ethernet;
// ----------------------------------------------------------------------------------
// This calculates a string's 32-bit CRC value usng the IEEE 802.3 Ethernet standard.
//
// This algorithm can be found at:
//
// http://cell-relay.indiana.edu/cell-relay/publications/software/CRC/32bitCRC.c.html
// ----------------------------------------------------------------------------------

Interface

Type BytePtr = ^Byte;

Function Update_CRC(CRC_Accum: LongWord; P: BytePtr; Size: LongWord): LongWord;
Function GetCRCOfString(St: String): LongWord;

Implementation

Const Polynomial = $04C11DB7;

Var CRC_Table: Array [0..255] Of LongWord;

Procedure Gen_CRC_Table;
Var I,J,CRC_Accum: LongWord;
Begin
  For I := 0 To 255 Do
  Begin
    CRC_Accum := I Shl 24;
    For J := 0 To 7 Do
    Begin
      If (CRC_Accum And $80000000) <> 0
       Then CRC_Accum := (CRC_Accum Shl 1) Xor Polynomial
       Else CRC_Accum := CRC_Accum Shl 1;
    End; // For J
    CRC_Table[I] := CRC_Accum;
  End; // For I
End; // Gen_CRC_Table

Function Update_CRC(CRC_Accum: LongWord; P: BytePtr; Size: LongWord): LongWord;
Var I: LongWord;
Begin
  While Size > 0 Do
  Begin
    I := ((CRC_Accum Shr 24) Xor P^) And $FF;
    Inc(LongInt(P));
    CRC_Accum := (CRC_Accum Shl 8) Xor CRC_Table[I];
    Dec(Size);
  End; // For J
  Result := CRC_Accum;
End; // Update_CRC

Function GetCRCOfString(St: String): LongWord;
Var St1: BytePtr;
Begin
  If St <> '' Then
  Begin
    GetMem(St1,Length(St) + 1);
    FillChar(St1^,Length(St) + 1,0);
    Move(St[1],St1^,Length(St));
    Result := Update_CRC(0,St1,Length(St) + 1);
    FreeMem(St1,Length(St) + 1);
  End
  Else Result := 0;
End; // GetCRCOfString

Initialization
  Gen_CRC_Table;
End.
