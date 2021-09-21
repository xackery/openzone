// Targa unit developed by Davie Reed, davie@smatters.com

Unit Targa;

Interface
{$DEFINE USETBITMAP}
Uses
  Windows,
  Graphics,
  Classes;

Type
  TConvertBPP = (cbNoConversion,cbTo8Bit,cbTo24Bit,cbTo32Bit);

// Setup the following variable before calling LoadFromFileX
//
// Global_KeepOriginalFormat: Word
// 0 = Use the files native bits per pixel for the TBitmap
// 1 = Force TBitmap of 256 colors and use gray it file was 24bit
// 2 = Force TBitmap to 24bit
// 3 = Force TBitmap to 32bit
//
// SAVETOFILEX(parm1,parm2,parm3);
//    Parm1=Filename
//    Parm2=TBitmap to save
//    Parm3=Type of TGA file to create
//           1 = Save as 256 Color file
//           2 = Save as 16M file

Procedure LoadFromFileX(FileName: String; Const BitMap: TBitmap; PreMultiplyAlphas: Boolean; Convert: TConvertBPP);
Procedure SaveToFileX(FileName: String; Const BitMap: TBitmap; MyPcxType: Byte);

Implementation

Const MaskTable: Packed Array[0..7] Of Byte = (128,64,32,16,8,4,2,1);

Type
  TTGAHeader = Packed Record
    IdentSize      : Byte;
    ColorMapType   : Byte;
    ImageType      : Byte;
    ColorMapStart  : Word;
    ColorMapLength : Word;
    ColorMapBits   : Byte;
    XStart         : Word;
    YStart         : Word;
    Width          : Word;
    Height         : Word;
    Bits           : Byte;
    Descriptor     : Byte;
  End;
  TypeRGB = Packed Record
    B,G,R: Byte;
  End;
  TMyPixelFormat = (mpf1bit, mpf4bit, mpf8bit, mpf15bit, mpf16bit, mpf24bit, mpf32bit);

Type
  TypeRegVer     = Set Of (Non_Registered,Registered,OEM,PRO,SYSOP);
  PDataLineArray = ^TDataLineArray;
  TDataLineArray = Packed Array[0..65535] Of Byte;
  TDataWordArray = Packed Array[0..65535] Of SmallInt;
  TFakePalette   = Packed Record
    LPal  : TLogPalette;
    Dummy : Packed Array[1..255] of TPaletteEntry;
  End;

  TypeEgaPalette = Packed Array[0..16] Of Byte;
  TypePalette    = Packed Array[0..255,1..3] Of Byte;

Const
  Global_HiColor      = 3;
  Global_HiColorAlpha = 4;
//  Global_KeepOriginalFormat : TConvertBPP = cbNoConversion;

Var
  PictureFile   : File;
  PaletteVGA    : TypePalette;
  TempArrayD    : PDataLineArray;
  TempArrayD2   : PDataLineArray;
  TempArrayDBIg : PDataLineArray;
  Premult       : Array[0..255,0..255] of Byte;

Const
  Const4096 = 8 * 1024;

Var
  Index1    : Word;
  Index2    : Word;
  IndexData : Packed Array[0..Const4096 - 1] Of Byte;

Procedure FileGetMore;
Var NumRead: Integer;
Begin
  FillChar(IndexData,Const4096,0);
  BlockRead(PictureFile,IndexData,Const4096,NumRead);
  Index1 := Const4096;
  Index2 := 0;
End;

Procedure FastGetBytes(Var Ptr1; NumBytes: Integer);
Var X: Integer;
Begin
  // If we have enough the block it! Otherwise do one at a time!

  If Index1 < NumBytes Then
  Begin
    If Index1 = 0 Then FileGetMore;
    For X := 0 To NumBytes - 1 Do
    Begin
      TDataLineArray(Ptr1)[X] := IndexData[Index2];
      Inc(Index2);
      Dec(Index1);
      If Index1 = 0 Then FileGetMore;
    End;
  End
  Else
  Begin
    // Block it fast!

    Move(IndexData[Index2],TDataLineArray(Ptr1)[0],NumBytes);
    Index2 := Index2 + Numbytes;
    Index1 := Index1 - NumBytes;
  End;
End;

Function FastGetByte: Byte;
Begin
  If Index1 = 0 Then FileGetMore;
  FastGetByte := IndexData[Index2];
  Inc(Index2);
  Dec(Index1);
End;

Function FastGetWord: Word;
Begin
  FastGetWord := Word(FastGetByte) + Word(FastGetByte) * 256;
End;

Procedure FileIoReset;
Begin
  Index1 := 0;
  Index2 := 0;
End;

Procedure OpenFile(Var FileName: String; Var FileOk: Boolean);
Var
  Io          : Integer;
  OldFileMode : Integer;

Begin
  FileIoReset;
  // Io:=IoResult;
  OldFileMode := FileMode;
  FileMode    := 0;
  AssignFile(PictureFile,FileName);
  Reset(PictureFile,1);
  Io := IoResult;
  If Io <> 0 Then FileOk := False;
  FileMode := OldFileMode;
End;

Procedure FillerUp(Var TempArrayD; Size: Integer; B1: Byte);
Begin
  FillChar(TempArrayD,Size,B1);
End;

Procedure ConvertXBitsToYBits(Var Input,Output: TDataLineArray; Xbits,Ybits,Width: Integer);
Var
  X,Z : Integer;
  B1  : Byte;

Begin
  // Generic converter to a single data line :)
  // Can go only from smaller bits to larger bits, otherwise you need to dither down!
  // PaletteVGA MUST be setup already!

  Case Xbits Of
    1:
    Begin
      Case Ybits Of
        4:
        Begin
          // From 1 bit to 4 bit, hmmmmm EZ :)

          For X := 0 To Width - 1 Do
          Begin
            B1 := (Input[X Shr 3] Shr (7 - (X Mod 8))) And 1;
            OutPut[X Shr 1] := OutPut[X Shr 1] Or (B1 Shl ((1 - (X Mod 2)) * 4));
          End;
        End;
        8:
        Begin
          // From 1 bit to 8 bit, hmmmmm EZ :)

          For X := 0 To Width - 1 Do
          Begin
            B1 := (Input[X Shr 3] Shr (7 - (X Mod 8))) And 1;
            OutPut[X] := B1;
          End;
        End;
        24:
        Begin
          // From 1 bit to 8 bit, hmmmmm EZ :)

          Z := 0;
          For X := 0 To Width - 1 Do
          Begin
            B1 := ((Input[X Shr 3] Shr (7 - (X Mod 8))) And 1) * 255;
            OutPut[Z + 0] := B1;
            OutPut[Z + 1] := B1;
            OutPut[Z + 2] := B1;
            Inc(Z,3);
          End;
        End;
        32:
        Begin
          // From 1 bit to 8 bit, hmmmmm EZ :)

          Z := 0;
          For X := 0 To Width - 1 Do
          Begin
            B1 := ((Input[X Shr 3] Shr (7 - (X Mod 8))) And 1) * 255;
            OutPut[Z + 0] := B1;
            OutPut[Z + 1] := B1;
            OutPut[Z + 2] := B1;
            OutPut[Z + 3] := 255;
            Inc(Z,4);
          End;
        End;
      End;
    End;
    4:
    Begin
      Case Ybits Of
        4: Move(Input[0],Output[0],Width);
        8:
        Begin
          // Go from 4 bits to 8 bit :)

          For X := 0 To Width - 1 Do
          Begin
            B1 := (Input[X Shr 1] Shr ((1 - (X Mod 2)) * 4)) And $0F;
            OutPut[X] := B1;
          End;
        End;
        24:
        Begin
          // Go from 4 bits to 24 bit :)

          Z := 0;
          For X := 0 To Width - 1 Do
          Begin
            B1 := (Input[X Shr 1] Shr ((1 - (X Mod 2)) * 4)) And $0F;
            OutPut[Z + 0] := (PaletteVGA[B1,3] * 255) Div 63;
            OutPut[Z + 1] := (PaletteVGA[B1,2] * 255) Div 63;
            OutPut[Z + 2] := (PaletteVGA[B1,1] * 255) Div 63;
            Inc(Z,3);
          End;
        End;
        32:
        Begin
          // Go from 4 bits to 32 bit :)

          Z := 0;
          For X := 0 To Width - 1 Do
          Begin
            B1 := (Input[X Shr 1] Shr ((1 - (X Mod 2)) * 4)) And $0F;
            OutPut[Z + 0] := (PaletteVGA[B1,3] * 255) Div 63;
            OutPut[Z + 1] := (PaletteVGA[B1,2] * 255) Div 63;
            OutPut[Z + 2] := (PaletteVGA[B1,1] * 255) Div 63;
            OutPut[Z + 3] := 255;
            Inc(Z,4);
          End;
        End;
      End;
    End;
    8:
    Begin
      Case Ybits Of
        1:
        Begin
          For X := 0 To Width - 1 Do OutPut[X Shr 3] := 0;
          For X := 0 To Width - 1 Do
          Begin
            B1 := InPut[X];
            OutPut[X Shr 3] := OutPut[X Shr 3] Or (B1 Shl (7 - (X Mod 8)));
          End;
        End;
        8: Move(Input[0],Output[0],Width);
        24:
        Begin
          // From 8 bit to 24 bit, hmmmmm 2EZ :)

          Z := 0;
          For X := 0 To Width - 1 Do
          Begin
            B1 := Input[X];
            OutPut[Z + 0] := (PaletteVGA[B1,3] * 255) Div 63;
            OutPut[Z + 1] := (PaletteVGA[B1,2] * 255) Div 63;
            OutPut[Z + 2] := (PaletteVGA[B1,1] * 255) Div 63;
            Inc(Z,3);
          End;
        End;
        32:
        Begin
          // From 8 bit to 32 bit, hmmmmm 2EZ :)

          Z := 0;
          For X := 0 To Width - 1 Do
          Begin
            B1 := Input[X];
            OutPut[Z + 0] := (PaletteVGA[B1,3] * 255) Div 63;
            OutPut[Z + 1] := (PaletteVGA[B1,2] * 255) Div 63;
            OutPut[Z + 2] := (PaletteVGA[B1,1] * 255) Div 63;
            OutPut[Z + 3] := 255;
            Inc(Z,4);
          End;
        End;
      End;
    End;
    24:
    Begin
      Case Ybits Of
        24: Move(Input[0],Output[0],Width * 3);
        32:
        Begin
          Z := 0;
          For X := 0 To Width - 1 Do
          Begin
            OutPut[Z + 0] := Input[X * 3 + 0];
            OutPut[Z + 1] := Input[X * 3 + 1];
            OutPut[Z + 2] := Input[X * 3 + 2];
            OutPut[Z + 3] := 255;
            Inc(Z,4);
          End;
        End;
      End;
    End;
  End;
End;

Procedure SetUpMaskGrayPalette;
Var I,J: Integer;
Begin
  For J := 0 To 255 Do
  Begin
    For I := 1 To 3 Do PaletteVga[J,I] := J * 63 Div 255;
  End;
End;

Function PCXGrayValue(R,G,B: Integer): Word;
Begin
  PCXGrayValue := ((R Shl 5) + (G Shl 6) + (B * 12)) Div 108;
End;

Procedure SaveThePalette(Const HPal: HPalette; Var SavePal: TypePalette; Var SysPal: TFakePalette);
Var I: Integer;
Begin
  For I := 0 To 255 Do
  Begin
    Syspal.LPal.PalPalEntry[I].peRed   := 0;
    Syspal.LPal.PalPalEntry[I].peGreen := 0;
    Syspal.LPal.PalPalEntry[I].peBlue  := 0;
  End;
  GetPaletteEntries(HPal,0,256,SysPal.LPal.PalPalEntry[0]);
  For I := 0 To 255 Do
  Begin
    SavePal[I,1] := (((Syspal.LPal.PalPalEntry[I].peRed))   Div 4);
    SavePal[I,2] := (((Syspal.LPal.PalPalEntry[I].peGreen)) Div 4);
    SavePal[I,3] := (((Syspal.LPal.PalPalEntry[I].peBlue))  Div 4);
  End;
End;

Procedure MakeGenPalette;
Var
  X     : Integer;
  R,G,B : Integer;

Begin
  X := 0;
  For R := 0 To 7 Do
  Begin
    For G := 0 To 7 Do
    Begin
      For B := 0 To 3 Do
      Begin
        PaletteVga[X,1] := (R + 1) * 8  - 1;
        PaletteVga[X,2] := (G + 1) * 8  - 1;
        PaletteVga[X,3] := (B + 1) * 16 - 1;
        Inc(X);
      End;
    End;
  End;
End;

Function ShouldIKeepOriginalFormat(Convert: TConvertBPP; Out BPP: Integer):Boolean;
Begin
  // Choices
  //    Use File Colors
  //    Force 256 Colors
  //    Force 16M Colors

  If Convert = cbNoConversion
   Then Result := True
   Else Result := False;
  If Convert = cbTo8Bit  Then BPP := 8;
  If Convert = cbTo24Bit Then BPP := 24;
  If Convert = cbTo32Bit Then BPP := 32;
End;

Procedure ReadTGAFileHeader(Var TGAHead: TTGAHeader;
                            Var FileOk: Boolean;
                            Var ErrorString: String;
                            Out Width: Integer;
                            Out Height: Integer;
                            Out BitsPerPixel: Integer;
                            Out Compressed: Boolean);
Var
  I,W1       : Integer;
  DummyArray : Packed Array[1..4048] Of Char;
  
Begin
  // Read Targa Header

  FastGetBytes(TGAHead,SizeOf(TTGAHeader));
  If Not (TGAHead.ImageType In [1,2,4,9,10,11]) Then
  Begin
    ErrorString := 'Invalid TGA file!';
    FileOk      := False;
    Exit;
  End;
  If Not (TGAHead.Bits In [1,4,8,16,24,32]) Then
  Begin
    ErrorString := 'Invalid TGA file!';
    FileOk      := False;
    Exit;
  End;
  Width        := TGAHead.Width;
  Height       := TGAHead.Height;
  BitsPerPixel := TGAHead.Bits;
  FastGetBytes(DummyArray,TGAHead.IdentSize);

  // Read in colormap

  For I := 0 To 255 Do
  Begin
    PaletteVGA[I,1] := 0;
    PaletteVGA[I,2] := 0;
    PaletteVGA[I,3] := 0;
  End;
  If TGAHead.ColorMapType <> 0 Then
  Begin
    Case TGAHead.ColorMapBits Of
      24:
      Begin
        For I := TGAHead.ColorMapStart To TGAHead.ColorMapStart + TGAHead.ColorMapLength - 1 Do
        Begin
          PaletteVGA[I,3] := FastGetByte Div 4;
          PaletteVGA[I,2] := FastGetByte Div 4;
          PaletteVGA[I,1] := FastGetByte Div 4;
        End;
      End;
      16:
      Begin
        For I := TGAHead.ColorMapStart To TGAHead.ColorMapStart + TGAHead.ColorMapLength - 1 Do
        Begin
          W1 := FastGetWord;
          PaletteVGA[I,3] := ((W1 Shr 10) And $1F) Shl 1;
          PaletteVGA[I,2] := ((W1 Shr  5) And $1F) Shl 1;
          PaletteVGA[I,1] := ((W1 Shr  0) And $1F) Shl 1;
        End;
      End;
    End;
  End;
  If (BitsPerPixel = 8) And (TGAHead.ColorMapType = 0) Then SetUpMaskGrayPalette
  Else
  Begin
    If BitsPerPixel = 1 Then
    Begin
      PaletteVGA[0,1] := 0;
      PaletteVGA[0,2] := 0;
      PaletteVGA[0,3] := 0;
      PaletteVGA[1,1] := 63;
      PaletteVGA[1,2] := 63;
      PaletteVGA[1,3] := 63;
    End;
  End;
  Compressed := False;
  If TGAHead.ImageType In [9,10,11] Then Compressed := True;
End;

Procedure MakePalBW(Var SysPal: TFakePalette);
Begin
  SysPal.LPal.palVersion             := $300;
  SysPal.LPal.palNumEntries          := 2;
  Syspal.LPal.PalPalEntry[0].peRed   := 0;
  Syspal.LPal.PalPalEntry[0].peGreen := 0;
  Syspal.LPal.PalPalEntry[0].peBlue  := 0;
  Syspal.LPal.PalPalEntry[0].peFlags := 0;
  Syspal.Dummy[1].peRed              := 255;
  Syspal.Dummy[1].peGreen            := 255;
  Syspal.Dummy[1].peBlue             := 255;
  Syspal.Dummy[1].peFlags            := 0;
End;

Procedure MakePalPalette(Var SysPal: TFakePalette);
Var I: Integer;
Begin
  SysPal.LPal.palVersion    := $300;
  SysPal.LPal.palNumEntries := 256;
  For I := 0 To 255 Do
  Begin
    Syspal.LPal.PalPalEntry[I].peRed   := (PaletteVga[I,1]) * 4;
    Syspal.LPal.PalPalEntry[I].peGreen := (PaletteVga[I,2]) * 4;
    Syspal.LPal.PalPalEntry[I].peBlue  := (PaletteVga[I,3]) * 4;
    Syspal.LPal.PalPalEntry[I].peFlags := 0;
  End;
End;

Procedure MakePalPaletteX(Var SysPal: TFakePalette; HowMany: Word);
Var I: Integer;
Begin
  SysPal.LPal.palVersion    := $300;
  SysPal.LPal.palNumEntries := HowMany;
  For I := 0 To HowMany - 1 Do
  Begin
    Syspal.LPal.PalPalEntry[I].peRed   := (PaletteVga[I,1]) * 4;
    Syspal.LPal.PalPalEntry[I].peGreen := (PaletteVga[I,2]) * 4;
    Syspal.LPal.PalPalEntry[I].peBlue  := (PaletteVga[I,3]) * 4;
    Syspal.LPal.PalPalEntry[I].peFlags := 0;
  End;
End;

Procedure PixelSwapArray(Var TempArrayD; Wide: Integer; PixelFormat: TMyPixelFormat);
Var
  W,X,Y,Z                 : Integer;
  Byte1,Byte2,Byte3,Byte4 : Byte;

Begin
  // Should I do 1 byte pixel or 3 byte pixels

  Case PixelFormat Of
    mpf8Bit:
    Begin
      Y := Wide Div 2;
      Z := Wide - 1;
      For X := 0 To Y - 1 Do
      Begin
        Byte1 := TDataLineArray(TempArrayD)[X];
        TDataLineArray(TempArrayD)[X] := TDataLineArray(TempArrayD)[Z];
        TDataLineArray(TempArrayD)[Z] := Byte1;
        Dec(Z);
      End;
    End;
    mpf24Bit:
    Begin
      Y := (Wide Div 3) Div 2;
      Z := Wide - 3;
      W := 0;
      For X := 0 To Y - 1 Do
      Begin
        Byte1 := TDataLineArray(TempArrayD)[W + 0];
        Byte2 := TDataLineArray(TempArrayD)[W + 1];
        Byte3 := TDataLineArray(TempArrayD)[W + 2];
        TDataLineArray(TempArrayD)[W + 0] := TDataLineArray(TempArrayD)[Z + 0];
        TDataLineArray(TempArrayD)[W + 1] := TDataLineArray(TempArrayD)[Z + 1];
        TDataLineArray(TempArrayD)[W + 2] := TDataLineArray(TempArrayD)[Z + 2];
        TDataLineArray(TempArrayD)[Z + 0] := Byte1;
        TDataLineArray(TempArrayD)[Z + 1] := Byte2;
        TDataLineArray(TempArrayD)[Z + 2] := Byte3;
        Dec(Z,3);
        Inc(W,3);
      End;
    End;
    mpf32Bit:
    Begin
      Y := (Wide Div 4) Div 2;
      Z := Wide - 4;
      W := 0;
      For X := 0 To Y - 1 Do
      Begin
        Byte1 := TDataLineArray(TempArrayD)[W + 0];
        Byte2 := TDataLineArray(TempArrayD)[W + 1];
        Byte3 := TDataLineArray(TempArrayD)[W + 2];
        Byte4 := TDataLineArray(TempArrayD)[W + 3];
        TDataLineArray(TempArrayD)[W + 0] := TDataLineArray(TempArrayD)[Z + 0];
        TDataLineArray(TempArrayD)[W + 1] := TDataLineArray(TempArrayD)[Z + 1];
        TDataLineArray(TempArrayD)[W + 2] := TDataLineArray(TempArrayD)[Z + 2];
        TDataLineArray(TempArrayD)[W + 3] := TDataLineArray(TempArrayD)[Z + 3];
        TDataLineArray(TempArrayD)[Z + 0] := Byte1;
        TDataLineArray(TempArrayD)[Z + 1] := Byte2;
        TDataLineArray(TempArrayD)[Z + 2] := Byte3;
        TDataLineArray(TempArrayD)[Z + 3] := Byte4;
        Dec(Z,4);
        Inc(W,4);
      End;
    End;
  End;
End;

Procedure TGAReverse(Const TGAHead: TTGAHeader; Var TempArrayD: TDataLineArray; TrueLineBytes: Integer; PixelFormat: TMyPixelFormat);
Begin
  If TGAHead.Descriptor And $10 <> 0 Then PixelSwapArray(TempArrayD,TrueLineBytes,PixelFormat);
End;

Procedure TGAMono2Vga(Width: Integer);
Var I: SmallInt;
Begin
  For I := 0 To Width - 1 Do
  Begin
    If ((TempArrayD^[I] Shr 3) And MaskTable[I And 7]) <> 0
     Then TempArrayD2^[I] := 1
     Else TempArrayD2^[I] := 0;
  End;
  Move(TempArrayD2^[0],TempArrayD^[0],Width);
End;

Function Pixels2Bytes(Width: Integer): Integer;
Begin
  Pixels2Bytes := (Width + 7) Div 8;
End;

Procedure ReadTGALine(Const TGAHead: TTGAHeader; LineBytes: Integer; PremultiplyAlphas: Boolean; Width,BitsPerPixel,ConvertToBits: Integer; KeepOriginalFormat: Boolean);
Var
  N,Size,LineSize : SmallInt;
  W1,Z            : Integer;
  R,G,B,B1        : Byte;

  Procedure TGA16_ANY_U(Var Z: Integer; Var TempArrayD; Width: Integer);

    Procedure Do8;
    Var
      W1,I  : Integer;
      R,G,B : Byte;

    Begin
      For I := 0 To Width - 1 Do
      Begin
        W1 := FastGetWord;
        R  := ((W1 Shr 10) And $1F) Shl 3;
        G  := ((W1 Shr  5) And $1F) Shl 3;
        B  := ((W1 Shr  0) And $1F) Shl 3;
        TDataLineArray(TempArrayD)[Z] := PcxGrayValue(R,G,B);
        Inc(Z);
      End;
    End;

    Procedure Do24;
    Var
      W1,I  : Integer;
      R,G,B : Byte;

    Begin
      For I := 0 To Width - 1 Do
      Begin
        W1 := FastGetWord;
        R  := ((W1 Shr 10) And $1F) Shl 3;
        G  := ((W1 Shr  5) And $1F) Shl 3;
        B  := ((W1 Shr  0) And $1F) Shl 3;
        TDataLineArray(TempArrayD)[Z + 0] := B;
        TDataLineArray(TempArrayD)[Z + 1] := G;
        TDataLineArray(TempArrayD)[Z + 2] := R;
        Inc(Z,Global_HiColor);
      End;
    End;

    Procedure Do32;
    Var
      W1,I  : Integer;
      R,G,B : Byte;

    Begin
      For I := 0 To Width - 1 Do
      Begin
        W1 := FastGetWord;
        R  := ((W1 Shr 10) And $1F) Shl 3;
        G  := ((W1 Shr  5) And $1F) Shl 3;
        B  := ((W1 Shr  0) And $1F) Shl 3;
        TDataLineArray(TempArrayD)[Z + 0] := B;
        TDataLineArray(TempArrayD)[Z + 1] := G;
        TDataLineArray(TempArrayD)[Z + 2] := R;
        TDataLineArray(TempArrayD)[Z + 3] := 255;
        Inc(Z,Global_HiColorAlpha);
      End;
    End;

  Begin
    If KeepOriginalFormat Then Do24
    Else
    Begin
      Case ConvertToBits Of
        8: Do8;
       24: Do24;
       32: Do32;
      End;
    End;
  End;

  Procedure TGA24_ANY_U(Var Z: Integer; Var TempArrayD; Width: Integer);
  Var RGB: TypeRGB;

    Procedure Do8;
    Var I: Integer;
    Begin
      For I := 0 To Width - 1 Do
      Begin
        FastGetBytes(RGB,3);
        TDataLineArray(TempArrayD)[Z] := PcxGrayValue(RGB.R,RGB.G,RGB.B);
        Inc(Z);
      End;
    End;

    Procedure Do24;
    Var I: Integer;
    Begin
      For I := 0 To Width - 1 Do
      Begin
        TDataLineArray(TempArrayD)[Z + 0] := FastGetByte;
        TDataLineArray(TempArrayD)[Z + 1] := FastGetByte;
        TDataLineArray(TempArrayD)[Z + 2] := FastGetByte;
        Inc(Z,Global_HiColor);
      End;
    End;

    Procedure Do32;
    Var I: Integer;
    Begin
      For I := 0 To Width - 1 Do
      Begin
        TDataLineArray(TempArrayD)[Z + 0] := FastGetByte;
        TDataLineArray(TempArrayD)[Z + 1] := FastGetByte;
        TDataLineArray(TempArrayD)[Z + 2] := FastGetByte;
        TDataLineArray(TempArrayD)[Z + 3] := 255;
        Inc(Z,Global_HiColorAlpha);
      End;
    End;

  Begin
    If (ConvertToBits <> 24) And Not KeepOriginalFormat Then
    Begin
      Case ConvertToBits Of
        8: Do8;
       24: Do24;
       32: Do32;
      End;
    End
    Else
    Begin
      FastGetBytes(TDataLineArray(TempArrayD)[Z],Width * Global_HiColor);
      Inc(Z,Global_HiColor * Width);
    End;
  End;

  Procedure TGA32_ANY_U(Var Z: Integer; Var TempArrayD; Width: Integer; PremultiplyAlphas: Boolean);
  Var RGB: TypeRGB;

    Procedure Do8;
    Var
      I       : Integer;
      B,G,R,A : Byte;

    Begin
      For I := 0 To Width - 1 Do
      Begin
        FastGetBytes(RGB,3);
        A := FastGetByte;
        If PremultiplyAlphas Then
        Begin
          B := PreMult[A,B];
          G := PreMult[A,G];
          R := PreMult[A,R];
        End;
        TDataLineArray(TempArrayD)[Z] := PcxGrayValue(RGB.R,RGB.G,RGB.B);
        Inc(Z);
      End;
    End;

    Procedure Do24;
    Var
      I       : Integer;
      B,G,R,A : Byte;

    Begin
      For I := 0 To Width - 1 Do
      Begin
        B := FastGetByte;
        G := FastGetByte;
        R := FastGetByte;
        A := FastGetByte;
        If PremultiplyAlphas Then
        Begin
          B := PreMult[A,B];
          G := PreMult[A,G];
          R := PreMult[A,R];
        End;
        TDataLineArray(TempArrayD)[Z + 0] := B;
        TDataLineArray(TempArrayD)[Z + 1] := G;
        TDataLineArray(TempArrayD)[Z + 2] := R;
        Inc(Z,Global_HiColor);
      End;
    End;

    Procedure Do32;
    Var
      I       : Integer;
      B,G,R,A : Byte;

    Begin
      For I := 0 To Width - 1 Do
      Begin
        B := FastGetByte;
        G := FastGetByte;
        R := FastGetByte;
        A := FastGetByte;
        If PremultiplyAlphas Then
        Begin
          B := PreMult[A,B];
          G := PreMult[A,G];
          R := PreMult[A,R];
        End;
        TDataLineArray(TempArrayD)[Z + 0] := B;
        TDataLineArray(TempArrayD)[Z + 1] := G;
        TDataLineArray(TempArrayD)[Z + 2] := R;
        TDataLineArray(TempArrayD)[Z + 3] := A;
        Inc(Z,Global_HiColorAlpha);
      End;
    End;

    Procedure PremultiplyRGBA;
    Var
      I       : Integer;
      R,G,B,A : Byte;

    Begin
      For I := 0 To Width - 1 Do
      Begin
        B := TDataLineArray(TempArrayD)[I * 4 + 0];
        G := TDataLineArray(TempArrayD)[I * 4 + 1];
        R := TDataLineArray(TempArrayD)[I * 4 + 2];
        A := TDataLineArray(TempArrayD)[I * 4 + 3];
        If PremultiplyAlphas Then
        Begin
          B := PreMult[A,B];
          G := PreMult[A,G];
          R := PreMult[A,R];
        End;
        TDataLineArray(TempArrayD)[I * 4 + 0] := B;
        TDataLineArray(TempArrayD)[I * 4 + 1] := G;
        TDataLineArray(TempArrayD)[I * 4 + 2] := R;
      End; // For I
    End;

  Begin
    If PremultiplyAlphas Or ((ConvertToBits <> 32) And Not KeepOriginalFormat) Then
    Begin
      Case ConvertToBits Of
        8: Do8;
       24: Do24;
       32: Do32;
      End;
    End
    Else
    Begin
      FastGetBytes(TDataLineArray(TempArrayD)[Z],Width * Global_HiColorAlpha);
      PremultiplyRGBA;
      Inc(Z,Global_HiColorAlpha * Width);
    End;
  End;

  Procedure Do8;
  Var I: Integer;
  Begin
    For I := 0 To Size - 1 Do
    Begin
      TempArrayD^[Z] := PcxGrayValue(R,G,B);
      Inc(Z);
    End;
  End;

  Procedure Do8Raw;
  Begin
    FastGetBytes(TempArrayD^[0],Width);
  End;

  Procedure Do8RawPart;
  Begin
    FastGetBytes(TempArrayD^[Z],Size);
    Inc(Z,Size);
  End;

  Procedure Do8Fill(B1:Byte);
  Begin
    FillerUp(TempArrayD^[Z],Size,B1);
    Inc(Z,Size);
  End;

  Procedure Do24Raw;
  Var I,Z: Integer;
  Begin
    Z := 0;
    For I := 0 To Width - 1 Do
    Begin
      B1                 := FastGetByte;
      TempArrayD^[Z + 0] := PaletteVGA[B1,3] * 4 + 3;
      TempArrayD^[Z + 1] := PaletteVGA[B1,2] * 4 + 3;
      TempArrayD^[Z + 2] := PaletteVGA[B1,1] * 4 + 3;
      Inc(Z,Global_HiColor);
    End;
  End;

  Procedure Do32Raw;
  Var I,Z: Integer;
  Begin
    Z := 0;
    For I := 0 To Width - 1 Do
    Begin
      B1                 := FastGetByte;
      TempArrayD^[Z + 0] := PaletteVGA[B1,3] * 4 + 3;
      TempArrayD^[Z + 1] := PaletteVGA[B1,2] * 4 + 3;
      TempArrayD^[Z + 2] := PaletteVGA[B1,1] * 4 + 3;
      TempArrayD^[Z + 3] := 255;
      Inc(Z,Global_HiColorAlpha);
    End;
  End;

  Procedure Do24RawPart;
  Var I: Integer;
  Begin
    For I := 0 To Size - 1 Do
    Begin
      B1                 := FastGetByte;
      TempArrayD^[Z + 0] := PaletteVGA[B1,3] * 4 + 3;
      TempArrayD^[Z + 1] := PaletteVGA[B1,2] * 4 + 3;
      TempArrayD^[Z + 2] := PaletteVGA[B1,1] * 4 + 3;
      Inc(Z,Global_HiColor);
    End;
  End;

  Procedure Do32RawPart;
  Var I: Integer;
  Begin
    For I := 0 To Size - 1 Do
    Begin
      B1                 := FastGetByte;
      TempArrayD^[Z + 0] := PaletteVGA[B1,3] * 4 + 3;
      TempArrayD^[Z + 1] := PaletteVGA[B1,2] * 4 + 3;
      TempArrayD^[Z + 2] := PaletteVGA[B1,1] * 4 + 3;
      TempArrayD^[Z + 3] := 255;
      Inc(Z,Global_HiColorAlpha);
    End;
  End;

  Procedure Do24Fill(B1:Byte);
  Var
    I     : Integer;
    R,G,B : Byte;

  Begin
    R := PaletteVGA[B1,1] * 4 + 3;
    G := PaletteVGA[B1,2] * 4 + 3;
    B := PaletteVGA[B1,3] * 4 + 3;
    For I := 0 To Size - 1 Do
    Begin
      TempArrayD^[Z + 0] := B;
      TempArrayD^[Z + 1] := G;
      TempArrayD^[Z + 2] := R;
      Inc(Z,Global_HiColor);
    End;
  End;

  Procedure Do32Fill(B1:Byte);
  Var
    I     : Integer;
    R,G,B : Byte;

  Begin
    R := PaletteVGA[B1,1] * 4 + 3;
    G := PaletteVGA[B1,2] * 4 + 3;
    B := PaletteVGA[B1,3] * 4 + 3;
    For I := 0 To Size - 1 Do
    Begin
      TempArrayD^[Z + 0] := B;
      TempArrayD^[Z + 1] := G;
      TempArrayD^[Z + 2] := R;
      TempArrayD^[Z + 2] := 255;
      Inc(Z,Global_HiColorAlpha);
    End;
  End;

  Procedure Do24;
  Var I: Integer;
  Begin
    For I := 0 To Size - 1 Do
    Begin
      TempArrayD^[Z + 0] := B;
      TempArrayD^[Z + 1] := G;
      TempArrayD^[Z + 2] := R;
      Inc(Z,Global_HiColor);
    End;
  End;

  Procedure Do32;
  Var I: Integer;
  Begin
    For I := 0 To Size - 1 Do
    Begin
      TempArrayD^[Z + 0] := B;
      TempArrayD^[Z + 1] := G;
      TempArrayD^[Z + 2] := R;
      TempArrayD^[Z + 3] := B1;
      Inc(Z,Global_HiColorAlpha);
    End;
  End;

Begin
  N := 0;
  If BitsPerPixel = 1
   Then LineSize := Pixels2Bytes(Width)
   Else LineSize := Width;

  // Uncompressed Lines

  If TGAHead.ImageType In [1,2,3] Then
  Begin
    Case BitsPerPixel Of
      1: FastGetBytes(TempArrayD^[0],LineBytes);
      8:
      Begin
        If KeepOriginalFormat Then Do8Raw
        Else
        Begin
          Case ConvertToBits Of
            8: Do8Raw;
           24: Do24Raw;
           32: Do32Raw;
          End;
        End;
      End;
      16:
      Begin
        Z := 0;
        TGA16_ANY_U(Z,TempArrayD^[0],Width);
      End;
      24:
      Begin
        Z := 0;
        TGA24_ANY_U(Z,TempArrayD^[0],Width);
      End;
      32:
      Begin
        Z := 0;
        TGA32_ANY_U(Z,TempArrayD^[0],Width,PremultiplyAlphas);
      End;
    End;
  End
  Else
  Begin
    // Compressed Lines

    Z := 0;
    Repeat
      B1   := FastGetByte;
      Size := (B1 And $7F) + 1;
      N    := N + Size;
      If (B1 And $80) <> 0 Then
      Begin
        Case BitsPerPixel Of
          1,8:
          Begin
            B1 := FastGetByte;
            If KeepOriginalFormat Then Do8Fill(B1)
            Else
            Begin
              Case ConvertToBits Of
                8: Do8Fill(B1);
               24: Do24Fill(B1);
               32: Do32Fill(B1);
              End;
            End
          End;
          16:
          Begin
            W1 := FastGetWord;
            R  := ((W1 Shr 10) And $1F) Shl 3;
            G  := ((W1 Shr  5) And $1F) Shl 3;
            B  := ((W1 Shr  0) And $1F) Shl 3;
            B1 := 255;
            If KeepOriginalFormat Then Do24
            Else
            Begin
              Case ConvertToBits Of
                8: Do8;
               24: Do24;
               32: Do32;
              End;
            End;
          End;
          24,32:
          Begin
            B := FastGetByte;
            G := FastGetByte;
            R := FastGetByte;
            If BitsPerPixel = 32 Then
            Begin
              B1 := FastGetByte;
              If PremultiplyAlphas Then
              Begin
                B := PreMult[B1,B];
                G := PreMult[B1,G];
                R := PreMult[B1,R];
              End;
            End
            Else B1 := 255;
            If KeepOriginalFormat Then
            Begin
              If BitsPerPixel = 24
               Then Do24
               Else Do32;
            End
            Else
            Begin
              Case ConvertToBits Of
                8: Do8;
               24: Do24;
               32: Do32;
              End;
            End;
          End;
        End;
      End
      Else
      Begin
        // Single bytes

        Case BitsPerPixel Of
          1,8:
          Begin
            If KeepOriginalFormat Then Do8RawPart
            Else
            Begin
              Case ConvertToBits Of
                8: Do8RawPart;
               24: Do24RawPart;
               32: Do32RawPart;
              End;
            End;
          End;
          16: TGA16_ANY_U(Z,TempArrayD^[0],Size);
          24: TGA24_ANY_U(Z,TempArrayD^[0],Size);
          32: TGA32_ANY_U(Z,TempArrayD^[0],Size,PremultiplyAlphas);
        End;
      End;
    Until N >= LineSize;
  End;
End;

Function GetMyPixelFormat(PF: TPixelFormat): TMyPixelFormat;
Begin
  Case PF Of
    pf1Bit:  Result := mpf1bit;
    pf4Bit:  Result := mpf4bit;
    pf8Bit:  Result := mpf8bit;
    pf15Bit: Result := mpf15bit;
    pf16Bit: Result := mpf16bit;
    pf24Bit: Result := mpf24bit;
    pf32Bit: Result := mpf32bit;
  Else
    Result := mpf32Bit;
  End;
End;

Procedure LoadFromFileX(FileName: String; Const Bitmap: TBitmap; PreMultiplyAlphas: Boolean; Convert: TConvertBPP);
Var
  II                 : Integer;
  NewWidth           : Integer;
  TrueLineBytes      : Integer;
  LineBytes          : Integer;
  StartLine          : Integer;
  IncLine            : Integer;
  I                  : Integer;
  Ptr1               : Pointer;
  Width,Height       : Integer;
  ConvertToBits      : Integer;
  KeepOriginalFormat : Boolean;
  BitsPerPixel       : Integer;
  Compressed         : Boolean;
  FileOk             : Boolean;
  ErrorString        : String;
  SysPal             : TFakePalette;
  TGAHead            : TTGAHeader;

Begin
  // Read Targa File

  KeepOriginalFormat := ShouldIKeepOriginalFormat(Convert,ConvertToBits);
  ErrorString          := '';
  FileOk               := True;
  OpenFile(FileName,FileOk);
  ReadTgaFileHeader(TGAHead,FileOK,ErrorString,Width,Height,BitsPerPixel,Compressed);
  If KeepOriginalFormat Then ConvertToBits := BitsPerPixel;
  If FileOk Then
  Begin
    // Set the bitmap size

    BitMap.Height := Height;
    BitMap.Width  := Width;
    Case BitsPerPixel Of
      1:
      Begin
        BitMap.PixelFormat := pf1bit;
        MakePalBW(SysPal);
        Bitmap.Palette := CreatePalette(Syspal.LPal);
      End;
      8:
      Begin
        BitMap.PixelFormat := pf8bit;
        MakePalPalette(SysPal);
        Bitmap.Palette := CreatePalette(Syspal.LPal);
      End;
      16: BitMap.PixelFormat := pf24bit;
      24,32:
      Begin
        If (KeepOriginalFormat And (BitsPerPixel = 32)) Or (ConvertToBits = 32)
         Then BitMap.PixelFormat := pf32bit
         Else BitMap.PixelFormat := pf24bit;
      End;
    End;
    Case BitsPerPixel Of
      1,8:
      Begin
        If KeepOriginalFormat Then
        Begin
          // Do nothing
        End
        Else
        Begin
          Case ConvertToBits Of
            8: BitMap.PixelFormat := pf8bit;
            24:
            Begin
              BitMap.PixelFormat := pf24bit;
              If BitsPerPixel <> 8 Then MakeGenPalette;
            End;
            32:
            Begin
              BitMap.PixelFormat := pf32bit;
              If BitsPerPixel <> 8 Then MakeGenPalette;
            End;
          End;
          MakePalPalette(SysPal);
          Bitmap.Palette := CreatePalette(Syspal.LPal);
        End;
      End;
      16,24,32:
      Begin
        If KeepOriginalFormat Then MakeGenPalette
        Else
        Begin
          Case ConvertToBits Of
            8:
            Begin
              BitMap.PixelFormat := pf8bit;
              SetUpMaskGrayPalette;
            End;
            24,32: MakeGenPalette;
          End;
        End;
        MakePalPalette(SysPal);
        Bitmap.Palette := CreatePalette(Syspal.LPal);
      End;
    End;

    // Load the image data

    If ((BitsPerPixel = 32) And KeepOriginalFormat) Or (ConvertToBits = 32)
     Then NewWidth := Width * Global_HiColorAlpha
     Else NewWidth := Width * Global_HiColor;
    Try
      GetMem(TempArrayD,NewWidth);
      Try
        GetMem(TempArrayD2,NewWidth);
        If BitsPerPixel = 1 Then LineBytes := Pixels2Bytes(Width)
        Else
        Begin
               If BitsPerPixel = 8 Then LineBytes := Width
          Else If (BitsPerPixel <> 32) Or ((ConvertToBits <> 32) And Not KeepOriginalFormat) Then LineBytes := Width * 3
          Else LineBytes := Width * 4;
        End;
        If KeepOriginalFormat Then TrueLineBytes := LineBytes
        Else
        Begin
          Case ConvertToBits Of
            8: TrueLineBytes := Width;
           24: TrueLineBytes := Width * Global_HiColor;
           32: TrueLineBytes := Width * Global_HiColorAlpha;
          End;
        End;
        If (TGAHead.Descriptor And $20) = 0 Then
        Begin
          StartLine := Height - 1;
          IncLine   := -1;
        End
        Else
        Begin
          StartLine := 0;
          IncLine   := 1;
        End;
        I  := StartLine;
        II := 0;

        If TGAHead.ImageType In [1,2,3,9,10,11] Then
        Begin
          Repeat
            ReadTGALine(TGAHead,LineBytes,PremultiplyAlphas,Width,BitsPerPixel,ConvertToBits,KeepOriginalFormat);
            Case BitsPerPixel Of
              1: TGAMono2Vga(Width);
            End;
            TGAReverse(TGAHead,TempArrayD^,TrueLineBytes,GetMyPixelFormat(Bitmap.PixelFormat));
            Ptr1 := BitMap.ScanLine[I];

            // Copy the data

            Move(TempArrayD^,Ptr1^,TrueLineBytes);
            Inc(II);
            I := I + IncLine;
          Until II >= Height;
        End;
      Finally
        FreeMem(TempArrayD2);
      End;
    Finally
      FreeMem(TempArrayD);
    End;
  End
  Else
  Begin
    // We failed to load the header or it was bad so create a 1x1x256 bitmap

    BitMap.Width  := 1;
    BitMap.Height := 1;
    BitsPerPixel  := 8;
  End;
  If IoResult <> 0 Then ;
  Close(PictureFile);
  If IoResult <> 0 Then ;
End;

Procedure SaveToFileX(FileName: String; Const BitMap: TBitmap; MyPcxType: Byte);
Var
  ResultStatus     : Boolean;
  File1            : File;
  TGAHead          : TTGAHeader;
  MyWidth          : Word;
  MyHeight         : Word;
  CurrBitsPerPixel : Word;
  NewLine          : PDataLineArray;
  SysPal           : TFakePalette;

  Procedure TGAWrite256Palette;
  Var
    X,Y : Word;
    B1  : Byte;

  Begin
    For X := 0 To 255 Do
    Begin
      For Y := 3 DownTo 1 Do
      Begin
        B1 := (PaletteVga[X,Y] * 255) Div 63;
        BlockWrite(File1,B1,1);
      End;
    End;
  End;

Const TGADescriptor: String[60] = 'TurboView(GIF-REED) produced this TARGA file!' + Chr($1A);

  Procedure TGAWriteHeader;
  Begin
    TGAHead.IdentSize := Length(TGADescriptor);
    If MyPcxType = 1 Then
    Begin
      TGAHead.ColorMapType   := 1;
      TGAHead.ImageType      := 1;
      TGAHead.ColorMapStart  := 0;
      TGAHead.ColorMapLength := 256;
      TGAHead.ColorMapBits   := 24;
    End
    Else
    Begin
      TGAHead.ColorMapType   := 0;
      TGAHead.ImageType      := 2;
      TGAHead.ColorMapStart  := 0;
      TGAHead.ColorMapLength := 0;
      TGAHead.ColorMapBits   := 24;
    End;
    TGAHead.XStart := 0;
    TGAHead.YStart := 0;
    TGAHead.Width  := MyWidth;
    TGAHead.Height := MyHeight;
    Case MyPcxType Of
      1:TGAHead.Bits := 8;
      2:TGAHead.Bits := 24;
    End;
    TGAHead.Descriptor := $20;
    BlockWrite(File1,TGAHead,SizeOf(TGAHead));
    BlockWrite(File1,TGADescriptor[1],Length(TGADescriptor));
    If TGAHead.ColorMapType = 1 Then TGAWrite256Palette;
  End;

  Procedure TGAWriteBody(Var ResultStatus: Boolean);
  Var
    Width_24 : Word;
    I        : Word;

  Begin
    Width_24     := MyWidth * 3;
    I            := 0;
    ResultStatus := True;
    
    Repeat
      TempArrayD := BitMap.ScanLine[I];
      Case MyPcxType Of
        1:
        Begin
          Case CurrBitsPerPixel Of
            1: ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,1,8,MyWidth);
            4: ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,4,8,MyWidth);
            8: ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,8,8,MyWidth);
          End;
          BlockWrite(File1,TempArrayDBIG^[0],MyWidth);
        End;
        2:
        Begin
          Case CurrBitsPerPixel Of
            1: ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,1, 24,MyWidth);
            4: ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,4, 24,MyWidth);
            8: ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,8, 24,MyWidth);
           24: ConvertXBitsToYBits(TempArrayD^,TempArrayDBIG^,24,24,MyWidth);
          End;
          BlockWrite(File1,TempArrayDBIG^[0],Width_24);
        End;
      End;
      Inc(I);
    Until (I >= MyHeight) Or Not ResultStatus;
  End;

Begin
  // Write TARGA files

  SaveThePalette(BitMap.Palette,PaletteVGA,SysPal);
  MyWidth  := BitMap.Width;
  MyHeight := BitMap.Height;
  Case BitMap.PixelFormat Of
    pf1bit:  CurrBitsPerPixel := 1;
    pf4bit:  CurrBitsPerPixel := 4;
    pf8bit:  CurrBitsPerPixel := 8;
    pf24bit: CurrBitsPerPixel := 24;
  End;
  GetMem(NewLine,MyWidth * 4);
  GetMem(TempArrayDBig,MyWidth * 4);
  Assign(File1,FileName);
  ReWrite(File1,1);
  TGAWriteHeader;
  TGAWriteBody(ResultStatus);
  Close(File1);
  FreeMem(TempArrayDBig,MyWidth * 4);
  FreeMem(NewLine,MyWidth * 4);
End;

Procedure InitPremult;
Var A,X: Byte;
Begin
  For A := 0 To 255 Do
  Begin
    For X := 0 To 255 Do PreMult[A,X] := Round(A * X / 255);
  End;
End;

Initialization
  InitPremult;
End.
