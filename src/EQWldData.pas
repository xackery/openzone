Unit EQWldData;
// --------------------------------------------------------------------------------
// Copyright (C) HackersQuest  (www.hackersquest.gomp.ch / www.ethernalquest.org)
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
//
// WldData.cpp: implementation of the CWldData class.
//
//////////////////////////////////////////////////////////////////////

Interface

Uses Classes,     dialogs;

Const
  MAX_FILE = 16;
  Version1WLD = $00015500;
  Version2WLD = $1000C800;

Type
  BPtr    = ^Byte;
  IPtr    = ^Integer;
  WPtr    = ^Word;
  SmIPtr  = ^SmallInt;
  LPtr    = ^LongWord;
  FPtr    = ^Single;
  DPtr    = ^Double;

  TByteArray = Packed Array Of Byte;

  TBuffer = Class
  Public
    Buffer       : BPtr;
    Position     : LongWord;
    Len          : LongWord;
    BufferLength : LongWord;
    Allocated    : Boolean;
    Constructor Create;                              Overload;
    Constructor Create(B: Pointer; L: LongWord = 0); Overload;
    Destructor  Destroy;                             Override;
    Procedure   Init(B: Pointer; L: LongWord = 0);
    Procedure   Clear;
    Function    Allocate(Size: LongWord): Boolean;
    Function    GetInt: Integer;
    Procedure   PutDWORD(D: LongWord);
    Procedure   PutWORD(D: Word);
    Procedure   PutFLOAT(F: Single);
    Procedure   PutBYTE(Data: Pointer; Size: LongWord);
    Function    GetDWORD: LongWord;
    Function    GetWORD: Word;
    Function    GetBYTE(_Len: LongWord = 1): BPtr;
    Function    GetFLOAT: Single;
    Function    GetDOUBLE: Double;
    Function    AtEnd: Boolean;
  End;

  EQWldDataFragment = Class;

  TEQWldData = Class
  Public
    MaxFragment   : LongWord;
    nFragment     : LongWord;
    Fragments     : Packed Array Of EQWldDataFragment;
    NameHash      : TByteArray;
    NewType       : Boolean;
    FragmentNames : TStringList;
    Constructor Create;
    Destructor  Destroy; Override;
    Function    GetName(Position: Integer): String;
    Function    GetFragment(Position: Integer): EQWldDataFragment;
    Function    GetFragmentIndex(F: EQWldDataFragment): Integer;   Overload;
    Function    GetFragmentIndex(F,T: EQWldDataFragment): Integer; Overload;
    Procedure   FreeAll;
    Function    Decode(Stream: TStream): Boolean; Overload;
    Function    Decode(P: Pointer): Boolean; Overload;
    Procedure   PrintAsc(Var F: System.Text);
    Procedure   Encode(Stream: TStream);
  End;

  DataPair = Packed Record
    Data1 : LongWord;
    Data2 : Single;
  End;

  FragmentReference = Class;

  EQWldDataFragment = Class
  Protected
    FID    : LongWord;
    FName  : String;
    Parent : TEQWldData;
    FIndex : LongWord;
  Public
    NameHashPos : LongInt;
    Constructor Create(Data: TEQWldData; _ID: LongWord; _Name: String);
    Destructor  Destroy;                          Override;
    Procedure   Clear;
    Function    Decode(Buffer: TBuffer): Boolean; Dynamic;
    Procedure   Encode(Buffer: TBuffer);          Dynamic;
    Procedure   PrintAsc(Var F: System.Text);     Dynamic;
    Function    LoadAsc(B: TBuffer): Boolean;     Dynamic;
    Function    DecodeNameReference(Buffer: TBuffer; Var NewName: String): Boolean;
    Function    DecodeFragmentReference(Buffer: TBuffer): FragmentReference;
    Procedure   DecodeDataPair(Buffer: TBuffer; Var Result: DataPair);
    Procedure   EncodeDataPair(Buffer: TBuffer; Result: DataPair);
    Procedure   PrintAscHeader(Var F: System.Text);
    Procedure   PrintAscFooter(Var F: System.Text);
    Function    GetFragmentReferences: TStringList; Dynamic;
    Property Name  : String   Read FName;
    Property ID    : LongWord Read FID;
    Property Index : LongWord Read FIndex;
  End;

  FragmentReference = Class
    Name     : String;
    Fragment : EQWldDataFragment;
    Position : LongInt;
    Constructor Create;                       Overload;
    Constructor Create(F: FragmentReference); Overload;
    Function    Equals(F: FragmentReference): FragmentReference;
    Function    Exclam: Boolean;
    Function    GetName: String;
    Procedure   SetName(NewName: String);
    Procedure   SetFragment(F: EQWldDataFragment);
  End;

  SubData01 = Class
  Public
    Flags    : LongWord;
    Params1  : LongWord;
    Params2  : LongWord;
    Params3  : Single;
    Params4  : Single;
    Params5  : Packed Array[0..2,0..2] Of LongWord;
    Fragment : FragmentReference;
    Size1    : LongInt;
    Data1    : Packed Array Of Byte;//BYTE   (*Data1)[8];
    Constructor Create;
    Destructor  Destroy; Override;
    Function    Decode(Buffer: TBuffer; Parent: EQWldDataFragment): Boolean;
    Procedure   PrintAsc(Var F: System.Text; Prefix: String = '');
    Procedure   Clear;
  End;

  EQWldDataFragmentWithReference = Class(EQWldDataFragment)
  Public
    Flags    : LongWord;
    Fragment : FragmentReference;
    Constructor Create(Data: TEQWldData; D: LongWord; Name: String = '');
    Destructor  Destroy;                            Override;
    Function    Decode(Buffer: TBuffer): Boolean;   Override;
    Procedure   Clear;                              Dynamic;
    Function    BaseDecode(Buffer: TBuffer): Boolean;
    Procedure   BaseEncode(Buffer: TBuffer);
    Procedure   Encode(Buffer: TBuffer);            Override;
    Function    GetFragmentReferences: TStringList; Override;
  End;

  Data03 = Class(EQWldDataFragment)
  Public
    Size1 : LongInt;
    Data1 : Packed Array Of TByteArray;
    Sizes : Packed Array Of LongWord;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data04 = Class(EQWldDataFragment)
  Public
    Flags   : LongWord;
    Params1 : Packed Array[0..0] Of LongWord;
    Params2 : Packed Array[0..0] Of LongWord;
    Size1   : LongInt;
    Data1   : Packed Array Of FragmentReference;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data05 = Class(EQWldDataFragmentWithReference)
  Public
    Flags : LongWord;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
  End;

  Data06SubSubData = Packed Record
    Params1   : LongWord;
    Fragments : Packed Array Of FragmentReference;
  End;

  Data06SubData = Packed Record
    Params1 : LongWord;
    Flags   : LongWord;
    Size1   : LongInt;
    Data1   : Packed Array Of Data06SubSubData;
  End;

  Data06 = Class(EQWldDataFragment)
  Public
    Flags    : LongWord;
    SubSize1 : LongInt;
    Size1    : LongInt;
    Params1  : Packed Array[0..1] Of Single;
    Fragment : FragmentReference;
    Params2  : Single;
    Params3  : Packed Array[0..2] Of LongWord;
    Params4  : Single;
    Params5  : LongWord;
    Params6  : LongWord;
    Data1    : Packed Array Of Data06SubData;
    Params7  : SubData01;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data07 = Class(EQWldDataFragmentWithReference)
  Public
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
  End;

  Data09 = Class(EQWldDataFragmentWithReference)
    Flags     : LongWord;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean;   Override;
    Procedure   Encode(Buffer: TBuffer);            Override;
    Procedure   PrintAsc(Var F: System.Text);       Override;
  End;

  Data10SubData = Packed Record
    Name      : String;
    NamePos   : Integer;
    Flags     : LongWord;
    Fragment1 : FragmentReference;
    Fragment2 : FragmentReference;
    Size      : LongInt;
    Data      : Packed Array Of LongWord;
  End;

  Data10 = Class(EQWldDataFragment)
  Public
    Flags    : LongWord;
    Fragment : FragmentReference;
    Params1  : Packed Array[0..2] Of LongWord;
    Params2  : Single;
    Size1    : LongInt;
    Data1    : Packed Array Of Data10SubData;
    Size2    : LongInt;
    Data2    : Packed Array Of FragmentReference;
    Data3    : Packed Array Of LongWord;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data11 = Class(EQWldDataFragmentWithReference)
  Public
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
  End;
{
  Data12SubData = Packed Record
   Case Integer Of
     0: (Data4 : Packed Array Of LongWord);
     1: (Data8 : Packed Array Of LongWord);
   End;
}

  Data12SubData4 = Packed Record
    E0,E1,E2,E3       : SmallInt; // Euler parameters for rotation
    MoveXNumerator    : SmallInt;
    MoveYNumerator    : SmallInt;
    MoveZNumerator    : SmallInt;
    MoveDenominator   : SmallInt;
  End;

  Data12SubData8 = Packed Record
    E0,E1,E2,E3       : Single; // Euler parameters for rotation
    MoveXNumerator    : Single;
    MoveYNumerator    : Single;
    MoveZNumerator    : Single;
    MoveDenominator   : Single;
  End;

  Data12 = Class(EQWldDataFragment)
  Public
    Flags   : LongWord;
    Size1   : LongInt;
    Data4   : Packed Array Of Data12SubData4;//LongWord;
    Data8   : Packed Array Of Data12SubData8;//LongWord;
//    SubData : Data12SubData;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data13 = Class(EQWldDataFragmentWithReference)
  Public
    Flags   : LongWord;
    Params1 : Packed Array[0..0] Of LongWord;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
  End;

  Data14SubData = Packed Record
    Size : LongWord;
    Data : Packed Array Of DataPair;
  End;

  Data14 = Class(EQWldDataFragment)
  Public
    Flags     : LongWord;
    Fragment1 : FragmentReference;
    Fragment2 : FragmentReference;
    Params1   : Packed Array[0..0] Of LongWord;
    Params2   : Packed Array[0..6] Of LongWord;
    Size1     : LongInt;
    Size2     : LongInt;
    Size3     : LongInt;
    Data1     : Packed Array Of Data14SubData;
    Data2     : Packed Array Of FragmentReference;
    Data3     : TByteArray;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                            Override;
    Function    Decode(Buffer: TBuffer): Boolean;   Override;
    Procedure   Encode(Buffer: TBuffer);            Override;
    Procedure   PrintAsc(Var F: System.Text);       Override;
    Procedure   Clear;
    Function    GetFragmentReferences: TStringList; Override;
  End;

  Data15 = Class(EQWldDataFragmentWithReference)
    Flags     : LongWord;
    Fragment1 : FragmentReference;
    X         : Single;
    Y         : Single;
    Z         : Single;
    RotateZ   : Single;
    RotateY   : Single;
    RotateX   : Single;
    Params1   : Single;
    ScaleY    : Single;
    ScaleX    : Single;
    Fragment2 : FragmentReference;
    Params2   : LongWord;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy; Override;
    Function    Decode(Buffer: TBuffer): Boolean;   Override;
    Procedure   Encode(Buffer: TBuffer);            Override;
    Procedure   PrintAsc(Var F: System.Text);       Override;
    Function    GetFragmentReferences: TStringList; Override;
  End;

  Data16 = Class(EQWldDataFragment)
  Public
    Params1 : Single;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean;   Override;
    Procedure   Encode(Buffer: TBuffer);            Override;
    Procedure   PrintAsc(Var F: System.Text);       Override;
  End;

  Data17SubData = Packed Record
    Size : LongWord;
    Data : Packed Array Of LongWord;
  End;

  Data17SubData1 = Packed Record
    X,Y,Z : Single;
  End;

  Data17 = Class(EQWldDataFragment)
  Public
    Flags   : LongWord;
    Size1   : LongInt;
    Size2   : LongInt;
    Params1 : Packed Array[0..0] Of Single;
    Params2 : Packed Array[0..0] Of Single;
    Data1   : Packed Array Of Data17SubData1;
    Data2   : Packed Array Of Data17SubData;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data18 = Class(EQWldDataFragmentWithReference)
  Public
    Flags   : LongWord;
    Params1 : Packed Array[0..0] Of Single;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
  End;

  Data1B = Class(EQWldDataFragment)
    Flags    : LongWord;
    Params2  : LongWord;
    Params3a : Single;
    Params3b : LongWord;
    Params4  : Packed Array [0..3] Of Single;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean;   Override;
    Procedure   Encode(Buffer: TBuffer);            Override;
    Procedure   PrintAsc(Var F: System.Text);       Override;
  End;

  Data1C = Class(EQWldDataFragmentWithReference)
    Flags     : LongWord;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean;   Override;
    Procedure   Encode(Buffer: TBuffer);            Override;
    Procedure   PrintAsc(Var F: System.Text);       Override;
  End;

  Data21SubData = Packed Record
    NormalX   : Single;
    NormalY   : Single;
    NormalZ   : Single;
    Distance  : Single;
    Child     : Packed Array[0..1] Of LongWord;
    NodeIndex : LongWord;
  End;

  Data21 = Class(EQWldDataFragment)
  Public
    Size1 : LongInt;
    Data1 : Packed Array Of Data21SubData;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data22SubData3 = Packed Record
    Flags   : LongWord;
    Size1   : LongInt;
    Data1   : Packed Array Of LongWord;
    Params1 : Packed Array[0..2] Of LongWord;
    Params2 : Packed Array[0..0] Of LongWord;
  End;

  Data22SubData4Union = Packed Record
   Case Integer Of
     0: (Params2_89 : Packed Array[0..1] Of LongWord);
     1: (Params2_AB : Packed Array[0..1] Of LongWord);
     2: (Params2_C  : Packed Array[0..1] Of LongWord);
     3: (Params2_D  : Packed Array[0..0] Of LongWord);
     4: (Params2_12 : Packed Array[0..0] Of LongWord);
     5: (Params2_EF : Packed Array[0..0] Of LongWord);
   End;

  Data22SubData4 = Packed Record
    Flags   : LongWord;
    Params1 : Packed Array[0..0] Of LongWord;
    _Type   : LongWord;
    Union   : Data22SubData4Union;
    Size1   : LongInt;
    Data1   : TByteArray;
  End;

  Data22SubData5 = Packed Record
    Params1 : Packed Array[0..2] Of LongWord;
    Params2 : Packed Array[0..0] Of LongWord;
    Params3 : Packed Array[0..0] Of LongWord;
    Params4 : Packed Array[0..0] Of LongWord;
    Params5 : Packed Array[0..0] Of LongWord;
  End;

  Data22SubData6Union = Packed Record      // Changed to a record
    Data1_B : Packed Array Of Byte;
    Data1_W : Packed Array Of Word;
  End;

  Data22SubData6 = Packed Record
    Size1 : Word;
    Union : Data22SubData6Union;
  End;

  Data22 = Class(EQWldDataFragment)
  Public
    Flags     : LongWord;
    Fragment1 : FragmentReference;
    Fragment2 : FragmentReference;
    Fragment3 : FragmentReference;
    Params1   : Packed Array[0..0] Of LongWord;
    Params2   : Packed Array[0..0] Of LongWord;
    Params3   : Packed Array[0..3] Of Single;
    Params5   : Packed Array[0..0] Of LongWord;
    Params6   : Packed Array[0..0] Of LongWord;
    Size1     : LongInt;
    Size2     : LongInt;
    Size3     : LongInt;
    Size4     : LongInt;
    Size5     : LongInt;
    Size6     : LongInt;
    Size7     : LongInt;
    Data1     : Packed Array Of Byte;
    Data2     : Packed Array Of Byte;
    Data3     : Packed Array Of Data22SubData3;
    Data4     : Packed Array Of Data22SubData4;
    Data5     : Packed Array Of Data22SubData5;
    Data6     : Packed Array Of Data22SubData6;
    Data7     : TByteArray;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data26 = Class(EQWldDataFragment)
  Public
    Fragment : FragmentReference;
    Params1  : Packed Array[0..0] Of LongWord;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data27 = Class(EQWldDataFragment)
  Public
    Fragment : FragmentReference;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data28 = Class(EQWldDataFragmentWithReference)
    Flags      : LongWord;
    X          : Single;
    Y          : Single;
    Z          : Single;
    Radius     : Single;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean;   Override;
    Procedure   Encode(Buffer: TBuffer);            Override;
    Procedure   PrintAsc(Var F: System.Text);       Override;
  End;

  Data29 = Class(EQWldDataFragment)
  Public
    Flags    : LongWord;
    Size1    : LongInt;
    Data1    : Packed Array Of LongWord;
    Size2    : LongInt;
    Data2    : TByteArray;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data2A = Class(EQWldDataFragmentWithReference)
  Public
    Flags : LongWord;
    Size1 : LongInt;
    Data1 : Packed Array of LongWord;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;    
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;                            Override;
  End;

  Data2CSubData1 = Packed Record
    X,Y,Z: Single;
  End;

  Data2CSubData2 = Packed Record
    TX,TZ : Single;
  End;

  Data2CSubData3 = Packed Record
    X,Y,Z : Single;
  End;

  Data2CSubData5 = Packed Record
    Flags   : Word;
    Unknown : Packed Array[0..3] Of Word;
    Vertex  : Packed Array[0..2] Of Word;
  End;

  Data2CSubData7 = Packed Record
    NumVertices : Word;
    Data        : Word;
  End;

  Data2CSubData9 = Packed Record
    NumPolys : Word;
    Data     : Word;
  End;

  Data2CSubData10 = Packed Record
    NumVertices : Word;
    Data        : Word;
  End;

  Data2C = Class(EQWldDataFragment)
  Public
    Flags     : LongWord;
    Size1     : LongInt;
    Size2     : LongInt;
    Size3     : LongInt;
    Size4     : LongInt;
    Size5     : LongInt;
    Size6     : LongInt;
    Size7     : LongInt;
    Size8     : LongInt;
    Size9     : LongInt;
    Size10    : LongInt;
    Params1   : Packed Array [0..2] Of Single;
    Params2   : Single;
    Params3   : Packed Array [0..2] Of LongWord;
    Data1     : Packed Array Of Data2CSubData1;   // Vertex list
    Data2     : Packed Array Of Data2CSubData2;   // Texture coordinates
    Data3     : Packed Array Of Data2CSubData3;   // Normal vectors
    Data4     : Packed Array Of Byte;
    Data5     : Packed Array Of Data2CSubData5;   // Triangles
    Data6     : Packed Array Of Byte;
    Data7     : Packed Array Of Data2CSubData7;   // Pieces
    Data8     : Packed Array Of Byte;
    Data9     : Packed Array Of Data2CSubData9;   // Polygon-Texture list
    Data10    : Packed Array Of Data2CSubData10;  // Vertex-texture list
    Fragment1 : FragmentReference;
    Fragment2 : FragmentReference;
    Fragment3 : FragmentReference;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data2D = Class(EQWldDataFragmentWithReference)
  Public
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
  End;

  Data2F = Class(EQWldDataFragmentWithReference)
  Public
    Flags : LongWord;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
  End;

  Data30 = Class(EQWldDataFragmentWithReference)
  Public
    Flags   : LongWord;
    Params1 : Packed Array[0..0] Of LongWord;
    Params2 : Packed Array[0..0] Of LongWord;
    Params3 : Packed Array[0..1] Of Single;
    Pair    : DataPair;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
  End;

  Data31 = Class(EQWldDataFragment)
  Public
    Size1 : LongInt;
    Data1 : Packed Array Of FragmentReference;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data32SubData5 = Packed Record
    Case Integer Of
      0: (Color: LongWord);
      1: (B,G,R,A: Byte);
    End;

  Data32 = Class(EQWldDataFragment)
  Public
    Data1 : LongWord;
    Count : LongWord;
    Data2 : LongWord;
    Data3 : LongWord;
    Data4 : LongWord;
    Data5 : Packed Array Of Data32SubData5;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data33 = Class(EQWldDataFragmentWithReference)
  Public
    Flags : LongWord;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
  End;

  Data34 = Class(EQWldDataFragment)
  Public
    Flags    : LongWord;
    Params1  : Packed Array[0..$12] Of LongWord;
    Params2  : Packed Array[0..1,0..2] Of LongWord;
    Params3  : Packed Array[0..1,0..2] Of LongWord;
    Fragment : FragmentReference;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data36SubData1 = Packed Record
    X,Y,Z: SmallInt;
  End;

  OldData36SubData2 = Packed Record
    I1,I2 : SmallInt;
  End;

  Data36SubData2 = Packed Record
    I1,I2 : LongInt;//SmallInt;
  End;

  Data36SubData3 = Packed Record
    X,Y,Z : ShortInt;
  End;

  Data36SubData4 = Packed Record
    Case Integer Of
      0: (Color: LongWord);
      1: (B,G,R,A: Byte);
    End;

  Data36SubData5 = Packed Record
    Flags  : Word;
    Vertex : Packed Array[0..2] Of Word;
  End;

  Data36SubData6 = Packed Record
    NumVertices : Word;
    Data        : Word;
  End;

  Data36SubData7 = Packed Record
    NumPolys : Word;
    Data     : Word;
  End;

  Data36SubData8 = Packed Record
    NumVertices : Word;
    Data        : Word;
  End;

  Data36SubData9 = Packed Record
    Parm: Packed Record
      Case Integer Of
       0: (VertexIndex1 : Word;      // If Data3 in [0..3]
           VertexIndex2 : Word);     // Only valid if Data3 = 1
       1: (Float        : Single);   // If Data3 = 4
      End; // Case
    Data2       : Byte;
    Data3       : Byte;
   End;

  Data36AxisAlignedBox = Packed Record
    MinX,MinY,MinZ,MaxX,MaxY,MaxZ: Single;
  End;

  Data36 = Class(EQWldDataFragment)
  Public
    Flags     : LongWord;
    Fragment1 : FragmentReference;
    Fragment2 : FragmentReference; // Points to Data2F if mesh is animated (Data2F points to Data37)
    Fragment3 : FragmentReference;
    Fragment4 : FragmentReference;
    Params1   : Packed Array[0..2] Of Single;
    Params2   : Packed Array[0..2] Of LongWord;
    Radius    : Single;
    Box       : Data36AxisAlignedBox;
    Size1     : Word;
    Size2     : Word;
    Size3     : Word;
    Size4     : Word;
    Size5     : Word;
    Size6     : Word;
    Size7     : Word;
    Size8     : Word;
    Size9     : Word;
    Size10    : Word;
    Data1     : Packed Array Of Data36SubData1;   // Vertex list
    Data2     : Packed Array Of Data36SubData2;   // Texture coordinates
    Data3     : Packed Array Of Data36SubData3;   // Normal vectors
    Data4     : Packed Array Of Data36SubData4;   // Colors
    Data5     : Packed Array Of Data36SubData5;   // Triangles
    Data6     : Packed Array Of Data36SubData6;   // Pieces
    Data7     : Packed Array Of Data36SubData7;
    Data8     : Packed Array Of Data36SubData8;
    Data9     : Packed Array Of Data36SubData9;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  Data37SubData1 = Packed Record
    X,Y,Z: SmallInt;
  End;

  Data37SubData2 = Packed Record
    Data: Packed Array Of Data37SubData1;
  End;

  Data37 = Class(EQWldDataFragment)
  Public
    Flags     : LongWord;
    Size1     : Word;
    Size2     : Word;
    Size3     : Word;
    Size4     : Word;
    Size5     : Word;
    Data1     : Packed Array Of Data37SubData2;
    Size6     : Word;
    Constructor Create(Data: TEQWldData; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

  DataWithName = Class(EQWldDataFragment)
  Public
    Size  : LongWord;
    Data1 : TByteArray;
    Constructor Create(Data: TEQWldData; ID: LongWord; Name: String = '');
    Destructor  Destroy;                          Override;
    Function    Decode(Buffer: TBuffer): Boolean; Override;
    Procedure   Encode(Buffer: TBuffer);          Override;
    Procedure   PrintAsc(Var F: System.Text);     Override;
    Procedure   Clear;
  End;

Implementation

Uses SysUtils,Forms,frmMainUnit;

// ------------------------------
// Static routines
// ------------------------------

Procedure DecodeBytes(Var Data: TByteArray);
Const Codes: Packed Array[0..7] Of Byte = ($95, $3A, $C5, $2A, $95, $7A, $95, $6A);
Var I: LongInt;
Begin
  For I := Low(Data) To High(Data) Do Data[I] := Data[I] Xor Codes[I And 7];
End; // DecodeBytes

Procedure PrintHex(Var F: System.Text; Data: Pointer; Size,Base,eSize: LongWord;
                   Offset: Integer; Prefix: String = '');
Var
  Position : LongWord;
  AchTemp  : Packed Array[0..63] Of Char;
  I,J      : LongWord;

Begin
  If Data <> Nil Then
  Begin
    If eSize < 1 Then eSize := 1;
    Position := Offset;
    If Position = $FFFFFFFF Then Position := 0;
    J := 0;
    AchTemp[0] := #0;
    For I := 0 To Size - 1 Do
    Begin
      If I Mod (Base Div eSize) <> 0 Then Write(F,' ')
      Else
      Begin
        If Offset >= 0 Then
        Begin
          If Offset <> 0
           Then Write(F,Prefix + IntToHex(Position Div eSize,4) + ' ')
           Else Write(F,Prefix + IntToHex(Position Div Base,4) + ' ');
        End
        Else Write(F,Prefix + ' ');
      End;
      Inc(Position,eSize);
      Case eSize Of
        2: Write(F,IntToHex(WPtr(LongWord(Data) + (I * 2))^,4));
        4: Write(F,IntToHex(LPtr(LongWord(Data) + (I * 4))^,8));
      Else
        If BPtr(LongWord(Data) + I)^ < 33
         Then AchTemp[I - J] := '.'
         Else AchTemp[I - J] := Char(BPtr(LongWord(Data) + I)^);
        Write(F,IntToHex(BPtr(LongWord(Data) + I)^,2));
      End; // Case

      If (I Mod (Base Div eSize)) = (Base Div eSize) - 1 Then
      Begin
        If eSize = 1 Then
        Begin
          AchTemp[I - J + 1] := #0;
          WriteLn(F,' ' + StrPas(AchTemp));
          AchTemp[0] := #0;
          J := I + 1;
        End
        Else WriteLn(F);
      End;
    End; // For I

    I := Size;
    If (I Mod (Base Div eSize)) <> 0 Then
    Begin
      If eSize = 1 Then
      Begin
        AchTemp[I - J + 1] := #0;
        WriteLn(F,' ' + StrPas(AchTemp));
        AchTemp[0] := #0;
      End
      Else WriteLn(F);
    End;
  End;
End; // PrintHex

// ------------------------------
// TBuffer
// ------------------------------

Constructor TBuffer.Create;
Begin
  Init(Nil,0);
End; // TBuffer.Create

Constructor TBuffer.Create(B: Pointer; L: LongWord = 0);
Begin
  Init(B,L);
End; // TBuffer.Create

Procedure TBuffer.Init(B: Pointer; L: LongWord = 0);
Begin
  Buffer       := B;
  Position     := 0;
  Len          := L;
  BufferLength := L;
  Allocated    := (B <> Nil);
End; // TBuffer.Init

Procedure TBuffer.Clear;
Begin
//  If Allocated Then FreeMem(Buffer,BufferLength);
//  Buffer       := Nil;
  Position     := 0;
  Len          := 0;
//  BufferLength := 0;
//  Allocated    := False;
End; // TBuffer.Clear

Destructor TBuffer.Destroy;
Begin
  If Allocated Then FreeMem(Buffer,BufferLength);
End;

Function TBuffer.Allocate(Size: LongWord): Boolean;
Var P: Pointer;
Begin
  If Size = 0 Then
  Begin
    Result := False;
    Exit;
  End;
  While Position + Size > BufferLength Do
  Begin
    Inc(BufferLength,1024);
    GetMem(P,BufferLength);
    If Allocated Then
    Begin
      Move(Buffer^,P^,BufferLength - 1024);
      FreeMem(Buffer,BufferLength - 1024);
    End;
    Allocated := True;
    Buffer := P;
  End; // While
  Result := True;
End; // TBuffer.Allocate

Function TBuffer.GetInt: Integer;
Begin
  Result := IPtr(LongWord(Buffer) + Position)^;
  Inc(Position,4);
End; // TBuffer.GetInt

Procedure TBuffer.PutDWORD(D: LongWord);
Begin
  If Allocate(4) Then
  Begin
    LPtr(LongWord(Buffer) + Position)^ := D;
    Inc(Position,4);
    If Len < Position Then Len := Position;
  End;
End; // TBuffer.PutDWORD

Procedure TBuffer.PutWORD(D: Word);
Begin
  If Allocate(2) Then
  Begin
    WPtr(LongWord(Buffer) + Position)^ := D;
    Inc(Position,2);
    If Len < Position Then Len  := Position;
  End;
End; // TBuffer.PutDWORD

Procedure TBuffer.PutFLOAT(F: Single);
Begin
  If Allocate(4) Then
  Begin
    FPtr(LongWord(Buffer) + Position)^ := F;
    Inc(Position,4);
    If Len < Position Then Len  := Position;
  End;
End; // TBuffer.PutFLOAT

Procedure TBuffer.PutBYTE(Data: Pointer; Size: LongWord);
Begin
  If (Size > 0) And Allocate(Size) Then
  Begin
    Move(Data^,BPtr(LongWord(Buffer) + Position)^,Size);
    Inc(Position,Size);
    If Len < Position Then Len := Position;
  End;
End; // TBuffer.PutBYTE

Function TBuffer.GetDWORD: LongWord;
Begin
  Result := LPtr(LongWord(Buffer) + Position)^;
  Inc(Position,4);
End; // TBuffer.GetDWORD

Function TBuffer.GetWORD: Word;
Begin
  Result := WPtr(LongWord(Buffer) + Position)^;
  Inc(Position,2);
End; // TBuffer.GetWORD

Function TBuffer.GetBYTE(_Len: LongWord = 1): BPtr;
Begin
  Result := BPtr(LongWord(Buffer) + Position);
  Inc(Position,_Len);
End; // TBuffer.GetDWORD

Function TBuffer.GetFLOAT: Single;
Begin
  Result := FPtr(LongWord(Buffer) + Position)^;
  Inc(Position,4);
End; // TBuffer.GetDWORD

Function TBuffer.GetDOUBLE: Double;
Begin
  Result := DPtr(LongWord(Buffer) + Position)^;
  Inc(Position,8);
End; // TBuffer.GetDWORD

Function TBuffer.AtEnd: Boolean;
Begin
  Result := (Position = Len);
End; // TBuffer.AtEnd

// ------------------------------
// TEQWldData
// ------------------------------

Constructor TEQWldData.Create;
Begin
  FragmentNames        := TStringList.Create;
  FragmentNames.Sorted := True;
End; // TEQWldData.Create

Destructor TEQWldData.Destroy;
Var I: Integer;
Begin
  For I := 0 To High(Fragments) Do Fragments[I].Free;
  SetLength(Fragments,0);
  FragmentNames.Free;
End; // TEQWldData.Destroy

Function TEQWldData.Decode(Stream: TStream): Boolean;
Var
  Header  : Packed Array[0..6] Of LongWord;
  Size    : LongWord;
  _Object : LongWord;
  B       : BPtr;
  Buffer  : TBuffer;
  MaxSize : LongWord;

Begin
  Result := False;

  // Check for stuff that required special handling that isnt implemented yet

  Stream.ReadBuffer(Header,SizeOf(Header));
  If Header[0] <> $54503D02 Then Exit;
  NewType := False;

  // Read the .WLD version identifier

  If ((Header[1] And $FFFFFFFE) <> Version1WLD) And
     ((Header[1] And $FFFFFFFE) <> Version2WLD) Then Exit;

  If (Header[1] And $FFFFFFFE) = Version2WLD Then NewType := True;

  If (Header[1] And 1) <> 0 Then Exit;

  maxFragment  := Header[2];
  SetLength(Fragments,MaxFragment + 1);
  Fragments[0] := Nil;
  nFragment    := 1;
  SetLength(NameHash,Header[5]);

  // Read the name hash

  Stream.ReadBuffer(NameHash[0],Header[5]);

  // Decode the name hash

  DecodeBytes(NameHash);

  // Read the fragments

  MaxSize := 0;
  B       := Nil;
  Buffer  := Nil;
  While nFragment <= MaxFragment Do
  Begin
    // Get the size of the fragment

    Stream.ReadBuffer(Size,4);

    If Size = $FFFFFFFF Then Break;

    // Get the fragment type

    Stream.ReadBuffer(_Object,4);
{
    // Display the fragment type

    If (nFragment And $FF) = 0 Then
    Begin
      frmMain.Label2.Caption := IntToStr(nFragment) + ' of ' +
                                IntToStr(MaxFragment) + ': ' +
                                IntToHex(_Object,2) + 'h';
      Application.ProcessMessages;
    End;
}
    If Size > MaxSize Then
    Begin
      If (B <> Nil) And (MaxSize > 0) Then FreeMem(B,MaxSize);
      MaxSize := Size;
      GetMem(B,Size);
    End;
    If Buffer = Nil
     Then Buffer := TBuffer.Create(B,Size)
     Else Buffer.Init(B,Size);

    // Get the fragment data

    Stream.ReadBuffer(B^,Size);

    // Decode the fragment
    
    Case _Object Of
      $03: Fragments[nFragment] := Data03.Create(Self);
      $04: Fragments[nFragment] := Data04.Create(Self);
      $05: Fragments[nFragment] := Data05.Create(Self);
      $06: Fragments[nFragment] := Data06.Create(Self);
      $07: Fragments[nFragment] := Data07.Create(Self);
      $09: Fragments[nFragment] := Data09.Create(Self);
      $10: Fragments[nFragment] := Data10.Create(Self);
      $11: Fragments[nFragment] := Data11.Create(Self);
      $12: Fragments[nFragment] := Data12.Create(Self);
      $13: Fragments[nFragment] := Data13.Create(Self);
      $14: Fragments[nFragment] := Data14.Create(Self);
      $15: Fragments[nFragment] := Data15.Create(Self);
      $16: Fragments[nFragment] := Data16.Create(Self);
      $17: Fragments[nFragment] := Data17.Create(Self);
      $18: Fragments[nFragment] := Data18.Create(Self);
      $1B: Fragments[nFragment] := Data1B.Create(Self);
      $1C: Fragments[nFragment] := Data1C.Create(Self);
      $21: Fragments[nFragment] := Data21.Create(Self);
      $22: Fragments[nFragment] := Data22.Create(Self);
      $26: Fragments[nFragment] := Data26.Create(Self);
      $27: Fragments[nFragment] := Data27.Create(Self);
      $28: Fragments[nFragment] := Data28.Create(Self);
      $29: Fragments[nFragment] := Data29.Create(Self);
      $2A: Fragments[nFragment] := Data2A.Create(Self);
      $2C: Fragments[nFragment] := Data2C.Create(Self);
      $2D: Fragments[nFragment] := Data2D.Create(Self);
      $2F: Fragments[nFragment] := Data2F.Create(Self);
      $30: Fragments[nFragment] := Data30.Create(Self);
      $31: Fragments[nFragment] := Data31.Create(Self);
      $32: Fragments[nFragment] := Data32.Create(Self);
      $33: Fragments[nFragment] := Data33.Create(Self);
      $34: Fragments[nFragment] := Data34.Create(Self);
      $36: Fragments[nFragment] := Data36.Create(Self);
      $37: Fragments[nFragment] := Data37.Create(Self);
    Else
      Fragments[nFragment] := DataWithName.Create(Self, _Object);
    End; // Case
    Inc(nFragment);
    If Not Fragments[nFragment - 1].Decode(Buffer) Then
    Begin
      Buffer.Allocated := False;
      Buffer.Free;
      FreeMem(B,MaxSize);
      Result := False;
      Exit;
    End;

    If (Fragments[nFragment - 1].Name <> '') And (FragmentNames.IndexOf(Fragments[nFragment - 1].Name) < 0) Then
     FragmentNames.AddObject(Fragments[nFragment - 1].Name,Fragments[nFragment - 1]);
  End; // While
  Buffer.Allocated := False;
  Buffer.Free;
  FreeMem(B,MaxSize);
  Result := True;
End; // TEQWldData.Decode

Function TEQWldData.Decode(P: Pointer): Boolean;
Var
  Header  : Packed Array[0..6] Of LongWord;
  Size    : LongWord;
  _Object : LongWord;
  B       : BPtr;
  Buffer  : TBuffer;
  MaxSize : LongWord;

Begin
  Result := False;

  // Check for stuff that required special handling that isnt implemented yet

  Move(P^,Header,SizeOf(Header));
  Inc(LongWord(P),SizeOf(Header));
  If Header[0] <> $54503D02 Then Exit;
  NewType := False;

  // Read the .WLD version identifier

  If ((Header[1] And $FFFFFFFE) <> Version1WLD) And
     ((Header[1] And $FFFFFFFE) <> Version2WLD) Then Exit;

  If (Header[1] And $FFFFFFFE) = Version2WLD Then NewType := True;

  If (Header[1] And 1) <> 0 Then Exit;

  maxFragment  := Header[2];
  SetLength(Fragments,MaxFragment + 1);
  Fragments[0] := Nil;
  nFragment    := 1;
  SetLength(NameHash,Header[5]);

  // Read the name hash

  If Header[5] > 0 Then Move(P^,NameHash[0],Header[5]); // Should always be true, but I'm being paranoid
  Inc(LongWord(P),Header[5]);

  // Decode the name hash

  DecodeBytes(NameHash);

  // Read the fragments

  MaxSize := 0;
  B       := Nil;
  Buffer  := Nil;
  While nFragment <= MaxFragment Do
  Begin
    // Get the size of the fragment

    Move(P^,Size,4);
    Inc(LongWord(P),4);

    If Size = $FFFFFFFF Then Break;

    // Get the fragment type

    Move(P^,_Object,4);
    Inc(LongWord(P),4);
{
    // Display the fragment type

    If (nFragment And $FF) = 0 Then
    Begin
      frmMain.Label2.Caption := IntToStr(nFragment) + ' of ' +
                                IntToStr(MaxFragment) + ': ' +
                                IntToHex(_Object,2) + 'h';
      Application.ProcessMessages;
    End;
}
    If Size > MaxSize Then
    Begin
      If (B <> Nil) And (MaxSize > 0) Then FreeMem(B,MaxSize);
      MaxSize := Size;
      GetMem(B,Size);
    End;
    If Buffer = Nil
     Then Buffer := TBuffer.Create(B,Size)
     Else Buffer.Init(B,Size);

    // Get the fragment data

    If Size > 0 Then Move(P^,B^,Size); // Should always be true, but I'm being paranoid
    Inc(LongWord(P),Size);

    // Decode the fragment
    
    Case _Object Of
      $03: Fragments[nFragment] := Data03.Create(Self);
      $04: Fragments[nFragment] := Data04.Create(Self);
      $05: Fragments[nFragment] := Data05.Create(Self);
      $06: Fragments[nFragment] := Data06.Create(Self);
      $07: Fragments[nFragment] := Data07.Create(Self);
      $09: Fragments[nFragment] := Data09.Create(Self);
      $10: Fragments[nFragment] := Data10.Create(Self);
      $11: Fragments[nFragment] := Data11.Create(Self);
      $12: Fragments[nFragment] := Data12.Create(Self);
      $13: Fragments[nFragment] := Data13.Create(Self);
      $14: Fragments[nFragment] := Data14.Create(Self);
      $15: Fragments[nFragment] := Data15.Create(Self);
      $16: Fragments[nFragment] := Data16.Create(Self);
      $17: Fragments[nFragment] := Data17.Create(Self);
      $18: Fragments[nFragment] := Data18.Create(Self);
      $1B: Fragments[nFragment] := Data1B.Create(Self);
      $1C: Fragments[nFragment] := Data1C.Create(Self);
      $21: Fragments[nFragment] := Data21.Create(Self);
      $22: Fragments[nFragment] := Data22.Create(Self);
      $26: Fragments[nFragment] := Data26.Create(Self);
      $27: Fragments[nFragment] := Data27.Create(Self);
      $28: Fragments[nFragment] := Data28.Create(Self);
      $29: Fragments[nFragment] := Data29.Create(Self);
      $2A: Fragments[nFragment] := Data2A.Create(Self);
      $2C: Fragments[nFragment] := Data2C.Create(Self);
      $2D: Fragments[nFragment] := Data2D.Create(Self);
      $2F: Fragments[nFragment] := Data2F.Create(Self);
      $30: Fragments[nFragment] := Data30.Create(Self);
      $31: Fragments[nFragment] := Data31.Create(Self);
      $32: Fragments[nFragment] := Data32.Create(Self);
      $33: Fragments[nFragment] := Data33.Create(Self);
      $34: Fragments[nFragment] := Data34.Create(Self);
      $36: Fragments[nFragment] := Data36.Create(Self);
      $37: Fragments[nFragment] := Data37.Create(Self);
    Else
      Fragments[nFragment] := DataWithName.Create(Self, _Object);
    End; // Case
    Inc(nFragment);
    If Not Fragments[nFragment - 1].Decode(Buffer) Then
    Begin
      Buffer.Allocated := False;
      Buffer.Free;
      FreeMem(B,MaxSize);
      Result := False;
      Exit;
    End;

    If (Fragments[nFragment - 1].Name <> '') And (FragmentNames.IndexOf(Fragments[nFragment - 1].Name) < 0) Then
     FragmentNames.AddObject(Fragments[nFragment - 1].Name,Fragments[nFragment - 1]);
    Fragments[nFragment - 1].FIndex := nFragment - 1; 
  End; // While
  If Buffer <> Nil Then Buffer.Allocated := False;
  Buffer.Free;
  FreeMem(B,MaxSize);
  Result := True;
End; // TEQWldData.Decode

Function TEQWldData.GetName(Position: Integer): String;
Begin
  If Position > 0 Then Result := '' Else
  Begin
    Position := -Position;
    If Position <= High(NameHash)
     Then Result := StrPas(@(NameHash[Position]))
     Else Result := '0x' + IntToHex(-Position,8);
  End;
End; // TEQWldData.GetName

Function TEQWldData.GetFragment(Position: Integer): EQWldDataFragment;
Var
  I    : LongInt;
  Name : String;

Begin
  If Position < 0 Then        // Changed to <=
  Begin
    Name := StrPas(@(NameHash[-Position - 1]));  // Added the -1 after it
    I    := FragmentNames.IndexOf(Name);
    If I >= 0 Then
    Begin
      Result := EQWldDataFragment(FragmentNames.Objects[I]);
      Exit;
    End;
  End;
  If LongWord(Position) >= nFragment Then Result := Nil Else Result := Fragments[Position];
End; // TEQWldData.GetFragment

Function TEQWldData.GetFragmentIndex(F: EQWldDataFragment): Integer;
Var I: LongInt;
Begin
  For I := 0 To nFragment - 1 Do
  Begin
    If Fragments[I] = F Then
    Begin
      Result := I;
      Exit;
    End;
  End; // For I
  Result := -1;
End; // TEQWldData.GetFragmentIndex

Function TEQWldData.GetFragmentIndex(F,T: EQWldDataFragment): Integer;
Begin
  Result := GetFragmentIndex(T) - GetFragmentIndex(F);
End; // TEQWldData.GetFragmentIndex

Procedure TEQWldData.FreeAll;
Var I: LongInt;
Begin
  SetLength(NameHash,0);
  For I := 0 To nFragment - 1 Do Fragments[I].Free;
  SetLength(Fragments,0);
End; // TEQWldData.FreeAll

Procedure TEQWldData.PrintAsc(Var F: System.Text);
Var I: LongInt;
Begin
  For I := 0 To nFragment - 1 Do
  Begin
{
    If Fragments[I] <> Nil
     Then frmMain.Label2.Caption := IntToStr(I + 1) + ' of ' + IntToStr(nFragment) + ': ' + IntToHex(Fragments[I].ID,2) + 'h'
     Else frmMain.Label2.Caption := IntToStr(I + 1) + ' of ' + IntToStr(nFragment) + ': Nil';
}     
    Application.ProcessMessages;
    If Fragments[I] <> Nil Then Fragments[I].PrintAsc(F);
  End; // For I
End; // TEQWldData.PrintAsc

Procedure TEQWldData.Encode(Stream: TStream);
Type
  TLenID = Packed Record
    Len : Cardinal;
    ID  : Cardinal;
  End;

Const
  BufSize = 65536; // 64k chunks

Var
  Header  : Packed Array[0..6] Of LongWord;
  B1      : Byte;
  L       : LongInt;
  I,J,K   : LongInt;
  B       : TBuffer;
  Names   : TStringList;
  Posn    : Packed Array Of LongWord;
  NData   : TByteArray;
  nData22 : LongInt;
  nData03 : LongWord;
  Frags   : TStringList;
  RefNum  : LongInt;
  Ref     : FragmentReference;
  St      : String;
  LI      : TLenID;
  S       : Array of TMemoryStream;
  Strm    : TMemoryStream;  

Begin
  // Form the name list

  Names := TStringList.Create;
  
  // We can wind up with an ENORMOUS amount of fragments, so we want the name list to auto-sort
  // so searching for a name will only involve a binary search.  The problem with this is that
  // the index of any given name will move around as more are added to the list, and we need to
  // remember the order in which they are written to the buffer.  The solution will be to store
  // the index as written as a "pointer" along with the string (yes, TStringList's are wonderful).

  Names.Sorted := True;
  SetLength(Posn,nFragment + 1);
  SetLength(NData,1);
  NData[0] := 0;
  Posn[0]  := 0;
  Names.AddObject('',Nil);
  nData22 := 0;
  nData03 := 0;
  NewType := False;
  For I := 0 To nFragment - 1 Do
  Begin
    If Fragments[I] <> Nil Then
    Begin
      If Fragments[I] Is Data22 Then Inc(nData22);
      If Fragments[I] Is Data03 Then Inc(nData03);

      // Add the fragment's name

      If Fragments[I].Name <> '' Then
      Begin
        J := Names.IndexOf(Fragments[I].Name);
        If J < 0 Then
        Begin
          K := High(NData) + 1;
          Posn[I + 1] := K;
          Names.AddObject(Fragments[I].Name,Pointer(K{Names.Count})); // Save the name index (as saved in the buffer) as a pointer
          SetLength(NData,K + Length(Fragments[I].Name) + 1);
          StrPCopy(@NData[K],Fragments[I].Name);
        End
        Else Posn[I + 1] := LongWord(Names.Objects[J]); // Posn[LongWord(Names.Objects[J]) + 1];
      End
      Else Posn[I + 1] := 0;

      // Scan all of the fragment references and add any names that
      // they have

      Frags := Fragments[I].GetFragmentReferences;
      For RefNum := 0 To Frags.Count - 1 Do
      Begin
        If Frags.Objects[RefNum] <> Nil Then
        Begin
          Ref := Frags.Objects[RefNum] As FragmentReference;
          If (Ref.Fragment = Nil) And (Ref.Name <> '') And (Ref.Position < 0) Then
          Begin
            St := Copy(Ref.Name,2,Length(Ref.Name) - 2);
            J  := Names.IndexOf(St);
            If J < 0 Then
            Begin
              K := High(NData) + 1;
              Ref.Position := K;
              Names.AddObject(St,Pointer(K{Names.Count}));  // Save the name index (as saved in the buffer) as a pointer
              SetLength(NData,K + Length(St) + 1);
              StrPCopy(@NData[K],St);
            End
            Else Ref.Position := LongWord(Names.Objects[J]); // Posn[LongWord(Names.Objects[J]) + 1];
          End;
        End;
      End; // For RefNum
      Frags.Free;

      // If this fragment is a Data10, encode the names

      If Fragments[I] Is Data10 Then
      Begin
        For RefNum := 0 To Data10(Fragments[I]).Size1 - 1 Do
        Begin
          St := Data10(Fragments[I]).Data1[RefNum].Name;
          If St <> '' Then
          Begin
            J := Names.IndexOf(St);
            If J < 0 Then
            Begin
              K := High(NData) + 1;
              Data10(Fragments[I]).Data1[RefNum].NamePos := K;
              Names.AddObject(St,Pointer(K{Names.Count}));  // Save the name index (as saved in the buffer) as a pointer
              SetLength(NData,K + Length(St) + 1);
              StrPCopy(@NData[K],St);
            End
            Else Data10(Fragments[I]).Data1[RefNum].NamePos := LongWord(Names.Objects[J]); // Posn[LongWord(Names.Objects[J]) + 1];
          End
          Else Data10(Fragments[I]).Data1[RefNum].NamePos := 0;
        End; // For RefNum
      End;
    End;
  End; // For I
  Names.Free;
  DecodeBytes(NData);

  Header[0] := $54503D02;
  If NewType
   Then Header[1] := Version2WLD
   Else Header[1] := Version1WLD;
  Header[2] := nFragment - 1;//1;
  Header[3] := nData22;//$1DBB;
  Header[4] := $680D4;
  Header[5] := High(NData) + 1;
  Header[6] := Cardinal(LongInt(nFragment) - (LongInt(nData03) + 6));  // Guessing here  //1;
  Stream.WriteBuffer(Header,SizeOf(Header));
  Stream.WriteBuffer(NData[0],High(NData) + 1);
  SetLength(NData,0);

  // Simply writing to a TMemoryStream is horribly inefficient, since it reallocates
  // every 2k.  For large files, this is death near the end of the file, since it
  // involves lots of copying.  Let's allocate 64k streams as we go and write them
  // all at once when we're finished.

  B := TBuffer.Create;
  SetLength(S,0);
  Strm := Nil;
  For I := 0 To nFragment - 1 Do
  Begin
    If (Fragments[I] <> Nil){ And (Fragments[I].ID = $2C)} Then
    Begin
      If Fragments[I].ID <> $35
       Then Fragments[I].NameHashPos := -LongInt(Posn[I + 1])
       Else Fragments[I].NameHashPos := Integer($FF000000);

      B.Clear;
      Fragments[I].Encode(B);
      B1 := 0;
      While (B.Len Mod 4) <> 0 Do B.PutBYTE(@B1,1);
      LI.Len := B.Len;
      LI.ID  := Fragments[I].ID;

      // If we don't have a temporary stream yet (or if writing to the current one
      // will exceed its size) then allocate a new one

      If (Strm = Nil) Or (B.Len + 8 + Strm.Position > Strm.Size) Then
      Begin
        SetLength(S,High(S) + 2);
        S[High(S)] := TMemoryStream.Create;
        S[High(S)].SetSize(BufSize);
        Strm := S[High(S)];
      End;

      // Save to the temporary stream

      Strm.WriteBuffer(LI,8);
      Strm.WriteBuffer(B.Buffer^,B.Len);
    End;
  End; // For I

  // If we saved anything to our temporary streams, determine the total size of the data
  // and concatenate it all to the first stream.  Then write that stream's contents to our
  // output stream and get rid of all the temporary streams.

  If Strm <> Nil Then
  Begin
    J := 0;
    For I := 0 To High(S) Do Inc(J,S[I].Position);
    S[0].SetSize(J + 1); // One extra byte to make sure we don't reallocate after we've written everything
    For I := 1 To High(S) Do S[0].WriteBuffer(S[I].Memory^,S[I].Position);
    Stream.WriteBuffer(S[0].Memory^,S[0].Position);
    For I := 0 To High(S) Do S[I].Free;
  End;
  SetLength(S,0);
  B.Free;
  L := -1;
  Stream.WriteBuffer(L,4);
  SetLength(Posn,0);
End; // TEQWldData.Encode

// ------------------------------
// FragmentReference
// ------------------------------

Constructor FragmentReference.Create;
Begin
  Name     := '';
  Fragment := Nil;
  Position := -1;
End; // FragmentReference.Create

Constructor FragmentReference.Create(F: FragmentReference);
Begin
  If F.Name <> '' Then Name := F.Name Else Name := '';
  Fragment := F.Fragment;
  Position := F.Position;
End; // FragmentReference.Create

Procedure FragmentReference.SetName(NewName: String);
Begin
  Name := NewName;
End; // FragmentReference.SetName

Procedure FragmentReference.SetFragment(F: EQWldDataFragment);
Begin
  Fragment := F;
End; // FragmentReference.SetFragment

Function FragmentReference.Equals(F: FragmentReference): FragmentReference;
Begin
  If F <> Self Then
  Begin
    SetName(F.Name);
    Fragment := F.Fragment;
  End;
  Result := Self;
End; // FragmentReference.Equals

Function FragmentReference.Exclam: Boolean;
Begin
  Result := (Name = '') And (Fragment = Nil);
End; // FragmentReference.Exclam

Function FragmentReference.GetName: String;
Begin
  If (Fragment <> Nil) And (Fragment.Name <> '')
   Then Result := Fragment.Name
   Else Result := Name;
End; // FragmentReference.GetName

// ------------------------------
// EQWldDataFragment
// ------------------------------

Constructor EQWldDataFragment.Create(Data: TEQWldData; _ID: LongWord; _Name: String);
Begin
  Parent := Data;
  FID    := _ID;
  FName  := _Name;
End; // EQWldDataFragment.Create

Destructor EQWldDataFragment.Destroy;
Begin
  Clear;
End; // EQWldDataFragment.Destroy

Function EQWldDataFragment.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  Result := DecodeNameReference(Buffer,FName);
End; // EQWldDataFragment.Decode

Procedure EQWldDataFragment.Encode(Buffer: TBuffer);
Begin
//  Buffer.PutDWORD(0);
  Buffer.PutDWORD(Cardinal(NameHashPos));
End; // EQWldDataFragment.Encode

Procedure EQWldDataFragment.Clear;
Begin
End; // EQWldDataFragment.Clear

Procedure EQWldDataFragment.PrintAsc(Var F: System.Text);
Begin
End; // EQWldDataFragment.PrintAsc

Function EQWldDataFragment.DecodeNameReference(Buffer: TBuffer; Var NewName: String): Boolean;
Var
  Position : LongInt;
  N        : String;

Begin
  N           := ''; // Failsafe
  Position    := Buffer.GetInt;
  NameHashPos := Position;
{
  If (LongWord(Position) = $FF000000) Or (LongWord(Position) = $FF191919) Then
  Begin // $35 has that :(
    N := '0xFF000000';
  End
  Else
  Begin
}
    If Position > 0 Then
    Begin
      NewName := '';
      Result  := False;
      Exit;
    End;
    N := Parent.GetName(Position);
//  End;
//  FName   := N;
  NewName := N;
  Result  := True;
End; // EQWldDataFragment.DecodeNameReference

Procedure EQWldDataFragment.DecodeDataPair(Buffer: TBuffer; Var Result: DataPair);
Begin
  Result.Data1 := Buffer.GetDWORD;
  Result.Data2 := Buffer.GetFLOAT;
End; // EQWldDataFragment.DecodeDataPair

Procedure EQWldDataFragment.EncodeDataPair(Buffer: TBuffer; Result: DataPair);
Begin
  Buffer.PutDWORD(Result.Data1);
  Buffer.PutFLOAT(Result.Data2);
End; // EQWldDataFragment.EncodeDataPair

Function EQWldDataFragment.GetFragmentReferences: TStringList;
Begin
  Result := TStringList.Create;
End; // EQWldDataFragment.GetFragmentReferences

Procedure EQWldDataFragment.PrintAscHeader(Var F: System.Text);
Begin
  Write(F,IntToHex(Parent.GetFragmentIndex(Self),8) + ' DATA_' + IntToHex(ID,2));
  If Name <> '' Then Write(F,': ' + Name);
  WriteLn(F);
End; // EQWldDataFragment.PrintAscHeader

Procedure EQWldDataFragment.PrintAscFooter;
Begin
  WriteLn(F,'END_DATA_' + IntToHex(ID,2));
End; // EQWldDataFragment.PrintAscFooter

Function EQWldDataFragment.DecodeFragmentReference(Buffer: TBuffer): FragmentReference;
Var
  FragmentIndex : LongInt;
  F             : EQWldDataFragment;
  R             : FragmentReference;
  N             : String;

Begin
  FragmentIndex := Buffer.GetINT; // Was GetDWORD
  F             := Parent.GetFragment(FragmentIndex);
  R             := FragmentReference.Create;
  R.Position    := FragmentIndex;
  R.SetFragment(F);
  If F <> Nil Then
  Begin
    N := F.Name;
    If N = '' Then R.SetName('FRG(' + IntToStr(Parent.GetFragmentIndex(Self,F)) + ')');
  End
  Else If FragmentIndex < 0 Then
  Begin
    N := Parent.GetName(FragmentIndex);
    If N <> '' Then R.SetName('(' + N + ')');
  End;
  Result := R;
End; // EQWldDataFragment.DecodeFragmentReference

Function EQWldDataFragment.LoadAsc(B: TBuffer): Boolean;
Begin
  Result := True;
End; // EQWldDataFragment.LoadAsc

// ------------------------------
// EQWldDataFragmentWithReference
// ------------------------------

Constructor EQWldDataFragmentWithReference.Create(Data: TEQWldData; D: LongWord; Name: String = '');
Begin
  Inherited Create(Data,D,Name);
End; // EQWldDataFragmentWithReference.Create

Destructor EQWldDataFragmentWithReference.Destroy;
Begin
  Clear;
End; // EQWldDataFragmentWithReference.Destroy

Procedure EQWldDataFragmentWithReference.Clear;
Begin
  Fragment.Free;
  Fragment := Nil;
End; // EQWldDataFragmentWithReference.Clear

Function EQWldDataFragmentWithReference.BaseDecode(Buffer: TBuffer): Boolean;
Begin
  Result := Inherited Decode(Buffer);
End; // EQWldDataFragmentWithReference.BaseDecode

Function EQWldDataFragmentWithReference.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  Fragment := DecodeFragmentReference(Buffer);
  Result   := (Fragment <> Nil);
End; // EQWldDataFragmentWithReference.Decode

Procedure EQWldDataFragmentWithReference.Encode(Buffer: TBuffer);
Var I: Integer;
Begin
  I := -Parent.GetFragmentIndex(Fragment.Fragment);
  If I = 0 Then I := Fragment.Position;
  Buffer.PutDWORD(LongWord(-I));
//  Buffer.PutDWORD(Parent.GetFragmentIndex(Fragment.Fragment));
End; // EQWldDataFragmentWithReference.Encode

Procedure EQWldDataFragmentWithReference.BaseEncode(Buffer: TBuffer);
Begin
  Inherited Encode(Buffer);
End; // EQWldDataFragmentWithReference.BaseEncode

Function EQWldDataFragmentWithReference.GetFragmentReferences: TStringList;
Var L: TStringList;
Begin
  L := Inherited GetFragmentReferences;
  L.AddObject('',Fragment);
  Result := L;
End; // EQWldDataFragmentWithReference.GetFragmentReferences

Constructor SubData01.Create;
Begin
//  Data1 := 0;
  Size1 := 0;
End; // SubData01.Create

Destructor SubData01.Destroy;
Begin
  Clear;
End; // SubData01.Destroy

Procedure SubData01.Clear;
Begin
  SetLength(Data1,0);
  Size1 := 0;
End; // SubData01.Clear;

Function SubData01.Decode(Buffer: TBuffer; Parent: EQWldDataFragment): Boolean;
Var I,J: LongInt;
Begin
  Clear;
  Params1 := Buffer.GetDWORD;
  Flags   := Buffer.GetDWORD;

  If (Flags And 1) <> 0 Then Params2  := Buffer.GetDWORD;
  If (Flags And 2) <> 0 Then Params3  := Buffer.GetFLOAT;
  If (Flags And 4) <> 0 Then Params4  := Buffer.GetFLOAT;
  If (Flags And 8) <> 0 Then Fragment := Parent.DecodeFragmentReference(Buffer);

  If (Flags And $10) <> 0 Then
  Begin
    For I := 0 To 2 Do
    Begin
      For J := 0 To 2 Do Params5[I][J] := Buffer.GetDWORD;
    End; // For I
  End;

  If (Flags And $20) <> 0 Then
  Begin
    Size1 := Buffer.GetDWORD;
    SetLength(Data1,Size1 * 8);
    If Size1 > 0 Then Move(Buffer.GetBYTE(Size1 * 8)^,Data1[0],Size1 * 8);
  End;

  Result := Buffer.AtEnd;
End; // SubData01.Decode

Procedure SubData01.PrintAsc(Var F: System.Text; Prefix: String = '');
Var I: LongInt;
Begin
  WriteLn(F,Prefix + 'Flags: $' + IntToHex(Flags,8));
  WriteLn(F,Prefix + 'Params1: $' + IntToHex(Params1,8));

  If (Flags And 1) <> 0 Then WriteLn(F,Prefix + 'Params2: $' + IntToHex(Params2,8));
  If (Flags And 2) <> 0 Then WriteLn(F,Prefix + 'Params3: ' + FloatToStr(Params3));
  If (Flags And 4) <> 0 Then WriteLn(F,Prefix + 'Params4: ' + FloatToStr(Params4));
  If (Flags And 8) <> 0 Then WriteLn(F,Prefix + 'Fragment: ' + Fragment.GetName);

  If (Flags And $10) <> 0 Then
  Begin
    For I := 0 To 2 Do
     WriteLn(F,Prefix + 'Params5_' + IntToStr(I) + ': $' +
               IntToHex(Params5[I,0],8) + ' $' +
               IntToHex(Params5[I,1],8) + ' $' +
               IntToHex(Params5[I,2],8));
  End;

  If (Flags And $20) <> 0 Then
  Begin
    WriteLn(F,Prefix + 'Size1: $' + IntToHex(Size1,8));
    PrintHex(F, @Data1[0], Size1, 8, 1, 0, Prefix);
  End;
End; // SubData01.PrintAsc

Constructor Data03.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,3,Name);
  Size1 := 0;
  SetLength(Data1,0);
  SetLength(Sizes,0);
End; // Data03.Create

Destructor Data03.Destroy;
Begin
  Clear;
End; // Data03.Destroy

Procedure Data03.Clear;
Var I: LongInt;
Begin
  For I := Low(Data1) To High(Data1) Do SetLength(Data1[I],0);
  SetLength(Data1,0);
  SetLength(Sizes,0);
  Size1 := 0;
End; // Data03.Clear

Function Data03.Decode(Buffer: TBuffer): Boolean;
Var
  I    : LongInt;
  Size : Word;

Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Size1 := Buffer.GetDWORD + 1;
    SetLength(Data1,Size1);
    SetLength(Sizes,Size1);
    For I := 0 To Size1 - 1 Do
    Begin
      Size     := Buffer.GetWORD;
      Sizes[I] := Size;
      SetLength(Data1[I],Size);
      If Size > 0 Then Move(Buffer.GetBYTE(Size)^,Data1[I][0],Size);
      DecodeBytes(Data1[I]);
    End; // For I
    If (Buffer.Position And 3) > 0
     Then Buffer.GetBYTE(4 - (Buffer.Position And 3))
     Else Buffer.GetBYTE(0);
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data03.Decode

Procedure Data03.Encode(Buffer: TBuffer);
Var I: LongInt;
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Size1 - 1);
  For I := 0 To Size1 - 1 Do
  Begin
    Buffer.PutWORD(Sizes[I]);
    DecodeBytes(Data1[I]);
    If Sizes[I] > 0 Then Buffer.PutBYTE(@Data1[I][0],Sizes[I]);
    DecodeBytes(Data1[I]);
  End; // For I
End; // Data03.Encode

Procedure Data03.PrintAsc(Var F: System.Text);
Var I: LongInt;
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Size1: $' + IntToHex(Size1,8));
  For I := 0 To Size1 - 1 Do WriteLn(F,'    ' + IntToHex(I,4) + ' NAME: ' + StrPas(@Data1[I][0]));
  PrintAscFooter(F);
End; // Data03.PrintAsc

Constructor Data04.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,4,Name);
  Size1 := 0;
  SetLength(Data1,0);
End; // Data04.Create

Destructor Data04.Destroy;
Begin
  Clear;
End; // Data04.Destroy

Procedure Data04.Clear;
Var I: Integer;
Begin
  For I := 0 To High(Data1) Do Data1[I].Free;
  SetLength(Data1,0);
  Size1 := 0;
End; // Data04.Clear

Function Data04.Decode(Buffer: TBuffer): Boolean;
Var Size,I: LongInt;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags := Buffer.GetDWORD;
    Size  := Buffer.GetDWORD;
    If (Flags And 4) <> 0 Then Params1[0] := Buffer.GetDWORD Else Params1[0] := 0;
    If (Flags And 8) <> 0 Then Params2[0] := Buffer.GetDWORD Else Params2[0] := 0;
    SetLength(Data1,Size);
    For I := 0 To Size - 1 Do
    Begin
      Size1 := I + 1;
      Data1[I] := DecodeFragmentReference(Buffer);
      If Data1[I] = Nil Then
      Begin
        Result := False;
        Exit;
      End;
    End; // For I
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data04.Decode

Procedure Data04.Encode(Buffer: TBuffer);
Var I: LongInt;
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutDWORD(Size1);
  If (Flags And 4) <> 0 Then Buffer.PutDWORD(Params1[0]);
  If (Flags And 8) <> 0 Then Buffer.PutDWORD(Params2[0]);
  For I := 0 To Size1 - 1 Do
   Buffer.PutDWORD(Parent.GetFragmentIndex(Data1[I].Fragment));
End; // Data04.Encode

Procedure Data04.PrintAsc(Var F: System.Text);
Var I: LongInt;
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  If (Flags And $4) <> 0 Then WriteLn(F,'    PARAM1: $' + IntToHex(Params1[0],8));
  If (Flags And $8) <> 0 Then WriteLn(F,'    PARAM2: $' + IntToHex(Params2[0],8));
  WriteLn(F,'    Size1: $' + IntToHex(Size1,4));
  For I := 0 To Size1 - 1 Do WriteLn(F,'    ' + IntToHex(I,4) + ' Fragment1: ' + Data1[I].GetName);
  PrintAscFooter(F);
End; // Data04.PrintAsc

Constructor Data05.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,5,Name);
End; // Data05.Create

Function Data05.Decode(Buffer: TBuffer): Boolean;
Begin
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    Flags  := Buffer.GetDWORD;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data05.Decode

Procedure Data05.Encode(Buffer: TBuffer);
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
End; // Data05.Encode

Procedure Data05.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $'   + IntToHex(Flags,8));
  WriteLn(F,'    Fragment: ' + Fragment.GetName);
  PrintAscFooter(F);
End; // Data05.PrintAsc

Constructor Data06.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,6,Name);
  SetLength(Data1,0);
  Size1 := 0;
  Params7 := SubData01.Create;
  Fragment.Free;
  Fragment := Nil;
End; // Data06.Create

Destructor Data06.Destroy;
Begin
  Clear;
  Params7.Free;
End; // Data06.Destroy

Procedure Data06.Clear;
Var I,J,K: LongInt;
Begin
  Fragment.Free;
  Fragment := Nil;
  For I := 0 To Size1 - 1 Do
  Begin
    For J := 0 To Data1[I].Size1 - 1 Do
    Begin
      For K := 0 To High(Data1[I].Data1[J].Fragments) Do Data1[I].Data1[J].Fragments[K].Free;
      SetLength(Data1[I].Data1[J].Fragments,0);
    End;
    SetLength(Data1[I].Data1,0);
  End;
  SetLength(Data1,0);
  Size1 := 0;
  Params7.Clear;
End; // Data06.Clear

Function Data06.Decode(Buffer: TBuffer): Boolean;
Var I,J,K: LongInt;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags      := Buffer.GetDWORD;
    SubSize1   := Buffer.GetDWORD;
    Size1      := Buffer.GetDWORD;
    Params1[0] := Buffer.GetFLOAT;
    Params1[1] := Buffer.GetFLOAT;
    Fragment   := DecodeFragmentReference(Buffer);

    If (Flags And $80) <> 0 Then Params2 := Buffer.GetFLOAT Else Params2 := 0;

    If (Flags And 1) <> 0 Then
    Begin
      Params3[0] := Buffer.GetDWORD;
      Params3[1] := Buffer.GetDWORD;
      Params3[2] := Buffer.GetDWORD;
    End
    Else
    Begin
      Params3[0] := 0;
      Params3[1] := 0;
      Params3[2] := 0;
    End;

    If (Flags And 2) <> 0 Then Params4 := Buffer.GetFLOAT Else Params4 := 0;
    If (Flags And 4) <> 0 Then Params5 := Buffer.GetDWORD Else Params5 := 0;
    If (Flags And 8) <> 0 Then Params6 := Buffer.GetDWORD Else Params6 := 0;

    SetLength(Data1,Size1);

    For I := 0 To Size1 - 1 Do
    Begin
      Data1[I].Params1 := Buffer.GetDWORD;
      Data1[I].Size1   := Buffer.GetDWORD;
      Data1[I].Flags   := Data1[I].Size1 And $80000000;
      Data1[I].Size1   := Data1[I].Size1 And $7FFFFFFF;
      SetLength(Data1[I].Data1,Data1[I].Size1);

      For J := 0 To Data1[I].Size1 - 1 Do
      Begin
        Data1[I].Data1[J].Params1 := Buffer.GetDWORD;
        SetLength(Data1[I].Data1[J].Fragments,subSize1);
        For K := 0 To SubSize1 - 1 Do
        Begin
          Data1[I].Data1[J].Fragments[K] := DecodeFragmentReference(Buffer);
        End; // For K
      End; // For J
    End; // For I

    Params7.Decode(Buffer, Self);
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data06.Decode

Procedure Data06.PrintAsc(Var F: System.Text);
Var I,J,K: LongInt;
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    SUB_Size1: $' + IntToHex(SubSize1,8));
  WriteLn(F,'    Params1: ' + FloatToStr(Params1[0]) + ' ' + FloatToStr(Params1[1]));
  WriteLn(F,'    Fragment: ' + Fragment.GetName);

  If (Flags And $80) <> 0 Then WriteLn(F,'    Params2: ' + FloatToStr(Params2));

  If (Flags And 1) <> 0 Then WriteLn(F,'    Params3: $' + IntToHex(Params3[0],8) +
                                                   ' $' + IntToHex(Params3[1],8) +
                                                   ' $' + IntToHex(Params3[2],8));
  If (Flags And 2) <> 0 Then WriteLn(F,'    Params4: ' + FloatToStr(Params4));
  If (Flags And 4) <> 0 Then WriteLn(F,'    Params5: $' + IntToHex(Params5,8));
  If (Flags And 8) <> 0 Then WriteLn(F,'    Params6: $' + IntToHex(Params6,8));

  WriteLn(F,'    Size1: $' + IntToHex(Size1,4));

  For I := 0 To Size1 - 1 Do
  Begin
    WriteLn(F,'    ' + IntToHex(I,4) + ' Params1: $' + IntToHex(Data1[I].Params1,8));
    WriteLn(F,'    ' + IntToHex(I,4) + ' Flags: $' + IntToHex(Data1[I].Flags,8));
    WriteLn(F,'    ' + IntToHex(I,4) + ' Size1: $' + IntToHex(Data1[I].Size1,4));

    For J := 0 To Size1 - 1 Do
    Begin
      WriteLn(F,'    ' + IntToHex(I,4) + ' ' + IntToHex(J,4) + ' Params1: $' + IntToHex(Data1[I].Data1[J].Params1,8));

      For K := 0 To SubSize1 - 1 Do
      Begin
        WriteLn(F,'    ' + IntToHex(I,4) +
                     ' ' + IntToHex(J,4) +
                     ' ' + IntToHex(K,4) + ' Fragment: ' + Data1[I].Data1[J].Fragments[k].GetName);
      End; // For K
    End; // For J
  End; // For I

  Params7.PrintAsc(F,'    Params7.');

  PrintAscFooter(F);
End; // Data06.PrintAsc

Constructor Data07.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,7,Name);
End; // Data07.Create

Function Data07.Decode(Buffer: TBuffer): Boolean;
Begin
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    If Buffer.GetDWORD <> 0 Then Result := False Else Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data07.Decode

Procedure Data07.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Fragment1: ' + Fragment.GetName);
  PrintAscFooter(F);
End; // Data07.PrintAsc

Constructor Data09.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$09,Name);
End; // Data09.Create

Function Data09.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    Flags  := Buffer.GetDWORD;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data09.Decode

Procedure Data09.Encode(Buffer: TBuffer);
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
End; // Data09.Encode

Procedure Data09.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Fragment: ' + Fragment.GetName);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  PrintAscFooter(F);
End; // Data09.PrintAsc

Constructor Data10.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$10,Name);
  SetLength(Data1,0);
  SetLength(Data2,0);
  SetLength(Data3,0);
  Size1    := 0;
  Size2    := 0;
  Fragment := Nil;
End; // Data10.Create

Destructor Data10.Destroy;
Begin
  Clear;
End; // Data10.Destroy

Procedure Data10.Clear;
Var I: LongInt;
Begin
  Fragment.Free;
  Fragment := Nil;
  For I := 0 To High(Data1) Do
  Begin
    Data1[I].Fragment1.Free;
    Data1[I].Fragment2.Free;
  End; // For I
  For I := 0 To Size1 - 1 Do SetLength(Data1[I].Data,0);
  For I := 0 To High(Data2) Do Data2[I].Free;
  SetLength(Data1,0);
  SetLength(Data2,0);
  SetLength(Data3,0);
  Size1 := 0;
  Size2 := 0;
End; // Data10.Clear

Function Data10.Decode(Buffer: TBuffer): Boolean;
Var I: LongInt;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags := Buffer.GetDWORD;
    Size1 := Buffer.GetDWORD;

    Fragment := DecodeFragmentReference(Buffer);
{
    If (Flags And $20000) <> 0 Then
    Begin
      Fragment := DecodeFragmentReference(Buffer);
    End
    Else
    Begin
      If Buffer.GetDWORD <> 0 Then
      Begin
        Result := False;
        Exit;
      End;
    End;
}
    If (Flags And $1) <> 0 Then Move(Buffer.GetByte(12)^,Params1[0],12);
    If (Flags And $2) <> 0 Then Params2 := Buffer.GetFLOAT Else Params2 := 0;
    SetLength(Data1,Size1);
    For I := 0 To Size1 - 1 Do
    Begin
      DecodeNameReference(Buffer,Data1[I].Name);
      Data1[I].Flags     := Buffer.GetDWORD;
      Data1[I].Fragment1 := DecodeFragmentReference(Buffer);
      Data1[I].Fragment2 := DecodeFragmentReference(Buffer);
      Data1[I].Size      := Buffer.GetDWORD;
      SetLength(Data1[I].Data,Data1[I].Size);
      If Data1[I].Size > 0 Then Move(Buffer.GetBYTE(Data1[I].Size * 4)^,Data1[I].Data[0],Data1[I].Size * 4);
    End;

    If (Flags And $200) <> 0 Then
    Begin
      Size2 := Buffer.GetDWORD;
      SetLength(Data2,Size2);
      SetLength(Data3,Size2);
      For I := 0 To Size2 - 1 Do Data2[I] := DecodeFragmentReference(Buffer);
      For I := 0 To Size2 - 1 Do Data3[I] := Buffer.GetDWORD;
    End;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data10.Decode

Procedure Data10.Encode(Buffer: TBuffer);
Var I: Integer;
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutDWORD(Size1);
  Buffer.PutDWORD(Parent.GetFragmentIndex(Fragment.Fragment));
  If (Flags And $1) <> 0 Then Buffer.PutBYTE(@(Params1[0]),12);
  If (Flags And $2) <> 0 Then Buffer.PutFLOAT(Params2);
  For I := 0 To Size1 - 1 Do
  Begin
    Buffer.PutDWORD(-Data1[I].NamePos);
    Buffer.PutDWORD(Data1[I].Flags);
    Buffer.PutDWORD(Parent.GetFragmentIndex(Data1[I].Fragment1.Fragment));
    Buffer.PutDWORD(Parent.GetFragmentIndex(Data1[I].Fragment2.Fragment));
    Buffer.PutDWORD(Data1[I].Size);
    If Data1[I].Size > 0 Then Buffer.PutBYTE(@(Data1[I].Data[0]),Data1[I].Size * 4);
  End;
  If (Flags And $200) <> 0 Then
  Begin
    Buffer.PutDWORD(Size2);
    For I := 0 To Size2 - 1 Do Buffer.PutDWORD(Parent.GetFragmentIndex(Data2[I].Fragment));
    For I := 0 To Size2 - 1 Do Buffer.PutDWORD(Data3[I]);
  End;
End; // Data10.Encode

Procedure Data10.PrintAsc(Var F: System.Text);
Var
  I      : LongInt;
  Prefix : String;

Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));

//  If (Flags And $20000) <> 0 Then
    WriteLn(F,'    Fragment: ', Fragment.GetName);

  If (Flags And $1) <> 0 Then
    WriteLn(F,'    Params1: $' + IntToHex(Params1[0],8) +
                          ' $' + IntToHex(Params1[1],8) +
                          ' $' + IntToHex(Params1[2],8));

  If (Flags And $2) <> 0 Then
    WriteLn(F,'    Params2: ' + FloatToStr(Params2));

  WriteLn(F,'    Size1: $' + IntToHex(Size1,4));


  For I := 0 To Size1 - 1 Do
  Begin
    Prefix := '    ' + IntToHex(I,4) + ' ';
    WriteLn(F,Prefix + 'NAME: ' + Data1[I].Name);
    WriteLn(F,Prefix + 'Flags: $' + IntToHex(Data1[I].Flags,8));
    WriteLn(F,Prefix + 'Fragment1: ' + Data1[I].Fragment1.GetName);
    WriteLn(F,Prefix + 'Fragment2: ' + Data1[I].Fragment2.GetName);
    WriteLn(F,Prefix + 'SIZE: $' + IntToHex(Data1[I].Size,4));
    PrintHex(F,@Data1[I].Data[0], Data1[I].Size, 4, 4, 0, Prefix);
  End;

  WriteLn(F,'    Size2: $' + IntToHex(Size2,4));
  For I := 0 To Size2 - 1 Do
   WriteLn(F,'    ' + IntToHex(I,4) + ' Fragment1: ' + Data2[I].GetName);

  For I := 0 To Size2 - 1 Do
   WriteLn(F,'    ' + IntToHex(I,4) + ' Params1: $' + IntToHex(Data3[I],8));

  PrintAscFooter(F);
End; // Data10.PrintAsc

Constructor Data11.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$11,Name);
End; // Data11.Create

Function Data11.Decode(Buffer: TBuffer): Boolean;
Begin
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    If Buffer.GetDWORD <> 0 Then Result := False Else Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data11.Decode

Procedure Data11.Encode(Buffer: TBuffer);
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(0);
End; // Data11.Encode

Procedure Data11.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Fragment: ' + Fragment.GetName);
  PrintAscFooter(F);
End; // Data11.PrintAsc

Constructor Data12.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$12,Name);
  Size1 := 0;
End; // Data12.Create

Destructor Data12.Destroy;
Begin
  Clear;
End;

Procedure Data12.Clear; 
Begin
  If Size1 <> 0 Then
  Begin
    If (Flags And 8) <> 0 Then
    Begin
      SetLength(Data4,0);
    End
    Else
    Begin
      SetLength(Data8,0);
    End;
    Size1 := 0;
  End;
End; // Data12.Clear

Function Data12.Decode(Buffer: TBuffer): Boolean; 
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags := Buffer.GetDWORD;
    Size1 := Buffer.GetDWORD;

    If (Flags And 8) <> 0 Then
    Begin
      SetLength(Data4,Size1);
      If Size1 > 0 Then Move(Buffer.GetBYTE(Size1 * 4 * 4)^,Data4[0],Size1 * 4 * 4);
    End
    Else
    Begin
      SetLength(Data8,Size1);
      If Size1 > 0 Then Move(Buffer.GetBYTE(Size1 * 4 * 8)^,Data8[0],Size1 * 4 * 8);
    End;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data12.Decode

Procedure Data12.Encode(Buffer: TBuffer);
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutDWORD(Size1);
  If (Flags And 8) <> 0 Then
  Begin
    If Size1 > 0 Then Buffer.PutBYTE(@(Data4[0]),Size1 * 4 * 4);
  End
  Else
  Begin
    If Size1 > 0 Then Buffer.PutBYTE(@(Data8[0]),Size1 * 4 * 8);
  End;
End; // Data12.Encode

Procedure Data12.PrintAsc(Var F: System.Text);
Var I: Integer;
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    Size1: ' + IntToStr(Size1));
  For I := 0 To Size1 - 1 Do
  Begin
    If (Flags And 8) <> 0 Then
    Begin
      WriteLn(F,Format('    ' + IntToHex(I,4) + ' R(d,x,y,z): %6d; %6d, %6d, %6d -- M(x,y,z,d): %6d, %6d, %6d; %6d',
                       [Data4[I].E0,
                        Data4[I].E1,
                        Data4[I].E2,
                        Data4[I].E3,
                        Data4[I].MoveXNumerator,
                        Data4[I].MoveYNumerator,
                        Data4[I].MoveZNumerator,
                        Data4[I].MoveDenominator]));
    End
    Else
    Begin
      WriteLn(F,Format('    ' + IntToHex(I,4) + ' R(d,x,y,z): %8.2f; %8.2f, %8.2f, %8.2f -- M(x,y,z,d): %8.2f, %8.2f, %8.2f; %8.2f',
                       [Data8[I].E0,
                        Data8[I].E1,
                        Data8[I].E2,
                        Data8[I].E3,
                        Data8[I].MoveXNumerator,
                        Data8[I].MoveYNumerator,
                        Data8[I].MoveZNumerator,
                        Data8[I].MoveDenominator]));
    End;
  End; // For I
{
  If (Flags And 8) <> 0
   Then PrintHex(F,@Data4[0], Size1 * 4, 4 * 4, 4,0)     // Assuming offset=0
   Else PrintHex(F,@Data8[0], Size1 * 8, 4 * 8, 4,0);    // Assuming offset=0
}
  PrintAscFooter(F);
End; // Data12.PrintAsc

Constructor Data13.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$13,Name);
End; // Data13.Create

Function Data13.Decode(Buffer: TBuffer): Boolean;
Begin
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    Flags := Buffer.GetDWORD;
    If (Flags And 1) <> 0 Then Params1[0] := Buffer.GetDWORD Else Params1[0] := 0;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data13.Decode

Procedure Data13.Encode(Buffer: TBuffer);
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  If (Flags And 1) <> 0 Then Buffer.PutDWORD(Params1[0]);
End; // Data13.Encode

Procedure Data13.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    Fragment: ' + Fragment.GetName);
  If (Flags And 1) <> 0 Then WriteLn(F,'    Params1: $' + IntToHex(Params1[0],8));
  PrintAscFooter(F);
End; // Data13.PrintAsc

Constructor Data14.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$14,Name);
  SetLength(Data1,0);
  SetLength(Data2,0);
  SetLength(Data3,0);
  Size1 := 0;
  Size2 := 0;
  Size3 := 0;
  Fragment1 := Nil;
  Fragment2 := Nil;
End; // Data14.Create

Destructor Data14.Destroy;
Begin
  Clear;
End; // Data14.Destroy

Procedure Data14.Clear;
Var I: LongInt;
Begin
  Fragment1.Free;
  Fragment2.Free;
  Fragment1 := Nil;
  Fragment2 := Nil;
  For I := 0 To High(Data1) Do SetLength(Data1[I].Data,0);
  For I := 0 To High(Data2) Do Data2[I].Free;
  SetLength(Data1,0);
  SetLength(Data2,0);
  SetLength(Data3,0);
  Size1 := 0;
  Size2 := 0;
  Size3 := 0;
End; // Data14.Clear

Function Data14.Decode(Buffer: TBuffer): Boolean;
Var I,J: LongInt;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags     := Buffer.GetDWORD;
    Fragment1 := DecodeFragmentReference(Buffer);
    Size1     := Buffer.GetDWORD;
    Size2     := Buffer.GetDWORD;

    If (Flags And $80) <> 0 Then
    Begin
      Fragment2 := DecodeFragmentReference(Buffer);
    End
    Else
    Begin
      If Buffer.GetDWORD <> 0 Then
      Begin
        Result := False;
        Exit;
      End;
    End;

    If (Flags  And $01) <> 0 Then Params1[0] := Buffer.GetDWORD Else Params1[0] := 0;
    If (Flags  And $02) <> 0
     Then For I := 0 To 6 Do Params2[I] := Buffer.GetDWORD
     Else For I := 0 To 6 Do Params2[I] := 0;

    SetLength(Data1,Size1);

    For I := 0 To Size1 - 1 Do
    Begin
      Data1[I].Size := Buffer.GetDWORD;
      SetLength(Data1[I].Data,Data1[I].Size);
      For J := 0 To Data1[I].Size - 1 Do DecodeDataPair(Buffer, Data1[I].Data[J]);
    End; // For I

    SetLength(Data2,Size2);

    For I := 0 To Size2 - 1 Do Data2[I] := DecodeFragmentReference(Buffer);

    Size3 := Buffer.GetDWORD;

    If Size3 <> 0 Then
    Begin
      SetLength(Data3,Size3);
      If Size3 > 0 Then Move(Buffer.GetBYTE(Size3)^,Data3[0],Size3);
      DecodeBytes(Data3);
    End;

    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data14.Decode

Function Data14.GetFragmentReferences: TStringList;
Var L: TStringList;
Begin
  L := Inherited GetFragmentReferences;
  L.AddObject('',Fragment1);
  L.AddObject('',Fragment2);
  Result := L;
End; // Data14.GetFragmentReferences

Procedure Data14.Encode(Buffer: TBuffer);
Var I,J: Integer;
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);

  I := -Parent.GetFragmentIndex(Fragment1.Fragment);
  If I = 0 Then I := Fragment1.Position;
  Buffer.PutDWORD(LongWord(-I));
//  Buffer.PutDWORD(Parent.GetFragmentIndex(Fragment1.Fragment));
  Buffer.PutDWORD(Size1);
  Buffer.PutDWORD(Size2);
  If (Flags And $80) <> 0
   Then Buffer.PutDWORD(Parent.GetFragmentIndex(Fragment2.Fragment))
   Else Buffer.PutDWORD(0);
  If (Flags  And $01) <> 0 Then Buffer.PutDWORD(Params1[0]);
  If (Flags  And $02) <> 0 Then For I := 0 To 6 Do Buffer.PutDWORD(Params2[I]);

  For I := 0 To Size1 - 1 Do
  Begin
    Buffer.PutDWORD(Data1[I].Size);
    For J := 0 To Data1[I].Size - 1 Do EncodeDataPair(Buffer, Data1[I].Data[J]);
  End; // For I

  For I := 0 To Size2 - 1 Do Buffer.PutDWORD(Parent.GetFragmentIndex(Data2[I].Fragment));

  Buffer.PutDWORD(Size3);

  If Size3 <> 0 Then Buffer.PutBYTE(@Data3[0],Size3);
End; // Data14.Encode

Procedure Data14.PrintAsc(Var F: System.Text);
Var I,J: LongInt;
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    Fragment1: ' + Fragment1.GetName);

  If (Flags And $80) <> 0 Then WriteLn(F,'    Fragment2: ', Fragment2.GetName);
  If (Flags And $1)  <> 0 Then WriteLn(F,'    Params1: $' + IntToHex(Params1[0],8));
  If (Flags And $2)  <> 0 Then PrintHex(F,@Params2[0], 7, 7 * 4, 4, -1, '    Params2:');

  WriteLn(F,'    Size1: $' + IntToHex(Size1,4));

  For I := 0 To Size1 - 1 Do
  Begin
    WriteLn(F,'    ' + IntToHex(I,4) + ' SIZE: $' + IntToHex(Data1[I].Size,4));
    For J := 0 To Data1[I].Size - 1 Do
     WriteLn(F,'    ' + IntToHex(I,4) +
                  ' ' + IntToHex(J,4) +
      ' DATA_PAIR: $' + IntToHex(Data1[I].Data[J].Data1,8) +
                  ' ' + FloatToStr(Data1[I].Data[J].Data2));
  End; // For I

  WriteLn(F,'    Size2: $' + IntToHex(Size2,4));

  For I := 0 To Size2 - 1 Do
   WriteLn(F,'    ' + IntToHex(I,4) + ' Fragment1: ' + Data2[I].GetName);

  If StrPas(@Data3[0]) <> '' Then WriteLn(F,'    STRING: ', StrPas(@Data3[0]));

  PrintAscFooter(F);
End; // Data14.PrintAsc

Constructor Data15.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$15,Name);
  Fragment1 := Nil;
  Fragment2 := Nil;
End; // Data15.Create

Destructor Data15.Destroy;
Begin
  Fragment1.Free;
  Fragment2.Free;
  Fragment1 := Nil;
  Fragment2 := Nil;
  Inherited;
End; // Data15.Destroy

Function Data15.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    Flags     := Buffer.GetDWORD;
    Fragment1 := DecodeFragmentReference(Buffer);
    X         := Buffer.GetFLOAT;
    Y         := Buffer.GetFLOAT;
    Z         := Buffer.GetFLOAT;
    RotateZ   := Buffer.GetFLOAT;
    RotateY   := Buffer.GetFLOAT;
    RotateX   := Buffer.GetFLOAT;
    Params1   := Buffer.GetFLOAT;
    ScaleY    := Buffer.GetFLOAT;
    ScaleX    := Buffer.GetFLOAT;
    Fragment2 := DecodeFragmentReference(Buffer);
    If ((Flags And $100) <> 0) Then Params2 := Buffer.GetDWORD;
    Result    := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data15.Decode

Function Data15.GetFragmentReferences: TStringList;
Var L: TStringList;
Begin
  L := Inherited GetFragmentReferences;
  L.AddObject('',Fragment1);
  L.AddObject('',Fragment2);
  Result := L;
End; // Data15.GetFragmentReferences

Procedure Data15.Encode(Buffer: TBuffer);
Var I: Integer;
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);

  I := -Parent.GetFragmentIndex(Fragment1.Fragment);
  If I = 0 Then I := Fragment1.Position;
  Buffer.PutDWORD(LongWord(-I));
  Buffer.PutFLOAT(X);
  Buffer.PutFLOAT(Y);
  Buffer.PutFLOAT(Z);
  Buffer.PutFloat(RotateZ);
  Buffer.PutFloat(RotateY);
  Buffer.PutFloat(RotateX);
  Buffer.PutFloat(Params1);
  Buffer.PutFloat(ScaleY);
  Buffer.PutFloat(ScaleX);
  I := -Parent.GetFragmentIndex(Fragment2.Fragment);
  If I = 0 Then I := Fragment2.Position;
  Buffer.PutDWORD(LongWord(-I));
  If ((Flags And $100) <> 0) Then Buffer.PutDWORD(Params2);
End; // Data15.Encode

Procedure Data15.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Fragment:  ' + Fragment.GetName);
  WriteLn(F,'    Flags:     $' + IntToHex(Flags,8));
  WriteLn(F,'    Fragment1: ' + Fragment1.GetName);
  WriteLn(F,'    X:         ' + FloatToStr(X));
  WriteLn(F,'    Y:         ' + FloatToStr(Y));
  WriteLn(F,'    Z:         ' + FloatToStr(Z));
  WriteLn(F,'    RotateZ:   ' + FloatToStr(RotateZ));
  WriteLn(F,'    RotateY:   ' + FloatToStr(RotateY));
  WriteLn(F,'    RotateX:   ' + FloatToStr(RotateX));
  WriteLn(F,'    Params1:   ' + FloatToStr(Params1));
  WriteLn(F,'    ScaleY:    ' + FloatToStr(ScaleY));
  WriteLn(F,'    ScaleX:    ' + FloatToStr(ScaleX));
  WriteLn(F,'    Fragment2: ' + Fragment2.GetName);
  If ((Flags And $100) <> 0) Then WriteLn(F,'    Params2: ' + IntToStr(Params2));
  PrintAscFooter(F);
End; // Data15.PrintAsc

Constructor Data16.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$16,Name);
  Params1 := 0;
End; // Data16.Create

Function Data16.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Params1 := Buffer.GetFLOAT;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data16.Decode

Procedure Data16.Encode(Buffer: TBuffer);
Begin
  Inherited Encode(Buffer);
  Buffer.PutFLOAT(Params1);
End; // Data16.Encode

Procedure Data16.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Params1: ' + FloatToStr(Params1));
  PrintAscFooter(F);
End; // Data16.PrintAsc

Constructor Data17.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$17,Name);
  SetLength(Data1,0);
  SetLength(Data2,0);
  Size1 := 0;
  Size2 := 0;
End; // Data17.Create

Destructor Data17.Destroy;
Begin
  Clear;
End; // Data17.Destroy

Procedure Data17.Clear;
Var I: LongInt;
Begin
  SetLength(Data1,0);
  For I := 0 To Size2 - 1 Do SetLength(Data2[I].Data,0);
  SetLength(Data2,0);
End; // Data17.Clear

Function Data17.Decode(Buffer: TBuffer): Boolean;
Var I: LongInt;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags      := Buffer.GetDWORD;
    Size1      := Buffer.GetDWORD;
    Size2      := Buffer.GetDWORD;
    Params1[0] := Buffer.GetFLOAT;

    If (Flags And 1) <> 0 Then
    Begin
      Params2[0] := Buffer.GetFLOAT;
    End
    Else
    Begin
      Params2[0] := 1; // $3F800000;
      Buffer.GetFLOAT;
    End;

    SetLength(Data1,Size1);
    If Size1 > 0 Then Move(Buffer.GetBYTE(12 * Size1)^,Data1[0],12 * Size1);
    SetLength(Data2,Size2);

    For I := 0 To Size2 - 1 Do
    Begin
      Data2[I].Size := Buffer.GetDWORD;
      SetLength(Data2[I].Data,Data2[I].Size);
      If Data2[I].Size > 0 Then Move(Buffer.GetBYTE(Data2[I].Size * 4)^,Data2[I].Data[0],Data2[I].Size * 4);
    End; // For I

    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data17.Decode

Procedure Data17.PrintAsc(Var F: System.Text);
Var
  I      : LongInt;
  Prefix : String;

Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    Params1: ' + FloatToStr(Params1[0]));

  If (Flags And 1) <> 0 Then
   WriteLn(F,'    Params2: ' + FloatToStr(Params2[0]));

  WriteLn(F,'    Size1: $' + IntToHex(Size1,8));

  For I := 0 To Size1 - 1 Do
   WriteLn(F,'    ' + IntToHex(I,4) + ' ' + FloatToStr(Data1[I].X) + ' ' + FloatToStr(Data1[I].Y) + ' ' + FloatToStr(Data1[I].Z));

  WriteLn(F,'    Size2: $' + IntToHex(Size2,8));

  For I := 0 To Size2 - 1 Do
  Begin
    Prefix := '    ' + IntToHex(I,4) + ' ';
    WriteLn(F,'    ' + Prefix + ' SIZE: $' + IntToHex(Data2[I].Size,8));
    PrintHex(F,@Data2[I].Data[0], Data2[I].Size, 4, 4, 0, Prefix);
  End; // For I

  PrintAscFooter(F);
End; // Data17.PrintAsc

Constructor Data18.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$18,Name);
End; // Data18.Create

Function Data18.Decode(Buffer: TBuffer): Boolean;
Begin
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    Flags  := Buffer.GetDWORD;
    If (Flags And 1) <> 0 Then Params1[0] := Buffer.GetFLOAT Else Params1[0] := 0;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data18.Decode

Procedure Data18.Encode(Buffer: TBuffer);
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  If (Flags And 1) <> 0 Then Buffer.PutFLOAT(Params1[0]);
End; // Data18.Encode

Procedure Data18.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Fragment: ' + Fragment.GetName);
  WriteLn(F,'    Flags: $'   + IntToHex(Flags,8));
  If (Flags And 1) <> 0 Then WriteLn(F,'    Params1: ' + FloatToStr(Params1[0]));
  PrintAscFooter(F);
End; // Data18.PrintAsc

Constructor Data1B.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$1B,Name);
End; // Data1B.Create

Function Data1B.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags   := Buffer.GetDWORD;
    Params2 := Buffer.GetDWORD;
    If (Flags And $10) <> 0 Then
    Begin
      If (Flags And 8) <> 0 Then Params3b := Buffer.GetDWORD;
      Params4[0] := Buffer.GetFLOAT;
      Params4[1] := Buffer.GetFLOAT;
      Params4[2] := Buffer.GetFLOAT;
      Params4[3] := Buffer.GetFLOAT;
    End
    Else Params3a := Buffer.GetFLOAT;
    Result  := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data1B.Decode

Procedure Data1B.Encode(Buffer: TBuffer);
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutDWORD(Params2);
  If (Flags And $10) <> 0 Then
  Begin
    If (Flags And 8) <> 0 Then Buffer.PutDWORD(Params3b);
    Buffer.PutFLOAT(Params4[0]);
    Buffer.PutFLOAT(Params4[1]);
    Buffer.PutFLOAT(Params4[2]);
    Buffer.PutFLOAT(Params4[3]);
  End
  Else Buffer.PutFLOAT(Params3a);
End; // Data1B.Encode

Procedure Data1B.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags:   $' + IntToHex(Flags,8));
  WriteLn(F,'    Params2: ' + IntToStr(Params2));
  If (Flags And $10) <> 0 Then
  Begin
    If (Flags And 8) <> 0 Then WriteLn(F,'    Params3: '    + IntToStr(Params3b));
    WriteLn(F,'    Params4[0]: ' + FloatToStr(Params4[0]));
    WriteLn(F,'    Params4[1]: ' + FloatToStr(Params4[1]));
    WriteLn(F,'    Params4[2]: ' + FloatToStr(Params4[2]));
    WriteLn(F,'    Params4[3]: ' + FloatToStr(Params4[3]));
  End
  Else WriteLn(F,'    Params3: ' + FloatToStr(Params3a));
  PrintAscFooter(F);
End; // Data1B.PrintAsc

Constructor Data1C.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$1C,Name);
End; // Data1C.Create

Function Data1C.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    Flags  := Buffer.GetDWORD;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data1C.Decode

Procedure Data1C.Encode(Buffer: TBuffer);
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
End; // Data1C.Encode

Procedure Data1C.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Fragment: ' + Fragment.GetName);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  PrintAscFooter(F);
End; // Data1C.PrintAsc

Constructor Data21.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$21,Name);
  SetLength(Data1,0);
  Size1 := 0;
End; // Data21.Create

Destructor Data21.Destroy;
Begin
  Clear;
End; // Data21.Destroy

Procedure Data21.Clear;
Begin
  SetLength(Data1,0);
  Size1 := 0;
End; // Data21.Clear

Function Data21.Decode(Buffer: TBuffer): Boolean;
Var I: LongInt;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Size1 := Buffer.GetDWORD;
    SetLength(Data1,Size1);
    For I := 0 To Size1 - 1 Do
    Begin
      Data1[I].NormalX   := Buffer.GetFLOAT;
      Data1[I].NormalY   := Buffer.GetFLOAT;
      Data1[I].NormalZ   := Buffer.GetFLOAT;
      Data1[I].Distance  := Buffer.GetFLOAT;
      Data1[I].NodeIndex := Buffer.GetDWORD;
      Data1[I].Child[0]  := Buffer.GetDWORD;
      Data1[I].Child[1]  := Buffer.GetDWORD;
    End;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data21.Decode

Procedure Data21.Encode(Buffer: TBuffer);
Var I: LongInt;
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Size1);
  For I := 0 To Size1 - 1 Do
  Begin
    Buffer.PutFLOAT(Data1[I].NormalX);
    Buffer.PutFLOAT(Data1[I].NormalY);
    Buffer.PutFLOAT(Data1[I].NormalZ);
    Buffer.PutFLOAT(Data1[I].Distance);
    Buffer.PutDWORD(Data1[I].NodeIndex);
    Buffer.PutDWORD(Data1[I].Child[0]);
    Buffer.PutDWORD(Data1[I].Child[1]);
  End; // For I
End; // Data21.Encode

Procedure Data21.PrintAsc(Var F: System.Text);
Var I: LongInt;
Begin
  PrintAscHeader(F);
  For I := 0 To Size1 - 1 Do
  Begin
    WriteLn(F,'    ' + IntToHex(I + 1,4) +
         ' INDEX: $' + IntToHex(Data1[I].Child[0],4) +
                ' $' + IntToHex(Data1[I].Child[1],4) +
         ' Params: ' + FloatToStrF(Data1[I].NormalX,ffFixed,9,6) +
                 ' ' + FloatToStrF(Data1[I].NormalY,ffFixed,9,6) +
                 ' ' + FloatToStrF(Data1[I].NormalZ,ffFixed,9,6) +
                 ' ' + FloatToStrF(Data1[I].Distance,ffFixed,9,6) +
                ' $' + IntToHex(Data1[I].NodeIndex,4));
  End; // For I
  PrintAscFooter(F);
End; // Data21.PrintAsc

Constructor Data22.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$22,Name);
  SetLength(Data1,0);
  SetLength(Data2,0);
  SetLength(Data3,0);
  SetLength(Data4,0);
  SetLength(Data5,0);
  SetLength(Data6,0);
  SetLength(Data7,0);
  Size1     := 0;
  Size2     := 0;
  Size3     := 0;
  Size4     := 0;
  Size5     := 0;
  Size6     := 0;
  Size7     := 0;
  Fragment1 := Nil;
  Fragment2 := Nil;
  Fragment3 := Nil;
End; // Data22.Create

Destructor Data22.Destroy;
Begin
  Clear;
End; // Data22.Destroy

Procedure Data22.Clear;
Var I: LongInt;
Begin
  SetLength(Data1,0);
  SetLength(Data2,0);
  For I := 0 To Size3 - 1 Do SetLength(Data3[I].Data1,0);
  SetLength(Data3,0);
  For I := 0 To Size4 - 1 Do SetLength(Data4[I].Data1,0);
  SetLength(Data4,0);
  SetLength(Data5,0);
  For I := 0 To Size6 - 1 Do
  Begin
    If (Flags And $20) <> 0
     Then SetLength(Data6[I].Union.Data1_W,0)
     Else SetLength(Data6[I].Union.Data1_B,0);
  End; // For I
  SetLength(Data6,0);
  SetLength(Data7,0);
  Size1 := 0;
  Size2 := 0;
  Size3 := 0;
  Size4 := 0;
  Size5 := 0;
  Size6 := 0;
  Size7 := 0;
  Fragment1.Free;
  Fragment2.Free;
  Fragment3.Free;
  Fragment1 := Nil;
  Fragment2 := Nil;
  Fragment3 := Nil;
End; // Data22.Clear

Function Data22.Decode(Buffer: TBuffer): Boolean;
Var I: LongInt;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags := Buffer.GetDWORD;

    If Flags = $00000101 Then
    Begin
      // not a known configuration of this chunk
      Result := True;
      Exit;
    End
    Else
    Begin
      Fragment1  := DecodeFragmentReference(Buffer);
      Size1      := Buffer.GetDWORD;
      Size2      := Buffer.GetDWORD;
      Params1[0] := Buffer.GetDWORD;
      Size3      := Buffer.GetDWORD;
      Size4      := Buffer.GetDWORD;
      Params2[0] := Buffer.GetDWORD;
      Size5      := Buffer.GetDWORD;
      Size6      := Buffer.GetDWORD;
      SetLength(Data1,12 * Size1);
      If Size1 > 0 Then Move(Buffer.GetBYTE(12 * Size1)^,Data1[0],12 * Size1);
      SetLength(Data2,Size2 * 8);
      If Size2 > 0 Then Move(Buffer.GetBYTE(Size2 * 8)^,Data2[0],Size2 * 8);
      SetLength(Data3,Size3);

      For I := 0 To Size3 - 1 Do
      Begin
        Data3[I].Flags := Buffer.GetDWORD;
        Data3[I].Size1 := Buffer.GetDWORD;
        SetLength(Data3[I].Data1,Data3[I].Size1);
        If Data3[I].Size1 > 0 Then Move(Buffer.GetBYTE(4 * Data3[I].Size1)^,Data3[I].Data1[0],4 * Data3[I].Size1);

        If (Data3[I].Flags And 2) <> 0 Then
        Begin
          Data3[I].Params1[0] := Buffer.GetDWORD;
          Data3[I].Params1[1] := Buffer.GetDWORD;
          Data3[I].Params1[2] := Buffer.GetDWORD;
          Data3[I].Params2[0] := Buffer.GetDWORD;
        End
        Else
        Begin
          Data3[I].Params1[0] := 0;
          Data3[I].Params1[1] := 0;
          Data3[I].Params1[2] := 0;
          Data3[I].Params2[0] := 0;
        End;
      End; // For I

      SetLength(Data4,Size4);

      For I := 0 To Size4 - 1 Do
      Begin
        Data4[I].Flags      := Buffer.GetDWORD;
        Data4[I].Params1[0] := Buffer.GetDWORD;
        Data4[I]._Type      := Buffer.GetDWORD;

        Case Data4[I]._Type Of
          $8,$9: Data4[I].Union.Params2_89[0] := Buffer.GetDWORD;
          $A,$B: Begin
                   Data4[I].Union.Params2_AB[0] := Buffer.GetDWORD;
                   Data4[I].Union.Params2_AB[1] := Buffer.GetDWORD;
                 End;
             $C: Begin
                   Data4[I].Union.Params2_C[0] := Buffer.GetDWORD;
                   Data4[I].Union.Params2_C[1] := Buffer.GetDWORD;
                 End;
             $D: Data4[I].Union.Params2_D[0]  := Buffer.GetDWORD;
            $12: Data4[I].Union.Params2_12[0] := Buffer.GetDWORD;
   $E,$FFFFFFF1: Data4[I].Union.Params2_EF[0] := Buffer.GetDWORD;
        Else
          Result := False;
          Exit;
        End; // Case

        If (Data4[I].Flags And 4) <> 0 Then
        Begin
          Data4[I].Size1 := Buffer.GetDWORD;
          SetLength(Data4[I].Data1,Data4[I].Size1);
          If Data4[I].Size1 > 0 Then Move(Buffer.GetBYTE(Data4[I].Size1)^,Data4[I].Data1[0],Data4[I].Size1);
          DecodeBytes(Data4[I].Data1);
        End;
      End; // For I

      SetLength(Data5,Size5);

      For I := 0 To Size5 - 1 Do
      Begin
        Data5[I].Params1[0] := Buffer.GetDWORD;
        Data5[I].Params1[1] := Buffer.GetDWORD;
        Data5[I].Params1[2] := Buffer.GetDWORD;
        Data5[I].Params2[0] := Buffer.GetDWORD;
        Data5[I].Params3[0] := Buffer.GetDWORD;
        Data5[I].Params4[0] := Buffer.GetDWORD;
        Data5[I].Params5[0] := Buffer.GetDWORD;
      End; // For I

      SetLength(Data6,Size6);

      For I := 0 To Size6 - 1 Do
      Begin
        Data6[I].Size1 := Buffer.GetWORD;

        If (Flags And $20) <> 0 Then
        Begin
          SetLength(Data6[I].Union.Data1_W,Data6[I].Size1);
          If Data6[I].Size1 > 0 Then Move(Buffer.GetBYTE(2 * Data6[I].Size1)^,Data6[I].Union.Data1_W[0],2 * Data6[I].Size1);
        End
        Else If (Flags And $80) <> 0 Then
        Begin
          SetLength(Data6[I].Union.Data1_B,Data6[I].Size1);
          If Data6[I].Size1 > 0 Then Move(Buffer.GetBYTE(Data6[I].Size1)^,Data6[I].Union.Data1_B[0],Data6[I].Size1);
        End
        Else
        Begin
          Result := False;
          Exit;
        End;
      End; // For I

{      If (Flags And 1) <> 0 Then
      Begin}
        Params3[0] := Buffer.GetFLOAT;
        Params3[1] := Buffer.GetFLOAT;
        Params3[2] := Buffer.GetFLOAT;
        Params3[3] := Buffer.GetFLOAT;
{      End
      Else
      Begin
        Params3[0] := 0;
        Params3[1] := 0;
        Params3[2] := 0;
        Params3[3] := 0;
      End;}
      If (Flags And 2) <> 0 Then Params5[0] := Buffer.GetDWORD Else Params5[0] := 0;
      If (Flags And 4) <> 0 Then Params6[0] := Buffer.GetDWORD Else Params6[0] := 0;

      Size7 := Buffer.GetDWORD;
      SetLength(Data7,Size7);
      If Size7 > 0 Then Move(Buffer.GetBYTE(Size7)^,Data7[0],Size7);
      DecodeBytes(Data7);

      If (Flags And $040) <> 0 Then Fragment2 := DecodeFragmentReference(Buffer);
      If (Flags And $100) <> 0 Then Fragment3 := DecodeFragmentReference(Buffer);

      If (Buffer.Position And 3) <> 0
       Then Buffer.GetBYTE(4 - (Buffer.Position And 3))
       Else Buffer.GetBYTE(0);
      Result := Buffer.AtEnd;
    End;
  End
  Else Result := False;
End; // Data22.Decode

Procedure Data22.Encode(Buffer: TBuffer);
Var
  I : LongInt;
  B : TByteArray;
  
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutDWORD(Parent.GetFragmentIndex(Fragment1.Fragment));
  Buffer.PutDWORD(Size1);
  Buffer.PutDWORD(Size2);
  Buffer.PutDWORD(Params1[0]);
  Buffer.PutDWORD(Size3);
  Buffer.PutDWORD(Size4);
  Buffer.PutDWORD(Params2[0]);
  Buffer.PutDWORD(Size5);
  Buffer.PutDWORD(Size6);
  If Size1 > 0 Then Buffer.PutBYTE(@Data1[0], Size1 * 12);
  If Size2 > 0 Then Buffer.PutBYTE(@Data2[0], Size2 * 8);

  For I := 0 To Size3 - 1 Do
  Begin
    Buffer.PutDWORD(Data3[I].Flags);
    Buffer.PutDWORD(Data3[I].Size1);
    If Data3[I].Size1 > 0 Then Buffer.PutBYTE(@Data3[I].Data1[0], 4 * Data3[I].Size1);

    If (Data3[I].Flags And 2) <> 0 Then
    Begin
      Buffer.PutDWORD(Data3[I].Params1[0]);
      Buffer.PutDWORD(Data3[I].Params1[1]);
      Buffer.PutDWORD(Data3[I].Params1[2]);
      Buffer.PutDWORD(Data3[I].Params2[0]);
    End;
  End; // For I

  For I := 0 To Size4 - 1 Do
  Begin
    Buffer.PutDWORD(Data4[I].Flags);
    Buffer.PutDWORD(Data4[I].Params1[0]);
    Buffer.PutDWORD(Data4[I]._Type);

    Case Data4[I]._Type Of
      $8,$9: Buffer.PutDWORD(Data4[I].Union.Params2_89[0]);
      $A,$B: Begin
               Buffer.PutDWORD(Data4[I].Union.Params2_AB[0]);
               Buffer.PutDWORD(Data4[I].Union.Params2_AB[1]);
             End;
         $C: Begin
               Buffer.PutDWORD(Data4[I].Union.Params2_C[0]);
               Buffer.PutDWORD(Data4[I].Union.Params2_C[1]);
               Buffer.PutDWORD(Data4[I].Union.Params2_D[0]);
             End;
         $D: Buffer.PutDWORD(Data4[I].Union.Params2_D[0]);
        $12: Buffer.PutDWORD(Data4[I].Union.Params2_12[0]);
    $E,$FFFFFFF1: Buffer.PutDWORD(Data4[I].Union.Params2_EF[0]);
    End; // Case

    If (Data4[I].Flags And 4) <> 0 Then
    Begin
      Buffer.PutDWORD(Data4[I].Size1);
      SetLength(B,Data4[I].Size1);
      If Data4[I].Size1 > 0 Then Move(Data4[I].Data1[0],B[0],Data4[I].Size1);
      DecodeBytes(B);
      If Data4[I].Size1 > 0 Then Buffer.PutBYTE(@B[0], Data4[I].Size1);
      SetLength(B,0);
    End;
  End; // For I

  For I := 0 To Size5 - 1 Do
  Begin
    Buffer.PutDWORD(Data5[I].Params1[0]);
    Buffer.PutDWORD(Data5[I].Params1[1]);
    Buffer.PutDWORD(Data5[I].Params1[2]);
    Buffer.PutDWORD(Data5[I].Params2[0]);
    Buffer.PutDWORD(Data5[I].Params3[0]);
    Buffer.PutDWORD(Data5[I].Params4[0]);
    Buffer.PutDWORD(Data5[I].Params5[0]);
  End; // For I

  For I := 0 To Size6 - 1 Do
  Begin
    Buffer.PutWORD(Data6[I].Size1);

    If (Flags And $20) <> 0 Then
    Begin
      If Data6[I].Size1 > 0 Then Buffer.PutBYTE(@Data6[I].Union.Data1_W[0], 2 * Data6[I].Size1);
    End
    Else If (Flags And $80) <> 0 Then
    Begin
      If Data6[I].Size1 > 0 Then Buffer.PutBYTE(@Data6[I].Union.Data1_B[0], Data6[I].Size1);
    End;
  End; // For I

  If (Flags And 1) <> 0 Then
  Begin
    Buffer.PutFLOAT(Params3[0]);
    Buffer.PutFLOAT(Params3[1]);
    Buffer.PutFLOAT(Params3[2]);
    Buffer.PutFLOAT(Params3[3]);
  End;
  If (Flags And 2) <> 0 Then Buffer.PutDWORD(Params5[0]);
  If (Flags And 4) <> 0 Then Buffer.PutDWORD(Params6[0]);

  Buffer.PutDWORD(Size7);
  SetLength(B,Size7);
  If Size7 > 0 Then Move(Data7[0],B[0],Size7);
  DecodeBytes(B);
  If Size7 > 0 Then Buffer.PutBYTE(@B[0], Size7);
  SetLength(B,0);
  If (Flags And $040) <> 0 Then Buffer.PutDWORD(Parent.GetFragmentIndex(Fragment2.Fragment));
  If (Flags And $100) <> 0 Then Buffer.PutDWORD(Parent.GetFragmentIndex(Fragment3.Fragment));
End; // Data22.Encode

Procedure Data22.PrintAsc(Var F: System.Text);
Type LPtr = ^LongWord;
Var
  I,J     : LongInt;
  B,B1,B2 : Byte;
  W       : Word;
  Typ     : Byte;
  Index   : LongInt;
  Count   : LongInt;
  Prefix  : String;

Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  If Fragment1 <> Nil Then WriteLn(F,'    Fragment1: ' + Fragment1.GetName);
  WriteLn(F,'    Params1: $' + IntToHex(Params1[0],8));
  WriteLn(F,'    Params2: $' + IntToHex(Params2[0],8));

  If {((Flags And $040) <> 0) And }(Fragment2 <> Nil) Then WriteLn(F,'    Fragment2: ' + Fragment2.GetName);
  If {((Flags And $100) <> 0) And }(Fragment3 <> Nil) Then WriteLn(F,'    Fragment3: ' + Fragment3.GetName);
{  If (Flags And 1)    <> 0 Then }WriteLn(F,'    Params3: ' + FloatToStr(Params3[0]) +
                                                      ' ' + FloatToStr(Params3[1]) +
                                                      ' ' + FloatToStr(Params3[2]) +
                                                      //' $' + IntToHex(LPtr(@Params3[3])^,8));
                                                      ' ' + FloatToStr(Params3[3]));
{  If (Flags And 2)    <> 0 Then }WriteLn(F,'    Params5: $' + IntToHex(Params5[0],8));
{  If (Flags And 4)    <> 0 Then }WriteLn(F,'    Params6: $' + IntToHex(Params6[0],8));

  WriteLn(F,'    Size1: $' + IntToHex(Size1,4));
  PrintHex(F,@Data1[0], Size1, 12,0,0);
  WriteLn(F,'    Size2: $' + IntToHex(Size2,4));
  PrintHex(F,@Data2[0], Size2, 8,0,0);
  WriteLn(F,'    Params1: $' + IntToHex(Params1[0],8));
  WriteLn(F,'    Size3: $' + IntToHex(Size3,4));

  For I := 0 To Size3 - 1 Do
  Begin
    Prefix := '    ' + IntToHex(I,4) + ' ';
    WriteLn(F,Prefix + 'Flags: $' + IntToHex(Data3[I].Flags,8));
    WriteLn(F,Prefix + 'Size1: $' + IntToHex(Data3[I].Size1,4));
    PrintHex(F,@Data3[I].Data1[0], Data3[I].Size1 * 4, 4, 4, 0, Prefix);

    If (Data3[I].Flags And 2) <> 0 Then
    Begin
      WriteLn(F,Prefix + 'Params1: $' + IntToHex(Data3[I].Params1[0],8) +
                                 ' $' + IntToHex(Data3[I].Params1[1],8) +
                                 ' $' + IntToHex(Data3[I].Params1[2],8));
      WriteLn(F,Prefix + 'Params2: $' + IntToHex(Data3[I].Params2[0],8));
    End;
  End; // For I

  WriteLn(F,'    Size4: $' + IntToHex(Size4,4));

  For I := 0 To Size4 - 1 Do
  Begin
    Prefix := '    ' + IntToHex(I,4) + ' ';
    WriteLn(F,Prefix + 'Flags: $'   + IntToHex(Data4[I].Flags,8));
    WriteLn(F,Prefix + 'Params1: $' + IntToHex(Data4[I].Params1[0],8));
    WriteLn(F,Prefix + 'TYPE: $'    + IntToHex(Data4[I]._Type,8));

    Case Data4[I]._Type Of
       $8,$9: WriteLn(F,Prefix + 'Params2: $' + IntToHex(Data4[I].Union.Params2_89[0],8) + ' $' + IntToHex(Data4[I].Union.Params2_89[1],8));
       $A,$B: WriteLn(F,Prefix + 'Params2: $' + IntToHex(Data4[I].Union.Params2_AB[0],8) + ' $' + IntToHex(Data4[I].Union.Params2_AB[1],8));
          $C: WriteLn(F,Prefix + 'Params2: $' + IntToHex(Data4[I].Union.Params2_C[0],8)  + ' $' + IntToHex(Data4[I].Union.Params2_C[1],8));
          $D: WriteLn(F,Prefix + 'Params2: $' + IntToHex(Data4[I].Union.Params2_D[0],8));
         $12: WriteLn(F,Prefix + 'Params2: $' + IntToHex(Data4[I].Union.Params2_12[0],8));
$E,$FFFFFFF1: WriteLn(F,Prefix + 'Params2: $' + IntToHex(Data4[I].Union.Params2_EF[0],8));
    End; // Case

    If Data4[I].Size1 <> 0 Then WriteLn(F,Prefix + 'STRING: ' + StrPas(@Data4[I].Data1[0]));
  End; // For I

  WriteLn(F,'    Size5: $' + IntToHex(Size5,4));

  For I := 0 To Size5 - 1 Do
  Begin
    Prefix := '    ' + IntToHex(I,4) + ' ';
    WriteLn(F,Prefix + 'Params1: $' + IntToHex(Data5[I].Params1[0],8) +
                               ' $' + IntToHex(Data5[I].Params1[1],8) +
                               ' $' + IntToHex(Data5[I].Params1[2],8));
    WriteLn(F,Prefix + 'Params2: $' + IntToHex(Data5[I].Params2[0],8));
    WriteLn(F,Prefix + 'Params3: $' + IntToHex(Data5[I].Params3[0],8));
    WriteLn(F,Prefix + 'Params4: $' + IntToHex(Data5[I].Params4[0],8));
    WriteLn(F,Prefix + 'Params5: $' + IntToHex(Data5[I].Params5[0],8));
  End; // For I

  WriteLn(F,'    Size6: $' + IntToHex(Size6,4));

  For I := 0 To Size6 - 1 Do
  Begin
    Prefix := '    ' + IntToHex(I,4);
    WriteLn(F,Prefix + 'Size1: $' + IntToHex(Data6[I].Size1,4));

    If (Flags And $20) <> 0
     Then PrintHex(F,@Data6[I].Union.Data1_W[0], Data6[I].Size1 * 2, 16, 2, -1, Prefix)
     Else// PrintHex(F,@Data6[I].Union.Data1_B[0], Data6[I].Size1, 16, 1, -1, Prefix);
     Begin
       Index := 0;
       J     := 0;
       Count := 0;
       While J <= High(Data6[I].Union.Data1_B) Do
       Begin
         B := Data6[I].Union.Data1_B[J];
         If B = $3F Then
         Begin
           W := Data6[I].Union.Data1_B[J + 1] + (Word(Data6[I].Union.Data1_B[J + 2]) Shl 8);
           Inc(Index,W);
           WriteLn(F,Prefix + ' $' + IntToHex(B,2) + ' $' + IntToHex(W,4) + ' Index += ' + IntToStr(W) + ' (new Index=' + IntToStr(Index) + ')');
           Inc(J,3);
         End
         Else If B = $FF Then
         Begin
           W := Data6[I].Union.Data1_B[J + 1] + (Word(Data6[I].Union.Data1_B[J + 2]) Shl 8);
           Inc(Index,W);
           Inc(Count,W);
           WriteLn(F,Prefix + ' $' + IntToHex(B,2) + ' $' + IntToHex(W,4) + ' Count += ' + IntToStr(W) + ' (new Count=' + IntToStr(Count) + ')');
           WriteLn(F,                    '                   Write ' + IntToStr(W) + ' indices (new Index=' + IntToStr(Index) + ')');
           Inc(J,3);
         End
         Else
         Begin
           B1  := B And $3F;
           Typ := B Shr 6;
           Case Typ Of
             0: Begin
                  Inc(Index,B1);
                  WriteLn(F,Prefix + ' $' + IntToHex(B,2) + ' Index += ' + IntToStr(B1) + ' (new Index=' + IntToStr(Index) + ')');
                End;
             1: Begin
                  B2 := B1 Shr 3;
                  B1 := B1 And 7;
                  Inc(Index,B2);
                  WriteLn(F,Prefix + ' $' + IntToHex(B,2) + ' Index += ' + IntToStr(B2) + ' (new Index=' + IntToStr(Index) + ')');
                  Inc(Index,B1);
                  Inc(Count,B1);
                  WriteLn(F,                    '             Count += ' + IntToStr(B1) + ' (new Count=' + IntToStr(Count) + ')');
                  WriteLn(F,                    '             Write ' + IntToStr(B1) + ' indices (new Index=' + IntToStr(Index) + ')');
                End;
             2: Begin
                  B2 := B1 Shr 3;
                  B1 := B1 And 7;
                  Inc(Index,B2);
                  Inc(Count,B2);
                  WriteLn(F,Prefix + ' $' + IntToHex(B,2) + ' Count += ' + IntToStr(B2) + ' (new Count=' + IntToStr(Count) + ')');
                  WriteLn(F,                    '             Write ' + IntToStr(B2) + ' indices (new Index=' + IntToStr(Index) + ')');
                  Inc(Index,B1);
                  WriteLn(F,                    '             Index += ' + IntToStr(B1) + ' (new Index=' + IntToStr(Index) + ')');
                End;
             3: Begin
                  Inc(Index,B1);
                  Inc(Count,B1);
                  WriteLn(F,Prefix + ' $' + IntToHex(B,2) + ' Count += ' + IntToStr(B1) + ' (new Count=' + IntToStr(Count) + ')');
                  WriteLn(F,                    '             Write ' + IntToStr(B1) + ' indices (new Index=' + IntToStr(Index) + ')');
                End;
           End; // Case
           Inc(J);
         End;
       End; // For J
     End;
  End; // For I

  If Size7 <> 0 Then WriteLn(F,'    STRING: ' + StrPas(@Data7[0]));

  PrintAscFooter(F);
End; // Data22.PrintAsc

Constructor Data26.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$26,Name);
  Fragment := Nil;
End; // Data26.Create

Destructor Data26.Destroy;
Begin
  Clear;
End; // Data26.Destroy

Procedure Data26.Clear;
Begin
  Fragment.Free;
  Fragment := Nil;
End; // Data26.Clear

Function Data26.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    If Buffer.GetDWORD <> 0 Then Result := False
    Else
    Begin
      Fragment   := DecodeFragmentReference(Buffer);
      Params1[0] := Buffer.GetDWORD;
      Result     := Buffer.AtEnd;
    End;
  End
  Else Result := False;
End; // Data26.Decode

Procedure Data26.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Fragment1: ' + Fragment.GetName);
  WriteLn(F,'    Params1: $'  + IntToHex(Params1[0],8));
  PrintAscFooter(F);
End; // Data26.PrintAsc

Constructor Data27.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$27,Name);
  Fragment := Nil;
End; // Data27.Create

Destructor Data27.Destroy;
Begin
  Clear;
End; // Data27.Destroy

Procedure Data27.Clear;
Begin
  Fragment.Free;
  Fragment := Nil;
End; // Data27.Clear

Function Data27.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Fragment := DecodeFragmentReference(Buffer);
    If Buffer.GetDWORD <> 0 Then Result := False Else Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data27.Decode

Procedure Data27.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Fragment1: ' + Fragment.GetName);
  PrintAscFooter(F);
End; // Data27.PrintAsc

Constructor Data28.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$28,Name);
End; // Data28.Create

Function Data28.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    Flags  := Buffer.GetDWORD;
    X      := Buffer.GetFLOAT;
    Y      := Buffer.GetFLOAT;
    Z      := Buffer.GetFLOAT;
    Radius := Buffer.GetFLOAT;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data28.Decode

Procedure Data28.Encode(Buffer: TBuffer);
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutFLOAT(X);
  Buffer.PutFLOAT(Y);
  Buffer.PutFLOAT(Z);
  Buffer.PutFLOAT(Radius);
End; // Data28.Encode

Procedure Data28.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Fragment: ' + Fragment.GetName);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    X:      ' + FloatToStr(X));
  WriteLn(F,'    Y:      ' + FloatToStr(Y));
  WriteLn(F,'    Z:      ' + FloatToStr(Z));
  WriteLn(F,'    Radius: ' + FloatToStr(Radius));
  PrintAscFooter(F);
End; // Data28.PrintAsc

Constructor Data29.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$29,Name);
  SetLength(Data1,0);
  Size1 := 0;
End; // Data29.Create

Destructor Data29.Destroy;
Begin
  Clear;
End; // Data29.Destroy

Procedure Data29.Clear;
Begin
  SetLength(Data1,0);
  Size1 := 0;
End; // Data29.Clear

Function Data29.Decode(Buffer: TBuffer): Boolean;
Var I: LongInt;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags      := Buffer.GetDWORD;
    Size1      := Buffer.GetDWORD;
    SetLength(Data1,Size1);
    For I := 0 To Size1 - 1 Do Data1[I] := Buffer.GetDWORD;
    Size2 := Buffer.GetDWORD;
    If Size2 > 0 Then
    Begin
      SetLength(Data2,Size2);
      If Size2 > 0 Then Move(Buffer.GetBYTE(Size2)^,Data2[0],Size2);
      DecodeBytes(Data2);

      // Pad to DWORD boundary

      If (Buffer.Position And 3) <> 0 Then Buffer.GetBYTE(4 - (Buffer.Position And 3));
    End;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data29.Decode

Procedure Data29.Encode(Buffer: TBuffer);
Var I: LongInt;
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutDWORD(Size1);
  For I := 0 To Size1 - 1 Do Buffer.PutDWORD(Data1[I]);
  Buffer.PutDWORD(Size2);
  DecodeBytes(Data2);
  If Size2 > 0 Then Buffer.PutBYTE(@Data2[0],Size2);

  // Pad to DWORD boundary

  I := 0;
  While (Buffer.Position And 3) <> 0 Do Buffer.PutBYTE(@I,1);
  DecodeBytes(Data2);
End; // Data29.Encode

Procedure Data29.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    Size1: $' + IntToHex(Size1,8));
  WriteLn(F,'    Size2: $' + IntToHex(Size2,8));
  WriteLn(F,'    Data2:  ' + StrPas(@Data2[0]));
  PrintHex(F,@Data1[0], Size1 * $4, $4, 0, 0);
  PrintAscFooter(F);
End; // Data29.PrintAsc

Constructor Data2A.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$2A,Name);
End; // Data2A.Create

Destructor Data2A.Destroy;
Begin
  Clear;
  Inherited;
End; // Data2A.Destroy

Procedure Data2A.Clear;
Begin
  Inherited;
  SetLength(Data1,0);
  Size1 := 0;
End; // Data2A.Clear

Function Data2A.Decode(Buffer: TBuffer): Boolean;
Var I: LongInt;
Begin
  Clear;
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    Flags := Buffer.GetDWORD;
    Size1 := Buffer.GetDWORD;
    SetLength(Data1,Size1);
    For I := 0 To Size1 - 1 Do Data1[I] := Buffer.GetDWORD;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data2A.Decode

Procedure Data2A.Encode(Buffer: TBuffer);
Var I: Integer;
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutDWORD(Size1);
  For I := 0 To Size1 - 1 Do Buffer.PutDWORD(Data1[I]);
End; // Data2A.Encode

Procedure Data2A.PrintAsc(Var F: System.Text);
Var I: LongInt;
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Fragment: ' + Fragment.GetName);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    Size1: $' + IntToHex(Size1,8));
  For I := 0 To Size1 - 1 Do WriteLn(F,'    Data1[' + IntToStr(I) + ']: $' + IntToHex(Data1[I],8));
  PrintAscFooter(F);
End; // Data2A.PrintAsc

Constructor Data2C.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$2C,Name);
  SetLength(Data1,0);
  SetLength(Data2,0);
  SetLength(Data3,0);
  SetLength(Data4,0);
  SetLength(Data5,0);
  SetLength(Data6,0);
  SetLength(Data7,0);
  SetLength(Data8,0);
  SetLength(Data9,0);
  SetLength(Data10,0);
  Size1     := 0;
  Size2     := 0;
  Size3     := 0;
  Size4     := 0;
  Size5     := 0;
  Size6     := 0;
  Size7     := 0;
  Size8     := 0;
  Size9     := 0;
  Size10    := 0;
  Fragment1 := Nil;
  Fragment2 := Nil;
  Fragment3 := Nil;
End; // Data2C.Create

Destructor Data2C.Destroy;
Begin
  Clear;
End; // Data2C.Destroy

Procedure Data2C.Clear;
Begin
  SetLength(Data1,0);
  SetLength(Data2,0);
  SetLength(Data3,0);
  SetLength(Data4,0);
  SetLength(Data5,0);
  SetLength(Data6,0);
  SetLength(Data7,0);

  If (Flags And $200)  <> 0 Then SetLength(Data8,0);
  If (Flags And $800)  <> 0 Then SetLength(Data9,0);
  If (Flags And $1000) <> 0 Then SetLength(Data10,0);

  Size1  := 0;
  Size2  := 0;
  Size3  := 0;
  Size4  := 0;
  Size5  := 0;
  Size6  := 0;
  Size7  := 0;
  Size8  := 0;
  Size9  := 0;
  Size10 := 0;

  Fragment1.Free;
  Fragment2.Free;
  Fragment3.Free;
  Fragment1 := Nil;
  Fragment2 := Nil;
  Fragment3 := Nil;
End; // Data2C.Clear

Function Data2C.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags     := Buffer.GetDWORD;
    Size1     := Buffer.GetDWORD;
    Size2     := Buffer.GetDWORD;
    Size3     := Buffer.GetDWORD;
    Size4     := Buffer.GetDWORD;
    Size5     := Buffer.GetDWORD;
    Size6     := Buffer.GetDWORD;
    Size7     := Buffer.GetDWORD;
    Fragment1 := DecodeFragmentReference(Buffer);
    If Fragment1 = Nil Then
    Begin
      Result := False;
      Exit;
    End;

    Fragment2 := DecodeFragmentReference(Buffer);

    If Buffer.GetDWORD <> 0 Then
    Begin
      Result := False;
      Exit;
    End;

    Fragment3 := DecodeFragmentReference(Buffer);

    If (Flags And 1) <> 0 Then
    Begin
      Params1[0] := Buffer.GetFLOAT;
      Params1[1] := Buffer.GetFLOAT;
      Params1[2] := Buffer.GetFLOAT;
    End
    Else
    Begin
      Params1[0] := 0;
      Params1[1] := 0;
      Params1[2] := 0;
      Buffer.GetBYTE(12);
    End;

    If (Flags And 2) <> 0 Then
    Begin
      Params2 := Buffer.GetDWORD;
    End
    Else
    Begin
      Params2 := $3F800000; // Why is this here???
      Buffer.GetDWORD;
    End;

    SetLength(Data1,Size1);       If Size1 > 0 Then Move(Buffer.GetBYTE($0C * Size1)^,Data1[0],$0C * Size1);
    SetLength(Data2,Size2);       If Size2 > 0 Then Move(Buffer.GetBYTE($08 * Size2)^,Data2[0],$08 * Size2);
    SetLength(Data3,Size3);       If Size3 > 0 Then Move(Buffer.GetBYTE($0C * Size3)^,Data3[0],$0C * Size3);
    SetLength(Data4,$04 * Size4); If Size4 > 0 Then Move(Buffer.GetBYTE($04 * Size4)^,Data4[0],$04 * Size4);
    SetLength(Data5,Size5);       If Size5 > 0 Then Move(Buffer.GetBYTE($10 * Size5)^,Data5[0],$10 * Size5);
    SetLength(Data6,$0C * Size6); If Size6 > 0 Then Move(Buffer.GetBYTE($0C * Size6)^,Data6[0],$0C * Size6);
    SetLength(Data7,Size7);       If Size7 > 0 Then Move(Buffer.GetBYTE($04 * Size7)^,Data7[0],$04 * Size7);

    If (Flags And $200) <> 0 Then
    Begin
      Size8 := Buffer.GetDWORD;

      If Size8 > 0 Then
      Begin
        SetLength(Data8,$04 * Size8);
        If Size8 > 0 Then Move(Buffer.GetBYTE($04 * Size8)^,Data8[0],$04 * Size8);
      End;
    End;

    If (Flags And $800) <> 0 Then
    Begin
      Size9 := Buffer.GetDWORD;
      If Size9 > 0 Then
      Begin
        SetLength(Data9,Size9);
        If Size9 > 0 Then Move(Buffer.GetBYTE($04 * Size9)^,Data9[0],$04 * Size9);
      End;
    End;

    If (Flags And $1000) <> 0 Then
    Begin
      Size10 := Buffer.GetDWORD;
      If Size10 > 0 Then
      Begin
        SetLength(Data10,Size10);
        If Size10 > 0 Then Move(Buffer.GetBYTE($04 * Size10)^,Data10[0],$04 * Size10);
      End;
    End;

    If (Flags And $2000) <> 0 Then
    Begin
      Params3[0] := Buffer.GetDWORD;
      Params3[1] := Buffer.GetDWORD;
      Params3[2] := Buffer.GetDWORD;
    End
    Else
    Begin
      Params3[0] := 0;
      Params3[1] := 0;
      Params3[2] := 0;
    End;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data2C.Decode

Procedure Data2C.Encode(Buffer: TBuffer);
Var L: LongWord;
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutDWORD(Size1);
  Buffer.PutDWORD(Size2);
  Buffer.PutDWORD(Size3);
  Buffer.PutDWORD(Size4);
  Buffer.PutDWORD(Size5);
  Buffer.PutDWORD(Size6);
  Buffer.PutDWORD(Size7);
  Buffer.PutDWORD(0);
  Buffer.PutDWORD(0);
  Buffer.PutDWORD(0);
  Buffer.PutDWORD(0);

  L := 0;
  If (Flags And 1) <> 0 Then
  Begin
    Buffer.PutFLOAT(Params1[0]);
    Buffer.PutFLOAT(Params1[1]);
    Buffer.PutFLOAT(Params1[2]);
  End
  Else
  Begin
    Buffer.PutDWORD(L);
    Buffer.PutDWORD(L);
    Buffer.PutDWORD(L);
  End;

  If (Flags And 2) <> 0
   Then Buffer.PutFLOAT(Params2)
   Else Buffer.PutDWORD(L);

  If Size1 > 0 Then
  Begin
    Buffer.PutBYTE(@Data1[0], Size1 * $0C);
    Buffer.PutBYTE(@Data2[0], Size1 * $08);
    Buffer.PutBYTE(@Data3[0], Size1 * $0C);
    Buffer.PutBYTE(@Data4[0], Size1 * $04);
    Buffer.PutBYTE(@Data5[0], Size1 * $10);
    Buffer.PutBYTE(@Data6[0], Size1 * $0C);
    Buffer.PutBYTE(@Data7[0], Size1 * $04);
  End;

  If (Flags And $200) <> 0 Then
  Begin
    Buffer.PutDWORD(Size8);
    If Size8 > 0 Then Buffer.PutBYTE(@Data8[0], Size8 * $04);
  End;

  If (Flags And $800) <> 0 Then
  Begin
    Buffer.PutDWORD(Size9);
    If Size9 > 0 Then Buffer.PutBYTE(@Data9[0], Size9 * $04);
  End;

  If (Flags And $1000) <> 0 Then
  Begin
    Buffer.PutDWORD(Size10);
    If Size10 > 0 Then Buffer.PutBYTE(@Data10[0], Size10 * $04);
  End;

  If (Flags And $2000) <> 0 Then
  Begin
    Buffer.PutDWORD(Params3[0]);
    Buffer.PutDWORD(Params3[1]);
    Buffer.PutDWORD(Params3[2]);
  End;
End; // Data2C.Encode

Procedure Data2C.PrintAsc(Var F: System.Text);
Var I: LongInt;
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    Fragment1: ' + Fragment1.GetName);

  If Not Fragment2.Exclam Then WriteLn(F,'    Fragment2: ' + Fragment2.GetName);
  If Not Fragment3.Exclam Then WriteLn(F,'    Fragment3: ' + Fragment3.GetName);

  If (Flags And 1) <> 0 Then
   WriteLn(F,'    Params1: ' + FloatToStrF(Params1[0],ffFixed,9,6) +//IntToHex(Params1[0],8) +
                         ' ' + FloatToStrF(Params1[1],ffFixed,9,6) +//IntToHex(Params1[1],8) +
                         ' ' + FloatToStrF(Params1[2],ffFixed,9,6));//IntToHex(Params1[2],8));

  If (Flags And 2) <> 0 Then
   WriteLn(F,'    Params2: ' + FloatToStrF(Params2,ffFixed,9,6));//IntToHex(Params2[0],8));

  If (Flags And $2000) <> 0 Then
   WriteLn(F,'    Params3: $' + IntToHex(Params3[0],8) +
                         ' $' + IntToHex(Params3[1],8) +
                         ' $' + IntToHex(Params3[2],8));

  WriteLn(F,'    Size1: $' + IntToHex(Size1,4));
  For I := 0 To Size1 - 1 Do
   WriteLn(F,'    ' + IntToHex(I,4) +
               ': ' + FloatToStrF(Data1[I].X,ffFixed,9,6) +
                ' ' + FloatToStrF(Data1[I].Y,ffFixed,9,6) +
                ' ' + FloatToStrF(Data1[I].Z,ffFixed,9,6));

  WriteLn(F,'    Size2: $' + IntToHex(Size2,4));
  For I := 0 To Size2 - 1 Do
   WriteLn(F,'    ' + IntToHex(I,4) +
               ': ' + FloatToStrF(Data2[I].TX,ffFixed,9,6) +
                ' ' + FloatToStrF(Data2[I].TZ,ffFixed,9,6));

  WriteLn(F,'    Size3: $' + IntToHex(Size3,4));
  For I := 0 To Size3 - 1 Do
   WriteLn(F,'    ' + IntToHex(I,4) +
               ': ' + FloatToStrF(Data3[I].X,ffFixed,9,6) +
                ' ' + FloatToStrF(Data3[I].Y,ffFixed,9,6) +
                ' ' + FloatToStrF(Data3[I].Z,ffFixed,9,6));

//  WriteLn(F,'    Size1: $' + IntToHex(Size1,8)); PrintHex(F,@Data1[0], Size1 * $0C, $0C, 0, 0);
//  WriteLn(F,'    Size2: $' + IntToHex(Size2,8)); PrintHex(F,@Data2[0], Size2 * $08, $08, 0, 0);
//  WriteLn(F,'    Size3: $' + IntToHex(Size3,8)); PrintHex(F,@Data3[0], Size3 * $0C, $0C, 0, 0);
  WriteLn(F,'    Size4: $' + IntToHex(Size4,8)); PrintHex(F,@Data4[0], Size4 * $04, $04, 0, 0);
  WriteLn(F,'    Size5: $' + IntToHex(Size5,8)); PrintHex(F,@Data5[0], Size5 * $10, $10, 0, 0);
  WriteLn(F,'    Size6: $' + IntToHex(Size6,8)); PrintHex(F,@Data6[0], Size6 * $0C, $0C, 0, 0);
  WriteLn(F,'    Size7: $' + IntToHex(Size7,8)); PrintHex(F,@Data7[0], Size7 * $04, $04, 0, 0);

  If (Flags And $200) <> 0 Then
  Begin
    WriteLn(F,'    SIZE8: $' + IntToHex(Size8,8));
    PrintHex(F,@Data8[0], Size8 * $4, $4, 0, 0);
  End;

  If (Flags And $800) <> 0 Then
  Begin
    WriteLn(F,'    SIZE9: $' + IntToHex(Size9,8));
    PrintHex(F,@Data9[0], Size9 * $4, $4, 0, 0);
  End;

  If (Flags And $800) <> 0 Then
  Begin
    WriteLn(F,'    Size10: $' + IntToHex(Size10,8));
    PrintHex(F,@Data10[0], Size10 * $4, $4, 0, 0);
  End;

  PrintAscFooter(F);
End; // Data2C.PrintAsc

Constructor Data2D.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$2D,Name);
End; // Data2D.Create

Function Data2D.Decode(Buffer: TBuffer): Boolean;
Begin
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    If Buffer.GetDWORD <> 0 Then Result := False Else Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data2D.Decode

Procedure Data2D.Encode(Buffer: TBuffer);
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(0);
End; // Data2D.Encode

Procedure Data2D.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Fragment: ' + Fragment.GetName);
  PrintAscFooter(F);
End; // Data2D.PrintAsc

Constructor Data2F.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$2F,Name);
End; // Data2F.Create

Function Data2F.Decode(Buffer: TBuffer): Boolean;
Begin
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    Flags  := Buffer.GetDWORD;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data2F.Decode

Procedure Data2F.Encode(Buffer: TBuffer);
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
End; // Data2F.Encode

Procedure Data2F.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $'    + IntToHex(Flags,8));
  WriteLn(F,'    Fragment: ' + Fragment.GetName);
  PrintAscFooter(F);
End; // Data2F.PrintAsc

Constructor Data30.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$30,Name);
End; // Data30.Create

Function Data30.Decode(Buffer: TBuffer): Boolean;
Begin
  Clear;
  If BaseDecode(Buffer) Then
  Begin
    Flags      := Buffer.GetDWORD;
    Params1[0] := Buffer.GetDWORD;
    Params2[0] := Buffer.GetDWORD;
    Params3[0] := Buffer.GetFLOAT;
    Params3[1] := Buffer.GetFLOAT;
    Inherited Decode(Buffer);
    If (Flags And 2) <> 0 Then DecodeDataPair(Buffer, Pair) Else
    Begin
      Pair.Data1 := 0;
      Pair.Data2 := 0;
    End;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data30.Decode

Procedure Data30.Encode(Buffer: TBuffer);
Begin
  BaseEncode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutDWORD(Params1[0]);
  Buffer.PutDWORD(Params2[0]);
  Buffer.PutFLOAT(Params3[0]);
  Buffer.PutFLOAT(Params3[1]);
  Inherited Encode(Buffer);
  If (Flags And 2) <> 0 Then EncodeDataPair(Buffer,Pair);
End; // Data30.Encode

Procedure Data30.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    Params1: $' + IntToHex(Params1[0],8));
  WriteLn(F,'    Params2: $' + IntToHex(Params2[0],8));
  WriteLn(F,'    Params3: ' + FloatToStr(Params3[0]) + ' ' + FloatToStr(Params3[1]));
  WriteLn(F,'    Fragment1: ' + Fragment.GetName);
  If (Flags And 2) <> 0 Then
   WriteLn(F,'    DATA_PAIR: $' + IntToHex(Pair.Data1,8) + ' ' + FloatToStr(Pair.Data2));
  PrintAscFooter(F);
End; // Data30.PrintAsc

Constructor Data31.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$31,Name);
  Size1 := 0;
  SetLength(Data1,0);
End; // Data31.Create

Destructor Data31.Destroy;
Begin
  Clear;
End; // Data31.Destroy

Procedure Data31.Clear;
Var I: Integer;
Begin
  For I := 0 To High(Data1) Do Data1[I].Free;
  SetLength(Data1,0);
  Size1 := 0;
End; // Data31.Clear

Function Data31.Decode(Buffer: TBuffer): Boolean;
Var I,Size: LongInt;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    If Buffer.GetDWORD = 0 Then
    Begin
      Size := Buffer.GetDWORD;
      SetLength(Data1,Size);
      For I := 0 To Size - 1 Do
      Begin
        Size1    := I + 1;
        Data1[I] := DecodeFragmentReference(Buffer);
        If Data1[I] = Nil Then
        Begin
          Result := False;
          Exit;
        End;
      End; // For I
      Result := Buffer.AtEnd;
    End
    Else Result := False;
  End
  Else Result := False;
End; // Data31.Decode

Procedure Data31.Encode(Buffer: TBuffer);
Var I: Integer;
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(0);
  Buffer.PutDWORD(Size1);
  For I := 0 To Size1 - 1 Do
   Buffer.PutDWORD(Parent.GetFragmentIndex(Data1[I].Fragment));
End; // Data31.Encode

Procedure Data31.PrintAsc(Var F: System.Text);
Var I: LongInt;
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Size1: $' + IntToHex(Size1,4));
  For I := 0 To Size1 - 1 Do
   WriteLn(F,'    ' + IntToHex(I,4) + ' Fragment1: ' + Data1[I].GetName);
  PrintAscFooter(F);
End; // Data31.PrintAsc

Constructor Data32.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$32,Name);
  Data1 := 0;
  Count := 0;
  Data2 := 0;
  Data3 := 0;
  Data4 := 0;
  SetLength(Data5,0);
End; // Data32.Create

Destructor Data32.Destroy;
Begin
  Clear;
End; // Data32.Destroy

Procedure Data32.Clear;
Begin
  SetLength(Data5,0);
End; // Data32.Clear

Function Data32.Decode(Buffer: TBuffer): Boolean;
Var I: LongInt;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Data1 := Buffer.GetDWORD;
    Count := Buffer.GetDWORD;
    Data2 := Buffer.GetDWORD;
    Data3 := Buffer.GetDWORD;
    Data4 := Buffer.GetDWORD;
    SetLength(Data5,Count);
    For I := 0 To Count - 1 Do Data5[I].Color := Buffer.GetDWORD;
    Result := True;
  End
  Else Result := False;
End; // Data32.Decode

Procedure Data32.Encode(Buffer: TBuffer);
Var I: Integer;
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Data1);
  Buffer.PutDWORD(Count);
  Buffer.PutDWORD(Data2);
  Buffer.PutDWORD(Data3);
  Buffer.PutDWORD(Data4);
  For I := 0 To High(Data5) Do Buffer.PutDWORD(Data5[I].Color);
End; // Data32.Encode

Procedure Data32.PrintAsc(Var F: System.Text);
Var I: LongInt;
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Data1: ' + IntToStr(Data1));
  WriteLn(F,'    Count: ' + IntToStr(Count));
  WriteLn(F,'    Data2: ' + IntToStr(Data2));
  WriteLn(F,'    Data3: ' + IntToStr(Data3));
  WriteLn(F,'    Data4: ' + IntToStr(Data4));
  For I := 0 To High(Data5) Do WriteLn(F,'    Data5[' + IntToStr(I) + ']: $' + IntToHex(Data5[I].Color,8));
  PrintAscFooter(F);
End; // Data32.PrintAsc

Constructor Data33.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$33,Name);
End; // Data05.Create

Function Data33.Decode(Buffer: TBuffer): Boolean;
Begin
  If BaseDecode(Buffer) Then
  Begin
    Inherited Decode(Buffer);
    Flags  := Buffer.GetDWORD;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data33.Decode

Procedure Data33.Encode(Buffer: TBuffer);
Begin
  BaseEncode(Buffer);
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
End; // Data33.Encode

Procedure Data33.PrintAsc(Var F: System.Text);
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $'   + IntToHex(Flags,8));
  WriteLn(F,'    Fragment: ' + Fragment.GetName);
  PrintAscFooter(F);
End; // Data33.PrintAsc

Constructor Data34.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$34,Name);
  Fragment := Nil;
End; // Data34.Create

Destructor Data34.Destroy;
Begin
  Clear;
End; // Data34.Destroy

Procedure Data34.Clear;
Begin
  Fragment.Free;
  Fragment := Nil;
End; // Data34.Clear

Function Data34.Decode(Buffer: TBuffer): Boolean;
Var I: LongInt;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags := Buffer.GetDWORD;
    For I := 0 To $12 Do Params1[I] := Buffer.GetDWORD;

    If (Flags And 1) <> 0 Then
    Begin
      For I := 0 To 1 Do
      Begin
        Params2[I][0] := Buffer.GetDWORD;
        Params2[I][1] := Buffer.GetDWORD;
        Params2[I][2] := Buffer.GetDWORD;
      End; // For I
    End
    Else
    Begin
      For I := 0 To 1 Do
      Begin
        Params2[I][0] := 0;
        Params2[I][1] := 0;
        Params2[I][2] := 0;
      End; // For I
    End;

    If (Flags And 2) <> 0 Then
    Begin
      For I := 0 To 1 Do
      Begin
        Params3[I][0] := Buffer.GetDWORD;
        Params3[I][1] := Buffer.GetDWORD;
        Params3[I][2] := Buffer.GetDWORD;
      End; // For I
    End
    Else
    Begin
      For I := 0 To 1 Do
      Begin
        Params3[I][0] := 0;
        Params3[I][1] := 0;
        Params3[I][2] := 0;
      End; // For I
    End;

    If (Flags And 4) <> 0 Then Fragment := DecodeFragmentReference(Buffer);
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data34.Decode

Procedure Data34.PrintAsc(Var F: System.Text);
Var I: LongInt;
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  PrintHex(F,@Params1[0], $13, 16, 4, -1, '    Params1.');

  If (Flags And 1) <> 0 Then
  Begin
    For I := 0 To 1 Do
     WriteLn(F,'    Params2_' + IntToStr(I) +
                        ': $' + IntToHex(Params2[I,0],8) +
                         ' $' + IntToHex(Params2[I,1],8) +
                         ' $' + IntToHex(Params2[I,2],8));
  End;

  If (Flags And 2) <> 0 Then
  Begin
    For I := 0 To 1 Do
     WriteLn(F,'    Params3_' + IntToStr(I) +
                        ': $' + IntToHex(Params3[I,0],8) +
                         ' $' + IntToHex(Params3[I,1],8) +
                         ' $' + IntToHex(Params3[I,2],8));
  End;

  If (Flags And 4) <> 0 Then WriteLn(F,'    Fragment: ' + Fragment.GetName);
  PrintAscFooter(F);
End; // Data34.PrintAsc

Constructor Data36.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$36,Name);
  SetLength(Data1,0);
  SetLength(Data2,0);
  SetLength(Data3,0);
  SetLength(Data4,0);
  SetLength(Data5,0);
  SetLength(Data6,0);
  SetLength(Data7,0);
  SetLength(Data8,0);
  SetLength(Data9,0);
  Size1     := 0;
  Size2     := 0;
  Size3     := 0;
  Size4     := 0;
  Size5     := 0;
  Size6     := 0;
  Size7     := 0;
  Size8     := 0;
  Size9     := 0;
  Size10    := 0;
  Fragment1 := Nil;
  Fragment2 := Nil;
  Fragment3 := Nil;
  Fragment4 := Nil;
End; // Data36.Create

Destructor Data36.Destroy;
Begin
  Clear;
End; // Data36.Destroy

Procedure Data36.Clear;
Begin
  SetLength(Data1,0);
  SetLength(Data2,0);
  SetLength(Data3,0);
  SetLength(Data4,0);
  SetLength(Data5,0);
  SetLength(Data6,0);
  SetLength(Data7,0);
  SetLength(Data8,0);
  SetLength(Data9,0);
  Size1  := 0;
  Size2  := 0;
  Size3  := 0;
  Size4  := 0;
  Size5  := 0;
  Size6  := 0;
  Size7  := 0;
  Size8  := 0;
  Size9  := 0;
  Size10 := 0;
  Fragment1.Free;
  Fragment2.Free;
  Fragment3.Free;
  Fragment4.Free;
  Fragment1 := Nil;
  Fragment2 := Nil;
  Fragment3 := Nil;
  Fragment4 := Nil;
End; // Data36.Clear

Function Data36.Decode(Buffer: TBuffer): Boolean;
Var
  _Data2 : Packed Array Of OldData36SubData2;
  I      : Integer;

Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags     := Buffer.GetDWORD;
    Fragment1 := DecodeFragmentReference(Buffer);
    Fragment2 := DecodeFragmentReference(Buffer);
    Fragment3 := DecodeFragmentReference(Buffer);

{    If (Flags And $10000) <> 0
     Then }Fragment4 := DecodeFragmentReference(Buffer);
{     Else
     Begin
       Buffer.GetDWORD;
       Fragment4 := Nil;
     End;}

{    If (Flags And 1) <> 0 Then
    Begin}
      Params1[0] := Buffer.GetFLOAT;
      Params1[1] := Buffer.GetFLOAT;
      Params1[2] := Buffer.GetFLOAT;
{    End
    Else
    Begin
      Buffer.GetFLOAT;
      Buffer.GetFLOAT;
      Buffer.GetFLOAT;
    End;}

{    If (Flags And $200) <> 0 Then
    Begin}
      Params2[0] := Buffer.GetDWORD;
      Params2[1] := Buffer.GetDWORD;
      Params2[2] := Buffer.GetDWORD;
{    End
    Else
    Begin
      Buffer.GetDWORD;
      Buffer.GetDWORD;
      Buffer.GetDWORD;
    End;}

{    If (Flags And $2) <> 0
     Then} Radius := Buffer.GetFLOAT;
{     Else Buffer.GetFLOAT;}

{    If (Flags And $4000) <> 0 Then
    Begin}
      Box.MinX := Buffer.GetFLOAT;
      Box.MinY := Buffer.GetFLOAT;
      Box.MinZ := Buffer.GetFLOAT;
      Box.MaxX := Buffer.GetFLOAT;
      Box.MaxY := Buffer.GetFLOAT;
      Box.MaxZ := Buffer.GetFLOAT;
{    End
    Else
    Begin
      Buffer.GetDWORD;
      Buffer.GetDWORD;
      Buffer.GetDWORD;
      Buffer.GetDWORD;
      Buffer.GetDWORD;
      Buffer.GetDWORD;
    End;}


    Size1  := Buffer.GetWORD;
    Size2  := Buffer.GetWORD;
    Size3  := Buffer.GetWORD;
    Size4  := Buffer.GetWORD;
    Size5  := Buffer.GetWORD;
    Size6  := Buffer.GetWORD;
    Size7  := Buffer.GetWORD;
    Size8  := Buffer.GetWORD;
    Size9  := Buffer.GetWORD;
    Size10 := Buffer.GetWORD;

    SetLength(Data1,Size1);
    SetLength(Data2,Size2);
    SetLength(Data3,Size3);
    SetLength(Data4,Size4);
    SetLength(Data5,Size5);
    SetLength(Data6,Size6);
    SetLength(Data7,Size7);
    SetLength(Data8,Size8);
    SetLength(Data9,Size9);

    If Size1 > 0 Then Move(Buffer.GetBYTE(6 * Size1)^,Data1[0],6 * Size1);

    // In new-format .WLD files, the texture coordinates are singles, not shorts

    If Parent.NewType Then
    Begin
      If Size2 > 0 Then Move(Buffer.GetBYTE(8 * Size2)^,Data2[0],8 * Size2);
    End
    Else
    Begin
      SetLength(_Data2,Size2);
      If Size2 > 0 Then Move(Buffer.GetBYTE(4 * Size2)^,_Data2[0],4 * Size2);
      For I := 0 To High(Data2) Do
      Begin
        Data2[I].I1 := _Data2[I].I1;
        Data2[I].I2 := _Data2[I].I2;
      End; // For I
      SetLength(_Data2,0);
    End;

//    Move(Buffer.GetBYTE(4 * Size2)^,Data2[0],4 * Size2);
    If Size3 > 0 Then Move(Buffer.GetBYTE(3 * Size3)^,Data3[0],3 * Size3);
    If Size4 > 0 Then Move(Buffer.GetBYTE(4 * Size4)^,Data4[0],4 * Size4);
    If Size5 > 0 Then Move(Buffer.GetBYTE(8 * Size5)^,Data5[0],8 * Size5);
    If Size6 > 0 Then Move(Buffer.GetBYTE(4 * Size6)^,Data6[0],4 * Size6);
    If Size7 > 0 Then Move(Buffer.GetBYTE(4 * Size7)^,Data7[0],4 * Size7);
    If Size8 > 0 Then Move(Buffer.GetBYTE(4 * Size8)^,Data8[0],4 * Size8);
    If Size9 > 0 Then Move(Buffer.GetBYTE(6 * Size9)^,Data9[0],6 * Size9);

    If (Buffer.Position And 3) <> 0
     Then Buffer.GetBYTE(4 - (Buffer.Position And 3))
     Else Buffer.GetBYTE(0);

    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data36.Decode

Procedure Data36.Encode(Buffer: TBuffer);
Var
  _Data2 : Packed Array Of OldData36SubData2;
  I      : Integer;

Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutDWORD(Parent.GetFragmentIndex(Fragment1.Fragment));
  Buffer.PutDWORD(Parent.GetFragmentIndex(Fragment2.Fragment));
  Buffer.PutDWORD(Parent.GetFragmentIndex(Fragment3.Fragment));
  If (Flags And $10000) <> 0
   Then Buffer.PutDWORD(Parent.GetFragmentIndex(Fragment4.Fragment))
   Else Buffer.PutDWORD(0);

  If (Flags And 1) <> 0 Then
  Begin
    Buffer.PutFLOAT(Params1[0]);
    Buffer.PutFLOAT(Params1[1]);
    Buffer.PutFLOAT(Params1[2]);
  End
  Else
  Begin
    Buffer.PutFLOAT(0);
    Buffer.PutFLOAT(0);
    Buffer.PutFLOAT(0);
  End;

  If (Flags And $200) <> 0 Then
  Begin
    Buffer.PutDWORD(Params2[0]);
    Buffer.PutDWORD(Params2[1]);
    Buffer.PutDWORD(Params2[2]);
  End
  Else
  Begin
    Buffer.PutDWORD(0);
    Buffer.PutDWORD(0);
    Buffer.PutDWORD(0);
  End;
{
  If (Flags And $2) <> 0
   Then Buffer.PutFLOAT(Radius)
   Else Buffer.PutFLOAT(0);
}
  Buffer.PutFLOAT(Radius);


  If (Flags And $4000) <> 0 Then
  Begin
    Buffer.PutFLOAT(Box.MinX);
    Buffer.PutFLOAT(Box.MinY);
    Buffer.PutFLOAT(Box.MinZ);
    Buffer.PutFLOAT(Box.MaxX);
    Buffer.PutFLOAT(Box.MaxY);
    Buffer.PutFLOAT(Box.MaxZ);
  End
  Else
  Begin
    Buffer.PutDWORD(0);
    Buffer.PutDWORD(0);
    Buffer.PutDWORD(0);
    Buffer.PutDWORD(0);
    Buffer.PutDWORD(0);
    Buffer.PutDWORD(0);
  End;

  Buffer.PutWord(Size1);
  Buffer.PutWord(Size2);
  Buffer.PutWord(Size3);
  Buffer.PutWord(Size4);
  Buffer.PutWord(Size5);
  Buffer.PutWord(Size6);
  Buffer.PutWord(Size7);
  Buffer.PutWord(Size8);
  Buffer.PutWord(Size9);
  Buffer.PutWord(Size10);

  If Size1 > 0 Then Buffer.PutBYTE(@Data1[0],6 * Size1);
  If Size2 > 0 Then
  Begin
    If Parent.NewType Then Buffer.PutBYTE(@Data2[0],8 * Size2)
    Else
    Begin
      SetLength(_Data2,Size2);
      For I := 0 To High(Data2) Do
      Begin
        _Data2[I].I1 := Data2[I].I1;
        _Data2[I].I2 := Data2[I].I2;
      End; // For I
      Buffer.PutBYTE(@_Data2[0],4 * Size2);
      SetLength(_Data2,0);
    End;
  End;
//  If Size2 > 0 Then Buffer.PutBYTE(@Data2[0],4 * Size2);
  If Size3 > 0 Then Buffer.PutBYTE(@Data3[0],3 * Size3);
  If Size4 > 0 Then Buffer.PutBYTE(@Data4[0],4 * Size4);
  If Size5 > 0 Then Buffer.PutBYTE(@Data5[0],8 * Size5);
  If Size6 > 0 Then Buffer.PutBYTE(@Data6[0],4 * Size6);
  If Size7 > 0 Then Buffer.PutBYTE(@Data7[0],4 * Size7);
  If Size8 > 0 Then Buffer.PutBYTE(@Data8[0],4 * Size8);
  If Size9 > 0 Then Buffer.PutBYTE(@Data9[0],6 * Size9);
End; // Data36.Encode

Procedure Data36.PrintAsc(Var F: System.Text);
Var
  I : LongInt;
  V : Double;

Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    Fragment1: ' + Fragment1.GetName);

  If Not Fragment2.Exclam Then WriteLn(F,'    Fragment2: ' + Fragment2.GetName);
  If Not Fragment3.Exclam Then WriteLn(F,'    Fragment3: ' + Fragment3.GetName);

  If {((Flags And $10000) <> 0) And }Not Fragment4.Exclam Then WriteLn(F,'    Fragment4: ' + Fragment4.GetName);
{  If (Flags And $00001) <> 0 Then} WriteLn(F,'    Params1: ' + FloatToStr(Params1[0]) +
                                                        ' ' + FloatToStr(Params1[1]) +
                                                        ' ' + FloatToStr(Params1[2]));
{  If (Flags And $00200) <> 0 Then} WriteLn(F,'    Params2: $' + IntToHex(Params2[0],8) +
                                                          ' $' + IntToHex(Params2[1],8) +
                                                          ' $' + IntToHex(Params2[2],8));
{  If (Flags And $00002) <> 0 Then} WriteLn(F,'    Radius:  ' + FloatToStr(Radius));
{  If (Flags And $04000) <> 0 Then} WriteLn(F,'    Box: ' + FloatToStr(Box.MinX) +
                                                      ' ' + FloatToStr(Box.MinY) +
                                                      ' ' + FloatToStr(Box.MinZ) +
                                                      ' ' + FloatToStr(Box.MaxX) +
                                                      ' ' + FloatToStr(Box.MaxY) +
                                                      ' ' + FloatToStr(Box.MaxZ));

  WriteLn(F,'    Size1: $' + IntToHex(Size1,4));

  V := 1 Shl Size10;

  For I := 0 To Size1 - 1 Do
   WriteLn(F,'    ' + IntToHex(I,4) +
               ': ' + FloatToStrF(Data1[I].X / V,ffFixed,9,6) +
                ' ' + FloatToStrF(Data1[I].Y / V,ffFixed,9,6) +
                ' ' + FloatToStrF(Data1[I].Z / V,ffFixed,9,6));

  WriteLn(F,'    Size2: $' + IntToHex(Size2,4));

//  Parent.PrintHex(F,Data1, Size1 * 6, 6);
  WriteLn(F,'    Size2: $' + IntToHex(Size2,4));  PrintHex(F,@Data2[0], Size2 * 4, 4, 0, 0);
  WriteLn(F,'    Size3: $' + IntToHex(Size3,4));  PrintHex(F,@Data3[0], Size3 * 3, 3, 0, 0);
  WriteLn(F,'    Size4: $' + IntToHex(Size4,4));  PrintHex(F,@Data4[0], Size4 * 4, 4, 0, 0);
  WriteLn(F,'    Size5: $' + IntToHex(Size5,4));  PrintHex(F,@Data5[0], Size5 * 8, 8, 0, 0);
  WriteLn(F,'    Size6: $' + IntToHex(Size6,4));  PrintHex(F,@Data6[0], Size6 * 4, 4, 0, 0);
  WriteLn(F,'    Size7: $' + IntToHex(Size7,4));  PrintHex(F,@Data7[0], Size7 * 4, 4, 0, 0);
  WriteLn(F,'    Size8: $' + IntToHex(Size8,4));  PrintHex(F,@data8[0], size8 * 4, 4, 0, 0);
  WriteLn(F,'    Size9: $' + IntToHex(Size9,4));

  For I := 0 To Size9 - 1 Do
  Begin
    If Data9[I].Data3 <> 4 Then
     WriteLn(F,'    ' + IntToHex(I,4) +
                 ': $' + IntToHex(Data9[I].Parm.VertexIndex1,4) +
                  ' $' + IntToHex(Data9[I].Parm.VertexIndex2,4) +
                  ' ' + IntToStr(Data9[I].Data2) +
                  ' ' + IntToStr(Data9[I].Data3))
    Else
     WriteLn(F,'    ' + IntToHex(I,4) +
                 ': ' + FloatToStr(Data9[I].Parm.Float) +
                  ' ' + IntToStr(Data9[I].Data2) +
                  ' ' + IntToStr(Data9[I].Data3));
  End; // For I

  WriteLn(F,'    Size10: $' + IntToHex(Size10,4));
  PrintAscFooter(F);
End; // Data36.PrintAsc

Constructor Data37.Create(Data: TEQWldData; Name: String = '');
Begin
  Inherited Create(Data,$37,Name);
  SetLength(Data1,0);
  Size1  := 0;
  Size2  := 0;
  Size3  := 0;
  Size4  := 0;
  Size5  := 0;
  Size6  := 0;
End; // Data37.Create

Destructor Data37.Destroy;
Begin
  Clear;
End; // Data37.Destroy

Procedure Data37.Clear;
Var I: Integer;
Begin
  For I := 0 To High(Data1) Do SetLength(Data1[I].Data,0);
  SetLength(Data1,0);
  Size1  := 0;
  Size2  := 0;
  Size3  := 0;
  Size4  := 0;
  Size5  := 0;
  Size6  := 0;
End; // Data37.Clear

Function Data37.Decode(Buffer: TBuffer): Boolean;
Var I: Integer;
Begin
  Clear;
  If Inherited Decode(Buffer) Then
  Begin
    Flags  := Buffer.GetDWORD;
    Size1  := Buffer.GetWORD;
    Size2  := Buffer.GetWORD;
    Size3  := Buffer.GetWORD;
    Size4  := Buffer.GetWORD;
    Size5  := Buffer.GetWORD;
    SetLength(Data1,Size2);
    For I := 0 To Size2 - 1 Do
    Begin
      SetLength(Data1[I].Data,Size1);
      If Size1 > 0 Then Move(Buffer.GetBYTE(Size1 * 6)^,Data1[I].Data[0],Size1 * 6);
    End; // For I
    Size6  := Buffer.GetWORD;
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // Data37.Decode

Procedure Data37.Encode(Buffer: TBuffer);
Var I: Integer;
Begin
  Inherited Encode(Buffer);
  Buffer.PutDWORD(Flags);
  Buffer.PutWord(Size1);
  Buffer.PutWord(Size2);
  Buffer.PutWord(Size3);
  Buffer.PutWord(Size4);
  Buffer.PutWord(Size5);
  If Size1 > 0 Then
  Begin
    For I := 0 To Size2 - 1 Do Buffer.PutBYTE(@Data1[I].Data[0],Size1 * 6);
  End;
  Buffer.PutWord(Size6);
End; // Data37.Encode

Procedure Data37.PrintAsc(Var F: System.Text);
Var I,J,K: LongInt;
Begin
  PrintAscHeader(F);
  WriteLn(F,'    Flags: $' + IntToHex(Flags,8));
  WriteLn(F,'    Size1: $' + IntToHex(Size1,4));
  WriteLn(F,'    Size2: $' + IntToHex(Size2,4));
  WriteLn(F,'    Size3: $' + IntToHex(Size3,4));
  WriteLn(F,'    Size4: $' + IntToHex(Size4,4));
  WriteLn(F,'    Size5: $' + IntToHex(Size5,4));
  K := 1 Shl Size5;
  For I := 0 To Size2 - 1 Do
  Begin
    For J := 0 To Size1 - 1 Do
    Begin
      WriteLn(F,'    ' + IntToHex(I,4) + ' ' + IntToHex(J,4) + ' ' + FloatToStr(Data1[I].Data[J].X / K) +
                                                               ' ' + FloatToStr(Data1[I].Data[J].Y / K) +
                                                               ' ' + FloatToStr(Data1[I].Data[J].Z / K));
    End; // For J
    WriteLn(F);
  End; // For I
  WriteLn(F,'    Size6: $' + IntToHex(Size6,4));
  PrintAscFooter(F);
End; // Data37.PrintAsc

Constructor DataWithName.Create(Data: TEQWldData; ID: LongWord; Name: String = '');
Begin
  Inherited Create(Data,ID,Name);
  Size := 0;
  SetLength(Data1,0);
End; // DataWithName.Create

Destructor DataWithName.Destroy;
Begin
  Clear;
End; // DataWithName.Destroy

Procedure DataWithName.Clear;
Begin
  SetLength(Data1,0);
  Size := 0;
End; // DataWithName.Clear

Function DataWithName.Decode(Buffer: TBuffer): Boolean;
Begin
  If Inherited Decode(Buffer) Then
  Begin
    Size := Buffer.Len - Buffer.Position;
    SetLength(Data1,Size);
    If Size > 0 Then Move(Buffer.GetBYTE(Size)^,Data1[0],Size);
    Result := Buffer.AtEnd;
  End
  Else Result := False;
End; // DataWithName.Decode

Procedure DataWithName.Encode(Buffer: TBuffer);
Begin
  Inherited Encode(Buffer);
  If Size > 0 Then Buffer.PutBYTE(Data1,Size);
End; // DataWithName.Encode     

Procedure DataWithName.PrintAsc(Var F: System.Text);
Begin
  Write(F,'R');
  PrintAscHeader(F);
  If High(Data1) >= 0 Then PrintHex(F, @Data1[0], Size, 16, 1, 4);
  Write(F,'R');
  PrintAscFooter(F);
End; // DataWithName.PrintAsc

End.
