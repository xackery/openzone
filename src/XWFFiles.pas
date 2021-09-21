Unit XWFFiles;

Interface

Uses Classes;

Const
  fourccTexture         = 'tex ';
  fourccVertexGroup     = 'vert';
  fourccPolygonGroup    = 'poly';
  fourccObject          = 'obj ';
  fourccObjectPlacement = 'objp';
  fourccOctreeNode      = 'octr';
  fourccBSPTreeNode     = 'bsp-';
  fourccWorld           = 'wld ';
  fourccNames           = 'name';
  fourccBone            = 'bone';
  fourccCreature        = 'crea';
  fourccVertBoneGroup   = 'vbon';
  fourccAnimation       = 'anim';
  fourccLights          = 'lite';
  fourccVertexColors    = 'vcol';

  colortypeAmbient  = 0;
  colortypeDiffuse  = 1;
  colortypeSpecular = 2;
  colortypeEmissive = 3;

Type
  TFourCC = Packed Array[0..3] Of Char;

  TXWFAtomRec = Packed Record
    FourCC   : TFourCC;
    Children : LongWord;
    Size     : LongWord;
  End;

  PXWFObjectPlacementRec = ^TXWFObjectPlacementRec;
  TXWFObjectPlacementRec = Packed Record
    ID       : LongWord;   // Object ID
    X,Y,Z    : Double;     // Position
    RX,RY,RZ : Double;     // Rotation
    SX,SY,SZ : Double;     // Scale
  End;

  PXWFVertexBoneRec = ^TXWFVertexBoneRec;
  TXWFVertexBoneRec = Packed Record
    BoneIndex : Integer;
  End;

  PXWFVertexRec = ^TXWFVertexRec;
  TXWFVertexRec = Packed Record
    X,Y,Z : Double;     // Position
    I,J,K : Double;     // Normal
    U,V   : Double;     // Texture coordinates
  End;

  PXWFVertexColorRec = ^TXWFVertexColorRec;
  TXWFVertexColorRec = Packed Record
    VertexIndex : LongWord;
    Color       : LongWord; // RGBA, where the first byte is R
  End;

  PXWFPolygonRec = ^TXWFPolygonRec;
  TXWFPolygonRec = Packed Record
    V1,V2,V3  : LongWord; // Vertex indices in the indicated vertex group
    TextureID : LongWord; // Texture ID
    Flags     : LongWord; // Bit 1 ... 0 = solid, 1 = can be walked through
                          // Bit 2 ... 1 = transparent (TEMPORARY UNTIL SHADERS ARE DEFINED)
                          // Bit 3 ... 1 = semitransparent (TEMPORARY UNTIL SHADERS ARE DEFINED)
  End;

  PXWFOctreeNodeRec = ^TXWFOctreeNodeRec;
  TXWFOctreeNodeRec = Packed Record
    Center : Packed Array[0..2] Of Double;
    Size   : Packed Array[0..2] Of Double;
  End;

  PXWFBSPTreeNodeRec = ^TXWFBSPTreeNodeRec;
  TXWFBSPTreeNodeRec = Packed Record
    Normal   : Packed Array[0..2] Of Double;
    Distance : Double;
    Center   : Packed Array[0..2] Of Double;
    Radius   : Double;
  End;

  PXWFBoneFrameRec = ^TXWFBoneFrameRec;
  TXWFBoneFrameRec = Packed Record
    Position    : Packed Array[0..2] Of Double; // X,Y,Z
    Orientation : Packed Array[0..3] Of Double; // W,X,Y,Z
  End;

  PXWFLightRec = ^TXWFLightRec;
  TXWFLightRec = Packed Record
    X,Y,Z    : Double;   // Light center position
    R,G,B    : Double;   // Each ranges from 0 to 1
    Radius   : Double;
    Ambient  : LongWord; // Nonzero if the light is ambient (no discernible center or attenuation)
  End;

  // Classes for dealing with atoms

  TXWFAtom = Class
  Protected
    FAtom     : TXWFAtomRec;
    FChildren : TList;       // Child atoms
    FFourCC   : String;      // Pascal copy of the FourCC to make life easier
    FSize     : LongWord;    // Copy of FAtom.Size
    FData     : Pointer;
    Function    GetChildCount: Integer;
    Function    GetChild(Index: Integer): TXWFAtom;
    Function    GetString(P: Pointer): String;
    Procedure   WriteString(St: String; P: Pointer);
    Function    ReadString(Var P: Pointer): String;
    Procedure   ReadFromStream(Stream: TStream);
    Procedure   SetupAtom;
    Procedure   BuildData; Dynamic;
    Procedure   UnpackData; Dynamic;
    Function    GetDataSize: Integer; Dynamic;
    Procedure   AddToData(Var P: Pointer); Dynamic;
    Procedure   ReadFromData(Var P: Pointer); Dynamic;
  Public
    Constructor Create; Overload;
    Constructor Create(AFourCC: String); Overload;
    Constructor Create(Const AtomRec: TXWFAtomRec; Stream: TStream); Overload;
    Constructor Create(Stream: TStream); Overload;
    Destructor  Destroy; Override;
    Procedure   WriteToStream(Stream: TStream);
    Procedure   AddChild(Child: TXWFAtom);
    Function    FindChild(AFourCC: String): TXWFAtom;
    Property    Children[Index: Integer] : TXWFAtom Read GetChild;
    Property    ChildCount               : Integer  Read GetChildCount;
    Property    FourCC                   : String   Read FFourCC;
    Property    Data                     : Pointer  Read FData;
    Property    Size                     : LongWord Read FSize;
  End;

  TXWFIDAtom = Class(TXWFAtom)
  Protected
    FID : LongWord;
    Function    GetDataSize: Integer; Override;
    Procedure   AddToData(Var P: Pointer); Override;
    Procedure   ReadFromData(Var P: Pointer); Override;
  Public
    Constructor Create(AFourCC: String); Overload;
    Constructor Create(Const AtomRec: TXWFAtomRec; Stream: TStream); Overload;
    Property    ID : LongWord Read FID Write FID;
  End;

  TXWFContainer = Class(TXWFAtom)
  Protected
    FItems    : Pointer;
    FDataSize : LongWord;
    FCount    : LongWord;
    Function    GetCount: LongWord;
    Function    GetItem(Index: Integer): Pointer;
    Function    GetDataSize: Integer; Override;
    Procedure   AddToData(Var P: Pointer); Override;
    Procedure   ReadFromData(Var P: Pointer); Override;
    Function    GetHeaderSize: Integer; Dynamic;
  Public
    Constructor Create(AFourCC: String; ADataSize: LongWord); Overload;
    Constructor Create(AFourCC: String; ADataSize, ACount: LongWord; AItems: Pointer); Overload;
    Constructor Create(Const AtomRec: TXWFAtomRec; Stream: TStream); Overload;
    Destructor  Destroy; Override;
    Property    Count                 : LongWord Read GetCount;
    Property    Items[Index: Integer] : Pointer  Read GetItem;
    Property    DataSize              : LongWord Read FDataSize;
  End;

  TXWFGroupContainer = Class(TXWFContainer)
  Protected
    FGroupID  : LongWord;
    Procedure   AddToData(Var P: Pointer); Override;
    Procedure   ReadFromData(Var P: Pointer); Override;
    Function    GetHeaderSize: Integer; Override;
  Public
    Constructor Create(AFourCC: String; AID, ADataSize: LongWord); Overload;
    Constructor Create(AFourCC: String; AID, ADataSize, ACount: LongWord; AItems: Pointer); Overload;
    Constructor Create(Const AtomRec: TXWFAtomRec; Stream: TStream); Overload;
    Property    GroupID : LongWord Read FGroupID;
  End;

  TXWFHashAtom = Class(TXWFIDAtom)
  Protected
    FKeysAndValues : TStringList;
    Function    GetCount: Integer;
    Function    GetKey(Index: Integer): String;
    Function    GetValue(Index: Integer): String;
    Function    GetDataSize: Integer; Override;
    Procedure   AddToData(Var P: Pointer); Override;
    Procedure   ReadFromData(Var P: Pointer); Override;
  Public
    Constructor Create(AFourCC: String); Overload;
    Constructor Create(Const AtomRec: TXWFAtomRec; Stream: TStream); Overload;
    Destructor  Destroy; Override;
    Procedure   Add(Key,Value: String);
    Function    Get(Key: String): String;
    Property    Count                  : Integer  Read GetCount;
    Property    Keys[Index: Integer]   : String   Read GetKey;
    Property    Values[Index: Integer] : String   Read GetValue;
  End;

  TXWFOctreeNode = Class(TXWFAtom)
  Protected
    Function    GetDataSize: Integer; Override;
    Procedure   AddToData(Var P: Pointer); Override;
    Procedure   ReadFromData(Var P: Pointer); Override;
  Public
    Center  : Array[0..2] Of Double;
    BoxSize : Array[0..2] Of Double; // Really half-size
    Constructor Create; Overload;
    Constructor Create(Const AtomRec: TXWFAtomRec; Stream: TStream); Overload;
  End;

  TXWFBSPTreeNode = Class(TXWFHashAtom)
  Protected
    Function    GetDataSize: Integer; Override;
    Procedure   AddToData(Var P: Pointer); Override;
    Procedure   ReadFromData(Var P: Pointer); Override;
  Public
    Normal   : Array[0..2] Of Double;
    Distance : Double;
    Center   : Array[0..2] Of Double;
    Radius   : Double;
    Constructor Create; Overload;
    Constructor Create(Const AtomRec: TXWFAtomRec; Stream: TStream); Overload;
  End;

  TXWFBoneAtom = Class(TXWFHashAtom)
  Protected
    Function    GetDataSize: Integer; Override;
    Procedure   AddToData(Var P: Pointer); Override;
    Procedure   ReadFromData(Var P: Pointer); Override;
  Public
    Frames      : TList; // List of TXWFBoneFrame
    MSPerFrame  : Integer;
    Constructor Create; Overload;
    Constructor Create(Const AtomRec: TXWFAtomRec; Stream: TStream); Overload;
    Destructor  Destroy; Override;
    Procedure   AddFrame(Const BoneRec: TXWFBoneFrameRec);
  End;

Implementation

Uses Math,SysUtils,WinSock;

// ------------------------------
// Utility routines
// ------------------------------

Function StrLPas(P: PChar; MaxLen: Integer): String;
Var
  I  : Integer;
  St : String;

Begin
  I  := Min(MaxLen,StrLen(P));
  St := '';
  While Length(St) < I Do St := St + ' ';
  Move(P^,St[1],I);
  Result := St;
End; // StrLPas

Function GetToken(Delim: String; Var St: String): String;
Var I: Integer;
Begin
  If St <> '' Then
  Begin
    // Special handling for strings enclosed in double quotes

    If St[1] = '"' Then
    Begin
      St     := Copy(St,2,Length(St));
      Result := GetToken('"',St);
      GetToken(Delim,St);
    End
    Else
    Begin
      I := Pos(Delim,St);
      If I <> 0 Then
      Begin
        Result := Trim(Copy(St,1,I - 1));
        St     := Trim(Copy(St,I + 1,Length(St)));
      End
      Else
      Begin
        Result := Trim(St);
        St     := '';
      End;
    End;
  End
  Else Result := '';
End; // GetToken

// ------------------------------
// TXWFAtom
// ------------------------------

Constructor TXWFAtom.Create;
Begin
  FFourCC        := '';
  FChildren      := TList.Create;
  FSize          := 0;
  FData          := Nil;
End; // TXWFAtom.Create

Constructor TXWFAtom.Create(AFourCC: String);
Begin
  FFourCC        := AFourCC;
  FChildren      := TList.Create;
  FSize          := 0;
  FData          := Nil;
End; // TXWFAtom.Create

Constructor TXWFAtom.Create(Const AtomRec: TXWFAtomRec; Stream: TStream);
Begin
  FAtom     := AtomRec;
  FFourCC   := StrLPas(FAtom.FourCC,4);
  FChildren := TList.Create;
  FSize     := htonl(FAtom.Size);
  If FSize > 0 Then GetMem(FData,FSize) Else FData := Nil;
  ReadFromStream(Stream);
End; // TXWFAtom.Create

Constructor TXWFAtom.Create(Stream: TStream);
Begin
  Stream.Read(FAtom,SizeOf(FAtom));
  FFourCC   := StrLPas(FAtom.FourCC,4);
  FChildren := TList.Create;
  FSize     := htonl(FAtom.Size);
  If FSize > 0 Then GetMem(FData,FSize) Else FData := Nil;
  ReadFromStream(Stream);
End; // TXWFAtom.Create

Destructor TXWFAtom.Destroy;
Var I: Integer;
Begin
  For I := 0 To FChildren.Count - 1 Do TObject(FChildren.Items[I]).Free;
  FChildren.Free;
  If FData <> Nil Then FreeMem(FData,FSize);
End; // TXWFAtom.Destroy

Procedure TXWFAtom.ReadFromStream(Stream: TStream);
Var
  ChildAtom : TXWFAtomRec;
  Child     : TXWFAtom;
  I         : Integer;
  St        : String;

Begin
  If FSize > 0 Then Stream.Read(FData^,FSize);
  UnpackData;
  For I := 1 To htonl(FAtom.Children) Do
  Begin
    Stream.Read(ChildAtom,SizeOf(ChildAtom));
    St := StrLPas(ChildAtom.FourCC,4);
         If St = fourccTexture         Then Child := TXWFHashAtom.Create(ChildAtom,Stream)
    Else If St = fourccVertexGroup     Then Child := TXWFGroupContainer.Create(ChildAtom,Stream)
    Else If St = fourccPolygonGroup    Then Child := TXWFGroupContainer.Create(ChildAtom,Stream)
    Else If St = fourccOctreeNode      Then Child := TXWFOctreeNode.Create(ChildAtom,Stream)
    Else If St = fourccBSPTreeNode     Then Child := TXWFBSPTreeNode.Create(ChildAtom,Stream)
    Else If St = fourccObject          Then Child := TXWFIDAtom.Create(ChildAtom,Stream)
    Else If St = fourccObjectPlacement Then Child := TXWFContainer.Create(ChildAtom,Stream)
    Else If St = fourccWorld           Then Child := TXWFAtom.Create(ChildAtom,Stream)
    Else If St = fourccNames           Then Child := TXWFHashAtom.Create(ChildAtom,Stream)
    Else If St = fourccBone            Then Child := TXWFBoneAtom.Create(ChildAtom,Stream)
    Else If St = fourccCreature        Then Child := TXWFHashAtom.Create(ChildAtom,Stream)
    Else If St = fourccVertBoneGroup   Then Child := TXWFGroupContainer.Create(ChildAtom,Stream)
    Else If St = fourccAnimation       Then Child := TXWFHashAtom.Create(ChildAtom,Stream)
    Else If St = fourccLights          Then Child := TXWFContainer.Create(ChildAtom,Stream)
    Else If St = fourccVertexColors    Then Child := TXWFGroupContainer.Create(ChildAtom,Stream)
    Else  Child := TXWFAtom.Create(ChildAtom,Stream);
    FChildren.Add(Child);
  End; // For I
End; // TXWFAtom.ReadFromStream

Procedure TXWFAtom.WriteToStream(Stream: TStream);
Var I: Integer;
Begin
  BuildData;
  FAtom.Children := htonl(FChildren.Count);
  FAtom.Size     := htonl(FSize);
  Stream.Write(FAtom,SizeOf(FAtom));
  If FSize > 0 Then Stream.Write(FData^,FSize);
  For I := 0 To FChildren.Count - 1 Do TXWFAtom(FChildren.Items[I]).WriteToStream(Stream);
End; // TXWFAtom.WriteToStream

Function TXWFAtom.GetDataSize: Integer;
Begin
  Result := 0;
End; // TXWFAtom.GetDataSize

Procedure TXWFAtom.AddToData(Var P: Pointer);
Begin
  // The base version does nothing
End; // TXWFAtom.AddToData

Procedure TXWFAtom.ReadFromData(Var P: Pointer);
Begin
  // The base version does nothing
End; // TXWFAtom.ReadFromData

Procedure TXWFAtom.UnpackData;
Var P: Pointer;
Begin
  P := FData;
  ReadFromData(P);
  FreeMem(FData);
  FData := Nil;
  FSize := 0;
End; // TXWFAtom.UnpackData

Function TXWFAtom.GetString(P: Pointer): String;
Var Size: Word;
Begin
  Move(P^,Size,2);
  Size := Word(htons(Size));
  Inc(LongWord(P),2);
  Result := StrLPas(P,Size);
End; // TXWFAtom.GetString

Procedure TXWFAtom.WriteString(St: String; P: Pointer);
Var Size: Word;
Begin
  Size := Word(htons(Length(St)));
  Move(Size,P^,2);
  Inc(LongWord(P),2);
  If St <> '' Then Move(St[1],P^,Length(St));
End; // TXWFAtom.WriteString

Function TXWFAtom.ReadString(Var P: Pointer): String;
Var
  Size : Word;
  St   : String;

Begin
  Move(P^,Size,SizeOf(Size));
  Size := htons(Size);
  SetLength(St,Size);
  Inc(LongWord(P),2);
  Move(P^,St[1],Size);
  Inc(LongWord(P),Size);
  Result := St;
End; // TXWFAtom.ReadString

Function TXWFAtom.GetChildCount: Integer;
Begin
  Result := FChildren.Count;
End; // TXWFAtom.GetChildCount

Function TXWFAtom.GetChild(Index: Integer): TXWFAtom;
Begin
  If (Index >= 0) And (Index < FChildren.Count)
   Then Result := TXWFAtom(FChildren.Items[Index])
   Else Result := Nil;
End; // TXWFAtom.GetChild

Procedure TXWFAtom.AddChild(Child: TXWFAtom);
Begin
  FChildren.Add(Child);
  FAtom.Children := FChildren.Count;
End; // TXWFAtom.AddChild

Procedure TXWFAtom.SetupAtom;
Begin
  Move(FourCC[1],FAtom.FourCC[0],4);
  FAtom.Children := LongWord(htonl(FChildren.Count));
  FAtom.Size     := LongWord(htonl(FSize));
End; // TXWFAtom.SetupAtom

Procedure TXWFAtom.BuildData;
Var P: Pointer;
Begin
  If FData <> Nil Then FreeMem(FData,FSize);
  FSize := GetDataSize;
  If FSize <> 0 Then
  Begin
    GetMem(FData,FSize);
    P := FData;
    AddToData(P);
  End;
  SetupAtom;
End; // TXWFAtom.BuildData

Function TXWFAtom.FindChild(AFourCC: String): TXWFAtom;
Var
  I     : Integer;
  Found : Boolean;
  Atom  : TXWFAtom;

Begin
  I      := 0;
  Found  := False;
  Result := Nil;
  While (I < FChildren.Count) And Not Found Do
  Begin
    Atom := TXWFAtom(FChildren.Items[I]);
    If Atom.FourCC = AFourCC Then
    Begin
      Result := Atom;
      Found  := True;
    End
    Else Inc(I);
  End; // While
End; // TXWFAtom.FindChild

// ------------------------------
// TXWFIDAtom
// ------------------------------

Constructor TXWFIDAtom.Create(AFourCC: String);
Begin
  Inherited Create(AFourCC);
  FID := 0;
End; // TXWFIDAtom.Create

Constructor TXWFIDAtom.Create(Const AtomRec: TXWFAtomRec; Stream: TStream);
Begin
  FID := 0;
  Inherited Create(AtomRec,Stream);
End; // TXWFIDAtom.Create

Function TXWFIDAtom.GetDataSize: Integer;
Begin
  Result := Inherited GetDataSize + SizeOf(LongWord);
End; // TXWFIDAtom.GetDataSize

Procedure TXWFIDAtom.AddToData(Var P: Pointer);
Var I: LongWord;
Begin
  Inherited AddToData(P);
  I := LongWord(ntohl(FID));
  Move(I,P^,SizeOf(I));
  Inc(LongWord(P),SizeOf(I));
End; // TXWFIDAtom.AddToData

Procedure TXWFIDAtom.ReadFromData(Var P: Pointer);
Var I: LongWord;
Begin
  Inherited ReadFromData(P);
  Move(P^,I,SizeOf(I));
  FID := LongWord(ntohl(I));
  Inc(LongWord(P),SizeOf(I));
End; // TXWFIDAtom.ReadFromData

// ------------------------------
// TXWFContainer
// ------------------------------

Constructor TXWFContainer.Create(AFourCC: String; ADataSize: LongWord);
Begin
  Inherited Create(AFourCC);
  FItems    := Nil;
  FCount    := 0;
  FDataSize := ADataSize;
End; // TXWFContainer.Create

Constructor TXWFContainer.Create(AFourCC: String; ADataSize, ACount: LongWord; AItems: Pointer);
Begin
  Inherited Create(AFourCC);
  FDataSize  := ADataSize;
  FCount     := ACount;
  GetMem(FItems,FCount * FDataSize);
  If AItems <> Nil
   Then Move(AItems^,FItems^,FCount * FDataSize)
   Else FillChar(FItems^,FCount * FDataSize,0);
End; // TXWFContainer.Create

Constructor TXWFContainer.Create(Const AtomRec: TXWFAtomRec; Stream: TStream);
Begin
  FItems    := Nil;
  FCount    := 0;
  FDataSize := 0;
  Inherited Create(AtomRec,Stream);
End; // TXWFContainer.Create

Destructor TXWFContainer.Destroy;
Begin
  If FItems <> Nil Then FreeMem(FItems,FCount * FDataSize);
  Inherited;
End; // TXWFContainer.Destroy

Function TXWFContainer.GetCount: LongWord;
Begin
  Result := FCount;
End; // TXWFContainer.GetCount

Function TXWFContainer.GetItem(Index: Integer): Pointer;
Begin
  If (Index >= 0) And (Index < FCount)
   Then Result := Pointer(LongWord(FItems) + Index * FDataSize)
   Else Result := Nil;
End; // TXWFContainer.GetItem

Function TXWFContainer.GetDataSize: Integer;
Begin
  Result := Inherited GetDataSize + GetHeaderSize + LongWord(FCount) * FDataSize;
End; // TXWFContainer.GetDataSize

Function TXWFContainer.GetHeaderSize: Integer;
Begin
  Result := SizeOf(LongWord);
End; // TXWFContainer.GetHeaderSize

Procedure TXWFContainer.AddToData(Var P: Pointer);
Begin
  Inherited AddToData(P);
  PLongWord(P)^ := LongWord(htonl(FCount));
  Inc(LongWord(P),SizeOf(LongWord));
  If FDataSize > 0 Then Move(FItems^,P^,FCount * FDataSize);
  Inc(LongWord(P),FCount * FDataSize);
End; // TXWFContainer.AddToData

Procedure TXWFContainer.ReadFromData(Var P: Pointer);
Var I: LongWord;
Begin
  Inherited ReadFromData(P);
  FCount := htonl(PLongWord(P)^);
  Inc(LongWord(P),SizeOf(LongWord));
  If FItems <> Nil Then FreeMem(FItems);
  FItems := Nil;
  If FCount > 0 Then
  Begin
    FDataSize := (FSize - GetHeaderSize) Div FCount;
    GetMem(FItems,FCount * FDataSize);
    Move(P^,FItems^,FCount * FDataSize);
    Inc(LongWord(P),FCount * FDataSize);
  End;
End; // TXWFContainer.ReadFromData

// ------------------------------
// TXWFGroupContainer
// ------------------------------

Constructor TXWFGroupContainer.Create(AFourCC: String; AID, ADataSize: LongWord);
Begin
  Inherited Create(AFourCC,ADataSize);
  FGroupID := AID;
End; // TXWFGroupContainer.Create

Constructor TXWFGroupContainer.Create(AFourCC: String; AID, ADataSize, ACount: LongWord; AItems: Pointer);
Begin
  Inherited Create(AFourCC,ADataSize,ACount,AItems);
  FGroupID := AID;
End; // TXWFGroupContainer.Create

Constructor TXWFGroupContainer.Create(Const AtomRec: TXWFAtomRec; Stream: TStream);
Begin
  FGroupID := 0;
  Inherited Create(AtomRec,Stream);
End; // TXWFGroupContainer.Create

Function TXWFGroupContainer.GetHeaderSize: Integer;
Begin
  Result := Inherited GetHeaderSize + SizeOf(LongWord);
End; // TXWFGroupContainer.GetHeaderSize

Procedure TXWFGroupContainer.AddToData(Var P: Pointer);
Begin
  PLongWord(P)^ := LongWord(htonl(FGroupID));
  Inc(LongWord(P),SizeOf(LongWord));
  Inherited AddToData(P);
End; // TXWFGroupContainer.AddToData

Procedure TXWFGroupContainer.ReadFromData(Var P: Pointer);
Begin
  FGroupID := htonl(PLongWord(P)^);
  Inc(LongWord(P),SizeOf(LongWord));
  Inherited ReadFromData(P);
End; // TXWFGroupContainer.ReadFromData

// ------------------------------
// TXWFHashAtom
// ------------------------------

Constructor TXWFHashAtom.Create(AFourCC: String);
Begin
  Inherited Create(AFourCC);
  FKeysAndValues := TStringList.Create;
End; // TXWFHashAtom.Create

Constructor TXWFHashAtom.Create(Const AtomRec: TXWFAtomRec; Stream: TStream);
Begin
  FKeysAndValues := TStringList.Create;
  Inherited Create(AtomRec,Stream);
End; // TXWFHashAtom.Create

Destructor TXWFHashAtom.Destroy;
Begin
  FKeysAndValues.Free;
  Inherited;
End; // TXWFHashAtom.Destroy

Procedure TXWFHashAtom.Add(Key,Value: String);
Var
  I     : Integer;
  Found : Boolean;
  St    : String;

Begin
  I     := 0;
  Found := False;
  While (I < FKeysAndValues.Count) And Not Found Do
  Begin
    St := FKeysAndValues.Strings[I];
    If GetToken('=',St) = Key
     Then Found := True
     Else Inc(I);
  End; // While
  If Found
   Then FKeysAndValues.Strings[I] := Key + '=' + Value
   Else FKeysAndValues.Add(Key + '=' + Value);
End; // TXWFHashAtom.Add

Function TXWFHashAtom.GetCount: Integer;
Begin
  Result := FKeysAndValues.Count;
End; // TXWFHashAtom.GetCount

Function TXWFHashAtom.GetKey(Index: Integer): String;
Var St: String;
Begin
  If (Index >= 0) And (Index < FKeysAndValues.Count) Then
  Begin
    St     := FKeysAndValues.Strings[Index];
    Result := GetToken('=',St);
  End
  Else Result := '';
End; // TXWFHashAtom.GetKey

Function TXWFHashAtom.GetValue(Index: Integer): String;
Var St: String;
Begin
  If (Index >= 0) And (Index < FKeysAndValues.Count) Then
  Begin
    St     := FKeysAndValues.Strings[Index];
    GetToken('=',St);
    Result := St;
  End
  Else Result := '';
End; // TXWFHashAtom.GetValue

Function TXWFHashAtom.Get(Key: String): String;
Var
  I     : Integer;
  Found : Boolean;
  St    : String;

Begin
  I      := 0;
  Found  := False;
  Result := '';
  While (I < FKeysAndValues.Count) And Not Found Do
  Begin
    St := FKeysAndValues.Strings[I];
    If GetToken('=',St) = Key Then
    Begin
      Result := St;
      Found  := True;
    End
    Else Inc(I);
  End; // While
End; // TXWFHashAtom.Get

Function TXWFHashAtom.GetDataSize: Integer;
Var
  I,J    : Integer;
  Keys   : Array Of String;
  Values : Array Of String;
  St     : String;

Begin
  SetLength(Keys,FKeysAndValues.Count);
  SetLength(Values,FKeysAndValues.Count);
  For I := 0 To FKeysAndValues.Count - 1 Do
  Begin
    St        := FKeysAndValues.Strings[I];
    Keys[I]   := GetToken('=',St);
    Values[I] := St;
  End; // For I
  J := 0;
  For I := 0 To High(Keys)   Do Inc(J,Length(Keys[I]) + 2);
  For I := 0 To High(Values) Do Inc(J,Length(Values[I]) + 2);
  SetLength(Keys,0);
  SetLength(Values,0);
  Result := Inherited GetDataSize + J + 4;
End; // TXWFHashAtom.GetDataSize

Procedure TXWFHashAtom.AddToData(Var P: Pointer);
Var
  I,J    : Integer;
  Keys   : Array Of String;
  Values : Array Of String;
  St     : String;

Begin
  Inherited AddToData(P);
  SetLength(Keys,FKeysAndValues.Count);
  SetLength(Values,FKeysAndValues.Count);

  For I := 0 To FKeysAndValues.Count - 1 Do
  Begin
    St        := FKeysAndValues.Strings[I];
    Keys[I]   := GetToken('=',St);
    Values[I] := St;
  End; // For I
  J := 0;
  For I := 0 To High(Keys)   Do Inc(J,Length(Keys[I]) + 2);
  For I := 0 To High(Values) Do Inc(J,Length(Values[I]) + 2);
  I := ntohl(J);
  Move(I,P^,SizeOf(I));
  Inc(LongWord(P),SizeOf(I));
  For I := 0 To High(Keys) Do
  Begin
    WriteString(Keys[I],P);
    Inc(LongWord(P),Length(Keys[I]) + 2);

    WriteString(Values[I],P);
    Inc(LongWord(P),Length(Values[I]) + 2);
  End; // For I
  SetLength(Keys,0);
  SetLength(Values,0);
End; // TXWFHashAtom.AddToData

Procedure TXWFHashAtom.ReadFromData(Var P: Pointer);
Var
  I     : Integer;
  Key   : String;
  Value : String;

Begin
  Inherited ReadFromData(P);
  Move(P^,I,SizeOf(I));
  Inc(LongWord(P),SizeOf(I));
  I := ntohl(I);
  While I > 0 Do
  Begin
    Key   := ReadString(P);
    Value := ReadString(P);
    Add(Key,Value);
    Dec(I,Length(Key) + Length(Value) + 4);
  End; // While
End; // TXWFHashAtom.ReadFromData

// ------------------------------
// TXWFOctreeNode
// ------------------------------

Constructor TXWFOctreeNode.Create;
Begin
  Inherited Create(fourccOctreeNode);
  Center[0]  := 0;
  Center[1]  := 0;
  Center[2]  := 0;
  BoxSize[0] := 0;
  BoxSize[1] := 0;
  BoxSize[2] := 0;
End; // TXWFOctreeNode.Create

Constructor TXWFOctreeNode.Create(Const AtomRec: TXWFAtomRec; Stream: TStream);
Begin
  Center[0]  := 0;
  Center[1]  := 0;
  Center[2]  := 0;
  BoxSize[0] := 0;
  BoxSize[1] := 0;
  BoxSize[2] := 0;
  Inherited Create(AtomRec,Stream);
End; // TXWFOctreeNode.Create

Function TXWFOctreeNode.GetDataSize: Integer;
Begin
  Result := Inherited GetDataSize + 6 * SizeOf(Double);
End; // TXWFOctreeNode.GetDataSize

Procedure TXWFOctreeNode.AddToData(Var P: Pointer);
Var I: Integer;
Begin
  Inherited AddToData(P);
  For I := 0 To 2 Do
  Begin
    Move(Center[I],P^,SizeOf(Center[I]));
    Inc(LongWord(P),SizeOf(Center[I]));
  End; // For I
  For I := 0 To 2 Do
  Begin
    Move(BoxSize[I],P^,SizeOf(BoxSize[I]));
    Inc(LongWord(P),SizeOf(BoxSize[I]));
  End; // For I
End; // TXWFOctreeNode.AddToData

Procedure TXWFOctreeNode.ReadFromData(Var P: Pointer);
Var I: Integer;
Begin
  Inherited ReadFromData(P);
  For I := 0 To 2 Do
  Begin
    Move(P^,Center[I],SizeOf(Center[I]));
    Inc(LongWord(P),SizeOf(Center[I]));
  End; // For I
  For I := 0 To 2 Do
  Begin
    Move(P^,BoxSize[I],SizeOf(BoxSize[I]));
    Inc(LongWord(P),SizeOf(BoxSize[I]));
  End; // For I
End; // TXWFOctreeNode.ReadFromData

// ------------------------------
// TXWFBSPTreeNode
// ------------------------------

Constructor TXWFBSPTreeNode.Create;
Begin
  Inherited Create(fourccBSPTreeNode);
  Normal[0]  := 0;
  Normal[1]  := 0;
  Normal[2]  := 0;
  Distance   := 0;
  Center[0]  := 0;
  Center[1]  := 0;
  Center[2]  := 0;
  Radius     := 0;
End; // TXWFBSPTreeNode.Create

Constructor TXWFBSPTreeNode.Create(Const AtomRec: TXWFAtomRec; Stream: TStream);
Begin
  Normal[0]  := 0;
  Normal[1]  := 0;
  Normal[2]  := 0;
  Distance   := 0;
  Center[0]  := 0;
  Center[1]  := 0;
  Center[2]  := 0;
  Radius     := 0;
  Inherited Create(AtomRec,Stream);
End; // TXWFBSPTreeNode.Create

Function TXWFBSPTreeNode.GetDataSize: Integer;
Begin
  Result := Inherited GetDataSize + 8 * SizeOf(Double);
End; // TXWFBSPTreeNode.GetDataSize

Procedure TXWFBSPTreeNode.AddToData(Var P: Pointer);
Var I: Integer;
Begin
  Inherited AddToData(P);
  For I := 0 To 2 Do
  Begin
    Move(Normal[I],P^,SizeOf(Normal[I]));
    Inc(LongWord(P),SizeOf(Normal[I]));
  End; // For I
  Move(Distance,P^,SizeOf(Distance));
  Inc(LongWord(P),SizeOf(Distance));
  For I := 0 To 2 Do
  Begin
    Move(Center[I],P^,SizeOf(Center[I]));
    Inc(LongWord(P),SizeOf(Center[I]));
  End; // For I
  Move(Radius,P^,SizeOf(Radius));
  Inc(LongWord(P),SizeOf(Radius));
End; // TXWFBSPTreeNode.AddToData

Procedure TXWFBSPTreeNode.ReadFromData(Var P: Pointer);
Var I: Integer;
Begin
  Inherited ReadFromData(P);
  For I := 0 To 2 Do
  Begin
    Move(P^,Normal[I],SizeOf(Normal[I]));
    Inc(LongWord(P),SizeOf(Normal[I]));
  End; // For I
  Move(P^,Distance,SizeOf(Distance));
  Inc(LongWord(P),SizeOf(Distance));
  For I := 0 To 2 Do
  Begin
    Move(P^,Center[I],SizeOf(Center[I]));
    Inc(LongWord(P),SizeOf(Center[I]));
  End; // For I
  Move(P^,Radius,SizeOf(Radius));
  Inc(LongWord(P),SizeOf(Radius));
End; // TXWFBSPTreeNode.ReadFromData

// ------------------------------
// TXWFBoneAtom
// ------------------------------

Constructor TXWFBoneAtom.Create;
Begin
  Frames     := TList.Create;
  MSPerFrame := 100;
  Inherited Create(fourccBone);
End; // TXWFBoneAtom.Create

Constructor TXWFBoneAtom.Create(Const AtomRec: TXWFAtomRec; Stream: TStream);
Begin
  Frames     := TList.Create;
  MSPerFrame := 100;
  Inherited Create(AtomRec,Stream);
End; // TXWFBoneAtom.Create

Destructor TXWFBoneAtom.Destroy;
Var I: Integer;
Begin
  For I := 0 To Frames.Count - 1 Do FreeMem(Frames.Items[I]);
  Frames.Free;
  Inherited;
End; // TXWFBoneAtom.Destroy

Procedure TXWFBoneAtom.AddFrame(Const BoneRec: TXWFBoneFrameRec);
Var P: PXWFBoneFrameRec;
Begin
  GetMem(P,SizeOf(TXWFBoneFrameRec));
  Move(BoneRec,P^,SizeOf(TXWFBoneFrameRec));
  Frames.Add(P);
End; // TXWFBoneAtom.AddFrame

Function TXWFBoneAtom.GetDataSize: Integer;
Begin
  Result := Inherited GetDataSize + 2 * SizeOf(Integer) + Frames.Count * SizeOf(TXWFBoneFrameRec);
End; // TXWFBoneAtom.GetDataSize

Procedure TXWFBoneAtom.AddToData(Var P: Pointer);
Var I: Integer;
Begin
  Inherited AddToData(P);
  
  I := htonl(MSPerFrame);
  Move(I,P^,SizeOf(I));
  Inc(LongWord(P),SizeOf(I));

  I := htonl(Frames.Count);
  Move(I,P^,SizeOf(I));
  Inc(LongWord(P),SizeOf(I));
  For I := 0 To Frames.Count - 1 Do
  Begin
    Move(Frames.Items[I]^,P^,SizeOf(TXWFBoneFrameRec));
    Inc(LongWord(P),SizeOf(TXWFBoneFrameRec));
  End; // For I
End; // TXWFBoneAtom.AddToData

Procedure TXWFBoneAtom.ReadFromData(Var P: Pointer);
Var
  I : Integer;
  Q : PXWFBoneFrameRec;

Begin
  Inherited ReadFromData(P);
  For I := 0 To Frames.Count - 1 Do FreeMem(Frames.Items[I]);
  Frames.Clear;

  Move(P^,MSPerFrame,SizeOf(MSPerFrame));
  Inc(LongWord(P),SizeOf(MSPerFrame));
  MSPerFrame := htonl(MSPerFrame);

  Move(P^,I,SizeOf(I));
  Inc(LongWord(P),SizeOf(I));
  I := htonl(I);

  While I > 0 Do
  Begin
    GetMem(Q,SizeOf(TXWFBoneFrameRec));
    Move(P^,Q^,SizeOf(TXWFBoneFrameRec));
    Inc(LongWord(P),SizeOf(TXWFBoneFrameRec));
    Frames.Add(Q);
    Dec(I);
  End; // While
End; // TXWFBoneAtom.ReadFromData

End.
