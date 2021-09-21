unit MyHashes;

interface

Uses SyncObjs;

Const
  DELETE_INT = MaxInt;

Type
  TIntegerKeyEnumeration = Array Of Integer;
  TSingleKeyEnumeration  = Array Of Single;
  TStringKeyEnumeration  = Array Of PChar;

  THandler = Procedure Of Object;

  PHashKey = ^THashKey;
  THashKey = Record
    Case Integer Of
      0: (Int : Integer);
      1: (S   : Single);
      2: (Str : PChar);
    End;

  PHashValue = ^THashValue;
  THashValue = Record
    Case Integer Of
      0: (Ptr     : Pointer);
      1: (Handler : THandler);
      2: (Str     : PChar);
      3: (Int     : Integer);
      4: (I64     : Int64);
    End;

  PKeyValuePair = ^TKeyValuePair;
  TKeyValuePair = Record
    Key   : THashKey;
    Value : THashValue;
  End;

  TEnumeration = Array Of TKeyValuePair;

  TAbstractHash = Class
  Protected
    FKeyValuePairs : Array Of TKeyValuePair;
    FMaxIndex      : Integer;
    FMutex         : TCriticalSection;
    Function    GetWithIndex(Key: PHashKey; Out GetIndex: Integer): PHashValue; Dynamic; Abstract;
    Function    IsDeleteValue(Value: PHashValue): Boolean; Dynamic; Abstract;
    Procedure   Allocate(Amount: Integer);
    Procedure   DeallocateKey(Index: Integer); Dynamic;
    Procedure   DeallocateValue(Index: Integer); Dynamic;
    Procedure   DeallocateItem(Index: Integer);
    Function    GetLowest: PKeyValuePair;
    Function    GetHighest: PKeyValuePair;
    Function    GetItem(Index: Integer): PKeyValuePair;
    Function    GetCount: Integer;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Function    InternalGet(Key: PHashKey): PHashValue;
    Procedure   InternalPut(Key: PHashKey; Value: PHashValue);
    Function    InternalIndexOf(Key: PHashKey): Integer;
    Function    InternalContainsKey(Key: PHashKey): Boolean;
    Procedure   InternalDelete(Key: PHashKey);
    Procedure   DeleteIndex(Index: Integer);
    Function    GetInsertionPoint(Key: PHashKey): Integer; Dynamic; Abstract;
    Procedure   Clear; Dynamic;
    Procedure   GetKeysAndValues(Var KeysAndValues: TEnumeration);
    Procedure   Lock;
    Procedure   Unlock;
    Property    Count                 : Integer       Read GetCount;
    Property    LowestItem            : PKeyValuePair Read GetLowest;
    Property    HighestItem           : PKeyValuePair Read GetLowest;
    Property    Items[Index: Integer] : PKeyValuePair Read GetItem;
  End;

  TIntegerHash = Class(TAbstractHash)
  Protected
    Function    GetWithIndex(Key: PHashKey; Out GetIndex: Integer): PHashValue; Override;
    Function    GetInsertionPoint(Key: PHashKey): Integer; Override;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Procedure   GetKeys(Var Keys: TIntegerKeyEnumeration);
    Procedure   Delete(Key: Integer);
    Function    ContainsKey(Key: Integer): Boolean;
    Function    IndexOf(Key: Integer): Integer;
  End;

  TSingleHash = Class(TAbstractHash)
  Protected
    Function    GetWithIndex(Key: PHashKey; Out GetIndex: Integer): PHashValue; Override;
    Function    GetInsertionPoint(Key: PHashKey): Integer; Override;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Procedure   GetKeys(Var Keys: TSingleKeyEnumeration);
    Procedure   Delete(Key: Single);
    Function    ContainsKey(Key: Single): Boolean;
    Function    IndexOf(Key: Single): Integer;
  End;

  TStringHash = Class(TAbstractHash)
  Protected
    Function    GetWithIndex(Key: PHashKey; Out GetIndex: Integer): PHashValue; Override;
    Function    GetInsertionPoint(Key: PHashKey): Integer; Override;
    Procedure   DeallocateKey(Index: Integer); Override;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Procedure   GetKeys(Var Keys: TStringKeyEnumeration);
    Procedure   Delete(Key: String);
    Function    ContainsKey(Key: String): Boolean;
    Function    IndexOf(Key: String): Integer;
  End;

  TIntegerPointerHash = Class(TIntegerHash)
  Protected
    Function    IsDeleteValue(Value: PHashValue): Boolean; Override;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Function    Get(Key: Integer): Pointer;
    Procedure   Put(Key: Integer; P: Pointer);
  End;

  TIntegerHandlerHash = Class(TIntegerHash)
  Protected
    Function    IsDeleteValue(Value: PHashValue): Boolean; Override;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Function    Get(Key: Integer): THandler;
    Procedure   Put(Key: Integer; H: THandler);
  End;

  TIntegerStringHash = Class(TIntegerHash)
  Protected
    Function    IsDeleteValue(Value: PHashValue): Boolean; Override;
    Procedure   DeallocateValue(Index: Integer); Override;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Function    Get(Key: Integer): String;
    Procedure   Put(Key: Integer; St: String);
  End;

  TIntegerIntegerHash = Class(TIntegerHash)
  Protected
    Function    IsDeleteValue(Value: PHashValue): Boolean; Override;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Function    Get(Key: Integer): Integer;
    Procedure   Put(Key,I: Integer);
  End;

  TSinglePointerHash = Class(TSingleHash)
  Protected
    Function    IsDeleteValue(Value: PHashValue): Boolean; Override;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Function    Get(Key: Single): Pointer;
    Procedure   Put(Key: Single; P: Pointer);
  End;

  TSingleInt64Hash = Class(TSingleHash)
  Protected
    Function    IsDeleteValue(Value: PHashValue): Boolean; Override;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Function    Get(Key: Single): Int64;
    Procedure   Put(Key: Single; I: Int64);
  End;

  TStringStringHash = Class(TStringHash)
  Protected
    Function    IsDeleteValue(Value: PHashValue): Boolean; Override;
    Procedure   DeallocateValue(Index: Integer); Override;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Function    Get(Key: String): String;
    Procedure   Put(Key,St: String);
  End;

implementation

Uses SysUtils;

// --------------------------------
// TAbstractHash
// --------------------------------

Constructor TAbstractHash.Create(ThreadSafe: Boolean);
Begin
  FMaxIndex := -1;
  SetLength(FKeyValuePairs,16);
  If ThreadSafe
   Then FMutex := TCriticalSection.Create
   Else FMutex := Nil;
End; // TAbstractHash.Create

Destructor TAbstractHash.Destroy;
Begin
  Clear;
  SetLength(FKeyValuePairs,0);
  FMutex.Free;
End; // TAbstractHash.Destroy

Procedure TAbstractHash.Allocate(Amount: Integer);
Var I: Integer;
Begin
  Try
    Lock;
    If FMaxIndex + Amount > High(FKeyValuePairs) Then
    Begin
      I := High(FKeyValuePairs) + 1;
      If I < Amount Then I := Amount;
      SetLength(FKeyValuePairs,(High(FKeyValuePairs) + 1) + I);
    End;
    Inc(FMaxIndex,Amount);
  Finally
    Unlock;
  End;
End; // TAbstractHash.Allocate

Procedure TAbstractHash.DeallocateKey(Index: Integer);
Begin
  // Does nothing in the base class
End; // TAbstractHash.DeallocateKey

Procedure TAbstractHash.DeallocateValue(Index: Integer);
Begin
  // Does nothing in the base class
End; // TAbstractHash.DeallocateValue

Procedure TAbstractHash.DeallocateItem(Index: Integer);
Begin
  DeallocateKey(Index);
  DeallocateValue(Index);
End; // TAbstractHash.DeallocateItem

Function TAbstractHash.InternalGet(Key: PHashKey): PHashValue;
Var GetIndex: Integer;
Begin
  If Key = Nil Then Raise Exception.Create('TAbstractHash.GetWithIndex: Key is Nil');
  Try
    Lock;
    Result := GetWithIndex(Key,GetIndex);
  Finally
    Unlock;
  End;
End; // TAbstractHash.InternalGet

Function TAbstractHash.InternalIndexOf(Key: PHashKey): Integer;
Var GetIndex: Integer;
Begin
  If Key = Nil Then Raise Exception.Create('TAbstractHash.InternalContainsKey: Key is Nil');
  Try
    Lock;
    GetWithIndex(Key,GetIndex);
    Result := GetIndex;
  Finally
    Unlock;
  End;
End; // TAbstractHash.InternalIndexOf

Function TAbstractHash.InternalContainsKey(Key: PHashKey): Boolean;
Var GetIndex: Integer;
Begin
  If Key = Nil Then Raise Exception.Create('TAbstractHash.InternalContainsKey: Key is Nil');
  Try
    Lock;
    GetWithIndex(Key,GetIndex);
    Result := (GetIndex >= 0);
  Finally
    Unlock;
  End;
End; // TAbstractHash.InternalContainsKey

Procedure TAbstractHash.DeleteIndex(Index: Integer);
Var I: Integer;
Begin
  Try
    Lock;
    If (Index >= 0) And (Index <= FMaxIndex) Then
    Begin
      DeallocateItem(Index);
      For I := Index To FMaxIndex - 1 Do FKeyValuePairs[I] := FKeyValuePairs[I + 1];
      Dec(FMaxIndex);
    End;
  Finally
    Unlock;
  End;
End; // TAbstractHash.DeleteIndex

Procedure TAbstractHash.InternalPut(Key: PHashKey; Value: PHashValue);
Var I,J,GetIndex: Integer;
Begin
  If Key = Nil Then Raise Exception.Create('TAbstractHash.InternalPut: Key is Nil');
  Try
    Lock;
    GetWithIndex(Key,GetIndex);
    If (Value = Nil) Or IsDeleteValue(Value) Then
    Begin
      If GetIndex >= 0 Then
      Begin
        DeallocateItem(GetIndex);
        For I := GetIndex To FMaxIndex - 1 Do FKeyValuePairs[I] := FKeyValuePairs[I + 1];
        Dec(FMaxIndex);
      End;
    End
    Else
    Begin
      If GetIndex >= 0 Then
      Begin
        DeallocateItem(GetIndex);
        FKeyValuePairs[GetIndex].Key   := Key^;
        FKeyValuePairs[GetIndex].Value := Value^;
      End
      Else
      Begin
        I := GetInsertionPoint(Key);
        If I >= 0 Then
        Begin
          Allocate(1);
          J := FMaxIndex;
          While J > I Do
          Begin
            FKeyValuePairs[J].Key   := FKeyValuePairs[J - 1].Key;
            FKeyValuePairs[J].Value := FKeyValuePairs[J - 1].Value;
            Dec(J);
          End; // While
          FKeyValuePairs[I].Key   := Key^;
          FKeyValuePairs[I].Value := Value^;
        End
        Else
        Begin
          Allocate(1);
          FKeyValuePairs[FMaxIndex].Key   := Key^;
          FKeyValuePairs[FMaxIndex].Value := Value^;
        End;
      End;
    End;
  Finally
    Unlock;
  End;
End; // TAbstractHash.InternalPut

Procedure TAbstractHash.Clear;
Var I: Integer;
Begin
  Try
    Lock;
    For I := 0 To FMaxIndex Do DeallocateItem(I);
    FMaxIndex := -1;
  Finally
    Unlock;
  End;
End; // TAbstractHash.Clear

Procedure TAbstractHash.InternalDelete(Key: PHashKey);
Var I,GetIndex: Integer;
Begin
  If Key = Nil Then Raise Exception.Create('TAbstractHash.InternalDelete: Key is Nil');
  Try
    Lock;
    GetWithIndex(Key,GetIndex);
    If GetIndex >= 0 Then
    Begin
      DeallocateItem(GetIndex);
      For I := GetIndex To FMaxIndex - 1 Do FKeyValuePairs[I] := FKeyValuePairs[I + 1];
      Dec(FMaxIndex);
    End;
  Finally
    Unlock;
  End;
End; // TAbstractHash.InternalDelete

Procedure TAbstractHash.GetKeysAndValues(Var KeysAndValues: TEnumeration);
Var I: Integer;
Begin
  Try
    Lock;
    SetLength(KeysAndValues,FMaxIndex + 1);
    For I := 0 To High(KeysAndValues) Do KeysAndValues[I] := FKeyValuePairs[I];
  Finally
    Unlock;
  End;
End; // TAbstractHash.GetKeysAndValues

Procedure TAbstractHash.Lock;
Begin
  If FMutex <> Nil Then FMutex.Enter;
End; // TAbstractHash.Lock

Procedure TAbstractHash.Unlock;
Begin
  If FMutex <> Nil Then FMutex.Leave;
End; // TAbstractHash.Unlock

Function TAbstractHash.GetLowest: PKeyValuePair;
Begin
  Try
    Lock;
    If FMaxIndex >= 0
     Then Result := @(FKeyValuePairs[0])
     Else Result := Nil;
  Finally
    Unlock;
  End;
End; // TAbstractHash.GetLowest

Function TAbstractHash.GetHighest: PKeyValuePair;
Begin
  Try
    Lock;
    If FMaxIndex >= 0
     Then Result := @(FKeyValuePairs[FMaxIndex])
     Else Result := Nil;
  Finally
    Unlock;
  End;
End; // TAbstractHash.GetHighest

Function TAbstractHash.GetItem(Index: Integer): PKeyValuePair;
Begin
  Try
    Lock;
    If (Index >= 0) And (Index <= FMaxIndex)
     Then Result := @(FKeyValuePairs[Index])
     Else Result := Nil;
  Finally
    Unlock;
  End;
End; // TAbstractHash.GetItem

Function TAbstractHash.GetCount: Integer;
Begin
  Try
    Lock;
    Result := FMaxIndex + 1;
  Finally
    Unlock;
  End;
End; // TAbstractHash.GetCount

// --------------------------------
// TIntegerHash
// --------------------------------

Constructor TIntegerHash.Create(ThreadSafe: Boolean);
Begin
  Inherited Create(ThreadSafe);
End; // TIntegerHash.Create

Destructor TIntegerHash.Destroy;
Begin
  Inherited;
End; // TIntegerHash.Destroy

Function TIntegerHash.GetWithIndex(Key: PHashKey; Out GetIndex: Integer): PHashValue;
Var
  Index  : Integer;
  Step   : Integer;
  Found  : Boolean;
  Done   : Boolean;
  KeyInt : Integer;

Begin
  If Key = Nil Then Raise Exception.Create('TIntegerHash.GetWithIndex: Key is Nil');
  If FMaxIndex >= 0 Then
  Begin
    KeyInt   := Key.Int;
    Index    := 0;
    Step     := FMaxIndex;
    If FMaxIndex = 0 Then
    Begin
      Found := (FKeyValuePairs[0].Key.Int = KeyInt);
      Done  := Found;
    End
    Else
    Begin
      Found := False;
      Done  := False;
    End;
    While (Step <> 0) And Not (Found Or Done) Do
    Begin
      If KeyInt < FKeyValuePairs[Index].Key.Int Then
      Begin
        If Step < 0 Then
        Begin
          If Index > 0 Then
          Begin
            Index := Index + Step;
            If Index < 0 Then Index := 0;
          End
          Else Done := True; // The key we're searching for definitely isn't in the list
        End
        Else
        Begin
          Step := -Step Div 2;
          If (Step <> 0) And (Index > 0) Then
          Begin
            Inc(Index,Step);
            If Index < 0 Then Index := 0;
          End
          Else Done := True;
        End;
      End
      Else If KeyInt > FKeyValuePairs[Index].Key.Int Then
      Begin
        If Step > 0 Then
        Begin
          If Index < FMaxIndex Then
          Begin
            Index := Index + Step;
            If Index > FMaxIndex Then Index := FMaxIndex;
          End
          Else Done := True; // The key we're searching for definitely isn't in the list
        End
        Else
        Begin
          Step := -Step Div 2;
          If (Step <> 0) And (Index < FMaxIndex) Then
          Begin
            Inc(Index,Step);
            If Index > FMaxIndex Then Index := FMaxIndex;
          End
          Else Done := True;
        End;
      End
      Else Found := True;
    End; // While
    If Found Then
    Begin
      Result   := @(FKeyValuePairs[Index].Value);
      GetIndex := Index;
    End
    Else
    Begin
      Result   := Nil;
      GetIndex := -1;
    End;
  End
  Else
  Begin
    Result   := Nil;
    GetIndex := -1;
  End;
End; // TIntegerHash.GetWithIndex

Function TIntegerHash.GetInsertionPoint(Key: PHashKey): Integer;
Var
  Index  : Integer;
  Step   : Integer;
  Found  : Boolean;
  Done   : Boolean;
  KeyInt : Integer;

Begin
  If Key = Nil Then Raise Exception.Create('TIntegerHash.GetInsertionPoint: Key is Nil');
  Try
    Lock;
    Index  := -1;
    KeyInt := Key.Int;
    If FMaxIndex >= 0 Then
    Begin
      Index    := 0;
      Step     := FMaxIndex;
      If FMaxIndex = 0 Then
      Begin
        Found := (FKeyValuePairs[0].Key.Int = KeyInt);
        Done  := Found;
      End
      Else
      Begin
        Found := False;
        Done  := False;
      End;
      While (Step <> 0) And Not (Found Or Done) Do
      Begin
        If KeyInt < FKeyValuePairs[Index].Key.Int Then
        Begin
          If Step < 0 Then
          Begin
            If Index > 0 Then
            Begin
              Index := Index + Step;
              If Index < 0 Then Index := 0;
            End
            Else Done := True; // The key we're searching for definitely isn't in the list
          End
          Else
          Begin
            Step := -Step Div 2;
            If (Step <> 0) And (Index > 0) Then
            Begin
              Inc(Index,Step);
              If Index < 0 Then Index := 0;
            End
            Else Done := True;
          End;
        End
        Else If KeyInt > FKeyValuePairs[Index].Key.Int Then
        Begin
          If Step > 0 Then
          Begin
            If Index < FMaxIndex Then
            Begin
              Index := Index + Step;
              If Index > FMaxIndex Then Index := FMaxIndex;
            End
            Else Done := True; // The key we're searching for definitely isn't in the list
          End
          Else
          Begin
            Step := -Step Div 2;
            If (Step <> 0) And (Index < FMaxIndex) Then
            Begin
              Inc(Index,Step);
              If Index > FMaxIndex Then Index := FMaxIndex;
            End
            Else Done := True;
          End;
        End
        Else Found := True;
      End; // While
      If Not Found Then
      Begin
        If KeyInt > FKeyValuePairs[Index].Key.Int Then Inc(Index);
      End;
    End
    Else
    Begin
      If FMaxIndex >= 0 Then
      Begin
        If KeyInt > FKeyValuePairs[Index].Key.Int Then Inc(Index);
      End
      Else Index := 0;
    End;
    Result := Index;
  Finally
    Unlock;
  End;
End; // TIntegerHash.GetInsertionPoint

Procedure TIntegerHash.GetKeys(Var Keys: TIntegerKeyEnumeration);
Var I: Integer;
Begin
  Try
    Lock;
    SetLength(Keys,FMaxIndex + 1);
    For I := 0 To High(Keys) Do Keys[I] := FKeyValuePairs[I].Key.Int;
  Finally
    Unlock;
  End;
End; // TIntegerHash.GetKeys

Procedure TIntegerHash.Delete(Key: Integer);
Var K: THashKey;
Begin
  K.Int := Key;
  InternalDelete(@K);
End; // TIntegerHash.Delete

Function TIntegerHash.ContainsKey(Key: Integer): Boolean;
Var K: THashKey;
Begin
  K.Int  := Key;
  Result := InternalContainsKey(@K);
End; // TIntegerHash.ContainsKey

Function TIntegerHash.IndexOf(Key: Integer): Integer;
Var K: THashKey;
Begin
  K.Int  := Key;
  Result := InternalIndexOf(@K);
End; // TIntegerHash.IndexOf

// --------------------------------
// TSingleHash
// --------------------------------

Constructor TSingleHash.Create(ThreadSafe: Boolean);
Begin
  Inherited Create(ThreadSafe);
End; // TSingleHash.Create

Destructor TSingleHash.Destroy;
Begin
  Inherited;
End; // TSingleHash.Destroy

Function TSingleHash.GetWithIndex(Key: PHashKey; Out GetIndex: Integer): PHashValue;
Var
  Index  : Integer;
  Step   : Integer;
  Found  : Boolean;
  Done   : Boolean;
  KeySng : Single;

Begin
  If Key = Nil Then Raise Exception.Create('TSingleHash.GetWithIndex: Key is Nil');
  If FMaxIndex >= 0 Then
  Begin
    KeySng   := Key.S;
    Index    := 0;
    Step     := FMaxIndex;
    If FMaxIndex = 0 Then
    Begin
      Found := (FKeyValuePairs[0].Key.S = KeySng);
      Done  := Found;
    End
    Else
    Begin
      Found := False;
      Done  := False;
    End;
    While (Step <> 0) And Not (Found Or Done) Do
    Begin
      If KeySng < FKeyValuePairs[Index].Key.S Then
      Begin
        If Step < 0 Then
        Begin
          If Index > 0 Then
          Begin
            Index := Index + Step;
            If Index < 0 Then Index := 0;
          End
          Else Done := True; // The key we're searching for definitely isn't in the list
        End
        Else
        Begin
          Step := -Step Div 2;
          If (Step <> 0) And (Index > 0) Then
          Begin
            Inc(Index,Step);
            If Index < 0 Then Index := 0;
          End
          Else Done := True;
        End;
      End
      Else If KeySng > FKeyValuePairs[Index].Key.S Then
      Begin
        If Step > 0 Then
        Begin
          If Index < FMaxIndex Then
          Begin
            Index := Index + Step;
            If Index > FMaxIndex Then Index := FMaxIndex;
          End
          Else Done := True; // The key we're searching for definitely isn't in the list
        End
        Else
        Begin
          Step := -Step Div 2;
          If (Step <> 0) And (Index < FMaxIndex) Then
          Begin
            Inc(Index,Step);
            If Index > FMaxIndex Then Index := FMaxIndex;
          End
          Else Done := True;
        End;
      End
      Else Found := True;
    End; // While
    If Found Then
    Begin
      Result   := @(FKeyValuePairs[Index].Value);
      GetIndex := Index;
    End
    Else
    Begin
      Result   := Nil;
      GetIndex := -1;
    End;
  End
  Else
  Begin
    Result   := Nil;
    GetIndex := -1;
  End;
End; // TSingleHash.GetWithIndex

Function TSingleHash.GetInsertionPoint(Key: PHashKey): Integer;
Var
  Index  : Integer;
  Step   : Integer;
  Found  : Boolean;
  Done   : Boolean;
  KeySng : Single;

Begin
  If Key = Nil Then Raise Exception.Create('TSingleHash.GetInsertionPoint: Key is Nil');
  Try
    Lock;
    Index  := -1;
    KeySng := Key.S;
    If FMaxIndex >= 0 Then
    Begin
      Index    := 0;
      Step     := FMaxIndex;
      If FMaxIndex = 0 Then
      Begin
        Found := (FKeyValuePairs[0].Key.S = KeySng);
        Done  := Found;
      End
      Else
      Begin
        Found := False;
        Done  := False;
      End;
      While (Step <> 0) And Not (Found Or Done) Do
      Begin
        If KeySng < FKeyValuePairs[Index].Key.S Then
        Begin
          If Step < 0 Then
          Begin
            If Index > 0 Then
            Begin
              Index := Index + Step;
              If Index < 0 Then Index := 0;
            End
            Else Done := True; // The key we're searching for definitely isn't in the list
          End
          Else
          Begin
            Step := -Step Div 2;
            If (Step <> 0) And (Index > 0) Then
            Begin
              Inc(Index,Step);
              If Index < 0 Then Index := 0;
            End
            Else Done := True;
          End;
        End
        Else If KeySng > FKeyValuePairs[Index].Key.S Then
        Begin
          If Step > 0 Then
          Begin
            If Index < FMaxIndex Then
            Begin
              Index := Index + Step;
              If Index > FMaxIndex Then Index := FMaxIndex;
            End
            Else Done := True; // The key we're searching for definitely isn't in the list
          End
          Else
          Begin
            Step := -Step Div 2;
            If (Step <> 0) And (Index < FMaxIndex) Then
            Begin
              Inc(Index,Step);
              If Index > FMaxIndex Then Index := FMaxIndex;
            End
            Else Done := True;
          End;
        End
        Else Found := True;
      End; // While
      If Not Found Then
      Begin
        If KeySng > FKeyValuePairs[Index].Key.S Then Inc(Index);
      End;
    End
    Else
    Begin
      If FMaxIndex >= 0 Then
      Begin
        If KeySng > FKeyValuePairs[Index].Key.S Then Inc(Index);
      End
      Else Index := 0;
    End;
    Result := Index;
  Finally
    Unlock;
  End;
End; // TSingleHash.GetInsertionPoint

Procedure TSingleHash.GetKeys(Var Keys: TSingleKeyEnumeration);
Var I: Integer;
Begin
  Try
    Lock;
    SetLength(Keys,FMaxIndex + 1);
    For I := 0 To High(Keys) Do Keys[I] := FKeyValuePairs[I].Key.S;
  Finally
    Unlock;
  End;
End; // TSingleHash.GetKeys

Procedure TSingleHash.Delete(Key: Single);
Var K: THashKey;
Begin
  K.S := Key;
  InternalDelete(@K);
End; // TSingleHash.Delete

Function TSingleHash.ContainsKey(Key: Single): Boolean;
Var K: THashKey;
Begin
  K.S    := Key;
  Result := InternalContainsKey(@K);
End; // TSingleHash.ContainsKey

Function TSingleHash.IndexOf(Key: Single): Integer;
Var K: THashKey;
Begin
  K.S  := Key;
  Result := InternalIndexOf(@K);
End; // TSingleHash.IndexOf

// --------------------------------
// TStringHash
// --------------------------------

Constructor TStringHash.Create(ThreadSafe: Boolean);
Begin
  Inherited Create(ThreadSafe);
End; // TStringHash.Create

Destructor TStringHash.Destroy;
Begin
  Inherited;
End; // TStringHash.Destroy

Function TStringHash.GetWithIndex(Key: PHashKey; Out GetIndex: Integer): PHashValue;
Var
  Index   : Integer;
  Step    : Integer;
  Found   : Boolean;
  Done    : Boolean;
  KeyStr  : PChar;
  Compare : Integer;

Begin
  If Key = Nil Then Raise Exception.Create('TStringHash.GetWithIndex: Key is Nil');
  If FMaxIndex >= 0 Then
  Begin
    KeyStr   := Key.Str;
    Index    := 0;
    Step     := FMaxIndex;
    If FMaxIndex = 0 Then
    Begin
      Found := (StrComp(FKeyValuePairs[0].Key.Str,KeyStr) = 0);
      Done  := Found;
    End
    Else
    Begin
      Found := False;
      Done  := False;
    End;
    While (Step <> 0) And Not (Found Or Done) Do
    Begin
      Compare := StrComp(KeyStr,FKeyValuePairs[Index].Key.Str);
      If Compare < 0 Then
      Begin
        If Step < 0 Then
        Begin
          If Index > 0 Then
          Begin
            Index := Index + Step;
            If Index < 0 Then Index := 0;
          End
          Else Done := True; // The key we're searching for definitely isn't in the list
        End
        Else
        Begin
          Step := -Step Div 2;
          If (Step <> 0) And (Index > 0) Then
          Begin
            Inc(Index,Step);
            If Index < 0 Then Index := 0;
          End
          Else Done := True;
        End;
      End
      Else If Compare > 0 Then
      Begin
        If Step > 0 Then
        Begin
          If Index < FMaxIndex Then
          Begin
            Index := Index + Step;
            If Index > FMaxIndex Then Index := FMaxIndex;
          End
          Else Done := True; // The key we're searching for definitely isn't in the list
        End
        Else
        Begin
          Step := -Step Div 2;
          If (Step <> 0) And (Index < FMaxIndex) Then
          Begin
            Inc(Index,Step);
            If Index > FMaxIndex Then Index := FMaxIndex;
          End
          Else Done := True;
        End;
      End
      Else Found := True;
    End; // While
    If Found Then
    Begin
      Result   := @(FKeyValuePairs[Index].Value);
      GetIndex := Index;
    End
    Else
    Begin
      Result   := Nil;
      GetIndex := -1;
    End;
  End
  Else
  Begin
    Result   := Nil;
    GetIndex := -1;
  End;
End; // TStringHash.GetWithIndex

Function TStringHash.GetInsertionPoint(Key: PHashKey): Integer;
Var
  Index   : Integer;
  Step    : Integer;
  Found   : Boolean;
  Done    : Boolean;
  KeyStr  : PChar;
  Compare : Integer;

Begin
  If Key = Nil Then Raise Exception.Create('TStringHash.GetInsertionPoint: Key is Nil');
  Try
    Lock;
    Index  := -1;
    KeyStr := Key.Str;
    If FMaxIndex >= 0 Then
    Begin
      Index   := 0;
      Step    := FMaxIndex;
      Compare := StrComp(KeyStr,FKeyValuePairs[Index].Key.Str);
      If FMaxIndex = 0 Then
      Begin
        Found := (Compare = 0);
        Done  := Found;
      End
      Else
      Begin
        Found := False;
        Done  := False;
      End;
      While (Step <> 0) And Not (Found Or Done) Do
      Begin
        Compare := StrComp(KeyStr,FKeyValuePairs[Index].Key.Str);
        If Compare < 0 Then
        Begin
          If Step < 0 Then
          Begin
            If Index > 0 Then
            Begin
              Index := Index + Step;
              If Index < 0 Then Index := 0;
            End
            Else Done := True; // The key we're searching for definitely isn't in the list
          End
          Else
          Begin
            Step := -Step Div 2;
            If (Step <> 0) And (Index > 0) Then
            Begin
              Inc(Index,Step);
              If Index < 0 Then Index := 0;
            End
            Else Done := True;
          End;
        End
        Else If Compare > 0 Then
        Begin
          If Step > 0 Then
          Begin
            If Index < FMaxIndex Then
            Begin
              Index := Index + Step;
              If Index > FMaxIndex Then Index := FMaxIndex;
            End
            Else Done := True; // The key we're searching for definitely isn't in the list
          End
          Else
          Begin
            Step := -Step Div 2;
            If (Step <> 0) And (Index < FMaxIndex) Then
            Begin
              Inc(Index,Step);
              If Index > FMaxIndex Then Index := FMaxIndex;
            End
            Else Done := True;
          End;
        End
        Else Found := True;
      End; // While
      If Not Found Then
      Begin
        If Compare > 0 Then Inc(Index);
      End;
    End
    Else
    Begin
      If FMaxIndex >= 0 Then
      Begin
        If Compare > 0 Then Inc(Index);
      End
      Else Index := 0;
    End;
    Result := Index;
  Finally
    Unlock;
  End;
End; // TStringHash.GetInsertionPoint

Procedure TStringHash.GetKeys(Var Keys: TStringKeyEnumeration);
Var I: Integer;
Begin
  Try
    Lock;
    SetLength(Keys,FMaxIndex + 1);
    For I := 0 To High(Keys) Do Keys[I] := FKeyValuePairs[I].Key.Str;
  Finally
    Unlock;
  End;
End; // TStringHash.GetKeys

Procedure TStringHash.Delete(Key: String);
Var K: THashKey;
Begin
  GetMem(K.Str,Length(Key) + 1);
  StrPCopy(K.Str,Key);
  InternalDelete(@K);
  FreeMem(K.Str);
End; // TStringHash.Delete

Function TStringHash.ContainsKey(Key: String): Boolean;
Var K: THashKey;
Begin
  GetMem(K.Str,Length(Key) + 1);
  StrPCopy(K.Str,Key);
  Result := InternalContainsKey(@K);
  FreeMem(K.Str);
End; // TStringHash.ContainsKey

Function TStringHash.IndexOf(Key: String): Integer;
Var K: THashKey;
Begin
  GetMem(K.Str,Length(Key) + 1);
  StrPCopy(K.Str,Key);
  Result := InternalIndexOf(@K);
  FreeMem(K.Str);
End; // TStringHash.IndexOf

Procedure TStringHash.DeallocateKey(Index: Integer);
Begin
  FreeMem(FKeyValuePairs[Index].Key.Str);
End; // TStringHash.DeallocateKey

// --------------------------------
// TIntegerPointerHash
// --------------------------------

Constructor TIntegerPointerHash.Create(ThreadSafe: Boolean);
Begin
  Inherited Create(ThreadSafe);
End; // TIntegerPointerHash.Create

Destructor TIntegerPointerHash.Destroy;
Begin
  Inherited;
End; // TIntegerPointerHash.Destroy

Function TIntegerPointerHash.IsDeleteValue(Value: PHashValue): Boolean;
Begin
  Result := (Value.Ptr = Nil);
End; // TIntegerPointerHash.IsDeleteValue

Function TIntegerPointerHash.Get(Key: Integer): Pointer;
Var
  K     : THashKey;
  Value : PHashValue;

Begin
  K.Int := Key;
  Value := InternalGet(@K);
  If Value <> Nil
   Then Result := Value.Ptr
   Else Result := Nil;
End; // TIntegerPointerHash.Get

Procedure TIntegerPointerHash.Put(Key: Integer; P: Pointer);
Var
  K     : THashKey;
  Value : THashValue;

Begin
  K.Int     := Key;
  Value.Ptr := P;
  InternalPut(@K,@Value);
End; // TIntegerPointerHash.Put

// --------------------------------
// TIntegerHandlerHash
// --------------------------------

Constructor TIntegerHandlerHash.Create(ThreadSafe: Boolean);
Begin
  Inherited Create(ThreadSafe);
End; // TIntegerHandlerHash.Create

Destructor TIntegerHandlerHash.Destroy;
Begin
  Inherited;
End; // TIntegerHandlerHash.Destroy

Function TIntegerHandlerHash.IsDeleteValue(Value: PHashValue): Boolean;
Var H: THandler;
Begin
  H := Value^.Handler;
  Result := (@H = Nil);
End; // TIntegerHandlerHash.IsDeleteValue

Function TIntegerHandlerHash.Get(Key: Integer): THandler;
Var
  K     : THashKey;
  Value : PHashValue;

Begin
  K.Int := Key;
  Value := InternalGet(@K);
  If Value <> Nil
   Then Result := Value.Handler
   Else Result := Nil;
End; // TIntegerHandlerHash.Get

Procedure TIntegerHandlerHash.Put(Key: Integer; H: THandler);
Var
  K     : THashKey;
  Value : THashValue;

Begin
  K.Int         := Key;
  Value.Handler := H;
  InternalPut(@K,@Value);
End; // TIntegerHandlerHash.Put

// --------------------------------
// TIntegerStringHash
// --------------------------------

Constructor TIntegerStringHash.Create(ThreadSafe: Boolean);
Begin
  Inherited Create(ThreadSafe);
End; // TIntegerStringHash.Create

Destructor TIntegerStringHash.Destroy;
Begin
  Inherited;
End; // TIntegerStringHash.Destroy

Function TIntegerStringHash.IsDeleteValue(Value: PHashValue): Boolean;
Begin
  Result := (Value.Str = Nil);
End; // TIntegerStringHash.IsDeleteValue

Procedure TIntegerStringHash.DeallocateValue(Index: Integer);
Begin
  FreeMem(FKeyValuePairs[Index].Value.Str);
End; // TIntegerStringHash.DeallocateValue

Function TIntegerStringHash.Get(Key: Integer): String;
Var
  K     : THashKey;
  Value : PHashValue;

Begin
  K.Int := Key;
  Value := InternalGet(@K);
  If Value <> Nil
   Then Result := StrPas(Value.Str)
   Else Result := '';
End; // TIntegerStringHash.Get

Procedure TIntegerStringHash.Put(Key: Integer; St: String);
Var
  K     : THashKey;
  Value : THashValue;

Begin
  K.Int := Key;
  GetMem(Value.Str,Length(St) + 1);
  StrPCopy(Value.Str,St);
  InternalPut(@K,@Value);
End; // TIntegerStringHash.Put

// --------------------------------
// TIntegerIntegerHash
// --------------------------------

Constructor TIntegerIntegerHash.Create(ThreadSafe: Boolean);
Begin
  Inherited Create(ThreadSafe);
End; // TIntegerIntegerHash.Create

Destructor TIntegerIntegerHash.Destroy;
Begin
  Inherited;
End; // TIntegerIntegerHash.Destroy

Function TIntegerIntegerHash.IsDeleteValue(Value: PHashValue): Boolean;
Begin
  Result := (Value.Int = DELETE_INT);
End; // TIntegerIntegerHash.IsDeleteValue

Function TIntegerIntegerHash.Get(Key: Integer): Integer;
Var
  K     : THashKey;
  Value : PHashValue;

Begin
  K.Int := Key;
  Value := InternalGet(@K);
  If Value <> Nil
   Then Result := Value.Int
   Else Result := 0;
End; // TIntegerIntegerHash.Get

Procedure TIntegerIntegerHash.Put(Key,I: Integer);
Var
  K     : THashKey;
  Value : THashValue;

Begin
  K.Int     := Key;
  Value.Int := I;
  InternalPut(@K,@Value);
End; // TIntegerIntegerHash.Put

// --------------------------------
// TSinglePointerHash
// --------------------------------

Constructor TSinglePointerHash.Create(ThreadSafe: Boolean);
Begin
  Inherited Create(ThreadSafe);
End; // TSinglePointerHash.Create

Destructor TSinglePointerHash.Destroy;
Begin
  Inherited;
End; // TSinglePointerHash.Destroy

Function TSinglePointerHash.IsDeleteValue(Value: PHashValue): Boolean;
Begin
  Result := (Value.Ptr = Nil);
End; // TSinglePointerHash.IsDeleteValue

Function TSinglePointerHash.Get(Key: Single): Pointer;
Var
  K     : THashKey;
  Value : PHashValue;

Begin
  K.S   := Key;
  Value := InternalGet(@K);
  If Value <> Nil
   Then Result := Value.Ptr
   Else Result := Nil;
End; // TSinglePointerHash.Get

Procedure TSinglePointerHash.Put(Key: Single; P: Pointer);
Var
  K     : THashKey;
  Value : THashValue;

Begin
  K.S       := Key;
  Value.Ptr := P;
  InternalPut(@K,@Value);
End; // TSinglePointerHash.Put

// --------------------------------
// TSingleInt64Hash
// --------------------------------

Constructor TSingleInt64Hash.Create(ThreadSafe: Boolean);
Begin
  Inherited Create(ThreadSafe);
End; // TSingleInt64Hash.Create

Destructor TSingleInt64Hash.Destroy;
Begin
  Inherited;
End; // TSingleInt64Hash.Destroy

Function TSingleInt64Hash.IsDeleteValue(Value: PHashValue): Boolean;
Begin
  Result := (Value.I64 = DELETE_INT);
End; // TSingleInt64Hash.IsDeleteValue

Function TSingleInt64Hash.Get(Key: Single): Int64;
Var
  K     : THashKey;
  Value : PHashValue;

Begin
  K.S   := Key;
  Value := InternalGet(@K);
  If Value <> Nil
   Then Result := Value.I64
   Else Result := 0;
End; // TSingleInt64Hash.Get

Procedure TSingleInt64Hash.Put(Key: Single; I: Int64);
Var
  K     : THashKey;
  Value : THashValue;

Begin
  K.S       := Key;
  Value.I64 := I;
  InternalPut(@K,@Value);
End; // TSingleInt64Hash.Put

// --------------------------------
// TStringStringHash
// --------------------------------

Constructor TStringStringHash.Create(ThreadSafe: Boolean);
Begin
  Inherited Create(ThreadSafe);
End; // TStringStringHash.Create

Destructor TStringStringHash.Destroy;
Begin
  Inherited;
End; // TStringStringHash.Destroy

Function TStringStringHash.IsDeleteValue(Value: PHashValue): Boolean;
Begin
  Result := (Value.Str = Nil);
End; // TStringStringHash.IsDeleteValue

Procedure TStringStringHash.DeallocateValue(Index: Integer);
Begin
  FreeMem(FKeyValuePairs[Index].Value.Str);
End; // TStringStringHash.DeallocateValue

Function TStringStringHash.Get(Key: String): String;
Var
  K     : THashKey;
  Value : PHashValue;

Begin
  GetMem(K.Str,Length(Key) + 1);
  StrPCopy(K.Str,Key);
  Value := InternalGet(@K);
  If Value <> Nil
   Then Result := StrPas(Value.Str)
   Else Result := '';
  FreeMem(K.Str);
End; // TStringStringHash.Get

Procedure TStringStringHash.Put(Key,St: String);
Var
  K     : THashKey;
  Value : THashValue;

Begin
  GetMem(K.Str,Length(Key) + 1);
  StrPCopy(K.Str,Key);
  GetMem(Value.Str,Length(St) + 1);
  StrPCopy(Value.Str,St);
  InternalPut(@K,@Value);
End; // TStringStringHash.Put

end.
