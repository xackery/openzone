unit MyHashes;

interface

Uses Sorter,SyncObjs;

Const
  DELETE_INT = MaxInt;

Type
  TIntegerKeyEnumeration = Array Of Integer;

  TIntegerPointerKeyValuePair = Record
    Key   : Integer;
    Value : Pointer;
  End;

  TIntegerStringKeyValuePair = Record
    Key   : Integer;
    Value : String;
  End;

  TIntegerIntegerKeyValuePair = Record
    Key   : Integer;
    Value : Integer;
  End;

  TIntegerPointerEnumeration = Array Of TIntegerPointerKeyValuePair;

  TIntegerStringEnumeration = Array Of TIntegerStringKeyValuePair;

  TIntegerIntegerEnumeration = Array Of TIntegerIntegerKeyValuePair;

  TIntegerPointerHash = Class
  Protected
    FKeyValuePairs : Array Of TIntegerPointerKeyValuePair;
    FSorter        : TQuickSorter;
    FMaxIndex      : Integer;
    FMutex         : TCriticalSection;
    Function    Compare(Index0,Index1: Integer): Integer;
    Procedure   Exchange(Index0,Index1: Integer);
    Procedure   Allocate(Amount: Integer);
    Function    GetWithIndex(Key: Integer; Out GetIndex: Integer): Pointer;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Procedure   Clear;
    Function    Get(Key: Integer): Pointer;
    Procedure   Put(Key: Integer; Value: Pointer);
    Procedure   GetKeys(Var Keys: TIntegerKeyEnumeration);
    Procedure   GetKeysAndValues(Var KeysAndValues: TIntegerPointerEnumeration);
    Procedure   Lock;
    Procedure   Unlock;
  End;

  TIntegerStringHash = Class
  Protected
    FKeyValuePairs : Array Of TIntegerStringKeyValuePair;
    FSorter        : TQuickSorter;
    FMaxIndex      : Integer;
    FMutex         : TCriticalSection;
    Function    Compare(Index0,Index1: Integer): Integer;
    Procedure   Exchange(Index0,Index1: Integer);
    Procedure   Allocate(Amount: Integer);
    Function    GetWithIndex(Key: Integer; Out GetIndex: Integer): String;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Procedure   Clear;
    Function    Get(Key: Integer): String;
    Procedure   Put(Key: Integer; Value: String);
    Procedure   GetKeys(Var Keys: TIntegerKeyEnumeration);
    Procedure   GetKeysAndValues(Var KeysAndValues: TIntegerStringEnumeration);
  End;

  TIntegerIntegerHash = Class
  Protected
    FKeyValuePairs : Array Of TIntegerIntegerKeyValuePair;
    FSorter        : TQuickSorter;
    FMaxIndex      : Integer;
    FMutex         : TCriticalSection;
    Function    Compare(Index0,Index1: Integer): Integer;
    Procedure   Exchange(Index0,Index1: Integer);
    Procedure   Allocate(Amount: Integer);
    Function    GetWithIndex(Key: Integer; Out GetIndex: Integer): Integer;
  Public
    Constructor Create(ThreadSafe: Boolean);
    Destructor  Destroy; Override;
    Procedure   Clear;
    Function    Get(Key: Integer): Integer;
    Procedure   Put(Key,Value: Integer);
    Procedure   GetKeys(Var Keys: TIntegerKeyEnumeration);
    Procedure   GetKeysAndValues(Var KeysAndValues: TIntegerIntegerEnumeration);
    Procedure   Lock;
    Procedure   Unlock;
  End;

implementation

// --------------------------------
// TIntegerPointerHash
// --------------------------------

Constructor TIntegerPointerHash.Create(ThreadSafe: Boolean);
Begin
  FMaxIndex := -1;
  SetLength(FKeyValuePairs,16);
  FSorter                := TQuickSorter.Create;
  FSorter.CompareMethod  := Compare;
  FSorter.ExchangeMethod := Exchange;
  If ThreadSafe
   Then FMutex := TCriticalSection.Create
   Else FMutex := Nil;
End; // TIntegerPointerHash.Create

Destructor TIntegerPointerHash.Destroy;
Begin
  SetLength(FKeyValuePairs,0);
  FSorter.Free;
  FMutex.Free;
End; // TIntegerPointerHash.Destroy

Function TIntegerPointerHash.GetWithIndex(Key: Integer; Out GetIndex: Integer): Pointer;
Var
  Index    : Integer;
  Step     : Integer;
  Found    : Boolean;
  Done     : Boolean;

Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    If FMaxIndex >= 0 Then
    Begin
      Index    := 0;
      Step     := FMaxIndex;
      If FMaxIndex = 0 Then
      Begin
        Found := (FKeyValuePairs[0].Key = Key);
        Done  := Found;
      End
      Else
      Begin
        Found := False;
        Done  := False;
      End;
      While (Step <> 0) And Not (Found Or Done) Do
      Begin
        If Key < FKeyValuePairs[Index].Key Then
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
        Else If Key > FKeyValuePairs[Index].Key Then
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
        Result   := FKeyValuePairs[Index].Value;
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
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerPointerHash.GetWithIndex

Function TIntegerPointerHash.Get(Key: Integer): Pointer;
Var GetIndex: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    Result := GetWithIndex(Key,GetIndex);
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerPointerHash.Get

Procedure TIntegerPointerHash.Put(Key: Integer; Value: Pointer);
Var I,GetIndex: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    GetWithIndex(Key,GetIndex);
    If Value = Nil Then
    Begin
      // Putting Nil into an existing slot deletes the slot (the array is already sorted so
      // we don't have to re-sort)

      If GetIndex >= 0 Then
      Begin
        For I := GetIndex To FMaxIndex - 1 Do FKeyValuePairs[I] := FKeyValuePairs[I + 1];
        Dec(FMaxIndex);
      End;
    End
    Else
    Begin
      If GetIndex >= 0 Then FKeyValuePairs[GetIndex].Value := Value
      Else
      Begin
        Allocate(1);
        FKeyValuePairs[FMaxIndex].Key   := Key;
        FKeyValuePairs[FMaxIndex].Value := Value;
      End;
      FSorter.Sort(0,FMaxIndex);
    End;
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerPointerHash.Put

Procedure TIntegerPointerHash.GetKeys(Var Keys: TIntegerKeyEnumeration);
Var I: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    SetLength(Keys,FMaxIndex + 1);
    For I := 0 To High(Keys) Do Keys[I] := FKeyValuePairs[I].Key;
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerPointerHash.GetKeys

Procedure TIntegerPointerHash.GetKeysAndValues(Var KeysAndValues: TIntegerPointerEnumeration);
Var I: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    SetLength(KeysAndValues,FMaxIndex + 1);
    For I := 0 To High(KeysAndValues) Do KeysAndValues[I] := FKeyValuePairs[I];
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerPointerHash.GetKeysAndValues

Function TIntegerPointerHash.Compare(Index0,Index1: Integer): Integer;
Begin
  Result := FKeyValuePairs[Index0].Key - FKeyValuePairs[Index1].Key;
End; // TIntegerPointerHash.Compare

Procedure TIntegerPointerHash.Exchange(Index0,Index1: Integer);
Var KVP: TIntegerPointerKeyValuePair;
Begin
  KVP                    := FKeyValuePairs[Index0];
  FKeyValuePairs[Index0] := FKeyValuePairs[Index1];
  FKeyValuePairs[Index1] := KVP;
End; // TIntegerPointerHash.Exchange

Procedure TIntegerPointerHash.Allocate(Amount: Integer);
Var I: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    If FMaxIndex + Amount > High(FKeyValuePairs) Then
    Begin
      I := High(FKeyValuePairs) + 1;
      If I < Amount Then I := Amount;
      SetLength(FKeyValuePairs,(High(FKeyValuePairs) + 1) + I);
    End;
    Inc(FMaxIndex,Amount);
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerPointerHash.Allocate

Procedure TIntegerPointerHash.Clear;
Begin
  FMaxIndex := -1;
End; // TIntegerPointerHash.Clear

Procedure TIntegerPointerHash.Lock;
Begin
  If FMutex <> Nil Then FMutex.Enter;
End; // TIntegerPointerHash.Lock

Procedure TIntegerPointerHash.Unlock;
Begin
  If FMutex <> Nil Then FMutex.Leave;
End; // TIntegerPointerHash.Unlock

// --------------------------------
// TIntegerStringHash
// --------------------------------

Constructor TIntegerStringHash.Create(ThreadSafe: Boolean);
Begin
  FMaxIndex := -1;
  SetLength(FKeyValuePairs,16);
  FSorter                := TQuickSorter.Create;
  FSorter.CompareMethod  := Compare;
  FSorter.ExchangeMethod := Exchange;
  If ThreadSafe
   Then FMutex := TCriticalSection.Create
   Else FMutex := Nil;
End; // TIntegerStringHash.Create

Destructor TIntegerStringHash.Destroy;
Begin
  SetLength(FKeyValuePairs,0);
  FSorter.Free;
  FMutex.Free;
End; // TIntegerStringHash.Destroy

Function TIntegerStringHash.GetWithIndex(Key: Integer; Out GetIndex: Integer): String;
Var
  Index    : Integer;
  Step     : Integer;
  Found    : Boolean;
  Done     : Boolean;

Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    If FMaxIndex >= 0 Then
    Begin
      Index    := 0;
      Step     := FMaxIndex;
      If FMaxIndex = 0 Then
      Begin
        Found := (FKeyValuePairs[0].Key = Key);
        Done  := Found;
      End
      Else
      Begin
        Found := False;
        Done  := False;
      End;
      While (Step <> 0) And Not (Found Or Done) Do
      Begin
        If Key < FKeyValuePairs[Index].Key Then
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
        Else If Key > FKeyValuePairs[Index].Key Then
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
        Result   := FKeyValuePairs[Index].Value;
        GetIndex := Index;
      End
      Else
      Begin
        Result   := '';
        GetIndex := -1;
      End;
    End
    Else
    Begin
      Result   := '';
      GetIndex := -1;
    End;
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerStringHash.GetWithIndex

Function TIntegerStringHash.Get(Key: Integer): String;
Var GetIndex: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    Result := GetWithIndex(Key,GetIndex);
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerStringHash.Get

Procedure TIntegerStringHash.Put(Key: Integer; Value: String);
Var I,GetIndex: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    GetWithIndex(Key,GetIndex);
    If Value = '' Then
    Begin
      // Putting an empty string into an existing slot deletes the slot (the array is already sorted so
      // we don't have to re-sort)

      If GetIndex >= 0 Then
      Begin
        For I := GetIndex To FMaxIndex - 1 Do FKeyValuePairs[I] := FKeyValuePairs[I + 1];
        Dec(FMaxIndex);
      End;
    End
    Else
    Begin
      If GetIndex >= 0 Then FKeyValuePairs[GetIndex].Value := Value
      Else
      Begin
        Allocate(1);
        FKeyValuePairs[FMaxIndex].Key   := Key;
        FKeyValuePairs[FMaxIndex].Value := Value;
      End;
      FSorter.Sort(0,FMaxIndex);
    End;
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerStringHash.Put

Procedure TIntegerStringHash.GetKeys(Var Keys: TIntegerKeyEnumeration);
Var I: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    SetLength(Keys,FMaxIndex + 1);
    For I := 0 To High(Keys) Do Keys[I] := FKeyValuePairs[I].Key;
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerStringHash.GetKeys

Procedure TIntegerStringHash.GetKeysAndValues(Var KeysAndValues: TIntegerStringEnumeration);
Var I: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    SetLength(KeysAndValues,FMaxIndex + 1);
    For I := 0 To High(KeysAndValues) Do KeysAndValues[I] := FKeyValuePairs[I];
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerStringHash.GetKeysAndValues

Function TIntegerStringHash.Compare(Index0,Index1: Integer): Integer;
Begin
  Result := FKeyValuePairs[Index0].Key - FKeyValuePairs[Index1].Key;
End; // TIntegerStringHash.Compare

Procedure TIntegerStringHash.Exchange(Index0,Index1: Integer);
Var KVP: TIntegerStringKeyValuePair;
Begin
  KVP                    := FKeyValuePairs[Index0];
  FKeyValuePairs[Index0] := FKeyValuePairs[Index1];
  FKeyValuePairs[Index1] := KVP;
End; // TIntegerStringHash.Exchange

Procedure TIntegerStringHash.Allocate(Amount: Integer);
Var I: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    If FMaxIndex + Amount > High(FKeyValuePairs) Then
    Begin
      I := High(FKeyValuePairs) + 1;
      If I < Amount Then I := Amount;
      SetLength(FKeyValuePairs,(High(FKeyValuePairs) + 1) + I);
    End;
    Inc(FMaxIndex,Amount);
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerStringHash.Allocate

Procedure TIntegerStringHash.Clear;
Begin
  FMaxIndex := -1;
End; // TIntegerStringHash.Clear

// --------------------------------
// TIntegerIntegerHash
// --------------------------------

Constructor TIntegerIntegerHash.Create(ThreadSafe: Boolean);
Begin
  FMaxIndex := -1;
  SetLength(FKeyValuePairs,16);
  FSorter                := TQuickSorter.Create;
  FSorter.CompareMethod  := Compare;
  FSorter.ExchangeMethod := Exchange;
  If ThreadSafe
   Then FMutex := TCriticalSection.Create
   Else FMutex := Nil;
End; // TIntegerIntegerHash.Create

Destructor TIntegerIntegerHash.Destroy;
Begin
  SetLength(FKeyValuePairs,0);
  FSorter.Free;
  FMutex.Free;
End; // TIntegerIntegerHash.Destroy

Function TIntegerIntegerHash.GetWithIndex(Key: Integer; Out GetIndex: Integer): Integer;
Var
  Index    : Integer;
  Step     : Integer;
  Found    : Boolean;
  Done     : Boolean;

Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    If FMaxIndex >= 0 Then
    Begin
      Index    := 0;
      Step     := FMaxIndex;
      If FMaxIndex = 0 Then
      Begin
        Found := (FKeyValuePairs[0].Key = Key);
        Done  := Found;
      End
      Else
      Begin
        Found := False;
        Done  := False;
      End;
      While (Step <> 0) And Not (Found Or Done) Do
      Begin
        If Key < FKeyValuePairs[Index].Key Then
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
        Else If Key > FKeyValuePairs[Index].Key Then
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
        Result   := FKeyValuePairs[Index].Value;
        GetIndex := Index;
      End
      Else
      Begin
        Result   := 0;
        GetIndex := -1;
      End;
    End
    Else
    Begin
      Result   := 0;
      GetIndex := -1;
    End;
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerIntegerHash.GetWithIndex

Function TIntegerIntegerHash.Get(Key: Integer): Integer;
Var GetIndex: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    Result := GetWithIndex(Key,GetIndex);
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerIntegerHash.Get

Procedure TIntegerIntegerHash.Put(Key,Value: Integer);
Var I,GetIndex: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    GetWithIndex(Key,GetIndex);
    If Value = DELETE_INT Then
    Begin
      // Putting DELETE_INT into an existing slot deletes the slot (the array is already sorted so
      // we don't have to re-sort)

      If GetIndex >= 0 Then
      Begin
        For I := GetIndex To FMaxIndex - 1 Do FKeyValuePairs[I] := FKeyValuePairs[I + 1];
        Dec(FMaxIndex);
      End;
    End
    Else
    Begin
      If GetIndex >= 0 Then FKeyValuePairs[GetIndex].Value := Value
      Else
      Begin
        Allocate(1);
        FKeyValuePairs[FMaxIndex].Key   := Key;
        FKeyValuePairs[FMaxIndex].Value := Value;
      End;
      FSorter.Sort(0,FMaxIndex);
    End;
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerIntegerHash.Put

Procedure TIntegerIntegerHash.GetKeys(Var Keys: TIntegerKeyEnumeration);
Var I: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    SetLength(Keys,FMaxIndex + 1);
    For I := 0 To High(Keys) Do Keys[I] := FKeyValuePairs[I].Key;
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerIntegerHash.GetKeys

Procedure TIntegerIntegerHash.GetKeysAndValues(Var KeysAndValues: TIntegerIntegerEnumeration);
Var I: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    SetLength(KeysAndValues,FMaxIndex + 1);
    For I := 0 To High(KeysAndValues) Do KeysAndValues[I] := FKeyValuePairs[I];
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerIntegerHash.GetKeysAndValues

Function TIntegerIntegerHash.Compare(Index0,Index1: Integer): Integer;
Begin
  Result := FKeyValuePairs[Index0].Key - FKeyValuePairs[Index1].Key;
End; // TIntegerIntegerHash.Compare

Procedure TIntegerIntegerHash.Exchange(Index0,Index1: Integer);
Var KVP: TIntegerIntegerKeyValuePair;
Begin
  KVP                    := FKeyValuePairs[Index0];
  FKeyValuePairs[Index0] := FKeyValuePairs[Index1];
  FKeyValuePairs[Index1] := KVP;
End; // TIntegerIntegerHash.Exchange

Procedure TIntegerIntegerHash.Allocate(Amount: Integer);
Var I: Integer;
Begin
  Try
    If FMutex <> Nil Then FMutex.Enter;
    If FMaxIndex + Amount > High(FKeyValuePairs) Then
    Begin
      I := High(FKeyValuePairs) + 1;
      If I < Amount Then I := Amount;
      SetLength(FKeyValuePairs,(High(FKeyValuePairs) + 1) + I);
    End;
    Inc(FMaxIndex,Amount);
  Finally
    If FMutex <> Nil Then FMutex.Leave;
  End;
End; // TIntegerIntegerHash.Allocate

Procedure TIntegerIntegerHash.Clear;
Begin
  FMaxIndex := -1;
End; // TIntegerIntegerHash.Clear

Procedure TIntegerIntegerHash.Lock;
Begin
  If FMutex <> Nil Then FMutex.Enter;
End; // TIntegerIntegerHash.Lock

Procedure TIntegerIntegerHash.Unlock;
Begin
  If FMutex <> Nil Then FMutex.Leave;
End; // TIntegerIntegerHash.Unlock

end.
