unit Sorter;

interface

Type
  TQSCompare  = Function(Index0,Index1: Integer): Integer Of Object;
  TQSExchange = Procedure(Index0,Index1: Integer) Of Object;
  TQSCompareProc  = Function(Index0,Index1: Integer): Integer;
  TQSExchangeProc = Procedure(Index0,Index1: Integer);
  TQuickSorter = Class
  Protected
    FCompare  : TQSCompare;
    FExchange : TQSExchange;
  Public
    Constructor Create;
    Procedure   Sort(Index0,Index1: Integer);
    Property    Compare  : TQSCompare  Read FCompare  Write FCompare;
    Property    Exchange : TQSExchange Read FExchange Write FExchange;
  End;
  TQuickSorterProc = Class
  Protected
    FCompare  : TQSCompareProc;
    FExchange : TQSExchangeProc;
  Public
    Constructor Create;
    Procedure   Sort(Index0,Index1: Integer);
    Property    Compare  : TQSCompareProc  Read FCompare  Write FCompare;
    Property    Exchange : TQSExchangeProc Read FExchange Write FExchange;
  End;

implementation

Uses SysUtils;

// --------------------------
// TQuickSorter
// --------------------------

Constructor TQuickSorter.Create;
Begin
  FCompare  := Nil;
  FExchange := Nil;
End; // TQuickSorter.Create

Procedure TQuickSorter.Sort(Index0,Index1: Integer);
Var Pivot: Integer;

  Function Partition(Index0,Index1: Integer): Integer;
  Var I,J: Integer;
  Begin
    I := Index0 - 1;
    J := Index1 + 1;
    While True Do
    Begin
      Repeat
        Dec(J);
      Until FCompare(J,Index0) <= 0;
      Repeat
        Inc(I);
      Until FCompare(I,Index0) >= 0;
      If I < J Then FExchange(I,J)
      Else
      Begin
        Partition := J;
        Exit;
      End;
    End; // While
  End; // Partition

Begin
  If Index0 < Index1 Then
  Begin
    Pivot := Partition(Index0,Index1);
    Sort(Index0,Pivot);
    Sort(Pivot + 1,Index1);
  End;
End; // TQuickSorter.Sort

// --------------------------
// TQuickSorterProc
// --------------------------

Constructor TQuickSorterProc.Create;
Begin
  FCompare  := Nil;
  FExchange := Nil;
End; // TQuickSorterProc.Create

Procedure TQuickSorterProc.Sort(Index0,Index1: Integer);
Var Pivot: Integer;

  Function Partition(Index0,Index1: Integer): Integer;
  Var I,J: Integer;
  Begin
    I := Index0 - 1;
    J := Index1 + 1;
    While True Do
    Begin
      Repeat
        Dec(J);
      Until FCompare(J,Index0) <= 0;
      Repeat
        Inc(I);
      Until FCompare(I,Index0) >= 0;
      If I < J Then FExchange(I,J)
      Else
      Begin
        Partition := J;
        Exit;
      End;
    End; // While
  End; // Partition

Begin
  If Index0 < Index1 Then
  Begin
    Pivot := Partition(Index0,Index1);
    Sort(Index0,Pivot);
    Sort(Pivot + 1,Index1);
  End;
End; // TQuickSorterProc.Sort

end.
