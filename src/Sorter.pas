unit Sorter;

interface

Type
  TQSCompare     = Function(Index0,Index1: Integer): Integer;
  TQSCompareObj  = Function(Index0,Index1: Integer): Integer Of Object;
  TQSExchange    = Procedure(Index0,Index1: Integer);
  TQSExchangeObj = Procedure(Index0,Index1: Integer) Of Object;
  TQuickSorter = Class
  Protected
    FCompare     : TQSCompare;
    FCompareObj  : TQSCompareObj;
    FExchange    : TQSExchange;
    FExchangeObj : TQSExchangeObj;
  Public
    Constructor Create;
    Procedure   Sort(Index0,Index1: Integer);
    Property    CompareProc     : TQSCompare     Read FCompare     Write FCompare;
    Property    CompareMethod   : TQSCompareObj  Read FCompareObj  Write FCompareObj;
    Property    ExchangeProc    : TQSExchange    Read FExchange    Write FExchange;
    Property    ExchangeMethod  : TQSExchangeObj Read FExchangeObj Write FExchangeObj;
  End;

implementation

Uses SysUtils;

Constructor TQuickSorter.Create;
Begin
  FCompare     := Nil;
  FCompareObj  := Nil;
  FExchange    := Nil;
  FExchangeObj := Nil;
End; // TQuickSorter.Create

Procedure TQuickSorter.Sort(Index0,Index1: Integer);
Var Pivot: Integer;

  Function Partition(Index0,Index1: Integer): Integer;
  Var I,J: Integer;

    Function Compare(Index0,Index1: Integer): Integer;
    Begin
      If Assigned(FCompare)
       Then Result := FCompare(Index0,Index1)
       Else Result := FCompareObj(Index0,Index1)
    End; // Compare

    Procedure Exchange(Index0,Index1: Integer);
    Begin
      If Assigned(FExchange)
       Then FExchange(Index0,Index1)
       Else FExchangeObj(Index0,Index1);
    End; // Exchange

  Begin
    I := Index0 - 1;
    J := Index1 + 1;
    While True Do
    Begin
      Repeat
        Dec(J);
      Until Compare(J,Index0) <= 0;
      Repeat
        Inc(I);
      Until Compare(I,Index0) >= 0;
      If I < J Then Exchange(I,J)
      Else
      Begin
        Partition := J;
        Exit;
      End;
    End; // While 
  End; // Partition

Begin
  If Assigned(FCompare) Or Assigned(FCompareObj) Then
  Begin
    If Assigned(FExchange) Or Assigned(FExchangeObj) Then
    Begin
      If Index0 < Index1 Then
      Begin
        Pivot := Partition(Index0,Index1);
        Sort(Index0,Pivot);
        Sort(Pivot + 1,Index1);
      End;
    End
    Else Raise Exception.Create('TQuickSorter: No exchange procedure or method has been assigned');
  End
  Else Raise Exception.Create('TQuickSorter: No compare procedure or method has been assigned');
End; // TQuickSorter.Sort

end.
