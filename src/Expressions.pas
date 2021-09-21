unit Expressions;

interface

Uses Classes;

Type
  TCardinality  = (caUnary,caBinary);
  TOperatorType = (otAdd,otSubtract,otMultiply,otDivide,otModulo,otNegate,
                   otAnd,otOr,otXor,otNot,otShl,otShr,
                   otEqualTo,otUnequalTo,otLessThan,otGreaterThan,otLessOrEqual,otGreaterOrEqual,
                   otTrunc,otRound,otRoundUp,otFrac,otSgn,otAbs,otInt,otFloor,otCeil,
                   otSqrt,otSin,otCos,otTan,otExp,otLn,otLog10,otLog2,otSqr,otAngle,
                   otOpenParend);
  TOperandType  = (onInteger,onReal,onBoolean,onString,onVariable,onScript,onError);
  TVariableType = (vaPlaceX,vaPlaceY,vaPlaceZ,vaRotateX,vaRotateY,vaRotateZ,
                   vaSizeX,vaSizeY,vaSizeZ,vaPi,vaVariable,vaParameter);

  TOpKind       = (okOperator,okOperand);

  TOperandValue = Record
    Case TOperandType Of
      onInteger: (Int: Integer);
         onReal: (Ext: Extended);
      onBoolean: (Bool: Boolean);
       onString: (St: ShortString);
    End; // Case
  TOperandRec = Record
    OperandType  : TOperandType;
    Value        : TOperandValue;
    Name         : ShortString;     // Only used if OperandType is onVariable
    VariableType : TVariableType;   // Only used if OperandType is onVariable
    ReadOnly     : Boolean;         // Only used if OperandType is onVariable
    Hidden       : Boolean;         // Only used if OperandType is onVariable
  End;
  TExpressionOpData = Record
    Case TOpKind Of
      okOperator: (OperatorType : TOperatorType);
      okOperand:  (Operand      : TOperandRec);
    End; // Case
  TExpressionOp = Class
    Kind     : TOpKind;
    Data     : TExpressionOpData;
    HintText : TStringList;
    Constructor Create;
    Constructor CreateOperator(OperatorType: TOperatorType);
    Constructor CreateIntegerOperand(I: Integer);
    Constructor CreateRealOperand(E: Extended);
    Constructor CreateBooleanOperand(B: Boolean);
    Constructor CreateStringOperand(St: String);
    Constructor CreateVariableOperand(AType: TVariableType); Overload;
    Constructor CreateVariableOperand(AType: TVariableType; AName: String; AReadOnly,AHidden: Boolean); Overload;
    Destructor  Destroy; Override;
    Function    Copy: TExpressionOp;
    Function    CopyNotVariableOperand: TExpressionOp;
  End;

  TExpression = Class; // Forward declaration
  TExpressionStack = Class(TStringList)
    Procedure Push(Item: TExpressionOp);
    Function  Pop: TExpressionOp;
  End;

  TExpression = Class
    Items : TExpressionStack;
    Constructor Create(St: String);
    Destructor  Destroy; Override;
    Function    Evaluate(Variables: TStringList): TOperandRec;
  End;

implementation

Uses ZoneClasses,SysUtils,Math;

Const
  Cardinalities : Array[TOperatorType] Of TCardinality = (caBinary,caBinary,caBinary,caBinary,caBinary,caUnary,
                                                          caBinary,caBinary,caBinary,caUnary,caBinary,caBinary,
                                                          caBinary,caBinary,caBinary,caBinary,caBinary,caBinary,
                                                          caUnary,caUnary,caUnary,caUnary,caUnary,caUnary,caUnary,caUnary,caUnary,
                                                          caUnary,caUnary,caUnary,caUnary,caUnary,caUnary,caUnary,caUnary,caUnary,caBinary,
                                                          caUnary);

  VariableNames: Array[TVariableType] Of String = ('PLACEX','PLACEY','PLACEZ',
                                                   'ROTATEX','ROTATEY','ROTATEZ',
                                                   'SIZEX','SIZEY','SIZEZ','PI','','');

  OperatorNames: Array[0..48] Of String = ('+','-','*','/','%','MOD','&','&&','AND','|','||','OR',
                                           '^','^^','XOR','!','~','NOT','=','==','!=','<>','<','>',
                                           '<=','>=','TRUNC','ROUND','ROUNDUP','FRAC','SGN','ABS',
                                           'INT','FLOOR','CEIL','SQRT','SIN','COS','TAN','EXP','LN',
                                           'LOG10','LOG2','SQR','<<','SHL','>>','SHR','@');

  OperatorTypes: Array[0..48] Of TOperatorType = (otAdd,otSubtract,otMultiply,otDivide,otModulo,
                                                  otModulo,otAnd,otAnd,otAnd,otOr,otOr,otOr,otXor,
                                                  otXor,otXor,otNot,otNot,otNot,otEqualTo,otEqualTo,
                                                  otUnequalTo,otUnequalTo,otLessThan,otGreaterThan,
                                                  otLessOrEqual,otGreaterOrEqual,otTrunc,otRound,
                                                  otRoundUp,otFrac,otSgn,otAbs,otInt,otFloor,otCeil,
                                                  otSqrt,otSin,otCos,otTan,otExp,otLn,otLog10,
                                                  otLog2,otSqr,otShl,otShl,otShr,otShr,otAngle);

  Precedence : Array[TOperatorType] Of Integer = (3,3,2,2,2,1,2,3,3,1,2,2,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5);

// TExpressionStack

Procedure TExpressionStack.Push(Item: TExpressionOp);
Begin
  AddObject('',Item);
End; // TExpressionStack.Push

Function  TExpressionStack.Pop: TExpressionOp;
Begin
  If Count > 0 Then
  Begin
    Result := TExpressionOp(Objects[Count - 1]);
    Delete(Count - 1);
  End
  Else Result := Nil;
End; // TExpressionStack.Pop

// ----------------------
// TExpressionOp
// ----------------------

Constructor TExpressionOp.Create;
Begin
  HintText := TStringList.Create;
End; // TExpressionOp.Create

Constructor TExpressionOp.CreateOperator(OperatorType: TOperatorType);
Begin
  Kind := okOperator;
  Data.OperatorType := OperatorType;
  HintText := TStringList.Create;
End; // TExpressionOp.CreateOperator

Constructor TExpressionOp.CreateIntegerOperand(I: Integer);
Begin
  Kind                     := okOperand;
  Data.Operand.OperandType := onInteger;
  Data.Operand.Value.Int   := I;
  HintText := TStringList.Create;
End; // TExpressionOp.CreateIntegerOperand

Constructor TExpressionOp.CreateRealOperand(E: Extended);
Begin
  Kind                     := okOperand;
  Data.Operand.OperandType := onReal;
  Data.Operand.Value.Ext   := E;
  HintText := TStringList.Create;
End; // TExpressionOp.CreateRealOperand

Constructor TExpressionOp.CreateBooleanOperand(B: Boolean);
Begin
  Kind                     := okOperand;
  Data.Operand.OperandType := onBoolean;
  Data.Operand.Value.Bool  := B;
  HintText := TStringList.Create;
End; // TExpressionOp.CreateBooleanOperand

Constructor TExpressionOp.CreateStringOperand(St: String);
Begin
  Kind                     := okOperand;
  Data.Operand.OperandType := onString;
  Data.Operand.Value.St    := St;
  HintText := TStringList.Create;
End; // TExpressionOp.CreateStringOperand

Constructor TExpressionOp.CreateVariableOperand(AType: TVariableType);
Begin
  Kind                      := okOperand;
  Data.Operand.OperandType  := onVariable;
  Data.Operand.VariableType := AType;
  Data.Operand.Name         := '';
  Data.Operand.ReadOnly     := False;
  Data.Operand.Hidden       := False;
  HintText := TStringList.Create;
End; // TExpressionOp.CreateVariableOperand

Constructor TExpressionOp.CreateVariableOperand(AType: TVariableType; AName: String; AReadOnly,AHidden: Boolean);
Begin
  Kind                      := okOperand;
  Data.Operand.OperandType  := onVariable;
  Data.Operand.VariableType := AType;
  Data.Operand.Name         := AName;
  Data.Operand.ReadOnly     := AReadOnly;
  Data.Operand.Hidden       := AHidden;
  HintText := TStringList.Create;
End; // TExpressionOp.CreateVariableOperand

Destructor TExpressionOp.Destroy;
Begin
  HintText.Free;
  Inherited;
End; // TExpressionOp.Destroy

Function TExpressionOp.CopyNotVariableOperand: TExpressionOp;
Var Op: TExpressionOp;
Begin
  Op                          := TExpressionOp.Create;
  Op.Kind                     := Kind;
  Op.Data.OperatorType        := Data.OperatorType;
  Op.Data.Operand.OperandType := Data.Operand.OperandType;
  Op.Data.Operand.Value       := Data.Operand.Value;
  Result                      := Op;
End; // TExpressionOp.CopyNotVariableOperand

Function TExpressionOp.Copy: TExpressionOp;
Var Op: TExpressionOp;
Begin
  Op      := TExpressionOp.Create;
  Op.Kind := Kind;
  Op.Data := Data;
  Result  := Op;
End; // TExpressionOp.Copy

// TExpression

Constructor TExpression.Create(St: String);
Var
  Stack           : TExpressionStack;
  LastWasOperator : Boolean;
  LastWasOpenPar  : Boolean;
  Token           : String;
  BareToken       : String;
  Op              : TExpressionOp;
  I,J             : Integer;
  E               : Extended;
  Done            : Boolean;
  OpType          : TOperatorType;

  Function GetToken(Var St: String): String;
  Var
    I            : Integer;
    Ch,Ch1       : Char;
    Done         : Boolean;
    Token        : String;
    Number       : Boolean;
    Scientific   : Boolean;
    QuotedString : Boolean;

  Begin
    // We are guaranteed to not be on a space

    I            := 1;
    Done         := False;
    Token        := '';
    Number       := False;
    Scientific   := False;
    QuotedString := False;

    While (I <= Length(St)) And Not Done Do
    Begin
      Ch    := St[I];
      Token := Token + Ch;

      If (I = 1) And (Ch In ['0'..'9']) Then Number := True;
      If (Ch = 'E') And Number Then Scientific := True;
      If Ch = '"' Then
      Begin
        QuotedString := Not QuotedString;
        If Not QuotedString Then Done := True;
      End;

      // A minus sign is a special case, if the previous token was an operator or open parenthesis.
      // It means that this has to be a negative number.

      If Not Done Then
      Begin
        If (Token = '-') And (LastWasOperator Or LastWasOpenPar) Then Done := True
        Else
        Begin
          // If the next character is a space or if we've reached the end of the string, then we've definitely
          // reached the end of the token

          If (I = Length(St)) Or (St[I + 1] = ' ') Then Done := True
          Else
          Begin
            // Numbers in scientific notation can have a minus sign for the exponent

            If (Ch = '-') And Not (Number Or Scientific) Then Done := True
            Else
            Begin
              // What we do depends on what's next and what we have

              If Ch In ['+','*','/','%','(',')','~','@'] Then Done := True  // These operators are definitely alone
              Else
              Begin
                Ch1 := St[I + 1];

                // First stop on the valid pairs

                     If (Token = '<=') Or
                        (Token = '>=') Or
                        (Token = '==') Or
                        (Token = '&&') Or
                        (Token = '||') Or
                        (Token = '^^') Or
                        (Token = '!=') Or
                        (Token = '<>') Then Done := True

                // Now stop on valid singlets

                Else If (Ch = '=') And (Ch1 <> '=') Then Done := True                // =
                Else If (Ch = '&') And (Ch1 <> '&') Then Done := True                // &
                Else If (Ch = '|') And (Ch1 <> '|') Then Done := True                // |
                Else If (Ch = '^') And (Ch1 <> '^') Then Done := True                // ^
                Else If (Ch = '<') And Not (Ch1 In ['=','>']) Then Done := True      // <
                Else If (Ch = '>') And (Ch1 <> '=') Then Done := True                // >
                Else If (Ch = '!') And (Ch1 <> '=') Then Done := True                // !

                // Now stop if the next character definitely is the start of an operator

                Else If (Ch1 = '-') And Not Scientific Then Done := True
                Else If Ch1 In ['+','*','/','%','!','(',')','~','<','@'] Then Done := True
                Else If (Ch1 = '=') And Not (Ch In ['=','<','>','!']) Then Done := True
                Else If (Ch1 = '&') And (Ch <> '&') Then Done := True
                Else If (Ch1 = '|') And (Ch <> '|') Then Done := True
                Else If (Ch1 = '^') And (Ch <> '^') Then Done := True
                Else If (Ch1 = '>') And (Ch <> '<') Then Done := True

                // Assume anything else is part of a number, variable, or other operator

                Else Inc(I);
              End;

              // Cancel scientifc notation mode if it's time to

              If (Ch = '-') And Scientific Then
              Begin
                Number     := False;
                Scientific := False;
              End;
            End;
          End;
        End;
      End;
    End; // While

    // Finally, remove leading and trailing spaces

    St     := Trim(Copy(St,I + 1,Length(St)));
    Result := Token;
  End; // GetToken

  Function IsOperator(St: String): Integer;
  Var
    I     : Integer;
    Found : Boolean;

  Begin
    I     := 0;
    Found := False;
    While (I <= High(OperatorNames)) And Not Found Do
     If OperatorNames[I] = St Then Found := True Else Inc(I);
    If Found Then Result := I Else Result := -1;
  End; // IsOperator

  Function IsString(St: String): Boolean;
  Begin
    Result := (Length(St) > 1) And (St[1] = '"') And (St[Length(St)] = '"');
  End; // IsString

  Function IsInteger(St: String): Boolean;
  Var
    I     : Integer;
    Found : Boolean;

  Begin
    If St[1] In ['$','-','0'..'9'] Then
    Begin
      I     := 2;
      Found := False;
      While (I <= Length(St)) And Not Found Do
      Begin
        If St[1] = '$'
         Then Found := Found Or Not (St[I] In ['0'..'9','A'..'F'])
         Else Found := Found Or Not (St[I] In ['0'..'9']);
        If Not Found Then Inc(I);
      End; // While
      Result := Not Found;
    End
    Else Result := False;
  End; // IsInteger

  Function IsReal(St: String): Boolean;
  Var
    I     : Integer;
    Found : Boolean;

  Begin
    If St[1] In ['-','.','0'..'9'] Then
    Begin
      I     := 2;
      Found := False;
      While (I <= Length(St)) And Not Found Do
      Begin
        Found := Found Or Not (St[I] In ['.','-','E','0'..'9']);
        If Not Found Then Inc(I);
      End; // While
      Result := Not Found;
    End
    Else Result := False;
  End; // IsReal

  Function IsGlobalVariable(St: String): Integer;
  Var
    I     : Integer;
    Found : Boolean;

  Begin
    I     := 0;
    Found := False;
    While (TVariableType(I) <= High(VariableNames)) And Not Found Do
    Begin
      If St = VariableNames[TVariableType(I)] Then Found := True Else Inc(I);
    End; // While
    If Found Then Result := I Else Result := -1;
  End; // IsVariable

Begin
  Items := TExpressionStack.Create;

  // Before we do anything, convert all commas sitting between numbers to periods in case any Euro decimal separators were used

  For I := 2 To Length(St) - 1 Do
  Begin
    If (St[I] = ',') And (St[I - 1] In ['0'..'9']) And (St[I + 1] In ['0'..'9']) Then St[I] := '.';
  End;

  // Parse the expression string and convert to postfix

  Stack           := TExpressionStack.Create;
  St              := Trim(St);
  LastWasOperator := True;  // Default to true
  LastWasOpenPar  := False; // Default to false
  While St <> '' Do
  Begin
    BareToken := GetToken(St);
    Token     := UpperCase(BareToken);

    // If the token is empty, it means there was a problem

    If Token <> '' Then
    Begin
      // First handle parentheses

      If Token = '(' Then
      Begin
        Stack.Push(TExpressionOp.CreateOperator(otOpenParend));
        LastWasOpenPar := True;
      End
      Else If Token = ')' Then
      Begin
        LastWasOpenPar := False;
        Repeat
          Op := Stack.Pop;
          If Not ((Op <> Nil) And (Op.Kind = okOperator) And (Op.Data.OperatorType = otOpenParend)) Then Items.Push(Op);
        Until (Stack.Count = 0) Or ((Op <> Nil) And (Op.Kind = okOperator) And (Op.Data.OperatorType = otOpenParend));
        If Op = Nil Then St := '' Else Op.Free;
      End

      // Look for operators

      Else
      Begin
        // Check for the negation operator, which is a special case

        Op     := Nil;
        OpType := otAdd; // It doesn't matter, it's only to make the compiler happy
        If (Token = '-') And (LastWasOperator Or LastWasOpenPar) Then
        Begin
          OpType := otNegate;
          Op     := TExpressionOp.CreateOperator(OpType);
        End
        Else
        Begin
          // Check for other operators

          I := IsOperator(Token);
          If I >= 0 Then
          Begin
            OpType := OperatorTypes[I];
            Op     := TExpressionOp.CreateOperator(OpType);
          End;
        End;
        LastWasOpenPar := False;
        If Op <> Nil Then
        Begin
          // Check precedence of what's on the stack

          Done := False;
          While (Stack.Count > 0) And Not Done Do
          Begin
            If Precedence[TExpressionOp(Stack.Objects[Stack.Count - 1]).Data.OperatorType] <= Precedence[OpType]
             Then Items.Push(Stack.Pop)
             Else Done := True;
          End; // While
          Stack.Push(Op);
          LastWasOperator := True;
        End
        Else
        Begin
          LastWasOperator := False;

          // It must be an operand, then

               If Token = 'TRUE'  Then Items.Push(TExpressionOp.CreateBooleanOperand(True))
          Else If Token = 'FALSE' Then Items.Push(TExpressionOp.CreateBooleanOperand(False))
          Else
          Begin
            If IsString(Token) Then Items.Push(TExpressionOp.CreateStringOperand(Copy(BareToken,2,Length(BareToken) - 2)))
            Else If IsInteger(Token) Then
            Begin
              Val(Token,I,J);
              If J = 0 Then Items.Push(TExpressionOp.CreateIntegerOperand(I)) Else St := '';
            End
            Else If IsReal(Token) Then
            Begin
              Val(Token,E,J);
              If J = 0 Then Items.Push(TExpressionOp.CreateRealOperand(E)) Else St := '';
            End
            Else
            Begin
              I := IsGlobalVariable(Token);
              If I >= 0
               Then Items.Push(TExpressionOp.CreateVariableOperand(TVariableType(I)))
               Else Items.Push(TExpressionOp.CreateVariableOperand(vaVariable,Token,False,False));
            End;
          End;
        End;
      End;
    End
    Else St := ''; // Abort
  End; // While

  // Drain the stack

  While Stack.Count > 0 Do Items.Push(Stack.Pop);

  // Cleanup

  Stack.Free;
End; // TExpression.Create

Destructor TExpression.Destroy;
Begin
  While Items.Count > 0 Do Items.Pop.Free;
  Items.Free;
End; // TExpression.Destroy

Function TExpression.Evaluate(Variables: TStringList): TOperandRec;
Var
  I,J    : Integer;
  Stack  : TExpressionStack;
  Op     : TExpressionOp;
  OpType : TOperatorType;
  Op1    : TExpressionOp;
  Op2    : TExpressionOp;
  Error  : Boolean;
  E      : Extended;
  B      : Boolean;

Begin
  Stack := TExpressionStack.Create;
  Error := False;
  I     := 0;
  While (I < Items.Count) And Not Error Do
  Begin
    Op2 := Nil;
    Repeat
      Op := TExpressionOp(Items.Objects[I]);
      If Op.Kind = okOperand Then
      Begin
        // If the operand references a variable, change it to the actual value

        If Op.Data.Operand.OperandType = onVariable Then
        Begin
          Op2 := Nil;
          Case Op.Data.Operand.VariableType Of
            vaPlaceX: Op2 := TExpressionOp.CreateRealOperand(PlaceX);
            vaPlaceY: Op2 := TExpressionOp.CreateRealOperand(PlaceY);
            vaPlaceZ: Op2 := TExpressionOp.CreateRealOperand(PlaceZ);
           vaRotateX: Op2 := TExpressionOp.CreateRealOperand(RotateX);
           vaRotateY: Op2 := TExpressionOp.CreateRealOperand(RotateY);
           vaRotateZ: Op2 := TExpressionOp.CreateRealOperand(RotateZ);
             vaSizeX: Op2 := TExpressionOp.CreateRealOperand(SizeX);
             vaSizeY: Op2 := TExpressionOp.CreateRealOperand(SizeY);
             vaSizeZ: Op2 := TExpressionOp.CreateRealOperand(SizeZ);
                vaPi: Op2 := TExpressionOp.CreateRealOperand(Pi);
              vaVariable:
              Begin
                J := Variables.IndexOf(Op.Data.Operand.Name);
                If J >= 0 Then Op2 := TExpressionOp(Variables.Objects[J]).Copy
                Else
                Begin
                  Op2   := Op.Copy;
                  Error := True;
                End;
              End;
          End; // Case
          Stack.Push(Op2);
        End
        Else Stack.Push(Op.CopyNotVariableOperand);
      End;
      Inc(I);
    Until (I >= Items.Count) Or ((Op <> Nil) And (Op.Kind = okOperator));
    If (Op <> Nil) And (Op.Kind = okOperator) Then
    Begin
      OpType := Op.Data.OperatorType;
      If Cardinalities[OpType] = caUnary Then Op1 := Stack.Pop
      Else
      Begin
        Op2 := Stack.Pop;
        Op1 := Stack.Pop;
      End;

      // Make sure the arguments are valid

      If (Op1 = Nil) Or ((Cardinalities[OpType] = caBinary) And (Op2 = Nil)) Then Error := True
      Else
      Begin
        B := False; // Default value

        Case OpType Of
          otAdd:
          Begin
            If (Op1.Data.Operand.OperandType = onInteger) And
               (Op2.Data.Operand.OperandType = onInteger) Then
            Begin
              Inc(Op1.Data.Operand.Value.Int,Op2.Data.Operand.Value.Int);
            End
            Else If (Op1.Data.Operand.OperandType In [onInteger,onReal]) And
                    (Op2.Data.Operand.OperandType In [onInteger,onReal]) Then
            Begin
              If Op1.Data.Operand.OperandType = onInteger
               Then E := Op1.Data.Operand.Value.Int
               Else E := Op1.Data.Operand.Value.Ext;
              If Op2.Data.Operand.OperandType = onInteger
               Then E := E + Op2.Data.Operand.Value.Int
               Else E := E + Op2.Data.Operand.Value.Ext;
              Op1.Data.Operand.Value.Ext   := E;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else If (Op1.Data.Operand.OperandType = onString) And
                    (Op2.Data.Operand.OperandType = onString) Then
            Begin
              Op1.Data.Operand.Value.St := Op1.Data.Operand.Value.St + Op2.Data.Operand.Value.St;
            End
            Else Error := True;
            Op2.Free;
            Stack.Push(Op1);
          End;
          otSubtract:
          Begin
            If (Op1.Data.Operand.OperandType = onInteger) And
               (Op2.Data.Operand.OperandType = onInteger) Then
            Begin
              Dec(Op1.Data.Operand.Value.Int,Op2.Data.Operand.Value.Int);
            End
            Else If (Op1.Data.Operand.OperandType In [onInteger,onReal]) And
                    (Op2.Data.Operand.OperandType In [onInteger,onReal]) Then
            Begin
              If Op1.Data.Operand.OperandType = onInteger
               Then E := Op1.Data.Operand.Value.Int
               Else E := Op1.Data.Operand.Value.Ext;
              If Op2.Data.Operand.OperandType = onInteger
               Then E := E - Op2.Data.Operand.Value.Int
               Else E := E - Op2.Data.Operand.Value.Ext;
              Op1.Data.Operand.Value.Ext   := E;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else Error := True;
            Op2.Free;
            Stack.Push(Op1);
          End;
          otMultiply:
          Begin
            If (Op1.Data.Operand.OperandType = onInteger) And
               (Op2.Data.Operand.OperandType = onInteger) Then
            Begin
              Op1.Data.Operand.Value.Int := Op1.Data.Operand.Value.Int * Op2.Data.Operand.Value.Int;
            End
            Else If (Op1.Data.Operand.OperandType In [onInteger,onReal]) And
                    (Op2.Data.Operand.OperandType In [onInteger,onReal]) Then
            Begin
              If Op1.Data.Operand.OperandType = onInteger
               Then E := Op1.Data.Operand.Value.Int
               Else E := Op1.Data.Operand.Value.Ext;
              If Op2.Data.Operand.OperandType = onInteger
               Then E := E * Op2.Data.Operand.Value.Int
               Else E := E * Op2.Data.Operand.Value.Ext;
              Op1.Data.Operand.Value.Ext   := E;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else Error := True;
            Op2.Free;
            Stack.Push(Op1);
          End;
          otDivide:
          Begin
            If (Op1.Data.Operand.OperandType = onInteger) And
               (Op2.Data.Operand.OperandType = onInteger) Then
            Begin
              If Op2.Data.Operand.Value.Int <> 0 Then
              Begin
                E := Op1.Data.Operand.Value.Int;
                Op1.Data.Operand.Value.Ext := E / Op2.Data.Operand.Value.Int;
              End
              Else Error := True;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else If (Op1.Data.Operand.OperandType In [onInteger,onReal]) And
                    (Op2.Data.Operand.OperandType In [onInteger,onReal]) Then
            Begin
              If Op1.Data.Operand.OperandType = onInteger
               Then E := Op1.Data.Operand.Value.Int
               Else E := Op1.Data.Operand.Value.Ext;
              If Op2.Data.Operand.OperandType = onInteger Then
              Begin
                If Op2.Data.Operand.Value.Int <> 0 Then E := E / Op2.Data.Operand.Value.Int Else Error := True;
              End
              Else
              Begin
                If Op2.Data.Operand.Value.Ext <> 0 Then E := E / Op2.Data.Operand.Value.Ext Else Error := True;
              End;
              Op1.Data.Operand.Value.Ext   := E;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else Error := True;
            Op2.Free;
            Stack.Push(Op1);
          End;
          otAngle:
          Begin
            If (Op1.Data.Operand.OperandType = onInteger) And
               (Op2.Data.Operand.OperandType = onInteger) Then
            Begin
              Op1.Data.Operand.Value.Ext := ArcTan2(Op2.Data.Operand.Value.Int,Op1.Data.Operand.Value.Int) * 180 / Pi;
              While Op1.Data.Operand.Value.Ext < 0 Do Op1.Data.Operand.Value.Ext := Op1.Data.Operand.Value.Ext + 360;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else If (Op1.Data.Operand.OperandType In [onInteger,onReal]) And
                    (Op2.Data.Operand.OperandType In [onInteger,onReal]) Then
            Begin
              If Op1.Data.Operand.OperandType = onInteger
               Then E := Op1.Data.Operand.Value.Int
               Else E := Op1.Data.Operand.Value.Ext;
              If Op2.Data.Operand.OperandType = onInteger
               Then E := ArcTan2(Op2.Data.Operand.Value.Int,E) * 180 / Pi
               Else E := ArcTan2(Op2.Data.Operand.Value.Ext,E) * 180 / Pi;
              While E < 0 Do E := E + 360;
              Op1.Data.Operand.Value.Ext   := E;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else Error := True;
            Op2.Free;
            Stack.Push(Op1);
          End;
          otModulo:
          Begin
            If (Op1.Data.Operand.OperandType = onInteger) And
               (Op2.Data.Operand.OperandType = onInteger) Then
            Begin
              If Op2.Data.Operand.Value.Int <> 0 Then
              Begin
                Op1.Data.Operand.Value.Int := Op1.Data.Operand.Value.Int Mod Op2.Data.Operand.Value.Int;
              End
              Else Error := True;
            End
            Else Error := True;
            Op2.Free;
            Stack.Push(Op1);
          End;
          otNegate:
          Begin
                 If Op1.Data.Operand.OperandType = onInteger Then Op1.Data.Operand.Value.Int := -Op1.Data.Operand.Value.Int
            Else If Op1.Data.Operand.OperandType = onReal    Then Op1.Data.Operand.Value.Ext := -Op1.Data.Operand.Value.Ext
            Else Error := True;
            Stack.Push(Op1);
          End;
          otAnd:
          Begin
            If (Op1.Data.Operand.OperandType = onBoolean) And
               (Op2.Data.Operand.OperandType = onBoolean) Then
            Begin
              Op1.Data.Operand.Value.Bool := Op1.Data.Operand.Value.Bool And Op2.Data.Operand.Value.Bool;
            End
            Else If (Op1.Data.Operand.OperandType = onInteger) And
                    (Op2.Data.Operand.OperandType = onInteger) Then
            Begin
              Op1.Data.Operand.Value.Int := Op1.Data.Operand.Value.Int And Op2.Data.Operand.Value.Int;
            End
            Else Error := True;
            Op2.Free;
            Stack.Push(Op1);
          End;
          otOr:
          Begin
            If (Op1.Data.Operand.OperandType = onBoolean) And
               (Op2.Data.Operand.OperandType = onBoolean) Then
            Begin
              Op1.Data.Operand.Value.Bool := Op1.Data.Operand.Value.Bool Or Op2.Data.Operand.Value.Bool;
            End
            Else If (Op1.Data.Operand.OperandType = onInteger) And
                    (Op2.Data.Operand.OperandType = onInteger) Then
            Begin
              Op1.Data.Operand.Value.Int := Op1.Data.Operand.Value.Int Or Op2.Data.Operand.Value.Int;
            End
            Else Error := True;
            Op2.Free;
            Stack.Push(Op1);
          End;
          otXor:
          Begin
            If (Op1.Data.Operand.OperandType = onBoolean) And
               (Op2.Data.Operand.OperandType = onBoolean) Then
            Begin
              Op1.Data.Operand.Value.Bool := Op1.Data.Operand.Value.Bool Xor Op2.Data.Operand.Value.Bool;
            End
            Else If (Op1.Data.Operand.OperandType = onInteger) And
                    (Op2.Data.Operand.OperandType = onInteger) Then
            Begin
              Op1.Data.Operand.Value.Int := Op1.Data.Operand.Value.Int Xor Op2.Data.Operand.Value.Int;
            End
            Else Error := True;
            Op2.Free;
            Stack.Push(Op1);
          End;
          otNot:
          Begin
                 If Op1.Data.Operand.OperandType = onBoolean Then Op1.Data.Operand.Value.Bool := Not Op1.Data.Operand.Value.Bool
            Else If Op1.Data.Operand.OperandType = onInteger Then Op1.Data.Operand.Value.Int  := Not Op1.Data.Operand.Value.Int
            Else Error := True;
            Stack.Push(Op1);
          End;
          otShl:
          Begin
            If (Op1.Data.Operand.OperandType = onInteger) And
               (Op2.Data.Operand.OperandType = onInteger) Then Op1.Data.Operand.Value.Int := Op1.Data.Operand.Value.Int Shl Op2.Data.Operand.Value.Int
            Else Error := True;
            Op2.Free;
            Stack.Push(Op1);
          End;
          otShr:
          Begin
            If (Op1.Data.Operand.OperandType = onInteger) And
               (Op2.Data.Operand.OperandType = onInteger) Then Op1.Data.Operand.Value.Int := Op1.Data.Operand.Value.Int Shr Op2.Data.Operand.Value.Int
            Else Error := True;
            Op2.Free;
            Stack.Push(Op1);
          End;
          otEqualTo:
          Begin
            If (Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType) Or
               ((Op1.Data.Operand.OperandType In [onInteger,onReal]) And
                (Op2.Data.Operand.OperandType In [onInteger,onReal])) Then
            Begin
              If Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType Then
              Begin
                Case Op1.Data.Operand.OperandType Of
                  onInteger: B := (Op1.Data.Operand.Value.Int  = Op2.Data.Operand.Value.Int);
                     onReal: B := (Op1.Data.Operand.Value.Ext  = Op2.Data.Operand.Value.Ext);
                  onBoolean: B := (Op1.Data.Operand.Value.Bool = Op2.Data.Operand.Value.Bool);
                   onString: B := (Op1.Data.Operand.Value.St   = Op2.Data.Operand.Value.St);
                   onScript: B := (Op1.Data.Operand.Value.St   = Op2.Data.Operand.Value.St);
                End; // Case
              End
              Else If Op1.Data.Operand.OperandType = onInteger
               Then B := (Op1.Data.Operand.Value.Int = Op2.Data.Operand.Value.Ext)
               Else B := (Op1.Data.Operand.Value.Ext = Op2.Data.Operand.Value.Int);
            End
            Else Error := True;
            Op1.Free;
            Op2.Free;
            Stack.Push(TExpressionOp.CreateBooleanOperand(B));              
          End;
          otUnEqualTo:
          Begin
            If (Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType) Or
               ((Op1.Data.Operand.OperandType In [onInteger,onReal]) And
                (Op2.Data.Operand.OperandType In [onInteger,onReal])) Then
            Begin
              If Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType Then
              Begin
                Case Op1.Data.Operand.OperandType Of
                  onInteger: B := (Op1.Data.Operand.Value.Int  <> Op2.Data.Operand.Value.Int);
                     onReal: B := (Op1.Data.Operand.Value.Ext  <> Op2.Data.Operand.Value.Ext);
                  onBoolean: B := (Op1.Data.Operand.Value.Bool <> Op2.Data.Operand.Value.Bool);
                   onString: B := (Op1.Data.Operand.Value.St   <> Op2.Data.Operand.Value.St);
                   onScript: B := (Op1.Data.Operand.Value.St   <> Op2.Data.Operand.Value.St);
                End; // Case
              End
              Else If Op1.Data.Operand.OperandType = onInteger
               Then B := (Op1.Data.Operand.Value.Int <> Op2.Data.Operand.Value.Ext)
               Else B := (Op1.Data.Operand.Value.Ext <> Op2.Data.Operand.Value.Int);
            End
            Else Error := True;
            Op1.Free;
            Op2.Free;
            Stack.Push(TExpressionOp.CreateBooleanOperand(B));
          End;
          otLessThan:
          Begin
            If (Op1.Data.Operand.OperandType In [onInteger,onReal]) And
               (Op2.Data.Operand.OperandType In [onInteger,onReal]) Then
            Begin
              If Op1.Data.Operand.OperandType = onInteger Then
              Begin
                If Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType
                 Then B := (Op1.Data.Operand.Value.Int < Op2.Data.Operand.Value.Int)
                 Else B := (Op1.Data.Operand.Value.Int < Op2.Data.Operand.Value.Ext);
              End
              Else
              Begin
                If Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType
                 Then B := (Op1.Data.Operand.Value.Ext < Op2.Data.Operand.Value.Ext)
                 Else B := (Op1.Data.Operand.Value.Ext < Op2.Data.Operand.Value.Int);
              End;
            End
            Else Error := True;
            Op1.Free;
            Op2.Free;
            Stack.Push(TExpressionOp.CreateBooleanOperand(B));
          End;
          otGreaterThan:
          Begin
            If (Op1.Data.Operand.OperandType In [onInteger,onReal]) And
               (Op2.Data.Operand.OperandType In [onInteger,onReal]) Then
            Begin
              If Op1.Data.Operand.OperandType = onInteger Then
              Begin
                If Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType
                 Then B := (Op1.Data.Operand.Value.Int > Op2.Data.Operand.Value.Int)
                 Else B := (Op1.Data.Operand.Value.Int > Op2.Data.Operand.Value.Ext);
              End
              Else
              Begin
                If Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType
                 Then B := (Op1.Data.Operand.Value.Ext > Op2.Data.Operand.Value.Ext)
                 Else B := (Op1.Data.Operand.Value.Ext > Op2.Data.Operand.Value.Int);
              End;
            End
            Else Error := True;
            Op1.Free;
            Op2.Free;
            Stack.Push(TExpressionOp.CreateBooleanOperand(B));
          End;
          otLessOrEqual:
          Begin
            If (Op1.Data.Operand.OperandType In [onInteger,onReal]) And
               (Op2.Data.Operand.OperandType In [onInteger,onReal]) Then
            Begin
              If Op1.Data.Operand.OperandType = onInteger Then
              Begin
                If Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType
                 Then B := (Op1.Data.Operand.Value.Int <= Op2.Data.Operand.Value.Int)
                 Else B := (Op1.Data.Operand.Value.Int <= Op2.Data.Operand.Value.Ext);
              End
              Else
              Begin
                If Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType
                 Then B := (Op1.Data.Operand.Value.Ext <= Op2.Data.Operand.Value.Ext)
                 Else B := (Op1.Data.Operand.Value.Ext <= Op2.Data.Operand.Value.Int);
              End;
            End
            Else Error := True;
            Op1.Free;
            Op2.Free;
            Stack.Push(TExpressionOp.CreateBooleanOperand(B));
          End;
          otGreaterOrEqual:
          Begin
            If (Op1.Data.Operand.OperandType In [onInteger,onReal]) And
               (Op2.Data.Operand.OperandType In [onInteger,onReal]) Then
            Begin
              If Op1.Data.Operand.OperandType = onInteger Then
              Begin
                If Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType
                 Then B := (Op1.Data.Operand.Value.Int >= Op2.Data.Operand.Value.Int)
                 Else B := (Op1.Data.Operand.Value.Int >= Op2.Data.Operand.Value.Ext);
              End
              Else
              Begin
                If Op1.Data.Operand.OperandType = Op2.Data.Operand.OperandType
                 Then B := (Op1.Data.Operand.Value.Ext >= Op2.Data.Operand.Value.Ext)
                 Else B := (Op1.Data.Operand.Value.Ext >= Op2.Data.Operand.Value.Int);
              End;
            End
            Else Error := True;
            Op1.Free;
            Op2.Free;
            Stack.Push(TExpressionOp.CreateBooleanOperand(B));
          End;
          otTrunc:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              // Do nothing
            End
            Else If Op1.Data.Operand.OperandType = onReal Then
            Begin
              J := Trunc(Op1.Data.Operand.Value.Ext);
              Op1.Data.Operand.Value.Int := J;
              Op1.Data.Operand.OperandType := onInteger;
            End
            Else Error := True;
            Stack.Push(Op1);
          End;
          otRound:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              // Do nothing
            End
            Else If Op1.Data.Operand.OperandType = onReal Then
            Begin
              J := Round(Op1.Data.Operand.Value.Ext);
              Op1.Data.Operand.Value.Int := J;
              Op1.Data.Operand.OperandType := onInteger;
            End
            Else Error := True;
            Stack.Push(Op1);
          End;
          otRoundUp:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              // Do nothing
            End
            Else If Op1.Data.Operand.OperandType = onReal Then
            Begin
              J := Trunc(Op1.Data.Operand.Value.Ext);
              If Op1.Data.Operand.Value.Ext < 0 Then
              Begin
                If J > Op1.Data.Operand.Value.Ext Then Dec(J);
              End
              Else
              Begin
                If J < Op1.Data.Operand.Value.Ext Then Inc(J);
              End;
              Op1.Data.Operand.Value.Int := J;
              Op1.Data.Operand.OperandType := onInteger;
            End
            Else Error := True;
            Stack.Push(Op1);
          End;
          otFrac:
          Begin
            If Op1.Data.Operand.OperandType = onReal Then Op1.Data.Operand.Value.Ext := Frac(Op1.Data.Operand.Value.Ext) Else Error := True;
            Stack.Push(Op1);
          End;
          otSgn:
          Begin
                 If Op1.Data.Operand.OperandType = onInteger Then Op1.Data.Operand.Value.Int := Sign(Op1.Data.Operand.Value.Int)
            Else If Op1.Data.Operand.OperandType = onReal Then
            Begin
              J := Sign(Op1.Data.Operand.Value.Ext);
              Op1.Data.Operand.Value.Int := J;
              Op1.Data.Operand.OperandType := onInteger;
            End
            Else Error := True;
            Stack.Push(Op1);
          End;
          otAbs:
          Begin
                 If Op1.Data.Operand.OperandType = onInteger Then Op1.Data.Operand.Value.Int := Abs(Op1.Data.Operand.Value.Int)
            Else If Op1.Data.Operand.OperandType = onReal    Then Op1.Data.Operand.Value.Ext := Abs(Op1.Data.Operand.Value.Ext)
            Else Error := True;
            Stack.Push(Op1);
          End;
          otInt:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              // Do nothing
            End
            Else If Op1.Data.Operand.OperandType = onReal Then
            Begin
              J := Trunc(Int(Op1.Data.Operand.Value.Ext));
              Op1.Data.Operand.Value.Int := J;
              Op1.Data.Operand.OperandType := onInteger;
            End
            Else Error := True;
            Stack.Push(Op1);
          End;
          otFloor:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              // Do nothing
            End
            Else If Op1.Data.Operand.OperandType = onReal Then
            Begin
              J := Floor(Op1.Data.Operand.Value.Ext);
              Op1.Data.Operand.Value.Int := J;
              Op1.Data.Operand.OperandType := onInteger;
            End
            Else Error := True;
            Stack.Push(Op1);
          End;
          otCeil:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              // Do nothing
            End
            Else If Op1.Data.Operand.OperandType = onReal Then
            Begin
              J := Ceil(Op1.Data.Operand.Value.Ext);
              Op1.Data.Operand.Value.Int := J;
              Op1.Data.Operand.OperandType := onInteger;
            End
            Else Error := True;
            Stack.Push(Op1);
          End;
          otSqrt:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              If Op1.Data.Operand.Value.Int > 0 Then
              Begin
                E := Sqrt(Op1.Data.Operand.Value.Int);
                Op1.Data.Operand.Value.Ext := E;
                Op1.Data.Operand.OperandType := onReal;
              End
              Else Error := True;
            End
            Else If Op1.Data.Operand.OperandType = onReal Then
            Begin
              If Op1.Data.Operand.Value.Ext > 0 Then
              Begin
                Op1.Data.Operand.Value.Ext := Sqrt(Op1.Data.Operand.Value.Ext);
              End
              Else Error := True;
            End
            Else Error := True;
            Stack.Push(Op1);
          End;
          otSin:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              E := Sin(Op1.Data.Operand.Value.Int * (Pi / 180));
              Op1.Data.Operand.Value.Ext := E;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else If Op1.Data.Operand.OperandType = onReal Then Op1.Data.Operand.Value.Ext := Sin(Op1.Data.Operand.Value.Ext * (Pi / 180))
            Else Error := True;
            Stack.Push(Op1);
          End;
          otCos:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              E := Cos(Op1.Data.Operand.Value.Int * (Pi / 180));
              Op1.Data.Operand.Value.Ext := E;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else If Op1.Data.Operand.OperandType = onReal Then Op1.Data.Operand.Value.Ext := Cos(Op1.Data.Operand.Value.Ext * (Pi / 180))
            Else Error := True;
            Stack.Push(Op1);
          End;
          otTan:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              E := Tan(Op1.Data.Operand.Value.Int * (Pi / 180));
              Op1.Data.Operand.Value.Ext := E;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else If Op1.Data.Operand.OperandType = onReal Then Op1.Data.Operand.Value.Ext := Tan(Op1.Data.Operand.Value.Ext * (Pi / 180))
            Else Error := True;
            Stack.Push(Op1);
          End;
          otExp:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              E := Exp(Op1.Data.Operand.Value.Int);
              Op1.Data.Operand.Value.Ext := E;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else If Op1.Data.Operand.OperandType = onReal Then Op1.Data.Operand.Value.Ext := Exp(Op1.Data.Operand.Value.Ext)
            Else Error := True;
            Stack.Push(Op1);
          End;
          otLn:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              If Op1.Data.Operand.Value.Int > 0 Then
              Begin
                E := Ln(Op1.Data.Operand.Value.Int);
                Op1.Data.Operand.Value.Ext := E;
                Op1.Data.Operand.OperandType := onReal;
              End
              Else Error := True;  
            End
            Else If Op1.Data.Operand.OperandType = onReal Then
            Begin
              If Op1.Data.Operand.Value.Ext > 0 Then Op1.Data.Operand.Value.Ext := Ln(Op1.Data.Operand.Value.Ext) Else Error := True;
            End
            Else Error := True;
            Stack.Push(Op1);
          End;
          otLog10:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              If Op1.Data.Operand.Value.Int > 0 Then
              Begin
                E := Log10(Op1.Data.Operand.Value.Int);
                Op1.Data.Operand.Value.Ext := E;
                Op1.Data.Operand.OperandType := onReal;
              End
              Else Error := True;
            End
            Else If Op1.Data.Operand.OperandType = onReal Then
            Begin
              If Op1.Data.Operand.Value.Ext > 0 Then Op1.Data.Operand.Value.Ext := Log10(Op1.Data.Operand.Value.Ext) Else Error := True;
            End
            Else Error := True;
            Stack.Push(Op1);
          End;
          otLog2:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              If Op1.Data.Operand.Value.Int > 0 Then
              Begin
                E := Log2(Op1.Data.Operand.Value.Int);
                Op1.Data.Operand.Value.Ext := E;
                Op1.Data.Operand.OperandType := onReal;
              End
              Else Error := True;
            End
            Else If Op1.Data.Operand.OperandType = onReal Then
            Begin
              If Op1.Data.Operand.Value.Ext > 0 Then Op1.Data.Operand.Value.Ext := Log2(Op1.Data.Operand.Value.Ext) Else Error := True;
            End
            Else Error := True;
            Stack.Push(Op1);
          End;
          otSqr:
          Begin
            If Op1.Data.Operand.OperandType = onInteger Then
            Begin
              E := Sqr(Op1.Data.Operand.Value.Int);
              Op1.Data.Operand.Value.Ext := E;
              Op1.Data.Operand.OperandType := onReal;
            End
            Else If Op1.Data.Operand.OperandType = onReal Then Op1.Data.Operand.Value.Ext := Sqr(Op1.Data.Operand.Value.Ext)
            Else Error := True;
            Stack.Push(Op1);
          End;
        End; // Case
      End;
    End;
  End; // While

  // Get the result

  If Error Or (Stack.Count = 0) Then Result.OperandType := onError
  Else
  Begin
    Op1    := Stack.Pop;
    Result := Op1.Data.Operand;
    Op1.Free;
  End;

  // Cleanup

  While Stack.Count > 0 Do Stack.Pop.Free;
  Stack.Free;
End; // TExpression.Evaluate

end.
