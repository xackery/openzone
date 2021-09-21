unit BitmapComponents;

interface

Uses Windows,Graphics,Classes,Forms,Controls,SyncObjs,MyHashes,U3DPolys,AbstractComponents,JCLGraphics;

Type
  ICanvasComponent = Interface(IAbstractComponent)
    Function GetCanvas: TJCLBitmap32;
    Property Canvas : TJCLBitmap32 Read GetCanvas;
  End;

  TCanvasButton = Class(TAbstractButton, ICanvasComponent)
  Protected
    FCanvas : TJCLBitmap32;
    Function    GetCanvas: TJCLBitmap32;
    Procedure   FillBackground; Override;
    Procedure   CreateChildren; Override;
  Public
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TAbstractButtonKind;
                       AStyle: TAbstractButtonStyle; ACaption: String);
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TCanvasEdit = Class(TAbstractEdit, ICanvasComponent)
  Protected
    FCanvas : TJCLBitmap32;
    Function    GetCanvas: TJCLBitmap32;
    Procedure   FillBackground; Override;
  Public
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; AText: String; AMaxLen: Integer);
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TCanvasScrollBar = Class(TAbstractScrollBar, ICanvasComponent)
  Protected
    FCanvas : TJCLBitmap32;
    Procedure   CreateChildren; Override;
    Procedure   AfterSetSize; Override;
    Function    GetCanvas: TJCLBitmap32;
    Procedure   FillBackground; Override;
  Public
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TScrollBarKind); Overload;
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop,ALength: Integer; AKind: TScrollBarKind); Overload;
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TCanvasTrackBar = Class(TAbstractTrackBar, ICanvasComponent)
  Protected
    FCanvas : TJCLBitmap32;
    Function    GetCanvas: TJCLBitmap32;
    Procedure   FillBackground; Override;
  Public
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer); Overload;
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop,ALength: Integer); Overload;
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TCanvasListBox = Class(TAbstractListBox, ICanvasComponent)
  Protected
    FCanvas : TJCLBitmap32;
    Procedure   AfterSetSize; Override;
    Procedure   CreateChildren; Override;
    Procedure   OutputText(X,Y: Integer; St: String; DefaultColor: TColor); Override;
    Function    GetCanvas: TJCLBitmap32;
    Procedure   FillBackground; Override;
  Public
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; AWordWrapped: Boolean);
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TCanvasLabel = Class(TAbstractLabel, ICanvasComponent)
  Protected
    FCanvas : TJCLBitmap32;
    Function    GetCanvas: TJCLBitmap32;
    Procedure   FillBackground; Override;
    Procedure   DoAutoSize; Override;
  Public
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; ATransparent: Boolean); Overload;
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop: Integer; ACaption: String; ATransparent: Boolean); Overload;
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TCanvasScrollPane = Class(TAbstractScrollPane, ICanvasComponent)
  Protected
    FCanvas : TJCLBitmap32;
    Procedure   CreateChildren; Override;
    Function    GetCanvas: TJCLBitmap32;
    Procedure   FillBackground; Override;
  Public
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer);
  End;

  TCanvasPanel = Class(TAbstractPanel, ICanvasComponent)
  Protected
    FCanvas : TJCLBitmap32;
    Procedure   CreateChildren; Override;
    Function    GetCanvas: TJCLBitmap32;
    Procedure   FillBackground; Override;
  Public
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String);
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TCanvasProgressBar = Class(TAbstractProgressBar, ICanvasComponent)
  Protected
    FCanvas : TJCLBitmap32;
    Function    GetCanvas: TJCLBitmap32;
    Procedure   FillBackground; Override;
  Public
    Constructor Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; AValue: Single; AColor: TColor);
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TCanvasWindow = Class(TAbstractWindow, ICanvasComponent)
  Protected
    FCanvas : TJCLBitmap32;
    Procedure   AfterSetSize; Override;
    Procedure   CalcClientExtents; Override;
    Procedure   CreateCloseButton; Override;
    Function    GetCanvas: TJCLBitmap32;
    Function    GetChildClipRect(ChildComponent: TAbstractComponent): TRect; Override;
    Procedure   FillBackground; Override;
  Public
    ModalResult : Boolean;
    Constructor Create(ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; AUI: TAbstractUI);
    Destructor  Destroy; Override;
    Procedure   Paint(X,Y: Integer); Override;
    Function    TextWidth(Const St: String): Integer;    Override;
    Function    TextHeight(Const St: String): Integer;   Override;
    Procedure   TextOut(X,Y: Integer; Const St: String); Override;
    Procedure   BeginClip(Rect: TRect); Override;
    Procedure   EndClip;                Override;
  End;

  TCanvasUI = Class(TAbstractUI)
  Public
    Constructor Create;
    Destructor  Destroy; Override;
    Function    AddWindow(ACaption: String): TAbstractWindow; Override;
  End;

implementation

Uses OgreInput, SysUtils, Math;

Const ScrollBarThickness = 16;
(*
// -------------------------------
// TCanvasComponent
// -------------------------------

Constructor TCanvasComponent.Create(AParent: TCanvasComponent);
Begin
  FCanvas     := AParent.Canvas;
  FLeft       := 0;
  FTop        := 0;
  FWidth      := 64;
  FHeight     := 64;
  FParent     := AParent;
  BasicInit;
End; // TCanvasComponent.Create

Constructor TCanvasComponent.Create(AParent: TCanvasComponent; ALeft,ATop,AWidth,AHeight: Integer);
Begin
  FCanvas     := AParent.Canvas;
  FLeft       := ALeft;
  FTop        := ATop;
  FWidth      := AWidth;
  FHeight     := AHeight;
  FParent     := AParent;
  BasicInit;
End; // TCanvasComponent.Create

Procedure TCanvasComponent.BasicInit;
Begin
  FVisible         := True;
  FEnabled         := True;
  FChildren        := TStringList.Create;
  FAlign           := [alLeft,alTop];
  FForeground      := TColor($00FFFFFF);
  FBackground      := TColor($00000000);
  FFocused         := False;
  FChanging        := False;
  FMouseOver       := False;
  If FParent <> Nil Then
  Begin
    FUI := FParent.UI;
    If FParent Is TCanvasWindow
     Then FOwner := TCanvasWindow(FParent)
     Else FOwner := FParent.Owner;
  End
  Else
  Begin
    FUI    := Nil;
    FOwner := Nil;
  End;
  CalcClientExtents;
  FMutex           := TCriticalSection.Create;
  FTag             := 0;
  FAcceptEvents    := [evtMouseDown,evtMouseUp,evtMouseMove];
  FOnMouseEnter    := Nil;
  FOnMouseExit     := Nil;
  FOnKeyDown       := Nil;
  FOnPaint         := Nil;
  FOnBeforePaint   := Nil;
  FOnMouseDown     := Nil;
  FOnClose         := Nil;
  FOnCloseQuery    := Nil;
  FTransparent     := False;
  FClipRect.Left   := 0;
  FClipRect.Top    := 0;
  FClipRect.Right  := 0;
  FClipRect.Bottom := 0;
  If FParent <> Nil Then FParent.AddChild(Self);
End; // TCanvasComponent.BasicInit

Destructor TCanvasComponent.Destroy;
Var I: Integer;
Begin
  For I := 0 To FChildren.Count - 1 Do FChildren.Objects[I].Free;
  FChildren.Free;
  FMutex.Free;
End; // TCanvasComponent.Destroy

Procedure TCanvasComponent.Changing;
Begin
  FChanging := True;
End; // TCanvasComponent.Changing

Procedure TCanvasComponent.Changed;
Begin
  FChanging := False;
End; // TCanvasComponent.Changed

Function TCanvasComponent.GetComponentCount: Integer;
Begin
  Try
    FMutex.Enter;
    Result := FChildren.Count;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.GetComponentCount

Function TCanvasComponent.GetComponent(Index: Integer): TCanvasComponent;
Begin
  Try
    FMutex.Enter;
    If (Index >= 0) And (Index < FChildren.Count)
     Then Result := TCanvasComponent(FChildren.Objects[Index])
     Else Result := Nil;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.GetComponent

Procedure TCanvasComponent.RepaintParent;
Begin
  If FParent <> Nil Then FParent.QueueRepaint Else QueueRepaint;
End; // TCanvasComponent.RepaintParent

Procedure TCanvasComponent.SetLeft(I: Integer);
Begin
  Try
    FMutex.Enter;
    If FLeft <> I Then
    Begin
      FLeft := I;
      CalcClientExtents;
      RepaintParent;
    End;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetLeft

Procedure TCanvasComponent.SetTop(I: Integer);
Begin
  Try
    FMutex.Enter;
    If FTop <> I Then
    Begin
      FTop := I;
      CalcClientExtents;
      RepaintParent;
    End;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetTop

Procedure TCanvasComponent.SetWidth(I: Integer);
Var J,K: Integer;
Begin
  Try
    FMutex.Enter;
    If FWidth <> I Then
    Begin
      K      := FWidth;
      FWidth := I;
      CalcClientExtents;
      AfterSetSize;
      For J := 0 To FChildren.Count - 1 Do TCanvasComponent(FChildren.Objects[J]).Realign(K,FHeight);
      RepaintParent;
    End;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetWidth

Procedure TCanvasComponent.SetHeight(I: Integer);
Var J,K: Integer;
Begin
  Try
    FMutex.Enter;
    If FHeight <> I Then
    Begin
      K       := FHeight;
      FHeight := I;
      CalcClientExtents;
      AfterSetSize;
      For J := 0 To FChildren.Count - 1 Do TCanvasComponent(FChildren.Objects[J]).Realign(FWidth,K);
      RepaintParent;
    End;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetHeight

Procedure TCanvasComponent.SetVisible(B: Boolean);
Var CanClose: Boolean;
Begin
  Try
    FMutex.Enter;
    If FVisible <> B Then
    Begin
      CanClose := True;
      If Assigned(FOnCloseQuery) And Not B Then FOnCloseQuery(Self,CanClose);
      If CanClose Then
      Begin
        FVisible := B;
        If FVisible Then QueueRepaint Else RepaintParent;  // Want to use the parent's background color
        If Assigned(FOnClose) And Not B Then FOnClose(Self);
      End;   
    End;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetVisible

Procedure TCanvasComponent.SetEnabled(B: Boolean);
Begin
  Try
    FMutex.Enter;
    If FEnabled <> B Then
    Begin
      FEnabled := B;
      QueueRepaint;
    End;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetEnabled

Procedure TCanvasComponent.SetForeground(C: TColor);
Begin
  Try
    FMutex.Enter;
    If C <> FForeground Then
    Begin
      FForeground := C;
      QueueRepaint;
    End;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetForeground

Procedure TCanvasComponent.SetBackground(C: TColor);
Begin
  Try
    FMutex.Enter;
    If C <> FBackground Then
    Begin
      FBackground := C;
      QueueRepaint;
    End;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetBackground

Procedure TCanvasComponent.AddChild(Child: TCanvasComponent);
Begin
  Try
    FMutex.Enter;
    FChildren.AddObject('',Child);
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.AddChild

Procedure TCanvasComponent.CalcAbsoluteXY(Out X,Y: Integer);
Var P: TCanvasComponent;
Begin
  P := FParent;
  If P = Nil Then
  Begin
    X := 0;
    Y := 0;
  End
  Else
  Begin
    X := FLeft;
    Y := FTop;
  End;
  While P <> Nil Do
  Begin
    Inc(X,P.FClientLeft);
    Inc(Y,P.FClientTop);
    If P.Parent <> Nil Then
    Begin
      Inc(X,P.FLeft);
      Inc(Y,P.FTop);
    End;
    P := P.Parent;
  End; // While
End; // TCanvasComponent.CalcAbsoluteXY

Procedure TCanvasComponent.CreateChildren;
Begin
End; // TCanvasComponent.CreateChildren

Procedure TCanvasComponent.Paint(X,Y: Integer);
Begin
End; // TCanvasComponent.Paint

Function TCanvasComponent.GetChildClipRect(ChildComponent: TCanvasComponent): TRect;
Var
  X,Y   : Integer;
  Rect  : TRect;
  Rect1 : TRect;

Begin
  CalcAbsoluteXY(X,Y);
  Rect.Left   := X + ClientLeft;
  Rect.Top    := Y + ClientTop;
  Rect.Right  := Rect.Left + ClientWidth  - 1;
  Rect.Bottom := Rect.Top  + ClientHeight - 1;
  If FParent <> Nil Then
  Begin
    Rect1       := FParent.GetChildClipRect(Self);
    Rect.Left   := Max(Rect.Left,   Rect1.Left);
    Rect.Top    := Max(Rect.Top,    Rect1.Top);
    Rect.Right  := Min(Rect.Right,  Rect1.Right);
    Rect.Bottom := Min(Rect.Bottom, Rect1.Bottom);
  End;
  Result := Rect;
End; // TCanvasComponent.GetChildClipRect

Function TCanvasComponent.GetClipRect: TRect;
Var
  X,Y   : Integer;
  Rect  : TRect;
  Rect1 : TRect;

Begin
  CalcAbsoluteXY(X,Y);
  Rect.Left   := X;
  Rect.Top    := Y;
  Rect.Right  := X + FWidth  - 1;
  Rect.Bottom := Y + FHeight - 1;
  If FParent <> Nil Then
  Begin
    Rect1       := FParent.GetChildClipRect(Self);
    Rect.Left   := Max(Rect.Left,   Rect1.Left);
    Rect.Top    := Max(Rect.Top,    Rect1.Top);
    Rect.Right  := Min(Rect.Right,  Rect1.Right);
    Rect.Bottom := Min(Rect.Bottom, Rect1.Bottom);
  End;
  Result := Rect;
End; // TCanvasComponent.GetClipRect

Procedure TCanvasComponent.BeforePaintingChildren;
Begin
End; // TCanvasComponent.BeforePaintingChildren

Procedure TCanvasComponent.AfterPaintingChildren;
Begin
End; // TCanvasComponent.AfterPaintingChildren

Function TCanvasComponent.TextWidth(Const St: String): Integer;
Begin
  If FOwner <> Nil
   Then Result := FOwner.TextWidth(St)
   Else Result := 0;
End; // TCanvasComponent.TextWidth

Function TCanvasComponent.TextHeight(Const St: String): Integer;
Begin
  If FOwner <> Nil
   Then Result := FOwner.TextHeight(St)
   Else Result := 0;
End; // TCanvasComponent.TextHeight

Procedure TCanvasComponent.TextOut(X,Y: Integer; St: String);
Begin
  If FOwner <> Nil Then FOwner.TextOut(X,Y,St);
End; // TCanvasComponent.TextOut

Procedure TCanvasComponent.BeginClip(Rect: TRect);
Begin
  If FOwner <> Nil Then FOwner.BeginClip(Rect);
End; // TCanvasComponent.BeginClip

Procedure TCanvasComponent.EndClip;
Begin
  If FOwner <> Nil Then FOwner.EndClip;
End; // TCanvasComponent.EndClip

Procedure TCanvasComponent.Repaint;
Var
  I,X,Y       : Integer;
  Rect        : TRect;
  OldClipRect : TRect;

Begin
  Try
    FMutex.Enter;

    // Clip to this component's extents

    CalcAbsoluteXY(X,Y);
    Rect        := GetClipRect;
    OldClipRect := FClipRect;
    FClipRect   := Rect;
    BeginClip(Rect);

    // Paint the background

    If (FCanvas <> Nil) And Not FTransparent Then
    Begin
      FCanvas.PenColor := FBackground;
      FCanvas.FillRectSC(X,Y,X + Width - 1,Y + Height - 1,FBackground,FClipRect);
    End;
    If FVisible Then
    Begin
      Paint(X,Y);

      If Assigned(FOnBeforePaint) Then FOnBeforePaint(Self,X,Y);

      // Paint the child components

      BeforePaintingChildren;
      For I := 0 To FChildren.Count - 1 Do
      Begin
        TCanvasComponent(FChildren.Objects[I]).Repaint;
        FClipRect := Rect;
      End; // For I
      AfterPaintingChildren;

      // See if the optional event handler was set

      If Assigned(FOnPaint) Then FOnPaint(Self,X,Y);
    End;
    FClipRect := OldClipRect;
    EndClip;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.Repaint

Procedure TCanvasComponent.SetTransparent(B: Boolean);
Begin
  Try
    FMutex.Enter;
    FTransparent := B;
    QueueRepaint;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetTransparent

Procedure TCanvasComponent.SetAlign(AAlign: TAlignSet);
Begin
  Try
    FMutex.Enter;
    FAlign := AAlign;
    QueueRepaint;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetAlign

Procedure TCanvasComponent.Realign(OldWidth,OldHeight: Integer);
Begin
  If FParent <> Nil Then
  Begin
    If (FParent.Width <> OldWidth) And ([alRight,alClient] * FAlign <> []) Then
    Begin
      If [alLeft,alClient] * FAlign <> [] Then
      Begin
        Width := Width + FParent.Width - OldWidth;
        If FWidth < 0 Then FWidth := 0;
//        QueueRepaint;
      End
      Else
      Begin
        Inc(FLeft,FParent.Width - OldWidth);
//        QueueRepaint;
      End;
    End;
    If (FParent.Height <> OldHeight) And ([alBottom,alClient] * FAlign <> []) Then
    Begin
      If [alTop,alClient] * FAlign <> [] Then
      Begin
        Height := Height + FParent.Height - OldHeight;
        If FHeight < 0 Then FHeight := 0;
//        QueueRepaint;
      End
      Else
      Begin
        Inc(FTop,FParent.Height - OldHeight);
//        QueueRepaint;
      End;
    End;
  End;
End; // TCanvasComponent.Realign

Procedure TCanvasComponent.AfterSetSize;
Begin
End; // TCanvasComponent.AfterSetSize

Procedure TCanvasComponent.CalcClientExtents;
Begin

    FClientLeft := 0;
    FClientTop  := 0;

{
  If FParent = Nil Then
  Begin
    FClientLeft := 0;
    FClientTop  := 0;
  End
  Else
  Begin
    FClientLeft := FLeft;
    FClientTop  := FTop;
  End;
}
  FClientWidth  := FWidth;
  FClientHeight := FHeight;
End; // TCanvasComponent.CalcClientExtents

Function TCanvasComponent.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
Var
  I    : Integer;
  B    : Boolean;
  Rect : TRect;

Begin
  B := False;
  Try
    FMutex.Enter;
    If FVisible And (evtMouseDown In FAcceptEvents) Then
    Begin
      If FParent = Nil Then
      Begin
        Dec(X,FLeft);
        Dec(Y,FTop);
      End;
      I := 0;
      While (I < FChildren.Count) And Not B Do
      Begin
        B := B Or TCanvasComponent(FChildren.Objects[I]).MouseDown(Button,Shift,X,Y);
        Inc(I);
      End; // While
      If Not B Then
      Begin
        Rect := GetClipRect;
        B    := B Or ((X >= Rect.Left) And (Y >= Rect.Top) And (X <= Rect.Right) And (Y <= Rect.Bottom));
        If B Then
        Begin
          Focused := True;
          MouseDownAction(Button,Shift,X,Y);
          If Assigned(FOnMouseDown) Then FOnMouseDown(Self,Button,Shift,X,Y);
        End;
      End;
    End;
  Finally
    FMutex.Leave;
  End;
  Result := B;
End; // TCanvasComponent.MouseDown

Function TCanvasComponent.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
Var
  I    : Integer;
  B    : Boolean;
  Rect : TRect;

Begin
  B := False;
  Try
    FMutex.Enter;
    If FVisible And (evtMouseUp In FAcceptEvents) Then
    Begin
      If FParent = Nil Then
      Begin
        Dec(X,FLeft);
        Dec(Y,FTop);
      End;
      I := 0;
      While (I < FChildren.Count) And Not B Do
      Begin
        B := B Or TCanvasComponent(FChildren.Objects[I]).MouseUp(Button,Shift,X,Y);
        Inc(I);
      End; // While
      If Not B Then
      Begin
        Rect := GetClipRect;
        B    := B Or ((X >= Rect.Left) And (Y >= Rect.Top) And (X <= Rect.Right) And (Y <= Rect.Bottom));
        If B Then
        Begin
          MouseUpAction(Button,Shift,X,Y);
          If Assigned(FOnMouseUp) Then FOnMouseUp(Self,Button,Shift,X,Y);
        End;
      End;
    End;
  Finally
    FMutex.Leave;
  End;
  Result := B;
End; // TCanvasComponent.MouseUp

Function TCanvasComponent.MouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
Var
  I    : Integer;
  B    : Boolean;
  Rect : TRect;

Begin
  B := False;
  Try
    FMutex.Enter;
    If FVisible And (evtMouseMove In FAcceptEvents) Then
    Begin
      If FParent = Nil Then
      Begin
        Dec(X,FLeft);
        Dec(Y,FTop);
      End;
      I := 0;
      While (I < FChildren.Count) And Not B Do
      Begin
        B := B Or TCanvasComponent(FChildren.Objects[I]).MouseMove(Shift,X,Y);
        Inc(I);
      End; // While
      If Not B Then
      Begin
        Rect := GetClipRect;
        B    := B Or ((X >= Rect.Left) And (Y >= Rect.Top) And (X <= Rect.Right) And (Y <= Rect.Bottom));
        If B Then
        Begin
          MouseMoveAction(Shift,X,Y);
        End;
      End;
      If B <> FMouseOver Then
      Begin
        FMouseOver := B;
             If Assigned(FOnMouseEnter) And     B Then FOnMouseEnter(Self)
        Else If Assigned(FOnMouseExit)  And Not B Then FOnMouseExit(Self);
        QueueRepaint;
      End;
    End;
  Finally
    FMutex.Leave;
  End;
  Result := B;
End; // TCanvasComponent.MouseMove

Procedure TCanvasComponent.MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
End; // TCanvasComponent.MouseDownAction

Procedure TCanvasComponent.MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
End; // TCanvasComponent.MouseUpAction

Procedure TCanvasComponent.MouseMoveAction(Shift: TShiftState; X, Y: Integer);
Begin
End; // TCanvasComponent.MouseMoveAction

Procedure TCanvasComponent.SetUI(AUI: TCanvasUI);
Var I: Integer;
Begin
  Try
    FMutex.Enter;
    FUI := AUI;
    If FParent = Nil Then
    Begin
      For I := 0 To FChildren.Count - 1 Do TCanvasComponent(FChildren.Objects[I]).UI := AUI;
    End;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetUI

Procedure TCanvasComponent.SetFocused(B: Boolean);
Begin
  Try
    FMutex.Enter;
    If B <> FFocused Then
    Begin
      If B Then
      Begin
        If FUI <> Nil Then
        Begin
          If FUI.FFocusedComponent <> Nil Then FUI.FFocusedComponent.Focused := False;
          FUI.FFocusedComponent := Self;
        End;
        FFocused := True;
        QueueRepaint;
      End
      Else
      Begin
        FFocused := False;
        If FUI <> Nil Then FUI.FFocusedComponent := Nil;
        QueueRepaint;
      End;
    End;
  Finally
    FMutex.Leave;
  End;
End; // TCanvasComponent.SetFocused

Function TCanvasComponent.KeyDown(Key,Shift: Integer; C: Char): Boolean;
Var
  B      : Boolean;
  I      : Integer;
  Child  : TCanvasComponent;
  Key1   : Word;
  Shift1 : TShiftState;

Begin
  B := False;
  Try
    FMutex.Enter;
    If FVisible Then
    Begin
      I := 0;
      While (I < FChildren.Count) And Not B Do
      Begin
        Child := TCanvasComponent(FChildren.Objects[I]);
        If (Child.Focused Or (Child.FChildren.Count > 0)) And Not B Then
        Begin
          B := B Or Child.KeyDownAction(Key,Shift,C);
          If B And Assigned(Child.FOnKeyDown) Then
          Begin
            Key1 := Key;
            Shift1 := [];
            If (Shift And ShiftFlag) <> 0 Then Shift1 := Shift1 + [ssShift];
            Child.FOnKeyDown(Child,Key1,Shift1);
          End;
        End;
        Inc(I);
      End; // While
      If Focused And Not B Then
      Begin
        B := B Or KeyDownAction(Key,Shift,C);
        If B And Assigned(FOnKeyDown) Then
        Begin
          Key1 := Key;
          Shift1 := [];
          If (Shift And ShiftFlag) <> 0 Then Shift1 := Shift1 + [ssShift];
          FOnKeyDown(Self,Key1,Shift1);
        End;
      End;
    End;
  Finally
    FMutex.Leave;
  End;
  Result := B;
End; // TCanvasComponent.KeyDown

Function TCanvasComponent.KeyDownAction(Key,Shift: Integer; C: Char): Boolean;
Begin
  Result := False;
End; // TCanvasComponent.KeyDownAction

Procedure TCanvasComponent.SetCanvas(ACanvas: TJCLBitmap32);
Var I: Integer;
Begin
  FCanvas := ACanvas;
  For I := 0 To FChildren.Count - 1 Do TCanvasComponent(FChildren.Objects[I]).Canvas := ACanvas;
End; // TCanvasComponent.SetCanvas

Procedure TCanvasComponent.QueueRepaint;
Begin
  If FUI <> Nil
   Then FUI.AddRepaint(Self)
   Else Repaint;
End; // TCanvasComponent.QueueRepaint

Procedure TCanvasComponent.Rectangle(X1,Y1,X2,Y2: Integer; FillColor: TColor);
Begin
  If (X1 <> X2) And (Y1 <> Y2) Then
  Begin
    FCanvas.FillRectSC(X1 + 1,Y1 + 1,X2 - 1,Y2 - 1,FillColor,FClipRect);
    FCanvas.FrameRectSC(X1,Y1,X2,Y2,FCanvas.PenColor,FClipRect);
  End;
End; // TCanvasComponent.Rectangle
*)
// -------------------------------
// TCanvasButton
// -------------------------------

Constructor TCanvasButton.Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TAbstractButtonKind;
                                 AStyle: TAbstractButtonStyle; ACaption: String);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,AKind,AStyle,ACaption);
  FCanvas := AParent.Canvas;
End; // TCanvasButton.Create

Procedure TCanvasButton.CreateChildren;
Begin
  FLabel := TCanvasLabel.Create(Self,0,0,'',True);
End; // TCanvasButton.CreateChildren

Function TCanvasButton.GetCanvas: TJCLBitmap32;
Begin
  Result := FCanvas;
End; // TCanvasButton.GetCanvas

Procedure TCanvasButton.FillBackground;
Var X,Y: Integer;
Begin
  If (FCanvas <> Nil) And Not FTransparent Then
  Begin
    CalcAbsoluteXY(X,Y);
    FCanvas.PenColor := FBackground;
    FCanvas.FillRectSC(X,Y,X + Width - 1,Y + Height - 1,FBackground,FClipRect);
  End;
End; // TCanvasButton.FillBackground

Procedure TCanvasButton.Paint(X,Y: Integer);
Var
  W,H,I : Integer;
  C     : TColor;

Begin
  Inherited Paint(X,Y);
  If FCanvas <> Nil Then
  Begin
    If FDown Then
    Begin
      FCanvas.PenColor := FForeground;
      Rectangle(X,Y,X + FWidth - 1,Y + FHeight - 1,FForeground);
      I := 1;
      C := FBackground;
    End
    Else
    Begin
      If FFlat And Not FMouseOver
       Then FCanvas.PenColor := FBackground
       Else
       Begin
         If FEnabled
          Then FCanvas.PenColor := FForeground
          Else FCanvas.PenColor := DefaultDisabledForeground;
       End;
      Rectangle(X,Y,X + FWidth - 1,Y + FHeight - 1,FBackground);
      I := 0;
      If FEnabled
       Then C := FForeGround
       Else C := DefaultDisabledForeground;
    End;
    Case FKind Of
      cbkCaption:
      Begin
{
        FCanvas.Font.Color := C;
        W := TextWidth(FCaption);
        H := TextHeight('Mg');
        TextOut(X + ((FWidth - W) Div 2) + I,Y + ((FHeight - H) Div 2) + I,FCaption);
}        
      End;
      cbkUpArrow:
      Begin
        FCanvas.PenColor := C;
        W := FWidth Div 2;
        FCanvas.MoveTo(X + W + I,     Y + 3 + I);
        FCanvas.LineToS(X + W + I,     Y + FHeight - 3 + I);
        FCanvas.MoveTo(X + W - 3 + I, Y + 6 + I);
        FCanvas.LineToS(X + W + I,     Y + 3 + I);
        FCanvas.MoveTo(X + W + 3 + I, Y + 6 + I);
        FCanvas.LineToS(X + W + I,     Y + 3 + I);
      End;
      cbkDownArrow:
      Begin
        FCanvas.PenColor := C;
        W := FWidth Div 2;
        FCanvas.MoveTo(X + W + I,     Y + FHeight - 4 + I);
        FCanvas.LineToS(X + W + I,     Y + 2 + I);
        FCanvas.MoveTo(X + W - 3 + I, Y + FHeight - 7 + I);
        FCanvas.LineToS(X + W + I,     Y + FHeight - 4 + I);
        FCanvas.MoveTo(X + W + 3 + I, Y + FHeight - 7 + I);
        FCanvas.LineToS(X + W + I,     Y + FHeight - 4 + I);
      End;
      cbkLeftArrow:
      Begin
        FCanvas.PenColor := C;
        W := FHeight Div 2;
        FCanvas.MoveTo(X + 3 + I,          Y + W + I);
        FCanvas.LineToS(X + FWidth - 3 + I, Y + W + I);
        FCanvas.MoveTo(X + 6 + I,          Y + W - 3 + I);
        FCanvas.LineToS(X + 3 + I,          Y + W + I);
        FCanvas.MoveTo(X + 6 + I,          Y + W + 3 + I);
        FCanvas.LineToS(X + 3 + I,          Y + W + I);
      End;
      cbkRightArrow:
      Begin
        FCanvas.PenColor := C;
        W := FHeight Div 2;
        FCanvas.MoveTo(X + 3 + I,          Y + W + I);
        FCanvas.LineToS(X + FWidth - 4 + I, Y + W + I);
        FCanvas.MoveTo(X + FWidth - 7 + I, Y + W - 3 + I);
        FCanvas.LineToS(X + FWidth - 4 + I, Y + W + I);
        FCanvas.MoveTo(X + FWidth - 7 + I, Y + W + 3 + I);
        FCanvas.LineToS(X + FWidth - 4 + I, Y + W + I);
      End;
      cbkX:
      Begin
        FCanvas.PenColor := C;
        FCanvas.MoveTo(X + 3,Y + 3);
        FCanvas.LineToS(X + FWidth - 3,Y + FHeight - 3);
        FCanvas.MoveTo(X + FWidth - 4,Y + 3);
        FCanvas.LineToS(X + 2,Y + FHeight - 3);
      End;
    End; // Case
  End;
End; // TCanvasButton.Paint

// -------------------------------
// TCanvasEdit
// -------------------------------

Constructor TCanvasEdit.Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; AText: String; AMaxLen: Integer);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,AText,AMaxLen);
  FCanvas := AParent.Canvas;
End; // TCanvasEdit.Create

Function TCanvasEdit.GetCanvas: TJCLBitmap32;
Begin
  Result := FCanvas;
End; // TCanvasEdit.GetCanvas

Procedure TCanvasEdit.FillBackground;
Var X,Y: Integer;
Begin
  If (FCanvas <> Nil) And Not FTransparent Then
  Begin
    CalcAbsoluteXY(X,Y);
    FCanvas.PenColor := FBackground;
    FCanvas.FillRectSC(X,Y,X + Width - 1,Y + Height - 1,FBackground,FClipRect);
  End;
End; // TCanvasEdit.FillBackground

Procedure TCanvasEdit.Paint(X,Y: Integer);
Var
  THeight : Integer;
  I,J     : Integer;

Begin
  Inherited Paint(X,Y);
  If FCanvas <> Nil Then
  Begin
    THeight := TextHeight('Mg');
    If THeight > 0 Then
    Begin
      // Clear the drawing area, possibly with a border

      If FShowBorder
       Then FCanvas.PenColor := FForeground
       Else FCanvas.PenColor := FBackground;
      Rectangle(X,Y,X + FWidth - 1,Y + FHeight - 1,FBackground);

      // Draw the text

      FCanvas.Font.Color := FForeground;
      I := (FHeight - THeight - 2) Div 2;
      TextOut(X + 2,Y + I,FText);

      // Draw the cursor

      If FFocused Then
      Begin
        FCanvas.PenColor := FForeground;
        J := 2 + TextWidth(FText) + 1;
        FCanvas.MoveTo(X + J,Y + I - 1);
        FCanvas.LineToS(X + J,Y + I + THeight + 2);
      End;
    End;
  End;
End; // TCanvasEdit.Paint

// -------------------------------
// TCanvasScrollBar
// -------------------------------

Constructor TCanvasScrollBar.Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TScrollBarKind);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,AKind);
  FCanvas := AParent.Canvas;
End; // TCanvasScrollBar.Create

Constructor TCanvasScrollBar.Create(AParent: ICanvasComponent; ALeft,ATop,ALength: Integer; AKind: TScrollBarKind);
Begin
  Inherited Create(AParent,ALeft,ATop,ALength,AKind);
  FCanvas := AParent.Canvas;
End; // TCanvasScrollBar.Create

Function TCanvasScrollBar.GetCanvas: TJCLBitmap32;
Begin
  Result := FCanvas;
End; // TCanvasScrollBar.GetCanvas

Procedure TCanvasScrollBar.FillBackground;
Var X,Y: Integer;
Begin
  If (FCanvas <> Nil) And Not FTransparent Then
  Begin
    CalcAbsoluteXY(X,Y);
    FCanvas.PenColor := FBackground;
    FCanvas.FillRectSC(X,Y,X + Width - 1,Y + Height - 1,FBackground,FClipRect);
  End;
End; // TCanvasScrollBar.FillBackground

Procedure TCanvasScrollBar.CreateChildren;
Begin
  If FKind = sbHorizontal Then
  Begin
    FULButton := TCanvasButton.Create(Self,1,1,ScrollBarThickness,FHeight - 2,cbkLeftArrow,cbsButton,'');
    FDRButton := TCanvasButton.Create(Self,FWidth - ScrollBarThickness - 1,1,ScrollBarThickness,FHeight - 2,cbkRightArrow,cbsButton,'');
  End
  Else
  Begin
    FULButton := TCanvasButton.Create(Self,1,1,FWidth - 2,ScrollBarThickness,cbkUpArrow,cbsButton,'');
    FDRButton := TCanvasButton.Create(Self,1,FHeight - ScrollBarThickness - 1,FWidth - 2,ScrollBarThickness,cbkDownArrow,cbsButton,'');
  End;
End; // TCanvasScrollBar.CreateChildren;

Procedure TCanvasScrollBar.AfterSetSize;
Begin
  If FKind = sbHorizontal
   Then FDRButton.Left := FWidth - ScrollBarThickness - 1
   Else FDRButton.Top := FHeight - ScrollBarThickness - 1;
End; // TCanvasScrollBar.AfterSetSize

Procedure TCanvasScrollBar.Paint(X,Y: Integer);
Var J,I1,I2: Integer;
Begin
  Inherited Paint(X,Y);
  If FCanvas <> Nil Then
  Begin
    FCanvas.PenColor := FForeground;
    Rectangle(X,Y,X + FWidth - 1,Y + FHeight - 1,FBackground);
    If FKind = sbHorizontal Then
    Begin
      J  := FWidth - 2 - 2 * ScrollBarThickness;
      I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + FPageSize)));
      I2 := FPosition + FPageSize - 1;
      If I2 > FMax + FPageSize - 1 Then I2 := FMax + FPageSize - 1;
      I2 := Round(J * ((FMax + FPageSize - I2 - 1) / (FMax - FMin + FPageSize)));
      Rectangle(X + ScrollBarThickness + 1 + I1,Y + 1,X + J + ScrollBarThickness + 1 - I2 - 1,Y + FHeight - 2,FBackground);
    End
    Else
    Begin
      J  := FHeight - 2 - 2 * ScrollBarThickness;
      I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + FPageSize)));
      I2 := FPosition + FPageSize - 1;
      If I2 > FMax + FPageSize - 1 Then I2 := FMax + FPageSize - 1;
      I2 := Round(J * ((FMax + FPageSize - I2 - 1) / (FMax - FMin + FPageSize)));
      Rectangle(X + 1,Y + ScrollBarThickness + 1 + I1,X + FWidth - 2,Y + J + ScrollBarThickness + 1 - I2 - 1,FBackground);
    End;
  End;
End; // TCanvasScrollBar.Paint

// -------------------------------
// TCanvasTrackBar
// -------------------------------

Constructor TCanvasTrackBar.Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight);
  FCanvas    := AParent.Canvas;
End; // TCanvasTrackBar.Create

Constructor TCanvasTrackBar.Create(AParent: ICanvasComponent; ALeft,ATop,ALength: Integer);
Begin
  Inherited Create(AParent,ALeft,ATop,ALength,ScrollBarThickness);
  FCanvas    := AParent.Canvas;
End; // TCanvasTrackBar.Create

Function TCanvasTrackBar.GetCanvas: TJCLBitmap32;
Begin
  Result := FCanvas;
End; // TCanvasTrackBar.GetCanvas

Procedure TCanvasTrackBar.FillBackground;
Var X,Y: Integer;
Begin
  If (FCanvas <> Nil) And Not FTransparent Then
  Begin
    CalcAbsoluteXY(X,Y);
    FCanvas.PenColor := FBackground;
    FCanvas.FillRectSC(X,Y,X + Width - 1,Y + Height - 1,FBackground,FClipRect);
  End;
End; // TCanvasTrackBar.FillBackground

Procedure TCanvasTrackBar.Paint(X,Y: Integer);
Var J,I1,I2: Integer;
Begin
  Inherited Paint(X,Y);
  If FCanvas <> Nil Then
  Begin
    FCanvas.PenColor := FForeground;
    Rectangle(X,Y + (FHeight Div 2),X + FWidth - 1,Y + (FHeight Div 2) + 1,FBackground);

    J  := FWidth - 2;
    I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + 1)));
    I2 := FPosition;
    If I2 > FMax Then I2 := FMax;
    I2 := Round(J * ((FMax - I2) / (FMax - FMin + 1)));
    Rectangle(X + 1 + I1,Y + 1,X + J - I2,Y + FHeight - 2,FBackground);

  End;
End; // TCanvasTrackBar.Paint

// -------------------------------
// TCanvasListBox
// -------------------------------

Constructor TCanvasListBox.Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; AWordWrapped: Boolean);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,AWordWrapped);
  FCanvas := AParent.Canvas;
End; // TCanvasListBox.Create

Function TCanvasListBox.GetCanvas: TJCLBitmap32;
Begin
  Result := FCanvas;
End; // TCanvasListBox.GetCanvas

Procedure TCanvasListBox.FillBackground;
Var X,Y: Integer;
Begin
  If (FCanvas <> Nil) And Not FTransparent Then
  Begin
    CalcAbsoluteXY(X,Y);
    FCanvas.PenColor := FBackground;
    FCanvas.FillRectSC(X,Y,X + Width - 1,Y + Height - 1,FBackground,FClipRect);
  End;
End; // TCanvasListBox.FillBackground

Procedure TCanvasListBox.CreateChildren;
Begin
  FScrollBar := TCanvasScrollBar.Create(Self,FWidth - ScrollBarThickness,0,ScrollBarThickness,FHeight,sbVertical);
End; // TCanvasListBox.CreateChildren

Procedure TCanvasListBox.Paint(X,Y: Integer);
Var
  I,J,K            : Integer;
  MaxTopIndex      : Integer;
  DisplayableItems : Integer;
  DefaultColor     : TColor;

Begin
  Inherited Paint(X,Y);
  If FCanvas <> Nil Then
  Begin
    FTextHeight := TextHeight('Mg');
    If FTextHeight > 0 Then
    Begin
      // Fix up the top index

      MaxTopIndex := GetMaxTopIndex;
      If FTopIndex > MaxTopIndex Then FTopIndex := MaxTopIndex;

      // Clear the drawing area, possibly with a border

      If FShowBorder
       Then FCanvas.PenColor := FForeground
       Else FCanvas.PenColor := FBackground;

      Rectangle(X,Y,X + FWidth - 1,Y + FHeight - 1,FBackground);

      DisplayableItems := GetDisplayableItems;
      If DisplayableItems * FTextHeight < FHeight Then Inc(DisplayableItems);

      // Paint a selection row

      If (FItemIndex >= FTopIndex) And (FItemIndex < FTopIndex + DisplayableItems) Then
      Begin
        I := FTopIndex;
        K := 0;
        While I < FItemIndex Do
        Begin
          J := Integer(FItems.Objects[I]);
          Inc(K,J Shr 16);
          Inc(I);
        End; // While
        J := Integer(FItems.Objects[FItemIndex]);
        FCanvas.FillRect(X + 1,Y + K * FTextHeight + 2,
                         X + FScrollBar.Left - 1,Y + (K + (J Shr 16)) * FTextHeight + 2,FSelectionBackground);
      End;

      // Paint the text

      I := FTopIndex;
      K := 0;
      While (I < FTopIndex + DisplayableItems) And (I < FItems.Count) Do
      Begin
        If I = FItemIndex
         Then DefaultColor := FSelectionForeground
         Else DefaultColor := FForeground;
        FCanvas.Font.Color := DefaultColor;
        If FWordWrapped Then
        Begin
          GetWordWrappedText(FItems.Strings[I]);
          For J := 0 To FWordWrappedText.Count - 1 Do
          Begin
            OutputText(X + 2,Y + 2 + K * FTextHeight,FWordWrappedText.Strings[J],DefaultColor);
            Inc(K);
          End; // For J
        End
        Else OutputText(X + 2,Y + 2 + (I - FTopIndex) * FTextHeight,FItems.Strings[I],DefaultColor);
        Inc(I);
      End; // While
    End;
  End;
End; // TCanvasListBox.Paint

Procedure TCanvasListBox.OutputText(X,Y: Integer; St: String; DefaultColor: TColor);
// This method relies on the fact that we are using Pascal-style strings, NOT
// null-terminated strings!
Type
  TRGBA = Packed Record
    R,G,B,A: Byte;
  End;

Var
  I,J : Integer;
  C   : TColor;

  Procedure WriteText;
  Begin
    If J > I Then
    Begin
      TextOut(X,Y,Copy(St,I,J - I));
      Inc(X,TextWidth(Copy(St,I,J - I)));
    End;
  End; // WriteText

Begin
  I := 1;
  J := 1;
  FLastColor    := DefaultColor;
  FCurrentColor := DefaultColor;
  While J <= Length(St) Do
  Begin
    If Not (St[J] In [LastColorCommand,SetColorCommand,ResetColorCommand]) Then Inc(J) Else
    Begin
      If St[J] = SetColorCommand Then
      Begin
        WriteText;
        FLastColor := FCurrentColor;
        If J + 1 <= Length(St) Then TRGBA(C).R := Ord(St[J + 1]) Else TRGBA(C).R := 0;
        If J + 2 <= Length(St) Then TRGBA(C).G := Ord(St[J + 2]) Else TRGBA(C).G := 0;
        If J + 3 <= Length(St) Then TRGBA(C).B := Ord(St[J + 3]) Else TRGBA(C).B := 0;
        FCurrentColor      := C;
        FCanvas.Font.Color := FCurrentColor;
        Inc(J,4);
        I := J;
      End
      Else If St[J] = LastColorCommand Then
      Begin
        WriteText;
        FCurrentColor      := FLastColor;
        FCanvas.Font.Color := FCurrentColor;
        Inc(J);
        I := J;
      End
      Else
      Begin
        WriteText;
        FLastColor         := FCurrentColor;
        FCurrentColor      := DefaultColor;
        FCanvas.Font.Color := FCurrentColor;
        Inc(J);
        I := J;
      End;
    End;
  End; // While
  WriteText;
End; // TCanvasListBox.OutputText

Procedure TCanvasListBox.AfterSetSize;
Var MaxTopIndex: Integer;
Begin
  MaxTopIndex := GetMaxTopIndex;
  FScrollBar.Max := MaxTopIndex;
  FScrollBar.PageSize := (FHeight - 4) Div FTextHeight;
End; // TCanvasListBox.AfterSetSize

// -------------------------------
// TCanvasLabel
// -------------------------------

Constructor TCanvasLabel.Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; ATransparent: Boolean);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,ACaption,ATransparent);
  FCanvas   := AParent.Canvas;
  FCaption  := ACaption;
  FAutoSize := True;
  If FCanvas <> Nil Then
  Begin
    Width  := TextWidth(FCaption);
    Height := TextHeight('Mg');
  End;
End; // TCanvasLabel.Create

Constructor TCanvasLabel.Create(AParent: ICanvasComponent; ALeft,ATop: Integer; ACaption: String; ATransparent: Boolean);
Var W,H: Integer;
Begin
  FCanvas := AParent.Canvas;
  Inherited Create(AParent,ALeft,ATop,ACaption,ATransparent);
End; // TCanvasLabel.Create

Function TCanvasLabel.GetCanvas: TJCLBitmap32;
Begin
  Result := FCanvas;
End; // TCanvasLabel.GetCanvas

Procedure TCanvasLabel.FillBackground;
Var X,Y: Integer;
Begin
  If (FCanvas <> Nil) And Not FTransparent Then
  Begin
    CalcAbsoluteXY(X,Y);
    FCanvas.PenColor := FBackground;
    FCanvas.FillRectSC(X,Y,X + Width - 1,Y + Height - 1,FBackground,FClipRect);
  End;
End; // TCanvasLabel.FillBackground

Procedure TCanvasLabel.Paint(X,Y: Integer);
Var
  I,J,K : Integer;
  St    : String;
  
Begin
  Inherited Paint(X,Y);
  If FCanvas <> Nil Then
  Begin
    FCanvas.Font.Color  := FForeground;
    If FWordWrapped Then
    Begin
      J := 0;
      For I := 0 To FWordWrappedText.Count - 1 Do J := Max(J,TextWidth(FWordWrappedText.Strings[I]));
      K := 0;
      For I := 0 To FWordWrappedText.Count - 1 Do
      Begin
        St := FWordWrappedText.Strings[I];
        Case Alignment Of
           taLeftJustify: TextOut(X,Y + K,St);
          taRightJustify: TextOut(X + Width - TextWidth(St),Y + K,St);
                taCenter: TextOut(X + (Width - TextWidth(St)) Div 2,Y + K,St);
        End; // Case
        Inc(K,TextHeight('Mg'));
      End; // For I
    End
    Else
    Begin
      Case Alignment Of
         taLeftJustify: TextOut(X,Y,FCaption);
        taRightJustify: TextOut(X + Width - TextWidth(FCaption),Y,FCaption);
              taCenter: TextOut(X + (Width - TextWidth(FCaption)) Div 2,Y,FCaption);
      End; // Case
    End;
  End;
End; // TCanvasLabel.Paint

Procedure TCanvasLabel.DoAutoSize;
Begin
  If FCanvas <> Nil Then
  Begin
    Width  := Parent.TextWidth(Caption);
    Height := Parent.TextHeight('Mg');
  End
  Else
  Begin
    Width  := 0;
    Height := 0;
  End;
End; // TAbstractLabel.DoAutoSize

// -------------------------------
// TCanvasPanel
// -------------------------------

Constructor TCanvasPanel.Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,ACaption);
  FCanvas  := AParent.Canvas;
End; // TCanvasPanel.Create

Function TCanvasPanel.GetCanvas: TJCLBitmap32;
Begin
  Result := FCanvas;
End; // TCanvasPanel.GetCanvas

Procedure TCanvasPanel.FillBackground;
Var X,Y: Integer;
Begin
  If (FCanvas <> Nil) And Not FTransparent Then
  Begin
    CalcAbsoluteXY(X,Y);
    FCanvas.PenColor := FBackground;
    FCanvas.FillRectSC(X,Y,X + Width - 1,Y + Height - 1,FBackground,FClipRect);
  End;
End; // TCanvasPanel.FillBackground

Procedure TCanvasPanel.CreateChildren;
Begin
  FLabel := TCanvasLabel.Create(Self,0,0,'');
End; // TCanvasPanel.CreateChildren

Procedure TCanvasPanel.Paint(X,Y: Integer);
Begin
  Inherited Paint(X,Y);
  If FShowBorder Then
  Begin
    FCanvas.PenColor := FForeground;
    If FTransparent
     Then FCanvas.FrameRectSC(X,Y,X + FWidth - 1,Y + FHeight - 1,FCanvas.PenColor,FClipRect)
     Else Rectangle(X,Y,X + FWidth - 1,Y + FHeight - 1,FBackground);
  End;
End; // TCanvasPanel.Paint

// -------------------------------
// TCanvasProgressBar
// -------------------------------

Constructor TCanvasProgressBar.Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer; AValue: Single; AColor: TColor);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,AValue,AColor);
  FCanvas := AParent.Canvas;
End; // TCanvasProgressBar.Create

Function TCanvasProgressBar.GetCanvas: TJCLBitmap32;
Begin
  Result := FCanvas;
End; // TCanvasProgressBar.GetCanvas

Procedure TCanvasProgressBar.FillBackground;
Var X,Y: Integer;
Begin
  If (FCanvas <> Nil) And Not FTransparent Then
  Begin
    CalcAbsoluteXY(X,Y);
    FCanvas.PenColor := FBackground;
    FCanvas.FillRectSC(X,Y,X + Width - 1,Y + Height - 1,FBackground,FClipRect);
  End;
End; // TCanvasProgressBar.FillBackground

Procedure TCanvasProgressBar.Paint(X,Y: Integer);
Var I: Integer;
Begin
  Inherited Paint(X,Y);
  FCanvas.PenColor := FForeground;
  Rectangle(X,Y,X + FWidth - 1,Y + FHeight - 1,FBackground);
  I := Trunc(Max(0,Value) * (FWidth - 2));
  If I > 0 Then
  Begin
    FCanvas.PenColor := FColor;
    Rectangle(X + 1,Y + 1,X + I - 1,Y + FHeight - 2,FColor);
  End;
End; // TCanvasProgressBar.Paint

// -------------------------------
// TCanvasScrollPane
// -------------------------------

Constructor TCanvasScrollPane.Create(AParent: ICanvasComponent; ALeft,ATop,AWidth,AHeight: Integer);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight);
  FCanvas := AParent.Canvas;
End; // TCanvasScrollPane.Create

Function TCanvasScrollPane.GetCanvas: TJCLBitmap32;
Begin
  Result := FCanvas;
End; // TCanvasScrollPane.GetCanvas

Procedure TCanvasScrollPane.FillBackground;
Var X,Y: Integer;
Begin
  If (FCanvas <> Nil) And Not FTransparent Then
  Begin
    CalcAbsoluteXY(X,Y);
    FCanvas.PenColor := FBackground;
    FCanvas.FillRectSC(X,Y,X + Width - 1,Y + Height - 1,FBackground,FClipRect);
  End;
End; // TCanvasScrollPane.FillBackground

Procedure TCanvasScrollPane.CreateChildren;
Begin
  FVertScrollBar          := Nil;
  FHorzScrollBar          := Nil;
  FVertScrollBar          := TCanvasScrollBar.Create(Self,0,0,ScrollBarThickness,50,sbVertical);
  FHorzScrollBar          := TCanvasScrollBar.Create(Self,0,0,50,ScrollBarThickness,sbHorizontal);
  FVertScrollBar.Min      := 0;
  FHorzScrollBar.Min      := 0;
  FVertScrollBar.OnChange := VertScrollBarChange;
  FHorzScrollBar.OnChange := HorzScrollBarChange;
  AfterSetSize;
End; // TCanvasScrollPane.CreateChildren

// -------------------------------
// TCanvasWindow
// -------------------------------

Constructor TCanvasWindow.Create(ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; AUI: TAbstractUI);
Begin
  FCanvas              := TJCLBitmap32.Create;
  FCanvas.SetSize(AWidth,AHeight);
  Inherited Create(ALeft,ATop,AWidth,AHeight,ACaption,AUI);
  FClipQueue           := TList.Create;
End; // TCanvasWindow.Create

Destructor TCanvasWindow.Destroy;
Begin
  FCanvas.Free;
  FClipQueue.Free;
  Inherited;
End; // TCanvasWindow.Destroy

Function TCanvasWindow.GetCanvas: TJCLBitmap32;
Begin
  Result := FCanvas;
End; // TCanvasWindow.GetCanvas

Procedure TCanvasWindow.FillBackground;
Var X,Y: Integer;
Begin
  If (FCanvas <> Nil) And Not FTransparent Then
  Begin
    CalcAbsoluteXY(X,Y);
    FCanvas.PenColor := FBackground;
    FCanvas.FillRectSC(X,Y,X + Width - 1,Y + Height - 1,FBackground,FClipRect);
  End;
End; // TCanvasWindow.FillBackground

Procedure TCanvasWindow.CreateCloseButton;
Begin
  FCloseButton := TCanvasButton.Create(Self,2,2,11,11,cbkX,cbsButton,'');
End; // TCanvasWindow.CreateCloseButton

Procedure TCanvasWindow.AfterSetSize;
Begin
  If (FWidth <> FCanvas.Width) Or (FHeight <> FCanvas.Height) Then
  Begin
    FCanvas.Free;
    FCanvas             := TJCLBitmap32.Create;
    FCanvas.SetSize(FWidth,FHeight);
  End;
End; // TCanvasWindow.AfterSetSize

Procedure TCanvasWindow.CalcClientExtents;
Var Offset: Integer;
Begin
  If (FCanvas <> Nil) And FShowCaption Then Offset := TextHeight('Mg') + 2 + 2 * FBorderWidth
  Else If FShowBorder Then Offset := FBorderWidth
  Else Offset := 0;
  If FShowBorder Then
  Begin
    FClientLeft   := FBorderWidth;
    FClientTop    := Offset;
    FClientWidth  := FWidth - 2 * FBorderWidth;
    FClientHeight := FHeight - Offset - FBorderWidth;
  End
  Else
  Begin
    FClientLeft   := 0;
    FClientTop    := Offset;
    FClientWidth  := FWidth;
    FClientHeight := FHeight - Offset;
  End;
End; // TCanvasWindow.CalcClientExtents

Function TCanvasWindow.GetChildClipRect(ChildComponent: TAbstractComponent): TRect;
Var
  X,Y   : Integer;
  Rect  : TRect;
  Rect1 : TRect;

Begin
  If ChildComponent = FCloseButton Then
  Begin
    CalcAbsoluteXY(X,Y);
    Rect.Left   := X;
    Rect.Top    := Y;
    Rect.Right  := Rect.Left + FWidth     - 1;
    Rect.Bottom := Rect.Top  + FClientTop - 2;
    If FShowBorder Then
    Begin
      Inc(Rect.Left);
      Inc(Rect.Top);
      Dec(Rect.Right);
    End;
    If FParent <> Nil Then
    Begin
      Rect1       := FParent.GetChildClipRect(Self);
      Rect.Left   := Max(Rect.Left,   Rect1.Left);
      Rect.Top    := Max(Rect.Top,    Rect1.Top);
      Rect.Right  := Min(Rect.Right,  Rect1.Right);
      Rect.Bottom := Min(Rect.Bottom, Rect1.Bottom);
    End;
  End
  Else Rect := Inherited GetChildClipRect(ChildComponent);
  Result := Rect;
End; // TCanvasWindow.GetChildClipRect

Procedure TCanvasWindow.Paint(X,Y: Integer);
Var Offset,CW: Integer;
Begin
  Inherited Paint(X,Y);
  If FCanvas <> Nil Then
  Begin
    Offset := FClientTop - 1;

    If FShowCaption Then
    Begin
      FCloseButton.Top    := -FClientTop + 3;
      FCloseButton.Width  := Offset - 5;
      FCloseButton.Height := Offset - 5;

      // Draw the caption box

      FCanvas.PenColor := FForeground;
      If FTransparent
       Then FCanvas.FrameRectSC(X,Y,X + FWidth - 1,Y + Offset,FCanvas.PenColor,FClipRect)
       Else Rectangle(X,Y,X + FWidth - 1,Y + Offset,FBackground);

      // Draw the caption

      FCanvas.Font.Color := FForeground;
      CW := TextWidth(FCaption);
      TextOut(X + ((FWidth - CW) Div 2),Y + 2,FCaption);
    End;

    // Draw a border around the rest of the panel

    If FShowBorder Then
    Begin
      FCanvas.PenColor := FForeground;
      If FTransparent
       Then FCanvas.FrameRectSC(X,Y + Offset,X + FWidth - 1,Y + FHeight - 1,FCanvas.PenColor,FClipRect)
       Else Rectangle(X,Y + Offset,X + FWidth - 1,Y + FHeight - 1,FBackground);
    End;
  End;
End; // TCanvasWindow.Paint

Function TCanvasWindow.TextWidth(Const St: String): Integer;
Begin
  Result := FCanvas.TextWidth(St);
End; // TCanvasWindow.TextWidth

Function TCanvasWindow.TextHeight(Const St: String): Integer;
Begin
  Result := FCanvas.TextHeight(St);
End; // TCanvasWindow.TextHeight

Procedure TCanvasWindow.TextOut(X,Y: Integer; Const St: String);
Begin
  FCanvas.TextOut(X,Y,St);
End; // TCanvasWindow.TextOut

Procedure TCanvasWindow.BeginClip(Rect: TRect);
Var MyRgn: HRGN;
Begin
  MyRgn := CreateRectRgn(Rect.Left,Rect.Top,Rect.Right + 1,Rect.Bottom + 1);
  SelectClipRgn(FCanvas.Handle,MyRgn);
  FClipQueue.Add(Pointer(MyRgn));
End; // TCanvasWindow.BeginClip

Procedure TCanvasWindow.EndClip;
Begin
  If FClipQueue.Count > 0 Then FClipQueue.Delete(FClipQueue.Count - 1);
  If FClipQueue.Count > 0
   Then SelectClipRgn(FCanvas.Handle,HRGN(FClipQueue.Items[FClipQueue.Count - 1]))
   Else SelectClipRgn(FCanvas.Handle,0);
End; // TCanvasWindow.EndClip

// -------------------------------
// TCanvasUI
// -------------------------------

Constructor TCanvasUI.Create;
Begin
  Inherited;
End; // TCanvasUI.Create

Destructor TCanvasUI.Destroy;
Begin
  Inherited;
End; // TCanvasUI.Destroy

Function TCanvasUI.AddWindow(ACaption: String): TAbstractWindow;
Var Window: TCanvasWindow;
Begin
  Window     := TCanvasWindow.Create(0,0,64,64,ACaption,Self);
  Window.FUI := Self;
  Try
    FWindowsMutex.Enter;
    FWindows.AddObject('',Window);
    Window.ZOrder := FWindows.Count - 1;
  Finally
    FWindowsMutex.Leave;
  End;
  Result := Window;
End; // TCanvasUI.AddWindow

end.

