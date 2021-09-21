unit ClearTypeText;

interface

Uses Windows,Messages,Graphics,Classes,Controls,StdCtrls,ExtCtrls,Grids,SysUtils;

{$R ClearTypeText.res}

Const
  WM_UPDCT = WM_USER + 2; {this message is called by hook procedure when
                             ClearType control should be updated  }


Type
  PBGRA = ^TBGRA;
  TBGRA = Packed Record
    B,G,R,A: Byte;
  End;
  TPublicFontControl = Class(TControl)
  Public
    Property Font;
  End;
  TBuffer = Record
    BMP    : HBITMAP;
    Width  : Integer;
    Height : Integer;
    Buffer : Pointer;
  End;

  TClearTypeLink = Class;

  TClearTypeFlavor = (ctfRGB,ctfBGR);
  TClearTypeText = Class(TComponent)
  Protected
    FSrcBMP       : TBitmap;
    FDstBMP       : TBitmap;
    FWinXP        : Boolean;
    FFlavor       : TClearTypeFlavor;
    FMyFont       : TFont;
    FWidth        : Single;
    FEnabled      : Boolean;
    FClientLinks  : TStringList; // List of TClearTypeLink
    FForceOnXP    : Boolean;
    FActiveFont   : TFont;
    FInactiveFont : TFont;
    Procedure   ClearizeScanLine(Src,Dst: PBGRA; Width: Integer);
    Procedure   SetWidth(AWidth: Single);
    Procedure   SetEnabled(B: Boolean);
    Procedure   SetForceOnXP(B: Boolean);
    Procedure   SetMyFont(Font: TFont);
    Procedure   SetFlavor(AFlavor: TClearTypeFlavor);
    Procedure   InvalidateClients;
    Procedure   SetActiveFont(Value: TFont);
    Procedure   SetInactiveFont(Value: TFont);
    Procedure   SetClientFonts;
    Procedure   ActiveFontOnChange(Sender: TObject);
    Procedure   InactiveFontOnChange(Sender: TObject);
    Procedure   SetClientFont(Client: TPublicFontControl);
    Procedure   GetTextExtents(Font: TFont; St: String; Var CX,CY: Integer);
    Procedure   InitBuffer(Var Buffer: TBitmap; W,H: Integer);
    Procedure   ResizeBuffer(Var Buffer: TBitmap; W,H: Integer);
    Procedure   KillBuffer(Var Buffer: TBitmap);
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
    Procedure   TextOut(X,Y: Integer; St: String; Canvas: TCanvas; BackColor: TColor);
    Procedure   ExtTextOut(X,Y: Integer; St: String; Options: LongInt; Canvas: TCanvas; BackColor: TColor; Rect: PRect);
    Procedure   DrawText(X,Y: Integer; St: String; Flags: LongInt; Canvas: TCanvas; BackColor: TColor; Rect: PRect; Opaque: Boolean);
    Procedure   DrawTextDC(X,Y: Integer; St: String; Flags: LongInt; DC: HDC; Font: TFont; ForeColor,BackColor: TColor; Rect: PRect; Opaque: Boolean);
    Procedure   DrawTextDCSel(X,Y: Integer; St: String; Flags: LongInt; DC: HDC; Font: TFont; SelLo,SelHi: Integer; ForeColor,BackColor,SelForeColor,SelBackColor: TColor; Rect: PRect; Opaque: Boolean);
    Procedure   AddClientLink(Link: TClearTypeLink);
    Procedure   RemoveClientLink(Link: TClearTypeLink; ResetFont: Boolean);
    Property    IsXP      : Boolean          Read FWinXP;
  Published
    Property    Flavor       : TClearTypeFlavor Read FFlavor       Write SetFlavor Default ctfRGB;
    Property    Width        : Single           Read FWidth        Write SetWidth;
    Property    Enabled      : Boolean          Read FEnabled      Write SetEnabled Default True;
    Property    ForceOnXP    : Boolean          Read FForceOnXP    Write SetForceOnXP Default False;
    Property    ActiveFont   : TFont            Read FActiveFont   Write SetActiveFont;
    Property    InactiveFont : TFont            Read FInactiveFont Write SetInactiveFont;
  End;
  TClearTypeLink = Class
  Protected
    FClearType : TClearTypeText;
    FClient    : TPublicFontControl;
    Procedure   SetClearType(AClearType: TClearTypeText);
  Public
    Constructor Create(AClient: TControl);
    Destructor  Destroy; Override;
    Property    Client    : TPublicFontControl Read FClient;
    Property    ClearType : TClearTypeText     Read FClearType Write SetClearType;
  End;
  TClearTypeLabel = Class(TLabel)
  Protected
    FClearTypeLink : TClearTypeLink;
    Function    GetClearType: TClearTypeText;
    Procedure   SetClearType(AClearType: TClearTypeText);
    Procedure   DoDrawText(Var Rect: TRect; Flags: Longint); Override;
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
  Published
    Property    ClearType: TClearTypeText Read GetClearType Write SetClearType Default Nil;
  End;
  TClearTypeCheckBox = Class(TCheckBox)
  Protected
    FClearTypeLink : TClearTypeLink;
    Function    GetClearType: TClearTypeText;
    Procedure   SetClearType(AClearType: TClearTypeText);
    Procedure   InternalPaint;
    Procedure   WMPaint(Var Msg: TWMPaint); Message WM_PAINT;
    Procedure   WMMove(Var Msg: TMessage); Message WM_MOVE;
    Procedure   WMSize(Var Msg: TMessage); Message WM_SIZE;
    Procedure   WMEnable(Var Msg: TMessage); Message WM_ENABLE;
    Procedure   WMUpdateCT(Var Msg: TMessage); Message WM_UPDCT;
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
  Published
    Property    ClearType: TClearTypeText Read GetClearType Write SetClearType Default Nil;
  End;
  TClearTypeRadioButton = Class(TRadioButton)
  Protected
    FClearTypeLink : TClearTypeLink;
    Function    GetClearType: TClearTypeText;
    Procedure   SetClearType(AClearType: TClearTypeText);
    Procedure   InternalPaint;
    Procedure   WMPaint(Var Msg: TWMPaint); Message WM_PAINT;
    Procedure   WMMove(Var Msg: TMessage); Message WM_MOVE;
    Procedure   WMSize(Var Msg: TMessage); Message WM_SIZE;
    Procedure   WMEnable(Var Msg: TMessage); Message WM_ENABLE;
    Procedure   WMUpdateCT(Var Msg: TMessage); Message WM_UPDCT;
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
  Published
    Property    ClearType: TClearTypeText Read GetClearType Write SetClearType Default Nil;
  End;
  TClearTypeGroupBox = Class(TGroupBox)
  Protected
    FClearTypeLink : TClearTypeLink;
    Function    GetClearType: TClearTypeText;
    Procedure   SetClearType(AClearType: TClearTypeText);
    Procedure   Paint; Override;
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
  Published
    Property    ClearType: TClearTypeText Read GetClearType Write SetClearType Default Nil;
  End;
  TClearTypeListBox = Class(TListBox)
  Protected
    FClearTypeLink : TClearTypeLink;
    Function    GetClearType: TClearTypeText;
    Procedure   SetClearType(AClearType: TClearTypeText);
    Procedure   DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); Override;
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
  Published
    Property    ClearType: TClearTypeText Read GetClearType Write SetClearType Default Nil;
  End;
  TClearTypeStringGrid = Class(TStringGrid)
  Protected
    FClearTypeLink : TClearTypeLink;
    Function    GetClearType: TClearTypeText;
    Procedure   SetClearType(AClearType: TClearTypeText);
    Procedure   DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); Override;
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
  Published
    Property    ClearType: TClearTypeText Read GetClearType Write SetClearType Default Nil;
  End;
  TClearTypeEdit = Class(TEdit)
  Protected
    FClearTypeLink : TClearTypeLink;
    FPainting      : Boolean;
    FLastCaretX    : Integer;
    Function    GetClearType: TClearTypeText;
    Procedure   SetClearType(AClearType: TClearTypeText);
    Procedure   InternalPaint(ReadCaret: Boolean);
    Procedure   WMPaste(Var Msg: TWMPaste); Message WM_PASTE;
    Procedure   WMKeyDown(Var Msg: TWMSysKeyDown); Message WM_KEYDOWN;
    Procedure   WMLButtonDown(Var Msg: TWMLButtonDown); Message WM_LBUTTONDOWN;
    Procedure   WMLButtonUp(Var Msg: TWMLButtonUp); Message WM_LBUTTONUP;
    Procedure   WMLButtonDblClk(Var Msg: TWMLButtonDblClk); Message WM_LBUTTONDBLCLK;
    Procedure   WMMouseMove(Var Msg: TWMMouseMove); Message WM_MOUSEMOVE;
    Procedure   WMChar(Var Msg: TWMChar); Message WM_Char;
    Procedure   WMPaint(Var Msg: TWMPaint); Message WM_PAINT;
    Procedure   WMMove(Var Msg: TMessage); Message WM_MOVE;
    Procedure   WMSize(Var Msg: TMessage); Message WM_SIZE;
    Procedure   WMEnable(Var Msg: TMessage); Message WM_ENABLE;
    Procedure   WMUpdateCT(Var Msg: TMessage); Message WM_UPDCT;
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
  Published
    Property    ClearType: TClearTypeText Read GetClearType Write SetClearType Default Nil;
  End;

  Procedure Register;

implementation

Uses Math;

Var
  WHook            : HHook;
  Hooks            : TList;
  ClearTypeClasses : TList;
  Lookup           : Array[0..256*12-1] Of Byte;

Type TCWPStruct = Packed record
    lParam: LPARAM;
    wParam: WPARAM;
    message: integer;
    wnd: HWND;
  End;

procedure Register;
begin
  RegisterComponents('CT2k', [TClearTypeText,TClearTypeLabel,TClearTypeEdit,
                              TClearTypeCheckBox,TClearTypeRadioButton,
                              TClearTypeListBox,TClearTypeGroupBox,TClearTypeStringGrid]);
end;

Procedure SetLookup;
Var I: Integer;
Begin
  For I := 0 To High(Lookup) Do Lookup[I] := Min(255,Round(I / 12));
End;

Function IsClearTypeControl(W: TWinControl): Boolean;
Var I: Integer;
Begin
  Result := True;
  For i := 0 to ClearTypeClasses.Count - 1 do
    If W is TWinControlClass(ClearTypeClasses.Items[i]) then
    Begin
      exit;
    End;
  Result := False;
End;

Procedure RegisterClearTypeControl(W: TWinControlClass);
Begin
  ClearTypeClasses.Add(W);
End;

Procedure AliasScanLine(Src,Dst: Pointer; Width: Integer; Flavor: TClearTypeFlavor);
Var R,G,B: Integer;
Begin
  Asm
    PUSHAD
    MOV   ECX,DWORD PTR Width
    TEST  ECX,ECX
    JZ    @Abort
    MOV   ESI,DWORD PTR Src
    MOV   EDI,DWORD PTR Dst
    SUB   EBX,EBX
    SUB   EDX,EDX
@ProcessLoop:
    // Red

    MOVZX EBX,BYTE PTR [ESI + 2]
    SUB   EAX,EAX
    SHL   EBX,2
    CMP   ECX,DWORD PTR Width
    JE    @FirstRed
    MOV   DL,[ESI - 2]
    MOV   AL,[ESI - 6]
    ADD   EAX,EDX
    ADD   EAX,EDX
    ADD   EAX,EDX
    JMP   @ContinueRed
@FirstRed:
    ADD   EAX,EBX
@ContinueRed:
    ADD   EAX,EBX
    MOV   DL,[ESI + 6]
    ADD   EAX,EDX
    ADD   EAX,EDX
    ADD   EAX,EDX
    MOV   DL,[ESI + 10]
    ADD   EAX,EDX
    MOV   DWORD PTR R,EAX

    // Green

    SUB   EAX,EAX
    CMP   ECX,DWORD PTR Width
    JE    @FirstGreen
    MOV   AL,[ESI - 3]
    JMP   @ContinueGreen
@FirstGreen:
    MOV   AL,[ESI + 1]
@ContinueGreen:
    MOV   DL,[ESI + 1]
    ADD   EAX,EDX
    ADD   EAX,EDX
    ADD   EAX,EDX
    MOVZX EBX,BYTE PTR [ESI + 5]
    SHL   EBX,2
    ADD   EAX,EBX
    MOV   DL,[ESI + 9]
    ADD   EAX,EDX
    ADD   EAX,EDX
    ADD   EAX,EDX
    CMP   ECX,1
    JE    @LastGreen
    MOV   DL,[ESI + 13]
    JMP   @DoneGreen
@LastGreen:
//    MOV   DL,[ESI + 9] // Redundant
@DoneGreen:
    ADD   EAX,EDX
    MOV   DWORD PTR G,EAX

    // Blue

    MOVZX EAX,BYTE PTR [ESI + 8]
    MOV   DL,[ESI]
    SHL   EAX,2
    ADD   EAX,EDX
    MOV   DL,[ESI + 4]
    ADD   EAX,EDX
    ADD   EAX,EDX
    ADD   EAX,EDX
    CMP   ECX,1
    JNE   @NotLastBlue
    MOVZX EBX,BYTE PTR [ESI + 8]
    SHL   EBX,2
    ADD   EAX,EBX
    JMP   @DoneBlue
@NotLastBlue:
    MOV   DL,BYTE PTR [ESI + 12]
    ADD   EAX,EDX
    ADD   EAX,EDX
    ADD   EAX,EDX
    MOV   DL,[ESI + 16]
    ADD   EAX,EDX
@DoneBlue:
    MOV   DWORD PTR B,EAX

    // Output

    CMP   BYTE PTR Flavor,ctfRGB
    JNE   @BGR

    MOV   EAX,DWORD PTR R
    MOV   DL,BYTE PTR Lookup[EAX]
    MOV   [EDI + 2],DL
    MOV   EAX,DWORD PTR G
    MOV   DL,BYTE PTR Lookup[EAX]
    MOV   [EDI + 1],DL
    MOV   EAX,DWORD PTR B
    MOV   DL,BYTE PTR Lookup[EAX]
    MOV   [EDI + 0],DL
    JMP   @NextColor
@BGR:
    MOV   EAX,DWORD PTR R
    MOV   DL,BYTE PTR Lookup[EAX]
    MOV   [EDI + 0],DL
    MOV   EAX,DWORD PTR G
    MOV   DL,BYTE PTR Lookup[EAX]
    MOV   [EDI + 1],DL
    MOV   EAX,DWORD PTR B
    MOV   DL,BYTE PTR Lookup[EAX]
    MOV   [EDI + 2],DL
@NextColor:
    MOV   DL,0FFh
    MOV   [EDI + 3],DL

    ADD   ESI,12
    ADD   EDI,4

    DEC   ECX
    JNZ   @ProcessLoop
@Abort:
    POPAD
  End;
End;

Function CallWndProcHook(nCode: integer; wParam: Longint; Var Msg: TCWPStruct): longint; stdcall;
Var
  i  : integer;
  r  : TRect;
  r2 : TRect;
  c  : TWinControl;

  Function IsPaintMsg: boolean;
  Begin
    With TWinControl(hooks[i]) do
    Begin
      result := false;
      If not HandleAllocated then exit;
      If C = Owner then
      Begin
        If (msg.message = WM_MOVE) then exit;
        Result := True;
        exit;
      End;
      If C.Owner = Owner then
      Begin
        GetWindowRect(msg.wnd, r);
        GetWindowRect(handle, r2);
        result := IntersectRect(r, r, r2);
      End;
    End;
  End;

Begin
  Result := CallNextHookEx(WHook, nCode, wParam, Longint(@Msg));
  If ((msg.message > CN_BASE) and (msg.message < CN_BASE + 500)) or
    (msg.message = WM_PAINT) or (msg.message = WM_SIZE)
    Or (msg.message = WM_MOVE)
    Then
  Begin
    c := FindControl(msg.wnd);
    If (c = Nil) or (IsClearTypeControl(c)) then exit;
    For i := 0 to hooks.Count - 1 do
    Begin
      If (IsPaintMsg) then
        SendMessage(TWinControl(hooks[i]).Handle, WM_UPDCT, 0, 0);
    End;
  End;
End;

Procedure AddHook(o: TWinControl);
Var
  i: integer;
Begin
  If hooks.Count = 0 then
    WHook := SetWindowsHookEx(WH_CALLWNDPROC, @CallWndProcHook, 0, GetCurrentThreadId);
  For i := 0 to Hooks.Count - 1 do
    If Hooks.Items[i] = o then exit;
  hooks.Add(o);
End;

Procedure RemoveHook(o: TWinControl);
Begin
  Hooks.Remove(o);
  If Hooks.Count = 0 then UnHookWindowsHookEx(WHook);
End;

// TClearTypeText

Constructor TClearTypeText.Create(AOwner: TComponent);
Var Version: Integer;
Begin
  Inherited Create(AOwner);
  InitBuffer(FSrcBMP,1024,128);
  InitBuffer(FDstBMP,1024,128);
  Version                := GetVersion;
  FWinXP                 := ((Version And $80000000) = 0) And ((Version And $FF) >= 5) And (((Version Shr 8) And $FF) >= 1);
  FMyFont                := TFont.Create;
  FFlavor                := ctfRGB;
  FWidth                 := 1.33;
  FEnabled               := True;
  FForceOnXP             := False;
  FClientLinks           := TStringList.Create;
  FActiveFont            := TFont.Create;
  FInactiveFont          := TFont.Create;
  FActiveFont.Name       := 'Arial';
  FInactiveFont.Name     := 'Microsoft Sans Serif';
  FActiveFont.Size       := 8;
  FInactiveFont.Size     := 8;
  FActiveFont.OnChange   := ActiveFontOnChange;
  FInactiveFont.OnChange := InactiveFontOnChange;
End;

Destructor TClearTypeText.Destroy;
Var I: Integer;
Begin
  For I := FClientLinks.Count - 1 DownTo 0 Do RemoveClientLink(TClearTypeLink(FClientLinks.Objects[I]),True);
  KillBuffer(FSrcBMP);
  KillBuffer(FDstBMP);
  FMyFont.Free;
  FClientLinks.Free;
  FActiveFont.Free;
  FInactiveFont.Free;
  inherited Destroy;
End;

Procedure TClearTypeText.InitBuffer(Var Buffer: TBitmap; W,H: Integer);
Begin
  Buffer             := TBitmap.Create;
  Buffer.Width       := W;
  Buffer.Height      := H;
  Buffer.PixelFormat := pf32Bit;
End;

Procedure TClearTypeText.ResizeBuffer(Var Buffer: TBitmap; W,H: Integer);
Var W0,H0: Integer;
Begin
  If (W > Buffer.Width) Or (H > Buffer.Height) Then
  Begin
    W0 := Buffer.Width;
    H0 := Buffer.Height;
    Buffer.Free;
    InitBuffer(Buffer,Max(W0,W),Max(H0,H));
  End;
End;

Procedure TClearTypeText.KillBuffer(Var Buffer: TBitmap);
Begin
  Buffer.Free;
End;

Procedure TClearTypeText.GetTextExtents(Font: TFont; St: String; Var CX,CY: Integer);
Var I: Integer;
Begin
  SetMyFont(Font);
  FSrcBMP.Canvas.Font.Assign(FMyFont);
  I  := FSrcBMP.Canvas.TextWidth(St);
  CX := I Div 3;
  If CX * 3 < I Then Inc(CX);
  CY := FSrcBMP.Canvas.TextHeight(St);
End;

Procedure TClearTypeText.TextOut(X,Y: Integer; St: String; Canvas: TCanvas; BackColor: TColor);
Var
  WX,WY : Integer;
  WCT   : Integer;
  I     : Integer;
  C     : TColor;
  R     : TRect;

Begin
  If FEnabled And (FForceOnXP Or Not IsXP) Then
  Begin
    GetTextExtents(Canvas.Font,St,WX,WY);
    If (FSrcBMP.Width <> WX) Or (FSrcBMP.Height <> WY) Then ResizeBuffer(FSrcBMP,WX,WY);

    WCT := WX Div 3;
    If WCT * 3 < WX Then Inc(WCT);

    If (FDstBMP.Width <> WCT) Or (FDstBMP.Height <> WY) Then ResizeBuffer(FDstBMP,WCT,WY);

    C := ColorToRGB(BackColor);

    FSrcBMP.Canvas.Brush.Color := C;
    FSrcBMP.Canvas.Brush.Style := bsSolid;
    R.Left   := 0;
    R.Top    := 0;
    R.Right  := FSrcBMP.Width;
    R.Bottom := FSrcBMP.Height;
    FSrcBMP.Canvas.FillRect(R);

    FDstBMP.Canvas.Brush.Color := C;
    FDstBMP.Canvas.Brush.Style := bsSolid;
    R.Left   := 0;
    R.Top    := 0;
    R.Right  := FDstBMP.Width;
    R.Bottom := FDstBMP.Height;
    FDstBMP.Canvas.FillRect(R);

    FSrcBMP.Canvas.Font.Assign(FMyFont);
    FSrcBMP.Canvas.Font.Color := Canvas.Font.Color;
    FSrcBMP.Canvas.Brush.Style := bsClear;
    FSrcBMP.Canvas.TextOut(0,0,St);

    For I := 0 To WY - 1 Do ClearizeScanLine(FSrcBMP.ScanLine[I],FDstBMP.ScanLine[I],WCT);
    Canvas.Draw(X,Y,FDstBMP);
  End
  Else
  Begin
    Windows.TextOut(Canvas.Handle,X,Y,PChar(St), Length(St));
  End;
End;

Procedure TClearTypeText.ExtTextOut(X,Y: Integer; St: String; Options: LongInt; Canvas: TCanvas; BackColor: TColor; Rect: PRect);
Var
  WX,WY : Integer;
  WCT   : Integer;
  I     : Integer;
  C     : TColor;
  R     : TRect;
  R0    : TRect;

Begin
  If FEnabled And (FForceOnXP Or Not IsXP) Then
  Begin
    GetTextExtents(Canvas.Font,St,WX,WY);
    If Rect <> Nil Then
    Begin
      R0 := Rect^;
      If WX < (R0.Right - Min(X,R0.Left)) * 3 Then WX := (R0.Right - Min(X,R0.Left)) * 3;
      If WY < R0.Bottom - Min(Y,R0.Top)       Then WY := R0.Bottom - Min(Y,R0.Top);
    End;

    If (FSrcBMP.Width < WX) Or (FSrcBMP.Height < WY) Then ResizeBuffer(FSrcBMP,WX,WY);

    WCT := WX Div 3;
    If WCT * 3 < WX Then Inc(WCT);

    If R0.Right - R0.Left < WCT Then R0.Right := R0.Left + WCT;

    If (FDstBMP.Width < WCT) Or (FDstBMP.Height < WY) Then ResizeBuffer(FDstBMP,WCT,WY);

    C := ColorToRGB(BackColor);

    If (Options And ETO_OPAQUE) <> 0 Then
    Begin
      FSrcBMP.Canvas.Brush.Color := C;
      FSrcBMP.Canvas.Brush.Style := bsSolid;
      R.Left   := 0;
      R.Top    := 0;
      R.Right  := FSrcBMP.Width;
      R.Bottom := FSrcBMP.Height;
      FSrcBMP.Canvas.FillRect(R);
    End
    Else
    Begin
      If Rect <> Nil
       Then StretchBlt(FSrcBMP.Canvas.Handle,0,0,WX,WY,Canvas.Handle,R0.Left,R0.Top,R0.Right - R0.Left,R0.Bottom - R0.Top,SRCCOPY)
       Else StretchBlt(FSrcBMP.Canvas.Handle,0,0,WX,WY,Canvas.Handle,X,Y,FDstBMP.Width,FDstBMP.Height,SRCCOPY);
    End;

    FDstBMP.Canvas.Brush.Color := C;
    FDstBMP.Canvas.Brush.Style := bsSolid;
    R.Left   := 0;
    R.Top    := 0;
    R.Right  := FDstBMP.Width;
    R.Bottom := FDstBMP.Height;
    FDstBMP.Canvas.FillRect(R);

    FSrcBMP.Canvas.Font.Assign(FMyFont);
    FSrcBMP.Canvas.Font.Color  := Canvas.Font.Color;
    FSrcBMP.Canvas.Brush.Style := bsClear;
    If Rect <> Nil
     Then FSrcBMP.Canvas.TextOut((X - R0.Left) * 3,Y - R0.Top,St)
     Else FSrcBMP.Canvas.TextOut(0,0,St);

    For I := 0 To WY - 1 Do
    Begin
      ClearizeScanLine(FSrcBMP.ScanLine[I],FDstBMP.ScanLine[I],WCT);
    End; // For I
    If Rect <> Nil Then
    Begin
      If (Options And ETO_CLIPPED) <> 0
       Then BitBlt(Canvas.Handle,R0.Left,R0.Top,R0.Right - R0.Left,R0.Bottom - R0.Top,FDstBMP.Canvas.Handle,0,0,SRCCOPY)
       Else BitBlt(Canvas.Handle,X,Y,FDstBMP.Width,FDstBMP.Height,FDstBMP.Canvas.Handle,X - R0.Left,Y - R0.Top,SRCCOPY);
    End
    Else BitBlt(Canvas.Handle,X,Y,FDstBMP.Width,FDstBMP.Height,FDstBMP.Canvas.Handle,0,0,SRCCOPY);
  End
  Else
  Begin
    Windows.ExtTextOut(Canvas.Handle, X, Y, Options, Rect, PChar(St), Length(St), Nil);
  End;
End;

Procedure TClearTypeText.DrawText(X,Y: Integer; St: String; Flags: LongInt; Canvas: TCanvas; BackColor: TColor; Rect: PRect; Opaque: Boolean);
Var
  WX,WY : Integer;
  WCT   : Integer;
  I     : Integer;
  C     : TColor;
  R     : TRect;
  R0    : TRect;

Begin
  If FEnabled And (FForceOnXP Or Not IsXP) And ((Flags And DT_CALCRECT) = 0) Then
  Begin
    GetTextExtents(Canvas.Font,St,WX,WY);
    If Rect <> Nil Then
    Begin
      R0 := Rect^;
      If WX < (R0.Right - Min(X,R0.Left)) * 3 Then WX := (R0.Right - Min(X,R0.Left)) * 3;
      If WY < R0.Bottom - Min(Y,R0.Top)       Then WY := R0.Bottom - Min(Y,R0.Top);
    End;

    If (FSrcBMP.Width < WX) Or (FSrcBMP.Height < WY) Then ResizeBuffer(FSrcBMP,WX,WY);

    WCT := WX Div 3;
    If WCT * 3 < WX Then Inc(WCT);

    If R0.Right - R0.Left < WCT Then R0.Right := R0.Left + WCT;

    If (FDstBMP.Width < WCT) Or (FDstBMP.Height < WY) Then ResizeBuffer(FDstBMP,WCT,WY);

    C := ColorToRGB(BackColor);

    If Opaque Then
    Begin
      FSrcBMP.Canvas.Brush.Color := C;
      FSrcBMP.Canvas.Brush.Style := bsSolid;
      R.Left   := 0;
      R.Top    := 0;
      R.Right  := FSrcBMP.Width;
      R.Bottom := FSrcBMP.Height;
      FSrcBMP.Canvas.FillRect(R);
    End
    Else
    Begin
      If Rect <> Nil
       Then StretchBlt(FSrcBMP.Canvas.Handle,0,0,WX,WY,Canvas.Handle,R0.Left,R0.Top,R0.Right - R0.Left,R0.Bottom - R0.Top,SRCCOPY)
       Else StretchBlt(FSrcBMP.Canvas.Handle,0,0,WX,WY,Canvas.Handle,X,Y,FDstBMP.Width,FDstBMP.Height,SRCCOPY);
    End;

    FDstBMP.Canvas.Brush.Color := C;
    FDstBMP.Canvas.Brush.Style := bsSolid;
    R.Left   := 0;
    R.Top    := 0;
    R.Right  := FDstBMP.Width;
    R.Bottom := FDstBMP.Height;
    FDstBMP.Canvas.FillRect(R);

    FSrcBMP.Canvas.Font.Assign(FMyFont);
    FSrcBMP.Canvas.Font.Color  := Canvas.Font.Color;
    FSrcBMP.Canvas.Brush.Style := bsClear;
    If Rect <> Nil Then
    Begin
      R.Left   := (X - R0.Left) * 3;
      R.Top    := Y - R0.Top;
      R.Right  := R.Left + WX;
      R.Bottom := R.Top + WY;
      Windows.DrawText(FSrcBMP.Canvas.Handle,PChar(St),Length(St),R,Flags);
    End
    Else FSrcBMP.Canvas.TextOut(0,0,St);
    For I := 0 To WY - 1 Do ClearizeScanLine(FSrcBMP.ScanLine[I],FDstBMP.ScanLine[I],WCT);
    If Rect <> Nil Then
    Begin
      If (Flags And DT_NOCLIP) = 0
       Then BitBlt(Canvas.Handle,R0.Left,R0.Top,R0.Right - R0.Left,R0.Bottom - R0.Top,FDstBMP.Canvas.Handle,0,0,SRCCOPY)
       Else BitBlt(Canvas.Handle,X,Y,FDstBMP.Width,FDstBMP.Height,FDstBMP.Canvas.Handle,X - R0.Left,Y - R0.Top,SRCCOPY);
    End
    Else BitBlt(Canvas.Handle,X,Y,FDstBMP.Width,FDstBMP.Height,FDstBMP.Canvas.Handle,0,0,SRCCOPY);
  End
  Else
  Begin
    Windows.DrawText(Canvas.Handle, PChar(St), Length(St), Rect^, Flags);
  End;
End;

Procedure TClearTypeText.DrawTextDC(X,Y: Integer; St: String; Flags: LongInt; DC: HDC; Font: TFont; ForeColor,BackColor: TColor; Rect: PRect; Opaque: Boolean);
Var
  WX,WY : Integer;
  WCT   : Integer;
  I     : Integer;
  C     : TColor;
  R     : TRect;
  R0    : TRect;
  R1    : TRect;

Begin
  If FEnabled And (FForceOnXP Or Not IsXP) And ((Flags And DT_CALCRECT) = 0) Then
  Begin
    GetTextExtents(Font,St,WX,WY);
    If Rect <> Nil Then
    Begin
      R0 := Rect^;
      If WX < (R0.Right - Min(X,R0.Left)) * 3 Then WX := (R0.Right - Min(X,R0.Left)) * 3;
      If WY < R0.Bottom - Min(Y,R0.Top)       Then WY := R0.Bottom - Min(Y,R0.Top);
    End;

    If (FSrcBMP.Width < WX) Or (FSrcBMP.Height < WY) Then ResizeBuffer(FSrcBMP,WX,WY);

    WCT := WX Div 3;
    If WCT * 3 < WX Then Inc(WCT);

    R1 := R0;

    If R0.Right - R0.Left < WCT Then R0.Right  := R0.Left + WCT;
    If R0.Bottom - R0.Top < WY  Then R0.Bottom := R0.Top + WY;

    If R1.Right - R1.Left < WX Then R1.Right  := R1.Left + WX;
    If R1.Bottom - R1.Top < WY Then R1.Bottom := R1.Top + WY;

    If (FDstBMP.Width < WCT) Or (FDstBMP.Height < WY) Then ResizeBuffer(FDstBMP,WCT,WY);

    C := ColorToRGB(BackColor);

    If Opaque Then
    Begin
      FSrcBMP.Canvas.Brush.Color := C;
      FSrcBMP.Canvas.Brush.Style := bsSolid;
      R.Left   := 0;
      R.Top    := 0;
      R.Right  := FSrcBMP.Width;
      R.Bottom := FSrcBMP.Height;
      FSrcBMP.Canvas.FillRect(R);
    End
    Else
    Begin
      If Rect <> Nil
       Then StretchBlt(FSrcBMP.Canvas.Handle,0,0,WX,WY,DC,R0.Left,R0.Top,R0.Right - R0.Left,R0.Bottom - R0.Top,SRCCOPY)
       Else StretchBlt(FSrcBMP.Canvas.Handle,0,0,WX,WY,DC,X,Y,WCT,WY,SRCCOPY);
    End;

    FDstBMP.Canvas.Brush.Color := C;
    FDstBMP.Canvas.Brush.Style := bsSolid;
    R.Left   := 0;
    R.Top    := 0;
    R.Right  := FDstBMP.Width;
    R.Bottom := FDstBMP.Height;
    FDstBMP.Canvas.FillRect(R);

    FSrcBMP.Canvas.Font.Assign(FMyFont);
    FSrcBMP.Canvas.Font.Color  := ForeColor;
    FSrcBMP.Canvas.Brush.Style := bsClear;
    If Rect <> Nil Then
    Begin
      R.Left   := (X - R0.Left) * 3;
      R.Top    := Y - R0.Top;
      R.Right  := R.Left + WX;
      R.Bottom := R.Top + WY;
      Windows.DrawText(FSrcBMP.Canvas.Handle,PChar(St),Length(St),R,Flags);
    End
    Else FSrcBMP.Canvas.TextOut(0,0,St);
    For I := 0 To WY - 1 Do ClearizeScanLine(FSrcBMP.ScanLine[I],FDstBMP.ScanLine[I],WCT);
    If Rect <> Nil Then
    Begin
      If (Flags And DT_NOCLIP) = 0
       Then BitBlt(DC,R0.Left,R0.Top,R0.Right - R0.Left,R0.Bottom - R0.Top,FDstBMP.Canvas.Handle,0,0,SRCCOPY)
       Else BitBlt(DC,X,Y,WCT,WY,FDstBMP.Canvas.Handle,X - R0.Left,Y - R0.Top,SRCCOPY);
    End
    Else BitBlt(DC,X,Y,WCT,WY,FDstBMP.Canvas.Handle,0,0,SRCCOPY);
  End
  Else
  Begin
    Windows.DrawText(DC, PChar(St), Length(St), Rect^, Flags);
  End;
End;

Procedure TClearTypeText.DrawTextDCSel(X,Y: Integer; St: String; Flags: LongInt; DC: HDC; Font: TFont; SelLo,SelHi: Integer; ForeColor,BackColor,SelForeColor,SelBackColor: TColor; Rect: PRect; Opaque: Boolean);
Var
  WX,WY : Integer;
  WCT   : Integer;
  I     : Integer;
  C     : TColor;
  C1    : TColor;
  R     : TRect;
  R0    : TRect;
  R1    : TRect;
  St1   : String;
  St2   : String;
  St3   : String;
  W1    : Integer;
  W2    : Integer;
  W3    : Integer;

Begin
  If FEnabled And (FForceOnXP Or Not IsXP) And ((Flags And DT_CALCRECT) = 0) Then
  Begin
    GetTextExtents(Font,St,WX,WY);
    If Rect <> Nil Then
    Begin
      R0 := Rect^;
      If WX < (R0.Right - Min(X,R0.Left)) * 3 Then WX := (R0.Right - Min(X,R0.Left)) * 3;
      If WY < R0.Bottom - Min(Y,R0.Top)       Then WY := R0.Bottom - Min(Y,R0.Top);
    End;

    If (FSrcBMP.Width < WX) Or (FSrcBMP.Height < WY) Then ResizeBuffer(FSrcBMP,WX,WY);

    WCT := WX Div 3;
    If WCT * 3 < WX Then Inc(WCT);

    R1 := R0;

    If R0.Right - R0.Left < WCT Then R0.Right  := R0.Left + WCT;
    If R0.Bottom - R0.Top < WY  Then R0.Bottom := R0.Top + WY;

    If R1.Right - R1.Left < WX Then R1.Right  := R1.Left + WX;
    If R1.Bottom - R1.Top < WY Then R1.Bottom := R1.Top + WY;

    If (FDstBMP.Width < WCT) Or (FDstBMP.Height < WY) Then ResizeBuffer(FDstBMP,WCT,WY);

    C  := ColorToRGB(BackColor);
    C1 := ColorToRGB(SelBackColor);

    If Opaque Then
    Begin
      FSrcBMP.Canvas.Brush.Color := C;
      FSrcBMP.Canvas.Brush.Style := bsSolid;
      R.Left   := 0;
      R.Top    := 0;
      R.Right  := FSrcBMP.Width;
      R.Bottom := FSrcBMP.Height;
      FSrcBMP.Canvas.FillRect(R);
    End
    Else
    Begin
      If Rect <> Nil
       Then StretchBlt(FSrcBMP.Canvas.Handle,0,0,WX,WY,DC,R0.Left,R0.Top,R0.Right - R0.Left,R0.Bottom - R0.Top,SRCCOPY)
       Else StretchBlt(FSrcBMP.Canvas.Handle,0,0,WX,WY,DC,X,Y,WCT,WY,SRCCOPY);
    End;

    FDstBMP.Canvas.Brush.Color := C;
    FDstBMP.Canvas.Brush.Style := bsSolid;
    R.Left   := 0;
    R.Top    := 0;
    R.Right  := FDstBMP.Width;
    R.Bottom := FDstBMP.Height;
    FDstBMP.Canvas.FillRect(R);

    FSrcBMP.Canvas.Font.Assign(FMyFont);
    FSrcBMP.Canvas.Font.Color  := ForeColor;
    FSrcBMP.Canvas.Brush.Style := bsClear;
    If Rect <> Nil Then
    Begin
      If SelLo >= SelHi Then
      Begin
        R.Left   := (X - R0.Left) * 3;
        R.Top    := Y - R0.Top;
        R.Right  := R.Left + WX;
        R.Bottom := R.Top + WY;
        Windows.DrawText(FSrcBMP.Canvas.Handle,PChar(St),Length(St),R,Flags);
      End
      Else
      Begin
        St1 := Copy(St,1,SelLo);
        St2 := Copy(St,SelLo + 1,SelHi - SelLo);
        St3 := Copy(St,SelHi + 1,Length(St));
        W1  := FSrcBMP.Canvas.TextWidth(St1);
        W2  := FSrcBMP.Canvas.TextWidth(St2);
        W3  := FSrcBMP.Canvas.TextWidth(St3);

        If St1 <> '' Then
        Begin
          R.Left   := (X - R0.Left) * 3;
          R.Top    := Y - R0.Top;
          R.Right  := R.Left + W1;
          R.Bottom := R.Top + WY;
          FSrcBMP.Canvas.Brush.Color := C;
          FSrcBMP.Canvas.Brush.Style := bsSolid;
          FSrcBMP.Canvas.FillRect(R);
          FSrcBMP.Canvas.Font.Color  := ForeColor;
          FSrcBMP.Canvas.Brush.Style := bsClear;
          Windows.DrawText(FSrcBMP.Canvas.Handle,PChar(St1),Length(St1),R,Flags);
        End;

        If St2 <> '' Then
        Begin
          R.Left   := (X - R0.Left) * 3 + W1;
          R.Top    := Y - R0.Top;
          R.Right  := R.Left + W2;
          R.Bottom := R.Top + WY;
          FSrcBMP.Canvas.Brush.Color := C1;
          FSrcBMP.Canvas.Brush.Style := bsSolid;
          FSrcBMP.Canvas.FillRect(R);
          FSrcBMP.Canvas.Font.Color  := SelForeColor;
          FSrcBMP.Canvas.Brush.Style := bsClear;
          Windows.DrawText(FSrcBMP.Canvas.Handle,PChar(St2),Length(St2),R,Flags);
        End;

        If St3 <> '' Then
        Begin
          R.Left   := (X - R0.Left) * 3 + W1 + W2;
          R.Top    := Y - R0.Top;
          R.Right  := R.Left + W3;
          R.Bottom := R.Top + WY;
          FSrcBMP.Canvas.Brush.Color := C;
          FSrcBMP.Canvas.Brush.Style := bsSolid;
          FSrcBMP.Canvas.FillRect(R);
          FSrcBMP.Canvas.Font.Color  := ForeColor;
          FSrcBMP.Canvas.Brush.Style := bsClear;
          Windows.DrawText(FSrcBMP.Canvas.Handle,PChar(St3),Length(St3),R,Flags);
        End;
      End;
    End
    Else FSrcBMP.Canvas.TextOut(0,0,St);
    For I := 0 To WY - 1 Do ClearizeScanLine(FSrcBMP.ScanLine[I],FDstBMP.ScanLine[I],WCT);
    If Rect <> Nil Then
    Begin
      If (Flags And DT_NOCLIP) = 0
       Then BitBlt(DC,R0.Left,R0.Top,R0.Right - R0.Left,R0.Bottom - R0.Top,FDstBMP.Canvas.Handle,0,0,SRCCOPY)
       Else BitBlt(DC,X,Y,WCT,WY,FDstBMP.Canvas.Handle,X - R0.Left,Y - R0.Top,SRCCOPY);
    End
    Else BitBlt(DC,X,Y,WCT,WY,FDstBMP.Canvas.Handle,0,0,SRCCOPY);
  End
  Else
  Begin
    Windows.DrawText(DC, PChar(St), Length(St), Rect^, Flags);
  End;
End;

Procedure TClearTypeText.ClearizeScanLine(Src,Dst: PBGRA; Width: Integer);
Var
  I      : Integer;
  R,G,B  : Integer;
  Src0   : PBGRA;

Begin
  AliasScanLine(Src,Dst,Width,FFlavor);
{
  I    := 0;
  Src0 := Src;
  While I < Width Do
  Begin
    // do red

    If I = 0
     Then R := 4 * Src0.R
     Else R := PBGRA(LongWord(Src) - 2 * 4).R + 3 * PBGRA(LongWord(Src) - 1 * 4).R;
    Inc(R,4 * Src.R + 3 * PBGRA(LongWord(Src) + 1 * 4).R + PBGRA(LongWord(Src) + 2 * 4).R);

    // do green

    If I = 0
     Then G := Src0.G
     Else G := PBGRA(LongWord(Src) - 1 * 4).G;
    Inc(G,3 * Src.G + 4 * PBGRA(LongWord(Src) + 1 * 4).G + 3 * PBGRA(LongWord(Src) + 2 * 4).G);
    If I = Width - 1
     Then Inc(G,PBGRA(LongWord(Src) + 2 * 4).G)
     Else Inc(G,PBGRA(LongWord(Src) + 3 * 4).G);

    // do blue

    B := Src.B + 3 * PBGRA(LongWord(Src) + 1 * 4).B + 4 * PBGRA(LongWord(Src) + 2 * 4).B;
    If I = Width - 1
     Then Inc(B,4 * PBGRA(LongWord(Src) + 2 * 4).B)
     Else Inc(B,3 * PBGRA(LongWord(Src) + 3 * 4).B + PBGRA(LongWord(Src) + 4 * 4).B);

    If FFlavor = ctfRGB Then
    Begin
      Dst.R := Lookup[R];//Round(R / 12);
      Dst.G := Lookup[G];//Round(G / 12);
      Dst.B := Lookup[B];//Round(B / 12);
    End
    Else
    Begin
      Dst.B := Lookup[R];//Round(R / 12);
      Dst.G := Lookup[G];//Round(G / 12);
      Dst.R := Lookup[B];//Round(B / 12);
    End;

    Inc(LongWord(Src),4 * 3);
    Inc(LongWord(Dst),4);

    Inc(I);
  End; // While
}
End;

Procedure TClearTypeText.SetMyFont(Font: TFont);
Var LF: TLogFont;
Begin
  Try
    FMyFont.Assign(Font);
    GetObject(FMyFont.Handle, SizeOf(LF), @LF);
    LF.lfEscapement  := 0;
    LF.lfOrientation := 0;
    LF.lfWidth       := Round(lf.lfHeight * FWidth);
    FMyFont.Handle   := CreateFontIndirect(LF);
  Finally
  End;
End;

Procedure TClearTypeText.SetWidth(AWidth: Single);
Begin
  If AWidth <> FWidth Then
  Begin
    FWidth := AWidth;
    InvalidateClients;
  End;
End;

Procedure TClearTypeText.SetEnabled(B: Boolean);
Begin
  If B <> FEnabled Then
  Begin
    FEnabled := B;
    SetClientFonts;
    InvalidateClients;
  End;
End;

Procedure TClearTypeText.SetForceOnXP(B: Boolean);
Begin
  If B <> FForceOnXP Then
  Begin
    FForceOnXP := B;
    InvalidateClients;
  End;
End;

Procedure TClearTypeText.SetFlavor(AFlavor: TClearTypeFlavor);
Begin
  If FFlavor <> AFlavor Then
  Begin
    FFlavor := AFlavor;
    InvalidateClients;
  End;
End;

Procedure TClearTypeText.AddClientLink(Link: TClearTypeLink);
Begin
  FClientLinks.AddObject('',Link);
  SetClientFont(Link.Client);
End;

Procedure TClearTypeText.RemoveClientLink(Link: TClearTypeLink; ResetFont: Boolean);
Var I: Integer;
Begin
  If Link <> Nil Then
  Begin
    If ResetFont Then
    Begin
      Link.Client.Font.Name := FInactiveFont.Name;
      Link.Client.Font.Size := FInactiveFont.Size;
    End;
    Link.FClearType := Nil;
  End;
  I := FClientLinks.IndexOfObject(Link);
  If I >= 0 Then FClientLinks.Delete(I);
End;

Procedure TClearTypeText.SetClientFont(Client: TPublicFontControl);
Var Font: TFont;
Begin
  If FEnabled
   Then Font := FActiveFont
   Else Font := FInactiveFont;
  Client.Font.Name := Font.Name;
  Client.Font.Size := Font.Size;
End;

Procedure TClearTypeText.InvalidateClients;
Var I: Integer;
Begin
  For I := 0 To FClientLinks.Count - 1 Do TClearTypeLink(FClientLinks.Objects[I]).Client.Invalidate;
End;

Procedure TClearTypeText.SetActiveFont(Value: TFont);
Begin
  FActiveFont.Assign(Value);
End;

Procedure TClearTypeText.SetInactiveFont(Value: TFont);
Begin
  FInactiveFont.Assign(Value);
End;

Procedure TClearTypeText.SetClientFonts;
Var I: Integer;
Begin
  For I := 0 To FClientLinks.Count - 1 Do SetClientFont(TClearTypeLink(FClientLinks.Objects[I]).Client);
End;

Procedure TClearTypeText.ActiveFontOnChange(Sender: TObject);
Begin
  SetClientFonts;
End;

Procedure TClearTypeText.InactiveFontOnChange(Sender: TObject);
Begin
  SetClientFonts;
End;

// TClearTypeLink

Constructor TClearTypeLink.Create(AClient: TControl);
Begin
  FClient    := TPublicFontControl(AClient);
  FClearType := Nil;
End;

Destructor TClearTypeLink.Destroy;
Begin
  If FClearType <> Nil Then FClearType.RemoveClientLink(Self,False);
  FClearType := Nil;
End;

Procedure TClearTypeLink.SetClearType(AClearType: TClearTypeText);
Begin
  If FClearType <> Nil Then FClearType.RemoveClientLink(Self,True);
  FClearType := AClearType;
  If FClearType <> Nil Then FClearType.AddClientLink(Self);
End;

// TClearTypeLabel

Constructor TClearTypeLabel.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FClearTypeLink := TClearTypeLink.Create(Self);
End;

Destructor TClearTypeLabel.Destroy;
Begin
  FClearTypeLink.Free;
  Inherited;
End;

Procedure TClearTypeLabel.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If (FClearTypeLink <> Nil) And
     (FClearTypeLink.ClearType <> Nil) And
     (AComponent = FClearTypeLink.ClearType) And
     (Operation = opRemove) Then FClearTypeLink.ClearType := Nil;
  Inherited Notification(AComponent,Operation);
End;

Function TClearTypeLabel.GetClearType: TClearTypeText;
Begin
  Result := FClearTypeLink.ClearType;
End;

Procedure TClearTypeLabel.SetClearType(AClearType: TClearTypeText);
Begin
  FClearTypeLink.ClearType := AClearType;
  Invalidate;
End;

Procedure TClearTypeLabel.DoDrawText(Var Rect: TRect; Flags: LongInt);
Var
  Text : String;
  Opt  : Integer;
  BC   : TColor;
  Size : TSize;

Begin
  Text := GetLabelText;
  If (Flags And DT_CALCRECT <> 0) And ((Text = '') Or ShowAccelChar And
    (Text[1] = '&') And (Text[2] = #0)) Then Text := Text + ' ';
  If Not ShowAccelChar Then Flags := Flags Or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  If (Flags And DT_CALCRECT) <> 0 Then
  Begin
    If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then
    Begin
      FClearTypeLink.ClearType.GetTextExtents(Font,Caption,Size.CX,Size.CY);
      Rect.Right  := Rect.Left + Size.CX;
      Rect.Bottom := Rect.Top + Size.CY;
    End
    Else DrawText(Canvas.Handle,PChar(Text),Length(Text),Rect,Flags);
  End
  Else
  Begin
    Opt := ETO_CLIPPED;
    If (Canvas.Brush.Style = bsSolid) Or Not Transparent Then
    Begin
      Opt := Opt Or ETO_OPAQUE;
      BC  := Color;
    End
    Else BC := Canvas.Brush.Color;
    If Not Enabled Then
    Begin
      OffsetRect(Rect, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
      If FClearTypeLink.ClearType <> Nil
       Then FClearTypeLink.ClearType.DrawText(Rect.Left,Rect.Top,Text,Flags,Canvas,BC,@Rect,(Opt And ETO_OPAQUE) <> 0)
       Else DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
      OffsetRect(Rect, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      If FClearTypeLink.ClearType <> Nil
       Then FClearTypeLink.ClearType.DrawText(Rect.Left,Rect.Top,Text,Flags,Canvas,BC,@Rect,(Opt And ETO_OPAQUE) <> 0)
       Else DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
    End
    Else
    Begin
      If FClearTypeLink.ClearType <> Nil
       Then FClearTypeLink.ClearType.DrawText(Rect.Left,Rect.Top,Text,Flags,Canvas,BC,@Rect,(Opt And ETO_OPAQUE) <> 0)
       Else DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
    End;
  End;
End;

// TClearTypeCheckBox

Constructor TClearTypeCheckBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FClearTypeLink := TClearTypeLink.Create(Self);
  ControlStyle := ControlStyle - [csopaque];
  AddHook(Self);
End;

Destructor TClearTypeCheckBox.Destroy;
Begin
  RemoveHook(Self);
  FClearTypeLink.Free;
  Inherited;
End;

Procedure TClearTypeCheckBox.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If (FClearTypeLink <> Nil) And
     (FClearTypeLink.ClearType <> Nil) And
     (AComponent = FClearTypeLink.ClearType) And
     (Operation = opRemove) Then FClearTypeLink.ClearType := Nil;
  Inherited Notification(AComponent,Operation);
End;

Function TClearTypeCheckBox.GetClearType: TClearTypeText;
Begin
  Result := FClearTypeLink.ClearType;
End;

Procedure TClearTypeCheckBox.SetClearType(AClearType: TClearTypeText);
Begin
  FClearTypeLink.ClearType := AClearType;
  Invalidate;
End;

Procedure TClearTypeCheckBox.WMMove(Var Msg: TMessage);
Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint;
  Inherited;
End;

Procedure TClearTypeCheckBox.WMSize(Var Msg: TMessage);
Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then
  Begin
    Inherited;
    WMMove(Msg);
  End
  Else Inherited;
End;

Procedure TClearTypeCheckBox.WMEnable(Var Msg: TMessage);
Begin
  Inherited;
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint;
End;

Procedure TClearTypeCheckBox.WMPaint(Var Msg: TWMPaint);
Var
  PS : TPaintStruct;
  R  : TRect;

Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then
  Begin
    GetUpdateRect(Handle, R, False);
    If Not IsRectEmpty(R) Then
    Begin
      BeginPaint(Handle, PS);
      Msg.Result := 0;
      InternalPaint;
      EndPaint(Handle, PS);
    End;
  End
  Else Inherited;
End;

Procedure TClearTypeCheckBox.InternalPaint;
Var
  DC   : HDC;
  R    : TRect;
  Size : TSize;
  Text : String;
  FTempDC : HDC;
  FTempBitmap : HBITMAP;
  FOldBitmap  : HBITMAP;

Begin
  If (Not HandleAllocated) Or Not Visible Then Exit;

  DC := GetDC(Handle);

  FTempDC     := CreateCompatibleDC(DC);
  FTempBitmap := CreateCompatibleBitmap(DC, Width, Height);
  FOldBitmap  := SelectObject(FTempDC, FTempBitmap);

  R.Left   := 0;
  R.Top    := 0;
  R.Right  := Width;
  R.Bottom := Height;
  PaintWindow(FTempDC);
  Size.cx := 0;
  Size.cy := 0;
  Text    := Caption;
  Windows.GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);
  If Alignment = taLeftJustify Then
  Begin
    FClearTypeLink.ClearType.DrawTextDC(0,((Height - Size.cy) Div 2) + 1,Text,0,DC,Font,Font.Color,Color,@R,True);
    BitBlt(DC,Width - 14,0,14,Height,FTempDC,Width - 14,0,SRCCOPY);
  End
  Else
  Begin
    FClearTypeLink.ClearType.DrawTextDC(18,((Height - Size.cy) Div 2) + 1,Text,0,DC,Font,Font.Color,Color,@R,True);
    BitBlt(DC,0,0,14,Height,FTempDC,0,0,SRCCOPY);
  End;

  SelectObject(FTempDC, FOldBitmap);
  DeleteObject(FTempBitmap);

  ReleaseDC(Handle,DC);
  DeleteDC(FTempDC);
End;

Procedure TClearTypeCheckBox.WMUpdateCT(Var Msg: TMessage);
Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint;
End;

// TClearTypeRadioButton

Constructor TClearTypeRadioButton.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FClearTypeLink := TClearTypeLink.Create(Self);
  ControlStyle   := ControlStyle - [csopaque];
  AddHook(Self);
End;

Destructor TClearTypeRadioButton.Destroy;
Begin
  RemoveHook(Self);
  FClearTypeLink.Free;
  Inherited;
End;

Procedure TClearTypeRadioButton.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If (FClearTypeLink <> Nil) And
     (FClearTypeLink.ClearType <> Nil) And
     (AComponent = FClearTypeLink.ClearType) And
     (Operation = opRemove) Then FClearTypeLink.ClearType := Nil;
  Inherited Notification(AComponent,Operation);
End;

Function TClearTypeRadioButton.GetClearType: TClearTypeText;
Begin
  Result := FClearTypeLink.ClearType;
End;

Procedure TClearTypeRadioButton.SetClearType(AClearType: TClearTypeText);
Begin
  FClearTypeLink.ClearType := AClearType;
  Invalidate;
End;

Procedure TClearTypeRadioButton.WMMove(Var Msg: TMessage);
Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint;
  Inherited;
End;

Procedure TClearTypeRadioButton.WMSize(Var Msg: TMessage);
Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then
  Begin
    Inherited;
    WMMove(Msg);
  End
  Else Inherited;
End;

Procedure TClearTypeRadioButton.WMEnable(Var Msg: TMessage);
Begin
  Inherited;
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint;
End;

Procedure TClearTypeRadioButton.WMPaint(Var Msg: TWMPaint);
Var
  PS : TPaintStruct;
  R  : TRect;

Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then
  Begin
    GetUpdateRect(Handle, R, False);
    If Not IsRectEmpty(R) Then
    Begin
      BeginPaint(Handle, PS);
      Msg.Result := 0;
      InternalPaint;
      EndPaint(Handle, PS);
    End;
  End
  Else Inherited;
End;

Procedure TClearTypeRadioButton.InternalPaint;
Var
  DC   : HDC;
  R    : TRect;
  Size : TSize;
  Text : String;
  FTempDC : HDC;
  FTempBitmap : HBITMAP;
  FOldBitmap  : HBITMAP;

Begin
  If (Not HandleAllocated) Or Not Visible Then Exit;

  DC := GetDC(Handle);

  FTempDC     := CreateCompatibleDC(DC);
  FTempBitmap := CreateCompatibleBitmap(DC, Width, Height);
  FOldBitmap  := SelectObject(FTempDC, FTempBitmap);

  R.Left   := 0;
  R.Top    := 0;
  R.Right  := Width;
  R.Bottom := Height;
  PaintWindow(FTempDC);
  Size.cx := 0;
  Size.cy := 0;
  Text    := Caption;
  Windows.GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);
  If Alignment = taLeftJustify Then
  Begin
    FClearTypeLink.ClearType.DrawTextDC(0,((Height - Size.cy) Div 2) + 1,Text,0,DC,Font,Font.Color,Color,@R,True);
    BitBlt(DC,Width - 14,0,14,Height,FTempDC,Width - 14,0,SRCCOPY);
  End
  Else
  Begin
    FClearTypeLink.ClearType.DrawTextDC(18,((Height - Size.cy) Div 2) + 1,Text,0,DC,Font,Font.Color,Color,@R,True);
    BitBlt(DC,0,0,14,Height,FTempDC,0,0,SRCCOPY);
  End;

  SelectObject(FTempDC, FOldBitmap);
  DeleteObject(FTempBitmap);

  ReleaseDC(Handle,DC);
  DeleteDC(FTempDC);
End;

Procedure TClearTypeRadioButton.WMUpdateCT(Var Msg: TMessage);
Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint;
End;

// TClearTypeGroupBox

Constructor TClearTypeGroupBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FClearTypeLink := TClearTypeLink.Create(Self);
End;

Destructor TClearTypeGroupBox.Destroy;
Begin
  FClearTypeLink.Free;
  Inherited;
End;

Procedure TClearTypeGroupBox.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If (FClearTypeLink <> Nil) And
     (FClearTypeLink.ClearType <> Nil) And
     (AComponent = FClearTypeLink.ClearType) And
     (Operation = opRemove) Then FClearTypeLink.ClearType := Nil;
  Inherited Notification(AComponent,Operation);
End;

Function TClearTypeGroupBox.GetClearType: TClearTypeText;
Begin
  Result := FClearTypeLink.ClearType;
End;

Procedure TClearTypeGroupBox.SetClearType(AClearType: TClearTypeText);
Begin
  FClearTypeLink.ClearType := AClearType;
  Invalidate;
End;

Procedure TClearTypeGroupBox.Paint;
Var
  R    : TRect;
  Size : TSize;
  H    : Integer;

Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then
  Begin
    Canvas.Font := Self.Font;
    H := Canvas.TextHeight('0');
    R := Rect(0,H Div 2 - 1, Width, Height);
    If Ctl3D Then
    Begin
      Inc(R.Left);
      Inc(R.Top);
      Canvas.Brush.Color := clBtnHighlight;
      Canvas.FrameRect(R);
      OffsetRect(R, -1, -1);
      Canvas.Brush.Color := clBtnShadow;
    End
    Else Canvas.Brush.Color := clWindowFrame;
    Canvas.FrameRect(R);

    Size.cx  := 0;
    Size.cy  := 0;
    Text     := Caption;
    ClearType.GetTextExtents(Font,Caption,Size.CX,Size.CY);
    R.Left   := 8;
    R.Top    := 0;
    R.Right  := Size.cx + 8;
    R.Bottom := Size.cy;

    If UseRightToLeftAlignment
     Then FClearTypeLink.ClearType.DrawTextDC(Width - Size.CX - 8,0,Text,0,Canvas.Handle,Font,Font.Color,Color,@R,True)
     Else FClearTypeLink.ClearType.DrawTextDC(8,0,Text,0,Canvas.Handle,Font,Font.Color,Color,@R,True);
  End
  Else Inherited;
End;

// TClearTypeListBox

Constructor TClearTypeListBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FClearTypeLink := TClearTypeLink.Create(Self);
End;

Destructor TClearTypeListBox.Destroy;
Begin
  FClearTypeLink.Free;
  Inherited;
End;

Procedure TClearTypeListBox.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If (FClearTypeLink <> Nil) And
     (FClearTypeLink.ClearType <> Nil) And
     (AComponent = FClearTypeLink.ClearType) And
     (Operation = opRemove) Then FClearTypeLink.ClearType := Nil;
  Inherited Notification(AComponent,Operation);
End;

Function TClearTypeListBox.GetClearType: TClearTypeText;
Begin
  Result := FClearTypeLink.ClearType;
End;

Procedure TClearTypeListBox.SetClearType(AClearType: TClearTypeText);
Begin
  FClearTypeLink.ClearType := AClearType;
  Invalidate;
End;

Procedure TClearTypeListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Flags : Longint;
  Data  : String;
  Size  : TSize;

begin
  if Assigned(OnDrawItem) then OnDrawItem(Self, Index, Rect, State)
  else
  begin
    Canvas.FillRect(Rect);
    if Index < Count then
    begin
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      if not UseRightToLeftAlignment
       then Inc(Rect.Left, 2)
       else Dec(Rect.Right, 2);
      Data := '';
      if (Style in [lbVirtual, lbVirtualOwnerDraw])
       then Data := DoGetData(Index)
       else Data := Items[Index];
      If FClearTypeLink.ClearType <> Nil Then
      Begin
        Size.cx  := 0;
        Size.cy  := 0;
        ClearType.GetTextExtents(Font,Data,Size.CX,Size.CY);
        FClearTypeLink.ClearType.DrawText(Rect.Left,Rect.Top,Data,Flags,Canvas,Canvas.Brush.Color,@Rect,False)
      End
      Else DrawText(Canvas.Handle, PChar(Data), Length(Data), Rect, Flags);
    end;
  end;
End;

// TClearTypeStringGrid

Constructor TClearTypeStringGrid.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FClearTypeLink := TClearTypeLink.Create(Self);
End;

Destructor TClearTypeStringGrid.Destroy;
Begin
  FClearTypeLink.Free;
  Inherited;
End;

Procedure TClearTypeStringGrid.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If (FClearTypeLink <> Nil) And
     (FClearTypeLink.ClearType <> Nil) And
     (AComponent = FClearTypeLink.ClearType) And
     (Operation = opRemove) Then FClearTypeLink.ClearType := Nil;
  Inherited Notification(AComponent,Operation);
End;

Function TClearTypeStringGrid.GetClearType: TClearTypeText;
Begin
  Result := FClearTypeLink.ClearType;
End;

Procedure TClearTypeStringGrid.SetClearType(AClearType: TClearTypeText);
Begin
  FClearTypeLink.ClearType := AClearType;
  Invalidate;
End;

Procedure TClearTypeStringGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
Var
  Size : TSize;
  Hold : Integer;

Begin
  If DefaultDrawing Then
  Begin
    If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then
    Begin
      Size.cx  := 0;
      Size.cy  := 0;
      ClearType.GetTextExtents(Font,Cells[ACol, ARow],Size.CX,Size.CY);
      FClearTypeLink.ClearType.DrawText(ARect.Left+2,ARect.Top+2,Cells[ACol, ARow],0,Canvas,Canvas.Brush.Color,@ARect,Canvas.Brush.Style = bsSolid);
    End
    Else Canvas.TextRect(ARect, ARect.Left+2, ARect.Top+2, Cells[ACol, ARow]);
  End;

  // From TCustomDrawGrid

  if Assigned(OnDrawCell) then
  begin
    if UseRightToLeftAlignment then
    begin
      ARect.Left := ClientWidth - ARect.Left;
      ARect.Right := ClientWidth - ARect.Right;
      Hold := ARect.Left;
      ARect.Left := ARect.Right;
      ARect.Right := Hold;
      ChangeGridOrientation(False);
    end;
    OnDrawCell(Self, ACol, ARow, ARect, AState);
    if UseRightToLeftAlignment then ChangeGridOrientation(True);
  end;
End;

// TClearTypeEdit

Constructor TClearTypeEdit.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FClearTypeLink := TClearTypeLink.Create(Self);
  FPainting      := False;
  FLastCaretX    := 0;
//  AddHook(Self);
End;

Destructor TClearTypeEdit.Destroy;
Begin
//  RemoveHook(Self);
  FClearTypeLink.Free;
  Inherited;
End;

Procedure TClearTypeEdit.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If (FClearTypeLink <> Nil) And
     (FClearTypeLink.ClearType <> Nil) And
     (AComponent = FClearTypeLink.ClearType) And
     (Operation = opRemove) Then FClearTypeLink.ClearType := Nil;
  Inherited Notification(AComponent,Operation);
End;

Function TClearTypeEdit.GetClearType: TClearTypeText;
Begin
  Result := FClearTypeLink.ClearType;
End;

Procedure TClearTypeEdit.SetClearType(AClearType: TClearTypeText);
Begin
  FClearTypeLink.ClearType := AClearType;
  Invalidate;
End;

Procedure TClearTypeEdit.WMMove(Var Msg: TMessage);
Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint(False);
  Inherited;
End;

Procedure TClearTypeEdit.WMSize(Var Msg: TMessage);
Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then
  Begin
    Inherited;
    WMMove(Msg);
  End
  Else Inherited;
End;

Procedure TClearTypeEdit.WMEnable(Var Msg: TMessage);
Begin
  Inherited;
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint(False);
End;

Procedure TClearTypeEdit.WMPaint(Var Msg: TWMPaint);
Var
  PS : TPaintStruct;
  R  : TRect;

Begin
  If FPainting Then Exit;
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then
  Begin
    GetUpdateRect(Handle, R, False);
    If Not IsRectEmpty(R) Then
    Begin
      BeginPaint(Handle, PS);
      Msg.Result := 0;
      InternalPaint(False);
      EndPaint(Handle, PS);
    End;
  End
  Else Inherited;
End;

Procedure TClearTypeEdit.InternalPaint(ReadCaret: Boolean);
Var
  DC          : HDC;
  R           : TRect;
  Size        : TSize;
  Text        : String;
  FTempDC     : HDC;
  FTempBitmap : HBITMAP;
  FOldBitmap  : HBITMAP;
  Margins     : Integer;
  SelLo       : Integer;
  SelHi       : Integer;
  W1          : Integer;
  St1         : String;
  P           : TPoint;
  X           : Integer;
  DiffCaret   : Boolean;
  XOfs        : Integer;
  ForeColor   : TColor;

Begin
  If FPainting Or (Not HandleAllocated) Or Not Visible Then Exit;

  Try
    FPainting := True;
    DC := GetDC(Handle);

    XOfs      := SendMessage(Handle,EM_GETFIRSTVISIBLELINE,0,0);
    Margins   := SendMessage(Handle,EM_GETMARGINS,0,0);
    DiffCaret := False;
    X         := 0;
    If Focused Then
    Begin
      GetCaretPos(P);
      W1 := (P.X - (Margins And $FFFF)) Or (P.Y Shl 16);
      DiffCaret := ((FLastCaretX <> P.X) And (P.X < Width)) Or Not ReadCaret;
      If DiffCaret Then
      Begin
        X := SendMessage(Handle,EM_CHARFROMPOS,0,W1) - XOfs;
      End;
      HideCaret(Handle);
    End;

    FTempDC     := CreateCompatibleDC(DC);
    FTempBitmap := CreateCompatibleBitmap(DC, Width, Height);
    FOldBitmap  := SelectObject(FTempDC, FTempBitmap);

    PaintWindow(FTempDC);
    Size.cx := 0;
    Size.cy := 0;
    Text    := Copy(Caption,XOfs + 1,Length(Caption));
    Windows.GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);

    SendMessage(Handle,EM_GETSEL,Integer(@SelLo),Integer(@SelHi));

    R.Left   := Margins And $FFFF;
    R.Top    := 0;
    R.Right  := Width - (Margins Shr 16);
    R.Bottom := Height;

    If Enabled
     Then ForeColor := Font.Color
     Else ForeColor := clGrayText;
    FClearTypeLink.ClearType.DrawTextDCSel(R.Left + 1,1,Text,0,FTempDC,Font,SelLo - XOfs,SelHi - XOfs,ForeColor,Color,clHighlightText,clHighlight,@R,True);
    BitBlt(DC,0,0,Width,Height,FTempDC,0,0,SRCCOPY);

    If Focused Then
    Begin
      If DiffCaret Then
      Begin
        St1 := Copy(Text,1,X And $FFFF);
        Windows.GetTextExtentPoint32(FClearTypeLink.ClearType.FSrcBMP.Canvas.Handle, PChar(St1), Length(St1), Size);
        If ReadCaret Then FLastCaretX := Round(Size.CX / 3) + (Margins And $FFFF);
        SetCaretPos(FLastCaretX,P.Y);
      End;
      ShowCaret(Handle);
    End;

    SelectObject(FTempDC, FOldBitmap);
    DeleteObject(FTempBitmap);

    DeleteDC(FTempDC);
    ReleaseDC(Handle,DC);
  Finally
    FPainting := False;
  End;
End;

Procedure TClearTypeEdit.WMPaste(Var Msg: TWMPaste); 
Begin
  Inherited;
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint(True);
End;

Procedure TClearTypeEdit.WMKeyDown(Var Msg: TWMKeyDown);
Begin
  Inherited;
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint(True);
End;

Procedure TClearTypeEdit.WMChar(Var Msg: TWMChar);
Begin
  Inherited;
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint(True);
End;

Procedure TClearTypeEdit.WMLButtonDown(Var Msg: TWMLButtonDown);
Begin
  Inherited;
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint(True);
End;

Procedure TClearTypeEdit.WMLButtonUp(Var Msg: TWMLButtonUp);
Begin
  Inherited;
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint(False);
End;

Procedure TClearTypeEdit.WMLButtonDblClk(Var Msg: TWMLButtonDblClk);
Begin
  Inherited;
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint(True);
End;

Procedure TClearTypeEdit.WMMouseMove(Var Msg: TWMMouseMove);
Begin
  Inherited;
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint((Msg.Keys And MK_LBUTTON) <> 0);
End;

Procedure TClearTypeEdit.WMUpdateCT(Var Msg: TMessage);
Begin
  If (FClearTypeLink.ClearType <> Nil) And (FClearTypeLink.ClearType.Enabled) Then InternalPaint(False);
End;

Initialization
  Hooks := TList.Create;
  ClearTypeClasses := TList.Create;
  RegisterClearTypeControl(TClearTypeCheckBox);
  RegisterClearTypeControl(TClearTypeRadioButton);
  SetLookup;
Finalization
  If Hooks.Count > 0 then UnHookWindowsHookEx(WHook);
  Hooks.Free;
  ClearTypeClasses.Free;
end.
