Unit OpenGLGUI;

Interface

Uses dglOpenGL,Graphics,Forms,AbstractComponents,Classes,Types,Controls;

Const
  BorderThickness    = 5;
  CaptionBarHeight   = 22;
  ScrollBarThickness = 14;

Type
  TGLButton = Class(TAbstractButton)
  Protected
    Procedure   CreateChildren; Override;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TAbstractButtonKind;
                       AStyle: TAbstractButtonStyle; ACaption: String);
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TGLRadioButton = Class(TAbstractButton)
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; AGroup: Integer);
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TGLCheckBox = Class(TGLRadioButton)
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String);
  End;

  TGLEdit = Class(TAbstractEdit)
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AText: String; AMaxLen: Integer);
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TGLScrollBar = Class(TAbstractScrollBar)
  Protected
    Procedure   CreateChildren; Override;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TScrollBarKind); Overload;
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,ALength: Integer; AKind: TScrollBarKind); Overload;
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TGLTrackBar = Class(TAbstractTrackBar)
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer); Overload;
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,ALength: Integer); Overload;
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TGLComboBox = Class(TAbstractComboBox)
  Protected
    Procedure   CreateChildren; Override;
    Procedure   ListPanelBeforePaint(Sender: TObject; X,Y: Integer);
    Procedure   ListPanelPaint(Sender: TObject; X,Y: Integer);
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth: Integer); Overload;
  End;

  TGLListBox = Class(TAbstractListBox)
  Protected
    Procedure   CreateChildren; Override;
    Procedure   OutputText(X,Y: Integer; St: String; DefaultColor: TColor); Override;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AWordWrapped: Boolean);
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TGLStringGrid = Class(TGLListBox)
  Protected
    FFixedRows       : Integer;
    FFixedForeground : TColor;
    FFixedBackground : TColor;
    FCols            : Integer;
    FColWidths       : Array Of Integer;
    Procedure   OutputText(X,Y: Integer; St: String; DefaultColor: TColor); Override;
    Procedure   SetFixedRows(I: Integer);
    Procedure   SetCols(I: Integer);
    Procedure   SetColWidths(I,J: Integer);
    Procedure   SetFixedForeground(C: TColor);
    Procedure   SetFixedBackground(C: TColor);
    Function    GetColWidths(I: Integer): Integer;
    Procedure   AfterSetSize; Override;
    Procedure   MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Function    GetAvailableVertPixels: Integer; Override;
//    Function    GetMaxTopIndex: Integer; Override;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
    Procedure   Paint(X,Y: Integer); Override;
    Property    FixedRows             : Integer Read FFixedRows       Write SetFixedRows;
    Property    Cols                  : Integer Read FCols            Write SetCols;
    Property    FixedForeground       : TColor  Read FFixedForeground Write SetFixedForeground;
    Property    FixedBackground       : TColor  Read FFixedBackground Write SetFixedBackground;
    Property    ColWidths[I: Integer] : Integer Read GetColWidths     Write SetColWidths;
  End;

  TGLPageControl = Class(TAbstractPageControl)
  Protected
    Function    GetTabHeight: Integer; Override;
    Function    GetTabWidth(Index: Integer): Integer; Override;
    Function    CreatePagePanel(PageCaption: String): TAbstractPanel; Override;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TGLLabel = Class(TAbstractLabel)
  Protected
    Procedure   FillBackground; Override;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; ATransparent: Boolean); Overload;
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop: Integer; ACaption: String; ATransparent: Boolean); Overload;
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TGLScrollPane = Class(TAbstractScrollPane)
  Protected
    Procedure   CreateChildren; Override;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
    Procedure   Repaint; Override;
  End;

  TGLPanel = Class(TAbstractPanel)
  Protected
    FTexX1 : Integer;
    FTexY1 : Integer;
    FTexX2 : Integer;
    FTexY2 : Integer;
    Procedure   CreateChildren; Override;
    Procedure   BeforePaintingChildren; Override;
    Procedure   AfterPaintingChildren; Override;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String);
    Procedure   Paint(X,Y: Integer); Override;
    Property    TexX1 : Integer Read FTexX1 Write FTexX1;
    Property    TexY1 : Integer Read FTexY1 Write FTexY1;
    Property    TexX2 : Integer Read FTexX2 Write FTexX2;
    Property    TexY2 : Integer Read FTexY2 Write FTexY2;
  End;

  TGLProgressBar = Class(TAbstractProgressBar)
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AValue: Single; AColor: TColor);
    Procedure   Paint(X,Y: Integer); Override;
  End;

  TGLWindow = Class(TAbstractWindow)
  Protected
    FAlpha : Byte;
    Procedure   CalcClientExtents; Override;
    Procedure   CreateCloseButton; Override;
    Procedure   BeforePaintingChildren; Override;
    Procedure   AfterPaintingChildren; Override;
  Public
    Constructor Create(ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; AUI: TAbstractUI);
    Destructor  Destroy; Override;
    Procedure   Paint(X,Y: Integer); Override;
    Function    TextWidth(Const St: String): Integer;    Override;
    Function    TextHeight(Const St: String): Integer;   Override;
    Procedure   TextOut(X,Y: Integer; Const St: String); Override;
    Procedure   BeginClip(Rect: TRect); Override;
    Procedure   EndClip;                Override;
    Property    Alpha : Byte Read FAlpha Write FAlpha;
  End;

  TGLUI = Class(TAbstractUI)
  Protected
    FGUITexture  : glUInt;      // the GUI texture
    FontTexture  : glUInt;      // the font texture
    BkgdTexture  : glUInt;      // the window background texture
    FontList     : glUInt;      // font display list
    FontWidth    : Packed Array[0..255] Of Byte;  // Array of font character widths
    FOwner       : TObject;     // Owner is a TSceneGL
    FTextures    : TStringList; // List of TTexture
    FInitialized : Boolean;
    Procedure   glWrite(X,Y: Integer; St: String);
    Procedure   LoadFontWidths;
    Procedure   BuildFont;
    Procedure   InitGUI;
    Function    LoadTexture(FileName: String; Var Texture: GLUInt; FlipV: Boolean): Boolean;
  Public
    Constructor Create(AOwner: TObject); // Owner is a TSceneGL
    Destructor  Destroy; Override;
    Function    AddWindow(ACaption: String): TAbstractWindow; Override;
    Procedure   Repaint; Override;
    Property    GUITexture : glUInt Read FGUITexture;
  End;

Var
  SepOne : Single;

Implementation

Uses SysUtils,URickGL,U3DPolys,Math;

Type
  TRGBA = Packed Record
    R,G,B,A: Byte;
  End;

// -------------------------------
// TGLButton
// -------------------------------

Constructor TGLButton.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TAbstractButtonKind;
                             AStyle: TAbstractButtonStyle; ACaption: String);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,AKind,AStyle,ACaption);
End; // TGLButton.Create

Procedure TGLButton.CreateChildren;
Begin
  FLabel := TGLLabel.Create(Self,0,0,'',True);
End; // TGLButton.CreateChildren

Procedure TGLButton.Paint(X,Y: Integer);
Var
  I,H         : Integer;
  Rect        : TRect;
  X1,Y1,X2,Y2 : Integer;

  Procedure DrawButton;
  Begin
    glBegin(GL_QUADS);
      If Width > Height Then
      Begin
        // left side
        glTexCoord2f(X1/256,   Y1/256); glVertex2f(X,   -Y);
        glTexCoord2f(X1/256,   Y2/256); glVertex2f(X,   -Y-Height);
        glTexCoord2f((X1+6)/256, Y2/256); glVertex2f(X+6, -Y-Height);
        glTexCoord2f((X1+6)/256, Y1/256); glVertex2f(X+6, -Y);

        // middle
        glTexCoord2f((X1+6)/256, Y1/256); glVertex2f(X+6, -Y);
        glTexCoord2f((X1+6)/256, Y2/256); glVertex2f(X+6, -Y-Height);
        glTexCoord2f((X2-6)/256, Y2/256); glVertex2f(X+Width-6, -Y-Height);
        glTexCoord2f((X2-6)/256, Y1/256); glVertex2f(X+Width-6, -Y);

        // right side
        glTexCoord2f((X2-6)/256, Y1/256); glVertex2f(X+Width-6, -Y);
        glTexCoord2f((X2-6)/256, Y2/256); glVertex2f(X+Width-6, -Y-Height);
        glTexCoord2f(X2/256, Y2/256); glVertex2f(X+Width, -Y-Height);
        glTexCoord2f(X2/256, Y1/256); glVertex2f(X+Width, -Y);
      End
      Else If Width < Height Then
      Begin
        // top side
        glTexCoord2f(X1/256, Y1/256); glVertex2f(X, -Y);
        glTexCoord2f(X1/256, (Y1+6)/256); glVertex2f(X, -Y-6);
        glTexCoord2f(X2/256, (Y1+6)/256); glVertex2f(X+Width,-Y-6);
        glTexCoord2f(X2/256, Y1/256); glVertex2f(X+Width, -Y);

        // middle
        glTexCoord2f(X1/256, (Y1+6)/256); glVertex2f(X, -Y-6);
        glTexCoord2f(X1/256, (Y2-6)/256); glVertex2f(X, -Y-Height+6);
        glTexCoord2f(X2/256, (Y2-6)/256); glVertex2f(X+Width, -Y-Height+6);
        glTexCoord2f(X2/256, (Y1+6)/256); glVertex2f(X+Width, -Y-6);

        // bottom side
        glTexCoord2f(X1/256, (Y2-6)/256); glVertex2f(X, -Y-Height+6);
        glTexCoord2f(X1/256, Y2/256); glVertex2f(X, -Y-Height);
        glTexCoord2f(X2/256, Y2/256); glVertex2f(X+Width, -Y-Height);
        glTexCoord2f(X2/256, (Y2-6)/256); glVertex2f(X+Width, -Y-Height+6);
      End
      Else
      Begin
        glTexCoord2f(X1/256, Y1/256); glVertex2f(X,         -Y);
        glTexCoord2f(X1/256, Y2/256); glVertex2f(X,         -Y - Height);
        glTexCoord2f(X2/256, Y2/256); glVertex2f(X + Width, -Y - Height);
        glTexCoord2f(X2/256, Y1/256); glVertex2f(X + Width, -Y);
      End;
    glEnd;
  End; // DrawButton

Begin
  glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
  glPushMatrix;
    glColor4ub(255,255,255,TGLWindow(FOwner).FAlpha);
    glTranslatef(0,0,SepOne);
    Case Self.Kind Of
      cbkCaption:
      Begin
        // Clip to the button area

        Rect.Left   := X + FOwner.Left;
        Rect.Top    := Y + FOwner.Top;
        Rect.Right  := Rect.Left + Width  - 1;
        Rect.Bottom := Rect.Top  + Height - 1;

        glScissor(Rect.Left, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - Rect.Bottom, Rect.Right - Rect.Left + 1, Rect.Bottom - Rect.Top + 1);
        glEnable(GL_SCISSOR_TEST);

        H := TextHeight(Caption);
        If FDown Then
        Begin
          X1 := 0;
          Y1 := 128;
          X2 := 64;
          Y2 := 192;
          DrawButton;

{
          If FEnabled
           Then I := FForeground
           Else I := DefaultDisabledForeground;
          glColor4ub(TRGBA(I).R,TRGBA(I).G,TRGBA(I).B,TGLWindow(FOwner).FAlpha);
          TextOut(X + 1 + (Width - TextWidth(Caption)) Div 2, Y + 1 + ((Height - H) Div 2) + H, Caption);
}
        End
        Else
        Begin
          X1 := 64;
          Y1 := 128;
          X2 := 128;
          Y2 := 192;
          DrawButton;
{
          If FEnabled
           Then I := FForeground
           Else I := DefaultDisabledForeground;
          glColor4ub(TRGBA(I).R,TRGBA(I).G,TRGBA(I).B,TGLWindow(FOwner).FAlpha);
          TextOut(X + (Width - TextWidth(Caption)) Div 2, Y + ((Height - H) Div 2) + H, Caption);
}          
        End;

        // Stop clipping

        glDisable(GL_SCISSOR_TEST);
        glScissor(0,TSceneGL(TGLUI(FUI).FOwner).WindowHeight,TSceneGL(TGLUI(FUI).FOwner).WindowWidth,TSceneGL(TGLUI(FUI).FOwner).WindowHeight);
      End;
      cbkUpArrow:
      Begin
        If FDown Then
        Begin
          glBegin(GL_QUADS);
            glTexCoord2f( 88/256, 80/256); glVertex2f(X,         -Y);
            glTexCoord2f( 88/256, 94/256); glVertex2f(X,         -Y - Height);
            glTexCoord2f(102/256, 94/256); glVertex2f(X + Width, -Y - Height);
            glTexCoord2f(102/256, 80/256); glVertex2f(X + Width, -Y);
          glEnd;
        End
        Else
        Begin
          If FFlat And Not FMouseOver Then
          Begin
            glBegin(GL_QUADS);
              glTexCoord2f( 89/256, 65/256); glVertex2f(X + 1,         -Y - 1);
              glTexCoord2f( 89/256, 77/256); glVertex2f(X + 1,         -Y - Height + 1);
              glTexCoord2f(101/256, 77/256); glVertex2f(X + Width - 1, -Y - Height + 1);
              glTexCoord2f(101/256, 65/256); glVertex2f(X + Width - 1, -Y - 1);
            glEnd;
          End
          Else
          Begin
            glBegin(GL_QUADS);
              glTexCoord2f( 88/256, 64/256); glVertex2f(X,         -Y);
              glTexCoord2f( 88/256, 78/256); glVertex2f(X,         -Y - Height);
              glTexCoord2f(102/256, 78/256); glVertex2f(X + Width, -Y - Height);
              glTexCoord2f(102/256, 64/256); glVertex2f(X + Width, -Y);
            glEnd;
          End;
        End;
      End;
      cbkDownArrow:
      Begin
        If FDown Then
        Begin
          glBegin(GL_QUADS);
            glTexCoord2f(104/256, 80/256); glVertex2f(X,         -Y);
            glTexCoord2f(104/256, 94/256); glVertex2f(X,         -Y - Height);
            glTexCoord2f(118/256, 94/256); glVertex2f(X + Width, -Y - Height);
            glTexCoord2f(118/256, 80/256); glVertex2f(X + Width, -Y);
          glEnd;
        End
        Else
        Begin
          If FFlat And Not FMouseOver Then
          Begin
            glBegin(GL_QUADS);
              glTexCoord2f(105/256, 65/256); glVertex2f(X + 1,         -Y - 1);
              glTexCoord2f(105/256, 77/256); glVertex2f(X + 1,         -Y - Height + 1);
              glTexCoord2f(117/256, 77/256); glVertex2f(X + Width - 1, -Y - Height + 1);
              glTexCoord2f(117/256, 65/256); glVertex2f(X + Width - 1, -Y - 1);
            glEnd;
          End
          Else
          Begin
            glBegin(GL_QUADS);
              glTexCoord2f(104/256, 64/256); glVertex2f(X,         -Y);
              glTexCoord2f(104/256, 78/256); glVertex2f(X,         -Y - Height);
              glTexCoord2f(118/256, 78/256); glVertex2f(X + Width, -Y - Height);
              glTexCoord2f(118/256, 64/256); glVertex2f(X + Width, -Y);
            glEnd;
          End;
        End;
      End;
      cbkRightArrow:
      Begin
        If FDown Then
        Begin
          glBegin(GL_QUADS);
            glTexCoord2f(72/256, 80/256); glVertex2f(X,         -Y);
            glTexCoord2f(72/256, 94/256); glVertex2f(X,         -Y - Height);
            glTexCoord2f(86/256, 94/256); glVertex2f(X + Width, -Y - Height);
            glTexCoord2f(86/256, 80/256); glVertex2f(X + Width, -Y);
          glEnd;
        End
        Else
        Begin
          If FFlat And Not FMouseOver Then
          Begin
            glBegin(GL_QUADS);
              glTexCoord2f(73/256, 65/256); glVertex2f(X + 1,         -Y - 1);
              glTexCoord2f(73/256, 77/256); glVertex2f(X + 1,         -Y - Height + 1);
              glTexCoord2f(85/256, 77/256); glVertex2f(X + Width - 1, -Y - Height + 1);
              glTexCoord2f(85/256, 65/256); glVertex2f(X + Width - 1, -Y - 1);
            glEnd;
          End
          Else
          Begin
            glBegin(GL_QUADS);
              glTexCoord2f(72/256, 64/256); glVertex2f(X,         -Y);
              glTexCoord2f(72/256, 78/256); glVertex2f(X,         -Y - Height);
              glTexCoord2f(86/256, 78/256); glVertex2f(X + Width, -Y - Height);
              glTexCoord2f(86/256, 64/256); glVertex2f(X + Width, -Y);
            glEnd;
          End;
        End;
      End;
      cbkLeftArrow:
      Begin
        If FDown Then
        Begin
          glBegin(GL_QUADS);
            glTexCoord2f(56/256, 80/256); glVertex2f(X,         -Y);
            glTexCoord2f(56/256, 94/256); glVertex2f(X,         -Y - Height);
            glTexCoord2f(70/256, 94/256); glVertex2f(X + Width, -Y - Height);
            glTexCoord2f(70/256, 80/256); glVertex2f(X + Width, -Y);
          glEnd;
        End
        Else
        Begin
          If FFlat And Not FMouseOver Then
          Begin
            glBegin(GL_QUADS);
              glTexCoord2f(57/256, 65/256); glVertex2f(X + 1,         -Y - 1);
              glTexCoord2f(57/256, 77/256); glVertex2f(X + 1,         -Y - Height + 1);
              glTexCoord2f(69/256, 77/256); glVertex2f(X + Width - 1, -Y - Height + 1);
              glTexCoord2f(69/256, 65/256); glVertex2f(X + Width - 1, -Y - 1);
            glEnd;
          End
          Else
          Begin
            glBegin(GL_QUADS);
              glTexCoord2f(56/256, 64/256); glVertex2f(X,         -Y);
              glTexCoord2f(56/256, 78/256); glVertex2f(X,         -Y - Height);
              glTexCoord2f(70/256, 78/256); glVertex2f(X + Width, -Y - Height);
              glTexCoord2f(70/256, 64/256); glVertex2f(X + Width, -Y);
            glEnd;
          End;
        End;
      End;
      cbkX:
      Begin
        If FDown Then
        Begin
          glBegin(GL_QUADS);
            glTexCoord2f(104/256, 48/256); glVertex2f(X,         -Y);
            glTexCoord2f(104/256, 62/256); glVertex2f(X,         -Y - Height);
            glTexCoord2f(118/256, 62/256); glVertex2f(X + Width, -Y - Height);
            glTexCoord2f(118/256, 48/256); glVertex2f(X + Width, -Y);
          glEnd;
        End
        Else
        Begin
          If FFlat And Not FMouseOver Then
          Begin
            glBegin(GL_QUADS);
              glTexCoord2f(105/256, 33/256); glVertex2f(X + 1,         -Y - 1);
              glTexCoord2f(105/256, 45/256); glVertex2f(X + 1,         -Y - Height + 1);
              glTexCoord2f(117/256, 45/256); glVertex2f(X + Width - 1, -Y - Height + 1);
              glTexCoord2f(117/256, 33/256); glVertex2f(X + Width - 1, -Y - 1);
            glEnd;
          End
          Else
          Begin
            glBegin(GL_QUADS);
              glTexCoord2f(104/256, 32/256); glVertex2f(X,         -Y);
              glTexCoord2f(104/256, 46/256); glVertex2f(X,         -Y - Height);
              glTexCoord2f(118/256, 46/256); glVertex2f(X + Width, -Y - Height);
              glTexCoord2f(118/256, 32/256); glVertex2f(X + Width, -Y);
            glEnd;
          End;
        End;
      End;
    End; // Case
  glPopMatrix;
End; // TGLButton.Paint

// -------------------------------
// TGLRadioButton
// -------------------------------

Constructor TGLRadioButton.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; AGroup: Integer);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,cbkCaption,cbsCheck,ACaption);
  FGroup      := AGroup;
  FAllowAllUp := False;
End; // TGLRadioButton.Create

Procedure TGLRadioButton.Paint(X,Y: Integer);
Var
  Rect : TRect;
  I    : Integer;
  H    : Integer;
  
Begin
  H := TextHeight(Caption);
  glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
  glPushMatrix;
    glColor4ub(255,255,255,TGLWindow(FOwner).FAlpha);
    glTranslatef(0,0,SepOne);

    // Clip to the button area

    Rect.Left   := X + FOwner.Left;
    Rect.Top    := Y + FOwner.Top;
    Rect.Right  := Rect.Left + Width  - 1;
    Rect.Bottom := Rect.Top  + Height - 1;

    glScissor(Rect.Left, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - Rect.Bottom + 1, Rect.Right - Rect.Left + 1, Rect.Bottom - Rect.Top + 1);
    glEnable(GL_SCISSOR_TEST);

    H := TextHeight(Caption);

    If FDown Then
    Begin
      glBegin(GL_QUADS);
        If FPressed Then
        Begin
          glTexCoord2f( 88/256, 32/256); glVertex2f(X,      -Y - (Height Div 2) + 6);
          glTexCoord2f( 88/256, 46/256); glVertex2f(X,      -Y - (Height Div 2) - 8);
          glTexCoord2f(102/256, 46/256); glVertex2f(X + 14, -Y - (Height Div 2) - 8);
          glTexCoord2f(102/256, 32/256); glVertex2f(X + 14, -Y - (Height Div 2) + 6);
        End
        Else
        Begin
          glTexCoord2f( 88/256, 48/256); glVertex2f(X,      -Y - (Height Div 2) + 6);
          glTexCoord2f( 88/256, 62/256); glVertex2f(X,      -Y - (Height Div 2) - 8);
          glTexCoord2f(102/256, 62/256); glVertex2f(X + 14, -Y - (Height Div 2) - 8);
          glTexCoord2f(102/256, 48/256); glVertex2f(X + 14, -Y - (Height Div 2) + 6);
        End;
      glEnd;
    End
    Else
    Begin
      glBegin(GL_QUADS);
        If FPressed Then
        Begin
          glTexCoord2f(72/256, 32/256); glVertex2f(X,      -Y - (Height Div 2) + 6);
          glTexCoord2f(72/256, 46/256); glVertex2f(X,      -Y - (Height Div 2) - 8);
          glTexCoord2f(86/256, 46/256); glVertex2f(X + 14, -Y - (Height Div 2) - 8);
          glTexCoord2f(86/256, 32/256); glVertex2f(X + 14, -Y - (Height Div 2) + 6);
        End
        Else
        Begin
          glTexCoord2f(72/256, 48/256); glVertex2f(X,      -Y - (Height Div 2) + 6);
          glTexCoord2f(72/256, 62/256); glVertex2f(X,      -Y - (Height Div 2) - 8);
          glTexCoord2f(86/256, 62/256); glVertex2f(X + 14, -Y - (Height Div 2) - 8);
          glTexCoord2f(86/256, 48/256); glVertex2f(X + 14, -Y - (Height Div 2) + 6);
        End;
      glEnd;
    End;

    // Draw the caption

    If FEnabled
     Then I := FForeground
     Else I := DefaultDisabledForeground;
    glColor4ub(TRGBA(I).R,TRGBA(I).G,TRGBA(I).B,TGLWindow(FOwner).FAlpha);
    TextOut(X + 22, Y + 1 + ((Height - H) Div 2) + H, Caption);

    // Stop clipping

    glDisable(GL_SCISSOR_TEST);
    glScissor(0,TSceneGL(TGLUI(FUI).FOwner).WindowHeight,TSceneGL(TGLUI(FUI).FOwner).WindowWidth,TSceneGL(TGLUI(FUI).FOwner).WindowHeight);
  glPopMatrix;
End; // TGLRadioButton.Paint

// -------------------------------
// TGLCheckBox
// -------------------------------

Constructor TGLCheckBox.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,ACaption,0);
  FGroup      := -1;
  FAllowAllUp := True;
End; // TGLCheckBox.Create

// -------------------------------
// TGLEdit
// -------------------------------

Constructor TGLEdit.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AText: String; AMaxLen: Integer);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,AText,AMaxLen);
End; // TGLEdit.Create

Procedure TGLEdit.Paint(X,Y: Integer);
Var H: Integer;
Begin
  glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
  glPushMatrix;
    glColor4ub(255,255,255,TGLWindow(FOwner).FAlpha);
//    glTranslatef(TCanvasWindow(FOwner).Left, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - TCanvasWindow(FOwner).Top, -TCanvasWindow(FOwner).ZOrder * 100 * SepOne);
    glTranslatef(0,0,SepOne);
    H := TextHeight(Text);
    glBegin(GL_QUADS);

      // top left corner of window.
      glTexCoord2f(72/256, 102/256);   glVertex2f(X,     -Y);
      glTexCoord2f(72/256, 104/256);   glVertex2f(X,     -Y - 1);
      glTexCoord2f(74/256, 104/256);   glVertex2f(X + 1, -Y - 1);
      glTexCoord2f(74/256, 102/256);   glVertex2f(X + 1, -Y);

      // top of window.
      glTexCoord2f(74/256, 102/256);   glVertex2f(X + 2,         -Y);
      glTexCoord2f(74/256, 104/256);   glVertex2f(X + 2,         -Y - 1);
      glTexCoord2f(84/256, 104/256);   glVertex2f(X + Width - 2, -Y - 1);
      glTexCoord2f(84/256, 102/256);   glVertex2f(X + Width - 2, -Y);

      // top right corder of window.
      glTexCoord2f(84/256, 102/256);   glVertex2f(X + Width - 2, -Y);
      glTexCoord2f(84/256, 104/256);   glVertex2f(X + Width - 2, -Y - 1);
      glTexCoord2f(86/256, 104/256);   glVertex2f(X + Width,     -Y - 1);
      glTexCoord2f(86/256, 102/256);   glVertex2f(X + Width,     -Y);

      // left side of window.
      glTexCoord2f(72/256, 104/256);   glVertex2f(X,     -Y - 2);
      glTexCoord2f(72/256, 114/256);   glVertex2f(X,     -Y - Height + 3);
      glTexCoord2f(74/256, 114/256);   glVertex2f(X + 1, -Y - Height + 3);
      glTexCoord2f(74/256, 104/256);   glVertex2f(X + 1, -Y - 2);

      // draw the main body of the window
      glTexCoord2f(74/256, 104/256);   glVertex2f(X + 2,         -Y - 2);
      glTexCoord2f(74/256, 114/256);   glVertex2f(X + 2,         -Y - Height + 3);
      glTexCoord2f(84/256, 114/256);   glVertex2f(X + Width - 3, -Y - Height + 3);
      glTexCoord2f(84/256, 104/256);   glVertex2f(X + Width - 3, -Y - 2);

      // right side of window.
      glTexCoord2f(84/256, 104/256);   glVertex2f(X + Width - 2, -Y - 2);
      glTexCoord2f(84/256, 114/256);   glVertex2f(X + Width - 2, -Y - Height + 3);
      glTexCoord2f(86/256, 114/256);   glVertex2f(X + Width - 1, -Y - Height + 3);
      glTexCoord2f(86/256, 104/256);   glVertex2f(X + Width - 1, -Y - 2);

      // bottom left corner of window.
      glTexCoord2f(72/256, 114/256);  glVertex2f(X,     -Y - Height + 2);
      glTexCoord2f(72/256, 116/256);  glVertex2f(X,     -Y - Height);
      glTexCoord2f(74/256, 116/256);  glVertex2f(X + 1, -Y - Height);
      glTexCoord2f(74/256, 114/256);  glVertex2f(X + 1, -Y - Height + 2);

      // bottom of window.
      glTexCoord2f(74/256, 114/256);  glVertex2f(X + 2,         -Y - Height + 2);
      glTexCoord2f(74/256, 116/256);  glVertex2f(X + 2,         -Y - Height);
      glTexCoord2f(84/256, 116/256);  glVertex2f(X + Width - 3, -Y - Height);
      glTexCoord2f(84/256, 114/256);  glVertex2f(X + Width - 3, -Y - Height + 2);

      // bottom right corder of window.
      glTexCoord2f(84/256, 114/256);  glVertex2f(X + Width - 2, -Y - Height + 2);
      glTexCoord2f(84/256, 116/256);  glVertex2f(X + Width - 2, -Y - Height);
      glTexCoord2f(86/256, 116/256);  glVertex2f(X + Width - 1, -Y - Height);
      glTexCoord2f(86/256, 114/256);  glVertex2f(X + Width - 1, -Y - Height + 2);
    glEnd;

    If FReadOnly Then glColor4ub(192,192,192,TGLWindow(FOwner).FAlpha);

    glTranslatef(0,0,SepOne);
    If FFocused
     Then TextOut(X + 3, Y + ((Height - H) Div 2) + H, Text + '|')
     Else TextOut(X + 3, Y + ((Height - H) Div 2) + H, Text);
  glPopMatrix;
End; // TGLEdit.Paint

// -------------------------------
// TGLScrollBar
// -------------------------------

Constructor TGLScrollBar.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TScrollBarKind);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,AKind);
End; // TGLScrollBar.Create

Constructor TGLScrollBar.Create(AParent: TAbstractComponent; ALeft,ATop,ALength: Integer; AKind: TScrollBarKind);
Begin
  Inherited Create(AParent,ALeft,ATop,ALength,AKind);
End; // TGLScrollBar.Create

Procedure TGLScrollBar.Paint(X,Y: Integer);
Var J,I1,I2: Integer;

  Procedure PaintSlider(X1,Y1,X2,Y2: Integer; Color: TColor);
  Var W,H: Integer;
  Begin
    glBegin(GL_QUADS);
      If FKind = sbVertical Then
      Begin
        // Top of slider

        W := X2 - X1 + 1;
        H := Math.Min(Y2 - Y1 + 1,4);
        If H > 0 Then
        Begin
          glTexCoord2f(41/256, 105/256);        glVertex2f(X1, -Y1);
          glTexCoord2f(41/256, (105 + H)/256);  glVertex2f(X1, -Y1 - H);
          glTexCoord2f(53/256, (105 + H)/256);  glVertex2f(X1 + W, -Y1 - H);
          glTexCoord2f(53/256, 105/256);        glVertex2f(X1 + W, -Y1);
        End;

        // Middle of slider

        H := Y2 - Y1 + 1 - 8;
        If H > 0 Then
        Begin
          glTexCoord2f(41/256, 109/256);        glVertex2f(X1, -Y1 - 4);
          glTexCoord2f(41/256, 113/256);        glVertex2f(X1, -Y1 - 4 - H);
          glTexCoord2f(53/256, 113/256);        glVertex2f(X1 + W, -Y1 - 4 - H);
          glTexCoord2f(53/256, 109/256);        glVertex2f(X1 + W, -Y1 - 4);
        End;

        // Bottom of slider

        W := X2 - X1 + 1;
        H := Math.Min(Y2 - Y1 + 1,4);
        If H > 0 Then
        Begin
          glTexCoord2f(41/256, (117 - H)/256);  glVertex2f(X1, -Y2 + H);
          glTexCoord2f(41/256, 117/256);        glVertex2f(X1, -Y2);
          glTexCoord2f(53/256, 117/256);        glVertex2f(X1 + W, -Y2);
          glTexCoord2f(53/256, (117 - H)/256);  glVertex2f(X1 + W, -Y2 + H);
        End;
      End
      Else
      Begin
      End;
    glEnd;
  End; // TGLScrollBar.PaintSlider

Begin
  glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
  glPushMatrix;
    glColor4ub(255,255,255,TGLWindow(FOwner).FAlpha);
//    glTranslatef(TCanvasWindow(FOwner).Left, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - TCanvasWindow(FOwner).Top, -TCanvasWindow(FOwner).ZOrder * 100 * SepOne);
    glTranslatef(0,0,SepOne);
    If FKind = sbHorizontal Then
    Begin
      J  := FWidth - 2 - 2 * ScrollBarThickness;
      I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + FPageSize)));
      I2 := FPosition + FPageSize - 1;
      If I2 > FMax + FPageSize - 1 Then I2 := FMax + FPageSize - 1;
      I2 := Round(J * ((FMax + FPageSize - I2 - 1) / (FMax - FMin + FPageSize)));
      PaintSlider(X + ScrollBarThickness + 1 + I1,Y + 1,X + J + ScrollBarThickness + 1 - I2 - 1,Y + FHeight - 2,FBackground);
    End
    Else
    Begin
      J  := FHeight - 2 - 2 * ScrollBarThickness;
      I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + FPageSize)));
      I2 := FPosition + FPageSize - 1;
      If I2 > FMax + FPageSize - 1 Then I2 := FMax + FPageSize - 1;
      I2 := Round(J * ((FMax + FPageSize - I2 - 1) / (FMax - FMin + FPageSize)));
      PaintSlider(X + 1,Y + ScrollBarThickness + 1 + I1,X + FWidth - 2,Y + J + ScrollBarThickness + 1 - I2 - 1,FBackground);
    End;
  glPopMatrix;
End; // TGLScrollBar.Paint

Procedure TGLScrollBar.CreateChildren;
Begin
  If FKind = sbHorizontal Then
  Begin
    FULButton := TGLButton.Create(Self,1,1,ScrollBarThickness,FHeight - 2,cbkLeftArrow,cbsButton,'');
    FDRButton := TGLButton.Create(Self,FWidth - ScrollBarThickness - 1,1,ScrollBarThickness,FHeight - 2,cbkRightArrow,cbsButton,'');
  End
  Else
  Begin
    FULButton := TGLButton.Create(Self,1,1,FWidth - 2,ScrollBarThickness,cbkUpArrow,cbsButton,'');
    FDRButton := TGLButton.Create(Self,1,FHeight - ScrollBarThickness - 1,FWidth - 2,ScrollBarThickness,cbkDownArrow,cbsButton,'');
  End;
End; // TGLScrollBar.CreateChildren

// -------------------------------
// TGLTrackBar
// -------------------------------

Constructor TGLTrackBar.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight);
  FDragWidth := 10;
End; // TGLTrackBar.Create

Constructor TGLTrackBar.Create(AParent: TAbstractComponent; ALeft,ATop,ALength: Integer);
Begin
  Inherited Create(AParent,ALeft,ATop,ALength);
  FDragWidth := 10;
End; // TGLTrackBar.Create

Procedure TGLTrackBar.Paint(X,Y: Integer);
Var I,J,K: Integer;
Begin
  glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
  glPushMatrix;
    glColor4ub(255,255,255,TGLWindow(FOwner).FAlpha);
//    glTranslatef(TCanvasWindow(FOwner).Left, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - TCanvasWindow(FOwner).Top, -TCanvasWindow(FOwner).ZOrder * 100 * SepOne);
    glTranslatef(0,0,SepOne);

    I := Height Div 2;

    // Left cap

    glBegin(GL_QUADS);
      glTexCoord2f(56/256,  96/256);   glVertex2f(X + 6,  -Y - I + 1);
      glTexCoord2f(56/256, 100/256);   glVertex2f(X + 6,  -Y - I - 3);
      glTexCoord2f(60/256, 100/256);   glVertex2f(X + 10, -Y - I - 3);
      glTexCoord2f(60/256,  96/256);   glVertex2f(X + 10, -Y - I + 1);
    glEnd;

    // Center portion

    glBegin(GL_QUADS);
      glTexCoord2f(60/256,  96/256);   glVertex2f(X + 10,         -Y - I + 1);
      glTexCoord2f(60/256, 100/256);   glVertex2f(X + 10,         -Y - I - 3);
      glTexCoord2f(66/256, 100/256);   glVertex2f(X + Width - 10, -Y - I - 3);
      glTexCoord2f(66/256,  96/256);   glVertex2f(X + Width - 10, -Y - I + 1);
    glEnd;

    // Right cap

    glBegin(GL_QUADS);
      glTexCoord2f(66/256,  96/256);   glVertex2f(X + Width - 10, -Y - I + 1);
      glTexCoord2f(66/256, 100/256);   glVertex2f(X + Width - 10, -Y - I - 3);
      glTexCoord2f(70/256, 100/256);   glVertex2f(X + Width - 6,  -Y - I - 3);
      glTexCoord2f(70/256,  96/256);   glVertex2f(X + Width - 6,  -Y - I + 1);
    glEnd;

    // Thumb tracker

    J := FWidth - 12;
    K := Round(J * ((FPosition - FMin) / Math.Max(FMax - FMin,1)));
    glTranslatef(0,0,SepOne);
    glBegin(GL_QUADS);
      glTexCoord2f( 88/256, 102/256);   glVertex2f(X + K - 6 + 6, -Y - I + 6);
      glTexCoord2f( 88/256, 116/256);   glVertex2f(X + K - 6 + 6, -Y - I - 8);
      glTexCoord2f(102/256, 116/256);   glVertex2f(X + K + 8 + 6, -Y - I - 8);
      glTexCoord2f(102/256, 102/256);   glVertex2f(X + K + 8 + 6, -Y - I + 6);
    glEnd;

  glPopMatrix;
End; // TGLTrackBar.Paint

// -------------------------------
// TGLComboBox
// -------------------------------

Constructor TGLComboBox.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth: Integer);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth);
End; // TGLComboBox.Create

Procedure TGLComboBox.CreateChildren;
Begin
  FEdit                     := TGLEdit.Create(Self,0,0,FWidth - ScrollBarThickness,FHeight,'',-1);
  FEdit.ReadOnly            := True;
  FButton                   := TGLButton.Create(Self,FWidth - ScrollBarThickness,0,ScrollBarThickness,FHeight,cbkDownArrow,cbsButton,'');
  FListPanel                := TGLPanel.Create(FParent,FLeft + 8,FTop + FHeight - 2,FWidth + 8,TextHeight('Mg') * 8 + 4,'');
  FListPanel.FillBackground := True;
  FListPanel.ShowBorder     := True;
  FListPanel.Visible        := False;
  FList                     := TGLListBox.Create(FListPanel,0,0,FListPanel.Width,FListPanel.Height,False);
  FListPanel.OnBeforePaint  := ListPanelBeforePaint;
  FListPanel.OnPaint        := ListPanelPaint;
End; // TGLComboBox.CreateChildren

Procedure TGLComboBox.ListPanelBeforePaint(Sender: TObject; X,Y: Integer);
Begin
  glPushMatrix;
    glTranslatef(0,0,100 * SepOne);
End; // TGLComboBox.ListPanelBeforePaint

Procedure TGLComboBox.ListPanelPaint(Sender: TObject; X,Y: Integer);
Begin
  glPopMatrix;
End; // TGLComboBox.ListPanelPaint

// -------------------------------
// TGLListBox
// -------------------------------

Constructor TGLListBox.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AWordWrapped: Boolean);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,AWordWrapped);
End; // TGLListBox.Create

Procedure TGLListBox.Paint(X,Y: Integer);
Var
  I,J,K            : Integer;
  MaxTopIndex      : Integer;
  DisplayableItems : Integer;
  Rect             : TRect;
  DefaultColor     : TColor;

Begin
  glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
  glPushMatrix;
    glColor4ub(255,255,255,TGLWindow(FOwner).FAlpha);
    glTranslatef(0,0,SepOne);
    FTextHeight := TextHeight('Mg');
    If FTextHeight > 0 Then
    Begin
      // Fix up the top index

//      MaxTopIndex := FItems.Count - ((FHeight - 4) Div FTextHeight) + 1;
      MaxTopIndex := GetMaxTopIndex;
//      If MaxTopIndex < 0 Then MaxTopIndex := 0;
      If FTopIndex > MaxTopIndex Then FTopIndex := MaxTopIndex;

      DisplayableItems   := GetDisplayableItems;
//      If DisplayableItems * FTextHeight < FHeight Then Inc(DisplayableItems);

      // Clip to the text area

      Rect.Left   := X + FOwner.Left;
      Rect.Top    := Y + FOwner.Top;
      Rect.Right  := Rect.Left + Width  - 1 - FScrollBar.Width;
      Rect.Bottom := Rect.Top  + Height - 1;

      glScissor(Rect.Left, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - Rect.Bottom, Rect.Right - Rect.Left + 1, Rect.Bottom - Rect.Top + 1);
      glEnable(GL_SCISSOR_TEST);

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
        glColor4ub(TRGBA(FSelectionBackground).R,TRGBA(FSelectionBackground).G,TRGBA(FSelectionBackground).B,TGLWindow(FOwner).FAlpha);
        glBegin(GL_QUADS);
          glTexCoord2f(12/256, 110/256);   glVertex2f(X + 1,                    -Y - K * FTextHeight - 2);
          glTexCoord2f(12/256, 114/256);   glVertex2f(X + 1,                    -Y - (K + (J Shr 16)) * FTextHeight - 2);
          glTexCoord2f(16/256, 114/256);   glVertex2f(X + FScrollBar.Left - 1,  -Y - (K + (J Shr 16)) * FTextHeight - 2);
          glTexCoord2f(16/256, 110/256);   glVertex2f(X + FScrollBar.Left - 1,  -Y - K * FTextHeight - 2);
        glEnd;
      End;

      // Paint the text

      I := FTopIndex;
      K := 0;
      While (I < FTopIndex + DisplayableItems) And (I < FItems.Count) Do
      Begin
        If I = FItemIndex
         Then DefaultColor := FSelectionForeground
         Else DefaultColor := FForeGround;
        glColor4ub(TRGBA(DefaultColor).R,TRGBA(DefaultColor).G,TRGBA(DefaultColor).B,TGLWindow(FOwner).FAlpha);
        If FWordWrapped Then
        Begin
          GetWordWrappedText(FItems.Strings[I]);
          For J := 0 To FWordWrappedText.Count - 1 Do
          Begin
            OutputText(X + 2,Y + 2 + K * FTextHeight + FTextHeight,FWordWrappedText.Strings[J],DefaultColor);
            Inc(K);
          End; // For J
        End
        Else OutputText(X + 2,Y + 2 + (I - FTopIndex) * FTextHeight + FTextHeight,FItems.Strings[I],DefaultColor);
        Inc(I);
      End; // While

      // Stop clipping

      glDisable(GL_SCISSOR_TEST);
      glScissor(0,TSceneGL(TGLUI(FUI).FOwner).WindowHeight,TSceneGL(TGLUI(FUI).FOwner).WindowWidth,TSceneGL(TGLUI(FUI).FOwner).WindowHeight);
    End;
  glPopMatrix;
End; // TGLListBox.Paint

Procedure TGLListBox.OutputText(X,Y: Integer; St: String; DefaultColor: TColor);
// This method relies on the fact that we are using Pascal-style strings, NOT
// null-terminated strings!
Type
  TRGBA = Packed Record
    B,G,R,A: Byte;
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
        FCurrentColor := C;
        glColor4ub(TRGBA(FCurrentColor).R,TRGBA(FCurrentColor).G,TRGBA(FCurrentColor).B,TGLWindow(FOwner).FAlpha);
        Inc(J,4);
        I := J;
      End
      Else If St[J] = LastColorCommand Then
      Begin
        WriteText;
        FCurrentColor := FLastColor;
        glColor4ub(TRGBA(FCurrentColor).R,TRGBA(FCurrentColor).G,TRGBA(FCurrentColor).B,TGLWindow(FOwner).FAlpha);
        Inc(J);
        I := J;
      End
      Else
      Begin
        WriteText;
        FLastColor    := FCurrentColor;
        FCurrentColor := DefaultColor;
        glColor4ub(TRGBA(FCurrentColor).R,TRGBA(FCurrentColor).G,TRGBA(FCurrentColor).B,TGLWindow(FOwner).FAlpha);
        Inc(J);
        I := J;
      End;
    End;
  End; // While
  WriteText;
End; // TGLListBox.OutputText

Procedure TGLListBox.CreateChildren;
Begin
  FScrollBar := TGLScrollBar.Create(Self,FWidth - ScrollBarThickness,0,ScrollBarThickness,FHeight,sbVertical);
End; // TGLListBox.CreateChildren

// -------------------------------
// TGLStringGrid
// -------------------------------

Constructor TGLStringGrid.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
Var I: Integer;
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,False);
  FFixedForeground := DefaultFixedForeground;
  FFixedBackground := DefaultFixedBackground;
  FFixedRows       := 1;
  FCols            := 5;
  SetLength(FColWidths,FCols);
  For I := 0 To High(FColWidths) Do FColWidths[I] := 64;
  FComponentType := ctStringGrid;
End; // TGLStringGrid.Create

Function TGLStringGrid.GetAvailableVertPixels: Integer;
Begin
  Result := Max(0,FHeight - 4 - FFixedRows * FTextHeight);
End; // TGLStringGrid.GetAvailableVertPixels

{
Function TGLStringGrid.GetMaxTopIndex: Integer;
Begin
  Result := FItems.Count + FFixedRows - ((FHeight - 4) Div FTextHeight);
End; // TGLStringGrid.GetMaxTopIndex
}
Procedure TGLStringGrid.Paint(X,Y: Integer);
Var
  I,J,K            : Integer;
  MaxTopIndex      : Integer;
  DisplayableItems : Integer;
  Rect             : TRect;
  DefaultColor     : TColor;

Begin
  glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
  glPushMatrix;
    glColor4ub(255,255,255,TGLWindow(FOwner).FAlpha);
    glTranslatef(0,0,SepOne);
    FTextHeight := TextHeight('Mg');
    If FTextHeight > 0 Then
    Begin
      // Fix up the top index

      MaxTopIndex := GetMaxTopIndex + 1;
      If MaxTopIndex < 0 Then MaxTopIndex := 0;
      If FTopIndex > MaxTopIndex Then FTopIndex := MaxTopIndex;

      DisplayableItems   := GetDisplayableItems;
//      If DisplayableItems * FTextHeight < FHeight Then Inc(DisplayableItems);
//      Dec(DisplayableItems,FFixedRows);

      // Clip to the text area

      Rect.Left   := X + FOwner.Left;
      Rect.Top    := Y + FOwner.Top;
      Rect.Right  := Rect.Left + Width  - 1 - FScrollBar.Width;
      Rect.Bottom := Rect.Top  + Height - 1;

      glScissor(Rect.Left, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - Rect.Bottom, Rect.Right - Rect.Left + 1, Rect.Bottom - Rect.Top + 1);
      glEnable(GL_SCISSOR_TEST);

      // Paint a selection row

      If (FItemIndex >= FTopIndex) And (FItemIndex < FTopIndex + DisplayableItems) Then
      Begin
        I := FTopIndex;
        K := FixedRows;
        While I < FItemIndex Do
        Begin
          J := Integer(FItems.Objects[I]);
          Inc(K,J Shr 16);
          Inc(I);
        End; // While
        J := Integer(FItems.Objects[FItemIndex]);
        glColor4ub(TRGBA(FSelectionBackground).R,TRGBA(FSelectionBackground).G,TRGBA(FSelectionBackground).B,TGLWindow(FOwner).FAlpha);
        glBegin(GL_QUADS);
          glTexCoord2f(12/256, 110/256);   glVertex2f(X + 1,                    -Y - K * FTextHeight - 2);
          glTexCoord2f(12/256, 114/256);   glVertex2f(X + 1,                    -Y - (K + (J Shr 16)) * FTextHeight - 2);
          glTexCoord2f(16/256, 114/256);   glVertex2f(X + FScrollBar.Left - 1,  -Y - (K + (J Shr 16)) * FTextHeight - 2);
          glTexCoord2f(16/256, 110/256);   glVertex2f(X + FScrollBar.Left - 1,  -Y - K * FTextHeight - 2);
        glEnd;
      End;

      // Paint the fixed rows

      I := 0;
      While (I < FFixedRows) And (I < FItems.Count) Do
      Begin
        glColor4ub(TRGBA(FFixedForeground).R,TRGBA(FFixedForeground).G,TRGBA(FFixedForeground).B,TGLWindow(FOwner).FAlpha);
        OutputText(X + 2,Y + 2 + I * FTextHeight + FTextHeight,FItems.Strings[I],DefaultColor);
        Inc(I);
      End; // While

      // Paint the text

      I := FTopIndex;
      While (I < FTopIndex + DisplayableItems) And (I < FItems.Count - FFixedRows) Do
      Begin
        If I = FItemIndex
         Then DefaultColor := FSelectionForeground
         Else DefaultColor := FForeGround;
        glColor4ub(TRGBA(DefaultColor).R,TRGBA(DefaultColor).G,TRGBA(DefaultColor).B,TGLWindow(FOwner).FAlpha);
        OutputText(X + 2,Y + 2 + (I - FTopIndex + FFixedRows) * FTextHeight + FTextHeight,FItems.Strings[I + FFixedRows],DefaultColor);
        Inc(I);
      End; // While

      // Stop clipping

      glDisable(GL_SCISSOR_TEST);
      glScissor(0,TSceneGL(TGLUI(FUI).FOwner).WindowHeight,TSceneGL(TGLUI(FUI).FOwner).WindowWidth,TSceneGL(TGLUI(FUI).FOwner).WindowHeight);
    End;
  glPopMatrix;
End; // TGLStringGrid.Paint

Procedure TGLStringGrid.OutputText(X,Y: Integer; St: String; DefaultColor: TColor);
// This method relies on the fact that we are using Pascal-style strings, NOT
// null-terminated strings!
Var I,J,K: Integer;

  Procedure WriteText;
  Begin
    TextOut(X,Y,Copy(St,I,J - I));
    Inc(X,ColWidths[K]);
    Inc(K);
  End; // WriteText

Begin
  I := 1;
  J := 1;
  K := 0;
  While J <= Length(St) Do
  Begin
    If St[J] <> #254 Then Inc(J) Else
    Begin
      WriteText;
      Inc(J);
      I := J;
    End;
  End; // While
  WriteText;
End; // TGLStringGrid.OutputText

Procedure TGLStringGrid.AfterSetSize;
Var MaxTopIndex: Integer;
Begin
  MaxTopIndex := GetMaxTopIndex;
  If MaxTopIndex < 0 Then MaxTopIndex := 0;
  FScrollBar.Max := MaxTopIndex;
  FScrollBar.PageSize := ((FHeight - 4) Div FTextHeight) - FFixedRows;
End; // TGLStringGrid.AfterSetSize

Procedure TGLStringGrid.MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  I,J,K,L,XX,YY : Integer;
  Found         : Boolean;

Begin
  Try
    FUI.LockWindowList;
    If FEnabled Then
    Begin
      If FTextHeight > 0 Then
      Begin
        CalcAbsoluteXY(XX,YY);
        If Y >= YY + 2 Then
        Begin
          I     := ((Y - YY - 2) Div FTextHeight);
          If I >= FFixedRows Then
          Begin
            Dec(I,FFixedRows);
            J     := FTopIndex;
            L     := 0;
            Found := False;
            While (J < FItems.Count) And Not Found Do
            Begin
              K := Integer(FItems.Objects[J]);
              If I < L + (K Shr 16) Then Found := True
              Else
              Begin
                Inc(L,K Shr 16);
                Inc(J);
              End;
            End; // While
            If Found
             Then ItemIndex := J
             Else ItemIndex := -1;
          End
          Else ItemIndex := -1;
        End;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TGLStringGrid.MouseUpAction

Procedure TGLStringGrid.SetFixedRows(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    If I <> FFixedRows Then
    Begin
      FFixedRows := I;
      AfterSetSize;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TGLStringGrid.SetFixedRows

Procedure TGLStringGrid.SetCols(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    If (I > 0) And (I <> FCols) Then
    Begin
      FCols := I;
      SetLength(FColWidths,FCols);
      For I := 0 To High(FColWidths) Do FColWidths[I] := 64;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TGLStringGrid.SetCols

Function TGLStringGrid.GetColWidths(I: Integer): Integer;
Begin
  Try
    FUI.LockWindowList;
    If (I >= 0) And (I < FCols) Then
    Begin
      Result := FColWidths[I];
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TGLStringGrid.GetColWidths

Procedure TGLStringGrid.SetColWidths(I,J: Integer);
Begin
  Try
    FUI.LockWindowList;
    If (I >= 0) And (I < FCols) And (J >= 0) Then
    Begin
      FColWidths[I] := J;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TGLStringGrid.SetColWidths

Procedure TGLStringGrid.SetFixedForeground(C: TColor);
Begin
  Try
    FUI.LockWindowList;
    If C <> FFixedForeground Then
    Begin
      FFixedForeground := C;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TGLStringGrid.SetFixedForeground

Procedure TGLStringGrid.SetFixedBackground(C: TColor);
Begin
  Try
    FUI.LockWindowList;
    If C <> FFixedBackground Then
    Begin
      FFixedBackground := C;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TGLStringGrid.SetFixedBackground

// -------------------------------
// TGLPageControl
// -------------------------------

Constructor TGLPageControl.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight);
End; // TGLPageControl.Create

Function TGLPageControl.GetTabHeight: Integer;
Begin
  Result := 20;
End; // TGLPageControl.GetTabHeight

Function TGLPageControl.GetTabWidth(Index: Integer): Integer;
Begin
  If (Index >= 0) And (Index < FPages.Count)
   Then Result := TextWidth(FPages.Strings[Index]) + 16
   Else Result := 0;
End; // TGLPageControl.GetTabWidth

Function TGLPageControl.CreatePagePanel(PageCaption: String): TAbstractPanel;
Begin
  Result := TGLPanel.Create(Self,0,0,ClientWidth,ClientHeight,'');
End; // TGLPageControl.CreatePagePanel

Procedure TGLPageControl.Paint(X,Y: Integer);
Var
  I       : Integer;
  TabInfo : PTabInfo;
  W,H     : Integer;
  Row     : Integer;

Begin
  glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
  glPushMatrix;
    glTranslatef(0,0,2 * SepOne);
    H := GetTabHeight;
    For I := 0 To FTabInfo.Count - 1 Do
    Begin
      glColor4ub(TRGBA(FForeground).R,TRGBA(FForeground).G,TRGBA(FForeground).B,TGLWindow(FOwner).FAlpha);
      TabInfo := PTabInfo(FTabInfo.Items[I]);
      Row     := FRows - 1 - ((TabInfo.Row + (FRows - 1 - FTopTabRow)) Mod FRows);
      W       := TabInfo.Width - 16; 

      glBegin(GL_QUADS);

        // Left end

        glTexCoord2f(0/256, 192/256); glVertex2f(X + TabInfo.X, -Y - Row * H);
        glTexCoord2f(0/256, 212/256); glVertex2f(X + TabInfo.X, -Y - (Row + 1) * H);
        glTexCoord2f(8/256, 212/256); glVertex2f(X + TabInfo.X + 8, -Y - (Row + 1) * H);
        glTexCoord2f(8/256, 192/256); glVertex2f(X + TabInfo.X + 8, -Y - Row * H);

        // Center

        glTexCoord2f(4/256,  192/256); glVertex2f(X + TabInfo.X + 8, -Y - Row * H);
        glTexCoord2f(4/256,  212/256); glVertex2f(X + TabInfo.X + 8, -Y - (Row + 1) * H);
        glTexCoord2f(13/256, 212/256); glVertex2f(X + TabInfo.X + 8 + W, -Y - (Row + 1) * H);
        glTexCoord2f(13/256, 192/256); glVertex2f(X + TabInfo.X + 8 + W, -Y - Row * H);

        // Right end

        glTexCoord2f(8/256,  192/256); glVertex2f(X + TabInfo.X + 8 + W, -Y - Row * H);
        glTexCoord2f(8/256,  212/256); glVertex2f(X + TabInfo.X + 8 + W, -Y - (Row + 1) * H);
        glTexCoord2f(16/256, 212/256); glVertex2f(X + TabInfo.X + 16 + W, -Y - (Row + 1) * H);
        glTexCoord2f(16/256, 192/256); glVertex2f(X + TabInfo.X + 16 + W, -Y - Row * H);
      glEnd;

      // Output the label

      If FActivePageIndex <> I Then glColor4ub(192,192,192,TGLWindow(FOwner).FAlpha);

      TextOut(X + TabInfo.X + 8 + (W - Min(TextWidth(FPages.Strings[I]),W)) Div 2,Y + Row * H + TextHeight('Mg') + (GetTabHeight - TextHeight('Mg')) Div 2,FPages.Strings[I]);
    End;
  glPopMatrix;
End; // TGLPageControl.Paint

// -------------------------------
// TGLLabel
// -------------------------------

Constructor TGLLabel.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; ATransparent: Boolean);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,ACaption,ATransparent);
End; // TGLLabel.Create

Constructor TGLLabel.Create(AParent: TAbstractComponent; ALeft,ATop: Integer; ACaption: String; ATransparent: Boolean);
Begin
  Inherited Create(AParent,ALeft,ATop,ACaption,ATransparent);
End; // TGLLabel.Create

Procedure TGLLabel.FillBackground;
Var X,Y: Integer;
Begin
  If Not FTransparent Then
  Begin
    CalcAbsoluteXY(X,Y);
    glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
    glPushMatrix;
      glColor4ub(TRGBA(FBackground).R,TRGBA(FBackground).G,TRGBA(FBackground).B,TGLWindow(FOwner).FAlpha);
      glTranslatef(0,0,SepOne);
      glBegin(GL_QUADS);
        glTexCoord2f(12/256, 110/256); glVertex2f(X, -Y);
        glTexCoord2f(12/256, 114/256); glVertex2f(X, -Y - Height);
        glTexCoord2f(16/256, 114/256); glVertex2f(X + Width, -Y - Height);
        glTexCoord2f(16/256, 110/256); glVertex2f(X + Width, -Y);
      glEnd;
    glPopMatrix;
  End;
End; // TGLLabel.FillBackground

Procedure TGLLabel.Paint(X,Y: Integer);
Var
  I,J,K : Integer;
  St    : String;
  
Begin
  glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
  glPushMatrix;
    glColor4ub(TRGBA(FForeground).R,TRGBA(FForeground).G,TRGBA(FForeground).B,TGLWindow(FOwner).FAlpha);
    glTranslatef(0,0,2 * SepOne);
    If FWordWrapped Then
    Begin
      J := 0;
      For I := 0 To FWordWrappedText.Count - 1 Do J := Max(J,TextWidth(FWordWrappedText.Strings[I]));
      K := TextHeight('Mg');
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
         taLeftJustify: TextOut(X,Y + TextHeight('Mg'),FCaption);
        taRightJustify: TextOut(X + Width - TextWidth(FCaption),Y + TextHeight('Mg'),FCaption);
              taCenter: TextOut(X + (Width - TextWidth(FCaption)) Div 2,Y + TextHeight('Mg'),FCaption);
      End; // Case
    End;
//    TextOut(X,Y + TextHeight('Mg'), Caption);
  glPopMatrix;
End; // TGLLabel.Paint

// -------------------------------
// TGLPanel
// -------------------------------

Constructor TGLPanel.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,ACaption);
  FTexX1 := 72;
  FTexY1 := 102;
  FTexX2 := 85;
  FTexY2 := 115;
End; // TGLPanel.Create

Procedure TGLPanel.CreateChildren;
Begin
  FLabel := TGLLabel.Create(Self,0,0,'',True);
End; // TGLPanel.CreateChildren

Procedure TGLPanel.BeforePaintingChildren;
Begin
  glPushMatrix;
    glTranslatef(0,0,3 * SepOne);
End; // TGLPanel.BeforePaintingChildren

Procedure TGLPanel.AfterPaintingChildren;
Begin
  glPopMatrix;
End; // TGLPanel.AfterPaintingChildren

Procedure TGLPanel.Paint(X,Y: Integer);
Begin
 
  glPushMatrix;
    glColor4ub(TRGBA(FForeground).R,TRGBA(FForeground).G,TRGBA(FForeground).B,TGLWindow(FOwner).FAlpha);
//    glColor4ub(255,255,255,TGLWindow(FOwner).FAlpha);
//    glTranslatef(TCanvasWindow(FOwner).Left, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - TCanvasWindow(FOwner).Top, -TCanvasWindow(FOwner).ZOrder * 100 * SepOne);

    If FFillBackground Then
    Begin
      // Draw the main body of the panel

      glTranslatef(0,0,SepOne);
      glBindTexture(GL_TEXTURE_2D, TGLUI(UI).BkgdTexture);
      glBegin(GL_QUADS);
        glTexCoord2f(0/256, 0/256);            glVertex2f(X, -Y);
        glTexCoord2f(0/256, Height/256);       glVertex2f(X, -Y - Height);
        glTexCoord2f(Width/256, Height/256);   glVertex2f(X + Width, -Y - Height);
        glTexCoord2f(Width/256, 0/256);        glVertex2f(X + Width, -Y);
      glEnd;
    End;

    If FShowBorder Then
    Begin
      glTranslatef(0,0,SepOne);
      glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);

      // Top edge

      glBegin(GL_QUADS);
        glTexCoord2f(FTexX1/256,       FTexY1/256); glVertex2f(X,         -Y);
        glTexCoord2f(FTexX1/256,       (FTexY1 + 2)/256); glVertex2f(X,         -Y - 2);
        glTexCoord2f((FTexX2 + 1)/256, (FTexY1 + 2)/256); glVertex2f(X + Width, -Y - 2);
        glTexCoord2f((FTexX2 + 1)/256, FTexY1/256); glVertex2f(X + Width, -Y);
      glEnd;

      // Bottom edge

      glBegin(GL_QUADS);
        glTexCoord2f(FTexX1/256,       (FTexY2 - 1)/256); glVertex2f(X,         -Y - Height + 2);
        glTexCoord2f(FTexX1/256,       (FTexY2 + 1)/256); glVertex2f(X,         -Y - Height);
        glTexCoord2f((FTexX2 + 1)/256, (FTexY2 + 1)/256); glVertex2f(X + Width, -Y - Height);
        glTexCoord2f((FTexX2 + 1)/256, (FTexY2 - 1)/256); glVertex2f(X + Width, -Y - Height + 2);
      glEnd;

      // Left edge

      glBegin(GL_QUADS);
        glTexCoord2f(FTexX1/256,       FTexY1/256); glVertex2f(X,     -Y);
        glTexCoord2f(FTexX1/256,       (FTexY2 + 1)/256); glVertex2f(X,     -Y - Height);
        glTexCoord2f((FTexX1 + 2)/256, (FTexY2 + 1)/256); glVertex2f(X + 2, -Y - Height);
        glTexCoord2f((FTexX1 + 2)/256, FTexY1/256); glVertex2f(X + 2, -Y);
      glEnd;

      // Right edge

      glBegin(GL_QUADS);
        glTexCoord2f((FTexX2 - 1)/256, FTexY1/256); glVertex2f(X + Width - 2, -Y);
        glTexCoord2f((FTexX2 - 1)/256, (FTexY2 + 1)/256); glVertex2f(X + Width - 2, -Y - Height);
        glTexCoord2f((FTexX2 + 1)/256, (FTexY2 + 1)/256); glVertex2f(X + Width,     -Y - Height);
        glTexCoord2f((FTexX2 + 1)/256, FTexY1/256); glVertex2f(X + Width,     -Y);
      glEnd;
    End;
  glPopMatrix;
End; // TGLPanel.Paint

// -------------------------------
// TGLProgressBar
// -------------------------------

Constructor TGLProgressBar.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AValue: Single; AColor: TColor);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight,AValue,AColor);
End; // TGLProgressBar.Create

Procedure TGLProgressBar.Paint(X,Y: Integer);
Var I: Integer;
Begin
  glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
  glPushMatrix;
//    glTranslatef(TCanvasWindow(FOwner).Left, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - TCanvasWindow(FOwner).Top, -TCanvasWindow(FOwner).ZOrder * 100 * SepOne);
    glTranslatef(0,0,SepOne);

    // Draw the volume in the tube

    glColor4ub(TRGBA(FColor).R,TRGBA(FColor).G,TRGBA(FColor).B,TGLWindow(FOwner).FAlpha);
    I := Trunc(Max(0,Value) * (Width - 2));
    If I > 0 Then
    Begin
      glBegin(GL_QUADS);
        glTexCoord2f( 6/256, 100/256);   glVertex2f(X + 1,     -Y - 1);
        glTexCoord2f( 6/256, 116/256);   glVertex2f(X + 1,     -Y - Height + 1);
        glTexCoord2f(10/256, 116/256);   glVertex2f(X + I + 1, -Y - Height + 1);
        glTexCoord2f(10/256, 100/256);   glVertex2f(X + I + 1, -Y - 1);
      glEnd;
    End;

    // Draw the tube

    glTranslatef(0,0,SepOne);
    glColor4ub(255,255,255,TGLWindow(FOwner).FAlpha);

    // Left cap

    glBegin(GL_QUADS);
      glTexCoord2f(12/256, 100/256);   glVertex2f(X,     -Y);
      glTexCoord2f(12/256, 108/256);   glVertex2f(X,     -Y - Height);
      glTexCoord2f(20/256, 108/256);   glVertex2f(X + 8, -Y - Height);
      glTexCoord2f(20/256, 100/256);   glVertex2f(X + 8, -Y);
    glEnd;

    // Center portion

    glBegin(GL_QUADS);
      glTexCoord2f(20/256, 100/256);   glVertex2f(X + 8,         -Y);
      glTexCoord2f(20/256, 108/256);   glVertex2f(X + 8,         -Y - Height);
      glTexCoord2f(28/256, 108/256);   glVertex2f(X + Width - 8, -Y - Height);
      glTexCoord2f(28/256, 100/256);   glVertex2f(X + Width - 8, -Y);
    glEnd;

    // Right cap

    glBegin(GL_QUADS);
      glTexCoord2f(28/256, 100/256);   glVertex2f(X + Width - 8, -Y);
      glTexCoord2f(28/256, 108/256);   glVertex2f(X + Width - 8, -Y - Height);
      glTexCoord2f(36/256, 108/256);   glVertex2f(X + Width - 1, -Y - Height);
      glTexCoord2f(36/256, 100/256);   glVertex2f(X + Width - 1, -Y);
    glEnd;

  glPopMatrix;
End; // TGLProgressBar.Paint

// -------------------------------
// TGLScrollPane
// -------------------------------

Constructor TGLScrollPane.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
Begin
  Inherited Create(AParent,ALeft,ATop,AWidth,AHeight);
End; // TGLScrollPane.Create

Procedure TGLScrollPane.CreateChildren;
Begin
  FVertScrollBar          := Nil;
  FHorzScrollBar          := Nil;
  FVertScrollBar          := TGLScrollBar.Create(Self,0,0,ScrollBarThickness,50,sbVertical);
  FHorzScrollBar          := TGLScrollBar.Create(Self,0,0,50,ScrollBarThickness,sbHorizontal);
  FVertScrollBar.Min      := 0;
  FHorzScrollBar.Min      := 0;
  FVertScrollBar.OnChange := VertScrollBarChange;
  FHorzScrollBar.OnChange := HorzScrollBarChange;
  AfterSetSize;
End; // TGLScrollPane.CreateChildren

Procedure TGLScrollPane.Repaint;
Var
  X,Y            : Integer;
  ChildComponent : TAbstractComponent;
  MaxX           : Integer;
  MaxY           : Integer;
  Rect           : TRect;
  Rect1          : TRect;
  OldClipRect    : TRect;

Begin
  Try
    FUI.LockWindowList;

    // Clip to this component's extents

    CalcAbsoluteXY(X,Y);
    Rect        := GetClipRect;
//    BeginClip(Rect);
    OldClipRect := FClipRect;
    FClipRect   := Rect;


    If FVisible Then
    Begin
      If Assigned(FOnBeforePaint) Then FOnBeforePaint(Self,X,Y);

      glPushMatrix;
      glTranslatef(0,0,SepOne);
      FVertScrollBar.Repaint;
      glTranslatef(0,0,SepOne);
      FHorzScrollBar.Repaint;
      glPopMatrix;

      If FVertScrollBar.Visible
       Then MaxX := FVertScrollBar.Left
       Else MaxX := FClientWidth;

      If FHorzScrollBar.Visible
       Then MaxY := FHorzScrollBar.Top
       Else MaxY := FClientHeight;

      Rect1.Left   := X + FOwner.Left;
      Rect1.Top    := Y + FOwner.Top;
      Rect1.Right  := Rect1.Left + MaxX - 1;
      Rect1.Bottom := Rect1.Top  + MaxY - 1;

      glScissor(Rect1.Left, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - Rect1.Bottom, Rect1.Right - Rect1.Left + 1, Rect1.Bottom - Rect1.Top + 1);
      glEnable(GL_SCISSOR_TEST);
      glPushMatrix;
      glTranslatef(0,0,SepOne);

//      BeginClip(Rect1);
      ChildComponent := Child;
      If ChildComponent <> Nil Then ChildComponent.Repaint;
//      EndClip;

      glPopMatrix;
      glDisable(GL_SCISSOR_TEST);
      glScissor(0,TSceneGL(TGLUI(FUI).FOwner).WindowHeight,TSceneGL(TGLUI(FUI).FOwner).WindowWidth,TSceneGL(TGLUI(FUI).FOwner).WindowHeight);

      // See if the optional event handler was set

      If Assigned(FOnPaint) Then FOnPaint(Self,X,Y);
    End;
    FClipRect := OldClipRect;
//    EndClip;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TGLScrollPane.Repaint

// -------------------------------
// TGLWindow
// -------------------------------

Constructor TGLWindow.Create(ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; AUI: TAbstractUI);
Begin
  Inherited Create(ALeft,ATop,AWidth,AHeight,ACaption,AUI);
  FAlpha       := 255;
  FBorderWidth := BorderThickness;
End; // TGLWindow.Create

Destructor TGLWindow.Destroy;
Begin
  Inherited;
End; // TGLWindow.Destroy

Procedure TGLWindow.Paint(X,Y: Integer);
Var H,Offset: Integer;
Begin
  glBindTexture(GL_TEXTURE_2D, TGLUI(UI).GUITexture);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glDepthMask(TRUE);

  glPushMatrix;
    glColor4ub(255,255,255,FAlpha);
    glTranslatef(X + FLeft, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - (Y + FTop), -(ZOrder + 1) * 100 * SepOne);
    H := TextHeight(Caption);
    If FShowBorder Then
    Begin
      glBegin(GL_QUADS);
        // top left corner of window.
        If FShowCaption Then
        Begin
          glTexCoord2f( 0/256, 0);      glVertex2f(0, 0);
          glTexCoord2f( 0/256, 27/256); glVertex2f(0, -27);
          glTexCoord2f(16/256, 27/256); glVertex2f(16, -27);
          glTexCoord2f(16/256, 0);      glVertex2f(16, 0);
        End
        Else
        Begin
          glTexCoord2f( 0/256, 0);      glVertex2f(0, 0);
          glTexCoord2f( 0/256, FBorderWidth/256); glVertex2f(0, -FBorderWidth);
          glTexCoord2f(16/256, FBorderWidth/256); glVertex2f(16, -FBorderWidth);
          glTexCoord2f(16/256, 0);      glVertex2f(16, 0);
        End;

        // top of window.
        If FShowCaption Then
        Begin
          glTexCoord2f(16/256, 0);      glVertex2f(16, 0);
          glTexCoord2f(16/256, 27/256); glVertex2f(16, -27);
          glTexCoord2f(96/256, 27/256); glVertex2f(Width-32, -27);
          glTexCoord2f(96/256, 0);      glVertex2f(Width-32, 0);
        End
        Else
        Begin
          glTexCoord2f(16/256, 0);      glVertex2f(16, 0);
          glTexCoord2f(16/256, FBorderWidth/256); glVertex2f(16, -FBorderWidth);
          glTexCoord2f(96/256, FBorderWidth/256); glVertex2f(Width-32, -FBorderWidth);
          glTexCoord2f(96/256, 0);      glVertex2f(Width-32, 0);
        End;

        // top right corder of window.
        If FShowCaption Then
        Begin
          glTexCoord2f(96/256, 0);        glVertex2f(Width-32, 0);
          glTexCoord2f(96/256, 27/256); glVertex2f(Width-32, -27);
          glTexCoord2f(128/256, 27/256);      glVertex2f(Width, -27);
          glTexCoord2f(128/256, 0);             glVertex2f(Width, 0);
        End
        Else
        Begin
          glTexCoord2f(96/256, 0);        glVertex2f(Width-32, 0);
          glTexCoord2f(96/256, FBorderWidth/256); glVertex2f(Width-32, -FBorderWidth);
          glTexCoord2f(128/256, FBorderWidth/256);      glVertex2f(Width, -FBorderWidth);
          glTexCoord2f(128/256, 0);             glVertex2f(Width, 0);
        End;

        // left side of window.
        If FShowCaption Then
        Begin
          glTexCoord2f(0/256, 27/256);  glVertex2f(0, -27);
          glTexCoord2f(0/256, (128-27)/256);    glVertex2f(0, -Height+FBorderWidth);
          glTexCoord2f(FBorderWidth/256, (128-27)/256);    glVertex2f(FBorderWidth,-Height+FBorderWidth);
          glTexCoord2f(FBorderWidth/256, 27/256);  glVertex2f(FBorderWidth, -27);
        End
        Else
        Begin
          glTexCoord2f(0/256, FBorderWidth/256);  glVertex2f(0, -FBorderWidth);
          glTexCoord2f(0/256, (128-FBorderWidth)/256);    glVertex2f(0, -Height+FBorderWidth);
          glTexCoord2f(FBorderWidth/256, (128-FBorderWidth)/256);    glVertex2f(FBorderWidth,-Height+FBorderWidth);
          glTexCoord2f(FBorderWidth/256, FBorderWidth/256);  glVertex2f(FBorderWidth, -FBorderWidth);
        End;

        // right side of window.
        If FShowCaption Then
        Begin
          glTexCoord2f((128 - FBorderWidth)/256, 27/256);glVertex2f(Width-FBorderWidth, -27);
          glTexCoord2f((128 - FBorderWidth)/256, (128-27)/256);  glVertex2f(Width-FBorderWidth, -Height+FBorderWidth);
          glTexCoord2f(128/256,   (128-27)/256);      glVertex2f(Width,-Height+FBorderWidth);
          glTexCoord2f(128/256, 27/256);      glVertex2f(Width, -27);
        End
        Else
        Begin
          glTexCoord2f((128 - FBorderWidth)/256, FBorderWidth/256);glVertex2f(Width-FBorderWidth, -FBorderWidth);
          glTexCoord2f((128 - FBorderWidth)/256, (128-FBorderWidth)/256);  glVertex2f(Width-FBorderWidth, -Height+FBorderWidth);
          glTexCoord2f(128/256,   (128-FBorderWidth)/256);      glVertex2f(Width,-Height+FBorderWidth);
          glTexCoord2f(128/256, FBorderWidth/256);      glVertex2f(Width, -FBorderWidth);
        End;

        // bottom left corner of window.
        glTexCoord2f( 0/256, (128-FBorderWidth)/256);   glVertex2f(0, -Height+FBorderWidth);
        glTexCoord2f( 0/256, 128/256);                  glVertex2f(0, -Height);
        glTexCoord2f(16/256, 128/256);                  glVertex2f(16, -Height);
        glTexCoord2f(16/256, (128-FBorderWidth)/256);   glVertex2f(16, -Height+FBorderWidth);

        // bottom of window.
        glTexCoord2f(16/256, (128-FBorderWidth)/256);   glVertex2f(16, -Height+FBorderWidth);
        glTexCoord2f(16/256, 128/256);                  glVertex2f(16, -Height);
        glTexCoord2f(96/256, 128/256);                  glVertex2f(Width-32, -Height);
        glTexCoord2f(96/256, (128-FBorderWidth)/256);   glVertex2f(Width-32, -Height+FBorderWidth);

        // bottom right corder of window.
        glTexCoord2f(96/256, (128-FBorderWidth)/256);   glVertex2f(Width-32, -Height+FBorderWidth);
        glTexCoord2f(96/256, 128/256);    glVertex2f(Width-32, -Height);
        glTexCoord2f(128/256, 128/256);         glVertex2f(Width, -Height);
        glTexCoord2f(128/256, (128-FBorderWidth)/256);        glVertex2f(Width, -Height+FBorderWidth);
      glEnd;

      // draw the main body of the window
      glBindTexture(GL_TEXTURE_2D, TGLUI(UI).BkgdTexture);
      glBegin(GL_QUADS);
        glTexCoord2f(0/256, 0/256);                                              glVertex2f(FBorderWidth, 0);
        glTexCoord2f(0/256, (Height - FBorderWidth)/256);                        glVertex2f(FBorderWidth, -Height+FBorderWidth);
        glTexCoord2f((Width - FBorderWidth)/256, (Height - FBorderWidth)/256);   glVertex2f(Width-FBorderWidth, -Height+FBorderWidth);
        glTexCoord2f((Width - FBorderWidth)/256, 0/256);                         glVertex2f(Width-FBorderWidth, 0);
      glEnd;
    End;
    If FShowCaption Then
    Begin
      Offset              := FClientTop - 1;
      FCloseButton.Top    := -FClientTop + BorderThickness + 2;
      FCloseButton.Width  := Offset - BorderThickness - 6;
      FCloseButton.Height := Offset - BorderThickness - 6;

      glColor4ub(TRGBA(FForeground).R,TRGBA(FForeground).G,TRGBA(FForeground).B,FAlpha);
      glTranslatef(0,0,SepOne);
      If FShowBorder
       Then TextOut((Width - TextWidth(Caption)) Div 2, BorderThickness + ((CaptionBarHeight - H) Div 2) + H, Caption)
       Else TextOut((Width - TextWidth(Caption)) Div 2, ((CaptionBarHeight - H) Div 2) + H, Caption);
    End;
    glColor4ub(255,255,255,255);
  glPopMatrix;
End; // TGLWindow.Paint

Procedure TGLWindow.CreateCloseButton;
Begin
  FCloseButton := TGLButton.Create(Self,2,2,11,11,cbkX,cbsButton,'');
End; // TGLWindow.CreateCloseButton

Function TGLWindow.TextWidth(Const St: String): Integer;
Var I,J: Integer;
Begin
  J := 0;
  For I := 1 To Length(St) Do Inc(J,TGLUI(FUI).FontWidth[Ord(St[I])]);
  Result := J;
End; // TGLWindow.TextWidth

Function TGLWindow.TextHeight(Const St: String): Integer;
Begin
  Result := 16;
End; // TGLWindow.TextHeight

Procedure TGLWindow.TextOut(X,Y: Integer; Const St: String);
Begin
  TGLUI(UI).glWrite(X,Y,St);
End; // TGLWindow.TextOut

Procedure TGLWindow.CalcClientExtents;
Var Offset: Integer;
Begin
  If FShowCaption Then Offset := CaptionBarHeight + FBorderWidth
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
End; // TGLWindow.CalcClientExtents

Procedure TGLWindow.BeginClip(Rect: TRect);
Begin
End; // TGLWindow.BeginClip

Procedure TGLWindow.EndClip;
Begin
End; // TGLWindow.EndClip

Procedure TGLWindow.BeforePaintingChildren;
Begin
  glPushMatrix;
    glTranslatef(Left, TSceneGL(TGLUI(FUI).FOwner).WindowHeight - Top, -(ZOrder + 1) * 100 * SepOne + 5 * SepOne);
End; // TGLWindow.BeforePaintingChildren

Procedure TGLWindow.AfterPaintingChildren;
Begin
  glPopMatrix;
End; // TGLWindow.AfterPaintingChildren

// -------------------------------
// TGLUI
// -------------------------------

Constructor TGLUI.Create(AOwner: TObject);
Begin
  Inherited Create;
  FOwner       := AOwner;
  FTextures    := TStringList.Create;
  FInitialized := False;
  LoadFontWidths;
End; // TGLUI.Create

Destructor TGLUI.Destroy;
Var I: Integer;
Begin
  For I := 0 To FTextures.Count - 1 Do FTextures.Objects[I].Free;
  FTextures.Free;
  Inherited;
End; // TGLUI.Destroy

Procedure TGLUI.glWrite(X,Y: Integer; St: String);
Var P: PChar;
Begin
  glBindTexture(GL_TEXTURE_2D, FontTexture);
  glPushMatrix;
    glTranslatef(X, -Y, 5 * SepOne);
    glListBase(FontList);
    P := PChar(St);
    glCallLists(Length(St), GL_BYTE, P);
  glPopMatrix;
  glBindTexture(GL_TEXTURE_2D, GUITexture);
End; // TGLUI.glWrite

Procedure TGLUI.LoadFontWidths;
Var F: File;
Begin
  If FileExists(ExtractFilePath(Application.ExeName) + 'font.fnt') Then
  Begin
    AssignFile(F,ExtractFilePath(Application.ExeName) + 'font.fnt');
    Reset(F,1);
    BlockRead(F,FontWidth[0],SizeOf(FontWidth));
    CloseFile(F);
  End;
End; // TGLUI.LoadFontWidths

Procedure TGLUI.BuildFont;
// Creates the font display list
Var
  I  : Integer;
  X  : glFloat;
  Y  : glFloat;
  XS : glFloat;

begin
  // Load the character widths from the font.fnt file

  If FileExists(ExtractFilePath(Application.ExeName) + 'font.fnt') Then
  Begin
    FontList := glGenLists(256);                 // Storage For 128 Characters
    For I := 0 to 255 do
    Begin
      X := (I Mod 16) / 16;                      // X Position Of Current Character
      Y := (I Div 16) / 16;                      // Y Position Of Current Character
      glNewList(FontList + I, GL_COMPILE);       // Start Building A List
      glBegin(GL_QUADS);
        XS := (16 - FontWidth[I]) / 2;
        If Frac(XS) <> 0 Then XS := Trunc(XS) + 1;
        XS := XS / 256;
        glTexCoord2f(X + XS,                                Y + 16 / 256);  glVertex2i(0, 0);
        glTexCoord2f(X + XS + FontWidth[I] / 256{ + 1 / 512}, Y + 16 / 256);  glVertex2i(FontWidth[I], 0);
        glTexCoord2f(X + XS + FontWidth[I] / 256{ + 1 / 512}, Y);             glVertex2i(FontWidth[I], 16);
        glTexCoord2f(X + XS,                                Y);             glVertex2i(0, 16);
      glEnd;
      glTranslatef(FontWidth[I], 0, 0);
      glEndList;
    End; // For I
  End;
End; // TGLUI.BuildFont

Function TGLUI.AddWindow(ACaption: String): TAbstractWindow;
Var Window: TAbstractWindow;
Begin
  Window    := TGLWindow.Create(0,0,64,64,ACaption,Self);
  Window.UI := Self;
  Try
    FWindowsMutex.Enter;
    FWindows.AddObject('',Window);
    Window.ZOrder := FWindows.Count - 1;
  Finally
    FWindowsMutex.Leave;
  End;
  Result := Window;
End; // TGLUI.AddWindow

Procedure TGLUI.InitGUI;
Begin
  LoadTexture(ExtractFilePath(Application.ExeName) + 'gui.png', FGUITexture, True);
  LoadTexture(ExtractFilePath(Application.ExeName) + 'font.png', FontTexture, True);
  LoadTexture(ExtractFilePath(Application.ExeName) + 'window_background.png', BkgdTexture, True);
  BuildFont;
End; // TGLUI.InitGUI

Function TGLUI.LoadTexture(FileName: String; Var Texture: GLUInt; FlipV: Boolean): Boolean;
Var Tex: TTexture;
Begin
  Tex := TTexture.Create(FOwner,Nil);
  Tex.MipMap := False;
  Tex.LoadTexture(FileName,'',FlipV);
  Tex.MinFilter := GL_NEAREST;
  Tex.MagFilter := GL_NEAREST;
  Tex.LoadTextureIntoOpenGL;
  Texture := Tex.ID;
  FTextures.AddObject('',Tex);
End; // TGLUI.LoadTexture

Procedure TGLUI.Repaint;
Var
  Ratio : GLFloat;
  Range : GLFloat;
  I     : Integer;
  
Begin
  glMatrixMode(GL_PROJECTION);  // Change Matrix Mode to Projection
  glLoadIdentity;             // Reset View
  glOrtho(0, TSceneGL(FOwner).WindowWidth, 0, TSceneGL(FOwner).WindowHeight, 0, 1000);
  glMatrixMode(GL_MODELVIEW);   // Change Projection to Matrix Mode
  glLoadIdentity;

  glTranslatef(0, 0, -100 * SepOne);
  glEnable(GL_TEXTURE_2D);
  glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
  glClear(GL_DEPTH_BUFFER_BIT); // Clear the depth buffer

  If Not FInitialized Then
  Begin
    InitGUI;
    FInitialized := True;
  End;

  Try
    FWindowsMutex.Enter;
    For I := 0 To FWindows.Count - 1 Do TAbstractWindow(FWindows.Objects[I]).Repaint;
  Finally
    FWindowsMutex.Leave;
  End;
  Try
    FQueueMutex.Enter;
    FRepaintQueue.Clear;
  Finally
    FQueueMutex.Leave;
  End;

  glMatrixMode(GL_PROJECTION);  // Change Matrix Mode to Projection
  glLoadIdentity;             // Reset View

  If TSceneGL(FOwner).FPerspective Then
  Begin
    gluPerspective(TSceneGL(FOwner).Angle,          // Field-of-view angle
                   TSceneGL(FOwner).Aspect,       // Aspect ratio of viewing volume
                   TSceneGL(FOwner).DistNear,       // Distance to near clipping plane
                   TSceneGL(FOwner).DistFar);       // Distance to far clipping plane
  End
  Else
  Begin
    // Orthogonal projection

    Range := 12;
    If TSceneGL(FOwner).WindowWidth <= TSceneGL(FOwner).WindowHeight Then
    Begin
      Ratio := TSceneGL(FOwner).WindowHeight / TSceneGL(FOwner).WindowWidth;
      GlOrtho(-Range,Range,-Range * Ratio,Range * Ratio,-Range * 4,Range * 4);
    End
    Else
    Begin
      Ratio := TSceneGL(FOwner).WindowWidth / TSceneGL(FOwner).WindowHeight;
      GlOrtho(-Range * Ratio,Range * Ratio,-Range,Range,-Range * 4,Range * 4);
    End;
  End;
  glViewport(0, 0, TSceneGL(FOwner).WindowWidth, TSceneGL(FOwner).WindowHeight);

  glMatrixMode(GL_MODELVIEW);   // Change Projection to Matrix Mode
  glLoadIdentity;
End; // TGLUI.Repaint

Initialization
  SepOne := 0.01;
End.

