unit frmChooseTextureUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, Buttons, ExtCtrls, ClearTypeText, OZDMUnit,
  IAeverButton;

Const BlitTexSize = 64;

type
  TLongArray = Packed Array[0..65536*16*100 - 1] Of LongWord;
  PLongArray = ^TLongArray;
  TfrmTextureChooser = class(TForm)
    Panel1: TPanel;
    btnOk: TIAEverButton;
    btnCancel: TIAEverButton;
    dgTextures: TDrawGrid;
    lblTextureName: TClearTypeLabel;
    procedure FormShow(Sender: TObject);
    procedure dgTexturesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure dgTexturesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    { Private declarations }
    BlitBufSize    : Integer;
    Procedure SetColsAndRows;
    Procedure InitBlitBuffer;
  public
    { Public declarations }
    DesiredTexture : Integer;
    BlitBuffer     : PLongArray;
    BlitBMP        : TBitmap;
    Procedure FlushBlitBuffer;
  end;

var
  frmTextureChooser: TfrmTextureChooser;

implementation

uses frmMainUnit, ZoneClasses, frmStatusUnit;

{$R *.dfm}

procedure TfrmTextureChooser.FormShow(Sender: TObject);
Var B: Boolean;
begin
  InitBlitBuffer;
  SetColsAndRows;
  B := True;
  dgTexturesSelectCell(Self, dgTextures.Col, dgTextures.Row, B);
end;

Procedure TfrmTextureChooser.SetColsAndRows;
Var I: Integer;
Begin
  I := (dgTextures.ClientWidth - 4) Div (dgTextures.DefaultColWidth + dgTextures.GridLineWidth);
  If I < 1 Then I := 1;
  dgTextures.ColCount := I;
  I := (TextureLibrary.Count - 1) Div I;
  If I * dgTextures.ColCount < (TextureLibrary.Count - 1) Then Inc(I);
  dgTextures.RowCount := I;
  If DesiredTexture >= 0 Then
  Begin
    dgTextures.Row := DesiredTexture Div dgTextures.ColCount;
    dgTextures.Col := DesiredTexture Mod dgTextures.ColCount;
  End;
End; // TfrmTextureChooser.SetColsAndRows

procedure TfrmTextureChooser.dgTexturesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
Type
  TRGBA = Packed Record
    R,G,B,A: Byte;
  End;
Var
  Index   : Integer;
  I,J,K,L : Integer;
  BS      : Integer;
  C       : TColor;

begin
  Index := ARow * dgTextures.ColCount + ACol;
  If (Index >= 0) And (Index < TextureLibrary.Count - 1) And (BlitBuffer <> Nil) Then
  Begin
    // Figure out the width, accounting for dword padding

    BS := (BlitTexSize Div 4) * 4;
    If BS < BlitTexSize Then Inc(BS,4);

    // Get the bitmap and transfer it to the swatch

    For I := 0 To BlitBMP.Height - 1 Do
     Move(BlitBuffer^[(Index * BS * BlitTexSize) + I * BS],PLongArray(BlitBMP.ScanLine[I])^[0],BS * 4);


    // Draw a selection (or non-selection) rectangle

    I := (dgTextures.DefaultColWidth - 3 - 64) Div 2;
    If gdSelected In State Then
    Begin
      J := 1;
      K := 0;
      dgTextures.Canvas.Draw(Rect.Left + I + 2,Rect.Top + I + 2,BlitBMP);

      dgTextures.Canvas.Pen.Color := cl3DDkShadow;
      dgTextures.Canvas.MoveTo(Rect.Left,Rect.Bottom - 1);
      dgTextures.Canvas.LineTo(Rect.Left,Rect.Top);
      dgTextures.Canvas.LineTo(Rect.Right - 1,Rect.Top);
      dgTextures.Canvas.Pen.Color := clBtnHighlight;
      dgTextures.Canvas.LineTo(Rect.Right - 1,Rect.Bottom - 1);
      dgTextures.Canvas.LineTo(Rect.Left,Rect.Bottom - 1);

      dgTextures.Canvas.Pen.Color := clBtnShadow;
      dgTextures.Canvas.MoveTo(Rect.Left + 1,Rect.Bottom - 2);
      dgTextures.Canvas.LineTo(Rect.Left + 1,Rect.Top + 1);
      dgTextures.Canvas.LineTo(Rect.Right - 1,Rect.Top + 1);

      C := TColor(ColorToRGB(dgTextures.Color));

      L := TRGBA(C).R + 64;
      If L > 255 Then L := 255;
      TRGBA(C).R := L;

      L := TRGBA(C).G - 16;
      If L < 0 Then L := 0;
      TRGBA(C).G := L;

      L := TRGBA(C).B - 16;
      If L < 0 Then L := 0;
      TRGBA(C).B := L;

      dgTextures.Canvas.Pen.Color := C;
    End
    Else
    Begin
      J := 0;
      K := 1;
      dgTextures.Canvas.Draw(Rect.Left + I + 1,Rect.Top + I + 1,BlitBMP);

      dgTextures.Canvas.Pen.Color := clBtnHighlight;
      dgTextures.Canvas.MoveTo(Rect.Left,Rect.Bottom - 1);
      dgTextures.Canvas.LineTo(Rect.Left,Rect.Top);
      dgTextures.Canvas.LineTo(Rect.Right - 1,Rect.Top);
      dgTextures.Canvas.Pen.Color := cl3DDkShadow;
      dgTextures.Canvas.LineTo(Rect.Right - 1,Rect.Bottom - 1);
      dgTextures.Canvas.LineTo(Rect.Left,Rect.Bottom - 1);

      dgTextures.Canvas.Pen.Color := clBtnShadow;
      dgTextures.Canvas.MoveTo(Rect.Right - 2,Rect.Top + 1);
      dgTextures.Canvas.LineTo(Rect.Right - 2,Rect.Bottom - 2);
      dgTextures.Canvas.LineTo(Rect.Left + 1,Rect.Bottom - 2);

      dgTextures.Canvas.Pen.Color := dgTextures.Color;
    End;

    For I := 0 To ((dgTextures.DefaultColWidth - 3 - 64) Div 2) - 1 Do
    Begin
      dgTextures.Canvas.MoveTo(Rect.Left + I + J + 1,Rect.Top + J + 1);
      dgTextures.Canvas.LineTo(Rect.Left + I + J + 1,Rect.Bottom - K - 1);

      dgTextures.Canvas.MoveTo(Rect.Right - I - K - 2,Rect.Top + J + 1);
      dgTextures.Canvas.LineTo(Rect.Right - I - K - 2,Rect.Bottom - K - 1);

      dgTextures.Canvas.MoveTo(Rect.Left  + J + 1,Rect.Top + I + J + 1);
      dgTextures.Canvas.LineTo(Rect.Right - K - 1,Rect.Top + I + J + 1);

      dgTextures.Canvas.MoveTo(Rect.Left  + J + 1,Rect.Bottom - I - K - 2);
      dgTextures.Canvas.LineTo(Rect.Right - K - 1,Rect.Bottom - I - K - 2);
    End; // For I
  End
  Else
  Begin
    dgTextures.Canvas.Pen.Color   := dgTextures.Color;
    dgTextures.Canvas.Brush.Color := dgTextures.Color;
    dgTextures.Canvas.Rectangle(Rect);
  End;
end;

procedure TfrmTextureChooser.FormResize(Sender: TObject);
begin
  SetColsAndRows;
  dgTextures.Invalidate;
end;

procedure TfrmTextureChooser.FormCreate(Sender: TObject);
begin
  DesiredTexture      := 0;
  BlitBuffer          := Nil;
  BlitBufSize         := 0;
  BlitBMP             := TBitmap.Create;
  BlitBMP.Width       := BlitTexSize;
  BlitBMP.Height      := BlitTexSize;
  BlitBMP.PixelFormat := pf32Bit;
end;

procedure TfrmTextureChooser.FormDestroy(Sender: TObject);
begin
  FlushBlitBuffer;
  BlitBMP.Free;
end;

Procedure TfrmTextureChooser.FlushBlitBuffer;
Begin
  If BlitBuffer <> Nil Then
  Begin
    FreeMem(BlitBuffer,BlitBufSize);
    BlitBuffer := Nil;
  End;
End; // TfrmTextureChooser.FlushBlitBuffer

Procedure TfrmTextureChooser.InitBlitBuffer;
Var
  I,J,K,L : Integer;
  BMP     : TBitmap;
  BMP1    : TBitmap;
  W       : Integer;
  R       : TRect;
  C       : TBGRA;
  P       : ^TBGRA;
  CR      : Byte;
  St      : String;
  Path    : String;

Begin
  If BlitBuffer = Nil Then
  Begin
    W := (BlitTexSize Div 4) * 4;
    If W < BlitTexSize Then Inc(W,4);
    I := W * BlitTexSize * (TextureLibrary.Count - 1) * 4;
    If I > 0 Then
    Begin
      BlitBufSize      := I;
      GetMem(BlitBuffer,BlitBufSize);
      K                := 0;
      BMP1             := TBitmap.Create;
      BMP1.Width       := BlitTexSize;
      BMP1.Height      := BlitTexSize;
      BMP1.PixelFormat := pf32Bit;

      // Load each texture and make four copies, one at each rotation (0, 90, 180, and 270).  Save
      // each texture in the blit buffer at our smaller texture size

      R.Left   := 0;
      R.Top    := 0;
      R.Right  := BlitTexSize;
      R.Bottom := BlitTexSize;

      frmStatus.Show;
      frmStatus.SetCaption('Loading textures');
      For I := 0 To TextureLibrary.Count - 2 Do
      Begin
        frmStatus.SetPosition(I / (TextureLibrary.Count - 1));

        Path := ExtractFilePath(Application.ExeName) + 'library\textures\' + TextureLibrary.Strings[I];
        St := Path + '.bmp';
        If Not FileExists(St) Then St := Path + '.jpg';
        If Not FileExists(St) Then St := Path + '.tga';
        If FileExists(St) Then
        Begin
          BMP := LoadImage(St);
          If BMP <> Nil Then
          Begin
            BMP.PixelFormat := pf32Bit;
            BMP1.Canvas.StretchDraw(R,BMP);
            BMP.Assign(BMP1);

            // Store the image//, exchanging red and blue

            For J := 0 To BMP.Height -  1 Do
            Begin
              P := BMP.ScanLine[J];
              For L := 0 To BMP.Width - 1 Do
              Begin
                C   := P^;
{
                CR  := C.R;
                C.R := C.B;
                C.B := CR;
}
                BlitBuffer^[K] := LongWord(C);
                Inc(K);
                Inc(LongWord(P),4);
              End; // For L
            End; // For J
            BMP.Free;
          End
          Else Inc(K,Sqr(BlitTexSize));
        End;
      End; // For I
      frmStatus.Hide;
      BMP1.Free;
    End;
  End;
End; // TfrmTextureChooser.InitBlitBuffer

procedure TfrmTextureChooser.dgTexturesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
Var I: Integer;
begin
  If (ARow >= 0) And (ACol >= 0) Then
  Begin
    I := ARow * dgTextures.ColCount + ACol;
    If I < TextureLibrary.Count - 1
     Then lblTextureName.Caption := TextureLibrary.Strings[I + 1]
     Else lblTextureName.Caption := '';
  End
  Else lblTextureName.Caption := '';
end;

end.
