unit frmConvertBMPUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ClearTypeText, OZDMUnit,
  IAeverButton;

type
  TfrmConvertBMP = class(TForm)
    Label1: TClearTypeLabel;
    edtInput: TClearTypeEdit;
    btnBrowseInput: TIAEverButton;
    Label2: TClearTypeLabel;
    edtOutput: TClearTypeEdit;
    btnBrowseOutput: TIAEverButton;
    cbOverwrite: TClearTypeCheckBox;
    btnOk: TIAEverButton;
    btnCancel: TIAEverButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    Panel1: TPanel;
    Label3: TClearTypeLabel;
    procedure btnBrowseInputClick(Sender: TObject);
    procedure btnBrowseOutputClick(Sender: TObject);
    procedure cbOverwriteClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure ConvertBMP;
  end;

var
  frmConvertBMP: TfrmConvertBMP;

implementation

{$R *.dfm}

Uses frmMainUnit;

Procedure TfrmConvertBMP.ConvertBMP;
Var
  InFileName  : String;
  OutFileName : String;
  BMP         : TBitmap;
  X,Y         : Integer;
  P,P1,P2     : PColor;
  Q,Q1,Q2     : PColor;
  R,R1,R2     : PColor;
  C           : TColor;
  Colors      : Array[1..4] Of TColor;
  Count       : Array[1..4] Of Integer;
  MaskColor   : TColor;
  I,J         : Integer;
  Mask        : LongWord;
  Col         : LongWord;

Begin
  InFileName := Trim(edtInput.Text);
  If cbOverwrite.Checked
   Then OutFileName := InFileName
   Else OutFileName := Trim(edtOutput.Text);

  If InFileName <> '' Then
  Begin
    If FileExists(InFileName) Then
    Begin
      If OutFileName <> '' Then
      Begin
        // Load the image and convert it to 32 bits per pixel

        BMP := TBitmap.Create;
        BMP.LoadFromFile(InFileName);
        BMP.PixelFormat := pf32Bit;

        // Determine the transparent color by looking at the color at each corner and choosing the one
        // that is most prevalent.  If they're all different, then the one at the upper-left is chosen.

        Colors[1] := BMP.Canvas.Pixels[0,0];
        Colors[2] := BMP.Canvas.Pixels[BMP.Width - 1,0];
        Colors[3] := BMP.Canvas.Pixels[0,BMP.Height - 1];
        Colors[4] := BMP.Canvas.Pixels[BMP.Width - 1,BMP.Height - 1];

        For I := 1 To 3 Do
        Begin
          Count[I] := 1;
          For J := I + 1 To 4 Do
          Begin
            If Colors[I] = Colors[J] Then Inc(Count[I]);
          End; // For J
        End; // For I

        MaskColor := Colors[1];
        J         := 1;
        For I := 2 To 4 Do
        Begin
          If Count[I] > Count[J] Then
          Begin
            J         := I;
            MaskColor := Colors[I];
          End;
        End;
        BMP.TransparentColor := MaskColor;
        Mask                 := LongWord(MaskColor);
        Mask                 := Mask And $00FFFFFF;

        // Must exchange red and blue of the mask because the format of 32-bit BMP is always BGRA

        Mask := ((Mask And $FF) Shl 16) + (Mask And $FF00) + ((Mask And $FF0000) Shr 16);

        // Change the alpha value of all pixels to $FF

        For Y := 0 To BMP.Height - 1 Do
        Begin
          P := BMP.ScanLine[Y];
          For X := 0 To BMP.Width - 1 Do
          Begin
            C   := P^;
            TBGRA(C).A := $FF;
            P^ := C;
            Inc(P);
          End; // For X
        End; // For Y

        // Scan the image one pixel at a time, changing the alpha value of transparent pixels
        // to $00.  In addition, do the same to all pixels that border them, since bi/trilinear
        // filtering blends adjacent color values.  Using ScanLine[] is a lot faster than using
        // Canvas.Pixels[], but it depends on the image being uncompressed 32-bit.

        For Y := 0 To BMP.Height - 1 Do
        Begin
          P  := BMP.ScanLine[Y];
          P1 := P;
          P2 := P;
          Inc(P2);
          If Y > 0 Then
          Begin
            Q  := BMP.ScanLine[Y - 1];
            Q1 := Q;
            Q2 := Q;
            Inc(Q2);
          End
          Else
          Begin
            Q  := P;
            Q1 := P1;
            Q2 := P2;
          End;
          If Y < BMP.Height - 1 Then
          Begin
            R  := BMP.ScanLine[Y + 1];
            R1 := R;
            R2 := R;
            Inc(R2);
          End
          Else
          Begin
            R  := P;
            R1 := P1;
            R2 := P2;
          End;
          For X := 0 To BMP.Width - 1 Do
          Begin
            C   := P^;
            Col := LongWord(C);
            If (Col And $00FFFFFF) = Mask Then
            Begin
              TBGRA(C).A := $00;
              P^         := C;
              If Y > 0 Then
              Begin
                C          := Q^;
                TBGRA(C).A := $00;
                Q^         := C;
              End;
              If Y < BMP.Height - 1 Then
              Begin
                C          := R^;
                TBGRA(C).A := $00;
                R^         := C;
              End;
              If X > 0 Then
              Begin
                C          := P1^;
                TBGRA(C).A := $00;
                P1^        := C;
                If Y > 0 Then
                Begin
                  C          := Q1^;
                  TBGRA(C).A := $00;
                  Q1^        := C;
                End;
                If Y < BMP.Height - 1 Then
                Begin
                  C          := R1^;
                  TBGRA(C).A := $00;
                  R1^        := C;
                End;
              End;
              If X < BMP.Width - 1 Then
              Begin
                C          := P2^;
                TBGRA(C).A := $00;
                P2^        := C;
                If Y > 0 Then
                Begin
                  C          := Q2^;
                  TBGRA(C).A := $00;
                  Q2^        := C;
                End;
                If Y < BMP.Height - 1 Then
                Begin
                  C          := R2^;
                  TBGRA(C).A := $00;
                  R2^        := C;
                End;
              End;
            End;
            P1 := P;
            Inc(P);
            Inc(P2);
            If Y > 0 Then
            Begin
              Q1 := Q;
              Inc(Q);
              Inc(Q2);
            End;
            If Y < BMP.Height - 1 Then
            Begin
              R1 := R;
              Inc(R);
              Inc(R2);
            End;
          End; // For X
        End; // For Y

        // Save the image

        Try
          BMP.SaveToFile(OutFileName);
        Except
          ShowMessage('There was an error writing to file "' + OutFileName + '".');
          ModalResult := mrNone;
        End;
        BMP.Free;
        ShowMessage('Bitmap has been converted.');
      End
      Else
      Begin
        ShowMessage('You must specify an output file.');
        ModalResult := mrNone;
      End;
    End
    Else
    Begin
      ShowMessage('Could not find file "' + InFileName + '".');
      ModalResult := mrNone;
    End;
  End
  Else
  Begin
    ShowMessage('You must specify an input file.');
    ModalResult := mrNone;
  End;
End; // ConvertBMP

procedure TfrmConvertBMP.btnBrowseInputClick(Sender: TObject);
begin
  dlgOpen.InitialDir := ExtractFilePath(Application.ExeName) + 'LIBRARY\TEXTURES';
  If dlgOpen.Execute Then edtInput.Text := dlgOpen.FileName;
end;

procedure TfrmConvertBMP.btnBrowseOutputClick(Sender: TObject);
begin
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName) + 'LIBRARY\TEXTURES';
  If dlgSave.Execute Then edtOutput.Text := dlgSave.FileName;
end;

procedure TfrmConvertBMP.cbOverwriteClick(Sender: TObject);
begin
  edtInput.Enabled := Not cbOverwrite.Checked;
  btnBrowseOutput.Enabled := Not cbOverwrite.Checked;
end;

procedure TfrmConvertBMP.btnOkClick(Sender: TObject);
begin
  ConvertBMP;
end;

end.
