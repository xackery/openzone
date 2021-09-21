unit frmMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PDJRotoLabel, ExtCtrls, ZPropLst, ClearTypeText, StdCtrls,
  IAeverButton, Buttons, Grids;

type
  PBGRA = ^TBGRA;
  TBGRA = Packed Record
    B,G,R,A: Byte;
  End;
  TfrmMain = class(TForm)
    PDJRotoLabel1: TPDJRotoLabel;
    Image1: TImage;
    ZPropList1: TZPropList;
    ClearTypeText1: TClearTypeText;
    ClearTypeLabel1: TClearTypeLabel;
    Label1: TLabel;
    btnToggle: TButton;
    ClearTypeCheckBox1: TClearTypeCheckBox;
    CheckBox1: TCheckBox;
    ClearTypeRadioButton1: TClearTypeRadioButton;
    IAeverButton1: TIAeverButton;
    BitBtn1: TBitBtn;
    IAEverButton2: TIAEverButton;
    ClearTypeGroupBox1: TClearTypeGroupBox;
    ClearTypeListBox1: TClearTypeListBox;
    ListBox1: TListBox;
    ClearTypeStringGrid1: TClearTypeStringGrid;
    Edit1: TEdit;
    ClearTypeEdit1: TClearTypeEdit;
    Edit2: TEdit;
    procedure btnToggleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

Procedure ClearizeScanLine(Src,Dst: PBGRA; Width: Integer);
Var
  I,I3,C    : Integer;
  Src0,Dst0 : PBGRA;
  
Begin
  I    := 0;
  I3   := 0;
  Src0 := Src;
  Dst0 := Dst;
  While I < Width Do
  Begin
    // do red

    If I = 0
     Then C := 4 * Src0.R
     Else c := PBGRA(LongWord(Src) - 2).R + 3 * PBGRA(LongWord(Src) - 1).R;
    Inc(C,4 * Src.R + 3 * PBGRA(LongWord(Src) + 1).R + PBGRA(LongWord(Src) + 2).R);
    Dst.R := C;

    // do green

    If I = 0
     Then C := Src0.G
     Else C := PBGRA(LongWord(Src) - 1).G;
    Inc(C,3 * Src.G + 4 * PBGRA(LongWord(Src) + 1).G + 3 * PBGRA(LongWord(Src) + 2).G);
    If I = Width - 1
     Then Inc(C,PBGRA(LongWord(Src) + 2).G)
     Else Inc(C,PBGRA(LongWord(Src) + 3).G);
    Dst.G := C;

    // do blue

    C := Src.B + 3 * PBGRA(LongWord(Src) + 1).B + 4 * PBGRA(LongWord(Src) + 2).B;
    If I = Width - 1
     Then Inc(C,4 * PBGRA(LongWord(Src) + 2).B)
     Else Inc(C,3 * PBGRA(LongWord(Src) + 3).B + PBGRA(LongWord(Src) + 4).B);
    Dst.B := C;

    Inc(I);
    Inc(I3,3);
  End; // While
End;

procedure TfrmMain.btnToggleClick(Sender: TObject);
begin
  ClearTypeText1.Enabled := Not ClearTypeText1.Enabled;
end;

procedure TfrmMain.FormShow(Sender: TObject);
Var I,J: Integer;
begin
  For I := 0 To 3 Do
   For J := 0 To 3 Do ClearTypeStringGrid1.Cells[I,J] := Format('%d, %d',[I,J]);
end;

end.
