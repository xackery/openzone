Unit Options3D;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CPButton;

Type
  TOptions3DForm = Class(TForm)
    GroupBox1: TGroupBox;
    HalfCheck: TCheckBox;
    TicksCheck: TCheckBox;
    Label4: TLabel;
    BackgroundColorBtn: TColorPickButton;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    TranslucentCheck: TCheckBox;
    ShowDiapasonesCheck: TCheckBox;
    procedure OkBtnClick(Sender: TObject);
    procedure HalfCheckClick(Sender: TObject);
    procedure TicksCheckClick(Sender: TObject);
    procedure BackgroundColorBtnChangeColor(sender: TObject;
      color: TColor);
    procedure CancelBtnClick(Sender: TObject);
    procedure TranslucentCheckClick(Sender: TObject);
    procedure ShowDiapasonesCheckClick(Sender: TObject);
  private
    { Private declarations }
    frame : TFrame;
    oldHalfBox : boolean;
    oldTicks : boolean;
    oldBackCol : TColor;
    oldTranslucent : boolean;
    oldShowDiapasones : boolean;
  public
    { Public declarations }
    procedure Call(iframe: TFrame);
  end;

var
  Options3DForm: TOptions3DForm;

implementation

Uses U3DPolys,GLVisir;

{$R *.DFM}

procedure TOptions3DForm.Call;
begin
 frame := iFrame;
 oldHalfBox := TGLVisir(frame).scene3d.bShowBoundPlanes;
 oldTicks := TGLVisir(frame).scene3d.bShowTicks;
 oldTicks := TGLVisir(frame).scene3d.bShowTicks;
 oldTranslucent := TGLVisir(frame).scene3d.scene.bTranslucentMode;
 oldBackCol := TGLVisir(frame).BackgroundColor;
 oldShowDiapasones := TGLVisir(frame).ShowDiapasones;

 HalfCheck.Checked := oldHalfBox;
 TicksCheck.Checked := oldTicks;
 BackgroundColorBtn.color := oldBackCol;
 TranslucentCheck.Checked := oldTranslucent;
 ShowDiapasonesCheck.checked := oldShowDiapasones;

 ShowModal;
 TGLVisir(frame).scene3d.Redraw;
end;

procedure TOptions3DForm.OkBtnClick(Sender: TObject);
begin
 TGLVisir(frame).scene3d.ShowBoundPlanes(HalfCheck.Checked);
 TGLVisir(frame).scene3d.ShowTicks(TicksCheck.Checked);
 TGLVisir(frame).scene3d.SetBackgroundColor(BackgroundColorBtn.color);
 TGLVisir(frame).scene3d.Redraw;
end;

procedure TOptions3DForm.HalfCheckClick(Sender: TObject);
begin
 TGLVisir(frame).scene3d.ShowBoundPlanes(HalfCheck.Checked);
 TGLVisir(frame).scene3d.ShowTicks(TicksCheck.Checked);
 TGLVisir(frame).scene3d.Redraw;
end;

procedure TOptions3DForm.TicksCheckClick(Sender: TObject);
begin
 if not HalfCheck.Checked then exit;
 TGLVisir(frame).scene3d.ShowTicks(TicksCheck.Checked);
 TGLVisir(frame).scene3d.Redraw;
end;

procedure TOptions3DForm.BackgroundColorBtnChangeColor(sender: TObject;
  color: TColor);
begin
if (frame <> nil) then
 begin
  TGLVisir(frame).scene3d.SetBackgroundColor(color);
  TGLVisir(frame).scene3d.Redraw;
 end;
end;

procedure TOptions3DForm.CancelBtnClick(Sender: TObject);
begin
 TGLVisir(frame).scene3d.ShowBoundPlanes(oldHalfBox);
 TGLVisir(frame).scene3d.ShowTicks(oldTicks);
 TGLVisir(frame).scene3d.SetBackgroundColor(oldBackCol);
 TGLVisir(frame).scene3d.Redraw;
end;

procedure TOptions3DForm.TranslucentCheckClick(Sender: TObject);
Var
  I,J : Integer;
  E : TEntity;

begin
 For I := 0 To TGLVisir(Frame).Scene3D.Scene.Entities.Count - 1 Do
 Begin
   E := TEntity(TGLVisir(Frame).Scene3D.Scene.Entities[I]);
//   E.Trans := TranslucentCheck.Checked;
(*
//   For J := 0 To E.Faces.Count - 1 Do
   For J := 0 To High(E.Faces) Do
   Begin
     If TranslucentCheck.Checked
      Then E.Faces[J].A := 128
      Else E.Faces[J].A := 0;
//      Then TFace(E.Faces.Items[J]).a := 128
//      Else TFace(E.Faces.Items[J]).a := 0;
   End; // For J
*)   
 End; // For I
 TGLVisir(frame).scene3d.Scene.bTranslucentMode := TranslucentCheck.Checked;
 TGLVisir(frame).scene3d.Redraw;
end;

procedure TOptions3DForm.ShowDiapasonesCheckClick(Sender: TObject);
begin
 TGLVisir(frame).ShowDiapasones := ShowDiapasonesCheck.Checked;
 TGLVisir(frame).scene3d.Redraw;
end;

end.
