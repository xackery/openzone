unit Options3D;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CPButton, GLVisir;

type
  TOptions3DForm = class(TForm)
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
    frame : TGLVisir;
    oldHalfBox : boolean;
    oldTicks : boolean;
    oldBackCol : TColor;
    oldTranslucent : boolean;
    oldShowDiapasones : boolean;
  public
    { Public declarations }
    procedure Call(iframe:TGLVisir);
  end;

var
  Options3DForm: TOptions3DForm;

implementation

{$R *.DFM}

procedure TOptions3DForm.Call;
begin
 frame := iFrame;
 oldHalfBox := frame.scene3d.bShowBoundPlanes;
 oldTicks := frame.scene3d.bShowTicks;
 oldTicks := frame.scene3d.bShowTicks;
 oldTranslucent := frame.scene3d.scene.bTranslucentMode;
 oldBackCol := frame.BackgroundColor;
 oldShowDiapasones := frame.scene3d.bShowDiapasones;

 HalfCheck.Checked := oldHalfBox;
 TicksCheck.Checked := oldTicks;
 BackgroundColorBtn.color := oldBackCol;
 TranslucentCheck.Checked := oldTranslucent;
 ShowDiapasonesCheck.checked := oldShowDiapasones;

 ShowModal;
 frame.scene3d.Redraw;
end;

procedure TOptions3DForm.OkBtnClick(Sender: TObject);
begin
 frame.scene3d.ShowBoundPlanes(HalfCheck.Checked);
 frame.scene3d.ShowTicks(TicksCheck.Checked);
 frame.scene3d.SetBackgroundColor(BackgroundColorBtn.color);
 frame.scene3d.Redraw;
end;

procedure TOptions3DForm.HalfCheckClick(Sender: TObject);
begin
 frame.scene3d.ShowBoundPlanes(HalfCheck.Checked);
 frame.scene3d.ShowTicks(TicksCheck.Checked);
 frame.scene3d.Redraw;
end;

procedure TOptions3DForm.TicksCheckClick(Sender: TObject);
begin
 if not HalfCheck.Checked then exit;
 frame.scene3d.ShowTicks(TicksCheck.Checked);
 frame.scene3d.Redraw;
end;

procedure TOptions3DForm.BackgroundColorBtnChangeColor(sender: TObject;
  color: TColor);
begin
if (frame <> nil) then
 begin
  frame.scene3d.SetBackgroundColor(color);
  frame.scene3d.Redraw;
 end;
end;

procedure TOptions3DForm.CancelBtnClick(Sender: TObject);
begin
 frame.scene3d.ShowBoundPlanes(oldHalfBox);
 frame.scene3d.ShowTicks(oldTicks);
 frame.scene3d.SetBackgroundColor(oldBackCol);
 frame.scene3d.Redraw;
end;

procedure TOptions3DForm.TranslucentCheckClick(Sender: TObject);
begin
 frame.scene3d.Scene.bTranslucentMode := TranslucentCheck.Checked;
 frame.scene3d.Redraw;
end;

procedure TOptions3DForm.ShowDiapasonesCheckClick(Sender: TObject);
begin
 frame.scene3d.bShowDiapasones := ShowDiapasonesCheck.Checked;
 frame.scene3d.Redraw;
end;

end.
