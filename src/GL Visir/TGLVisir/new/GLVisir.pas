//==============================================================================
// Product name: GLVisir
// Copyright 2000-2001 AidAim Software.
// Description:
//  GLVisir is a component for on-screen displaying animated or static
//  3D scenes using hardware or software OpenGL for rendering.
//  It is especially designed for rendering 3D surfaces and 3D sets.
// Version: 2.5
// Date: 12/23/2000
//==============================================================================
unit GLVisir;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GL, GLU, UrickGL, U3Dpolys, ExtCtrls, StdCtrls, Buttons, ToolWin,
  ComCtrls, ImgList, Math, About3D;

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
type
 // Vector type
 VectorType = Record
           x,y,z: double;
 end;

 // Polygon type
 PolygonType = Record
             n: array [0..3] of integer; // vertexes no
             numVertexes : integer; // number of vertexes
				    //2 - line, 3 - triangle, 4 - box
             col : TColor; // color of polygon
 end;

 // 3D model record definition
 TModelRec = Record
    v : array of VectorType;  // array of vertexes
    p : array of PolygonType; // array of polygons
    nv: integer;	      // number of vertexes
    np: integer;              // number of polygons
 end;

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
 ObjectDrawStyleType = (Wired, Filled, Combined);

 TModifyAction =
  (maNone, maRotateRight, maRotateLeft, maRotateUp, maRotateDown,
         maMoveRight, maMoveLeft, maMoveUp, maMoveDown, maZoomIn, maZoomOut,
         maRotateLeft2, maRotateRight2, maMoveForward, maMoveBackward);
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
   TScene3D = class;

   TGLVisir = class(TFrame)
    Panel1: TPanel;
    TopPanel: TPanel;
    Panel2: TPanel;
    BottomPanel: TPanel;
    Panel3: TPanel;
    ImagePanelContainer: TPanel;
    TitlePanel: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    StyleCombo: TComboBox;
    btnRotateLeft: TSpeedButton;
    btnRotateUp: TSpeedButton;
    btnRotateRight: TSpeedButton;
    btnRotateDown: TSpeedButton;
    ZoomInBtn: TSpeedButton;
    ZoomOutBtn: TSpeedButton;
    OptionsBtn: TSpeedButton;
    Panel4: TPanel;
    AnimationPanel: TPanel;
    StopBtn: TSpeedButton;
    PlayBtn: TSpeedButton;
    RewindBtn: TSpeedButton;
    ForwardBtn: TSpeedButton;
    ShowAllBtn: TSpeedButton;
    Label1: TLabel;
    AnimTrackBar: TTrackBar;
    SpeedTrackBar: TTrackBar;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    RepeatBtn: TSpeedButton;
    StraightBtn: TSpeedButton;
    ActionTimer: TTimer;
    DefaultBtn: TSpeedButton;
    OpenBtn: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    AnimationTimer: TTimer;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    DiapasonesPanel: TPanel;
    ImagePanel: TPanel;
    SaveBtn: TSpeedButton;
    AboutBtn: TSpeedButton;
    btnRotateLeft2: TSpeedButton;
    btnRotateRight2: TSpeedButton;
    Bevel4: TBevel;
    btnMoveForward: TSpeedButton;
    btnMoveLeft: TSpeedButton;
    btnMoveRight: TSpeedButton;
    btnMoveBackward: TSpeedButton;
    btnMoveUp: TSpeedButton;
    btnMoveDown: TSpeedButton;
    procedure btnRotateRightMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnRotateRightMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnRotateLeftMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnRotateUpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnRotateDownMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ZoomInBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ZoomOutBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DefaultBtnClick(Sender: TObject);
    procedure ActionTimerTimer(Sender: TObject);
    procedure StyleComboChange(Sender: TObject);
    procedure OptionsBtnClick(Sender: TObject);
    procedure ShowAllBtnClick(Sender: TObject);
    procedure AnimationTimerTimer(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure PlayBtnClick(Sender: TObject);
    procedure RewindBtnClick(Sender: TObject);
    procedure ForwardBtnClick(Sender: TObject);
    procedure RepeatBtnClick(Sender: TObject);
    procedure StraightBtnClick(Sender: TObject);
    procedure AnimTrackBarChange(Sender: TObject);
    procedure SpeedTrackBarChange(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure ImagePanelContainerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImagePanelContainerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImagePanelContainerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure btnRotateLeft2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnRotateRight2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnMoveForwardMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnMoveBackwardMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnMoveLeftMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnMoveRightMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnMoveUpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnMoveDownMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    bInitialized : boolean;
    mouseBtn1, mouseBtn2 : boolean; // is mouse buttons pressed?
    mouseStartX, mouseStartY : integer; // coords of last click

    function GetBackCol : TColor; procedure SetBackCol(value:TColor);
    function GetaxisesNamesCol : TColor; procedure SetaxisesNamesCol(value:TColor);
    function GetDrawStyle : ObjectDrawStyleType; procedure SetDrawStyle(value:ObjectDrawStyleType);
    function GetCaption : string; procedure SetCaption(value:string);
    function GetaxisXName : string; procedure SetaxisXName(value:string);
    function GetaxisYName : string; procedure SetaxisYName(value:string);
    function GetaxisZName : string; procedure SetaxisZName(value:string);

    procedure AddSphereToNewFrame;

    procedure Redraw;

    procedure SetAnimationPanel;
    procedure SetControlPanel;

  protected
    procedure WndProc(var Message: TMessage); override;

  public
    //--- main objects ---
    scene3D : TScene3D; // scene

    mouseKMove, mouseKRotate, mouseKZoom : double; // multipliers of rotation/... from mouse

    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy; override;
    procedure Init;
    procedure Fit(bCoordsSymmetry,bTrueAspectRatio:boolean);
    procedure Update; override;
    procedure SaveToFile(fileName : string);
    procedure LoadFromFile(fileName : string);

    // Remove all frames
    procedure Clear;
    // Add new frame with specified model
    procedure AddModelToNewFrame(mdl : TModelRec);
    // Add model to specified frame
    procedure AddModelToFrame(mdl : TModelRec;nFrame : integer);

  published
    property BackgroundColor : TColor read GetBackCol write SetBackCol;
    property AxisNamesColor : TColor read GetAxisesNamesCol write SetAxisesNamesCol;
    property Style : ObjectDrawStyleType read GetDrawStyle
                                         write SetDrawStyle
                                         default Filled;
    property Caption : String read GetCaption write SetCaption;
    property AxisXName : String read GetaxisXName write SetaxisXName;
    property AxisYName : String read GetaxisYName write SetaxisYName;
    property AxisZName : String read GetaxisZName write SetaxisZName;
  end;

  TScene3D = class
  private
    imagePanel : TPanel; // panel to display scene on it
    diapasonesPanel : TPanel; // panel to display coordinate ranges
    xmin, xmax, ymin, ymax, zmin, zmax : double; // coordinate ranges of objects in scene

    // Add new tick to axis
    procedure AddTick(var entity:Tentity; x,y,z:double;dx,dy,dz:double;
                      a1,a2,a3:double;
                      labelDx:double;s:String);
    // add new frame with boundary planes
    procedure AddBoundPlanesToNewFrame(bCoordsCenterSymmetry:boolean);

    // display diapasone values
    procedure DrawDiapasones;

  public
    //--- main objects ---
    scene : TSceneGL; // scene
    Mouse : T3DMouse; // change object by mouse
    modifyAction : TModifyAction; // current action (rotate right,left,...)
    SampleText:T3dText; // font sample for ticks on axis

    bInitialized : boolean; // was graphic initialized?
    curFrame : integer; // current frame (in Scene list)
    lastFrame : integer; // No of last frame (in Scene list)
    firstPlane : integer; // No of 1st element of boundary planes (in Scene list)
    lastPlane : integer; //  No of last element of boundary planes (in Scene list)
    firstTick : integer; //  No of 1st tick element of boundary planes (in Scene list)
    lastTick : integer; //  No of last tick element of boundary planes (in Scene list)

    // user defined parameters
    bCoordsCenterSymmetry,bTrueAspectRatio:boolean; // center and zoom all frames?
    bCycledAnimation : boolean; // loop animation?
    bShowAllFrames : boolean; // show all animation frames simultaneously?
    bShowBoundPlanes : boolean; // show boundary planes?
    bShowTicks : boolean; // show ticks on boundary planes?
    bShowDiapasones : boolean;// show value ranges
    backgroundColor : TColor; // background color
    modelColor : TColor; // color of 3D model (if only 1 color used)
    modelCombinedColor : TColor; // line color in Combined Style
    planeColor : TColor; // color of boundary planes
    FontColor : TColor; // color of ticks on axis
    fontScale : double; // font scale of ticks values on axis
    axisXTickCount,axisYTickCount,axisZTickCount : integer; // number of ticks on axis
    axisXName,axisYName,axisZName : String; // axis names
    axisNameColor : TColor; // axis names color
    modelStyle : integer; // draw style (wired/filled/combined)
    deltaRotate, deltaZoom, deltaMove : double;

    { Public declarations }
    constructor Create(AOwner:TComponent; iImagePanel, iDiapasonesPanel:TPanel);
    procedure Init;
    procedure Update;
    procedure Redraw;

    procedure ShowBoundPlanes(flag : boolean); // show boundary planes?
    procedure ShowTicks(flag : boolean); // show ticks on boundary planes?

    procedure ShowAllFrames(flag : boolean); // show all animation frames simultaneously?

    procedure SetStyle(style: integer); // set style of drawing (wired/filled/combined)
    procedure SetModelColor(col: TColor); // set color of models in frames
    procedure SetBackgroundColor(col: TColor); // set background color

    procedure SetFrameByNum(iFrame:integer); // set i-th frame as active
    procedure SetPrevFrame; // make previous frame active
    procedure SetNextFrame; // make next frame active

    procedure ProcessAction(action:TModifyAction; delta:double); // change scene - rotate, ...
    procedure ProcessModifyAction; // process actions
    procedure DefaultPosition; // restore default position

    procedure Fit(bCoordsCenterSymmetry1,bTrueAspectRatio1:boolean); // center and zoom frames

    procedure ClearScene; // remove all frames from scene
    procedure SaveScene(fileName:String); // save all frames to file
    procedure LoadScene(fileName:String); // load frames from file
    procedure SaveFrameToBMP(fileName:String); // save current frame to bmp file

    procedure AddModelToEntity(mdl : TModelRec; entity:TEntity); // add model to frame

   //--- methods for user model definition ---
    // add new frame with specified model
    procedure AddModelToNewFrame(mdl : TModelRec);
    // add model to specified frame
    procedure AddModelToFrame(mdl : TModelRec;nFrame : integer);
  end;

procedure Register;

implementation

uses Options3D;

{$R *.DFM}

//------------------------------------------------------------------------------
//         Create and set default parameters values
//------------------------------------------------------------------------------
constructor TGLVisir.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  bInitialized := false;
  scene3D := TScene3D.Create(Aowner, ImagePanel, DiapasonesPanel);
  styleCombo.ItemIndex := 1;
  mouseBtn1 := false; // mouse buttons not pressed
  mouseBtn2 := false;
  mouseKMove := 0.02;
  mouseKRotate := 1;
  mouseKZoom := 0.02;
end;

destructor TGLVisir.Destroy;
begin
 scene3D.ClearScene;
 scene3D.scene.destroy;
 inherited Destroy;
end;

//------------------------------------------------------------------------------
//                   Graph init
//------------------------------------------------------------------------------
procedure TGLVisir.Init;
begin
  bInitialized := true;
  Scene3D.Init();
  SetAnimationPanel;
  SetControlPanel;
  if (csDesigning in ComponentState) then
   begin
    AnimationTimer.Enabled := false;
    ActionTimer.Enabled := false;
    AddSphereToNewFrame;
    Fit(true,true);
   end;
  if (not (csDesigning in ComponentState)) then
   begin
    Application.CreateForm(TOptions3DForm, Options3DForm);
    Application.CreateForm(TAboutBox, AboutBox);
   end;
end;

//------------------------------------------------------------------------------
//                Center, zoom, ...
//------------------------------------------------------------------------------
procedure TGLVisir.Fit;
begin
  Scene3D.Fit(bCoordsSymmetry,bTrueAspectRatio);
  SetAnimationPanel;
  SetControlPanel;
  Update;
  Scene3D.DefaultPosition;
end;


//------------------------------------------------------------------------------
//                        Redraw scene
//------------------------------------------------------------------------------
procedure TGLVisir.Redraw;
begin
 Scene3D.Redraw;
end;

procedure TGLVisir.WndProc(var Message: TMessage);
begin
 if ((message.Msg = WM_ACTIVATE) or
     (message.Msg = WM_PAINT) or
     (message.Msg = WM_NCPAINT) or
     (message.Msg = WM_SHOWWINDOW)) then Redraw;
 inherited WndProc(Message);
end;


//------------------------------------------------------------------------------
//             Enable/Disable buttons
//------------------------------------------------------------------------------
procedure TGLVisir.SetAnimationPanel;
begin

 if (scene3D.lastFrame>0) then
  AnimTrackBar.Max := scene3D.lastFrame;

 // if all frames shown
 if (ShowAllBtn.Down or (scene3D.lastFrame <= 0)) then
  begin
   //--- disable ---
   playBtn.Enabled := false;
   stopBtn.Enabled := false;
   rewindBtn.Enabled := false;
   forwardBtn.Enabled := false;
   AnimTrackBar.Enabled := false;
   RepeatBtn.Enabled := false;
   StraightBtn.Enabled := false;
   SpeedTrackBar.Enabled := false;

   if (scene3D.lastFrame <= 0) then
    ShowAllBtn.Enabled := false;
  end
 else
  begin
   //--- enable ---
   playBtn.Enabled := true;
   stopBtn.Enabled := true;
   rewindBtn.Enabled := true;
   forwardBtn.Enabled := true;
   AnimTrackBar.Enabled := true;
   RepeatBtn.Enabled := true;
   StraightBtn.Enabled := true;
   SpeedTrackBar.Enabled := true;
   ShowAllBtn.Enabled := true;

   if (not playBtn.Down) then
     stopBtn.Enabled:=false;

   if (scene3D.lastFrame = scene3D.curFrame) then
    if (not scene3D.bCycledAnimation) then
     begin
      forwardBtn.Enabled := false;
      PlayBtn.Enabled := true;
      StopBtn.Enabled := false;
      PlayBtn.Down := false;
     end;

   if (scene3D.curFrame=0) then
    if (not scene3D.bCycledAnimation) then
     begin
      rewindBtn.Enabled := false;
     end;

   AnimTrackBar.Position := scene3D.curFrame;
  end;

end;

procedure TGLVisir.SetControlPanel;
begin
  if (scene3D.lastFrame < 0) then
  begin
    btnRotateUp.Enabled     := False;
    btnRotateDown.Enabled   := False;
    btnRotateLeft.Enabled   := False;
    btnRotateRight.Enabled  := False;
    btnRotateLeft2.Enabled  := False;
    btnRotateRight2.Enabled := False;
    btnMoveForward.Enabled  := False;
    btnMoveBackward.Enabled := False;
    btnMoveLeft.Enabled     := False;
    btnMoveRight.Enabled    := False;
    btnMoveUp.Enabled       := False;
    btnMoveDown.Enabled     := False;
    DefaultBtn.Enabled      := False;
    ZoomInBtn.Enabled       := False;
    ZoomOutBtn.Enabled      := False;
    StyleCombo.Enabled      := False;
    OptionsBtn.Enabled      := False;
    SaveBtn.Enabled         := False;
  end
  else
  begin
    btnRotateUp.Enabled     := True;
    btnRotateDown.Enabled   := True;
    btnRotateLeft.Enabled   := True;
    btnRotateRight.Enabled  := True;
    btnRotateLeft2.Enabled  := True;
    btnRotateRight2.Enabled := True;
    btnMoveForward.Enabled  := True;
    btnMoveBackward.Enabled := True;
    btnMoveLeft.Enabled     := True;
    btnMoveRight.Enabled    := True;
    btnMoveUp.Enabled       := True;
    btnMoveDown.Enabled     := True;
    DefaultBtn.Enabled      := True;
    ZoomInBtn.Enabled       := True;
    ZoomOutBtn.Enabled      := True;
    StyleCombo.Enabled      := True;
    OptionsBtn.Enabled      := True;
    SaveBtn.Enabled         := True;
  end;
end;


//------------------------------------------------------------------------------
//              Overriding of update
//------------------------------------------------------------------------------
procedure TGLVisir.Update;
begin
 if (not bInitialized) then Init;
 Scene3D.Update;
end;



//------------------------------------------------------------------------------
//        Remove all frames
//------------------------------------------------------------------------------
procedure TGLVisir.Clear;
begin
  scene3D.ClearScene;
end;


//------------------------------------------------------------------------------
//        Add specified model to new frame
//------------------------------------------------------------------------------
procedure TGLVisir.AddModelToNewFrame(mdl : TModelRec);
begin
  scene3D.AddModelToNewFrame(mdl);
end;


//------------------------------------------------------------------------------
//        Add model to specified frame
//------------------------------------------------------------------------------
procedure TGLVisir.AddModelToFrame(mdl : TModelRec;nFrame : integer);
begin
 scene3D.AddModelToFrame(mdl,nFrame);
end;


//------------------------------------------------------------------------------
//         pressing buttons, changing scene view
//------------------------------------------------------------------------------
procedure TGLVisir.btnRotateRightMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maRotateRight2;
end;

procedure TGLVisir.btnRotateRightMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maNone;
end;

procedure TGLVisir.btnRotateLeftMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maRotateLeft2;
end;

procedure TGLVisir.btnRotateUpMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maRotateUp;
end;

procedure TGLVisir.btnRotateDownMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maRotateDown;
end;

procedure TGLVisir.ZoomInBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 scene3D.modifyAction := maZoomIn;
end;

procedure TGLVisir.ZoomOutBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 scene3D.modifyAction := maZoomOut;
end;

// Restore default parameters
procedure TGLVisir.DefaultBtnClick(Sender: TObject);
begin
 scene3D.DefaultPosition;
end;


//------------------------------------------------------------------------------
//     Call rotate, zoom when according buttons pressed
//------------------------------------------------------------------------------

procedure TGLVisir.ActionTimerTimer(Sender: TObject);
begin
 if (not bInitialized) then Init;
 scene3D.ProcessModifyAction;
end;

//------------------------------------------------------------------------------
//                Set draw style
//------------------------------------------------------------------------------
procedure TGLVisir.StyleComboChange(Sender: TObject);
begin
 if (StyleCombo.ItemIndex = 0) then
  scene3D.SetStyle(1) // wired
 else
 if (StyleCombo.ItemIndex = 1) then
  scene3D.SetStyle(2) // filled
 else
 if (StyleCombo.ItemIndex = 2) then
  scene3D.SetStyle(3); // combined
 scene3D.Redraw;
end;


//------------------------------------------------------------------------------
//         Show Options window
//------------------------------------------------------------------------------
procedure TGLVisir.OptionsBtnClick(Sender: TObject);
begin
 Options3DForm.Call(self);
end;


//------------------------------------------------------------------------------
//        Show all frames simultaneously?
//------------------------------------------------------------------------------
procedure TGLVisir.ShowAllBtnClick(Sender: TObject);
begin
 scene3D.ShowAllFrames(ShowAllBtn.Down);
 SetAnimationPanel;
 Redraw;
end;


//------------------------------------------------------------------------------
//                  Animation: by timer - next frame
//------------------------------------------------------------------------------
procedure TGLVisir.AnimationTimerTimer(Sender: TObject);
begin
try
 if (playBtn.Down and playBtn.Enabled) then
 begin
  scene3D.SetNextFrame;
  SetAnimationPanel;
  scene3D.Redraw;
 end;
except
end;
end;


//------------------------------------------------------------------------------
//           Process animation controls
//------------------------------------------------------------------------------
procedure TGLVisir.StopBtnClick(Sender: TObject);
begin
 playBtn.Down := false;
 stopBtn.Enabled := false;
 SetAnimationPanel;
end;

procedure TGLVisir.PlayBtnClick(Sender: TObject);
begin
 scene3D.SetFrameByNum(0);
 stopBtn.Enabled := true;
 SetAnimationPanel;
end;

procedure TGLVisir.RewindBtnClick(Sender: TObject);
begin
  scene3D.SetPrevFrame;
  SetAnimationPanel;
  scene3D.Redraw;
end;

procedure TGLVisir.ForwardBtnClick(Sender: TObject);
begin
  scene3D.SetNextFrame;
  SetAnimationPanel;
  scene3D.Redraw;
end;

procedure TGLVisir.RepeatBtnClick(Sender: TObject);
begin
 scene3D.bCycledAnimation := true;
 SetAnimationPanel;
end;

procedure TGLVisir.StraightBtnClick(Sender: TObject);
begin
 scene3D.bCycledAnimation := false;
 SetAnimationPanel;
end;

procedure TGLVisir.AnimTrackBarChange(Sender: TObject);
begin
  scene3D.SetFrameByNum(AnimTrackBar.Position);
  SetAnimationPanel;
  scene3D.Redraw;
end;

procedure TGLVisir.SpeedTrackBarChange(Sender: TObject);
begin
 case SpeedTrackBar.Position of
  0 : AnimationTimer.Interval := 200;
  1 : AnimationTimer.Interval := 500;
  2 : AnimationTimer.Interval := 1000;
 end;
end;


//------------------------------------------------------------------------------
//                       Read file
//------------------------------------------------------------------------------
procedure TGLVisir.OpenBtnClick(Sender: TObject);
begin
 if (OpenDialog1.Execute) then
  begin
     scene3D.LoadScene(OpenDialog1.FileName);
     Style := GetDrawStyle;
     RepeatBtn.Down := scene3D.bCycledAnimation;
     ShowAllBtn.Down := scene3D.bShowAllFrames;
     Fit(scene3D.bCoordsCenterSymmetry,scene3D.bTrueAspectRatio);
  end;
end;

//------------------------------------------------------------------------------
//                       Write file
//------------------------------------------------------------------------------
procedure TGLVisir.SaveBtnClick(Sender: TObject);
begin
 if (SaveDialog1.Execute) then
  begin
   if (ExtractFileExt(SaveDialog1.fileName) = '.bmp') then
    begin
     scene3D.SaveFrameToBMP(SaveDialog1.FileName);
    end
   else
    begin
     scene3D.SaveScene(SaveDialog1.FileName);
    end;
  end;
end;


//------------------------------------------------------------------------------
//       About
//------------------------------------------------------------------------------
procedure TGLVisir.AboutBtnClick(Sender: TObject);
begin
 AboutBox.ShowModal;
end;


//------------------------------------------------------------------------------
//                Mouse move processing
//------------------------------------------------------------------------------
procedure TGLVisir.ImagePanelContainerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
 // if left button pressed
 if (mouseBtn1) then
  begin
//   if (rotateMode) then
    begin
     if (X > mouseStartX) then
      scene3D.ProcessAction(maRotateRight, (X-mouseStartX)*mouseKRotate)
     else
      if (X < mouseStartX) then
       scene3D.ProcessAction(maRotateLeft, (mouseStartX-X)*mouseKRotate);
     if (Y > mouseStartY) then
      scene3D.ProcessAction(maRotateDown, (Y-mouseStartY)*mouseKRotate)
     else
      if (Y < mouseStartY) then
       scene3D.ProcessAction(maRotateUp, (mouseStartY-Y)*mouseKRotate);
    end
{   else
    begin
     if (X > mouseStartX) then
      scene3D.ProcessAction(MoveRight,(X-mouseStartX)*mouseKMove)
     else
      if (X < mouseStartX) then
       scene3D.ProcessAction(MoveLeft,(mouseStartX-X)*mouseKMove);
     if (Y > mouseStartY) then
      scene3D.ProcessAction(MoveDown,(Y-mouseStartY)*mouseKMove)
     else
      if (Y < mouseStartY) then
       scene3D.ProcessAction(MoveUp,(mouseStartY-Y)*mouseKMove);
    end}
  end
 else
 // if right button pressed
 if (mouseBtn2) then
  begin
{
     if (X > mouseStartX) then
      scene3D.ProcessAction(ZoomIn, (X-mouseStartX)*mouseKZoom)
     else
      if (X < mouseStartX) then
       scene3D.ProcessAction(ZoomOut, (mouseStartX-X)*mouseKZoom);
}
//     If RotateMode Then
       if (X > mouseStartX) then
        scene3D.ProcessAction(maRotateLeft2, (X-mouseStartX)*mouseKRotate)
       else
        if (X < mouseStartX) then
         scene3D.ProcessAction(maRotateRight2, (mouseStartX-X)*mouseKRotate);
     if (Y > mouseStartY) then
      scene3D.ProcessAction(maZoomIn, (Y-mouseStartY)*mouseKZoom)
     else
      if (Y < mouseStartY) then
       scene3D.ProcessAction(maZoomOut, (mouseStartY-Y)*mouseKZoom);
  end;

 if (mouseBtn1 or mouseBtn2) then
   Redraw;

 mouseStartX := X; // click coordinates
 mouseStartY := Y;
end;


//------------------------------------------------------------------------------
//                Click processing
//------------------------------------------------------------------------------
procedure TGLVisir.ImagePanelContainerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    mouseStartX := X; // coordinates
    mouseStartY := Y;
    if (Button = mbLeft) then
     begin
      mouseBtn1 := true;
      mouseBtn2 := false;
     end
    else
     begin
      mouseBtn1 := false;
      mouseBtn2 := true;
     end;
end;

procedure TGLVisir.ImagePanelContainerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
      mouseBtn1 := false;
      mouseBtn2 := false;
end;

procedure TGLVisir.FrameResize(Sender: TObject);
begin
  Scene3D.Scene.UpdateArea(ImagePanel.width,ImagePanel.height);
  Redraw;
end;

////////////////////////////////////////////////////////////////////////////////
function TGLVisir.GetBackCol : TColor;
begin
 result:=scene3D.backgroundColor;
end;

procedure TGLVisir.SetBackCol(value:TColor);
begin
 scene3D.SetBackgroundColor(value);
 ImagePanel.Color := value;
 FrameResize(nil);
end;

function TGLVisir.GetaxisesNamesCol : TColor;
begin
 result:=scene3D.axisNameColor;
end;

procedure TGLVisir.SetAxisesNamesCol(value:TColor);
begin
 scene3D.axisNameColor := value;
 if (scene3D.lastFrame >= 0) then
  if (csDesigning in ComponentState) then
  begin
   scene3D.ClearScene;
   AddSphereToNewFrame;
   Fit(true,true);
   Redraw;
  end;
end;

function TGLVisir.GetDrawStyle;
begin
 case StyleCombo.ItemIndex of
  0: result := Wired;
  1: result := Filled;
  2: result := Combined;
  else
   result := Filled;
 end;
end;

procedure TGLVisir.SetDrawStyle(value:ObjectDrawStyleType);
begin
 case value of
  Wired: StyleCombo.ItemIndex:=0;
  Filled: StyleCombo.ItemIndex:=1;
  Combined: StyleCombo.ItemIndex:=2;
 end;
 StyleComboChange(nil);
 Redraw;
end;

function TGLVisir.GetCaption : string;
begin
 result:= TitlePanel.Caption;
end;

procedure TGLVisir.SetCaption(value:string);
begin
 TitlePanel.Caption := value;
end;

function TGLVisir.GetaxisXName : string;
begin
 result:=scene3D.axisXName;
end;

procedure TGLVisir.SetaxisXName(value:string);
begin
 scene3D.axisXName := value;
 if (scene3D.lastFrame >= 0) then
  if (csDesigning in ComponentState) then
  begin
   scene3D.ClearScene;
   AddSphereToNewFrame;
   Fit(true,true);
   Redraw;
  end;
end;

function TGLVisir.GetaxisYName : string;
begin
 result:=scene3D.axisYName;
end;

procedure TGLVisir.SetaxisYName(value:string);
begin
 scene3D.axisYName := value;
 if (scene3D.lastFrame >= 0) then
  if (csDesigning in ComponentState) then
  begin
   scene3D.ClearScene;
   AddSphereToNewFrame;
   Fit(true,true);
   Redraw;
  end;
end;

function TGLVisir.GetaxisZName : string;
begin
 result:=scene3D.axisZName;
end;

procedure TGLVisir.SetaxisZName(value:string);
begin
 scene3D.axisZName := value;
 if (scene3D.lastFrame >= 0) then
  if (csDesigning in ComponentState) then
  begin
   scene3D.ClearScene;
   AddSphereToNewFrame;
   Fit(true,true);
   Redraw;
  end;
end;


///////////////////////////////////////////////////////////////////////////////
procedure TGLVisir.AddSphereToNewFrame;
Const
  NXY = 20;
  NXZ = 20;

var mdl : TModelRec;
//    v : array [0..500] of VectorType;
//    p : array [0..1000] of PolygonType;
    v : array [0..NXY * NXZ - 1] of VectorType;
    p : array [0..NXY * NXZ - 1] of PolygonType;
    i,j,nv,np:integer;
    r : double;

    i1,j1 : Double;
    j2    : Integer;
    i3,j3 : Integer;
    si,ci : Double;

begin

  // Speed-optimized code

  r := 1;
  For i := 0 To NXY - 1 Do
  Begin
    j2 := 0;
    i1 := i / NXY;
    i3 := (i + 1) Mod NXY;
    si := sin(2 * 3.14 * i1);
    ci := cos(2 * 3.14 * i1);
    For j := 0 To NXZ - 1 Do
    Begin
      j1 := j / NXZ;
      j3 := ((j + 1) Mod NXZ) * NXY;
      v[i+j*NXY].x    := r * si * cos(2*3.14*j1);
      v[i+j*NXY].y    := r * ci * cos(2*3.14*j1);
      v[i+j*NXY].z    := r * sin(2*3.14*j1);
      p[i+j*NXY].n[0] := i  + j2;
      p[i+j*NXY].n[1] := i3 + j2;
      p[i+j*NXY].n[2] := i3 + j3;
      p[i+j*NXY].n[3] := i  + j3;
      p[i+j*NXY].numVertexes := 4;
      p[i+j*NXY].col := clRed;
      Inc(j2,NXY);
    End; // For j
  End; // For i
  nv := NXY * NXZ;
  np := NXY * NXZ;
{
  r:=1;
  nv := 0; np:= 0;
  for i:=0 to 19 do
   for j:=0 to 19 do
    begin
     v[nv+i+j*20].x := r*sin(2*3.14*i/20)*cos(2*3.14*j/20);
     v[nv+i+j*20].y := r*cos(2*3.14*i/20)*cos(2*3.14*j/20);
     v[nv+i+j*20].z := r*sin(2*3.14*j/20);
    end;

  for i:=0 to 19 do
   for j:=0 to 19 do
    begin
     p[np+i+j*20].n[0] := nv+i+j*20;
     p[np+i+j*20].n[1] := nv+((i+1) mod 20)+j*20;
     p[np+i+j*20].n[2] := nv+((i+1) mod 20)+((j+1) mod 20)*20;
     p[np+i+j*20].n[3] := nv+i+((j+1) mod 20)*20;
     p[np+i+j*20].numVertexes:=4;
     p[np+i+j*20].col := clRed;
    end;
  nv :=nv+400; np:=np+400;
}
  // prepare structure for AddModel call
  mdl.nv := nv;
  mdl.np := np;
  SetLength(mdl.v, mdl.nv);
  SetLength(mdl.p, mdl.np);
  for i:=0 to nv-1 do mdl.v[i] := v[i];
  for i:=0 to np-1 do mdl.p[i] := p[i];
  AddModelToNewFrame(mdl);
end;

//------------------------------------------------------------------------------
//                Save models
//------------------------------------------------------------------------------
procedure TGLVisir.SaveToFile(fileName : string);
begin
     scene3D.SaveScene(fileName);
end;

//------------------------------------------------------------------------------
//                Read models
//------------------------------------------------------------------------------
procedure TGLVisir.LoadFromFile(fileName : string);
begin
     scene3D.LoadScene(fileName);
end;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


//------------------------------------------------------------------------------
//         Create and set default parameters values
//------------------------------------------------------------------------------
constructor TScene3D.Create;
var
  light:Tlight;
begin
  imagePanel         := iImagePanel;
  diapasonesPanel    := iDiapasonesPanel;
  bInitialized       := false; // graphic was not initialized
  curFrame           := -1; // current frame No
  lastFrame          := -1; // last frame No
  bCycledAnimation   := true; // cycled animation
  bShowAllFrames     := false; // show frame by frame
  bShowBoundPlanes   := true; // show bound planes
  bShowTicks         := false; // don't show ticks
  bShowDiapasones    := true; // show diapasones
  planeColor         := clWhite;
  backgroundColor    := clBlack;
  modelColor         := clBlue;
  modelCombinedColor := clWhite;
  FontColor          := clWhite;
  axisXTickCount     := 2;
  axisYTickCount     := 2;
  axisZTickCount     := 2;
  axisXName          := 'X';
  axisYName          := 'Y';
  axisZName          := 'Z';
  axisNameColor      := clYellow;
  fontScale          := 0.2;
  deltaRotate        := 3;
  deltaMove          := 0.1;
  deltaZoom          := 0.1;
  SampleText         := nil;
  Scene              := TSceneGL.create; // create empty scene

  // create the light

  light              := Tlight.create(1);
  light.LightType    := CLstar;
  light.CutOffAngle  := 5;
  Light.SpotExponent := 200;
  Light.SetOrientation(1,1,1);
  Light.Source.SetPosition(-10,0,-5);
  Scene.Lights.add(light);

  Scene.InitRC(ImagePanel.handle);    {initialice Rendering context}
  Scene.UpdateArea(ImagePanel.width,ImagePanel.height);

  Mouse := T3DMouse.create(nil);            {instantiate the manipulation class}
  Mouse.scale(1,1,0.1,1,1,1);               {set movement and rotation speeds}
end;


//------------------------------------------------------------------------------
//                   Graph init
//------------------------------------------------------------------------------
procedure TScene3D.Init;
begin
  Scene.InitRC(ImagePanel.handle);    {initialice Rendering context}
  Scene.UpdateArea(ImagePanel.width,ImagePanel.height);

  // Create font sample

  SampleText := T3Dtext.create(Scene.DC, 1);
  SampleText.fontScale[1] := fontScale;
  SampleText.fontScale[2] := fontScale;
  SampleText.fontScale[3] := fontScale;
  SampleText.id:=1;
end;


//------------------------------------------------------------------------------
//                     Scene redraw
//------------------------------------------------------------------------------
procedure TScene3D.Redraw;
begin
 try
   DrawDiapasones;
   Scene.Redraw;
  except
  end;
end;


//------------------------------------------------------------------------------
// Draw value ranges
//------------------------------------------------------------------------------
procedure TScene3D.DrawDiapasones;
var s:string;
begin
  diapasonesPanel.Visible := bShowDiapasones;
  s := '';
  s := s + axisXName+': ('+FormatFloat('#0.0000',xmin)+', '+
                    FormatFloat('#0.0000',xmax)+')  ';
  s := s + axisYName+': ('+FormatFloat('#0.0000',ymin)+', '+
                    FormatFloat('#0.0000',ymax)+')  ';
  s := s + axisZName+':('+FormatFloat('#0.0000',zmin)+', '+
                    FormatFloat('#0.0000',zmax)+')  ';
  diapasonesPanel.Caption := s;
end;

//------------------------------------------------------------------------------
//           Draw on update
//------------------------------------------------------------------------------
procedure TScene3D.Update;
begin
  // if 1st call - graph init
  if not bInitialized then Init;
  Scene.UpdateArea(ImagePanel.width,ImagePanel.height);
  Redraw;
end;

//------------------------------------------------------------------------------
// add model to frmae
//------------------------------------------------------------------------------
Procedure TScene3D.AddModelToEntity(mdl: TModelRec; entity: TEntity);
Var
  i,j   : Integer;
  Face  : TFace;
  r,g,b : Integer;
  K     : Integer;
  P     : PolygonType;

Begin
  // by all polygons
  For i := 0 To mdl.np - 1 Do
  Begin
    // get colors components
    modelColor := mdl.p[i].col;

    // This is faster
    r := modelColor And $FF;
    g := (modelColor Shr 8) And $FF;
    b := (modelColor Shr 16) And $FF;
{
    r := (mdl.p[i].col div $1) mod $100;
    g := (mdl.p[i].col div $100) mod $100;
    b := (mdl.p[i].col div $10000) mod $100;
}
    // set current color
    entity.R := r;
    entity.G := g;
    entity.B := b;

    //--- add Face ---
    Face := entity.addFace;

    // add vertexes
    P := mdl.p[i];
    For j := 0 To P.numVertexes - 1 Do
    Begin
      K := P.n[j];
      Face.AddVertex(mdl.v[K].x,
                     mdl.v[K].y,
                     mdl.v[K].z,0,0,1);
    End; // For j
    
    //--- add Face (mirror) ---
    Face := entity.addFace;
    // add vertexes
    For j := P.numVertexes - 1 DownTo 0 Do
    Begin
      K := P.n[j];
      Face.AddVertex(mdl.v[K].x,
                     mdl.v[K].y,
                     mdl.v[K].z,0,0,1);
    End; // For j
  End; // For i

  // set lines color in combined style
{
  r := (modelCombinedColor div $1) mod $100;
  g := (modelCombinedColor div $100) mod $100;
  b := (modelCombinedColor div $10000) mod $100;
}
  // This is faster
  r := modelCombinedColor And $FF;
  g := (modelCombinedColor Shr 8) And $FF;
  b := (modelCombinedColor Shr 16) And $FF;

  entity.SetBoundColor(r,g,b);
End; // TScene3D.AddModelToEntity

//------------------------------------------------------------------------------
//        add new frame with specified model
//------------------------------------------------------------------------------
procedure TScene3D.AddModelToNewFrame(mdl : TModelRec);
var entity:Tentity;

begin
  // create new empty model
  entity:=TEntity.Create;
  // if not 1st frame - hide it
  if (curFrame >= 0) then
   entity.visible := false;

  // add model
  AddModelToEntity(mdl, entity);

  // claculate normals
  entity.CalcNormals;
  // add to scene
  Scene.Entities.add(entity);


  
  // inc frames counter
  inc(lastFrame);
  // current frame - 0-th
  if (curFrame = -1) then curFrame:=0;
end;


//------------------------------------------------------------------------------
// add model to specified frame
//------------------------------------------------------------------------------
procedure TScene3D.AddModelToFrame(mdl : TModelRec;nFrame : integer);
begin
  if (nFrame > Scene.Entities.Count - 1)
   then AddModelToNewFrame(mdl)
   else AddModelToEntity(mdl, Scene.Entities.Items[nFrame]);
end;


//------------------------------------------------------------------------------
//  add new tick to axis
//------------------------------------------------------------------------------
procedure TScene3D.AddTick;
var Face:TFace;
    texto:T3dText;
    r,g,b : integer;
begin

  //--- add line ---
  Face:=entity.addFace;
     Face.AddVertex(x-dx,y-dy,z-dz,0,0,1);
     Face.AddVertex(x+dx,y+dy,z+dz,0,0,1);
     Face.AddVertex(x-dx,y-dy,z-dz,0,0,1);
     Face.AddVertex(x+dx,y+dy,z+dz,0,0,1);

  // This is faster
  r := fontColor And $FF;
  g := (fontColor Shr 8) And $FF;
  b := (fontColor Shr 16) And $FF;
{
    // get color components
    r := (fontColor div $1) mod $100;
    g := (fontColor div $100) mod $100;
    b := (fontColor div $10000) mod $100;
}
  texto:=T3Dtext.create(SampleText);
  Texto.SetColor(r,g,b,255);
  Texto.localRotate(a1,a2,a3);
  Texto.Move(x+labelDx,y,z);
  texto.SetText(s);
  Scene.Entities.Add(texto);
end;


//------------------------------------------------------------------------------
//     add new frame with bound planes
//------------------------------------------------------------------------------
procedure TScene3D.AddBoundPlanesToNewFrame;
var entity:Tentity;
    r,g,b : integer;
    Face:TFace;
    x1,x2,y1,y2,z1,z2,dx,dy,dz,xx1,yy1,zz1 : double;
    i : integer;
    texto:T3dText;
    s:String;
begin
  // store 1st elment No
  firstPlane := scene.Entities.Count;
  // create new empty model
  entity:=TEntity.Create;
    x1:=-1;x2:=1;y1:=-1;y2:=1;z1:=-1;z2:=1;
    dx:=0.3;dy:=0.3;dz:=0.3;

    // This is faster
    r := planeColor And $FF;
    g := (planeColor Shr 8) And $FF;
    b := (planeColor Shr 16) And $FF;
{
    // get color components
    r := (planeColor div $1) mod $100;
    g := (planeColor div $100) mod $100;
    b := (planeColor div $10000) mod $100;
}
    // set current color
    entity.SetColor(r,g,b,255);
    // wired
    entity.wireframe := 1;

  if (not bCoordsCenterSymmetry) then
   begin
   //----- add 3 boxes -------
    //--- add Face ---
    Face:=entity.addFace;
     // add Face vertexes
     Face.AddVertex(x1,y1,z1,0,0,1);
     Face.AddVertex(x1,y2,z1,0,0,1);
     Face.AddVertex(x2,y2,z1,0,0,1);
     Face.AddVertex(x2,y1,z1,0,0,1);
    //--- add mirror Face ---
    Face:=entity.addFace;
     // add vertexes
     Face.AddVertex(x1,y1,z1,0,0,1);
     Face.AddVertex(x2,y1,z1,0,0,1);
     Face.AddVertex(x2,y2,z1,0,0,1);
     Face.AddVertex(x1,y2,z1,0,0,1);
    //--- add Face ---
    Face:=entity.addFace;
     // add vertexes
     Face.AddVertex(x1,y1,z1,0,0,1);
     Face.AddVertex(x2,y1,z1,0,0,1);
     Face.AddVertex(x2,y1,z2,0,0,1);
     Face.AddVertex(x1,y1,z2,0,0,1);
    //--- mirror ---
    Face:=entity.addFace;
     // add vertexes
     Face.AddVertex(x1,y1,z1,0,0,1);
     Face.AddVertex(x1,y1,z2,0,0,1);
     Face.AddVertex(x2,y1,z2,0,0,1);
     Face.AddVertex(x2,y1,z1,0,0,1);

    //--- add Face ---
    Face:=entity.addFace;
     // add vertexes
     Face.AddVertex(x1,y1,z1,0,0,1);
     Face.AddVertex(x1,y1,z2,0,0,1);
     Face.AddVertex(x1,y2,z2,0,0,1);
     Face.AddVertex(x1,y2,z1,0,0,1);
    //--- mirror ---
    Face:=entity.addFace;
     // add vertexes
     Face.AddVertex(x1,y1,z1,0,0,1);
     Face.AddVertex(x1,y2,z1,0,0,1);
     Face.AddVertex(x1,y2,z2,0,0,1);
     Face.AddVertex(x1,y1,z2,0,0,1);
   end;
  //--- add axis ---
  if (not bCoordsCenterSymmetry) then
   begin
    xx1 := x1;
    yy1 := y1;
    zz1 := z1;
   //--- axis X ---
    //--- add line ---
    Face:=entity.addFace;
     Face.AddVertex(x2,y1,z1,0,0,1);
     Face.AddVertex(x2+dx,y1,z1,0,0,1);
     Face.AddVertex(x2,y1,z1,0,0,1);
     Face.AddVertex(x2+dx,y1,z1,0,0,1);
    //--- add arrow ---
    Face:=entity.addFace;
     Face.AddVertex(x2+dx,y1,z1,0,0,1);
     Face.AddVertex(x2+dx/2,y1-dy/3,z1,0,0,1);
     Face.AddVertex(x2+dx,y1,z1,0,0,1);
     Face.AddVertex(x2+dx/2,y1-dy/3,z1,0,0,1);
    Face:=entity.addFace;
     Face.AddVertex(x2+dx,y1,z1,0,0,1);
     Face.AddVertex(x2+dx/2,y1+dy/3,z1,0,0,1);
     Face.AddVertex(x2+dx,y1,z1,0,0,1);
     Face.AddVertex(x2+dx/2,y1+dy/3,z1,0,0,1);

   //--- axis Y ---
    //--- add line ---
    Face:=entity.addFace;
     Face.AddVertex(x1,y2,z1,0,0,1);
     Face.AddVertex(x1,y2+dy,z1,0,0,1);
     Face.AddVertex(x1,y2,z1,0,0,1);
     Face.AddVertex(x1,y2+dy,z1,0,0,1);
    //--- add arrow ---
    Face:=entity.addFace;
     Face.AddVertex(x1,y2+dy,z1,0,0,1);
     Face.AddVertex(x1-dx/3,y2+dy/2,z1,0,0,1);
     Face.AddVertex(x1,y2+dy,z1,0,0,1);
     Face.AddVertex(x1-dx/3,y2+dy/2,z1,0,0,1);
    Face:=entity.addFace;
     Face.AddVertex(x1,y2+dy,z1,0,0,1);
     Face.AddVertex(x1+dx/3,y2+dy/2,z1,0,0,1);
     Face.AddVertex(x1,y2+dy,z1,0,0,1);
     Face.AddVertex(x1+dx/3,y2+dy/2,z1,0,0,1);

   //--- axis Z ---
    //--- add line ---
    Face:=entity.addFace;
     Face.AddVertex(x1,y1,z2,0,0,1);
     Face.AddVertex(x1,y1,z2+dz,0,0,1);
     Face.AddVertex(x1,y1,z2,0,0,1);
     Face.AddVertex(x1,y1,z2+dz,0,0,1);
    //--- add arrow ---
    Face:=entity.addFace;
     Face.AddVertex(x1,y1,z2+dz,0,0,1);
     Face.AddVertex(x1,y1-dy/3,z2+dz/2,0,0,1);
     Face.AddVertex(x1,y1,z2+dz,0,0,1);
     Face.AddVertex(x1,y1-dy/3,z2+dz/2,0,0,1);
    Face:=entity.addFace;
     Face.AddVertex(x1,y1,z2+dz,0,0,1);
     Face.AddVertex(x1,y1+dy/3,z2+dz/2,0,0,1);
     Face.AddVertex(x1,y1,z2+dz,0,0,1);
     Face.AddVertex(x1,y1+dy/3,z2+dz/2,0,0,1);
   end
  else
   begin
    xx1 := 0;
    yy1 := 0;
    zz1 := 0;
   //--- axis X ---
    //--- add line ---
    Face:=entity.addFace;
     Face.AddVertex(x1-dx,0,0,0,0,1);
     Face.AddVertex(x2+dx,0,0,0,0,1);
     Face.AddVertex(x1-dx,0,0,0,0,1);
     Face.AddVertex(x2+dx,0,0,0,0,1);
    //--- add arrow ---
    Face:=entity.addFace;
     Face.AddVertex(x2+dx,0,0,0,0,1);
     Face.AddVertex(x2+dx/2,0-dy/3,0,0,0,1);
     Face.AddVertex(x2+dx,0,0,0,0,1);
     Face.AddVertex(x2+dx/2,0-dy/3,0,0,0,1);
    Face:=entity.addFace;
     Face.AddVertex(x2+dx,0,0,0,0,1);
     Face.AddVertex(x2+dx/2,0+dy/3,0,0,0,1);
     Face.AddVertex(x2+dx,0,0,0,0,1);
     Face.AddVertex(x2+dx/2,0+dy/3,0,0,0,1);

   //--- axis Y ---
    //--- add line ---
    Face:=entity.addFace;
     Face.AddVertex(0,y1-dy,0,0,0,1);
     Face.AddVertex(0,y2+dy,0,0,0,1);
     Face.AddVertex(0,y1-dy,0,0,0,1);
     Face.AddVertex(0,y2+dy,0,0,0,1);
    //--- add arrow ---
    Face:=entity.addFace;
     Face.AddVertex(0,y2+dy,0,0,0,1);
     Face.AddVertex(0-dx/3,y2+dy/2,0,0,0,1);
     Face.AddVertex(0,y2+dy,0,0,0,1);
     Face.AddVertex(0-dx/3,y2+dy/2,0,0,0,1);
    Face:=entity.addFace;
     Face.AddVertex(0,y2+dy,0,0,0,1);
     Face.AddVertex(0+dx/3,y2+dy/2,0,0,0,1);
     Face.AddVertex(0,y2+dy,0,0,0,1);
     Face.AddVertex(0+dx/3,y2+dy/2,0,0,0,1);

   //--- axis Z ---
    //--- add line ---
    Face:=entity.addFace;
     Face.AddVertex(0,0,z1-dz,0,0,1);
     Face.AddVertex(0,0,z2+dz,0,0,1);
     Face.AddVertex(0,0,z1-dz,0,0,1);
     Face.AddVertex(0,0,z2+dz,0,0,1);
    //--- add arrow ---
    Face:=entity.addFace;
     Face.AddVertex(0,0,z2+dz,0,0,1);
     Face.AddVertex(0,0-dy/3,z2+dz/2,0,0,1);
     Face.AddVertex(0,0,z2+dz,0,0,1);
     Face.AddVertex(0,0-dy/3,z2+dz/2,0,0,1);
    Face:=entity.addFace;
     Face.AddVertex(0,0,z2+dz,0,0,1);
     Face.AddVertex(0,0+dy/3,z2+dz/2,0,0,1);
     Face.AddVertex(0,0,z2+dz,0,0,1);
     Face.AddVertex(0,0+dy/3,z2+dz/2,0,0,1);
   end;

  // add half-box with axises to scene
  Scene.Entities.add(entity);

  // This is faster
  r := axisNameColor And $FF;
  g := (axisNameColor Shr 8) And $FF;
  b := (axisNameColor Shr 16) And $FF;
{
   // get current color
   r := (axisNameColor div $1) mod $100;
   g := (axisNameColor div $100) mod $100;
   b := (axisNameColor div $10000) mod $100;
}
  // axis names
  texto:=T3Dtext.create(SampleText);
  Texto.SetColor(r,g,b,255);
  Texto.Move(x2+dx,yy1,zz1);
  TexTo.fontScale[1] := TexTo.fontScale[1]*1.3;
  TexTo.fontScale[2] := TexTo.fontScale[2]*1.3;
  TexTo.fontScale[3] := TexTo.fontScale[3]*1.3;
  texto.SetText(axisXName);
  Scene.Entities.Add(texto); 
  texto:=T3Dtext.create(SampleText);
  Texto.SetColor(r,g,b,255);
  Texto.Move(xx1,y2+dy,zz1);
  TexTo.fontScale[1] := TexTo.fontScale[1]*1.3;
  TexTo.fontScale[2] := TexTo.fontScale[2]*1.3;
  TexTo.fontScale[3] := TexTo.fontScale[3]*1.3;
  texto.SetText(axisYName);
  Scene.Entities.Add(texto); 
  texto:=T3Dtext.create(SampleText);
  Texto.SetColor(r,g,b,255);
  Texto.Move(xx1,yy1,z2+dy);
  TexTo.fontScale[1] := TexTo.fontScale[1]*1.3;
  TexTo.fontScale[2] := TexTo.fontScale[2]*1.3;
  TexTo.fontScale[3] := TexTo.fontScale[3]*1.3;
  texto.SetText(axisZName);
  Scene.Entities.Add(texto); 

  // No of last element
  lastPlane := scene.Entities.Count-1;

  // No of 1st element
  firstTick := scene.Entities.Count;
  // create new empty model
  entity:=TEntity.Create;
  // ticks on axis X
  for i:=0 to axisXTickCount-1 do
  begin
   s := FormatFloat('#0.00',xmin+(xmax-xmin)*i/(axisXTickCount-1));
   AddTick(entity, x1+(x2-x1)*i/(axisXTickCount-1)+dx/4,yy1-dy/2,zz1-dz/4, 0,dy/4,0, 0,90,0, 0,s);
  end;
  // ticks on axis Y
  for i:=0 to axisyTickCount-1 do
  begin
   s := FormatFloat('#0.00',ymin+(ymax-ymin)*i/(axisyTickCount-1));
   AddTick(entity, xx1,y1+(y2-y1)*i/(axisyTickCount-1)+dy/4,zz1-dz/4, 0,0,dz/4, 0,-45,0, -0.3,s);
  end;
  // ticks on axis Y
  for i:=0 to axiszTickCount-1 do
  begin
   s := FormatFloat('#0.00',zmin+(zmax-zmin)*i/(axiszTickCount-1));
   AddTick(entity, xx1,yy1-dy/2,z1+(z2-z1)*i/(axiszTickCount-1)+dz/4, 0,dy/4,0, 0,0,0, -0.3,s);
  end;
  // add ticks to scene
  Scene.Entities.add(entity);

  // last element No
  lastTick := scene.Entities.Count-1;
end;


//------------------------------------------------------------------------------
//        Center and zoom all frames
//------------------------------------------------------------------------------
procedure TScene3D.Fit;
var x1,x2,y1,y2,z1,z2,
    cx,cy,cz,kx,ky,kz,k:double;
    i:integer;
begin
  bCoordsCenterSymmetry := bCoordsCenterSymmetry1;
  bTrueAspectRatio := bTrueAspectRatio1;
  
  // get coordinate ranges
  xmax := -100000000; ymax := -100000000; zmax := -100000000;
  xmin := 100000000; ymin := 100000000; zmin := 100000000;

  for i:=0 to Scene.Entities.Count-1 do
   begin
     TEntity(Scene.Entities.Items[i]).GetDiapasones(x1,x2,y1,y2,z1,z2);
     if (x1<xmin) then xmin:=x1;
     if (y1<ymin) then ymin:=y1;
     if (z1<zmin) then zmin:=z1;
     if (x2>xmax) then xmax:=x2;
     if (y2>ymax) then ymax:=y2;
     if (z2>zmax) then zmax:=z2;
   end;

  if (bCoordsCenterSymmetry) then
   begin
    x1 := -max(abs(xmin),abs(xmax)); x2:=abs(x1);
    y1 := -max(abs(ymin),abs(ymax)); y2:=abs(y1);
    z1 := -max(abs(zmin),abs(zmax)); z2:=abs(z1);
   end
  else
   begin
    x1 := xmin; x2 := xmax;
    y1 := ymin; y2 := ymax;
    z1 := zmin; z2 := zmax;
   end;

  // center coords
  cx := (x1+x2)/2;
  cy := (y1+y2)/2;
  cz := (z1+z2)/2;

  // zoom multiplier
  if (abs(x2-x1)>0.000001) then
   kx := 2/(x2-x1)
  else
   kx := 1;
  if (abs(y2-y1)>0.000001) then
   ky := 2/(y2-y1)
  else
   ky := 1;
  if (abs(z2-z1)>0.000001) then
   kz := 2/(z2-z1)
  else
   kz := 1;

  if (bTrueAspectRatio) then
   k := min(kx,min(ky,kz))
  else
   k := kz;

  if (not bCoordsCenterSymmetry) then
   if (zmin = 0) then
    if (zmax - zmin < 2/k) then
     cz := 0;

  // center and zoom
  for i:=0 to Scene.Entities.Count-1 do
   begin
     if (not bCoordsCenterSymmetry) then
      TEntity(Scene.Entities.Items[i]).MoveVertices(-cx,-cy,-cz);
     if (bTrueAspectRatio) then
      TEntity(Scene.Entities.Items[i]).MulVertices(k,k,k)
     else
      TEntity(Scene.Entities.Items[i]).MulVertices(kx,ky,kz);
   end;

  // flat pictures or sets started from zero move to the back wall
  if (zmin = 0) then
   if ((zmin = zmax) or (zmax - zmin < 2/k)) then
    begin
     if (not bCoordsCenterSymmetry) then
      begin
       cz := 1;
       for i:=0 to Scene.Entities.Count-1 do
        TEntity(Scene.Entities.Items[i]).MoveVertices(0,0,-cz);
      end;
    end;

  AddBoundPlanesToNewFrame(bCoordsCenterSymmetry); // add bound planes
  ShowBoundPlanes(bShowBoundPlanes);
  ShowTicks(bShowTicks);
end;


//------------------------------------------------------------------------------
// Show boundary planes
//------------------------------------------------------------------------------
procedure TScene3D.ShowBoundPlanes(flag : boolean);
var i :integer;
begin
 bShowBoundPlanes := flag;
 // show / hide planes
 for i:=firstPlane to lastPlane do
   TEntity(Scene.Entities.Items[i]).visible := flag;
 // и отметки тоже
 for i:=firstTick to lastTick do
    TEntity(Scene.Entities.Items[i]).visible := flag;
end;


//------------------------------------------------------------------------------
// show ticks on axis
//------------------------------------------------------------------------------
procedure TScene3D.ShowTicks(flag : boolean);
var i :integer;
begin
 bShowTicks := flag;
 // show / hide
 for i:=firstTick to lastTick do
    TEntity(Scene.Entities.Items[i]).visible := flag;
end;


//------------------------------------------------------------------------------
// show all frames simultaneously?
//------------------------------------------------------------------------------
procedure TScene3D.ShowAllFrames(flag : boolean);
var i :integer;
begin
 bShowAllFrames := flag;
 // show / hide
 for i:=0 to lastFrame do
  TEntity(Scene.Entities.Items[i]).visible := flag;

 TEntity(Scene.Entities.Items[curFrame]).visible := true;
end;


//------------------------------------------------------------------------------
// set draw style
//------------------------------------------------------------------------------
procedure TScene3D.SetStyle;
var i :integer;
begin
 modelStyle := style;
 for i:=0 to lastFrame do
    TEntity(Scene.Entities.Items[i]).wireframe := modelStyle;
end;


//------------------------------------------------------------------------------
// set color of model
//------------------------------------------------------------------------------
procedure TScene3D.SetModelColor(col: TColor);
var i :integer;
    r,g,b:integer;
begin
 modelColor := col;

  // This is faster
  r := modelColor And $FF;
  g := (modelColor Shr 8) And $FF;
  b := (modelColor Shr 16) And $FF;
{
 // get RGB components
 r := (modelColor div $1) mod $100;
 g := (modelColor div $100) mod $100;
 b := (modelColor div $10000) mod $100;
}
 for i:=0 to lastFrame do
    TEntity(Scene.Entities.Items[i]).SetColor(r,g,b,255);
end;


//------------------------------------------------------------------------------
// set background color
//------------------------------------------------------------------------------
procedure TScene3D.SetBackgroundColor(col: TColor);
var  r,g,b:integer;
begin
 backgroundColor := col;

  // This is faster
  r := backgroundColor And $FF;
  g := (backgroundColor Shr 8) And $FF;
  b := (backgroundColor Shr 16) And $FF;
{
 // get RGB components
 r := (backgroundColor div $1) mod $100;
 g := (backgroundColor div $100) mod $100;
 b := (backgroundColor div $10000) mod $100;
}
 Scene.BackR:=r / 255;
 Scene.BackG:=g / 255;
 Scene.BackB:=b / 255;
end;


//------------------------------------------------------------------------------
//         set active frame
//------------------------------------------------------------------------------
procedure TScene3D.SetFrameByNum;
begin
 TEntity(Scene.Entities.Items[curFrame]).visible := false;
 curFrame := iFrame;
 TEntity(Scene.Entities.Items[curFrame]).visible := true;
end;


//------------------------------------------------------------------------------
//           Set previous frame active
//------------------------------------------------------------------------------
procedure TScene3D.SetPrevFrame;
begin
 TEntity(Scene.Entities.Items[curFrame]).visible := false;
 curFrame :=curFrame-1;
 if (curFrame < 0) then
  if (bCycledAnimation) then
   curFrame:=lastFrame
  else
   curFrame:=0;
 TEntity(Scene.Entities.Items[curFrame]).visible := true;
end;


//------------------------------------------------------------------------------
//           Set next frame active
//------------------------------------------------------------------------------
procedure TScene3D.SetNextFrame;
begin
 TEntity(Scene.Entities.Items[curFrame]).visible := false;
 curFrame:=curFrame+1;
 if (curFrame > lastFrame) then
  if (bCycledAnimation) then
   curFrame:=0
  else
   curFrame:=lastFrame;

 TEntity(Scene.Entities.Items[curFrame]).visible := true;
end;


//------------------------------------------------------------------------------
//           Process actions of scene modification
//------------------------------------------------------------------------------
procedure TScene3D.ProcessModifyAction;
begin
  case (modifyAction) of
   maRotateUp,maRotateDown,maRotateLeft,maRotateRight,
   maRotateLeft2,maRotateRight2:
      ProcessAction(modifyAction,deltaRotate);
   maMoveUp,maMoveDown,maMoveLeft,maMoveRight,
   maMoveForward,maMoveBackward:
      ProcessAction(modifyAction,deltaMove);
   maZoomIn,maZoomOut:
      ProcessAction(modifyAction,deltaZoom);
  end;
  if (modifyAction <> maNone) then Redraw;
end;


//------------------------------------------------------------------------------
//            Change scene
//------------------------------------------------------------------------------
procedure TScene3D.ProcessAction;
var i:integer;
begin
  case (action) of
   maRotateRight:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).Rotation[2] :=
        TEntity(Scene.Entities.Items[i]).Rotation[2] + delta;
      end;
    end;
   maRotateLeft:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).Rotation[2] :=
        TEntity(Scene.Entities.Items[i]).Rotation[2] - delta;
      end;
    end;
   maRotateUp:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).Rotation[1] :=
        TEntity(Scene.Entities.Items[i]).Rotation[1] - delta;
      end;
    end;
   maRotateDown:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).Rotation[1] :=
        TEntity(Scene.Entities.Items[i]).Rotation[1] + delta;
      end;
    end;
   maRotateLeft2:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).Rotation[3] :=
        TEntity(Scene.Entities.Items[i]).Rotation[3] - delta;
      end;
    end;
   maRotateRight2:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).Rotation[3] :=
        TEntity(Scene.Entities.Items[i]).Rotation[3] + delta;
      end;
    end;
   maMoveLeft:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).localPosition[1] :=
        TEntity(Scene.Entities.Items[i]).localPosition[1] + delta;
      end;
    end;
   maMoveRight:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).localPosition[1] :=
        TEntity(Scene.Entities.Items[i]).localPosition[1] - delta;
      end;
    end;
   maMoveUp:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).localPosition[2] :=
        TEntity(Scene.Entities.Items[i]).localPosition[2] - delta;
      end;
    end;
   maMoveDown:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).localPosition[2] :=
        TEntity(Scene.Entities.Items[i]).localPosition[2] + delta;
      end;
    end;
   maMoveForward:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).localPosition[3] :=
        TEntity(Scene.Entities.Items[i]).localPosition[3] + delta;
      end;
    end;
   maMoveBackward:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).localPosition[3] :=
        TEntity(Scene.Entities.Items[i]).localPosition[3] - delta;
      end;
    end;
   maZoomIn:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).Scale[1] :=
        TEntity(Scene.Entities.Items[i]).Scale[1] + delta;
       TEntity(Scene.Entities.Items[i]).Scale[2] :=
        TEntity(Scene.Entities.Items[i]).Scale[2] + delta;
       TEntity(Scene.Entities.Items[i]).Scale[3] :=
        TEntity(Scene.Entities.Items[i]).Scale[3] + delta;
      end;
    end;
   maZoomOut:
    begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).Scale[1] :=
        TEntity(Scene.Entities.Items[i]).Scale[1] - delta;
       TEntity(Scene.Entities.Items[i]).Scale[2] :=
        TEntity(Scene.Entities.Items[i]).Scale[2] - delta;
       TEntity(Scene.Entities.Items[i]).Scale[3] :=
        TEntity(Scene.Entities.Items[i]).Scale[3] - delta;
      end;
    end;
  end;
end;


//------------------------------------------------------------------------------
//  restore default parameters
//------------------------------------------------------------------------------
procedure TScene3D.DefaultPosition;
var i:integer;
begin
     for i:=0 to Scene.Entities.Count-1 do
      begin
       TEntity(Scene.Entities.Items[i]).localPosition[1] := 0;
       TEntity(Scene.Entities.Items[i]).localPosition[2] := 0;
       TEntity(Scene.Entities.Items[i]).localPosition[3] := 0;
       TEntity(Scene.Entities.Items[i]).Rotation[1] := 0;
       TEntity(Scene.Entities.Items[i]).Rotation[2] := 0;
       TEntity(Scene.Entities.Items[i]).Rotation[3] := 0;
       TEntity(Scene.Entities.Items[i]).Scale[1] := 1;
       TEntity(Scene.Entities.Items[i]).Scale[2] := 1;
       TEntity(Scene.Entities.Items[i]).Scale[3] := 1;
      end;
      Redraw;
end;


//------------------------------------------------------------------------------
// remove all frames
//------------------------------------------------------------------------------
procedure TScene3D.ClearScene;
var i:integer;
begin
 for i:=0 to scene.Entities.Count-1 do
  TEntity(Scene.Entities.Items[i]).Free;
 Scene.Entities.Clear;
 curFrame := -1;
 lastFrame := -1;
end;


//------------------------------------------------------------------------------
// save all frames to 3mf file
//------------------------------------------------------------------------------
procedure TScene3D.SaveScene;
var st : TFileStream;
    i:integer;
    strLen : longInt;
    strStream : TStringStream;
begin
 st := TFileStream.Create(fileName,fmCreate or fmOpenWrite);
 //--- save header ---
 st.write('3mf',3); // file type ID
 st.write(bCoordsCenterSymmetry,sizeof(bCoordsCenterSymmetry)); // coordinate sytem - in center?
 st.write(bTrueAspectRatio,sizeof(bTrueAspectRatio)); // true aspect ratio?
 st.write(bCycledAnimation,sizeof(bCycledAnimation)); // cycled animation
 st.write(bShowAllFrames,sizeof(bShowAllFrames)); // frame by frame?
 st.write(bShowBoundPlanes,sizeof(bShowBoundPlanes));
 st.write(lastFrame,sizeof(lastFrame));
 st.write(curFrame,sizeof(curFrame));
 st.write(modelColor,sizeof(modelColor));
 st.write(modelCombinedColor,sizeof(modelColor));
 st.write(backgroundColor,sizeof(backgroundColor));
 st.write(axisNameColor,sizeof(TColor));
 strStream := TStringStream.Create(axisXName);
 strLen := strStream.Size;
 st.write(strLen,sizeof(longInt));
 st.CopyFrom(strStream,strStream.Size);
 strStream.Free;
 strStream := TStringStream.Create(axisYName);
 strLen := strStream.Size;
 st.write(strLen,sizeof(longInt));
 st.CopyFrom(strStream,strStream.Size);
 strStream.Free;
 strStream := TStringStream.Create(axisZName);
 strLen := strStream.Size;
 st.write(strLen,sizeof(longInt));
 st.CopyFrom(strStream,strStream.Size);
 strStream.Free;
 //--- save main data ---
 for i:=0 to lastFrame do
  TEntity(Scene.Entities.Items[i]).SaveToStream(st);
 st.Free;
end;


//------------------------------------------------------------------------------
// load frames from 3mf file
//------------------------------------------------------------------------------
procedure TScene3D.LoadScene;
var st : TFileStream;
    i:integer;
    StrLen : longInt;
    id : array [0..2] of char;
    entity : TEntity;
    strStream : TStringStream;
begin
 st := TFileStream.Create(fileName,fmOpenRead);
 //--- load header ---
 st.read(id,3); // file type ID
 if (String(id) <> '3mf') then
  Raise Exception.Create('Invalid file format!')
 else
  ClearScene;
 st.read(bCoordsCenterSymmetry,sizeof(bCoordsCenterSymmetry));
 st.read(bTrueAspectRatio,sizeof(bTrueAspectRatio));
 st.read(bCycledAnimation,sizeof(bCycledAnimation));
 st.read(bShowAllFrames,sizeof(bShowAllFrames));
 st.read(bShowBoundPlanes,sizeof(bShowBoundPlanes));
 st.read(lastFrame,sizeof(lastFrame));
 st.read(curFrame,sizeof(curFrame));
 st.read(modelColor,sizeof(modelColor));
 st.read(modelCombinedColor,sizeof(modelColor));
 st.read(backgroundColor,sizeof(backgroundColor));
 st.read(axisNameColor,sizeof(TColor));

 st.read(strLen,sizeof(longInt));
 strStream := TStringStream.Create('');
 strStream.CopyFrom(st,strLen);
 axisXName := strStream.DataString;
 strStream.Free;
 st.read(strLen,sizeof(longInt));
 strStream := TStringStream.Create('');
 strStream.CopyFrom(st,strLen);
 axisYName := strStream.DataString;
 strStream.Free;
 st.read(strLen,sizeof(longInt));
 strStream := TStringStream.Create('');
 strStream.CopyFrom(st,strLen);
 axisZName := strStream.DataString;
 strStream.Free;

 //--- считываем тело ---
 for i:=0 to lastFrame do
 begin
  entity := TEntity.Create;
  entity.LoadFromStream(st);
  Scene.Entities.Add(entity);
 end;
 st.Free;
end;

//------------------------------------------------------------------------------
// save frame to BMP
//------------------------------------------------------------------------------
procedure TScene3D.SaveFrameToBMP;
var bmp : TBitmap;
    tempCanvas : TCanvas;
    destRect: TRect;
begin
 Redraw;
 bmp := TBitmap.Create;
 with bmp do
  begin
   Height := imagePanel.clientrect.Bottom-imagePanel.clientrect.Top+1;
   Width :=  imagePanel.clientrect.Right-imagePanel.clientrect.Left+1;
   destRect := Rect(0,0,Width,Height);
  end;
 tempCanvas := TCanvas.Create;
 tempCanvas.Handle := GetDC(imagePanel.Handle);
 bmp.Canvas.CopyRect(destRect, tempCanvas, imagePanel.clientrect);
 bmp.SaveToFile(fileName);
 bmp.free;
end;

////////////////////////////////////////////////////////////////////////////////
procedure Register;
begin
  RegisterComponents('Samples', [TGLVisir]);
end;


procedure TGLVisir.btnRotateLeft2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maRotateLeft;
end;

procedure TGLVisir.btnRotateRight2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maRotateRight;
end;

procedure TGLVisir.btnMoveForwardMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maMoveForward;
end;

procedure TGLVisir.btnMoveBackwardMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maMoveBackward;
end;

procedure TGLVisir.btnMoveLeftMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maMoveLeft;
end;

procedure TGLVisir.btnMoveRightMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maMoveRight;
end;

procedure TGLVisir.btnMoveUpMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maMoveUp;
end;

procedure TGLVisir.btnMoveDownMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  scene3D.modifyAction := maMoveDown;
end;

end.
