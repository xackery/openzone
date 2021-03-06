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
Unit GLVisir;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLU, URickGL, U3DPolys, ExtCtrls, StdCtrls, Buttons,
  ComCtrls, ImgList, Math, About3D, ActnList, Points3D, GLPanel;

Type
  // Polygon type

  PNormal = ^TNormal;
  TNormal = Packed Record
    X,Y,Z,W: ShortInt;
  End;

  VertexType = Packed Record
    Normal      : LongWord;          // xyzw format to save memory
//    NX,NY,NZ    : Single;            // normal vector, each vertex has it?s own normal vector, this is useful for certain tricks
    TX,TZ       : Single;            // Texture X and Z coordinates
    X,Y,Z       : Single;            // position x,y,z
    Color       : TColor;            // vertex?s color, rgba - Must be bytes and in this order for glColor4ubv() to work
  End;

  PolygonType = Record
//    N           : Array [0..2] Of Integer; // Vertexes no
//    NumVertexes : Integer;                 // Number of vertexes   2 - line, 3 - triangle, 4 - box
    TextureID   : Integer;
    Solid       : Boolean;
  End;

  // 3D model record definition
  TModelRec = Record
    V  : Array Of VertexType;  // Array of vertexes
    P  : Array Of PolygonType; // Array of polygons
    NV : Integer;              // Number of vertexes
    NP : Integer;              // Number of polygons
  End;

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

  TObjectDrawStyle = (Wired, Filled, Combined);

  TModifyAction = (None, RotateRight, RotateLeft, RotateUp, RotateDown,
                   MoveRight, MoveLeft, MoveUp, MoveDown, ZoomIn, ZoomOut,
                   RotateLeft2, RotateRight2);

  TNewDiapasones = Procedure(St: String) Of Object;

  TScene3D = Class;
  TMakePanelThread = Class;

  TGLVisir = Class(TFrame)
    TopPanel: TPanel;
    LeftPanel: TPanel;
    BottomPanel: TPanel;
    RightPanel: TPanel;
    TitlePanel: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel4: TPanel;
    AnimationPanel: TPanel;
    Label1: TLabel;
    AnimTrackBar: TTrackBar;
    SpeedTrackBar: TTrackBar;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    ActionTimer: TTimer;
    AnimationTimer: TTimer;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ActionList1: TActionList;
    acOpen: TAction;
    acSave: TAction;
    acInfo: TAction;
    ImageList1: TImageList;
    acRotate: TAction;
    acMove: TAction;
    acMoveUp: TAction;
    acMoveDown: TAction;
    acMoveLeft: TAction;
    acMoveRight: TAction;
    acRotateLeft: TAction;
    acRotateRight: TAction;
    acDefault: TAction;
    acZoomIn: TAction;
    acZoomOut: TAction;
    acOptions: TAction;
    acPlay: TAction;
    acStop: TAction;
    acFastForward: TAction;
    acRewind: TAction;
    acRepeat: TAction;
    acNoRepeat: TAction;
    acShowAllAnim: TAction;
    ImagePanelContainer: TPanel;
    DiapasonesPanel: TPanel;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    UpBtn: TSpeedButton;
    SpeedButton7: TSpeedButton;
    DownBtn: TSpeedButton;
    LeftBtn: TSpeedButton;
    RightBtn: TSpeedButton;
    RotateLeftBtn: TSpeedButton;
    RotateRightBtn: TSpeedButton;
    Bevel3: TBevel;
    ZoomInBtn: TSpeedButton;
    ZoomOutBtn: TSpeedButton;
    StyleCombo: TComboBox;
    BitBtn1: TBitBtn;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    ImagePanel: TGLVisirPanel;
    Procedure RightBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure RightBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure LeftBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure UpBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure DownBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure ZoomInBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure ZoomOutBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure ActionTimerTimer(Sender: TObject);
    Procedure StyleComboChange(Sender: TObject);
    Procedure AnimationTimerTimer(Sender: TObject);
    Procedure AnimTrackBarChange(Sender: TObject);
    Procedure SpeedTrackBarChange(Sender: TObject);
    Procedure ImagePanelContainerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure ImagePanelContainerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure ImagePanelContainerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure FrameResize(Sender: TObject);
    Procedure btnLeft2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure btnRight2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure acInfoExecute(Sender: TObject);
    procedure acRotateExecute(Sender: TObject);
    procedure acMoveExecute(Sender: TObject);
    procedure acDefaultExecute(Sender: TObject);
    procedure acOptionsExecute(Sender: TObject);
    procedure acPlayExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure acRewindExecute(Sender: TObject);
    procedure acFastForwardExecute(Sender: TObject);
    procedure acRepeatExecute(Sender: TObject);
    procedure acNoRepeatExecute(Sender: TObject);
    procedure acShowAllAnimExecute(Sender: TObject);
    procedure acMoveUpExecute(Sender: TObject);
    procedure acMoveDownExecute(Sender: TObject);
    procedure acMoveLeftExecute(Sender: TObject);
    procedure acMoveRightExecute(Sender: TObject);
    procedure acRotateLeftExecute(Sender: TObject);
    procedure acRotateRightExecute(Sender: TObject);
    procedure acZoomInExecute(Sender: TObject);
    procedure acZoomOutExecute(Sender: TObject);
    procedure LeftBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DownBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ZoomInBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ZoomOutBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RotateLeftBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RotateLeftBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RotateRightBtnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RotateRightBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  Protected
    bInitialized             : Boolean;
    bInitializing            : Boolean;
    RotateMode               : Boolean; // Rotate / move mode
    MouseBtn1, MouseBtn2     : Boolean; // is mouse buttons pressed?
    MouseStartX, MouseStartY : Integer; // coords of last click
    FShowBoundPlanes         : Boolean;
    FShowDiapasones          : Boolean;
    FShowTicks               : Boolean;
    FAllowMouse              : Boolean;
    FDestroying              : Boolean;
    FDummyCounter            : Integer;
    FLastAbsX                : Integer;
    FLastAbsY                : Integer;
    MPT                      : TMakePanelThread;
    Function  IsAllInitialized(Sender: TObject): Boolean;
    Function  MPTDoneIs2(Sender: TObject): Boolean;
    Procedure DoInitFromForm(Sender: TObject);
    Procedure DoSceneUpdateArea(Sender: TObject);
    Function  GetBackCol: TColor;
    Function  GetPlaneCol: TColor;
    Function  GetAxisNamesCol: TColor;
    Function  GetDrawStyle: TObjectDrawStyle;
    Function  GetCaption: String;
    Function  GetAxisXName: String;
    Function  GetAxisYName: String;
    Function  GetAxisZName: String;
    Procedure SetBackCol(Value: TColor);
    Procedure SetPlaneCol(Value: TColor);
    Procedure SetAxisNamesCol(Value: TColor);
    Procedure SetDrawStyle(Value: TObjectDrawStyle);
    Procedure SetCaption(Value: String);
    Procedure SetAxisXName(Value: String);
    Procedure SetAxisYName(Value: String);
    Procedure SetAxisZName(Value: String);
//    Procedure AddSphereToNewFrame;
    Procedure Redraw;
    Procedure SetAnimationPanel;
    Procedure SetControlPanel;
    Function  GetShowCaption: Boolean;
    Function  GetShowAnimation: Boolean;
    Function  GetShowMovement: Boolean;
    Function  GetShowDiapasones: Boolean;
    Function  GetShowBoundPlanes: Boolean;
    Function  GetShowTicks: Boolean;
    Function  GetAllowMouse: Boolean;
    Procedure SetShowCaption(B: Boolean);
    Procedure SetShowAnimation(B: Boolean);
    Procedure SetShowMovement(B: Boolean);
    Procedure SetShowDiapasones(B: Boolean);
    Procedure SetShowBoundPlanes(B: Boolean);
    Procedure SetShowTicks(B: Boolean);
    Procedure SetAllowMouse(B: Boolean);
    Procedure WndProc(Var Message: TMessage); Override;
    Procedure DummyProc;
    Procedure CreateDummyWindow;
  Public
    //--- main objects ---
    Scene3D      : TScene3D; // Scene
    MouseKMove   : Double;   // Multipliers of rotation/... from mouse
    MouseKRotate : Double;
    MouseKZoom   : Double;
    MPTDone      : Integer;

    { Public declarations }
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
    Procedure   Init(CreateDummy: Boolean);
    Procedure   Fit(bCoordsSymmetry,bTrueAspectRatio,bScale: Boolean);
    Procedure   FitFrame(bCoordsSymmetry,bTrueAspectRatio,bScale: Boolean; nFrame: Integer; DoRedraw: Boolean);
    Procedure   Update; Override;
    Procedure   Clear;
    Procedure   AddModelToNewFrame(Mdl: TModelRec; Mirror,CalcVertexNormals: Boolean);
    Procedure   AddModelToFrame(Mdl: TModelRec; nFrame: Integer; Mirror,CalcVertexNormals: Boolean);
    Procedure   SetDiapasones(St: String);
    Procedure   TriggerMouseMove(Shift: TShiftState; X, Y: Integer);
    Procedure   TriggerMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure   TriggerMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  Published
    Property BackgroundColor : TColor           Read GetBackCol         Write SetBackCol;
    Property PlaneColor      : TColor           Read GetPlaneCol        Write SetPlaneCol;
    Property AxisNamesColor  : TColor           Read GetAxisNamesCol    Write SetAxisNamesCol;
    Property Style           : TObjectDrawStyle Read GetDrawStyle       Write SetDrawStyle       Default Filled;
    Property Caption         : String           Read GetCaption         Write SetCaption;
    Property AxisXName       : String           Read GetAxisXName       Write SetAxisXName;
    Property AxisYName       : String           Read GetAxisYName       Write SetAxisYName;
    Property AxisZName       : String           Read GetAxisZName       Write SetAxisZName;
    Property ShowCaption     : Boolean          Read GetShowCaption     Write SetShowCaption     Default True;
    Property ShowAnimation   : Boolean          Read GetShowAnimation   Write SetShowAnimation   Default True;
    Property ShowMovement    : Boolean          Read GetShowMovement    Write SetShowMovement    Default True;
    Property ShowDiapasones  : Boolean          Read GetShowDiapasones  Write SetShowDiapasones  Default True;
    Property ShowBoundPlanes : Boolean          Read GetShowBoundPlanes Write SetShowBoundPlanes Default True;
    Property ShowTicks       : Boolean          Read GetShowTicks       Write SetShowTicks       Default True;
    Property AllowMouse      : Boolean          Read GetAllowMouse      Write SetAllowMouse      Default True;
    Property Initialized     : Boolean          Read bInitialized;
  End;

  TMakePanelThread = Class(TThread)
    GLVisir : TGLVisir;
    Procedure Execute; Override;
  End;

  TScene3D = Class
  Private
    FWindowHandle    : THandle;
    FLeft            : Integer;
    FTop             : Integer;
    FWidth           : Integer;
    FHeight          : Integer;
    FOwner           : TComponent;
//    ImagePanel       : TPanel; // panel to display scene on it
//    DiapasonesPanel  : TPanel; // panel to display coordinate ranges
    FOnNewDiapasones : TNewDiapasones;
    LPX,LPY,LPZ      : Single;
    LSX,LSY,LSZ      : Single;

    // Add new tick to axis
    Procedure AddTick(Var Model: TModel;
                      x,y,z: Double;
                      dx,dy,dz: Double;
                      a1,a2,a3: Double;
                      labelDx: Double;
                      S: String);
    // add new frame with boundary planes
    Procedure AddBoundPlanesToNewFrame(bCoordsCenterSymmetry: Boolean);

    // display diapasone Values
    Procedure DrawDiapasones;
  Public
    XMin, XMax       : Double; // coordinate ranges of objects in scene
    YMin, YMax       : Double;
    ZMin, ZMax       : Double;
    //--- main objects ---
//    View         : TGLVisir;
    Scene        : TSceneGL; // scene
//    Mouse        : T3DMouse; // change object by mouse
    ModifyAction : TModifyAction; // current action (Rotate right,left,...)
//    SampleText   : T3DText; // font sample for ticks on axis
    bInitialized : Boolean; // was graphic initialized?
    CurFrame     : Integer; // current frame (in Scene list)
    LastFrame    : Integer; // No of last frame (in Scene list)
    FirstPlane   : Integer; // No of 1st element of boundary planes (in Scene list)
    LastPlane    : Integer; // No of last element of boundary planes (in Scene list)
    FirstTick    : Integer; // No of 1st tick element of boundary planes (in Scene list)
    LastTick     : Integer; // No of last tick element of boundary planes (in Scene list)

    AxisX        : T3DText;
    AxisY        : T3DText;
    AxisZ        : T3DText;

    // user defined parameters
    bCoordsCenterSymmetry,bTrueAspectRatio:Boolean; // center and zoom all frames?
    bCycledAnimation   : Boolean; // loop animation?
    bShowAllFrames     : Boolean; // show all animation frames simultaneously?
    bShowBoundPlanes   : Boolean; // show boundary planes?
    bShowTicks         : Boolean; // show ticks on boundary planes?
//    bShowDiapasones    : Boolean; // show Value ranges
    BackgroundColor    : TColor;  // Background color
    PlaneColor         : TColor;  // color of boundary planes
    FontColor          : TColor;  // color of ticks on axis
    FontScale          : Double;  // font scale of ticks Values on axis
    AxisXTickCount     : Integer; // number of ticks on X axis
    AxisYTickCount     : Integer; // number of ticks on Y axis
    AxisZTickCount     : Integer; // number of ticks on Z axis
    AxisXName          : String;  // X axis name
    AxisYName          : String;  // Y axis name
    AxisZName          : String;  // Z axis name
    AxisNamesColor     : TColor; // axis names color
    ModelStyle         : TWireFrame; // draw style (wired/filled/combined)
    DeltaRotate        : Double;
    DeltaZoom          : Double;
    DeltaMove          : Double;

    FitCX              : Double;
    FitCY              : Double;
    FitCZ              : Double;
    FitKX              : Double;
    FitKY              : Double;
    FitKZ              : Double;

    { Public declarations }
//    Constructor Create(AOwner: TComponent; iImagePanel,iDiapasonesPanel: TPanel);
    Constructor Create(AOwner: TComponent; WindowHandle: THandle; AWidth,AHeight: Integer);
    Destructor  Destroy; Override;
    Procedure   Init;
    Procedure   Update(ALeft,ATop,AWidth,AHeight: Integer);
    Function    Redraw: Boolean;
    Procedure   ShowBoundPlanes(Flag: Boolean);  // show boundary planes?
//    Procedure   ShowDiapasones(Flag: Boolean);   // show boundary planes?
    Procedure   ShowTicks(Flag: Boolean);        // show ticks on boundary planes?
    Procedure   ShowAllFrames(Flag: Boolean);    // show all animation frames simultaneously?
    Procedure   SetStyle(Style: TWireFrame);        // set style of drawing (wired/filled/combined)
//    Procedure   SetModelColor(Col: TColor);      // set color of models in frames
    Procedure   SetAxisNamesColor(Col: TColor);  // set axis names color
    Procedure   SetBackgroundColor(Col: TColor); // set Background color
    Procedure   SetPlaneColor(Col: TColor);      // set plane color
    Procedure   SetFrameByNum(iFrame: Integer);  // set i-th frame as active
    Procedure   SetPrevFrame;                    // make previous frame active
    Procedure   SetNextFrame;                    // make next frame active
    Procedure   ProcessAction(Action: TModifyAction; Delta: double); // change scene - Rotate, ...
    Procedure   ProcessModifyAction;             // process actions
    Procedure   DefaultPosition(DoRedraw: Boolean);                 // restore default position
    Procedure   Fit(bCoordsCenterSymmetry1,bTrueAspectRatio1,bScale: Boolean); // center and zoom frames
    Procedure   FitFrame(bCoordsCenterSymmetry1,bTrueAspectRatio1,bScale: Boolean; nFrame: Integer);
    Procedure   ClearScene;                      // remove all frames from scene
    Procedure   SaveFrameToBMP(FileName: String);
    Procedure   AddModelToModel(Mdl: TModelRec; Model: TModel; Mirror,CalcVertexNormals: Boolean); // add model to frame
    Procedure   AddModelToNewFrame(Mdl: TModelRec; Mirror,CalcVertexNormals: Boolean);
    Procedure   AddModelToFrame(Mdl: TModelRec; nFrame: Integer; Mirror,CalcVertexNormals: Boolean);
    Procedure   SetEntityVisible(B: Boolean);
    Procedure   CalculateClickRay(X,Y: Integer; Var DestX,DestY,DestZ: Single);
    Function    GetNewModel(Index: Integer): TModel; Overload;
    Function    GetNewModel: TModel; Overload;
    Property    OnNewDiapasones : TNewDiapasones Read FOnNewDiapasones Write FOnNewDiapasones;
    Property    Width  : Integer Read FWidth  Write FWidth;
    Property    Height : Integer Read FHeight Write FHeight;
  End;

Var CreateDummyFinished: Boolean;

Procedure Register;
Function  AllInitialized: Boolean;
//Procedure FullScreenTriggerMouseMove(Shift: TShiftState; X, Y: Integer);
//Procedure FullScreenTriggerMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//Procedure FullScreenTriggerMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//Procedure FullScreenTriggerPostMessage(Message, WParam: Longint; LParam: Longint);

Implementation

Uses SyncObjs, Options3D;

Var
  WHandle              : THandle;
  MPM                  : TCriticalSection;
  InitializingGLVisirs : TThreadSafeList;

{$R *.DFM}

Function AllInitialized: Boolean;
Var
  I,J : Integer;
  B   : Boolean;

Begin
  J := InitializingGLVisirs.Count;
  I := 0;
  B := True;
  While (I < J) And B Do
  Begin
    B := B And TGLVisir(InitializingGLVisirs.Items[I]).Initialized;
    Inc(I);
  End; // While
  Result := B;
End; // AllInitialized
{
Procedure FullScreenTriggerMouseMove(Shift: TShiftState; X, Y: Integer);
Var GLV: TGLVisir;
Begin
  If InitializingGLVisirs.Count > 0 Then
  Begin
    GLV := TGLVisir(InitializingGLVisirs.Items[0]);
    GLV.TriggerMouseMove(Shift,X,Y);
  End;
End; // FullScreenTriggerMouseMove

Procedure FullScreenTriggerMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var GLV: TGLVisir;
Begin
  If InitializingGLVisirs.Count > 0 Then
  Begin
    GLV := TGLVisir(InitializingGLVisirs.Items[0]);
    GLV.TriggerMouseDown(Button,Shift,X,Y);
  End;
End; // FullScreenTriggerMouseDown

Procedure FullScreenTriggerMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var GLV: TGLVisir;
Begin
  If InitializingGLVisirs.Count > 0 Then
  Begin
    GLV := TGLVisir(InitializingGLVisirs.Items[0]);
    GLV.TriggerMouseUp(Button,Shift,X,Y);
  End;
End; // FullScreenTriggerMouseUp

Procedure FullScreenTriggerPostMessage(Message, WParam: Longint; LParam: Longint);
Var GLV: TGLVisir;
Begin
  If InitializingGLVisirs.Count > 0 Then
  Begin
    GLV := TGLVisir(InitializingGLVisirs.Items[0]);
    If GLV.Initialized Then
    Begin
      PostMessage(GLV.Handle,Message,WParam,LParam);
    End;
  End;
End; // FullScreenTriggerPostMessage
}
Procedure RepaintAll;
Var I,J: Integer;
Begin
  If AllInitialized Then
  Begin
    J := InitializingGLVisirs.Count;
    I := 0;
    While I < J Do
    Begin
      TGLVisir(InitializingGLVisirs.Items[I]).Repaint;
      Inc(I);
    End; // While
  End;
End; // RepaintAll

function __WindowProc(HWindow: HWnd; Message, WParam: Longint; LParam: Longint): Longint; StdCall;
//Var Msg: TMessage;
{
Var
  Panel : TPanel;
  Scene : TSceneGL;
}
Begin

//  LogToFile('__WindowProc(' + IntToStr(HWindow) + '): $' + IntToHex(Message,4));
{
  Case Message Of
    WM_CREATE:
    Begin
      LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_CREATE');
      Scene := TSceneGL(SceneHash.Get(GetParent(HWindow)));
      If Scene <> Nil Then
      Begin
        Scene.InitDC;
        Scene.InitRC;
        Scene.SetActive(True);
        Scene.Redraw;
      End
      Else LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_CREATE...ERROR: Scene is Nil!');
    End;
    WM_DESTROY:
    Begin
      LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_DESTROY');
      Scene := TSceneGL(SceneHash.Get(GetParent(HWindow)));
      If Scene <> Nil Then
      Begin
        wglMakeCurrent(0, 0);
        wglDeleteContext(Scene.HRC);
        ReleaseDC(Scene.WindowHandle, Scene.DC);
      End
      Else LogToFile('WindowProc(' + IntToStr(HWindow) + '): WM_DESTROY...ERROR: Scene is Nil!');

//      Scene.WindowHandle := 0;
      WindowHash.Put(HWindow,Nil);
      SceneHash.Put(HWindow,Nil);
    End;
    WM_CLOSE:
    Begin
      PostQuitMessage(0);
      Result := 0;
    End;
  Else
//    Result := DefWindowProc(HWindow, Message, WParam, LParam);
  End; // Case

//  LogToFile('WindowProc: $' + IntToHex(Message,4));

  If (Message >= WM_MOUSEFIRST) And (Message <= WM_MOUSELAST) Then
  Begin

//    LogToFile('WindowProc: $' + IntToHex(Message,4));

//    Msg.Msg    := Message;
//    Msg.WParam := WParam;
//    Msg.LParam := LParam;

//    PostThreadMessage(MainThreadID,Message,WParam,LParam);

    Panel := TPanel(WindowHash.Get(HWindow));
    If Panel <> Nil Then PostMessage(Panel.Handle,Message,WParam,LParam);
//    If Panel <> Nil Then Panel.Dispatch(Msg);
  End;
//  Result := 0;

//  If (Message = WM_DESTROY) Or (Message = WM_NCDESTROY) Then Result := 0 Else
}
  Result := DefWindowProc(HWindow, Message, WParam, LParam);
End; // __WindowProc

// ---------------------------
// TGLVisir
// ---------------------------

Function TGLVisir.IsAllInitialized(Sender: TObject): Boolean;
Begin
  Result := AllInitialized;
End; // TGLVisir.IsAllInitialized

Function TGLVisir.MPTDoneIs2(Sender: TObject): Boolean;
Begin
  Result := (MPTDone = 2);
End; // TGLVisir.MPTDoneIs2

Procedure TGLVisir.DoInitFromForm(Sender: TObject);
Begin
  Init(True);
End; // TGLVisir.DoInitFromForm

Procedure TGLVisir.DoSceneUpdateArea(Sender: TObject);
Begin
  Scene3D.Scene.UpdateArea(0,0,ImagePanel.Width,ImagePanel.Height);
End; // TGLVisir.DoSceneUpdateArea

//------------------------------------------------------------------------------
//         Create and set default parameters Values
//------------------------------------------------------------------------------
Constructor TGLVisir.Create(AOwner: TComponent);
Var TheOwner: TComponent;
Begin
  LogToFile('TGLVisir.Create(): Begin');
  Inherited Create(AOwner);

  TheOwner := GetUltimateOwner(Self);
  If TheOwner Is TGLForm Then
  Begin
    TGLForm(TheOwner).OnAllInitialized := IsAllInitialized;
    TGLForm(TheOwner).OnMPTDone        := MPTDoneIs2;
  End;
  If Not (csDesigning In ComponentState) Then ImagePanel.Caption := 'Initializing...';


//  CreateDummyWindow;


  FDestroying          := False;
  FShowBoundPlanes     := True;
  FShowDiapasones      := True;
  FShowTicks           := True;
  FAllowMouse          := True;
  bInitialized         := False;
  bInitializing        := False;
  Scene3D              := TScene3D.Create(Self,ImagePanel.Handle,ImagePanel.Width,ImagePanel.Height);//(AOwner, ImagePanel, DiapasonesPanel);
  Scene3D.OnNewDiapasones := SetDiapasones;
//  Scene3D.View         := Self;
  StyleCombo.ItemIndex := 1;
  RotateMode           := True; // Rotate mode
  MouseBtn1            := False; // mouse buttons not pressed
  MouseBtn2            := False;
  MouseKMove           := 0.02;
  MouseKRotate         := 1;
  MouseKZoom           := 0.02;
  FDummyCounter        := 0;
  FLastAbsX            := -1;
  FLastAbsY            := -1;


  MPT := Nil;
  MPTDone := 0;


  ImagePanel.OnUpdateArea     := DoSceneUpdateArea;
  ImagePanel.OnInit           := DoInitFromForm;
  ImagePanel.OnAllInitialized := IsAllInitialized;
//  ImagePanel.Scene := Scene3D.Scene;


  If Scene3D.Scene <> Nil Then Scene3D.Scene.Name := Name;

//  ImagePanel.HandleNeeded;



//  Scene3D.Scene.MakeImagePanel(@ImagePanel);
//  Scene3D.FWindowHandle := Scene3D.Scene.WindowHandle;

  ActionTimer.Enabled := True;

//  Init;

//  Scene3D.FWindowHandle := ImagePanel.Handle;
//  Scene3D.Init;

  LogToFile('TGLVisir.Create(): End');
End; // TGLVisir.Create

Destructor TGLVisir.Destroy;
Var I: Integer;
Begin
  I := InitializingGLVisirs.GetIndexOf(Self);
  If I >= 0 Then InitializingGLVisirs.Delete(I);

  If MPTDone > 0 Then
  Begin
    While MPTDone < 2 Do Sleep(100);
  End;

  FDestroying := True;
  Scene3D.Free;
  Scene3D := Nil;
  Inherited;
End; // TGLVisir.Destroy

Procedure TGLVisir.CreateDummyWindow;
Var
  DC           : HDC;
//  HRC          : HGLRC;
  ActiveWindow : THandle;

  Function DoMakeImagePanel: THandle;
  Var
    WinClass     : TWndClassEx;
    dwStyle      : Cardinal;
    WindowHandle : THandle;

  Begin
    LogToFile('TGLVisir.CreateDummyWindow.DoMakeImagePanel(' + Name + ') Begin');



    WinClass.lpszClassName := 'GLVISIRCLASS';
    WinClass.cbSize        := SizeOf(WinClass);
    WinClass.style         := CS_HREDRAW Or CS_VREDRAW Or CS_OWNDC;
    WinClass.lpfnWndProc   := @__WindowProc;
    WinClass.hInstance     := hInstance;
    WinClass.hIcon         := 0;
    WinClass.hIconSm       := 0;
    WinClass.hCursor       := LoadCursor(0, IDC_ARROW);
    WinClass.hbrBackground := 0;
    WinClass.lpszMenuName  := Nil;
    WinClass.cbClsExtra    := 0;
    WinClass.cbWndExtra    := 0;

    LogToFile('TGLVisir.CreateDummyWindow.DoMakeImagePanel(' + Name + '): Registering window class');

    Result := 0;
    If RegisterClassEx(WinClass) = 0 Then LogToFile('TGLVisir.CreateDummyWindow.DoMakeImagePanel(' + Name + '): unable to register window class')
    Else
    Begin
      dwStyle := WS_CLIPCHILDREN Or WS_CLIPSIBLINGS Or WS_POPUP Or WS_VISIBLE;

      LogToFile('TGLVisir.CreateDummyWindow.DoMakeImagePanel(' + Name + '): Creating window');

      WindowHandle := CreateWindowEx(0, 'GLVISIRCLASS', Nil, dwStyle, 0, 0, 640, 480, 0, 0, hInstance, Nil);
      If WindowHandle = 0 Then
      Begin
        LogToFile('TGLVisir.CreateDummyWindow.DoMakeImagePanel(' + Name + '): unable to create OpenGL window');
      End
      Else
      Begin
        Result := WindowHandle;
        LogToFile('TGLVisir.CreateDummyWindow.DoMakeImagePanel(' + Name + '): Window has been created');
      End;
    End;
    LogToFile('TGLVisir.CreateDummyWindow.DoMakeImagePanel(' + Name + ') End');
  End; // DoMakeImagePanel

  Procedure SetDCPixelFormat(DC: HDC; HWindow: THandle);
  Var
    nPixelFormat : Integer;
    PFD          : TPixelFormatDescriptor;
    Code         : Integer;

  Begin
//    LogToFile('TGLVisir.CreateDummyWindow.SetDCPixelFormat(' + Name + '): Begin, WindowHandle = ' + IntToStr(HWindow));
    FillChar(PFD, SizeOf(PFD),0);
    With PFD Do
    Begin
      nSize      := SizeOf(PFD);                               // Size of this structure
      nVersion   := 1;                                         // Version number
      dwFlags    := PFD_DRAW_TO_WINDOW Or
                    PFD_SUPPORT_OPENGL Or
                    PFD_DOUBLEBUFFER;                          // Flags
      iPixelType := PFD_TYPE_RGBA;                             // RGBA pixel values
      cColorBits := 24;                                        // 24-bit color
      cDepthBits := 32;                                        // 32-bit depth buffer


      iLayerType := PFD_MAIN_PLANE;                            // Layer type
    End;
//    LogToFile('TGLVisir.CreateDummyWindow.SetDCPixelFormat(' + Name + '): Calling ChoosePixelFormat()');
    nPixelFormat := ChoosePixelFormat(DC, @PFD);
//    If nPixelFormat = 0
//     Then LogToFile('TGLVisir.CreateDummyWindow.SetDCPixelFormat(' + Name + '): error in ChoosePixelFormat, code = ' + IntToStr(GetLastError))
//     Else LogToFile('TGLVisir.CreateDummyWindow.SetDCPixelFormat(' + Name + '): nPixelFormat = ' + IntToStr(nPixelFormat));
    If Not SetPixelFormat(DC, nPixelFormat, @PFD) Then
    Begin
      Code := GetLastError;
//      LogToFile('TGLVisir.CreateDummyWindow.SetDCPixelFormat(' + Name + '): error in SetPixelFormat(DC=' + IntToStr(DC) + '), code = ' + IntToStr(Code));
    End;
    DescribePixelFormat(DC, nPixelFormat, SizeOf(TPixelFormatDescriptor), PFD);
//    If Not DescribePixelFormat(DC, nPixelFormat, SizeOf(TPixelFormatDescriptor), PFD) Then
//     LogToFile('TGLVisir.CreateDummyWindow.SetDCPixelFormat(' + Name + '): error in DescribePixelFormat, code = ' + IntToStr(GetLastError));
//    LogToFile('TGLVisir.CreateDummyWindow.SetDCPixelFormat(' + Name + '): End');
  End; // SetDCPixelFormat

Begin
  If (WHandle = 0) And Not (csDesigning In ComponentState) Then
  Begin
    ActiveWindow := GetActiveWindow;

//    LogToFile('TGLVisir.CreateDummyWindow(): ActiveWindow = ' + IntToStr(ActiveWindow));

    WHandle := DoMakeImagePanel;
    If WHandle <> 0 Then
    Begin
//      LogToFile('TGLVisir.CreateDummyWindow(): WHandle = ' + IntToStr(WHandle));

      ShowWindow(WHandle, SW_SHOW);

      Application.ProcessMessages;

      SetWindowPos(WHandle, HWND_TOPMOST, 0, 0, 100, 100, SWP_NOMOVE);
      SetWindowPos(WHandle, HWND_TOPMOST, 0, 0, 100, 100, SWP_NOSIZE);

//      If Not SetWindowPos(WHandle, HWND_TOPMOST, 0, 0, 100, 100, SWP_NOMOVE) Then
//       LogToFile('TGLVisir.CreateDummyWindow(): SetWindowPos(' + IntToStr(WHandle) + '): WM_SIZE: Error = $' + IntToHex(GetLastError,8));

//      If Not SetWindowPos(WHandle, HWND_TOPMOST, 0, 0, 100, 100, SWP_NOSIZE) Then
//       LogToFile('TGLVisir.CreateDummyWindow(): SetWindowPos(' + IntToStr(WHandle) + '): WM_MOVE: Error = $' + IntToHex(GetLastError,8));

      DC := GetDC(WHandle);
      If DC <> 0 Then
      Begin
        SetDCPixelFormat(DC,WHandle);
{
        HRC := wglCreateContext(DC);
        wglMakeCurrent(DC, HRC);

        Application.ProcessMessages;

        Sleep(5000);

        If HRC <> 0 Then
        Begin
          wglMakeCurrent(0, 0);
          wglDeleteContext(HRC);
          LogToFile('TGLVisir.Create(' + Name + '): HRC = ' + IntToStr(HRC));
        End;
}
        ReleaseDC(WHandle, DC);
      End;

      LogToFile('TGLVisir.CreateDummyWindow(): Cleaning up');

      ShowWindow(WHandle, SW_HIDE);
      DestroyWindow(WHandle);
      Windows.UnregisterClass('GLVISIRCLASS',hInstance);

      SetActiveWindow(ActiveWindow);

      Application.ProcessMessages;

      CreateDummyFinished := True;

      LogToFile('TGLVisir.CreateDummyWindow(): Finished');
    End;
  End;
End; // CreateDummyWindow

//------------------------------------------------------------------------------
//                   Graph init
//------------------------------------------------------------------------------
Procedure TGLVisir.Init(CreateDummy: Boolean);
Var H: THandle;
Begin
{
  LogToFile('TGLVisir.Init(' + Name + '): Begin: CreateDummy = ' + BoolToStr(CreateDummy,True) +
            ' CreateDummyFinished = ' + BoolToStr(CreateDummyFinished,True) +
            ' WHandle = ' + IntToStr(WHandle));
}
  If (Not (bInitialized Or bInitializing)) And Not (csDesigning In ComponentState) Then
  Begin
    bInitializing := True;
    InitializingGLVisirs.Add(Self);
    If CreateDummy And Not CreateDummyFinished Then CreateDummyWindow;
    If WHandle <> 0 Then
    Begin
      LogToFile('TGLVisir.Init(' + Name + '): Begin: Starting render thread');

      Scene3D.Scene.StartRenderThread;
      Scene3D.Scene.Name := Name;
      H := ImagePanel.Handle; // Causes a TScene3D.Redraw...we have to be careful what we allow on a redraw




{
      Scene3D.Scene.MakeImagePanel(@ImagePanel);
      Scene3D.FWindowHandle := Scene3D.Scene.WindowHandle;
      Scene3D.Init;
      bInitialized := True;
}

      SetAnimationPanel;
      SetControlPanel;
      If csDesigning In ComponentState Then
      Begin
        AnimationTimer.Enabled := False;
        ActionTimer.Enabled    := False;
//        AddSphereToNewFrame;
        Fit(True,True,True);
      End
      Else
      Begin
    //    Application.CreateForm(TOptions3DForm, Options3DForm);
    //    Application.CreateForm(TAboutBox, AboutBox);
      End;


      If MPT = Nil Then
      Begin
        MPTDone      := 1;
        MPT          := TMakePanelThread.Create(True);
        MPT.Priority := tpHigher;
//        LogToFile('Created MakePanelThread with thread ID $' + IntToHex(MPT.ThreadID,8) ,true);
        MPT.FreeOnTerminate := True;
        MPT.GLVisir := Self;
        MPT.Resume;
      End;

    End;
  End;
  LogToFile('TGLVisir.Init(' + Name + '): End');
End;

//------------------------------------------------------------------------------
//                Center, zoom, ...
//------------------------------------------------------------------------------
Procedure TGLVisir.Fit(bCoordsSymmetry,bTrueAspectRatio,bScale: Boolean);
Begin
  Scene3D.Fit(bCoordsSymmetry,bTrueAspectRatio,bScale);
  SetAnimationPanel;
  SetControlPanel;
  Update;
  Scene3D.DefaultPosition(True);
End; // TGLVisir.Fit

Procedure TGLVisir.FitFrame(bCoordsSymmetry,bTrueAspectRatio,bScale: Boolean; nFrame: Integer; DoRedraw: Boolean);
Begin
  Scene3D.FitFrame(bCoordsSymmetry,bTrueAspectRatio,bScale,nFrame);
  SetAnimationPanel;
  SetControlPanel;
  If DoRedraw Then Update;
  Scene3D.DefaultPosition(DoRedraw);
End; // TGLVisir.FitFrame

//------------------------------------------------------------------------------
//                        Redraw scene
//------------------------------------------------------------------------------

Procedure TGLVisir.Redraw;
Begin
  If (Not (csDesigning In ComponentState)) And AllInitialized And (Scene3D <> Nil) And Not FDestroying Then Scene3D.Redraw;
End; // TGLVisir.Redraw

//PROFILE-NO
Procedure TGLVisir.WndProc(Var Message: TMessage);
//Var X,Y: Integer;
Begin
//  LogToFile('TGLVisir.WndProc(' + Name + '): (' + IntToStr(GetTickCount) + ')  Message = $' + IntToHex(Message.Msg,4) + ', WParam = ' + IntToStr(Message.WParam) + ', LParam = ' + IntToStr(Message.LParam));


  If (Message.Msg = WM_ACTIVATE)    Or
     (Message.Msg = WM_PAINT)       Or
     (Message.Msg = WM_NCPAINT)     Or
     (Message.Msg = WM_SHOWWINDOW) Then
  Begin
{
    If Message.Msg = WM_SHOWWINDOW Then
    Begin
      LogToFile('TGLVisir.WndProc(' + Name + '): WM_SHOWWINDOW   WParam = ' + IntToStr(Message.WParam) + ', LParam = ' + IntToStr(Message.LParam));
      Update;
    End;
}
{
    GetAbsolutePosition(Self,X,Y);
    If ((X <> FLastAbsX) Or (Y <> FLastAbsY)) And (Scene3D <> Nil) Then
    Begin
      FrameResize(Self);
      FLastAbsX := X;
      FLastAbsY := Y;
    End;
}

//    Update;

    Redraw;
  End;

//  LogToFile('Message: ' + IntToHex(Message.Msg,8));

  Inherited WndProc(Message);
End; // TGLVisir.WndProc
//PROFILE-YES

//------------------------------------------------------------------------------
//             Enable/Disable buttons
//------------------------------------------------------------------------------

Function TGLVisir.GetShowCaption: Boolean;
Begin
  Result := TopPanel.Visible;
End; // TGLVisir.GetShowCaption

Function TGLVisir.GetShowAnimation: Boolean;
Begin
  Result := BottomPanel.Visible;
End; // TGLVisir.GetShowAnimation

Function TGLVisir.GetShowMovement: Boolean;
Begin
  Result := RightPanel.Visible;
End; // TGLVisir.GetShowMovement

Function TGLVisir.GetShowDiapasones: Boolean;
Begin
  Result := FShowDiapasones;
End; // TGLVisir.GetShowDiapasones

Function TGLVisir.GetShowBoundPlanes: Boolean;
Begin
  Result := FShowBoundPlanes;
End; // TGLVisir.GetShowBoundPlanes

Function TGLVisir.GetShowTicks: Boolean;
Begin
  Result := FShowTicks;
End; // TGLVisir.GetShowTicks

Function TGLVisir.GetAllowMouse: Boolean;
Begin
  Result := FAllowMouse;
End; // TGLVisir.GetAllowMouse

Procedure TGLVisir.SetShowCaption(B: Boolean);
Begin
  TopPanel.Visible := B;
  LeftPanel.Visible := B And BottomPanel.Visible;
End; // TGLVisir.SetShowCaption

Procedure TGLVisir.SetShowAnimation(B: Boolean);
Begin
  BottomPanel.Visible := B;
  LeftPanel.Visible := B And TopPanel.Visible;
End; // TGLVisir.SetShowAnimation

Procedure TGLVisir.SetShowMovement(B: Boolean);
Begin
  RightPanel.Visible := B;
End; // TGLVisir.SetShowMovement

Procedure TGLVisir.SetShowDiapasones(B: Boolean);
Begin
  FShowDiapasones := B;
  DiapasonesPanel.Visible := FShowDiapasones;
//  Scene3D.ShowDiapasones(B);
  Redraw;
End; // TGLVisir.SetShowDiapasones

Procedure TGLVisir.SetShowBoundPlanes(B: Boolean);
Begin
  FShowBoundPlanes := B;
  Scene3D.ShowBoundPlanes(B);
  Redraw;
End; // TGLVisir.SetShowBoundPlanes

Procedure TGLVisir.SetShowTicks(B: Boolean);
Begin
  FShowTicks := B;
  Scene3D.ShowTicks(B);
  Redraw;
End; // TGLVisir.SetShowTicks

Procedure TGLVisir.SetAllowMouse(B: Boolean);
Begin
  FAllowMouse := B;
End; // TGLVisir.SetAllowMouse

Procedure TGLVisir.SetAnimationPanel;
Begin
  If Scene3D.LastFrame > 0 Then AnimTrackBar.Max := Scene3D.LastFrame;

  // if all frames shown

  If acShowAllAnim.Checked Or (Scene3D.LastFrame <= 0) Then
  Begin
    //--- disable ---

    acPlay.Enabled        := False;
    acStop.Enabled        := False;
    acRewind.Enabled      := False;
    acFastForward.Enabled := False;
    AnimTrackBar.Enabled  := False;
    acRepeat.Enabled      := False;
    acNoRepeat.Enabled    := False;
    SpeedTrackBar.Enabled := False;
    If Scene3D.LastFrame <= 0 Then acShowAllAnim.Enabled := False;
  End
  Else
  Begin
    //--- enable ---

    acPlay.Enabled        := True;
    acStop.Enabled        := True;
    acRewind.Enabled      := True;
    acFastForward.Enabled := True;
    AnimTrackBar.Enabled  := True;
    acRepeat.Enabled      := True;
    acNoRepeat.Enabled    := True;
    SpeedTrackBar.Enabled := True;
    acShowAllAnim.Enabled := True;

    acStop.Enabled := Not acPlay.Checked;
    AnimationTimer.Enabled := acPlay.Checked;
//    If Not acPlay.Checked Then acStop.Enabled := False;

    If Scene3D.LastFrame = Scene3D.CurFrame Then
     If Not Scene3D.bCycledAnimation Then
     Begin
       acFastForward.Enabled := False;
       acPlay.Enabled        := True;
       acStop.Enabled        := False;
       acPlay.Checked        := False;
     End;

    If Scene3D.CurFrame = 0 Then
     If Not Scene3D.bCycledAnimation Then acRewind.Enabled := False;

    AnimTrackBar.Position := Scene3D.CurFrame;
  End;
End; // TGLVisir.SetAnimationPanel

Procedure TGLVisir.SetControlPanel;
Var B: Boolean;
Begin
  B := (Scene3D.LastFrame >= 0);
  acMoveUp.Enabled      := B;
  acMoveDown.Enabled    := B;
  acMoveLeft.Enabled    := B;
  acMoveRight.Enabled   := B;
  acRotateLeft.Enabled  := B;
  acRotateRight.Enabled := B;
  acDefault.Enabled     := B;
  acZoomIn.Enabled      := B;
  acZoomOut.Enabled     := B;
  StyleCombo.Enabled    := B;
  acOptions.Enabled     := B;
  acRotate.Enabled      := B;
  acMove.Enabled        := B;
  acSave.Enabled        := B;
End; // TGLVisir.SetControlPanel

Procedure TGLVisir.DummyProc;
Begin
  FDummyCounter := (FDummyCounter + 1) And $FF;
End; // TGLVisir.DummyProc

//------------------------------------------------------------------------------
//              Overriding of update
//------------------------------------------------------------------------------
Procedure TGLVisir.Update;
Begin
  If Not FDestroying Then
  Begin
    If Not (csDesigning In ComponentState) Then
    Begin
      If Not bInitialized Then Init(False);
      If AllInitialized Then Scene3D.Update(FLastAbsX,FLastAbsY,ImagePanel.Width,ImagePanel.Height);
    End;
  End;
End; // TGLVisir.Update

//------------------------------------------------------------------------------
//        Remove all frames
//------------------------------------------------------------------------------
Procedure TGLVisir.Clear;
Begin
  Scene3D.ClearScene;
End; // TGLVisir.Clear

//------------------------------------------------------------------------------
//        Add specified model to new frame
//------------------------------------------------------------------------------
Procedure TGLVisir.AddModelToNewFrame(Mdl: TModelRec; Mirror,CalcVertexNormals: Boolean);
Begin
  Scene3D.AddModelToNewFrame(Mdl,Mirror,CalcVertexNormals);
End; // TGLVisir.AddModelToNewFrame

//------------------------------------------------------------------------------
//        Add model to specified frame
//------------------------------------------------------------------------------
Procedure TGLVisir.AddModelToFrame(Mdl: TModelRec; nFrame: Integer; Mirror,CalcVertexNormals: Boolean);
Begin
  Scene3D.AddModelToFrame(Mdl,nFrame,Mirror,CalcVertexNormals);
End; // TGLVisir.AddModelToFrame

//------------------------------------------------------------------------------
//         pressing buttons, changing scene view
//------------------------------------------------------------------------------
Procedure TGLVisir.RightBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If RotateMode
   Then Scene3D.ModifyAction := RotateRight
   Else Scene3D.ModifyAction := MoveRight;
End;

Procedure TGLVisir.RightBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  Scene3D.ModifyAction := None;
End;

Procedure TGLVisir.LeftBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If RotateMode
   Then Scene3D.ModifyAction := RotateLeft
   Else Scene3D.ModifyAction := MoveLeft;
End;

Procedure TGLVisir.UpBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If RotateMode
   Then Scene3D.ModifyAction := RotateUp
   Else Scene3D.ModifyAction := MoveUp;
End;

Procedure TGLVisir.DownBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
 If RotateMode
  Then Scene3D.ModifyAction := RotateDown
  Else Scene3D.ModifyAction := MoveDown;
End;

Procedure TGLVisir.ZoomInBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  Scene3D.ModifyAction := ZoomIn;
End;

Procedure TGLVisir.ZoomOutBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  Scene3D.ModifyAction := ZoomOut;
End;

//------------------------------------------------------------------------------
//     Call Rotate, zoom when according buttons pressed
//------------------------------------------------------------------------------

Procedure TGLVisir.ActionTimerTimer(Sender: TObject);
//Var
//  X,Y      : Integer;
//  Vis      : Boolean;
//  TheOwner : TComponent;
  
Begin
  If (Not FDestroying) And Not (csDesigning In ComponentState) Then
  Begin
{
    GetAbsolutePosition(Self,X,Y);

    TheOwner := GetUltimateOwner(ImagePanel);

    Vis := ImagePanel.Visible And TForm(TheOwner).Visible And (TForm(TheOwner).WindowState <> wsMinimized) And TForm(TheOwner).Showing;

//    LogToFile(IntToStr(GetTickCount) + ' ' + TForm(TheOwner).Name + ': ' + IntToStr(Integer(TForm(TheOwner).WindowState)));


    If (Scene3D <> Nil) And ((X <> FLastAbsX) Or (Y <> FLastAbsY) Or (Vis <> Scene3D.Scene.WindowIsVisible)) Then
    Begin
      If Vis <> Scene3D.Scene.WindowIsVisible Then LogToFile('TGLVisir.ActionTimerTimer(' + Name + '): Different visibility: new visibility is ' + BoolToStr(Vis,True));
      LogToFile('TGLVisir.ActionTimerTimer(' + Name + '): Different position (' + Name + '): (' + IntToStr(FLastAbsX) + ', ' + IntToStr(FLastAbsY) + ') ---> (' + IntToStr(X) + ', ' + IntToStr(Y) + ')');
      FLastAbsX := X;
      FLastAbsY := Y;
      If (Vis <> Scene3D.Scene.WindowIsVisible) And Vis Then
      Begin
        ImagePanel.HandleNeeded;
        GetAbsolutePosition(ImagePanel,X,Y);
      End;
      FrameResize(Self);
    End;
}

//    If Not bInitialized Then Init;
    If AllInitialized Then Scene3D.ProcessModifyAction;
  End;
End;

//------------------------------------------------------------------------------
//                Set draw style
//------------------------------------------------------------------------------
Procedure TGLVisir.StyleComboChange(Sender: TObject);
Begin
       If StyleCombo.ItemIndex = 0 Then Scene3D.SetStyle(wfLines)             // Wired
  Else If StyleCombo.ItemIndex = 1 Then Scene3D.SetStyle(wfPolygons)          // Filled
  Else If StyleCombo.ItemIndex = 2 Then Scene3D.SetStyle(wfLinesAndPolygons); // Combined
  Redraw;
End;

//------------------------------------------------------------------------------
//                  Animation: by timer - next frame
//------------------------------------------------------------------------------
Procedure TGLVisir.AnimationTimerTimer(Sender: TObject);
Begin
  If (Not FDestroying) And Not (csDesigning In ComponentState) Then
  Begin
    Try
      If AllInitialized And acPlay.Checked And acPlay.Enabled Then
      Begin
        Scene3D.SetNextFrame;
        SetAnimationPanel;
        Redraw;
      End;
    Except
    End;
  End;
End;

Procedure TGLVisir.AnimTrackBarChange(Sender: TObject);
Begin
  Scene3D.SetFrameByNum(AnimTrackBar.Position);
  SetAnimationPanel;
  Redraw;
End;

Procedure TGLVisir.SpeedTrackBarChange(Sender: TObject);
Begin
  Case SpeedTrackBar.Position Of
    0: AnimationTimer.Interval := 200;
    1: AnimationTimer.Interval := 500;
    2: AnimationTimer.Interval := 1000;
  End; // Case
End;

//------------------------------------------------------------------------------
//                Mouse move processing
//------------------------------------------------------------------------------
Procedure TGLVisir.ImagePanelContainerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Begin
  If FAllowMouse Then
  Begin
    If MouseBtn1 Then
    Begin
      // if left button pressed

      If RotateMode Then
      Begin
             If X > MouseStartX Then Scene3D.ProcessAction(RotateRight, (X - MouseStartX) * MouseKRotate)
        Else If X < MouseStartX Then Scene3D.ProcessAction(RotateLeft,  (MouseStartX - X) * MouseKRotate);
             If Y > MouseStartY Then Scene3D.ProcessAction(RotateDown,  (Y - MouseStartY) * MouseKRotate)
        Else If Y < MouseStartY Then Scene3D.ProcessAction(RotateUp,    (MouseStartY - Y) * MouseKRotate);
      End
      Else
      Begin
             If X > MouseStartX Then Scene3D.ProcessAction(MoveRight,(X - MouseStartX) * MouseKMove)
        Else If X < MouseStartX Then Scene3D.ProcessAction(MoveLeft, (MouseStartX - X) * MouseKMove);
             If Y > MouseStartY Then Scene3D.ProcessAction(MoveDown, (Y - MouseStartY) * MouseKMove)
        Else If Y < MouseStartY Then Scene3D.ProcessAction(MoveUp,   (MouseStartY - Y) * MouseKMove);
      End;
    End
    Else If MouseBtn2 Then  
    Begin
      // if right button pressed
//           if X > MouseStartX then Scene3D.ProcessAction(ZoomIn,  (X - MouseStartX) * MouseKZoom)
//      else if X < MouseStartX then Scene3D.ProcessAction(ZoomOut, (MouseStartX - X) * MouseKZoom);

           If X > MouseStartX Then Scene3D.ProcessAction(RotateRight2, (X - MouseStartX) * MouseKRotate)
      Else If X < MouseStartX Then Scene3D.ProcessAction(RotateLeft2,  (MouseStartX - X) * MouseKRotate);
           If Y > MouseStartY Then Scene3D.ProcessAction(ZoomIn,       (Y - MouseStartY) * MouseKZoom)
      Else If Y < MouseStartY Then Scene3D.ProcessAction(ZoomOut,      (MouseStartY - Y) * MouseKZoom);
    End;
    If MouseBtn1 Or MouseBtn2 Then Redraw;
  End;
  MouseStartX := X; // click coordinates
  MouseStartY := Y;
  If Assigned(OnMouseMove) Then OnMouseMove(Self,Shift,X,Y);
End;

//------------------------------------------------------------------------------
//                Click processing
//------------------------------------------------------------------------------
Procedure TGLVisir.ImagePanelContainerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  MouseStartX := X; // coordinates
  MouseStartY := Y;
  If Button = mbLeft Then
  Begin
    MouseBtn1 := True;
    MouseBtn2 := False;
  End
  Else
  Begin
    MouseBtn1 := False;
    MouseBtn2 := True;
  End;
  If Assigned(OnMouseDown) Then OnMouseDown(Self,Button,Shift,X,Y);
End;

Procedure TGLVisir.ImagePanelContainerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  MouseBtn1 := False;
  MouseBtn2 := False;
  If Assigned(OnMouseUp) Then OnMouseUp(Self,Button,Shift,X,Y);
End;

Procedure TGLVisir.TriggerMouseMove(Shift: TShiftState; X, Y: Integer);
Begin
  ImagePanelContainerMouseMove(Self,Shift,X,Y);
End; // TGLVisir.TriggerMouseMove

Procedure TGLVisir.TriggerMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  ImagePanelContainerMouseDown(Self,Button,Shift,X,Y);
End; // TGLVisir.TriggerMouseDown

Procedure TGLVisir.TriggerMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  ImagePanelContainerMouseUp(Self,Button,Shift,X,Y);
End; // TGLVisir.TriggerMouseUp

Procedure TGLVisir.FrameResize(Sender: TObject);
Begin
  If AllInitialized And (Not FDestroying) And Not (csDesigning In ComponentState) Then
  Begin
//    Scene3D.Scene.UpdateArea(ImagePanel.width,ImagePanel.height);
    Scene3D.Update(FLastAbsX,FLastAbsY,ImagePanel.Width,ImagePanel.Height);
//    Redraw;
  End;
End;

////////////////////////////////////////////////////////////////////////////////

Function TGLVisir.GetBackCol: TColor;
Begin
  Result := Scene3D.BackgroundColor;
End; // TGLVisir.GetBackCol

Procedure TGLVisir.SetBackCol(Value: TColor);
Begin
  Scene3D.SetBackgroundColor(Value);
//  ImagePanel.Color := Value;
  FrameResize(Nil);
End; // TGLVisir.SetBackCol

Function TGLVisir.GetPlaneCol: TColor;
Begin
  Result := Scene3D.PlaneColor;
End; // TGLVisir.GetPlaneCol

Procedure TGLVisir.SetPlaneCol(Value: TColor);
Begin
  Scene3D.SetPlaneColor(Value);
{
  If Scene3D.LastFrame >= 0 Then
   If (csDesigning In ComponentState) And Not FDestroying Then
   Begin
     Scene3D.ClearScene;
//     AddSphereToNewFrame;
     Fit(True,True);
     Redraw;
   End;
}
End; // TGLVisir.SetPlaneCol

Function TGLVisir.GetAxisNamesCol: TColor;
Begin
  Result := Scene3D.AxisNamesColor;
End; // TGLVisir.GetAxisNamesCol

Procedure TGLVisir.SetAxisNamesCol(Value: TColor);
Begin
  Scene3D.SetAxisNamesColor(Value);
  If Scene3D.LastFrame >= 0 Then
   If (csDesigning In ComponentState) And Not FDestroying Then
   Begin
     Scene3D.ClearScene;
//     AddSphereToNewFrame;
     Fit(True,True,True);
     Redraw;
   End;
End; // TGLVisir.SetAxisNamesCol

Function TGLVisir.GetDrawStyle: TObjectDrawStyle;
Begin
  Case StyleCombo.ItemIndex Of
    0: Result := Wired;
    1: Result := Filled;
    2: Result := Combined;
  Else
    Result := Filled;
  End; // Case
End; // TGLVisir.GetDrawStyle

Procedure TGLVisir.SetDrawStyle(Value: TObjectDrawStyle);
Begin
  Case Value Of
      Wired: StyleCombo.ItemIndex := 0;
     Filled: StyleCombo.ItemIndex := 1;
   Combined: StyleCombo.ItemIndex := 2;
  End; // Case
  StyleComboChange(Nil);
  Redraw;
End; // TGLVisir.SetDrawStyle

Function TGLVisir.GetCaption: String;
Begin
  Result := TitlePanel.Caption;
End; // TGLVisir.GetCaption

Procedure TGLVisir.SetCaption(Value: String);
Begin
  TitlePanel.Caption := Value;
End; // TGLVisir.SetCaption

Function TGLVisir.GetAxisXName: String;
Begin
  Result := Scene3D.AxisXName;
End; // TGLVisir.GetAxisXName

Procedure TGLVisir.SetAxisXName(Value: String);
Begin
  Scene3D.AxisXName := Value;
  If Scene3D.LastFrame >= 0 Then
   If (csDesigning In ComponentState) And Not FDestroying Then
   Begin
     Scene3D.ClearScene;
//     AddSphereToNewFrame;
     Fit(True,True,True);
     Redraw;
   End;
End; // TGLVisir.SetAxisXName

Function TGLVisir.GetAxisYName: String;
Begin
  Result := Scene3D.AxisYName;
End; // TGLVisir.GetAxisYName

Procedure TGLVisir.SetAxisYName(Value: String);
Begin
  Scene3D.AxisYName := Value;
  If Scene3D.LastFrame >= 0 Then
   If (csDesigning In ComponentState) And Not FDestroying Then
   Begin
     Scene3D.ClearScene;
//     AddSphereToNewFrame;
     Fit(True,True,True);
     Redraw;
   End;
End; // TGLVisir.SetAxisYName

Function TGLVisir.GetAxisZName: String;
Begin
  Result := Scene3D.AxisZName;
End; // TGLVisir.GetAxisZName

Procedure TGLVisir.SetAxisZName(Value: String);
Begin
  Scene3D.AxisZName := Value;
  If Scene3D.LastFrame >= 0 Then
   If (csDesigning In ComponentState) And Not FDestroying Then
   Begin
     Scene3D.ClearScene;
//     AddSphereToNewFrame;
     Fit(True,True,True);
     Redraw;
   End;
End; // TGLVisir.SetAxisZName

procedure TGLVisir.LeftBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Scene3D.ModifyAction := None;
end;

procedure TGLVisir.UpBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Scene3D.ModifyAction := None;
end;

procedure TGLVisir.DownBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Scene3D.ModifyAction := None;
end;

procedure TGLVisir.ZoomInBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Scene3D.ModifyAction := None;
end;

procedure TGLVisir.ZoomOutBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Scene3D.ModifyAction := None;
end;

procedure TGLVisir.RotateLeftBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If RotateMode Then Scene3D.ModifyAction := RotateLeft2;
end;

procedure TGLVisir.RotateLeftBtnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Scene3D.ModifyAction := None;
end;

procedure TGLVisir.RotateRightBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If RotateMode Then Scene3D.ModifyAction := RotateRight2;
end;

procedure TGLVisir.RotateRightBtnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Scene3D.ModifyAction := None;
end;

///////////////////////////////////////////////////////////////////////////////
(*
Procedure TGLVisir.AddSphereToNewFrame;
Const
  NXY = 20;
  NXZ = 20;

Var
  Mdl   : TModelRec;
//  v     : array [0..500] of VectorType;
//  p     : array [0..1000] of PolygonType;
  v     : Array [0..NXY * NXZ - 1] Of VertexType;
  p     : Array [0..(NXY * NXZ) * 2 - 1] Of PolygonType;
  i,j   : Integer;
  nv,np : Integer;
  r     : Double;

  i1,j1 : Double;
  j2    : Integer;
  i3,j3 : Integer;
  si,ci : Double;

Begin
  Try
    // Speed-optimized code

    r := 1;
    For I := 0 To NXY - 1 Do
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
        TRGBA(v[i+j*NXY].Color).R := 255;
        TRGBA(v[i+j*NXY].Color).G := 0;
        TRGBA(v[i+j*NXY].Color).B := 0;
        TRGBA(v[i+j*NXY].Color).A := 255;

        p[i+j*NXY*2].n[0] := i  + j2;
        p[i+j*NXY*2].n[1] := i3 + j2;
        p[i+j*NXY*2].n[2] := i3 + j3;
//        p[i+j*NXY*2].n[3] := i  + j3;
//        p[i+j*NXY*2].numVertexes := 4;
        p[i+j*NXY*2].TextureID   := -1;

        p[i+j*NXY*2+1].n[0] := i  + j2;
//        p[i+j*NXY*2+1].n[1] := i3 + j2;
        p[i+j*NXY*2+1].n[1] := i3 + j3;
        p[i+j*NXY*2+1].n[2] := i  + j3;
//        p[i+j*NXY*2+1].numVertexes := 4;
        p[i+j*NXY*2+1].TextureID   := -1;

        Inc(j2,NXY);
      End; // For j
    End; // For i
    nv := NXY * NXZ;
    np := NXY * NXZ * 2;
  {
    r:=1;
    nv := 0; np:= 0;
    For I := 0 To 19 do
     for j:=0 to 19 do
      Begin
       v[nv+i+j*20].x := r*sin(2*3.14*i/20)*cos(2*3.14*j/20);
       v[nv+i+j*20].y := r*cos(2*3.14*i/20)*cos(2*3.14*j/20);
       v[nv+i+j*20].z := r*sin(2*3.14*j/20);
      End;

    For I := 0 To 19 do
     for j:=0 to 19 do
      Begin
       p[np+i+j*20].n[0] := nv+i+j*20;
       p[np+i+j*20].n[1] := nv+((i+1) mod 20)+j*20;
       p[np+i+j*20].n[2] := nv+((i+1) mod 20)+((j+1) mod 20)*20;
       p[np+i+j*20].n[3] := nv+i+((j+1) mod 20)*20;
       p[np+i+j*20].numVertexes:=4;
       p[np+i+j*20].col := clRed;
      End;
    nv :=nv+400; np:=np+400;
  }
    // prepare structure for AddModel call
    Mdl.nv := nv;
    Mdl.np := np;
    SetLength(Mdl.v, Mdl.nv);
    SetLength(Mdl.p, Mdl.np);
    For I := 0 To NV - 1 Do Mdl.v[I] := v[I];
    For I := 0 To NP - 1 Do Mdl.p[I] := p[I];

    Try
      Scene3D.GetNewModel;
    Except
      ShowMessage('Caught exception in TGLVisir.AddSphereToNewFrame, trying to get a new model');
    End;
    Try
      AddModelToFrame(Mdl,Scene3D.Scene.Entities.Count - 1);
    Except
      ShowMessage('Caught exception in TGLVisir.AddSphereToNewFrame, trying to add to the model');
    End;

  //  AddModelToNewFrame(Mdl);

    Try
      TEntity(Scene3D.Scene.Entities.Items[Scene3D.Scene.Entities.Count - 1]).Visible := True;
    Except
      ShowMessage('Caught exception in TGLVisir.AddSphereToNewFrame, trying to make the entity visible');
    End;

    SetLength(Mdl.v, 0);
    SetLength(Mdl.p, 0);
  Except
    ShowMessage('Caught exception in TGLVisir.AddSphereToNewFrame');
  End;
End; // TGLVisir.AddSphereToNewFrame
*)
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// ---------------------------
// TScene3D
// ---------------------------

//------------------------------------------------------------------------------
//         Create and set default parameters Values
//------------------------------------------------------------------------------
Constructor TScene3D.Create(AOwner: TComponent; WindowHandle: THandle; AWidth,AHeight: Integer);//(AOwner: TComponent; iImagePanel,iDiapasonesPanel: TPanel);
Begin
  FOnNewDiapasones   := Nil;
  FWindowHandle      := WindowHandle;
  FLeft              := 0;
  FTop               := 0;
  FWidth             := AWidth;
  FHeight            := AHeight;
  FOwner             := AOwner;
//  ImagePanel         := iImagePanel;
//  DiapasonesPanel    := iDiapasonesPanel;
  bInitialized       := False; // graphic was not initialized
  CurFrame           := -1; // current frame No
  LastFrame          := -1; // last frame No
  bCycledAnimation   := True; // cycled animation
  bShowAllFrames     := False; // show frame by frame
  bShowBoundPlanes   := True; // show bound planes
  bShowTicks         := False; // don't show ticks
//  bShowDiapasones    := True; // show diapasones
  FirstPlane         := 0;
  LastPlane          := -1;
  FirstTick          := 0;
  LastTick           := -1;          
  PlaneColor         := clWhite;
  BackgroundColor    := clBlack;
  FontColor          := clWhite;
  AxisXTickCount     := 2;
  AxisYTickCount     := 2;
  AxisZTickCount     := 2;
  AxisXName          := 'X';
  AxisYName          := 'Y';
  AxisZName          := 'Z';
  AxisNamesColor     := clYellow;
  FontScale          := 0.2;
  DeltaRotate        := 3;
  DeltaMove          := 0.1;
  DeltaZoom          := 0.1;
//  SampleText         := Nil;
{  If csDesigning In FOwner.ComponentState
   Then Scene := Nil
   Else }Scene := TSceneGL.Create(Self); // create empty scene
//  View               := Nil;
  AxisX              := Nil;
  AxisY              := Nil;
  AxisZ              := Nil;

  LPX := 0;
  LPY := 0;
  LPZ := 0;

  LSX := 0;
  LSY := 0;
  LSZ := 0;


//  Scene.AddLight(1);
{
  // create the light

  Light              := TLight.create(1);
  Light.LightType    := clStar;
  Light.CutOffAngle  := 5;
  Light.SpotExponent := 200;
  Light.SetOrientation(1,1,1);
  Light.Source.SetPosition(-10,0,-5);
  Scene.Lights.add(Light);
}
//  Scene.InitRC(ImagePanel.Handle);    // initialize Rendering context
//  Scene.UpdateArea(ImagePanel.Width,ImagePanel.Height);


//  Mouse := T3DMouse.Create(Nil);    // instantiate the manipulation class
//  Mouse.Scale(1,1,0.1,1,1,1);       // set movement and rotation speeds
End; // TScene3D.Create

Destructor TScene3D.Destroy;
Begin
  AxisX.Free;
  AxisY.Free;
  AxisZ.Free;
//  Mouse.Free;
//  SampleText.Free;
  Scene.Free;
End; // TScene3D.Destroy

//------------------------------------------------------------------------------
//                   Graph init
//------------------------------------------------------------------------------
Procedure TScene3D.Init;
Begin
  If (Not bInitialized) And (Scene <> Nil) Then Scene.AddLight(0);
  FitCX := 0;
  FitCY := 0;
  FitCZ := 0;
  FitKX := 1;
  FitKY := 1;
  FitKZ := 1;
//  If Scene.WindowHandle = 0 Then Scene.InitRC(FWindowHandle);


//  If Scene.DC  = 0 Then Scene.InitDC;
//  If Scene.HRC = 0 Then Scene.InitRC;
  

//  If View.Showing And TCustomForm(View.Owner).Active Then
//  Begin


//    Scene.SetActive(True);
//    Scene.UpdateArea(FLeft,FTop,FWidth,FHeight);


//  End;
{
  Begin
    Scene.InitRC(ImagePanel.handle);
    Scene.UpdateArea(ImagePanel.width,ImagePanel.height);
  End;
}
  // Create font sample (can't take place until after the TSceneGL has been initialized, because
  // only then do we have a display context).
{
  If SampleText = Nil Then
  Begin
    SampleText := T3Dtext.Create(Scene,Scene.DC, 1);
    SampleText.FontScale[1] := FontScale;
    SampleText.FontScale[2] := FontScale;
    SampleText.FontScale[3] := FontScale;
    SampleText.ID := 1;
  End;
}
  bInitialized  := True;
End; // TScene3D.Init

//------------------------------------------------------------------------------
//                     Scene redraw
//------------------------------------------------------------------------------
Function TScene3D.Redraw: Boolean;
Begin
  If (Scene <> Nil) And AllInitialized Then
  Begin
//  LogToFile('TScene3D.Redraw(): Begin');

//  Try
    DrawDiapasones;
//    If View.Showing And Not View.FDestroying {And TCustomForm(View.Owner).Active} Then
//    Begin
(*
      // We do *not* want to do this here!!!

      If Not Scene.Active Then
      Begin
//        LogToFile('TScene3D.Redraw(): Making scene active');

        Scene.SetActive(True);
      End;
*)
//      LogToFile('TScene3D.Redraw(): Calling TSceneGL.UpdateArea()');

    Scene.UpdateArea(FLeft,FTop,FWidth,FHeight);

//      LogToFile('TScene3D.Redraw(): Calling TSceneGL.Redraw');

    Result := Scene.Redraw;
//    End;
//  Except
//  End;

//  LogToFile('TScene3D.Redraw(): End');
  End
  Else Result := False;
End; // TScene3D.Redraw

//------------------------------------------------------------------------------
// Draw value ranges
//------------------------------------------------------------------------------
Procedure TScene3D.DrawDiapasones;
Var S: String;
Begin
//  DiapasonesPanel.Visible := bShowDiapasones;
  If Assigned(FOnNewDiapasones) Then
  Begin
    S := '';
    S := S + AxisXName + ': (' + FormatFloat('#0.0000',XMin) + ', ' + FormatFloat('#0.0000',XMax) + ')  ';
    S := S + AxisYName + ': (' + FormatFloat('#0.0000',YMin) + ', ' + FormatFloat('#0.0000',YMax) + ')  ';
    S := S + AxisZName + ': (' + FormatFloat('#0.0000',ZMin) + ', ' + FormatFloat('#0.0000',ZMax) + ')  ';
    FOnNewDiapasones(S);
//    DiapasonesPanel.Caption := S;
  End;
End; // TScene3D.DrawDiapasones

//------------------------------------------------------------------------------
//           Draw on update
//------------------------------------------------------------------------------
Procedure TScene3D.Update(ALeft,ATop,AWidth,AHeight: Integer);
Begin
  LogToFile('TScene3D.Update(): Begin');

  // if 1st call - graph init
//  If Not View.FDestroying Then
//  Begin
//    if not bInitialized then Init;
  FLeft   := ALeft;
  FTop    := ATop;
  FWidth  := AWidth;
  FHeight := AHeight;

  If (Scene <> Nil) And AllInitialized Then
  Begin
//    LogToFile('TScene3D.Update(): calling Scene.UpdateArea for size ' + IntToStr(FWidth) + ' x ' + IntToStr(FHeight));

    Scene.UpdateArea(FLeft,FTop,FWidth,FHeight);

    LogToFile('TScene3D.Update(): calling Redraw');

    Redraw;
  End;

  LogToFile('TScene3D.Update(): End');
End; // TScene3D.Update

//------------------------------------------------------------------------------
// add model to frame
//------------------------------------------------------------------------------
Procedure TScene3D.AddModelToModel(Mdl: TModelRec; Model: TModel; Mirror,CalcVertexNormals: Boolean);
Type
  PSingle12Array = ^TSingle12Array;
  TSingle12Array = Array [0..11] Of Single;

Var
  I,J,K,L     : Integer;
  TextureID   : Integer;
  Solid       : Boolean;
  V1,V2,V3,V4 : T3DPoint;
  N           : T3DPoint;
  PVerts      : PSingle12Array;

Begin
  V1 := T3DPoint.Create;
  V2 := T3DPoint.Create;
  V3 := T3DPoint.Create;
  V4 := T3DPoint.Create;
  N  := T3DPoint.Create;

  Model.Clear;
  If Mirror Then
  Begin
    Model.AddFaces(Mdl.NP * 2);
    Model.AddVertices(Mdl.NV * 2);
  End
  Else
  Begin
    Model.AddFaces(Mdl.NP);
    Model.AddVertices(Mdl.NV);
  End;

  J := 0;
  K := 0;
  If Mirror Then
  Begin
    For I := 0 To Mdl.NV - 1 Do
    Begin
      Model.Positions.DataArray[J]     := Mdl.V[I].X;
      Model.Positions.DataArray[J + 1] := Mdl.V[I].Y;
      Model.Positions.DataArray[J + 2] := Mdl.V[I].Z;
      Model.VNormals[K]                := Mdl.V[I].Normal;
      Model.Vertices[K].TX             := Mdl.V[I].TX;
      Model.Vertices[K].TZ             := Mdl.V[I].TZ;
      Model.Colors[K]                  := Mdl.V[I].Color;
      Inc(J,3);
      Inc(K);
    End; // For I
    If Mdl.NV <> Mdl.NP * 4 Then
    Begin
      L := 0;
      For I := 0 To Mdl.NP - 1 Do
      Begin
        Model.Positions.DataArray[J + 0] := Mdl.V[L + 2].X;
        Model.Positions.DataArray[J + 1] := Mdl.V[L + 2].Y;
        Model.Positions.DataArray[J + 2] := Mdl.V[L + 2].Z;
        Model.Positions.DataArray[J + 3] := Mdl.V[L + 1].X;
        Model.Positions.DataArray[J + 4] := Mdl.V[L + 1].Y;
        Model.Positions.DataArray[J + 5] := Mdl.V[L + 1].Z;
        Model.Positions.DataArray[J + 6] := Mdl.V[L + 0].X;
        Model.Positions.DataArray[J + 7] := Mdl.V[L + 0].Y;
        Model.Positions.DataArray[J + 8] := Mdl.V[L + 0].Z;
        Model.VNormals[K + 0]            := Mdl.V[L + 2].Normal;
        Model.Vertices[K + 0].TX         := Mdl.V[L + 2].TX;
        Model.Vertices[K + 0].TZ         := Mdl.V[L + 2].TZ;
        Model.Colors[K + 0]              := Mdl.V[L + 2].Color;
        Model.VNormals[K + 1]            := Mdl.V[L + 1].Normal;
        Model.Vertices[K + 1].TX         := Mdl.V[L + 1].TX;
        Model.Vertices[K + 1].TZ         := Mdl.V[L + 1].TZ;
        Model.Colors[K + 1]              := Mdl.V[L + 1].Color;
        Model.VNormals[K + 2]            := Mdl.V[L + 0].Normal;
        Model.Vertices[K + 2].TX         := Mdl.V[L + 0].TX;
        Model.Vertices[K + 2].TZ         := Mdl.V[L + 0].TZ;
        Model.Colors[K + 2]              := Mdl.V[L + 0].Color;
        Inc(J,9);
        Inc(K,3);
        Inc(L,3);
      End; // For I
    End
    Else
    Begin
      L := 0;
      For I := 0 To Mdl.NP - 1 Do
      Begin
        Model.Positions.DataArray[J + 0]  := Mdl.V[L + 3].X;
        Model.Positions.DataArray[J + 1]  := Mdl.V[L + 3].Y;
        Model.Positions.DataArray[J + 2]  := Mdl.V[L + 3].Z;
        Model.Positions.DataArray[J + 3]  := Mdl.V[L + 2].X;
        Model.Positions.DataArray[J + 4]  := Mdl.V[L + 2].Y;
        Model.Positions.DataArray[J + 5]  := Mdl.V[L + 2].Z;
        Model.Positions.DataArray[J + 6]  := Mdl.V[L + 1].X;
        Model.Positions.DataArray[J + 7]  := Mdl.V[L + 1].Y;
        Model.Positions.DataArray[J + 8]  := Mdl.V[L + 1].Z;
        Model.Positions.DataArray[J + 9]  := Mdl.V[L + 0].X;
        Model.Positions.DataArray[J + 10] := Mdl.V[L + 0].Y;
        Model.Positions.DataArray[J + 11] := Mdl.V[L + 0].Z;
        Model.VNormals[K + 0]             := Mdl.V[L + 3].Normal;
        Model.Vertices[K + 0].TX          := Mdl.V[L + 3].TX;
        Model.Vertices[K + 0].TZ          := Mdl.V[L + 3].TZ;
        Model.Colors[K + 0]               := Mdl.V[L + 3].Color;
        Model.VNormals[K + 1]             := Mdl.V[L + 2].Normal;
        Model.Vertices[K + 1].TX          := Mdl.V[L + 2].TX;
        Model.Vertices[K + 1].TZ          := Mdl.V[L + 2].TZ;
        Model.Colors[K + 1]               := Mdl.V[L + 2].Color;
        Model.VNormals[K + 2]             := Mdl.V[L + 1].Normal;
        Model.Vertices[K + 2].TX          := Mdl.V[L + 1].TX;
        Model.Vertices[K + 2].TZ          := Mdl.V[L + 1].TZ;
        Model.Colors[K + 2]               := Mdl.V[L + 1].Color;
        Model.VNormals[K + 3]             := Mdl.V[L + 0].Normal;
        Model.Vertices[K + 3].TX          := Mdl.V[L + 0].TX;
        Model.Vertices[K + 3].TZ          := Mdl.V[L + 0].TZ;
        Model.Colors[K + 3]               := Mdl.V[L + 0].Color;
        Inc(J,12);
        Inc(K,4);
        Inc(L,4);
      End; // For I
    End;
  End
  Else
  Begin
    For I := 0 To Mdl.NV - 1 Do
    Begin
      Model.Positions.DataArray[J]     := Mdl.V[I].X;
      Model.Positions.DataArray[J + 1] := Mdl.V[I].Y;
      Model.Positions.DataArray[J + 2] := Mdl.V[I].Z;
      Model.VNormals[K]                := Mdl.V[I].Normal;
      Model.Vertices[K].TX             := Mdl.V[I].TX;
      Model.Vertices[K].TZ             := Mdl.V[I].TZ;
      Model.Colors[K]                  := Mdl.V[I].Color;
      Inc(J,3);
      Inc(K);
    End; // For I
  End;

  For I := 0 To Mdl.NP - 1 Do
  Begin
    //--- Add Face ---

    If Mirror
     Then K := I * 2
     Else K := I;

    TextureID   := Mdl.P[I].TextureID;
    Solid       := Mdl.P[I].Solid;

    // The face's texture is Nil by defualt

    Model.Faces[K].Texture := TextureID;
    If Solid
     Then Model.Faces[K].Flags := Model.Faces[K].Flags Or ffSolid
     Else Model.Faces[K].Flags := Model.Faces[K].Flags And Not ffSolid;

    If Mirror Then
    Begin
      //--- Add Face (mirror) ---

      If Solid
       Then Model.Faces[K + 1].Flags := Model.Faces[K + 1].Flags Or ffSolid
       Else Model.Faces[K + 1].Flags := Model.Faces[K + 1].Flags And Not ffSolid;

      // The face's texture is Nil by defualt

      Model.Faces[K + 1].Texture := TextureID;
    End;
  End; // For I

  // Check for faces where the vertices overlap...some OpenGL implementations crash if the triangles aren't really triangles

  If Mdl.NV <> Mdl.NP * 4 Then
  Begin
    // Model is made entirely of triangles
    J := 0;
    K := 0;
    PVerts := PSingle12Array(Model.Positions.DataArray);
    For I := 0 To High(Model.Faces) Do
    Begin
      // V1 <--> V2

      If (PVerts[0] = PVerts[3]) And (PVerts[1] = PVerts[4]) And (PVerts[2] = PVerts[5]) Then
      Begin
        PVerts[3] := PVerts[3] + Random * 0.001;
        PVerts[4] := PVerts[4] + Random * 0.001;
        PVerts[5] := PVerts[5] + Random * 0.001;
      End;
{      If (Model.Positions.DataArray[J]     = Model.Positions.DataArray[J + 3]) And
         (Model.Positions.DataArray[J + 1] = Model.Positions.DataArray[J + 4]) And
         (Model.Positions.DataArray[J + 2] = Model.Positions.DataArray[J + 5]) Then
      Begin
        Model.Positions.DataArray[J + 3] := Model.Positions.DataArray[J + 3] + Random * 0.001;
        Model.Positions.DataArray[J + 4] := Model.Positions.DataArray[J + 4] + Random * 0.001;
        Model.Positions.DataArray[J + 5] := Model.Positions.DataArray[J + 5] + Random * 0.001;
      End;
}
      // V1 <--> V3

      If (PVerts[0] = PVerts[6]) And (PVerts[1] = PVerts[7]) And (PVerts[2] = PVerts[8]) Then
      Begin
        PVerts[6] := PVerts[6] + Random * 0.001;
        PVerts[7] := PVerts[7] + Random * 0.001;
        PVerts[8] := PVerts[8] + Random * 0.001;
      End;
{      If (Model.Positions.DataArray[J]     = Model.Positions.DataArray[J + 6]) And
         (Model.Positions.DataArray[J + 1] = Model.Positions.DataArray[J + 7]) And
         (Model.Positions.DataArray[J + 2] = Model.Positions.DataArray[J + 8]) Then
      Begin
        Model.Positions.DataArray[J + 6] := Model.Positions.DataArray[J + 6] + Random * 0.001;
        Model.Positions.DataArray[J + 7] := Model.Positions.DataArray[J + 7] + Random * 0.001;
        Model.Positions.DataArray[J + 8] := Model.Positions.DataArray[J + 8] + Random * 0.001;
      End;
}
      // V2 <--> V3

      If (PVerts[3] = PVerts[6]) And (PVerts[4] = PVerts[7]) And (PVerts[5] = PVerts[8]) Then
      Begin
        PVerts[6] := PVerts[6] + Random * 0.001;
        PVerts[7] := PVerts[7] + Random * 0.001;
        PVerts[8] := PVerts[8] + Random * 0.001;
      End;
{      If (Model.Positions.DataArray[J + 3] = Model.Positions.DataArray[J + 6]) And
         (Model.Positions.DataArray[J + 4] = Model.Positions.DataArray[J + 7]) And
         (Model.Positions.DataArray[J + 5] = Model.Positions.DataArray[J + 8]) Then
      Begin
        Model.Positions.DataArray[J + 6] := Model.Positions.DataArray[J + 6] + Random * 0.001;
        Model.Positions.DataArray[J + 7] := Model.Positions.DataArray[J + 7] + Random * 0.001;
        Model.Positions.DataArray[J + 8] := Model.Positions.DataArray[J + 8] + Random * 0.001;
      End;
}
      // We don't need to calculate the face normals here...we're calling CalcNormals below
{
      // Calculate the face normal so we can use it for occlusion culling (need it for backface culling), and
      // check for any non-opaque faces

      V1.Copy(Model.Positions.DataArray[J + 6],Model.Positions.DataArray[J + 7],Model.Positions.DataArray[J + 8]);
      V2.Copy(Model.Positions.DataArray[J + 3],Model.Positions.DataArray[J + 4],Model.Positions.DataArray[J + 5]);
      V3.Copy(Model.Positions.DataArray[J + 0],Model.Positions.DataArray[J + 1],Model.Positions.DataArray[J + 2]);
      N.GetNormalTo(V1,V2,V3);

      Model.FNormals.DataArray[K]     := N.X;
      Model.FNormals.DataArray[K + 1] := N.Y;
      Model.FNormals.DataArray[K + 2] := N.Z;
}
      Inc(J,9);
      Inc(K,3);
      Inc(LongWord(PVerts),36);
    End; // For I
  End
  Else
  Begin
    // Assume the model is made entirely of quads (no triangle-quad mixing allowed)
    
    J := 0;
    K := 0;
    For I := 0 To High(Model.Faces) Do
    Begin
      // V1 <--> V2

      If (Model.Positions.DataArray[J]     = Model.Positions.DataArray[J + 3]) And
         (Model.Positions.DataArray[J + 1] = Model.Positions.DataArray[J + 4]) And
         (Model.Positions.DataArray[J + 2] = Model.Positions.DataArray[J + 5]) Then
      Begin
        Model.Positions.DataArray[J + 3] := Model.Positions.DataArray[J + 3] + Random * 0.001;
        Model.Positions.DataArray[J + 4] := Model.Positions.DataArray[J + 4] + Random * 0.001;
        Model.Positions.DataArray[J + 5] := Model.Positions.DataArray[J + 5] + Random * 0.001;
      End;

      // V1 <--> V3

      If (Model.Positions.DataArray[J]     = Model.Positions.DataArray[J + 6]) And
         (Model.Positions.DataArray[J + 1] = Model.Positions.DataArray[J + 7]) And
         (Model.Positions.DataArray[J + 2] = Model.Positions.DataArray[J + 8]) Then
      Begin
        Model.Positions.DataArray[J + 6] := Model.Positions.DataArray[J + 6] + Random * 0.001;
        Model.Positions.DataArray[J + 7] := Model.Positions.DataArray[J + 7] + Random * 0.001;
        Model.Positions.DataArray[J + 8] := Model.Positions.DataArray[J + 8] + Random * 0.001;
      End;

      // V1 <--> V4

      If (Model.Positions.DataArray[J]     = Model.Positions.DataArray[J + 9]) And
         (Model.Positions.DataArray[J + 1] = Model.Positions.DataArray[J + 10]) And
         (Model.Positions.DataArray[J + 2] = Model.Positions.DataArray[J + 11]) Then
      Begin
        Model.Positions.DataArray[J + 9]  := Model.Positions.DataArray[J + 9]  + Random * 0.001;
        Model.Positions.DataArray[J + 10] := Model.Positions.DataArray[J + 10] + Random * 0.001;
        Model.Positions.DataArray[J + 11] := Model.Positions.DataArray[J + 11] + Random * 0.001;
      End;

      // V2 <--> V3

      If (Model.Positions.DataArray[J + 3] = Model.Positions.DataArray[J + 6]) And
         (Model.Positions.DataArray[J + 4] = Model.Positions.DataArray[J + 7]) And
         (Model.Positions.DataArray[J + 5] = Model.Positions.DataArray[J + 8]) Then
      Begin
        Model.Positions.DataArray[J + 6] := Model.Positions.DataArray[J + 6] + Random * 0.001;
        Model.Positions.DataArray[J + 7] := Model.Positions.DataArray[J + 7] + Random * 0.001;
        Model.Positions.DataArray[J + 8] := Model.Positions.DataArray[J + 8] + Random * 0.001;
      End;

      // V2 <--> V4

      If (Model.Positions.DataArray[J + 3] = Model.Positions.DataArray[J + 9]) And
         (Model.Positions.DataArray[J + 4] = Model.Positions.DataArray[J + 10]) And
         (Model.Positions.DataArray[J + 5] = Model.Positions.DataArray[J + 11]) Then
      Begin
        Model.Positions.DataArray[J + 9]  := Model.Positions.DataArray[J + 9]  + Random * 0.001;
        Model.Positions.DataArray[J + 10] := Model.Positions.DataArray[J + 10] + Random * 0.001;
        Model.Positions.DataArray[J + 11] := Model.Positions.DataArray[J + 11] + Random * 0.001;
      End;

      // V3 <--> V4

      If (Model.Positions.DataArray[J + 6] = Model.Positions.DataArray[J + 9]) And
         (Model.Positions.DataArray[J + 7] = Model.Positions.DataArray[J + 10]) And
         (Model.Positions.DataArray[J + 8] = Model.Positions.DataArray[J + 11]) Then
      Begin
        Model.Positions.DataArray[J + 9]  := Model.Positions.DataArray[J + 9]  + Random * 0.001;
        Model.Positions.DataArray[J + 10] := Model.Positions.DataArray[J + 10] + Random * 0.001;
        Model.Positions.DataArray[J + 11] := Model.Positions.DataArray[J + 11] + Random * 0.001;
      End;

      // We don't need to calculate the face normals here...we're calling CalcNormals below
{
      // Calculate the face normal so we can use it for occlusion culling (need it for backface culling), and
      // check for any non-opaque faces

      V1.Copy(Model.Positions.DataArray[J + 6],Model.Positions.DataArray[J + 7],Model.Positions.DataArray[J + 8]);
      V2.Copy(Model.Positions.DataArray[J + 3],Model.Positions.DataArray[J + 4],Model.Positions.DataArray[J + 5]);
      V3.Copy(Model.Positions.DataArray[J + 0],Model.Positions.DataArray[J + 1],Model.Positions.DataArray[J + 2]);
      N.GetNormalTo(V1,V2,V3);

      Model.FNormals.DataArray[K]     := N.X;
      Model.FNormals.DataArray[K + 1] := N.Y;
      Model.FNormals.DataArray[K + 2] := N.Z;
}
      Inc(J,12);
      Inc(K,3);
    End; // For I
  End;

  N.Free;
  V4.Free;
  V3.Free;
  V2.Free;
  V1.Free;

  // Sort the model's faces in order of decreasing area so the occlusion culler can work more efficiently

//  ModelFaceSorter.SortFacesByArea(Model);

  Model.CalcExtents(False);
  Model.CalcNormals(CalcVertexNormals);
End; // TScene3D.AddModelToModel

//------------------------------------------------------------------------------
//        add new frame with specified model
//------------------------------------------------------------------------------
Procedure TScene3D.AddModelToNewFrame(Mdl: TModelRec; Mirror,CalcVertexNormals: Boolean);
Var
  Model      : TModel;
  Renderable : TRenderable;
  Entity     : TEntity;

Begin
  If Scene <> Nil Then
  Begin
    // Create new empty model and add it to the scene

    Model      := TModel(Scene.Models.GetNew(0));
    Renderable := TRenderable(Scene.Renderables.GetNew(0));
    Entity     := TEntity(Scene.Entities.GetNew(0));
    Renderable.AddModel('',Model);
    Entity.Renderable := Renderable;

    AddModelToModel(Mdl, Model, Mirror, CalcVertexNormals);

    // inc frames counter

    Inc(LastFrame);

    // Make the entity visible, since it is invisible by default, but only if it's the first frame

    If CurFrame = 0 Then Entity.Visible := True;

    // current frame - 0-th

    If CurFrame = -1 Then CurFrame := 0;
  End;
End; // TScene3D.AddModelToNewFrame

Function TScene3D.GetNewModel(Index: Integer): TModel;
Var
  Entity     : TEntity;
  Renderable : TRenderable;
  Model      : TModel;

Begin
  Model := Nil;
  If Scene <> Nil Then
  Begin
    If (Index >= 0) And (Index < Scene.Entities.Count) Then
    Begin
      Entity     := TEntity(Scene.Entities.Items[Index]);
      Renderable := TRenderable(Entity.Renderable);
      Model      := Renderable.Models[0];
      Entity.Init;
      Model.SetHasAlphaTrans := False;
    End
    Else
    Begin
      While Index >= Scene.Entities.Count Do
      Begin
        Entity            := TEntity(Scene.Entities.GetNew(0));
        Renderable        := TRenderable(Scene.Renderables.GetNew(0));
        Model             := TModel(Scene.Models.GetNew(0));
        Renderable.AddModel('',Model);
        Entity.Renderable := Renderable;
      End; // While
    End;
    Model.Clear;
    Model.CalcExtents(False);
  End;
  Result := Model;
End; // TScene3D.GetNewModel

Function TScene3D.GetNewModel: TModel;
Var
  Entity     : TEntity;
  Renderable : TRenderable;
  Model      : TModel;
  Index      : Integer;

Begin
  Model := Nil;
  If Scene <> Nil Then
  Begin
    Index      := Scene.Entities.Count;
    Entity     := TEntity(Scene.Entities.GetNew(0));
    Renderable := TRenderable(Scene.Renderables.GetNew(0));
    Model      := TModel(Scene.Models.GetNew(0));
    Renderable.AddModel('',Model);
    Entity.Renderable := Renderable;
    Model.Clear;
    Model.CalcExtents(False);
  End;
  Result := Model;
End; // TScene3D.GetNewModel

//------------------------------------------------------------------------------
// add model to specified frame
//------------------------------------------------------------------------------
Procedure TScene3D.AddModelToFrame(Mdl: TModelRec; nFrame: Integer; Mirror,CalcVertexNormals: Boolean);
Begin
  If Scene <> Nil Then
  Begin
    If nFrame > Scene.Entities.Count - 1
     Then AddModelToNewFrame(Mdl,Mirror,CalcVertexNormals)
     Else AddModelToModel(Mdl, TRenderable(TEntity(Scene.Entities[nFrame]).Renderable).Models[0],Mirror,CalcVertexNormals);
  End;
End; // TScene3D.AddModelToFrame

//------------------------------------------------------------------------------
//  add new tick to axis
//------------------------------------------------------------------------------
Procedure TScene3D.AddTick(Var Model: TModel;
                           x,y,z: Double;
                           dx,dy,dz: Double;
                           a1,a2,a3: Double;
                           labelDx: Double;
                           S: String);
Var
  I,J   : Integer;
  Texto : T3DText;
  R,G,B : Integer;

Begin

  //--- add line ---

  I := Model.AddVertices(6);
  J := Model.AddFaces(2);
//  SetLength(Model.Faces[J].Vertices,4);
//  Model.Faces[J].NumVerts := 4;

  Model.SetFace(J,[I,I + 1,I + 2]);
  Model.SetVertex(I + 0,x-dx,y-dy,z-dz,0,0,1);
  Model.SetVertex(I + 1,x+dx,y+dy,z+dz,0,0,1);
  Model.SetVertex(I + 2,x-dx,y-dy,z-dz,0,0,1);

  Model.SetFace(J,[I + 3,I + 4,I + 5]);
  Model.SetVertex(I + 3,x-dx,y-dy,z-dz,0,0,1);
  Model.SetVertex(I + 4,x-dx,y-dy,z-dz,0,0,1);
  Model.SetVertex(I + 5,x+dx,y+dy,z+dz,0,0,1);

  // This is faster

  r := LongWord(fontColor) And $FF;
  g := (LongWord(fontColor) Shr 8) And $FF;
  b := (LongWord(fontColor) Shr 16) And $FF;
{
    // get color components
    r := (fontColor div $1) mod $100;
    g := (fontColor div $100) mod $100;
    b := (fontColor div $10000) mod $100;
}
  If (Scene <> Nil) And (Scene.SampleText <> Nil) Then
  Begin
    texto := T3Dtext.create(Scene,Scene.SampleText);
    Texto.SetColor(r,g,b,255);
    Texto.localRotate(a1,a2,a3);
    Texto.Position.Copy(x+labelDx,y,z);
    texto.SetText(s);
    Scene.Entities.Add(texto);
  End;
End; // TScene3D.AddTick

//------------------------------------------------------------------------------
//     add new frame with bound planes
//------------------------------------------------------------------------------
Procedure TScene3D.AddBoundPlanesToNewFrame(bCoordsCenterSymmetry: Boolean);
Var
  Model       : TModel;
  Renderable  : TRenderable;
  Entity      : TEntity;
  R,G,B       : Integer;
  X1,X2       : Double;
  Y1,Y2       : Double;
  Z1,Z2       : Double;
  DX,DY,DZ    : Double;
  XX1,YY1,ZZ1 : Double;
  I,J         : Integer;
  Texto       : T3DText;
  S           : String;

Begin
  If Scene <> Nil Then
  Begin
    // store 1st elment No

    FirstPlane := Scene.Entities.Count;

    // create new empty model

    Model      := TModel(Scene.Models.GetNew(0));
    Renderable := TRenderable(Scene.Renderables.GetNew(0));
    Entity     := TEntity(Scene.Entities.GetNew(0));
    Renderable.AddModel('',Model);
    Entity.Renderable := Renderable;

    x1:=-1; x2:=1; y1:=-1; y2:=1; z1:=-1; z2:=1;
    dx:=0.3; dy:=0.3; dz:=0.3;

    // wired
    Entity.Wireframe := wfLines;

    If (not bCoordsCenterSymmetry) Then
    Begin
      J := Model.AddVertices(36 + 27);

     //----- add 3 boxes -------
      //--- add Face ---
      I := Model.AddFaces(2);
       // add Face vertexes
       Model.SetVertex(J + 0,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 1,x1,y2,z1,0,0,1);
       Model.SetVertex(J + 2,x2,y2,z1,0,0,1);

       Model.SetVertex(J + 3,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 4,x2,y2,z1,0,0,1);
       Model.SetVertex(J + 5,x2,y1,z1,0,0,1);
      //--- add mirror Face ---
      I := Model.AddFaces(2);
       // add vertexes
       Model.SetVertex(J + 6,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 7,x2,y1,z1,0,0,1);
       Model.SetVertex(J + 8,x2,y2,z1,0,0,1);

       Model.SetVertex(J + 9,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 10,x2,y2,z1,0,0,1);
       Model.SetVertex(J + 11,x1,y2,z1,0,0,1);

      //--- add Face ---
      I := Model.AddFaces(2);
       // add vertexes
       Model.SetVertex(J + 12,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 13,x2,y1,z1,0,0,1);
       Model.SetVertex(J + 14,x2,y1,z2,0,0,1);

       Model.SetVertex(J + 15,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 16,x2,y1,z2,0,0,1);
       Model.SetVertex(J + 17,x1,y1,z2,0,0,1);
      //--- mirror ---
      I := Model.AddFaces(2);
       // add vertexes
       Model.SetVertex(J + 18,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 19,x1,y1,z2,0,0,1);
       Model.SetVertex(J + 20,x2,y1,z2,0,0,1);

       Model.SetVertex(J + 21,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 22,x2,y1,z2,0,0,1);
       Model.SetVertex(J + 23,x2,y1,z1,0,0,1);

      //--- add Face ---
      I := Model.AddFaces(2);
       // add vertexes
       Model.SetVertex(J + 24,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 25,x1,y1,z2,0,0,1);
       Model.SetVertex(J + 26,x1,y2,z2,0,0,1);

       Model.SetVertex(J + 27,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 28,x1,y2,z2,0,0,1);
       Model.SetVertex(J + 29,x1,y2,z1,0,0,1);
      //--- mirror ---
      I := Model.AddFaces(2);
       // add vertexes
       Model.SetVertex(J + 30,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 31,x1,y2,z1,0,0,1);
       Model.SetVertex(J + 32,x1,y2,z2,0,0,1);

       Model.SetVertex(J + 33,x1,y1,z1,0,0,1);
       Model.SetVertex(J + 34,x1,y2,z2,0,0,1);
       Model.SetVertex(J + 35,x1,y1,z2,0,0,1);

       Inc(J,36);
    End
    Else J := Model.AddVertices(27);

    //--- add axis ---
    If (not bCoordsCenterSymmetry) Then
    Begin
      xx1 := x1;
      yy1 := y1;
      zz1 := z1;
     //--- axis X ---
      //--- add line ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 0,x2,y1,z1,0,0,1);
       Model.SetVertex(J + 1,x2+dx,y1,z1,0,0,1);
       Model.SetVertex(J + 2,x2+dx,y1,z1,0,0,1);

      //--- add arrow ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 3,x2+dx,y1,z1,0,0,1);
       Model.SetVertex(J + 4,x2+dx/2,y1-dy/3,z1,0,0,1);
       Model.SetVertex(J + 5,x2+dx/2,y1-dy/3,z1,0,0,1);

      I := Model.AddFaces(1);
       Model.SetVertex(J + 6,x2+dx,y1,z1,0,0,1);
       Model.SetVertex(J + 7,x2+dx/2,y1+dy/3,z1,0,0,1);
       Model.SetVertex(J + 8,x2+dx/2,y1+dy/3,z1,0,0,1);

     //--- axis Y ---
      //--- add line ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 9,x1,y2,z1,0,0,1);
       Model.SetVertex(J + 10,x1,y2+dy,z1,0,0,1);
       Model.SetVertex(J + 11,x1,y2+dy,z1,0,0,1);

      //--- add arrow ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 12,x1,y2+dy,z1,0,0,1);
       Model.SetVertex(J + 13,x1-dx/3,y2+dy/2,z1,0,0,1);
       Model.SetVertex(J + 14,x1-dx/3,y2+dy/2,z1,0,0,1);

      I := Model.AddFaces(1);
       Model.SetVertex(J + 15,x1,y2+dy,z1,0,0,1);
       Model.SetVertex(J + 16,x1+dx/3,y2+dy/2,z1,0,0,1);
       Model.SetVertex(J + 17,x1+dx/3,y2+dy/2,z1,0,0,1);

     //--- axis Z ---
      //--- add line ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 18,x1,y1,z2,0,0,1);
       Model.SetVertex(J + 19,x1,y1,z2+dz,0,0,1);
       Model.SetVertex(J + 20,x1,y1,z2+dz,0,0,1);

      //--- add arrow ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 21,x1,y1,z2+dz,0,0,1);
       Model.SetVertex(J + 22,x1,y1-dy/3,z2+dz/2,0,0,1);
       Model.SetVertex(J + 23,x1,y1-dy/3,z2+dz/2,0,0,1);

      I := Model.AddFaces(1);
       Model.SetVertex(J + 24,x1,y1,z2+dz,0,0,1);
       Model.SetVertex(J + 25,x1,y1+dy/3,z2+dz/2,0,0,1);
       Model.SetVertex(J + 26,x1,y1+dy/3,z2+dz/2,0,0,1);
    end
    else
    Begin
      xx1 := 0;
      yy1 := 0;
      zz1 := 0;
     //--- axis X ---
      //--- add line ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 0,x1-dx,0,0,0,0,1);
       Model.SetVertex(J + 1,x2+dx,0,0,0,0,1);
       Model.SetVertex(J + 2,x2+dx,0,0,0,0,1);

      //--- add arrow ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 3,x2+dx,0,0,0,0,1);
       Model.SetVertex(J + 4,x2+dx/2,0-dy/3,0,0,0,1);
       Model.SetVertex(J + 5,x2+dx/2,0-dy/3,0,0,0,1);

      I := Model.AddFaces(1);
       Model.SetVertex(J + 6,x2+dx,0,0,0,0,1);
       Model.SetVertex(J + 7,x2+dx/2,0+dy/3,0,0,0,1);
       Model.SetVertex(J + 8,x2+dx/2,0+dy/3,0,0,0,1);

     //--- axis Y ---
      //--- add line ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 9,0,y1-dy,0,0,0,1);
       Model.SetVertex(J + 10,0,y2+dy,0,0,0,1);
       Model.SetVertex(J + 11,0,y2+dy,0,0,0,1);

      //--- add arrow ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 12,0,y2+dy,0,0,0,1);
       Model.SetVertex(J + 13,0-dx/3,y2+dy/2,0,0,0,1);
       Model.SetVertex(J + 14,0-dx/3,y2+dy/2,0,0,0,1);

      I := Model.AddFaces(1);
       Model.SetVertex(J + 15,0,y2+dy,0,0,0,1);
       Model.SetVertex(J + 16,0+dx/3,y2+dy/2,0,0,0,1);
       Model.SetVertex(J + 17,0+dx/3,y2+dy/2,0,0,0,1);

     //--- axis Z ---
      //--- add line ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 18,0,0,z1-dz,0,0,1);
       Model.SetVertex(J + 19,0,0,z2+dz,0,0,1);
       Model.SetVertex(J + 20,0,0,z2+dz,0,0,1);

      //--- add arrow ---
      I := Model.AddFaces(1);
       Model.SetVertex(J + 21,0,0,z2+dz,0,0,1);
       Model.SetVertex(J + 22,0,0-dy/3,z2+dz/2,0,0,1);
       Model.SetVertex(J + 23,0,0-dy/3,z2+dz/2,0,0,1);

      I := Model.AddFaces(1);
       Model.SetVertex(J + 24,0,0,z2+dz,0,0,1);
       Model.SetVertex(J + 25,0,0+dy/3,z2+dz/2,0,0,1);
       Model.SetVertex(J + 26,0,0+dy/3,z2+dz/2,0,0,1);
    End;

    // This is faster
    r := LongWord(planeColor) And $FF;
    g := (LongWord(planeColor) Shr 8) And $FF;
    b := (LongWord(planeColor) Shr 16) And $FF;

    // set current color
    Model.SetColor(r,g,b,255);

    // Set arrow colors
    Model.Colors[J + 0] := $FF0000FF;
    Model.Colors[J + 1] := $FF0000FF;
    Model.Colors[J + 2] := $FF0000FF;
    Model.Colors[J + 3] := $FF0000FF;
    Model.Colors[J + 4] := $FF0000FF;
    Model.Colors[J + 5] := $FF0000FF;
    Model.Colors[J + 6] := $FF0000FF;
    Model.Colors[J + 7] := $FF0000FF;
    Model.Colors[J + 8] := $FF0000FF;

    Model.Colors[J +  9] := $FF00FF00;
    Model.Colors[J + 10] := $FF00FF00;
    Model.Colors[J + 11] := $FF00FF00;
    Model.Colors[J + 12] := $FF00FF00;
    Model.Colors[J + 13] := $FF00FF00;
    Model.Colors[J + 14] := $FF00FF00;
    Model.Colors[J + 15] := $FF00FF00;
    Model.Colors[J + 16] := $FF00FF00;
    Model.Colors[J + 17] := $FF00FF00;

    Model.Colors[J + 18] := $FFFF0000;
    Model.Colors[J + 19] := $FFFF0000;
    Model.Colors[J + 20] := $FFFF0000;
    Model.Colors[J + 21] := $FFFF0000;
    Model.Colors[J + 22] := $FFFF0000;
    Model.Colors[J + 23] := $FFFF0000;
    Model.Colors[J + 24] := $FFFF0000;
    Model.Colors[J + 25] := $FFFF0000;
    Model.Colors[J + 26] := $FFFF0000;

    Model.CalcExtents(False);

    // add half-box with axises to scene

    // This is faster
    r := LongWord(AxisNamesColor) And $FF;
    g := (LongWord(AxisNamesColor) Shr 8) And $FF;
    b := (LongWord(AxisNamesColor) Shr 16) And $FF;

    // axis names

    If Scene.SampleText <> Nil Then
    Begin
      If AxisX = Nil Then
      Begin
        AxisX := T3Dtext.create(Scene,Scene.SampleText);
        AxisX.SetColor(r,g,b,255);
        AxisX.FontScale[1] := AxisX.FontScale[1]*1.3;
        AxisX.FontScale[2] := AxisX.FontScale[2]*1.3;
        AxisX.FontScale[3] := AxisX.FontScale[3]*1.3;
        AxisX.SetText(AxisXName);
      End;
      Texto := T3DText.Create(Scene,AxisX);
      Texto.Position.Copy(x2+dx,yy1,zz1);
      Scene.Entities.Add(Texto);

      If AxisY = Nil Then
      Begin
        AxisY := T3Dtext.create(Scene,Scene.SampleText);
        AxisY.SetColor(r,g,b,255);
        AxisY.FontScale[1] := AxisY.FontScale[1]*1.3;
        AxisY.FontScale[2] := AxisY.FontScale[2]*1.3;
        AxisY.FontScale[3] := AxisY.FontScale[3]*1.3;
        AxisY.SetText(AxisYName);
      End;
      Texto := T3DText.Create(Scene,AxisY);
      Texto.Position.Copy(xx1,y2+dy,zz1);
      Scene.Entities.Add(Texto);

      If AxisZ = Nil Then
      Begin
        AxisZ := T3Dtext.create(Scene,Scene.SampleText);
        AxisZ.SetColor(r,g,b,255);
        AxisZ.FontScale[1] := AxisZ.FontScale[1]*1.3;
        AxisZ.FontScale[2] := AxisZ.FontScale[2]*1.3;
        AxisZ.FontScale[3] := AxisZ.FontScale[3]*1.3;
        AxisZ.SetText(AxisZName);
      End;
      Texto := T3DText.Create(Scene,AxisZ);
      Texto.Position.Copy(xx1,yy1,z2+dy);
      Scene.Entities.Add(Texto);
    End;

    // No of last element

    LastPlane := Scene.Entities.Count - 1;

    // No of 1st element

    FirstTick := Scene.Entities.Count;

    // create new empty model

    Model      := TModel(Scene.Models.GetNew(0));
    Renderable := TRenderable(Scene.Renderables.GetNew(0));
    Entity     := TEntity(Scene.Entities.GetNew(0));
    Renderable.AddModel('',Model);
    Entity.Renderable := Renderable;

    // ticks on axis X

    For I := 0 To AxisXTickCount - 1 Do
    Begin
      s := FormatFloat('#0.00',XMin+(XMax-XMin)*i/(AxisXTickCount - 1));
      AddTick(Model, x1+(x2-x1)*i/(AxisXTickCount - 1)+dx/4,yy1-dy/2,zz1-dz/4, 0,dy/4,0, 0,90,0, 0,s);
    End; // For I

    // ticks on axis Y

    For I := 0 To AxisYTickCount - 1 Do
    Begin
      s := FormatFloat('#0.00',YMin+(YMax-YMin)*i/(AxisYTickCount - 1));
      AddTick(Model, xx1,y1+(y2-y1)*i/(AxisYTickCount - 1)+dy/4,zz1-dz/4, 0,0,dz/4, 0,-45,0, -0.3,s);
    End; // For I

    // ticks on axis Y

    For I := 0 To AxisZTickCount - 1 Do
    Begin
      s := FormatFloat('#0.00',ZMin+(ZMax-ZMin)*i/(AxisZTickCount - 1));
      AddTick(Model, xx1,yy1-dy/2,z1+(z2-z1)*i/(AxisZTickCount - 1)+dz/4, 0,dy/4,0, 0,0,0, -0.3,s);
    End; // For I

    Model.CalcExtents(False);

    // last element No

    LastTick := Scene.Entities.Count - 1;
  End;
End; // TScene3D.AddBoundPlanesToNewFrame

//------------------------------------------------------------------------------
//        Center and zoom all frames
//------------------------------------------------------------------------------
Procedure TScene3D.Fit(bCoordsCenterSymmetry1,bTrueAspectRatio1,bScale: Boolean);
var x1,x2,y1,y2,z1,z2,
    cx,cy,cz,kx,ky,kz,k:double;
    i:Integer;

Begin
  If Scene <> Nil Then
  Begin
    FitCX := 0;
    FitCY := 0;
    FitCZ := 0;
    FitKX := 1;
    FitKY := 1;
    FitKZ := 1;

    bCoordsCenterSymmetry := bCoordsCenterSymmetry1;
    bTrueAspectRatio      := bTrueAspectRatio1;

    // get coordinate ranges
    XMax := -100000000; YMax := -100000000; ZMax := -100000000;
    XMin :=  100000000; YMin :=  100000000; ZMin :=  100000000;

    For I := 0 To Scene.Entities.Count - 1 Do
    Begin
      TEntity(Scene.Entities[I]).LocalPosition.Copy(0,0,0);
      TEntity(Scene.Entities[I]).LocalScale.Copy(1,1,1);
      TEntity(Scene.Entities[I]).LocalRotation.Copy(0,0,0);
      TEntity(Scene.Entities[I]).RebuildTransformMatrix;
      TEntity(Scene.Entities[I]).GetDiapasones(x1,x2,y1,y2,z1,z2);
      If x1 < XMin Then XMin := x1;
      If y1 < YMin Then YMin := y1;
      If z1 < ZMin Then ZMin := z1;
      If x2 > XMax Then XMax := x2;
      If y2 > YMax Then YMax := y2;
      If z2 > ZMax Then ZMax := z2;
    End; // For I

    If bCoordsCenterSymmetry Then
    Begin
      x1 := -max(abs(XMin),abs(XMax)); x2 := abs(x1);
      y1 := -max(abs(YMin),abs(YMax)); y2 := abs(y1);
      z1 := -max(abs(ZMin),abs(ZMax)); z2 := abs(z1);
    End
    Else
    Begin
      x1 := XMin; x2 := XMax;
      y1 := YMin; y2 := YMax;
      z1 := ZMin; z2 := ZMax;
    End;

    // center coords
    cx := (x1 + x2) / 2;
    cy := (y1 + y2) / 2;
    cz := (z1 + z2) / 2;

    // zoom multiplier
    If (abs(x2 - x1) > 0.000001)
     Then kx := 2/(x2-x1)
     Else kx := 1;
    If (abs(y2 - y1) > 0.000001)
     Then ky := 2/(y2-y1)
     Else ky := 1;
    If (abs(z2 - z1) > 0.000001)
     Then kz := 2/(z2-z1)
     Else kz := 1;

    If bTrueAspectRatio
     Then k := min(kx,min(ky,kz))
     Else k := kz;

    If (Not bCoordsCenterSymmetry) Then
     If (ZMin = 0) Then
      If (ZMax - ZMin < 2 / k) Then
       cz := 0;
  {
    K  := K  * 100;
    KX := KX * 100;
    KY := KY * 100;
    KZ := KZ * 100;

    Scene.DefaultCamera.SetPosition(0,0,800);
    Scene.DefaultCamera.LookAt(0,0,-100);
    Scene.DefaultCamera.SetVectorUp(1,0,0);
  }
  {
        If bTrueAspectRatio Then
        Begin
          CX := CX * K;
          CY := CY * K;
          CZ := CZ * K;
        End
        Else
        Begin
          CX := CX * KX;
          CY := CY * KY;
          CZ := CZ * KZ;
        End;
  }

    // center and zoom
    For I := 0 To Scene.Entities.Count - 1 Do
    Begin
      If Not bCoordsCenterSymmetry Then
      Begin
  //      TEntity(Scene.Entities[I]).Position.Subtract(CX,CY,CZ);
        TEntity(Scene.Entities[I]).LocalPosition.Copy(-CX,-CY,-CZ);
        FitCX := CX;
        FitCY := CY;
        FitCZ := CZ;

        LPX := CX;
        LPY := CY;
        LPZ := CZ;

        If bTrueAspectRatio Then
        Begin
    //      TEntity(Scene.Entities[I]).Scale.Multiply(K);
          TEntity(Scene.Entities[I]).LocalScale.Copy(K,K,K);
    //      TEntity(Scene.Entities[I]).Position.Multiply(K);
          FitKX := K;
          FitKY := K;
          FitKZ := K;

          LSX := K;
          LSY := K;
          LSZ := K;

    {      CX := CX * K;
          CY := CY * K;
          CZ := CZ * K;}
        End
        Else
        Begin
    //      TEntity(Scene.Entities[I]).Scale.Multiply(KX,KY,KZ);
          TEntity(Scene.Entities[I]).LocalScale.Copy(KX,KY,KZ);
    //      TEntity(Scene.Entities[I]).Position.Multiply(KX,KY,KZ);
          FitKX := KX;
          FitKY := KY;
          FitKZ := KZ;

          LSX := KX;
          LSY := KY;
          LSZ := KZ;
    {      CX := CX * KX;
          CY := CY * KY;
          CZ := CZ * KZ;}
        End;
      End
      Else
      Begin
        TEntity(Scene.Entities[I]).LocalPosition.Copy(0,0,0);

        FitCX := 0;
        FitCY := 0;
        FitCZ := 0;

        LPX := 0;
        LPY := 0;
        LPZ := 0;

        If bScale Then
        Begin
          If bTrueAspectRatio Then
          Begin
      //      TEntity(Scene.Entities[I]).Scale.Multiply(K);
            TEntity(Scene.Entities[I]).LocalScale.Copy(K,K,K);
      //      TEntity(Scene.Entities[I]).Position.Multiply(K);
            FitKX := K;
            FitKY := K;
            FitKZ := K;

            LSX := K;
            LSY := K;
            LSZ := K;

      {      CX := CX * K;
            CY := CY * K;
            CZ := CZ * K;}
          End
          Else
          Begin
      //      TEntity(Scene.Entities[I]).Scale.Multiply(KX,KY,KZ);
            TEntity(Scene.Entities[I]).LocalScale.Copy(KX,KY,KZ);
      //      TEntity(Scene.Entities[I]).Position.Multiply(KX,KY,KZ);
            FitKX := KX;
            FitKY := KY;
            FitKZ := KZ;

            LSX := KX;
            LSY := KY;
            LSZ := KZ;
      {      CX := CX * KX;
            CY := CY * KY;
            CZ := CZ * KZ;}
          End;
        End
        Else TEntity(Scene.Entities[I]).LocalScale.Copy(1,1,1);
      End;
    End;
{
    // flat pictures or sets started from zero move to the back wall

    If (ZMin = 0) Then
     If ((ZMin = ZMax) Or (ZMax - ZMin < 2 / K)) Then
     Begin
       If (Not bCoordsCenterSymmetry) Then
       Begin
         CZ := 1;
         For I := 0 To Scene.Entities.Count - 1 Do
          TEntity(Scene.Entities[I]).Position.Subtract(0,0,CZ);
       End;
     End;
}
    // Call this again for each entity so they know their size

    For I := 0 To Scene.Entities.Count - 1 Do
     TEntity(Scene.Entities[I]).GetDiapasones(x1,x2,y1,y2,z1,z2);

    If bShowBoundPlanes Or bShowTicks Then AddBoundPlanesToNewFrame(bCoordsCenterSymmetry); // add bound planes
    ShowBoundPlanes(bShowBoundPlanes);
    ShowTicks(bShowTicks);
  End;
End; // TScene3D.Fit

Procedure TScene3D.FitFrame(bCoordsCenterSymmetry1,bTrueAspectRatio1,bScale: Boolean; nFrame: Integer);
Var
  x1,x2,y1,y2,z1,z2   : Double;
  cx,cy,cz,kx,ky,kz,k : Double;
  sx1,sx2,sy1,sy2,sz1,sz2   : Double;

Begin
  If (Scene <> Nil) And (nFrame >= 0) And (nFrame < Scene.Entities.Count) Then
  Begin
    FitCX := 0;
    FitCY := 0;
    FitCZ := 0;
    FitKX := 1;
    FitKY := 1;
    FitKZ := 1;

    bCoordsCenterSymmetry := bCoordsCenterSymmetry1;
    bTrueAspectRatio      := bTrueAspectRatio1;

    // get coordinate ranges
    XMax := -100000000; YMax := -100000000; ZMax := -100000000;
    XMin := 100000000; YMin := 100000000; ZMin := 100000000;

    // Don't want to rescale, since existing entities are already scaled to a different amount, but we must
    // call TEntity.GetDiapasones


    TEntity(Scene.Entities[nFrame]).GetDiapasones(sx1,sx2,sy1,sy2,sz1,sz2);
  {
    TEntity(Scene.Entities.Items[nFrame]).GetDiapasones(x1,x2,y1,y2,z1,z2);

    if x1 < XMin then XMin := x1;
    if y1 < YMin then YMin := y1;
    if z1 < ZMin then ZMin := z1;
    if x2 > XMax then XMax := x2;
    if y2 > YMax then YMax := y2;
    if z2 > ZMax then ZMax := z2;
  }

    if sx1 < XMin then XMin := sx1;
    if sy1 < YMin then YMin := sy1;
    if sz1 < ZMin then ZMin := sz1;
    if sx2 > XMax then XMax := sx2;
    if sy2 > YMax then YMax := sy2;
    if sz2 > ZMax then ZMax := sz2;

    If (bCoordsCenterSymmetry) Then
    Begin
      x1 := -max(abs(XMin),abs(XMax)); x2 := abs(x1);
      y1 := -max(abs(YMin),abs(YMax)); y2 := abs(y1);
      z1 := -max(abs(ZMin),abs(ZMax)); z2 := abs(z1);
    end
    else
    Begin
      x1 := XMin; x2 := XMax;
      y1 := YMin; y2 := YMax;
      z1 := ZMin; z2 := ZMax;
    End;

    // center coords
    cx := (x1+x2)/2;
    cy := (y1+y2)/2;
    cz := (z1+z2)/2;

    // zoom multiplier
    If (abs(x2-x1)>0.000001)
     then kx := 2/(x2-x1)
     else kx := 1;
    If (abs(y2-y1)>0.000001)
     then ky := 2/(y2-y1)
     else ky := 1;
    If (abs(z2-z1)>0.000001)
     then kz := 2/(z2-z1)
     else kz := 1;

    if bTrueAspectRatio
     then k := min(kx,min(ky,kz))
     else k := kz;

    If Not bCoordsCenterSymmetry Then
     If ZMin = 0 Then
      If (ZMax - ZMin) < (2 / k) Then cz := 0;

    // center and zoom
    If Not bCoordsCenterSymmetry Then
    Begin
  //    TEntity(Scene.Entities[nFrame]).Position.Subtract(CX,CY,CZ);
      TEntity(Scene.Entities[nFrame]).LocalPosition.Copy(-LPX,-LPY,-LPZ);
      FitCX := CX;
      FitCY := CY;
      FitCZ := CZ;
    End
    Else
    Begin
      TEntity(Scene.Entities[nFrame]).LocalPosition.Copy(0,0,0);
      FitCX := 0;
      FitCY := 0;
      FitCZ := 0;
    End;
    If bScale Then
    Begin
      If bTrueAspectRatio Then
      Begin
    //    TEntity(Scene.Entities[nFrame]).Scale.Multiply(K);
    //    TEntity(Scene.Entities[nFrame]).Position.Multiply(K);
        TEntity(Scene.Entities[nFrame]).LocalScale.Copy(LSX,LSY,LSZ);
        FitKX := K;
        FitKY := K;
        FitKZ := K;
      End
      Else
      Begin
    //    TEntity(Scene.Entities[nFrame]).Scale.Multiply(KX,KY,KZ);
    //    TEntity(Scene.Entities[nFrame]).Position.Multiply(KX,KY,KZ);
        TEntity(Scene.Entities[nFrame]).LocalScale.Copy(LSX,LSY,LSZ);
        FitKX := KX;
        FitKY := KY;
        FitKZ := KZ;
      End;
    End
    Else TEntity(Scene.Entities[nFrame]).LocalScale.Copy(1,1,1);
{
    // flat pictures or sets started from zero move to the back wall
    If (ZMin = 0) Then
     If ((ZMin = ZMax) or (ZMax - ZMin < 2/k)) Then
     Begin
       If (not bCoordsCenterSymmetry) Then
       Begin
         cz := 1;
         TEntity(Scene.Entities[nFrame]).Position.Subtract(0,0,CZ);
       End;
     End;
}
    // Need to call this again so the entity knows its size

    TEntity(Scene.Entities[nFrame]).GetDiapasones(sx1,sx2,sy1,sy2,sz1,sz2);

    If bShowBoundPlanes Or bShowTicks Then AddBoundPlanesToNewFrame(bCoordsCenterSymmetry); // add bound planes
    ShowBoundPlanes(bShowBoundPlanes);
    ShowTicks(bShowTicks);
  End;
End; // TScene3D.FitFrame

//------------------------------------------------------------------------------
// Show boundary planes
//------------------------------------------------------------------------------
Procedure TScene3D.ShowBoundPlanes(Flag: Boolean);
Var I: Integer;
Begin
  bShowBoundPlanes := Flag;
  If Scene <> Nil Then
  Begin
    // show / hide planes

    For I := FirstPlane To LastPlane Do
     If (I >= 0) And (I < Scene.Entities.Count) Then
      TEntity(Scene.Entities[I]).Visible := Flag;
  //  ? ??????? ????
  //  For I := FirstTick To LastTick Do Scene.Entities[I]).Visible := Flag;
  End;
End; // TScene3D.ShowBoundPlanes

//------------------------------------------------------------------------------
// show ticks on axis
//------------------------------------------------------------------------------
Procedure TScene3D.ShowTicks(Flag: Boolean);
Var I: Integer;
Begin
  bShowTicks := Flag;
  If Scene <> Nil Then
  Begin
    // show / hide

    For I := FirstTick To LastTick Do
     If (I >= 0) And (I < Scene.Entities.Count) Then
      TEntity(Scene.Entities[I]).Visible := Flag;
  End;
End; // TScene3D.ShowTicks

//------------------------------------------------------------------------------
// show all frames simultaneously?
//------------------------------------------------------------------------------
Procedure TScene3D.ShowAllFrames(Flag: Boolean);
Var I: Integer;
Begin
  bShowAllFrames := Flag;
  If Scene <> Nil Then
  Begin
    // show / hide

    For I := 0 To LastFrame Do
     If (I >= 0) And (I < Scene.Entities.Count) Then
      TEntity(Scene.Entities[I]).Visible := Flag;

    TEntity(Scene.Entities[CurFrame]).Visible := True;
  End;  
End; // TScene3D.ShowAllFrames

//------------------------------------------------------------------------------
// set draw style
//------------------------------------------------------------------------------
Procedure TScene3D.SetStyle(Style: TWireFrame);
Var I: Integer;
Begin
  ModelStyle := Style;
  If Scene <> Nil Then
  Begin
    For I := 0 To LastFrame Do
     If (I >= 0) And (I < Scene.Entities.Count) Then
      TEntity(Scene.Entities[I]).WireFrame := ModelStyle;
  End;
End; // TScene3D.SetStyle
(*
//------------------------------------------------------------------------------
// set color of model
//------------------------------------------------------------------------------
Procedure TScene3D.SetModelColor(Col: TColor);
Var
  I     : Integer;
  R,G,B : Integer;

Begin
  ModelColor := Col;

  // This is faster

  B := LongWord(ModelColor) And $FF;
  G := (LongWord(ModelColor) Shr 8) And $FF;
  R := (LongWord(ModelColor) Shr 16) And $FF;
{
  // Get RGB components

  R := (ModelColor Div $1)     Mod $100;
  G := (ModelColor Div $100)   Mod $100;
  B := (ModelColor Div $10000) Mod $100;
}
  For I := 0 To LastFrame Do TEntity(Scene.Entities[I]).SetColor(R,G,B,255);
End; // TScene3D.SetModelColor
*)
//------------------------------------------------------------------------------
// set Background color
//------------------------------------------------------------------------------
Procedure TScene3D.SetBackgroundColor(Col: TColor);
Var R,G,B: Integer;
Begin
  BackgroundColor := Col;

  // This is faster

  R := LongWord(BackgroundColor) And $FF;
  G := (LongWord(BackgroundColor) Shr 8) And $FF;
  B := (LongWord(BackgroundColor) Shr 16) And $FF;
{
  // Get RGB components

  R := (BackgroundColor Div $1)     Mod $100;
  G := (BackgroundColor Div $100)   Mod $100;
  B := (BackgroundColor Div $10000) Mod $100;
}
  If Scene <> Nil Then
  Begin
    Scene.BackR := R / 255;
    Scene.BackG := G / 255;
    Scene.BackB := B / 255;
  End;
End; // TScene3D.SetBackgroundColor

Procedure TScene3D.SetPlaneColor(Col: TColor);
Var I,R,G,B: Integer;
Begin
  PlaneColor := Col;

  // This is faster

  R := LongWord(PlaneColor) And $FF;
  G := (LongWord(PlaneColor) Shr 8) And $FF;
  B := (LongWord(PlaneColor) Shr 16) And $FF;
{
  // Get RGB components

  R := (PlaneColor Div $1)     Mod $100;
  G := (PlaneColor Div $100)   Mod $100;
  B := (PlaneColor Div $10000) Mod $100;
}
  If Scene <> Nil Then
  Begin
    For I := FirstPlane To LastPlane - 3 Do
    Begin
      If (I >= 0) And (I < Scene.Entities.Count) Then
       TRenderable(TEntity(Scene.Entities[I]).Renderable).Models[0].SetColor(R,G,B,255);
    End;
  End;
End; // TScene3D.SetPlaneColor

Procedure TScene3D.SetAxisNamesColor(Col: TColor);
Var I,R,G,B: Integer;
Begin
  AxisNamesColor := Col;

  // This is faster

  R := LongWord(AxisNamesColor) And $FF;
  G := (LongWord(AxisNamesColor) Shr 8) And $FF;
  B := (LongWord(AxisNamesColor) Shr 16) And $FF;
{
  // Get RGB components

  R := (PlaneColor Div $1)     Mod $100;
  G := (PlaneColor Div $100)   Mod $100;
  B := (PlaneColor Div $10000) Mod $100;
}
  If Scene <> Nil Then
  Begin
    If LastPlane >= 3 Then
     For I := LastPlane - 3 To LastPlane - 1 Do
      If (I >= 0) And (I < Scene.Entities.Count) Then
       TRenderable(TEntity(Scene.Entities[I]).Renderable).Models[0].SetColor(R,G,B,255);
  End;     
End; // TScene3D.SetAxisNamesColor

//------------------------------------------------------------------------------
//         set active frame
//------------------------------------------------------------------------------
Procedure TScene3D.SetFrameByNum(iFrame: Integer);
Begin
  If Scene <> Nil Then
  Begin
    If (CurFrame >= 0) And (CurFrame< Scene.Entities.Count) Then TEntity(Scene.Entities[CurFrame]).Visible := False;
    CurFrame := iFrame;
    If (CurFrame >= 0) And (CurFrame< Scene.Entities.Count) Then TEntity(Scene.Entities[CurFrame]).Visible := True;
  End;  
End; // TScene3D.SetFrameByNum

//------------------------------------------------------------------------------
//           Set previous frame active
//------------------------------------------------------------------------------
Procedure TScene3D.SetPrevFrame;
Begin
  If Scene <> Nil Then
  Begin
    If (CurFrame >= 0) And (CurFrame < Scene.Entities.Count) Then
     TEntity(Scene.Entities[CurFrame]).Visible := False;
    Dec(CurFrame);
    If CurFrame < 0 Then
     If bCycledAnimation
      Then CurFrame := LastFrame
      Else CurFrame := 0;
    If (CurFrame >= 0) And (CurFrame < Scene.Entities.Count) Then
     TEntity(Scene.Entities[CurFrame]).Visible := True;
  End;
End; // TScene3D.SetPrevFrame

//------------------------------------------------------------------------------
//           Set next frame active
//------------------------------------------------------------------------------
Procedure TScene3D.SetNextFrame;
Begin
  If Scene <> Nil Then
  Begin
    If (CurFrame >= 0) And (CurFrame < Scene.Entities.Count) Then
     TEntity(Scene.Entities[CurFrame]).Visible := False;
    Inc(CurFrame);
    If CurFrame > LastFrame Then
     If bCycledAnimation
      Then CurFrame := 0
      Else CurFrame := LastFrame;
    If (CurFrame >= 0) And (CurFrame < Scene.Entities.Count) Then
     TEntity(Scene.Entities[CurFrame]).Visible := True;
  End;     
End; // TScene3D.SetNextFrame

//------------------------------------------------------------------------------
//           Process actions of scene modification
//------------------------------------------------------------------------------
Procedure TScene3D.ProcessModifyAction;
Begin
  Case ModifyAction Of
    RotateUp,
    RotateDown,
    RotateLeft,
    RotateRight,
    RotateLeft2,
    RotateRight2: ProcessAction(ModifyAction,DeltaRotate);

    MoveUp,
    MoveDown,
    MoveLeft,
    MoveRight: ProcessAction(ModifyAction,DeltaMove);

    ZoomIn,
    ZoomOut: ProcessAction(ModifyAction,DeltaZoom);
  End; // Case
  If ModifyAction <> None Then Redraw;
End; // TScene3D.ProcessModifyAction

//------------------------------------------------------------------------------
//            Change scene
//------------------------------------------------------------------------------
Procedure TScene3D.ProcessAction(Action: TModifyAction; Delta: Double);
Var
  I : Integer;
  E : TEntity;

Begin
  If Scene <> Nil Then
  Begin
    Case Action Of
      RotateRight:
      Begin
        For I := 0 To Scene.Entities.Count - 1 Do
        Begin
          E := TEntity(Scene.Entities[I]);
          E.LocalRotation.X := E.LocalRotation.X + Delta;
          E.LocalRotation.Dirty := True;
        End; // For I
        For I := 0 To Scene.Lights.Count   - 1 Do TLight(Scene.Lights.Items[I]).Rotation[1]   := TLight(Scene.Lights.Items[I]).Rotation[1]    + Delta;
      End;
      RotateLeft:
      Begin
        For I := 0 To Scene.Entities.Count - 1 Do
        Begin
          E := TEntity(Scene.Entities[I]);
          E.LocalRotation.X := E.LocalRotation.X - Delta;
          E.LocalRotation.Dirty := True;
        End; // For I
        For I := 0 To Scene.Lights.Count   - 1 Do TLight(Scene.Lights.Items[I]).Rotation[1]    := TLight(Scene.Lights.Items[I]).Rotation[1]    - Delta;
      End;
      RotateUp:
      Begin
        For I := 0 To Scene.Entities.Count - 1 Do
        Begin
          E := TEntity(Scene.Entities[I]);
          E.LocalRotation.Y := E.LocalRotation.Y + Delta;
          E.LocalRotation.Dirty := True;
        End; // For I
        For I := 0 To Scene.Lights.Count   - 1 Do TLight(Scene.Lights.Items[I]).Rotation[2]    := TLight(Scene.Lights.Items[I]).Rotation[2]    + Delta;
      End;
      RotateDown:
      Begin
        For I := 0 To Scene.Entities.Count - 1 Do
        Begin
          E := TEntity(Scene.Entities[I]);
          E.LocalRotation.Y := E.LocalRotation.Y - Delta;
          E.LocalRotation.Dirty := True;
        End; // For I
        For I := 0 To Scene.Lights.Count   - 1 Do TLight(Scene.Lights.Items[I]).Rotation[2]    := TLight(Scene.Lights.Items[I]).Rotation[2]    - Delta;
      End;
      RotateLeft2:
      Begin
        For I := 0 To Scene.Entities.Count - 1 Do
        Begin
          E := TEntity(Scene.Entities[I]);
          E.LocalRotation.Z := E.LocalRotation.Z + Delta;
          E.LocalRotation.Dirty := True;
        End; // For I
        For I := 0 To Scene.Lights.Count   - 1 Do TLight(Scene.Lights.Items[I]).Rotation[3]    := TLight(Scene.Lights.Items[I]).Rotation[3]    + Delta;
      End;
      RotateRight2:
      Begin
        For I := 0 To Scene.Entities.Count - 1 Do
        Begin
          E := TEntity(Scene.Entities[I]);
          E.LocalRotation.Z := E.LocalRotation.Z - Delta;
          E.LocalRotation.Dirty := True;
        End; // For I
        For I := 0 To Scene.Lights.Count   - 1 Do TLight(Scene.Lights.Items[I]).Rotation[3]    := TLight(Scene.Lights.Items[I]).Rotation[3]    - Delta;
      End;
  {
      MoveLeft:  For I := 0 To Scene.Entities.Count - 1 Do TEntity(Scene.Entities[I]).LocalPosition[1] := TEntity(Scene.Entities[I]).LocalPosition[1] - Delta;
      MoveRight: For I := 0 To Scene.Entities.Count - 1 Do TEntity(Scene.Entities[I]).LocalPosition[1] := TEntity(Scene.Entities[I]).LocalPosition[1] + Delta;
      MoveUp:    For I := 0 To Scene.Entities.Count - 1 Do TEntity(Scene.Entities[I]).LocalPosition[2] := TEntity(Scene.Entities[I]).LocalPosition[2] + Delta;
      MoveDown:  For I := 0 To Scene.Entities.Count - 1 Do TEntity(Scene.Entities[I]).LocalPosition[2] := TEntity(Scene.Entities[I]).LocalPosition[2] - Delta;
  }
      ZoomIn:
      Begin
        For I := 0 To Scene.Entities.Count - 1 Do
        Begin
          TEntity(Scene.Entities[I]).LocalScale.Multiply(1.1);
  {
          TEntity(Scene.Entities[I]).Scale.X := TEntity(Scene.Entities[I]).Scale.X * 1.1;// + Delta;
          TEntity(Scene.Entities[I]).Scale.Y := TEntity(Scene.Entities[I]).Scale.Y * 1.1;// + Delta;
          TEntity(Scene.Entities[I]).Scale.Z := TEntity(Scene.Entities[I]).Scale.Z * 1.1;// + Delta;
  }
  //        TEntity(Scene.Entities[I]).Position.Multiply(1.1);
        End;
        LSX := LSX * 1.1;
        LSY := LSY * 1.1;
        LSZ := LSZ * 1.1;
  {
        For I := 0 To Scene.Lights.Count - 1 Do
        Begin
          TLight(Scene.Lights.Items[I]).Scale[1] := TLight(Scene.Lights.Items[I]).Scale[1] * 1.1;// + Delta;
          TLight(Scene.Lights.Items[I]).Scale[2] := TLight(Scene.Lights.Items[I]).Scale[2] * 1.1;// + Delta;
          TLight(Scene.Lights.Items[I]).Scale[3] := TLight(Scene.Lights.Items[I]).Scale[3] * 1.1;// + Delta;
        End;
  }
      End;
      ZoomOut:
      Begin
        For I := 0 To Scene.Entities.Count - 1 Do
        Begin
          TEntity(Scene.Entities[I]).LocalScale.Divide(1.1);
  {
          TEntity(Scene.Entities[I]).Scale.X := TEntity(Scene.Entities[I]).Scale.X / 1.1;// - Delta;
          TEntity(Scene.Entities[I]).Scale.Y := TEntity(Scene.Entities[I]).Scale.Y / 1.1;// - Delta;
          TEntity(Scene.Entities[I]).Scale.Z := TEntity(Scene.Entities[I]).Scale.Z / 1.1;// - Delta;
  }
  //        TEntity(Scene.Entities[I]).Position.Divide(1.1);
        End;
        LSX := LSX / 1.1;
        LSY := LSY / 1.1;
        LSZ := LSZ / 1.1;
  {
        For I := 0 To Scene.Lights.Count - 1 Do
        Begin
          TLight(Scene.Lights.Items[I]).Scale[1] := TLight(Scene.Lights.Items[I]).Scale[1] / 1.1;// - Delta;
          TLight(Scene.Lights.Items[I]).Scale[2] := TLight(Scene.Lights.Items[I]).Scale[2] / 1.1;// - Delta;
          TLight(Scene.Lights.Items[I]).Scale[3] := TLight(Scene.Lights.Items[I]).Scale[3] / 1.1;// - Delta;
        End;
  }      
      End;
    End; // Case
  End;
End; // TScene3D.ProcessAction

//------------------------------------------------------------------------------
//  restore default parameters
//------------------------------------------------------------------------------
Procedure TScene3D.DefaultPosition(DoRedraw: Boolean);
Var I: Integer;
Begin

  If Scene <> Nil Then
  Begin
    For I := 0 To Scene.Entities.Count - 1 Do
    Begin
//      TEntity(Scene.Entities[I]).LocalPosition.Copy(0,0,0);
      TEntity(Scene.Entities[I]).LocalRotation.Copy(0,0,0);
//      TEntity(Scene.Entities[I]).LocalScale.Copy(1,1,1);
    End; // For I
    If DoRedraw Then Redraw;
  End;  
End; // TScene3D.DefaultPosition

//------------------------------------------------------------------------------
// remove all frames
//------------------------------------------------------------------------------
Procedure TScene3D.ClearScene;
Begin
  If Scene <> Nil Then
  Begin
    If bInitialized Then Scene.ClearScene(False,False);
    CurFrame  := -1;
    LastFrame := -1;
    FitCX     := 0;
    FitCY     := 0;
    FitCZ     := 0;
    FitKX     := 1;
    FitKY     := 1;
    FitKZ     := 1;
  End;
End; // TScene3D.ClearScene

//------------------------------------------------------------------------------
// Saves current frame to BMP file
//------------------------------------------------------------------------------
Procedure TScene3D.SaveFrameToBMP(FileName: String);
Var
  BMP        : TBitmap;
  TempCanvas : TCanvas;
  DestRect   : TRect;

Begin
  Redraw;
  BMP := TBitmap.Create;
  With BMP Do
  Begin
    Height   := FHeight;//ImagePanel.ClientRect.Bottom - ImagePanel.ClientRect.Top  + 1;
    Width    := FWidth;//ImagePanel.ClientRect.Right  - ImagePanel.ClientRect.Left + 1;
    DestRect := Rect(0,0,FWidth,FHeight);
  End;
  TempCanvas        := TCanvas.Create;
  TempCanvas.Handle := GetDC(FWindowHandle);//ImagePanel.Handle);
  BMP.Canvas.CopyRect(DestRect, TempCanvas, DestRect);//ImagePanel.ClientRect);
  BMP.SaveToFile(FileName);
  BMP.Free;
  TempCanvas.Free;
End; // TScene3D.SaveFrameToBMP

Procedure TScene3D.SetEntityVisible(B: Boolean);
Var I: Integer;
Begin
  If Scene <> Nil Then
  Begin
    For I := 0 To Scene.Entities.Count - 1 Do TEntity(Scene.Entities[I]).Visible := B;
  End;  
End; // TScene3D.SetEntityVisible

Procedure TScene3D.CalculateClickRay(X,Y: Integer; Var DestX,DestY,DestZ: Single);
// For the scene's default camera, calculates a point in space corresponding to an
// X,Y position in the render pane.
Var
  W,H       : Integer;
  U,V       : Single;
  U1,V1     : Single;
  Right     : T3DPoint{I3dPoint};
  Up        : T3DPoint{I3dPoint};
  Fwd       : T3DPoint{I3dPoint};
  Obs       : T3DPoint{I3dPoint};
  Dest      : T3DPoint{I3dPoint};

Begin
  If Scene <> Nil Then
  Begin
    W  := FWidth;
    H  := FHeight;
    V  := Scene.DistNear * Tan(Scene.Angle * (Pi / 180) / 2);
    U  := V * W / H;
    U1 := U * ((X - (W / 2)) / (W / 2));
    V1 := V * ((Y - (H / 2)) / (H / 2));

    // Slight fudge factor to put it just inside the near clipping plane

    U1 := U1 * 1.1;
    V1 := V1 * 1.1;

    Obs := T3DPoint.Create{Point}(Scene.DefaultCamera.Position);

    Fwd := T3DPoint.Create{Point}(Scene.DefaultCamera.SceneCenter.X,
                                Scene.DefaultCamera.SceneCenter.Y,
                                Scene.DefaultCamera.SceneCenter.Z);
    Fwd.Subtract(Obs);
    Fwd.Normalize;

    Up := T3DPoint.Create{Point}(Scene.DefaultCamera.UpVector.X,
                               Scene.DefaultCamera.UpVector.Y,
                               Scene.DefaultCamera.UpVector.Z);
    Up.Normalize;

    Right := T3DPoint.Create{Point}(Fwd);
    Right.Cross(Up);
    Right.Normalize;

    Dest := T3DPoint.Create{Point}(Right);
    Dest.Multiply(U1);
    Dest.Add(Obs);
    Up.Multiply(-V1);
    Dest.Add(Up);

    Fwd.Multiply(Scene.DistNear * 1.1);

    Dest.Add(Fwd);

    DestX := Dest.X;
    DestY := Dest.Y;
    DestZ := Dest.Z;

    // Cleanup

{    Dest.Free;
    Right.Free;
    Up.Free;
    Fwd.Free;
    Obs.Free;}
  End;
End; // TScene3D.CalculateClickRay

////////////////////////////////////////////////////////////////////////////////
Procedure Register;
Begin
  RegisterComponents('Samples', [TGLVisir]);
End; // Register

Procedure TGLVisir.btnLeft2MouseDown(Sender: TObject;
 Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  If RotateMode Then Scene3D.ModifyAction := RotateLeft2;
End;

Procedure TGLVisir.btnRight2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  If RotateMode Then Scene3D.ModifyAction := RotateRight2;
End;

procedure TGLVisir.acInfoExecute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TGLVisir.acRotateExecute(Sender: TObject);
begin
  RotateMode := True;
end;

procedure TGLVisir.acMoveExecute(Sender: TObject);
begin
  RotateMode := False;
end;

procedure TGLVisir.acDefaultExecute(Sender: TObject);
begin
  Scene3D.ModifyAction := None;
  Scene3D.DefaultPosition(True);
end;

procedure TGLVisir.acOptionsExecute(Sender: TObject);
begin
  Options3DForm.Call(Self);
end;

procedure TGLVisir.acPlayExecute(Sender: TObject);
begin
  Scene3D.SetFrameByNum(0);
  acStop.Enabled := True;
  SetAnimationPanel;
end;

procedure TGLVisir.acStopExecute(Sender: TObject);
begin
  acPlay.Checked := False;
  acStop.Enabled := False;
  SetAnimationPanel;
end;

procedure TGLVisir.acRewindExecute(Sender: TObject);
begin
  Scene3D.SetPrevFrame;
  SetAnimationPanel;
  Redraw;
end;

procedure TGLVisir.acFastForwardExecute(Sender: TObject);
begin
  Scene3D.SetNextFrame;
  SetAnimationPanel;
  Redraw;
end;

procedure TGLVisir.acRepeatExecute(Sender: TObject);
begin
  Scene3D.bCycledAnimation := True;
  SetAnimationPanel;
end;

procedure TGLVisir.acNoRepeatExecute(Sender: TObject);
begin
  Scene3D.bCycledAnimation := False;
  SetAnimationPanel;
end;

procedure TGLVisir.acShowAllAnimExecute(Sender: TObject);
begin
  Scene3D.ShowAllFrames(acShowAllAnim.Checked);
  SetAnimationPanel;
  Redraw;
end;

procedure TGLVisir.acMoveUpExecute(Sender: TObject);
begin
  DummyProc;
end;

procedure TGLVisir.acMoveDownExecute(Sender: TObject);
begin
  DummyProc;
end;

procedure TGLVisir.acMoveLeftExecute(Sender: TObject);
begin
  DummyProc;
end;

procedure TGLVisir.acMoveRightExecute(Sender: TObject);
begin
  DummyProc;
end;

procedure TGLVisir.acRotateLeftExecute(Sender: TObject);
begin
  DummyProc;
end;

procedure TGLVisir.acRotateRightExecute(Sender: TObject);
begin
  DummyProc;
end;

procedure TGLVisir.acZoomInExecute(Sender: TObject);
begin
  DummyProc;
end;

procedure TGLVisir.acZoomOutExecute(Sender: TObject);
begin
  DummyProc;
end;

Procedure TGLVisir.SetDiapasones(St: String);
Begin
  DiapasonesPanel.Caption := St;
End; // TGLVisir.SetDiapasones

// -----------------------------
// TMakePanelThread
// -----------------------------

Procedure TMakePanelThread.Execute;
// Started by TGLVisir.Init
Begin
  If GLVisir.Scene3D.Scene <> Nil Then
  Begin
    GLVisir.Scene3D.Scene.MakeImagePanel(@GLVisir.ImagePanel);
    GLVisir.Scene3D.FWindowHandle := GLVisir.Scene3D.Scene.WindowHandle;
    Try
      MPM.Enter;

      GLVisir.Scene3D.Scene.InitDC;
      GLVisir.Scene3D.Scene.InitRC;
      GLVisir.Scene3D.Scene.SetActive(True);

      While Not GLVisir.Scene3D.Scene.Active Do Sleep(100);
      Sleep(100); // Just to make sure the driver is ready
      GLVisir.Scene3D.Init;
      GLVisir.bInitialized  := True;
      GLVisir.bInitializing := False;

      LogToFile('TGLVisir.Init(' + GLVisir.Name + '): is now initialized');

      RepaintAll;
  //    GLVisir.Redraw;
    Finally
      MPM.Leave;
      GLVisir.MPTDone := 2;
    End;
  End
  Else GLVisir.MPTDone := 2;
End; // TMakePanelThread.Execute

Initialization
  WHandle              := 0;
  CreateDummyFinished  := False;
  Try
    MPM := TCriticalSection.Create;
  Except
    MPM := Nil;
  End;
  Try
    InitializingGLVisirs := TThreadSafeList.Create;
  Except
    InitializingGLVisirs := Nil;
  End;
Finalization
  MPM.Free;
  InitializingGLVisirs.Clear;
  InitializingGLVisirs.Free;
End.

