unit frmTexPickerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ZoneClasses, ComCtrls, Grids,
  ImgList, ToolWin, MPlayer, MMSystem, GLVisir, URickGL, SharedComboBox,
  GLPanel, ClearTypeText, OZDMUnit, IAeverButton;

Const
  BlitTexSize             = 64;
  TexSize                 = 24;
  MaxUndo                 = 20;
  WM_FINISHED             = WM_USER + $200;
  ciSelected              = 'SELECTED';
  DefaultTopographicColor = $000080FF; // Orange
  ClipTopographicColor    = $00FF0000; // Blue

type
  TIsOnEvent = Function(X,Y: Integer; Exclude: Integer = -1): Integer Of Object;
  TConfigurableItems = Class
    AllowMultipleSelection : Boolean;
    SelectedIndex          : Integer;
    SelectedCount          : Integer;
    EndPoint               : Integer;
    Adding                 : Boolean;
    Items                  : TStringList;
    Undo                   : Array[0..MaxUndo - 1] Of TStringList;
    UndoBase               : Integer;
    UndoLevel              : Integer;
    UndoMax                : Integer;
    Changed                : Boolean;
    CanUndo                : Boolean;
    CanRedo                : Boolean;
    SelectButton           : TToolButton;
    UndoButton             : TControl;
    RedoButton             : TControl;
    SnapToGridButton       : TToolButton;
    SnapToOthersButton     : TToolButton;
    MouseDownX             : Integer;
    MouseDownY             : Integer;
    MouseMoveX             : Integer;
    MouseMoveY             : Integer;
    DownOnSelectedItems    : Boolean;
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   Clear;
    Procedure   Init;
    Procedure   DeleteSelected;
    Procedure   CopyToUndo;
    Procedure   CopyFromUndo;
    Procedure   AddUndo;
    Procedure   DoUndo;
    Procedure   DoRedo;
    Function    InSelectionMode: Boolean;
    Function    SnapToGridIsOn: Boolean;
    Function    SnapToOthersIsOn: Boolean;
    Function    IsOn(X,Y: Integer; Exclude: Integer = -1): Integer; Dynamic; Abstract;
    Function    CheckSelected(X,Y: Integer): Boolean; Dynamic; Abstract;
    Procedure   DoSelectAt(X,Y: Integer); Dynamic; Abstract;
    Procedure   DoCreateAt(X,Y: Single); Dynamic; Abstract;
    Procedure   DoMouseDown(X,Y: Integer);
    Procedure   DoMouseMove(X,Y: Integer);
    Procedure   DoMouseUp(X,Y: Integer);
    Procedure   DoSnapToOther(Index: Integer; Var X,Y: Single); Dynamic;
    Function    DoSetPoint(X0,Y0,X,Y: Single): Boolean; Dynamic; Abstract;
    Procedure   Draw; Dynamic; Abstract;
    Procedure   ShowSelectedParameters; Dynamic;
    Procedure   ClearSelections;
    Procedure   DoBeforeDelete(Index: Integer); Dynamic;
    Function    CanCreateQuery: Boolean; Dynamic;
    Procedure   SelectInRectangle(X1,Y1,X2,Y2: Integer); Dynamic;
    Procedure   DrawSelectionRectangle(RepaintFirst: Boolean);
  End;
  TConfigurableBounds = Class(TConfigurableItems)
    Function    IsOn(X,Y: Integer; Exclude: Integer = -1): Integer; Override;
    Function    CheckSelected(X,Y: Integer): Boolean; Override;
    Procedure   DoSelectAt(X,Y: Integer); Override;
    Procedure   DoCreateAt(X,Y: Single); Override;
    Procedure   DoSnapToOther(Index: Integer; Var X,Y: Single); Override;
    Function    DoSetPoint(X0,Y0,X,Y: Single): Boolean; Override;
    Procedure   Draw; Override;
  End;
  TConfigurableZonePlanes = Class(TConfigurableItems)
    Function    IsOn(X,Y: Integer; Exclude: Integer = -1): Integer; Override;
    Function    CheckSelected(X,Y: Integer): Boolean; Override;
    Procedure   DoSelectAt(X,Y: Integer); Override;
    Procedure   DoCreateAt(X,Y: Single); Override;
    Procedure   DoSnapToOther(Index: Integer; Var X,Y: Single); Override;
    Function    DoSetPoint(X0,Y0,X,Y: Single): Boolean; Override;
    Procedure   Draw; Override;
    Procedure   ShowSelectedParameters; Override;
  End;
  TConfigurableMeshes = Class(TConfigurableItems)
    MeshCreateIndex : TStringList;
    Constructor Create;
    Destructor  Destroy; Override;
    Function    IsOn(X,Y: Integer; Exclude: Integer = -1): Integer; Override;
    Function    CheckSelected(X,Y: Integer): Boolean; Override;
    Procedure   DoSelectAt(X,Y: Integer); Override;
    Procedure   DoCreateAt(X,Y: Single); Override;
    Function    DoSetPoint(X0,Y0,X,Y: Single): Boolean; Override;
    Procedure   Draw; Override;
    Procedure   ShowSelectedParameters; Override;
    Procedure   DoBeforeDelete(Index: Integer); Override;
    Function    CanCreateQuery: Boolean; Override;
    Procedure   SelectInRectangle(X1,Y1,X2,Y2: Integer); Override;
  End;
  TConfigurableSounds = Class(TConfigurableItems)
    Function    IsOn(X,Y: Integer; Exclude: Integer = -1): Integer; Override;
    Function    CheckSelected(X,Y: Integer): Boolean; Override;
    Procedure   DoSelectAt(X,Y: Integer); Override;
    Procedure   DoCreateAt(X,Y: Single); Override;
    Function    DoSetPoint(X0,Y0,X,Y: Single): Boolean; Override;
    Procedure   Draw; Override;
    Procedure   ShowSelectedParameters; Override;
    Function    CanCreateQuery: Boolean; Override;
  End;
  TConfigurableClipElevation = Class(TConfigurableItems)
    X1 : Single;
    X2 : Single;
    Y1 : Single;
    Y2 : Single;
    Z  : Single;
    Function    IsOn(X,Y: Integer; Exclude: Integer = -1): Integer; Override;
    Function    CheckSelected(X,Y: Integer): Boolean; Override;
    Procedure   DoSelectAt(X,Y: Integer); Override;
    Procedure   DoCreateAt(X,Y: Single); Override;
    Function    DoSetPoint(X0,Y0,X,Y: Single): Boolean; Override;
    Procedure   Draw; Override;
    Function    CanCreateQuery: Boolean; Override;
  End;
  PLongWord = ^LongWord;
  TLongArray = Packed Array[0..TexSize * TexSize - 1] Of LongInt;
  PLongArray = ^TLongArray;
  TfrmTexPicker = class(TGLForm)
    grpCurrentTexture: TClearTypeGroupBox;
    imgTexture: TImage;
    btnRotateLeft: TSpeedButton;
    btnRotateRight: TSpeedButton;
    GroupBox2: TClearTypeGroupBox;
    lbTextures: TClearTypeListBox;
    pnlLeft: TPanel;
    Splitter1: TSplitter;
    pnlBottom: TPanel;
    btnOk: TIAEverButton;
    sbHorizontal: TScrollBar;
    sbVertical: TScrollBar;
    rbLand: TClearTypeRadioButton;
    rbWater: TClearTypeRadioButton;
    btnCancel: TIAEverButton;
    pcGroundEditor: TPageControl;
    tbNames: TTabSheet;
    tbIcons: TTabSheet;
    dgTextures: TDrawGrid;
    cbShowBounds: TClearTypeCheckBox;
    tbBounds: TTabSheet;
    ToolBar1: TToolBar;
    ilBoundsEditor: TImageList;
    tbBoundsSelect: TToolButton;
    tbBoundsDraw: TToolButton;
    Panel2: TPanel;
    tbBoundsSnapToGrid: TToolButton;
    tbBoundsSnapToEndpoints: TToolButton;
    ToolButton1: TToolButton;
    ToolBar2: TToolBar;
    tbBoundsUndo: TToolButton;
    tbBoundsRedo: TToolButton;
    tbBoundsDelete: TToolButton;
    ToolButton5: TToolButton;
    tbMeshes: TTabSheet;
    ToolBar3: TToolBar;
    tbMeshesSelect: TToolButton;
    tbMeshPlace: TToolButton;
    ToolButton2: TToolButton;
    tbMeshDelete: TToolButton;
    Panel1: TPanel;
    Panel3: TPanel;
    Label2: TClearTypeLabel;
    lblSelectedMesh: TClearTypeLabel;
    cbVaryAngle: TClearTypeCheckBox;
    cbVarySize: TClearTypeCheckBox;
    lblCurrentTex: TClearTypeLabel;
    ToolButton3: TToolButton;
    tbMeshVarySize: TToolButton;
    tbMeshVaryAngle: TToolButton;
    gbShading: TClearTypeGroupBox;
    rbNoShading: TClearTypeRadioButton;
    rbShadeBySlope: TClearTypeRadioButton;
    rbShadeByElevation: TClearTypeRadioButton;
    tbZonePlanes: TTabSheet;
    ToolBar4: TToolBar;
    ToolBar5: TToolBar;
    tbZonePlanesSelect: TToolButton;
    tbZonePlanesDraw: TToolButton;
    ToolButton7: TToolButton;
    tbZonePlanesSnapToGrid: TToolButton;
    tbZonePlanesSnapToEndpoints: TToolButton;
    tbZonePlanesUndo: TToolButton;
    tbZonePlanesRedo: TToolButton;
    ToolButton12: TToolButton;
    tbZonePlanesDelete: TToolButton;
    cbZonePlaneInfiniteZ: TClearTypeCheckBox;
    Label4: TClearTypeLabel;
    edtZonePlaneMinZ: TClearTypeEdit;
    Label5: TClearTypeLabel;
    edtZonePlaneMaxZ: TClearTypeEdit;
    Label6: TClearTypeLabel;
    edtZonePlaneZoneID: TClearTypeEdit;
    Label7: TClearTypeLabel;
    edtZonePlaneDestX: TClearTypeEdit;
    Label8: TClearTypeLabel;
    edtZonePlaneDestY: TClearTypeEdit;
    Label9: TClearTypeLabel;
    edtZonePlaneDestZ: TClearTypeEdit;
    Label10: TClearTypeLabel;
    edtZonePlaneDestHeading: TClearTypeEdit;
    cbIgnoreZonePlaneDestX: TClearTypeCheckBox;
    cbIgnoreZonePlaneDestY: TClearTypeCheckBox;
    cbIgnoreZonePlaneDestZ: TClearTypeCheckBox;
    cbIgnoreZonePlaneDestHeading: TClearTypeCheckBox;
    Bevel1: TBevel;
    tbSounds: TTabSheet;
    lbSounds: TClearTypeListBox;
    ToolBar6: TToolBar;
    tbSoundsSelect: TToolButton;
    tbSoundsDraw: TToolButton;
    ToolButton8: TToolButton;
    tbSoundsSnapToGrid: TToolButton;
    ToolBar7: TToolBar;
    tbSoundsUndo: TToolButton;
    tbSoundsRedo: TToolButton;
    ToolButton14: TToolButton;
    tbSoundsDelete: TToolButton;
    ToolBar8: TToolBar;
    tbSoundsPlay: TToolButton;
    tbSoundsStop: TToolButton;
    ToolButton4: TToolButton;
    Panel4: TPanel;
    cbAreaSound: TClearTypeCheckBox;
    pnlSoundInfo: TPanel;
    ToolButton6: TToolButton;
    tbPlaySound: TToolButton;
    rbDaySound: TClearTypeRadioButton;
    rbNightSound: TClearTypeRadioButton;
    cbTopographic: TClearTypeCheckBox;
    cbShowOverlay: TClearTypeCheckBox;
    glvMesh: TGLVisir;
    Splitter2: TSplitter;
    Label3: TClearTypeLabel;
    btnHelp: TIAEverButton;
    GroupBox1: TClearTypeGroupBox;
    Label11: TClearTypeLabel;
    Label12: TClearTypeLabel;
    edtTop: TClearTypeEdit;
    edtBottom: TClearTypeEdit;
    cbInfinite: TClearTypeCheckBox;
    tbClipElevation: TTabSheet;
    tbElevation: TTrackBar;
    Label13: TClearTypeLabel;
    edtElevation: TClearTypeEdit;
    btnSplitPolys: TIAEverButton;
    Label14: TClearTypeLabel;
    Label15: TClearTypeLabel;
    btnBrowseUpper: TIAEverButton;
    btnBrowseLower: TIAEverButton;
    Bevel2: TBevel;
    Bevel3: TBevel;
    cbUpperTexture: TComboBox;
    cbLowerTexture: TComboBox;
    cbShowHiddenGridElements: TClearTypeCheckBox;
    Panel5: TPanel;
    Label16: TClearTypeLabel;
    Panel6: TPanel;
    Label17: TClearTypeLabel;
    gbPaintOn: TClearTypeGroupBox;
    gbShow: TClearTypeGroupBox;
    tvMeshes: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure lbTexturesClick(Sender: TObject);
    procedure btnRotateLeftClick(Sender: TObject);
    procedure btnRotateRightClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure sbHorizontalChange(Sender: TObject);
    procedure sbVerticalChange(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure dgTexturesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure dgTexturesClick(Sender: TObject);
    procedure lbTexturesDblClick(Sender: TObject);
    procedure dgTexturesDblClick(Sender: TObject);
    procedure cbShowBoundsClick(Sender: TObject);
    procedure pcGroundEditorChange(Sender: TObject);
    procedure tbBoundsDeleteClick(Sender: TObject);
    procedure tbBoundsUndoClick(Sender: TObject);
    procedure tbBoundsRedoClick(Sender: TObject);
    procedure tbMeshDeleteClick(Sender: TObject);
    procedure tbMeshVarySizeClick(Sender: TObject);
    procedure tbMeshVaryAngleClick(Sender: TObject);
    procedure rbNoShadingClick(Sender: TObject);
    procedure rbShadeBySlopeClick(Sender: TObject);
    procedure rbShadeByElevationClick(Sender: TObject);
    procedure tbZonePlanesDeleteClick(Sender: TObject);
    procedure tbZonePlanesUndoClick(Sender: TObject);
    procedure tbZonePlanesRedoClick(Sender: TObject);
    procedure cbZonePlaneInfiniteZClick(Sender: TObject);
    procedure edtZonePlaneMinZChange(Sender: TObject);
    procedure edtZonePlaneMaxZChange(Sender: TObject);
    procedure edtZonePlaneZoneIDChange(Sender: TObject);
    procedure edtZonePlaneDestXChange(Sender: TObject);
    procedure edtZonePlaneDestYChange(Sender: TObject);
    procedure edtZonePlaneDestZChange(Sender: TObject);
    procedure edtZonePlaneDestHeadingChange(Sender: TObject);
    procedure cbIgnoreZonePlaneDestXClick(Sender: TObject);
    procedure cbIgnoreZonePlaneDestYClick(Sender: TObject);
    procedure cbIgnoreZonePlaneDestZClick(Sender: TObject);
    procedure cbIgnoreZonePlaneDestHeadingClick(Sender: TObject);
    procedure tbSoundsStopClick(Sender: TObject);
    procedure tbSoundsPlayClick(Sender: TObject);
    procedure tbSoundsUndoClick(Sender: TObject);
    procedure tbSoundsRedoClick(Sender: TObject);
    procedure tbSoundsDeleteClick(Sender: TObject);
    procedure cbAreaSoundClick(Sender: TObject);
    procedure tbPlaySoundClick(Sender: TObject);
    procedure lbSoundsClick(Sender: TObject);
    procedure pnlSoundInfoResize(Sender: TObject);
    procedure cbTopographicClick(Sender: TObject);
    procedure cbShowOverlayClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure tbElevationChange(Sender: TObject);
    procedure edtElevationChange(Sender: TObject);
    procedure btnSplitPolysClick(Sender: TObject);
    procedure btnBrowseUpperClick(Sender: TObject);
    procedure btnBrowseLowerClick(Sender: TObject);
    procedure cbShowHiddenGridElementsClick(Sender: TObject);
    procedure tvMeshesChange(Sender: TObject; Node: TTreeNode);
    procedure tvMeshesCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure tvMeshesCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure Splitter2Moved(Sender: TObject);
  private
    { Private declarations }
    Repositioning       : Boolean;
    BlitBuffer          : PLongArray;
    BlitBufSize         : Integer;
    MaskBuffer          : PLongArray;
    MaskBufSize         : Integer;
    OverlayMaskBuffer   : PLongArray;
    OverlayMaskBufSize  : Integer;
    OverlayBuffer       : PLongArray;
    OverlayBufSize      : Integer;
    DestBuffer          : PLongArray;
    DestBufSize         : Integer;
    OldSBHPos           : Integer;
    OldSBVPos           : Integer;
    LTextures           : Array Of Integer; // Land textures
    WTextures           : Array Of Integer; // Water textures (underwater)
    TexVisible          : Array Of Boolean;
    CursorX             : Integer;
    CursorY             : Integer;
    Drawing             : Boolean;
    Angle               : Integer;
    PaintBMP            : TBitmap;
    BlitBMP             : TBitmap;
    SelectedTex         : Integer;
    boolChangingCEEdit  : Boolean;
    boolChangingCETB    : Boolean;
    ClipElevation       : TConfigurableClipElevation;
    SelectedMeshObject  : String;

    Bounds              : TConfigurableBounds;
    ZonePlanes          : TConfigurableZonePlanes;
    Meshes              : TConfigurableMeshes;
    Sounds              : TConfigurableSounds;

    OrigMeshes          : TStringList;
    MouseIsDown         : Boolean;

    LBMP                : TBitmap;
    WBMP                : TBitmap;

    SoundFiles          : Array Of Pointer;
    SoundSizes          : Array Of Integer;
    SoundIndexes        : Array Of Integer;

    // For WAV file playback

    fData               : PChar;
    fWaveHdr            : PWAVEHDR;
    fWaveOutHandle      : HWAVEOUT;

    // For topographic lines

    PolyMinZ            : Array of Single;
    PolyMaxZ            : Array of Single;
    ZoneMinZ            : Single;
    ZoneMaxZ            : Single;

    // Grid offsets

    GXOfs               : Integer;
    GYOfs               : Integer;

    // Used for non-ground polys

    Tree                : TTree;
    TreeMesh            : TMeshObject;

    Procedure PaintGridAt(X,Y: Integer);
    Procedure PositionScrollBars;
    Procedure RotateLeft(Src,Dest: TBitmap);
    Procedure RotateRight(Src,Dest: TBitmap);
    Procedure DrawDestBuffer;
    Procedure DrawAtDest(X,Y: Integer);
    Procedure FillInTextureArray;
    Procedure DropTexture;
    Procedure SaveChanges;
    Procedure SetupDrawGrid;
    Procedure DrawBounds;
    Procedure DrawMeshes;
    Procedure DrawZoneLines;
    Procedure DrawSounds;
    Function  ScreenXToZoneYCoord(X: Single): Single;
    Function  ScreenYToZoneXCoord(Y: Single): Single;
    Function  ZoneXToScreenYCoord(X: Single): Single;
    Function  ZoneYToScreenXCoord(Y: Single): Single;
    Function  ZoneToScreenDeltaCoord(Delta: Single): Single;
    Procedure SnapToGrid(Var X,Y: Single);
    Procedure PlayWAV(Stream: TMemoryStream);
    Procedure StopWAV;
    Procedure WaveOutProc(hwo: HWAVEOUT; uMsg: UINT; dwParam1,dwParam2: DWORD);
    Procedure WMFinished(Var Msg: TMessage); Message WM_FINISHED;
    Procedure LoadSounds;
    Procedure PaintTopographic(ClipX1,ClipY1,ClipX2,ClipY2: Integer; LineColor: TColor; MinZ,MaxZ: Single);
    Procedure LoadModelFromMesh;
    Procedure CalculateOverlay;
  public
    { Public declarations }
    MO1         : TMeshObject;  // Land
    MO2         : TMeshObject;  // Water (underwater)
    GridWidth   : Integer;
    GridHeight  : Integer;
    MinX        : Double; // Zone bounds
    MinY        : Double;
    MaxX        : Double;
    MaxY        : Double;
    GMinX       : Double; // Ground mesh bounds
    GMinY       : Double;
    GMaxX       : Double;
    GMaxY       : Double;
    ZGridWidth  : Integer;
    ZGridHeight : Integer;
    Procedure PaintGrid;
  end;

var
  frmTexPicker: TfrmTexPicker;

implementation

Uses frmStatusUnit,frmMainUnit,Math,S3D,U3DPolys, frmChooseTextureUnit, CommCtrl, Points3D;

Const
  XStep           = GridSize;
  YStep           = GridSize;
  SelectDist      = 8;
  SelectPointDist = 4;

Type
  TWAVInfo = Class
    WAVFileName : String;
    Index       : Integer;
  End;

{$R *.dfm}

// ----------------------------
// TConfigurableItems
// ----------------------------

Constructor TConfigurableItems.Create;
Var I: Integer;
Begin
  Items := TStringList.Create;
  For I := 0 To High(Undo) Do Undo[I] := TStringList.Create;
  SelectButton           := Nil;
  UndoButton             := Nil;
  RedoButton             := Nil;
  SnapToGridButton       := Nil;
  SnapToOthersButton     := Nil;
  AllowMultipleSelection := False;
  Init;
End; // TConfigurableItems.Create

Destructor TConfigurableItems.Destroy;
Var I: Integer;
Begin
  Clear;
  Items.Free;
  For I := 0 To High(Undo) Do Undo[I].Free;
End; // TConfigurableItems.Destroy

Procedure TConfigurableItems.Clear;
Var I,J: Integer;
Begin
  For I := 0 To Items.Count - 1 Do Items.Objects[I].Free;
  Items.Clear;
  For I := 0 To High(Undo) Do
  Begin
    For J := 0 To Undo[I].Count - 1 Do Undo[I].Objects[J].Free;
    Undo[I].Clear;
  End; // For I
End; // TConfigurableItems.Clear

Procedure TConfigurableItems.Init;
Begin
  Clear;
  SelectedIndex       := -1;
  SelectedCount       := 0;
  EndPoint            := 0;
  Adding              := False;
  UndoBase            := 0;
  UndoLevel           := 0;
  UndoMax             := -1;
  Changed             := False;
  CanUndo             := False;
  CanRedo             := False;
  MouseDownX          := 0;
  MouseDownY          := 0;
  MouseMoveX          := 0;
  MouseMoveY          := 0;
  DownOnSelectedItems := False;
  If Assigned(UndoButton) Then UndoButton.Enabled := CanUndo;
  If Assigned(RedoButton) Then RedoButton.Enabled := CanRedo;
End; // TConfigurableItems.Init

Procedure TConfigurableItems.ClearSelections;
Var I: Integer;
Begin
  SelectedIndex := -1;
  SelectedCount := 0;
  If AllowMultipleSelection Then
   For I := 0 To Items.Count - 1 Do Items.Strings[I] := '';
End; // TConfigurableItems.ClearSelections

Procedure TConfigurableItems.DeleteSelected;
Var I: Integer;
Begin
  If SelectedCount > 0 Then
  Begin
    If Assigned(UndoButton) Then AddUndo;
    If AllowMultipleSelection Then
    Begin
      I := 0;
      While I < Items.Count Do
      Begin
        If Items.Strings[I] <> '' Then // Any non-empty string designates the item as being selected
        Begin
          DoBeforeDelete(I);
          Items.Objects[I].Free;
          Items.Delete(I);
        End
        Else Inc(I);
      End; // While
    End
    Else
    Begin
      Items.Objects[SelectedIndex].Free;
      Items.Delete(SelectedIndex);
    End;
    ClearSelections;
    frmTexPicker.PaintGrid;
  End;
End; // TConfigurableItems.DeleteSelected

Procedure TConfigurableItems.CopyToUndo;
Var I: Integer;
Begin
  For I := 0 To Undo[UndoLevel].Count - 1 Do Undo[UndoLevel].Objects[I].Free;
  Undo[UndoLevel].Clear;
  For I := 0 To Items.Count - 1 Do Undo[UndoLevel].AddObject('',TConfigurableItem(Items.Objects[I]).MakeCopy);
End; // TConfigurableItems.CopyToUndo

Procedure TConfigurableItems.CopyFromUndo;
Var I: Integer;
Begin
  For I := 0 To Items.Count - 1 Do Items.Objects[I].Free;
  Items.Clear;

  // Add with an empty string so none are designated as selected

  For I := 0 To Undo[UndoLevel].Count - 1 Do Items.AddObject('',TConfigurableItem(Undo[UndoLevel].Objects[I]).MakeCopy); 
End; // TConfigurableItems.CopyFromUndo

Procedure TConfigurableItems.AddUndo;
Begin
  CopyToUndo;
  UndoMax   := UndoLevel;
  UndoLevel := (UndoLevel + 1) Mod MaxUndo;
  If UndoLevel = UndoBase Then UndoBase := (UndoBase + 1) Mod MaxUndo;
  CanUndo   := True;
  CanRedo   := False;
  If Assigned(UndoButton) Then UndoButton.Enabled := CanUndo;
  If Assigned(RedoButton) Then RedoButton.Enabled := CanRedo;
End; // TConfigurableItems.AddUndo

Procedure TConfigurableItems.DoUndo;
Begin
  If UndoLevel <> UndoBase Then
  Begin
    CopyToUndo;
    If Assigned(RedoButton) And Not CanRedo Then UndoMax := UndoLevel;
    Dec(UndoLevel);
    If UndoLevel < 0 Then UndoLevel := MaxUndo - 1;
    CopyFromUndo;
    CanUndo       := (UndoLevel <> UndoBase);
    CanRedo       := True;
    If Assigned(UndoButton) Then UndoButton.Enabled := CanUndo;
    If Assigned(RedoButton) Then RedoButton.Enabled := CanRedo;
    ClearSelections;
    EndPoint      := 0;
    Adding        := False;
    frmTexPicker.PaintGrid;
  End;
End; // TConfigurableItems.DoUndo

Procedure TConfigurableItems.DoRedo;
Var I: Integer;
Begin
  I := (UndoLevel + 1) Mod MaxUndo;
  If I <> UndoBase Then
  Begin
    UndoLevel     := I;
    CopyFromUndo;
    CanUndo       := True;
    CanRedo       := (UndoLevel <> UndoMax);
    If Assigned(UndoButton) Then UndoButton.Enabled := CanUndo;
    If Assigned(RedoButton) Then RedoButton.Enabled := CanRedo;
    ClearSelections;
    EndPoint      := 0;
    Adding        := False;
    frmTexPicker.PaintGrid;
  End;
End; // TConfigurableItems.DoRedo

Function TConfigurableItems.InSelectionMode: Boolean;
Begin
  If Assigned(SelectButton)
   Then Result := SelectButton.Down
   Else Result := True;
End; // TConfigurableItems.InSelectionMode

Function TConfigurableItems.SnapToGridIsOn: Boolean;
Begin
  If Assigned(SnapToGridButton)
   Then Result := SnapToGridButton.Down
   Else Result := False;
End; // TConfigurableItems.SnapToGridIsOn

Function TConfigurableItems.SnapToOthersIsOn: Boolean;
Begin
  If Assigned(SnapToOthersButton)
   Then Result := SnapToOthersButton.Down
   Else Result := False;
End; // TConfigurableItems.SnapToOthersIsOn

Procedure TConfigurableItems.DoMouseDown(X,Y: Integer);
Var X0,Y0: Single;
Begin
  EndPoint := 0;
  If InSelectionMode Then
  Begin
    MouseDownX := X;
    MouseDownY := Y;
    MouseMoveX := X;
    MouseMoveY := Y;

    // First check the currently selected one

    If SelectedCount > 0
     Then DownOnSelectedItems := CheckSelected(X,Y)
     Else DownOnSelectedItems := False;
    If Not DownOnSelectedItems Then DoSelectAt(X,Y);
  End
  Else
  Begin
    If CanCreateQuery Then
    Begin
      X0 := frmTexPicker.ScreenYToZoneXCoord(Y);
      Y0 := frmTexPicker.ScreenXToZoneYCoord(X);
      If SnapToGridIsOn Then frmTexPicker.SnapToGrid(X0,Y0);
      If Assigned(UndoButton) Then AddUndo;
      DoCreateAt(X0,Y0);
      ClearSelections;
      EndPoint      := 2;
      Adding        := True;
      Changed       := True;
      SelectedIndex := Items.Count - 1;
      SelectedCount := 1;
      If AllowMultipleSelection Then Items.Strings[SelectedIndex] := ciSelected;
    End;
  End;
End; // TConfigurableItems.DoMouseDown

Procedure TConfigurableItems.DoMouseMove(X,Y: Integer);
Var
  X0,Y0   : Single;
  X00,Y00 : Single;
  I       : Integer;

Begin
  If frmTexPicker.MouseIsDown And (InSelectionMode Or Adding) Then
  Begin
    If (SelectedCount > 0) And (EndPoint > 0) Then
    Begin
      X0  := frmTexPicker.ScreenYToZoneXCoord(Y);
      Y0  := frmTexPicker.ScreenXToZoneYCoord(X);
      X00 := frmTexPicker.ScreenYToZoneXCoord(MouseMoveY);
      Y00 := frmTexPicker.ScreenXToZoneYCoord(MouseMoveX);

      // Snap to grid

      If SnapToGridIsOn Then frmTexPicker.SnapToGrid(X0,Y0);

      // Snap to endpoints

      If SnapToOthersIsOn Then
      Begin
        I := IsOn(X,Y,SelectedIndex);
        If (I >= 0) And (I <> SelectedIndex) Then DoSnapToOther(I,X0,Y0);
      End;

      If Assigned(UndoButton) And Not Changed Then AddUndo;
      Changed := DoSetPoint(X00,Y00,X0,Y0);
      frmTexPicker.PaintGrid;
    End;
    MouseMoveX := X;
    MouseMoveY := Y;
    DrawSelectionRectangle(True);
  End;
End; // TConfigurableItems.DoMouseMove

Procedure TConfigurableItems.DoMouseUp(X,Y: Integer);
Var I: Integer;
Begin
  If InSelectionMode Then
  Begin
    If ((MouseDownX = MouseMoveX) And (MouseDownY = MouseMoveY)) Or Not AllowMultipleSelection
     Then I := IsOn(X,Y)
     Else I := -1;
    If Not Changed Then
    Begin
      If I >= 0 Then
      Begin
        ClearSelections;
        SelectedCount    := 1;
        SelectedIndex    := I;
        Items.Strings[I] := ciSelected;
      End
      Else SelectInRectangle(MouseDownX,MouseDownY,MouseMoveX,MouseMoveY);
    End;
  End;
  ShowSelectedParameters;
  Changed  := False;
  Draw;
  EndPoint := 0;
  Adding   := False;
  frmTexPicker.PaintGrid;
End; // TConfigurableItems.DoMouseUp

Procedure TConfigurableItems.DoSnapToOther(Index: Integer; Var X,Y: Single);
Begin
  // Do nothing in the base class
End; // TConfigurableItems.DoSnapToOther

Procedure TConfigurableItems.ShowSelectedParameters;
Begin
  // Do nothing in the base class
End; // TConfigurableItems.ShowSelectedParameters

Procedure TConfigurableItems.DoBeforeDelete(Index: Integer);
Begin
  // Do nothing in the base class
End; // TConfigurableItems.DoBeforeDelete

Function TConfigurableItems.CanCreateQuery: Boolean;
Begin
  Result := True;
End; // TConfigurableItems.CanCreateQuery

Procedure TConfigurableItems.SelectInRectangle(X1,Y1,X2,Y2: Integer);
Begin
  ClearSelections;
End; // TConfigurableItems.SelectInRectangle

Procedure TConfigurableItems.DrawSelectionRectangle(RepaintFirst: Boolean);
Var Region: THandle;
Begin
  // Draw the selection rectangle if we allow multiple selection

  If AllowMultipleSelection And frmTexPicker.MouseIsDown And InSelectionMode And Not DownOnSelectedItems Then
  Begin
    If RepaintFirst Then frmTexPicker.PaintGrid;

    // Clip to the drawing area

    Region := CreateRectRgn(0,0,0,0);
    SetRectRgn(Region,frmTexPicker.Splitter1.Left + frmTexPicker.Splitter1.Width,0,frmTexPicker.sbVertical.Left,frmTexPicker.sbHorizontal.Top);
    SelectClipRgn(frmTexPicker.Canvas.Handle,Region);

    // Draw the selection rectangle

    frmTexPicker.Canvas.Pen.Color   := clBlack;
    frmTexPicker.Canvas.Brush.Style := bsClear;
    frmTexPicker.Canvas.Pen.Style   := psDot;
    frmTexPicker.Canvas.Rectangle(MouseDownX,MouseDownY,MouseMoveX,MouseMoveY);
    frmTexPicker.Canvas.Brush.Style := bsSolid;
    frmTexPicker.Canvas.Pen.Style   := psSolid;

    // Go back to no clipping

    SelectClipRgn(frmTexPicker.Canvas.Handle,0);
    DeleteObject(Region);
  End;
End; // TConfigurableItems.DrawSelectionRectangle

// ----------------------------
// TConfigurableBounds
// ----------------------------

Function TConfigurableBounds.IsOn(X,Y: Integer; Exclude: Integer = -1): Integer;
Var
  I     : Integer;
  Found : Boolean;
  Dist  : Single;
  ZB    : TZoneBound;

Begin
  I     := 0;
  Found := False;
  While (I < Items.Count) And Not Found Do
  Begin
    If (Exclude < 0) Or (Exclude <> I) Then
    Begin
      ZB   := TZoneBound(Items.Objects[I]);
      Dist := Abs(DistanceFromLineSegment(frmTexPicker.ZoneYToScreenXCoord(ZB.Y1),
                                          frmTexPicker.ZoneXToScreenYCoord(ZB.X1),
                                          frmTexPicker.ZoneYToScreenXCoord(ZB.Y2),
                                          frmTexPicker.ZoneXToScreenYCoord(ZB.X2),X,Y));
      If Dist <= SelectDist Then Found := True Else Inc(I);
    End
    Else Inc(I);
  End; // While
  If Found Then Result := I Else Result := -1;
End; // TConfigurableBounds.IsOn

Function TConfigurableBounds.CheckSelected(X,Y: Integer): Boolean;
Var
  D  : Single;
  ZB : TZoneBound;
  
Begin
  Result := False;
  ZB     := TZoneBound(Items.Objects[SelectedIndex]);
  D      := Sqrt(Sqr(X - frmTexPicker.ZoneYToScreenXCoord(ZB.Y1)) +
                 Sqr(Y - frmTexPicker.ZoneXToScreenYCoord(ZB.X1)));
  If D < SelectPointDist Then
  Begin
    EndPoint := 1;
    Result   := True;
  End
  Else
  Begin
    D := Sqrt(Sqr(X - frmTexPicker.ZoneYToScreenXCoord(ZB.Y2)) +
              Sqr(Y - frmTexPicker.ZoneXToScreenYCoord(ZB.X2)));
    If D < SelectPointDist Then
    Begin
      EndPoint := 2;
      Result   := True;
    End;
  End;
End; // TConfigurableBounds.CheckSelected

Procedure TConfigurableBounds.DoSelectAt(X,Y: Integer);
Var
  I  : Integer;
  ZB : TZoneBound;

Begin
  I := IsOn(X,Y);
  If (I = SelectedIndex) And (I >= 0) Then CheckSelected(X,Y);
  If I >= 0 Then
  Begin
    ZB     := TZoneBound(Items.Objects[I]);
    frmTexPicker.cbInfinite.Checked := (ZB.Z1 = ZB.Z2);
    frmTexPicker.edtTop.Text        := FloatToStr(ZB.Z2);
    frmTexPicker.edtBottom.Text     := FloatToStr(ZB.Z1);
  End;
End; // TConfigurableBounds.DoSelectAt

Procedure TConfigurableBounds.DoCreateAt(X,Y: Single);
Var ZB: TZoneBound;
Begin
  ZB    := TZoneBound.Create;
  Items.AddObject('',ZB);
  ZB.X1 := X;
  ZB.Y1 := Y;
  ZB.X2 := X;
  ZB.Y2 := Y;
End; // TConfigurableBounds.DoCreateAt

Procedure TConfigurableBounds.DoSnapToOther(Index: Integer; Var X,Y: Single);
Var
  D     : Single;
  ZB    : TZoneBound;
  X0,Y0 : Single;

Begin
  ZB := TZoneBound(Items.Objects[Index]);
  X0 := frmTexPicker.ZoneYToScreenXCoord(Y);
  Y0 := frmTexPicker.ZoneXToScreenYCoord(X);
  D  := Sqrt(Sqr(X0 - frmTexPicker.ZoneYToScreenXCoord(ZB.Y1)) + Sqr(Y0 - frmTexPicker.ZoneXToScreenYCoord(ZB.X1)));
  If D < SelectDist Then
  Begin
    X := ZB.X1;
    Y := ZB.Y1;
  End
  Else
  Begin
    D := Sqrt(Sqr(X0 - frmTexPicker.ZoneYToScreenXCoord(ZB.Y2)) + Sqr(Y0 - frmTexPicker.ZoneXToScreenYCoord(ZB.X2)));
    If D < SelectDist Then
    Begin
      X := ZB.X2;
      Y := ZB.Y2;
    End;
  End;
End; // TConfigurableBounds.DoSnapToOther

Function TConfigurableBounds.DoSetPoint(X0,Y0,X,Y: Single): Boolean;
Begin
  If EndPoint = 1 Then
  Begin
    TZoneBound(Items.Objects[SelectedIndex]).X1 := X;
    TZoneBound(Items.Objects[SelectedIndex]).Y1 := Y;
  End
  Else
  Begin
    TZoneBound(Items.Objects[SelectedIndex]).X2 := X;
    TZoneBound(Items.Objects[SelectedIndex]).Y2 := Y;
  End;
  Result := True;
End; // TConfigurableBounds.DoSetPoint

Procedure TConfigurableBounds.Draw;
Begin
  frmTexPicker.DrawBounds;
  DrawSelectionRectangle(False);
End; // TConfigurableBounds.Draw

// ----------------------------
// TConfigurableZonePlanes
// ----------------------------

Function TConfigurableZonePlanes.IsOn(X,Y: Integer; Exclude: Integer = -1): Integer;
Var
  I     : Integer;
  Found : Boolean;
  Dist  : Single;
  ZP    : TZonePlane;

Begin
  I     := 0;
  Found := False;
  While (I < Items.Count) And Not Found Do
  Begin
    If (Exclude < 0) Or (Exclude <> I) Then
    Begin
      ZP   := TZonePlane(Items.Objects[I]);
      Dist := Abs(DistanceFromLineSegment(frmTexPicker.ZoneYToScreenXCoord(ZP.Y1),
                                          frmTexPicker.ZoneXToScreenYCoord(ZP.X1),
                                          frmTexPicker.ZoneYToScreenXCoord(ZP.Y2),
                                          frmTexPicker.ZoneXToScreenYCoord(ZP.X2),X,Y));
      If Dist <= SelectDist Then Found := True Else Inc(I);
    End
    Else Inc(I);
  End; // While
  If Found Then Result := I Else Result := -1;
End; // TConfigurableZonePlanes.IsOn

Function TConfigurableZonePlanes.CheckSelected(X,Y: Integer): Boolean;
Var D: Single;
Begin
  Result := False;
  D      := Sqrt(Sqr(X - frmTexPicker.ZoneYToScreenXCoord(TZonePlane(Items.Objects[SelectedIndex]).Y1)) +
                 Sqr(Y - frmTexPicker.ZoneXToScreenYCoord(TZonePlane(Items.Objects[SelectedIndex]).X1)));
  If D < SelectPointDist Then
  Begin
    EndPoint := 1;
    Result   := True;
  End
  Else
  Begin
    D := Sqrt(Sqr(X - frmTexPicker.ZoneYToScreenXCoord(TZonePlane(Items.Objects[SelectedIndex]).Y2)) +
              Sqr(Y - frmTexPicker.ZoneXToScreenYCoord(TZonePlane(Items.Objects[SelectedIndex]).X2)));
    If D < SelectPointDist Then
    Begin
      EndPoint := 2;
      Result   := True;
    End;
  End;
End; // TConfigurableZonePlanes.CheckSelected

Procedure TConfigurableZonePlanes.DoSelectAt(X,Y: Integer);
Var I: Integer;
Begin
  I := IsOn(X,Y);
  If (I = SelectedIndex) And (I >= 0) Then CheckSelected(X,Y);
End; // TConfigurableZonePlanes.DoSelectAt

Procedure TConfigurableZonePlanes.DoCreateAt(X,Y: Single);
Var ZP: TZonePlane;
Begin
  ZP              := TZonePlane.Create;
  Items.AddObject('',ZP);
  ZP.X1           := X;
  ZP.Y1           := Y;
  ZP.Z1           := 0;
  ZP.X2           := X;
  ZP.Y2           := Y;
  ZP.Z2           := 0;
  ZP.InfiniteZ    := True;
  ZP.DestZoneID   := 0;
  ZP.DestX        := 0;
  ZP.DestY        := 0;
  ZP.DestZ        := 0;
  ZP.DestAngle    := 0;
  ZP.HasDestX     := True;
  ZP.HasDestY     := True;
  ZP.HasDestZ     := True;
  ZP.HasDestAngle := True;
End; // TConfigurableZonePlanes.DoCreateAt

Procedure TConfigurableZonePlanes.DoSnapToOther(Index: Integer; Var X,Y: Single);
Var
  D     : Single;
  ZP    : TZonePlane;
  X0,Y0 : Single;

Begin
  ZP := TZonePlane(Items.Objects[Index]);
  X0 := frmTexPicker.ZoneYToScreenXCoord(Y);
  Y0 := frmTexPicker.ZoneXToScreenYCoord(X);
  D  := Sqrt(Sqr(X0 - frmTexPicker.ZoneYToScreenXCoord(ZP.Y1)) + Sqr(Y0 - frmTexPicker.ZoneXToScreenYCoord(ZP.X1)));
  If D < SelectDist Then
  Begin
    X := ZP.X1;
    Y := ZP.Y1;
  End
  Else
  Begin
    D := Sqrt(Sqr(X0 - frmTexPicker.ZoneYToScreenXCoord(ZP.Y2)) + Sqr(Y0 - frmTexPicker.ZoneXToScreenYCoord(ZP.X2)));
    If D < SelectDist Then
    Begin
      X := ZP.X2;
      Y := ZP.Y2;
    End;
  End;
End; // TConfigurableZonePlanes.DoSnapToOther

Function TConfigurableZonePlanes.DoSetPoint(X0,Y0,X,Y: Single): Boolean;
Begin
  If EndPoint = 1 Then
  Begin
    TZonePlane(Items.Objects[SelectedIndex]).X1 := X;
    TZonePlane(Items.Objects[SelectedIndex]).Y1 := Y;
  End
  Else
  Begin
    TZonePlane(Items.Objects[SelectedIndex]).X2 := X;
    TZonePlane(Items.Objects[SelectedIndex]).Y2 := Y;
  End;
  Result := True;
End; // TConfigurableZonePlanes.DoSetPoint

Procedure TConfigurableZonePlanes.Draw;
Begin
  frmTexPicker.DrawZoneLines;
  DrawSelectionRectangle(False);
End; // TConfigurableZonePlanes.Draw

Procedure TConfigurableZonePlanes.ShowSelectedParameters;
Var ZP: TZonePlane;
Begin
  With frmTexPicker Do
  Begin
    If SelectedIndex >= 0 Then
    Begin
      ZP                                   := TZonePlane(ZonePlanes.Items.Objects[SelectedIndex]);
      cbZonePlaneInfiniteZ.Checked         := ZP.InfiniteZ;
      edtZonePlaneMinZ.Text                := FloatToStr(ZP.Z1);
      edtZonePlaneMaxZ.Text                := FloatToStr(ZP.Z2);
      edtZonePlaneZoneID.Text              := IntToStr(ZP.DestZoneID);
      edtZonePlaneDestX.Text               := FloatToStr(ZP.DestX);
      edtZonePlaneDestY.Text               := FloatToStr(ZP.DestY);
      edtZonePlaneDestZ.Text               := FloatToStr(ZP.DestZ);
      edtZonePlaneDestHeading.Text         := IntToStr(ZP.DestAngle);
      cbIgnoreZonePlaneDestX.Checked       := Not ZP.HasDestX;
      cbIgnoreZonePlaneDestY.Checked       := Not ZP.HasDestY;
      cbIgnoreZonePlaneDestZ.Checked       := Not ZP.HasDestZ;
      cbIgnoreZonePlaneDestHeading.Checked := Not ZP.HasDestAngle;
    End
    Else
    Begin
      cbZonePlaneInfiniteZ.Checked         := True;
      edtZonePlaneMinZ.Text                := '0';
      edtZonePlaneMaxZ.Text                := '0';
      edtZonePlaneZoneID.Text              := '0';
      edtZonePlaneDestX.Text               := '0';
      edtZonePlaneDestY.Text               := '0';
      edtZonePlaneDestZ.Text               := '0';
      edtZonePlaneDestHeading.Text         := '0';
      cbIgnoreZonePlaneDestX.Checked       := False;
      cbIgnoreZonePlaneDestY.Checked       := False;
      cbIgnoreZonePlaneDestZ.Checked       := False;
      cbIgnoreZonePlaneDestHeading.Checked := False;
    End;
  End;
End; // TConfigurableZonePlanes.ShowSelectedParameters

// ----------------------------
// TConfigurableMeshes
// ----------------------------

Constructor TConfigurableMeshes.Create;
Begin
  Inherited;
  MeshCreateIndex        := TStringList.Create;
  MeshCreateIndex.Sorted := True;
End; // TConfigurableMeshes.Create

Destructor TConfigurableMeshes.Destroy;
Begin
  MeshCreateIndex.Free;
  Inherited;
End; // TConfigurableMeshes.Destroy

Function TConfigurableMeshes.IsOn(X,Y: Integer; Exclude: Integer = -1): Integer;
Var
  I         : Integer;
  Found     : Boolean;
  Dist      : Single;
  ML        : TMeshLibraryObjectReference;

Begin
  I     := 0;
  Found := False;
  While (I < Items.Count) And Not Found Do
  Begin
    If (Exclude < 0) Or (Exclude <> I) Then
    Begin
      ML   := TMeshLibraryObjectReference(Items.Objects[I]);
      Dist := Sqrt(Sqr(X - frmTexPicker.ZoneYToScreenXCoord(ML.Loc.Y)) +
                   Sqr(Y - frmTexPicker.ZoneXToScreenYCoord(ML.Loc.X)));
      If Dist <= SelectDist Then Found := True Else Inc(I);
    End
    Else Inc(I);
  End; // While
  If Found Then Result := I Else Result := -1;
End; // TConfigurableMeshes.IsOn

Function TConfigurableMeshes.CheckSelected(X,Y: Integer): Boolean;
Var
  ML    : TMeshLibraryObjectReference;
  I     : Integer;
  Found : Boolean;
  D     : Single;

Begin
  I     := 0;
  Found := False;
  While (I < Items.Count) And Not Found Do
  Begin
    If Items.Strings[I] <> '' Then
    Begin
      ML := TMeshLibraryObjectReference(Items.Objects[I]);
      D := Sqrt(Sqr(X - frmTexPicker.ZoneYToScreenXCoord(ML.Loc.Y)) +
                Sqr(Y - frmTexPicker.ZoneXToScreenYCoord(ML.Loc.X)));
      If D < SelectPointDist Then
      Begin
        Found    := True;
        EndPoint := 1;
      End
      Else Inc(I);
    End
    Else Inc(I);
  End; // While
  Result := Found;
End; // TConfigurableMeshes.CheckSelected

Procedure TConfigurableMeshes.DoSelectAt(X,Y: Integer);
Var I: Integer;
Begin
  ClearSelections;
  I := IsOn(X,Y);
  If I >= 0 Then
  Begin
    SelectedCount    := 1;
    Items.Strings[I] := ciSelected;
  End;
End; // TConfigurableMeshes.DoSelectAt

Procedure TConfigurableMeshes.DoCreateAt(X,Y: Single);
Var
  St  : String;
  ML  : TMeshLibraryObjectReference;
  I,J : Integer;

  Function IndexOfInItems(St: String): Integer;
  Var
    I     : Integer;
    Found : Boolean;

  Begin
    I     := 0;
    Found := False;
    While (I < Items.Count) And Not Found Do
    Begin
      If TMeshLibraryObjectReference(Items.Objects[I]).GetName = St Then Found := True Else Inc(I);
    End; // While
    If Found Then Result := I Else Result := -1;
  End; // IndexOf

Begin
  St := frmTexPicker.SelectedMeshObject; //frmTexPicker.lbMeshes.Items.Strings[frmTexPicker.lbMeshes.ItemIndex];
  ML := TMeshLibraryObjectReference.Create(TGroupObject(MeshLibrary.Objects[MeshLibraryObjectIndex(St)]));

  J := MeshCreateIndex.IndexOf(St);
  If J >= 0
   Then I := Integer(MeshCreateIndex.Objects[J]) + 1
   Else I := 1;

  While frmMain.Zone.NameExists(St + IntToStr(I)) Or
        (IndexOfInItems(St + IntToStr(I)) >= 0)   Do Inc(I);

  If J < 0
   Then MeshCreateIndex.AddObject(St,Pointer(I))
   Else MeshCreateIndex.Objects[J] := Pointer(I);

  ML.SetName(St + IntToStr(I));
  ML.Loc.X := X;
  ML.Loc.Y := Y;

  // Trees aren't always perfectly vertical and they can be at any Z angle

  If frmTexPicker.cbVaryAngle.Checked Then
  Begin
    ML.Rotate.XAngle := ML.Rotate.XAngle + (Random * 6 - 3);
    ML.Rotate.YAngle := ML.Rotate.YAngle + (Random * 6 - 3);
    ML.Rotate.ZAngle := Random * 360;
  End;

  // Many trees are younger than normal and some are larger than average

  If frmTexPicker.cbVarySize.Checked Then ML.Size.Multiply(0.6 + Random * 0.5);
  Items.AddObject('',ML);
  frmTexPicker.lblSelectedMesh.Caption := St + IntToStr(I);
End; // TConfigurableMeshes.DoCreateAt

Function TConfigurableMeshes.DoSetPoint(X0,Y0,X,Y: Single): Boolean;
Var
  I  : Integer;
  ML : TMeshLibraryObjectReference;

Begin
  Result := False;
  For I := 0 To Items.Count - 1 Do
  Begin
    If Items.Strings[I] <> '' Then
    Begin
      ML     := TMeshLibraryObjectReference(Items.Objects[I]);
      Result := True;
      If InSelectionMode Then
      Begin
        ML.Loc.X := ML.Loc.X + (X - X0);
        ML.Loc.Y := ML.Loc.Y + (Y - Y0);
      End
      Else
      Begin
        ML.Loc.X := X;
        ML.Loc.Y := Y;
      End;
    End;
  End; // For I
End; // TConfigurableMeshes.DoSetPoint

Procedure TConfigurableMeshes.Draw;
Begin
  frmTexPicker.DrawMeshes;
  DrawSelectionRectangle(False);
End; // TConfigurableMeshes.Draw

Procedure TConfigurableMeshes.ShowSelectedParameters;
Var
  I     : Integer;
  Found : Boolean;
  St    : String;
  ML    : TMeshLibraryObjectReference;

Begin
  If SelectedCount = 1 Then
  Begin
    I     := 0;
    Found := False;
    While (I < Items.Count) And Not Found Do
    Begin
      If Items.Strings[I] <> '' Then Found := True Else Inc(I);
    End; // While
    If Found
     Then frmTexPicker.lblSelectedMesh.Caption := TMeshLibraryObjectReference(Items.Objects[I]).Group.GetName
     Else frmTexPicker.lblSelectedMesh.Caption := '(none)';
  End
  Else
  Begin
    // If all of the selected objects are of the same type the display it

    St    := '';
    I     := 0;
    Found := False;
    While (I < Items.Count) And Not Found Do
    Begin
      If Items.Strings[I] <> '' Then
      Begin
        ML := TMeshLibraryObjectReference(Items.Objects[I]);
             If St = ''                Then St := ML.Group.GetName
        Else If St <> ML.Group.GetName Then
        Begin
          Found := True;
          St    := '(multiple types)';
        End;
      End;
      Inc(I);
    End; // While
    If St = '' Then St := '(none)';
    frmTexPicker.lblSelectedMesh.Caption := St;
  End;
End; // TConfigurableMeshes.ShowSelectedParameters

Procedure TConfigurableMeshes.DoBeforeDelete(Index: Integer);
Begin
  If Index < frmTexPicker.OrigMeshes.Count Then frmTexPicker.OrigMeshes.Delete(Index);
End; // TConfigurableMeshes.DoBeforeDelete

Function TConfigurableMeshes.CanCreateQuery: Boolean;
Begin
  Result := (frmTexPicker.SelectedMeshObject <> ''); //(frmTexPicker.lbMeshes.Items.Count > 0) And (frmTexPicker.lbMeshes.ItemIndex >= 0);
End; // TConfigurableMeshes.CanCreateQuery

Procedure TConfigurableMeshes.SelectInRectangle(X1,Y1,X2,Y2: Integer);
Var
  I         : Integer;
  ML        : TMeshLibraryObjectReference;
  X,Y       : Single;

Begin
  If X2 < X1 Then
  Begin
    I  := X2;
    X2 := X1;
    X1 := I;
  End;
  If Y2 < Y1 Then
  Begin
    I  := Y2;
    Y2 := Y1;
    Y1 := I;
  End;
  ClearSelections;
  For I := 0 To Items.Count - 1 Do
  Begin
    ML := TMeshLibraryObjectReference(Items.Objects[I]);
    X  := frmTexPicker.ZoneYToScreenXCoord(ML.Loc.Y);
    Y  := frmTexPicker.ZoneXToScreenYCoord(ML.Loc.X);
    If (X >= X1 - 2) And (Y >= Y1 - 2) And
       (X <= X2 + 2) And (Y <= Y2 + 2) Then
    Begin
      Inc(SelectedCount);
      Items.Strings[I] := ciSelected;
    End;
  End; // For I
End; // TConfigurableMeshes.SelectInRectangle

// ----------------------------
// TConfigurableSounds
// ----------------------------

Function TConfigurableSounds.IsOn(X,Y: Integer; Exclude: Integer): Integer;
Var
  I     : Integer;
  Found : Boolean;
  Dist  : Single;
  S     : TSound;

Begin
  I     := 0;
  Found := False;
  While (I < Items.Count) And Not Found Do
  Begin
    If (Exclude < 0) Or (Exclude <> I) Then
    Begin
      S    := TSound(Items.Objects[I]);
      Dist := Sqrt(Sqr(X - frmTexPicker.ZoneYToScreenXCoord(S.Y)) +
                   Sqr(Y - frmTexPicker.ZoneXToScreenYCoord(S.X)));
      If (Dist <= SelectDist) Or
         (Abs(Dist - frmTexPicker.ZoneToScreenDeltaCoord(S.Radius)) <= SelectDist)
       Then Found := True
       Else Inc(I);
    End
    Else Inc(I);
  End; // While
  If Found Then Result := I Else Result := -1;
End; // TConfigurableSounds.IsOn

Function TConfigurableSounds.CheckSelected(X,Y: Integer): Boolean;
Var
  Dist : Single;
  S    : TSound;

Begin
  Result := False;
  S      := TSound(Items.Objects[SelectedIndex]);
  Dist   := Sqrt(Sqr(X - frmTexPicker.ZoneYToScreenXCoord(S.Y)) +
                 Sqr(Y - frmTexPicker.ZoneXToScreenYCoord(S.X)));
  If Dist < SelectPointDist Then
  Begin
    EndPoint := 1;
    Result   := True;
  End
  Else If Abs(Dist - frmTexPicker.ZoneToScreenDeltaCoord(S.Radius)) <= SelectDist Then
  Begin
    EndPoint := 2;
    Result   := True;
  End;
End; // TConfigurableSounds.CheckSelected

Procedure TConfigurableSounds.DoSelectAt(X,Y: Integer);
Var I,J: Integer;
Begin
  I := IsOn(X,Y);
  If (I = SelectedIndex) And (I >= 0) Then CheckSelected(X,Y);

  // Select the sound name in the list box and try to make the name visible

  If I >= 0 Then
  Begin
    If frmTexPicker.rbDaySound.Checked
     Then I := frmTexPicker.lbSounds.Items.IndexOf(TSound(Items.Objects[I]).DayName)
     Else I := frmTexPicker.lbSounds.Items.IndexOf(TSound(Items.Objects[I]).NightName);
    If I >= 0 Then
    Begin
      frmTexPicker.lbSounds.ItemIndex := I;
      If I < frmTexPicker.lbSounds.TopIndex Then frmTexPicker.lbSounds.TopIndex := I
      Else
      Begin
        J := frmTexPicker.lbSounds.ClientHeight Div frmTexPicker.lbSounds.ItemHeight;
        If frmTexPicker.lbSounds.TopIndex < I - J + 1 Then frmTexPicker.lbSounds.TopIndex := I - J + 1;
      End;
    End
    Else
    Begin
      frmTexPicker.lbSounds.ItemIndex := 0;
      frmTexPicker.lbSounds.TopIndex := 0;
    End;
  End;
End; // TConfigurableSounds.DoSelectAt

Procedure TConfigurableSounds.DoCreateAt(X,Y: Single);
Var
  S            : TSound;
  HighestPoint : T3DPoint;
  W1,W2,W3     : T3DPoint;
  MinPt        : T3DPoint;
  MaxPt        : T3DPoint;

Begin
  S        := TSound.Create;
  Items.AddObject('',S);
  If frmTexPicker.lbSounds.ItemIndex > 0
   Then S.DayName := frmTexPicker.lbSounds.Items[frmTexPicker.lbSounds.ItemIndex]
   Else S.DayName := '';
  S.Area   := True;
  S.X      := X;
  S.Y      := Y;

  If frmMain.Zone.ElevationGrid.CanGetHeightAtAbsolute(X,Y) Then S.Z := frmMain.Zone.ElevationGrid.GetHeightAtAbsolute(X,Y)
  Else If frmTexPicker.Tree <> Nil Then
  Begin
    HighestPoint := T3DPoint.Create;
    W1           := T3DPoint.Create;
    W2           := T3DPoint.Create;
    W3           := T3DPoint.Create;
    MinPt        := T3DPoint.Create;
    MaxPt        := T3DPoint.Create;
    frmTexPicker.Tree.Root.Mesh.GetBounds(MinPt,MaxPt);
    frmTexPicker.Tree.GetMaxHeightAt(X,Y,HighestPoint,W1,W2,W3,MinPt.Z - 1,MaxPt.Z + 1);
    S.Z          := HighestPoint.Z;
    HighestPoint.Free;
    W1.Free;
    W2.Free;
    W3.Free;
    MinPt.Free;
    MaxPt.Free;
  End
  Else S.Z := 0;
  S.Radius := 64;
End; // TConfigurableSounds.DoCreateAt

Function TConfigurableSounds.DoSetPoint(X0,Y0,X,Y: Single): Boolean;
Var
  S            : TSound;
  HighestPoint : T3DPoint;
  W1,W2,W3     : T3DPoint;
  MinPt        : T3DPoint;
  MaxPt        : T3DPoint;

Begin
  S := TSound(Items.Objects[SelectedIndex]);
  If EndPoint = 1 Then
  Begin
    S.X := X;
    S.Y := Y;
    If frmMain.Zone.ElevationGrid.CanGetHeightAtAbsolute(X,Y) Then S.Z := frmMain.Zone.ElevationGrid.GetHeightAtAbsolute(X,Y)
    Else If frmTexPicker.Tree <> Nil Then
    Begin
      HighestPoint := T3DPoint.Create;
      W1           := T3DPoint.Create;
      W2           := T3DPoint.Create;
      W3           := T3DPoint.Create;
      MinPt        := T3DPoint.Create;
      MaxPt        := T3DPoint.Create;
      frmTexPicker.Tree.Root.Mesh.GetBounds(MinPt,MaxPt);
      frmTexPicker.Tree.GetMaxHeightAt(X,Y,HighestPoint,W1,W2,W3,MinPt.Z - 1,MaxPt.Z + 1);
      S.Z          := HighestPoint.Z;
      HighestPoint.Free;
      W1.Free;
      W2.Free;
      W3.Free;
      MinPt.Free;
      MaxPt.Free;
    End
    Else S.Z := 0;
  End
  Else S.Radius := Sqrt(Sqr(X - S.X) + Sqr(Y - S.Y));
  Result := True;
End; // TConfigurableSounds.DoSetPoint

Procedure TConfigurableSounds.Draw;
Begin
  frmTexPicker.DrawSounds;
  DrawSelectionRectangle(False);
End; // TConfigurableSounds.Draw

Procedure TConfigurableSounds.ShowSelectedParameters;
Var S: TSound;
Begin
  If SelectedIndex >= 0 Then
  Begin
    S                                := TSound(Items.Objects[SelectedIndex]);
    frmTexPicker.cbAreaSound.Checked := S.Area;
    If S.DayName <> ''
     Then frmTexPicker.rbDaySound.Caption := 'Day: ' + S.DayName
     Else frmTexPicker.rbDaySound.Caption := 'Day: (none)';
    If S.NightName <> ''
     Then frmTexPicker.rbNightSound.Caption := 'Night: ' + S.NightName
     Else frmTexPicker.rbNightSound.Caption := 'Night: (none)';
  End
  Else
  Begin
    frmTexPicker.cbAreaSound.Checked  := False;
    frmTexPicker.rbDaySound.Caption   := 'Day: (none)';
    frmTexPicker.rbNightSound.Caption := 'Night: (none)';
  End;
End; // TConfigurableSounds.ShowSelectedParameters

Function TConfigurableSounds.CanCreateQuery: Boolean;
Begin
  Result := (frmTexPicker.lbSounds.Items.Count > 0) And (frmTexPicker.lbSounds.ItemIndex >= 0);
End; // TConfigurableSounds.CanCreateQuery

// ----------------------------
// TConfigurableClipElevation
// ----------------------------

Procedure TConfigurableClipElevation.Draw;
Var
  Region : THandle;
  SX1    : Integer;
  SY1    : Integer;
  SX2    : Integer;
  SY2    : Integer;
  X0     : Integer;

Begin
  // Clip to the drawing area

  Region := CreateRectRgn(0,0,0,0);
  SetRectRgn(Region,frmTexPicker.Splitter1.Left + frmTexPicker.Splitter1.Width,0,frmTexPicker.sbVertical.Left,frmTexPicker.sbHorizontal.Top);
  SelectClipRgn(frmTexPicker.Canvas.Handle,Region);

  SX1 := Round(frmTexPicker.ZoneYToScreenXCoord(Y1));
  SY1 := Round(frmTexPicker.ZoneXToScreenYCoord(X1));
  SX2 := Round(frmTexPicker.ZoneYToScreenXCoord(Y2));
  SY2 := Round(frmTexPicker.ZoneXToScreenYCoord(X2));
  If SX2 < SX1 Then
  Begin
    X0  := SX2;
    SX2 := SX1;
    SX1 := X0;
  End;
  If SY2 < SY1 Then
  Begin
    X0  := SY2;
    SY2 := SY1;
    SY1 := X0;
  End;
  X0  := frmTexPicker.Splitter1.Left + frmTexPicker.Splitter1.Width;
  frmTexPicker.PaintTopographic(SX1 - X0,SY1,SX2 - X0,SY2,ClipTopographicColor,Z,Z);
  frmTexPicker.Canvas.Pen.Color   := clYellow;
  frmTexPicker.Canvas.Brush.Color := clYellow;
  frmTexPicker.Canvas.Brush.Style := bsClear;
  frmTexPicker.Canvas.Rectangle(SX1,SY1,SX2,SY2);
  frmTexPicker.Canvas.Brush.Style := bsSolid;
  frmTexPicker.Canvas.Rectangle(SX1 - 2,SY1 - 2,SX1 + 3,SY1 + 3);
  frmTexPicker.Canvas.Rectangle(SX2 - 2,SY1 - 2,SX2 + 3,SY1 + 3);
  frmTexPicker.Canvas.Rectangle(SX1 - 2,SY2 - 2,SX1 + 3,SY2 + 3);
  frmTexPicker.Canvas.Rectangle(SX2 - 2,SY2 - 2,SX2 + 3,SY2 + 3);

  // Go back to no clipping

  SelectClipRgn(frmTexPicker.Canvas.Handle,0);
  DeleteObject(Region);  
End; // TConfigurableClipElevation.Draw

Function TConfigurableClipElevation.CheckSelected(X,Y: Integer): Boolean;
Var
  D   : Single;
  ZB  : TZoneBound;
  SX1 : Single;
  SY1 : Single;
  SX2 : Single;
  SY2 : Single;

Begin
  Result := False;
  SX1    := frmTexPicker.ZoneYToScreenXCoord(Y1);
  SY1    := frmTexPicker.ZoneXToScreenYCoord(X1);
  SX2    := frmTexPicker.ZoneYToScreenXCoord(Y2);
  SY2    := frmTexPicker.ZoneXToScreenYCoord(X2);
  D      := Sqrt(Sqr(X - SX1) + Sqr(Y - SY1));
  If D < SelectPointDist Then
  Begin
    EndPoint := 1;
    Result   := True;
  End
  Else
  Begin
    D := Sqrt(Sqr(X - SX2) + Sqr(Y - SY1));
    If D < SelectPointDist Then
    Begin
      EndPoint := 2;
      Result   := True;
    End
    Else
    Begin
      D := Sqrt(Sqr(X - SX1) + Sqr(Y - SY2));
      If D < SelectPointDist Then
      Begin
        EndPoint := 3;
        Result   := True;
      End
      Else
      Begin
        D := Sqrt(Sqr(X - SX2) + Sqr(Y - SY2));
        If D < SelectPointDist Then
        Begin
          EndPoint := 4;
          Result   := True;
        End;
      End;
    End;
  End;
End; // TConfigurableClipElevation.CheckSelected

Function TConfigurableClipElevation.IsOn(X,Y: Integer; Exclude: Integer = -1): Integer;
Begin
  Result := 0;
End; // TConfigurableClipElevation.IsOn

Function TConfigurableClipElevation.CanCreateQuery: Boolean;
Begin
  Result := False;
End; // TConfigurableClipElevation.CanCreateQuery

Procedure TConfigurableClipElevation.DoSelectAt(X,Y: Integer);
Begin
End; // TConfigurableClipElevation.DoSelectAt

Procedure TConfigurableClipElevation.DoCreateAt(X,Y: Single);
Begin
End; // TConfigurableClipElevation.DoCreateAt

Function TConfigurableClipElevation.DoSetPoint(X0,Y0,X,Y: Single): Boolean;
Begin
  Case EndPoint Of
    1: Begin
         X1 := X;
         Y1 := Y;
       End;
    2: Begin
         X1 := X;
         Y2 := Y;
       End;
    3: Begin
         X2 := X;
         Y1 := Y;
       End;
    4: Begin
         X2 := X;
         Y2 := Y;
       End;
  End; // Case
  Result := True;
End; // TConfigurableClipElevation.DoSetPoint

// ----------------------------
// TfrmTexPicker
// ----------------------------

Procedure TfrmTexPicker.LoadSounds;
Var
  Path     : String;
  S        : TSearchRec;
  S3DFile  : TS3DFile;
  WAVList  : TStringList;
  I,J,K    : Integer;
  Size     : Integer;
  St       : String;
  WAVInfo  : TWAVInfo;
  Count    : Integer;
  P        : Pointer;
  FileList : TStringList;
  List     : TStringList;
  Files    : Array Of String;
  FCounter : Integer;

Begin
  If High(SoundFiles) < 0 Then
  Begin
    lbSounds.Clear;
    Path := frmMain.ProgramSettings.EQDir;
    If Path <> '' Then
    Begin
      If Path[Length(Path)] <> '\' Then Path := Path + '\';
    End;

    // Find out how many files there are and sort them so older files are checked first
    // (apparently older files get replaced by newer ones in newer files -- but the old
    // ones were't deleted).

    Count    := 0;
    FileList := TStringList.Create;
    List     := TStringList.Create;
    If FindFirst(Path + 'snd*.pfs',faAnyFile,S) = 0 Then
    Begin
      Repeat
        Inc(Count);
        St := Copy(S.Name,4,Length(S.Name) - 7);
        While Length(St) < 4 Do St := '0' + St;
        List.AddObject(St,Pointer(FileList.Count));
        FileList.Add(S.Name);
      Until FindNext(S) <> 0;
    End;
    FindClose(S);
    List.Sort;
    SetLength(Files,FileList.Count);
    For I := 0 To High(Files) Do Files[I] := FileList.Strings[Integer(List.Objects[I])];
    FileList.Free;
    List.Free;

    // Load the files

    If High(Files) >= 0 Then
    Begin
      frmStatus.Show;
      frmStatus.SetCaption('Loading sounds');
      J                      := 0;
      WAVList                := TStringList.Create;
      WAVList.Sorted         := True;
      tbSoundsPlay.Enabled   := False;
      tbPlaySound.Enabled    := False;
      pcGroundEditor.Enabled := False;
      For FCounter := 0 To High(Files) Do
      Begin
        frmStatus.SetPosition(J / Count);
        Inc(J);
        S3DFile := TS3DFile.Create;
        S3DFile.LoadFromFile(Path + Files[FCounter]);
        For I := 0 To S3DFile.FileCount - 1 Do
        Begin
          St := LowerCase(S3DFile.FileName[I]);
          If Copy(St,Length(St) - 3,4) = '.wav' Then
          Begin
            P := S3DFile.DataPointer[I];
            If P <> Nil Then
            Begin
              St   := Copy(St,1,Length(St) - 4);
              K    := WAVList.IndexOf(St);
              Size := S3DFile.FileSize[I];

              // Overwrite sound files where there were newer replacements

              If K >= 0 Then
              Begin
                WAVInfo := TWAVInfo(WAVList.Objects[K]);
                FreeMem(SoundFiles[WAVInfo.Index]);
                GetMem(SoundFiles[WAVInfo.Index],Size);
                Move(P^,SoundFiles[WAVInfo.Index]^,Size);
                SoundSizes[WAVInfo.Index] := Size;
                WAVInfo.WAVFileName       := S3DFile.FileName[I];
              End
              Else
              Begin
                SetLength(SoundFiles,High(SoundFiles) + 2);
                SetLength(SoundSizes,High(SoundSizes) + 2);
                SoundSizes[High(SoundSizes)] := Size;
                GetMem(SoundFiles[High(SoundFiles)],Size);
                Move(P^,SoundFiles[High(SoundFiles)]^,Size);
                WAVInfo             := TWAVInfo.Create;
                WAVInfo.WAVFileName := S3DFile.FileName[I];
                WAVInfo.Index       := High(SoundFiles);
                WAVList.AddObject(St,WAVInfo);
              End;
            End
            Else ShowMessage('Error uncompressing ' + S3DFile.FileName[I] + ' from ' + Files[FCounter]);
          End;
        End; // For I
        S3DFile.Free;
      End; // For FCounter
      SetLength(SoundIndexes,WAVList.Count);
      lbSounds.Items.Add('(none)');
      For I := 0 To WAVList.Count - 1 Do
      Begin
        lbSounds.Items.Add(WAVList.Strings[I]);
        SoundIndexes[I] := TWAVInfo(WAVList.Objects[I]).Index;
        WAVList.Objects[I].Free;
      End;
      tbSoundsPlay.Enabled   := True;
      tbPlaySound.Enabled    := True;
      pcGroundEditor.Enabled := True;
      frmStatus.Hide;
      WAVList.Free;
    End;
    SetLength(Files,0);
  End;
End; // TfrmTexPicker.LoadSounds

Procedure TfrmTexPicker.CalculateOverlay;
Var
  GX,GY      : Integer;
  X,Y        : Integer;
  ZX,ZY      : Single;
  V          : T3DPoint;
  Regions    : Array Of TRegion;
  Region     : TRegion;
  V1,V2,V3   : T3DPoint;
  P1,P2      : T3DPoint;
  I,J,K      : Integer;
  P,IP       : TPolygon;
  Norm       : Single;
  TX,TZ      : Single;
  Intersect  : T3DPoint;
  OMPtr      : PLongWord;
  OVPtr      : PLongWord;
  OMPtr0     : PLongWord;
  OVPtr0     : PLongWord;
  Pitch      : Integer;
  W1,W2,W3   : T3DPoint;
  BS         : Integer;

  MinPt      : T3DPoint;
  MaxPt      : T3DPoint;

  DX1,DX2    : Single;
  DY1,DY2    : Single;
  DU1,DU2    : Single;
  DV1,DV2    : Single;

  Denom      : Single;

  UX,UY      : Single;
  VX,VY      : Single;

  U0,V0      : Single;
  Path       : String;
  Tex        : TTexture;
  C          : TBGRA;
  B          : Byte;
  St         : String;
  GH,GW      : Integer;

  XInc       : Single;
  YInc       : Single;

//  Textures   : String;
//  Opacities  : String;
//  Parameters : String;
  L          : TStringList;

Begin
  // First make an ORDERED texture list

  L        := TStringList.Create;
  L.Sorted := True;
  For I := 1 To TextureLibrary.Count - 1 Do L.AddObject(TextureLibrary.Strings[I],Pointer(I - 1));

  BS := (BlitTexSize Div 4) * 4;
  If BS < BlitTexSize Then Inc(BS,4);

  // Create a mesh containing everything in the zone, except mesh references and land

  TreeMesh.Free;
  Tree.Free;

  frmStatus.SetCaption('Generating zone mesh');
  TreeMesh := frmMain.Zone.BuildPolygonList(True,True,True,False);

  frmStatus.SetCaption('Calculating polygon normals');
  TreeMesh.CalcNormals;

  frmStatus.SetCaption('Calculating texture coordinates');
  For I := 0 To TreeMesh.Polygons.Count - 1 Do
  Begin
    frmStatus.SetPosition(I / TreeMesh.Polygons.Count);
    TreeMesh.CalcTextureCoords(TPolygon(TreeMesh.Polygons.Objects[I]));
  End; // For I

  // Create a BSP tree and split it along a grid

  frmStatus.SetCaption('Creating BSP tree');
  Tree := TTree.Create(TreeMesh,False);
  Tree.SplitAlongGrid(32,32 * 3);

  frmStatus.SetCaption('Converting polygons to triangles');
  Tree.Root.ConvertToTriangles;

  MinPt := T3DPoint.Create;
  MaxPt := T3DPoint.Create;
  Tree.Root.Mesh.GetBounds(MinPt,MaxPt);

  // For each pixel in the mask, find the highest point in the mesh

  P1        := T3DPoint.Create;
  P2        := T3DPoint.Create;
  Intersect := T3DPoint.Create;
  W1        := T3DPoint.Create;
  W2        := T3DPoint.Create;
  W3        := T3DPoint.Create;
  OMPtr0    := PLongWord(OverlayMaskBuffer);
  OVPtr0    := PLongWord(OverlayBuffer);
  Pitch     := (TexSize Div 4) * 4;

  Path      := ExtractFilePath(Application.ExeName) + 'library\textures\';

  If Pitch < TexSize Then Inc(Pitch,4);
  frmStatus.SetCaption('Creating overlay and overlay mask');

  // Scan through all grid blocks

  GH := Max(ZGridHeight,1);
  GW := Max(ZGridWidth,1);

  If GYOfs > 0
   Then XInc := GYOfs * XStep - (MaxX - GMaxX)
   Else XInc := 0;

  If GXOfs > 0
   Then YInc := GXOfs * YStep - (MaxY - GMaxY)
   Else YInc := 0;

  For GY := 0 To GH - 1 Do
  Begin
    frmStatus.SetPosition(GY / GH);
    For GX := 0 To GW - 1 Do
    Begin
      // Calculate the overlay and overlay mask pointers for each block

      OMPtr := PLongWord(LongWord(OMPtr0) + (GY * ZGridWidth + GX) * Pitch * TexSize * 4);
      OVPtr := PLongWord(LongWord(OVPtr0) + (GY * ZGridWidth + GX) * Pitch * TexSize * 4);

      // Scan through each pixel in the block

      For Y := 0 To TexSize - 1 Do
      Begin
        For X := 0 To TexSize - 1 Do
        Begin
          // Get the world coordinates from the screen coordinates (we can't use the standard
          // conversion routines since those take the black grid into account and we don't want
          // to do that here)

          ZY := MaxY + YInc - (GX * TexSize + X) * YStep / TexSize;
          ZX := MaxX + XInc - (GY * TexSize + Y) * XStep / TexSize;

          // Check the grid element at ZX and ZY to see if it is hidden

          IP := Tree.GetMaxHeightAt(ZX,ZY,Intersect,W1,W2,W3,MinPt.Z - 1, MaxPt.Z + 1);

          // Compare the resulting Z position with the Z position of the ground mesh.  If we found something
          // that's higher than the ground then we want to show it.

          If (IP <> Nil) And
             ((Not frmMain.Zone.ElevationGrid.IsVisibleAtAbsolute(ZX,ZY)) Or
              (Not frmMain.Zone.ElevationGrid.CanGetHeightAtAbsolute(ZX,ZY)) Or
              (Intersect.Z > frmMain.Zone.ElevationGrid.GetHeightAtAbsolute(ZX,ZY))) Then
          Begin
            // We found a polygon that shows above the ground. Now we need to figure out the
            // appropriate texture coordinate on the polygon:
            //
            // Formula for texture coordinates: Vt = MV - V0, where
            //
            // Vt = |u|           <-- texture coordinates
            //      |v|
            //      |w|
            //
            // M = |ux vx wx|     <-- transformation matrix
            //     |uy vy wy|
            //     |uz vz wz|
            //
            // V = |x|            <-- polygon coordinates
            //     |y|
            //     |z|
            //
            // V0 = |u0|          <-- offset
            //      |v0|
            //      |z0|
            //
            // Where (wx,wy,wz) is parallel to the polygon normal.
            //
            // We are ignoring Z coordinates here since we are looking straight down
            // (no perspective projection).  This reduces the equation to a 2-dimensional
            // problem:
            //
            // Vt = |u|
            //      |v|
            //
            // M = |ux vx|
            //     |uy vy|
            //
            // V = |x|
            //     |y|
            //
            // V0 = |u0|
            //      |v0|
            //
            // We need to know M and V0.  First solve for M, using known vertex and texture coordinates:
            //
            //       du1   du2             dv1   dv2
            //       --- - ---             --- - ---
            //       dx1   dx2             dx1   dx2            du        dy           dv        dy
            // vx = -----------,     vy = -----------,     ux = -- - vy * --,     uy = -- - vy * --
            //       dy1   dy2             dy1   dy2            dx        dx           dx        dx
            //       ---   ---             --- - ---
            //       dx1   dx2             dx1   dx2
            //
            // The trick is choosing a dx1 and dx2 that aren't zero (note now we can choose
            // any dx when it comes to calculating ux and uy once we know vx and vy)
            //
            // Then solve for V0, using known vertex and texture coordinates:
            //
            // u0 = x * ux + y * vx - u
            // v0 = x * uy + y * vy - v

            If (W1.X <> W2.X) And (W1.X <> W3.X) Then
            Begin
              DX1 := W1.X - W2.X;
              DX2 := W1.X - W3.X;
              DY1 := W1.Y - W2.Y;
              DY2 := W1.Y - W3.Y;
              DU1 := IP.TX[0] - IP.TX[1];
              DU2 := IP.TX[0] - IP.TX[2];
              DV1 := IP.TZ[0] - IP.TZ[1];
              DV2 := IP.TZ[0] - IP.TZ[2];
            End
            Else If (W1.X <> W2.X) And (W2.X <> W3.X) Then
            Begin
              DX1 := W1.X - W2.X;
              DX2 := W2.X - W3.X;
              DY1 := W1.Y - W2.Y;
              DY2 := W2.Y - W3.Y;
              DU1 := IP.TX[0] - IP.TX[1];
              DU2 := IP.TX[1] - IP.TX[2];
              DV1 := IP.TZ[0] - IP.TZ[1];
              DV2 := IP.TZ[1] - IP.TZ[2];
            End
            Else If (W1.X <> W3.X) And (W2.X <> W3.X) Then
            Begin
              DX1 := W1.X - W3.X;
              DX2 := W2.X - W3.X;
              DY1 := W1.Y - W3.Y;
              DY2 := W2.Y - W3.Y;
              DU1 := IP.TX[0] - IP.TX[2];
              DU2 := IP.TX[1] - IP.TX[2];
              DV1 := IP.TZ[0] - IP.TZ[2];
              DV2 := IP.TZ[1] - IP.TZ[2];
            End
            Else
            Begin
              DX1 := 0;
              DX2 := 0;
              DY1 := 0;
              DY2 := 0;
              DU1 := 0;
              DU2 := 0;
              DV1 := 0;
              DV2 := 0;
            End;
            If DX1 <> 0 Then
            Begin
              Denom := DY1 / DX1 - DY2 / DX2;
              If Denom <> 0 Then
              Begin
                VX := (DU1 / DX1 - DU2 / DX2) / Denom;
                VY := (DV1 / DX1 - DV2 / DX2) / Denom;
                UX := (DU1 - VX * DY1) / DX1;
                UY := (DV1 - VY * DY1) / DX1;
                U0 := W1.X * UX + W1.Y * VX - IP.TX[0];
                V0 := W1.X * UY + W1.Y * VY - IP.TZ[0];

                // Calculate u and v, ignoring multiples of the texture size

                TX := Frac(Intersect.X * UX + Intersect.Y * VX - U0);
                TZ := Frac(Intersect.X * UY + Intersect.Y * VY - V0);

                // It's possible for tx and/or tz to be negative, so we need to check for
                // it and correct it if necessary

                If TX < 0 Then TX := TX + 1;
                If TZ < 0 Then TZ := TZ + 1;

                // Okay, get the polygon's texture.  If the polygon is using animated textures (that is,
                // multiple textures), then get the first one.

//                BreakupTextureString(IP.Texture,Textures,Opacities,Parameters);
{
                I := L.IndexOf(GetToken(';',Textures));

                // Did we get a texture?

                If I >= 0 Then
                Begin
                  I := Integer(L.Objects[I]);

                  // The overlay mask should be zero (i.e. black) for areas where we want to show the
                  // overlay (we already filled in the entire overlay mask with $FFFFFFFF when we created it).

                  OMPtr^ := 0;

                  J      := (Round(TX * BS) + Round(TZ * BS) * BS) + I * BS * BS * 4;

                  // Get the color from the texture and exchange the red and blue components

                  C      := PRGBA(@(BlitBuffer[J]))^;

                  // Save the overlay color

                  OVPtr^ := LongWord(C);
                End;
}

                I := frmMain.glView.Scene3D.Scene.Textures.IndexOf(Path + IP.TextureInfo.FirstTexture {GetToken(';',Textures)} + '.bmp');

                // Did we get a texture?

                If I >= 0 Then
                Begin
                  I := Integer(L.Objects[I]);

                  // The overlay mask should be zero (i.e. black) for areas where we want to show the
                  // overlay (we already filled in the entire overlay mask with $FFFFFFFF when we created it).

                  OMPtr^ := 0;

                  // Try to get the texture, but it's possible that we don't have it.  In that case we'll
                  // display the polygon as pure white.

                  Tex    := TTexture(frmMain.glView.Scene3D.Scene.Textures.Objects[I]);
                  If (Tex <> Nil) And (Tex.Buffer3 <> Nil) And (Tex.Size > 0) Then
                  Begin

                    // .BMP files are usually stored bottom-to-top

                    J      := (Round(TX * (Tex.BMPInfo.Width - 1)) + Round(TZ * (Tex.BMPInfo.Height - 1)) * Tex.BMPInfo.Width) * 4;

                    // Get the color from the texture and exchange the red and blue components

                    C      := PBGRA(LongWord(Tex.Buffer3) + J)^;
{
                    B      := C.B;
                    C.B    := C.R;
                    C.R    := B;
}
                  End
                  Else
                  Begin
                    C.R := 255;
                    C.G := 255;
                    C.B := 255;
                    C.A := 255;
                  End;

                  // Save the overlay color

                  OVPtr^ := LongWord(C);
                End;
              End;
            End;
          End;
          Inc(LongWord(OMPtr),4);
          Inc(LongWord(OVPtr),4);
        End; // For X
      End; // For Y
    End; // For GX
  End; // For GY

  // Cleanup

  P1.Free;
  P2.Free;
  W1.Free;
  W2.Free;
  W3.Free;
  MinPt.Free;
  MaxPt.Free;
  Intersect.Free;
  L.Free;
End; // TfrmTexPicker.CalculateOverlay

procedure TfrmTexPicker.FormShow(Sender: TObject);
Var
  I,J,K,L : Integer;
  W       : Integer;
  BMP     : TBitmap;
  BMP1    : TBitmap;
  P       : TPolygon;
  Points  : Array Of Windows.TPoint;
  GX,GY   : Integer;
  X,Y     : Integer;
  V       : T3DPoint;
  R       : TRect;
  GX1,GY1 : Single;
  LinePtr : PLongArray;
  MaxY1   : Single;
  MaxX1   : Single;

  Procedure AddMesh(ZO: TZoneObject);
  Var
    I  : Integer;
    GO : TGroupObject;
    ML : TMeshLibraryObjectReference;

  Begin
    If ZO Is TMeshLibraryObjectReference Then
    Begin
      ML := TMeshLibraryObjectReference.Create(TMeshLibraryObjectReference(ZO));
      ML.ChangeToAbsolute(ZO.GetParent);
      ML.SetParent(Nil);
      Meshes.Items.AddObject('',ML);
      OrigMeshes.AddObject(ZO.GetName,ZO);
    End
    Else If ZO Is TGroupObject Then
    Begin
      GO := TGroupObject(ZO);
      For I := 0 To GO.Objects.Count - 1 Do AddMesh(TZoneObject(GO.Objects.Objects[I]));
    End;
  End; // AddMesh

  Procedure ExchangeRedAndBlue(BMP: TBitmap);
  Var
    I,J : Integer;
    L   : LongWord;
    P   : ^LongWord;
    R   : Byte;

  Begin
    For I := 0 To BMP.Height - 1 Do
    Begin
      P := BMP.ScanLine[I];
      For J := 0 To BMP.Width - 1 Do
      Begin
        L          := P^;
        R          := TBGRA(L).R;
        TBGRA(L).R := TBGRA(L).B;
        TBGRA(L).B := R;
        P^         := L;
        Inc(LongWord(P),4);
      End; // For J
    End; // For I
  End; // ExchangeRedAndBlue

begin
  While Not AllInitialized Do
  Begin
    Application.ProcessMessages;
    Sleep(1);
  End; // While
  frmStatus.Show;
  lblCurrentTex.Caption    := '';
  Repositioning            := False;
  SelectedTex              := -1;
  OldSBHPos                := -1;
  OldSBVPos                := -1;
  CursorX                  := -1;
  CursorY                  := -1;
  Drawing                  := False;
  Angle                    := 0;
  Tree                     := Nil;
  TreeMesh                 := Nil;
  boolChangingCEEdit       := False;
  boolChangingCETB         := False;

  // Load the texture library, skipping the "default setting" entry

  cbUpperTexture.Clear;
  cbLowerTexture.Clear;
  For I := 1 To TextureLibrary.Count - 1 Do
  Begin
    cbUpperTexture.Items.Add(TextureLibrary.Strings[I]);
    cbLowerTexture.Items.Add(TextureLibrary.Strings[I]);
  End; // For I
  If cbUpperTexture.Items.Count > 0 Then cbUpperTexture.ItemIndex := 0;
  If cbLowerTexture.Items.Count > 0 Then cbLowerTexture.ItemIndex := 0;

  If (frmMain.Zone.ElevationGrid.NX > 0) And (frmMain.Zone.ElevationGrid.NY > 0) Then
  Begin
    If MaxX > frmMain.Zone.ElevationGrid.MaxX Then
    Begin
      GY1   := MaxX - frmMain.Zone.ElevationGrid.MaxX;
      GYOfs := Trunc(GY1 / GridSize);
      If GYOfs * GridSize < GY1 Then Inc(GYOfs);
    End
    Else GYOfs := 0;

    If MaxY > frmMain.Zone.ElevationGrid.MaxY Then
    Begin
      GX1   := MaxY - frmMain.Zone.ElevationGrid.MaxY;
      GXOfs := Trunc(GX1 / GridSize);
      If GXOfs * GridSize < GX1 Then Inc(GXOfs);
    End
    Else GXOfs := 0;

    If MinX < frmMain.Zone.ElevationGrid.MinX Then
    Begin
      GY1 := frmMain.Zone.ElevationGrid.MinX - MinX;
      GY  := Trunc(GY1 / GridSize);
      If GY * GridSize < GY1 Then Inc(GY);
    End
    Else GY := 0;

    If MinY < frmMain.Zone.ElevationGrid.MinY Then
    Begin
      GX1 := frmMain.Zone.ElevationGrid.MinY - MinY;
      GX  := Trunc(GX1 / GridSize);
      If GX * GridSize < GX1 Then Inc(GX);
    End
    Else GX := 0;

    ZGridWidth  := GridWidth  + GXOfs + GX;
    ZGridHeight := GridHeight + GYOfs + GY;
  End
  Else
  Begin
    ZGridWidth               := Trunc((MaxY - MinY) / GridSize);
    ZGridHeight              := Trunc((MaxX - MinX) / GridSize);
    If ZGridWidth  * GridSize < MaxY - MinY Then Inc(ZGridWidth);
    If ZGridHeight * GridSize < MaxX - MinX Then Inc(ZGridHeight);
    GYOfs := 0;
    GXOfs := 0;
  End;

  OrigMeshes               := TStringList.Create;
  lblSelectedMesh.Caption  := '(none)';
  rbDaySound.Caption       := 'Day: (none)';
  rbNightSound.Caption     := 'Night: (none)';

  // Store the minimum and maximum Z extents for each polygon

  If MO1 <> Nil Then I := MO1.Polygons.Count Else I := 0;
  If MO2 <> Nil Then J := MO2.Polygons.Count Else J := 0;
  SetLength(PolyMinZ,I + J);
  SetLength(PolyMaxZ,I + J);
  If MO1 <> Nil Then
  Begin
    For K := 0 To MO1.Polygons.Count - 1 Do
    Begin
      P := TPolygon(MO1.Polygons.Objects[K]);
      If High(P.Vertices) >= 0 Then
      Begin
        V := T3DPoint(MO1.Vertices.Objects[P.Vertices[0]]);
        PolyMinZ[K] := V.Z;
        PolyMaxZ[K] := V.Z;
        For L := 1 To High(P.Vertices) Do
        Begin
          V := T3DPoint(MO1.Vertices.Objects[P.Vertices[L]]);
          If V.Z < PolyMinZ[K] Then PolyMinZ[K] := V.Z;
          If V.Z > PolyMaxZ[K] Then PolyMaxZ[K] := V.Z;
        End; // For L
      End
      Else
      Begin
        PolyMinZ[K] := 0;
        PolyMaxZ[K] := 0;
      End;
    End; // For K
  End;
  If MO2 <> Nil Then
  Begin
    For K := 0 To MO2.Polygons.Count - 1 Do
    Begin
      P := TPolygon(MO2.Polygons.Objects[K]);
      If High(P.Vertices) >= 0 Then
      Begin
        V := T3DPoint(MO2.Vertices.Objects[P.Vertices[0]]);
        PolyMinZ[K + I] := V.Z;
        PolyMaxZ[K + I] := V.Z;
        For L := 1 To High(P.Vertices) Do
        Begin
          V := T3DPoint(MO2.Vertices.Objects[P.Vertices[L]]);
          If V.Z < PolyMinZ[K + I] Then PolyMinZ[K + I] := V.Z;
          If V.Z > PolyMaxZ[K + I] Then PolyMaxZ[K + I] := V.Z;
        End; // For L
      End
      Else
      Begin
        PolyMinZ[K + I] := 0;
        PolyMaxZ[K + I] := 0;
      End;
    End; // For K
  End;
  If High(PolyMinZ) >= 0 Then
  Begin
    ZoneMinZ := PolyMinZ[0];
    ZoneMaxZ := PolyMaxZ[0];
    For I := 1 To High(PolyMinZ) Do If PolyMinZ[I] < ZoneMinZ Then ZoneMinZ := PolyMinZ[I];
    For I := 1 To High(PolyMaxZ) Do If PolyMaxZ[I] > ZoneMaxZ Then ZoneMaxZ := PolyMaxZ[I];
  End
  Else
  Begin
    ZoneMinZ := 0;
    ZoneMaxZ := 0;
  End;

  // Initialize and load configurable items

  Bounds.Init;
  ZonePlanes.Init;
  Meshes.Init;
  Sounds.Init;
  For I := 0 To frmMain.Zone.Bounds.Count     - 1 Do Bounds.Items.AddObject('',TZoneBound.Create(TZoneBound(frmMain.Zone.Bounds.Objects[I])));
  For I := 0 To frmMain.Zone.ZonePlanes.Count - 1 Do ZonePlanes.Items.AddObject('',TZonePlane.Create(TZonePlane(frmMain.Zone.ZonePlanes.Objects[I])));
  For I := 0 To frmMain.Zone.Count            - 1 Do AddMesh(TZoneObject(frmMain.Zone.Objects[I]));
  For I := 0 To frmMain.Zone.Sounds.Count     - 1 Do Sounds.Items.AddObject('',TSound.Create(TSound(frmMain.Zone.Sounds.Objects[I])));

  frmMain.LoadMeshLibraryList(tvMeshes);
  SelectedMeshObject := '';
//  lbMeshes.Clear;
//  lbMeshes.Items.AddStrings(MeshLibrary);
  PositionScrollBars;
  pnlSoundInfoResize(Self);

  // Load the texture list, skipping the "default setting" entry

  lbTextures.Clear;
  For I := 1 To TextureLibrary.Count - 1 Do lbTextures.Items.Add(TextureLibrary.Strings[I]);

  // Allocate the blit buffer and load the texture data

  If BlitBuffer = Nil Then
  Begin
    W := (BlitTexSize Div 4) * 4;
    If W < BlitTexSize Then Inc(W,4);
    I := W * BlitTexSize * (TextureLibrary.Count - 1) * 4; 
    If I > 0 Then
    Begin
      BlitBufSize      := I * 4; // Store each one at four rotations
      GetMem(BlitBuffer,BlitBufSize);
      K                := 0;
//      BMP              := TBitmap.Create;
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

      frmStatus.SetCaption('Loading textures');
      For I := 1 To TextureLibrary.Count - 1 Do
      Begin
        frmStatus.SetPosition(I / TextureLibrary.Count);

        BMP := LoadImage(ExtractFilePath(Application.ExeName) + 'library\textures\' + TextureLibrary.Strings[I] + '.bmp');
        If BMP = Nil Then BMP := LoadImage(ExtractFilePath(Application.ExeName) + 'library\textures\' + TextureLibrary.Strings[I] + '.jpg');
        If BMP = Nil Then BMP := LoadImage(ExtractFilePath(Application.ExeName) + 'library\textures\' + TextureLibrary.Strings[I] + '.tga');
        If BMP <> Nil Then
        Begin
//          BMP.PixelFormat := pf32Bit;
          BMP1.Canvas.StretchDraw(R,BMP);
          BMP.Assign(BMP1);

          // Exchange red and blue components

//        ExchangeRedAndBlue(BMP);

          // Store at 0 degrees

          For J := 0 To BMP.Height -  1 Do
          Begin
            Move(BMP.ScanLine[J]^,BlitBuffer^[K],W * 4);
            Inc(K,W);
          End; // For J

          // Store at 90 degrees

          RotateLeft(BMP,BMP1);
          For J := 0 To BMP1.Height -  1 Do
          Begin
            Move(BMP1.ScanLine[J]^,BlitBuffer^[K],W * 4);
            Inc(K,W);
          End; // For J

          // Store at 180 degrees

          RotateLeft(BMP1,BMP);
          For J := 0 To BMP.Height -  1 Do
          Begin
            Move(BMP.ScanLine[J]^,BlitBuffer^[K],W * 4);
            Inc(K,W);
          End; // For J

          // Store at 270 degrees

          RotateLeft(BMP,BMP1);
          For J := 0 To BMP1.Height -  1 Do
          Begin
            Move(BMP1.ScanLine[J]^,BlitBuffer^[K],W * 4);
            Inc(K,W);
          End; // For J
          BMP.Free;
        End
        Else Inc(K,W * BMP1.Height * 4);
      End; // For I
//      BMP.Free;
      BMP1.Free;
    End;
  End;

  // Select the first texture if we can

  If lbTextures.Items.Count > 0 Then
  Begin
    lbTextures.ItemIndex := 0;
    lbTextures.OnClick(Self);
  End;

  // Now that the textures are loaded, set up our draw grid

  SetupDrawGrid;

  // Allocate the mask buffer

  If MaskBuffer <> Nil Then FreeMem(MaskBuffer,MaskBufSize);
  MaskBuffer := Nil;
  If OverlayMaskBuffer <> Nil Then FreeMem(OverlayMaskBuffer,OverlayMaskBufSize);
  OverlayMaskBuffer := Nil;
  If OverlayBuffer <> Nil Then FreeMem(OverlayBuffer,OverlayBufSize);
  OverlayBuffer := Nil;
  W := (TexSize Div 4) * 4;
  If W < TexSize Then Inc(W,4);
  I := Max(ZGridWidth,1) * Max(ZGridHeight,1) * TexSize * W * 4;
  If I > 0 Then
  Begin
    MaskBufSize        := I;
    OverlayMaskBufSize := I;
    OverlayBufSize     := I;
    GetMem(MaskBuffer,MaskBufSize);
    GetMem(OverlayMaskBuffer,OverlayMaskBufSize);
    GetMem(OverlayBuffer,OverlayBufSize);
    FillChar(MaskBuffer^,MaskBufSize,0);                 // Important!
    FillChar(OverlayMaskBuffer^,OverlayMaskBufSize,255); // Important!
    FillChar(OverlayBuffer^,OverlayBufSize,0);           // Important!
    BMP                    := TBitmap.Create;
    BMP.Width              := TexSize;
    BMP.Height             := TexSize;
    BMP.PixelFormat        := pf32Bit;
    BMP.Canvas.Brush.Style := bsSolid;

    // Create the overlay and overlay mask

    CalculateOverlay;

    // Create the mask using the land areas only

    If MO1 <> Nil Then
    Begin
      frmStatus.SetCaption('Creating polygon mask');
      For I := 0 To MO1.Polygons.Count - 1 Do
      Begin
        frmStatus.SetPosition(I / MO1.Polygons.Count);

        P := TPolygon(MO1.Polygons.Objects[I]);

        // We only need to worry about land polygons; land will be 1 (white), underwater will be 0 (black)

        // Clear the bitmap

        BMP.Canvas.Pen.Color   := clBlack;
        BMP.Canvas.Brush.Color := clBlack;
        BMP.Canvas.Rectangle(0,0,TexSize,TexSize);

        // Set to white now so we can write land areas

        BMP.Canvas.Pen.Color   := clWhite;
        BMP.Canvas.Brush.Color := clWhite;

        // First we need to determine which grid this polygon is in

        GX := P.Tag Mod GridWidth;
        GY := P.Tag Div GridWidth;

        // Now make a mask of the polygon

        SetLength(Points,High(P.Vertices) + 1);
        MaxY1 := GMaxY - GX * YStep;
        MaxX1 := GMaxX - GY * XStep;
        For J := 0 To High(P.Vertices) Do
        Begin
          V           := T3DPoint(MO1.Vertices.Objects[P.Vertices[J]]);
          Points[J].X := Round((MaxY1 - (V.Y + MO1.Loc.Y)) * ((TexSize - 1) / YStep));
          Points[J].Y := Round((MaxX1 - (V.X + MO1.Loc.X)) * ((TexSize - 1) / XStep));
        End; // For J

        BMP.Canvas.Polygon(Points);

        // Or the mask with the mask buffer

        J := ((GY + GYOfs) * ZGridWidth + GX + GXOfs) * TexSize * W;
        For Y := 0 To TexSize - 1 Do
        Begin
          K       := J + Y * W;
          LinePtr := PLongArray(BMP.ScanLine[Y]);
          For X := 0 To W - 1 Do MaskBuffer^[K + X] := MaskBuffer^[K + X] Or LinePtr^[X];
        End; // For Y
      End; // For I
    End;

    BMP.Free;
    SetLength(Points,0);
  End;

  // Allocate the texture arrays

  I := Max(GridWidth,0) * Max(GridHeight,0);
  SetLength(LTextures,I);
  SetLength(WTextures,I);
  SetLength(TexVisible,I);

  // Fill in the texture array

  FillInTextureArray;

  // Allocate the destination buffer

  If DestBuffer <> Nil Then FreeMem(DestBuffer,DestBufSize);
  DestBuffer := Nil;
  W := (TexSize Div 4) * 4;
  If W < TexSize Then Inc(W,4);
  I := Max(ZGridWidth,1) * Max(ZGridHeight,1) * TexSize * W * 4;
  If I > 0 Then
  Begin
    DestBufSize := I;
    GetMem(DestBuffer,DestBufSize);
    FillChar(DestBuffer^,DestBufSize,0);
  End;

  frmStatus.SetCaption('Pre-drawing textures');
  DrawDestBuffer;
  frmStatus.Visible := False;

  // Clear out the model view

  glvMesh.Scene3D.Scene.LockBSPTree('TfrmTexPicker.FormShow');
  glvMesh.Scene3D.Scene.ClearScene(True,True);
  glvMesh.Update;//.Scene3D.Update(glvMesh.Width,glvMesh.Height);
  glvMesh.Scene3D.Scene.UnlockBSPTree;

  While Not glvMesh.Initialized Do
  Begin
    Application.ProcessMessages;
    Sleep(1);
  End; // While

  // Set some default clipping parameters

  ClipElevation.Items.Add('');
  ClipElevation.X1  := MinX + (MaxX - MinX) * 0.25;
  ClipElevation.X2  := MinX + (MaxX - MinX) * 0.75;
  ClipElevation.Y1  := MinY + (MaxY - MinY) * 0.25;
  ClipElevation.Y2  := MinY + (MaxY - MinY) * 0.75;
  ClipElevation.Z   := (ZoneMinZ + ZoneMaxZ) / 2;
  edtElevation.Text := Trim(Format('%8.2f',[ClipElevation.Z]));
end;

procedure TfrmTexPicker.lbTexturesClick(Sender: TObject);
Var I,J,BS : Integer;
begin
  If (BlitBuffer <> Nil) And (lbTextures.ItemIndex >= 0) And (lbTextures.ItemIndex < lbTextures.Items.Count) Then
  Begin
    // Figure out the width, accounting for dword padding

    BS := (BlitTexSize Div 4) * 4;
    If BS < BlitTexSize Then Inc(BS,4);

    // When the user first selects a bitmap, default the rotation to 0 degrees, otherwise rotate it every time they click on it

    J := lbTextures.ItemIndex;
    If SelectedTex <> J
     Then Angle := 0
     Else Angle := (Angle + 1) And 3;
    SelectedTex := J;

    // Get the bitmap and transfer it to the swatch

    For I := 0 To BlitBMP.Height - 1 Do Move(BlitBuffer^[((SelectedTex * 4 + Angle) * BS * BlitTexSize) + I * BS],PLongArray(BlitBMP.ScanLine[I])^[0],BS * 4);
    imgTexture.Picture.Bitmap.Assign(BlitBMP);
    If (J >= 0) And (J < lbTextures.Items.Count)
     Then lblCurrentTex.Caption := lbTextures.Items.Strings[J]
     Else lblCurrentTex.Caption := '';
  End;
end;

procedure TfrmTexPicker.btnRotateLeftClick(Sender: TObject);
Var BMP: TBitmap;
begin
  Angle := (Angle + 1) And 3;
  BMP := TBitmap.Create;
  BMP.Assign(imgTexture.Picture.Bitmap);
  RotateLeft(BMP,imgTexture.Picture.Bitmap);
  BMP.Free;
  imgTexture.Repaint;
end;

procedure TfrmTexPicker.btnRotateRightClick(Sender: TObject);
Var BMP: TBitmap;
begin
  Angle := (Angle - 1) And 3;
  BMP := TBitmap.Create;
  BMP.Assign(imgTexture.Picture.Bitmap);
  RotateRight(BMP,imgTexture.Picture.Bitmap);
  BMP.Free;
  imgTexture.Repaint;
end;

Procedure TfrmTexPicker.PaintGrid;
Var
  I,J   : Integer;
  SW    : Integer;
  X0    : Integer;
  X,Y   : Integer;
  W,H   : Integer;
  ZW,ZH : Integer;
  X1,Y1 : Integer;
  X2,Y2 : Integer;
  C     : TColor;
  Vis   : Boolean;

Begin
  // Determine the dimensions of the paint area

  SW := GetSystemMetrics(SM_CXHSCROLL);
  X0 := Splitter1.Left + Splitter1.Width;
  W  := (ClientWidth  - X0 - SW) Div (TexSize + 1);
  H  := (ClientHeight - pnlBottom.Height - SW) Div (TexSize + 1);
  ZW := W;
  ZH := H;
  If W  > GridWidth   - 1 Then W  := GridWidth   - 1;
  If H  > GridHeight  - 1 Then H  := GridHeight  - 1;
  If ZW > ZGridWidth  - 1 Then ZW := ZGridWidth  - 1;
  If ZH > ZGridHeight - 1 Then ZH := ZGridHeight - 1;

  // Draw a blank grid

  Canvas.Pen.Color := Color;
  For X := 0 To ZW + 1 Do
  Begin
    Canvas.MoveTo(X * (TexSize + 1) + X0,0);
    Canvas.LineTo(X * (TexSize + 1) + X0,(ZH + 1) * (TexSize + 1));
  End; // For X
  For Y := 0 To ZH + 1 Do
  Begin
    Canvas.MoveTo(X0,Y * (TexSize + 1));
    Canvas.LineTo(X0 + (ZW + 1) * (TexSize + 1),Y * (TexSize + 1));
  End; // For Y

  // Draw a black grid

  Canvas.Pen.Color := clBlack;
  For X := GXOfs To GXOfs + W + 1 Do
  Begin
    Canvas.MoveTo(X * (TexSize + 1) + X0,0);
    Canvas.LineTo(X * (TexSize + 1) + X0,(H + 1) * (TexSize + 1));
  End; // For X
  For Y := GYOfs To GYOfs + H + 1 Do
  Begin
    Canvas.MoveTo(X0,Y * (TexSize + 1));
    Canvas.LineTo(X0 + (W + 1) * (TexSize + 1),Y * (TexSize + 1));
  End; // For Y

  // Blit the pre-masked textures

  Canvas.Pen.Color   := Color;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  If BlitBufSize > 0 Then
  Begin
    J := (TexSize Div 4) * 4;
    If J < TexSize Then Inc(J,4);
    For X := 0 To ZW Do
    Begin
      X1 := X + sbHorizontal.Position - sbHorizontal.Min;
      If X1 < ZGridWidth Then
      Begin
        For Y := 0 To ZH Do
        Begin
          Y1 := Y + sbVertical.Position - sbVertical.Min;
          If Y1 < ZGridHeight Then
          Begin
            // Get the image

            For I := 0 To TexSize - 1 Do
             Move(DestBuffer^[(Y1 * ZGridWidth + X1) * TexSize * J + I * J],PaintBMP.ScanLine[I]^,J * 4);

            // Use the next line instead to test the mask

//             Move(OverlayBuffer^[(Y1 * ZGridWidth + X1) * TexSize * J + I * J],PaintBMP.ScanLine[I]^,J * 4);
//             Move(MaskBuffer^[(Y1 * ZGridWidth + X1) * TexSize * J + I * J],PaintBMP.ScanLine[I]^,J * 4);

            If (X1 >= GXOfs) And (X1 < GXOfs + GridWidth)  And
               (Y1 >= GYOfs) And (Y1 < GYOfs + GridHeight) Then
            Begin
              I := (Y1 - GYOfs) * GridWidth + (X1 - GXOfs);
              Vis := TexVisible[I];
            End
            Else Vis := True;

            // Display it

            Canvas.Draw(X * (TexSize + 1) + X0 + 1,Y * (TexSize + 1) + 1,PaintBMP);

            // If the patch is hidden, put an "X" over it

            If Not Vis Then
            Begin
              C := Canvas.Pen.Color;
              Canvas.Pen.Color := clRed;
              X2 := X * (TexSize + 1) + X0 + 1;
              Y2 := Y * (TexSize + 1) + 1;
              Canvas.MoveTo(X2,Y2);
              Canvas.LineTo(X2 + TexSize,Y2 + TexSize);
              Canvas.MoveTo(X2 + TexSize,Y2);
              Canvas.LineTo(X2,Y2 + TexSize);
              Canvas.Pen.Color := C;
            End;
          End
          Else Canvas.Rectangle(X * (TexSize + 1) + X0 + 1,Y * (TexSize + 1) + 1,(X + 1) * (TexSize + 1) + X0 + 1,pnlBottom.Top);
        End; // For Y
      End
      Else Canvas.Rectangle(X * (TexSize + 1) + X0 + 1,0,(X + 1) * (TexSize + 1) + X0 + 1,pnlBottom.Top);
    End; // For X
  End;

  // Fill in the lower right corner

  Canvas.Rectangle(ClientWidth - SW,ClientHeight - pnlBottom.Height - SW,ClientWidth,ClientHeight - pnlBottom.Height);

  // Save the scrollbar positions

  OldSBHPos := sbHorizontal.Position;
  OldSBVPos := sbVertical.Position;
  If (pcGroundEditor.ActivePage = tbBounds) Or cbShowBounds.Checked Then Bounds.Draw;
  If pcGroundEditor.ActivePage = tbMeshes        Then Meshes.Draw;
  If pcGroundEditor.ActivePage = tbZonePlanes    Then ZonePlanes.Draw;
  If pcGroundEditor.ActivePage = tbSounds        Then Sounds.Draw;

  If cbTopographic.Checked                       Then PaintTopographic(0,0,frmTexPicker.sbVertical.Left - (frmTexPicker.Splitter1.Left + frmTexPicker.Splitter1.Width),
                                                                       frmTexPicker.sbHorizontal.Top,DefaultTopographicColor,ZoneMinZ,ZoneMaxZ);
                                                                       
  If pcGroundEditor.ActivePage = tbClipElevation Then ClipElevation.Draw;
End; // TfrmTexPicker.PaintGrid

Procedure TfrmTexPicker.PaintGridAt(X,Y: Integer);
Var
  I,J,X0 : Integer;
  X1,Y1  : Integer;
  X2,Y2  : Integer;
  C      : TColor;

Begin
  X0 := Splitter1.Left + Splitter1.Width;
  If BlitBufSize > 0 Then
  Begin
    J := (TexSize Div 4) * 4;
    If J < TexSize Then Inc(J,4);
    X1 := X + sbHorizontal.Position - sbHorizontal.Min;
    If X1 < ZGridWidth Then
    Begin
      Y1 := Y + sbVertical.Position - sbVertical.Min;
      If Y1 < ZGridHeight Then
      Begin
        // Get the image

        For I := 0 To TexSize - 1 Do
         Move(DestBuffer^[(Y1 * ZGridWidth + X1) * TexSize * J + I * J],PaintBMP.ScanLine[I]^,J * 4);

        // Display it

        Canvas.Draw(X * (TexSize + 1) + X0 + 1,Y * (TexSize + 1) + 1,PaintBMP);

        // If the patch is hidden, put an "X" over it

        If (X1 >= GXOfs) And (X1 < GXOfs + GridWidth)  And
           (Y1 >= GYOfs) And (Y1 < GYOfs + GridHeight) Then
        Begin
          I := (Y1 - GYOfs) * GridWidth + (X1 - GXOfs);
          If Not TexVisible[I] Then
          Begin
            C := Canvas.Pen.Color;
            Canvas.Pen.Color := clRed;
            X2 := X * (TexSize + 1) + X0 + 1;
            Y2 := Y * (TexSize + 1) + 1;
            Canvas.MoveTo(X2,Y2);
            Canvas.LineTo(X2 + TexSize,Y2 + TexSize);
            Canvas.MoveTo(X2 + TexSize,Y2);
            Canvas.LineTo(X2,Y2 + TexSize);
            Canvas.Pen.Color := C;
          End;
        End;
      End
      Else Canvas.Rectangle(X * (TexSize + 1) + X0 + 1,Y * (TexSize + 1) + 1,(X + 1) * (TexSize + 1) + X0 + 1,pnlBottom.Top);
    End
    Else Canvas.Rectangle(X * (TexSize + 1) + X0 + 1,0,(X + 1) * (TexSize + 1) + X0 + 1,pnlBottom.Top);
  End;
End; // TfrmTexPicker.PaintGridAt

Procedure TfrmTexPicker.PositionScrollBars;
Var I,W,H: Integer;
Begin
  Repositioning := True;

  // Go to a safe state

  sbHorizontal.Min      := 0;
  sbHorizontal.PageSize := 0;
  sbHorizontal.Max      := 100;
  sbVertical.Min        := 0;
  sbVertical.PageSize   := 0;
  sbVertical.Max        := 100;

  // Determine the dimensions of the paint area

  I := GetSystemMetrics(SM_CXHSCROLL);
  W := ClientWidth  - (Splitter1.Left + Splitter1.Width) - I;
  H := ClientHeight - pnlBottom.Height - I;

  // Position the scrollbars

  sbHorizontal.Left  := Splitter1.Left + Splitter1.Width;
  sbHorizontal.Top   := H;
  sbHorizontal.Width := W;
  sbVertical.Left    := ClientWidth - I;
  sbVertical.Top     := 0;
  sbVertical.Height  := H;

  // Set the scrollbar page sizes

  W := W Div (TexSize + 1);
  H := H Div (TexSize + 1);
  If W > ZGridWidth  Then W := ZGridWidth;
  If H > ZGridHeight Then H := ZGridHeight;
  sbHorizontal.Min         := 1;
  sbVertical.Min           := 1;
  sbHorizontal.Max         := Max(sbHorizontal.Min,ZGridWidth);
  sbVertical.Max           := Max(sbVertical.Min,ZGridHeight);
  sbHorizontal.PageSize    := W;
  sbVertical.PageSize      := H;
  sbHorizontal.LargeChange := W;
  sbVertical.LargeChange   := H;
  If sbHorizontal.Position < sbHorizontal.Min Then sbHorizontal.Position := sbHorizontal.Min;
  If sbHorizontal.Position > sbHorizontal.Max Then sbHorizontal.Position := sbHorizontal.Max;
  If sbVertical.Position   < sbVertical.Min   Then sbVertical.Position   := sbVertical.Min;
  If sbVertical.Position   > sbVertical.Max   Then sbVertical.Position   := sbVertical.Max;
  If sbHorizontal.Position > ZGridWidth  - W + 1 Then sbHorizontal.Position := ZGridWidth  - W + 1;
  If sbVertical.Position   > ZGridHeight - H + 1 Then sbVertical.Position   := ZGridHeight - H + 1;
  Repositioning    := False;
End; // TfrmTexPicker.PositionScrollBars

procedure TfrmTexPicker.FormResize(Sender: TObject);
begin
  PositionScrollBars;
  Repaint;
end;

procedure TfrmTexPicker.Splitter1Moved(Sender: TObject);
begin
  SetupDrawGrid;
  PositionScrollBars;
  tvMeshes.Invalidate;
  Repaint;
end;

procedure TfrmTexPicker.sbHorizontalChange(Sender: TObject);
Var I,W: Integer;
begin
  If Not Repositioning Then
  Begin
    I := GetSystemMetrics(SM_CXHSCROLL);
    W := ClientWidth - (Splitter1.Left + Splitter1.Width) - I;
    W := W Div (TexSize + 1);
    If W > ZGridWidth Then W := ZGridWidth;
    If sbHorizontal.Position > ZGridWidth - W + 1 Then sbHorizontal.Position := ZGridWidth - W + 1;
    If sbHorizontal.Position <> OldSBHPos Then PaintGrid;
  End;
end;

procedure TfrmTexPicker.sbVerticalChange(Sender: TObject);
Var I,H: Integer;
begin
  If Not Repositioning Then
  Begin
    I := GetSystemMetrics(SM_CXHSCROLL);
    H := ClientHeight - pnlBottom.Height - I;
    H := H Div (TexSize + 1);
    If H > ZGridHeight Then H := ZGridHeight;
    If sbVertical.Position > ZGridHeight - H + 1 Then sbVertical.Position := ZGridHeight - H + 1;
    If sbVertical.Position <> OldSBVPos Then PaintGrid;
  End;
end;

procedure TfrmTexPicker.FormPaint(Sender: TObject);
begin
  PaintGrid;
end;

procedure TfrmTexPicker.FormCreate(Sender: TObject);
begin
  glvMesh.Scene3D.Scene.DistFar := 10000;
  BlitBuffer         := Nil;
  MaskBuffer         := Nil;
  DestBuffer         := Nil;
  BlitBufSize        := 0;
  MaskBufSize        := 0;
  OverlayMaskBufSize := 0;
  OverlayBufSize     := 0;
  DestBufSize        := 0;
  PaintBMP                  := TBitmap.Create;
  PaintBMP.Width            := TexSize;
  PaintBMP.Height           := TexSize;
  PaintBMP.PixelFormat      := pf32Bit;
  BlitBMP                   := TBitmap.Create;
  BlitBMP.Width             := BlitTexSize;
  BlitBMP.Height            := BlitTexSize;
  BlitBMP.PixelFormat       := pf32Bit;
  LBMP                      := Nil;
  WBMP                      := Nil;
  ClipElevation             := TConfigurableClipElevation.Create;
  pcGroundEditor.ActivePage := tbNames;
  SetLength(SoundFiles,0);
  SetLength(SoundSizes,0);
  SetLength(SoundIndexes,0);

  Bounds                           := TConfigurableBounds.Create;
  Bounds.SelectButton              := tbBoundsSelect;
  Bounds.UndoButton                := tbBoundsUndo;
  Bounds.RedoButton                := tbBoundsRedo;
  Bounds.SnapToGridButton          := tbBoundsSnapToGrid;
  Bounds.SnapToOthersButton        := tbBoundsSnapToEndpoints;

  ZonePlanes                       := TConfigurableZonePlanes.Create;
  ZonePlanes.SelectButton          := tbZonePlanesSelect;
  ZonePlanes.UndoButton            := tbZonePlanesUndo;
  ZonePlanes.RedoButton            := tbZonePlanesRedo;
  ZonePlanes.SnapToGridButton      := tbZonePlanesSnapToGrid;
  ZonePlanes.SnapToOthersButton    := tbZonePlanesSnapToEndpoints;

  Meshes                           := TConfigurableMeshes.Create;
  Meshes.SelectButton              := tbMeshesSelect;
  Meshes.AllowMultipleSelection    := True;

  Sounds                           := TConfigurableSounds.Create;
  Sounds.SelectButton              := tbSoundsSelect;
  Sounds.UndoButton                := tbSoundsUndo;
  Sounds.RedoButton                := tbSoundsRedo;
  Sounds.SnapToGridButton          := tbSoundsSnapToGrid;
end;

procedure TfrmTexPicker.FormDestroy(Sender: TObject);
Var I: Integer;
begin
  For I := 0 To High(SoundFiles) Do FreeMem(SoundFiles[I]);
  SetLength(SoundFiles,0);
  SetLength(SoundSizes,0);
  SetLength(SoundIndexes,0);
  If BlitBuffer        <> Nil Then FreeMem(BlitBuffer,BlitBufSize);
  If MaskBuffer        <> Nil Then FreeMem(MaskBuffer,MaskBufSize);
  If OverlayMaskBuffer <> Nil Then FreeMem(OverlayMaskBuffer,OverlayMaskBufSize);
  If OverlayBuffer     <> Nil Then FreeMem(OverlayBuffer,OverlayBufSize);
  PaintBMP.Free;
  BlitBMP.Free;
  If LBMP <> Nil Then LBMP.Free;
  If WBMP <> Nil Then WBMP.Free;
  Bounds.Free;
  ZonePlanes.Free;
  Meshes.Free;
  Sounds.Free;
  ClipElevation.Free;
end;

Procedure TfrmTexPicker.RotateLeft(Src,Dest: TBitmap);
Var
  I,X,Y : Integer;
  P1,P2 : PLongArray;

Begin
  If (Src.Width        = Dest.Width)  And
     (Src.Height       = Dest.Height) And
     (Src.Width        = Src.Height)  And
     (Src.PixelFormat  = pf32Bit)     And
     (Dest.PixelFormat = pf32Bit)     Then
  Begin
    I := Src.Width - 1;
    For X := 0 To I Do
    Begin
      P2 := Dest.ScanLine[I - X];
      For Y := 0 To I Do
      Begin
        P1 := Src.ScanLine[I - Y];
        P2^[I - Y] := P1^[X];
      End; // For Y
    End; // For X
  End;
End; // TfrmTexPicker.RotateLeft

Procedure TfrmTexPicker.RotateRight(Src,Dest: TBitmap);
Var
  I,X,Y : Integer;
  P1,P2 : PLongArray;

Begin
  If (Src.Width        = Dest.Width)  And
     (Src.Height       = Dest.Height) And
     (Src.Width        = Src.Height)  And
     (Src.PixelFormat  = pf32Bit)     And
     (Dest.PixelFormat = pf32Bit)     Then
  Begin
    I := Src.Width - 1;
    For X := 0 To I Do
    Begin
      P2 := Dest.ScanLine[X];
      For Y := 0 To I Do
      Begin
        P1 := Src.ScanLine[I - Y];
        P2^[Y] := P1^[X];
      End; // For Y
    End; // For X
  End;
End; // TfrmTexPicker.RotateRight

procedure TfrmTexPicker.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Meshes.MeshCreateIndex.Clear;
  SetLength(PolyMinZ,0);
  SetLength(PolyMaxZ,0);
  If MaskBuffer <> Nil Then FreeMem(MaskBuffer,MaskBufSize);
  MaskBuffer := Nil;
  If OverlayMaskBuffer <> Nil Then FreeMem(OverlayMaskBuffer,OverlayMaskBufSize);
  OverlayMaskBuffer := Nil;
  If OverlayBuffer <> Nil Then FreeMem(OverlayBuffer,OverlayBufSize);
  OverlayBuffer := Nil;
  If DestBuffer <> Nil Then FreeMem(DestBuffer,DestBufSize);
  DestBuffer := Nil;
  SetLength(LTextures,0);
  SetLength(WTextures,0);
  SetLength(TexVisible,0);
  Bounds.Clear;
  ZonePlanes.Clear;
  Meshes.Clear;
  Sounds.Clear;
  OrigMeshes.Free;
  TreeMesh.Free;
  Tree.Free;
end;

Procedure TfrmTexPicker.DrawDestBuffer;
Var X,Y: Integer;
Begin
  For X := 0 To ZGridWidth - 1 Do
  Begin
    frmStatus.SetPosition(X / ZGridWidth);
    For Y := 0 To ZGridHeight - 1 Do DrawAtDest(X,Y);
  End; // For X
End; // TfrmTexPicker.DrawDestBuffer

Procedure TfrmTexPicker.DrawAtDest(X,Y: Integer);
Var
  TS          : Integer;
  BS          : Integer;
  I,J,K       : Integer;
  PL,PW,PM,PD : PLongWord;
  POM,PO      : PLongWord;
  PM0,PD0     : PLongWord;
  POM0,PO0    : PLongWord;
  LT,WT       : Integer;
  R           : TRect;
  Normal      : T3DPoint;
  Light       : T3DPoint;
  Illum       : Single;
  BGRA        : TBGRA;
  LookupNE    : Array[0..255] Of Integer;
  LookupSW    : Array[0..255] Of Integer;
  B1,B2,B3    : Boolean;
  LookupElev  : Array[0..((TexSize Div 4) * 4) - 1,0..TexSize - 1] Of Single;
  DZ          : Single;
  L           : LongWord;
  InGrid      : Boolean;
  Vis         : Boolean;

  Function MakeBMP(Size: Integer): TBitmap;
  Begin
    Result             := TBitmap.Create;
    Result.Width       := Size;
    Result.Height      := Size;
    Result.PixelFormat := pf32Bit;
  End; // MakeBMP

Begin
  // Figure out the bitmap widths accounting for dword padding

  TS := (TexSize Div 4) * 4;
  If TS < TexSize Then Inc(TS,4);
  BS := (BlitTexSize Div 4) * 4;
  If BS < BlitTexSize Then Inc(BS,4);

  If LBMP = Nil Then LBMP := MakeBMP(TexSize);
  If WBMP = Nil Then WBMP := MakeBMP(TexSize);

  R.Left   := 0;
  R.Top    := 0;
  R.Right  := TexSize;
  R.Bottom := TexSize;

  InGrid := (X >= GXOfs) And
            (Y >= GYOfs) And
            (X < GXOfs + GridWidth) And
            (Y < GYOfs + GridHeight);
//            (X < GXOfs + frmMain.Zone.ElevationGrid.NX) And
//            (Y < GXOfs + frmMain.Zone.ElevationGrid.NY);
  If InGrid Then
  Begin
    I := (Y - GYOfs) * GridWidth + (X - GXOfs);
    Vis := TexVisible[I];
  End
  Else Vis := True;
  Vis := Vis Or cbShowHiddenGridElements.Checked;

  If rbShadeBySlope.Checked And InGrid Then
  Begin
    // Calculate light illumination in the northeast corner

    Normal := frmMain.Zone.ElevationGrid.GetFaceNENormal(X,Y);
    Light  := T3DPoint.Create(1,1,-1);
    Light.Normalize;
    Illum := -Light.Dot(Normal);
    For I := 0 To 255 Do
    Begin
      LookupNE[I] := 64 + Round(I * Illum);
      If LookupNE[I] < 0   Then LookupNE[I] := 0;
      If LookupNE[I] > 255 Then LookupNE[I] := 255;
    End;
    Normal.Free;
    Light.Free;

    // Calculate light illumination in the southwest corner

    Normal := frmMain.Zone.ElevationGrid.GetFaceSWNormal(X,Y);
    Light  := T3DPoint.Create(1,1,-1);
    Light.Normalize;
    Illum := -Light.Dot(Normal);
    For I := 0 To 255 Do
    Begin
      LookupSW[I] := 64 + Round(I * Illum);
      If LookupSW[I] < 0   Then LookupSW[I] := 0;
      If LookupSW[I] > 255 Then LookupSW[I] := 255;
    End;
    Normal.Free;
    Light.Free;
  End;

  // Calculate illumination based on elevation

  If rbShadeByElevation.Checked And InGrid Then
  Begin
    DZ := frmMain.Zone.ElevationGrid.MaxZ - frmMain.Zone.ElevationGrid.MinZ;
    For J := 0 To TexSize - 1 Do
    Begin
      For I := 0 To TS - 1 Do
      Begin
        If DZ <> 0
         Then LookupElev[I,J] := 0.25 + 0.75 * (frmMain.Zone.ElevationGrid.GetHeightAtAbsolute(frmMain.Zone.ElevationGrid.MaxX - (Y * XStep) - (J / (TexSize - 1)) * XStep,
                                                                                               frmMain.Zone.ElevationGrid.MaxY - (X * YStep) - (I / (TS      - 1)) * YStep) - frmMain.Zone.ElevationGrid.MinZ) / DZ
         Else LookupElev[I,J] := 1;
      End; // For I
    End; // For J
  End;

  // Get the land bitmap

  If InGrid Then
  Begin
    J := (Y - GYOfs) * GridWidth + X - GXOfs;
    If (J >= 0) And (J <= High(LTextures)) Then
    Begin
      LT := LTextures[J] * BS * BlitTexSize;
      For I := 0 To BlitBMP.Height - 1 Do Move(BlitBuffer^[LT + I * BS],PLongArray(BlitBMP.ScanLine[I])^[0],BS * 4);
      LBMP.Canvas.StretchDraw(R,BlitBMP);

      // Get the water bitmap

      WT := WTextures[J] * BS * BlitTexSize;
      For I := 0 To BlitBMP.Height - 1 Do Move(BlitBuffer^[WT + I * BS],PLongArray(BlitBMP.ScanLine[I])^[0],BS * 4);
      WBMP.Canvas.StretchDraw(R,BlitBMP);
    End;
  End;

  // Put the bitmaps together

  B1   := rbShadeBySlope.Checked;
  B2   := rbShadeByElevation.Checked;
  B3   := cbShowOverlay.Checked;
  K    := (Y * ZGridWidth + X) * TexSize * TS;
  PM0  := PLongWord(MaskBuffer);
  PD0  := PLongWord(DestBuffer);
  POM0 := PLongWord(OverlayMaskBuffer);
  PO0  := PLongWord(OverlayBuffer);
  Inc(PM0,K);
  Inc(PD0,K);
  Inc(POM0,K);
  Inc(PO0,K);
  For I := 0 To TexSize - 1 Do
  Begin
    PL  := PLongWord(LBMP.ScanLine[I]);
    PW  := PLongWord(WBMP.ScanLine[I]);
    PM  := PM0;
    PD  := PD0;
    POM := POM0;
    PO  := PO0;
    Inc(PM0,TS);
    Inc(PD0,TS);
    Inc(POM0,TS);
    Inc(PO0,TS);
    For J := 0 To TS - 1 Do
    Begin
      If InGrid And Vis Then L := (PL^ And PM^) + (PW^ And (Not PM^)) Else L := ColorToRGB(Color);
      If B3 Then L := (L And POM^) + PO^;
      If B1 Then
      Begin
        BGRA := TBGRA(L);
        If J <= I Then
        Begin
          BGRA.R := LookupSW[BGRA.R];
          BGRA.G := LookupSW[BGRA.G];
          BGRA.B := LookupSW[BGRA.B];
        End
        Else
        Begin
          BGRA.R := LookupNE[BGRA.R];
          BGRA.G := LookupNE[BGRA.G];
          BGRA.B := LookupNE[BGRA.B];
        End;
        PD^ := LongWord(BGRA);
      End
      Else If B2 Then
      Begin
        BGRA   := TBGRA(L);
        BGRA.R := Round(BGRA.R * LookupElev[J,I]);
        BGRA.G := Round(BGRA.G * LookupElev[J,I]);
        BGRA.B := Round(BGRA.B * LookupElev[J,I]);
        PD^ := LongWord(BGRA);
      End
      Else PD^ := L;
      Inc(PL);
      Inc(PW);
      Inc(PM);
      Inc(PD);
      Inc(POM);
      Inc(PO);
    End; // For J
  End; // For I
End; // TfrmTexPicker.DrawAtDest

Procedure TfrmTexPicker.FillInTextureArray;
Const Noise = 0.1;
Var
  I,J,K       : Integer;
  L           : TStringList;
  P           : TPolygon;
  GX,GY       : Integer;
//  Textures   : String;
//  Opacities  : String;
//  Parameters : String;

Begin
  // First make an ORDERED texture list

  L        := TStringList.Create;
  L.Sorted := True;
  For I := 1 To TextureLibrary.Count - 1 Do L.AddObject(TextureLibrary.Strings[I],Pointer(I - 1));

  // Get the texture visibility info

  For I := 0 To High(TexVisible) Do TexVisible[I] := frmMain.Zone.ElevationGrid.Visible[I];

  // Now go through the polygons and assign the texture values (doing land polygons first)

  If MO1 <> Nil Then
  Begin
    frmStatus.SetCaption('Determining textures and orientations...Pass 1');
    For I := 0 To MO1.Polygons.Count - 1 Do
    Begin
      frmStatus.SetPosition(I / MO1.Polygons.Count);

      P  := TPolygon(MO1.Polygons.Objects[I]);
//      BreakupTextureString(P.Texture,Textures,Opacities,Parameters);
      GX := P.Tag Mod GridWidth;
      GY := P.Tag Div GridWidth;
      J  := L.IndexOf(P.TextureInfo.FirstTexture);//GetToken(';',Textures)); //(P.Texture);
      If Not P.HasAngle Then P.CalculateAngle(MO1);
      Case P.Angle Of
        0: K := 0;
       90: K := 1;
      180: K := 2;
      270: K := 3;
      Else
        K := 0;
      End; // Case
      If J >= 0 Then LTextures[GY * GridWidth + GX] := Integer(L.Objects[J]) * 4 + K;
    End; // For I
  End;

  // Do the same for the underwater polygons

  If MO2 <> Nil Then
  Begin
    frmStatus.SetCaption('Determining textures and orientations...Pass 2');
    For I := 0 To MO2.Polygons.Count - 1 Do
    Begin
      frmStatus.SetPosition(I / MO2.Polygons.Count);

      P  := TPolygon(MO2.Polygons.Objects[I]);
//      BreakupTextureString(P.Texture,Textures,Opacities,Parameters);
      GX := P.Tag Mod GridWidth;
      GY := P.Tag Div GridWidth;
      J  := L.IndexOf(P.TextureInfo.FirstTexture);//GetToken(';',Textures)); // (P.Texture);
      If Not P.HasAngle Then P.CalculateAngle(MO2);
      Case P.Angle Of
        0: K := 0;
       90: K := 1;
      180: K := 2;
      270: K := 3;
      Else
        K := 0;
      End; // Case
      If J >= 0 Then WTextures[GY * GridWidth + GX] := Integer(L.Objects[J]) * 4 + K;
    End; // For I
  End;
  L.Free;
End; // TfrmTexPicker.FillInTextureArray

procedure TfrmTexPicker.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Var SB,SP: Integer;

  Procedure DrawCursor(X,Y: Integer; C: TColor);
  Begin
    X := SP + X * (TexSize + 1);
    Y := Y * (TexSize + 1);
    Canvas.Pen.Color := C;
    Canvas.MoveTo(X,Y);
    Canvas.LineTo(X + TexSize + 1,Y);
    Canvas.LineTo(X + TexSize + 1,Y + TexSize + 1);
    Canvas.LineTo(X,Y + TexSize + 1);
    Canvas.LineTo(X,Y);
  End; // DrawCursor

begin
  If (pcGroundEditor.ActivePage = tbNames) Or
     (pcGroundEditor.ActivePage = tbIcons) Then
  Begin
    SP := Splitter1.Left + Splitter1.Width;
    SB := GetSystemMetrics(SM_CXHSCROLL);
    If (X >= SP)                                  And
       (X < ClientWidth - SB)                     And
       (Y < ClientHeight - pnlBottom.Height - SB) Then
    Begin
      X := (X - SP) Div (TexSize + 1);
      Y := Y Div (TexSize + 1);
      If (X + (sbHorizontal.Position - sbHorizontal.Min) >= GXOfs) And
         (X + (sbHorizontal.Position - sbHorizontal.Min) <  GXOfs + GridWidth)  And
         (Y + (sbVertical.Position   - sbVertical.Min)   >= GYOfs) And
         (Y + (sbVertical.Position   - sbVertical.Min)   <  GYOfs + GridHeight) Then
      Begin
        If ((X <> CursorX) Or (Y <> CursorY)) And (CursorX >= 0) And (CursorY >= 0) Then DrawCursor(CursorX,CursorY,clBlack);
        CursorX := X;
        CursorY := Y;
        DrawCursor(CursorX,CursorY,clRed);
        DropTexture;
        If cbShowBounds.Checked  Then Bounds.Draw;
        If cbTopographic.Checked Then PaintTopographic(0,0,frmTexPicker.sbVertical.Left - (frmTexPicker.Splitter1.Left + frmTexPicker.Splitter1.Width),
                                                       frmTexPicker.sbHorizontal.Top,DefaultTopographicColor,ZoneMinZ,ZoneMaxZ);
      End;
    End
    Else
    Begin
      If (CursorX >= 0) And (CursorY >= 0) Then DrawCursor(CursorX,CursorY,clBlack);
      CursorX := -1;
      CursorY := -1;
    End;
  End
  Else If pcGroundEditor.ActivePage = tbBounds        Then Bounds.DoMouseMove(X,Y)
  Else If pcGroundEditor.ActivePage = tbMeshes        Then Meshes.DoMouseMove(X,Y)
  Else If pcGroundEditor.ActivePage = tbZonePlanes    Then ZonePlanes.DoMouseMove(X,Y)
  Else If pcGroundEditor.ActivePage = tbSounds        Then Sounds.DoMouseMove(X,Y)
  Else If pcGroundEditor.ActivePage = tbClipElevation Then ClipElevation.DoMouseMove(X,Y);
end;

procedure TfrmTexPicker.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseIsDown := True;
  If ((pcGroundEditor.ActivePage = tbNames) Or
     (pcGroundEditor.ActivePage = tbIcons)) And
     (Button = mbLeft)                      Then
  Begin
    Drawing := True;
    DropTexture;
  End
  Else If pcGroundEditor.ActivePage = tbBounds        Then Bounds.DoMouseDown(X,Y)
  Else If pcGroundEditor.ActivePage = tbMeshes        Then Meshes.DoMouseDown(X,Y)
  Else If pcGroundEditor.ActivePage = tbZonePlanes    Then ZonePlanes.DoMouseDown(X,Y)
  Else If pcGroundEditor.ActivePage = tbSounds        Then Sounds.DoMouseDown(X,Y)
  Else If pcGroundEditor.ActivePage = tbClipElevation Then ClipElevation.DoMouseDown(X,Y);
end;

procedure TfrmTexPicker.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var SB,SP: Integer;
begin
  MouseIsDown := False;
  If (pcGroundEditor.ActivePage = tbNames) Or
     (pcGroundEditor.ActivePage = tbIcons) Then
  Begin
    SP := Splitter1.Left + Splitter1.Width;
    SB := GetSystemMetrics(SM_CXHSCROLL);
    If (X >= SP)                                  And
       (X < ClientWidth - SB)                     And
       (Y < ClientHeight - pnlBottom.Height - SB) Then
    Begin
      X := (X - SP) Div (TexSize + 1) - GXOfs + (sbHorizontal.Position - sbHorizontal.Min);
      Y := Y        Div (TexSize + 1) - GYOfs + (sbVertical.Position   - sbVertical.Min);
      If (X >= 0) And (X < GridWidth)  And
         (Y >= 0) And (Y < GridHeight) Then
      Begin
        If Button = mbRight Then
        Begin
          TexVisible[Y * GridWidth + X] := Not TexVisible[Y * GridWidth + X];
          PaintGridAt(CursorX,CursorY);
        End;
      End;
    End;
    Drawing := False;
  End
  Else If pcGroundEditor.ActivePage = tbBounds        Then Bounds.DoMouseUp(X,Y)
  Else If pcGroundEditor.ActivePage = tbMeshes        Then Meshes.DoMouseUp(X,Y)
  Else If pcGroundEditor.ActivePage = tbZonePlanes    Then ZonePlanes.DoMouseUp(X,Y)
  Else If pcGroundEditor.ActivePage = tbSounds        Then Sounds.DoMouseUp(X,Y)
  Else If pcGroundEditor.ActivePage = tbClipElevation Then ClipElevation.DoMouseUp(X,Y);
end;

procedure TfrmTexPicker.FormDeactivate(Sender: TObject);
begin
  Drawing := False;
end;

procedure TfrmTexPicker.FormActivate(Sender: TObject);
begin
  Drawing := False;
end;

Procedure TfrmTexPicker.DropTexture;
Var X,Y: Integer;
Begin
  If Drawing And (CursorX >= 0) And (CursorY >= 0) Then
  Begin
    X := CursorX + (sbHorizontal.Position - sbHorizontal.Min);
    Y := CursorY + (sbVertical.Position   - sbVertical.Min);
    If (X >= GXOfs) And (Y >= GYOfs) And (X < GXOfs + GridWidth) And (Y < GYOfs + GridHeight) Then
    Begin
      If rbLand.Checked
       Then LTextures[(Y - GYOfs) * GridWidth + X - GXOfs] := SelectedTex * 4 + Angle
       Else WTextures[(Y - GYOfs) * GridWidth + X - GXOfs] := SelectedTex * 4 + Angle;
      DrawAtDest(X,Y);
      PaintGridAt(CursorX,CursorY);
      If cbShowBounds.Checked  Then Bounds.Draw;
      If cbTopographic.Checked Then PaintTopographic(0,0,frmTexPicker.sbVertical.Left - (frmTexPicker.Splitter1.Left + frmTexPicker.Splitter1.Width),
                                                     frmTexPicker.sbHorizontal.Top,DefaultTopographicColor,ZoneMinZ,ZoneMaxZ);
    End;
  End;
End; // TfrmTexPicker.DropTexture

procedure TfrmTexPicker.btnOkClick(Sender: TObject);
begin
  SaveChanges;
end;

Procedure TfrmTexPicker.SaveChanges;
Var
  I,J,K  : Integer;
  P      : TPolygon;
  V      : T3DPoint;
  GX,GY  : Integer;
  CX,CY  : Double;
  ML,ML1 : TMeshLibraryObjectReference;

  Function DeleteUnusedMesh(ZO: TZoneObject): Boolean;
  Var
    I  : Integer;
    GO : TGroupObject;

  Begin
    Result := False;
    If ZO Is TMeshLibraryObjectReference Then
    Begin
      I := OrigMeshes.IndexOfObject(ZO);
      If I < 0 Then
      Begin
        If ZO.GetParent <> Nil
         Then ZO.GetParent.Objects.Delete(ZO.GetParent.Objects.IndexOfObject(ZO))
         Else frmMain.Zone.Delete(frmMain.Zone.IndexOfObject(ZO));
        ZO.Free;
        Result := True;
      End;
    End
    Else If ZO Is TGroupObject Then
    Begin
      GO := TGroupObject(ZO);
      I  := 0;
      While I < GO.Objects.Count Do
      Begin
        If Not DeleteUnusedMesh(TZoneObject(GO.Objects.Objects[I])) Then Inc(I);
      End; // While
    End;
  End; // DeleteUnusedMesh

  Procedure ConvertMeshCoordinates(ZO: TZoneObject);

    Procedure ClimbParent(ZO,Parent: TZoneObject);
    Begin
      If Parent <> Nil Then
      Begin
        ClimbParent(ZO,Parent.GetParent);
        ZO.Loc.Subtract(Parent.Loc);
        If Parent.Size.X <> 0 Then ZO.Loc.X := ZO.Loc.X / Parent.Size.X;
        If Parent.Size.Y <> 0 Then ZO.Loc.Y := ZO.Loc.Y / Parent.Size.Y;
        If Parent.Size.Z <> 0 Then ZO.Loc.Z := ZO.Loc.Z / Parent.Size.Z;
        Parent.Rotate.NegativeRotate(ZO.Loc);
        If Parent.Size.X <> 0 Then ZO.Size.X := ZO.Size.X / Parent.Size.X;
        If Parent.Size.Y <> 0 Then ZO.Size.Y := ZO.Size.Y / Parent.Size.Y;
        If Parent.Size.Z <> 0 Then ZO.Size.Z := ZO.Size.Z / Parent.Size.Z;
      End;
    End; // ClimbParent

  Begin
    // Traverse the object's parents in top-down order

    ClimbParent(ZO,ZO.GetParent);
  End; // ConvertMeshCoordinates

Begin
  frmStatus.Show;

  // Save the texture visibility info

  frmStatus.SetCaption('Saving patch visbility');
  For I := 0 To High(TexVisible) Do frmMain.Zone.ElevationGrid.Visible[I] := TexVisible[I];

  // Save the land polygons

  frmStatus.SetCaption('Saving land polygons');
  If MO1 <> Nil Then
  Begin
    For I := 0 To MO1.Polygons.Count - 1 Do
    Begin
      frmStatus.SetPosition(I / MO1.Polygons.Count);

      P              := TPolygon(MO1.Polygons.Objects[I]);
      GX             := P.Tag Mod GridWidth;
      GY             := P.Tag Div GridWidth;
      P.SetSingleTexture(TextureLibrary[(LTextures[GY * GridWidth + GX] Div 4) + 1],'');
      J              := LTextures[GY * GridWidth + GX] And 3;
      P.HasTexCoords :=  True;
      SetLength(P.TX,High(P.Vertices) + 1);
      SetLength(P.TZ,High(P.Vertices) + 1);
      CX             := GMaxY - GX * YStep;
      CY             := GMaxX - GY * XStep;
      P.HasAngle     := True;
      P.Angle        := 90 * (LTextures[GY * GridWidth + GX] And 3);
      For K := 0 To High(P.Vertices) Do
      Begin
        V := T3DPoint(MO1.Vertices.Objects[P.Vertices[K]]);
        Case J Of
          0: Begin
               P.TX[K] := ((GX + 1) * 1) + 1/TextureSize - (MO1.Loc.Y + V.Y - CX) * ((1 - 2/TextureSize) / YStep);
               P.TZ[K] := (GY       * 1) - 1/TextureSize + (MO1.Loc.X + V.X - CY) * ((1 - 2/TextureSize) / XStep);
             End;
          1: Begin
               P.TZ[K] := (GX       * 1) - 1/TextureSize + (MO1.Loc.Y + V.Y - CX) * ((1 - 2/TextureSize) / YStep);
               P.TX[K] := (GY       * 1) - 1/TextureSize + (MO1.Loc.X + V.X - CY) * ((1 - 2/TextureSize) / XStep);
             End;
          2: Begin
               P.TX[K] := (GX       * 1) - 1/TextureSize + (MO1.Loc.Y + V.Y - CX) * ((1 - 2/TextureSize) / YStep);
               P.TZ[K] := ((GY + 1) * 1) + 1/TextureSize - (MO1.Loc.X + V.X - CY) * ((1 - 2/TextureSize) / XStep);
             End;
          3: Begin
               P.TZ[K] := ((GX + 1) * 1) + 1/TextureSize - (MO1.Loc.Y + V.Y - CX) * ((1 - 2/TextureSize) / YStep);
               P.TX[K] := ((GY + 1) * 1) + 1/TextureSize - (MO1.Loc.X + V.X - CY) * ((1 - 2/TextureSize) / XStep);
             End;
        End; // Case
{
        Case J Of
          0: Begin
               P.TX[K] := Round(((GX + 1) * TextureSize) + 1 - (MO1.Loc.Y + V.Y - CX) * ((TextureSize - 2) / YStep));
               P.TZ[K] := Round((GY       * TextureSize) - 1 + (MO1.Loc.X + V.X - CY) * ((TextureSize - 2) / XStep));
             End;
          1: Begin
               P.TZ[K] := Round((GX       * TextureSize) - 1 + (MO1.Loc.Y + V.Y - CX) * ((TextureSize - 2) / YStep));
               P.TX[K] := Round((GY       * TextureSize) - 1 + (MO1.Loc.X + V.X - CY) * ((TextureSize - 2) / XStep));
             End;
          2: Begin
               P.TX[K] := Round((GX       * TextureSize) - 1 + (MO1.Loc.Y + V.Y - CX) * ((TextureSize - 2) / YStep));
               P.TZ[K] := Round(((GY + 1) * TextureSize) + 1 - (MO1.Loc.X + V.X - CY) * ((TextureSize - 2) / XStep));
             End;
          3: Begin
               P.TZ[K] := Round(((GX + 1) * TextureSize) + 1 - (MO1.Loc.Y + V.Y - CX) * ((TextureSize - 2) / YStep));
               P.TX[K] := Round(((GY + 1) * TextureSize) + 1 - (MO1.Loc.X + V.X - CY) * ((TextureSize - 2) / XStep));
             End;
        End; // Case
}
      End; // For K
    End; // For I
  End;

  // Save the underwater polygons

  frmStatus.SetCaption('Saving underwater polygons');
  If MO2 <> Nil Then
  Begin
    For I := 0 To MO2.Polygons.Count - 1 Do
    Begin
      frmStatus.SetPosition(I / MO2.Polygons.Count);

      P              := TPolygon(MO2.Polygons.Objects[I]);
      GX             := P.Tag Mod GridWidth;
      GY             := P.Tag Div GridWidth;
      P.SetSingleTexture(TextureLibrary[(WTextures[GY * GridWidth + GX] Div 4) + 1],'');
      J              := WTextures[GY * GridWidth + GX] And 3;
      P.HasTexCoords :=  True;
      SetLength(P.TX,High(P.Vertices) + 1);
      SetLength(P.TZ,High(P.Vertices) + 1);
      CX             := GMaxY - GX * YStep;
      CY             := GMaxX - GY * XStep;
      P.HasAngle     := True;
      P.Angle        := 90 * (WTextures[GY * GridWidth + GX] And 3);
      For K := 0 To High(P.Vertices) Do
      Begin
        V := T3DPoint(MO2.Vertices.Objects[P.Vertices[K]]);
        Case J Of
          0: Begin
               P.TX[K] := ((GX + 1) * 1) + 1/TextureSize - (MO2.Loc.Y + V.Y - CX) * ((1 - 2/TextureSize) / YStep);
               P.TZ[K] := (GY       * 1) - 1/TextureSize + (MO2.Loc.X + V.X - CY) * ((1 - 2/TextureSize) / XStep);
             End;
          1: Begin
               P.TZ[K] := (GX       * 1) - 1/TextureSize + (MO2.Loc.Y + V.Y - CX) * ((1 - 2/TextureSize) / YStep);
               P.TX[K] := (GY       * 1) - 1/TextureSize + (MO2.Loc.X + V.X - CY) * ((1 - 2/TextureSize) / XStep);
             End;
          2: Begin
               P.TX[K] := (GX       * 1) - 1/TextureSize + (MO2.Loc.Y + V.Y - CX) * ((1 - 2/TextureSize) / YStep);
               P.TZ[K] := ((GY + 1) * 1) + 1/TextureSize - (MO2.Loc.X + V.X - CY) * ((1 - 2/TextureSize) / XStep);
             End;
          3: Begin
               P.TZ[K] := ((GX + 1) * 1) + 1/TextureSize - (MO2.Loc.Y + V.Y - CX) * ((1 - 2/TextureSize) / YStep);
               P.TX[K] := ((GY + 1) * 1) + 1/TextureSize - (MO2.Loc.X + V.X - CY) * ((1 - 2/TextureSize) / XStep);
             End;
        End; // Case
{        
        Case J Of
          0: Begin
               P.TX[K] := Round(((GX + 1) * TextureSize) + 1 - (MO2.Loc.Y + V.Y - CX) * ((TextureSize - 2) / YStep));
               P.TZ[K] := Round((GY       * TextureSize) - 1 + (MO2.Loc.X + V.X - CY) * ((TextureSize - 2) / XStep));
             End;
          1: Begin
               P.TZ[K] := Round((GX       * TextureSize) - 1 + (MO2.Loc.Y + V.Y - CX) * ((TextureSize - 2) / YStep));
               P.TX[K] := Round((GY       * TextureSize) - 1 + (MO2.Loc.X + V.X - CY) * ((TextureSize - 2) / XStep));
             End;
          2: Begin
               P.TX[K] := Round((GX       * TextureSize) - 1 + (MO2.Loc.Y + V.Y - CX) * ((TextureSize - 2) / YStep));
               P.TZ[K] := Round(((GY + 1) * TextureSize) + 1 - (MO2.Loc.X + V.X - CY) * ((TextureSize - 2) / XStep));
             End;
          3: Begin
               P.TZ[K] := Round(((GX + 1) * TextureSize) + 1 - (MO2.Loc.Y + V.Y - CX) * ((TextureSize - 2) / YStep));
               P.TX[K] := Round(((GY + 1) * TextureSize) + 1 - (MO2.Loc.X + V.X - CY) * ((TextureSize - 2) / XStep));
             End;
        End; // Case
}
      End; // For K
    End; // For I
  End;

  // Copy the bounds back to the zone

  For I := 0 To frmMain.Zone.Bounds.Count - 1 Do frmMain.Zone.Bounds.Objects[I].Free;
  frmMain.Zone.Bounds.Clear;
  For I := 0 To Bounds.Items.Count - 1 Do frmMain.Zone.Bounds.AddObject('',TZoneBound.Create(TZoneBound(Bounds.Items.Objects[I])));
  frmMain.Zone.BuildBoundingPolygons;

  // Copy the zone planes back to the zone

  For I := 0 To frmMain.Zone.ZonePlanes.Count - 1 Do frmMain.Zone.ZonePlanes.Objects[I].Free;
  frmMain.Zone.ZonePlanes.Clear;
  For I := 0 To ZonePlanes.Items.Count - 1 Do frmMain.Zone.ZonePlanes.AddObject('',TZonePlane.Create(TZonePlane(ZonePlanes.Items.Objects[I])));

  // Get rid of any meshes in the zone that aren't in our OrigMeshes list

  I := 0;
  While I < frmMain.Zone.Count Do
  Begin
    If Not DeleteUnusedMesh(TZoneObject(frmMain.Zone.Objects[I])) Then Inc(I);
  End; // While

  // Copy the meshes back to the zone

  frmStatus.SetCaption('Saving mesh objects');
  For I := 0 To Meshes.Items.Count - 1 Do
  Begin
    frmStatus.SetPosition(I / Meshes.Items.Count);
    ML := TMeshLibraryObjectReference(Meshes.Items.Objects[I]);
    If I < OrigMeshes.Count Then
    Begin
      ML1 := TMeshLibraryObjectReference(OrigMeshes.Objects[I]);

      ML1.Loc.Copy(ML.Loc);
      ML1.Rotate.Copy(ML.Rotate);
      ML1.Size.Copy(ML.Size);

      // If the original mesh is in a group, we need to convert from absolute coordinates

      ConvertMeshCoordinates(ML1);
    End
    Else
    Begin
      ML1 := TMeshLibraryObjectReference.Create(ML);
      frmMain.Zone.AddObject(ML.GetName,ML1);
    End;
  End; // For I

  // Copy the sounds back to the zone

  For I := 0 To frmMain.Zone.Sounds.Count - 1 Do frmMain.Zone.Sounds.Objects[I].Free;
  frmMain.Zone.Sounds.Clear;
  For I := 0 To Sounds.Items.Count - 1 Do frmMain.Zone.Sounds.AddObject('',TSound.Create(TSound(Sounds.Items.Objects[I])));

  // Refresh the main form and close this one

  frmMain.LoadObjectTreeView;
  frmStatus.Visible := False;
End; // TfrmTexPicker.SaveChanges

Procedure TfrmTexPicker.SetupDrawGrid;
Var SW,X,Y: Integer;
Begin
  SW := GetSystemMetrics(SM_CXHSCROLL);

  // First see if everything will fit without a scrollbar

  dgTextures.ColCount := 0;
  dgTextures.RowCount := 0;
  X := (dgTextures.ClientWidth - 1)  Div BlitTexSize;
  Y := (dgTextures.ClientHeight - 1) Div BlitTexSize;
  If X < 1 Then X := 1;
  If Y < 1 Then Y := 1;
  If X * Y < lbTextures.Items.Count Then
  Begin
    X := (dgTextures.ClientWidth - 1 - SW) Div BlitTexSize;
    If X < 1 Then X := 1;
  End;
  Y := lbTextures.Items.Count Div X;
  If Y * X < lbTextures.Items.Count Then Inc(Y);
  dgTextures.ColCount := X;
  dgTextures.RowCount := Y;
End; // TfrmTexPicker.SetupDrawGrid

procedure TfrmTexPicker.dgTexturesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
Var I,J,BS: Integer;
begin
  If BlitBuffer <> Nil Then
  Begin
    // Figure out which bitmap to get

    J := ARow * dgTextures.ColCount + ACol;
    If (J >= 0) And (J < lbTextures.Items.Count) Then
    Begin
      // Figure out the width, accounting for dword padding

      BS := (BlitTexSize Div 4) * 4;
      If BS < BlitTexSize Then Inc(BS,4);

      // Get the bitmap and transfer it to the swatch

      For I := 0 To BlitBMP.Height - 1 Do Move(BlitBuffer^[(J * 4 * BS * BlitTexSize) + I * BS],PLongArray(BlitBMP.ScanLine[I])^[0],BS * 4);
      dgTextures.Canvas.Draw(Rect.Left,Rect.Top,BlitBMP);
    End;
  End;
end;

procedure TfrmTexPicker.dgTexturesClick(Sender: TObject);
Var I,J,BS: Integer;
begin
  If BlitBuffer <> Nil Then
  Begin
    // Figure out which bitmap to get

    J := dgTextures.Row * dgTextures.ColCount + dgTextures.Col;
    If (J >= 0) And (J < lbTextures.Items.Count) Then
    Begin
      // Figure out the width, accounting for dword padding

      BS := (BlitTexSize Div 4) * 4;
      If BS < BlitTexSize Then Inc(BS,4);

      // When the user first selects a bitmap, default the rotation to 0 degrees, otherwise rotate it every time they click on it

      If SelectedTex <> J
       Then Angle := 0
       Else Angle := (Angle + 1) And 3;
      SelectedTex := J;

      // Get the bitmap and transfer it to the swatch

      For I := 0 To BlitBMP.Height - 1 Do Move(BlitBuffer^[((SelectedTex * 4 + Angle) * BS * BlitTexSize) + I * BS],PLongArray(BlitBMP.ScanLine[I])^[0],BS * 4);
      imgTexture.Picture.Bitmap.Assign(BlitBMP);
      lblCurrentTex.Caption := lbTextures.Items.Strings[J];
    End
    Else lblCurrentTex.Caption := '';
  End;
end;

procedure TfrmTexPicker.lbTexturesDblClick(Sender: TObject);
begin
  lbTexturesClick(Self);
end;

procedure TfrmTexPicker.dgTexturesDblClick(Sender: TObject);
begin
  dgTexturesClick(Self);
end;

Procedure TfrmTexPicker.DrawBounds;
Var
  I  : Integer;
  B  : TZoneBound;
  X0 : Integer;
  X  : Single;
  Y  : Single;
  X1 : Integer;
  Y1 : Integer;
  X2 : Integer;
  Y2 : Integer;
  A  : Single;

  Procedure DrawBound(I: Integer);
  Var Region: THandle;
  Begin
    // Clip to the drawing area

    Region := CreateRectRgn(0,0,0,0);
    SetRectRgn(Region,Splitter1.Left + Splitter1.Width,0,sbVertical.Left,sbHorizontal.Top);
    SelectClipRgn(Canvas.Handle,Region);

    // Convert the bound to screen coordinates

    B  := TZoneBound(Bounds.Items.Objects[I]);
    X1 := Round(ZoneYToScreenXCoord(B.Y1));
    Y1 := Round(ZoneXToScreenYCoord(B.X1));
    X2 := Round(ZoneYToScreenXCoord(B.Y2));
    Y2 := Round(ZoneXToScreenYCoord(B.X2));

    // Draw the bound line

    Canvas.MoveTo(X1,Y1);
    Canvas.LineTo(X2,Y2);
    Canvas.Rectangle(X1 - 2,Y1 - 2,X1 + 3,Y1 + 3);
    Canvas.Rectangle(X2 - 2,Y2 - 2,X2 + 3,Y2 + 3);

    // Draw a short normal vector

    A  := ArcTan2(Y2 - Y1,X2 - X1) - (Pi / 2);
    X1 := (X1 + X2) Div 2;
    Y1 := (Y1 + Y2) Div 2;
    Canvas.MoveTo(X1,Y1);
    Inc(X1,Round(16 * Cos(A)));
    Inc(Y1,Round(16 * Sin(A)));
    Canvas.LineTo(X1,Y1);
    Canvas.LineTo(X1 + Round(8 * Cos(A - 3 * (Pi / 4))),Y1 + Round(8 * Sin(A - 3 * (Pi / 4))));
    Canvas.MoveTo(X1,Y1);
    Canvas.LineTo(X1 + Round(8 * Cos(A + 3 * (Pi / 4))),Y1 + Round(8 * Sin(A + 3 * (Pi / 4))));

    // Go back to no clipping

    SelectClipRgn(Canvas.Handle,0);
    DeleteObject(Region);
  End; // DrawBound

Begin
  X0 := Splitter1.Left + Splitter1.Width;
  X  := (sbHorizontal.Position - sbHorizontal.Min) * YStep;
  Y  := (sbVertical.Position   - sbVertical.Min)   * XStep;
  For I := 0 To Bounds.Items.Count - 1 Do
  Begin
    If (I = Bounds.SelectedIndex) And (pcGroundEditor.ActivePage = tbBounds) Then
    Begin
      Canvas.Pen.Color   := clRed;
      Canvas.Brush.Color := clRed;
    End
    Else
    Begin
      Canvas.Pen.Color   := clYellow;
      Canvas.Brush.Color := clYellow;
    End;
    DrawBound(I);
  End; // For I

  // Draw the selected bound

  If (Bounds.SelectedIndex >= 0) And (pcGroundEditor.ActivePage = tbBounds) Then
  Begin
    Canvas.Pen.Color   := clRed;
    Canvas.Brush.Color := clRed;
    DrawBound(Bounds.SelectedIndex);
  End;
End; // TfrmTexPicker.DrawBounds

Procedure TfrmTexPicker.DrawZoneLines;
Var
  I  : Integer;
  B  : TZonePlane;
  X0 : Integer;
  X  : Single;
  Y  : Single;
  X1 : Integer;
  Y1 : Integer;
  X2 : Integer;
  Y2 : Integer;

  Procedure DrawZoneLine(I: Integer);
  Var Region: THandle;
  Begin
    // Clip to the drawing area

    Region := CreateRectRgn(0,0,0,0);
    SetRectRgn(Region,Splitter1.Left + Splitter1.Width,0,sbVertical.Left,sbHorizontal.Top);
    SelectClipRgn(Canvas.Handle,Region);

    // Convert the zone line to screen coordinates

    B  := TZonePlane(ZonePlanes.Items.Objects[I]);
    X1 := Round(ZoneYToScreenXCoord(B.Y1));
    Y1 := Round(ZoneXToScreenYCoord(B.X1));
    X2 := Round(ZoneYToScreenXCoord(B.Y2));
    Y2 := Round(ZoneXToScreenYCoord(B.X2));

    // Draw the zone line

    Canvas.MoveTo(X1,Y1);
    Canvas.LineTo(X2,Y2);
    Canvas.Rectangle(X1 - 2,Y1 - 2,X1 + 3,Y1 + 3);
    Canvas.Rectangle(X2 - 2,Y2 - 2,X2 + 3,Y2 + 3);

    // Go back to no clipping

    SelectClipRgn(Canvas.Handle,0);
    DeleteObject(Region);
  End; // DrawZoneLine

Begin
  X0 := Splitter1.Left + Splitter1.Width;
  X  := (sbHorizontal.Position - sbHorizontal.Min) * YStep;
  Y  := (sbVertical.Position   - sbVertical.Min)   * XStep;
  For I := 0 To ZonePlanes.Items.Count - 1 Do
  Begin
    If (I = ZonePlanes.SelectedIndex) And (pcGroundEditor.ActivePage = tbZonePlanes) Then
    Begin
      Canvas.Pen.Color   := clRed;
      Canvas.Brush.Color := clRed;
    End
    Else
    Begin
      Canvas.Pen.Color   := clBlue;
      Canvas.Brush.Color := clBlue;
    End;
    DrawZoneLine(I);
  End; // For I

  // Draw the selected zoneline

  If (ZonePlanes.SelectedIndex >= 0) And (pcGroundEditor.ActivePage = tbZonePlanes) Then
  Begin
    Canvas.Pen.Color   := clRed;
    Canvas.Brush.Color := clRed;
    DrawZoneLine(ZonePlanes.SelectedIndex);
  End;
End; // TfrmTexPicker.DrawZoneLines

Procedure TfrmTexPicker.DrawMeshes;
Var
  I   : Integer;
  ML  : TMeshLibraryObjectReference;
  X0  : Integer;
  X   : Single;
  Y   : Single;
  Cur : String;

  Procedure DrawMesh(ML: TMeshLibraryObjectReference);
  Var
    Region : THandle;
    X,Y    : Integer;

  Begin
    // Clip to the drawing area

    Region := CreateRectRgn(0,0,0,0);
    SetRectRgn(Region,Splitter1.Left + Splitter1.Width,0,sbVertical.Left,sbHorizontal.Top);
    SelectClipRgn(Canvas.Handle,Region);

    // Convert the mesh to screen coordinates

    X := Round(ZoneYToScreenXCoord(ML.Loc.Y));
    Y := Round(ZoneXToScreenYCoord(ML.Loc.X));

    // Draw the mesh line

    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle(X - 2,Y - 2,X + 3,Y + 3);
    Canvas.Brush.Style := bsClear;
    Canvas.Ellipse(X - 7,Y - 7,X + 8,Y + 8);
    Canvas.Brush.Style := bsSolid;

    // Go back to no clipping

    SelectClipRgn(Canvas.Handle,0);
    DeleteObject(Region);
  End; // DrawMesh

Begin
  X0 := Splitter1.Left + Splitter1.Width;
  X  := (sbHorizontal.Position - sbHorizontal.Min) * YStep;
  Y  := (sbVertical.Position   - sbVertical.Min)   * XStep;
  Cur := SelectedMeshObject;
{
  If lbMeshes.ItemIndex >= 0
   Then Cur := lbMeshes.Items.Strings[lbMeshes.ItemIndex]
   Else Cur := '';
}
  For I := 0 To Meshes.Items.Count - 1 Do
  Begin
    ML := TMeshLibraryObjectReference(Meshes.Items.Objects[I]);
    If (Meshes.Items.Strings[I] <> '') And (pcGroundEditor.ActivePage = tbMeshes) Then
    Begin
      Canvas.Pen.Color   := clRed;
      Canvas.Brush.Color := clRed;
    End
    Else
    Begin
      If (Length(Cur) > 0) And (ML.Group.GetName = Cur) Then
      Begin                         
        Canvas.Pen.Color   := clFuchsia;
        Canvas.Brush.Color := clFuchsia;
      End
      Else
      Begin
        Canvas.Pen.Color   := clYellow;
        Canvas.Brush.Color := clYellow;
      End;
    End;
    DrawMesh(ML);
  End; // For I
End; // TfrmTexPicker.DrawMeshes

Procedure TfrmTexPicker.DrawSounds;
Var
  I  : Integer;
  S  : TSound;
  X0 : Integer;
  X  : Single;
  Y  : Single;
  X1 : Integer;
  Y1 : Integer;
  R1 : Integer;

  Procedure DrawSound(I: Integer);
  Var Region: THandle;
  Begin
    // Clip to the drawing area

    Region := CreateRectRgn(0,0,0,0);
    SetRectRgn(Region,Splitter1.Left + Splitter1.Width,0,sbVertical.Left,sbHorizontal.Top);
    SelectClipRgn(Canvas.Handle,Region);

    // Convert the zone line to screen coordinates

    S  := TSound(Sounds.Items.Objects[I]);
    X1 := Round(ZoneYToScreenXCoord(S.Y));
    Y1 := Round(ZoneXToScreenYCoord(S.X));
    R1 := Round(S.Radius * ((TexSize + 1) / XStep));

    // Draw the sound line

    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle(X1 - 2,Y1 - 2,X1 + 3,Y1 + 3);
    Canvas.Brush.Style := bsClear;
    Canvas.Ellipse(X1 - R1,Y1 - R1,X1 + R1 + 1,Y1 + R1 + 1);
    Canvas.Brush.Style := bsSolid;

    // Go back to no clipping

    SelectClipRgn(Canvas.Handle,0);
    DeleteObject(Region);
  End; // DrawSound

Begin
  X0 := Splitter1.Left + Splitter1.Width;
  X  := (sbHorizontal.Position - sbHorizontal.Min) * YStep;
  Y  := (sbVertical.Position   - sbVertical.Min)   * XStep;
  For I := 0 To Sounds.Items.Count - 1 Do
  Begin
    If (I = Sounds.SelectedIndex) And (pcGroundEditor.ActivePage = tbSounds) Then
    Begin
      Canvas.Pen.Color   := clRed;
      Canvas.Brush.Color := clRed;
    End
    Else
    Begin
      Canvas.Pen.Color   := clBlue;
      Canvas.Brush.Color := clBlue;
    End;
    DrawSound(I);
  End; // For I

  // Draw the selected zoneline

  If (Sounds.SelectedIndex >= 0) And (pcGroundEditor.ActivePage = tbSounds) Then
  Begin
    Canvas.Pen.Color   := clRed;
    Canvas.Brush.Color := clRed;
    DrawSound(Sounds.SelectedIndex);
  End;
End; // TfrmTexPicker.DrawSounds

procedure TfrmTexPicker.cbShowBoundsClick(Sender: TObject);
begin
  PaintGrid;
end;

Procedure TfrmTexPicker.SnapToGrid(Var X,Y: Single);
Begin
  X := Trunc(((X - MinX) + XStep / 4) / (XStep / 2)) * (XStep / 2) + MinX;
  Y := Trunc(((Y - MinY) + YStep / 4) / (YStep / 2)) * (YStep / 2) + MinY;
End; // TfrmTexPicker.SnapToGrid

Function TfrmTexPicker.ScreenXToZoneYCoord(X: Single): Single;
Begin
  X := X - (Splitter1.Left + Splitter1.Width);
  If X < 0 Then X := 0;
  Result := {MaxY}GMaxY + GXOfs * YStep - (X + (sbHorizontal.Position - sbHorizontal.Min) * (TexSize + 1)) * YStep / (TexSize + 1);
End; // TfrmTexPicker.ScreenXToZoneYCoord

Function TfrmTexPicker.ScreenYToZoneXCoord(Y: Single): Single;
Begin
  Result := {MaxX}GMaxX + GYOfs * XStep - (Y + (sbVertical.Position - sbVertical.Min) * (TexSize + 1)) * XStep / (TexSize + 1);
End; // TfrmTexPicker.ScreenYToZoneXCoord

Function TfrmTexPicker.ZoneXToScreenYCoord(X: Single): Single;
Var YOfs: Single;
Begin
  YOfs   := (sbVertical.Position - sbVertical.Min) * XStep;
  Result := (GMaxX - X - YOfs + GYOfs * GridSize) * ((TexSize + 1) / XStep);
End; // TfrmTexPicker.ZoneXToScreenYCoord

Function TfrmTexPicker.ZoneYToScreenXCoord(Y: Single): Single;
Var X0,XOfs: Single;
Begin
  X0     := Splitter1.Left + Splitter1.Width;
  XOfs   := (sbHorizontal.Position - sbHorizontal.Min) * YStep;
  Result := (GMaxY - Y - XOfs + GXOfs * GridSize) * ((TexSize + 1) / YStep) + X0;
End; // TfrmTexPicker.ZoneYToScreenXCoord

Function TfrmTexPicker.ZoneToScreenDeltaCoord(Delta: Single): Single;
Begin
  Result := Delta * ((TexSize + 1) / XStep);
End; // TfrmTexPicker.ZoneToScreenDeltaCoord

procedure TfrmTexPicker.pcGroundEditorChange(Sender: TObject);
begin
  If pcGroundEditor.ActivePage = tbSounds Then LoadSounds;
  grpCurrentTexture.Visible := ((pcGroundEditor.ActivePage = tbNames) Or
                                (pcGroundEditor.ActivePage = tbIcons));
  PaintGrid;
end;

procedure TfrmTexPicker.tbBoundsDeleteClick(Sender: TObject);
begin
  Bounds.DeleteSelected;
end;

procedure TfrmTexPicker.tbBoundsUndoClick(Sender: TObject);
begin
  Bounds.DoUndo;
end;

procedure TfrmTexPicker.tbBoundsRedoClick(Sender: TObject);
begin
  Bounds.DoRedo;
end;

procedure TfrmTexPicker.tbMeshDeleteClick(Sender: TObject);
begin
  Meshes.DeleteSelected;
end;

procedure TfrmTexPicker.tbMeshVarySizeClick(Sender: TObject);
Var
  I  : Integer;
  ML : TMeshLibraryObjectReference;

begin
  For I := Meshes.Items.Count - 1 DownTo 0 Do
  Begin
    If Meshes.Items.Strings[I] <> '' Then
    Begin
      ML := TMeshLibraryObjectReference(Meshes.Items.Objects[I]);
      ML.Size.Multiply(0.6 + Random * 0.5);
    End;
  End; // For I
end;

procedure TfrmTexPicker.tbMeshVaryAngleClick(Sender: TObject);
Var
  I  : Integer;
  ML : TMeshLibraryObjectReference;

begin
  For I := Meshes.Items.Count - 1 DownTo 0 Do
  Begin
    If Meshes.Items.Strings[I] <> '' Then
    Begin
      ML := TMeshLibraryObjectReference(Meshes.Items.Objects[I]);
      ML.Rotate.XAngle := ML.Rotate.XAngle + (Random * 6 - 3);
      ML.Rotate.YAngle := ML.Rotate.YAngle + (Random * 6 - 3);
      ML.Rotate.ZAngle := Random * 360;
    End;
  End; // For I
end;

procedure TfrmTexPicker.rbNoShadingClick(Sender: TObject);
begin
  frmStatus.Show;
  frmStatus.SetCaption('Pre-drawing textures');
  DrawDestBuffer;
  frmStatus.Visible := False;
  PaintGrid;
end;

procedure TfrmTexPicker.rbShadeBySlopeClick(Sender: TObject);
begin
  frmStatus.Show;
  frmStatus.SetCaption('Pre-drawing textures');
  DrawDestBuffer;
  frmStatus.Visible := False;
  PaintGrid;
end;

procedure TfrmTexPicker.rbShadeByElevationClick(Sender: TObject);
begin
  frmStatus.Show;
  frmStatus.SetCaption('Pre-drawing textures');
  DrawDestBuffer;
  frmStatus.Visible := False;
  PaintGrid;
end;

procedure TfrmTexPicker.tbZonePlanesDeleteClick(Sender: TObject);
begin
  ZonePlanes.DeleteSelected;
end;

procedure TfrmTexPicker.tbZonePlanesUndoClick(Sender: TObject);
begin
  ZonePlanes.DoUndo;
end;

procedure TfrmTexPicker.tbZonePlanesRedoClick(Sender: TObject);
begin
  ZonePlanes.DoRedo;
end;

procedure TfrmTexPicker.cbZonePlaneInfiniteZClick(Sender: TObject);
begin
  If ZonePlanes.SelectedIndex >= 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).InfiniteZ := cbZonePlaneInfiniteZ.Checked;
  edtZonePlaneMinZ.Enabled := Not cbZonePlaneInfiniteZ.Checked;
  edtZonePlaneMaxZ.Enabled := Not cbZonePlaneInfiniteZ.Checked;
end;

procedure TfrmTexPicker.edtZonePlaneMinZChange(Sender: TObject);
Var
  I : Single;
  J : Integer;

begin
  If ZonePlanes.SelectedIndex >= 0 Then
  Begin
    Val(Trim(edtZonePlaneMinZ.Text),I,J);
    If J = 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).Z1 := I;
  End;
end;

procedure TfrmTexPicker.edtZonePlaneMaxZChange(Sender: TObject);
Var
  I : Single;
  J : Integer;

begin
  If ZonePlanes.SelectedIndex >= 0 Then
  Begin
    Val(Trim(edtZonePlaneMaxZ.Text),I,J);
    If J = 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).Z2 := I;
  End;
end;

procedure TfrmTexPicker.edtZonePlaneZoneIDChange(Sender: TObject);
Var I,J: Integer;
begin
  If ZonePlanes.SelectedIndex >= 0 Then
  Begin
    Val(Trim(edtZonePlaneZoneID.Text),I,J);
    If J = 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).DestZoneID := I;
  End;
end;

procedure TfrmTexPicker.edtZonePlaneDestXChange(Sender: TObject);
Var
  I : Single;
  J : Integer;

begin
  If ZonePlanes.SelectedIndex >= 0 Then
  Begin
    Val(Trim(edtZonePlaneDestX.Text),I,J);
    If J = 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).DestX := I;
  End;
end;

procedure TfrmTexPicker.edtZonePlaneDestYChange(Sender: TObject);
Var
  I : Single;
  J : Integer;

begin
  If ZonePlanes.SelectedIndex >= 0 Then
  Begin
    Val(Trim(edtZonePlaneDestY.Text),I,J);
    If J = 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).DestY := I;
  End;
end;

procedure TfrmTexPicker.edtZonePlaneDestZChange(Sender: TObject);
Var
  I : Single;
  J : Integer;

begin
  If ZonePlanes.SelectedIndex >= 0 Then
  Begin
    Val(Trim(edtZonePlaneDestZ.Text),I,J);
    If J = 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).DestZ := I;
  End;
end;

procedure TfrmTexPicker.edtZonePlaneDestHeadingChange(Sender: TObject);
Var I,J: Integer;
begin
  If ZonePlanes.SelectedIndex >= 0 Then
  Begin
    Val(Trim(edtZonePlaneDestHeading.Text),I,J);
    If J = 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).DestAngle := I;
  End;
end;

procedure TfrmTexPicker.cbIgnoreZonePlaneDestXClick(Sender: TObject);
begin
  If ZonePlanes.SelectedIndex >= 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).HasDestX := Not cbIgnoreZonePlaneDestX.Checked;
end;

procedure TfrmTexPicker.cbIgnoreZonePlaneDestYClick(Sender: TObject);
begin
  If ZonePlanes.SelectedIndex >= 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).HasDestY := Not cbIgnoreZonePlaneDestY.Checked;
end;

procedure TfrmTexPicker.cbIgnoreZonePlaneDestZClick(Sender: TObject);
begin
  If ZonePlanes.SelectedIndex >= 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).HasDestZ := Not cbIgnoreZonePlaneDestZ.Checked;
end;

procedure TfrmTexPicker.cbIgnoreZonePlaneDestHeadingClick(Sender: TObject);
begin
  If ZonePlanes.SelectedIndex >= 0 Then TZonePlane(ZonePlanes.Items.Objects[ZonePlanes.SelectedIndex]).HasDestAngle := Not cbIgnoreZonePlaneDestHeading.Checked;
end;

// Callback function to be called during waveform-audio playback
// to process messages related to the progress of the playback.

procedure waveOutPrc(hwo: HWAVEOUT; uMsg: UINT; dwInstance,
  dwParam1, dwParam2: DWORD); stdcall;
begin
  TfrmTexPicker(dwInstance).WaveOutProc(hwo, uMsg, dwParam1, dwParam2);
end; // waveOutPrc

procedure TfrmTexPicker.WaveOutProc(hwo: HWAVEOUT; uMsg: UINT; dwParam1,
  dwParam2: DWORD);
begin
  case uMsg of
    WOM_OPEN:;
    WOM_CLOSE:
      fWaveOutHandle := 0;
    WOM_DONE:
      PostMessage(Handle, WM_FINISHED, 0, 0);
  end
end; // TfrmTexPicker.WaveOutProc

procedure TfrmTexPicker.PlayWAV(Stream: TMemoryStream);
var
  mmioHandle            : HMMIO;
  mmckInfoParent        : MMCKInfo;
  mmckInfoSubChunk      : MMCKInfo;
  dwFmtSize, dwDataSize : DWORD;
  pFormat               : PWAVEFORMATEX;
  wBlockSize            : word;
  mmioInfoRec           : TMMIOInfo;

begin
  // Set up the MMIOINFO structure to tell mmioOpen() that we're opening from a memory buffer

  FillChar(mmioInfoRec,SizeOf(mmioInfoRec),0);
  mmioInfoRec.pchBuffer := Stream.Memory;
  mmioInfoRec.fccIOProc := FOURCC_MEM;
  mmioInfoRec.cchBuffer := Stream.Size;

  // The mmioOpen function opens a file for unbuffered or buffered I/O

  mmioHandle := mmioOpen(Nil, @mmioInfoRec, MMIO_READ);

//  mmioHandle := mmioOpen(PChar(szFileName), nil, MMIO_READ or MMIO_ALLOCBUF);
  if mmioHandle = 0 then raise Exception.Create('Unable to open stream');

  try
    // mmioStringToFOURCC converts a null-terminated string to a four-character code

    mmckInfoParent.fccType := mmioStringToFOURCC('WAVE', 0);

    // The mmioDescend function descends into a chunk of a RIFF file

    if mmioDescend(mmioHandle, @mmckinfoParent, nil, MMIO_FINDRIFF) <>
      MMSYSERR_NOERROR then raise Exception.Create('Stream is not a valid wave file');

    mmckinfoSubchunk.ckid := mmioStringToFourCC('fmt ', 0);
    if mmioDescend(mmioHandle, @mmckinfoSubchunk, @mmckinfoParent,
      MMIO_FINDCHUNK) <> MMSYSERR_NOERROR then
      raise Exception.Create('Stream is not a valid wave file');

    dwFmtSize := mmckinfoSubchunk.cksize;
    GetMem(pFormat, dwFmtSize);

    try
      // The mmioRead function reads a specified number of bytes from a file

      if DWORD(mmioRead(mmioHandle, PChar(pFormat), dwFmtSize)) <>
        dwFmtSize then
        raise Exception.Create('Error reading wave data');

      if pFormat^.wFormatTag <> WAVE_FORMAT_PCM then
        raise Exception.Create('Invalid wave file format');

      // The waveOutOpen function opens the given waveform-audio output device for playback

      if waveOutOpen(@fWaveOutHandle, WAVE_MAPPER, pFormat, 0, 0,
        WAVE_FORMAT_QUERY) <> MMSYSERR_NOERROR then
        raise Exception.Create('Can''t play format');

      mmioAscend(mmioHandle, @mmckinfoSubchunk, 0);
      mmckinfoSubchunk.ckid := mmioStringToFourCC('data', 0);
      if mmioDescend(mmioHandle, @mmckinfoSubchunk, @mmckinfoParent,
        MMIO_FINDCHUNK) <> MMSYSERR_NOERROR then
        raise Exception.Create('No data chunk');

      dwDataSize := mmckinfoSubchunk.cksize;
      if dwDataSize = 0 then
        raise Exception.Create('Chunk has no data');

      if waveOutOpen(@fWaveOutHandle, WAVE_MAPPER, pFormat,
        DWORD(@WaveOutPrc), Integer(Self), CALLBACK_FUNCTION) <> MMSYSERR_NOERROR then
      begin
        fWaveOutHandle := 0;
        raise Exception.Create('Failed to open output device');
      end;

      wBlockSize := pFormat^.nBlockAlign;

      ReallocMem(pFormat, 0);
      ReallocMem(fData, dwDataSize);

      if DWORD(mmioRead(mmioHandle, fData, dwDataSize)) <> dwDataSize then
        raise Exception.Create('Unable to read data chunk');

      GetMem(fWaveHdr, SizeOf(WAVEHDR));
      fWaveHdr^.lpData  := fData;
      fWaveHdr^.dwBufferLength := dwDataSize;
      fWaveHdr^.dwFlags := 0;
      fWaveHdr^.dwLoops := 0;
      fWaveHdr^.dwUser := 0;

      // The waveOutPrepareHeader function prepares a waveform-audio data block for playback.
      if waveOutPrepareHeader(fWaveOutHandle, fWaveHdr,
        SizeOf(WAVEHDR)) <> MMSYSERR_NOERROR then
        raise Exception.Create('Unable to prepare header');

      // The waveOutWrite function sends a data block to the given waveform-audio output device.
      if waveOutWrite(fWaveOutHandle, fWaveHdr, SizeOf(WAVEHDR)) <>
        MMSYSERR_NOERROR then
        raise Exception.Create('Failed to write to device');

    finally
      ReallocMem(pFormat, 0)
    end
  finally
    mmioClose(mmioHandle, 0)
  end
end; // TfrmTexPicker.PlayWAV

procedure TfrmTexPicker.StopWAV;
begin
  // The waveOutReset function stops playback on the given waveform-audio output device

  WaveOutReset(fWaveOutHandle);
end; // TfrmTexPicker.StopWAV

procedure TfrmTexPicker.WMFinished(var Msg: TMessage);
begin
  WaveOutUnprepareHeader(fWaveOutHandle, fWaveHdr, SizeOf(WAVEHDR));
  WaveOutClose(fWaveOutHandle);
  ReallocMem(fData, 0);
  ReallocMem(fWaveHdr, 0);
  tbSoundsPlay.Enabled := True;
  tbPlaySound.Enabled  := True;
end; // TfrmTexPicker.WMFinished

procedure TfrmTexPicker.tbSoundsStopClick(Sender: TObject);
begin
  StopWAV;
end;

procedure TfrmTexPicker.tbSoundsPlayClick(Sender: TObject);
Var MS: TMemoryStream;
begin
  If (lbSounds.ItemIndex > 0) And (lbSounds.ItemIndex <= High(SoundFiles)) Then
  Begin
    tbSoundsPlay.Enabled := False;
    tbPlaySound.Enabled  := False;
    try
      MS := TMemoryStream.Create;
      MS.WriteBuffer(SoundFiles[SoundIndexes[lbSounds.ItemIndex - 1]]^,SoundSizes[SoundIndexes[lbSounds.ItemIndex - 1]]);
      PlayWAV(MS);
      MS.Free;
    except
      tbSoundsPlay.Enabled := True;
      tbPlaySound.Enabled  := True;
      raise;
    end
  End;
end;

procedure TfrmTexPicker.tbSoundsUndoClick(Sender: TObject);
begin
  Sounds.DoUndo;
end;

procedure TfrmTexPicker.tbSoundsRedoClick(Sender: TObject);
begin
  Sounds.DoRedo;
end;

procedure TfrmTexPicker.tbSoundsDeleteClick(Sender: TObject);
begin
  Sounds.DeleteSelected;
end;

procedure TfrmTexPicker.cbAreaSoundClick(Sender: TObject);
begin
  If Sounds.SelectedIndex >= 0 Then TSound(Sounds.Items.Objects[Sounds.SelectedIndex]).Area := cbAreaSound.Checked;
end;

procedure TfrmTexPicker.tbPlaySoundClick(Sender: TObject);
Var
  MS : TMemoryStream;
  I  : Integer;
  S  : TSound;

begin
  If Sounds.SelectedIndex >= 0 Then
  Begin
    S := TSound(Sounds.Items.Objects[Sounds.SelectedIndex]);
    If S.DayName <> '' Then
    Begin
      I := lbSounds.Items.IndexOf(S.DayName) - 1;
      If (I >= 0) And (I <= High(SoundFiles)) Then
      Begin
        tbSoundsPlay.Enabled := False;
        tbPlaySound.Enabled  := False;
        try
          MS := TMemoryStream.Create;
          MS.WriteBuffer(SoundFiles[SoundIndexes[I]]^,SoundSizes[SoundIndexes[I]]);
          PlayWAV(MS);
          MS.Free;
        except
          tbSoundsPlay.Enabled := True;
          tbPlaySound.Enabled  := True;
          raise;
        end
      End;
    End;
  End;
end;

procedure TfrmTexPicker.lbSoundsClick(Sender: TObject);
begin
  If (Sounds.SelectedIndex >= 0) And
     (lbSounds.ItemIndex >= 0)   Then
  Begin
    If lbSounds.ItemIndex > 0 Then
    Begin
      If rbDaySound.Checked
       Then TSound(Sounds.Items.Objects[Sounds.SelectedIndex]).DayName   := lbSounds.Items.Strings[lbSounds.ItemIndex]
       Else TSound(Sounds.Items.Objects[Sounds.SelectedIndex]).NightName := lbSounds.Items.Strings[lbSounds.ItemIndex];
    End
    Else
    Begin
      If rbDaySound.Checked
       Then TSound(Sounds.Items.Objects[Sounds.SelectedIndex]).DayName   := ''
       Else TSound(Sounds.Items.Objects[Sounds.SelectedIndex]).NightName := '';
    End;
    Sounds.ShowSelectedParameters;
  End;
end;

procedure TfrmTexPicker.pnlSoundInfoResize(Sender: TObject);
begin
  rbDaySound.Width   := Width - rbDaySound.Left;
  rbNightSound.Width := Width - rbNightSound.Left;
end;

Procedure TfrmTexPicker.PaintTopographic(ClipX1,ClipY1,ClipX2,ClipY2: Integer; LineColor: TColor; MinZ,MaxZ: Single);
Const ZInc = 50;
Var
  Z      : Single;
  Region : THandle;

  Procedure DrawTopographicLines(Z: Single; MO: TMeshObject);
  Var
    I,J,K,L   : Integer;
    P         : TPolygon;
    NumPoints : Integer;
    V         : Array[0..1] Of T3DPoint;
    V1        : T3DPoint;
    Index0    : Integer;
    Index1    : Integer;
    X,Y       : Array[0..1] Of Single;
    DeltaZ0   : Single;
    DeltaZ1   : Single;
    DeltaZ    : Single;
    Ratio     : Single;
    H         : Integer;

  Begin
    If MO <> Nil Then
    Begin
      I := MO.Polygons.Count;
      For K := 0 To I - 1 Do
      Begin
        If (PolyMinZ[K] <= Z) And (PolyMaxZ[K] >= Z) Then
        Begin
          P := TPolygon(MO.Polygons.Objects[K]);
          If High(P.Vertices) >= 2 Then
          Begin
            Index0    := 0;
            Index1    := 1;
            H         := High(P.Vertices);
            NumPoints := 0;
            L         := 0;
            While (L <= H) And (NumPoints < 2) Do
            Begin
              V[NumPoints] := T3DPoint(MO.Vertices.Objects[P.Vertices[Index0]]);
              V1           := T3DPoint(MO.Vertices.Objects[P.Vertices[Index1]]);
              DeltaZ1      := V1.Z - Z;
              DeltaZ0      := V[NumPoints].Z - Z;

              // Find out if this segment crosses the topographic elevation

              If DeltaZ1 * DeltaZ0 <= 0 Then
              Begin
                DeltaZ := V1.Z - V[NumPoints].Z;
                If DeltaZ <> 0 Then
                Begin
                  Ratio        := Abs(DeltaZ0) / Abs(DeltaZ);
                  X[NumPoints] :=  V[NumPoints].X + (V1.X - V[NumPoints].X) * Ratio + MO.Loc.X;
                  Y[NumPoints] :=  V[NumPoints].Y + (V1.Y - V[NumPoints].Y) * Ratio + MO.Loc.Y;
                End
                Else
                Begin
                  X[NumPoints] := V1.X + MO.Loc.X;
                  Y[NumPoints] := V1.Y + MO.Loc.Y;
                End;
                Inc(NumPoints);
              End;
              Index0 := Index1;
              Inc(Index1);
              If Index1 > H Then Index1 := 0;
              Inc(L);
            End; // While
            If NumPoints = 2 Then
            Begin
              Canvas.MoveTo(Round(ZoneYToScreenXCoord(Y[0])),Round(ZoneXToScreenYCoord(X[0])));
              Canvas.LineTo(Round(ZoneYToScreenXCoord(Y[1])),Round(ZoneXToScreenYCoord(X[1])));
            End;
          End;
        End;
      End; // For K
    End;
  End; // DrawTopographicLines

Begin
  // Clip to the drawing area

  Region := CreateRectRgn(0,0,0,0);
  SetRectRgn(Region,frmTexPicker.Splitter1.Left + frmTexPicker.Splitter1.Width + ClipX1,ClipY1,
                    frmTexPicker.Splitter1.Left + frmTexPicker.Splitter1.Width + ClipX2,ClipY2);
  SelectClipRgn(frmTexPicker.Canvas.Handle,Region);

  Z := MinZ;
  Z := Trunc(Z / ZInc) * ZInc;
  If Z > MinZ Then Z := Z - ZInc;
  Canvas.Pen.Color := LineColor;
  Canvas.Pen.Style := psSolid;
  While Z < MaxZ Do
  Begin
    DrawTopographicLines(Z,MO1);
    DrawTopographicLines(Z,MO2);
    Z := Z + ZInc;
  End; // While

  // Go back to no clipping

  SelectClipRgn(frmTexPicker.Canvas.Handle,0);
  DeleteObject(Region);
End; // TfrmTexPicker.PaintTopographic

procedure TfrmTexPicker.cbTopographicClick(Sender: TObject);
begin
  PaintGrid;
end;

procedure TfrmTexPicker.cbShowOverlayClick(Sender: TObject);
begin
  frmStatus.Show;
  frmStatus.SetCaption('Pre-drawing textures');
  DrawDestBuffer;
  frmStatus.Visible := False;
  PaintGrid;
end;

procedure TfrmTexPicker.LoadModelFromMesh;
Var
  I,J    : Integer;
  GO     : TGroupObject;
  Mesh   : TMeshObject;
  List   : TStringList;
  Light  : TLight;
  Entity : TEntity;
  Tree   : TTree;

begin
  If (tvMeshes.SelectionCount = 1) And
     (tvMeshes.Selections[0] <> Nil) And
     (tvMeshes.Selections[0].Data <> Nil) And
     Not frmMain.LoadingMeshLibraryList Then
  Begin
    glvMesh.Scene3D.Scene.LockBSPTree('TfrmTexPicker.LoadModelFromMesh');

    GO := TGroupObject(tvMeshes.Selections[0].Data);
    SelectedMeshObject := GO.GetName;
    List := TStringList.Create;
    GO.AddToPolygonList(List,False,True,True,True);

    glvMesh.scene3D.scene.bTranslucentMode := True;

    glvMesh.Scene3D.Scene.ClearScene(True,True);
    glvMesh.Clear;

    For J := 0 To List.Count - 1 Do
    Begin
      Mesh := TMeshObject(List.Objects[J]);
      Tree := Nil;
      frmMain.ReloadMesh(List,0,J,False,False,False,glvMesh,Tree,False,ExtractFilePath(Application.ExeName) + 'library\textures\');
      Tree.Free;
      Mesh.Free;
    End; // For J
    List.Free;

    glvMesh.scene3D.scene.DefaultCamera.LookAt(0,0,-100);
    glvMesh.scene3D.scene.DefaultCamera.SetVectorUp(1,0,0);
    glvMesh.scene3D.scene.DefaultCamera.SetPosition(0,0,8);

//    Mesh := List.Objects[0] As TMeshObject;



    // Get rid of all entities
{
    For I := glvMesh.Scene3D.Scene.Entities.Count - 1 DownTo 0 Do
    Begin
      TEntity(glvMesh.Scene3D.Scene.Entities.Items[I]).Free;
      glvMesh.Scene3D.Scene.Entities.Delete(I);
    End; // For I
}

{
    // Add a dummy entity since ReloadMesh expects it

    Entity := TEntity.Create(glvMesh.Scene3D.Scene);
    glvMesh.Scene3D.Scene.Entities.Add(Entity);
}
{
    Tree := Nil;
    frmMain.ReloadMesh(List,0,False,False,False,glvMesh,Tree,True);
    List.Free;
    Tree.Free;
    Mesh.Free;
}


    Light := TLight(glvMesh.scene3D.scene.Lights.Items[0]);
    Light.Position.Copy(0,0,100000);
    Light.SetOrientation(1,1,0);
    Light.Attenuation := 0;//.001;

    // The frustum culling and occlusion culling code don't play well with LocalScale

    glvMesh.Scene3D.Scene.FrustumCulling := False;
    glvMesh.Scene3D.Scene.OcclusionManager.Enabled := False;
    glvMesh.Fit(True,True,True);

    For J := 0 To glvMesh.Scene3D.Scene.Entities.Count - 1 Do
    Begin
      Entity := TEntity(glvMesh.Scene3D.Scene.Entities.Items[J]);
      Entity.BSPTreeNode.CheckEntity(Entity);
    End; // For J

    glvMesh.Scene3D.Scene.DefaultCamera.Position.Dirty := True;


    glvMesh.Scene3D.Scene.UnlockBSPTree;    

//    glvMesh.Fit(False,True,True);
    glvMesh.Scene3D.Redraw;
  End
  Else SelectedMeshObject := '';
End; // TfrmTexPicker.LoadModelFromMesh

procedure TfrmTexPicker.btnHelpClick(Sender: TObject);
begin
  Application.HelpJump('The_Ground_Editor');
end;

procedure TfrmTexPicker.tbElevationChange(Sender: TObject);
begin
  If Not boolChangingCEEdit Then
  Begin
    boolChangingCETB  := True;
    ClipElevation.Z   := ZoneMinZ + (tbElevation.Position - tbElevation.Min) * ((ZoneMaxZ - ZoneMinZ) / (tbElevation.Max - tbElevation.Min));
    edtElevation.Text := Trim(Format('%8.3f',[ClipElevation.Z]));
    PaintGrid;
    ClipElevation.Draw;
    boolChangingCETB  := False;
  End;
end;

procedure TfrmTexPicker.edtElevationChange(Sender: TObject);
Var
  I : Integer;
  S : Single;

begin
  If Not boolChangingCETB Then
  Begin
    boolChangingCEEdit := True;
    Val(Trim(edtElevation.Text),S,I);
    If I = 0 Then
    Begin
      ClipElevation.Z := S;
      If ZoneMaxZ > ZoneMinZ
       Then tbElevation.Position := tbElevation.Min + Min(tbElevation.Max - tbElevation.Min,Max(0,Round((S - ZoneMinZ) * ((tbElevation.Max - tbElevation.Min) / (ZoneMaxZ - ZoneMinZ)))))
       Else tbElevation.Position := tbElevation.Min;
      PaintGrid;
      ClipElevation.Draw;
    End;
    boolChangingCEEdit := False;
  End;
end;

procedure TfrmTexPicker.btnSplitPolysClick(Sender: TObject);
Const SplitName = 'GroundSplit';
Var
  I         : Integer;
  ZO        : TZoneObject;
  NewMeshes : TStringList;

  Procedure SplitPolygons(X1,Y1,X2,Y2,Z: Single; MO: TMeshObject; GroundMesh: Boolean);
  Var
    I,J,K,L    : Integer;
    P          : TPolygon;
    GX,GY      : Integer;
    Index0     : Integer;
    Index1     : Integer;
    H          : Integer;
    NumPoints  : Integer;
    V          : Array[0..1] Of T3DPoint;
    DeltaZ0    : Single;
    DeltaZ1    : Single;
    DeltaZ     : Single;
    Ratio      : Single;
    X,Y        : Array[0..1] Of Single;
    M          : TMeshObject;
    P1         : TPolygon;
    V1         : T3DPoint;
    HiddenTags : TStringList;
    Region     : TRegion;
    PMinX      : Single;
    PMinY      : Single;
    PMinZ      : Single;
    PMaxX      : Single;
    PMaxY      : Single;
    PMaxZ      : Single;

  Begin
    M := Nil;
    HiddenTags := TStringList.Create;
    HiddenTags.Sorted := True;

    // Pass 1 -- Hide any grid elements where we will be splitting the polygons, and
    // determine how many polygons will be split for all meshes

    I := MO.Polygons.Count;
    J := 0;
    For K := 0 To I - 1 Do
    Begin
      // Make sure the polygon is at least a triangle

      P := TPolygon(MO.Polygons.Objects[K]);
      If High(P.Vertices) >= 2 Then
      Begin
        // Make sure the polygon crosses the elevation.  There are two different tests depending on
        // whether the polygon came from a ground mesh or not.

        If (Not GroundMesh) Or ((PolyMinZ[K] <= Z) And (PolyMaxZ[K] >= Z)) Then
        Begin
          P.GetBounds(MO,PMinX,PMinY,PMinZ,PMaxX,PMaxY,PMaxZ);
          If GroundMesh Or ((PMinZ <= Z) And (PMaxZ >= Z)) Then
          Begin
            // Get the polygon's bounding box and make sure at least part of it is within the clipping rectangle

            If (PMaxY > Min(Y1,Y2)) And
               (PMinY < Max(Y1,Y2)) And
               (PMaxX > Min(X1,X2)) And
               (PMinX < Max(X1,X2)) Then
            Begin
              // Make sure the polygon is visible

              If GroundMesh Then
              Begin
                GX := P.Tag Mod GridWidth;
                GY := P.Tag Div GridWidth;
              End;
              If (Not GroundMesh) Or TexVisible[GY * GridWidth + GX] Then
              Begin
                // Find where the polygon crosses the elevation

                Index0    := 0;
                Index1    := 1;
                H         := High(P.Vertices);
                NumPoints := 0;
                L         := 0;
                While (L <= H) And (NumPoints < 2) Do
                Begin
                  V[NumPoints] := T3DPoint(MO.Vertices.Objects[P.Vertices[Index0]]);
                  V1           := T3DPoint(MO.Vertices.Objects[P.Vertices[Index1]]);
                  DeltaZ1      := V1.Z - Z;
                  DeltaZ0      := V[NumPoints].Z - Z;

                  // Find out if this segment crosses the topographic elevation

                  If DeltaZ1 * DeltaZ0 <= 0 Then
                  Begin
                    DeltaZ := V1.Z - V[NumPoints].Z;
                    If DeltaZ <> 0 Then
                    Begin
                      Ratio        := Abs(DeltaZ0) / Abs(DeltaZ);
                      X[NumPoints] :=  V[NumPoints].X + (V1.X - V[NumPoints].X) * Ratio + MO.Loc.X;
                      Y[NumPoints] :=  V[NumPoints].Y + (V1.Y - V[NumPoints].Y) * Ratio + MO.Loc.Y;
                    End
                    Else
                    Begin
                      X[NumPoints] := V1.X + MO.Loc.X;
                      Y[NumPoints] := V1.Y + MO.Loc.Y;
                    End;
                    Inc(NumPoints);
                  End;
                  Index0 := Index1;
                  Inc(Index1);
                  If Index1 > H Then Index1 := 0;
                  Inc(L);
                End; // While
                If NumPoints = 2 Then
                Begin
                  // Hide the grid element

                  If GroundMesh Then
                  Begin
                    TexVisible[GY * GridWidth + GX] := False;
                    HiddenTags.Add(IntToStr(GY * GridWidth + GX));
                  End;
                  Inc(J);
                End;
              End;
            End;
          End;
        End;
      End;
    End; // For K

    // Pass 2 -- Copy polygons for all newly hidden tags

    If J > 0 Then
    Begin
      If GroundMesh Then
      Begin
        For K := 0 To I - 1 Do
        Begin
          P := TPolygon(MO.Polygons.Objects[K]);
          If High(P.Vertices) >= 2 Then
          Begin
            // Determine which grid this polygon is in

            GX := P.Tag Mod GridWidth;
            GY := P.Tag Div GridWidth;

            J := HiddenTags.IndexOf(IntToStr(GY * GridWidth + GX));

            If J >= 0 Then
            Begin
              // Add a copy of the polygon to the new mesh

              If M = Nil Then
              Begin
                M := TMeshObject.Create;
                J := 0;
                While frmMain.Zone.NameExists(SplitName + IntToStr(J)) Do Inc(J);
                M.SetName(SplitName + IntToStr(J));
              End;
              P1 := TPolygon.CreateBasic(P);
              SetLength(P1.TX,High(P.TX) + 1);
              SetLength(P1.TZ,High(P.TZ) + 1);
              SetLength(P1.Colors,High(P.Colors) + 1);
              SetLength(P1.Vertices,High(P.Vertices) + 1);
              For J := 0 To High(P1.TX) Do P1.TX[J] := P.TX[J];
              For J := 0 To High(P1.TZ) Do P1.TZ[J] := P.TZ[J];
              For J := 0 To High(P1.Colors) Do P1.Colors[J] := P.Colors[J];
              For J := 0 To High(P1.Vertices) Do
              Begin
                V1 := T3DPoint.Create(T3DPoint(MO.Vertices.Objects[P.Vertices[J]]));
                M.Vertices.AddObject('',V1);
                P1.Vertices[J] := M.Vertices.Count - 1;
                If P.Vertices[J] < MO.Normals.Count Then
                Begin
                  V1 := T3DPoint.Create(T3DPoint(MO.Normals.Objects[P.Vertices[J]]));
                  M.Normals.AddObject('',V1);
                End;
              End; // For J
              M.Polygons.AddObject('',P1);
            End;
          End;
        End; // For K
      End
      Else M := MO;

      // Pass 3 -- Split the polygons

      If M <> Nil Then
      Begin
        M.ConvertToTriangles;
        Region := TRegion.Create(M,True);
        V1 := T3DPoint.Create(0,0,-1);
        Region.SplitAlongPlane(V1,Z,False,False);
        M.ConvertToTriangles;
        Region.Free;
      End;

      // Pass 4 -- Assign textures

      If M <> Nil Then
      Begin
        For K := 0 To M.Polygons.Count - 1 Do
        Begin
          P := TPolygon(M.Polygons.Objects[K]);
          P.GetBounds(M,PMinX,PMinY,PMinZ,PMaxX,PMaxY,PMaxZ);
               If ((Abs(PMinZ - Z) < 0.01) And (PMaxZ > Z)) Or
                  ((PMinZ > Z) And GroundMesh) Then P.Texture := cbUpperTexture.Text + '+0'
          Else If ((Abs(PMaxZ - Z) < 0.01) And (PMinZ < Z)) Or
                  ((PMaxZ < Z) And GroundMesh) Then P.Texture := cbLowerTexture.Text + '+0';
        End; // For K
      End;

      // Add the new mesh to the zone

      If (M <> Nil) And GroundMesh Then
      Begin
        frmMain.Zone.AddObject(M.GetName,M);
        NewMeshes.AddObject(M.GetName,M);
      End;
    End;
    HiddenTags.Free;
  End; // SplitPolygons

begin
  NewMeshes := TStringList.Create;
  NewMeshes.Sorted := True;
  If MO1 <> Nil Then SplitPolygons(ClipElevation.X1,ClipElevation.Y1,ClipElevation.X2,ClipElevation.Y2,ClipElevation.Z,MO1,True);
  If MO2 <> Nil Then SplitPolygons(ClipElevation.X1,ClipElevation.Y1,ClipElevation.X2,ClipElevation.Y2,ClipElevation.Z,MO2,True);
  frmStatus.Show;
  frmStatus.SetCaption('Loading sounds');
  For I := 0 To frmMain.Zone.Count - 1 Do
  Begin
    frmStatus.SetPosition(I / frmMain.Zone.Count);
    ZO := TZoneObject(frmMain.Zone.Objects[I]);
    If (ZO <> MO1) And
       (ZO <> MO2) And
       (ZO Is TMeshObject) And
       (NewMeshes.IndexOf(ZO.GetName) < 0) And
       (Copy(ZO.GetName,1,Length(SplitName)) = SplitName) Then SplitPolygons(ClipElevation.X1,ClipElevation.Y1,ClipElevation.X2,ClipElevation.Y2,ClipElevation.Z,TMeshObject(ZO),False);
  End; // For I
  NewMeshes.Free;
  For I := 0 To High(TexVisible) Do frmMain.Zone.ElevationGrid.Visible[I] := TexVisible[I];
  FillChar(OverlayMaskBuffer^,OverlayMaskBufSize,255); // Important!
  FillChar(OverlayBuffer^,OverlayBufSize,0);           // Important!
  CalculateOverlay;
  frmStatus.SetCaption('Pre-drawing textures');
  DrawDestBuffer;
  frmStatus.Hide;
  PaintGrid;
end;

procedure TfrmTexPicker.btnBrowseUpperClick(Sender: TObject);
Var I: Integer;
begin
  If frmTextureChooser.ShowModal = mrOk Then
  Begin
    If (frmTextureChooser.dgTextures.Row >= 0) And
       (frmTextureChooser.dgTextures.Col >= 0) Then
    Begin
      I := frmTextureChooser.dgTextures.Row * frmTextureChooser.dgTextures.ColCount + frmTextureChooser.dgTextures.Col;
      If I < TextureLibrary.Count Then cbUpperTexture.ItemIndex := I;
    End;
  End;
end;

procedure TfrmTexPicker.btnBrowseLowerClick(Sender: TObject);
Var I: Integer;
begin
  If frmTextureChooser.ShowModal = mrOk Then
  Begin
    If (frmTextureChooser.dgTextures.Row >= 0) And
       (frmTextureChooser.dgTextures.Col >= 0) Then
    Begin
      I := frmTextureChooser.dgTextures.Row * frmTextureChooser.dgTextures.ColCount + frmTextureChooser.dgTextures.Col;
      If I < TextureLibrary.Count Then cbLowerTexture.ItemIndex := I;
    End;
  End;
end;

procedure TfrmTexPicker.cbShowHiddenGridElementsClick(Sender: TObject);
begin
  DrawDestBuffer;
  PaintGrid;
end;

procedure TfrmTexPicker.tvMeshesChange(Sender: TObject; Node: TTreeNode);
begin
  LoadModelFromMesh;
  Meshes.Draw;
end;

procedure TfrmTexPicker.tvMeshesCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; var DefaultDraw: Boolean);
Var ImageRect1, NewRect, ImageRect2, ResRect: TRect;
begin
  StretchBlt(tvMeshes.Canvas.Handle,ARect.Left,ARect.Top,ARect.Right - ARect.Left,ARect.Bottom - ARect.Top,
             frmMain.Image4.Canvas.Handle,0,Round(ARect.Top * frmMain.Image4.Height / tvMeshes.Height),
             frmMain.Image4.Width,
             Round((ARect.Bottom - ARect.Top) * frmMain.Image4.Height / tvMeshes.Height),SRCCOPY);
end;

procedure TfrmTexPicker.tvMeshesCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
Var
  Rect : TRect;
  iSbh : integer;

begin
  If cdsSelected In State Then
  Begin
    tvMeshes.Canvas.Brush.Style := bsSolid;
    tvMeshes.Canvas.Font.Color  := clWhite;
    tvMeshes.Canvas.Brush.Color := clNavy;
  End
  Else
  Begin
    tvMeshes.Canvas.Brush.Style := bsClear;
    tvMeshes.Canvas.Font.Color  := clBlack;
  End;
  Rect := Node.DisplayRect(True);

  tvMeshes.Canvas.TextOut(Rect.Left,Rect.Top,Node.Text);
  DefaultDraw := False;

  iSbh := GetScrollPos(Sender.Handle,SB_HORZ);
  Rect := Node.DisplayRect(False);
  Rect.Left := Rect.Left - iSbh;
  Rect.Left := Rect.Left + ((Node.Level+1) * TreeView_GetIndent(Sender.Handle));

  If Node.HasChildren Then
  Begin
    If Node.Expanded
     Then frmMain.ilTreeView.Draw(Sender.Canvas,Rect.Left - 16,Rect.Top,1)
     Else frmMain.ilTreeView.Draw(Sender.Canvas,Rect.Left - 16,Rect.Top,0);
  End;
end;

procedure TfrmTexPicker.Splitter2Moved(Sender: TObject);
begin
  tvMeshes.Invalidate;
end;

end.
