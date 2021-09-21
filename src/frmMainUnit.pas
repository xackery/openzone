Unit frmMainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLVisir, ExtCtrls, Points3D, ZoneClasses, StdCtrls, Grids, U3DPolys, Menus,
  ImgList, ComCtrls, ToolWin, SharedComboBox, ActnMan, ActnCtrls, ActnList,
  StdActns, BandActn, ActnMenus, Buttons, ZPropLst, AppEvnts, URickGL,
  TB2Dock, TB2Toolbar, TB2Item, TB2Common, GLPanel, DesignIntf, TrCtrls,
  AboutColorButton, ClearTypeText, OZDMUnit, Xdom_3_2, Q3Types, Anim8or;

Const
  Version     = '8.6';
  INIFileName = 'OpenZone.ini';
  FirstEntity = 7;

type
  PLongWord = ^LongWord;
  PBGRA = ^TBGRA;
  TBGRA = Packed Record
    B,G,R,A: Byte;
  End;
  TTimeOfDay = Record
    Hour   : Integer;
    Minute : Single;
    Day    : Integer;
    Month  : Integer;
    Year   : Integer;
  End;
  TSplitPair = Class
    I1,I2 : Integer;
    P1,P2 : TPolygon;     // P1 is the ground polygon
    M1,M2 : TMeshObject;  // M1 is the ground mesh
    B1,B2 : Boolean;
    R1,R2 : TRegion;
    N1,N2 : T3DPoint;
    D1,D2 : Single;
    Q1,Q2 : TMeshObject;
  End;
  TProgramSettings = Record
    EQDir                 : String;
    MenuIconSize          : Integer;
    ToolbarIconSize       : Integer;
    UseNumericKeypad      : Boolean;
    AnimateTextures       : Boolean;
    ShowZoneDimensions    : Boolean;
    ShowLightSources      : Integer;
    ShowModelOrigins      : Integer;
    ShowHotSpots          : Integer;
    ClearType             : Boolean;
    ClearTypeRGB          : Boolean; // True = RGB, False = BGR
    ClearTypeActive       : String;
    ClearTypeInactive     : String;
    ClearTypeActiveSize   : Integer;
    ClearTypeInactiveSize : Integer;
    WLDCreatureSize       : Single;
    XWFCreatureSize       : Single;
    SmallMoveAmount       : Single;
    SmallRotateAmount     : Single;      
  End;
  TCrosshairType = (crNone,crCreateObject,crGetSetGroundHeight);

  TMyTreeView = Class(TTreeView)
  Published
    Property Images;
  End;

  TfrmMain = class(TGLForm)
    glView: TGLVisir;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    CoolBar1: TCoolBar;
    ilMain16: TImageList;
    ToolBar2: TToolBar;
    tbPalette: TTabControl;
    ilPalette: TImageList;
    tvMain: TTreeView;
    Splitter2: TSplitter;
    Label1: TLabel;
    cbTextureSet: TComboBox;
    ActionManager1: TActionManager;
    ActionList1: TActionList;
    ActionToolBar1: TActionToolBar;
    DomImplementation1: TDomImplementation;
    XmlToDomParser1: TXmlToDomParser;
    ZPropList1: TZPropList;
    Splitter3: TSplitter;
    alGUI: TActionList;
    acNewScene: TAction;
    acOpenScene: TAction;
    acSaveScene: TAction;
    acSaveAs: TAction;
    acImport3DS: TAction;
    acImportOGRE: TAction;
    acExportToWLD: TAction;
    acExportToS3D: TAction;
    acViewScriptLog: TAction;
    acCreateLightsWLD: TAction;
    acCreateObjectsWLD: TAction;
    acExit: TAction;
    acEditCut: TAction;
    acEditCopy: TAction;
    acEditPaste: TAction;
    acChangeWater: TAction;
    acEditGround: TAction;
    acInsertScene: TAction;
    acTranslate: TAction;
    acChangeView: TAction;
    acEditDelete: TAction;
    acRename: TAction;
    acGroup: TAction;
    acUngroup: TAction;
    acHelpMovementKeys: TAction;
    acGetGroundHeight: TAction;
    acSetGroundHeight: TAction;
    cbCrosshair: TComboBox;
    edtRaiseLowerRadius: TEdit;
    pnlGroundEdit: TPanel;
    edtRaiseLowerAmount: TEdit;
    acRaiseLand: TAction;
    udRaiseLowerRadius: TUpDown;
    udRaiseLowerAmount: TUpDown;
    acFlattenAtPeak: TAction;
    acFlattenAtAverage: TAction;
    acFlattenAtLowest: TAction;
    acRumpleGround: TAction;
    acMountainize: TAction;
    imgCompass: TImage;
    acAddZoneExit: TAction;
    sbMain: TStatusBar;
    acExportAsMesh: TAction;
    acInsertMeshLibraryObject: TAction;
    acInsertLightSource: TAction;
    acConvertToTransparent: TAction;
    acExtendEdges: TAction;
    acRumpleGroundWithinRadius: TAction;
    ApplicationEvents1: TApplicationEvents;
    acImportGround3DS: TAction;
    acExportZoneLineInfo: TAction;
    tbTimeOfDay: TTrackBar;
    Label10: TLabel;
    acGenerateMap: TAction;
    acZoneProperties: TAction;
    acPreferences: TAction;
    acShiftZone: TAction;
    acSetObjectPosToCurrent: TAction;
    acHelpAbout: TAction;
    acImportQuake3Map: TAction;
    Panel1: TPanel;
    btnMoveObjectNorth: TSpeedButton;
    btnMoveObjectSouth: TSpeedButton;
    btnMoveObjectEast: TSpeedButton;
    btnMoveObjectWest: TSpeedButton;
    btnMoveObjectUp: TSpeedButton;
    btnMoveObjectDown: TSpeedButton;
    acMoveObjectNorth: TAction;
    acMoveObjectSouth: TAction;
    acMoveObjectEast: TAction;
    acMoveObjectWest: TAction;
    acMoveObjectUp: TAction;
    acMoveObjectDown: TAction;
    pnlRight: TPanel;
    glvMesh: TGLVisir;
    Splitter1: TSplitter;
    Splitter4: TSplitter;
    pcRight: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    pnlDisplayOptions: TPanel;
    Panel4: TPanel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    acEditPolygonProperties: TAction;
    acDeletePolygons: TAction;
    acSplitWithGround: TAction;
    acJumpToObjectLocation: TAction;
    acHelp: TAction;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    acExportToXWF: TAction;
    lblTime: TLabel;
    tmrTimeOfDay: TTimer;
    acExportObjectS3D: TAction;
    acConvert3DSToMsh: TAction;
    TBDock1: TTBDock;
    TBToolbar1: TTBToolbar;
    mnuFile: TTBSubmenuItem;
    mnuNew: TTBItem;
    mnuOpen: TTBItem;
    mnuSave: TTBItem;
    mnuSaveAs: TTBItem;
    N6: TTBSeparatorItem;
    Properties1: TTBItem;
    N1: TTBSeparatorItem;
    Converttotransparentbitmap1: TTBItem;
    N9: TTBSeparatorItem;
    Import1: TTBSubmenuItem;
    mnuImport3DS: TTBItem;
    Import3DSgroundmesh1: TTBItem;
    acConvert3DSToMsh1: TTBItem;
    mnuImportOGREXMLMesh: TTBItem;
    LoadQuake3map1: TTBItem;
    Export1: TTBSubmenuItem;
    mnuExportToS3D: TTBItem;
    mnuExportToWLD: TTBItem;
    ExporttoXWF1: TTBItem;
    ExportzonelineinfotoZPT1: TTBItem;
    mnuCreateEmptyLights: TTBItem;
    mnuCreateEmptyObjects: TTBItem;
    GenerateShowEQAdminToolMAPfile1: TTBItem;
    ExportzoneobjectstoS3D1: TTBItem;
    N2: TTBSeparatorItem;
    mnuExit: TTBItem;
    mnuEdit: TTBSubmenuItem;
    mnuEditCut: TTBItem;
    mnuEditCopy: TTBItem;
    mnuEditPaste: TTBItem;
    mnuDelete: TTBItem;
    N4: TTBSeparatorItem;
    mnuChangeWaterLevel: TTBItem;
    Changepolygonproperties1: TTBItem;
    Deletepolygonfrommesh1: TTBItem;
    N10: TTBSeparatorItem;
    mnuInsertScene: TTBItem;
    mnuTranslate: TTBItem;
    N11: TTBSeparatorItem;
    Movenorth1: TTBItem;
    Movesouth1: TTBItem;
    Moveeast1: TTBItem;
    Movewest1: TTBItem;
    Moveup1: TTBItem;
    Movedown1: TTBItem;
    N7: TTBSeparatorItem;
    Shiftentirezone1: TTBItem;
    N8: TTBSeparatorItem;
    Preferences1: TTBItem;
    mnuView: TTBSubmenuItem;
    mnuChangeView: TTBItem;
    mnuViewScriptLog: TTBItem;
    Jumptoobjectlocation1: TTBItem;
    mnuObject: TTBSubmenuItem;
    mnuRename: TTBItem;
    mnuGroup: TTBItem;
    mnuUngroup: TTBItem;
    Exportasmesh1: TTBItem;
    acInsertMeshLibraryObject1: TTBItem;
    Insertlightsource1: TTBItem;
    Moveobjecttocurrentposition1: TTBItem;
    Splitwithgroundmesh1: TTBItem;
    Ground1: TTBSubmenuItem;
    mnuEditGround: TTBItem;
    N5: TTBSeparatorItem;
    Getgroundheight1: TTBItem;
    Setgroundheight1: TTBItem;
    Raiselowerland1: TTBItem;
    Flattengroundatpeakelevation1: TTBItem;
    Flattengroundataverageelevation1: TTBItem;
    Flattengroundatlowestelevation1: TTBItem;
    Rumpleground1: TTBItem;
    MountainizeEdges1: TTBItem;
    Addzoneexit1: TTBItem;
    Extendedges1: TTBItem;
    Rumplegroundwithinradius1: TTBItem;
    mnuHelp: TTBSubmenuItem;
    OpenZoneHelp1: TTBItem;
    mnuHelpKeys: TTBItem;
    About1: TTBItem;
    TBDock2: TTBDock;
    TBToolbar2: TTBToolbar;
    TBItem1: TTBItem;
    TBItem2: TTBItem;
    TBItem3: TTBItem;
    TBSeparatorItem1: TTBSeparatorItem;
    TBItem4: TTBItem;
    TBItem5: TTBItem;
    TBItem6: TTBItem;
    TBSeparatorItem2: TTBSeparatorItem;
    TBItem7: TTBItem;
    TBSeparatorItem3: TTBSeparatorItem;
    TBItem8: TTBItem;
    TBItem9: TTBItem;
    TBSeparatorItem4: TTBSeparatorItem;
    TBItem10: TTBItem;
    TBItem11: TTBItem;
    TBSeparatorItem5: TTBSeparatorItem;
    TBItem12: TTBItem;
    TBItem13: TTBItem;
    TBItem14: TTBItem;
    TBItem15: TTBItem;
    TBItem16: TTBItem;
    TBItem17: TTBItem;
    TBItem18: TTBItem;
    TBSeparatorItem6: TTBSeparatorItem;
    TBItem19: TTBItem;
    TBItem20: TTBItem;
    TBSeparatorItem7: TTBSeparatorItem;
    TBItem21: TTBItem;
    ilMain24: TImageList;
    acToolBarIconSize16: TAction;
    acToolBarIconSize24: TAction;
    TBSubmenuItem1: TTBSubmenuItem;
    TBItem22: TTBItem;
    TBItem23: TTBItem;
    ilMain32: TImageList;
    acToolBarIconSize32: TAction;
    TBItem24: TTBItem;
    ilMain64: TImageList;
    acToolBarIconSize64: TAction;
    TBItem25: TTBItem;
    acMenuIconSize16: TAction;
    acMenuIconSize24: TAction;
    acMenuIconSize32: TAction;
    acMenuIconSize64: TAction;
    TBSubmenuItem2: TTBSubmenuItem;
    TBItem26: TTBItem;
    TBItem27: TTBItem;
    TBItem28: TTBItem;
    TBItem29: TTBItem;
    TBItem30: TTBItem;
    Bevel8: TBevel;
    acImportXWF: TAction;
    TBItem31: TTBItem;
    acExportTo3DS: TAction;
    TBItem32: TTBItem;
    pnlMoveAndCompass: TPanel;
    Image2: TImage;
    Image1: TImage;
    rbCircularArea: TTrRadioButton;
    rbSquareArea: TTrRadioButton;
    Image3: TImage;
    cbShowTransparent: TTrCheckBox;
    cbTransparentAsSolid: TTrCheckBox;
    cbShowPlayerLight: TTrCheckBox;
    cbShowZonelines: TTrCheckBox;
    cbWalkAlongGround: TTrCheckBox;
    cbShowStars: TTrCheckBox;
    cbRunTime: TTrCheckBox;
    cbAnimateTextures: TTrCheckBox;
    btnNegate: TAboutColorButton;
    Image4: TImage;
    tvMeshes: TTreeView;
    acEditMeshLibraryObjectCategory: TAction;
    TBItem33: TTBItem;
    ilTreeView: TImageList;
    cbShowLightSources: TComboBox;
    Label9: TLabel;
    acInsertModelOrigin: TAction;
    TBItem34: TTBItem;
    Label11: TLabel;
    cbShowModelOrigins: TComboBox;
    acSetObjectPosToCrosshair: TAction;
    TBItem35: TTBItem;
    btnRotateObjectLeft: TSpeedButton;
    acRotateObjectLeft: TAction;
    acRotateObjectRight: TAction;
    btnRotateObjectRight: TSpeedButton;
    TBItem36: TTBItem;
    TBItem37: TTBItem;
    acImportAN8: TAction;
    TBItem38: TTBItem;
    acSplitSelectedMeshes: TAction;
    TBItem39: TTBItem;
    acConvertAN8ToMsh: TAction;
    TBItem40: TTBItem;
    acSizeUp10Percent: TAction;
    btnSizeUp: TSpeedButton;
    acSizeDown10Percent: TAction;
    btnSizeDown: TSpeedButton;
    TBItem41: TTBItem;
    TBItem42: TTBItem;
    acToggleInsertMeshesIntoZoneGeometry: TAction;
    TBItem43: TTBItem;
    acConvertToMesh: TAction;
    TBItem44: TTBItem;
    acSplitMeshes: TAction;
    TBItem45: TTBItem;
    acInvertMeshes: TAction;
    TBItem46: TTBItem;
    mnuMesh: TTBSubmenuItem;
    TBSeparatorItem8: TTBSeparatorItem;
    TBSeparatorItem9: TTBSeparatorItem;
    TBSeparatorItem10: TTBSeparatorItem;
    TBSeparatorItem11: TTBSeparatorItem;
    TBSeparatorItem12: TTBSeparatorItem;
    TBSeparatorItem13: TTBSeparatorItem;
    TBSeparatorItem14: TTBSeparatorItem;
    TBSeparatorItem15: TTBSeparatorItem;
    acCombineMeshes: TAction;
    TBItem47: TTBItem;
    acFlipTexCoords: TAction;
    TBItem48: TTBItem;
    acMultiplyTexCoords: TAction;
    TBItem49: TTBItem;
    acChangeLibraryReference: TAction;
    TBItem50: TTBItem;
    acExportToSQL: TAction;
    TBItem51: TTBItem;
    Label12: TLabel;
    cbShowHotSpots: TComboBox;
    acInsertHotSpot: TAction;
    TBItem52: TTBItem;
    acMoveObjectToLastHotSpot: TAction;
    TBItem53: TTBItem;
    acSelectHotspot: TAction;
    TBItem54: TTBItem;
    edtSetHeight: TEdit;
    acTutorial: TAction;
    TBItem55: TTBItem;
    ClearTypeLabel1: TClearTypeLabel;
    ClearTypeLabel2: TClearTypeLabel;
    ClearTypeLabel3: TClearTypeLabel;
    ClearTypeLabel4: TClearTypeLabel;
    acExportToXWA: TAction;
    TBItem56: TTBItem;
    TBSeparatorItem16: TTBSeparatorItem;
    TBSeparatorItem17: TTBSeparatorItem;
    TBSeparatorItem18: TTBSeparatorItem;
    acExportToAN8: TAction;
    TBItem57: TTBItem;
    acAlterAn8File: TAction;
    TBItem58: TTBItem;
    cbRealisticSky: TTrCheckBox;
    acSaveSelectedAs: TAction;
    TBItem59: TTBItem;
    imgElevation: TImage;
    acImportDirectX: TAction;
    TBItem60: TTBItem;
    acConvertDXToAN8: TAction;
    TBItem61: TTBItem;
    cbEnableShaders: TTrCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tbPaletteChange(Sender: TObject);
    procedure tvMainChange(Sender: TObject; Node: TTreeNode);
    procedure cbTextureSetChange(Sender: TObject);
    procedure cbShowTransparentClick(Sender: TObject);
    Procedure ImportFromOGREXMLMesh(FileName: String);
    procedure tvMainDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvMainDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure acNewSceneExecute(Sender: TObject);
    procedure acOpenSceneExecute(Sender: TObject);
    procedure acSaveSceneExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acImport3DSExecute(Sender: TObject);
    procedure acImportOGREExecute(Sender: TObject);
    procedure acExportToWLDExecute(Sender: TObject);
    procedure acExportToS3DExecute(Sender: TObject);
    procedure acViewScriptLogExecute(Sender: TObject);
    procedure acCreateLightsWLDExecute(Sender: TObject);
    procedure acCreateObjectsWLDExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acEditCutExecute(Sender: TObject);
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
    procedure acChangeWaterExecute(Sender: TObject);
    procedure acEditGroundExecute(Sender: TObject);
    procedure acInsertSceneExecute(Sender: TObject);
    procedure acTranslateExecute(Sender: TObject);
    procedure acChangeViewExecute(Sender: TObject);
    procedure acEditDeleteExecute(Sender: TObject);
    procedure acRenameExecute(Sender: TObject);
    procedure acGroupExecute(Sender: TObject);
    procedure acUngroupExecute(Sender: TObject);
    procedure acHelpMovementKeysExecute(Sender: TObject);
    procedure acGetGroundHeightExecute(Sender: TObject);
    procedure acSetGroundHeightExecute(Sender: TObject);
    procedure cbCrosshairChange(Sender: TObject);
    procedure acRaiseLandExecute(Sender: TObject);
    procedure edtRaiseLowerRadiusChange(Sender: TObject);
    procedure acFlattenAtPeakExecute(Sender: TObject);
    procedure acFlattenAtAverageExecute(Sender: TObject);
    procedure acFlattenAtLowestExecute(Sender: TObject);
    procedure acRumpleGroundExecute(Sender: TObject);
    procedure acMountainizeExecute(Sender: TObject);
    procedure acAddZoneExitExecute(Sender: TObject);
    procedure acExportAsMeshExecute(Sender: TObject);
    procedure acInsertMeshLibraryObjectExecute(Sender: TObject);
    procedure acInsertLightSourceExecute(Sender: TObject);
    procedure acConvertToTransparentExecute(Sender: TObject);
    procedure acExtendEdgesExecute(Sender: TObject);
    procedure acRumpleGroundWithinRadiusExecute(Sender: TObject);
    procedure rbCircularAreaClick(Sender: TObject);
    procedure rbSquareAreaClick(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG;
      var Handled: Boolean);
    procedure acImportGround3DSExecute(Sender: TObject);
    procedure acExportZoneLineInfoExecute(Sender: TObject);
    procedure cbShowPlayerLightClick(Sender: TObject);
    procedure tbTimeOfDayChange(Sender: TObject);
    procedure acGenerateMapExecute(Sender: TObject);
    procedure acZonePropertiesExecute(Sender: TObject);
    procedure acPreferencesExecute(Sender: TObject);
    procedure acShiftZoneExecute(Sender: TObject);
    procedure cbTransparentAsSolidClick(Sender: TObject);
    procedure acSetObjectPosToCurrentExecute(Sender: TObject);
    procedure btnNegateClick(Sender: TObject);
    procedure ZPropList1NewObject(Sender: TZPropList; OldObj,
      NewObj: TObject);
    procedure acHelpAboutExecute(Sender: TObject);
    procedure glViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure glViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure glViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure acImportQuake3MapExecute(Sender: TObject);
    procedure acMoveObjectNorthExecute(Sender: TObject);
    procedure acMoveObjectSouthExecute(Sender: TObject);
    procedure acMoveObjectEastExecute(Sender: TObject);
    procedure acMoveObjectWestExecute(Sender: TObject);
    procedure acMoveObjectUpExecute(Sender: TObject);
    procedure acMoveObjectDownExecute(Sender: TObject);
    procedure pcRightMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure acEditPolygonPropertiesExecute(Sender: TObject);
    procedure acDeletePolygonsExecute(Sender: TObject);
    procedure acSplitWithGroundExecute(Sender: TObject);
    procedure cbShowZonelinesClick(Sender: TObject);
    procedure acJumpToObjectLocationExecute(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure cbShowStarsClick(Sender: TObject);
    procedure acExportToXWFExecute(Sender: TObject);
    procedure tmrTimeOfDayTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acExportObjectS3DExecute(Sender: TObject);
    procedure acConvert3DSToMshExecute(Sender: TObject);
    procedure acToolBarIconSize16Execute(Sender: TObject);
    procedure acToolBarIconSize24Execute(Sender: TObject);
    procedure acToolBarIconSize32Execute(Sender: TObject);
    procedure acToolBarIconSize64Execute(Sender: TObject);
    procedure acMenuIconSize16Execute(Sender: TObject);
    procedure acMenuIconSize24Execute(Sender: TObject);
    procedure acMenuIconSize32Execute(Sender: TObject);
    procedure acMenuIconSize64Execute(Sender: TObject);
    procedure cbAnimateTexturesClick(Sender: TObject);
    procedure ZPropList1PropNameHint(Sender: TZPropList; Prop: IProperty;
      HintInfo: PHintInfo);
    procedure acImportXWFExecute(Sender: TObject);
    procedure acExportTo3DSExecute(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure pcRightResize(Sender: TObject);
    procedure pnlMoveAndCompassResize(Sender: TObject);
    procedure tvMeshesChange(Sender: TObject; Node: TTreeNode);
    procedure acEditMeshLibraryObjectCategoryExecute(Sender: TObject);
    procedure tvMeshesCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure tvMeshesCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure cbShowLightSourcesChange(Sender: TObject);
    procedure acInsertModelOriginExecute(Sender: TObject);
    procedure cbShowModelOriginsChange(Sender: TObject);
    procedure acSetObjectPosToCrosshairExecute(Sender: TObject);
    procedure acRotateObjectLeftExecute(Sender: TObject);
    procedure acRotateObjectRightExecute(Sender: TObject);
    procedure acImportAN8Execute(Sender: TObject);
    procedure acSplitSelectedMeshesExecute(Sender: TObject);
    procedure acConvertAN8ToMshExecute(Sender: TObject);
    procedure acSizeUp10PercentExecute(Sender: TObject);
    procedure acSizeDown10PercentExecute(Sender: TObject);
    procedure acToggleInsertMeshesIntoZoneGeometryExecute(Sender: TObject);
    procedure acConvertToMeshExecute(Sender: TObject);
    procedure acSplitMeshesExecute(Sender: TObject);
    procedure acInvertMeshesExecute(Sender: TObject);
    procedure acCombineMeshesExecute(Sender: TObject);
    procedure acFlipTexCoordsExecute(Sender: TObject);
    procedure acMultiplyTexCoordsExecute(Sender: TObject);
    procedure acChangeLibraryReferenceExecute(Sender: TObject);
    procedure acExportToSQLExecute(Sender: TObject);
    procedure cbShowHotSpotsChange(Sender: TObject);
    procedure acInsertHotSpotExecute(Sender: TObject);
    procedure acMoveObjectToLastHotSpotExecute(Sender: TObject);
    procedure acSelectHotspotExecute(Sender: TObject);
    procedure edtSetHeightChange(Sender: TObject);
    procedure acTutorialExecute(Sender: TObject);
    procedure tvMeshesAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure acExportToXWAExecute(Sender: TObject);
    procedure acExportToAN8Execute(Sender: TObject);
    procedure acAlterAn8FileExecute(Sender: TObject);
    procedure cbWalkAlongGroundClick(Sender: TObject);
    procedure cbRealisticSkyClick(Sender: TObject);
    procedure acSaveSelectedAsExecute(Sender: TObject);
    procedure acImportDirectXExecute(Sender: TObject);
    procedure acConvertDXToAN8Execute(Sender: TObject);
    procedure cbEnableShadersClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Zone                   : TZone;
    Observer               : T3DPoint;
    Theta                  : Single;
    Phi                    : Single;
    BirdsEye               : Boolean;
    ZoneMinX               : Single;
    ZoneMinY               : Single;
    ZoneMinZ               : Single;
    ZoneMaxX               : Single;
    ZoneMaxY               : Single;
    ZoneMaxZ               : Single;
    LoadingParms           : Boolean;
    MasterTexList          : TSharedComboBoxStrings;
    OldRow                 : Integer;
    UpdatingCBProps        : Boolean;
    LastNode               : TTreeNode;
    LastLoadedScene        : String;
    SettingTexSet          : Boolean;
    ObjLoc                 : T3DPoint;
    ObjRotate              : THeading;
    ObjSize                : T3DPoint;
    ParameterNames         : Array Of String;
    ParameterValues        : Array Of String;
    DraggedNode            : TTreeNode;
    Clipboard              : TStringList; // List of TZoneObject
    Tree                   : TTree;
    SetHeight              : Single;
    Crosshair              : TCrosshairType;
    CrosshairX             : Integer;
    CrosshairY             : Integer;
    SelectedMesh           : String;
    SelectedCreature       : String;
    SelectedPolygons       : TList;       // List of polygon indices
    SelectedPolyMesh       : TMeshObject;
    SplashShowTime         : Cardinal;
    HighestPoint           : T3DPoint;
    HPV1                   : T3DPoint;
    HPV2                   : T3DPoint;
    HPV3                   : T3DPoint;
    MasterTree             : TTree;
    MasterTreeMesh         : TMeshObject;
    MasterTreeMinPt        : T3DPoint;
    MasterTreeMaxPt        : T3DPoint;
    MasterMeshList         : TStringList;
    ClickIndex             : Integer;
    LeftDown               : Boolean;
    ClickIntersect         : T3DPoint;
    ClickObs               : T3DPoint;
    ClickDest              : T3DPoint;
    ClickPoly              : TPolygon;
    ClickPolyIndex         : Integer;
    LastTickCount          : LongWord;
    CurrTickCount          : LongWord;
    UpdatingFromRender     : Boolean;
    UpdatingFromSlider     : Boolean;
    TimeOfDay              : TTimeOfDay;
    ProgramSettings        : TProgramSettings;
    LoadingMeshLibraryList : Boolean;
    SelectedHotSpot        : THotSpot;
    SelectedHotSpotML      : TMeshLibraryObjectReference;
    AllowHotSpotChange     : Boolean;
    SettingSetHeightEdit   : Boolean;
    Procedure CreateNewZone;
    Procedure LoadScene(FileName: String);
    Procedure SaveScene(FileName: String);
    Procedure SaveSelectedScene(FileName: String);
    Procedure RenderZone(BirdsEyeView: Boolean);
    Procedure DisplayStats;
    Procedure ReloadModel(L: TStringList; Breakup: Boolean);
    Function  ReloadMesh(L: TStringList; Base,Index: Integer; Breakup,AddWater,Master: Boolean; View: TGLVisir; Var Tree: TTree; Center: Boolean; TexPath: String): TEntity;
    Procedure AddLightSpheres;
    Procedure AddModelOrigins;
    Procedure AddHotSpots;
    Procedure CalcTextureCoords(Mesh: TMeshObject; Index: Integer; View: TGLVisir; Master: Boolean; Var Mdl: TModelRec);
    Procedure LoadParameters;
    Procedure ReloadZone;
    Procedure UpdateParameter(Value: String; Element: Integer);
    Procedure BuildScriptPalette;
    Procedure LoadScriptBitmaps;
    Procedure SetupPalette;
    Procedure LoadObjectTreeView;
    Procedure RefreshObject(ZO: TZoneObject; RedrawScene,SaveAndRestore,HandleSelected: Boolean);
    Procedure CreateObject(Sender: TObject);
    Procedure AddNewObject(St: String; ZO: TZoneObject);
    Function  GetGridRect(ACol,ARow: Integer; SG: TStringGrid): TRect;
    Procedure ExportToWLD(Stream: TStream; FileName: String; ZoneMesh: TMeshObject = Nil);
    Procedure UpdateTitleBar;
    Function  WaterAreaCount: Integer;
    Function  LavaAreaCount: Integer;
    Function  PvPAreaCount: Integer;
    Function  IceAreaCount: Integer;
    Function  IceWaterAreaCount: Integer;
    Procedure ImportFrom3DS(FileName: String);
    Procedure ImportFromXWF(FileName: String);
    Procedure ImportFromAN8(FileName: String; Rotate: Boolean);
    Procedure ImportFromDirectX(FileName: String; Rotate: Boolean);
    Procedure ImportGroundFrom3DS(FileName: String);
    Procedure CreateEmptyWLD(Stream: TStream);
    Function  CreateObjWLD(Stream: TStream; ZoneObjects: Boolean): TStringList;
    Function  CreateChrWLD(Stream: TStream): TStringList;
    Procedure CreateLightsWLD(Stream: TStream);
    Procedure CreateEmptyLightsWLD(Stream: TStream);
    Procedure CreateObjectsWLD(Stream: TStream; ZoneMesh: TMeshObject);
    Procedure CreateEmptyObjectsWLD(Stream: TStream);
    Procedure ExportToXWA(Stream: TStream; ZoneName: String);
    Procedure ExportToS3D(Stream: TStream; ZoneName: String);
    Procedure ExportToEQEmuMap(Stream: TStream);
    Procedure ExportToDoorsSQL(FileName: String);
    Procedure ExportToObjectsSQL(FileName: String);
    Procedure ExportToChrS3D(Stream: TStream; WLDName: String);
    Procedure ExportToObjS3D(Stream: TStream; WLDName: String; ZoneObjects: Boolean);
    Procedure UpdatePosition(Value: String; Element: Integer);
    Procedure UpdateRotation(Value: String; Element: Integer);
    Procedure UpdateSize(Value: String; Element: Integer);
    Procedure UpdateSQLRef(Value: String; Element: Integer);
    Procedure EnableGUI(B: Boolean);
    Procedure CopyToClipboard;
    Procedure SetViewToNorth;
    Procedure SetCrosshair;
    Procedure SetSelectionRectangles;
    Procedure SetPolygonSelectionRectangles;
    Procedure SetZoneLinePolygons;
    Procedure RegenerateGround;
    Procedure RegenerateGroundMeshes;
    Procedure UpdateStatusBar;
    Procedure RegenerateMeshesWithGravity;
    Procedure LoadPreferences;
    Procedure SavePreferences;
    Procedure MakeSoundFiles(ZoneName: String);
    Function  ValidSelections: Boolean;
    Procedure FreeMasterMeshList;
    Function  GetNodeForMesh(TN: TTreeNode; Var Count: Integer): TTreeNode;
    Function  GetMeshIndexAtScreenPos(X,Y: Integer; SaveIntersect: Boolean): Integer;
    Procedure CalculateClickRay(X,Y: Integer);
    Procedure GenerateStarField;
    Procedure LoadMeshLibraryList(tvMeshes: TTreeView);
    Procedure FlattenGroundAtElevation(Elevation: Single);
    Function  GetHighestZ(Const X,Y: Single; Out Z: Single): Boolean;
    Procedure LoadCreatureSkeleton(An8File: TAn8File; View: TGLVisir; Entity: TEntity; ZO: TZoneObject);
    Procedure SetClearTypeSettings;
    Procedure LoadDefaultShaders;
    Procedure LoadDynamicLights;
    procedure TBItemPrePaint(Item: TTBCustomItem;
      Viewer: TTBItemViewer; IsRotated: Boolean; const Canvas: TCanvas;
      ARect: TRect; const ACaption: String; ADrawDisabledShadow: Boolean;
      AFormat: Cardinal);
  end;

Function LoadImage(FileName: String): TBitmap;

var
  frmMain: TfrmMain;

implementation

uses frmNewZoneUnit,Math,GL,ZoneFile, frmScriptLogUnit, EQWldData, frmStatusUnit,
  frmWaterPropertiesUnit, frmHelpKeysUnit, frmEditTextureListUnit, Model_3DSMax,S3D,Targa,
  frmTexPickerUnit, frmTranslateUnit, frmMountainizeUnit, OpenIL, OpenILU, OpenILUT,
  frmSelectMeshUnit, Expressions, frmConvertBMPUnit, frmExtendGroundUnit,
  frmGenerateMapUnit, frmZonePropertiesUnit,INIFiles, frmPreferencesUnit,
  frmShiftZoneUnit, frmAddZoneExitUnit, frmSplashUnit, Q3BSP, Q3Shaders, 
  frmEditPolygonPropertiesUnit,Sorter, frmXWFExportOptionsUnit, CommCtrl,
  MyHashes, frmFlipTexCoordsUnit, frmMultiplyTexCoordsUnit,
  frmSelectHotspotUnit, frmDeleteAN8ObjectsUnit;

Const
  ObsHeight         = 8;
  MoveSpeed         = 12;
  RotateAmount      = 0.10;
  CreateDist        = 45;
  GetHeightDist     = 0;//16;
  TempDDSFileName   = '_TEMPDDS_.DDS';
  SpecialTexture    = 128;
  spSemiTransparent = 1;
  spMasked          = 2;

  // Special entities

  EntityCrosshair                 = 0;
  EntitySelectionRectangle        = 1;
  EntityPolygonSelectionRectangle = 2;
  EntityZoneLines                 = 3;
  EntityLightSpheres              = 4;
  EntityModelOrigin               = 5;
  EntityHotSpot                   = 6;

Var
  TagList    : TTagListArray;
  SortRegion : TRegion;
  SortMesh   : TMeshObject;

{$R *.dfm}

Procedure AddFragment(W: TEQWldData; D: EQWldDataFragment);
Begin
  Inc(W.nFragment);
  SetLength(W.Fragments,W.nFragment);
  W.Fragments[W.nFragment - 1] := D;
End; // AddFragment

Function GetFirstData03(W: TEQWldData): EQWldDataFragment;
Var
  I     : Integer;
  Found : Boolean;

Begin
  I      := 0;
  Found  := False;
  Result := Nil;
  While (I <= High(W.Fragments)) And Not Found Do
  Begin
    If (W.Fragments[I] <> Nil) And (W.Fragments[I].ID = 3) Then
    Begin
      Result := W.Fragments[I];
      Found  := True;
    End
    Else Inc(I);
  End; // While
End; // GetFirstData03

Procedure MakeTransparentTextureEntry(W: TEQWldData; D31: Data31);
Var D30: Data30;
Begin
  D30 := Data30.Create(W,'M0000_MDF');
  D30.Flags := 2;
  D30.Params1[0] := 0;
  D30.Params2[0] := $004E4E4E;
  D30.Params3[0] := 0;
  D30.Params3[1] := 0;
  D30.Fragment   := FragmentReference.Create;
  D30.Fragment.Fragment := Nil;
  D30.Fragment.Position := 0;
  D30.Pair.Data1 := 0;
  D30.Pair.Data2 := 0;

  AddFragment(W,D30);

  Inc(D31.Size1);
  SetLength(D31.Data1,High(D31.Data1) + 2);
  D31.Data1[High(D31.Data1)]          := FragmentReference.Create;
  D31.Data1[High(D31.Data1)].Fragment := D30;
End; // MakeTransparentTextureEntry

Procedure MakeTextureEntry(W: TEQWldData; D31: Data31; Texture,FragName: String; SemiTransparent,Masked: Boolean);
Var
  I,J         : Integer;
//  Tokens      : TTokenArray;
  D03         : Array Of Data03;
  D04         : Data04;
  D05         : Data05;
  D30         : Data30;
  TextureInfo : TTextureInfo;
  St          : String;
  St0         : String;
//  Textures   : String;
//  Opacities  : String;
//  Parameters : String;
  AnimTime   : Single;

  Function NewTexName(Texture: String; Index: Integer): String;
  Begin
    Result := Copy(Texture,1,8) + '_' + IntToStr(Index) + '_frag' + IntToStr(W.nFragment)
//    Result := Texture + '_' + IntToStr(D31.Size1 + Index);
  End; // NewTexName

Begin
  // It may actually be an animated texture, so separate it into a list

//  BreakupTextureString(Texture,Textures,Opacities,Parameters);
  TextureInfo := TTextureInfo.Create(Texture);
  If TextureInfo.TextureMaps.Count > 0 Then//Textures <> '' Then
  Begin
//    GetTokens(';',Textures,Tokens);
    SetLength(D03,TextureInfo.TextureMaps.Count);//High(Tokens) + 1);
    St0 := '';
    For I := 0 To TextureInfo.TextureMaps.Count - 1 Do//High(Tokens) Do
    Begin
      St := TextureInfo.TextureMaps.Strings[I];
      If I = 0 Then St0 := St;

      // Strip off the texture set reference

      J := LastDelimiter('/',St);//Tokens[I]);
      If J = 0 Then J := LastDelimiter('\',St);//Tokens[I]);
      If J > 0 Then St := Copy(St,J + 1,Length(St{Tokens[I]}));

      // Make the filename entry

      If FragName = '' Then D03[I] := Data03.Create(W,NewTexName(St{Tokens[I]},I))
      Else
      Begin
        If TextureInfo.TextureMaps.Count = 1//High(Tokens) = 0
         Then D03[I] := Data03.Create(W,FragName)
         Else D03[I] := Data03.Create(W,FragName + '_' + IntToStr(I));
      End;
      D03[I].Size1 := 1;
      SetLength(D03[I].Data1,1);
      SetLength(D03[I].Sizes,1);
      D03[I].Sizes[0] := Length(TextureInfo.TextureMaps.Strings[I]{Tokens[I]}) + 5;
      SetLength(D03[I].Data1[0],D03[I].Sizes[0]);
      StrPCopy(@D03[I].Data1[0][0],St + '.BMP');
    End; // For I

    If FragName = ''
     Then D04 := Data04.Create(W,NewTexName(St0{Tokens[0]},0) + '_SPRITE')
     Else D04 := Data04.Create(W,FragName + '_SPRITE');
    If TextureInfo.TextureMaps.Count > 1 Then//High(Tokens) > 0 Then
    Begin
      D04.Flags      := $18;
      D04.Params2[0] := $64;
//      Val(Parameters,AnimTime,I);
//      If I <> 0 Then AnimTime := (High(Tokens) + 1) * DefaultAnimTimePerFrame;
      AnimTime := TextureInfo.AnimTime;
      If AnimTime < TextureInfo.TextureMaps.Count{(High(Tokens) + 1)} * MinimumAnimTimePerFrame Then AnimTime := TextureInfo.TextureMaps.Count{(High(Tokens) + 1)} * MinimumAnimTimePerFrame;
      If AnimTime > 100 Then AnimTime := 100;
      AnimTime := AnimTime / TextureInfo.TextureMaps.Count;//(High(Tokens) + 1);
      D04.Params2[0] := Round(AnimTime * 1000); // Milliseconds per frame
    End
    Else
    Begin
      D04.Flags      := $10;
      D04.Params2[0] := 0;
    End;
    D04.Size1      := TextureInfo.TextureMaps.Count;//High(Tokens) + 1;
    SetLength(D04.Data1,TextureInfo.TextureMaps.Count);//High(Tokens) + 1);
    For I := 0 To TextureInfo.TextureMaps.Count Do//High(Tokens) Do
    Begin
      D04.Data1[I]          := FragmentReference.Create;
      D04.Data1[I].Fragment := D03[I];
    End; // For I
    D04.Params1[0] := 0;

    D05                   := Data05.Create(W);
    D05.Flags             := $50;
    D05.Fragment          := FragmentReference.Create;
    D05.Fragment.Fragment := D04;

    If FragName = ''
     Then D30 := Data30.Create(W,NewTexName(St0{Tokens[0]},0) + '_MDF')
     Else D30 := Data30.Create(W,FragName + '_MDF');
    D30.Flags := 2;

    // $00000000 .... nothing
    // $00000001 .... color, no texture
    // $00000005 .... color, no texture

    // $40000005 .... color, no texture

    // $80000000 .... nothing
    // $80000001 .... color and texture
    // $80000002 .... color and texture
    // $80000003 .... color, no texture
    // $80000004 .... color, no texture
    // $80000005 .... color and texture, semitransparent
    // $80000006 .... color and texture, semitransparent
    // $80000007 .... color and texture
    // $80000008 .... color and texture
    // $80000009 .... color and texture, semitransparent

    // $8000000B .... used by fire textures (don't know what it does yet)

    // $80000013 .... color and texture, masked

    // $C0000005 .... color and texture, semitransparent

    // $F0000005 .... crashes the client

    D30.Params1[0] := $80000001;

    If Masked Then
    Begin
      If SemiTransparent
       Then D30.Params1[0] := D30.Params1[0] Or $A
       Else D30.Params1[0] := D30.Params1[0] Or $12;
    End
    Else If SemiTransparent Then D30.Params1[0] := D30.Params1[0] Or 4;
  {
    If SemiTransparent Then D30.Params1[0] := D30.Params1[0] Or 4;
    If Masked          Then D30.Params1[0] := D30.Params1[0] Or $A;//$12;
  }
    D30.Params2[0] := { $00B2B2B2;} $004E4E4E;
    D30.Params3[0] := 0;
    D30.Params3[1] := 0.75;
    D30.Fragment   := FragmentReference.Create;
    D30.Fragment.Fragment := D05;
    D30.Pair.Data1 := 0;
    D30.Pair.Data2 := 0;

    For I := 0 To TextureInfo.TextureMaps.Count{High(Tokens)} Do AddFragment(W,D03[I]);
    AddFragment(W,D04);
    AddFragment(W,D05);
    AddFragment(W,D30);

    If D31 <> Nil Then
    Begin
      Inc(D31.Size1);
      SetLength(D31.Data1,High(D31.Data1) + 2);
      D31.Data1[High(D31.Data1)]          := FragmentReference.Create;
      D31.Data1[High(D31.Data1)].Fragment := D30;
    End;
//    SetLength(Tokens,0);
    SetLength(D03,0);
  End;
  TextureInfo.Free;
End; // MakeTextureEntry

Function Capitalize(St: String): String;
Var
  I   : Integer;
  Cap : Boolean;

Function LoCase(C: Char): Char;
Begin
  If C In ['A'..'Z'] Then C := Chr(Ord(C) + 32);
  Result := C;
End; // LoCase

Begin
  Cap := True;
  For I := 1 To Length(St) Do
  Begin
    If Cap Then St[I] := UpCase(St[I]) Else St[I] := LoCase(St[I]);
    Cap := (St[I] In [' ','_']);
  End; // For I
  Result := St;
End; // Capitalize

Procedure ConvertBMPtoDDS(FileName: String);
Var
  ImgID   : TILuint;
  P       : PChar;
  Buffer  : Packed Array Of LongWord;
  St      : String;
  I,J     : Integer;
  Texture : TTexture;
  P1,P2   : PLongWord;
  L       : LongWord;
  OpacMap : Boolean;
  W,H     : Integer;
  Info    : TILInfo;

  Function GetHighPowerOf2(X: Integer): Integer;
  Var I: Integer;
  Begin
    I := 1;
    While (I < X) And (I <> 0) Do I := I Shl 1;
    If I = 0 Then I := 256;
    Result := I;
  End; // GetHighPowerOf2

Begin
  // Check if the shared lib's version matches the executable's version.

  If (ilGetInteger(IL_VERSION_NUM) < IL_VERSION) Or
     (iluGetInteger(ILU_VERSION_NUM) < ILU_VERSION) Then Exit;

  // Generate the main image name to use.

  ilGenImages(1, @ImgId);

  // Bind this image name.

  ilBindImage(ImgId);

  // Loads the image specified by File into the image named by ImgId.

  GetMem(P,Length(FileName) + 1);
  StrPCopy(P,FileName);
  If ilLoadImage(P) = IL_FALSE Then
  Begin
    FreeMem(P);
    Exit;
  End;
  FreeMem(P);

  // Enable this to let us overwrite the destination file if it already exists.

  ilEnable(IL_FILE_OVERWRITE);

  // If this texture uses a separate opacity map then we need to write a texture that
  // takes it into account

  St := UpperCase(FileName);

  St := Copy(St,Length(ExtractFilePath(Application.ExeName)) + 1,Length(St));
  If Copy(St,1,1) = '\' Then St := Copy(St,2,Length(St));
  St := Copy(St,Length('library\textures\') + 1,Length(St));

//  St := ExtractFileName(UpperCase(FileName));
  St := Copy(St,1,Length(St) - Length(ExtractFileExt(St)));
  I  := frmMain.glView.Scene3D.Scene.Textures.IndexOf(ExtractFilePath(Application.ExeName) + 'library\textures\' + St + '.bmp');
  OpacMap := False;
  If I >= 0 Then
  Begin
    Texture := TTexture(frmMain.glView.Scene3D.Scene.Textures.Objects[I]);

    iluGetImageInfo(@Info);
    If ((Texture.BMPInfo.Width <> Info.Width) Or (Texture.BMPInfo.Height <> Info.Height)) Then iluScale(Texture.BMPInfo.Width,Texture.BMPInfo.Height,Info.Depth);

    If Texture.OBuffer3 <> Nil Then
    Begin
      ilConvertImage(IL_BGRA,IL_UNSIGNED_BYTE);
      J := Texture.BMPInfo.Width * Texture.BMPInfo.Height;
      SetLength(Buffer,J * 4);
      P1 := Texture.Buffer3;
      P2 := Texture.OBuffer3;
      For I := 0 To J - 1 Do
      Begin
        L := P1^;
        If TBGRA(P2^).R = 0 Then TBGRA(L).A := 0;
        Buffer[I] := L;
        Inc(LongWord(P1),4);
        Inc(LongWord(P2),4);
      End; // For I
      ilSetPixels(0,0,0,Texture.BMPInfo.Width,Texture.BMPInfo.Height,1,IL_BGRA,IL_UNSIGNED_BYTE,@Buffer[0]);
      SetLength(Buffer,0);
      OpacMap := True;
    End
    Else If Texture.NeedsMask Then
    Begin
      ilConvertImage(IL_BGRA,IL_UNSIGNED_BYTE);
      J := Texture.BMPInfo.Width * Texture.BMPInfo.Height;
      SetLength(Buffer,J * 4);
      P1 := Texture.Buffer3;
      For I := 0 To J - 1 Do
      Begin
        L := P1^;
        Buffer[I] := L;
        Inc(LongWord(P1),4);
      End; // For I
      ilSetPixels(0,0,0,Texture.BMPInfo.Width,Texture.BMPInfo.Height,1,IL_BGRA,IL_UNSIGNED_BYTE,@Buffer[0]);
      SetLength(Buffer,0);
      OpacMap := True;
    End;

    // If the image is not a power of 2 on each side, expand it until it is

    W := GetHighPowerOf2(Texture.BMPInfo.Width);
    H := GetHighPowerOf2(Texture.BMPInfo.Height);
    If (W <> Texture.BMPInfo.Width) Or (H <> Texture.BMPInfo.Height) Then
    Begin
      iluGetImageInfo(@Info);
      iluScale(W,H,Info.Depth);
    End;
  End;

  // Flip the image vertically since .DDS images are normally upside-down (but don't
  // do it if we altered the image since our internal images are already flipped)

  If Not OpacMap Then iluFlipImage();

  // Save to our temp filename

  St := ExtractFilePath(Application.ExeName) + TempDDSFileName;
  GetMem(P,Length(St) + 1);
  StrPCopy(P,St);
  ilSaveImage(P);
  FreeMem(P);

  // We're done with the image, so let's delete it.

  ilDeleteImages(1, @ImgId);
End; // ConvertBMPtoDDS

Function LoadImage(FileName: String): TBitmap;
Var
  ImgID  : TILuint;
  P      : PChar;
  BMP    : TBitmap;
  I      : Integer;
  W,H    : TILuint;
  Info   : TILInfo;

  Function GetHighPowerOf2(X: Integer): Integer;
  Var I: Integer;
  Begin
    I := 1;
    While (I < X) And (I <> 0) Do I := I Shl 1;
    If I = 0 Then I := 256;
    Result := I;
  End; // GetHighPowerOf2

Begin
  // Check if the shared lib's version matches the executable's version.

  If (ilGetInteger(IL_VERSION_NUM) < IL_VERSION) Or
     (iluGetInteger(ILU_VERSION_NUM) < ILU_VERSION) Or
     Not FileExists(FileName) Then
  Begin
    Result := Nil;
    Exit;
  End;

  // Generate the main image name to use.

  ilGenImages(1, @ImgId);

  // Bind this image name.

  ilBindImage(ImgId);

  // Enable setting the image origin and set it so that it is always at the upper left corner

  ilEnable(IL_ORIGIN_SET);
  ilOriginFunc(IL_ORIGIN_UPPER_LEFT);

  // Loads the image specified by File into the image named by ImgId.

  GetMem(P,Length(FileName) + 1);
  StrPCopy(P,FileName);
  If ilLoadImage(P) = IL_FALSE Then
  Begin
    FreeMem(P);
    Result := Nil;
    Exit;
  End;
  FreeMem(P);

  // If the image is not a power of 2 on each side, expand it until it is

  iluGetImageInfo(@Info);
  W := GetHighPowerOf2(Info.Width);
  H := GetHighPowerOf2(Info.Height);
  If (W <> Info.Width) Or (H <> Info.Height) Then
  Begin
    ShowMessage('Warning: image ' + ExtractFileName(FileName) + ' does not have power-of-two dimensions.'); 
    iluScale(W,H,Info.Depth);
  End;

  // Create the bitmap and copy the pixels

  W               := ilGetInteger(IL_IMAGE_WIDTH);
  H               := ilGetInteger(IL_IMAGE_HEIGHT);
  BMP             := TBitmap.Create;
  BMP.Pixelformat := pf32bit;
  BMP.Width       := W;
  BMP.Height      := H;
  Result          := BMP;
  If (Info.Format <> IL_BGRA) Or (Info._Type <> IL_UNSIGNED_BYTE) Then ilConvertImage(IL_BGRA,IL_UNSIGNED_BYTE);

  For I := 0 To H - 1 Do ilCopyPixels(0,I,0,W,1,1,IL_BGRA,IL_UNSIGNED_BYTE,BMP.ScanLine[I]);

  // We're done with the image, so let's delete it.

  ilDeleteImages(1, @ImgId);
End; // LoadImage

// ------------------------------------
// TfrmMain
// ------------------------------------

Procedure TfrmMain.LoadPreferences;
Var INI: TINIFile;
Begin
  INI                                   := TINIFile.Create(ExtractFilePath(Application.ExeName) + INIFileName);
  ProgramSettings.EQDir                 := INI.ReadString('GENERAL','EQDIR','');
  ProgramSettings.MenuIconSize          := INI.ReadInteger('GENERAL','MENUICONSIZE',16);
  ProgramSettings.ToolbarIconSize       := INI.ReadInteger('GENERAL','TOOLBARICONSIZE',16);
  ProgramSettings.UseNumericKeypad      := INI.ReadBool('GENERAL','USENUMERICKEYPAD',True);
  ProgramSettings.ShowZoneDimensions    := INI.ReadBool('GENERAL','SHOWZONEDIMENSIONS',True);
  ProgramSettings.AnimateTextures       := INI.ReadBool('GENERAL','ANIMATETEXTURES',True);
  ProgramSettings.ShowLightSources      := INI.ReadInteger('GENERAL','SHOWLIGHTSOURCES',0);
  ProgramSettings.ShowModelOrigins      := INI.ReadInteger('GENERAL','SHOWMODELORIGINS',0);
  ProgramSettings.ShowHotSpots          := INI.ReadInteger('GENERAL','SHOWHOTSPOTS',0);
  ProgramSettings.ClearType             := INI.ReadBool('GENERAL','CLEARTYPE',False);
  ProgramSettings.ClearTypeRGB          := INI.ReadBool('GENERAL','CLEARTYPERGB',True);
  ProgramSettings.ClearTypeInactive     := INI.ReadString('GENERAL','CLEARTYPEINACTIVEFONT','Microsoft Sans Serif');
  ProgramSettings.ClearTypeActive       := INI.ReadString('GENERAL','CLEARTYPEACTIVEFONT','Arial');
  ProgramSettings.ClearTypeInactiveSize := INI.ReadInteger('GENERAL','CLEARTYPEINACTIVEFONTSIZE',8);
  ProgramSettings.ClearTypeActiveSize   := INI.ReadInteger('GENERAL','CLEARTYPEACTIVEFONTSIZE',8);
  ProgramSettings.WLDCreatureSize       := StrToFloat(INI.ReadString('GENERAL','WLDCREATURESIZE','0.1'));
  ProgramSettings.XWFCreatureSize       := StrToFloat(INI.ReadString('GENERAL','XWFCREATURESIZE','0.15'));
  ProgramSettings.SmallMoveAmount       := StrToFloat(INI.ReadString('GENERAL','SMALLMOVEAMOUNT','0.1'));
  ProgramSettings.SmallRotateAmount     := StrToFloat(INI.ReadString('GENERAL','SMALLROTATEAMOUNT','0.1'));
  cbAnimateTextures.Checked             := ProgramSettings.AnimateTextures;
  glView.ShowDiapasones                 := ProgramSettings.ShowZoneDimensions;
  cbShowLightSources.ItemIndex          := ProgramSettings.ShowLightSources;
  cbShowModelOrigins.ItemIndex          := ProgramSettings.ShowModelOrigins;
  cbShowHotSpots.ItemIndex              := ProgramSettings.ShowHotSpots;
  Case ProgramSettings.MenuIconSize Of
    24: acMenuIconSize24.Execute;
    32: acMenuIconSize32.Execute;
    64: acMenuIconSize64.Execute;
  Else
    acMenuIconSize16.Execute;
  End; // Case
  Case ProgramSettings.ToolbarIconSize Of
    24: acToolbarIconSize24.Execute;
    32: acToolbarIconSize32.Execute;
    64: acToolbarIconSize64.Execute;
  Else
    acToolbarIconSize16.Execute;
  End; // Case
  INI.Free;
End; // LoadPreferences

Procedure TfrmMain.SavePreferences;
Var INI: TINIFile;
Begin
  INI := TINIFile.Create(ExtractFilePath(Application.ExeName) + INIFileName);
  INI.EraseSection('GENERAL');
  INI.WriteString('GENERAL','EQDIR',ProgramSettings.EQDir);
  INI.WriteInteger('GENERAL','MENUICONSIZE',ProgramSettings.MenuIconSize);
  INI.WriteInteger('GENERAL','TOOLBARICONSIZE',ProgramSettings.ToolbarIconSize);
  INI.WriteBool('GENERAL','USENUMERICKEYPAD',ProgramSettings.UseNumericKeypad);
  INI.WriteBool('GENERAL','SHOWZONEDIMENSIONS',ProgramSettings.ShowZoneDimensions);
  INI.WriteBool('GENERAL','ANIMATETEXTURES',ProgramSettings.AnimateTextures);
  INI.WriteInteger('GENERAL','SHOWLIGHTSOURCES',ProgramSettings.ShowLightSources);
  INI.WriteInteger('GENERAL','SHOWMODELORIGINS',ProgramSettings.ShowModelOrigins);
  INI.WriteInteger('GENERAL','SHOWHOTSPOTS',ProgramSettings.ShowHotSpots);
  INI.WriteBool('GENERAL','CLEARTYPE',ProgramSettings.ClearType);
  INI.WriteBool('GENERAL','CLEARTYPERGB',ProgramSettings.ClearTypeRGB);
  INI.WriteString('GENERAL','CLEARTYPEINACTIVEFONT',ProgramSettings.ClearTypeInactive);
  INI.WriteString('GENERAL','CLEARTYPEACTIVEFONT',ProgramSettings.ClearTypeActive);
  INI.WriteInteger('GENERAL','CLEARTYPEINACTIVEFONTSIZE',ProgramSettings.ClearTypeInactiveSize);
  INI.WriteInteger('GENERAL','CLEARTYPEACTIVEFONTSIZE',ProgramSettings.ClearTypeActiveSize);
  INI.WriteString('GENERAL','WLDCREATURESIZE',FloatToStr(ProgramSettings.WLDCreatureSize));
  INI.WriteString('GENERAL','XWFCREATURESIZE',FloatToStr(ProgramSettings.XWFCreatureSize));
  INI.WriteString('GENERAL','SMALLMOVEAMOUNT',FloatToStr(ProgramSettings.SmallMoveAmount));
  INI.WriteString('GENERAL','SMALLROTATEAMOUNT',FloatToStr(ProgramSettings.SmallRotateAmount));
  INI.Free;
End; // SavePreferences

Procedure TfrmMain.SetClearTypeSettings;
Begin
  DataModule1.ClearTypeText.Enabled           := ProgramSettings.ClearType;
  DataModule1.ClearTypeText.InactiveFont.Name := ProgramSettings.ClearTypeInactive;
  DataModule1.ClearTypeText.ActiveFont.Name   := ProgramSettings.ClearTypeActive;
  DataModule1.ClearTypeText.InactiveFont.Size := ProgramSettings.ClearTypeInactiveSize;
  DataModule1.ClearTypeText.ActiveFont.Size   := ProgramSettings.ClearTypeActiveSize;
  If ProgramSettings.ClearTypeRGB
   Then DataModule1.ClearTypeText.Flavor := ctfRGB
   Else DataModule1.ClearTypeText.Flavor := ctfBGR;
End; // TfrmMain.SetClearTypeSettings

Procedure TfrmMain.LoadDefaultShaders;
Var VertexShader,FragmentShader: String;

  Function LoadShaderText(FileName: String): String;
  Var
    St   : String;
    List : TStringList;

  Begin
    St := '';
    If FileExists(FileName) Then
    Begin
      List := TStringList.Create;
      List.LoadFromFile(FileName);
      St := List.Text;
      List.Free;
    End;
    Result := St;
  End; // LoadShaderText

Begin
  glView.Scene3D.Scene.ShaderManager.Enabled := cbEnableShaders.Checked;

  VertexShader := LoadShaderText(ExtractFilePath(Application.EXEName) + 'library\shaders\vertex\vertex.txt');
  If VertexShader <> '' Then
  Begin
    glView.Scene3D.Scene.ShaderManager.AddShader(stVertex,'vertex',VertexShader);
    VertexShader := 'vertex';
  End;

  FragmentShader := '';

{
  FragmentShader := LoadShaderText(ExtractFilePath(Application.EXEName) + 'library\shaders\fragment\normal.txt');
  If FragmentShader <> '' Then
  Begin
    glView.Scene3D.Scene.ShaderManager.AddShader(stFragment,'normal',FragmentShader);
    FragmentShader := 'normal';
  End;
}
  glView.Scene3D.Scene.ShaderManager.RegisterProgram(VertexShader,FragmentShader);
  glView.Scene3D.Scene.ShaderManager.SetDefaultProgram(VertexShader,FragmentShader);
 
End; // TfrmMain.LoadDefaultShaders

procedure TfrmMain.FormCreate(Sender: TObject);
Var I: Integer;
begin
  // Initialize DevIL.

  ilInit();

  // Turn off mipmapping and VBO usage

  UseMipmapping                 := False;
  UseVBOExtension               := False;

  // Change the far clipping plane

  glView.Scene3D.Scene.DistFar  := 10000;
  glvMesh.Scene3D.Scene.DistFar := 10000;

//  glView.Init;
//  glvMesh.Init;
{
  glView.Scene3D.Scene.InitDC;
  glvMesh.Scene3D.Scene.InitDC;

  glView.Scene3D.Scene.InitRC;
  glvMesh.Scene3D.Scene.InitRC;

  glView.Scene3D.Init;
  glvMesh.Scene3D.Init;
}
//  glView.Update;
//  glvMesh.Update;

  LoadPreferences;

{
  If ProgramSettings.ClearType Then
  Begin
    ZPropList1.Font.Name := 'Arial';
    ZPropList1.Font.Size := 8;
    tvMeshes.Font.Name   := 'Arial';
    tvMeshes.Font.Size   := 8;
  End
  Else
  Begin
    ZPropList1.Font.Name := 'Microsoft Sans Serif';
    ZPropList1.Font.Size := 8;
    tvMeshes.Font.Name   := 'Microsoft Sans Serif';
    tvMeshes.Font.Size   := 8;
  End;
}
  GenerateStarField; // Do this before the Randomize call

  Randomize;
  LoadingMeshLibraryList     := False;
  TimeOfDay.Hour             := tbTimeOfDay.Position Div 60;
  TimeOfDay.Minute           := tbTimeOfDay.Position Mod 60;
  TimeOfDay.Day              := 20; // (shrug) just pick anything...we'll do the vernal equinox for a happy medium
  TimeOfDay.Month            := 2;  // 0 is January
  TimeOfDay.Year             := 1000;
  UpdatingFromRender         := False;
  UpdatingFromSlider         := False;
  LastTickCount              := 0;
  CurrTickCount              := 0;
//  DecimalSeparator           := '.';
  SelectedPolygons           := TList.Create;
  SelectedPolyMesh           := Nil;
  ClickPoly                  := Nil;
  ClickPolyIndex             := -1;
  SelectedMesh               := '';
  SelectedCreature           := '';
  CrosshairX                 := -1;
  CrosshairY                 := -1;
  cbCrosshair.ItemIndex      := 0;
  Crosshair                  := crNone;
  SetHeight                  := 0;
  SelectedHotSpot            := Nil;
  SelectedHotSpotML          := Nil;
  AllowHotSpotChange         := True;
  SettingSetHeightEdit       := False;
  ObjLoc                     := T3DPoint.Create;
  ObjRotate                  := THeading.Create;
  ObjSize                    := T3DPoint.Create;
  HighestPoint               := T3DPoint.Create;
  HPV1                       := T3DPoint.Create;
  HPV2                       := T3DPoint.Create;
  HPV3                       := T3DPoint.Create;
  LastNode                   := Nil;
  DraggedNode                := Nil;
  UpdatingCBProps            := False;
  Clipboard                  := TStringList.Create;
  Tree                       := Nil;
  SettingTexSet              := False;
  OldRow                     := -1;
  MasterTexList              := TSharedComboBoxStrings.Create;
  MasterTexList.Sorted       := True;
  Zone                       := TZone.Create;
  BirdsEye                   := True;
  glView.Scene3D.Scene.CheckHidden := Not BirdsEye;
  glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
  glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;
  Observer                   := T3DPoint.Create(0,0,0);
  Theta                      := Pi / 2;
  Phi                        := 0;
  dlgOpen.InitialDir         := ExtractFilePath(Application.ExeName) + 'SCENES';
  dlgSave.InitialDir         := ExtractFilePath(Application.ExeName) + 'ZONES';
  MasterMeshList             := Nil;
  ClickIndex                 := -1;
  LeftDown                   := False;
  ClickIntersect             := T3DPoint.Create;
  ClickObs                   := T3DPoint.Create;
  ClickDest                  := T3DPoint.Create;

  // Load the texture set list

  cbTextureSet.Clear;
  cbTextureSet.AddItem('(none)',Nil);
  For I := 0 To TextureLibraries.Count - 1 Do cbTextureSet.Items.Add(TextureLibraries.Strings[I]);
  cbTextureSet.ItemIndex := 0;
  MasterTree      := Nil;
  MasterTreeMesh  := Nil;
  MasterTreeMinPt := Nil;
  MasterTreeMaxPt := Nil;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
Var I: Integer;
begin
  ZPropList1.CurObj := Nil;     // This and the next line are important
  Application.ProcessMessages;
  glView.Scene3D.Scene.ClearScene(True,True);
  For I := 0 To Clipboard.Count - 1 Do Clipboard.Objects[I].Free;
  Clipboard.Free;
  SetLength(ParameterNames,0);
  SetLength(ParameterValues,0);
  MasterTexList.Free;
  Zone.Free;
  ObjLoc.Free;
  ObjRotate.Free;
  ObjSize.Free;
  Tree.Free;
  HighestPoint.Free;
  HPV1.Free;
  HPV2.Free;
  HPV3.Free;
  ClickIntersect.Free;
  Clickobs.Free;
  ClickDest.Free;
  SelectedPolygons.Free;
  If MasterTree      <> Nil Then MasterTree.Free;
  If MasterTreeMesh  <> Nil Then MasterTreeMesh.Free;
  If MasterTreeMinPt <> Nil Then MasterTreeMinPt.Free;
  If MasterTreeMaxPt <> Nil Then MasterTreeMaxPt.Free;
  FreeMasterMeshList;
  Observer.Free;
end;

Procedure TfrmMain.FreeMasterMeshList;
Var I: Integer;
Begin
  If MasterMeshList <> Nil Then
  Begin
    For I := 0 To MasterMeshList.Count - 1 Do MasterMeshList.Objects[I].Free;
    MasterMeshList.Free;
  End;
End; // TfrmMain.FreeMasterMeshList

Procedure TfrmMain.ReloadZone;
Var B: Boolean;
Begin
  B := tmrTimeOfDay.Enabled;
  tmrTimeOfDay.Enabled := False;
  glView.Scene3D.Scene.DefaultCamera.LookAt(0,0,-100);
  glView.Scene3D.Scene.DefaultCamera.SetVectorUp(1,0,0);
  glView.Scene3D.Scene.DefaultCamera.SetPosition(0,0,8{00});
  FreeMasterMeshList;
  MasterMeshList := Zone.BuildPolygonLists;
  ReloadModel(MasterMeshList,False);
  frmStatus.Show;
  If MasterTreeMesh  <> Nil Then MasterTreeMesh.Free;
  If MasterTree      <> Nil Then MasterTree.Free;
  If MasterTreeMinPt <> Nil Then MasterTreeMinPt.Free;
  If MasterTreeMaxPt <> Nil Then MasterTreeMaxPt.Free;
  MasterTreeMesh := Zone.BuildPolygonList(True,False,True,False);
  frmStatus.SetCaption('Building BSP tree');
  MasterTree     := TTree.Create(MasterTreeMesh,False);
  MasterTree.SplitAlongGrid(1024,1024 * 3);
  MasterTreeMinPt := T3DPoint.Create;
  MasterTreeMaxPt := T3DPoint.Create;
  MasterTree.Root.Mesh.GetBounds(MasterTreeMinPt,MasterTreeMaxPt);
  frmStatus.Hide;
  tmrTimeOfDay.Enabled := B;
End; // TfrmMain.ReloadZone

Procedure TfrmMain.SetViewToNorth;
Begin
  If BirdsEye Then
  Begin
    glView.Scene3D.Scene.DefaultCamera.SetPosition(0,0,8{00});
    glView.Scene3D.Scene.DefaultCamera.LookAt(0,0,-100);
    glView.Scene3D.Scene.DefaultCamera.SetVectorUp(1,0,0);
  End;
End; // TfrmMain.SetViewToNorth

Procedure TfrmMain.LoadScene(FileName: String);
Begin
  ZPropList1.CurObj := Nil;
  Application.ProcessMessages;
  tvMain.ClearSelection;
  Application.ProcessMessages;
  glView.Scene3D.Scene.ClearScene(True,True);
  LastLoadedScene   := FileName;
  BirdsEye          := True;
  glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
  glView.Scene3D.Scene.CheckHidden := Not BirdsEye;
  glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;  
  glView.AllowMouse := BirdsEye;
  Zone.Free;
  Zone := TZone.Create;
  frmStatus.Show;
  frmStatus.SetCaption('Loading scene...');
  frmStatus.SetPosition(0);
  Zone.LoadFromFile(FileName);

  SelectedPolygons.Clear;
  SelectedPolyMesh  := Nil;
  SelectedHotSpot   := Nil;
  SelectedHotSpotML := Nil;

  Observer.Free;
  Observer       := T3DPoint.Create((Zone.ElevationGrid.MinX + Zone.ElevationGrid.MaxX) / 2,
                                    (Zone.ElevationGrid.MinY + Zone.ElevationGrid.MaxY) / 2,
                                    (Zone.BoundMinZ          + Zone.BoundMaxZ) / 2);

  // Get a polygon list from the zone

  LoadObjectTreeView;
  ReloadZone;

  // Render the zone

  frmStatus.Hide;
  glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
  glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;
  RenderZone(BirdsEye);
  glView.Fit(Not BirdsEye,True,False);
  SetViewToNorth;
  UpdateTitleBar;
  glView.Invalidate;
End; // TfrmMain.LoadScene

Procedure TfrmMain.SaveScene(FileName: String);
Begin
  LastLoadedScene := FileName;
  Zone.SaveToFile(FileName,Nil);
  UpdateTitleBar;
End; // TfrmMain.SaveScene

Procedure TfrmMain.SaveSelectedScene(FileName: String);
Var
  I  : Integer;
  TN : TTreeNode;
  ZO : TZoneObject;
  L  : TStringList;

Begin
  L := TStringList.Create;
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    TN := tvMain.Selections[I];
    If TN <> Nil Then
    Begin
      ZO := TZoneObject(TN.Data);
      If (Zone.GetObjectIndex(ZO) >= 0) And (ZO.GetParent = Nil) Then L.AddObject(ZO.GetName,ZO);
    End;
  End; // For I
  Zone.SaveToFile(FileName,L);
  L.Free;
End; // TfrmMain.SaveSelectedScene

Procedure TfrmMain.CreateNewZone;
Type
  PByteArray = ^TByteArray;
  TBytearray = Packed Array[0..65535] Of Byte;
  PWordArray = ^TWordArray;
  TWordarray = Packed Array[0..65535] Of Word;
  TVertexAliasArray = Array Of Integer;

Var
  poNS,poEW,poUD : LongInt;
  szNS,szEW,szUD : LongInt;
  MO,MO1         : TMeshObject;
  BMP            : TBitmap;
  X,Y,Y1,Y2,X2   : Integer;
  I              : Integer;
  X0,Y0,Z0       : Double;
  SX,SY          : Integer;
  SZ             : Double;
  NSX,NSY        : Integer;
  SX1,SY1        : Integer;
  P1,P2,P3,P4    : Integer;
  PixScale       : Integer;
  Continue       : Boolean;
  St             : String;
  St1            : PChar;
  P              : TPolygon;
  Found          : Boolean;
  V              : T3DPoint;
  BMPHeader      : Pointer;
  BMPHeaderSize  : Cardinal;
  BMPBits        : Pointer;
  BMPBitsSize    : Cardinal;
  VAlias         : TVertexAliasArray;
  Tree           : TTree;

  Function LoadVal(Var Num: LongInt; Name,St: String; Min,Max: LongInt): Boolean;
  Var I,J: LongInt;
  Begin
    Val(St,I,J);
    If (J <> 0) Or (I < Min) Or (I > Max) Then
    Begin
      ShowMessage('Invalid ' + Name + '.'#13#10 +
                  'Enter a value from ' + IntToStr(Min) +
                  ' to ' + IntToStr(Max) + '.');
      Result := False;
    End
    Else
    Begin
      Num    := I;
      Result := True;
    End;
  End; // LoadVal

  Procedure MakeBoundingBox;
  Var
    P     : TPolygon;
    I,J   : Integer;
    Inset : Integer;
    Depth : Integer;
    MO    : TMeshObject;

  Begin
    MO := TMeshObject.Create(meshBoundingBox);
    I  := 0;
    Val(frmNewZone.edtInset.Text,Inset,J);
    If J <> 0 Then Inset := 100;
    Val(frmNewZone.edtDepth.Text,Depth,J);
    If J <> 0 Then Depth := 2500;

    // East edge

    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW - (szEW / 2) + Inset,poUD - Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW - (szEW / 2) + Inset,poUD + Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW + (szEW / 2) - Inset,poUD + Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW + (szEW / 2) - Inset,poUD - Depth));

    P := TPolygon.Create([I,I + 1,I + 2,I + 3],frmNewZone.cbTexture.Text);
    P.TextureState := tsTransparent;
    MO.Polygons.AddObject('',P);
    Inc(I,4);

    // South edge

    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW + (szEW / 2) - Inset,poUD - Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW + (szEW / 2) - Inset,poUD + Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW + (szEW / 2) - Inset,poUD + Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW + (szEW / 2) - Inset,poUD - Depth));

    P := TPolygon.Create([I,I + 1,I + 2,I + 3],frmNewZone.cbTexture.Text);
    P.TextureState := tsTransparent;
    MO.Polygons.AddObject('',P);
    Inc(I,4);

    // West edge

    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW + (szEW / 2) - Inset,poUD - Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW + (szEW / 2) - Inset,poUD + Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW - (szEW / 2) + Inset,poUD + Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW - (szEW / 2) + Inset,poUD - Depth));

    P := TPolygon.Create([I,I + 1,I + 2,I + 3],frmNewZone.cbTexture.Text);
    P.TextureState := tsTransparent;
    MO.Polygons.AddObject('',P);
    Inc(I,4);

    // North edge

    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW - (szEW / 2) + Inset,poUD - Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW - (szEW / 2) + Inset,poUD + Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW - (szEW / 2) + Inset,poUD + Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW - (szEW / 2) + Inset,poUD - Depth));

    P := TPolygon.Create([I,I + 1,I + 2,I + 3],frmNewZone.cbTexture.Text);
    P.TextureState := tsTransparent;
    MO.Polygons.AddObject('',P);
    Inc(I,4);

    // Top edge

    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW - (szEW / 2) + Inset,poUD + Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW - (szEW / 2) + Inset,poUD + Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW + (szEW / 2) + Inset,poUD + Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW + (szEW / 2) - Inset,poUD + Depth));

    P := TPolygon.Create([I,I + 1,I + 2,I + 3],frmNewZone.cbTexture.Text);
    P.TextureState := tsTransparent;
    MO.Polygons.AddObject('',P);
    Inc(I,4);

    // Bottom edge

    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW - (szEW / 2) + Inset,poUD - Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS - (szNS / 2) + Inset,poEW + (szEW / 2) - Inset,poUD - Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW + (szEW / 2) - Inset,poUD - Depth));
    MO.Vertices.AddObject('',T3DPoint.Create(poNS + (szNS / 2) - Inset,poEW - (szEW / 2) + Inset,poUD - Depth));

    P := TPolygon.Create([I,I + 1,I + 2,I + 3],frmNewZone.cbTexture.Text);
    P.TextureState := tsTransparent;
    MO.CalcNormals;
    MO.Polygons.AddObject('',P);
    Zone.AddObject(meshBoundingBox,MO);
  End; // MakeBoundingBox

  Procedure LoadTerragenHeightmap(FileName: String; BMP: TBitmap);
  Type
    TTerragenSCALChunk = Packed Record
      X,Y,Z: Single;
    End;
    TTerragenCRADChunk = Packed Record
      Radius: Single;
    End;
    TTerragenCRVMChunk = Packed Record
      Mode: LongWord;
    End;
    TTerragenALTWChunk = Packed Record
      HeightScale : SmallInt;
      BaseHeight  : SmallInt;
    End;
    TTerragenSIZEChunk = Packed Record
      ShortLen : Word;
      Padding  : Word;
    End;
    TTerragenXPTSChunk = Packed Record
      XPts    : Word;
      Padding : Word;
    End;
    TTerragenYPTSChunk = Packed Record
      YPts    : Word;
      Padding : Word;
    End;
    TTerragenHeader = Packed Record
      stTerragen : Packed Array[0..7] Of Char;
      stTerrain  : Packed Array[0..7] Of Char;
    End;

  Var
    Stream      : TFileStream;
    St          : ShortString;
    Header      : TTerragenHeader;
    Ok          : Boolean;
    Eof         : Boolean;
    SIZE        : TTerragenSIZEChunk;
    XPTS        : TTerragenXPTSChunk;
    YPTS        : TTerragenYPTSChunk;
    SCAL        : TTerragenSCALChunk;
    CRAD        : TTerragenCRADChunk;
    CRVM        : TTerragenCRVMChunk;
    ALTW        : TTerragenALTWChunk;
    Elevations  : Packed Array Of SmallInt;
    ALTWPadding : Word;
    X,Y,I       : Integer;
    P           : ^Word;

    Procedure ReadChunkName;
    Begin
      St := '1234';
      Stream.ReadBuffer(St[1],4);
    End; // ReadChunkName

  Begin
    Stream := TFileStream.Create(FileName,fmOpenRead);
    If (Stream.Size - Stream.Position) >= SizeOf(Header) Then
    Begin
      Stream.ReadBuffer(Header,SizeOf(Header));
      St := '12345678';
      Move(Header.stTerragen,St[1],8);
      If St = 'TERRAGEN' Then
      Begin
        Move(Header.stTerrain,St[1],8);
        If St = 'TERRAIN ' Then
        Begin
          Ok  := True;
          Eof := False;
          SetLength(Elevations,0);
          While Ok And Not Eof Do
          Begin
            If (Stream.Size - Stream.Position) >= 4 Then
            Begin
              ReadChunkName;
              If St = 'SIZE' Then
              Begin
                If (Stream.Size - Stream.Position) >= SizeOf(SIZE) Then
                Begin
                  Stream.ReadBuffer(SIZE,SizeOf(SIZE));
                  XPTS.XPts := SIZE.ShortLen + 1;
                  YPTS.YPts := SIZE.ShortLen + 1;
                End
                Else Ok := False;
              End
              Else If St = 'XPTS' Then
              Begin
                If (Stream.Size - Stream.Position) >= SizeOf(XPTS) Then
                Begin
                  Stream.ReadBuffer(XPTS,SizeOf(XPTS));
                End
                Else Ok := False;
              End
              Else If St = 'YPTS' Then
              Begin
                If (Stream.Size - Stream.Position) >= SizeOf(YPTS) Then
                Begin
                  Stream.ReadBuffer(YPTS,SizeOf(YPTS));
                End
                Else Ok := False;
              End
              Else If St = 'SCAL' Then
              Begin
                If (Stream.Size - Stream.Position) >= SizeOf(SCAL) Then
                Begin
                  Stream.ReadBuffer(SCAL,SizeOf(SCAL));
                End
                Else Ok := False;
              End
              Else If St = 'CRAD' Then
              Begin
                If (Stream.Size - Stream.Position) >= SizeOf(CRAD) Then
                Begin
                  Stream.ReadBuffer(CRAD,SizeOf(CRAD));
                End
                Else Ok := False;
              End
              Else If St = 'CRVM' Then
              Begin
                If (Stream.Size - Stream.Position) >= SizeOf(CRVM) Then
                Begin
                  Stream.ReadBuffer(CRVM,SizeOf(CRVM));
                End
                Else Ok := False;
              End
              Else If St = 'ALTW' Then
              Begin
                If (Stream.Size - Stream.Position) >= SizeOf(ALTW) Then
                Begin
                  Stream.ReadBuffer(ALTW,SizeOf(ALTW));
                  SetLength(Elevations,XPTS.XPts * YPTS.YPts);
                  If (Stream.Size - Stream.Position) >= XPTS.XPts * YPTS.YPts * 2 + 2 Then
                  Begin
                    Stream.ReadBuffer(Elevations[0],XPTS.XPts * YPTS.YPts * 2);
                    Stream.ReadBuffer(ALTWPadding,2);
                  End
                  Else Ok := False;
                End
                Else Ok := False;
              End
              Else If St = 'EOF ' Then Eof := True
              Else Ok := False;
            End;
          End; // While
          If Ok Then
          Begin
            BMP.Width       := XPTS.XPts;
            BMP.Height      := YPTS.YPts;
            BMP.PixelFormat := pf16Bit;
            For Y := 0 To YPTS.YPts - 1 Do
            Begin
              P := BMP.ScanLine[(YPTS.YPts - 1) - Y];
              For X := 0 To XPTS.XPts - 1 Do
              Begin
                I := Elevations[Y * XPTS.XPts + X];
                Inc(I,32768);
                P^ := Word(I);
                Inc(LongInt(P),2);
              End; // For X
            End; // For Y
          End
          Else
          Begin
            BMP.Width  := 32;
            BMP.Height := 32;
          End;
          SetLength(Elevations,0);
        End;
      End;
    End;
    Stream.Free;
  End; // LoadTerragenHeightmap

begin
  If Application.MessageBox('The current zone will be lost!'#13#10 +
                            'Are you sure?',
                            'Warning',
                            MB_OKCANCEL) = IDOK Then
  Begin
    If frmNewZone.ShowModal = mrOk Then
    Begin
      // Based on what was selected, create a starting point

      If frmNewZone.rbOutside.Checked Then
      Begin
        If Not LoadVal(poNS,'north-south position',frmNewZone.edtNorthSouthPosition.Text,-32768,32767) Then Exit;
        If Not LoadVal(poEW,'east-west position',  frmNewZone.edtEastWestPosition.Text,  -32768,32767) Then Exit;
        If Not LoadVal(poUD,'up-down position',    frmNewZone.edtUpDownPosition.Text,    -32508,32500) Then Exit;
        If Not LoadVal(szNS,'north-south size',    frmNewZone.edtNorthSouthSize.Text,    -32768,32767) Then Exit;
        If Not LoadVal(szEW,'east-west size',      frmNewZone.edtEastWestSize.Text,      -32768,32767) Then Exit;
        If Not LoadVal(szUD,'up-down size',        frmNewZone.edtUpDownSize.Text,        -25500,25500) Then Exit;

        If poNS + (szNS Div 2) > 32767 Then
        Begin
          ShowMessage('North edge cannot proceed past 32767');
          Exit;
        End;
        If poNS - (szNS Div 2) < -32768 Then
        Begin
          ShowMessage('South edge cannot proceed past -32768');
          Exit;
        End;
        If poEW + (szEW Div 2) > 32767 Then
        Begin
          ShowMessage('West edge cannot proceed past 32767');
          Exit;
        End;
        If poEW - (szEW Div 2) < -32768 Then
        Begin
          ShowMessage('East edge cannot proceed past -32768');
          Exit;
        End;
        If poUD + (szUD Div 2) > 32767 Then
        Begin
          ShowMessage('Top edge cannot proceed past 32767');
          Exit;
        End;
        If poUD - (szUD Div 2) < -32768 Then
        Begin
          ShowMessage('Bottom edge cannot proceed past -32768');
          Exit;
        End;

        // Are we generating the ground from a heightmap?

        If frmNewZone.cbUseHeightmap.Checked Then
        Begin
          dlgOpen.FileName   := '';
          dlgOpen.InitialDir := ExtractFilePath(Application.ExeName) + 'HEIGHTMAPS';
          dlgOpen.Filter     := 'Bitmap heightmaps (*.bmp;*.tga)|*.BMP;*.TGA|Terragen heightmaps (*.ter)|*.TER';
          If dlgOpen.Execute And FileExists(dlgOpen.FileName) Then
          Begin
            BMP := TBitmap.Create;
                 If UpperCase(ExtractFileExt(dlgOpen.FileName)) = '.BMP' Then BMP.LoadFromFile(dlgOpen.FileName)
            Else If UpperCase(ExtractFileExt(dlgOpen.FileName)) = '.TGA' Then LoadFromFileX(dlgOpen.FileName,BMP,False,cbNoConversion)
            Else LoadTerragenHeightmap(dlgOpen.FileName,BMP);
            If BMP.Width * BMP.Height > 64 * 64 Then
            Begin
              St := 'This image is ' + IntToStr(BMP.Width) + ' x ' + IntToStr(BMP.Height) + ' pixels in size!'#13#10#13#10 +
                    'Using anything larger than 64 x 64 can take a HORRENDOUSLY long time'#13#10 +
                    'and might cause you to run out of memory.  Using large heightmaps'#13#10 +
                    'is NOT recommended.'#13#10#13#10 +
                    'Are you SURE you want to do this?';
              St1 := StrAlloc(Length(St) + 1);
              StrPCopy(St1,St);
              Continue := (Application.MessageBox(St1,'WARNING!',MB_OKCANCEL) = IDOK);
              StrDispose(St1);
            End
            Else Continue := True;
            If Continue Then
            Begin
              Zone.Free;
              Zone := TZone.Create;
              Zone.SetDefaultLandTexture(frmNewZone.cbTexture.Text);
              Zone.SetDefaultUnderwaterTexture(frmNewZone.cbBelowTexture.Text);
              Case BMP.PixelFormat Of
                 pf8Bit,                     // Assume grayscale
                pf24Bit,
                pf32Bit: PixScale := 255;
                pf15Bit: PixScale := 32767;
                pf16Bit: PixScale := 65535;
              Else
                BMP.Free;
                ShowMessage('Image must be 8, 16, 24, or 32-bit.');
                Exit;
              End; // Case
              If (BMP.Width > 1) And (BMP.Height > 1) Then
              Begin
                SX := GridSize;
                SY := GridSize;
                SZ := szUD / PixScale;

                // Round the size up to a texture boundary (this is vital, or the texture picker will fail)

                If (szNS Div SX) * SX < szNS Then szNS := ((szNS Div SX) + 1) * SX;
                If (szEW Div SY) * SY < szEW Then szEW := ((szEW Div SY) + 1) * SY;

                MO := TMeshObject.Create(meshHeightMapGround);
                X0 := poNS + szNS / 2;
                Y0 := poEW + szEW / 2;
                Z0 := poUD - szUD / 2;

                // Assume north is at the top of the bitmap

                NSY := szNS Div SX;
                NSX := szEW Div SY;
                If NSY * SX < szNS Then Inc(NSY);
                If NSX * SY < szEW Then Inc(NSX);

                Zone.ElevationGrid.MinX := poNS - szNS / 2;
                Zone.ElevationGrid.MinY := poEW - szEW / 2;
                Zone.ElevationGrid.MinZ := poUD - szUD / 2;
                Zone.ElevationGrid.MaxX := poNS + szNS / 2;
                Zone.ElevationGrid.MaxY := poEW + szEW / 2;
                Zone.ElevationGrid.MaxZ := poUD + szUD / 2;
                Zone.ElevationGrid.NX   := NSX + 1;
                Zone.ElevationGrid.NY   := NSY + 1;
                SetLength(Zone.ElevationGrid.Heights,(NSX + 1) * (NSY + 1));
                SetLength(Zone.ElevationGrid.Visible,NSX * NSY);
                For I := 0 To High(Zone.ElevationGrid.Visible) Do Zone.ElevationGrid.Visible[I] := True;

                I := 0;
                For SY1 := 0 To NSY - 1 Do
                Begin
                  Y  := Round(SY1       * ((BMP.Height - 1) / NSY));
                  Y2 := Round((SY1 + 1) * ((BMP.Height - 1) / NSY));

                  // Some Windows .BMP files are internally flipped vertically

                  Y1 := Y;
                  If UpperCase(ExtractFileExt(dlgOpen.FileName)) = '.BMP' Then
                  Begin
                    GetDIBSizes(BMP.Handle,BMPHeaderSize,BMPBitsSize);
                    GetMem(BMPHeader,BMPHeaderSize);
                    GetMem(BMPBits,BMPBitsSize);
                    GetDIB(BMP.Handle,BMP.Palette,BMPHeader^,BMPBits^);
                    If BitMapInfo(BMPHeader^).bmiHeader.biHeight < 0 Then
                    Begin
                      Y1 := (BMP.Height - 1) - Y1;
                      Y2 := (BMP.Height - 1) - Y2;
                    End;
                    FreeMem(BMPHeader,BMPHeaderSize);
                    FreeMem(BMPBits,BMPBitsSize);
                  End;

                  For SX1 := 0 To NSX - 1 Do
                  Begin
                    X  := Round(SX1       * ((BMP.Width - 1) / NSX));
                    X2 := Round((SX1 + 1) * ((BMP.Width - 1) / NSX));

                    Case BMP.PixelFormat Of
                      pf8Bit:
                      Begin
                        P1 := PByteArray(BMP.ScanLine[Y1])^[X];
                        P2 := PByteArray(BMP.ScanLine[Y1])^[X2];
                        P3 := PByteArray(BMP.ScanLine[Y2])^[X];
                        P4 := PByteArray(BMP.ScanLine[Y2])^[X2];
                      End;
                      pf15Bit,
                      pf16Bit:
                      Begin
                        P1 := PWordArray(BMP.ScanLine[Y1])^[X];
                        P2 := PWordArray(BMP.ScanLine[Y1])^[X2];
                        P3 := PWordArray(BMP.ScanLine[Y2])^[X];
                        P4 := PWordArray(BMP.ScanLine[Y2])^[X2];
                      End;
                      pf24Bit:
                      Begin
                        P1 := PByteArray(BMP.ScanLine[Y1])^[X  * 3];              // Assume grayscale
                        P2 := PByteArray(BMP.ScanLine[Y1])^[X2 * 3];
                        P3 := PByteArray(BMP.ScanLine[Y2])^[X  * 3];
                        P4 := PByteArray(BMP.ScanLine[Y2])^[X2 * 3];
                      End;
                      pf32Bit:
                      Begin
                        P1 := PByteArray(BMP.ScanLine[Y1])^[X  * 4];              // Assume grayscale
                        P2 := PByteArray(BMP.ScanLine[Y1])^[X2 * 4];
                        P3 := PByteArray(BMP.ScanLine[Y2])^[X  * 4];
                        P4 := PByteArray(BMP.ScanLine[Y2])^[X2 * 4];
                      End;
                    Else
                      P1 := 0; // Make the compiler happy
                      P2 := 0;
                      P3 := 0;
                      P4 := 0;
                    End; // Case

                    Zone.ElevationGrid.SetHeight(SX1,SY1,Z0 + P1 * SZ);
                    If SX1 = NSX - 1 Then Zone.ElevationGrid.SetHeight(SX1 + 1,SY1,Z0 + P2 * SZ);
                    If SY1 = NSY - 1 Then
                    Begin
                      Zone.ElevationGrid.SetHeight(SX1,SY1 + 1,Z0 + P3 * SZ);
                      If SX1 = NSX - 1 Then Zone.ElevationGrid.SetHeight(SX1 + 1,SY1 + 1,Z0 + P4 * SZ);
                    End;

                    // Start with triangles to save us a step later

                    MO.Vertices.AddObject('',T3DPoint.Create(X0 - SY1 * SX,       Y0 - SX1 * SY,       Z0 + P1 * SZ));
                    MO.Vertices.AddObject('',T3DPoint.Create(X0 - SY1 * SX,       Y0 - (SX1 + 1) * SY, Z0 + P2 * SZ));
                    MO.Vertices.AddObject('',T3DPoint.Create(X0 - (SY1 + 1) * SX, Y0 - (SX1 + 1) * SY, Z0 + P4 * SZ));
                    P := TPolygon.Create([I,I + 1,I + 2],frmNewZone.cbTexture.Text);
                    P.HasTexCoords := True;
                    SetLength(P.TX,3);
                    SetLength(P.TZ,3);
                    P.TX[0] := SX1 + 1/TextureSize;
                    P.TX[1] := (SX1 + 1) - 1/TextureSize;
                    P.TX[2] := P.TX[1];
                    P.TZ[0] := SY1 + 1/TextureSize;
                    P.TZ[1] := P.TZ[0];
                    P.TZ[2] := (SY1 + 1) - 1/TextureSize;

                    // We're going to use Tag to contain the grid position.  It will be
                    // automatically copied if the polygon is broken up.

                    P.Tag    := SY1 * NSX + SX1;
                    P.HasTag := True;
                    MO.Polygons.AddObject('',P);
                    Inc(I,3);

                    MO.Vertices.AddObject('',T3DPoint.Create(X0 - SY1 * SX,       Y0 - SX1 * SY,       Z0 + P1 * SZ));
                    MO.Vertices.AddObject('',T3DPoint.Create(X0 - (SY1 + 1) * SX, Y0 - (SX1 + 1) * SY, Z0 + P4 * SZ));
                    MO.Vertices.AddObject('',T3DPoint.Create(X0 - (SY1 + 1) * SX, Y0 - SX1 * SY,       Z0 + P3 * SZ));
                    P := TPolygon.Create([I,I + 1,I + 2],frmNewZone.cbTexture.Text);
                    P.HasTexCoords := True;
                    SetLength(P.TX,3);
                    SetLength(P.TZ,3);
                    P.TX[0] := SX1       + 1/TextureSize;
                    P.TX[1] := (SX1 + 1) - 1/TextureSize;
                    P.TX[2] := P.TX[0];
                    P.TZ[0] := SY1       + 1/TextureSize;
                    P.TZ[1] := (SY1 + 1) - 1/TextureSize;
                    P.TZ[2] := P.TZ[1];

                    // We're going to use Tag to contain the grid position.  It will be
                    // automatically copied if the polygon is broken up.

                    P.Tag    := SY1 * NSX + SX1;
                    P.HasTag := True;
                    MO.Polygons.AddObject('',P);
                    Inc(I,3);
                  End; // For X
                End; // For Y
                MO.CalcNormals;

                // If we specified a below-water texture and there are already water areas set, split everything up along the water
                // boundaries and assign the water textures

                Tree := TTree.Create(MO,False);
                If (High(Zone.Water) >= 0) And
                   (frmNewZone.cbBelowTexture.Text <> '') And
                   (frmNewZone.cbBelowTexture.Text <> frmNewZone.cbTexture.Text) Then
                Begin
                  For I := 0 To High(Zone.Water) Do
                  Begin
                    Tree.SplitAlongWater(Zone.Water[I]);
                    Case Zone.Water[I].WType Of
                      wtWater: Tree.Root.SetFlaggedToAttribute(raWater,0);
                       wtLava: Tree.Root.SetFlaggedToAttribute(raLava,0);
                        wtPvP: Tree.Root.SetFlaggedToAttribute(raPvP,0);
                        wtIce: Tree.Root.SetFlaggedToAttribute(raIce,0);
                   wtIceWater: Tree.Root.SetFlaggedToAttribute(raIceWater,0);
                    End; // Case
                    If Zone.Water[I].WType <> wtPvP Then Tree.Root.SetFlaggedToTexture(frmNewZone.cbBelowTexture.Text);
                  End; // For I

                  // Put the underwater polygons in their own mesh

                  MO1 := Tree.SplitOffMesh([raWater,raLava]);
                  MO1.SetName(meshHeightMapUnderwater);
                End
                Else MO1 := Nil;
                Tree.Free;

                // Add the ground and underwater meshes

                Zone.AddObject(MO.GetName,MO);
                If MO1 <> Nil Then Zone.AddObject(MO1.GetName,MO1);

                // Make the bounding box

                If frmNewZone.cbMakeBoundingBox.Checked Then MakeBoundingBox;
              End;
              BMP.Free;
            End
            Else Exit;
          End
          Else
          Begin
            ShowMessage('Operation cancelled');
            Exit;
          End;
        End
        Else
        Begin
          Zone.Free;
          Zone := TZone.Create;
          Zone.SetDefaultLandTexture(frmNewZone.cbTexture.Text);
          Zone.SetDefaultUnderwaterTexture(frmNewZone.cbBelowTexture.Text);

          SX   := GridSize;
          SY   := GridSize;

          // Round the size up to a texture boundary (this is vital)

          If (szNS Div SX) * SX < szNS Then szNS := ((szNS Div SX) + 1) * SX;
          If (szEW Div SY) * SY < szEW Then szEW := ((szEW Div SY) + 1) * SY;

          NSY := (szNS Div SX);
          NSX := (szEW Div SY);

          // Set up the elevation grid

          Zone.ElevationGrid.MinX := poNS - szNS / 2;
          Zone.ElevationGrid.MinY := poEW - szEW / 2;
          Zone.ElevationGrid.MinZ := poUD;
          Zone.ElevationGrid.MaxX := poNS + szNS / 2;
          Zone.ElevationGrid.MaxY := poEW + szEW / 2;
          Zone.ElevationGrid.MaxZ := poUD;
          Zone.ElevationGrid.NX   := NSX + 1;
          Zone.ElevationGrid.NY   := NSY + 1;
          SetLength(Zone.ElevationGrid.Heights,(NSX + 1) * (NSY + 1));
          SetLength(Zone.ElevationGrid.Visible,NSX * NSY);
          For I := 0 To High(Zone.ElevationGrid.Heights) Do Zone.ElevationGrid.Heights[I] := poUD;
          For I := 0 To High(Zone.ElevationGrid.Visible) Do Zone.ElevationGrid.Visible[I] := True;

          // Create the mesh

          MO := TMeshObject.Create(meshHeightMapGround);
          X0 := poNS + szNS / 2;
          Y0 := poEW + szEW / 2;
          Z0 := poUD;
          I  := 0;
          For X := 0 To NSX - 1 Do
          Begin
            For Y := 0 To NSY - 1 Do
            Begin
              // Start with triangles to save us a step later

              MO.Vertices.AddObject('',T3DPoint.Create(X0 - Y * SX,       Y0 - X * SY,       Z0));
              MO.Vertices.AddObject('',T3DPoint.Create(X0 - Y * SX,       Y0 - (X + 1) * SY, Z0));
              MO.Vertices.AddObject('',T3DPoint.Create(X0 - (Y + 1) * SX, Y0 - (X + 1) * SY, Z0));
              P := TPolygon.Create([I,I + 1,I + 2],frmNewZone.cbTexture.Text);
              P.HasTexCoords := True;
              SetLength(P.TX,3);
              SetLength(P.TZ,3);
              P.TX[0] := X       + 1/TextureSize;
              P.TX[1] := (X + 1) - 1/TextureSize;
              P.TX[2] := P.TX[1];
              P.TZ[0] := Y       + 1/TextureSize;
              P.TZ[1] := P.TZ[0];
              P.TZ[2] := (Y + 1) - 1/TextureSize;

              // We're going to use Tag to contain the grid position.  It will be
              // automatically copied if the polygon is broken up.

              P.Tag    := Y * NSX + X;
              P.HasTag := True;
              MO.Polygons.AddObject('',P);
              Inc(I,3);

              MO.Vertices.AddObject('',T3DPoint.Create(X0 - Y * SX,       Y0 - X * SY,       Z0));
              MO.Vertices.AddObject('',T3DPoint.Create(X0 - (Y + 1) * SX, Y0 - (X + 1) * SY, Z0));
              MO.Vertices.AddObject('',T3DPoint.Create(X0 - (Y + 1) * SX, Y0 - X * SY,       Z0));
              P := TPolygon.Create([I,I + 1,I + 2],frmNewZone.cbTexture.Text);
              P.HasTexCoords := True;
              SetLength(P.TX,3);
              SetLength(P.TZ,3);
              P.TX[0] := X       + 1/TextureSize;
              P.TX[1] := (X + 1) - 1/TextureSize;
              P.TX[2] := P.TX[0];
              P.TZ[0] := Y       + 1/TextureSize;
              P.TZ[1] := (Y + 1) - 1/TextureSize;
              P.TZ[2] := P.TZ[1];

              // We're going to use Tag to contain the grid position.  It will be
              // automatically copied if the polygon is broken up.

              P.Tag    := Y * NSX + X;
              P.HasTag := True;
              MO.Polygons.AddObject('',P);
              Inc(I,3);
            End; // For Y
          End; // For X
          MO.CalcNormals;

          If frmNewZone.cbMakeBoundingBox.Checked Then MakeBoundingBox;

          Zone.AddObject(MO.GetName,MO);
        End;
      End;

      If frmNewZone.cbMakeBoundingBox.Checked Then
      Begin
        Zone.BuildBounds;
        Zone.BuildBoundingPolygons;
      End;  
      BirdsEye         := True;
      glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
      glView.Scene3D.Scene.CheckHidden := Not BirdsEye;
      glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;
      SelectedPolygons.Clear;
      SelectedPolyMesh  := Nil;
      SelectedHotSpot   := Nil;
      SelectedHotSpotML := Nil;

      // Get a polygon list from the zone

      ReloadZone;

      glView.AllowMouse := BirdsEye;
      LoadObjectTreeView;
      Observer.Free;
      Observer := T3DPoint.Create(poEW,poNS,poUD);

      // Render the zone

      RenderZone(BirdsEye);
      glView.Fit(Not BirdsEye,True,False);
      SetViewToNorth;
      LastLoadedScene  := '';
      UpdateTitleBar;
    End;
  End;
end; // TfrmMain.CreateNewZone

Procedure TfrmMain.RegenerateGround;
Var
  MO,MO1      : TMeshObject;
  MO2,MO3     : TMeshObject;
  X0,Y0       : Single;
  Z1,Z2,Z3,Z4 : Single;
  P,P1        : TPolygon;
  I,J,K       : Integer;
  X,Y         : Integer;
  SX,SY       : Integer;
  Tree        : TTree;
  Tex1        : String;
  Tex2        : String;
  GX,GY       : Integer;
  L,L1        : TStringList;
  Found       : Boolean;
  TN          : TTreeNode;

Begin
  If (Zone.ElevationGrid.NY > 1) And (Zone.ElevationGrid.NX > 1) Then
  Begin
    MO   := TMeshObject.Create(meshHeightMapGround);
    X0   := Zone.ElevationGrid.MaxX;
    Y0   := Zone.ElevationGrid.MaxY;
    SX   := GridSize;
    SY   := GridSize;
    I    := 0;
    Tex1 := UpperCase(Zone.GetDefaultLandTexture);
    Tex2 := UpperCase(Zone.GetDefaultUnderwaterTexture);
    For Y := 0 To Zone.ElevationGrid.NY - 2 Do
    Begin
      For X := 0 To Zone.ElevationGrid.NX - 2 Do
      Begin
        Z1 := Zone.ElevationGrid.GetHeight(X,Y);
        Z2 := Zone.ElevationGrid.GetHeight(X + 1,Y);
        Z3 := Zone.ElevationGrid.GetHeight(X,Y + 1);
        Z4 := Zone.ElevationGrid.GetHeight(X + 1,Y + 1);

        // Start with triangles to save us a step later

        MO.Vertices.AddObject('',T3DPoint.Create(X0 - Y * SX,       Y0 - X * SY,       Z1));
        MO.Vertices.AddObject('',T3DPoint.Create(X0 - Y * SX,       Y0 - (X + 1) * SY, Z2));
        MO.Vertices.AddObject('',T3DPoint.Create(X0 - (Y + 1) * SX, Y0 - (X + 1) * SY, Z4));
        P := TPolygon.Create([I,I + 1,I + 2],Tex1);
        P.HasTexCoords := True;
        SetLength(P.TX,3);
        SetLength(P.TZ,3);
        P.TX[0] := X       + 1/TextureSize;
        P.TX[1] := (X + 1) - 1/TextureSize;
        P.TX[2] := P.TX[1];
        P.TZ[0] := Y       + 1/TextureSize;
        P.TZ[1] := P.TZ[0];
        P.TZ[2] := (Y + 1) - 1/TextureSize;

        // We're going to use Tag to contain the grid position.  It will be
        // automatically copied if the polygon is broken up.

        P.Tag    := Y * (Zone.ElevationGrid.NX - 1) + X;
        P.HasTag := True;
        MO.Polygons.AddObject('',P);
        Inc(I,3);

        MO.Vertices.AddObject('',T3DPoint.Create(X0 - Y * SX,       Y0 - X * SY,       Z1));
        MO.Vertices.AddObject('',T3DPoint.Create(X0 - (Y + 1) * SX, Y0 - (X + 1) * SY, Z4));
        MO.Vertices.AddObject('',T3DPoint.Create(X0 - (Y + 1) * SX, Y0 - X * SY,       Z3));
        P := TPolygon.Create([I,I + 1,I + 2],Tex1);
        P.HasTexCoords := True;
        SetLength(P.TX,3);
        SetLength(P.TZ,3);
        P.TX[0] := X       + 1/TextureSize;
        P.TX[1] := (X + 1) - 1/TextureSize;
        P.TX[2] := P.TX[0];
        P.TZ[0] := Y       + 1/TextureSize;
        P.TZ[1] := (Y + 1) - 1/TextureSize;
        P.TZ[2] := P.TZ[1];

        // We're going to use Tag to contain the grid position.  It will be
        // automatically copied if the polygon is broken up.

        P.Tag    := Y * (Zone.ElevationGrid.NX - 1) + X;
        P.HasTag := True;
        MO.Polygons.AddObject('',P);
        Inc(I,3);
      End; // For X
    End; // For Y
    MO.CalcNormals;

    // If we specified a below-water texture and there are already water areas set, split everything up along the water
    // boundaries and assign the water textures

    frmStatus.SetCaption('Splitting along water boundaries');
    If (High(Zone.Water) >= 0) And
       (GetFirstTexture(Zone.GetDefaultLandTexture) <> '') And
       (Zone.GetDefaultUnderwaterTexture <> Zone.GetDefaultLandTexture) Then
    Begin
      Tree := TTree.Create(MO,False);
      For I := 0 To High(Zone.Water) Do
      Begin
        frmStatus.SetPosition(I / (High(Zone.Water) + 1));
        Tree.SplitAlongWater(Zone.Water[I]);
        Case Zone.Water[I].WType Of
          wtWater: Tree.Root.SetFlaggedToAttribute(raWater,0);
           wtLava: Tree.Root.SetFlaggedToAttribute(raLava,0);
            wtPvP: Tree.Root.SetFlaggedToAttribute(raPvP,0);
            wtIce: Tree.Root.SetFlaggedToAttribute(raIce,0);
       wtIceWater: Tree.Root.SetFlaggedToAttribute(raIceWater,0);
        End; // Case
        If (Zone.Water[I].WType <> wtPvP) And (GetFirstTexture(Zone.GetDefaultUnderwaterTexture) <> '') Then
         Tree.Root.SetFlaggedFromTextureToTexture(Zone.GetDefaultLandTexture,Zone.GetDefaultUnderwaterTexture);
      End; // For I

      // Put the underwater polygons in their own mesh

      frmStatus.SetCaption('Splitting off underwater mesh');
      frmStatus.SetPosition(0);
      MO1 := Tree.SplitOffMesh([raWater,raLava]);
      MO1.SetName(meshHeightMapUnderwater);
      Tree.Free;
    End
    Else MO1 := Nil;

    // Get the old meshes

    MO2  := Zone.FindMeshObject(meshHeightMapGround);
    MO3  := Zone.FindMeshObject(meshHeightMapUnderwater);

    // Shift the new meshes to match the old ones

    If MO2 <> Nil Then
    Begin
      For I := 0 To MO.Vertices.Count - 1 Do T3DPoint(MO.Vertices.Objects[I]).Subtract(MO2.Loc);
      If (MO1 <> Nil) And (MO3 <> Nil) Then
       For I := 0 To MO1.Vertices.Count - 1 Do T3DPoint(MO1.Vertices.Objects[I]).Subtract(MO3.Loc);
    End;

    // Now bring in any textures the user has set with the ground editor

    frmStatus.Visible := True;
    L := TStringList.Create;
    K := Zone.ElevationGrid.NX - 1;

    If MO2 <> Nil Then
    Begin
      // Make lists of polygons in the existing ground mesh that don't have the default land texture,
      // and index the lists by the polygons' tag values so they can be quickly looked up

      For I := 0 To MO2.Polygons.Count - 1 Do
      Begin
        P1 := TPolygon(MO2.Polygons.Objects[I]);
        P1.ConvertTexturesToUpperCase;
        If P1.HasTag And (P1.Texture <> Tex1) Then
        Begin
          While L.Count < (P1.Tag + 1) Do L.AddObject('',Nil);
          L1 := TStringList(L.Objects[P1.Tag]);
          If L1 = Nil Then
          Begin
            L1 := TStringList.Create;
            L.Objects[P1.Tag] := L1;
          End;
          L1.AddObject('',P1);
        End;
      End; // For I
      frmStatus.SetCaption('Retexturing land polygons');
      For I := 0 To MO.Polygons.Count - 1 Do
      Begin
        frmStatus.SetPosition(I / MO.Polygons.Count);
        P := TPolygon(MO.Polygons.Objects[I]);
        If P.HasTag Then
        Begin
          GX := P.Tag Mod K;
          GY := P.Tag Div K;
          If L.Count > P.Tag Then
          Begin
            L1 := TStringList(L.Objects[P.Tag]);
            If L1 <> Nil Then
            Begin
              For J := 0 To L1.Count - 1 Do
              Begin
                P1 := TPolygon(L1.Objects[J]);
                If Not P1.HasAngle Then P1.CalculateAngle(MO2);
                P.Texture := P1.Texture;
  //              P.CopyTextureFromPolygon(P1);
                P.Angle    := P1.Angle;
                P.HasAngle := P1.HasAngle;
                P.SetTexCoordsFromAngle(MO,X0,Y0,GX,GY);
              End; // For J
            End;
          End;
        End;
      End; // For I
    End;
    For I := 0 To L.Count - 1 Do L.Objects[I].Free;

    L.Clear;
    If (MO3 <> Nil) And (MO1 <> Nil) Then
    Begin
      // Make lists of polygons in the existing ground mesh that don't have the default underwater texture,
      // and index the lists by the polygons' tag values so they can be quickly looked up

      For I := 0 To MO3.Polygons.Count - 1 Do
      Begin
        P1 := TPolygon(MO3.Polygons.Objects[I]);
        P1.ConvertTexturesToUpperCase;
        If P1.HasTag And (P1.Texture <> Tex2) Then
        Begin
          While L.Count < (P1.Tag + 1) Do L.AddObject('',Nil);
          L1 := TStringList(L.Objects[P1.Tag]);
          If L1 = Nil Then
          Begin
            L1 := TStringList.Create;
            L.Objects[P1.Tag] := L1;
          End;
          L1.AddObject('',P1);
        End;
      End; // For I
      frmStatus.SetCaption('Retexturing underwater polygons');
      For I := 0 To MO1.Polygons.Count - 1 Do
      Begin
        frmStatus.SetPosition(I / MO1.Polygons.Count);
        P := TPolygon(MO1.Polygons.Objects[I]);
        If P.HasTag Then// And (P.Texture = Tex1) Then
        Begin
          GX := P.Tag Mod K;
          GY := P.Tag Div K;
          If L.Count > P.Tag Then
          Begin
            L1 := TStringList(L.Objects[P.Tag]);
            If L1 <> Nil Then
            Begin
              For J := 0 To L1.Count - 1 Do
              Begin
                P1 := TPolygon(L1.Objects[J]);
                If Not P1.HasAngle Then P1.CalculateAngle(MO3);
                P.Texture := P1.Texture;
  //              P.CopyTextureFromPolygon(P1);
                P.Angle    := P1.Angle;
                P.HasAngle := P1.HasAngle;
                P.SetTexCoordsFromAngle(MO,X0,Y0,GX,GY);
              End; // For J
            End;
          End;  
        End;
      End; // For I
    End;
    For I := 0 To L.Count - 1 Do L.Objects[I].Free;
    L.Free;

    // Get rid of the old meshes and replace them with the new ones

    If MO2 <> Nil Then
    Begin
      // First change the associated mesh

      I := Zone.IndexOfObject(MO2);
      L := TStringList.Create;
      MO2.AddToPolygonList(L,False,True,True,True);
      MasterMeshList.Objects[I].Free;
      MasterMeshList.Objects[I] := L.Objects[0];
      L.Free;

      // Now swap the object attached to the tree

      If tvMain.Items.Count > 0
       Then TN := GetNodeForMesh(tvMain.Items[0],I)
       Else TN := Nil;
      If TN <> Nil Then TN.Data := MO;

      // Swap the object

      frmStatus.SetCaption('Replacing land mesh');
      frmStatus.SetPosition(0);
      MO.SetName(MO2.GetName);
      MO.Loc.Copy(MO2.Loc);
      MO.Rotate.Copy(MO2.Rotate);
      MO.Size.Copy(MO2.Size);
      MO.SetParent(Nil);
      MO.SetZone(Zone);
      If ZPropList1.CurObj = MO2 Then ZPropList1.CurObj := MO;
      Zone.Objects[Zone.IndexOfObject(MO2)] := MO;
      MO2.Free;
    End
    Else
    Begin
      frmStatus.SetCaption('Adding land mesh');
      frmStatus.SetPosition(0);
      MO.SetParent(Nil);
      MO.SetZone(Zone);
      Zone.InsertObject(0,MO.GetName,MO);
    End;
    If MO1 <> Nil Then
    Begin
      If MO3 <> Nil Then
      Begin
        // First change the associated mesh

        I := Zone.IndexOfObject(MO3);
        L := TStringList.Create;
        MO3.AddToPolygonList(L,False,True,True,True);
        MasterMeshList.Objects[I].Free;
        MasterMeshList.Objects[I] := L.Objects[0];
        L.Free;

        // Now swap the object attached to the tree

        If tvMain.Items.Count > 0
         Then TN := GetNodeForMesh(tvMain.Items[0],I)
         Else TN := Nil;
        If TN <> Nil Then TN.Data := MO;

        // Swap the object

        frmStatus.SetCaption('Replacing underwater mesh');
        frmStatus.SetPosition(0);
        MO1.SetName(MO3.GetName);
        MO1.Loc.Copy(MO3.Loc);
        MO1.Rotate.Copy(MO3.Rotate);
        MO1.Size.Copy(MO3.Size);
        MO1.SetParent(Nil);
        MO1.SetZone(Zone);
        If ZPropList1.CurObj = MO3 Then ZPropList1.CurObj := MO1;
        Zone.Objects[Zone.IndexOfObject(MO3)] := MO1;
        MO3.Free;
      End
      Else
      Begin
        frmStatus.SetCaption('Adding underwater mesh');
        frmStatus.SetPosition(0);
        MO1.SetParent(Nil);
        MO1.SetZone(Zone);
        Zone.InsertObject(1,MO1.GetName,MO1);
      End;
    End;
  End;
  frmStatus.Hide;
End; // TfrmMain.RegenerateGround

Procedure TfrmMain.LoadDynamicLights;
Var
  I            : Integer;
  Lights       : TStringList;
  Light        : TLightObject;
  DynamicLight : TDynamicLight;
  
Begin
  // Add dynamic lights

  glView.Scene3D.Scene.LockBSPTree('TfrmMain.LoadDynamicLights');
  Lights := Zone.GetAllLightSources;
  glView.Scene3D.Scene.DynamicLights.FreeAll;
  For I := 0 To Lights.Count - 1 Do
  Begin
    Light        := TLightObject(Lights.Objects[I]);
    DynamicLight := TDynamicLight(glView.Scene3D.Scene.DynamicLights.GetNew(0));
    DynamicLight.Sphere.Center.Copy(Light.Loc);
    DynamicLight.Sphere.Radius := Light.Radius;
    DynamicLight.Color[0] := TBGRA(Light.Color).B / 255;
    DynamicLight.Color[1] := TBGRA(Light.Color).G / 255;
    DynamicLight.Color[2] := TBGRA(Light.Color).R / 255;
    DynamicLight.Color[3] := 1;//TBGRA(Light.Color).A;
    DynamicLight.Flicker  := Light.Flicker / 100;
    Light.Free;
  End; // For I
  Lights.Free;

  glView.Scene3D.Scene.UnlockBSPTree;
End; // TfrmMain.LoadDynamicLights

Procedure TfrmMain.ReloadModel(L: TStringList; Breakup: Boolean);
Var
  I            : Integer;
  B,B0         : Boolean;
  Entity       : TEntity;
  Renderable   : TRenderable;
  ZO           : TZoneObject;
  TexPath      : String;


Begin
  B0 := tmrTimeOfDay.Enabled;
  tmrTimeOfDay.Enabled := False;
  MasterTexList.Clear;
  ZoneMinX := 9999999;
  ZoneMinY := 9999999;
  ZoneMinZ := 9999999;
  ZoneMaxX := -9999999;
  ZoneMaxY := -9999999;
  ZoneMaxZ := -9999999;
  glView.Scene3D.Scene.bTranslucentMode := True;
  B := frmStatus.Visible;
  If Not B Then frmStatus.Show;
  frmStatus.SetCaption('Loading model into OpenGL');

  glView.Scene3D.Scene.LockBSPTree('TfrmMain.ReloadModel');

  SetCrosshair;
  SetSelectionRectangles;
  AddModelOrigins;
  SetPolygonSelectionRectangles;
  SetZoneLinePolygons;
  AddLightSpheres;
  AddHotSpots;
  For I := 0 To L.Count - 1 Do
  Begin
    frmStatus.SetPosition(I / L.Count);
    ZO := Zone.FindObjectByName(L.Strings[I]);
    If ZO <> Nil Then
    Begin
      If ZO Is TCreatureLibraryObjectReference
       Then TexPath := ExtractFilePath(Application.ExeName) + 'library\creatures\'
       Else TexPath := ExtractFilePath(Application.ExeName) + 'library\textures\';
    End
    Else TexPath := ExtractFilePath(Application.ExeName) + 'library\textures\';
    Entity := ReloadMesh(L,0,I,Breakup,True,True,glView,Tree,False,TexPath);
    If (ZO <> Nil) And (ZO Is TCreatureLibraryObjectReference) Then LoadCreatureSkeleton(TCreatureLibraryObjectReference(ZO).An8File,glView,Entity,ZO);
  End; // For I

  // Get rid of entities that are left over.  This is typically because the user deleted something from the zone.

  While glView.Scene3D.Scene.Entities.Count > L.Count + FirstEntity Do
  Begin
    Entity := TEntity(glView.Scene3D.Scene.Entities.Items[glView.Scene3D.Scene.Entities.Count - 1]);
    Entity.BSPTreeNode.RemoveEntity(Entity);
    glView.Scene3D.Scene.Entities.Delete(glView.Scene3D.Scene.Entities.Count - 1);
    Renderable := TRenderable(Entity.Renderable);
    Entity.Renderable := Nil;
    I := glView.Scene3D.Scene.Renderables.GetIndexOf(Renderable);
    glView.Scene3D.Scene.Renderables.Delete(I);
    Renderable.Free;
    Entity.Free;
  End; // While

  LoadDynamicLights;

  glView.Scene3D.Scene.UnlockBSPTree;
  If Not B Then frmStatus.Hide;
  tmrTimeOfDay.Enabled := B0;
End; // TfrmMain.ReloadModel

Function TfrmMain.ReloadMesh(L: TStringList; Base,Index: Integer; Breakup,AddWater,Master: Boolean; View: TGLVisir; Var Tree: TTree; Center: Boolean; TexPath: String): TEntity;
Var
  I,J,K,M     : LongInt;
  Mdl         : TModelRec;
  P           : TPolygon;
  Texture     : TTexture;
  Mesh        : TMeshObject;
  St,St1      : String;
//  Textures    : String;
//  Opacities   : String;
//  Parameters  : String;
  V           : T3DPoint;
  Tex1        : TStringList;
  Tex2        : Array Of String;
  Tex3        : Array Of TStringList;
  Tex3a       : TStringList;
  NeedsMask   : Array Of Boolean;
  ZP          : TZonePlane;
  Model       : TModel;
  X0,Y0,Z0    : Single;
  X1,Y1,Z1    : Single;
  CX,CY,CZ    : Single;
  TextureSet  : TTextureSet;
  TexObjs     : TTextureList;
  AnimTime    : Single;
  B           : Boolean;
  Entity      : TEntity;
  Position    : Points3D.T3DPoint;
  Rotation    : Points3D.T3DPoint;
  Scale       : Points3D.T3DPoint;
  TextureInfo : TTextureInfo;

Begin
  B := tmrTimeOfDay.Enabled;
  tmrTimeOfDay.Enabled := False;
  Mesh := TMeshObject(L.Objects[Index - Base]);

  // Add the water, but only once

  If (Index = 0) And AddWater Then Zone.AddWaterToMesh(Mesh);

  // Build the tree

  If Tree <> Nil Then Tree.Free;
  Tree := TTree.Create(Mesh,False);

  If BreakUp Then
  Begin
    frmStatus.SetCaption('Breaking zone into grid regions');
    Tree.SplitAlongGrid(1024,1024 * 3);
    If High(Zone.Water) >= 0 Then
    Begin
      frmStatus.SetCaption('Breaking off water, lava, and PvP regions');
      For J := 0 To High(Zone.Water) Do
      Begin
        Tree.SplitAlongWater(Zone.Water[J]);
        Case Zone.Water[J].WType Of
          wtWater: Tree.Root.SetFlaggedToAttribute(raWater,0);
           wtLava: Tree.Root.SetFlaggedToAttribute(raLava,0);
            wtPvP: Tree.Root.SetFlaggedToAttribute(raPvP,0);
            wtIce: Tree.Root.SetFlaggedToAttribute(raIce,0);
       wtIceWater: Tree.Root.SetFlaggedToAttribute(raIceWater,0);
        End; // Case
      End; // For J
    End;
    If Zone.ZonePlanes.Count > 0 Then
    Begin
      frmStatus.SetCaption('Breaking off zone line regions');
      For I := 0 To Zone.ZonePlanes.Count - 1 Do
      Begin
        ZP := TZonePlane(Zone.ZonePlanes.Objects[I]);
        Tree.SplitAlongZonePlane(ZP);
        Tree.Root.SetFlaggedToAttribute(raZoneLine,I);
      End; // For I
    End;
  End;

  Tree.Root.ConvertToTriangles;

  // We must *not* call TMeshObject.Coalesce or vertex normals get screwed up
  // (sharing vertices is BAD, BAD, BAD)

  // Load the polygon list into the GL viewer

  View.Scene3D.Scene.LockBSPTree('TfrmMain.ReloadMesh');

  If View.Scene3D.Scene.Entities.Count > Index + FirstEntity Then
  Begin
    Entity   := TEntity(View.Scene3D.Scene.Entities.Items[Index + FirstEntity]);
    Position := Points3D.T3DPoint.Create(Entity.Position);
    Rotation := Points3D.T3DPoint.Create(Entity.Rotation);
    Scale    := Points3D.T3DPoint.Create(Entity.Scale);
  End
  Else
  Begin
    Entity   := Nil;
    Position := Nil;
    Rotation := Nil;
    Scale    := Nil;
  End;

  Model      := View.Scene3D.GetNewModel(Index + FirstEntity);  // Creates an entity, renderable, and model if necessary

  // Create a texture set for the model -- every model will have its own texture set instead
  // of using raw texture ID's.  This way we don't have to worry about what textures are already loaded.

  TextureSet := TTextureSet.Create(View.Scene3D.Scene);

  // Set up the model-loading structure

  Mdl.np := Mesh.Polygons.Count;
  Mdl.nv := Mesh.Polygons.Count * 3;
  SetLength(Mdl.v,Mdl.nv);
  SetLength(Mdl.p,Mdl.np);

  // Add the polygons and log all the textures used (it's important to log the textures before assigning
  // texture IDs because the texture lists auto-sort for speed)

  Tex1        := TStringList.Create;
  Tex1.Sorted := True;
  SetLength(Tex2,0);
  SetLength(Tex3,0);
  SetLength(NeedsMask,0);

  K := 0;
  For I := 0 To Mesh.Polygons.Count - 1 Do
  Begin
    P := TPolygon(Mesh.Polygons.Objects[I]);
    For M := 0 To 2 Do
    Begin
      // We have to reverse the point order because OpenGL has the opposite way of doing things

      V              := T3DPoint(Mesh.Vertices.Objects[P.Vertices[2 - M]]);
      Mdl.V[K + M].X := V.X;
      Mdl.V[K + M].Y := V.Y;
      Mdl.V[K + M].Z := V.Z;
      If Master Then
      Begin
        If V.X < ZoneMinX Then ZoneMinX := V.X;
        If V.Y < ZoneMinY Then ZoneMinY := V.Y;
        If V.Z < ZoneMinZ Then ZoneMinZ := V.Z;
        If V.X > ZoneMaxX Then ZoneMaxX := V.X;
        If V.Y > ZoneMaxY Then ZoneMaxY := V.Y;
        If V.Z > ZoneMaxZ Then ZoneMaxZ := V.Z;
      End;
    End; // For M

    Mdl.P[I].TextureID := -1;

    // Important!  Make sure all slashes are consistent (or exporting to .WLD will fail)
    // This was changed so that the polygons automatically fix themselves.

    St := UpperCase(P.Texture);

    // Strip off any changeable parameters

//    BreakupTextureString(St,Textures,Opacities,Parameters);
    If P.TextureInfo.FirstTexture <> '' Then// Textures <> '' Then
    Begin
      St := P.TextureInfo.ToStringNoParms;// //Textures + '|' + Opacities;
      J  := Tex1.IndexOf(St);
      If J < 0 Then
      Begin
        // Save the string before we fix the slashes (Tex1 contains common names)

        Tex1.AddObject(St,Pointer(High(Tex2) + 1));     // Tex1 auto-sorts so we need to save the index of the entry in Tex2 so we can get it later
{
        // Fix slashes

        J  := Pos('/',St);
        Repeat
          If J > 0 Then St[J] := '\';
          J := Pos('/',St);
        Until J < 1;
}
        SetLength(Tex2,High(Tex2) + 2);
        Tex2[High(Tex2)] := P.Texture;//P.TextureInfo.ToString;//St + '+' + Parameters;                     // Store the fully-qualified filename in Tex2
        SetLength(NeedsMask,High(NeedsMask) + 2);
        NeedsMask[High(NeedsMask)] := P.NeedsMask;

//        P.Texture := St + '+' + Parameters;

        // Tex3 contains a list of specific variants (same textures and opacities, but different parameters)

        SetLength(Tex3,High(Tex3) + 2);
        Tex3[High(Tex3)] := TStringList.Create;
        Tex3[High(Tex3)].Sorted := True;
        Tex3[High(Tex3)].Add(P.Texture);
      End
      Else
      Begin
//        P.Texture := Tex1.Strings[J] + '+' + Parameters; // Get the "fixed" path + filename
        If Tex3[Integer(Tex1.Objects[J])].IndexOf(P.Texture) < 0 Then Tex3[Integer(Tex1.Objects[J])].Add(P.Texture);
        NeedsMask[J] := NeedsMask[J] Or P.NeedsMask;
      End;

      // Add the texture to the master texture list

      If Master And (MasterTexList.IndexOf(St) < 0) Then MasterTexList.Add(St);
    End;
    Inc(K,3); // Vertex counter
  End; // For I

  // Load the textures

  For J := 0 To High(Tex2){TexList.Count - 1} Do
  Begin
    // The texture might actually be a list of textures.

//    BreakupTextureString(Tex2[J],Textures,Opacities,Parameters);
    TextureInfo := TTextureInfo.Create(Tex2[J]);
    K := Integer(Tex1.Objects[J]);
    SetLength(TexObjs,0);
    For M := 0 To TextureInfo.TextureMaps.Count - 1 Do
    Begin
      St  := TextureInfo.TextureMaps.Strings[M];//GetToken(';',Textures);  // Texture map
      St1 := TextureInfo.OpacityMaps.Strings[M];//GetToken(';',Opacities); // Opacity map
      St  := TexPath + UpperCase(St) + '.bmp';
      If St1 <> '' Then St1 := TexPath + UpperCase(St1) + '.bmp';
      Texture                 := View.Scene3D.Scene.AddTexture(St,St1,NeedsMask[K]);
      Texture.Automatic       := False;
      Texture.EnvironmentMode := GL_MODULATE;
      SetLength(TexObjs,High(TexObjs) + 2);
      TexObjs[High(TexObjs)]  := Texture;
    End; // For M
    TextureInfo.Free;

    // Make a textureset entry for each variant of the texture (different parameters)

    For M := 0 To Tex3[J].Count - 1 Do
    Begin
      TextureInfo := TTextureInfo.Create(Tex3[J].Strings[M]);
//      BreakupTextureString(Tex3[J].Strings[M],Textures,Opacities,Parameters);
//      Val(Parameters,AnimTime,I);
      AnimTime := TextureInfo.AnimTime; 
      If High(TexObjs) > 0 Then
      Begin
        If (I <> 0) Or (AnimTime = 0) Then AnimTime := DefaultAnimTimePerFrame * (High(TexObjs) + 1);
        If AnimTime < (High(TexObjs) + 1) * MinimumAnimTimePerFrame Then AnimTime := (High(TexObjs) + 1) * MinimumAnimTimePerFrame;
        If AnimTime > 100 Then AnimTime := 100;
      End;
      AnimTime := AnimTime / (High(TexObjs) + 1);
      Tex3[J].Objects[M] := Pointer(TextureSet.NumTextures);        // Get the current texture set ID before adding the new textures
      TextureSet.AddTextures(TexObjs,Round(AnimTime * 1000));
      TextureInfo.Free;
    End; // For M

    SetLength(TexObjs,0);
  End; // For J

  // Map the faces to each texture

  M := 0;
  For I := 0 To Mesh.Polygons.Count - 1 Do
  Begin
    P := TPolygon(Mesh.Polygons.Objects[I]);

    // If the user already supplied a color then use that color, otherwise use
    // a default.

    For K := 0 To 2 Do
    Begin
      If P.HasColor Then
      Begin
        TRGBA(Mdl.V[M + K].Color).R := Round((255 - TRGBA(P.Colors[2 - K]).A) * ({255 -} TRGBA(P.Colors[2 - K]).R) / 255);
        TRGBA(Mdl.V[M + K].Color).G := Round((255 - TRGBA(P.Colors[2 - K]).A) * ({255 -} TRGBA(P.Colors[2 - K]).G) / 255);
        TRGBA(Mdl.V[M + K].Color).B := Round((255 - TRGBA(P.Colors[2 - K]).A) * ({255 -} TRGBA(P.Colors[2 - K]).B) / 255);
        TRGBA(Mdl.V[M + K].Color).A := 255; // Force to solid
      End
      Else
      Begin
        TRGBA(Mdl.V[M + K].Color).R := 255;
        TRGBA(Mdl.V[M + K].Color).G := 255;
        TRGBA(Mdl.V[M + K].Color).B := 255;
        TRGBA(Mdl.V[M + K].Color).A := 255;
      End;

//      St := P.Texture;

      // Strip off any changeable parameters

//      BreakupTextureString(St,Textures,Opacities,Parameters);
//      St := Textures + '|' + Opacities;
      St  := P.TextureInfo.ToStringNoParms;
//      St1 := P.TextureInfo.ToString;
      If St{Textures} <> '' Then
      Begin
        J                  := Tex1.IndexOf(St);
        Tex3a              := Tex3[Integer(Tex1.Objects[J])];
        Mdl.P[I].TextureID := Integer(Tex3a.Objects[Tex3a.IndexOf(P.Texture)]); // The texture ID is the texture set ID -- every model will have a texture set
        P.TextureRef       := TextureSet.GetTexture(Mdl.P[I].TextureID); // Not deterministic (based on time) but good enough
      End
      Else Mdl.P[I].TextureID := -1;

      // Transparency and semitransparency

      If P.TextureState = tsTransparent Then
      Begin
        If cbShowTransparent.Checked Then
        Begin
          If cbTransparentAsSolid.Checked
           Then TBGRA(Mdl.V[M + K].Color).A := 255
           Else TBGRA(Mdl.V[M + K].Color).A := 64;
        End
        Else TBGRA(Mdl.V[M + K].Color).A := 0;
        Mdl.P[I].TextureID := -1;
        TBGRA(Mdl.V[M + K].Color).R := 255;
        TBGRA(Mdl.V[M + K].Color).G := 255;
        TBGRA(Mdl.V[M + K].Color).B := 255;
      End
      Else If P.TextureState = tsSemiTransparent Then TBGRA(Mdl.V[M + K].Color).A := 128;
    End; // For K
    Inc(M,3);
  End; // For I

  Tex1.Free;
  SetLength(Tex2,0);
  For I := 0 To High(Tex3) Do Tex3[I].Free;
  SetLength(Tex3,0);
  SetLength(NeedsMask,0);

  CalcTextureCoords(TMeshObject(L.Objects[Index - Base]),Index,View,Master,Mdl);

  // Center the model if we've been told to

  If Center And (Mdl.NV > 0) Then
  Begin
    X0 := Mdl.V[0].X;
    Y0 := Mdl.V[0].Y;
    Z0 := Mdl.V[0].Z;
    X1 := X0;
    Y1 := Y0;
    Z1 := Z0;
    For I := 1 To Mdl.NV - 1 Do
    Begin
      If Mdl.V[I].X < X0 Then X0 := Mdl.V[I].X;
      If Mdl.V[I].Y < Y0 Then Y0 := Mdl.V[I].Y;
      If Mdl.V[I].Z < Z0 Then Z0 := Mdl.V[I].Z;
      If Mdl.V[I].X > X1 Then X1 := Mdl.V[I].X;
      If Mdl.V[I].Y > Y1 Then Y1 := Mdl.V[I].Y;
      If Mdl.V[I].Z > Z1 Then Z1 := Mdl.V[I].Z;
    End; // For I
    CX := (X0 + X1) / 2;
    CY := (Y0 + Y1) / 2;
    CZ := (Z0 + Z1) / 2;
    For I := 0 To Mdl.NV - 1 Do
    Begin
      Mdl.V[I].X := Mdl.V[I].X - CX;
      Mdl.V[I].Y := Mdl.V[I].Y - CY;
      Mdl.V[I].Z := Mdl.V[I].Z - CZ;
    End; // For I
  End;

  // Add the model to the GL viewer

  View.AddModelToFrame(Mdl,Index + FirstEntity,False,False);  // Clears the TModel's texture set
  Model.CalcExtents(False);
  Model.PutTextureSet(0,TextureSet);    // Has to come after the TModel has been built

  // Load the bone piece indices

  SetLength(Model.PieceIndices,Mdl.NV);
  K := 0;
  For I := 0 To Mesh.Polygons.Count - 1 Do
  Begin
    P := TPolygon(Mesh.Polygons.Objects[I]);
    For M := 0 To 2 Do
    Begin
      // We have to reverse the point order because OpenGL has the opposite way of doing things

      V := T3DPoint(Mesh.Vertices.Objects[P.Vertices[2 - M]]);
      If High(Mesh.BoneIndices) >= P.Vertices[2 - M]
       Then Model.PieceIndices[K + M] := Mesh.BoneIndices[P.Vertices[2 - M]]
       Else Model.PieceIndices[K + M] := -1;
    End; // For M
    Inc(K,3); // Vertex counter
  End; // For I

  View.Scene3D.Scene.Texturing := True;

  SetLength(Mdl.V,0);
  SetLength(Mdl.P,0);

  TEntity(View.Scene3D.Scene.Entities.Items[Index + FirstEntity]).UseLocalPosAndScale := True;

  If Entity <> Nil Then
  Begin
    Entity.Position.Copy(Position);
    Entity.Rotation.Copy(Rotation);
    Entity.Scale.Copy(Scale);
    Position.Free;
    Rotation.Free;
    Scale.Free;
  End;

  Result := TEntity(View.Scene3D.Scene.Entities.Items[Index + FirstEntity]);

  View.Scene3D.Scene.UnlockBSPTree;
  tmrTimeOfDay.Enabled := B;
End; // TfrmMain.ReloadMesh

Procedure TfrmMain.AddLightSpheres;
Var
  I,J,K    : Integer;
  NumPolys : Integer;
  List     : TStringList;
  LO       : TLightObject;
  Model    : TModel;
  Model1   : TModel;
  Mdl      : TModelRec;

Begin
  Try
    glView.Scene3D.Scene.LockBSPTree('TfrmMain.AddLightSpheres');
    Model := glView.Scene3D.GetNewModel(EntityLightSpheres);
    Model.Clear;
    If cbShowLightSources.ItemIndex > 1 Then
    Begin
      Case cbShowLightSources.ItemIndex Of
        2: TEntity(glView.Scene3D.Scene.Entities.Items[EntityLightSpheres]).WireFrame := wfPoints;
        3: TEntity(glView.Scene3D.Scene.Entities.Items[EntityLightSpheres]).WireFrame := wfLines;
        4: TEntity(glView.Scene3D.Scene.Entities.Items[EntityLightSpheres]).WireFrame := wfPolygons;
        5: TEntity(glView.Scene3D.Scene.Entities.Items[EntityLightSpheres]).WireFrame := wfLinesAndPolygons;
      End; // Case
      List := Zone.GetAllLightSources;
      If List.Count > 0 Then
      Begin
        Model1 := TModel.Create(Nil);
        Model1.GeneratePentakisDodecahedron;
        NumPolys := Model1.Positions.Length Div 9;
        Mdl.NP := NumPolys * List.Count;
        Mdl.NV := Mdl.NP * 3;
        SetLength(Mdl.P,Mdl.NP);
        SetLength(Mdl.V,Mdl.NV);
        For I := 0 To List.Count - 1 Do
        Begin
          LO := TLightObject(List.Objects[I]);
          For J := 0 To NumPolys - 1 Do
          Begin
            For K := 0 To 2 Do
            Begin
              Mdl.V[(I * NumPolys + J) * 3 + K].X     := Model1.Positions.DataArray[(J * 3 + K) * 3 + 0] * LO.Radius + LO.Loc.X;
              Mdl.V[(I * NumPolys + J) * 3 + K].Y     := Model1.Positions.DataArray[(J * 3 + K) * 3 + 1] * LO.Radius + LO.Loc.Y;
              Mdl.V[(I * NumPolys + J) * 3 + K].Z     := Model1.Positions.DataArray[(J * 3 + K) * 3 + 2] * LO.Radius + LO.Loc.Z;
              Mdl.V[(I * NumPolys + J) * 3 + K].Color := $80000000 Or (ColorToRGB(LO.Color) And $FFFFFF);
            End; // For K
          End; // For J
        End; // For I
        For I := 0 To Mdl.NP - 1 Do Mdl.P[I].TextureID := -1;

        // Add the model to the GL viewer

        glView.AddModelToFrame(Mdl,EntityLightSpheres,False,False);

        // Cleanup

        SetLength(Mdl.V,0);
        SetLength(Mdl.P,0);
        For I := 0 To List.Count - 1 Do List.Objects[I].Free;
        Model1.Free;
      End;
      List.Free;
    End;
    Model.CalcExtents(False);
  Finally
    glView.Scene3D.Scene.UnlockBSPTree;
  End;
End; // TfrmMain.AddLightSpheres

Procedure TfrmMain.CalcTextureCoords(Mesh: TMeshObject; Index: Integer; View: TGLVisir; Master: Boolean; Var Mdl: TModelRec);
Type TPolyArray = Packed Array Of Boolean;
Var
  I,J,K,L     : Integer;
  MinX        : Single;
  MinY        : Single;
  MinZ        : Single;
  MaxX        : Single;
  MaxY        : Single;
  MaxZ        : Single;
  DX,DY,DZ    : Single;
  RX1,RX2,RX3 : Single;
  RY1,RY2,RY3 : Single;
  RZ1,RZ2,RZ3 : Single;
  P1          : T3DPoint;
  P2          : T3DPoint;
  P3          : T3DPoint;
  P           : TPolygon;
  PolyNX      : Packed Array Of Single;
  PolyNY      : Packed Array Of Single;
  PolyNZ      : Packed Array Of Single;
  PolyNCount  : Packed Array Of Integer;
  Len         : Single;
  Normal,N1   : T3DPoint;

Begin
  P1 := T3DPoint.Create;
  P2 := T3DPoint.Create;
  P3 := T3DPoint.Create;
  Mesh.CalcNormals;

  // Now figure out the texture coordinates

  L := 0;
  For J := 0 To Mdl.NP - 1 Do
  Begin
    P := TPolygon(Mesh.Polygons.Objects[J]);

    // If texture coordinates were already supplied then simply copy them, otherwise
    // we'll try to figure out some good ones

    If (Mdl.P[J].TextureID >= 0) And P.HasTexCoords Then
    Begin
      // Assume the polygon is a triangle and get the vertices. We already reversed the order to make
      // OpenGL happy, so now let's reverse it again to make the client happy

      Mdl.V[L + 2].TX := P.TX[0];
      Mdl.V[L + 2].TZ := P.TZ[0];
      Mdl.V[L + 1].TX := P.TX[1];
      Mdl.V[L + 1].TZ := P.TZ[1];
      Mdl.V[L + 0].TX := P.TX[2];
      Mdl.V[L + 0].TZ := P.TZ[2];
    End
    Else
    Begin
      SetLength(P.TX,High(P.Vertices) + 1);
      SetLength(P.TZ,High(P.Vertices) + 1);

      If Mdl.P[J].TextureID >= 0 Then
      Begin
        // Get the extents of the polygon

        MinX := 9999999;
        MinY := 9999999;
        MinZ := 9999999;
        MaxX := -9999999;
        MaxY := -9999999;
        MaxZ := -9999999;
        For K := 0 To 2 Do
        Begin
          P1.Copy(Mdl.V[L + K].X, Mdl.V[L + K].Y, Mdl.V[L + K].Z);
          If P1.X < MinX Then MinX := P1.X;
          If P1.Y < MinY Then MinY := P1.Y;
          If P1.Z < MinZ Then MinZ := P1.Z;
          If P1.X > MaxX Then MaxX := P1.X;
          If P1.Y > MaxY Then MaxY := P1.Y;
          If P1.Z > MaxZ Then MaxZ := P1.Z;
        End; // For K

        // Assume the polygon is a triangle and get the vertices. We already reversed the order to make
        // OpenGL happy, so now let's reverse it again to make the client happy

        P1.Copy(Mdl.V[L + 2].X, Mdl.V[L + 2].Y, Mdl.V[L + 2].Z);
        P2.Copy(Mdl.V[L + 1].X, Mdl.V[L + 1].Y, Mdl.V[L + 1].Z);
        P3.Copy(Mdl.V[L + 0].X, Mdl.V[L + 0].Y, Mdl.V[L + 0].Z);

        // Normalize so that the points always start at zero

        If Master Then
        Begin
          P1.Subtract(RegionSize * Trunc(ZoneMinX / RegionSize),
                      RegionSize * Trunc(ZoneMinY / RegionSize),
                      RegionSize * Trunc(ZoneMinZ / RegionSize));

          P2.Subtract(RegionSize * Trunc(ZoneMinX / RegionSize),
                      RegionSize * Trunc(ZoneMinY / RegionSize),
                      RegionSize * Trunc(ZoneMinZ / RegionSize));

          P3.Subtract(RegionSize * Trunc(ZoneMinX / RegionSize),
                      RegionSize * Trunc(ZoneMinY / RegionSize),
                      RegionSize * Trunc(ZoneMinZ / RegionSize));
        End;

        Normal := T3DPoint.Create(P1);
        N1     := T3DPoint.Create(P3);
        Normal.Subtract(P2);
        N1.Subtract(P2);
        Normal.Cross(N1);
        Normal.Normalize;
        N1.Free;

        // Here's where the magic is.  It bases the texture coordinates
        // on the direction in which the polygon faces.

        DX := Abs(MaxX - MinX);
        DY := Abs(MaxY - MinY);
        DZ := Abs(MaxZ - MinZ);

        If Abs(Normal.Z) > Abs(Normal.X) Then
        Begin
          If Abs(Normal.Z) > Abs(Normal.Y) Then
          Begin
            // Polygon is mostly in the x-y plane

            P1.X := Rescale * P1.X / RegionSize;
            P1.Y := Rescale * P1.Y / RegionSize;
            P2.X := Rescale * P2.X / RegionSize;
            P2.Y := Rescale * P2.Y / RegionSize;
            P3.X := Rescale * P3.X / RegionSize;
            P3.Y := Rescale * P3.Y / RegionSize;
          End
          Else
          Begin
            // Polygon is mostly in the x-z plane

            P1.X := Rescale * P1.X / RegionSize;
            P1.Y := Rescale * P1.Z / RegionSize;
            P2.X := Rescale * P2.X / RegionSize;
            P2.Y := Rescale * P2.Z / RegionSize;
            P3.X := Rescale * P3.X / RegionSize;
            P3.Y := Rescale * P3.Z / RegionSize;
          End;
        End
        Else
        Begin
          If Abs(Normal.Y) > Abs(Normal.X) Then
          Begin
            // Polygon is mostly in the x-z plane

            P1.X := Rescale * P1.X / RegionSize;
            P1.Y := Rescale * P1.Z / RegionSize;
            P2.X := Rescale * P2.X / RegionSize;
            P2.Y := Rescale * P2.Z / RegionSize;
            P3.X := Rescale * P3.X / RegionSize;
            P3.Y := Rescale * P3.Z / RegionSize;
          End
          Else
          Begin
            // Polygon is mostly in the y-z plane

            P1.X := Rescale * P1.Y / RegionSize;
            P1.Y := Rescale * P1.Z / RegionSize;
            P2.X := Rescale * P2.Y / RegionSize;
            P2.Y := Rescale * P2.Z / RegionSize;
            P3.X := Rescale * P3.Y / RegionSize;
            P3.Y := Rescale * P3.Z / RegionSize;
          End;
        End;
        Normal.Free;

        Mdl.V[L + 2].TX := P1.X;
        Mdl.V[L + 2].TZ := P1.Y;
        Mdl.V[L + 1].TX := P2.X;
        Mdl.V[L + 1].TZ := P2.Y;
        Mdl.V[L + 0].TX := P3.X;
        Mdl.V[L + 0].TZ := P3.Y;

        // Copy the texture and normal information back to the model

        P.TX[0] := P1.X;
        P.TZ[0] := P1.Y;
        P.TX[1] := P2.X;
        P.TZ[1] := P2.Y;
        P.TX[2] := P3.X;
        P.TZ[2] := P3.Y;
      End
      Else
      Begin
        For K := 0 To 2 Do
        Begin
          P.TX[K] := 0;
          P.TZ[K] := 0;
        End; // For K
      End;
    End;

    // Copy the normals (we're reversing the order because we reversed the vertex
    // order, but in reality it doesn't matter because GLVisir makes them all
    // the same--it only does Goraud shading).  We're also negating them because
    // of the same reason--the vertex order was reversed.

    For K := 0 To 2 Do
    Begin
      Normal := T3DPoint(Mesh.Normals.Objects[P.Vertices[K]]);
      TNormal(Mdl.V[L + (2 - K)].Normal).X := Round(Normal.X * 127);
      TNormal(Mdl.V[L + (2 - K)].Normal).Y := Round(Normal.Y * 127);
      TNormal(Mdl.V[L + (2 - K)].Normal).Z := Round(Normal.Z * 127);
    End; // For K
    Inc(L,3);
  End; // For J
  P1.Free;
  P2.Free;
  P3.Free;
End; // TfrmMain.CalcTextureCoords

Procedure TfrmMain.RenderZone(BirdsEyeView: Boolean);
Const Latitude = 42;
Var
  I                 : Integer;
  Light             : TLight;
  List              : TStringList;
  L                 : TLightObject;
  C                 : TColor;
  SkyR              : Single;
  SkyG              : Single;
  SkyB              : Single;
  AmbientLightLevel : Single;
  AmbientLightX     : Single;
  AmbientLightY     : Single;
  AmbientLightZ     : Single;
  AmbientLightR     : Single;
  AmbientLightG     : Single;
  AmbientLightB     : Single;
  AmbientLightA     : Single;
  SunAngle          : Single;
  MinuteOfTheDay    : Single;
  RX,RY,RZ          : Single;
  S                 : Single;
  DayOfTheYear      : Single;
  Hour              : Integer;
  St                : String;
  Time              : LongInt;
  ElapsedMinutes    : Single;

Begin
  // If we have the checkbox checked to let time run continuously, update the time

  If cbRunTime.Checked Then
  Begin
    If LastTickCount <> 0 Then
    Begin
      Time := LongInt(CurrTickCount) - LongInt(LastTickCount);
      If Time > 0 Then
      Begin
        // Calculate the current time of day (sigh...what I wouldn't give for a Java GregorianCalendar class)

        ElapsedMinutes   := (20 * Time) / (60 * 1000);
        TimeOfDay.Minute := TimeOfDay.Minute + ElapsedMinutes;
        While TimeOfDay.Minute >= 60 Do
        Begin
          Inc(TimeOfDay.Hour);
          TimeOfDay.Minute := TimeOfDay.Minute - 60;
          If TimeOfDay.Hour = 24 Then
          Begin
            TimeOfDay.Hour := 0;
            Inc(TimeOfDay.Day);
            Case TimeOfDay.Month Of
              0,2,4,6,7,9,11:
              Begin
                If TimeOfDay.Day = 32 Then
                Begin
                  Inc(TimeOfDay.Month);
                  TimeOfDay.Day := 0;
                End;
              End;
              3,5,8,10:
              Begin
                If TimeOfDay.Day = 31 Then
                Begin
                  Inc(TimeOfDay.Month);
                  TimeOfDay.Day := 0;
                End;
              End;
              1:
              Begin
                If (TimeOfDay.Year Div 4) = 0 Then
                Begin
                  If TimeOfDay.Day = 30 Then
                  Begin
                    Inc(TimeOfDay.Month);
                    TimeOfDay.Day := 0;
                  End;
                End
                Else
                Begin
                  If TimeOfDay.Day = 29 Then
                  Begin
                    Inc(TimeOfDay.Month);
                    TimeOfDay.Day := 0;
                  End;
                End;
              End;
            End; // Case
            If TimeOfDay.Month = 12 Then
            Begin
              TimeOfDay.Month := 0;
              Inc(TimeOfDay.Year);
            End;
          End;
        End; // While

        // Set the time-of-day slider to the new time

        If Not UpdatingFromSlider Then
        Begin
          UpdatingFromRender   := True;
          tbTimeOfDay.Position := TimeOfDay.Hour * 60 + Trunc(TimeOfDay.Minute);
          UpdatingFromRender   := False;
        End;
      End;
    End;
  End;

  // Calculate the ambient lighting level, direction, and color

  If (TimeOfDay.Hour < 5) Or (TimeOfDay.Hour >= 19) Then
  Begin
    AmbientLightLevel := 0.2;  // Slightly bluish moonlight
    AmbientLightX     := 0;
    AmbientLightY     := 0;
    AmbientLightZ     := 1;

    AmbientLightR     := 0.4;
    AmbientLightG     := 0.39;
    AmbientLightB     := 0.9;

    AmbientLightA     := 1;
  End
  Else
  Begin
    // No north-south component.  If I wanted to be really accurate I could look at the sun's position based on season
    // but that would depend on lat/long coordinates, atmospheric conditions, planet's axial tilt, tilt precession,
    // planet's orbit, sun's luminosity (which affects light color btw), etc :), which we don't have.  I could always
    // make something up...

    S             := (TimeOfDay.Hour - 5) * 60 + TimeOfDay.Minute;
    AmbientLightX := 0;
    AmbientLightY := -Cos(S * (Pi / (14 * 60)));
    AmbientLightZ := {-}Sin(S * (Pi / (14 * 60)));

    AmbientLightLevel := 0.2 + 0.8 * Sin(S * (Pi / (14 * 60)));

    // Actually we are taking atmospheric conditions into account with a rough model (I am *not* going to delve into
    // real scattering theory here)

    SunAngle      := (S - 60) * (Pi / (12 * 60)); // Assuming 12 hour days (vernal or autumnal equinox)

    AmbientLightR := Max(Min(2.3 * Sin(SunAngle) + 1,1),0);
    AmbientLightG := Max(Min(1.2 * Sin(SunAngle) + 0.7,1),0);
    AmbientLightB := Max(Min(0.1 * Abs(Sin(SunAngle)) + 0.9,1),0);
  End;

  // Set the star field rotation

  MinuteOfTheDay := TimeOfDay.Hour * 60 + TimeOfDay.Minute;
  DayOfTheYear   := 0;
  For I := 1 To TimeOfDay.Month Do
  Begin
    Case I Of
      1,3,5,7,8,10,12:
      Begin
        DayOfTheYear := DayOfTheYear + 31;
      End;
      4,6,9,11:
      Begin
        DayOfTheYear := DayOfTheYear + 30;
      End;
      2:
      Begin
        If (TimeOfDay.Year Mod 4) = 0
         Then DayOfTheYear := DayOfTheYear + 29
         Else DayOfTheYear := DayOfTheYear + 28;
      End;
    End; // Case
  End; // For I
  If (TimeOfDay.Year Mod 4) = 0
   Then DayOfTheYear := DayOfTheYear / 366
   Else DayOfTheYear := DayOfTheYear / 365;

  DayOfTheYear := DayOfTheYear * (2 * Pi); // Make it an angle

  RX := -360 * MinuteOfTheDay / (24 * 60);
  RY := -Latitude + 23.5 * Cos(DayOfTheYear); // Lat: 42 degrees, with 23.5 degree planetary axial tilt
  RZ := 23.5 * Sin(DayOfTheYear);
  glView.Scene3D.Scene.StarField.SetRotate(RX,RY,RZ); // OpenGL takes degrees, not radians

        // Sun position in the sky

        RX := -360 * MinuteOfTheDay / (24 * 60) + 90;     // Earth rotates once per day

        // We are at 42 degrees north latitude, but the earth has a 23.45 degree axial tilt with respect to the ecliptic

        RY := -Latitude - 23.45 * Cos(DayOfTheYear);
        RZ := 23.5 * Sin(DayOfTheYear);
//        glView.Scene3D.Scene.Sun.SetRotate(RX,RY,RZ);     // OpenGL takes degrees, not radians

        // It's expensive to re-shade the sky so only do it every second

        glView.Scene3D.Scene.SkySphere1.Visible := cbRealisticSky.Checked;
        glView.Scene3D.Scene.SkySphere1.SetRotate(RX,RY,RZ); // OpenGL takes degrees, not radians
//          glView.Scene3D.Scene.SkySphere1.SetRotateWithMoon(RX,RY,RZ,RMX,RMY,RMZ,{0.12}0.06 * 0.5 * (1 + Cos(DayOfTheYear * (365 / 29.53059))));

  // Set the star alpha (so they fade)

  If (TimeOfDay.Hour < 5) Or (TimeOfDay.Hour >= 19) Then
  Begin
    // Night: stars at 100%

    glView.Scene3D.Scene.StarField.Alpha := 255;
  End
  Else If (TimeOfDay.Hour >= 6) And (TimeOfDay.Hour < 18) Then
  Begin
    // Day: no stars visible

    glView.Scene3D.Scene.StarField.Alpha := 0;
  End
  Else If TimeOfDay.Hour >= 18 Then
  Begin
    // Sunset: stars fade in (linearly: not necessarily accurate but I'm not writing this for NASA)

    glView.Scene3D.Scene.StarField.Alpha := Round(255 * TimeOfDay.Minute / 60);
  End
  Else
  Begin
    // Sunrise: stars fade out

    glView.Scene3D.Scene.StarField.Alpha := Round(255 * (60 - TimeOfDay.Minute) / 60);
  End;

  // Set the sky color

  If (TimeOfDay.Hour < 4) Or (TimeOfDay.Hour >= 20) Then
  Begin
    SkyR := 0;
    SkyG := 0;
    SkyB := 0;
  End
  Else
  Begin
    S    := (TimeOfDay.Hour - 4) * 60 + TimeOfDay.Minute;
    SkyR := (108 / 255) * Sin(Pi * S / (16 * 60));
    SkyG := (180 / 255) * Sin(Pi * S / (16 * 60));
    SkyB := (218 / 255) * (1 - (Sqr(1 - Sin(Pi * S / (16 * 60)))));
  End;
  TRGBA(C).R := Round(SkyR * 255);
  TRGBA(C).G := Round(SkyG * 255);
  TRGBA(C).B := Round(SkyB * 255);
  TRGBA(C).A := 0;
  glView.Scene3D.SetBackgroundColor(C);

  // Set the time-of-day label

  Case TimeOfDay.Hour Of
         0: St := Format('12:%.2d AM',[Trunc(TimeOfDay.Minute)]);
     1..11: St := Format('%d:%.2d AM',[TimeOfDay.Hour,Trunc(TimeOfDay.Minute)]);
        12: St := Format('12:%.2d PM',[Trunc(TimeOfDay.Minute)]);
    13..23: St := Format('%d:%.2d PM',[TimeOfDay.Hour - 12,Trunc(TimeOfDay.Minute)]);
  End; // Case
  lblTime.Caption := St;

  // Set the ambient light

  glView.Scene3D.Scene.LockBSPTree('TfrmMain.RenderZone');
  If glView.Scene3D.Scene.Lights.Count > 0 Then
  Begin
    Light := TLight(glView.Scene3D.Scene.Lights.Items[0]);
    Light.Ambient(AmbientLightLevel * AmbientLightR,AmbientLightLevel * AmbientLightG,AmbientLightLevel * AmbientLightB,1);
    Light.Diffuse(AmbientLightLevel * AmbientLightR,AmbientLightLevel * AmbientLightG,AmbientLightLevel * AmbientLightB,1);
    Light.Specular(AmbientLightLevel * AmbientLightR,AmbientLightLevel * AmbientLightG,AmbientLightLevel * AmbientLightB,1);
  //  Light.Ambient(tbAmbientLight.Position / 10,tbAmbientLight.Position / 10,tbAmbientLight.Position / 10,1);
  //  Light.Diffuse(tbAmbientLight.Position / 20,tbAmbientLight.Position / 20,tbAmbientLight.Position / 20,1);
  //  Light.Specular(tbAmbientLight.Position / 20,tbAmbientLight.Position / 20,tbAmbientLight.Position / 20,1);
  //  Light.Position.Copy(0,0,100000);
    Light.Position.Copy(AmbientLightX,AmbientLightY,AmbientLightZ);
    Light.Position.Multiply(10000);
    Light.SetOrientation(-AmbientLightX,-AmbientLightY,-AmbientLightZ);// (1,1,0);
    Light.Attenuation := 0.0;//.001;
    Light.LightType   := clStar;//clAmbiental;

  // Star alpha

//  glView.Scene3D.Scene.StarField.Alpha := Round(255 * (1 - (tbAmbientLight.Position / 10)));

  // Light "held" by player

    If glView.Scene3D.Scene.Lights.Count = 1 Then //glView.Scene3D.Scene.Lights.Add(TLight.Create(2));
     glView.Scene3D.Scene.AddLight(1);

    Light := TLight(glView.scene3D.scene.Lights.Items[1]);
  {
    Light.LightType    := clSpot;
    Light.CutOffAngle  := 1;
    Light.SpotExponent := 127;
    Light.SetOrientation(0,0,-1);
  }
    If cbShowPlayerLight.Checked And Not BirdsEyeView
  //   Then Light.Ambient(0.5,0.5,0.5,1)
     Then Light.Ambient(1,1,0.8,1)
     Else Light.Ambient(0,0,0,1);
    Light.Diffuse(0,0,0,1);
    Light.Specular(0,0,0,1);

    Light.LightType    := clStar;
    Light.Attenuation := 1 / Sqr(50);
    Light.AttenuationType := clQuadratic;

  {
    Light.Ambient(0.5,0.5,0.5,1);
    Light.Diffuse(0.25,0.25,0.25,1);
    Light.Specular(0.5,0.5,0.5,1);
  }
  //  Light.Attenuation := 0.0;//.001;
  //  Light.Source.SetPosition(Observer.X,Observer.Y,Observer.Z + 10000);
    Light.Position.Copy(Observer.X,Observer.Y,Observer.Z + ObsHeight);

    // Light sources in the zone

    If cbShowLightSources.ItemIndex = 1 Then
    Begin
      List := Zone.GetAllLightSources;
      For I := 0 To List.Count - 1 Do
      Begin
        L     := TLightObject(List.Objects[I]);
        If glView.Scene3D.Scene.Lights.Count >= I + 3 Then Light := TLight(glView.Scene3D.Scene.Lights.Items[I + 2])
        Else
        Begin
          glView.Scene3D.Scene.AddLight(I + 2);
          Light := TLight(glView.Scene3D.Scene.Lights.Items[glView.Scene3D.Scene.Lights.Count - 1]);
  //        Light := TLight.Create(I + 3);
  //        glView.Scene3D.Scene.Lights.Add(Light);
        End;
        Light.LightType    := clStar;
  //      Light.CutOffAngle  := 1;
  //      Light.SpotExponent := 127;
  //      Light.SetOrientation(0,0,-1);

        Light.Ambient(0,0,0,1);
        Light.Diffuse(0,0,0,1);
        Light.Specular(0,0,0,1);

        Light.Ambient(TBGRA(L.Color).R / 255,TBGRA(L.Color).G / 255,TBGRA(L.Color).B / 255,0);
  //      Light.Diffuse(TBGRA(L.Color).R / 255,TBGRA(L.Color).G / 255,TBGRA(L.Color).B / 255,1);
  //      Light.Specular(TBGRA(L.Color).R / 255,TBGRA(L.Color).G / 255,TBGRA(L.Color).B / 255,1);

  //      Light.Attenuation := 4;

        If L.Radius <> 0
         Then Light.Attenuation := 16 / Sqr(L.Radius)  //0.025;
         Else Light.Attenuation := 1600;
        Light.AttenuationType := clQuadratic;
        Light.Position.Copy(L.Loc.X,L.Loc.Y,L.Loc.Z);
      End; // For I
      For I := 0 To List.Count - 1 Do List.Objects[I].Free;
      List.Free;
    End
    Else
    Begin
      While glView.Scene3D.Scene.Lights.Count > 2 Do
      Begin
        TLight(glView.Scene3D.Scene.Lights.Items[2]).Free;
        glView.Scene3D.Scene.Lights.Delete(2);
      End; // While
    End;
  End;
  SetCrosshair;
  glView.Scene3D.Scene.UnlockBSPTree;
  SetZoneLinePolygons;
  If Not BirdsEyeView Then
  Begin
    glView.scene3D.scene.DefaultCamera.SetPosition(Observer.X,Observer.Y,Observer.Z + ObsHeight);
    glView.scene3D.scene.DefaultCamera.LookAt(Observer.X + Cos(Phi) * Sin(Theta),
                                              Observer.Y + Sin(Phi) * Sin(Theta),
                                              Observer.Z + ObsHeight + Cos(Theta));
    glView.scene3D.scene.DefaultCamera.SetVectorUp(-Cos(Phi) * Cos(Theta),
                                                   -Sin(Phi) * Cos(Theta),
                                                   Sin(Theta));
  End;
  UpdateStatusBar;
  glView.Scene3D.Scene.ActiveCamera.Position.Dirty := True; // Need this to force the camera node to be correct
  glView.Scene3D.Redraw;
End; // TfrmMain.RenderZone

Procedure TfrmMain.UpdateStatusBar;
Begin
  If BirdsEye
   Then sbMain.Panels.Items[0].Text := 'Birds-eye mode'
   Else sbMain.Panels.Items[0].Text := 'Fly mode';
End; // TfrmMain.UpdateStatusBar

Function TfrmMain.GetHighestZ(Const X,Y: Single; Out Z: Single): Boolean;
Begin
  Result := False;
  If Zone.ElevationGrid.CanGetHeightAtAbsolute(X,Y) Then
  Begin
    Z      := Zone.ElevationGrid.GetHeightAtAbsolute(X,Y);
    Result := True;
  End
  Else
  Begin
    If (MasterTree <> Nil) And
       (MasterTree.GetMaxHeightAt(X,Y,HighestPoint,HPV1,HPV2,HPV3,MasterTreeMinPt.Z - 1,MasterTreeMaxPt.Z + 1) <> Nil) Then
    Begin
      Z      := HighestPoint.Z;
      Result := True;
    End;
  End;
End; // TfrmMain.GetHighestZ

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
Const Scale = 1;
Var
  MS    : Single;
  RA    : Single;

  Procedure ClampToGround;
  Var Z: Single;
  Begin
    If GetHighestZ(Observer.X,Observer.Y,Z) Then Observer.Z := Z;
  End; // ClampToGround

begin
  If glView.Scene3D.Scene.Entities.Count > FirstEntity Then
  Begin
    If Not BirdsEye Then
    Begin
      MS := MoveSpeed;
      RA := RotateAmount;
      If ssShift In Shift Then
      Begin
        MS := MS / 10;
        RA := RA / 10;
      End;
      If ssAlt In Shift Then
      Begin
        MS := MS / 10;
        RA := RA / 10;
      End;
      Case Key Of
        VK_HOME:
        Begin
          If Not cbWalkAlongGround.Checked Then
          Begin
            Observer.Z := Observer.Z + (MS / Scale);
            DisplayStats;
            RenderZone(BirdsEye);
          End;
          Key := 0;
        End;
        VK_END:
        Begin
          If Not cbWalkAlongGround.Checked Then
          Begin
            Observer.Z := Observer.Z - (MS / Scale);
            DisplayStats;
            RenderZone(BirdsEye);
          End;
          Key := 0;
        End;
        VK_UP,VK_NUMPAD8:
        Begin
          If (Key = VK_UP) Or ProgramSettings.UseNumericKeypad Then
          Begin
            Observer.X := Observer.X + (MS / Scale) * Cos(Phi) * Sin(Theta);
            Observer.Y := Observer.Y + (MS / Scale) * Sin(Phi) * Sin(Theta);
            Observer.Z := Observer.Z + (MS / Scale) * Cos(Theta);
            If cbWalkAlongGround.Checked Then ClampToGround;
            DisplayStats;
            RenderZone(BirdsEye);
            Key := 0;
          End;
        End;
        VK_DOWN,VK_NUMPAD2:
        Begin
          If (Key = VK_DOWN) Or ProgramSettings.UseNumericKeypad Then
          Begin
            Observer.X := Observer.X - (MS / Scale) * Cos(Phi) * Sin(Theta);
            Observer.Y := Observer.Y - (MS / Scale) * Sin(Phi) * Sin(Theta);
            Observer.Z := Observer.Z - (MS / Scale) * Cos(Theta);
            If cbWalkAlongGround.Checked Then ClampToGround;
            DisplayStats;
            RenderZone(BirdsEye);
            Key := 0;
          End;
        End;
        VK_LEFT,VK_NUMPAD4:
        Begin
          If (Key = VK_LEFT) Or ProgramSettings.UseNumericKeypad Then
          Begin
            If ssCtrl In Shift Then
            Begin
              // Strafe left

              Observer.X := Observer.X + (MS / Scale) * Cos(Phi + Pi / 2) * Sin(Theta);
              Observer.Y := Observer.Y + (MS / Scale) * Sin(Phi + Pi / 2) * Sin(Theta);
              If cbWalkAlongGround.Checked Then ClampToGround;
              DisplayStats;
              RenderZone(BirdsEye);
              Key := 0;
            End
            Else
            Begin
              // Turn left

              Phi := Phi + RA;
              While Phi >= 2 * Pi Do Phi := Phi - 2 * Pi;
              DisplayStats;
              RenderZone(BirdsEye);
              Key := 0;
            End;
          End;
        End;
        VK_RIGHT,VK_NUMPAD6:
        Begin
          If (Key = VK_RIGHT) Or ProgramSettings.UseNumericKeypad Then
          Begin
            If ssCtrl In Shift Then
            Begin
              Observer.X := Observer.X + (MS / Scale) * Cos(Phi - Pi / 2) * Sin(Theta);
              Observer.Y := Observer.Y + (MS / Scale) * Sin(Phi - Pi / 2) * Sin(Theta);
              If cbWalkAlongGround.Checked Then ClampToGround;
              DisplayStats;
              RenderZone(BirdsEye);
              Key := 0;
            End
            Else
            Begin
              Phi := Phi - RA;
              While Phi < 0 Do Phi := Phi + 2 * Pi;
              DisplayStats;
              RenderZone(BirdsEye);
              Key := 0;
            End;
          End;
        End;
        VK_NEXT,VK_NUMPAD3:
        Begin
          If (Key = VK_NEXT) Or ProgramSettings.UseNumericKeypad Then
          Begin
            Theta := Theta + RA;
            If Theta > Pi Then Theta := Pi;
            DisplayStats;
            RenderZone(BirdsEye);
            Key := 0;
          End;
        End;
        VK_PRIOR,VK_NUMPAD9:
        Begin
          If (Key = VK_PRIOR) Or ProgramSettings.UseNumericKeypad Then
          Begin
            Theta := Theta - RA;
            If Theta < 0 Then Theta := 0;
            DisplayStats;
            RenderZone(BirdsEye);
            Key := 0;
          End;
        End;
        VK_CLEAR,VK_NUMPAD5:
        Begin
          If (Key = VK_CLEAR) Or ProgramSettings.UseNumericKeypad Then
          Begin
            Theta := Pi / 2;
            DisplayStats;
            RenderZone(BirdsEye);
            Key := 0;
          End;
        End;
      End; // Case
    End;
  End;
end;

procedure TfrmMain.FormShow(Sender: TObject);
Var
  TickCount : Cardinal;
  I         : Integer;
  Item      : TTBCustomItem;

  Procedure Process(Item: TTBCustomItem);
  Var I: Integer;
  Begin
    Item.DefaultDrawing := False;
    Item.OnPrePaint := TBItemPrePaint;
    For I := 0 To Item.Count - 1 Do Process(Item.Items[I]);
  End;

begin

  For I := 0 To TBToolbar1.Items.Count - 1 Do Process(TBToolbar1.Items.Items[I]);

  Application.HelpFile := ExtractFilePath(Application.ExeName) + 'openzone.hlp';

//  glView.Init;
//  glvMesh.Init;

  glView.Update;
  glvMesh.Update;

  While Not AllInitialized Do
  Begin
    Application.ProcessMessages;
    Sleep(1);
  End; // While

  SettingSetHeightEdit := True;
  edtSetHeight.Text    := '0';
  SettingSetHeightEdit := False;

  LastLoadedScene   := '';
  LoadingParms      := False;
  UpdateStatusBar;
  LoadScriptBitmaps;
  BuildScriptPalette;
  SetupPalette;
  UpdateTitleBar;
  EnableGUI(True);
  ZPropList1.Middle := 90;
  TickCount := GetTickCount - SplashShowTime;

  // Set up the meshes panel

  LoadMeshLibraryList(tvMeshes);

//  glvMesh.Scene3D.Scene.SetActive(True);

//  For I := 0 To MasterTextures.Count - 1 Do (MasterTextures.Objects[I] As TTexture).LoadTextureIntoOpenGL;

  glView.Scene3D.Scene.SkySphere1.SetTexture(Nil,False);
  glView.Scene3D.Scene.SkySphere1.UseAlpha := False;
  glView.Scene3D.Scene.SkySphere1.Size := 105;

  glView.Scene3D.Scene.OcclusionManager.Enabled := False;
  glView.Scene3D.Scene.ClearScene(True,True);
  glView.Clear;
  glView.Update;//Scene3D.Update(glView.Width,glView.Height);

  glvMesh.Scene3D.Scene.ClearScene(True,True);
  glvMesh.Clear;
  glvMesh.Update;//Scene3D.Update(glvMesh.Width,glvMesh.Height);

  While Not (glView.Initialized And glvMesh.Initialized) Do
  Begin
    Application.ProcessMessages;
    Sleep(1);
  End; // While

  // Make sure the splash screen is displayed for at least two seconds

  If (TickCount > 0) And (TickCount < 2000) Then Sleep(2000 - TickCount);

  frmSplash.Hide;

  // We have to override the captions for the TSpeedButtons or the action captions will
  // force captions on them (and we won't be able to see the glyphs)

  btnMoveObjectNorth.Caption   := '';
  btnMoveObjectSouth.Caption   := '';
  btnMoveObjectEast.Caption    := '';
  btnMoveObjectWest.Caption    := '';
  btnMoveObjectUp.Caption      := '';
  btnMoveObjectDown.Caption    := '';
  btnRotateObjectLeft.Caption  := '';
  btnRotateObjectRight.Caption := '';
  btnSizeUp.Caption            := '';
  btnSizeDown.Caption          := '';

  tmrTimeOfDay.Enabled := True;


   
  tvMeshes.Font.Name   := ZPropList1.Font.Name;
  tvMeshes.Font.Size   := ZPropList1.Font.Size;
  TBToolbar1.Font.Name := ZPropList1.Font.Name;
  TBToolbar1.Font.Size := ZPropList1.Font.Size;
  TBToolbar1.View.GetFont.Name := ZPropList1.Font.Name;
  TBToolbar1.View.GetFont.Size := ZPropList1.Font.Size;

  // Load the default vertex and fragment shader that we will use for dynamic lighting if the card supports GLSL

  LoadDefaultShaders;

end;

Procedure TfrmMain.LoadMeshLibraryList(tvMeshes: TTreeView);
Var
  TN,TN1,TN2 : TTreeNode;
  I,J        : Integer;
  GO         : TGroupObject;
  Categories : TStringList;
  List       : TStringList;
  List1      : TStringList;
  St,St1     : String;

Begin
  LoadingMeshLibraryList := True;

  TN := Nil;
  tvMeshes.Items.Clear;

  // First add all mesh objects that don't have a category

  For I := 0 To MeshLibrary.Count - 1 Do
  Begin
    GO := TGroupObject(MeshLibrary.Objects[I]);
    If GO.Category = '' Then
    Begin
      TN := tvMeshes.Items.Add(TN,GO.GetName);
      TN.Data := GO;
    End;
  End; // For I

  // Now make a sorted list of all categories.  For each category, make a sorted list of
  // all meshes that lie within it

  Categories := TStringList.Create;
  Categories.Sorted := True;
  For I := 0 To MeshLibrary.Count - 1 Do
  Begin
    GO := TGroupObject(MeshLibrary.Objects[I]);
    If GO.Category <> '' Then
    Begin
      St := LowerCase(GO.Category);
      J  := Categories.IndexOf(St);
      If J < 0 Then
      Begin
        List := TStringList.Create;
        List.Sorted := True;
        Categories.AddObject(St,List);
      End
      Else List := TStringList(Categories.Objects[J]);
      List.AddObject(LowerCase(GO.GetName),GO);
    End;
  End; // For I

  // The sort should automatically place subcategories below their parents (they are
  // delimited by periods).  Go through the list, adding each category (and any parent
  // categories as necessary), and add the mesh objects.

  List1 := TStringList.Create;
  For I := 0 To Categories.Count - 1 Do
  Begin
    St := Categories.Strings[I]; // Get the full category

    // Add nodes to the tree view.  Traverse down the category chain, branching off as necessary.
    // TN contains the last root node added and TN1 contains the last added node (which could
    // also be a root node).  List1 contains the last category branch added.

    J := 0;
    Repeat
      // Get the next branch level

      St1 := GetToken('.',St);

      // Compare to the last branch added

      If (J >= List1.Count) Or (St1 <> List1.Strings[J]) Then
      Begin
        // Either a different branch or a new one

        If J = 0 Then
        Begin
          TN  := tvMeshes.Items.Add(TN,St1);
          TN1 := TN;
        End
        Else TN1 := tvMeshes.Items.AddChild(TN1,St1);

        // Either add to or replace the saved category branch so successive categories will be
        // compared to this branch

        If J >= List1.Count
         Then List1.Add(St1)
         Else List1.Strings[J] := St1;
      End
      Else
      Begin
        If J = 0
         Then TN1 := TN
         Else TN1 := TN1.getLastChild;
      End;
      Inc(J);
    Until St = '';
    While List1.Count > J Do List1.Delete(List1.Count - 1);

    // Now that we are at the bottom, add the group objects

    List := TStringList(Categories.Objects[I]);
    For J := 0 To List.Count - 1 Do
    Begin
      GO       := TGroupObject(List.Objects[J]);
      TN2      := tvMeshes.Items.AddChild(TN1,GO.GetName);
      TN2.Data := GO;
    End; // For J 
  End; // For I

  // Now add creatures

  TN := tvMeshes.Items.Add(TN,'creatures');
  For I := 0 To CreatureLibrary.Count - 1 Do
  Begin
    TN1      := tvMeshes.Items.AddChild(TN,CreatureLibrary.Strings[I]);
    TN1.Data := CreatureLibrary.Objects[I];
  End;

  // Cleanup

  List1.Free;
  For I := 0 To Categories.Count - 1 Do Categories.Objects[I].Free;
  Categories.Free;
  LoadingMeshLibraryList := False;
End; // TfrmMain.LoadMeshLibraryList

Procedure TfrmMain.DisplayStats;
Var
  St : String;
  R  : Integer;
  V  : T3DPoint;

Begin

  V := T3DPoint.Create(Observer);
  V.Add(CreateDist * Cos(Phi),CreateDist * Sin(Phi),0{-ObsHeight});

  If BirdsEye
   Then sbMain.Panels.Items[1].Text := ''
   Else sbMain.Panels.Items[1].Text := 'Position: X = ' + Format('%5.3f',[Observer.X]) +
                                               ', Y = ' + Format('%5.3f',[Observer.Y]) +
                                               ', Z = ' + Format('%5.3f',[Observer.Z + ObsHeight]) +
                                      '   Create: X = ' + Format('%5.3f',[V.X]) +
                                               ', Y = ' + Format('%5.3f',[V.Y]) +
                                               ', Z = ' + Format('%5.3f',[V.Z]);
  V.Free;

  sbMain.Repaint;

  // Draw the compass

  R := imgCompass.Width Div 2;
  imgCompass.Canvas.Pen.Color   := Color;
  imgCompass.Canvas.Brush.Color := Color;
  imgCompass.Canvas.Brush.Style := bsSolid;
  imgCompass.Canvas.Rectangle(0,0,imgCompass.Width,imgCompass.Height);
  imgCompass.Canvas.Pen.Color   := clBlack;
  imgCompass.Canvas.Brush.Color := clWhite;
  imgCompass.Canvas.Ellipse(0,0,imgCompass.Width,imgCompass.Height);
  imgCompass.Canvas.MoveTo(R,R);
  imgCompass.Canvas.LineTo(R - Round(R * Cos((Pi / 2) - Phi)),R - Round(R * Sin((Pi / 2) - Phi)));
  imgCompass.Repaint;

  // Draw the elevation

  imgElevation.Canvas.Pen.Color   := Color;
  imgElevation.Canvas.Brush.Color := Color;
  imgElevation.Canvas.Brush.Style := bsSolid;
  imgElevation.Canvas.Rectangle(0,0,imgElevation.Width,imgElevation.Height);
  imgElevation.Canvas.Pen.Color   := clBlack;
  imgElevation.Canvas.Brush.Color := clWhite;
  imgElevation.Canvas.Ellipse(-imgElevation.Width,0,imgElevation.Width,imgElevation.Height);
  imgElevation.Canvas.MoveTo(0,R);
  imgElevation.Canvas.LineTo(Round(R * Cos((Pi / 2) - Theta)),R - Round(R * Sin((Pi / 2) - Theta)));

  // We need to set these corner pixels for transparency to work

  imgElevation.Canvas.Pixels[0,0] := Color;
  imgElevation.Canvas.Pixels[0,imgElevation.Height - 1] := Color;
  imgElevation.Repaint;

  St := Format('%5.3f',[SetHeight]);
  SettingSetHeightEdit := True;
  edtSetHeight.Text    := St;
  SettingSetHeightEdit := False;
End; // TfrmMain.DisplayStats

procedure TfrmMain.ExportToWLD(Stream: TStream; FileName: String; ZoneMesh: TMeshObject);
Type VertexArray = Packed Array Of Integer;
Var
  I,J,K        : Integer;
  D04          : Data04;
  D05          : Data05;
  D30          : Data30;
  Regions      : Packed Array Of Data22;
  D35          : EQWldDataFragment;
  D36          : Data36;
  D22          : Data22;
  St,St1,St2   : String;
  D1C          : Data1C;
  D2A          : Data2A;
  D09          : Data09;
  D08          : DataWithName;
  D14          : Data14;
  D15          : Data15;
  D16          : DataWithName;
  D1B          : Data1B;
  D29Water     : Data29;
  D29Lava      : Data29;
  D29PvP       : Data29;
  D29ZoneLines : Array Of Data29;
  D29Index     : Integer;
  D29Count     : Integer;
  List         : TStringList;
//  SemiTrans    : TStringList;
  OutTexCount  : Integer;
  V1           : Integer;
  P            : TPolygon;
  X0,Y0,Z0     : Single;
  X1,Y1,Z1     : Single;
  Texture      : String;
//  TexIndex     : Array Of Integer;
  Vertices     : Array Of Integer;
  Mesh         : TMeshObject;
  TexList      : TStringList;
  D31          : Data31;
  D21          : Data21;
  W            : TEQWldData;
  Sprites      : TStringList;
//  FirstData03  : Data03;
  V            : T3DPoint;
  C            : TColor;
  R            : Byte;
  Region       : TRegion;
  D21Index     : Integer;
  CX,CY,CZ     : Double;
  VX,VY,VZ     : Double;
  Center       : T3DPoint;
  MinPt        : T3DPoint;
  MaxPt        : T3DPoint;
  MaxVertDist  : T3DPoint;
  MaxDist      : Single;
  Parent       : TRegion;
  HighParent   : TRegion;
//  TagList      : TTagListArray;
  TagCount     : Integer;
  Special      : Integer;
//  Lights       : TStringList;
  TextureSorter : TQuickSorterProc;
  TagSorter     : TQuickSorterProc;
  LightTreeHash : TLightTreeHash;

  Procedure MakeWaterTextureEntry(Index: Integer);
  Var
    D03     : Array Of Data03;
    Texture : String;
    I       : Integer;

  Begin
    SetLength(D03,High(Zone.Water[Index].Tex) + 1);
    For I := 0 To High(Zone.Water[Index].Tex) Do
    Begin
      D03[I] := Data03.Create(W,'W1_' + IntToStr(OutTexCount) + '_' + IntToStr(I));
      D03[I].Size1 := 1;
      SetLength(D03[I].Data1,1);
      SetLength(D03[I].Sizes,1);
      Texture := ExtractFileName(Zone.Water[Index].Tex[I]) + '.bmp';
      D03[I].Sizes[0] := Length(Texture) + 5;
      SetLength(D03[I].Data1[0],D03[I].Sizes[0]);
      StrPCopy(@D03[I].Data1[0][0],Texture);
    End; // For I
    D04 := Data04.Create(W,'W' + IntToStr(OutTexCount) + '_SPRITE'); // Force uniqueness
    D04.Flags      := $18;
    D04.Params2[0] := $64;
    D04.Size1      := High(D03) + 1;
    SetLength(D04.Data1,High(D03) + 1);
    For I := 0 To High(D03) Do
    Begin
      D04.Data1[I]          := FragmentReference.Create;
      D04.Data1[I].Fragment := D03[I];
    End; // For I
    D04.Params1[0] := 0;

    D05                   := Data05.Create(W);
    D05.Flags             := $50;
    D05.Fragment          := FragmentReference.Create;
    D05.Fragment.Fragment := D04;

    D30 := Data30.Create(W,'W' + IntToStr(OutTexCount) + '_MDF');
    D30.Flags := 2;
    If Zone.Water[Index].SemiTrans
     Then D30.Params1[0] := $80000005
     Else D30.Params1[0] := $80000001;
    D30.Params2[0] := $004E4E4E;
    D30.Params3[0] := 0;
    D30.Params3[1] := 0.75;
    D30.Fragment   := FragmentReference.Create;
    D30.Fragment.Fragment := D05;
    D30.Pair.Data1 := 0;
    D30.Pair.Data2 := 0;

    For I := 0 To High(D03) Do AddFragment(W,D03[I]);
    AddFragment(W,D04);
    AddFragment(W,D05);
    AddFragment(W,D30);

    Inc(D31.Size1);
    SetLength(D31.Data1,High(D31.Data1) + 2);
    D31.Data1[High(D31.Data1)]          := FragmentReference.Create;
    D31.Data1[High(D31.Data1)].Fragment := D30;
    Inc(OutTexCount,High(D03) + 1);
    SetLength(D03,0);
  End; // MakeWaterTextureEntry

  Procedure AddWaterRegion(Region: TRegion);
  Begin
    If (Region.Left = Nil) And (Region.Right = Nil) Then
    Begin
      frmStatus.SetPosition(D29Count / D29Water.Size1);
      If Region.Attribute.Attr = raWater Then
      Begin
        D29Water.Data1[D29Count] := D29Index;
        Inc(D29Count);
      End;
      Inc(D29Index);
    End
    Else
    Begin
      If Region.Left  <> Nil Then AddWaterRegion(Region.Left);
      If Region.Right <> Nil Then AddWaterRegion(Region.Right);
    End;
  End; // AddWaterRegion

  Procedure AddLavaRegion(Region: TRegion);
  Begin
    If (Region.Left = Nil) And (Region.Right = Nil) Then
    Begin
      frmStatus.SetPosition(D29Count / D29Lava.Size1);
      If Region.Attribute.Attr = raLava Then
      Begin
        D29Lava.Data1[D29Count] := D29Index;
        Inc(D29Count);
      End;
      Inc(D29Index);
    End
    Else
    Begin
      If Region.Left  <> Nil Then AddLavaRegion(Region.Left);
      If Region.Right <> Nil Then AddLavaRegion(Region.Right);
    End;
  End; // AddLavaRegion

  Procedure AddPvPRegion(Region: TRegion);
  Begin
    If (Region.Left = Nil) And (Region.Right = Nil) Then
    Begin
      frmStatus.SetPosition(D29Count / D29PvP.Size1);
      If Region.Attribute.Attr = raPvP Then
      Begin
        D29PvP.Data1[D29Count] := D29Index;
        Inc(D29Count);
      End;
      Inc(D29Index);
    End
    Else
    Begin
      If Region.Left  <> Nil Then AddPvPRegion(Region.Left);
      If Region.Right <> Nil Then AddPvPRegion(Region.Right);
    End;
  End; // AddPvPRegion

  Procedure AddZoneLineRegion(Region: TRegion; Index: Integer);
  Begin
    If (Region.Left = Nil) And (Region.Right = Nil) Then
    Begin
      If (Region.Attribute.Attr = raZoneLine) And (Region.Attribute.Value = Index) Then
      Begin
        frmStatus.SetPosition(D29Count / D29ZoneLines[Index].Size1);
        D29ZoneLines[Index].Data1[D29Count] := D29Index;
        Inc(D29Count);
      End;
      Inc(D29Index);
    End
    Else
    Begin
      If Region.Left  <> Nil Then AddZoneLineRegion(Region.Left,Index);
      If Region.Right <> Nil Then AddZoneLineRegion(Region.Right,Index);
    End;
  End; // AddZoneLineRegion

  Function FindTextureInSprite(P: TPolygon; S: Data36): Integer;
  Var
    I,J,K   : Integer;
    Found   : Boolean;
    St      : String;
    Special : Integer;

  Begin
    St := P.Texture;
    If P.TextureState = tsTransparent Then St := TransparentTexture
    Else
    Begin
      Special := 0;
      If P.TextureState = tsSemiTransparent Then Special := Special Or spSemiTransparent; //St := St + #241;
      If P.Masked Then Special := Special Or spMasked;
      If Special <> 0 Then St := St + Chr(SpecialTexture + Special);
    End;
    I     := TexList.IndexOf(St);
    J     := 0;
    Found := False;
    While (J < S.Size7) And Not Found Do
    Begin
      If S.Data7[J].Data = I Then Found := True Else Inc(J);
    End; // While
    If Found Then
    Begin
      K     := 0;
      Found := False;
      While (K < S.Size8) And Not Found Do
      Begin
        If S.Data8[K].Data = I Then Found := True Else Inc(K);
      End; // While
      If Found Then Result := J + (K Shl 16) Else Result := -1;
    End
    Else Result := -1;
  End; // FindTextureInSprite

  Procedure ProcessRegion(ARegion: TRegion);
  Const Cube = 512;
  Var
    I,J,K,L,H  : Integer; // Only these variables have to be in the recursive part
    A,B        : Integer;
    Light      : TLightObject;
    Ray        : T3DPoint;
    Vertex     : T3DPoint;
    Normal     : T3DPoint;
    Dot        : Single;
    Intersect  : T3DPoint;
    Found      : Boolean;
    P1         : TPolygon;
    Intensity  : Single;
    PCenter    : T3DPoint;
    ToCenter   : T3DPoint;
//    RCenter    : T3DPoint;
    NRegions   : TStringList;
    RSize      : T3DPoint;
    Reg        : TRegion;
    MeshVerts  : TStringList;
    LightColor : TColor;
    CR,CG,CB   : Integer;
    Special    : Integer;
    C,C1       : Integer;

    Procedure GetNearRegions(List: TStringList; Region: TRegion; LightLoc: T3DPoint; VertexDist: Single);
    Begin
      If (High(Region.Polygons) >= 0) And
         (Region.Sphere.Center.DistanceFrom(LightLoc) < VertexDist + 1.1 * Region.Sphere.Radius) Then List.AddObject('',Region);
      If Region.Left  <> Nil Then GetNearRegions(List,Region.Left,LightLoc,VertexDist);
      If Region.Right <> Nil Then GetNearRegions(List,Region.Right,LightLoc,VertexDist);
    End; // GetNearRegions

    Function CompareTextures(Index0,Index1: Integer): Integer;

      Function GetTexIndex(Index: Integer): Integer;
      Var
        Poly    : TPolygon;
        St      : String;
        Special : Integer;

      Begin
        Poly   := TPolygon(SortMesh.Polygons.Objects[SortRegion.Polygons[TexIndex[Index]]]);
        St     := Poly.Texture;
        If Poly.TextureState = tsTransparent Then St := TransparentTexture
        Else
        Begin
          Special := 0;
          If Poly.TextureState = tsSemiTransparent Then Special := Special Or spSemiTransparent; //St := St + #241;
          If Poly.Masked Then Special := Special Or spMasked;
          If Special <> 0 Then St := St + Chr(SpecialTexture + Special);
        End;
        Result := SortTexList.IndexOf(St);
      End; // GetTexIndex

    Begin
      Result := GetTexIndex(Index0) - GetTexIndex(Index1);
    End; // CompareTextures

    Procedure ExchangeTextures(Index0,Index1: Integer);
    Var I: Integer;
    Begin
      I                := TexIndex[Index0];
      TexIndex[Index0] := TexIndex[Index1];
      TexIndex[Index1] := I;
    End; // ExchangeTextures

    Function CompareTags(Index0,Index1: Integer): Integer;
    Begin
      Result := TagList[Index0] - TagList[Index1];
    End; // CompareTags

    Procedure ExchangeTags(Index0,Index1: Integer);
    Var I: Integer;
    Begin
      I               := TagList[Index0];
      TagList[Index0] := TagList[Index1];
      TagList[Index1] := I;
    End; // ExchangeTags

  Begin
    frmStatus.SetPosition(D21Index / D21.Size1);
    If (ARegion.Left = Nil) And (ARegion.Right = Nil) Then
    Begin
      D36 := Nil;
      If High(ARegion.Polygons) >= 0 Then
      Begin
        MeshVerts := Mesh.Vertices; // Make a local reference to improve speed
        D36 := Data36.Create(W,'R' + IntToStr(Sprites.Count + 1) + '_DMSPRITEDEF');

        // Fill in the region data

        D36.Flags              := $18003;
        D36.Fragment1          := FragmentReference.Create;
        D36.Fragment1.Fragment := D31;
        D36.Fragment2          := FragmentReference.Create;
        D36.Fragment3          := FragmentReference.Create;
        D36.Fragment4          := FragmentReference.Create;
        D36.Fragment4.Fragment := GetFirstData03(W);
        D36.Params1[0] := 0;
        D36.Params1[1] := 0;
        D36.Params1[2] := 0;
        D36.Params2[0] := 0;
        D36.Params2[1] := 0;
        D36.Params2[2] := 0;

        D36.Box.MinX := 0;
        D36.Box.MinY := 0;
        D36.Box.MinZ := 0;
        D36.Box.MaxX := 0;
        D36.Box.MaxY := 0;
        D36.Box.MaxZ := 0;

        I := (High(ARegion.Polygons) + 1) * 3;

        D36.Size1  := I;                               // Vertices
        D36.Size2  := I;                               // Texture coordinates
        D36.Size3  := I;                               // Normals
        D36.Size4  := I;                               // Colors
        D36.Size5  := High(ARegion.Polygons) + 1;      // Polygons
        D36.Size6  := 0;                               // Unknown
        D36.Size7  := 0;                               // Polygon-texture counts
        D36.Size8  := 0;                               // Vertex-texture counts
        D36.Size9  := 0;                               // Unknown
//        D36.Size10 := 4;//7;                               // Scale

        SetLength(D36.Data1,D36.Size1);
        SetLength(D36.Data2,D36.Size2);
        SetLength(D36.Data3,D36.Size3);
        SetLength(D36.Data4,D36.Size4);
        SetLength(D36.Data5,D36.Size5 * 4);
        SetLength(D36.Data6,D36.Size6);
        SetLength(D36.Data7,D36.Size7);
        SetLength(D36.Data8,D36.Size8);

        // Load the polygon info.  TexIndex has some slack; if it's too small for what we need we'll grow it, but we
        // won't shrink it if it has more space than we need.  Reallocating it every time simply takes too long.
        // We'll deallocate the whole thing when we're done.

        If High(TexIndex) < High(ARegion.Polygons) Then SetLength(TexIndex,High(ARegion.Polygons) + 1);
        For I := 0 To High(ARegion.Polygons) Do TexIndex[I] := I;
        Region := ARegion;

        SortRegion  := Region;
        SortMesh    := Mesh;
        SortTexList := TexList;

        TextureSorter          := TQuickSorterProc.Create;
        TextureSorter.Compare  := @CompareTextures;
        TextureSorter.Exchange := @ExchangeTextures;
        TextureSorter.Sort(0,High(ARegion.Polygons));   // NOT High(TexIndex)!!!!
        TextureSorter.Free;

//        QuickSortTextures(0,High(ARegion.Polygons));  // NOT High(TexIndex)!!!!

        // The polygons should all be triangles at this point.  Make sure we have enough room for the vertex list.

        If High(Vertices) < ((High(ARegion.Polygons) + 1) * 3 - 1) Then SetLength(Vertices,(High(ARegion.Polygons) + 1) * 3);
        For I := 0 To High(ARegion.Polygons) Do
        Begin
          P := TPolygon(Mesh.Polygons.Objects[ARegion.Polygons[TexIndex[I]]]);

          // Can you pass through the polygon?

          If Not P.Solid
           Then D36.Data5[I].Flags := $10   // $10 means you can go through it
           Else D36.Data5[I].Flags := 0;

          // Find the polygon's vertices and add them to the sprite

          // While it may be tempting to get rid of redundant vertices, remember that
          // vertices also have texture coordinates mapped to them.  To allow neighboring
          // polygons to end on different texture coordinates, they have to have different
          // vertices.  From a space standpoint, it would be more efficient to eliminate
          // those vertices where all the normals and texture coordinates are the same.
          // Perhaps in a future version...

          For J := 0 To High(P.Vertices) Do
          Begin
            // Form a vertex index to make life easier

            V1 := I * 3 + J;

            // Add each vertex point.  For now, save the raw vertex number, as we'll be
            // using it later.

            Vertices[V1] := P.Vertices[J];

            // Texture coordinates are bound to vertices, not polygons. That means that
            // polygons which meet at the same vertex have to have the same texture
            // coordinates at that point.

            D36.Data2[V1].I1      := Round(P.TX[J] * TextureSize);
            D36.Data2[V1].I2      := Round(P.TZ[J] * TextureSize);

            // Each vertex has a normal vector for phong shading

            Normal := T3DPoint(Mesh.Normals.Objects[P.Vertices[J]]);
            D36.Data3[V1].X       := Round(Normal.X * 127);
            D36.Data3[V1].Y       := Round(Normal.Y * 127);
            D36.Data3[V1].Z       := Round(Normal.Z * 127);

            // BGRA Color (tint/transparency)

            If P.HasColor Then
            Begin
              // EQ uses BGRA while we use RGBA, so load the color components individually

              C               := P.Colors[J];
              D36.Data4[V1].R := TRGBA(C).R;
              D36.Data4[V1].G := TRGBA(C).G;
              D36.Data4[V1].B := TRGBA(C).B;
              D36.Data4[V1].A := TRGBA(C).A;
            End
            Else
            Begin
              If Zone.ZoneType = ztOutdoor
               Then D36.Data4[V1].Color := $FF000000
               Else D36.Data4[V1].Color := $00000000;
            End;

            // Write the correct vertex index

            D36.Data5[I].Vertex[J] := V1;
          End; // For J

          // The number of polygons and number of vertices arrays are sorted by texture.
          // If we have already processed a polygon with this texture, then increment
          // the number of polygons found and increase the vertex count. Otherwise make
          // new entries for this texture.

          J := FindTextureInSprite(P,D36);
          If J >= 0 Then
          Begin
            Inc(D36.Data7[J And $FFFF].NumPolys);
            Inc(D36.Data8[J Shr 16].NumVertices,High(P.Vertices) + 1);
          End
          Else
          Begin
            Texture := P.Texture;
            If P.TextureState = tsTransparent Then Texture := TransparentTexture
            Else
            Begin
              Special := 0;
              If P.TextureState = tsSemiTransparent Then Special := Special Or spSemiTransparent; //Texture := Texture + #241;
              If P.Masked Then Special := Special Or spMasked;
              If Special <> 0 Then Texture := Texture + Chr(SpecialTexture + Special);
            End;
            J := TexList.IndexOf(Texture);
            If J >= 0 Then
            Begin
              Inc(D36.Size7);
              Inc(D36.Size8);
              SetLength(D36.Data7,D36.Size7);
              SetLength(D36.Data8,D36.Size8);
              D36.Data7[D36.Size7 - 1].NumPolys    := 1;
              D36.Data7[D36.Size7 - 1].Data        := J;
              D36.Data8[D36.Size8 - 1].NumVertices := High(P.Vertices) + 1;
              D36.Data8[D36.Size8 - 1].Data        := J;
            End;
          End;
        End; // For I

        // Calculate the region center, trying to keep roundoff error down

        ARegion.GetCenterOfVolumeAndMaxDistance(Center,MaxDist);
        CX := Round(Center.X);
        CY := Round(Center.Y);
        CZ := Round(Center.Z);

        D36.Params1[0] := CX;
        D36.Params1[1] := CY;
        D36.Params1[2] := CZ;
        D36.Radius     := MaxDist;

        D36.Size10 := 15;
        While (D36.Size10 > 0) And ((1 Shl D36.Size10) > Trunc(32767 / D36.Radius)) Do Dec(D36.Size10);
(*
        // Now handle lighting

        Ray       := T3DPoint.Create;
        Normal    := T3DPoint.Create;
//        PCenter   := T3DPoint.Create;
        ToCenter  := T3DPoint.Create;
        Vertex    := T3DPoint.Create;
        RSize     := T3DPoint.Create;
//        Intersect := T3DPoint.Create;
        NRegions := TStringList.Create;
        RSize.Copy(ARegion.MaxPt);
        RSize.Subtract(ARegion.MinPt);
        For K := 0 To Lights.Count - 1 Do
        Begin
          Light    := TLightObject(Lights.Objects[K]);
          If Light.Loc.DistanceFrom(Center) <= Abs(Light.Radius) + MaxDist Then
          Begin

            // See if there are any solid polygons in the way

            NRegions.Clear;
            GetNearRegions(NRegions,Tree.Root,Light.Loc,RSize.GetLength);

            For I := 0 To High(ARegion.Polygons) Do
            Begin
              P := TPolygon(Mesh.Polygons.Objects[ARegion.Polygons[TexIndex[I]]]);
              If Not P.HasColor Then
              Begin
                // Find the center of the polygon, using the average of its vertices (really its center of gravity)

                PCenter := P.GetCenter(Mesh);

                For J := 0 To High(P.Vertices) Do
                Begin
                  L  := P.Vertices[J];
                  Vertex.Copy(T3DPoint(MeshVerts.Objects[L]));
                  Mesh.MakeAbsolute(Vertex);

                  // We're going to check to see if any polygons are in the way, but we're really interested in
                  // whether the light can illuminate the polygon, not merely its vertex (which is at the polygon's edge).
                  // I've found that roundoff error can cause polygons to be lit when they're actually obscured, so
                  // we'll choose a point that's a short distance from the vertex as our target point instead.

                  ToCenter.Copy(PCenter);
                  ToCenter.Subtract(Vertex);
                  ToCenter.Multiply(0.05); // Move towards the polygon's center by 5 percent of the distance
                  Vertex.Add(ToCenter);

                  // Only check if the polygon is within range of the light source

                  If Vertex.DistanceFrom(Light.Loc) <= Abs(Light.Radius) Then
                  Begin
                    // Draw a ray from the light source to the vertex we're going to test and make sure that the polygon
                    // we're testing doesn't face away from the light source

                    Ray.Copy(Light.Loc);
                    Ray.Subtract(Vertex);
                    Normal.Copy(T3DPoint(Mesh.Normals.Objects[L]));
                    Dot := Ray.Dot(Normal);

                    // Only check if the light is pointing toward the polygon

                    If Dot > 0 Then
                    Begin
                      // Check all regions that are near enough to possibly be in the way of the light ray
                      // for obstructing polygons

                      B          := 0;
                      Found      := False;
                      LightColor := Light.Color;
                      While (B < NRegions.Count) And Not Found Do
                      Begin
                        Reg := TRegion(NRegions.Objects[B]);
                        A   := 0;
                        While (A <= High(Reg.Polygons)) And Not Found Do
                        Begin
                          P1 := TPolygon(Mesh.Polygons.Objects[Reg.Polygons[A]]);
                          If (P1.TextureState <> tsTransparent) And (P1 <> P) Then
                          Begin
                            If (P1.Solid Or Not P1.HasSolid) And (High(P1.Vertices) = 2) Then
                            Begin
                              If P1.IntersectsSegment(Light.Loc,Vertex,Mesh,Intersect) Then
{
                              If LineFacet(Light.Loc,Vertex,
                                           T3DPoint(MeshVerts.Objects[P1.Vertices[0]]),
                                           T3DPoint(MeshVerts.Objects[P1.Vertices[1]]),
                                           T3DPoint(MeshVerts.Objects[P1.Vertices[2]]),Intersect) Then
}
                              Begin
                                If Not Intersect.Equals(Vertex) Then
                                Begin
                                  If (P1.TextureState = tsSemiTransparent) And (P1.TextureRef <> Nil) Then
                                  Begin
                                    // Semitransparent polys affect the light color but let the light continue

                                    CR := TRGBA(LightColor).R - (255 - TRGBA(P1.TextureRef.AvgColor).R);
                                    CG := TRGBA(LightColor).G - (255 - TRGBA(P1.TextureRef.AvgColor).G);
                                    CB := TRGBA(LightColor).B - (255 - TRGBA(P1.TextureRef.AvgColor).B);
                                    If P1.HasColor Then
                                    Begin
                                      // Just look at the first color since this is expensive to do

                                      CR := TRGBA(LightColor).R - (255 - TRGBA(P1.Colors[0]).R);
                                      CG := TRGBA(LightColor).G - (255 - TRGBA(P1.Colors[0]).G);
                                      CB := TRGBA(LightColor).B - (255 - TRGBA(P1.Colors[0]).B);
                                    End;
                                    If CR < 0 Then CR := 0;
                                    If CG < 0 Then CG := 0;
                                    If CB < 0 Then CB := 0;
                                    TRGBA(LightColor).R := CR;
                                    TRGBA(LightColor).G := CG;
                                    TRGBA(LightColor).B := CB;
                                  End
                                  Else Found := True;
                                End;
                                Intersect.Free;
                              End;
                            End;
                          End;
                          Inc(A);
                        End; // While
                        Inc(B);
                      End; // While

                      // If no polygons were found that block the light then illuminate the polygon

                      If Not Found Then
                      Begin
                        // Light intensity is a function of 1/r^2

                        Intensity := 1 / Sqr((Ray.GetLength * (4 / Abs(Light.Radius))) + 1);

                        // Make the dot product (which represents the angle of incidence with the
                        // polygon) nonlinear.  We're doing this for two reasons: 1) We can only shade
                        // at vertices, which proabably isn't the closest point of the polygon, and
                        // 2) rougher surfaces will scatter light more.

                        Intensity := Sqrt(Intensity);

                        // Form a vertex index to make life easier

                        V1 := I * 3 + J;

                        // Blue and red are swapped in the .WLD file

                        If P.HasColor
                         Then L := TRGBA(P.Colors[J]).B + Round(TRGBA(LightColor).B * Dot * Intensity)
                         Else L :=                        Round(TRGBA(LightColor).B * Dot * Intensity);
                        If L > 255 Then L := 255;
                        TRGBA(P.Colors[J]).B := L;

                        If P.HasColor
                         Then L := TRGBA(P.Colors[J]).G + Round(TRGBA(LightColor).G * Dot * Intensity)
                         Else L :=                        Round(TRGBA(LightColor).G * Dot * Intensity);
                        If L > 255 Then L := 255;
                        TRGBA(P.Colors[J]).G := L;

                        If P.HasColor
                         Then L := TRGBA(P.Colors[J]).R + Round(TRGBA(LightColor).R * Dot * Intensity)
                         Else L :=                        Round(TRGBA(LightColor).R * Dot * Intensity);
                        If L > 255 Then L := 255;
                        TRGBA(P.Colors[J]).R := L;

                        P.HasColor := True;
                      End;
                    End;
                  End;
                End; // For J
                PCenter.Free;
              End;
            End; // For I
          End;
        End; // For K
//        Intersect.Free;
        NRegions.Free;
        RSize.Free;
        Vertex.Free;
        ToCenter.Free;
//        PCenter.Free;
        Normal.Free;
        Ray.Free;


        For I := 0 To High(ARegion.Polygons) Do
        Begin
          P := TPolygon(Mesh.Polygons.Objects[ARegion.Polygons[TexIndex[I]]]);
          If P.HasColor Then
          Begin
            For J := 0 To High(P.Vertices) Do
            Begin
              V1 := I * 3 + J;

              // Blue and red are swapped in the .WLD file

              TRGBA(D36.Data4[V1]).B := TRGBA(P.Colors[J]).R;
              TRGBA(D36.Data4[V1]).G := TRGBA(P.Colors[J]).G;
              TRGBA(D36.Data4[V1]).R := TRGBA(P.Colors[J]).B;
            End; // For J
          End;
        End; // For I
*)


        // Now set the actual vertex values, trying to keep roundoff error down

        K := 1 Shl D36.Size10;
        V := T3DPoint.Create;
        For J := 0 To D36.Size1 - 1 Do
        Begin
          V.Copy(T3DPoint(MeshVerts.Objects[Vertices[J]]));
          Mesh.MakeAbsolute(V);
          VX := V.X;
          VY := V.Y;
          VZ := V.Z;
          VX := (VX - CX) * K;
          VY := (VY - CY) * K;
          VZ := (VZ - CZ) * K;
          D36.Data1[J].X := Round(VX);
          D36.Data1[J].Y := Round(VY);
          D36.Data1[J].Z := Round(VZ);
        End; // For J
        V.Free;
      End;
      Sprites.AddObject('',D36);

      // Fill in the tree entry (it's already been preallocated)

      I := D21Index;
      D21.Data1[I].NodeIndex := Sprites.Count;
      D21.Data1[I].NormalX   := 0;
      D21.Data1[I].NormalY   := 0;
      D21.Data1[I].NormalZ   := 0;
      D21.Data1[I].Distance  := 0;
      D21.Data1[I].Child[0]  := 0;
      D21.Data1[I].Child[1]  := 0;

      // Create the region reference for the region we created

      St  := IntToStr(Sprites.Count);
      While Length(St) < 6 Do St := '0' + St;
{
      If St = '001541' Then
       ShowMessage('test');
}
      Regions[Sprites.Count - 1] := Data22.Create(W,'R' + St);
      D22        := Regions[Sprites.Count - 1];
      If D36 <> Nil
       Then D22.Flags := $181
       Else D22.Flags := $81;
      D22.Fragment1 := FragmentReference.Create;
      D22.Fragment2 := FragmentReference.Create;
      If D36 <> Nil Then
      Begin
        D22.Fragment3 := FragmentReference.Create;
        D22.Fragment3.Fragment := D36;
      End;
      D22.Params1[0] := 0;
      D22.Params2[0] := 0;
      ARegion.BoundMesh.GetCenterOfVolumeAndMaxDistance(Center,MaxDist);

      D22.Params3[0] := Center.X;
      D22.Params3[1] := Center.Y;
      D22.Params3[2] := Center.Z;
      D22.Params3[3] := MaxDist;
      D22.Params5[0] := 0;
      D22.Params6[0] := 0;
      D22.Size1 := 0;
      D22.Size2 := 0;
      D22.Size3 := 0;
      D22.Size4 := 0;
      D22.Size5 := 1;
      D22.Size6 := 1;
      D22.Size7 := 0;
      SetLength(D22.Data5,D22.Size5);
      D22.Data5[0].Params1[0] := 0;
      D22.Data5[0].Params1[1] := 0;
      D22.Data5[0].Params1[2] := 0;
      D22.Data5[0].Params2[0] := 0;
      D22.Data5[0].Params3[0] := 1;
      D22.Data5[0].Params4[0] := 0;
      D22.Data5[0].Params5[0] := 0;
      SetLength(D22.Data6,D22.Size6);

      // This is apparently an encoded list of regions that's needed for mobs to fall or
      // otherwise vertically position themselves (such as on slopes).  My guess is it's a list
      // of all regions above and below the current one (a column), or maybe a list of all regions
      // that border the current one (or both).

      // More experimentation shows that it's most likely a list of regions within a certain radius--when
      // the player enters that group of regions, the mobs in the pertinent region reposition themselves.
      // It seems to tell the client that the player is in a region's vicinity, and therefore mobs in it
      // might be visible.  Due to the the fact that regions are stored in a tree, forming this list is
      // expensive--hence calculating it in advance.

      // Researching how BSP trees are used tells me that this is most likely the PVS (potentially
      // visible set) of nodes that are visibile from the given node. Everything I'm reading says that
      // it has to be figured out in advance, due to the CPU-intensive nature of doing so.

      // Get the extents of this region

      ARegion.BoundMesh.GetBounds(MinPt,MaxPt);

      // Get a list of all regions within a certain distance of this one (using a cube instead of a sphere,
      // while returning a longer region list, is faster and simpler).  Like the TexIndex array above,
      // TagList has some slack; it will grow if necessary, but not shrink to save CPU time.  TagCount
      // returns the actual number of valid tags in the list.

      MinPt.Subtract(Cube,Cube,Cube);
      MaxPt.Add(Cube,Cube,Cube);
      TagCount := 0; // Important!!!
      Tree.Root.GetTagsOfRegionsInVolume(TagList,TagCount,MinPt,MaxPt);

      // Sort the region list in ascending order

      TagSorter          := TQuickSorterProc.Create;
      TagSorter.Compare  := @CompareTags;
      TagSorter.Exchange := @ExchangeTags;
      TagSorter.Sort(0,TagCount - 1);
      TagSorter.Free;

//      QuickSortTagList(0,TagCount - 1);

      // Form the encoded region list -- pass 1: determine how big the list should be

      D22.Data6[0].Size1 := 0;
      I := 0;
      J := 0;
      K := 0;
      While I < TagCount Do
      Begin
        // The list is run-length encoded.  There appear to be several modes in which the data can be written.
        // (I've figured most of it out; this technique might not be the most space efficient but it's easiest to code)

        // Skip inaccessible regions

        If TagList[I] > J Then
        Begin
          // $40..$BF provide better compression but aren't strictly necessary

          If TagList[I] - J < $3F
           Then Inc(K)
           Else Inc(K,3);
        End;

        // Write a block of accessible ones

        L := I;                                                                    // Remember our starting point
        While (I < TagCount - 1) And (TagList[I + 1] = TagList[I] + 1) Do Inc(I);  // Gather all contiguous tags together
        L := (I - L) + 1;                                                          // The amount of indices (tags) to write
        J := TagList[I] + 1;                                                       // This is what we expect to write once we're done
        If L < $3F
         Then Inc(K)                                                               // One block for them all ("one block to bind them...")
         Else Inc(K,3);                                                            // One block for them all ("one block to bind them...")

        // Rinse and repeat

        Inc(I); // Move to the next tag
      End; // While

      // Form the encoded region list -- pass 2: write the list

      D22.Data6[0].Size1 := K;
      SetLength(D22.Data6[0].Union.Data1_B,D22.Data6[0].Size1);
      I := 0;
      J := 0;
      K := 0;
      While I < TagCount Do
      Begin
        // The list is run-length encoded.  There appear to be several modes in which the data can be written.
        // (I've figured most of it out; this technique might not be the most space efficient but it's easiest to code)

        // Skip inaccessible regions

        If TagList[I] > J Then
        Begin
          // $40..$BF provide better compression but aren't strictly necessary

          If TagList[I] - J < $3F Then
          Begin
            Inc(K);
            D22.Data6[0].Union.Data1_B[K - 1] := TagList[I] - J;    // $00..$3E: Skip <amount>
          End
          Else
          Begin
            Inc(K,3);
            L := (TagList[I] - J) And $FF;
            H := (TagList[I] - J) Shr 8;
            D22.Data6[0].Union.Data1_B[K - 3] := $3F;                              // $3F is "special": skip by word count
            D22.Data6[0].Union.Data1_B[K - 2] := L;                                // Failsafe: sometimes Delphi does weird things with type conversion
            D22.Data6[0].Union.Data1_B[K - 1] := H;
          End;
        End;

        // Write a block of accessible ones

        L := I;                                                                    // Remember our starting point
        While (I < TagCount - 1) And (TagList[I + 1] = TagList[I] + 1) Do Inc(I);  // Gather all contiguous tags together
        L := (I - L) + 1;                                                          // The amount of indices (tags) to write
        J := TagList[I] + 1;                                                       // This is what we expect to write once we're done
        If L < $3F Then
        Begin
          Inc(K);                                                                  // One block for them all ("one block to bind them...")
          D22.Data6[0].Union.Data1_B[K - 1] := L + $C0;                            // $C0..$FE: Write (<amount> And $3F) region indices
        End
        Else
        Begin
          Inc(K,3);                                                                // One block for them all ("one block to bind them...")
          H := L Shr 8;                                                            // High byte
          L := L And $FF;                                                          // Low byte
          D22.Data6[0].Union.Data1_B[K - 3] := $FF;                                // $FF is "special": write a block by word count
          D22.Data6[0].Union.Data1_B[K - 2] := L;                                  // Failsafe: sometimes Delphi does weird things with type conversion
          D22.Data6[0].Union.Data1_B[K - 1] := H;
        End;

        // Rinse and repeat

        Inc(I); // Move to the next tag
      End; // While
    End
    Else
    Begin
      // Fill in the tree entry (it's already been preallocated)

      I                      := D21Index;
      D21.Data1[I].NodeIndex := 0;
      D21.Data1[I].NormalX   := ARegion.SplitNorm.X;
      D21.Data1[I].NormalY   := ARegion.SplitNorm.Y;
      D21.Data1[I].NormalZ   := ARegion.SplitNorm.Z;
      D21.Data1[I].Distance  := ARegion.SplitDist;
      D21.Data1[I].Child[0]  := 0;
      D21.Data1[I].Child[1]  := 0;

      If ARegion.Left <> Nil Then
      Begin
        // Preallocate a tree entry and process the region

        Inc(D21Index);
        D21.Data1[I].Child[1] := D21Index + 1;
        ProcessRegion(ARegion.Left);
      End;
      If ARegion.Right <> Nil Then
      Begin
        // Preallocate a tree entry and process the region

        Inc(D21Index);
        D21.Data1[I].Child[0] := D21Index + 1;
        ProcessRegion(ARegion.Right);
      End;
    End;
  End; // ProcessRegion

begin
  OutTexCount := 0;
  frmStatus.Show;
//  SemiTrans := TStringList.Create;
  TexList        := TStringList.Create;

  // We need to regenerate the model

  frmStatus.SetCaption('Creating polygon mesh');
  If ZoneMesh = Nil Then
  Begin
    Mesh := Zone.BuildPolygonList(True,False,False,False);
    List := TStringList.Create;
    List.AddObject('',Mesh);
    ReloadModel(List,True);
    List.Free;
  End
  Else Mesh := ZoneMesh;

  W := TEQWldData.Create;

  W.nFragment := 1;
  SetLength(W.Fragments,1);
  W.Fragments[0] := Nil;
  D35 := EQWldDataFragment.Create(W,$35,'');
  AddFragment(W,D35);

  St := UpperCase(ExtractFileName(FileName));
  St := Copy(St,1,Length(St) - Length(ExtractFileExt(FileName)));

  D31 := Data31.Create(W,St + '_MP');
  D31.Size1 := 0;
  SetLength(D31.Data1,0);

//  FirstData03 := Nil;

  // Before exporting, we need to make a comprehensive list of all textures that we're using

  // Create an entry for each texture we use, except water and transparent

  frmStatus.SetCaption('Creating polygon entries');
  For I := 0 To Mesh.Polygons.Count - 1 Do
  Begin
    frmStatus.SetPosition(I / Mesh.Polygons.Count);
    P  := TPolygon(Mesh.Polygons.Objects[I]);
    St := P.Texture;
    If (P.TextureState <> tsTransparent) Then
    Begin
      Special := 0;
      If P.TextureState = tsSemiTransparent Then Special := Special Or spSemiTransparent;
      If P.Masked Then Special := Special Or spMasked;
      If Special = 0 Then
      Begin
        If TexList.IndexOf(St) < 0 Then
        Begin
          MakeTextureEntry(W,D31,St,'',False,False);
          TexList.Add(St);
        End;
      End
      Else If (Special And spSemiTransparent) <> 0 Then
      Begin
        If (Special And spMasked) = 0 Then
        Begin
          St2 := St + Chr(SpecialTexture + spSemiTransparent);
          If TexList.IndexOf(St2) < 0 Then
          Begin
            MakeTextureEntry(W,D31,St,'',True,False);
            TexList.Add(St2);
          End;
        End
        Else
        Begin
          St2 := St + Chr(SpecialTexture + spSemiTransparent + spMasked);
          If TexList.IndexOf(St2) < 0 Then
          Begin
            MakeTextureEntry(W,D31,St,'',True,True);
            TexList.Add(St2);
          End;
        End;
      End
      Else If (Special And spMasked) <> 0 Then
      Begin
        St2 := St + Chr(SpecialTexture + spMasked);
        If TexList.IndexOf(St2) < 0 Then
        Begin
          MakeTextureEntry(W,D31,St,'',False,True);
          TexList.Add(St2);
        End;
      End;
    End;
  End; // For I

  // Now create a transparent entry

  TexList.Add(TransparentTexture);
  MakeTransparentTextureEntry(W,D31);

  AddFragment(W,D31);

  // Prepare to make Data36 structures from each region (all polygons must be triangles)

  Sprites := TStringList.Create;

  // Create a tree fragment with one enpty tree entry

  D21 := Data21.Create(W);
  D21.Size1 := Tree.Root.CountNodes;
  SetLength(D21.Data1,D21.Size1);
  D21Index := 0;

  // The polygons have already been broken into regions.  Create the region tree and regions

  frmStatus.SetCaption('Creating region tree and regions');
  Center      := T3DPoint.Create;
  MinPt       := T3DPoint.Create;
  MaxPt       := T3DPoint.Create;
  MaxVertDist := T3DPoint.Create;
  SetLength(Regions,Tree.Root.CountLeafNodes);
  I := 0;
  Tree.Root.TagLeafNodes(I);
  SetLength(TagList,0);
  SetLength(TexIndex,0);

  LightTreeHash := TLightTreeHash.Create(Zone,Tree);

  Tree.Root.ShadePolygonsByLights(LightTreeHash);

  ProcessRegion(Tree.Root);
  SetLength(TagList,0);
  Center.Free;
  MinPt.Free;
  MaxPt.Free;
  MaxVertDist.Free;
  SetLength(TexIndex,0);
  SetLength(Vertices,0);

  // Default light def

  frmStatus.SetPosition(0);
  frmStatus.SetCaption('Creating default light structure');
  D1B          := Data1B.Create(W,'DEFAULT_LIGHTDEF');

  // Outdoor?

  D1B.Flags    := 4;
  D1B.Params2  := 1;
  D1B.Params3a := 1;

  // Indoor?
{
  D1B.Flags      := $14;
  D1B.Params2    := 1;
  D1B.Params4[0] := 1;
  D1B.Params4[1] := 1.1;
  D1B.Params4[2] := 1.1;
  D1B.Params4[3] := 1.1;
}
  // Data08: camera info

  frmStatus.SetCaption('Creating camera structure');
  D08 := DataWithName.Create(W,$08,'CAMERA_DUMMY');
  D08.Size := $68;
  SetLength(D08.Data1,D08.Size);
  For I := 0 To D08.Size - 1 Do D08.Data1[I] := 0;
  D08.Data1[$04] := 4;
  D08.Data1[$08] := 1;
  D08.Data1[$16] := $80; // -1.0
  D08.Data1[$17] := $BF;
  D08.Data1[$1A] := $80; // 1.0
  D08.Data1[$1B] := $3F;
  D08.Data1[$22] := $80; // 1.0
  D08.Data1[$23] := $3F;
  D08.Data1[$26] := $80; // 1.0
  D08.Data1[$27] := $3F;
  D08.Data1[$2E] := $80; // 1.0
  D08.Data1[$2F] := $3F;
  D08.Data1[$32] := $80; // -1.0
  D08.Data1[$33] := $BF;
  D08.Data1[$3A] := $80; // -1.0
  D08.Data1[$3B] := $BF;
  D08.Data1[$3E] := $80; // -1.0
  D08.Data1[$3F] := $BF;
  D08.Data1[$40] := 4;
  D08.Data1[$50] := 1;
  D08.Data1[$54] := 2;
  D08.Data1[$58] := 3;
  D08.Data1[$60] := 1;
  D08.Data1[$64] := $B;

  // Data1C has an unknown purpose

  D1C := Data1C.Create(W);
  D1C.Fragment := FragmentReference.Create;
  D1C.Fragment.Fragment := D1B;

  // Mode ambient light info

  frmStatus.SetCaption('Creating default ambient light structure');
  D2A := Data2A.Create(W,'DEFAULT_AMBIENTLIGHT');
  D2A.Fragment := FragmentReference.Create;
  D2A.Fragment.Fragment := D1C;
  D2A.Flags := 0;
  D2A.Size1 := High(Regions) + 1;
  SetLength(D2A.Data1,D2A.Size1);

  For I := 0 To High(D2A.Data1) Do D2A.Data1[I] := I;

  // Data09 has an unknown purpose

  D09                   := Data09.Create(W);
  D09.Fragment          := FragmentReference.Create;
  D09.Fragment.Fragment := D08;
  D09.Flags             := 0;

  // Player info

  D14 := Data14.Create(W,'PLAYER_1');
  D14.Flags := 0;
  D14.Fragment1 := FragmentReference.Create;
  D14.Fragment1.Fragment := Nil;
  D14.Fragment1.Name     := '(FLYCAMCALLBACK)';
//  D14.Fragment2 := FragmentReference.Create;
  D14.Params1[0] := 0;
  D14.Params2[0] := 0;
  D14.Params2[1] := 0;
  D14.Params2[2] := 0;
  D14.Params2[3] := 0;
  D14.Params2[4] := 0;
  D14.Params2[5] := 0;
  D14.Params2[6] := 0;
  D14.Size1 := 1;
  SetLength(D14.Data1,D14.Size1);
  D14.Data1[0].Size := 1;
  SetLength(D14.Data1[0].Data,D14.Data1[0].Size);
  D14.Data1[0].Data[0].Data1 := 0;
  D14.Data1[0].Data[0].Data2 := 1E30;
  D14.Size2 := 1;
  SetLength(D14.Data2,D14.Size2);
  D14.Data2[0] := FragmentReference.Create;
  D14.Data2[0].Fragment := D09;
  D14.Size3 := 0;

  // Data16 has an unknown purpose

  D16 := DataWithName.Create(W,$16);
  D16.Size := 4;
  SetLength(D16.Data1,D16.Size);
  D16.Data1[0] := $CD;
  D16.Data1[1] := $CC;
  D16.Data1[2] := $CC;
  D16.Data1[3] := $3D;

  // Data15 has an unknown purpose

  D15 := Data15.Create(W);
  D15.Fragment := FragmentReference.Create;
  D15.Fragment.Fragment := D14;
  D15.Fragment1 := FragmentReference.Create;
  D15.Fragment1.Fragment := D16;
  D15.Flags      := $2E;
  D15.X          := ZoneMinX;
  D15.Y          := ZoneMinY;
  D15.Z          := ZoneMaxZ;
  D15.RotateZ    := 0;
  D15.RotateY    := 0;
  D15.RotateX    := 0;
  D15.Params1    := 0;
  D15.ScaleY     := 0.5;
  D15.ScaleX     := 0.5;
  D15.Fragment2  := FragmentReference.Create;
  D15.Fragment2.Position := 0; 
  D15.Params2    := 30;//0;

  // Define the water regions

  D29Water := Nil;
  D29Lava  := Nil;
  D29PvP   := Nil;
  If High(Zone.Water) >= 0 Then
  Begin
    // First do water areas

    If WaterAreaCount > 0 Then
    Begin
      // This string is "magic": the client apparently looks for it

      D29Water       := Data29.Create(W,'WT_ZONE');
      D29Water.Flags := 0;
      D29Water.Size2 := 0;

      frmStatus.SetPosition(0);
      frmStatus.SetCaption('Defining water regions');

      // For every water area, find out how many regions are within it and add them

      D29Water.Size1 := Tree.Root.CountLeafNodesWithAttrs([raWater]);
      SetLength(D29Water.Data1,D29Water.Size1);
      D29Index := 0;
      D29Count := 0;
      If D29Water.Size1 <> 0 Then AddWaterRegion(Tree.Root);
    End;

    // Now do lava areas

    If LavaAreaCount > 0 Then
    Begin
      // This string is "magic": the client apparently looks for it

      D29Lava       := Data29.Create(W,'LA_ZONE');
      D29Lava.Flags := 0;
      D29Lava.Size2 := 0;

      frmStatus.SetPosition(0);
      frmStatus.SetCaption('Defining lava regions');

      // For every lava area, find out how many regions are within it and add them

      D29Lava.Size1 := Tree.Root.CountLeafNodesWithAttrs([raLava]);
      SetLength(D29Lava.Data1,D29Lava.Size1);
      D29Index := 0;
      D29Count := 0;
      If D29Lava.Size1 <> 0 Then AddLavaRegion(Tree.Root);
    End;

    // Now do PvP areas

    If PvPAreaCount > 0 Then
    Begin
      // This string is "magic": the client apparently looks for it

      D29PvP       := Data29.Create(W,'DRP_ZONE');
      D29PvP.Flags := 0;
      D29PvP.Size2 := 0;

      frmStatus.SetPosition(0);
      frmStatus.SetCaption('Defining PvP regions');

      // For every PvP area, find out how many regions are within it and add them

      D29PvP.Size1 := Tree.Root.CountLeafNodesWithAttrs([raPvP]);
      SetLength(D29PvP.Data1,D29PvP.Size1);
      D29Index := 0;
      D29Count := 0;
      If D29PvP.Size1 <> 0 Then AddPvPRegion(Tree.Root);
    End;

    // Now do ice areas

    If IceAreaCount > 0 Then
    Begin
      // This string is "magic": the client apparently looks for it

      D29Water       := Data29.Create(W,Format('DRN__%.5d000000000000000000000_S_000000000000',[D29Count]));
      D29Water.Flags := 0;
      D29Water.Size2 := 0;

      frmStatus.SetPosition(0);
      frmStatus.SetCaption('Defining ice regions');

      // For every ice area, find out how many regions are within it and add them

      D29Water.Size1 := Tree.Root.CountLeafNodesWithAttrs([raIce]);
      SetLength(D29Water.Data1,D29Water.Size1);
      D29Index := 0;
      D29Count := 0;
      If D29Water.Size1 <> 0 Then AddWaterRegion(Tree.Root);
    End;

    // Now do ice water areas

    If IceWaterAreaCount > 0 Then
    Begin
      // This string is "magic": the client apparently looks for it

      D29Water       := Data29.Create(W,Format('DRN__%.5d000000000000000000000___000000000000',[D29Count]));
      D29Water.Flags := 0;
      D29Water.Size2 := 0;

      frmStatus.SetPosition(0);
      frmStatus.SetCaption('Defining ice water regions');

      // For every ice area, find out how many regions are within it and add them

      D29Water.Size1 := Tree.Root.CountLeafNodesWithAttrs([raIceWater]);
      SetLength(D29Water.Data1,D29Water.Size1);
      D29Index := 0;
      D29Count := 0;
      If D29Water.Size1 <> 0 Then AddWaterRegion(Tree.Root);
    End;
  End;

  // Define the zone line regions

  SetLength(D29ZoneLines,Zone.ZonePlanes.Count);
  For I := 0 To Zone.ZonePlanes.Count - 1 Do
  Begin
{
    // Old way?

    // We first have to construct the name of the fragment.  The name tells the client the
    // destination zone ID, destination X, Y, and Z coordinates, and the destination heading.
    // For any values that are ignored the field has to be filled in with all "9"'s.

    // Destination zone ID is 5 digits

    St := Format('%.5u',[Zone.ZonePlanes[I].DestZoneID]);

    // Destination zone X, Y, and Z coordinates are 6 digits

    If Zone.ZonePlanes[I].HasDestX Then
    Begin
      St1 := Copy(Format('%8.6f',[Zone.ZonePlanes[I].DestX]),1,6);
      If St1[6] = '.' Then
      Begin
        If St1[1] = '-'
         Then St1 := '-0' + Copy(St1,2,4)
         Else St1 := '0' + Copy(St1,1,5);
      End;
      St := St + St1;
    End
    Else St := St + '999999';

    If Zone.ZonePlanes[I].HasDestY Then
    Begin
      St1 := Copy(Format('%8.6f',[Zone.ZonePlanes[I].DestY]),1,6);
      If St1[6] = '.' Then
      Begin
        If St1[1] = '-'
         Then St1 := '-0' + Copy(St1,2,4)
         Else St1 := '0' + Copy(St1,1,5);
      End;
      St := St + St1;
    End
    Else St := St + '999999';

    If Zone.ZonePlanes[I].HasDestZ Then
    Begin
      St1 := Copy(Format('%8.6f',[Zone.ZonePlanes[I].DestZ]),1,6);
      If St1[6] = '.' Then
      Begin
        If St1[1] = '-'
         Then St1 := '-0' + Copy(St1,2,4)
         Else St1 := '0' + Copy(St1,1,5);
      End;
      St := St + St1;
    End
    Else St := St + '999999';

    // Destination zone heading is 3 digits

    J := Zone.ZonePlanes[I].DestAngle;
    While J < 0 Do Inc(J,360);
    While J >= 360 Do Dec(J,360);
    If Zone.ZonePlanes[I].HasDestAngle
     Then St := St + Format('%.3u',[Round(J * 512 / 360)])
     Else St := St + '999';
}
    // New way? I guess 00255 means don't use the old way? Does this mean that zones
    // may not have 255 as their ID?

    St := '00255' + Format('%.6u',[I + 1]);

    D29ZoneLines[I] := Data29.Create(W,'DRNTP' + St + '_ZONE');   // Magic string
    D29ZoneLines[I].Flags := 0;
    D29ZoneLines[I].Size2 := 0;

    frmStatus.SetPosition(0);
    frmStatus.SetCaption('Defining zone line (' + IntToStr(I + 1) + ' of ' + IntToStr(Zone.ZonePlanes.Count) + ')');

    // For every zone line, find out how many regions are within it and add them

    J := I Shl 16;
    D29ZoneLines[I].Size1 := Tree.Root.CountLeafNodesWithAttrs([TRegionAttribute(Integer(raZoneLine) + J)]);
    SetLength(D29ZoneLines[I].Data1,D29ZoneLines[I].Size1);
    D29Index := 0;
    D29Count := 0;
    If D29ZoneLines[I].Size1 <> 0 Then AddZoneLineRegion(Tree.Root,I);
  End; // For I

  // Add the sprite structures

  frmStatus.SetCaption('Adding regions');
  For I := 0 To Sprites.Count - 1 Do
  Begin
    frmStatus.SetPosition(I / Sprites.Count);
    If Sprites.Objects[I] <> Nil Then AddFragment(W,Data36(Sprites.Objects[I]));
  End; // For I
  frmStatus.SetPosition(1);

  // Add the default light def

  AddFragment(W,D1B);

  // Add the camera info

  AddFragment(W,D08);

  // Add the tree

  AddFragment(W,D21);

  // Add the regions

  frmStatus.SetCaption('Adding region references');
  For I := 0 To High(Regions) Do
  Begin
    frmStatus.SetPosition(I / (High(Regions) + 1));
    AddFragment(W,Regions[I]);
  End; // For I
  frmStatus.SetPosition(1);

  // Add the Data1C struct

  AddFragment(W,D1C);

  // Add the Data2A light info

  AddFragment(W,D2A);

  // Add the Data09 struct

  AddFragment(W,D09);

  // Add the Data14 struct

  AddFragment(W,D14);

  // Add the Data16 struct

  AddFragment(W,D16);

  // Add the Data15 struct

  AddFragment(W,D15);

  // Add the D29 structs

  If D29Water <> Nil Then AddFragment(W,D29Water);
  If D29Lava  <> Nil Then AddFragment(W,D29Lava);
  If D29PvP   <> Nil Then AddFragment(W,D29PvP);
  For I := 0 To High(D29ZoneLines) Do AddFragment(W,D29ZoneLines[I]);

  // Write to the file

  frmStatus.SetPosition(0);
  frmStatus.SetCaption('Encoding output file');
  W.Encode(Stream);

  // Cleanup

  SetLength(D29ZoneLines,0);
  LightTreeHash.Free;
  Sprites.Clear;
  Sprites.Free;
  W.Free;
  If ZoneMesh = Nil Then Mesh.Free;
//  SemiTrans.Free;
  TexList.Free;

  ReloadZone;
  RenderZone(BirdsEye);
  glView.Fit(Not BirdsEye,True,False);
  SetViewToNorth;
  frmStatus.SetCaption('');
  frmStatus.Close;
  Application.ProcessMessages;
end; // TfrmMain.ExportToWLD

Procedure TfrmMain.LoadParameters;
Var
  ZO         : TZoneObject;
  Names      : String;
  Values     : String;
  RealValues : String;
  TN         : TTreeNode;

  Procedure ShowValues(SetSize: Boolean; Delim,St,S: String; Col: Integer);
  Var
    J   : Integer;
    St1 : String;
    S1  : String;

  Begin
    If SetSize Then
    Begin
      SetLength(ParameterNames,0);
      SetLength(ParameterValues,0);
    End;
    J := 0;
    While (St <> '') Or (S <> '') Do
    Begin
      St1 := GetToken(Delim,St);
      S1  := GetToken(',',S);
      If SetSize Then
      Begin
        SetLength(ParameterNames,J + 1);
        SetLength(ParameterValues,J + 1);
      End;
      If J <= High(ParameterNames) Then
      Begin
        If Col = 1 Then
        Begin
          If St1 = '' Then St1 := #240 + '(' + S1 + ')'; // The #240 is a trigger we will catch later
          ParameterValues[J] := St1;
        End
        Else ParameterNames[J] := St1;
      End;
      Inc(J);
    End; // While
    If SetSize Then
    Begin
      While J <= High(ParameterNames) Do
      Begin
        ParameterNames[J]  := '';
        ParameterValues[J] := '';
        Inc(J);
      End; // While
    End;
  End; // ShowValues

Begin
  LoadingParms := True;
  If tvMain.SelectionCount > 0 Then
  Begin
    TN := tvMain.Selections[0];
    ZO := TZoneObject(TN.Data);
    If ZO <> Nil Then
    Begin
      ObjLoc.Copy(ZO.Loc);
      ObjRotate.Copy(ZO.Rotate);
      ObjSize.Copy(ZO.Size);
      If ZO Is TScriptedObject Then
      Begin
        Names      := Trim(TScriptedObject(ZO).GetParameterNamesAndValues(Values,RealValues));
        Values     := Trim(Values);
        RealValues := Trim(RealValues);
        ShowValues(True,' ',Names,RealValues,0);
        ShowValues(False,',',Values,RealValues,1);
      End;
    End;
  End
  Else
  Begin
    Names      := '';
    Values     := '';
    RealValues := '';
    ShowValues(True,' ',Names,RealValues,0);
    ShowValues(False,',',Values,RealValues,1);
  End;
  LoadingParms := False;
End; // TfrmMain.LoadParameters

Function TfrmMain.GetGridRect(ACol,ARow: Integer; SG: TStringGrid): TRect;
Var R: TRect;
Begin
  If ACol > 0               Then R.Left  := SG.ColWidths[ACol - 1] Else R.Left := 0;
  If ACol < SG.ColCount - 1 Then R.Right := R.Left + SG.ColWidths[ACol] Else R.Right := SG.Width;
  R.Top    := ARow * SG.DefaultRowHeight;
  R.Bottom := R.Top + SG.DefaultRowHeight;
  Result := R;
End; // TfrmMain.GetGridRect

procedure TfrmMain.FormResize(Sender: TObject);
begin
  tbPalette.Width := ToolBar2.Width;
end;

Procedure TfrmMain.RefreshObject(ZO: TZoneObject; RedrawScene,SaveAndRestore,HandleSelected: Boolean);
Var
  SO      : TScriptedObject;
  I,J     : Integer;
  P       : Array[0..2] Of Single;
  S       : Array[0..2] Of Single;
  R       : Array[0..2] Of Single;
  St      : String;
  L1      : TStringList;
  E       : TEntity;
  TexPath : String;
  Entity  : TEntity;

begin
  If Zone.GetObjectIndex(ZO) >= 0 Then
  Begin
    If ZO Is TScriptedObject Then
    Begin
      SO := TScriptedObject(ZO);
      SO.ClearParameters;
      For I := 0 To High(ParameterValues) Do
      Begin
        St := Trim(ParameterValues[I]);
        If Copy(St,1,1) = #240 Then St := '';
        If St <> '' Then SO.AddParameter(UpperCase(ParameterNames[I]) + ' ' + St);
      End; // For I
    End;

    // Save the position and rotation

    If SaveAndRestore Then
    Begin
      If BirdsEye Then
      Begin
        If glView.Scene3D.Scene.Entities.Count > FirstEntity Then
        Begin
          P[0] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Position.X;
          P[1] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Position.Y;
          P[2] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Position.Z;
          S[0] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Scale.X;
          S[1] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Scale.Y;
          S[2] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Scale.Z;
          R[0] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Rotation.X;
          R[1] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Rotation.Y;
          R[2] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Rotation.Z;
        End;
      End
      Else
      Begin
        P[0] := glView.Scene3D.Scene.DefaultCamera.Position.X;
        P[1] := glView.Scene3D.Scene.DefaultCamera.Position.Y;
        P[2] := glView.Scene3D.Scene.DefaultCamera.Position.Z;
        S[0] := glView.Scene3D.Scene.DefaultCamera.SceneCenter.X;
        S[1] := glView.Scene3D.Scene.DefaultCamera.SceneCenter.Y;
        S[2] := glView.Scene3D.Scene.DefaultCamera.SceneCenter.Z;
        R[0] := glView.Scene3D.Scene.DefaultCamera.UpVector.X;
        R[1] := glView.Scene3D.Scene.DefaultCamera.UpVector.Y;
        R[2] := glView.Scene3D.Scene.DefaultCamera.UpVector.Z;
      End;
    End;

    // Get a polygon list from the zone

    L1 := Zone.BuildPolygonLists(ZO);
//    L  := TStringList.Create;
    I  := Zone.GetObjectIndex(ZO);

    If ZO Is TCreatureLibraryObjectReference
     Then TexPath := ExtractFilePath(Application.ExeName) + 'library\creatures\'
     Else TexPath := ExtractFilePath(Application.ExeName) + 'library\textures\';

//    For J := 0 To L1.Count - 1 Do L.AddObject('',L1.Objects[J]);
    For J := 0 To L1.Count - 1 Do Entity := ReloadMesh(L1,I,I + J,False,True,True,glView,Tree,False,TexPath);

    If (ZO <> Nil) And (ZO Is TCreatureLibraryObjectReference) Then LoadCreatureSkeleton(TCreatureLibraryObjectReference(ZO).An8File,glView,Entity,ZO);

    If HandleSelected Then
    Begin
      // Regenerate the selection rectangles

      SetSelectionRectangles;
      AddModelOrigins;
      SetPolygonSelectionRectangles;
    End;

    // Reload dynamic light info for shaders

    LoadDynamicLights;

    // Render the zone

    If RedrawScene Then
    Begin
      AddLightSpheres;
      AddHotSpots;
      RenderZone(BirdsEye);
    End;
    If BirdsEye Then
    Begin
      For J := 0 To L1.Count - 1 Do glView.FitFrame(Not BirdsEye{False},True,False,I + J + FirstEntity,False);
    End;
    For J := 0 To L1.Count - 1 Do L1.Objects[J].Free;
    L1.Free;
//    L.Free;

    // Make sure the selection rectangles will display properly

    If HandleSelected Then
    Begin
      If BirdsEye Then
      Begin
        For J := 1 To FirstEntity - 1 Do glView.FitFrame(False,True,False,J,False);
      End;
    End;

    // Restore the position and rotation

    If SaveAndRestore Then
    Begin
      If BirdsEye Then
      Begin
        // FitFrame only changes "local" settings so we don't need to restore the normal settings,
        // and we don't want to either because animated objects have them set differently and
        // doing this will screw them up.      
{
        For J := 0 To glView.Scene3D.Scene.Entities.Count - 1 Do
        Begin
          E := TEntity(glView.Scene3D.Scene.Entities.Items[J]);
          E.Position.Copy(P[0],P[1],P[2]);
          E.Scale.Copy(S[0],S[1],S[2]);
          E.Rotation.Copy(R[0],R[1],R[2]);
        End; // For J
}        
      End
      Else
      Begin
        glView.Scene3D.Scene.DefaultCamera.SetPosition(P[0],P[1],P[2]);
        glView.Scene3D.Scene.DefaultCamera.LookAt(S[0],S[1],S[2]);
        glView.Scene3D.Scene.DefaultCamera.SetVectorUp(R[0],R[1],R[2]);
      End;
    End;
    If RedrawScene Then glView.Scene3D.Redraw;
  End;
End; // TfrmMain.RefreshObject

Procedure TfrmMain.UpdateParameter(Value: String; Element: Integer);
Var
  St : String;
  ZO : TZoneObject;

begin
  St := UpperCase(Trim(ParameterValues[Element]));
  If (tvMain.SelectionCount = 1)                         And
     (tvMain.Selections[0] <> Nil)                       And
     (tvMain.Selections[0].Data <> Nil)                  And
     (Element >= 0)                                      And
     (glView.Scene3D.Scene.Entities.Count > FirstEntity) And
     (UpperCase(Trim(Value)) <> St)                      And
     Not LoadingParms                                    Then
  Begin
    ParameterValues[Element] := Value;
    ZO := TZoneObject(tvMain.Selections[0].Data);
    ZO.Loc.Copy(ObjLoc);
    ZO.Rotate.Copy(ObjRotate);
    ZO.Size.Copy(ObjSize);
    RefreshObject(ZO,True,True,True);
    LoadParameters;
    Application.ProcessMessages;
  End;
End; // TfrmMain.UpdateParameter

Procedure TfrmMain.BuildScriptPalette;
Var
  L1,L2 : TStringList;
  I     : Integer;
  S     : TScripted;

Begin
  L1 := TStringList.Create;
  L2 := TStringList.Create;
  For I := 0 To ScriptLibrary.Count - 1 Do
  Begin
    S  := TScripted(ScriptLibrary.Objects[I]);
    If (L1.IndexOf(S.Category) < 0) And (UpperCase(S.Category) <> 'HIDDEN') Then
    Begin
      L1.Add(S.Category);
      L2.Add(Capitalize(S.Category));
    End;
  End; // For I
  tbPalette.Tabs.Clear;
  L2.Sort;
  tbPalette.Tabs.AddStrings(L2);
  L1.Free;
  L2.Free;
End; // TfrmMain.BuildScriptPalette

Procedure TfrmMain.LoadScriptBitmaps;
Var
  I,J,K : Integer;
  S     : TScripted;
  St    : String;
  BMP   : TBitmap;
  C     : Array[1..4] Of TColor;
  D     : Array[1..4] Of Integer;

Begin
  ilPalette.Clear;
  ilPalette.Width  := 16;
  ilPalette.Height := 16;
  For I := 0 To ScriptLibrary.Count - 1 Do
  Begin
    S   := TScripted(ScriptLibrary.Objects[I]);
    St  := ExtractFilePath(Application.ExeName) + 'library\scripts\' + S.Name + '.bmp';
    BMP := TBitmap.Create;
    If FileExists(St)
     Then BMP.LoadFromFile(St)
     Else BMP.LoadFromFile(ExtractFilePath(Application.ExeName) + 'generic.bmp');
    C[1] := BMP.Canvas.Pixels[0,0];
    C[2] := BMP.Canvas.Pixels[BMP.Width - 1,0];
    C[3] := BMP.Canvas.Pixels[0,BMP.Height - 1];
    C[4] := BMP.Canvas.Pixels[BMP.Width - 1,BMP.Height - 1];
    D[1] := 1;
    D[2] := 1;
    D[3] := 1;
    D[4] := 1;
    For J := 1 To 3 Do
     For K := J + 1 To 4 Do
     Begin
       If C[J] = C[K] Then
       Begin
         Inc(D[J]);
         Dec(D[K]);
       End;
     End; // For K
    For J := 2 To 4 Do If D[J] > D[1] Then C[1] := C[J];
    ilPalette.AddMasked(BMP,C[1]);
    BMP.Free;
  End; // For I
End; // TfrmMain.LoadScriptBitmaps

procedure TfrmMain.tbPaletteChange(Sender: TObject);
begin
  SetupPalette;
end;

Procedure TfrmMain.SetupPalette;
Var
  I,J : Integer;
  S   : TScripted;
  St  :  String;
  L   : TStringList;
  A   : TAction;
  CA  : TContainedAction;
  ACI : TActionClientItem;

Begin
  St := UpperCase(tbPalette.Tabs.Strings[tbPalette.TabIndex]);
  L  := TStringList.Create;
  For I := 0 To ScriptLibrary.Count - 1 Do
  Begin
    S := TScripted(ScriptLibrary.Objects[I]);
    If UpperCase(S.Category) = St Then L.Add(UpperCase(S.Name));
  End; // For I
  L.Sort;

  // Get rid of all the actions

  While ActionList1.ActionCount > 0 Do
  Begin
    CA := ActionList1.Actions[0];
    CA.ActionList := Nil;
    CA.Free;
  End; // For I

  // Make an action for each scripted object in the category

  For I := L.Count - 1 DownTo 0 Do
  Begin
    For J := 0 To ScriptLibrary.Count - 1 Do
    Begin
      S := TScripted(ScriptLibrary.Objects[J]);
      If L.Strings[I] = UpperCase(S.Name) Then
      Begin
        A            := TAction.Create(Self);
        A.ActionList := ActionList1;
        A.OnExecute  := CreateObject;
        A.Name       := 'act' + S.Name;
        A.Caption    := ''; // No caption, just a button
        A.Category   := 'Scripted Objects';
        A.ImageIndex := J;
        A.Tag        := J;
        A.Hint       := Capitalize(S.Name);
        ACI          := ActionManager1.ActionBars.ActionBars[0].Items.Add;
        ACI.Action   := A;
      End;
    End; // For J
  End; // For I
  L.Free;
End; // TfrmMain.SetupPalette

Procedure TfrmMain.LoadObjectTreeView;
Var
  I    : Integer;
  ZO   : TZoneObject;
  TN,T : TTreeNode;

  Procedure AddGroup(TN: TTreeNode; GO: TGroupObject);
  Var
    I   : Integer;
    ZO  : TZoneObject;
    TN1 : TTreeNode;

  Begin
    For I := 0 To GO.Objects.Count - 1 Do
    Begin
      ZO       := TZoneObject(GO.Objects.Objects[I]);
      TN1      := tvMain.Items.AddChild(TN,ZO.GetName);
      TN1.Data := ZO;
      If ZO Is TGroupObject Then AddGroup(TN1,TGroupObject(ZO));
    End; // For I
  End; // AddGroup

Begin
  LoadingParms := True;
  tvMain.Items.Clear;
  T  := Nil;
  TN := Nil;//tvMain.Items.Add(Nil,'Zone');
  For I := 0 To Zone.Count - 1 Do
  Begin
    ZO       := TZoneObject(Zone.Objects[I]);
    TN       := tvMain.Items.Add(TN,ZO.GetName);
//    TN       := TTreeNode.Create(tvMain.Items);
    LastNode := TN;
    TN.Data  := ZO;
//    tvMain.Items.AddObject(TN,ZO.GetName,ZO);
    If I = 0 Then T := TN;

    If ZO Is TGroupObject Then AddGroup(TN,TGroupObject(ZO));
  End; // For I
  LoadingParms := False;
  ZPropList1.CurObj := Nil;
  If T <> Nil Then tvMain.Selected := T;
End; // TfrmMain.LoadObjectTreeView

procedure TfrmMain.tvMainChange(Sender: TObject; Node: TTreeNode);
Var ZO: TZoneObject;
begin
  If Not LoadingParms Then
  Begin
    If tvMain.SelectionCount >= 1 Then
    Begin
      LoadParameters;
      If (tvMain.Selections[0] <> Nil)      And
         (tvMain.Selections[0].Data <> Nil)  Then
      Begin
        ZO := TZoneObject(tvMain.Selections[0].Data);
        ZPropList1.CurObj := ZO;
      End;
    End
    Else LoadParameters;
  End;
end;

Function TfrmMain.ValidSelections: Boolean;
Var
  I     : Integer;
  Valid : Boolean;

Begin
  I := 0;
  Valid := True;
  While (I < tvMain.SelectionCount) And Valid Do
  Begin
    If (tvMain.Selections[I]      = Nil) Or
       (tvMain.Selections[I].Data = Nil) Then Valid := False Else Inc(I);
  End; // While
  Result := Valid;
End; // TfrmMain.ValidSelections

Procedure TfrmMain.UpdatePosition(Value: String; Element: Integer);
Var
  ZO : TZoneObject;
  D  : Double;
  B  : Boolean;
  B1 : Boolean;
  E  : TExpression;
  Op : TOperandRec;
  L  : TStringList;
  I  : Integer;

Begin
  If (tvMain.SelectionCount >= 1)                        And
     ValidSelections                                     And
     (glView.Scene3D.Scene.Entities.Count > FirstEntity) And
     Not LoadingParms                                    Then
  Begin
    E  := TExpression.Create(Value);
    L  := TStringList.Create;
    Op := E.Evaluate(L);
    L.Free;
    E.Free;
    If Op.OperandType In [onInteger,onReal] Then
    Begin
      If Op.OperandType = onInteger
       Then D := Op.Value.Int
       Else D := Op.Value.Ext;
      B1 := True;
      For I := 0 To tvMain.SelectionCount - 1 Do
      Begin
        ZO := TZoneObject(tvMain.Selections[I].Data);
        ObjLoc.Copy(ZO.Loc);
        Case Element Of
          0: ObjLoc.X := D;
          1: ObjLoc.Y := D;
          2: ObjLoc.Z := D;
        End; // Case
        B  := ObjLoc.Equals(ZO.Loc);
        ZO.Loc.Copy(ObjLoc);
        B1 := B1 And B;
        If Not B Then
        Begin
          RefreshObject(ZO,True,True,True);
        End;
      End; // For I
      If Not B1 Then LoadParameters;

      If (tvMain.SelectionCount = 1) And
         (ZO Is TMeshLibraryObjectReference) And
         TMeshLibraryObjectReference(ZO).Gravity Then ShowMessage('Gravity is ON for this object');
    End;
  End;
End; // TfrmMain.UpdatePosition

Procedure TfrmMain.UpdateRotation(Value: String; Element: Integer);
Var
  ZO : TZoneObject;
  D  : Single;
  B  : Boolean;
  B1 : Boolean;
  E  : TExpression;
  Op : TOperandRec;
  L  : TStringList;
  I  : Integer;

Begin
  If (tvMain.SelectionCount >= 1)                        And
     ValidSelections                                     And
     (glView.Scene3D.Scene.Entities.Count > FirstEntity) And
     Not LoadingParms                                    Then
  Begin
    E  := TExpression.Create(Value);
    L  := TStringList.Create;
    Op := E.Evaluate(L);
    L.Free;
    E.Free;
    If Op.OperandType In [onInteger,onReal] Then
    Begin
      If Op.OperandType = onInteger
       Then D := Op.Value.Int
       Else D := Op.Value.Ext;
      B1 := True;
      For I := 0 To tvMain.SelectionCount - 1 Do
      Begin
        ZO := TZoneObject(tvMain.Selections[I].Data);
        ObjRotate.Copy(ZO.Rotate);
        Case Element Of
          0: ObjRotate.XAngle := D;
          1: ObjRotate.YAngle := D;
          2: ObjRotate.ZAngle := D;
        End; // Case
        B  := ObjRotate.Equals(ZO.Rotate);
        ZO.Rotate.Copy(ObjRotate);
        B1 := B1 And B;
        If Not B Then
        Begin
          RefreshObject(ZO,True,True,True);
        End;
      End; // For I
      If Not B1 Then LoadParameters;
    End;
  End;
End; // TfrmMain.UpdateRotation

Procedure TfrmMain.UpdateSize(Value: String; Element: Integer);
Var
  ZO : TZoneObject;
  D  : Double;
  B  : Boolean;
  B1 : Boolean;
  E  : TExpression;
  Op : TOperandRec;
  L  : TStringList;
  I  : Integer;

Begin
  If (tvMain.SelectionCount >= 1)                        And
     ValidSelections                                     And
     (glView.Scene3D.Scene.Entities.Count > FirstEntity) And
     Not LoadingParms                                    Then
  Begin
    E  := TExpression.Create(Value);
    L  := TStringList.Create;
    Op := E.Evaluate(L);
    L.Free;
    E.Free;
    If Op.OperandType In [onInteger,onReal] Then
    Begin
      If Op.OperandType = onInteger
       Then D := Op.Value.Int
       Else D := Op.Value.Ext;
      B1 := True;
      For I := 0 To tvMain.SelectionCount - 1 Do
      Begin
        ZO := TZoneObject(tvMain.Selections[I].Data);
        Case Element Of
          0: ObjSize.X := D;
          1: ObjSize.Y := D;
          2: ObjSize.Z := D;
        End; // Case
        B  := ObjSize.Equals(ZO.Size);
        ZO.Size.Copy(ObjSize);
        B1 := B1 And B;
        If Not B Then
        Begin
          RefreshObject(ZO,True,True,True);
        End;
      End; // For I
      If Not B1 Then LoadParameters;
    End;
  End;
End; // TfrmMain.UpdateSize

Procedure TfrmMain.UpdateSQLRef(Value: String; Element: Integer);
Var
  ZO : TZoneObject;
  B  : Boolean;
  B1 : Boolean;
  I  : Integer;
  R  : TSQLRef;

Begin
  If (tvMain.SelectionCount >= 1)                        And
     ValidSelections                                     And
     (glView.Scene3D.Scene.Entities.Count > FirstEntity) And
     Not LoadingParms                                    Then
  Begin
    B1 := True;
    For I := 0 To tvMain.SelectionCount - 1 Do
    Begin
      ZO := TZoneObject(tvMain.Selections[I].Data);
      If ZO Is TMeshLibraryObjectReference Then
      Begin
        R := GetSQLRefValueFromVisual(Value);
        B := (R = TMeshLibraryObjectReference(ZO).SQLRef);
        TMeshLibraryObjectReference(ZO).SQLRef := R;
      End;
      B1 := B1 And B;
      If Not B Then
      Begin
        RefreshObject(ZO,True,True,True);
      End;
    End; // For I
    If Not B1 Then LoadParameters;
  End;
End; // TfrmMain.UpdateSQLRef

Procedure TfrmMain.AddNewObject(St: String; ZO: TZoneObject);
Var
  L       : TStringList;
  L1      : TStringList;
  I,J     : Integer;
  P       : Array[0..2] Of Single;
  S       : Array[0..2] Of Single;
  R       : Array[0..2] Of Single;
  TexPath : String;
  Entity  : TEntity;

Begin
  Zone.AddObject(St,ZO);
  ZO.SetZone(Zone);

  L := TStringList.Create;
  ZO.AddToPolygonList(L,False,False,True,True);
  If MasterMeshList = Nil Then MasterMeshList := TStringList.Create;
  MasterMeshList.AddObject('',L.Objects[0]);
  L.Free;

  L1   := Zone.BuildPolygonLists(ZO);
  L    := TStringList.Create;
  I    := Zone.GetObjectIndex(ZO);
//  While L.Count < I Do L.Add('');

  If ZO Is TCreatureLibraryObjectReference
   Then TexPath := ExtractFilePath(Application.ExeName) + 'library\creatures\'
   Else TexPath := ExtractFilePath(Application.ExeName) + 'library\textures\';

  For J := 0 To L1.Count - 1 Do L.AddObject('',L1.Objects[J]);
  For J := 0 To L1.Count - 1 Do Entity := ReloadMesh(L,I,I + J,False,True,True,glView,Tree,False,TexPath);
  For J := 0 To L1.Count - 1 Do L1.Objects[J].Free;
  L1.Free;
  L.Free;

  If (ZO <> Nil) And (ZO Is TCreatureLibraryObjectReference) Then LoadCreatureSkeleton(TCreatureLibraryObjectReference(ZO).An8File,glView,Entity,ZO);

  // Save the position and rotation

  P[0] := glView.Scene3D.Scene.DefaultCamera.Position.X;
  P[1] := glView.Scene3D.Scene.DefaultCamera.Position.Y;
  P[2] := glView.Scene3D.Scene.DefaultCamera.Position.Z;
  S[0] := glView.Scene3D.Scene.DefaultCamera.SceneCenter.X;
  S[1] := glView.Scene3D.Scene.DefaultCamera.SceneCenter.Y;
  S[2] := glView.Scene3D.Scene.DefaultCamera.SceneCenter.Z;
  R[0] := glView.Scene3D.Scene.DefaultCamera.UpVector.X;
  R[1] := glView.Scene3D.Scene.DefaultCamera.UpVector.Y;
  R[2] := glView.Scene3D.Scene.DefaultCamera.UpVector.Z;

  LoadDynamicLights;

  AddLightSpheres;
  AddHotSpots;
  RenderZone(BirdsEye);
  If BirdsEye Then glView.FitFrame(True,True,False,I + FirstEntity,True);

  // Restore the position and rotation

  glView.Scene3D.Scene.DefaultCamera.SetPosition(P[0],P[1],P[2]);
  glView.Scene3D.Scene.DefaultCamera.LookAt(S[0],S[1],S[2]);
  glView.Scene3D.Scene.DefaultCamera.SetVectorUp(R[0],R[1],R[2]);

  LoadObjectTreeView;

  LastNode.Selected := True;
  tvMain.Selected   := LastNode;
  LoadParameters;
End; // TfrmMain.AddNewObject

Procedure TfrmMain.CreateObject(Sender: TObject);
Var
  A  : TAction;
  SC : TScripted;
  SO : TScriptedObject;
  St : String;
  I  : Integer;

Begin
  A  := TAction(Sender);
  SC := TScripted(ScriptLibrary.Objects[A.Tag]);
  St := SC.Name;

  // Find a unique name

  I := 1;
  While Zone.NameExists(St + IntToStr(I)) Do Inc(I);
  St := St + IntToStr(I);
  If InputQuery('Create new Object','Enter new object name:',St) Then
  Begin
    St := Trim(St);
    If St <> '' Then
    Begin
      If Not Zone.NameExists(St) Then
      Begin
        SO := TScriptedObject.Create(St);
        SO.SetScriptName(SC.Name);
        If Not BirdsEye Then
        Begin
          SO.Loc.X := Observer.X + CreateDist * Cos(Phi);
          SO.Loc.Y := Observer.Y + CreateDist * Sin(Phi);
          SO.Loc.Z := Observer.Z;
        End;

        // Add the object

        AddNewObject(St,SO);
      End
      Else ShowMessage('An object with that name already exists. Please choose a different one.')
    End
    Else ShowMessage('You cannot enter a blank name.');
  End;
  glView.Scene3D.Redraw;
End; // TfrmMain.CreateObject

Procedure TfrmMain.UpdateTitleBar;
Begin
  If LastLoadedScene = ''
   Then Caption := 'OpenZone ' + Version
   Else Caption := ExtractFileName(LastLoadedScene) + ' - ' + 'OpenZone ' + Version;
  Application.Title := Caption; 
End; // TfrmMain.UpdateTitleBar

procedure TfrmMain.cbTextureSetChange(Sender: TObject);
begin
  If Not SettingTexSet Then
  Begin
    If cbTextureSet.ItemIndex < 0
     Then SetTextureSet(cbTextureSet.Text)
     Else SetTextureSet('');
    DisplayStats;
    RenderZone(BirdsEye);
  End;  
end;

procedure TfrmMain.cbShowTransparentClick(Sender: TObject);
begin
  ReloadZone;
  RenderZone(BirdsEye);
  glView.Fit(Not BirdsEye,True,False);
  SetViewToNorth;
  DisplayStats;
end;

Function TfrmMain.WaterAreaCount: Integer;
Var I,J: Integer;
Begin
  J := 0;
  For I := 0 To High(Zone.Water) Do If Zone.Water[I].WType = wtWater Then Inc(J);
  Result := J;
End; // TfrmMain.WaterAreaCount

Function TfrmMain.LavaAreaCount: Integer;
Var I,J: Integer;
Begin
  J := 0;
  For I := 0 To High(Zone.Water) Do If Zone.Water[I].WType = wtLava Then Inc(J);
  Result := J;
End; // TfrmMain.LavaAreaCount

Function TfrmMain.PvPAreaCount: Integer;
Var I,J: Integer;
Begin
  J := 0;
  For I := 0 To High(Zone.Water) Do If Zone.Water[I].WType = wtPvP Then Inc(J);
  Result := J;
End; // TfrmMain.PvPAreaCount

Function TfrmMain.IceAreaCount: Integer;
Var I,J: Integer;
Begin
  J := 0;
  For I := 0 To High(Zone.Water) Do If Zone.Water[I].WType = wtIce Then Inc(J);
  Result := J;
End; // TfrmMain.IceAreaCount

Function TfrmMain.IceWaterAreaCount: Integer;
Var I,J: Integer;
Begin
  J := 0;
  For I := 0 To High(Zone.Water) Do If Zone.Water[I].WType = wtIceWater Then Inc(J);
  Result := J;
End; // TfrmMain.IceWaterAreaCount

Procedure TfrmMain.ImportFrom3DS(FileName: String);
Begin
//  LastLoadedScene := '';
  BirdsEye          := True;
  glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
  glView.Scene3D.Scene.CheckHidden := Not BirdsEye;
  glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;  
  glView.AllowMouse := BirdsEye;
//  Zone.Free;
//  Zone := TZone.Create;
  Zone.ImportFrom3DSFile(FileName);
  Observer.Free;
  Observer        := T3DPoint.Create(0,0,0);
  LoadObjectTreeView;
  MeshLibrary.Sort;

  LoadMeshLibraryList(tvMeshes);

  // Get a polygon list from the zone

  ReloadZone;

  // Render the zone

  RenderZone(BirdsEye);
  glView.Fit(Not BirdsEye,True,False);
  SetViewToNorth;
  UpdateTitleBar;
End; // TfrmMain.ImportFrom3DS

Procedure TfrmMain.ImportFromXWF(FileName: String);
Var Stream: TFileStream;
Begin
//  LastLoadedScene := '';
  BirdsEye          := True;
  glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
  glView.Scene3D.Scene.CheckHidden := Not BirdsEye;
  glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;  
  glView.AllowMouse := BirdsEye;
//  Zone.Free;
//  Zone := TZone.Create;
  Stream := TFileStream.Create(FileName,fmOpenRead);
  Zone.ImportFromXWFFile(Stream);
  Stream.Free;
  Observer.Free;
  Observer        := T3DPoint.Create(0,0,0);
  LoadObjectTreeView;
  MeshLibrary.Sort;

  LoadMeshLibraryList(tvMeshes);

  // Get a polygon list from the zone

  ReloadZone;

  // Render the zone

  RenderZone(BirdsEye);
  glView.Fit(Not BirdsEye,True,False);
  SetViewToNorth;
  UpdateTitleBar;
End; // TfrmMain.ImportFromXWF

Procedure TfrmMain.ImportFromAN8(FileName: String; Rotate: Boolean);
Begin
//  LastLoadedScene := '';
  BirdsEye          := True;
  glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
  glView.Scene3D.Scene.CheckHidden := Not BirdsEye;
  glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;
  glView.AllowMouse := BirdsEye;
//  Zone.Free;
//  Zone := TZone.Create;
  Zone.ImportFromAN8File(FileName,Rotate);
  Observer.Free;
  Observer        := T3DPoint.Create(0,0,0);
  LoadObjectTreeView;
  MeshLibrary.Sort;

  LoadMeshLibraryList(tvMeshes);

  // Get a polygon list from the zone

  ReloadZone;

  // Render the zone

  RenderZone(BirdsEye);
  glView.Fit(Not BirdsEye,True,False);
  SetViewToNorth;
  UpdateTitleBar;
End; // TfrmMain.ImportFromAN8

Procedure TfrmMain.ImportFromDirectX(FileName: String; Rotate: Boolean);
Begin
//  LastLoadedScene := '';
  BirdsEye          := True;
  glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
  glView.Scene3D.Scene.CheckHidden := Not BirdsEye;
  glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;
  glView.AllowMouse := BirdsEye;
//  Zone.Free;
//  Zone := TZone.Create;
  Zone.ImportFromDirectXFile(FileName,Rotate);
  Observer.Free;
  Observer        := T3DPoint.Create(0,0,0);
  LoadObjectTreeView;
  MeshLibrary.Sort;

  LoadMeshLibraryList(tvMeshes);

  // Get a polygon list from the zone

  ReloadZone;

  // Render the zone

  RenderZone(BirdsEye);
  glView.Fit(Not BirdsEye,True,False);
  SetViewToNorth;
  UpdateTitleBar;
End; // TfrmMain.ImportFromDirectX

Procedure TfrmMain.ImportGroundFrom3DS(FileName: String);
Begin
//  LastLoadedScene := '';
  BirdsEye          := True;
  glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
  glView.Scene3D.Scene.CheckHidden := Not BirdsEye;
  glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;  
  glView.AllowMouse := BirdsEye;
//  Zone.Free;
//  Zone := TZone.Create;
  Zone.ImportGroundFrom3DSFile(FileName);
  Observer.Free;
  Observer        := T3DPoint.Create(0,0,0);
  LoadObjectTreeView;

  // Get a polygon list from the zone

  ReloadZone;

  // Render the zone

  RenderZone(BirdsEye);
  glView.Fit(Not BirdsEye,True,False);
  SetViewToNorth;
  UpdateTitleBar;
End; // TfrmMain.ImportGroundFrom3DS

Procedure TfrmMain.ImportFromOGREXMLMesh(FileName: String);
Var
  Doc                : TDOMDocument;
  Mesh               : TDOMElement;
  SubMeshes          : TDOMElement;
  SubMesh            : TDOMElement;
  MeshItem           : TDOMElement;
  VertexBuffer       : TDOMElement;
  Vertex             : TDOMElement;
  VertexElement      : TDOMElement;
  Face               : TDOMElement;
  Material           : TDOMAttr;
  MaterialName       : String;
  MO                 : TMeshObject;
  Count              : Integer;
  Positions          : Boolean;
  Normals            : Boolean;
  Colors             : Boolean;
  NumTexCoords       : Integer;
  TexCoordSets       : Integer;
  TexCoordDimensions : Integer;
  PX,PY,PZ           : Extended;
  NX,NY,NZ           : Extended;
  U,V                : Extended;
  V1,V2,V3           : Integer;
  TX,TZ              : Array Of Single;
  P                  : TPolygon;
  I                  : Integer;

  Function GetIntegerAttr(Element: TDOMElement; AttrName: String; Default: Integer): Integer;
  Var Attr: TDOMAttr;
  Begin
    Attr := Element.getAttributeNode(AttrName);
    If Attr <> Nil Then Result := StrtoInt(Attr.nodeValue) Else Result := Default;
  End; // GetIntegerAttr

  Function GetBooleanAttr(Element: TDOMElement; AttrName: String; Default: Boolean): Boolean;
  Var Attr: TDOMAttr;
  Begin
    Attr := Element.getAttributeNode(AttrName);
    If Attr <> Nil
     Then Result := (LowerCase(Attr.nodeValue) = 'true')
     Else Result := Default;
  End; // GetBooleanAttr

  Function GetRealAttr(Element: TDOMElement; AttrName: String; Default: Extended): Extended;
  Var Attr: TDOMAttr;
  Begin
    Attr := Element.getAttributeNode(AttrName);
    If Attr <> Nil Then Result := StrtoFloat(Attr.nodeValue) Else Result := Default;
  End; // GetRealAttr

Begin
  If FileExists(FileName) Then
  Begin
    Doc := XMLToDOMParser1.fileToDom(FileName);
    If Doc <> Nil Then
    Begin
      LastLoadedScene   := '';
      BirdsEye          := True;
      glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
      glView.Scene3D.Scene.CheckHidden := Not BirdsEye;
      glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;      
      glView.AllowMouse := BirdsEye;
      Zone.Free;
      Zone := TZone.Create;

      // Parse the DOM document

      Mesh      := Doc.findFirstChildElement;
      SubMeshes := Mesh.findFirstChildElement;
      While SubMeshes <> Nil Do
      Begin
        If LowerCase(SubMeshes.tagName) = 'submeshes' Then
        Begin
          SubMesh := SubMeshes.findFirstChildElement;
          While SubMesh <> Nil Do
          Begin
            If LowerCase(SubMesh.tagName) = 'submesh' Then
            Begin
              // Look for the material name

              Material := SubMesh.getAttributeNode('material');
              If Material <> Nil
               Then MaterialName := Material.nodeValue
               Else MaterialName := '';

              // Set up the texture coordinate cache

              SetLength(TX,0);
              SetLength(TZ,0);

              // Create a new mesh object and add it to the zone

              MO := TMeshObject.Create('Mesh' + IntToStr(Zone.Count + 1));
              Zone.AddObject(MO.GetName,MO);
              MeshItem := SubMesh.findFirstChildElement;
              While MeshItem <> Nil Do
              Begin
                If LowerCase(MeshItem.tagName) = 'geometry' Then
                Begin
                  Count := GetIntegerAttr(MeshItem,'count',0);
                  VertexBuffer := MeshItem.findFirstChildElement;
                  While VertexBuffer <> Nil Do
                  Begin
                    If LowerCase(VertexBuffer.tagName) = 'vertexbuffer' Then
                    Begin
                      Positions          := GetBooleanAttr(VertexBuffer,'positions',False);
                      Normals            := GetBooleanAttr(VertexBuffer,'normals',False);
                      Colors             := GetBooleanAttr(VertexBuffer,'colours',False);
                      NumTexCoords       := GetIntegerAttr(VertexBuffer,'numtexcoords',0);
                      TexCoordSets       := GetIntegerAttr(VertexBuffer,'texcoordsets',0);
                      TexCoordDimensions := GetIntegerAttr(VertexBuffer,'texcoorddimensions',0);
                      Vertex := VertexBuffer.findFirstChildElement;
                      While Vertex <> Nil Do
                      Begin
                        If LowerCase(Vertex.tagName) = 'vertex' Then
                        Begin
                          VertexElement := Vertex.findFirstChildElement;
                          PX := 0;
                          PY := 0;
                          PZ := 0;
                          NX := 0;
                          NY := 0;
                          NZ := 0;
                          U  := 0;
                          V  := 0;
                          While VertexElement <> Nil Do
                          Begin
                            If LowerCase(VertexElement.tagName) = 'position' Then
                            Begin
                              PX := GetRealAttr(VertexElement,'x',0);
                              PY := GetRealAttr(VertexElement,'y',0);
                              PZ := GetRealAttr(VertexElement,'z',0);
                            End
                            Else If LowerCase(VertexElement.tagName) = 'normal' Then
                            Begin
                              NX := GetRealAttr(VertexElement,'x',0);
                              NY := GetRealAttr(VertexElement,'y',0);
                              NZ := GetRealAttr(VertexElement,'z',0);
                            End
                            Else If LowerCase(VertexElement.tagName) = 'texcoord' Then
                            Begin
                              // We don't support 3D textures

                              U := GetRealAttr(VertexElement,'u',0);
                              V := GetRealAttr(VertexElement,'v',0);
                            End;
                            VertexElement := VertexElement.findNextSiblingElement;
                          End; // While

                          // Now add the vertex

                          If Positions Then MO.Vertices.AddObject('',T3DPoint.Create(PX,PY,PZ));
                          If Normals Then
                          Begin
                            MO.Normals.AddObject('',T3DPoint.Create(NX,NY,NZ));
{
                            MO.NormalX[MO.NumNormals - 1] := NX;
                            MO.NormalY[MO.NumNormals - 1] := NY;
                            MO.NormalZ[MO.NumNormals - 1] := NZ;
}
                          End;

                          // Make sure we have at least two-dimensional texture coordinates

                          If (NumTexCoords > 0) And (TexCoordDimensions >= 2) Then
                          Begin
                            SetLength(TX,High(TX) + 2);
                            SetLength(TZ,High(TZ) + 2);
                            TX[High(TX)] := U;
                            TZ[High(TZ)] := V;
                          End;
                        End;
                        Vertex := Vertex.findNextSiblingElement;
                      End; // While
                    End;
                    VertexBuffer := VertexBuffer.findNextSiblingElement;
                  End; // While
                End
                Else If LowerCase(MeshItem.tagName) = 'faces' Then
                Begin
                  Count := GetIntegerAttr(MeshItem,'count',0);
                  Face  := MeshItem.findFirstChildElement;
                  While Face <> Nil Do
                  Begin
                    V1 := GetIntegerAttr(Face,'v1',-1);
                    V2 := GetIntegerAttr(Face,'v2',-1);
                    V3 := GetIntegerAttr(Face,'v3',-1);

                    If (V1 >= 0) And (V2 >= 0) And (V3 >= 0) Then
                    Begin
                      P := TPolygon.Create;
                      MO.Polygons.AddObject('',P);
                      SetLength(P.Vertices,3);
                      P.SetSingleTexture(MaterialName,'');

                      // Have to reverse the vertex order

                      P.Vertices[0] := V3;
                      P.Vertices[1] := V2;
                      P.Vertices[2] := V1;
                      If High(TX) >= 0 Then
                      Begin
                        // If there are any texture coordinates, assume there are as many as
                        // there are vertices

                        SetLength(P.TX,3);
                        SetLength(P.TZ,3);
                        For I := 0 To 2 Do
                        Begin
                          P.TX[I] := TX[P.Vertices[I]];
                          P.TZ[I] := TZ[P.Vertices[I]];
                        End; // For I
                        P.HasTexCoords := True;
                      End;
                    End;

                    Face := Face.findNextSiblingElement;
                  End; // While
                End;
                MeshItem := MeshItem.findNextSiblingElement;
              End; // While
              MO.CalcNormals;

              // Clear the texture coordinate cache

              SetLength(TX,0);
              SetLength(TZ,0);

            End;
            SubMesh := SubMesh.findNextSiblingElement;
          End; // While
        End;
        SubMeshes := SubMeshes.findNextSiblingElement;
      End; // While

      // Load the model and update the view

      Observer.Free;
      Observer := T3DPoint.Create(0,0,0);
      LoadObjectTreeView;

      // Get a polygon list from the zone

      ReloadZone;

      // Render the zone

      RenderZone(BirdsEye);
      glView.Fit(Not BirdsEye,True,False);
      SetViewToNorth;
      UpdateTitleBar;
    End;

    // Cleanup

    Doc.Free;
//    XMLToDOMParser1.DOMImpl.freeDocument(Doc);
//    XMLToDOMParser1.DOMImpl.freeUnusedASModels;
  End;
End; // TfrmMain.ImportFromOGREXMLMesh

Procedure TfrmMain.CreateEmptyWLD(Stream: TStream);
Var W: TEQWldData;
Begin
  // Create the .WLD file, with room for 2 packets

  W := TEQWldData.Create;
  W.nFragment := 1;
  SetLength(W.Fragments,1);

  // The first packet is always empty

  W.Fragments[0] := Nil;

  // Encode the .WLD file

  W.Encode(Stream);
  W.Free;
End; // TfrmMain.CreateEmptyWLD

Function TfrmMain.CreateObjWLD(Stream: TStream; ZoneObjects: Boolean): TStringList;
Var
  W        : TEQWldData;
  I,J,K    : Integer;
  MeshType : Array Of Boolean;
  Tex      : TStringList;
  D03      : Data03;
  St       : String;
  MO       : TMeshObject;
  List     : TStringList;
  MeshHash : TIntegerStringHash;

  Procedure TallyObject(ZO: TZoneObject);
  Var
    I   : Integer;
    GO  : TGroupObject;
    ML  : TMeshLibraryObjectReference;

  Begin
    If ZO Is TMeshLibraryObjectReference Then
    Begin
      If Not TMeshLibraryObjectReference(ZO).InsertMesh Then
      Begin
        ML := TMeshLibraryObjectReference(ZO);
        I  := MeshLibrary.IndexOfObject(ML.Group);
        If I >= 0 Then
        Begin
          MeshType[I] := True;
          MeshHash.Put(I,MeshLibrary.Strings[I]);
        End;
      End;
    End
    Else If ZO Is TGroupObject Then
    Begin
      GO := TGroupObject(ZO);
      For I := 0 To GO.Objects.Count - 1 Do TallyObject(TZoneObject(GO.Objects.Objects[I]));
    End;
  End; // TallyObject

  Procedure EncodeMesh(MO: TMeshObject);
  Const Shift = 7;
  Var
    I,J      : Integer;
    Textures : TStringList;
    P        : TPolygon;
    St       : String;
    D31      : Data31;
    D36      : Data36;
    D2D      : Data2D;
    D14      : Data14;
    Special  : Integer;
    MaxPt    : T3DPoint;
    MinPt    : T3DPoint;
    V        : T3DPoint;
    V1       : Integer;
//    TexIndex : Array Of Integer;
    Normal   : T3DPoint;
    C        : TColor;
    R        : Byte;
    Texture  : String;
    VX,VY,VZ : Single;
    Scale    : Integer;
    Trans    : Boolean;
    TextureSorter : TQuickSorterProc;

    Function FindTextureInSprite(P: TPolygon; S: Data36): Integer;
    Var
      I,J,K   : Integer;
      Found   : Boolean;
      St      : String;
      Special : Integer;

    Begin
      St := P.Texture;
      If P.TextureState = tsTransparent Then St := TransparentTexture
      Else
      Begin
        Special := 0;
        If P.TextureState = tsSemiTransparent Then Special := Special Or spSemiTransparent;
        If P.Masked                           Then Special := Special Or spMasked;
        If Special <> 0 Then St := St + Chr(SpecialTexture + Special);
      End;
      I     := Textures.IndexOf(St);
      J     := 0;
      Found := False;
      While (J < S.Size7) And Not Found Do
      Begin
        If S.Data7[J].Data = I Then Found := True Else Inc(J);
      End; // While
      If Found Then
      Begin
        K     := 0;
        Found := False;
        While (K < S.Size8) And Not Found Do
        Begin
          If S.Data8[K].Data = I Then Found := True Else Inc(K);
        End; // While
        If Found Then Result := J + (K Shl 16) Else Result := -1;
      End
      Else Result := -1;
    End; // FindTextureInSprite

    Function CompareTextures(Index0,Index1: Integer): Integer;

      Function GetTexIndex(Index: Integer): Integer;
      Var
        Poly    : TPolygon;
        St      : String;
        Special : Integer;

      Begin
        Poly   := TPolygon(SortMesh.Polygons.Objects[TexIndex[Index]]);
        St     := Poly.Texture;
        If Poly.TextureState = tsTransparent Then St := TransparentTexture
        Else
        Begin
          Special := 0;
          If Poly.TextureState = tsSemiTransparent Then Special := Special Or spSemiTransparent; //St := St + #241;
          If Poly.Masked Then Special := Special Or spMasked;
          If Special <> 0 Then St := St + Chr(SpecialTexture + Special);
        End;
        Result := SortTexList.IndexOf(St);
      End; // GetTexIndex

    Begin
      Result := GetTexIndex(Index0) - GetTexIndex(Index1);
    End; // CompareTextures

    Procedure ExchangeTextures(Index0,Index1: Integer);
    Var I: Integer;
    Begin
      I                := TexIndex[Index0];
      TexIndex[Index0] := TexIndex[Index1];
      TexIndex[Index1] := I;
    End; // ExchangeTextures

  Begin
    Textures := TStringList.Create;

    // First determine all of the textures used in the mesh

    Trans := False;
    For I := 0 To MO.Polygons.Count - 1 Do
    Begin
      P  := TPolygon(MO.Polygons.Objects[I]);
      St := UpperCase(P.Texture);
      Special := 0;
      If P.TextureState <> tsTransparent Then
      Begin
        If P.TextureState = tsSemiTransparent Then Special := Special Or spSemiTransparent;
        If P.Masked                           Then Special := Special Or spMasked;
        If Special <> 0 Then St := St + Chr(SpecialTexture + Special);
        If Textures.IndexOf(St) < 0 Then Textures.Add(St);
      End
      Else Trans := True;
    End; // For I

    // Create texture entries

    D31 := Data31.Create(W,UpperCase(MO.GetName) + '_MP');
    For I := 0 To Textures.Count - 1 Do
    Begin
      St := Textures.Strings[I];
      If St <> '' Then
      Begin
        J := Ord(St[Length(St)]);
        If J >= SpecialTexture Then
        Begin
          St := Copy(St,1,Length(St) - 1);
          MakeTextureEntry(W,D31,St,'',(J And spSemiTransparent) <> 0,(J And spMasked) <> 0);
        End
        Else MakeTextureEntry(W,D31,St,'',False,False);
      End;
    End; // For I

    // Now make a transparent texture entry if we have to

    If Trans Then
    Begin
      Textures.Add(TransparentTexture);
      MakeTransparentTextureEntry(W,D31);
    End;

    // Add our texture list fragment

    AddFragment(W,D31);

    // Encode the mesh

    MaxPt := T3DPoint.Create;
    MinPt := T3DPoint.Create;
    MO.GetBounds(MinPt,MaxPt);

    D36                        := Data36.Create(W,UpperCase(MO.GetName) + '_DMSPRITEDEF');
    If ZoneObjects
     Then D36.Flags := $14003  // $10000 = Fragment4, $4000 = has box, $1 = has Params1
     Else D36.Flags := $04003; // $4000 = has box, $1 = has Params1

    D36.Fragment1              := FragmentReference.Create;
    D36.Fragment1.Fragment     := D31;
    D36.Fragment2              := FragmentReference.Create;
    D36.Fragment3              := FragmentReference.Create;
    D36.Fragment4              := FragmentReference.Create;
    If (D36.Flags And $10000) <> 0 Then D36.Fragment4.Fragment := GetFirstData03(W);
    D36.Params1[0]             := 0; //(MaxPt.X + MinPt.X) / 2;
    D36.Params1[1]             := 0; //(MaxPt.Y + MinPt.Y) / 2;
    D36.Params1[2]             := 0; //(MaxPt.Z + MinPt.Z) / 2;
    D36.Params2[0]             := 0; // Not used unless Flags contains $200
    D36.Params2[1]             := 0;
    D36.Params2[2]             := 0;
    D36.Box.MinX               := MinPt.X;
    D36.Box.MinY               := MinPt.Y;
    D36.Box.MinZ               := MinPt.Z;
    D36.Box.MaxX               := MaxPt.X;
    D36.Box.MaxY               := MaxPt.Y;
    D36.Box.MaxZ               := MaxPt.Z;
    MaxPt.Subtract(MinPt);
    D36.Radius                 := MaxPt.GetLength;

    I := MO.Polygons.Count * 3;
    D36.Size1  := I;                               // Vertices
    D36.Size2  := I;                               // Texture coordinates
    D36.Size3  := I;                               // Normals
    D36.Size4  := I;                               // Colors
    D36.Size5  := MO.Polygons.Count;               // Polygons
    D36.Size6  := 0;                               // Unknown
    D36.Size7  := 0;                               // Polygon-texture counts
    D36.Size8  := 0;                               // Vertex-texture counts
    D36.Size9  := 0;                               // Unknown
//    D36.Size10 := Shift;                           // Scale

    D36.Size10 := 15;                              // Scale
    While (D36.Size10 > 0) And ((1 Shl D36.Size10) > Trunc(32767 / D36.Radius)) Do Dec(D36.Size10);

    SetLength(D36.Data1,D36.Size1);
    SetLength(D36.Data2,D36.Size2);
    SetLength(D36.Data3,D36.Size3);
    SetLength(D36.Data4,D36.Size4);
    SetLength(D36.Data5,D36.Size5 * 4);
    SetLength(D36.Data6,D36.Size6);
    SetLength(D36.Data7,D36.Size7);
    SetLength(D36.Data8,D36.Size8);

    SetLength(TexIndex,MO.Polygons.Count);
    For I := 0 To High(TexIndex) Do TexIndex[I] := I;

    SortMesh    := MO;
    SortTexList := Textures;

    TextureSorter          := TQuickSorterProc.Create;
    TextureSorter.Compare  := @CompareTextures;
    TextureSorter.Exchange := @ExchangeTextures;
    TextureSorter.Sort(0,MO.Polygons.Count - 1);
    TextureSorter.Free;

//    QuickSortTextures(0,MO.Polygons.Count - 1);

    // The polygons should all be triangles at this point.  Make sure we have enough room for the vertex list.

    Scale := 1 Shl D36.Size10;
    For I := 0 To MO.Polygons.Count - 1 Do
    Begin
      P := TPolygon(MO.Polygons.Objects[TexIndex[I]]);
      If Not P.HasTexCoords Then MO.CalcTextureCoords(P);

      // Can you pass through the polygon?

      If Not P.Solid
       Then D36.Data5[I].Flags := $10   // $10 means you can go through it
       Else D36.Data5[I].Flags := 0;

      // Find the polygon's vertices and add them to the sprite

      // While it may be tempting to get rid of redundant vertices, remember that
      // vertices also have texture coordinates mapped to them.  To allow neighboring
      // polygons to end on different texture coordinates, they have to have different
      // vertices.  From a space standpoint, it would be more efficient to eliminate
      // those vertices where all the normals and texture coordinates are the same.
      // Perhaps in a future version...

      For J := 0 To High(P.Vertices) Do
      Begin
        // Form a vertex index to make life easier

        V1 := I * 3 + J;

        // Texture coordinates are bound to vertices, not polygons. That means that
        // polygons which meet at the same vertex have to have the same texture
        // coordinates at that point.

        D36.Data2[V1].I1      := Round(P.TX[J] * TextureSize);
        D36.Data2[V1].I2      := Round(P.TZ[J] * TextureSize);

        // Each vertex has a normal vector for phong shading

        If MO.Normals.Count <> MO.Vertices.Count Then MO.CalcNormals;

        Normal := T3DPoint(MO.Normals.Objects[P.Vertices[J]]);
        D36.Data3[V1].X       := Round(Normal.X * 127);
        D36.Data3[V1].Y       := Round(Normal.Y * 127);
        D36.Data3[V1].Z       := Round(Normal.Z * 127);

        // RGBA Color (tint/transparency)

        If P.HasColor Then
        Begin
          // EQ uses BGRA while we use RGBA, so load the color components individually

          C               := P.Colors[J];
          D36.Data4[V1].R := TRGBA(C).R;
          D36.Data4[V1].G := TRGBA(C).G;
          D36.Data4[V1].B := TRGBA(C).B;
          D36.Data4[V1].A := TRGBA(C).A;
        End
        Else
        Begin
          If Zone.ZoneType = ztOutdoor
           Then D36.Data4[V1].Color := $FF000000
           Else D36.Data4[V1].Color := $00000000;
        End;

        // Write the correct vertex index

        D36.Data5[I].Vertex[J] := V1;

        // Save the vertex information

        V  := T3DPoint(MO.Vertices.Objects[P.Vertices[J]]);
        VX := V.X;
        VY := V.Y;
        VZ := V.Z;
        VX := VX * Scale;
        VY := VY * Scale;
        VZ := VZ * Scale;
        D36.Data1[V1].X := Round(VX);
        D36.Data1[V1].Y := Round(VY);
        D36.Data1[V1].Z := Round(VZ);
      End; // For J

      // The number of polygons and number of vertices arrays are sorted by texture.
      // If we have already processed a polygon with this texture, then increment
      // the number of polygons found and increase the vertex count. Otherwise make
      // new entries for this texture.

      J := FindTextureInSprite(P,D36);
      If J >= 0 Then
      Begin
        Inc(D36.Data7[J And $FFFF].NumPolys);
        Inc(D36.Data8[J Shr 16].NumVertices,High(P.Vertices) + 1);
      End
      Else
      Begin
        Texture := P.Texture;
        If P.TextureState = tsTransparent Then Texture := TransparentTexture
        Else
        Begin
          Special := 0;
          If P.TextureState = tsSemiTransparent Then Special := Special Or spSemiTransparent;
          If P.Masked Then Special := Special Or spMasked;
          If Special <> 0 Then Texture := Texture + Chr(SpecialTexture + Special);
        End;
        J := Textures.IndexOf(Texture);
        If J >= 0 Then
        Begin
          Inc(D36.Size7);
          Inc(D36.Size8);
          SetLength(D36.Data7,D36.Size7);
          SetLength(D36.Data8,D36.Size8);
          D36.Data7[D36.Size7 - 1].NumPolys    := 1;
          D36.Data7[D36.Size7 - 1].Data        := J;
          D36.Data8[D36.Size8 - 1].NumVertices := High(P.Vertices) + 1;
          D36.Data8[D36.Size8 - 1].Data        := J;
        End;
      End;
    End; // For I

    AddFragment(W,D36);
    MaxPt.Free;
    MinPt.Free;
    SetLength(TexIndex,0);

    // Make a reference to the mesh

    D2D                        := Data2D.Create(W);
    D2D.Fragment               := FragmentReference.Create;
    D2D.Fragment.Fragment      := D36;
    AddFragment(W,D2D);

    // Encode an actor reference

    D14                        := Data14.Create(W,UpperCase(MO.GetName) + '_ACTORDEF');
    D14.Fragment1              := FragmentReference.Create;
    D14.Fragment1.Fragment     := Nil;
    D14.Fragment1.Name         := '(SPRITECALLBACK)';
    If ZoneObjects Then
    Begin
      D14.Flags              := $80;
      D14.Fragment2          := FragmentReference.Create;
      D14.Fragment2.Fragment := Nil;
      D14.Fragment2.Name     := '';
    End
    Else D14.Flags := 0;
    D14.Size1                  := 1;
    SetLength(D14.Data1,1);
    D14.Data1[0].Size          := 1;
    SetLength(D14.Data1[0].Data,1);
    D14.Data1[0].Data[0].Data1 := 0;
    D14.Data1[0].Data[0].Data2 := 1.00000001504747E30;
    D14.Size2 := 1;
    SetLength(D14.Data2,1);
    D14.Data2[0]               := FragmentReference.Create;
    D14.Data2[0].Fragment      := D2D;
    AddFragment(W,D14);

    Textures.Free;
  End; // EncodeMesh

Begin
  Tex := TStringList.Create;
  SetLength(MeshType,MeshLibrary.Count);
  For I := 0 To High(MeshType) Do MeshType[I] := False;
  MeshHash := TIntegerStringHash.Create(False);

  // Create the .WLD file, with room for 2 packets

  W := TEQWldData.Create;
  W.nFragment := 1;
  SetLength(W.Fragments,1);

  // The first packet is always empty

  W.Fragments[0] := Nil;

  // Tally all of the kinds of objects to be encoded

  For I := 0 To Zone.Count - 1 Do TallyObject(TZoneObject(Zone.Objects[I]));

  // Add any objects the user explicity added in File...Properties (e.g. door meshes)

  For I := 0 To Zone.ExtraMeshes.Count - 1 Do
  Begin
    St := Zone.ExtraMeshes.Strings[I];

    // The format is <originalname>/<exportname>.  Get the original name and look for it in the mesh library.

    K := Pos('/',St);
    If K > 0 Then St := Copy(St,1,K - 1);
    J := MeshLibrary.IndexOf(St);
    If J >= 0 Then
    Begin
      MeshType[J] := True;
      St := Zone.ExtraMeshes.Strings[I];
      If K > 0 Then St := Copy(St,K + 1,Length(St));
      MeshHash.Put(J,St); // Save the export name
    End;
  End; // For I

  // Encode each of the object types

  K := 0;
  For I := 0 To High(MeshType) Do
  Begin
    If MeshType[I] Then
    Begin
      List := TStringList.Create;
      TGroupObject(MeshLibrary.Objects[I]).AddToPolygonList(List,False,True,True,True);
      MO := Zone.CoalescePolygonList(List);
      MO.ConvertToTriangles;
//      MO.SetName(TGroupObject(MeshLibrary.Objects[I]).GetName);
      MO.SetName(MeshHash.Get(I)); // Use the export name
      EncodeMesh(MO);
      For J := 0 To List.Count - 1 Do List.Objects[J].Free;
      MO.Free;
      List.Free;
      Inc(K);
    End;
  End; // For I

  // Form a list of all textures we need

  For I := 0 To High(W.Fragments) Do
  Begin
    If (W.Fragments[I] <> Nil) And (W.Fragments[I].ID = 3) Then
    Begin
      D03 := Data03(W.Fragments[I]);
      St  := StrPas(@(D03.Data1[0][0]));
      If Tex.IndexOf(St) < 0 Then Tex.Add(St);
    End;
  End; // For I

  // Encode the .WLD file

  W.Encode(Stream);
  W.Free;

  // Cleanup

  SetLength(MeshType,0);
  MeshHash.Free;
  Result := Tex;
End; // TfrmMain.CreateObjWLD

Function TfrmMain.CreateChrWLD(Stream: TStream): TStringList;
Var
  I,J   : Integer;
  W     : TEQWldData;
  Tex   : TStringList;
  D03   : Data03;
  D31   : Data31;
  St    : String;
  List1 : TStringList;
  ZO    : TZoneObject;
  CL    : TCreatureLibraryObjectReference;

  Procedure EncodeCreature(An8: TAn8File; Const Model: String; W: TEQWLDData);
  Const
    ObjectIndex = 0;
    FigureIndex = 0;
    VertOffset  = -2.5;

  Var
    I,J,K,L,M     : Integer;
    MaxPt         : Points3D.T3DPoint;
    MinPt         : Points3D.T3DPoint;
    MaxPt0        : Points3D.T3DPoint;
    MinPt0        : Points3D.T3DPoint;
    Obj           : TAn8Object;
    Mat           : TAn8Material;
    Mesh          : TAn8Mesh;
    Mesh1         : TAn8Mesh;
    Point         : Points3D.T3DPoint;
    Face          : TAn8Face;
    Trans         : Boolean;
    D10           : Data10;
    D11           : Data11;
    D14           : Data14;
    D31           : Data31;
    D12s          : Array Of Data12;
    D13s          : Array Of Data13;
    D2Ds          : Array Of Data2D;
    D36s          : Array Of Data36;
    Textures      : TStringList;
    Scale         : Integer;
    V1            : Integer;
    Normal        : Points3D.T3DPoint;
    VX,VY,VZ      : Single;
    Texture       : String;
    Bones         : TStringList;
    NewBones      : TStringList;
    NewBones1     : TStringList;
    Bone          : TAn8Bone;
    Bone0         : TAn8Bone;
    Bone1         : TAn8Bone;
    Bone2         : TAn8Bone;
    Bone3         : TAn8Bone;
    Norm          : Single;
    Q,Q1          : TQuaternion;
    V             : Points3D.T3DPoint;
    NO            : TAn8NamedObject;
    S             : Single;
    Sequence      : TAn8Sequence;
    SeqName       : String;
    SeqPrefix     : String;
    DropLastFrame : Boolean;
    MSPerFrame    : Integer;
    JointAngleX   : TAn8JointAngle;
    JointAngleY   : TAn8JointAngle;
    JointAngleZ   : TAn8JointAngle;
    Sequences     : TStringList;
    NewMeshes     : TStringList;
    ObjectList    : TStringList;
    MaterialList  : TStringList;
    DZ,DZ1,ML     : Single;
    BaseMeshCount : Integer;
    Found         : Boolean;
    BaseIndex     : Array[0..1] Of Integer;
    SeparateHeads : Boolean;
    HeadCount     : Integer;
    BodyCount     : Integer;
    BodyMesh      : Integer;
    BodyTex       : Integer;
    HeadMesh      : Integer;
    HeadTex       : Integer;
    DoHeadOnly    : Boolean;
    VertSorter    : TQuickSorterProc;

    Function FindTextureInSprite(Face: TAn8Face; S: Data36): Integer;
    Var
      I,J,K   : Integer;
      Found   : Boolean;
      St      : String;
      Special : Integer;

    Begin
      If (Face.Material.Surface.Diffuse.Texture <> Nil) And
         (Face.Material.Surface.Diffuse.Texture.FileName <> '')
       Then St := Face.Material.Surface.Diffuse.Texture.Name
       Else St := TransparentTexture;
      I     := Textures.IndexOf(St);
      J     := 0;
      Found := False;
      While (J < S.Size7) And Not Found Do
      Begin
        If S.Data7[J].Data = I Then Found := True Else Inc(J);
      End; // While
      If Found Then
      Begin
        K     := 0;
        Found := False;
        While (K < S.Size8) And Not Found Do
        Begin
          If S.Data8[K].Data = I Then Found := True Else Inc(K);
        End; // While
        If Found Then Result := J + (K Shl 16) Else Result := -1;
      End
      Else Result := -1;
    End; // FindTextureInSprite

    Function FindBoneInSprite(Index: Integer; S: Data36): Integer;
    Var
      I     : Integer;
      Found : Boolean;

    Begin
      I     := 0;
      Found := False;
      While (I < S.Size6) And Not Found Do
      Begin
        If S.Data6[I].Data = Index Then Found := True Else Inc(I);
      End; // While
      If Found Then Result := I Else Result := -1;
    End; // FindBoneInSprite

    Procedure CreateTextureEntries(Obj: TAn8Object; AddToData31,HeadOnly,BodyOnly: Boolean);
    Var
      I         : Integer;
      Component : TObject;

      Procedure ProcessMesh(Mesh: TAn8Mesh);
      Var
        I,J : Integer;
        Mat : TAn8Material;
        St  : String;

      Begin
        // Make sure we want to process this mesh

        If ((Mesh.Name  = 'he') And Not BodyOnly) Or
           ((Mesh.Name <> 'he') And Not HeadOnly) Then
        Begin
          For J := 0 To Mesh.MaterialList.Count - 1 Do
          Begin
            Mat := TAn8Material(Mesh.MaterialList.Objects[J]);

            // Only process each material once

            If MaterialList.IndexOf(Mat.Name) < 0 Then
            Begin
              MaterialList.AddObject(Mat.Name,Mat);

              // The texture should be attached to the diffuse color; make sure there is one

              If (Mat.Surface.Diffuse.Texture <> Nil) And
                 (Mat.Surface.Diffuse.Texture.FileName <> '') Then
              Begin
                St := ExtractFileNameNoExt(UpperCase(Mat.Surface.Diffuse.Texture.FileName));

                // Strip off the three-character model prefix from the texture filename
                // because we want to use the prefix for our model instead, but ONLY if
                // the texture doesn't start with "clk".

                If (UpperCase(Copy(St,1,3)) <> 'CLK') And
                   (UpperCase(Copy(St,1,4)) <> 'HELM') Then St := Model + Copy(St,4,Length(St));

                // Make the texture entry

                If AddToData31
                   Then MakeTextureEntry(W,D31,ExtractFileNameNoExt(Mat.Surface.Diffuse.Texture.FileName),St,False,False)
                   Else MakeTextureEntry(W,Nil,ExtractFileNameNoExt(Mat.Surface.Diffuse.Texture.FileName),St,False,False);
                  Textures.Add(Mat.Surface.Diffuse.Texture.Name);
              End
              Else Trans := True;
            End;
          End; // For J
        End;
      End; // ProcessMesh

      Procedure ProcessGroup(Group: TAn8Group);
      Var
        I         : Integer;
        Component : TObject;

      Begin
        For I := 0 To Group.Components.Count - 1 Do
        Begin
          Component := Group.Components.Objects[I];
               If Component Is TAn8Mesh  Then ProcessMesh(TAn8Mesh(Component))
          Else If Component Is TAn8Group Then ProcessGroup(TAn8Group(Component));
        End; // For I
      End; // ProcessGroup

    Begin
      For I := 0 To Obj.Components.Count - 1 Do
      Begin
        Component := Obj.Components.Objects[I];
             If Component Is TAn8Mesh  Then ProcessMesh(TAn8Mesh(Component))
        Else If Component Is TAn8Group Then ProcessGroup(TAn8Group(Component));
      End; // For I
    End; // CreateTextureEntries

    Function CompareAn8Vertices(Index0,Index1: Integer): Integer;

      Function GetVertIndex(Index: Integer): Integer;
      Var
        I    : Integer;
        Face : TAn8Face;

      Begin
        I      := Integer(VertTexList.Objects[VertIndex[Index]]);
        Face   := TAn8Face(SortAn8Mesh.Faces.Objects[(I And $7FFFFFFF) Shr 2]);
        Result := TAn8Bone(SortAn8Mesh.PrimaryBones.Objects[Face.Points[I And 3]]).Index;
      End; // GetVertIndex

    Begin
      Result := GetVertIndex(Index0) - GetVertIndex(Index1);
    End; // CompareAn8Vertices

    Procedure ExchangeAn8Vertices(Index0,Index1: Integer);
    Var I: Integer;
    Begin
      I                 := VertIndex[Index0];
      VertIndex[Index0] := VertIndex[Index1];
      VertIndex[Index1] := I;
    End; // ExchangeAn8Vertices

  Begin
    // Make sure we have an object, a figure, and a sequence

    If (An8.Objects.Count > 0) And
       (An8.Figures.Count > 0) And
       (An8.Sequences.Count > 0) Then
    Begin
      // Create a list that will be used for vertices and texture coordinates. The reason for
      // this is that Anim8or files share vertices but have different texture coordinates and
      // WLD files don't, so we need a way to create unique instances.

      VertTexList := TStringList.Create;
      VertTexList.Sorted := True;

      // Don't rely on the Anim8or file to specify the order in which to process objects.
      // The first object must always be the one with the base body and head.

      ObjectList := TStringList.Create;
      Zone.BuildAn8ObjectList(An8,ObjectList,SeparateHeads);

      // Encode the materials

      Textures := TStringList.Create; // Contains texture names, in order of usage
      Obj      := TAn8Object(ObjectList.Objects[ObjectIndex]);
      Trans    := False;
      D31      := Data31.Create(W,Model + '_MP');

      // Iterate through all objects and do those materials that the meshes in each object reference

      MaterialList := TStringList.Create;
      MaterialList.Sorted := True;
      J := -1;
      For I := 0 To ObjectList.Count - 1 Do
      Begin
        If TokenizeAn8ObjectName(ObjectList.Strings[I],BodyMesh,BodyTex,HeadMesh,HeadTex) Then
        Begin
          // Every time we encounter a new body mesh, encode the textures for it.  In practice,
          // this means that there will be body textures encoded for the plain and first robed models

          DoHeadOnly := (BodyMesh = J) And (BodyMesh = 0);
          CreateTextureEntries(TAn8Object(ObjectList.Objects[I]),BodyTex < 11{True},DoHeadOnly,False);
          J := BodyMesh;
        End
        Else CreateTextureEntries(TAn8Object(ObjectList.Objects[I]),True,I > 0,False);
      End; // For I

      // If we need a transparent texture, add it to the end

      If Trans Then
      Begin
        Textures.Add(TransparentTexture);
        MakeTransparentTextureEntry(W,D31);
      End;

      // Add our texture list fragment

      AddFragment(W,D31);

      // Combine the meshes into head meshes and body meshes

      NewMeshes := TStringList.Create;
      Zone.BuildAn8HeadBodyMeshes(An8,ObjectList,NewMeshes,FigureIndex,SeparateHeads,False,BaseMeshCount,DZ,ML,HeadCount,BodyCount);
      DZ := DZ / 2;

      // Encode the meshes

      MinPt  := Points3D.T3DPoint.Create;
      MaxPt  := Points3D.T3DPoint.Create;
      MinPt0 := Points3D.T3DPoint.Create;
      MaxPt0 := Points3D.T3DPoint.Create;
      SetLength(D36s,NewMeshes.Count);
      For I := 0 To NewMeshes.Count - 1 Do
      Begin
        Mesh := TAn8Mesh(NewMeshes.Objects[I]);

        // Build the unique vertex-texture coordinate list

        VertTexList.Clear;
        For K := 0 To Mesh.Faces.Count - 1 Do
        Begin
          Face := TAn8Face(Mesh.Faces.Objects[K]);
          If High(Face.TexCoords) = High(Face.Points) Then
          Begin
            For J := 0 To High(Face.Points) Do
            Begin
              St := IntToStr(Face.Points[J]) + ',' + IntToStr(Face.TexCoords[J]);
              If VertTexList.IndexOf(St) < 0 Then VertTexList.AddObject(St,Pointer((K Shl 2) + J));
            End; // For J
          End
          Else
          Begin
            For J := 0 To High(Face.Points) Do
            Begin
              St := IntToStr(Face.Points[J]);
              If VertTexList.IndexOf(St) < 0 Then VertTexList.AddObject(St,Pointer(((K Shl 2) + J) Or $80000000));
            End; // For J
          End;
        End; // For K

        // Build the Data36 object

        D36s[I]       := Data36.Create(W,Model + Mesh.Name + '_DMSPRITEDEF');
        D36s[I].Flags := 3; // $1 = has Params1
        For J := 0 To Mesh.Points.Count - 1 Do
        Begin
          Point := Points3D.T3DPoint(Mesh.Points.Objects[J]);
          If J = 0 Then
          Begin
            MinPt.Copy(Point);
            MaxPt.Copy(Point);
          End
          Else
          Begin
            If Point.X < MinPt.X Then MinPt.X := Point.X;
            If Point.Y < MinPt.Y Then MinPt.Y := Point.Y;
            If Point.Z < MinPt.Z Then MinPt.Z := Point.Z;
            If Point.X > MaxPt.X Then MaxPt.X := Point.X;
            If Point.Y > MaxPt.Y Then MaxPt.Y := Point.Y;
            If Point.Z > MaxPt.Z Then MaxPt.Z := Point.Z;
          End;
        End; // For J

        MinPt.Multiply(ProgramSettings.WLDCreatureSize);
        MaxPt.Multiply(ProgramSettings.WLDCreatureSize);

        MinPt.Y := MinPt.Y - DZ;
        MaxPt.Y := MaxPt.Y - DZ;

        If I = 0 Then
        Begin
          MinPt0.Copy(MinPt);
          MaxPt0.Copy(MaxPt);
        End
        Else
        Begin
          If MinPt.X < MinPt0.X Then MinPt0.X := MinPt.X;
          If MinPt.Y < MinPt0.Y Then MinPt0.Y := MinPt.Y;
          If MinPt.Z < MinPt0.Z Then MinPt0.Z := MinPt.Z;
          If MaxPt.X > MaxPt0.X Then MaxPt0.X := MaxPt.X;
          If MaxPt.Y > MaxPt0.Y Then MaxPt0.Y := MaxPt.Y;
          If MaxPt.Z > MaxPt0.Z Then MaxPt0.Z := MaxPt.Z;
        End;
        D36s[I].Fragment1              := FragmentReference.Create;
        D36s[I].Fragment1.Fragment     := D31;
        D36s[I].Fragment2              := FragmentReference.Create;
        D36s[I].Fragment3              := FragmentReference.Create;
        D36s[I].Fragment4              := FragmentReference.Create;
        D36s[I].Params1[0]             := 0;
        D36s[I].Params1[1]             := 0;
        D36s[I].Params1[2]             := 0;
        D36s[I].Params2[0]             := 0;
        D36s[I].Params2[1]             := 0;
        D36s[I].Params2[2]             := 0;
        MaxPt.Subtract(MinPt);
        D36s[I].Radius                 := MaxPt.GetLength / 2;
        MaxPt.Add(MinPt);

        J := VertTexList.Count;
        D36s[I].Size1  := J;                               // Vertices
        D36s[I].Size2  := J;                               // Texture coordinates
        D36s[I].Size3  := J;                               // Normals
        D36s[I].Size4  := 0;                               // Colors
        D36s[I].Size5  := Mesh.Faces.Count;                // Polygons
        D36s[I].Size6  := 0;                               // Unknown
        D36s[I].Size7  := 0;                               // Polygon-texture counts
        D36s[I].Size8  := 0;                               // Vertex-texture counts
        D36s[I].Size9  := 0;                               // Unknown

        D36s[I].Size10 := 15;                              // Scale
        If D36s[I].Radius <> 0 Then While (D36s[I].Size10 > 0) And ((1 Shl D36s[I].Size10) > Trunc(32767 / D36s[I].Radius)) Do Dec(D36s[I].Size10);
        If MinPt.X <> 0 Then While (D36s[I].Size10 > 0) And ((1 Shl D36s[I].Size10) > Trunc(Abs(32767 / MinPt.X))) Do Dec(D36s[I].Size10);
        If MinPt.Y <> 0 Then While (D36s[I].Size10 > 0) And ((1 Shl D36s[I].Size10) > Trunc(Abs(32767 / MinPt.Y))) Do Dec(D36s[I].Size10);
        If MinPt.Z <> 0 Then While (D36s[I].Size10 > 0) And ((1 Shl D36s[I].Size10) > Trunc(Abs(32767 / MinPt.Z))) Do Dec(D36s[I].Size10);
        If MaxPt.X <> 0 Then While (D36s[I].Size10 > 0) And ((1 Shl D36s[I].Size10) > Trunc(Abs(32767 / MaxPt.X))) Do Dec(D36s[I].Size10);
        If MaxPt.Y <> 0 Then While (D36s[I].Size10 > 0) And ((1 Shl D36s[I].Size10) > Trunc(Abs(32767 / MaxPt.Y))) Do Dec(D36s[I].Size10);
        If MaxPt.Z <> 0 Then While (D36s[I].Size10 > 0) And ((1 Shl D36s[I].Size10) > Trunc(Abs(32767 / MaxPt.Z))) Do Dec(D36s[I].Size10);

        // Set the scale

        Scale := 1 Shl D36s[I].Size10;

        // Sort the vertices by bone

        SetLength(VertIndex,VertTexList.Count);
        For J := 0 To High(VertIndex) Do VertIndex[J] := J;

        SortAn8Mesh         := Mesh;
        VertSorter          := TQuickSorterProc.Create;
        VertSorter.Compare  := @CompareAn8Vertices;
        VertSorter.Exchange := @ExchangeAn8Vertices;
        VertSorter.Sort(0,High(VertIndex));
        VertSorter.Free;

        // Allocate Data36 arrays

        SetLength(D36s[I].Data1,D36s[I].Size1);
        SetLength(D36s[I].Data2,D36s[I].Size2);
        SetLength(D36s[I].Data3,D36s[I].Size3);
        SetLength(D36s[I].Data4,D36s[I].Size4);
        SetLength(D36s[I].Data5,D36s[I].Size5 * 4);
        SetLength(D36s[I].Data6,D36s[I].Size6);
        SetLength(D36s[I].Data7,D36s[I].Size7);
        SetLength(D36s[I].Data8,D36s[I].Size8);

        // Allocate some helpful objects

        Q  := TQuaternion.Create;
        Q1 := TQuaternion.Create;
        V  := Points3D.T3DPoint.Create;

        // Load the points

        NO := TAn8NamedObject(TAn8Figure(An8.Figures.Objects[FigureIndex]).RootBone.FindNamedObjects.Objects[0]);
        For K := 0 To VertTexList.Count - 1 Do
        Begin
          J    := Integer(VertTexList.Objects[VertIndex[K]]);
          Face := TAn8Face(Mesh.Faces.Objects[(J And $7FFFFFFF) Shr 2]);
          L    := Face.Points[J And 3];

          // Get the bone for this point

          Bone := TAn8Bone(Mesh.PrimaryBones.Objects[L]);

          // Each vertex has a normal vector for phong shading

          Normal := Points3D.T3DPoint(Mesh.Normals.Objects[L]);

          // Get the point

          Point := Points3D.T3DPoint(Mesh.Points.Objects[L]);

          // Texture coordinates are bound to vertices, not polygons. That means that
          // polygons which meet at the same vertex have to have the same texture
          // coordinates at that point.

          If High(Face.TexCoords) = High(Face.Points) Then
          Begin
            L := Face.TexCoords[J And 3];
            D36s[I].Data2[K].I1 := Round(TAn8TexCoord(Mesh.TexCoords.Objects[L]).TX * 256);
            D36s[I].Data2[K].I2 := Round(TAn8TexCoord(Mesh.TexCoords.Objects[L]).TZ * 256);
          End
          Else
          Begin
            D36s[I].Data2[K].I1 := 0;
            D36s[I].Data2[K].I2 := 0;
          End;

          // Transform the normal

          V.Copy(Normal);
          Q.Copy(Bone.Orientation);
          Bone1 := Bone.Parent;
          While Bone1 <> Nil Do
          Begin
            Q1.Copy(Bone1.Orientation);
            Q1.Multiply(Q);
            Q.Copy(Q1);
            Bone1 := Bone1.Parent;
          End; // While
          Q.Invert;
          Q.Transform(V);

          D36s[I].Data3[K].X := Round(V.Z * 127);
          D36s[I].Data3[K].Y := Round(V.X * 127);
          D36s[I].Data3[K].Z := Round(V.Y * 127);

          // Transform the point

          V.Copy(Point);
          V.Subtract(Bone.Origin);

          If NO <> Nil Then V.Add(NO.Base);
          Q.Copy(Bone.Orientation);
          Bone1 := Bone.Parent;
          While Bone1 <> Nil Do
          Begin
            Q1.Copy(Bone1.Orientation);
            Q1.Multiply(Q);
            Q.Copy(Q1);
            Bone1 := Bone1.Parent;
          End; // While
          Q.Invert;
          Q.Transform(V);

          VX    := V.X;
          VY    := V.Y;
          VZ    := V.Z;
          VX    := VX * Scale * ProgramSettings.WLDCreatureSize;
          VY    := VY * Scale * ProgramSettings.WLDCreatureSize;
          VZ    := VZ * Scale * ProgramSettings.WLDCreatureSize;

          D36s[I].Data1[K].X := Round(VZ);
          D36s[I].Data1[K].Y := Round(VX);
          D36s[I].Data1[K].Z := Round(VY);
        End; // For K

        // The vertex array is sorted by bone index.  If we have already processed a
        // vertex with this bone, then increment the number of vertices found. Otherwise
        // make a new entry for this vertex.

        For K := 0 To VertTexList.Count - 1 Do
        Begin
          J    := Integer(VertTexList.Objects[VertIndex[K]]);
          Face := TAn8Face(SortAn8Mesh.Faces.Objects[(J And $7FFFFFFF) Shr 2]);
          L    := Face.Points[J And 3];
          Bone := TAn8Bone(Mesh.PrimaryBones.Objects[L]);
          J    := FindBoneInSprite(Bone.Index,D36s[I]);
          If J >= 0 Then Inc(D36s[I].Data6[J].NumVertices)
          Else
          Begin
            Inc(D36s[I].Size6);
            SetLength(D36s[I].Data6,D36s[I].Size6);
            D36s[I].Data6[D36s[I].Size6 - 1].NumVertices := 1;
            D36s[I].Data6[D36s[I].Size6 - 1].Data        := Bone.Index;
          End;
        End; // For K

        // Sort the faces by texture

        Zone.SortAn8FacesByTexture(Mesh,Textures);

        // Change the vertex-texcoord list so it returns the sorted index

        For K := 0 To High(VertIndex) Do VertTexList.Objects[VertIndex[K]] := Pointer(K);

        // Load the faces

        For K := 0 To Mesh.Faces.Count - 1 Do
        Begin
          Face := TAn8Face(Mesh.Faces.Objects[TexIndex[K]]);

          D36s[I].Data5[K].Flags := 0; // Cannot pass through the polygon

          // Find the polygon's vertices and add them to the sprite

          // While it may be tempting to get rid of redundant vertices, remember that
          // vertices also have texture coordinates mapped to them.  To allow neighboring
          // polygons to end on different texture coordinates, they have to have different
          // vertices.  From a space standpoint, it would be more efficient to eliminate
          // those vertices where all the normals and texture coordinates are the same.
          // Perhaps in a future version...

          For J := 0 To High(Face.Points) Do
          Begin
            // Build the lookup string from the face

            If High(Face.TexCoords) = High(Face.Points)
             Then St := IntToStr(Face.Points[J]) + ',' + IntToStr(Face.TexCoords[J])
             Else St := IntToStr(Face.Points[J]);

            // Find the vertex index

            L := VertTexList.IndexOf(St);
            If L >= 0
             Then L := Integer(VertTexList.Objects[L])
             Else L := 0; // Should never happen
            D36s[I].Data5[K].Vertex[J] := L;
          End; // For J

          // The number of polygons array is sorted by texture (and indirectly, the
          // vertex array as well).  If we have already processed a polygon with this
          // texture, then increment the number of polygons found and increase the
          // vertex count. Otherwise make new entries for this texture.

          J := FindTextureInSprite(Face,D36s[I]);
          If J >= 0 Then
          Begin
            Inc(D36s[I].Data7[J And $FFFF].NumPolys);
            Inc(D36s[I].Data8[J Shr 16].NumVertices,High(Face.Points) + 1);
          End
          Else
          Begin
            If (Face.Material.Surface.Diffuse.Texture <> Nil) And
               (Face.Material.Surface.Diffuse.Texture.FileName <> '')
             Then Texture := Face.Material.Surface.Diffuse.Texture.Name
             Else Texture := TransparentTexture;
            J := Textures.IndexOf(Texture);
            If J >= 0 Then
            Begin
              Inc(D36s[I].Size7);
              Inc(D36s[I].Size8);
              SetLength(D36s[I].Data7,D36s[I].Size7);
              SetLength(D36s[I].Data8,D36s[I].Size8);
              D36s[I].Data7[D36s[I].Size7 - 1].NumPolys    := 1;
              D36s[I].Data7[D36s[I].Size7 - 1].Data        := J;
              D36s[I].Data8[D36s[I].Size8 - 1].NumVertices := High(Face.Points) + 1;
              D36s[I].Data8[D36s[I].Size8 - 1].Data        := J;
            End;
          End;
        End; // For K

        // Cleanup

        Q.Free;
        Q1.Free;
        V.Free;

        // Add the mesh fragment

        AddFragment(W,D36s[I]);
      End; // For I

      // Get the list of bones since they will be used often

      Bones := TStringList.Create;
      TAn8Figure(An8.Figures.Objects[FigureIndex]).RootBone.GetBones(Bones);

      // Add Data12 and Data13 entries for the base skeleton

      SetLength(D12s,Bones.Count);
      SetLength(D13s,Bones.Count);
      For I := 0 To Bones.Count - 1 Do
      Begin
        Bone := TAn8Bone(Bones.Objects[I]);
        If I > 0
         Then St := UpperCase(Bone.Name)
         Else St := '';

        // Create the Data12

        D12s[I] := Data12.Create(W,Model + St + '_TRACKDEF');
        D12s[I].Size1 := 1;
        D12s[I].Flags := 8;
        SetLength(D12s[I].Data4,D12s[I].Size1);

        D12s[I].Data4[0].E0 := Round(Bone.Orientation.W * 16384);
        D12s[I].Data4[0].E1 := Round(Bone.Orientation.Z * 16384);
        D12s[I].Data4[0].E2 := Round(Bone.Orientation.X * 16384);
        D12s[I].Data4[0].E3 := Round(Bone.Orientation.Y * 16384);
        VX := 0;
        VY := 1;
        VZ := 0;

        If I = 1 Then VY := 0;
        If I > 0
         Then S := Bone.Parent.Len * ProgramSettings.WLDCreatureSize
         Else S := -DZ * ProgramSettings.WLDCreatureSize;

        D12s[I].Data4[0].MoveXNumerator  := Round(256 * VZ * S);
        D12s[I].Data4[0].MoveYNumerator  := Round(256 * VX * S);
        D12s[I].Data4[0].MoveZNumerator  := Round(256 * VY * S);

             If Copy(St,Length(St) - 5,6) = '_POINT' Then D12s[I].Data4[0].MoveDenominator := 231
        Else If I = 1 Then D12s[I].Data4[0].MoveDenominator := 284
        Else D12s[I].Data4[0].MoveDenominator := 256;

        // Create the Data13 and point it to the Data12

        D13s[I] := Data13.Create(W,Model + St + '_TRACK');
        D13s[I].Flags := 4;
        D13s[I].Fragment := FragmentReference.Create;
        D13s[I].Fragment.Fragment := D12s[I];

        AddFragment(W,D12s[I]);
        AddFragment(W,D13s[I]);
      End; // For I

      // Add a Data2D entry for the first body-head pair of meshes (the head mesh is first)

      SetLength(D2Ds,BaseMeshCount);
      For I := 0 To High(D2Ds) Do
      Begin
        D2Ds[I] := Data2D.Create(W);
        D2Ds[I].Fragment := FragmentReference.Create;
        D2Ds[I].Fragment.Fragment := D36s[I];
        AddFragment(W,D2Ds[I]);
      End; // For I

      If BaseMeshCount = 2 Then
      Begin
        // Point the second Data2D to the first body mesh (the second body mesh will be the robed mesh)

        If BodyCount = 1
         Then D2Ds[1].Fragment.Fragment := D36s[Min(HeadCount,High(D36s))]
         Else D2Ds[1].Fragment.Fragment := D36s[Min(HeadCount + 1,High(D36s))];

        // If there are four or more heads, then the last head is the first mesh...in that case,
        // the first head is the second mesh (or the third mesh if there is a robed mesh).

        If HeadCount >= 4 Then
        Begin
          If BodyCount = 1
           Then D2Ds[0].Fragment.Fragment := D36s[1]
           Else D2Ds[0].Fragment.Fragment := D36s[2];
        End
        Else D2Ds[0].Fragment.Fragment := D36s[0];
      End;

      // Add a Data10 entry for the skeleton

      D10          := Data10.Create(W,Model + '_HS_DEF');
      D10.Flags    := $202;
      D10.Params2  := Max(MaxPt0.X - MinPt0.X,Max(MaxPt0.Z - MinPt0.Z,MaxPt0.Y - MinPt0.Y));
      D10.Size1    := Bones.Count;
      D10.Size2    := High(D2Ds) + 1;
      D10.Fragment := FragmentReference.Create;
      SetLength(D10.Data1,D10.Size1);
      SetLength(D10.Data2,D10.Size2);
      SetLength(D10.Data3,D10.Size2);
      For I := 0 To D10.Size1 - 1 Do
      Begin
        Bone := TAn8Bone(Bones.Objects[I]);
        If I > 0
         Then St := UpperCase(Bone.Name)
         Else St := '';
        D10.Data1[I].Name      := Model + St + '_DAG';
        D10.Data1[I].Flags     := 0;
        D10.Data1[I].Fragment1 := FragmentReference.Create;
        D10.Data1[I].Fragment2 := FragmentReference.Create;
        D10.Data1[I].Fragment1.Fragment := D13s[I];
        D10.Data1[I].Size := Bone.Children.Count;
        SetLength(D10.Data1[I].Data,D10.Data1[I].Size);
        For J := 0 To D10.Data1[I].Size - 1 Do D10.Data1[I].Data[J] := TAn8Bone(Bone.Children.Objects[J]).Index;
      End; // For I
      For I := 0 To D10.Size2 - 1 Do
      Begin
        D10.Data2[I] := FragmentReference.Create;
        D10.Data2[I].Fragment := D2Ds[I];
        D10.Data3[I] := 2;
      End; // For I
      AddFragment(W,D10);

      // Sort the sequences by prefix (otherwise they won't work)

      Sequences := TStringList.Create;
      For J := 0 To An8.Sequences.Count - 1 Do
      Begin
        Sequence := TAn8Sequence(An8.Sequences.Objects[J]);
        TokenizeAn8SequenceName(Sequence.Name,SeqName,SeqPrefix,DropLastFrame,MSPerFrame);
        SeqPrefix := UpperCase(SeqPrefix);
        Sequences.AddObject(SeqPrefix,Pointer(J));
      End; // For J
      Sequences.Sort;

      // Add Data12 and Data13 entries for all alternate animations

      For J := 0 To An8.Sequences.Count - 1 Do
      Begin
        Sequence := TAn8Sequence(An8.Sequences.Objects[Integer(Sequences.Objects[J])]);
        TokenizeAn8SequenceName(Sequence.Name,SeqName,SeqPrefix,DropLastFrame,MSPerFrame);
        SeqPrefix := UpperCase(SeqPrefix);

        // For this sequence, get the overall height of the model

        NewBones  := TStringList.Create;
        NewBones1 := TStringList.Create;
        V         := Points3D.T3DPoint.Create;
        Q         := TQuaternion.Create;
        Q1        := TQuaternion.Create;
        Bone0     := Sequence.GetRootBone(0);
        Bone0.GetBones(NewBones);
        For I := 0 To BaseMeshCount - 1 Do
        Begin
          Mesh := TAn8Mesh(NewMeshes.Objects[I]);
          For K := 0 To Mesh.Points.Count - 1 Do
          Begin
            V.Copy(Points3D.T3DPoint(Mesh.Points.Objects[K]));
            Bone := TAn8Bone(Mesh.PrimaryBones.Objects[K]);

            // Reverse transform up to the root bone, saving a list of the alias bones

            NewBones1.Clear;
            NewBones1.AddObject('',TAn8Bone(NewBones.Objects[Bone.Index]));
            V.Subtract(Bone.Origin);
            If NO <> Nil Then V.Add(NO.Base);
            Q.Copy(Bone.Orientation);
            Bone1 := Bone.Parent;
            While Bone1 <> Nil Do
            Begin
              NewBones1.AddObject('',TAn8Bone(NewBones.Objects[Bone1.Index]));
              Q1.Copy(Bone1.Orientation);
              Q1.Multiply(Q);
              Q.Copy(Q1);
              Bone1 := Bone1.Parent;
            End; // While
            Q.Invert;
            Q.Transform(V);

            // Transform back down, using the new bones

            For L := NewBones1.Count - 1 DownTo 0 Do
            Begin
              Bone := TAn8Bone(NewBones1.Objects[L]);
              Bone.Orientation.Transform(V);
            End; // For L
            V.Add(Bone.Origin);

            // Check the point against the extents

            If (I = 0) And (K = 0) Then
            Begin
              MinPt.Copy(V);
              MaxPt.Copy(V);
            End
            Else
            Begin
              If Point.X < MinPt.X Then MinPt.X := Point.X;
              If Point.Y < MinPt.Y Then MinPt.Y := Point.Y;
              If Point.Z < MinPt.Z Then MinPt.Z := Point.Z;
              If Point.X > MaxPt.X Then MaxPt.X := Point.X;
              If Point.Y > MaxPt.Y Then MaxPt.Y := Point.Y;
              If Point.Z > MaxPt.Z Then MaxPt.Z := Point.Z;
            End;
          End; // For K
        End; // For I
        DZ1 := (MaxPt.Y - MinPt.Y) / 2;
        Q1.Free;
        Q.Free;
        V.Free;
        NewBones1.Free;
        NewBones.Free;
        Bone0.Free;

        For I := 0 To Bones.Count - 1 Do
        Begin
          Bone := TAn8Bone(Bones.Objects[I]);
          If I > 0
           Then St := UpperCase(Bone.Name)
           Else St := '';

          // Don't show bones for holding items, showing the creature's name, etc.

          If Copy(St,Length(St) - 5,6) <> '_POINT' Then
          Begin
            // Create the Data12

            D12s[I] := Data12.Create(W,SEQPrefix + Model + St + '_TRACKDEF');
            If DropLastFrame
             Then D12s[I].Size1 := Sequence.Frames - 1
             Else D12s[I].Size1 := Sequence.Frames;

            // Create a basic quaternion, a work quaternion, and a work point

            Q  := TQuaternion.Create;
            Q1 := TQuaternion.Create;
            V  := Points3D.T3DPoint.Create;

            // Find the joint angles

            L := Sequence.JointAngles.IndexOf(Bone.Name + '_X');
            If L >= 0
             Then JointAngleX := TAn8JointAngle(Sequence.JointAngles.Objects[L])
             Else JointAngleX := Nil;

            L := Sequence.JointAngles.IndexOf(Bone.Name + '_Y');
            If L >= 0
             Then JointAngleY := TAn8JointAngle(Sequence.JointAngles.Objects[L])
             Else JointAngleY := Nil;

            L := Sequence.JointAngles.IndexOf(Bone.Name + '_Z');
            If L >= 0
             Then JointAngleZ := TAn8JointAngle(Sequence.JointAngles.Objects[L])
             Else JointAngleZ := Nil;

            If ((JointAngleX = Nil) Or ((JointAngleX.Track.Keys.Count = 1) And (TAn8FloatKey(JointAngleX.Track.Keys.Objects[0]).Frame = 0))) And
               ((JointAngleY = Nil) Or ((JointAngleY.Track.Keys.Count = 1) And (TAn8FloatKey(JointAngleY.Track.Keys.Objects[0]).Frame = 0))) And
               ((JointAngleZ = Nil) Or ((JointAngleZ.Track.Keys.Count = 1) And (TAn8FloatKey(JointAngleZ.Track.Keys.Objects[0]).Frame = 0))) Then D12s[I].Size1 := 1;

            D12s[I].Flags := 8;
            SetLength(D12s[I].Data4,D12s[I].Size1);

            // Loop through the frames

            For K := 0 To D12s[I].Size1 - 1 Do
            Begin
              // Look for the X, Y, Z components of the adjustment to this bone

              Q.Copy(Bone.Orientation);

              If JointAngleX <> Nil Then
              Begin
                V.Copy(1,0,0);
                Q.Transform(V);
                Q1.FromAngleAxis(JointAngleX.Track.GetValueForFrame(K) * Pi / 180,V);
                Q1.Multiply(Q);
                Q.Copy(Q1);
              End;

              If JointAngleZ <> Nil Then
              Begin
                V.Copy(0,0,1);
                Q.Transform(V);
                Q1.FromAngleAxis(JointAngleZ.Track.GetValueForFrame(K) * Pi / 180,V);
                Q1.Multiply(Q);
                Q.Copy(Q1);
              End;

              If JointAngleY <> Nil Then
              Begin
                V.Copy(0,1,0);
                Q.Transform(V);
                Q1.FromAngleAxis(JointAngleY.Track.GetValueForFrame(K) * Pi / 180,V);
                Q1.Multiply(Q);
                Q.Copy(Q1);
              End;

              D12s[I].Data4[K].E0 := Round(Q.W * 16384);
              D12s[I].Data4[K].E1 := Round(Q.Z * 16384);
              D12s[I].Data4[K].E2 := Round(Q.X * 16384);
              D12s[I].Data4[K].E3 := Round(Q.Y * 16384);

              VX := 0;
              VY := 1;
              VZ := 0;
              If I = 1 Then VY := 0;
              If I > 0
               Then S := Bone.Parent.Len * ProgramSettings.WLDCreatureSize
               Else S := -DZ * ProgramSettings.WLDCreatureSize;
              D12s[I].Data4[K].MoveXNumerator  := Round(256 * VZ * S{Bone.Len});
              D12s[I].Data4[K].MoveYNumerator  := Round(256 * VX * S{Bone.Len});
              D12s[I].Data4[K].MoveZNumerator  := Round(256 * VY * S{Bone.Len});
//              D12s[I].Data4[K].MoveDenominator := 256;

                   If Copy(St,Length(St) - 5,6) = '_POINT' Then D12s[I].Data4[K].MoveDenominator := 231
              Else If I = 1 Then D12s[I].Data4[K].MoveDenominator := 284
              Else D12s[I].Data4[K].MoveDenominator := 256;

            End; // For K

            // Create the Data13 and point it to the Data12

            D13s[I] := Data13.Create(W,SeqPrefix + Model + St + '_TRACK');
            D13s[I].Fragment := FragmentReference.Create;
            D13s[I].Fragment.Fragment := D12s[I];
            If D12s[I].Size1 > 1 Then
            Begin
              D13s[I].Flags := 5;
              D13s[I].Params1[0] := MSPerFrame;
            End
            Else D13s[I].Flags := 4;

            AddFragment(W,D12s[I]);
            AddFragment(W,D13s[I]);

            V.Free;
            Q1.Free;
            Q.Free;
          End;
        End; // For I
      End; // For J
      Sequences.Free;

      // Add alternate texture sets

      // Look for any other head textures

      For I := 1 To 4 Do
      Begin
        J     := 0;
        Found := False;
        While (J < ObjectList.Count) And Not Found Do
        Begin
          If TokenizeAn8ObjectName(ObjectList.Strings[J],BodyMesh,BodyTex,HeadMesh,HeadTex) And (HeadTex = I) Then
//          If Copy(ObjectList.Strings[J],1,8) = 'body00' + Format('%.2d',[I]) Then
          Begin
            Found := True;
            CreateTextureEntries(TAn8Object(ObjectList.Objects[J]),False,False,SeparateHeads);
          End
          Else Inc(J);
        End; // While
      End; // For I

      // Look for any other body textures

      For I := 1 To 4 Do
      Begin
        J     := 0;
        Found := False;
        While (J < ObjectList.Count) And Not Found Do
        Begin
          If TokenizeAn8ObjectName(ObjectList.Strings[J],BodyMesh,BodyTex,HeadMesh,HeadTex) And (BodyTex = I) Then
          Begin
            Found := True;
            CreateTextureEntries(TAn8Object(ObjectList.Objects[J]),False,False,SeparateHeads);
          End
          Else Inc(J);
        End; // While
      End; // For I

      // Look for alternate faces (for player models)

      For I := 1 To 7 Do
      Begin
        J     := 0;
        Found := False;
        K := An8.Materials.IndexOf(Format('face_%.2d',[I]));
        If K >= 0 Then
        Begin
          Mat := TAn8Material(An8.Materials.Objects[K]);
          If MaterialList.IndexOf(Mat.Name) < 0 Then
          Begin
            MaterialList.AddObject(Mat.Name,Mat);

            // The texture should be attached to the diffuse color

            If (Mat.Surface.Diffuse.Texture <> Nil) And
               (Mat.Surface.Diffuse.Texture.FileName <> '') Then
            Begin
              St := ExtractFileNameNoExt(UpperCase(Mat.Surface.Diffuse.Texture.FileName));
              St := Copy(St,4,Length(St));
              MakeTextureEntry(W,Nil,ExtractFileNameNoExt(Mat.Surface.Diffuse.Texture.FileName),Model + St,False,False);
              Textures.Add(Mat.Surface.Diffuse.Texture.Name);
            End
            Else Trans := True;
          End;
        End;
      End; // For I

      // Add a Data11 entry and point it to the Data10 entry

      D11 := Data11.Create(W);
      D11.Fragment := FragmentReference.Create;
      D11.Fragment.Fragment := D10;
      AddFragment(W,D11);

      // Add a Data14 entry and point it to the Data11 entry

      D14                        := Data14.Create(W,Model + '_ACTORDEF');
      D14.Fragment1              := FragmentReference.Create;
      D14.Fragment1.Fragment     := Nil;
      D14.Fragment1.Name         := '(SPRITECALLBACK)';
      D14.Flags                  := 0; // This should be $80 if we have meshes tied to individual bones (which we don't do)
      D14.Size1                  := 1;
      SetLength(D14.Data1,1);
      D14.Data1[0].Size          := 1;
      SetLength(D14.Data1[0].Data,1);
      D14.Data1[0].Data[0].Data1 := 0;
      D14.Data1[0].Data[0].Data2 := 1.00000001504747E30;
      D14.Size2 := 1;
      SetLength(D14.Data2,1);
      D14.Data2[0]               := FragmentReference.Create;
      D14.Data2[0].Fragment      := D11;
      AddFragment(W,D14);

      // Cleanup

      VertTexList.Free;
      Bones.Free;
      SetLength(D12s,0);
      SetLength(D13s,0);
      SetLength(D2Ds,0);
      SetLength(D36s,0);
      SetLength(TexIndex,0);
      SetLength(VertIndex,0);
      MinPt.Free;
      MaxPt.Free;
      MinPt0.Free;
      MaxPt0.Free;
      Textures.Free;
      For I := 0 To NewMeshes.Count - 1 Do NewMeshes.Objects[I].Free;
      NewMeshes.Free;
      ObjectList.Free;
      MaterialList.Free;
    End;
  End; // EncodeCreature

Begin
  // Create the texture list

  Tex := TStringList.Create;

  // Create the .WLD file, with room for 2 packets

  W := TEQWldData.Create;
  W.nFragment := 1;
  SetLength(W.Fragments,1);

  // The first packet is always empty

  W.Fragments[0] := Nil;

  // Encode the creatures to be added

  List1 := TStringList.Create;

  // First export the creatures referenced by zone objects

  For I := 0 To Zone.Count - 1 Do
  Begin
    ZO := TZoneObject(Zone.Objects[I]);
    If ZO Is TCreatureLibraryObjectReference Then
    Begin
      CL := TCreatureLibraryObjectReference(ZO);
      If List1.IndexOfObject(CL.An8File) < 0 Then
      Begin
        CL.LoadCreature;
        J := CreatureLibrary.IndexOfObject(CL.An8File);
        If J >= 0 Then // Should always be true
        Begin
          EncodeCreature(CL.An8File,UpperCase(CreatureLibrary.Strings[J]),W);
          List1.AddObject('',CL.An8File);
        End;
      End;
    End;
  End; // For I

  // Now export creatures the user has explicitly added

  For I := 0 To Zone.Creatures.Count - 1 Do
  Begin
    St := Zone.Creatures.Strings[I];
    J  := Pos(',',St);
    If J > 0 Then St := UpperCase(Copy(St,1,J - 1));

    // If the creature hasn't yet been imported, import it now

    If (Zone.Creatures.Objects[I] = Nil) Or Not TAn8File(Zone.Creatures.Objects[I]).Loaded Then Zone.ImportCreature(I);

    If List1.IndexOfObject(TAn8File(Zone.Creatures.Objects[I])) < 0 Then
    Begin
      EncodeCreature(TAn8File(Zone.Creatures.Objects[I]),St,W);
      List1.AddObject('',TAn8File(Zone.Creatures.Objects[I]));
    End;
  End; // For I
  List1.Free;

  // Form a list of all textures we need

  For I := 0 To High(W.Fragments) Do
  Begin
    If (W.Fragments[I] <> Nil) And (W.Fragments[I].ID = 3) Then
    Begin
      D03 := Data03(W.Fragments[I]);
      St  := StrPas(@(D03.Data1[0][0]));
      If Tex.IndexOf(St) < 0 Then Tex.Add(St);
    End;
  End; // For I

  // Encode the .WLD file

  W.Encode(Stream);
  W.Free;

  // Return the texture list

  Result := Tex;
End; // TfrmMain.CreateChrWLD

Procedure TfrmMain.CreateLightsWLD(Stream: TStream);
Var
  W           : TEQWldData;
  I           : Integer;
  BaseLoc     : T3DPoint;
  BaseHeading : THeading;
  BaseScale   : T3DPoint;
  Lights      : Integer;

  Procedure AddLight(ZO: TZoneObject);
  Var
    I     : Integer;
    GO    : TGroupObject;
    ML    : TMeshLibraryObjectReference;
    Light : TLightObject;
    Loc   : T3DPoint;

    Procedure AddLightRef(Light: TLightObject; Loc: T3DPoint);
    Var
      D1B   : Data1B;
      D1C   : Data1C;
      D28   : Data28;

    Begin
      Loc.Multiply(BaseScale);
      BaseHeading.Rotate(Loc);
      Loc.Add(BaseLoc);

      D1B                   := Data1B.Create(W,'L' + IntToStr(Lights) + '_LDEF');
      D1B.Flags             := $1E;
      D1B.Params2           := 1;
      D1B.Params3b          := 200;
      D1B.Params4[0]        := 1;
      D1B.Params4[1]        := TBGRA(Light.Color).R / 255;
      D1B.Params4[2]        := TBGRA(Light.Color).G / 255;
      D1B.Params4[3]        := TBGRA(Light.Color).B / 255;
//      D1B.Params4[0]        := 1;
//      D1B.Params4[1]        := 1;
//      D1B.Params4[2]        := 1;
//      D1B.Params4[3]        := 0.235294103622437;

      D1C                   := Data1C.Create(W);
      D1C.Fragment          := FragmentReference.Create;
      D1C.Fragment.Fragment := D1B;
      D1C.Flags             := 0;

      D28                   := Data28.Create(W);
      D28.Fragment          := FragmentReference.Create;
      D28.Fragment.Fragment := D1C;
      D28.Flags             := $100;
      D28.X                 := Loc.X;
      D28.Y                 := Loc.Y;
      D28.Z                 := Loc.Z;
//      D28.Params1[0]        := L.X + Light.Loc.X;
//      D28.Params1[1]        := L.Y + Light.Loc.Y;
//      D28.Params1[2]        := L.Z + Light.Loc.Z;
      D28.Radius            := Light.Radius;

      Inc(W.nFragment,3);
      SetLength(W.Fragments,High(W.Fragments) + 4);
      Inc(Lights);
      W.Fragments[W.nFragment - 3] := D1B;
      W.Fragments[W.nFragment - 2] := D1C;
      W.Fragments[W.nFragment - 1] := D28;
    End; // AddLightRef

  Begin
    If ZO Is TLightObject Then
    Begin
      Light := TLightObject(ZO);
      Loc   := Light.GetAbsoluteLocation;
      AddLightRef(Light,Loc);
      Loc.Free;
    End
    Else If ZO Is TMeshLibraryObjectReference Then
    Begin
      Loc := ZO.GetAbsoluteLocation;
      BaseLoc.Add(Loc);
      BaseHeading.Copy(ZO.Rotate);
      BaseScale.Multiply(ZO.Size);
      ML := TMeshLibraryObjectReference(ZO);
      If ML.Group <> Nil Then AddLight(ML.Group);
      BaseLoc.Subtract(Loc);
      BaseHeading.Copy(0,0,0);
      If ZO.Size.X <> 0 Then BaseScale.X := BaseScale.X / ZO.Size.X;
      If ZO.Size.Y <> 0 Then BaseScale.Y := BaseScale.Y / ZO.Size.Y;
      If ZO.Size.Z <> 0 Then BaseScale.Z := BaseScale.Z / ZO.Size.Z;
      Loc.Free;
    End
    Else If ZO Is TGroupObject Then
    Begin
      GO := TGroupObject(ZO);
      For I := 0 To GO.Objects.Count - 1 Do AddLight(TZoneObject(GO.Objects.Objects[I]));
    End;
  End; // AddLight

Begin
  // Create the .WLD file, with room for 1 packet

  W := TEQWldData.Create;
  W.nFragment := 1;
  SetLength(W.Fragments,1);
  Lights := 0;

  // The first packet is always empty

  W.Fragments[0] := Nil;

  // Create light packets for every light source we can find in the zone

  BaseLoc     := T3DPoint.Create;
  BaseHeading := THeading.Create;
  BaseScale   := T3DPoint.Create;
  For I := 0 To Zone.Count - 1 Do AddLight(TZoneObject(Zone.Objects[I]));
  BaseScale.Free;
  BaseHeading.Free;
  BaseLoc.Free;

  // Encode the .WLD file

  W.Encode(Stream);
  W.Free;
End; // TfrmMain.CreateLightsWLD

Procedure TfrmMain.CreateEmptyLightsWLD(Stream: TStream);
Var
  W   : TEQWldData;
//  D1B : Data1B;

Begin
  // Create the .WLD file, with room for 2 packets

  W := TEQWldData.Create;
  W.nFragment := 1;//2;
  SetLength(W.Fragments,1);//2);

  // The first packet is always empty

  W.Fragments[0] := Nil;



{
  // Create a single light packet

  D1B          := Data1B.Create(W,'L0_LDEF');
  D1B.Flags    := $1E;
  D1B.Params2  := 1;
  D1B.Params3b := 200;

  // Add the light packet to the .WLD file

  W.Fragments[1] := D1B;
}



  // Encode the .WLD file

  W.Encode(Stream);
  W.Free;
End; // TfrmMain.CreateEmptyLightsWLD

Procedure TfrmMain.CreateObjectsWLD(Stream: TStream; ZoneMesh: TMeshObject);
Var
  W             : TEQWldData;
  I             : Integer;
  EncodedCount  : Integer;
  LightTreeHash : TLightTreeHash;
//  Meshes       : Array Of TMeshObject;
//  Lights       : TStringList;

  Procedure EncodeObject(ZO: TZoneObject);
  Var
    I,J,K,L    : Integer;
    A,B        : Integer;
    V1         : Integer;
    Found      : Boolean;
    GO         : TGroupObject;
    ML         : TMeshLibraryObjectReference;
    MO         : TMeshObject;
    D32        : Data32;
    D33        : Data33;
    D15        : Data15;

    MaxPt      : T3DPoint;
    MinPt      : T3DPoint;
    Ray        : T3DPoint;
    Normal     : T3DPoint;
    Center     : T3DPoint;
    PCenter    : T3DPoint;
    ToCenter   : T3DPoint;
    Vertex     : T3DPoint;
    RSize      : T3DPoint;
    Intersect  : T3DPoint;
    NRegions   : TStringList;
    Light      : TLightObject;
    P,P1       : TPolygon;
    Dot        : Single;
    Intensity  : Single;
    Reg        : TRegion;
    List       : TStringList;
//    TexIndex   : Array Of Integer;
    Textures   : TStringList;
    St         : String;
    Special    : Integer;
    LightColor : TColor;
    CR,CG,CB   : Integer;
    TextureSorter : TQuickSorterProc;

    Procedure GetNearRegions(List: TStringList; Region: TRegion; LightLoc: T3DPoint; VertexDist: Single);
    Begin
      If (High(Region.Polygons) >= 0) And
         (Region.Sphere.Center.DistanceFrom(LightLoc) < VertexDist + 1.1 * Region.Sphere.Radius) Then List.AddObject('',Region);
      If Region.Left  <> Nil Then GetNearRegions(List,Region.Left,LightLoc,VertexDist);
      If Region.Right <> Nil Then GetNearRegions(List,Region.Right,LightLoc,VertexDist);
    End; // GetNearRegions
{
    Procedure QuickSortTextures(P,R: Integer);
    Var Q: Integer;

      Function Partition(P,R: Integer): Integer;
      Var
        I,J,L   : Integer;
        K       : Integer;
        Special : Integer;

        Function GetTexIndex(P: Integer): Integer;
        Var
          Poly : TPolygon;
          St   : String;

        Begin
          Poly := TPolygon(MO.Polygons.Objects[TexIndex[P]]);
          St   := Poly.Texture;
          If Poly.TextureState = tsTransparent Then St := TransparentTexture
          Else
          Begin
            Special := 0;
            If Poly.TextureState = tsSemiTransparent Then Special := Special Or spSemiTransparent;
            If Poly.Masked                           Then Special := Special Or spMasked;
            If Special <> 0 Then St := St + Chr(SpecialTexture + Special);
          End;
          Result := Textures.IndexOf(St);
        End; // GetTexIndex

      Begin
        K := GetTexIndex(P);
        I := P - 1;
        J := R + 1;
        While True Do
        Begin
          Repeat
            Dec(J);
          Until GetTexIndex(J) <= K;
          Repeat
            Inc(I);
          Until GetTexIndex(I) >= K;
          If I < J Then
          Begin
            L           := TexIndex[I];
            TexIndex[I] := TexIndex[J];
            TexIndex[J] := L;
          End
          Else
          Begin
            Partition := J;
            Exit;
          End;
        End; // While
      End; // Partition

    Begin
      If P < R Then
      Begin
        Q := Partition(P,R);
        QuickSortTextures(P,Q);
        QuickSortTextures(Q + 1,R);
      End;
    End; // QuickSortTextures
}
    Function CompareTextures(Index0,Index1: Integer): Integer;

      Function GetTexIndex(Index: Integer): Integer;
      Var
        Poly    : TPolygon;
        St      : String;
        Special : Integer;

      Begin
        Poly   := TPolygon(SortMesh.Polygons.Objects[TexIndex[Index]]);
        St     := Poly.Texture;
        If Poly.TextureState = tsTransparent Then St := TransparentTexture
        Else
        Begin
          Special := 0;
          If Poly.TextureState = tsSemiTransparent Then Special := Special Or spSemiTransparent; //St := St + #241;
          If Poly.Masked Then Special := Special Or spMasked;
          If Special <> 0 Then St := St + Chr(SpecialTexture + Special);
        End;
        Result := SortTexList.IndexOf(St);
      End; // GetTexIndex

    Begin
      Result := GetTexIndex(Index0) - GetTexIndex(Index1);
    End; // CompareTextures

    Procedure ExchangeTextures(Index0,Index1: Integer);
    Var I: Integer;
    Begin
      I                := TexIndex[Index0];
      TexIndex[Index0] := TexIndex[Index1];
      TexIndex[Index1] := I;
    End; // ExchangeTextures

  Begin
    If (ZO Is TMeshLibraryObjectReference) And
       Not (TMeshLibraryObjectReference(ZO).InsertMesh Or (TMeshLibraryObjectReference(ZO).SQLRef <> srNone)) Then
    Begin
      ML := TMeshLibraryObjectReference(ZO.MakeCopy);
      ML.ChangeToAbsolute(ML.GetParent);
      ML.SetParent(Nil);

      I    := MeshLibrary.IndexOfObject(ML.Group);
      List := TStringList.Create;
      ML.AddToPolygonList(List,False,True,True,True);
      MO   := TMeshObject(List.Objects[0]);
      List.Free;
      MO.ConvertToTriangles;
{
      If Meshes[I] = Nil Then
      Begin
        List := TStringList.Create;
        ML.AddToPolygonList(List,False,True);
        Meshes[I] := TMeshObject(List.Objects[0]);
        List.Free;
        Meshes[I].ConvertToTriangles;
      End;
      MO := Meshes[I];
}
      D32                    := Data32.Create(W,'ENT' + IntToStr(EncodedCount) + '_DMT');
      D32.Data1              := 1;
      D32.Count              := 3 * MO.Polygons.Count;
      D32.Data2              := 1;
      D32.Data3              := 200;
      D32.Data4              := 0;
      SetLength(D32.Data5,D32.Count);
      For I := 0 To High(D32.Data5) Do D32.Data5[I].Color := $D9000000;

      D33                    := Data33.Create(W);
      D33.Flags              := 0;
      D33.Fragment           := FragmentReference.Create;
      D33.Fragment.Fragment  := D32;

      D15                    := Data15.Create(W);
      D15.Fragment           := FragmentReference.Create;
      D15.Fragment.Fragment  := Nil;
      D15.Fragment.Name      := '(' + UpperCase(ML.Group.GetName) + '_ACTORDEF)';
      D15.Flags              := $32E;
      D15.Fragment1          := FragmentReference.Create;
      D15.Fragment1.Fragment := Nil;
      D15.Fragment1.Position := 0;
      D15.X                  := ML.Loc.X;
      D15.Y                  := ML.Loc.Y;
      D15.Z                  := ML.Loc.Z;
      D15.RotateZ            := ML.Rotate.ZAngle * (512 / 360);
      D15.RotateY            := ML.Rotate.YAngle * (512 / 360);
      D15.RotateX            := ML.Rotate.XAngle * (512 / 360);
      D15.Params1            := 0;
      D15.ScaleY             := ML.Size.Y;
      D15.ScaleX             := ML.Size.X;
      D15.Fragment2          := FragmentReference.Create;
      D15.Fragment2.Fragment := D33;
      D15.Params2            := 0;

      // Now handle lighting

      MinPt     := T3DPoint.Create;
      MaxPt     := T3DPoint.Create;
      Ray       := T3DPoint.Create;
      Normal    := T3DPoint.Create;
      Center    := T3DPoint.Create;
//      PCenter   := T3DPoint.Create;
      ToCenter  := T3DPoint.Create;
      Vertex    := T3DPoint.Create;
      RSize     := T3DPoint.Create;
//      Intersect := T3DPoint.Create;
      NRegions  := TStringList.Create;

      MO.GetBounds(MinPt,MaxPt);
      RSize.Copy(MaxPt);
      RSize.Subtract(MinPt);
      Center.Copy(MinPt);
      Center.Add(MaxPt);
      Center.Divide(2);
//      Center.Add(ML.Loc);

      // First determine all of the textures used in the mesh

      Textures := TStringList.Create;
      For I := 0 To MO.Polygons.Count - 1 Do
      Begin
        P       := TPolygon(MO.Polygons.Objects[I]);
        St      := UpperCase(P.Texture);
        Special := 0;
        If P.TextureState = tsTransparent Then St := TransparentTexture
        Else
        Begin
          If P.TextureState = tsSemiTransparent Then Special := Special Or spSemiTransparent;
          If P.Masked                           Then Special := Special Or spMasked;
          If Special <> 0 Then St := St + Chr(SpecialTexture + Special);
        End;
        If Textures.IndexOf(St) < 0 Then Textures.Add(St);
      End; // For I

      SetLength(TexIndex,MO.Polygons.Count);
      For I := 0 To High(TexIndex) Do TexIndex[I] := I;

      SortMesh    := MO;
      SortTexList := Textures;

      TextureSorter          := TQuickSorterProc.Create;
      TextureSorter.Compare  := @CompareTextures;
      TextureSorter.Exchange := @ExchangeTextures;
      TextureSorter.Sort(0,MO.Polygons.Count - 1);
      TextureSorter.Free;

//      QuickSortTextures(0,MO.Polygons.Count - 1);

      MO.CalcNormals;

      MO.ShadePolygonsByLights(LightTreeHash,Nil,Center,RSize.GetLength);

      For I := 0 To MO.Polygons.Count - 1 Do
      Begin
        P := TPolygon(MO.Polygons.Objects[TexIndex[I]]);
        If P.HasColor Then
        Begin
          For J := 0 To High(P.Vertices) Do
          Begin
            // Form a vertex index to make life easier

            V1 := I * 3 + J;

            // EQ uses BGRA while we use RGBA, so load the color components individually

            D32.Data5[V1].R := TRGBA(P.Colors[J]).R;
            D32.Data5[V1].G := TRGBA(P.Colors[J]).G;
            D32.Data5[V1].B := TRGBA(P.Colors[J]).B;
          End; // For J
        End;
      End; // For I

//      Intersect.Free;
      NRegions.Free;
      RSize.Free;
      Vertex.Free;
      ToCenter.Free;
//      PCenter.Free;
      Center.Free;
      Normal.Free;
      Ray.Free;
      MaxPt.Free;
      MinPt.Free;

      SetLength(TexIndex,0);
      Textures.Free;

      AddFragment(W,D32);
      AddFragment(W,D33);
      AddFragment(W,D15);

      Inc(EncodedCount);
      ML.Free;
      MO.Free;
    End
    Else If ZO Is TGroupObject Then
    Begin
      GO := TGroupObject(ZO);
      For I := 0 To GO.Objects.Count - 1 Do EncodeObject(TZoneObject(GO.Objects.Objects[I]));
    End;
  End; // EncodeObject

Begin
  EncodedCount := 0;

  W := TEQWldData.Create;
  W.nFragment := 1;
  SetLength(W.Fragments,1);
  W.Fragments[0] := Nil;

//  SetLength(Meshes,MeshLibrary.Count);
//  For I := 0 To High(Meshes) Do Meshes[I] := Nil;

  LightTreeHash := TLightTreeHash.Create(Zone,Tree);
  frmStatus.Show;
  frmStatus.SetPosition(0);
  frmStatus.SetCaption('Encoding placeable mesh objects');
  For I := 0 To Zone.Count - 1 Do
  Begin
    frmStatus.SetPosition(I / Zone.Count);
    EncodeObject(TZoneObject(Zone.Objects[I]));
  End; // For I
  frmStatus.Hide;

//  For I := 0 To High(Meshes) Do If Meshes[I] <> Nil Then Meshes[I].Free;
//  SetLength(Meshes,0);
  LightTreeHash.Free;

  // Encode the .WLD file

  W.Encode(Stream);
  W.Free;
End; // TfrmMain.CreateObjectsWLD

Procedure TfrmMain.CreateEmptyObjectsWLD(Stream: TStream);
Var
  W   : TEQWldData;
//  D32 : Data32;
//  D33 : Data33;

Begin
  W := TEQWldData.Create;
  W.nFragment := 1;//3;
  SetLength(W.Fragments,1);//3);
  W.Fragments[0] := Nil;



{
  // The object list packet

  D32       := Data32.Create(W,'ENT0_DMT');
  D32.Data1 := 1;
  D32.Count := 0;    // No actual objects
  D32.Data2 := 1;
  D32.Data3 := 200;  // Maybe this is a float instead, like Data1B.Params3?
  D32.Data4 := 0;
  SetLength(D32.Data5,D32.Count);

  // Refers to the object list packet

  D33                   := Data33.Create(W);
  D33.Flags             := 0;
  D33.Fragment          := FragmentReference.Create;
  D33.Fragment.Fragment := D32;

  // Add the packets

  W.Fragments[1] := D32;
  W.Fragments[2] := D33;
}



  // Encode the .WLD file

  W.Encode(Stream);
  W.Free;
End; // TfrmMain.CreateEmptyObjectsWLD

Procedure TfrmMain.ExportToEQEmuMap(Stream: TStream);
Const
  MAP_VERSION = $01000000;
  NodeFinal   = 1;

Var
  Mesh         : TMeshObject;
  Header       : TEQEmuMapHeader;
  FaceList     : TList;
  MeshFaceList : TList;
  MinPt        : T3DPoint;
  MaxPt        : T3DPoint;
  Faces        : Packed Array Of TEQEmuMapFace;
  Nodes        : TList;
  I,J          : Integer;
  Polygon      : TPolygon;
  V            : T3DPoint;
  Plane        : TPlane;
  VV           : Array[0..2] Of Points3D.T3DPoint;

  Procedure AddNode(AMinPt,AMaxPt: T3DPoint; MeshFaceList: TList);
  Const MinSize = 64;
  Var
    MinPt : T3DPoint;
    MaxPt : T3DPoint;
    DX,DY : Single;
    MX,MY : Single;
    Node  : PEQEmuMapNode;
    I,J   : Integer;
    Lists : Array[0..3] Of TList;

  Begin
    // Get the overall size of the mesh

    DX := AMaxPt.X - AMinPt.X;
    DY := AMaxPt.Y - AMinPt.Y;
    If (DX > MinSize) Or (DY > MinSize) Then
    Begin
      MX := (AMinPt.X + AMaxPt.X) / 2;
      MY := (AMinPt.Y + AMaxPt.Y) / 2;
      If (DX > MinSize) And (DY > MinSize) Then
      Begin
        GetMem(Node,SizeOf(TEQEmuMapNode));
        Node.MinX         := AMinPt.X;
        Node.MinY         := AMinPt.Y;
        Node.MaxX         := AMaxPt.X;
        Node.MaxY         := AMaxPt.Y;
        Node.Flags        := 0;
        Nodes.Add(Node);

        Lists[0] := TList.Create;
        Lists[1] := TList.Create;
        Lists[2] := TList.Create;
        Lists[3] := TList.Create;
        MinPt    := T3DPoint.Create(AMinPt);
        MaxPt    := T3DPoint.Create(AMaxPt);
        MinPt.X  := MX;
        MinPt.Y  := MY;
        For I := 0 To MeshFaceList.Count - 1 Do
        Begin
          J := Integer(MeshFaceList.Items[I]);
          If ((Faces[J].V[0].X >= MinPt.X) And (Faces[J].V[0].Y >= MinPt.Y)) Or
             ((Faces[J].V[1].X >= MinPt.X) And (Faces[J].V[1].Y >= MinPt.Y)) Or
             ((Faces[J].V[2].X >= MinPt.X) And (Faces[J].V[2].Y >= MinPt.Y)) Then Lists[0].Add(Pointer(J));
          If ((Faces[J].V[0].X <  MinPt.X) And (Faces[J].V[0].Y >= MinPt.Y)) Or
             ((Faces[J].V[1].X <  MinPt.X) And (Faces[J].V[1].Y >= MinPt.Y)) Or
             ((Faces[J].V[2].X <  MinPt.X) And (Faces[J].V[2].Y >= MinPt.Y)) Then Lists[1].Add(Pointer(J));
          If ((Faces[J].V[0].X >= MinPt.X) And (Faces[J].V[0].Y <  MinPt.Y)) Or
             ((Faces[J].V[1].X >= MinPt.X) And (Faces[J].V[1].Y <  MinPt.Y)) Or
             ((Faces[J].V[2].X >= MinPt.X) And (Faces[J].V[2].Y <  MinPt.Y)) Then Lists[3].Add(Pointer(J));
          If ((Faces[J].V[0].X <  MinPt.X) And (Faces[J].V[0].Y <  MinPt.Y)) Or
             ((Faces[J].V[1].X <  MinPt.X) And (Faces[J].V[1].Y <  MinPt.Y)) Or
             ((Faces[J].V[2].X <  MinPt.X) And (Faces[J].V[2].Y <  MinPt.Y)) Then Lists[2].Add(Pointer(J));
        End; // For I
        Node.Union.Nodes[0] := Nodes.Count;
        AddNode(MinPt,MaxPt,Lists[0]);

        MinPt.Copy(AMinPt);
        MaxPt.Copy(AMaxPt);
        MaxPt.X := MX;
        MinPt.Y := MY;
        Node.Union.Nodes[1] := Nodes.Count;
        AddNode(MinPt,MaxPt,Lists[1]);

        MinPt.Copy(AMinPt);
        MaxPt.Copy(AMaxPt);
        MaxPt.X := MX;
        MaxPt.Y := MY;
        Node.Union.Nodes[2] := Nodes.Count;
        AddNode(MinPt,MaxPt,Lists[2]);

        MinPt.Copy(AMinPt);
        MaxPt.Copy(AMaxPt);
        MinPt.X := MX;
        MaxPt.Y := MY;
        Node.Union.Nodes[3] := Nodes.Count;
        AddNode(MinPt,MaxPt,Lists[3]);

        Lists[0].Free;
        Lists[1].Free;
        Lists[2].Free;
        Lists[3].Free;
        MinPt.Free;
        MaxPt.Free;
      End
      Else If DX > MinSize Then
      Begin
        GetMem(Node,SizeOf(TEQEmuMapNode));
        Node.MinX         := AMinPt.X;
        Node.MinY         := AMinPt.Y;
        Node.MaxX         := AMaxPt.X;
        Node.MaxY         := AMaxPt.Y;
        Node.Flags        := 0;
        Nodes.Add(Node);

        // Nodes 1 and 2 will be used (the left two)

        Lists[0] := TList.Create;
        Lists[1] := TList.Create;
        MinPt    := T3DPoint.Create(AMinPt);
        MaxPt    := T3DPoint.Create(AMaxPt);
        MinPt.X  := (MinPt.X + MaxPt.X) / 2;
        For I := 0 To MeshFaceList.Count - 1 Do
        Begin
          J := Integer(MeshFaceList.Items[I]);
          If (Faces[J].V[0].X >= MinPt.X) Or (Faces[J].V[1].X >= MinPt.X) Or (Faces[J].V[2].X >= MinPt.X) Then Lists[0].Add(Pointer(J));
          If (Faces[J].V[0].X <  MinPt.X) Or (Faces[J].V[1].X <  MinPt.X) Or (Faces[J].V[2].X <  MinPt.X) Then Lists[1].Add(Pointer(J));
        End; // For I
        Node.Union.Nodes[0] := Nodes.Count;
        AddNode(MinPt,MaxPt,Lists[0]);

        MinPt.Copy(AMinPt);
        MaxPt.X := MX;
        Node.Union.Nodes[1] := Nodes.Count;
        AddNode(MinPt,MaxPt,Lists[1]);

        Node.Union.Nodes[2] := 0;
        Node.Union.Nodes[3] := 0;
        Lists[0].Free;
        Lists[1].Free;
        MinPt.Free;
        MaxPt.Free;
      End
      Else
      Begin
        GetMem(Node,SizeOf(TEQEmuMapNode));
        Node.MinX         := AMinPt.X;
        Node.MinY         := AMinPt.Y;
        Node.MaxX         := AMaxPt.X;
        Node.MaxY         := AMaxPt.Y;
        Node.Flags        := 0;
        Nodes.Add(Node);

        // Nodes 1 and 4 will be used (the top two)

        Lists[0] := TList.Create;
        Lists[3] := TList.Create;
        MinPt    := T3DPoint.Create(AMinPt);
        MaxPt    := T3DPoint.Create(AMaxPt);
        MinPt.Y  := (MinPt.Y + MaxPt.Y) / 2;
        For I := 0 To MeshFaceList.Count - 1 Do
        Begin
          J := Integer(MeshFaceList.Items[I]);
          If (Faces[J].V[0].Y >= MinPt.Y) Or (Faces[J].V[1].Y >= MinPt.Y) Or (Faces[J].V[2].Y >= MinPt.Y) Then Lists[0].Add(Pointer(J));
          If (Faces[J].V[0].Y <  MinPt.Y) Or (Faces[J].V[1].Y <  MinPt.Y) Or (Faces[J].V[2].Y <  MinPt.Y) Then Lists[3].Add(Pointer(J));
        End; // For I
        Node.Union.Nodes[0] := Nodes.Count;
        AddNode(MinPt,MaxPt,Lists[0]);

        MinPt.Copy(AMinPt);
        MaxPt.Y := MY;
        Node.Union.Nodes[3] := Nodes.Count;
        AddNode(MinPt,MaxPt,Lists[3]);

        Node.Union.Nodes[1] := 0;
        Node.Union.Nodes[2] := 0;
        Lists[0].Free;
        Lists[3].Free;
        MinPt.Free;
        MaxPt.Free;
      End;
    End
    Else
    Begin
      GetMem(Node,SizeOf(TEQEmuMapNode));
      Node.MinX         := AMinPt.X;
      Node.MinY         := AMinPt.Y;
      Node.MaxX         := AMaxPt.X;
      Node.MaxY         := AMaxPt.Y;
      Node.Flags        := NodeFinal;
      Node.Union.Count  := Mesh.Polygons.Count;
      Node.Union.Offset := FaceList.Count;
      For I := 0 To MeshFaceList.Count - 1 Do FaceList.Add(MeshFaceList.Items[I]);
      Nodes.Add(Node);
    End;
  End; // AddNode

Begin
  // Make a mesh of EVERYTHING in the zone

  Mesh  := Zone.BuildPolygonList(False,False,False,False);
  MinPt := T3DPoint.Create;
  MaxPt := T3DPoint.Create;
  Mesh.GetBounds(MinPt,MaxPt);

  // Create an empty face list.  It will contain lists of faces, grouped by node.

  FaceList := TList.Create;

  // Create a master array of faces

  SetLength(Faces,Mesh.Polygons.Count);
  Plane := TPlane.Create;
  VV[0] := Points3D.T3DPoint.Create;
  VV[1] := Points3D.T3DPoint.Create;
  VV[2] := Points3D.T3DPoint.Create;
  For I := 0 To Mesh.Polygons.Count - 1 Do
  Begin
    Polygon := TPolygon(Mesh.Polygons.Objects[I]);
    For J := 0 To 2 Do
    Begin
      V := T3DPoint(Mesh.Vertices.Objects[Polygon.Vertices[J Mod (High(Polygon.Vertices) + 1)]]);
      Faces[I].V[J].X := V.X;
      Faces[I].V[J].Y := V.Y;
      Faces[I].V[J].Z := V.Z;
      VV[J].Copy(V.X,V.Y,V.Z);
    End; // For J
    Plane.Setup(VV[0],VV[1],VV[2]);
    Faces[I].NX := Plane.Normal.X;
    Faces[I].NY := Plane.Normal.Y;
    Faces[I].NZ := Plane.Normal.Z;
    Faces[I].ND := Plane.Distance;
  End; // For I

  // Create the nodes

  Nodes := TList.Create;
  MeshFaceList := TList.Create;
  For I := 0 To Mesh.Polygons.Count - 1 Do MeshFaceList.Add(Pointer(I));
  AddNode(MinPt,MaxPt,MeshFaceList);

  // Set up the header

  Header.Version        := MAP_VERSION;
  Header.Face_Count     := Mesh.Polygons.Count;
  Header.Node_Count     := Nodes.Count;
  Header.FaceList_Count := FaceList.Count;

  // Write the file

  Stream.WriteBuffer(Header,SizeOf(Header));
  If Mesh.Polygons.Count > 0 Then Stream.WriteBuffer(Faces[0],Mesh.Polygons.Count * SizeOf(TEQEmuMapFace));
  For I := 0 To Nodes.Count - 1 Do Stream.WriteBuffer(Nodes.Items[I]^,SizeOf(TEQEmuMapNode));
  For I := 0 To FaceList.Count - 1 Do
  Begin
    J := Integer(FaceList.Items[I]);
    Stream.WriteBuffer(J,SizeOf(Integer));
  End; // For I

  // Cleanup

  Mesh.Free;
  MinPt.Free;
  MaxPt.Free;
  FaceList.Free;
  MeshFaceList.Free;
  For I := 0 To Nodes.Count - 1 Do FreeMem(Nodes.Items[I]);
  Nodes.Free;
  Plane.Free;
  VV[0].Free;
  VV[1].Free;
  VV[2].Free;
End; // TfrmMain.ExportToEQEmuMap

Procedure TfrmMain.ExportToDoorsSQL(FileName: String);
Var
  F  : System.Text;
  I  : Integer;
  ID : Integer;
  St : String;

  Procedure ExportDoorRefObject(ZO: TZoneObject);
  Var
    I      : Integer;
    ML     : TMeshLibraryObjectReference;
    GO     : TGroupObject;
    Fields : String;
    Values : String;
    St     : String;
    Tokens : TTokenArray;

  Begin
    If (ZO Is TMeshLibraryObjectReference) And (TMeshLibraryObjectReference(ZO).SQLRef = srDoors) Then
    Begin
      ML := TMeshLibraryObjectReference(ZO.MakeCopy);
      ML.ChangeToAbsolute(ML.GetParent);
      ML.SetParent(Nil);
      St := Zone.ShortName;
      If St = '' Then St := LowerCase(ExtractFileNameNoExt(FileName));
      Fields := 'doorid, zone, name, pos_y, pos_x, pos_z, heading, opentype, dest_zone';
      Values := Format('%d, "%s", "%s", %8.3f, %8.3f, %8.3f, %8.3f, %d, "%s"',
                       [ID, St, UpperCase(ML.Group.GetName), ML.Loc.X, ML.Loc.Y, ML.Loc.Z, ML.Rotate.ZAngle * (512 / 360), 0, 'NONE']);
      For I := 0 To ML.SQLParmCount - 1 Do
      Begin
        St := ML.SQLParm[I];
        GetTokens(',',St,Tokens);
        If (High(Tokens) = 2) And (LowerCase(Tokens[0]) = 'doors') And (Tokens[1] <> '') Then
        Begin
          Fields := Fields + ', ' + Tokens[1];
          Values := Values + ', "' + Tokens[2] + '"';
        End;
        SetLength(Tokens,0);
      End; // For I
      WriteLn(F,'INSERT INTO doors (' + Fields + ') VALUES (' + Values + ');');
      Inc(ID);
      ML.Free;
    End
    Else If ZO Is TGroupObject Then
    Begin
      GO := TGroupObject(ZO);
      For I := 0 To GO.Objects.Count - 1 Do ExportDoorRefObject(TZoneObject(GO.Objects.Objects[I]));
    End;
  End; // ExportDoorRefObject

Begin
  AssignFile(F,FileName);
  ReWrite(F);

  St := Zone.ShortName;
  If St = '' Then St := LowerCase(ExtractFileNameNoExt(FileName));

  frmStatus.Show;
  frmStatus.SetPosition(0);
  frmStatus.SetCaption('Exporting door table SQL');

  WriteLn(F,'DELETE FROM doors WHERE zone = "' + St + '";');
  ID := 1; // The first ID should be 1 (doors with ID 0 don't display)
  For I := 0 To Zone.Count - 1 Do
  Begin
    frmStatus.SetPosition(I / Zone.Count);
    ExportDoorRefObject(TZoneObject(Zone.Objects[I]));
  End; // For I

  frmStatus.Hide;

  CloseFile(F);
End; // TfrmMain.ExportToDoorsSQL

Procedure TfrmMain.ExportToObjectsSQL(FileName: String);
Var
  F  : System.Text;
  I  : Integer;
  ID : Integer;
  St : String;

  Procedure ExportObjectRefObject(ZO: TZoneObject);
  Var
    I      : Integer;
    ML     : TMeshLibraryObjectReference;
    CL     : TCreatureLibraryObjectReference;
    GO     : TGroupObject;
    Fields : String;
    Values : String;
    St     : String;
    Tokens : TTokenArray;

  Begin
    If (ZO Is TMeshLibraryObjectReference) And (TMeshLibraryObjectReference(ZO).SQLRef = srObjects) Then
    Begin
      ML := TMeshLibraryObjectReference(ZO.MakeCopy);
      ML.ChangeToAbsolute(ML.GetParent);
      ML.SetParent(Nil);
      St := Zone.ShortName;
      If St = '' Then St := LowerCase(ExtractFileNameNoExt(FileName));
      Fields := 'zoneid, objectname, xpos, ypos, zpos, heading';
      Values := Format('%d, "%s", %8.3f, %8.3f, %8.3f, %8.3f',
                       [9999, UpperCase(ML.Group.GetName), ML.Loc.X, ML.Loc.Y, ML.Loc.Z, ML.Rotate.ZAngle * (256 / 360)]);
      For I := 0 To ML.SQLParmCount - 1 Do
      Begin
        St := ML.SQLParm[I];
        GetTokens(',',St,Tokens);
        If (High(Tokens) = 2) And (LowerCase(Tokens[0]) = 'object') And (Tokens[1] <> '') Then
        Begin
          Fields := Fields + ', ' + Tokens[1];
          Values := Values + ', "' + Tokens[2] + '"';
        End;
        SetLength(Tokens,0);
      End; // For I
      WriteLn(F,'INSERT INTO object (' + Fields + ') VALUES (' + Values + ');');
      Inc(ID);
      ML.Free;
    End
    Else If ZO Is TCreatureLibraryObjectReference Then
    Begin
      CL := TCreatureLibraryObjectReference(ZO.MakeCopy);
      I  := CreatureLibrary.IndexOfObject(CL.An8File);
      If I >= 0 Then
      Begin
        CL.ChangeToAbsolute(CL.GetParent);
        CL.SetParent(Nil);
        St := Zone.ShortName;
        If St = '' Then St := LowerCase(ExtractFileNameNoExt(FileName));
        Fields := 'zoneid, objectname, xpos, ypos, zpos, heading';
        Values := Format('%d, "%s", %8.3f, %8.3f, %8.3f, %8.3f',
                         [9999, UpperCase(CreatureLibrary.Strings[I]), CL.Loc.X, CL.Loc.Y, CL.Loc.Z, CL.Rotate.ZAngle * (256 / 360)]);
        WriteLn(F,'INSERT INTO object (' + Fields + ') VALUES (' + Values + ');');
        Inc(ID);
      End;
      CL.Free;
    End
    Else If ZO Is TGroupObject Then
    Begin
      GO := TGroupObject(ZO);
      For I := 0 To GO.Objects.Count - 1 Do ExportObjectRefObject(TZoneObject(GO.Objects.Objects[I]));
    End;
  End; // ExportObjectRefObject

Begin
  AssignFile(F,FileName);
  ReWrite(F);

  St := Zone.ShortName;
  If St = '' Then St := LowerCase(ExtractFileNameNoExt(FileName));

  frmStatus.Show;
  frmStatus.SetPosition(0);
  frmStatus.SetCaption('Exporting object table SQL');

  // Get rid of objects in this zone.  This is tricky since the object table uses zone ID numbers.  We have to use an
  // inner join with the zone table to figure out what to delete.

  WriteLn(F,'DELETE object FROM object INNER JOIN zone WHERE object.zoneid = zone.zoneidnumber AND zone.short_name = "' + St + '";');
  ID := 1; // The first ID should be 1 (objects with ID 0 don't display)

  // Export the object table references.  We don't yet know the proper zone ID, so they will be temporarily exported as 9999.

  For I := 0 To Zone.Count - 1 Do
  Begin
    frmStatus.SetPosition(I / Zone.Count);
    ExportObjectRefObject(TZoneObject(Zone.Objects[I]));
  End; // For I

  // Change the 9999 zone ID's to the correct zone ID with a join

  WriteLn(F,'UPDATE object JOIN zone SET object.zoneid = zone.zoneidnumber WHERE object.zoneid = 9999 AND zone.short_name = "' + St + '";');

  frmStatus.Hide;

  CloseFile(F);
End; // TfrmMain.ExportToObjectsSQL

Procedure TfrmMain.ExportToXWA(Stream: TStream; ZoneName: String);
Var
  FileList    : TStringList;
  I,J,K       : Integer;
//  Tokens      : TTokenArray;
  FileOnly    : String;
  St,St1      : String;
  TextureInfo : TTextureInfo;
//  Textures    : String;
//  Opacities   : String;
//  Parameters  : String;
  S3DFile     : TS3DFile;
  MemStream   : TMemoryStream;
  FileStream  : TFileStream;
  ZoneMesh    : TMeshObject;
  List        : TStringList;
  CharTexList : TStringList;
  B           : Boolean;

Begin
  B := tmrTimeOfDay.Enabled;
  tmrTimeOfDay.Enabled := False;

  // Create the S3D archive

  S3DFile := TS3DFile.Create;

  // Load the model with everything to be exported so we can get all of the textures

  frmStatus.Show;
  frmStatus.SetPosition(0);
  frmStatus.SetCaption('Creating polygon mesh');
  ZoneMesh := Zone.BuildPolygonList(False,False,False,False);
  Zone.AddWaterToMesh(ZoneMesh);
  List := TStringList.Create;
  List.AddObject('',ZoneMesh);
  ReloadModel(List,True);
  List.Free;

  // Find out all the actual texture filenames, adding them to the archive as we go

  FileList := TStringList.Create;
  FileList.Sorted := True;
  For I := 0 To MasterTexList.Count - 1 Do
  Begin
    // We have to copy the string because GetTokens destroys it

    St := MasterTexList.Strings[I];
    If (St <> TransparentTexture) Then
    Begin
      TextureInfo := TTextureInfo.Create(St);
//      BreakupTextureString(St,Textures,Opacities,Parameters);
//      GetTokens(';',Textures,Tokens);
      For J := 0 To TextureInfo.TextureMaps.Count - 1 Do// High(Tokens) Do
      Begin
        // Save the texture reference, including texture folder

        St := TextureInfo.TextureMaps.Strings[J];//Tokens[J];

        // Strip off the texture set reference (if you have duplicate names in different
        // folders, it will choose only one of them--we have to do this anyway or the
        // client probably won't like it)

        FileOnly := TextureInfo.TextureMaps.Strings[J];//Tokens[J];
        K := LastDelimiter('/',FileOnly);
        If K = 0 Then K := LastDelimiter('\',FileOnly);
        If K > 0 Then FileOnly := Copy(FileOnly,K + 1,Length(FileOnly));

        // Convert to lowercase and add it to the list (and to the archive) if it
        // isn't already there

        FileOnly := LowerCase(FileOnly + '.BMP');
        If FileList.IndexOf(FileOnly) < 0 Then
        Begin
          FileList.Add(FileOnly);
          St  := LowerCase(ExtractFilePath(Application.ExeName) + 'library\textures\' + St + '.bmp');
          If Not FileExists(St) Then St := Copy(St,1,Length(St) - Length(ExtractFileExt(St))) + '.jpg';
          If Not FileExists(St) Then St := Copy(St,1,Length(St) - Length(ExtractFileExt(St))) + '.tga';
          If FileExists(St) Then
          Begin
            ConvertBMPToDDS(St);
            FileStream := TFileStream.Create(ExtractFilePath(Application.ExeName) + TempDDSFileName,fmOpenRead);
            If FileStream.Size > 0 Then
            Begin
              S3DFile.AddFile(FileOnly);
              K := S3DFile.FileCount - 1;
              S3DFile.FileSize[K] := FileStream.Size;
              FileStream.ReadBuffer(S3DFile.DataPointer[K]^,FileStream.Size);
            End;
            FileStream.Free;
          End;
        End;
      End; // For J
      TextureInfo.Free;
    End;
  End; // For I

//  SetLength(Tokens,0);

  // Generate our <zone>.xwf file and add it to the archive

  MemStream := TMemoryStream.Create;

  List := TStringList.Create;
  CharTexList := TStringList.Create;
  CharTexList.Sorted := True;
  For I := 0 To glView.Scene3D.Scene.Textures.Count - 1 Do
   List.AddObject(LowerCase(glView.Scene3D.Scene.Textures.Strings[I]),glView.Scene3D.Scene.Textures.Objects[I]);

  ReloadZone; // Have to refresh what's loaded in OpenGL because we loaded different stuff
  frmStatus.Show;

  Zone.ExportToXWFFile(MemStream,List,CharTexList,frmXWFExportOptions.rbOctree.Checked,ProgramSettings.XWFCreatureSize);
  List.Free;
  S3DFile.AddFile(LowerCase(ZoneName + '.xwf'));
  I := S3DFile.FileCount - 1;
  S3DFile.FileSize[I] := MemStream.Size;
  Move(MemStream.Memory^,S3DFile.DataPointer[I]^,MemStream.Size);
  MemStream.Free;

  // Export the textures that go with any exported character models

  For I := 0 To CharTexList.Count - 1 Do
  Begin
    St := CharTexList.Strings[I];
    If St <> '' Then
    Begin
      St1 := LowerCase(ExtractFilePath(Application.ExeName) + 'library\creatures\' + St);
      If FileExists(St1) Then
      Begin
        ConvertBMPToDDS(St1);
        FileStream := TFileStream.Create(ExtractFilePath(Application.ExeName) + TempDDSFileName,fmOpenRead);
        If FileStream.Size > 0 Then
        Begin
          S3DFile.AddFile(St);
          K := S3DFile.FileCount - 1;
          S3DFile.FileSize[K] := FileStream.Size;
          FileStream.ReadBuffer(S3DFile.DataPointer[K]^,FileStream.Size);
        End;
        FileStream.Free;
      End;
    End;
  End; // For I

  // Write everything to the stream (the S3D object will automatically sort the files
  // in alphabetical order for us)

  frmStatus.Show;
  frmStatus.SetPosition(0);
  frmStatus.SetCaption('Compressing data and writing .XWA archive');

  S3DFile.SaveToStream(Stream);

  frmStatus.SetCaption('');
  frmStatus.Close;

  // Cleanup

  ZoneMesh.Free;
  S3DFile.Free;
  FileList.Free;
  CharTexList.Free;
  tmrTimeOfDay.Enabled := B;
End; // TfrmMain.ExportToXWA

Procedure TfrmMain.ExportToS3D(Stream: TStream; ZoneName: String);
Var
  FileList    : TStringList;
  I,J,K       : Integer;
//  Tokens      : TTokenArray;
  FileOnly    : String;
  St,St1      : String;
  TextureInfo : TTextureInfo;
//  Textures    : String;
//  Opacities   : String;
//  Parameters  : String;
  S3DFile     : TS3DFile;
  MemStream   : TMemoryStream;
  FileStream  : TFileStream;
  ZoneMesh    : TMeshObject;
  List        : TStringList;
  B           : Boolean;

Begin
  B := tmrTimeOfDay.Enabled;
  tmrTimeOfDay.Enabled := False;

  // Create the S3D archive

  S3DFile := TS3DFile.Create;

  // Find out all the actual texture filenames, adding them to the archive as we go

  FileList := TStringList.Create;
  FileList.Sorted := True;
  For I := 0 To MasterTexList.Count - 1 Do
  Begin
    // We have to copy the string because GetTokens destroys it

    St := MasterTexList.Strings[I];
    If (St <> TransparentTexture) Then
    Begin
      TextureInfo := TTextureInfo.Create(St);
//      BreakupTextureString(St,Textures,Opacities,Parameters);
//      GetTokens(';',Textures,Tokens);
      For J := 0 To TextureInfo.TextureMaps.Count - 1 Do//High(Tokens) Do
      Begin
        // Save the texture reference, including texture folder

        St := TextureInfo.TextureMaps.Strings[J];//Tokens[J];

        // Strip off the texture set reference (if you have duplicate names in different
        // folders, it will choose only one of them--we have to do this anyway or the
        // client probably won't like it)

        FileOnly := TextureInfo.TextureMaps.Strings[J];//Tokens[J];
        K := LastDelimiter('/',FileOnly);
        If K = 0 Then K := LastDelimiter('\',FileOnly);
        If K > 0 Then FileOnly := Copy(FileOnly,K + 1,Length(FileOnly));

        // Convert to lowercase and add it to the list (and to the archive) if it
        // isn't already there

        FileOnly := LowerCase(FileOnly + '.BMP');
        If FileList.IndexOf(FileOnly) < 0 Then
        Begin
          FileList.Add(FileOnly);
          St  := LowerCase(ExtractFilePath(Application.ExeName) + 'library\textures\' + St + '.bmp');
          If Not FileExists(St) Then St := Copy(St,1,Length(St) - Length(ExtractFileExt(St))) + '.jpg';
          If Not FileExists(St) Then St := Copy(St,1,Length(St) - Length(ExtractFileExt(St))) + '.tga';
          If FileExists(St) Then
          Begin

            ConvertBMPToDDS(St);
            FileStream := TFileStream.Create(ExtractFilePath(Application.ExeName) + TempDDSFileName,fmOpenRead);
            If FileStream.Size > 0 Then
            Begin
              S3DFile.AddFile(FileOnly);
              K := S3DFile.FileCount - 1;
              S3DFile.FileSize[K] := FileStream.Size;
              FileStream.ReadBuffer(S3DFile.DataPointer[K]^,FileStream.Size);
            End;
            FileStream.Free;


{
            BMP := TBitmap.Create;
            BMP.LoadFromFile(St);
            DDSStream := ConvertBMPToDDS(BMP);
            S3DFile.AddFile(Tokens[J]);
            K := S3DFile.FileCount - 1;
            S3DFile.FileSize[K] := DDSStream.Size;
            DDSStream.ReadBuffer(S3DFile.DataPointer[K]^,DDSStream.Size);
            DDSStream.Free;
            BMP.Free;
}
{
            FileStream := TFileStream.Create(St,fmOpenRead);
            S3DFile.AddFile(Tokens[J]);
            K := S3DFile.FileCount - 1;
            S3DFile.FileSize[K] := FileStream.Size;
            FileStream.ReadBuffer(S3DFile.DataPointer[K]^,FileStream.Size);
            FileStream.Free;
}
          End;
        End;
      End; // For J
    End;
  End; // For I

//  SetLength(Tokens,0);

  frmStatus.Show;
  frmStatus.SetPosition(0);
  frmStatus.SetCaption('Creating polygon mesh');
  ZoneMesh := Zone.BuildPolygonList(True,False,True,False);
  List := TStringList.Create;
  List.AddObject('',ZoneMesh);
  ReloadModel(List,True);
  List.Free;

  // Generate our lights.wld file and add it to the archive

  MemStream := TMemoryStream.Create;
  CreateLightsWLD(MemStream);
  S3DFile.AddFile('lights.wld');
  I                   := S3DFile.FileCount - 1;
  S3DFile.FileSize[I] := MemStream.Size;
  Move(MemStream.Memory^,S3DFile.DataPointer[I]^,MemStream.Size);
  MemStream.Free;

  // Generate our objects.wld file and add it to the archive

  MemStream := TMemoryStream.Create;
  CreateObjectsWLD(MemStream,ZoneMesh);
  S3DFile.AddFile('objects.wld');
  I                   := S3DFile.FileCount - 1;
  S3DFile.FileSize[I] := MemStream.Size;
  Move(MemStream.Memory^,S3DFile.DataPointer[I]^,MemStream.Size);
  MemStream.Free;

  // Generate our <zone>.wld file and add it to the archive

  MemStream := TMemoryStream.Create;
  ExportToWLD(MemStream,LowerCase(ZoneName + '.wld'),ZoneMesh);
  S3DFile.AddFile(LowerCase(ZoneName + '.wld'));
  I := S3DFile.FileCount - 1;
  S3DFile.FileSize[I] := MemStream.Size;
  Move(MemStream.Memory^,S3DFile.DataPointer[I]^,MemStream.Size);
  MemStream.Free;

  // Write everything to the stream (the S3D object will automatically sort the files
  // in alphabetical order for us)

  frmStatus.Show;
  frmStatus.SetPosition(0);
  frmStatus.SetCaption('Compressing data and writing .S3D archive');

  S3DFile.SaveToStream(Stream);

  frmStatus.SetCaption('');
  frmStatus.Close;

  // Cleanup

  ZoneMesh.Free;
  S3DFile.Free;
  FileList.Free;
  tmrTimeOfDay.Enabled := B;
End; // TfrmMain.ExportToS3D

Procedure TfrmMain.ExportToChrS3D(Stream: TStream; WLDName: String);
Var
  I,J        : Integer;
  S3DFile    : TS3DFile;
  MemStream  : TMemoryStream;
  FileStream : TFileStream;
  List       : TStringList;
  St         : String;

Begin
  // Create the S3D archive

  S3DFile := TS3DFile.Create;

  // Generate our <zone>_chr.wld file and add it to the archive

  MemStream := TMemoryStream.Create;
  List := CreateChrWLD(MemStream);
//  CreateEmptyWLD(MemStream);
  S3DFile.AddFile(LowerCase(WLDName + '.wld'));
  I                   := S3DFile.FileCount - 1;
  S3DFile.FileSize[I] := MemStream.Size;
  Move(MemStream.Memory^,S3DFile.DataPointer[I]^,MemStream.Size);
  MemStream.Free;

  For I := 0 To List.Count - 1 Do
  Begin
    St := LowerCase(ExtractFilePath(Application.ExeName) + 'library\creatures\' + List.Strings[I]);
    If Not FileExists(St) Then St := Copy(St,1,Length(St) - Length(ExtractFileExt(St))) + '.JPG';
    If Not FileExists(St) Then St := Copy(St,1,Length(St) - Length(ExtractFileExt(St))) + '.TGA';
    If FileExists(St) Then
    Begin
      ConvertBMPToDDS(St);
      FileStream := TFileStream.Create(ExtractFilePath(Application.ExeName) + TempDDSFileName,fmOpenRead);
      If FileStream.Size > 0 Then
      Begin
        S3DFile.AddFile(LowerCase(List.Strings[I]));
        J := S3DFile.FileCount - 1;
        S3DFile.FileSize[J] := FileStream.Size;
        FileStream.ReadBuffer(S3DFile.DataPointer[J]^,FileStream.Size);
      End;
      FileStream.Free;
    End;
  End; // For I
  List.Free;

  // Write everything to the stream (the S3D object will automatically sort the files
  // in alphabetical order for us)

  frmStatus.Show;
  frmStatus.SetPosition(0);
  frmStatus.SetCaption('Compressing data and writing .S3D archive');

  S3DFile.SaveToStream(Stream);

  frmStatus.SetCaption('');
  frmStatus.Close;

  // Cleanup

  S3DFile.Free;
End; // TfrmMain.ExportToChrS3D

Procedure TfrmMain.ExportToObjS3D(Stream: TStream; WLDName: String; ZoneObjects: Boolean);
Var
  I,J        : Integer;
  S3DFile    : TS3DFile;
  MemStream  : TMemoryStream;
  FileStream : TFileStream;
  List       : TStringList;
  St         : String;

Begin
  // Create the S3D archive

  S3DFile := TS3DFile.Create;

  // Generate our <zone>_obj.wld file and add it to the archive

  MemStream := TMemoryStream.Create;
  List := CreateObjWLD(MemStream,ZoneObjects);
  S3DFile.AddFile(LowerCase(WLDName + '.wld'));
  I                   := S3DFile.FileCount - 1;
  S3DFile.FileSize[I] := MemStream.Size;
  Move(MemStream.Memory^,S3DFile.DataPointer[I]^,MemStream.Size);
  MemStream.Free;

  For I := 0 To List.Count - 1 Do
  Begin
    St := LowerCase(ExtractFilePath(Application.ExeName) + 'library\textures\' + List.Strings[I]);
    If Not FileExists(St) Then St := Copy(St,1,Length(St) - Length(ExtractFileExt(St))) + '.JPG';
    If Not FileExists(St) Then St := Copy(St,1,Length(St) - Length(ExtractFileExt(St))) + '.TGA';
    If FileExists(St) Then
    Begin
      ConvertBMPToDDS(St);
      FileStream := TFileStream.Create(ExtractFilePath(Application.ExeName) + TempDDSFileName,fmOpenRead);
      If FileStream.Size > 0 Then
      Begin
        S3DFile.AddFile(LowerCase(List.Strings[I]));
        J := S3DFile.FileCount - 1;
        S3DFile.FileSize[J] := FileStream.Size;
        FileStream.ReadBuffer(S3DFile.DataPointer[J]^,FileStream.Size);
      End;
      FileStream.Free;
    End;
  End; // For I
  List.Free;

  // Write everything to the stream (the S3D object will automatically sort the files
  // in alphabetical order for us)

  frmStatus.Show;
  frmStatus.SetPosition(0);
  frmStatus.SetCaption('Compressing data and writing .S3D archive');

  S3DFile.SaveToStream(Stream);

  frmStatus.SetCaption('');
  frmStatus.Close;

  // Cleanup

  S3DFile.Free;
End; // TfrmMain.ExportToObjS3D

Procedure TfrmMain.EnableGUI(B: Boolean);
Var I: Integer;
Begin
//  For I := 0 To MainMenu1.Items.Count - 1 Do MainMenu1.Items[I].Enabled := B;
  CoolBar1.Enabled                   := B;
//  ToolBar1.Enabled                   := B;
  ToolBar2.Enabled                   := B;
  tbPalette.Enabled                  := B;
  ActionToolBar1.Enabled             := B;
  tvMain.Enabled                     := B;
  ZPropList1.Enabled                 := B;
  pcRight.Enabled                    := B;
  tvMeshes.Enabled                   := B;
  pnlGroundEdit.Enabled              := B;
  pnlDisplayOptions.Enabled          := B;
//  glView.Enabled                     := B;
  edtRaiseLowerAmount.Enabled        := B;
  udRaiseLowerAmount.Enabled         := B;
  btnNegate.Enabled                  := B;
  cbTextureSet.Enabled               := B;
  cbShowTransparent.Enabled          := B;
  cbTransparentAsSolid.Enabled       := B;
  acInsertLightSource.Enabled        := B And Not BirdsEye;
  acInsertModelOrigin.Enabled        := B And Not BirdsEye;
  cbShowLightSources.Enabled         := B;
  cbShowModelOrigins.Enabled         := B;
  cbShowHotSpots.Enabled             := B;
  cbShowZoneLines.Enabled            := B;
  tbTimeOfDay.Enabled                := B;
  cbShowStars.Enabled                := B;
  cbRealisticSky.Enabled             := B;
  cbEnableShaders.Enabled            := B;
  cbRunTime.Enabled                  := B;
  acMoveObjectNorth.Enabled          := B;
  acMoveObjectSouth.Enabled          := B;
  acMoveObjectEast.Enabled           := B;
  acMoveObjectWest.Enabled           := B;
  acMoveObjectUp.Enabled             := B;
  acMoveObjectDown.Enabled           := B;
  acRotateObjectLeft.Enabled         := B;
  acRotateObjectRight.Enabled        := B;
  acSizeUp10Percent.Enabled          := B;
  acSizeDown10Percent.Enabled        := B;
  acNewScene.Enabled                 := B;
  acOpenScene.Enabled                := B;
  acSaveScene.Enabled                := B;
  acEditCut.Enabled                  := B;
  acEditCopy.Enabled                 := B;
  acEditPaste.Enabled                := B;
  acEditDelete.Enabled               := B;
  acChangeView.Enabled               := B;
  acChangeWater.Enabled              := B;
  acEditGround.Enabled               := B;
  acGroup.Enabled                    := B;
  acUngroup.Enabled                  := B;
  acExportToWLD.Enabled              := B;
  acExportToS3D.Enabled              := B;
  acExportAsMesh.Enabled             := B;
  acExportZoneLineInfo.Enabled       := B;
  acExportToXWF.Enabled              := B;
  acExportObjectS3D.Enabled          := B;
  acImport3DS.Enabled                := B;
  acImportOGRE.Enabled               := B;
  acImportGround3DS.Enabled          := B;
  acImportQuake3Map.Enabled          := B;
  acExit.Enabled                     := B;
  acExtendEdges.Enabled              := B;
  acSaveAs.Enabled                   := B;
  acPreferences.Enabled              := B;
  acZoneProperties.Enabled           := B;
  acConvertToTransparent.Enabled     := B;
  acCreateLightsWLD.Enabled          := B;
  acCreateObjectsWLD.Enabled         := B;
  acGenerateMap.Enabled              := B;
  acConvert3DSToMsh.Enabled          := B;
  acHelpAbout.Enabled                := B;
  acShiftZone.Enabled                := B;
  acInsertScene.Enabled              := B;
  acTranslate.Enabled                := B;
  acViewScriptLog.Enabled            := B;
  acJumpToObjectLocation.Enabled     := B And Not BirdsEye;
  acSplitWithGround.Enabled          := B;
  acRename.Enabled                   := B;
  acRumpleGround.Enabled             := B;
  acMountainize.Enabled              := B;
  rbCircularArea.Enabled             := B And Not BirdsEye;
  rbSquareArea.Enabled               := B And Not BirdsEye;
  edtRaiseLowerRadius.Enabled        := B And Not BirdsEye;
  udRaiseLowerRadius.Enabled         := B And Not BirdsEye;
  acGetGroundHeight.Enabled          := B And Not BirdsEye;
  acSetGroundHeight.Enabled          := B And Not BirdsEye;
  acRaiseLand.Enabled                := B And Not BirdsEye;
  acFlattenAtPeak.Enabled            := B And Not BirdsEye;
  acFlattenAtAverage.Enabled         := B And Not BirdsEye;
  acFlattenAtLowest.Enabled          := B And Not BirdsEye;
  acAddZoneExit.Enabled              := B And Not BirdsEye;
  cbCrosshair.Enabled                := B And Not BirdsEye;
  acInsertMeshLibraryObject.Enabled  := B And Not BirdsEye;
  cbWalkAlongGround.Enabled          := B And Not BirdsEye;
  acRumpleGroundWithinRadius.Enabled := B And Not BirdsEye;
  cbShowPlayerLight.Enabled          := B And Not BirdsEye;
  acSetObjectPosToCurrent.Enabled    := B And Not BirdsEye;
  acSetObjectPosToCrosshair.Enabled  := B And Not BirdsEye;
  acEditPolygonProperties.Enabled    := B {And (SelectedPolygon <> Nil)} And (SelectedPolyMesh <> Nil);
  acDeletePolygons.Enabled           := B And (SelectedPolygons.Count > 0) And (SelectedPolyMesh <> Nil);
  acConvertToMesh.Enabled            := B;
  acSplitMeshes.Enabled              := B;
  acInvertMeshes.Enabled             := B;
  acInsertHotSpot.Enabled            := B And Not BirdsEye;
  acMoveObjectToLastHotSpot.Enabled  := B And Not BirdsEye;
  acSelectHotSpot.Enabled            := B;
End; // TfrmMain.EnableGUI

procedure TfrmMain.tvMainDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Sender = tvMain) And (Source = tvMain);
end;

procedure TfrmMain.tvMainDragDrop(Sender, Source: TObject; X, Y: Integer);
Var
  TN  : TTreeNode;
  SZO : TZoneObject;
  DZO : TZoneObject;
  SP  : TGroupObject;
  DP  : TGroupObject;

begin
  If (Sender = tvMain) And (Source = tvMain) Then
  Begin
    If htOnLabel In tvMain.GetHitTestInfoAt(X,Y) Then
    Begin
      TN := tvMain.GetNodeAt(X,Y);
      If (TN <> Nil)               And
         (DraggedNode <> Nil)      And
         (TN.Data <> Nil)          And
         (DraggedNode.Data <> Nil) Then
      Begin
        // Get the TZoneObjects that the source and destination nodes point to

        SZO := TZoneObject(DraggedNode.Data);
        DZO := TZoneObject(TN.Data);

        // Make sure the nodes are different

        If SZO <> DZO Then
        Begin
          // Get their parent objects (or if the destination is a group, get it instead)

          SP := SZO.GetParent;
          If DZO Is TGroupObject
           Then DP := TGroupObject(DZO)
           Else DP := DZO.GetParent;

          // Make sure the source and destination parent objects are different

          If SP <> DP Then
          Begin
            // Move the source object all the way up to the root level, or until it's inside the
            // destination group

            While (SP <> Nil) And (SP <> DP) Do
            Begin
              SP.UnGroup(SZO);
              SP := SZO.GetParent;
            End; // While

            // If the source object isn't in the destination group, move it down until it is

            If SP <> DP Then DP.Group(SZO);

            DraggedNode := Nil;

            LoadObjectTreeView;
            ReloadZone;
            RenderZone(BirdsEye);
            If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
            SetViewToNorth;
            DisplayStats;
          End;
        End;
      End;
    End;
  End;
end;

procedure TfrmMain.tvMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DraggedNode := tvMain.GetNodeAt(X,Y);
end;

Procedure TfrmMain.CopyToClipboard;
Var
  I  : Integer;
  ZO : TZoneObject;
Begin
  For I := 0 To Clipboard.Count - 1 Do Clipboard.Objects[I].Free;
  Clipboard.Clear;
  If Not LoadingParms Then
  Begin
    For I := 0 To tvMain.SelectionCount - 1 Do
    Begin
      If (tvMain.Selections[I] <> Nil)      And
         (tvMain.Selections[I].Data <> Nil) Then
      Begin
        ZO := TZoneObject(tvMain.Selections[I].Data);
        Clipboard.AddObject('',ZO.MakeCopy);
        TZoneObject(Clipboard.Objects[Clipboard.Count - 1]).ChangeToAbsolute(ZO.GetParent);
      End;
    End; // For I
  End;
End; // TfrmMain.CopyToClipboard

procedure TfrmMain.acNewSceneExecute(Sender: TObject);
begin
  CreateNewZone;
end;

procedure TfrmMain.acOpenSceneExecute(Sender: TObject);
begin
  dlgOpen.FileName   := '';
  dlgOpen.InitialDir := ExtractFilePath(Application.ExeName) + 'SCENES';
  dlgOpen.Filter     := 'OpenZone scenes (*.scn)|*.SCN';
  If dlgOpen.Execute Then LoadScene(dlgOpen.FileName);

  // Loading the scene might have caused the texture set to change

  SettingTexSet := True;
  If TextureSet <> ''
   Then cbTextureSet.ItemIndex := cbTextureSet.Items.IndexOf(TextureSet)
   Else cbTextureSet.ItemIndex := 0;
  SettingTexSet := False;
end;

procedure TfrmMain.acSaveSceneExecute(Sender: TObject);
begin
  If LastLoadedScene = '' Then acSaveAsExecute(Self) Else SaveScene(LastLoadedScene);
end;

procedure TfrmMain.acSaveAsExecute(Sender: TObject);
Var
  St       : String;
  S        : PChar;
  FileName : String;

begin
  dlgSave.FileName   := '';
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName) + 'SCENES';
  dlgSave.Filter     := 'Scene files (*.scn)|*.SCN';
  If dlgSave.Execute Then
  Begin
    FileName := dlgSave.FileName;
    If Pos('.',FileName) = 0 Then FileName := FileName + '.scn';
    If FileExists(FileName) Then
    Begin
      St := 'Overwrite file ' + FileName + '?';
      S  := StrAlloc(Length(St) + 1);
      StrPCopy(S,St);
      If Application.MessageBox(S,'File Exists',MB_OKCANCEL) = IDOK Then SaveScene(FileName);
      StrDispose(S);
    End
    Else SaveScene(FileName);
  End;
end;

procedure TfrmMain.acImport3DSExecute(Sender: TObject);
begin
  Try
    EnableGUI(False);
    dlgOpen.FileName   := '';
    dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
    dlgOpen.Filter     := '3DS Max files (*.3ds)|*.3DS';
    If dlgOpen.Execute Then ImportFrom3DS(dlgOpen.FileName);
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acImportOGREExecute(Sender: TObject);
begin
  Try
    EnableGUI(False);
    dlgOpen.FileName   := '';
    dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
    dlgOpen.Filter     := 'OGRE XML Mesh files (*.xml)|*.XML';
    If dlgOpen.Execute Then ImportFromOGREXMLMesh(dlgOpen.FileName);
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acExportToWLDExecute(Sender: TObject);
Var
  Stream   : TFileStream;
  St       : String;
  St1      : PChar;
  Continue : Boolean;
  FileName : String;

begin
  Try
    EnableGUI(False);
    dlgSave.FileName   := '';
    dlgSave.InitialDir := ExtractFilePath(Application.ExeName) + 'ZONES';
    dlgSave.Filter     := 'EQ world geometry files (*.wld)|*.WLD';
    If dlgSave.Execute Then
    Begin
      FileName := dlgSave.FileName;
      If Pos('.',FileName) = 0 Then FileName := FileName + '.wld';
      If FileExists(FileName) Then
      Begin
        St  := 'Overwrite file ' + FileName + '?';
        St1 := StrAlloc(Length(St) + 1);
        StrPCopy(St1,St);
        Continue := (Application.MessageBox(St1,'File Exists',MB_OKCANCEL) = IDOK);
        StrDispose(St1);
      End
      Else Continue := True;
      If Continue Then
      Begin
        Stream := TFileStream.Create(FileName,fmCreate);
        ExportToWLD(Stream,FileName);
        Stream.Free;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acExportToS3DExecute(Sender: TObject);
Var
  Stream   : TFileStream;
  FileName : String;
  St       : String;
  St1      : PChar;
  Continue : Boolean;
  ZoneName : String;

begin
  Try
    EnableGUI(False);
    ZoneName := Zone.ShortName;
    If InputQuery('Export to .S3D','Enter new zone name:',ZoneName) Then
    Begin
      ZoneName := Trim(ZoneName);
      If ZoneName <> '' Then
      Begin
        FileName    := ExtractFilePath(Application.ExeName) + 'zones\' + ZoneName + '.s3d';
        If FileExists(FileName) Then
        Begin
          St  := 'Overwrite file ' + FileName + '?';
          St1 := StrAlloc(Length(St) + 1);
          StrPCopy(St1,St);
          Continue := (Application.MessageBox(St1,'File Exists',MB_OKCANCEL) = IDOK);
          StrDispose(St1);
        End
        Else Continue := True;
        If Continue Then
        Begin
          If Application.MessageBox('This will build an .S3D file containing the textures your'#13#10 +
                                    'scene is referencing.'#13#10#13#10 +
                                    'You CANNOT distribute any .S3D files you create unless you'#13#10 +
                                    'have permission to distribute ALL of the textures inside them!'#13#10#13#10 +
                                    'Continue?',
                                    '!! WARNING !!',
                                    MB_OKCANCEL) = IDOK Then
          Begin
            Stream := TFileStream.Create(FileName,fmCreate);
            ExportToS3D(Stream,ZoneName);
            Stream.Free;

            FileName := ExtractFilePath(Application.ExeName) + 'zones\' + ZoneName + '_chr.s3d';
            Stream   := TFileStream.Create(FileName,fmCreate);
            ExportToChrS3D(Stream,ZoneName + '_chr');
            Stream.Free;

            FileName := ExtractFilePath(Application.ExeName) + 'zones\' + ZoneName + '_obj.s3d';
            Stream   := TFileStream.Create(FileName,fmCreate);
            ExportToObjS3D(Stream,ZoneName + '_obj',True);
            Stream.Free;

            Try
              FileName := ExtractFilePath(Application.ExeName) + 'eqemu_maps\' + ZoneName + '.map';
              Stream   := TFileStream.Create(FileName,fmCreate);
              ExportToEQEmuMap(Stream);
              Stream.Free;
            Except
              ShowMessage('Could not export the zone map file.  Please make sure that your eqemu_maps subfolder exists.');
            End;

            FileName := ExtractFilePath(Application.ExeName) + 'zones\' + ZoneName + '_doors.sql';
            ExportToDoorsSQL(FileName);

            FileName := ExtractFilePath(Application.ExeName) + 'zones\' + ZoneName + '_objects.sql';
            ExportToObjectsSQL(FileName);

            MakeSoundFiles(ZoneName);
          End;
        End;
      End
      Else ShowMessage('You must enter a valid zone name.');
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acViewScriptLogExecute(Sender: TObject);
begin
  frmScriptLog.ShowModal;
end;

procedure TfrmMain.acCreateLightsWLDExecute(Sender: TObject);
Var Stream: TFileStream;
begin
  Try
    EnableGUI(False);
    Stream := TFileStream.Create(ExtractFilePath(Application.ExeName) + 'lights.wld',fmCreate);
    CreateEmptyLightsWLD(Stream);
    Stream.Free;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acCreateObjectsWLDExecute(Sender: TObject);
Var Stream: TFileStream;
begin
  Try
    EnableGUI(False);
    Stream := TFileStream.Create(ExtractFilePath(Application.ExeName) + 'objects.wld',fmCreate);
    CreateEmptyObjectsWLD(Stream);
    Stream.Free;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acEditCutExecute(Sender: TObject);
Var
  I    : Integer;
  ZO   : TZoneObject;
  List : TStringList;

begin
  Try
    EnableGUI(False);
    If Not LoadingParms Then
    Begin
      CopyToClipboard;
      List := TStringList.Create;
      For I := 0 To tvMain.SelectionCount - 1 Do
      Begin
        If (tvMain.Selections[I] <> Nil)      And
           (tvMain.Selections[I].Data <> Nil) Then List.AddObject('',TZoneObject(tvMain.Selections[I].Data));
      End; // For I
      ZPropList1.CurObj := Nil;
      Application.ProcessMessages;
      For I := 0 To List.Count - 1 Do
      Begin
        ZO := TZoneObject(List.Objects[I]);
        If ZO.GetParent <> Nil
         Then ZO.GetParent.Objects.Delete(ZO.GetParent.Objects.IndexOfObject(ZO))
         Else Zone.Delete(Zone.IndexOfObject(ZO));
        ZO.Free;
      End; // For I
      LoadObjectTreeView;
      ReloadZone;
      RenderZone(BirdsEye);
      If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
      SetViewToNorth;
      DisplayStats;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acEditCopyExecute(Sender: TObject);
begin
  Try
    EnableGUI(False);
    CopyToClipboard;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acEditPasteExecute(Sender: TObject);
Const MoveAmount = 10;
Var
  I        : Integer;
  ZO       : TZoneObject;
  ClipCopy : TZoneObject;
  MustMove : Boolean;
  OfsX     : Single;
  OfsY     : Single;

  Function NameExistsInClipboard(Check,ZO: TZoneObject): Boolean;
  Var
    I  : Integer;
    GO : TGroupObject;
    B  : Boolean;

  Begin
    B := False;
    If ZO <> Check Then B := (ZO.GetName = Check.GetName);
    If Check Is TGroupObject Then
    Begin
      I  := 0;
      GO := TGroupObject(Check);
      While (I < GO.Objects.Count) And Not B Do
      Begin
        B := B Or NameExistsInClipboard(TZoneObject(GO.Objects.Objects[I]),ZO);
        Inc(I);
      End; // While
    End;
    Result := B;
  End; // NameExistsInClipboard

  Procedure MakeUniqueName(ZO: TZoneObject);
  Var
    GO      : TGroupObject;
    St,St0  : String;
    I,J,K,L : Integer;
    C1,C2   : Char;

  Begin
    St  := ZO.GetName;
    St0 := St;
    I   := 1;
    C1  := #0;
    C2  := #0;
    While Zone.NameExists(St) Or NameExistsInClipboard(ClipCopy,ZO) Do
    Begin
      If St <> '' Then
      Begin
        C1 := St[1];
        C2 := St[Length(St)];
      End;
      If (St0 <> '') And (C2 In ['0'..'9']) And Not (C1 In ['0'..'9']) Then
      Begin
        J := Length(St);
        While (J > 0) And (St[J] In ['0'..'9']) Do Dec(J);
        Val(Copy(St,J + 1,Length(St)),K,L);
        St := Copy(St,1,J) + IntToStr(K + 1);
      End
      Else
      Begin
        St := St0 + IntToStr(I);
        Inc(I);
      End;
      ZO.SetName(St);
    End; // Whle
    If ZO Is TGroupObject Then
    Begin
      GO := TGroupObject(ZO);
      For I := 0 To GO.Objects.Count - 1 Do MakeUniqueName(TZoneObject(GO.Objects.Objects[I]));
    End;
  End; // MakeUniqueName

begin
  Try
    EnableGUI(False);
    If Not LoadingParms Then
    Begin
      OfsX := Random * 2 * MoveAmount - MoveAmount;
      OfsY := Random * 2 * MoveAmount - MoveAmount;
      For I := 0 To Clipboard.Count - 1 Do
      Begin
        ZO := TZoneObject(Clipboard.Objects[I]);
        MustMove := Zone.NameExists(ZO.GetName);
        ClipCopy := ZO.MakeCopy;
        MakeUniqueName(ClipCopy);
        If MustMove Then
        Begin
          ClipCopy.Loc.X := ClipCopy.Loc.X + OfsX;
          ClipCopy.Loc.Y := ClipCopy.Loc.Y + OfsY;
        End;
        AddNewObject(ClipCopy.GetName,ClipCopy);
      End; // For I
      FreeMasterMeshList;
      MasterMeshList := Zone.BuildPolygonLists;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acChangeWaterExecute(Sender: TObject);
Var
  I,J     : Integer;
  Alpha   : Array Of Integer;
  OldLTex : String;
  OldWTex : String;
  MO1     : TMeshObject;
  MO2     : TMeshObject;
  P       : TPolygon;

begin
  Try
    EnableGUI(False);
    SetLength(Alpha,High(Zone.Water) + 1);
    For I := 0 To High(Zone.Water) Do Alpha[I] := TBGRA(Zone.Water[I].Color).A;
    OldLTex := UpperCase(Zone.GetDefaultLandTexture);
    OldWTex := UpperCase(Zone.GetDefaultUnderwaterTexture);
    frmWaterProperties.ShowModal;
    If frmWaterProperties.ModalResult = mrOk Then
    Begin
      Zone.SetDefaultLandTexture(frmWaterProperties.scbLandTex.Text + '+0');
      Zone.SetDefaultUnderwaterTexture(frmWaterProperties.scbUnderwaterTex.Text + '+0');
      SetLength(Zone.Water,High(frmWaterProperties.Water) + 1);
      For J := 0 To High(Zone.Water) Do Zone.Water[J] := frmWaterProperties.Water[J];

      // If the user changed the defaults, we need to scan the meshes and change them accordingly

      MO1 := Zone.FindMeshObject(meshHeightMapGround);
      MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);
      If MO1 <> Nil Then
      Begin
        For I := 0 To MO1.Polygons.Count - 1 Do
        Begin
          P := TPolygon(MO1.Polygons.Objects[I]);
          If UpperCase(P.Texture) = OldLTex Then P.Texture := Zone.GetDefaultLandTexture;
        End; // For I
      End;
      If MO2 <> Nil Then
      Begin
        For I := 0 To MO2.Polygons.Count - 1 Do
        Begin
          P := TPolygon(MO2.Polygons.Objects[I]);
          If UpperCase(P.Texture) = OldWTex Then P.Texture := Zone.GetDefaultUnderwaterTexture;
        End; // For I
      End;

      RegenerateGround;
      ReloadZone;
      RenderZone(BirdsEye);
      If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
      SetViewToNorth;
      DisplayStats;
    End;
    SetLength(Alpha,0);
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acEditGroundExecute(Sender: TObject);
Var
  ZO1,ZO2 : TZoneObject;
  MO1,MO2 : TMeshObject;
  I       : Integer;

begin
  Try
    EnableGUI(False);
    I := Zone.IndexOf(meshHeightMapGround);
    If I >= 0
     Then MO1 := TMeshObject(Zone.Objects[I])
     Else MO1 := Nil;
    I := Zone.IndexOf(meshHeightMapUnderwater);
    If I >= 0
     Then MO2 := TMeshObject(Zone.Objects[I])
     Else MO2 := Nil;

    // Open the ground editor

    frmTexPicker.GridWidth  := Max(Zone.ElevationGrid.NX - 1,0);
    frmTexPicker.GridHeight := Max(Zone.ElevationGrid.NY - 1,0);
    frmTexPicker.MO1        := MO1;
    frmTexPicker.MO2        := MO2;
    frmTexPicker.MinX       := ZoneMinX;
    frmTexPicker.MinY       := ZoneMinY;
    frmTexPicker.MaxX       := ZoneMaxX;
    frmTexPicker.MaxY       := ZoneMaxY;
    If (Zone.ElevationGrid.NX > 0) And (Zone.ElevationGrid.NY > 0) Then
    Begin
      frmTexPicker.GMinX      := Zone.ElevationGrid.MinX;
      frmTexPicker.GMinY      := Zone.ElevationGrid.MinY;
      frmTexPicker.GMaxX      := Zone.ElevationGrid.MaxX;
      frmTexPicker.GMaxY      := Zone.ElevationGrid.MaxY;
    End
    Else
    Begin
      If MasterTreeMaxPt <> Nil Then
      Begin
        frmTexPicker.GMaxX := MasterTreeMaxPt.X;
        frmTexPicker.GMaxY := MasterTreeMaxPt.Y;
      End;
      If MasterTreeMinPt <> Nil Then
      Begin
        frmTexPicker.GMinX := MasterTreeMinPt.X;
        frmTexPicker.GMinY := MasterTreeMinPt.Y;
      End;
    End;

    ZPropList1.CurObj := Nil;
    Application.ProcessMessages;

    // Need to make sure the elevation grid bounds are correct so shading by
    // elevation will work

    Zone.FixElevationGridBounds;

    SelectedPolygons.Clear;
    SelectedPolyMesh := Nil;
    frmTexPicker.ShowModal;
    If frmTexPicker.ModalResult = mrOk Then
    Begin
      ReloadZone;
      RenderZone(BirdsEye);
      If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
      SetViewToNorth;
      DisplayStats;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acInsertSceneExecute(Sender: TObject);
Var
  ZO : TZone;
  I  : Integer;

begin
  Try
    EnableGUI(False);
    dlgOpen.FileName   := '';
    dlgOpen.InitialDir := ExtractFilePath(Application.ExeName) + 'SCENES';
    dlgOpen.Filter     := 'OpenZone scenes (*.scn)|*.SCN';
    If dlgOpen.Execute Then
    Begin
      ZO := TZone.Create;
      BirdsEye          := True;
      glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
      glView.Scene3D.Scene.CheckHidden := Not BirdsEye;
      glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;      
      glView.AllowMouse := BirdsEye;
      ZO.LoadFromFile(dlgOpen.FileName);
      For I := 0 To ZO.Count - 1 Do Zone.AddObject(ZO.Strings[I],ZO.Objects[I]);
      ZO.Clear;
      ZO.Free;

      Observer.Free;
      Observer       := T3DPoint.Create(0,0,0);

      LoadObjectTreeView;

      // Get a polygon list from the zone

      ReloadZone;

      // Render the zone

      RenderZone(BirdsEye);
      glView.Fit(Not BirdsEye,True,False);
      SetViewToNorth;
    End;

    // Loading the scene might have caused the texture set to change

    SettingTexSet := True;
    If TextureSet <> ''
     Then cbTextureSet.ItemIndex := cbTextureSet.Items.IndexOf(TextureSet)
     Else cbTextureSet.ItemIndex := 0;
    SettingTexSet := False;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acTranslateExecute(Sender: TObject);
Var
  X,Y,Z : Double;
  I     : Integer;
  ZO    : TZoneObject;

begin
  Try
    EnableGUI(False);
    If tvMain.SelectionCount > 0 Then
    Begin
      frmTranslate.ShowModal;
      If frmTranslate.ModalResult = mrOk Then
      Begin
        Val(frmTranslate.edtTranslateX.Text,X,I);
        If I = 0 Then
        Begin
          Val(frmTranslate.edtTranslateY.Text,Y,I);
          If I = 0 Then
          Begin
            Val(frmTranslate.edtTranslateZ.Text,Z,I);
            If I = 0 Then
            Begin
              For I := 0 To tvMain.SelectionCount - 1 Do
              Begin
                If (tvMain.Selections[I] <> Nil)      And
                   (tvMain.Selections[I].Data <> Nil) Then
                Begin
                  ZO := TZoneObject(tvMain.Selections[I].Data);
                  ZO.Loc.X := ZO.Loc.X + X;
                  ZO.Loc.Y := ZO.Loc.Y + Y;
                  ZO.Loc.Z := ZO.Loc.Z + Z;
                  If tvMain.SelectionCount = 1 Then
                  Begin
                    ZPropList1.CurObj := Nil; // Force it to reload
                    ZPropList1.CurObj := ZO;
                  End;
                End;
              End; // For I
              ReloadZone;
              RenderZone(BirdsEye);
              If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
              SetViewToNorth;
              DisplayStats;
            End
            Else ShowMessage('Invalid Z value entered.');
          End
          Else ShowMessage('Invalid Y value entered.');
        End
        Else ShowMessage('Invalid X value entered.');
      End;
    End
    Else ShowMessage('You have to select something to move first.');
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acChangeViewExecute(Sender: TObject);
Var I: Integer;
begin
  Try
    EnableGUI(False);
    BirdsEye          := Not BirdsEye;
    glView.AllowMouse := BirdsEye;
    CrosshairX := -1;
    CrosshairY := -1;
    glView.Scene3D.Scene.FrustumCulling := Not BirdsEye;
    glView.Scene3D.Scene.CheckHidden := Not BirdsEye;
    glView.Scene3D.Scene.OcclusionManager.Enabled := Not BirdsEye;
    RenderZone(BirdsEye);
    glView.Fit(Not BirdsEye,True,False);
//    For I := 0 To glView.Scene3D.Scene.Entities.Count - 1 Do TEntity(glView.Scene3D.Scene.Entities.Items[I]).RebuildTransformMatrix;
    SetViewToNorth;
    DisplayStats;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acEditDeleteExecute(Sender: TObject);
Var
  TN   : TTreeNode;
  ZO   : TZoneObject;
  St   : String;
  St1  : PChar;
  I    : Integer;

begin
  Try
    EnableGUI(False);
    If Not LoadingParms Then
    Begin
      If tvMain.SelectionCount > 0 Then
      Begin
        If tvMain.SelectionCount = 1 Then
        Begin
          TN := tvMain.Selections[0];
          If TN <> Nil Then
          Begin
            ZO := TZoneObject(TN.Data);

            // Present a confirmation dialog

            St  := 'Delete object "' + ZO.GetName + '"?    (This cannot be undone!)';
            St1 := StrAlloc(Length(St) + 1);
            StrPCopy(St1,St);
            If Application.MessageBox(St1,'Confirm',MB_OKCANCEL) = IDOK Then
            Begin
              ZPropList1.CurObj := Nil;
              Application.ProcessMessages;
              If ZO.GetParent <> Nil
               Then ZO.GetParent.Objects.Delete(ZO.GetParent.Objects.IndexOfObject(ZO))
               Else Zone.Delete(Zone.IndexOfObject(ZO));
              ZO.Free;
              SelectedPolygons.Clear;
              SelectedPolyMesh := Nil;
              LoadObjectTreeView;
              ReloadZone;
              RenderZone(BirdsEye);
              If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
              SetViewToNorth;
              DisplayStats;
            End;
            StrDispose(St1);
          End;
        End
        Else
        Begin
          St  := 'Delete selected objects?    (This cannot be undone!)';
          St1 := StrAlloc(Length(St) + 1);
          StrPCopy(St1,St);
          If Application.MessageBox(St1,'Confirm',MB_OKCANCEL) = IDOK Then
          Begin
            ZPropList1.CurObj := Nil;
            Application.ProcessMessages;
            For I := 0 To tvMain.SelectionCount - 1 Do
            Begin
              TN := tvMain.Selections[I];
              If TN <> Nil Then
              Begin
                ZO := TZoneObject(TN.Data);
                If Zone.GetObjectIndex(ZO) >= 0 Then
                Begin
                  If ZO.GetParent <> Nil
                   Then ZO.GetParent.Objects.Delete(ZO.GetParent.Objects.IndexOfObject(ZO))
                   Else Zone.Delete(Zone.IndexOfObject(ZO));
                  ZO.Free;
                  SelectedPolygons.Clear;
                  SelectedPolyMesh := Nil;
                End;
              End;
            End; // For I
            LoadObjectTreeView;
            ReloadZone;
            RenderZone(BirdsEye);
            If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
            SetViewToNorth;
            DisplayStats;
          End;
          StrDispose(St1);
        End;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acRenameExecute(Sender: TObject);
Var
  TN      : TTreeNode;
  ZO      : TZoneObject;
  NewName : String;

begin
  Try
    EnableGUI(False);
    If Not LoadingParms Then
    Begin
      If tvMain.SelectionCount = 1 Then
      Begin
        TN := tvMain.Selections[0];
        If TN <> Nil Then
        Begin
          ZO      := TZoneObject(TN.Data);
          NewName := ZO.GetName;
          If InputQuery('Rename Object','Enter new object name:',NewName) Then
          Begin
            NewName := Trim(NewName);
            If NewName <> '' Then
            Begin
              If UpperCase(NewName) <> UpperCase(ZO.GetName) Then
              Begin
                If Zone.NameExists(NewName) Then ShowMessage('An object with that name already exists. Please choose a different one.')
                Else
                Begin
                  ZO.SetName(NewName);
                  LoadObjectTreeView;
                End;
              End;
            End
            Else ShowMessage('You cannot enter a blank name.');
          End;
        End;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acGroupExecute(Sender: TObject);
Var
  X,Y,Z      : Double;
  I          : LongWord;
  J          : Integer;
  ZO         : TZoneObject;
  GO         : TGroupObject;
  GroupName  : String;
  Ok         : Boolean;
  Parent     : TGroupObject;
  HasParent  : Boolean;
  Index      : Integer;
  MaxIndex   : Integer;
  SelIndex   : Array Of Integer;
  IsCreature : Boolean;

begin
  Try
    EnableGUI(False);
    If tvMain.SelectionCount > 0 Then
    Begin
      Ok        := True;
      I         := 0;
      HasParent := False;
      Parent    := Nil;
      Index     := -1;
      MaxIndex  := -1;
      X         := 0;
      Y         := 0;
      Z         := 0;

      // First make sure all objects to be grouped have the same parent

      IsCreature := False;
      While (I < tvMain.SelectionCount) And Ok Do
      Begin
        If (tvMain.Selections[I]      <> Nil) And
           (tvMain.Selections[I].Data <> Nil) Then
        Begin
          ZO := TZoneObject(tvMain.Selections[I].Data);
          IsCreature := IsCreature Or (ZO Is TCreatureLibraryObjectReference);
          If ZO.GetParent <> Nil
           Then J := ZO.GetParent.Objects.IndexOfObject(ZO)
           Else J := ZO.GetZone.IndexOfObject(ZO);
          If J > MaxIndex Then MaxIndex := J;
          If Not HasParent Then
          Begin
            Parent    := ZO.GetParent;
            HasParent := True;
            X         := ZO.Loc.X;
            Y         := ZO.Loc.Y;
            Z         := ZO.Loc.Z;
            Index     := J;
          End
          Else
          Begin
            If ZO.GetParent <> Parent Then Ok := False
            Else
            Begin
              If ZO.Loc.X < X Then X := ZO.Loc.X;
              If ZO.Loc.Y < Y Then Y := ZO.Loc.Y;
              If ZO.Loc.Z < Z Then Z := ZO.Loc.Z;
              If J < Index Then Index := J;
            End;
          End;
        End;
        Inc(I);
      End; // For I

      // If the parents all match, group the objects

      If Ok And (Index >= 0) Then
      Begin
        // Now make sure that no objects are creature references (we can't allow grouping of those or ReloadModel won't load the skeletons properly)

        If Not IsCreature Then
        Begin
          GroupName := '';
          If InputQuery('Group Objects','Enter new group name:',GroupName) Then
          Begin
            GroupName := Trim(GroupName);
            If GroupName <> '' Then
            Begin
              If Not Zone.NameExists(GroupName) Then
              Begin
                // The TTreeView gives you the selections reversed historical order.
                // I want to group simply based on top-down display order, so we have
                // to make an index list.

                SetLength(SelIndex,MaxIndex + 1);
                For I := 0 To High(SelIndex) Do SelIndex[I] := -1;
                For I := 0 To tvMain.SelectionCount - 1 Do
                Begin
                  If (tvMain.Selections[I]      <> Nil) And
                     (tvMain.Selections[I].Data <> Nil) Then
                  Begin
                    ZO := TZoneObject(tvMain.Selections[I].Data);
                    If Parent <> Nil
                     Then J := Parent.Objects.IndexOfObject(ZO)
                     Else J := ZO.GetZone.IndexOfObject(ZO);
                    If (J >= 0) And (J <= High(SelIndex)) Then SelIndex[J] := I;
                  End;
                End; // For I

                // Create and insert the group object

                GO := TGroupObject.Create(GroupName);
                GO.Loc.X := X;
                GO.Loc.Y := Y;
                GO.Loc.Z := Z;
                If Parent <> Nil
                 Then Parent.Objects.InsertObject(Index,GroupName,GO)
                 Else Zone.InsertObject(Index,GroupName,GO);
                GO.SetParent(Parent);

                // Group the objects

                For I := 0 To High(SelIndex) Do
                Begin
                  If SelIndex[I] >= 0 Then
                  Begin
                    ZO := TZoneObject(tvMain.Selections[SelIndex[I]].Data);
                    GO.Group(ZO);
                  End;
                End; // For I
                SetLength(SelIndex,0);
                SelectedPolygons.Clear;
                SelectedPolyMesh := Nil;
                LoadObjectTreeView;
                ReloadZone;
                RenderZone(BirdsEye);
                If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
                SetViewToNorth;
                DisplayStats;
              End
              Else ShowMessage('An object with that name already exists. Please choose a different one.');
            End
            Else ShowMessage('You cannot enter a blank name.');
          End;
        End
        Else ShowMessage('You cannot group creature objects');
      End
      Else ShowMessage('You can only group objects that have the same parent object.');
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acUngroupExecute(Sender: TObject);
Var
  ZO : TZoneObject;
  GO : TGroupObject;

begin
  Try
    EnableGUI(False);
    If tvMain.SelectionCount = 1 Then
    Begin
      If (tvMain.Selections[0] <> Nil)      And
         (tvMain.Selections[0].Data <> Nil) Then
      Begin
        ZO := TZoneObject(tvMain.Selections[0].Data);
        If ZO Is TGroupObject Then
        Begin
          GO := TGroupObject(ZO);
          If GO.Objects.Count > 0 Then
          Begin
            GO.UnGroupAll;
                 If GO.GetParent <> Nil Then GO.GetParent.Objects.Delete(GO.GetParent.Objects.IndexOfObject(GO))
            Else If GO.GetZone   <> Nil Then GO.GetZone.Delete(GO.GetZone.IndexOfObject(GO));
            SelectedPolygons.Clear;
            SelectedPolyMesh := Nil;
            LoadObjectTreeView;
            ReloadZone;
            RenderZone(BirdsEye);
            If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
            SetViewToNorth;
            DisplayStats;
          End
          Else ShowMessage('This group object is empty.  This shouldn''t ever happen...');
        End
        Else
        Begin
          If ZO.GetParent <> Nil Then
          Begin
            ZO.GetParent.UnGroup(ZO);
            SelectedPolygons.Clear;
            SelectedPolyMesh := Nil;
            LoadObjectTreeView;
            ReloadZone;
            RenderZone(BirdsEye);
            If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
            SetViewToNorth;
            DisplayStats;
          End;
        End;
      End;
    End
    Else ShowMessage('You must select one group object to ungroup');
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acHelpMovementKeysExecute(Sender: TObject);
begin
  Application.HelpJump('Looking_at_your_zone_and_moving_around_in_it');
end;

Procedure TfrmMain.RegenerateGroundMeshes;
Var MO1,MO2: TMeshObject;
Begin
  RegenerateGround;
  MO1 := Zone.FindMeshObject(meshHeightMapGround);
  MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);
  If MO1 <> Nil Then RefreshObject(MO1,True,True,True);
  If MO2 <> Nil Then RefreshObject(MO2,True,True,True);
  CrosshairX := -1;
  CrosshairY := -1;
  RenderZone(BirdsEye);
  If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
  SetViewToNorth;
End; // TfrmMain.RegenerateGroundMeshes

procedure TfrmMain.acGetGroundHeightExecute(Sender: TObject);
Var
  MO1,MO2   : TMeshObject;
  SX,SY     : Integer;
  X,Y       : Single;
  X0,Y0     : Integer;

begin
  Try
    EnableGUI(False);
    MO1 := Zone.FindMeshObject(meshHeightMapGround);
    MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);
    If (MO1 <> Nil) Or (MO2 <> Nil) Then
    Begin
      X         := Observer.X + GetHeightDist * Cos(Phi);
      Y         := Observer.Y + GetHeightDist * Sin(Phi);
      SX        := GridSize;
      SY        := GridSize;
      Y0        := Round((Zone.ElevationGrid.MaxX - X) / SX);
      X0        := Round((Zone.ElevationGrid.MaxY - Y) / SY);
      SetHeight := Zone.ElevationGrid.GetHeight(X0,Y0);
      DisplayStats;
    End
    Else ShowMessage('Could not find any ground or underwater areas.');
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acSetGroundHeightExecute(Sender: TObject);
Var MO1,MO2: TMeshObject;
begin
  Try
    EnableGUI(False);
    MO1 := Zone.FindMeshObject(meshHeightMapGround);
    MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);

    // Continue only if we found the mesh we're looking for

    If (MO1 <> Nil) Or (MO2 <> Nil)
     Then FlattenGroundAtElevation(SetHeight)
     Else ShowMessage('Could not locate the ground mesh.');
  Finally
    SelectedPolygons.Clear;
    SelectedPolyMesh := Nil;
    EnableGUI(True);
  End;
end;

Procedure TfrmMain.SetCrosshair;
Var
  Model   : TModel;
  Mdl     : TModelRec;
  X,Y     : Single;
  X0,Y0   : Integer;
  X1,Y1   : Integer;
  I,J     : Integer;
  SX,SY   : Integer;
  Z0      : Single;
  Z1,Z2   : Single;
  Z3,Z4   : Single;
  Z1A,Z2A : Single;
  Z3A,Z4A : Single;
  Z5,Z6   : Single;
  Z7,Z8   : Single;
  Dist    : Single;
  C,S,CS  : Single;

  Procedure FreeCrosshair;
  Begin
    Model := glView.Scene3D.GetNewModel(EntityCrosshair);
    Model.Clear;
  End; // FreeCrosshair

Begin
  SetLength(Mdl.V,0);
  SetLength(Mdl.P,0);
  Mdl.NV := 0;
  Mdl.NP := 0;
  glView.Scene3D.Scene.LockBSPTree('TfrmMain.SetCrosshair');
  If Not BirdsEye Then
  Begin
    Case Crosshair Of
      crNone: FreeCrosshair;
      crCreateObject:
      Begin
        FreeCrosshair;
        X := Observer.X + CreateDist * Cos(Phi);
        Y := Observer.Y + CreateDist * Sin(Phi);

        SetLength(Mdl.V,12);
        SetLength(Mdl.P,4);
        Mdl.NV := 12;
        Mdl.NP := 4;

        For I := 0 To 11 Do
        Begin
          Mdl.V[I].Z        := Observer.Z;
          Mdl.V[I].Color    := $FF00FFFF;
        End; // For I

        Mdl.V[2].X := X;
        Mdl.V[2].Y := Y;
        Mdl.V[1].X := X + 5 * Cos(Phi + 60 * Pi / 180);
        Mdl.V[1].Y := Y + 5 * Sin(Phi + 60 * Pi / 180);
        Mdl.V[0].X := X + 5 * Cos(Phi + 30 * Pi / 180);
        Mdl.V[0].Y := Y + 5 * Sin(Phi + 30 * Pi / 180);

        Mdl.V[5].X := X;
        Mdl.V[5].Y := Y;
        Mdl.V[4].X := X + 5 * Cos(Phi + 150 * Pi / 180);
        Mdl.V[4].Y := Y + 5 * Sin(Phi + 150 * Pi / 180);
        Mdl.V[3].X := X + 5 * Cos(Phi + 120 * Pi / 180);
        Mdl.V[3].Y := Y + 5 * Sin(Phi + 120 * Pi / 180);

        Mdl.V[8].X := X;
        Mdl.V[8].Y := Y;
        Mdl.V[7].X := X + 5 * Cos(Phi + 240 * Pi / 180);
        Mdl.V[7].Y := Y + 5 * Sin(Phi + 240 * Pi / 180);
        Mdl.V[6].X := X + 5 * Cos(Phi + 210 * Pi / 180);
        Mdl.V[6].Y := Y + 5 * Sin(Phi + 210 * Pi / 180);

        Mdl.V[11].X := X;
        Mdl.V[11].Y := Y;
        Mdl.V[10].X := X + 5 * Cos(Phi + 330 * Pi / 180);
        Mdl.V[10].Y := Y + 5 * Sin(Phi + 330 * Pi / 180);
        Mdl.V[9].X  := X + 5 * Cos(Phi + 300 * Pi / 180);
        Mdl.V[9].Y  := Y + 5 * Sin(Phi + 300 * Pi / 180);

        For I := 0 To Mdl.NP - 1 Do Mdl.P[I].TextureID := -1;

        // Add the model to the GL viewer

        glView.AddModelToFrame(Mdl,0,False,False);
        Model.CalcExtents(False);
      End;
      crGetSetGroundHeight:
      Begin
        X  := Observer.X + GetHeightDist * Cos(Phi);
        Y  := Observer.Y + GetHeightDist * Sin(Phi);
        SX := GridSize;
        SY := GridSize;
        Y0 := Round((Zone.ElevationGrid.MaxX - X) / SX);
        X0 := Round((Zone.ElevationGrid.MaxY - Y) / SY);
        If (X0 <> CrosshairX) Or (Y0 <> CrosshairY) Then
        Begin
          FreeCrosshair;
          CrosshairX := X0;
          CrosshairY := Y0;

          SetLength(Mdl.V,0);
          SetLength(Mdl.P,0);
          Mdl.NV := 0;
          Mdl.NP := 0;
          C      := Cos(30 * Pi / 180);
          S      := Sin(30 * Pi / 180);
          CS     := Cos(45 * Pi / 180);
          For X1 := X0 - 6 To X0 + 6 Do
          Begin
            If (X1 >= 0) And (X1 < Zone.ElevationGrid.NX) Then
            Begin
              Y := Zone.ElevationGrid.MaxY - (X1 * SY);
              For Y1 := Y0 - 6 To Y0 + 6 Do
              Begin
                If (Y1 >= 0) And (Y1 < Zone.ElevationGrid.NY) Then
                Begin
                  X := Zone.ElevationGrid.MaxX - (Y1 * SX);

                  Inc(Mdl.NV,18);
                  Inc(Mdl.NP,6);
                  SetLength(Mdl.V,Mdl.NV);
                  SetLength(Mdl.P,Mdl.NP);
                  Z0 := Zone.ElevationGrid.GetHeight(X1,Y1) + 0.2;

//                  For I := 0 To Mdl.NV - 1 Do Mdl.V[I].UseColor := False;

                  If X1 > 0 Then
                  Begin
                    Z2 := Zone.ElevationGrid.GetHeight(X1 - 1,Y1) + 0.2;
                    If Y1 > 0
                     Then Z5 := Zone.ElevationGrid.GetHeight(X1 - 1,Y1 - 1) + 0.2
                     Else Z5 := Z2;
                    If Y1 < Zone.ElevationGrid.NY - 1
                     Then Z6 := Zone.ElevationGrid.GetHeight(X1 - 1,Y1 + 1) + 0.2
                     Else Z6 := Z2;
                  End
                  Else
                  Begin
                    Z2 := Z0;
                    If Y1 > 0
                     Then Z5 := Zone.ElevationGrid.GetHeight(X1,Y1 - 1) + 0.2
                     Else Z5 := Z2;
                    If Y1 < Zone.ElevationGrid.NY - 1
                     Then Z6 := Zone.ElevationGrid.GetHeight(X1,Y1 + 1) + 0.2
                     Else Z6 := Z2;
                  End;

                  If Y1 > 0
                   Then Z1 := Zone.ElevationGrid.GetHeight(X1,Y1 - 1) + 0.2
                   Else Z1 := Z0;

                  If X1 < Zone.ElevationGrid.NX - 1 Then
                  Begin
                    Z4 := Zone.ElevationGrid.GetHeight(X1 + 1,Y1) + 0.2;
                    If Y1 > 0
                     Then Z8 := Zone.ElevationGrid.GetHeight(X1 + 1,Y1 - 1) + 0.2
                     Else Z8 := Z4;
                    If Y1 < Zone.ElevationGrid.NY - 1
                     Then Z7 := Zone.ElevationGrid.GetHeight(X1 + 1,Y1 + 1) + 0.2
                     Else Z7 := Z4;
                  End
                  Else
                  Begin
                    Z4 := Z0;
                    If Y1 > 0
                     Then Z8 := Zone.ElevationGrid.GetHeight(X1,Y1 - 1) + 0.2
                     Else Z8 := Z4;
                    If Y1 < Zone.ElevationGrid.NY - 1
                     Then Z7 := Zone.ElevationGrid.GetHeight(X1,Y1 + 1) + 0.2
                     Else Z7 := Z4;
                  End;

                  If Y1 < Zone.ElevationGrid.NY - 1
                   Then Z3 := Zone.ElevationGrid.GetHeight(X1,Y1 + 1) + 0.2
                   Else Z3 := Z0;

                  Mdl.V[Mdl.NV - 16].X := X;
                  Mdl.V[Mdl.NV - 16].Y := Y;
                  Mdl.V[Mdl.NV - 16].Z := Z0;
                  Mdl.V[Mdl.NV - 17].X := X - 5 * C;
                  Mdl.V[Mdl.NV - 17].Y := Y + 5 * S;
                  Mdl.V[Mdl.NV - 17].Z := Z0 + (5 / SX) * (Z3 - Z0) * C + (5 / SY) * (Z2 - Z0) * S;
                  Mdl.V[Mdl.NV - 18].X := X - 5 * S;
                  Mdl.V[Mdl.NV - 18].Y := Y + 5 * C;
                  Mdl.V[Mdl.NV - 18].Z := Z0 + (5 / SX) * (Z3 - Z0) * S + (5 / SY) * (Z2 - Z0) * C;

                  Z3A := Z3 + 2 * ((Z0 + Z7) / 2 - Z3);
                  Z4A := Z4 + 2 * ((Z0 + Z7) / 2 - Z4);

                  Mdl.V[Mdl.NV - 13].X := X;
                  Mdl.V[Mdl.NV - 13].Y := Y;
                  Mdl.V[Mdl.NV - 13].Z := Z0;
                  Mdl.V[Mdl.NV - 14].X := X - 5 * S;
                  Mdl.V[Mdl.NV - 14].Y := Y - 5 * C;
                  Mdl.V[Mdl.NV - 14].Z := Z0 + (5 / SX) * (Z4A - Z0) * S + (5 / SY) * (Z4 - Z0) * C;
                  Mdl.V[Mdl.NV - 15].X := X - 5 * CS;
                  Mdl.V[Mdl.NV - 15].Y := Y - 5 * CS;
                  Mdl.V[Mdl.NV - 15].Z := Z0 + (5 / SX) * (Z7 - Z0) * CS;

                  Mdl.V[Mdl.NV - 10].X := X;
                  Mdl.V[Mdl.NV - 10].Y := Y;
                  Mdl.V[Mdl.NV - 10].Z := Z0;
                  Mdl.V[Mdl.NV - 11].X := X - 5 * CS;
                  Mdl.V[Mdl.NV - 11].Y := Y - 5 * CS;
                  Mdl.V[Mdl.NV - 11].Z := Z0 + (5 / SX) * (Z7 - Z0) * CS;
                  Mdl.V[Mdl.NV - 12].X := X - 5 * C;
                  Mdl.V[Mdl.NV - 12].Y := Y - 5 * S;
                  Mdl.V[Mdl.NV - 12].Z := Z0 + (5 / SX) * (Z3 - Z0) * C + (5 / SY) * (Z3A - Z0) * S;

                  Z2A := Z2 + 2 * ((Z0 + Z5) / 2 - Z2);
                  Z1A := Z1 + 2 * ((Z0 + Z5) / 2 - Z1);

                  Mdl.V[Mdl.NV - 7].X := X;
                  Mdl.V[Mdl.NV - 7].Y := Y;
                  Mdl.V[Mdl.NV - 7].Z := Z0;
                  Mdl.V[Mdl.NV - 8].X := X + 5 * S;
                  Mdl.V[Mdl.NV - 8].Y := Y + 5 * C;
                  Mdl.V[Mdl.NV - 8].Z := Z0 + (5 / SX) * (Z2A - Z0) * S + (5 / SY) * (Z2 - Z0) * C;
                  Mdl.V[Mdl.NV - 9].X := X + 5 * CS;
                  Mdl.V[Mdl.NV - 9].Y := Y + 5 * CS;
                  Mdl.V[Mdl.NV - 9].Z := Z0 + (5 / SX) * (Z5 - Z0) * CS;

                  Mdl.V[Mdl.NV - 4].X := X;
                  Mdl.V[Mdl.NV - 4].Y := Y;
                  Mdl.V[Mdl.NV - 4].Z := Z0;
                  Mdl.V[Mdl.NV - 5].X := X + 5 * CS;
                  Mdl.V[Mdl.NV - 5].Y := Y + 5 * CS;
                  Mdl.V[Mdl.NV - 5].Z := Z0 + (5 / SX) * (Z5 - Z0) * CS;
                  Mdl.V[Mdl.NV - 6].X := X + 5 * C;
                  Mdl.V[Mdl.NV - 6].Y := Y + 5 * S;
                  Mdl.V[Mdl.NV - 6].Z := Z0 + (5 / SX) * (Z1 - Z0) * C + (5 / SY) * (Z1A - Z0) * S;

                  Mdl.V[Mdl.NV - 1].X := X;
                  Mdl.V[Mdl.NV - 1].Y := Y;
                  Mdl.V[Mdl.NV - 1].Z := Z0;
                  Mdl.V[Mdl.NV - 2].X := X + 5 * C;
                  Mdl.V[Mdl.NV - 2].Y := Y - 5 * S;
                  Mdl.V[Mdl.NV - 2].Z := Z0 + (5 / SX) * (Z1 - Z0) * C + (5 / SY) * (Z4 - Z0) * S;
                  Mdl.V[Mdl.NV - 3].X := X + 5 * S;
                  Mdl.V[Mdl.NV - 3].Y := Y - 5 * C;
                  Mdl.V[Mdl.NV - 3].Z := Z0 + (5 / SX) * (Z1 - Z0) * S + (5 / SY) * (Z4 - Z0) * C;

                  For I := Mdl.NP - 6 To Mdl.NP - 1 Do
                  Begin
                    Mdl.P[I].TextureID   := -1;

                    For J := 0 To 2 Do
                    Begin
                      If rbCircularArea.Checked Then
                      Begin
                        Dist := Sqrt(Sqr(X1 - X0) + Sqr(Y1 - Y0));
                             If (X1 = X0) And (Y1 = Y0) Then Mdl.V[I * 3 + J].Color := $FF0000FF
                        Else If Dist <= udRaiseLowerRadius.Position Then Mdl.V[I * 3 + J].Color := $FF0080FF
                        Else Mdl.V[I * 3 + J].Color := $FF00FFFF;
                      End
                      Else
                      Begin
                             If (X1 = X0) And (Y1 = Y0) Then Mdl.V[I * 3 + J].Color := $FF0000FF
                        Else If (Abs(X1 - X0) <= udRaiseLowerRadius.Position) And
                                (Abs(Y1 - Y0) <= udRaiseLowerRadius.Position) Then Mdl.V[I * 3 + J].Color := $FF0080FF
                        Else Mdl.V[I * 3 + J].Color := $FF00FFFF;
                      End;
                    End; // For J
                  End; // For I
                End;
              End; // For Y1
            End;
          End; // For X1

          // Add the model to the GL viewer

          glView.AddModelToFrame(Mdl,EntityCrosshair,False,False);
          Model.CalcExtents(False);
        End;
      End;
    End; // Case
  End
  Else FreeCrosshair;
  glView.Scene3D.Scene.UnlockBSPTree;
  SetLength(Mdl.v,0);
  SetLength(Mdl.p,0);
End; // TfrmMain.SetCrosshair

Procedure TfrmMain.SetZoneLinePolygons;
Var
  Model    : TModel;
  I,J      : Integer;
  ZP       : TZonePlane;
  Mdl      : TModelRec;

Begin
  glView.Scene3D.Scene.LockBSPTree('TfrmMain.SetZoneLinePolygons');
  Model := glView.Scene3D.GetNewModel(EntityZoneLines);
  If (Zone.ZonePlanes.Count > 0) And cbShowZonelines.Checked Then
  Begin
    Mdl.NP := Zone.ZonePlanes.Count * 4;
    Mdl.NV := Mdl.NP * 3;
    SetLength(Mdl.P,Mdl.NP);
    SetLength(Mdl.V,Mdl.NV);
    For I := 0 To Zone.ZonePlanes.Count - 1 Do
    Begin
      ZP                         := TZonePlane(Zone.ZonePlanes.Objects[I]);

      Mdl.P[I * 4 + 0].TextureID := -1;
      Mdl.P[I * 4 + 1].TextureID := -1;
      Mdl.P[I * 4 + 2].TextureID := -1;
      Mdl.P[I * 4 + 3].TextureID := -1;

      Mdl.V[I * 12 + 0].X        := ZP.X1;
      Mdl.V[I * 12 + 0].Y        := ZP.Y1;
      Mdl.V[I * 12 + 1].X        := ZP.X1;
      Mdl.V[I * 12 + 1].Y        := ZP.Y1;
      Mdl.V[I * 12 + 2].X        := ZP.X2;
      Mdl.V[I * 12 + 2].Y        := ZP.Y2;
      Mdl.V[I * 12 + 3].X        := ZP.X2;
      Mdl.V[I * 12 + 3].Y        := ZP.Y2;
      Mdl.V[I * 12 + 4].X        := ZP.X2;
      Mdl.V[I * 12 + 4].Y        := ZP.Y2;
      Mdl.V[I * 12 + 5].X        := ZP.X1;
      Mdl.V[I * 12 + 5].Y        := ZP.Y1;
      If ZP.InfiniteZ Then
      Begin
        Mdl.V[I * 12 + 0].Z := Zone.BoundMinZ - BoundOffset;
        Mdl.V[I * 12 + 1].Z := Zone.BoundMaxZ + BoundOffset;
        Mdl.V[I * 12 + 2].Z := Zone.BoundMaxZ + BoundOffset;
        Mdl.V[I * 12 + 3].Z := Zone.BoundMaxZ + BoundOffset;
        Mdl.V[I * 12 + 4].Z := Zone.BoundMinZ - BoundOffset;
        Mdl.V[I * 12 + 5].Z := Zone.BoundMinZ - BoundOffset;
      End
      Else
      Begin
        Mdl.V[I * 12 + 0].Z := ZP.Z1;
        Mdl.V[I * 12 + 1].Z := ZP.Z2;
        Mdl.V[I * 12 + 2].Z := ZP.Z2;
        Mdl.V[I * 12 + 3].Z := ZP.Z2;
        Mdl.V[I * 12 + 4].Z := ZP.Z1;
        Mdl.V[I * 12 + 5].Z := ZP.Z1;
      End;
      For J := 0 To 11 Do Mdl.V[I * 12 + J].Color := $80FFC8FF;
      For J := 0 To 5  Do Mdl.V[I * 12 + 11 - J] := Mdl.V[I * 12 + J];
    End; // For I

    // Add the model to the GL viewer

    glView.AddModelToFrame(Mdl,EntityZoneLines,False,False);
    Model.CalcExtents(False);
    SetLength(Mdl.V,0);
    SetLength(Mdl.P,0);
  End;
  glView.Scene3D.Scene.UnlockBSPTree;
End; // TfrmMain.SetZoneLinePolygons

Procedure TfrmMain.SetSelectionRectangles;
Const SelOffset = 1;
Var
  Entity   : TEntity;
  Model    : TModel;
  Mdl      : TModelRec;
  MinPos   : T3DPoint;
  MaxPos   : T3DPoint;
  FirstTex : String;
  I,J,K,L  : Integer;
  ZO       : TZoneObject;
  TN       : TTreeNode;
  HS       : THotSpot;
  List     : TStringList;

Begin
  If tvMain.SelectionCount > 0 Then
  Begin
    glView.Scene3D.Scene.LockBSPTree('TfrmMain.SetSelectionRectangles');
    Model := glView.Scene3D.GetNewModel(EntitySelectionRectangle);
    TEntity(glView.Scene3D.Scene.Entities.Items[EntitySelectionRectangle]).WireFrame := wfLines;
    Mdl.NV := tvMain.SelectionCount * 24;
    Mdl.NP := tvMain.SelectionCount * 6;
    SetLength(Mdl.V,Mdl.NV);
    SetLength(Mdl.P,Mdl.NP);
    For I := 0 To Mdl.NV - 1 Do Mdl.V[I].Color := $FF00FFFF;
    For I := 0 To Mdl.NP - 1 Do Mdl.P[I].TextureID   := -1;
    L := 0;
    For K := 0 To tvMain.SelectionCount - 1 Do
    Begin
      TN := tvMain.Selections[K];
      If TN <> Nil Then
      Begin
        ZO := TZoneObject(TN.Data);
        If ZO <> Nil Then
        Begin
          If AllowHotSpotChange Then
          Begin
            If ZO Is THotSpot Then
            Begin
              SelectedHotSpot   := THotSpot(ZO);
              SelectedHotSpotML := Nil;
            End
            Else If ZO Is TMeshLibraryObjectReference Then
            Begin
              List := TMeshLibraryObjectReference(ZO).Group.GetHotSpots;
              If List.Count > 0 Then
              Begin
                HS := THotSpot(List.Objects[0]);
                If HS <> Nil Then
                Begin
                  SelectedHotSpot   := HS;
                  SelectedHotSpotML := TMeshLibraryObjectReference(ZO);
                End;
              End;
              List.Free;
            End;
          End;

          If ZO Is TCreatureLibraryObjectReference Then
          Begin
            I := Zone.GetObjectIndex(ZO);
            If (I >= 0) And (I + FirstEntity < glView.Scene3D.Scene.Entities.Count) Then
            Begin
              Entity := TEntity(glView.Scene3D.Scene.Entities.Items[I + FirstEntity]);
              Entity.GetExtents;
              MinPos := T3DPoint.Create(Entity.UnadjustedBox.MinPt);
              MaxPos := T3DPoint.Create(Entity.UnadjustedBox.MaxPt);
              MinPos.Multiply(ProgramSettings.XWFCreatureSize);
              MaxPos.Multiply(ProgramSettings.XWFCreatureSize);
              MinPos.Add(ZO.Loc);
              MaxPos.Add(ZO.Loc);
            End
            Else ZO.GetSize(MinPos,MaxPos,FirstTex);
          End
          Else ZO.GetSize(MinPos,MaxPos,FirstTex);

          // Front

          Mdl.V[0 + L].X := MaxPos.X + SelOffset;
          Mdl.V[0 + L].Y := MinPos.Y - SelOffset;
          Mdl.V[0 + L].Z := MinPos.Z - SelOffset;

          Mdl.V[1 + L].X := MaxPos.X + SelOffset;
          Mdl.V[1 + L].Y := MinPos.Y - SelOffset;
          Mdl.V[1 + L].Z := MaxPos.Z + SelOffset;

          Mdl.V[2 + L].X := MinPos.X - SelOffset;
          Mdl.V[2 + L].Y := MinPos.Y - SelOffset;
          Mdl.V[2 + L].Z := MaxPos.Z + SelOffset;

          Mdl.V[3 + L].X := MinPos.X - SelOffset;
          Mdl.V[3 + L].Y := MinPos.Y - SelOffset;
          Mdl.V[3 + L].Z := MinPos.Z - SelOffset;

          // Back

          Mdl.V[4 + L].X := MinPos.X - SelOffset;
          Mdl.V[4 + L].Y := MaxPos.Y + SelOffset;
          Mdl.V[4 + L].Z := MinPos.Z - SelOffset;

          Mdl.V[5 + L].X := MinPos.X - SelOffset;
          Mdl.V[5 + L].Y := MaxPos.Y + SelOffset;
          Mdl.V[5 + L].Z := MaxPos.Z + SelOffset;

          Mdl.V[6 + L].X := MaxPos.X + SelOffset;
          Mdl.V[6 + L].Y := MaxPos.Y + SelOffset;
          Mdl.V[6 + L].Z := MaxPos.Z + SelOffset;

          Mdl.V[7 + L].X := MaxPos.X + SelOffset;
          Mdl.V[7 + L].Y := MaxPos.Y + SelOffset;
          Mdl.V[7 + L].Z := MinPos.Z - SelOffset;

          // Left

          Mdl.V[8 + L]  := Mdl.V[3 + L];
          Mdl.V[9 + L]  := Mdl.V[2 + L];
          Mdl.V[10 + L] := Mdl.V[5 + L];
          Mdl.V[11 + L] := Mdl.V[4 + L];

          // Right

          Mdl.V[12 + L] := Mdl.V[7 + L];
          Mdl.V[13 + L] := Mdl.V[6 + L];
          Mdl.V[14 + L] := Mdl.V[1 + L];
          Mdl.V[15 + L] := Mdl.V[0 + L];

          // Top

          Mdl.V[16 + L] := Mdl.V[1 + L];
          Mdl.V[17 + L] := Mdl.V[6 + L];
          Mdl.V[18 + L] := Mdl.V[5 + L];
          Mdl.V[19 + L] := Mdl.V[2 + L];

          // Bottom

          Mdl.V[20 + L] := Mdl.V[7 + L];
          Mdl.V[21 + L] := Mdl.V[0 + L];
          Mdl.V[22 + L] := Mdl.V[3 + L];
          Mdl.V[23 + L] := Mdl.V[4 + L];

          MinPos.Free;
          MaxPos.Free;
        End;
        Inc(L,24);
      End;
    End; // For K

    // Add the model to the GL viewer

    glView.AddModelToFrame(Mdl,EntitySelectionRectangle,False,False);
    Model.CalcExtents(False);
    SetLength(Mdl.V,0);
    SetLength(Mdl.P,0);
    glView.Scene3D.Scene.UnlockBSPTree;
  End;
End; // TfrmMain.SetSelectionRectangle

Procedure TfrmMain.SetPolygonSelectionRectangles;
Const SelOffset = 1;
Var
  Model     : TModel;
  Mdl       : TModelRec;
  V1,V2     : T3DPoint;
  I,J,K,L,M : Integer;
  Normal    : T3DPoint;
  Center    : T3DPoint;
  P         : TPolygon;
  MO        : TMeshObject;

Begin
  glView.Scene3D.Scene.LockBSPTree('TfrmMain.SetPolygonSelectionRectangles');
  Model := glView.Scene3D.GetNewModel(EntityPolygonSelectionRectangle);
  TEntity(glView.Scene3D.Scene.Entities.Items[EntityPolygonSelectionRectangle]).WireFrame := wfLines;
  If (SelectedPolygons.Count > 0) And (SelectedPolyMesh <> Nil) Then
  Begin
    MO := SelectedPolyMesh;
    J  := 0;
    For L := 0 To SelectedPolygons.Count - 1 Do
    Begin
      P := TPolygon(MO.Polygons.Objects[Integer(SelectedPolygons.Items[L])]);
      Inc(J,High(P.Vertices) + 1);
    End; // For L
    Mdl.NP := J * 4;
    Mdl.NV := J * 12;
    SetLength(Mdl.V,Mdl.NV);
    SetLength(Mdl.P,Mdl.NP);
    For I := 0 To Mdl.NV - 1 Do Mdl.V[I].Color    := $FF00FF00;  // Green
    For I := 0 To Mdl.NP - 1 Do Mdl.P[I].TextureID := -1;
    M := 0;
    For L := 0 To SelectedPolygons.Count - 1 Do
    Begin
      P := TPolygon(MO.Polygons.Objects[Integer(SelectedPolygons.Items[L])]);
      J := High(P.Vertices) + 1;

      Normal := P.GetNormal(MO);
      Center := P.GetCenter(MO);

      V1 := T3DPoint.Create;
      V2 := T3DPoint.Create;
      For I := 0 To J - 1 Do
      Begin
        K  := (I + 1) Mod J;
        V1.Copy(T3DPoint(MO.Vertices.Objects[P.Vertices[I]]));
        V2.Copy(T3DPoint(MO.Vertices.Objects[P.Vertices[K]]));
        MO.MakeAbsolute(V1);
        MO.MakeAbsolute(V2);

        Mdl.V[I * 6 + 0 + M * 12].X := V1.X + Normal.X * SelOffset;
        Mdl.V[I * 6 + 0 + M * 12].Y := V1.Y + Normal.Y * SelOffset;
        Mdl.V[I * 6 + 0 + M * 12].Z := V1.Z + Normal.Z * SelOffset;

        Mdl.V[I * 6 + 1 + M * 12].X := V2.X + Normal.X * SelOffset;
        Mdl.V[I * 6 + 1 + M * 12].Y := V2.Y + Normal.Y * SelOffset;
        Mdl.V[I * 6 + 1 + M * 12].Z := V2.Z + Normal.Z * SelOffset;

        Mdl.V[I * 6 + 2 + M * 12].X := V2.X - Normal.X * SelOffset;
        Mdl.V[I * 6 + 2 + M * 12].Y := V2.Y - Normal.Y * SelOffset;
        Mdl.V[I * 6 + 2 + M * 12].Z := V2.Z - Normal.Z * SelOffset;

        Mdl.V[I * 6 + 3 + M * 12].X := V2.X - Normal.X * SelOffset;
        Mdl.V[I * 6 + 3 + M * 12].Y := V2.Y - Normal.Y * SelOffset;
        Mdl.V[I * 6 + 3 + M * 12].Z := V2.Z - Normal.Z * SelOffset;

        Mdl.V[I * 6 + 4 + M * 12].X := V1.X - Normal.X * SelOffset;
        Mdl.V[I * 6 + 4 + M * 12].Y := V1.Y - Normal.Y * SelOffset;
        Mdl.V[I * 6 + 4 + M * 12].Z := V1.Z - Normal.Z * SelOffset;

        Mdl.V[I * 6 + 5 + M * 12].X := V1.X + Normal.X * SelOffset;
        Mdl.V[I * 6 + 5 + M * 12].Y := V1.Y + Normal.Y * SelOffset;
        Mdl.V[I * 6 + 5 + M * 12].Z := V1.Z + Normal.Z * SelOffset;
      End; // For I
      V1.Free;
      V2.Free;

      For I := 0 To J - 1 Do
      Begin
        Mdl.V[I * 3 + J * 6 + 0 + M * 12].X := Center.X - Normal.X * SelOffset;
        Mdl.V[I * 3 + J * 6 + 0 + M * 12].Y := Center.Y - Normal.Y * SelOffset;
        Mdl.V[I * 3 + J * 6 + 0 + M * 12].Z := Center.Z - Normal.Z * SelOffset;
        Mdl.V[I * 3 + J * 6 + 1 + M * 12]   := Mdl.V[I * 6 + 4 + M * 12];
        Mdl.V[I * 3 + J * 6 + 2 + M * 12]   := Mdl.V[I * 6 + 3 + M * 12];

        Mdl.V[I * 3 + J * 9 + 0 + M * 12].X := Center.X + Normal.X * SelOffset;
        Mdl.V[I * 3 + J * 9 + 0 + M * 12].Y := Center.Y + Normal.Y * SelOffset;
        Mdl.V[I * 3 + J * 9 + 0 + M * 12].Z := Center.Z + Normal.Z * SelOffset;
        Mdl.V[I * 3 + J * 9 + 1 + M * 12]   := Mdl.V[I * 6 + 1 + M * 12];
        Mdl.V[I * 3 + J * 9 + 2 + M * 12]   := Mdl.V[I * 6 + 0 + M * 12];
      End; // For I
      Normal.Free;
      Center.Free;
      Inc(M,J);
    End; // For L

    // Add the model to the GL viewer

    glView.AddModelToFrame(Mdl,EntityPolygonSelectionRectangle,False,False);
    Model.CalcExtents(False);
    SetLength(Mdl.V,0);
    SetLength(Mdl.P,0);
  End;
  glView.Scene3D.Scene.UnlockBSPTree;
End; // TfrmMain.SetPolygonSelectionRectangles

Procedure TfrmMain.AddModelOrigins;
Const Radius = 0.5;
Var
  Model    : TModel;
  Model1   : TModel;
  Mdl      : TModelRec;
  I,J,K,L  : Integer;
  MO       : TModelOrigin;
  NumPolys : Integer;
  AbsLoc   : T3DPoint;
  ZO       : TZoneObject;
  TN       : TTreeNode;

Begin
  glView.Scene3D.Scene.LockBSPTree('TfrmMain.AddModelOrigins');
  Model := glView.Scene3D.GetNewModel(EntityModelOrigin);
  Model.Clear;
  If cbShowModelOrigins.ItemIndex > 0 Then
  Begin
    Case cbShowModelOrigins.ItemIndex Of
      1: TEntity(glView.Scene3D.Scene.Entities.Items[EntityModelOrigin]).WireFrame := wfPoints;
      2: TEntity(glView.Scene3D.Scene.Entities.Items[EntityModelOrigin]).WireFrame := wfLines;
      3: TEntity(glView.Scene3D.Scene.Entities.Items[EntityModelOrigin]).WireFrame := wfPolygons;
      4: TEntity(glView.Scene3D.Scene.Entities.Items[EntityModelOrigin]).WireFrame := wfLinesAndPolygons;
    End; // Case
    Model1 := TModel.Create(Nil);
    Model1.GeneratePentakisDodecahedron;
    NumPolys := Model1.Positions.Length Div 9;
    Mdl.NP := 0;
    Mdl.NV := 0;
    For K := 0 To tvMain.SelectionCount - 1 Do
    Begin
      TN := tvMain.Selections[K];
      If TN <> Nil Then
      Begin
        ZO := TZoneObject(TN.Data);
        If (ZO <> Nil) And ((ZO Is TGroupObject) Or (ZO Is TModelOrigin)) Then
        Begin
          If ZO Is TGroupObject
           Then MO := TGroupObject(ZO).FindModelOrigin
           Else MO := TModelOrigin(ZO);
          If MO <> Nil Then Inc(Mdl.NP,NumPolys);
        End;
      End;
    End; // For K
    Mdl.NV := Mdl.NP * 3;
    SetLength(Mdl.P,Mdl.NP);
    SetLength(Mdl.V,Mdl.NV);
    For I := 0 To Mdl.NP - 1 Do Mdl.P[I].TextureID := -1;
    L := 0;
    For K := 0 To tvMain.SelectionCount - 1 Do
    Begin
      TN := tvMain.Selections[K];
      If TN <> Nil Then
      Begin
        ZO := TZoneObject(TN.Data);
        If (ZO <> Nil) And ((ZO Is TGroupObject) Or (ZO Is TModelOrigin)) Then
        Begin
          If ZO Is TGroupObject
           Then MO := TGroupObject(ZO).FindModelOrigin
           Else MO := TModelOrigin(ZO);
          If MO <> Nil Then
          Begin
            AbsLoc := MO.GetAbsoluteLocation;
            For I := 0 To NumPolys - 1 Do
            Begin
              For J := 0 To 2 Do
              Begin
                Mdl.V[I * 3 + J + L].X     := Model1.Positions.DataArray[(I * 3 + J) * 3 + 0] * Radius + AbsLoc.X;
                Mdl.V[I * 3 + J + L].Y     := Model1.Positions.DataArray[(I * 3 + J) * 3 + 1] * Radius + AbsLoc.Y;
                Mdl.V[I * 3 + J + L].Z     := Model1.Positions.DataArray[(I * 3 + J) * 3 + 2] * Radius + AbsLoc.Z;
                Mdl.V[I * 3 + J + L].Color := $80800000;
              End; // For J
            End; // For I
            Inc(L,NumPolys * 3);
            AbsLoc.Free;
          End;
        End;
      End;
    End; // For K

    // Add the model to the GL viewer

    glView.AddModelToFrame(Mdl,EntityModelOrigin,False,False);
    Model.CalcExtents(False);

    // Cleanup

    SetLength(Mdl.V,0);
    SetLength(Mdl.P,0);
    Model1.Free;
  End;
  glView.Scene3D.Scene.UnlockBSPTree;
End; // TfrmMain.AddModelOrigins

Procedure TfrmMain.AddHotSpots;
Const Radius = 0.5;
Var
  Model    : TModel;
  Model1   : TModel;
  Mdl      : TModelRec;
  I,J,K,L  : Integer;
  NumPolys : Integer;
  ZO       : TZoneObject;
  TN       : TTreeNode;

  Procedure TallyHotSpot(ZO: TZoneObject);
  Var I: Integer;
  Begin
    If (ZO <> Nil) And ((ZO Is TGroupObject) Or (ZO Is TMeshLibraryObjectReference) Or (ZO Is THotSpot)) Then
    Begin
      If ZO Is TGroupObject Then
      Begin
        For I := 0 To TGroupObject(ZO).Objects.Count - 1 Do TallyHotSpot(TZoneObject(TGroupObject(ZO).Objects.Objects[I]));
      End
      Else If ZO Is TMeshLibraryObjectReference Then TallyHotSpot(TMeshLibraryObjectReference(ZO).Group)
      Else Inc(Mdl.NP,NumPolys);
    End;
  End; // TallyHotSpot

  Procedure ProcessHotSpot(ZO,Parent: TZoneObject);
  Var
    I,J    : Integer;
    AbsLoc : T3DPoint;
    C      : TColor;

  Begin
    If (ZO <> Nil) And ((ZO Is TGroupObject) Or (ZO Is TMeshLibraryObjectReference) Or (ZO Is THotSpot)) Then
    Begin
      If ZO Is TGroupObject Then
      Begin
        For I := 0 To TGroupObject(ZO).Objects.Count - 1 Do ProcessHotSpot(TZoneObject(TGroupObject(ZO).Objects.Objects[I]),Parent);
      End
      Else If ZO Is TMeshLibraryObjectReference Then
      Begin
        ProcessHotSpot(TMeshLibraryObjectReference(ZO).Group,ZO);
      End
      Else
      Begin
        AbsLoc := ZO.GetAbsoluteLocation;
        If Parent <> Nil Then Parent.MakeAbsolute(AbsLoc);
        C      := $80000080;
        If ZO = SelectedHotSpot Then C := $800040C0;
        For I := 0 To NumPolys - 1 Do
        Begin
          For J := 0 To 2 Do
          Begin
            Mdl.V[I * 3 + J + L].X     := Model1.Positions.DataArray[(I * 3 + J) * 3 + 0] * Radius + AbsLoc.X;
            Mdl.V[I * 3 + J + L].Y     := Model1.Positions.DataArray[(I * 3 + J) * 3 + 1] * Radius + AbsLoc.Y;
            Mdl.V[I * 3 + J + L].Z     := Model1.Positions.DataArray[(I * 3 + J) * 3 + 2] * Radius + AbsLoc.Z;
            Mdl.V[I * 3 + J + L].Color := C;
          End; // For J
        End; // For I
        Inc(L,NumPolys * 3);
        AbsLoc.Free;
      End;
    End;
  End; // ProcessHotSpot

Begin
  glView.Scene3D.Scene.LockBSPTree('TfrmMain.AddHotSpots');
  Model := glView.Scene3D.GetNewModel(EntityHotSpot);
  Model.Clear;
  If cbShowHotSpots.ItemIndex > 0 Then
  Begin
    Case cbShowHotSpots.ItemIndex Of
      1: TEntity(glView.Scene3D.Scene.Entities.Items[EntityHotSpot]).WireFrame := wfPoints;
      2: TEntity(glView.Scene3D.Scene.Entities.Items[EntityHotSpot]).WireFrame := wfLines;
      3: TEntity(glView.Scene3D.Scene.Entities.Items[EntityHotSpot]).WireFrame := wfPolygons;
      4: TEntity(glView.Scene3D.Scene.Entities.Items[EntityHotSpot]).WireFrame := wfLinesAndPolygons;
    End; // Case
    Model1 := TModel.Create(Nil);
    Model1.GeneratePentakisDodecahedron;
    NumPolys := Model1.Positions.Length Div 9;
    Mdl.NP := 0;
    Mdl.NV := 0;
    For K := 0 To tvMain.SelectionCount - 1 Do
    Begin
      TN := tvMain.Selections[K];
      If TN <> Nil Then TallyHotSpot(TZoneObject(TN.Data));
    End; // For K
    Mdl.NV := Mdl.NP * 3;
    SetLength(Mdl.P,Mdl.NP);
    SetLength(Mdl.V,Mdl.NV);
    For I := 0 To Mdl.NP - 1 Do Mdl.P[I].TextureID := -1;
    L := 0;
    For K := 0 To tvMain.SelectionCount - 1 Do
    Begin
      TN := tvMain.Selections[K];
      If TN <> Nil Then ProcessHotSpot(TZoneObject(TN.Data),Nil);
    End; // For K

    // Add the model to the GL viewer

    glView.AddModelToFrame(Mdl,EntityHotSpot,False,False);
    Model.CalcExtents(False);

    // Cleanup

    SetLength(Mdl.V,0);
    SetLength(Mdl.P,0);
    Model1.Free;
  End;
  glView.Scene3D.Scene.UnlockBSPTree;
End; // TfrmMain.AddHotSpots

procedure TfrmMain.cbCrosshairChange(Sender: TObject);
begin
  Crosshair  := TCrossHairType(cbCrosshair.ItemIndex);
  CrosshairX := -1;
  CrosshairY := -1;
  RenderZone(BirdsEye);
end;

procedure TfrmMain.acRaiseLandExecute(Sender: TObject);
Var
  MO1,MO2 : TMeshObject;
  I,J     : Integer;
  V,V2    : T3DPoint;
  SX,SY   : Integer;
  D       : Single;
  X,Y     : Single;
  X0,Y0   : Integer;

begin
  Try
    EnableGUI(False);
    MO1 := Zone.FindMeshObject(meshHeightMapGround);
    MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);

    // Continue only if we found the mesh we're looking for

    If (MO1 <> Nil) Or (MO2 <> Nil) Then
    Begin
      If udRaiseLowerAmount.Position <> 0 Then
      Begin
        X  := Observer.X + GetHeightDist * Cos(Phi);
        Y  := Observer.Y + GetHeightDist * Sin(Phi);
        SX := GridSize;
        SY := GridSize;
        Y0 := Round((Zone.ElevationGrid.MaxX - X) / SX);
        X0 := Round((Zone.ElevationGrid.MaxY - Y) / SY);
        X  := Zone.ElevationGrid.MaxX - (Y0 * SX);
        Y  := Zone.ElevationGrid.MaxY - (X0 * SY);

        // Search the mesh for the nearest vertex in x-y space

        V := T3DPoint.Create;
        For I := 0 To MO1.Vertices.Count - 1 Do
        Begin
          V2 := T3DPoint(MO1.Vertices.Objects[I]);
          V.Copy(X,Y,V2.Z);
          V.Subtract(V2);
          If rbCircularArea.Checked Then
          Begin
            D := V.GetLength;
            If (Frac((Zone.ElevationGrid.MaxX - V2.X) / SX) = 0) And
               (Frac((Zone.ElevationGrid.MaxY - V2.X) / SY) = 0) And
               (D <= udRaiseLowerRadius.Position * SX * 1.01)    Then V2.Z := V2.Z + udRaiseLowerAmount.Position;
          End
          Else
          Begin
            If (Abs(V.X) <= udRaiseLowerRadius.Position) And
               (Abs(V.Y) <= udRaiseLowerRadius.Position) Then V2.Z := V2.Z + udRaiseLowerAmount.Position;
          End;
        End; // For I
        If MO2 <> Nil Then
        Begin
          For I := 0 To MO2.Vertices.Count - 1 Do
          Begin
            V2 := T3DPoint(MO2.Vertices.Objects[I]);
            V.Copy(X,Y,V2.Z);
            V.Subtract(V2);
            If rbCircularArea.Checked Then
            Begin
              D := V.GetLength;
              If (Frac((Zone.ElevationGrid.MaxX - V2.X) / SX) = 0) And
                 (Frac((Zone.ElevationGrid.MaxY - V2.X) / SY) = 0) And
                 (D <= udRaiseLowerRadius.Position * SX * 1.01)    Then V2.Z := V2.Z + udRaiseLowerAmount.Position;
            End
            Else
            Begin
              If (Abs(V.X) <= udRaiseLowerRadius.Position) And
                 (Abs(V.Y) <= udRaiseLowerRadius.Position) Then V2.Z := V2.Z + udRaiseLowerAmount.Position;
            End;
          End; // For I
        End;
        V.Free;

        // Set the elevation heights

        For I := 0 To Zone.ElevationGrid.NX - 1 Do
        Begin
          For J := 0 To Zone.ElevationGrid.NY - 1 Do
          Begin
            If rbCircularArea.Checked Then
            Begin
              If Sqrt(Sqr(I - X0) + Sqr(J - Y0)) <= udRaiseLowerRadius.Position Then
               Zone.ElevationGrid.SetHeight(I,J,Zone.ElevationGrid.GetHeight(I,J) + udRaiseLowerAmount.Position);
            End
            Else
            Begin
              If (Abs(I - X0) <= udRaiseLowerRadius.Position) And
                 (Abs(J - Y0) <= udRaiseLowerRadius.Position) Then
               Zone.ElevationGrid.SetHeight(I,J,Zone.ElevationGrid.GetHeight(I,J) + udRaiseLowerAmount.Position);
            End;
          End; // For J
        End; // For I

        RegenerateGroundMeshes;
        RegenerateMeshesWithGravity;
      End
      Else ShowMessage('Your raise/lower amount is set to zero.');
    End
    Else ShowMessage('Could not locate the ground mesh.');
  Finally
    SelectedPolygons.Clear;
    SelectedPolyMesh := Nil;
    EnableGUI(True);
  End;
end;

procedure TfrmMain.edtRaiseLowerRadiusChange(Sender: TObject);
begin
  CrosshairX := -1;
  CrosshairY := -1;
  RenderZone(BirdsEye);
end;

Procedure TfrmMain.FlattenGroundAtElevation(Elevation: Single);
Var
  MO1,MO2 : TMeshObject;
  I,J     : Integer;
  V       : T3DPoint;
  V2      : T3DPoint;
  X,Y     : Single;
  X0,Y0   : Integer;
  SX,SY   : Integer;
  D       : Single;

Begin
  MO1 := Zone.FindMeshObject(meshHeightMapGround);
  MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);

  // Continue only if we found the mesh we're looking for

  If (MO1 <> Nil) Or (MO2 <> Nil) Then
  Begin
    X  := Observer.X + GetHeightDist * Cos(Phi);
    Y  := Observer.Y + GetHeightDist * Sin(Phi);
    SX := GridSize;
    SY := GridSize;
    Y0 := Round((Zone.ElevationGrid.MaxX - X) / SX);
    X0 := Round((Zone.ElevationGrid.MaxY - Y) / SY);
    X  := Zone.ElevationGrid.MaxX - (Y0 * SX);
    Y  := Zone.ElevationGrid.MaxY - (X0 * SY);

    // Search the mesh for the nearest vertex in x-y space

    V := T3DPoint.Create;
    For I := 0 To MO1.Vertices.Count - 1 Do
    Begin
      V2 := T3DPoint(MO1.Vertices.Objects[I]);
      V.Copy(X,Y,V2.Z);
      V.Subtract(V2);
      If rbCircularArea.Checked Then
      Begin
        D := V.GetLength;
        If (Frac((Zone.ElevationGrid.MaxX - V2.X) / SX) = 0) And
           (Frac((Zone.ElevationGrid.MaxY - V2.X) / SY) = 0) And
           (D <= udRaiseLowerRadius.Position * SX * 1.01)    Then V2.Z := Elevation;
      End
      Else
      Begin
        If (Abs(V.X) <= udRaiseLowerRadius.Position) And
           (Abs(V.Y) <= udRaiseLowerRadius.Position) Then V2.Z := Elevation;
      End;
    End; // For I
    If MO2 <> Nil Then
    Begin
      For I := 0 To MO2.Vertices.Count - 1 Do
      Begin
        V2 := T3DPoint(MO2.Vertices.Objects[I]);
        V.Copy(X,Y,V2.Z);
        V.Subtract(V2);
        If rbCircularArea.Checked Then
        Begin
          D := V.GetLength;
          If (Frac((Zone.ElevationGrid.MaxX - V2.X) / SX) = 0) And
             (Frac((Zone.ElevationGrid.MaxY - V2.X) / SY) = 0) And
             (D <= udRaiseLowerRadius.Position * SX * 1.01)    Then V2.Z := Elevation;
        End
        Else
        Begin
          If (Abs(V.X) <= udRaiseLowerRadius.Position) And
             (Abs(V.Y) <= udRaiseLowerRadius.Position) Then V2.Z := Elevation;
        End;
      End; // For I
    End;
    V.Free;

    // Set the elevation heights

    For I := 0 To Zone.ElevationGrid.NX - 1 Do
    Begin
      For J := 0 To Zone.ElevationGrid.NY - 1 Do
      Begin
        If rbCircularArea.Checked Then
        Begin
          If Sqrt(Sqr(I - X0) + Sqr(J - Y0)) <= udRaiseLowerRadius.Position Then Zone.ElevationGrid.SetHeight(I,J,Elevation);
        End
        Else
        Begin
          If (Abs(I - X0) <= udRaiseLowerRadius.Position) And
             (Abs(J - Y0) <= udRaiseLowerRadius.Position) Then Zone.ElevationGrid.SetHeight(I,J,Elevation);
        End;
      End; // For J
    End; // For I

    RegenerateGroundMeshes;
    RegenerateMeshesWithGravity;
  End;
End; // TfrmMain.FlattenGroundAtElevation

procedure TfrmMain.acFlattenAtPeakExecute(Sender: TObject);
Var
  MO1,MO2 : TMeshObject;
  I,J     : Integer;
  SX,SY   : Integer;
  X,Y,Z   : Single;
  X0,Y0   : Integer;
  Z1      : Single;

begin
  Try
    EnableGUI(False);
    MO1 := Zone.FindMeshObject(meshHeightMapGround);
    MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);

    // Continue only if we found the mesh we're looking for

    If (MO1 <> Nil) Or (MO2 <> Nil) Then
    Begin
      X  := Observer.X + GetHeightDist * Cos(Phi);
      Y  := Observer.Y + GetHeightDist * Sin(Phi);
      SX := GridSize;
      SY := GridSize;
      Y0 := Round((Zone.ElevationGrid.MaxX - X) / SX);
      X0 := Round((Zone.ElevationGrid.MaxY - Y) / SY);

      // Calculate the height at which to set everything

      Z1 := -9999999;
      For I := 0 To Zone.ElevationGrid.NX - 1 Do
      Begin
        For J := 0 To Zone.ElevationGrid.NY - 1 Do
        Begin
          If rbCircularArea.Checked Then
          Begin
            If Sqrt(Sqr(I - X0) + Sqr(J - Y0)) <= udRaiseLowerRadius.Position Then
            Begin
              Z := Zone.ElevationGrid.GetHeight(I,J);
              If Z > Z1 Then Z1 := Z;
            End;
          End
          Else
          Begin
            If (Abs(I - X0) <= udRaiseLowerRadius.Position) And
               (Abs(J - Y0) <= udRaiseLowerRadius.Position) Then
            Begin
              Z := Zone.ElevationGrid.GetHeight(I,J);
              If Z > Z1 Then Z1 := Z;
            End;
          End;
        End; // For J
      End; // For I
      FlattenGroundAtElevation(Z1);
    End
    Else ShowMessage('Could not locate the ground mesh.');
  Finally
    SelectedPolygons.Clear;
    SelectedPolyMesh := Nil;
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acFlattenAtAverageExecute(Sender: TObject);
Var
  MO1,MO2 : TMeshObject;
  I,J,K   : Integer;
  SX,SY   : Integer;
  X,Y     : Single;
  X0,Y0   : Integer;
  Z1      : Single;

begin
  Try
    EnableGUI(False);
    MO1 := Zone.FindMeshObject(meshHeightMapGround);
    MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);

    // Continue only if we found the mesh we're looking for

    If (MO1 <> Nil) Or (MO2 <> Nil) Then
    Begin
      X  := Observer.X + GetHeightDist * Cos(Phi);
      Y  := Observer.Y + GetHeightDist * Sin(Phi);
      SX := GridSize;
      SY := GridSize;
      Y0 := Round((Zone.ElevationGrid.MaxX - X) / SX);
      X0 := Round((Zone.ElevationGrid.MaxY - Y) / SY);

      // Calculate the height at which to set everything

      Z1 := 0;
      K  := 0;
      For I := 0 To Zone.ElevationGrid.NX - 1 Do
      Begin
        For J := 0 To Zone.ElevationGrid.NY - 1 Do
        Begin
          If rbCircularArea.Checked Then
          Begin
            If Sqrt(Sqr(I - X0) + Sqr(J - Y0)) <= udRaiseLowerRadius.Position Then
            Begin
              Z1 := Z1 + Zone.ElevationGrid.GetHeight(I,J);
              Inc(K);
            End;
          End
          Else
          Begin
            If (Abs(I - X0) <= udRaiseLowerRadius.Position) And
               (Abs(J - Y0) <= udRaiseLowerRadius.Position) Then
            Begin
              Z1 := Z1 + Zone.ElevationGrid.GetHeight(I,J);
              Inc(K);
            End;
          End;
        End; // For J
      End; // For I
      If K > 0 Then Z1 := Z1 / K;
      FlattenGroundAtElevation(Z1);
    End
    Else ShowMessage('Could not locate the ground mesh.');
  Finally
    SelectedPolygons.Clear;
    SelectedPolyMesh := Nil;
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acFlattenAtLowestExecute(Sender: TObject);
Var
  MO1,MO2 : TMeshObject;
  I,J     : Integer;
  SX,SY   : Integer;
  X,Y,Z   : Single;
  X0,Y0   : Integer;
  Z1      : Single;

begin
  Try
    EnableGUI(False);
    MO1 := Zone.FindMeshObject(meshHeightMapGround);
    MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);

    // Continue only if we found the mesh we're looking for

    If (MO1 <> Nil) Or (MO2 <> Nil) Then
    Begin
      X  := Observer.X + GetHeightDist * Cos(Phi);
      Y  := Observer.Y + GetHeightDist * Sin(Phi);
      SX := GridSize;
      SY := GridSize;
      Y0 := Round((Zone.ElevationGrid.MaxX - X) / SX);
      X0 := Round((Zone.ElevationGrid.MaxY - Y) / SY);

      // Calculate the height at which to set everything

      Z1 := 9999999;
      For I := 0 To Zone.ElevationGrid.NX - 1 Do
      Begin
        For J := 0 To Zone.ElevationGrid.NY - 1 Do
        Begin
          If rbCircularArea.Checked Then
          Begin
            If Sqrt(Sqr(I - X0) + Sqr(J - Y0)) <= udRaiseLowerRadius.Position Then
            Begin
              Z := Zone.ElevationGrid.GetHeight(I,J);
              If Z < Z1 Then Z1 := Z;
            End;
          End
          Else
          Begin
            If (Abs(I - X0) <= udRaiseLowerRadius.Position) And
               (Abs(J - Y0) <= udRaiseLowerRadius.Position) Then
            Begin
              Z := Zone.ElevationGrid.GetHeight(I,J);
              If Z < Z1 Then Z1 := Z;
            End;
          End;
        End; // For J
      End; // For I
      FlattenGroundAtElevation(Z1);
    End
    Else ShowMessage('Could not locate the ground mesh.');
  Finally
    SelectedPolygons.Clear;
    SelectedPolyMesh := Nil;
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acRumpleGroundExecute(Sender: TObject);
Var
  I,J      : Integer;
  Changed1 : Array Of Boolean;
  Changed2 : Array Of Boolean;
  Adjust   : Single;
  H        : Single;

begin
  Try
    EnableGUI(False);
    If udRaiseLowerAmount.Position <> 0 Then
    Begin
      For I := 0 To Zone.ElevationGrid.NX - 1 Do
      Begin
        For J := 0 To Zone.ElevationGrid.NY - 1 Do
        Begin
          Adjust := (2 * Random * udRaiseLowerAmount.Position) - udRaiseLowerAmount.Position;
          H      := Zone.ElevationGrid.GetHeight(I,J);
          Zone.ElevationGrid.SetHeight(I,J,H + Adjust);
        End; // For J
      End; // For I
      SetLength(Changed1,0);
      SetLength(Changed2,0);

      RegenerateGroundMeshes;
      RegenerateMeshesWithGravity;
    End
    Else ShowMessage('Your raise/lower amount is set to zero.');
  Finally
    SelectedPolygons.Clear;
    SelectedPolyMesh := Nil;
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acMountainizeExecute(Sender: TObject);
Const MountainHeight = 150;
Var
  X,Y         : Integer;
  Average     : Single;
  Noise       : Single;
  SX,SY       : Integer;
  RaiseHeight : Single;
  I           : Integer;

  Procedure ShiftTags(XInc,YInc: Integer; ShiftSouthOrEast: Boolean);
  Var MO1,MO2: TMeshObject;

    Procedure Shift(MO: TMeshObject);
    Var
      I     : Integer;
      P     : TPolygon;
      X,Y   : Integer;
      XI,YI : Integer;
    Begin
      For I := 0 To MO.Polygons.Count - 1 Do
      Begin
        P := TPolygon(MO.Polygons.Objects[I]);
        Y := P.Tag Div (Zone.ElevationGrid.NX - 1);
        X := P.Tag - Y * (Zone.ElevationGrid.NX - 1);
        If ShiftSouthOrEast Then
        Begin
          XI := XInc;
          YI := YInc;
        End
        Else
        Begin
          XI := 0;
          YI := 0;
        End;
        P.Tag := (Y + YI) * (Zone.ElevationGrid.NX + XInc - 1) + X + XI;
      End; // For I
    End; // Shift

  Begin
    MO1 := Zone.FindMeshObject(meshHeightMapGround);
    MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);
    If MO1 <> Nil Then Shift(MO1);
    If MO2 <> Nil Then Shift(MO2);
  End; // ShiftTags

begin
  Try
    EnableGUI(False);
    frmMountainize.edtAmount.Text := IntToStr(MountainHeight);
    If frmMountainize.ShowModal = mrOk Then
    Begin
      Val(Trim(frmMountainize.edtAmount.Text),RaiseHeight,I);
      If (Height > 0) And (I = 0) Then
      Begin
        If frmMountainize.cbNorth.Checked Or
           frmMountainize.cbSouth.Checked Or
           frmMountainize.cbEast.Checked  Or
           frmMountainize.cbWest.Checked  Then
        Begin
          If (Zone.ElevationGrid.NX > 16) And
             (Zone.ElevationGrid.NY > 16) Then
          Begin
            SX := GridSize;
            SY := GridSize;

            // Determine the average height of the center area

            Average := 0;
            For X := 7 To Zone.ElevationGrid.NX - 8 Do
            Begin
              For Y := 7 To Zone.ElevationGrid.NY - 8 Do Average := Average + Zone.ElevationGrid.GetHeight(X,Y);
            End; // For X
            Average := Average / ((Zone.ElevationGrid.NX - 14) * (Zone.ElevationGrid.NY - 14));

            // Grow the zone if we have to

            If frmMountainize.cbGrow.Checked Then
            Begin
              If frmMountainize.cbNorth.Checked Then
              Begin
                SetLength(Zone.ElevationGrid.Heights,Zone.ElevationGrid.NX * (Zone.ElevationGrid.NY + 6));
                SetLength(Zone.ElevationGrid.Visible,(Zone.ElevationGrid.NX - 1) * (Zone.ElevationGrid.NY + 5));
                For Y := Zone.ElevationGrid.NY - 1 DownTo 0 Do
                Begin
                  For X := Zone.ElevationGrid.NX - 1 DownTo 0 Do
                   Zone.ElevationGrid.Heights[(Y + 6) * Zone.ElevationGrid.NX + X] := Zone.ElevationGrid.Heights[Y * Zone.ElevationGrid.NX + X];
                End; // For Y
                For Y := Zone.ElevationGrid.NY - 2 DownTo 0 Do
                Begin
                  For X := Zone.ElevationGrid.NX - 2 DownTo 0 Do
                   Zone.ElevationGrid.Visible[(Y + 6) * (Zone.ElevationGrid.NX - 1) + X] := Zone.ElevationGrid.Visible[Y * (Zone.ElevationGrid.NX - 1) + X];
                End; // For Y
                ShiftTags(0,6,True);
                Inc(Zone.ElevationGrid.NY,6);
                Zone.ElevationGrid.MaxX := Zone.ElevationGrid.MaxX + 6 * SY;
                For Y := 0 To 5 Do
                Begin
                  For X := 0 To Zone.ElevationGrid.NX - 1 Do Zone.ElevationGrid.SetHeight(X,Y,Average);
                  For X := 0 To Zone.ElevationGrid.NX - 2 Do Zone.ElevationGrid.SetVisible(X,Y,True);
                End; // For Y
              End;

              If frmMountainize.cbSouth.Checked Then
              Begin
                SetLength(Zone.ElevationGrid.Heights,Zone.ElevationGrid.NX * (Zone.ElevationGrid.NY + 6));
                SetLength(Zone.ElevationGrid.Visible,(Zone.ElevationGrid.NX - 1) * (Zone.ElevationGrid.NY + 5));
                ShiftTags(0,6,False);
                Inc(Zone.ElevationGrid.NY,6);
                Zone.ElevationGrid.MinX := Zone.ElevationGrid.MinX - 6 * SY;
                For Y := Zone.ElevationGrid.NY - 6 To Zone.ElevationGrid.NY - 1 Do
                Begin
                  For X := 0 To Zone.ElevationGrid.NX - 1 Do Zone.ElevationGrid.SetHeight(X,Y,Average);
                  For X := 0 To Zone.ElevationGrid.NX - 2 Do Zone.ElevationGrid.SetVisible(X,Y - 1,True);
                End; // For Y
              End;

              If frmMountainize.cbWest.Checked Then
              Begin
                SetLength(Zone.ElevationGrid.Heights,(Zone.ElevationGrid.NX + 6) * Zone.ElevationGrid.NY);
                SetLength(Zone.ElevationGrid.Visible,(Zone.ElevationGrid.NX + 5) * (Zone.ElevationGrid.NY - 1));
                For Y := Zone.ElevationGrid.NY - 1 DownTo 0 Do
                Begin
                  For X := Zone.ElevationGrid.NX - 1 DownTo 0 Do Zone.ElevationGrid.Heights[Y * (Zone.ElevationGrid.NX + 6) + X + 6] := Zone.ElevationGrid.Heights[Y * Zone.ElevationGrid.NX + X];
                End; // For Y
                For Y := Zone.ElevationGrid.NY - 2 DownTo 0 Do
                Begin
                  For X := Zone.ElevationGrid.NX - 2 DownTo 0 Do Zone.ElevationGrid.Visible[Y * (Zone.ElevationGrid.NX + 5) + X + 6] := Zone.ElevationGrid.Visible[Y * (Zone.ElevationGrid.NX - 1) + X];
                End; // For Y
                ShiftTags(6,0,True);
                Inc(Zone.ElevationGrid.NX,6);
                Zone.ElevationGrid.MaxY := Zone.ElevationGrid.MaxY + 6 * SX;
                For Y := 0 To Zone.ElevationGrid.NY - 1 Do
                Begin
                  For X := 0 To 5 Do Zone.ElevationGrid.SetHeight(X,Y,Average);
                End; // For Y
                For Y := 0 To Zone.ElevationGrid.NY - 2 Do
                Begin
                  For X := 0 To 5 Do Zone.ElevationGrid.SetVisible(X,Y,True);
                End; // For Y
              End;

              If frmMountainize.cbEast.Checked Then
              Begin
                SetLength(Zone.ElevationGrid.Heights,(Zone.ElevationGrid.NX + 6) * Zone.ElevationGrid.NY);
                SetLength(Zone.ElevationGrid.Visible,(Zone.ElevationGrid.NX + 5) * (Zone.ElevationGrid.NY - 1));
                For Y := Zone.ElevationGrid.NY - 1 DownTo 0 Do
                Begin
                  For X := Zone.ElevationGrid.NX - 1 DownTo 0 Do
                   Zone.ElevationGrid.Heights[Y * (Zone.ElevationGrid.NX + 6) + X] := Zone.ElevationGrid.Heights[Y * Zone.ElevationGrid.NX + X];
                End; // For Y
                For Y := Zone.ElevationGrid.NY - 2 DownTo 0 Do
                Begin
                  For X := Zone.ElevationGrid.NX - 2 DownTo 0 Do
                   Zone.ElevationGrid.Visible[Y * (Zone.ElevationGrid.NX + 5) + X] := Zone.ElevationGrid.Visible[Y * (Zone.ElevationGrid.NX - 1) + X];
                End; // For Y
                ShiftTags(6,0,False);
                Inc(Zone.ElevationGrid.NX,6);
                Zone.ElevationGrid.MinY := Zone.ElevationGrid.MinY - 6 * SX;
                For Y := 0 To Zone.ElevationGrid.NY - 1 Do
                Begin
                  For X := Zone.ElevationGrid.NX - 6 To Zone.ElevationGrid.NX - 1 Do Zone.ElevationGrid.SetHeight(X,Y,Average);
                End; // For Y
                For Y := 0 To Zone.ElevationGrid.NY - 2 Do
                Begin
                  For X := Zone.ElevationGrid.NX - 7 To Zone.ElevationGrid.NX - 2 Do Zone.ElevationGrid.SetVisible(X,Y,True);
                End; // For Y
              End;
            End;

            // Raise the edges to the average height plus a fixed amount

            If frmMountainize.cbNorth.Checked Then
            Begin
              For X := 0 To Zone.ElevationGrid.NX - 1 Do
              Begin
                For Y := 0 To 6 Do
                Begin
                  Noise := Random * 40 - 20;
                  Zone.ElevationGrid.SetHeight(X,Y,Average + Noise + RaiseHeight);
                End; // For Y
              End; // For X
              Zone.AddBound(Zone.ElevationGrid.MaxX - 6.5 * SY,Zone.ElevationGrid.MinY,0,Zone.ElevationGrid.MaxX - 6.5 * SY,Zone.ElevationGrid.MaxY,0);
            End;

            If frmMountainize.cbSouth.Checked Then
            Begin
              For X := 0 To Zone.ElevationGrid.NX - 1 Do
              Begin
                For Y := Zone.ElevationGrid.NY - 7 To Zone.ElevationGrid.NY - 1 Do
                Begin
                  Noise := Random * 40 - 20;
                  Zone.ElevationGrid.SetHeight(X,Y,Average + Noise + RaiseHeight);
                End; // For Y
              End; // For X
              Zone.AddBound(Zone.ElevationGrid.MinX + 6.5 * SY,Zone.ElevationGrid.MaxY,0,Zone.ElevationGrid.MinX + 6.5 * SY,Zone.ElevationGrid.MinY,0);
            End;

            If frmMountainize.cbEast.Checked Then
            Begin
              For X := Zone.ElevationGrid.NX - 7 To Zone.ElevationGrid.NX - 1 Do
              Begin
                For Y := 0 To Zone.ElevationGrid.NY - 1 Do
                Begin
                  Noise := Random * 40 - 20;
                  Zone.ElevationGrid.SetHeight(X,Y,Average + Noise + RaiseHeight);
                End; // For Y
              End; // For X
              Zone.AddBound(Zone.ElevationGrid.MinX,Zone.ElevationGrid.MinY + 6.5 * SY,0,Zone.ElevationGrid.MaxX,Zone.ElevationGrid.MinY + 6.5 * SY,0);
            End;

            If frmMountainize.cbWest.Checked Then
            Begin
              For X := 0 To 6 Do
              Begin
                For Y := 0 To Zone.ElevationGrid.NY - 1 Do
                Begin
                  Noise := Random * 40 - 20;
                  Zone.ElevationGrid.SetHeight(X,Y,Average + Noise + RaiseHeight);
                End; // For Y
              End; // For X
              Zone.AddBound(Zone.ElevationGrid.MaxX,Zone.ElevationGrid.MaxY - 6.5 * SY,0,Zone.ElevationGrid.MinX,Zone.ElevationGrid.MaxY - 6.5 * SY,0);
            End;

            If frmMountainize.cbNorth.Checked Or
               frmMountainize.cbSouth.Checked Or
               frmMountainize.cbEast.Checked  Or
               frmMountainize.cbWest.Checked  Then Zone.BreakUpBounds;

            RegenerateGroundMeshes;
            Zone.BuildBoundingPolygons;
            ReloadZone;
            RenderZone(BirdsEye);
            If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
          End
          Else ShowMessage('The zone is too small to mountainize.');
        End
        Else ShowMessage('You must select an edge to mountainize.');
      End
      Else ShowMessage('You must enter a valid amount by which to raise the zone edges.');
    End;
  Finally
    SelectedPolygons.Clear;
    SelectedPolyMesh := Nil;
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acAddZoneExitExecute(Sender: TObject);
Var
  I       : Integer;
  X,Y     : Integer;
  Average : Single;
  Angle   : Integer;
  ZO      : TZoneObject;
  MO1,MO2 : TMeshObject;
  SX,SY   : Integer;
  X0,Y0   : Integer;
  RX,RY   : Single;
  GoAhead : Boolean;
  MaxX    : Single;
  MaxY    : Single;

begin
  If frmAddZoneExit.ShowModal = mrOk Then
  Begin
    Try
      EnableGUI(False);
      MO1 := Zone.FindMeshObject(meshHeightMapGround);
      MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);
      If (MO1 <> Nil) Or (MO2 <> Nil) Then
      Begin
        If (Zone.ElevationGrid.NX > 16) And
           (Zone.ElevationGrid.NY > 16) Then
        Begin
          // Flatten the land

          Angle := Round(Phi * 180 / Pi);
          While Angle <    0 Do Inc(Angle,360);
          While Angle >= 360 Do Dec(Angle,360);

          RX := Observer.X + GetHeightDist * Cos(Phi);
          RY := Observer.Y + GetHeightDist * Sin(Phi);
          SX := GridSize;
          SY := GridSize;
          Y0 := Round((Zone.ElevationGrid.MaxX - RX) / SX);
          X0 := Round((Zone.ElevationGrid.MaxY - RY) / SY);

          // Determine the height at the red crosshair

          Average := Zone.ElevationGrid.GetHeight(X0,Y0);

          GoAhead := False;
          MaxX    := Zone.ElevationGrid.MaxX;
          MaxY    := Zone.ElevationGrid.MaxY;
          If frmAddZoneExit.rbLeft.Checked Then
          Begin
            Case Angle Of
              0..45,316..359: // North
              Begin
                If (X0 >= 4) And (Y0 = 7) And (X0 <= Zone.ElevationGrid.NX - 3) Then GoAhead := True
                Else
                Begin
                  GoAhead := (Application.MessageBox('This is not a mountainized zone edge.'#13#10 + 'Continue anyway?',
                                                     'Warning', MB_OKCANCEL) = IDOK);
                End;
                If GoAhead Then
                Begin
                  Zone.ElevationGrid.SetHeight(X0,    Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 - 5,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 6,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 - 6,Average);

                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 - 4,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 - 4,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 - 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 - 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 - 6,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 - 6,Average);

                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 - 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 - 6,Average);

                  // Build bounding lines for the zone exit, accounting for the fact that ground polygons are split into triangles
                  // from northwest to southeast

                  Zone.AddBound(MaxX - (Y0 - 0.5) * SY,MaxY - (X0 + 2.0) * SX,0,MaxX - (Y0 - 1.0) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.0) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 - 3.0) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 3.0) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 - 3.5) * SY,MaxY - (X0 + 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 3.5) * SY,MaxY - (X0 + 1.0) * SX,0,MaxX - (Y0 - 3.5) * SY,MaxY - (X0 - 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 3.5) * SY,MaxY - (X0 - 1.0) * SX,0,MaxX - (Y0 - 4.0) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 4.0) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 - 4.5) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 4.5) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 - 4.5) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 4.5) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 - 6.0) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 6.0) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 - 6.5) * SY,MaxY - (X0 + 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 6.5) * SY,MaxY - (X0 + 1.0) * SX,0,MaxX - (Y0 - 6.5) * SY,MaxY - (X0 - 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 6.5) * SY,MaxY - (X0 - 3.5) * SX,0,MaxX - (Y0 - 2.0) * SY,MaxY - (X0 - 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 2.0) * SY,MaxY - (X0 - 3.5) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 3.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 3.0) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 1.0) * SX,0,MaxX - (Y0 - 1.0) * SY,MaxY - (X0 - 0.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.0) * SY,MaxY - (X0 - 0.5) * SX,0,MaxX - (Y0 - 0.5) * SY,MaxY - (X0 - 0.5) * SX,0);

                  Zone.BreakUpBounds;
                End;
              End;
              46..135: // West
              Begin
                If (Y0 >= 2) And (X0 = 7) And (Y0 <= Zone.ElevationGrid.NY - 5) Then GoAhead := True
                Else
                Begin
                  GoAhead := (Application.MessageBox('This is not a mountainized zone edge.'#13#10 + 'Continue anyway?',
                                                     'Warning', MB_OKCANCEL) = IDOK);
                End;
                If GoAhead Then
                Begin
                  Zone.ElevationGrid.SetHeight(X0,    Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 5,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 5,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 6,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 6,Y0 - 1,Average);

                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 4,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 4,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 5,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 5,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 6,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 6,Y0 + 2,Average);

                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 5,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 6,Y0 + 1,Average);

                  // Build bounding lines for the zone exit, accounting for the fact that ground polygons are split into triangles
                  // from northwest to southeast

                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 0.5) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 3.5) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 3.5) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 4.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 4.0) * SX,0,MaxX - (Y0 + 1.0) * SY,MaxY - (X0 - 4.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.0) * SY,MaxY - (X0 - 4.5) * SX,0,MaxX - (Y0 - 1.0) * SY,MaxY - (X0 - 4.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.0) * SY,MaxY - (X0 - 4.5) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 5.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 5.0) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 6.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 6.5) * SX,0,MaxX - (Y0 + 3.0) * SY,MaxY - (X0 - 6.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 3.0) * SY,MaxY - (X0 - 6.5) * SX,0,MaxX - (Y0 + 3.5) * SY,MaxY - (X0 - 6.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 3.5) * SY,MaxY - (X0 - 6.0) * SX,0,MaxX - (Y0 + 3.5) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 3.5) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 + 0.5) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 0.5) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 + 0.5) * SY,MaxY - (X0 - 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 0.5) * SY,MaxY - (X0 - 1.0) * SX,0,MaxX - (Y0 + 1.0) * SY,MaxY - (X0 - 0.5) * SX,0);

                  Zone.BreakUpBounds;
                End;
              End;
              136..225: // South
              Begin
                If (X0 >= 2) And (Y0 = Zone.ElevationGrid.NY - 8) And (X0 <= Zone.ElevationGrid.NX - 5) Then GoAhead := True
                Else
                Begin
                  GoAhead := (Application.MessageBox('This is not a mountainized zone edge.'#13#10 + 'Continue anyway?',
                                                     'Warning', MB_OKCANCEL) = IDOK);
                End;
                If GoAhead Then
                Begin
                  Zone.ElevationGrid.SetHeight(X0,    Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 + 5,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 6,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 + 6,Average);

                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 + 4,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 + 4,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 + 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 + 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 + 6,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 + 6,Average);

                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 + 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 + 6,Average);

                  // Build bounding lines for the zone exit, accounting for the fact that ground polygons are split into triangles
                  // from northwest to southeast

                  Zone.AddBound(MaxX - (Y0 + 0.5) * SY,MaxY - (X0 - 2.0) * SX,0,MaxX - (Y0 + 1.0) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.0) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 + 3.0) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 3.0) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 + 3.5) * SY,MaxY - (X0 - 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 3.5) * SY,MaxY - (X0 - 1.0) * SX,0,MaxX - (Y0 + 3.5) * SY,MaxY - (X0 + 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 3.5) * SY,MaxY - (X0 + 1.0) * SX,0,MaxX - (Y0 + 4.0) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 4.0) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 + 4.5) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 4.5) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 + 4.5) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 4.5) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 + 6.0) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 6.0) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 + 6.5) * SY,MaxY - (X0 - 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 6.5) * SY,MaxY - (X0 - 1.0) * SX,0,MaxX - (Y0 + 6.5) * SY,MaxY - (X0 + 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 6.5) * SY,MaxY - (X0 + 3.5) * SX,0,MaxX - (Y0 + 2.0) * SY,MaxY - (X0 + 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 2.0) * SY,MaxY - (X0 + 3.5) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 3.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 3.0) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 1.0) * SX,0,MaxX - (Y0 + 1.0) * SY,MaxY - (X0 + 0.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.0) * SY,MaxY - (X0 + 0.5) * SX,0,MaxX - (Y0 + 0.5) * SY,MaxY - (X0 + 0.5) * SX,0);

                  Zone.BreakUpBounds;
                End;
              End;
              226..314: // East
              Begin
                If (Y0 >= 4) And (X0 = Zone.ElevationGrid.NX - 8) And (Y0 <= Zone.ElevationGrid.NY - 3) Then GoAhead := True
                Else
                Begin
                  GoAhead := (Application.MessageBox('This is not a mountainized zone edge.'#13#10 + 'Continue anyway?',
                                                     'Warning', MB_OKCANCEL) = IDOK);
                End;
                If GoAhead Then
                Begin
                  Zone.ElevationGrid.SetHeight(X0,    Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 5,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 5,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 6,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 6,Y0 + 1,Average);

                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 4,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 4,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 5,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 5,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 6,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 6,Y0 - 2,Average);

                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 5,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 6,Y0 - 1,Average);

                  // Build bounding lines for the zone exit, accounting for the fact that ground polygons are split into triangles
                  // from northwest to southeast

                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 0.5) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 3.5) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 3.5) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 4.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 4.0) * SX,0,MaxX - (Y0 - 1.0) * SY,MaxY - (X0 + 4.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.0) * SY,MaxY - (X0 + 4.5) * SX,0,MaxX - (Y0 + 1.0) * SY,MaxY - (X0 + 4.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.0) * SY,MaxY - (X0 + 4.5) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 5.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 5.0) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 6.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 6.5) * SX,0,MaxX - (Y0 - 3.0) * SY,MaxY - (X0 + 6.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 3.0) * SY,MaxY - (X0 + 6.5) * SX,0,MaxX - (Y0 - 3.5) * SY,MaxY - (X0 + 6.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 3.5) * SY,MaxY - (X0 + 6.0) * SX,0,MaxX - (Y0 - 3.5) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 3.5) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 - 0.5) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 0.5) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 - 0.5) * SY,MaxY - (X0 + 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 0.5) * SY,MaxY - (X0 + 1.0) * SX,0,MaxX - (Y0 - 1.0) * SY,MaxY - (X0 + 0.5) * SX,0);

                  Zone.BreakUpBounds;
                End;
              End;
            End; // Case
          End
          Else
          Begin
            Case Angle Of
              0..45,316..359: // North
              Begin
                If (X0 >= 2) And (Y0 = 7) And (X0 <= Zone.ElevationGrid.NX - 5) Then GoAhead := True
                Else
                Begin
                  GoAhead := (Application.MessageBox('This is not a mountainized zone edge.'#13#10 + 'Continue anyway?',
                                                     'Warning', MB_OKCANCEL) = IDOK);
                End;
                If GoAhead Then
                Begin
                  Zone.ElevationGrid.SetHeight(X0,    Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 - 5,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 6,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 - 6,Average);

                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 - 4,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 - 4,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 - 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 - 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 - 6,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 - 6,Average);

                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 - 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 - 6,Average);

                  // Build bounding lines for the zone exit, accounting for the fact that ground polygons are split into triangles
                  // from northwest to southeast

                  Zone.AddBound(MaxX - (Y0 - 3.5) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 - 0.5) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 3.5) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 - 3.5) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 4.0) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 - 3.5) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 4.5) * SY,MaxY - (X0 + 1.0) * SX,0,MaxX - (Y0 - 4.0) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 4.5) * SY,MaxY - (X0 - 1.0) * SX,0,MaxX - (Y0 - 4.5) * SY,MaxY - (X0 + 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 5.0) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 - 4.5) * SY,MaxY - (X0 - 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 6.5) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 - 5.0) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 6.5) * SY,MaxY - (X0 + 3.0) * SX,0,MaxX - (Y0 - 6.5) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 6.0) * SY,MaxY - (X0 + 3.5) * SX,0,MaxX - (Y0 - 6.5) * SY,MaxY - (X0 + 3.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 3.5) * SX,0,MaxX - (Y0 - 6.0) * SY,MaxY - (X0 + 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 0.5) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.0) * SY,MaxY - (X0 + 0.5) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 0.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 0.5) * SY,MaxY - (X0 + 1.0) * SX,0,MaxX - (Y0 - 1.0) * SY,MaxY - (X0 + 0.5) * SX,0);

                  Zone.BreakUpBounds;
                End;
              End;
              46..135: // West
              Begin
                If (Y0 >= 4) And (X0 = 7) And (Y0 <= Zone.ElevationGrid.NY - 3) Then GoAhead := True
                Else
                Begin
                  GoAhead := (Application.MessageBox('This is not a mountainized zone edge.'#13#10 + 'Continue anyway?',
                                                     'Warning', MB_OKCANCEL) = IDOK);
                End;
                If GoAhead Then
                Begin
                  Zone.ElevationGrid.SetHeight(X0,    Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 5,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 5,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 6,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 - 6,Y0 + 1,Average);

                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 4,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 4,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 5,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 5,Y0 - 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 6,Y0 - 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 6,Y0 - 2,Average);

                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 5,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 6,Y0 - 1,Average);

                  // Build bounding lines for the zone exit, accounting for the fact that ground polygons are split into triangles
                  // from northwest to southeast

                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 1.0) * SX,0,MaxX - (Y0 + 2.0) * SY,MaxY - (X0 - 0.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 3.0) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.0) * SY,MaxY - (X0 - 3.5) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 3.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.0) * SY,MaxY - (X0 - 3.5) * SX,0,MaxX - (Y0 + 1.0) * SY,MaxY - (X0 - 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 4.0) * SX,0,MaxX - (Y0 - 1.0) * SY,MaxY - (X0 - 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 4.5) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 4.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 4.5) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 - 4.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 6.0) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 4.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.0) * SY,MaxY - (X0 - 6.5) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 6.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 3.5) * SY,MaxY - (X0 - 6.5) * SX,0,MaxX - (Y0 + 1.0) * SY,MaxY - (X0 - 6.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 3.5) * SY,MaxY - (X0 - 2.0) * SX,0,MaxX - (Y0 - 3.5) * SY,MaxY - (X0 - 6.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 3.0) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 - 3.5) * SY,MaxY - (X0 - 2.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.0) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 - 3.0) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 0.5) * SY,MaxY - (X0 - 1.0) * SX,0,MaxX - (Y0 - 1.0) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 0.5) * SY,MaxY - (X0 - 0.5) * SX,0,MaxX - (Y0 - 0.5) * SY,MaxY - (X0 - 1.0) * SX,0);

                  Zone.BreakUpBounds;
                End;
              End;
              136..225: // South
              Begin
                If (X0 >= 4) And (Y0 = Zone.ElevationGrid.NY - 8) And (X0 <= Zone.ElevationGrid.NX - 3) Then GoAhead := True
                Else
                Begin
                  GoAhead := (Application.MessageBox('This is not a mountainized zone edge.'#13#10 + 'Continue anyway?',
                                                     'Warning', MB_OKCANCEL) = IDOK);
                End;
                If GoAhead Then
                Begin
                  Zone.ElevationGrid.SetHeight(X0,    Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 + 5,Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 + 6,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 + 6,Average);

                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 + 4,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 + 4,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 + 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 + 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 3,Y0 + 6,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 2,Y0 + 6,Average);

                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 + 5,Average);
                  Zone.ElevationGrid.SetHeight(X0 - 1,Y0 + 6,Average);

                  // Build bounding lines for the zone exit, accounting for the fact that ground polygons are split into triangles
                  // from northwest to southeast

                  Zone.AddBound(MaxX - (Y0 + 3.5) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 + 0.5) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 3.5) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 + 3.5) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 4.0) * SY,MaxY - (X0 - 1.5) * SX,0,MaxX - (Y0 + 3.5) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 4.5) * SY,MaxY - (X0 - 1.0) * SX,0,MaxX - (Y0 + 4.0) * SY,MaxY - (X0 - 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 4.5) * SY,MaxY - (X0 + 1.0) * SX,0,MaxX - (Y0 + 4.5) * SY,MaxY - (X0 - 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 5.0) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 + 4.5) * SY,MaxY - (X0 + 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 6.5) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 + 5.0) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 6.5) * SY,MaxY - (X0 - 3.0) * SX,0,MaxX - (Y0 + 6.5) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 6.0) * SY,MaxY - (X0 - 3.5) * SX,0,MaxX - (Y0 + 6.5) * SY,MaxY - (X0 - 3.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 3.5) * SX,0,MaxX - (Y0 + 6.0) * SY,MaxY - (X0 - 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 0.5) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.0) * SY,MaxY - (X0 - 0.5) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 - 0.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 0.5) * SY,MaxY - (X0 - 1.0) * SX,0,MaxX - (Y0 + 1.0) * SY,MaxY - (X0 - 0.5) * SX,0);

                  Zone.BreakUpBounds;
                End;
              End;
              226..314: // East
              Begin
                If (Y0 >= 2) And (X0 = Zone.ElevationGrid.NX - 8) And (Y0 <= Zone.ElevationGrid.NY - 5) Then GoAhead := True
                Else
                Begin
                  GoAhead := (Application.MessageBox('This is not a mountainized zone edge.'#13#10 + 'Continue anyway?',
                                                     'Warning', MB_OKCANCEL) = IDOK);
                End;
                If GoAhead Then
                Begin
                  Zone.ElevationGrid.SetHeight(X0,    Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0,    Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 1,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 5,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 5,Y0 - 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 6,Y0,    Average);
                  Zone.ElevationGrid.SetHeight(X0 + 6,Y0 - 1,Average);

                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 4,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 4,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 5,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 5,Y0 + 2,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 6,Y0 + 3,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 6,Y0 + 2,Average);

                  Zone.ElevationGrid.SetHeight(X0 + 2,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 3,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 5,Y0 + 1,Average);
                  Zone.ElevationGrid.SetHeight(X0 + 6,Y0 + 1,Average);

                  // Build bounding lines for the zone exit, accounting for the fact that ground polygons are split into triangles
                  // from northwest to southeast

                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 1.0) * SX,0,MaxX - (Y0 - 2.0) * SY,MaxY - (X0 + 0.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 3.0) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 1.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.0) * SY,MaxY - (X0 + 3.5) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 3.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.0) * SY,MaxY - (X0 + 3.5) * SX,0,MaxX - (Y0 - 1.0) * SY,MaxY - (X0 + 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 4.0) * SX,0,MaxX - (Y0 + 1.0) * SY,MaxY - (X0 + 3.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 4.5) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 4.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 4.5) * SX,0,MaxX - (Y0 + 1.5) * SY,MaxY - (X0 + 4.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 6.0) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 4.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 - 1.0) * SY,MaxY - (X0 + 6.5) * SX,0,MaxX - (Y0 - 1.5) * SY,MaxY - (X0 + 6.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 3.5) * SY,MaxY - (X0 + 6.5) * SX,0,MaxX - (Y0 - 1.0) * SY,MaxY - (X0 + 6.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 3.5) * SY,MaxY - (X0 + 2.0) * SX,0,MaxX - (Y0 + 3.5) * SY,MaxY - (X0 + 6.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 3.0) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 + 3.5) * SY,MaxY - (X0 + 2.0) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 1.0) * SY,MaxY - (X0 + 1.5) * SX,0,MaxX - (Y0 + 3.0) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 0.5) * SY,MaxY - (X0 + 1.0) * SX,0,MaxX - (Y0 + 1.0) * SY,MaxY - (X0 + 1.5) * SX,0);
                  Zone.AddBound(MaxX - (Y0 + 0.5) * SY,MaxY - (X0 + 0.5) * SX,0,MaxX - (Y0 + 0.5) * SY,MaxY - (X0 + 1.0) * SX,0);

                  Zone.BreakUpBounds;
                End;
              End;
            End; // Case
          End;
          If GoAhead Then
          Begin
            RegenerateGroundMeshes;
            Zone.BuildBoundingPolygons;
            ReloadZone;
            RenderZone(BirdsEye);
            If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
          End;
        End
        Else ShowMessage('The zone is too small to perform this action.');
      End
      Else ShowMessage('Could not locate the ground mesh.');
    Finally
      SelectedPolygons.Clear;
      SelectedPolyMesh := Nil;
      EnableGUI(True);
    End;
  End;  
end;

procedure TfrmMain.acExportAsMeshExecute(Sender: TObject);
Var
  TN      : TTreeNode;
  ZO      : TZoneObject;
  MO,Mesh : TMeshObject;
  MOR     : TModelOrigin;
  GO      : TGroupObject;
  List    : TStringList;
  NewName : String;
  F       : System.Text;
  MinPt   : T3DPoint;
  MaxPt   : T3DPoint;
  I       : Integer;
  Found   : Boolean;
  GoAhead : Boolean;
  AbsLoc  : T3DPoint;

  Procedure AddLight(ZO: TZoneObject);
  Var
    I     : Integer;
    Group : TGroupObject;
    L     : TLightObject;
    Loc1  : T3DPoint;
    L1    : TZoneObject;
    ML    : TMeshLibraryObjectReference;

  Begin
    If ZO Is TLightObject Then
    Begin
      L    := TLightObject(ZO);
      Loc1 := L.GetAbsoluteLocation;
      L1   := L.MakeCopy;
      L1.Loc.Copy(Loc1);
      L1.Loc.Subtract(MinPt);
      GO.Objects.AddObject(L1.GetName,L1);
      Loc1.Free;
    End
    Else If ZO Is TGroupObject Then
    Begin
      Group := TGroupObject(ZO);
      For I := 0 To Group.Objects.Count - 1 Do AddLight(TZoneObject(Group.Objects.Objects[I]));
    End
    Else If ZO Is TMeshLibraryObjectReference Then
    Begin
      ML := TMeshLibraryObjectReference(ZO);
      If ML.Group <> Nil Then //AddLight(ML.Group);
      Begin
        Group := TGroupObject.Create(ML.Group);
        Group.SetZone(Zone);
        Group.SetParent(ZO.GetParent);
        Group.Loc.Copy(ZO.Loc);
        Group.Rotate.Copy(ZO.Rotate);
        Group.Size.Copy(ZO.Size);
        AddLight(Group);
        Group.Free;
      End;
    End;
  End; // AddLight

  Procedure AddHotSpot(ZO: TZoneObject);
  Var
    I     : Integer;
    Group : TGroupObject;
    HS    : THotSpot;
    Loc1  : T3DPoint;
    HS1   : TZoneObject;
    ML    : TMeshLibraryObjectReference;

  Begin
    If ZO Is THotSpot Then
    Begin
      HS   := THotSpot(ZO);
      Loc1 := HS.GetAbsoluteLocation;
      HS1  := HS.MakeCopy;
      HS1.Loc.Copy(Loc1);
      HS1.Loc.Subtract(MinPt);
      GO.Objects.AddObject(HS1.GetName,HS1);
      Loc1.Free;
    End
    Else If ZO Is TGroupObject Then
    Begin
      Group := TGroupObject(ZO);
      For I := 0 To Group.Objects.Count - 1 Do AddHotSpot(TZoneObject(Group.Objects.Objects[I]));
    End
    Else If ZO Is TMeshLibraryObjectReference Then
    Begin
      ML := TMeshLibraryObjectReference(ZO);
      If ML.Group <> Nil Then //AddLight(ML.Group);
      Begin
        Group := TGroupObject.Create(ML.Group);
        Group.SetZone(Zone);
        Group.SetParent(ZO.GetParent);
        Group.Loc.Copy(ZO.Loc);
        Group.Rotate.Copy(ZO.Rotate);
        Group.Size.Copy(ZO.Size);
        AddHotSpot(Group);
        Group.Free;
      End;
    End;
  End; // AddHotSpot

begin
  Try
    EnableGUI(False);
    If Not LoadingParms Then
    Begin
      If tvMain.SelectionCount = 1 Then
      Begin
        TN := tvMain.Selections[0];
        If TN <> Nil Then
        Begin
          ZO      := TZoneObject(TN.Data);
          NewName := ZO.GetName;
          If InputQuery('Export Object as Mesh','Enter mesh name:',NewName) Then
          Begin
            NewName := Trim(NewName);
            If NewName <> '' Then
            Begin
              Found := (MeshLibraryObjectIndex(NewName) >= 0);
              If Found Then
              Begin
                GoAhead := (Application.MessageBox('An object with that name already exists.'#13#10 +
                                          'Overwrite it?',
                                          'Warning',
                                          MB_OKCANCEL) = IDOK);
              End
              Else GoAhead := True;
              If GoAhead Then
              Begin
                // Look for a model origin

                     If ZO Is TGroupObject Then MOR := TGroupObject(ZO).FindModelOrigin
                Else If ZO Is TModelOrigin Then MOR := TModelOrigin(ZO)
                Else MOR := Nil;

                // Create a mesh object from the currently selected object

                List := TStringList.Create;
                ZO.AddToPolygonList(List,False,True,True,True);
                MO := Zone.CoalescePolygonList(List);
                MO.SetName(NewName);

                // The coordinates in the mesh are relative to the zone.  We need to change the mesh vertices
                // so the coordinates are relative only to the mesh.

                MinPt := T3DPoint.Create;
                MaxPt := T3DPoint.Create;
                MO.GetBounds(MinPt,MaxPt);

                // If we have a model origin then make that the new object's origin, otherwise
                // use the bottom center of the object

                If MOR = Nil Then
                Begin
                  // Center in the X and Y directions

                  MinPt.X := (MinPt.X + MaxPt.X) / 2;
                  MinPt.Y := (MinPt.Y + MaxPt.Y) / 2;
                End
                Else
                Begin
                  AbsLoc := MOR.GetAbsoluteLocation;
                  MinPt.Copy(AbsLoc);
                  AbsLoc.Free;
                End;
                For I := 0 To MO.Vertices.Count - 1 Do T3DPoint(MO.Vertices.Objects[I]).Subtract(MinPt);

                // Form a group object from what we have selected

                If MO.Vertices.Count > 0 Then
                Begin
                  If Found Then
                  Begin
                    GO := TGroupObject(MeshLibrary.Objects[MeshLibraryObjectIndex(NewName)]);
                    For I := 0 To GO.Objects.Count - 1 Do GO.Objects.Objects[I].Free;
                    GO.Objects.Clear;
                  End
                  Else GO := TGroupObject.Create(MO.GetName);
                  GO.Objects.AddObject(MO.GetName,MO);
                End;

                // The client doesn't like placeable objects with more than 1532 polygons.

                Mesh := TMeshObject.Create(MO);
                Mesh.ConvertToTriangles;
                If Mesh.Polygons.Count > 1532 Then ShowMessage('Warning: this object contains ' + IntToStr(Mesh.Polygons.Count) + 'polygons.'#13#10 +
                                                               'Exported objects with more than 1532 polygons might not display properly in the client.');
                Mesh.Free;

                // Add any lights that were in the selection

                AddLight(ZO);

                // Add any hot spots that were in the selection

                AddHotSpot(ZO);

                // Save the group to the mesh library

                GO.SetName(NewName);
                AssignFile(F,ExtractFilePath(Application.EXEName) + 'library\meshes\' + NewName + '.msh');
                ReWrite(F);
                GO.SaveToFile(F,0);
                CloseFile(F);

                // Add the new group to the library in memory

                If Not Found Then MeshLibrary.AddObject(GO.GetName,GO);

                // Cleanup

                For I := 0 To List.Count - 1 Do List.Objects[I].Free;
                List.Free;
                MeshLibrary.Sort;
                LoadObjectTreeView;
                LoadMeshLibraryList(tvMeshes);
                MinPt.Free;
                MaxPt.Free;
              End;
            End
            Else ShowMessage('You cannot enter a blank name.');
          End;
        End;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acInsertMeshLibraryObjectExecute(Sender: TObject);
Var
  ML : TMeshLibraryObjectReference;
  CL : TCreatureLibraryObjectReference;
  St : String;
  I  : Integer;

Begin
  If SelectedMesh <> '' Then
  Begin
    St := SelectedMesh;

    // Find a unique name

    I := 1;
    While Zone.NameExists(St + IntToStr(I)) Do Inc(I);
    St := St + IntToStr(I);
    If InputQuery('Create new Object','Enter new object name:',St) Then
    Begin
      St := Trim(St);
      If St <> '' Then
      Begin
        If Not Zone.NameExists(St) Then
        Begin
          I  := MeshLibrary.IndexOf(SelectedMesh);
          ML := TMeshLibraryObjectReference.Create(TGroupObject(MeshLibrary.Objects[I]));
          ML.SetName(St);

          ML.Loc.X := Observer.X + CreateDist * Cos(Phi);
          ML.Loc.Y := Observer.Y + CreateDist * Sin(Phi);
          ML.Loc.Z := Observer.Z;

          // Add the object

          AddNewObject(St,ML);

          // Adjust it's position since gravity is on by default

          RefreshObject(ML,True,True,True);
        End
        Else ShowMessage('An object with that name already exists. Please choose a different one.')
      End
      Else ShowMessage('You cannot enter a blank name.');
    End;
    glView.scene3D.Redraw;
  End
  Else If SelectedCreature <> '' Then
  Begin
    St := SelectedCreature;

    // Find a unique name

    I := 1;
    While Zone.NameExists(St + IntToStr(I)) Do Inc(I);
    St := St + IntToStr(I);
    If InputQuery('Add creature reference','Enter new object name:',St) Then
    Begin
      St := Trim(St);
      If St <> '' Then
      Begin
        If Not Zone.NameExists(St) Then
        Begin
          I  := CreatureLibrary.IndexOf(SelectedCreature);
          CL := TCreatureLibraryObjectReference.Create(TAn8File(CreatureLibrary.Objects[I]));
          CL.SetName(St);

          CL.Loc.X := Observer.X + CreateDist * Cos(Phi);
          CL.Loc.Y := Observer.Y + CreateDist * Sin(Phi);
          CL.Loc.Z := Observer.Z;

          // Add the object

          AddNewObject(St,CL);

          // Adjust it's position since gravity is on by default

          RefreshObject(CL,True,True,True);
        End
        Else ShowMessage('An object with that name already exists. Please choose a different one.')
      End
      Else ShowMessage('You cannot enter a blank name.');
    End;
    glView.scene3D.Redraw;
  End;
end;

Procedure TfrmMain.RegenerateMeshesWithGravity;
Var
  I,J : Integer;
  ZO  : TZoneObject;
  B   : Boolean;

Begin
  B := frmStatus.Visible;
  frmStatus.Show;
  frmStatus.SetCaption('Refreshing zone objects');
  J := Zone.Count - 1;
  For I := 0 To J Do
  Begin
    frmStatus.SetPosition(I / Zone.Count);
    ZO := TZoneObject(Zone.Objects[I]);
    If ZO Is TMeshLibraryObjectReference Then
    Begin
      If TMeshLibraryObjectReference(ZO).Gravity Then RefreshObject(ZO,False,I = J,I = J);
    End;
  End; // For I
  If Not B Then frmStatus.Hide;
  RenderZone(BirdsEye);
End; // TfrmMain.RegenerateMeshesWithGravity

procedure TfrmMain.acInsertLightSourceExecute(Sender: TObject);
Var
  L  : TLightObject;
  St : String;
  I  : Integer;

Begin
  St := 'light';
  If St <> '' Then
  Begin
    // Find a unique name

    I := 1;
    While Zone.NameExists(St + IntToStr(I)) Do Inc(I);
    St := St + IntToStr(I);
    If InputQuery('Create new object','Enter new object name:',St) Then
    Begin
      St := Trim(St);
      If St <> '' Then
      Begin
        If Not Zone.NameExists(St) Then
        Begin
          L := TLightObject.Create(St);
          L.Loc.X := Observer.X + CreateDist * Cos(Phi);
          L.Loc.Y := Observer.Y + CreateDist * Sin(Phi);
          L.Loc.Z := Observer.Z;

          // Add the object

          AddNewObject(St,L);
        End
        Else ShowMessage('An object with that name already exists. Please choose a different one.')
      End
      Else ShowMessage('You cannot enter a blank name.');
    End;
    glView.scene3D.Redraw;
  End;
end;

procedure TfrmMain.acConvertToTransparentExecute(Sender: TObject);
begin
  frmConvertBMP.ShowModal;
end;

procedure TfrmMain.acExtendEdgesExecute(Sender: TObject);
Var
  X,Y     : Integer;
  Noise   : Single;
  SX,SY   : Integer;

  Procedure ShiftTags(XInc,YInc: Integer; ShiftSouthOrEast: Boolean);
  Var MO1,MO2: TMeshObject;

    Procedure Shift(MO: TMeshObject);
    Var
      I     : Integer;
      P     : TPolygon;
      X,Y   : Integer;
      XI,YI : Integer;
    Begin
      For I := 0 To MO.Polygons.Count - 1 Do
      Begin
        P := TPolygon(MO.Polygons.Objects[I]);
        Y := P.Tag Div (Zone.ElevationGrid.NX - 1);
        X := P.Tag - Y * (Zone.ElevationGrid.NX - 1);
        If ShiftSouthOrEast Then
        Begin
          XI := XInc;
          YI := YInc;
        End
        Else
        Begin
          XI := 0;
          YI := 0;
        End;
        P.Tag := (Y + YI) * (Zone.ElevationGrid.NX + XInc - 1) + X + XI;
      End; // For I
    End; // Shift

  Begin
    MO1 := Zone.FindMeshObject(meshHeightMapGround);
    MO2 := Zone.FindMeshObject(meshHeightMapUnderwater);
    If MO1 <> Nil Then Shift(MO1);
    If MO2 <> Nil Then Shift(MO2);
  End; // ShiftTags

begin
  Try
    EnableGUI(False);
    If frmExtendGround.ShowModal = mrOk Then
    Begin
      If frmExtendGround.cbNorth.Checked Or
         frmExtendGround.cbSouth.Checked Or
         frmExtendGround.cbEast.Checked  Or
         frmExtendGround.cbWest.Checked  Then
      Begin
        SX := GridSize;
        SY := GridSize;

        // Grow the zone at the edges we selected

        If frmExtendGround.cbNorth.Checked Then
        Begin
          SetLength(Zone.ElevationGrid.Heights,Zone.ElevationGrid.NX * (Zone.ElevationGrid.NY + 6));
          SetLength(Zone.ElevationGrid.Visible,(Zone.ElevationGrid.NX - 1) * (Zone.ElevationGrid.NY + 5));
          For Y := Zone.ElevationGrid.NY - 1 DownTo 0 Do
          Begin
            For X := Zone.ElevationGrid.NX - 1 DownTo 0 Do
            Begin
              Zone.ElevationGrid.Heights[(Y + 6) * Zone.ElevationGrid.NX + X] := Zone.ElevationGrid.Heights[Y * Zone.ElevationGrid.NX + X];
            End; // For X
          End; // For Y
          For Y := Zone.ElevationGrid.NY - 2 DownTo 0 Do
          Begin
            For X := Zone.ElevationGrid.NX - 2 DownTo 0 Do
            Begin
              Zone.ElevationGrid.Visible[(Y + 6) * (Zone.ElevationGrid.NX - 1) + X] := Zone.ElevationGrid.Visible[Y * (Zone.ElevationGrid.NX - 1) + X];
            End; // For X
          End; // For Y
          ShiftTags(0,6,True);
          Inc(Zone.ElevationGrid.NY,6);
          Zone.ElevationGrid.MaxX := Zone.ElevationGrid.MaxX + 6 * SY;
          For Y := 0 To 5 Do
          Begin
            For X := 0 To Zone.ElevationGrid.NX - 1 Do
            Begin
              If frmExtendGround.cbRumple.Checked
               Then Noise := Random * 10 - 5
               Else Noise := 0;
              Zone.ElevationGrid.SetHeight(X,Y,Zone.ElevationGrid.GetHeight(X,6) + Noise);
            End; // For X
            For X := 0 To Zone.ElevationGrid.NX - 2 Do Zone.ElevationGrid.SetVisible(X,Y,True);
          End; // For Y
        End;

        If frmExtendGround.cbSouth.Checked Then
        Begin
          SetLength(Zone.ElevationGrid.Heights,Zone.ElevationGrid.NX * (Zone.ElevationGrid.NY + 6));
          SetLength(Zone.ElevationGrid.Visible,(Zone.ElevationGrid.NX - 1) * (Zone.ElevationGrid.NY + 5));
          ShiftTags(0,6,False);
          Inc(Zone.ElevationGrid.NY,6);
          Zone.ElevationGrid.MinX := Zone.ElevationGrid.MinX - 6 * SY;
          For Y := Zone.ElevationGrid.NY - 6 To Zone.ElevationGrid.NY - 1 Do
          Begin
            For X := 0 To Zone.ElevationGrid.NX - 1 Do
            Begin
              If frmExtendGround.cbRumple.Checked
               Then Noise := Random * 10 - 5
               Else Noise := 0;
              Zone.ElevationGrid.SetHeight(X,Y,Zone.ElevationGrid.GetHeight(X,Zone.ElevationGrid.NY - 7) + Noise);
            End; // For X
            For X := 0 To Zone.ElevationGrid.NX - 2 Do Zone.ElevationGrid.SetVisible(X,Y - 1,True);
          End; // For Y
        End;

        If frmExtendGround.cbWest.Checked Then
        Begin
          SetLength(Zone.ElevationGrid.Heights,(Zone.ElevationGrid.NX + 6) * Zone.ElevationGrid.NY);
          SetLength(Zone.ElevationGrid.Visible,(Zone.ElevationGrid.NX + 5) * (Zone.ElevationGrid.NY - 1));
          For Y := Zone.ElevationGrid.NY - 1 DownTo 0 Do
          Begin
            For X := Zone.ElevationGrid.NX - 1 DownTo 0 Do
            Begin
              Zone.ElevationGrid.Heights[Y * (Zone.ElevationGrid.NX + 6) + X + 6] := Zone.ElevationGrid.Heights[Y * Zone.ElevationGrid.NX + X];
            End; // For X
          End; // For Y
          For Y := Zone.ElevationGrid.NY - 2 DownTo 0 Do
          Begin
            For X := Zone.ElevationGrid.NX - 2 DownTo 0 Do
            Begin
              Zone.ElevationGrid.Visible[Y * (Zone.ElevationGrid.NX + 5) + X + 6] := Zone.ElevationGrid.Visible[Y * (Zone.ElevationGrid.NX - 1) + X];
            End; // For X
          End; // For Y
          ShiftTags(6,0,True);
          Inc(Zone.ElevationGrid.NX,6);
          Zone.ElevationGrid.MaxY := Zone.ElevationGrid.MaxY + 6 * SX;
          For Y := 0 To Zone.ElevationGrid.NY - 1 Do
          Begin
            For X := 0 To 5 Do
            Begin
              If frmExtendGround.cbRumple.Checked
               Then Noise := Random * 10 - 5
               Else Noise := 0;
              Zone.ElevationGrid.SetHeight(X,Y,Zone.ElevationGrid.GetHeight(6,Y) + Noise);
            End; // For X
          End; // For Y
          For Y := 0 To Zone.ElevationGrid.NY - 2 Do
          Begin
            For X := 0 To 5 Do Zone.ElevationGrid.SetVisible(X,Y,True);
          End; // For Y
        End;

        If frmExtendGround.cbEast.Checked Then
        Begin
          SetLength(Zone.ElevationGrid.Heights,(Zone.ElevationGrid.NX + 6) * Zone.ElevationGrid.NY);
          SetLength(Zone.ElevationGrid.Visible,(Zone.ElevationGrid.NX + 5) * (Zone.ElevationGrid.NY - 1));
          For Y := Zone.ElevationGrid.NY - 1 DownTo 0 Do
          Begin
            For X := Zone.ElevationGrid.NX - 1 DownTo 0 Do
            Begin
              Zone.ElevationGrid.Heights[Y * (Zone.ElevationGrid.NX + 6) + X] := Zone.ElevationGrid.Heights[Y * Zone.ElevationGrid.NX + X];
            End; // For X
          End; // For Y
          For Y := Zone.ElevationGrid.NY - 2 DownTo 0 Do
          Begin
            For X := Zone.ElevationGrid.NX - 2 DownTo 0 Do
            Begin
              Zone.ElevationGrid.Visible[Y * (Zone.ElevationGrid.NX + 5) + X] := Zone.ElevationGrid.Visible[Y * (Zone.ElevationGrid.NX - 1) + X];
            End; // For X
          End; // For Y
          ShiftTags(6,0,False);
          Inc(Zone.ElevationGrid.NX,6);
          Zone.ElevationGrid.MinY := Zone.ElevationGrid.MinY - 6 * SX;
          For Y := 0 To Zone.ElevationGrid.NY - 1 Do
          Begin
            For X := Zone.ElevationGrid.NX - 6 To Zone.ElevationGrid.NX - 1 Do
            Begin
              If frmExtendGround.cbRumple.Checked
               Then Noise := Random * 10 - 5
               Else Noise := 0;
              Zone.ElevationGrid.SetHeight(X,Y,Zone.ElevationGrid.GetHeight(Zone.ElevationGrid.NX - 7,Y) + Noise);
            End; // For X
          End; // For Y
          For Y := 0 To Zone.ElevationGrid.NY - 2 Do
          Begin
            For X := Zone.ElevationGrid.NX - 7 To Zone.ElevationGrid.NX - 2 Do Zone.ElevationGrid.SetVisible(X,Y,True);
          End; // For Y
        End;

        If frmExtendGround.cbNorth.Checked Or
           frmExtendGround.cbSouth.Checked Or
           frmExtendGround.cbEast.Checked  Or
           frmExtendGround.cbWest.Checked  Then Zone.BreakUpBounds;

        RegenerateGroundMeshes;
      End
      Else ShowMessage('You must select an edge to extend.');
    End;
  Finally
    SelectedPolygons.Clear;
    SelectedPolyMesh := Nil;
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acRumpleGroundWithinRadiusExecute(Sender: TObject);
Var
  I,J     : Integer;
  V,V2    : T3DPoint;
  SX,SY   : Integer;
  D       : Single;
  X,Y     : Single;
  X0,Y0   : Integer;
  Adjust  : Single;
  H       : Single;

begin
  Try
    EnableGUI(False);

    // Continue only if we found the mesh we're looking for

    If udRaiseLowerAmount.Position <> 0 Then
    Begin
      X  := Observer.X + GetHeightDist * Cos(Phi);
      Y  := Observer.Y + GetHeightDist * Sin(Phi);
      SX := GridSize;
      SY := GridSize;
      Y0 := Round((Zone.ElevationGrid.MaxX - X) / SX);
      X0 := Round((Zone.ElevationGrid.MaxY - Y) / SY);
      X  := Zone.ElevationGrid.MaxX - (Y0 * SX);
      Y  := Zone.ElevationGrid.MaxY - (X0 * SY);

      // Search the elevation grid for the nearest vertex in x-y space

      V  := T3DPoint.Create;
      V2 := T3DPoint.Create;
      For I := 0 To Zone.ElevationGrid.NX - 1 Do
      Begin
        For J := 0 To Zone.ElevationGrid.NY - 1 Do
        Begin
          V2.X := Zone.ElevationGrid.MaxX - J * SY;
          V2.Y := Zone.ElevationGrid.MaxY - I * SX;
          V2.Z := Zone.ElevationGrid.GetHeight(I,J);
          V.Copy(X,Y,V2.Z);
          V.Subtract(V2);
          If rbCircularArea.Checked Then
          Begin
            D := V.GetLength;
            If D <= udRaiseLowerRadius.Position * SX * 1.01 Then
            Begin
              Adjust := (2 * Random * udRaiseLowerAmount.Position) - udRaiseLowerAmount.Position;
              H      := Zone.ElevationGrid.GetHeight(I,J);
              Zone.ElevationGrid.SetHeight(I,J,H + Adjust);
            End;
          End
          Else
          Begin
            If (Abs(V.X) <= udRaiseLowerRadius.Position * SX * 1.01) And
               (Abs(V.Y) <= udRaiseLowerRadius.Position * SX * 1.01) Then
            Begin
              Adjust := (2 * Random * udRaiseLowerAmount.Position) - udRaiseLowerAmount.Position;
              H      := Zone.ElevationGrid.GetHeight(I,J);
              Zone.ElevationGrid.SetHeight(I,J,H + Adjust);
            End;
          End;
        End; // For J
      End; // For I
      V.Free;
      V2.Free;

      RegenerateGroundMeshes;
      RegenerateMeshesWithGravity;
    End
    Else ShowMessage('Your raise/lower amount is set to zero.');
  Finally
    SelectedPolygons.Clear;
    SelectedPolyMesh := Nil;
    EnableGUI(True);
  End;
end;

procedure TfrmMain.rbCircularAreaClick(Sender: TObject);
begin
  CrosshairX := -1;
  CrosshairY := -1;
  RenderZone(BirdsEye);
end;

procedure TfrmMain.rbSquareAreaClick(Sender: TObject);
begin
  CrosshairX := -1;
  CrosshairY := -1;
  RenderZone(BirdsEye);
end;

procedure TfrmMain.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  If Msg.message = WM_KEYDOWN Then
  Begin
    If ((Msg.hwnd = rbCircularArea.Handle) Or
        (Msg.hwnd = rbSquareArea.Handle))  And
       ((Msg.wParam = VK_LEFT)  Or
        (Msg.wParam = VK_RIGHT) Or
        (Msg.wParam = VK_UP)    Or
        (Msg.wParam = VK_DOWN)) Then Msg.hwnd := Handle;
  End;
end;

procedure TfrmMain.acImportGround3DSExecute(Sender: TObject);
begin
  Try
    EnableGUI(False);
    dlgOpen.FileName   := '';
    dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
    dlgOpen.Filter     := '3DS Max files (*.3ds)|*.3DS';
    If dlgOpen.Execute Then ImportGroundFrom3DS(dlgOpen.FileName);
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acExportZoneLineInfoExecute(Sender: TObject);
Var
  FileName : String;
  St       : String;
  St1      : PChar;
  Continue : Boolean;
  F        : System.Text;
  I        : Integer;
  X,Y,Z    : Single;
  ZP       : TZonePlane;

begin
  Try
    EnableGUI(False);
    dlgSave.FileName   := '';
    dlgSave.InitialDir := ExtractFilePath(Application.ExeName) + 'ZONES';
    dlgSave.Filter     := 'OpenZone zone_points files (*.zpt)|*.ZPT';
    If dlgSave.Execute Then
    Begin
      FileName := dlgSave.FileName;
      If Pos('.',FileName) = 0 Then FileName := FileName + '.zpt';
      If FileExists(FileName) Then
      Begin
        St  := 'Overwrite file ' + FileName + '?';
        St1 := StrAlloc(Length(St) + 1);
        StrPCopy(St1,St);
        Continue := (Application.MessageBox(St1,'File Exists',MB_OKCANCEL) = IDOK);
        StrDispose(St1);
      End
      Else Continue := True;
      If Continue Then
      Begin
        AssignFile(F,FileName);
        ReWrite(F);
        WriteLn(F,'# Put this data into your zone_points table for this zone');
        WriteLn(F,'#');
        WriteLn(F,'# EQEmu and OpenZone have different coordinate systems.');
        WriteLn(F,'# OpenZone uses the coordinate system in .WLD files and');
        WriteLn(F,'# as such the X and Y values here are exchanged for the');
        WriteLn(F,'# EQEmu database''s benefit.');
        WriteLn(F,'# -------------------------------------------------------');
        WriteLn(F,'number'#9'y'#9'x'#9'z'#9'heading'#9'target_zone'#9'target_y'#9'target_x'#9'target_z'#9'target_heading');
        For I := 0 To Zone.ZonePlanes.Count - 1 Do
        Begin
          // EQEmu has X and Y coordinates switched from OpenZone (OpenZone uses the same coordinate system
          // as .WLD files).

          ZP := TZonePlane(Zone.ZonePlanes.Objects[I]);
          X := (ZP.X1 + ZP.X2) / 2;
          Y := (ZP.Y1 + ZP.Y2) / 2;
          If ZP.InfiniteZ
           Then Z := Zone.ElevationGrid.GetHeightAtAbsolute(X,Y)
           Else Z := (ZP.Z1 + ZP.Z2) / 2;
          St := IntToStr(I + 1) + #9 +
                FloatToStr(X) + #9 +
                FloatToStr(Y) + #9 +
                FloatToStr(Z) + #9 +
                '0' + #9 +
                IntToStr(ZP.DestZoneID) + #9;
          If ZP.HasDestX
           Then St := St + FloatToStr(ZP.DestX) + #9
           Else St := St + '999999' + #9;
          If ZP.HasDestY
           Then St := St + FloatToStr(ZP.DestY) + #9
           Else St := St + '999999' + #9;
          If ZP.HasDestZ
           Then St := St + FloatToStr(ZP.DestZ) + #9
           Else St := St + '999999' + #9;
          If ZP.HasDestAngle
           Then St := St + IntToStr(Round(ZP.DestAngle * 512 / 360))
           Else St := St + '999';
          WriteLn(F,St);
        End; // For I
        CloseFile(F);
        ShowMessage('Zoneline information exported to tab-delimited text file.'#13#10'Place it into your zone_points table to use it.');
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.cbShowPlayerLightClick(Sender: TObject);
begin
  RenderZone(BirdsEye);
end;

procedure TfrmMain.tbTimeOfDayChange(Sender: TObject);
begin
  If Not UpdatingFromRender Then
  Begin
    UpdatingFromSlider := True;
    TimeOfDay.Hour   := tbTimeOfDay.Position Div 60;
    TimeOfDay.Minute := tbTimeOfDay.Position Mod 60;
    TimeOfDay.Day    := 20; // (shrug) just pick anything...we'll do the vernal equinox for a happy medium
    TimeOfDay.Month  := 2;  // 0 is January
    TimeOfDay.Year   := 1000;
    RenderZone(BirdsEye);
    UpdatingFromSlider := False;
  End;
end;

procedure TfrmMain.acGenerateMapExecute(Sender: TObject);
Var
  FileName : String;
  St       : String;
  St1      : PChar;
  Continue : Boolean;

begin
  frmGenerateMap.edtLongName.Text  := Zone.LongName;
  frmGenerateMap.edtShortName.Text := Zone.ShortName;
  If frmGenerateMap.ShowModal = mrOk Then
  Begin
    Try
      EnableGUI(False);
      dlgSave.FileName   := Trim(frmGenerateMap.edtShortName.Text) + '.map';
      dlgSave.InitialDir := ExtractFilePath(Application.ExeName) + 'ZONES';
      dlgSave.Filter     := 'ShowEQ/Admin Tool map files (*.map)|*.MAP';
      If dlgSave.Execute Then
      Begin
        FileName := dlgSave.FileName;
        If Pos('.',FileName) = 0 Then FileName := FileName + '.map';
        If FileExists(FileName) Then
        Begin
          St  := 'Overwrite file ' + FileName + '?';
          St1 := StrAlloc(Length(St) + 1);
          StrPCopy(St1,St);
          Continue := (Application.MessageBox(St1,'File Exists',MB_OKCANCEL) = IDOK);
          StrDispose(St1);
        End
        Else Continue := True;
        If Continue Then
        Begin
          frmGenerateMap.SaveMapToFile(FileName);
          ShowMessage('Map saved.');
        End;
      End;
    Finally
      EnableGUI(True);
    End;
  End;
end;

procedure TfrmMain.acZonePropertiesExecute(Sender: TObject);
Var
  I,J : Integer;
  St  : String;
  St1 : String;

begin
  frmZoneProperties.edtLongName.Text  := Zone.LongName;
  frmZoneProperties.edtShortName.Text := Zone.ShortName;
  frmZoneProperties.rbOutdoor.Checked := (Zone.ZoneType = ztOutdoor);
  frmZoneProperties.rbIndoor.Checked  := (Zone.ZoneType = ztIndoor);
  frmZoneProperties.lbZoneExtraMeshes.Items.Clear;
  frmZoneProperties.lbZoneExtraMeshes.Items.AddStrings(Zone.ExtraMeshes);
  frmZoneProperties.lbZoneExtraCreatures.Items.Clear;
  frmZoneProperties.lbZoneExtraCreatures.Items.AddStrings(Zone.Creatures);
  frmZoneProperties.lbMeshLibrary.Items.Clear;
  frmZoneProperties.lbCreatureLibrary.Items.Clear;
  For I := 0 To MeshLibrary.Count - 1 Do frmZoneProperties.lbMeshLibrary.Items.Add(MeshLibrary.Strings[I]);
  For I := 0 To CreatureLibrary.Count - 1 Do frmZoneProperties.lbCreatureLibrary.Items.Add(CreatureLibrary.Strings[I]);
  If frmZoneProperties.ShowModal = mrOk Then
  Begin
    Zone.LongName  := Trim(frmZoneProperties.edtLongName.Text);
    Zone.ShortName := Trim(frmZoneProperties.edtShortName.Text);
    If frmZoneProperties.rbOutdoor.Checked
     Then Zone.ZoneType := ztOutdoor
     Else Zone.ZoneType := ztIndoor;
    Zone.ExtraMeshes.Clear;
    Zone.ExtraMeshes.AddStrings(frmZoneProperties.lbZoneExtraMeshes.Items);
    Zone.Creatures.Clear;
    For I := 0 To frmZoneProperties.lbZoneExtraCreatures.Items.Count - 1 Do
    Begin
      St  := frmZoneProperties.lbZoneExtraCreatures.Items.Strings[I];
      St1 := St;
      J   := Pos(',',St1);
      St1 := Copy(St1,J + 2,Length(St1));
      J   := CreatureLibrary.IndexOf(St1);
      If J >= 0 Then Zone.Creatures.AddObject(St,CreatureLibrary.Objects[J]);
    End; // For I
  End;
end;

Procedure TfrmMain.MakeSoundFiles(ZoneName: String);
Var
  F     : File;
  S     : TSoundRec;
  T     : System.Text;
  I,J   : Integer;
  Sound : TSound;

Begin
  AssignFile(T,ExtractFilePath(Application.ExeName) + '\zones\' + ZoneName + '_sndbnk.eff');
  ReWrite(T);
  WriteLn(T,'EMIT');
  For I := 0 To Zone.Sounds.Count - 1 Do
  Begin
    Sound := TSound(Zone.Sounds.Objects[I]);
    If Not Sound.Area Then
    Begin
      If Sound.DayName   <> '' Then WriteLn(T,Sound.DayName);
      If Sound.NightName <> '' Then WriteLn(T,Sound.NightName);
    End;
  End; // For I
  WriteLn(T,'LOOP');
  For I := 0 To Zone.Sounds.Count - 1 Do
  Begin
    Sound := TSound(Zone.Sounds.Objects[I]);
    If Sound.Area Then
    Begin
      If Sound.DayName   <> '' Then WriteLn(T,Sound.DayName);
      If Sound.NightName <> '' Then WriteLn(T,Sound.NightName);
    End;
  End; // For I
  CloseFile(T);

  AssignFile(F,ExtractFilePath(Application.ExeName) + '\zones\' + ZoneName + '_sounds.eff');
  ReWrite(F,1);
  J := 1;
  For I := 0 To Zone.Sounds.Count - 1 Do
  Begin
    FillChar(S,SizeOf(S),0);
    Sound    := TSound(Zone.Sounds.Objects[I]);
    S.L1[1]  := (I + 1) * $800 + $02300000;
    S.L1[4]  := I + 100000;
    S.X      := Sound.X;
    S.Y      := Sound.Y;
    S.Z      := Sound.Z;
    S.Radius := Sound.Radius;
    If Sound.Area Then
    Begin
      If Sound.DayName <> '' Then
      Begin
        S.Sound1 := J + 161;
        Inc(J);
      End
      Else S.Sound1 := 0;
      If Sound.NightName <> '' Then
      Begin
        S.Sound2 := J + 161;
        Inc(J);
      End
      Else S.Sound2 := 0;
      S.B1     := 0;
      S.L3[1]  := 0;
      S.Dist1  := 0;
    End
    Else
    Begin
      If Sound.DayName <> '' Then
      Begin
        S.Sound1 := J;
        Inc(J);
      End
      Else S.Sound1 := 0;
      If Sound.NightName <> '' Then
      Begin
        S.Sound2 := J;
        Inc(J);
      End
      Else S.Sound2 := 0;
      S.B1     := 2;
      S.L3[1]  := 100;
      S.Dist1  := Round(Sound.Radius);
    End;
    BlockWrite(F,S,SizeOf(S));
  End; // For I
  CloseFile(F);
End; // MakeSounds

procedure TfrmMain.acPreferencesExecute(Sender: TObject);
Var
  St  : String;
  St1 : String;
  I,J : Integer;

begin
  Try
//    St := frmPreferences.ShellTreeView.Path;
    St := ProgramSettings.EQDir;
    If (St <> '') And (St[Length(St)] <> '\') Then St := St + '\';
    If DirectoryExists(ProgramSettings.EQDir) Then
    Begin
      frmPreferences.ShellTreeView.Path := St;//EQDir;
    End;
  Finally
//    frmPreferences.ShellTreeView.Path := St;
    frmPreferences.cbUseNumericKeypad.Checked   := ProgramSettings.UseNumericKeypad;
    frmPreferences.cbShowZoneDimensions.Checked := ProgramSettings.ShowZoneDimensions;
    frmPreferences.cbClearType.Checked          := ProgramSettings.ClearType;
    If ProgramSettings.ClearTypeRGB
     Then frmPreferences.cbClearTypeMethod.ItemIndex := 0
     Else frmPreferences.cbClearTypeMethod.ItemIndex := 1;
    frmPreferences.fcbInactive.TTonly := False;
    frmPreferences.fcbActive.TTonly := False;
    frmPreferences.fcbInactive.ItemIndex := frmPreferences.fcbInactive.Items.IndexOf(ProgramSettings.ClearTypeInactive);
    frmPreferences.fcbActive.ItemIndex   := frmPreferences.fcbActive.Items.IndexOf(ProgramSettings.ClearTypeActive);
    frmPreferences.cbInactiveSize.Text   := IntToStr(ProgramSettings.ClearTypeInactiveSize);
    frmPreferences.cbActiveSize.Text     := IntToStr(ProgramSettings.ClearTypeActiveSize);
    St1 := Trim(Format('%f',[ProgramSettings.WLDCreatureSize]));
    frmPreferences.edtWLDSize.Text       := St1;
    St1 := Trim(Format('%f',[ProgramSettings.XWFCreatureSize]));
    frmPreferences.edtXWFSize.Text       := St1;
    St1 := Trim(Format('%f',[ProgramSettings.SmallMoveAmount]));
    frmPreferences.edtMoveAmount.Text    := St1;
    St1 := Trim(Format('%f',[ProgramSettings.SmallRotateAmount]));
    frmPreferences.edtRotateAmount.Text  := St1;
    If frmPreferences.ShowModal = mrOk Then
    Begin
      ProgramSettings.EQDir              := frmPreferences.ShellTreeView.Path;
      ProgramSettings.UseNumericKeypad   := frmPreferences.cbUseNumericKeypad.Checked;
      ProgramSettings.ShowZoneDimensions := frmPreferences.cbShowZoneDimensions.Checked;
      ProgramSettings.ClearType          := frmPreferences.cbClearType.Checked;
      ProgramSettings.ClearTypeRGB       := (frmPreferences.cbClearTypeMethod.ItemIndex = 0);
      Val(frmPreferences.edtWLDSize.Text,ProgramSettings.WLDCreatureSize,I);
      Val(frmPreferences.edtXWFSize.Text,ProgramSettings.XWFCreatureSize,I);
      Val(frmPreferences.edtMoveAmount.Text,ProgramSettings.SmallMoveAmount,I);
      Val(frmPreferences.edtRotateAmount.Text,ProgramSettings.SmallRotateAmount,I);
      If frmPreferences.fcbInactive.ItemIndex >= 0 Then ProgramSettings.ClearTypeInactive := frmPreferences.fcbInactive.Items[frmPreferences.fcbInactive.ItemIndex];
      If frmPreferences.fcbActive.ItemIndex   >= 0 Then ProgramSettings.ClearTypeActive   := frmPreferences.fcbActive.Items[frmPreferences.fcbActive.ItemIndex];
      Val(frmPreferences.cbInactiveSize.Text,I,J);
      If J = 0 Then ProgramSettings.ClearTypeInactiveSize := I;
      Val(frmPreferences.cbActiveSize.Text,I,J);
      If J = 0 Then ProgramSettings.ClearTypeActiveSize := I;
{
      If ProgramSettings.ClearType Then
      Begin
        ZPropList1.Font.Name := 'Arial';
        ZPropList1.Font.Size := 8;
        tvMeshes.Font.Name   := 'Arial';
        tvMeshes.Font.Size   := 8;
      End
      Else
      Begin
        ZPropList1.Font.Name := 'Microsoft Sans Serif';
        ZPropList1.Font.Size := 8;
        tvMeshes.Font.Name   := 'Microsoft Sans Serif';
        tvMeshes.Font.Size   := 8;
      End;
}
      DataModule1.ClearTypeText.Enabled           := ProgramSettings.ClearType;
      DataModule1.ClearTypeText.InactiveFont.Name := ProgramSettings.ClearTypeInactive;
      DataModule1.ClearTypeText.ActiveFont.Name   := ProgramSettings.ClearTypeActive;
      DataModule1.ClearTypeText.InactiveFont.Size := ProgramSettings.ClearTypeInactiveSize;
      DataModule1.ClearTypeText.ActiveFont.Size   := ProgramSettings.ClearTypeActiveSize;

      If ProgramSettings.ClearTypeRGB
       Then DataModule1.ClearTypeText.Flavor := ctfRGB
       Else DataModule1.ClearTypeText.Flavor := ctfBGR;

      tvMeshes.Font.Name   := ZPropList1.Font.Name;
      tvMeshes.Font.Size   := ZPropList1.Font.Size;
      TBToolbar1.Font.Name := ZPropList1.Font.Name;
      TBToolbar1.Font.Size := ZPropList1.Font.Size;
      TBToolbar1.View.GetFont.Name := ZPropList1.Font.Name;
      TBToolbar1.View.GetFont.Size := ZPropList1.Font.Size;

      glView.ShowDiapasones := ProgramSettings.ShowZoneDimensions;
      SavePreferences;
    End;
  End;
end;

procedure TfrmMain.acShiftZoneExecute(Sender: TObject);
Var
  I     : Integer;
  X,Y,Z : Single;

begin
  frmShiftZone.edtXShift.Text := '0';
  frmShiftZone.edtYShift.Text := '0';
  frmShiftZone.edtZShift.Text := '0';
  If frmShiftZone.ShowModal = mrOk Then
  Begin
    Val(Trim(frmShiftZone.edtXShift.Text),X,I);
    If I = 0 Then
    Begin
      Val(Trim(frmShiftZone.edtYShift.Text),Y,I);
      If I = 0 Then
      Begin
        Val(Trim(frmShiftZone.edtZShift.Text),Z,I);
        If I = 0 Then
        Begin
          Try
            EnableGUI(False);
            ZPropList1.CurObj := Nil;
            Zone.ShiftZone(X,Y,Z);
            Observer.X := Observer.X + X;
            Observer.Y := Observer.Y + Y;
            Observer.Z := Observer.Z + Z;
            ReloadZone;
            CrosshairX := -1;
            CrosshairY := -1;
            RenderZone(BirdsEye);
            If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
            SetViewToNorth;
            DisplayStats;
          Finally
            EnableGUI(True);
          End;
        End
        Else ShowMessage('You must enter a valid Z shift value.');
      End
      Else ShowMessage('You must enter a valid Y shift value.');
    End
    Else ShowMessage('You must enter a valid X shift value.');
  End;
end;

procedure TfrmMain.cbTransparentAsSolidClick(Sender: TObject);
begin
  ReloadZone;
  RenderZone(BirdsEye);
  If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
  SetViewToNorth;
  DisplayStats;
end;

procedure TfrmMain.acSetObjectPosToCurrentExecute(Sender: TObject);
Var
  TN   : TTreeNode;
  ZO   : TZoneObject;

begin
  Try
    EnableGUI(False);
    If Not LoadingParms Then
    Begin
      If tvMain.SelectionCount = 1 Then
      Begin
        TN := tvMain.Selections[0];
        If TN <> Nil Then
        Begin
          ZO := TZoneObject(TN.Data);
          If ZO.GetParent = Nil Then
          Begin
            ZO.Loc.Copy(Observer);
//            ZO.Loc.Z          := ZO.Loc.Z - ObsHeight;
            LoadParameters;
            ZPropList1.CurObj := Nil; // Force it to reload
            ZPropList1.CurObj := ZO;
            RefreshObject(ZO,True,True,True);

            If (ZO Is TMeshLibraryObjectReference) And
               TMeshLibraryObjectReference(ZO).Gravity Then ShowMessage('Gravity is ON for this object');
          End
          Else ShowMessage('Cannot perform this operation on a grouped object.');
        End;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.btnNegateClick(Sender: TObject);
begin
  udRaiseLowerAmount.Position := -udRaiseLowerAmount.Position;
end;

procedure TfrmMain.ZPropList1NewObject(Sender: TZPropList; OldObj,
  NewObj: TObject);
Var
  P : Array[0..2] Of Single;
  S : Array[0..2] Of Single;
  R : Array[0..2] Of Single;
  I : Integer;
  E : TEntity;

begin
  // Save the position and rotation

  If BirdsEye Then
  Begin
    If glView.Scene3D.Scene.Entities.Count > FirstEntity Then
    Begin
      P[0] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Position.X;
      P[1] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Position.Y;
      P[2] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Position.Z;
      S[0] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Scale.X;
      S[1] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Scale.Y;
      S[2] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Scale.Z;
      R[0] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Rotation.X;
      R[1] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Rotation.Y;
      R[2] := TEntity(glView.Scene3D.Scene.Entities.Items[FirstEntity]).Rotation.Z;
    End;
  End
  Else
  Begin
    P[0] := glView.Scene3D.Scene.DefaultCamera.Position.X;
    P[1] := glView.Scene3D.Scene.DefaultCamera.Position.Y;
    P[2] := glView.Scene3D.Scene.DefaultCamera.Position.Z;
    S[0] := glView.Scene3D.Scene.DefaultCamera.SceneCenter.X;
    S[1] := glView.Scene3D.Scene.DefaultCamera.SceneCenter.Y;
    S[2] := glView.Scene3D.Scene.DefaultCamera.SceneCenter.Z;
    R[0] := glView.Scene3D.Scene.DefaultCamera.UpVector.X;
    R[1] := glView.Scene3D.Scene.DefaultCamera.UpVector.Y;
    R[2] := glView.Scene3D.Scene.DefaultCamera.UpVector.Z;
  End;

 // Update the selection rectangles

  SelectedPolygons.Clear;
  If (NewObj <> Nil) And (NewObj Is TMeshObject)
   Then SelectedPolyMesh := TMeshObject(NewObj)
   Else SelectedPolyMesh := Nil;
  SetSelectionRectangles;
  AddModelOrigins;
  SetPolygonSelectionRectangles;
  AddLightSpheres;
  AddHotSpots;

  // Render the zone

  RenderZone(BirdsEye);
  If BirdsEye Then
  Begin
    For I := 1 To FirstEntity - 1 Do glView.FitFrame(False,True,False,I,False);
  End;

  // Restore the position and rotation

  If BirdsEye Then
  Begin
    // FitFrame only changes "local" settings so we don't need to restore the normal settings,
    // and we don't want to either because animated objects have them set differently and
    // doing this will screw them up.
{
    For I := 0 To glView.Scene3D.Scene.Entities.Count - 1 Do
    Begin
      E := TEntity(glView.Scene3D.Scene.Entities.Items[I]);
      E.Position.Copy(P[0],P[1],P[2]);
      E.Scale.Copy(S[0],S[1],S[2]);
      E.Rotation.Copy(R[0],R[1],R[2]);
    End; // For I
}    
  End
  Else
  Begin
    glView.Scene3D.Scene.DefaultCamera.SetPosition(P[0],P[1],P[2]);
    glView.Scene3D.Scene.DefaultCamera.LookAt(S[0],S[1],S[2]);
    glView.Scene3D.Scene.DefaultCamera.SetVectorUp(R[0],R[1],R[2]);
  End;
  glView.Scene3D.Redraw;
  EnableGUI(True);
end;

procedure TfrmMain.acHelpAboutExecute(Sender: TObject);
begin
  frmSplash.Visible       := False;
  frmSplash.btnOk.Visible := True;
  frmSplash.ShowModal;
end;

Function TfrmMain.GetNodeForMesh(TN: TTreeNode; Var Count: Integer): TTreeNode;
// For the mesh in MasterMeshList indexed by Count, this recursive method returns
// the TTreeNode in tvMain that corresponds to it.
Var Next: TTreeNode;
Begin
  Result := Nil;
  If TN.HasChildren Then Result := GetNodeForMesh(TN.getFirstChild,Count);
  If Result = Nil Then
  Begin
    If (Count = 0) And Not TN.HasChildren Then Result := TN
    Else
    Begin
      If Not TN.HasChildren Then Dec(Count);
      Next := TN.getNextSibling;
      If Next <> Nil Then Result := GetNodeForMesh(Next,Count);
    End;
  End;
End; // TfrmMain.GetNodeForMesh

Procedure TfrmMain.CalculateClickRay(X,Y: Integer);
Var
  W,H       : Integer;
  U,V       : Single;
  U1,V1     : Single;
  U2,V2     : Single;
  Right     : T3DPoint;
  Up        : T3DPoint;
  Fwd       : T3DPoint;
  Obs       : T3DPoint;
  Dest      : T3DPoint;

Begin
  W  := glView.ImagePanel.Width;
  H  := glView.ImagePanel.Height;
  V  := glView.Scene3D.Scene.DistNear * Tan(glView.Scene3D.Scene.Angle * (Pi / 180) / 2);
  U  := V * W / H;
  U1 := U * ((X - (W / 2)) / (W / 2));
  V1 := V * ((Y - (H / 2)) / (H / 2));
  U2 := U1 * glView.Scene3D.Scene.DistFar / glView.Scene3D.Scene.DistNear;
  V2 := V1 * glView.Scene3D.Scene.DistFar / glView.Scene3D.Scene.DistNear;

  Obs := T3DPoint.Create(glView.Scene3D.Scene.DefaultCamera.Position.X,
                         glView.Scene3D.Scene.DefaultCamera.Position.Y,
                         glView.Scene3D.Scene.DefaultCamera.Position.Z);

  Fwd := T3DPoint.Create(glView.Scene3D.Scene.DefaultCamera.SceneCenter.X,
                         glView.Scene3D.Scene.DefaultCamera.SceneCenter.Y,
                         glView.Scene3D.Scene.DefaultCamera.SceneCenter.Z);
  Fwd.Subtract(Obs);
  Fwd.Normalize;

  Up := T3DPoint.Create(glView.Scene3D.Scene.DefaultCamera.UpVector.X,
                        glView.Scene3D.Scene.DefaultCamera.UpVector.Y,
                        glView.Scene3D.Scene.DefaultCamera.UpVector.Z);
  Up.Normalize;

  Right := T3DPoint.Create(Fwd);
  Right.Cross(Up);
  Right.Normalize;

  Dest := T3DPoint.Create(Right);
  Dest.Multiply(U2);
  Dest.Add(Obs);
  Up.Multiply(-V2);
  Dest.Add(Up);
  Fwd.Multiply(glView.Scene3D.Scene.DistFar / glView.Scene3D.Scene.DistNear);
  Dest.Add(Fwd);

  ClickObs.Copy(Obs);
  ClickDest.Copy(Dest);

  // Cleanup

  Dest.Free;
  Right.Free;
  Up.Free;
  Fwd.Free;
  Obs.Free;
End; // TfrmMain.CalculateClickRay

Function TfrmMain.GetMeshIndexAtScreenPos(X,Y: Integer; SaveIntersect: Boolean): Integer;
Var
  I,J,K     : Integer;
  Clicked   : TStringList;
  P         : TPolygon;
  MO        : TMeshObject;
  Intersect : T3DPoint;
  Closest   : T3DPoint;
  Distance  : Single;
  Distance1 : Single;
  Obs       : T3DPoint;
  Fwd       : T3DPoint;
  V1,V2,V3  : T3DPoint;

Begin
  If MasterMeshList <> Nil Then
  Begin
    CalculateClickRay(X,Y);

    // Form a list of anything we may have clicked on

    Clicked        := TStringList.Create;
    Intersect      := T3DPoint.Create;
    Closest        := T3DPoint.Create(ClickDest);
    K              := -1;
    Distance       := Closest.DistanceFrom(ClickObs);
    ClickPoly      := Nil;
    ClickPolyIndex := -1;

    Obs := T3DPoint.Create(glView.Scene3D.Scene.DefaultCamera.Position.X,
                           glView.Scene3D.Scene.DefaultCamera.Position.Y,
                           glView.Scene3D.Scene.DefaultCamera.Position.Z);
    Fwd := T3DPoint.Create(glView.Scene3D.Scene.DefaultCamera.SceneCenter.X,
                           glView.Scene3D.Scene.DefaultCamera.SceneCenter.Y,
                           glView.Scene3D.Scene.DefaultCamera.SceneCenter.Z);
    Fwd.Subtract(Obs);
    Fwd.Normalize;

    V1 := T3DPoint.Create;
    V2 := T3DPoint.Create;
    V3 := T3DPoint.Create;
    For I := 0 To MasterMeshList.Count - 1 Do
    Begin
      MO := TMeshObject(MasterMeshList.Objects[I]);
      MO.ConvertToTriangles;
      For J := 0 To MO.Polygons.Count - 1 Do
      Begin
        P := TPolygon(MO.Polygons.Objects[J]);
        V1.Copy(T3DPoint(MO.Vertices.Objects[P.Vertices[0]]));
        V2.Copy(T3DPoint(MO.Vertices.Objects[P.Vertices[1]]));
        V3.Copy(T3DPoint(MO.Vertices.Objects[P.Vertices[2]]));
        MO.MakeAbsolute(V1);
        MO.MakeAbsolute(V2);
        MO.MakeAbsolute(V3);
        If P.IsVisibleTo(Obs,Fwd,MO,False) And
           LineFacet(ClickObs,ClickDest,V1,V2,V3,Intersect) Then
        Begin
          Distance1 := Intersect.DistanceFrom(Observer);
          If (Distance1 < Distance) And (Distance1 >= glView.Scene3D.Scene.DistNear) Then
          Begin
            Distance := Distance1;
            Closest.Copy(Intersect);
            If SaveIntersect Then ClickIntersect.Copy(Closest);
            K              := I;
            ClickPoly      := P;
            ClickPolyIndex := J;
          End;
        End;
      End; // For J
    End; // For I
    V1.Free;
    V2.Free;
    V3.Free;

    // Cleanup

    Clicked.Free;
    Intersect.Free;
    Closest.Free;
    Obs.Free;
    Fwd.Free;
    Result := K;
  End
  Else Result := -1;
End; // TfrmMain.GetMeshIndexAtScreenPos

procedure TfrmMain.glViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  I,J,K : Integer;
  TN    : TTreeNode;

begin
  If Button In [mbLeft,mbRight] Then
  Begin
    I := GetMeshIndexAtScreenPos(X,Y,False);

    // Pick the closest object

    ClickIndex := -1;
    If I >= 0 Then
    Begin
      Try
        EnableGUI(False);
        J := I; // Save it here
        If tvMain.Items.Count > 0
         Then TN := GetNodeForMesh(tvMain.Items[0],I)
         Else TN := Nil;
        If TN <> Nil Then
        Begin
          ClickIndex := J;
          If (TObject(TN.Data) Is TMeshObject) And
             (Not TZoneObject(TN.Data).IsSpecialObject) And
             (ClickPolyIndex >= 0) And
             (ClickPolyIndex < TMeshObject(TN.Data).Polygons.Count) Then
          Begin
            If (Button = mbLeft) Or (SelectedPolyMesh <> TMeshObject(TN.Data)) Then
            Begin
              SelectedPolyMesh := TMeshObject(TN.Data);
              SelectedPolygons.Clear;
              SelectedPolygons.Add(Pointer(ClickPolyIndex));
            End
            Else
            Begin
              K := SelectedPolygons.IndexOf(Pointer(ClickPolyIndex));
              If K < 0
               Then SelectedPolygons.Add(Pointer(ClickPolyIndex))
               Else SelectedPolygons.Delete(K);
            End;
          End
          Else
          Begin
            SelectedPolygons.Clear;
            SelectedPolyMesh := Nil;
          End;
          If Button = mbLeft
           Then tvMain.Selected := TN
           Else tvMain.Select(TN,[ssCtrl]);
          AddModelOrigins;
          AddHotSpots; 
          SetSelectionRectangles; 
          SetPolygonSelectionRectangles;
          glView.Scene3D.Redraw;
        End;
      Finally
        EnableGUI(True);
      End;
    End;
    LeftDown := False;
  End;
end;

procedure TfrmMain.glViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var I: Integer;
begin
  If Button = mbLeft Then
  Begin
    I        := GetMeshIndexAtScreenPos(X,Y,True);
    LeftDown := True;
    If I <> ClickIndex Then ClickIndex := -1;
  End;
end;

procedure TfrmMain.glViewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  I       : Integer;
  Closest : T3DPoint;
  TN      : TTreeNode;
  MO      : TMeshObject;
  ZO      : TZoneObject;
  AbsLoc  : T3DPoint;
  List    : TStringList;

begin
  If LeftDown Then
  Begin
    If ClickIndex >= 0 Then
    Begin
      CalculateClickRay(X,Y);
      Closest := T3DPoint.Create;
      If ssShift In Shift Then
      Begin
        gts_point_segment_closest(ClickIntersect,ClickObs,ClickDest,Closest);
        Closest.X := ClickIntersect.X;
        Closest.Y := ClickIntersect.Y;
      End
      Else
      Begin
        If (ClickIntersect.Z - ClickObs.Z) * (ClickIntersect.Z - ClickDest.Z) < 0 Then
        Begin
          Closest.Copy(ClickDest);
          Closest.Subtract(ClickObs);
          Closest.Multiply(Abs(ClickIntersect.Z - ClickObs.Z) / Abs(Closest.Z));
          Closest.Add(ClickObs);
        End
        Else
        Begin
          gts_point_segment_closest(ClickIntersect,ClickObs,ClickDest,Closest);
          Closest.Z := ClickIntersect.Z;
        End;
      End;
      Closest.Subtract(ClickIntersect);
      I := ClickIndex; // Have to copy it because GetNodeForMesh destroys it
      If tvMain.Items.Count > 0
       Then TN := GetNodeForMesh(tvMain.Items[0],I)
       Else TN := Nil;
      If TN <> Nil Then
      Begin
        ZO     := TZoneObject(TN.Data);
        If ZO Is TMeshLibraryObjectReference Then
        Begin
          If (ssShift In Shift) And TMeshLibraryObjectReference(ZO).Gravity Then ShowMessage('Gravity is ON for this object')
          Else
          Begin
            ZO.Loc.Add(Closest);
            AbsLoc := ZO.GetAbsoluteLocation;
            ClickIntersect.Copy(AbsLoc);
            AbsLoc.Free;
            List := TStringList.Create;
            ZO.AddToPolygonList(List,False,False,True,True);
            MasterMeshList.Objects[ClickIndex].Free;
            MasterMeshList.Objects[ClickIndex] := List.Objects[0];
            LoadParameters;
            ZPropList1.CurObj := Nil; // Force it to reload
            ZPropList1.CurObj := ZO;
            RefreshObject(ZO,True,True,True);
            List.Free;
          End;
        End;
      End;
      Closest.Free;
    End;
  End;
end;

procedure TfrmMain.acImportQuake3MapExecute(Sender: TObject);
Const Q3Scale = 5;
Var
  Quake3BSP : TQuake3BSP;
  MO        : TMeshObject;
  I,J,K,L,M : Integer;
  V         : T3DPoint;
  N         : T3DPoint;
  P,P1      : TPolygon;
  St        : String;
  C         : TRGBA;
  QuakePath : String;
  ShaderMan : TShaderManager;
  Shader    : TShader;
  Cull      : Integer;

  Function AdjustTexCoord(S: Single): Single;
  Begin
    S := 10 - S;
    While S < 0 Do S := S + 10;
    Result := S;
  End; // AdjustTexCoord

  Function FindTexName(Shader: TShader): String;
  Var
    I,J   : Integer;
    Found : Boolean;
    St    : String;

    Function TextureExists(Path: String): Boolean;
    Var
      S     : TSearchRec;
      Found : Boolean;

    Begin
      Found := False;
      If FindFirst(Path,faAnyFile,S) = 0 Then
      Begin
        Repeat
          If (S.Attr And faDirectory) = 0 Then Found := True; 
        Until (FindNext(S) <> 0) Or Found;
      End;
      FindClose(S);
      Result := Found;
    End; // TextureExists

  Begin
    I      := 0;
    Found  := False;
    Result := '';
    Cull   := Shader.Cull;
{
    If Pos('skullarch_b',LowerCase(Shader.Name)) > 0 Then
     ShowMessage('test');
}
    While (I <= High(Shader.Layers)) And Not Found Do
    Begin
//      J := 0;
      If Shader.Layers[I].numOfTextures > 0 Then
      Begin
        For J := 0 To High(Shader.Layers[I].TextureNames) Do
        Begin
          If (Copy(Shader.Layers[I].TextureNames[J],1,1) <> '$') And
             (Length(ExtractFileExt(Shader.Layers[I].TextureNames[J])) > 0) Then
          Begin
            St := Shader.Layers[I].TextureNames[J];
{
            If Pos('flame1',LowerCase(St)) > 0 Then
             ShowMessage('test');
}

            // Make sure the texture actually exists

            St := Copy(St,1,Length(St) - Length(ExtractFileExt(St)));
            If TextureExists(ExtractFilePath(Application.ExeName) + 'library\textures\quake3\' + St + '.*') Then
            Begin
              If Result <> '' Then Result := Result + ';';
              Result := Result + 'quake3\' + St;
              If (Shader.Layers[I].SrcBlend  = GL_ONE) And
                 (Shader.Layers[I].DestBlend = GL_ONE) Then
              Begin
//                 P.TextureState := tsSemiTransparent;
                 P.NeedsMask    := True;
                 P.Masked       := True;
                 P.HasMasked    := True;
              End;
            End;
          End;
        End; // For J
      End;
      If Result <> '' Then
      Begin
        Found := True;
        If ((Shader.Surface And 8388608) <> 0) And Not P.Masked Then P.TextureState := tsSemiTransparent;
      End;
      Inc(I);
    End; // While
  End; // FindTexName

begin
  dlgOpen.FileName   := '';
  dlgOpen.Filter     := 'Quake 3 maps (*.bsp)|*.BSP';
  dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
  If dlgOpen.Execute Then
  Begin
    QuakePath := ExtractFilePath(dlgOpen.FileName) + '\..';
    ShaderMan := TShaderManager.Create(QuakePath);
    Quake3BSP := TQuake3BSP.Create;
    Quake3BSP.ShaderManager := ShaderMan;
    If Quake3BSP.LoadBSP(QuakePath,ExtractFileName(dlgOpen.FileName)) Then
    Begin
      MO := TMeshObject.Create('Q3Mesh' + IntToStr(Zone.Count + 1));
      Zone.AddObject(MO.GetName,MO);

      // Add vertices

      For I := 0 To Quake3BSP.numOfVerts - 1 Do
      Begin
        V := T3DPoint.Create(Quake3BSP.Vertices[I].Position.X / Q3Scale,
                             -Quake3BSP.Vertices[I].Position.Z / Q3Scale,
                             Quake3BSP.Vertices[I].Position.Y / Q3Scale);
        N := T3DPoint.Create(Quake3BSP.Vertices[I].Normal.X,
                             -Quake3BSP.Vertices[I].Normal.Z,
                             Quake3BSP.Vertices[I].Normal.Y);
        N.Normalize;
        MO.Vertices.AddObject('',V);
        MO.Normals.AddObject('',N);
      End; // For I

      // Add polygons

      For I := 0 To Quake3BSP.numOfFaces - 1 Do
      Begin
        If Quake3BSP.Faces[I].numMeshVerts > 0 Then
        Begin
          K := Quake3BSP.Faces[I].numMeshVerts Div 3;
          M := 0;
          For L := 0 To K - 1 Do
          Begin
            P := TPolygon.Create;
            SetLength(P.Vertices,3);
            SetLength(P.TX,3);
            SetLength(P.TZ,3);
            SetLength(P.Colors,3);
            For J := 0 To 2 Do
            Begin
              P.Vertices[J]  := Quake3BSP.Faces[I].startVertIndex + Quake3BSP.MeshVertices[Quake3BSP.Faces[I].meshVertIndex + M + J];
              P.TX[J]        := Quake3BSP.Vertices[P.Vertices[J]].TextureCoord.X;
              P.TZ[J]        := Quake3BSP.Vertices[P.Vertices[J]].TextureCoord.Y;
              C.B            := Quake3BSP.Vertices[P.Vertices[J]].Color[0];
              C.G            := Quake3BSP.Vertices[P.Vertices[J]].Color[1];
              C.R            := Quake3BSP.Vertices[P.Vertices[J]].Color[2];
              C.A            := 255 - Quake3BSP.Vertices[P.Vertices[J]].Color[3];
              P.Colors[J]    := TColor(C);
            End; // For J
            J := ShaderMan.IndexOf(Quake3BSP.TextureInfo[Quake3BSP.Faces[I].TextureID].TextureName);
            If J >= 0 Then
            Begin
              Shader := ShaderMan.Items[J];
              St     := FindTexName(Shader);
              If St <> '' Then
              Begin
                P.Texture := St;

                If Cull = GL_NONE Then
                Begin
                  P.HasTexCoords := True;
                  P.HasColor     := True;
                  P1 := TPolygon.Create(P);
                  For J := 0 To 2 Do
                  Begin
                    P1.Vertices[J] := P.Vertices[2 - J];
                    P1.TX[J]       := P.TX[2 - J];
                    P1.TZ[J]       := P.TZ[2 - J];
                  End; // For J
                  MO.Polygons.AddObject('',P1);
                End;

              End
              Else P.Texture := 'quake3\' + Quake3BSP.TextureInfo[Quake3BSP.Faces[I].TextureID].TextureName;
            End
            Else P.Texture := 'quake3\' + Quake3BSP.TextureInfo[Quake3BSP.Faces[I].TextureID].TextureName;
            P.HasTexCoords := True;
            P.HasColor     := True;
            MO.Polygons.AddObject('',P);
            Inc(M,3);
          End; // For L
        End;
      End; // For I

      // Load the model and update the view

      Observer.Free;
      Observer := T3DPoint.Create(0,0,0);
      LoadObjectTreeView;

      // Get a polygon list from the zone

      ReloadZone;

      // Render the zone

      RenderZone(BirdsEye);
      glView.Fit(Not BirdsEye,True,False);
      SetViewToNorth;
      UpdateTitleBar;
    End
    Else ShowMessage('There was a problem importing ' + dlgOpen.FileName + '.' + #13#10 +
                     'Make sure you have created a "quake3" folder under "library\textures" and' + #13#10 +
                     'extracted all your .pk3 files in that quake3 folder.');
    Quake3BSP.Free;
    ShaderMan.Free;
  End;
end;

procedure TfrmMain.acMoveObjectNorthExecute(Sender: TObject);
Var
  I,J  : Integer;
  ZO   : TZoneObject;
  List : TStringList;

begin
  List := TStringList.Create;
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    ZO := TZoneObject((tvMain.Selections[I]).Data);
    If Not ZO.IsSpecialObject Then
    Begin
      J  := Zone.GetObjectIndex(ZO);
      ZO.Loc.X := ZO.Loc.X + ProgramSettings.SmallMoveAmount;
      If Abs(Round(ZO.Loc.X * 10) - (ZO.Loc.X * 10)) < 0.01 Then ZO.Loc.X := Round(ZO.Loc.X * 10) / 10;
      List.Clear;
      ZO.AddToPolygonList(List,False,False,True,True);
      MasterMeshList.Objects[J].Free;
      MasterMeshList.Objects[J] := List.Objects[0];
      RefreshObject(ZO,True,True,True);
      If I = 0 Then
      Begin
        ZPropList1.CurObj := Nil;
        ZPropList1.CurObj := ZO;
      End;
    End;
  End; // For I
  List.Free;
end;

procedure TfrmMain.acMoveObjectSouthExecute(Sender: TObject);
Var
  I,J  : Integer;
  ZO   : TZoneObject;
  List : TStringList;

begin
  List := TStringList.Create;
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    ZO := TZoneObject((tvMain.Selections[I]).Data);
    If Not ZO.IsSpecialObject Then
    Begin
      J  := Zone.GetObjectIndex(ZO);
      ZO.Loc.X := ZO.Loc.X - ProgramSettings.SmallMoveAmount;
      If Abs(Round(ZO.Loc.X * 10) - (ZO.Loc.X * 10)) < 0.01 Then ZO.Loc.X := Round(ZO.Loc.X * 10) / 10;
      List.Clear;
      ZO.AddToPolygonList(List,False,False,True,True);
      MasterMeshList.Objects[J].Free;
      MasterMeshList.Objects[J] := List.Objects[0];
      RefreshObject(ZO,True,True,True);
      If I = 0 Then
      Begin
        ZPropList1.CurObj := Nil;
        ZPropList1.CurObj := ZO;
      End;
    End;
  End; // For I
  List.Free;
end;

procedure TfrmMain.acMoveObjectEastExecute(Sender: TObject);
Var
  I,J  : Integer;
  ZO   : TZoneObject;
  List : TStringList;

begin
  List := TStringList.Create;
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    ZO := TZoneObject((tvMain.Selections[I]).Data);
    If Not ZO.IsSpecialObject Then
    Begin
      J  := Zone.GetObjectIndex(ZO);
      ZO.Loc.Y := ZO.Loc.Y - ProgramSettings.SmallMoveAmount;
      If Abs(Round(ZO.Loc.Y * 10) - (ZO.Loc.Y * 10)) < 0.01 Then ZO.Loc.Y := Round(ZO.Loc.Y * 10) / 10;
      List.Clear;
      ZO.AddToPolygonList(List,False,False,True,True);
      MasterMeshList.Objects[J].Free;
      MasterMeshList.Objects[J] := List.Objects[0];
      RefreshObject(ZO,True,True,True);
      If I = 0 Then
      Begin
        ZPropList1.CurObj := Nil;
        ZPropList1.CurObj := ZO;
      End;
    End;
  End; // For I
  List.Free;
end;

procedure TfrmMain.acMoveObjectWestExecute(Sender: TObject);
Var
  I,J  : Integer;
  ZO   : TZoneObject;
  List : TStringList;

begin
  List := TStringList.Create;
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    ZO := TZoneObject((tvMain.Selections[I]).Data);
    If Not ZO.IsSpecialObject Then
    Begin
      J  := Zone.GetObjectIndex(ZO);
      ZO.Loc.Y := ZO.Loc.Y + ProgramSettings.SmallMoveAmount;
      If Abs(Round(ZO.Loc.Y * 10) - (ZO.Loc.Y * 10)) < 0.01 Then ZO.Loc.Y := Round(ZO.Loc.Y * 10) / 10;
      List.Clear;
      ZO.AddToPolygonList(List,False,False,True,True);
      MasterMeshList.Objects[J].Free;
      MasterMeshList.Objects[J] := List.Objects[0];
      RefreshObject(ZO,True,True,True);
      If I = 0 Then
      Begin
        ZPropList1.CurObj := Nil;
        ZPropList1.CurObj := ZO;
      End;
    End;
  End; // For I
  List.Free;
end;

procedure TfrmMain.acMoveObjectUpExecute(Sender: TObject);
Var
  I,J  : Integer;
  ZO   : TZoneObject;
  List : TStringList;

begin
  List := TStringList.Create;
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    ZO := TZoneObject((tvMain.Selections[I]).Data);
    If Not ZO.IsSpecialObject Then
    Begin
      J  := Zone.GetObjectIndex(ZO);
      ZO.Loc.Z := ZO.Loc.Z + ProgramSettings.SmallMoveAmount;
      If Abs(Round(ZO.Loc.Z * 10) - (ZO.Loc.Z * 10)) < 0.01 Then ZO.Loc.Z := Round(ZO.Loc.Z * 10) / 10;
      List.Clear;
      ZO.AddToPolygonList(List,False,False,True,True);
      MasterMeshList.Objects[J].Free;
      MasterMeshList.Objects[J] := List.Objects[0];
      RefreshObject(ZO,True,True,True);
      If I = 0 Then
      Begin
        ZPropList1.CurObj := Nil;
        ZPropList1.CurObj := ZO;
      End;
    End;
  End; // For I
  List.Free;
end;

procedure TfrmMain.acMoveObjectDownExecute(Sender: TObject);
Var
  I,J  : Integer;
  ZO   : TZoneObject;
  List : TStringList;

begin
  List := TStringList.Create;
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    ZO := TZoneObject((tvMain.Selections[I]).Data);
    If Not ZO.IsSpecialObject Then
    Begin
      J  := Zone.GetObjectIndex(ZO);
      ZO.Loc.Z := ZO.Loc.Z - ProgramSettings.SmallMoveAmount;
      If Abs(Round(ZO.Loc.Z * 10) - (ZO.Loc.Z * 10)) < 0.01 Then ZO.Loc.Z := Round(ZO.Loc.Z * 10) / 10;
      List.Clear;
      ZO.AddToPolygonList(List,False,False,True,True);
      MasterMeshList.Objects[J].Free;
      MasterMeshList.Objects[J] := List.Objects[0];
      RefreshObject(ZO,True,True,True);
      If I = 0 Then
      Begin
        ZPropList1.CurObj := Nil;
        ZPropList1.CurObj := ZO;
      End;
    End;
  End; // For I
  List.Free;
end;

procedure TfrmMain.pcRightMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
Var I :Integer;
begin
  If (Y < pcRight.TabWidth) And (X < pcRight.PageCount * (pcRight.TabWidth + 7)) Then
  Begin
    I := X Div (pcRight.TabWidth + 7);
    If I < pcRight.PageCount
     Then pcRight.Hint := pcRight.Pages[I].Hint
     Else pcRight.Hint := '';
  End
  Else pcRight.Hint := '';
end;

procedure TfrmMain.acEditPolygonPropertiesExecute(Sender: TObject);
Var
  I,J           : Integer;
  NumObjects    : Integer;
  List          : TStringList;
  ChangedMeshes : TStringList;
  St            : String;
  St1           : String;
  OrigTexture   : String;
  NewTexture    : String;
  Mesh          : TMeshObject;
  ZO            : TZoneObject;
  Found         : Boolean;
  Polygon       : TPolygon;
  AnimTime      : Single;

  Procedure ChangePolygon(P: TPolygon);
  Begin
    P.Solid := Not frmEditPolygonProperties.cbPassThrough.Checked;
    If P.Solid Then P.HasSolid := True;
    P.Masked    := frmEditPolygonProperties.cbMasked.Checked;
    P.HasMasked := P.Masked;
         If frmEditPolygonProperties.rbSolid.Checked           Then P.TextureState := tsSolid
    Else If frmEditPolygonProperties.rbSemitransparent.Checked Then P.TextureState := tsSemitransparent
    Else If frmEditPolygonProperties.rbTransparent.Checked     Then P.TextureState := tsTransparent;
    P.Texture  := St;
    P.HasColor := frmEditPolygonProperties.cbHasColor.Checked;
  End; // ChangePolygon

begin
  Try
    EnableGUI(False);
    If SelectedPolygons.Count = 0
     Then frmEditPolygonProperties.P := TPolygon(SelectedPolyMesh.Polygons.Objects[0])
     Else frmEditPolygonProperties.P := TPolygon(SelectedPolyMesh.Polygons.Objects[Integer(SelectedPolygons.Items[0])]);
    If {(SelectedPolygon <> Nil) And} (SelectedPolyMesh <> Nil) Then
    Begin
      If frmEditPolygonProperties.ShowModal = mrOk Then
      Begin
        If SelectedPolygons.Count = 0
         Then OrigTexture := UpperCase(TPolygon(SelectedPolyMesh.Polygons.Objects[0]).Texture)
         Else OrigTexture := UpperCase(TPolygon(SelectedPolyMesh.Polygons.Objects[Integer(SelectedPolygons.Items[0])]).Texture);

        // Assemble the textures

        St := frmEditPolygonProperties.GetTexturesAsString;

        If SelectedPolygons.Count = 0 Then
        Begin
          For I := 0 To SelectedPolyMesh.Polygons.Count - 1 Do ChangePolygon(TPolygon(SelectedPolyMesh.Polygons.Objects[I]));
        End
        Else
        Begin
          For I := 0 To SelectedPolygons.Count - 1 Do ChangePolygon(TPolygon(SelectedPolyMesh.Polygons.Objects[Integer(SelectedPolygons.Items[I])]));
        End;
        NewTexture := St;

        ChangedMeshes := TStringList.Create;
        If frmEditPolygonProperties.cbGlobalChange.Checked Then
        Begin
          NumObjects := Zone.GetObjectMeshCount;
          For I := 0 To NumObjects - 1 Do
          Begin
            ZO := Zone.GetZoneObject(I);
            If ZO Is TMeshObject Then
            Begin
              Mesh  := TMeshObject(ZO);
              Found := False;
              For J := 0 To Mesh.Polygons.Count - 1 Do
              Begin
                Polygon := TPolygon(Mesh.Polygons.Objects[J]);
                If UpperCase(Polygon.Texture) = OrigTexture Then
                Begin
                  Found := True;
                  Polygon.Solid := Not frmEditPolygonProperties.cbPassThrough.Checked;
                  If Polygon.Solid Then Polygon.HasSolid := True;
                  Polygon.Masked    := frmEditPolygonProperties.cbMasked.Checked;
                  Polygon.HasMasked := Polygon.Masked;
                       If frmEditPolygonProperties.rbSolid.Checked           Then Polygon.TextureState := tsSolid
                  Else If frmEditPolygonProperties.rbSemitransparent.Checked Then Polygon.TextureState := tsSemitransparent
                  Else If frmEditPolygonProperties.rbTransparent.Checked     Then Polygon.TextureState := tsTransparent;
                  Polygon.Texture  := NewTexture;
                  Polygon.HasColor := frmEditPolygonProperties.cbHasColor.Checked;
                End;
              End; // For J
              If Found Then ChangedMeshes.AddObject('',Pointer(I));
            End;
          End; // For I
        End
        Else ChangedMeshes.AddObject('',Pointer(Zone.GetObjectIndex(SelectedPolyMesh)));

        List := TStringList.Create;
        For J := 0 To ChangedMeshes.Count - 1 Do
        Begin
          I    := Integer(ChangedMeshes.Objects[J]);
          Mesh := TMeshObject(Zone.GetZoneObject(I));
          Mesh.AddToPolygonList(List,False,True,True,True);
          MasterMeshList.Objects[I].Free;
          MasterMeshList.Objects[I] := List.Objects[0];
          RefreshObject(Mesh,True,True,True);
          List.Clear;
        End; // For J
        List.Free;
        ChangedMeshes.Free;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acDeletePolygonsExecute(Sender: TObject);
Var
  I    : Integer;
  List : TStringList;

begin
  Try
    EnableGUI(False);
    If (SelectedPolygons.Count > 0) And (SelectedPolyMesh <> Nil) Then
    Begin
      If Application.MessageBox('Delete selected polygons?'#13#10'This cannot be undone!','Confirm',MB_OKCANCEL) = IDOK Then
      Begin
        List := TStringList.Create;
        For I := 0 To SelectedPolyMesh.Polygons.Count - 1 Do List.AddObject('',Nil);
        For I := 0 To SelectedPolygons.Count - 1 Do List.Objects[Integer(SelectedPolygons.Items[I])] := SelectedPolyMesh.Polygons.Objects[Integer(SelectedPolygons.Items[I])];
        For I := List.Count - 1 DownTo 0 Do
        Begin
          If List.Objects[I] <> Nil Then
          Begin
            List.Objects[I].Free;
            SelectedPolyMesh.Polygons.Delete(I);
          End;
        End; // For I

//        SelectedPolyMesh.Polygons.Delete(ClickPolyIndex);
//        SelectedPolygon.Free;
        SelectedPolyMesh.RemoveUnusedVertices;

        List            := TStringList.Create;
        I               := Zone.GetObjectIndex(SelectedPolyMesh);
        SelectedPolygons.Clear;
        SelectedPolyMesh.AddToPolygonList(List,False,True,True,True);
        MasterMeshList.Objects[I].Free;
        MasterMeshList.Objects[I] := List.Objects[0];
        RefreshObject(SelectedPolyMesh,True,True,True);
        List.Free;

        SelectedPolyMesh := Nil;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acSplitWithGroundExecute(Sender: TObject);
Var
  I,J,K,L    : Integer;
  P,P1       : TPolygon;
  MO1        : TMeshObject;
  MO2        : TMeshObject;
  SplitPairs : TStringList;
  SplitPair  : TSplitPair;
  SplitPair1 : TSplitPair;
  MO         : TMeshObject;
  V1,V2      : T3DPoint;
  TN         : TTreeNode;
  ZO         : TZoneObject;
  Meshes     : TStringList;
  Splits     : TStringList;
  Found      : Boolean;
  Normal     : T3DPoint;
  Center     : T3DPoint;
  B,B1       : Boolean;

begin
  Try
    EnableGUI(False);
    If Application.MessageBox('Split selected objects with the ground? (This cannot be undone!)','Confirm',MB_OKCANCEL) = IDOK Then
    Begin
      Meshes := TStringList.Create;
      For I := 0 To tvMain.SelectionCount - 1 Do
      Begin
        TN := tvMain.Selections[I];
        If TN <> Nil Then
        Begin
          ZO := TZoneObject(TN.Data);
          If ZO Is TMeshObject Then Meshes.AddObject('',ZO);
        End;
      End; // For I
      If Meshes.Count > 0 Then
      Begin
        SelectedPolygons.Clear;
        SelectedPolyMesh := Nil;

        // Find the two ground meshes

        MO1  := Zone.FindMeshObject(meshHeightMapGround);
        MO2  := Zone.FindMeshObject(meshHeightMapUnderwater);

        frmStatus.Show;
        frmStatus.SetCaption('Checking land polygons');
        SplitPairs := TStringList.Create;
        If MO1 <> Nil Then
        Begin
          For I := 0 To MO1.Polygons.Count - 1 Do
          Begin
            frmStatus.SetPosition(I / MO1.Polygons.Count);
            P := TPolygon(MO1.Polygons.Objects[I]);
            If P.HasTag And Zone.ElevationGrid.Visible[P.Tag] Then
            Begin
              For K := 0 To Meshes.Count - 1 Do
              Begin
                MO := TMeshObject(Meshes.Objects[K]);
                For J := 0 To MO.Polygons.Count - 1 Do
                Begin
                  P1 := TPolygon(MO.Polygons.Objects[J]);
                  If P.IntersectsPolygon(P1,MO1,MO) Then
                  Begin
                    SplitPair := TSplitPair.Create;
                    SplitPair.P1 := P;
                    SplitPair.P2 := P1;
                    SplitPair.M1 := MO1;
                    SplitPair.M2 := MO;
                    SplitPair.I1 := I;
                    SplitPair.I2 := J;
                    SplitPair.B1 := False;
                    SplitPair.B2 := False;
                    SplitPairs.AddObject('',SplitPair);
                  End;
                End; // For J
              End; // For K
            End;
          End; // For I
        End;

        frmStatus.SetCaption('Checking underwater polygons');
        If MO2 <> Nil Then
        Begin
          For I := 0 To MO2.Polygons.Count - 1 Do
          Begin
            frmStatus.SetPosition(I / MO2.Polygons.Count);
            P := TPolygon(MO2.Polygons.Objects[I]);
            If P.HasTag And Zone.ElevationGrid.Visible[P.Tag] Then
            Begin
              For K := 0 To Meshes.Count - 1 Do
              Begin
                MO := TMeshObject(Meshes.Objects[K]);
                For J := 0 To MO.Polygons.Count - 1 Do
                Begin
                  P1 := TPolygon(MO.Polygons.Objects[J]);
                  If P.IntersectsPolygon(P1,MO2,MO) Then
                  Begin
                    SplitPair := TSplitPair.Create;
                    SplitPair.P1 := P;
                    SplitPair.P2 := P1;
                    SplitPair.M1 := MO2;
                    SplitPair.M2 := MO;
                    SplitPair.I1 := I;
                    SplitPair.I2 := J;
                    SplitPair.B1 := False;
                    SplitPair.B2 := False;
                    SplitPairs.AddObject('',SplitPair);
                  End;
                End; // For J
              End; // For K
            End;
          End; // For I
        End;

        // For the polygons, calculate the splitting planes

        V1 := T3DPoint.Create;
        V2 := T3DPoint.Create;
        For I := 0 To SplitPairs.Count - 1 Do
        Begin
          SplitPair := TSplitPair(SplitPairs.Objects[I]);
          SplitPair.Q1 := Nil;
          SplitPair.Q2 := Nil;
          SplitPair.R1 := Nil;
          SplitPair.R2 := Nil;
          SplitPair.N1 := SplitPair.P1.GetNormal(SplitPair.M1);
          SplitPair.N2 := SplitPair.P2.GetNormal(SplitPair.M2);
          V1.Copy(T3DPoint(SplitPair.M1.Vertices.Objects[SplitPair.P1.Vertices[0]]));
          V2.Copy(T3DPoint(SplitPair.M2.Vertices.Objects[SplitPair.P2.Vertices[0]]));
          SplitPair.M1.MakeAbsolute(V1);
          SplitPair.M2.MakeAbsolute(V2);
          SplitPair.D1 := GetHessianDistance(SplitPair.N1,V1);
          SplitPair.D2 := GetHessianDistance(SplitPair.N2,V2);
        End; // For I
        V1.Free;
        V2.Free;

        // Split the polygons.  We do this by creating exactly one region for each polygon we want to
        // split and splitting it with every polygon that intersects it.  The resulting region trees will
        // have areas that are flagged and areas that are unflagged.  We only want to keep the unflagged
        // parts.

        frmStatus.SetCaption('Splitting polygons');
        For I := 0 To SplitPairs.Count - 1 Do
        Begin
          frmStatus.SetPosition(I / SplitPairs.Count);
          SplitPair := TSplitPair(SplitPairs.Objects[I]);

          If Not SplitPair.B1 Then
          Begin
            SplitPair.Q1 := TMeshObject.Create;
            SplitPair.Q1.AddCopyOfPolygon(SplitPair.P1,SplitPair.M1);
            SplitPair.R1 := TRegion.Create(SplitPair.Q1,True);
            SplitPair.R1.SetFlags;
            For J := I To SplitPairs.Count - 1 Do
            Begin
              SplitPair1 := TSplitPair(SplitPairs.Objects[J]);
              If SplitPair1.P1 = SplitPair.P1 Then
              Begin
                SplitPair.R1.SplitAlongPlane(SplitPair1.N2,SplitPair1.D2,False,False);
                SplitPair1.B1 := True;
              End;
            End; // For J
          End;

          If Not SplitPair.B2 Then
          Begin
            SplitPair.Q2 := TMeshObject.Create;
            SplitPair.Q2.AddCopyOfPolygon(SplitPair.P2,SplitPair.M2);
            SplitPair.R2 := TRegion.Create(SplitPair.Q2,True);
            SplitPair.R2.SetFlags;
            For J := I To SplitPairs.Count - 1 Do
            Begin
              SplitPair1 := TSplitPair(SplitPairs.Objects[J]);
              If SplitPair1.P2 = SplitPair.P2 Then
              Begin
                SplitPair.R2.SplitAlongPlane(SplitPair1.N1,SplitPair1.D1,False,False);
                SplitPair1.B2 := True;
              End;
            End; // For J
          End;
        End; // For I

        // Get rid of any polygons that didn't intersect anything but aren't in the ground

        frmStatus.SetCaption('Discarding object polygons that are entirely above ground');
        For J := 0 To Meshes.Count - 1 Do
        Begin
          frmStatus.SetPosition(J / Meshes.Count);
          MO := TMeshObject(Meshes.Objects[J]);
          For I := 0 To MO.Polygons.Count - 1 Do
          Begin
            P := TPolygon(MO.Polygons.Objects[I]);
            If P <> Nil Then
            Begin

              Found      := False;
              SplitPair1 := Nil;
              For K := 0 To SplitPairs.Count - 1 Do
              Begin
                SplitPair := TSplitPair(SplitPairs.Objects[K]);
                If P = SplitPair.P2 Then Found := True;
                If MO = SplitPair.M2 Then SplitPair1 := SplitPair;
              End; // For K
              If (SplitPair1 <> Nil) And Not Found Then
              Begin

                // The polygon didn't intersect anything.  See which side of the ground it's on

                V1 := P.GetCenter(MO);
                V2 := SplitPair.P1.GetCenter(SplitPair.M1);
                V2.Subtract(V1);
                Normal := SplitPair.P1.GetNormal(SplitPair.M1);
                If V2.Dot(Normal) < 0 Then
                Begin
                  P.Free;
                  MO.Polygons.Objects[I] := Nil;
                End;
                V1.Free;
                V2.Free;
                Normal.Free;
              End;
            End;
          End; // For I
        End; // For J

        // Get rid of any polygons that face the land polygons we used to split them

        V1 := T3DPoint.Create;
        V2 := T3DPoint.Create;
        frmStatus.SetCaption('Discarding polygons that are partially above ground');
        For J := 0 To SplitPairs.Count - 1 Do
        Begin
          frmStatus.SetPosition(J / SplitPairs.Count);
          SplitPair := TSplitPair(SplitPairs.Objects[J]);
          If SplitPair.Q2 <> Nil Then
          For I := 0 To SplitPair.Q2.Polygons.Count - 1 Do
          Begin
            P := TPolygon(SplitPair.Q2.Polygons.Objects[I]);
            If P <> Nil Then
            Begin
              Center := P.GetCenter(SplitPair.Q2);
              K      := 0;
              Found  := False;
              While (K <= High(P.Vertices)) And Not Found Do
              Begin
                V1.Copy(T3DPoint(SplitPair.Q2.Vertices.Objects[P.Vertices[K]]));
                V2.Copy(T3DPoint(SplitPair.Q2.Vertices.Objects[P.Vertices[(K + 1) Mod (High(P.Vertices) + 1)]]));
                SplitPair.Q2.MakeAbsolute(V1);
                SplitPair.Q2.MakeAbsolute(V2);
                V1.Add(V2);
                V1.Divide(2);
                V2.Copy(V1);
                V2.Subtract(Center);
                V2.Multiply(2);
                V2.Add(Center);
                B := False;
                For L := 0 To SplitPairs.Count - 1 Do
                Begin
                  SplitPair1 := TSplitPair(SplitPairs.Objects[L]);
                  If SplitPair1.P2 = SplitPair.P2 Then
                  Begin
                    // If this polygon faces the splitting polygon and a line drawn from the center of this
                    // polygon through one if its edges intersects the splitting polygon then we know that
                    // this polygon was split off by the splitting polygon and doesn't merely sit in front
                    // of it (this is an issue when dealing with multiple splitting polygons that form a
                    // concave surface -- it's possible to be technically facing a splitting polygon but
                    // actually be behind the one that really matters).  However, if a polygon is known to
                    // lie behind the polygon that split it off, keep it regardless (necessary for polygons
                    // that were split by pairs of opposite-facing polygons).

                    B1 := SplitPair1.P1.IntersectsSegment(Center,V2,SplitPair1.M1);
                    If P.CenterFacesPolygon(SplitPair1.P1,SplitPair.Q2,SplitPair1.M1,True)
                     Then Found := B1
                     Else B     := B Or B1;
                  End;
                End; // While
                Inc(K);
              End; // While
              If Found And Not B Then
              Begin
                P.Free;
                SplitPair.Q2.Polygons.Objects[I] := Nil;
              End;
              Center.Free;
            End;
          End; // For I
        End; // For J
        V1.Free;
        V2.Free;

        // Get rid of any land polygons that face the object polygons we used to split them

        V1 := T3DPoint.Create;
        V2 := T3DPoint.Create;
        frmStatus.SetCaption('Discarding land polygons that are inside selected objects');
        For J := 0 To SplitPairs.Count - 1 Do
        Begin
          frmStatus.SetPosition(J / SplitPairs.Count);
          SplitPair := TSplitPair(SplitPairs.Objects[J]);
          If SplitPair.Q1 <> Nil Then
          For I := 0 To SplitPair.Q1.Polygons.Count - 1 Do
          Begin
            P := TPolygon(SplitPair.Q1.Polygons.Objects[I]);
            If P <> Nil Then
            Begin
              Center := P.GetCenter(SplitPair.Q1);
              K      := 0;
              Found  := False;
              While (K <= High(P.Vertices)) And Not Found Do
              Begin
                V1.Copy(T3DPoint(SplitPair.Q1.Vertices.Objects[P.Vertices[K]]));
                V2.Copy(T3DPoint(SplitPair.Q1.Vertices.Objects[P.Vertices[(K + 1) Mod (High(P.Vertices) + 1)]]));
                SplitPair.Q1.MakeAbsolute(V1);
                SplitPair.Q1.MakeAbsolute(V2);
                V1.Add(V2);
                V1.Divide(2);
                V2.Copy(V1);
                V2.Subtract(Center);
                V2.Multiply(2);
                V2.Add(Center);
                B := False;
                For L := 0 To SplitPairs.Count - 1 Do
                Begin
                  SplitPair1 := TSplitPair(SplitPairs.Objects[L]);
                  If SplitPair1.P1 = SplitPair.P1 Then
                  Begin
                    // If this polygon faces the splitting polygon and a line drawn from the center of this
                    // polygon through one if its edges intersects the splitting polygon then we know that
                    // this polygon was split off by the splitting polygon and doesn't merely sit in front
                    // of it (this is an issue when dealing with multiple splitting polygons that form a
                    // concave surface -- it's possible to be technically facing a splitting polygon but
                    // actually be behind the one that really matters).  However, if a polygon is known to
                    // lie behind the polygon that split it off, keep it regardless (necessary for polygons
                    // that were split by pairs of opposite-facing polygons).

                    B1 := SplitPair1.P2.IntersectsSegment(Center,V2,SplitPair1.M2);
                    If P.CenterFacesPolygon(SplitPair1.P2,SplitPair.Q1,SplitPair1.M2,True)
                     Then Found := B1
                     Else B     := B Or B1;
                  End;
                End; // While
                Inc(K);
              End; // While
              If Found And Not B Then
              Begin
                P.Free;
                SplitPair.Q1.Polygons.Objects[I] := Nil;
              End;
              Center.Free;
            End;
          End; // For I
        End; // For J
        V1.Free;
        V2.Free;

        // Copy the unflagged split object polygons back to the selected objects and get rid
        // of the original unsplit polygons

        frmStatus.SetCaption('Copying remaining object polygons and discarding original unsplit polygons');
        For I := 0 To SplitPairs.Count - 1 Do
        Begin
          frmStatus.SetPosition(I / SplitPairs.Count);
          SplitPair := TSplitPair(SplitPairs.Objects[I]);
          If SplitPair.Q2 <> Nil Then
          Begin
            // Free the original polygon and flag the slot for deletion

            SplitPair.P2.Free;
            SplitPair.M2.Polygons.Objects[SplitPair.I2] := Nil;
            SplitPair.R2.AddToMesh(SplitPair.M2,False,True);
          End;
        End; // For I

        // Discard the slots that contained the original polygons in the selected objects

        frmStatus.SetCaption('Condensing selected objects');
        For J := 0 To Meshes.Count - 1 Do
        Begin
          frmStatus.SetPosition(J / Meshes.Count);
          MO := TMeshObject(Meshes.Objects[J]);
          I := 0;
          While I < MO.Polygons.Count Do
          Begin
            If MO.Polygons.Objects[I] = Nil
             Then MO.Polygons.Delete(I)
             Else Inc(I);
          End; // While
          MO.ConvertToTriangles;
          MO.RemoveUnusedVertices;
        End; // For J

        // Create new objects containing the polygons from the ground mesh

        Splits := TStringList.Create;
        K      := 0;
        frmStatus.SetCaption('Creating split objects from remaining land polygons');
        For I := 0 To SplitPairs.Count - 1 Do
        Begin
          frmStatus.SetPosition(I / SplitPairs.Count);
          SplitPair := TSplitPair(SplitPairs.Objects[I]);
          If SplitPair.Q1 <> Nil Then
          Begin
            MO := TMeshObject.Create;
            SplitPair.R1.AddToMesh(MO,False,True);
            J := K + 1;
            While Zone.NameExists('split' + IntToStr(J)) Do Inc(J);
            MO.SetName('split' + IntToStr(J));
            Splits.AddObject('',MO);
            K := J;
          End;
        End; // For I

        // Add any polygons from the tags that we're going to turn off that didn't intersect anything

        If MO1 <> Nil Then
        Begin
          frmStatus.SetCaption('Adding unsplit land polygons');
          For I := 0 To MO1.Polygons.Count - 1 Do
          Begin
            frmStatus.SetPosition(I / MO1.Polygons.Count);
            P := TPolygon(MO1.Polygons.Objects[I]);
            If P.HasTag Then
            Begin
              J     := 0;
              Found := False;
              K     := -1;
              L     := -1;
              While (J < SplitPairs.Count) And Not Found Do
              Begin
                SplitPair := TSplitPair(SplitPairs.Objects[J]);
                If SplitPair.Q1 <> Nil Then Inc(L);
                If P = SplitPair.P1 Then Found := True;
                If (P.Tag = SplitPair.P1.Tag) And (SplitPair.M1 = MO1) Then K := L;
                Inc(J);
              End; // While
              If (K >= 0) And Not Found Then
              Begin
                MO := TMeshObject(Splits.Objects[K]);
                MO.AddCopyOfPolygon(P,MO1);
              End;
            End;
          End; // For I
        End;

        If MO2 <> Nil Then
        Begin
          frmStatus.SetCaption('Adding unsplit underwater polygons');
          For I := 0 To MO2.Polygons.Count - 1 Do
          Begin
            frmStatus.SetPosition(I / MO2.Polygons.Count);
            P := TPolygon(MO2.Polygons.Objects[I]);
            If P.HasTag Then
            Begin
              J     := 0;
              Found := False;
              K     := -1;
              L     := -1;
              While (J < SplitPairs.Count) And Not Found Do
              Begin
                SplitPair := TSplitPair(SplitPairs.Objects[J]);
                If SplitPair.Q1 <> Nil Then Inc(L);
                If P = SplitPair.P1 Then Found := True;
                If (P.Tag = SplitPair.P1.Tag) And (SplitPair.M1 = MO2) Then K := L;
                Inc(J);
              End; // While
              If (K >= 0) And Not Found Then
              Begin
                MO := TMeshObject(Splits.Objects[K]);
                MO.AddCopyOfPolygon(P,MO2);
              End;
            End;
          End; // For I
        End;

        // Add the new ground mesh split objects

        frmStatus.SetCaption('Adding new ground objects to zone');
        For I := 0 To Splits.Count - 1 Do
        Begin
          frmStatus.SetPosition(I / Splits.Count);
          MO := TMeshObject(Splits.Objects[I]);
          MO.ConvertToTriangles;
          AddNewObject(MO.GetName,MO);
        End; // For I
        Splits.Free;

        // Turn off the ground patches and free allocated objects

        frmStatus.SetCaption('Disabling original ground polygons');
        For I := 0 To SplitPairs.Count - 1 Do
        Begin
          frmStatus.SetPosition(I / SplitPairs.Count);
          SplitPair := TSplitPair(SplitPairs.Objects[I]);

          // Turn off the patch

          Zone.ElevationGrid.Visible[SplitPair.P1.Tag] := False;

          // Cleanup

          SplitPair.N1.Free;
          SplitPair.N2.Free;
          If SplitPair.R1 <> Nil Then SplitPair.R1.Free;
          If SplitPair.R2 <> Nil Then SplitPair.R2.Free;
          If SplitPair.Q1 <> Nil Then SplitPair.Q1.Free;
          If SplitPair.Q2 <> Nil Then SplitPair.Q2.Free;
          SplitPair.Free;
        End; // For I

        SplitPairs.Free;
        frmStatus.Hide;

        RegenerateGroundMeshes;
        ReloadZone;
        RenderZone(BirdsEye);
        If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
      End;
      Meshes.Free;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.cbShowZonelinesClick(Sender: TObject);
begin
  RenderZone(BirdsEye);
  If BirdsEye Then glView.FitFrame(False,True,False,3,False);
end;

procedure TfrmMain.acJumpToObjectLocationExecute(Sender: TObject);
Var
  TN : TTreeNode;
  ZO : TZoneObject;
  V  : T3DPoint;

begin
  If tvMain.SelectionCount > 0 Then
  Begin
    TN := tvMain.Selections[0];
    If TN <> Nil Then
    Begin
      ZO := TZoneObject(TN.Data);
      If ZO <> Nil Then
      Begin
        V := ZO.GetAbsoluteLocation;
        Observer.Copy(V);
        V.Free;
        DisplayStats;
        RenderZone(BirdsEye);
      End;
    End;
  End;
end;

procedure TfrmMain.acHelpExecute(Sender: TObject);
begin
  Application.HelpJump('OpenZone');
end;

Procedure TfrmMain.GenerateStarField;
Var
  I     : Integer;
  Stars : TStringList;
  Star  : TStar;
  Len   : Single;
  S     : Single;

Begin
  // Use the same seed so the star field will always be the same

  RandSeed := 10;
  Stars    := TStringList.Create;

  // Handle each step in creating the stars separately so a change to one algorithm won't necessarily affect the others

  For I := 0 To 4999 Do
  Begin
    Star := TStar.Create;
    Stars.AddObject('',Star);
    Repeat
      Star.X := Random * 2 - 1;
      Star.Y := Random * 2 - 1;
      Star.Z := Random * 2 - 1;
      Len    := Sqrt(Sqr(Star.X) + Sqr(Star.Y) + Sqr(Star.Z));
    Until Len <> 0;
    Star.X := Star.X * 99 / Len;
    Star.Y := Star.Y * 99 / Len;
    Star.Z := Star.Z * 99 / Len;
  End; // For I

  // Handle star color separately

  For I := 0 To Stars.Count - 1 Do
  Begin
    Star := TStar(Stars.Objects[I]);
    S    := Random;
         If S < 0.95 Then Star.Color := $FFFFF0F0 // Bluish-white (atmospheric scattering shifts it to blue)
    Else If S < 0.98 Then Star.Color := $FFD0FFFF // Yellowish
    Else Star.Color := $FFD0D0FF; // Red (giants)
  End; // For I

  // Handle star brightness separately

  For I := 0 To Stars.Count - 1 Do
  Begin
    Star := TStar(Stars.Objects[I]);
    S    := Random;
    TBGRA(Star.Color).R := Round(TBGRA(Star.Color).R * S);
    TBGRA(Star.Color).G := Round(TBGRA(Star.Color).G * S);
    TBGRA(Star.Color).B := Round(TBGRA(Star.Color).B * S);
  End; // For I

  glView.Scene3D.Scene.StarField.AddStars(Stars);
  Stars.Free;
End; // TfrmMain.GenerateStarField

procedure TfrmMain.cbShowStarsClick(Sender: TObject);
begin
  glView.Scene3D.Scene.StarField.Visible := cbShowStars.Checked;
  RenderZone(BirdsEye);
end;

procedure TfrmMain.acExportToXWFExecute(Sender: TObject);
Var
  Stream      : TFileStream;
  St          : String;
  St1         : PChar;
  Continue    : Boolean;
  FileName    : String;
  TexList     : TStringList;
  CharTexList : TStringList;
  I           : Integer;

begin
  Try
    EnableGUI(False);
    dlgSave.FileName   := '';
    dlgSave.InitialDir := ExtractFilePath(Application.ExeName) + 'ZONES';
    dlgSave.Filter     := 'eXtensible World Files (*.xwf)|*.XWF';
    If dlgSave.Execute Then
    Begin
      FileName := dlgSave.FileName;
      If Pos('.',FileName) = 0 Then FileName := FileName + '.xwf';
      If FileExists(FileName) Then
      Begin
        St  := 'Overwrite file ' + FileName + '?';
        St1 := StrAlloc(Length(St) + 1);
        StrPCopy(St1,St);
        Continue := (Application.MessageBox(St1,'File Exists',MB_OKCANCEL) = IDOK);
        StrDispose(St1);
      End
      Else Continue := True;
      If Continue Then
      Begin
        If frmXWFExportOptions.ShowModal = mrOk Then
        Begin
          Stream := TFileStream.Create(FileName,fmCreate);
          frmStatus.Show;
          frmStatus.SetCaption('Exporting zone to XWF');

          TexList := TStringList.Create;
          CharTexList := TStringList.Create;
          For I := 0 To glView.Scene3D.Scene.Textures.Count - 1 Do
           TexList.AddObject(LowerCase(glView.Scene3D.Scene.Textures.Strings[I]),glView.Scene3D.Scene.Textures.Objects[I]);
          Zone.ExportToXWFFile(Stream,TexList,CharTexList,frmXWFExportOptions.rbOctree.Checked,ProgramSettings.XWFCreatureSize);
          CharTexList.Free;
          TexList.Free;
          Stream.Free;
        End;
        frmStatus.Hide;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.tmrTimeOfDayTimer(Sender: TObject);
begin
  CurrTickCount := GetTickCount;
  If ProgramSettings.AnimateTextures Then RenderZone(BirdsEye);
  LastTickCount := CurrTickCount;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tmrTimeOfDay.Enabled := False;
end;

procedure TfrmMain.acExportObjectS3DExecute(Sender: TObject);
Var
  Stream   : TFileStream;
  FileName : String;
  St       : String;
  St1      : PChar;
  Continue : Boolean;
  ObjName  : String;

begin
  Try
    EnableGUI(False);
    ObjName := '';
    If InputQuery('Export to .S3D','Enter object filename without extension (e.g. "global_chr"):',ObjName) Then
    Begin
      ObjName := Trim(ObjName);
      If ObjName <> '' Then
      Begin
        FileName := ExtractFilePath(Application.ExeName) + 'zones\' + ObjName + '.s3d';
        If FileExists(FileName) Then
        Begin
          St  := 'Overwrite file ' + FileName + '?';
          St1 := StrAlloc(Length(St) + 1);
          StrPCopy(St1,St);
          Continue := (Application.MessageBox(St1,'File Exists',MB_OKCANCEL) = IDOK);
          StrDispose(St1);
        End
        Else Continue := True;
        If Continue Then
        Begin
          If Application.MessageBox('This will build an .S3D file containing the objects and textures your'#13#10 +
                                    'scene is referencing.'#13#10#13#10 +
                                    'You CANNOT distribute any .S3D files you create unless you'#13#10 +
                                    'have permission to distribute ALL of the textures inside them!'#13#10#13#10 +
                                    'Continue?',
                                    '!! WARNING !!',
                                    MB_OKCANCEL) = IDOK Then
          Begin
            FileName := ExtractFilePath(Application.ExeName) + 'zones\' + ObjName + '.s3d';
            Stream   := TFileStream.Create(FileName,fmCreate);
            ExportToObjS3D(Stream,ObjName,False);
            Stream.Free;
          End;
        End;
      End
      Else ShowMessage('You must enter a valid file name.');
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acConvert3DSToMshExecute(Sender: TObject);
Var
  I     : Integer;
  Found : Boolean;
  St    : String;

  Procedure ConvertFile(FileName: String; Prompt,MakeCompliant: Boolean);
  Var
    I       : Integer;
    ZO      : TZoneObject;
    GO      : TGroupObject;
    MO      : TMeshObject;
    NewName : String;
    St      : String;
    Found   : Boolean;
    GoAhead : Boolean;
    F       : System.Text;

  Begin
    ZO := Zone.ImportObjectFrom3DSFile(FileName);
    If ZO <> Nil Then
    Begin
      NewName := ZO.GetName;
      If Prompt
       Then GoAhead := InputQuery('Export Object as Mesh','Enter mesh name:',NewName)
       Else GoAhead := True;
      NewName := Trim(NewName);
      If GoAhead Then
      Begin
        If NewName <> '' Then
        Begin
          If MakeCompliant Then
          Begin
            St := Trim(UpperCase(NewName));
            If (Copy(St,1,2) = 'IT') And (Length(St) > 2) And (St[3] In ['0'..'9']) Then
            Begin
              If Copy(St,Length(St) - 2,3) <> 'DEF' Then
              Begin
                St      := St + '_DMSPRITEDEF';
                NewName := St;
              End;
            End;
          End;
          Found := (MeshLibraryObjectIndex(NewName) >= 0);
          If Found And Prompt Then
          Begin
            GoAhead := (Application.MessageBox('An object with that name already exists.'#13#10 +
                                      'Overwrite it?',
                                      'Warning',
                                      MB_OKCANCEL) = IDOK);
          End
          Else GoAhead := True;
          If GoAhead Then
          Begin
            // Set the object to the desired name

            ZO.SetName(NewName);

            // Form a group object from what we have selected (we either have a
            // mesh object or a group object)

            If (ZO Is TMeshObject) And (TMeshObject(ZO).Vertices.Count > 0) Then
            Begin
              If Found Then
              Begin
                GO := TGroupObject(MeshLibrary.Objects[MeshLibraryObjectIndex(NewName)]);
                For I := 0 To GO.Objects.Count - 1 Do GO.Objects.Objects[I].Free;
                GO.Objects.Clear;
              End
              Else GO := TGroupObject.Create(ZO.GetName);
              GO.Objects.AddObject(ZO.GetName,ZO);
            End
            Else GO := TGroupObject(ZO);

            // Save the group to the mesh library

            GO.SetName(NewName);
            AssignFile(F,ExtractFilePath(Application.EXEName) + 'library\meshes\' + NewName + '.msh');
            ReWrite(F);
            GO.SaveToFile(F,0);
            CloseFile(F);

            // Add the new group to the library in memory

            If Not Found Then MeshLibrary.AddObject(GO.GetName,GO);
          End;
        End
        Else ShowMessage('You cannot enter a blank name.');
      End;
    End;
  End; // ConvertFile

begin
  Try
    EnableGUI(False);
    dlgOpen.FileName   := '';
    dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
    dlgOpen.Filter     := '3DS Max files (*.3ds)|*.3DS';
    dlgOpen.Options    := dlgOpen.Options + [ofAllowMultiSelect];
    If dlgOpen.Execute Then
    Begin
      // See if the objects might conform to ITxxxx type.  If any do, prompt the user to see if
      // OpenZone should ensure that the names fully conform.

      I     := 0;
      Found := False;
      While (I < dlgOpen.Files.Count) And Not Found Do
      Begin
        St := ExtractFileNameNoExt(Trim(UpperCase(dlgOpen.Files.Strings[I])));
        If (Copy(St,1,2) = 'IT') And (Length(St) > 2) And (St[3] In ['0'..'9'])
         Then Found := True
         Else Inc(I);
      End; // While

      If Found Then
      Begin
        If Application.MessageBox('Some of the names appear to be possible equipment names.'#13#10 +
                                  'Do you want OpenZone to modify them to ensure they are'#13#10 +
                                  'fully compliant with the client?',
                                  'Question',
                                  MB_YESNO) <> IDYES Then Found := False;
      End;

      For I := 0 To dlgOpen.Files.Count - 1 Do ConvertFile(dlgOpen.Files.Strings[I],dlgOpen.Files.Count = 1,Found);
    End;

    // Cleanup

    LoadObjectTreeView;
    MeshLibrary.Sort;
    LoadMeshLibraryList(tvMeshes);
  Finally
    dlgOpen.Options := dlgOpen.Options - [ofAllowMultiSelect];
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acToolBarIconSize16Execute(Sender: TObject);
begin
  tbToolbar2.Images               := ilMain16;
  pcRight.Images                  := ilMain16;
  pcRight.TabWidth                := 22;
  ProgramSettings.ToolbarIconSize := 16;
  SavePreferences;
end;

procedure TfrmMain.acToolBarIconSize24Execute(Sender: TObject);
begin
  tbToolbar2.Images               := ilMain24;
  pcRight.Images                  := ilMain24;
  pcRight.TabWidth                := 30;
  ProgramSettings.ToolbarIconSize := 24;
  SavePreferences;
end;

procedure TfrmMain.acToolBarIconSize32Execute(Sender: TObject);
begin
  tbToolbar2.Images               := ilMain32;
  pcRight.Images                  := ilMain32;
  pcRight.TabWidth                := 38;
  ProgramSettings.ToolbarIconSize := 32;
  SavePreferences;
end;

procedure TfrmMain.acToolBarIconSize64Execute(Sender: TObject);
begin
  tbToolbar2.Images               := ilMain64;
  pcRight.Images                  := ilMain64;
  pcRight.TabWidth                := 70;
  ProgramSettings.ToolbarIconSize := 64;
  SavePreferences;
end;

procedure TfrmMain.acMenuIconSize16Execute(Sender: TObject);
begin
  tbToolbar1.Images            := ilMain16;
  ProgramSettings.MenuIconSize := 16;
  SavePreferences;
end;

procedure TfrmMain.acMenuIconSize24Execute(Sender: TObject);
begin
  tbToolbar1.Images            := ilMain24;
  ProgramSettings.MenuIconSize := 24;
  SavePreferences;
end;

procedure TfrmMain.acMenuIconSize32Execute(Sender: TObject);
begin
  tbToolbar1.Images            := ilMain32;
  ProgramSettings.MenuIconSize := 32;
  SavePreferences;
end;

procedure TfrmMain.acMenuIconSize64Execute(Sender: TObject);
begin
  tbToolbar1.Images            := ilMain64;
  ProgramSettings.MenuIconSize := 64;
  SavePreferences;
end;

procedure TfrmMain.cbAnimateTexturesClick(Sender: TObject);
begin
  ProgramSettings.AnimateTextures := cbAnimateTextures.Checked;
  SavePreferences;
end;

procedure TfrmMain.ZPropList1PropNameHint(Sender: TZPropList;
  Prop: IProperty; HintInfo: PHintInfo);
Var
  I      : Integer;
  St     : String;
  SO     : TScriptedObject;
  Script : TScripted;
  Op     : TExpressionOp;

begin
  If Prop <> Nil Then
  Begin
    St := UpperCase(Prop.GetName);
    SO := TScriptedObject(ZPropList1.CurObj);
    If (SO <> Nil) And (ZPropList1.CurObj Is TScriptedObject) Then
    Begin
      Script := SO.GetScript;
      If Script <> Nil Then
      Begin
        I := Script.Variables.IndexOf(St);
        If I >= 0 Then
        Begin
          Op := TExpressionOp(Script.Variables.Objects[I]);
          If Op.HintText.Count > 0 Then
          Begin
            St := HintInfo.HintStr + #13#10;
            For I := 0 To Op.HintText.Count - 1 Do St := St + #13#10 + Op.HintText.Strings[I];
            HintInfo.HintStr := St;
            HintInfo.HideTimeout := 500 + 4000 * Op.HintText.Count; // The longer the hint, the longer it stays visible
          End;
        End;
      End; 
    End;
  End;
end;

procedure TfrmMain.acImportXWFExecute(Sender: TObject);
begin
  Try
    EnableGUI(False);
    dlgOpen.FileName   := '';
    dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
    dlgOpen.Filter     := 'eXtensible World Format files (*.xwf)|*.XWF';
    If dlgOpen.Execute Then
    Begin
      frmStatus.Show;
      frmStatus.SetCaption('Importing .XWF zone');
      ImportFromXWF(dlgOpen.FileName);
      frmStatus.Hide;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acExportTo3DSExecute(Sender: TObject);
Var
  St       : String;
  St1      : PChar;
  Continue : Boolean;
  FileName : String;
  TexList  : TStringList;
  I        : Integer;

begin
  Try
    EnableGUI(False);
    dlgSave.FileName   := '';
    dlgSave.InitialDir := ExtractFilePath(Application.ExeName) + 'ZONES';
    dlgSave.Filter     := '3DS Max files (*.3ds)|*.3DS';
    If dlgSave.Execute Then
    Begin
      FileName := dlgSave.FileName;
      If Pos('.',FileName) = 0 Then FileName := FileName + '.3ds';
      If FileExists(FileName) Then
      Begin
        St  := 'Overwrite file ' + FileName + '?';
        St1 := StrAlloc(Length(St) + 1);
        StrPCopy(St1,St);
        Continue := (Application.MessageBox(St1,'File Exists',MB_OKCANCEL) = IDOK);
        StrDispose(St1);
      End
      Else Continue := True;
      If Continue Then
      Begin
        frmStatus.Show;
        frmStatus.SetCaption('Exporting zone to 3DS');

        TexList := TStringList.Create;
        For I := 0 To glView.Scene3D.Scene.Textures.Count - 1 Do
         TexList.AddObject(LowerCase(glView.Scene3D.Scene.Textures.Strings[I]),glView.Scene3D.Scene.Textures.Objects[I]);

        Zone.ExportTo3DSFile(FileName,TexList);
        TexList.Free;
        frmStatus.Hide;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.Splitter1Moved(Sender: TObject);
begin
  imgCompass.Left   := pnlMoveAndCompass.Width - imgCompass.Width;
  imgElevation.Left := imgCompass.Left - imgElevation.Width;
end;

procedure TfrmMain.pcRightResize(Sender: TObject);
begin
  tvMeshes.Invalidate;
end;

procedure TfrmMain.pnlMoveAndCompassResize(Sender: TObject);
begin
  imgCompass.Left   := pnlMoveAndCompass.Width - imgCompass.Width;
  imgElevation.Left := imgCompass.Left - imgElevation.Width;
end;

Procedure TfrmMain.LoadCreatureSkeleton(An8File: TAn8File; View: TGLVisir; Entity: TEntity; ZO: TZoneObject);
Var
  I,J,K          : Integer;
  Sequence       : TAn8Sequence;
  Model          : TModel;
  SeqName        : String;
  SeqPrefix      : String;
  DropLastFrame  : Boolean;
  MSPerFrame     : Integer;
  SR             : TSkeletonRenderable;
  Frames         : TStringList;
  RootBone       : TAn8Bone;
  BoneMatrix     : Points3D.T4x4Matrix;
  BoneQuaternion : TQuaternion;
  Obj            : TAn8Object;
  MinPos         : T3DPoint;
  MaxPos         : T3DPoint;

  Procedure ProcessBone(FrameCount: Integer; FrameTime: Single; Frame: TSkeletonFrame; Bone: TAn8Bone; Sequence: TAn8Sequence; M: Points3D.T4x4Matrix; Q: TQuaternion);
  Var
    I,J,K       : Integer;
    Q1,Q0,Q2    : TQuaternion;
    M1          : T3x3Matrix;
    MA1         : Points3D.T4x4Matrix;
    Angle       : Single;
    AX,AY,AZ    : Single;
    Angle0      : Single;
    AX0,AY0,AZ0 : Single;
    ParentIndex : Integer;
    BX,BY,BZ    : Single;
    JointAngleX : TAn8JointAngle;
    JointAngleY : TAn8JointAngle;
    JointAngleZ : TAn8JointAngle;
    L           : Integer;
    V           : T3DPoint;

  Begin
    If Bone <> Nil Then
    Begin
      Q1  := TQuaternion.Create(Bone.Orientation);
      Q1.Normalize; // Important!!


      If Bone.Parent <> Nil Then
      Begin
        BX := 0;
        BY := 0;
        BZ := Bone.Parent.Len;
        If Bone.Index = 1 Then BZ := 0;
      End
      Else
      Begin
        BX := {ZO.Loc.X;//}0;
        BY := {ZO.Loc.Y;//}0;
        BZ := {ZO.Loc.Z;//}0;
      End;

      V := T3DPoint.Create;

      // Find the joint angles

      L := Sequence.JointAngles.IndexOf(Bone.Name + '_X');
      If L >= 0
       Then JointAngleX := TAn8JointAngle(Sequence.JointAngles.Objects[L])
       Else JointAngleX := Nil;

      L := Sequence.JointAngles.IndexOf(Bone.Name + '_Y');
      If L >= 0
       Then JointAngleY := TAn8JointAngle(Sequence.JointAngles.Objects[L])
       Else JointAngleY := Nil;

      L := Sequence.JointAngles.IndexOf(Bone.Name + '_Z');
      If L >= 0
       Then JointAngleZ := TAn8JointAngle(Sequence.JointAngles.Objects[L])
       Else JointAngleZ := Nil;

      Q0 := TQuaternion.Create(Bone.Orientation);

      If JointAngleX <> Nil Then
      Begin
        V.Copy(1,0,0);
        Q0.Transform(V);
        Q1.FromAngleAxis(JointAngleX.Track.GetValueForFrame(FrameCount) * Pi / 180,V);
        Q1.Multiply(Q0);
        Q0.Copy(Q1);
      End;

      If JointAngleZ <> Nil Then
      Begin
        V.Copy(0,0,1);
        Q0.Transform(V);
        Q1.FromAngleAxis(JointAngleZ.Track.GetValueForFrame(FrameCount) * Pi / 180,V);
        Q1.Multiply(Q0);
        Q0.Copy(Q1);
      End;

      If JointAngleY <> Nil Then
      Begin
        V.Copy(0,1,0);
        Q0.Transform(V);
        Q1.FromAngleAxis(JointAngleY.Track.GetValueForFrame(FrameCount) * Pi / 180,V);
        Q1.Multiply(Q0);
        Q0.Copy(Q1);
      End;

      Q1.Copy(Q0.W,Q0.Z,Q0.X,Q0.Y);
      Q1.Normalize;
      Q0.Free;
      V.Free;

//      Q1.Transform(BX,BY,BZ);

      M1  := Q1.ToRotationMatrix;
      MA1 := Points3D.T4x4Matrix.Create;
      MA1.LoadIdentity;
      MA1.LoadRotationMatrix(M1);
      M1.Free;
      MA1.M[1,4] := BX;
      MA1.M[2,4] := BY;
      MA1.M[3,4] := BZ;

      MA1.Multiply(M);

      Q0 := TQuaternion.Create(Q);
      Q0.Normalize; // Important!!
      Q0.Multiply(Q1);
      Q2 := TQuaternion.Create(Q0);
//      Q1.Copy(Q0);
      Q0.Free;

      Q2.ToAngleAxis(Angle,AX,AY,AZ);      // Cumulative
      Q1.ToAngleAxis(Angle0,AX0,AY0,AZ0);  // Relative to last bone


      Angle  := Angle  * 180 / Pi; // Convert to degrees because OpenGL uses degrees
      Angle0 := Angle0 * 180 / Pi; // Convert to degrees because OpenGL uses degrees

      If Bone.Parent <> Nil
       Then ParentIndex := Bone.Parent.Index
       Else ParentIndex := -1;

      Frame.SetPiece(Bone.Index,Bone.Name,
                     MA1.M[1,4],MA1.M[2,4],MA1.M[3,4],
                     MA1.M[1,1],MA1.M[1,2],MA1.M[1,3],
                     MA1.M[2,1],MA1.M[2,2],MA1.M[2,3],
                     MA1.M[3,1],MA1.M[3,2],MA1.M[3,3],
                     Q2.W,Q2.X,Q2.Y,Q2.Z,
                     Q1.W,Q1.X,Q1.Y,Q1.Z,
                     Angle,AX,AY,AZ,
                     MA1.M[1,4],MA1.M[2,4],MA1.M[3,4],
                     Angle0,AX0,AY0,AZ0,
                     BX,
                     BY,
                     BZ,
                     ParentIndex);

      For J := 0 To Bone.Children.Count - 1 Do
      Begin
        ProcessBone(FrameCount,FrameTime,Frame,TAn8Bone(Bone.Children.Objects[J]),Sequence,MA1,Q2);
      End; // For J
      Q1.Free;
      Q2.Free;
      MA1.Free;
    End;
  End; // ProcessBone

Begin
  If An8File.Objects.Count > 0 Then
  Begin
    Obj := TAn8Object(An8File.Objects.Objects[0]);

    Model := Nil;
    Entity.Name := 'Loaded An8 object';
    If Entity.Renderable <> Nil Then
    Begin
  //          If Not HasAnimations Then LoadedRenderables.AddObject('',Entity.Renderable);
      If TRenderable(Entity.Renderable).Count > 0 Then Model := TRenderable(Entity.Renderable).Models[0];
    End;

    // Load the other renderables (animations)

    // First look for a sequence with prefix P01

    J := -1;
    I := 0;
    While (I < An8File.Sequences.Count) And (J < 0) Do
    Begin
      Sequence := TAn8Sequence(An8File.Sequences.Objects[I]);
      If (Sequence <> Nil) And (Sequence.Figure <> Nil) And TokenizeAn8SequenceName(Sequence.Name,SeqName,SeqPrefix,DropLastFrame,MSPerFrame) Then
      Begin
        If UpperCase(SeqPrefix) = 'P01'
         Then J := I
         Else Inc(I); 
      End
      Else Inc(I);
    End; // While

    // If we didn't find a P01 sequence, just use the first one

    If J < 0 Then J := 0;

    // Continue if we have at least one sequence

    If J < An8File.Sequences.Count Then
    Begin
      I := J;
      Sequence := TAn8Sequence(An8File.Sequences.Objects[I]);
      If (Sequence <> Nil) And (Sequence.Figure <> Nil) And TokenizeAn8SequenceName(Sequence.Name,SeqName,SeqPrefix,DropLastFrame,MSPerFrame) Then
      Begin
        If Sequence.Figure.ReferencesObject(Obj) Then
        Begin
          SR := TSkeletonRenderable(View.Scene3D.Scene.Renderables.GetNew(1));
          If Model <> Nil Then SR.AddModel('body0',Model);
          Frames := TStringList.Create;
          For J := 0 To Sequence.Frames - 1 Do
          Begin
            RootBone := Sequence.Figure.RootBone;
            K := 0;
            RootBone.AssignIndex(K);
            Frames.AddObject('',TSkeletonFrame.Create(SR));
            SR.AddFrame(TSkeletonFrame(Frames.Objects[Frames.Count - 1]));
            BoneMatrix := Points3D.T4x4Matrix.Create;
            BoneMatrix.LoadIdentity;
            BoneQuaternion := TQuaternion.Create;

            ProcessBone(J,J / Sequence.Frames,TSkeletonFrame(Frames.Objects[Frames.Count - 1]),RootBone,Sequence,BoneMatrix,BoneQuaternion);

            BoneQuaternion.Free;
            BoneMatrix.Free;
          End; // For J
          If Sequence.Frames > 1 Then SR.AnimSpeed := 1 / (Sequence.Frames * MSPerFrame / 1000);
          //For J := 0 To Frames.Count - 1 Do SR.AddFrame(TSkeletonFrame(Frames.Objects[J]));
          Frames.Free;
  //            LoadedRenderables.AddObject('',SR);

          // Get rid of the existing renderable because we're going to replace it with the TSkeletonRenderable we created

          View.Scene3D.Scene.Renderables.FreeItem(Entity.Renderable);

          // Point the entity to the new renderable

          Entity.Renderable := SR;

          // Entities that use skeleton renderables don't have their position, rotation, or scale in their mesh.
          // Instead, set those things in the entity itself.

          Entity.Position.Copy(ZO.Loc.X,ZO.Loc.Y,ZO.Loc.Z);
          Entity.Scale.Copy(ProgramSettings.XWFCreatureSize,ProgramSettings.XWFCreatureSize,ProgramSettings.XWFCreatureSize);
          Entity.Rotation.Copy(ZO.Rotate.XAngle,ZO.Rotate.YAngle,ZO.Rotate.ZAngle);
          Entity.Update; // Critical: this ensures that Entity.GetExtents creates the correct bounding box by making sure that its transformation matrix is recalculated

          Entity.GetExtents;
          MinPos := T3DPoint.Create(Entity.UnadjustedBox.MinPt);
          MaxPos := T3DPoint.Create(Entity.UnadjustedBox.MaxPt);
          MinPos.Multiply(ProgramSettings.XWFCreatureSize);
          MaxPos.Multiply(ProgramSettings.XWFCreatureSize);
          MinPos.Add(ZO.Loc);
          MaxPos.Add(ZO.Loc);
          If ZO Is TCreatureLibraryObjectReference Then TCreatureLibraryObjectReference(ZO).SetSize(MinPos,MaxPos);
          MinPos.Free;
          MaxPos.Free;
          If ZO Is TCreatureLibraryObjectReference Then TCreatureLibraryObjectReference(ZO).RespondToGravity;
        End;
      End;
    End;
  End;
End;

procedure TfrmMain.tvMeshesChange(Sender: TObject; Node: TTreeNode);
Var
  I,J,K         : Integer;
  GO            : TGroupObject;
  Mesh          : TMeshObject;
  List          : TStringList;
  Light         : TLight;
  Entity        : TEntity;
  Tree          : TTree;
  An8File       : TAn8File;
  Obj           : TAn8Object;
  ZO            : TZoneObject;
  HasAnimations : Boolean;
  V             : T3DPoint;
  V1            : T3DPoint;
  Sequence      : TAn8Sequence;
  NO            : TAn8NamedObject;
  Bones         : TStringList;
  Q             : TQuaternion;
  Q1            : TQuaternion;
  Bone          : TAn8Bone;
  Bone1         : TAn8Bone;

begin
  If (tvMeshes.SelectionCount = 1) And
     (tvMeshes.Selections[0] <> Nil) And
     (tvMeshes.Selections[0].Data <> Nil) And
     Not LoadingMeshLibraryList Then
  Begin
    glvMesh.Scene3D.Scene.LockBSPTree('TfrmMain.tvMeshesChange');

    SelectedMesh     := '';
    SelectedCreature := '';

    glvMesh.Scene3D.Scene.bTranslucentMode := True;

    // Get rid of all entities, models, and renderables

    glvMesh.Scene3D.Scene.ClearScene(True,True);
    glvMesh.Clear;

    If TObject(tvMeshes.Selections[0].Data) Is TGroupObject Then
    Begin
      GO           := TGroupObject(tvMeshes.Selections[0].Data);
      SelectedMesh := GO.GetName;
      List         := TStringList.Create;
      GO.AddToPolygonList(List,False,True,True,True);

      For J := 0 To List.Count - 1 Do
      Begin
        Mesh := TMeshObject(List.Objects[J]);
        Tree := Nil;
        frmMain.ReloadMesh(List,0,J,False,False,False,glvMesh,Tree,False,ExtractFilePath(Application.ExeName) + 'library\textures\');
        Tree.Free;
        Mesh.Free;
      End; // For J
      List.Free;

    End
    Else If TObject(tvMeshes.Selections[0].Data) Is TAn8File Then
    Begin
      frmStatus.Show;
      frmStatus.SetCaption('Loading creature model...');
      SelectedCreature := tvMeshes.Selections[0].Text;
      An8File := TAn8File(tvMeshes.Selections[0].Data);
      If Not An8File.Loaded Then
      Begin
        An8File.LoadFromFile(ExtractFilePath(Application.EXEName) + 'library\creatures\' + tvMeshes.Selections[0].Text + '.an8');
        For I := 0 To An8File.Objects.Count - 1 Do
        Begin
          Obj := TAn8Object(An8File.Objects.Objects[I]);
          For J := 0 To Obj.Components.Count - 1 Do TAn8Component(Obj.Components.Objects[J]).ConvertToTriangles;
          Obj.MergeVeryClosePoints;
        End; // For I
        For I := 0 To An8File.Figures.Count - 1 Do TAn8Figure(An8File.Figures.Objects[I]).DeterminePrimaryBones;
      End;
      frmStatus.Hide;

      If An8File.Objects.Count > 0 Then
      Begin
        Obj := TAn8Object(An8File.Objects.Objects[0]);
        List := TStringList.Create;
        List.AddObject('',Obj);
        ZO := Zone.ImportAn8Objects('an8',An8File,List,True,False);
        List.Clear;
        HasAnimations := False;
        Entity := Nil;
        If ZO <> Nil Then
        Begin
          ZO.AddToPolygonList(List,False,True,True,True);
          Mesh := Zone.CoalescePolygonList(List);
          For I := 0 To List.Count - 1 Do List.Objects[I].Free;

          // Subtract the bone origin from all points

          V := T3DPoint.Create;
          If An8File.Sequences.Count > 0 Then
          Begin
            Sequence := TAn8Sequence(An8File.Sequences.Objects[0]);
            If (Sequence <> Nil) And (Sequence.Figure <> Nil) And (Sequence.Figure.RootBone <> Nil) Then
            Begin
              HasAnimations := True;
              NO := TAn8NamedObject(Sequence.Figure.RootBone.FindNamedObjects.Objects[0]);

              Bones := TStringList.Create;
              Sequence.Figure.RootBone.GetBones(Bones);
              Q  := TQuaternion.Create;
              Q1 := TQuaternion.Create;
              For I := 0 To Mesh.Vertices.Count - 1 Do
              Begin
                V1 := T3DPoint(Mesh.Vertices.Objects[I]);
                If (High(Mesh.BoneIndices) >= I) And (Mesh.BoneIndices[I] >= 0) And (Mesh.BoneIndices[I] < Bones.Count) Then
                Begin
                  Bone := TAn8Bone(Bones.Objects[Mesh.BoneIndices[I]]);
                  V.Copy(V1.Y,V1.Z,V1.X);
                  V.Subtract(Bone.Origin);

                  If NO <> Nil Then V.Add(NO.Base);
                  Q.Copy(Bone.Orientation);
                  Bone1 := Bone.Parent;
                  While Bone1 <> Nil Do
                  Begin
                    Q1.Copy(Bone1.Orientation);
                    Q1.Multiply(Q);
                    Q.Copy(Q1);
                    Bone1 := Bone1.Parent;
                  End; // While
                  Q.Invert;
                  Q.Transform(V);

                  V1.Copy(V.Z,V.X,V.Y);
                End;
              End; // For I
              Q.Free;
              Q1.Free;
              Bones.Free;
            End;
          End;
          V.Free;

          List.Clear;
          List.AddObject('',Mesh);
          Tree := Nil;
          Entity := frmMain.ReloadMesh(List,0,0,False,False,False,glvMesh,Tree,False,ExtractFilePath(Application.ExeName) + 'library\creatures\');
          Tree.Free;
          Mesh.Free;
          ZO.Free;
        End;
        List.Free;

        // Make a list of all renderables so we can switch between them

  //      LoadedRenderables.Clear;
  //      AdjustedSequence := Nil;
  //      SelectedFrame := -1;


        If Entity <> Nil Then
        Begin
          ZO := TMeshObject.Create;
          LoadCreatureSkeleton(An8File,glvMesh,Entity,ZO);  //glvMesh.Scene3D.Scene.Entities.Count > FirstEntity Then
          ZO.Free;
        End;
      End;  
    End;

    glvMesh.Scene3D.Scene.DefaultCamera.LookAt(0,0,-100);
    glvMesh.Scene3D.Scene.DefaultCamera.SetVectorUp(1,0,0);
    glvMesh.Scene3D.Scene.DefaultCamera.SetPosition(0,0,8{00});

    If glvMesh.Scene3D.Scene.Lights.Count > 0 Then
    Begin
      Light := TLight(glvMesh.Scene3D.Scene.Lights.Items[0]);
      Light.Position.Copy(0,0,100000);
      Light.SetOrientation(1,1,0);
      Light.Attenuation := 0;//.001;
    End;

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

    glvMesh.Scene3D.Redraw;
  End
  Else
  Begin
    SelectedMesh     := '';
    SelectedCreature := '';
  End;
end;

procedure TfrmMain.acEditMeshLibraryObjectCategoryExecute(Sender: TObject);
Var
  St    : String;
  GO    : TGroupObject;
  I     : Integer;
  F     : System.Text;
  Found : Boolean;
  TN    : TTreeNode;
  Child : TTreeNode;

  Procedure FindData(TN: TTreeNode; Var Found: Boolean; P: Pointer);
  Var I: Integer;
  Begin
    While (TN <> Nil) And Not Found Do
    Begin
      If TN.Data = P Then
      Begin
        Found := True;
        tvMeshes.Select(TN);
      End
      Else FindData(TN.getFirstChild,Found,P);
      TN := TN.GetNext;
    End; // While
  End; // FindData

begin
  If (tvMeshes.Selections[0] <> Nil) And
     (tvMeshes.Selections[0].Data <> Nil) Then
  Begin
    If TObject(tvMeshes.Selections[0].Data) Is TGroupObject Then
    Begin
      GO := TGroupObject(tvMeshes.Selections[0].Data);
      St := GO.Category;
      If InputQuery('Change Category','Enter new library object category:',St) Then
      Begin
        GO.Category := Trim(St);
        AssignFile(F,ExtractFilePath(Application.EXEName) + 'library\meshes\' + GO.GetName + '.msh');
        ReWrite(F);
        GO.SaveToFile(F,0);
        CloseFile(F);
        LoadMeshLibraryList(tvMeshes);

        // Try to re-select the mesh object

        Found := False;
        TN    := tvMeshes.TopItem;
        FindData(TN,Found,Pointer(GO));
      End;
    End
    Else ShowMessage('You can only change the category for mesh objects, not creatures.');
  End;
end;

procedure TfrmMain.tvMeshesCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; var DefaultDraw: Boolean);
Var
  ImageRect1 : TRect;
  ImageRect2 : TRect;
  NewRect    : TRect;
  ResRect    : TRect;
  H          : Integer;

begin
  Exit;
{
  If tvMeshes.Items.Count > 0 Then
  Begin
    ImageRect1 := tvMeshes.Items.GetNode(TreeView_GetFirstVisible(tvMeshes.Handle)).DisplayRect(False);
    ImageRect2 := tvMeshes.Items.GetNode(TreeView_GetLastVisible(tvMeshes.Handle)).DisplayRect(False);
    H          := ImageRect2.Bottom - ImageRect1.Top;
  End
  Else H := 0;
  StretchBlt(tvMeshes.Canvas.Handle,ARect.Left,ARect.Top,ARect.Right - ARect.Left,ARect.Bottom - ARect.Top,
             Image4.Canvas.Handle,0,Round(ARect.Top * Image4.Height / H),
             Image4.Width,
             Round((ARect.Bottom - ARect.Top) * Image4.Height / H),SRCCOPY);
}

  StretchBlt(tvMeshes.Canvas.Handle,ARect.Left,ARect.Top,ARect.Right - ARect.Left,ARect.Bottom - ARect.Top,
             Image4.Canvas.Handle,0,Round((ARect.Top) * Image4.Height / tvMeshes.Height),
             Image4.Width,
             Round((ARect.Bottom - ARect.Top) * Image4.Height / tvMeshes.Height),SRCCOPY);

end;

procedure TfrmMain.tvMeshesCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
Var
  Rect : TRect;
  iSbh : integer;
{
  ImageRect1 : TRect;
  ImageRect2 : TRect;
  H          : Integer;
}
begin
Exit;
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
{
  Rect := Node.DisplayRect(False);
  If tvMeshes.Items.Count > 0 Then
  Begin
    ImageRect1 := tvMeshes.Items.GetNode(TreeView_GetFirstVisible(tvMeshes.Handle)).DisplayRect(False);
    ImageRect2 := tvMeshes.Items.GetNode(TreeView_GetLastVisible(tvMeshes.Handle)).DisplayRect(False);
    H          := ImageRect2.Bottom - ImageRect1.Top;
  End
  Else H := 0;
  StretchBlt(tvMeshes.Canvas.Handle,
             Rect.Left,Rect.Top,Rect.Right - Rect.Left,Rect.Bottom - Rect.Top,
             Image4.Canvas.Handle,0,Round(Rect.Top * Image4.Height / H),
             Image4.Width,
             Round((Rect.Bottom - Rect.Top) * Image4.Height / H),SRCCOPY);
}
{
  Rect := Node.DisplayRect(False);
  StretchBlt(tvMeshes.Canvas.Handle,
             Rect.Left,Rect.Top,Rect.Right - Rect.Left,Rect.Bottom - Rect.Top,
             Image4.Canvas.Handle,
             0,
             Round(Rect.Top * Image4.Height / tvMeshes.Height),
             Image4.Width,
             Round((Rect.Bottom - Rect.Top) * Image4.Height / tvMeshes.Height),SRCCOPY);
}
  Rect := Node.DisplayRect(True);
  Rect.Right := Rect.Left + tvMeshes.Width - 4; // Need to expand the width because the text is wider when my fake ClearType is on




  DataModule1.ClearTypeText.ExtTextOut(Rect.Left,Rect.Top,Node.Text,ETO_CLIPPED,tvMeshes.Canvas,tvMeshes.Canvas.Brush.Color,@Rect);

//  tvMeshes.Canvas.TextOut(Rect.Left,Rect.Top,Node.Text);
  DefaultDraw := False;

  iSbh := GetScrollPos(Sender.Handle,SB_HORZ);
  Rect := Node.DisplayRect(False);
  Rect.Left := Rect.Left - iSbh;
  Rect.Left := Rect.Left + ((Node.Level+1) * TreeView_GetIndent(Sender.Handle));

  If Node.HasChildren Then
  Begin
    If Node.Expanded
     Then ilTreeView.Draw(Sender.Canvas,Rect.Left - 16,Rect.Top,1)
     Else ilTreeView.Draw(Sender.Canvas,Rect.Left - 16,Rect.Top,0);
  End;
end;

procedure TfrmMain.cbShowLightSourcesChange(Sender: TObject);
begin
  ProgramSettings.ShowLightSources := cbShowLightSources.ItemIndex;
  SavePreferences;
  AddLightSpheres;
  RenderZone(BirdsEye);
end;

procedure TfrmMain.acInsertModelOriginExecute(Sender: TObject);
Var
  MO  : TModelOrigin;
  St  : String;
  I   : Integer;

Begin
  St := 'modelorigin';
  If St <> '' Then
  Begin
    // Find a unique name

    I := 1;
    While Zone.NameExists(St + IntToStr(I)) Do Inc(I);
    St := St + IntToStr(I);
    If InputQuery('Add model origin','Enter new object name:',St) Then
    Begin
      St := Trim(St);
      If St <> '' Then
      Begin
        If Not Zone.NameExists(St) Then
        Begin
          MO := TModelOrigin.Create(St);
          MO.Loc.X := Observer.X + CreateDist * Cos(Phi);
          MO.Loc.Y := Observer.Y + CreateDist * Sin(Phi);
          MO.Loc.Z := Observer.Z;

          // Add the object

          AddNewObject(St,MO);
        End
        Else ShowMessage('An object with that name already exists. Please choose a different one.')
      End
      Else ShowMessage('You cannot enter a blank name.');
    End;
    glView.scene3D.Redraw;
  End;
end;

procedure TfrmMain.cbShowModelOriginsChange(Sender: TObject);
begin
  ProgramSettings.ShowModelOrigins := cbShowModelOrigins.ItemIndex;
  SavePreferences;
  AddModelOrigins;
  RenderZone(BirdsEye);
end;

procedure TfrmMain.acSetObjectPosToCrosshairExecute(Sender: TObject);
Var
  TN   : TTreeNode;
  ZO   : TZoneObject;

begin
  Try
    EnableGUI(False);
    If Not LoadingParms Then
    Begin
      If tvMain.SelectionCount = 1 Then
      Begin
        TN := tvMain.Selections[0];
        If TN <> Nil Then
        Begin
          ZO := TZoneObject(TN.Data);
          If ZO.GetParent = Nil Then
          Begin
            ZO.Loc.Copy(Observer);
            ZO.Loc.Add(CreateDist * Cos(Phi),CreateDist * Sin(Phi),0{-ObsHeight});
            LoadParameters;
            ZPropList1.CurObj := Nil; // Force it to reload
            ZPropList1.CurObj := ZO;
            RefreshObject(ZO,True,True,True);

            If (ZO Is TMeshLibraryObjectReference) And
               TMeshLibraryObjectReference(ZO).Gravity Then ShowMessage('Gravity is ON for this object');
          End
          Else ShowMessage('Cannot perform this operation on a grouped object.');
        End;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acRotateObjectLeftExecute(Sender: TObject);
Var
  I,J  : Integer;
  ZO   : TZoneObject;
  List : TStringList;

begin
  List := TStringList.Create;
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    ZO := TZoneObject((tvMain.Selections[I]).Data);
    If Not ZO.IsSpecialObject Then
    Begin
      J  := Zone.GetObjectIndex(ZO);
      ZO.Rotate.ZAngle := ZO.Rotate.ZAngle + ProgramSettings.SmallRotateAmount;
      If Abs(Round(ZO.Rotate.ZAngle * 10) - (ZO.Rotate.ZAngle * 10)) < 0.01 Then ZO.Rotate.ZAngle := Round(ZO.Rotate.ZAngle * 10) / 10;
      While ZO.Rotate.ZAngle < 0 Do ZO.Rotate.ZAngle := ZO.Rotate.ZAngle + 360;
      While ZO.Rotate.ZAngle >= 360 Do ZO.Rotate.ZAngle := ZO.Rotate.ZAngle - 360;
      List.Clear;
      ZO.AddToPolygonList(List,False,False,True,True);
      MasterMeshList.Objects[J].Free;
      MasterMeshList.Objects[J] := List.Objects[0];
      RefreshObject(ZO,True,True,True);
      If I = 0 Then
      Begin
        ZPropList1.CurObj := Nil;
        ZPropList1.CurObj := ZO;
      End;
    End;
  End; // For I
  List.Free;
end;

procedure TfrmMain.acRotateObjectRightExecute(Sender: TObject);
Var
  I,J  : Integer;
  ZO   : TZoneObject;
  List : TStringList;

begin
  List := TStringList.Create;
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    ZO := TZoneObject((tvMain.Selections[I]).Data);
    If Not ZO.IsSpecialObject Then
    Begin
      J  := Zone.GetObjectIndex(ZO);
      ZO.Rotate.ZAngle := ZO.Rotate.ZAngle - ProgramSettings.SmallRotateAmount;
      If Abs(Round(ZO.Rotate.ZAngle * 10) - (ZO.Rotate.ZAngle * 10)) < 0.01 Then ZO.Rotate.ZAngle := Round(ZO.Rotate.ZAngle * 10) / 10;
      While ZO.Rotate.ZAngle < 0 Do ZO.Rotate.ZAngle := ZO.Rotate.ZAngle + 360;
      While ZO.Rotate.ZAngle >= 360 Do ZO.Rotate.ZAngle := ZO.Rotate.ZAngle - 360;
      List.Clear;
      ZO.AddToPolygonList(List,False,False,True,True);
      MasterMeshList.Objects[J].Free;
      MasterMeshList.Objects[J] := List.Objects[0];
      RefreshObject(ZO,True,True,True);
      If I = 0 Then
      Begin
        ZPropList1.CurObj := Nil;
        ZPropList1.CurObj := ZO;
      End;
    End;
  End; // For I
  List.Free;
end;

procedure TfrmMain.acImportAN8Execute(Sender: TObject);
Var Rotate: Boolean;
begin
  Try
    EnableGUI(False);
    dlgOpen.FileName   := '';
    dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
    dlgOpen.Filter     := 'Anim8or files (*.an8)|*.AN8';
    If dlgOpen.Execute Then
    Begin
      frmStatus.Show;
      frmStatus.SetCaption('Importing .AN8 zone');
      Rotate := (Application.MessageBox('Is the Anim8or model built such that Y is the up direction?',
                                        'Question',
                                         MB_YESNO) = IDYES);
      ImportFromAN8(dlgOpen.FileName,Rotate);
      frmStatus.Hide;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acSplitSelectedMeshesExecute(Sender: TObject);
Var
  Meshes : TStringList;
  I,J    : Integer;
  TN     : TTreeNode;
  ZO     : TZoneObject;
  MO     : TMeshObject;
  MO1    : TMeshObject;
  Tree   : TTree;

  Procedure AddMeshes(Base: String; Region: TRegion; Var Index: Integer);
  Var
    MO : TMeshObject;
    I  : Integer;
    P  : TPolygon;
    St : String;

  Begin
    If Region <> Nil Then
    Begin
      If (Region.Left = Nil) And (Region.Right = Nil) Then
      Begin
        If High(Region.Polygons) >= 0 Then
        Begin
          MO := TMeshObject.Create;
          For I := 0 To High(Region.Polygons) Do
          Begin
            P  := TPolygon(Region.Mesh.Polygons.Objects[Region.Polygons[I]]);
            MO.AddCopyOfPolygon(P,Region.Mesh);
          End; // For I
          MO.ConvertToTriangles;
          St := Base;
          While Zone.NameExists(St + IntToStr(Index)) Do Inc(Index);
          St := St + IntToStr(Index);
          Zone.AddObject(St,MO);
          MO.SetName(St);
          MO.SetZone(Zone);
  //        AddNewObject(St,MO);
        End;
      End
      Else
      Begin
        AddMeshes(Base,Region.Left,Index);
        AddMeshes(Base,Region.Right,Index);
      End;
    End;
  End; // AddMeshes

begin
  Try
    EnableGUI(False);
    If Application.MessageBox('Split selected mesh objects with a grid? (This cannot be undone!)','Confirm',MB_OKCANCEL) = IDOK Then
    Begin
      Meshes := TStringList.Create;
      For I := 0 To tvMain.SelectionCount - 1 Do
      Begin
        TN := tvMain.Selections[I];
        If TN <> Nil Then
        Begin
          ZO := TZoneObject(TN.Data);
          If ZO Is TMeshObject Then Meshes.AddObject('',ZO);
        End;
      End; // For I
      If Meshes.Count > 0 Then
      Begin
        For I := 0 To Meshes.Count - 1 Do
        Begin
          MO   := TMeshObject(Meshes.Objects[I]);
          Tree := TTree.Create(MO,False);
          Tree.SplitAlongGrid(1024,1024 * 3);
          If (Tree.Root.Left <> Nil) Or (Tree.Root.Right <> Nil) Then
          Begin
            J := 0;
            AddMeshes(MO.GetName,Tree.Root,J);
            If MO.GetParent <> Nil
             Then MO.GetParent.Objects.Delete(MO.GetParent.Objects.IndexOfObject(MO))
             Else Zone.Delete(Zone.IndexOfObject(MO));
            MO.Free;
          End;
          Tree.Free;
        End; // For I
        SelectedPolygons.Clear;
        SelectedPolyMesh := Nil;
        LoadObjectTreeView;
        ReloadZone;
        RenderZone(BirdsEye);
        If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
        SetViewToNorth;
        DisplayStats;
      End;
      Meshes.Free;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acConvertAN8ToMshExecute(Sender: TObject);
Var
  I     : Integer;
  Found : Boolean;
  St    : String;

  Procedure ConvertFile(FileName: String; Prompt,MakeCompliant: Boolean);
  Var
    I       : Integer;
    ZO      : TZoneObject;
    GO      : TGroupObject;
    MO      : TMeshObject;
    NewName : String;
    St      : String;
    Found   : Boolean;
    GoAhead : Boolean;
    F       : System.Text;
    Rotate  : Boolean;

  Begin
    Rotate := (Application.MessageBox('Is the Anim8or model built such that Y is the up direction?',
                                      'Question',
                                       MB_YESNO) = IDYES);
    ZO := Zone.ImportObjectFromAn8File(FileName,Rotate,False);
    If ZO <> Nil Then
    Begin
      NewName := ZO.GetName;
      If Prompt
       Then GoAhead := InputQuery('Export Object as Mesh','Enter mesh name:',NewName)
       Else GoAhead := True;
      NewName := Trim(NewName);
      If GoAhead Then
      Begin
        If NewName <> '' Then
        Begin
          If MakeCompliant Then
          Begin
            St := Trim(UpperCase(NewName));
            If (Copy(St,1,2) = 'IT') And (Length(St) > 2) And (St[3] In ['0'..'9']) Then
            Begin
              If Copy(St,Length(St) - 2,3) <> 'DEF' Then
              Begin
                St      := St + '_DMSPRITEDEF';
                NewName := St;
              End;
            End;
          End;
          Found := (MeshLibraryObjectIndex(NewName) >= 0);
          If Found And Prompt Then
          Begin
            GoAhead := (Application.MessageBox('An object with that name already exists.'#13#10 +
                                      'Overwrite it?',
                                      'Warning',
                                      MB_OKCANCEL) = IDOK);
          End
          Else GoAhead := True;
          If GoAhead Then
          Begin
            // Set the object to the desired name

            ZO.SetName(NewName);

            // Form a group object from what we have selected (we either have a
            // mesh object or a group object)

            If (ZO Is TMeshObject) And (TMeshObject(ZO).Vertices.Count > 0) Then
            Begin
              If Found Then
              Begin
                GO := TGroupObject(MeshLibrary.Objects[MeshLibraryObjectIndex(NewName)]);
                For I := 0 To GO.Objects.Count - 1 Do GO.Objects.Objects[I].Free;
                GO.Objects.Clear;
              End
              Else GO := TGroupObject.Create(ZO.GetName);
              GO.Objects.AddObject(ZO.GetName,ZO);
            End
            Else GO := TGroupObject(ZO);

            // Save the group to the mesh library

            GO.SetName(NewName);
            AssignFile(F,ExtractFilePath(Application.EXEName) + 'library\meshes\' + NewName + '.msh');
            ReWrite(F);
            GO.SaveToFile(F,0);
            CloseFile(F);

            // Add the new group to the library in memory

            If Not Found Then MeshLibrary.AddObject(GO.GetName,GO);
          End;
        End
        Else ShowMessage('You cannot enter a blank name.');
      End;
    End;
  End; // ConvertFile

begin
  Try
    EnableGUI(False);
    dlgOpen.FileName   := '';
    dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
    dlgOpen.Filter     := 'Anim8or files (*.an8)|*.AN8';
    dlgOpen.Options    := dlgOpen.Options + [ofAllowMultiSelect];
    If dlgOpen.Execute Then
    Begin
      // See if the objects might conform to ITxxxx type.  If any do, prompt the user to see if
      // OpenZone should ensure that the names fully conform.

      I     := 0;
      Found := False;
      While (I < dlgOpen.Files.Count) And Not Found Do
      Begin
        St := ExtractFileNameNoExt(Trim(UpperCase(dlgOpen.Files.Strings[I])));
        If (Copy(St,1,2) = 'IT') And (Length(St) > 2) And (St[3] In ['0'..'9'])
         Then Found := True
         Else Inc(I);
      End; // While

      If Found Then
      Begin
        If Application.MessageBox('Some of the names appear to be possible equipment names.'#13#10 +
                                  'Do you want OpenZone to modify them to ensure they are'#13#10 +
                                  'fully compliant with the client?',
                                  'Question',
                                  MB_YESNO) <> IDYES Then Found := False;
      End;

      For I := 0 To dlgOpen.Files.Count - 1 Do ConvertFile(dlgOpen.Files.Strings[I],dlgOpen.Files.Count = 1,Found);
    End;

    // Cleanup

    LoadObjectTreeView;
    MeshLibrary.Sort;
    LoadMeshLibraryList(tvMeshes);
  Finally
    dlgOpen.Options := dlgOpen.Options - [ofAllowMultiSelect];
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acSizeUp10PercentExecute(Sender: TObject);
Var
  I,J  : Integer;
  ZO   : TZoneObject;
  List : TStringList;

begin
  List := TStringList.Create;
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    ZO := TZoneObject((tvMain.Selections[I]).Data);
    If Not ZO.IsSpecialObject Then
    Begin
      J  := Zone.GetObjectIndex(ZO);
      ZO.Size.Multiply(1.1);
      List.Clear;
      ZO.AddToPolygonList(List,False,False,True,True);
      MasterMeshList.Objects[J].Free;
      MasterMeshList.Objects[J] := List.Objects[0];
      RefreshObject(ZO,True,True,True);
      If I = 0 Then
      Begin
        ZPropList1.CurObj := Nil;
        ZPropList1.CurObj := ZO;
      End;
    End;
  End; // For I
  List.Free;
end;

procedure TfrmMain.acSizeDown10PercentExecute(Sender: TObject);
Var
  I,J  : Integer;
  ZO   : TZoneObject;
  List : TStringList;

begin
  List := TStringList.Create;
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    ZO := TZoneObject((tvMain.Selections[I]).Data);
    If Not ZO.IsSpecialObject Then
    Begin
      J  := Zone.GetObjectIndex(ZO);
      ZO.Size.Multiply(1 / 1.1);
      List.Clear;
      ZO.AddToPolygonList(List,False,False,True,True);
      MasterMeshList.Objects[J].Free;
      MasterMeshList.Objects[J] := List.Objects[0];
      RefreshObject(ZO,True,True,True);
      If I = 0 Then
      Begin
        ZPropList1.CurObj := Nil;
        ZPropList1.CurObj := ZO;
      End;
    End;
  End; // For I
  List.Free;
end;

procedure TfrmMain.acToggleInsertMeshesIntoZoneGeometryExecute(
  Sender: TObject);
Var
  I  : Integer;
  ZO : TZoneObject;

begin
  For I := 0 To tvMain.SelectionCount - 1 Do
  Begin
    ZO := TZoneObject((tvMain.Selections[I]).Data);
    If ZO Is TMeshLibraryObjectReference Then TMeshLibraryObjectReference(ZO).InsertMesh := Not TMeshLibraryObjectReference(ZO).InsertMesh;
  End; // For I
end;

procedure TfrmMain.acConvertToMeshExecute(Sender: TObject);
Var
  I,J   : Integer;
  TN    : TTreeNode;
  ZO    : TZoneObject;
  List  : TStringList;
  MO    : TMeshObject;
  GO    : TGroupObject;
  V     : T3DPoint;

begin
  Try
    EnableGUI(False);
    If Application.MessageBox('Convert selected objects to meshes? (This cannot be undone!)','Confirm',MB_OKCANCEL) = IDOK Then
    Begin
      If Not LoadingParms Then
      Begin
        ZPropList1.CurObj := Nil;
        Application.ProcessMessages;
        I := 0;
        While I < tvMain.SelectionCount Do
        Begin
          TN := tvMain.Selections[I];
          If TN <> Nil Then
          Begin
            ZO := TZoneObject(TN.Data);
            If Not (ZO Is TMeshObject) Then
            Begin
              // Create a mesh object from the currently selected object, ignoring mesh references

              List := TStringList.Create;
              ZO.AddToPolygonList(List,False,True,False,True);
              MO := Zone.CoalescePolygonList(List);

              // Skip objects with no polygons (lights, model origins, etc.)

              If MO.Polygons.Count > 0 Then
              Begin
                MO.SetName(ZO.GetName);

                // The coordinates in the mesh are relative to the zone.  We need to change the mesh vertices
                // so the coordinates are relative only to the mesh.

                V := ZO.GetAbsoluteLocation;
                For J := 0 To MO.Vertices.Count - 1 Do T3DPoint(MO.Vertices.Objects[J]).Subtract(V);
                V.Free;
                MO.Loc.Copy(ZO.Loc);

                // Insert the new mesh object after the current one and get rid of the original object

                GO := ZO.GetParent;
                MO.SetParent(GO);
                If GO <> Nil Then
                Begin
                  J := GO.Objects.IndexOfObject(ZO);
                  If J >= 0 Then   // Should always be true
                  Begin
                    GO.Objects.InsertObject(J + 1,MO.GetName,MO);
                    GO.Objects.Delete(J);
                    ZO.Free;
                  End;
                End
                Else
                Begin
                  J := Zone.IndexOfObject(ZO);
                  If J >= 0 Then   // Should always be true
                  Begin
                    Zone.InsertObject(J + 1,MO.GetName,MO);
                    Zone.Delete(J);
                    ZO.Free;
                  End;
                End;

                // Cleanup

                For J := 0 To List.Count - 1 Do List.Objects[J].Free;
                List.Clear;

                J := Zone.GetObjectIndex(MO);
                MO.AddToPolygonList(List,False,True,True,True);
                MasterMeshList.Objects[J].Free;
                MasterMeshList.Objects[J] := List.Objects[0];

                List.Free;
                MeshLibrary.Sort;
              End;
            End;
          End;
          Inc(I);
        End; // While
        LoadObjectTreeView;
        RegenerateGroundMeshes;
        ReloadZone;
        RenderZone(BirdsEye);
        If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acSplitMeshesExecute(Sender: TObject);
Var
  I,J,K,L    : Integer;
  TN         : TTreeNode;
  ZO         : TZoneObject;
  MO1        : TMeshObject;
  MO2        : TMeshObject;
  P1         : TPolygon;
  P2         : TPolygon;
  Meshes     : TStringList;
  SplitPairs : TStringList;
  SplitPair  : TSplitPair;
  SplitPair1 : TSplitPair;
  V1         : T3DPoint;
  V2         : T3DPoint;

begin
  Try
    EnableGUI(False);
    If Application.MessageBox('Split selected meshes with each other? (This cannot be undone!)','Confirm',MB_OKCANCEL) = IDOK Then
    Begin
      If Not LoadingParms Then
      Begin
        Meshes := TStringList.Create;
        For I := 0 To tvMain.SelectionCount - 1 Do
        Begin
          TN := tvMain.Selections[I];
          If TN <> Nil Then
          Begin
            ZO := TZoneObject(TN.Data);
            If ZO Is TMeshObject Then Meshes.AddObject('',ZO);
          End;
        End; // For I
        If Meshes.Count > 0 Then
        Begin
          // Build a list of all polygons that intersect

          frmStatus.Show;
          SplitPairs := TStringList.Create;
          frmStatus.SetCaption('Building list of intersecting polygons');
          For I := 0 To Meshes.Count - 1 Do
          Begin
            frmStatus.SetPosition(I / Meshes.Count);
            MO1 := TMeshObject(Meshes.Objects[I]);
            For J := I + 1 To Meshes.Count - 1 Do
            Begin
              MO2 := TMeshObject(Meshes.Objects[J]);
              For K := 0 To MO1.Polygons.Count - 1 Do
              Begin
                P1 := TPolygon(MO1.Polygons.Objects[K]);
                For L := 0 To MO2.Polygons.Count - 1 Do
                Begin
                  P2 := TPolygon(MO2.Polygons.Objects[L]);
                  If P1.IntersectsPolygon(P2,MO1,MO2) Then
                  Begin
                    SplitPair := TSplitPair.Create;
                    SplitPair.P1 := P1;
                    SplitPair.P2 := P2;
                    SplitPair.M1 := MO1;
                    SplitPair.M2 := MO2;
                    SplitPair.I1 := K;
                    SplitPair.I2 := L;
                    SplitPair.B1 := False;
                    SplitPair.B2 := False;
                    SplitPairs.AddObject('',SplitPair);
                  End;
                End; // For L
              End; // For K
            End; // For J
          End; // For I

          // For the polygons, calculate the splitting planes

          V1 := T3DPoint.Create;
          V2 := T3DPoint.Create;
          frmStatus.SetCaption('Calculating splitting planes');
          For I := 0 To SplitPairs.Count - 1 Do
          Begin
            frmStatus.SetPosition(I / SplitPairs.Count);
            SplitPair := TSplitPair(SplitPairs.Objects[I]);
            SplitPair.Q1 := Nil;
            SplitPair.Q2 := Nil;
            SplitPair.R1 := Nil;
            SplitPair.R2 := Nil;
            SplitPair.N1 := SplitPair.P1.GetNormal(SplitPair.M1);
            SplitPair.N2 := SplitPair.P2.GetNormal(SplitPair.M2);
            V1.Copy(T3DPoint(SplitPair.M1.Vertices.Objects[SplitPair.P1.Vertices[0]]));
            V2.Copy(T3DPoint(SplitPair.M2.Vertices.Objects[SplitPair.P2.Vertices[0]]));
            SplitPair.M1.MakeAbsolute(V1);
            SplitPair.M2.MakeAbsolute(V2);
            SplitPair.D1 := GetHessianDistance(SplitPair.N1,V1);
            SplitPair.D2 := GetHessianDistance(SplitPair.N2,V2);
          End; // For I
          V1.Free;
          V2.Free;

          frmStatus.SetCaption('Splitting polygons');
          For I := 0 To SplitPairs.Count - 1 Do
          Begin
            frmStatus.SetPosition(I / SplitPairs.Count);
            SplitPair := TSplitPair(SplitPairs.Objects[I]);

            If Not SplitPair.B1 Then
            Begin
              SplitPair.Q1 := TMeshObject.Create;
              SplitPair.Q1.AddCopyOfPolygon(SplitPair.P1,SplitPair.M1);
              SplitPair.R1 := TRegion.Create(SplitPair.Q1,True);
              SplitPair.R1.SetFlags;
              For J := I To SplitPairs.Count - 1 Do
              Begin
                SplitPair1 := TSplitPair(SplitPairs.Objects[J]);
                If SplitPair1.P1 = SplitPair.P1 Then
                Begin
                  SplitPair.R1.SplitAlongPlane(SplitPair1.N2,SplitPair1.D2,False,False);
                  SplitPair1.B1 := True;
                End;
              End; // For J
            End;

            If Not SplitPair.B2 Then
            Begin
              SplitPair.Q2 := TMeshObject.Create;
              SplitPair.Q2.AddCopyOfPolygon(SplitPair.P2,SplitPair.M2);
              SplitPair.R2 := TRegion.Create(SplitPair.Q2,True);
              SplitPair.R2.SetFlags;
              For J := I To SplitPairs.Count - 1 Do
              Begin
                SplitPair1 := TSplitPair(SplitPairs.Objects[J]);
                If SplitPair1.P2 = SplitPair.P2 Then
                Begin
                  SplitPair.R2.SplitAlongPlane(SplitPair1.N1,SplitPair1.D1,False,False);
                  SplitPair1.B2 := True;
                End;
              End; // For J
            End;
          End; // For I

          // Copy the split object polygons back to the selected objects and get rid
          // of the original unsplit polygons

          frmStatus.SetCaption('Copying remaining object polygons and discarding original unsplit polygons');
          For I := 0 To SplitPairs.Count - 1 Do
          Begin
            frmStatus.SetPosition(I / SplitPairs.Count);
            SplitPair := TSplitPair(SplitPairs.Objects[I]);

            If SplitPair.Q1 <> Nil Then
            Begin
              // Free the original polygon and add the new polygons

              SplitPair.P1.Free;
              SplitPair.M1.Polygons.Objects[SplitPair.I1] := Nil;
              SplitPair.R1.AddToMesh(SplitPair.M1,False,False);
            End;

            If SplitPair.Q2 <> Nil Then
            Begin
              // Free the original polygon and add the new polygons

              SplitPair.P2.Free;
              SplitPair.M2.Polygons.Objects[SplitPair.I2] := Nil;
              SplitPair.R2.AddToMesh(SplitPair.M2,False,False);
            End;
          End; // For I

          // Discard the slots that contained the original polygons in the selected objects

          frmStatus.SetCaption('Condensing selected objects');
          For J := 0 To Meshes.Count - 1 Do
          Begin
            frmStatus.SetPosition(J / Meshes.Count);
            MO1 := TMeshObject(Meshes.Objects[J]);
            I := 0;
            While I < MO1.Polygons.Count Do
            Begin
              If MO1.Polygons.Objects[I] = Nil
               Then MO1.Polygons.Delete(I)
               Else Inc(I);
            End; // While
            MO1.ConvertToTriangles;
            MO1.RemoveUnusedVertices;
          End; // For J

          // Cleanup

          frmStatus.SetCaption('Cleaning up splitting pairs');
          For I := 0 To SplitPairs.Count - 1 Do
          Begin
            frmStatus.SetPosition(I / SplitPairs.Count);
            SplitPair := TSplitPair(SplitPairs.Objects[I]);
            SplitPair.N1.Free;
            SplitPair.N2.Free;
            If SplitPair.R1 <> Nil Then SplitPair.R1.Free;
            If SplitPair.R2 <> Nil Then SplitPair.R2.Free;
            If SplitPair.Q1 <> Nil Then SplitPair.Q1.Free;
            If SplitPair.Q2 <> Nil Then SplitPair.Q2.Free;
            SplitPair.Free;
          End; // For I
          SplitPairs.Free;
          frmStatus.Hide;
        End;
        RegenerateGroundMeshes;
        ReloadZone;
        RenderZone(BirdsEye);
        If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
        SelectedPolygons.Clear;
        SelectedPolyMesh := Nil;

        // Cleanup

        Meshes.Free;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acInvertMeshesExecute(Sender: TObject);
Var
  I,J    : Integer;
  TN     : TTreeNode;
  ZO     : TZoneObject;
  MO     : TMeshObject;
  Meshes : TStringList;
  P      : TPolygon;
  List   : TStringList;

begin
  Try
    EnableGUI(False);
    If Not LoadingParms Then
    Begin
      Meshes := TStringList.Create;
      For I := 0 To tvMain.SelectionCount - 1 Do
      Begin
        TN := tvMain.Selections[I];
        If TN <> Nil Then
        Begin
          ZO := TZoneObject(TN.Data);
          If ZO Is TMeshObject Then Meshes.AddObject('',ZO);
        End;
      End; // For I
      If Meshes.Count > 0 Then
      Begin
        For I := 0 To Meshes.Count - 1 Do
        Begin
          MO := TMeshObject(Meshes.Objects[I]);
          For J := 0 To MO.Polygons.Count - 1 Do TPolygon(MO.Polygons.Objects[J]).Invert;
        End; // For I
      End;

      // Refresh the changed meshes

      List := TStringList.Create;
      For J := 0 To Meshes.Count - 1 Do
      Begin
        MO := TMeshObject(Meshes.Objects[J]);
        I  := Zone.GetObjectIndex(MO);
        MO.AddToPolygonList(List,False,True,True,True);
        MasterMeshList.Objects[I].Free;
        MasterMeshList.Objects[I] := List.Objects[0];
        RefreshObject(MO,True,True,True);
        List.Clear;
      End; // For J
      List.Free;

      // Cleanup

      Meshes.Free;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acCombineMeshesExecute(Sender: TObject);
Var
  I,J    : Integer;
  TN     : TTreeNode;
  ZO     : TZoneObject;
  MO     : TMeshObject;
  MO1    : TMeshObject;
  GO     : TGroupObject;
  Meshes : TStringList;
  P      : TPolygon;
  MinPt  : T3DPoint;
  MaxPt  : T3DPoint;

begin
  Try
    EnableGUI(False);
    If Application.MessageBox('Combine selected meshes? (This cannot be undone!)','Confirm',MB_OKCANCEL) = IDOK Then
    Begin
      If Not LoadingParms Then
      Begin
        ZPropList1.CurObj := Nil;
        Application.ProcessMessages;
        Meshes := TStringList.Create;
        For I := 0 To tvMain.SelectionCount - 1 Do
        Begin
          TN := tvMain.Selections[I];
          If TN <> Nil Then
          Begin
            ZO := TZoneObject(TN.Data);
            If ZO Is TMeshObject Then Meshes.AddObject('',ZO);
          End;
        End; // For I

        // Build a new mesh and add the other meshes to it

        MO1 := TMeshObject.Create;
        J := 1;
        While Zone.NameExists('combine' + IntToStr(J)) Do Inc(J);
        MO1.SetName('combine' + IntToStr(J));

        For I := 0 To Meshes.Count - 1 Do
        Begin
          MO := TMeshObject(Meshes.Objects[I]);
          For J := 0 To MO.Polygons.Count - 1 Do
          Begin
            P := TPolygon(MO.Polygons.Objects[J]);
            MO1.AddCopyOfPolygon(P,MO);
          End; // For J
          GO := MO.GetParent;
          If GO <> Nil Then
          Begin
            J := GO.Objects.IndexOfObject(MO);
            If J >= 0 Then   // Should always be true
            Begin
              GO.Objects.Delete(J);
              MO.Free;
            End;
          End
          Else
          Begin
            J := Zone.IndexOfObject(MO);
            If J >= 0 Then   // Should always be true
            Begin
              Zone.Delete(J);
              MO.Free;
            End;
          End;
        End; // For I

        MaxPt := T3DPoint.Create;
        MinPt := T3DPoint.Create;
        MO1.GetBounds(MinPt,MaxPt);
        For I := 0 To MO1.Vertices.Count - 1 Do T3DPoint(MO1.Vertices.Objects[I]).Subtract(MinPt);
        MO1.Loc.Copy(MinPt);
        MinPt.Free;
        MaxPt.Free;
        Zone.AddObject(MO1.GetName,MO1);

        LoadObjectTreeView;
        RegenerateGroundMeshes;
        ReloadZone;
        RenderZone(BirdsEye);
        If BirdsEye Then glView.Fit(Not BirdsEye,True,False);
        SelectedPolygons.Clear;
        SelectedPolyMesh := Nil;

        // Cleanup

        Meshes.Free;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acFlipTexCoordsExecute(Sender: TObject);
Var
  I,J,K : Integer;
  MO    : TMeshObject;
  P     : TPolygon;
  FlipH : Boolean;
  FlipV : Boolean;
  List  : TStringList;

begin
  Try
    EnableGUI(False);
    If (SelectedPolyMesh <> Nil) And (SelectedPolygons.Count > 0) Then
    Begin
      If frmFlipTexCoords.ShowModal = mrOk Then
      Begin
        FlipH := frmFlipTexCoords.cbFlipHorizontal.Checked;
        FlipV := frmFlipTexCoords.cbFlipVertical.Checked;
        If FlipH Or FlipV Then
        Begin
          MO := SelectedPolyMesh;
          For J := 0 To SelectedPolygons.Count - 1 Do
          Begin
            P := TPolygon(MO.Polygons.Objects[Integer(SelectedPolygons.Items[J])]);
            If FlipH Then
            Begin
              For K := 0 To High(P.TX) Do P.TX[K] := -P.TX[K];
            End;
            If FlipV Then
            Begin
              For K := 0 To High(P.TZ) Do P.TZ[K] := -P.TZ[K];
            End;
          End; // For J
          List := TStringList.Create;
          J  := Zone.GetObjectIndex(MO);
          MO.AddToPolygonList(List,False,True,True,True);
          MasterMeshList.Objects[J].Free;
          MasterMeshList.Objects[J] := List.Objects[0];
          RefreshObject(MO,True,True,True);
          List.Free;
        End;
      End;
    End
    Else ShowMessage('You must selected one or more polygons first.');
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acMultiplyTexCoordsExecute(Sender: TObject);
Var
  I,J,K : Integer;
  MO    : TMeshObject;
  P     : TPolygon;
  MulH  : Single;
  MulV  : Single;
  List  : TStringList;

begin
  Try
    EnableGUI(False);
    If (SelectedPolyMesh <> Nil) And (SelectedPolygons.Count > 0) Then
    Begin
      If frmMultiplyTexCoords.ShowModal = mrOk Then
      Begin
        Val(frmMultiplyTexCoords.edtHorizontal.Text,MulH,I);
        If I <> 0 Then MulH := 1;
        Val(frmMultiplyTexCoords.edtVertical.Text,MulV,I);
        If I <> 0 Then MulV := 1;
        MO := SelectedPolyMesh;
        For J := 0 To SelectedPolygons.Count - 1 Do
        Begin
          P := TPolygon(MO.Polygons.Objects[Integer(SelectedPolygons.Items[J])]);
          For K := 0 To High(P.TX) Do P.TX[K] := P.TX[K] * MulH;
          For K := 0 To High(P.TZ) Do P.TZ[K] := P.TZ[K] * MulV;
        End; // For J
        List := TStringList.Create;
        J  := Zone.GetObjectIndex(MO);
        MO.AddToPolygonList(List,False,True,True,True);
        MasterMeshList.Objects[J].Free;
        MasterMeshList.Objects[J] := List.Objects[0];
        RefreshObject(MO,True,True,True);
        List.Free;
      End;
    End
    Else ShowMessage('You must selected one or more polygons first.');
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acChangeLibraryReferenceExecute(Sender: TObject);
Var
  TN   : TTreeNode;
  ZO   : TZoneObject;
  ML   : TMeshLibraryObjectReference;
  CL   : TCreatureLibraryObjectReference;
  GO   : TGroupObject;
  I,J  : Integer;
  List : TStringList;
  Diff : Boolean;

begin
  Try
    EnableGUI(False);
    If Not LoadingParms Then
    Begin
      Diff := False;
      If (SelectedMesh <> '') Or (SelectedCreature <> '') Then
      Begin
        For J := 0 To tvMain.SelectionCount - 1 Do
        Begin
          TN := tvMain.Selections[J];
          If TN <> Nil Then
          Begin
            ZO := TZoneObject(TN.Data);
            If ZO Is TMeshLibraryObjectReference Then
            Begin
              If SelectedMesh <> '' Then
              Begin
                ML := TMeshLibraryObjectReference(ZO);
                I  := MeshLibrary.IndexOf(SelectedMesh);
                If (I >= 0) And (ML.Group <> TGroupObject(MeshLibrary.Objects[I])) Then
                Begin
                  ML.Group := TGroupObject(MeshLibrary.Objects[I]);
                  List     := TStringList.Create;
                  I        := Zone.GetObjectIndex(ML);
                  ML.AddToPolygonList(List,False,True,True,True);
                  MasterMeshList.Objects[I].Free;
                  MasterMeshList.Objects[I] := List.Objects[0];
                  RefreshObject(ML,True,True,True);
                  List.Free;
                End;
              End
              Else Diff := True;
            End
            Else If ZO Is TCreatureLibraryObjectReference Then
            Begin
              If SelectedCreature <> '' Then
              Begin
                CL := TCreatureLibraryObjectReference(ZO);
                I  := CreatureLibrary.IndexOf(SelectedCreature);
                If (I >= 0) And (CL.An8File <> TAn8File(CreatureLibrary.Objects[I])) Then
                Begin
                  CL.An8File := TAn8File(CreatureLibrary.Objects[I]);
                  List       := TStringList.Create;
                  I          := Zone.GetObjectIndex(CL);
                  CL.AddToPolygonList(List,False,True,True,True);
                  MasterMeshList.Objects[I].Free;
                  MasterMeshList.Objects[I] := List.Objects[0];
                  RefreshObject(CL,True,True,True);
                  List.Free;
                End;
              End
              Else Diff := True;
            End;
          End;
        End; // For J
      End
      Else ShowMessage('You must select an object in the mesh library first.');
      If Diff Then ShowMessage('Warning: you cannot change a mesh to a creature and vice versa.');
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acExportToSQLExecute(Sender: TObject);
Var
  Stream   : TFileStream;
  FileName : String;
  St       : String;
  St1      : PChar;
  Continue : Boolean;
  ZoneName : String;
  Mesh     : TMeshObject;

begin
  Try
    EnableGUI(False);
    ZoneName := Zone.ShortName;
    If InputQuery('Export to .SQL','Enter new zone name:',ZoneName) Then
    Begin
      ZoneName := Trim(ZoneName);
      If ZoneName <> '' Then
      Begin
        FileName    := ExtractFilePath(Application.ExeName) + 'zones\' + ZoneName + '.sql';
        If FileExists(FileName) Then
        Begin
          St  := 'Overwrite file ' + FileName + '?';
          St1 := StrAlloc(Length(St) + 1);
          StrPCopy(St1,St);
          Continue := (Application.MessageBox(St1,'File Exists',MB_OKCANCEL) = IDOK);
          StrDispose(St1);
        End
        Else Continue := True;
        If Continue Then
        Begin
          // Make a mesh of EVERYTHING in the zone so we can force SQL scripts to execute

          frmStatus.Show;
          frmStatus.SetCaption('Building zone objects (necessary to update SQL data)');
          Mesh     := Zone.BuildPolygonList(False,False,False,True);
          Mesh.Free;
          frmStatus.SetCaption('Exporting SQL');

          FileName := ExtractFilePath(Application.ExeName) + 'zones\' + ZoneName + '_doors.sql';
          ExportToDoorsSQL(FileName);

          FileName := ExtractFilePath(Application.ExeName) + 'zones\' + ZoneName + '_objects.sql';
          ExportToObjectsSQL(FileName);

          frmStatus.Hide;
        End;
      End
      Else ShowMessage('You must enter a valid zone name.');
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.cbShowHotSpotsChange(Sender: TObject);
begin
  ProgramSettings.ShowHotSpots := cbShowHotSpots.ItemIndex;
  SavePreferences;
  AddHotSpots;
  RenderZone(BirdsEye);
end;

procedure TfrmMain.acInsertHotSpotExecute(Sender: TObject);
Var
  HS  : THotSpot;
  St  : String;
  I   : Integer;

Begin
  St := 'hotspot';
  If St <> '' Then
  Begin
    // Find a unique name

    I := 1;
    While Zone.NameExists(St + IntToStr(I)) Do Inc(I);
    St := St + IntToStr(I);
    If InputQuery('Add hotspot','Enter new object name:',St) Then
    Begin
      St := Trim(St);
      If St <> '' Then
      Begin
        If Not Zone.NameExists(St) Then
        Begin
          HS       := THotSpot.Create(St);
          HS.Loc.X := Observer.X + CreateDist * Cos(Phi);
          HS.Loc.Y := Observer.Y + CreateDist * Sin(Phi);
          HS.Loc.Z := Observer.Z;

          // Add the object

          AddNewObject(St,HS);
        End
        Else ShowMessage('An object with that name already exists. Please choose a different one.')
      End
      Else ShowMessage('You cannot enter a blank name.');
    End;
    glView.scene3D.Redraw;
  End;
end;

procedure TfrmMain.acMoveObjectToLastHotSpotExecute(Sender: TObject);
Var
  TN     : TTreeNode;
  ZO     : TZoneObject;
  I      : Integer;
  AbsLoc : T3DPoint;

begin
  Try
    EnableGUI(False);
    If Not LoadingParms Then
    Begin
      If tvMain.SelectionCount = 1 Then
      Begin
        TN := tvMain.Selections[0];
        If TN <> Nil Then
        Begin
          ZO := TZoneObject(TN.Data);
          If ZO.GetParent = Nil Then
          Begin
            If (SelectedHotSpot <> Nil) And (SelectedHotSpot Is THotSpot) Then
            Begin
              I := Zone.GetObjectIndex(SelectedHotSpot);
              If (I >= 0) Or
                 ((SelectedHotSpotML <> Nil) And
                  (Zone.GetObjectIndex(SelectedHotSpotML) >= 0) And
                  (SelectedHotSpotML.Group.ContainsObject(SelectedHotSpot))) Then
              Begin
                If ZO Is TMeshLibraryObjectReference Then TMeshLibraryObjectReference(ZO).Gravity := False;
                If I >= 0 Then
                Begin
                  AbsLoc := SelectedHotSpot.GetAbsoluteLocation;
                  ZO.Loc.Copy(AbsLoc);
                  AbsLoc.Free;
                End
                Else
                Begin
                  AbsLoc := SelectedHotSpot.GetAbsoluteLocation;
                  SelectedHotSpotML.MakeAbsolute(AbsLoc);
                  ZO.Loc.Copy(AbsLoc);
                  AbsLoc.Free;
                End;
                LoadParameters;
                ZPropList1.CurObj := Nil; // Force it to reload
                ZPropList1.CurObj := ZO;
                RefreshObject(ZO,True,True,True);

                If (ZO Is TMeshLibraryObjectReference) And
                   TMeshLibraryObjectReference(ZO).Gravity Then ShowMessage('Gravity is ON for this object');
              End
              Else ShowMessage('Could not locate the selected hotspot.');
            End
            Else ShowMessage('No hotspot is selected.');
          End
          Else ShowMessage('Cannot perform this operation on a grouped object.');
        End;
      End
      Else ShowMessage('You can only do this if you have selected one and only one object.');
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acSelectHotspotExecute(Sender: TObject);
Var
  TN   : TTreeNode;
  ZO   : TZoneObject;
  List : TStringList;
  I    : Integer;

begin
  Try
    EnableGUI(False);
    If Not LoadingParms Then
    Begin
      If tvMain.SelectionCount = 1 Then
      Begin
        TN := tvMain.Selections[0];
        If TN <> Nil Then
        Begin
          ZO := TZoneObject(TN.Data);
          If ZO Is TMeshLibraryObjectReference Then
          Begin
            frmSelectHotspot.lblObjectName.Caption := ZO.GetName;
            List := TMeshLibraryObjectReference(ZO).Group.GetHotSpots;
            frmSelectHotspot.lbHotspots.Clear;
            For I := 0 To List.Count - 1 Do frmSelectHotspot.lbHotspots.Items.Add(List.Strings[I]);
            If List.Count > 0 Then frmSelectHotspot.lbHotspots.ItemIndex := 0;
            If frmSelectHotspot.ShowModal = mrOk Then
            Begin
              If (frmSelectHotspot.lbHotspots.ItemIndex >= 0) And (frmSelectHotspot.lbHotspots.ItemIndex < List.Count) Then
              Begin
                SelectedHotspot   := THotSpot(List.Objects[frmSelectHotspot.lbHotspots.ItemIndex]);
                SelectedHotspotML := TMeshLibraryObjectReference(ZO);
                AddHotSpots;
                LoadParameters;
                AllowHotSpotChange := False;
                ZPropList1.CurObj := Nil; // Force it to reload
                ZPropList1.CurObj := ZO;
                RefreshObject(ZO,True,True,True);
                AllowHotSpotChange := True;
              End;
            End;
            List.Free;
          End
          Else ShowMessage('This operation only works on objects you have placed from the mesh library.');
        End;
      End
      Else ShowMessage('You can only do this if you have selected one and only one object.');
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.edtSetHeightChange(Sender: TObject);
Var
  I  : Integer;
  S  : Single;
  St : String;

begin
  If Not SettingSetHeightEdit Then
  Begin
    St := Trim(edtSetHeight.Text);
    If St <> '' Then
    Begin
      Val(St,S,I);
      If I = 0 Then
      Begin
        SetHeight := S;
        edtSetHeight.Color := clWindow;
      End
      Else edtSetHeight.Color := $00C0C0FF;
    End
    Else edtSetHeight.Color := $00C0C0FF;
  End;
end;

procedure TfrmMain.acTutorialExecute(Sender: TObject);
begin
  Application.HelpJump('Tutorial');
end;

procedure TfrmMain.tvMeshesAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
Var
  Rect : TRect;
  iSbh : integer;
  Opt  : Integer;
  I    : Integer;

begin
  tvMeshes.Font.Name   := ZPropList1.Font.Name;
  tvMeshes.Font.Size   := ZPropList1.Font.Size;
  If Stage = cdPrePaint Then
  Begin
    Opt := ETO_CLIPPED;
    If cdsSelected In State Then
    Begin
      tvMeshes.Canvas.Brush.Style := bsSolid;
      tvMeshes.Canvas.Font.Color  := clWhite;
      tvMeshes.Canvas.Brush.Color := clNavy;
      Opt := Opt Or ETO_OPAQUE;
    End
    Else
    Begin
      tvMeshes.Canvas.Brush.Style := bsClear;
      tvMeshes.Canvas.Font.Color  := clBlack;
    End;

    Rect := Node.DisplayRect(False);
    StretchBlt(tvMeshes.Canvas.Handle,Rect.Left,Rect.Top,Rect.Right - Rect.Left,Rect.Bottom - Rect.Top,
               Image4.Canvas.Handle,0,Round(Rect.Top * Image4.Height / tvMeshes.Height),
               Image4.Width,
               Round((Rect.Bottom - Rect.Top) * Image4.Height / tvMeshes.Height),SRCCOPY);
  {
    Rect := Node.DisplayRect(False);
    If tvMeshes.Items.Count > 0 Then
    Begin
      ImageRect1 := tvMeshes.Items.GetNode(TreeView_GetFirstVisible(tvMeshes.Handle)).DisplayRect(False);
      ImageRect2 := tvMeshes.Items.GetNode(TreeView_GetLastVisible(tvMeshes.Handle)).DisplayRect(False);
      H          := ImageRect2.Bottom - ImageRect1.Top;
    End
    Else H := 0;
    StretchBlt(tvMeshes.Canvas.Handle,
               Rect.Left,Rect.Top,Rect.Right - Rect.Left,Rect.Bottom - Rect.Top,
               Image4.Canvas.Handle,0,Round(Rect.Top * Image4.Height / H),
               Image4.Width,
               Round((Rect.Bottom - Rect.Top) * Image4.Height / H),SRCCOPY);
  }
  {
    Rect := Node.DisplayRect(False);
    StretchBlt(tvMeshes.Canvas.Handle,
               Rect.Left,Rect.Top,Rect.Right - Rect.Left,Rect.Bottom - Rect.Top,
               Image4.Canvas.Handle,
               0,
               Round(Rect.Top * Image4.Height / tvMeshes.Height),
               Image4.Width,
               Round((Rect.Bottom - Rect.Top) * Image4.Height / tvMeshes.Height),SRCCOPY);
  }
    Rect := Node.DisplayRect(True);
    Rect.Right := Rect.Left + tvMeshes.Width - 4; // Need to expand the width because the text is wider when my fake ClearType is on




    DataModule1.ClearTypeText.ExtTextOut(Rect.Left,Rect.Top,Node.Text,Opt,tvMeshes.Canvas,tvMeshes.Canvas.Brush.Color,@Rect);

  //  tvMeshes.Canvas.TextOut(Rect.Left,Rect.Top,Node.Text);
    DefaultDraw := False;

    iSbh := GetScrollPos(Sender.Handle,SB_HORZ);
    Rect := Node.DisplayRect(False);
    Rect.Left := Rect.Left - iSbh;
    Rect.Left := Rect.Left + ((Node.Level+1) * TreeView_GetIndent(Sender.Handle));

    If Node.HasChildren Then
    Begin
      If Node.Expanded
       Then ilTreeView.Draw(Sender.Canvas,Rect.Left - 16,Rect.Top,1)
       Else ilTreeView.Draw(Sender.Canvas,Rect.Left - 16,Rect.Top,0);
    End;
  End;
end;

procedure TfrmMain.TBItemPrePaint(Item: TTBCustomItem;
  Viewer: TTBItemViewer; IsRotated: Boolean; const Canvas: TCanvas;
  ARect: TRect; const ACaption: String; ADrawDisabledShadow: Boolean;
  AFormat: Cardinal);
var
  DC: HDC;

  procedure Draw(FontColor: TColor);
  begin
    if not IsRotated then
      If (DataModule1.ClearTypeText <> Nil) Then
      Begin
        Viewer.View.GetFont.Name := TBToolbar1.Font.Name;
        Viewer.View.GetFont.Size := TBToolbar1.Font.Size;
        DataModule1.ClearTypeText.DrawTextDC(ARect.Left,ARect.Top,ACaption,AFormat,DC,Canvas.Font,FontColor,GetBkColor(Canvas.Handle),@ARect,False);
      End
      Else DrawText(DC, PChar(ACaption), Length(ACaption), ARect, AFormat)
    else
      DrawRotatedText(DC, ACaption, ARect, AFormat);
  end;

var
  ShadowColor, HighlightColor, SaveTextColor: DWORD;
begin
  DC := Canvas.Handle;
  if not ADrawDisabledShadow then
    Draw(Canvas.Font.Color)
  else begin
    ShadowColor    := GetSysColor(COLOR_BTNSHADOW);
    HighlightColor := GetSysColor(COLOR_BTNHIGHLIGHT);
    OffsetRect(ARect, 1, 1);
    SaveTextColor := SetTextColor(DC, HighlightColor);
    Draw(ColorToRGB(HighlightColor));
    OffsetRect(ARect, -1, -1);
    SetTextColor(DC, ShadowColor);
    Draw(ColorToRGB(ShadowColor));
    SetTextColor(DC, SaveTextColor);
  end;
end;

procedure TfrmMain.acExportToXWAExecute(Sender: TObject);
Var
  Stream   : TFileStream;
  FileName : String;
  St       : String;
  St1      : PChar;
  Continue : Boolean;
  ZoneName : String;

begin
  Try
    EnableGUI(False);
    ZoneName := Zone.ShortName;
    If InputQuery('Export to .XWA','Enter new zone name:',ZoneName) Then
    Begin
      ZoneName := Trim(ZoneName);
      If ZoneName <> '' Then
      Begin
        FileName    := ExtractFilePath(Application.ExeName) + 'zones\' + ZoneName + '.xwa';
        If FileExists(FileName) Then
        Begin
          St  := 'Overwrite file ' + FileName + '?';
          GetMem(St1,Length(St) + 1);
          StrPCopy(St1,St);
          Continue := (Application.MessageBox(St1,'File Exists',MB_OKCANCEL) = IDOK);
          FreeMem(St1);
        End
        Else Continue := True;
        If Continue Then
        Begin
          If Application.MessageBox('This will build an .XWA file containing the textures your'#13#10 +
                                    'scene is referencing.'#13#10#13#10 +
                                    'You CANNOT distribute any .XWA files you create unless you'#13#10 +
                                    'have permission to distribute ALL of the textures inside them!'#13#10#13#10 +
                                    'Continue?',
                                    '!! WARNING !!',
                                    MB_OKCANCEL) = IDOK Then
          Begin
            If frmXWFExportOptions.ShowModal = mrOk Then
            Begin
              Stream := TFileStream.Create(FileName,fmCreate);
              ExportToXWA(Stream,ZoneName);
              Stream.Free;

              Try
                FileName := ExtractFilePath(Application.ExeName) + 'eqemu_maps\' + ZoneName + '.map';
                Stream   := TFileStream.Create(FileName,fmCreate);
                ExportToEQEmuMap(Stream);
                Stream.Free;
              Except
                ShowMessage('Could not export the zone map file.  Please make sure that your eqemu_maps subfolder exists.');
              End;

              FileName := ExtractFilePath(Application.ExeName) + 'zones\' + ZoneName + '_doors.sql';
              ExportToDoorsSQL(FileName);

              FileName := ExtractFilePath(Application.ExeName) + 'zones\' + ZoneName + '_objects.sql';
              ExportToObjectsSQL(FileName);

              MakeSoundFiles(ZoneName);

//              ReloadZone; // Have to refresh what's loaded in OpenGL because we loaded different stuff
            End;
          End;
        End;
      End
      Else ShowMessage('You must enter a valid zone name.');
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acExportToAN8Execute(Sender: TObject);
Var
  St       : String;
  St1      : PChar;
  Continue : Boolean;
  FileName : String;
  TexList  : TStringList;
  I        : Integer;

begin
  Try
    EnableGUI(False);
    dlgSave.FileName   := '';
    dlgSave.InitialDir := ExtractFilePath(Application.ExeName) + 'ZONES';
    dlgSave.Filter     := 'Anim8or files (*.an8)|*.AN8';
    If dlgSave.Execute Then
    Begin
      FileName := dlgSave.FileName;
      If Pos('.',FileName) = 0 Then FileName := FileName + '.an8';
      If FileExists(FileName) Then
      Begin
        St  := 'Overwrite file ' + FileName + '?';
        St1 := StrAlloc(Length(St) + 1);
        StrPCopy(St1,St);
        Continue := (Application.MessageBox(St1,'File Exists',MB_OKCANCEL) = IDOK);
        StrDispose(St1);
      End
      Else Continue := True;
      If Continue Then
      Begin
        frmStatus.Show;
        frmStatus.SetCaption('Exporting zone to AN8');

        TexList := TStringList.Create;
        For I := 0 To glView.Scene3D.Scene.Textures.Count - 1 Do
         TexList.AddObject(LowerCase(glView.Scene3D.Scene.Textures.Strings[I]),glView.Scene3D.Scene.Textures.Objects[I]);

        Zone.ExportToAN8File(FileName,TexList);
        TexList.Free;
        frmStatus.Hide;
      End;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acAlterAn8FileExecute(Sender: TObject);
begin
  frmDeleteAn8Objects.ShowModal;
end;

procedure TfrmMain.cbWalkAlongGroundClick(Sender: TObject);
begin
  If (frmMain.Zone.ElevationGrid.NX = 0) Or (frmMain.Zone.ElevationGrid.NY = 0) Then
  Begin
    Try
      EnableGUI(False);
      ReloadZone;
    Finally
      EnableGUI(True);
    End;
  End;
end;

procedure TfrmMain.cbRealisticSkyClick(Sender: TObject);
begin
  glView.Scene3D.Scene.SkySphere1.Visible := cbRealisticSky.Checked;
  RenderZone(BirdsEye);
end;

procedure TfrmMain.acSaveSelectedAsExecute(Sender: TObject);
Var
  St       : String;
  S        : PChar;
  FileName : String;
  Cont     : Boolean;

begin
  dlgSave.FileName   := '';
  dlgSave.InitialDir := ExtractFilePath(Application.ExeName) + 'SCENES';
  dlgSave.Filter     := 'Scene files (*.scn)|*.SCN';
  If dlgSave.Execute Then
  Begin
    FileName := dlgSave.FileName;
    If Pos('.',FileName) = 0 Then FileName := FileName + '.scn';
    If FileExists(FileName) Then
    Begin
      Cont := False;
      St := 'Overwrite file ' + FileName + '?';
      S  := StrAlloc(Length(St) + 1);
      StrPCopy(S,St);
      If Application.MessageBox(S,'File Exists',MB_OKCANCEL) = IDOK Then Cont := True;
      StrDispose(S);
    End
    Else Cont := True;
    If Cont Then SaveSelectedScene(FileName);
    Begin
    End;
  End;
end;

procedure TfrmMain.acImportDirectXExecute(Sender: TObject);
Var Rotate: Boolean;
begin
  Try
    EnableGUI(False);
    dlgOpen.FileName   := '';
    dlgOpen.InitialDir := ExtractFilePath(Application.ExeName);
    dlgOpen.Filter     := 'DirectX files (*.x)|*.X';
    If dlgOpen.Execute Then
    Begin
      frmStatus.Show;
      frmStatus.SetCaption('Importing .X zone');
      Rotate := (Application.MessageBox('Is the DirectX model built such that Y is the up direction?',
                                        'Question',
                                         MB_YESNO) = IDYES);
      ImportFromDirectX(dlgOpen.FileName,Rotate);
      frmStatus.Hide;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.acConvertDXToAN8Execute(Sender: TObject);
Var
  DXFile  : TDirectXFile;
  An8File : TAn8File;
  St      : String;
  Cont    : Boolean;
  S       : PChar;

begin
  Try
    EnableGUI(False);
    dlgOpen.Filter := 'DirectX files (*.x)|*.X';
    If dlgOpen.Execute Then
    Begin
      frmStatus.Show;
      frmStatus.SetCaption('Importing .X zone');
      If FileExists(dlgOpen.FileName) Then
      Begin
        DXFile := TDirectXFile.Create;
        DXFile.LoadFromFile(dlgOpen.FileName);
        An8File := DXFile.CreateAn8File;
        St := ExtractFilePath(dlgOpen.FileName) + ExtractFileNameNoExt(dlgOpen.FileName) + '.an8';
        dlgSave.InitialDir := ExtractFilePath(dlgOpen.FileName);
        dlgSave.Filter     := 'Anim8or files (*.an8)|*.AN8';
        dlgSave.FileName   := St;
        If dlgSave.Execute Then
        Begin
          If FileExists(dlgSave.FileName) Then
          Begin
            Cont := False;
            St   := 'Overwrite file ' + dlgSave.FileName + '?';
            S    := StrAlloc(Length(St) + 1);
            StrPCopy(S,St);
            If Application.MessageBox(S,'File Exists',MB_OKCANCEL) = IDOK Then Cont := True;
            StrDispose(S);
          End
          Else Cont := True;
          If Cont Then An8File.SaveToFile(dlgSave.FileName);
        End;
        An8File.Free;
        DXFile.Free;
      End;
      frmStatus.Hide;
    End;
  Finally
    EnableGUI(True);
  End;
end;

procedure TfrmMain.cbEnableShadersClick(Sender: TObject);
begin
  glView.Scene3D.Scene.ShaderManager.Enabled := cbEnableShaders.Checked;
  RenderZone(BirdsEye);
end;

Initialization
  LoadTexProc := @LoadImage;
End.
