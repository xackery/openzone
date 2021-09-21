unit AbstractComponents;

interface

Uses Windows,Graphics,Classes,Forms,Controls,SyncObjs,MyHashes,SimpleInterfacedObjects;

Const
  ShiftFlag                  = $1000;
  DefaultSelectionForeground = $00FFFFFF;
  DefaultSelectionBackground = $00000080;
  DefaultFixedForeground     = $00FFFF00;
  DefaultFixedBackground     = $00808080;
  DefaultDisabledForeground  = $00404040;
  LastColorCommand           = #253;
  SetColorCommand            = #254;
  ResetColorCommand          = #255;
  ScrollBarThickness         = 16;

Type
  TAbstractUI        = Class;
  TAbstractWindow    = Class;
  TAbstractComponent = Class;

  TEventType = (evtMouseDown,evtMouseUp,evtMouseMove);
  TEventTypes = Set Of TEventType;

  TPaintEvent = Procedure(Sender: TObject; X,Y: Integer) Of Object;
  TCloseQueryEvent = Procedure(Sender: TObject; Var CanClose: Boolean) Of Object;

  TAbstractComponentType = (ctAbstract,ctButton,ctEdit,ctLabel,ctListBox,ctPanel,ctProgressBar,ctScrollBar,ctScrollPane,
                            ctStringGrid,ctTrackBar,ctWindow,ctPageControl,ctComboBox);

  IAbstractComponent = Interface(IInterface)
    Procedure SetUI(AUI: TAbstractUI);
    Procedure AddChild(Child: TAbstractComponent);
    Procedure QueueRepaint;
    Procedure SetLeft(I: Integer);
    Procedure SetTop(I: Integer);
    Procedure SetWidth(I: Integer);
    Procedure SetHeight(I: Integer);
    Function  GetChildClipRect(ChildComponent: TAbstractComponent): TRect;
    Function  GetComponentCount: Integer;
    Function  GetComponent(Index: Integer): TAbstractComponent;
    Function  TextWidth(Const St: String): Integer;
    Function  TextHeight(Const St: String): Integer;
    Function  GetComponentType: TAbstractComponentType;
    Property  ComponentType  : TAbstractComponentType Read GetComponentType;
    Property  ComponentCount : Integer                Read GetComponentCount;
    Property  Components[Index: Integer]: TAbstractComponent Read GetComponent;
  End;

  TAbstractComponent = Class(TSimpleInterfacedObject, IAbstractComponent)
  Protected
    FLeft          : Integer;
    FTop           : Integer;
    FWidth         : Integer;
    FHeight        : Integer;
    FClientLeft    : Integer;
    FClientTop     : Integer;
    FClientWidth   : Integer;
    FClientHeight  : Integer;
    FVisible       : Boolean;
    FEnabled       : Boolean;
    FParent        : TAbstractComponent;
    FChildren      : TStringList;
    FForeground    : TColor;
    FBackground    : TColor;
    FAlign         : TAlignSet;
    FReadOnly      : Boolean;   
//    FMutex         : TCriticalSection;
    FFocused       : Boolean;
    FUI            : TAbstractUI;
    FOwner         : TAbstractWindow;
    FChanging      : Boolean;
    FMouseOver     : Boolean;
    FOnMouseEnter  : TNotifyEvent;
    FOnMouseExit   : TNotifyEvent;
    FOnKeyDown     : TKeyEvent;
    FOnPaint       : TPaintEvent;
    FOnBeforePaint : TPaintEvent;
    FOnAfterPaint  : TPaintEvent;
    FOnMouseDown   : TMouseEvent;
    FOnMouseUp     : TMouseEvent;
    FTag           : Integer;
    FAcceptEvents  : TEventTypes;
    FClipRect      : TRect;
    FOnClose       : TNotifyEvent;
    FOnShow        : TNotifyEvent;
    FOnLostFocus   : TNotifyEvent;
    FOnCloseQuery  : TCloseQueryEvent;
    FTransparent   : Boolean;
    FComponentType : TAbstractComponentType;
    FMinWidth      : Integer;
    FMinHeight     : Integer;
    Procedure   SetLeft(I: Integer);
    Procedure   SetTop(I: Integer);
    Procedure   SetWidth(I: Integer);
    Procedure   SetHeight(I: Integer);
    Procedure   SetMinWidth(I: Integer);
    Procedure   SetMinHeight(I: Integer);
    Procedure   SetVisible(B: Boolean);
    Procedure   SetEnabled(B: Boolean); 
    Procedure   SetForeground(C: TColor);
    Procedure   SetBackground(C: TColor);
    Procedure   SetAlign(AAlign: TAlignSet);
    Procedure   SetReadOnly(AReadOnly: Boolean);
    Procedure   Realign(OldWidth,OldHeight: Integer);
    Procedure   BasicInit;
    Procedure   CalcAbsoluteXY(Out X,Y: Integer);
    Procedure   RepaintParent;
    Procedure   AfterSetSize; Dynamic;
    Procedure   CalcClientExtents; Dynamic;
    Procedure   MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Dynamic;
    Procedure   MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Dynamic;
    Procedure   MouseMoveAction(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor); Dynamic;
    Function    KeyDownAction(Key,Shift: Integer; C: Char): Boolean; Dynamic;
    Procedure   SetUI(AUI: TAbstractUI);
    Procedure   SetFocused(B: Boolean);
    Procedure   Changing;
    Procedure   Changed;
    Procedure   QueueRepaint; Dynamic;
    Procedure   Rectangle(X1,Y1,X2,Y2: Integer; FillColor: TColor); Dynamic;
    Function    GetComponentCount: Integer;
    Function    GetComponent(Index: Integer): TAbstractComponent;
    Procedure   SetTransparent(B: Boolean);
    Procedure   CreateChildren; Dynamic;
    Procedure   BeforePaintingChildren; Dynamic;
    Procedure   AfterPaintingChildren; Dynamic;
    Procedure   FillBackground; Dynamic;
    Procedure   InternalSetEnabled(B: Boolean); Dynamic;
    Function    GetComponentType: TAbstractComponentType; 
  Public
    Constructor Create(AParent: TAbstractComponent; AComponentType: TAbstractComponentType); Overload;
    Constructor Create(AParent: TAbstractComponent; AComponentType: TAbstractComponentType; ALeft,ATop,AWidth,AHeight: Integer); Overload;
    Procedure   Repaint; Dynamic;
    Destructor  Destroy; Override;
    Procedure   Paint(X,Y: Integer); Dynamic;
    Procedure   AddChild(Child: TAbstractComponent); Dynamic;
    Function    MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; Dynamic;
    Function    MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; Dynamic;
    Function    MouseMove(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor): Boolean; Dynamic;
    Function    ContainsCursor(X, Y: Integer): TAbstractComponent; Dynamic;
    Function    KeyDown(Key,Shift: Integer; C: Char): Boolean;
    Function    GetChildClipRect(ChildComponent: TAbstractComponent): TRect; Dynamic;
    Function    GetClipRect: TRect;
    Function    TextWidth(Const St: String): Integer;    Dynamic;
    Function    TextHeight(Const St: String): Integer;   Dynamic;
    Procedure   TextOut(X,Y: Integer; Const St: String); Dynamic;
    Procedure   BeginClip(Rect: TRect); Dynamic;
    Procedure   EndClip;                Dynamic;
    Property    Left           : Integer                Read FLeft          Write SetLeft;
    Property    Top            : Integer                Read FTop           Write SetTop;
    Property    Width          : Integer                Read FWidth         Write SetWidth;
    Property    Height         : Integer                Read FHeight        Write SetHeight;
    Property    MinWidth       : Integer                Read FMinWidth      Write SetMinWidth;
    Property    MinHeight      : Integer                Read FMinHeight     Write SetMinHeight;
    Property    Visible        : Boolean                Read FVisible       Write SetVisible;
    Property    Enabled        : Boolean                Read FEnabled       Write SetEnabled;
    Property    Foreground     : TColor                 Read FForeground    Write SetForeground;
    Property    Background     : TColor                 Read FBackground    Write SetBackground;
    Property    Align          : TAlignSet              Read FAlign         Write SetAlign;
    Property    ReadOnly       : Boolean                Read FReadOnly      Write SetReadOnly;            
    Property    Parent         : TAbstractComponent     Read FParent;
    Property    ClientWidth    : Integer                Read FClientWidth;
    Property    ClientHeight   : Integer                Read FClientHeight;
    Property    ClientLeft     : Integer                Read FClientLeft;
    Property    ClientTop      : Integer                Read FClientTop;
    Property    Owner          : TAbstractWindow        Read FOwner;
    Property    UI             : TAbstractUI            Read FUI            Write SetUI;
    Property    Focused        : Boolean                Read FFocused       Write SetFocused;
    Property    OnKeyDown      : TKeyEvent              Read FOnKeyDown     Write FOnKeyDown;
    Property    OnMouseEnter   : TNotifyEvent           Read FOnMouseEnter  Write FOnMouseEnter;
    Property    OnMouseExit    : TNotifyEvent           Read FOnMouseExit   Write FOnMouseExit;
    Property    OnPaint        : TPaintEvent            Read FOnPaint       Write FOnPaint;
    Property    OnBeforePaint  : TPaintEvent            Read FOnBeforePaint Write FOnBeforePaint;
    Property    OnAfterPaint   : TPaintEvent            Read FOnAfterPaint  Write FOnAfterPaint;
    Property    OnMouseDown    : TMouseEvent            Read FOnMouseDown   Write FOnMouseDown;
    Property    OnMouseUp      : TMouseEvent            Read FOnMouseUp     Write FOnMouseUp;
    Property    OnLostFocus    : TNotifyEvent           Read FOnLostFocus   Write FOnLostFocus;
    Property    Tag            : Integer                Read FTag           Write FTag;
    Property    AcceptEvents   : TEventTypes            Read FAcceptEvents  Write FAcceptEvents;
    Property    Transparent    : Boolean                Read FTransparent   Write SetTransparent;
    Property    ComponentType  : TAbstractComponentType Read GetComponentType;
    Property    ComponentCount : Integer                Read GetComponentCount;
    Property    Components[Index: Integer]: TAbstractComponent Read GetComponent;
  End;

  TAbstractLabel = Class;

  TAbstractButtonKind  = (cbkCaption,cbkUpArrow,cbkDownArrow,cbkLeftArrow,cbkRightArrow,cbkX);
  TAbstractButtonStyle = (cbsButton,cbsCheck);
  TAbstractButton = Class(TAbstractComponent)
  Protected
    FLabel      : TAbstractLabel;
    FKind       : TAbstractButtonKind;
    FDown       : Boolean;
    FPressed    : Boolean;
    FStyle      : TAbstractButtonStyle;
    FOnClick    : TNotifyEvent;
    FFlat       : Boolean;
    FGroup      : Integer;
    FAllowAllUp : Boolean;
    FModalRes   : TModalResult;
    Procedure   SetCaption(St: String);
    Procedure   SetKind(AKind: TAbstractButtonKind);
    Procedure   SetDown(B: Boolean);
    Procedure   SetStyle(AStyle: TAbstractButtonStyle);
    Procedure   SetFlat(B: Boolean);
    Procedure   SetWordWrapped(B: Boolean);
    Function    GetWordWrapped: Boolean;
    Function    GetCaption: String;
    Procedure   MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure   MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure   CreateChildren; Override;
    Procedure   AdjustLabelPosition;
    Procedure   InternalSetEnabled(B: Boolean); Override;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TAbstractButtonKind;
                       AStyle: TAbstractButtonStyle; ACaption: String);
    Procedure   Click;
    Property    Caption     : String               Read GetCaption     Write SetCaption;
    Property    Kind        : TAbstractButtonKind  Read FKind          Write SetKind;
    Property    Down        : Boolean              Read FDown          Write SetDown;
    Property    Style       : TAbstractButtonStyle Read FStyle         Write SetStyle;
    Property    OnClick     : TNotifyEvent         Read FOnClick       Write FOnClick;
    Property    Flat        : Boolean              Read FFlat          Write SetFlat;
    Property    Group       : Integer              Read FGroup         Write FGroup;
    Property    AllowAllUp  : Boolean              Read FAllowAllUp    Write FAllowAllUp;
    Property    ModalRes    : TModalResult         Read FModalRes      Write FModalRes;
    Property    WordWrapped : Boolean              Read GetWordWrapped Write SetWordWrapped;
  End;

  TAbstractEdit = Class(TAbstractComponent)
  Protected
    FText       : String;
    FMaxLen     : Integer;
    FShowBorder : Boolean;
    FOnChange   : TNotifyEvent;
    Procedure   SetText(St: String);
    Function    KeyDownAction(Key,Shift: Integer; C: Char): Boolean; Override;
    Procedure   SetShowBorder(B: Boolean);
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AText: String; AMaxLen: Integer);
    Property    Text       : String       Read FText       Write SetText;
    Property    ShowBorder : Boolean      Read FShowBorder Write SetShowBorder;
    Property    OnChange   : TNotifyEvent Read FOnChange   Write FOnChange;
  End;

  TAbstractScrollBar = Class(TAbstractComponent)
  Protected
    FKind     : TScrollBarKind;
    FMin      : Integer;
    FMax      : Integer;
    FPosition : Integer;
    FPageSize : Integer;
    FSmallInc : Integer;
    FULButton : TAbstractButton; // Up/left button
    FDRButton : TAbstractButton; // Down/right button
    FOnChange : TNotifyEvent;
    FDragging : Boolean;
    FDragX    : Integer;
    FDragY    : Integer;
    Procedure   SetMin(I: Integer);
    Procedure   SetMax(I: Integer);
    Procedure   SetPosition(I: Integer);
    Procedure   SetPageSize(I: Integer);
    Procedure   SetSmallInc(I: Integer);
    Procedure   ULButtonClick(Sender: TObject);
    Procedure   DRButtonClick(Sender: TObject);
    Procedure   MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure   MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure   MouseMoveAction(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor); Override;
    Procedure   CreateChildren; Override;
    Procedure   AfterSetSize; Override;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TScrollBarKind); Overload;
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,ALength: Integer; AKind: TScrollBarKind); Overload;
    Property    Kind     : TScrollBarKind Read FKind;
    Property    Min      : Integer        Read FMin      Write SetMin;
    Property    Max      : Integer        Read FMax      Write SetMax;
    Property    Position : Integer        Read FPosition Write SetPosition;
    Property    PageSize : Integer        Read FPageSize Write SetPageSize;
    Property    SmallInc : Integer        Read FSmallInc Write SetSmallInc;
    Property    OnChange : TNotifyEvent   Read FOnChange Write FOnChange;
  End;

  TAbstractTrackBar = Class(TAbstractComponent)
  Protected
    FMin       : Integer;
    FMax       : Integer;
    FPosition  : Integer;
    FStep      : Integer;
    FOnChange  : TNotifyEvent;
    FDragging  : Boolean;
    FDragX     : Integer;
    FDragY     : Integer;
    FAssociate : TAbstractEdit;
    FDragWidth : Integer;
    Procedure   SetMin(I: Integer);
    Procedure   SetMax(I: Integer);
    Procedure   SetStep(I: Integer);
    Procedure   SetPosition(I: Integer);
    Procedure   SetAssociate(Edit: TAbstractEdit);
    Procedure   MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure   MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure   MouseMoveAction(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor); Override;
    Procedure   AssociateChange(Sender: TObject);
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer); Overload;
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,ALength: Integer); Overload;
    Property    Min       : Integer        Read FMin       Write SetMin;
    Property    Max       : Integer        Read FMax       Write SetMax;
    Property    Step      : Integer        Read FStep      Write SetStep;
    Property    Position  : Integer        Read FPosition  Write SetPosition;
    Property    OnChange  : TNotifyEvent   Read FOnChange  Write FOnChange;
    Property    Associate : TAbstractEdit  Read FAssociate Write SetAssociate;
  End;

  TAbstractListBox = Class(TAbstractComponent)
  Protected
    FItems               : TStringList;
    FItemIndex           : Integer;
    FTopIndex            : Integer;
    FTextHeight          : Integer;
    FScrollBar           : TAbstractScrollBar;
    FShowBorder          : Boolean;
    FSelectionForeground : TColor;
    FSelectionBackground : TColor;
    FWordWrapped         : Boolean;
    FWordWrappedText     : TStringList;
    FSetupCharWidths     : Boolean;
    FCharWidths          : Packed Array[0..255] Of Integer;
    FLastColor           : TColor;
    FCurrentColor        : TColor;
    FOnClick             : TNotifyEvent;
    Function    GetItem(Index: Integer): String;
    Procedure   SetItem(Index: Integer; St: String);
    Procedure   SetItemIndex(Index: Integer);
    Procedure   SetTopIndex(Index: Integer);
    Function    GetItemCount: Integer;
    Function    GetItemHeight: Integer;
    Procedure   AfterSetSize; Override;
    Procedure   SetShowBorder(B: Boolean);
    Procedure   ScrollBarChange(Sender: TObject);
    Procedure   MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure   SetSelectionForeground(C: TColor);
    Procedure   SetSelectionBackground(C: TColor);
    Procedure   CreateChildren; Override;
    Procedure   GetWordWrappedText(St: String);
    Function    GetDisplayableItems: Integer;
    Procedure   OutputText(X,Y: Integer; St: String; DefaultColor: TColor); Dynamic;
    Function    GetAvailableVertPixels: Integer; Dynamic;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AWordWrapped: Boolean);
    Destructor  Destroy; Override;
    Procedure   Add(St: String);
    Procedure   Delete(Index: Integer);
    Procedure   Clear;
    Function    GetMaxTopIndex: Integer; Dynamic;
    Property    Items[Index: Integer] : String       Read GetItem              Write SetItem;
    Property    ItemIndex             : Integer      Read FItemIndex           Write SetItemIndex;
    Property    TopIndex              : Integer      Read FTopIndex            Write SetTopIndex;
    Property    ItemCount             : Integer      Read GetItemCount;
    Property    ItemHeight            : Integer      Read GetItemHeight;
    Property    ShowBorder            : Boolean      Read FShowBorder          Write SetShowBorder;
    Property    SelectionForeground   : TColor       Read FSelectionForeground Write SetSelectionForeground;
    Property    SelectionBackground   : TColor       Read FSelectionBackground Write SetSelectionBackground;
    Property    WordWrapped           : Boolean      Read FWordWrapped;
    Property    OnClick               : TNotifyEvent Read FOnClick             Write FOnClick;
  End;

  TAbstractLabel = Class(TAbstractComponent)
  Protected
    FCaption         : String;
    FAutoSize        : Boolean;
    FWordWrapped     : Boolean;
    FWordWrappedText : TStringList;
    FSetupCharWidths : Boolean;
    FCharWidths      : Packed Array[0..255] Of Integer;
    FAlignment       : TAlignment;
    FLastColor       : TColor;
    FCurrentColor    : TColor;
    Procedure   SetCaption(St: String);
    Procedure   SetAutoSize(B: Boolean);
    Procedure   SetWordWrapped(B: Boolean);
    Procedure   DoAutoSize; Dynamic;
    Procedure   GetWordWrappedText(St: String);
    Procedure   SetAlignment(A: TAlignment);
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; ATransparent: Boolean); Overload;
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop: Integer; ACaption: String; ATransparent: Boolean); Overload;
    Destructor  Destroy; Override;
    Property    Caption     : String     Read FCaption     Write SetCaption;
    Property    AutoSize    : Boolean    Read FAutoSize    Write SetAutoSize;
    Property    WordWrapped : Boolean    Read FWordWrapped Write SetWordWrapped;
    Property    Alignment   : TAlignment Read FAlignment   Write SetAlignment;
  End;

  TAbstractScrollPane = Class(TAbstractComponent)
  Protected
    FVertScrollBar : TAbstractScrollBar;
    FHorzScrollBar : TAbstractScrollBar;
    Procedure   AfterSetSize; Override;
    Function    GetChild: TAbstractComponent;
    Procedure   SetChild(Component: TAbstractComponent);
    Procedure   VertScrollBarChange(Sender: TObject);
    Procedure   HorzScrollBarChange(Sender: TObject);
    Procedure   CreateChildren; Override;
    Function    GetHorzSmallInc: Integer;
    Procedure   SetHorzSmallInc(I: Integer);
    Function    GetVertSmallInc: Integer;
    Procedure   SetVertSmallInc(I: Integer);
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
    Procedure   Repaint; Override;
    Procedure   AddChild(Component: TAbstractComponent); Override;
    Function    GetChildClipRect(ChildComponent: TAbstractComponent): TRect; Override;
    Property    Child         : TAbstractComponent Read GetChild Write SetChild;
    Property    VertScrollBar : TAbstractScrollBar Read FVertScrollBar;
    Property    HorzScrollBar : TAbstractScrollBar Read FHorzScrollBar;
    Property    VertSmallInc  : Integer            Read GetVertSmallInc Write SetVertSmallInc;
  End;

  TAbstractPanel = Class(TAbstractComponent)
  Protected
    FLabel          : TAbstractLabel;
    FShowBorder     : Boolean;
    FFillBackground : Boolean;
    Function    GetCaption: String;
    Procedure   SetCaption(St: String);
    Procedure   AdjustLabelPosition;
    Procedure   SetShowBorder(B: Boolean);
    Procedure   SetWordWrapped(B: Boolean);
    Function    GetWordWrapped: Boolean;
    Procedure   CreateChildren; Override;
    Procedure   SetFillBackground(B: Boolean);
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String);
    Property    Caption        : String  Read GetCaption      Write SetCaption;
    Property    ShowBorder     : Boolean Read FShowBorder     Write SetShowBorder;
    Property    WordWrapped    : Boolean Read GetWordWrapped  Write SetWordWrapped;
    Property    FillBackground : Boolean Read FFillBackground Write SetFillBackground;
  End;

  TAbstractComboBox = Class(TAbstractComponent)
  Protected
    FList          : TAbstractListBox;
    FListPanel     : TAbstractPanel;
    FEdit          : TAbstractEdit;
    FButton        : TAbstractButton;
    FLastListIndex : Integer;
    Function    GetItemCount: Integer;
    Function    GetItem(Index: Integer): String;
    Function    GetItemIndex: Integer;
    Procedure   SetItemIndex(Index: Integer);
    Procedure   CreateChildren; Override;
    Procedure   ButtonClick(Sender: TObject);
    Procedure   ListClick(Sender: TObject);
    Procedure   ListLostFocus(Sender: TObject);
    Procedure   EditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth: Integer);
    Destructor  Destroy; Override;
    Procedure   Add(St: String);
    Property    ItemCount             : Integer Read GetItemCount;
    Property    ItemIndex             : Integer Read GetItemIndex Write SetItemIndex;
    Property    Items[Index: Integer] : String  Read GetItem;
  End;

  PTabInfo = ^TTabInfo;
  TTabInfo = Record
    Row   : Integer; // Zero-indexed
    X     : Integer; // Pixel location on the row
    Width : Integer; // Width in pixels
  End;

  TAbstractPageControl = Class(TAbstractComponent)
  Protected
    FActivePage      : TAbstractPanel;
    FActivePageIndex : Integer;
    FPages           : TStringList;
    FTopTabRow       : Integer;
    FTabInfo         : TList;      // List of TTabInfo
    FRows            : Integer;
    Procedure   AfterSetSize; Override;
    Procedure   SetActivePage(Page: TAbstractPanel);    
    Procedure   SetActivePageIndex(Page: Integer);
    Function    GetTabHeight: Integer; Dynamic; Abstract;
    Procedure   CalculateTabInfo;
    Function    GetTabWidth(Index: Integer): Integer; Dynamic; Abstract;
    Function    CreatePagePanel(PageCaption: String): TAbstractPanel; Dynamic; Abstract;
    Procedure   ResizePages;
    Procedure   MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
    Destructor  Destroy; Override;
    Function    AddPage(PageCaption: String): TAbstractPanel;
    Property    ActivePage      : TAbstractPanel Read FActivePage      Write SetActivePage;
    Property    ActivePageIndex : Integer        Read FActivePageIndex Write SetActivePageIndex;
  End;

  TAbstractProgressBar = Class(TAbstractComponent)
  Protected
    FValue : Single; // Ranges from 0 to 1
    FColor : TColor;
    Procedure   SetValue(AValue: Single);
    Procedure   SetColor(AColor: TColor);
  Public
    Constructor Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AValue: Single; AColor: TColor);
    Property    Color : TColor Read FColor Write SetColor;
    Property    Value : Single Read FValue Write SetValue;
  End;

  TAbstractWindow = Class(TAbstractComponent)
  Protected
    FCaption        : String;
    FShowCaption    : Boolean;
    FShowBorder     : Boolean;
    FCloseButton    : TAbstractButton;
    FZOrder         : Integer;
    FDragging       : Boolean;
    FResizing       : TCursor;
    FResizeUL       : Boolean;
    FDragX          : Integer;
    FDragY          : Integer;
    FClosable       : Boolean;
    FClipQueue      : TList; // List of HRGN
    FBorderWidth    : Integer;
    FModalResult    : TModalResult;
    FResizeable     : Boolean;
    FOnZOrderChange : TNotifyEvent;
    Procedure   SetCaption(St: String);
    Procedure   SetShowCaption(B: Boolean);
    Procedure   SetShowBorder(B: Boolean);
    Procedure   CloseButtonClick(Sender: TObject);
    Procedure   SetZOrder(I: Integer);
    Procedure   MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure   MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure   MouseMoveAction(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor); Override;
    Procedure   SetClosable(B: Boolean);
    Procedure   SetResizeable(B: Boolean);
    Procedure   CreateCloseButton; Dynamic;
  Public
    Constructor Create(ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; AUI: TAbstractUI);
    Destructor  Destroy; Override;
    Function    GetChildClipRect(ChildComponent: TAbstractComponent): TRect; Override;
    Function    MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; Override;
    Function    MouseMove(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor): Boolean; Override;
    Function    ContainsCursor(X, Y: Integer): TAbstractComponent; Override;
    Procedure   Show;
    Procedure   ShowModal;
    Procedure   Hide;
    Property    Caption        : String           Read FCaption        Write SetCaption;
    Property    ShowCaption    : Boolean          Read FShowCaption    Write SetShowCaption;
    Property    ShowBorder     : Boolean          Read FShowBorder     Write SetShowBorder;
    Property    ZOrder         : Integer          Read FZOrder         Write SetZOrder;
    Property    OnShow         : TNotifyEvent     Read FOnShow         Write FOnShow;
    Property    OnClose        : TNotifyEvent     Read FOnClose        Write FOnClose;
    Property    OnCloseQuery   : TCloseQueryEvent Read FOnCloseQuery   Write FOnCloseQuery;
    Property    OnZOrderChange : TNotifyEvent     Read FOnZOrderChange Write FOnZOrderChange;
    Property    Closable       : Boolean          Read FClosable       Write SetClosable;
    Property    Resizeable     : Boolean          Read FResizeable     Write SetResizeable;
    Property    BorderWidth    : Integer          Read FBorderWidth;
    Property    ModalResult    : TModalResult     Read FModalResult;
  End;

  TMoveToFrontEvent = Procedure(Sender: TObject; Index: Integer) Of Object;

  TAbstractUI = Class
  Protected
    FWindows          : TStringList;
    FFocusedComponent : TAbstractComponent;
    FRepaintQueue     : TStringList;
    FQueueMutex       : TCriticalSection;
    FWindowsMutex     : TCriticalSection;
    FOnMoveToFront    : TMoveToFrontEvent;
    FModalWindow      : TAbstractWindow;
    Function    GetWindow(Index: Integer): TAbstractWindow;
    Function    GetNumWindows: Integer;
    Procedure   AddRepaint(Component: TAbstractComponent);
  Public
    Constructor Create;
    Destructor  Destroy; Override;
    Function    AddWindow(ACaption: String): TAbstractWindow; Dynamic;
    Procedure   RemoveWindow(Window: TAbstractWindow); Dynamic;
    Procedure   Repaint; Dynamic;
    Function    MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    Function    MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    Function    MouseMove(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor): Boolean;
    Function    ContainsCursor(X, Y: Integer): TAbstractComponent;
    Function    KeyDown(Key,Shift: Integer; C: Char): Boolean;
    Function    GetIndexOfWindow(Window: TAbstractWindow): Integer;
    Procedure   LockWindowList;
    Procedure   UnlockWindowList;
    Procedure   LockRepaintQueue;
    Procedure   UnlockRepaintQueue;
    Procedure   MoveToFront(Window: TAbstractWindow);
    Function    MakeModal(Window: TAbstractWindow): Boolean;
    Property    Windows[Index: Integer] : TAbstractWindow     Read GetWindow;
    Property    NumWindows              : Integer             Read GetNumWindows;
    Property    OnMoveToFront           : TMoveToFrontEvent   Read FOnMoveToFront Write FOnMoveToFront;
    Property    FocusedComponent        : TAbstractComponent  Read FFocusedComponent;
  End;

Var
  KeyboardHash : TIntegerPointerHash;

implementation

Uses OgreInput, SysUtils, Math;

Procedure SetupKeyboardHash;
Begin
  KeyboardHash := TIntegerPointerHash.Create(True);
  KeyboardHash.Put(KC_1,Pointer('1'));
  KeyboardHash.Put(KC_2,Pointer('2'));
  KeyboardHash.Put(KC_3,Pointer('3'));
  KeyboardHash.Put(KC_4,Pointer('4'));
  KeyboardHash.Put(KC_5,Pointer('5'));
  KeyboardHash.Put(KC_6,Pointer('6'));
  KeyboardHash.Put(KC_7,Pointer('7'));
  KeyboardHash.Put(KC_8,Pointer('8'));
  KeyboardHash.Put(KC_9,Pointer('9'));
  KeyboardHash.Put(KC_0,Pointer('0'));
  KeyboardHash.Put(KC_MINUS,Pointer('-'));
  KeyboardHash.Put(KC_EQUALS,Pointer('='));
  KeyboardHash.Put(KC_Q,Pointer('q'));
  KeyboardHash.Put(KC_W,Pointer('w'));
  KeyboardHash.Put(KC_E,Pointer('e'));
  KeyboardHash.Put(KC_R,Pointer('r'));
  KeyboardHash.Put(KC_T,Pointer('t'));
  KeyboardHash.Put(KC_Y,Pointer('y'));
  KeyboardHash.Put(KC_U,Pointer('u'));
  KeyboardHash.Put(KC_I,Pointer('i'));
  KeyboardHash.Put(KC_O,Pointer('o'));
  KeyboardHash.Put(KC_P,Pointer('p'));
  KeyboardHash.Put(KC_LBRACKET,Pointer('['));
  KeyboardHash.Put(KC_RBRACKET,Pointer(']'));
  KeyboardHash.Put(KC_A,Pointer('a'));
  KeyboardHash.Put(KC_S,Pointer('s'));
  KeyboardHash.Put(KC_D,Pointer('d'));
  KeyboardHash.Put(KC_F,Pointer('f'));
  KeyboardHash.Put(KC_G,Pointer('g'));
  KeyboardHash.Put(KC_H,Pointer('h'));
  KeyboardHash.Put(KC_J,Pointer('j'));
  KeyboardHash.Put(KC_K,Pointer('k'));
  KeyboardHash.Put(KC_L,Pointer('l'));
  KeyboardHash.Put(KC_SEMICOLON,Pointer(';'));
  KeyboardHash.Put(KC_APOSTROPHE,Pointer(''''));
  KeyboardHash.Put(KC_GRAVE,Pointer('`'));
  KeyboardHash.Put(KC_BACKSLASH,Pointer('\'));
  KeyboardHash.Put(KC_Z,Pointer('z'));
  KeyboardHash.Put(KC_X,Pointer('x'));
  KeyboardHash.Put(KC_C,Pointer('c'));
  KeyboardHash.Put(KC_V,Pointer('v'));
  KeyboardHash.Put(KC_B,Pointer('b'));
  KeyboardHash.Put(KC_N,Pointer('n'));
  KeyboardHash.Put(KC_M,Pointer('m'));
  KeyboardHash.Put(KC_COMMA,Pointer(','));
  KeyboardHash.Put(KC_PERIOD,Pointer('.'));
  KeyboardHash.Put(KC_SLASH,Pointer('/'));
  KeyboardHash.Put(KC_SPACE,Pointer(' '));

  KeyboardHash.Put(ShiftFlag + KC_1,Pointer('!'));
  KeyboardHash.Put(ShiftFlag + KC_2,Pointer('@'));
  KeyboardHash.Put(ShiftFlag + KC_3,Pointer('#'));
  KeyboardHash.Put(ShiftFlag + KC_4,Pointer('$'));
  KeyboardHash.Put(ShiftFlag + KC_5,Pointer('%'));
  KeyboardHash.Put(ShiftFlag + KC_6,Pointer('^'));
  KeyboardHash.Put(ShiftFlag + KC_7,Pointer('&'));
  KeyboardHash.Put(ShiftFlag + KC_8,Pointer('*'));
  KeyboardHash.Put(ShiftFlag + KC_9,Pointer('('));
  KeyboardHash.Put(ShiftFlag + KC_0,Pointer(')'));
  KeyboardHash.Put(ShiftFlag + KC_MINUS,Pointer('_'));
  KeyboardHash.Put(ShiftFlag + KC_EQUALS,Pointer('+'));
  KeyboardHash.Put(ShiftFlag + KC_Q,Pointer('Q'));
  KeyboardHash.Put(ShiftFlag + KC_W,Pointer('W'));
  KeyboardHash.Put(ShiftFlag + KC_E,Pointer('E'));
  KeyboardHash.Put(ShiftFlag + KC_R,Pointer('R'));
  KeyboardHash.Put(ShiftFlag + KC_T,Pointer('T'));
  KeyboardHash.Put(ShiftFlag + KC_Y,Pointer('Y'));
  KeyboardHash.Put(ShiftFlag + KC_U,Pointer('U'));
  KeyboardHash.Put(ShiftFlag + KC_I,Pointer('I'));
  KeyboardHash.Put(ShiftFlag + KC_O,Pointer('O'));
  KeyboardHash.Put(ShiftFlag + KC_P,Pointer('P'));
  KeyboardHash.Put(ShiftFlag + KC_LBRACKET,Pointer('{'));
  KeyboardHash.Put(ShiftFlag + KC_RBRACKET,Pointer('}'));
  KeyboardHash.Put(ShiftFlag + KC_A,Pointer('A'));
  KeyboardHash.Put(ShiftFlag + KC_S,Pointer('S'));
  KeyboardHash.Put(ShiftFlag + KC_D,Pointer('D'));
  KeyboardHash.Put(ShiftFlag + KC_F,Pointer('F'));
  KeyboardHash.Put(ShiftFlag + KC_G,Pointer('G'));
  KeyboardHash.Put(ShiftFlag + KC_H,Pointer('H'));
  KeyboardHash.Put(ShiftFlag + KC_J,Pointer('J'));
  KeyboardHash.Put(ShiftFlag + KC_K,Pointer('K'));
  KeyboardHash.Put(ShiftFlag + KC_L,Pointer('L'));
  KeyboardHash.Put(ShiftFlag + KC_SEMICOLON,Pointer(':'));
  KeyboardHash.Put(ShiftFlag + KC_APOSTROPHE,Pointer('"'));
  KeyboardHash.Put(ShiftFlag + KC_GRAVE,Pointer('~'));
  KeyboardHash.Put(ShiftFlag + KC_BACKSLASH,Pointer('|'));
  KeyboardHash.Put(ShiftFlag + KC_Z,Pointer('Z'));
  KeyboardHash.Put(ShiftFlag + KC_X,Pointer('X'));
  KeyboardHash.Put(ShiftFlag + KC_C,Pointer('C'));
  KeyboardHash.Put(ShiftFlag + KC_V,Pointer('V'));
  KeyboardHash.Put(ShiftFlag + KC_B,Pointer('B'));
  KeyboardHash.Put(ShiftFlag + KC_N,Pointer('N'));
  KeyboardHash.Put(ShiftFlag + KC_M,Pointer('M'));
  KeyboardHash.Put(ShiftFlag + KC_COMMA,Pointer('<'));
  KeyboardHash.Put(ShiftFlag + KC_PERIOD,Pointer('>'));
  KeyboardHash.Put(ShiftFlag + KC_SLASH,Pointer('?'));
  KeyboardHash.Put(ShiftFlag + KC_SPACE,Pointer(' '));
End; // SetupKeyboardHash

// -------------------------------
// TAbstractComponent
// -------------------------------

Constructor TAbstractComponent.Create(AParent: TAbstractComponent; AComponentType: TAbstractComponentType);
Begin
  FLeft          := 0;
  FTop           := 0;
  FWidth         := 64;
  FHeight        := 64;
  FParent        := AParent;
  FComponentType := AComponentType;
  FReadOnly      := False;
  BasicInit;
End; // TAbstractComponent.Create

Constructor TAbstractComponent.Create(AParent: TAbstractComponent; AComponentType: TAbstractComponentType; ALeft,ATop,AWidth,AHeight: Integer);
Begin
  FLeft          := ALeft;
  FTop           := ATop;
  FWidth         := AWidth;
  FHeight        := AHeight;
  FParent        := AParent;
  FComponentType := AComponentType;
  FMinWidth      := 0;
  FMinHeight     := 0;
  FReadOnly      := False;
  BasicInit;
End; // TAbstractComponent.Create

Procedure TAbstractComponent.BasicInit;
Begin
  FVisible         := True;
  FEnabled         := True;
  FChildren        := TStringList.Create;
  FAlign           := [alLeft,alTop];
  FForeground      := TColor($00FFFFFF);
  FBackground      := TColor($00000000);
  FFocused         := False;
  FChanging        := False;
  FMouseOver       := False;
  If FParent <> Nil Then
  Begin
    FUI    := FParent.UI;
    FOwner := FParent.Owner;
  End
  Else
  Begin
    FUI    := Nil;
    FOwner := Nil;
  End;
  CalcClientExtents;
//  FMutex           := TCriticalSection.Create;
  FTag             := 0;
  FAcceptEvents    := [evtMouseDown,evtMouseUp,evtMouseMove];
  FOnMouseEnter    := Nil;
  FOnMouseExit     := Nil;
  FOnKeyDown       := Nil;
  FOnPaint         := Nil;
  FOnBeforePaint   := Nil;
  FOnAfterPaint    := Nil;
  FOnMouseDown     := Nil;
  FOnClose         := Nil;
  FOnCloseQuery    := Nil;
  FOnLostFocus     := Nil;
  FTransparent     := False;
  FClipRect.Left   := 0;
  FClipRect.Top    := 0;
  FClipRect.Right  := 0;
  FClipRect.Bottom := 0;
  If FParent <> Nil Then FParent.AddChild(Self);
End; // TAbstractComponent.BasicInit

Destructor TAbstractComponent.Destroy;
Var I: Integer;
Begin
  For I := 0 To FChildren.Count - 1 Do FChildren.Objects[I].Free;
  FChildren.Free;
//  FMutex.Free;
  Inherited;
End; // TAbstractComponent.Destroy

Function TAbstractComponent.GetComponentType: TAbstractComponentType;
Begin
  Result := FComponentType;
End; // TAbstractComponent.GetComponentType

Procedure TAbstractComponent.Changing;
Begin
  FChanging := True;
End; // TAbstractComponent.Changing

Procedure TAbstractComponent.Changed;
Begin
  FChanging := False;
End; // TAbstractComponent.Changed

Function TAbstractComponent.GetComponentCount: Integer;
Begin
  Try
    FUI.LockWindowList;
    Result := FChildren.Count;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.GetComponentCount

Function TAbstractComponent.GetComponent(Index: Integer): TAbstractComponent;
Begin
  Try
    FUI.LockWindowList;
    If (Index >= 0) And (Index < FChildren.Count)
     Then Result := TAbstractComponent(FChildren.Objects[Index])
     Else Result := Nil;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.GetComponent

Procedure TAbstractComponent.RepaintParent;
Begin
  If FParent <> Nil Then FParent.QueueRepaint Else QueueRepaint;
End; // TAbstractComponent.RepaintParent
{
Function TAbstractComponent.GetUI: TAbstractUI;
Begin
  Result := FUI;
End; // TAbstractComponent.GetUI

Function TAbstractComponent.GetOwner: TAbstractWindow;
Begin
  Result := FOwner;
End; // TAbstractComponent.GetOwner

Function TAbstractComponent.GetLeft: Integer;
Begin
  Result := FLeft;
End; // TAbstractComponent.GetLeft

Function TAbstractComponent.GetTop: Integer;
Begin
  Result := FTop;
End; // TAbstractComponent.GetTop

Function TAbstractComponent.GetWidth: Integer;
Begin
  Result := FWidth;
End; // TAbstractComponent.GetWidth

Function TAbstractComponent.GetHeight: Integer;
Begin
  Result := FHeight;
End; // TAbstractComponent.GetHeight

Function TAbstractComponent.GetClientLeft: Integer;
Begin
  Result := FClientLeft;
End; // TAbstractComponent.GetClientLeft

Function TAbstractComponent.GetClientTop: Integer;
Begin
  Result := FClientTop;
End; // TAbstractComponent.GetClientTop

Function TAbstractComponent.GetParent: IAbstractComponent;
Begin
  Result := FParent;
End; // TAbstractComponent.GetParent
}
Procedure TAbstractComponent.SetLeft(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    If FLeft <> I Then
    Begin
      FLeft := I;
      CalcClientExtents;
      RepaintParent;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetLeft

Procedure TAbstractComponent.SetTop(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    If FTop <> I Then
    Begin
      FTop := I;
      CalcClientExtents;
      RepaintParent;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetTop

Procedure TAbstractComponent.SetWidth(I: Integer);
Var J,K: Integer;
Begin
  Try
    FUI.LockWindowList;
    I := Max(FMinWidth,I);
    If FWidth <> I Then
    Begin
      K      := FWidth;
      FWidth := I;
      CalcClientExtents;
      AfterSetSize;
      For J := 0 To FChildren.Count - 1 Do TAbstractComponent(FChildren.Objects[J]).Realign(K,FHeight);
      RepaintParent;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetWidth

Procedure TAbstractComponent.SetHeight(I: Integer);
Var J,K: Integer;
Begin
  Try
    FUI.LockWindowList;
    I := Max(FMinHeight,I);
    If FHeight <> I Then
    Begin
      K       := FHeight;
      FHeight := I;
      CalcClientExtents;
      AfterSetSize;
      For J := 0 To FChildren.Count - 1 Do TAbstractComponent(FChildren.Objects[J]).Realign(FWidth,K);
      RepaintParent;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetHeight

Procedure TAbstractComponent.SetMinWidth(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    I := Max(0,I);
    If FMinWidth <> I Then
    Begin
      FMinWidth := I;
      Width     := Max(Width,FMinWidth);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetMinWidth

Procedure TAbstractComponent.SetMinHeight(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    I := Max(0,I);
    If FMinHeight <> I Then
    Begin
      FMinHeight := I;
      Height     := Max(Height,FMinHeight);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetMinHeight

Procedure TAbstractComponent.SetVisible(B: Boolean);
Var CanClose: Boolean;
Begin
  Try
    FUI.LockWindowList;
    If FVisible <> B Then
    Begin
      CanClose := True;
      If Assigned(FOnCloseQuery) And Not B Then FOnCloseQuery(Self,CanClose);
      If CanClose Then
      Begin
        FVisible := B;
        If FVisible Then QueueRepaint Else RepaintParent;  // Want to use the parent's background color
        If Assigned(FOnClose) And Not B Then FOnClose(Self);
        If Assigned(FOnShow) And B Then FOnShow(Self);
        If Not B Then Focused := False;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetVisible

Procedure TAbstractComponent.InternalSetEnabled(B: Boolean);
Begin
  // Base class does nothing
End; // TAbstractComponent.InternalSetEnabled

Procedure TAbstractComponent.SetEnabled(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If FEnabled <> B Then
    Begin
      FEnabled := B;
      InternalSetEnabled(B);
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetEnabled

Procedure TAbstractComponent.SetForeground(C: TColor);
Begin
  Try
    FUI.LockWindowList;
    If C <> FForeground Then
    Begin
      FForeground := C;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetForeground

Procedure TAbstractComponent.SetBackground(C: TColor);
Begin
  Try
    FUI.LockWindowList;
    If C <> FBackground Then
    Begin
      FBackground := C;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetBackground

Procedure TAbstractComponent.AddChild(Child: TAbstractComponent);
Begin
  Try
    FUI.LockWindowList;
    FChildren.AddObject('',Child);
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.AddChild

Procedure TAbstractComponent.CalcAbsoluteXY(Out X,Y: Integer);
Var P: TAbstractComponent;
Begin
  P := FParent;
  If P = Nil Then
  Begin
    X := 0;
    Y := 0;
  End
  Else
  Begin
    X := FLeft;
    Y := FTop;
  End;
  While P <> Nil Do
  Begin
    Inc(X,P.ClientLeft);
    Inc(Y,P.ClientTop);
    If P.Parent <> Nil Then
    Begin
      Inc(X,P.Left);
      Inc(Y,P.Top);
    End;
    P := P.Parent;
  End; // While
End; // TAbstractComponent.CalcAbsoluteXY

Procedure TAbstractComponent.CreateChildren;
Begin
End; // TAbstractComponent.CreateChildren

Procedure TAbstractComponent.Paint(X,Y: Integer);
Begin
End; // TAbstractComponent.Paint

Function TAbstractComponent.GetChildClipRect(ChildComponent: TAbstractComponent): TRect;
Var
  X,Y   : Integer;
  Rect  : TRect;
  Rect1 : TRect;

Begin
  CalcAbsoluteXY(X,Y);
  Rect.Left   := X + ClientLeft;
  Rect.Top    := Y + ClientTop;
  Rect.Right  := Rect.Left + ClientWidth  - 1;
  Rect.Bottom := Rect.Top  + ClientHeight - 1;
  If FParent <> Nil Then
  Begin
    Rect1       := FParent.GetChildClipRect(Self);
    Rect.Left   := Max(Rect.Left,   Rect1.Left);
    Rect.Top    := Max(Rect.Top,    Rect1.Top);
    Rect.Right  := Min(Rect.Right,  Rect1.Right);
    Rect.Bottom := Min(Rect.Bottom, Rect1.Bottom);
  End;
  Result := Rect;
End; // TAbstractComponent.GetChildClipRect

Function TAbstractComponent.GetClipRect: TRect;
Var
  X,Y   : Integer;
  Rect  : TRect;
  Rect1 : TRect;

Begin
  CalcAbsoluteXY(X,Y);
  Rect.Left   := X;
  Rect.Top    := Y;
  Rect.Right  := X + FWidth  - 1;
  Rect.Bottom := Y + FHeight - 1;
  If FParent <> Nil Then
  Begin
    Rect1       := FParent.GetChildClipRect(Self);
    Rect.Left   := Max(Rect.Left,   Rect1.Left);
    Rect.Top    := Max(Rect.Top,    Rect1.Top);
    Rect.Right  := Min(Rect.Right,  Rect1.Right);
    Rect.Bottom := Min(Rect.Bottom, Rect1.Bottom);
  End;
  Result := Rect;
End; // TAbstractComponent.GetClipRect

Procedure TAbstractComponent.BeforePaintingChildren;
Begin
End; // TAbstractComponent.BeforePaintingChildren

Procedure TAbstractComponent.AfterPaintingChildren;
Begin
End; // TAbstractComponent.AfterPaintingChildren

Function TAbstractComponent.TextWidth(Const St: String): Integer;
Begin
  If FOwner <> Nil
   Then Result := FOwner.TextWidth(St)
   Else Result := 0;
End; // TAbstractComponent.TextWidth

Function TAbstractComponent.TextHeight(Const St: String): Integer;
Begin
  If FOwner <> Nil
   Then Result := FOwner.TextHeight(St)
   Else Result := 0;
End; // TAbstractComponent.TextHeight

Procedure TAbstractComponent.TextOut(X,Y: Integer; Const St: String);
Begin
  If FOwner <> Nil Then FOwner.TextOut(X,Y,St);
End; // TAbstractComponent.TextOut

Procedure TAbstractComponent.BeginClip(Rect: TRect);
Begin
  If FOwner <> Nil Then FOwner.BeginClip(Rect);
End; // TAbstractComponent.BeginClip

Procedure TAbstractComponent.EndClip;
Begin
  If FOwner <> Nil Then FOwner.EndClip;
End; // TAbstractComponent.EndClip

Procedure TAbstractComponent.FillBackground;
Begin
End; // TAbstractComponent.FillBackground

Procedure TAbstractComponent.Repaint;
Var
  I,X,Y       : Integer;
  Rect        : TRect;
  OldClipRect : TRect;

Begin
  Try
    FUI.LockWindowList;

    // Clip to this component's extents

    CalcAbsoluteXY(X,Y);
    Rect        := GetClipRect;
    OldClipRect := FClipRect;
    FClipRect   := Rect;
    BeginClip(Rect);

    // Paint the background

    FillBackground;
    If FVisible Then
    Begin
      If Assigned(FOnBeforePaint) Then FOnBeforePaint(Self,X,Y);

      Paint(X,Y);

      If Assigned(FOnAfterPaint) Then FOnAfterPaint(Self,X,Y);

      // Paint the child components

      BeforePaintingChildren;
      For I := 0 To FChildren.Count - 1 Do
      Begin
        TAbstractComponent(FChildren.Objects[I]).Repaint;
        FClipRect := Rect;
      End; // For I
      AfterPaintingChildren;

      // See if the optional event handler was set

      If Assigned(FOnPaint) Then FOnPaint(Self,X,Y);
    End;
    FClipRect := OldClipRect;
    EndClip;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.Repaint

Procedure TAbstractComponent.SetTransparent(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    FTransparent := B;
    QueueRepaint;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetTransparent

Procedure TAbstractComponent.SetAlign(AAlign: TAlignSet);
Begin
  Try
    FUI.LockWindowList;
    FAlign := AAlign;
    QueueRepaint;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetAlign

Procedure TAbstractComponent.SetReadOnly(AReadOnly: Boolean);
Begin
  Try
    FUI.LockWindowList;
    FReadOnly := AReadOnly;
    QueueRepaint;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetReadOnly

Procedure TAbstractComponent.Realign(OldWidth,OldHeight: Integer);
Begin
  If FParent <> Nil Then
  Begin
    If (FParent.Width <> OldWidth) And ([alRight,alClient] * FAlign <> []) Then
    Begin
      If [alLeft,alClient] * FAlign <> [] Then
      Begin
        Width := Width + FParent.Width - OldWidth;
        If FWidth < 0 Then FWidth := 0;
      End
      Else
      Begin
        Inc(FLeft,FParent.Width - OldWidth);
      End;
    End;
    If (FParent.Height <> OldHeight) And ([alBottom,alClient] * FAlign <> []) Then
    Begin
      If [alTop,alClient] * FAlign <> [] Then
      Begin
        Height := Height + FParent.Height - OldHeight;
        If FHeight < 0 Then FHeight := 0;
      End
      Else
      Begin
        Inc(FTop,FParent.Height - OldHeight);
      End;
    End;
  End;
End; // TAbstractComponent.Realign

Procedure TAbstractComponent.AfterSetSize;
Begin
End; // TAbstractComponent.AfterSetSize

Procedure TAbstractComponent.CalcClientExtents;
Begin
  FClientLeft   := 0;
  FClientTop    := 0;
  FClientWidth  := FWidth;
  FClientHeight := FHeight;
End; // TAbstractComponent.CalcClientExtents

Function TAbstractComponent.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
Var
  I    : Integer;
  B    : Boolean;
  Rect : TRect;

Begin
  B := False;
  Try
    FUI.LockWindowList;
    If FVisible And (evtMouseDown In FAcceptEvents) Then
    Begin
      If FParent = Nil Then
      Begin
        Dec(X,FLeft);
        Dec(Y,FTop);
      End;
      I := 0;
      While (I < FChildren.Count) And Not B Do
      Begin
        B := B Or TAbstractComponent(FChildren.Objects[I]).MouseDown(Button,Shift,X,Y);
        Inc(I);
      End; // While
      If Not B Then
      Begin
        Rect := GetClipRect;
        B    := B Or ((X >= Rect.Left) And (Y >= Rect.Top) And (X <= Rect.Right) And (Y <= Rect.Bottom));
        If B And Not FReadOnly Then
        Begin
          Focused := True;
          MouseDownAction(Button,Shift,X,Y);
        End;
        If B And Assigned(FOnMouseDown) Then FOnMouseDown(Self,Button,Shift,X,Y);
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
  Result := B;
End; // TAbstractComponent.MouseDown

Function TAbstractComponent.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
Var
  I    : Integer;
  B    : Boolean;
  Rect : TRect;

Begin
  B := False;
  Try
    FUI.LockWindowList;
    If FVisible And (evtMouseUp In FAcceptEvents) Then
    Begin
      If FParent = Nil Then
      Begin
        Dec(X,FLeft);
        Dec(Y,FTop);
      End;
      I := 0;
      While (I < FChildren.Count) And Not B Do
      Begin
        B := B Or TAbstractComponent(FChildren.Objects[I]).MouseUp(Button,Shift,X,Y);
        Inc(I);
      End; // While
      If Not B Then
      Begin
        Rect := GetClipRect;
        B    := B Or ((X >= Rect.Left) And (Y >= Rect.Top) And (X <= Rect.Right) And (Y <= Rect.Bottom));
        If B And Not FReadOnly Then
        Begin
          MouseUpAction(Button,Shift,X,Y);
        End;
        If B And Assigned(FOnMouseUp) Then FOnMouseUp(Self,Button,Shift,X,Y);
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
  Result := B;
End; // TAbstractComponent.MouseUp

Function TAbstractComponent.MouseMove(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor): Boolean;
Var
  I    : Integer;
  B    : Boolean;
  Rect : TRect;

Begin
  B := False;
  Try
    FUI.LockWindowList;
    If FVisible And (evtMouseMove In FAcceptEvents) And Not FReadOnly Then
    Begin
      If FParent = Nil Then
      Begin
        Dec(X,FLeft);
        Dec(Y,FTop);
      End;
      I := 0;
      While (I < FChildren.Count) And Not B Do
      Begin
        B := B Or TAbstractComponent(FChildren.Objects[I]).MouseMove(Shift,X,Y,Cursor);
        Inc(I);
      End; // While
      If Not B Then
      Begin
        Rect := GetClipRect;
        B    := B Or ((X >= Rect.Left) And (Y >= Rect.Top) And (X <= Rect.Right) And (Y <= Rect.Bottom));
        If B Then
        Begin
          MouseMoveAction(Shift,X,Y,Cursor);
        End;
      End;
      If B <> FMouseOver Then
      Begin
        FMouseOver := B;
             If Assigned(FOnMouseEnter) And     B Then FOnMouseEnter(Self)
        Else If Assigned(FOnMouseExit)  And Not B Then FOnMouseExit(Self);
        QueueRepaint;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
  Result := B;
End; // TAbstractComponent.MouseMove

Function TAbstractComponent.ContainsCursor(X, Y: Integer): TAbstractComponent;
Var
  I    : Integer;
  Comp : TAbstractComponent;
  Rect : TRect;

Begin
  Comp := Nil;
  Try
    FUI.LockWindowList;
    If FVisible Then
    Begin
      If FParent = Nil Then
      Begin
        Dec(X,FLeft);
        Dec(Y,FTop);
      End;
      I := 0;
      While (I < FChildren.Count) And (Comp = Nil) Do
      Begin
        Comp := TAbstractComponent(FChildren.Objects[I]).ContainsCursor(X,Y);
        Inc(I);
      End; // While
      If Comp = Nil Then
      Begin
        Rect := GetClipRect;
        If (X >= Rect.Left) And (Y >= Rect.Top) And (X <= Rect.Right) And (Y <= Rect.Bottom) Then Comp := Self;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
  Result := Comp;
End; // TAbstractComponent.ContainsCursor

Procedure TAbstractComponent.MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
End; // TAbstractComponent.MouseDownAction

Procedure TAbstractComponent.MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
End; // TAbstractComponent.MouseUpAction

Procedure TAbstractComponent.MouseMoveAction(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor);
Begin
End; // TAbstractComponent.MouseMoveAction

Procedure TAbstractComponent.SetUI(AUI: TAbstractUI);
Var I: Integer;
Begin
  Try
    FUI.LockWindowList;
    FUI := AUI;
    If FParent = Nil Then
    Begin
      For I := 0 To FChildren.Count - 1 Do TAbstractComponent(FChildren.Objects[I]).UI := AUI;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetUI

Procedure TAbstractComponent.SetFocused(B: Boolean);
Var OldFocusedComponent: TAbstractComponent;
Begin
  Try
    FUI.LockWindowList;
    If B <> FFocused Then
    Begin
      If B Then
      Begin
        If FUI <> Nil Then
        Begin
          OldFocusedComponent := FUI.FFocusedComponent;
          FUI.FFocusedComponent := Self;          // Have to do this first for the combo box to work
          If OldFocusedComponent <> Nil Then OldFocusedComponent.Focused := False;
        End;
        FFocused := True;
        QueueRepaint;
      End
      Else
      Begin
        FFocused := False;
        If Assigned(FOnLostFocus) Then FOnLostFocus(Self);
        If (FUI <> Nil) And (FUI.FFocusedComponent = Self) Then FUI.FFocusedComponent := Nil;
        QueueRepaint;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractComponent.SetFocused

Function TAbstractComponent.KeyDown(Key,Shift: Integer; C: Char): Boolean;
Var
  B      : Boolean;
  I      : Integer;
  Child  : TAbstractComponent;
  Key1   : Word;
  Shift1 : TShiftState;

Begin
  B := False;
  Try
    FUI.LockWindowList;
    If FVisible And Not FReadOnly Then
    Begin
      I := 0;
      While (I < FChildren.Count) And Not B Do
      Begin
        Child := TAbstractComponent(FChildren.Objects[I]);
        If (Child.Focused Or (Child.FChildren.Count > 0)) And Not B Then
        Begin
          B := B Or Child.KeyDownAction(Key,Shift,C);
          If B And Assigned(Child.FOnKeyDown) Then
          Begin
            Key1 := Key;
            Shift1 := [];
            If (Shift And ShiftFlag) <> 0 Then Shift1 := Shift1 + [ssShift];
            Child.FOnKeyDown(Child,Key1,Shift1);
          End;
        End;
        Inc(I);
      End; // While
      If Focused And Not B Then
      Begin
        B := B Or KeyDownAction(Key,Shift,C);
        If B And Assigned(FOnKeyDown) Then
        Begin
          Key1 := Key;
          Shift1 := [];
          If (Shift And ShiftFlag) <> 0 Then Shift1 := Shift1 + [ssShift];
          FOnKeyDown(Self,Key1,Shift1);
        End;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
  Result := B;
End; // TAbstractComponent.KeyDown

Function TAbstractComponent.KeyDownAction(Key,Shift: Integer; C: Char): Boolean;
Begin
  Result := False;
End; // TAbstractComponent.KeyDownAction

Procedure TAbstractComponent.QueueRepaint;
Begin
End; // TAbstractComponent.QueueRepaint

Procedure TAbstractComponent.Rectangle(X1,Y1,X2,Y2: Integer; FillColor: TColor);
Begin
End; // TAbstractComponent.Rectangle

// -------------------------------
// TAbstractButton
// -------------------------------

Constructor TAbstractButton.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TAbstractButtonKind;
                                   AStyle: TAbstractButtonStyle; ACaption: String);
Begin
  Inherited Create(AParent,ctButton,ALeft,ATop,AWidth,AHeight);
  FKind       := AKind;
  FStyle      := AStyle;
  FDown       := False;
  FPressed    := False;
  FFlat       := False;
  FGroup      := 0;
  FAllowAllUp := True;
  FOnClick    := Nil;
  FModalRes   := mrNone;
  CreateChildren;
  FLabel.AcceptEvents := [];
  FLabel.Caption      := ACaption;
  FLabel.Alignment    := taCenter;
  AdjustLabelPosition;
End; // TAbstractButton.Create

Procedure TAbstractButton.CreateChildren;
Begin
  FLabel := TAbstractLabel.Create(Self,0,0,'',True);
End; // TAbstractButton.CreateChildren

Procedure TAbstractButton.InternalSetEnabled(B: Boolean);
Begin
  If B
   Then FLabel.Foreground := FForeground
   Else FLabel.Foreground := DefaultDisabledForeground;
End; // TAbstractButton.InternalSetEnabled

Procedure TAbstractButton.SetWordWrapped(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If FLabel.WordWrapped <> B Then
    Begin
      If B Then FLabel.Width := FWidth - 6; // Must pre-set the width so the label knows how to word wrap
      FLabel.WordWrapped := B;
      AdjustLabelPosition;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractButton.SetWordWrapped

Function TAbstractButton.GetWordWrapped: Boolean;
Begin
  Result := FLabel.WordWrapped;
End; // TAbstractButton.GetWordWrapped

Procedure TAbstractButton.AdjustLabelPosition;
Begin
  FLabel.Left := (FWidth  - FLabel.Width) Div 2;
  FLabel.Top  := (FHeight - FLabel.Height) Div 2;
End; // TAbstractButton.AdjustLabelPosition

Procedure TAbstractButton.SetFlat(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If B <> FFlat Then
    Begin
      FFlat := B;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractButton.SetFlat

Procedure TAbstractButton.SetCaption(St: String);
Begin
  Try
    FUI.LockWindowList;
    If St <> FLabel.Caption Then
    Begin
      FLabel.Caption := St;
      AdjustLabelPosition;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractButton.SetCaption

Function TAbstractButton.GetCaption: String;
Begin
  Result := FLabel.Caption;
End; // TAbstractButton.GetCaption

Procedure TAbstractButton.SetKind(AKind: TAbstractButtonKind);
Begin
  Try
    FUI.LockWindowList;
    If AKind <> FKind Then
    Begin
      FKind := AKind;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractButton.SetKind

Procedure TAbstractButton.SetDown(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If B <> FDown Then
    Begin
      FDown := B;
      QueueRepaint;
      If ((B And (FStyle = cbsCheck)) Or
          ((Not B) And (FStyle = cbsButton))) Then Click;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractButton.SetDown

Procedure TAbstractButton.SetStyle(AStyle: TAbstractButtonStyle);
Begin
  Try
    FUI.LockWindowList;
    If AStyle <> FStyle Then
    Begin
      FStyle := AStyle;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractButton.SetStyle

Procedure TAbstractButton.MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{Var
  I         : Integer;
  Component : TAbstractComponent;
  AButton   : TAbstractButton;}

Begin
  Try
    FUI.LockWindowList;
    If FEnabled Then
    Begin
      FPressed := True;
      If FStyle <> cbsCheck
       Then Down := True
       Else QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractButton.MouseDownAction

Procedure TAbstractButton.MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  AAU       : Boolean;
  I         : Integer;
  Component : TAbstractComponent;
  AButton   : TAbstractButton;

Begin
  Try
    FUI.LockWindowList;
    If FEnabled Then
    Begin
      If FStyle <> cbsCheck Then
      Begin
        FPressed := False;
        Down     := False;
      End
      Else
      Begin
        If FDown Then
        Begin
          AAU := self.FAllowAllUp;
          If AAU Then
          Begin
            // Negative group number is used by checkboxes

            If FGroup >= 0 Then
            Begin
              For I := 0 To FParent.ComponentCount - 1 Do
              Begin
                Component := FParent.Components[I];
                If Component Is TAbstractButton Then
                Begin
                  AButton := TAbstractButton(Component);
                  If (AButton <> Self) And (AButton.Group = Group) And (AButton.Style = cbsCheck) Then AAU := AAU And AButton.AllowAllUp;
                End;
              End; // For I
            End;
            If AAU Then
            Begin
              FPressed := False;
              Down     := False;
            End
            Else
            Begin
              FPressed := False;
              QueueRepaint;
            End;
          End
          Else
          Begin
            FPressed := False;
            QueueRepaint;
          End;
        End
        Else
        Begin
          // Negative group number is used by checkboxes

          If FGroup >= 0 Then
          Begin
            If FParent <> Nil Then
            Begin
              For I := 0 To FParent.ComponentCount - 1 Do
              Begin
                Component := FParent.Components[I];
                If Component Is TAbstractButton Then
                Begin
                  AButton := TAbstractButton(Component);
                  If (AButton <> Self) And (AButton.Group = Group) And (AButton.Style = cbsCheck) Then AButton.Down := False;
                End;
              End; // For I
            End;
          End;  
          FPressed := False;
          Down := True;
        End;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractButton.MouseUpAction

Procedure TAbstractButton.Click;
Begin
  If (FOwner <> Nil) And (FOwner Is TAbstractWindow) Then FOwner.FModalResult := FModalRes; // Must do this first since OnClick could close the window
  If Assigned(FOnClick) Then FOnClick(Self);
  If (FModalRes <> mrNone) And (FOwner <> Nil) And (FOwner Is TAbstractWindow) Then FOwner.Hide;
End; // TAbstractButton.Click

// -------------------------------
// TAbstractEdit
// -------------------------------

Constructor TAbstractEdit.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AText: String; AMaxLen: Integer);
Begin
  Inherited Create(AParent,ctEdit,ALeft,ATop,AWidth,AHeight);
  FText       := AText;
  FMaxLen     := AMaxLen;
  FShowBorder := True;
  FOnChange   := Nil;
End; // TAbstractEdit.Create

Procedure TAbstractEdit.SetText(St: String);
Begin
  Try
    FUI.LockWindowList;
    If St <> FText Then
    Begin
      If FMaxLen > 0 Then St := Copy(St,1,FMaxLen);
      FText := St;
      QueueRepaint;
      If Assigned(FOnChange) Then FOnChange(Self);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractEdit.SetText

Function TAbstractEdit.KeyDownAction(Key,Shift: Integer; C: Char): Boolean;
Begin
  If C <> #0 Then Text := Text + C
  Else If Key = KC_BACK Then
  Begin
    If FText <> '' Then Text := Copy(FText,1,Length(FText) - 1);
  End;
  Result := True;
End; // TAbstractEdit.KeyDownAction

Procedure TAbstractEdit.SetShowBorder(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If FShowBorder <> B Then
    Begin
      FShowBorder := B;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractEdit.SetShowBorder

// -------------------------------
// TAbstractScrollBar
// -------------------------------

Constructor TAbstractScrollBar.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AKind: TScrollBarKind);
Begin
  Inherited Create(AParent,ctScrollBar,ALeft,ATop,AWidth,AHeight);
  FKind     := AKind;
  FMin      := 0;
  FMax      := 0;
  FPosition := 0;
  FPageSize := 1;
  FSmallInc := 1;
  FOnChange := Nil;
  FDragging := False;
  FDragX    := 0;
  FDragY    := 0;
  CreateChildren;
  FULButton.OnClick := ULButtonClick;
  FDRButton.OnClick := DRButtonClick;
End; // TAbstractScrollBar.Create

Constructor TAbstractScrollBar.Create(AParent: TAbstractComponent; ALeft,ATop,ALength: Integer; AKind: TScrollBarKind);
Begin
  If AKind = sbHorizontal
   Then Inherited Create(AParent,ctScrollBar,ALeft,ATop,ALength,ScrollBarThickness)
   Else Inherited Create(AParent,ctScrollBar,ALeft,ATop,ScrollBarThickness,ALength);
  FKind     := AKind;
  FMin      := 0;
  FMax      := 0;
  FPosition := 0;
  FPageSize := 1;
  FSmallInc := 1;
  FOnChange := Nil;
  FDragging := False;
  FDragX    := 0;
  FDragY    := 0;
  CreateChildren;
  If FKind = sbHorizontal Then
  Begin
    FULButton.Align := [alTop,alBottom,alLeft];
    FDRButton.Align := [alTop,alBottom,alRight];
  End
  Else
  Begin
    FULButton.Align := [alTop,alLeft,alRight];
    FDRButton.Align := [alBottom,alLeft,alRight];
  End;
  FULButton.OnClick := ULButtonClick;
  FDRButton.OnClick := DRButtonClick;
End; // TAbstractScrollBar.Create

Procedure TAbstractScrollBar.CreateChildren;
Begin
  If FKind = sbHorizontal Then
  Begin
    FULButton := TAbstractButton.Create(Self,1,1,ScrollBarThickness,FHeight - 2,cbkLeftArrow,cbsButton,'');
    FDRButton := TAbstractButton.Create(Self,FWidth - ScrollBarThickness - 1,1,ScrollBarThickness,FHeight - 2,cbkRightArrow,cbsButton,'');
  End
  Else
  Begin
    FULButton := TAbstractButton.Create(Self,1,1,FWidth - 2,ScrollBarThickness,cbkUpArrow,cbsButton,'');
    FDRButton := TAbstractButton.Create(Self,1,FHeight - ScrollBarThickness - 1,FWidth - 2,ScrollBarThickness,cbkDownArrow,cbsButton,'');
  End;
End; // TAbstractScrollBar.CreateChildren;

Procedure TAbstractScrollBar.AfterSetSize;
Begin
  If FKind = sbHorizontal
   Then FDRButton.Left := FWidth - ScrollBarThickness - 1
   Else FDRButton.Top := FHeight - ScrollBarThickness - 1;
End; // TAbstractScrollBar.AfterSetSize

Procedure TAbstractScrollBar.ULButtonClick(Sender: TObject);
Begin
  Try
    FUI.LockWindowList;
    If Position > 0 Then Position := Position - FSmallInc;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractScrollBar.ULButtonClick

Procedure TAbstractScrollBar.DRButtonClick(Sender: TObject);
Begin
  Try
    FUI.LockWindowList;
    If Position < Max Then Position := Position + FSmallInc;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractScrollBar.DRButtonClick

Procedure TAbstractScrollBar.SetMin(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    If (FMin <> I) And (I <= FMax) Then
    Begin
      FMin := I;
      QueueRepaint;
      If Assigned(FOnChange) Then FOnChange(Self);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractScrollBar.SetMin

Procedure TAbstractScrollBar.SetMax(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    If (FMax <> I) And (I >= FMin) Then
    Begin
      FMax := I;
      QueueRepaint;
      If Assigned(FOnChange) Then FOnChange(Self);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractScrollBar.SetMax

Procedure TAbstractScrollBar.SetPosition(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    I := Math.Max(FMin,I);
    I := Math.Min(FMax,I);
    If (FPosition <> I) And (I >= FMin) And (I <= FMax) Then
    Begin
      FPosition := I;
      QueueRepaint;
      If Assigned(FOnChange) Then FOnChange(Self);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractScrollBar.SetPosition

Procedure TAbstractScrollBar.SetPageSize(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    If (I <> FPageSize) And (I > 0) Then
    Begin
      FPageSize := I;
      QueueRepaint;
      If Assigned(FOnChange) Then FOnChange(Self);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractScrollBar.SetPageSize

Procedure TAbstractScrollBar.SetSmallInc(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    If (I <> FSmallInc) And (I > 0) Then
    Begin
      FSmallInc := I;
      QueueRepaint;
      If Assigned(FOnChange) Then FOnChange(Self);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractScrollBar.SetSmallInc

Procedure TAbstractScrollBar.MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  J     : Integer;
  XX,YY : Integer;
  J1,J2 : Integer;
  I1,I2 : Integer;

Begin
  Try
    FUI.LockWindowList;
    Inherited MouseDownAction(Button,Shift,X,Y);
    FDragging := False;
    If FEnabled Then
    Begin
      CalcAbsoluteXY(XX,YY);
      Dec(X,XX);
      Dec(Y,YY);
      If FKind = sbHorizontal Then
      Begin
        J1 := ScrollBarThickness + 1;
        J2 := FDRButton.FLeft;

        J  := FWidth - 2 - 2 * ScrollBarThickness;
        I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + FPageSize)));
        I2 := FPosition + FPageSize - 1;
        If I2 > FMax + FPageSize - 1 Then I2 := FMax + FPageSize - 1;
        I2 := Round(J * ((FMax + FPageSize - I2 - 1) / (FMax - FMin + FPageSize)));

        If (X >= J1 + I1) And (X <= J2 - I2) Then
        Begin
          FDragX    := X;
          FDragY    := Position;
          FDragging := True;
        End;
      End
      Else
      Begin
        J1 := ScrollBarThickness + 1;
        J2 := FDRButton.FTop;

        J  := FHeight - 2 - 2 * ScrollBarThickness;
        I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + FPageSize)));
        I2 := FPosition + FPageSize - 1;
        If I2 > FMax + FPageSize - 1 Then I2 := FMax + FPageSize - 1;
        I2 := Round(J * ((FMax + FPageSize - I2 - 1) / (FMax - FMin + FPageSize)));

        If (Y >= J1 + I1) And (Y <= J2 - I2) Then
        Begin
          FDragX    := Position;
          FDragY    := Y;
          FDragging := True;
        End;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractScrollBar.MouseDownAction

Procedure TAbstractScrollBar.MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  J     : Integer;
  XX,YY : Integer;
  J1,J2 : Integer;
  I1,I2 : Integer;
  D,U   : Boolean;

Begin
  Try
    FUI.LockWindowList;
    If FEnabled And Not FDragging Then
    Begin
      CalcAbsoluteXY(XX,YY);
      Dec(X,XX);
      Dec(Y,YY);
      If FKind = sbHorizontal Then
      Begin
        J1 := ScrollBarThickness + 1;
        J2 := FDRButton.FLeft;

        J  := FWidth - 2 - 2 * ScrollBarThickness;
        I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + FPageSize)));
        I2 := FPosition + FPageSize - 1;
        If I2 > FMax + FPageSize - 1 Then I2 := FMax + FPageSize - 1;
        I2 := Round(J * ((FMax + FPageSize - I2 - 1) / (FMax - FMin + FPageSize)));

        D := (X > J1)      And (X < J1 + I1);
        U := (X > J2 - I2) And (X < J2);
      End
      Else
      Begin
        J1 := ScrollBarThickness + 1;
        J2 := FDRButton.FTop;

        J  := FHeight - 2 - 2 * ScrollBarThickness;
        I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + FPageSize)));
        I2 := FPosition + FPageSize - 1;
        If I2 > FMax + FPageSize - 1 Then I2 := FMax + FPageSize - 1;
        I2 := Round(J * ((FMax + FPageSize - I2 - 1) / (FMax - FMin + FPageSize)));

        D := (Y > J1)      And (Y < J1 + I1);
        U := (Y > J2 - I2) And (Y < J2);
      End;
           If D Then Position := Math.Max(Position - PageSize,FMin)
      Else If U Then Position := Math.Min(Position + PageSize,FMax);
    End;
    FDragging := False;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractScrollBar.MouseUpAction

Procedure TAbstractScrollBar.MouseMoveAction(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor);
Var
  I,J   : Integer;
  XX,YY : Integer;
  J1,J2 : Integer;
  I1,I2 : Integer;

Begin
  Try
    FUI.LockWindowList;
    Inherited MouseMoveAction(Shift,X,Y,Cursor);
    If FEnabled And FDragging Then
    Begin
      CalcAbsoluteXY(XX,YY);
      Dec(X,XX);
      Dec(Y,YY);
      If FKind = sbHorizontal Then
      Begin
        J1 := ScrollBarThickness + 1;
        J2 := FDRButton.FLeft;

        J  := FWidth - 2 - 2 * ScrollBarThickness;
        I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + FPageSize)));
        I2 := FPosition + FPageSize - 1;
        If I2 > FMax + FPageSize - 1 Then I2 := FMax + FPageSize - 1;
        I2 := Round(J * ((FMax + FPageSize - I2 - 1) / (FMax - FMin + FPageSize)));

        If I1 + I2 > 0 Then
        Begin
          I := Round(FDragY + (FMax - FMin) * (X - FDragX) / (I1 + I2));
          I := Math.Max(I,FMin);
          I := Math.Min(I,FMax);
          Position := I;
        End;
      End
      Else
      Begin
        J1 := ScrollBarThickness + 1;
        J2 := FDRButton.FTop;

        J  := FHeight - 2 - 2 * ScrollBarThickness;
        I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + FPageSize)));
        I2 := FPosition + FPageSize - 1;
        If I2 > FMax + FPageSize - 1 Then I2 := FMax + FPageSize - 1;
        I2 := Round(J * ((FMax + FPageSize - I2 - 1) / (FMax - FMin + FPageSize)));

        If I1 + I2 > 0 Then
        Begin
          I := Round(FDragX + (FMax - FMin) * (Y - FDragY) / (I1 + I2));
          I := Math.Max(I,FMin);
          I := Math.Min(I,FMax);
          Position := I;
        End;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractScrollBar.MouseMoveAction

// -------------------------------
// TAbstractTrackBar
// -------------------------------

Constructor TAbstractTrackBar.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
Begin
  Inherited Create(AParent,ctTrackBar,ALeft,ATop,AWidth,AHeight);
  FMin           := 0;
  FMax           := 0;
  FPosition      := 0;
  FOnChange      := Nil;
  FDragging      := False;
  FDragX         := 0;
  FDragY         := 0;
  FStep          := 1;
  FDragWidth     := 2;
  FAssociate     := Nil;
End; // TAbstractTrackBar.Create

Constructor TAbstractTrackBar.Create(AParent: TAbstractComponent; ALeft,ATop,ALength: Integer);
Begin
  Inherited Create(AParent,ctTrackBar,ALeft,ATop,ALength,ScrollBarThickness);
  FMin       := 0;
  FMax       := 0;
  FPosition  := 0;
  FOnChange  := Nil;
  FDragging  := False;
  FDragX     := 0;
  FDragY     := 0;
  FStep      := 1;
  FDragWidth := 2;
  FAssociate := Nil;
End; // TAbstractTrackBar.Create

Procedure TAbstractTrackBar.SetMin(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    If (FMin <> I) And (I <= FMax) Then
    Begin
      FMin := I;
      QueueRepaint;
      If Assigned(FOnChange) Then FOnChange(Self);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractTrackBar.SetMin

Procedure TAbstractTrackBar.SetMax(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    If (FMax <> I) And (I >= FMin) Then
    Begin
      FMax := I;
      QueueRepaint;
      If Assigned(FOnChange) Then FOnChange(Self);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractTrackBar.SetMax

Procedure TAbstractTrackBar.SetStep(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    If FStep <> I Then
    Begin
      FStep := I;
      QueueRepaint;
      If Assigned(FOnChange) Then FOnChange(Self);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractTrackBar.SetStep

Procedure TAbstractTrackBar.SetPosition(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    I := Round(I / Step) * Step;
    If (FPosition <> I) And (I >= FMin) And (I <= FMax) Then
    Begin
      FPosition := I;
      If FAssociate <> Nil Then FAssociate.Text := IntToStr(Position);
      QueueRepaint;
      If Assigned(FOnChange) Then FOnChange(Self);
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractTrackBar.SetPosition

Procedure TAbstractTrackBar.SetAssociate(Edit: TAbstractEdit);
Begin
  Try
    FUI.LockWindowList;
    If (FAssociate <> Edit) Then
    Begin
      If FAssociate <> Nil Then FAssociate.OnChange := Nil;
      FAssociate := Edit;
      If FAssociate <> Nil Then
      Begin
        FAssociate.Text     := IntToStr(Position);
        FAssociate.OnChange := AssociateChange;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractTrackBar.SetAssociate

Procedure TAbstractTrackBar.AssociateChange(Sender: TObject);
Var I,J: Integer;
Begin
  Val(Associate.Text,I,J);
  If (J = 0) And (I >= FMin) And (I <= FMax) Then Position := I;
End; // TAbstractTrackBar.AssociateChange

Procedure TAbstractTrackBar.MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  J     : Integer;
  XX,YY : Integer;
  J1,J2 : Integer;
  I1,I2 : Integer;

Begin
  Try
    FUI.LockWindowList;
    Inherited MouseDownAction(Button,Shift,X,Y);
    FDragging := False;
    If FEnabled Then
    Begin
      CalcAbsoluteXY(XX,YY);
      Dec(X,XX);
      Dec(Y,YY);
      J1 := 1;
      J2 := FWidth;

      J  := FWidth - 2;
      I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + 1)));
      I2 := FPosition;
      If I2 > FMax Then I2 := FMax;
      I2 := Round(J * ((FMax - I2) / (FMax - FMin + 1)));

      Dec(I1,FDragWidth Div 2);
      Dec(I2,FDragWidth Div 2);

      If (X >= J1 + I1) And (X <= J2 - I2) Then
      Begin
        FDragX    := X;
        FDragY    := Position;
        FDragging := True;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractTrackBar.MouseDownAction

Procedure TAbstractTrackBar.MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  J     : Integer;
  XX,YY : Integer;
  J1,J2 : Integer;
  I1,I2 : Integer;
  D,U   : Boolean;

Begin
  Try
    FUI.LockWindowList;
    If FEnabled And Not FDragging Then
    Begin
      CalcAbsoluteXY(XX,YY);
      Dec(X,XX);
      Dec(Y,YY);
      J1 := 1;
      J2 := FWidth;

      J  := FWidth - 2;
      I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + 1)));
      I2 := FPosition;
      If I2 > FMax Then I2 := FMax;
      I2 := Round(J * ((FMax - I2) / (FMax - FMin + 1)));

      D := (X > J1)      And (X < J1 + I1);
      U := (X > J2 - I2) And (X < J2);
           If D Then Position := Math.Max(Position - 1,FMin)
      Else If U Then Position := Math.Min(Position + 1,FMax);
    End;
    FDragging := False;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractTrackBar.MouseUpAction

Procedure TAbstractTrackBar.MouseMoveAction(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor);
Var
  I,J   : Integer;
  XX,YY : Integer;
  J1,J2 : Integer;
  I1,I2 : Integer;

Begin
  Try
    FUI.LockWindowList;
    Inherited MouseMoveAction(Shift,X,Y,Cursor);
    If FEnabled And FDragging Then
    Begin
      CalcAbsoluteXY(XX,YY);
      Dec(X,XX);
      Dec(Y,YY);
      J1 := 1;
      J2 := FWidth;

      J  := FWidth - 2;
      I1 := Round(J * ((FPosition - FMin) / (FMax - FMin + 1)));
      I2 := FPosition;
      If I2 > FMax Then I2 := FMax;
      I2 := Round(J * ((FMax - I2) / (FMax - FMin + 1)));

      Dec(I1,FDragWidth Div 2);
      Dec(I2,FDragWidth Div 2);

      If I1 + I2 > 0 Then
      Begin
        I := Round(FDragY + (FMax - FMin) * (X - FDragX) / (I1 + I2));
        I := Math.Max(I,FMin);
        I := Math.Min(I,FMax);
        Position := I;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractTrackBar.MouseMoveAction

// -------------------------------
// TAbstractComboBox
// -------------------------------

Constructor TAbstractComboBox.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth: Integer);
Begin
  Inherited Create(AParent,ctComboBox,ALeft,ATop,AWidth,16);
  FHeight := TextHeight('Mg') + 6; // Have to set this after calling the constructor because there has to be a connection to the parent
  CreateChildren;
  FButton.OnClick   := ButtonClick;
  FList.OnClick     := ListClick;
  FList.OnLostFocus := ListLostFocus;
  FEdit.OnMouseUp   := EditMouseUp;
End; // TAbstractComboBox.Create

Destructor TAbstractComboBox.Destroy;
Begin
  Inherited;
End; // TAbstractComboBox.Destroy

Procedure TAbstractComboBox.Add(St: String);
Begin
  FList.Add(St);
  If FList.ItemCount = 1 Then
  Begin
    FEdit.Text := St;
    FList.ItemIndex := 0;
  End;
End; // TAbstractComboBox.Add

Function TAbstractComboBox.GetItemCount: Integer;
Begin
  Result := FList.ItemCount;
End; // TAbstractComboBox.GetItemCount

Function TAbstractComboBox.GetItem(Index: Integer): String;
Begin
  Result := FList.Items[Index];
End; // TAbstractComboBox.GetItem

Function TAbstractComboBox.GetItemIndex: Integer;
Begin
  Result := FList.ItemIndex;
End; // TAbstractComboBox.GetItemIndex

Procedure TAbstractComboBox.CreateChildren;
Begin
  FEdit                     := TAbstractEdit.Create(Self,0,0,FWidth - ScrollBarThickness,FHeight,'',-1);
  FEdit.ReadOnly            := True;
  FButton                   := TAbstractButton.Create(Self,FWidth - ScrollBarThickness,0,ScrollBarThickness,FHeight,cbkDownArrow,cbsButton,'');
  FListPanel                := TAbstractPanel.Create(FParent,FLeft + 8,FTop + FHeight - 2,FWidth + 8,TextHeight('Mg') * 8 + 4,'');
  FListPanel.FillBackground := True;
  FListPanel.ShowBorder     := True;
  FListPanel.Visible        := False;
  FList                     := TAbstractListBox.Create(FListPanel,0,0,FListPanel.Width,FListPanel.Height,False);
End; // TAbstractComboBox.CreateChildren

Procedure TAbstractComboBox.SetItemIndex(Index: Integer);
Begin
  FList.ItemIndex := Index;
  If FList.ItemIndex >= 0
   Then FEdit.Text := FList.Items[FList.ItemIndex]
   Else FEdit.Text := '';
End; // TAbstractComboBox.SetItemIndex

Procedure TAbstractComboBox.ButtonClick(Sender: TObject);
Begin
  FListPanel.Visible := Not FListPanel.Visible;
  If FListPanel.Visible Then
  Begin
    FLastListIndex := FList.ItemIndex;
    FList.Focused  := True;
  End;
End; // TAbstractComboBox.ButtonClick

Procedure TAbstractComboBox.EditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  ButtonClick(FButton);
End; // TAbstractComboBox.EditMouseUp

Procedure TAbstractComboBox.ListClick(Sender: TObject);
Begin
  If (FList.ItemIndex < 0) And (FList.ItemCount > 0) Then
  Begin
    If FLastListIndex < 0 Then FLastListIndex := 0;
    FList.ItemIndex := FLastListIndex;
  End;
  FListPanel.Visible := False;
  If FList.ItemIndex >= 0
   Then FEdit.Text := FList.Items[FList.ItemIndex]
   Else FEdit.Text := '';
End; // TAbstractComboBox.ListClick

Procedure TAbstractComboBox.ListLostFocus(Sender: TObject);
Begin
  If (FUI.FocusedComponent <> FButton) And (FUI.FocusedComponent <> FEdit) Then ListClick(FList);
End; // TAbstractComboBox.ListLostFocus

// -------------------------------
// TAbstractListBox
// -------------------------------

Constructor TAbstractListBox.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AWordWrapped: Boolean);
Begin
  Inherited Create(AParent,ctListBox,ALeft,ATop,AWidth,AHeight);
  FWordWrapped         := AWordWrapped;
  FItems               := TStringList.Create;
  FTextHeight          := TextHeight('Mg');
  CreateChildren;
  FScrollBar.Align     := [alTop,alBottom,alRight];
  FScrollBar.OnChange  := ScrollBarChange;
  FShowBorder          := False;
  FItemIndex           := -1;
  FSelectionForeground := TColor(DefaultSelectionForeground);
  FSelectionBackground := TColor(DefaultSelectionBackground);
  FWordWrappedText     := TStringList.Create;
  FSetupCharWidths     := False;
  FOnClick             := Nil;
  TopIndex             := 0;
End; // TAbstractListBox.Create

Destructor TAbstractListBox.Destroy;
Begin
  FItems.Free;
  FWordWrappedText.Free;
  Inherited;
End; // TAbstractListBox.Destroy

Procedure TAbstractListBox.CreateChildren;
Begin
  FScrollBar := TAbstractScrollBar.Create(Self,FWidth - ScrollBarThickness,0,ScrollBarThickness,FHeight,sbVertical);
End; // TAbstractListBox.CreateChildren

Procedure TAbstractListBox.GetWordWrappedText(St: String);
// Clears FWordWrappedText and puts the word-wrapped text in there
Type
  TTokenArray      = Array Of String;
  TTokenWidthArray = Array Of Integer;

Var
  Tokens     : TTokenArray;
  Widths     : TTokenWidthArray;
  TotalWidth : Integer;
  TH         : Integer;
  I          : Integer;
  Threshold  : Integer;

  Function GetToken(Var St: String; Var TokenWidth: Integer): String;
  Var
    I,J   : Integer;
    Found : Boolean;
    St1   : String;

  Begin
    St1 := '';
    J   := 0;
    If St <> '' Then
    Begin
      I := 1;
      Found := False;
      While (I <= Length(St)) And Not Found Do
      Begin
        Case St[I] Of
          LastColorCommand:
          Begin
            // Set-to-last-color command: automatically include it and the next three characters

            St1 := St1 + LastColorCommand;
            Inc(I);
          End;
          SetColorCommand:
          Begin
            // Set-color command: automatically include it and the next three characters

            St1 := St1 + Copy(St,I,4);
            Inc(I,4);
          End;
          ResetColorCommand:
          Begin
            // Reset-color-to-default-command: automatically include it

            St1 := St1 + ResetColorCommand;
            Inc(I);
          End;
          #32:
          Begin
            // Space delimiter

            St    := Trim(Copy(St,I + 1,Length(St)));
            Found := True;
          End;
        Else
          // Any other character

          St1 := St1 + St[I];
          Inc(J,FCharWidths[Ord(St[I])]);
          Inc(I);
        End; // Case
      End; // While
      If Not Found Then St := '';
    End;
    TokenWidth := J;
    Result     := St1;
  End; // GetToken

  Procedure GetTokens(Var St: String; Var TokenArray: TTokenArray; Var TokenWidthArray: TTokenWidthArray);
  Var
    Token      : String;
    TokenWidth : Integer;

  Begin
    SetLength(TokenArray,0);
    SetLength(TokenWidthArray,0);
    St := Trim(St);
    If St <> '' Then
    Begin
      Repeat
        Token := GetToken(St,TokenWidth);
        SetLength(TokenArray,High(TokenArray) + 2);
        SetLength(TokenWidthArray,High(TokenWidthArray) + 2);
        TokenArray[High(TokenArray)] := Token;
        TokenWidthArray[High(TokenWidthArray)] := TokenWidth;
      Until St = '';
    End;
  End; // GetTokens

Begin
  // Set up character widths

  If Not FSetupCharWidths Then
  Begin
    For I := 0 To 255 Do FCharWidths[I] := TextWidth(Chr(I));
    FSetupCharWidths := True;
  End;

  Threshold := Width - 2 - FScrollBar.Width;
  St        := Trim(St);
  FWordWrappedText.Clear;
  If St <> '' Then
  Begin
    GetTokens(St,Tokens,Widths);
    St         := Tokens[0];
    I          := 1;
    TotalWidth := Widths[0];
    While I <= High(Tokens) Do
    Begin
      If TotalWidth + FCharWidths[32] + Widths[I] <= Threshold Then
      Begin
        St := St + ' ' + Tokens[I];
        Inc(TotalWidth,FCharWidths[32] + Widths[I]);
        Inc(I);
      End
      Else
      Begin
        FWordWrappedText.Add(St);
        St         := Tokens[I];
        TotalWidth := Widths[I];
        Inc(I);
      End;
    End; // While
    If St <> '' Then FWordWrappedText.Add(St);
    SetLength(Tokens,0);
    SetLength(Widths,0);
  End;
End; // TAbstractListBox.GetWordWrappedText

Procedure TAbstractListBox.ScrollBarChange(Sender: TObject);
Begin
  Try
    FUI.LockWindowList;
    If Not FChanging Then
    Begin
      Changing;
      TopIndex := FScrollBar.Position;
      Changed;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.ScrollBarChange

Procedure TAbstractListBox.Add(St: String);
Var I: Integer;
Begin
  Try
    FUI.LockWindowList;
    If FWordWrapped Then
    Begin
      GetWordWrappedText(St);
      If FWordWrappedText.Count > 0 Then
      Begin
        If FItems.Count = 0 Then
        Begin
          FItems.AddObject(St,Pointer(FItems.Count + (FWordWrappedText.Count Shl 16)));
        End
        Else
        Begin
          I := Integer(FItems.Objects[FItems.Count - 1]);
          FItems.AddObject(St,Pointer(((I And $FFFF) + (I Shr 16)) + (FWordWrappedText.Count Shl 16)));
        End;
      End;
    End
    Else FItems.AddObject(St,Pointer(FItems.Count + (1 Shl 16)));
    AfterSetSize;
    QueueRepaint;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.Add

Procedure TAbstractListBox.Delete(Index: Integer);
Var I,J,K: Integer;
Begin
  Try
    FUI.LockWindowList;
    If (Index >= 0) And (Index < FItems.Count) Then
    Begin
      J := Integer(FItems.Objects[Index]);
      FItems.Delete(Index);
      For I := Index To FItems.Count - 1 Do
      Begin
        K := Integer(FItems.Objects[I]);
        K := (K And $FFFF) - (J Shr 16) Or Integer(K And $FFFF0000);
        FItems.Objects[I] := Pointer(K);
      End; // For I
      AfterSetSize;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.Delete

Procedure TAbstractListBox.Clear;
Begin
  Try
    FUI.LockWindowList;
    If FItems.Count > 0 Then
    Begin
      FItems.Clear;
      FItemIndex := -1;
      FTopIndex  := 0;
      AfterSetSize;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.Clear

Function TAbstractListBox.GetDisplayableItems: Integer;
Var
  AvailPix  : Integer;
  AvailRows : Integer;
  I,J       : Integer;
  NumLines  : Integer;
  ItemCount : Integer;
  Done      : Boolean;

Begin
  If FItems.Count = 0 Then Result := 0
  Else
  Begin
    AvailPix  := GetAvailableVertPixels;
    AvailRows := AvailPix Div TextHeight('Mg');
    If AvailRows * FTextHeight < AvailPix Then Inc(AvailRows);

//    DisplayableLines := (FHeight - 4) Div TextHeight('Mg');
    I                := FTopIndex;
    NumLines         := 0;
    ItemCount        := 0;
    Done             := False;
    While (I < FItems.Count) And Not Done Do
    Begin
      J := Integer(FItems.Objects[I]);
      If NumLines < AvailRows Then
      Begin
        Inc(ItemCount);
        Inc(NumLines,J Shr 16);
      End
      Else Done := True;
      Inc(I);
    End; // While
    Result := ItemCount;
  End;
End; // TAbstractListBox.GetDisplayableItems

Function TAbstractListBox.GetAvailableVertPixels: Integer;
Begin
  Result := Max(0,FHeight - 4);
End; // TAbstractListBox.GetAvailableVertPixels

Function TAbstractListBox.GetMaxTopIndex: Integer;
Var
  AvailPix  : Integer;
  AvailRows : Integer;
  I,J,K     : Integer;
  Done      : Boolean;

Begin
//  Result := FItems.Count - ((FHeight - 4) Div FTextHeight);

  // First find out how many rows we can display

  AvailPix := GetAvailableVertPixels;
  AvailRows := AvailPix Div TextHeight('Mg');
//  If AvailRows * FTextHeight < AvailPix Then Inc(AvailRows);

  // Now, starting with the last entry, count up the number of required rows until
  // exceeding the available amount or running out of entries

  I    := FItems.Count - 1;
  Done := False;
  K    := 0;
  While (I >= 0) And Not Done Do
  Begin
    J := Integer(FItems.Objects[I]);
    If K + (J Shr 16) > AvailRows Then Done := True
    Else
    Begin
      Inc(K,J Shr 16);
      Dec(I);
    End;
  End; // While
  Result := Max(0,I);
End; // TAbstractListBox.GetMaxTopIndex

Procedure TAbstractListBox.OutputText(X,Y: Integer; St: String; DefaultColor: TColor);
Begin
End; // TAbstractListBox.OutputText

Function TAbstractListBox.GetItem(Index: Integer): String;
Begin
  Try
    FUI.LockWindowList;
    If (Index >= 0) And (Index < FItems.Count)
     Then Result := FItems.Strings[Index]
     Else Result := '';
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.GetItem

Procedure TAbstractListBox.SetItem(Index: Integer; St: String);
Var I,J,K: Integer;
Begin
  Try
    FUI.LockWindowList;
    If (Index >= 0) And (Index < FItems.Count) Then
    Begin
      FItems.Strings[Index] := St;
      If FWordWrapped Then
      Begin
        GetWordWrappedText(St);
        If FItems.Count > 1 Then
        Begin
          J := Integer(FItems.Objects[Index - 1]);
          J := (J And $FFFF) + (J Shr 16) + (FWordWrappedText.Count Shl 16);
          FItems.Objects[Index] := Pointer(J);
        End
        Else
        Begin
          J := FWordWrappedText.Count Shl 16;
          FItems.Objects[0] := Pointer(J);
        End;
        For I := Index + 1 To FItems.Count - 1 Do
        Begin
          K := Integer(FItems.Objects[I]);
          K := (J And $FFFF) + (J Shr 16) Or Integer(K And $FFFF0000);
          FItems.Objects[I] := Pointer(K);
          J := K;
        End; // For I
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.SetItem

Procedure TAbstractListBox.SetItemIndex(Index: Integer);
Begin
  Try
    FUI.LockWindowList;
    If (Index >= -1) And (Index < FItems.Count) Then
    Begin
      FItemIndex := Index;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.SetItemIndex

Procedure TAbstractListBox.SetTopIndex(Index: Integer);
Var MaxTopIndex: Integer;
Begin
  Try
    FUI.LockWindowList;
    MaxTopIndex := GetMaxTopIndex;
    If Index >= 0 Then
    Begin
      If Index > MaxTopIndex Then Index := MaxTopIndex;
      If (Index >= 0) And (Index <> FTopIndex) Then
      Begin
        FTopIndex := Index;
        FScrollBar.Position := FTopIndex;
        QueueRepaint;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.SetTopIndex

Function TAbstractListBox.GetItemCount: Integer;
Begin
  Try
    FUI.LockWindowList;
    Result := FItems.Count;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.GetItemCount

Function TAbstractListBox.GetItemHeight: Integer;
Begin
  Try
    FUI.LockWindowList;
    Result := FTextHeight;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.GetItemHeight

Procedure TAbstractListBox.AfterSetSize;
Var MaxTopIndex: Integer;
Begin
  MaxTopIndex := GetMaxTopIndex;
//  If MaxTopIndex < 0 Then MaxTopIndex := 0;
  FScrollBar.Max := MaxTopIndex;
  FScrollBar.PageSize := (FHeight - 4) Div FTextHeight;
End; // TAbstractListBox.AfterSetSize

Procedure TAbstractListBox.SetShowBorder(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If FShowBorder <> B Then
    Begin
      FShowBorder := B;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.SetShowBorder

Procedure TAbstractListBox.MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
          If Assigned(FOnClick) Then FOnClick(Self);
        End;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.MouseUpAction

Procedure TAbstractListBox.SetSelectionForeground(C: TColor);
Begin
  Try
    FUI.LockWindowList;
    If C <> FSelectionForeground Then
    Begin
      FSelectionForeground := C;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.SetSelectionForeground

Procedure TAbstractListBox.SetSelectionBackground(C: TColor);
Begin
  Try
    FUI.LockWindowList;
    If C <> FSelectionBackground Then
    Begin
      FSelectionBackground := C;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractListBox.SetSelectionBackground

// -------------------------------
// TAbstractLabel
// -------------------------------

Constructor TAbstractLabel.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; ATransparent: Boolean);
Begin
  Inherited Create(AParent,ctLabel,ALeft,ATop,AWidth,AHeight);
  FAutoSize        := False; // Must happen first
  FCaption         := ACaption;
  FWordWrapped     := False;
  FTransparent     := ATransparent;
  FWordWrappedText := TStringList.Create;
  FSetupCharWidths := False;
  FAlignment       := taLeftJustify;
  DoAutoSize;
End; // TAbstractLabel.Create

Constructor TAbstractLabel.Create(AParent: TAbstractComponent; ALeft,ATop: Integer; ACaption: String; ATransparent: Boolean);
Begin
  Inherited Create(AParent,ctLabel,ALeft,ATop,64,24);
  FCaption         := ACaption;
  FAutoSize        := True;
  FWordWrapped     := False;
  FTransparent     := ATransparent;
  FWordWrappedText := TStringList.Create;
  FSetupCharWidths := False;
  FAlignment       := taLeftJustify;
  DoAutoSize;
End; // TAbstractLabel.Create

Destructor TAbstractLabel.Destroy;
Begin
  FWordWrappedText.Free;
  Inherited;
End; // TAbstractLabel.Destroy

Procedure TAbstractLabel.DoAutoSize;
Begin
  If FAutoSize Then
  Begin
    If FWordWrapped Then
    Begin
      Height := FWordWrappedText.Count * TextHeight('Mg');
    End
    Else
    Begin
      Width  := TextWidth(FCaption);
      Height := TextHeight('Mg');
    End;
  End;
End; // TAbstractLabel.DoAutoSize

Procedure TAbstractLabel.SetCaption(St: String);
Begin
  Try
    FUI.LockWindowList;
    If FCaption <> St Then
    Begin
      FCaption := St;
      If FWordWrapped Then GetWordWrappedText(FCaption);
      If FAutoSize Then DoAutoSize;
      Self.RepaintParent;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractLabel.SetCaption

Procedure TAbstractLabel.SetAutoSize(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If FAutoSize <> B Then
    Begin
      FAutoSize := B;
      If FWordWrapped Then GetWordWrappedText(FCaption);
      If FAutoSize Then DoAutoSize;
      Self.RepaintParent;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractLabel.SetAutoSize

Procedure TAbstractLabel.SetAlignment(A: TAlignment);
Begin
  Try
    FUI.LockWindowList;
    If FAlignment <> A Then
    Begin
      FAlignment := A;
      Self.RepaintParent;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractLabel.SetAlignment

Procedure TAbstractLabel.SetWordWrapped(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If FWordWrapped <> B Then
    Begin
      FWordWrapped := B;
      If FWordWrapped Then
      Begin
        FAutoSize := True;
        GetWordWrappedText(FCaption);
      End;
      If FAutoSize Then DoAutoSize;
      Self.RepaintParent;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractLabel.SetWordWrapped

Procedure TAbstractLabel.GetWordWrappedText(St: String);
// Clears FWordWrappedText and puts the word-wrapped text in there
Type
  TTokenArray      = Array Of String;
  TTokenWidthArray = Array Of Integer;

Var
  Tokens     : TTokenArray;
  Widths     : TTokenWidthArray;
  TotalWidth : Integer;
  TH         : Integer;
  I          : Integer;
  Threshold  : Integer;

  Function GetToken(Var St: String; Var TokenWidth: Integer): String;
  Var
    I,J   : Integer;
    Found : Boolean;
    St1   : String;

  Begin
    St1 := '';
    J   := 0;
    If St <> '' Then
    Begin
      I := 1;
      Found := False;
      While (I <= Length(St)) And Not Found Do
      Begin
        Case St[I] Of
          LastColorCommand:
          Begin
            // Set-to-last-color command: automatically include it and the next three characters

            St1 := St1 + LastColorCommand;
            Inc(I);
          End;
          SetColorCommand:
          Begin
            // Set-color command: automatically include it and the next three characters

            St1 := St1 + Copy(St,I,4);
            Inc(I,4);
          End;
          ResetColorCommand:
          Begin
            // Reset-color-to-default-command: automatically include it

            St1 := St1 + ResetColorCommand;
            Inc(I);
          End;
          #32:
          Begin
            // Space delimiter

            St    := Trim(Copy(St,I + 1,Length(St)));
            Found := True;
          End;
        Else
          // Any other character

          St1 := St1 + St[I];
          Inc(J,FCharWidths[Ord(St[I])]);
          Inc(I);
        End; // Case
      End; // While
      If Not Found Then St := '';
    End;
    TokenWidth := J;
    Result     := St1;
  End; // GetToken

  Procedure GetTokens(Var St: String; Var TokenArray: TTokenArray; Var TokenWidthArray: TTokenWidthArray);
  Var
    Token      : String;
    TokenWidth : Integer;

  Begin
    SetLength(TokenArray,0);
    SetLength(TokenWidthArray,0);
    St := Trim(St);
    If St <> '' Then
    Begin
      Repeat
        Token := GetToken(St,TokenWidth);
        SetLength(TokenArray,High(TokenArray) + 2);
        SetLength(TokenWidthArray,High(TokenWidthArray) + 2);
        TokenArray[High(TokenArray)] := Token;
        TokenWidthArray[High(TokenWidthArray)] := TokenWidth;
      Until St = '';
    End;
  End; // GetTokens

Begin
  // Set up character widths

  If Not FSetupCharWidths Then
  Begin
    For I := 0 To 255 Do FCharWidths[I] := TextWidth(Chr(I));
    FSetupCharWidths := True;
  End;

  Threshold := Width;
  St        := Trim(St);
  FWordWrappedText.Clear;
  If St <> '' Then
  Begin
    GetTokens(St,Tokens,Widths);
    St         := Tokens[0];
    I          := 1;
    TotalWidth := Widths[0];
    While I <= High(Tokens) Do
    Begin
      If TotalWidth + FCharWidths[32] + Widths[I] <= Threshold Then
      Begin
        St := St + ' ' + Tokens[I];
        Inc(TotalWidth,FCharWidths[32] + Widths[I]);
        Inc(I);
      End
      Else
      Begin
        FWordWrappedText.Add(St);
        St         := Tokens[I];
        TotalWidth := Widths[I];
        Inc(I);
      End;
    End; // While
    If St <> '' Then FWordWrappedText.Add(St);
    SetLength(Tokens,0);
    SetLength(Widths,0);
  End;
End; // TAbstractLabel.GetWordWrappedText

// -------------------------------
// TAbstractPanel
// -------------------------------

Constructor TAbstractPanel.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; ACaption: String);
Begin
  Inherited Create(AParent,ctPanel,ALeft,ATop,AWidth,AHeight);
  CreateChildren;
  FLabel.AcceptEvents := [];
  FShowBorder         := True;
  FFillBackground     := False;
  FLabel.Caption := ACaption;
  AdjustLabelPosition;
End; // TAbstractPanel.Create

Procedure TAbstractPanel.CreateChildren;
Begin
  FLabel := TAbstractLabel.Create(Self,0,0,'',True);
End; // TAbstractPanel.CreateChildren

Function TAbstractPanel.GetCaption: String;
Begin
  Result := FLabel.Caption;
End; // TAbstractPanel.GetCaption

Procedure TAbstractPanel.SetCaption(St: String);
Begin
  Try
    FUI.LockWindowList;
    If FLabel.Caption <> St Then
    Begin
      FLabel.Caption := St;
      AdjustLabelPosition;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractPanel.SetCaption

Procedure TAbstractPanel.SetFillBackground(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If FFillBackground <> B Then
    Begin
      FFillBackground := B;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractPanel.SetFillBackground

Procedure TAbstractPanel.SetWordWrapped(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If FLabel.WordWrapped <> B Then
    Begin
      FLabel.WordWrapped := B;
      AdjustLabelPosition;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractPanel.SetWordWrapped

Function TAbstractPanel.GetWordWrapped: Boolean;
Begin
  Result := FLabel.WordWrapped;
End; // TAbstractPanel.GetWordWrapped

Procedure TAbstractPanel.AdjustLabelPosition;
Begin
  FLabel.Left := (FWidth  - FLabel.Width) Div 2;
  FLabel.Top  := (FHeight - FLabel.Height) Div 2;
End; // TAbstractPanel.AdjustLabelPosition

Procedure TAbstractPanel.SetShowBorder(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If FShowBorder <> B Then
    Begin
      FShowBorder := B;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractPanel.SetShowBorder

// -------------------------------
// TAbstractPageControl
// -------------------------------

Constructor TAbstractPageControl.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
Begin
  Inherited Create(AParent,ctPageControl,ALeft,ATop,AWidth,AHeight);
  FPages           := TStringList.Create;
  FTabInfo         := TList.Create;
  FActivePage      := Nil;
  FActivePageIndex := -1;
  FTopTabRow       := -1;
  FRows            := 0;
End; // TAbstractPageControl.Create

Destructor TAbstractPageControl.Destroy;
Var I: Integer;
Begin
  FPages.Free;
  For I := 0 To FTabInfo.Count - 1 Do FreeMem(FTabInfo.Items[I]);
  FTabInfo.Free;
  Inherited;
End; // TAbstractPageControl.Destroy

Procedure TAbstractPageControl.CalculateTabInfo;
Var
  I,J        : Integer;
  W          : Integer;
  TabInfo    : PTabInfo;
  TabInfo1   : PTabInfo;
  WidthAccum : Integer;
  TabsOnRow  : Integer;
  Extra      : Integer;
  XAdd       : Integer;
  XInc       : Integer;
  OldRows    : Integer;

Begin
  Try
    FUI.LockWindowList;
    OldRows := FRows;
    FRows := 0;
    If FPages.Count > 0 Then
    Begin
      FRows      := 1;
      WidthAccum := 0;
      TabsOnRow  := 0;

      For I := 0 To FPages.Count - 1 Do
      Begin
        TabInfo := PTabInfo(FTabInfo.Items[I]);
        W       := GetTabWidth(I);
        If W + WidthAccum > FWidth Then
        Begin
          // If we need more than one row, justify all tabs on the current row

          J     := I - TabsOnRow;
          Extra := FWidth - WidthAccum;
          XAdd  := 0;
          If TabsOnRow > 1 Then XInc := Extra Div (TabsOnRow - 1);
          While TabsOnRow > 0 Do
          Begin
            TabInfo1 := PTabInfo(FTabInfo.Items[J]);
            If TabsOnRow > 1 Then
            Begin
              Inc(TabInfo1.X,XAdd);
              Inc(TabInfo1.Width,XInc);
              Inc(XAdd,XInc);
              Dec(Extra,XInc);
            End
            Else
            Begin
              Inc(TabInfo1.X,XAdd);
              Inc(TabInfo1.Width,Extra);
            End;
            Dec(TabsOnRow);
            Inc(J);
          End; // While

          // Add the new tab to another row

          Inc(FRows);
          WidthAccum := 0;
        End;
        TabInfo.X     := WidthAccum;
        TabInfo.Width := Min(W,FWidth);
        TabInfo.Row   := FRows - 1;
        Inc(TabsOnRow);
        Inc(WidthAccum,TabInfo.Width);
      End; // For I
    End;
    If OldRows <> FRows Then ResizePages;
    QueueRepaint;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractPageControl.CalculateTabInfo

Procedure TAbstractPageControl.MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  XX      : Integer;
  YY      : Integer;
  I       : Integer;
  Found   : Boolean;
  TabInfo : PTabInfo;
  RowY    : Integer;
  RowH    : Integer;
  
Begin
  Try
    FUI.LockWindowList;
    If FEnabled Then
    Begin
      CalcAbsoluteXY(XX,YY);
      Dec(X,XX);
      Dec(Y,YY);
      I     := 0;
      Found := False;
      RowH  := GetTabHeight;
      While (I < FTabInfo.Count) And Not Found Do
      Begin
        TabInfo := PTabInfo(FTabInfo.Items[I]);
        RowY    := (FRows - 1 - ((TabInfo.Row + (FRows - 1 - FTopTabRow)) Mod FRows)) * GetTabHeight;
        If (X >= TabInfo.X) And (X < TabInfo.X + TabInfo.Width) And (Y >= RowY) And (Y < RowY + GetTabHeight) Then
        Begin
          Found           := True;
          ActivePageIndex := I;
        End
        Else Inc(I);
      End; // While
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractPageControl.MouseDownAction

Procedure TAbstractPageControl.ResizePages;
// Assumes that FRows has been calculated already
Var
  I    : Integer;
  Page : TAbstractPanel;

Begin
  Try
    FUI.LockWindowList;
    For I := 0 To FPages.Count - 1 Do
    Begin
      Page        := TAbstractPanel(FPages.Objects[I]);
      Page.Left   := 0;
      Page.Top    := FRows * GetTabHeight;
      Page.Width  := FWidth;
      Page.Height := FHeight - Page.Top;
    End; // For I
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractPageControl.ResizePages

Procedure TAbstractPageControl.AfterSetSize;
Begin
  Try
    FUI.LockWindowList;
    CalculateTabInfo;
    ResizePages;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractPageControl.AfterSetSize

Function TAbstractPageControl.AddPage(PageCaption: String): TAbstractPanel;
Var
  NewPage : TAbstractPanel;
  TabInfo : PTabInfo;

Begin
  Try
    FUI.LockWindowList;
    NewPage := CreatePagePanel(PageCaption);
    FPages.AddObject(PageCaption,NewPage);
    GetMem(TabInfo,SizeOf(TTabInfo));
    FTabInfo.Add(TabInfo);
    If FPages.Count = 1 Then
    Begin
      FActivePage      := NewPage;
      FActivePageIndex := 0;
    End;
    NewPage.Visible := (FActivePageIndex = FPages.Count - 1);
    CalculateTabInfo;
    ResizePages;
    Result := NewPage;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractPageControl.AddPage

Procedure TAbstractPageControl.SetActivePage(Page: TAbstractPanel);
Var
  I,J     : Integer;
  TabInfo : PTabInfo;

Begin
  Try
    FUI.LockWindowList;
    If Page <> Nil Then
    Begin
      J := FPages.IndexOfObject(Page);
      If (J >= 0) And (Page <> FActivePage) Then
      Begin
        For I := 0 To FPages.Count - 1 Do TAbstractPanel(FPages.Objects[I]).Visible := (J = I);
        FActivePage      := Page;
        FActivePageIndex := J;
        TabInfo          := PTabInfo(FTabInfo.Items[J]);
        FTopTabRow       := (TabInfo.Row + FRows - 1) Mod FRows;
        QueueRepaint;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractPageControl.SetActivePage

Procedure TAbstractPageControl.SetActivePageIndex(Page: Integer);
Var
  I       : Integer;
  TabInfo : PTabInfo;

Begin
  Try
    FUI.LockWindowList;
    If (Page >= 0) And (Page < FPages.Count) Then
    Begin
      For I := 0 To FPages.Count - 1 Do
      Begin
        TAbstractComponent(FPages.Objects[I]).Visible := (Page = I);
        If Page = I Then FActivePage := TAbstractPanel(FPages.Objects[I]);
      End; // For I
      FActivePageIndex := Page;
      TabInfo          := PTabInfo(FTabInfo.Items[Page]);
      FTopTabRow       := (TabInfo.Row + FRows - 1) Mod FRows;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractPageControl.SetActivePageIndex

// -------------------------------
// TAbstractProgressBar
// -------------------------------

Constructor TAbstractProgressBar.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer; AValue: Single; AColor: TColor);
Begin
  Inherited Create(AParent,ctProgressBar,ALeft,ATop,AWidth,AHeight);
  FValue := AValue;
  FColor := AColor;
End; // TAbstractProgressBar.Create

Procedure TAbstractProgressBar.SetColor(AColor: TColor);
Begin
  Try
    FUI.LockWindowList;
    If FColor <> AColor Then
    Begin
      FColor := AColor;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractProgressBar.SetColor

Procedure TAbstractProgressBar.SetValue(AValue: Single);
Begin
  Try
    FUI.LockWindowList;
    If FValue <> AValue Then
    Begin
      FValue := AValue;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractProgressBar.SetValue

// -------------------------------
// TAbstractScrollPane
// -------------------------------

Constructor TAbstractScrollPane.Create(AParent: TAbstractComponent; ALeft,ATop,AWidth,AHeight: Integer);
Begin
  Inherited Create(AParent,ctScrollPane,ALeft,ATop,AWidth,AHeight);
  CreateChildren;
End; // TAbstractScrollPane.Create

Procedure TAbstractScrollPane.CreateChildren;
Begin
  FVertScrollBar          := Nil;
  FHorzScrollBar          := Nil;
  FVertScrollBar          := TAbstractScrollBar.Create(Self,0,0,ScrollBarThickness,50,sbVertical);
  FHorzScrollBar          := TAbstractScrollBar.Create(Self,0,0,50,ScrollBarThickness,sbHorizontal);
  FVertScrollBar.Min      := 0;
  FHorzScrollBar.Min      := 0;
  FVertScrollBar.OnChange := VertScrollBarChange;
  FHorzScrollBar.OnChange := HorzScrollBarChange;
  AfterSetSize;
End; // TAbstractScrollPane.CreateChildren

Procedure TAbstractScrollPane.AddChild(Component: TAbstractComponent);
Begin
  SetChild(Component);
  AfterSetSize;
End; // TAbstractScrollPane.AddChild

Function TAbstractScrollPane.GetChild: TAbstractComponent;
Begin
  If FChildren.Count >= 3
   Then Result := TAbstractComponent(FChildren.Objects[2])
   Else Result := Nil;
End; // TAbstractScrollPane.GetChild

Procedure TAbstractScrollPane.SetChild(Component: TAbstractComponent);
Begin
  If FChildren.Count >= 3
   Then FChildren.Objects[2] := Component // The application must remember to free the old child
   Else FChildren.AddObject('',Component);
  AfterSetSize;
End; // TAbstractScrollPane.SetChild

Function TAbstractScrollPane.GetHorzSmallInc: Integer;
Begin
  Result := FHorzScrollBar.SmallInc;
End; // TAbstractScrollPane.GetHorzSmallInc

Procedure TAbstractScrollPane.SetHorzSmallInc(I: Integer);
Begin
  HorzScrollBar.SmallInc := I;
End; // TAbstractScrollPane.SetHorzSmallInc

Function TAbstractScrollPane.GetVertSmallInc: Integer;
Begin
  Result := FVertScrollBar.SmallInc;
End; // TAbstractScrollPane.GetVertSmallInc

Procedure TAbstractScrollPane.SetVertSmallInc(I: Integer);
Begin
  VertScrollBar.SmallInc := I;
End; // TAbstractScrollPane.SetVertSmallInc

Procedure TAbstractScrollPane.AfterSetSize;
Var ChildComponent: TAbstractComponent;
Begin
  ChildComponent := Child;
  If FVertScrollBar <> Nil Then FVertScrollBar.Visible := (ChildComponent <> Nil) And (ClientHeight < ChildComponent.Height);
  If FHorzScrollBar <> Nil Then FHorzScrollBar.Visible := (ChildComponent <> Nil) And (ClientWidth  < ChildComponent.Width);
  If (FVertScrollBar <> Nil) And FVertScrollBar.Visible Then
  Begin
    FVertScrollBar.Left := ClientWidth - FVertScrollBar.Width;
    If FHorzScrollBar.Visible
     Then FVertScrollBar.Height := ClientHeight - FHorzScrollBar.Height
     Else FVertScrollBar.Height := ClientHeight;
    FVertScrollBar.Max := ChildComponent.Height - FHeight;
    FVertScrollBar.PageSize := FHeight;
    ChildComponent.Top := -FVertScrollBar.Position;
  End
  Else If ChildComponent <> Nil Then ChildComponent.Top := 0;
  If (FHorzScrollBar <> Nil) And FHorzScrollBar.Visible Then
  Begin
    FHorzScrollBar.Top := ClientHeight - FHorzScrollBar.Height;
    If FVertScrollBar.Visible
     Then FHorzScrollBar.Width := ClientWidth - FVertScrollBar.Width
     Else FHorzScrollBar.Width := ClientWidth;
    FHorzScrollBar.Max := ChildComponent.Width - FWidth;
    FHorzScrollBar.PageSize := FWidth;
    ChildComponent.Left := -FHorzScrollBar.Position;
  End
  Else If ChildComponent <> Nil Then ChildComponent.Left := 0;
End; // TAbstractScrollPane.AfterSetSize

Procedure TAbstractScrollPane.VertScrollBarChange(Sender: TObject);
Var ChildComponent: TAbstractComponent;
Begin
  ChildComponent := Child;
  If ChildComponent <> Nil Then
  Begin
    If FVertScrollBar.Visible
     Then ChildComponent.Top := -FVertScrollBar.Position
     Else ChildComponent.Top := 0;
  End;
End; // TAbstractScrollPane.VertScrollBarChange

Procedure TAbstractScrollPane.HorzScrollBarChange(Sender: TObject);
Var ChildComponent: TAbstractComponent;
Begin
  ChildComponent := Child;
  If ChildComponent <> Nil Then
  Begin
    If FHorzScrollBar.Visible
     Then ChildComponent.Left := -FHorzScrollBar.Position
     Else ChildComponent.Left := 0;
  End;
End; // TAbstractScrollPane.HorzScrollBarChange

Function TAbstractScrollPane.GetChildClipRect(ChildComponent: TAbstractComponent): TRect;
Var
  X,Y   : Integer;
  MaxX  : Integer;
  MaxY  : Integer;
  Rect  : TRect;
  Rect1 : TRect;

Begin
  If ChildComponent = Child Then
  Begin
    CalcAbsoluteXY(X,Y);

    If FVertScrollBar.Visible
     Then MaxX := FVertScrollBar.Left
     Else MaxX := ClientWidth;

    If FHorzScrollBar.Visible
     Then MaxY := FHorzScrollBar.Top
     Else MaxY := ClientHeight;

    Rect.Left   := X + ClientLeft;
    Rect.Top    := Y + ClientTop;
    Rect.Right  := Rect.Left + MaxX - 1;
    Rect.Bottom := Rect.Top  + MaxY - 1;
    If FParent <> Nil Then
    Begin
      Rect1       := FParent.GetChildClipRect(Self);
      Rect.Left   := Max(Rect.Left,   Rect1.Left);
      Rect.Top    := Max(Rect.Top,    Rect1.Top);
      Rect.Right  := Min(Rect.Right,  Rect1.Right);
      Rect.Bottom := Min(Rect.Bottom, Rect1.Bottom);
    End;
  End
  Else Rect := Inherited GetChildClipRect(ChildComponent);
  Result := Rect;
End; // TAbstractScrollPane.GetChildClipRect

Procedure TAbstractScrollPane.Repaint;
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
    BeginClip(Rect);
    OldClipRect := FClipRect;
    FClipRect   := Rect;

    // Paint the background

    FillBackground;
    If FVisible Then
    Begin
      If Assigned(FOnBeforePaint) Then FOnBeforePaint(Self,X,Y);

      FVertScrollBar.Repaint;
      FHorzScrollBar.Repaint;

      If FVertScrollBar.Visible
       Then MaxX := FVertScrollBar.Left
       Else MaxX := FClientWidth;

      If FHorzScrollBar.Visible
       Then MaxY := FHorzScrollBar.Top
       Else MaxY := FClientHeight;

      Rect1.Left   := X;
      Rect1.Top    := Y;
      Rect1.Right  := X + MaxX - 1;
      Rect1.Bottom := Y + MaxY - 1;
      BeginClip(Rect1);
      ChildComponent := Child;
      If ChildComponent <> Nil Then ChildComponent.Repaint;
      EndClip;

      // See if the optional event handler was set

      If Assigned(FOnPaint) Then FOnPaint(Self,X,Y);
    End;
    FClipRect := OldClipRect;
    EndClip;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractScrollPane.Repaint

// -------------------------------
// TAbstractWindow
// -------------------------------

Constructor TAbstractWindow.Create(ALeft,ATop,AWidth,AHeight: Integer; ACaption: String; AUI: TAbstractUI);
Begin
  Inherited Create(Nil,ctWindow,ALeft,ATop,AWidth,AHeight);
  FOwner               := Self;
  FCaption             := ACaption;
  FShowCaption         := True;
  FShowBorder          := True;
  FUI                  := AUI;
  FZOrder              := 0;
  FDragging            := False;
  FResizing            := crDefault;
  FResizeUL            := False;
  FDragX               := 0;
  FDragY               := 0;
  FClipQueue           := TList.Create;
  CreateCloseButton;
  FCloseButton.Flat    := True;
  FCloseButton.OnClick := CloseButtonClick;
  FModalResult         := mrNone;
  FClosable            := True;
  FBorderWidth         := 1;
  FResizeable          := True;
End; // TAbstractWindow.Create

Destructor TAbstractWindow.Destroy;
Begin
  FClipQueue.Free;
  Inherited;
End; // TAbstractWindow.Destroy

Procedure TAbstractWindow.CreateCloseButton;
Begin
  FCloseButton := TAbstractButton.Create(Self,2,2,11,11,cbkX,cbsButton,'');
End; // TAbstractWindow.CreateCloseButton

Procedure TAbstractWindow.SetClosable(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    FClosable := B;
    FCloseButton.Visible := B;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractWindow.SetClosable

Procedure TAbstractWindow.SetResizeable(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    FResizeable := B;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractWindow.SetResizeable

Procedure TAbstractWindow.MouseDownAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var I: Integer;
Begin
  Inherited MouseDownAction(Button,Shift,X,Y);
  If FShowBorder
   Then I := FBorderWidth
   Else I := 0;
  If (Y >= I) And (Y < FClientTop) Then
  Begin
    FDragX    := X;
    FDragY    := Y;
    FDragging := True;
  End
  Else FDragging := False;
  If Not FDragging Then
  Begin
    If FShowBorder And FResizeable Then
    Begin
      If Y < FBorderWidth Then
      Begin
        If X < FBorderWidth + 8 Then
        Begin
          FResizing := crSizeNWSE;
          FResizeUL := True;
        End
        Else If X >= FWidth - FBorderWidth - 8 Then
        Begin
          FResizing := crSizeNESW;
          FResizeUL := False;
        End
        Else
        Begin
          FResizing := crSizeNS;
          FResizeUL := True;
        End;
      End
      Else If Y >= FHeight - FBorderWidth Then
      Begin
        If X < FBorderWidth + 8 Then
        Begin
          FResizing := crSizeNESW;
          FResizeUL := True;
        End
        Else If X >= FWidth - FBorderWidth - 8 Then
        Begin
          FResizing := crSizeNWSE;
          FResizeUL := False;
        End
        Else
        Begin
          FResizing := crSizeNS;
          FResizeUL := False;
        End;
      End
      Else If X < FBorderWidth Then
      Begin
        If Y < FBorderWidth + 8 Then
        Begin
          FResizing := crSizeNWSE;
          FResizeUL := True;
        End
        Else If Y >= FHeight - FBorderWidth - 8 Then
        Begin
          FResizing := crSizeNESW;
          FResizeUL := True;
        End
        Else
        Begin
          FResizing := crSizeWE;
          FResizeUL := True;
        End;
      End
      Else If X >= FWidth - FBorderWidth Then
      Begin
        If Y < FBorderWidth + 8 Then
        Begin
          FResizing := crSizeNESW;
          FResizeUL := False;
        End
        Else If Y >= FHeight - FBorderWidth - 8 Then
        Begin
          FResizing := crSizeNWSE;
          FResizeUL := False;
        End
        Else
        Begin
          FResizing := crSizeWE;
          FResizeUL := False;
        End;
      End
      Else
      Begin
        FResizing := crDefault;
        FResizeUL := False;
      End;
      If FResizing <> crDefault Then
      Begin
        FDragX := X;
        FDragY := Y;
        If FShowBorder Then
        Begin
          If FMinWidth  = 0 Then FMinWidth  := 2 * FBorderWidth;
          If FMinHeight = 0 Then FMinHeight := FClientTop + FBorderWidth;
        End
        Else
        Begin
          If FMinHeight = 0 Then FMinHeight := FClientTop;
        End;
      End;
    End;
  End;
End; // TAbstractWindow.MouseDownAction

Procedure TAbstractWindow.MouseUpAction(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  Inherited MouseUpAction(Button,Shift,X,Y);
  FDragging := False;
  FResizing := crDefault;
End; // TAbstractWindow.MouseUpAction

Procedure TAbstractWindow.MouseMoveAction(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor);
Var W0,H0: Integer;
Begin
  Try
    FUI.LockWindowList;
    Inherited MouseMoveAction(Shift,X,Y,Cursor);
    If {(ssLeft In Shift) And} FDragging Then
    Begin
      Left := Left + (X - FDragX);
      Top  := Top  + (Y - FDragY);
    End
    Else If FResizing <> crDefault Then
    Begin
      W0 := Width;
      H0 := Height;
      Case FResizing Of
        crSizeNWSE:
        Begin
          If FResizeUL Then
          Begin
            Width  := Width  - (X - FDragX);
            Height := Height - (Y - FDragY);
            Left   := Left   - (Width - W0);
            Top    := Top    - (Height - H0);
          End
          Else
          Begin
            Width  := Width  + (X - FDragX);
            Height := Height + (Y - FDragY);
            FDragX := X;
            FDragY := Y;
          End;
        End;
        crSizeNESW:
        Begin
          If FResizeUL Then
          Begin
            Width  := Width  - (X - FDragX);
            Height := Height + (Y - FDragY);
            Left   := Left   - (Width - W0);
            FDragY := Y;
          End
          Else
          Begin
            Width  := Width  + (X - FDragX);
            Height := Height - (Y - FDragY);
            Top    := Top    - (Height - H0);
            FDragX := X;
          End;
        End;
        crSizeNS:
        Begin
          If FResizeUL Then
          Begin
            Height := Height - (Y - FDragY);
            Top    := Top    - (Height - H0);
          End
          Else
          Begin
            Height := Height + (Y - FDragY);
            FDragY := Y;
          End;
        End;
        crSizeWE:
        Begin
          If FResizeUL Then
          Begin
            Width  := Width - (X - FDragX);
            Left   := Left  - (Width - W0);
          End
          Else
          Begin
            Width  := Width + (X - FDragX);
            FDragX := X;
          End;
        End;
      End; // Case
      Cursor := FResizing;
    End
    Else If FShowBorder And FResizeable Then
    Begin
      If Y < FBorderWidth Then
      Begin
             If X < FBorderWidth + 8           Then Cursor := crSizeNWSE
        Else If X >= FWidth - FBorderWidth - 8 Then Cursor := crSizeNESW
        Else Cursor := crSizeNS;
      End
      Else If Y >= FHeight - FBorderWidth Then
      Begin
             If X < FBorderWidth + 8           Then Cursor := crSizeNESW
        Else If X >= FWidth - FBorderWidth - 8 Then Cursor := crSizeNWSE
        Else Cursor := crSizeNS;
      End
      Else If X < FBorderWidth Then
      Begin
             If Y < FBorderWidth + 8            Then Cursor := crSizeNWSE
        Else If Y >= FHeight - FBorderWidth - 8 Then Cursor := crSizeNESW
        Else Cursor := crSizeWE;
      End
      Else If X >= FWidth - FBorderWidth Then
      Begin
             If Y < FBorderWidth + 8            Then Cursor := crSizeNESW
        Else If Y >= FHeight - FBorderWidth - 8 Then Cursor := crSizeNWSE
        Else Cursor := crSizeWE;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractWindow.MouseMoveAction

Function TAbstractWindow.MouseMove(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor): Boolean;
Var
  I    : Integer;
  B    : Boolean;
  Rect : TRect;

Begin
  B := False;
  Try
    FUI.LockWindowList;
    If FVisible And (evtMouseMove In FAcceptEvents) Then
    Begin
      If FParent = Nil Then
      Begin
        Dec(X,FLeft);
        Dec(Y,FTop);
      End;
      If fResizing = crDefault Then
      Begin
        I := 0;
        While (I < FChildren.Count) And Not B Do
        Begin
          B := B Or TAbstractComponent(FChildren.Objects[I]).MouseMove(Shift,X,Y,Cursor);
          Inc(I);
        End; // While
      End;
      If Not B Then
      Begin
        Rect := GetClipRect;
        B    := B Or (FResizing <> crDefault) Or ((X >= Rect.Left) And (Y >= Rect.Top) And (X <= Rect.Right) And (Y <= Rect.Bottom)) Or FDragging;
        If B Then
        Begin
          MouseMoveAction(Shift,X,Y,Cursor);
        End;
      End;
      If B <> FMouseOver Then
      Begin
        FMouseOver := B;
             If Assigned(FOnMouseEnter) And     B Then FOnMouseEnter(Self)
        Else If Assigned(FOnMouseExit)  And Not B Then FOnMouseExit(Self);
        QueueRepaint;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
  Result := B;
End; // TAbstractWindow.MouseMove

Function TAbstractWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
Var
  I    : Integer;
  B    : Boolean;
  Rect : TRect;

Begin
  B := False;
  Try
    FUI.LockWindowList;
    If FVisible And (evtMouseUp In FAcceptEvents) Then
    Begin
      If FParent = Nil Then
      Begin
        Dec(X,FLeft);
        Dec(Y,FTop);
      End;
      I := 0;
      While (I < FChildren.Count) And Not B Do
      Begin
        B := B Or TAbstractComponent(FChildren.Objects[I]).MouseUp(Button,Shift,X,Y);
        Inc(I);
      End; // While
      If Not B Then
      Begin
        Rect := GetClipRect;
        B    := B Or (FResizing <> crDefault) Or ((X >= Rect.Left) And (Y >= Rect.Top) And (X <= Rect.Right) And (Y <= Rect.Bottom));
        If B Then
        Begin
          MouseUpAction(Button,Shift,X,Y);
          If Assigned(FOnMouseUp) Then FOnMouseUp(Self,Button,Shift,X,Y);
        End;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
  Result := B;
End; // TAbstractWindow.MouseUp

Function TAbstractWindow.ContainsCursor(X, Y: Integer): TAbstractComponent;
Var
  I    : Integer;
  Comp : TAbstractComponent;
  Rect : TRect;

Begin
  Comp := Nil;
  Try
    FUI.LockWindowList;
    If FVisible Then
    Begin
      If FParent = Nil Then
      Begin
        Dec(X,FLeft);
        Dec(Y,FTop);
      End;
      I := 0;
      While (I < FChildren.Count) And (Comp = Nil) Do
      Begin
        Comp := TAbstractComponent(FChildren.Objects[I]).ContainsCursor(X,Y);
        Inc(I);
      End; // While
      If Comp = Nil Then
      Begin
        Rect := GetClipRect;
        If ((X >= Rect.Left) And (Y >= Rect.Top) And (X <= Rect.Right) And (Y <= Rect.Bottom)) Or FDragging Then Comp := Self;
      End;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
  Result := Comp;
End; // TAbstractWindow.ContainsCursor

Procedure TAbstractWindow.SetCaption(St: String);
Begin
  Try
    FUI.LockWindowList;
    If FCaption <> St Then
    Begin
      FCaption := St;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractWindow.SetCaption

Procedure TAbstractWindow.SetShowCaption(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If FShowCaption <> B Then
    Begin
      FShowCaption         := B;
      FCloseButton.Visible := B;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractWindow.SetShowCaption

Procedure TAbstractWindow.SetShowBorder(B: Boolean);
Begin
  Try
    FUI.LockWindowList;
    If FShowBorder <> B Then
    Begin
      If B Then
      Begin
        FMinWidth  := 2 * FBorderWidth;
        FMinHeight := FClientTop + FBorderWidth;
      End
      Else
      Begin
        FMinWidth  := 0;
        FMinHeight := FClientTop;
      End;
      FShowBorder := B;
      If Not B Then FResizeable := B;
      QueueRepaint;
    End;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractWindow.SetShowBorder

Procedure TAbstractWindow.CloseButtonClick(Sender: TObject);
Begin
  Try
    FUI.LockWindowList;
    Visible := False;
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractWindow.CloseButtonClick

Function TAbstractWindow.GetChildClipRect(ChildComponent: TAbstractComponent): TRect;
Var
  X,Y   : Integer;
  Rect  : TRect;
  Rect1 : TRect;

Begin
  If ChildComponent = FCloseButton Then
  Begin
    CalcAbsoluteXY(X,Y);
    Rect.Left   := X;
    Rect.Top    := Y;
    Rect.Right  := Rect.Left + FWidth     - 1;
    Rect.Bottom := Rect.Top  + FClientTop - 2;
    If FShowBorder Then
    Begin
      Inc(Rect.Left);
      Inc(Rect.Top);
      Dec(Rect.Right);
    End;
    If FParent <> Nil Then
    Begin
      Rect1       := FParent.GetChildClipRect(Self);
      Rect.Left   := Max(Rect.Left,   Rect1.Left);
      Rect.Top    := Max(Rect.Top,    Rect1.Top);
      Rect.Right  := Min(Rect.Right,  Rect1.Right);
      Rect.Bottom := Min(Rect.Bottom, Rect1.Bottom);
    End;
  End
  Else Rect := Inherited GetChildClipRect(ChildComponent);
  Result := Rect;
End; // TAbstractWindow.GetChildClipRect

Procedure TAbstractWindow.Show;
Begin
  If (FUI.FModalWindow = Nil) And Not Visible Then
  Begin
    FModalResult := mrNone;
    SetVisible(True);
    FUI.MoveToFront(Self);
  End;
End; // TAbstractWindow.Show

Procedure TAbstractWindow.ShowModal;
Begin
  If (Not Visible) And FUI.MakeModal(Self) Then
  Begin
    FModalResult := mrNone;
    SetVisible(True);
    FUI.MoveToFront(Self);
  End;
End; // TAbstractWindow.ShowModal

Procedure TAbstractWindow.Hide;
Begin
  SetVisible(False);
End; // TAbstractWindow.Hide

Procedure TAbstractWindow.SetZOrder(I: Integer);
Begin
  Try
    FUI.LockWindowList;
    FZOrder := I;
    If Assigned(FOnZOrderChange) Then FOnZOrderChange(Self);
  Finally
    FUI.UnlockWindowList;
  End;
End; // TAbstractWindow.SetZOrder

// -------------------------------
// TAbstractUI
// -------------------------------

Constructor TAbstractUI.Create;
Begin
  FQueueMutex    := TCriticalSection.Create;
  FWindowsMutex  := TCriticalSection.Create;
  FWindows       := TStringList.Create;
  FRepaintQueue  := TStringList.Create;
  FOnMoveToFront := Nil;
  FModalWindow   := Nil;
End; // TAbstractUI.Create

Destructor TAbstractUI.Destroy;
Var I: Integer;
Begin
  For I := 0 To FWindows.Count  - 1 Do FWindows.Objects[I].Free;
  FWindows.Free;
  FRepaintQueue.Free;
  FQueueMutex.Free;
  FWindowsMutex.Free;
End; // TAbstractUI.Destroy

Function TAbstractUI.AddWindow(ACaption: String): TAbstractWindow;
Var Window: TAbstractWindow;
Begin
  Window    := TAbstractWindow.Create(0,0,64,64,ACaption,Self);
  Window.UI := Self;
  Try
    FWindowsMutex.Enter;
    FWindows.AddObject('',Window);
    Window.ZOrder := FWindows.Count - 1;
  Finally
    FWindowsMutex.Leave;
  End;
  Result := Window;
End; // TAbstractUI.AddWindow

Procedure TAbstractUI.RemoveWindow(Window: TAbstractWindow);
Var
  I,J,K : Integer;
  W     : TAbstractWindow;

Begin
  // Locking two items in succession can result in a deadlock if we're not very careful...as a general
  // rule we must always lock them in the same order, we should only do it in one place, and we must
  // NEVER call this from a thread that already has the inner item locked.

  Try
    FWindowsMutex.Enter;
    Try
      FQueueMutex.Enter;
      I := FRepaintQueue.IndexOfObject(Window);
      If I >= 0 Then FRepaintQueue.Delete(I);
    Finally
      FQueueMutex.Leave;
    End;
    If (Window = FFocusedComponent) Or
       ((FFocusedComponent <> Nil) And (FFocusedComponent.Owner = Window)) Then FFocusedComponent := Nil;
    I := FWindows.IndexOfObject(Window);
    If I >= 0 Then
    Begin
      J := Window.ZOrder;
      FWindows.Delete(I);
      K := -1;
      For I := 0 To FWindows.Count - 1 Do
      Begin
        W := TAbstractWindow(FWindows.Objects[I]);
        If W.ZOrder > J Then
        Begin
          W.ZOrder := W.ZOrder - 1;
          If W.ZOrder = 0  Then K := I;
        End;
      End; // For I
      If (K >= 0) And Assigned(FOnMoveToFront) Then FOnMoveToFront(TAbstractWindow(FWindows.Objects[K]),K);
    End;
  Finally
    FWindowsMutex.Leave;
  End;
End; // TAbstractUI.RemoveWindow

Function TAbstractUI.GetWindow(Index: Integer): TAbstractWindow;
Begin
  Try
    FWindowsMutex.Enter;
    If (Index >= 0) And (Index < FWindows.Count)
     Then Result := TAbstractWindow(FWindows.Objects[Index])
     Else Result := Nil;
  Finally
    FWindowsMutex.Leave;
  End;
End; // TAbstractUI.GetPanel

Function TAbstractUI.GetNumWindows: Integer;
Begin
  Try
    FWindowsMutex.Enter;
    Result := FWindows.Count;
  Finally
    FWindowsMutex.Leave;
  End;
End; // TAbstractUI.GetNumWindows

Procedure TAbstractUI.Repaint;
Var
  I    : Integer;
  List : TStringList;

Begin
  // First copy the queue list

  List := TStringList.Create;
  Try
    FQueueMutex.Enter;
    For I := 0 To FRepaintQueue.Count - 1 Do List.AddObject('',FRepaintQueue.Objects[I]);
    FRepaintQueue.Clear;
  Finally
    FQueueMutex.Leave;
  End;

  // Paint the items in the list

  For I := 0 To List.Count - 1 Do TAbstractComponent(List.Objects[I]).Repaint;
  List.Free;
End; // TAbstractUI.Repaint

Procedure TAbstractUI.AddRepaint(Component: TAbstractComponent);
Var I: Integer;
Begin
  Try
    FQueueMutex.Enter;
    I := FRepaintQueue.IndexOfObject(Component);
    If I >= 0 Then FRepaintQueue.Delete(I);
    FRepaintQueue.AddObject('',Component);
  Finally
    FQueueMutex.Leave;
  End;
End; // TAbstractUI.AddRepaint

Function TAbstractUI.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
Var
  I     : Integer;
  Found : Boolean;
  Wins  : Array Of TAbstractWindow;
  W     : TAbstractWindow;

Begin
  Try
    FWindowsMutex.Enter;

    If (FModalWindow <> Nil) And Not FModalWindow.Visible Then FModalWindow := Nil;
    If FModalWindow = Nil Then
    Begin
      SetLength(Wins,FWindows.Count);
      For I := 0 To FWindows.Count - 1 Do
      Begin
        W := TAbstractWindow(FWindows.Objects[I]);
        Wins[W.ZOrder] := W;
      End; // For I

      I     := 0;
      Found := False;
      While (I <= High(Wins)) And Not Found Do
      Begin
        If Wins[I].Visible Then Found := Found Or Wins[I].MouseDown(Button,Shift,X,Y);
        If Found Then MoveToFront(Wins[I]);
        Inc(I);
      End; // While

      SetLength(Wins,0);
    End
    Else Found := FModalWindow.MouseDown(Button,Shift,X,Y);
    Result := Found;
  Finally
    FWindowsMutex.Leave;
  End;
End; // TAbstractUI.MouseDown

Function TAbstractUI.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
Var
  I     : Integer;
  Found : Boolean;
  Wins  : Array Of TAbstractWindow;
  W     : TAbstractWindow;

Begin
  Try
    FWindowsMutex.Enter;

    If (FModalWindow <> Nil) And Not FModalWindow.Visible Then FModalWindow := Nil;
    If FModalWindow = Nil Then
    Begin
      SetLength(Wins,FWindows.Count);
      For I := 0 To FWindows.Count - 1 Do
      Begin
        W := TAbstractWindow(FWindows.Objects[I]);
        Wins[W.ZOrder] := W;
      End; // For I

      I     := 0;
      Found := False;
      While (I <= High(Wins)) And Not Found Do
      Begin
        If Wins[I].Visible Then Found := Found Or Wins[I].MouseUp(Button,Shift,X,Y);

        // Need to do this in case we aren't on top of a window or it isn't visible

        Wins[I].FResizing := crDefault;
        Inc(I);
      End; // While

      SetLength(Wins,0);
    End
    Else
    Begin
      Found := FModalWindow.MouseUp(Button,Shift,X,Y);
      FModalWindow.FResizing := crDefault;
    End;
    Result := Found;
  Finally
    FWindowsMutex.Leave;
  End;
End; // TAbstractUI.MouseUp

Function TAbstractUI.MouseMove(Shift: TShiftState; X, Y: Integer; Var Cursor: TCursor): Boolean;
Var
  I     : Integer;
  Found : Boolean;
  Wins  : Array Of TAbstractWindow;
  W     : TAbstractWindow;

Begin
  Try
    FWindowsMutex.Enter;

    If (FModalWindow <> Nil) And Not FModalWindow.Visible Then FModalWindow := Nil;
    If FModalWindow = Nil Then
    Begin
      SetLength(Wins,FWindows.Count);
      For I := 0 To FWindows.Count - 1 Do
      Begin
        W := TAbstractWindow(FWindows.Objects[I]);
        Wins[W.ZOrder] := W;
      End; // For I

      I     := 0;
      Found := False;
      While (I <= High(Wins)) And Not Found Do
      Begin
        If Wins[I].Visible Then Found := Found Or Wins[I].MouseMove(Shift,X,Y,Cursor);
        Inc(I);
      End; // While

      SetLength(Wins,0);
    End
    Else Found := FModalWindow.MouseMove(Shift,X,Y,Cursor);
    Result := Found;
  Finally
    FWindowsMutex.Leave;
  End;
End; // TAbstractUI.MouseMove

Function TAbstractUI.ContainsCursor(X, Y: Integer): TAbstractComponent;
Var
  I    : Integer;
  Comp : TAbstractComponent;
  Wins : Array Of TAbstractWindow;
  W    : TAbstractWindow;

Begin
  Try
    FWindowsMutex.Enter;

    If (FModalWindow <> Nil) And Not FModalWindow.Visible Then FModalWindow := Nil;
    If FModalWindow = Nil Then
    Begin
      SetLength(Wins,FWindows.Count);
      For I := 0 To FWindows.Count - 1 Do
      Begin
        W := TAbstractWindow(FWindows.Objects[I]);
        Wins[W.ZOrder] := W;
      End; // For I

      I    := 0;
      Comp := Nil;
      While (I <= High(Wins)) And (Comp = Nil) Do
      Begin
        If Wins[I].Visible Then Comp := Wins[I].ContainsCursor(X,Y);
        Inc(I);
      End; // While

      SetLength(Wins,0);
    End
    Else Comp := FModalWindow.ContainsCursor(X,Y);
    Result := Comp;
  Finally
    FWindowsMutex.Leave;
  End;
End; // TAbstractUI.ContainsCursor

Function TAbstractUI.KeyDown(Key,Shift: Integer; C: Char): Boolean;
Var
  I     : Integer;
  Found : Boolean;
  Wins  : Array Of TAbstractWindow;
  W     : TAbstractWindow;

Begin
  Try
    FWindowsMutex.Enter;

    If (FModalWindow <> Nil) And Not FModalWindow.Visible Then FModalWindow := Nil;
    If FModalWindow = Nil Then
    Begin
      SetLength(Wins,FWindows.Count);
      For I := 0 To FWindows.Count - 1 Do
      Begin
        W := TAbstractWindow(FWindows.Objects[I]);
        Wins[W.ZOrder] := W;
      End; // For I

      I     := 0;
      Found := False;
      While (I < FWindows.Count) And Not Found Do
      Begin
        If Wins[I].Visible Then Found := Found Or Wins[I].KeyDown(Key,Shift,C);
        Inc(I);
      End; // While

      SetLength(Wins,0);
    End
    Else Found := FModalWindow.KeyDown(Key,Shift,C);
    Result := Found;
  Finally
    FWindowsMutex.Leave;
  End;
End; // TAbstractUI.KeyDown

Procedure TAbstractUI.MoveToFront(Window: TAbstractWindow);
Var
  I,J : Integer;
  W   : TAbstractWindow;

Begin
  If Window.ZOrder > 0 Then
  Begin
    Try
      FWindowsMutex.Enter;
      J := Window.ZOrder;
      Window.ZOrder := 0;
      For I := 0 To FWindows.Count - 1 Do
      Begin
        W := TAbstractWindow(FWindows.Objects[I]);
        If (W <> Window) And (W.ZOrder < J) Then W.ZOrder := W.ZOrder + 1;
      End; // For I
      If Assigned(FOnMoveToFront) Then FOnMoveToFront(Window,FWindows.IndexOfObject(Window));
    Finally
      FWindowsMutex.Leave;
    End;
  End;
End; // TAbstractUI.MoveToFront

Function TAbstractUI.MakeModal(Window: TAbstractWindow): Boolean;
Begin
  Try
    FWindowsMutex.Enter;
    If FModalWindow = Nil Then
    Begin
      FModalWindow := Window;
      Result       := True;
    End
    Else Result := False;
  Finally
    FWindowsMutex.Leave;
  End;
End; // TAbstractUI.MakeModal

Function TAbstractUI.GetIndexOfWindow(Window: TAbstractWindow): Integer;
Begin
  Try
    FWindowsMutex.Enter;
    Result := FWindows.IndexOfObject(Window);
  Finally
    FWindowsMutex.Leave;
  End;
End; // TAbstractUI.GetIndexOfWindow

Procedure TAbstractUI.LockWindowList;
Begin
  FWindowsMutex.Enter;
End; // TAbstractUI.LockWindowList

Procedure TAbstractUI.UnlockWindowList;
Begin
  FWindowsMutex.Leave;
End; // TAbstractUI.UnlockWindowList

Procedure TAbstractUI.LockRepaintQueue;
Begin
  FQueueMutex.Enter;
End; // TAbstractUI.LockRepaintQueue

Procedure TAbstractUI.UnlockRepaintQueue;
Begin
  FQueueMutex.Leave;
End; // TAbstractUI.UnlockRepaintQueue

Initialization
  SetupKeyboardHash;
Finalization
  KeyboardHash.Free;
end.
