unit SharedComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Forms, Buttons, DB,
  DBCtrls, ClearTypeText;

type
  TChangeValueEvent = Procedure(Sender: TObject; Var Value: Integer) Of Object;
  TSharedComboBox = Class;
  TSharedComboBoxStrings = Class(TStringList)
  Private
    FComboBoxes : TStringList;
    FAutoUpdate : Boolean;
  Protected
    Procedure   PutObject(Index: Integer; AObject: TObject); Override;
    Procedure   UpdateComboBoxes;
    Procedure   SetAutoUpdate(B: Boolean);
    Property    ComboBoxes : TStringList Read FComboBoxes;
  Public
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   Clear; Override;
    Procedure   Delete(Index: Integer); Override;
    Function    Add(Const S: String): Integer; Override;
    Procedure   Insert(Index: Integer; Const S: String); Override;
    Property    AutoUpdate : Boolean Read FAutoUpdate Write SetAutoUpdate;
  End;

  TComboBoxForm = Class(TCustomForm)
  Protected
    Procedure   CreateParams(Var Params: TCreateParams); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); Override;
    Destructor  Destroy; Override;
  End;

  TSharedComboBox = Class(TWinControl)
  Private
    { Private declarations }
    FClearTypeLink : TClearTypeLink;
    FEdit          : TEdit;
    FListBox       : TListBox;
    FButton        : TSpeedButton;
    FScrollBar     : TScrollBar;
    FItems         : TSharedComboBoxStrings;
    FDropDownCount : Integer;
    FItemHeight    : Integer;
    FIsLoaded      : Boolean;
    FForm          : TComboBoxForm;
    FInForm        : Integer;
    FItemIndex     : Integer;
    FSBChanging    : Boolean;
    FEditChanging  : Boolean;
    FStyle         : TComboBoxStyle;
    FOnChange      : TNotifyEvent;
    FAdjustedIH    : Boolean;
    Procedure   SetItems(List: TSharedComboBoxStrings); Virtual;
    Procedure   SetDropDownCount(Count: Integer);
    Procedure   SetItemHeight(AHeight: Integer);
    Procedure   AdjustSize(Var W, H: Integer); Reintroduce;
    Procedure   WMSize(Var Message: TWMSize); Message WM_SIZE;
    Procedure   SetStyle(S: TComboBoxStyle); Virtual;
    Procedure   SetItemIndex(Index: Integer);
    Procedure   FixPageSize;
    procedure   CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    { Protected declarations }
    Procedure   Loaded; Override;
    Procedure   ButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure   ScrollBarChange(Sender: TObject);
    Procedure   FormExit(Sender: TObject);
    Procedure   Adjust(Var L,T: Integer);
    Procedure   DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    Procedure   SetListBoxPos;
    Procedure   EditChange(Sender: TObject);
    Procedure   ListBoxClick(Sender: TObject);
    Procedure   EditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure   ListBoxKeyPress(Sender: TObject; Var Key: Char);
    Procedure   SetEnabled(Value: Boolean); Override;
    Function    GetCtl3D: Boolean;
    Procedure   SetCtl3D(B: Boolean);
    Function    GetBorderStyle: TBorderStyle;
    Procedure   SetBorderStyle(B: TBorderStyle);
    Function    GetDroppedDown: Boolean;
    Procedure   SetDroppedDown(Value: Boolean);
    Procedure   Change; Virtual;
    Function    GetClearType: TClearTypeText;
    Procedure   SetClearType(AClearType: TClearTypeText);
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
    Procedure   SetBounds(ALeft, ATop, AWidth, AHeight: Integer); Override;
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); Override;
  published
    { Published declarations }
    Property    DropDownCount : Integer                Read FDropDownCount Write SetDropDownCount;
    Property    ItemHeight    : Integer                Read FItemHeight    Write SetItemHeight;
    Property    Style         : TComboBoxStyle         Read FStyle         Write SetStyle;
    Property    Items         : TSharedComboBoxStrings Read FItems         Write SetItems;
    Property    ItemIndex     : Integer                Read FItemIndex     Write SetItemIndex;
    Property    Align;
    Property    Anchors;
    Property    Enabled;
    Property    Text;
    Property    Visible;
    Property    OnChange      : TNotifyEvent           Read FOnChange      Write FOnChange;
    Property    Ctl3D         : Boolean                Read GetCtl3D       Write SetCtl3D;
    Property    BorderStyle   : TBorderStyle           Read GetBorderStyle Write SetBorderStyle;
    Property    DroppedDown   : Boolean                Read GetDroppedDown Write SetDroppedDown;
    Property    ClearType     : TClearTypeText         Read GetClearType   Write SetClearType Default Nil;
  end;

  TDBSharedComboBox = Class(TSharedComboBox)
  Private
    FDataLink            : TFieldDataLink;
    FChanging            : Boolean;
    FOnGetValueFromIndex : TChangeValueEvent;
    FOnGetIndexFromValue : TChangeValueEvent;
    Procedure   DataChange(Sender: TObject);
    Procedure   EditingChange(Sender: TObject);
    Function    GetComboItem: Integer;
    Function    GetDataField: String;
    Function    GetDataSource: TDataSource;
    Function    GetField: TField;
    Function    GetReadOnly: Boolean;
    Procedure   SetComboItem(Const Value: Integer);
    Procedure   SetDataField(Const Value: String);
    Procedure   SetDataSource(Value: TDataSource);
    Procedure   SetReadOnly(Value: Boolean);
    Procedure   UpdateData(Sender: TObject);
    Procedure   CMExit(Var Message: TCMExit); Message CM_EXIT;
    Procedure   CMGetDataLink(Var Message: TMessage); Message CM_GETDATALINK;
    Procedure   SetItems(List: TSharedComboBoxStrings); Override;
  Protected
    Procedure   Change; Override;
    Procedure   Notification(AComponent: TComponent; Operation: TOperation); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
    Function    ExecuteAction(Action: TBasicAction): Boolean; Override;
    Function    UpdateAction(Action: TBasicAction): Boolean; Override;
    Property    Field      : TField      Read GetField;
  Published
    Property    DropDownCount;
    Property    ItemHeight;
    Property    Items;
    Property    ItemIndex;
    Property    Align;
    Property    Anchors;
    Property    Enabled;
    Property    Text;
    Property    Visible;
    Property    OnChange;
    Property    Ctl3D;
    Property    BorderStyle;
    Property    DroppedDown;
    Property    ClearType;
    Property    DataField  : String      Read GetDataField  Write SetDataField;
    Property    DataSource : TDataSource Read GetDataSource Write SetDataSource;
    Property    ReadOnly   : Boolean     Read GetReadOnly   Write SetReadOnly Default False;
    Property    OnGetValueFromIndex : TChangeValueEvent Read FOnGetValueFromIndex Write FOnGetValueFromIndex;
    Property    OnGetIndexFromValue : TChangeValueEvent Read FOnGetIndexFromValue Write FOnGetIndexFromValue;
  End;

procedure Register;

implementation

{$R SHAREDCOMBOBOX}

Uses Graphics,VDBConsts;

Const MinSize = 10;

// TSharedComboBoxStrings

Constructor TSharedComboBoxStrings.Create;
Begin
  Inherited;
  FComboBoxes := TStringList.Create;
  FAutoUpdate := True;
End; // TSharedComboBoxStrings.Create

Destructor TSharedComboBoxStrings.Destroy;
Begin
  FComboBoxes.Free;
  Inherited;
End; // TSharedComboBoxStrings.Destroy

Procedure TSharedComboBoxStrings.Clear;
Begin
  Inherited;
  UpdateComboBoxes;
End; // TSharedComboBoxStrings.Clear

Procedure TSharedComboBoxStrings.Delete(Index: Integer);
Begin
  Inherited Delete(Index);
  UpdateComboBoxes;
End; // TSharedComboBoxStrings.Delete

Function TSharedComboBoxStrings.Add(Const S: String): Integer;
Begin
  Result := Inherited Add(S);
  UpdateComboBoxes;
End; // TSharedComboBoxStrings.Add

Procedure TSharedComboBoxStrings.Insert(Index: Integer; Const S: String);
Begin
  Inherited Insert(Index,S);
  UpdateComboBoxes;
End; // TSharedComboBoxStrings.Insert

Procedure TSharedComboBoxStrings.PutObject(Index: Integer; AObject: TObject);
Begin
  Inherited PutObject(Index,AObject);
  UpdateComboBoxes;
End; // TSharedComboBoxStrings.PutObject

Procedure TSharedComboBoxStrings.UpdateComboBoxes;
Var
  I  : Integer;
  CB : TSharedComboBox;

Begin
  If FAutoUpdate Then
   For I := 0 To FComboBoxes.Count - 1 Do
   Begin
     CB := FComboBoxes.Objects[I] As TSharedComboBox;
     CB.Items := Self;
     CB.Invalidate;
     CB.Repaint;
   End; // For I
End; // TSharedComboBoxStrings.UpdateComboBoxes

Procedure TSharedComboBoxStrings.SetAutoUpdate(B: Boolean);
Begin
  FAutoUpdate := B;
  UpdateComboBoxes;
End; // TSharedComboBoxStrings.SetAutoUpdate

// TComboBoxForm

Constructor TComboBoxForm.Create(AOwner: TComponent);
Begin
  CreateNew(AOwner);
End; // TComboBoxForm.Create

Constructor TComboBoxForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
Begin
  Inherited CreateNew(AOwner,Dummy);
  ControlStyle := ControlStyle - [csFramed];
End; // TComboBoxForm.CreateNew

Destructor TComboBoxForm.Destroy;
Begin
  Inherited;
End; // TComboBoxForm.Destroy

Procedure TComboBoxForm.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  Params.Style := WS_POPUP Or WS_BORDER;
End; // TComboBoxForm.CreateParams

// TSharedComboBox

Constructor TSharedComboBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FClearTypeLink         := TClearTypeLink.Create(Self);
  ControlStyle           := ControlStyle - [csAcceptsControls, csSetCaption] + [csFramed, csOpaque];
  FForm                  := TComboBoxForm.Create(Self);
  FEdit                  := TEdit.Create(Self);
  FListBox               := TListBox.Create(FForm);
  FButton                := TSpeedButton.Create(Self);
  FScrollBar             := TScrollBar.Create(FForm);
  FEdit.Visible          := True;
  FListBox.Visible       := True;
  FButton.Visible        := True;
  FScrollBar.Visible     := False;
  FEdit.Enabled          := True;
  FListBox.Enabled       := True;
  FButton.Enabled        := True;
  FScrollBar.Enabled     := True;
  FEdit.Parent           := Self;
  FListBox.Parent        := FForm;
  FButton.Parent         := Self;
  FScrollBar.Parent      := FForm;
  FScrollBar.Kind        := sbVertical;
  FButton.OnMouseDown    := ButtonMouseDown;
  FListBox.OnDrawItem    := DrawItem;
  FListBox.Style         := lbOwnerDrawFixed;
  FForm.BorderStyle      := bsSingle;
  FForm.AutoScroll       := False;
  FForm.OnDeactivate     := FormExit;
  FListBox.BorderStyle   := bsNone;
  FItems                 := Nil;
  FIsLoaded              := False;
  FInForm                := 0;
  FDropDownCount         := 8;
  FSBChanging            := False;
  FListBox.ItemHeight    := 16;
  FItemHeight            := FListBox.ItemHeight;
  FButton.Glyph.Handle   := LoadBitmap(HInstance, 'DropDown');
  FButton.NumGlyphs      := 1;
  FButton.Margin         := -1;
  FButton.Spacing        := -1;
  FButton.Layout         := blGlyphRight;
  FButton.Invalidate;
  FScrollBar.OnChange    := ScrollBarChange;
  FScrollBar.LargeChange := FDropDownCount;
  FListBox.OnClick       := ListBoxClick;
  FListBox.OnKeyPress    := ListBoxKeyPress;
  FEdit.OnMouseDown      := EditMouseDown;
  FEdit.BorderStyle      := bsSingle;
  FStyle                 := csDropDown;
  ParentCtl3D            := False;
  Ctl3D                  := False;
  FEdit.Ctl3D            := True;
  FForm.Ctl3D            := True;
  FItemIndex             := -1;
  Width                  := 80;
  Height                 := 21;
  FOnChange              := Nil;
  FAdjustedIH            := False;
  FixPageSize;
End; // TSharedComboBox.Create

Destructor TSharedComboBox.Destroy;
Begin
  FClearTypeLink.Free;
  Inherited;
End; // TSharedComboBox.Destroy

Procedure TSharedComboBox.CMFontChanged(Var Message: TMessage);
Begin
  inherited;
  FEdit.Font    := Font;
  FForm.Font    := Font;
  FListBox.Font := Font;
  Try
    FListBox.ItemHeight := FListBox.Canvas.TextHeight('Mg') + 3;
  Finally
  End;
  Invalidate;
End; // TSharedComboBox.CMFontChanged

Procedure TSharedComboBox.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If (FClearTypeLink <> Nil) And
     (FClearTypeLink.ClearType <> Nil) And
     (AComponent = FClearTypeLink.ClearType) And
     (Operation = opRemove) Then FClearTypeLink.ClearType := Nil;
  Inherited;
End; // TSharedComboBox.Notification

Function TSharedComboBox.GetClearType: TClearTypeText;
Begin
  Result := FClearTypeLink.ClearType;
End; // TSharedComboBox.GetClearType

Procedure TSharedComboBox.SetClearType(AClearType: TClearTypeText);
Begin
  FClearTypeLink.ClearType := AClearType;
  Invalidate;
End; // TSharedComboBox.SetClearType

Procedure TSharedComboBox.SetItems(List: TSharedComboBoxStrings);
Var W,H: Integer;
Begin
  FItems             := List;
  If (List <> Nil) And (List.ComboBoxes.IndexOfObject(Self) < 0) Then List.ComboBoxes.AddObject('',Self);
  W                  := Width;
  H                  := Height;
  FEditChanging      := True;
  FEdit.Text         := '';
  Text               := '';
  Application.ProcessMessages;
  FEditChanging      := False;
  FItemIndex         := -1;
  FListBox.ItemIndex := -1;
  AdjustSize(W,H);
End; // TSharedComboBox.SetList

Procedure TSharedComboBox.SetDropDownCount(Count: Integer);
Var W,H: Integer;
Begin
  FDropDownCount         := Count;
  FScrollBar.LargeChange := FDropDownCount;
  W                      := Width;
  H                      := Height;
  AdjustSize(W,H);
End; // TSharedComboBox.SetDropDownCount

Procedure TSharedComboBox.Change;
Begin
  Inherited Changed;
  If Assigned(FOnChange) Then FOnChange(Self);
End; // TSharedComboBox.Change

Procedure TSharedComboBox.FixPageSize;
Var I: Integer;
Begin
  I := FDropDownCount;
  If I < 0 Then I := 0;
  If (FItems <> Nil) And (I > FItems.Count) Then I := FItems.Count;
  FScrollBar.PageSize := I;
End; // TSharedComboBox.FixPageSize

Procedure TSharedComboBox.SetItemHeight(AHeight: Integer);
Var W,H: Integer;
Begin
  FItemHeight         := AHeight;
  If FListBox <> Nil Then FListBox.ItemHeight := AHeight;
  W                   := Width;
  H                   := Height;
  AdjustSize(W,H);
End; // TSharedComboBox.SetItemHeight

Procedure AbsLeft(Var L: Integer; Control: TWinControl);
Begin
  Inc(L,Control.Left);
  If Control.Parent <> Nil Then AbsLeft(L,Control.Parent);
End; // AbsLeft

Procedure AbsTop(Var T: Integer; Control: TWinControl);
Begin
  Inc(T,Control.Top);
  If Control.Parent <> Nil Then AbsTop(T,Control.Parent);
End; // AbsTop

Function TSharedComboBox.GetDroppedDown: Boolean;
Begin
  Result := (FForm <> Nil) And FForm.Visible;
End; // TSharedComboBox.GetDroppedDown

Procedure TSharedComboBox.SetDroppedDown(Value: Boolean);
Begin
  If FForm <> Nil Then
  Begin
    If Not Value Then FForm.Visible := False
    Else
    Begin
      SetListBoxPos;
      FForm.Visible := True;
    End;
    FInForm := 0;
  End;
End; // TSharedComboBox.SetDroppedDown

Procedure TSharedComboBox.Adjust(Var L,T: Integer);
Var
  DW,DH : Integer;
  C     : TControl;
  F     : TForm;

Begin
  C := Self;
  While (C <> Nil) And Not (C Is TForm) Do C := C.Parent;
  If (C <> Nil) And (C Is TForm) Then
  Begin
    F  := C As TForm;
    DW := F.Width  - F.ClientWidth;
    DH := F.Height - F.ClientHeight;
    Inc(L,DW Div 2);
    Inc(T,DH - (DW Div 2));
  End;
End; // TSharedComboBox.Adjust

Procedure TSharedComboBox.AdjustSize(Var W, H: Integer);
Var I,L,T: Integer;
Begin
  If (FEdit      = Nil) Or
     (FListBox   = Nil) Or
     (FButton    = Nil) Or
     (FScrollBar = Nil) Or
     (csLoading In ComponentState) Then Exit;
  If (Parent <> Nil) And Not FAdjustedIH Then
  Begin
    FAdjustedIH         := True;
    FItemHeight         := FListBox.Canvas.TextHeight('Mg');
    FListBox.ItemHeight := FItemHeight;
    H                   := FEdit.Height;
  End;
  If W < FScrollBar.Width + 10 Then W := FScrollBar.Width + 10;
  If H < MinSize Then H := MinSize;
  FEdit.SetBounds(0,0,W - FScrollBar.Width,H);
  FButton.SetBounds(W - FScrollBar.Width,0,FScrollBar.Width,H);
  L := 0; AbsLeft(L,Self);
  T := 0; AbsTop(T,Self);
  Adjust(L,T);
  If (FItems <> Nil) And (FItems.Count > FDropDownCount) Then
  Begin
    FListBox.SetBounds(0,0,W - FScrollBar.Width - 2,FDropDownCount * FListBox.ItemHeight);
    While FListBox.Items.Count < FDropDownCount Do FListBox.Items.Add('');
    FScrollBar.SetBounds(W - FScrollBar.Width - 2,0,FScrollBar.Width,FListBox.Height);
    FScrollBar.Visible := True;
    If FItems.Count > 0
     Then FScrollBar.Max := FItems.Count - 1// - FDropDownCount
     Else
     Begin
       FScrollBar.Min      := 0;
       FScrollBar.PageSize := 0;
       FScrollBar.Max      := 0;
     End;
  End
  Else
  Begin
    If FItems <> Nil Then I := FItems.Count Else I := 1;
    FScrollBar.Min      := 0;
    FScrollBar.PageSize := 0;
    FScrollBar.Max      := 0;
//    If FIsLoaded Then
//    Begin
      While FListBox.Items.Count > I Do FListBox.Items.Delete(FListBox.Items.Count - 1);
      While FListBox.Items.Count < I Do FListBox.Items.Add('');
//    End;
    FListBox.SetBounds(0,0,W - 2,FListBox.ItemHeight * I);
    FScrollBar.Visible := False;
  End;
  FixPageSize;
  If T < 0 Then T := 0;
  FForm.SetBounds(L,T,W,FListBox.Height + 2);
End; // TSharedComboBox.AdjustSize

Procedure TSharedComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
Var W, H: Integer;
Begin
  W := AWidth;
  H := AHeight;
  AdjustSize(W, H);
  Inherited SetBounds(ALeft, ATop, W, H);
End; // TSharedComboBox.SetBounds

Procedure TSharedComboBox.WMSize(var Message: TWMSize);
Var W, H: Integer;
Begin
  Inherited;

  // check for minimum size

  W := Width;
  H := Height;
  AdjustSize(W, H);
  If (W <> Width) Or (H <> Height) Then Inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
End; // TSharedComboBox.WMSize

Procedure TSharedComboBox.FormExit(Sender: TObject);
Begin
  If FInForm = 0 Then
  Begin
    FInForm := 1;
    If FForm.Visible Then
    Begin
      Application.ProcessMessages;
      FForm.Visible := False;
      Application.ProcessMessages;
    End;
    If FInForm = 1 Then FInForm := 0 Else FInForm := 3;
  End;  
End; // TSharedComboBox.FormExit

Procedure TSharedComboBox.Loaded;
Var W, H: Integer;
Begin
  Inherited Loaded;
  FIsLoaded := True;
  W         := Width;
  H         := Height;
  AdjustSize (W, H);
  If (W <> Width) Or (H <> Height) Then Inherited SetBounds(Left, Top, W, H);
End; // TSharedComboBox.Loaded

Procedure TSharedComboBox.SetListBoxPos;
Var L,T: Integer;
Begin
  L := 0;      AbsLeft(L,Self);
  T := Height; AbsTop(T,Self);
  Adjust(L,T);
  If T + FListBox.Height + 2 > Screen.Height Then
  Begin
    L := 0;                    AbsLeft(L,Self);
    T := -FListBox.Height - 2; AbsTop(T,Self);
    Adjust(L,T);
  End;
  If T < 0 Then T := 0;
  FForm.SetBounds(L,T,Width,FListBox.Height + 2);
End; // TSharedComboBox.SetListBoxPos

Procedure TSharedComboBox.EditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  If FInForm = 0 Then
  Begin
    If (FStyle = csDropDownList) And Not FForm.Visible Then
    Begin
      SetListBoxPos;
      FForm.Visible := True;
    End;
  End;
  If FInForm = 1 Then Inc(FInForm) Else FInForm := 0;
End; // TSharedComboBox.EditMouseDown

Procedure TSharedComboBox.ButtonMouseDown(Sender: TObject; Button: TMouseButton;
                                          Shift: TShiftState; X, Y: Integer);
Begin
  If FInForm = 0 Then
  Begin
    If FForm.Visible Then FForm.Visible := False
    Else
    Begin
      SetListBoxPos;
      FForm.Visible := True;
    End;
  End;
  If FInForm = 1 Then Inc(FInForm) Else FInForm := 0;
End; // TSharedComboBox.ButtonMouseDown

Procedure TSharedComboBox.DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
Var I,TH: Integer;
Begin
  Inc(Index,FScrollBar.Position);
  TH := FListBox.Canvas.TextHeight('Mg');
  I  := (Rect.Bottom - Rect.Top - TH) Div 2;
  If odSelected In State Then
  Begin
    FListBox.Canvas.Pen.Color   := clHighlight;
    FListBox.Canvas.Brush.Color := clHighlight;
    FListBox.Canvas.Rectangle(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom);
    FListBox.Canvas.Font.Color  := clHighlightText;
    If (FItems <> Nil) And (Index >= 0) And (Index < FItems.Count) Then
    Begin
      If (FClearTypeLink <> Nil) And (FClearTypeLink.ClearType <> Nil)
       Then FClearTypeLink.ClearType.DrawText(Rect.Left + 2,Rect.Top + I,FItems.Strings[Index],0,FListBox.Canvas,FListBox.Canvas.Brush.Color,@Rect,False)
       Else FListBox.Canvas.TextOut(Rect.Left + 2,Rect.Top + I,FItems.Strings[Index]);
    End;
  End
  Else
  Begin
    FListBox.Canvas.Pen.Color   := clWindow;
    FListBox.Canvas.Brush.Color := clWindow;
    FListBox.Canvas.Rectangle(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom);
    FListBox.Canvas.Font.Color  := clWindowText;
    If (FItems <> Nil) And (Index >= 0) And (Index < FItems.Count) Then
    Begin
      If (FClearTypeLink <> Nil) And (FClearTypeLink.ClearType <> Nil)
       Then FClearTypeLink.ClearType.DrawText(Rect.Left + 2,Rect.Top + I,FItems.Strings[Index],0,FListBox.Canvas,FListBox.Canvas.Brush.Color,@Rect,False)
       Else FListBox.Canvas.TextOut(Rect.Left + 2,Rect.Top + I,FItems.Strings[Index]);
    End;
  End;
End; // TSharedComboBox.DrawItem

Procedure TSharedComboBox.ScrollBarChange(Sender: TObject);
Var I,J: Integer;
Begin
  If Not FSBChanging Then
  Begin
    FSBChanging := True;
    J := FScrollBar.Position;
    If (FItems <> Nil) And (J > FItems.Count - FDropDownCount) Then
     J := FItems.Count - FDropDownCount;
    If (J >= FScrollBar.Min) And (J <= FScrollBar.Max) Then FScrollBar.Position := J;
    Application.ProcessMessages;
    I := FItemIndex - J;
    If (I < 0) Or (I >= FListBox.Items.Count) Then I := -1;
    FListBox.ItemIndex := I;
    FListBox.Invalidate;
    FListBox.Repaint;
    FSBChanging := False;
  End;
End; // TSharedComboBox.ScrollBarChange

Procedure TSharedComboBox.SetStyle(S: TComboBoxStyle);
Begin
  Case S Of
        csDropDown: FEdit.ReadOnly := False;
    csDropDownList: Begin
                      FEdit.ReadOnly := True;
                      ItemIndex      := -1;
                    End;
  End; // Case
  FStyle := S;
End; // TSharedComboBox.SetStyle

Procedure TSharedComboBox.SetItemIndex(Index: Integer);
Begin
  FItemIndex := Index;
  If (FItems <> Nil) And (Index >= 0) And (Index < FItems.Count) Then
  Begin
    FSBChanging        := True;
    FEdit.Text         := FItems.Strings[Index];
    Text               := FEdit.Text;
    If (FItemIndex >= FScrollBar.Min) And
       (FItemIndex <= FScrollBar.Max) Then FScrollBar.Position := FItemIndex;
    FListBox.ItemIndex := Index;
    Application.ProcessMessages;
    FSBChanging        := False;
    ScrollBarChange(Self);
    FListBox.Invalidate;
    FListBox.Repaint;
  End
  Else
  Begin
    FEdit.Text  := '';
    Text        := FEdit.Text;
    FSBChanging := True;
    FListBox.ItemIndex := -1;
    FSBChanging := False;
    FListBox.Invalidate;
    FListBox.Repaint;
  End;
End; // TSharedComboBox.SetItemIndex

Procedure TSharedComboBox.EditChange(Sender: TObject);
Begin
  Text := FEdit.Text;
  If Not FEditChanging Then Change;
End; // TSharedComboBox.EditChange

Procedure TSharedComboBox.ListBoxClick(Sender: TObject);
Var B: Boolean;
Begin
  FEditChanging  := True;
  B              := FEdit.ReadOnly;
  FEdit.ReadOnly := False;
  If FListBox.ItemIndex >= 0 Then
  Begin
    FItemIndex := FListBox.ItemIndex + FScrollBar.Position;
    If (FItems <> Nil) And (FItemIndex >= 0) And (FItemIndex < FItems.Count)
     Then FEdit.Text := FItems.Strings[FItemIndex]
     Else FEdit.Text := '';
  End
  Else
  Begin
    FItemIndex := -1;
    FEdit.Text := '';
  End;
  Text := FEdit.Text;
  FormExit(Self);
  (Owner As TForm).ActiveControl := FEdit;
  If Not B Then FEdit.SelectAll;
  FEdit.ReadOnly := B;
  FEditChanging := False;
  Change;
End; // TSharedComboBox.ListBoxClick

Procedure TSharedComboBox.ListBoxKeyPress(Sender: TObject; Var Key: Char);
Begin
  If FListBox.ItemIndex >= 0
   Then FItemIndex := FListBox.ItemIndex + FScrollBar.Position
   Else FItemIndex := -1;
End; // TSharedComboBox.ListBoxClick

Procedure TSharedComboBox.SetEnabled(Value: Boolean); 
Begin
  Inherited;
  FEdit.Enabled      := Value;
  FScrollBar.Enabled := Value;
  FListBox.Enabled   := Value;
  FButton.Enabled    := Value;
  If FForm.Visible Then FormExit(Self);
End; // TSharedComboBox.SetEnabled

Function TSharedComboBox.GetCtl3D: Boolean;
Begin
  Result := FEdit.Ctl3D;
End; // TSharedComboBox.GetCtl3D

Procedure TSharedComboBox.SetCtl3D(B: Boolean);
Begin
  FEdit.Ctl3D := B;
End; // TSharedComboBox.SetCtl3D

Function TSharedComboBox.GetBorderStyle: TBorderStyle;
Begin
  Result := FEdit.BorderStyle;
End; // TSharedComboBox.GetBorderStyle

Procedure TSharedComboBox.SetBorderStyle(B: TBorderStyle);
Begin
  FEdit.BorderStyle := B;
End; // TSharedComboBox.SetBorderStyle

// TDBSharedComboBox

Constructor TDBSharedComboBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FChanging                 := False;
  Style                     := csDropDownList;
  ControlStyle              := ControlStyle + [csReplicatable];
  FDataLink                 := TFieldDataLink.Create;
  FDataLink.Control         := Self;
  FDataLink.OnDataChange    := DataChange;
  FDataLink.OnUpdateData    := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
  FOnGetValueFromIndex      := Nil;
  FOnGetIndexFromValue      := Nil;
End; // TDBSharedComboBox.Create

Destructor TDBSharedComboBox.Destroy;
Begin
  FDataLink.Free;
  FDataLink := Nil;
  Inherited Destroy;
End; // TDBSharedComboBox.Destroy

Procedure TDBSharedComboBox.DataChange(Sender: TObject);
Var I: Integer;
begin
  If Not FChanging Then
  Begin
    If FDataLink.Field <> Nil Then
    Begin
      I := FDataLink.Field.AsInteger;
      If Assigned(FOnGetIndexFromValue) Then FOnGetIndexFromValue(Self,I);
      SetComboItem(I);
    End;
  End;
End; // TDBSharedComboBox.DataChange

Procedure TDBSharedComboBox.EditingChange(Sender: TObject);
Begin
End; // TDBSharedComboBox.EditingChange

Procedure TDBSharedComboBox.UpdateData(Sender: TObject);
Var I: Integer;
Begin
  If FDataLink.Field <> Nil Then
  Begin
    I := GetComboItem;
    If Assigned(FOnGetValueFromIndex) Then FOnGetValueFromIndex(Self,I);
    FDataLink.Field.AsInteger := I;
  End;
End; // TDBSharedComboBox.UpdateData

Function TDBSharedComboBox.GetDataSource: TDataSource;
Begin
  Result := FDataLink.DataSource;
End; // TDBSharedComboBox.GetDataSource

Procedure TDBSharedComboBox.SetDataSource(Value: TDataSource);
Begin
  If Not (FDataLink.DataSourceFixed And (csLoading In ComponentState)) Then
    FDataLink.DataSource := Value;
  If Value <> Nil Then Value.FreeNotification(Self);
End; // TDBSharedComboBox.SetDataSource

Procedure TDBSharedComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  Inherited Notification(AComponent, Operation);
  If (Operation = opRemove) And (FDataLink <> Nil) And
    (AComponent = DataSource) Then DataSource := Nil;
end; // TDBSharedComboBox.Notification

Function TDBSharedComboBox.GetDataField: String;
Begin
  Result := FDataLink.FieldName;
End; // TDBSharedComboBox.GetDataField

Procedure TDBSharedComboBox.SetDataField(Const Value: String);
Begin
  FDataLink.FieldName := Value;
End; // TDBSharedComboBox.SetDataField

Function TDBSharedComboBox.GetReadOnly: Boolean;
Begin
  Result := FDataLink.ReadOnly;
End; // TDBSharedComboBox.GetReadOnly

Procedure TDBSharedComboBox.SetReadOnly(Value: Boolean);
Begin
  FDataLink.ReadOnly := Value;
End; // TDBSharedComboBox.SetReadOnly

Function TDBSharedComboBox.GetField: TField;
Begin
  Result := FDataLink.Field;
End; // TDBSharedComboBox.GetField

Procedure TDBSharedComboBox.SetComboItem(Const Value: Integer);
Var
  I      : Integer;
  Redraw : Boolean;

Begin
  If Items <> Nil Then
  Begin
    Redraw := (Style <> csSimple) And HandleAllocated;
    If Redraw Then SendMessage(Handle, WM_SETREDRAW, 0, 0);
    Try
      If (Value >= 0) And (Value < Items.Count) Then I := Value Else I := -1;
      If ItemIndex <> I Then ItemIndex := I;
    Finally
      If Redraw Then
      Begin
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
        Invalidate;
      End;
    End;
  End;
End; // TDBSharedComboBox.SetComboItem

Function TDBSharedComboBox.GetComboItem: Integer;
Begin
  Result := ItemIndex;
End; // TDBSharedComboBox.GetComboItem

Procedure TDBSharedComboBox.Change;
Begin
  If Not FChanging Then
  Begin
    FChanging := True;
    FDataLink.Edit;
    Inherited Change;
    FDataLink.Modified;
    UpdateData(Self);
    FChanging := False;
  End;
End; // TDBSharedComboBox.Change

Procedure TDBSharedComboBox.SetItems(List: TSharedComboBoxStrings);
Begin
  Inherited SetItems(List);
  DataChange(Self);
End; // TDBSharedComboBox.SetItems

Procedure TDBSharedComboBox.CMExit(Var Message: TCMExit);
Begin
  Try
    FDataLink.UpdateRecord;
  Except
    FEdit.SelectAll;
    SetFocus;
    Raise;
  End;
  Inherited;
End; // TDBSharedComboBox.CMExit

Procedure TDBSharedCombobox.CMGetDatalink(Var Message: TMessage);
Begin
  Message.Result := Integer(FDataLink);
End; // TDBSharedCombobox.CMGetDatalink

Function TDBSharedComboBox.ExecuteAction(Action: TBasicAction): Boolean;
Begin
  Result := Inherited ExecuteAction(Action) Or (FDataLink <> Nil) And
    FDataLink.ExecuteAction(Action);
End; // TDBSharedComboBox.ExecuteAction

Function TDBSharedComboBox.UpdateAction(Action: TBasicAction): Boolean;
Begin
  Result := Inherited UpdateAction(Action) Or (FDataLink <> Nil) And
    FDataLink.UpdateAction(Action);
End; // TDBSharedComboBox.UpdateAction

Procedure Register;
Begin
  RegisterComponents('Samples', [TSharedComboBox,TDBSharedComboBox]);
End;

end.
