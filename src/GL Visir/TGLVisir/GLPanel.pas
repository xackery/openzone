Unit GLPanel;

Interface

Uses ExtCtrls,Classes,Forms,Messages,SysUtils;

Type
  TBooleanQueryEvent = Function(Sender: TObject): Boolean Of Object;

  TGLForm = Class(TForm)
  Protected
    FOnAllInitialized : TBooleanQueryEvent;
    FOnMPTDone        : TBooleanQueryEvent;
    Procedure   WndProc(Var Message: TMessage); Override;
    Function    AllInitialized: Boolean;
  Public
    GLPanels : TStringList;
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
    Procedure   DoShow; Override;
    Function    CloseQuery: Boolean; Override;
    Property    OnAllInitialized : TBooleanQueryEvent Read FOnAllInitialized Write FOnAllInitialized;
    Property    OnMPTDone        : TBooleanQueryEvent Read FOnMPTDone        Write FOnMPTDone;
  End;

  TGLVisirPanel = Class(TPanel)
  Protected
    FOnUpdateArea : TNotifyEvent;
    FOnInit           : TNotifyEvent;
    FOnAllInitialized : TBooleanQueryEvent;
    Procedure   AdjustSize; Override;
    Function    AllInitialized: Boolean;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
    Procedure   Init;
    Property    OnUpdateArea     : TNotifyEvent Read FOnUpdateArea Write FOnUpdateArea;
    Property    OnInit           : TNotifyEvent       Read FOnInit           Write FOnInit;
    Property    OnAllInitialized : TBooleanQueryEvent Read FOnAllInitialized Write FOnAllInitialized;
  End;

Procedure Register;

Implementation

Uses Windows,Dialogs;

Function GetUltimateOwner(Component: TComponent): TComponent;
Var Owner: TComponent;
Begin
  Owner := Component;
  While (Owner.Owner <> Nil) And Not (Owner.Owner Is TApplication) Do Owner := Owner.Owner;
  Result := Owner;
End; // GetUltimateOwner

// ---------------------------
// TGLForm
// ---------------------------

Constructor TGLForm.Create(AOwner: TComponent);
Begin
  GLPanels          := TStringList.Create;
  FOnAllInitialized := Nil;

  FOnMPTDone        := Nil;
  Inherited Create(AOwner);
End; // TGLForm.Create

Destructor TGLForm.Destroy;
Var I: Integer;
Begin
  For I := 0 To GLPanels.Count - 1 Do TGLVisirPanel(GLPanels.Objects[I]).OnUpdateArea := Nil;
  GLPanels.Free;
  GLPanels := Nil;
  Inherited;
End; // TGLForm.Destroy

Function TGLForm.AllInitialized: Boolean;
Begin
  If Assigned(FOnAllInitialized)
   Then Result := FOnAllInitialized(Self)
   Else Result := True;
End; // TGLForm.AllInitialized

//PROFILE-NO
Procedure TGLForm.WndProc(Var Message: TMessage);
Type PWMSysCommand = ^TWMSysCommand;
Begin
  Case Message.Msg Of
    WM_SHOWWINDOW:
    Begin
      If (Message.WParam <> 0) Or AllInitialized Then Inherited WndProc(Message);
    End;
    WM_WINDOWPOSCHANGING:
    Begin
      If Not AllInitialized Then
      Begin
        With PWindowPos(Message.lParam)^ Do Flags := Flags Or SWP_NOMOVE Or SWP_NOSIZE;
      End;
      Inherited WndProc(Message);
    End;
  Else
    Inherited WndProc(Message);
  End; // Case
End; // TGLForm.WndProc
//PROFILE-YES

Procedure TGLForm.DoShow;
Var
  I     : Integer;
  Panel : TGLVisirPanel;

Begin
  If GLPanels <> Nil Then
  Begin
    For I := 0 To GLPanels.Count - 1 Do
    Begin
      Panel := TGLVisirPanel(GLPanels.Objects[I]);
      If Not (csDesigning In ComponentState) //Then TGLVisir(Panel.Owner).Init(True);
         Then Panel.Init;
    End; // For I
  End;
  Inherited;
End; // TGLForm.DoShow

Function TGLForm.CloseQuery: Boolean;
Var
  B     : Boolean;
  I     : Integer;
  Panel : TGLVisirPanel;

Begin
  If GLPanels <> Nil Then
  Begin
    B := False;
    For I := 0 To GLPanels.Count - 1 Do
    Begin
      Panel := TGLVisirPanel(GLPanels.Objects[I]);
      If Assigned(FOnMPTDone)
       Then B := B Or FOnMPTDone(Self)
       Else B := True;
//      B := B Or (TGLVisir(Panel.Owner).MPTDone = 2);
    End; // For I
  End
  Else B := True;
  If B Then
  Begin
    If Assigned(OnCloseQuery) Then OnCloseQuery(Self, B);
  End
  Else ShowMessage('Cannot close while the OpenGL panels are still initializing');
  Result := B;
End; // TGLForm.CloseQuery

// ---------------------------
// TGLVisirPanel
// ---------------------------

Constructor TGLVisirPanel.Create(AOwner: TComponent);
Var TheOwner: TComponent;
Begin
  FOnUpdateArea     := Nil;
  FOnInit           := Nil;
  FOnAllInitialized := Nil;
  Inherited Create(AOwner);
  TheOwner := GetUltimateOwner(Self);
  If TheOwner Is TGLForm Then
   TGLForm(TheOwner).GLPanels.AddObject('',Self);
End; // TGLVisirPanel.Create

Destructor TGLVisirPanel.Destroy;
Begin
  Inherited;
End; // TGLVisirPanel.Destroy

Function TGLVisirPanel.AllInitialized: Boolean;
Begin
  If Assigned(FOnAllInitialized)
   Then Result := FOnAllInitialized(Self)
   Else Result := True;
End; // TGLVisirPanel.AllInitialized

Procedure TGLVisirPanel.AdjustSize;
Begin
  Inherited AdjustSize;
  If AllInitialized And Not (csDesigning In Owner.ComponentState) Then
  Begin
    If Assigned(FOnUpdateArea) Then FOnUpdateArea(Self);
  End;
End; // TGLVisirPanel.AdjustSize

Procedure TGLVisirPanel.Init;
Begin
  If Assigned(FOnInit) Then FOnInit(Self);
End; // TGLVisirPanel.Init

Procedure Register;
Begin
  RegisterComponents('Samples', [TGLForm,TGLVisirPanel]);
End; // Register

End.
