Unit OgreEventQueue;

Interface

Uses Classes,SyncObjs,OgreInputEvent;

Type
  EventQueue = Class
  Protected
    List                : TStringList;
    Mutex               : TCriticalSection;
    mActivateEventQueue : Boolean;
  Public
    Constructor Create;
    Destructor  Destroy; Override;
    Procedure   Push(Event: InputEvent);
    Function    Pop: InputEvent;
    Procedure   ActivateEventQueue(Activate: Boolean);
    Function    GetSize: Integer;
  End;

Implementation

Constructor EventQueue.Create;
Begin
  List                := TStringList.Create;
  Mutex               := TCriticalSection.Create;
  mActivateEventQueue := False;
End; // EventQueue.Create

Destructor EventQueue.Destroy;
Var I: Integer;
Begin
  For I := 0 To List.Count - 1 Do List.Objects[I].Free;
  List.Free;
  Mutex.Free;
End; // EventQueue.Destroy

Procedure EventQueue.Push(Event: InputEvent);
Begin
  Try
    Mutex.Enter;
    If mActivateEventQueue Then List.AddObject('',Event);
  Finally
    Mutex.Leave;
  End;
End; // EventQueue.Push

Function EventQueue.Pop: InputEvent;
Begin
  Try
    Mutex.Enter;
    If mActivateEventQueue And (List.Count > 0) Then
    Begin
      Result := InputEvent(List.Objects[0]);
      List.Delete(0);
    End
    Else Result := Nil;
  Finally
    Mutex.Leave;
  End;
End; // EventQueue.Pop

Procedure EventQueue.ActivateEventQueue(Activate: Boolean);
Begin
  Try
    Mutex.Enter;
    mActivateEventQueue := Activate;
  Finally
    Mutex.Leave;
  End;
End; // EventQueue.ActivateEventQueue

Function EventQueue.GetSize: Integer;
Begin
  Try
    Mutex.Enter;
    Result := List.Count;
  Finally
    Mutex.Leave;
  End;
End; // EventQueue.GetSize

End.
