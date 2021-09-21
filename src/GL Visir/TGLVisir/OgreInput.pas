Unit OgreInput;
{
-----------------------------------------------------------------------------
This source file is part of OGRE
    (Object-oriented Graphics Rendering Engine)
For the latest info, see http://www.stevestreeting.com/ogre/

Copyright © 2000-2001 Steven J. Streeting
Also see acknowledgements in Readme.html

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place - Suite 330, Boston, MA 02111-1307, USA, or go to
http://www.gnu.org/copyleft/gpl.html.
-----------------------------------------------------------------------------
}

Interface

Uses Windows,OgreEventQueue;

Const
  // Keyboard scan codes - copied from DirectInput for now for speed.
  KC_ESCAPE          = $01;
  KC_1               = $02;
  KC_2               = $03;
  KC_3               = $04;
  KC_4               = $05;
  KC_5               = $06;
  KC_6               = $07;
  KC_7               = $08;
  KC_8               = $09;
  KC_9               = $0A;
  KC_0               = $0B;
  KC_MINUS           = $0C;    { - on main keyboard }
  KC_EQUALS          = $0D;
  KC_BACK            = $0E;    { backspace }
  KC_TAB             = $0F;
  KC_Q               = $10;
  KC_W               = $11;
  KC_E               = $12;
  KC_R               = $13;
  KC_T               = $14;
  KC_Y               = $15;
  KC_U               = $16;
  KC_I               = $17;
  KC_O               = $18;
  KC_P               = $19;
  KC_LBRACKET        = $1A;
  KC_RBRACKET        = $1B;
  KC_RETURN          = $1C;    { Enter on main keyboard }
  KC_LCONTROL        = $1D;
  KC_A               = $1E;
  KC_S               = $1F;
  KC_D               = $20;
  KC_F               = $21;
  KC_G               = $22;
  KC_H               = $23;
  KC_J               = $24;
  KC_K               = $25;
  KC_L               = $26;
  KC_SEMICOLON       = $27;
  KC_APOSTROPHE      = $28;
  KC_GRAVE           = $29;    { accent grave }
  KC_LSHIFT          = $2A;
  KC_BACKSLASH       = $2B;
  KC_Z               = $2C;
  KC_X               = $2D;
  KC_C               = $2E;
  KC_V               = $2F;
  KC_B               = $30;
  KC_N               = $31;
  KC_M               = $32;
  KC_COMMA           = $33;
  KC_PERIOD          = $34;    { . on main keyboard }
  KC_SLASH           = $35;    { '/' on main keyboard }
  KC_RSHIFT          = $36;
  KC_MULTIPLY        = $37;    { * on numeric keypad }
  KC_LMENU           = $38;    { left Alt }
  KC_SPACE           = $39;
  KC_CAPITAL         = $3A;
  KC_F1              = $3B;
  KC_F2              = $3C;
  KC_F3              = $3D;
  KC_F4              = $3E;
  KC_F5              = $3F;
  KC_F6              = $40;
  KC_F7              = $41;
  KC_F8              = $42;
  KC_F9              = $43;
  KC_F10             = $44;
  KC_NUMLOCK         = $45;
  KC_SCROLL          = $46;    { Scroll Lock }
  KC_NUMPAD7         = $47;
  KC_NUMPAD8         = $48;
  KC_NUMPAD9         = $49;
  KC_SUBTRACT        = $4A;    { - on numeric keypad }
  KC_NUMPAD4         = $4B;
  KC_NUMPAD5         = $4C;
  KC_NUMPAD6         = $4D;
  KC_ADD             = $4E;    { + on numeric keypad }
  KC_NUMPAD1         = $4F;
  KC_NUMPAD2         = $50;
  KC_NUMPAD3         = $51;
  KC_NUMPAD0         = $52;
  KC_DECIMAL         = $53;    { . on numeric keypad }
  KC_OEM_102         = $56;    { < > | on UK/Germany keyboards }
  KC_F11             = $57;
  KC_F12             = $58;
  KC_F13             = $64;    {                     (NEC PC98) }
  KC_F14             = $65;    {                     (NEC PC98) }
  KC_F15             = $66;    {                     (NEC PC98) }
  KC_KANA            = $70;    { (Japanese keyboard)            }
  KC_ABNT_C1         = $73;    { / ? on Portugese (Brazilian) keyboards }
  KC_CONVERT         = $79;    { (Japanese keyboard)            }
  KC_NOCONVERT       = $7B;    { (Japanese keyboard)            }
  KC_YEN             = $7D;    { (Japanese keyboard)            }
  KC_ABNT_C2         = $7E;    { Numpad . on Portugese (Brazilian) keyboards }
  KC_NUMPADEQUALS    = $8D;    { = on numeric keypad (NEC PC98) }
  KC_PREVTRACK       = $90;    { Previous Track (KC_CIRCUMFLEX on Japanese keyboard) }
  KC_AT              = $91;    {                     (NEC PC98) }
  KC_COLON           = $92;    {                     (NEC PC98) }
  KC_UNDERLINE       = $93;    {                     (NEC PC98) }
  KC_KANJI           = $94;    { (Japanese keyboard)            }
  KC_STOP            = $95;    {                     (NEC PC98) }
  KC_AX              = $96;    {                     (Japan AX) }
  KC_UNLABELED       = $97;    {                        (J3100) }
  KC_NEXTTRACK       = $99;    { Next Track }
  KC_NUMPADENTER     = $9C;    { Enter on numeric keypad }
  KC_RCONTROL        = $9D;
  KC_MUTE            = $A0;    { Mute }
  KC_CALCULATOR      = $A1;    { Calculator }
  KC_PLAYPAUSE       = $A2;    { Play / Pause }
  KC_MEDIASTOP       = $A4;    { Media Stop }
  KC_VOLUMEDOWN      = $AE;    { Volume - }
  KC_VOLUMEUP        = $B0;    { Volume + }
  KC_WEBHOME         = $B2;    { Web home }
  KC_NUMPADCOMMA     = $B3;    { , on numeric keypad (NEC PC98) }
  KC_DIVIDE          = $B5;    { / on numeric keypad }
  KC_SYSRQ           = $B7;
  KC_RMENU           = $B8;    { right Alt }
  KC_PAUSE           = $C5;    { Pause }
  KC_HOME            = $C7;    { Home on arrow keypad }
  KC_UP              = $C8;    { UpArrow on arrow keypad }
  KC_PGUP            = $C9;    { PgUp on arrow keypad }
  KC_LEFT            = $CB;    { LeftArrow on arrow keypad }
  KC_RIGHT           = $CD;    { RightArrow on arrow keypad }
  KC_END             = $CF;    { End on arrow keypad }
  KC_DOWN            = $D0;    { DownArrow on arrow keypad }
  KC_PGDOWN          = $D1;    { PgDn on arrow keypad }
  KC_INSERT          = $D2;    { Insert on arrow keypad }
  KC_DELETE          = $D3;    { Delete on arrow keypad }
  KC_LWIN            = $DB;    { Left Windows key }
  KC_RWIN            = $DC;    { Right Windows key }
  KC_APPS            = $DD;    { AppMenu key }
  KC_POWER           = $DE;    { System Power }
  KC_SLEEP           = $DF;    { System Sleep }
  KC_WAKE            = $E3;    { System Wake }
  KC_WEBSEARCH       = $E5;    { Web Search }
  KC_WEBFAVORITES    = $E6;    { Web Favorites }
  KC_WEBREFRESH      = $E7;    { Web Refresh }
  KC_WEBSTOP         = $E8;    { Web Stop }
  KC_WEBFORWARD      = $E9;    { Web Forward }
  KC_WEBBACK         = $EA;    { Web Back }
  KC_MYCOMPUTER      = $EB;    { My Computer }
  KC_MAIL            = $EC;    { Mail }
  KC_MEDIASELECT     = $ED;    { Media Select }

Type
  KeyCode = Integer;

  (** Structure representing a snapshot of the state of the mouse
      input controller. *)
  MouseState = Record
    (** Absolute position of the mouse pointer. *)
    Xabs, Yabs, Zabs: Integer;

    (** Relative position of the mouse pointer. *)
    Xrel, Yrel, Zrel: Integer;

    {** The buttons that have been pressed. Each bit maps to a mouse button. *}
    Buttons: Integer;
  End;

  BufferedKeysDownSet = Set Of Byte;

  {* Abstract class which allows input to be read from various
      controllers.
      @remarks
          You can access an appropriate concrete subclass of this interface by
          calling PlatformManager::createInputReader.
      @warning Temporary implementation only. This class is likely to be
          refactored into a better design when I get time to look at it
          properly. For now it's a quick-and-dirty way to get what I need.
      @see
          PlatformManager::createInputReader
  }
  InputReader = Class
  Protected

    (** The modifiers are a binary flags that represent what buttons are pressed,
        and what key modifiers are down (e.g. shift/alt). *)
    mModifiers: Integer;

    (** Internal Cursor object.
        @remarks
        This is a mathematical representation of where the cursor is, it does
        not draw a cursor.
        @see CursorGuiElement. *)

//    Cursor* mCursor;

    (** EventQueue is used for buffered input support. *)
    mEventQueue: EventQueue;

    (** Wether to use buffering input support - buffering support relies on using
    an EventQueue.
    @see class EventQueue *)
    mUseBufferedKeys, mUseBufferedMouse: Boolean;

    (** The mouse state in immediate mode. *)
//    MouseState mMouseState;

    /// Set of all the keys currently depressed based on buffered input events

    mBufferedKeysDown: BufferedKeysDownSet;

    (** Creates mouse moved or dragged events depending if any button is pressed. *)
//    Procedure MouseMoved;

    (** Creates a MouseEvent that first gets processed by the cursor, then gets
    pushed on the queue. *)
//    Procedure CreateMouseEvent(ID, Button: Integer);

    (** Creates mouse pressed, released, and clicked events. *)
//    Procedure TriggerMouseButton(nMouseCode: Integer; MousePressed: Boolean);

    Procedure CreateKeyEvent(ID, Key: Integer);
    Procedure KeyChanged(Key: Integer; Down: Boolean);

    (** Return whether a key is down in immediate mode. *)
    Function IsKeyDownImmediate(KC: KeyCode): Boolean; Dynamic; Abstract;

    (** Creates mouse moved or dragged events depending if any button is pressed. *)
    Procedure MouseMoved;

    (** Creates a MouseEvent that first gets processed by the cursor, then gets
        pushed on the queue. *)
    Procedure CreateMouseEvent(ID, Button: Integer);

    (** Creates mouse pressed, released, and clicked events. *)
    Procedure TriggerMouseButton(nMouseCode: Integer; MousePressed: Boolean);
  Public
    Constructor Create;

    {* Initialise the input system.
        @note
            Only keyboard and mouse currently implemented.
        @param
            pWindow The window to capture input for
        @param
            useKeyboard If true, keyboard input will be supported.
        @param
            useMouse If true, mouse input will be supported.
        @param
            useGameController If true, joysticks/gamepads will be supported.
    }
    Procedure Initialise(WindowHandle: HWND; UseKeyboard: Boolean = True; UseMouse: Boolean = True;
                         UseGameController: Boolean = False); Dynamic; Abstract;

    {* Captures the state of all the input devices.
        @remarks
            This method captures the state of all input devices and
            stores it internally for use when the enquiry methods are
            next called. This is done to ensure that all input is
            captured at once and therefore combinations of input are not
            subject to time differences when methods are called.

    }
    Procedure   Capture; Dynamic; Abstract;

    {* Determines if the specified key is currently depressed.
        @note This enquiry method uses the state of the keyboard at the
            last 'capture' call.
    }
    Function    IsKeyDown(KC: KeyCode): Boolean; Dynamic;

    {** Retrieves the relative position of the mouse when capture was
        called relative to the last time. *}
    Function    GetMouseRelativeX: Integer; Dynamic;

    {** Retrieves the relative position of the mouse when capture was
        called relative to the last time. *}
    Function    GetMouseRelativeY: Integer; Dynamic;

    {** Retrieves the relative position of the mouse when capture was
        called relative to the last time. *}
    Function    GetMouseRelativeZ: Integer; Dynamic;

    {** Retrieves the relative (compared to the last input poll) mouse movement
        on the X (horizontal) axis. *}
    Function    GetMouseRelX: Integer; Dynamic; Abstract;

    {** Retrieves the relative (compared to the last input poll) mouse movement
        on the Y (vertical) axis. *}
    Function    GetMouseRelY: Integer; Dynamic; Abstract;

    {** Retrieves the relative (compared to the last input poll) mouse movement
        on the Z (mouse wheel) axis. *}
    Function    GetMouseRelZ: Integer; Dynamic; Abstract;

    {** Retrieves the absolute mouse position on the X (horizontal) axis. *}
    Function    GetMouseAbsX: Integer; Dynamic; Abstract;
    {** Retrieves the absolute mouse position on the Y (vertical) axis. *}
    Function    GetMouseAbsY: Integer; Dynamic; Abstract;
    {** Retrieves the absolute mouse position on the Z (mouse wheel) axis. *}
    Function    GetMouseAbsZ: Integer; Dynamic; Abstract;

    {* Tells the reader to use buffered input and update the passed in queue.
        @remarks
            The default behaviour of the input reader is simply to capture the
            current state of the mouse / keyboard on demand. An alternative is to use
            buffered input where all events are registered on a queue.
    }
    Procedure UseBufferedInput(pEventQueue: EventQueue; Keys: Boolean = True; Mouse: Boolean = True);

    Procedure SetBufferedInput(Keys,Mouse: Boolean); Dynamic;
  End;

  {* Defines the interface a platform-specific library must implement.
      @remarks
          Any library (.dll, .so) wishing to implement a
          platform-specific version of this input reader must export the
          symbol 'createInputReader' with the signature void
          createPlatformInputReader(InputReader** ppReader).
  }
  DLL_CREATEINPUTREADER = Procedure(Var ppReader: InputReader);

  {** Retrieves the pressed state of a mouse button. *}
  Function IsButtonDown(Const MS: MouseState; Button: Byte): Integer;
  Function GetKeyChar(KeyCode, Modifiers: Integer): Char;

Implementation

Uses OgreKeyEvent,OgreInputEvent;

Function IsButtonDown(Const MS: MouseState; Button: Byte): Integer;
Begin
  Result := MS.Buttons And (1 Shl Button);
End; // IsButtonDown

Constructor InputReader.Create;
Begin
//  mCursor = 0;
  mModifiers        := 0;
  mEventQueue       := Nil;
  mUseBufferedKeys  := False;
  mUseBufferedMouse := False;
End; // InputReader.Create

Procedure InputReader.CreateKeyEvent(ID, Key: Integer);
Var KE: KeyEvent;
Begin
  KE := KeyEvent.Create({Nil, }ID, Key, 0, // hack fix time
          mModifiers);  // hack fix click count
  If mEventQueue <> Nil Then mEventQueue.Push(KE);
End; // InputReader.CreateKeyEvent

Procedure InputReader.KeyChanged(Key: Integer; Down: Boolean);
Begin
  If Down Then
  Begin
    Case Key Of
      KC_LMENU,
      KC_RMENU:    mModifiers := mModifiers Or ALT_MASK;

      KC_LSHIFT,
      KC_RSHIFT:   mModifiers := mModifiers Or SHIFT_MASK;

      KC_LCONTROL,
      KC_RCONTROL: mModifiers := mModifiers Or CTRL_MASK;
    End; // Case

    CreateKeyEvent(KE_KEY_PRESSED, Key);

    // Update keydown map
    mBufferedKeysDown := mBufferedKeysDown + [Key];
  End
  Else
  Begin
    Case Key Of
      KC_LMENU,
      KC_RMENU:    mModifiers := mModifiers And (Not ALT_MASK);

      KC_LSHIFT,
      KC_RSHIFT:   mModifiers := mModifiers And (Not SHIFT_MASK);

      KC_LCONTROL,
      KC_RCONTROL: mModifiers := mModifiers And (Not CTRL_MASK);
    End; // Case

    CreateKeyEvent(KE_KEY_RELEASED, Key);
    CreateKeyEvent(KE_KEY_CLICKED, Key);

    // Update keydown map
    mBufferedKeysDown := mBufferedKeysDown - [Key];
  End;
End; // InputReader.KeyChanged

Procedure InputReader.UseBufferedInput(pEventQueue: EventQueue; Keys,Mouse: Boolean);
Begin
  mEventQueue := pEventQueue;

//  if (mCursor) delete mCursor;

//  mCursor = new Cursor();

  // initial states of buffered don't call setBufferedInput
  // because that can be overriden (in the future) to save releasing and acquiring unchanged inputs
  // if we ever decide to release and acquire devices

  mUseBufferedKeys  := Keys;
  mUseBufferedMouse := Mouse;
End; // InputReader.UseBufferedInput

Procedure InputReader.SetBufferedInput(Keys,Mouse: Boolean);
Begin
  mUseBufferedKeys  := Keys;
  mUseBufferedMouse := Mouse;
End; // InputReader.SetBufferedInput

Function InputReader.IsKeyDown(KC: KeyCode): Boolean;
Begin
  If mUseBufferedKeys
   Then Result := KC In mBufferedKeysDown
   Else Result := isKeyDownImmediate(KC);
End; // InputReader.IsKeyDown

Function InputReader.GetMouseRelativeX: Integer;
Begin
  Result := GetMouseRelX;
End; // InputReader.GetMouseRelativeX

Function InputReader.GetMouseRelativeY: Integer;
Begin
  Result := GetMouseRelY;
End; // InputReader.GetMouseRelativeY

Function InputReader.GetMouseRelativeZ: Integer;
Begin
  Result := GetMouseRelZ;
End; // InputReader.GetMouseRelativeZ

Procedure InputReader.MouseMoved;
Begin
{
  If (mModifiers And BUTTON_ANY_MASK) <> 0 // don't need to know which button. you can get that from the modifiers
   Then CreateMouseEvent(ME_MOUSE_DRAGGED, 0)
   Else CreateMouseEvent(ME_MOUSE_MOVED, 0);
}
End; // InputReader.MouseMoved

Procedure InputReader.TriggerMouseButton(nMouseCode: Integer; MousePressed: Boolean);
Begin
{
  If MousePressed Then
  Begin
    mModifiers := mModifiers Or nMouseCode;
    CreateMouseEvent(ME_MOUSE_PRESSED, nMouseCode);

    // Update immediate-mode mouse button state
    Case nMouseCode Of
      BUTTON0_MASK: mMouseState.Buttons := mMouseState.Buttons Or $1;
      BUTTON1_MASK: mMouseState.Buttons := mMouseState.Buttons Or $2;
      BUTTON2_MASK: mMouseState.Buttons := mMouseState.Buttons Or $4;
    End; // Case
  End
  Else
  Begin // button up... trigger MouseReleased, and MouseClicked
    mModifiers := mModifiers And (Not nMouseCode);
    CreateMouseEvent(ME_MOUSE_RELEASED, nMouseCode);
    //CreateMouseEvent(ME_MOUSE_CLICKED, nMouseCode);  // JCA - moved to EventDispatcher

    // Update immediate-mode mouse button state
    Case nMouseCode Of
      BUTTON0_MASK: mMouseState.Buttons := mMouseState.Buttons And $FE;
      BUTTON1_MASK: mMouseState.Buttons := mMouseState.Buttons And $FD;
      BUTTON2_MASK: mMouseState.Buttons := mMouseState.Buttons And $FB;
    End; // Case
  End;
}
End; // InputReader.TriggerMouseButton

Procedure InputReader.CreateMouseEvent(ID, Button: Integer);
Begin
{
        MouseEvent* me =
    new MouseEvent(
        NULL, id, button, 0, // hack fix time
                    mModifiers,
        mCursor->getX(), mCursor->getY(), mCursor->getZ(),
        mCursor->getRelX(), mCursor->getRelY(), mCursor->getRelZ(),
        0
    );	// hack fix click count


  mCursor->processEvent(me);
        mEventQueue->push(me);
}        
End; //

Function GetKeyChar(KeyCode, Modifiers: Integer): Char;
Begin
  Result := #0;
  If Modifiers = 0 Then
  Begin
    Case KeyCode Of
      KC_1: Result := '1';
      KC_2: Result := '2';
      KC_3: Result := '3';
      KC_4: Result := '4';
      KC_5: Result := '5';
      KC_6: Result := '6';
      KC_7: Result := '7';
      KC_8: Result := '8';
      KC_9: Result := '9';
      KC_0: Result := '0';
      KC_MINUS: Result := '-';      (* - on main keyboard *)
      KC_EQUALS: Result := '=';
      KC_Q: Result := 'q';
      KC_W: Result := 'w';
      KC_E: Result := 'e';
      KC_R: Result := 'r';
      KC_T: Result := 't';
      KC_Y: Result := 'y';
      KC_U: Result := 'u';
      KC_I: Result := 'i';
      KC_O: Result := 'o';
      KC_P: Result := 'p';
      KC_LBRACKET: Result := '[';
      KC_RBRACKET: Result := ']';
      KC_A: Result := 'a';
      KC_S: Result := 's';
      KC_D: Result := 'd';
      KC_F: Result := 'f';
      KC_G: Result := 'g';
      KC_H: Result := 'h';
      KC_J: Result := 'j';
      KC_K: Result := 'k';
      KC_L: Result := 'l';
      KC_SEMICOLON: Result := ';';
      KC_APOSTROPHE: Result := '''';
      KC_GRAVE: Result := '`';      (* accent grave *)
      KC_BACKSLASH: Result := '\';
      KC_Z: Result := 'z';
      KC_X: Result := 'x';
      KC_C: Result := 'c';
      KC_V: Result := 'v';
      KC_B: Result := 'b';
      KC_N: Result := 'n';
      KC_M: Result := 'm';
      KC_COMMA: Result := ',';
      KC_PERIOD: Result := '.';      (* . on main keyboard *)
      KC_SLASH: Result := '/';      (* '/' on main keyboard *)
      KC_MULTIPLY: Result := '*';    (* * on numeric keypad *)
      KC_SPACE: Result := ' ';
      KC_NUMPAD7: Result := '7';
      KC_NUMPAD8: Result := '8';
      KC_NUMPAD9: Result := '9';
      KC_SUBTRACT: Result := '-';    (* - on numeric keypad *)
      KC_NUMPAD4: Result := '4';
      KC_NUMPAD5: Result := '5';
      KC_NUMPAD6: Result := '6';
      KC_ADD: Result := '+';      (* + on numeric keypad *)
      KC_NUMPAD1: Result := '1';
      KC_NUMPAD2: Result := '2';
      KC_NUMPAD3: Result := '3';
      KC_NUMPAD0: Result := '0';
      KC_DECIMAL: Result := '.';    (* . on numeric keypad *)
      KC_NUMPADEQUALS: Result := '=';  (* = on numeric keypad (NEC PC98) *)
      KC_AT: Result := '@';        (*                     (NEC PC98) *)
      KC_COLON: Result := ':';      (*                     (NEC PC98) *)
      KC_UNDERLINE: Result := '_';    (*                     (NEC PC98) *)
      KC_NUMPADCOMMA: Result := ',';  (* , on numeric keypad (NEC PC98) *)
      KC_DIVIDE: Result := '/';      (* / on numeric keypad *)
    End; // Case
  End
  Else If Modifiers = SHIFT_MASK Then
  Begin
    Case KeyCode Of
      KC_1: Result := '!';
      KC_2: Result := '@';
      KC_3: Result := '#';
      KC_4: Result := '$';
      KC_5: Result := '%';
      KC_6: Result := '^';
      KC_7: Result := '&';
      KC_8: Result := '*';
      KC_9: Result := '(';
      KC_0: Result := ')';
      KC_MINUS: Result := '_';
      KC_EQUALS: Result := '+';
      KC_Q: Result := 'Q';
      KC_W: Result := 'W';
      KC_E: Result := 'E';
      KC_R: Result := 'R';
      KC_T: Result := 'T';
      KC_Y: Result := 'Y';
      KC_U: Result := 'U';
      KC_I: Result := 'I';
      KC_O: Result := 'O';
      KC_P: Result := 'P';
      KC_LBRACKET: Result := '{';
      KC_RBRACKET: Result := '}';
      KC_A: Result := 'A';
      KC_S: Result := 'S';
      KC_D: Result := 'D';
      KC_F: Result := 'F';
      KC_G: Result := 'G';
      KC_H: Result := 'H';
      KC_J: Result := 'J';
      KC_K: Result := 'K';
      KC_L: Result := 'L';
      KC_SEMICOLON: Result := ':';
      KC_APOSTROPHE: Result := '"';
      KC_GRAVE: Result := '~';      (* accent grave *)
      KC_BACKSLASH: Result := '|';
      KC_Z: Result := 'Z';
      KC_X: Result := 'X';
      KC_C: Result := 'C';
      KC_V: Result := 'V';
      KC_B: Result := 'B';
      KC_N: Result := 'N';
      KC_M: Result := 'M';
      KC_COMMA: Result := '<';
      KC_PERIOD: Result := '>';      (* . on main keyboard *)
      KC_SLASH: Result := '?';      (* '/' on main keyboard *)
      KC_MULTIPLY: Result := '*';    (* * on numeric keypad *)
      KC_SPACE: Result := ' ';
    End; // Case
  End;
End; // GetKeyChar

End.
