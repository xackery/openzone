Unit OgreKeyEvent;
(*
-----------------------------------------------------------------------------
This source file is part of OGRE
    (Object-oriented Graphics Rendering Engine)
For the latest info, see http://www.ogre3d.org/

Copyright © 2000-2002 The OGRE Team
Also see acknowledgements in Readme.html

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU Lesser General  License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Lesser General  License for more details.

You should have received a copy of the GNU Lesser General  License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place - Suite 330, Boston, MA 02111-1307, USA, or go to
http://www.gnu.org/copyleft/lesser.txt.
-----------------------------------------------------------------------------
*)
(***************************************************************************
OgreKeyEvent.h  -
 *
-------------------
begin                : Dec 03 2002
copyright            : (C) 2002 by Kenny Sabir
email                : kenny@sparksuit.com
***************************************************************************)

Interface

Uses OgreInputEvent;

(**
 * @version 1.22 04/22/99
 * @author Carl Quinn
 *)

Const
  KE_FIRST_EVENT = 2500;
  KE_LAST_EVENT  = 2504;

  KE_KEY_CLICKED  = KE_FIRST_EVENT;
  KE_KEY_PRESSED  = KE_FIRST_EVENT + 1;
  KE_KEY_RELEASED = KE_FIRST_EVENT + 2;
  KE_KEY_FOCUSIN  = KE_FIRST_EVENT + 3;
  KE_KEY_FOCUSOUT = KE_FIRST_EVENT + 4;

Type
  KeyEvent = Class(InputEvent)
  Protected
    (**
     * Which key was pressed
     *)
    mKey: Integer;
  Public
    (**
     * Constructs a KeyEvent object with the specified source KeyTarget,
     * type, modifiers, coordinates, and click count.
     *
     * @param source       the KeyTarget that originated the event
     * @param id           the integer that identifies the event
     * @param when         a long int that gives the time the event occurred
     * @param modifiers    the modifier keys down during event
     *                     (shift, ctrl, alt, meta)
     * @param x            the horizontal x coordinate for the key location
     * @param y            the vertical y coordinate for the key location
     * @param clickCount   the number of key clicks associated with event
     *)
    Constructor Create({PositionTarget* source, }ID, Key: Integer; When: Single; Modifiers: Integer);

    (**
     * Returns a parameter string identifying this event.
     * This method is useful for event-logging and for debugging.
     *
     * @return a string identifying the event and its attributes
     *)
    Function ParamString: String;

    (** return the char of the button *)
    Function GetKeyChar: Char;

    (** return the ID of the button *)
    Property GetKey : Integer Read mKey;
  End;

Implementation

Uses OgreInput,SysUtils;

Constructor KeyEvent.Create({PositionTarget* source, }ID, Key: Integer; When: Single; Modifiers: Integer);
Begin
  Inherited Create({Source, }ID, Trunc(When), Modifiers);
  mKey := Key;
End; // KeyEvent.Create

Function KeyEvent.GetKeyChar: Char;
Begin
  Result := OgreInput.GetKeyChar(mKey, mModifiers);
End; // KeyEvent.GetKeyChar

(**
 * Returns a parameter string identifying this event.
 * This method is useful for event-logging and for debugging.
 *
 * @return a string identifying the event and its attributes
 *)
Function KeyEvent.ParamString: String;
Var TypeStr: String;
Begin
  Case mId Of
    KE_KEY_PRESSED:  TypeStr := 'KEY_PRESSED';
    KE_KEY_RELEASED: TypeStr := 'KEY_RELEASED';
    KE_KEY_CLICKED:  TypeStr := 'KEY_CLICKED';
    KE_KEY_FOCUSIN:  TypeStr := 'KEY_FOCUSIN';
    KE_KEY_FOCUSOUT: TypeStr := 'KEY_FOCUSOUT';
  Else
    TypeStr := 'unknown type';
  End; // Case
  Result := TypeStr + ', key=' + IntToStr(mKey);
End; // KeyEvent.ParamString

End.
