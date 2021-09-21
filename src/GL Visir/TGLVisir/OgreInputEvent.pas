Unit OgreInputEvent;
(*
-----------------------------------------------------------------------------
This source file is part of OGRE
    (Object-oriented Graphics Rendering Engine)
For the latest info, see http://www.ogre3d.org/

Copyright © 2000-2002 The OGRE Team
Also see acknowledgements in Readme.html

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place - Suite 330, Boston, MA 02111-1307, USA, or go to
http://www.gnu.org/copyleft/lesser.txt.
-----------------------------------------------------------------------------
*)

Interface

(***************************************************************************
OgreInputEvent.h  -
* The root event class for all GuiElement-level input events.
*
* Input events are delivered to listeners before they are
* processed normally by the source where they originated.
* This allows listeners and GuiElement subclasses to "consume"
* the event so that the source will not process them in their
* default manner.  For example, consuming mousePressed events
* on a Button GuiElement will prevent the Button from being
* activated.
-------------------
begin                : Nov 19 2002
copyright            : (C) 2002 by Kenny Sabir
email                : kenny@sparksuit.com
***************************************************************************)

(** The root event class for all GuiElement-level input events.
@remarks
Input events are delivered to listeners before they are
processed normally by the source where they originated.
This allows listeners and GuiElement subclasses to "consume"
the event so that the source will not process them in their
default manner.  For example, consuming mousePressed events
on a Button GuiElement will prevent the Button from being
activated.
*)

Const
  (**
   * This flag indicates that the Shift key was down when the event
   * occurred.
   *)
  SHIFT_MASK      = 1 Shl 0;

  (**
   * This flag indicates that the Control key was down when the event
   * occurred.
   *)
  CTRL_MASK       = 1 Shl 1;

  (**
   * This flag indicates that the Meta key was down when the event
   * occurred. For mouse events, this flag indicates that the right
   * button was pressed or released.
   *)

  META_MASK       = 1 Shl 2;
  (**
   * This flag indicates that the Alt key was down when
   * the event occurred. For mouse events, this flag indicates that the
   * middle mouse button was pressed or released.
   *)
  ALT_MASK        = 1 Shl 3;
  BUTTON0_MASK    = 1 Shl 4;
  BUTTON1_MASK    = 1 Shl 5;
  BUTTON2_MASK    = 1 Shl 6;
  BUTTON3_MASK    = 1 Shl 7;
  BUTTON_ANY_MASK = $F Shl 4;


Type
  InputEvent = Class
  Protected
    (**
     * Not implemented yet
     *)
    mWhen: Single;

    (**
     * The state of the modifier keys at the time the input
     * event was fired.
     *)
    mModifiers: Integer;

    (**
     * The target to process the event. This is ususally found by the dispatcher
     *)
    // EventTarget* mSource;  <-- Can't use this in Delphi

    (**
     * The ID of the event
     *)
    mId: Integer;

    (**
     * whether the event has been consumed
     *)
    mConsumed: Boolean;

  Public
    (**
     * Constructs an InputEvent object with the specified source GuiElement,
     * modifiers, and type.
     * @param source the object where the event originated
     * @id the event type
     * @when the time the event occurred
     * @modifiers the modifier keys down while event occurred
     *)
    Constructor Create({EventTarget* source, }ID,When,Modifiers: Integer);

    (**
     * Consumes this event so that it will not be processed
     * in the default manner by the source which originated it.
     *)
    Procedure Consume;

    (**
     * Returns whether or not the Alt modifier is down on this event.
     *)
    Function IsAltDown: Boolean;

    (**
     * Returns whether or not the Control modifier is down on this event.
     *)
    Function IsControlDown: Boolean;

    (**
     * Returns whether or not the Meta modifier is down on this event.
     *)
    Function IsMetaDown: Boolean;

    (**
     * Returns whether or not the Shift modifier is down on this event.
     *)
    Function IsShiftDown: Boolean;

    Function IsEventBetween(Start,_End: Integer): Boolean;

    (**
     * Returns the modifiers flag for this event.
     *)
    Property GetModifiers : Integer Read mModifiers;

    (**
     * Returns the timestamp of when this event occurred. Not implemented yet
     *)
    Property GetWhen      : Single  Read mWhen;

    (**
     * Returns whether or not this event has been consumed.
     * @see #consume
     *)
    Property IsConsumed   : Boolean Read mConsumed;

    Property GetID        : Integer Read mID;

//    EventTarget* getSource const;
  End;

Implementation

Constructor InputEvent.Create({EventTarget* source, }ID,When,Modifiers: Integer);
Begin
  mWhen      := When;
  mModifiers := Modifiers;
//  mSource    := Source;
  mID        := ID;
  mConsumed  := False;
End; // InputEvent.Create

Procedure InputEvent.Consume;
Begin
  mConsumed := True;
End; // InputEvent.Consume

Function InputEvent.IsAltDown: Boolean;
Begin
  Result := ((mModifiers And ALT_MASK) <> 0);
End; // InputEvent.IsAltDown

Function InputEvent.IsControlDown: Boolean;
Begin
  Result := ((mModifiers And CTRL_MASK) <> 0);
End; // InputEvent.IsControlDown

Function InputEvent.IsMetaDown: Boolean;
Begin
  Result := ((mModifiers And META_MASK) <> 0);
End; // InputEvent.IsMetaDown

Function InputEvent.IsShiftDown: Boolean;
Begin
  Result := ((mModifiers And SHIFT_MASK) <> 0);
End; // InputEvent.IsShiftDown

Function InputEvent.IsEventBetween(Start,_End: Integer): Boolean;
Begin
  Result := ((mId >= Start) And (mId <= _End));
End; // InputEvent.IsEventBetween

{
EventTarget* InputEvent.GetSource const
Begin
        Result := mSource;

End; //
}
End.
