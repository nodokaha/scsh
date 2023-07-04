/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_grab_mode_binding = S48_FALSE;
#define scx_extract_grab_mode(x) S48_EXTRACT_ENUM(x, scx_grab_mode_binding)
s48_value scx_grab_states_binding = S48_FALSE;
#define scx_enter_grab_status(x) S48_ENTER_ENUM(x, scx_grab_states_binding)

s48_value scx_Grab_Pointer(s48_value dpy, s48_value win, 
			   s48_value ownerp, s48_value events, 
			   s48_value pmode, s48_value kmode, 
			   s48_value confine_to, s48_value cursor, 
			   s48_value time) {
  int res;
  S48_DECLARE_GC_PROTECT_9(dpy, win, ownerp, events, pmode, kmode, confine_to,
			   cursor, time);
  res = XGrabPointer(scx_extract_display(dpy),
			 scx_extract_window(win),
			 S48_EXTRACT_BOOLEAN(ownerp),
			 scx_extract_event_mask(events),
			 scx_extract_grab_mode(pmode), 
			 scx_extract_grab_mode(kmode),
			 scx_extract_window(confine_to),
			 scx_extract_cursor(cursor),
			 scx_extract_time(time));
  S48_GC_RETURN(scx_enter_grab_status(res));
}

s48_value scx_Ungrab_Pointer(s48_value dpy, s48_value time) {
  S48_DECLARE_GC_PROTECT_2(dpy, time);
  XUngrabPointer(scx_extract_display(dpy), scx_extract_time(time));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Change_Active_Pointer_Grab(s48_value dpy, s48_value events,
					 s48_value cursor, s48_value time){
  S48_DECLARE_GC_PROTECT_4(dpy, events, cursor, time);
  XChangeActivePointerGrab(scx_extract_display(dpy),
			   scx_extract_event_mask(events),
			   scx_extract_cursor(cursor),
			   scx_extract_time(time));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Grab_Button(s48_value dpy, s48_value button, s48_value mods,
			  s48_value win, s48_value ownerp, s48_value events,
			  s48_value pmode, s48_value kmode, 
			  s48_value confine_to, s48_value cursor) {
  S48_DECLARE_GC_PROTECT_10(dpy, button, mods, win, ownerp, events, pmode,
			    kmode, confine_to, cursor);
  XGrabButton(scx_extract_display(dpy),
	      scx_extract_button(button),
	      scx_extract_state_set(mods),
	      scx_extract_window(win),
	      S48_EXTRACT_BOOLEAN(ownerp),
	      scx_extract_event_mask(events),
	      scx_extract_grab_mode(pmode),
	      scx_extract_grab_mode(kmode),
	      scx_extract_window(confine_to),
	      scx_extract_cursor(cursor));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Ungrab_Button(s48_value dpy, s48_value button,
			    s48_value mods, s48_value win) {
  S48_DECLARE_GC_PROTECT_4(dpy, button, mods, win);
  XUngrabButton(scx_extract_display(dpy),
		scx_extract_button(button),
		scx_extract_state_set(mods),
		scx_extract_window(win));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Grab_Keyboard(s48_value dpy, s48_value win, s48_value ownerp, 
			    s48_value pmode, s48_value kmode,
			    s48_value time) {
  int res;
  S48_DECLARE_GC_PROTECT_6(dpy, win, ownerp, pmode, kmode, time);
  res = XGrabKeyboard( scx_extract_display(dpy),
			   scx_extract_window(win),
			   S48_EXTRACT_BOOLEAN(ownerp),
			   scx_extract_grab_mode(pmode),
			   scx_extract_grab_mode(kmode),
			   scx_extract_time(time));
  S48_GC_RETURN(scx_enter_grab_status(res));
}

s48_value scx_Ungrab_Keyboard(s48_value dpy, s48_value time){
  S48_DECLARE_GC_PROTECT_2(dpy, time);
  XUngrabKeyboard(scx_extract_display(dpy),
		  scx_extract_time(time));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Grab_Key(s48_value dpy, s48_value key, s48_value mods,
		       s48_value win, s48_value ownerp, s48_value pmode,
		       s48_value kmode) {
  S48_DECLARE_GC_PROTECT_7(dpy, key, mods, win, ownerp, pmode, kmode);
  XGrabKey(scx_extract_display(dpy),
	   s48_extract_integer(key), 
	   scx_extract_state_set(mods),
	   scx_extract_window(win),
	   S48_EXTRACT_BOOLEAN(ownerp),
	   scx_extract_grab_mode(pmode),
	   scx_extract_grab_mode(kmode));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Ungrab_Key(s48_value dpy, s48_value key, s48_value mods,
			 s48_value win) {
  S48_DECLARE_GC_PROTECT_4(dpy, key, mods, win);
  XUngrabKey(scx_extract_display(dpy),
	     s48_extract_integer(key), 
	     scx_extract_state_set(mods),
	     scx_extract_window(win));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_event_mode_binding = S48_FALSE;
#define scx_extract_event_mode(x) S48_EXTRACT_ENUM(x, scx_event_mode_binding)

s48_value scx_Allow_Events(s48_value dpy, s48_value event_mode,
			   s48_value time) {
  S48_DECLARE_GC_PROTECT_3(dpy, event_mode, time);
  XAllowEvents(scx_extract_display(dpy),
	       scx_extract_event_mode(event_mode),
	       scx_extract_time(time));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Grab_Server(s48_value dpy) {
  XGrabServer(scx_extract_display(dpy));
  return S48_UNSPECIFIC;
}

s48_value scx_Ungrab_Server(s48_value dpy){
  XUngrabServer(scx_extract_display(dpy));
  return S48_UNSPECIFIC;
}

void scx_init_grab(void) {
  SCX_PRO_IMP(scx_grab_mode_binding, "scx-grab-mode");
  SCX_PRO_IMP(scx_grab_states_binding, "scx-grab-states");
  SCX_PRO_IMP(scx_event_mode_binding, "scx-event-mode");

  S48_EXPORT_FUNCTION(scx_Grab_Pointer);
  S48_EXPORT_FUNCTION(scx_Ungrab_Pointer);
  S48_EXPORT_FUNCTION(scx_Grab_Button);
  S48_EXPORT_FUNCTION(scx_Ungrab_Button);
  S48_EXPORT_FUNCTION(scx_Change_Active_Pointer_Grab);
  S48_EXPORT_FUNCTION(scx_Grab_Keyboard);
  S48_EXPORT_FUNCTION(scx_Ungrab_Keyboard);
  S48_EXPORT_FUNCTION(scx_Grab_Key);
  S48_EXPORT_FUNCTION(scx_Ungrab_Key);
  S48_EXPORT_FUNCTION(scx_Allow_Events);
  S48_EXPORT_FUNCTION(scx_Grab_Server);
  S48_EXPORT_FUNCTION(scx_Ungrab_Server);
}
		      
