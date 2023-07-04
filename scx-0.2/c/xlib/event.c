/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_queued_mode_binding = S48_FALSE;
#define scx_extract_queued_mode(x) S48_EXTRACT_ENUM(x, scx_queued_mode_binding)

s48_value scx_Events_Queued(s48_value display, s48_value mode) {
  int r;
  S48_DECLARE_GC_PROTECT_2(display, mode);
  r = XEventsQueued(scx_extract_display(display),
		    scx_extract_queued_mode(mode));
  S48_GC_RETURN(s48_enter_integer(r));
}

s48_value scx_Events_Pending(s48_value display) {
  return s48_enter_integer(XPending(scx_extract_display(display)));
}

s48_value scx_Next_Event(s48_value display) {
  XEvent e;
  XNextEvent(scx_extract_display(display), &e);
  return scx_enter_event(&e);
}

s48_value scx_Peek_Event(s48_value display) {
  XEvent e;
  XPeekEvent(scx_extract_display(display), &e);
  return scx_enter_event(&e);
}

s48_value scx_Get_Motion_Events(s48_value display, s48_value window,
				s48_value from, s48_value to) {
  int n,i;
  XTimeCoord* p;
  s48_value l = S48_NULL, t = S48_FALSE;
  S48_DECLARE_GC_PROTECT_6(display, window, from, to, l, t);
  p = XGetMotionEvents(scx_extract_display(display),
		       scx_extract_window(window),
		       scx_extract_time(from),
		       scx_extract_time(to),
		       &n);

  for (i = n-1; i >= 0; i--) {
    t = s48_cons(s48_enter_fixnum(p[i].x), s48_enter_fixnum(p[i].y));
    t = s48_cons(scx_enter_time(p[i].time), t);
    l = s48_cons(t, l);
  }
  XFree((char*)p);

  S48_GC_RETURN(l);
}

s48_value scx_Send_Event(s48_value display, s48_value window,
			 s48_value propagate,
			 s48_value event_mask, s48_value event) {
  XEvent e;
  Status r;
  S48_DECLARE_GC_PROTECT_5(display, window, propagate, event_mask, event);
  scx_extract_event(event, &e);
  
  r = XSendEvent(scx_extract_display(display),
		 scx_extract_window(window),
		 S48_EXTRACT_BOOLEAN(propagate),
		 scx_extract_event_mask(event_mask),
		 &e);
  S48_GC_RETURN(r ? S48_TRUE : S48_FALSE);
}

void scx_init_event(void) {
  SCX_PRO_IMP(scx_queued_mode_binding, "scx-queued-mode");

  S48_EXPORT_FUNCTION(scx_Events_Queued);
  S48_EXPORT_FUNCTION(scx_Events_Pending);
  S48_EXPORT_FUNCTION(scx_Next_Event);
  S48_EXPORT_FUNCTION(scx_Peek_Event);
  S48_EXPORT_FUNCTION(scx_Get_Motion_Events);
  S48_EXPORT_FUNCTION(scx_Send_Event);
}
