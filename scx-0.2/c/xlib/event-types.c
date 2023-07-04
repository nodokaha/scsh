/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

#define EENTER(i, n, f) S48_RECORD_SET(e, i, f(xe->n))

#define EENTER_START(rtype) \
  s48_value e = s48_make_record(rtype); \
  S48_DECLARE_GC_PROTECT(1); \
  S48_GC_PROTECT_1(e); \
  EENTER(0, type, scx_enter_event_type); \
  EENTER(1, serial, s48_enter_integer); \
  EENTER(2, send_event, S48_ENTER_BOOLEAN); \
  EENTER(3, display, scx_enter_display);

#define EENTER_END() \
  S48_GC_UNPROTECT(); \
  return e

s48_value scx_key_event_binding = S48_FALSE;

s48_value scx_enter_key_event(XKeyEvent* xe) {
  EENTER_START(scx_key_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, root, scx_enter_window);
  EENTER(6, subwindow, scx_enter_window);
  EENTER(7, time, scx_enter_time);
  EENTER(8, x, s48_enter_integer);
  EENTER(9, y, s48_enter_integer);
  EENTER(10, x_root, s48_enter_integer);
  EENTER(11, y_root, s48_enter_integer);
  EENTER(12, state, scx_enter_state_set);
  EENTER(13, keycode, scx_enter_keycode);
  EENTER(14, same_screen, S48_ENTER_BOOLEAN);
  EENTER_END();
}

s48_value scx_button_event_binding = S48_FALSE;

s48_value scx_enter_button_event(XButtonEvent* xe) {
  EENTER_START(scx_button_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, root, scx_enter_window);
  EENTER(6, subwindow, scx_enter_window);
  EENTER(7, time, scx_enter_time);
  EENTER(8, x, s48_enter_integer);
  EENTER(9, y, s48_enter_integer);
  EENTER(10, x_root, s48_enter_integer);
  EENTER(11, y_root, s48_enter_integer);
  EENTER(12, state, scx_enter_state_set);
  EENTER(13, button, scx_enter_button);
  EENTER(14, same_screen, S48_ENTER_BOOLEAN);
  EENTER_END();
}

s48_value scx_motion_event_binding = S48_FALSE;

s48_value scx_enter_motion_event(XMotionEvent* xe) {
  EENTER_START(scx_motion_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, root, scx_enter_window);
  EENTER(6, subwindow, scx_enter_window);
  EENTER(7, time, scx_enter_time);
  EENTER(8, x, s48_enter_integer);
  EENTER(9, y, s48_enter_integer);
  EENTER(10, x_root, s48_enter_integer);
  EENTER(11, y_root, s48_enter_integer);
  EENTER(12, state, scx_enter_state_set);
  EENTER(13, is_hint, S48_ENTER_BOOLEAN);
  EENTER(14, same_screen, S48_ENTER_BOOLEAN);
  EENTER_END();
}

s48_value scx_crossing_event_binding = S48_FALSE;

s48_value scx_enter_crossing_event(XCrossingEvent* xe) {
  EENTER_START(scx_crossing_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, root, scx_enter_window);
  EENTER(6, subwindow, scx_enter_window);
  EENTER(7, time, scx_enter_time);
  EENTER(8, x, s48_enter_integer);
  EENTER(9, y, s48_enter_integer);
  EENTER(10, x_root, s48_enter_integer);
  EENTER(11, y_root, s48_enter_integer);
  EENTER(12, mode, scx_enter_notify_mode);
  EENTER(13, detail, scx_enter_notify_detail);
  EENTER(14, same_screen, S48_ENTER_BOOLEAN);
  EENTER(15, focus, S48_ENTER_BOOLEAN);
  EENTER(16, state, scx_enter_state_set);
  EENTER_END();
}

s48_value scx_focus_change_event_binding = S48_FALSE;

s48_value scx_enter_focus_change_event(XFocusChangeEvent* xe) {
  EENTER_START(scx_focus_change_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, mode, scx_enter_notify_mode);
  EENTER(6, detail, scx_enter_notify_detail);
  EENTER_END();
}

s48_value scx_expose_event_binding = S48_FALSE;

s48_value scx_enter_expose_event(XExposeEvent* xe) {
  EENTER_START(scx_expose_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, x, s48_enter_integer);
  EENTER(6, y, s48_enter_integer);
  EENTER(7, width, s48_enter_integer);
  EENTER(8, height, s48_enter_integer);
  EENTER(9, count, s48_enter_integer);
  EENTER_END();
}

s48_value scx_graphics_expose_event_binding = S48_FALSE;

s48_value scx_enter_graphics_expose_event(XGraphicsExposeEvent* xe) {
  EENTER_START(scx_graphics_expose_event_binding);
  EENTER(4, drawable, scx_enter_drawable);
  EENTER(5, x, s48_enter_integer);
  EENTER(6, y, s48_enter_integer);
  EENTER(7, width, s48_enter_integer);
  EENTER(8, height, s48_enter_integer);
  EENTER(9, count, s48_enter_integer);
  EENTER(10, major_code, s48_enter_integer);
  EENTER(11, minor_code, s48_enter_integer);
  EENTER_END();
}

s48_value scx_no_expose_event_binding = S48_FALSE;

s48_value scx_enter_no_expose_event(XNoExposeEvent* xe) {
  EENTER_START(scx_no_expose_event_binding);
  EENTER(4, drawable, scx_enter_drawable);
  EENTER(5, major_code, s48_enter_integer);
  EENTER(6, minor_code, s48_enter_integer);
  EENTER_END();
}

s48_value scx_visibility_event_binding = S48_FALSE;

s48_value scx_enter_visibility_event(XVisibilityEvent* xe) {
  EENTER_START(scx_visibility_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, state, scx_enter_visibility_state);
  EENTER_END();
}

s48_value scx_create_window_event_binding = S48_FALSE;

s48_value scx_enter_create_window_event(XCreateWindowEvent* xe) {
  EENTER_START(scx_create_window_event_binding);
  EENTER(4, parent, scx_enter_window);
  EENTER(5, window, scx_enter_window);
  EENTER(6, x, s48_enter_integer);
  EENTER(7, y, s48_enter_integer);
  EENTER(8, width, s48_enter_integer);
  EENTER(9, height, s48_enter_integer);
  EENTER(10, border_width, s48_enter_integer);
  EENTER(11, override_redirect, S48_ENTER_BOOLEAN);
  EENTER_END();
}

s48_value scx_destroy_window_event_binding = S48_FALSE;

s48_value scx_enter_destroy_window_event(XDestroyWindowEvent* xe) {
  EENTER_START(scx_destroy_window_event_binding);
  EENTER(4, event, scx_enter_window);
  EENTER(5, window, scx_enter_window);
  EENTER_END();
}

s48_value scx_unmap_event_binding = S48_FALSE;

s48_value scx_enter_unmap_event(XUnmapEvent* xe) {
  EENTER_START(scx_unmap_event_binding);
  EENTER(4, event, scx_enter_window);
  EENTER(5, window, scx_enter_window);
  EENTER(6, from_configure, S48_ENTER_BOOLEAN);
  EENTER_END();
}

s48_value scx_map_event_binding = S48_FALSE;

s48_value scx_enter_map_event(XMapEvent* xe) {
  EENTER_START(scx_map_event_binding);
  EENTER(4, event, scx_enter_window);
  EENTER(5, window, scx_enter_window);
  EENTER(6, override_redirect, S48_ENTER_BOOLEAN);
  EENTER_END();
}

s48_value scx_map_request_event_binding = S48_FALSE;

s48_value scx_enter_map_request_event(XMapRequestEvent* xe) {
  EENTER_START(scx_map_request_event_binding);
  EENTER(4, parent, scx_enter_window);
  EENTER(5, window, scx_enter_window);
  EENTER_END();
}

s48_value scx_reparent_event_binding = S48_FALSE;

s48_value scx_enter_reparent_event(XReparentEvent* xe) {
  EENTER_START(scx_reparent_event_binding);
  EENTER(4, event, scx_enter_window);
  EENTER(5, window, scx_enter_window);
  EENTER(6, parent, scx_enter_window);
  EENTER(7, x, s48_enter_integer);
  EENTER(8, y, s48_enter_integer);
  EENTER(9, override_redirect, S48_ENTER_BOOLEAN);
  EENTER_END();
}

s48_value scx_configure_event_binding = S48_FALSE;

s48_value scx_enter_configure_event(XConfigureEvent* xe) {
  EENTER_START(scx_configure_event_binding);
  EENTER(4, event, scx_enter_window);
  EENTER(5, window, scx_enter_window);
  EENTER(6, x, s48_enter_integer);
  EENTER(7, y, s48_enter_integer);
  EENTER(8, width, s48_enter_integer);
  EENTER(9, height, s48_enter_integer);
  EENTER(10, border_width, s48_enter_integer);
  EENTER(11, above, scx_enter_window);
  EENTER(12, override_redirect, S48_ENTER_BOOLEAN);
  EENTER_END();
}

s48_value scx_gravity_event_binding = S48_FALSE;

s48_value scx_enter_gravity_event(XGravityEvent* xe) {
  EENTER_START(scx_gravity_event_binding);
  EENTER(4, event, scx_enter_window);
  EENTER(5, window, scx_enter_window);
  EENTER(6, x, s48_enter_integer);
  EENTER(7, y, s48_enter_integer);
  EENTER_END();
}

s48_value scx_resize_request_event_binding = S48_FALSE;

s48_value scx_enter_resize_request_event(XResizeRequestEvent* xe) {
  EENTER_START(scx_resize_request_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, width, s48_enter_integer);
  EENTER(6, height, s48_enter_integer);
  EENTER_END();
}

s48_value scx_configure_request_event_binding = S48_FALSE;

s48_value scx_enter_configure_request_event(XConfigureRequestEvent* xe) {
  XWindowChanges WC;
  EENTER_START(scx_configure_request_event_binding);
  EENTER(4, parent, scx_enter_window);
  EENTER(5, window, scx_enter_window);
  WC.x = xe->x;
  WC.y = xe->y;
  WC.width = xe->width;
  WC.height = xe->height;
  WC.border_width = xe->border_width;
  WC.sibling = xe->above;
  WC.stack_mode = xe->detail;
  S48_RECORD_SET(e, 6, scx_enter_window_changes(&WC, xe->value_mask));
  EENTER_END();
}

s48_value scx_circulate_event_binding = S48_FALSE;

s48_value scx_enter_circulate_event(XCirculateEvent* xe) {
  EENTER_START(scx_circulate_event_binding);
  EENTER(4, event, scx_enter_window);
  EENTER(5, window, scx_enter_window);
  EENTER(6, place, scx_enter_place);
  EENTER_END();
}

s48_value scx_circulate_request_event_binding = S48_FALSE;

s48_value scx_enter_circulate_request_event(XCirculateRequestEvent* xe) {
  EENTER_START(scx_circulate_request_event_binding);
  EENTER(4, parent, scx_enter_window);
  EENTER(5, window, scx_enter_window);
  EENTER(6, place, scx_enter_place);
  EENTER_END();
}

s48_value scx_property_event_binding = S48_FALSE;

s48_value scx_enter_property_event(XPropertyEvent* xe) {
  EENTER_START(scx_property_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, atom, scx_enter_atom);
  EENTER(6, time, scx_enter_time);
  EENTER(7, state, scx_enter_property_state);
  EENTER_END();
}

s48_value scx_selection_clear_event_binding = S48_FALSE;

s48_value scx_enter_selection_clear_event(XSelectionClearEvent* xe) {
  EENTER_START(scx_selection_clear_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, selection, scx_enter_atom);
  EENTER(6, time, scx_enter_time);
  EENTER_END();
}

s48_value scx_selection_request_event_binding = S48_FALSE;

s48_value scx_enter_selection_request_event(XSelectionRequestEvent* xe) {
  EENTER_START(scx_selection_request_event_binding);
  EENTER(4, owner, scx_enter_window);
  EENTER(5, requestor, scx_enter_window);
  EENTER(6, selection, scx_enter_atom);
  EENTER(7, target, scx_enter_atom);
  EENTER(8, property, scx_enter_atom);
  EENTER(9, time, scx_enter_time);
  EENTER_END();
}

s48_value scx_selection_event_binding = S48_FALSE;

s48_value scx_enter_selection_event(XSelectionEvent* xe) {
  EENTER_START(scx_selection_event_binding);
  EENTER(4, requestor, scx_enter_window);
  EENTER(5, selection, scx_enter_atom);
  EENTER(6, target, scx_enter_atom);
  EENTER(7, property, scx_enter_atom);
  EENTER(8, time, scx_enter_time);
  EENTER_END();
}

s48_value scx_colormap_event_binding = S48_FALSE;

s48_value scx_enter_colormap_event(XColormapEvent* xe) {
  EENTER_START(scx_colormap_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, colormap, scx_enter_colormap);
  EENTER(6, new, S48_ENTER_BOOLEAN);
  EENTER(7, state, scx_enter_colormap_state);
  EENTER_END();
}

s48_value scx_client_message_event_binding = S48_FALSE;

s48_value scx_enter_client_message_event(XClientMessageEvent* xe) {
  EENTER_START(scx_client_message_event_binding);
  EENTER(4, window, scx_enter_window);
  S48_RECORD_SET(e, 5, scx_enter_property(xe->message_type,
					  xe->format,
					  xe->data.b,
					  (20 / (xe->format >> 3))));
  EENTER_END();
}

s48_value scx_mapping_event_binding = S48_FALSE;

s48_value scx_enter_mapping_event(XMappingEvent* xe) {
  EENTER_START(scx_mapping_event_binding);
  EENTER(4, window, scx_enter_window);
  EENTER(5, request, scx_enter_mapping_request);
  EENTER(6, first_keycode, scx_enter_keycode);
  EENTER(7, count, s48_enter_integer);
  EENTER_END();
}

s48_value scx_keymap_event_binding = S48_FALSE;

s48_value scx_enter_keymap_event(XKeymapEvent* xe) {
  s48_value temp; int i;
  EENTER_START(scx_keymap_event_binding);
  temp = s48_make_vector(32*8, s48_enter_fixnum(0));
  for (i = 0; i < 32; i++) {
    int j; char b = xe->key_vector[i];
    for (j = 0; j < 8; j++)
      S48_VECTOR_SET(temp, i*8 + j, s48_enter_fixnum((b & (1 << j)) ? 1 : 0));
  }
  S48_RECORD_SET(e, 4, temp);
  EENTER_END();
}

s48_value scx_enter_event(XEvent* e) {
  switch (e->type) {
  case KeyPress : case KeyRelease :
    return scx_enter_key_event((XKeyEvent*)e);
  case ButtonPress : case ButtonRelease :
    return scx_enter_button_event((XButtonEvent*)e);
  case MotionNotify :
    return scx_enter_motion_event((XMotionEvent*)e);
  case EnterNotify : case LeaveNotify :
    return scx_enter_crossing_event((XCrossingEvent*)e);
  case FocusIn : case FocusOut :
    return scx_enter_focus_change_event((XFocusChangeEvent*)e);
  case KeymapNotify :
    return scx_enter_keymap_event((XKeymapEvent*)e);
  case Expose :
    return scx_enter_expose_event((XExposeEvent*)e);
  case GraphicsExpose :
    return scx_enter_graphics_expose_event((XGraphicsExposeEvent*)e);
  case NoExpose :
    return scx_enter_no_expose_event((XNoExposeEvent*)e);
  case VisibilityNotify :
    return scx_enter_visibility_event((XVisibilityEvent*)e);
  case CreateNotify :
    return scx_enter_create_window_event((XCreateWindowEvent*)e);
  case DestroyNotify :
    return scx_enter_destroy_window_event((XDestroyWindowEvent*)e);
  case UnmapNotify :
    return scx_enter_unmap_event((XUnmapEvent*)e);
  case MapNotify :
    return scx_enter_map_event((XMapEvent*)e);
  case MapRequest :
    return scx_enter_map_request_event((XMapRequestEvent*)e);
  case ReparentNotify :
    return scx_enter_reparent_event((XReparentEvent*)e);
  case ConfigureNotify :
    return scx_enter_configure_event((XConfigureEvent*)e);
  case ConfigureRequest :
    return scx_enter_configure_request_event((XConfigureRequestEvent*)e);
  case GravityNotify :
    return scx_enter_gravity_event((XGravityEvent*)e);
  case ResizeRequest :
    return scx_enter_resize_request_event((XResizeRequestEvent*)e);
  case CirculateRequest :
    return scx_enter_circulate_request_event((XCirculateRequestEvent*)e);
  case PropertyNotify :
    return scx_enter_property_event((XPropertyEvent*)e);
  case SelectionClear :
    return scx_enter_selection_clear_event((XSelectionClearEvent*)e);
  case SelectionRequest :
    return scx_enter_selection_request_event((XSelectionRequestEvent*)e);
  case SelectionNotify :
    return scx_enter_selection_event((XSelectionEvent*)e);
  case ColormapNotify :
    return scx_enter_colormap_event((XColormapEvent*)e);
  case ClientMessage :
    return scx_enter_client_message_event((XClientMessageEvent*)e);
  case MappingNotify :
    return scx_enter_mapping_event((XMappingEvent*)e);
  default: return S48_FALSE;
  } /* switch end */
}

/*** extraction ****************************************************/

#define EEXTRACT(i, name, f) xe->name = f(S48_RECORD_REF(e, i))
#define EEXTRACT_START(rtype) \
  S48_DECLARE_GC_PROTECT(1); \
  S48_GC_PROTECT_1(e); \
  s48_check_record_type(e, rtype); \
  EEXTRACT(0, type, scx_extract_event_type); \
  EEXTRACT(1, serial, s48_extract_integer); \
  EEXTRACT(2, send_event, S48_EXTRACT_BOOLEAN); \
  EEXTRACT(3, display, scx_extract_display)
#define EEXTRACT_END() \
  S48_GC_UNPROTECT()

void scx_extract_key_event(s48_value e, XKeyEvent* xe) {
  EEXTRACT_START(scx_key_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, root, scx_extract_window);
  EEXTRACT(6, subwindow, scx_extract_window);
  EEXTRACT(7, time, scx_extract_time);
  EEXTRACT(8, x, s48_extract_integer);
  EEXTRACT(9, y, s48_extract_integer);
  EEXTRACT(10, x_root, s48_extract_integer);
  EEXTRACT(11, y_root, s48_extract_integer);
  EEXTRACT(12, state, scx_extract_state_set);
  EEXTRACT(13, keycode, scx_extract_keycode);
  EEXTRACT(14, same_screen, S48_EXTRACT_BOOLEAN);
  EEXTRACT_END();
}

void scx_extract_button_event(s48_value e, XButtonEvent* xe) {
  EEXTRACT_START(scx_button_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, root, scx_extract_window);
  EEXTRACT(6, subwindow, scx_extract_window);
  EEXTRACT(7, time, scx_extract_time);
  EEXTRACT(8, x, s48_extract_integer);
  EEXTRACT(9, y, s48_extract_integer);
  EEXTRACT(10, x_root, s48_extract_integer);
  EEXTRACT(11, y_root, s48_extract_integer);
  EEXTRACT(12, state, scx_extract_state_set);
  EEXTRACT(13, button, scx_extract_button);
  EEXTRACT(14, same_screen, S48_EXTRACT_BOOLEAN);
  EEXTRACT_END();
}

void scx_extract_motion_event(s48_value e, XMotionEvent* xe) {
  EEXTRACT_START(scx_motion_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, root, scx_extract_window);
  EEXTRACT(6, subwindow, scx_extract_window);
  EEXTRACT(7, time, scx_extract_time);
  EEXTRACT(8, x, s48_extract_integer);
  EEXTRACT(9, y, s48_extract_integer);
  EEXTRACT(10, x_root, s48_extract_integer);
  EEXTRACT(11, y_root, s48_extract_integer);
  EEXTRACT(12, state, scx_extract_state_set);
  EEXTRACT(13, is_hint, S48_EXTRACT_BOOLEAN);
  EEXTRACT(14, same_screen, S48_EXTRACT_BOOLEAN);
  EEXTRACT_END();
}

void scx_extract_crossing_event(s48_value e, XCrossingEvent* xe) {
  EEXTRACT_START(scx_crossing_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, root, scx_extract_window);
  EEXTRACT(6, subwindow, scx_extract_window);
  EEXTRACT(7, time, scx_extract_time);
  EEXTRACT(8, x, s48_extract_integer);
  EEXTRACT(9, y, s48_extract_integer);
  EEXTRACT(10, x_root, s48_extract_integer);
  EEXTRACT(11, y_root, s48_extract_integer);
  EEXTRACT(12, mode, scx_extract_notify_mode);
  EEXTRACT(13, detail, scx_extract_notify_detail);
  EEXTRACT(14, same_screen, S48_EXTRACT_BOOLEAN);
  EEXTRACT(15, focus, S48_EXTRACT_BOOLEAN);
  EEXTRACT(16, state, scx_extract_state_set);
  EEXTRACT_END();
}

void scx_extract_focus_change_event(s48_value e, XFocusChangeEvent* xe) {
  EEXTRACT_START(scx_focus_change_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, mode, scx_extract_notify_mode);
  EEXTRACT(6, detail, scx_extract_notify_detail);
  EEXTRACT_END();
}

void scx_extract_expose_event(s48_value e, XExposeEvent* xe) {
  EEXTRACT_START(scx_expose_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, x, s48_extract_integer);
  EEXTRACT(6, y, s48_extract_integer);
  EEXTRACT(7, width, s48_extract_integer);
  EEXTRACT(8, height, s48_extract_integer);
  EEXTRACT(9, count, s48_extract_integer);
  EEXTRACT_END();
}

void scx_extract_graphics_expose_event(s48_value e, XGraphicsExposeEvent* xe) {
  EEXTRACT_START(scx_graphics_expose_event_binding);
  EEXTRACT(4, drawable, scx_extract_drawable);
  EEXTRACT(5, x, s48_extract_integer);
  EEXTRACT(6, y, s48_extract_integer);
  EEXTRACT(7, width, s48_extract_integer);
  EEXTRACT(8, height, s48_extract_integer);
  EEXTRACT(9, count, s48_extract_integer);
  EEXTRACT(10, major_code, s48_extract_integer);
  EEXTRACT(11, minor_code, s48_extract_integer);
  EEXTRACT_END();
}

void scx_extract_no_expose_event(s48_value e, XNoExposeEvent* xe) {
  EEXTRACT_START(scx_no_expose_event_binding);
  EEXTRACT(4, drawable, scx_extract_drawable);
  EEXTRACT(5, major_code, s48_extract_integer);
  EEXTRACT(6, minor_code, s48_extract_integer);
  EEXTRACT_END();
}

void scx_extract_visibility_event(s48_value e, XVisibilityEvent* xe) {
  EEXTRACT_START(scx_visibility_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, state, scx_extract_visibility_state);
  EEXTRACT_END();
}

void scx_extract_create_window_event(s48_value e, XCreateWindowEvent* xe) {
  EEXTRACT_START(scx_create_window_event_binding);
  EEXTRACT(4, parent, scx_extract_window);
  EEXTRACT(5, window, scx_extract_window);
  EEXTRACT(6, x, s48_extract_integer);
  EEXTRACT(7, y, s48_extract_integer);
  EEXTRACT(8, width, s48_extract_integer);
  EEXTRACT(9, height, s48_extract_integer);
  EEXTRACT(10, border_width, s48_extract_integer);
  EEXTRACT(11, override_redirect, S48_EXTRACT_BOOLEAN);
  EEXTRACT_END();
}

void scx_extract_destroy_window_event(s48_value e, XDestroyWindowEvent* xe) {
  EEXTRACT_START(scx_destroy_window_event_binding);
  EEXTRACT(4, event, scx_extract_window);
  EEXTRACT(5, window, scx_extract_window);
  EEXTRACT_END();
}

void scx_extract_unmap_event(s48_value e, XUnmapEvent* xe) {
  EEXTRACT_START(scx_unmap_event_binding);
  EEXTRACT(4, event, scx_extract_window);
  EEXTRACT(5, window, scx_extract_window);
  EEXTRACT(6, from_configure, S48_EXTRACT_BOOLEAN);
  EEXTRACT_END();
}

void scx_extract_map_event(s48_value e, XMapEvent* xe) {
  EEXTRACT_START(scx_map_event_binding);
  EEXTRACT(4, event, scx_extract_window);
  EEXTRACT(5, window, scx_extract_window);
  EEXTRACT(6, override_redirect, S48_EXTRACT_BOOLEAN);
  EEXTRACT_END();
}

void scx_extract_map_request_event(s48_value e, XMapRequestEvent* xe) {
  EEXTRACT_START(scx_map_request_event_binding);
  EEXTRACT(4, parent, scx_extract_window);
  EEXTRACT(5, window, scx_extract_window);
  EEXTRACT_END();
}

void scx_extract_reparent_event(s48_value e, XReparentEvent* xe) {
  EEXTRACT_START(scx_reparent_event_binding);
  EEXTRACT(4, event, scx_extract_window);
  EEXTRACT(5, window, scx_extract_window);
  EEXTRACT(6, parent, scx_extract_window);
  EEXTRACT(7, x, s48_extract_integer);
  EEXTRACT(8, y, s48_extract_integer);
  EEXTRACT(9, override_redirect, S48_EXTRACT_BOOLEAN);
  EEXTRACT_END();
}

void scx_extract_configure_event(s48_value e, XConfigureEvent* xe) {
  EEXTRACT_START(scx_configure_event_binding);
  EEXTRACT(4, event, scx_extract_window);
  EEXTRACT(5, window, scx_extract_window);
  EEXTRACT(6, x, s48_extract_integer);
  EEXTRACT(7, y, s48_extract_integer);
  EEXTRACT(8, width, s48_extract_integer);
  EEXTRACT(9, height, s48_extract_integer);
  EEXTRACT(10, border_width, s48_extract_integer);
  EEXTRACT(11, above, scx_extract_window);
  EEXTRACT(12, override_redirect, S48_EXTRACT_BOOLEAN);
  EEXTRACT_END();
}

void scx_extract_gravity_event(s48_value e, XGravityEvent* xe) {
  EEXTRACT_START(scx_gravity_event_binding);
  EEXTRACT(4, event, scx_extract_window);
  EEXTRACT(5, window, scx_extract_window);
  EEXTRACT(6, x, s48_extract_integer);
  EEXTRACT(7, y, s48_extract_integer);
  EEXTRACT_END();
}

void scx_extract_resize_request_event(s48_value e, XResizeRequestEvent* xe) {
  EEXTRACT_START(scx_resize_request_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, width, s48_extract_integer);
  EEXTRACT(6, height, s48_extract_integer);
  EEXTRACT_END();
}

void scx_extract_configure_request_event(s48_value e,
					 XConfigureRequestEvent* xe) {
  XWindowChanges WC;
  EEXTRACT_START(scx_configure_request_event_binding);
  EEXTRACT(4, parent, scx_extract_window);
  EEXTRACT(5, window, scx_extract_window);
  xe->value_mask = scx_extract_window_changes(S48_RECORD_REF(e, 6), &WC);
  xe->x = WC.x;
  xe->y = WC.y;
  xe->width = WC.width;
  xe->height = WC.height;
  xe->border_width = WC.border_width;
  xe->above = WC.sibling;
  xe->detail = WC.stack_mode;
  EEXTRACT_END();
}

void scx_extract_circulate_event(s48_value e, XCirculateEvent* xe) {
  EEXTRACT_START(scx_circulate_event_binding);
  EEXTRACT(4, event, scx_extract_window);
  EEXTRACT(5, window, scx_extract_window);
  EEXTRACT(6, place, scx_extract_place);
  EEXTRACT_END();
}

void scx_extract_circulate_request_event(s48_value e,
					 XCirculateRequestEvent* xe) {
  EEXTRACT_START(scx_circulate_request_event_binding);
  EEXTRACT(4, parent, scx_extract_window);
  EEXTRACT(5, window, scx_extract_window);
  EEXTRACT(6, place, scx_extract_place);
  EEXTRACT_END();
}

void scx_extract_property_event(s48_value e, XPropertyEvent* xe) {
  EEXTRACT_START(scx_property_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, atom, scx_extract_atom);
  EEXTRACT(6, time, scx_extract_time);
  EEXTRACT(7, state, scx_extract_property_state);
  EEXTRACT_END();
}

void scx_extract_selection_clear_event(s48_value e, XSelectionClearEvent* xe) {
  EEXTRACT_START(scx_selection_clear_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, selection, scx_extract_atom);
  EEXTRACT(6, time, scx_extract_time);
  EEXTRACT_END();
}

void scx_extract_selection_request_event(s48_value e,
					 XSelectionRequestEvent* xe) {
  EEXTRACT_START(scx_selection_request_event_binding);
  EEXTRACT(4, owner, scx_extract_window);
  EEXTRACT(5, requestor, scx_extract_window);
  EEXTRACT(6, selection, scx_extract_atom);
  EEXTRACT(7, target, scx_extract_atom);
  EEXTRACT(8, property, scx_extract_atom);
  EEXTRACT(9, time, scx_extract_time);
  EEXTRACT_END();
}

void scx_extract_selection_event(s48_value e, XSelectionEvent* xe) {
  EEXTRACT_START(scx_selection_event_binding);
  EEXTRACT(4, requestor, scx_extract_window);
  EEXTRACT(5, selection, scx_extract_atom);
  EEXTRACT(6, target, scx_extract_atom);
  EEXTRACT(7, property, scx_extract_atom);
  EEXTRACT(8, time, scx_extract_time);
  EEXTRACT_END();
}

void scx_extract_colormap_event(s48_value e, XColormapEvent* xe) {
  EEXTRACT_START(scx_colormap_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, colormap, scx_extract_colormap);
  EEXTRACT(6, new, S48_EXTRACT_BOOLEAN);
  EEXTRACT(7, state, scx_extract_colormap_state);
  EEXTRACT_END();
}

void scx_extract_client_message_event(s48_value e, XClientMessageEvent* xe) {
  int nelements, i;
  char* data; int bytes;
  EEXTRACT_START(scx_client_message_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  scx_extract_property(S48_RECORD_REF(e, 5), &xe->message_type, &xe->format,
		       &data, &nelements);
  if (xe->format == 32) bytes = nelements * 4;
  else if (xe->format == 16) bytes = nelements * 2;
  else bytes = nelements;
  for (i = 0; i < 20; i++)
    if (i < bytes)
      xe->data.b[i] = data[i];
    else
      xe->data.b[i] = 0;
  free(data);
  EEXTRACT_END();
}

void scx_extract_mapping_event(s48_value e, XMappingEvent* xe) {
  EEXTRACT_START(scx_mapping_event_binding);
  EEXTRACT(4, window, scx_extract_window);
  EEXTRACT(5, request, scx_extract_mapping_request);
  EEXTRACT(6, first_keycode, scx_extract_keycode);
  EEXTRACT(7, count, s48_extract_integer);
  EEXTRACT_END();
}

void scx_extract_keymap_event(s48_value e, XKeymapEvent* xe) {
  s48_value temp; int i;
  EEXTRACT_START(scx_keymap_event_binding);
  xe->window = (Window)0;
  temp = S48_RECORD_REF(e, 4);
  for (i = 0; i < 32; i++) {
    int j; char* b = &xe->key_vector[i];
    for (j = 0; j < 8; j++)
      *b |= (s48_extract_fixnum(S48_VECTOR_REF(temp, i*8 + j))
	     == 0 ? 0 : 1) << j;
  }
  EEXTRACT_END();
}

void scx_extract_event(s48_value se, XEvent* e) {
  int t;
  S48_DECLARE_GC_PROTECT_1(se);
  t = scx_extract_event_type(S48_RECORD_REF(se, 0));
  switch (t) {
  case KeyPress : case KeyRelease :
    scx_extract_key_event(se, (XKeyEvent*)e); break;
  case ButtonPress : case ButtonRelease :
    scx_extract_button_event(se, (XButtonEvent*)e); break;
  case MotionNotify :
    scx_extract_motion_event(se, (XMotionEvent*)e); break;
  case EnterNotify : case LeaveNotify :
    scx_extract_crossing_event(se, (XCrossingEvent*)e); break;
  case FocusIn : case FocusOut :
    scx_extract_focus_change_event(se, (XFocusChangeEvent*)e); break;
  case KeymapNotify :
    scx_extract_keymap_event(se, (XKeymapEvent*)e); break;
  case Expose :
    scx_extract_expose_event(se, (XExposeEvent*)e); break;
  case GraphicsExpose :
    scx_extract_graphics_expose_event(se, (XGraphicsExposeEvent*)e); break;
  case NoExpose :
    scx_extract_no_expose_event(se, (XNoExposeEvent*)e); break;
  case VisibilityNotify :
    scx_extract_visibility_event(se, (XVisibilityEvent*)e); break;
  case CreateNotify :
    scx_extract_create_window_event(se, (XCreateWindowEvent*)e); break;
  case DestroyNotify :
    scx_extract_destroy_window_event(se, (XDestroyWindowEvent*)e); break;
  case UnmapNotify :
    scx_extract_unmap_event(se, (XUnmapEvent*)e); break;
  case MapNotify :
    scx_extract_map_event(se, (XMapEvent*)e); break;
  case MapRequest :
    scx_extract_map_request_event(se, (XMapRequestEvent*)e); break;
  case ReparentNotify :
    scx_extract_reparent_event(se, (XReparentEvent*)e); break;
  case ConfigureNotify :
    scx_extract_configure_event(se, (XConfigureEvent*)e); break;
  case ConfigureRequest :
    scx_extract_configure_request_event(se, (XConfigureRequestEvent*)e); break;
  case GravityNotify :
    scx_extract_gravity_event(se, (XGravityEvent*)e); break;
  case ResizeRequest :
    scx_extract_resize_request_event(se, (XResizeRequestEvent*)e); break;
  case CirculateRequest :
    scx_extract_circulate_request_event(se, (XCirculateRequestEvent*)e); break;
  case PropertyNotify :
    scx_extract_property_event(se, (XPropertyEvent*)e); break;
  case SelectionClear :
    scx_extract_selection_clear_event(se, (XSelectionClearEvent*)e); break;
  case SelectionRequest :
    scx_extract_selection_request_event(se, (XSelectionRequestEvent*)e); break;
  case SelectionNotify :
    scx_extract_selection_event(se, (XSelectionEvent*)e); break;
  case ColormapNotify :
    scx_extract_colormap_event(se, (XColormapEvent*)e); break;
  case ClientMessage :
    scx_extract_client_message_event(se, (XClientMessageEvent*)e); break;
  case MappingNotify :
    scx_extract_mapping_event(se, (XMappingEvent*)e); break;
  }
  S48_GC_UNPROTECT();
}

void scx_init_event_types() {
  SCX_PRO_IMP(scx_key_event_binding, "scx-key-event");
  SCX_PRO_IMP(scx_button_event_binding, "scx-button-event");
  SCX_PRO_IMP(scx_motion_event_binding, "scx-motion-event");
  SCX_PRO_IMP(scx_crossing_event_binding, "scx-crossing-event");
  SCX_PRO_IMP(scx_focus_change_event_binding, "scx-focus-change-event");
  SCX_PRO_IMP(scx_expose_event_binding, "scx-expose-event");
  SCX_PRO_IMP(scx_graphics_expose_event_binding, "scx-graphics-expose-event");
  SCX_PRO_IMP(scx_no_expose_event_binding, "scx-no-expose-event");
  SCX_PRO_IMP(scx_visibility_event_binding, "scx-visibility-event");
  SCX_PRO_IMP(scx_create_window_event_binding, "scx-create-window-event");
  SCX_PRO_IMP(scx_destroy_window_event_binding, "scx-destroy-window-event");
  SCX_PRO_IMP(scx_unmap_event_binding, "scx-unmap-event");
  SCX_PRO_IMP(scx_map_event_binding, "scx-map-event");
  SCX_PRO_IMP(scx_map_request_event_binding, "scx-map-request-event");
  SCX_PRO_IMP(scx_reparent_event_binding, "scx-reparent-event");
  SCX_PRO_IMP(scx_configure_event_binding, "scx-configure-event");
  SCX_PRO_IMP(scx_gravity_event_binding, "scx-gravity-event");
  SCX_PRO_IMP(scx_resize_request_event_binding, "scx-resize-request-event");
  SCX_PRO_IMP(scx_configure_request_event_binding,
	      "scx-configure-request-event");
  SCX_PRO_IMP(scx_circulate_event_binding, "scx-circulate-event");
  SCX_PRO_IMP(scx_circulate_event_binding, "scx-circulate-request-event");
  SCX_PRO_IMP(scx_property_event_binding, "scx-property-event");
  SCX_PRO_IMP(scx_selection_clear_event_binding, "scx-selection-clear-event");
  SCX_PRO_IMP(scx_selection_request_event_binding,
	      "scx-selection-request-event");
  SCX_PRO_IMP(scx_selection_event_binding, "scx-selection-event");
  SCX_PRO_IMP(scx_colormap_event_binding, "scx-colormap-event");
  SCX_PRO_IMP(scx_client_message_event_binding, "scx-client-message-event");
  SCX_PRO_IMP(scx_mapping_event_binding, "scx-mapping-event");
  SCX_PRO_IMP(scx_keymap_event_binding, "scx-keymap-event");
}
