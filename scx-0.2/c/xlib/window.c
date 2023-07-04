/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_set_window_attribute_binding = S48_FALSE;
#define scx_extract_set_window_attribute(x) \
  S48_EXTRACT_ENUM(x, scx_set_window_attribute_binding)

static unsigned long
scx_extract_set_window_attribute_alist(s48_value attribs, 
				       XSetWindowAttributes* Xattrs) {
  unsigned long mask = 0;
  s48_value v = S48_FALSE;
  S48_DECLARE_GC_PROTECT_2(attribs, v);
  while (attribs != S48_NULL) {
    int mv = scx_extract_set_window_attribute(S48_CAR(S48_CAR(attribs)));
    v = S48_CDR(S48_CAR(attribs));
    attribs = S48_CDR(attribs);
    mask = mask | (1L << mv);
    switch (1L << mv) {
    case CWBackPixmap:
      Xattrs->background_pixmap = scx_extract_pixmap(v); break;
    case CWBackPixel:
      Xattrs->background_pixel = scx_extract_pixel(v); break;
    case CWBorderPixmap:
      Xattrs->border_pixmap = scx_extract_pixmap(v); break;
    case CWBorderPixel:
      Xattrs->border_pixel = scx_extract_pixel(v); break;
    case CWBitGravity:
      Xattrs->bit_gravity = scx_extract_bit_gravity(v); break;
    case CWWinGravity:
      Xattrs->win_gravity = scx_extract_win_gravity(v); break;
    case CWBackingStore:
      Xattrs->backing_store = scx_extract_backing_store(v); break;
    case CWBackingPlanes:
      Xattrs->backing_planes = s48_extract_integer(v); break;
    case CWBackingPixel:
      Xattrs->backing_pixel = scx_extract_pixel(v); break;
    case CWOverrideRedirect:
      Xattrs->override_redirect = S48_EXTRACT_BOOLEAN(v); break;
    case CWSaveUnder:
      Xattrs->save_under = S48_EXTRACT_BOOLEAN(v); break;
    case CWEventMask:
      Xattrs->event_mask = scx_extract_event_mask(v); break;
    case CWDontPropagate:
      Xattrs->do_not_propagate_mask = scx_extract_event_mask(v); break;
    case CWColormap:
      Xattrs->colormap = scx_extract_colormap(v); break; 
    case CWCursor:
      Xattrs->cursor = scx_extract_cursor(v); break;
    }
  }
  S48_GC_RETURN(mask);
}

s48_value scx_Create_Window (s48_value display, s48_value parent, 
			     s48_value x, s48_value y, 
			     s48_value width, s48_value height, 
			     s48_value border_width, s48_value depth, 
			     s48_value class, s48_value visual, 
			     s48_value attribs) {
  Window win;
  XSetWindowAttributes Xattrs;
  unsigned long mask;
  S48_DECLARE_GC_PROTECT_11(display, parent, x, y, width, height, border_width,
			    depth, class, visual, attribs);
  mask = scx_extract_set_window_attribute_alist(attribs, &Xattrs);
  
  win = XCreateWindow(scx_extract_display(display), 
		      scx_extract_window(parent), 
		      (int)s48_extract_integer(x),
		      (int)s48_extract_integer(y),
		      (int)s48_extract_integer(width),
		      (int)s48_extract_integer(height), 
		      (int)s48_extract_integer(border_width),
		      s48_extract_integer(depth),
		      scx_extract_window_class(class),
		      scx_extract_visual(visual), mask, &Xattrs);
  S48_GC_RETURN(scx_enter_window(win));
}

s48_value scx_Create_Simple_Window(s48_value display, s48_value parent,
				   s48_value x, s48_value y, 
				   s48_value width, s48_value height, 
				   s48_value border_width, s48_value border, 
				   s48_value background) {
  Window win;
  S48_DECLARE_GC_PROTECT_9(display, parent, x, y, width, height, border_width,
			   border, background);
  win = XCreateSimpleWindow(scx_extract_display(display),
			    scx_extract_window(parent),
			    (int)s48_extract_integer(x),
			    (int)s48_extract_integer(y),
			    (int)s48_extract_integer(width),
			    (int)s48_extract_integer(height), 
			    (int)s48_extract_integer(border_width),
			    scx_extract_pixel(border),
			    scx_extract_pixel(background));
  S48_GC_RETURN(scx_enter_window(win));
}

s48_value scx_Change_Window_Attributes(s48_value display, s48_value window,
				       s48_value attribs) {
  XSetWindowAttributes Xattrs;
  unsigned long mask;
  S48_DECLARE_GC_PROTECT_3(display, window, attribs);
  mask = scx_extract_set_window_attribute_alist(attribs, &Xattrs);
  XChangeWindowAttributes(scx_extract_display(display),
			  scx_extract_window(window), 
			  mask, &Xattrs);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_enter_window_changes(XWindowChanges* WC, unsigned long mask) {
  s48_value res = S48_NULL, t = S48_FALSE;
  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2(res, t);
  
  if (mask & CWX) {
    t = scx_enter_window_change(0);
    t = s48_cons(t, s48_enter_integer(WC->x));
    res = s48_cons(t, res);
  }
  if (mask & CWY) {
    t = scx_enter_window_change(1);
    t = s48_cons(t, s48_enter_integer(WC->y));
    res = s48_cons(t, res);
  }
  if (mask & CWWidth) {
    t = scx_enter_window_change(2);
    t = s48_cons(t, s48_enter_integer(WC->width));
    res = s48_cons(t, res);
  }
  if (mask & CWHeight) {
    t = scx_enter_window_change(3);
    t = s48_cons(t, s48_enter_integer(WC->height));
    res = s48_cons(t, res);
  }
  if (mask & CWBorderWidth) {
    t = scx_enter_window_change(4);
    t = s48_cons(t, s48_enter_integer(WC->border_width));
    res = s48_cons(t, res);
  }
  if (mask & CWSibling) {
    t = scx_enter_window_change(5);
    t = s48_cons(t, scx_enter_window(WC->sibling));
    res = s48_cons(t, res);
  }
  if (mask & CWStackMode) {
    t = scx_enter_window_change(6);
    t = s48_cons(t, scx_enter_stack_mode(WC->stack_mode));
    res = s48_cons(t, res);
  }
  S48_GC_UNPROTECT();
  return res;
}

unsigned long scx_extract_window_changes(s48_value changes,
					 XWindowChanges* WC) {
  unsigned long mask = 0;
  s48_value v = S48_FALSE;
  S48_DECLARE_GC_PROTECT_2(changes, v);
  while (changes != S48_NULL) {
    int mv = scx_extract_window_change(S48_CAR(S48_CAR(changes)));
    v = S48_CDR(S48_CAR(changes));
    changes = S48_CDR(changes);
    mask = mask | (1L << mv);
    switch (1L << mv) {
    case CWX:
      WC->x = s48_extract_integer(v); break;
    case CWY:
      WC->y = s48_extract_integer(v); break;
    case CWWidth:
      WC->width = s48_extract_integer(v); break;
    case CWHeight:
      WC->height = s48_extract_integer(v); break;
    case CWBorderWidth:
      WC->border_width = s48_extract_integer(v); break;
    case CWSibling:
      WC->sibling = scx_extract_window(v); break;
    case CWStackMode:
      WC->stack_mode = scx_extract_stack_mode(v); break;
    }
  }
  S48_GC_RETURN(mask);
}

s48_value scx_Configure_Window(s48_value display, s48_value window, 
			       s48_value changes) {
  XWindowChanges WC;
  unsigned long mask;
  S48_DECLARE_GC_PROTECT_3(display, window, changes);
  mask = scx_extract_window_changes(changes, &WC);

  XConfigureWindow(scx_extract_display(display), scx_extract_window(window),
		   mask, &WC);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_window_attibutes_binding = S48_FALSE;

s48_value scx_enter_window_attributes(XWindowAttributes* WA) {
  s48_value v = s48_make_record(scx_window_attibutes_binding);
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(v);
  S48_RECORD_SET(v, 0, s48_enter_integer(WA->x));
  S48_RECORD_SET(v, 1, s48_enter_integer(WA->y));
  S48_RECORD_SET(v, 2, s48_enter_integer(WA->width));
  S48_RECORD_SET(v, 3, s48_enter_integer(WA->height));
  S48_RECORD_SET(v, 4, s48_enter_integer(WA->border_width));
  S48_RECORD_SET(v, 5, s48_enter_integer(WA->depth));
  S48_RECORD_SET(v, 6, scx_enter_visual(WA->visual));
  S48_RECORD_SET(v, 7, scx_enter_window(WA->root));
  S48_RECORD_SET(v, 8, scx_enter_window_class(WA->class));
  S48_RECORD_SET(v, 9, scx_enter_bit_gravity(WA->bit_gravity));
  S48_RECORD_SET(v, 10, scx_enter_win_gravity(WA->win_gravity));
  S48_RECORD_SET(v, 11, scx_enter_backing_store(WA->backing_store));
  S48_RECORD_SET(v, 12, s48_enter_integer(WA->backing_planes));
  S48_RECORD_SET(v, 13, scx_enter_pixel(WA->backing_pixel));
  S48_RECORD_SET(v, 14, S48_ENTER_BOOLEAN(WA->save_under));
  S48_RECORD_SET(v, 15, scx_enter_colormap(WA->colormap));
  S48_RECORD_SET(v, 16, S48_ENTER_BOOLEAN(WA->map_installed));
  S48_RECORD_SET(v, 17, scx_enter_map_state(WA->map_state));
  S48_RECORD_SET(v, 18, scx_enter_event_mask(WA->all_event_masks));
  S48_RECORD_SET(v, 19, scx_enter_event_mask(WA->your_event_mask));
  S48_RECORD_SET(v, 20, scx_enter_event_mask(WA->do_not_propagate_mask));
  S48_RECORD_SET(v, 21, S48_ENTER_BOOLEAN(WA->override_redirect));
  S48_RECORD_SET(v, 22, scx_enter_screen(WA->screen));
  S48_GC_UNPROTECT();
  return v;
}

s48_value scx_Get_Window_Attributes(s48_value display, s48_value window) {
  XWindowAttributes WA;
  S48_DECLARE_GC_PROTECT_2(display, window);
  if (!XGetWindowAttributes(scx_extract_display(display),
			    scx_extract_window(window),
			    &WA))
    S48_GC_RETURN(S48_FALSE);
  else
    S48_GC_RETURN(scx_enter_window_attributes(&WA));
}

s48_value scx_Get_Geometry(s48_value display, s48_value drawable) {
  s48_value v = S48_FALSE;
  Window root;
  int x, y;
  unsigned int width, height, border_width, depth;
  S48_DECLARE_GC_PROTECT_3(display, drawable, v);

  if (!XGetGeometry(scx_extract_display(display),
		    scx_extract_drawable(drawable),
		    &root, &x, &y, &width, &height, &border_width, &depth))
    S48_GC_RETURN(S48_FALSE);
  else {
    v = s48_make_vector(7, S48_FALSE);
    S48_VECTOR_SET(v, 0, scx_enter_window(root));
    S48_VECTOR_SET(v, 1, s48_enter_fixnum(x));
    S48_VECTOR_SET(v, 2, s48_enter_fixnum(y));
    S48_VECTOR_SET(v, 3, s48_enter_fixnum(width));
    S48_VECTOR_SET(v, 4, s48_enter_fixnum(height));
    S48_VECTOR_SET(v, 5, s48_enter_fixnum(border_width));
    S48_VECTOR_SET(v, 6, s48_enter_fixnum(depth));
    S48_GC_RETURN(v);
  }
}

s48_value scx_Map_Window(s48_value display, s48_value window) {
  S48_DECLARE_GC_PROTECT_2(display, window);
  XMapWindow(scx_extract_display(display), scx_extract_window(window));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Map_Raised(s48_value display, s48_value window) {
  S48_DECLARE_GC_PROTECT_2(display, window);
  XMapRaised(scx_extract_display(display), scx_extract_window(window));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Map_Subwindows(s48_value display, s48_value window) {
  S48_DECLARE_GC_PROTECT_2(display, window);
  XMapSubwindows(scx_extract_display(display), scx_extract_window(window));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Unmap_Window(s48_value display, s48_value window) {
  S48_DECLARE_GC_PROTECT_2(display, window);
  XUnmapWindow(scx_extract_display(display), scx_extract_window(window));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Unmap_Subwindows(s48_value display, s48_value window) {
  S48_DECLARE_GC_PROTECT_2(display, window);
  XUnmapSubwindows(scx_extract_display(display), scx_extract_window(window));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Destroy_Window(s48_value display, s48_value window) {
  S48_DECLARE_GC_PROTECT_2(display, window);
  XDestroyWindow(scx_extract_display(display), scx_extract_window(window));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Destroy_Subwindows(s48_value display, s48_value window) {
  S48_DECLARE_GC_PROTECT_2(display, window);
  XDestroySubwindows(scx_extract_display(display),
		     scx_extract_window(window));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Raise_Window(s48_value display, s48_value window) {
  S48_DECLARE_GC_PROTECT_2(display, window);
  XRaiseWindow(scx_extract_display(display),
	       scx_extract_window(window));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Lower_Window(s48_value display, s48_value window) {
  S48_DECLARE_GC_PROTECT_2(display, window);
  XLowerWindow(scx_extract_display(display),
	       scx_extract_window(window));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_circulate_direction_binding = S48_FALSE;
#define scx_extract_direction(x) \
  S48_EXTRACT_ENUM(x, scx_circulate_direction_binding)

s48_value scx_Circulate_Subwindows(s48_value display, s48_value window,
				   s48_value dir) {
  S48_DECLARE_GC_PROTECT_3(display, window, dir);
  XCirculateSubwindows(scx_extract_display(display),
		       scx_extract_window(window), 
		       scx_extract_direction(dir));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Restack_Windows(s48_value display, s48_value windows) {
  int i, n = s48_list_length(windows);
  Window wins[n];
  S48_DECLARE_GC_PROTECT_2(display, windows);
  for (i = n-1; i >= 0; i--) {
    wins[i] = scx_extract_window(S48_CAR(windows));
    windows = S48_CDR(windows);
  }
  XRestackWindows(scx_extract_display(display),
		  wins, n);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Clear_Area(s48_value display, s48_value window,
			 s48_value x, s48_value y,
			 s48_value width, s48_value height,
			 s48_value exposures) {
  S48_DECLARE_GC_PROTECT_7(display, window, x, y, window, height, exposures);
  XClearArea(scx_extract_display(display), scx_extract_window(window),
	     s48_extract_integer(x), s48_extract_integer(y),
	     s48_extract_integer(width), s48_extract_integer(height),
	     S48_EXTRACT_BOOLEAN(exposures));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Clear_Window(s48_value display, s48_value window) {
  S48_DECLARE_GC_PROTECT_2(display, window);
  XClearWindow(scx_extract_display(display),
	       scx_extract_window(window));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Query_Tree(s48_value display, s48_value window) {
  Window root, parent, *children;
  int i;
  unsigned n;
  s48_value c = S48_NULL, res = S48_NULL;
  S48_DECLARE_GC_PROTECT_4(display, window, c, res);
  
  if (! XQueryTree (scx_extract_display(display),
		    scx_extract_window(window), 
		    &root, &parent, &children, &n))
    S48_GC_RETURN(S48_FALSE);

  for (i = 0; i < n; i++)
    c = s48_cons(scx_enter_window(children[i]), c);
  if (children != NULL) XFree(children);

  res = s48_cons(c, res);
  res = s48_cons(scx_enter_window(parent), res);
  res = s48_cons(scx_enter_window(root), res);

  S48_GC_RETURN(res);
}

s48_value scx_Translate_Coordinates(s48_value display, s48_value src,
				    s48_value dest,
				    s48_value x, s48_value y) {
  int rx, ry;
  Window child;
  s48_value res = S48_NULL;
  S48_DECLARE_GC_PROTECT_6(display, src, dest, x, y, res);

  if (!XTranslateCoordinates(scx_extract_display(display), 
			     scx_extract_window(src), 
			     scx_extract_window(dest),
			     (int)s48_extract_integer (x), 
			     (int)s48_extract_integer (y), 
			     &rx, &ry, &child))
    S48_GC_RETURN(S48_FALSE);

  res = s48_cons(scx_enter_window(child), res);
  res = s48_cons(s48_enter_fixnum(ry), res);
  res = s48_cons(s48_enter_fixnum(rx), res);

  S48_GC_RETURN(res);
}

s48_value scx_Query_Pointer(s48_value display, s48_value window) {
  s48_value v = S48_FALSE;
  Bool ret;
  Window root, child;
  int r_x, r_y, x, y;
  unsigned int mask;
  S48_DECLARE_GC_PROTECT_3(display, window, v);
  
  ret = XQueryPointer(scx_extract_display(display), scx_extract_window(window),
		      &root, &child, &r_x, &r_y, &x, &y, &mask);

  v = s48_make_vector(8, S48_FALSE);

  S48_VECTOR_SET(v, 0, scx_enter_window(root));
  S48_VECTOR_SET(v, 1, scx_enter_window(child));
  S48_VECTOR_SET(v, 2, s48_enter_fixnum(r_x));
  S48_VECTOR_SET(v, 3, s48_enter_fixnum(r_y));
  S48_VECTOR_SET(v, 4, s48_enter_fixnum(x));
  S48_VECTOR_SET(v, 5, s48_enter_fixnum(y));
  S48_VECTOR_SET(v, 6, s48_enter_integer((unsigned long)mask));
  S48_VECTOR_SET(v, 7, ret ? S48_TRUE : S48_FALSE);

  S48_GC_RETURN(v);
}

void scx_init_window(void) {
  SCX_PRO_IMP(scx_set_window_attribute_binding, "scx-set-window-attribute");
  SCX_PRO_IMP(scx_window_attibutes_binding, "scx-window-attributes");
  SCX_PRO_IMP(scx_circulate_direction_binding, "scx-circulate-direction");

  S48_EXPORT_FUNCTION(scx_Create_Window);
  S48_EXPORT_FUNCTION(scx_Create_Simple_Window);
  S48_EXPORT_FUNCTION(scx_Change_Window_Attributes);
  S48_EXPORT_FUNCTION(scx_Configure_Window);
  S48_EXPORT_FUNCTION(scx_Get_Window_Attributes);
  S48_EXPORT_FUNCTION(scx_Get_Geometry);
  S48_EXPORT_FUNCTION(scx_Map_Window);
  S48_EXPORT_FUNCTION(scx_Map_Raised);
  S48_EXPORT_FUNCTION(scx_Map_Subwindows);
  S48_EXPORT_FUNCTION(scx_Unmap_Window);
  S48_EXPORT_FUNCTION(scx_Unmap_Subwindows);  
  S48_EXPORT_FUNCTION(scx_Destroy_Window);
  S48_EXPORT_FUNCTION(scx_Destroy_Subwindows);
  S48_EXPORT_FUNCTION(scx_Raise_Window);
  S48_EXPORT_FUNCTION(scx_Lower_Window);
  S48_EXPORT_FUNCTION(scx_Circulate_Subwindows);
  S48_EXPORT_FUNCTION(scx_Restack_Windows);
  S48_EXPORT_FUNCTION(scx_Clear_Area);
  S48_EXPORT_FUNCTION(scx_Clear_Window);
  S48_EXPORT_FUNCTION(scx_Query_Tree);
  S48_EXPORT_FUNCTION(scx_Translate_Coordinates);
  S48_EXPORT_FUNCTION(scx_Query_Pointer);
}
