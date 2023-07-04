/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_screen_format_binding = S48_FALSE;

s48_value scx_enter_screenformat(ScreenFormat* sf) {
  s48_value res = s48_make_record(scx_screen_format_binding);
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(res);
  S48_RECORD_SET(res, 0, s48_enter_integer(sf->depth));
  S48_RECORD_SET(res, 1, s48_enter_integer(sf->bits_per_pixel));
  S48_RECORD_SET(res, 2, s48_enter_integer(sf->scanline_pad));
  S48_GC_UNPROTECT();
  return res;
}

s48_value scx_screen_list = S48_NULL;

s48_value scx_enter_screen(Screen* scr) {
  s48_value s = scx_struct_cache_ref(scr, scx_screen_list);
  if (s == S48_FALSE) {
    S48_DECLARE_GC_PROTECT(1);
    s = s48_make_record(scx_screen);
    S48_GC_PROTECT_1(s);
    scx_struct_cache_set(scr, &scx_screen_list, s);

    S48_RECORD_SET(s, 0, s48_enter_integer((long)scr));
    S48_RECORD_SET(s, 1, scx_enter_display(DisplayOfScreen(scr)));
    S48_RECORD_SET(s, 2, scx_enter_window(RootWindowOfScreen(scr)));
    S48_RECORD_SET(s, 3, s48_enter_integer(WidthOfScreen(scr)));
    S48_RECORD_SET(s, 4, s48_enter_integer(HeightOfScreen(scr)));
    S48_RECORD_SET(s, 5, s48_enter_integer(WidthMMOfScreen(scr)));
    S48_RECORD_SET(s, 6, s48_enter_integer(HeightMMOfScreen(scr)));
    S48_RECORD_SET(s, 7, s48_enter_integer(XScreenNumberOfScreen(scr)));
    S48_RECORD_SET(s, 8, s48_enter_integer(DefaultDepthOfScreen(scr)));
    S48_RECORD_SET(s, 9, scx_enter_visual(DefaultVisualOfScreen(scr)));
    S48_RECORD_SET(s, 10, scx_enter_gc(DefaultGCOfScreen(scr)));
    S48_RECORD_SET(s, 11, scx_enter_colormap(DefaultColormapOfScreen(scr)));
    S48_RECORD_SET(s, 12, scx_enter_pixel(WhitePixelOfScreen(scr)));
    S48_RECORD_SET(s, 13, scx_enter_pixel(BlackPixelOfScreen(scr)));
    S48_RECORD_SET(s, 14, s48_enter_integer(MaxCmapsOfScreen(scr)));
    S48_RECORD_SET(s, 15, s48_enter_integer(MinCmapsOfScreen(scr)));
    S48_RECORD_SET(s, 16, scx_enter_backing_store(DoesBackingStore(scr)));
    S48_RECORD_SET(s, 17, S48_ENTER_BOOLEAN(DoesSaveUnders(scr)));
    S48_RECORD_SET(s, 18, scx_enter_event_mask(EventMaskOfScreen(scr)));
    S48_GC_UNPROTECT();
  }
  return s;
}

s48_value scx_Screen_Number_Of_Screen(s48_value screen) {
  Screen* sp = (Screen*)s48_extract_integer(S48_RECORD_REF(screen, 0));
  int num = XScreenNumberOfScreen(sp);
  return s48_enter_integer(num);
}

s48_value scx_display_list = S48_NULL;
s48_value scx_general_after_function_binding = S48_FALSE;

static int scx_after_function_wrapper(Display* dpy) {
  s48_value display = scx_enter_display(dpy);
  s48_value fun = S48_SHARED_BINDING_REF(scx_general_after_function_binding);

  s48_call_scheme(fun, 1, display);
  return 0;
}

s48_value scx_initialize_display_binding = S48_FALSE;

s48_value scx_enter_display(Display* dpy) {
  s48_value d = scx_struct_cache_ref(dpy, scx_display_list);
  if (d == S48_FALSE) {
    int i;
    s48_value l = S48_NULL;
    S48_DECLARE_GC_PROTECT(2);
    d = s48_make_record(scx_display);
    S48_GC_PROTECT_2(d, l);

    /* have to do this first, because screens want to reference their
       display */
    scx_struct_cache_set(dpy, &scx_display_list, d);

    S48_RECORD_SET(d, 0, s48_enter_integer((long)dpy));
    S48_RECORD_SET(d, 1, s48_enter_integer(ConnectionNumber(dpy)));
    S48_RECORD_SET(d, 2, s48_enter_integer(ProtocolVersion(dpy)));
    S48_RECORD_SET(d, 3, s48_enter_integer(ProtocolRevision(dpy)));
    S48_RECORD_SET(d, 4, s48_enter_string(ServerVendor(dpy)));
    S48_RECORD_SET(d, 5, scx_enter_byte_order(ImageByteOrder(dpy)));
    S48_RECORD_SET(d, 6, s48_enter_integer(BitmapUnit(dpy)));
    S48_RECORD_SET(d, 7, scx_enter_bit_order(BitmapBitOrder(dpy)));
    S48_RECORD_SET(d, 8, s48_enter_integer(BitmapPad(dpy)));
    S48_RECORD_SET(d, 9, s48_enter_integer(VendorRelease(dpy)));
    S48_RECORD_SET(d, 10, s48_enter_integer(QLength(dpy)));
    S48_RECORD_SET(d, 11, s48_enter_string(DisplayString(dpy)));
    S48_RECORD_SET(d, 12,
		   scx_enter_screen(ScreenOfDisplay(dpy, DefaultScreen(dpy))));
    for (i = ScreenCount(dpy)-1; i >= 0; i--)
      l = s48_cons(scx_enter_screen(ScreenOfDisplay(dpy, i)), l);
    S48_RECORD_SET(d, 13, l);
    S48_RECORD_SET(d, 14, S48_FALSE); /* the after-function */
    XSetAfterFunction(dpy, &scx_after_function_wrapper);
    S48_RECORD_SET(d, 15, S48_FALSE); /* wakeup-port, set by initialize */
    S48_RECORD_SET(d, 16, S48_TRUE); /* use-warnings? */
    S48_RECORD_SET(d, 17, S48_FALSE); /* error-queue, set by initialize */
    s48_call_scheme(S48_SHARED_BINDING_REF(scx_initialize_display_binding),
		    1, d);
    S48_GC_UNPROTECT();
  }
  return d;
}

s48_value scx_Open_Display (s48_value name) {
  Display* res = XOpenDisplay(s48_extract_string(name));
  if (res == NULL)
    return S48_FALSE;
  else
    return scx_enter_display(res);
}

s48_value scx_Close_Display(s48_value display) {
  XCloseDisplay(scx_extract_display(display));
  return S48_UNSPECIFIC;
}

s48_value scx_Display_Last_Request_Read(s48_value display) {
  Display* d = scx_extract_display(display);
  return s48_enter_integer(LastKnownRequestProcessed(d));
}

s48_value scx_Next_Request(s48_value display) {
  Display* d = scx_extract_display(display);
  return s48_enter_integer(NextRequest(d));
}

s48_value scx_Display_Flush(s48_value display) {
  XFlush(scx_extract_display(display));
  return S48_UNSPECIFIC;
}

s48_value scx_Display_Sync(s48_value display, s48_value discard) {
  S48_DECLARE_GC_PROTECT_2(display, discard);
  XSync(scx_extract_display(display), S48_EXTRACT_BOOLEAN(discard));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_No_Op(s48_value display) {
  XNoOp(scx_extract_display(display));
  return S48_UNSPECIFIC;
}

s48_value scx_Display_Select_Input(s48_value display, s48_value window, 
				   s48_value event_mask) {
  S48_DECLARE_GC_PROTECT_3(display, window, event_mask);
  XSelectInput(scx_extract_display(display),
	       scx_extract_window(window),
	       scx_extract_event_mask(event_mask));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

void scx_init_display(void) {
  SCX_PRO_IMP(scx_screen_format_binding, "scx-screen-format");
  SCX_PRO_IMP(scx_general_after_function_binding,
	      "scx-general-after-function");
  SCX_PRO_IMP(scx_initialize_display_binding, "scx-initialize-display");
  S48_GC_PROTECT_GLOBAL(scx_display_list);
  S48_GC_PROTECT_GLOBAL(scx_screen_list);

  S48_EXPORT_FUNCTION(scx_Screen_Number_Of_Screen);
  S48_EXPORT_FUNCTION(scx_Open_Display);
  S48_EXPORT_FUNCTION(scx_Close_Display);
  S48_EXPORT_FUNCTION(scx_Display_Last_Request_Read);
  S48_EXPORT_FUNCTION(scx_Next_Request);
  S48_EXPORT_FUNCTION(scx_Display_Flush);
  S48_EXPORT_FUNCTION(scx_Display_Sync);
  S48_EXPORT_FUNCTION(scx_No_Op);
  S48_EXPORT_FUNCTION(scx_Display_Select_Input);
}
