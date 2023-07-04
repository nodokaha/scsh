/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_Create_Pixmap_Cursor(s48_value display,
				   s48_value src, s48_value mask,
				   s48_value foreground,
				   s48_value background,
				   s48_value x, s48_value y) {
  XColor f, b;
  Cursor xc;
  S48_DECLARE_GC_PROTECT_7(display, src, mask, foreground, background, x, y);
  scx_extract_color(foreground, &f);
  scx_extract_color(background, &b);
  xc = XCreatePixmapCursor(scx_extract_display(display),
			   scx_extract_pixmap(src),
			   scx_extract_pixmap(mask),
			   &f, &b,
			   s48_extract_integer(x),
			   s48_extract_integer(y));
  S48_GC_RETURN(scx_enter_cursor(xc));
}

s48_value scx_Create_Glyph_Cursor(s48_value display,
				  s48_value src_font, s48_value mask_font,
				  s48_value srcc, s48_value maskc,
				  s48_value foreground, s48_value background) {
  XColor f, b;
  Cursor xc;
  S48_DECLARE_GC_PROTECT_7(display, src_font, mask_font, srcc, maskc,
			   foreground, background);
  scx_extract_color(foreground, &f);
  scx_extract_color(background, &b);
  xc = XCreateGlyphCursor(scx_extract_display(display),
			  scx_extract_font(src_font),
			  scx_extract_font(mask_font),
			  s48_extract_integer(srcc),
			  s48_extract_integer(maskc),
			  &f, &b);
  S48_GC_RETURN(scx_enter_cursor(xc));
}

s48_value scx_Create_Font_Cursor(s48_value display, s48_value shape) {
  Cursor xc;
  S48_DECLARE_GC_PROTECT_2(display, shape);
  xc = XCreateFontCursor(scx_extract_display(display),
			 s48_extract_integer(shape));
  S48_GC_RETURN(scx_enter_cursor(xc));
}

s48_value scx_Define_Cursor(s48_value display, s48_value window,
			    s48_value cursor) {
  S48_DECLARE_GC_PROTECT_3(display, window, cursor);
  XDefineCursor(scx_extract_display(display), scx_extract_window(window),
		scx_extract_cursor(cursor));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Undefine_Cursor(s48_value display, s48_value window) {
  S48_DECLARE_GC_PROTECT_2(display, window);
  XUndefineCursor(scx_extract_display(display),
		  scx_extract_window(window));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Recolor_Cursor(s48_value display, s48_value cursor,
			     s48_value foreground, s48_value background) {
  XColor f, b;
  S48_DECLARE_GC_PROTECT_4(display, cursor, foreground, background);
  scx_extract_color(foreground, &f);
  scx_extract_color(background, &b);
  XRecolorCursor(scx_extract_display(display),
		 scx_extract_cursor(cursor),
		 &f, &b);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Free_Cursor(s48_value display, s48_value cursor) {
  S48_DECLARE_GC_PROTECT_2(display, cursor);
  XFreeCursor(scx_extract_display(display),
	      scx_extract_cursor(cursor));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

void scx_init_cursor(void) {
  S48_EXPORT_FUNCTION(scx_Create_Pixmap_Cursor);
  S48_EXPORT_FUNCTION(scx_Create_Glyph_Cursor);
  S48_EXPORT_FUNCTION(scx_Create_Font_Cursor);
  S48_EXPORT_FUNCTION(scx_Define_Cursor);
  S48_EXPORT_FUNCTION(scx_Undefine_Cursor);
  S48_EXPORT_FUNCTION(scx_Recolor_Cursor);
  S48_EXPORT_FUNCTION(scx_Free_Cursor);
}
