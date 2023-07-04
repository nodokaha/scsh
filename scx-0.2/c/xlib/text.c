/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_Draw_Image_String(s48_value display, s48_value drawable,
				s48_value gc, s48_value x, s48_value y,
				s48_value string) {
  S48_DECLARE_GC_PROTECT_6(display, drawable, gc, x, y, string);
  XDrawImageString(scx_extract_display(display),
		   scx_extract_drawable(drawable),
		   scx_extract_gc(gc),
		   s48_extract_integer(x),
		   s48_extract_integer(y),
		   s48_extract_string(string),
		   S48_STRING_LENGTH(string));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Draw_Image_String_16(s48_value display, s48_value drawable,
				   s48_value gc, s48_value x, s48_value y,
				   s48_value string) {
  int i, len = s48_list_length(string);
  XChar2b chars[len];
  S48_DECLARE_GC_PROTECT_6(display, drawable, gc, x, y, string);
  for (i = 0; i < len; i++) {
    chars[i].byte1 = s48_extract_char(S48_CAR(S48_CAR(string)));
    chars[i].byte2 = s48_extract_char(S48_CDR(S48_CAR(string)));
    string = S48_CDR(string);
  }
  XDrawImageString16(scx_extract_display(display),
		     scx_extract_drawable(drawable),
		     scx_extract_gc(gc),
		     s48_extract_integer(x),
		     s48_extract_integer(y),
		     chars, len);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_text_item = S48_FALSE;

void scx_extract_text_item(s48_value v, XTextItem* ti) {
  S48_DECLARE_GC_PROTECT_1(v);
  s48_check_record_type(v, scx_text_item);
  if (S48_RECORD_REF(v, 0) != S48_FALSE) {
    ti->nchars = S48_STRING_LENGTH(S48_RECORD_REF(v, 0));
    ti->chars = (char*)malloc(ti->nchars);
    strncpy(ti->chars, s48_extract_string(S48_RECORD_REF(v, 0)), ti->nchars);
  } else {
    ti->chars = NULL;
    ti->nchars = 0;
  }
  ti->delta = s48_extract_integer(S48_RECORD_REF(v, 1));
  ti->font = scx_extract_font(S48_RECORD_REF(v, 2));
  S48_GC_UNPROTECT();
  /* Free all chars arrays! */
}

void scx_extract_text_item_16(s48_value v, XTextItem16* ti) {
  s48_value l = S48_NULL;
  S48_DECLARE_GC_PROTECT_2(v, l);
  s48_check_record_type(v, scx_text_item);
  {
    if (S48_RECORD_REF(v, 0) != S48_FALSE) {
      int i, n;
      XChar2b* s;
      l = S48_RECORD_REF(v, 0);
      n = s48_list_length(l);
      s = (XChar2b*)malloc(n * sizeof(XChar2b));
      for (i = n-1; i >= 0; i--) {
	ti->chars[i].byte1 = s48_extract_integer(S48_CAR(S48_CAR(l)));
	ti->chars[i].byte2 = s48_extract_integer(S48_CDR(S48_CAR(l)));
      }
      ti->nchars = n;
    } else {
      ti->chars = NULL;
      ti->nchars = 0;
    }
    ti->delta = s48_extract_integer(S48_RECORD_REF(v, 1));
    ti->font = scx_extract_font(S48_RECORD_REF(v, 2));
  }
  S48_GC_UNPROTECT();
  /* Free all chars arrays! */
}

s48_value scx_Draw_Text(s48_value display, s48_value drawable,
			s48_value gc, s48_value x, s48_value y,
			s48_value items) {
  int i, len = s48_list_length(items);
  XTextItem parts[len];
  S48_DECLARE_GC_PROTECT_6(display, drawable, gc, x, y, items);
  for (i = 0; i < len; i++) {
    scx_extract_text_item(S48_CAR(items), &parts[i]);
    items = S48_CDR(items);
  }
  XDrawText(scx_extract_display(display),
	    scx_extract_drawable(drawable),
	    scx_extract_gc(gc),
	    s48_extract_integer(x),
	    s48_extract_integer(y),
	    parts, len);
  for (i = 0; i < len; i++)
    free(parts[i].chars);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Draw_Text_16(s48_value display, s48_value drawable,
			   s48_value gc, s48_value x, s48_value y,
			   s48_value items) {
  int i, len = s48_list_length(items);
  XTextItem16 parts[len];
  S48_DECLARE_GC_PROTECT_6(display, drawable, gc, x, y, items);
  for (i = 0; i < len; i++) {
    scx_extract_text_item_16(S48_CAR(items), &parts[i]);
    items = S48_CDR(items);
  }
  XDrawText16(scx_extract_display(display),
	      scx_extract_drawable(drawable),
	      scx_extract_gc(gc),
	      s48_extract_integer(x),
	      s48_extract_integer(y),
	      parts, len);
  for (i = 0; i < len; i++)
    free(parts[i].chars);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Text_Extents(s48_value font_struct, s48_value string) {
  XCharStruct overall;
  int dir, ascent, descent;
  S48_DECLARE_GC_PROTECT_2(font_struct, string);
  XTextExtents(scx_extract_fontstruct(font_struct),
	       s48_extract_string(string),
	       S48_STRING_LENGTH(string),
	       &dir, &ascent, &descent,
	       &overall);
  S48_GC_RETURN(scx_enter_charstruct(&overall));
}

s48_value scx_Text_Extents_16(s48_value font_struct, s48_value string) {
  XCharStruct overall;
  int dir, ascent, descent;
  int i, len = s48_list_length(string);
  XChar2b chars[len];
  S48_DECLARE_GC_PROTECT_2(font_struct, string);
  for (i = 0; i < len; i++) {
    chars[i].byte1 = s48_extract_char(S48_CAR(S48_CAR(string)));
    chars[i].byte2 = s48_extract_char(S48_CDR(S48_CAR(string)));
    string = S48_CDR(string);
  }
  XTextExtents16(scx_extract_fontstruct(font_struct),
		 chars, len,
		 &dir, &ascent, &descent,
		 &overall);
  S48_GC_RETURN(scx_enter_charstruct(&overall));
}

void scx_init_text(void) {
  S48_GC_PROTECT_GLOBAL(scx_text_item);
  scx_text_item = s48_get_imported_binding("scx-text-item");
  
  S48_EXPORT_FUNCTION(scx_Draw_Image_String);
  S48_EXPORT_FUNCTION(scx_Draw_Image_String_16);
  S48_EXPORT_FUNCTION(scx_Draw_Text);
  S48_EXPORT_FUNCTION(scx_Draw_Text_16);
  S48_EXPORT_FUNCTION(scx_Text_Extents);
  S48_EXPORT_FUNCTION(scx_Text_Extents_16);
}
