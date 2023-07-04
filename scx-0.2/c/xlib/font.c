/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_Load_Font(s48_value display, s48_value font_name) {
  Font f;
  S48_DECLARE_GC_PROTECT_2(display, font_name);
  f = XLoadFont(scx_extract_display(display),
		s48_extract_string(font_name));
  S48_GC_RETURN(scx_enter_font(f));
}

s48_value scx_Unload_Font(s48_value display, s48_value font) {
  S48_DECLARE_GC_PROTECT_2(display, font);
  XUnloadFont(scx_extract_display(display),
	      scx_extract_font(font));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Query_Font(s48_value display, s48_value font) {
  XFontStruct* fs;
  S48_DECLARE_GC_PROTECT_2(display, font);
  fs = XQueryFont(scx_extract_display(display),
		  scx_extract_font(font));
  if (fs == NULL)
    S48_GC_RETURN(S48_FALSE);
  else
    S48_GC_RETURN(scx_enter_fontstruct(fs));
}

s48_value scx_Load_Query_Font(s48_value display, s48_value font_name) {
  XFontStruct* fs;
  S48_DECLARE_GC_PROTECT_2(display, font_name);
  fs = XLoadQueryFont(scx_extract_display(display),
		      s48_extract_string(font_name));
  if (fs == NULL)
    S48_GC_RETURN(S48_FALSE);
  else
    S48_GC_RETURN(scx_enter_fontstruct(fs));
}

s48_value scx_Free_Font(s48_value display, s48_value fontstruct) {
  S48_DECLARE_GC_PROTECT_2(display, fontstruct);
  XFreeFont(scx_extract_display(display),
	    scx_extract_fontstruct(fontstruct));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_List_Fonts(s48_value display, s48_value pattern, s48_value max) {
  s48_value res = S48_NULL;
  int i, count;
  char** fonts;
  S48_DECLARE_GC_PROTECT_4(display, pattern, max, res);

  fonts = XListFonts(scx_extract_display(display), s48_extract_string(pattern),
		     s48_extract_integer(max), &count);
  for (i = count; i > 0; i--)
    res = s48_cons(s48_enter_string(fonts[i-1]), res);
  XFreeFontNames(fonts);
  S48_GC_RETURN(res);
}

s48_value scx_List_Fonts_With_Info(s48_value display, s48_value pattern,
				   s48_value max) {
  s48_value res = S48_NULL, cell = S48_NULL;
  int i, count;
  char** fonts;
  XFontStruct* infos;
  S48_DECLARE_GC_PROTECT_5(display, pattern, max, res, cell);

  fonts = XListFontsWithInfo(scx_extract_display(display),
			     s48_extract_string(pattern),
			     s48_extract_integer(max), &count,
			     &infos);
  for (i = count; i > 0; i--) {
    cell = scx_enter_fontstruct(&infos[i-1]);
    cell = s48_cons(s48_enter_string(fonts[i-1]), cell);
    res = s48_cons(cell, res);
  }
  XFreeFontNames(fonts); /* FontStructs have to be freed later */
  S48_GC_RETURN(res);
}

s48_value scx_Set_Font_Path(s48_value display, s48_value dirs) {
  int i, n = s48_list_length(dirs);
  char* sa[n];
  s48_value l = dirs;
  S48_DECLARE_GC_PROTECT_3(display, dirs, l);
  for (i = 0; i < n; i++) {
    sa[i] = s48_extract_string(S48_CAR(l));
    l = S48_CDR(l);
  }
  XSetFontPath(scx_extract_display(display), sa, n);

  S48_GC_RETURN(S48_UNSPECIFIC);
}
	       
s48_value scx_Get_Font_Path(s48_value display) {
  int n, i;
  char** sa;
  s48_value res = S48_NULL;
  S48_DECLARE_GC_PROTECT_2(display, res);

  sa = XGetFontPath(scx_extract_display(display), &n);

  for (i = n; i > 0; i--)
    res = s48_cons(s48_enter_string(sa[i]), res);
  XFreeFontPath(sa);

  S48_GC_RETURN(res);
}

/* TODO:

s48_value scx_Font_Properties(s48_value Xfontstruct) {
  s48_value v, t = S48_FALSE;
  int i,n;
  XFontStruct* fs = scx_extract_fontstruct(Xfontstruct);
  XFontProp* p;
  S48_DECLARE_GC_PROTECT(2);

  n = fs->n_properties;
  v = s48_make_vector(n, S48_FALSE);
  S48_GC_PROTECT_2(v, t);

  for (i = 0; i < n; i++) {
    p = fs->properties+i;
    t = scx_enter_atom(p->name);
    t = s48_cons(t, s48_enter_integer(p->card32));
    S48_VECTOR_SET(v, i, t);
  }

  S48_GC_UNPROTECT();
  return v;
}

s48_value scx_Font_Property(s48_value Xfontstruct, s48_value Xatom) {
  unsigned long val;
  if (XGetFontProperty(scx_extract_fontstruct(Xfontstruct),
		       scx_extract_atom(Xatom),
		       &val))
    return s48_enter_integer(val);
  else
    return S48_FALSE;
}

s48_value scx_Font_Info(s48_value Xfontstruct) {
  XFontStruct* fs = scx_extract_fontstruct(Xfontstruct);
  s48_value v = s48_make_vector(9, S48_FALSE);
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(v);
  
  S48_VECTOR_SET(v, 0, s48_enter_fixnum(fs->direction));
  S48_VECTOR_SET(v, 1, s48_enter_fixnum(fs->min_char_or_byte2));
  S48_VECTOR_SET(v, 2, s48_enter_fixnum(fs->max_char_or_byte2));
  S48_VECTOR_SET(v, 3, s48_enter_fixnum(fs->min_byte1));
  S48_VECTOR_SET(v, 4, s48_enter_fixnum(fs->max_byte1));
  S48_VECTOR_SET(v, 5, S48_ENTER_BOOLEAN(fs->all_chars_exist));
  S48_VECTOR_SET(v, 6, s48_enter_fixnum(fs->default_char));
  S48_VECTOR_SET(v, 7, s48_enter_fixnum(fs->ascent));
  S48_VECTOR_SET(v, 8, s48_enter_fixnum(fs->descent));

  S48_GC_UNPROTECT();
  return v;
}

s48_value scx_Char_Info(s48_value Xfontstruct, s48_value index) {
  // index must be an integer, #f for 'min or #t for 'max
  XCharStruct* cp;
  XFontStruct* p = scx_extract_fontstruct(Xfontstruct);
  s48_value v = S48_FALSE;
  S48_DECLARE_GC_PROTECT(1);

  if (S48_FALSE_P(index))
    cp = &p->min_bounds;
  else if (S48_TRUE_P(index))
    cp = &p->max_bounds;
  else
    cp = &(p->per_char[s48_extract_integer(index)]); // calculated in scheme
  
  v = s48_make_vector(6, S48_FALSE);
  S48_GC_PROTECT_1(v);

  S48_VECTOR_SET(v, 0, s48_enter_fixnum(cp->lbearing));
  S48_VECTOR_SET(v, 1, s48_enter_fixnum(cp->rbearing));
  S48_VECTOR_SET(v, 2, s48_enter_fixnum(cp->width));
  S48_VECTOR_SET(v, 3, s48_enter_fixnum(cp->ascent));
  S48_VECTOR_SET(v, 4, s48_enter_fixnum(cp->descent));
  S48_VECTOR_SET(v, 5, s48_enter_fixnum(cp->attributes));

  S48_GC_UNPROTECT();
  return v;
}
*/

void scx_init_font(void) {
  S48_EXPORT_FUNCTION(scx_Load_Font);
  S48_EXPORT_FUNCTION(scx_Unload_Font);
  S48_EXPORT_FUNCTION(scx_Query_Font);
  S48_EXPORT_FUNCTION(scx_Load_Query_Font);
  S48_EXPORT_FUNCTION(scx_Free_Font);
  S48_EXPORT_FUNCTION(scx_List_Fonts);
  S48_EXPORT_FUNCTION(scx_List_Fonts_With_Info);
  S48_EXPORT_FUNCTION(scx_Set_Font_Path);
  S48_EXPORT_FUNCTION(scx_Get_Font_Path);
}
