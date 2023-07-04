/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_gc_function_binding = S48_FALSE;
s48_value scx_line_style_binding = S48_FALSE;
s48_value scx_cap_style_binding = S48_FALSE;
s48_value scx_join_style_binding = S48_FALSE;
s48_value scx_fill_style_binding = S48_FALSE;
s48_value scx_fill_rule_binding = S48_FALSE;
s48_value scx_subwindow_mode_binding = S48_FALSE;
s48_value scx_arc_mode_binding = S48_FALSE;
#define scx_extract_gc_function(x) S48_EXTRACT_ENUM(x, scx_gc_function_binding)
#define scx_extract_line_style(x) S48_EXTRACT_ENUM(x, scx_line_style_binding)
#define scx_extract_cap_style(x) S48_EXTRACT_ENUM(x, scx_cap_style_binding)
#define scx_extract_join_style(x) S48_EXTRACT_ENUM(x, scx_join_style_binding)
#define scx_extract_fill_style(x) S48_EXTRACT_ENUM(x, scx_fill_style_binding)
#define scx_extract_fill_rule(x) S48_EXTRACT_ENUM(x, scx_fill_rule_binding)
#define scx_extract_subwindow_mode(x) \
  S48_EXTRACT_ENUM(x, scx_subwindow_mode_binding)
#define scx_extract_arc_mode(x) S48_EXTRACT_ENUM(x, scx_arc_mode_binding)

s48_value scx_gc_functions_binding = S48_FALSE;
s48_value scx_line_styles_binding = S48_FALSE;
s48_value scx_cap_styles_binding = S48_FALSE;
s48_value scx_join_styles_binding = S48_FALSE;
s48_value scx_fill_styles_binding = S48_FALSE;
s48_value scx_fill_rules_binding = S48_FALSE;
s48_value scx_subwindow_modes_binding = S48_FALSE;
s48_value scx_arc_modes_binding = S48_FALSE;
#define scx_enter_gc_function(x) S48_ENTER_ENUM(x, scx_gc_functions_binding)
#define scx_enter_line_style(x) S48_ENTER_ENUM(x, scx_line_styles_binding)
#define scx_enter_cap_style(x) S48_ENTER_ENUM(x, scx_cap_styles_binding)
#define scx_enter_join_style(x) S48_ENTER_ENUM(x, scx_join_styles_binding)
#define scx_enter_fill_style(x) S48_ENTER_ENUM(x, scx_fill_styles_binding)
#define scx_enter_fill_rule(x) S48_ENTER_ENUM(x, scx_fill_rules_binding)
#define scx_enter_subwindow_mode(x) \
  S48_ENTER_ENUM(x, scx_subwindow_modes_binding)
#define scx_enter_arc_mode(x) S48_ENTER_ENUM(x, scx_arc_modes_binding)

s48_value scx_gc_value_set_binding = S48_FALSE;
#define scx_extract_gc_value_set(x) \
  S48_EXTRACT_ENUM_SET(x, scx_gc_value_set_binding)
#define scx_enter_gc_value_set(x) \
  s48_enter_enum_set(x, scx_gc_value_set_binding)

s48_value scx_char_struct_binding = S48_FALSE;

s48_value scx_enter_charstruct(XCharStruct* cs) {
  s48_value res = s48_make_record(scx_char_struct_binding);
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(res);
  S48_RECORD_SET(res, 0, s48_enter_fixnum(cs->lbearing));
  S48_RECORD_SET(res, 1, s48_enter_fixnum(cs->rbearing));
  S48_RECORD_SET(res, 2, s48_enter_fixnum(cs->width));
  S48_RECORD_SET(res, 3, s48_enter_fixnum(cs->ascent));
  S48_RECORD_SET(res, 4, s48_enter_fixnum(cs->descent));
  S48_RECORD_SET(res, 5, s48_enter_integer(cs->attributes));
  S48_GC_UNPROTECT();
  return res;
}

s48_value scx_enter_fontstruct(XFontStruct* fs) {
  int i;
  s48_value plist = S48_NULL, t = S48_NULL;
  s48_value res = s48_make_record(scx_fontstruct);
  S48_DECLARE_GC_PROTECT(3);
  S48_GC_PROTECT_3(res, plist, t);
  S48_RECORD_SET(res, 0, S48_ENTER_POINTER(fs));
  S48_RECORD_SET(res, 1, scx_enter_font(fs->fid));
  S48_RECORD_SET(res, 2, scx_enter_font_direction(fs->direction));
  S48_RECORD_SET(res, 3, s48_enter_integer(fs->min_char_or_byte2));
  S48_RECORD_SET(res, 4, s48_enter_integer(fs->max_char_or_byte2));
  S48_RECORD_SET(res, 5, s48_enter_integer(fs->min_byte1));
  S48_RECORD_SET(res, 6, s48_enter_integer(fs->max_byte1));
  S48_RECORD_SET(res, 7, S48_ENTER_BOOLEAN(fs->all_chars_exist));
  S48_RECORD_SET(res, 8, s48_enter_integer(fs->default_char));
  for (i = fs->n_properties-1; i >= 0; i--) {
    t = s48_cons(scx_enter_atom(fs->properties[i].name),
		 s48_enter_integer(fs->properties[i].card32));
    plist = s48_cons(t, plist);
  }
  S48_RECORD_SET(res, 9, plist);
  S48_RECORD_SET(res, 10, scx_enter_charstruct(&fs->min_bounds));
  S48_RECORD_SET(res, 11, scx_enter_charstruct(&fs->max_bounds));

  {
    int count;
    if ((fs->min_byte1 == 0) && (fs->max_byte1 == 0))
      count = (fs->max_char_or_byte2 - fs->min_char_or_byte2);
    else
      count = (fs->max_char_or_byte2 - fs->min_char_or_byte2) * 256
	- fs->min_byte1 + fs->max_byte1;
    t = s48_make_vector(count, S48_FALSE);
    for (i = 0; i < count; i++)
      S48_VECTOR_SET(t, i, scx_enter_charstruct(&fs->per_char[i]));
    S48_RECORD_SET(res, 12, t);
  }

  S48_RECORD_SET(res, 13, s48_enter_integer(fs->ascent));
  S48_RECORD_SET(res, 14, s48_enter_integer(fs->descent));

  S48_GC_UNPROTECT();
  return res;
}

s48_value scx_enter_gc(GC gc) {
  s48_value v = s48_make_record(scx_gc);
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(v);
  S48_RECORD_SET(v, 0, S48_ENTER_POINTER(gc));
  S48_GC_UNPROTECT();
  return v;
}

unsigned long scx_extract_gc_value_alist(s48_value values, XGCValues* GCV) {
  unsigned long mask = 0;
  s48_value v = S48_FALSE;
  S48_DECLARE_GC_PROTECT_2(values, v);
  while (values != S48_NULL) {
    int mv = scx_extract_gc_value(S48_CAR(S48_CAR(values)));
    v = S48_CDR(S48_CAR(values));
    values = S48_CDR(values);
    mask = mask | (1L << mv);
    switch (1L << mv) {
    case GCFunction:
      GCV->function = scx_extract_gc_function(v); break;
    case GCPlaneMask:
      GCV->plane_mask = scx_extract_pixel(v); break;
    case GCForeground:
      GCV->foreground = scx_extract_pixel(v); break;
    case GCBackground:
      GCV->background = scx_extract_pixel(v); break;
    case GCLineWidth:
      GCV->line_width = s48_extract_integer(v); break;
    case GCLineStyle:
      GCV->line_style = scx_extract_line_style(v); break;
    case GCCapStyle:
      GCV->cap_style = scx_extract_cap_style(v); break;
    case GCJoinStyle:
      GCV->join_style = scx_extract_join_style(v); break;
    case GCFillStyle:
      GCV->fill_style = scx_extract_fill_style(v); break;
    case GCFillRule:
      GCV->fill_rule = scx_extract_fill_rule(v); break;
    case GCTile:
      GCV->tile = scx_extract_pixmap(v); break;
    case GCStipple:
      GCV->stipple = scx_extract_pixmap(v); break;
    case GCTileStipXOrigin:
      GCV->ts_x_origin = s48_extract_integer(v); break;
    case GCTileStipYOrigin:
      GCV->ts_y_origin = s48_extract_integer(v); break;
    case GCFont:
      GCV->font = scx_extract_font(v); break;
    case GCSubwindowMode:
      GCV->subwindow_mode = scx_extract_subwindow_mode(v); break;
    case GCGraphicsExposures:
      GCV->graphics_exposures = S48_EXTRACT_BOOLEAN(v); break;
    case GCClipXOrigin:
      GCV->clip_x_origin = s48_extract_integer(v); break;
    case GCClipYOrigin:
      GCV->clip_y_origin = s48_extract_integer(v); break;
    case GCClipMask:
      GCV->clip_mask = scx_extract_pixmap(v); break;
    case GCDashOffset:
      GCV->dash_offset = s48_extract_integer(v); break;
    case GCDashList:
      GCV->dashes = (char)s48_extract_integer(v); break;
    case GCArcMode:
      GCV->arc_mode = scx_extract_arc_mode(v); break;
    }
  }
  S48_GC_UNPROTECT();
  return mask;
}

static s48_value scx_enter_gc_value_alist(s48_value values, XGCValues* GCV) {
  S48_DECLARE_GC_PROTECT(3);
  s48_value res = S48_NULL;
  s48_value v = S48_FALSE;
  S48_GC_PROTECT_3(res, v, values);
  while (values != S48_NULL) {
    int mv = scx_extract_gc_value(S48_CAR(values));
    switch (1L << mv) {
    case GCFunction:
      v = scx_enter_gc_function(GCV->function); break;
    case GCPlaneMask:
      v = scx_enter_pixel(GCV->plane_mask); break;
    case GCForeground:
      v = scx_enter_pixel(GCV->foreground); break;
    case GCBackground:
      v = scx_enter_pixel(GCV->background); break;
    case GCLineWidth:
      v = s48_enter_integer(GCV->line_width); break;
    case GCLineStyle:
      v = scx_enter_line_style(GCV->line_style); break;
    case GCCapStyle:
      v = scx_enter_cap_style(GCV->cap_style); break;
    case GCJoinStyle:
      v = scx_enter_join_style(GCV->join_style); break;
    case GCFillStyle:
      v = scx_enter_fill_style(GCV->fill_style); break;
    case GCFillRule:
      v = scx_enter_fill_rule(GCV->fill_rule); break;
    case GCTile:
      v = scx_enter_pixmap(GCV->tile); break;
    case GCStipple:
      v = scx_enter_pixmap(GCV->stipple); break;
    case GCTileStipXOrigin:
      v = s48_enter_integer(GCV->ts_x_origin); break;
    case GCTileStipYOrigin:
      v = s48_enter_integer(GCV->ts_y_origin); break;
    case GCFont:
      v = scx_enter_font(GCV->font); break;
    case GCSubwindowMode:
      v = scx_enter_subwindow_mode(GCV->subwindow_mode); break;
    case GCGraphicsExposures:
      v = S48_ENTER_BOOLEAN(GCV->graphics_exposures); break;
    case GCClipXOrigin:
      v = s48_enter_integer(GCV->clip_x_origin); break;
    case GCClipYOrigin:
      v = s48_enter_integer(GCV->clip_y_origin); break;
    case GCClipMask:
      v = scx_enter_pixmap(GCV->clip_mask); break;
    case GCDashOffset:
      v = s48_enter_integer(GCV->dash_offset); break;
    case GCDashList:
      v = s48_enter_integer(GCV->dashes); break;
    case GCArcMode:
      v = scx_enter_arc_mode(GCV->arc_mode); break;
    }
    v = s48_cons(S48_CAR(values), v);
    res = s48_cons(v, res);
    values = S48_CDR(values);
  }
  S48_GC_UNPROTECT();
  return res;
}

s48_value scx_Create_Gc(s48_value display, s48_value drawable, 
			s48_value values) {
  XGCValues GCV;
  unsigned long mask;
  GC gc;
  S48_DECLARE_GC_PROTECT_3(display, drawable, values);
  mask = scx_extract_gc_value_alist(values, &GCV);
  gc = XCreateGC(scx_extract_display(display), 
		 scx_extract_drawable(drawable), 
		 mask, &GCV);
  S48_GC_RETURN(scx_enter_gc(gc));
}

s48_value scx_Copy_Gc(s48_value display, s48_value source, s48_value mask,
		      s48_value dest) {
  S48_DECLARE_GC_PROTECT_4(display, source, mask, dest);
  XCopyGC(scx_extract_display(display), scx_extract_gc(source), 
	  scx_extract_gc_value_set(mask), scx_extract_gc(dest));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Change_Gc(s48_value display, s48_value gc, s48_value values) {
  XGCValues GCV;
  unsigned long mask;
  S48_DECLARE_GC_PROTECT_3(display, gc, values);
  mask = scx_extract_gc_value_alist(values, &GCV);
  XChangeGC(scx_extract_display(display), scx_extract_gc(gc),
	    mask, &GCV);
  S48_GC_RETURN(S48_UNSPECIFIC);
}


#define ValidGCValuesBits \
    (GCFunction | GCPlaneMask | GCForeground | GCBackground | GCLineWidth |\
    GCLineStyle | GCCapStyle | GCJoinStyle | GCFillStyle | GCFillRule |\
    GCTile | GCStipple | GCTileStipXOrigin | GCTileStipYOrigin | GCFont |\
    GCSubwindowMode | GCGraphicsExposures | GCClipXOrigin | GCClipYOrigin |\
    GCDashOffset | GCArcMode)

s48_value scx_Get_Gc_Values(s48_value display, s48_value gc,
			    s48_value values) {
  unsigned long mask = 0;
  XGCValues GCV;
  S48_DECLARE_GC_PROTECT_3(display, gc, values);
  for (; values != S48_NULL; values = S48_CDR(values))
    mask |= (1L << scx_extract_gc_value(S48_CAR(values)));
  
  if (!XGetGCValues(scx_extract_display(display), 
		    scx_extract_gc(gc), 
		    mask, &GCV))
    S48_GC_RETURN(S48_FALSE);
  else
    S48_GC_RETURN(scx_enter_gc_value_alist(values, &GCV));
}

s48_value scx_Free_Gc(s48_value display, s48_value gc) {
  S48_DECLARE_GC_PROTECT_2(display, gc);
  XFreeGC(scx_extract_display(display), scx_extract_gc(gc));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_GContext_From_Gc(s48_value gc) {
  return scx_enter_gcontext(XGContextFromGC(scx_extract_gc(gc)));
}

s48_value scx_Set_Dashes(s48_value display, s48_value gc, s48_value dashoffset,
			 s48_value dashlist) {
  int i, n = s48_list_length(dashlist);
  char dl[n];
  S48_DECLARE_GC_PROTECT_4(display, gc, dashoffset, dashlist);
  for (i = 0; i < n; i++) {
    dl[i] = s48_extract_integer(S48_CAR(dashlist));
    dashlist = S48_CDR(dashlist);
  }
  XSetDashes(scx_extract_display(display), scx_extract_gc(gc),
	     s48_extract_integer(dashoffset),
	     dl, n);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_rectangle_ordering_binding = S48_FALSE;
#define scx_extract_rectangle_ordering(x) \
  S48_EXTRACT_ENUM(x, scx_rectangle_ordering_binding)

s48_value scx_Set_Clip_Rectangles(s48_value display, s48_value gc,
				  s48_value x_origin, s48_value y_origin,
				  s48_value rects, s48_value ordering) {
  int i, n = s48_list_length(rects);
  XRectangle crects[n];
  s48_value r = S48_FALSE;
  S48_DECLARE_GC_PROTECT_7(display, gc, x_origin, y_origin, rects,
			   ordering, r);
  for (i = 0; i < n; i++) {
    r = S48_CAR(rects);
    crects[i].x = s48_extract_integer(S48_CAR(r)); r = S48_CDR(r);
    crects[i].y = s48_extract_integer(S48_CAR(r)); r = S48_CDR(r);
    crects[i].width = s48_extract_integer(S48_CAR(r)); r = S48_CDR(r);
    crects[i].height = s48_extract_integer(S48_CAR(r)); r = S48_CDR(r);
    rects = S48_CDR(rects);
  }
  XSetClipRectangles(scx_extract_display(display), scx_extract_gc(gc),
		     s48_extract_integer(x_origin),
		     s48_extract_integer(y_origin),
		     crects, n, scx_extract_rectangle_ordering(ordering));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Query_Best_Size(s48_value screen, s48_value class,
			      s48_value width, s48_value height) {
  unsigned int rw, rh;
  Screen* s;
  S48_DECLARE_GC_PROTECT_4(screen, class, width, height);
  s = scx_extract_screen(screen);

  if (!XQueryBestSize(s->display, 
		      s48_extract_integer(class),
		      s->root,
		      (int)s48_extract_integer(width), 
		      (int)s48_extract_integer(height), 
		      &rw, &rh))
    S48_GC_RETURN(S48_FALSE);
  else
    S48_GC_RETURN(s48_cons(s48_enter_fixnum(rw), s48_enter_fixnum(rh)));
}

void scx_init_gcontext(void) {
  SCX_PRO_IMP(scx_gc_function_binding, "scx-gc-function");
  SCX_PRO_IMP(scx_line_style_binding, "scx-line-style");
  SCX_PRO_IMP(scx_cap_style_binding, "scx-cap-style");
  SCX_PRO_IMP(scx_join_style_binding, "scx-join-style");
  SCX_PRO_IMP(scx_fill_style_binding, "scx-fill-style");
  SCX_PRO_IMP(scx_fill_rule_binding, "scx-fill-rule");
  SCX_PRO_IMP(scx_subwindow_mode_binding, "scx-subwindow-mode");
  SCX_PRO_IMP(scx_arc_mode_binding, "scx-arc-mode");
  SCX_PRO_IMP(scx_gc_functions_binding, "scx-gc-functions");
  SCX_PRO_IMP(scx_line_styles_binding, "scx-line-styles");
  SCX_PRO_IMP(scx_cap_styles_binding, "scx-cap-styles");
  SCX_PRO_IMP(scx_join_styles_binding, "scx-join-styles");
  SCX_PRO_IMP(scx_fill_styles_binding, "scx-fill-styles");
  SCX_PRO_IMP(scx_fill_rules_binding, "scx-fill-rules");
  SCX_PRO_IMP(scx_subwindow_modes_binding, "scx-subwindow-modes");
  SCX_PRO_IMP(scx_arc_modes_binding, "scx-arc-modes");

  SCX_PRO_IMP(scx_gc_value_set_binding, "scx-gc-value-set");
  SCX_PRO_IMP(scx_char_struct_binding, "scx-char-struct");
  SCX_PRO_IMP(scx_rectangle_ordering_binding, "scx-rectangle-ordering");

  S48_EXPORT_FUNCTION(scx_Create_Gc);
  S48_EXPORT_FUNCTION(scx_Copy_Gc);
  S48_EXPORT_FUNCTION(scx_Change_Gc);
  S48_EXPORT_FUNCTION(scx_Get_Gc_Values);
  S48_EXPORT_FUNCTION(scx_Free_Gc);
  S48_EXPORT_FUNCTION(scx_GContext_From_Gc);
  S48_EXPORT_FUNCTION(scx_Set_Dashes);
  S48_EXPORT_FUNCTION(scx_Set_Clip_Rectangles);
  S48_EXPORT_FUNCTION(scx_Query_Best_Size);
}
