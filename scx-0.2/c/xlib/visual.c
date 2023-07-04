/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_visual_class_binding = S48_FALSE;
#define scx_extract_visual_class(x) \
  S48_EXTRACT_ENUM(x, scx_visual_class_binding)
s48_value scx_visual_classes_binding = S48_FALSE;
#define scx_enter_visual_class(x) S48_ENTER_ENUM(x, scx_visual_classes_binding)

s48_value scx_enter_visual(Visual* vis) {
  s48_value v = s48_make_record(scx_visual);
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(v);
  S48_RECORD_SET(v, 0, S48_ENTER_POINTER(vis));
  S48_GC_UNPROTECT();
  return v;
}

s48_value scx_visual_info_binding = S48_FALSE;

s48_value scx_enter_visual_info(XVisualInfo* vi) {
  s48_value v = s48_make_record(scx_visual_info_binding);
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(v);
  S48_RECORD_SET(v, 0, scx_enter_visual(vi->visual));
  S48_RECORD_SET(v, 1, scx_enter_visualid(vi->visualid));
  S48_RECORD_SET(v, 2, s48_enter_integer(vi->screen));
  S48_RECORD_SET(v, 3, s48_enter_integer(vi->depth));
  S48_RECORD_SET(v, 4, scx_enter_visual_class(vi->class));
  S48_RECORD_SET(v, 5, s48_enter_integer(vi->red_mask));
  S48_RECORD_SET(v, 6, s48_enter_integer(vi->green_mask));
  S48_RECORD_SET(v, 7, s48_enter_integer(vi->blue_mask));
  S48_RECORD_SET(v, 8, s48_enter_integer(vi->bits_per_rgb));
  S48_RECORD_SET(v, 9, s48_enter_integer(vi->colormap_size));
  S48_GC_UNPROTECT();
  return v;
}

unsigned int scx_extract_visual_info(s48_value v, XVisualInfo* vi) {
  unsigned long mask = 0;
  S48_DECLARE_GC_PROTECT_1(v);
  s48_check_record_type(v, scx_visual_info_binding);
  if (!S48_FALSE_P(S48_RECORD_REF(v, 0)))
    vi->visual = scx_extract_visual(S48_RECORD_REF(v, 0));
  if (!S48_FALSE_P(S48_RECORD_REF(v, 1))) {
    vi->visualid = scx_extract_visualid(S48_RECORD_REF(v, 1));
    mask |= VisualIDMask;
  }
  if (!S48_FALSE_P(S48_RECORD_REF(v, 2))) {
    vi->screen = s48_extract_integer(S48_RECORD_REF(v, 2));
    mask |= VisualScreenMask;
  }
  if (!S48_FALSE_P(S48_RECORD_REF(v, 3))) {
    vi->depth = s48_extract_integer(S48_RECORD_REF(v, 4));
    mask |= VisualDepthMask;
  }
  if (!S48_FALSE_P(S48_RECORD_REF(v, 4))) {
    vi->class = scx_extract_visual_class(S48_RECORD_REF(v, 5));
    mask |= VisualClassMask;
  }
  if (!S48_FALSE_P(S48_RECORD_REF(v, 5))) {
    vi->red_mask = s48_extract_integer(S48_RECORD_REF(v, 6));
    mask |= VisualRedMaskMask;
  }
  if (!S48_FALSE_P(S48_RECORD_REF(v, 6))) {
    vi->green_mask = s48_extract_integer(S48_RECORD_REF(v, 7));
    mask |= VisualGreenMaskMask;
  }
  if (!S48_FALSE_P(S48_RECORD_REF(v, 7))) {
    vi->blue_mask = s48_extract_integer(S48_RECORD_REF(v, 8));
    mask |= VisualBlueMaskMask;
  }
  if (!S48_FALSE_P(S48_RECORD_REF(v, 8))) {
    vi->bits_per_rgb = s48_extract_integer(S48_RECORD_REF(v, 9));
    mask |= VisualBitsPerRGBMask;
  }
  if (!S48_FALSE_P(S48_RECORD_REF(v, 9))) {
    vi->colormap_size = s48_extract_integer(S48_RECORD_REF(v, 10));
    mask |= VisualColormapSizeMask;
  }
  S48_GC_RETURN(mask);
}

s48_value scx_Get_Visual_Info(s48_value display, s48_value template) {
  XVisualInfo templ;
  unsigned long mask = scx_extract_visual_info(template, &templ);
  XVisualInfo* vis;
  int n, i;
  s48_value l = S48_NULL;
  S48_DECLARE_GC_PROTECT_3(display, template, l);

  vis = XGetVisualInfo(scx_extract_display(display),
		       mask, &templ, &n);

  for (i = n-1; i >= 0; i--)
    l = s48_cons(scx_enter_visual_info(&vis[i]), l);
  S48_GC_UNPROTECT();
  return l;
}

s48_value scx_Match_Visual_Info(s48_value display, s48_value scrnum,
				s48_value depth, s48_value class) {
  XVisualInfo vi;
  S48_DECLARE_GC_PROTECT_4(display, scrnum, depth, class);
  if (! XMatchVisualInfo(scx_extract_display(display),
			 s48_extract_integer(scrnum),
			 s48_extract_integer(depth),
			 scx_extract_visual_class(class),
			 &vi) )
    S48_GC_RETURN(S48_FALSE);
  else
    S48_GC_RETURN(scx_enter_visual_info(&vi));
}

s48_value scx_VisualIDFromVisual(s48_value visual) {
  return s48_enter_integer(XVisualIDFromVisual(scx_extract_visual(visual)));
}

void scx_init_visual(void) {
  SCX_PRO_IMP(scx_visual_class_binding, "scx-visual-class");
  SCX_PRO_IMP(scx_visual_classes_binding, "scx-visual-classes");
  SCX_PRO_IMP(scx_visual_info_binding, "scx-visual-info");

  S48_EXPORT_FUNCTION(scx_Get_Visual_Info);
  S48_EXPORT_FUNCTION(scx_Match_Visual_Info);
  S48_EXPORT_FUNCTION(scx_VisualIDFromVisual);
}
