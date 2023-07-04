#include "xlib.h"

/*** create or destroy regions *************************************/

s48_value scx_Create_Region() {
  return scx_enter_region(XCreateRegion());
}

s48_value scx_Set_Region(s48_value display, s48_value gc, s48_value r) {
  XSetRegion(scx_extract_display(display), scx_extract_gc(gc),
	     scx_extract_region(r));
  return S48_UNSPECIFIC;
}

s48_value scx_Destroy_Region(s48_value r) {
  XDestroyRegion(scx_extract_region(r));
  return S48_UNSPECIFIC;
}

/*** determine if regions are empty or equal ***********************/

s48_value scx_Empty_Region(s48_value r) {
  return S48_ENTER_BOOLEAN(XEmptyRegion(scx_extract_region(r)));
}

s48_value scx_Equal_Region(s48_value r1, s48_value r2) {
  Bool res;
  S48_DECLARE_GC_PROTECT_2(r1, r2);
  res = XEqualRegion(scx_extract_region(r1),
		     scx_extract_region(r2));
  S48_GC_RETURN(S48_ENTER_BOOLEAN(res));
}

s48_value scx_Point_In_Region(s48_value r, s48_value x, s48_value y) {
  Bool res;
  S48_DECLARE_GC_PROTECT_3(r, x, y);
  res = XPointInRegion(scx_extract_region(r),
		       s48_extract_integer(x),
		       s48_extract_integer(y));
  S48_GC_RETURN(S48_ENTER_BOOLEAN(res));
}

s48_value scx_rect_in_region_type_binding = S48_FALSE;
s48_value scx_rect_in_region_types_binding = S48_FALSE;
#define scx_extract_rect_in_region_type(x) \
  S48_EXTRACT_ENUM(x, scx_rect_in_region_type_binding)
#define scx_enter_rect_in_region_type(x) \
  S48_ENTER_ENUM(x, scx_rect_in_region_types_binding)

s48_value scx_Rect_In_Region(s48_value r, s48_value x, s48_value y,
			     s48_value w, s48_value h) {
  int res;
  S48_DECLARE_GC_PROTECT_5(r, x, y, w, h);
  res = XRectInRegion(scx_extract_region(r),
		      s48_extract_integer(x),
		      s48_extract_integer(y),
		      s48_extract_integer(w),
		      s48_extract_integer(h));
  S48_GC_RETURN(scx_enter_rect_in_region_type(res));
}

/*** region arithmetic *********************************************/

s48_value scx_Intersect_Region(s48_value sra, s48_value srb, s48_value dr) {
  S48_DECLARE_GC_PROTECT_3(sra, srb, dr);
  XIntersectRegion(scx_extract_region(sra), scx_extract_region(srb),
		   scx_extract_region(dr));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Union_Region(s48_value sra, s48_value srb, s48_value dr) {
  S48_DECLARE_GC_PROTECT_3(sra, srb, dr);
  XUnionRegion(scx_extract_region(sra), scx_extract_region(srb),
	       scx_extract_region(dr));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Union_Rect_With_Region(s48_value x, s48_value y, s48_value w,
				     s48_value h, s48_value src,
				     s48_value dest) {
  XRectangle r;
  S48_DECLARE_GC_PROTECT_6(x, y, w, h, src, dest);
  r.x = s48_extract_integer(x);
  r.y = s48_extract_integer(y);
  r.width = s48_extract_integer(w);
  r.height = s48_extract_integer(h);

  XUnionRectWithRegion(&r, scx_extract_region(src), scx_extract_region(dest));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Subtract_Region(s48_value sra, s48_value srb, s48_value dr) {
  S48_DECLARE_GC_PROTECT_3(sra, srb, dr);
  XSubtractRegion(scx_extract_region(sra), scx_extract_region(srb),
		  scx_extract_region(dr));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Xor_Region(s48_value sra, s48_value srb, s48_value dr) {
  S48_DECLARE_GC_PROTECT_3(sra, srb, dr);
  XXorRegion(scx_extract_region(sra), scx_extract_region(srb),
	     scx_extract_region(dr));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Offset_Region(s48_value r, s48_value dx, s48_value dy) {
  S48_DECLARE_GC_PROTECT_3(r, dx, dy);
  XOffsetRegion(scx_extract_region(r), s48_extract_integer(dx),
		s48_extract_integer(dy));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Shrink_Region(s48_value r, s48_value dx, s48_value dy) {
  S48_DECLARE_GC_PROTECT_3(r, dx, dy);
  XShrinkRegion(scx_extract_region(r), s48_extract_integer(dx),
		s48_extract_integer(dy));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

/*** generate regions **********************************************/

extern s48_value scx_fill_rule_binding; /* from gcontext.c */
#define scx_extract_fill_rule(x) S48_EXTRACT_ENUM(x, scx_fill_rule_binding)

s48_value scx_Polygon_Region(s48_value points, s48_value fill_rule) {
  int i, n = s48_list_length(points);
  Region r;
  XPoint ps[n];
  S48_DECLARE_GC_PROTECT_2(points, fill_rule);
  for (i = 0; i < n; i++) {
    ps[i].x = s48_extract_integer(S48_CAR(S48_CAR(points)));
    ps[i].y = s48_extract_integer(S48_CDR(S48_CAR(points)));
    points = S48_CDR(points);
  }
  r = XPolygonRegion(ps, n, scx_extract_fill_rule(fill_rule));
  S48_GC_RETURN(scx_enter_region(r));
}

s48_value scx_Clip_Box(s48_value r) {
  XRectangle rect;
  s48_value l = S48_NULL;
  S48_DECLARE_GC_PROTECT_2(r, l);
  XClipBox(scx_extract_region(r), &rect);
  l = s48_cons(s48_enter_integer(rect.height), l);
  l = s48_cons(s48_enter_integer(rect.width), l);
  l = s48_cons(s48_enter_integer(rect.y), l);
  l = s48_cons(s48_enter_integer(rect.x), l);
  S48_GC_RETURN(l);
}

void scx_init_region(void) {
  SCX_PRO_IMP(scx_rect_in_region_type_binding, "scx-rect-in-region-type");
  SCX_PRO_IMP(scx_rect_in_region_types_binding, "scx-rect-in-region-types");

  S48_EXPORT_FUNCTION(scx_Destroy_Region);
  S48_EXPORT_FUNCTION(scx_Create_Region);
  S48_EXPORT_FUNCTION(scx_Clip_Box);
  S48_EXPORT_FUNCTION(scx_Empty_Region);
  S48_EXPORT_FUNCTION(scx_Equal_Region);
  S48_EXPORT_FUNCTION(scx_Point_In_Region);
  S48_EXPORT_FUNCTION(scx_Rect_In_Region);
  S48_EXPORT_FUNCTION(scx_Intersect_Region);
  S48_EXPORT_FUNCTION(scx_Union_Region);
  S48_EXPORT_FUNCTION(scx_Union_Rect_With_Region);
  S48_EXPORT_FUNCTION(scx_Subtract_Region);
  S48_EXPORT_FUNCTION(scx_Xor_Region);
  S48_EXPORT_FUNCTION(scx_Offset_Region);
  S48_EXPORT_FUNCTION(scx_Shrink_Region);
  S48_EXPORT_FUNCTION(scx_Polygon_Region);
  S48_EXPORT_FUNCTION(scx_Set_Region);
}

