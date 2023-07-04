/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_coord_mode_binding = S48_FALSE;
#define scx_extract_coord_mode(x) S48_EXTRACT_ENUM(x, scx_coord_mode_binding)
s48_value scx_polygon_shape_binding = S48_FALSE;
#define scx_extract_polygon_shape(x) \
  S48_EXTRACT_ENUM(x, scx_polygon_shape_binding)

s48_value scx_Copy_Area(s48_value display, s48_value src, s48_value dest,
			s48_value gc, s48_value srcx, s48_value srcy,
			s48_value width, s48_value height, s48_value destx,
			s48_value desty) {
  S48_DECLARE_GC_PROTECT_10(display, src, dest, gc, srcx, srcy, width, height,
			    destx, desty);
  XCopyArea(scx_extract_display(display), scx_extract_drawable(src),
	    scx_extract_drawable(dest), scx_extract_gc(gc),
	    (int)s48_extract_integer(srcx), (int)s48_extract_integer(srcy),
	    (int)s48_extract_integer(width), (int)s48_extract_integer(height), 
	    (int)s48_extract_integer(destx), (int)s48_extract_integer(desty));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Copy_Plane(s48_value display, s48_value src, s48_value dest,
			 s48_value gc, s48_value srcx, s48_value srcy,
			 s48_value width, s48_value height,
			 s48_value destx, s48_value desty, s48_value plane) {
  S48_DECLARE_GC_PROTECT_11(display, src, dest, gc, srcx, srcy, width, height,
			    destx, desty, plane);
  XCopyPlane(scx_extract_display(display), scx_extract_drawable(src),
	     scx_extract_drawable(dest), scx_extract_gc(gc),
	     (int)s48_extract_integer(srcx), (int)s48_extract_integer(srcy),
	     (int)s48_extract_integer(width), (int)s48_extract_integer(height),
	     (int)s48_extract_integer(destx), (int)s48_extract_integer(desty),
	     (unsigned long)s48_extract_integer(plane));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Draw_Point(s48_value display, s48_value drawable,
			 s48_value gc, s48_value x, s48_value y) {
  S48_DECLARE_GC_PROTECT_5(display, drawable, gc, x, y);
  XDrawPoint(scx_extract_display(display), scx_extract_drawable(drawable),
	     scx_extract_gc(gc), (int)s48_extract_integer(x),
	     (int)s48_extract_integer(y));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

static void List_To_XPoints(s48_value l, XPoint* p, int n) {
  int i;
  S48_DECLARE_GC_PROTECT_1(l);
  for (i = 0; i < n; i++) {
    s48_value point = S48_CAR(l);
    p[i].x = (int)s48_extract_integer(S48_CAR(point));
    p[i].y = (int)s48_extract_integer(S48_CDR(point));
    l = S48_CDR(l);
  }
  S48_GC_UNPROTECT();
}

s48_value scx_Draw_Points(s48_value display, s48_value drawable,
			  s48_value gc, s48_value points, s48_value mode) {
  int n = s48_list_length(points);
  XPoint p[n];
  S48_DECLARE_GC_PROTECT_5(display, drawable, gc, points, mode);
  List_To_XPoints(points, p, n);
  XDrawPoints(scx_extract_display(display), scx_extract_drawable(drawable),
	      scx_extract_gc(gc), p, n,
	      scx_extract_coord_mode(mode));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Draw_Line(s48_value display, s48_value drawable,
			s48_value gc, s48_value x1, s48_value y1,
			s48_value x2, s48_value y2) {
  S48_DECLARE_GC_PROTECT_7(display, drawable, gc, x1, y1, x2, y2);
  XDrawLine(scx_extract_display(display),scx_extract_drawable(drawable),
	    scx_extract_gc(gc), (int)s48_extract_integer(x1),
	    (int)s48_extract_integer(y1), (int)s48_extract_integer(x2),
	    (int)s48_extract_integer(y2));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Draw_Lines(s48_value display, s48_value drawable,
			 s48_value gc, s48_value points, s48_value mode) {
  int n = s48_list_length(points);
  XPoint p[n];
  S48_DECLARE_GC_PROTECT_5(display, drawable, gc, points, mode);
  List_To_XPoints(points, p, n);
  XDrawLines(scx_extract_display(display), scx_extract_drawable(drawable),
	     scx_extract_gc(gc), p, n,
	     scx_extract_coord_mode(mode));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_segment_binding = S48_FALSE;

static void List_To_XSegments(s48_value l, XSegment* p, int n) {
  int i;
  S48_DECLARE_GC_PROTECT_1(l);
  for (i = 0; i < n; i++) {
    s48_value s = S48_CAR(l);
    s48_check_record_type(s, scx_segment_binding);
    p[i].x1 = (int)s48_extract_integer(S48_RECORD_REF(s, 0));
    p[i].y1 = (int)s48_extract_integer(S48_RECORD_REF(s, 1));
    p[i].x2 = (int)s48_extract_integer(S48_RECORD_REF(s, 2));
    p[i].y2 = (int)s48_extract_integer(S48_RECORD_REF(s, 3));
    l = S48_CDR(l);
  }
  S48_GC_UNPROTECT();
}

s48_value scx_Draw_Segments(s48_value display, s48_value drawable,
			    s48_value gc, s48_value segs) {
  int n = s48_list_length(segs);
  XSegment p[n];
  S48_DECLARE_GC_PROTECT_4(display, drawable, gc, segs);
  List_To_XSegments(segs, p, n);
  XDrawSegments(scx_extract_display(display), scx_extract_drawable(drawable),
		scx_extract_gc(gc), p, n);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Draw_Rectangle(s48_value display, s48_value drawable, 
			     s48_value gc, s48_value x, s48_value y,
			     s48_value width, s48_value height) {
  S48_DECLARE_GC_PROTECT_7(display, drawable, gc, x, y, width, height);
  XDrawRectangle(scx_extract_display(display), scx_extract_drawable(drawable),
		 scx_extract_gc(gc), 
		 (int)s48_extract_integer(x),
		 (int)s48_extract_integer(y),
		 (int)s48_extract_integer(width),
		 (int)s48_extract_integer(height));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_rectangle_binding = S48_FALSE;

static void List_To_XRectangles(s48_value l, XRectangle* p, int n) {
  int i;
  S48_DECLARE_GC_PROTECT_1(l);
  for (i = 0; i < n; i++) {
    s48_value r = S48_CAR(l);
    s48_check_record_type(r, scx_rectangle_binding);
    p[i].x = (int)s48_extract_integer(S48_RECORD_REF(r, 0));
    p[i].y = (int)s48_extract_integer(S48_RECORD_REF(r, 1));
    p[i].width = (int)s48_extract_integer(S48_RECORD_REF(r, 2));
    p[i].height = (int)s48_extract_integer(S48_RECORD_REF(r, 3));
    l = S48_CDR(l);
  }
  S48_GC_UNPROTECT();
}

s48_value scx_Draw_Rectangles(s48_value display, s48_value drawable,
			      s48_value gc, s48_value rects) {
  int n = s48_list_length(rects);
  XRectangle p[n];
  S48_DECLARE_GC_PROTECT_4(display, drawable, gc, rects);
  List_To_XRectangles(rects, p, n);
  XDrawRectangles(scx_extract_display(display), scx_extract_drawable(drawable),
		  scx_extract_gc(gc), p, n);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Draw_Arc(s48_value display, s48_value drawable,
		       s48_value gc, s48_value x, s48_value y, s48_value w,
		       s48_value h, s48_value a1, s48_value a2) {
  S48_DECLARE_GC_PROTECT_9(display, drawable, gc, x, y, w, h, a1, a2);
  XDrawArc(scx_extract_display(display), scx_extract_drawable(drawable),
	   scx_extract_gc(gc), (int)s48_extract_integer(x),
	   (int)s48_extract_integer(y), (int)s48_extract_integer(w),
	   (int)s48_extract_integer(h), (int)s48_extract_integer(a1),
	   (int)s48_extract_integer(a2));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_arc_binding = S48_FALSE;

static void List_To_XArcs(s48_value l, XArc* p, int n) {
  int i;
  S48_DECLARE_GC_PROTECT_1(l);
  for (i = 0; i < n; i++) {
    s48_value r = S48_CAR(l);
    s48_check_record_type(r, scx_arc_binding);
    p[i].x = (int)s48_extract_integer(S48_RECORD_REF(r, 0));
    p[i].y = (int)s48_extract_integer(S48_RECORD_REF(r, 1));
    p[i].width = (int)s48_extract_integer(S48_RECORD_REF(r, 2));
    p[i].height = (int)s48_extract_integer(S48_RECORD_REF(r, 3));
    p[i].angle1 = (int)s48_extract_integer(S48_RECORD_REF(r, 4));
    p[i].angle2 = (int)s48_extract_integer(S48_RECORD_REF(r, 5));
    l = S48_CDR(l);
  }
  S48_GC_UNPROTECT();
}

s48_value scx_Draw_Arcs(s48_value display, s48_value drawable,
			s48_value gc, s48_value arcs) {
  int n = s48_list_length(arcs);
  XArc p[n];
  S48_DECLARE_GC_PROTECT_4(display, drawable, gc, arcs);
  List_To_XArcs(arcs, p, n);
  XDrawArcs(scx_extract_display(display), scx_extract_drawable(drawable),
	    scx_extract_gc(gc), p, n);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Fill_Rectangle(s48_value display, s48_value drawable, 
			     s48_value gc, s48_value x, s48_value y,
			     s48_value width, s48_value height) {
  S48_DECLARE_GC_PROTECT_7(display, drawable, gc, x, y, width, height);
  XFillRectangle(scx_extract_display(display), scx_extract_drawable(drawable),
		 scx_extract_gc(gc), 
		 (int)s48_extract_integer(x),
		 (int)s48_extract_integer(y),
		 (int)s48_extract_integer(width),
		 (int)s48_extract_integer(height));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Fill_Rectangles(s48_value display, s48_value drawable,
			      s48_value gc, s48_value rects) {
  int n = s48_list_length(rects);
  XRectangle p[n];
  S48_DECLARE_GC_PROTECT_4(display, drawable, gc, rects);
  List_To_XRectangles(rects, p, n);
  XFillRectangles(scx_extract_display(display), scx_extract_drawable(drawable),
		  scx_extract_gc(gc), p, n);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Fill_Polygon(s48_value display, s48_value drawable,
			   s48_value gc, s48_value points,
			   s48_value shape, s48_value mode) {
  int n = s48_list_length(points);
  XPoint p[n];
  S48_DECLARE_GC_PROTECT_6(display, drawable, gc, points, shape, mode);
  List_To_XPoints(points, p, n);
  XFillPolygon(scx_extract_display(display), scx_extract_drawable(drawable),
	       scx_extract_gc(gc), p, n,
	       scx_extract_polygon_shape(shape),
	       scx_extract_coord_mode(mode));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Fill_Arc(s48_value display, s48_value drawable,
		       s48_value gc, s48_value x, s48_value y, s48_value w,
		       s48_value h, s48_value a1, s48_value a2) {
  S48_DECLARE_GC_PROTECT_9(display, drawable, gc, x, y, w, h, a1, a2);
  XFillArc(scx_extract_display(display), scx_extract_drawable(drawable),
	   scx_extract_gc(gc), (int)s48_extract_integer(x),
	   (int)s48_extract_integer(y), (int)s48_extract_integer(w),
	   (int)s48_extract_integer(h), (int)s48_extract_integer(a1),
	   (int)s48_extract_integer(a2));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Fill_Arcs(s48_value display, s48_value drawable,
			s48_value gc, s48_value arcs) {
  int n = s48_list_length(arcs);
  XArc p[n];
  S48_DECLARE_GC_PROTECT_4(display, drawable, gc, arcs);
  List_To_XArcs(arcs, p, n);
  XFillArcs(scx_extract_display(display), scx_extract_drawable(drawable),
	    scx_extract_gc(gc), p, n);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

void scx_init_graphics(void) {
  SCX_PRO_IMP(scx_coord_mode_binding, "scx-coord-mode");
  SCX_PRO_IMP(scx_polygon_shape_binding, "scx-polygon-shape");
  SCX_PRO_IMP(scx_segment_binding, "scx-segment");
  SCX_PRO_IMP(scx_rectangle_binding, "scx-rectangle");
  SCX_PRO_IMP(scx_arc_binding, "scx-arc");

  S48_EXPORT_FUNCTION(scx_Copy_Area);
  S48_EXPORT_FUNCTION(scx_Copy_Plane);
  S48_EXPORT_FUNCTION(scx_Draw_Point);
  S48_EXPORT_FUNCTION(scx_Draw_Points);
  S48_EXPORT_FUNCTION(scx_Draw_Line);
  S48_EXPORT_FUNCTION(scx_Draw_Lines);
  S48_EXPORT_FUNCTION(scx_Draw_Segments);
  S48_EXPORT_FUNCTION(scx_Draw_Rectangle);
  S48_EXPORT_FUNCTION(scx_Draw_Rectangles);
  S48_EXPORT_FUNCTION(scx_Draw_Arc);
  S48_EXPORT_FUNCTION(scx_Draw_Arcs);
  S48_EXPORT_FUNCTION(scx_Fill_Rectangle);
  S48_EXPORT_FUNCTION(scx_Fill_Rectangles);
  S48_EXPORT_FUNCTION(scx_Fill_Polygon);
  S48_EXPORT_FUNCTION(scx_Fill_Arc);
  S48_EXPORT_FUNCTION(scx_Fill_Arcs);
}
