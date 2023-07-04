#include "xrender.h"

/* XGlyphInfo */

s48_value scx_enter_glyphinfo(XGlyphInfo *i)
{
  s48_value girec = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(girec);
  girec = s48_make_record(scx_xrender_glyphinfo_record_type);
  S48_RECORD_SET(girec, 0, s48_enter_integer((long) i));
  S48_GC_UNPROTECT();
  return girec;
}

#define XGLYPHINFO_GET(FUN, FN) \
	s48_value FUN(s48_value sv) { \
	  return s48_enter_integer(scx_extract_glyphinfo(sv)->FN); \
        }

#define XGLYPHINFO_SET(FUN, FN) \
	s48_value FUN(s48_value sv, s48_value nv) {			\
	  S48_DECLARE_GC_PROTECT(2);					\
									\
	  S48_GC_PROTECT_2(sv, nv);					\
	  scx_extract_glyphinfo(sv)->FN = s48_enter_integer(nv);	\
	  S48_GC_UNPROTECT();						\
	  return S48_UNSPECIFIC; \
	}

XGLYPHINFO_GET(scx_xglyphinfo_width_get, width);
XGLYPHINFO_GET(scx_xglyphinfo_height_get, height);
XGLYPHINFO_GET(scx_xglyphinfo_x_get, x);
XGLYPHINFO_GET(scx_xglyphinfo_y_get, y);
XGLYPHINFO_GET(scx_xglyphinfo_xOff_get, xOff);
XGLYPHINFO_GET(scx_xglyphinfo_yOff_get, yOff);

XGLYPHINFO_SET(scx_xglyphinfo_width_set, width);
XGLYPHINFO_SET(scx_xglyphinfo_height_set, height);
XGLYPHINFO_SET(scx_xglyphinfo_x_set, x);
XGLYPHINFO_SET(scx_xglyphinfo_y_set, y);
XGLYPHINFO_SET(scx_xglyphinfo_xOff_set, xOff);
XGLYPHINFO_SET(scx_xglyphinfo_yOff_set, yOff);

/* XRenderColor */

s48_value scx_enter_xrendercolor(XRenderColor *xrc)
{
  s48_value rcrec = S48_UNSPECIFIC;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(rcrec);
  rcrec = s48_make_record(scx_xrender_xrendercolor_record_type);
  S48_RECORD_SET(rcrec, 0, s48_enter_integer((long) xrc));
  S48_GC_UNPROTECT();
  return rcrec;
}

#define XRENDERCOLOR_GET(FUN, FN) \
	s48_value FUN(s48_value sv) { \
	  return s48_enter_integer(scx_extract_xrendercolor(sv)->FN); \
	}

#define XRENDERCOLOR_SET(FUN, FN) \
	s48_value FUN(s48_value sv, s48_value nv) {			\
	  S48_DECLARE_GC_PROTECT(2);					\
									\
	  S48_GC_PROTECT_2(sv, nv);					\
	  scx_extract_xrendercolor(sv)->FN = s48_enter_integer(nv);	\
	  S48_GC_UNPROTECT();						\
	  return S48_UNSPECIFIC; \
	}

XRENDERCOLOR_GET(scx_xrendercolor_red_get, red);
XRENDERCOLOR_GET(scx_xrendercolor_green_get, green);
XRENDERCOLOR_GET(scx_xrendercolor_blue_get, blue);
XRENDERCOLOR_GET(scx_xrendercolor_alpha_get, alpha);

XRENDERCOLOR_SET(scx_xrendercolor_red_set, red);
XRENDERCOLOR_SET(scx_xrendercolor_green_set, green);
XRENDERCOLOR_SET(scx_xrendercolor_blue_set, blue);
XRENDERCOLOR_SET(scx_xrendercolor_alpha_set, alpha);

void scx_xrender_init(void)
{
  S48_GC_PROTECT_GLOBAL(scx_xrender_glyphinfo_record_type);
  scx_xrender_glyphinfo_record_type = s48_get_imported_binding("xglyphinfo");

  S48_GC_PROTECT_GLOBAL(scx_xrender_xrendercolor_record_type);
  scx_xrender_xrendercolor_record_type = s48_get_imported_binding("xrendercolor");

  S48_EXPORT_FUNCTION(scx_xglyphinfo_width_get);
  S48_EXPORT_FUNCTION(scx_xglyphinfo_height_get);
  S48_EXPORT_FUNCTION(scx_xglyphinfo_x_get);
  S48_EXPORT_FUNCTION(scx_xglyphinfo_y_get);
  S48_EXPORT_FUNCTION(scx_xglyphinfo_xOff_get);
  S48_EXPORT_FUNCTION(scx_xglyphinfo_yOff_get);

  S48_EXPORT_FUNCTION(scx_xglyphinfo_width_set);
  S48_EXPORT_FUNCTION(scx_xglyphinfo_height_set);
  S48_EXPORT_FUNCTION(scx_xglyphinfo_x_set);
  S48_EXPORT_FUNCTION(scx_xglyphinfo_y_set);
  S48_EXPORT_FUNCTION(scx_xglyphinfo_xOff_set);
  S48_EXPORT_FUNCTION(scx_xglyphinfo_yOff_set);

  S48_EXPORT_FUNCTION(scx_xrendercolor_red_get);
  S48_EXPORT_FUNCTION(scx_xrendercolor_green_get);
  S48_EXPORT_FUNCTION(scx_xrendercolor_blue_get);
  S48_EXPORT_FUNCTION(scx_xrendercolor_alpha_get);

  S48_EXPORT_FUNCTION(scx_xrendercolor_red_set);
  S48_EXPORT_FUNCTION(scx_xrendercolor_green_set);
  S48_EXPORT_FUNCTION(scx_xrendercolor_blue_set);
  S48_EXPORT_FUNCTION(scx_xrendercolor_alpha_set);
}
