#include <X11/Xft/Xft.h>
#include <scheme48.h>
#include <xlib.h>
#include <xrender.h>

#ifdef XFT_VERSION
#define SCX_XFT_VERSION XFT_MAJOR
#define SCX_XFT_VERSION_MAJOR XFT_MAJOR
#define SCX_XFT_VERSION_MINOR XFT_MINOR
#else
#define SCX_XFT_VERSION 1
#define SCX_XFT_VERSION_MAJOR 1
#define SCX_XFT_VERSION_MINOR 0
#endif

#define SCX_XFT_FAMILY	    0	/* String */
#define SCX_XFT_STYLE	    1	/* String */
#define SCX_XFT_SLANT	    2	/* Int */
#define SCX_XFT_WEIGHT	    3	/* Int */
#define SCX_XFT_SIZE	    4	/* Double */
#define SCX_XFT_PIXEL_SIZE  5	/* Double */
#define SCX_XFT_ENCODING    6	/* String */
#define SCX_XFT_SPACING	    7	/* Int */
#define SCX_XFT_FOUNDRY	    8	/* String */
#define SCX_XFT_CORE	    9	/* Bool */
#define SCX_XFT_ANTIALIAS   10	/* Bool */
#define SCX_XFT_XLFD	    11	/* String */
#define SCX_XFT_FILE	    12	/* String */
#define SCX_XFT_INDEX	    13	/* Int */
#define SCX_XFT_RASTERIZER  14  /* String */
#define SCX_XFT_OUTLINE	    15	/* Bool */
#define SCX_XFT_SCALABLE    16	/* Bool */
#define SCX_XFT_RGBA	    17	/* Int */
#define SCX_XFT_SCALE	    18	/* double */
#define SCX_XFT_RENDER	    19	/* Bool */
#define SCX_XFT_MINSPACE    20	/* Bool use minimum line spacing */
#define SCX_XFT_DPI	    21 	/* double */
#define SCX_XFT_CHAR_WIDTH  22	/* Int */
#define SCX_XFT_CHAR_HEIGHT 23  /* Int */

enum xft_pattern_get_return_type {
  SCX_XFT_INVALID,
  SCX_XFT_UNIMPLEMENTED,
  SCX_XFT_STRING,
  SCX_XFT_DOUBLE,
  SCX_XFT_INT,
  SCX_XFT_BOOL
};

struct xft_pattern_property {
  int scx_id;
  char *name;
  enum xft_pattern_get_return_type type;
};

static struct xft_pattern_property xft_pattern_property_tbl [] = {
  {SCX_XFT_FAMILY,		XFT_FAMILY,		SCX_XFT_STRING},
  {SCX_XFT_STYLE,		XFT_STYLE,		SCX_XFT_STRING},
  {SCX_XFT_SLANT,		XFT_SLANT,		SCX_XFT_INT},
  {SCX_XFT_WEIGHT,		XFT_WEIGHT,		SCX_XFT_INT},
  {SCX_XFT_SIZE,		XFT_SIZE,		SCX_XFT_DOUBLE},
  {SCX_XFT_PIXEL_SIZE,		XFT_PIXEL_SIZE,		SCX_XFT_DOUBLE},
#if SCX_XFT_VERSION < 2
  {SCX_XFT_ENCODING,		XFT_ENCODING,		SCX_XFT_STRING},
#else
  {SCX_XFT_ENCODING,		XFT_ENCODING,		SCX_XFT_UNIMPLEMENTED},
#endif
  {SCX_XFT_SPACING,		XFT_SPACING,		SCX_XFT_INT},
  {SCX_XFT_FOUNDRY,		XFT_FOUNDRY,		SCX_XFT_STRING},
#if SCX_XFT_VERSION < 2
  {SCX_XFT_CORE,		XFT_CORE,		SCX_XFT_BOOL},
#else
  {SCX_XFT_CORE,		XFT_CORE,		SCX_XFT_UNIMPLEMENTED},
#endif
  {SCX_XFT_ANTIALIAS,		XFT_ANTIALIAS,		SCX_XFT_BOOL},
  {SCX_XFT_XLFD,		XFT_XLFD,		SCX_XFT_STRING},
  {SCX_XFT_FILE,		XFT_FILE,		SCX_XFT_STRING},
  {SCX_XFT_INDEX,		XFT_INDEX,		SCX_XFT_INT},
  {SCX_XFT_RASTERIZER,		XFT_RASTERIZER,		SCX_XFT_STRING},
  {SCX_XFT_OUTLINE,		XFT_OUTLINE,		SCX_XFT_STRING},
  {SCX_XFT_SCALABLE,		XFT_SCALABLE,		SCX_XFT_BOOL},
  {SCX_XFT_RGBA,		XFT_RGBA,		SCX_XFT_INT},
  {SCX_XFT_SCALE,		XFT_SCALE,		SCX_XFT_DOUBLE},
  {SCX_XFT_RENDER,		XFT_RENDER,		SCX_XFT_BOOL},
  {SCX_XFT_MINSPACE,		XFT_MINSPACE,		SCX_XFT_BOOL},
  {SCX_XFT_DPI,			XFT_DPI,		SCX_XFT_DOUBLE},
#if SCX_XFT_VERSION < 2
  {SCX_XFT_CHAR_WIDTH,		XFT_CHAR_WIDTH,		SCX_XFT_INT},
  {SCX_XFT_CHAR_HEIGHT,		XFT_CHAR_HEIGHT,	SCX_XFT_INT},
#else
  {SCX_XFT_CHAR_WIDTH,		XFT_CHAR_WIDTH,		SCX_XFT_UNIMPLEMENTED},
  {SCX_XFT_CHAR_HEIGHT,		XFT_CHAR_HEIGHT,	SCX_XFT_UNIMPLEMENTED},
#endif
  {NULL,			NULL,			SCX_XFT_INVALID}
};

/* declare static s48_value initialized with S48_FALSE */
#define SCX_DECLARE_STATIC_S48VAL(NAME) static s48_value NAME = S48_FALSE

#define XFT_GC_PROTECT_IMPORT_BINDING(CN, SN)		\
	S48_GC_PROTECT_GLOBAL(CN);			\
	CN = s48_get_imported_binding(SN);

/* scheme record types */
SCX_DECLARE_STATIC_S48VAL(scx_xftpattern_record_type);
SCX_DECLARE_STATIC_S48VAL(scx_xftfont_record_type);
SCX_DECLARE_STATIC_S48VAL(scx_xftdraw_record_type);
SCX_DECLARE_STATIC_S48VAL(scx_xftcolor_record_type);
SCX_DECLARE_STATIC_S48VAL(scx_xftobjectset_record_type);
SCX_DECLARE_STATIC_S48VAL(scx_xftfontset_record_type);

/* C values exported to scheme */
SCX_DECLARE_STATIC_S48VAL(scx_XftResultMatch);
SCX_DECLARE_STATIC_S48VAL(scx_XftResultNoMatch);
SCX_DECLARE_STATIC_S48VAL(scx_XftResultTypeMismatch);
SCX_DECLARE_STATIC_S48VAL(scx_XftResultNoId);
SCX_DECLARE_STATIC_S48VAL(scx_XftVersionMajor);
SCX_DECLARE_STATIC_S48VAL(scx_XftVersionMinor);

SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_family);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_style);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_slant);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_weight);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_size);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_pixel_size);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_encoding);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_spacing);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_foundry);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_core);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_antialias);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_xlfd);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_file);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_index);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_rasterizer);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_outline);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_scalable);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_rgba);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_scale);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_render);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_minspace);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_dpi);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_char_width);
SCX_DECLARE_STATIC_S48VAL(scx_xft_pattern_char_height);

SCX_DECLARE_STATIC_S48VAL(scx_xft_weight_light);
SCX_DECLARE_STATIC_S48VAL(scx_xft_weight_medium);
SCX_DECLARE_STATIC_S48VAL(scx_xft_weight_demibold);
SCX_DECLARE_STATIC_S48VAL(scx_xft_weight_bold);
SCX_DECLARE_STATIC_S48VAL(scx_xft_weight_black);

SCX_DECLARE_STATIC_S48VAL(scx_xft_slant_roman);
SCX_DECLARE_STATIC_S48VAL(scx_xft_slant_italic);
SCX_DECLARE_STATIC_S48VAL(scx_xft_slant_oblique);

SCX_DECLARE_STATIC_S48VAL(scx_xft_spacing_proportional);
SCX_DECLARE_STATIC_S48VAL(scx_xft_spacing_mono);
SCX_DECLARE_STATIC_S48VAL(scx_xft_spacing_charcell);

SCX_DECLARE_STATIC_S48VAL(scx_xft_rgba_none);
SCX_DECLARE_STATIC_S48VAL(scx_xft_rgba_rgb);
SCX_DECLARE_STATIC_S48VAL(scx_xft_rgba_bgr);
SCX_DECLARE_STATIC_S48VAL(scx_xft_rgba_vrgb);
SCX_DECLARE_STATIC_S48VAL(scx_xft_rgba_vbgr);

#define XFT_REC_ACCESSOR_MAKER(FN, TN, RN) \
	s48_value FN(TN *p)				\
	{						\
	  s48_value xftrec = S48_FALSE;			\
	  S48_DECLARE_GC_PROTECT(1);			\
							\
	  S48_GC_PROTECT_1(xftrec);			\
	  xftrec = s48_make_record(RN);			\
	  S48_RECORD_SET(xftrec, 0, s48_enter_integer((long) p)); \
	  S48_GC_UNPROTECT();				\
	  return xftrec;				\
	}

#define SCX_EXPORT_INTEGER_TO_S48(scheme_name, scheme_val, i) \
	S48_GC_PROTECT_GLOBAL(scheme_val); \
	scheme_val = s48_enter_integer(i); \
	s48_define_exported_binding(scheme_name, scheme_val);

/* function prototypes */
s48_value scx_enter_xftpattern(XftPattern *p);
#define scx_extract_xftpattern(x) \
	((XftPattern *) s48_extract_integer(s48_checked_record_ref(x, 0, scx_xftpattern_record_type)))

s48_value scx_enter_xftfont(XftFont *f, s48_value dpy);
#define scx_extract_xftfont(x) \
	((XftFont *) s48_extract_integer(s48_checked_record_ref(x, 0, scx_xftfont_record_type)))

s48_value scx_enter_xftdraw(XftDraw *d);
#define scx_extract_xftdraw(x) \
	((XftDraw *) s48_extract_integer(s48_checked_record_ref(x, 0, scx_xftdraw_record_type)))

s48_value scx_enter_xftcolor(XftColor *c);
#define scx_extract_xftcolor(x) \
	((XftColor *) s48_extract_integer(s48_checked_record_ref(x, 0, scx_xftcolor_record_type)))

s48_value scx_enter_xftobjectset(XftObjectSet *os);
#define scx_extract_xftobjectset(x) \
	((XftObjectSet *) s48_extract_integer(s48_checked_record_ref(x, 0, scx_xftobjectset_record_type)))

s48_value scx_enter_xftfontset(XftFontSet *fs);
#define scx_extract_xftfontset(x) \
	((XftFontSet *) s48_extract_integer(s48_checked_record_ref(x, 0, scx_xftfontset_record_type)))

struct xft_pattern_property* lookup_pattern_property_by_id(int id);

