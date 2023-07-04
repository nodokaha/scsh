#include <X11/extensions/Xrender.h>
#include <scheme48.h>
#include <xlib.h>

/* add more stuff later */

static s48_value scx_xrender_glyphinfo_record_type		= S48_FALSE;
static s48_value scx_xrender_xrendercolor_record_type		= S48_FALSE;

s48_value scx_enter_glyphinfo(XGlyphInfo *i);
#define scx_extract_glyphinfo(x) \
	((XGlyphInfo *) s48_extract_integer(s48_checked_record_ref(x, 0, scx_xrender_glyphinfo_record_type)))

s48_value scx_enter_xrendercolor(XRenderColor *xrc);
#define scx_extract_xrendercolor(x) \
	((XRenderColor *) s48_extract_integer(s48_checked_record_ref(x, 0, scx_xrender_xrendercolor_record_type)))


