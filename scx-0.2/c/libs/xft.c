#include "xft.h"

XFT_REC_ACCESSOR_MAKER(scx_enter_xftpattern, XftPattern, scx_xftpattern_record_type);

XFT_REC_ACCESSOR_MAKER(scx_enter_xftdraw, XftDraw, scx_xftdraw_record_type);

XFT_REC_ACCESSOR_MAKER(scx_enter_xftcolor, XftColor, scx_xftcolor_record_type);

XFT_REC_ACCESSOR_MAKER(scx_enter_xftobjectset, XftObjectSet, scx_xftobjectset_record_type);

XFT_REC_ACCESSOR_MAKER(scx_enter_xftfontset, XftFontSet, scx_xftfontset_record_type);

s48_value scx_enter_xftfont(XftFont *xf, s48_value dpy)
{
  s48_value rec = S48_FALSE;
  S48_DECLARE_GC_PROTECT(2);
  
  S48_GC_PROTECT_2(rec, dpy);
  rec = s48_make_record(scx_xftfont_record_type);
  S48_RECORD_SET(rec, 0, s48_enter_integer((long) xf));
  S48_RECORD_SET(rec, 1, scx_enter_xftpattern(xf->pattern));
  S48_RECORD_SET(rec, 2, dpy);
  S48_GC_UNPROTECT();
  return rec;
}

s48_value scx_XftPatternCreate(void) 
{
  return scx_enter_xftpattern(XftPatternCreate());
}


s48_value scx_XftPatternDestroy(s48_value sxp)
{
  XftPatternDestroy(scx_extract_xftpattern(sxp));
  return S48_UNSPECIFIC;
}

s48_value scx_XftPatternDuplicate(s48_value sxp)
{
  XftPattern *p;

  p = XftPatternDuplicate(scx_extract_xftpattern(sxp));
  return scx_enter_xftpattern(p);
}

struct xft_pattern_property* lookup_pattern_property_by_id(int id)
{
  struct xft_pattern_property *tbl;

  for (tbl = &xft_pattern_property_tbl[0]; 
       (tbl->scx_id != id) && (tbl->type != SCX_XFT_INVALID); 
       tbl++)
    ;
  return tbl;
}

s48_value scx_XftPatternGet(s48_value sxp, s48_value sobj, s48_value sid)
{
  XftPattern *p;
  XftResult rc;
  XftValue v;
  int obj, i;
  s48_value rv = S48_UNSPECIFIC;
  s48_value rl = S48_UNSPECIFIC;
  struct xft_pattern_property *tbl;
  S48_DECLARE_GC_PROTECT(5);

  S48_GC_PROTECT_5(rl, rv, sxp, sobj, sid);
  p = scx_extract_xftpattern(sxp);
  obj = s48_extract_integer(sobj);
  i = (int) s48_extract_integer(sid);

  tbl = lookup_pattern_property_by_id(obj);
  rc = XftPatternGet(p, tbl->name, i, &v);

  if (rc != XftResultMatch)
    {
      rl = s48_list_2(s48_enter_integer(rc), S48_UNSPECIFIC);
      S48_GC_UNPROTECT();
      return rl;
    }
  
  switch (tbl->type) {
  case SCX_XFT_STRING:
    rv = s48_enter_string(v.u.s);
    break;
  case SCX_XFT_DOUBLE:
    rv = s48_enter_double(v.u.d);
    break;
  case SCX_XFT_INT:
    rv = s48_enter_integer(v.u.i);
    break;
  case SCX_XFT_BOOL:
    rv = v.u.b ? S48_TRUE : S48_FALSE;
    break;
  case SCX_XFT_UNIMPLEMENTED:
    rv = S48_UNSPECIFIC;
    break;
  }

  rl = s48_list_2(s48_enter_integer(rc), rv);
  S48_GC_UNPROTECT();
  return rl;
}

s48_value scx_XftPatternAdd(s48_value sxp, s48_value sobj, 
			    s48_value sval, s48_value sappend)
{
  XftPattern *p;
  XftValue v;
  int obj;
  Bool append, rv;
  struct xft_pattern_property *tbl;
  S48_DECLARE_GC_PROTECT(4);

  S48_GC_PROTECT_4(sxp, sobj, sval, sappend);
  p = scx_extract_xftpattern(sxp);
  obj = s48_extract_integer(sobj);
  append = S48_TRUE_P(sappend);
  tbl = lookup_pattern_property_by_id(obj);

  switch (tbl->type) {
  case SCX_XFT_STRING:
    v.type = XftTypeString;
    v.u.s = s48_extract_string(sval);
    break;
  case SCX_XFT_DOUBLE:
    v.type = XftTypeDouble;
    v.u.d = s48_extract_double(sval);
    break;
  case SCX_XFT_INT:
    v.type = XftTypeInteger;
    v.u.i = s48_extract_integer(sval);
    break;
  case SCX_XFT_BOOL:
    v.type = XftTypeBool;
    v.u.b = S48_TRUE_P(sval) ? True : False;
    break;
  }
  rv = XftPatternAdd(p, tbl->name, v, append);

  S48_GC_UNPROTECT();
  return rv ? S48_TRUE : S48_FALSE;
}

s48_value scx_XftFontMatch(s48_value sdpy, s48_value sscreenno, s48_value sxp)
{
  XftPattern *p;
  XftResult r;
  S48_DECLARE_GC_PROTECT(3);
  
  S48_GC_PROTECT_3(sdpy, sscreenno, sxp);
  p = XftFontMatch((Display *) scx_extract_display(sdpy),
		   s48_extract_integer(sscreenno),
		   scx_extract_xftpattern(sxp),
		   &r);

  S48_GC_UNPROTECT();
  if (p == NULL)
  return s48_list_2(S48_FALSE, S48_FALSE);
  else
  return s48_list_2(s48_enter_integer(r), scx_enter_xftpattern(p));
}

s48_value scx_XftFontOpenPattern(s48_value sdpy, s48_value sxp)
{
  XftFont *xf;
  XftPattern *p;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sdpy, sxp);
  p = scx_extract_xftpattern(sxp);
  xf = XftFontOpenPattern(scx_extract_display(sdpy), p);
  S48_GC_UNPROTECT();
  return xf == NULL ? S48_FALSE : scx_enter_xftfont(xf, sdpy);
}

s48_value scx_XftFontOpenName(s48_value sdpy, s48_value sscreenno, s48_value sname)
{
  XftFont *xf;
  S48_DECLARE_GC_PROTECT(3);

  S48_GC_PROTECT_3(sdpy, sscreenno, sname);
  xf = XftFontOpenName(scx_extract_display(sdpy), s48_extract_integer(sscreenno),
		       s48_extract_string(sname));
  S48_GC_UNPROTECT();
  return xf == NULL ? S48_FALSE : scx_enter_xftfont(xf, sdpy);
}

s48_value scx_XftFontOpenXlfd(s48_value sdpy, s48_value sscreenno, s48_value sxlfd)
{
  XftFont *xf;
  S48_DECLARE_GC_PROTECT(3);

  S48_GC_PROTECT_3(sdpy, sscreenno, sxlfd);
  xf = XftFontOpenXlfd(scx_extract_display(sdpy), s48_extract_integer(sscreenno),
		       s48_extract_string(sxlfd));
  S48_GC_UNPROTECT();
  return xf == NULL ? S48_FALSE : scx_enter_xftfont(xf, sdpy);
}

s48_value scx_XftFontClose(s48_value sdpy, s48_value sxf)
{
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sdpy, sxf);
  XftFontClose(scx_extract_display(sdpy),
	       scx_extract_xftfont(sxf));
  S48_GC_UNPROTECT();
  return S48_UNSPECIFIC;
}
  
s48_value scx_XftDrawCreate(s48_value sdpy, s48_value sdrawable,
			    s48_value svisual, s48_value scolormap)
{
  XftDraw *d;
  S48_DECLARE_GC_PROTECT(4);

  S48_GC_PROTECT_4(sdpy, sdrawable, svisual, scolormap);
  d = XftDrawCreate(scx_extract_display(sdpy),
		    scx_extract_drawable(sdrawable),
		    scx_extract_visual(svisual),
		    scx_extract_colormap(scolormap));
  S48_GC_UNPROTECT();
  return scx_enter_xftdraw(d);
}

s48_value scx_XftDrawCreateBitmap(s48_value sdpy, s48_value sdrawable)
{
  XftDraw *d;
  S48_DECLARE_GC_PROTECT(2);
  
  S48_GC_PROTECT_2(sdpy, sdrawable);
  d = XftDrawCreateBitmap(scx_extract_display(sdpy),
			  scx_extract_drawable(sdrawable));
  S48_GC_UNPROTECT();
  return scx_enter_xftdraw(d);
}

s48_value scx_XftDrawChange(s48_value sxd, s48_value sdrawable)
{
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sxd, sdrawable);
  XftDrawChange(scx_extract_xftdraw(sxd), 
		scx_extract_drawable(sdrawable));
  S48_GC_UNPROTECT();
  return S48_UNSPECIFIC;
}


s48_value scx_XftDrawDisplay(s48_value sxd)
{
#if SCX_XFT_VERSION > 1
  return scx_enter_display(XftDrawDisplay(scx_extract_xftdraw(sxd)));
#else
  return S48_FALSE;
#endif
}

s48_value scx_XftDrawDrawable(s48_value sxd)
{
#if SCX_XFT_VERSION > 1
  return scx_enter_drawable(XftDrawDrawable(scx_extract_xftdraw(sxd)));
#else
  return S48_FALSE;
#endif
}

s48_value scx_XftDrawColormap(s48_value sxd)
{
#if SCX_XFT_VERSION > 1
  return scx_enter_colormap(XftDrawColormap(scx_extract_xftdraw(sxd)));
#else
  return S48_FALSE;
#endif
}

s48_value scx_XftDrawVisual(s48_value sxd)
{
#if SCX_XFT_VERSION > 1
  return scx_enter_visual(XftDrawVisual(scx_extract_xftdraw(sxd)));
#else
  return S48_FALSE;
#endif
}
  
s48_value scx_XftDrawDestroy(s48_value sxd)
{
  XftDrawDestroy(scx_extract_xftdraw(sxd));
  return S48_UNSPECIFIC;
}

s48_value scx_XftTextExtents8(s48_value sdpy, s48_value sxf, s48_value sstr)
{
  XGlyphInfo *extents;
  S48_DECLARE_GC_PROTECT(3);

  S48_GC_PROTECT_3(sdpy, sxf, sstr);
  XftTextExtents8(scx_extract_display(sdpy),
		  scx_extract_xftfont(sxf),
		  (XftChar8 *) s48_extract_string(sstr),
		  S48_STRING_LENGTH(sstr),
		  extents);

  S48_GC_UNPROTECT();
  return scx_enter_glyphinfo(extents);
}

s48_value scx_XftDrawString8(s48_value sxd, s48_value sxc,
			     s48_value sxf, s48_value sx,
			     s48_value sy, s48_value sstr)
{
  S48_DECLARE_GC_PROTECT(6);

  S48_GC_PROTECT_6(sxd, sxc, sxf, sx, sy, sstr);
  XftDrawString8(scx_extract_xftdraw(sxd), scx_extract_xftcolor(sxc),
		 scx_extract_xftfont(sxf), 
		 s48_extract_integer(sx), s48_extract_integer(sy), 
		 (XftChar8 *) s48_extract_string(sstr),
		 S48_STRING_LENGTH(sstr));
  S48_GC_UNPROTECT();
  return S48_UNSPECIFIC;
}

s48_value scx_XftDrawRect(s48_value sxd, s48_value sxc,
			  s48_value sx, s48_value sy,
			  s48_value sw, s48_value sh)
{
  S48_DECLARE_GC_PROTECT(6);

  S48_GC_PROTECT_6(sxd, sxc, sx, sy, sw, sh);
  XftDrawRect(scx_extract_xftdraw(sxd), scx_extract_xftcolor(sxc),
	      s48_extract_integer(sx), s48_extract_integer(sy),
	      s48_extract_integer(sw), s48_extract_integer(sh));
  S48_GC_UNPROTECT();
  return S48_UNSPECIFIC;
}

s48_value scx_XftDrawSetClip(s48_value sxd, s48_value sreg)
{
  Bool b;
  S48_DECLARE_GC_PROTECT(2);
  
  S48_GC_PROTECT_2(sxd, sreg);
  b = XftDrawSetClip(scx_extract_xftdraw(sxd), scx_extract_region(sreg));
  S48_GC_UNPROTECT();
  return b ? S48_TRUE : S48_FALSE;
}

s48_value scx_XftObjectSetCreate(void)
{
  return scx_enter_xftobjectset(XftObjectSetCreate());
}

s48_value scx_XftObjectSetDestroy(s48_value sxo)
{
  XftObjectSetDestroy(scx_extract_xftobjectset(sxo));
  return S48_UNSPECIFIC;
}

s48_value scx_XftObjectSetAdd(s48_value sxo, s48_value sobj)
{
  struct xft_pattern_property *tbl;
  Bool b;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sxo, sobj);
  tbl = lookup_pattern_property_by_id(s48_extract_integer(sobj));
  b = XftObjectSetAdd(scx_extract_xftobjectset(sxo), tbl->name);
  S48_GC_UNPROTECT();
  return b ? S48_TRUE : S48_FALSE;
}

s48_value scx_XftListFontsPatternObjects(s48_value sdpy, s48_value sscreenno,
					 s48_value sxp, s48_value sxo)
{
  XftFontSet *fs;
#if XFT_VERSION > 1
  FcConfig *fcc;
#endif
  S48_DECLARE_GC_PROTECT(4);

  S48_GC_PROTECT_4(sdpy, sscreenno, sxp, sxo);
#if XFT_VERSION > 1
  fcc = FcConfigGetCurrent();
  fs = FcFontList(fcc, scx_extract_xftpattern(sxp),
		  scx_extract_xftobjectset(sxo));
#else
  fs = XftListFontsPatternObjects(scx_extract_display(sdpy),
				  s48_extract_integer(sscreenno),
				  scx_extract_xftpattern(sxp),
				  scx_extract_xftobjectset(sxo));
#endif
  S48_GC_UNPROTECT();
  return scx_enter_xftfontset(fs);
}

s48_value scx_XftFontSetCreate(void)
{
#if XFT_VERSION > 1
  return scx_enter_xftfontset(FcFontSetCreate());
#else
  return scx_enter_xftfontset(XftFontSetCreate());
#endif
}

s48_value scx_XftFontSetDestroy(s48_value sxfs)
{
  XftFontSetDestroy(scx_extract_xftfontset(sxfs));
  return S48_UNSPECIFIC;
}

s48_value scx_XftFontSetAdd(s48_value sxfs, s48_value sxp)
{
  Bool b;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sxfs, sxp);
#if XFT_VERSION > 1
  b = FcFontSetAdd(scx_extract_xftfontset(sxfs),
		   scx_extract_xftpattern(sxp));
#else
  b = XftFontSetAdd(scx_extract_xftfontset(sxfs),
		    scx_extract_xftpattern(sxp));
#endif
  S48_GC_UNPROTECT();
  return b ? S48_TRUE : S48_FALSE;
}

s48_value scx_XftColorAllocName(s48_value sdpy, s48_value svisual,
				s48_value scolormap, s48_value sname)
{
  s48_value res = S48_FALSE;
  XftColor *xftcolor;
  Bool b = False;
  S48_DECLARE_GC_PROTECT(5);

  S48_GC_PROTECT_5(res, sdpy, svisual, scolormap, sname);
  if ((xftcolor = (XftColor *) calloc(1, sizeof(XftColor))) != NULL)
    b = XftColorAllocName(scx_extract_display(sdpy), scx_extract_visual(svisual),
			  scx_extract_colormap(scolormap), s48_extract_string(sname),
			  xftcolor);
  res = s48_list_2(b ? S48_TRUE : S48_FALSE, 
		   b ? scx_enter_xftcolor(xftcolor) : S48_FALSE);
  S48_GC_UNPROTECT();
  return res;
}

s48_value scx_XftColorAllocValue(s48_value sdpy, s48_value svisual,
				 s48_value scolormap, s48_value sxrendercolor)
{
  s48_value res = S48_FALSE;
  XftColor *xftcolor;
  Bool b = False;
  S48_DECLARE_GC_PROTECT(5);

  S48_GC_PROTECT_5(res, sdpy, svisual, scolormap, sxrendercolor);
  if ((xftcolor = (XftColor *) calloc(1, sizeof(XftColor))) != NULL)
    b = XftColorAllocValue(scx_extract_display(sdpy), scx_extract_visual(svisual),
			   scx_extract_colormap(scolormap), 
			   scx_extract_xrendercolor(sxrendercolor),
			   xftcolor);
  res = s48_list_2(b ? S48_TRUE : S48_FALSE,
		   b ? scx_enter_xftcolor(xftcolor) : S48_FALSE);
  S48_GC_UNPROTECT();
  return res;
}

s48_value scx_XftColorFree(s48_value sdpy, s48_value svisual,
			   s48_value scolormap, s48_value sxc)
{
  S48_DECLARE_GC_PROTECT(4);

  S48_GC_PROTECT_4(sdpy, svisual, scolormap, sxc);
  XftColorFree(scx_extract_display(sdpy), scx_extract_visual(svisual),
	       scx_extract_colormap(scolormap), scx_extract_xftcolor(sxc));
  S48_GC_UNPROTECT();
  return S48_UNSPECIFIC;
}

s48_value scx_XftPatternPrint(s48_value sxp)
{
#if XFT_VERSION > 1
  FcPatternPrint(scx_extract_xftpattern(sxp));
#else
  XftPatternPrint(scx_extract_xftpattern(sxp));
#endif
  return S48_UNSPECIFIC;
}

s48_value scx_XftFontSetPrint(s48_value sxfs)
{
#if XFT_VERSION > 1
  FcFontSetPrint(scx_extract_xftfontset(sxfs));
#else
  XftFontSetPrint(scx_extract_xftfontset(sxfs));
#endif
  return S48_UNSPECIFIC;
}

s48_value scx_XftDefaultHasRender(s48_value sdpy)
{
  return XftDefaultHasRender(scx_extract_display(sdpy)) ? S48_TRUE : S48_FALSE;
}

s48_value scx_XftDefaultSubstitute(s48_value sdpy, s48_value sscreen, s48_value spat)
{
  S48_DECLARE_GC_PROTECT(3);

  S48_GC_PROTECT_3(sdpy, sscreen, spat);
  XftDefaultSubstitute(scx_extract_display(sdpy), s48_extract_integer(sscreen),
		       scx_extract_xftpattern(spat));
  S48_GC_UNPROTECT();
  return S48_UNSPECIFIC;
}

#define XFTFONT_GET(FUN, FN)						\
	s48_value FUN(s48_value sxf) {					\
	  return s48_enter_integer(scx_extract_xftfont(sxf)->FN);	\
	}

XFTFONT_GET(scx_xftfont_ascent_get, ascent);
XFTFONT_GET(scx_xftfont_descent_get, descent);
XFTFONT_GET(scx_xftfont_height_get, height);
XFTFONT_GET(scx_xftfont_max_advance_width_get, max_advance_width);

s48_value scx_xftfont_pattern_get_internal(s48_value sxf) 
{
  return scx_enter_xftpattern(scx_extract_xftfont(sxf)->pattern);
}

s48_value scx_xftfontset_count_get(s48_value sxfs)
{
  return s48_enter_integer(scx_extract_xftfontset(sxfs)->nfont);
}

s48_value scx_xftfontset_pattern_ref(s48_value sxfs, s48_value index)
{
  int i;
  XftFontSet *xfs;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(sxfs, index);
  i = s48_extract_integer(index);
  xfs = scx_extract_xftfontset(sxfs);
  S48_GC_UNPROTECT();

  if ((i >= 0) && (i < xfs->nfont))
    return scx_enter_xftpattern(xfs->fonts[i]);
  else
    return S48_FALSE;
}

void scx_xft_init(void)
{
#if XFT_VERSION > 1
  FcInit();
#endif

  XFT_GC_PROTECT_IMPORT_BINDING(scx_xftpattern_record_type, "xft-pattern"); 
  XFT_GC_PROTECT_IMPORT_BINDING(scx_xftfont_record_type, "xft-font"); 
  XFT_GC_PROTECT_IMPORT_BINDING(scx_xftdraw_record_type, "xft-draw"); 
  XFT_GC_PROTECT_IMPORT_BINDING(scx_xftcolor_record_type, "xft-color"); 
  XFT_GC_PROTECT_IMPORT_BINDING(scx_xftobjectset_record_type, "xft-objectset"); 
  XFT_GC_PROTECT_IMPORT_BINDING(scx_xftfontset_record_type, "xft-fontset"); 

  SCX_EXPORT_INTEGER_TO_S48("scx-xft-result-match", scx_XftResultMatch, 
			    XftResultMatch);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-result-no-match", scx_XftResultNoMatch, 
			    XftResultNoMatch);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-result-type-mismatch", scx_XftResultTypeMismatch,
			    XftResultTypeMismatch);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-result-no-id", scx_XftResultNoId,
			    XftResultNoId);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-version-major", scx_XftVersionMajor,
			    SCX_XFT_VERSION_MAJOR);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-version-minor", scx_XftVersionMinor,
			    SCX_XFT_VERSION_MINOR);

  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-family", scx_xft_pattern_family,
			    SCX_XFT_FAMILY);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-style", scx_xft_pattern_style,
			    SCX_XFT_STYLE);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-slant", scx_xft_pattern_slant,
			    SCX_XFT_SLANT);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-weight", scx_xft_pattern_weight,
			    SCX_XFT_WEIGHT);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-size", scx_xft_pattern_size,
			    SCX_XFT_SIZE);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-pixel-size", scx_xft_pattern_pixel_size,
			    SCX_XFT_PIXEL_SIZE);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-encoding", scx_xft_pattern_encoding,
			    SCX_XFT_ENCODING);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-spacing", scx_xft_pattern_spacing,
			    SCX_XFT_SPACING);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-foundry", scx_xft_pattern_foundry,
			    SCX_XFT_FOUNDRY);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-core", scx_xft_pattern_core,
			    SCX_XFT_CORE);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-antialias", scx_xft_pattern_antialias,
			    SCX_XFT_ANTIALIAS);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-xlfd", scx_xft_pattern_xlfd,
			    SCX_XFT_XLFD);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-file", scx_xft_pattern_file,
			    SCX_XFT_FILE);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-index", scx_xft_pattern_index,
			    SCX_XFT_INDEX);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-rasterizer", scx_xft_pattern_rasterizer,
			    SCX_XFT_RASTERIZER);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-outline", scx_xft_pattern_outline,
			    SCX_XFT_OUTLINE);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-scalable", scx_xft_pattern_scalable,
			    SCX_XFT_SCALABLE);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-rgba", scx_xft_pattern_rgba,
			    SCX_XFT_RGBA);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-scale", scx_xft_pattern_scale,
			    SCX_XFT_SCALE);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-render", scx_xft_pattern_render,
			    SCX_XFT_RENDER);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-minspace", scx_xft_pattern_minspace,
			    SCX_XFT_MINSPACE);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-dpi", scx_xft_pattern_dpi,
			    SCX_XFT_DPI);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-char-width", scx_xft_pattern_char_width,
			    SCX_XFT_CHAR_WIDTH);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-pattern-char-height", scx_xft_pattern_char_height,
			    SCX_XFT_CHAR_HEIGHT);

  SCX_EXPORT_INTEGER_TO_S48("scx-xft-weight-light", scx_xft_weight_light,
			    XFT_WEIGHT_LIGHT);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-weight-medium", scx_xft_weight_medium,
			    XFT_WEIGHT_MEDIUM);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-weight-demibold", scx_xft_weight_demibold,
			    XFT_WEIGHT_DEMIBOLD);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-weight-bold", scx_xft_weight_bold,
			    XFT_WEIGHT_BOLD);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-weight-black", scx_xft_weight_black,
			    XFT_WEIGHT_BLACK);

  SCX_EXPORT_INTEGER_TO_S48("scx-xft-slant-roman", scx_xft_slant_roman,
			    XFT_SLANT_ROMAN);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-slant-italic", scx_xft_slant_italic,
			    XFT_SLANT_ITALIC);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-slant-oblique", scx_xft_slant_oblique,
			    XFT_SLANT_OBLIQUE);

  SCX_EXPORT_INTEGER_TO_S48("scx-xft-spacing-proportional", scx_xft_spacing_proportional,
			    XFT_PROPORTIONAL);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-spacing-mono", scx_xft_spacing_mono,
			    XFT_MONO);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-spacing-charcell", scx_xft_spacing_charcell,
			    XFT_CHARCELL);

  SCX_EXPORT_INTEGER_TO_S48("scx-xft-rgba-none", scx_xft_rgba_none,
			    XFT_RGBA_NONE);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-rgba-rgb", scx_xft_rgba_rgb,
			    XFT_RGBA_RGB);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-rgba-bgr", scx_xft_rgba_bgr,
			    XFT_RGBA_BGR);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-rgba-vrgb", scx_xft_rgba_vrgb,
			    XFT_RGBA_VRGB);
  SCX_EXPORT_INTEGER_TO_S48("scx-xft-rgba-vbgr", scx_xft_rgba_vbgr,
			    XFT_RGBA_VBGR);

  S48_EXPORT_FUNCTION(scx_XftPatternCreate);
  S48_EXPORT_FUNCTION(scx_XftPatternDestroy);
  S48_EXPORT_FUNCTION(scx_XftPatternDuplicate);
  S48_EXPORT_FUNCTION(scx_XftPatternGet);
  S48_EXPORT_FUNCTION(scx_XftPatternAdd);

  S48_EXPORT_FUNCTION(scx_XftFontMatch);
  S48_EXPORT_FUNCTION(scx_XftFontOpenPattern);
  S48_EXPORT_FUNCTION(scx_XftFontOpenName);
  S48_EXPORT_FUNCTION(scx_XftFontOpenXlfd);
  S48_EXPORT_FUNCTION(scx_XftFontClose);
  
  S48_EXPORT_FUNCTION(scx_XftDrawCreate);
  S48_EXPORT_FUNCTION(scx_XftDrawCreateBitmap);
  S48_EXPORT_FUNCTION(scx_XftDrawChange);
  S48_EXPORT_FUNCTION(scx_XftDrawDisplay);
  S48_EXPORT_FUNCTION(scx_XftDrawDrawable);
  S48_EXPORT_FUNCTION(scx_XftDrawColormap);
  S48_EXPORT_FUNCTION(scx_XftDrawVisual);
  S48_EXPORT_FUNCTION(scx_XftDrawDestroy);
  
  S48_EXPORT_FUNCTION(scx_XftTextExtents8);
  S48_EXPORT_FUNCTION(scx_XftDrawString8);
  S48_EXPORT_FUNCTION(scx_XftDrawRect);
  S48_EXPORT_FUNCTION(scx_XftDrawSetClip);

  S48_EXPORT_FUNCTION(scx_XftObjectSetCreate);
  S48_EXPORT_FUNCTION(scx_XftObjectSetDestroy);
  S48_EXPORT_FUNCTION(scx_XftObjectSetAdd);
  S48_EXPORT_FUNCTION(scx_XftListFontsPatternObjects);
  S48_EXPORT_FUNCTION(scx_XftFontSetCreate);
  S48_EXPORT_FUNCTION(scx_XftFontSetDestroy);
  S48_EXPORT_FUNCTION(scx_XftFontSetAdd);

  S48_EXPORT_FUNCTION(scx_XftColorAllocName);
  S48_EXPORT_FUNCTION(scx_XftColorAllocValue);
  S48_EXPORT_FUNCTION(scx_XftColorFree);

  S48_EXPORT_FUNCTION(scx_XftPatternPrint);
  S48_EXPORT_FUNCTION(scx_XftFontSetPrint);

  S48_EXPORT_FUNCTION(scx_XftDefaultHasRender);
  S48_EXPORT_FUNCTION(scx_XftDefaultSubstitute);

  S48_EXPORT_FUNCTION(scx_xftfont_ascent_get);
  S48_EXPORT_FUNCTION(scx_xftfont_descent_get);
  S48_EXPORT_FUNCTION(scx_xftfont_height_get);
  S48_EXPORT_FUNCTION(scx_xftfont_max_advance_width_get);
  S48_EXPORT_FUNCTION(scx_xftfont_pattern_get_internal);
  S48_EXPORT_FUNCTION(scx_xftfontset_count_get);
  S48_EXPORT_FUNCTION(scx_xftfontset_pattern_ref);  
}
