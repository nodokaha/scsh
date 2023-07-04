#include "xlib.h"

s48_value scx_colormap_alloc_binding = S48_FALSE;
#define scx_extract_colormap_alloc(x) S48_EXTRACT_ENUM(x, scx_colormap_alloc_binding)

double s48_extract_number(s48_value v) {
  if (S48_DOUBLE_P(v))
    return s48_extract_double(v);
  else return s48_extract_integer(v);
}

void scx_extract_color(s48_value v, XColor* c) {
  S48_DECLARE_GC_PROTECT_1(v);
  s48_check_record_type(v, scx_color);
  c->pixel = scx_extract_pixel(S48_RECORD_REF(v, 0));
  c->flags = 0;
  if (S48_RECORD_REF(v, 1) != S48_FALSE) {
    c->flags |= DoRed;
    c->red = s48_extract_number(S48_RECORD_REF(v, 1)) * 65536;
  }
  if (S48_RECORD_REF(v, 2) != S48_FALSE) {
    c->flags |= DoGreen;
    c->green = s48_extract_number(S48_RECORD_REF(v, 2)) * 65536;
  }
  if (S48_RECORD_REF(v, 3) != S48_FALSE) {
    c->flags |= DoBlue;
    c->blue = s48_extract_number(S48_RECORD_REF(v, 3)) * 65536;
  }
  S48_GC_UNPROTECT();
}

void scx_copy_color(const XColor* c, s48_value v) {
  S48_DECLARE_GC_PROTECT(1);
  s48_check_record_type(v, scx_color);
  S48_GC_PROTECT_1(v);
  S48_RECORD_SET(v, 0, scx_enter_pixel(c->pixel));
  S48_RECORD_SET(v, 1, (c->flags & DoRed) ?
		 s48_enter_double((double)c->red / 65636.0) : S48_FALSE);
  S48_RECORD_SET(v, 2, (c->flags & DoGreen) ?
		 s48_enter_double((double)c->green / 65636.0) : S48_FALSE);
  S48_RECORD_SET(v, 3, (c->flags & DoBlue) ?
		 s48_enter_double((double)c->blue / 65636.0) : S48_FALSE);
  S48_GC_UNPROTECT();
}

s48_value scx_enter_color(const XColor* c) {
  s48_value res = s48_make_record(scx_color);
  scx_copy_color(c, res);
  return res;
}

s48_value scx_Create_Colormap (s48_value display, s48_value window,
			       s48_value visual, s48_value alloc) {
  Colormap cm;
  S48_DECLARE_GC_PROTECT_4(display, window, visual, alloc);
  cm = XCreateColormap(scx_extract_display(display),
				scx_extract_window(window),
				scx_extract_visual(visual),
				scx_extract_colormap_alloc(alloc));
  S48_GC_RETURN(scx_enter_colormap(cm));
}

s48_value scx_Copy_Colormap_And_Free(s48_value display, s48_value colormap) {
  Colormap cm;
  S48_DECLARE_GC_PROTECT_2(display, colormap);
  cm = XCopyColormapAndFree(scx_extract_display(display), 
				     scx_extract_colormap(colormap));
  S48_GC_RETURN(scx_enter_colormap(cm));
}

s48_value scx_Free_Colormap (s48_value display, s48_value colormap) {
  S48_DECLARE_GC_PROTECT_2(display, colormap);
  XFreeColormap(scx_extract_display(display), 
		scx_extract_colormap(colormap));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Alloc_Color(s48_value display, s48_value colormap, 
			  s48_value color) {
  XColor cp;
  S48_DECLARE_GC_PROTECT_3(display, colormap, color);
  scx_extract_color(color, &cp);

  if (!XAllocColor(scx_extract_display(display), 
		   scx_extract_colormap(colormap), &cp))
    S48_GC_RETURN(S48_FALSE);
  else {
    scx_copy_color(&cp, color);
    S48_GC_RETURN(S48_UNSPECIFIC);
  }
}

s48_value scx_Alloc_Named_Color(s48_value display, s48_value colormap,
				s48_value color_name) {
  XColor screen, exact;
  int r;
  s48_value s = S48_NULL, e = S48_NULL;
  S48_DECLARE_GC_PROTECT_5(display, colormap, color_name , s, e);

  r = XAllocNamedColor(scx_extract_display(display), 
		       scx_extract_colormap(colormap), 
		       s48_extract_string(color_name),
		       &screen, &exact);

  if (r != 0) {
    s = scx_enter_color(&screen);
    e = scx_enter_color(&exact);
    S48_GC_RETURN(s48_cons(s, e));
  } else
    S48_GC_RETURN(S48_FALSE);
}

s48_value scx_Alloc_Color_Cells (s48_value display, s48_value colormap,
				 s48_value contig, s48_value nplanes,
				 s48_value npixels) {
  int npl = s48_extract_integer(nplanes);
  int npx = s48_extract_integer(npixels);
  unsigned long plane_masks[npl];
  unsigned long pixels[npx];
  s48_value pls = S48_NULL, pxs = S48_NULL;
  S48_DECLARE_GC_PROTECT_7(display, colormap, contig, nplanes, npixels,
			   pls, pxs);

  if (XAllocColorCells(scx_extract_display(display),
		       scx_extract_colormap(colormap),
		       !S48_FALSE_P(contig),
		       plane_masks, npl,
		       pixels, npx)) {
    int i;
    for (i = npl-1; i >= 0; i--)
      pls = s48_cons(s48_enter_integer(plane_masks[i]), pls);
    for (i = npx-1; i >= 0; i--)
      pxs = s48_cons(scx_enter_pixel(pixels[i]), pxs);
    S48_GC_RETURN(s48_cons(pls, pxs));
  } else
    S48_GC_RETURN(S48_FALSE);
}

s48_value scx_Alloc_Color_Planes(s48_value display, s48_value colormap,
				 s48_value contig, s48_value ncolors,
				 s48_value nreds, s48_value ngreens,
				 s48_value nblues) {
  int npx = s48_extract_integer(ncolors);
  int nre = s48_extract_integer(nreds);
  int ngr = s48_extract_integer(ngreens);
  int nbl = s48_extract_integer(nblues);
  unsigned long pixels[npx];
  unsigned long rmask, gmask, bmask;
  s48_value pxs = S48_NULL;
  s48_value res = S48_NULL;
  S48_DECLARE_GC_PROTECT_9(display, colormap, contig, ncolors, nreds, ngreens,
			   nblues, pxs, res);

  if (XAllocColorPlanes(scx_extract_display(display),
			scx_extract_colormap(colormap),
			!S48_FALSE_P(contig),
			pixels, npx,
			nre, ngr, nbl,
			&rmask, &gmask, &bmask)) {
    int i;
    for (i = npx-1; i >= 0; i--)
      pxs = s48_cons(scx_enter_pixel(pixels[i]), pxs);
    res = s48_cons(s48_enter_integer(bmask), res);
    res = s48_cons(s48_enter_integer(gmask), res);
    res = s48_cons(s48_enter_integer(rmask), res);
    res = s48_cons(pxs, res);
    S48_GC_RETURN(res);
  } else
    S48_GC_RETURN(S48_FALSE);
}

s48_value scx_Free_Colors(s48_value display, s48_value colormap,
			  s48_value pixels, s48_value planes) {
  int i, n = s48_list_length(pixels);
  unsigned long cpixels[n];
  s48_value l = pixels;
  S48_DECLARE_GC_PROTECT_5(display, colormap, pixels, planes, l);
  for (i = 0; i < n; i++) {
    cpixels[i] = scx_extract_pixel(S48_CAR(l));
    l = S48_CDR(l);
  }
  XFreeColors(scx_extract_display(display), scx_extract_colormap(colormap),
	      cpixels, n, s48_extract_integer(planes));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Query_Colors(s48_value display, s48_value colormap,
			   s48_value colors) {
  int i, n = s48_list_length(colors);
  XColor ccolors[n];
  s48_value l = colors;
  S48_DECLARE_GC_PROTECT_4(display, colormap, colors, l);
  for (i = 0; i < n; i++) {
    scx_extract_color(S48_CAR(l), &ccolors[i]);
    l = S48_CDR(l);
  }
  XQueryColors(scx_extract_display(display), scx_extract_colormap(colormap),
	       ccolors, n);
  l = colors;
  for (i = 0; i < n; i++) {
    scx_copy_color(&ccolors[i], S48_CAR(l));
    l = S48_CDR(l);
  }
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Lookup_Color(s48_value display, s48_value colormap,
			   s48_value color_name) {
  XColor cexact, cscreen;
  s48_value r = S48_NULL;
  int res;
  S48_DECLARE_GC_PROTECT_4(display, colormap, color_name, r);
  res = XLookupColor(scx_extract_display(display),
			 scx_extract_colormap(colormap),
			 s48_extract_string(color_name),
			 &cexact, &cscreen);
  if (res == 0) S48_GC_RETURN(S48_FALSE);
  r = scx_enter_color(&cscreen);
  r = s48_cons(scx_enter_color(&cexact), r);
  S48_GC_RETURN(r);
}

s48_value scx_Parse_Color(s48_value display, s48_value colormap,
			  s48_value spec) {
  XColor ret;
  S48_DECLARE_GC_PROTECT_3(display, colormap, spec);
  if (XParseColor(scx_extract_display(display),
		  scx_extract_colormap(colormap),
		  s48_extract_string(spec),
		  &ret)) {
    S48_GC_RETURN(scx_enter_color(&ret));
  } else
    S48_GC_RETURN(S48_FALSE);
}

s48_value scx_Store_Colors(s48_value display, s48_value colormap, 
			   s48_value colors) {
  int i, n = s48_list_length(colors);
  XColor ccolors[n];
  s48_value l = colors;
  S48_DECLARE_GC_PROTECT_4(display, colormap, colors, l);
  for (i = 0; i < n; i++) {
    scx_extract_color(S48_CAR(l), &ccolors[i]);
    l = S48_CDR(l);
  }

  XStoreColors(scx_extract_display(display),
	       scx_extract_colormap(colormap),
	       ccolors, n);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Store_Named_Color(s48_value display, s48_value colormap,
				s48_value color_name, s48_value pixel,
				s48_value do_red, s48_value do_green,
				s48_value do_blue) {
  S48_DECLARE_GC_PROTECT_7(display, colormap, color_name, pixel, do_red,
			   do_green, do_blue);
  XStoreNamedColor(scx_extract_display(display),
		   scx_extract_colormap(colormap),
		   s48_extract_string(color_name),
		   scx_extract_pixel(pixel),
		   (S48_EXTRACT_BOOLEAN(do_red) ? DoRed : 0) |
		   (S48_EXTRACT_BOOLEAN(do_green) ? DoGreen : 0) |
		   (S48_EXTRACT_BOOLEAN(do_blue) ? DoBlue : 0));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

void scx_init_colormap(void) {
  SCX_PRO_IMP(scx_colormap_alloc_binding, "scx-colormap-alloc");

  S48_EXPORT_FUNCTION(scx_Create_Colormap);
  S48_EXPORT_FUNCTION(scx_Copy_Colormap_And_Free);
  S48_EXPORT_FUNCTION(scx_Free_Colormap);
  S48_EXPORT_FUNCTION(scx_Alloc_Color);
  S48_EXPORT_FUNCTION(scx_Alloc_Named_Color);
  S48_EXPORT_FUNCTION(scx_Alloc_Color_Cells);
  S48_EXPORT_FUNCTION(scx_Alloc_Color_Planes);
  S48_EXPORT_FUNCTION(scx_Free_Colors);
  S48_EXPORT_FUNCTION(scx_Query_Colors);
  S48_EXPORT_FUNCTION(scx_Lookup_Color);
  S48_EXPORT_FUNCTION(scx_Parse_Color);
  S48_EXPORT_FUNCTION(scx_Store_Colors);
  S48_EXPORT_FUNCTION(scx_Store_Named_Color);
}
