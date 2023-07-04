/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_Create_Pixmap(s48_value display, s48_value drawable, s48_value w,
			    s48_value h, s48_value depth) {
  Pixmap pm;
  S48_DECLARE_GC_PROTECT_5(display, drawable, w, h, depth);
  pm = XCreatePixmap(scx_extract_display(display), 
		     scx_extract_drawable(drawable), 
		     (int)s48_extract_integer(w), 
		     (int)s48_extract_integer(h),
		     (int)s48_extract_integer(depth));
  S48_GC_RETURN(scx_enter_pixmap(pm));
}

s48_value scx_Free_Pixmap(s48_value display, s48_value pixmap) {
  S48_DECLARE_GC_PROTECT_2(display, pixmap);
  XFreePixmap(scx_extract_display(display), scx_extract_pixmap(pixmap));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Read_Bitmap_File(s48_value display, s48_value drawable, 
			       s48_value filename) {
  unsigned width, height;
  int res, xhot, yhot;
  Pixmap bitmap;
  s48_value ret = S48_FALSE;
  S48_DECLARE_GC_PROTECT_4(display, drawable, filename, ret);

  res = XReadBitmapFile(scx_extract_display(display), 
			scx_extract_drawable(drawable),
			s48_extract_string(filename), &width, &height, &bitmap,
			&xhot, &yhot);

  if (res != BitmapSuccess)
    S48_GC_RETURN(s48_enter_integer(res));
  
  ret = s48_cons(s48_enter_integer(yhot), S48_NULL);
  ret = s48_cons(s48_enter_integer(xhot), ret);
  ret = s48_cons(s48_enter_integer(height), ret);
  ret = s48_cons(s48_enter_integer(width), ret);
  ret = s48_cons(scx_enter_pixmap(bitmap), ret);
  S48_GC_RETURN(ret);
}

s48_value scx_Write_Bitmap_File(s48_value display, s48_value filename, 
				s48_value bitmap, s48_value w, s48_value h,
				s48_value x, s48_value y) {
  int ret;
  S48_DECLARE_GC_PROTECT_7(display, filename, bitmap, w, h, x, y);
  ret = XWriteBitmapFile(scx_extract_display(display), 
			 s48_extract_string(filename),
			 scx_extract_pixmap(bitmap), 
			 (int)s48_extract_integer(w),
			 (int)s48_extract_integer(h),
			 (int)s48_extract_integer(x), 
			 (int)s48_extract_integer(y));
  S48_GC_RETURN(s48_enter_integer(ret));
}

s48_value scx_Create_Bitmap_From_Data(s48_value display, s48_value drawable, 
				      s48_value data, s48_value w,
				      s48_value h) {
  Pixmap pm;
  S48_DECLARE_GC_PROTECT_5(display, drawable, data, w, h);
  pm = XCreateBitmapFromData(scx_extract_display(display), 
			     scx_extract_drawable(drawable),
			     s48_extract_string(data), 
			     s48_extract_integer(w),
			     s48_extract_integer(h));
  S48_GC_RETURN(scx_enter_pixmap(pm));
}

s48_value scx_Create_Pixmap_From_Bitmap_Data(s48_value display, 
					     s48_value drawable,
					     s48_value data,
					     s48_value w,s48_value h,
					     s48_value f, s48_value b,
					     s48_value depth) {
  Pixmap pm;
  S48_DECLARE_GC_PROTECT_8(display, drawable, data, w, h, f, b, depth);
  pm = XCreatePixmapFromBitmapData(scx_extract_display(display),
				   scx_extract_drawable(drawable),
				   s48_extract_string(data), 
				   (int)s48_extract_integer(w),
				   (int)s48_extract_integer(h),
				   scx_extract_pixel(f),
				   scx_extract_pixel(b),
				   (int)s48_extract_integer(depth));
  S48_GC_RETURN(scx_enter_pixmap(pm));
}

void scx_init_pixmap(void) {
  S48_EXPORT_FUNCTION(scx_Free_Pixmap);
  S48_EXPORT_FUNCTION(scx_Create_Pixmap);
  S48_EXPORT_FUNCTION(scx_Read_Bitmap_File);
  S48_EXPORT_FUNCTION(scx_Write_Bitmap_File);
  S48_EXPORT_FUNCTION(scx_Create_Bitmap_From_Data);
  S48_EXPORT_FUNCTION(scx_Create_Pixmap_From_Bitmap_Data);
}

