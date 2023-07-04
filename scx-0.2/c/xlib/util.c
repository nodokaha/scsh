#include "xlib.h"

s48_value scx_Get_Default(s48_value dpy, s48_value program,
			  s48_value option) {
  char* ret;
  S48_DECLARE_GC_PROTECT_3(dpy, program, option);
  if (ret = XGetDefault(scx_extract_display(dpy),
			s48_extract_string(program),
			s48_extract_string(option)))
    S48_GC_RETURN(s48_enter_string(ret));
  else S48_GC_RETURN(S48_FALSE);
}

s48_value scx_Resource_Manager_String(s48_value dpy) {
  char* ret;
  if (ret = XResourceManagerString (scx_extract_display(dpy)))
    return s48_enter_string(ret);
  else return S48_FALSE;
}

s48_value scx_Parse_Geometry(s48_value strg) {
  s48_value ret;
  int x, y, res;
  unsigned int w, h;
  S48_DECLARE_GC_PROTECT_2(strg, ret);

  res = XParseGeometry (s48_extract_string(strg), &x, &y, &w, &h);

  ret = s48_make_vector(6, S48_FALSE);

  if (res & XNegative) S48_VECTOR_SET(ret, 0, S48_TRUE);
  if (res & YNegative) S48_VECTOR_SET(ret, 1, S48_TRUE);
  if (res & XValue) S48_VECTOR_SET(ret, 2, s48_enter_integer(x));
  if (res & YValue) S48_VECTOR_SET(ret, 3, s48_enter_integer(y));
  if (res & WidthValue) S48_VECTOR_SET(ret, 4, s48_enter_integer(w));
  if (res & HeightValue) S48_VECTOR_SET(ret, 5, s48_enter_integer(h));
  S48_GC_UNPROTECT();

  return ret;
}

void scx_init_util(void) {
  S48_EXPORT_FUNCTION(scx_Get_Default);
  S48_EXPORT_FUNCTION(scx_Resource_Manager_String);
  S48_EXPORT_FUNCTION(scx_Parse_Geometry);
}
