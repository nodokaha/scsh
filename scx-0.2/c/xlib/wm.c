/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_Reparent_Window(s48_value display, s48_value window, 
			      s48_value parent, s48_value x, s48_value y) {
  S48_DECLARE_GC_PROTECT_5(display, window, parent, x, y);
  XReparentWindow(scx_extract_display(display),
		  scx_extract_window(window),
		  scx_extract_window(parent),
		  (int)s48_extract_integer(x), (int)s48_extract_integer(y));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Install_Colormap(s48_value display, s48_value colormap) {
  S48_DECLARE_GC_PROTECT_2(display, colormap);
  XInstallColormap(scx_extract_display(display),
		   scx_extract_colormap(colormap));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Uninstall_Colormap(s48_value display, s48_value colormap) {
  S48_DECLARE_GC_PROTECT_2(display, colormap);
  XUninstallColormap(scx_extract_display(display), 
		     scx_extract_colormap(colormap));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_List_Installed_Colormaps(s48_value display, s48_value window) {
  int i, n;
  Colormap *ret;
  s48_value l = S48_NULL;
  S48_DECLARE_GC_PROTECT_3(display, window, l);

  ret = XListInstalledColormaps(scx_extract_display(display),
				scx_extract_window(window),
				&n);
  for (i = n-1; i >= 0; i--)
    l = s48_cons(scx_enter_colormap(ret[i]), l);
  XFree((char*)ret);
  S48_GC_RETURN(l);
}

s48_value scx_revert_to_binding = S48_FALSE;
s48_value scx_revert_tos_binding = S48_FALSE;
#define scx_extract_revert_to(x) S48_EXTRACT_ENUM(x, scx_revert_to_binding)
#define scx_enter_revert_to(x) S48_ENTER_ENUM(x, scx_revert_tos_binding)

s48_value scx_Set_Input_Focus(s48_value display, s48_value window, 
			      s48_value revert_to, s48_value time) {
  S48_DECLARE_GC_PROTECT_4(display, window, revert_to, time);
  XSetInputFocus(scx_extract_display(display), scx_extract_window(window),
		 scx_extract_revert_to(revert_to),
		 scx_extract_time(time));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Get_Input_Focus(s48_value display) {
  Window win;
  int revert_to;
  s48_value v = S48_FALSE;
  S48_DECLARE_GC_PROTECT_2(display, v);
  
  XGetInputFocus(scx_extract_display(display), &win, &revert_to);
  v = scx_enter_revert_to(revert_to);
  v = s48_cons(scx_enter_window(win), v);
  S48_GC_RETURN(v);
}

s48_value scx_Warp_Pointer(s48_value dpy, s48_value src, s48_value dst,
			   s48_value srcx, s48_value srcy, s48_value srcw,
			   s48_value srch, s48_value dstx, s48_value dsty) {
  S48_DECLARE_GC_PROTECT_9(dpy, src, dst, srcx, srcy, srcw, srch, dstx, dsty);
  XWarpPointer(scx_extract_display(dpy), 
	       scx_extract_window(src), scx_extract_window(dst),
	       (int)s48_extract_integer(srcx), 
	       (int)s48_extract_integer(srcy), 
	       (int)s48_extract_integer(srcw),
	       (int)s48_extract_integer(srch), 
	       (int)s48_extract_integer(dstx), 
	       (int)s48_extract_integer(dsty));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Bell(s48_value display, s48_value percent) {
  S48_DECLARE_GC_PROTECT_2(display, percent);
  XBell(scx_extract_display(display), s48_extract_integer(percent));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Set_Access_Control(s48_value display, s48_value on) {
  S48_DECLARE_GC_PROTECT_2(display, on);
  XSetAccessControl(scx_extract_display(display), !S48_FALSE_P(on));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_save_set_binding = S48_FALSE;
#define scx_extract_save_set(x) S48_EXTRACT_ENUM(x, scx_save_set_binding)

s48_value scx_Change_Save_Set(s48_value display, s48_value win, 
			      s48_value mode) {
  S48_DECLARE_GC_PROTECT_3(display, win, mode);
  XChangeSaveSet(scx_extract_display(display), scx_extract_window(win),
		 scx_extract_save_set(mode));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_close_down_mode_binding = S48_FALSE;
#define scx_extract_close_down_mode(x) \
  S48_EXTRACT_ENUM(x, scx_close_down_mode_binding)

s48_value scx_Set_Close_Down_Mode(s48_value display, s48_value mode) {
  S48_DECLARE_GC_PROTECT_2(display, mode);
  XSetCloseDownMode(scx_extract_display(display),
		    scx_extract_close_down_mode(mode));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Kill_Client(s48_value display, s48_value xid) {
  S48_DECLARE_GC_PROTECT_2(display, xid);
  XKillClient(scx_extract_display(display), (XID)s48_extract_integer(xid));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Get_Pointer_Mapping(s48_value display) {
  unsigned char map[256];
  int i, n;
  s48_value ret = S48_FALSE;
  S48_DECLARE_GC_PROTECT_2(display, ret);

  n = XGetPointerMapping (scx_extract_display(display), map, 256);
  ret = s48_make_vector(n, S48_NULL);
  for (i = 0; i < n; i++)
    S48_VECTOR_SET(ret, i, s48_enter_integer(map[i]));
  S48_GC_RETURN(ret);
}

s48_value scx_Set_Pointer_Mapping(s48_value display, s48_value map) {
  int i, n = S48_VECTOR_LENGTH(map);
  unsigned char p[n];
  int ret;
  S48_DECLARE_GC_PROTECT_2(display, map);

  for (i = 0; i < n; i++)
    p[i] = (int)s48_extract_integer(S48_VECTOR_REF(map, i));

  ret = XSetPointerMapping(scx_extract_display(display), p, n);
  S48_GC_RETURN((ret == MappingSuccess) ? S48_TRUE : S48_FALSE);
}

void scx_init_wm(void) { 
  SCX_PRO_IMP(scx_revert_to_binding, "scx-revert-to");
  SCX_PRO_IMP(scx_revert_tos_binding, "scx-revert-tos");
  SCX_PRO_IMP(scx_save_set_binding, "scx-save-set");
  SCX_PRO_IMP(scx_close_down_mode_binding, "scx-close-down-mode");

  S48_EXPORT_FUNCTION(scx_Reparent_Window);
  S48_EXPORT_FUNCTION(scx_Install_Colormap);
  S48_EXPORT_FUNCTION(scx_Uninstall_Colormap);
  S48_EXPORT_FUNCTION(scx_List_Installed_Colormaps);
  S48_EXPORT_FUNCTION(scx_Set_Input_Focus);
  S48_EXPORT_FUNCTION(scx_Get_Input_Focus);
  S48_EXPORT_FUNCTION(scx_Warp_Pointer);
  S48_EXPORT_FUNCTION(scx_Bell);
  S48_EXPORT_FUNCTION(scx_Set_Access_Control);
  S48_EXPORT_FUNCTION(scx_Change_Save_Set);
  S48_EXPORT_FUNCTION(scx_Set_Close_Down_Mode);
  S48_EXPORT_FUNCTION(scx_Kill_Client);
  S48_EXPORT_FUNCTION(scx_Get_Pointer_Mapping);
  S48_EXPORT_FUNCTION(scx_Set_Pointer_Mapping);
}
