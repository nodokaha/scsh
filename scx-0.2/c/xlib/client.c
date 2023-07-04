#include "xlib.h"

s48_value scx_Iconify_Window(s48_value display, s48_value w, s48_value scr) {
  S48_DECLARE_GC_PROTECT_3(display, w, scr);
  if (!XIconifyWindow(scx_extract_display(display),
		      scx_extract_window(w),
		      s48_extract_integer(scr)))
    S48_GC_RETURN(S48_FALSE);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Withdraw_Window(s48_value display, s48_value w, s48_value scr) {
  S48_DECLARE_GC_PROTECT_3(display, w, scr);
  if (!XWithdrawWindow(scx_extract_display(display),
		       scx_extract_window(w),
		       s48_extract_integer(scr)))
    S48_GC_RETURN(S48_FALSE);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Reconfigure_Wm_Window(s48_value dpy, s48_value w, s48_value scr,
				    s48_value conf) {
  XWindowChanges WC;
  unsigned long mask;
  S48_DECLARE_GC_PROTECT_4(dpy, w, scr, conf);
  mask = scx_extract_window_changes(conf, &WC);

  if (!XReconfigureWMWindow(scx_extract_display(dpy),
			    scx_extract_window(w),
			    s48_extract_integer(scr),
			    mask, &WC))
    S48_GC_RETURN(S48_FALSE);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Get_Wm_Command(s48_value dpy, s48_value w) {
  int i, ac;
  char** av;
  s48_value ret = S48_NULL;
  S48_DECLARE_GC_PROTECT_3(dpy, w, ret);

  if (!XGetCommand (scx_extract_display(dpy), 
		    scx_extract_window(w),
		    &av, &ac))
    S48_GC_RETURN(S48_FALSE);

  for (i = ac-1; i >= 0; i--)
    ret = s48_cons(s48_enter_string(av[i]), ret);
  if (av != NULL) XFreeStringList(av);
  S48_GC_RETURN(ret);
}

s48_value scx_Set_Wm_Command(s48_value dpy, s48_value w, s48_value cmd) {
  int i, n = s48_list_length(cmd);
  char *argv[n];
  S48_DECLARE_GC_PROTECT_3(dpy, w, cmd);
  for (i = 0; i < n; i++) {
    argv[i] = s48_extract_string(S48_CAR(cmd));
    cmd = S48_CDR(cmd);
  }
  XSetCommand(scx_extract_display(dpy), 
	      scx_extract_window(w), 
	      argv, n);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Get_Wm_Protocols(s48_value display, s48_value w) {
  Atom *p;
  int i, n;
  s48_value ret = S48_NULL;
  S48_DECLARE_GC_PROTECT_3(display, w, ret);

  if (!XGetWMProtocols (scx_extract_display(display),
			scx_extract_window(w), &p, &n))
    S48_GC_RETURN(S48_FALSE);

  for (i = n-1; i >= 0; i--)
    ret = s48_cons(scx_enter_atom(p[i]), ret);
  XFree((char *)p);
  S48_GC_RETURN(ret);
}

s48_value scx_Set_Wm_Protocols (s48_value display, s48_value w, s48_value v) {
  int i, n = s48_list_length(v);
  Atom p[n];
  S48_DECLARE_GC_PROTECT_3(display, w, v);
  
  for (i = 0; i < n; i++) {
    p[i] = scx_extract_atom(S48_CAR(v));
    v = S48_CDR(v);
  }
  
  if (!XSetWMProtocols (scx_extract_display(display),
			scx_extract_window(w),
			p, n))
    S48_GC_RETURN(S48_FALSE);

  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Get_Wm_Class (s48_value display, s48_value w) {
  s48_value ret = S48_FALSE, x = S48_FALSE;
  XClassHint c;
  S48_DECLARE_GC_PROTECT_4(display, w, ret, x);

  c.res_name = c.res_class = 0;
  if (!XGetClassHint(scx_extract_display(display), 
		     scx_extract_window(w), &c))
    S48_GC_RETURN(S48_FALSE);

  ret = s48_enter_string(c.res_class);
  ret = s48_cons(s48_enter_string(c.res_name), ret);
  XFree(c.res_name);
  XFree(c.res_class);
  S48_GC_RETURN(ret);
}

s48_value scx_Set_Wm_Class(s48_value dpy, s48_value w, s48_value name, 
			   s48_value class) {
  XClassHint c;
  S48_DECLARE_GC_PROTECT_4(dpy, w, name, class);
  c.res_name = s48_extract_string(name);
  c.res_class = s48_extract_string(class);
  XSetClassHint(scx_extract_display(dpy),
		scx_extract_window(dpy),
		&c);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_initial_state_binding = S48_FALSE;
#define scx_extract_initial_state(x) \
  S48_EXTRACT_ENUM(x, scx_initial_state_binding)
s48_value scx_initial_states_binding = S48_FALSE;
#define scx_enter_initial_state(x) \
  S48_ENTER_ENUM(x, scx_initial_states_binding)

s48_value scx_wm_hint_binding = S48_FALSE;
#define scx_extract_wm_hint(x) S48_EXTRACT_ENUM(x, scx_wm_hint_binding)
s48_value scx_wm_hints_binding = S48_FALSE;
#define scx_enter_wm_hint(x) S48_ENTER_ENUM(x, scx_wm_hints_binding)

s48_value scx_enter_wm_hint_alist(XWMHints* p) {
  s48_value res = S48_NULL, t = S48_FALSE;
  S48_DECLARE_GC_PROTECT_2(res, t);
  if (p->flags & InputHint) {
    t = scx_enter_wm_hint(0); t = s48_cons(t, S48_ENTER_BOOLEAN(p->input));
    res = s48_cons(t, res);
  }
  if (p->flags & StateHint) {
    t = scx_enter_wm_hint(1);
    t = s48_cons(t, scx_enter_initial_state(p->initial_state));
    res = s48_cons(t, res);
  }
  if (p->flags & IconPixmapHint) {
    t = scx_enter_wm_hint(2);
    t = s48_cons(t, scx_enter_pixmap(p->icon_pixmap));
    res = s48_cons(t, res);
  }
  if (p->flags & IconWindowHint) {
    t = scx_enter_wm_hint(3);
    t = s48_cons(t, scx_enter_window(p->icon_window));
    res = s48_cons(t, res);
  }
  if (p->flags & IconPositionHint) {
    t = s48_enter_integer(p->icon_y);
    t = s48_cons(s48_enter_integer(p->icon_x), t);
    t = s48_cons(scx_enter_wm_hint(4), t);
    res = s48_cons(t, res);
  }
  if (p->flags & IconMaskHint) {
    t = scx_enter_wm_hint(5);
    t = s48_cons(t, scx_enter_pixmap(p->icon_mask));
    res = s48_cons(t, res);
  }
  if (p->flags & WindowGroupHint) {
    t = scx_enter_wm_hint(6);
    t = s48_cons(t, scx_enter_window(p->window_group));
    res = s48_cons(t, res);
  }
  t = scx_enter_wm_hint(8);
  t = s48_cons(t, S48_ENTER_BOOLEAN(p->flags & XUrgencyHint));
  res = s48_cons(t, res);
  
  S48_GC_RETURN(res);
}

void scx_extract_wm_hint_alist(s48_value alist, XWMHints* p) {
  s48_value v = S48_FALSE;
  S48_DECLARE_GC_PROTECT_2(alist, v);
  p->flags = 0;
  while (alist != S48_NULL) {
    int h = scx_extract_wm_hint(S48_CAR(S48_CAR(alist)));
    v = S48_CDR(S48_CAR(alist));
    p->flags |= (1L << h);
    switch (1L << h) {
    case InputHint:
      p->input = S48_EXTRACT_BOOLEAN(v);
      break;
    case StateHint:
      p->initial_state = scx_extract_initial_state(v);
      break;
    case IconPixmapHint:
      p->icon_pixmap = scx_extract_pixmap(v);
      break;
    case IconWindowHint:
      p->icon_window = scx_extract_window(v);
      break;
    case IconPositionHint:
      p->icon_x = s48_extract_integer(S48_CAR(v));
      p->icon_y = s48_extract_integer(S48_CDR(v));
      break;
    case IconMaskHint:
      p->icon_mask = scx_extract_pixmap(v);
      break;
    case WindowGroupHint:
      p->window_group = scx_extract_window(v);
      break;
    case XUrgencyHint:
      if (v == S48_FALSE)
	p->flags &= ~XUrgencyHint;
      break;
    }
    alist = S48_CDR(alist);
  }
  S48_GC_UNPROTECT();
}

s48_value scx_Get_Wm_Hints(s48_value dpy, s48_value w) {
  XWMHints* p;
  s48_value res = S48_NULL;
  S48_DECLARE_GC_PROTECT_3(dpy, w, res);

  p = XGetWMHints(scx_extract_display(dpy),
		  scx_extract_window(w));
  if (p == NULL)
    S48_GC_RETURN(S48_FALSE);
  res = scx_enter_wm_hint_alist(p);
  XFree(p);
  S48_GC_RETURN(res);
}

s48_value scx_Set_Wm_Hints (s48_value dpy, s48_value w, s48_value hints) {
  XWMHints WMH;
  S48_DECLARE_GC_PROTECT_3(dpy, w, hints);
  scx_extract_wm_hint_alist(hints, &WMH);

  XSetWMHints(scx_extract_display(dpy),
	      scx_extract_window(w),
	      &WMH);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Get_Transient_For(s48_value dpy, s48_value w) {
  Window win;
  S48_DECLARE_GC_PROTECT_2(dpy, w);
  if (!XGetTransientForHint(scx_extract_display(dpy), 
			    scx_extract_window(w), 
			    &win))
    S48_GC_RETURN(S48_FALSE);
  S48_GC_RETURN(scx_enter_window(win));
}

s48_value scx_Set_Transient_For(s48_value dpy, s48_value w, s48_value pw) {
  S48_DECLARE_GC_PROTECT_3(dpy, w, pw);
  XSetTransientForHint(scx_extract_display(dpy), 
		       scx_extract_window(w), 
		       scx_extract_window(pw));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Get_Text_Property(s48_value dpy, s48_value w, s48_value a) {
  XTextProperty ret;
  s48_value res = S48_FALSE;
  S48_DECLARE_GC_PROTECT_4(dpy, w, a, res);
  if (!XGetTextProperty (scx_extract_display(dpy),
			 scx_extract_window(w), 
			 &ret,
			 scx_extract_atom(a)))
    S48_GC_RETURN(S48_FALSE);
  res = scx_enter_property(ret.encoding, ret.format, ret.value, ret.nitems);
  XFree(ret.value);
  S48_GC_RETURN(res);
}

s48_value scx_Set_Text_Property(s48_value dpy, s48_value w, s48_value prop, 
				s48_value a) {
  XTextProperty p;
  S48_DECLARE_GC_PROTECT_4(dpy, w, prop, a);
  scx_extract_property(prop, &p.encoding, &p.format,
		       (char**)&p.value,
		       (int*)&p.nitems);
  XSetTextProperty(scx_extract_display(dpy),
		   scx_extract_window(w), 
		   &p, scx_extract_atom(a));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_size_hint_binding = S48_FALSE;
#define scx_extract_size_hint(h) S48_EXTRACT_ENUM(h, scx_size_hint_binding)
s48_value scx_size_hints_binding = S48_FALSE;
#define scx_enter_size_hint(h) S48_ENTER_ENUM(h, scx_size_hints_binding)

s48_value scx_enter_size_hint_alist(XSizeHints* sh) {
  int i;
  s48_value res = S48_NULL, v = S48_FALSE, t = S48_FALSE;
  S48_DECLARE_GC_PROTECT(3);
  S48_GC_PROTECT_3(res, v, t);
  for (i = 0; i < 10; i++) {
    if (sh->flags & (1L << i)) {
      switch (1L << i) {
      case USPosition: case PPosition:
	v = s48_enter_integer(sh->x);
	v = s48_cons(v, s48_enter_integer(sh->y));
	break;
      case USSize: case PSize:
	v = s48_enter_integer(sh->width);
	v = s48_cons(v, s48_enter_integer(sh->height));
	break;
      case PMinSize:
	v = s48_enter_integer(sh->min_width);
	v = s48_cons(v, s48_enter_integer(sh->min_height));
	break;
      case PMaxSize:
	v = s48_enter_integer(sh->max_width);
	v = s48_cons(v, s48_enter_integer(sh->max_height));
	break;
      case PResizeInc:
	v = s48_enter_integer(sh->width_inc);
	v = s48_cons(v, s48_enter_integer(sh->height_inc));
	break;
      case PAspect:
	v = s48_enter_integer(sh->min_aspect.x);
	v = s48_cons(v, s48_enter_integer(sh->min_aspect.y));
	t = s48_enter_integer(sh->max_aspect.x);
	t = s48_cons(t, s48_enter_integer(sh->max_aspect.y));
	v = s48_cons(v, t);
	break;
      case PBaseSize:
	v = s48_enter_integer(sh->base_width);
	v = s48_cons(v, s48_enter_integer(sh->base_height));
	break;
      case PWinGravity:
	v = scx_enter_win_gravity(sh->win_gravity);
	break;
      default: v = S48_FALSE;
      }
      t = scx_enter_size_hint(i);
      t = s48_cons(t, v);
      res = s48_cons(t, res);
    }
  }
  S48_GC_UNPROTECT();
  return res;
}

void scx_extract_size_hint_alist(s48_value l, XSizeHints* sh) {
  s48_value v = S48_FALSE;
  S48_DECLARE_GC_PROTECT_2(l, v);
  sh->flags = 0;
  for (; l != S48_NULL; l = S48_CDR(l)) {
    int m = scx_extract_size_hint(S48_CAR(S48_CAR(l)));
    v = S48_CDR(S48_CAR(l));
    sh->flags |= (1L << m);
    switch (1L << m) {
    case USPosition: case PPosition:
      sh->x = s48_extract_integer(S48_CAR(v));
      sh->y = s48_extract_integer(S48_CDR(v));
      break;
    case USSize: case PSize:
      sh->width = s48_extract_integer(S48_CAR(v));
      sh->height = s48_extract_integer(S48_CDR(v));
      break;
    case PMinSize:
      sh->min_width = s48_extract_integer(S48_CAR(v));
      sh->min_height = s48_extract_integer(S48_CDR(v));
      break;
    case PMaxSize:
      sh->max_width = s48_extract_integer(S48_CAR(v));
      sh->max_height = s48_extract_integer(S48_CDR(v));
      break;
    case PResizeInc:
      sh->width_inc = s48_extract_integer(S48_CAR(v));
      sh->height_inc = s48_extract_integer(S48_CDR(v));
      break;
    case PAspect:
      sh->min_aspect.x = s48_extract_integer(S48_CAR(S48_CAR(v)));
      sh->min_aspect.y = s48_extract_integer(S48_CDR(S48_CAR(v)));
      sh->max_aspect.x = s48_extract_integer(S48_CAR(S48_CDR(v)));
      sh->max_aspect.y = s48_extract_integer(S48_CDR(S48_CDR(v)));
      break;
    case PBaseSize:
      sh->base_width = s48_extract_integer(S48_CAR(v));
      sh->base_height = s48_extract_integer(S48_CDR(v));
      break;
    case PWinGravity:
      sh->win_gravity = scx_extract_win_gravity(v);
      break;
    }
  }
  S48_GC_UNPROTECT();
}

s48_value scx_Get_Wm_Normal_Hints(s48_value dpy, s48_value win) {
  XSizeHints SH;
  long supplied_by_user;
  S48_DECLARE_GC_PROTECT_2(dpy, win);
  if (!XGetWMNormalHints(scx_extract_display(dpy),
			 scx_extract_window(win),
			 &SH, &supplied_by_user))
    S48_GC_RETURN(S48_FALSE);
  /* ignoring supplied_by_user ... ?! */
  S48_GC_RETURN(scx_enter_size_hint_alist(&SH));
}

s48_value scx_Set_Wm_Normal_Hints(s48_value dpy, s48_value win, 
				  s48_value hints) {
  XSizeHints SH;
  S48_DECLARE_GC_PROTECT_3(dpy, win, hints);
  scx_extract_size_hint_alist(hints, &SH);

  XSetWMNormalHints(scx_extract_display(dpy),
		    scx_extract_window(win),
		    &SH);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_icon_size_binding = S48_FALSE;

s48_value scx_enter_icon_size(XIconSize* is) {
  s48_value res = s48_make_record(scx_icon_size_binding);
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(res);
  S48_RECORD_SET(res, 0, s48_enter_integer(is->min_width));
  S48_RECORD_SET(res, 1, s48_enter_integer(is->min_height));
  S48_RECORD_SET(res, 2, s48_enter_integer(is->max_width));
  S48_RECORD_SET(res, 3, s48_enter_integer(is->max_height));
  S48_RECORD_SET(res, 4, s48_enter_integer(is->width_inc));
  S48_RECORD_SET(res, 5, s48_enter_integer(is->height_inc));
  S48_GC_UNPROTECT();
  return res;
}

void scx_extract_icon_size(s48_value r, XIconSize* is) {
  S48_DECLARE_GC_PROTECT_1(r);
  s48_check_record_type(r, scx_icon_size_binding);
  is->min_width = s48_extract_integer(S48_RECORD_REF(r, 0));
  is->min_height = s48_extract_integer(S48_RECORD_REF(r, 1));
  is->max_width = s48_extract_integer(S48_RECORD_REF(r, 2));
  is->max_height = s48_extract_integer(S48_RECORD_REF(r, 3));
  is->width_inc = s48_extract_integer(S48_RECORD_REF(r, 4));
  is->height_inc = s48_extract_integer(S48_RECORD_REF(r, 5));
  S48_GC_UNPROTECT();
}

s48_value scx_Get_Icon_Sizes(s48_value dpy, s48_value w) {
  XIconSize *p;
  int i, n;
  s48_value v = S48_NULL;
  S48_DECLARE_GC_PROTECT_3(dpy, w, v);
  
  if (!XGetIconSizes (scx_extract_display(dpy), 
		      scx_extract_window(w), 
		      &p, &n))
    S48_GC_RETURN(S48_FALSE);
  
  for (i = n-1; i >= 0; i--)
    v = s48_cons(scx_enter_icon_size(&p[i]), v);
  XFree((char *)p);
  S48_GC_RETURN(v);
}

s48_value scx_Set_Icon_Sizes(s48_value dpy, s48_value w, s48_value v) {
  int i, n = s48_list_length(v);
  XIconSize p[n];
  S48_DECLARE_GC_PROTECT_3(dpy, w, v);
  for (i = 0; i < n; i++) {
    scx_extract_icon_size(S48_CAR(v), &p[i]);
    v = S48_CDR(v);
  }
  XSetIconSizes(scx_extract_display(dpy), 
		scx_extract_window(w), 
		p, n);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

scx_init_client() {
  SCX_PRO_IMP(scx_icon_size_binding, "scx-icon-size");
  SCX_PRO_IMP(scx_initial_state_binding, "scx-initial-state");
  SCX_PRO_IMP(scx_initial_states_binding, "scx-initial-states");
  SCX_PRO_IMP(scx_wm_hint_binding, "scx-wm-hint");
  SCX_PRO_IMP(scx_wm_hints_binding, "scx-wm-hints");
  SCX_PRO_IMP(scx_size_hint_binding, "scx-size-hint");
  SCX_PRO_IMP(scx_size_hints_binding, "scx-size-hints");

  S48_EXPORT_FUNCTION(scx_Iconify_Window);
  S48_EXPORT_FUNCTION(scx_Withdraw_Window);
  S48_EXPORT_FUNCTION(scx_Reconfigure_Wm_Window);
  S48_EXPORT_FUNCTION(scx_Get_Wm_Command);
  S48_EXPORT_FUNCTION(scx_Get_Text_Property);
  S48_EXPORT_FUNCTION(scx_Set_Text_Property);
  S48_EXPORT_FUNCTION(scx_Get_Wm_Protocols);
  S48_EXPORT_FUNCTION(scx_Set_Wm_Protocols);
  S48_EXPORT_FUNCTION(scx_Get_Wm_Class);
  S48_EXPORT_FUNCTION(scx_Set_Wm_Class);
  S48_EXPORT_FUNCTION(scx_Set_Wm_Command);
  S48_EXPORT_FUNCTION(scx_Get_Wm_Hints);
  S48_EXPORT_FUNCTION(scx_Set_Wm_Hints);
  S48_EXPORT_FUNCTION(scx_Get_Transient_For);
  S48_EXPORT_FUNCTION(scx_Set_Transient_For);
  S48_EXPORT_FUNCTION(scx_Get_Wm_Normal_Hints);
  S48_EXPORT_FUNCTION(scx_Set_Wm_Normal_Hints);
  S48_EXPORT_FUNCTION(scx_Get_Icon_Sizes);
  S48_EXPORT_FUNCTION(scx_Set_Icon_Sizes);
}
