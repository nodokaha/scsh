/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_Change_Keyboard_Mapping(s48_value display,
				      s48_value first_keycode,
				      s48_value keysyms_lists) {
  int max = 0, n = s48_list_length(keysyms_lists), i;
  s48_value l = keysyms_lists;
  S48_DECLARE_GC_PROTECT_3(display, first_keycode, keysyms_lists);
  for (i = 0; i < n; i++) {
    int m = s48_list_length(S48_CAR(l));
    if (m > max) max = m;
    l = S48_CDR(l);
  }
  {
    KeySym ks[max * n];
    l = keysyms_lists;
    for (i = 0; i < n; i++) {
      s48_value l2 = S48_CAR(l);
      int j, n2 = s48_list_length(l2);
      for (j = 0; j < n2; j++) {
	if (l2 == S48_NULL)
	  ks[i * max + j] = NoSymbol;
	else {
	  ks[i * max + j] = scx_extract_keysym(S48_CAR(l2));
	  l2 = S48_CDR(l2);
	}
      }
      l = S48_CDR(l);
    }

    XChangeKeyboardMapping(scx_extract_display(display),
			   s48_extract_integer(first_keycode),
			   max, ks, max * n);
  }
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Get_Keyboard_Mapping(s48_value display, s48_value first_keycode,
				   s48_value count) {
  int kpk, ccount = s48_extract_integer(count);
  KeySym* ks = XGetKeyboardMapping(scx_extract_display(display),
				   s48_extract_integer(first_keycode),
				   ccount, &kpk);
  s48_value l = S48_NULL, l2 = S48_NULL;
  int i;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(l, l2);
  for (i = ccount; i > 0; i--) {
    int j;
    l2 = S48_NULL;
    for (j = kpk; j > 0; j--) {
      if (ks[i * kpk + j - 1] != NoSymbol)
	l2 = s48_cons(scx_enter_keysym(ks[i * kpk + j - 1]), l2);
    }
    l = s48_cons(l2, l);
  }
  S48_GC_UNPROTECT();

  XFree(ks);
  S48_GC_RETURN(l);
}

s48_value scx_Display_Keycodes(s48_value display) {
  int min, max;
  XDisplayKeycodes(scx_extract_display(display), &min, &max);
  return s48_cons(s48_enter_fixnum(min), s48_enter_fixnum(max));
}

s48_value scx_Set_Modifier_Mapping(s48_value display, s48_value modmap) {
  int max = 0;
  s48_value l = modmap, l2 = S48_FALSE;
  S48_DECLARE_GC_PROTECT_4(display, modmap, l, l2);
  for (; l != S48_NULL; l = S48_CDR(l)) {
    int m = s48_list_length(S48_CDR(S48_CAR(l)));
    if (m > max) max = m;
  }
  {
    int res;
    KeyCode ks[8*max];
    XModifierKeymap cmap;
    cmap.max_keypermod = max;
    cmap.modifiermap = ks;
    
    for (l = modmap; l != S48_NULL; l = S48_CDR(l)) {
      int mod = scx_extract_state(S48_CAR(S48_CAR(l)));
      int j;
      l2 = S48_CDR(S48_CAR(l));
      j = 0;
      for (j = 0; j < max; j++) {
	if ((mod < 0) || (mod > 7)) continue; /* TODO: error?? */
	if (l2 != S48_NULL) {
	  ks[mod*max + j] = s48_extract_integer(S48_CAR(l2));
	  l2 = S48_CDR(l2);
	} else
	  ks[mod*max + j] = 0;
      }
    }
    res = XSetModifierMapping(scx_extract_display(display),
			      &cmap);
    S48_GC_RETURN(s48_enter_integer(res));
  }
}

s48_value scx_Get_Modifier_Mapping(s48_value display) {
  XModifierKeymap* km;
  s48_value l = S48_NULL, l2 = S48_NULL;
  int i;
  S48_DECLARE_GC_PROTECT(3);
  S48_GC_PROTECT_3(display, l, l2);
  km = XGetModifierMapping(scx_extract_display(display));
  for (i = 7; i >= 0; i--) {
    int j;
    l2 = S48_NULL;
    for (j = km->max_keypermod - 1; j >= 0; j--) {
      KeyCode c = km->modifiermap[i*km->max_keypermod + j];
      l2 = s48_cons(s48_enter_integer(c), l2);
    }
    l2 = s48_cons(scx_enter_state(i), l2);
    l = s48_cons(l2, l);
  }
  S48_GC_UNPROTECT();
  XFreeModifiermap(km);
  return l;
}

s48_value scx_String_To_Keysym(s48_value string) {
  return scx_enter_keysym(XStringToKeysym(s48_extract_string(string)));
}

s48_value scx_Keysym_To_String(s48_value ks) {
  char* s = XKeysymToString(scx_extract_keysym(ks));
  s48_value res = s48_enter_string(s);
  XFree(s);
  return res;
}

s48_value scx_Keycode_To_Keysym(s48_value display, s48_value kc, s48_value i) {
  KeySym ks;
  S48_DECLARE_GC_PROTECT_3(display, kc, i);
  ks = XKeycodeToKeysym(scx_extract_display(display),
			s48_extract_integer(kc),
			s48_extract_integer(i));
  S48_GC_RETURN(scx_enter_keysym(ks));
}

s48_value scx_Keysym_To_Keycode(s48_value display, s48_value ks) {
  KeyCode kc;
  S48_DECLARE_GC_PROTECT_2(display, ks);
  kc = XKeysymToKeycode(scx_extract_display(display),
				scx_extract_keysym(ks));
  S48_GC_RETURN(s48_enter_integer(kc));
}

s48_value scx_Convert_Case(s48_value keysym) {
  KeySym low, up;
  s48_value res = S48_FALSE;
  S48_DECLARE_GC_PROTECT_2(keysym, res);
  XConvertCase(scx_extract_keysym(keysym), &low, &up);
  res = scx_enter_keysym(up);
  res = s48_cons(scx_enter_keysym(low), res);
  S48_GC_RETURN(res);
}

s48_value scx_Lookup_Keysym(s48_value key_event, s48_value index) {
  XKeyEvent ke;
  s48_value res = S48_FALSE;
  S48_DECLARE_GC_PROTECT_3(key_event, index, res);
  scx_extract_key_event(key_event, &ke);
  res = scx_enter_keysym(XLookupKeysym(&ke, s48_extract_integer(index)));
  S48_GC_RETURN(res);
}

s48_value scx_Refresh_Keyboard_Mapping(s48_value mapping_event) {
  XMappingEvent e;
  scx_extract_mapping_event(mapping_event, &e);
  XRefreshKeyboardMapping(&e);
  return S48_UNSPECIFIC;
}

s48_value scx_Lookup_String(s48_value key_event) {
  XKeyEvent e;
  char buf[1024]; 
  int len; 
  KeySym keysym_return; 
  s48_value res = S48_FALSE;
  S48_DECLARE_GC_PROTECT_2(key_event, res);
  
  scx_extract_key_event(key_event, &e);
  len = XLookupString(&e, buf, 1023, &keysym_return, NULL);
  buf[len] = 0;

  res = s48_enter_string(buf);
  res = s48_cons(scx_enter_keysym(keysym_return), res);

  S48_GC_RETURN(res);
}

s48_value scx_Rebind_Keysym(s48_value display, s48_value keysym,
			    s48_value mod_keysyms, s48_value str) {
  int i, n = s48_list_length(mod_keysyms);
  KeySym mods[n];
  S48_DECLARE_GC_PROTECT_4(display, keysym, mod_keysyms, str);

  for (i = 0; i < n; i++) {
    mods[i] = scx_extract_keysym(S48_CAR(mod_keysyms));
    mod_keysyms = S48_CDR(mod_keysyms);
  }
  XRebindKeysym(scx_extract_display(display), 
		scx_extract_keysym(keysym),
		mods, n, 
		(unsigned char *)s48_extract_string(str),
		S48_STRING_LENGTH(str));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

scx_init_key () {
  S48_EXPORT_FUNCTION(scx_Change_Keyboard_Mapping);
  S48_EXPORT_FUNCTION(scx_Get_Keyboard_Mapping);
  S48_EXPORT_FUNCTION(scx_Display_Keycodes);
  S48_EXPORT_FUNCTION(scx_Set_Modifier_Mapping);
  S48_EXPORT_FUNCTION(scx_Get_Modifier_Mapping);
  S48_EXPORT_FUNCTION(scx_String_To_Keysym);
  S48_EXPORT_FUNCTION(scx_Keysym_To_String);
  S48_EXPORT_FUNCTION(scx_Keycode_To_Keysym);
  S48_EXPORT_FUNCTION(scx_Keysym_To_Keycode);
  S48_EXPORT_FUNCTION(scx_Convert_Case);
  S48_EXPORT_FUNCTION(scx_Lookup_Keysym);
  S48_EXPORT_FUNCTION(scx_Refresh_Keyboard_Mapping);
  S48_EXPORT_FUNCTION(scx_Lookup_String);
  S48_EXPORT_FUNCTION(scx_Rebind_Keysym);
}
