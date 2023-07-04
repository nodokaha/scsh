/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_Intern_Atom(s48_value display, s48_value name,
			  s48_value only_if_exists) {
  Atom a;
  S48_DECLARE_GC_PROTECT_3(display, name, only_if_exists);
  a = XInternAtom(scx_extract_display(display), 
		  s48_extract_string(name),
		  S48_EXTRACT_BOOLEAN(only_if_exists));
  S48_GC_RETURN(scx_enter_atom(a));
}

s48_value scx_Intern_Atoms(s48_value display, s48_value names,
			   s48_value only_if_exists) {
  int i, n = s48_list_length(names);
  char* cnames[n];
  Atom atoms[n];
  s48_value l = S48_NULL;
  S48_DECLARE_GC_PROTECT_4(display, names, only_if_exists, l);
  for (i = 0; i < n; i++)
    cnames[i] = s48_extract_string(names);
  if (!XInternAtoms(scx_extract_display(display), 
		    cnames, n,
		    S48_EXTRACT_BOOLEAN(only_if_exists),
		    atoms))
    S48_GC_RETURN(S48_FALSE);
  else {
    for (i = n-1; i >= 0; i--)
      l = s48_cons(scx_enter_atom(atoms[i]), l);
    S48_GC_RETURN(l);
  }
}

s48_value scx_Get_Atom_Name(s48_value display, s48_value a) {
  char* s;
  s48_value str;
  S48_DECLARE_GC_PROTECT_3(display, a, str);
  s = XGetAtomName(scx_extract_display(display),
		   scx_extract_atom(a));
  str = s48_enter_string(s);
  XFree(s);
  S48_GC_RETURN(str);
}

s48_value scx_List_Properties(s48_value display, s48_value window) {
  int n, i;
  Atom *atoms;
  s48_value l = S48_NULL;
  S48_DECLARE_GC_PROTECT_3(display, window, l);
  atoms = XListProperties (scx_extract_display(display), 
			   scx_extract_window(window), &n);

  for (i = n-1; i >= 0; i--)
    l = s48_cons(scx_enter_atom(atoms[i]), l);
  XFree ((char*)atoms);
  S48_GC_RETURN(l);
}

s48_value scx_Rotate_Window_Properties(s48_value display, s48_value window,
				       s48_value properties, s48_value delta) {
  int i, n = s48_list_length(properties);
  Atom p[n];
  S48_DECLARE_GC_PROTECT_4(display, window, properties, delta);
  for (i = 0; i < n; i++) {
    p[i] = scx_extract_atom(S48_CAR(properties));
    properties = S48_CDR(properties);
  }
  XRotateWindowProperties(scx_extract_display(display), 
			  scx_extract_window(window),
			  p, n,	s48_extract_integer(delta));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Delete_Property(s48_value display, s48_value window,
			      s48_value prop) {
  S48_DECLARE_GC_PROTECT_3(display, window, prop);
  XDeleteProperty(scx_extract_display(display), scx_extract_window(window),
		  scx_extract_atom(prop));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_property_binding = S48_FALSE;
s48_value scx_property_format_binding = S48_FALSE;
s48_value scx_property_formats_binding = S48_FALSE;

void scx_extract_property(s48_value p, Atom* type, int* format,
			  char** data, int* nelements) {
  int i;
  s48_value d = S48_RECORD_REF(p, 2);
  S48_DECLARE_GC_PROTECT_2(p, d);
  s48_check_record_type(p, scx_property_binding);
  *type = scx_extract_atom(S48_RECORD_REF(p, 0));
  *format = S48_EXTRACT_ENUM(S48_RECORD_REF(p, 1),
			     scx_property_format_binding);
  switch (*format) {
  case 0:
    *format = 8;
    *nelements = S48_STRING_LENGTH(d);
    *data = (char*)malloc(*nelements);
    strcpy(*data, s48_extract_string(d));
    break;
  case 1:
    *format = 16;
    *nelements = s48_list_length(d);
    *data = (char*)malloc(2 * (*nelements));
    for (i = 0; i < *nelements; i++) {
      (*(short**)data)[i] = s48_extract_integer(S48_CAR(d));
      d = S48_CDR(d);
    }
    break;
  case 2:
    *format = 32;
    *nelements = s48_list_length(d);
    *data = (char*)malloc(4 * (*nelements));
    for (i = 0; i < *nelements; i++) {
      (*(long**)data)[i] = s48_extract_integer(S48_CAR(d));
      d = S48_CDR(d);
    }
    break;
  }
  S48_GC_UNPROTECT();
}

s48_value scx_enter_property(Atom type, int format, char* data,
			     int nelements) {
  s48_value p = s48_make_record(scx_property_binding);
  s48_value l = S48_NULL;
  int i;
  S48_DECLARE_GC_PROTECT(2);
  S48_GC_PROTECT_2(p, l);

  S48_RECORD_SET(p, 0, scx_enter_atom(type));
  switch (format) {
  case 8:
    S48_RECORD_SET(p, 1, S48_ENTER_ENUM(0, scx_property_formats_binding));
    S48_RECORD_SET(p, 2, s48_enter_substring(data, nelements));
    break;
  case 16:
    S48_RECORD_SET(p, 1, S48_ENTER_ENUM(1, scx_property_formats_binding));
    for (i = nelements-1; i >= 0; i--)
      l = s48_cons(s48_enter_integer(((short*)data)[i]), l);
    S48_RECORD_SET(p, 2, l);
    break;
  case 32:
    S48_RECORD_SET(p, 1, S48_ENTER_ENUM(2, scx_property_formats_binding));
    for (i = nelements-1; i >= 0; i--)
      l = s48_cons(s48_enter_integer(((long*)data)[i]), l);
    S48_RECORD_SET(p, 2, l);
    break;
  default: /* should not be possible */
    S48_RECORD_SET(p, 1, s48_enter_integer(format));
    S48_RECORD_SET(p, 2, S48_FALSE);
  }
  S48_GC_UNPROTECT();
  return p;
}

s48_value scx_Get_Window_Property(s48_value display, s48_value window, 
				  s48_value atom, s48_value start,
				  s48_value len, s48_value deletep,
				  s48_value req_type) {
  Atom actual_type;
  int format, i;
  unsigned long nitems, bytes_left;
  unsigned char* data = NULL;
  s48_value p = S48_FALSE, res = S48_NULL;
  S48_DECLARE_GC_PROTECT_9(display, window, atom, start, len, deletep,
			   req_type, p, res);
  if (XGetWindowProperty (scx_extract_display(display), 
			  scx_extract_window(window),
			  scx_extract_atom(atom), 
			  s48_extract_integer(start),
			  s48_extract_integer(len),
			  S48_EXTRACT_BOOLEAN(deletep), 
			  scx_extract_atom(req_type),
			  &actual_type, &format, &nitems,
			  &bytes_left, &data) == Success) {
    p = scx_enter_property(actual_type, format, data, nitems);

    XFree(data);
    res = s48_cons(s48_enter_integer(bytes_left), p);
    S48_GC_RETURN(res);
  } else
    /* Property does not exists */
    S48_GC_RETURN(S48_FALSE);
}

s48_value scx_change_property_mode_binding = S48_FALSE;
#define scx_extract_change_property_mode(x) \
  S48_EXTRACT_ENUM(x, scx_change_property_mode_binding)

s48_value scx_Change_Property(s48_value display, s48_value window,
			      s48_value atom,  s48_value mode,
			      s48_value property) {
  Atom type;
  int format, nelements;
  char* data;
  S48_DECLARE_GC_PROTECT_5(display, window, atom, mode, property);
  
  scx_extract_property(property, &type, &format, &data, &nelements);

  XChangeProperty(scx_extract_display(display), scx_extract_window(window),
		  scx_extract_atom(atom), type, format,
		  scx_extract_change_property_mode(mode),
		  data, nelements);
  free(data);
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Set_Selection_Owner(s48_value display, s48_value selection,
				  s48_value owner, s48_value time) {
  S48_DECLARE_GC_PROTECT_4(display, selection, owner, time);
  XSetSelectionOwner(scx_extract_display(display), scx_extract_atom(selection),
		     scx_extract_window(owner), scx_extract_time(time));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

s48_value scx_Get_Selection_Owner(s48_value display, s48_value selection) {
  Window w;
  S48_DECLARE_GC_PROTECT_2(display, selection);
  w = XGetSelectionOwner(scx_extract_display(display),
			 scx_extract_atom(selection));
  S48_GC_RETURN(scx_enter_window(w));
}

s48_value scx_Convert_Selection(s48_value display, s48_value selection,
				s48_value target, s48_value property,
				s48_value requestor, s48_value time) {
  S48_DECLARE_GC_PROTECT_6(display, selection, target, property, requestor,
			   time);
  XConvertSelection(scx_extract_display(display), scx_extract_atom(selection),
		    scx_extract_atom(target), scx_extract_atom(property),
		    scx_extract_window(requestor), scx_extract_time(time));
  S48_GC_RETURN(S48_UNSPECIFIC);
}

void scx_init_property(void) {
  SCX_PRO_IMP(scx_property_binding, "scx-property");
  SCX_PRO_IMP(scx_property_format_binding, "scx-property-format");
  SCX_PRO_IMP(scx_property_formats_binding, "scx-property-formats");
  SCX_PRO_IMP(scx_change_property_mode_binding, "scx-change-property-mode");

  S48_EXPORT_FUNCTION(scx_Intern_Atom);
  S48_EXPORT_FUNCTION(scx_Intern_Atoms);
  S48_EXPORT_FUNCTION(scx_Get_Atom_Name);
  S48_EXPORT_FUNCTION(scx_List_Properties);
  S48_EXPORT_FUNCTION(scx_Rotate_Window_Properties);
  S48_EXPORT_FUNCTION(scx_Delete_Property);
  S48_EXPORT_FUNCTION(scx_Get_Window_Property);
  S48_EXPORT_FUNCTION(scx_Change_Property);
  S48_EXPORT_FUNCTION(scx_Set_Selection_Owner);
  S48_EXPORT_FUNCTION(scx_Get_Selection_Owner);
  S48_EXPORT_FUNCTION(scx_Convert_Selection);
}
