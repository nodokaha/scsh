#include "xlib.h"
#include <stdio.h>

s48_value scx_error_code_binding = S48_FALSE;
s48_value scx_error_codes_binding = S48_FALSE;

s48_value scx_enter_error_code(int code) {
  s48_value v = S48_SHARED_BINDING_REF(scx_error_codes_binding);
  if (code < S48_VECTOR_LENGTH(v))
    return S48_VECTOR_REF(v, code);
  else
    return s48_enter_integer(code); /* Extension Errors */
}

s48_value scx_extract_error_code(s48_value code) {
  if (S48_RECORD_P(code))
    return s48_extract_integer(s48_checked_record_ref(code, 1,
						      scx_error_code_binding));
  else
    return s48_extract_integer(code);
}

s48_value scx_x_error_binding = S48_FALSE;

s48_value scx_enter_x_error(XErrorEvent* xe) {
  s48_value e = s48_make_record(scx_x_error_binding);
  char s[1024];
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(e);

  S48_RECORD_SET(e, 0, scx_enter_display(xe->display));
  S48_RECORD_SET(e, 1, s48_enter_integer(xe->serial));
  S48_RECORD_SET(e, 2, scx_enter_error_code(xe->error_code));
  S48_RECORD_SET(e, 3, s48_enter_integer(xe->request_code));
  S48_RECORD_SET(e, 4, s48_enter_integer(xe->minor_code));
  S48_RECORD_SET(e, 5, s48_enter_integer(xe->resourceid));
  XGetErrorText(xe->display, xe->error_code, s, 1023);
  S48_RECORD_SET(e, 6, s48_enter_string(s));
  
  S48_GC_UNPROTECT();
  return e;
}

void scx_extract_x_error(s48_value e, XErrorEvent* xe) {
  S48_DECLARE_GC_PROTECT_1(e);
  s48_check_record_type(e, scx_x_error_binding);
  xe->type = 1;
  xe->display = scx_extract_display(S48_RECORD_REF(e, 0));
  xe->serial = s48_extract_integer(S48_RECORD_REF(e, 1));
  xe->error_code = scx_extract_error_code(S48_RECORD_REF(e, 2));
  xe->request_code = s48_extract_integer(S48_RECORD_REF(e, 3));
  xe->minor_code = s48_extract_integer(S48_RECORD_REF(e, 4));
  xe->resourceid = s48_extract_integer(S48_RECORD_REF(e, 5));
  S48_GC_UNPROTECT();
}

/* Default error handlers of the Xlib */
extern int _XDefaultIOError();   
extern int _XDefaultError();

static s48_value internal_x_error_handler_binding = S48_FALSE;

static int error_handler_wrapper(Display* dpy, XErrorEvent* e) {
  char handled = 0;
  if ((internal_x_error_handler_binding != S48_FALSE) &&
      (S48_SHARED_BINDING_REF(internal_x_error_handler_binding) != S48_FALSE)){
    s48_value v = S48_FALSE, display = S48_FALSE, err = S48_FALSE;
    S48_DECLARE_GC_PROTECT_3(v, display, err);
    display = scx_enter_display(dpy);
    err = scx_enter_x_error(e);
    v =
      s48_call_scheme(S48_SHARED_BINDING_REF(internal_x_error_handler_binding),
		      2, display, err);
    handled = (v != S48_FALSE);
    S48_GC_UNPROTECT();
  }
  if (!handled)
    _XDefaultError(dpy, e);
  return 0;
}

s48_value scx_Get_Error_Text(s48_value display, s48_value code) {
  char buf[1024];
  S48_DECLARE_GC_PROTECT_2(display, code);
  XGetErrorText(scx_extract_display(display), scx_extract_error_code(code),
		buf, 1024);
  buf[1023] = 0;
  S48_GC_RETURN(s48_enter_string(buf));
}

s48_value scx_Get_Error_Database_Text(s48_value display, s48_value name,
				      s48_value message, s48_value def) {
  char buf[1024];
  S48_DECLARE_GC_PROTECT_4(display, name, message, def);
  XGetErrorDatabaseText(scx_extract_display(display),
			s48_extract_string(name),
			s48_extract_string(message),
			s48_extract_string(def),
			buf, 1024);
  buf[1023] = 0;
  S48_GC_RETURN(s48_enter_string(buf));
}

s48_value internal_x_fatal_error_handler_binding = S48_FALSE;

static int fatal_error_handler_wrapper(Display* d) {
  /* call the scheme-func internal-x-fatal-error-handler, which does
     the rest. */
  if ((internal_x_fatal_error_handler_binding != S48_FALSE) &&
      (S48_SHARED_BINDING_REF(internal_x_fatal_error_handler_binding)
       != S48_FALSE))
    s48_call_scheme(S48_SHARED_BINDING_REF(
                      internal_x_fatal_error_handler_binding),
		    1, scx_enter_display(d));
  /* In case the scheme error handler does not exit (or none exists): */
  _XDefaultIOError (d);
  /* And if even the default handler does not exit: */
  exit(1);         
  /*NOTREACHED*/
  return 0;
}

void scx_init_error() {
  SCX_PRO_IMP(scx_error_code_binding, "scx-error-code");
  SCX_PRO_IMP(scx_error_codes_binding, "scx-error-codes");
  SCX_PRO_IMP(scx_x_error_binding, "scx-x-error");
  S48_GC_PROTECT_GLOBAL(internal_x_error_handler_binding);
  S48_GC_PROTECT_GLOBAL(internal_x_fatal_error_handler_binding);

  S48_EXPORT_FUNCTION(scx_Get_Error_Text);
  S48_EXPORT_FUNCTION(scx_Get_Error_Database_Text);

  internal_x_fatal_error_handler_binding =
    s48_get_imported_binding("internal-x-fatal-error-handler");
  internal_x_error_handler_binding =
    s48_get_imported_binding("internal-x-error-handler");

  (void)XSetIOErrorHandler(fatal_error_handler_wrapper);
  (void)XSetErrorHandler(error_handler_wrapper);
}
