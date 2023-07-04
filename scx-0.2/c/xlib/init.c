#include "xlib.h"

s48_value scx_Xlib_Release_4_Or_Later () {
    return S48_TRUE;
}

s48_value scx_Xlib_Release_5_Or_Later () {
#ifdef XLIB_RELEASE_5_OR_LATER
    return S48_TRUE;
#else
    return S48_FALSE;
#endif
}

s48_value scx_Xlib_Release_6_Or_Later () {
#ifdef XLIB_RELEASE_6_OR_LATER
    return S48_TRUE;
#else
    return S48_FALSE;
#endif
}

extern void scx_init_types();
extern void scx_init_window();
extern void scx_init_display();
extern void scx_init_color();
extern void scx_init_colormap();
extern void scx_init_pixel();
extern void scx_init_gcontext();
extern void scx_init_event();
extern void scx_init_pixmap();
extern void scx_init_graphics();
extern void scx_init_font();
extern void scx_init_cursor();
extern void scx_init_text();
extern void scx_init_property();
extern void scx_init_wm();
extern void scx_init_client();
extern void scx_init_key();
extern void scx_init_error();
extern void scx_init_extension();
extern void scx_init_util();
extern void scx_init_grab();
extern void scx_init_visual();
extern void scx_init_region();
extern void scx_init_event_types();

#ifdef WITH_XFT
extern void scx_xft_init();
extern void scx_xrender_init();
#endif

void scx_init_xlib(void) {
  S48_EXPORT_FUNCTION(scx_Xlib_Release_4_Or_Later);
  S48_EXPORT_FUNCTION(scx_Xlib_Release_5_Or_Later);
  S48_EXPORT_FUNCTION(scx_Xlib_Release_6_Or_Later);
  
  scx_init_types();
  scx_init_display();
  scx_init_visual();
  scx_init_colormap();
  scx_init_cursor();
  scx_init_error();
  scx_init_event();
  scx_init_font();
  scx_init_gcontext();
  scx_init_grab();
  scx_init_graphics();
  scx_init_key();
  scx_init_property();
  scx_init_text();
  scx_init_window();
  scx_init_wm();
  scx_init_pixmap();
  scx_init_client();
  scx_init_util();
  scx_init_event_types();
  scx_init_region();

/*extern void scx_init_xpm();*/

#ifdef WITH_XFT
  s48_add_external_init(scx_xft_init);
  s48_add_external_init(scx_xrender_init);
#endif

}


/*
 #if defined(XLIB_RELEASE_5_OR_LATER) && (defined(sun) || defined(__sun__)) &&\
    defined(__svr4__)
*/
/*
 * Stub interface to dynamic linker routines
 * that SunOS uses but didn't ship with 4.1.
 *
 * The C library routine wcstombs in SunOS 4.1 tries to dynamically
 * load some routines using the dlsym interface, described in dlsym(3x).
 * Unfortunately SunOS 4.1 does not include the necessary library, libdl.
 */
/*
void *dlopen() { return 0; }
void *dlsym() { return 0; }
int dlclose() { return -1; }
 #endif
*/
