/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#ifndef _SCX_XLIB_H
#define _SCX_XLIB_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>

#ifndef NeedFunctionPrototypes          /* Kludge */
 #error "X11 Release 3 (or earlier) no longer supported"
#endif

#if XlibSpecificationRelease >= 5
#  define XLIB_RELEASE_5_OR_LATER
#endif

#if XlibSpecificationRelease >= 6
#  define XLIB_RELEASE_6_OR_LATER
#endif

#include <scheme48.h>

#define S48_DECLARE_GC_PROTECT_1(v0) \
  S48_DECLARE_GC_PROTECT(1); S48_GC_PROTECT_1(v0)
#define S48_DECLARE_GC_PROTECT_2(v0, v1) \
  S48_DECLARE_GC_PROTECT(2); S48_GC_PROTECT_2(v0, v1)
#define S48_DECLARE_GC_PROTECT_3(v0, v1, v2) \
  S48_DECLARE_GC_PROTECT(3); S48_GC_PROTECT_3(v0, v1, v2)
#define S48_DECLARE_GC_PROTECT_4(v0, v1, v2, v3) \
  S48_DECLARE_GC_PROTECT(4); S48_GC_PROTECT_4(v0, v1, v2, v3)
#define S48_DECLARE_GC_PROTECT_5(v0, v1, v2, v3, v4) \
  S48_DECLARE_GC_PROTECT(5); S48_GC_PROTECT_5(v0, v1, v2, v3, v4)
#define S48_DECLARE_GC_PROTECT_6(v0, v1, v2, v3, v4, v5) \
  S48_DECLARE_GC_PROTECT(6); S48_GC_PROTECT_6(v0, v1, v2, v3, v4, v5)
#define S48_DECLARE_GC_PROTECT_7(v0, v1, v2, v3, v4, v5, v6) \
  S48_DECLARE_GC_PROTECT(7); S48_GC_PROTECT_7(v0, v1, v2, v3, v4, v5, v6)
#define S48_DECLARE_GC_PROTECT_8(v0, v1, v2, v3, v4, v5, v6, v7) \
  S48_DECLARE_GC_PROTECT(8); S48_GC_PROTECT_8(v0, v1, v2, v3, v4, v5, v6, v7)
#define S48_DECLARE_GC_PROTECT_9(v0, v1, v2, v3, v4, v5, v6, v7, v8) \
  S48_DECLARE_GC_PROTECT(9); \
  S48_GC_PROTECT_9(v0, v1, v2, v3, v4, v5, v6, v7, v8)
#define S48_DECLARE_GC_PROTECT_10(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9) \
  S48_DECLARE_GC_PROTECT(10); \
  S48_GC_PROTECT_10(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9)
#define S48_DECLARE_GC_PROTECT_11(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)\
  S48_DECLARE_GC_PROTECT(11); \
  S48_GC_PROTECT_11(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)

#define S48_GC_RETURN(v) do { S48_GC_UNPROTECT(); return(v); } while(0)

#define S48_GC_PROTECT_11(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) \
  (___gc_buffer[2]=(long)&(v1), \
   ___gc_buffer[3]=(long)&(v2), \
   ___gc_buffer[4]=(long)&(v3), \
   ___gc_buffer[5]=(long)&(v4), \
   ___gc_buffer[6]=(long)&(v5), \
   ___gc_buffer[7]=(long)&(v6), \
   ___gc_buffer[8]=(long)&(v7), \
   ___gc_buffer[9]=(long)&(v8), \
   ___gc_buffer[10]=(long)&(v9), \
   ___gc_buffer[11]=(long)&(v10), \
   ___gc_buffer[12]=(long)&(v11), \
   s48_push_gc_rootsB((char *) ___gc_buffer, 11))

#define S48_NULL_P(x) S48_EQ(x, S48_NULL)

#define S48_INTEGER_P(x) (S48_FIXNUM_P(x) || S48_BIGNUM_P(x))
/* TODO: S48_x_POINTER already exitst ?!*/
#define S48_POINTER_P(x) S48_INTEGER_P(x)
#define S48_ENTER_POINTER(x) s48_enter_integer((long)x)
#define S48_EXTRACT_POINTER(x) (void*)s48_extract_integer(x)

extern s48_value s48_checked_record_ref(s48_value value, int i,
					s48_value rectype);

#define SCX_PRO_IMP(var, name) \
  do { S48_GC_PROTECT_GLOBAL(var); var = s48_get_imported_binding(name); } \
  while (0)

extern int s48_list_length(s48_value list);

#define S48_EXTRACT_ENUM(x, type) \
  s48_extract_integer(s48_checked_record_ref(x, 1, type))

#define S48_ENTER_ENUM(index, vvector) \
  S48_VECTOR_REF(S48_SHARED_BINDING_REF(vvector), index)

#define S48_EXTRACT_ENUM_SET(x, type) s48_extract_enum_set(x, type)
extern unsigned long s48_extract_enum_set(s48_value v, s48_value type);
extern s48_value s48_enter_enum_set(unsigned long v, s48_value type);

/*** Extraction-Macros for the XIDs ********************************/

#define scx_extract_window(x) (Window)s48_extract_integer(x)
#define scx_enter_window(x) s48_enter_integer((long)x)
#define scx_extract_drawable(x) (Drawable)s48_extract_integer(x)
#define scx_enter_drawable(x) s48_enter_integer((long)x)
#define scx_extract_font(x) (Font)s48_extract_integer(x)
#define scx_enter_font(x) s48_enter_integer((long)x)
#define scx_extract_pixmap(x) (Pixmap)s48_extract_integer(x)
#define scx_enter_pixmap(x) s48_enter_integer((long)x)
#define scx_extract_cursor(x) (Cursor)s48_extract_integer(x)
#define scx_enter_cursor(x) s48_enter_integer((long)x)
#define scx_extract_colormap(x) (Colormap)s48_extract_integer(x)
#define scx_enter_colormap(x) s48_enter_integer((long)x)
#define scx_extract_gcontext(x) (GContext)s48_extract_integer(x)
#define scx_enter_gcontext(x) s48_enter_integer((long)x)
#define scx_extract_keysym(x) (GContext)s48_extract_integer(x)
#define scx_enter_keysym(x) s48_enter_integer((long)x)

/* other CARD32 */

#define scx_enter_atom(x) s48_enter_integer((long)x)
#define scx_extract_atom(x) (Atom)s48_extract_integer(x)
#define scx_enter_visualid(x) s48_enter_integer((long)x)
#define scx_extract_visualid(x) (VisualID)s48_extract_integer(x)
#define scx_enter_time(x) s48_enter_integer(x)
#define scx_extract_time(x) (Time)s48_extract_integer(x)

/* other ints */

#define scx_enter_keycode(x) s48_enter_fixnum(x)
#define scx_extract_keycode(x) (KeyCode)s48_extract_integer(x)
#define scx_extract_pixel(x) (Pixel)s48_extract_integer(x)
#define scx_enter_pixel(x) s48_enter_integer((long)x)

/* records */

#define scx_extract_region(x) (Region)S48_EXTRACT_POINTER(x)
#define scx_enter_region(r) S48_ENTER_POINTER(r)


extern s48_value scx_display;
#define scx_extract_display(x) \
  (Display*)s48_extract_integer(s48_checked_record_ref(x, 0, scx_display))
extern s48_value scx_enter_display(Display* dpy);
#define SCX_DISPLAY_AFTER_FUNCTION(d) \
  s48_checked_record_ref(d, 14, scx_display)

extern s48_value scx_color;
extern void scx_extract_color(s48_value v, XColor* c);
extern void scx_copy_color(const XColor* c, s48_value v);
extern s48_value scx_enter_color(const XColor* c);

extern s48_value scx_gc;
#define scx_extract_gc(x) \
  (GC)s48_extract_integer(s48_checked_record_ref(x, 0, scx_gc))
extern s48_value scx_enter_gc(GC gc);

extern s48_value scx_fontstruct;
#define scx_extract_fontstruct(x)\
  (XFontStruct*)s48_extract_integer(s48_checked_record_ref(x, 0,\
							   scx_fontstruct))
extern s48_value scx_enter_charstruct(XCharStruct* cs);
extern s48_value scx_enter_fontstruct(XFontStruct* fs);

extern s48_value scx_screenformat;
#define scx_extract_screenformat(x)\
  (ScreenFormat*)s48_extract_integer(s48_checked_record_ref(x, 0,\
							   scx_screenformat))
extern s48_value scx_enter_screenformat(ScreenFormat* sf);

extern s48_value scx_visual;
#define scx_extract_visual(x)\
  (Visual*)s48_extract_integer(s48_checked_record_ref(x, 0,\
						      scx_visual))
extern s48_value scx_enter_visual(Visual* vis);

extern s48_value scx_screen;
#define scx_extract_screen(x)\
  (Screen*)s48_extract_integer(s48_checked_record_ref(x, 0,\
						      scx_screen))
extern s48_value scx_enter_screen(Screen* scr);

extern void scx_extract_property(s48_value p, Atom* type, int* format,
				 char** data, int* nelements);
extern s48_value scx_enter_property(Atom type, int format, char* data,
				    int nelements);

extern scx_enter_enter_window_changes(XWindowChanges* WC, unsigned long mask);
extern unsigned long scx_extract_window_changes(s48_value changes,
						XWindowChanges* WC);

extern s48_value scx_enter_x_error(XErrorEvent* xe);
extern void scx_extract_x_error(s48_value e, XErrorEvent* xe);

/* more types */

extern s48_value scx_event_mask_binding;
#define scx_extract_event_mask(x) \
  S48_EXTRACT_ENUM_SET(x, scx_event_mask_binding)
#define scx_enter_event_mask(x) s48_enter_enum_set(x, scx_event_mask_binding)

extern s48_value scx_gc_value_binding;
extern s48_value scx_gc_values_binding;
#define scx_extract_gc_value(x) S48_EXTRACT_ENUM(x, scx_gc_value_binding)
#define scx_enter_gc_value(x) S48_ENTER_ENUM(x, scx_gc_values_binding)

extern s48_value scx_byte_order_binding;
extern s48_value scx_byte_orders_binding;
#define scx_extract_byte_order(x) S48_EXTRACT_ENUM(x, scx_byte_order_binding)
#define scx_enter_byte_order(x)  S48_ENTER_ENUM(x, scx_byte_orders_binding)

extern s48_value scx_bit_order_binding;
extern s48_value scx_bit_orders_binding;
#define scx_extract_bit_order(x) S48_EXTRACT_ENUM(x, scx_bit_order_binding)
#define scx_enter_bit_order(x) S48_ENTER_ENUM(x, scx_bit_orders_binding)

extern s48_value scx_bit_gravity_binding;
extern s48_value scx_bit_gravities_binding;
#define scx_extract_bit_gravity(x) S48_EXTRACT_ENUM(x, scx_bit_gravity_binding)
#define scx_enter_bit_gravity(x) S48_ENTER_ENUM(x, scx_bit_gravities_binding)

extern s48_value scx_win_gravity_binding;
extern s48_value scx_win_gravities_binding;
#define scx_extract_win_gravity(x) S48_EXTRACT_ENUM(x, scx_win_gravity_binding)
#define scx_enter_win_gravity(x) S48_ENTER_ENUM(x, scx_win_gravities_binding)

extern s48_value scx_event_type_binding;
extern s48_value scx_event_types_binding;
#define scx_extract_event_type(x) S48_EXTRACT_ENUM(x, scx_event_type_binding)
#define scx_enter_event_type(x) S48_ENTER_ENUM(x, scx_event_types_binding)

extern s48_value scx_font_direction_binding;
extern s48_value scx_font_directions_binding;
#define scx_extract_font_direction(x) \
  S48_EXTRACT_ENUM(x, scx_font_direction_binding)
#define scx_enter_font_direction(x) \
  S48_ENTER_ENUM(x, scx_font_directions_binding)

extern s48_value scx_state_binding;
extern s48_value scx_states_binding;
#define scx_extract_state(x) S48_EXTRACT_ENUM(x, scx_state_binding)
#define scx_enter_state(x) S48_ENTER_ENUM(x, scx_states_binding)

extern s48_value scx_state_set_binding;
#define scx_extract_state_set(x) S48_EXTRACT_ENUM_SET(x, scx_state_set_binding)
#define scx_enter_state_set(x) s48_enter_enum_set(x, scx_state_set_binding)

extern s48_value scx_button_binding;
extern s48_value scx_buttons_binding;
#define scx_extract_button(x) S48_EXTRACT_ENUM(x, scx_button_binding)
#define scx_enter_button(x) S48_ENTER_ENUM(x, scx_buttons_binding)

extern s48_value scx_notify_mode_binding;
extern s48_value scx_notify_modes_binding;
#define scx_extract_notify_mode(x) S48_EXTRACT_ENUM(x, scx_notify_mode_binding)
#define scx_enter_notify_mode(x) S48_ENTER_ENUM(x, scx_notify_modes_binding)

extern s48_value scx_notify_detail_binding;
extern s48_value scx_notify_details_binding;
#define scx_extract_notify_detail(x) \
  S48_EXTRACT_ENUM(x, scx_notify_detail_binding)
#define scx_enter_notify_detail(x) \
  S48_ENTER_ENUM(x, scx_notify_details_binding)

extern s48_value scx_visibility_state_binding;
extern s48_value scx_visibility_states_binding;
#define scx_extract_visibility_state(x) \
  S48_EXTRACT_ENUM(x, scx_visibility_state_binding)
#define scx_enter_visibility_state(x) \
  S48_ENTER_ENUM(x, scx_visibility_states_binding)

extern s48_value scx_place_binding;
extern s48_value scx_places_binding;
#define scx_extract_place(x) S48_EXTRACT_ENUM(x, scx_place_binding)
#define scx_enter_place(x) S48_ENTER_ENUM(x, scx_places_binding)

extern s48_value scx_property_state_binding;
extern s48_value scx_property_states_binding;
#define scx_extract_property_state(x) \
  S48_EXTRACT_ENUM(x, scx_property_state_binding)
#define scx_enter_property_state(x) \
  S48_ENTER_ENUM(x, scx_property_states_binding)

extern s48_value scx_colormap_state_binding;
extern s48_value scx_colormap_states_binding;
#define scx_extract_colormap_state(x) \
  S48_EXTRACT_ENUM(x, scx_colormap_state_binding)
#define scx_enter_colormap_state(x) \
  S48_ENTER_ENUM(x, scx_colormap_states_binding)

extern s48_value scx_mapping_request_binding;
extern s48_value scx_mapping_requests_binding;
#define scx_extract_mapping_request(x) \
  S48_EXTRACT_ENUM(x, scx_mapping_request_binding)
#define scx_enter_mapping_request(x) \
  S48_ENTER_ENUM(x, scx_mapping_requests_binding)

extern s48_value scx_backing_store_binding;
extern s48_value scx_backing_stores_binding;
#define scx_extract_backing_store(x) \
  S48_EXTRACT_ENUM(x, scx_backing_store_binding)
#define scx_enter_backing_store(x) \
  S48_ENTER_ENUM(x, scx_backing_stores_binding)

extern s48_value scx_map_state_binding;
extern s48_value scx_map_states_binding;
#define scx_extract_map_state(x) S48_EXTRACT_ENUM(x, scx_map_state_binding)
#define scx_enter_map_state(x) S48_ENTER_ENUM(x, scx_map_states_binding)

extern s48_value scx_window_class_binding;
extern s48_value scx_window_classes_binding;
#define scx_extract_window_class(x) \
  S48_EXTRACT_ENUM(x, scx_window_class_binding)
#define scx_enter_window_class(x) \
  S48_ENTER_ENUM(x, scx_window_classes_binding)

extern s48_value scx_stack_mode_binding;
extern s48_value scx_stack_modes_binding;
#define scx_extract_stack_mode(x) S48_EXTRACT_ENUM(x, scx_stack_mode_binding)
#define scx_enter_stack_mode(x) S48_ENTER_ENUM(x, scx_stack_modes_binding)

extern s48_value scx_window_change_binding;
extern s48_value scx_window_changes_binding;
#define scx_extract_window_change(x) \
  S48_EXTRACT_ENUM(x, scx_window_change_binding)
#define scx_enter_window_change(x) \
  S48_ENTER_ENUM(x, scx_window_changes_binding)

#endif
