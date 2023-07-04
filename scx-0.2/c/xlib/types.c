/* Copyright 2001-2003 by Norbert Freudemann, David Frese */

#include "xlib.h"

s48_value scx_display = S48_FALSE;
s48_value scx_color = S48_FALSE;
s48_value scx_gc = S48_FALSE;
s48_value scx_fontstruct = S48_FALSE;
s48_value scx_screenformat = S48_FALSE;
s48_value scx_visual = S48_FALSE;
s48_value scx_screen = S48_FALSE;
s48_value scx_event_mask_binding = S48_FALSE;
s48_value scx_gc_value_binding = S48_FALSE;
s48_value scx_gc_values_binding = S48_FALSE;
s48_value scx_byte_order_binding = S48_FALSE;
s48_value scx_byte_orders_binding = S48_FALSE;
s48_value scx_bit_order_binding = S48_FALSE;
s48_value scx_bit_orders_binding = S48_FALSE;
s48_value scx_bit_gravity_binding = S48_FALSE;
s48_value scx_bit_gravities_binding = S48_FALSE;
s48_value scx_win_gravity_binding = S48_FALSE;
s48_value scx_win_gravities_binding = S48_FALSE;
s48_value scx_event_type_binding = S48_FALSE;
s48_value scx_event_types_binding = S48_FALSE;
s48_value scx_font_direction_binding = S48_FALSE;
s48_value scx_font_directions_binding = S48_FALSE;
s48_value scx_state_binding = S48_FALSE;
s48_value scx_states_binding = S48_FALSE;
s48_value scx_state_set_binding = S48_FALSE;
s48_value scx_button_binding = S48_FALSE;
s48_value scx_buttons_binding = S48_FALSE;
s48_value scx_notify_mode_binding = S48_FALSE;
s48_value scx_notify_modes_binding = S48_FALSE;
s48_value scx_notify_detail_binding = S48_FALSE;
s48_value scx_notify_details_binding = S48_FALSE;
s48_value scx_visibility_state_binding = S48_FALSE;
s48_value scx_visibility_states_binding = S48_FALSE;
s48_value scx_place_binding = S48_FALSE;
s48_value scx_places_binding = S48_FALSE;
s48_value scx_property_state_binding = S48_FALSE;
s48_value scx_property_states_binding = S48_FALSE;
s48_value scx_colormap_state_binding = S48_FALSE;
s48_value scx_colormap_states_binding = S48_FALSE;
s48_value scx_mapping_request_binding = S48_FALSE;
s48_value scx_mapping_requests_binding = S48_FALSE;
s48_value scx_backing_store_binding = S48_FALSE;
s48_value scx_backing_stores_binding = S48_FALSE;
s48_value scx_map_state_binding = S48_FALSE;
s48_value scx_map_states_binding = S48_FALSE;
s48_value scx_window_class_binding = S48_FALSE;
s48_value scx_window_classes_binding = S48_FALSE;
s48_value scx_stack_mode_binding = S48_FALSE;
s48_value scx_stack_modes_binding = S48_FALSE;
s48_value scx_window_change_binding = S48_FALSE;
s48_value scx_window_changes_binding = S48_FALSE;

s48_value s48_checked_record_ref(s48_value value, int i,
				 s48_value rectype) {
  s48_check_record_type(value, rectype);
  return S48_UNSAFE_RECORD_REF(value, i);
}

int s48_list_length(s48_value list) {
  int res = 0;
  while (list != S48_NULL) {
    res++;
    list = S48_CDR(list);
  }
  return res;
}

s48_value scx_enum_set_type_binding = S48_FALSE;

extern unsigned long s48_extract_enum_set(s48_value v, s48_value type) {
  S48_DECLARE_GC_PROTECT_2(v, type);
  s48_check_record_type(v, scx_enum_set_type_binding);
  if (S48_UNSAFE_SHARED_BINDING_REF(type) !=
      S48_UNSAFE_RECORD_REF(v, 0))
    s48_raise_argument_type_error(v);
  S48_GC_RETURN(s48_extract_integer(S48_UNSAFE_RECORD_REF(v, 1)));
}

s48_value s48_enter_enum_set(unsigned long v, s48_value type) {
  s48_value r = s48_make_record(scx_enum_set_type_binding);
  S48_DECLARE_GC_PROTECT(1);
  S48_GC_PROTECT_1(r);
  S48_RECORD_SET(r, 0, S48_SHARED_BINDING_REF(type));
  S48_RECORD_SET(r, 1, s48_enter_integer(v));
  S48_GC_UNPROTECT();
  return r;
}

s48_value scx_struct_cache_ref(void* cpointer, s48_value list) {
  S48_DECLARE_GC_PROTECT_1(list);
  while (list != S48_NULL) {
    if (S48_EXTRACT_POINTER(S48_CAR(S48_CAR(list))) == cpointer)
      S48_GC_RETURN(S48_WEAK_POINTER_REF(S48_CDR(S48_CAR(list))));
    list = S48_CDR(list);
  }
  S48_GC_RETURN(S48_FALSE);
}

void scx_struct_cache_set(void* cpointer, s48_value* l, s48_value v) {
  s48_value list = *l;
  s48_value previous = S48_FALSE;
  s48_value pair = S48_FALSE;
  S48_DECLARE_GC_PROTECT(4);
  S48_GC_PROTECT_4(v, list, pair, previous);

  /* create the new entry */
  pair = s48_make_weak_pointer(v);
  pair = s48_cons(S48_ENTER_POINTER(cpointer), pair);
  *l = s48_cons(pair, *l);
  previous = *l;

  /* remove all empty and duplicate entries */
  while (list != S48_NULL) {
    s48_value entry = S48_CAR(list);
    if ((S48_WEAK_POINTER_REF(S48_CDR(entry)) == S48_FALSE) ||
	(S48_EXTRACT_POINTER(S48_CAR(entry)) == cpointer))
      S48_SET_CDR(previous, S48_CDR(list));
    else
      previous = list;
    list = S48_CDR(list);
  }
  S48_GC_UNPROTECT();
}

void scx_init_types(void) {
  SCX_PRO_IMP(scx_enum_set_type_binding, "s48-enum-set-type");
  SCX_PRO_IMP(scx_display, "scx-display");
  SCX_PRO_IMP(scx_color, "scx-color");
  SCX_PRO_IMP(scx_gc, "scx-gc");
  SCX_PRO_IMP(scx_fontstruct, "scx-fontstruct");
  SCX_PRO_IMP(scx_screenformat, "scx-screenformat");
  SCX_PRO_IMP(scx_visual, "scx-visual");
  SCX_PRO_IMP(scx_screen, "scx-screen");

  SCX_PRO_IMP(scx_event_mask_binding, "scx-event-mask");
  SCX_PRO_IMP(scx_gc_value_binding, "scx-gc-value");
  SCX_PRO_IMP(scx_gc_values_binding, "scx-gc-values");
  SCX_PRO_IMP(scx_byte_order_binding, "scx-byte-order");
  SCX_PRO_IMP(scx_byte_orders_binding, "scx-byte-orders");
  SCX_PRO_IMP(scx_bit_order_binding, "scx-bit-order");
  SCX_PRO_IMP(scx_bit_orders_binding, "scx-bit-orders");
  SCX_PRO_IMP(scx_bit_gravity_binding, "scx-bit-gravity");
  SCX_PRO_IMP(scx_bit_gravities_binding, "scx-bit-gravities");
  SCX_PRO_IMP(scx_win_gravity_binding, "scx-win-gravity");
  SCX_PRO_IMP(scx_win_gravities_binding, "scx-win-gravities");
  SCX_PRO_IMP(scx_event_type_binding, "scx-event-type");
  SCX_PRO_IMP(scx_event_types_binding, "scx-event-types");
  SCX_PRO_IMP(scx_font_direction_binding, "scx-font-direction");
  SCX_PRO_IMP(scx_font_directions_binding, "scx-font-directions");
  SCX_PRO_IMP(scx_state_binding, "scx-state");
  SCX_PRO_IMP(scx_states_binding, "scx-states");
  SCX_PRO_IMP(scx_state_set_binding, "scx-state-set");
  SCX_PRO_IMP(scx_button_binding, "scx-button");
  SCX_PRO_IMP(scx_buttons_binding, "scx-buttons");
  SCX_PRO_IMP(scx_notify_mode_binding, "scx-notify-mode");
  SCX_PRO_IMP(scx_notify_modes_binding, "scx-notify-modes");
  SCX_PRO_IMP(scx_notify_detail_binding, "scx-notify-detail");
  SCX_PRO_IMP(scx_notify_details_binding, "scx-notify-details");
  SCX_PRO_IMP(scx_visibility_state_binding, "scx-visibility-state");
  SCX_PRO_IMP(scx_visibility_states_binding, "scx-visibility-states");
  SCX_PRO_IMP(scx_place_binding, "scx-place");
  SCX_PRO_IMP(scx_places_binding, "scx-places");
  SCX_PRO_IMP(scx_property_state_binding, "scx-property-state");
  SCX_PRO_IMP(scx_property_states_binding, "scx-property-states");
  SCX_PRO_IMP(scx_colormap_state_binding, "scx-colormap-state");
  SCX_PRO_IMP(scx_colormap_state_binding, "scx-colormap-states");
  SCX_PRO_IMP(scx_mapping_request_binding, "scx-mapping-request");
  SCX_PRO_IMP(scx_mapping_requests_binding, "scx-mapping-requests");
  SCX_PRO_IMP(scx_backing_store_binding, "scx-backing-store");
  SCX_PRO_IMP(scx_backing_stores_binding, "scx-backing-stores");
  SCX_PRO_IMP(scx_map_state_binding, "scx-map-state");
  SCX_PRO_IMP(scx_map_states_binding, "scx-map-states");
  SCX_PRO_IMP(scx_window_class_binding, "scx-window-class");
  SCX_PRO_IMP(scx_window_classes_binding, "scx-window-classes");
  SCX_PRO_IMP(scx_stack_mode_binding, "scx-stack-mode");
  SCX_PRO_IMP(scx_stack_modes_binding, "scx-stack-modes");
  SCX_PRO_IMP(scx_window_change_binding, "scx-window-change");
  SCX_PRO_IMP(scx_window_changes_binding, "scx-window-changes");
}
